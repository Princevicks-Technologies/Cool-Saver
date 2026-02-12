import { Blockfrost, Constr, Data, Lucid, SpendingValidator, fromText, getAddressDetails, paymentCredentialOf, toUnit } from "lucid-cardano";
import validatorHex from "../artifacts/validator.hex?raw";

const MODE = { TimeLocked: 0, TargetLocked: 1, Hybrid: 2 };
const POLICY = { Strict: 0, NonStrict: 1 };

let lucid;
let walletApi;
let script;

const VaultDatum = Data.Object({
  vdOwner: Data.Bytes(),
  vdBeneficiary: Data.Bytes(),
  vdMode: Data.Integer(),
  vdPolicy: Data.Integer(),
  vdTarget: Data.Integer(),
  vdUnlockTime: Data.Integer(),
});

const actionRedeemer = (kind) => {
  if (kind === "Deposit") return Data.to(new Constr(0, []));
  if (kind === "Withdraw") return Data.to(new Constr(1, []));
  return Data.to(new Constr(2, []));
};

export function safeStringify(obj) {
  return JSON.stringify(obj, (_, v) => (typeof v === "bigint" ? `${v.toString()}n` : v));
}

export function safeParse(text) {
  return JSON.parse(text, (_, v) => (typeof v === "string" && /^-?\d+n$/.test(v) ? BigInt(v.slice(0, -1)) : v));
}

export function saveJson(key, value) {
  localStorage.setItem(key, safeStringify(value));
}

export function loadJson(key, fallback = null) {
  const raw = localStorage.getItem(key);
  return raw ? safeParse(raw) : fallback;
}

export async function initLucid({ network, blockfrostUrl, blockfrostKey }) {
  lucid = await Lucid.new(new Blockfrost(blockfrostUrl, blockfrostKey), network);
  script = {
    type: "PlutusV2",
    script: validatorHex.trim(),
  };
  return lucid;
}

export async function connectWallet(walletName) {
  if (!window.cardano?.[walletName]) throw new Error(`Wallet ${walletName} not found`);
  walletApi = await window.cardano[walletName].enable();
  lucid.selectWallet(walletApi);
  return true;
}

export async function getWalletAddress() {
  return lucid.wallet.address();
}

export async function getPkh() {
  const addr = await getWalletAddress();
  return paymentCredentialOf(addr).hash;
}

export function getScript() {
  return script;
}

export function getScriptAddress() {
  return lucid.utils.validatorToAddress(script);
}

export async function listAllVaultUtxos() {
  return lucid.utxosAt(getScriptAddress());
}

export function parseVaultDatum(utxo) {
  if (!utxo.datum) throw new Error("Missing inline datum");
  return Data.from(utxo.datum, VaultDatum);
}

export async function listVaultsByOwner(ownerBech32OrPkh) {
  const pkh = ownerBech32OrPkh.startsWith("addr")
    ? paymentCredentialOf(ownerBech32OrPkh).hash
    : ownerBech32OrPkh;
  const utxos = await listAllVaultUtxos();
  return utxos.filter((u) => parseVaultDatum(u).vdOwner === pkh);
}

function beneficiaryToBytes(beneficiaryAddressOrNull) {
  if (!beneficiaryAddressOrNull) return "";
  return paymentCredentialOf(beneficiaryAddressOrNull).hash;
}

function datumFromArgs({ ownerPkh, mode, policy, targetLovelace, unlockTimeMs, beneficiaryAddressOrNull }) {
  return Data.to(
    {
      vdOwner: ownerPkh,
      vdBeneficiary: beneficiaryToBytes(beneficiaryAddressOrNull),
      vdMode: BigInt(mode),
      vdPolicy: BigInt(policy),
      vdTarget: BigInt(targetLovelace ?? 0),
      vdUnlockTime: BigInt(unlockTimeMs ?? 0),
    },
    VaultDatum,
  );
}

export async function createVault({ mode, policy, targetLovelace, unlockTimeMs, beneficiaryAddressOrNull, isPublic, purposeLabel }) {
  const ownerPkh = await getPkh();
  const datum = datumFromArgs({ ownerPkh, mode, policy, targetLovelace, unlockTimeMs, beneficiaryAddressOrNull });
  const tx = await lucid
    .newTx()
    .payToContract(getScriptAddress(), { inline: datum }, { lovelace: 2_000_000n })
    .complete();
  const signed = await tx.sign().complete();
  const txHash = await signed.submit();
  saveJson(`vault-meta:${txHash}#0`, { isPublic, purposeLabel, createdAt: Date.now() });
  return txHash;
}

export async function deposit({ utxoRef, amountLovelace }) {
  const [txHash, outputIndexText] = utxoRef.split("#");
  const outputIndex = Number(outputIndexText);
  const utxos = await listAllVaultUtxos();
  const target = utxos.find((u) => u.txHash === txHash && u.outputIndex === outputIndex);
  if (!target) throw new Error("Vault UTxO not found");
  const value = target.assets.lovelace + BigInt(amountLovelace);
  const tx = await lucid
    .newTx()
    .collectFrom([target], actionRedeemer("Deposit"))
    .attachSpendingValidator(script)
    .payToContract(getScriptAddress(), { inline: target.datum }, { lovelace: value })
    .complete();
  const signed = await tx.sign().complete();
  return signed.submit();
}

export async function withdraw({ utxoRef }) {
  const [txHash, outputIndexText] = utxoRef.split("#");
  const outputIndex = Number(outputIndexText);
  const utxos = await listAllVaultUtxos();
  const target = utxos.find((u) => u.txHash === txHash && u.outputIndex === outputIndex);
  if (!target) throw new Error("Vault UTxO not found");
  const tx = await lucid
    .newTx()
    .collectFrom([target], actionRedeemer("Withdraw"))
    .attachSpendingValidator(script)
    .validFrom(Date.now())
    .complete();
  const signed = await tx.sign().complete();
  return signed.submit();
}

export async function forceWithdraw({ utxoRef, treasuryAddress }) {
  const [txHash, outputIndexText] = utxoRef.split("#");
  const outputIndex = Number(outputIndexText);
  const utxos = await listAllVaultUtxos();
  const target = utxos.find((u) => u.txHash === txHash && u.outputIndex === outputIndex);
  if (!target) throw new Error("Vault UTxO not found");
  const d = parseVaultDatum(target);
  const recipientPkh = d.vdBeneficiary && d.vdBeneficiary.length > 0 ? d.vdBeneficiary : d.vdOwner;
  const recipientAddr = lucid.utils.credentialToAddress({ type: "Key", hash: recipientPkh });
  const total = target.assets.lovelace;
  const penalty = (total * 300n) / 10000n;
  const recipientAmount = total - penalty;
  const tx = await lucid
    .newTx()
    .collectFrom([target], actionRedeemer("ForceWithdraw"))
    .attachSpendingValidator(script)
    .payToAddress(treasuryAddress, { lovelace: penalty })
    .payToAddress(recipientAddr, { lovelace: recipientAmount })
    .complete();
  const signed = await tx.sign().complete();
  return signed.submit();
}

export { MODE, POLICY };
