import React, { useEffect, useState } from 'react';
import { useParams } from 'react-router-dom';
import { deposit, forceWithdraw, listAllVaultUtxos, parseVaultDatum, withdraw } from '../lib/lucidClient';

export default function VaultDetails() {
  const { vaultRef } = useParams();
  const [utxo, setUtxo] = useState();
  const [amount, setAmount] = useState(1000000);
  const [msg, setMsg] = useState('');
  useEffect(() => { listAllVaultUtxos().then((all) => setUtxo(all.find(u => `${u.txHash}#${u.outputIndex}` === vaultRef))); }, [vaultRef]);
  if (!utxo) return <p>Loading vault...</p>;
  const d = parseVaultDatum(utxo);
  return <div><h2>Vault {vaultRef}</h2><p>Saved: {String(utxo.assets.lovelace)} lovelace</p><p>Mode: {String(d.vdMode)}</p><p>Policy: {String(d.vdPolicy)}</p>
    <input type='number' value={amount} onChange={e=>setAmount(Number(e.target.value))}/><button onClick={async()=>setMsg(await deposit({ utxoRef: vaultRef, amountLovelace: amount }))}>Deposit</button>
    <button onClick={async()=>setMsg(await withdraw({ utxoRef: vaultRef }))}>Withdraw</button>
    <button onClick={async()=>setMsg(await forceWithdraw({ utxoRef: vaultRef, treasuryAddress: import.meta.env.VITE_TREASURY_ADDRESS }))}>Force withdraw</button>
    <p>{msg}</p></div>;
}
