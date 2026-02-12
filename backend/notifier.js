import dotenv from 'dotenv';
import fetch from 'node-fetch';
import { createDb } from './db.js';
import { createEmailProvider } from './emailProvider.js';

dotenv.config();
const db = createDb(process.env.DB_PATH || './data.db', './schema.sql');
const email = createEmailProvider(process.env);

function eligible(mode, unlockTime, target, lovelace, now) {
  const timeOk = now >= Number(unlockTime);
  const targetOk = BigInt(lovelace) >= BigInt(target);
  if (mode === 0) return timeOk;
  if (mode === 1) return targetOk;
  return timeOk && targetOk;
}

async function fetchUtxosAtScript() {
  const r = await fetch(`${process.env.BLOCKFROST_URL}/addresses/${process.env.SCRIPT_ADDRESS}/utxos`, {
    headers: { project_id: process.env.BLOCKFROST_KEY }
  });
  if (!r.ok) throw new Error(`Blockfrost failed: ${r.status}`);
  return r.json();
}

async function run() {
  const utxos = await fetchUtxosAtScript();
  const map = new Map(utxos.map((u) => [`${u.tx_hash}#${u.output_index}`, u]));
  const subs = db.prepare('SELECT * FROM unlock_subscriptions').all();
  const now = Date.now();

  for (const sub of subs) {
    const meta = db.prepare('SELECT * FROM vault_metadata WHERE vaultRef=?').get(sub.vaultRef);
    if (!meta) continue;
    const already = db.prepare('SELECT 1 FROM notifications WHERE vaultRef=? AND type=?').get(sub.vaultRef, 'UNLOCK_ELIGIBLE');
    if (already) continue;

    const utxo = map.get(sub.vaultRef);
    if (!utxo) continue;
    const lovelace = utxo.amount.find((a) => a.unit === 'lovelace')?.quantity || '0';
    if (!eligible(Number(meta.mode), Number(meta.unlockTime), meta.target, lovelace, now)) continue;

    await email.send({
      to: sub.email,
      subject: 'Your Cool Saver vault can be withdrawn',
      text: `Vault ${sub.vaultRef} is now eligible for withdrawal.`
    });
    db.prepare('INSERT OR IGNORE INTO notifications(vaultRef,type,sentAt) VALUES(?,?,?)')
      .run(sub.vaultRef, 'UNLOCK_ELIGIBLE', new Date().toISOString());
  }
}

run().then(() => process.exit(0)).catch((e) => {
  console.error(e);
  process.exit(1);
});
