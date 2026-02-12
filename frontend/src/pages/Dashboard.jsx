import React, { useEffect, useState } from 'react';
import { listAllVaultUtxos, parseVaultDatum } from '../lib/lucidClient';
import { Link } from 'react-router-dom';

export default function Dashboard() {
  const [vaults, setVaults] = useState([]);
  useEffect(() => { listAllVaultUtxos().then(setVaults).catch(console.error); }, []);
  const unlocked = vaults.filter(v => {
    const d = parseVaultDatum(v);
    const now = Date.now();
    const timeOk = now >= Number(d.vdUnlockTime || 0n);
    const targetOk = BigInt(v.assets.lovelace) >= BigInt(d.vdTarget || 0n);
    return Number(d.vdMode) === 0 ? timeOk : Number(d.vdMode) === 1 ? targetOk : (timeOk && targetOk);
  });
  const total = vaults.reduce((a,v)=>a + BigInt(v.assets.lovelace), 0n);
  return <div><h2>Dashboard</h2><div className="grid"><article>Total saved: {String(total)} lovelace</article><article>Active: {vaults.length}</article><article>Unlocked: {unlocked.length}</article></div><Link className="btn" to="/app/create">New vault</Link><ul>{vaults.map(v=><li key={`${v.txHash}#${v.outputIndex}`}><Link to={`/app/vault/${v.txHash}#${v.outputIndex}`}>{v.txHash}#{v.outputIndex}</Link></li>)}</ul></div>;
}
