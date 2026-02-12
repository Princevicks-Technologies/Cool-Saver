import React, { useState } from 'react';
import { createVault, MODE, POLICY, getPkh } from '../lib/lucidClient';
import { api } from '../lib/api';

export default function CreateVault() {
  const [form, setForm] = useState({ mode: MODE.TimeLocked, policy: POLICY.Strict, targetLovelace: 0, unlockTimeMs: Date.now()+86400000, beneficiaryAddressOrNull: '', isPublic: true, purposeLabel: 'emergency' });
  const [msg, setMsg] = useState('');
  const set = (k,v)=>setForm(prev=>({...prev,[k]:v}));
  const submit = async ()=>{
    try {
      if (Number(form.policy) === POLICY.NonStrict) alert('Warning: Non-Strict allows early withdrawal with 3% treasury penalty.');
      const txHash = await createVault(form);
      const ownerPkh = await getPkh();
      await api(`/api/vaults/${txHash}#0`, { method:'POST', body: JSON.stringify({ ownerPkh, ...form, target: form.targetLovelace, unlockTime: form.unlockTimeMs, beneficiary: form.beneficiaryAddressOrNull })});
      setMsg(`Created ${txHash}#0`);
    } catch (e) { setMsg(e.message); }
  };
  return <div><h2>Create Vault</h2>
    <label>Mode <select onChange={e=>set('mode', Number(e.target.value))}><option value={0}>Time-Locked</option><option value={1}>Target-Locked</option><option value={2}>Hybrid</option></select></label>
    <label>Policy <select onChange={e=>set('policy', Number(e.target.value))}><option value={0}>Strict</option><option value={1}>Non-Strict</option></select></label>
    {(form.mode===1||form.mode===2)&&<label>Target lovelace <input type='number' onChange={e=>set('targetLovelace', Number(e.target.value))}/></label>}
    {(form.mode===0||form.mode===2)&&<label>Unlock datetime <input type='datetime-local' onChange={e=>set('unlockTimeMs', new Date(e.target.value).getTime())}/></label>}
    <label>Beneficiary (optional)<input onChange={e=>set('beneficiaryAddressOrNull', e.target.value)}/></label>
    <label>Public <input type='checkbox' checked={form.isPublic} onChange={e=>set('isPublic', e.target.checked)}/></label>
    <label>Purpose <select onChange={e=>set('purposeLabel', e.target.value)}><option>rent</option><option>tuition</option><option>business</option><option>emergency</option><option>wedding</option><option>travel</option></select></label>
    <button onClick={submit}>Create vault</button><p>{msg}</p></div>;
}
