import React, { useState } from 'react';
import { api } from '../lib/api';
import { connectWallet, getPkh, getWalletAddress, initLucid } from '../lib/lucidClient';

export default function Register() {
  const [email, setEmail] = useState('');
  const [msg, setMsg] = useState('');
  const submit = async () => {
    try {
      await initLucid({ network: import.meta.env.VITE_NETWORK, blockfrostUrl: import.meta.env.VITE_BLOCKFROST_URL, blockfrostKey: import.meta.env.VITE_BLOCKFROST_KEY });
      await connectWallet('nami');
      const walletAddress = await getWalletAddress();
      const pkh = await getPkh();
      const out = await api('/api/auth/register', { method: 'POST', body: JSON.stringify({ walletAddress, pkh, email }) });
      localStorage.setItem('token', out.token);
      setMsg('Account created.');
    } catch (e) { setMsg(e.message); }
  };
  return <div><h2>Create Account</h2><input value={email} onChange={e=>setEmail(e.target.value)} placeholder="Email"/><button onClick={submit}>Connect wallet + Save</button><p>{msg}</p></div>;
}
