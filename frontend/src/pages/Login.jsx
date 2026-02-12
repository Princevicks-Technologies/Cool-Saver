import React, { useState } from 'react';
import { api } from '../lib/api';
import { connectWallet, getWalletAddress, initLucid } from '../lib/lucidClient';

export default function Login() {
  const [msg, setMsg] = useState('');
  const submit = async () => {
    try {
      await initLucid({ network: import.meta.env.VITE_NETWORK, blockfrostUrl: import.meta.env.VITE_BLOCKFROST_URL, blockfrostKey: import.meta.env.VITE_BLOCKFROST_KEY });
      await connectWallet('nami');
      const walletAddress = await getWalletAddress();
      const out = await api('/api/auth/login', { method: 'POST', body: JSON.stringify({ walletAddress }) });
      localStorage.setItem('token', out.token);
      setMsg('Logged in.');
    } catch (e) { setMsg(e.message); }
  };
  return <div><h2>Login</h2><button onClick={submit}>Connect Wallet</button><p>{msg}</p></div>;
}
