import React from 'react';
import { Link } from 'react-router-dom';

export default function Home() {
  return <div>
    <section className="hero">
      <h2>Build better saving habits</h2>
      <p>Create secure ADA vaults with clear goals and lock rules.</p>
      <div><Link className="btn" to="/app/create">Create Vault</Link> <Link className="btn ghost" to="/login">Connect Wallet</Link></div>
    </section>
    <section><h3>How it works</h3><ol><li>Create your vault rule.</li><li>Deposit ADA anytime (friends can sponsor too).</li><li>Withdraw when your rule is satisfied.</li></ol></section>
    <section><h3>Vault modes</h3><div className="grid"><article>Time-Locked</article><article>Target-Locked</article><article>Hybrid</article></div></section>
    <section><h3>Policy</h3><p><b>Strict</b>: no early exits. <b>Non-Strict</b>: early owner withdrawal with 3% treasury penalty.</p></section>
  </div>;
}
