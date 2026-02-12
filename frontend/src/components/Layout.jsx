import React from 'react';
import { Link } from 'react-router-dom';

export default function Layout({ children }) {
  return (
    <div className="shell">
      <header>
        <h1>Cool Saver</h1>
        <nav>
          <Link to="/">Home</Link>
          <Link to="/about">About</Link>
          <Link to="/app">Dashboard</Link>
          <Link to="/terms">Terms</Link>
          <Link to="/privacy">Privacy</Link>
        </nav>
      </header>
      <main>{children}</main>
      <footer>© Cool Saver • Save with confidence</footer>
    </div>
  );
}
