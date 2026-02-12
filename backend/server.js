import express from 'express';
import cors from 'cors';
import jwt from 'jsonwebtoken';
import dotenv from 'dotenv';
import { createDb } from './db.js';

dotenv.config();
const app = express();
const port = Number(process.env.PORT || 4000);
const db = createDb(process.env.DB_PATH || './data.db', './schema.sql');

app.use(cors());
app.use(express.json());

const sign = (walletAddress) => jwt.sign({ walletAddress }, process.env.JWT_SECRET || 'dev_secret', { expiresIn: '7d' });

function auth(req, res, next) {
  const token = req.headers.authorization?.replace('Bearer ', '');
  if (!token) return res.status(401).json({ error: 'Missing token' });
  try {
    req.user = jwt.verify(token, process.env.JWT_SECRET || 'dev_secret');
    next();
  } catch {
    res.status(401).json({ error: 'Invalid token' });
  }
}

app.post('/api/auth/register', (req, res) => {
  const { walletAddress, pkh, email } = req.body;
  if (!walletAddress || !pkh || !email) return res.status(400).json({ error: 'walletAddress, pkh, email required' });
  const now = new Date().toISOString();
  db.prepare('INSERT OR REPLACE INTO users(walletAddress, pkh, email, createdAt) VALUES(?,?,?,?)').run(walletAddress, pkh, email, now);
  res.json({ ok: true, token: sign(walletAddress) });
});

app.post('/api/auth/login', (req, res) => {
  const { walletAddress } = req.body;
  const user = db.prepare('SELECT * FROM users WHERE walletAddress=?').get(walletAddress);
  if (!user) return res.status(404).json({ error: 'Account not found' });
  res.json({ token: sign(walletAddress), user });
});

app.get('/api/user/me', auth, (req, res) => {
  const user = db.prepare('SELECT * FROM users WHERE walletAddress=?').get(req.user.walletAddress);
  res.json({ user });
});

app.get('/api/vaults', (req, res) => {
  const owner = req.query.owner;
  if (!owner) return res.status(400).json({ error: 'owner required' });
  const rows = db.prepare('SELECT * FROM vault_metadata WHERE ownerPkh=? ORDER BY createdAt DESC').all(owner);
  res.json(rows);
});

app.get('/api/vaults/public', (_req, res) => {
  const rows = db.prepare('SELECT * FROM vault_metadata WHERE isPublic=1 ORDER BY createdAt DESC').all();
  res.json(rows);
});

app.get('/api/vaults/:vaultRef', (req, res) => {
  const row = db.prepare('SELECT * FROM vault_metadata WHERE vaultRef=?').get(req.params.vaultRef);
  if (!row) return res.status(404).json({ error: 'Not found' });
  res.json(row);
});

app.post('/api/vaults/:vaultRef', auth, (req, res) => {
  const { ownerPkh, isPublic, purposeLabel, beneficiary, mode, policy, target, unlockTime } = req.body;
  const createdAt = new Date().toISOString();
  db.prepare(`INSERT OR REPLACE INTO vault_metadata
    (vaultRef, ownerPkh, isPublic, purposeLabel, beneficiary, mode, policy, target, unlockTime, createdAt)
    VALUES(?,?,?,?,?,?,?,?,?,?)`).run(req.params.vaultRef, ownerPkh, Number(!!isPublic), purposeLabel, beneficiary || '', mode, policy, String(target), String(unlockTime), createdAt);
  res.json({ ok: true });
});

app.post('/api/vaults/:vaultRef/subscribe-unlock-email', auth, (req, res) => {
  const user = db.prepare('SELECT * FROM users WHERE walletAddress=?').get(req.user.walletAddress);
  if (!user) return res.status(404).json({ error: 'User missing' });
  db.prepare('INSERT OR IGNORE INTO unlock_subscriptions(vaultRef,email,createdAt) VALUES(?,?,?)')
    .run(req.params.vaultRef, user.email, new Date().toISOString());
  res.json({ ok: true });
});

app.listen(port, () => console.log(`Backend running on :${port}`));
