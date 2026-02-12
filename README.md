# Cool Saver — ADA-Only Savings Vault

Production-oriented fullstack Cardano dApp with:
- Plutus V2 validator (`/onchain`)
- Lucid.js offchain transaction module (`/offchain/lucid.js`)
- Express + SQLite metadata/notification backend (`/backend`)
- React + Vite multi-page frontend (`/frontend`)

## Features
- Savings modes: Time-Locked, Target-Locked, Hybrid
- Policies: Strict and Non-Strict (3% force-withdraw penalty)
- ADA only
- Sponsored deposits
- Owner-only withdrawals
- Optional beneficiary payout
- Public/private + purpose labels (off-chain indexing)
- Unlock eligibility email notifications

---

## 1) Compile validator and generate CBOR/hex

Prerequisites: GHC/Cabal + Plutus V2 libraries.

```bash
# from repo root
cabal run compile-validator
```

This writes:
- `artifacts/validator.cbor`
- `artifacts/validator.hex`

> `onchain/Compile.hs` is set to `penaltyBps = 300` and expects a treasury PKH.

---

## 2) Backend setup (Blockfrost preprod + API)

```bash
cd backend
cp .env.example .env
npm install
npm run start
```

Required env values:
- `JWT_SECRET`
- `BLOCKFROST_URL=https://cardano-preprod.blockfrost.io/api/v0`
- `BLOCKFROST_KEY`
- `SCRIPT_ADDRESS` (from your compiled validator)

Database is SQLite (`DB_PATH`, defaults to `./data.db`).

---

## 3) Frontend setup

```bash
cd frontend
cp .env.example .env
npm install
npm run dev
```

Required env values:
- `VITE_NETWORK=Preprod`
- `VITE_BLOCKFROST_URL`
- `VITE_BLOCKFROST_KEY`
- `VITE_BACKEND_URL=http://localhost:4000`
- `VITE_TREASURY_ADDRESS`

Wallets supported through CIP-30 injection (Nami/Eternl/Lace compatible).

---

## 4) Notifier setup

Run notifier manually or with cron:

```bash
cd backend
npm run notifier
```

What it does:
1. Polls script UTxOs from Blockfrost
2. Checks mode unlock conditions against on-chain lovelace and time
3. Sends unlock eligibility email once per vault subscription

Email adapter is provider-agnostic (`backend/emailProvider.js`) with:
- `console` (default)
- `sendgrid`
- `mailgun`
- `ses`

---

## 5) End-to-end preprod testing flow

1. Register and login in UI (`/register`, `/login`).
2. Create vault in `/app/create`.
3. Confirm metadata is stored in backend (`GET /api/vaults?owner=<pkh>`).
4. Sponsor deposit via `/v/:vaultRef` or vault detail page.
5. Withdraw when eligible or force-withdraw if non-strict.
6. Subscribe to email unlock endpoint and run notifier script.

---

## Security notes and limitations

- On-chain data is public; “private” only affects app indexing visibility.
- Frontend/backend never custody private keys.
- Non-strict force-withdraw incurs treasury penalty (3%).
- Validator intentionally keeps checks minimal for script size.
- You should audit and benchmark script before production rollout.
- Replace default treasury PKH and test vectors before mainnet.
