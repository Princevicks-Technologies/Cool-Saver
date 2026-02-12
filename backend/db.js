import Database from 'better-sqlite3';
import fs from 'fs';

export function createDb(dbPath, schemaPath) {
  const db = new Database(dbPath);
  const schema = fs.readFileSync(schemaPath, 'utf8');
  db.exec(schema);
  return db;
}

export const asJsonSafe = (obj) => JSON.stringify(obj, (_, v) => (typeof v === 'bigint' ? v.toString() : v));
