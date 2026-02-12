import React, { useState } from 'react';

export default function Settings() {
  const [emailUnlock, setEmailUnlock] = useState(true);
  return <div><h2>Settings</h2><label>Email unlock notifications <input type='checkbox' checked={emailUnlock} onChange={e=>setEmailUnlock(e.target.checked)} /></label></div>;
}
