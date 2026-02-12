const BASE = import.meta.env.VITE_BACKEND_URL;

export async function api(path, options = {}) {
  const token = localStorage.getItem('token');
  const res = await fetch(`${BASE}${path}`, {
    headers: {
      'Content-Type': 'application/json',
      ...(token ? { Authorization: `Bearer ${token}` } : {}),
      ...(options.headers || {})
    },
    ...options
  });
  if (!res.ok) throw new Error((await res.json()).error || 'Request failed');
  return res.json();
}
