import React from 'react';
import { Navigate, Route, Routes } from 'react-router-dom';
import Layout from './components/Layout';
import Home from './pages/Home';
import About from './pages/About';
import Register from './pages/Register';
import Login from './pages/Login';
import Dashboard from './pages/Dashboard';
import CreateVault from './pages/CreateVault';
import VaultDetails from './pages/VaultDetails';
import PublicVault from './pages/PublicVault';
import Settings from './pages/Settings';
import Terms from './pages/Terms';
import Privacy from './pages/Privacy';

export default function App() {
  return (
    <Layout>
      <Routes>
        <Route path='/' element={<Home />} />
        <Route path='/about' element={<About />} />
        <Route path='/register' element={<Register />} />
        <Route path='/login' element={<Login />} />
        <Route path='/app' element={<Dashboard />} />
        <Route path='/app/create' element={<CreateVault />} />
        <Route path='/app/vault/:vaultRef' element={<VaultDetails />} />
        <Route path='/v/:vaultRef' element={<PublicVault />} />
        <Route path='/app/settings' element={<Settings />} />
        <Route path='/terms' element={<Terms />} />
        <Route path='/privacy' element={<Privacy />} />
        <Route path='*' element={<Navigate to='/' />} />
      </Routes>
    </Layout>
  );
}
