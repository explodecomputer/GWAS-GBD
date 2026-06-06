import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'

// Set VITE_BASE env var to override (e.g. '/' for a custom domain)
const base = process.env.VITE_BASE ?? '/GWAS-GBD/'

export default defineConfig({
  base,
  plugins: [vue()],
  build: {
    outDir: '../docs',
    emptyOutDir: true,
  },
})
