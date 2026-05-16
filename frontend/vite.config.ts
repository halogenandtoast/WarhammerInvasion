import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'

const backendPort = process.env.BACKEND_PORT ?? '3000'

// https://vite.dev/config/
export default defineConfig({
  plugins: [vue()],
  server: {
    proxy: {
      '/api': {
        target: `http://localhost:${backendPort}`,
        changeOrigin: true,
      },
      '/ws': {
        target: `ws://localhost:${backendPort}`,
        ws: true,
        changeOrigin: true,
      },
    },
  },
})
