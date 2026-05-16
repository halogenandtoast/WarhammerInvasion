<script setup lang="ts">
import { computed, onMounted, onUnmounted, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import Rules from './views/Rules.vue'
import Cards from './views/Cards.vue'

type Route = 'rules' | 'cards'

const { t } = useI18n({ useScope: 'global' })

const navItems: readonly Route[] = ['rules', 'cards'] as const

function parseRoute(): Route {
  const hash = window.location.hash.replace(/^#\/?/, '')
  if (hash.startsWith('cards')) return 'cards'
  return 'rules'
}

const route = ref<Route>(parseRoute())
const view = computed(() => (route.value === 'cards' ? Cards : Rules))

function onHashChange() {
  route.value = parseRoute()
}

function navigate(e: Event, target: Route) {
  e.preventDefault()
  window.location.hash = target === 'rules' ? '' : '#/cards'
  route.value = target
}

onMounted(() => window.addEventListener('hashchange', onHashChange))
onUnmounted(() => window.removeEventListener('hashchange', onHashChange))
</script>

<template>
  <div class="app-shell">
    <nav class="topnav" :aria-label="t('app.nav.primary_label')">
      <div class="topnav-inner">
        <a class="brand" href="#" @click="navigate($event, 'rules')">
          <span class="brand-mark" aria-hidden="true">⚔</span>
          <span class="brand-text">{{ t('app.brand') }}</span>
        </a>
        <ul class="nav-links">
          <li v-for="id in navItems" :key="id">
            <a
              :href="id === 'rules' ? '#' : '#/cards'"
              :class="{ active: route === id }"
              :aria-current="route === id ? 'page' : undefined"
              @click="navigate($event, id)"
            >
              {{ t(`app.nav.${id}`) }}
            </a>
          </li>
        </ul>
      </div>
    </nav>
    <component :is="view" />
  </div>
</template>

<style scoped>
.app-shell {
  display: flex;
  flex-direction: column;
  min-height: 100dvh;
}

.topnav {
  position: sticky;
  top: 0;
  z-index: var(--z-topnav);
  background: color-mix(in srgb, var(--bg-elev) 78%, transparent);
  backdrop-filter: saturate(160%) blur(12px);
  -webkit-backdrop-filter: saturate(160%) blur(12px);
  border-bottom: 1px solid var(--border);
}

.topnav-inner {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 1.25rem;
  height: 48px;
  padding: 0 1.25rem;
}

.brand {
  display: inline-flex;
  align-items: center;
  gap: 0.5rem;
  color: var(--fg);
  font-weight: 600;
  font-size: 0.95rem;
  letter-spacing: 0.01em;
}
.brand:hover {
  text-decoration: none;
  color: var(--accent-strong);
}

.brand-mark {
  font-size: 1.05rem;
  color: var(--accent-strong);
  line-height: 1;
}

.nav-links {
  list-style: none;
  margin: 0;
  padding: 0;
  display: flex;
  gap: 0.125rem;
}

.nav-links a {
  position: relative;
  display: inline-flex;
  align-items: center;
  height: 32px;
  padding: 0 0.75rem;
  border-radius: var(--radius-pill);
  color: var(--fg-dim);
  font-size: 0.875rem;
  font-weight: 500;
  letter-spacing: 0.01em;
  transition: color var(--transition-fast), background-color var(--transition-fast);
}

.nav-links a:hover {
  color: var(--fg);
  text-decoration: none;
}

.nav-links a.active {
  color: var(--fg);
  background: color-mix(in srgb, var(--fg) 8%, transparent);
}

/* Restore mobile tap targets without bulking up the bar visually */
@media (pointer: coarse) {
  .nav-links a {
    min-height: var(--tap-target);
    height: auto;
    padding: 0.5rem 0.9rem;
  }
}

@media (max-width: 480px) {
  .topnav-inner {
    height: 44px;
    padding: 0 0.875rem;
    gap: 0.5rem;
  }
  .brand-text {
    display: none;
  }
}
</style>
