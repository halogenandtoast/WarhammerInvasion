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
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 1rem;
  padding: 0.6rem 1.5rem;
  background: var(--bg-elev);
  border-bottom: 1px solid var(--border);
  position: sticky;
  top: 0;
  z-index: var(--z-topnav);
}

.brand {
  display: inline-flex;
  align-items: center;
  gap: 0.55rem;
  color: var(--fg);
  font-weight: 600;
  letter-spacing: 0.02em;
}
.brand:hover {
  text-decoration: none;
  color: var(--accent-strong);
}

.brand-mark {
  font-size: 1.2rem;
  color: var(--accent-strong);
}

.nav-links {
  list-style: none;
  margin: 0;
  padding: 0;
  display: flex;
  gap: 0.25rem;
}

.nav-links a {
  display: inline-flex;
  align-items: center;
  min-height: var(--tap-target);
  padding: 0 0.85rem;
  border-radius: var(--radius-md);
  color: var(--fg-dim);
  font-size: 0.95rem;
}

.nav-links a:hover {
  background: var(--bg-elev-2);
  color: var(--fg);
  text-decoration: none;
}

.nav-links a.active {
  color: var(--fg);
  background: var(--bg-elev-2);
  box-shadow: inset 0 -2px 0 var(--accent);
}

@media (max-width: 480px) {
  .topnav {
    padding: 0.5rem 1rem;
  }
  .brand-text {
    display: none;
  }
}
</style>
