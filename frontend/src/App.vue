<script setup lang="ts">
import { computed, onMounted, onUnmounted, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import Rules from './views/Rules.vue'
import Cards from './views/Cards.vue'
import Login from './views/Login.vue'
import Register from './views/Register.vue'
import Decks from './views/Decks.vue'
import DeckEdit from './views/DeckEdit.vue'
import DeckView from './views/DeckView.vue'
import Lobby from './views/Lobby.vue'
import Game from './views/Game.vue'
import { auth } from './stores/auth'

type Route =
  | { name: 'rules' }
  | { name: 'cards' }
  | { name: 'login' }
  | { name: 'register' }
  | { name: 'decks' }
  | { name: 'deck-view'; id: string }
  | { name: 'deck-edit'; id: string }
  | { name: 'lobby' }
  | { name: 'game'; id: string; inviteToken: string | null; password: string | null }

const { t } = useI18n({ useScope: 'global' })

function parseRoute(): Route {
  const hash = window.location.hash.replace(/^#\/?/, '')
  if (hash.startsWith('cards')) return { name: 'cards' }
  if (hash === 'login') return { name: 'login' }
  if (hash === 'register') return { name: 'register' }
  if (hash === 'decks') return { name: 'decks' }
  if (hash === 'lobby') return { name: 'lobby' }
  const gameMatch = /^games\/([\w-]+)(?:\?(.*))?$/.exec(hash)
  if (gameMatch) {
    const id = gameMatch[1]
    const qs = new URLSearchParams(gameMatch[2] ?? '')
    return {
      name: 'game',
      id,
      inviteToken: qs.get('t'),
      password: qs.get('password'),
    }
  }
  const edit = /^decks\/([\w-]+)\/edit$/.exec(hash)
  if (edit) return { name: 'deck-edit', id: edit[1] }
  const view = /^decks\/([\w-]+)$/.exec(hash)
  if (view) return { name: 'deck-view', id: view[1] }
  return { name: 'rules' }
}

const route = ref<Route>(parseRoute())

function onHashChange() {
  route.value = parseRoute()
}

function navigate(target: string) {
  if (target.startsWith('#')) {
    window.location.hash = target.slice(1)
  } else {
    window.location.hash = target
  }
  route.value = parseRoute()
}

function navClick(e: Event, target: string) {
  e.preventDefault()
  navigate(target)
}

async function doLogout() {
  await auth.logout()
  navigate('#/login')
}

// Public nav: always visible. Decks/Play show up once signed in.
type NavKey = 'rules' | 'cards' | 'decks' | 'play'
const navItems = computed<{ id: string; href: string; key: NavKey }[]>(() => {
  const base: { id: string; href: string; key: NavKey }[] = [
    { id: 'rules', href: '#/', key: 'rules' },
    { id: 'cards', href: '#/cards', key: 'cards' },
  ]
  if (auth.isAuthenticated.value) {
    base.push({ id: 'play', href: '#/lobby', key: 'play' })
    base.push({ id: 'decks', href: '#/decks', key: 'decks' })
  }
  return base
})

const view = computed(() => {
  switch (route.value.name) {
    case 'cards':
      return { component: Cards, props: {} as Record<string, unknown> }
    case 'login':
      return { component: Login, props: {} }
    case 'register':
      return { component: Register, props: {} }
    case 'decks':
      if (!auth.isAuthenticated.value && auth.ready.value) {
        navigate('#/login')
        return { component: Login, props: {} }
      }
      return { component: Decks, props: {} }
    case 'deck-view':
      if (!auth.isAuthenticated.value && auth.ready.value) {
        navigate('#/login')
        return { component: Login, props: {} }
      }
      return { component: DeckView, props: { deckId: route.value.id } }
    case 'deck-edit':
      if (!auth.isAuthenticated.value && auth.ready.value) {
        navigate('#/login')
        return { component: Login, props: {} }
      }
      return { component: DeckEdit, props: { deckId: route.value.id } }
    case 'lobby':
      if (!auth.isAuthenticated.value && auth.ready.value) {
        navigate('#/login')
        return { component: Login, props: {} }
      }
      return { component: Lobby, props: {} }
    case 'game':
      if (!auth.isAuthenticated.value && auth.ready.value) {
        navigate('#/login')
        return { component: Login, props: {} }
      }
      return {
        component: Game,
        props: {
          gameId: route.value.id,
          inviteToken: route.value.inviteToken,
          password: route.value.password,
        },
      }
    default:
      return { component: Rules, props: {} }
  }
})

const isActive = (key: string): boolean => {
  if (key === 'rules') return route.value.name === 'rules'
  if (key === 'cards') return route.value.name === 'cards'
  if (key === 'decks')
    return (
      route.value.name === 'decks' ||
      route.value.name === 'deck-view' ||
      route.value.name === 'deck-edit'
    )
  if (key === 'play') return route.value.name === 'lobby' || route.value.name === 'game'
  return false
}

onMounted(async () => {
  window.addEventListener('hashchange', onHashChange)
  await auth.bootstrap()
  // Re-evaluate the current route now that we know the auth state.
  route.value = parseRoute()
})
onUnmounted(() => window.removeEventListener('hashchange', onHashChange))
</script>

<template>
  <div class="app-shell">
    <nav class="topnav" :aria-label="t('app.nav.primary_label')">
      <div class="topnav-inner">
        <a
          class="brand"
          :href="auth.isAuthenticated.value ? '#/lobby' : '#'"
          @click="navClick($event, auth.isAuthenticated.value ? '#/lobby' : '#/')"
        >
          <span class="brand-mark" aria-hidden="true">⚔</span>
          <span class="brand-text">{{ t('app.brand') }}</span>
        </a>
        <ul class="nav-links">
          <li v-for="item in navItems" :key="item.id">
            <a
              :href="item.href"
              :class="{ active: isActive(item.key) }"
              :aria-current="isActive(item.key) ? 'page' : undefined"
              @click="navClick($event, item.href)"
            >
              {{ t(`app.nav.${item.id}`) }}
            </a>
          </li>
        </ul>
        <div class="auth-area">
          <template v-if="auth.isAuthenticated.value">
            <span class="hello">{{ auth.user.value?.displayName }}</span>
            <button type="button" class="auth-button" @click="doLogout">
              {{ t('app.nav.logout') }}
            </button>
          </template>
          <template v-else>
            <a href="#/login" class="auth-link" :class="{ active: route.name === 'login' }" @click="navClick($event, '#/login')">
              {{ t('app.nav.login') }}
            </a>
            <a href="#/register" class="auth-button accent" @click="navClick($event, '#/register')">
              {{ t('app.nav.register') }}
            </a>
          </template>
        </div>
      </div>
    </nav>
    <component :is="view.component" v-bind="view.props" @navigate="navigate" />
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
  flex: 1;
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

.auth-area {
  display: flex;
  align-items: center;
  gap: 0.5rem;
}

.hello {
  font-size: 0.85rem;
  color: var(--fg-dim);
  max-width: 14ch;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.auth-link {
  display: inline-flex;
  align-items: center;
  height: 32px;
  padding: 0 0.7rem;
  color: var(--fg-dim);
  border-radius: var(--radius-pill);
  font-size: 0.85rem;
}

.auth-link:hover {
  color: var(--fg);
  text-decoration: none;
}

.auth-link.active {
  color: var(--fg);
}

.auth-button {
  height: 32px;
  padding: 0 0.85rem;
  background: transparent;
  color: var(--fg-dim);
  border: 1px solid var(--border);
  border-radius: var(--radius-pill);
  font-size: 0.85rem;
  cursor: pointer;
}

.auth-button:hover {
  color: var(--fg);
  border-color: var(--fg-dim);
}

.auth-button.accent {
  background: var(--accent);
  border-color: var(--accent);
  color: var(--on-accent);
  text-decoration: none;
  display: inline-flex;
  align-items: center;
}

.auth-button.accent:hover {
  background: var(--accent-strong);
  border-color: var(--accent-strong);
  text-decoration: none;
}

@media (pointer: coarse) {
  .nav-links a,
  .auth-link,
  .auth-button {
    min-height: var(--tap-target);
    height: auto;
    padding: 0.5rem 0.9rem;
  }
}

@media (max-width: 540px) {
  .topnav-inner {
    height: 44px;
    padding: 0 0.875rem;
    gap: 0.5rem;
  }
  .brand-text,
  .hello {
    display: none;
  }
  .nav-links {
    flex: 0 1 auto;
  }
}
</style>
