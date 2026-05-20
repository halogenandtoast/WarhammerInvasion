<script setup lang="ts">
import { computed, onMounted, onUnmounted } from 'vue'
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
import { gateRoute, installRouter, navigate, route, uninstallRouter } from './router'

const { t } = useI18n({ useScope: 'global' })

function navClick(e: Event, target: string) {
  e.preventDefault()
  navigate(target)
}

async function doLogout() {
  await auth.logout()
  navigate('#/login')
}

// Public nav: always visible. "Decks" requires sign-in; "Play" links
// to the lobby and is open to guests (they spectate from there).
type NavKey = 'rules' | 'cards' | 'decks' | 'play'
const navItems = computed<{ id: string; href: string; key: NavKey }[]>(() => {
  const base: { id: string; href: string; key: NavKey }[] = [
    { id: 'play', href: '#/lobby', key: 'play' },
    { id: 'rules', href: '#/rules', key: 'rules' },
    { id: 'cards', href: '#/cards', key: 'cards' },
  ]
  if (auth.isAuthenticated.value) {
    base.push({ id: 'decks', href: '#/decks', key: 'decks' })
  }
  return base
})

const effectiveRoute = computed(() =>
  gateRoute(route.value, {
    isAuthenticated: auth.isAuthenticated.value,
    ready: auth.ready.value,
  }),
)

const view = computed(() => {
  const r = effectiveRoute.value
  switch (r.name) {
    case 'rules': return { component: Rules, props: {} as Record<string, unknown> }
    case 'cards': return { component: Cards, props: {} }
    case 'login': return { component: Login, props: {} }
    case 'register': return { component: Register, props: {} }
    case 'decks': return { component: Decks, props: {} }
    case 'deck-view': return { component: DeckView, props: { deckId: r.id } }
    case 'deck-edit': return { component: DeckEdit, props: { deckId: r.id } }
    case 'lobby':
      // Lobby is the home page — signed in or not. Guests get a
      // read-only view (chat + games), with sign-in CTAs for the
      // affordances they can't use.
      return { component: Lobby, props: {} }
    case 'game':
      // Guests are spectator-only; the game socket accepts them on
      // games that have spectators enabled. Auth-gating happens in
      // the lobby UI before they navigate here.
      return {
        component: Game,
        props: { gameId: r.id, inviteToken: r.inviteToken, password: r.password },
      }
  }
})

const NAV_ACTIVE: Record<NavKey, ReadonlySet<string>> = {
  play: new Set(['lobby', 'game']),
  rules: new Set(['rules']),
  cards: new Set(['cards']),
  decks: new Set(['decks', 'deck-view', 'deck-edit']),
}

const isActive = (key: NavKey): boolean => NAV_ACTIVE[key].has(route.value.name)

onMounted(async () => {
  installRouter()
  await auth.bootstrap()
})
onUnmounted(uninstallRouter)
</script>

<template>
  <div class="app-shell">
    <nav class="topnav" :aria-label="t('app.nav.primary_label')">
      <div class="topnav-inner">
        <a class="brand" href="#/" @click="navClick($event, '#/')">
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
    <component :is="view.component" v-bind="view.props" />
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
  color: var(--fg);
  font-weight: 600;
  font-size: 0.95rem;
  letter-spacing: 0.01em;
}
.brand:hover {
  text-decoration: none;
  color: var(--accent-strong);
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
