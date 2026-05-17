<script setup lang="ts">
// Merged engine-log + player-chat stream in the side rail of the
// in-game page. Sorted chronologically; log lines render terse, chat
// lines render as tinted bubbles in the speaker's race colour.
//
// The parent passes already-merged + sorted messages and the
// race-class lookup so this component stays pure-render.

import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import type { ChatLine, LogEntry } from '../api/protocol'
import { formatTime } from '../lib/format'
import { logCategoryClass } from '../lib/gameLog'
import { useAutoScroll } from '../composables/useAutoScroll'

export type Message =
  | { kind: 'log'; at: string; entry: LogEntry }
  | { kind: 'chat'; at: string; line: ChatLine }

const props = defineProps<{
  messages: Message[]
  // Resolves a chat author's userId to a `race-<slug>` class (or null
  // for unknown / system speakers).
  raceClassOf: (userId: string) => string | null
  // The parent owns log entry rendering — it has the t() bound seat
  // list and ENUM_PARAM_TABLE lookups.
  formatLogEntry: (entry: LogEntry) => string
}>()

const { t } = useI18n({ useScope: 'global' })

const scrollEl = useAutoScroll(() => props.messages.length)
const isEmpty = computed(() => props.messages.length === 0)
</script>

<template>
  <div ref="scrollEl" class="messages-scroll">
    <div v-if="isEmpty" class="messages-empty">
      {{ t('game.messages.empty') }}
    </div>
    <ol v-else class="messages-list" role="log" aria-live="polite">
      <template v-for="(msg, i) in messages" :key="`${msg.at}-${i}`">
        <li
          v-if="msg.kind === 'log'"
          class="log-line"
          :class="logCategoryClass(msg.entry.category)"
        >
          <time class="log-time">{{ formatTime(msg.entry.at) }}</time>
          <p class="log-text">{{ formatLogEntry(msg.entry) }}</p>
        </li>
        <li
          v-else
          class="chat-line"
          :class="raceClassOf(msg.line.from.userId)"
        >
          <div class="chat-meta">
            <span class="chat-author">{{ msg.line.from.displayName }}</span>
            <time class="chat-time">{{ formatTime(msg.line.at) }}</time>
          </div>
          <p class="chat-text">{{ msg.line.text }}</p>
        </li>
      </template>
    </ol>
  </div>
</template>

<style scoped>
.messages-scroll {
  flex: 1;
  overflow-y: auto;
  padding: 0.5rem 0.75rem;
  min-height: 0;
}
.messages-empty {
  margin: auto;
  color: var(--fg-faint);
  font-style: italic;
  text-align: center;
  padding: 1rem 0;
}
.messages-list {
  list-style: none;
  margin: 0;
  padding: 0;
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
  font-size: 0.82rem;
  line-height: 1.35;
}

/* ─── log entries (engine transcript) ─── */
.log-line {
  display: grid;
  grid-template-columns: 3rem 1fr;
  column-gap: 0.45rem;
  align-items: baseline;
  border-left: 2px solid transparent;
  padding-left: 0.4rem;
  color: var(--fg-dim);
}
.log-time { color: var(--fg-faint); font-size: 0.68rem; font-variant-numeric: tabular-nums; }
.log-text { margin: 0; word-break: break-word; }
.log-line.cat-turn   { border-left-color: var(--accent); color: var(--fg); font-weight: 600; }
.log-line.cat-phase  { border-left-color: var(--race-empire, #d4b357); color: var(--fg); }
.log-line.cat-action { border-left-color: var(--accent-strong, #c4634a); color: var(--fg); }
.log-line.cat-result { border-left-color: var(--accent-strong, #c4634a); color: var(--fg); font-weight: 600; }
.log-line.cat-system { /* default colors */ }

/* ─── chat lines (player messages) ─── */
.chat-line {
  --chat-hue: var(--accent);
  --chat-author: var(--accent-strong);
  --chat-bg: rgba(196, 99, 74, 0.10);
  --chat-ring: rgba(196, 99, 74, 0.18);
  display: flex;
  flex-direction: column;
  gap: 0.15rem;
  padding: 0.45rem 0.6rem;
  margin: 0.25rem 0;
  background: var(--chat-bg);
  border-left: 3px solid var(--chat-hue);
  border-radius: var(--radius-sm);
  box-shadow: 0 0 0 1px var(--chat-ring);
}
.chat-line.race-empire {
  --chat-hue: var(--race-empire);
  --chat-author: var(--race-empire-strong);
  --chat-bg: var(--race-empire-tint);
  --chat-ring: var(--race-empire-ring);
}
.chat-line.race-dwarf {
  --chat-hue: var(--race-dwarf);
  --chat-author: var(--race-dwarf-strong);
  --chat-bg: var(--race-dwarf-tint);
  --chat-ring: var(--race-dwarf-ring);
}
.chat-line.race-high-elf {
  --chat-hue: var(--race-high-elf);
  --chat-author: var(--race-high-elf-strong);
  --chat-bg: var(--race-high-elf-tint);
  --chat-ring: var(--race-high-elf-ring);
}
.chat-line.race-chaos {
  --chat-hue: var(--race-chaos);
  --chat-author: var(--race-chaos-strong);
  --chat-bg: var(--race-chaos-tint);
  --chat-ring: var(--race-chaos-ring);
}
.chat-line.race-orc {
  --chat-hue: var(--race-orc);
  --chat-author: var(--race-orc-strong);
  --chat-bg: var(--race-orc-tint);
  --chat-ring: var(--race-orc-ring);
}
.chat-line.race-dark-elf {
  --chat-hue: var(--race-dark-elf);
  --chat-author: var(--race-dark-elf-strong);
  --chat-bg: var(--race-dark-elf-tint);
  --chat-ring: var(--race-dark-elf-ring);
}
.chat-line .chat-author { color: var(--chat-author); }
.chat-meta { display: flex; align-items: baseline; gap: 0.4rem; }
.chat-author {
  font-weight: 600;
  font-size: 0.86rem;
  color: var(--fg);
}
.chat-time { color: var(--fg-faint); font-size: 0.7rem; font-variant-numeric: tabular-nums; }
.chat-text {
  margin: 0;
  font-size: 0.9rem;
  line-height: 1.4;
  word-break: break-word;
  color: var(--fg);
}
</style>
