<script setup lang="ts">
// Shared wrapper for the sign-in / register forms. Both forms have the
// same outer chrome (centered card, eyebrow + heading + lead, error
// strip, footer "switch" link); only the body fields differ.
//
// Slot contract:
//   #default — the form body itself (label/inputs, submit button)
//   #switch  — the footer "no account yet? / already a member?" link
//
// The form's <form> element is owned by this component so the submit
// behaviour and the error banner stay consistent across both views.

defineProps<{
  eyebrow: string
  heading: string
  lead: string
  error: string | null
}>()

const emit = defineEmits<{ (e: 'submit'): void }>()
</script>

<template>
  <main class="auth-page">
    <form class="auth-card" novalidate @submit.prevent="emit('submit')">
      <header>
        <p class="eyebrow">{{ eyebrow }}</p>
        <h1>{{ heading }}</h1>
        <p class="lead">{{ lead }}</p>
      </header>

      <slot />

      <p v-if="error" class="error">{{ error }}</p>

      <p class="switch"><slot name="switch" /></p>
    </form>
  </main>
</template>

<style scoped>
.auth-page {
  display: grid;
  place-items: start center;
  padding: 4rem 1rem;
  min-height: calc(100dvh - 60px);
  background: var(--bg);
}

.auth-card {
  width: 100%;
  max-width: 420px;
  display: flex;
  flex-direction: column;
  gap: 1rem;
  padding: 2rem;
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-xl);
}

.eyebrow {
  margin: 0;
  font-size: 0.72rem;
  letter-spacing: 0.18em;
  text-transform: uppercase;
  color: var(--accent-strong);
}

h1 {
  margin: 0.4rem 0 0.3rem;
  font-size: 1.6rem;
  color: var(--fg);
}

.lead {
  margin: 0;
  color: var(--fg-dim);
}

.switch {
  margin: 0;
  color: var(--fg-dim);
  font-size: 0.9rem;
  text-align: center;
}

.switch :deep(a) {
  color: var(--accent-strong);
  text-decoration: none;
}

.switch :deep(a:hover) {
  text-decoration: underline;
}

.error {
  margin: 0;
  padding: 0.55rem 0.75rem;
  border-left: 3px solid var(--accent-strong);
  background: var(--bg);
  color: var(--fg);
  border-radius: var(--radius-sm);
  font-size: 0.9rem;
}

:slotted(.field) {
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
}

:slotted(.field-label) {
  font-size: 0.72rem;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  color: var(--fg-faint);
}

:slotted(.field input) {
  min-height: var(--tap-target);
  padding: 0.55rem 0.85rem;
  background: var(--bg);
  color: var(--fg);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  font-size: 1rem;
}

:slotted(.field input:focus-visible) {
  outline: 2px solid var(--accent-strong);
  outline-offset: 1px;
  border-color: var(--accent-strong);
}

:slotted(.hint) {
  font-size: 0.78rem;
  color: var(--fg-faint);
}

:slotted(.primary) {
  min-height: var(--tap-target);
  padding: 0 1rem;
  background: var(--accent);
  border: 1px solid var(--accent);
  color: var(--on-accent);
  border-radius: var(--radius-md);
  font-size: 1rem;
  cursor: pointer;
}

:slotted(.primary:disabled) {
  opacity: 0.6;
  cursor: progress;
}

:slotted(.primary:hover:not(:disabled)) {
  background: var(--accent-strong);
  border-color: var(--accent-strong);
}
</style>
