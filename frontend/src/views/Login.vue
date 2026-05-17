<script setup lang="ts">
import { ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { auth, AuthError } from '../stores/auth'
import { navigate } from '../router'
import AuthCard from '../components/AuthCard.vue'

const { t } = useI18n({ useScope: 'global' })

const email = ref('')
const password = ref('')
const busy = ref(false)
const error = ref<string | null>(null)

const KNOWN_ERRORS = new Set(['invalid_credentials', 'invalid_email'])

function errorLabel(code: string): string {
  if (KNOWN_ERRORS.has(code)) return t(`auth.errors.${code}`)
  return code
}

async function submit() {
  if (busy.value) return
  busy.value = true
  error.value = null
  try {
    await auth.login(email.value.trim(), password.value)
    navigate('#/decks')
  } catch (e) {
    error.value = e instanceof AuthError ? errorLabel(e.code) : t('auth.errors.network')
  } finally {
    busy.value = false
  }
}
</script>

<template>
  <AuthCard
    :eyebrow="t('auth.login.eyebrow')"
    :heading="t('auth.login.heading')"
    :lead="t('auth.login.lead')"
    :error="error"
    @submit="submit"
  >
    <label class="field">
      <span class="field-label">{{ t('auth.fields.email') }}</span>
      <input v-model="email" type="email" required autocomplete="email" :placeholder="t('auth.fields.email_placeholder')" />
    </label>

    <label class="field">
      <span class="field-label">{{ t('auth.fields.password') }}</span>
      <input v-model="password" type="password" required autocomplete="current-password" />
    </label>

    <button class="primary" type="submit" :disabled="busy">
      {{ busy ? t('auth.login.submitting') : t('auth.login.submit') }}
    </button>

    <template #switch>
      {{ t('auth.login.no_account') }}
      <a href="#/register" @click.prevent="navigate('#/register')">
        {{ t('auth.login.go_register') }}
      </a>
    </template>
  </AuthCard>
</template>
