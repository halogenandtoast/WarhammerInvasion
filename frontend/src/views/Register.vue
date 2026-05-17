<script setup lang="ts">
import { ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { auth, AuthError } from '../stores/auth'
import { navigate } from '../router'
import AuthCard from '../components/AuthCard.vue'

const { t } = useI18n({ useScope: 'global' })

const email = ref('')
const password = ref('')
const displayName = ref('')
const busy = ref(false)
const error = ref<string | null>(null)

const KNOWN_ERRORS = new Set([
  'email_in_use',
  'invalid_email',
  'password_too_short',
  'password_too_long',
  'invalid_display_name',
])

function errorLabel(code: string): string {
  if (KNOWN_ERRORS.has(code)) return t(`auth.errors.${code}`)
  return code
}

async function submit() {
  if (busy.value) return
  if (password.value.length < 8) {
    error.value = t('auth.errors.password_too_short')
    return
  }
  busy.value = true
  error.value = null
  try {
    await auth.register(email.value.trim(), password.value, displayName.value.trim())
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
    :eyebrow="t('auth.register.eyebrow')"
    :heading="t('auth.register.heading')"
    :lead="t('auth.register.lead')"
    :error="error"
    @submit="submit"
  >
    <label class="field">
      <span class="field-label">{{ t('auth.fields.display_name') }}</span>
      <input
        v-model="displayName"
        type="text"
        required
        maxlength="50"
        autocomplete="nickname"
        :placeholder="t('auth.fields.display_name_placeholder')"
      />
    </label>

    <label class="field">
      <span class="field-label">{{ t('auth.fields.email') }}</span>
      <input v-model="email" type="email" required autocomplete="email" />
    </label>

    <label class="field">
      <span class="field-label">{{ t('auth.fields.password') }}</span>
      <input v-model="password" type="password" required minlength="8" autocomplete="new-password" />
      <small class="hint">{{ t('auth.fields.password_hint') }}</small>
    </label>

    <button class="primary" type="submit" :disabled="busy">
      {{ busy ? t('auth.register.submitting') : t('auth.register.submit') }}
    </button>

    <template #switch>
      {{ t('auth.register.have_account') }}
      <a href="#/login" @click.prevent="navigate('#/login')">
        {{ t('auth.register.go_login') }}
      </a>
    </template>
  </AuthCard>
</template>
