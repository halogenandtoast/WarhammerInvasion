import { createI18n, type Composer } from 'vue-i18n'
import en from '../locales/en.yaml'

export type MessageSchema = typeof en
export type Locale = 'en'

const STORAGE_KEY = 'whi.locale'
const SUPPORTED: readonly Locale[] = ['en'] as const

function detectLocale(): Locale {
  if (typeof window === 'undefined') return 'en'
  const stored = window.localStorage.getItem(STORAGE_KEY) as Locale | null
  if (stored && SUPPORTED.includes(stored)) return stored
  const nav = window.navigator.language.slice(0, 2).toLowerCase() as Locale
  return SUPPORTED.includes(nav) ? nav : 'en'
}

export const i18n = createI18n({
  legacy: false,
  globalInjection: false,
  locale: detectLocale(),
  fallbackLocale: 'en',
  messages: { en },
  missingWarn: import.meta.env.DEV,
  fallbackWarn: import.meta.env.DEV,
})

export function setLocale(locale: Locale) {
  const composer = i18n.global as unknown as Composer
  composer.locale.value = locale
  if (typeof window !== 'undefined') {
    window.localStorage.setItem(STORAGE_KEY, locale)
    window.document.documentElement.lang = locale
  }
}
