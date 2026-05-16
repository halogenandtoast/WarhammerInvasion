/// <reference types="vite/client" />
/// <reference types="@intlify/unplugin-vue-i18n/messages" />

declare module '*.yaml' {
  // Loaded and pre-compiled by @intlify/unplugin-vue-i18n; the shape is
  // arbitrary nested locale-message data, so we trust vue-i18n's own
  // runtime typing here rather than re-deriving it.
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const content: any
  export default content
}
