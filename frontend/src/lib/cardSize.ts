// Card-size constants — mirror the --card-w / --card-h CSS custom
// properties defined in src/styles/tokens.css.
//
// CSS variables can't be read directly from SVG attributes (we'd need
// a layout pass to resolve them), so for SVG-rendered card layouts we
// pass numeric values in viewBox units. Anything HTML/CSS uses the
// CSS var; anything SVG uses these constants. If you change one,
// change the other.

export const CARD_W = 96
export const CARD_H = 134

export const CARD_W_SM = 72
export const CARD_H_SM = 100

export const CARD_ASPECT = CARD_W / CARD_H

export interface CardSize {
  w: number
  h: number
}

export const CARD_MD: CardSize = { w: CARD_W, h: CARD_H }
export const CARD_SM: CardSize = { w: CARD_W_SM, h: CARD_H_SM }
