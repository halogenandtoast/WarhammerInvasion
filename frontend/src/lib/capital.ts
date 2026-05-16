// Mapping from race → capital board asset. Assets live under
// frontend/public/capitals/ and are served verbatim by Vite at /capitals/*.
// Add new entries when the engine grows past Dwarf (see backend
// Invasion/Types.hs).

import type { Race } from '../api/protocol'

export function capitalImageFor(race: Race): string {
  switch (race) {
    case 'Dwarf':
      return '/capitals/dwarves.jpg'
    case 'Empire':
      return '/capitals/empire.jpg'
    case 'HighElf':
      return '/capitals/high-elves.jpg'
    case 'Chaos':
      return '/capitals/chaos.jpg'
    case 'Orc':
      return '/capitals/orcs.jpg'
    case 'DarkElf':
      return '/capitals/dark-elves.jpg'
  }
}

export function raceLabel(race: Race): string {
  switch (race) {
    case 'Dwarf':
      return 'Dwarf'
    case 'Empire':
      return 'Empire'
    case 'HighElf':
      return 'High Elf'
    case 'Chaos':
      return 'Chaos'
    case 'Orc':
      return 'Orc'
    case 'DarkElf':
      return 'Dark Elf'
  }
}
