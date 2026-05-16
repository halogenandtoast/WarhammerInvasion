-- migrate:up

TRUNCATE TABLE decks;

ALTER TABLE decks DROP CONSTRAINT decks_faction_check;
ALTER TABLE decks DROP COLUMN faction;

ALTER TABLE decks ADD COLUMN capital TEXT NULL;
ALTER TABLE decks
  ADD CONSTRAINT decks_capital_check
  CHECK (capital IS NULL OR capital IN ('empire', 'dwarf', 'high_elf', 'chaos', 'orc', 'dark_elf'));

-- migrate:down

ALTER TABLE decks DROP CONSTRAINT decks_capital_check;
ALTER TABLE decks DROP COLUMN capital;

ALTER TABLE decks ADD COLUMN faction TEXT NULL;
ALTER TABLE decks
  ADD CONSTRAINT decks_faction_check
  CHECK (faction IS NULL OR faction IN ('order', 'destruction'));
