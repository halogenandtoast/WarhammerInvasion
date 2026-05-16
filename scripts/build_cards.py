#!/usr/bin/env python3
"""
Build the cards dataset for the frontend.

- Reads JSON files from `--json-src` (default: ~/Downloads/invasion).
- Walks card image dirs from `--img-src` (default: WARHAMMER_ INVASION LCG/CARDS, BOARDS, TOKENS).
- Resizes images to 600px wide JPEGs in `frontend/public/cards/<slug>.jpg`.
- Writes the merged dataset to `frontend/src/data/cards.json`.

For sets without JSON, generates stub cards from filenames (number + name).
"""

from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
DEFAULT_JSON_SRC = Path.home() / "Downloads" / "invasion"
DEFAULT_IMG_SRC = (
    Path.home()
    / "Downloads"
    / "WARHAMMER_ INVASION LCG"
    / "CARDS, BOARDS, TOKENS"
)
PUBLIC_CARDS = REPO_ROOT / "frontend" / "public" / "cards"
DATA_OUT = REPO_ROOT / "frontend" / "public" / "cards.json"

# Canonical set order — mirrors the directory layout under
# `WARHAMMER_ INVASION LCG/CARDS, BOARDS, TOKENS/`. Each tuple is:
#   (cycle, set_name, image_dir_relative, json_filename_or_None)
# Order is significant: cycles and sets render in this order in the frontend.
SETS: list[tuple[str, str, str, str | None]] = [
    # 01 — Core
    ("Core Cycle", "Core", "01 - Core", "Core.json"),

    # 02 — Corruption cycle (battlepacks 1–6, no JSON, stubbed from filenames)
    ("The Corruption Cycle", "The Skavenblight Threat", "02 - Corruption/01 - The Skavenblight Threat", None),
    ("The Corruption Cycle", "Path of the Zealot", "02 - Corruption/02 - Path of the Zealot", None),
    ("The Corruption Cycle", "Tooth and Claw", "02 - Corruption/03 - Tooth and Claw", None),
    ("The Corruption Cycle", "The Deathmaster's Dance", "02 - Corruption/04 - The Deathmaster_s Dance", None),
    ("The Corruption Cycle", "The Warpstone Chronicles", "02 - Corruption/05 - The Warpstone Chronicles", None),
    ("The Corruption Cycle", "Arcane Fire", "02 - Corruption/06 - Arcane Fire", None),

    # 03 — Assault on Ulthuan (deluxe)
    ("Assault on Ulthuan Cycle", "Assault on Ulthuan", "03 - Assault on Ulthuan", "Assault on Ulthuan.json"),

    # 04 — Enemy cycle (battlepacks 1–6)
    ("The Enemy Cycle", "The Burning of Derricksburg", "04 - Enemy/01 - The Burning of Derricksburg", "The Burning of Derricksburg.json"),
    ("The Enemy Cycle", "The Fall of Karak Grimaz", "04 - Enemy/02 - The Fall of Karak Grimaz", "The Fall of Karak Grimaz.json"),
    ("The Enemy Cycle", "The Silent Forge", "04 - Enemy/03 - The Silent Forge", "The Silent Forge.json"),
    ("The Enemy Cycle", "Redemption of a Mage", "04 - Enemy/04 - Redemption of a Mage", "Redemption of a Mage.json"),
    ("The Enemy Cycle", "The Fourth Waystone", "04 - Enemy/05 - The Fourth Waystone", "The Fourth Waystone.json"),
    ("The Enemy Cycle", "Bleeding Sun", "04 - Enemy/06 - Bleeding Sun", "Bleeding Sun.json"),

    # 05 — March of the Damned (deluxe)
    ("March of the Damned Cycle", "March of the Damned", "05 - March of the Damned", "March of the Damned.json"),

    # 06 — Morrslieb cycle (battlepacks 1–6)
    ("The Morrslieb Cycle", "Omens of Ruin", "06 - Morrslieb/01 - Omens of Ruin", "Omens of Ruin.json"),
    ("The Morrslieb Cycle", "The Chaos Moon", "06 - Morrslieb/02 - The Chaos Moon", "The Chaos Moon.json"),
    ("The Morrslieb Cycle", "The Twin Tailed Comet", "06 - Morrslieb/03 - The Twin Tailed Comet", "The Twin Tailed Comet.json"),
    ("The Morrslieb Cycle", "Signs in the Stars", "06 - Morrslieb/04 - Signs in the Stars", "Signs in the Stars.json"),
    ("The Morrslieb Cycle", "The Eclipse of Hope", "06 - Morrslieb/05 - The Eclipse of Hope", "The Eclipse of Hope.json"),
    ("The Morrslieb Cycle", "Fiery Dawn", "06 - Morrslieb/06 - Fiery Dawn", "Fiery Dawn.json"),

    # 07 — Legends (deluxe)
    ("Legends Cycle", "Legends", "07 - Legends", "Legends.json"),

    # 08 — Capital cycle (battlepacks 1–6)
    ("The Capital Cycle", "The Inevitable City", "08 - Capital/01 - The Inevitable City", "The Inevitable City.json"),
    ("The Capital Cycle", "Realm of the Phoenix King", "08 - Capital/02 - Realm of the Phoenix King", "Realm of the Phoenix King.json"),
    ("The Capital Cycle", "The Iron Rock", "08 - Capital/03 - The Iron Rock", "The Iron Rock.json"),
    ("The Capital Cycle", "Karaz-a-Karak", "08 - Capital/04 - Karaz-a-Karak", "Karaz-a-Karak.json"),
    ("The Capital Cycle", "City of Winter", "08 - Capital/05 - City of Winter", "City of Winter.json"),
    ("The Capital Cycle", "The Imperial Throne", "08 - Capital/06 - The Imperial Throne", "The Imperial Throne.json"),

    # 09 — Bloodquest cycle (battlepacks 1–6)
    ("The Bloodquest Cycle", "Rising Dawn", "09 - Bloodquest/01 - Rising Dawn", "Rising Dawn.json"),
    ("The Bloodquest Cycle", "Fragments of Power", "09 - Bloodquest/02 - Fragments of Power", "Fragments of Power.json"),
    ("The Bloodquest Cycle", "The Accursed Dead", "09 - Bloodquest/03 - The Accursed Dead", "The Accursed Dead.json"),
    ("The Bloodquest Cycle", "Vessel of the Winds", "09 - Bloodquest/04 - Vessel of the Winds", "Vessel of the Winds.json"),
    ("The Bloodquest Cycle", "Portent of Doom", "09 - Bloodquest/05 - Portent of Doom", "Portent of Doom.json"),
    ("The Bloodquest Cycle", "Shield of the Gods", "09 - Bloodquest/06 - Shield of the Gods", "Shield of the Gods.json"),

    # 10 — Eternal War cycle (battlepacks 1–6)
    ("Eternal War Cycle", "Days of Blood", "10 - Eternal War/01 - Days of Blood", "Days of Blood.json"),
    ("Eternal War Cycle", "Oaths of Vengeance", "10 - Eternal War/02 - Oaths of Vengeance", "Oaths of Vengeance.json"),
    ("Eternal War Cycle", "Battle for the Old World", "10 - Eternal War/03 - Battle for the Old World", "Battle for the Old World.json"),
    ("Eternal War Cycle", "Glory of Days Past", "10 - Eternal War/04 - Glory of Days Past", "Glory of Days Past.json"),
    ("Eternal War Cycle", "The Ruinous Hordes", "10 - Eternal War/05 - The Ruinous Hordes", "The Ruinous Hordes.json"),
    ("Eternal War Cycle", "Faith and Steel", "10 - Eternal War/06 - Faith and Steel", "Faith and Steel.json"),

    # 11 — Cataclysm (deluxe)
    ("Cataclysm Cycle", "Cataclysm", "11 - Cataclysm", "Cataclysm.json"),

    # 12 — Hidden Kingdoms (final expansion, no JSON)
    ("Hidden Kingdoms Cycle", "Hidden Kingdoms", "12 - Hidden Kingdoms", None),

    # 13 — Forever War (fan cycle, 2 battlepacks)
    ("Forever War Cycle", "Death Night", "13 - Fan Cycle - Forever War/Battle Pack 1 - Death Night", None),
    ("Forever War Cycle", "Dragonflight", "13 - Fan Cycle - Forever War/Battle Pack 2 - Dragonflight", None),

    # 14–16, 18 — assorted fan cycles
    ("Land of Chivalry Cycle", "Land of Chivalry", "14 - Fan Cycle - Land of Chivalry", None),
    ("Death and Dust Cycle", "Death and Dust", "15 - Fan Cycle - Death and Dust", None),
    ("The Rebirth Cycle", "What Is Dead May Never Die", "16 - Fan Cycle - The Rebirth/01 - What Is Dead May Never Die", None),
    ("Mad Miners Cycle", "Mad Miners (April Fools)", "18 - Fan Cycle - Mad Miners - April Fools", None),
]


def slugify(s: str) -> str:
    s = s.lower().strip()
    s = re.sub(r"[^a-z0-9]+", "-", s)
    return s.strip("-")


def parse_filename(name: str) -> tuple[int | None, str]:
    """`'001 - Defender of the Hold.jpg'` -> (1, 'Defender of the Hold')."""
    base = Path(name).stem
    m = re.match(r"^\s*(\d+)\s*-\s*(.+)$", base)
    if not m:
        return None, base
    num = int(m.group(1))
    title = m.group(2).strip()
    # Filenames substitute apostrophes with `_`
    title = title.replace("_", "'")
    return num, title


def find_image(img_dir: Path, number: int) -> Path | None:
    """Find image file matching this card number in dir, tolerant of formatting."""
    if not img_dir.is_dir():
        return None
    candidates = []
    for f in img_dir.iterdir():
        if f.suffix.lower() not in (".jpg", ".jpeg", ".png"):
            continue
        n, _ = parse_filename(f.name)
        if n == number:
            candidates.append(f)
    if not candidates:
        return None
    # Prefer .jpg over .png if both exist
    candidates.sort(key=lambda p: (0 if p.suffix.lower() == ".jpg" else 1, p.name))
    return candidates[0]


def resize_image(src: Path, dst: Path) -> bool:
    if dst.exists():
        return True
    dst.parent.mkdir(parents=True, exist_ok=True)
    # Use ImageMagick — fast and handles png->jpg.
    cmd = [
        "magick",
        str(src),
        "-resize",
        "600x600>",  # only shrink, never enlarge
        "-strip",
        "-quality",
        "82",
        str(dst),
    ]
    res = subprocess.run(cmd, capture_output=True, text=True)
    if res.returncode != 0:
        print(f"  ! magick failed for {src}: {res.stderr.strip()}", file=sys.stderr)
        return False
    return True


def normalize_card(raw: dict, img_filename: str | None, cycle: str, set_name: str) -> dict:
    """Clean up the raw JSON card record. `set_name` overrides any value in the
    JSON to keep the canonical set name from the SETS table."""
    name = raw.get("name", "").strip()
    number = raw.get("number")
    return {
        "id": f"{slugify(set_name)}-{number:03d}" if isinstance(number, int) else f"{slugify(set_name)}-{slugify(name)}",
        "name": name,
        "set": set_name,
        "cycle": cycle,
        "number": number,
        "type": (raw.get("type") or "").strip() or None,
        "race": (raw.get("race") or "").strip() or None,
        "cost": _to_str(raw.get("cost")),
        "loyalty": _to_int(raw.get("loyalty")),
        "power": _to_str(raw.get("power")),
        "health": _to_str(raw.get("health")),
        "traits": (raw.get("traits") or "").strip() or None,
        "text": (raw.get("text") or "").strip() or None,
        "quantity": _to_str(raw.get("quantity:") or raw.get("quantity")),
        "illustrator": (raw.get("illustrator") or "").strip() or None,
        "image": img_filename,
        "stub": False,
    }


def _to_str(v) -> str | None:
    if v is None:
        return None
    s = str(v).strip()
    return s or None


def _to_int(v) -> int | None:
    if v is None or v == "":
        return None
    try:
        return int(v)
    except (TypeError, ValueError):
        return None


def stub_card(set_name: str, cycle: str, number: int, title: str, img_filename: str | None) -> dict:
    return {
        "id": f"{slugify(set_name)}-{number:03d}",
        "name": title,
        "set": set_name,
        "cycle": cycle,
        "number": number,
        "type": None,
        "race": None,
        "cost": None,
        "loyalty": None,
        "power": None,
        "health": None,
        "traits": None,
        "text": None,
        "quantity": None,
        "illustrator": None,
        "image": img_filename,
        "stub": True,
    }


def main() -> int:
    p = argparse.ArgumentParser()
    p.add_argument("--json-src", type=Path, default=DEFAULT_JSON_SRC)
    p.add_argument("--img-src", type=Path, default=DEFAULT_IMG_SRC)
    p.add_argument("--out", type=Path, default=DATA_OUT)
    p.add_argument("--out-images", type=Path, default=PUBLIC_CARDS)
    p.add_argument("--no-images", action="store_true", help="Skip image processing")
    p.add_argument("--workers", type=int, default=8)
    args = p.parse_args()

    json_src: Path = args.json_src
    img_src: Path = args.img_src

    if not json_src.is_dir():
        print(f"json src not found: {json_src}", file=sys.stderr)
        return 1
    if not img_src.is_dir():
        print(f"img src not found: {img_src}", file=sys.stderr)
        return 1

    cards: list[dict] = []
    image_jobs: list[tuple[Path, Path]] = []  # (src, dst)

    # Walk the canonical ordered set list. JSON sets emit fully populated cards;
    # sets without JSON (None) get stubbed from filenames.
    for cycle, set_name, rel_dir, json_filename in SETS:
        img_dir = img_src / rel_dir
        if not img_dir.is_dir():
            print(f"skip (no image dir): {rel_dir}")
            continue

        if json_filename:
            json_path = json_src / json_filename
            if not json_path.exists():
                print(f"skip (no JSON file): {json_filename}")
                continue
            with open(json_path) as f:
                raw_cards = json.load(f)
            count = 0
            for raw in raw_cards:
                number = raw.get("number")
                img_path = find_image(img_dir, number) if isinstance(number, int) else None
                img_filename = None
                if img_path:
                    slug = f"{slugify(set_name)}-{number:03d}.jpg"
                    img_filename = slug
                    image_jobs.append((img_path, args.out_images / slug))
                cards.append(normalize_card(raw, img_filename, cycle, set_name))
                count += 1
            print(f"loaded {cycle} / {set_name}: {count} cards")
        else:
            files = sorted(
                f for f in img_dir.iterdir()
                if f.suffix.lower() in (".jpg", ".jpeg", ".png")
            )
            any_numbered = any(parse_filename(f.name)[0] is not None for f in files)
            seq = 0
            seen_numbers: set[int] = set()
            for f in files:
                number, title = parse_filename(f.name)
                if number is None:
                    if any_numbered:
                        continue
                    seq += 1
                    number = seq
                    title = Path(f.name).stem.replace("_", "'")
                if number in seen_numbers:
                    continue
                seen_numbers.add(number)
                slug = f"{slugify(set_name)}-{number:03d}.jpg"
                image_jobs.append((f, args.out_images / slug))
                cards.append(stub_card(set_name, cycle, number, title, slug))
            print(f"stubbed {cycle} / {set_name}: {len(seen_numbers)} cards")

    # Resize images in parallel.
    if not args.no_images and image_jobs:
        args.out_images.mkdir(parents=True, exist_ok=True)
        todo = [(s, d) for (s, d) in image_jobs if not d.exists()]
        print(f"images: {len(image_jobs)} total, {len(todo)} to build")
        ok_count = len(image_jobs) - len(todo)
        with ThreadPoolExecutor(max_workers=args.workers) as ex:
            futs = {ex.submit(resize_image, s, d): (s, d) for (s, d) in todo}
            for i, fut in enumerate(as_completed(futs), 1):
                if fut.result():
                    ok_count += 1
                if i % 50 == 0:
                    print(f"  resized {i}/{len(todo)}")
        print(f"images: {ok_count}/{len(image_jobs)} ok")

    # Cards are already in the right order (cycle order from SETS, then
    # original JSON/file order within each set). Just write.
    args.out.parent.mkdir(parents=True, exist_ok=True)
    with open(args.out, "w") as f:
        json.dump(cards, f, indent=2, ensure_ascii=False)
    print(f"wrote {len(cards)} cards -> {args.out}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
