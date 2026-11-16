# audio-catalog

Builds a SQLite catalog of audio files: where they are, how big and how long
they are, their tags, and roughly what they contain — speech, music, noise or
silence — with a timeline per file.

## Requirements

- Racket 8.3 or later (uses only the main distribution: `db`, `json`, `rackunit`)
- `ffmpeg` and `ffprobe` on the PATH or in `/opt/homebrew/bin` (`brew install ffmpeg`)

## Use

    racket main.rkt scan ~/Dropbox          # first run reads everything; later runs only what changed
    racket main.rkt summary                 # totals by content type
    racket main.rkt list --content speech   # one line per file
    racket main.rkt list --under ~/Dropbox/Aufnahmen
    racket main.rkt show FILE               # timeline: 0:00:00 - 0:00:14 speech ...
    racket main.rkt dupes                   # byte-identical files (SHA-1)
    racket main.rkt export catalog.csv

`racket main.rkt` alone prints all options. `raco make main.rkt` once makes
start-up faster. `raco test tests.rkt` runs the unit tests (no ffmpeg needed).

The catalog lives in `~/.audio-catalog/catalog.sqlite` (override with
`--db FILE`). It is kept outside Dropbox on purpose.

### Dropbox online-only files

Reading an online-only file makes Dropbox download it. `scan` therefore
records such files with status `online-only` (path and size only) and does
not open them. `scan --hydrate` reads them anyway. Detection is "has a size
but occupies no disk blocks"; **this was not tested on macOS** — try a small
folder with a known online-only file first.

## What gets stored

`files`: path, size, mtime, SHA-1, container format, codec, sample rate,
channels, bit rate, duration, title/artist/album/date tags, a one-word
`content` (speech | music | noise | silence | mixed), and seconds of each.

`segments`: (file, start, end, label) — the timeline. Resolution is about
2 seconds.

Query it directly with `sqlite3 ~/.audio-catalog/catalog.sqlite` whenever the
built-in reports are not enough.

## How the content detection works

No trained model. Audio is decoded to 16 kHz mono, cut into 64 ms frames,
and each ~2 s block is described by a few classic features:

| feature   | meaning                                                  | typical |
|-----------|----------------------------------------------------------|---------|
| `flux`    | how much the spectrum changes frame to frame             | music low, speech high |
| `tonal`   | share of energy in narrow spectral peaks                 | noise low |
| `persist` | share of those peaks that stay at the same frequency     | music high |
| `flat`    | spectral flatness                                        | noise high |
| `flux-sd` | how irregular the change is                              | speech high |

`classify.rkt` is four threshold rules on these. `racket dump.rkt FILE`
prints the numbers and the decision for every block, which is the way to see
why a file was labelled as it was and to retune.

### How well it does

Thresholds were set on, and measured against, the same small public sample
set, so treat these as optimistic:

- read speech (43 clips): 95 % of blocks, 43/43 files labelled speech
- music (8 pieces, classical/ragtime/pop/jazz): 95 % of blocks, 7/8 files
- household and office noise (39 clips): about 85 % of blocks

Known weak spots:

- Drum-heavy music with little melody comes out as noise.
- Singing and speech over music are not separate categories; expect
  `music`, or a speech/music mix.
- Background voices (TV through a wall, crowd babble) count as speech
  about half the time.
- Steady hums can occasionally look like music.
- Not tested on your own recordings: phone voice memos, choir rehearsals and
  the like may need the thresholds nudged.

Speed: a 30-minute MP3 takes about 8 seconds.

## Files

    main.rkt      command line, scanning, reports
    decode.rkt    ffprobe metadata, ffmpeg decoding
    features.rkt  FFT and per-frame / per-block features
    classify.rkt  thresholds, smoothing, segments, per-file summary (pure)
    db.rkt        SQLite schema and queries
    dump.rkt      print raw features per block
    tests.rkt     rackunit tests

After changing features or thresholds, bump `analyzer-version` in `db.rkt`;
the next `scan` then re-analyses everything.

## Later: who is speaking

The `segments` table already says where the speech is, which is the input a
speaker-identification stage needs. A private version would run entirely on
the Mac: compute a voice embedding for each speech segment with a local
model, label a few segments per person by hand, and match the rest by
similarity, storing the result in a new `speakers` / `segment_speaker` pair
of tables. Nothing here uploads audio anywhere.
