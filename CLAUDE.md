# CLAUDE.md

## README.org editing rules

- Org-mode inline markup delimiters (`=code=`, `~code~`, `*bold*`,
  `/italic/`, `_underline_`, `+strike+`, etc.) must have a space between
  the delimiter and adjacent CJK characters (Chinese text or full-width
  punctuation like `「」`, `，`, `。`). Without the space, org-mode can
  fail to recognize the markup boundary, and it's harder to read.
  - Good: `配置 =wl-folders-file= 后` (space between `=` and CJK text on
    both sides)
  - Good: `Outlook (=davmail= 网关)` (space between `(`/`)` and `=`, and
    between `=` and CJK text)
  - Bad: `账号（=davmail=网关）` (full-width `（` jammed against `=`, and
    `=` jammed against `网关`)
  - No extra space needed when the delimiter is next to ASCII text, or
    at the start/end of a line.
- Do not write conversational narration into README.org prose (e.g.
  "顺手做了一些改进", "这里我们额外加了一个小优化"). Describe what the
  config does and why, as documentation, not as a running commentary
  on the editing session that produced it.

## Communication

- Talk to the user in Chinese.
- Write comments in code (including code inside README.org source
  blocks) in English.
