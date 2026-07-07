# cc-switch.el
Switch model provider in Emacs way, like cc-switch

本项目意图提供同 /home/jamie/proj/cc-switch-cli/ 类似的功能，只是将其转换成 emacs package，用 elisp 实现其对应的逻辑即 ui

`cc-switch.el` is a pure Emacs Lisp companion for
[cc-switch-cli](https://github.com/SaladDay/cc-switch-cli).  It reads the
existing cc-switch SQLite database and switches Claude Code or Codex providers
from Emacs, without shelling out to the `cc-switch` binary.

## Status

V1 is intentionally narrow:

- supported apps: Claude Code and Codex
- supported workflow: list providers, show current provider, switch provider,
  export a Claude provider, and run local diagnostics
- UI: `completing-read`, so Vertico, Ivy, Helm, and similar completion UIs work
  naturally
- storage: existing `~/.cc-switch/cc-switch.db`

It does not implement provider CRUD, proxy/daemon takeover, MCP, prompts,
skills, OAuth login, speed tests, stream checks, WebDAV sync, or migration from
legacy `config.json`.

## Requirements

- Emacs 29.1 or newer
- built-in `sqlite` and `json`
- existing cc-switch SQLite database at `~/.cc-switch/cc-switch.db`, or at
  `$CC_SWITCH_CONFIG_DIR/cc-switch.db`
- Codex switching additionally requires `toml.el`

If `~/.cc-switch/config.json` exists but `cc-switch.db` does not, run
cc-switch-cli once first so it can perform its own migration.

## Installation

Place this repository on `load-path`:

```elisp
(add-to-list 'load-path "/path/to/cc-switch.el")
(require 'cc-switch)
```

For Codex support, install the `toml` package before switching Codex providers.
Claude commands continue to work when `toml.el` is unavailable.

## Commands

- `M-x cc-switch-provider-list`
- `M-x cc-switch-provider-current`
- `M-x cc-switch-provider-switch`
- `M-x cc-switch-use`
- `M-x cc-switch-switch-claude`
- `M-x cc-switch-switch-codex`
- `M-x cc-switch-provider-export`
- `M-x cc-switch-diagnose`

Most commands use `cc-switch-default-app` unless called with a prefix argument,
which prompts for `claude` or `codex`.

## Configuration

```elisp
(setq cc-switch-default-app "claude")
(setq cc-switch-config-dir "~/.cc-switch")
(setq cc-switch-claude-config-dir "~/.claude")
(setq cc-switch-codex-home "~/.codex")
```

When a directory option is nil, `cc-switch.el` follows the corresponding
environment variable where applicable:

- `CC_SWITCH_CONFIG_DIR`
- `CLAUDE_CONFIG_DIR`
- `CODEX_HOME`, only when it points at an existing directory

## Safety Model

`cc-switch.el` writes live config files directly, so V1 stays conservative:

- refuses to switch if the target app config directory does not already exist
- refuses to switch when cc-switch proxy takeover/live backup state is detected
- writes live config first, then updates SQLite `is_current`
- rolls live files back if the DB update fails
- writes via temporary file plus rename
- keeps one sibling backup named `.<filename>.cc-switch-el.bak`
- never prints provider `settings_config`, API keys, or tokens in candidates,
  diagnostics, or error messages

Claude switching writes `settings.json`.  Codex switching writes `config.toml`;
official Codex providers may also update `auth.json`, while third-party
providers preserve existing `auth.json` to avoid clobbering ChatGPT OAuth state.

## Future Plan

- provider add/edit/duplicate/delete
- MCP sync
- prompt management
- skills management
- proxy/daemon status integration and safe hot-switch
- richer Codex auth handling and official OAuth helpers
- Gemini, OpenCode, Hermes, and OpenClaw support
- legacy `config.json` read-only import or migration helper
- richer UI through `tabulated-list` or `transient`

## Development

Run tests:

```bash
emacs --batch -L . -l cc-switch.el -l test/cc-switch-test.el -f ert-run-tests-batch-and-exit
```

Byte-compile:

```bash
emacs --batch -L . -f batch-byte-compile cc-switch.el
```
