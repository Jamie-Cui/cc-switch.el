# Changelog

## 0.1.0 - Unreleased

- Renamed the package and public namespace to `agent-switch`.
- Removed all SQLite, `cc-switch.db`, old command, feature, variable, and
  compatibility paths.
- Added versioned per-Profile JSON storage and separate state storage below
  `user-emacs-directory`.
- Added extensible Client, Adapter, Profile, and synchronous/asynchronous Job
  protocols.
- Added built-in Claude Code, Codex, and OpenCode
  Adapters with transactional activation and recovery.
- Replaced `tabulated-list-mode` with an internal section dashboard supporting
  Client folding without a `magit-section` dependency.
- Used standard `hl-line-mode` row highlighting and single-line Profile rows.
- Moved Status into an always-visible top preamble, removed the dashboard
  title, and removed blank lines between sections.
- Simplified Client headings to names only and rendered bounded-width Profile
  name, ID, model, and provider Base URL columns, with `(action required)` for
  incomplete Profiles or unmatched auth-source references and `-` for missing
  summary values.
- Added Evil-aware shared structural keys while preserving native Evil
  navigation/search keys.
- Added managed Profile CRUD, external Profile registration/copying, direct
  JSON editing, semantic faces, file watchers, and diagnostics.
- Added an explicit Adopt action for adopting unmanaged live
  configuration as the selected managed Profile without persisting secrets.
- Kept Adopt in the transient menu and removed the redundant Edit suffix;
  Profile editing remains available through `RET`.
- Automatically capture a Client's initial live configuration as its managed
  `default` Profile when it starts without Profiles, while retaining a
  first-run marker so deliberate deletion does not recreate it.
- Use random IDs for automatically captured Profiles while keeping `default`
  as the user-facing name, and render required-action markers in bold error
  styling.
- Allow any managed Profile to be deleted, including the current or selected
  Profile, and remove its selection record without changing live config.
- Added incomplete-capture metadata so adopted Profiles with omitted secrets
  are rejected safely at Apply time without adding dashboard status labels.
- Removed the built-in gptel Default Client and its optional dependency.
- Centralized Profile validation across managed, Elisp, discovered, and
  activation boundaries; duplicate identities now become explicit conflicts.
- Restricted persisted secret references to auth-source/authinfo; environment
  references are rejected.
- Generate auth-source placeholders for every captured secret, deriving
  `authinfo.machine` from the provider Base URL and `authinfo.login` from the
  full secret field path, so users only need to add matching authinfo entries.
- Mark Profiles as `(action required)` dynamically when an auth-source
  reference has no matching secret entry, its Adapter validation fails, or its
  payload schema is stale; refreshing clears resolvable conditions.
- Added a standalone authinfo helper with a Unix process contract: the token is
  the only standard output, diagnostics use standard error, and status codes
  report lookup or usage failure without exposing the token.
- Upgraded Codex Profiles to payload schema v2 with strict command-delivered
  authinfo credentials for remote providers; Adopt converts legacy `env_key`
  configuration and Apply writes Codex command-backed provider authentication
  without placing the API key in Profile JSON, state, or TOML.
- Represented Codex's built-in OpenAI provider semantically as `openai` and
  materialized it as `agent-switch-openai` for authinfo-managed API-key access.
  Native OAuth credentials and `~/.codex/auth.json` remain untouched and
  unmanaged.
- Added reusable noninteractive create/adopt/apply/delete operations with
  mutation locking, explicit unprotected policy, and compensating recovery.
- Routed dashboard Adopt through the noninteractive operations boundary and
  removed dead warning aggregation from complete built-in captures.
- Added state schema v2 selection provenance with automatic v1 migration and
  Adapter payload schema version checks.
- Made diagnostics available as structured data before dashboard rendering.
- Aligned built-in Profile matchers with their actual replace/merge semantics.
- Normalized missing Codex provider tables to empty Profile objects so
  persistence, status matching, and activation verification agree.
- Moved operation failure details out of the dashboard and into `*Messages*`,
  and rendered the dashboard data path with the default face.
- Changed `RET` on a Profile to visit its managed JSON for editing and removed
  the separate Profile summary buffer.
- Made rollback preserve post-apply external changes through optimistic hashes.
- Removed speculative Adapter fields, hidden commands, manual Profile ordering,
  custom coverage infrastructure, and generated files from the package recipe.
- Licensed the package under GPL-3.0-or-later with SPDX headers.
