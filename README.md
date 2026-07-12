# agent-switch.el

[![Test](https://github.com/Jamie-Cui/agent-switch.el/actions/workflows/test.yml/badge.svg)](https://github.com/Jamie-Cui/agent-switch.el/actions/workflows/test.yml)
[![Melpazoid](https://github.com/Jamie-Cui/agent-switch.el/actions/workflows/melpazoid.yml/badge.svg)](https://github.com/Jamie-Cui/agent-switch.el/actions/workflows/melpazoid.yml)

`agent-switch.el` is an Emacs control panel for selecting provider profiles
used by LLM agent clients. It includes adapters for Claude Code, Codex, and
OpenCode global configuration. Additional clients,
adapters, and profiles can be registered entirely in Emacs Lisp.

The package is a breaking rename and redesign of `cc-switch.el`. It does not
read, import, or migrate `cc-switch.db`, and it does not provide old commands,
features, variables, or compatibility aliases.

## Requirements

- Emacs 29.1 or newer
- `transient` 0.4 or newer
- `toml` 1.0.0 or newer
- `tomelr` 0.4.3 or newer

## Installation

Put the repository on `load-path`, install the dependencies, then load the
package:

```elisp
(require 'agent-switch)
```

Open the dashboard with `M-x agent-switch`.

## Dashboard

The dashboard uses an internal section model and derives from `special-mode`.
It does not depend on `magit-section` or `tabulated-list-mode`.
Always-visible status lines appear directly at the top, followed by the
collapsible Client sections and single-line Profile rows.
Client headings contain only the Client name. Profile rows contain the current
marker followed by bounded-width name, Profile ID, model, and provider Base URL
columns. Missing model or Base URL values display `-`. Incomplete,
Adapter-invalid, schema-incompatible, and unmatched-authinfo Profiles append
`(action required)` to the name column.
The standard `hl-line-mode` highlights the row at point.

Common keys in Evil and non-Evil sessions:

| Key | Action |
| --- | --- |
| `TAB` | Expand or collapse a Client |
| `RET` | Edit a Profile, or toggle a Client section |
| `?` | Open the transient action menu |
| `q` | Quit the dashboard window |

Additional non-Evil keys:

| Key | Action |
| --- | --- |
| `g` | Refresh |
| `n` / `p` | Next / previous section |

In Evil normal state, `g` and `n/p` keep their native Evil behavior. The
package deliberately does not add `gr`, `C-j/C-k`, or `gj/gk` alternatives.

The `?` menu contains Apply, Adopt, New, Copy, Delete, refresh, and diagnostics.
`M-x agent-switch-adopt-current-at-point` captures the live provider-owned state as a
managed Profile. Complete captures are recorded as the current selection.
All three built-in Client sources share the same capture path, which replaces
every secret marker with a generated auth-source reference. Adapters may still
explicitly return an incomplete capture; those Profiles display
`(action required)`. Profiles whose auth-source references have no matching
entry also display `(action required)`; refresh the dashboard after adding the
entry. Apply still performs the authoritative secret check and validation.
Generated authinfo objects include a `comments` array describing likely
pre-adoption secret locations for Claude Code, Codex, or OpenCode. These hints
are metadata and do not participate in lookup or current-state matching.
`RET` visits the managed Profile JSON using the user's normal Emacs file mode.
Editing and saving never applies a Profile automatically; Apply is always
explicit. Operation failures are logged through `message` to `*Messages*` and
are not retained in the dashboard status preamble.

On a Client's first dashboard startup, if it has live configuration but no
managed, external, or discovered Profiles, agent-switch captures that state as
the managed `default` Profile. This writes only agent-switch's Profile and state
files; it does not rewrite the Client configuration. A persistent initialization
marker prevents a deliberately emptied Client from being adopted again later.
If no live state can be captured, the Client continues to show `No profiles`.
`default` is the editable Profile name; Profiles created through the dashboard
use independently generated random `p-xxxxxxxx` identifiers.

Delete accepts any managed Profile, including the current or selected one. The
live Client configuration is left unchanged, and any matching selection record
is removed.

## Built-in Clients

### Claude Code

The Claude Adapter manages provider-related keys below `env` in
`~/.claude/settings.json`. Permissions, hooks, MCP configuration, unrelated
environment variables, and unknown settings are preserved.

### Codex

The Codex Adapter structurally parses and merges `~/.codex/config.toml`, then
generates and reparses TOML before committing. It owns `model_provider`,
`model`, optional `small_model`, and the selected `model_providers.<id>` patch.
Sandbox, MCP, project, and unknown configuration are preserved semantically.

Codex Profiles use payload schema v2. Every remote provider must contain a
command-delivered `credential` auth-source reference; `ollama` and `lmstudio`
are the only credential-free built-ins. Adopt converts a legacy provider
`env_key` into that reference instead of persisting the environment-variable
name. Apply verifies that the authinfo entry exists and writes Codex's
`model_providers.<id>.auth` command configuration. At request time, Codex runs
the bundled batch Emacs helper, which writes only the token to standard output.

Every `model_providers.<id>` table in the global Codex config is discovered as
a read-only Profile using the global `model` and optional `small_model` values.
Refresh the dashboard after editing `config.toml`. Discovered `env_key` values
become command-delivered authinfo references; environment variable contents are
never read or copied. Use Copy to create an editable managed Profile.

The semantic Profile provider ID `openai` is materialized as the private live
provider ID `agent-switch-openai`, because Codex reserves its built-in `openai`
provider. An existing built-in OpenAI configuration is adopted as an API-key
placeholder and displays `(action required)` until the matching authinfo entry
exists. agent-switch neither reads nor modifies `~/.codex/auth.json`; existing
ChatGPT OAuth state remains outside its scope. Profiles from payload schema v1
must be adopted again before Apply.

TOML comments, blank lines, and field order cannot be preserved by the
parse/generate path. The first rewrite of each source hash shows a diff and
requires confirmation. Every write creates a timestamped backup.

### OpenCode

The OpenCode Adapter manages the global `provider.<id>` patch, `model`, and
optional `small_model` in `opencode.json` or `opencode.jsonc`. Other providers
and unknown values are preserved. Project configuration and command/session
overrides are outside this Client's scope.

## Storage

The default data directory is:

```text
~/.emacs.d/agent-switch/
├── profiles/
│   ├── claude/<profile-id>.json
│   ├── codex/<profile-id>.json
│   └── opencode/<profile-id>.json
└── state.json
```

Customize it with:

```elisp
(setq agent-switch-directory
      (expand-file-name "agent-switch/" user-emacs-directory))
```

Each managed Profile has its own versioned JSON file. Its envelope records an
Adapter payload schema version so incompatible future payloads fail clearly.
`state.json` schema v2 stores selection records containing the Profile ID,
payload snapshot, and `applied` or `adopted` provenance, plus recovery
confirmations. Schema v1 state is migrated in memory and written as v2 on the
next state-changing operation. Client visibility is buffer-local.

Profile identity is `(client-id, profile-id)`, so different Clients may reuse
the same Profile ID.

## Secrets

Managed, Elisp-declared, and Adapter-discovered Profiles must not contain
plaintext API keys, tokens, passwords, or similar values. All ingress paths and
the final activation boundary enforce this rule. Environment-variable
references are not accepted; store secrets in authinfo and use an auth-source
reference:

```json
{
  "source": "auth-source",
  "authinfo": {
    "machine": "api.example.com",
    "login": "agent"
  }
}
```

For example, the corresponding `~/.authinfo.gpg` entry is:

```text
machine api.example.com login agent password SECRET
```

agent-switch reads one explicit authinfo file. It defaults to
`~/.authinfo.gpg` and can be changed with:

```elisp
(setq agent-switch-authinfo-file
      (expand-file-name "agent-switch.authinfo.gpg" user-emacs-directory))
```

To use an existing plaintext `~/.authinfo` file instead:

```elisp
(setq agent-switch-authinfo-file
      (expand-file-name "~/.authinfo"))
```

Keep plaintext authinfo files readable only by their owner (for example, mode
`600`). Prefer an encrypted `.gpg` file when practical.

For captured provider configuration using `https://relay.example.com/api`,
agent-switch automatically writes an auth-source placeholder for every secret:

```json
{
  "env": {
    "ANTHROPIC_BASE_URL": "https://relay.example.com/api",
    "ANTHROPIC_AUTH_TOKEN": {
      "source": "auth-source",
      "authinfo": {
        "machine": "relay.example.com",
        "login": "env.ANTHROPIC_AUTH_TOKEN"
      }
    }
  }
}
```

The user only needs to add the matching entry to `~/.authinfo.gpg`:

```text
machine relay.example.com login env.ANTHROPIC_AUTH_TOKEN password YOUR_REAL_TOKEN
```

No Profile edit is required. `YOUR_REAL_TOKEN` is a placeholder for the actual
token and must never be copied into the Profile JSON. The generated
`authinfo.machine` is the hostname from a conventional provider Base URL field
such as `*_BASE_URL`, `base_url`, or `baseURL`; when none exists, it falls back
to the Client ID. The generated `authinfo.login` is the full secret field path,
which keeps multiple secrets for one provider distinct.

Codex uses command delivery so the API key does not enter the dashboard or
activation process, or the generated TOML. A Codex Profile credential looks
like:

```json
{
  "source": "auth-source",
  "delivery": "command",
  "authinfo": {
    "machine": "api.openai.com",
    "login": "codex.openai.api-key"
  }
}
```

Its matching authinfo entry is:

```text
machine api.openai.com login codex.openai.api-key password YOUR_REAL_API_KEY
```

References are checked through Emacs auth-source only during activation.
Value-delivered references are then resolved in memory. Command-delivered
references remain references and are resolved by the Client through the
standalone helper. The dashboard uses the same lookup to detect missing entries
without persisting resolved values. Resolved values are excluded from
Profile/state files, the dashboard, diagnostics, and sanitized error messages.

## Safety Model

Activation follows this sequence:

1. Validate the Profile.
2. Check secret references and resolve value-delivered secrets.
3. Snapshot live Client state.
4. Apply the Adapter-owned patch.
5. Read the Client again and verify the selected Profile.
6. Commit the last selection to `state.json`.

Built-in Adapters restore their snapshot if activation, verification, or state
commit fails. Third-party Adapters without snapshot/rollback support are marked
as unprotected. The public operations API requires an explicit policy override;
the dashboard asks for confirmation before recording that override.

File writes compare the content hash captured at read time immediately before
an atomic same-directory rename. Detected external changes abort the write;
agent-switch does not automatically merge them. Rollback uses the hash produced
by agent-switch's own write and refuses to overwrite a detected later change.
Profile/state multi-file operations use compensating recovery: failed adoption
removes the newly created Profile, and failed deletion state commits restore
the deleted Profile when no later writer has occupied its path.

A damaged Profile file is shown as a disabled error item without blocking
other Profiles. A damaged `state.json` is treated as empty, read-only state
until the user explicitly resets it; reset first keeps a timestamped copy.
Duplicate Profile IDs from managed, Elisp, or discovered sources collapse into
one disabled conflict item rather than choosing an ambiguous Profile.

## Elisp Operations

The noninteractive operations layer is the stable composition boundary. It
does not prompt, visit files, render buffers, or emit user messages:

```elisp
(let* ((client (agent-switch-get-client "claude"))
       (profile (agent-switch-find-profile "claude" "work"))
       ;; Unprotected Adapters require an explicit non-nil third argument.
       (job (agent-switch-apply-profile client profile)))
  (agent-switch-job-start
   job
   (lambda (value) (message "Applied %s" (agent-switch-profile-name value)))
   (lambda (error-value) (message "Apply failed: %s" error-value))))

;; Returns a Profile directly or an agent-switch Job for asynchronous Adapters.
(agent-switch-adopt-current
(agent-switch-get-client "opencode") "Adopted OpenCode")

;; Stable machine-readable diagnostics.
(agent-switch-diagnostics-data)
```

Other reusable operations are `agent-switch-create-managed-profile`,
`agent-switch-adopt-capture`, and `agent-switch-delete-managed-profile`.

## Elisp Extensions

Adapters use a declarative callback protocol. `:current` and `:activate` are
required. Optional capabilities are `:validate`, `:discover`, `:snapshot`,
`:rollback`, `:profile-current-p`, `:capture-current`, `:watch-paths`,
`:watch-setup`, `:profile-template`, and `:profile-columns`.
`:profile-columns` returns a secret-safe plist containing `:model` and
`:base-url` strings for the dashboard. `:payload-version` is a positive integer
and defaults to 1.

```elisp
(agent-switch-define-adapter my-agent
  :name "My Agent"
  :current
  (lambda (_client _context)
    ;; Return the actual provider-owned state.
    (let ((state (make-hash-table :test #'equal)))
      (puthash "model" "local/model" state)
      state))
  :activate
  (lambda (_client profile _context)
    ;; Apply (agent-switch-profile-payload profile).
    t))

(agent-switch-register-client
 'my-agent-default
 :name "My Agent Default"
 :adapter 'my-agent)
```

Declare a read-only, activatable external Profile from Elisp:

```elisp
(agent-switch-register-profile
 'my-agent-default
 'local
 :name "Local"
 :payload (let ((payload (make-hash-table :test #'equal)))
            (puthash "model" "local/model" payload)
            payload))
```

Callbacks may return a value directly or return an `agent-switch-job` for
asynchronous process/network work. The dashboard tracks pending Jobs, ignores
stale generations, and uses optional Job cancellation during cleanup.

`:capture-current` may return a payload hash table for a complete legacy
capture, or an `agent-switch-capture-result` with payload, completeness, and
warnings. `:profile-current-p` must satisfy the operational invariant that a
Profile reported current can be applied again without changing Adapter-owned
state; patch subobjects may use subset semantics, while replaced fields must
also match presence and absence.

Managed Create starts from the Adapter's optional `:profile-template` JSON
object. Edit visits the Profile JSON file directly.

## Development

Built-in adapter code is split by responsibility:

```text
agent-switch-adapter-utils.el  shared adapter mechanisms
agent-switch-claude.el         Claude Code adapter
agent-switch-codex.el          Codex adapter
agent-switch-opencode.el       OpenCode adapter
agent-switch.el                package loading and built-in registration
```

Provider modules depend on the shared mechanisms, never on one another.

```sh
make compile
make test
```

## License

GPL-3.0-or-later. See [LICENSE](LICENSE).
