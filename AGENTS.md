# AGENTS.md

Public guidance for contributors and coding agents working on `emacs-forgejo`.

## Project

`emacs-forgejo` is an Emacs 29.1+ client for Forgejo instances. It supports
issues, pull requests, reviews, notifications, repository settings, watch rules,
and AGit-Flow submission. Sources live in `lisp/`, ERT tests in `tests/`, and
`keymap-popup` is the only non-core runtime dependency.

## Architecture

- `forgejo-api.el` owns asynchronous Forgejo REST requests, response
  classification, pagination, rate limits, and timeout cleanup.
- `forgejo-db.el` is the SQLite cache. Issue, pull-request, and timeline views
  should render from cached data; sync callbacks upsert responses and then
  refresh affected buffers. Do not block ordinary views on network requests.
- List views use `tabulated-list-mode` with `forgejo-tl-print`; detail and
  timeline views use EWOC. Keep rendering separate from API and database code.
- Issue, pull-request, review, notification, settings, and watch behavior belongs
  in the corresponding modules. Keep Git and AGit-Flow subprocess behavior in
  `forgejo-vc.el`.
- Scope state by host, owner, and repository. Async callbacks must verify that
  buffers and processes are live and that the response still belongs to the
  intended repository.

## API and data safety

- Forgejo is authoritative; do not assume GitHub-compatible endpoints or
  response shapes. Verify behavior against current Forgejo documentation or
  source, including status codes and issue-versus-pull filter differences.
- Preserve pagination metadata and partial-result handling. Never silently
  present an incomplete page set as complete.
- Authenticate through `auth-source`. Never hard-code, persist in fixtures, or
  expose tokens in logs, messages, errors, or test output.
- Keep HTTP response parsing and argument construction pure where practical.
  Always clean up retrieval buffers, processes, and timeout timers.
- Use parameterized SQLite operations and transactions for related writes. Tests
  must use temporary databases and must not touch a user's cache or credentials.

## Emacs Lisp conventions

- Use lexical binding. Public names use `forgejo-`; internal helpers use
  `forgejo--` or `forgejo-MODULE--`.
- Keep interactive commands thin. Prefer small pure helpers, explicit arguments,
  and plain alists or plists; apply network, database, process, buffer, and
  notification effects at module boundaries.
- Every `defcustom` and `defface` outside `forgejo.el` needs an explicit
  `:group 'forgejo`; always provide an accurate `:type` or face specification.
- Use `keymap-popup-define` for interactive popup keymaps and ordinary
  `defvar-keymap` for non-popup parent maps.
- Keep changes focused and add ERT coverage for API shapes, cache behavior,
  pagination, filtering, buffer rendering, and AGit argument construction.

## Verification

The Makefile enters the Nix development environment when available.

```sh
make test      # Compile and run each ERT test file
make lint      # Run checkdoc over package sources
make compile   # Byte-compile package sources
make dev       # Compile, lint, compile tests, and run the full ERT suite
```

Run `make dev` and `nix flake check` before proposing a change. Add new source
and test files to the Makefile's explicit lists and `admin/sources`, the closed
Nix build manifest. Checkdoc diagnostics must fail the gate.

Use `make load EMACSCLIENT='emacsclient --socket-name forgejo-dev'` for a
running development Emacs. Private overrides belong in ignored `local.mk`.
Reload is one fail-fast eval with bounded map rollback, not an arbitrary Lisp
transaction. Preserve custom options and bindings. Run reload/failure tests
in a disposable Emacs; do not use a contributor's live editor or credentials.
See README.org for reload limitations and the non-Nix fallback.

## Contributions

Keep reports and patches focused on reproducible behavior. Include the relevant
test commands and their results. Contributions of roughly 15 lines or more
require [FSF copyright assignment](https://www.gnu.org/prep/maintain/html_node/Copyright-Papers.html);
see [the project discussion](https://codeberg.org/thanosapollo/emacs-forgejo/issues/17).
