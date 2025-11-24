# AGENT.md

Working notes for future agents contributing to flywire. This doc explains our planning/devlogs flow, how we develop and test in Doom Emacs, and coding practices that keep the Elisp predictable in a live Emacs environment.

## Workflow: Plans vs Notes

- Plans live in `devlogs/` with filenames like `YYYYMMDDHHMM-Plan_Phase_N.org`.
  - Contents: tasks, decisions, acceptance criteria, tentative file layout.
- Notes (what actually happened) also live in `devlogs/` as `YYYYMMDDHHMM-Notes_*.org`.
  - Contents: what worked, what didn’t, concrete diffs/decisions, and follow‑ups.
- Keep planning docs forward‑looking; keep notes factual/retrospective. Don’t mix them.
- Reference: see `devlogs/20251116105736-Plan_Phase_0.org`, `devlogs/20251116105752-Plan_Phase_1.org`, and subsequent `Notes_*` entries.
- Timestamp accuracy
  - Create the file however you like (Org capture, `touch`, Emacs, etc.), then rename it based on the filesystem creation time so the prefix is always correct.
  - Command: `ts=$(stat -f '%SB' -t '%Y%m%d%H%M%S' devlogs/tmp.org) && mv devlogs/tmp.org "devlogs/${ts}-Plan_Phase_2.org"`.
  - This uses macOS `stat` to read the real creation time (`%SB`) and ensures distinct prefixes even when multiple files are created the same minute.
- **Phase Completion**: Once you finish a phase, **write a devlog before making a commit**. The commit should contain the devlog describing what was done, challenges/changes, and next steps.
- **Parentheses/Syntax**: If you get stuck trying to balance parens in edited files or fixing syntax errors, you can stop and ask the user to intervene.

## Elisp Conventions (Project-Specific)

- Headers & lexical-binding
  - All files start with `-*- lexical-binding: t; -*-`.
  - Provide a clear package header and `provide` symbol.
- Namespaces
  - Public functions: `flywire-...`; internal helpers: `flywire--...`.
  - One feature per file; file provides its feature name.
- Keybindings
  - Default prefix is `C-c C-d`
  - Avoid `C-c <letter>`; that space is reserved for users.
- Autoloads & load order
  - Interactive commands that live outside `flywire.el` must be autoloaded from `flywire.el`, e.g.:
    ```elisp
    (autoload 'flywire-foo "flywire-foo" "Open foo" t)
    ```
  - Libraries may `(require 'flywire)` for shared defs (defgroup, faces, errors).
- Compatibility
  - Target Emacs 28+
- Docs & comments
  - Write helpful docstrings and minimal comments before complex blocks. Keep code self-explanatory.

## Testing & Quality Gates

- **Mandatory Runner**: You **MUST** run tests via `nix run .#test` which sets up the test dependencies.
- **Run Often**: Check tests and lints often and try to ensure they both run cleanly.
- **Runner Integrity**: Tests can fail while changes are in progress, but the **test runner itself should not be in a broken state** (e.g., ensure syntax is valid so tests *can* run).
- Linting: run `nix run .#lint` to check for style, packaging, and byte-compilation issues.
- Keep UI non-blocking

### Running the automated test suite

 - Default runner:  
   ```sh
   nix run .#test
   ```
   Or, if you want to run manually in a dev shell:
   ```sh
   nix develop
   emacs -q --batch -l test/run-tests.el
   ```  
   Dependencies (including `ert-async` and `propcheck`) are provided by the Nix environment.
 - The runner prints every asynchronous test’s progress. When debugging, re-run with `EDEBUG=1` or add `(message ...)` calls, but remove noisy logging before you ship.
 - If you add new tests, make sure they can run in batch (no interactive prompts, no buffers left behind).

### Running the linter

 - Default runner:
   ```sh
   nix run .#lint
   ```
   This checks for:
   - `checkdoc` (docstring style)
   - `package-lint` (packaging best practices; ignores "redundant" naming warnings)
   - `byte-compile` (syntax and compilation warnings/errors)

## Devlogs: When To Write Notes

- After any discrete task lands (keybinding fix, autoload change, new file skeleton), add a `Notes_*.org` entry summarizing:
  - Problem, root cause, fix, verification steps, and any follow-ups.
- Keep entries short and scan-friendly; include commands or Emacs forms that helped verify.

## Common Pitfalls

- Autoload failures: ensure `flywire.el` autoloads interactive commands from other files.
- Reserved key sequences: don’t bind `C-c <letter>`.
- Load-path issues: confirm the working copy is in `load-path` during WIP.
- Over-eager `require`: avoid heavy `require` at top-level if it creates cycles; autoload where possible.

## Building Agent Environments

When integrating Flywire into an agent loop, prefer the **Session** API over global functions.

### 1. Creating a Dedicated Environment

Avoid running agent actions in the user's global context if possible. Instead, define an environment that targets a specific frame or buffer.

```elisp
(defun my-agent-env-create ()
  (let ((frame (make-frame '((name . "Agent Frame")))))
    (flywire-env-create
     :name "my-agent"
     :run (lambda (thunk)
            (with-selected-frame frame
              (funcall thunk)))
     :snapshot (lambda (profile)
                 (with-selected-frame frame
                   (flywire-snapshot-get-snapshot profile)))
     :teardown (lambda (_session)
                 (delete-frame frame)))))
```

### 2. Handling Interactive Events

Agents often need to pause for user input (minibuffer) or wait for operations to complete. Use `flywire-session-on-event` to react to these states.

- **`:minibuffer-open`**: The agent should stop executing steps and optionally decide to fill the prompt.
- **`:idle`**: Useful for triggering "thinking" loops when Emacs is quiet.

### 3. Headless Testing

For CI/CD, use a headless environment that simulates windowing without a GUI.

```elisp
(defun my-headless-env ()
  (flywire-env-create
   :name "headless"
   :run (lambda (thunk)
          (with-temp-buffer
            (funcall thunk)))))
```

Use `nix run .#test` to verify your agent logic in batch mode.