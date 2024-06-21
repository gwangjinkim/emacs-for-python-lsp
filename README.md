# Emacs Configuration for Python Development

This repository contains an Emacs configuration optimized for Python development. It leverages modern tools and packages like `lsp-mode`, `pyright`, `Projectile`, `Ivy`, and more to provide a powerful and efficient development environment.

## Features

- **Project Management** with Projectile
- **Code Navigation** and **Autocompletion** with LSP and Pyright
- **Syntax Checking** with Flycheck
- **Version Control** with Magit
- **Interactive Python Shell** with IPython
- **Virtual Environment Management** with pyvenv

## Installation

1. Ensure you have Emacs installed.
2. Clone this repository to your Emacs configuration directory (e.g., `~/.emacs.d`).
3. Install `pyright` globally using npm:
   ```sh
   npm install -g pyright
4. Open Emacs and let it install the required packages.

## Key Bindings and Usage

Here is a table of useful shortcuts and commands for everyday development work with this configuration:

| Shortcut                  | Command                                  | Description                                                                                      |
|---------------------------|------------------------------------------|--------------------------------------------------------------------------------------------------|
| `C-x C-f`                 | `find-file`                               | Open a file.                                                                                     |
| `C-x b`                   | `switch-to-buffer`                        | Switch between open buffers.                                                                     |
| `C-c p f`                 | `projectile-find-file`                    | Find a file in the current project.                                                              |
| `C-c p p`                 | `projectile-switch-project`               | Switch between projects.                                                                         |
| `C-c p s g`               | `projectile-grep`                         | Search for a string in the project using grep.                                                   |
| `M-.`                     | `lsp-find-definition`                     | Jump to the definition of the symbol under the cursor.                                           |
| `M-,`                     | `xref-pop-marker-stack`                   | Jump back to the previous location before `lsp-find-definition`.                                 |
| `C-c l r`                 | `lsp-rename`                              | Rename the symbol under the cursor.                                                              |
| `C-c l a`                 | `lsp-execute-code-action`                 | Perform a code action (e.g., quick fix).                                                         |
| `C-c l h`                 | `lsp-describe-thing-at-point`             | Show documentation for the symbol under the cursor.                                              |
| `C-c l d`                 | `lsp-ui-doc-show`                         | Show inline documentation popup.                                                                 |
| `C-c l i`                 | `lsp-ui-imenu`                            | Show an imenu for the current buffer, useful for navigating code.                                |
| `C-c c`                   | `compile`                                 | Compile the project.                                                                             |
| `C-c p t`                 | `projectile-test-project`                 | Run the tests for the project.                                                                   |
| `C-c C-c`                 | `elpy-shell-send-region-or-buffer`        | Send the current region or buffer to the Python shell.                                           |
| `C-c C-k`                 | `elpy-shell-kill`                         | Kill the Python shell.                                                                           |
| `M-x pyvenv-workon`       | `pyvenv-workon`                           | Activate a virtual environment by name.                                                          |
| `M-x pyvenv-deactivate`   | `pyvenv-deactivate`                       | Deactivate the current virtual environment.                                                      |
| `C-x g`                   | `magit-status`                            | Open Magit status buffer for Git integration.                                                    |
| `C-c M-g`                 | `magit-dispatch`                          | Open Magit dispatch popup for various Git commands.                                              |
| `C-s`                     | `swiper`                                  | Search for a string in the current buffer.                                                       |
| `M-x counsel-M-x`         | `counsel-M-x`                             | Enhanced `M-x` with Ivy integration for command execution.                                       |
| `M-x counsel-find-file`   | `counsel-find-file`                       | Enhanced file finder with Ivy integration.                                                       |


## Getting Started

To get started with this configuration:

1. Open Emacs.
2. Use `C-x C-f` to open a file or `C-c p p` to switch to a project.
3. Use `M-x lsp` to start the language server for your project.
4. Use `C-c C-c` to send code to the Python shell and `C-x g` to open Magit for version control.

## Tips

- Use `C-c p f` to quickly navigate to files within your project.
- Leverage `M-.` and `M-,` for efficient code navigation.
- Use `C-c l r` to rename symbols across your project.
- Regularly use `C-x g` to commit and push your changes with Magit.

## Customization

Feel free to customize this configuration to fit your workflow. 
The key bindings and commands listed here are just a starting point. 
Explore the documentation for each package to discover more features and options.

Happy coding!




## Some more key bindings

| Shortcut                  | Command                                  | Description                                                                                      |
|---------------------------|------------------------------------------|--------------------------------------------------------------------------------------------------|
| `C-x C-f`                 | `find-file`                               | Open a file.                                                                                     |
| `C-x b`                   | `switch-to-buffer`                        | Switch between open buffers.                                                                     |
| `C-x k`                   | `kill-buffer`                             | Kill the current buffer.                                                                         |
| `C-x C-s`                 | `save-buffer`                             | Save the current buffer.                                                                         |
| `C-x C-w`                 | `write-file`                              | Write the current buffer to a file.                                                              |
| `C-g`                     | `keyboard-quit`                           | Cancel the current command.                                                                      |
| `M-x`                     | `execute-extended-command`                | Execute an extended command by name.                                                             |
| `C-x o`                   | `other-window`                            | Switch to another window.                                                                        |
| `C-x 1`                   | `delete-other-windows`                    | Close all other windows.                                                                         |
| `C-x 2`                   | `split-window-below`                      | Split the window horizontally.                                                                   |
| `C-x 3`                   | `split-window-right`                      | Split the window vertically.                                                                     |
| `C-x 0`                   | `delete-window`                           | Delete the current window.                                                                       |
| `M-<`                     | `beginning-of-buffer`                     | Move to the beginning of the buffer.                                                             |
| `M->`                     | `end-of-buffer`                           | Move to the end of the buffer.                                                                   |
| `M-/`                     | `dabbrev-expand`                          | Perform dynamic abbreviation expansion.                                                          |
| `C-s`                     | `swiper`                                  | Search for a string in the current buffer.                                                       |
| `C-r`                     | `swiper`                                  | Search backward for a string in the current buffer.                                              |
| `M-%`                     | `query-replace`                           | Interactive search and replace.                                                                  |
| `C-M-%`                   | `query-replace-regexp`                    | Interactive search and replace using regular expressions.                                        |
| `M-x counsel-M-x`         | `counsel-M-x`                             | Enhanced `M-x` with Ivy integration for command execution.                                       |
| `M-x counsel-find-file`   | `counsel-find-file`                       | Enhanced file finder with Ivy integration.                                                       |
| `C-c p f`                 | `projectile-find-file`                    | Find a file in the current project.                                                              |
| `C-c p p`                 | `projectile-switch-project`               | Switch between projects.                                                                         |
| `C-c p s g`               | `projectile-grep`                         | Search for a string in the project using grep.                                                   |
| `M-.`                     | `lsp-find-definition`                     | Jump to the definition of the symbol under the cursor.                                           |
| `M-,`                     | `xref-pop-marker-stack`                   | Jump back to the previous location before `lsp-find-definition`.                                 |
| `C-c l r`                 | `lsp-rename`                              | Rename the symbol under the cursor.                                                              |
| `C-c l a`                 | `lsp-execute-code-action`                 | Perform a code action (e.g., quick fix).                                                         |
| `C-c l h`                 | `lsp-describe-thing-at-point`             | Show documentation for the symbol under the cursor.                                              |
| `C-c l d`                 | `lsp-ui-doc-show`                         | Show inline documentation popup.                                                                 |
| `C-c l i`                 | `lsp-ui-imenu`                            | Show an imenu for the current buffer, useful for navigating code.                                |
| `C-c c`                   | `compile`                                 | Compile the project.                                                                             |
| `C-c p t`                 | `projectile-test-project`                 | Run the tests for the project.                                                                   |
| `C-c C-c`                 | `elpy-shell-send-region-or-buffer`        | Send the current region or buffer to the Python shell.                                           |
| `C-c C-k`                 | `elpy-shell-kill`                         | Kill the Python shell.                                                                           |
| `M-x pyvenv-workon`       | `pyvenv-workon`                           | Activate a virtual environment by name.                                                          |
| `M-x pyvenv-deactivate`   | `pyvenv-deactivate`                       | Deactivate the current virtual environment.                                                      |
| `C-x g`                   | `magit-status`                            | Open Magit status buffer for Git integration.                                                    |
| `C-c M-g`                 | `magit-dispatch`                          | Open Magit dispatch popup for various Git commands.                                              |
| `C-x h`                   | `mark-whole-buffer`                       | Select the entire buffer.                                                                        |
| `M-w`                     | `kill-ring-save`                          | Copy the selected region to the kill ring (clipboard).                                           |
| `C-w`                     | `kill-region`                             | Cut the selected region to the kill ring (clipboard).                                            |
| `C-y`                     | `yank`                                    | Paste from the kill ring (clipboard).                                                            |
| `M-y`                     | `yank-pop`                                | Cycle through previous yanks.                                                                    |
| `C-=`                     | `er/expand-region`                        | Expand the selected region by semantic units.                                                    |
| `M-x flycheck-list-errors`| `flycheck-list-errors`                    | Display a list of errors in the current buffer.                                                  |
| `M-x flycheck-next-error` | `flycheck-next-error`                     | Jump to the next error.                                                                          |
| `M-x flycheck-previous-error` | `flycheck-previous-error`             | Jump to the previous error.                                                                      |
| `C-M-\`                   | `indent-region`                           | Indent the selected region according to the major mode.                                          |
| `M-x`                     | `eshell`                                  | Open an Emacs shell for running shell commands.                                                  |
| `C-x C-e`                 | `eval-last-sexp`                          | Evaluate the Emacs Lisp expression before the cursor.                                            |
| `M-:`                     | `eval-expression`                         | Evaluate a single Emacs Lisp expression.                                                         |

