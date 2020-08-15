# pamela

`pamela` gra**P**hifies [**A**rchimate **M**odel **E**xchange fi**L**e form**A**t](https://www.opengroup.org/open-group-archimate-model-exchange-file-format).

# Installation

First of all, get `stack` if you don't have it already: see the [official stack documentation](https://docs.haskellstack.org/en/stable/README/#how-to-install). Note that stack is also included in the [Haskell platform](http://hackage.haskell.org/platform/), and on Linux it is usually available in your package manager.

If you have `git`, you can now clone the repository and build:

```shell
git clone https://github.com/normenmueller/pamela.git
cd pamela
git checkout <commit/tag/branch>
stack install
```

If you don't have `git`, just download the sources for your preferred
commit/branch/tag via the GitHub interface, and run `stack install` in the
directory that contains `stack.yaml` file.

This will install `pamela` executable to `$HOME/.local/bin`.

To install the `bash` completion *system-wide*:

```
pamela --bash-completion-script pamela >/etc/bash_completion.d/pamela
```

To install the `zsh` completion user-specific, add the following to your `.zshrc`:

```
pamela --zsh-completion-script `which pamela` > ${HOME}/.local/_pamela
FPATH=${HOME}/.local/:$FPATH

if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh-completions:$FPATH

  autoload -Uz compinit
  compinit
fi
```

# Usage

Usage information is available via `pamela -h`.

# License

See [LICENSE](https://github.com/normenmueller/armlet/blob/master/LICENSE) for
details.

© 2020 Normen Müller
