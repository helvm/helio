# 🏗️ INSTALL

How to download, test and run.

## Download

You need a client of `git`:
```bash
git clone https://github.com/helvm/helio.git
cd helio
```

## Compile

To compile you need `cabal` and `make`:
```bash
make
```

## By make

```bash
# Start REPL.
make repl

# Generate documentation.
make haddock

# Analyze coverage.
make hpc
```

## Install StAN
```bash
git clone https://github.com/kowainik/stan.git
cd stan
cabal v2-build exe:stan
cp "$(cabal v2-exec --verbose=0 --offline sh -- -c 'command -v stan')" ~/.local/bin/stan
```

## Other

For more see [CONTRIBUTING](../developers/CONTRIBUTING.md).

## 🦄 🌈 ❤️ 💛 💚 💙 🤍 🖤
