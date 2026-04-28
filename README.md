# Worms da Wish

## Origin

Originally an University Project in UMinho, now an continuously improved p# 🐛 Worms da Wish
> A Haskell-powered 2D artillery game focused on functional purity and chaotic physics.


## 🛠 Tech Stack
- **Language:** Haskell
- **Graphics:** Gloss Library
- **Build System:** Stack / Cabal
- **Environment:** Developed on Linux

## 🚀 Key Features & Engineering
- Nothing so far

## 📦 How to Run

- Clone this Repository

```sh
cabal run
```

## 📜 Documentation
This project follows Haddock standards. To generate:
```sh
cabal haddock
```

## ✅ Quality Assurance & Testing
The project uses a comprehensive suite of unit tests to ensure game logic reliability.

- **Edge Case Coverage:** Validation of map boundaries, non-rectangular maps, and overlapping entities.
- **State Integrity:** Formal rules to prevent invalid states (e.g., worms alive in water or inside solid rock).

To run the tests:
```sh
cabal test
```