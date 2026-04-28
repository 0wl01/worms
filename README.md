# Worms da Wish

![Build Status](https://img.shields.io/badge/Build-Passing-brightgreen?style=for-the-badge)


## Origin

Originally an University Project in UMinho, now an continuously improved p# 🐛 Worms da Wish
> A Haskell-powered 2D artillery game focused on functional purity and chaotic physics.


## 🛠 Tech Stack
- **Language:** ![Haskell](https://img.shields.io/badge/Haskell-5D4F85?style=for-the-badge&logo=haskell&logoColor=white)
- **Graphics:** Gloss Library
- **Build System:** Stack / Cabal
- **Environment:** ![Platform](https://img.shields.io/badge/Platform-WSL2%20%2F%20Linux-blue?style=for-the-badge)

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