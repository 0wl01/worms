# Worms da Wish

<p align="center">
  <img src="assets/worms_templogo.png" alt="Worms da Wish Logo" width="400">
</p>

## Origin

An University Project in UMinho, now an continuously improved game 🐛 <b>
Worms da Wish </b> 
> A Haskell-powered 2D artillery game focused on functional purity and chaotic physics.

## 🛠 Tech Stack
- **Language:** <img src="https://raw.githubusercontent.com/devicons/devicon/master/icons/haskell/haskell-original.svg" width="45" align="center" />
- **Graphics:** <img src="https://raw.githubusercontent.com/devicons/devicon/master/icons/opengl/opengl-plain.svg" width="40" title="OpenGL/Gloss" align="center" />
- **Build System:** <img src="https://img.shields.io/badge/Cabal-ffad13?style=for-the-badge&logo=haskell&logoColor=white" align="center"  />
- **Environment:** <img src="https://raw.githubusercontent.com/devicons/devicon/master/icons/linux/linux-original.svg" width="40" title="Linux" align="center" />

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