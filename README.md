# Worms da Wish

<p align="center">
  <img src="link_para_tua_imagem.png" alt="Worms da Wish Logo" width="400">
</p>

## Origin

Originally an University Project in UMinho, now an continuously improved p# 🐛 Worms da Wish
> A Haskell-powered 2D artillery game focused on functional purity and chaotic physics.


![OpenGL](https://img.shields.io/badge/OpenGL-FFFFFF?style=for-the-badge&logo=opengl&logoColor=5586A4)
## 🛠 Tech Stack
- **Language:** ![Haskell](https://img.shields.io/badge/Haskell-5D4F85?style=for-the-badge&logo=haskell&logoColor=white)
- **Graphics:** ![Gloss](https://img.shields.io/badge/Graphics-Gloss-blue?style=for-the-badge&logo=opengl&logoColor=white)
- **Build System:** ![Cabal](https://img.shields.io/badge/Cabal-ffad13?style=for-the-badge&logo=haskell&logoColor=white)
- **Environment:** ![Linux](https://img.shields.io/badge/Linux-FCC624?style=for-the-badge&logo=linux&logoColor=black)

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