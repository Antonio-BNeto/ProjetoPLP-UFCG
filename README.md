# ⚓️ BATALHA NAVAL ⚓️

## 📖 Descrição
Este projeto é uma implementação do clássico jogo Batalha Naval, desenvolvido em Haskell e executado no terminal. Foi criado como parte da avaliação da disciplina de Paradigmas de Linguagens de Programação.

---

### 🛠 Tecnologias e Ferramentas
- **Haskell** (GHC 9.6.x)
- **Cabal** (ferramenta de build e execução)
- **Terminal** (entrada e saída de dados)

---

## ▶️ Como Executar

### 1️⃣ Pré-requisitos
- Instalar **GHC** e **Cabal**:
  - No Linux:
    ```bash
    sudo apt install ghc cabal-install
    ```
  - No macOS:
    ```bash
    brew install ghc cabal-install
    ```
  - No Windows:  
    Baixar pelo [Haskell Platform](https://www.haskell.org/platform/).

### 2️⃣ Clonando, Compilando e Rodando

1.  Para clonar o repositório, execute um dos dois comandos abaixo no seu terminal:

    ```bash
    # Usando HTTPS
    git clone https://github.com/Antonio-BNeto/ProjetoPLP-UFCG.git
    # Ou usando SSH
    git clone git@github.com:Antonio-BNeto/ProjetoPLP-UFCG.git
    ```

2. Entre na pasta do projeto:

    ```bash
    cd ProjetoPLP-UFCG/Haskell/
    ```

3. Compile e execute o jogo:

    ```bash
    cabal run
    ```

---

## 🎮 Como jogar
Você enfrentará um bot em uma batalha para ver quem afunda a frota inimiga primeiro. Cada jogador possui um tabuleiro com 17 casas ocupadas por navios:

- 1 Porta aviões (5 espaços)
- 1 Encouraçado (4 espaços)
- 1 Cruzador (3 espaços)
- 1 Submarino (3 espaços)
- 1 Destroyer (2 espaços)

### Instruções:

1. No menu principal, digite 1 para iniciar um novo jogo ou 2 para sair.
2. Para atacar, digite as coordenadas (linha e coluna) separadas por um espaço e pressione Enter.
    - Formato: linha coluna

    - Exemplo: 3 5 (ataca a casa na linha 3, coluna 5)

---

### 👥 Integrantes

- [Antônio Neto](https://github.com/Antonio-BNeto)
- [Arthur Correia](https://github.com/ArthurCorreiaa)
- [Carlos Leonardo](https://github.com/Carlosalvesss)
- [Matheus Miranda](https://github.com/matheusmendonca0706)
- [Ruan Rodrigues](https://github.com/Ruanrodrigues20)
- [Yasmim Silva](https://github.com/yasmim-silva)

---
📜 Licença
Projeto desenvolvido exclusivamente para fins acadêmicos.
