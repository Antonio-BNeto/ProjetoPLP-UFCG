# 🎮 Batalha Naval em Prolog

Este projeto implementa o clássico jogo **Batalha Naval** utilizando **Prolog**.  
O objetivo é praticar conceitos de **Paradigma Lógico** vistos na disciplina **Paradigmas de Linguagens de Programação (PLP)** — UFCG.

---

## 🚀 Como executar

### 1. Instalar o SWI-Prolog

No Linux (exemplo com Fedora/Arch/Debian):

```bash
sudo dnf install swi-prolog        # Fedora
sudo pacman -S swi-prolog          # Arch Linux
sudo apt install swi-prolog        # Ubuntu/Debian
```

### 2. Clonar o repositório

```bash
git clone https://github.com/Antonio-BNeto/ProjetoPLP-UFCG.git
cd ProjetoPLP-UFCG/Prolog
```

### 3. Executar o jogo

A partir da raiz do projeto:

```bash
swipl -s src/main.pl
```

O jogo iniciará automaticamente chamando o predicado `start/0`.

---

## 🎯 Funcionalidades

- Menu inicial com opções de:
  - Iniciar jogo
  - Como funciona
  - Sair
- Posicionamento de navios do jogador
- Geração automática de navios do Bot
- Sistema de combate:
  - Acerto
  - Erro
  - Navio afundado
- Verificação de vitória/derrota
- Exibição lado a lado dos tabuleiros
- Arte em ASCII para menus e resultados