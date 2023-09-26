Código Haskell de exemplo para Construção de Compiladores
==========================================================================

Este repositório contém todo os exemplos de código utilizados na disciplina 
BCC328 - Construção de Compiladores I.


Configurando o ambiente de desenvolvimento
----------------------------------------------

O ambiente contendo todas as versões adequadas de compiladores e bibliotecas
pode ser obtido usando o gerenciador de pacotes [Nix](https://nixos.org/download).
Versões para diferentes sistemas operacionais estão disponíveis no link 
anterior.

Após ter instalado o Nix, clone o respositório e execute o seguinte comando na 
pasta contendo o arquivo `default.nix`:

```
nix-shell --pure
```

Após uma longa sequência de instalação e compilação de ferramentas, você terá 
o ambiente propriamente configurado.


Estrutura do código
---------------------

O repositório é composto pelas seguintes pastas:

- Automata: implementação de algoritmos sobre autômatos finitos.

- Exp: implementação de um compilador de expressões aritméticas. A versão atual 
  gera código executável utilizando a infraestrutura do LLVM.

- Imp: implementação de um interpretador / compilador para uma máquina virtual 
  para uma linguagem imperativa simples.

- Lexing: exemplo de uso de um gerador de analisador léxico.

- Markup: implementação de um compilador protótipo de markdown para html. Utilizado
  para revisão da linguagem Haskell.

- Parsing: implementação de algoritmos sobre gramáticas livres de contexto.

- Stack: implementação de uma máquina virtual utilizando uma arquitetura baseada 
  em pilha.


Executando / Testando código
-------------------------------

Para executar e testar o código você precisa iniciar o `nix-shell`.

Para gerenciamento de dependências deste projeto, utilizamos o `cabal`. Para compilar 
o código deste projeto (gerando diferentes executáveis), execute:

```
cabal new-build
```

Para utilizar o interpretador da linguagem, execute:

```
cabal new-repl
```

