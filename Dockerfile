FROM nixos/nix
LABEL description="Códigos de exemplo utilizados na disciplina BCC328 - Construção de Compiladores I"

RUN nix-env --install --attr nixpkgs.vim
RUN git clone https://github.com/BCC328-compiladores/bcc328-exemplos.git
WORKDIR bcc328-exemplos/
RUN nix-channel --update
RUN nix-shell --pure --command "cabal update"
