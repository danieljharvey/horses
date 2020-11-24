FROM nixos/nix

RUN echo "trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" >> /etc/nix/nix.conf
RUN echo "substituters = https://hydra.iohk.io" >> /etc/nix/nix.conf

RUN cat /etc/nix/nix.conf

COPY . /
RUN nix-build

RUN cd result/bin && ls

CMD ["result/bin/mimsa","server"]
