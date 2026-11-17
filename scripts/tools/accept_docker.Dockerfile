# accept_docker.Dockerfile -- imagem do ACEITE em Docker local (dev-only, fora do bundle). Uma imagem por versao (ref/nova), a partir do
# bundle linux-x86_64 ja construido. Base e snapshot do apt IGUAIS aos de build_bundle_linux.sh (IMAGE, APT_SNAPSHOT).
# Contexto: um diretorio so com bundle.tar.zst, bundle.tar.zst.sha256 e install.sh (o instalador do MESMO commit do bundle; o bundle nao o
# traz), criado por accept_docker.sh. A instalacao e a de usuario (install.sh --bundle): /opt/prevcarga/{bin,versions/<v>,current}; os
# links /opt/prevcarga/config e /opt/prevcarga/models apontam para code/ da versao instalada (pontos de montagem :ro dos overlays).
# A base nao traz ca-certificates: o apt do snapshot (https) roda com Verify-Peer=false no update E no install (E8-T4; antes so o update); a
# integridade dos pacotes segue pela assinatura GPG do Release (chaveiro da base) e sha256 do indice.
ARG BASE=ubuntu:22.04@sha256:5ec03bb3441e8b0bf3b4f9cd4629a1ae763010dc3035bb8da3ae6cf026486401
FROM ${BASE}
ARG APT_SNAPSHOT=20261027T000000Z
RUN echo "deb [check-valid-until=no] https://snapshot.ubuntu.com/ubuntu/${APT_SNAPSHOT} jammy main universe" > /etc/apt/sources.list \
 && apt-get update -o Acquire::https::Verify-Peer=false \
 && apt-get install -o Acquire::https::Verify-Peer=false -y --no-install-recommends ca-certificates tzdata zstd \
 && rm -rf /var/lib/apt/lists/*
COPY bundle.tar.zst bundle.tar.zst.sha256 install.sh /tmp/b/
RUN cd /tmp/b && sha256sum -c bundle.tar.zst.sha256 \
 && sh install.sh --version accept --bundle /tmp/b/bundle.tar.zst --sha256 "$(cut -c1-64 bundle.tar.zst.sha256)" --root /opt/prevcarga \
 && v="$(head -n 1 /opt/prevcarga/current)" \
 && ln -s "versions/$v/code/config" /opt/prevcarga/config && ln -s "versions/$v/code/models" /opt/prevcarga/models \
 && rm -rf /tmp/b
ENV TZ=UTC LC_ALL=C.UTF-8 PATH=/opt/prevcarga/bin:/usr/sbin:/usr/bin:/sbin:/bin
ENTRYPOINT ["prevcarga"]
