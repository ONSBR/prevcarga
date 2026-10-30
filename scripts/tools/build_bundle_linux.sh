#!/usr/bin/env bash
# build_bundle_linux.sh -- build REPRODUTIVEL do bundle linux-x86_64 (R 4.5.3, ADR-0036; M8.1). DEV-ONLY, bash so-Linux, fora do bundle.
#
# Uso: scripts/tools/build_bundle_linux.sh <versao> [--out DIR] [--src REPO] [--rev REV]
#   --out  diretorio de saida (default <repo>/dist, gitignored). Recebe prevcarga-<v>-linux-x86_64.tar.zst, .sha256 e .RUNTIME.json
#   --src  repositorio git de onde sai o code/ (default: este repo). --rev: commit (default HEAD). Arvore suja => recusa.
#
# O build roda DENTRO de um container ubuntu:22.04 pinado por digest, limpo (sem python3), com apt de snapshot.ubuntu.com numa data
# fixa e MAKEFLAGS=-j1. Nada e publicado: so escreve em --out. Mesmo commit + mesmo --out-agnostico => mesmo runtime_id.
# Rede usada no build (somente leitura): snapshot.ubuntu.com, cdn.posit.co (R .deb, sha256 pinado), packagemanager.posit.co (renv.lock).
set -euo pipefail

IMAGE='ubuntu:22.04@sha256:5ec03bb3441e8b0bf3b4f9cd4629a1ae763010dc3035bb8da3ae6cf026486401'
APT_SNAPSHOT='20261027T000000Z'
R_VERSION='4.5.3'
R_DEB_URL="https://cdn.posit.co/r/ubuntu-2204/pkgs/r-${R_VERSION}_1_amd64.deb"
R_DEB_SHA256='2fc9317e2bec2163bb132dff7a499812e3f06582d30c175d77cf6a9b7da5c0d4'
P3M_BINARY_REPO='https://packagemanager.posit.co/cran/__linux__/jammy/2026-10-27'
# glibc core NAO entra no syslib (vem do host; o bundle exige glibc >= a do container, 2.35)
GLIBC_CORE_RE='^(libc|libm|libpthread|libdl|librt|libutil|libresolv|libmvec|libanl|libnsl|libBrokenLocale|libnss_[a-z]+|ld-linux[-a-z0-9.]*)\.so'

die() { echo "build_bundle_linux: $*" >&2; exit 2; }

# ------------------------------------------------------------------ dentro do container
inside() {
  VER="$1"
  umask 022
  export DEBIAN_FRONTEND=noninteractive LC_ALL=C.UTF-8 LANG=C.UTF-8 MAKEFLAGS=-j1 TZ=UTC
  rm -f /etc/apt/sources.list.d/* 2>/dev/null || true
  # snapshot.ubuntu.com so atende por https e a imagem base nao traz certificados: ca-certificates (so confianca TLS do build,
  # nunca entra no bundle) vem do archive padrao ANTES de trocar para o snapshot pinado
  apt-get update -qq && apt-get install -y -qq --no-install-recommends ca-certificates >/dev/null
  {
    for suite in jammy jammy-updates jammy-security; do
      echo "deb https://snapshot.ubuntu.com/ubuntu/${APT_SNAPSHOT} ${suite} main restricted universe multiverse"
    done
  } > /etc/apt/sources.list
  echo "[build] apt snapshot update" >&2
  apt-get -o Acquire::Check-Valid-Until=false update -qq
  echo "[build] apt install toolchain" >&2
  # toolchain SEM python3 (nem recomendados); o R .deb puxa as bibliotecas de execucao
  apt-get install -y -qq --no-install-recommends ca-certificates curl zstd tar xz-utils binutils \
    build-essential gfortran pkg-config libcurl4-openssl-dev libssl-dev libxml2-dev zlib1g-dev libicu-dev >/dev/null
  echo "[build] R .deb" >&2
  curl -fsSL -o /tmp/r.deb "${R_DEB_URL}"
  echo "${R_DEB_SHA256}  /tmp/r.deb" | sha256sum -c - >/dev/null
  apt-get install -y -qq --no-install-recommends /tmp/r.deb >/dev/null
  command -v python3 >/dev/null 2>&1 && die "python3 presente no container de build"
  RBIN=/opt/R/${R_VERSION}/bin
  "${RBIN}/Rscript" --version 2>&1 | grep -q "${R_VERSION}" || die "R ${R_VERSION} nao instalado"

  # renv fica FORA da library do bundle (bootstrap em /build/renvlib); restore direto na library do proprio R, sem R_LIBS_*
  mkdir -p /build/renvlib /build/proj /build/stage/prevcarga/code
  CODE=/build/stage/prevcarga/code
  cp /src/renv.lock /build/proj/renv.lock
  "${RBIN}/Rscript" --vanilla -e "
    options(HTTPUserAgent = sprintf('R/%s R (%s)', getRversion(), paste(getRversion(), R.version\$platform, R.version\$arch, R.version\$os)))
    install.packages('renv', lib = '/build/renvlib', repos = '${P3M_BINARY_REPO}', quiet = TRUE)" >/dev/null
  echo "[build] renv restore" >&2
  export RENV_CONFIG_CACHE_ENABLED=FALSE RENV_CONFIG_SANDBOX_ENABLED=FALSE RENV_CONFIG_AUTOLOADER_ENABLED=FALSE
  export RENV_PATHS_ROOT=/build/renvroot LIBARROW_BINARY=true
  unset ARROW_S3 || true
  "${RBIN}/Rscript" --vanilla -e '
    .libPaths(c("/build/renvlib", .libPaths()))
    renv::restore(project = "/build/proj", lockfile = "/build/proj/renv.lock", library = R.home("library"),
                  clean = FALSE, prompt = FALSE)' 2>&1 | tee /build/restore.log
  # todo pacote do lock carrega pelo R do container sem R_LIBS_*
  "${RBIN}/Rscript" --vanilla -e '
    pk <- names(jsonlite::fromJSON("/build/proj/renv.lock", simplifyVector = FALSE)$Packages)
    bad <- pk[!vapply(pk, requireNamespace, NA, quietly = TRUE)]
    if (length(bad)) stop("pacotes nao carregam: ", paste(bad, collapse = ", "))'
  # pacotes que o renv compilou do fonte (sem binario P3M): honestidade em RUNTIME.json -> compilados
  grep -E 'built from source' /build/restore.log | sed -E 's/^- Installing ([^ ]+).*/\1/' | sort > /build/compilados.txt || true

  echo "[build] stage + syslib + manifest" >&2
  # stage: runtime/ (R_HOME do R do container) + syslib/ + code/ + RUNTIME.json
  ROOT=/build/stage/prevcarga
  rm -rf "${ROOT}"; mkdir -p "${ROOT}"
  cp -a "/opt/R/${R_VERSION}" "${ROOT}/runtime"
  RH="${ROOT}/runtime/lib/R"
  relocate_r "${RH}"
  build_syslib "${ROOT}"
  mkdir -p "${CODE}"
  "${RBIN}/Rscript" --vanilla /src/scripts/tools/bundle_manifest.R code-tree --src /src --dest "${CODE}"
  mkdir -p "${CODE}/install"
  "${RBIN}/Rscript" --vanilla /src/scripts/tools/export_error_catalog.R --out "${CODE}/install/error-catalog.tsv"
  BUILT_AT="$(date -u -d "@${SOURCE_DATE_EPOCH}" +%Y-%m-%dT%H:%M:%SZ)"
  "${RBIN}/Rscript" --vanilla /src/scripts/tools/bundle_manifest.R write --root "${ROOT}" --version "${VER}" \
    --platform linux-x86_64 --code-commit "${CODE_COMMIT}" --apt-snapshot "${APT_SNAPSHOT}" --built-at "${BUILT_AT}" \
    --compilados /build/compilados.txt | tee /build/runtime_id.txt

  echo "[build] tar.zst" >&2
  OUTF="/out/prevcarga-${VER}-linux-x86_64"
  tar --sort=name --mtime="@${SOURCE_DATE_EPOCH}" --owner=0 --group=0 --numeric-owner --format=posix \
      --pax-option=exthdr.name=%d/PaxHeaders/%f,delete=atime,delete=ctime \
      -C /build/stage -cf - prevcarga | zstd -19 -T1 -q -f -o "${OUTF}.tar.zst"
  cp "${ROOT}/RUNTIME.json" "${OUTF}.RUNTIME.json"
  ( cd /out && sha256sum "prevcarga-${VER}-linux-x86_64.tar.zst" > "prevcarga-${VER}-linux-x86_64.tar.zst.sha256" )
  chown -R "${HOST_UID}:${HOST_GID}" /out
  echo "runtime_id $(cat /build/runtime_id.txt)"
}

# R do Posit tem R_HOME fixo em bin/R, nos wrappers e em etc/ldpaths: torna o runtime/ relocavel (calculado a partir de $0).
relocate_r() {
  local rh="$1" f
  f="${rh}/bin/R"
  sed -i -E "s#/opt/R/${R_VERSION}/lib/R#\${R_HOME_DIR}#g; s#/opt/R/${R_VERSION}#\${R_PREFIX_DIR}#g" "${f}"
  sed -i -E '0,/^R_HOME_DIR=.*/s##R_HOME_DIR="$(cd "$(dirname "$(readlink -f "$0")")/.." \&\& pwd)"\nR_PREFIX_DIR="$(cd "${R_HOME_DIR}/../.." \&\& pwd)"#' "${f}"
  # wrappers de bin/: R e Rscript apontam para lib/R/bin (Rscript recebe RHOME para nao usar o prefixo compilado)
  cat > "${rh}/../../bin/R" <<'WRAP'
#!/bin/sh
d="$(cd "$(dirname "$(readlink -f "$0")")" && pwd)"
exec "${d}/../lib/R/bin/R" "$@"
WRAP
  cat > "${rh}/../../bin/Rscript" <<'WRAP'
#!/bin/sh
d="$(cd "$(dirname "$(readlink -f "$0")")" && pwd)"
RHOME="$(cd "${d}/../lib/R" && pwd)"
export RHOME
exec "${d}/../lib/R/bin/Rscript" "$@"
WRAP
  chmod 0755 "${rh}/../../bin/R" "${rh}/../../bin/Rscript"
  # syslib/ (irmao de runtime/) entra no caminho de bibliotecas, logo apos ${R_HOME}/lib e antes do sistema do host
  sed -i -E 's#R_LD_LIBRARY_PATH=\$\{R_HOME\}/lib:#R_LD_LIBRARY_PATH=${R_HOME}/lib:${R_HOME}/../../../syslib:#' "${rh}/etc/ldpaths"
  grep -q 'syslib' "${rh}/etc/ldpaths" || die "etc/ldpaths sem syslib"
}

# syslib/: fecho ldd de R e de todo .so da library, menos a glibc core
build_syslib() {
  local root="$1" rh sl so
  rh="${root}/runtime/lib/R"; sl="${root}/syslib"
  mkdir -p "${sl}"
  {
    echo "${rh}/bin/exec/R"
    find "${rh}/lib" "${rh}/modules" "${rh}/library" -name '*.so' -o -name '*.so.*' 2>/dev/null
  } | sort | while read -r so; do
    LD_LIBRARY_PATH="${rh}/lib" ldd "${so}" 2>/dev/null | awk '/=> \//{print $3}'
  done | sort -u | while read -r lib; do
    case "${lib}" in "${root}/runtime/"*|"/opt/R/${R_VERSION}/"*) continue;; esac
    b="$(basename "${lib}")"
    if echo "${b}" | grep -Eq "${GLIBC_CORE_RE}"; then continue; fi
    install -m 0755 "$(readlink -f "${lib}")" "${sl}/${b}"
  done
  # dependencias nao resolvidas (ldd "not found") quebram o bundle
  if find "${rh}/lib" "${rh}/library" -name '*.so*' | sort | while read -r so; do
       LD_LIBRARY_PATH="${rh}/lib:${sl}" ldd "${so}" 2>/dev/null | grep 'not found' | sed "s#^#${so##*/}: #"; done | grep .; then
    die "ldd com dependencia 'not found' no bundle"
  fi
}

if [ "${1:-}" = "--inside" ]; then
  shift; inside "$@"; exit 0
fi

# ------------------------------------------------------------------ no host
[ "$(uname -s)" = Linux ] || die "so Linux"
VER="${1:-}"; [ -n "${VER}" ] || die "uso: build_bundle_linux.sh <versao> [--out DIR] [--src REPO] [--rev REV]"
shift
HERE="$(cd "$(dirname "$0")/../.." && pwd)"
SRC="${HERE}"; REV=HEAD; OUT=""
while [ $# -gt 0 ]; do
  case "$1" in
    --out) OUT="$2"; shift 2;;
    --src) SRC="$(cd "$2" && pwd)"; shift 2;;
    --rev) REV="$2"; shift 2;;
    *) die "argumento desconhecido: $1";;
  esac
done
OUT="${OUT:-${SRC}/dist}"
command -v docker >/dev/null 2>&1 || die "docker ausente no host"
[ -z "$(git -C "${SRC}" status --porcelain --untracked-files=no)" ] || die "arvore suja em ${SRC}: commite antes do build"
CODE_COMMIT="$(git -C "${SRC}" rev-parse "${REV}")"
SOURCE_DATE_EPOCH="$(git -C "${SRC}" log -1 --format=%ct "${REV}")"
mkdir -p "${OUT}"; OUT="$(cd "${OUT}" && pwd)"
WORK="$(mktemp -d)"
trap 'rm -rf "${WORK}"' EXIT
mkdir -p "${WORK}/src"
git -C "${SRC}" archive "${REV}" | tar -x -C "${WORK}/src"      # arvore LIMPA do commit: sem untracked, sem data/, sem .state/
docker run --rm \
  -e SOURCE_DATE_EPOCH="${SOURCE_DATE_EPOCH}" -e CODE_COMMIT="${CODE_COMMIT}" -e HOST_UID="$(id -u)" -e HOST_GID="$(id -g)" \
  -v "${WORK}/src:/src:ro" -v "${OUT}:/out" "${IMAGE}" bash /src/scripts/tools/build_bundle_linux.sh --inside "${VER}"
echo "bundle: ${OUT}/prevcarga-${VER}-linux-x86_64.tar.zst"
cat "${OUT}/prevcarga-${VER}-linux-x86_64.tar.zst.sha256"
