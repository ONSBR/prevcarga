#!/bin/sh
# install.sh -- instalador user-local do prevcarga (Linux x86_64; M8.3, ADR-0038, contrato de erros ADR-0040).
# O Windows usa o install.ps1 nativo (M8.4); WSL2 nao e requisito.
#
# Uso:
#   install.sh [--version <v>|latest] [--release-base <url>] [--bundle <arq> [--sha256 <h>]]
#              [--root <dir>] [--data <dir>] [--state <dir>] [--skip-platform-check] [--add-to-path]
#              [--profile <arq>] [--format text|json]
#   install.sh --rollback [--root <dir>] [--format json]
#   install.sh --uninstall --yes [--purge] [--root <dir>] [--profile <arq>] [--format json]
#
# O sha256 do bundle e OBRIGATORIO e conferido antes de qualquer extracao; nada e escrito em <raiz> antes disso.
# Raiz = ~/.local/share/prevcarga (so o DEFAULT de --root le $HOME; nenhum outro uso de variavel de ambiente).

PC_HERE="$(dirname "$(readlink -f "$0")")"
PC_CATALOG="$PC_HERE/error-catalog.tsv"   # GERADO (uniao de scripts/lib/errors/*.tsv); no bundle extraido: code/install/
PC_FORMAT=text
PC_WARNINGS=""
TMPD=""
STAGE=""

# --- erros (ADR-0040) ------------------------------------------------------------------------------------------
pc_exit_for_class() { # tabela 0-15 de errors.R (so as classes dos utilitarios): 6 bundle, 8 config (inclui uso), 9 integrity, 10 network
  case "$1" in bundle) echo 6;; config) echo 8;; integrity) echo 9;; network) echo 10;; *) echo 1;; esac; }

pc_catalog_row() { # <error.id> -> linha id\tclass\tmessage\thint; fallback embutido (mesmo texto de install.tsv) antes da extracao
  ## o catalogo GERADO (install/error-catalog.tsv) tem cabecalho id,class,exit_code,message,hint: as colunas sao lidas
  ## pelo NOME do cabecalho, nunca por posicao
  row="$(awk -F'\t' -v id="$1" 'NR==1 {for (i = 1; i <= NF; i++) c[$i] = i; next}
    $c["id"]==id {printf "%s\t%s\t%s\t%s\n", $c["id"], $c["class"], $c["message"], $c["hint"]; exit}' "$PC_CATALOG" 2>/dev/null)"
  if [ -n "$row" ]; then printf '%s\n' "$row"; return; fi
  case "$1" in
    install.download_failed)      printf 'install.download_failed\tnetwork\tfalha ao baixar o bundle\tconfira --release-base e a rede\n';;
    install.io_failed)            printf 'install.io_failed\tbundle\tfalha de escrita na raiz da instalacao\tconfira permissoes e espaco em disco da raiz\n';;
    install.manifest_invalid)     printf 'install.manifest_invalid\tbundle\tRUNTIME.json invalido ou arquivo do bundle divergente\treinstale o bundle da mesma versao; a versao atual nao foi alterada\n';;
    install.no_previous_version)  printf 'install.no_previous_version\tconfig\tnao ha versao anterior retida para voltar\tinstale a versao desejada de novo com o instalador (--version / -Version)\n';;
    install.preflight_failed)     printf 'install.preflight_failed\tbundle\tplataforma nao suportada\tLinux x86_64 glibc >= 2.35 com zstd, tar, sha256sum e cliente HTTPS; ou Windows 64-bit com PowerShell >= 5.1 e espaco livre na raiz\n';;
    install.sha_mismatch)         printf 'install.sha_mismatch\tintegrity\tsha256 do bundle nao confere\tbaixe o bundle novamente e confira o SHA256SUMS\n';;
    install.sha_missing)          printf 'install.sha_missing\tintegrity\tsha256 do bundle nao informado\tpasse --sha256 ou um SHA256SUMS ao lado do bundle\n';;
    install.state_inside_versions) printf 'install.state_inside_versions\tconfig\t--state aponta para dentro de versions/\tuse um diretorio de estado fora de <raiz>/versions\n';;
    *)                            printf 'install.usage\tconfig\targumento invalido ou ausente\tveja a ajuda do instalador (install.sh --help / install.ps1 -Help)\n';;
  esac; }

pc_json_str() { printf '%s' "$1" | sed -e 's/\\/\\\\/g' -e 's/"/\\"/g' | tr -d '\n\r\t'; }

pc_cleanup() {
  [ -n "$STAGE" ] && rm -rf "$STAGE"
  [ -n "$TMPD" ] && rm -rf "$TMPD"
  return 0
}
trap pc_cleanup EXIT
trap 'exit 130' INT TERM HUP

pc_die() { # pc_die <error.id> <onde> [details]
  id="$1"; onde="${2:-}"; details="${3:-}"
  row="$(pc_catalog_row "$id")"
  classe="$(printf '%s' "$row" | cut -f2)"; msg="$(printf '%s' "$row" | cut -f3)"; hint="$(printf '%s' "$row" | cut -f4)"
  code="$(pc_exit_for_class "$classe")"
  if [ "$PC_FORMAT" = json ]; then   # UM unico JSON em stdout, stderr vazio
    printf '{"ok":false,"exit_code":%s,"error":{"id":"%s","class":"%s","message":"%s","hint":"%s","where":"%s","details":"%s"}}\n' \
      "$code" "$id" "$classe" "$(pc_json_str "$msg")" "$(pc_json_str "$hint")" "$(pc_json_str "$onde")" "$(pc_json_str "$details")"
  else                               # texto ASCII puro em stderr
    printf 'prevcarga: erro [%s] (exit %s)\no que: %s\nonde: %s\ncomo corrigir: %s\n' "$classe" "$code" "$msg" "$onde" "$hint" >&2
  fi
  exit "$code"
}

pc_warn() {
  PC_WARNINGS="${PC_WARNINGS}${PC_WARNINGS:+|}$1"
  [ "$PC_FORMAT" = json ] || printf 'prevcarga: aviso: %s\n' "$1" >&2
}

# --- banner, etapas e mensagens: so em texto com stderr num terminal; tudo em stderr (testes/CI/json: nada) -----------
pc_ui() { # 0 = mostrar; a mesma regra vale para o banner, as etapas, as explicacoes e os proximos passos
  [ "$PC_FORMAT" = json ] && return 1
  [ -t 2 ] || return 1
  return 0
}

pc_step() { # pc_step <n> <texto>: etapa 'prevcarga: [n/6] ...'
  pc_ui || return 0
  printf 'prevcarga: [%s/6] %s\n' "$1" "$2" >&2
}

pc_note() { # pc_note <texto>: explicacao de uma linha, alinhada sob o texto da etapa
  pc_ui || return 0
  printf '                 %s\n' "$1" >&2
}

pc_banner() { # pc_banner <versao|vazio>: o banner da CLI (blueprint "Banner da CLI"; scripts/lib/banner.R) sem cor
  pc_ui || return 0
  # >>> banner: copia LITERAL de .prevcarga_banner_static() (scripts/lib/banner.R: nome + curva + eixo + identidade); paridade em tests/install >>>
  printf '%s\n' \
    '   _ __   _ __   ___ __   __  ___   __ _  _ __   __ _   __ _' \
    '  | '\''_ \ | '\''__| / _ \\ \ / / / __| / _` || '\''__| / _` | / _` |' \
    '  | |_) || |   |  __/ \ V / | (__ | (_| || |   | (_| || (_| |' \
    '  | .__/ |_|    \___|  \_/   \___| \__,_||_|    \__, | \__,_|' \
    '  |_|                                           |___/' \
    '' \
    '                      ______-'\'''\'''\'''\''---_   _----_' \
    '                  _--'\''               '\''-'\''      '\''-_' \
    '  -_            _'\''                               '\''' \
    '    '\''--_______--' \
    '  |-----------+-----------+-----------+-----------|' \
    '  0h          6h          12h         18h       24h' \
    '' \
    '  Previsao de carga do SIN - ONS' >&2
  # <<< banner <<<
  if [ -n "$1" ]; then printf '  prevcarga %s | instalador | linux-x86_64\n' "$1" >&2
  else printf '%s\n' '  prevcarga | instalador | linux-x86_64' >&2; fi
  printf '\n' >&2
}

pc_intro() { # pc_intro <versao|vazio>: banner + o que vai acontecer (antes da etapa 1)
  pc_ui || return 0
  pc_banner "$1"
  printf 'o que vai acontecer: instala sem administrador em %s\n' "$ROOT" >&2
  printf '  o estado (state_dir) e os dados nao sao alterados; a versao atual so muda no fim, depois de tudo conferido\n' >&2
}

pc_next_steps() { # proximos passos, depois da linha 'prevcarga: instalado ...' (que fica em stdout)
  pc_ui || return 0
  d="$(pc_config_get data_dir)"
  {
    printf '\nproximos passos:\n'
    # o bloco marcado no perfil vale tanto desta execucao (--add-to-path) quanto de uma instalacao anterior (upgrade sem a flag)
    if [ -f "$PROFILE" ] && grep -q '^# >>> prevcarga >>>$' "$PROFILE" && grep -qF "PATH=\"$ROOT/bin:" "$PROFILE"; then
      printf '  - %s/bin esta no PATH pelo bloco em %s, que o shell so le no login:\n' "$ROOT" "$PROFILE"
      printf '    neste terminal rode: . %s (ou faca login de novo)\n' "$PROFILE"
    else
      printf '  - ponha %s/bin no PATH: export PATH="%s/bin:$PATH" (ou reinstale com --add-to-path)\n' "$ROOT" "$ROOT"
    fi
    printf '  - rode: prevcarga doctor (confere runtime, config e deck)\n'
    if [ -n "$d" ]; then
      printf '  - deck configurado: data_dir = %s (em %s/config.yaml)\n' "$d" "$ROOT"
    else
      printf '  - aponte o deck: reinstale com --data <dir> ou ponha data_dir: <dir> em %s/config.yaml\n' "$ROOT"
    fi
  } >&2
}

pc_usage_text() {
  cat <<'EOF'
uso: install.sh [--version <v>|latest] [--release-base <url>] [--bundle <arq> [--sha256 <h>]]
                [--root <dir>] [--data <dir>] [--state <dir>] [--skip-platform-check]
                [--add-to-path] [--profile <arq>] [--format text|json]
     install.sh --rollback [--root <dir>] [--format json]
     install.sh --uninstall --yes [--purge] [--root <dir>] [--profile <arq>] [--format json]
EOF
}

# --- argumentos ------------------------------------------------------------------------------------------------
VER=""; BASE=""; BUNDLE=""; SHA=""; ROOT=""; DATA=""; STATE=""; PROFILE=""
SKIP=0; ROLLBACK=0; UNINSTALL=0; YES=0; PURGE=0; ADDPATH=0

# pre-passe: o formato vale tambem para erros de argumento
prev=""
for a in "$@"; do
  [ "$prev" = "--format" ] && [ "$a" = json ] && PC_FORMAT=json
  [ "$a" = "--format=json" ] && PC_FORMAT=json
  prev="$a"
done

while [ $# -gt 0 ]; do
  k="$1"; v=""; has_v=0
  case "$k" in --*=*) v="${k#*=}"; k="${k%%=*}"; has_v=1;; esac
  case "$k" in
    --version|--release-base|--bundle|--sha256|--root|--data|--state|--profile|--format)
      if [ "$has_v" = 0 ]; then
        [ $# -ge 2 ] || pc_die install.usage "$k" "falta o valor"
        v="$2"; shift
      fi
      [ -n "$v" ] || pc_die install.usage "$k" "valor vazio"
      case "$k" in
        --version) VER="$v";; --release-base) BASE="$v";; --bundle) BUNDLE="$v";; --sha256) SHA="$v";;
        --root) ROOT="$v";; --data) DATA="$v";; --state) STATE="$v";; --profile) PROFILE="$v";;
        --format) case "$v" in json|text) PC_FORMAT="$v";; *) pc_die install.usage "--format" "use text ou json";; esac;;
      esac;;
    --skip-platform-check) SKIP=1;;
    --rollback) ROLLBACK=1;;
    --uninstall) UNINSTALL=1;;
    --yes) YES=1;;
    --purge) PURGE=1;;
    --add-to-path) ADDPATH=1;;
    -h|--help) pc_usage_text; exit 0;;
    *) pc_die install.usage "$1" "argumento desconhecido";;
  esac
  shift
done

[ "$ROLLBACK" = 1 ] && [ "$UNINSTALL" = 1 ] && pc_die install.usage "--rollback --uninstall" "opcoes exclusivas"
[ "$PURGE" = 1 ] && [ "$UNINSTALL" = 0 ] && pc_die install.usage "--purge" "so vale com --uninstall"
[ "$YES" = 1 ] && [ "$UNINSTALL" = 0 ] && pc_die install.usage "--yes" "so vale com --uninstall"
[ -n "$SHA" ] && [ -z "$BUNDLE" ] && pc_die install.usage "--sha256" "so vale com --bundle"
[ -n "$BUNDLE" ] && [ -n "$BASE" ] && pc_die install.usage "--bundle --release-base" "opcoes exclusivas"

# so o DEFAULT da raiz le o ambiente
[ -n "$ROOT" ] || { [ -n "${HOME:-}" ] || pc_die install.usage "--root" "sem HOME: informe --root"; ROOT="$HOME/.local/share/prevcarga"; }

pc_abs() { case "$1" in /*) printf '%s\n' "$1";; *) printf '%s/%s\n' "$(pwd)" "$1";; esac; }
ROOT="$(pc_abs "$ROOT")"; ROOT="${ROOT%/}"
[ -n "$ROOT" ] || pc_die install.usage "--root" "raiz nao pode ser /"
[ -z "$STATE" ] || STATE="$(pc_abs "$STATE")"
[ -z "$DATA" ] || DATA="$(pc_abs "$DATA")"
[ -z "$BUNDLE" ] || BUNDLE="$(pc_abs "$BUNDLE")"
[ -n "$PROFILE" ] || PROFILE="${HOME:-/nonexistent}/.profile"
PROFILE="$(pc_abs "$PROFILE")"

if [ -n "$STATE" ]; then
  case "$STATE" in "$ROOT/versions"|"$ROOT/versions/"*) pc_die install.state_inside_versions "$STATE";; esac
fi

# --- utilitarios -----------------------------------------------------------------------------------------------
pc_swap_file() { # <arquivo> <conteudo>: tempfile no mesmo diretorio + mv -f (nunca symlink)
  t="$(mktemp "$(dirname "$1")/.tmp.XXXXXX")" || pc_die install.io_failed "$(dirname "$1")"
  printf '%s\n' "$2" > "$t" && chmod 0644 "$t" && mv -f "$t" "$1" || { rm -f "$t"; pc_die install.io_failed "$1"; }
}

pc_read_file() { head -n 1 "$1" 2>/dev/null | tr -d '\r'; }

pc_swap_current() { # $1 = versao ja validada e renomeada para versions/<v>
  [ -d "$ROOT/versions/$1" ] || pc_die install.manifest_invalid "$ROOT/versions/$1"
  pc_swap_file "$ROOT/current" "$1"
}

pc_config_get() { # <chave>: valor de uma chave de topo do config.yaml existente (sem aspas)
  [ -f "$ROOT/config.yaml" ] || return 0
  awk -v k="$1" 'index($0, k ":")==1 { v=substr($0, length(k)+2); sub(/^ +/, "", v); sub(/ +#.*$/, "", v);
                   if (v ~ /^\047.*\047$/) { v=substr(v,2,length(v)-2); gsub(/\047\047/, "\047", v) }
                   print v; exit }' "$ROOT/config.yaml"
}

pc_yaml_q() { printf "'%s'" "$(printf '%s' "$1" | sed "s/'/''/g")"; }

pc_write_config() { # config.yaml da instalacao (schema v1 do M1.4a); preserva as chaves ja existentes num upgrade
  d="${DATA:-$(pc_config_get data_dir)}"
  s="${STATE:-$(pc_config_get state_dir)}"; [ -n "$s" ] || s="$ROOT/state"
  t="$(mktemp "$ROOT/.config.XXXXXX")" || pc_die install.io_failed "$ROOT"
  {
    if [ -f "$ROOT/config.yaml" ]; then
      grep -v -e '^data_dir:' -e '^state_dir:' "$ROOT/config.yaml"
      grep -q '^schema_version:' "$ROOT/config.yaml" || echo 'schema_version: 1'
    else
      printf '%s\n' '# config.yaml da instalacao (gerado por install.sh; schema v1). Precedencia: flag da CLI > este arquivo > default.' \
        'schema_version: 1' 'format: text' 'color: auto'
    fi
    [ -z "$d" ] || printf 'data_dir: %s\n' "$(pc_yaml_q "$d")"
    printf 'state_dir: %s\n' "$(pc_yaml_q "$s")"
  } > "$t" && chmod 0644 "$t" && mv -f "$t" "$ROOT/config.yaml" || { rm -f "$t"; pc_die install.io_failed "$ROOT/config.yaml"; }
  STATE_EFFECTIVE="$s"
}

pc_profile_block_strip() { # remove o bloco marcado de um arquivo de perfil (stdout)
  awk '/^# >>> prevcarga >>>$/ {skip=1; next} /^# <<< prevcarga <<<$/ {skip=0; next} !skip {print}' "$1"
}

pc_add_to_path() { # bloco marcado e idempotente no perfil do usuario
  mkdir -p "$(dirname "$PROFILE")" || pc_die install.io_failed "$PROFILE"
  t="$(mktemp "$(dirname "$PROFILE")/.profile.XXXXXX")" || pc_die install.io_failed "$PROFILE"
  { [ ! -f "$PROFILE" ] || pc_profile_block_strip "$PROFILE"
    printf '# >>> prevcarga >>>\nPATH="%s/bin:$PATH"; export PATH\n# <<< prevcarga <<<\n' "$ROOT"
  } > "$t" && mv -f "$t" "$PROFILE" || { rm -f "$t"; pc_die install.io_failed "$PROFILE"; }
}

pc_installed_versions() { # versoes (nao stage) em versions/
  [ -d "$ROOT/versions" ] || return 0
  for d in "$ROOT"/versions/*; do [ -d "$d" ] && basename "$d"; done
}

pc_emit_ok() { # pc_emit_ok <acao> <json-extra-sem-chaves-externas> <linhas-texto>
  if [ "$PC_FORMAT" = json ]; then
    w=""
    if [ -n "$PC_WARNINGS" ]; then
      w=',"warnings":['; first=1
      oldifs="$IFS"; IFS='|'
      for x in $PC_WARNINGS; do [ "$first" = 1 ] || w="$w,"; first=0; w="$w\"$(pc_json_str "$x")\""; done
      IFS="$oldifs"; w="$w]"
    fi
    printf '{"ok":true,"exit_code":0,"result":{"action":"%s","root":"%s"%s%s}}\n' "$1" "$(pc_json_str "$ROOT")" "${2:+,$2}" "$w"
  else
    printf '%s\n' "$3"
  fi
}

# --- ciclo de vida: rollback / uninstall -------------------------------------------------------------------------
if [ "$ROLLBACK" = 1 ]; then
  cur="$(pc_read_file "$ROOT/current")"
  prv="$(pc_read_file "$ROOT/previous")"
  if [ -z "$prv" ] || [ "$prv" = "$cur" ] || [ ! -d "$ROOT/versions/$prv" ]; then
    pc_die install.no_previous_version "$ROOT" "current=${cur:-nenhum}"
  fi
  pc_swap_current "$prv"
  rm -f "$ROOT/previous"
  pc_emit_ok rollback "\"version\":\"$(pc_json_str "$prv")\",\"from\":\"$(pc_json_str "$cur")\"" "prevcarga: rollback de $cur para $prv em $ROOT"
  exit 0
fi

if [ "$UNINSTALL" = 1 ]; then
  [ "$YES" = 1 ] || pc_die install.usage "--uninstall" "confirme com --yes"
  st="$(pc_config_get state_dir)"; [ -n "$st" ] || st="$ROOT/state"
  [ -z "$STATE" ] || st="$STATE"
  [ -d "$ROOT" ] || { pc_emit_ok uninstall '"removed":false' "prevcarga: nada a remover em $ROOT"; exit 0; }
  rm -rf "$ROOT/versions" "$ROOT/bin" "$ROOT/current" "$ROOT/previous" "$ROOT/config.yaml" || pc_die install.io_failed "$ROOT"
  if [ -f "$PROFILE" ] && grep -q '^# >>> prevcarga >>>$' "$PROFILE"; then
    t="$(mktemp "$(dirname "$PROFILE")/.profile.XXXXXX")" || pc_die install.io_failed "$PROFILE"
    pc_profile_block_strip "$PROFILE" > "$t" && mv -f "$t" "$PROFILE" || { rm -f "$t"; pc_die install.io_failed "$PROFILE"; }
  fi
  purged=false
  if [ "$PURGE" = 1 ]; then
    case "$st" in ""|/|"${HOME:-/nonexistent}") pc_die install.usage "--purge" "estado recusado: $st";; esac
    rm -rf "$st" || pc_die install.io_failed "$st"
    purged=true
  fi
  rmdir "$ROOT" 2>/dev/null
  pc_emit_ok uninstall "\"removed\":true,\"purged\":$purged" "prevcarga: desinstalado de $ROOT (estado $( [ "$purged" = true ] && echo removido || echo preservado ))"
  exit 0
fi

# --- instalacao ---------------------------------------------------------------------------------------------------
pc_preflight() {
  fail=""
  arch="$(uname -m 2>/dev/null)"
  [ "$arch" = x86_64 ] || fail="$fail arquitetura=$arch"
  if ldd --version 2>&1 | grep -qi musl; then fail="$fail libc=musl"; fi
  g="$(getconf GNU_LIBC_VERSION 2>/dev/null | awk '{print $2}')"
  if [ -z "$g" ]; then fail="$fail glibc=ausente"
  elif [ "$(printf '%s\n2.35\n' "$g" | sort -V | head -n 1)" != "2.35" ]; then fail="$fail glibc=$g"; fi
  for c in zstd tar sha256sum awk; do command -v "$c" >/dev/null 2>&1 || fail="$fail $c=ausente"; done
  if [ -z "$BUNDLE" ] && ! command -v curl >/dev/null 2>&1 && ! command -v wget >/dev/null 2>&1; then fail="$fail curl/wget=ausente"; fi
  if [ -n "$fail" ]; then
    if [ "$SKIP" = 1 ]; then pc_warn "preflight ignorado (--skip-platform-check):$fail"
    else pc_die install.preflight_failed "plataforma" "${fail# }"; fi
  fi
}

pc_get() { # <url> <destino>
  if command -v curl >/dev/null 2>&1; then curl -fsSL --proto '=https' -o "$2" "$1" 2>/dev/null
  else wget -q -O "$2" "$1" 2>/dev/null; fi
}

pc_sha_from_sums() { awk -v f="$2" '{ n=$2; sub(/^\*/, "", n); if (n==f) { print $1; exit } }' "$1" 2>/dev/null; }

# Dois layouts de release, escolhidos pela --release-base:
#   GitHub Releases (https://github.com/<org>/<repo>): assets da release de tag v<v>; a mais recente pelo
#     redirect /releases/latest/download/latest.txt (latest.txt e asset de toda release);
#   bucket (qualquer outra base): <base>/releases/<v>/ e <base>/releases/latest.txt.
pc_is_github() { case "$BASE" in https://github.com/*/*/*) return 1;; https://github.com/?*/?*) return 0;; *) return 1;; esac; }
pc_url_latest() { if pc_is_github; then printf '%s/releases/latest/download/latest.txt' "$BASE"; else printf '%s/releases/latest.txt' "$BASE"; fi; }
pc_url_release() { if pc_is_github; then printf '%s/releases/download/v%s' "$BASE" "$1"; else printf '%s/releases/%s' "$BASE" "$1"; fi; }

pc_fetch_verified() { # define TARBALL (fora de <raiz>) so se o sha256 conferir
  TMPD="$(mktemp -d)" || pc_die install.preflight_failed mktemp
  if [ -n "$BUNDLE" ]; then
    TARBALL="$BUNDLE"
    [ -f "$TARBALL" ] || pc_die install.usage "--bundle" "arquivo inexistente: $TARBALL"
    [ -n "$SHA" ] || SHA="$(pc_sha_from_sums "$(dirname "$TARBALL")/SHA256SUMS" "$(basename "$TARBALL")")"
    [ -n "$SHA" ] || pc_die install.sha_missing "$TARBALL"
    bv="${VER:-$(basename "$TARBALL" | sed -n 's/^prevcarga-\(.*\)-linux-x86_64\.tar\.zst$/\1/p')}"
    pc_intro "$bv"
    pc_step 1 "bundle local $(basename "$TARBALL") (sem download)"
  else
    [ -n "$BASE" ] || BASE="https://github.com/onsbr/prevcarga"   # GitHub Releases; o bucket so por --release-base
    BASE="${BASE%/}"
    if [ "$VER" = latest ] || [ -z "$VER" ]; then
      u="$(pc_url_latest)"
      pc_get "$u" "$TMPD/latest.txt" || pc_die install.download_failed "$u"
      VER="$(pc_read_file "$TMPD/latest.txt")"
      [ -n "$VER" ] || pc_die install.download_failed "$u" "versao vazia"
    fi
    nome="prevcarga-$VER-linux-x86_64.tar.zst"
    TARBALL="$TMPD/$nome"
    u="$(pc_url_release "$VER")"
    pc_intro "$VER"
    pc_step 1 "baixando $nome (pode levar alguns minutos)"
    pc_get "$u/$nome" "$TARBALL" || pc_die install.download_failed "$u/$nome"
    pc_get "$u/SHA256SUMS" "$TMPD/SHA256SUMS" || pc_die install.download_failed "$u/SHA256SUMS"
    SHA="$(pc_sha_from_sums "$TMPD/SHA256SUMS" "$nome")"
    [ -n "$SHA" ] || pc_die install.sha_missing "$u/SHA256SUMS" "$nome ausente do SHA256SUMS"
  fi
  pc_step 2 "conferindo sha256 do bundle"
  pc_note "o arquivo inteiro e conferido antes de qualquer escrita em $ROOT"
  got="$(sha256sum "$TARBALL" | cut -d' ' -f1)"
  [ "$got" = "$SHA" ] || pc_die install.sha_mismatch "$TARBALL" "esperado=$SHA obtido=$got"
}

pc_preflight
pc_fetch_verified

if [ -z "$VER" ]; then  # --bundle sem --version: prevcarga-<v>-linux-x86_64.tar.zst
  VER="$(basename "$TARBALL" | sed -n 's/^prevcarga-\(.*\)-linux-x86_64\.tar\.zst$/\1/p')"
  [ -n "$VER" ] || pc_die install.usage "--bundle" "nao foi possivel inferir a versao; use --version"
fi
case "$VER" in */*|.*|*[!A-Za-z0-9._+-]*) pc_die install.usage "--version" "versao invalida: $VER";; esac

# a partir daqui a raiz pode ser escrita
mkdir -p "$ROOT/versions" || pc_die install.io_failed "$ROOT"
STAGE="$ROOT/versions/.stage-$VER"
rm -rf "$STAGE"
mkdir -p "$STAGE" || pc_die install.io_failed "$STAGE"
pc_step 3 "extraindo o bundle"
pc_note "numa area temporaria (versions/.stage-$VER); a versao atual so muda na etapa 5"
tar --zstd -xf "$TARBALL" -C "$STAGE" --strip-components=1 2>/dev/null || pc_die install.manifest_invalid "$TARBALL" "falha ao extrair"
[ -f "$STAGE/code/install/error-catalog.tsv" ] && PC_CATALOG="$STAGE/code/install/error-catalog.tsv"

# valida TODO files[] do RUNTIME.json com o Rscript do proprio stage (R_HOME do host nunca vaza)
[ -x "$STAGE/runtime/bin/Rscript" ] && [ -f "$STAGE/RUNTIME.json" ] || pc_die install.manifest_invalid "$STAGE/RUNTIME.json" "bundle incompleto"
nf="$(grep -o '"path" *:' "$STAGE/RUNTIME.json" 2>/dev/null | wc -l | tr -d ' ')"
case "$nf" in ""|0) nf="os";; esac
pc_step 4 "validando $nf arquivos do RUNTIME.json (pode levar alguns minutos)"
pc_note "confere o sha256 de cada arquivo do bundle antes de ativar; se algo divergir, nada muda"
env -u R_HOME "$STAGE/runtime/bin/Rscript" --vanilla -e \
  'r <- commandArgs(TRUE)[1]; lib <- file.path(r, "code/scripts/lib"); options(prevcarga.lib = lib); source(file.path(lib, "errors.R")); source(file.path(lib, "runtime.R")); prevcarga_runtime_validate(r, strict = TRUE)' \
  "$STAGE" >/dev/null 2>&1 || pc_die install.manifest_invalid "$STAGE/RUNTIME.json"

pc_step 5 "ativando a versao $VER"
prev_cur="$(pc_read_file "$ROOT/current")"
if [ -d "$ROOT/versions/$VER" ]; then   # reinstalacao da mesma versao: troca o diretorio
  rm -rf "$ROOT/versions/.old-$VER"
  mv "$ROOT/versions/$VER" "$ROOT/versions/.old-$VER" || pc_die install.io_failed "$ROOT/versions/$VER"
fi
mv "$STAGE" "$ROOT/versions/$VER" || pc_die install.io_failed "$ROOT/versions/$VER"
STAGE=""
mkdir -p "$ROOT/bin" || pc_die install.io_failed "$ROOT/bin"
LAUNCHER="$ROOT/versions/$VER/code/install/prevcarga"
[ -f "$LAUNCHER" ] || LAUNCHER="$PC_HERE/prevcarga"   # bundles antigos: lancador ao lado do install.sh
[ -f "$LAUNCHER" ] || pc_die install.manifest_invalid "$LAUNCHER" "lancador ausente"
cp "$LAUNCHER" "$ROOT/bin/.prevcarga.new" && chmod 0755 "$ROOT/bin/.prevcarga.new" \
  && mv -f "$ROOT/bin/.prevcarga.new" "$ROOT/bin/prevcarga" || pc_die install.io_failed "$ROOT/bin/prevcarga"
# prevcarga-sync (M8.6): utilitario de rede FORA do pacote R, ao lado do lancador, com o catalogo GERADO que ele le (<R>/bin/error-catalog.tsv)
SYNC="$ROOT/versions/$VER/code/install/prevcarga-sync.sh"
[ -f "$SYNC" ] || SYNC="$PC_HERE/prevcarga-sync.sh"   # bundles antigos: utilitario ao lado do install.sh
if [ -f "$SYNC" ]; then
  cp "$SYNC" "$ROOT/bin/.prevcarga-sync.new" && chmod 0755 "$ROOT/bin/.prevcarga-sync.new" \
    && mv -f "$ROOT/bin/.prevcarga-sync.new" "$ROOT/bin/prevcarga-sync" || pc_die install.io_failed "$ROOT/bin/prevcarga-sync"
else
  pc_warn "prevcarga-sync ausente do bundle: o utilitario de sincronizacao nao foi instalado"
fi
CAT="$ROOT/versions/$VER/code/install/error-catalog.tsv"
[ -f "$CAT" ] || CAT="$PC_CATALOG"
if [ -f "$CAT" ]; then
  cp "$CAT" "$ROOT/bin/.error-catalog.new" && mv -f "$ROOT/bin/.error-catalog.new" "$ROOT/bin/error-catalog.tsv" || pc_die install.io_failed "$ROOT/bin/error-catalog.tsv"
fi
pc_swap_current "$VER"
rm -rf "$ROOT/versions/.old-$VER"

# previous = versao que estava em current (se outra); retencao N=2 = current + previous
if [ -n "$prev_cur" ] && [ "$prev_cur" != "$VER" ] && [ -d "$ROOT/versions/$prev_cur" ]; then
  pc_swap_file "$ROOT/previous" "$prev_cur"
else
  prev_cur="$(pc_read_file "$ROOT/previous")"
  [ -d "$ROOT/versions/${prev_cur:-/}" ] && [ "$prev_cur" != "$VER" ] || { rm -f "$ROOT/previous"; prev_cur=""; }
fi
for v in $(pc_installed_versions); do
  [ "$v" = "$VER" ] && continue
  [ -n "$prev_cur" ] && [ "$v" = "$prev_cur" ] && continue
  rm -rf "$ROOT/versions/$v"
done
[ -z "$prev_cur" ] || pc_note "a versao anterior ($prev_cur) fica guardada: install.sh --rollback volta para ela"

pc_write_config
mkdir -p "$STATE_EFFECTIVE" || pc_die install.io_failed "$STATE_EFFECTIVE"
[ "$ADDPATH" = 1 ] && pc_add_to_path

pc_step 6 "conferindo a instalacao (doctor)"
pc_note "um aviso aqui nao desfaz a instalacao; 'prevcarga doctor' mostra o detalhe"
doctor="$(env -u R_HOME "$ROOT/bin/prevcarga" doctor --format json 2>/dev/null)"
case "$doctor" in "{"*) ;; *) doctor="null";; esac
doctor="$(printf '%s' "$doctor" | tr -d '\n')"
pc_emit_ok install "\"version\":\"$(pc_json_str "$VER")\",\"current\":\"$(pc_json_str "$VER")\",\"previous\":\"$(pc_json_str "$prev_cur")\",\"state_dir\":\"$(pc_json_str "$STATE_EFFECTIVE")\",\"doctor\":$doctor" \
  "prevcarga: instalado $VER em $ROOT (bin: $ROOT/bin/prevcarga)"
pc_next_steps
exit 0
