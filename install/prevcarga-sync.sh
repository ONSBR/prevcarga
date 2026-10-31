#!/bin/sh
# prevcarga-sync.sh -- sincroniza os artefatos e as inferencias OFICIAIS do bucket publico para o registry local
# (M8.6; ADR-0039, contrato de erros ADR-0040). Linux; o Windows usa o par nativo prevcarga-sync.ps1/.cmd (WSL2 nao e requisito).
# FORA do pacote R: junto com o install.sh e o upload, e um dos unicos utilitarios que falam rede; so LEITURA HTTPS(HTTP),
# sem credencial, sem variavel de ambiente (o pacote R nunca fala rede). Instalado como <R>/bin/prevcarga-sync pelo install.sh.
#
# Uso:
#   prevcarga-sync --what artifacts|forecasts [--release-base <url>] [--state <dir>] [--model <m>]... [--area <a>]...
#                  [--origin <AAAA-MM-DD>] [--format text|json]
#   artifacts : modelos das duas classes (artifacts/<modelo>/[<variante>/]<area>/), pesos da combina (artifacts/combina/<area>/)
#               e perfis k-hat (artifacts/reconcile/<ss>/), no mesmo layout do bucket, em <state>/artifacts/.
#               O conjunto oficial vem do CLI instalado ao lado (`prevcarga models|areas --format json`); o bucket NUNCA e listado.
#               --model filtra os modelos (combina/reconcile sempre entram); --area filtra pelo ultimo segmento do prefixo
#               (folha para modelos e combina, subsistema para reconcile).
#   forecasts : inferencias oficiais forecasts/<O>/<available_until>/<versao>/ da origem --origin, em <state>/forecasts/.
#               As chaves vem de forecasts/<O>/latest.txt (uma `<available_until>/<versao>` por linha; ponteiro mutavel).
#
# Atomicidade: tudo e baixado e conferido (sha256 contra o SHA256SUMS da chave) num staging `.tmp-*` do registry ANTES de
# qualquer rename; so entao cada v_<hash>/ vira publico (rename) e so DEPOIS o latest.txt local e regravado (tempfile + mv -f).
# Qualquer falha antes do rename deixa <state> byte-identico ao de antes (o trap remove o staging). Um v_<hash> local ja presente e
# CONFERIDO, nunca sobrescrito: sha divergente => sync.sha_mismatch (exit 9). Rede (curl/wget) => sync.network (exit 10).
# Saida: texto ASCII (erro em stderr) ou, com --format json, UM documento em stdout e stderr vazio. Exits: 0, 6, 8, 9, 10.

PC_HERE="$(dirname "$(readlink -f "$0")")"
PC_CATALOG="$PC_HERE/error-catalog.tsv"   # GERADO (uniao de scripts/lib/errors/*.tsv), copiado ao lado pelo install.sh
PC_CLI="$PC_HERE/prevcarga"
PC_VERSION="1.0.0"
PC_FORMAT=text
TMPD=""
STAGE=""
MADE_S=0
MADE_W=0
WHAT=""
S=""

# --- erros (ADR-0040) ------------------------------------------------------------------------------------------
pc_exit_for_class() { # tabela 0-15 de errors.R (so as classes dos utilitarios): 6 bundle, 8 config (inclui uso), 9 integrity, 10 network
  case "$1" in bundle) echo 6;; config) echo 8;; integrity) echo 9;; network) echo 10;; *) echo 1;; esac; }

pc_catalog_row() { # <error.id> -> id\tclass\tmessage\thint; colunas do catalogo GERADO lidas pelo NOME; fallback embutido (mesmo texto de sync.tsv)
  row="$(awk -F'\t' -v id="$1" 'NR==1 {for (i = 1; i <= NF; i++) c[$i] = i; next}
    $c["id"]==id {printf "%s\t%s\t%s\t%s\n", $c["id"], $c["class"], $c["message"], $c["hint"]; exit}' "$PC_CATALOG" 2>/dev/null)"
  if [ -n "$row" ]; then printf '%s\n' "$row"; return; fi
  case "$1" in
    sync.io_failed)      printf 'sync.io_failed\tbundle\tfalha de escrita ou utilitario ausente no registry local\tconfira permissoes e espaco em disco do --state e se o prevcarga instalado esta ao lado do prevcarga-sync\n';;
    sync.network)        printf 'sync.network\tnetwork\tfalha de rede ao baixar do release-base\tconfira --release-base e a conexao e repita; nada foi gravado no registry local\n';;
    sync.sha_mismatch)   printf 'sync.sha_mismatch\tintegrity\tsha256 de um objeto nao confere com o SHA256SUMS publicado\tbaixe de novo; se persistir o publicado esta corrompido; nada foi gravado no registry local\n';;
    *)                   printf 'sync.usage\tconfig\targumento invalido ou ausente do prevcarga-sync\tveja prevcarga-sync --help (-Help no PowerShell)\n';;
  esac; }

pc_json_str() { printf '%s' "$1" | sed -e 's/\\/\\\\/g' -e 's/"/\\"/g' | tr -d '\n\r\t'; }

pc_cleanup() {
  [ -n "$STAGE" ] && rm -rf "$STAGE"
  [ -n "$TMPD" ] && rm -rf "$TMPD"
  # diretorios que ESTA execucao criou e ficaram vazios (falha antes de qualquer rename): a arvore volta a ser a de antes
  [ "$MADE_W" = 1 ] && rmdir "$S/$WHAT" 2>/dev/null
  [ "$MADE_S" = 1 ] && rmdir "$S" 2>/dev/null
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
    printf '{"ok":false,"exit_code":%s,"verb":"prevcarga-sync","error":{"id":"%s","class":"%s","message":"%s","hint":"%s","where":"%s","details":"%s"}}\n' \
      "$code" "$id" "$classe" "$(pc_json_str "$msg")" "$(pc_json_str "$hint")" "$(pc_json_str "$onde")" "$(pc_json_str "$details")"
  else                               # texto ASCII puro em stderr
    printf 'prevcarga: erro [%s] (exit %s)\no que: %s\nonde: %s\ncomo corrigir: %s\n' "$classe" "$code" "$msg" "$onde" "$hint" >&2
  fi
  exit "$code"
}

pc_usage_text() {
  cat <<'EOF'
uso: prevcarga-sync --what artifacts|forecasts [--release-base <url>] [--state <dir>]
                    [--model <m>]... [--area <a>]... [--origin <AAAA-MM-DD>] [--format text|json]
EOF
}

# --- argumentos ------------------------------------------------------------------------------------------------
BASE=""; MODELS=""; AREAS=""; ORIGIN=""

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
    --what|--release-base|--state|--model|--area|--origin|--format)
      if [ "$has_v" = 0 ]; then
        [ $# -ge 2 ] || pc_die sync.usage "$k" "falta o valor"
        v="$2"; shift
      fi
      [ -n "$v" ] || pc_die sync.usage "$k" "valor vazio"
      case "$k" in
        --what) case "$v" in artifacts|forecasts) WHAT="$v";; *) pc_die sync.usage "--what" "use artifacts ou forecasts";; esac;;
        --release-base) BASE="$v";; --state) S="$v";; --origin) ORIGIN="$v";;
        --model) MODELS="$MODELS $(printf '%s' "$v" | tr ',' ' ')";;
        --area) AREAS="$AREAS $(printf '%s' "$v" | tr ',' ' ')";;
        --format) case "$v" in json|text) PC_FORMAT="$v";; *) pc_die sync.usage "--format" "use text ou json";; esac;;
      esac;;
    -h|--help) pc_usage_text; exit 0;;
    *) pc_die sync.usage "$1" "argumento desconhecido";;
  esac
  shift
done

[ -n "$WHAT" ] || pc_die sync.usage "--what" "informe artifacts ou forecasts"
if [ "$WHAT" = forecasts ]; then
  [ -n "$ORIGIN" ] || pc_die sync.usage "--origin" "informe --origin AAAA-MM-DD com --what forecasts"
  [ -z "$MODELS$AREAS" ] || pc_die sync.usage "--model --area" "so valem com --what artifacts"
  case "$ORIGIN" in [0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]) ;; *) pc_die sync.usage "--origin" "formato AAAA-MM-DD: $ORIGIN";; esac
  [ "$(date -u -d "$ORIGIN" +%Y-%m-%d 2>/dev/null)" = "$ORIGIN" ] || pc_die sync.usage "--origin" "data invalida: $ORIGIN"
else
  [ -z "$ORIGIN" ] || pc_die sync.usage "--origin" "so vale com --what forecasts"
fi

[ -n "$BASE" ] || BASE="https://prevcarga.s3.amazonaws.com"   # [TBD: ADR-0039]; o mesmo default do install.sh
BASE="${BASE%/}"
case "$BASE" in http://?*|https://?*) ;; *) pc_die sync.usage "--release-base" "informe uma URL http(s): $BASE";; esac

pc_config_get() { # <chave>: valor de uma chave de topo do <R>/config.yaml da instalacao (sem aspas)
  f="$PC_HERE/../config.yaml"
  [ -f "$f" ] || return 0
  awk -v k="$1" 'index($0, k ":")==1 { v=substr($0, length(k)+2); sub(/^ +/, "", v); sub(/ +#.*$/, "", v);
                   if (v ~ /^\047.*\047$/) { v=substr(v,2,length(v)-2); gsub(/\047\047/, "\047", v) }
                   print v; exit }' "$f"
}
[ -n "$S" ] || S="$(pc_config_get state_dir)"
[ -n "$S" ] || { [ -f "$PC_HERE/../config.yaml" ] && S="$PC_HERE/../state"; }
[ -n "$S" ] || pc_die sync.usage "--state" "informe --state <dir>"
case "$S" in /*) ;; *) S="$(pwd)/$S";; esac
S="${S%/}"
[ -n "$S" ] || pc_die sync.usage "--state" "estado nao pode ser /"

# --- transporte ------------------------------------------------------------------------------------------------
pc_get() { # <url> <destino>: 0 ok; 44 = HTTP 404 (objeto ausente); 1 = qualquer outra falha (conexao cortada, 5xx, DNS)
  if command -v curl >/dev/null 2>&1; then
    code="$(curl -s --proto '=https,http' --connect-timeout 20 -o "$2" -w '%{http_code}' "$1" 2>/dev/null)" || return 1
    case "$code" in 200) return 0;; 404) return 44;; *) return 1;; esac
  elif command -v wget >/dev/null 2>&1; then
    wget -q -O "$2" "$1" 2>/dev/null && return 0
    return 1
  fi
  return 1
}

pc_sha() { sha256sum "$1" | cut -d' ' -f1; }

pc_list_json() { # linhas -> ["a","b"]
  awk 'BEGIN { printf "[" } NF { gsub(/\\/, "\\\\"); gsub(/"/, "\\\""); printf "%s\"%s\"", (n++ ? "," : ""), $0 } END { printf "]" }'
}

# --- enumeracao do conjunto oficial (so artifacts; o bucket nunca e listado) -----------------------------------------
pc_enumerate() { # grava em $TMPD/prefixos os prefixos relativos (artifacts/...) ordenados, ja filtrados por --model/--area (nao e chamada em $(...): pc_die sai do shell principal)
  [ -x "$PC_CLI" ] || pc_die sync.io_failed "$PC_CLI" "prevcarga instalado ao lado nao encontrado"
  MJ="$("$PC_CLI" models --format json 2>/dev/null)"; AJ="$("$PC_CLI" areas --format json 2>/dev/null)"
  case "$MJ" in "{"*) ;; *) pc_die sync.io_failed "$PC_CLI models" "falha ao enumerar os modelos";; esac
  case "$AJ" in "{"*) ;; *) pc_die sync.io_failed "$PC_CLI areas" "falha ao enumerar as areas";; esac
  # cada membro: {"sigla":..,"mdir":"<m>","exogena":null|"<x>",...} -> `<mdir> <exogena|vazio>`
  mem="$(printf '%s' "$MJ" | sed 's/{"sigla"/\
&/g' | sed -n -E 's/^\{"sigla":"[^"]*","mdir":"([^"]*)","exogena":(null|"([^"]*)").*/\1 \3/p')"
  leaves="$(printf '%s' "$AJ" | grep -o '"areas":\[[^]]*\]' | sed -e 's/"areas":\[//' -e 's/\]//' -e 's/"//g' | tr ',' '\n')"
  order="$(printf '%s' "$AJ" | sed -n 's/.*"order":\[\([^]]*\)\].*/\1/p' | tr -d '"' | tr ',' '\n')"
  [ -n "$mem" ] && [ -n "$leaves" ] && [ -n "$order" ] || pc_die sync.io_failed "$PC_CLI" "enumeracao vazia (models/areas)"
  for m in $MODELS; do   # filtro que nao casa com nada do conjunto oficial e erro de uso (como o release-pack)
    printf '%s\n' "$mem" | awk -v m="$m" '$1==m {f=1} END {exit !f}' || pc_die sync.usage "--model" "modelo desconhecido: $m"
  done
  for a in $AREAS; do
    printf '%s\n%s\n' "$leaves" "$order" | grep -qx -- "$a" || pc_die sync.usage "--area" "area desconhecida: $a"
  done
  {
    printf '%s\n' "$mem" | while read -r mdir exo; do
      [ -n "$mdir" ] || continue
      if [ -n "$MODELS" ]; then case " $MODELS " in *" $mdir "*) ;; *) continue;; esac; fi
      for a in $leaves; do printf 'artifacts/%s%s/%s\n' "$mdir" "${exo:+/$exo}" "$a"; done
    done
    for a in $leaves; do printf 'artifacts/combina/%s\n' "$a"; done
    for s in $order; do printf 'artifacts/reconcile/%s\n' "$s"; done
  } | while read -r p; do
    if [ -n "$AREAS" ]; then case " $AREAS " in *" ${p##*/} "*) ;; *) continue;; esac; fi
    printf '%s\n' "$p"
  done | LC_ALL=C sort -u > "$TMPD/prefixos"
}

# --- protocolo por chave ------------------------------------------------------------------------------------------
BAIXADOS=""; PULADOS=""; TROCADOS=""; AUSENTES=""; PENDING=""; POINTERS=""
TMPD="$(mktemp -d)" || pc_die sync.io_failed "mktemp" "sem diretorio temporario"

pc_stage_init() { # staging unico do registry (`.tmp-*`, ADR-0030), no MESMO volume dos destinos: criado so ao baixar o 1o objeto
  [ -z "$STAGE" ] || return 0
  [ -d "$S" ] || MADE_S=1
  [ -d "$S/$WHAT" ] || MADE_W=1
  mkdir -p "$S/$WHAT" || pc_die sync.io_failed "$S/$WHAT"
  STAGE="$(mktemp -d "$S/$WHAT/.tmp-XXXXXX")" || pc_die sync.io_failed "$S/$WHAT"
}

pc_safe_rel() { # rejeita caminho absoluto, '..' e vazio vindos do SHA256SUMS publicado
  case "$1" in ""|/*|..|../*|*/..|*/../*) return 1;; esac
  return 0
}

pc_verify_local() { # <dir local> <sums remoto>: 0 se todos os arquivos listados e o SHA256SUMS local conferem
  [ -f "$1/SHA256SUMS" ] && cmp -s "$1/SHA256SUMS" "$2" || return 1
  (cd "$1" && sha256sum -c --quiet "$2" >/dev/null 2>&1)
}

pc_sync_key() { # <chave relativa a <state>, ex.: artifacts/lgbm/SECO/v_ab12>: baixa+confere no staging, ou confere o local
  key="$1"; url="$BASE/$key"
  pc_get "$url/SHA256SUMS" "$TMPD/sums"; rc=$?
  [ $rc = 0 ] || pc_die sync.network "$url/SHA256SUMS"
  if [ -d "$S/$key" ]; then
    pc_verify_local "$S/$key" "$TMPD/sums" || pc_die sync.sha_mismatch "$S/$key" "versao local divergente do publicado; nao foi sobrescrita"
    PULADOS="$PULADOS$key
"
    return 0
  fi
  pc_stage_init
  sd="$STAGE/${key#*/}"
  mkdir -p "$sd" || pc_die sync.io_failed "$sd"
  while read -r h f; do
    [ -n "$h" ] || continue
    f="${f#\*}"
    pc_safe_rel "$f" || pc_die sync.sha_mismatch "$key/$f" "caminho invalido no SHA256SUMS publicado"
    mkdir -p "$sd/$(dirname "$f")" || pc_die sync.io_failed "$sd"
    pc_get "$url/$f" "$sd/$f" || pc_die sync.network "$url/$f"
    [ "$(pc_sha "$sd/$f")" = "$h" ] || pc_die sync.sha_mismatch "$key/$f" "esperado=$h"
  done < "$TMPD/sums"
  cp "$TMPD/sums" "$sd/SHA256SUMS" || pc_die sync.io_failed "$sd/SHA256SUMS"
  PENDING="$PENDING$key
"
}

pc_sync_prefix() { # <prefixo artifacts/...>: le o latest.txt do prefixo e sincroniza a versao apontada
  p="$1"
  pc_get "$BASE/$p/latest.txt" "$TMPD/latest"; rc=$?
  if [ $rc = 44 ]; then AUSENTES="$AUSENTES$p
"; return 0; fi
  [ $rc = 0 ] || pc_die sync.network "$BASE/$p/latest.txt"
  v="$(head -n 1 "$TMPD/latest" | tr -d '\r')"
  case "$v" in v_[0-9a-f]*) case "$v" in *[!0-9a-f_v]*) v="";; esac;; *) v="";; esac
  [ -n "$v" ] || pc_die sync.sha_mismatch "$BASE/$p/latest.txt" "latest.txt nao aponta para v_<hash>"
  POINTERS="$POINTERS$p $v
"
  pc_sync_key "$p/$v"
}

# --- execucao ---------------------------------------------------------------------------------------------------------
PREFIXOS=""
if [ "$WHAT" = artifacts ]; then
  pc_enumerate
  PREFIXOS="$(cat "$TMPD/prefixos")"
  [ -n "$PREFIXOS" ] || pc_die sync.usage "--model --area" "os filtros nao selecionam nenhum prefixo oficial"
  for p in $PREFIXOS; do pc_sync_prefix "$p"; done
  [ -n "$POINTERS" ] || pc_die sync.network "$BASE" "nenhum prefixo oficial publicado em --release-base"
else
  pc_get "$BASE/forecasts/$ORIGIN/latest.txt" "$TMPD/latest"; rc=$?
  [ $rc = 0 ] || pc_die sync.network "$BASE/forecasts/$ORIGIN/latest.txt"
  while read -r k; do
    k="$(printf '%s' "$k" | tr -d '\r')"
    [ -n "$k" ] || continue
    case "$k" in [0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]T[0-9][0-9][0-9][0-9]/?*) ;; *) pc_die sync.sha_mismatch "$BASE/forecasts/$ORIGIN/latest.txt" "chave invalida: $k";; esac
    pc_safe_rel "$k" || pc_die sync.sha_mismatch "$BASE/forecasts/$ORIGIN/latest.txt" "chave invalida: $k"
    PREFIXOS="$PREFIXOS forecasts/$ORIGIN/$k"
    pc_sync_key "forecasts/$ORIGIN/$k"
  done < "$TMPD/latest"
  [ -n "$PREFIXOS" ] || pc_die sync.network "$BASE/forecasts/$ORIGIN/latest.txt" "nenhuma chave publicada"
fi

# fase 2: tudo conferido; so agora cada chave vira publica (rename) e, por ultimo, o latest.txt local (ponteiro mutavel)
for key in $PENDING; do
  mkdir -p "$S/$(dirname "$key")" || pc_die sync.io_failed "$S/$(dirname "$key")"
  mv "$STAGE/${key#*/}" "$S/$key" || pc_die sync.io_failed "$S/$key"
  BAIXADOS="$BAIXADOS$key
"
done
if [ "$WHAT" = artifacts ]; then
  printf '%s' "$POINTERS" | while read -r p v; do
    [ -n "$p" ] || continue
    [ "$(head -n 1 "$S/$p/latest.txt" 2>/dev/null | tr -d '\r')" = "$v" ] && continue
    t="$(mktemp "$S/$p/.tmp-latest-XXXXXX")" && printf '%s\n' "$v" > "$t" && chmod 0644 "$t" && mv -f "$t" "$S/$p/latest.txt" || { rm -f "$t"; exit 1; }
    printf '%s/latest.txt\n' "$p" >> "$TMPD/trocados"
  done || pc_die sync.io_failed "$S" "falha ao regravar o latest.txt local"
  [ -f "$TMPD/trocados" ] && TROCADOS="$(cat "$TMPD/trocados")"
fi

nb="$(printf '%s' "$BAIXADOS" | grep -c .)"; np="$(printf '%s' "$PULADOS" | grep -c .)"
if [ "$PC_FORMAT" = json ]; then
  printf '{"ok":true,"exit_code":0,"verb":"prevcarga-sync","error":null,"warnings":[],"result":{"what":"%s","prefixos":%s,"baixados":%s,"pulados":%s,"latest_trocados":%s,"ausentes":%s},"run_id":null,"version":"%s"}\n' \
    "$WHAT" "$(printf '%s\n' $PREFIXOS | pc_list_json)" "$(printf '%s' "$BAIXADOS" | pc_list_json)" "$(printf '%s' "$PULADOS" | pc_list_json)" \
    "$(printf '%s\n' "$TROCADOS" | pc_list_json)" "$(printf '%s' "$AUSENTES" | pc_list_json)" "$PC_VERSION"
else
  printf 'prevcarga-sync: %s: %s baixados, %s ja presentes (estado %s)\n' "$WHAT" "$nb" "$np" "$S"
fi
exit 0
