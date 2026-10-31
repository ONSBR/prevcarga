#!/usr/bin/env bash
# scripts/tools/prepare_windows_gate.sh -- empacota, NO LINUX, o que a VM Windows precisa para rodar o gate (M8.11). Ferramenta de DEV, fora do bundle.
# NAO envia nada para lugar nenhum (sem rede, sem aws): so copia para uma pasta (e, com --zip, gera um .zip dela). Sem variavel de ambiente.
# Roteiro completo: docs/reference/windows-gate.md.
#
#   prepare_windows_gate.sh --out <pasta nova> --bundle <prevcarga-V-windows-x86_64.zip> [--sha256 <h>]
#       [--linux-output <entrega do forecast Linux>] [--deck <deck>] [--state <estado sincronizado>]
#       [--nfr-deck <deck real com manifest.json>] [--nfr-state <estado oficial sincronizado>] [--linux-bundle <tar.zst>] [--zip]
#
# Layout de <pasta>:
#   gate\install\...                 install.ps1, prevcarga.cmd, prevcarga-sync.*, error-catalog.tsv (GERADO aqui), tests\*.Tests.ps1 e helpers
#   gate\tests\windows\...           Invoke-WindowsGate.ps1, write-receipt.R, control-perturb.R
#   gate\scripts\tools\receipts.R    formato dos recibos (rodado pelo Rscript.exe do bundle instalado)
#   gate\docs\reference\windows-gate.md   o roteiro
#   bundle\prevcarga-V-windows-x86_64.zip (+ .sha256)
#   ref\linux-x86_64\ ref\deck\ ref\state\   (se informados) | nfr\deck\ nfr\state\ (se informados)
#   PACOTE.json, SHA256SUMS          inventario com sha256 de todo arquivo (a VM confere antes de rodar)
# Sai 0 se o pacote ficou completo; 8 uso invalido; 9 insumo corrompido (sha256 diverge); 2 pacote incompleto (arquivo esperado ausente).
set -u
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo="$(cd "$here/../.." && pwd)"

die() { echo "prevcarga: erro [config] (exit $1)" >&2; echo "o que: $2" >&2; echo "onde: ${3:--}" >&2; echo "como corrigir: ${4:-rode com --help}" >&2; exit "$1"; }
usage() { sed -n '2,19p' "$0" | sed 's/^# \{0,1\}//'; }

out=""; bundle=""; sha=""; linux_out=""; deck=""; state=""; nfr_deck=""; nfr_state=""; linux_bundle=""; do_zip=0
while [ $# -gt 0 ]; do
  case "$1" in
    --help|-h) usage; exit 0 ;;
    --out) out="${2:-}"; shift 2 ;;
    --bundle) bundle="${2:-}"; shift 2 ;;
    --sha256) sha="${2:-}"; shift 2 ;;
    --linux-output) linux_out="${2:-}"; shift 2 ;;
    --deck) deck="${2:-}"; shift 2 ;;
    --state) state="${2:-}"; shift 2 ;;
    --nfr-deck) nfr_deck="${2:-}"; shift 2 ;;
    --nfr-state) nfr_state="${2:-}"; shift 2 ;;
    --linux-bundle) linux_bundle="${2:-}"; shift 2 ;;
    --zip) do_zip=1; shift ;;
    *) die 8 "argumento desconhecido" "$1" "rode com --help" ;;
  esac
done
[ -n "$out" ] || die 8 "falta --out" "--out" "informe uma pasta nova"
[ -n "$bundle" ] || die 8 "falta --bundle" "--bundle" "informe o zip windows-x86_64 (dist/prevcarga-V-windows-x86_64.zip)"
[ -f "$bundle" ] || die 8 "bundle inexistente" "$bundle"
case "$(basename "$bundle")" in prevcarga-*-windows-x86_64.zip) ;; *) die 8 "o bundle precisa se chamar prevcarga-<v>-windows-x86_64.zip" "$bundle" ;; esac
if [ -e "$out" ] && [ -n "$(ls -A "$out" 2>/dev/null)" ]; then die 8 "--out existe e nao esta vazio" "$out" "use uma pasta nova"; fi
[ -z "$nfr_deck" ] || [ -f "$nfr_deck/manifest.json" ] || die 8 "o deck real precisa de manifest.json" "$nfr_deck"
if [ -n "$linux_out" ]; then
  for f in manifest.json run_manifest.json; do [ -f "$linux_out/$f" ] || die 8 "entrega Linux sem $f" "$linux_out" "use a saida de prevcarga forecast"; done
fi
if [ -n "$deck$state" ] && { [ -z "$deck" ] || [ -z "$state" ]; }; then die 8 "--deck e --state andam juntos" "--deck/--state"; fi
if [ -n "$nfr_deck$nfr_state" ] && { [ -z "$nfr_deck" ] || [ -z "$nfr_state" ]; }; then die 8 "--nfr-deck e --nfr-state andam juntos" "--nfr-deck/--nfr-state"; fi
if [ -n "$linux_out" ] && { [ -z "$deck" ] || [ -z "$state" ]; }; then die 8 "--linux-output pede --deck e --state (a VM roda o mesmo deck)" "--linux-output"; fi

# sha256 do bundle: o informado (ou o .sha256 ao lado) tem de bater com o arquivo
got="$(sha256sum "$bundle" | cut -d' ' -f1)"
if [ -z "$sha" ] && [ -f "$bundle.sha256" ]; then sha="$(cut -d' ' -f1 < "$bundle.sha256")"; fi
if [ -n "$sha" ] && [ "$sha" != "$got" ]; then die 9 "sha256 do bundle diverge do informado" "$bundle" "regere o bundle ou corrija --sha256"; fi

mkdir -p "$out/gate/install/tests" "$out/gate/tests/windows" "$out/gate/scripts/tools" "$out/gate/docs/reference" "$out/bundle" || die 8 "nao consegui criar --out" "$out"
for f in install.ps1 prevcarga.cmd prevcarga-sync.ps1 prevcarga-sync.cmd; do cp "$repo/install/$f" "$out/gate/install/$f" || die 2 "faltou install/$f" "$repo/install"; done
for f in install.Tests.ps1 prevcarga-sync.Tests.ps1 windows-vm.Tests.ps1 windows-vm-cross.Tests.ps1 windows-vm.Helpers.ps1 PSScriptAnalyzerSettings.psd1; do
  cp "$repo/install/tests/$f" "$out/gate/install/tests/$f" || die 2 "faltou install/tests/$f" "$repo/install/tests"
done
for f in Invoke-WindowsGate.ps1 write-receipt.R control-perturb.R; do cp "$repo/tests/windows/$f" "$out/gate/tests/windows/$f" || die 2 "faltou tests/windows/$f" "$repo/tests/windows"; done
cp "$repo/scripts/tools/receipts.R" "$out/gate/scripts/tools/receipts.R" || die 2 "faltou receipts.R" "$repo/scripts/tools"
cp "$repo/docs/reference/windows-gate.md" "$out/gate/docs/reference/windows-gate.md" || die 2 "faltou o roteiro" "$repo/docs/reference"
# catalogo de erros GERADO (nao versionado): o instalador e o sync o leem ao lado dos .ps1
rs="$(command -v Rscript || true)"
[ -n "$rs" ] || die 8 "Rscript ausente no PATH (use o R 4.5.3 do projeto)" "Rscript" "export PATH=/opt/R/4.5.3/bin:\$PATH"
"$rs" --vanilla "$repo/scripts/tools/export_error_catalog.R" --out "$out/gate/install/error-catalog.tsv" >/dev/null || die 2 "export_error_catalog.R falhou" "$repo/scripts/tools"
[ -s "$out/gate/install/error-catalog.tsv" ] || die 2 "catalogo gerado vazio" "$out/gate/install/error-catalog.tsv"

bn="$(basename "$bundle")"
cp "$bundle" "$out/bundle/$bn"; printf '%s  %s\n' "$got" "$bn" > "$out/bundle/$bn.sha256"
if [ -n "$linux_out" ]; then
  mkdir -p "$out/ref"; cp -rL "$linux_out" "$out/ref/linux-x86_64"; cp -rL "$deck" "$out/ref/deck"; cp -rL "$state" "$out/ref/state"
fi
if [ -n "$nfr_deck" ]; then mkdir -p "$out/nfr"; cp -rL "$nfr_deck" "$out/nfr/deck"; cp -rL "$nfr_state" "$out/nfr/state"; fi
lsha=""
if [ -n "$linux_bundle" ]; then
  [ -f "$linux_bundle" ] || die 8 "bundle Linux inexistente" "$linux_bundle"
  lsha="$(sha256sum "$linux_bundle" | cut -d' ' -f1)"
  mkdir -p "$out/ref"; printf '%s  %s\n' "$lsha" "$(basename "$linux_bundle")" > "$out/ref/linux-bundle.sha256"
fi

# inventario: SHA256SUMS (sha256sum, LF) e PACOTE.json (ASCII)
( cd "$out" && find . -type f ! -name SHA256SUMS ! -name PACOTE.json -print0 | sort -z | xargs -0 sha256sum | sed 's#  \./#  #' > SHA256SUMS )
n="$(wc -l < "$out/SHA256SUMS" | tr -d ' ')"
{
  printf '{\n  "bundle": "bundle/%s",\n  "bundle_sha256": "%s",\n  "linux_bundle_sha256": %s,\n' "$bn" "$got" "$([ -n "$lsha" ] && printf '"%s"' "$lsha" || printf 'null')"
  printf '  "ref": %s,\n  "nfr": %s,\n  "arquivos": %s,\n' "$([ -n "$linux_out" ] && echo true || echo false)" "$([ -n "$nfr_deck" ] && echo true || echo false)" "$n"
  printf '  "uso": "powershell -NoProfile -ExecutionPolicy Bypass -File gate\\\\tests\\\\windows\\\\Invoke-WindowsGate.ps1 -Bundle bundle\\\\%s -Sha256 %s -RefDir ref"\n}\n' "$bn" "$got"
} > "$out/PACOTE.json"

# autoconferencia do pacote (o teste Linux repete): os arquivos que a VM exige existem e os .ps1/.R/.cmd sao ASCII
need="gate/install/install.ps1 gate/install/prevcarga.cmd gate/install/error-catalog.tsv gate/install/tests/windows-vm.Tests.ps1 gate/install/tests/windows-vm.Helpers.ps1 gate/install/tests/windows-vm-cross.Tests.ps1 gate/install/tests/install.Tests.ps1 gate/install/tests/prevcarga-sync.Tests.ps1 gate/tests/windows/Invoke-WindowsGate.ps1 gate/tests/windows/write-receipt.R gate/scripts/tools/receipts.R gate/docs/reference/windows-gate.md bundle/$bn bundle/$bn.sha256 SHA256SUMS PACOTE.json"
for f in $need; do [ -f "$out/$f" ] || die 2 "pacote incompleto: falta $f" "$out"; done
if LC_ALL=C grep -rlP '[^\x00-\x7F]' "$out/gate" --include='*.ps1' --include='*.cmd' --include='*.R' --include='*.psd1' >/dev/null 2>&1; then die 2 "arquivo .ps1/.cmd/.R com byte > 127 no pacote" "$out/gate"; fi

if [ "$do_zip" -eq 1 ]; then
  command -v zip >/dev/null 2>&1 || die 8 "--zip pede o utilitario zip" "zip" "instale o zip ou copie a pasta"
  ( cd "$out" && zip -qr "../$(basename "$out").zip" . ) || die 2 "zip falhou" "$out"
  echo "zip: $(dirname "$out")/$(basename "$out").zip"
fi
echo "pacote: $out ($n arquivos; bundle sha256 $got)"
echo "na VM: gate\\tests\\windows\\Invoke-WindowsGate.ps1 -Bundle bundle\\$bn -Sha256 $got -RefDir ref   (roteiro: gate\\docs\\reference\\windows-gate.md)"
exit 0
