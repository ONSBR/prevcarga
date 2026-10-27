# Estado: banner (M1.4c)

- `scripts/lib/banner.R`: `prevcarga_banner(ctx, tty, os)` devolve 8 linhas ASCII (nome, curva 6x48 de perfil fixo
  `.prevcarga_banner_profile`, `versao <ctx$version>`); cor ANSI (ESC[36m..ESC[0m) so nas linhas da curva.
- `prevcarga_use_color(ctx, tty, os)`: `always` sim; `never` nao; `auto` so em TTY fora do Windows. Nunca le env.
- `prevcarga_banner_wanted(ctx, verb)`: TRUE so em `--help`, `--version`, sem verbo (NULL/vazio) e `doctor`, e nunca com `format = json`.
  Consumidores: fachada (M1.4) e doctor (M1.5).
- Testes: `scripts/lib/tests/test-banner.R`.
