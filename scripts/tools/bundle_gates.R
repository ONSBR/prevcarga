## bundle_gates.R -- gates de higiene do bundle (M8.1; reutilizados pelo M8.2 e pelo M8.12). Ferramenta de DEV (fora do bundle).
## Funcoes puras sobre caminhos: sem rede, sem bash, portaveis Linux/Windows. Vazio (0 linhas) = PASS.

## Python-free: nenhum .py, python*, libpython*, pyvenv.cfg, uv.lock, pyproject.toml em toda a arvore (arquivos E diretorios).
pc_bundle_python_free <- function(root) {
  rel <- list.files(root, recursive = TRUE, all.files = TRUE, include.dirs = TRUE, no.. = TRUE)
  rel <- sort(gsub("\\", "/", rel, fixed = TRUE), method = "radix")
  base <- basename(rel)
  rules <- c(py = "\\.py$", python = "^python", libpython = "libpython", pyvenv = "^pyvenv\\.cfg$",
             uv_lock = "^uv\\.lock$", pyproject = "^pyproject\\.toml$")
  hits <- lapply(names(rules), function(k) {
    i <- grepl(rules[[k]], base, perl = TRUE)
    if (any(i)) data.frame(path = rel[i], motivo = k, stringsAsFactors = FALSE) else NULL
  })
  out <- do.call(rbind, c(list(data.frame(path = character(), motivo = character(), stringsAsFactors = FALSE)), hits))
  out <- out[order(out$path, out$motivo, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

## Rede/env: varre *.R e scripts/prevcarga de `code_dir` (raiz do code/ do bundle); so scripts/lib/runtime.R e isento.
## Linhas inteiramente de comentario (`^\\s*#`) nao contam: sao prosa (ex.: deck_reader.R e helpers_loess.R citam Sys.getenv para
## dizer que NAO o usam), nao chamada. Uma chamada real, mesmo com comentario no fim da linha, continua acusada.
pc_bundle_net_grep <- function(code_dir) {
  rx <- "s3://|aws s3|paws|httr|curl::|download\\.file|Sys\\.getenv"
  fs <- list.files(code_dir, pattern = "\\.R$", recursive = TRUE, full.names = FALSE)
  fs <- union(fs[fs != "scripts/lib/runtime.R"], intersect("scripts/prevcarga", list.files(code_dir, recursive = TRUE)))
  fs <- sort(fs, method = "radix")
  hits <- lapply(fs, function(f) {
    l <- readLines(file.path(code_dir, f), encoding = "UTF-8", warn = FALSE)
    i <- which(grepl(rx, l, perl = TRUE) & !grepl("^\\s*#", l, perl = TRUE))
    if (length(i)) data.frame(path = f, linha = i, texto = l[i], stringsAsFactors = FALSE) else NULL
  })
  do.call(rbind, c(list(data.frame(path = character(), linha = integer(), texto = character(),
                                   stringsAsFactors = FALSE)), hits))
}
