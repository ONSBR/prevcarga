## Motor-brinquedo, parte A. O contador vive no env da instancia do motor (nunca no globalenv).
engine_base <- function() 100

engine_bump <- function() {
  e <- environment(engine_bump)
  n <- get0(".counter", envir = e, inherits = FALSE, ifnotfound = 0) + 1
  assign(".counter", n, envir = e)
  n
}

engine_count <- function() get0(".counter", envir = environment(engine_count), inherits = FALSE, ifnotfound = 0)

engine_fit <- function(x) {
  engine_bump()
  list(base = engine_base(), level = mean(x))
}
