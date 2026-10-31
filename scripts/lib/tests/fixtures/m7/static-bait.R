## static-bait.R -- isca do CONTROL do test-m7-static.R (M7.V). Um exemplo de cada padrao proibido; NUNCA e codigo de produto nem e executado.
## Esta linha de comentario cita Sys.getenv( system( mclapply cut_invariant s3:// e nao pode disparar regra alguma.
bait_env <- Sys.getenv("HOME")
bait_fork <- parallel::mclapply(1:2, identity)
bait_shell <- system("ls")
bait_symlink <- file.symlink("a", "b")
bait_tmp <- "/tmp/bait"
bait_clock <- Sys.time()
bait_today <- Sys.Date()
bait_base_dt <- as.POSIXct("2026-01-01 08:00:00")
bait_strptime <- strptime("2026-01-01", "%Y-%m-%d")
bait_as_date <- as.Date(bait_base_dt)
bait_net <- c("s3://bucket/key", "paws.storage", "download.file", "httr::GET")
bait_py <- "python3"
bait_legacy <- c("cut_invariant", "train_cutoff", "last_label_day")
bait_counts <- c(192, 8L, 5L, bait_n == 24)
bait_msg <- "mensagem com acentuação"
