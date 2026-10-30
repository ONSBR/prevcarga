## test-cut.R -- RD.5 (AC7 do epico, `corte_horario`): o corte intradiario e HORARIO. available_until 09:00 x 09:30 sobre o mesmo
## deck: a meia hora isolada 09:00-09:30 nao forma hora cheia e e descartada em TODAS as series (decisao do Gabriel,
## 2026-10-30; diverge do oraculo de proposito) => saidas iguais nos interval_end comuns (max|Delta| == 0), 1o interval_end
## 09:30 e 10:00, ultimo O+9 00:00 e grade == prevcarga_delivery_grid(cuts, 9). CONTROL: perturbar 08:00-09:00 muda a saida.
## Deck sintetico do fixture_store + leitor (truncate_at) + despachante; RGD_MED e RGD_MAX.

for (e in rd5_variants()) {
  local({
    exo <- e
    test_that(sprintf("[PLUGINS] corte_horario: %s: 09:00 x 09:30 iguais nos intervalos comuns (Delta == 0); grade do despachante sem buraco", exo), {
      st <- rd5_store(); art <- rd5_art(exo)
      c09 <- rd5_cuts(st, "09:00"); c0930 <- rd5_cuts(st, "09:30")
      expect_identical(c09$available_until, rd5_au("09:00")); expect_identical(c0930$available_until, rd5_au("09:30"))
      p09 <- rd5_panel(st, c09, "infer"); p0930 <- rd5_panel(st, c0930, "infer")
      expect_gt(nrow(p0930$load), nrow(p09$load))                      # o painel de 09:30 traz a meia hora a mais
      o09 <- rd5_infer(art, st, c09); o0930 <- rd5_infer(art, st, c0930)
      expect_identical(format(o09$delivery$interval_end[[1L]], "%H:%M"), "09:30")
      expect_identical(format(o0930$delivery$interval_end[[1L]], "%H:%M"), "10:00")
      for (x in list(list(o09, c09), list(o0930, c0930))) {
        ie <- x[[1L]]$delivery$interval_end
        expect_identical(ie, rd_lib$prevcarga_delivery_grid(x[[2L]], 9L))
        expect_true(all(diff(as.numeric(ie)) == 1800))                 # 30 min sem buraco
        expect_identical(ie[[length(ie)]], rd5_au("00:00", st$O + 9L))
        expect_identical(lubridate::tz(ie), x[[2L]]$tz)
        expect_identical(dim(x[[1L]]$values), c(9L, 48L))
      }
      cm <- rd5_common(o09$delivery, o0930$delivery)
      expect_gt(cm$n, 400L)
      cat(sprintf("\n[RD.5 medida] corte_horario %s: max|Delta| 09:00 x 09:30 = %g MW em %d interval_end comuns\n",
                  exo, max(abs(cm$a - cm$b)), cm$n))
      expect_identical(max(abs(cm$a - cm$b)), 0)                       # Delta == 0: a meia hora isolada nao entra
      expect_identical(o09$meta$head_used, TRUE); expect_identical(o0930$meta$head_used, TRUE)
      ## CONTROL: a hora cheia 09:00-10:00 so entra quando o corte chega a 10:00 (09:30 ainda nao a completa)
      c10 <- rd5_cuts(st, "10:00"); o10 <- rd5_infer(art, st, c10)
      cc <- rd5_common(o09$delivery[interval_end >= rd5_au("10:30")], o10$delivery[interval_end >= rd5_au("10:30")])
      expect_gt(max(abs(cc$a - cc$b)), RD5_TOL)
    })

    test_that(sprintf("[PLUGINS] corte_horario: %s: CONTROL perturbar a hora 08:00-09:00 muda a saida; painel alem do corte => contract", exo), {
      st <- rd5_store(); art <- rd5_art(exo)
      c09 <- rd5_cuts(st, "09:00"); c0930 <- rd5_cuts(st, "09:30")
      base <- rd5_infer(art, st, c09)
      man <- rd5_perturb(st, "load", after = rd5_au("07:59"), at_or_before = rd5_au("08:30"))
      ctl <- rd5_infer(art, man, c09)
      expect_gt(rd5_delta(base$delivery, ctl$delivery), RD5_TOL)
      ## a meia hora isolada 09:00-09:30 perturbada nao muda a saida do corte 09:30 (hora incompleta descartada)
      b0930 <- rd5_infer(art, st, c0930)
      iso <- rd5_perturb(st, "load", after = rd5_au("08:59"), at_or_before = rd5_au("09:00"))
      expect_lte(rd5_delta(b0930$delivery, rd5_infer(art, iso, c0930)$delivery), RD5_TOL)
      ## painel CRU alem do corte entregue com o corte 09:00 => contract (o despachante confere, nunca corta)
      e1 <- rd5_err(rd5_infer(art, st, c09, panel = rd5_panel(st, c0930, "infer")))
      expect_rd5_class(e1, "contract", 2L, id = "plugin.infer_beyond_cut")
      expect_match(conditionMessage(e1), "09:30", fixed = TRUE)
      expect_null(rd5_err(rd5_infer(art, st, c09, panel = rd5_panel(st, c09, "infer"))))   # CONTROL: o painel do proprio corte
    })
  })
}
