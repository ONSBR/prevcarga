#' Fuso semantico do horario de verao brasileiro (constante do motor; nao e o fuso da carga)
.engine_hv_tz <- function() "America/Sao_Paulo"

#' Flag de fuso ('%Z') do meio-dia de cada dia em `.engine_hv_tz()`; "-02" marca horario de verao
.hvz_flag <- function(Data){
   format(make_datetime(year(Data),month(Data),mday(Data),12L,tz = .engine_hv_tz()),"%Z")
}

#' Exige o mesmo `tzone` em DataHora de CARGAHIST/EXOGENAHIST/EXOGENAPREV (e igual a `tz`, se dado)
.engine_assert_tz <- function(deck,tz = NULL){
   cols <- c("CARGAHIST","EXOGENAHIST","EXOGENAPREV")
   ## anexo de feriados (opcional): mesmo tzone de CARGAHIST
   cols <- c(cols,intersect(c("CARGAFER","EXOGENAFER"),names(deck)[!vapply(deck,is.null,NA)]))
   got <- vapply(cols,function(n) tz(deck[[n]]$DataHora),"")
   want <- if(is.null(tz)) got[[1L]] else tz
   bad <- which(got != want)
   if(length(bad)){
      engine_abort("engine.dessem_tz_mismatch",where = "_dessem-engine/TransformaDeck",
                   details = list(column = paste0(cols[bad[[1L]]],"$DataHora"),tz_found = got[[bad[[1L]]]],
                                  tz_expected = want))
   }
   invisible(TRUE)
}

#' Transforma os decks de entrada em arquivos de Series Historicas e de Dados Diarios
#'
#' @param deck Deck de entrada
#' @param horizonteprevisao Horizonte de previsao Semana Operativa ou 10 dias c("SEMOPER","10DIAS")
#' @param dataInicioPrev data de inicio da previsao
#' @param tz fuso da carga (`cuts$tz`); NULL adota o `tzone` de CARGAHIST. O `DataHora` do deck chega no
#'   frame de modelagem (o despachante aplicou `to_model_frame`); o motor nao faz aritmetica de hora.
#'
#' @return Dados para entrada de modelos
#'
TransformaDeck <- function(deck,dataInicioPrev,horizonteprevisao = c("SEMOPER","10DIAS"),tz = NULL){
   .engine_assert_tz(deck,tz)
   #------------------------------------------------------ -
   # HORIZONTE DE PREVISAO ====
   #------------------------------------------------------ -
   dataInicioPrev <- .as_day(dataInicioPrev)
   if(is.numeric(horizonteprevisao)) horizonteprevisao <- c("SEMOPER","10DIAS")[horizonteprevisao + 1]
   if (horizonteprevisao[1]=="SEMOPER") {
      if(wday(dataInicioPrev)==6){
         dataFimPrev <- dataInicioPrev + 7
      } else {
         dataFimPrev <- dataInicioPrev + (6 - as.numeric(format(dataInicioPrev + 1, format='%w')))
      }
   } else if(horizonteprevisao[1] == "10DIAS"){
      dataFimPrev <- dataInicioPrev + 9
      maxDataDisp <- max(as_date(deck[["EXOGENAPREV"]]$DataHora))
      if(dataFimPrev > maxDataDisp){
         message("Exogena prevista nao disponivel para o horizonte desejado")
         message("Ajustanto horizonte...")
         dataFimPrev <- maxDataDisp
      }
   } else {
      stop("Variavel Horizonte de Previsao fora dos padroes suportados")
   }

   message(paste("Horizonte de previsao informado:",dataInicioPrev,"a",dataFimPrev))

   dias_previsao=seq(dataInicioPrev, dataFimPrev, "days")
   numdiasprev=length(dias_previsao)

   #------------------------------------------------------ -
   # DADOS DE CARGA E MONTAGEM DA SERIE HISTORICA ====
   #------------------------------------------------------ -

   dtCarga <- copy(deck[["CARGAHIST"]])
   dtCarga <- dtCarga[,.(DataHora,Data = as_date(DataHora),Prev = F,Variavel = "Carga",Valor = Carga)]
   ## anexo de feriados (opcional): dias de ilha empilhados na serie historica (Prev = F)
   diasFer <- as_date(character())
   if(!is.null(deck[["CARGAFER"]])){
      dtCargaFer <- copy(deck[["CARGAFER"]])
      dtCargaFer <- dtCargaFer[,.(DataHora,Data = as_date(DataHora),Prev = F,Variavel = "Carga",Valor = Carga)]
      diasFer <- sort(unique(dtCargaFer$Data))
      dtCarga <- rbind(dtCarga,dtCargaFer)
   }

   message("CONCLUIDA LEITURA DOS DADOS DE CARGA")



   #================================ =
   # LEITURA DO ARQUIVO COM FERIADOS
   #================================ =

   dtFeriados <- copy(deck[["FERIADOS"]][,.(Data, DiaEspecial = Tipo)])
   message("CONCLUIDA LEITURA DOS DADOS DE FERIADOS")

   #============================================ =
   # LEITURA DO ARQUIVO COM DADOS DE TEMPERATURA ====
   #============================================ =

   dtExogena <- copy(deck[["EXOGENAHIST"]])
   dtExogena[,`:=` (Data = as_date(DataHora), Prev = F)]
   if(!is.null(deck[["EXOGENAFER"]])){
      dtExogenaFer <- copy(deck[["EXOGENAFER"]])
      dtExogenaFer[,`:=` (Data = as_date(DataHora), Prev = F)]
      dtExogena <- rbind(dtExogena,dtExogenaFer,use.names = TRUE)
   }
   dtExogena <- melt(dtExogena,id.vars = c("DataHora","Data","Prev"),variable.name = "Variavel",value.name = "Valor")

   message("CONCLUIDA LEITURA DOS DADOS DE EXOGENA HISTORICA")

   #=============================================== =
   # LEITURA DO ARQUIVO COM PREVISAO DE TEMPERATURA
   #=============================================== =

   dtExogenaPrev <- copy(deck[["EXOGENAPREV"]])
   dtExogenaPrev[,`:=` (Data = as_date(DataHora), Prev = T)]
   dtExogenaPrev <- melt(dtExogenaPrev[Data <= dataFimPrev],id.vars = c("DataHora","Data","Prev"),variable.name = "Variavel",value.name = "Valor")
   message("CONCLUIDA LEITURA DOS DADOS DE EXOGENA PREVISTA")

   #====================================== =
   # CRIA DATA.FRAME COM DADOS HISTORICOS
   #====================================== =

   dtExogena <- rbind(dtExogena,dtExogenaPrev)
   dtDados <- rbind(dtCarga,dtExogena)
   dtDados <- dcast(dtDados,DataHora + Data + Prev ~ Variavel,value.var = "Valor")
   dtDados <- dtFeriados[dtDados, on = "Data"]
   dtDados[is.na(DiaEspecial),DiaEspecial := 0]
   dtDados[,HVZ := .hvz_flag(Data)]
   dtDados[HVZ == "-02",HV := 1]
   dtDados[!HVZ == "-02",HV := 0]
   dtDados[,HVZ := NULL]
   ## Bloco: 0 = janela comum + horizonte; k = k-esima corrida contigua de dias do anexo (ordem de data).
   ## Dia adjacente a janela comum continua sendo bloco proprio (os lags nao atravessam).
   dtDados[,Bloco := 0L]
   if(length(diasFer)){
      blocoDia <- cumsum(c(1L,as.integer(diff(diasFer) > 1L)))
      dtDados[,Bloco := ifelse(Data %in% diasFer,blocoDia[match(Data,diasFer)],0L)]
   }

   DADOSLIDOS= c(deck[names(deck) %in% c("COMBINA","PATAMARES")],
                 list(SERIECOMP=dtDados,DIASPREV = dias_previsao,NDIASPREV = numdiasprev))

   return(DADOSLIDOS)
}


#' Leitura dos decks de entrada
#'
#' @param deck Caminho dos arquivos ou lista contendo os data.frames das tabelas de entrada
#' @param arqsAllowed (opcional) lista de tabelas de entrada para filtro
#'
#' @return lista de data.frames com dados do deck de entrada do modelo
#'
deckEntrada <- function(deck,arqsAllowed = c("CARGAHIST","EXOGENAHIST","COMBINA",
                                             "FERIADOS","PATAMARES","EXOGENAPREV")){
   if(is.character(deck)){
      patFil <- paste(paste0('.*',arqsAllowed,'.*'),collapse = "|")
      arqs <- list.files(deck,pattern = patFil,full.names = T)
      arqs_nomes <-  gsub('^.*_|\\.csv','',list.files(deck,pattern = patFil))
      deck <- lapply(arqs,fread,sep = ";",dec = ",")
      names(deck) <- arqs_nomes
   } else if(is.list(deck)) {
      deck <- deck[arqsAllowed]
      if(!is.data.table(deck[[1]])){
         deck <- lapply(deck,data.table)
      }
   } else {
      stop("Deck de entrada fora dos padroes suportados")
   }
   if(length(deck) > length(arqsAllowed)) stop("Verificar arquivos de entrada, possibilidade de duplicatas")
   if(length(deck) < length(arqsAllowed)) warning(paste("Arquivos faltantes:",
                                                        paste(arqsAllowed[which(!arqsAllowed %in% names(deck))],collapse = ",")))
   return(deck)
}
