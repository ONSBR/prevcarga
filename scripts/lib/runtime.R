## runtime.R — guarda de determinismo (M1)
##
## Responsabilidade: rodar PRIMEIRO no topo de TODO comando da CLI. Seta e ASSERE o contrato de
## determinismo in-process; aborta ruidoso se o ambiente puder quebrar Δ=0.
##
## Contrato (invariante do repo): TZ=UTC, LC_ALL=C.UTF-8, BLAS/OpenMP 1-thread
## (OMP_NUM_THREADS / OPENBLAS_NUM_THREADS / MKL_NUM_THREADS / VECLIB_MAXIMUM_THREADS = 1).
## Em R in-process (Matrix/data.table/arrow) o multithread de BLAS reintroduz não-determinismo —
## por isso aqui NÃO é só setar, é ASSERIR e abortar sob threads>1 ou TZ≠UTC.
##
## Gate falsificável (M1): um comando GUARDADO (ex. `models`) aborta sob threads>1 / TZ≠UTC (enforce roda
## antes do corpo). `doctor`/`status` são ISENTOS do enforce — rodam prevcarga_runtime_check() e REPORTAM o
## env sujo (FAIL na linha do pin) em vez de abortar, pois diagnosticar o ambiente quebrado é o caso de uso.

prevcarga_runtime_enforce <- function(strict = TRUE) {
  stop("runtime.R: prevcarga_runtime_enforce() não implementado — TODO M1")
}

prevcarga_runtime_check <- function() {
  stop("runtime.R: prevcarga_runtime_check() não implementado — TODO M1")
}
