# ======================================================================
# ANÁLISE COMPLETA DE ADERÊNCIA DA ETo
# DISTRIBUIÇÕES NORMAL E GAMA
# KOLMOGOROV-SMIRNOV / MONTE CARLO / LILLIEFORS
#
# VERSÃO OTIMIZADA E CORRIGIDA
#
# - B = 20.000 réplicas
# - Reestimação dos parâmetros em cada réplica
# - Processamento paralelo
# - Checkpoints automáticos
# - Retomada automática
# - Cache da Normal por tamanho amostral N
# - Monte Carlo Gama otimizado
# - Excel + CSV + RDS
#
# Período: 1986-2015
# Local: Macapá - AP
# ======================================================================


# ======================================================================
# 0. LIMPAR AMBIENTE
# ======================================================================

rm(list = ls())

options(
  stringsAsFactors = FALSE,
  scipen = 999
)


# ======================================================================
# 1. PACOTES
# ======================================================================

pacotes <- c(
  "tidyverse",
  "lubridate",
  "readxl",
  "writexl",
  "future",
  "future.apply"
)

pacotes_faltantes <- pacotes[
  !pacotes %in% rownames(installed.packages())
]

if (length(pacotes_faltantes) > 0) {
  
  install.packages(
    pacotes_faltantes,
    dependencies = TRUE
  )
}

suppressPackageStartupMessages({
  
  library(tidyverse)
  library(lubridate)
  library(readxl)
  library(writexl)
  library(future)
  library(future.apply)
  
})


# ======================================================================
# 2. CONFIGURAÇÕES
# ======================================================================

arquivo <- "C:/Users/Dr. Jefferson/Downloads/ETo/ETo.xlsx"

B_mc <- 20000L

alpha_teste <- 0.05

seed_base <- 20260809L

versao_metodo <- "ETO_MC_OTIMIZADO_V2"


# ======================================================================
# 3. PASTAS
# ======================================================================

pasta_saida <- file.path(
  dirname(arquivo),
  "Resultados_70_combinacoes"
)

dir.create(
  pasta_saida,
  recursive = TRUE,
  showWarnings = FALSE
)


pasta_checkpoint <- file.path(
  pasta_saida,
  "checkpoints"
)

dir.create(
  pasta_checkpoint,
  recursive = TRUE,
  showWarnings = FALSE
)


pasta_checkpoint_normal <- file.path(
  pasta_checkpoint,
  "Normal"
)

dir.create(
  pasta_checkpoint_normal,
  recursive = TRUE,
  showWarnings = FALSE
)


pasta_checkpoint_gama <- file.path(
  pasta_checkpoint,
  "Gama"
)

dir.create(
  pasta_checkpoint_gama,
  recursive = TRUE,
  showWarnings = FALSE
)


# ======================================================================
# 4. PROCESSAMENTO PARALELO
# ======================================================================

nucleos_detectados <- parallel::detectCores(
  logical = TRUE
)

if (
  is.na(nucleos_detectados) ||
  nucleos_detectados < 2
) {
  
  nucleos_usados <- 1L
  
} else {
  
  nucleos_usados <- max(
    1L,
    nucleos_detectados - 1L
  )
}


cat("\n")
cat("============================================================\n")
cat("CONFIGURAÇÃO DE PROCESSAMENTO\n")
cat("============================================================\n")

cat(
  "Núcleos lógicos detectados:",
  nucleos_detectados,
  "\n"
)

cat(
  "Núcleos utilizados:",
  nucleos_usados,
  "\n"
)

cat(
  "Réplicas de Monte Carlo:",
  format(
    B_mc,
    big.mark = ".",
    decimal.mark = ","
  ),
  "\n"
)

cat(
  "Nível de significância:",
  alpha_teste,
  "\n"
)

cat(
  "Checkpoint:",
  pasta_checkpoint,
  "\n"
)


if (nucleos_usados > 1L) {
  
  future::plan(
    future::multisession,
    workers = nucleos_usados
  )
  
} else {
  
  future::plan(
    future::sequential
  )
}


# ======================================================================
# 5. VERIFICAR ARQUIVO
# ======================================================================

if (!file.exists(arquivo)) {
  
  stop(
    paste0(
      "Arquivo não encontrado:\n",
      arquivo
    )
  )
}


# ======================================================================
# 6. LEITURA
# ======================================================================

dados <- read_excel(
  arquivo,
  sheet = 1
)


# ======================================================================
# 7. COLUNAS
# ======================================================================

if (!all(c("data", "Eto") %in% names(dados))) {
  
  stop(
    "A planilha deve conter as colunas 'data' e 'Eto'."
  )
}


# ======================================================================
# 8. DATA
# ======================================================================

if (inherits(dados$data, "POSIXt")) {
  
  dados$data <- as.Date(dados$data)
  
} else if (inherits(dados$data, "Date")) {
  
  dados$data <- as.Date(dados$data)
  
} else if (is.numeric(dados$data)) {
  
  dados$data <- as.Date(
    dados$data,
    origin = "1899-12-30"
  )
  
} else {
  
  dados$data <- dmy(
    as.character(dados$data)
  )
}


# ======================================================================
# 9. PREPARAÇÃO
# ======================================================================

dados <- dados %>%
  
  mutate(
    
    Eto = as.numeric(Eto),
    
    ano = year(data),
    
    mes = month(data),
    
    dia = day(data)
    
  ) %>%
  
  arrange(data)


# ======================================================================
# 10. VERIFICAÇÃO DA BASE
# ======================================================================

cat("\n")
cat("============================================================\n")
cat("VERIFICAÇÃO DA BASE\n")
cat("============================================================\n")

cat(
  "Número de registros:",
  nrow(dados),
  "\n"
)

cat(
  "Datas únicas:",
  n_distinct(dados$data),
  "\n"
)

cat(
  "Data inicial:",
  as.character(min(dados$data)),
  "\n"
)

cat(
  "Data final:",
  as.character(max(dados$data)),
  "\n"
)

cat(
  "Número de anos:",
  n_distinct(dados$ano),
  "\n"
)

cat(
  "Datas duplicadas:",
  sum(duplicated(dados$data)),
  "\n"
)

cat(
  "Valores ausentes de ETo:",
  sum(is.na(dados$Eto)),
  "\n"
)

cat(
  "Valores de ETo <= 0:",
  sum(
    dados$Eto <= 0,
    na.rm = TRUE
  ),
  "\n"
)


# ======================================================================
# 11. CALENDÁRIO COMPLETO
# ======================================================================

calendario_completo <- tibble(
  
  data = seq.Date(
    from = as.Date("1986-01-01"),
    to = as.Date("2015-12-31"),
    by = "day"
  )
)


datas_faltantes <- calendario_completo %>%
  
  anti_join(
    dados %>% distinct(data),
    by = "data"
  )


cat(
  "Datas faltantes:",
  nrow(datas_faltantes),
  "\n"
)


# ======================================================================
# 12. CONSISTÊNCIA
# ======================================================================

if (nrow(dados) != 10957) {
  
  stop(
    paste(
      "A série possui",
      nrow(dados),
      "registros. Eram esperados 10957."
    )
  )
}


if (n_distinct(dados$data) != 10957) {
  
  stop(
    "A série não possui 10957 datas únicas."
  )
}


if (any(duplicated(dados$data))) {
  
  stop(
    "Existem datas duplicadas."
  )
}


if (nrow(datas_faltantes) > 0) {
  
  print(datas_faltantes)
  
  stop(
    "Existem datas faltantes."
  )
}


if (any(is.na(dados$Eto))) {
  
  stop(
    "Existem valores ausentes de ETo."
  )
}


if (
  any(
    dados$Eto <= 0,
    na.rm = TRUE
  )
) {
  
  stop(
    "Existem valores de ETo menores ou iguais a zero."
  )
}


cat("\nBASE APROVADA NAS VERIFICAÇÕES DE CONSISTÊNCIA.\n")


# ======================================================================
# 13. MESES
# ======================================================================

nomes_meses <- c(
  "Janeiro",
  "Fevereiro",
  "Março",
  "Abril",
  "Maio",
  "Junho",
  "Julho",
  "Agosto",
  "Setembro",
  "Outubro",
  "Novembro",
  "Dezembro"
)


# ======================================================================
# 14. AGRUPAMENTO
# ======================================================================

agrupar_mes <- function(df_mes) {
  
  m <- unique(df_mes$mes)
  
  if (length(m) != 1) {
    
    stop(
      "A função agrupar_mes deve receber apenas um mês."
    )
  }
  
  
  if (m == 2) {
    
    dias_base <- 28L
    
    janelas <- c(
      28L,
      14L,
      7L,
      4L
    )
    
  } else {
    
    dias_base <- 30L
    
    janelas <- c(
      30L,
      15L,
      10L,
      6L,
      5L,
      3L
    )
  }
  
  
  purrr::map_dfr(
    
    janelas,
    
    function(tamanho_janela) {
      
      df_mes %>%
        
        filter(
          dia <= dias_base
        ) %>%
        
        group_by(ano) %>%
        
        mutate(
          
          grupo =
            (dia - 1L) %/%
            tamanho_janela +
            1L
          
        ) %>%
        
        group_by(
          ano,
          grupo
        ) %>%
        
        summarise(
          
          n_registros = n(),
          
          n_dias = n_distinct(dia),
          
          possui_na = any(
            is.na(Eto)
          ),
          
          ETo_sum = if (
            n_registros == tamanho_janela &&
            n_dias == tamanho_janela &&
            !possui_na
          ) {
            
            sum(Eto)
            
          } else {
            
            NA_real_
          },
          
          .groups = "drop"
        ) %>%
        
        filter(
          !is.na(ETo_sum)
        ) %>%
        
        mutate(
          
          mes = m,
          
          Mes = nomes_meses[m],
          
          Escala = tamanho_janela
          
        ) %>%
        
        select(
          mes,
          Mes,
          ano,
          Escala,
          grupo,
          ETo_sum
        )
    }
  )
}


# ======================================================================
# 15. TODOS OS AGRUPAMENTOS
# ======================================================================

dados_ag <- dados %>%
  
  split(.$mes) %>%
  
  purrr::map_dfr(
    agrupar_mes
  ) %>%
  
  arrange(
    mes,
    desc(Escala),
    ano,
    grupo
  )


# ======================================================================
# 16. 70 COMBINAÇÕES
# ======================================================================

combinacoes <- dados_ag %>%
  
  group_by(
    mes,
    Mes,
    Escala
  ) %>%
  
  summarise(
    N = n(),
    .groups = "drop"
  ) %>%
  
  arrange(
    mes,
    desc(Escala)
  ) %>%
  
  mutate(
    ID = row_number(),
    .before = 1
  )


cat("\n")
cat("============================================================\n")
cat("COMBINAÇÕES MÊS x ESCALA\n")
cat("============================================================\n")

print(
  combinacoes,
  n = Inf
)

cat(
  "\nTotal de combinações:",
  nrow(combinacoes),
  "\n"
)


if (nrow(combinacoes) != 70) {
  
  stop(
    "Não foram produzidas as 70 combinações esperadas."
  )
}


# ======================================================================
# 17. TAMANHOS AMOSTRAIS
# ======================================================================

combinacoes <- combinacoes %>%
  
  mutate(
    
    N_esperado = case_when(
      
      mes == 2 & Escala == 28 ~ 30L,
      
      mes == 2 & Escala == 14 ~ 60L,
      
      mes == 2 & Escala == 7 ~ 120L,
      
      mes == 2 & Escala == 4 ~ 210L,
      
      mes != 2 & Escala == 30 ~ 30L,
      
      mes != 2 & Escala == 15 ~ 60L,
      
      mes != 2 & Escala == 10 ~ 90L,
      
      mes != 2 & Escala == 6 ~ 150L,
      
      mes != 2 & Escala == 5 ~ 180L,
      
      mes != 2 & Escala == 3 ~ 300L,
      
      TRUE ~ NA_integer_
    ),
    
    N_correto =
      N == N_esperado
  )


if (!all(combinacoes$N_correto)) {
  
  print(
    combinacoes %>%
      filter(!N_correto)
  )
  
  stop(
    "Existem tamanhos amostrais diferentes dos esperados."
  )
}


cat(
  "\nTodos os tamanhos amostrais estão corretos.\n"
)


# ======================================================================
# 18. AJUSTE NORMAL
# ======================================================================

ajuste_normal <- function(x) {
  
  x <- x[
    is.finite(x)
  ]
  
  if (length(x) < 2) {
    
    stop(
      "Amostra Normal insuficiente."
    )
  }
  
  media <- mean(x)
  
  desvio <- sd(x)
  
  if (
    !is.finite(desvio) ||
    desvio <= 0
  ) {
    
    stop(
      "Desvio-padrão inválido."
    )
  }
  
  list(
    media = media,
    desvio = desvio
  )
}


# ======================================================================
# 19. AJUSTE GAMA
# ======================================================================

ajuste_gama <- function(x) {
  
  x <- x[
    is.finite(x) &
      x > 0
  ]
  
  if (length(x) < 2) {
    
    stop(
      "Amostra Gama insuficiente."
    )
  }
  
  media_x <- mean(x)
  
  A <- log(media_x) -
    mean(log(x))
  
  if (
    !is.finite(A) ||
    A <= 0
  ) {
    
    stop(
      "Não foi possível estimar os parâmetros da Gama."
    )
  }
  
  alfa <- (
    1 +
      sqrt(
        1 +
          (4 / 3) * A
      )
  ) / (
    4 * A
  )
  
  beta <- media_x / alfa
  
  if (
    !is.finite(alfa) ||
    !is.finite(beta) ||
    alfa <= 0 ||
    beta <= 0
  ) {
    
    stop(
      "Parâmetros Gama inválidos."
    )
  }
  
  list(
    alfa = alfa,
    beta = beta
  )
}


# ======================================================================
# 20. ESTATÍSTICA D
# ======================================================================

calcular_D_valores <- function(
    x,
    F0) {
  
  n <- length(x)
  
  indices <- seq_len(n)
  
  Dmais <- max(
    indices / n -
      F0
  )
  
  Dmenos <- max(
    F0 -
      (indices - 1) / n
  )
  
  Dmax <- max(
    Dmais,
    Dmenos
  )
  
  c(
    Dmax = Dmax,
    Dmais = Dmais,
    Dmenos = Dmenos
  )
}


# ======================================================================
# 21. D NORMAL OBSERVADO
# ======================================================================

calcular_D_normal_obs <- function(
    x,
    media,
    desvio) {
  
  x <- sort(
    x[
      is.finite(x)
    ]
  )
  
  F0 <- pnorm(
    x,
    mean = media,
    sd = desvio
  )
  
  F0 <- pmin(
    pmax(F0, 0),
    1
  )
  
  calcular_D_valores(
    x,
    F0
  )
}


# ======================================================================
# 22. D GAMA OBSERVADO
# ======================================================================

calcular_D_gama_obs <- function(
    x,
    alfa,
    beta) {
  
  x <- sort(
    x[
      is.finite(x) &
        x > 0
    ]
  )
  
  F0 <- pgamma(
    x,
    shape = alfa,
    scale = beta
  )
  
  F0 <- pmin(
    pmax(F0, 0),
    1
  )
  
  calcular_D_valores(
    x,
    F0
  )
}


# ======================================================================
# 23. MONTE CARLO NORMAL OTIMIZADO
# ======================================================================

simular_D_normal_otimizado <- function(
    n,
    B,
    seed) {
  
  set.seed(seed)
  
  D_sim <- numeric(B)
  
  indices <- seq_len(n)
  
  emp_superior <- indices / n
  
  emp_inferior <- (indices - 1) / n
  
  
  for (b in seq_len(B)) {
    
    x_sim <- sort(
      rnorm(n)
    )
    
    media_sim <- mean(x_sim)
    
    desvio_sim <- sd(x_sim)
    
    F0 <- pnorm(
      x_sim,
      mean = media_sim,
      sd = desvio_sim
    )
    
    Dmais <- max(
      emp_superior -
        F0
    )
    
    Dmenos <- max(
      F0 -
        emp_inferior
    )
    
    D_sim[b] <- max(
      Dmais,
      Dmenos
    )
  }
  
  D_sim
}


# ======================================================================
# 24. CHECKPOINT NORMAL
# ======================================================================

obter_normal_N <- function(
    n,
    B,
    seed,
    alpha,
    pasta_checkpoint,
    versao_metodo) {
  
  arquivo_cp <- file.path(
    pasta_checkpoint,
    sprintf(
      "Normal_N_%03d.rds",
      n
    )
  )
  
  
  if (file.exists(arquivo_cp)) {
    
    cp <- tryCatch(
      readRDS(arquivo_cp),
      error = function(e) NULL
    )
    
    valido <-
      !is.null(cp) &&
      identical(
        cp$versao_metodo,
        versao_metodo
      ) &&
      identical(
        as.integer(cp$N),
        as.integer(n)
      ) &&
      identical(
        as.integer(cp$B),
        as.integer(B)
      ) &&
      identical(
        as.integer(cp$Seed),
        as.integer(seed)
      ) &&
      isTRUE(
        all.equal(
          cp$alpha,
          alpha
        )
      )
    
    if (valido) {
      
      return(cp)
    }
  }
  
  
  inicio <- Sys.time()
  
  
  D_sim <- simular_D_normal_otimizado(
    n = n,
    B = B,
    seed = seed
  )
  
  
  Dcritico <- unname(
    quantile(
      D_sim,
      probs = 1 - alpha,
      type = 8
    )
  )
  
  
  fim <- Sys.time()
  
  
  resultado <- list(
    
    versao_metodo = versao_metodo,
    
    N = as.integer(n),
    
    B = as.integer(B),
    
    Seed = as.integer(seed),
    
    alpha = alpha,
    
    Dcritico = Dcritico,
    
    D_sim = D_sim,
    
    Tempo_segundos =
      as.numeric(
        difftime(
          fim,
          inicio,
          units = "secs"
        )
      )
  )
  
  
  saveRDS(
    resultado,
    arquivo_cp
  )
  
  
  resultado
}


# ======================================================================
# 25. CACHE NORMAL POR N
# ======================================================================

Ns_normal <- sort(
  unique(
    combinacoes$N
  )
)


cat("\n")
cat("============================================================\n")
cat("MONTE CARLO - NORMAL\n")
cat("============================================================\n")

cat(
  "Valores de N:",
  paste(
    Ns_normal,
    collapse = ", "
  ),
  "\n"
)


tarefas_normal <- lapply(
  
  seq_along(Ns_normal),
  
  function(j) {
    
    list(
      n = Ns_normal[j],
      seed = seed_base + j - 1L
    )
  }
)


tempo_normal_inicio <- Sys.time()


resultados_cache_normal <-
  future.apply::future_lapply(
    
    tarefas_normal,
    
    function(tarefa) {
      
      obter_normal_N(
        
        n = tarefa$n,
        
        B = B_mc,
        
        seed = tarefa$seed,
        
        alpha = alpha_teste,
        
        pasta_checkpoint =
          pasta_checkpoint_normal,
        
        versao_metodo =
          versao_metodo
      )
    },
    
    future.seed = TRUE
  )


tempo_normal_fim <- Sys.time()


cache_normal <- list()

criticos_normal_N <- vector(
  "list",
  length(resultados_cache_normal)
)


for (
  j in seq_along(
    resultados_cache_normal
  )
) {
  
  res <-
    resultados_cache_normal[[j]]
  
  chave <-
    as.character(res$N)
  
  cache_normal[[chave]] <-
    res$D_sim
  
  
  criticos_normal_N[[j]] <- tibble(
    
    N =
      res$N,
    
    Dcritico_Normal =
      res$Dcritico,
    
    B =
      res$B,
    
    Seed =
      res$Seed,
    
    Tempo_segundos =
      res$Tempo_segundos
  )
  
  
  cat(
    sprintf(
      "Normal N=%3d | D crítico=%.6f | %.2f s\n",
      res$N,
      res$Dcritico,
      res$Tempo_segundos
    )
  )
}


criticos_normal_N <-
  bind_rows(
    criticos_normal_N
  )


# ======================================================================
# 26. NORMAL NAS 70 COMBINAÇÕES
# ======================================================================

resultados_normal <- vector(
  "list",
  nrow(combinacoes)
)


cat("\n")
cat("============================================================\n")
cat("NORMAL - ANÁLISE DAS 70 COMBINAÇÕES\n")
cat("============================================================\n")


for (
  i in seq_len(
    nrow(combinacoes)
  )
) {
  
  mes_i <-
    combinacoes$mes[i]
  
  Mes_i <-
    combinacoes$Mes[i]
  
  escala_i <-
    combinacoes$Escala[i]
  
  
  x <- dados_ag %>%
    
    filter(
      mes == mes_i,
      Escala == escala_i
    ) %>%
    
    pull(
      ETo_sum
    )
  
  
  n <- length(x)
  
  
  fit <- ajuste_normal(x)
  
  
  D_obs <- calcular_D_normal_obs(
    
    x = x,
    
    media =
      fit$media,
    
    desvio =
      fit$desvio
  )
  
  
  # ================================================================
  # CORREÇÃO IMPORTANTE
  # Os dois colchetes precisam ficar juntos.
  # ================================================================
  
  D_sim <- cache_normal[[as.character(n)]]
  
  
  D_critico <- unname(
    
    quantile(
      
      D_sim,
      
      probs =
        1 - alpha_teste,
      
      type = 8
    )
  )
  
  
  p_mc <- (
    
    1 +
      
      sum(
        
        D_sim >=
          unname(
            D_obs["Dmax"]
          )
      )
    
  ) / (
    
    B_mc + 1
  )
  
  
  rejeita <-
    unname(
      D_obs["Dmax"]
    ) >
    D_critico
  
  
  EP_p <- sqrt(
    
    p_mc *
      
      (1 - p_mc) /
      
      (B_mc + 1)
  )
  
  
  resultados_normal[[i]] <- tibble(
    
    ID = i,
    
    mes = mes_i,
    
    Mes = Mes_i,
    
    Escala = escala_i,
    
    N = n,
    
    Media =
      fit$media,
    
    Desvio =
      fit$desvio,
    
    Dmais =
      unname(
        D_obs["Dmais"]
      ),
    
    Dmenos =
      unname(
        D_obs["Dmenos"]
      ),
    
    Dmax =
      unname(
        D_obs["Dmax"]
      ),
    
    Dcritico =
      D_critico,
    
    p_MonteCarlo =
      p_mc,
    
    EP_p_MonteCarlo =
      EP_p,
    
    Rejeita_H0 =
      rejeita,
    
    Decisao =
      ifelse(
        rejeita,
        "Rejeita H0",
        "Não rejeita H0"
      ),
    
    B =
      B_mc
  )
  
  
  cat(
    
    sprintf(
      
      "[NORMAL %02d/70] %-10s | %2d dias | N=%3d | D=%.5f | Dcrit=%.5f | p=%.5f | %s\n",
      
      i,
      
      Mes_i,
      
      escala_i,
      
      n,
      
      unname(
        D_obs["Dmax"]
      ),
      
      D_critico,
      
      p_mc,
      
      ifelse(
        rejeita,
        "REJEITA",
        "não rejeita"
      )
    )
  )
}


resultados_normal <-
  bind_rows(
    resultados_normal
  )


# ======================================================================
# 27. MONTE CARLO GAMA OTIMIZADO
# ======================================================================

simular_D_gama_otimizado <- function(
    n,
    B,
    alfa_obs,
    beta_obs,
    seed) {
  
  set.seed(seed)
  
  D_sim <- numeric(B)
  
  indices <- seq_len(n)
  
  emp_superior <-
    indices / n
  
  emp_inferior <-
    (indices - 1) / n
  
  
  for (
    b in seq_len(B)
  ) {
    
    x_sim <- sort(
      
      rgamma(
        
        n = n,
        
        shape = alfa_obs,
        
        scale = beta_obs
      )
    )
    
    
    media_sim <-
      mean(x_sim)
    
    
    A_sim <-
      log(media_sim) -
      mean(log(x_sim))
    
    
    if (
      !is.finite(A_sim) ||
      A_sim <= 0
    ) {
      
      D_sim[b] <-
        NA_real_
      
      next
    }
    
    
    alfa_sim <- (
      
      1 +
        
        sqrt(
          
          1 +
            
            (4 / 3) *
            A_sim
        )
      
    ) / (
      
      4 *
        A_sim
    )
    
    
    beta_sim <-
      media_sim /
      alfa_sim
    
    
    F0 <- pgamma(
      
      x_sim,
      
      shape =
        alfa_sim,
      
      scale =
        beta_sim
    )
    
    
    F0 <- pmin(
      pmax(
        F0,
        0
      ),
      1
    )
    
    
    Dmais <- max(
      
      emp_superior -
        F0
    )
    
    
    Dmenos <- max(
      
      F0 -
        emp_inferior
    )
    
    
    D_sim[b] <- max(
      Dmais,
      Dmenos
    )
  }
  
  
  D_sim[
    is.finite(D_sim)
  ]
}


# ======================================================================
# 28. FUNÇÃO DE UMA COMBINAÇÃO GAMA + CHECKPOINT
# ======================================================================

processar_gama_com_checkpoint <- function(
    i,
    mes_i,
    Mes_i,
    escala_i,
    x,
    B,
    seed_i,
    alpha,
    pasta_checkpoint,
    versao_metodo) {
  
  n <- length(x)
  
  
  arquivo_cp <- file.path(
    
    pasta_checkpoint,
    
    sprintf(
      "Gama_%02d_%02d_%02d_N%03d.rds",
      i,
      mes_i,
      escala_i,
      n
    )
  )
  
  
  if (
    file.exists(
      arquivo_cp
    )
  ) {
    
    cp <- tryCatch(
      
      readRDS(
        arquivo_cp
      ),
      
      error =
        function(e) NULL
    )
    
    
    valido <-
      
      !is.null(cp) &&
      
      identical(
        cp$versao_metodo,
        versao_metodo
      ) &&
      
      identical(
        as.integer(cp$ID),
        as.integer(i)
      ) &&
      
      identical(
        as.integer(cp$N),
        as.integer(n)
      ) &&
      
      identical(
        as.integer(cp$B),
        as.integer(B)
      ) &&
      
      identical(
        as.integer(cp$Seed),
        as.integer(seed_i)
      ) &&
      
      isTRUE(
        
        all.equal(
          cp$alpha_teste,
          alpha
        )
      )
    
    
    if (valido) {
      
      resultado_cp <-
        cp$resultado
      
      resultado_cp$Origem <-
        "Checkpoint"
      
      return(
        resultado_cp
      )
    }
  }
  
  
  tempo_inicio <-
    Sys.time()
  
  
  fit_obs <-
    ajuste_gama(x)
  
  
  alfa_obs <-
    fit_obs$alfa
  
  beta_obs <-
    fit_obs$beta
  
  
  D_obs <- calcular_D_gama_obs(
    
    x = x,
    
    alfa =
      alfa_obs,
    
    beta =
      beta_obs
  )
  
  
  D_sim <- simular_D_gama_otimizado(
    
    n = n,
    
    B = B,
    
    alfa_obs =
      alfa_obs,
    
    beta_obs =
      beta_obs,
    
    seed =
      seed_i
  )
  
  
  B_efetivo <-
    length(D_sim)
  
  
  if (
    B_efetivo <
    0.999 * B
  ) {
    
    warning(
      
      paste(
        "Combinação",
        i,
        "teve apenas",
        B_efetivo,
        "réplicas válidas de",
        B
      )
    )
  }
  
  
  if (B_efetivo == 0) {
    
    stop(
      paste(
        "Nenhuma réplica válida para a combinação",
        i
      )
    )
  }
  
  
  D_critico <- unname(
    
    quantile(
      
      D_sim,
      
      probs =
        1 - alpha,
      
      type = 8
    )
  )
  
  
  p_mc <- (
    
    1 +
      
      sum(
        
        D_sim >=
          unname(
            D_obs["Dmax"]
          )
      )
    
  ) / (
    
    B_efetivo + 1
  )
  
  
  rejeita <-
    
    unname(
      D_obs["Dmax"]
    ) >
    
    D_critico
  
  
  EP_p <- sqrt(
    
    p_mc *
      
      (1 - p_mc) /
      
      (B_efetivo + 1)
  )
  
  
  tempo_fim <-
    Sys.time()
  
  
  tempo_segundos <- as.numeric(
    
    difftime(
      tempo_fim,
      tempo_inicio,
      units = "secs"
    )
  )
  
  
  resultado <- tibble(
    
    ID = i,
    
    mes =
      mes_i,
    
    Mes =
      Mes_i,
    
    Escala =
      escala_i,
    
    N = n,
    
    Alfa =
      alfa_obs,
    
    Beta =
      beta_obs,
    
    Dmais =
      unname(
        D_obs["Dmais"]
      ),
    
    Dmenos =
      unname(
        D_obs["Dmenos"]
      ),
    
    Dmax =
      unname(
        D_obs["Dmax"]
      ),
    
    Dcritico =
      D_critico,
    
    p_MonteCarlo =
      p_mc,
    
    EP_p_MonteCarlo =
      EP_p,
    
    Rejeita_H0 =
      rejeita,
    
    Decisao =
      ifelse(
        rejeita,
        "Rejeita H0",
        "Não rejeita H0"
      ),
    
    B =
      B_efetivo,
    
    Seed =
      seed_i,
    
    Tempo_segundos =
      tempo_segundos,
    
    Origem =
      "Calculado"
  )
  
  
  checkpoint <- list(
    
    versao_metodo =
      versao_metodo,
    
    ID =
      as.integer(i),
    
    mes =
      as.integer(mes_i),
    
    Mes =
      Mes_i,
    
    Escala =
      as.integer(escala_i),
    
    N =
      as.integer(n),
    
    B =
      as.integer(B),
    
    Seed =
      as.integer(seed_i),
    
    alpha_teste =
      alpha,
    
    resultado =
      resultado
  )
  
  
  saveRDS(
    checkpoint,
    arquivo_cp
  )
  
  
  resultado
}


# ======================================================================
# 29. PREPARAR TAREFAS GAMA
# ======================================================================

tarefas_gama <- vector(
  "list",
  nrow(combinacoes)
)


for (
  i in seq_len(
    nrow(combinacoes)
  )
) {
  
  mes_i <-
    combinacoes$mes[i]
  
  Mes_i <-
    combinacoes$Mes[i]
  
  escala_i <-
    combinacoes$Escala[i]
  
  
  x_i <- dados_ag %>%
    
    filter(
      mes == mes_i,
      Escala == escala_i
    ) %>%
    
    pull(
      ETo_sum
    )
  
  
  tarefas_gama[[i]] <- list(
    
    i = i,
    
    mes_i =
      mes_i,
    
    Mes_i =
      Mes_i,
    
    escala_i =
      escala_i,
    
    x =
      x_i,
    
    seed_i =
      seed_base + i
  )
}


# ======================================================================
# 30. CHECKPOINTS EXISTENTES
# ======================================================================

arquivos_cp_existentes <- list.files(
  
  pasta_checkpoint_gama,
  
  pattern =
    "\\.rds$",
  
  full.names =
    TRUE
)


cat("\n")
cat("============================================================\n")
cat("GAMA - MONTE CARLO OTIMIZADO E PARALELO\n")
cat("============================================================\n")

cat(
  "Total de combinações:",
  length(tarefas_gama),
  "\n"
)

cat(
  "Checkpoints encontrados:",
  length(arquivos_cp_existentes),
  "\n"
)

cat(
  "Núcleos utilizados:",
  nucleos_usados,
  "\n"
)

cat(
  "Réplicas por combinação:",
  format(
    B_mc,
    big.mark = ".",
    decimal.mark = ","
  ),
  "\n"
)

cat(
  "\nA execução pode ser interrompida.\n"
)

cat(
  "Ao executar novamente, checkpoints válidos serão reutilizados.\n\n"
)


# ======================================================================
# 31. GAMA EM PARALELO
# ======================================================================

tempo_gama_inicio <-
  Sys.time()


resultados_gama_lista <-
  future.apply::future_lapply(
    
    tarefas_gama,
    
    function(tarefa) {
      
      processar_gama_com_checkpoint(
        
        i =
          tarefa$i,
        
        mes_i =
          tarefa$mes_i,
        
        Mes_i =
          tarefa$Mes_i,
        
        escala_i =
          tarefa$escala_i,
        
        x =
          tarefa$x,
        
        B =
          B_mc,
        
        seed_i =
          tarefa$seed_i,
        
        alpha =
          alpha_teste,
        
        pasta_checkpoint =
          pasta_checkpoint_gama,
        
        versao_metodo =
          versao_metodo
      )
    },
    
    future.seed = TRUE,
    
    future.scheduling = 1
  )


tempo_gama_fim <-
  Sys.time()


# ======================================================================
# 32. UNIR GAMA
# ======================================================================

resultados_gama <-
  bind_rows(
    resultados_gama_lista
  ) %>%
  
  arrange(ID)


# ======================================================================
# 33. RESULTADOS GAMA
# ======================================================================

cat("\n")
cat("============================================================\n")
cat("RESULTADOS GAMA\n")
cat("============================================================\n")


for (
  i in seq_len(
    nrow(resultados_gama)
  )
) {
  
  linha <-
    resultados_gama[i, ]
  
  
  cat(
    
    sprintf(
      
      "[GAMA %02d/70] %-10s | %2d dias | N=%3d | D=%.5f | Dcrit=%.5f | p=%.5f | %s | %.2f s | %s\n",
      
      linha$ID,
      
      linha$Mes,
      
      linha$Escala,
      
      linha$N,
      
      linha$Dmax,
      
      linha$Dcritico,
      
      linha$p_MonteCarlo,
      
      ifelse(
        linha$Rejeita_H0,
        "REJEITA",
        "não rejeita"
      ),
      
      linha$Tempo_segundos,
      
      linha$Origem
    )
  )
}


# ======================================================================
# 34. ORGANIZAR
# ======================================================================

resultados_normal <-
  resultados_normal %>%
  
  arrange(
    mes,
    desc(Escala)
  )


resultados_gama <-
  resultados_gama %>%
  
  arrange(
    mes,
    desc(Escala)
  )


# ======================================================================
# 35. TABELA LONGA - 140 ANÁLISES
# ======================================================================

resultados_140 <- bind_rows(
  
  resultados_normal %>%
    
    transmute(
      
      mes,
      
      Mes,
      
      Escala,
      
      N,
      
      Modelo =
        "Normal",
      
      Parametro_1_nome =
        "Media",
      
      Parametro_1 =
        Media,
      
      Parametro_2_nome =
        "Desvio",
      
      Parametro_2 =
        Desvio,
      
      Dmais,
      
      Dmenos,
      
      Dmax,
      
      Dcritico,
      
      p_MonteCarlo,
      
      EP_p_MonteCarlo,
      
      Rejeita_H0,
      
      Decisao,
      
      B
    ),
  
  
  resultados_gama %>%
    
    transmute(
      
      mes,
      
      Mes,
      
      Escala,
      
      N,
      
      Modelo =
        "Gama",
      
      Parametro_1_nome =
        "Alfa",
      
      Parametro_1 =
        Alfa,
      
      Parametro_2_nome =
        "Beta",
      
      Parametro_2 =
        Beta,
      
      Dmais,
      
      Dmenos,
      
      Dmax,
      
      Dcritico,
      
      p_MonteCarlo,
      
      EP_p_MonteCarlo,
      
      Rejeita_H0,
      
      Decisao,
      
      B
    )
) %>%
  
  arrange(
    mes,
    desc(Escala),
    Modelo
  )


# ======================================================================
# 36. COMPARAÇÃO NORMAL x GAMA
# ======================================================================

comparacao <- resultados_normal %>%
  
  select(
    
    mes,
    
    Mes,
    
    Escala,
    
    N,
    
    Media,
    
    Desvio,
    
    Dmax_Normal =
      Dmax,
    
    Dcritico_Normal =
      Dcritico,
    
    p_Normal =
      p_MonteCarlo,
    
    Rejeita_Normal =
      Rejeita_H0,
    
    Decisao_Normal =
      Decisao
  ) %>%
  
  left_join(
    
    resultados_gama %>%
      
      select(
        
        mes,
        
        Escala,
        
        Alfa,
        
        Beta,
        
        Dmax_Gama =
          Dmax,
        
        Dcritico_Gama =
          Dcritico,
        
        p_Gama =
          p_MonteCarlo,
        
        Rejeita_Gama =
          Rejeita_H0,
        
        Decisao_Gama =
          Decisao
      ),
    
    by = c(
      "mes",
      "Escala"
    )
  ) %>%
  
  mutate(
    
    Situacao = case_when(
      
      !Rejeita_Normal &
        !Rejeita_Gama ~
        
        "Normal e Gama não rejeitadas",
      
      
      Rejeita_Normal &
        !Rejeita_Gama ~
        
        "Normal rejeitada; Gama não rejeitada",
      
      
      !Rejeita_Normal &
        Rejeita_Gama ~
        
        "Normal não rejeitada; Gama rejeitada",
      
      
      Rejeita_Normal &
        Rejeita_Gama ~
        
        "Normal e Gama rejeitadas"
    )
  ) %>%
  
  arrange(
    mes,
    desc(Escala)
  )


# ======================================================================
# 37. RESUMO DAS REJEIÇÕES
# ======================================================================

n_rejeita_normal <- sum(
  resultados_normal$Rejeita_H0
)

n_rejeita_gama <- sum(
  resultados_gama$Rejeita_H0
)


resumo_rejeicoes <- tibble(
  
  Modelo = c(
    "Normal",
    "Gama"
  ),
  
  Total_combinacoes = c(
    nrow(resultados_normal),
    nrow(resultados_gama)
  ),
  
  Rejeita_H0 = c(
    n_rejeita_normal,
    n_rejeita_gama
  ),
  
  Nao_rejeita_H0 = c(
    
    nrow(resultados_normal) -
      n_rejeita_normal,
    
    nrow(resultados_gama) -
      n_rejeita_gama
  ),
  
  Percentual_rejeicao = c(
    
    100 *
      n_rejeita_normal /
      nrow(resultados_normal),
    
    100 *
      n_rejeita_gama /
      nrow(resultados_gama)
  )
)


# ======================================================================
# 38. REJEIÇÕES NORMAL
# ======================================================================

rejeicoes_normal <-
  resultados_normal %>%
  
  filter(
    Rejeita_H0
  ) %>%
  
  select(
    Mes,
    Escala,
    N,
    Media,
    Desvio,
    Dmax,
    Dcritico,
    p_MonteCarlo,
    EP_p_MonteCarlo,
    Decisao
  )


# ======================================================================
# 39. REJEIÇÕES GAMA
# ======================================================================

rejeicoes_gama <-
  resultados_gama %>%
  
  filter(
    Rejeita_H0
  ) %>%
  
  select(
    Mes,
    Escala,
    N,
    Alfa,
    Beta,
    Dmax,
    Dcritico,
    p_MonteCarlo,
    EP_p_MonteCarlo,
    Decisao
  )


# ======================================================================
# 40. AMBAS REJEITADAS
# ======================================================================

rejeicoes_ambas <-
  comparacao %>%
  
  filter(
    Rejeita_Normal &
      Rejeita_Gama
  )


# ======================================================================
# 41. RESUMO NORMAL x GAMA
# ======================================================================

resumo_situacao <-
  comparacao %>%
  
  count(
    Situacao,
    name = "Numero_combinacoes"
  ) %>%
  
  mutate(
    
    Percentual =
      100 *
      Numero_combinacoes /
      sum(
        Numero_combinacoes
      )
  )


# ======================================================================
# 42. AUDITORIA PELO p-VALOR
# ======================================================================

auditoria_normal <-
  resultados_normal %>%
  
  mutate(
    
    rejeita_p =
      p_MonteCarlo <=
      alpha_teste,
    
    concorda =
      rejeita_p ==
      Rejeita_H0
  ) %>%
  
  filter(
    !concorda
  )


auditoria_gama <-
  resultados_gama %>%
  
  mutate(
    
    rejeita_p =
      p_MonteCarlo <=
      alpha_teste,
    
    concorda =
      rejeita_p ==
      Rejeita_H0
  ) %>%
  
  filter(
    !concorda
  )


# ======================================================================
# 43. CHECAGEM JANEIRO / 30 DIAS
# ======================================================================

checagem_jan30 <-
  comparacao %>%
  
  filter(
    mes == 1,
    Escala == 30
  )


# ======================================================================
# 44. TEMPOS
# ======================================================================

tempo_normal_total <- as.numeric(
  
  difftime(
    tempo_normal_fim,
    tempo_normal_inicio,
    units = "mins"
  )
)


tempo_gama_total <- as.numeric(
  
  difftime(
    tempo_gama_fim,
    tempo_gama_inicio,
    units = "mins"
  )
)


tempo_total_mc <-
  tempo_normal_total +
  tempo_gama_total


resumo_tempos <- tibble(
  
  Etapa = c(
    "Normal",
    "Gama",
    "Total Monte Carlo"
  ),
  
  Tempo_minutos = c(
    tempo_normal_total,
    tempo_gama_total,
    tempo_total_mc
  ),
  
  Nucleos_utilizados = c(
    nucleos_usados,
    nucleos_usados,
    nucleos_usados
  )
)


# ======================================================================
# 45. RESULTADO FINAL NO CONSOLE
# ======================================================================

cat("\n\n")
cat("============================================================\n")
cat("RESULTADO FINAL DAS 70 COMBINAÇÕES\n")
cat("============================================================\n")


cat(
  "\nNORMAL - rejeições:",
  n_rejeita_normal,
  "de 70\n"
)


cat(
  "GAMA   - rejeições:",
  n_rejeita_gama,
  "de 70\n"
)


cat("\n")
cat("------------------------------------------------------------\n")
cat("RESUMO DAS REJEIÇÕES\n")
cat("------------------------------------------------------------\n")

print(
  resumo_rejeicoes,
  width = Inf
)


cat("\n")
cat("------------------------------------------------------------\n")
cat("COMBINAÇÕES REJEITADAS - NORMAL\n")
cat("------------------------------------------------------------\n")

if (
  nrow(rejeicoes_normal) == 0
) {
  
  cat(
    "Nenhuma combinação rejeitou a distribuição Normal.\n"
  )
  
} else {
  
  print(
    rejeicoes_normal,
    n = Inf,
    width = Inf
  )
}


cat("\n")
cat("------------------------------------------------------------\n")
cat("COMBINAÇÕES REJEITADAS - GAMA\n")
cat("------------------------------------------------------------\n")

if (
  nrow(rejeicoes_gama) == 0
) {
  
  cat(
    "Nenhuma combinação rejeitou a distribuição Gama.\n"
  )
  
} else {
  
  print(
    rejeicoes_gama,
    n = Inf,
    width = Inf
  )
}


cat("\n")
cat("------------------------------------------------------------\n")
cat("NORMAL E GAMA REJEITADAS\n")
cat("------------------------------------------------------------\n")

if (
  nrow(rejeicoes_ambas) == 0
) {
  
  cat(
    "Nenhuma combinação rejeitou simultaneamente Normal e Gama.\n"
  )
  
} else {
  
  print(
    rejeicoes_ambas,
    n = Inf,
    width = Inf
  )
}


cat("\n")
cat("------------------------------------------------------------\n")
cat("SITUAÇÃO NORMAL x GAMA\n")
cat("------------------------------------------------------------\n")

print(
  resumo_situacao,
  n = Inf,
  width = Inf
)


cat("\n")
cat("------------------------------------------------------------\n")
cat("CHECAGEM - JANEIRO / 30 DIAS\n")
cat("------------------------------------------------------------\n")

print(
  checagem_jan30,
  width = Inf
)


cat("\n")
cat("------------------------------------------------------------\n")
cat("TEMPOS DE EXECUÇÃO\n")
cat("------------------------------------------------------------\n")

print(
  resumo_tempos,
  width = Inf
)


# ======================================================================
# 46. EXCEL
# ======================================================================

arquivo_resultados <- file.path(
  
  pasta_saida,
  
  "Resultados_70_combinacoes_Normal_Gama_OTIMIZADO.xlsx"
)


if (
  file.exists(
    arquivo_resultados
  )
) {
  
  unlink(
    arquivo_resultados
  )
}


write_xlsx(
  
  list(
    
    "Resumo" =
      resumo_rejeicoes,
    
    "Situacao_Normal_Gama" =
      resumo_situacao,
    
    "Comparacao_70" =
      comparacao,
    
    "Resultados_140" =
      resultados_140,
    
    "Normal_70" =
      resultados_normal,
    
    "Gama_70" =
      resultados_gama,
    
    "Rejeicoes_Normal" =
      rejeicoes_normal,
    
    "Rejeicoes_Gama" =
      rejeicoes_gama,
    
    "Rejeicoes_Ambas" =
      rejeicoes_ambas,
    
    "Criticos_Normal_N" =
      criticos_normal_N,
    
    "Combinacoes" =
      combinacoes,
    
    "Auditoria_Normal" =
      auditoria_normal,
    
    "Auditoria_Gama" =
      auditoria_gama,
    
    "Checagem_Jan30" =
      checagem_jan30,
    
    "Tempos" =
      resumo_tempos
  ),
  
  path =
    arquivo_resultados
)


# ======================================================================
# 47. CSV
# ======================================================================

write_csv(
  resultados_normal,
  file.path(
    pasta_saida,
    "Resultados_Normal_70.csv"
  )
)


write_csv(
  resultados_gama,
  file.path(
    pasta_saida,
    "Resultados_Gama_70.csv"
  )
)


write_csv(
  comparacao,
  file.path(
    pasta_saida,
    "Comparacao_Normal_Gama_70.csv"
  )
)


write_csv(
  resultados_140,
  file.path(
    pasta_saida,
    "Resultados_140_analises.csv"
  )
)


write_csv(
  resumo_rejeicoes,
  file.path(
    pasta_saida,
    "Resumo_rejeicoes.csv"
  )
)


write_csv(
  resumo_situacao,
  file.path(
    pasta_saida,
    "Resumo_situacao_Normal_Gama.csv"
  )
)


write_csv(
  resumo_tempos,
  file.path(
    pasta_saida,
    "Tempo_processamento.csv"
  )
)


# ======================================================================
# 48. SALVAR OBJETOS COMPLETOS EM RDS
# ======================================================================

arquivo_objetos <- file.path(
  
  pasta_saida,
  
  "Resultados_completos_ETo.rds"
)


saveRDS(
  
  list(
    
    configuracao = list(
      
      B_mc =
        B_mc,
      
      alpha_teste =
        alpha_teste,
      
      seed_base =
        seed_base,
      
      nucleos_detectados =
        nucleos_detectados,
      
      nucleos_usados =
        nucleos_usados,
      
      versao_metodo =
        versao_metodo
    ),
    
    combinacoes =
      combinacoes,
    
    resultados_normal =
      resultados_normal,
    
    resultados_gama =
      resultados_gama,
    
    resultados_140 =
      resultados_140,
    
    comparacao =
      comparacao,
    
    resumo_rejeicoes =
      resumo_rejeicoes,
    
    resumo_situacao =
      resumo_situacao,
    
    rejeicoes_normal =
      rejeicoes_normal,
    
    rejeicoes_gama =
      rejeicoes_gama,
    
    rejeicoes_ambas =
      rejeicoes_ambas,
    
    criticos_normal_N =
      criticos_normal_N,
    
    auditoria_normal =
      auditoria_normal,
    
    auditoria_gama =
      auditoria_gama,
    
    checagem_jan30 =
      checagem_jan30,
    
    resumo_tempos =
      resumo_tempos
  ),
  
  arquivo_objetos
)


# ======================================================================
# 49. ENCERRAR PROCESSAMENTO PARALELO
# ======================================================================

future::plan(
  future::sequential
)


# ======================================================================
# 50. MENSAGEM FINAL
# ======================================================================

cat("\n\n")
cat("============================================================\n")
cat("ARQUIVOS SALVOS COM SUCESSO\n")
cat("============================================================\n")


cat(
  "\nPasta de resultados:\n",
  pasta_saida,
  "\n"
)


cat(
  "\nArquivo Excel:\n",
  arquivo_resultados,
  "\n"
)


cat(
  "\nArquivo RDS completo:\n",
  arquivo_objetos,
  "\n"
)


cat(
  
  sprintf(
    "\nTempo da Normal: %.2f minutos\n",
    tempo_normal_total
  )
)


cat(
  
  sprintf(
    "Tempo da Gama: %.2f minutos\n",
    tempo_gama_total
  )
)


cat(
  
  sprintf(
    "Tempo Monte Carlo total: %.2f minutos\n",
    tempo_total_mc
  )
)


cat(
  "\nNúcleos utilizados:",
  nucleos_usados,
  "\n"
)


cat(
  "Réplicas por Monte Carlo:",
  format(
    B_mc,
    big.mark = ".",
    decimal.mark = ","
  ),
  "\n"
)


cat("\n")
cat("IMPORTANTE:\n")

cat(
  "Os checkpoints foram mantidos em:\n",
  pasta_checkpoint,
  "\n"
)

cat(
  "Se o processamento for interrompido, execute ESTE MESMO script novamente.\n"
)

cat(
  "As análises concluídas serão recuperadas automaticamente.\n"
)


cat("\n")
cat("============================================================\n")
cat("ANÁLISE DAS 70 COMBINAÇÕES CONCLUÍDA\n")
cat("============================================================\n")


# ======================================================================
# 51. PARTE 2 - ANÁLISES COMPLEMENTARES
# ======================================================================

arquivo_parte2 <-
  "C:/Users/Dr. Jefferson/Downloads/ETo/Parte_2_Analises_Complementares_ETo.R"


if (
  file.exists(
    arquivo_parte2
  )
) {
  
  cat("\n")
  cat("============================================================\n")
  cat("INICIANDO PARTE 2 - ANÁLISES COMPLEMENTARES\n")
  cat("============================================================\n")
  
  
  source(
    arquivo_parte2,
    encoding = "UTF-8"
  )
  
} else {
  
  cat("\n")
  
  cat(
    "Arquivo da Parte 2 não encontrado. A Parte 1 foi concluída normalmente.\n"
  )
  
  cat(
    "Caminho procurado:\n",
    arquivo_parte2,
    "\n"
  )
}


# ======================================================================
# FIM
# ======================================================================
