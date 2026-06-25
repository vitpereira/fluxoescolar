#' Calcular os 5 indicadores de fluxo escolar
#'
#' A partir do painel longitudinal individual da PNADC, calcula:
#' \enumerate{
#'   \item Abandono (intra-ano): primeira obs do ano com freq=1, ultima
#'         obs do mesmo ano com freq=0
#'   \item Evasao (entre-anos): freq=1 em t, freq=0 em t+1
#'   \item Promocao (entre-anos): serie avancou
#'   \item Repetencia (entre-anos): mesma serie em t e t+1
#'   \item Nao-progressao = repetencia + evasao = 1 - promocao
#' }
#'
#' @param painel data.table com painel linkado (output de
#'   \code{\link{construir_painel}}), filtrado para \code{link_ok == 1}.
#' @param tipo \code{"inter"} (entre-anos: evasao/promocao/repetencia/nao-progressao)
#'   ou \code{"intra"} (intra-ano: abandono).
#' @param desagregacao Variaveis pelas quais agrupar a tabela. Default:
#'   \code{c("ano_t", "macroetapa")}. Outras opcoes: \code{"sexo", "raca",
#'   "quintil_renda", "perfil_cadu", "rede_agrupada", "regiao", "UF",
#'   "defasagem_cat"}.
#' @param periodo Vetor c(ano_min, ano_max) para restringir. Default
#'   \code{NULL} (todos os anos).
#' @param peso Variavel do peso amostral (default \code{"peso_v1028"}).
#'
#' @return data.table com colunas de desagregacao + colunas das taxas:
#' \code{flag_promocao, flag_repetencia, flag_evasao, flag_naoprog} (inter)
#' ou \code{flag_abandono} (intra), e \code{n_pessoa} (n na celula).
#'
#' @details
#' Para o tipo \code{"inter"}, espera-se que o painel ja tenha sido
#' transformado em formato de pares de transicao (t, t+1) atraves de uma
#' funcao interna. Para \code{"intra"}, espera-se primeira/ultima obs
#' do ano.
#'
#' @examples
#' \dontrun{
#' inter <- calcular_indicadores(painel,
#'                               tipo = "inter",
#'                               desagregacao = c("ano_t", "macroetapa"),
#'                               periodo = c(2018, 2023))
#' }
#'
#' @export
calcular_indicadores <- function(painel,
                                 tipo = c("inter", "intra"),
                                 desagregacao = c("ano_t", "macroetapa"),
                                 periodo = NULL,
                                 peso = "peso_v1028") {

    tipo <- match.arg(tipo)
    data.table::setDT(painel)

    if (tipo == "inter") {
        df <- construir_transicoes_inter(painel)
        df <- flagar_inter(df)
        if (!is.null(periodo)) {
            df <- df[ano_t >= periodo[1] & ano_t <= periodo[2]]
        }
        flag_cols <- c("flag_promocao", "flag_repetencia", "flag_evasao",
                       "flag_naoprog")
    } else {
        df <- construir_transicoes_intra(painel)
        df <- flagar_intra(df)
        if (!is.null(periodo)) {
            df <- df[ano_cal >= periodo[1] & ano_cal <= periodo[2]]
        }
        flag_cols <- "flag_abandono"
    }

    # Computar quintis de renda, perfil_cadu, regiao, macroetapa se nao existirem
    df <- derivar_variaveis(df, tipo = tipo)

    # Manter so colunas necessarias
    keep <- c(desagregacao, flag_cols, peso)
    keep <- intersect(keep, names(df))
    df <- df[, ..keep]

    # Collapse ponderado
    by_vars <- intersect(desagregacao, names(df))
    df[, n_pessoa := 1L]
    formula_avg <- vapply(flag_cols, function(v) {
        sprintf("%s = stats::weighted.mean(%s, %s, na.rm = TRUE)",
                v, v, peso)
    }, character(1))
    formula_str <- paste(c(formula_avg, "n_pessoa = .N"), collapse = ", ")
    expr <- parse(text = sprintf(".(%s)", formula_str))
    result <- df[, eval(expr), by = by_vars]
    result
}

# ----------------------------------------------------------------------------
# Helpers de transicao
# ----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
construir_transicoes_inter <- function(painel) {
    # Restringir aos linkados
    df <- painel[link_ok == 1 & !is.na(person_id)]
    # Uma obs por pessoa-ano (primeira do ano)
    data.table::setorder(df, person_id, Ano, Trimestre, visita)
    df_one <- df[, data.table::first(.SD), by = .(person_id, Ano)]
    # Lag
    data.table::setorder(df_one, person_id, Ano)
    cols_to_lag <- intersect(
        c("freq_escola", "idade", "etapa_consolid", "serie", "rede",
          "renda_dom_pc", "peso_v1028", "Trimestre"),
        names(df_one)
    )
    for (c in cols_to_lag) {
        df_one[[paste0(c, "_next")]] <- data.table::shift(df_one[[c]], -1L,
                                                          type = "lead")
    }
    df_one[, ano_next := data.table::shift(Ano, -1L, type = "lead"),
           by = person_id]
    # Manter transicoes ano+1
    df_one <- df_one[!is.na(ano_next) & ano_next == Ano + 1]
    df_one <- df_one[freq_escola == 1 & !is.na(etapa_consolid)]

    # Renomear para clareza
    data.table::setnames(df_one,
        c("Ano", "ano_next", "etapa_consolid", "etapa_consolid_next",
          "serie", "serie_next", "freq_escola", "freq_escola_next",
          "peso_v1028"),
        c("ano_t", "ano_t1", "etapa_t", "etapa_t1",
          "serie_t", "serie_t1", "freq_escola_t", "freq_escola_t1",
          "peso_v1028"),
        skip_absent = TRUE
    )
    df_one
}

#' @keywords internal
#' @noRd
construir_transicoes_intra <- function(painel) {
    df <- painel[link_ok == 1 & !is.na(person_id)]
    data.table::setorder(df, person_id, Ano, Trimestre, visita)
    df[, ano_cal := Ano]
    df[, n_obs_yr := .N, by = .(person_id, ano_cal)]
    df <- df[n_obs_yr >= 2]
    df[, first_obs := visita == min(visita), by = .(person_id, ano_cal)]
    df[, last_obs := visita == max(visita), by = .(person_id, ano_cal)]
    # Wide format
    first_dt <- df[first_obs == TRUE,
                   .(person_id, ano_cal, idade, freq_escola, sexo, raca,
                     etapa_consolid, serie, rede, renda_dom_pc, peso_v1028,
                     UF, hh_id)]
    last_dt <- df[last_obs == TRUE,
                  .(person_id, ano_cal, freq_escola_last = freq_escola)]
    data.table::setnames(first_dt,
        c("idade", "freq_escola", "etapa_consolid", "serie", "rede",
          "renda_dom_pc", "peso_v1028"),
        c("idade_first", "freq_escola_first", "etapa_consolid_first",
          "serie_first", "rede_first", "renda_dom_pc_first",
          "peso_v1028_first"))
    out <- merge(first_dt, last_dt, by = c("person_id", "ano_cal"))
    out <- out[freq_escola_first == 1 & !is.na(etapa_consolid_first)]
    out
}

#' @keywords internal
#' @noRd
flagar_inter <- function(df) {
    df[, flag_promocao := 0L]
    df[, flag_repetencia := 0L]
    df[, flag_evasao := 0L]

    df[freq_escola_t1 != 1 | is.na(freq_escola_t1), flag_evasao := 1L]
    df[freq_escola_t1 == 1 & etapa_t1 == etapa_t &
       serie_t1 == serie_t + 1, flag_promocao := 1L]
    df[freq_escola_t1 == 1 & etapa_t == 4 & serie_t == 5 &
       etapa_t1 == 5 & serie_t1 == 6, flag_promocao := 1L]
    df[freq_escola_t1 == 1 & etapa_t == 5 & serie_t == 9 &
       etapa_t1 == 10, flag_promocao := 1L]
    df[freq_escola_t1 == 1 & etapa_t == 12 & etapa_t1 > 12, flag_promocao := 1L]
    df[freq_escola_t1 == 1 & etapa_t == etapa_t1 &
       serie_t1 == serie_t, flag_repetencia := 1L]
    df[, flag_naoprog := pmin(flag_repetencia + flag_evasao, 1L)]
    df
}

#' @keywords internal
#' @noRd
flagar_intra <- function(df) {
    df[, flag_abandono := as.integer(
        freq_escola_last != 1 | is.na(freq_escola_last))]
    df
}

#' @keywords internal
#' @noRd
derivar_variaveis <- function(df, tipo = "inter") {

    sufixo <- if (tipo == "inter") "_t" else "_first"
    var_idade <- paste0("idade", sufixo)
    var_serie <- paste0("serie", sufixo)
    var_etapa <- paste0("etapa", sufixo)
    if (tipo == "intra") var_etapa <- "etapa_consolid_first"
    if (tipo == "inter") var_etapa <- "etapa_t"
    var_renda <- if (tipo == "inter") "renda_dom_pc" else "renda_dom_pc_first"
    var_rede  <- if (tipo == "inter") "rede_t" else "rede_first"
    var_ano   <- if (tipo == "inter") "ano_t" else "ano_cal"

    # Macroetapa
    if (!"macroetapa" %in% names(df) && var_etapa %in% names(df)) {
        df[, macroetapa := NA_integer_]
        df[get(var_etapa) == 4, macroetapa := 1L]
        df[get(var_etapa) == 5, macroetapa := 2L]
        df[get(var_etapa) %in% c(10, 11, 12), macroetapa := 3L]
        df[get(var_etapa) == 20, macroetapa := 4L]
        df[get(var_etapa) == 21, macroetapa := 5L]
    }

    # Quintis nacionais por ano
    if (!"quintil_renda" %in% names(df) && var_renda %in% names(df)) {
        df[, quintil_renda := NA_integer_]
        anos <- unique(df[[var_ano]])
        for (a in anos) {
            idx <- df[[var_ano]] == a
            if (sum(!is.na(df[[var_renda]][idx])) >= 5) {
                qs <- stats::quantile(df[[var_renda]][idx],
                                      probs = seq(0.2, 0.8, 0.2),
                                      na.rm = TRUE)
                cuts <- c(-Inf, qs, Inf)
                df[idx, quintil_renda := as.integer(
                    cut(get(var_renda), breaks = cuts, include.lowest = TRUE))]
            }
        }
    }

    # Rede agrupada (1=privada, 2=publica)
    if (!"rede_agrupada" %in% names(df) && var_rede %in% names(df)) {
        df[, rede_agrupada := NA_integer_]
        df[get(var_rede) == 1, rede_agrupada := 1L]
        df[get(var_rede) %in% c(2, 3, 4), rede_agrupada := 2L]
    }

    # Defasagem idade-serie
    if (!"defasagem_cat" %in% names(df) && var_idade %in% names(df) &&
        var_serie %in% names(df) && var_etapa %in% names(df)) {
        df[, idade_padrao := NA_integer_]
        df[get(var_etapa) %in% c(4, 5), idade_padrao := get(var_serie) + 5L]
        df[get(var_etapa) == 10, idade_padrao := 15L]
        df[get(var_etapa) == 11, idade_padrao := 16L]
        df[get(var_etapa) == 12, idade_padrao := 17L]
        df[, def_anos := get(var_idade) - idade_padrao]
        df[, defasagem_cat := data.table::fcase(
            def_anos <= 0, 1L,
            def_anos == 1, 2L,
            def_anos >= 2 & !is.na(def_anos), 3L,
            default = NA_integer_
        )]
    }
    df
}
