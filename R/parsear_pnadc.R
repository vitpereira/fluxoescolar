#' Parsear microdados PNADC trimestral
#'
#' Le os arquivos fixed-width dentro dos zips da PNADC Trimestral e os
#' converte para data.table, mantendo apenas as variaveis necessarias para
#' os indicadores de fluxo escolar.
#'
#' @param dir_zips Pasta onde os zips PNADC foram baixados (output de
#'   \code{\link{baixar_pnadc}}). Espera-se que contenha subpasta
#'   \code{docs/input_PNADC_trimestral.txt}.
#' @param dir_destino Pasta onde salvar os parquets ou .rds. Sera criada
#'   se nao existir.
#' @param anos Vetor de anos para processar. Default \code{2012:2024}.
#' @param formato Formato de saida: \code{"rds"} (default) ou \code{"parquet"}
#'   (requer pacote arrow).
#' @param skip_existing Logical. Se TRUE, pula arquivos ja parseados.
#' @param verbose Logical.
#'
#' @return Invisivelmente, lista de data.tables (um por ano-trimestre).
#'
#' @details
#' As variaveis extraidas sao:
#' \itemize{
#'   \item Identificadores do painel: \code{Ano, Trimestre, UF, UPA, V1008,
#'         V1014, V1016 (visita), V1028 (peso)}.
#'   \item Pessoa: \code{V2001, V2003, V2007 (sexo), V2009 (idade),
#'         V2010 (raca)}.
#'   \item Educacao: \code{V3001/V3001A (le-escreve), V3002 (frequenta),
#'         V3002A (rede), V3003/V3003A (etapa), V3006 (serie),
#'         V3013/V3013A (ultima serie concluida).}
#'   \item Renda: \code{V403312 (rendimento trabalho), VD4019, VD4001, VD4002}.
#' }
#'
#' @examples
#' \dontrun{
#' parsear_pnadc(dir_zips = "C:/dados/pnadc",
#'               dir_destino = "C:/dados/pnadc_parsed")
#' }
#'
#' @export
parsear_pnadc <- function(dir_zips,
                          dir_destino,
                          anos = 2012:2024,
                          formato = c("rds", "parquet"),
                          skip_existing = TRUE,
                          verbose = TRUE) {

    formato <- match.arg(formato)
    fs::dir_create(dir_destino)

    input_path <- file.path(dir_zips, "docs", "input_PNADC_trimestral.txt")
    if (!fs::file_exists(input_path)) {
        stop("Layout nao encontrado em ", input_path,
             ". Rode baixar_pnadc() primeiro.")
    }

    layout <- ler_layout_sas(input_path)
    wanted <- variaveis_padrao()
    found <- intersect(wanted, names(layout))
    if (verbose) message(glue::glue("Layout: {length(layout)} variaveis ",
                                    "({length(found)}/{length(wanted)} desejadas)"))

    results <- list()
    for (ano in anos) {
        for (q in 1:4) {
            tag <- sprintf("%02d%d", q, ano)
            zip_path <- file.path(dir_zips, sprintf("PNADC_%s.zip", tag))
            if (!fs::file_exists(zip_path)) {
                if (verbose) message(glue::glue("  {ano}Q{q}: ZIP nao encontrado, pulando"))
                next
            }

            ext <- if (formato == "rds") ".rds" else ".parquet"
            out_path <- file.path(dir_destino, sprintf("pnadc_%s%s", tag, ext))

            if (fs::file_exists(out_path) && skip_existing) {
                if (verbose) message(glue::glue("  {ano}Q{q}: ja parseado, pulando"))
                next
            }

            if (verbose) message(glue::glue("  {ano}Q{q}: parseando..."))
            df <- parsear_zip_pnadc(zip_path, layout, found)
            df$Ano <- ano
            df$Trimestre <- q

            if (formato == "rds") {
                saveRDS(df, out_path)
            } else {
                if (!requireNamespace("arrow", quietly = TRUE)) {
                    stop("Pacote 'arrow' necessario para formato parquet")
                }
                arrow::write_parquet(df, out_path)
            }

            results[[tag]] <- df
        }
    }

    invisible(results)
}

# ----------------------------------------------------------------------------
# Helpers internos
# ----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
ler_layout_sas <- function(path) {
    txt <- readLines(path, encoding = "latin1", warn = FALSE)
    txt <- paste(txt, collapse = "\n")
    pat <- "@(\\d+)\\s+(\\w+)\\s+\\$?(\\d+)\\."
    m <- gregexpr(pat, txt)
    matches <- regmatches(txt, m)[[1]]
    layout <- list()
    for (entry in matches) {
        parts <- regmatches(entry, regexec(pat, entry))[[1]]
        if (length(parts) == 4) {
            name <- parts[3]
            pos <- as.integer(parts[2])
            width <- as.integer(parts[4])
            layout[[name]] <- c(pos = pos, width = width)
        }
    }
    layout
}

#' @keywords internal
#' @noRd
variaveis_padrao <- function() {
    c("UF", "UPA", "Estrato",
      "V1008", "V1014", "V1016", "V1027", "V1028",
      "V2001", "V2003", "V2005", "V2007", "V2008", "V2009", "V2010",
      "V3001", "V3001A", "V3002", "V3002A",
      "V3003", "V3003A", "V3005", "V3005A",
      "V3006", "V3014",
      "V3013", "V3013A",
      "VD3004", "VD3005",
      "V403312", "VD4019", "VD4001", "VD4002")
}

#' @keywords internal
#' @noRd
parsear_zip_pnadc <- function(zip_path, layout, vars) {
    # Extrai temporariamente
    files_inside <- utils::unzip(zip_path, list = TRUE)
    txt_files <- files_inside$Name[grepl("\\.txt$", files_inside$Name)]
    if (length(txt_files) == 0) {
        stop("Nenhum .txt encontrado em ", zip_path)
    }
    # Pegar o maior (microdados)
    sizes <- files_inside$Length[match(txt_files, files_inside$Name)]
    micro <- txt_files[which.max(sizes)]

    tmpdir <- tempfile("pnadc_")
    fs::dir_create(tmpdir)
    on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
    utils::unzip(zip_path, files = micro, exdir = tmpdir)
    txt_path <- file.path(tmpdir, micro)

    # Calcular colspecs
    starts <- vapply(vars, function(v) layout[[v]]["pos"], numeric(1))
    widths <- vapply(vars, function(v) layout[[v]]["width"], numeric(1))
    ends <- starts + widths - 1

    df <- readr::read_fwf(
        txt_path,
        col_positions = readr::fwf_positions(starts, ends, col_names = vars),
        col_types = readr::cols(.default = readr::col_character()),
        locale = readr::locale(encoding = "latin1"),
        progress = FALSE
    )
    data.table::setDT(df)

    # Type coercion
    int_vars <- c("UF", "V1008", "V1014", "V1016", "V2001", "V2003",
                  "V2005", "V2009", "VD3004")
    num_vars <- c("V1028", "V1027", "V403312", "VD4019")
    for (v in intersect(int_vars, names(df))) {
        df[[v]] <- as.integer(df[[v]])
    }
    for (v in intersect(num_vars, names(df))) {
        df[[v]] <- as.numeric(df[[v]])
    }
    df
}
