test_that("construir_painel marca link_ok=1 para individuos consistentes", {
    df_mock <- data.table::data.table(
        hh_id = rep("11_001_01_01", 5),
        V2003 = rep(1L, 5),
        Ano = c(2023L, 2023L, 2023L, 2023L, 2024L),
        Trimestre = c(1L, 2L, 3L, 4L, 1L),
        visita = 1:5,
        sexo = rep(1L, 5),
        idade = c(15L, 15L, 15L, 15L, 16L),
        peso_v1028 = rep(100, 5)
    )
    df_link <- construir_painel(df_mock)
    expect_true(all(df_link$link_ok == 1L))
})

test_that("construir_painel marca link_ok=0 para sexo inconsistente", {
    df_mock <- data.table::data.table(
        hh_id = rep("11_001_01_01", 5),
        V2003 = rep(1L, 5),
        Ano = c(2023L, 2023L, 2023L, 2023L, 2024L),
        Trimestre = c(1L, 2L, 3L, 4L, 1L),
        visita = 1:5,
        sexo = c(1L, 1L, 2L, 2L, 2L),  # sexo muda - quebra link
        idade = c(15L, 15L, 15L, 15L, 16L),
        peso_v1028 = rep(100, 5)
    )
    df_link <- construir_painel(df_mock)
    # link_ok deve ser 0 porque sexo nao e consistente
    expect_true(all(df_link$link_ok == 0L))
})

test_that("construir_painel exige minimo de visitas", {
    df_mock <- data.table::data.table(
        hh_id = "11_001_01_01",
        V2003 = 1L,
        Ano = 2023L, Trimestre = 1L, visita = 1L,
        sexo = 1L, idade = 15L, peso_v1028 = 100
    )
    df_link <- construir_painel(df_mock, min_visitas = 2)
    expect_equal(df_link$link_ok[1], 0L)
})
