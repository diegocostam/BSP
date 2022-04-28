#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Pacotes necessários

library(shiny) # Para criar o dashboard em Shiny
library(dplyr) # Manipulação de dados
library(magrittr) # Utilização do pipe (%>%)
library(tidyr) # Pivotagem dos dados

source('a1.R', local = TRUE)
source('a2.R', local = TRUE)
source('a3.R', local = TRUE)
source('a5.R', local = TRUE)

# APP SHINY

shinyServer(function(input, output) {
        
# A1 ----------------------------------------------------------------------
    
    # Filtro do ano
    output$ano_rh <- renderUI({
        pickerInput("ano_rh_filtro", h4("Selecione o ano:"),
                    choices = unique(data_rh_tec$Ano) %>% sort(),
                    selected = unique(data_rh_tec$Ano)[1],
                    multiple = FALSE)
    })
    
## Função técnica ---------------------------------------------------------
    
    output$prof <- renderUI({
        pickerInput("prof_filtro", h4("Selecione a(s) profissão(ões):"),
                    choices = unique(data_rh_tec$profissao),
                    selected = unique(data_rh_tec$profissao),
                    options = list(`actions-box` = TRUE),multiple = T)
    })
    
    ano_rh_tec <- reactive({
        if (length(input$ano_rh_filtro) == 0) {
            return()
        } else {
            data_rh_tec %>% filter(Ano %in% input$ano_rh_filtro)
        }
        
    })
    
    mes_rh_tec <- reactive({
        if (length(input$meses_rh) == 0) {
            return()
        } else {
            ano_rh_tec() %>% filter(Mes %in% input$meses_rh)
        }
        
    })
    
    prof_mes_rh_tec <- reactive({
        if (length(input$prof_filtro) == 0 | length(input$meses_rh) == 0) {
            return()
        } else {
            mes_rh_tec() %>% filter(profissao %in% input$prof_filtro)
        }
        
    })
    
    rh_trat_tec <- reactive({
        prof_mes_rh_tec() %>%
            select(!c(Mes, Ano)) %>%
            pivot_wider(names_from = Funcao_Tecnica, values_from = Quantitativo)
    })
    
    output$tab_rh_tec <- renderTable(
        rh_trat_tec(), digits = 0
    )
    
    # Download do efetivo por função técnica
    output$download_rh_tec <- downloadHandler(
        filename = function(){"tabela.xlsx"}, 
        content = function(fname){
            writexl::write_xlsx(rh_trat_tec(), fname)
        }
    )

## Obs ---------------------------------------------------------------------

    # Usando o filtro geral do ano
    ano_obs <- reactive({
        if (length(input$ano_rh_filtro) == 0) {
            return()
        } else {
            obs %>% filter(Ano %in% input$ano_rh_filtro)
        }
        
    })
    
    # Usando o mesmo filtro da função técnica
    mes_obs <- reactive({
        if (length(input$meses_rh) == 0) {
            return()
        } else {
            ano_obs() %>% filter(Mes %in% input$meses_rh)
        }
        
    })
    
    output$text_obs <- renderText(
        mes_obs()$Obs
    )

## Função administrativa ---------------------------------------------------

    # Usando o filtro geral do ano
    ano_rh_adm <- reactive({
        if (length(input$ano_rh_filtro) == 0) {
            return()
        } else {
            data_rh_adm %>% filter(Ano %in% input$ano_rh_filtro)
        }
        
    })
    
    # Usando o mesmo filtro da função técnica
    mes_rh_adm <- reactive({
        if (length(input$meses_rh) == 0) {
            return()
        } else {
            ano_rh_adm() %>% filter(Mes %in% input$meses_rh)
        }
        
    })
    
    # Usando o mesmo filtro da função técnica
    prof_mes_rh_adm <- reactive({
        if (length(input$prof_filtro) == 0 | length(input$meses_rh) == 0) {
            return()
        } else {
            mes_rh_adm() %>% filter(profissao %in% input$prof_filtro)
        }
        
    })
    
    rh_trat_adm <- reactive({
        prof_mes_rh_adm() %>%
            select(!c(Mes, Ano)) %>%
            pivot_wider(names_from = Funcao_Administrativa, values_from = Quantitativo)
    })
    
    output$tab_rh_adm <- renderTable(
        rh_trat_adm(), digits = 0
    )
    
    # Download do efetivo por função administrativa
    output$download_rh_adm <- downloadHandler(
        filename = function(){"tabela.xlsx"}, 
        content = function(fname){
            writexl::write_xlsx(rh_trat_adm(), fname)
        }
    )


# A2 ----------------------------------------------------------------------

    # Filtro do ano
    output$ano_if <- renderUI({
        pickerInput("ano_if_filtro", h4("Selecione o ano:"),
                    choices = unique(infra_inter$Ano) %>% sort(),
                    selected = unique(infra_inter$Ano)[1],
                    multiple = FALSE)
    })
    
## Leitos hospitalares de internação ---------------------------------------

    # Usando o filtro geral do ano
    ano_infra_inter <- reactive({
        if (length(input$ano_if_filtro) == 0) {
            return()
        } else {
            infra_inter %>% filter(Ano %in% input$ano_if_filtro)
        }
        
    })
    
    mes_infra_inter <- reactive({
        if (length(input$meses_infra) == 0) {
            return()
        } else {
            ano_infra_inter() %>% filter(Mes %in% input$meses_infra) %>% select(!c(Mes, Ano))
        }
        
    })
    
    output$tab_infra_inter <- renderTable(
        mes_infra_inter(), digits = 0
    )
    
    # Download das consultas ambulatoriais
    output$download_infra_inter <- downloadHandler(
        filename = function(){"tabela.xlsx"}, 
        content = function(fname){
            writexl::write_xlsx(mes_infra_inter(), fname)
        }
    )
    
## Salas/Consultórios/Leitos sem internação ---------------------------------------

    # Usando o filtro geral do ano
    ano_infra_salas <- reactive({
        if (length(input$ano_if_filtro) == 0) {
            return()
        } else {
            infra_salas %>% filter(Ano %in% input$ano_if_filtro)
        }
        
    })
    
    mes_infra_salas <- reactive({
        if (length(input$meses_infra) == 0) {
            return()
        } else {
            ano_infra_salas() %>% filter(Mes %in% input$meses_infra) %>% select(!c(Mes, Ano))
        }
        
    })
    
    output$tab_infra_salas <- renderTable(
        mes_infra_salas(), digits = 0
    )
    
    # Download das consultas ambulatoriais
    output$download_infra_salas <- downloadHandler(
        filename = function(){"tabela.xlsx"}, 
        content = function(fname){
            writexl::write_xlsx(mes_infra_salas(), fname)
        }
    )
    
## Leitos para hospital dia ---------------------------------------
    
    # Usando o filtro geral do ano
    ano_infra_hospdia <- reactive({
        if (length(input$ano_if_filtro) == 0) {
            return()
        } else {
            infra_hospdia %>% filter(Ano %in% input$ano_if_filtro)
        }
        
    })
    
    mes_infra_hospdia <- reactive({
        if (length(input$meses_infra) == 0) {
            return()
        } else {
            ano_infra_hospdia() %>% filter(Mes %in% input$meses_infra) %>% select(!c(Mes, Ano))
        }
        
    })
    
    output$tab_infra_hospdia <- renderTable(
        mes_infra_hospdia(), digits = 0
    )
    
    # Download das consultas ambulatoriais
    output$download_infra_hospdia <- downloadHandler(
        filename = function(){"tabela.xlsx"}, 
        content = function(fname){
            writexl::write_xlsx(mes_infra_hospdia(), fname)
        }
    )
    
# A3 ------------------------------------------------------------
    
## Assistência ambulatorial ------------------------------------------------

    # Filtro do ano
    output$ano_aa <- renderUI({
        pickerInput("ano_aa_filtro", h4("Selecione o ano:"),
                    choices = unique(aten_amb$Ano) %>% sort(),
                    selected = unique(aten_amb$Ano)[1],
                    multiple = FALSE)
    })
    
### Atendimento ambulatorial ------------------------------------------------

    # Selecionando a coluca "Clinicas" para inserção no filtro
    output$clinica <- renderUI({
        
        pickerInput("clinicas", h4("Selecione a(s) clínicas(s) ou serviço(s):"),
                    choices = unique(aten_amb$Clinica),
                    selected = unique(aten_amb$Clinica),
                    options = list(`actions-box` = TRUE),multiple = T)
        
    })

    
    # Selecionando a coluca "Clientela" para inserção no filtro
    output$clientela <- renderUI({
        
        pickerInput("clientelas", h4("Selecione a(s) clientela(s):"),
                    choices = unique(aten_amb$Clientela),
                    selected = unique(aten_amb$Clientela)[8],
                    options = list(`actions-box` = TRUE),multiple = T)
        
    })
    
    # Usando o filtro geral do ano
    ano_aten_amb <- reactive({
        if (length(input$ano_aa_filtro) == 0) {
            return()
        } else {
            aten_amb %>% filter(Ano %in% input$ano_aa_filtro) %>% select(!Ano)
        }
        
    })
    
    # Criando a tabela filtrada por clínicas
    cli_aten_amb <- reactive({
        if (length(input$clinicas) == 0) {
            return()
        } else {
            ano_aten_amb() %>% filter(Clinica %in% input$clinicas)
        }
        
    })
    
    # Criando a tabela filtrada por clínicas e mês
    mes_cli_aten_amb <- reactive({
        if (length(input$meses) == 0 | length(input$clinicas) == 0) {
            return()
        } else {
            cli_aten_amb() %>% filter(Mes %in% input$meses)
        }
        
    })
    
    # Transformando os meses de valor de uma célula para coluna do data.frame
    mes_cli_aten_amb_wid <- reactive({
        if (input$checkbox == TRUE) {
            pivot_wider(mes_cli_aten_amb(), names_from = Mes, values_from = Atendimentos)
        } else{
            mes_cli_aten_amb()
        }
        
    })
    
    # Criando a tabela filtrada por clínicas e mês e clientela
    mes_cli_aten_amb_wid2 <- reactive({
        if (length(input$clientelas) == 0 | length(input$meses) == 0 | length(input$clinicas) == 0) {
            return()
        } else {
            mes_cli_aten_amb_wid() %>% filter(Clientela %in% input$clientelas)
        }
        
    })
    
    
    # Saida da tabela Assistência Ambulatorial
    output$aten_amb_tab <- renderTable(
        
        mes_cli_aten_amb_wid2(), digits = 0
        
    )
    
    # Download das consultas ambulatoriais
    output$download <- downloadHandler(
        filename = function(){"tabela.xlsx"}, 
        content = function(fname){
            writexl::write_xlsx(mes_cli_aten_amb_wid2(), fname)
        }
    )


### Atentimento ambulatoriais por clientela ---------------------------------
#ATENÇÂO
    # Saida da tabela Assistência Ambulatorial Clientela
    output$cli_tab <- renderTable(
        
        clientela_amb, digits = 0
        
    )
    
    # Download das consultas ambulatoriais por clientela
    output$download_clientela <- downloadHandler(
        filename = function(){"tabela.xlsx"}, 
        content = function(fname){
            writexl::write_xlsx(clientela_amb, fname)
        }
    )
    
## Indices ambulatoriais ---------------------------------
    
    # Filtro do ano
    output$ano_ia <- renderUI({
        pickerInput("ano_ia_filtro", h4("Selecione o ano:"),
                    choices = unique(aten_amb$Ano) %>% sort(),
                    selected = unique(aten_amb$Ano)[1],
                    multiple = FALSE)
    })
    
### Número de pareceres ambulatoriais ------------------------------------------
    
    # Usando o filtro geral do ano
    ano_par_amb <- reactive({
        if (length(input$ano_ia_filtro) == 0) {
            return()
        } else {
            par_amb %>% filter(ano %in% input$ano_ia_filtro) %>% select(!ano)
        }
        
    })
    
    # Criando a tabela filtrada por clínicas
    cli_par_amb <- reactive({
        if (length(input$pmc_f) == 0) {
            return()
        } else {
            ano_par_amb() %>% filter(clinica %in% input$pmc_f) # Utilizando filtro do PMC
        }
        
    })
    
    # Saida da tabela Índice Ambulatorial número de pareceres
    output$par_amb_tab <- renderTable({
        
        cli_par_amb()}, digits = 0, caption = "Pareceres ambulatoriais",
        caption.placement = getOption("xtable.caption.placement"), 
        caption.width = getOption("xtable.caption.width", NULL)
        
    )
    
    # Download dos pareceres ambulatoriais
    output$download_par_amb <- downloadHandler(
        filename = function(){"tabela.xlsx"}, 
        content = function(fname){
            writexl::write_xlsx(cli_par_amb(), fname)
        }
    )
    
### Prazos de marcação de consulta ------------------------------------------

    # Selecionando a coluca "Clínicas" da PMC para inserção no filtro
    output$pmc <- renderUI({
        
        pickerInput("pmc_f", h4("Selecione a(s) clínica(s):"),
                    choices = unique(pmc$clinicas),
                    selected = unique(pmc$clinicas),
                    options = list(`actions-box` = TRUE),multiple = T)
        
    })
    
    # Usando o filtro geral do ano
    ano_pmc <- reactive({
        if (length(input$ano_ia_filtro) == 0) {
            return()
        } else {
            pmc %>% filter(ano %in% input$ano_ia_filtro) %>% select(!ano)
        }
        
    })
    
    # Criando a tabela filtrada por clínicas para PMC
    cli_pmc <- reactive({
        if (length(input$pmc_f) == 0) {
            return()
        } else {
            ano_pmc() %>% filter(clinicas %in% input$pmc_f)
        }
        
    })
    
    # Saida da tabela Índice Ambulatorial Prazo de Marcação de Consulta
    output$pmc_tab <- renderTable({
        
        cli_pmc()}, digits = 0, caption = "Prazo de marcação de consulta por clínica",
        caption.placement = getOption("xtable.caption.placement"), 
        caption.width = getOption("xtable.caption.width", NULL)
        
    )
    
    # Download dos PMC ambulatoriais
    output$download_pmc <- downloadHandler(
        filename = function(){"tabela.xlsx"}, 
        content = function(fname){
            writexl::write_xlsx(cli_pmc(), fname)
        }
    )
    
### Absenteísmo -------------------------------------------------------------
    
    # Usando o filtro geral do ano
    ano_abs <- reactive({
        if (length(input$ano_ia_filtro) == 0) {
            return()
        } else {
            abs %>% filter(ano %in% input$ano_ia_filtro) %>% select(!ano)
        }
        
    })
    
    # Criando a tabela filtrada por clínicas
    # Utilizando o mesmo filtro do pmc
    cli_abs <- reactive({
        if (length(input$pmc_f) == 0) {
            return()
        } else {
            ano_abs() %>% filter(clinicas %in% input$pmc_f)
        }
        
    })
    
    # Saida da tabela Índice Ambulatorial número de pareceres
    output$abs_tab <- renderTable({
        
        cli_abs()},caption = "Absenteísmo",
        caption.placement = getOption("xtable.caption.placement"), 
        caption.width = getOption("xtable.caption.width", NULL)
        
    )
    
    # Download dos PMC ambulatoriais
    output$download_abs <- downloadHandler(
        filename = function(){"tabela.xlsx"}, 
        content = function(fname){
            writexl::write_xlsx(cli_abs(), fname)
        }
    )
    
# A5 ----------------------------------------------------------------------
    
    # Filtro do ano
    output$ano_int_clientela <- renderUI({
        pickerInput("ano_int_cli_filtro", h4("Selecione o ano:"),
                    choices = unique(int_clientela$Ano) %>% sort(),
                    selected = unique(int_clientela$Ano)[1],
                    multiple = FALSE)
    })
    
    # Selecionando a coluca "Clinicas" para inserção no filtro
    output$clinica_int_clientela <- renderUI({
        
        pickerInput("clinica_int_clientela_filtro", h4("Selecione a(s) clínicas(s) ou serviço(s):"),
                    choices = unique(int_clientela$Clinica),
                    selected = unique(int_clientela$Clinica),
                    options = list(`actions-box` = TRUE),multiple = T)
        
    })
    
    # Selecionando a coluca "Clientela" para inserção no filtro
    output$clientela_int <- renderUI({
        
        pickerInput("clientela_int_filtro", h4("Selecione a(s) clientela(s):"),
                    choices = unique(int_clientela$Clientela),
                    selected = unique(int_clientela$Clientela)[8],
                    options = list(`actions-box` = TRUE),multiple = T)
        
    })
    
## Clientela da Internação ------------------------------------------------
    
    # Usando o filtro geral do ano
    data_ano_int_cli <- reactive({
        if (length(input$ano_int_cli_filtro) == 0) {
            return()
        } else {
            int_clientela %>% filter(Ano %in% input$ano_int_cli_filtro) %>% select(!Ano)
        }
        
    })
    
    # Criando a tabela filtrada por clínicas
    data_clinica_int_cli <- reactive({
        if (length(input$clinica_int_clientela_filtro) == 0) {
            return()
        } else {
            data_ano_int_cli() %>% filter(Clinica %in% input$clinica_int_clientela_filtro)
        }
        
    })
    
    
})