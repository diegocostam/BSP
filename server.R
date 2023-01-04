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
source('a7.R', local = TRUE)
source('a6.R', local = TRUE)

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
        if (length(input$prof_filtro) == 0 | length(input$meses_rh) == 0) {
            return()
        } else {
            prof_mes_rh_tec() %>%
                select(!c(Mes, Ano)) %>%
                pivot_wider(names_from = Funcao_Tecnica, values_from = Quantitativo) %>% 
                mutate(TOTAL = select(.,`CSM / CAM(QT) / CAP / CFN`:OUTROS) %>% 
                           apply(1, sum, na.rm=TRUE))
        }
    })
    
    rh_trat_tec_tot <- reactive({
        if(length(input$prof_filtro) == 0 | length(input$meses_rh) == 0){
            return()
        } else {
            
            TOTAL <- rh_trat_tec() %>% summarise_if(is.numeric, sum, na.rm = TRUE)
            
            TOTAL <- tibble(profissao="TOTAL", TOTAL)
            
            rbind(rh_trat_tec(), TOTAL)
        }
    })
    
    output$tab_rh_tec <- renderTable(
        rh_trat_tec_tot(), digits = 0
    )
    
    # Download do efetivo por função técnica
    output$download_rh_tec <- downloadHandler(
        filename = function(){"tabela.xlsx"}, 
        content = function(fname){
            writexl::write_xlsx(rh_trat_tec_tot(), fname)
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
        if (length(input$prof_filtro) == 0 | length(input$meses_rh) == 0) {
            return()
        } else {
            prof_mes_rh_adm() %>%
                select(!c(Mes, Ano)) %>%
                pivot_wider(names_from = Funcao_Administrativa, values_from = Quantitativo) %>% 
                mutate(TOTAL = select(.,`CSM / CAM(QT) / CAP / CFN`:OUTROS) %>% 
                           apply(1, sum, na.rm=TRUE))
        }
    })
    
    
    rh_trat_adm_tot <- reactive({
        if(length(input$prof_filtro) == 0 | length(input$meses_rh) == 0){
            return()
        } else {
            
            TOTAL <- rh_trat_adm() %>% summarise_if(is.numeric, sum, na.rm = TRUE)
            
            TOTAL <- tibble(profissao="TOTAL", TOTAL)
            
            rbind(rh_trat_adm(), TOTAL)
        }
    })
    
    output$tab_rh_adm <- renderTable(
        rh_trat_adm_tot(), digits = 0
    )
    
    # Download do efetivo por função administrativa
    output$download_rh_adm <- downloadHandler(
        filename = function(){"tabela.xlsx"}, 
        content = function(fname){
            writexl::write_xlsx(rh_trat_adm_tot(), fname)
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
            pivot_wider(mes_cli_aten_amb(), names_from = Mes, values_from = Atendimentos) %>% 
                mutate(TOTAL = select_if(., is.numeric) %>%  rowSums(na.rm = TRUE))
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
    
    cli_par_amb_tot <- reactive({
        if(length(input$pmc_f) == 0){
            return()
        } else {
            
            TOTAL <- cli_par_amb() %>% summarise_if(is.numeric, sum, na.rm = TRUE)
            
            TOTAL <- data.frame(clinica="TOTAL", TOTAL)
            
            rbind(cli_par_amb(), TOTAL)
        }
    })
    
    # Saida da tabela Índice Ambulatorial número de pareceres
    output$par_amb_tab <- renderTable({
        
        cli_par_amb_tot()}, digits = 0, caption = "Pareceres ambulatoriais",
        caption.placement = getOption("xtable.caption.placement"), 
        caption.width = getOption("xtable.caption.width", NULL)
        
    )
    
    # Download dos pareceres ambulatoriais
    output$download_par_amb <- downloadHandler(
        filename = function(){"tabela.xlsx"}, 
        content = function(fname){
            writexl::write_xlsx(cli_par_amb_tot(), fname)
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
    
    # Criando a tabela filtrada por clínicas e mês
    mes_cli_int <- reactive({
        if (length(input$meses_inter) == 0 | length(input$clinica_int_clientela_filtro) == 0) {
            return()
        } else {
            data_clinica_int_cli() %>% filter(Mes %in% input$meses_inter)
        }
        
    })
    
    # Transformando os meses de valor de uma célula para coluna do data.frame
    mes_cli_int_wid <- reactive({
        
            pivot_wider(mes_cli_int(), names_from = Mes, values_from = Internacao) %>% 
            mutate(TOTAL =  select_if(., is.numeric) %>%  rowSums(na.rm = TRUE))
        
        
    })
    
    # Criando a tabela filtrada por clínicas e mês e clientela
    mes_cli_int_wid2 <- reactive({
        if (length(input$clientela_int_filtro) == 0 | length(input$meses_inter) == 0 | length(input$clinica_int_clientela_filtro) == 0) {
            return()
        } else {
            mes_cli_int_wid() %>% filter(Clientela %in% input$clientela_int_filtro)
        }
        
    })
    
    
    # Saida da tabela Internação Clientela
    output$int_cli_tab <- renderTable(
        
        mes_cli_int_wid2(), digits = 0
        
    )
    
    # Download das consultas ambulatoriais
    output$download_int_cli <- downloadHandler(
        filename = function(){"tabela.xlsx"}, 
        content = function(fname){
            writexl::write_xlsx(mes_cli_int_wid2(), fname)
        }
    )
    
## Clientela da Internação ------------------------------------------------
    
    # Filtro do ano
    output$ano_tipo_inter <- renderUI({
        pickerInput("ano_tipo_inter_filtro", h4("Selecione o ano:"),
                    choices = unique(int_clientela$Ano) %>% sort(),
                    selected = unique(int_clientela$Ano)[1],
                    multiple = FALSE)
    })
    
    # Usando o filtro geral do ano
    data_ano_tipo_inter <- reactive({
        if (length(input$ano_tipo_inter_filtro) == 0) {
            return()
        } else {
            int_clientela %>% filter(Ano %in% input$ano_tipo_inter_filtro) %>% select(!Ano)
        }
        
    })
    
    # Criando a tabela filtrada por clínicas e mês
    mes_tipo_int <- reactive({
        if (length(input$meses_tipo_inter) == 0 | length(input$ano_tipo_inter_filtro) == 0) {
            return()
        } else {
            data_ano_tipo_inter() %>% filter(Mes %in% input$meses_tipo_inter)
        }
        
    })
    
    # Transformando os meses de valor de uma célula para coluna do data.frame
    mes_tipo_int_wid <- reactive({
        
        if(length(input$meses_tipo_inter) == 0 | length(input$ano_tipo_inter_filtro) == 0){
            return()
        } else {
            a <- pivot_wider(mes_tipo_int(), names_from = Mes, values_from = Internacao) %>% 
                filter(Clientela == "TOTAL") %>% select(!Clientela)
            
            cir <- a %>% filter(Clinica %in% c("2- CIRURGIA BUCO-MAXILO FACIAL", "4- CIRURGIA CARDÍACA", "5- CIRURGIA GERAL",
                                               "7- CIRURGIA PLÁSTICA", "8- CIRURGIA TORÁCICA","9- CIRURGIA VASCULAR",
                                               "15- GINECOLOGIA", "20- NEUROCIRURGIA", "23- OFTALMOLOGIA", "25- OTORRINOLARINGOLOGIA",
                                               "28- PROCTOLOGIA", "31- TRAUMATO-ORTOPEDIA", "32- UROLOGIA")) %>% 
                summarise_if(is.numeric, sum, na.rm = TRUE)
            ob <- a %>% filter(Clinica %in% c("22- OBSTETRÍCIA")) %>% 
                summarise_if(is.numeric, sum, na.rm = TRUE)
            ped <- a %>% filter(Clinica %in% c("26- PEDIATRIA")) %>% 
                summarise_if(is.numeric, sum, na.rm = TRUE)
            nc <- a %>% filter(Clinica %in% c("1- CARDIOLOGIA", "10- CLÍNICA MÉDICA", "11- DERMATOLOGIA",
                                              "12- DOENÇAS INFECCIOSAS E PARASITÁRIAS (DIP)", "13- ENDOCRINOLOGIA",
                                              "14- GASTROENTEROLOGIA", "16- GERIATRIA", "17- HEMATOLOGIA",
                                              "18- MEDICINA NUCLEAR", "19- NEFROLOGIA", "21- NEUROLOGIA",
                                              "24- ONCOLOGIA", "27- PNEUMOLOGIA", "30- REUMATOLOGIA")) %>% 
                summarise_if(is.numeric, sum, na.rm = TRUE)
            cf <- a %>% filter(Clinica %in% c("33- UTI", "34- UTI PEDIÁTRICO", "35- UTI NEONATAL",
                                              "36- UNIDADE DE TRATAMENTO DE QUEIMADOS (UTQ)",
                                              "37- UNIDADE CORONARIANA (UC)", "38- UNIDADE DE PACIENTE GRAVE (UPG)",
                                              "39- UNIDADE INTERMEDIÁRIA (UI)", "40- OUTROS", "EMERGÊNCIA")) %>% 
                summarise_if(is.numeric, sum, na.rm = TRUE)
            
            Tipo <- c("CLÍNICAS CIRÚRGICAS MENOS OBSTETRÍCIA", "OBSTETRÍCIA",
                      "PEDIÁTRICA", "CLÍNICAS NÃO CIRÚRGICAS", "UNIDADES FECHADAS")
            
            b <- rbind(cir, ob, ped, nc, cf)
            
            c <- cbind(Tipo, b)
            
            TOTAL <- c %>% summarise_if(is.numeric, sum, na.rm = TRUE)
            
            TOTAL <- data.frame(Tipo="TOTAL", TOTAL)
            
            rbind(c, TOTAL) %>% mutate(TOTAL = select_if(., is.numeric) %>%  rowSums(na.rm = TRUE))
        }
        
    })

    # Saida da tabela Internação Clientela
    output$int_tipo_tab <- renderTable(
        
        mes_tipo_int_wid(), digits = 0
        
    )
    
    # Download das consultas ambulatoriais
    output$download_int_tipo <- downloadHandler(
        filename = function(){"tabela.xlsx"}, 
        content = function(fname){
            writexl::write_xlsx(mes_tipo_int_wid(), fname)
        }
    )
    

## Indicadores Hospitalares ------------------------------------------------

    output$ano_ind <- renderUI({
        pickerInput("ano_ind_filtro", h4("Selecione o ano:"),
                    choices = unique(ind_inter$Ano) %>% sort(),
                    selected = unique(ind_inter$Ano)[1],
                    multiple = FALSE)
    })
    
    
    
### Indicadores de Internação ----------------------------------------------
    
    data_ano_ind_int <- reactive({
        ind_inter %>% filter(Ano %in% input$ano_ind_filtro) %>% select(!Ano)
    })
    
    data_mes_ind_int <- reactive({
        if(length(input$meses_ind) == 0){
            return()
        } else {
            data_ano_ind_int() %>% filter(Mes %in% input$meses_ind)
        }
    })
    
    data_mes_ind_wid <- reactive({
        if(length(input$meses_ind) == 0){
            return()
        } else {
            pivot_wider(data_mes_ind_int(), names_from = Mes, values_from = Valores)
        }
    })
    
    # Saida da tabela Internação Clientela
    output$ind_tab <- renderTable(
        
        data_mes_ind_wid(), digits = 0
        
    )
    
    # Download das consultas ambulatoriais
    output$download_ind <- downloadHandler(
        filename = function(){"tabela.xlsx"}, 
        content = function(fname){
            writexl::write_xlsx(data_mes_ind_wid(), fname)
        }
    )
    

### Outros indicadores de internação ----------------------------------------------------
    
    oii_ano <- reactive({
        ind_inter_outros %>% filter(Ano %in% input$ano_ind_filtro) %>% select(!Ano)
    })
    
    oii_ano_mes <- reactive({
        if(length(input$meses_ind) == 0){
            return()
        } else {
            oii_ano() %>% filter(Mes %in% input$meses_ind)
        }
    })
    
    oii_ano_mes_t <- reactive({
        if(length(input$meses_ind) == 0){
            return()
        } else {
            c <- pivot_wider(oii_ano_mes(), names_from = Mes, values_from = Valor)
            
            TOTAL <- c %>% summarise_if(is.numeric, sum, na.rm = TRUE)
            
            TOTAL <- data.frame(Tipo="TOTAL", TOTAL)
            
            rbind(c, TOTAL) %>% mutate(TOTAL = select_if(., is.numeric) %>%  rowSums(na.rm = TRUE))
        }
    })
    
    # Saida da tabela Internação Clientela
    output$oii_tab <- renderTable(
        
        oii_ano_mes_t(), digits = 0
        
    )
    
    # Download das consultas ambulatoriais
    output$download_oii <- downloadHandler(
        filename = function(){"tabela.xlsx"}, 
        content = function(fname){
            writexl::write_xlsx(oii_ano_mes_t(), fname)
        }
    )
    
### Indicadores hospitalares ----------------------------------------------------
    
    ih_ano <- reactive({
        taxas %>% filter(Ano %in% input$ano_ind_filtro) %>% select(!Ano)
    })
    
    ih_ano_mes <- reactive({
        if(length(input$meses_ind) == 0){
            return()
        } else {
            ih_ano() %>% filter(Mes %in% input$meses_ind)
        }
    })
    
    ih_ano_mes_t <- reactive({
        if(length(input$meses_ind) == 0){
            return()
        } else {
            pivot_longer(ih_ano_mes(), cols = 2:11, names_to = "Indicadores", values_to = "taxa") %>% 
                filter(Indicadores != "MÉDIA DE CENSO DIÁRIO (MCD)") %>% 
                mutate(taxa = paste(round2(taxa, digits = 2), "%", sep = "")) %>%
                bind_rows(., pivot_longer(ih_ano_mes(), cols = 2:11, names_to = "Indicadores", values_to = "taxa") %>%
                              filter(Indicadores == "MÉDIA DE CENSO DIÁRIO (MCD)") %>%
                              mutate(taxa = round2(taxa, digits = 2) %>% as.character())) %>%
                pivot_wider(names_from = Mes, values_from = taxa)
        }
    })
    
    # Saida da tabela Internação Clientela
    output$ih_tab <- renderTable(
        
        ih_ano_mes_t(), digits = 0
        
    )
    
    # Download das consultas ambulatoriais
    output$download_ih <- downloadHandler(
        filename = function(){"tabela.xlsx"}, 
        content = function(fname){
            writexl::write_xlsx(ih_ano_mes_t(), fname)
        }
    )
    
### Partos e Nascimento ----------------------------------------------------

    # Parto
    parto_ano <- reactive({
        ind_parto %>% filter(Ano %in% input$ano_ind_filtro) %>% select(!Ano)
    })
    
    parto_ano_mes <- reactive({
        if(length(input$meses_ind) == 0){
            return()
        } else {
            parto_ano() %>% filter(Mes %in% input$meses_ind)
        }
    })
    
    parto_ano_mes_t <- reactive({
        if(length(input$meses_ind) == 0){
            return()
        } else {
            c <- pivot_wider(parto_ano_mes(), names_from = Mes, values_from = Quantidade)
            
            TOTAL <- c %>% summarise_if(is.numeric, sum, na.rm = TRUE)
            
            TOTAL <- data.frame(Partos="TOTAL", TOTAL)
            
            rbind(c, TOTAL) %>% mutate(TOTAL = select_if(., is.numeric) %>%  rowSums(na.rm = TRUE))
        }
    })
    
    # Saida da tabela Internação Clientela
    output$parto_tab <- renderTable(
        
        parto_ano_mes_t(), digits = 0
        
    )
    
    # Download das consultas ambulatoriais
    output$download_parto <- downloadHandler(
        filename = function(){"tabela.xlsx"}, 
        content = function(fname){
            writexl::write_xlsx(parto_ano_mes_t(), fname)
        }
    )
    
    # Nascimento
    nascimento_ano <- reactive({
        ind_nascimento %>% filter(Ano %in% input$ano_ind_filtro) %>% select(!Ano)
    })
    
    nascimento_ano_mes <- reactive({
        if(length(input$meses_ind) == 0){
            return()
        } else {
            nascimento_ano() %>% filter(Mes %in% input$meses_ind)
        }
    })
    
    nascimento_ano_mes_t <- reactive({
        if(length(input$meses_ind) == 0){
            return()
        } else {
            c <- pivot_wider(nascimento_ano_mes(), names_from = Mes, values_from = Quantidade)
            
            TOTAL <- c %>% summarise_if(is.numeric, sum, na.rm = TRUE)
            
            TOTAL <- data.frame(Nascimentos="TOTAL", TOTAL)
            
            rbind(c, TOTAL) %>% mutate(TOTAL = select_if(., is.numeric) %>%  rowSums(na.rm = TRUE))
        }
    })
    
    # Saida da tabela Internação Clientela
    output$nascimento_tab <- renderTable(
        
        nascimento_ano_mes_t(), digits = 0
        
    )
    
    # Download das consultas ambulatoriais
    output$download_nascimento <- downloadHandler(
        filename = function(){"tabela.xlsx"}, 
        content = function(fname){
            writexl::write_xlsx(nascimento_ano_mes_t(), fname)
        }
    )
    
### Procedimentos Cirúgicos ------------------------------------------------    

    # Cirurgias
    proc_cirur_ano <- reactive({
        proc_cirurg %>% filter(Ano %in% input$ano_ind_filtro) %>% select(!Ano)
    })
    
    proc_cirur_ano_mes <- reactive({
        if(length(input$meses_ind) == 0){
            return()
        } else {
            proc_cirur_ano() %>% filter(Mes %in% input$meses_ind)
        }
    })
    
    proc_cirur_ano_mes_t <- reactive({
        if(length(input$meses_ind) == 0){
            return()
        } else {
            a <- proc_cirur_ano_mes() %>% transmute(`Clínicas cirúrgicas`, total = select(., 2:5) %>% rowSums(na.rm = TRUE), Mes)
            c <- pivot_wider(a, names_from = Mes, values_from = total)
            
            TOTAL <- c %>% summarise_if(is.numeric, sum, na.rm = TRUE)
            
            TOTAL <- tibble(`Clínicas cirúrgicas`="TOTAL", TOTAL)
            
            rbind(c, TOTAL) %>% mutate(TOTAL = select_if(., is.numeric) %>%  rowSums(na.rm = TRUE))
        }
    })
    
    # Saida da tabela Internação Clientela
    output$cirur_tab <- renderTable(
        
        proc_cirur_ano_mes_t(), digits = 0
        
    )
    
    # Download das consultas ambulatoriais
    output$download_cirur <- downloadHandler(
        filename = function(){"tabela.xlsx"}, 
        content = function(fname){
            writexl::write_xlsx(proc_cirur_ano_mes_t(), fname)
        }
    )
    
    # Anestesias

    
})