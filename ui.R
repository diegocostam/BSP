#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Pacotes necessários

library(shiny) # Para criar o dashboard em Shiny
library(shinyWidgets) # Para criar filtros personalisados
library(shinythemes) # Inserir temas

# Define UI para aplicação
navbarPage("Mapas de Serviços Produzidos",
           # Link para RECURSOS HUMANOS
           tabPanel("RECURSOS HUMANOS",
                    sidebarLayout(
                        #Barra lateral com os filtros
                        sidebarPanel(width = 3,
                                     uiOutput("ano_rh"),
                                     pickerInput("meses_rh", h4("Selecione o(s) mês(es):"),
                                                 choices = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                                             "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"),
                                                 selected = "JAN",
                                                 multiple = FALSE),
                                     uiOutput("prof")
                                     ),
                        # Painel com as saídas
                        mainPanel(
                            tabsetPanel(
                                    tabPanel("Função técnica",
                                             tableOutput("tab_rh_tec"),
                                             downloadButton("download_rh_tec", "Baixar Planilha"),
                                             textOutput("text_obs")
                                             ),
                                    tabPanel("Função administrativa",
                                             tableOutput("tab_rh_adm"),
                                             downloadButton("download_rh_adm", "Baixar Planilha"),
                                             )
                                          )
                                       )
                               )
                    ),
           tabPanel("INSTALAÇÃO FÍSICA",
                    sidebarLayout(
                        sidebarPanel(width = 3,
                                     uiOutput("ano_if"),
                                     pickerInput("meses_infra", h4("Selecione o(s) mês(es):"),
                                                 choices = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                                             "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"),
                                                 selected = "JAN",
                                                 multiple = FALSE)
                        ),
                        mainPanel(
                            tabsetPanel(
                                tabPanel("Leitos hospitalares de internação",
                                         tableOutput("tab_infra_inter"),
                                         downloadButton("download_infra_inter", "Baixar Planilha")),
                                tabPanel("Salas/Consultórios/Leitos sem internação",
                                         tableOutput("tab_infra_salas"),
                                         downloadButton("download_infra_salas", "Baixar Planilha")),
                                tabPanel("Leitos para hospital dia",
                                         tableOutput("tab_infra_hospdia"),
                                         downloadButton("download_infra_hospdia", "Baixar Planilha"))
                            )
                        )
                    )
                    ),
           # Link da ASSISTÊNCIA AMBULATORIAL
             tabPanel("ASSISTÊNCIA AMBULATORIAL",
                 fluidPage(theme = shinytheme("cerulean"),

    # Sidebar com os filtros
    sidebarLayout(
        # Cria a caixa de seleção dos filtros.
        sidebarPanel(width = 3,
        # Aba do atendimento ambulatorial
        conditionalPanel(condition = "input.tabselected==1",
                         uiOutput("ano_aa"),
        uiOutput("clinica"),
        pickerInput("meses", h4("Selecione o(s) mês(es):"),
                    choices = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"),
                    selected = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                 "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"),
                    options = list(`actions-box` = TRUE),multiple = T),
        checkboxInput("checkbox", label = "Mostrar meses em colunas", value = TRUE),
        uiOutput("clientela")
     
        ),
        # Aba dos índices ambulatorias
        conditionalPanel(condition = "input.tabselected==2",
                         uiOutput("ano_ia"),
                         uiOutput("pmc"))),

        # Mostra as Tabelas e demais saidas na tela principal
        mainPanel(
            tabsetPanel(id = "tabselected",
                # Aba do atendimento ambulatorial
                #navbarMenu("Atendimento ambulatorial",
                tabPanel("Clínica e serviços", value = 1,
                         # Gera a tabela do atendimento ambulatorial   
                         tableOutput("aten_amb_tab"),
                         # Botão para download dos dados
                         downloadButton("download", "Baixar Planilha")), # Se reativar navbarMenu, acrescentar parênteses nessa linha.
                #tabPanel("Clientela",
                         # Gera a tabela do atendimento por clientela
                         #tableOutput("cli_tab"),
                         # Botão download dados clientela
                         #downloadButton("download_clientela", "Baixar Planilha"))),   
                # Aba dos índices ambulatoriais
                navbarMenu("Índices Laboratoriais",
                tabPanel("Pareceres ambulatoriais", value = 2,
                         # Gera a tabela do número de pareceres ambulatoriais
                         tableOutput("par_amb_tab"),
                         # Botão download dados do número de pareceres ambulatoriais
                         downloadButton("download_par_amb", "Baixar Planilha")),
                tabPanel("Prazo de marcação de consultas", value = 2,
                         # Gera a tabela do prazo de marcação de consultas
                         tableOutput("pmc_tab"),
                         # Botão download dados do prazo de marcação de consultas
                         downloadButton("download_pmc", "Baixar Planilha")),
                tabPanel("Absenteísmo", value = 2,
                         # Gera a tabela do absenteísmo
                         tableOutput("abs_tab"),
                         # Botão download dados do absenteísmo
                         downloadButton("download_abs", "Baixar Planilha")),
            ))
        )
    )
)),
                tabPanel("INTERNAÇÃO",
                         sidebarLayout(
                             sidebarPanel(width = 3,
                                          conditionalPanel(condition = "input.tabselected_int==1",
                                                           uiOutput("ano_int_clientela"),
                                                           pickerInput("meses_inter", h4("Selecione o(s) mês(es):"),
                                                                       choices = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                                                                   "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"),
                                                                       selected = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                                                                    "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"),
                                                                       options = list(`actions-box` = TRUE),multiple = T),
                                                           uiOutput("clinica_int_clientela"),
                                                           uiOutput("clientela_int")
                                          ),
                                          conditionalPanel(condition = "input.tabselected_int==2",
                                                           uiOutput("ano_tipo_inter"),
                                                           pickerInput("meses_tipo_inter", h4("Selecione o(s) mês(es):"),
                                                                       choices = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                                                                   "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"),
                                                                       selected = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                                                                    "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"),
                                                                       options = list(`actions-box` = TRUE),multiple = T)
                                          )
                             ),
                             mainPanel(
                                 tabsetPanel(
                                     id = "tabselected_int",
                                     tabPanel("Clínicas e Serviços", value = 1,
                                              tableOutput("int_cli_tab"),
                                              downloadButton("download_int_cli", "Baixar Planilha")
                                              ),
                                     tabPanel("Tipos de Internação", value = 2,
                                              tableOutput("int_tipo_tab"),
                                              downloadButton("download_int_tipo", "Baixar Planilha")
                                              )
                                 )
                             )
                         )),
                tabPanel("INDICADORES HOSPITALARES",
                         sidebarLayout(
                             sidebarPanel(width = 3,
                                          conditionalPanel(condition = "input.tabselected_ind==1",
                                                           uiOutput("ano_ind"),
                                                           pickerInput("meses_ind", h4("Selecione o(s) mês(es):"),
                                                                       choices = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                                                                   "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"),
                                                                       selected = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                                                                    "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"),
                                                                       options = list(`actions-box` = TRUE),multiple = T)
                                          ),
                                          conditionalPanel(condition = "input.tabselected_ind==2",
                                                           uiOutput("ano_ind_cir"),
                                                           pickerInput("meses_ind", h4("Selecione o(s) mês(es):"),
                                                                       choices = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                                                                   "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"),
                                                                       selected = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                                                                    "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"),
                                                                       options = list(`actions-box` = TRUE),multiple = T))#,
                                                           #uiOutput("clinica_cirurgia")
                             ),
                             mainPanel(
                                 tabsetPanel(
                                     id = "tabselected_ind",
                                     tabPanel("Indicadores de Internação", value = 1,
                                              tableOutput("ind_tab"),
                                              downloadButton("download_ind", "Baixar Planilha")
                                     ),
                                     tabPanel("Outros indicadores de internação", value = 1,
                                              tableOutput("oii_tab"),
                                              downloadButton("downloas_oii", "Baixar Planilha")
                                     ),
                                     tabPanel("Indicadores hospitalares", value = 1,
                                              tableOutput("ih_tab"),
                                              downloadButton("download_ih", "Baixar Planilha")
                                     ),
                                     tabPanel("Partos e Nascimentos", value = 1,
                                              tableOutput("parto_tab"),
                                              downloadButton("download_parto", "Baixar Planilha"),
                                              tableOutput("nascimento_tab"),
                                              downloadButton("download_nascimento", "Baixar Planilha")
                                     ),
                                     tabPanel("Procedimentos Cirúrgicos", value = 1,
                                              tableOutput("cirur_tab"),
                                              downloadButton("download_cirur", "Baixar Planilha")
                                     )
                                 )
                             )
                         )
                         )
)
