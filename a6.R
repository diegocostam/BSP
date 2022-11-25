# Pacotes necessários

library(dplyr) # Manipulação de dados
library(magrittr) # Utilização do pipe (%>%)
library(readxl) # Leitura de arquivos do Excel
library(tidyr) # Pivotagem dos dados
library(janitor) # Padronização dos nomes das variáveis
library(stringr) # Manipular strings

# Entrando com os Anexos, manipulando e juntado os dados.

# Nomes dos arquivos Anexo
data_names <- list.files(pattern = "Anexo A")

# Outros indicadores de internacão ---------------------------------------

ind_inter_outros <- data.frame()
ind_parto <- data.frame()
ind_nascimento <- data.frame()

for (i in 1:length(data_names)) {
  
  data_all <- read_excel(data_names[i], sheet = 6, skip = 3, col_names = FALSE)
  
  # Outros indicadores de internação
  oii <- data_all %>% select(1,7) %>% slice(1:4) %>% rename(Tipo = `...1`, Valor = `...7`) %>% 
    mutate(Tipo = substring(Tipo, regexpr("N", Tipo)))
  oii2 <- data_all %>% select(10,17) %>% slice(1:3) %>% rename(Tipo = `...10`, Valor = `...17`) %>% 
    mutate(Tipo = substring(Tipo, regexpr("N", Tipo)))
  
  oii_all <- rbind(oii,oii2)
  
  oii_all$Mes <- substr(sub(".*V - ", "", data_names[i]), 1 , 3)
  oii_all$Ano <- substr(sub(".*V - ", "", data_names[i]), 5 , 8)
  
  ind_inter_outros <- rbind(ind_inter_outros, oii_all)
  
  # Partos
  parto <- data_all %>% select(1,6) %>% slice(18:19) %>% rename(Partos = `...1`, Quantidade = `...6`) %>% 
    mutate(Partos = substring(Partos, regexpr("N", Partos)))
  
  parto$Mes <- substr(sub(".*V - ", "", data_names[i]), 1 , 3)
  parto$Ano <- substr(sub(".*V - ", "", data_names[i]), 5 , 8)
  
  ind_parto <- rbind(ind_parto, parto)
  
  # Nascimentos
  nascimento <- data_all %>% select(9,17) %>% slice(18:19) %>% rename(Nascimentos = `...9`, Quantidade = `...17`) %>% 
    mutate(Nascimentos = substring(Nascimentos, regexpr("N", Nascimentos)))
  
  nascimento$Mes <- substr(sub(".*V - ", "", data_names[i]), 1 , 3)
  nascimento$Ano <- substr(sub(".*V - ", "", data_names[i]), 5 , 8)
  
  ind_nascimento <- rbind(ind_nascimento, nascimento)
  
}

# Fazendo ajustes nos dados
ind_inter_outros <- ind_inter_outros %>%
  mutate(Mes = factor(Mes, levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                      "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"))) %>%
  arrange(Mes)

ind_parto <- ind_parto %>%
  mutate(Mes = factor(Mes, levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                      "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"))) %>%
  arrange(Mes)

ind_nascimento <- ind_nascimento %>%
  mutate(Mes = factor(Mes, levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                      "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"))) %>%
  arrange(Mes)


# Indicadores hospitalares ------------------------------------------------

# Precisam de informações do anexo A5 e A6 para o cálculo das taxas
# Usa o arquivo ind_inter

# Saídas obstetrícia

saida_obst <- data.frame()

for (i in 1:length(data_names)) {
  
  data <- read_excel(data_names[i], sheet = 5, range = "O26:Q26", col_names = FALSE)
  data$Mes <- substr(sub(".*V - ", "", data_names[i]), 1 , 3)
  data$Ano <- substr(sub(".*V - ", "", data_names[i]), 5 , 8)
  
  data <- data %>% mutate(said_obt = rowSums(select(.,`...1`,`...2`,`...3`), na.rm = TRUE)) %>% select(said_obt, Mes, Ano)
  
  saida_obst <- rbind(saida_obst, data)
  
}

# Fazendo ajustes nos dados
saida_obst <- saida_obst %>%
  mutate(Mes = factor(Mes, levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                      "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"))) %>%
  arrange(Mes)

# Taxa de mortalidade institucional (TxMI)



# ind_inter %>% 
#   filter(Indicadores %in% c("ÓBITOS", "ALTAS", "TRANSFERÊNCIAS EXTERNAS")) %>%
#   group_by(Mes, Ano) %>% summarise(saidas=sum(Valores)) %>% left_join(ind_inter_outros %>%
#                                                                         filter(Tipo %in% c("Nº DE ÓBITOS APÓS 24 h DE INTERNAÇÃO")) %>%  group_by(Mes, Ano) %>% summarise(int24obt=sum(Valor))) %>% left_join(saida_obst) %>% left_join( ind_nascimento %>% filter(Nascimentos %in% "Nº DE NASCIDOS VIVOS") %>% select(nasc_vivo = Quantidade, Mes, Ano)) %>% 
#   left_join( ind_nascimento %>% filter(Nascimentos %in% "Nº DE NASCIDOS MORTOS") %>% select(nasc_morto = Quantidade, Mes, Ano)) %>% left_join(ind_inter_outros %>% filter(Tipo %in% "Nº DE ÓBITOS DE CRIANÇAS (Até 24 h de nascido)") %>% select(obt24cri = Valor, Mes, Ano)) %>% left_join(ind_inter_outros %>% filter(Tipo %in% "Nº DE INFECÇÕES HOSPITALARES") %>% select(inf_hosp = Valor, Mes, Ano)) %>% left_join(ind_inter_outros %>% filter(Tipo %in% "Nº DE ÓBITOS OPERATÓRIOS(Até 10 dias da cirurgias)") %>% select(obt10ope = Valor, Mes, Ano))


  
  
  
  

 # mutate(`TAXA DE MORTALIDADE INSTITUCIONAL (TxMI)`=int24obt/saidas*100)