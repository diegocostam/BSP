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
dia_mes <- data.frame()

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
  
  # Número de dias no mês
  # Pega do anexo A5
  dm <- read_excel(data_names[i], sheet = 5, range = "P2:P2", col_names = FALSE) %>% rename(d_m = `...1`)
  
  dm$Mes <- substr(sub(".*V - ", "", data_names[i]), 1 , 3)
  dm$Ano <- substr(sub(".*V - ", "", data_names[i]), 5 , 8)
  
  dia_mes <- rbind(dia_mes, dm)
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


taxas <- ind_inter %>% filter(Indicadores %in% c("ÓBITOS", "ALTAS", "TRANSFERÊNCIAS EXTERNAS")) %>%
  group_by(Mes, Ano) %>% summarise(saidas=sum(Valores)) %>% left_join(ind_inter_outros %>% filter(Tipo %in% c("Nº DE ÓBITOS APÓS 24 h DE INTERNAÇÃO")) %>%  group_by(Mes, Ano) %>% summarise(int24obt=sum(Valor))) %>% left_join(ind_inter_outros %>% filter(Tipo %in% "Nº DE ÓBITOS POR CAUSAS MATERNAS") %>% select(obtMater = Valor, Mes, Ano)) %>% left_join(saida_obst) %>% left_join( ind_nascimento %>% filter(Nascimentos %in% "Nº DE NASCIDOS VIVOS") %>% select(nasc_vivo = Quantidade, Mes, Ano)) %>%
  left_join(ind_nascimento %>% filter(Nascimentos %in% "Nº DE NASCIDOS MORTOS") %>% select(nasc_morto = Quantidade, Mes, Ano)) %>% left_join(ind_inter_outros %>% filter(Tipo %in% "Nº DE ÓBITOS DE CRIANÇAS (Até 24 h de nascido)") %>% select(obt24cri = Valor, Mes, Ano)) %>% left_join(ind_inter_outros %>% filter(Tipo %in% "Nº DE INFECÇÕES HOSPITALARES") %>% select(inf_hosp = Valor, Mes, Ano)) %>% left_join(ind_inter_outros %>% filter(Tipo %in% "Nº DE ÓBITOS OPERATÓRIOS(Até 10 dias da cirurgias)") %>% select(obt10ope = Valor, Mes, Ano)) %>% left_join(proc_cirurg %>% group_by(Mes, Ano) %>% summarise(gde = sum(GDE., na.rm = TRUE), med = sum(MED., na.rm = TRUE), peq = sum(PEQ., na.rm = TRUE), pro_cir_amb = sum(`Procedimentos cirúrgicos ambulatoriais`, na.rm = TRUE)) %>% transmute(tot_proc_cir = gde + med + peq + pro_cir_amb, Mes, Ano)) %>% left_join(ind_inter_outros %>% filter(Tipo %in% "Nº DE ÓBITOS POR ANESTESIA") %>% select(obt_anest = Valor, Mes, Ano)) %>% left_join(anestesia %>% group_by(Mes, Ano) %>% summarise(GERAL = sum(GERAL, na.rm = TRUE), LOCAL = sum(LOCAL, na.rm = TRUE), BLOQUEIO = sum(BLOQUEIO, na.rm = TRUE), OUTRAS = sum(OUTRAS, na.rm = TRUE)) %>% transmute(tot_anest = GERAL + LOCAL + BLOQUEIO + OUTRAS, Mes, Ano)) %>% left_join(ind_parto %>% filter(Partos %in% "Nº DE PARTOS CESÁRIOS") %>% select(cesaria = Quantidade, Mes, Ano)) %>% left_join(ind_parto %>% group_by(Mes, Ano) %>% summarise(tot_partos = sum(Quantidade))) %>% left_join(ind_inter %>% filter(Indicadores %in% "P/D") %>% select(p_d = Valores, Mes, Ano)) %>% left_join(dia_mes) %>% left_join(ind_inter %>% filter(Indicadores %in% "Nº DE PARECERES NAS INTERNAÇÕES") %>% select(parecer_int = Valores, Mes, Ano)) %>% transmute(Mes, Ano, `TAXA DE MORTALIDADE INSTITUCIONAL (T X MI)` = int24obt/saidas*100, `TAXA DE MORTALIDADE NATERNA (T X MM)` = obtMater/said_obt*100, `TAXA DE NATIMORTLIDADE (T X N)` = nasc_morto/nasc_vivo*100, `TAXA DE MORTALIDADE NEONATAL (T X MN)` = obt24cri/nasc_vivo*100, `TAXA DE INFECÇÃO HOSPITALAR GLOBAL (T X IHG)` = inf_hosp/saidas*100, `TAXA DE MORTALIDADE OPERATÓRIA (T X MO)` = obt10ope/tot_proc_cir*100, `TAXA DE MORTALIDADE POR ANESTESIA (T X MA)` = obt_anest/tot_anest*100, `TAXA DE CESÁRIAS (T X C)` = cesaria/tot_partos*100, `MÉDIA DE CENSO DIÁRIO (MCD)` = p_d/d_m, `TAXA DE PARECERES (T X P)` = parecer_int/saidas*100)

