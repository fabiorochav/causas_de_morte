
# instalando pacote
#install.packages("remotes")
#remotes::install_github("rfsaldanha/microdatasus")

# carregando pacote
library(microdatasus)
library(dplyr)
library(tidyverse)

# coletando dados

dados = fetch_datasus(year_start = 2000, 
                      year_end = 2023, 
                      uf = "MG", 
                      information_system = "SIM-DO")

# Processando os dados
dados_processo = process_sim(dados)

# filtrar cid relativa
dados_processo_filter = dados_processo %>%
  filter(grepl("314610", CODMUNOCOR))

dados_contado = dados_processo_filter %>%
  mutate(ano = year(DTOBITO)) %>%
  count(ano, CAUSABAS)

dados_ordenados = dados_contado[order(-dados_contado$ano, -dados_contado$n), ]

top_10_por_ano <- dados_ordenados %>%
  group_by(ano) %>%
  slice_head(n = 3) %>%
  mutate(percentual = (n / sum(n))*100)

ggplot(top_10_por_ano, aes(x = ano, y = percentual)) +
  geom_col(aes(fill = CAUSABAS, color = CAUSABAS)) +
  theme_minimal()+
  labs(title = "Principais causas de morte - Ouro Preto (2010-2021)",
       subtitle = "CID - Classificação Internacional de Doenças",
       caption = "Elaboração: Fábio Rocha - Dados: Datasus",
       y = "%",
       x = "")

# Dicionário de mapeamento de códigos CID para significados reais
cid_significados <- c("I64" = "Acidente vascular cerebral (AVC)",
                      "R99" = "Causa não especificada",
                      "J189" = "Pneumonia, causa não especificada",
                      "I219" = "Infarto agudo do miocárdio",
                      "I10" = "Hipertensão arterial",
                      "E149" = "Diabetes mellitus não especificado",
                      "J159" = "Pneumonia bacteriana não especificada",
                      "C349" = "Neoplasia maligna de brônquios e pulmão",
                      "Y349" = "Evento adverso após procedimento cirúrgico não especificado",
                      "A419" = "Septicemia não especificada",
                      "I694" = "Fibrilação e flutter atriais",
                      "I110" = "Hipertensão arterial primária",
                      "B342" = "Infecção por coronavírus de localização não especificada")

# Substituir códigos CID pelos significados reais
top_10_por_ano <- top_10_por_ano %>%
  mutate(CAUSABAS = ifelse(CAUSABAS %in% names(cid_significados), cid_significados[CAUSABAS], CAUSABAS))

# Gerar o gráfico de colunas com os significados reais
ggplot(top_10_por_ano, aes(x = ano, y = percentual)) +
  geom_col(aes(fill = CAUSABAS)) +
  theme_minimal() +
  labs(title = "Principais causas de morte - Ouro Preto (2010-2021)",
       subtitle = "CID - Classificação Internacional de Doenças",
       caption = "Elaboração: Fábio Rocha - Dados: Datasus",
       y = "%",
       x = "") +
  theme(legend.position = "right") +
  guides(fill = guide_legend(title = NULL))


