

# Causas mal definidas -------------------------------------------------------------------------


# abrindo arquivo com todos os obitos antes da correção

library(readr)
base_mortalidade_antesdacorrecao <- read_csv("PIBIC/_SCHENIA_2023_2024/4_analise da base/base_mortalidade_antesdacorrecao.csv", show_col_types = FALSE)
colnames(base_mortalidade_antesdacorrecao)


## Para calcular o percentual de óbitos por sexo, tipologia e ano para a causa
#"101-103 SINT, SIN E ACH ANORM CLÍN E LAB, NCOP", você pode seguir os seguintes passos:

# 1 - Filtrar os dados para a causa específica.
# 2 - Agrupar os dados por sexo, tipologia e ano, e somar os óbitos para cada grupo.
# 3 - Calcular o percentual dos óbitos para cada grupo.

library(dplyr)
library(tidyr)

# Filtra os dados para a causa específica
dados_filtrados <- base_mortalidade_antesdacorrecao %>%
  filter(`Causa - CID-BR-10` == "101-103 SINT, SIN E ACH ANORM CLÍN E LAB, NCOP")

# Calcula o total de óbitos
soma_obitos_total <- dados_filtrados %>%
  summarise(across(starts_with('<1'):starts_with('80 anos e mais'), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "faixa_etaria", values_to = "obitos_total") %>%
  summarise(total_obitos = sum(obitos_total))

# Agrupa e calcula os percentuais por sexo, tipologia e ano
percentuais <- dados_filtrados %>%
  group_by(sexo, tipologia, ano) %>%
  summarise(across(starts_with('<1'):starts_with('80 anos e mais'), sum, na.rm = TRUE), .groups = 'drop') %>%
  pivot_longer(cols = starts_with('<1'):starts_with('80 anos e mais'), names_to = "faixa_etaria", values_to = "obitos") %>%
  group_by(sexo, tipologia, ano) %>%
  summarise(total_obitos = sum(obitos), .groups = 'drop') %>%
  mutate(percentual = (total_obitos / soma_obitos_total$total_obitos) * 100)

# Exibe os resultados
print(percentuais)


# proporção de causas -FEMININO +++++++++++++++++++++++++++++++++++++++++++++++++++
library(dplyr)

# Filtra os dados para o sexo feminino
dados_feminino_novo <- base_mortalidade_antesdacorrecao %>%
  filter(sexo == "F")

# Calcula a proporção para 2018
proporcao_2018_f <- dados_feminino_novo %>%
  filter(ano == 2018) %>%
  group_by(Causa = `Causa - CID-BR-10`) %>%
  summarise(proporcao_2018 = sum(Total) / sum(dados_feminino_novo %>% filter(ano == 2018) %>% pull(Total)) * 100) 

# Calcula a proporção para 2021
proporcao_2021_f <- dados_feminino_novo %>%
  filter(ano == 2021) %>%
  group_by(Causa = `Causa - CID-BR-10`) %>%
  summarise(proporcao_2021 = sum(Total) / sum(dados_feminino_novo %>% filter(ano == 2021) %>% pull(Total)) * 100) 

# Junta as proporções
tabela_proporcoes_f <- left_join(proporcao_2018_f, proporcao_2021_f, by = "Causa")

# Agrupa causas com menos de 4%, exceto "104-113 CAUSAS EXTERNAS DE MORBIDADE E MORTALIDADE"
tabela_proporcoes_agrupada_f <- tabela_proporcoes_f %>%
  filter(proporcao_2018 >= 4 | proporcao_2021 >= 4 | Causa == "104-113 CAUSAS EXTERNAS DE MORBIDADE E MORTALIDADE") %>%
  bind_rows(tibble(Causa = "Outros",
                   proporcao_2018 = sum(tabela_proporcoes_f %>%
                                          filter(proporcao_2018 < 4 & Causa != "104-113 CAUSAS EXTERNAS DE MORBIDADE E MORTALIDADE") %>%
                                          pull(proporcao_2018)),
                   proporcao_2021 = sum(tabela_proporcoes_f %>%
                                          filter(proporcao_2021 < 4 & Causa != "104-113 CAUSAS EXTERNAS DE MORBIDADE E MORTALIDADE") %>%
                                          pull(proporcao_2021)))) %>%
  arrange(Causa)  # Ordena por ordem alfabética
tabela_proporcoes_agrupada_f # Exibe a tabela final



# proporção de causas - MASCULINO +++++++++++++++++++++++++++++++++++++++++++++++++++
library(dplyr)
# Filtra os dados para o sexo masculino
dados_masculino_novo <- base_mortalidade_antesdacorrecao %>%filter(sexo == "M")
# Calcula a proporção para 2018
proporcao_2018_m <- dados_masculino_novo %>%
  filter(ano == 2018) %>%
  group_by(Causa = `Causa - CID-BR-10`) %>%
  summarise(proporcao_2018 = sum(Total) / sum(dados_masculino_novo %>% filter(ano == 2018) %>% pull(Total)) * 100) 

# Calcula a proporção para 2021
proporcao_2021_m <- dados_masculino_novo %>%
  filter(ano == 2021) %>%
  group_by(Causa = `Causa - CID-BR-10`) %>%
  summarise(proporcao_2021 = sum(Total) / sum(dados_masculino_novo %>% filter(ano == 2021) %>% pull(Total)) * 100) 
# Junta as proporções
tabela_proporcoes_m <- left_join(proporcao_2018_m, proporcao_2021_m, by = "Causa")
# Agrupa causas com menos de 4%, exceto "104-113 CAUSAS EXTERNAS DE MORBIDADE E MORTALIDADE"
tabela_proporcoes_agrupada_m <- tabela_proporcoes_m %>%
  filter(proporcao_2018 >= 4 | proporcao_2021 >= 4 | Causa == "104-113 CAUSAS EXTERNAS DE MORBIDADE E MORTALIDADE") %>%
  bind_rows(tibble(Causa = "Outros",
                   proporcao_2018 = sum(tabela_proporcoes_m %>%
                                          filter(proporcao_2018 < 4 & Causa != "104-113 CAUSAS EXTERNAS DE MORBIDADE E MORTALIDADE") %>%
                                          pull(proporcao_2018)),
                   proporcao_2021 = sum(tabela_proporcoes_m %>%
                                          filter(proporcao_2021 < 4 & Causa != "104-113 CAUSAS EXTERNAS DE MORBIDADE E MORTALIDADE") %>%
                                          pull(proporcao_2021)))) %>%
  arrange(Causa)  # Ordena por ordem alfabética
tabela_proporcoes_agrupada_m # Exibe a tabela final




# Gráfico com as proporções de CMD - FEMININO ++++++++++++++++++++++++++++++++++++
# Carregar o pacote ggplot2 e RColorBrewer
library(ggplot2)
library(RColorBrewer)

# Definir a paleta de cores azul
cores_azul <- brewer.pal(n = 2, name = "YlOrBr")  # Paleta de azul do RColorBrewer

# Criar o gráfico com fundo branco e sem borda
ggplot(percentuais %>% filter(sexo == 'F'), aes(x = tipologia, y = percentual, fill = as.factor(ano))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", percentual)),
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            hjust = 0.5) +
  labs(x = "Tipologia",
       y = NULL,  # Remove o título do eixo y
       fill = "") +
  scale_fill_manual(values = cores_azul) +  # Aplicar as cores personalizadas
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA),  # Fundo branco e sem borda
        plot.background = element_rect(fill = "white", color = NA),    # Fundo branco e sem borda
        axis.title.y = element_blank(),  # Remove o título do eixo y
        axis.text.y = element_blank())    # Remove os rótulos do eixo y
        #panel.grid.major = element_blank(), # Remove as linhas de grade principais
        #panel.grid.minor = element_blank()) # Remove as linhas de grade secundárias




# Gráfico com as proporções de CMD - MASCULINO ++++++++++++++++++++++++++++++++++++

# Carregar o pacote ggplot2 e RColorBrewer
library(ggplot2)
library(RColorBrewer)

# Definir a paleta de cores azul
cores_azul <- brewer.pal(n = 2, name = "Blues")  # Paleta de azul do RColorBrewer

# Criar o gráfico com fundo branco e sem borda
ggplot(percentuais %>% filter(sexo == 'M'), aes(x = tipologia, y = percentual, fill = as.factor(ano))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", percentual)),
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            hjust = 0.5) +
  labs(x = "Tipologia",
       y = NULL,  # Remove o título do eixo y
       fill = "") +
  scale_fill_manual(values = cores_azul) +  # Aplicar as cores personalizadas
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA),  # Fundo branco e sem borda
        plot.background = element_rect(fill = "white", color = NA),    # Fundo branco e sem borda
        axis.title.y = element_blank(),  # Remove o título do eixo y
        axis.text.y = element_blank())    # Remove os rótulos do eixo y
        #panel.grid.major = element_blank(), # Remove as linhas de grade principais
        #panel.grid.minor = element_blank()) # Remove as linhas de grade secundárias




# Proporções das principais causas bem definidas ------------------------------


# base completa e corrigida: 
base_corrigida_mortalidade_trienio_2018_2021 <- read.csv("~/Mortalidade_no_semiarido/PIBIC/_SCHENIA_2023_2024/4_analise da base/base_corrigida_mortalidade_trienio_2018_2021.csv")
View(base_corrigida_mortalidade_trienio_2018_2021)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# tabela de Proporção dos Grupos de Causas para o Sexo Feminino no Semiárido +++++++++
library(dplyr)

# Filtra os dados para o sexo feminino
dados_feminino <- base_corrigida_mortalidade_trienio_2018_2021 %>%
  filter(sexo == "F")

# Calcula a proporção para 2018
proporcao_2018 <- dados_feminino %>%
  filter(ano == 2018) %>%
  group_by(Causa = `Causa...CID.BR.10`) %>%
  summarise(proporcao_2018 = sum(Total) / sum(dados_feminino %>% filter(ano == 2018) %>% pull(Total)) * 100) 

# Calcula a proporção para 2021
proporcao_2021 <- dados_feminino %>%
  filter(ano == 2021) %>%
  group_by(Causa = `Causa...CID.BR.10`) %>%
  summarise(proporcao_2021 = sum(Total) / sum(dados_feminino %>% filter(ano == 2021) %>% pull(Total)) * 100) 

# Junta as proporções
tabela_proporcoes <- left_join(proporcao_2018, proporcao_2021, by = "Causa")

# Agrupa causas com menos de 4%, exceto "104-113 CAUSAS EXTERNAS DE MORBIDADE E MORTALIDADE"
tabela_proporcoes_agrupada <- tabela_proporcoes %>%
  filter(proporcao_2018 >= 4 | proporcao_2021 >= 4 | Causa == "104-113 CAUSAS EXTERNAS DE MORBIDADE E MORTALIDADE") %>%
  bind_rows(tibble(Causa = "Outros",
                   proporcao_2018 = sum(tabela_proporcoes %>%
                                          filter(proporcao_2018 < 4 & Causa != "104-113 CAUSAS EXTERNAS DE MORBIDADE E MORTALIDADE") %>%
                                          pull(proporcao_2018)),
                   proporcao_2021 = sum(tabela_proporcoes %>%
                                          filter(proporcao_2021 < 4 & Causa != "104-113 CAUSAS EXTERNAS DE MORBIDADE E MORTALIDADE") %>%
                                          pull(proporcao_2021)))) %>%
  arrange(Causa)  # Ordena por ordem alfabética

# Exibe a tabela final
tabela_proporcoes_agrupada

# retirando a proporção de causas externas*********************************
library(dplyr)

# Filtra os dados para o sexo feminino
dados_feminino <- base_corrigida_mortalidade_trienio_2018_2021 %>%
  filter(sexo == "F") %>%
  mutate(Total = ifelse(`Causa...CID.BR.10` == "104-113 CAUSAS EXTERNAS DE MORBIDADE E MORTALIDADE", 0, Total))
# Calcula a proporção para 2018
proporcao_2018 <- dados_feminino %>%
  filter(ano == 2018) %>%
  group_by(Causa = `Causa...CID.BR.10`) %>%
  summarise(proporcao_2018 = sum(Total) / sum(dados_feminino %>% filter(ano == 2018) %>% pull(Total)) * 100)
# Calcula a proporção para 2021
proporcao_2021 <- dados_feminino %>%
  filter(ano == 2021) %>%
  group_by(Causa = `Causa...CID.BR.10`) %>%
  summarise(proporcao_2021 = sum(Total) / sum(dados_feminino %>% filter(ano == 2021) %>% pull(Total)) * 100)
tabela_proporcoes <- left_join(proporcao_2018, proporcao_2021, by = "Causa") # Junta as proporções
# Agrupa causas com menos de 4%
tabela_proporcoes_agrupada_f_semcausaexterna <- tabela_proporcoes %>%
  filter(proporcao_2018 >= 4 | proporcao_2021 >= 4) %>%
  bind_rows(tibble(Causa = "Outros",
                   proporcao_2018 = sum(tabela_proporcoes %>%
                                          filter(proporcao_2018 < 4) %>%
                                          pull(proporcao_2018)),
                   proporcao_2021 = sum(tabela_proporcoes %>%
                                          filter(proporcao_2021 < 4) %>%
                                          pull(proporcao_2021)))) %>%
  arrange(Causa)  # Ordena por ordem alfabética

# Exibe a tabela final
tabela_proporcoes_agrupada_f_semcausaexterna


# grafico de linha com os percentuais de causas externas - masculino ***************
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(stringr)

# Filtra os dados para o sexo masculino e para as Causas Externas
dados_masculino_externas <- base_corrigida_mortalidade_trienio_2018_2021 %>%
  filter(sexo == "F" & `Causa...CID.BR.10` == "104-113 CAUSAS EXTERNAS DE MORBIDADE E MORTALIDADE")

# Transforma os dados para long format
dados_long <- dados_masculino_externas %>%
  select(ano, starts_with("X")) %>%
  pivot_longer(cols = starts_with("X"), names_to = "faixa_etaria", values_to = "mortalidade") %>%
  mutate(faixa_etaria = recode(faixa_etaria,
                               `X.1` = "0 a 1 anos",
                               `X1.a.4.anos` = "1 a 4 anos",
                               `X5.a.9.anos` = "5 a 9 anos",
                               `X10.a.14.anos` = "10 a 14 anos",
                               `X15.a.19.anos` = "15 a 19 anos",
                               `X20.a.24.anos` = "20 a 24 anos",
                               `X25.a.29.anos` = "25 a 29 anos",
                               `X30.a.34.anos` = "30 a 34 anos",
                               `X35.a.39.anos` = "35 a 39 anos",
                               `X40.a.44.anos` = "40 a 44 anos",
                               `X45.a.49.anos` = "45 a 49 anos",
                               `X50.a.54.anos` = "50 a 54 anos",
                               `X55.a.59.anos` = "55 a 59 anos",
                               `X60.a.64.anos` = "60 a 64 anos",
                               `X65.a.69.anos` = "65 a 69 anos",
                               `X70.a.74.anos` = "70 a 74 anos",
                               `X75.a.79.anos` = "75 a 79 anos",
                               `X80.anos.e.mais` = "80 anos e mais")) %>%
  group_by(ano, faixa_etaria) %>%
  summarise(mortalidade = sum(mortalidade), .groups = "drop") %>%
  group_by(ano) %>%
  mutate(total_ano = sum(mortalidade)) %>%
  ungroup() %>%
  mutate(porcentagem = (mortalidade / total_ano) * 100) %>%
  mutate(faixa_etaria = str_replace(faixa_etaria, " anos", "")) # Remove o texto " anos"
# Define a ordem das faixas etárias
ordem_idades <- c("0 a 1", "1 a 4", "5 a 9", "10 a 14", "15 a 19",
                  "20 a 24", "25 a 29", "30 a 34", "35 a 39",
                  "40 a 44", "45 a 49", "50 a 54", "55 a 59",
                  "60 a 64", "65 a 69", "70 a 74", "75 a 79", 
                  "80 e mais")

# Converte a coluna faixa_etaria em um fator com a ordem definida
dados_long <- dados_long %>%
  mutate(faixa_etaria = factor(faixa_etaria, levels = ordem_idades))

# Definir a paleta de cores azul
cores_amarelo <- brewer.pal(n = 3, name = "YlOrBr")  # Paleta de azul do RColorBrewer
# Cria o gráfico de linha com paleta de cores 'Blues' e sem títulos nos eixos
ggplot(dados_long, aes(x = faixa_etaria, y = porcentagem, color = as.factor(ano), group = ano)) +
  geom_line(size = 1)  +
  geom_point() +
  scale_color_manual(values = c(cores_amarelo[2], cores_amarelo[3])) +
  scale_y_continuous(
    breaks = seq(0, max(dados_long$porcentagem, na.rm = TRUE), by = 2),
    labels = scales::percent_format(scale = 1)
  ) +  
  labs(
    title = NULL,
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Ajusta a leitura das faixas etárias



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# tabela de Proporção dos Grupos de Causas para o Sexo Masculino no Semiárido +++++++++
library(dplyr)

# Filtra os dados para o sexo masculino
dados_masculino <- base_corrigida_mortalidade_trienio_2018_2021 %>%
  filter(sexo == "M")

# Calcula a proporção para 2018
proporcao_2018_m <- dados_masculino %>%
  filter(ano == 2018) %>%
  group_by(Causa = `Causa...CID.BR.10`) %>%
  summarise(proporcao_2018 = sum(Total) / sum(dados_masculino %>% filter(ano == 2018) %>% pull(Total)) * 100) 
# Calcula a proporção para 2021
proporcao_2021_m <- dados_masculino %>%
  filter(ano == 2021) %>%
  group_by(Causa = `Causa...CID.BR.10`) %>%
  summarise(proporcao_2021 = sum(Total) / sum(dados_masculino %>% filter(ano == 2021) %>% pull(Total)) * 100) 
# Junta as proporções
tabela_proporcoes_m <- left_join(proporcao_2018_m, proporcao_2021_m, by = "Causa")

# Agrupa causas com menos de 4%, exceto "104-113 CAUSAS EXTERNAS DE MORBIDADE E MORTALIDADE"
tabela_proporcoes_agrupada_m <- tabela_proporcoes_m %>%
  filter(proporcao_2018 >= 4 | proporcao_2021 >= 4 | Causa == "104-113 CAUSAS EXTERNAS DE MORBIDADE E MORTALIDADE") %>%
  bind_rows(tibble(Causa = "Outros",
                   proporcao_2018 = sum(tabela_proporcoes_m %>%
                                          filter(proporcao_2018 < 4 & Causa != "104-113 CAUSAS EXTERNAS DE MORBIDADE E MORTALIDADE") %>%
                                          pull(proporcao_2018)),
                   proporcao_2021 = sum(tabela_proporcoes_m %>%
                                          filter(proporcao_2021 < 4 & Causa != "104-113 CAUSAS EXTERNAS DE MORBIDADE E MORTALIDADE") %>%
                                          pull(proporcao_2021)))) %>%
  arrange(Causa)  # Ordena por ordem alfabética
# Exibe a tabela final
tabela_proporcoes_agrupada_m

# retirando a causa externa ****************************************************
library(dplyr)

# Filtra os dados para o sexo masculino e ajusta o valor de óbito da causa externa para zero
dados_masculino <- base_corrigida_mortalidade_trienio_2018_2021 %>%
  filter(sexo == "M") %>%
  mutate(Total = ifelse(`Causa...CID.BR.10` == "104-113 CAUSAS EXTERNAS DE MORBIDADE E MORTALIDADE", 0, Total))

# Calcula a proporção para 2018
proporcao_2018 <- dados_masculino %>%
  filter(ano == 2018) %>%
  group_by(Causa = `Causa...CID.BR.10`) %>%
  summarise(proporcao_2018 = sum(Total) / sum(dados_masculino %>% filter(ano == 2018) %>% pull(Total)) * 100)
# Calcula a proporção para 2021
proporcao_2021 <- dados_masculino %>%
  filter(ano == 2021) %>%
  group_by(Causa = `Causa...CID.BR.10`) %>%
  summarise(proporcao_2021 = sum(Total) / sum(dados_masculino %>% filter(ano == 2021) %>% pull(Total)) * 100)
tabela_proporcoes <- left_join(proporcao_2018, proporcao_2021, by = "Causa") # Junta as proporções
# Agrupa causas com menos de 4%
tabela_proporcoes_agrupada_m_semcausaexterna <- tabela_proporcoes %>%
  filter(proporcao_2018 >= 4 | proporcao_2021 >= 4) %>%
  bind_rows(tibble(Causa = "Outros",
                   proporcao_2018 = sum(tabela_proporcoes %>%
                                          filter(proporcao_2018 < 4) %>%
                                          pull(proporcao_2018)),
                   proporcao_2021 = sum(tabela_proporcoes %>%
                                          filter(proporcao_2021 < 4) %>%
                                          pull(proporcao_2021)))) %>%
  arrange(Causa)  # Ordena por ordem alfabética
tabela_proporcoes_agrupada_m_semcausaexterna # Exibe a tabela final


# grafico de linha com os percentuais de causas externas - masculino ***************
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(stringr)

# Filtra os dados para o sexo masculino e para as Causas Externas
dados_masculino_externas <- base_corrigida_mortalidade_trienio_2018_2021 %>%
  filter(sexo == "M" & `Causa...CID.BR.10` == "104-113 CAUSAS EXTERNAS DE MORBIDADE E MORTALIDADE")

# Transforma os dados para long format
dados_long <- dados_masculino_externas %>%
  select(ano, starts_with("X")) %>%
  pivot_longer(cols = starts_with("X"), names_to = "faixa_etaria", values_to = "mortalidade") %>%
  mutate(faixa_etaria = recode(faixa_etaria,
                               `X.1` = "0 a 1 anos",
                               `X1.a.4.anos` = "1 a 4 anos",
                               `X5.a.9.anos` = "5 a 9 anos",
                               `X10.a.14.anos` = "10 a 14 anos",
                               `X15.a.19.anos` = "15 a 19 anos",
                               `X20.a.24.anos` = "20 a 24 anos",
                               `X25.a.29.anos` = "25 a 29 anos",
                               `X30.a.34.anos` = "30 a 34 anos",
                               `X35.a.39.anos` = "35 a 39 anos",
                               `X40.a.44.anos` = "40 a 44 anos",
                               `X45.a.49.anos` = "45 a 49 anos",
                               `X50.a.54.anos` = "50 a 54 anos",
                               `X55.a.59.anos` = "55 a 59 anos",
                               `X60.a.64.anos` = "60 a 64 anos",
                               `X65.a.69.anos` = "65 a 69 anos",
                               `X70.a.74.anos` = "70 a 74 anos",
                               `X75.a.79.anos` = "75 a 79 anos",
                               `X80.anos.e.mais` = "80 anos e mais")) %>%
  group_by(ano, faixa_etaria) %>%
  summarise(mortalidade = sum(mortalidade), .groups = "drop") %>%
  group_by(ano) %>%
  mutate(total_ano = sum(mortalidade)) %>%
  ungroup() %>%
  mutate(porcentagem = (mortalidade / total_ano) * 100) %>%
  mutate(faixa_etaria = str_replace(faixa_etaria, " anos", "")) # Remove o texto " anos"

# Define a ordem das faixas etárias
ordem_idades <- c("0 a 1", "1 a 4", "5 a 9", "10 a 14", "15 a 19",
                  "20 a 24", "25 a 29", "30 a 34", "35 a 39",
                  "40 a 44", "45 a 49", "50 a 54", "55 a 59",
                  "60 a 64", "65 a 69", "70 a 74", "75 a 79", 
                  "80 e mais")

# Converte a coluna faixa_etaria em um fator com a ordem definida
dados_long <- dados_long %>%
  mutate(faixa_etaria = factor(faixa_etaria, levels = ordem_idades))

# Definir a paleta de cores azul
cores_azul <- brewer.pal(n = 3, name = "Blues")  # Paleta de azul do RColorBrewer


# Cria o gráfico de linha com paleta de cores 'Blues' e sem títulos nos eixos
ggplot(dados_long, aes(x = faixa_etaria, y = porcentagem, color = as.factor(ano), group = ano)) +
  geom_line(size = 1)  +
  geom_point() +
  scale_color_manual(values = c(cores_azul[2], cores_azul[3])) +
  scale_y_continuous(
    breaks = seq(0, max(dados_long$porcentagem, na.rm = TRUE), by = 2),
    labels = scales::percent_format(scale = 1)
  ) +  
  labs(
    title = NULL,
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Ajusta a leitura das faixas etárias

#
# Analise condição de vida ----------------------------------------------------

library(readr)

condicao_vida <- read.csv("~/Mortalidade_no_semiarido/PIBIC/_SCHENIA_2023_2024/4_analise da base/basecompletacondição_de_vida_semiarido_2022.csv")
colnames(condicao_vida)
View(condicao_vida)
# Ajustar o modelo de regressão
modelo <- lm(taxa_bruta_mortalidade ~ taxa_analfabetismo + rede_de_esgoto +
             tx_banheiro + tx_coleta_lixo + 
               tx_rede_agua, data = condicao_vida)

# Exibir o resumo do modelo
summary(modelo)


# Carregar o pacote ggplot2
library(ggplot2)

# Criar o gráfico de dispersão
ggplot(condicao_vida, aes(x = rede_de_esgoto, y = taxa_bruta_mortalidade, color = Tipo)) +
  geom_point() +                         # Adicionar pontos
  labs(
    x = "Rede de Esgoto",
    y = "Taxa Bruta de Mortalidade",
    color = "Tipo"
  ) +
  theme_minimal()   


# Criar o gráfico de dispersão
ggplot(condicao_vida, aes(x = tx_banheiro, y = taxa_bruta_mortalidade)) +
  geom_point() +                         # Adicionar pontos
  labs(
    x = "Rede de Esgoto",
    y = "Taxa Bruta de Mortalidade"
  ) +
  theme_minimal()   



# Carregar os pacotes necessários
library(ggplot2)
library(dplyr)

# Substituir os nomes das tipologias
condicao_vida <- condicao_vida %>%
  mutate(Tipo = recode(Tipo,
                       "IntermediarioAdjacente" = "Intermediário Adjacente",
                       "IntermediarioRemoto" = "Intermediário Remoto",
                       "RuralAdjacente" = "Rural Adjacente",
                       "RuralRemoto" = "Rural Remoto",
                       "Urbano" = "Urbano"))

# Carregar os pacotes necessários
library(ggplot2)
library(RColorBrewer)

# Criar uma paleta personalizada com 5 cores da paleta "YlGnBu"
ylgnbu_palette <- brewer.pal(9, "Blues")[4:8]

# Criar o gráfico de dispersão
ggplot(condicao_vida, aes(x = rede_de_esgoto, y = taxa_bruta_mortalidade, color = Tipo)) +
  geom_point() +  # Adicionar pontos
  labs(
    x = "Proporção de domicílios com rede de esgoto",
    y = "Taxa bruta de mortalidade",
    color = "Tipologia"
  ) +
  facet_wrap(~ Tipo, nrow = 1) +  # Colocar os gráficos lado a lado
  scale_color_manual(values = ylgnbu_palette) +  # Aplicar a paleta "YlGnBu"
  theme_minimal() +
  theme(
    legend.position = "bottom"  # Mover a legenda para baixo do eixo x
  )










