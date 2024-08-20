

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
       fill = "Ano") +
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
       fill = "Ano") +
  scale_fill_manual(values = cores_azul) +  # Aplicar as cores personalizadas
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA),  # Fundo branco e sem borda
        plot.background = element_rect(fill = "white", color = NA),    # Fundo branco e sem borda
        axis.title.y = element_blank(),  # Remove o título do eixo y
        axis.text.y = element_blank())    # Remove os rótulos do eixo y
        #panel.grid.major = element_blank(), # Remove as linhas de grade principais
        #panel.grid.minor = element_blank()) # Remove as linhas de grade secundárias




# Proporções das principais causas bem definidas ------------------------------

base_corrigida_mortalidade_trienio_2018_2021 <- read.csv("~/Mortalidade_no_semiarido/PIBIC/_SCHENIA_2023_2024/4_analise da base/base_corrigida_mortalidade_trienio_2018_2021.csv")
View(base_corrigida_mortalidade_trienio_2018_2021)


# Analise condição de vida ----------------------------------------------------

library(readr)

condicao_vida <- read.csv("~/Mortalidade_no_semiarido/PIBIC/_SCHENIA_2023_2024/4_analise da base/basecompletacondição_de_vida_semiarido_2022.csv")
colnames(condicao_vida)

# Ajustar o modelo de regressão
modelo <- lm(taxa_bruta_mortalidade ~ taxa_analfabetismo + rede_de_esgoto , data = condicao_vida)

# Exibir o resumo do modelo
summary(modelo)


















