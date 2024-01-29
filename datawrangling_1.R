## Data Wrangling R - Tidyverse
## MBA DSA USP ESALQ

install.packages("tidyverse")
install.packages("data.table")
install.packages("stringr")

library(tidyverse)
library(data.table)
library(kableExtra)
library(stringr)

#--------------------Importar os datasets---------------------------------------
# "dataset_inicial" - LOA - Orçamento da União

path_file <- 'LOA_PPA_PLOA_detalhada.xlsx'
dataset_inicial <- read_excel(path_file)
abas_arquivo <- excel_sheets(path_file)

#cada aba do arquivo transformada em arquivo .csv para melhor análise
for (aba in abas_arquivo) {
  nome_arquivo_csv <- paste0(aba, ".csv")
  dados_aba <- read_excel(path_file, sheet = aba)
  write.csv(dados_aba, file = nome_arquivo_csv, row.names = FALSE)
}

# carregamento do dataset para entendimento

df_receita_orcamento_md <- read_csv("Receitas_Un.Orcam_MD_AnexoI.csv")

df_receita_orcamento_md %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 12)

#--------------------Entendimento do Dataset-----------------------------------------------
#estrutura da base de dados
glimpse(df_receita_orcamento_md)
#dimensões do dataset: linhas e colunas
dim(df_receita_orcamento_md)
#nomes das variáveis (colunas)
names(df_receita_orcamento_md)

#visualizar para entendimento a soma dos valores das receitas da DEFESA. 
print(df_receita_orcamento_md %>% filter(FA == "DEFESA") %>% summarise(media = sum(VALOR)))


# Gráfico de Barras - Exibir a distribuição dos valores orçamentários (VALOR) por órgão orçamentário (ORGAO ORCAMENTARIO).
top7_orgaos <- df_receita_orcamento_md %>%
  group_by(`ORGAO ORCAMENTARIO`) %>%
  summarise(Total = sum(VALOR)) %>%
  arrange(desc(Total)) %>%
  top_n(7, Total)

ggplot(top7_orgaos, aes(x = reorder(`ORGAO ORCAMENTARIO`, Total), y = Total)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(title = "Top 7 Órgãos Orçamentários por Valor Total", x = "Órgão Orçamentário", y = "Valor Total Orçamentário")

# Gráfico de Linhas -  Analisar a tendência de valores orçamentários (VALOR) ao longo do tempo (EXERCICIO FINANCEIRO).
ggplot(df_receita_orcamento_md, aes(x = `EXERCICIO FINANCEIRO`, y = VALOR, group = FA, color = FA)) +
  geom_line() +
  labs(title = "Tendência de Valores Orçamentários por Ano e Força Armada", x = "Exercício Financeiro", y = "Valor")

#Gráfico de Dispersão - Explorar a relação entre as variáveis SKU e VALOR.
ggplot(df_receita_orcamento_md, aes(x = SKU, y = VALOR)) +
  geom_point(aes(color = FA)) +
  labs(title = "Relação entre SKU e Valor Orçamentário", x = "SKU", y = "Valor")

# Criando o resumo do DataFrame
df_sum <- df_receita_orcamento_md %>% 
  group_by(FA) %>% 
  summarise(Total = sum(VALOR))

# Gráfico de pizza
ggplot(df_sum, aes(x = "", y = Total, fill = FA)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(fill = "Força Armada", title = "Proporção Orçamentária por Força Armada")


#--------------------Formatação------------------------------------------------------------
#função rename para alterar os nomes das variáveis

format_df__receita_md <- df_receita_orcamento_md %>% 
  rename(Referencia = `SKU REF.`, 
         Codigo = SKU,
         Receita = VALOR)

# Para remover o objeto, caso não seja mais necessário. 
# rm(df_receita_orcamento_md)  

View(format_df__receita_md)

format_df__receita_md <- format_df__receita_md %>% 
  mutate(Codigo = as.character(Codigo))
glimpse(format_df__receita_md)

unique(format_df__receita_md$FA)
unique(format_df__receita_md$Referencia)
unique(format_df__receita_md$`ORGAO ORCAMENTARIO`)
unique(format_df__receita_md$TITULO)
unique(format_df__receita_md$FONTE)
unique(format_df__receita_md$`EXERCICIO FINANCEIRO`)

# formatação da coluna FONTE para resumo das informações
format_df__receita_md <- format_df__receita_md %>% 
  mutate(FONTE = case_when(
    str_detect(FONTE, "LDO-2023") ~ "LDO-2023",
    str_detect(FONTE, "LDO-2022") ~ "LDO-2022",
    str_detect(FONTE, "LDO-2021") ~ "LDO-2021",
    str_detect(FONTE, "LDO-2020") ~ "LDO-2020",
    str_detect(FONTE, "LDO-2019") ~ "LDO-2019",
    str_detect(FONTE, "LDO-2018") ~ "LDO-2018",
    str_detect(FONTE, "LDO-2017") ~ "LDO-2017",
    str_detect(FONTE, "LDO-2016") ~ "LDO-2016",
    TRUE ~ FONTE
  ))

# Exclusão das colunas REFERENCIA e TITULO pois todas as linhas contém a mesma informação.            
format_df__receita_md <-  format_df__receita_md %>% 
  select(-c(Referencia, TITULO ))

View(format_df__receita_md)

format_df__receita_md %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 12)

group_FA <- group_by(format_df__receita_md, FA)
desc_FA_Receita <- group_FA %>% 
  summarise(observações = n(),
            média = mean(Receita),
            mediana = median(Receita),
            soma = sum(Receita),
            maximo = max(Receita),
            minimo = min(Receita)
            ) %>% 
  arrange(desc(maximo))

# desc_group <-  format_df__receita_md %>% 
#   group_by(FA, `EXERCICIO FINANCEIRO`) %>% 
#   summarise(media_receita = mean(Receita),
#             soma = sum(Receita),
#             máximo = max(Receita),
#             mínimo = min(Receita),
#             contagem = n()) %>% 
#   arrange(desc(máximo))

desc_FA_Receita %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 12)

# desc_group %>%
#   kable() %>%
#   kable_styling(bootstrap_options = "striped",
#                 full_width = F,
#                 font_size = 12)

# table(format_df__receita_md$FA)
# format_df__receita_md %>% count(FA)
# freq_FA <- as.data.frame(table(format_df__receita_md$`ORGAO ORCAMENTARIO`,
#                                format_df__receita_md$FA
#                                ))
# freq_FA

freq_Orcament <- format_df__receita_md %>% 
  count(`ORGAO ORCAMENTARIO`, FA, name = 'contagem')
freq_Orcament %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 12)

#filtro de exercício financeiro a partir de 2022, e Receita maior que a média do Exército, agrupado pela Força Armada
format_df__receita_md %>% 
  group_by(FA) %>% 
  filter(`EXERCICIO FINANCEIRO`>= 2022 & Receita > 483513773) %>% 
  ungroup()

# As 10% maiores receitas
format_df__receita_md %>%  slice_max(order_by = Receita, prop = 0.10)
# As 10% menores receitas
format_df__receita_md %>%  slice_min(order_by = Receita, prop = 0.10)

format_df__receita_md[format_df__receita_md$`EXERCICIO FINANCEIRO` >= 2022, c(1, 6)]

# importação e formatação da tabela de despesa orçamento MD
df_despesa_md <- read_csv("Despesa_por_Un._TipoI.csv")
#View(df_despesa_md)

f_despesa_md <- df_despesa_md %>% 
  rename(Codigo = SKU, 
         Despesa = TOTAL, 
         `Despesa 2-9` = `TOTAL 2-9`)
f_despesa_md <- f_despesa_md %>% 
  mutate(Codigo = as.character(Codigo))
glimpse(f_despesa_md)
f_despesa_md <-  f_despesa_md %>% 
  select(-c(`SKU REF.`, FA , `ORGAO ORCAMENTARIO`, TITULO, FONTE ))
#

md_receita_despesa %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 12)
#--------------------Join-------------------------------------------------------

md_receita_despesa <- left_join(format_df__receita_md, f_despesa_md, by = c("Codigo", "EXERCICIO FINANCEIRO"))
view(md_receita_despesa)

#--------------------Iterações com Purrr----------------------------------------
# Configurar R para evitar notação científica
options(scipen = 999)
vetor_variaveis_selecionadas <- c("Receita", "Despesa", "Despesa 2-9")
# O argumento na.rm = T solicita a remoção de NAs antes de fazer as contas (T é TRUE)
# O ~ indica que trata-se de uma função, ou seja, escreveremos uma função
estatisticas_orcamento <-  map(md_receita_despesa[vetor_variaveis_selecionadas], ~ summary(.x, na.rm = TRUE))
estatisticas_orcamento  

#--------------------Tratamento de Valores Faltantes----------------------------------------

qtd_na <- md_receita_despesa %>% summarise(across(everything(), ~ sum(is.na(.))))
print(qtd_na)

linhas_na <- md_receita_despesa %>% 
  filter(if_any(everything(), is.na))
View(linhas_na)

View( md_receita_despesa %>% 
  filter(if_all(everything(), ~ . == "")))

md_receita_despesa <- md_receita_despesa %>% 
  filter(complete.cases(.))
view(md_receita_despesa)
# ---------------------- Enriquecimento -------------------------------

df_enriquecido <- md_receita_despesa %>% 
  mutate(
    `Categoria Receita` = cut(Receita / sum(Receita, na.rm = TRUE), 
                      breaks = quantile(Receita / sum(Receita), probs = 0:5/5),
                      labels = c("muito baixo", "baixo", "medio", "alto", "muito alto"),
                      include.lowest = TRUE),
    `Categoria Despesa` = cut(Despesa / sum(Despesa, na.rm = TRUE), 
                      breaks = quantile(Despesa / sum(Despesa), probs = 0:5/5, na.rm = TRUE),
                      labels = c("muito baixo", "baixo", "médio", "alto", "muito alto"),
                      include.lowest = TRUE)
        )
View(df_enriquecido)

write_csv(format_df__receita_md, "df_receita_md.csv")
write_csv(f_despesa_md, "df_despesa_md.csv")
write_csv(md_receita_despesa, "orcamento_md.csv")
