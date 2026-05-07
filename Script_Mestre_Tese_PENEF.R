# ==============================================================================
# TESE DE DOUTORADO - SCRIPT MESTRE DE ANÁLISE DE DADOS
# Objetivo: Operacionalização das Dimensões Institucional, Contextual e Micropolítica
# Capítulos: 4 (Seleção de Casos) e 5 (Integração, Geoprocessamento e Matriz Contraste)
# ==============================================================================

# ==============================================================================
# BLOCO ZERO: SETUP DO AMBIENTE E DIRETÓRIO DE TRABALHO (REPRODUTIBILIDADE)
# ==============================================================================

# 1. Lista de pacotes necessários para rodar todas as rotinas da tese
pacotes_necessarios <- c("tidyverse", "geobr", "sf", "ggspatial", "prettymapr")

# 2. Verifica ausência de pacotes no ambiente local e instala automaticamente
pacotes_faltantes <- pacotes_necessarios[!(pacotes_necessarios %in% installed.packages()[,"Package"])]
if(length(pacotes_faltantes) > 0) {
  install.packages(pacotes_faltantes)
}

# 3. Carregamento do ecossistema de dados e geoprocessamento
library(tidyverse)
library(geobr)
library(sf)
library(ggspatial)

# 4. Definição do diretório de trabalho local (Atenção: alterar para a pasta real do PC)
# setwd("C:/Caminho/Para/Sua/Pasta/Bases_Tese")


# ==============================================================================
# SCRIPT 01: CAPÍTULO 4 - SELEÇÃO DE CASOS (MOST DIFFERENT CASES - MDC)
# Objetivo: Filtragem quantitativa e seleção das 4 unidades escolares contrastantes
# ==============================================================================

# 1. Importação da matriz de dados consolidada das escolas (IVS, Censo e BCB)
base_escolas <- read_csv("IVS_Escolas.csv")

# 2. Definição dos quartis de contraste (Vulnerabilidade vs. Engajamento)
# Criação de categorias estruturais para isolar os extremos da distribuição
escolas_categorizadas <- base_escolas %>%
  mutate(
    perfil_vulnerabilidade = case_when(
      inse_escola <= quantile(inse_escola, 0.25, na.rm = TRUE) ~ "Alta Vulnerabilidade",
      inse_escola >= quantile(inse_escola, 0.75, na.rm = TRUE) ~ "Baixa Vulnerabilidade",
      TRUE ~ "Intermediário"
    ),
    perfil_engajamento = case_when(
      taxa_engajamento_penef >= quantile(taxa_engajamento_penef, 0.75, na.rm = TRUE) ~ "Alto Engajamento",
      taxa_engajamento_penef <= quantile(taxa_engajamento_penef, 0.25, na.rm = TRUE) ~ "Baixo Engajamento",
      TRUE ~ "Intermediário"
    )
  )

# 3. Seleção empírica das 4 unidades de análise (Casos Contrastantes)
# Isolamento das escolas municipais do Recife nos quadrantes extremos (Alto x Baixo)
amostra_mdc <- escolas_categorizadas %>%
  filter(rede_dependencia == "Municipal", municipio == "Recife") %>%
  filter(perfil_vulnerabilidade != "Intermediário" & perfil_engajamento != "Intermediário") %>%
  # Recorte das unidades nominais para a investigação qualitativa documental
  filter(nome_escola %in% c("EM Padre José de Anchieta", 
                            "EM Três Carneiros", 
                            "EM Diná de Oliveira", 
                            "EM Casa dos Ferroviários")) %>%
  select(codigo_inep, nome_escola, perfil_vulnerabilidade, perfil_engajamento)

# 4. Exportação da amostra base para o Apêndice e Relatório
write_csv(amostra_mdc, "Amostra_4_Escolas_MDC.csv")


# ==============================================================================
# SCRIPT 02: CAPÍTULO 5 - OPERACIONALIZAÇÃO E CRUZAMENTO DOS MICRODADOS
# Objetivo: Integração de bases (5.1 e 5.2), Geoprocessamento (5.3) e Matriz Contraste (5.4)
# ==============================================================================

# ------------------------------------------------------------------------------
# 5.1 e 5.2 IMPORTAÇÃO E CRUZAMENTO RELACIONAL DAS BASES (MERGE)
# ------------------------------------------------------------------------------

# Importação dos microdados brutos do INEP e do Banco Central (Aprender Valor)
dados_inep_recife <- read_csv("INES_2023_Escolas_Recife.csv")
dados_bcb_engajamento <- read_csv("2023_Engajamento_Recife_ens_fundamental.csv")

# Data Wrangling: Padronização e remoção de valores nulos na chave primária
dados_bcb_limpos <- dados_bcb_engajamento %>%
  drop_na(codigo_inep) %>%
  mutate(codigo_inep = as.numeric(codigo_inep))

# Unificação utilizando o Código INEP como Chave Primária (Primary Key)
base_integrada_cap5 <- dados_inep_recife %>%
  inner_join(dados_bcb_limpos, by = "codigo_inep") %>%
  inner_join(amostra_mdc, by = "codigo_inep") %>% # Mantém os perfis do MDC
  mutate(adesao_efetiva = ifelse(taxa_engajamento_penef > 0, 1, 0))

# Exportação do dataset integrado (Apenas Casos Estudados)
write_csv(base_integrada_cap5, "Base_Integrada_4Escolas_PENEF.csv")


# ------------------------------------------------------------------------------
# 5.3 GEORREFERENCIAMENTO E MODELAGEM CARTOGRÁFICA (TERRITORIALIZAÇÃO)
# ------------------------------------------------------------------------------

# Extração da malha de setores censitários do Recife via IBGE (Fundo geográfico)
recife_setores <- read_census_tract(code_tract = 2611606, year = 2010, showProgress = FALSE)

# Estruturação espacial das 4 unidades escolares com as coordenadas reais
escolas_coord <- data.frame(
  nome_escola = c("EM Padre José de Anchieta", 
                  "EM Três Carneiros", 
                  "EM Diná de Oliveira", 
                  "EM Casa dos Ferroviários"),
  lon = c(-34.915, -34.948, -34.922, -34.960), # Coordenadas Longitude (Ajustar p/ exatas)
  lat = c(-8.075, -8.121, -8.033, -8.085)      # Coordenadas Latitude (Ajustar p/ exatas)
)

# Cruzamento do perfil de engajamento para a legenda do mapa
escolas_coord_perfil <- escolas_coord %>%
  inner_join(amostra_mdc, by = "nome_escola")

# Conversão do dataframe para formato Simple Features (sf) - CRS 4674 (SIRGAS 2000)
escolas_sf <- st_as_sf(escolas_coord_perfil, coords = c("lon", "lat"), crs = 4674)

# Plotagem do Mapa Realista (Integração ggplot2 e ggspatial)
mapa_realista_recife <- ggplot() +
  # Camada base: Malha urbana e ruas via OpenStreetMap
  annotation_map_tile(type = "osm", zoomin = 0) +
  # Camada secundária: Setores censitários suavizados (cinza transparente)
  geom_sf(data = recife_setores, fill = NA, color = "gray40", linewidth = 0.1, alpha = 0.4) +
  # Camada principal: Marcadores das escolas (Cores baseadas no Perfil de Engajamento)
  geom_sf(data = escolas_sf, aes(fill = perfil_engajamento), color = "white", size = 4, shape = 21, stroke = 0.8) +
  # Estética visual padronizada (ABNT)
  theme_minimal() +
  scale_fill_manual(values = c("Alto Engajamento" = "blue", "Baixo Engajamento" = "red")) +
  labs(
    title = "Distribuição Espacial das Unidades de Análise (MDC)",
    subtitle = "Casos de Alto e Baixo Engajamento na Rede Municipal do Recife",
    caption = "Fonte: Elaboração própria via R (geobr e ggspatial).",
    fill = "Perfil de Engajamento"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10)
  )

# Salvar o artefato cartográfico em PNG com alta resolução (300 dpi)
ggsave("Mapa_Realista_Refinado_Cap5.png", plot = mapa_realista_recife, width = 8, height = 6, dpi = 300)


# ------------------------------------------------------------------------------
# 5.4 ESTATÍSTICA DESCRITIVA E CONSTRUÇÃO DA META-MATRIZ DE CONTRASTE (CROSS-CASE)
# ------------------------------------------------------------------------------

# Geração de sumário descritivo base por perfil de vulnerabilidade/engajamento
sumario_engajamento <- base_integrada_cap5 %>%
  group_by(perfil_engajamento, perfil_vulnerabilidade) %>%
  summarise(
    taxa_media_engajamento = mean(taxa_engajamento_penef, na.rm = TRUE),
    total_formacoes_bc = sum(formacoes_concluidas, na.rm = TRUE), # Variável hipotética extraída do BCB
    total_estudantes_avaliados = sum(estudantes_avaliados, na.rm = TRUE), # Idem
    .groups = "drop"
  )

write_csv(sumario_engajamento, "Sumario_Estatistico_Cap5.csv")

# ESTRUTURAÇÃO DA META-MATRIZ DE CONTRASTE (PIVOTAGEM)
# Objetivo: Transformar as linhas das 4 escolas em colunas (Lado a Lado) 
# para a leitura comparativa das dimensões (Cross-Case Analysis)

matriz_contraste <- base_integrada_cap5 %>%
  # 1. Seleção exclusiva das variáveis teóricas de interesse da tese
  select(
    nome_escola,
    perfil_engajamento,
    perfil_vulnerabilidade,
    taxa_engajamento_penef,
    ideb_escola,           # Variável contexto INEP
    ivs_territorio,        # Variável contexto IPEA
    tx_esforco_docente     # Variável agência INEP/Censo Escolar
  ) %>%
  # 2. Conversão de todas as métricas para formato texto (para padronizar o pivot)
  mutate(across(-nome_escola, as.character)) %>%
  # 3. Pivotagem Longa: agrupa todas as variáveis teóricas em uma única coluna 'dimensao_analitica'
  pivot_longer(
    cols = -nome_escola,
    names_to = "dimensao_analitica",
    values_to = "valor_registrado"
  ) %>%
  # 4. Pivotagem Larga: transforma o nome de cada escola em uma coluna própria
  pivot_wider(
    names_from = nome_escola,
    values_from = valor_registrado
  ) %>%
  # 5. Organização da visualização final
  arrange(dimensao_analitica)

# Exportação da Meta-Matriz Analítica pronta para ser colada no Word da Tese
write_csv(matriz_contraste, "Meta_Matriz_Contraste_MDC.csv")

# ==============================================================================
# FIM DA ROTINA COMPUTACIONAL
# ==============================================================================