library(readxl); library(plm); library(dplyr); library(stargazer); library(ggplot2)
library(correlation); library(stringr); library(tictoc); library(janitor)
library(tidyr); library(purrr); library(ggpmisc); library(ggsci); library(furrr)
library(patchwork)

setwd("/home/abdinardo/Dropbox/00 Artigo_Israel_Panel_Data")

main_dir <- getwd()
arquivos <- paste0(main_dir,"/",list.files(main_dir,pattern="^BD\\w+.xlsx$"))
bd_com <- read_excel(arquivos[1],sheet=1,col_names=T) |> clean_names()
rm(arquivos,main_dir)

tic()
plan(multisession, workers = 3)
bd_com_corr_all <- bd_com |>
  select(-c(2:4)) |>
  split(~empresa) |>
  future_map_dfr(\(df){
    correlation(df,method = "pearson",verbose=F) |>
      mutate(Group = unique(df$empresa)) |>
      relocate(Group,.before = Parameter1)
  },.progress = T)
plan(sequential)
toc()

print(glue::glue("Total de correlações: {nrow(bd_com_corr_all)}"))

combinacoes <- crossing(
  Sinal = c("positive","negative"),
  VD = c("errowin","mepawin","disperwin"),
  VI = c("esg_score_win","esg_comb_win","env_pillar_win","social_pillar_win","gov_pillar_win")
)
lista_corr_signal <- pmap(combinacoes,\(Sinal,VD,VI){
  if(Sinal == "negative"){
    filter(bd_com_corr_all, r < 0, p < 0.1, Parameter1 == VD, Parameter2 == VI)
  }else{
    filter(bd_com_corr_all, r > 0, p < 0.1, Parameter1 == VD, Parameter2 == VI)
  }
})
names(lista_corr_signal) <- str_c(combinacoes$Sinal,combinacoes$VD,combinacoes$VI,sep = "_")

table_corr_signal <- map_dfr(lista_corr_signal,\(df){return(df)}) |>
  mutate(
    R2 = round(100*r^2,2),
    correlacao = ifelse(r < 0, "Negative Correlation", "Positive Correlation"),
    Parameter2 = factor(str_remove_all(Parameter2,"_win"))
  )

print(glue::glue("Total de empresas que apresentaram correlações significativas: {length(unique(table_corr_signal$Group))}"))

cores <- c("#5050FFFF","#CE3D32FF","#749B58FF","#F0E685FF","#466983FF","#BA6338FF")
names(cores) <- unique(table_corr_signal$Parameter2)
tamanho_eixos <- 7

counts <- table_corr_signal |>
  group_by(Parameter2, correlacao, Parameter1) |>
  summarise(n_obs = n(),y_med_r = max(r, na.rm = TRUE)*1.01,
            y_med_r2 = max(R2, na.rm = TRUE)*1.01,.groups = "drop")

P1 <- table_corr_signal |>
  ggplot(aes(x = Parameter2, y = r, fill = Parameter2)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.6) +
  geom_jitter(color = "black", alpha = .5, width = 0.25, height = 0,size=1.1) +
  geom_text(data = counts,
            aes(x = Parameter2, y = y_med_r, label = n_obs, group = Parameter2),
            color = "red", fontface = "bold", size = 4, inherit.aes = FALSE, position = position_jitter(width=0.25,height=0,seed=123)) +
  scale_fill_manual(values = cores) +
  labs(x = "ESG indexes",y = "Correlation values",fill = "",
       caption = "Numbers in red are sample size.") +
  scale_y_continuous(n.breaks = 5,expand = expansion(mult = c(0.05, 0.15)),
                     labels = scales::label_number(accuracy = 0.01,big.mark=",")
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size=tamanho_eixos,face="bold"),
    axis.title = element_text(size=tamanho_eixos,face="bold"),
    strip.text = element_text(size=tamanho_eixos,face="bold"),
    legend.position = "none"
  ) +
  facet_wrap(~ correlacao + Parameter1, scales = "free_y")

P2 <- table_corr_signal |>
  ggplot(aes(x = Parameter2, y = R2, fill = Parameter2)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.6) +
  geom_jitter(color = "black", alpha = .5, width = 0.25, height = 0,size=1.1) +
  geom_text(data = counts,
            aes(x = Parameter2, y = y_med_r2, label = n_obs, group = Parameter2),
            color = "red", fontface = "bold", size = 4, inherit.aes = FALSE, position = position_jitter(width=0.25,height=0,seed=123)) +
  scale_fill_manual(values = cores) +
  labs(x = "ESG indexes",y = "Coefficient of determination (R²) values",fill = "",
       caption = "Numbers in red are sample size.") +
  scale_y_continuous(n.breaks = 5,expand = expansion(mult = c(0.05, 0.15)),
                     labels = scales::label_number(accuracy = 1,big.mark=",")
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size=tamanho_eixos,face="bold"),
    axis.title = element_text(size=tamanho_eixos,face="bold"),
    strip.text = element_text(size=tamanho_eixos,face="bold"),
    legend.position = "none"
  ) +
  facet_wrap(~ correlacao + Parameter1, scales = "free_y")

x11(); P1
x11(); P2

lista_modelos <- list()
modelos <- c("pooling","within","between","random")
modelos_random <- c("swar","walhus","nerlove")
variaveis_controle <- colnames(bd_com)[13:21]
lista_equacoes <- pmap(combinacoes[-1],\(VD,VI){
  formula(paste0(VD," ~ ",paste(c(VI,variaveis_controle[c(7)]),collapse = "+")))
})
names(lista_equacoes) <- str_c(combinacoes$Sinal,combinacoes$VD,combinacoes$VI,sep = "_")
nomes <- names(lista_equacoes)

for(equacao in names(lista_equacoes)){
  for(modelo in modelos){
    if(modelo != "random"){
      lista_modelos[paste0(equacao,"_",modelo)] <- list(
        try(plm(lista_equacoes[[equacao]],
                filter(bd_com,empresa %in% lista_corr_signal[[equacao]]$Group),
                model = modelo,index = c('empresa', 'ano_do_exercicio')),silent = T)
      )
    }else{
      for(modelo_random in modelos_random){
        lista_modelos[paste0(equacao,"_",modelo,"_",modelo_random)] <-list(
          try(plm(lista_equacoes[[equacao]],
                  filter(bd_com,empresa %in% lista_corr_signal[[equacao]]$Group),
                  model = modelo,random.method = modelo_random,index = c('empresa', 'ano_do_exercicio')),
              silent = T)
        )
      }
    }
  }
}


library(modelsummary)
modelsummary(
  resposta1,
  fmt = NULL,
  output = "default",
  estimate = "{round(estimate,3)}{stars}",
  statistic = "p = {round(p.value,3)}",
  stars = TRUE
)

x11(); G25

bd <- read.delim(
  file = "https://github.com/Abdinardo/ESG_Paper/raw/main/ESG_data/BD_ESG_WIN.csv",
  sep = ";"
) |> clean_names()


library(imputeTS)

empresas <- sort(unique(bd$ticket))
# "ZOREN.IS"     "032500.KQ"
x <- bd |> filter(ticket == "026960.KS")


bd_corrigido <- bd |>
 split(~ticket) |>
 map_dfr(\(x){
  
 df <- arrange(x,year)
 print(unique(df$ticket))

 #browser()
 
 if(all(is.na(df$qanalyst))){
  df$qanalyst <- 0
 } else if(any(is.na(df$qanalyst))){
  resposta <- try(as.integer(
   round(
    na_seadec(
     zoo::zoo(df$qanalyst,order.by = as.Date(paste0(df$year,"-01-01"))),
     algorithm = "interpolation",find_frequency = T),0)))
  
  if (class(resposta)=="character"||class(resposta)=="try-error"){
   resposta <- try(as.integer(round(na_mean(zoo::zoo(df$qanalyst,order.by = paste0(df$year,"-01-01") ) ),0) ))
  }
  df$qanalyst <- resposta 
   
   
   
   
 }

 
 return(df)


})


BD <- NULL
for (i in 1:length(bd_corrigido)) BD <- bind_rows(BD,bd_corrigido[[i]])

write.csv(bd_corrigido,"BD_ESG_WIN_FULL.csv",row.names = F)

abdi <- read.csv("BD_ESG_WIN_FULL.csv")

##############################################################################

library(dplyr)
library(tidyr)
library(gt)
library(gtExtras)
library(forcats)

# 1. Filtrar os dados
dados <- bd |>
  filter(ticket %in% tbl_companies$ticket) |>
  select(-c(1:3)) |>
  rename(
   `escore esg` = esg_score,
   `esg combinado` = esg_comb,
   ambiental = env_pillar,
   social = social_pillar,
   governança = gov_pillar,
   `qtde analistas` = qanalyst
  )

# 2. Converter para formato longo
dados_long <- dados |>
  pivot_longer(
    cols = everything(),
    names_to = "Variavel",
    values_to = "Valor"
  )

# 3. Criar tabela estatística
tabela <- dados_long |>
  group_by(Variavel) |>
  summarise(
    Minimo   = min(Valor, na.rm = TRUE),
    Q1       = quantile(Valor, 0.25, na.rm = TRUE),
    Mediana  = median(Valor, na.rm = TRUE),
    Q3       = quantile(Valor, 0.75, na.rm = TRUE),
    Maximo   = max(Valor, na.rm = TRUE),
    Media    = mean(Valor, na.rm = TRUE),
    DP       = sd(Valor, na.rm = TRUE),
    CV       = DP / Media,
    N        = sum(!is.na(Valor)),
    
    # coluna necessária para o gtExtras
    Distribuicao = list(Valor),
    
    .groups = "drop"
  ) |>
  mutate(
    ordem = case_when(
      Variavel == "disper"      ~1,
      Variavel == "mape"        ~2,
      Variavel == "escore esg"   ~3,
      Variavel == "esg combinado"    ~4,
      Variavel == "qtde analistas" ~ 99,
      TRUE ~ 10
    )
  ) |> arrange(ordem)

# 4. Criar tabela acadêmica com gráfico de densidade
tabela_final <- tabela |> select(-ordem) |>
  gt() |>
  fmt_number(
    columns = where(is.numeric),
    decimals = 3,
    drop_trailing_zeros = TRUE
  ) |>
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 18
  ) |>
  gtExtras::gt_plt_dist(
    column = Distribuicao,
    type = "histogram",
    trim = F,
    bw = function(x) diff(range(x)) / 15,
    fig_dim = c(8,30),
    same_limit = F
  ) |>
  cols_label(
    Variavel     = "Variável",
    Minimo       = "Mínimo",
    Q1           = "1º Quartil",
    Mediana      = "Mediana",
    Q3           = "3º Quartil",
    Maximo       = "Máximo",
    Media        = "Média",
    DP           = "DP",
    CV           = "CV",
    N            = "N",
    Distribuicao = "Histograma"
  )
tabela_final

setwd("/home/abdinardo/Dropbox/00 Artigo_Israel_Panel_Data/")

#gt::gtsave(tabela_final, "tabela_academica.html")
gt::gtsave(tabela_final, "tabela_academica.png", zoom = 2)

ggsave(
  "figura_2.png",
  plot = Plot2,
  width = 16,
  height = 10,
  units = "cm",
  dpi = 600,
  bg = "white"
)

library(ggstatsplot)

D1 <- bd_corr_chosen |>
 filter(status == "Correlação negativa",P1 == "disper")

p <- ggbetweenstats(
  data = D1,
  x = P2,
  y = r,
  type = "nonparametric"
)

p +
  ggplot2::scale_fill_manual(
    values = c("#5050FFFF","#CE3D32FF","#749B58FF","#F0E685FF","#466983FF")
  ) +
  ggplot2::geom_jitter(
    color = "gray50",
    width = 0.15,
    alpha = 0.6,
    size = 2
  )

library(dunn.test)

D1 <- bd_corr_chosen |> filter(status == "Correlação negativa",P1 == "disper")
dunn.test::dunn.test(x = D1$r,g = D1$P2,method = "holm",table = T,interpret = T)

############################################################################################
# Carregar a biblioteca
library(tidyverse)
library(klaR)

# Supondo que 'dados_esg' seja o seu data frame com as 10 métricas
# Remova a coluna com o nome da empresa antes de rodar o modelo

dados_esg <- vroom::vroom("negativas_codificadas.csv") |>
 janitor::clean_names() |>
 dplyr::select(-c(2,13)) |>
 # mutate(
 #  across(is.numeric,as.factor) 
 # ) |>
 as.data.frame()

dados_para_cluster <- dados_esg[, -1] 

# Converter para fator para garantir que o algoritmo trate como categoria
# dados_para_cluster[] <- lapply(dados_para_cluster, as.factor)

# Executar o K-modes (exemplo com 3 grupos)
set.seed(123) # Para resultados reproduzíveis
modelo_kmodes <- kmodes(dados_para_cluster, modes = 11, iter.max = 10)

# Ver os resultados
print(modelo_kmodes)
sort(table(modelo_kmodes$cluster),decreasing = T)

filter(dados_esg,cluster == 11) |> View()

# Adicionar o grupo de volta à sua tabela original
dados_esg$cluster <- modelo_kmodes$cluster

library(ggplot2)
library(cluster)

# 1. Definir o intervalo de clusters que queremos testar
max_k <- 13
total_withindiff <- numeric(max_k)
sil_width <- numeric(max_k)


# 2. Rodar o loop para calcular o erro (withindiff) para cada K
set.seed(123) # Para consistência
for (i in 2:max_k) {
  modelo <- kmodes(dados_para_cluster, modes = i, iter.max = 10)
  total_withindiff[i] <- sum(modelo$withindiff)
  
  # Usando daisy para gerar distâncias categóricas
  dist_matrix <- daisy(dados_para_cluster, metric = "gower")
  sil <- silhouette(modelo$cluster, dist_matrix)
  sil_width[i-1] <- mean(sil[,3])
}

plot(2:13, sil_width[1:12], type="b")


# 3. Criar um data frame para o plot
df_cotovelo <- data.frame(k = 1:max_k, erro = total_withindiff)

x11(); ggplot(df_cotovelo, aes(x = k, y = erro)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue", size = 3) +
  scale_x_continuous(breaks = 1:max_k) +
  labs(title = "Método do Cotovelo - K-Modes ESG",
       x = "Número de Grupos (K)",
       y = "Soma das Diferenças Internas (Total Withindiff)") +
  theme_minimal()

library(FactoMineR)
library(factoextra)


# 3. Executar a ACM
res_mca <- MCA(dados_para_cluster, graph = FALSE)

# Gráfico das Categorias (Métricas): Mostra quais métricas ESG "andam junta
x11();fviz_mca_var(res_mca, repel = TRUE, ggtheme = theme_minimal())

# Gráfico das Empresas (Indivíduos): Empresas próximas no mapa têm o mesmo padrão de destaque ESG.
x11();fviz_mca_ind(res_mca, label = "none", habillage = 1,addEllipses = TRUE, ggtheme = theme_minimal())

# Importância das Variáveis: Veja quais métricas mais explicam a variação dos dados.
x11();fviz_screeplot(res_mca, addlabels = TRUE) # Gráfico de variância explicada

summary(res_mca)

####################################################################################
# 1. Carregar as bibliotecas necessárias
library(klaR)    # Para o K-modes
library(cluster) # Para a distância de Gower (função daisy)
library(uwot)    # Para o UMAP
library(ggplot2) # Para visualização

# 2. Criar dados de exemplo (categóricos)
set.seed(42)
dados <- data.frame(
  cor = as.factor(sample(c("Vermelho", "Azul", "Verde"), 200, replace = TRUE)),
  tamanho = as.factor(sample(c("P", "M", "G"), 200, replace = TRUE)),
  material = as.factor(sample(c("Algodão", "Seda", "Lã"), 200, replace = TRUE))
)

# 3. Executar o K-modes (ex: 3 clusters)
modelo_km <- kmodes(dados, modes = 3)
dados$cluster <- as.factor(modelo_km$cluster)

# 4. Calcular a matriz de distância de Gower
# Essencial para que o UMAP entenda a diferença entre as categorias
dist_gower <- daisy(dados[, 1:3], metric = "gower")

# 5. Aplicar o UMAP sobre a matriz de distância
# 'n_neighbors' controla o equilíbrio entre estrutura local e global
umap_res <- umap(dist_gower, n_neighbors = 15, min_dist = 0.1)

# 6. Preparar dados para o gráfico
df_plot <- data.frame(
  V1 = umap_res[,1], 
  V2 = umap_res[,2], 
  Cluster = dados$cluster
)

# 7. Visualizar
x11(); ggplot(df_plot, aes(x = V1, y = V2, color = Cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Visualização UMAP dos Clusters K-modes",
       x = "Dimensão 1", y = "Dimensão 2")

##############################################################################
# 4. Calcular a matriz de distância de Gower
# Essencial para que o UMAP entenda a diferença entre as categorias
dist_gower <- daisy(dados_para_cluster, metric = "gower")

# 5. Aplicar o UMAP sobre a matriz de distância
# 'n_neighbors' controla o equilíbrio entre estrutura local e global
umap_res <- umap(dist_gower, n_neighbors = 15, min_dist = 0.1, metric = "manhattan")

# 6. Preparar dados para o gráfico
df_plot <- data.frame(
  V1 = umap_res[,1], 
  V2 = umap_res[,2], 
  Nome = dados_esg$ticket,
  Cluster = as.factor(modelo_kmodes$cluster)
)

# 7. Visualizar
x11(); ggplot(df_plot, aes(x = V1, y = V2, label = Nome, color=Cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_text(vjust=1.5)+
  theme_minimal() +
  labs(title = "Visualização UMAP dos Clusters K-modes",
       x = "Dimensão 1", y = "Dimensão 2")
####################################################################################
umap_res1 <- umap(dados_para_cluster, n_neighbors = 15, min_dist = 0.1, metric = "manhattan")

# 6. Preparar dados para o gráfico
df_plot <- data.frame(
  V1 = umap_res1[,1], 
  V2 = umap_res1[,2], 
  Nome = dados_esg$ticket,
  Cluster = as.factor(modelo_kmodes$cluster)
)

# 7. Visualizar
x11(); ggplot(df_plot, aes(x = V1, y = V2, label = Nome, color=Cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_text(vjust=1.5)+
  theme_minimal() +
  labs(title = "Visualização UMAP dos Clusters K-modes",
       x = "Dimensão 1", y = "Dimensão 2")

####################################################################################
map_dbl(metodo_cluster,\(x){
  maxSE(gap_stat_nc$Tab[, "gap"], gap_stat_nc$Tab[, "SE.sim"], method = x)  
  
}) #|> DescTools::Mode() |> as.numeric() |> min()

map_dbl(metodo_cluster,\(x){
  maxSE(gap_stat_pc$Tab[, "gap"], gap_stat_pc$Tab[, "SE.sim"], method = x)  
  
}) #|> DescTools::Mode() |> as.numeric() |> min()

g_nc_pc <- G_nc / G_pc

ggsave(
  "figura_3.png",
  plot = g_nc_pc,
  width = 16,
  height = 10,
  units = "cm",
  dpi = 600,
  bg = "white"
)



#####################################################################################

negativas_codificadas |>
  summarise(across(where(is.integer), sum, na.rm = TRUE),.by = cluster) |> 
  pivot_longer(-cluster,names_to = "ESG",values_to = "Freq") |>
  mutate(Prop = round(Freq / sum(Freq)*100,1),.by = cluster)
  
positivas_codificadas |>
  summarise(across(where(is.integer), sum, na.rm = TRUE),.by = cluster) |> 
  pivot_longer(-cluster,names_to = "ESG",values_to = "Freq") |>
  mutate(Prop = round(Freq / sum(Freq)*100,1),.by = cluster)

####################################################################################

teste1 <- pgmm(
 formula = disper ~ lag(disper) + lag(env_pillar) + lag(qanalyst) |
  lag(disper, 2:99) + lag(env_pillar, 2:99) + lag(qanalyst, 2:99),
 data = filter(bd,ticket %in% lista_corr_signal[[1]]$ticket),
 index = c('ticket', 'year'),
 model="twosteps", effect="twoways",
 collapse = TRUE
); summary(teste1)

teste2 <- pgmm(
 formula = disper ~ lag(disper) + env_pillar + qanalyst | 
  lag(disper, 2:99),
 data = filter(bd,ticket %in% lista_corr_signal[[1]]$ticket),
 index = c('ticket', 'year'),
 model="twosteps", effect="twoways",
 collapse = T,
 transformation = "d"
); summary(teste2)
################################################################################

abdi <- inner_join(
 x = bd_corr_chosen_pearson,
 y = bd_corr_chosen_spearman,
 by = join_by(ticket,P1,P2,status)
)

length(unique(abdi$ticket))

inner_join(
  bd_corr_chosen |> filter(status == "Correlação positiva") |> distinct(ticket),
  bd_corr_chosen |> filter(status == "Correlação negativa") |> distinct(ticket),
  by = join_by(ticket)
)

################################################################################

plm(
  disper ~ lag(disper) + env_pillar + qanalyst,
  data = filter(bd,ticket %in% lista_corr_signal[[1]]$ticket),
  model = "within", effect = "twoways"
) |> summary(vcov = \(x) vcovSCC(x,type = "HC1",inner = "cluster"))

plm(
  disper ~  lag(disper) + env_pillar + qanalyst,
  data = filter(bd,ticket %in% lista_corr_signal[[1]]$ticket),
  model = "within", effect = "twoways"
) |> pbgtest()

plm(
  disper ~ lag(disper) + env_pillar + qanalyst,
  data = filter(bd,ticket %in% lista_corr_signal[[1]]$ticket),
  model = "random"
) |> pcdtest()

teste <- phtest(resposta1[[2]],resposta1[[3]], method = "aux",
                vcov = \(x) vcovSCC(x,type = "HC1",inner = "cluster") 
                #function(x) vcovHC(x,method="arellano",type="HC1",cluster="group")
)





