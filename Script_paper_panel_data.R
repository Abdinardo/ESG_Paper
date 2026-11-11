# Carregamento das bibliotecas ----
library(readxl); library(plm); library(dplyr); library(stargazer); library(ggplot2)
library(correlation); library(stringr); library(tictoc); library(janitor)
library(tidyr); library(purrr); library(ggpmisc); library(ggsci); library(furrr); library(patchwork)
#library(texreg)

# Definição do diretório de trabalho ----
setwd("/home/abdinardo/Dropbox/00 Artigo_Israel_Panel_Data")

# Carregamento dos arquivos .xlsx para bd_com ----
main_dir <- getwd()
arquivos <- paste0(main_dir,"/",list.files(main_dir,pattern="^BD\\w+.xlsx$"))
bd_com <- read_excel(arquivos[1],sheet=1,col_names=T) |> clean_names()
rm(arquivos,main_dir)

# Cálculo das correlações correlações para bd_com ----
# tic()
# bd_com_corr_all <- bd_com |>
#  select(-c(2:4)) |>
#  group_by(empresa) |>
#  correlation(method = "pearson") |>
#  ungroup()
# toc()

tic()
plan(multisession, workers = 3)
bd_com_corr_all <-  bd_com |>
 select(-c(2:4)) |>
 split(~empresa) |>
 future_map_dfr(\(df){
   correlation(df,method = "pearson",verbose = F) |>
   mutate(Group = unique(df$empresa)) |>
   relocate(Group,.before = Parameter1)
   },.progress = T)
plan(sequential)
toc()

# Identificação das correlações de Pearson significativas entre as variáveis de interesse e as variáveis dependentes ----
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

unique(table_corr_signal$Group) |> length() # total de empresas que apresentaram correlações significativas: 40.

# Gráfico boxplot das correlações ----
cores <- c("#5050FFFF","#CE3D32FF","#749B58FF","#F0E685FF","#466983FF","#BA6338FF")
names(cores) <- unique(table_corr_signal$Parameter2)
tamanho_eixos <- 9

# x11(); table_corr_signal |>
# ggplot(aes(x=Parameter2,y=r,fill=Parameter2)) +
# geom_boxplot(alpha=0.9,outlier.shape=NA,outlier.alpha = .9,show.legend = T)+
# geom_jitter(color="black",alpha=.5,width = 0.25, height = 0)+
# scale_fill_manual(values=cores) +
# labs(x="Índices de endividamento",y="Valores de correlação",fill="") +
# scale_y_continuous(n.breaks = 7) +
# theme_bw() +
# facet_wrap(~correlacao + Parameter1,scales="free_y")

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

x11(); P1/P2

# Definição dos modelos e da lista que os armazenará----
lista_modelos <- list()
modelos <- c("pooling","within","between","random")
modelos_random <- c("swar","walhus","nerlove")
variaveis_controle <- colnames(bd_com)[13:21]
lista_equacoes <- pmap(combinacoes[-1],\(VD,VI){
 formula(paste0(VD," ~ ",paste(c(VI,variaveis_controle[7]
                                 ),collapse = "+")))
})
names(lista_equacoes) <- str_c(combinacoes$Sinal,combinacoes$VD,combinacoes$VI,sep = "_")
nomes <- names(lista_equacoes)

# Cálculo de todos os modelos ----
for(equacao in names(lista_equacoes)){
 for(modelo in modelos){
  if(modelo != "random"){
   lista_modelos[paste0(equacao,"_",modelo)] <- list(
     try(
      plm(lista_equacoes[[equacao]],filter(bd_com,empresa %in% lista_corr_signal[[equacao]]$Group),
      model = modelo,index = c('empresa','ano_do_exercicio','pais'),effect='twoways'), silent = T)
   )
  }else{
   for(modelo_random in modelos_random){
    lista_modelos[paste0(equacao,"_",modelo,"_",modelo_random)] <-list(
      try(
       plm(lista_equacoes[[equacao]],filter(bd_com,empresa %in% lista_corr_signal[[equacao]]$Group),
       model = modelo,random.method = modelo_random,
       index = c('empresa','ano_do_exercicio','pais'),effect='twoways'), silent = T)
    )
   }
  }
 }
}

# Função para criar os gráficos ----
funcao_grafico <- function(i,posicaoX,textoX,textoY){
 paises <- lista_corr_signal[[i]]$Group
 textoRodape <- paste0("Country of origin: ",
  paste(
   unique(
    case_when(
     str_detect(paises,".KS$") ~ "KS: South Korea",
     str_detect(paises,".BK$") ~ "BK: Thailand",
     str_detect(paises,".J$") ~  "J: South Africa",
     str_detect(paises,".IS$") ~ "IS: Turkey",
     str_detect(paises,".SA$") ~ "SA: Brazil",
     str_detect(paises,".MX$") ~ "MX: Mexico",
     str_detect(paises,".DU$") ~ "DU: United Arab Emirates",
     str_detect(paises,".KL$") ~ "KL: Malaysia",
     .default =  "L: Malaysia"
    )
   ), collapse = "; "
  ),"."
 )

 bd_com |>
 filter(empresa %in% paises) |>
 ggplot(aes(x=.data[[combinacoes$VI[i]]],y=.data[[combinacoes$VD[i]]],group=empresa,color=empresa)) +
 geom_smooth(method = "lm",formula = "y ~ x",se=F,linewidth=1.5,linetype="solid") +
 geom_point(size=2) +
 scale_color_manual(values = pal_igv("default")(length(paises))) +
 stat_poly_eq(
  aes(label = paste(after_stat(rr.label), sep = "~~~")),  # Apenas o R²
  formula = y ~ x,
  parse = TRUE,   # Permite exibir com formatação matemática
  size = 4,       # Tamanho do texto
  label.x = posicaoX, # Posição horizontal (right, left, center)
  label.y = "top"    # Posição vertical (top, bottom, center)
 ) +
 labs(
  x=textoX,
  y=textoY,
  color="Company",
  caption = textoRodape
 )+
 theme_bw() +
 theme(
  axis.title = element_text(size=10,face="bold"),
  axis.text = element_text(size=11),
  legend.text = element_text(size=11),
  plot.caption = element_text(size=11,hjust = 0.05),
  plot.caption.position = "plot"
 )
}

# Gráfico para Sinal = negative, VD = disperwin, VI = env_pillar_win ----
valor_lista <- 1 # Variável para mudar a lista de dados a ser visualizada
G1 <- funcao_grafico(valor_lista,"right","Environmental Pillar Score","Dispersion of analysts' forecasts")
x11();G1

# Modelos para Sinal = negative, VD = disperwin, VI = env_pillar_win ----
resposta1 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta1,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))
paste0(1:length(resposta1)," - ",names(resposta1))
#screenreg(resposta1,digits = 3)

# Gráfico para Sinal = negative, VD = disperwin, VI = esg_comb_win ----
valor_lista <- 2 # Variável para mudar a lista de dados a ser visualizada
G2 <- funcao_grafico(valor_lista,"right","ESG Combined Score","Dispersion of analysts' forecasts")
x11();G2

# Modelos para Sinal = negative, VD = disperwin, VI = esg_comb_win ----
resposta2 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta2[-3],type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = negative, VD = disperwin, VI = esg_score_win ----
valor_lista <- 3# Variável para mudar a lista de dados a ser visualizada
G3 <- funcao_grafico(valor_lista,"right","ESG Score","Dispersion of analysts' forecasts")
x11();G3

# Modelos para Sinal = negative, VD = disperwin, VI = esg_score_win ----
resposta3 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta3,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = negative, VD = disperwin, VI = gov_pillar_win ----
valor_lista <- 4# Variável para mudar a lista de dados a ser visualizada
G4 <- funcao_grafico(valor_lista,"right","Governance Pillar Score","Dispersion of analysts' forecasts")
x11();G4

# Modelos para Sinal = negative, VD = disperwin, VI = gov_pillar_win ----
resposta4 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta4,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = negative, VD = disperwin, VI = social_pillar_win ----
valor_lista <- 5# Variável para mudar a lista de dados a ser visualizada
G5 <- funcao_grafico(valor_lista,"right","Social Pillar Score","Dispersion of analysts' forecasts")
x11();G5

# Modelos para Sinal = negative, VD = disperwin, VI = social_pillar_win ----
resposta5 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta5,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = negative, VD = errowin, VI = env_pillar_win ----
valor_lista <- 6 # Variável para mudar a lista de dados a ser visualizada
G6 <- funcao_grafico(valor_lista,"right","Environmental Pillar Score","Prediction Error")
x11();G6

# Modelos para Sinal = negative, VD = errowin, VI = env_pillar_win ----
resposta6 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta6,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = negative, VD = errowin, VI = esg_comb_win ----
valor_lista <- 7 # Variável para mudar a lista de dados a ser visualizada
G7 <- funcao_grafico(valor_lista,"right","ESG Combined Score","Prediction Error")
x11();G7

# Modelos para Sinal = negative, VD = errowin, VI = esg_comb_win ----
resposta7 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta7,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = negative, VD = errowin, VI = esg_score_win ----
valor_lista <- 8 # Variável para mudar a lista de dados a ser visualizada
G8 <- funcao_grafico(valor_lista,"right","ESG Score","Prediction Error")
x11();G8

# Modelos para Sinal = negative, VD = errowin, VI = esg_score_win ----
resposta8 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta8,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = negative, VD = errowin, VI = gov_pillar_win ----
valor_lista <- 9 # Variável para mudar a lista de dados a ser visualizada
G9 <- funcao_grafico(valor_lista,"right","Governance Pillar Score","Prediction Error")
x11();G9

# Modelos para Sinal = negative, VD = errowin, VI = gov_pillar_win ----
resposta9 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta9,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = negative, VD = errowin, VI = social_pillar_win ----
valor_lista <- 10 # Variável para mudar a lista de dados a ser visualizada
G10 <- funcao_grafico(valor_lista,"right","Social Pillar Score","Prediction Error")
x11();G10

# Modelos para Sinal = negative, VD = errowin, VI = social_pillar_win ----
resposta10 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta10,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = negative, VD = mepawin, VI = env_pillar_win ----
valor_lista <- 11 # Variável para mudar a lista de dados a ser visualizada
G11 <- funcao_grafico(valor_lista,"right","Environmental Pillar Score","Mean Absolute Prediction Error")
x11();G11

# Modelos para Sinal = negative, VD = mepawin, VI = env_pillar_win ----
resposta11 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta11,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = negative, VD = mepawin, VI = esg_comb_win ----
valor_lista <- 12 # Variável para mudar a lista de dados a ser visualizada
G12 <- funcao_grafico(valor_lista,"right","ESG Combined Score","Mean Absolute Prediction Error")
x11();G12

# Modelos para Sinal = negative, VD = mepawin, VI = esg_comb_win ----
resposta12 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta12,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = negative, VD = mepawin, VI = esg_score_win ----
valor_lista <- 13 # Variável para mudar a lista de dados a ser visualizada
G13 <- funcao_grafico(valor_lista,"right","ESG Score","Mean Absolute Prediction Error")
x11();G13

# Modelos para Sinal = negative, VD = mepawin, VI = esg_score_win ----
resposta13 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta13,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = negative, VD = mepawin, VI = gov_pillar_win ----
valor_lista <- 14 # Variável para mudar a lista de dados a ser visualizada
G14 <- funcao_grafico(valor_lista,"right","Governance Pillar Score","Mean Absolute Prediction Error")
x11();G14

# Modelos para Sinal = negative, VD = mepawin, VI = gov_pillar_win ----
resposta14 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta14,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = negative, VD = mepawin, VI = social_pillar_win ----
valor_lista <- 15 # Variável para mudar a lista de dados a ser visualizada
G15 <- funcao_grafico(valor_lista,"right","Social Pillar Score","Mean Absolute Prediction Error")
x11();G15

# Modelos para Sinal = negative, VD = mepawin, VI = social_pillar_win ----
resposta15 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta15,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = positive, VD = disperwin, VI = env_pillar_win ----
valor_lista <- 16 # Variável para mudar a lista de dados a ser visualizada
G16 <- funcao_grafico(valor_lista,"left","Environmental Pillar Score","Dispersion of analysts' forecasts")
x11();G16

# Modelos para Sinal = positive, VD = disperwin, VI = env_pillar_win ----
resposta16 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta16,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = positive, VD = disperwin, VI = esg_comb_win ----
valor_lista <- 17# Variável para mudar a lista de dados a ser visualizada
G17 <- funcao_grafico(valor_lista,"left","ESG Combined Score","Dispersion of analysts' forecasts")
x11();G17

# Modelos para Sinal = positive, VD = disperwin, VI = esg_comb_win ----
resposta17 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta17,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = positive, VD = disperwin, VI = esg_score_win ----
valor_lista <- 18# Variável para mudar a lista de dados a ser visualizada
G18 <- funcao_grafico(valor_lista,"left","ESG Score","Dispersion of analysts' forecasts")
x11();G18

# Modelos para Sinal = positive, VD = disperwin, VI = esg_score_win ----
resposta18 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta18,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = positive, VD = disperwin, VI = gov_pillar_win ----
valor_lista <- 19# Variável para mudar a lista de dados a ser visualizada
G19 <- funcao_grafico(valor_lista,"left","Governance Pillar Score","Dispersion of analysts' forecasts")
x11();G19

# Modelos para Sinal = positive, VD = disperwin, VI = gov_pillar_win ----
resposta19 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta19[-3],type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = positive, VD = disperwin, VI = social_pillar_win ----
valor_lista <- 20# Variável para mudar a lista de dados a ser visualizada
G20 <- funcao_grafico(valor_lista,"left","Social Pillar Score","Dispersion of analysts' forecasts")
x11();G20

# Modelos para Sinal = positive, VD = disperwin, VI = social_pillar_win ----
resposta20 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta20,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = positive, VD = errowin, VI = env_pillar_win ----
valor_lista <- 21 # Variável para mudar a lista de dados a ser visualizada
G21 <- funcao_grafico(valor_lista,"left","Environmental Pillar Score","Prediction Error")
x11();G21

# Modelos para Sinal = positive, VD = errowin, VI = env_pillar_win ----
resposta21 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta21,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = positive, VD = errowin, VI = esg_comb_win ----
valor_lista <- 22 # Variável para mudar a lista de dados a ser visualizada
G22 <- funcao_grafico(valor_lista,"left","ESG Combined Score","Prediction Error")
x11();G22

# Modelos para Sinal = positive, VD = errowin, VI = esg_comb_win ----
resposta22 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta22,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = positive, VD = errowin, VI = esg_score_win ----
valor_lista <- 23 # Variável para mudar a lista de dados a ser visualizada
G23 <- funcao_grafico(valor_lista,"left","ESG Score","Prediction Error")
x11();G23

# Modelos para Sinal = positive, VD = errowin, VI = esg_score_win ----
resposta23 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta23,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = positive, VD = errowin, VI = gov_pillar_win ----
valor_lista <- 24 # Variável para mudar a lista de dados a ser visualizada
G24 <- funcao_grafico(valor_lista,"left","Governance Pillar Score","Prediction Error")
x11();G24

# Modelos para Sinal = positive, VD = errowin, VI = gov_pillar_win ----
resposta24 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta24,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = positive, VD = errowin, VI = social_pillar_win ----
valor_lista <- 25 # Variável para mudar a lista de dados a ser visualizada
G25 <- funcao_grafico(valor_lista,"left","Social Pillar Score","Prediction Error")
x11();G25

# Modelos para Sinal = positive, VD = errowin, VI = social_pillar_win ----
resposta25 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta25,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = positive, VD = mepawin, VI = env_pillar_win ----
valor_lista <- 26 # Variável para mudar a lista de dados a ser visualizada
G26 <- funcao_grafico(valor_lista,"left","Environmental Pillar Score","Mean Absolute Prediction Error")
x11();G26

# Modelos para Sinal = positive, VD = mepawin, VI = env_pillar_win ----
resposta26 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta26,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = positive, VD = mepawin, VI = esg_comb_win ----
valor_lista <- 27 # Variável para mudar a lista de dados a ser visualizada
G27 <- funcao_grafico(valor_lista,"left","ESG Combined Score","Mean Absolute Prediction Error")
x11();G27

# Modelos para Sinal = negative, VD = mepawin, VI = esg_comb_win ----
resposta27 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta27,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = positive, VD = mepawin, VI = esg_score_win ----
valor_lista <- 28 # Variável para mudar a lista de dados a ser visualizada
G28 <- funcao_grafico(valor_lista,"left","ESG Score","Mean Absolute Prediction Error")
x11();G28

# Modelos para Sinal = positive, VD = mepawin, VI = esg_score_win ----
resposta28 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta28,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = positive, VD = mepawin, VI = gov_pillar_win ----
valor_lista <- 29 # Variável para mudar a lista de dados a ser visualizada
G29 <-  funcao_grafico(valor_lista,"left","Governance Pillar Score","Mean Absolute Prediction Error")
x11();G29

# Modelos para Sinal = positive, VD = mepawin, VI = gov_pillar_win ----
resposta29 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta29,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))

# Gráfico para Sinal = positive, VD = mepawin, VI = social_pillar_win ----
valor_lista <- 30 # Variável para mudar a lista de dados a ser visualizada
G30 <- funcao_grafico(valor_lista,"left","Social Pillar Score","Mean Absolute Prediction Error")
x11();G30

# Modelos para Sinal = positive, VD = mepawin, VI = social_pillar_win ----
resposta30 <- Filter(is.list,lista_modelos[str_detect(names(lista_modelos),names(lista_equacoes)[valor_lista])])
stargazer(resposta30,type="text",dep.var.labels = stringr::str_replace_all(nomes[valor_lista],"_"," "))



