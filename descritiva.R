##====================================================================================================
#                                       Analise Descritiva TCC
##====================================================================================================
# Infos:

# Período inicial de análise: 2011 a 2019
# Na descritiva, juntar os bancos em um só para fazer os gráficos
# Cores: #660033, #8B3E65, #A06181, #C49CB0, #E0CCD6
# middle color finder: https://www.dannybrien.com/middle/

## Analise inicial do genero e evasao

# grafico 1: genero, evasao por genero - com alunos ativos
# grafico 2: genero, evasao por genero - sem alunos ativos
# grafico 3: linha do tempo perido de entrada, por genero
# grafico 4: linha do tempo perido de saida, por genero

## Analise univariada

# tabela 1: contingencia univariada
# tabela 2: sistema de cotas
# grafico 4: idade, n_semestres, km
# grafico 5: ira, taxa_rep
# grafico 6: n_sr, rep_obr, trancamentos

## Analise bivarida

# tabela 4: contingencia bivariada
# grafico 4: idade, n_semestres, km vs evasao
# grafico 5: ira, taxa_rep vs evasao
# grafico 6: n_sr, rep_obr, trancamentos vs evasao

## Analise de correlação

# tabela 5: estatistica de teste e p-valor  - evasao vs explicativas
# tabela 6: corr entre variaveis numericas

##====================================================================================================
library(tidyverse)
library(magrittr)
library(epitools)

setwd("D:/Unb/Disciplinas/EST/TCC")
source('funcoes.R', encoding='UTF-8') # Funções

cic = readRDS("Dados_TCC/cic.rds")
est = readRDS("Dados_TCC/est_final.rds")    
mat = readRDS("Dados_TCC/mat_final.rds") 

cic_orig = readRDS("Dados_TCC/cic_original.rds") 
est_orig = readRDS("Dados_TCC/est_original2.rds")
mat_orig = readRDS("Dados_TCC/mat_original2.rds")


# Tabelas e odds -------------------------------------------------------------------------------------

? oddsratio

# agrupar racas para a analise descritiva
mat$raca = plyr::revalue(mat$raca, c(`Preta`='Negra',
                                     `Parda`='Negra'))
est$raca = plyr::revalue(est$raca, c(`Preta`='Negra',
                                     `Parda`='Negra'))

# corrigir escola
mat$Escola = plyr::revalue(mat$Escola, c(`Não informado`='Sem Informação'))
est$Escola = plyr::revalue(est$Escola, c(`Sem informação`='Sem Informação'))

# reprovacao C1 mat
mat$rep_calculo1[which(mat$rep_calculo1=='Não' & ! mat$Aluno %in% com_c1)] = 'Sem Informação'



# data.frame(table(x, useNA = 'ifany'))
View(tab_uni(est)); View(tab_uni(mat)); View(tab_uni(cic))

# Evasão vs variaveis
View(tab_biv(est));View(tab_biv(mat));View(tab_biv(cic))

oddsratio(cic$Escola,cic$evasao)

# cotas
table(est$cota);table(mat$cota);table(cic$cota);
table(est$cota)/57;table(mat$cota)/5

# cotas x escola
table(cic$cota,cic$Escola);table(est$cota,est$Escola);table(mat$cota,mat$Escola)
prop.table(table(cic$cota,cic$Escola),margin=1)
prop.table(table(est$cota,est$Escola),margin=1)
prop.table(table(mat$cota,mat$Escola),margin=1)

# para escrever a tabela em latex
require(Hmisc)
Hmisc::latex()


# Investigando reprovacoes em C1 na mat --------------------------------------------------------------

matematica = readRDS("Dados_TCC/matematica.rds") # read_xlsx("mat_final.xlsx")
alunas_mat = mat %>% pull(Aluno)

View(matematica %>% filter(Aluno %in% alunas_mat & 
                           nome_disciplina %in% c('Calculo 1', 'Cálculo 1','Cálculo 1 - Semipresencial',
                                                  'CALCULO 1', 'CÁLCULO 1', 
                                                  'CÁLCULO 1 - SEMIPRESENCIAL'))
)

com_c1 = matematica %>% filter(Aluno %in% alunas_mat & 
                               nome_disciplina %in% c('Calculo 1','Cálculo 1',
                                                      'Cálculo 1 - Semipresencial',
                                                      'CALCULO 1', 'CÁLCULO 1', 
                                                      'CÁLCULO 1 - SEMIPRESENCIAL')) %>% pull(Aluno)

com_c1 = unique(com_c1)

View(mat %>% filter(Aluno %in% com_c1))
View(mat %>% filter(!Aluno %in% com_c1))

table(mat$rep_calculo1) # 40 Não, 3 Sim

# Apenas 18 alunas possuem C1, 3 reprovaram
# 15 alunas nao tem C1 e constam como 'Não' reprovacao


mat$rep_calculo1[which(mat$rep_calculo1=='Não' & ! mat$Aluno %in% com_c1)] = 'Sim'

table(mat$rep_calculo1) # 15 Não, 28 Sim
table(mat$evasao, mat$rep_calculo1)

##====================================================================================================
## Funções para facilitar...

genero = function(dados,ativo=TRUE){
  
  if(ativo==FALSE){
    dados = dados %>% filter(! forma_saida_curso %in% c('Ativo','ativo'))
  }
  
  uni = dados %>% group_by(curso) %>%
    count(genero) %>% mutate(freq=round(n/sum(n)*100, 2),
                             pct=paste(gsub("\\.",",",freq), "%", sep= ''),
                             label=str_squish(str_c(pct, ' (', n,')'))) %>% 
    select(-c(n,pct))
  
  biv = dados %>% group_by(curso,genero) %>%
    count(evasao) %>% mutate(freq=round(n/sum(n)*100, 2),
                             pct=paste(gsub("\\.",",",freq), "%", sep= ''),
                             label=str_squish(str_c(pct, ' (', n,')'))) %>% 
    filter(evasao != 'Não') %>% select(-c(evasao,n,pct))
  
  final = as.data.frame(rbind(uni,biv)) %>%
    mutate(variavel = rep(c('Frequência','Evasão'),each=6))
  
  return(final)
}

##====================================================================================================
# Gráficos e medidas 

ativos = genero(rbind(cic_orig,est_orig,mat_orig))
n_ativos = genero(rbind(cic_orig,est_orig,mat_orig), ativo=FALSE)

ativos$variavel = factor(ativos$variavel, levels = c('Frequência','Evasão'))
n_ativos$variavel = factor(n_ativos$variavel, levels = c('Frequência','Evasão'))

# grafico 1: genero, evasao por genero - com alunos ativos
# grafico 2: genero, evasao por genero - sem alunos ativos

ativos %>% 
  ggplot(aes(x=genero, y=freq, fill=curso, label=label)) +
  geom_bar(stat='identity', width = .8) + 
  geom_text(vjust=-0.5, size=3)+
  facet_grid(variavel~curso, scales='free') + 
  coord_cartesian(ylim=c(0,100))+
  theme_minimal() + 
  labs(y='',x='Gênero') +
  scale_fill_manual(values=c('#660033', '#954E71', '#C49CB0')) + #954E71 
  theme(legend.position = 'none',
        strip.text.x = element_text(size = 11)) 

ggsave('Gráficos/Ativos/g1.pdf',width = 16, height = 12, units = "cm")


# Periodo de ingresso

# na est apenas 6 alunos entraram entre 20111 e 20132, todos sao duplicatas com historico
# periodos: 20111, 20122, 20132

est_orig %>% filter(Aluno %in% c("Aluno106951", "Aluno109408", "Aluno129501",
                                 "Aluno132812", "Aluno132926", "Aluno145177"))

anos = paste0('20',as.character(11:19))
periodos = c(paste0(anos,'1'),paste0(anos,'2'))

evasao = rbind(cic_orig,est_orig,mat_orig) %>% group_by(curso,periodo_saida_curso) %>% 
  filter(evasao=='Sim') %>% count(genero) 

# grafico 2: linha do tempo perido de saida, genero

as.data.frame(evasao) %>% 
  ggplot(aes(x=periodo_saida_curso, y=n, color=genero, label=n, group=genero)) +
  geom_line(stat='identity',position='identity', size=1) + 
  geom_text(vjust=-0.5, size=3, check_overlap = T, show.legend = F)+
  facet_grid(rows = 'curso', scales='free') + 
  scale_x_discrete(limits = sort(periodos),guide = guide_axis(angle = 90))+
  theme_minimal() + 
  labs(y='',x='Período de Saída do Curso') +
  scale_color_manual(values=c('#C49CB0','#660033'), name='Gênero', labels=c('Feminino','Masculino')) +
  theme(legend.position = 'top',
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        strip.text.y = element_text(size = 10)) +
  scale_y_continuous(expand = expansion(c(0, 0.2))) 

ggsave('Gráficos/Ativos/g2.pdf',width = 20, height = 15, units = "cm")

# grafico 3: linha do tempo perido de entrada, por genero

ingresso = rbind(cic_orig,est_orig,mat_orig) %>% group_by(curso,periodo_ingresso_curso) %>% 
  count(genero) # com ativos


as.data.frame(ingresso) %>% 
  ggplot(aes(x=periodo_ingresso_curso, y=n, color=genero, label=n, group=genero)) +
  geom_line(stat='identity',position='identity', size=1) + 
  geom_text(vjust=-0.5, size=3, check_overlap = T, show.legend = F)+
  facet_grid(rows = 'curso', scales='free') + 
  scale_x_discrete(limits = sort(periodos),guide = guide_axis(angle = 90))+
  theme_minimal() + 
  labs(y='',x='Período de Ingresso no Curso') +
  scale_color_manual(values=c('#C49CB0','#660033'), name='Gênero', labels=c('Feminino','Masculino')) +
  theme(legend.position = 'top',
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        strip.text.y = element_text(size = 10)) +
  scale_y_continuous(expand = expansion(c(0, 0.2))) 

ggsave('Gráficos/Ativos/g3.pdf',width = 20, height = 15, units = "cm")


# grafico 4: linha do tempo perido de saida, por genero

saida = rbind(cic_orig,est_orig,mat_orig) %>% group_by(curso,periodo_saida_curso) %>% 
  filter(forma_saida_curso != 'Ativo') %>%
  count(genero) # sem ativos


as.data.frame(saida) %>% 
  ggplot(aes(x=periodo_saida_curso, y=n, color=genero, label=n, group=genero)) +
  geom_line(stat='identity',position='identity', size=1) + 
  geom_text(vjust=-0.5, size=3, check_overlap = T, show.legend = F)+
  facet_grid(rows = 'curso', scales='free') + 
  scale_x_discrete(limits = sort(periodos),guide = guide_axis(angle = 90))+
  theme_minimal() + 
  labs(y='',x='Período de Saída do Curso') +
  scale_color_manual(values=c('#C49CB0','#660033'), name='Gênero', labels=c('Feminino','Masculino')) +
  theme(legend.position = 'top',
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        strip.text.y = element_text(size = 10)) +
  scale_y_continuous(expand = expansion(c(0, 0.2))) 

ggsave('Gráficos/Ativos/g4.pdf',width = 20, height = 15, units = "cm")

##====================================================================================================
# Grids

dados = rbind(cic,est,mat) %>% 
  select(id,curso,evasao,ira,n_evasao,idade,n_trancamentos,
         n_sr,taxa_rep,n_semestres,rep_obr,km,minutes) %>% 
  pivot_longer(!c(id,curso,evasao), names_to='variavel',values_to='valores') %>% 
  arrange(variavel)

# https://www.datacamp.com/tutorial/facets-ggplot-r

nome_var = list(
  'idade'='Idade',
  'ira'='IRA',
  'n_semestres'='Semestres cursados',
  'km'='Distância de \n deslocamento (km)',
  'minutes'='Tempo de \n deslocamento (min)',
  'n_trancamentos'='Trancamentos',
  'n_sr' = 'Menções SR',
  'rep_obr'='Reprovações em obr \n do 1º semestre',
  'taxa_rep'='Taxa de reprovação',
  'n_evasao'='Evasões anteriores'
)

? labeller

dados$evasao = factor(dados$evasao, levels = c('Sim','Não'))
dados$curso = factor(dados$curso, levels = unique(dados$curso))
dados$variavel = factor(dados$variavel, levels = c('idade','ira','n_semestres','km','minutes',
                                                   'n_trancamentos','n_sr','rep_obr','taxa_rep','n_evasao'))

nome_curso = levels(dados$curso)

rotulo = function(variable,value){
  if(variable=='variavel'){
    return(nome_var[value])
  } else {
    return(nome_curso)
  }
}


grafico_biv = function(dados,variaveis){
  
  g_biv = dados %>% 
    filter(variavel %in% variaveis) %>% 
    ggplot(aes(x=evasao, y=valores, fill=curso)) +
    stat_boxplot(aes(evasao, valores), 
                 geom='errorbar', linetype=1, width=0.5) +
    geom_boxplot(width=0.75) + 
    stat_summary(fun="mean", geom="point", shape=23, fill="white")+
    facet_grid(variavel~curso, scales='free', labeller = rotulo) +
    theme_minimal() + 
    labs(y='',x='Evasão') +
    scale_fill_manual(values=c('#660033', '#954E71', '#C49CB0')) +
    theme(legend.position = 'none',
          strip.text.x = element_text(size = 11)) 
  
  print(g_biv)
  
}


grafico_uni = function(dados,variaveis){
  
  g_uni = dados %>% 
    filter(variavel %in% variaveis) %>% 
    ggplot(aes(x=factor(''), y=valores, fill=curso)) +
    stat_boxplot(aes(factor(''), valores), 
                 geom='errorbar', linetype=1, width=0.4) +
    geom_boxplot(width=0.6) + 
    stat_summary(fun="mean", geom="point", shape=23, fill="white")+
    facet_grid(variavel~curso, scales='free', labeller = rotulo) +
    theme_minimal() + 
    labs(y='',x='') +
    scale_fill_manual(values=c('#660033', '#954E71', '#C49CB0')) +  # '#8B3E65'
    theme(legend.position = 'none',
          strip.text.x = element_text(size = 11)) 
  
  print(g_uni)
  
}

# grafico 4
grafico_uni(dados,c('idade','n_semestres','km')) +
  theme(panel.spacing = unit(0.75, 'cm', data=NULL))
ggsave('Gráficos/Ativos/g4.pdf',width = 16.5, height = 14, units = "cm")

# grafico 5
grafico_uni(dados,c('ira', 'taxa_rep')) +
  theme(panel.spacing = unit(0.75, 'cm', data=NULL))
ggsave('Gráficos/Ativos/g5.pdf',width = 16.5, height = 9.5, units = "cm")

# grafico 6
grafico_uni(dados,c('n_sr', 'rep_obr', 'n_trancamentos')) +
  theme(panel.spacing = unit(0.75, 'cm', data=NULL))
ggsave('Gráficos/Ativos/g6.pdf',width = 16.5, height = 14, units = "cm")


# grafico 7
grafico_biv(dados,c('idade','n_semestres','km')) +
  theme(panel.spacing = unit(0.75, 'cm', data=NULL))
ggsave('Gráficos/Ativos/g7.pdf',width = 16.5, height = 14, units = "cm")

# grafico 8
grafico_biv(dados,c('ira', 'taxa_rep')) +
  theme(panel.spacing = unit(0.75, 'cm', data=NULL))
ggsave('Gráficos/Ativos/g8.pdf',width = 16.5, height = 9.5, units = "cm")

# grafico 9
grafico_biv(dados,c('n_sr', 'rep_obr', 'n_trancamentos')) +
  theme(panel.spacing = unit(0.75, 'cm', data=NULL))
ggsave('Gráficos/Ativos/g9.pdf',width = 16.5, height = 14, units = "cm")


##====================================================================================================
# Medidas resumo

boxplot.stats(est$minutes)$stats
by(mat$rep_obr, mat$evasao, summary)


medidas_uni = function(dados){
  
  dados$evasao = factor(dados$evasao, levels = c('Sim','Não'))
  
  vars = names(dados)[c(23,4,29,32,24,25,31,26)]
  media = c(); cv = c()
  
  for(i in 1:length(vars)){
    media[i] = round(mean(dados[,vars[i]], na.rm=T),2)
    cv[i] = round(sd(dados[,vars[i]], na.rm=T)/media[i],2) * 100
  }
  
  return(data.frame(vars,media,cv))
}

medidas_biv = function(dados){
  
  dados$evasao = factor(dados$evasao, levels = c('Sim','Não'))
  
  vars = names(dados)[c(23,4,29,32,24,25,31,26)]
  media_sim = c(); media_nao = c()
  cv_sim = c(); cv_nao = c()
  
  for(i in 1:length(vars)){
    media_biv = by(dados[,vars[i]], dados$evasao, function(x) mean(x,na.rm = T))
    cv_biv = by(mat$rep_obr, mat$evasao, function(x) sd(x,na.rm = T))
    
    media_sim[i] = round(media_biv[1],2)
    media_nao[i] = round(media_biv[2],2)
    
    cv_sim[i] = round(cv_biv[1]/media_biv[1],2) * 100
    cv_nao[i] = round(cv_biv[2]/media_biv[2],2) * 100
    
  }
  
  return(data.frame(vars,media_sim,cv_sim,media_nao,cv_nao))
}

medidas_uni(cic);medidas_uni(est);medidas_uni(mat)
medidas_biv(cic);medidas_biv(est);medidas_biv(mat)

##====================================================================================================
# Correlacao entre variaveis

teste_quali = function(dados){
  
  vars = names(dados)[c(9,11,16,19,27,28,30,36)]
  est = c(); pvalor = c()
  
  for(i in 1:length(vars)){
    tab = table(dados[,vars[i]],dados[,'evasao'])
    
    if(any(tab<5)){
      teste = fisher.test(tab)
      est[i] = '-'
    } else {
      teste = chisq.test(tab)
      est[i] = round(teste$statistic,4)
    }
    
    #est[i] = teste$statistic
    pvalor[i] = round(as.numeric(format(teste$p.value,scientific=F)),4)
    
  }
  
  return(data.frame(vars,est,pvalor) %>% arrange(pvalor))
}

teste_quanti = function(dados){
  vars = names(dados)[c(4,21,23,24,25,26,29,31,32)]
  est = c(); pvalor = c()
  
  y = ifelse(dados$evasao=='Sim',1,0)
  
  for(i in 1:length(vars)){
    mod = glm(y ~ dados[,vars[i]], family = 'binomial')
    est[i] = coef(summary(mod))[2,'z value']
    pvalor[i] = coef(summary(mod))[2,'Pr(>|z|)']
  }
  
  pvalor = round(as.numeric(format(pvalor,scientific=F)),4)
  est = round(est,4)
  
  return(data.frame(vars,est,pvalor) %>% arrange(pvalor))
}

teste_quanti(cic); teste_quanti(est); teste_quanti(mat)


# Correlação entre variáveis numéricas

cor_cic = cor(cic[,c(4,21,23,24,25,26,29,31,32,33)], use='pairwise.complete.obs')  
cor_est = cor(est[,c(4,21,23,24,25,26,29,31,32,33)], use='pairwise.complete.obs')  
cor_mat = cor(mat[,c(4,21,23,24,25,26,29,31,32,33)], use='pairwise.complete.obs')  
