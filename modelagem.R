##====================================================================================================
#                                          Modelagem TCC
##====================================================================================================
# Infos:

# Ajuste dos modelos de regressão logística

##====================================================================================================
library(tidyverse)
library(MASS)
library(leaps)
library(rsample)
library(pROC)
library(caret)

setwd("D:/Unb/Disciplinas/EST/TCC")

cic = readRDS("Dados_TCC/cic.rds")
est = readRDS("Dados_TCC/est_final.rds")    
mat = readRDS("Dados_TCC/mat_final.rds") 

##====================================================================================================
# Manipulando dados...

# NAs 6 cic, 10 est, 3 mat
cic$km[which(is.na(cic$km))] = mean(cic$km, na.rm = T)
est$km[which(is.na(est$km))] = mean(est$km, na.rm = T)
mat$km[which(is.na(mat$km))] = mean(mat$km, na.rm = T)

# Factor evasão
cic$evasao = factor(cic$evasao, levels=c('Não','Sim'))
est$evasao = factor(est$evasao, levels=c('Não','Sim'))
mat$evasao = factor(mat$evasao, levels=c('Não','Sim'))

# Escala da taxa de reprovacao
cic$taxa_rep = 100*cic$taxa_rep
est$taxa_rep = 100*est$taxa_rep
mat$taxa_rep = 100*mat$taxa_rep

mat$raca = factor(mat$raca, levels=c('Sem Informação','Negra', 'Branca'))
est$renda_media = factor(est$renda_media, levels = c('Sem Informação','Renda Alta','Renda Média-Alta',
                                                     'Renda Média-Baixa','Renda Baixa'))
cic$renda_media = factor(cic$renda_media, levels = c('Sem Informação','Renda Alta','Renda Média-Alta',
                                                     'Renda Média-Baixa','Renda Baixa'))
mat$renda_media = factor(mat$renda_media, levels = c('Sem Informação','Renda Alta','Renda Média-Alta',
                                                     'Renda Média-Baixa','Renda Baixa'))

# Amostras de treino e teste

# CIC
set.seed(0)

split_cic = initial_split(cic, prop = 0.8, strata = evasao)
treino_cic = training(split_cic)
teste_cic = testing(split_cic)

saveRDS(teste_cic,'teste_cic.rds'); saveRDS(treino_cic,'treino_cic.rds')

# EST 
set.seed(0)

split_est = initial_split(est, prop = 0.8, strata = evasao)
treino_est = training(split_est)
teste_est = testing(split_est)

saveRDS(teste_est,'teste_est.rds'); saveRDS(treino_est,'treino_est.rds')

# MAT
set.seed(0)

split_mat = initial_split(mat, prop = 0.8, strata = evasao)
treino_mat = training(split_mat)
teste_mat = testing(split_mat)

saveRDS(teste_mat,'teste_mat.rds'); saveRDS(treino_mat,'treino_mat.rds')

##====================================================================================================
# Erros        

# Possiveis variaveis causando problema:
# escola, forma de ingresso

# glm.fit: algorithm did not converge
# This warning often occurs when you attempt to fit a logistic regression model 
# in R and you experience perfect separation – that is, a predictor variable is
# able to perfectly separate the response variable into 0’s and 1’s.
 
# Warning message:
# glm.fit: fitted probabilities numerically 0 or 1 occurred 
# https://www.statology.org/glm-fit-fitted-probabilities-numerically-0-or-1-occurred/

##====================================================================================================
# Modelo Comum aos 3 cursos

# tirar: raca, curriculo, forma de ingresso, escola
# fazer um com taxa de reprovacao, outro com ira (tx rep = 26, ira = 4)


# CIC 

modcic = glm(evasao ~., data = cic[,c(4,9,20,21,23,24,25,27,28,29,31,32,36)], family='binomial')
summary(modcic)

# EST

modest = glm(evasao ~., data = est[,c(4,9,20,21,23,24,25,27,28,29,31,32,36)], family='binomial')
summary(modest)

# MAT

modmat = glm(evasao ~., data = mat[,c(4,9,20,21,23,24,25,27,28,29,31,32,36)], family='binomial')
summary(modmat)


##====================================================================================================
# Agrupamentos
# Testar diferentes agrupamentos de variaveis categoricas

# CIC - Forma de ingresso e Local por renda (1)

cic2 = cic
cic2$renda_media = as.character(cic2$renda_media)
cic2$forma_ingresso_curso[which(!cic2$forma_ingresso_curso %in% c('PAS','Vestibular'))] = 'Outra'
cic2$renda_media[which(cic2$renda_media %in% c('Renda Alta'))] = 'Renda Média-Alta'
cic2$renda_media[which(cic2$renda_media %in% c('Renda Baixa','Sem Informação'))] = 'Renda Média-Baixa'

# Testes de associacao, novo agrupamento
chisq.test(table(cic2$forma_ingresso_curso, cic2$evasao))
chisq.test(table(cic2$renda_media, cic2$evasao))

# EST - Raça e Local por renda (1)

est2 = est
est2$renda_media = as.character(est2$renda_media)
est2$raca[which(est2$raca %in% c('Amarela'))] = 'Sem Informação'
est2$renda_media[which(est2$renda_media %in% c('Renda Alta'))] = 'Renda Média-Alta'
est2$renda_media[which(est2$renda_media %in% c('Renda Baixa','Sem Informação'))] = 'Renda Média-Baixa'

# Testes de associacao, novo agrupamento
chisq.test(table(est2$raca, est2$evasao))
chisq.test(table(est2$renda_media, est2$evasao))

# EST - Raça e Local por renda (2)

est2 = est
est2$renda_media = as.character(est2$renda_media)
est2$raca[which(est2$raca %in% c('Amarela'))] = 'Sem Informação'
est2$renda_media[which(est2$renda_media %in% c('Renda Baixa','Sem Informação'))] = 'Renda Baixa/Sem informação'

# Testes de associacao, novo agrupamento
chisq.test(table(est2$raca, est2$evasao))
chisq.test(table(est2$renda_media, est2$evasao))

# MAT - Local por renda (1)

mat2 = mat
mat2$renda_media = as.character(mat2$renda_media)
mat2$renda_media[which(mat2$renda_media %in% c('Renda Alta'))] = 'Renda Média-Alta'
mat2$renda_media[which(mat2$renda_media %in% c('Renda Baixa','Sem Informação'))] = 'Renda Média-Baixa'

# Testes de associacao, novo agrupamento
fisher.test(table(mat2$renda_media, mat2$evasao))

##====================================================================================================
# Stepwise
# Testar tambem os diferentes agrupamentos!

# CIC 

print(names(cic)[c(4,9,11,16,20,21,23,24,25,26,27,28,29,30,31,36)])

fullcic = glm(evasao ~ ., data = cic[,c(4,9,11,16,20,21,23,24,25,26,27,28,29,30,31,36)], family = 'binomial')
nullcic = glm(evasao ~ 1, data = cic, family = 'binomial')

stepcic = stepAIC(nullcic,
                direction = 'forward',
                scope = list(upper = fullcic,
                             lower = nullcic),
                trace = 0)
summary(stepcic)

# EST

print(names(est)[c(4,9,19,20,21,23,24,25,26,27,28,29,31,36)])

fullest = glm(evasao ~ ., data = est[,c(4,9,19,20,21,23,24,25,26,27,28,29,31,36)], family = 'binomial')
nullest = glm(evasao ~ 1, data = est, family = 'binomial')

stepest = stepAIC(nullest,
                direction = 'forward',
                scope = list(upper = fullest,
                             lower = nullest),
                trace = 0)
summary(stepest)


# MAT

print(names(mat)[c(4,9,19,20,21,23,24,25,26,27,28,29,31,36)])

fullmat = glm(evasao ~ ., data = mat[,c(4,9,19,20,21,23,24,25,26,27,28,29,31,36)], family = 'binomial')
nullmat = glm(evasao ~ 1, data = mat, family = 'binomial')

stepmat = stepAIC(nullmat,
                direction = 'forward',
                scope = list(upper = fullmat,
                             lower = nullmat),
                trace = 0)
summary(stepmat)


##====================================================================================================
# Seleção do melhor modelo para cada tamanho
# Testar tambem os diferentes agrupamentos!


require(leaps)
selecao = regsubsets(evasao ~., data = cic[,c(4,9,20,21,23,24,25,26,27,28,29,31,32,36)], method=c('forward'))
summary(selecao, all.best=T)


selecao_mod = function(dados,vars,n){
  
  # criar vetores dos criterios
  aic=c();dev=c();hos=c();variaveis=c()
  
  # subconjuntos possiveis (usar index das colunas do banco)
  aux <- combinat::combn(vars, n)
  
  
  for(i in 1:ncol(aux)){
    
    data = data.frame(dados[,c(20,aux[,i])])
    mod <- glm(evasao ~ ., data=data, family = 'binomial')
    
    aic[i]=mod$aic; dev[i]=mod$deviance;
    hos[i]=ResourceSelection::hoslem.test(mod$y,mod$fitted.values,g=10)$p.value
    
    variaveis[i]=paste(aux[,i],collapse = ', ')
  }
  
  criterios <- data.frame(aic,dev,pvalor=format(hos, scientific = F),variaveis)
  
  return(criterios %>% mutate(pvalor = as.numeric(as.character(pvalor))) %>% 
         filter(pvalor>0.05) %>% arrange(aic))
  
}

# Mudar n da função para testar diferentes tamanhos de modelo

# CIC 
print(names(cic)[c(4,11,16,23,26,27,29,30)])

selecao_mod(cic,c(4,11,16,23,26,27,29,30),5)[1:5,]
selecao_mod(cic,c(4,11,16,23,26,27,29,30),4)[1:5,]

# EST
print(names(est)[c(4,23,24,25,27,26,29,36)])

selecao_mod(est,c(4,23,24,25,27,26,29,36),5)[1:5,]
selecao_mod(est,c(4,23,24,25,27,26,29,36),4)[1:5,]

# MAT
print(names(mat)[c(4,9,19,21,24,25,26,29)])

selecao_mod(mat,c(4,9,19,21,24,25,26,29),6)[1:5,]
selecao_mod(mat,c(4,9,19,21,24,25,26,29),5)[1:5,]
selecao_mod(mat,c(4,9,19,21,24,25,26,29),4)[1:5,]

##====================================================================================================
# Comparar modelos
#   Modelos com interação, testes deviance

# CIC ---------------------------------------------------------------------------------------------


# Modelo IRA

mod = glm(evasao ~ ira + curriculo + Escola + n_semestres, data = treino_cic, family = 'binomial')
mod2 = glm(evasao ~ ira + curriculo + Escola + n_semestres + ira:n_semestres, data = treino_cic, family = 'binomial')

anova(mod2,mod, test='LRT')
ResourceSelection::hoslem.test(mod2$y,mod2$fitted.values,g=10)

# Modelo Taxa de Reprovação

mod = glm(evasao ~ taxa_rep + curriculo + n_semestres, data = treino_cic, family = 'binomial')
mod2 = glm(evasao ~ taxa_rep + curriculo + Escola + n_semestres, data = treino_cic, family = 'binomial')

anova(mod2,mod, test='LRT')
ResourceSelection::hoslem.test(mod2$y,mod2$fitted.values,g=10)

mod = glm(evasao ~ taxa_rep + curriculo + Escola + n_semestres, data = treino_cic, family = 'binomial')
mod2 = glm(evasao ~ taxa_rep + curriculo + Escola + n_semestres + taxa_rep:n_semestres, data = treino_cic, family = 'binomial')

anova(mod2,mod, test='LRT')
ResourceSelection::hoslem.test(mod2$y,mod2$fitted.values,g=10)


# EST ---------------------------------------------------------------------------------------------

# Modelo IRA

mod = glm(evasao ~ ira + n_semestres + n_trancamentos, data = treino_est, family = 'binomial')
mod2 = glm(evasao ~ ira + n_semestres + n_trancamentos + idade, data = treino_est, family = 'binomial')

anova(mod2,mod, test='LRT')
ResourceSelection::hoslem.test(mod2$y,mod2$fitted.values,g=10)

mod2 = glm(evasao ~ ira + n_semestres + n_trancamentos + n_semestres:n_trancamentos, data = treino_est, family = 'binomial')
anova(mod2,mod, test='LRT')
ResourceSelection::hoslem.test(mod2$y,mod2$fitted.values,g=10)

mod3 = glm(evasao ~ ira + n_semestres + n_trancamentos + ira:n_trancamentos, data = treino_est, family = 'binomial')
anova(mod3,mod, test='LRT')
ResourceSelection::hoslem.test(mod3$y,mod3$fitted.values,g=10)


mod4 = glm(evasao ~ ira + n_semestres + n_trancamentos + n_semestres:ira, data = treino_est, family = 'binomial')
anova(mod4,mod, test='LRT')
ResourceSelection::hoslem.test(mod4$y,mod4$fitted.values,g=10)


# Modelo Taxa de Reprovação

mod = glm(evasao ~ taxa_rep + n_semestres + n_trancamentos, data = treino_est, family = 'binomial')
mod2 = glm(evasao ~ taxa_rep + n_semestres + n_trancamentos + idade, data = treino_est, family = 'binomial')

anova(mod2,mod, test='LRT')
ResourceSelection::hoslem.test(mod2$y,mod2$fitted.values,g=10)

mod2 = glm(evasao ~ taxa_rep + n_semestres + n_trancamentos + n_semestres:n_trancamentos, data = treino_est, family = 'binomial')
anova(mod2,mod, test='LRT')
ResourceSelection::hoslem.test(mod2$y,mod2$fitted.values,g=10)


mod3 = glm(evasao ~ taxa_rep + n_semestres + n_trancamentos + taxa_rep:n_trancamentos, data = treino_est, family = 'binomial')
anova(mod3,mod, test='LRT')
ResourceSelection::hoslem.test(mod3$y,mod3$fitted.values,g=10)


mod4 = glm(evasao ~ taxa_rep + n_semestres + n_trancamentos + n_semestres:taxa_rep, data = treino_est, family = 'binomial')
anova(mod4,mod, test='LRT')
ResourceSelection::hoslem.test(mod4$y,mod4$fitted.values,g=10)


# MAT ---------------------------------------------------------------------------------------------

# Modelo IRA

mod = glm(evasao ~ ira + n_semestres + n_trancamentos, data = treino_mat, family = 'binomial')
mod2 = glm(evasao ~ ira + n_semestres + n_trancamentos + n_semestres:ira, data = treino_mat, family = 'binomial')

anova(mod2,mod, test='LRT')
ResourceSelection::hoslem.test(mod2$y,mod2$fitted.values,g=10)

mod3 = glm(evasao ~ ira + n_semestres + n_trancamentos + sistema_cotas, data = treino_mat, family = 'binomial')

anova(mod3,mod, test='LRT')
ResourceSelection::hoslem.test(mod3$y,mod3$fitted.values,g=10)

# Modelo Taxa de Reprovação

mod = glm(evasao ~ taxa_rep + n_sr + n_trancamentos, data = treino_mat, family = 'binomial')
mod2 = glm(evasao ~ taxa_rep + n_sr + n_trancamentos + n_sr:n_trancamentos, data = treino_mat, family = 'binomial')

anova(mod2,mod, test='LRT')
ResourceSelection::hoslem.test(mod2$y,mod2$fitted.values,g=10)

mod3 = glm(evasao ~ taxa_rep + n_sr + n_trancamentos + sistema_cotas, data = treino_mat, family = 'binomial')

anova(mod3,mod, test='LRT')
ResourceSelection::hoslem.test(mod3$y,mod3$fitted.values,g=10)

# Modelo IRA e Taxa de Reprovação

mod = glm(evasao ~  ira + n_trancamentos + taxa_rep, data = treino_mat, family = 'binomial')
mod2 = glm(evasao ~  ira + n_trancamentos + taxa_rep + n_sr, data = treino_mat, family = 'binomial')

anova(mod2,mod, test='LRT')
ResourceSelection::hoslem.test(mod2$y,mod2$fitted.values,g=10)


##====================================================================================================
# Modelos finais
# Ajustar na amostra de treino e na amostra completa

treino_cic = readRDS("Dados_TCC/treino_cic.rds"); teste_cic = readRDS("Dados_TCC/teste_cic.rds")
treino_est = readRDS("Dados_TCC/treino_est.rds"); teste_est = readRDS("Dados_TCC/teste_est.rds")
treino_mat = readRDS("Dados_TCC/treino_mat.rds"); teste_mat = readRDS("Dados_TCC/teste_mat.rds")


# Matriz de confusao

confusao = function(y, prob, corte){
  
  tabela = caret::confusionMatrix(factor(ifelse(prob>=corte,'Sim','Não'),levels=c('Não','Sim')),
                                  y,positive='Sim')
  
  return(c(tabela$overall["Accuracy"], 
           tabela$byClass["Sensitivity"], 
           tabela$byClass["Specificity"]))
  
}

# CIC
mod = glm(evasao ~ ira + curriculo + Escola + n_semestres, data = teste_cic, family = 'binomial')
summary(mod)

# Predição
table(ifelse(prob>=0.6,'Sim0','Não0'),teste_cic$evasao)
confusao(teste_cic$evasao,prob,0.6)

# Testes de ajuste
ResourceSelection::hoslem.test(mod$y,mod$fitted.values,g=10)
res_dev = sum(residuals(mod,type='deviance')^2)
p_dev= 1 - pchisq(res_dev, df=df.residual(mod))
res_pearson = sum(residuals(mod,type='pearson')^2)
p_pearson = 1 - pchisq(res_pearson, df=df.residual(mod))

# Paramentros e IC
mod = glm(evasao ~ ira + curriculo + Escola + n_semestres, data = cic, family = 'binomial')
summary(mod)

format(exp(cbind(Odds_Ratio = coef(mod), confint(mod))),scientific = FALSE)



# EST
mod = glm(evasao ~ taxa_rep + n_semestres + n_trancamentos + idade, data = teste_est, family = 'binomial')
summary(mod)

# Predição
table(ifelse(prob>=0.6,'Sim0','Não0'),teste_est$evasao)
confusao(teste_est$evasao,prob,0.6)

# Testes de ajuste
ResourceSelection::hoslem.test(mod$y,mod$fitted.values,g=10)
res_dev = sum(residuals(mod,type='deviance')^2)
p_dev= 1 - pchisq(res_dev, df=df.residual(mod))
res_pearson = sum(residuals(mod,type='pearson')^2)
p_pearson = 1 - pchisq(res_pearson, df=df.residual(mod))

# Paramentros e IC
mod = glm(evasao ~  taxa_rep + n_semestres + n_trancamentos + idade, data = est, family = 'binomial')
summary(mod)

format(exp(cbind(Odds_Ratio = coef(mod), confint(mod))),scientific = FALSE)


# MAT
mod = glm(evasao ~ taxa_rep + n_trancamentos, data = teste_mat, family = 'binomial')
summary(mod)

# Predição
table(ifelse(prob>=0.55,'Sim0','Não0'),teste_mat$evasao)
confusao(teste_mat$evasao,prob,0.55)

# Testes de ajuste
ResourceSelection::hoslem.test(mod$y,mod$fitted.values,g=10)
res_dev = sum(residuals(mod,type='deviance')^2)
p_dev= 1 - pchisq(res_dev, df=df.residual(mod))
res_pearson = sum(residuals(mod,type='pearson')^2)
p_pearson = 1 - pchisq(res_pearson, df=df.residual(mod))

# Paramentros e IC
mod = glm(evasao ~ taxa_rep + n_trancamentos, data = mat, family = 'binomial')
summary(mod)

format(exp(cbind(Odds_Ratio = coef(mod), confint(mod))),scientific = FALSE)


##====================================================================================================
# Gráficos
# Curva ROC, residuos


# CIC

mod = glm(evasao ~ ira + curriculo + Escola + n_semestres, data = treino_cic, family = 'binomial')

prob = predict(mod, newdata = teste_cic, type = "response")
roc = roc(teste_cic$evasao ~ prob)

ggroc(roc, colour = '#660033', size = 1) +
  geom_segment(aes(x=1, xend=0, y=0, yend=1), color='lightgrey', linetype='dashed') +
  #ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')')) +
  labs(x='Especificidade',y='Sensibilidade')+
  theme_minimal() +
  annotate(geom = "text", x = 0.5, y = 0.5, label = paste0(round(roc$auc,3)), hjust = "left", size=4)

ggsave('roc_cic.pdf',width = 10, height = 8, units = "cm")

plot(roc, print.auc=TRUE, auc.polygon=TRUE, #grid=c(0.1, 0.2),
     max.auc.polygon=TRUE, auc.polygon.col='#660033',
     xlab="Especificidade", ylab="Sensibilidade")

# Residuos
res_pearson <- residuals(mod, type = "pearson") / sqrt(1 - hatvalues(mod))

ggplot() +
  geom_point(aes(x= fitted(mod), y=res_pearson))+
  theme_minimal()+
  geom_hline(yintercept=0)+
  geom_smooth(aes(x= fitted(mod), y=res_pearson), color='#660033', fill='#660033', se=FALSE)+
  labs(x='Probabilidade estimada',y='Resíduos Pearson')

ggsave('dev1_cic.pdf',width = 12, height = 8, units = "cm")


res_dev <- residuals(mod, type = "deviance") / sqrt(1 - hatvalues(mod))

ggplot() +
  geom_point(aes(x= fitted(mod), y=res_dev))+
  theme_minimal()+
  geom_hline(yintercept=0)+
  geom_smooth(aes(x= fitted(mod), y=res_dev), color='#660033', fill='#660033', se=FALSE)+
  labs(x='Probabilidade estimada',y='Resíduos Deviance')

ggsave('dev2_cic.pdf',width = 12, height = 8, units = "cm")


# EST 

mod = glm(evasao ~  taxa_rep + n_semestres + n_trancamentos + idade, data = treino_est, family = 'binomial')

prob = predict(mod, newdata = teste_est, type = "response")
roc = roc(teste_est$evasao ~ prob)

ggroc(roc, colour = '#8B3E65', size = 1) +
  geom_segment(aes(x=1, xend=0, y=0, yend=1), color='lightgrey', linetype='dashed') +
  #ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')')) +
  labs(x='Especificidade',y='Sensibilidade')+
  theme_minimal() +
  annotate(geom = "text", x = 0.5, y = 0.5, label = paste0(round(roc$auc,3)), hjust = "left")

ggsave('roc_est.pdf',width = 10, height = 8, units = "cm")


# Residuos
res_pearson <- residuals(mod, type = "pearson") / sqrt(1 - hatvalues(mod))

ggplot() +
  geom_point(aes(x= fitted(mod), y=res_pearson))+
  theme_minimal()+
  geom_hline(yintercept=0)+
  geom_smooth(aes(x= fitted(mod), y=res_pearson), color='#8B3E65', fill='#8B3E65', se=FALSE)+
  labs(x='Probabilidade estimada',y='Resíduos Pearson')

ggsave('dev1_est.pdf',width = 12, height = 8, units = "cm") 


res_dev <- residuals(mod, type = "deviance") / sqrt(1 - hatvalues(mod))

ggplot() +
  geom_point(aes(x= fitted(mod), y=res_dev))+
  theme_minimal()+
  geom_hline(yintercept=0)+
  geom_smooth(aes(x= fitted(mod), y=res_dev), color='#8B3E65', fill='#8B3E65', se=FALSE)+
  labs(x='Probabilidade estimada',y='Resíduos Deviance')

ggsave('dev2_est.pdf',width = 12, height = 8, units = "cm")


plot(cooks.distance(mod))
cooks.distance(mod)[which.max(cooks.distance(mod))]


# MAT

mod = glm(evasao ~ taxa_rep + n_trancamentos, data = treino_mat, family = 'binomial')

prob = predict(mod, newdata = teste_mat, type = "response")
roc = roc(teste_mat$evasao ~ prob)

ggroc(roc, colour = '#C49CB0', size = 1) +
  geom_segment(aes(x=1, xend=0, y=0, yend=1), color='lightgrey', linetype='dashed') +
  #ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')')) +
  labs(x='Especificidade',y='Sensibilidade')+
  theme_minimal() +
  annotate(geom = "text", x = 0.5, y = 0.5, label = paste0(round(roc$auc,3)), hjust = "left")

ggsave('roc_mat.pdf',width = 10, height = 8, units = "cm")


# Residuos
res_pearson <- residuals(mod, type = "pearson") / sqrt(1 - hatvalues(mod))

ggplot() +
  geom_point(aes(x= fitted(mod), y=res_pearson))+
  theme_minimal()+
  geom_hline(yintercept=0)+
  geom_smooth(aes(x= fitted(mod), y=res_pearson), color='#C49CB0', fill='#C49CB0', se=FALSE)+
  labs(x='Probabilidade estimada',y='Resíduos Pearson')

ggsave('dev1_mat.pdf',width = 12, height = 8, units = "cm")


res_dev <- residuals(mod, type = "deviance") / sqrt(1 - hatvalues(mod))

ggplot() +
  geom_point(aes(x= fitted(mod), y=res_dev))+
  theme_minimal()+
  geom_hline(yintercept=0)+
  geom_smooth(aes(x= fitted(mod), y=res_dev), color='#C49CB0', fill='#C49CB0', se=FALSE)+
  labs(x='Probabilidade estimada',y='Resíduos Deviance')

ggsave('dev2_mat.pdf',width = 12, height = 8, units = "cm")

