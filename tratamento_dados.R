##====================================================================================================
#                                       Manipulação dos dados TCC
##====================================================================================================
# Infos:

# Período de análise: 2011 a 2019

# Menção:
# CC <- Crédito Concedido 
# SR <- Sem Rendimento
# II <- Inferior
# MI <- Médio Inferior
# MM <- Médio
# MS <- Médio Superior
# SS <- Superior
# TJ <- Trancamento justificado
# TR <- Trancamento

# Bancos:
# CIC
# 1991/1 a 2019/2, 123126 linhas, 3614 alunos (388 mulheres)
# 2014/1 a 2019/2, 874 alunos (102 mulheres)

# EST
# 1998/2 a 2019/1, 10996 linhas, 446 alunos (140 mulheres)
# 2014/1 a 2019/2, 217 alunos (69 mulheres)

# MAT
# 1993/2 a 2019/1, 3284 linhas, 190 alunos (43 mulheres)
# 2014/1 a 2019/2, 80 alunos (20 mulheres)
# Obs.: Aluno109422 fez Bacharelado e mudou para Licenciatura

##====================================================================================================
library(tidyverse)
library(readxl)
library(magrittr)
library(lubridate)

setwd("D:/Unb/Disciplinas/EST/TCC")
source('funcoes.R', encoding='UTF-8') # Funções

cic = readRDS("Dados_TCC/computacao.rds") # read_xlsx("computacao_final.xlsx")[,1:27]
est = readRDS("Dados_TCC/estatistica.rds")# read_xlsx("est_final.xlsx")
mat = readRDS("Dados_TCC/matematica.rds") # read_xlsx("mat_final.xlsx")
 
# saveRDS(est,file='estatistica.rds')

glimpse(cic); glimpse(est); glimpse(mat)

##====================================================================================================
## Manipulando dados...


# Conferindo dados faltantes NAs ---------------------------------------------------------------------

colSums(is.na(est))
names(which(colSums(is.na(est))>0))  # quais colunas possuem NA?


# Filtrando colunas e padronizando nomes das colunas -------------------------------------------------

names(est) == names(mat)
names(cic) %in% names(mat)
names(est)[which(!names(est) %in% names(cic))]

#   banco do cic nao tem as colunas raca, modalidade_disciplina = criar com NAs
#   forma_ingresso_unb (cic) = a forma_ingresso_curso (mat e est)

cic$raca = rep(NA,nrow(cic)); cic$modalidade_disciplina = rep(NA,nrow(cic))
colnames(cic)[which(names(cic)=='forma_ingresso_unb')] = 'forma_ingresso_curso'
colnames(cic)[3] = 'ira'; colnames(cic)[23] = 'creditos_aprovados_no_periodo'
colnames(mat)[27] = 'creditos_aprovados_no_periodo'; colnames(est)[27] = 'creditos_aprovados_no_periodo'

mat %<>% select(names(cic)); est %<>% select(names(cic))


# Dados com padronização original  -------------------------------------------------------------------

est_orig = info_alunas(filtro(est,11)); mat_orig = info_alunas(filtro(mat,11))
cic_orig = info_alunas(filtro(cic,11))

# Problemas est "Aluno165881" "Aluno202400" 

# "Aluno165881" sao 2 alunos distintos (cep, nascimento e outros diferentes),
# mas com o mesmo registro!! - um deles tem historico ate 20212 (?) (2 homens)

# "Aluno202400" sao 2 alunos distintos (cep, nascimento e outros diferentes),
# mas com o mesmo registro!! - um deles consta como ativo! (1 mulher e 1 homem)

View(est %>% filter(Aluno %in% c("Aluno165881","Aluno202400")))
est_orig %>% filter(Aluno %in% c("Aluno165881","Aluno202400")) 

# retirando alunos problematicos
est_orig %<>% filter(! periodo_saida_curso %in% c('Ativo','20212')) 

# Problemas mat "Aluno109422" - consta como ativo

# "Aluno109422" sao 2 alunos distintos (cep, nascimento e outros diferentes),
# mas com o mesmo registro!! - um deles consta como ativo! (2 homens)

View(mat %>% filter(Aluno %in% c("Aluno109422")))
mat_orig %>% filter(Aluno %in% c("Aluno109422")) 

# retirando alunos problematicos
mat_orig %<>% filter(! periodo_saida_curso %in% c('Ativo')) 


# Retirar alunos duplicados
est_orig %<>% filter(! id %in% alunos_dup(est_orig)$id) 
mat_orig %<>% filter(! id %in% alunos_dup(mat_orig)$id) 
cic_orig %<>% filter(! id %in% alunos_dup(cic_orig)$id) 

# cic tem alunos duplicados escondidos - historicos passados, com matricula e id_pessoa diferentes
ceps = cic_orig$cep[which(duplicated(cic_orig$cep))]
datas = cic_orig$nascimento[which(duplicated(cic_orig$nascimento))]
ids = cic_orig %>% filter(cep %in% ceps & nascimento %in% datas & genero=='M') %>% 
  arrange(as.Date(data_ingresso_curso,"%d/%m/%Y")) %>% distinct(cep, nascimento, .keep_all = T) %>% pull(id)
retirar = cic_orig %>% filter(cep %in% ceps & nascimento %in% datas & genero=='M' & !(id %in% ids))%>% pull(id)

# 'Aluno238153','Aluno264959','Aluno235599','Aluno170106'
cic_orig = cic_orig %>% filter(! (id %in% retirar | Aluno %in% c("Aluno170106","Aluno238153")))

# Padronizando nomes dos cursos
mat_orig = mat_orig %>% filter(curso != "Matemática Licenciatura Notur")

cic_orig$curso = plyr::revalue(cic_orig$curso, c(`CIÊNCIA DA COMPUTAÇÃO`='Ciência da Computação'))
mat_orig$curso = plyr::revalue(mat_orig$curso, c(`Matemática Bacharelado Diurno`='Matemática'))
est_orig$curso = plyr::revalue(est_orig$curso, c(`ESTATÍSTICA`='Estatística'))


# Salvar dados
saveRDS(est_orig,file='Dados_TCC/est_original.rds')
saveRDS(mat_orig,file='Dados_TCC/mat_original.rds')
saveRDS(cic_orig,file='Dados_TCC/cic_original.rds')


# Padronizando valores das linhas --------------------------------------------------------------------

#   verificar se os codigos e nomes das disciplinas estao corretos
View(disc_dup(mat)); View(disc_dup(est)); View(disc_dup(cic))


#   verificar disciplinas comuns nos 3 cursos
disciplinas = unique(cic$codigo_disciplina[which(cic$codigo_disciplina %in% est$codigo_disciplina &
                            cic$codigo_disciplina %in% mat$codigo_disciplina)])

#   padronizar valores de colunas comuns nos 3 bancos
colunas = c("genero","modalidade_disciplina","sistema_cotas","cota","Escola","curso",
            "forma_ingresso_curso","forma_saida_curso","mencao_disciplina","raca" )

#   valores possiveis das colunas em cada banco
valores_cic = rapply(cic[which(names(cic) %in% colunas)], function(x) unique(x))
valores_mat = rapply(mat[which(names(mat) %in% colunas)], function(x) unique(x))
valores_est = rapply(est[which(names(est) %in% colunas)], function(x) unique(x))

View(valores_cic); View(valores_mat); View(valores_est)


#   sistema de cotas
cic$sistema_cotas = plyr::revalue(cic$sistema_cotas, c(Nao='Não'))
mat$sistema_cotas = plyr::revalue(mat$sistema_cotas, c(Nao='Não',`Nã`='Não'))
est$sistema_cotas = plyr::revalue(est$sistema_cotas, c(Nao='Não',`Nã`='Não'))

#   cota
cic$cota = ifelse(cic$sistema_cotas=='Não','Sistema Universal','Sistema Cotas não especificado')
mat$cota = ifelse(mat$sistema_cotas=='Não','Sistema Universal',mat$cota)
est$cota = ifelse(est$sistema_cotas=='Não','Sistema Universal',est$cota)

mat$cota = plyr::revalue(mat$cota, c(`Candidato Negro`='Negro',
                                     `Egresso de escola pública, rend`='Egresso de Escola Pública',
                                     `Egresso de escola pública`='Egresso de Escola Pública',
                                     `Escola Púb. Alta Renda-Não PPI`='Escola Pública Alta Renda-Não PPI'))

est$cota = plyr::revalue(est$cota, c(`Candidato Negro`='Negro',
                                     `Egresso de escola pública, rend`='Egresso de Escola Pública',
                                     `Egresso de escola pública`='Egresso de Escola Pública',
                                     `Escola Púb. Alta Renda-Não PPI`='Escola Pública Alta Renda-Não PPI',
                                     `Escola Púb Baixa Renda-Não PPI`='Escola Pública Baixa Renda-Não PPI'))


#   escola
cic$Escola = plyr::revalue(cic$Escola, c(Publica='Pública'))
mat$Escola = plyr::revalue(mat$Escola, c(Publica='Pública'))
est$Escola = plyr::revalue(est$Escola, c(Publica='Pública',`Não informado`='Sem informação'))

#   curso
View(mat %>% filter(curso=='Matemática Licenciatura Notur')) #Aluno109422

cic$curso = plyr::revalue(cic$curso, c(`CIÊNCIA DA COMPUTAÇÃO`='Ciência da Computação'))
mat$curso = plyr::revalue(mat$curso, c(`Matemática Bacharelado Diurno`='Matemática',
                                       `Matemática Licenciatura Notur`='Matemática'))
est$curso = plyr::revalue(est$curso, c(`ESTATÍSTICA`='Estatística'))

#   raca
#   branca, amarela, parda, preta, indígena, Sem Informação
#   segundo IBGE, negro = preto ou pardo

mat$raca = plyr::revalue(mat$raca, c(`Não cadastrada`='Sem Informação',
                                     `Não Cadastrada`='Sem Informação',
                                     `Não dispõe da informação`='Sem Informação',
                                     `Não Informado`='Sem Informação',
                                     `Branco`='Branca',
                                     `Negro`='Negra',
                                     `Pardo`='Parda'))

est$raca = plyr::revalue(est$raca, c(`Não cadastrada`='Sem Informação',
                                     `Não Cadastrada`='Sem Informação',
                                     `Não dispõe da informação`='Sem Informação',
                                     `Não Informado`='Sem Informação',
                                     `Branco`='Branca',
                                     `Negro`='Negra',
                                     `Pardo`='Parda',
                                     `Não quero declarar raça/cor`='Sem Informação',
                                     `Amarelo (de origem oriental)`='Amarela'))


#   forma de ingresso no curso

cic$forma_ingresso_curso = plyr::revalue(cic$forma_ingresso_curso, 
                                   c(`Programa de Avaliação Seriada`='PAS',
                                     `Sisu-Sistema de Seleção Unificada`='SISU/ENEM',
                                     `Enem  UnB`='SISU/ENEM',
                                     `Portador Diplom Curso Superior`='Diploma Curso Superior',
                                     `Convênio-Int`='Convênio',
                                     `Convênio - Andifes`='Convênio',
                                     `Transferência Facultativa`='Transferência',
                                     `Transferência Obrigatória`='Transferência'
                                     #`Acordo Cultural-PEC-G`='',
                                     #`PEC-Peppfol-Graduação`='',
                                     #`Matrícula Cortesia`=''
                                     ))

mat$forma_ingresso_curso = plyr::revalue(mat$forma_ingresso_curso, 
                                         c(`Programa de Avaliação Seriada`='PAS',
                                           `Sisu-Sistema de Seleção Unificada`='SISU/ENEM',
                                           `Enem  UnB`='SISU/ENEM',
                                           `Portador Diplom Curso Superior`='Diploma Curso Superior',
                                           `Transferência Facultativa`='Transferência',
                                           `Duplo Curso`='Duplo Curso',
                                           `Dupla Diplomação`='Duplo Curso',
                                           `Dupla Habilitação`='Duplo Curso',
                                           `Mudança de Habilitação`='Mudança de Curso',
                                           `Mudança de Curso`='Mudança de Curso'
                                           ))

est$forma_ingresso_curso = plyr::revalue(est$forma_ingresso_curso, 
                                         c(`Programa de Avaliação Seriada`='PAS',
                                           `Sisu-Sistema de Seleção Unificada`='SISU/ENEM',
                                           `Enem  UnB`='SISU/ENEM',
                                           `Portador Diplom Curso Superior`='Diploma Curso Superior',
                                           `Transferência Facultativa`='Transferência',
                                           `Transferência Obrigatória`='Transferência'
                                           #`Mudança de Curso`=''
                                           ))

# forma saida curso

cic$forma_saida_curso = plyr::revalue(cic$forma_saida_curso, 
                                      c(`Repr 3 vezes na mesma disc obr`='Desligamento - Rendimento',
                                        `Deslig - não cumpriu condição`='Desligamento - Rendimento',
                                        `Desligamento Voluntário`='Desligamento - Voluntário',
                                        `Desligamento Decisão  Judicial`='Desligamento - Decisão Judicial',
                                        `Desligamento por Força de Intercãmbio`='Desligamento - Intercâmbio',
                                        `Desligamento Jubilamento`='Desligamento - Jubilamento',
                                        `Desligamento-Força de Convênio`='Desligamento - Convênio',
                                        `ativo`='Ativo'
                                      ))


mat$forma_saida_curso = plyr::revalue(mat$forma_saida_curso, 
                                      c(`Repr 3 vezes na mesma disc obr`='Desligamento - Rendimento',
                                        `Deslig - não cumpriu condição`='Desligamento - Rendimento',
                                        `Desligamento Voluntário`='Desligamento - Voluntário'
                                      ))


est$forma_saida_curso = plyr::revalue(est$forma_saida_curso, 
                                      c(`Repr 3 vezes na mesma disc obr`='Desligamento - Rendimento',
                                        `Deslig - não cumpriu condição`='Desligamento - Rendimento',
                                        `Desligamento Voluntário`='Desligamento - Voluntário',
                                        `CONCLUÍDO`='Formatura'
                                      ))

# periodo de saida do curso
#   cic tem alunos ainda ativos, alguns com disciplinas cursadas em 20200 (verão)
#   outros sairam no curso em 20201 ou constam como ativos - considerar como ativos em 20192
cic %>% filter(periodo_cursou_disciplina=='20200')
cic %>% filter(periodo_saida_curso %in% c('20201','ativo'))

cic$periodo_saida_curso = plyr::revalue(cic$periodo_saida_curso,c(ativo='20192',
                                                                  `20201`='20192'))

est$periodo_saida_curso = plyr::revalue(est$periodo_saida_curso,c(Ativo='20192'))

mat$periodo_saida_curso = plyr::revalue(mat$periodo_saida_curso,c(Ativo='20192'))


# para substituir NAs
cic = cic %>% tidyr::replace_na(list(raca='Sem Informação', mencao_disciplina='Sem Informação',
                      modalidade_disciplina='Sem Informação',forma_ingresso_curso='Sem Informação',
                      Escola='Sem Informação'))

est = est %>% tidyr::replace_na(list(raca='Sem Informação', mencao_disciplina='Sem Informação',
                                     modalidade_disciplina='Sem Informação',forma_ingresso_curso='Sem Informação',
                                     Escola='Sem Informação'))

mat = mat %>% tidyr::replace_na(list(raca='Sem Informação', mencao_disciplina='Sem Informação',
                                     modalidade_disciplina='Sem Informação',forma_ingresso_curso='Sem Informação',
                                     Escola='Sem Informação'))


# Filtrando linhas repetidas e periodo de analise ---------------------------------------------------

est_filtrado = filtro(est,11,c('F')); mat_filtrado = filtro(mat,11,c('F'))
cic_filtrado = filtro(cic,11,c('F'))

names(which(colSums(is.na(est_filtrado))>0))  # quais colunas possuem NA?
names(which(colSums(is.na(mat_filtrado))>0))  # quais colunas possuem NA?
names(which(colSums(is.na(cic_filtrado))>0))  # quais colunas possuem NA?


# Banco de dados apenas com as infos das alunas -----------------------------------------------------

est_alunas = info_alunas(est_filtrado); mat_alunas = info_alunas(mat_filtrado)
cic_alunas = info_alunas(cic_filtrado)


# Retirando alunas duplicadas -----------------------------------------------------------------------

# mat = 3 (38), est = 2 (73), cic = 59 (147)
View(alunos_dup(mat_alunas)); View(alunos_dup(est_alunas))
View(alunos_dup(cic_alunas))

# mat: Aluno108893, Aluno117784
# est: Aluno145177, Aluno156942, Aluno202400

# Aluno117784 se formou em Matemática e depois cursou Estatística (e abandonou)

dup_mat = mat %>% filter(Aluno %in% c('Aluno108893', 'Aluno117784')) %>% arrange(Aluno)
dup_est = est %>% filter(Aluno %in% c('Aluno145177', 'Aluno156942', 'Aluno202400')) %>% arrange(Aluno)

# nos registros mais recentes o sistema não puxa o historico de periodos anteriores...
View(dup_est %>% filter(Aluno=='Aluno145177') %>%
       select(Aluno,ira,periodo_ingresso_curso,periodo_saida_curso,
              periodo_cursou_disciplina,codigo_disciplina,nome_disciplina,mencao_disciplina))


# Banco do cic possui alunas com nascimento, CEP e ira identicos,
# mas matriculas e id_pessoa diferentes!!

View(cic_alunas[which(duplicated(data.frame(cic_alunas$cep,cic_alunas$nascimento,cic_alunas$ira))),])

# Conferindo as alunas que provavelmente evadiram anteriormente, 
# mas o historico consta em registros distintos 
View(cic %>% filter(Aluno %in% c('Aluno238153','Aluno264959','Aluno235599','Aluno170106')))

# Corrigindo no banco do cic as alunas com historico de evasao
cic$Aluno[which(cic$Aluno=='Aluno238153')] = 'Aluno264959'
cic$Aluno[which(cic$Aluno=='Aluno170106')] = 'Aluno235599'

cic_alunas = info_alunas(filtro(cic,11,c('F')))

View(cic_alunas %>% filter(Aluno %in% c('Aluno238153','Aluno264959','Aluno235599','Aluno170106')))


# Verificar nº de semestres cursados e idade ao ingressar entre 
# as alunas com historico de evasao

mat_alunas %>% filter(Aluno %in% c('Aluno108893', 'Aluno117784')) %>%  
  group_by(Aluno) %>% 
  summarise(idade = min(idade), semestres = sum(n_semestres))

est_alunas %>% filter(Aluno %in% c('Aluno145177', 'Aluno156942')) %>%  
  group_by(Aluno) %>% 
  summarise(idade = min(idade), semestres = sum(n_semestres))

cic_alunas %>% filter(Aluno %in% c('Aluno264959','Aluno235599')) %>%  
  group_by(Aluno) %>% 
  summarise(idade = min(idade), semestres = sum(n_semestres))


# Retirar alunas duplicadas
est_alunas %<>% filter(! id %in% alunos_dup(est_alunas)$id) 
mat_alunas %<>% filter(! id %in% alunos_dup(mat_alunas)$id) 
cic_alunas %<>% filter(! id %in% alunos_dup(cic_alunas)$id) 


# Criando variáveis específicas de cada curso -------------------------------------------------------

# Curriculo
# est: 2014.1; mat: 2011.2; cic: 2015.2

# Mat e Est 
#   a priori só possuem curriculo novo
#   Avaliar as alunas com historico anterior a data de atualizacao do curriculo
View(mat_filtrado %>% filter(Aluno %in% c('Aluno108893', 'Aluno117784')) %>% arrange(Aluno))
View(est_filtrado %>% filter(Aluno %in% c('Aluno145177', 'Aluno156942', 'Aluno202400')) %>% arrange(Aluno))

View(est_filtrado %>% filter(periodo_ingresso_curso=='20132') %>% arrange(Aluno))

est_alunas$curriculo = rep('Novo',nrow(est_alunas))
mat_alunas$curriculo = rep('Novo',nrow(mat_alunas))

# Cic
#   Curriculo antigo: alunos que sairam do curso antes de 2015.2
#   Curriculo novo: alunos que entraram a partir de 2015.2
#   Avaliar alunas que entraram antes de 2015.2 mas sairam depois de 2015.2
#   e olhar o mín. de créditos para a formatura (214 no novo curriculo)

cic_filtrado = filtro(cic,11,c('F'))

unique(cic_filtrado$min_cred_para_formatura)
View(cic_filtrado %>% filter(min_cred_para_formatura==214))

cic_filtrado$data_ingresso_curso = as.Date(paste0(ifelse(substring(cic_filtrado$periodo_ingresso_curso,5,5) == 1, '01/03/', "01/08/"),
                                                  substring(cic_filtrado$periodo_ingresso_curso,1,4)),"%d/%m/%Y") 

cic_filtrado$data_saida_curso = as.Date(paste0(ifelse(substring(cic_filtrado$periodo_saida_curso,5,5) == 1, '01/03/', "01/08/"),
                                               substring(cic_filtrado$periodo_saida_curso,1,4)),"%d/%m/%Y") 

# A aluna cursou as disciplinas exclusivas do curriculo novo?
cic_filtrado %>% group_by(Aluno) %>% 
  mutate(disc_novas=ifelse(any(codigo_disciplina %in% c('113476','116726','113468')),'Sim','Não'))


curriculo = cic_filtrado %>% mutate(curriculo = case_when(
  data_ingresso_curso >= as.Date('01/08/2015',"%d/%m/%Y") ~ 'Novo',
  data_saida_curso <= as.Date('01/08/2015',"%d/%m/%Y") ~ 'Antigo',
  TRUE ~ ifelse(min_cred_para_formatura==214,'Novo','Antigo')
)) %>% group_by(Aluno,curriculo) %>% count()

cic_alunas = left_join(cic_alunas,curriculo[,1:2],by='Aluno')

cic_alunas %>% filter(Aluno %in% c('Aluno264959','Aluno235599'))
cic_alunas$curriculo[which(cic_alunas$Aluno=='Aluno235599')] = 'Novo'



# Reprovação nas disciplinas obrigatorias do 1º semestre

View(cic %>% group_by(codigo_disciplina,nome_disciplina) %>% count())

# Calculo 1: '113034','200107','MAT002','MAT013'
# Computação em Estatística 1: '106755','EST000'
# Estatística Exploratória: '115118','EST003'
# Introdução à Ciência da Computação: '113913','CIC000' 
# Introdução à Probabilidade: '115924','EST006' 

# Calculo 1: '113034','200107','MAT002','MAT013'
# Introdução à Ciência da Computação: '113913','CIC000' 

# Calculo 1: '113034','200107','MAT002','MAT013'
# Algoritmos e Programação de Computadores: '113476'
# Informática e Sociedade: '116726'
# Introdução aos Sistemas Computacionais: '113468'

# Calculo 1: '113034','200107','MAT002','MAT013'
# Computação Básica: '116301'
# Física 1: '118001'
# Física 1 Experimental: '118010'
# Inglês Instrumental 1: '145971'
# Leitura e Produção de Textos: '140481'

obr_est = c('113034','200107','MAT002','MAT013',
            '106755','EST000','115118','EST003',
            '113913','CIC000','115924','EST006')

obr_mat = c('113034','200107','MAT002','MAT013',
            '113913','CIC000')

obr_cic_novo = c('113034','200107','MAT002','MAT013',
            '113476','116726','113468')

obr_cic_antigo = c('113034','200107','MAT002','MAT013',
                 '116301','118001','118010','145971','140481')


est_alunas = rep_obr(est_filtrado,est_alunas,obr_est)
mat_alunas = rep_obr(mat_filtrado,mat_alunas,obr_mat)

cic_alunas = rbind(rep_obr(cic_filtrado,cic_alunas[which(cic_alunas$curriculo=='Novo'),],obr_cic_novo),
                   rep_obr(cic_filtrado,cic_alunas[which(cic_alunas$curriculo=='Antigo'),],obr_cic_antigo)
)


# CEP e Local de residencia -------------------------------------------------------------------------

library(ggmap)
citation("ggmap")

# banco dos ceps para consulta do local
cep = rbind(cic_alunas[,c(13,1,2,7)],mat_alunas[,c(13,1,2,7)],est_alunas[,c(13,1,2,7)])
writexl::write_xlsx(cep,path = 'D:/Unb/Disciplinas/EST/TCC/Dados_TCC/cep.xlsx')

# codigo Conculta_CEP.py para extrair o endereço
local = read.csv('D:/Unb/Disciplinas/EST/TCC/Dados_TCC/local.csv', encoding = 'UTF-8')


# conferindo localidades
unique(local$localidade)
local[which(local$localidade==' '),] # ceps invalidos ou NA
local[which(local$localidade %in% c('Barreiras','Cidade Ocidental','Formosa','Recife',
                                    'Rio de Janeiro','Uberlândia','Valparaíso de Goiás')),]

local$bairro = as.character(local$bairro)
aux = which(local$bairro %in% c('Sul','Norte'))
local$bairro[aux] = paste0('Águas Claras ',local$bairro[aux])

dist = local[which(! local$localidade %in% c(' ','Barreiras','Recife','Rio de Janeiro','Uberlândia')),]

# Definir o endereço de origem e destino
destino = "70910-900"
chave = "chave" # colocar aqui sua chave de acesso do google!

register_google(key = chave) 


# Calcular a distancia 
# https://rdrr.io/cran/ggmap/man/mapdist.html

dist$endereco = paste(dist$cep,dist$bairro,
                      dist$logradouro,dist$localidade,
                      sep = ", ")


distancia = mapdist(from=dist$endereco,to='70910900, Universidade de Brasília, Brasília', mode='driving')
distancia = distancia %>% distinct()

saveRDS(distancia,file='D:/Unb/Disciplinas/EST/TCC/Dados_TCC/distancia.rds')


# Obter as coordenadas dos CEPs - nao eh necessario!
coord_origem = geocode(dist$endereco)

dist$coordenadas = paste0(coord_origem$lat, ", ", coord_origem$lon)

coordenadas = cbind(dist$cep,coord_origem) %>% distinct()
names(coordenadas) = c('cep','lon','lat')

coord_destino = geocode('70910900') # -15.76562 -47.87170
coord_destino = '-15.76562+-47.87170'


library(gmapsdistance)
gmapsdistance::set.api.key(chave)

coordenadas = coordenadas %>%
  mutate(origem = paste0(lat, "+", lon),
         destino = rep(coord_destino,180))

coordenadas$distancia = NA
coordenadas$tempo = NA

for(i in c(1:nrow(coordenadas))){
  
  coordenadas$distancia[i] = gmapsdistance::gmapsdistance(coordenadas$origem[i],
                                                          coord_destino, mode='driving')$Distance
  coordenadas$tempo[i] = gmapsdistance::gmapsdistance(coordenadas$origem[i],
                                                      coord_destino, mode='driving')$Time
}

saveRDS(coordenadas, file = 'Dados_TCC/coordenadas.rds')


# algumas coordenadas estao erradas!!
cep_errados = c('73252200','73251901','73090909','73090914','73090918','73091900',
                '72425040','73062705','72415504','72425110')
coordenadas %>% filter(cep %in% cep_errados) %>% arrange(distancia)

# 73252200 = -15.684703587242707, -47.82719494906916
# 73251901 = -15.691117369079407, -47.82534933925685
# 73090909 = -15.652509196410845, -47.84248700044499
# 73090914 = -15.646939626250315, -47.836193061099046
# 73090918 = -15.6673978445169, -47.83389319087211
# 73091900 = -15.676572934579887, -47.827791944956815
# 72425040, = -16.00652056457955, -48.07157088147759
# 73062705 = -15.645536749894427, -47.817483515262694
# 72415504 = -16.02537817630345, -48.08154236058674
# 72425110 = -16.012581557291448, -48.068941657044576

coord_certas = c('-15.68470+-47.82719','-15.69111+-47.82535','-15.652509+-47.842487',
                 '-15.646939+-47.836193','-15.667397+-47.833893','-15.676572+-47.827791',
      '-16.00652+-48.07157','-15.64553+-47.81748','-16.025378+-48.081542','-16.01258+-48.06894')
dist_certas = c(); tempo_certos = c()

for(i in c(1:length(coord_certas))){
  dist_certas[i] = gmapsdistance::gmapsdistance(coord_certas[i],
                               coord_destino, mode='driving')$Distance
  tempo_certos[i] = gmapsdistance::gmapsdistance(coord_certas[i],
                               coord_destino, mode='driving')$Time
  
}

coord = data.frame(cep_errados,coord_certas,dist_certas,tempo_certos)

# Criando colunas dos locais de residencia e distancias nos bancos finais

distancia = readRDS("Dados_TCC/distancia.rds")
endereco = unlist(strsplit(distancia$from, ','))
cep = endereco[str_detect(endereco, '^[:digit:]') & str_length(endereco)>1]
bairro = endereco[which(endereco %in% cep)+1]

distancia$cep = cep; distancia$bairro = str_trim(bairro)


# Juntando bancos
est_alunas = left_join(est_alunas,distancia[,c('km','minutes','cep','bairro')],by='cep')
mat_alunas = left_join(mat_alunas,distancia[,c('km','minutes','cep','bairro')],by='cep')
cic_alunas = left_join(cic_alunas,distancia[,c('km','minutes','cep','bairro')],by='cep')


for(i in c(1:length(coord_certas))){
  cic_alunas$km[which(cic_alunas$cep==coord$cep_errados[i])] = coord$dist_certas[i]/1000
  cic_alunas$minutes[which(cic_alunas$cep==coord$cep_errados[i])] = coord$tempo_certos[i]/60
  
  est_alunas$km[which(est_alunas$cep==coord$cep_errados[i])] = coord$dist_certas[i]/1000
  est_alunas$minutes[which(est_alunas$cep==coord$cep_errados[i])] = coord$tempo_certos[i]/60
  
  mat_alunas$km[which(mat_alunas$cep==coord$cep_errados[i])] = coord$dist_certas[i]/1000
  mat_alunas$minutes[which(mat_alunas$cep==coord$cep_errados[i])] = coord$tempo_certos[i]/60
  
}


# RA e Renda ----------------------------------------------------------------------------------------

# Mapa da Riqueza FGV: https://cps.fgv.br/riqueza
# faixa de renda por classe: https://cps.fgv.br/qual-faixa-de-renda-familiar-das-classes

ra = matrix(c(
'Plano Piloto', 'Renda Alta',
'Lago Sul', 'Renda Alta', 
'Lago Norte', 'Renda Alta', 
'Jardim Botânico', 'Renda Alta', 
'Sudoeste/Octogonal', 'Renda Alta',  
'Park Way', 'Renda Alta', 
'Fercal', 'Renda Baixa',
'Águas Claras', 'Renda Alta',
'Sobradinho', 'Renda Média-Alta',
'Guará', 'Renda Média-Alta',
'Cruzeiro', 'Renda Média-Alta',  
'Vicente Pires', 'Renda Média-Alta',
'Núcleo Bandeirante', 'Renda Média-Alta',
'Taguatinga', 'Renda Média-Alta',
'Gama', 'Renda Média-Baixa',
'Candangolândia', 'Renda Média-Alta',
'Riacho Fundo I','Renda Média-Baixa',
'Brazlândia', 'Renda Baixa',
'Samambaia', 'Renda Média-Baixa', 
'Santa Maria', 'Renda Média-Baixa',
'Planaltina', 'Renda Baixa',
'Ceilândia', 'Renda Média-Baixa',
'Recantos das Emas', 'Renda Baixa',
'Riacho Fundo II', 'Renda Média-Baixa', 
'Paranoá', 'Renda Baixa', 
'São Sebastião', 'Renda Baixa', 
'Sobradinho II', 'Renda Média-Baixa',  
'SIA', 'Renda Média-Alta',
'Varjão', 'Renda Baixa',
'Itapoã', 'Renda Baixa',
'Arniqueira', 'Renda Média-Alta',
'Sol Nascente/Pôr do Sol', 'Renda Baixa',
'Estrutural/SCIA', 'Renda Baixa',
'Outra', 'Sem Informação'),
ncol=2,nrow = 34, byrow = TRUE
)

renda = data.frame(ra)
names(renda) = c('ra','renda_media')

local_renda = data.frame(bairro = unique(c(cic_alunas$bairro,est_alunas$bairro,mat_alunas$bairro)))

local_renda = local_renda %>% mutate(ra = case_when(
  bairro %in% c("Asa Sul","Asa Norte","Setor Noroeste",
                "Granja do Torto","Campus Universitário Darcy Ribeiro") ~ 'Plano Piloto',
  bairro %in% c("Setor de Habitações Individuais Sul","Setor de Mansões Dom Bosco") ~ 'Lago Sul',  
  bairro %in% c("Setor de Habitações Individuais Norte") ~ 'Lago Norte',  
  bairro %in% c("Setor Habitacional Jardim Botânico","Jardins Mangueiral") ~ 'Jardim Botânico', 
  bairro %in% c("Área Octogonal","Setor Sudoeste") ~ 'Sudoeste/Octogonal',   
  bairro %in% c("Park Way") ~ 'Park Way',  
  #bairro %in% c() ~ 'Fercal', 
  bairro %in% c("Águas Claras Sul","Águas Claras Norte") ~ 'Águas Claras', 
  bairro %in% c("Sobradinho","Setor de Mansões de Sobradinho","Setor Habitacional Contagem",
                "Grande Colorado","Região dos Lagos") ~ 'Sobradinho', 
  bairro %in% c("Guará I","Guará II") ~ 'Guará', 
  bairro %in% c("Cruzeiro Novo") ~ 'Cruzeiro',   
  bairro %in% c("Setor Habitacional Vicente Pires - Trecho 3","Setor Habitacional Vicente Pires") ~ 'Vicente Pires', 
  bairro %in% c("Núcleo Bandeirante") ~ 'Núcleo Bandeirante', 
  bairro %in% c("Taguatinga Sul","Taguatinga Norte") ~ 'Taguatinga', 
  bairro %in% c("Setor Industrial","Setor Oeste","Setor Sul") ~ 'Gama', 
  bairro %in% c("Candangolândia") ~ 'Candangolândia', 
  bairro %in% c("Riacho Fundo I") ~ 'Riacho Fundo I',
  #bairro %in% c() ~ 'Brazlândia', 
  bairro %in% c("Samambaia Sul","Samambaia Norte") ~ 'Samambaia',  
  bairro %in% c("Santa Maria") ~ 'Santa Maria', 
  bairro %in% c("Setor Tradicional","Setor Residencial Leste") ~ 'Planaltina', 
  bairro %in% c("Ceilândia Sul","Ceilândia Norte") ~ 'Ceilândia', 
  bairro %in% c("Recanto das Emas") ~ 'Recantos das Emas', 
  bairro %in% c("Riacho Fundo II") ~ 'Riacho Fundo II', 
  bairro %in% c("Paranoá") ~ 'Paranoá',  
  bairro %in% c("Setor Residencial Oeste","Vila Nova") ~ 'São Sebastião',  
  #bairro %in% c() ~ 'Sobradinho II',   
  #bairro %in% c() ~ 'SIA', 
  bairro %in% c("Varjão") ~ 'Varjão', 
  bairro %in% c("Fazendinha","Del Lago I") ~ 'Itapoã', 
  #bairro %in% c() ~ 'Arniqueira', 
  bairro %in% c("Setor Habitacional Pôr do Sol") ~ 'Sol Nascente/Pôr do Sol', 
  bairro %in% c("Setor Leste") ~ 'Estrutural/SCIA', 
  TRUE ~ 'Outra' # Octogonal, Formosa, Valparaíso, outros estados
                 # Jardim Ipanema, Residencial Villa Suiça, Área Rural de Formosa
))

local_renda$bairro[which(!local_renda$ra %in% renda$ra)]

# refazer distancia:
# 72445060, 72265040 Setor Industrial - Gama
# 73330041 Setor Tradicional - Planaltina
# 73360504 Setor Residencial Leste - Planaltina

# Setor Oeste pode ser Gama (72425040, 72425110) ou Sobradinho II (73062705) 
# - renda eh a mesma, tanto faz

cic_alunas %>% filter(bairro=='Setor Sul')

# PDAD Codeplan: https://www.codeplan.df.gov.br/wp-content/uploads/2022/05/Apresentacao_RAs.pdf

# Renda
local_renda = left_join(local_renda,renda,by='ra')


est_alunas = left_join(est_alunas,local_renda,by='bairro')
mat_alunas = left_join(mat_alunas,local_renda,by='bairro')
cic_alunas = left_join(cic_alunas,local_renda,by='bairro')

# Salvando bancos finais ----------------------------------------------------------------------------

# cic ainda ta com uma duplicata (?)
cic_alunas = cic_alunas[-which(duplicated(cic_alunas$Aluno)),] 

saveRDS(est_alunas,file='Dados_TCC/est.rds')
saveRDS(mat_alunas,file='Dados_TCC/mat.rds')
saveRDS(cic_alunas,file='Dados_TCC/cic.rds')

