##====================================================================================================
#                                   Funções - Analise Descritiva TCC
##====================================================================================================
## Manipulando dados

# Indentificando alunos duplicados
alunos_dup = function(dados){
  
  aux = dados %>% distinct(Aluno, Id_pessoa, periodo_ingresso_curso, .keep_all = T) 
  
  aux1 = aux[which(duplicated(aux$Aluno)),]
  aux2 = aux[which(duplicated(data.frame(aux$cep,aux$nascimento,
                                         aux$ira,aux$periodo_ingresso_curso))),]
  
  print(paste0('Alunos duplicados: ',nrow(aux1)))
  print(paste0('CEPs e nascimento duplicados: ',nrow(aux2)))
  
  #aux$data_ingresso_curso = as.Date(aux$data_ingresso_curso,"%d/%m/%Y")
  
  aux3 = aux %>% filter(Aluno %in% aux1$Aluno) %>% group_by(Aluno,Id_pessoa) %>%
    filter(as.Date(data_ingresso_curso,"%d/%m/%Y")!=max(as.Date(data_ingresso_curso,"%d/%m/%Y")))
  
  
  return(dplyr::bind_rows(aux3,aux2) %>% 
           select(id,Aluno,Id_pessoa,periodo_ingresso_curso,periodo_saida_curso,
                  cep,nascimento,ira,forma_saida_curso) %>% arrange(cep))
}

# Identificando disciplinas duplicadas
disc_dup = function(dados){
  
  aux = dados %>% group_by(codigo_disciplina,nome_disciplina) %>% count()
  
  print(paste0('nº Códigos: ',length(unique(aux$codigo_disciplina)))) 
  print(paste0('nº Nomes: ',length(unique(aux$nome_disciplina)))) 
  
  aux2 = aux[c(which(duplicated(aux$codigo_disciplina)), 
               which(duplicated(aux$nome_disciplina))),]
  
  disc = aux %>% 
    filter(codigo_disciplina %in% aux2$codigo_disciplina |
             nome_disciplina %in% aux2$nome_disciplina) %>%
    arrange(codigo_disciplina)
  
  return(disc)
}

# Modificando o tipo de variável para valor numérico
tipo_var = function(dados){
  dados = dados %>% transform(ira = as.numeric(ira),
                              media_semestre_aluno = as.numeric(media_semestre_aluno),
                              min_cred_para_formatura = as.numeric(min_cred_para_formatura),
                              creditos_no_periodo = as.numeric(creditos_no_periodo),
                              creditos_disciplina = as.numeric(creditos_disciplina),
                              total_creditos_cursados_aluno = as.numeric(total_creditos_cursados_aluno),
                              creditos_aprovados_no_periodo = as.numeric(creditos_aprovados_no_periodo))
  return(dados)
}


# Filtro do período inicial de análise i, e gênero dos alunos
#   - i indica a decada dos anos 2000 (ex: i=08 -> 2008)
filtro = function(dados,i, generos=c('F','M')){ 
  
  anos = paste0('20',as.character(i:19))
  periodos = c(paste0(anos,'1'),paste0(anos,'2'))
  
  # filtrando o periodo de analise
  dados_filtrados = dados %>% 
    filter(periodo_ingresso_curso %in% periodos & genero %in% generos)
  
  # pegando o historico de todos os alunos ativos nesse periodo,
  # inclusive de anos anteriores ao periodo de analise
  
  # filtrando novamente o genero devido a aluna duplicada
  dados_filtrados2 = tipo_var(dados) %>% 
    filter(Aluno %in% dados_filtrados$Aluno & genero %in% generos)
  
  
  return(dados_filtrados2)
  
}


# Transformação dos bancos originais e criação das variáveis de interesse
info_alunas = function(dados){ 
  
  dados_alunas = dados %>% distinct(Aluno, periodo_ingresso_curso, .keep_all = T) %>% 
    mutate(id = 1:nrow(.)) %>% select('id',names(dados)[1:29])
  
  # adicionando coluna id no banco
  # tirando disciplinas cursadas depois de 20192
  dados = left_join(dados, dados_alunas[c('Aluno','periodo_saida_curso','id')],
                    by=c('Aluno','periodo_saida_curso'))%>%
    filter(periodo_cursou_disciplina!='20200') 
  
  # Evasão
  dados_alunas %<>% filter(forma_saida_curso!='Transferência') 
  dados_alunas$evasao = ifelse(dados_alunas$forma_saida_curso %in% c("ativo","Ativo","Formatura"),"Não","Sim")
  
  # Número de evasões anteriores
  evasoes = dados_alunas %>% 
    group_by(Aluno) %>% count() %>% mutate(n_evasao=n-1) %>% select(-n)
  #names(evasoes) = c('Aluno','n_evasao')
  
  dados_alunas = left_join(dados_alunas,evasoes,by='Aluno')%>% 
    mutate_at(vars('n_evasao'), ~replace_na(.,0))
  
  
  # Idade ao ingressar
  #   1 de março e 1 de agosto
  
  # Criando uma nova variável para identificar o dia de ingresso do aluno em relação ao semestre 
  dados_alunas$data_ingresso_curso = (paste0(ifelse(substring(dados_alunas$periodo_ingresso_curso,5,5) == 1, '01/03/', "01/08/"),
                                             substring(dados_alunas$periodo_ingresso_curso,1,4))) 
  
  # Criando uma nova variável para identificar a idade do aluno no ingresso
  dados_alunas$idade = floor(time_length(difftime(as.Date(dados_alunas$data_ingresso_curso,"%d/%m/%Y"), 
                                                  as.Date(dados_alunas$nascimento,"%d/%m/%Y")),"years"))
  
  # Número de trancamentos
  #   Quantas vezes um aluno trancou alguma disciplina? Considerar mencao TR ou TJ
  
  trancamentos = dados %>% filter(mencao_disciplina %in% c('TR','TJ')) %>%
    group_by(Aluno) %>% count()
  names(trancamentos) = c('Aluno','n_trancamentos')
  
  dados_alunas = left_join(dados_alunas,trancamentos,by='Aluno')%>% 
    mutate_at(vars('n_trancamentos'), ~replace_na(.,0))
  
  # as.numeric(trancamentos[which(trancamentos[,1]=='Aluno148953'),2])
  
  # Número de mencoes SR
  sem_rendimento = dados %>% filter(mencao_disciplina == 'SR') %>%
    group_by(Aluno) %>% count()
  names(sem_rendimento) = c('Aluno','n_sr')
  
  dados_alunas = left_join(dados_alunas,sem_rendimento,by='Aluno')%>% 
    mutate_at(vars('n_sr'), ~replace_na(.,0))
  
  
  # Taxa de reprovacao
  # total de creditos reprovados/total de creditos efetivamente cursados
  # SR,MI,II = reprovados; MM,MI,SS = aprovados; total = aprovados + reprovados
  
  tx_rep = dados %>% filter(mencao_disciplina %in% c('SR','II','MI','MM','MS','SS')) %>%
    group_by(Aluno) %>% summarise(total=sum(creditos_disciplina),
                               reprovados=sum(creditos_disciplina[mencao_disciplina %in% c('SR','II','MI')]),
                               taxa_rep = round(reprovados/total,3)) # count(mencao_disciplina)
  
  dados_alunas = left_join(dados_alunas,tx_rep[,c(1,4)],by='Aluno')%>% 
    mutate_at(vars('taxa_rep'), ~replace_na(.,0))
  
  # Reprovou Cálculo 1 (codigo = 113034 ou 200107 ou MAT002)
  # agrupar por Aluno para puxar o histórico de Cálculo 1
  calculo1 = dados %>% filter(codigo_disciplina %in% c('113034','200107','MAT002','MAT013') &
                                mencao_disciplina %in% c('SR','II','MI')) %>%
    group_by(Aluno,nome_disciplina) %>% count(mencao_disciplina)
  
  dados_alunas$rep_calculo1 = ifelse(dados_alunas$Aluno %in% calculo1$Aluno, 'Sim','Não')
  
  # Cursou verao
  
  verao = dados %>% filter(substring(periodo_cursou_disciplina,5,5) == 0) %>%
    group_by(Aluno,nome_disciplina) %>% count()
  
  dados_alunas$verao = ifelse(dados_alunas$Aluno %in% verao$Aluno, 'Sim','Não')
  
  
  # Número de semestres cursados
  
  dados_alunas$semestre_saida_curso = paste0(ifelse(substring(dados_alunas$periodo_saida_curso,5,5) == 1, '01/07/', "01/01/"),
                                             ifelse(substring(dados_alunas$periodo_saida_curso,5,5) == 1, 
                                                    substring(dados_alunas$periodo_saida_curso,1,4),
                                                    as.numeric(substring(dados_alunas$periodo_saida_curso,1,4))+1)) 
  
  
  dados_alunas$semestre_ingresso_curso = (paste0(ifelse(substring(dados_alunas$periodo_ingresso_curso,5,5) == 1, '01/01/', "01/07/"),
                                                 substring(dados_alunas$periodo_ingresso_curso,1,4))) 
  
  dados_alunas$n_semestres = round(time_length(difftime(as.Date(dados_alunas$semestre_saida_curso,"%d/%m/%Y"), 
                                                        as.Date(dados_alunas$semestre_ingresso_curso,"%d/%m/%Y")),"months"))/6
  
  
  # Retirando colunas 
  dados_alunas = dados_alunas %>%
    select(-c("periodo_cursou_disciplina","modalidade_disciplina",           
              "media_semestre_aluno","min_cred_para_formatura",         
              "creditos_no_periodo","total_creditos_cursados_aluno",  
              "codigo_disciplina","nome_disciplina","creditos_disciplina",             
              "mencao_disciplina","creditos_aprovados_no_periodo",
              "semestre_saida_curso","semestre_ingresso_curso"))
  
  return(dados_alunas)
  
}


# Reprovou obrigatoria
#   Quantas vezes o aluno reporvou disciplina obrigatoria?

rep_obr = function(dados,dados_alunas,obr){
  
  rep = dados %>% filter(codigo_disciplina %in% obr & mencao_disciplina %in% c('SR','II','MI')) %>% 
    group_by(Aluno) %>% count()
  names(rep) = c('Aluno','rep_obr')
  
  dados_alunas = left_join(dados_alunas,rep,by='Aluno')%>% 
    mutate_at(vars('rep_obr'), ~replace_na(.,0))
  
  return(dados_alunas)
}



##====================================================================================================
## Descritiva

# Tabela univariada
tab_uni = function(dados){
  
  dados$evasao = factor(dados$evasao, levels = c('Sim','Não'))
  tabela = c()
  variaveis = names(dados)[c(5,9,10,11,16,18,19,21,27,28,30,36)]
  
  for(var in variaveis){
    
    aux = t(rbind(table(dados[,var]),
                  paste0(round(prop.table(table(dados[,var])),4)*100,'%'),
                  rep(var,length(unique(dados[,var]))))
    )
    tabela = rbind(tabela,aux)
  }
  
  return(data.frame(variavel=tabela[,3],
                    categoria=row.names(tabela),
                    n=tabela[,1],
                    freq=tabela[,2])
  )
  
}


# Tabela bivaridada
#   Evasao vs variaveis
tab_biv = function(dados){
  
  dados$evasao = factor(dados$evasao, levels = c('Sim','Não'))
  tabela = data.frame(variavel='',categoria='',
                      `Sim`='',`Não`='',`Sim %`='',`Não %`='')
  variaveis = names(dados)[c(5,9,10,11,16,18,19,21,27,28,30,36)]
  
  
  for(var in variaveis){
    
    
    n = t(table(dados$evasao,dados[,var]))
    freq = t(round(prop.table(table(dados$evasao,dados[,var]),margin = 2), 4)*100)
    tab=cbind(n,matrix(paste0(freq,'%'),ncol=ncol(n),nrow=nrow(n),byrow = FALSE))
    
    tabela = rbind(tabela,
                   data.frame(variavel=rep(var,nrow(n)),
                              categoria=row.names(n),
                              `Sim`=tab[,1],`Não`=tab[,2],
                              `Sim %`=tab[,3],`Não %`=tab[,4])
    )
    
  }
  
  row.names(tabela) = NULL
  return(tabela[-1,])
  
}
