# Código para Análise dos Microdados do Censo Escolar
Repositório códigos dados educacionais

Carregando os pacotes necessarios para  realizar tratamento na base de dados

```
library(dplyr)
```

A seguir vamos importar duas bases de dados, uma de matrículas para o Centro Oeste e outra de Turmas.
Vamos utilizar essa base como referência da programação.

```
MATRICULA_CO <- read.csv("~/CensoEscolar/2019/microdados_educacao_basica_2019/microdados_educacao_basica_2019/DADOS/MATRICULA_CO.CSV", sep="|")

TURMAS <- read.csv("~/Censo Escolar/2019/microdados_educacao_basica_2019/microdados_educacao_basica_2019/DADOS/TURMAS.CSV", sep="|")
```

Agora estou salvando os dados num objeto chamdo co. Depois utilizo a função `names` para ver as variáveis da base de dados.
```
co = MATRICULA_CO

names(co)
```

Agora utilizando as indicações de filtros do INEP, pego as matrículas do Centro Oeste.

```
co = co %>% 
  filter(TP_TIPO_ATENDIMENTO_TURMA %in% c(1,2)) 
```
Conferi o resultado com as sinopses estatísticas do censo escolar. 
O número de matrícula foi de 3.666.663, o mesmo da sinopse  

Salvei a base de dados TURMAS num objeto chamado turma. Em seguida, visualizei os nomes das 
variáveis na base de dados.

```
turma = TURMAS
names(turma)
```

Agora fazer um filtro para GOIAS e Juntar com a base de turmas em seguida

```
go = co %>% 
  filter(CO_UF == 52)
```

Pegando somente a variavel tx_hr_inicial e final para fazer o turno

```
turma = turma %>% select(TX_HR_INICIAL, ID_TURMA)

goias = go %>% 
  inner_join(turma, by ='ID_TURMA')

head(goias)

co = goias

names(co)
```

# Desigualdade racial

INDICADOR 01: Percentual de matrículas no Ensino Médio por Cor ou Raça.  

DADO CONFERIDO PARA GOIAS NO SITE, CONFERE COM O QUE TA LA

```
co %>% 
  filter(TP_COR_RACA %in% c(1,2,3)) %>% 
  filter(TP_ETAPA_ENSINO %in% c(25:38)) %>%
  filter(TP_DEPENDENCIA %in% c(1:3)) %>% 
  mutate(raca_cor = ifelse(TP_COR_RACA %in% c(2,3),'NEGROS',
                           ifelse(TP_COR_RACA %in% c(1),'BRANCOS','NSA'))) %>% 
  group_by(raca_cor) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(percentual = n/sum(n))
```


INDICADOR 02: Percentual de Matrpciulas no Ensino Médio por Cor ou Raça em estudantes com idades entre 15 e 17 anos  
DADO CONFERIDO PARA GOIAS NO SITE, CONFERE COM O QUE TA LA

```
co %>% 
  filter(TP_COR_RACA %in% c(1,2,3)) %>% 
  filter(TP_ETAPA_ENSINO %in% c(25:38)) %>%
  filter(CO_UF %in% c(52)) %>% 
  filter(TP_DEPENDENCIA %in% c(1:3)) %>% 
  mutate(raca_cor = ifelse(TP_COR_RACA %in% c(2,3),'NEGROS',
         ifelse(TP_COR_RACA %in% c(1),'BRANCOS','NSA')),
  idades = ifelse(NU_IDADE_REFERENCIA %in% c(15,16,17),'15 a 17','outros')) %>% 
  group_by(raca_cor,idades) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(raca_cor) %>% 
  mutate(p = n/sum(n)) %>% 
  filter(idades == '15 a 17')
```
INDICADOR 03 % DE MAT NO EM POR COR/RAÇA por turno  
bATE COM O RESULTADO DO SITE
```
co %>% 
  filter(TP_COR_RACA %in% c(1,2,3)) %>% 
  filter(TP_ETAPA_ENSINO %in% c(25:38)) %>%
  filter(CO_UF %in% c(52)) %>% 
  filter(TP_DEPENDENCIA %in% c(1:3)) %>% 
  mutate(raca_cor = ifelse(TP_COR_RACA %in% c(2,3),'NEGROS',
      ifelse(TP_COR_RACA %in% c(1),'BRANCOS','NSA')),
      idades = ifelse(NU_IDADE_REFERENCIA %in% c(15,16,17),'15 a 17','outros'),
      turno = ifelse(TX_HR_INICIAL >= 5 & TX_HR_INICIAL<13, 'MATUTINO',
      ifelse(TX_HR_INICIAL >= 13 & TX_HR_INICIAL<17, 'VESPERTINO',
      ifelse(TX_HR_INICIAL >= 17 ,'NOTURNO','NSA')))) %>% 
  group_by(raca_cor,turno) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(raca_cor) %>% 
  mutate(p = n*100/sum(n))
```

INDICADOR 04 distorção idade serie ensino medio por cor  
bate com o site
```
co %>% 
  filter(TP_COR_RACA %in% c(1,2,3)) %>% 
  filter(TP_ETAPA_ENSINO %in% c(25:38)) %>%
  filter(CO_UF %in% c(52)) %>% 
  filter(TP_DEPENDENCIA %in% c(1:3)) %>% 
mutate(raca_cor = ifelse(TP_COR_RACA %in% c(2,3),'NEGROS','BRANCOS')) %>% 
mutate(tis1 = ifelse(TP_ETAPA_ENSINO %in% c(25,30,35) & NU_IDADE_REFERENCIA>= 17,1,0),
tis2 = ifelse(TP_ETAPA_ENSINO %in% c(26,31,36) & NU_IDADE_REFERENCIA>= 18,1,0),
tis3 = ifelse(TP_ETAPA_ENSINO %in% c(27,32,37) & NU_IDADE_REFERENCIA>= 19,1,0),
tis4 = ifelse(TP_ETAPA_ENSINO %in% c(28,33,38) & NU_IDADE_REFERENCIA>= 20,1,0),
tis = tis1+tis2+tis3+tis4) %>%
  group_by(raca_cor,tis) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(raca_cor) %>% 
  mutate(p = n*100/sum(n)) %>% 
  filter(tis == 1)
```

Indicador por turno
Os percentuais estão muito proximos com o do site
```
co %>% 
  filter(TP_COR_RACA %in% c(1,2,3)) %>% 
  filter(TP_ETAPA_ENSINO %in% c(25:38)) %>%
  filter(CO_UF %in% c(52)) %>% 
  filter(TP_DEPENDENCIA %in% c(1:3)) %>% 
  mutate(raca_cor = ifelse(TP_COR_RACA %in% c(2,3),'NEGROS','BRANCOS')) %>% 
  mutate(tis1 = ifelse(TP_ETAPA_ENSINO %in% c(25,30,35) & NU_IDADE_REFERENCIA>= 17,1,0),
         tis2 = ifelse(TP_ETAPA_ENSINO %in% c(26,31,36) & NU_IDADE_REFERENCIA>= 18,1,0),
         tis3 = ifelse(TP_ETAPA_ENSINO %in% c(27,32,37) & NU_IDADE_REFERENCIA>= 19,1,0),
         tis4 = ifelse(TP_ETAPA_ENSINO %in% c(28,33,38) & NU_IDADE_REFERENCIA>= 20,1,0),
         tis = tis1+tis2+tis3+tis4,
         turno = ifelse(TX_HR_INICIAL >= 5 & TX_HR_INICIAL<13, 'MATUTINO',
                 ifelse(TX_HR_INICIAL >= 13 & TX_HR_INICIAL<17, 'VESPERTINO',
                 ifelse(TX_HR_INICIAL >= 17 ,'NOTURNO','NSA')))) %>%
  group_by(raca_cor,turno, tis) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(raca_cor) %>% 
  mutate(p = n*100/sum(n)) %>% 
  ungroup() %>% 
  group_by(turno, raca_cor) %>% 
  mutate(p = round(100*n/sum(n),1)) %>% 
  filter(tis ==1)
```
# DESIGUALDADE GENERO

Ospercentuais batem com o site, os totais não.  
Indicador 05 percentual de pessoas por etapa de ensino e sexo 15 a 17 anos
```
co %>% 
  filter(TP_ETAPA_ENSINO %in% c(25:38,16:21,41)) %>%
  filter(CO_UF %in% c(52)) %>% 
  filter(TP_DEPENDENCIA %in% c(1:3)) %>% 
  mutate(ensino = ifelse(TP_ETAPA_ENSINO %in% c(25:38),'MEDIO','FUNDA'),
  idades = ifelse(NU_IDADE_REFERENCIA %in% c(15,16,17),'15 a 17','outros')) %>%
  filter(idades == '15 a 17')%>%
  group_by(ensino, TP_SEXO) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(TP_SEXO) %>% 
  mutate(p = n/sum(n)) 
```

Escola publica ou privada por sexo  
Valores batem com o site, pequena distroção na rede privada
```
co %>% 
  filter(TP_ETAPA_ENSINO %in% c(25:38,16:21,41)) %>%
  filter(CO_UF %in% c(52)) %>% 
  filter(TP_DEPENDENCIA %in% c(1:4)) %>% 
  mutate(ensino = ifelse(TP_ETAPA_ENSINO %in% c(25:38),'MEDIO','FUNDA'),
  idades = ifelse(NU_IDADE_REFERENCIA %in% c(15,16,17),'15 a 17','outros'),
  pub.priv = ifelse(TP_DEPENDENCIA %in% c(1:3),'Pública','Privada')) %>%
  filter(idades == '15 a 17')%>%
  group_by(ensino, TP_SEXO, pub.priv) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(pub.priv,TP_SEXO) %>% 
  mutate(p = n/sum(n)) 
```

Dependencia administrativa  
Escola publica por ente federativo por sexo  
Valores batem com o site  
```
co %>% 
  filter(TP_ETAPA_ENSINO %in% c(25:38,16:21,41)) %>%
  filter(CO_UF %in% c(52)) %>% 
  filter(TP_DEPENDENCIA %in% c(1:3)) %>% 
  mutate(ensino = ifelse(TP_ETAPA_ENSINO %in% c(25:38),'MEDIO','FUNDA'),
         idades = ifelse(NU_IDADE_REFERENCIA %in% c(15,16,17),'15 a 17','outros'),
         pub = ifelse(TP_DEPENDENCIA %in% c(1),'Federal',
               ifelse(TP_DEPENDENCIA %in% c(2),'Estadual','Municipal'))) %>%
  filter(idades == '15 a 17')%>%
  group_by(ensino, TP_SEXO, pub) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(pub,TP_SEXO) %>% 
  mutate(p = n/sum(n))
```

Indicador 06 percentual de pessoas por etapa de ensino e sexo  15 a 17 anos e 18 a 29 anos  
Valores batem com o que ta no site, inclusive os totais
```
co %>% 
  filter(TP_ETAPA_ENSINO %in% c(25:38)) %>%
  filter(CO_UF %in% c(52)) %>% 
  filter(TP_DEPENDENCIA %in% c(1:3)) %>% 
  mutate(ensino = ifelse(TP_ETAPA_ENSINO %in% c(25:38),'MEDIO','FUNDA'),
         idades = ifelse(NU_IDADE_REFERENCIA %in% c(15,16,17),'15 a 17',
          ifelse(NU_IDADE_REFERENCIA %in% c(18:29),'18 a 29','Outros'))) %>%
  filter(idades == '15 a 17' | idades == '18 a 29')%>%
  group_by(ensino, TP_SEXO, idades) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(TP_SEXO) %>% 
  mutate(p = n/sum(n)) 
```

Indicador escola privada
```
co %>% 
  filter(TP_ETAPA_ENSINO %in% c(25:38)) %>%
  filter(CO_UF %in% c(52)) %>% 
  filter(TP_DEPENDENCIA %in% c(4)) %>% 
  mutate(ensino = ifelse(TP_ETAPA_ENSINO %in% c(25:38),'MEDIO','FUNDA'),
         idades = ifelse(NU_IDADE_REFERENCIA %in% c(15,16,17),'15 a 17',
                         ifelse(NU_IDADE_REFERENCIA %in% c(18:29),'18 a 29','Outros'))) %>%
  filter(idades == '15 a 17' | idades == '18 a 29')%>%
  group_by(ensino, TP_SEXO, idades) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(TP_SEXO) %>% 
  mutate(p = n/sum(n)) 
```

Indicador escola pública
```
co %>% 
  filter(TP_ETAPA_ENSINO %in% c(25:38)) %>%
  filter(CO_UF %in% c(52)) %>% 
  filter(TP_DEPENDENCIA %in% c(1:3)) %>% 
  mutate(ensino = ifelse(TP_ETAPA_ENSINO %in% c(25:38),'MEDIO','FUNDA'),
         idades = ifelse(NU_IDADE_REFERENCIA %in% c(15,16,17),'15 a 17',
                         ifelse(NU_IDADE_REFERENCIA %in% c(18:29),'18 a 29','Outros')),
          pub = ifelse(TP_DEPENDENCIA %in% c(1),'Federal',
                ifelse(TP_DEPENDENCIA %in% c(2),'Estadual','Municipal'))) %>%
  filter(idades == '15 a 17' | idades == '18 a 29')%>%
  group_by(ensino,pub, TP_SEXO, idades) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(TP_SEXO) %>% 
  mutate(p = n/sum(n)) 
```

indicador 07 idade correta por turno e sexo  
Não consegui montar a variavel turno integral
```
co %>% 
  filter(TP_ETAPA_ENSINO %in% c(25:38)) %>%
  filter(CO_UF %in% c(52)) %>% 
  filter(TP_DEPENDENCIA %in% c(1:3)) %>% 
  mutate(ensino = ifelse(TP_ETAPA_ENSINO %in% c(25:38),'MEDIO','FUNDA'),
         idades = ifelse(NU_IDADE_REFERENCIA %in% c(15,16,17),'15 a 17',
                  ifelse(NU_IDADE_REFERENCIA %in% c(18:29),'18 a 29','Outros')),
         turno = ifelse(TX_HR_INICIAL >= 5 & TX_HR_INICIAL<13, 'MATUTINO',
                 ifelse(TX_HR_INICIAL >= 5 & TX_HR_INICIAL<17, 'INTEGRAL',
                 ifelse(TX_HR_INICIAL >= 17 ,'NOTURNO','NSA')))) %>%
  filter(idades == '15 a 17')%>%
  group_by(ensino, TP_SEXO, turno) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(TP_SEXO) %>% 
  mutate(p = n/sum(n)) 
```

# Educação Especial
## Dar atenção depois, vai ficar para uma segunda etapa  
```
co %>% 
  filter(IN_ESPECIAL_EXCLUSIVA == 1 | (IN_NECESSIDADE_ESPECIAL == 1&
          IN_ESPECIAL_EXCLUSIVA == 0)) %>%
  #filter(IN_ESPECIAL_EXCLUSIVA == 1) %>%
  #filter(IN_NECESSIDADE_ESPECIAL == 1 & IN_ESPECIAL_EXCLUSIVA == 0) %>%
  filter(CO_UF %in% c(52)) %>% 
  filter(TP_DEPENDENCIA %in% c(1:3)) %>% 
  mutate(def = case_when(
    IN_BAIXA_VISAO == 1 ~ 'Def. Visual',
    IN_CEGUEIRA == 1 ~ 'Def. Visual',
    IN_DEF_AUDITIVA == 1 ~ 'Def. Auditiva',
    IN_DEF_FISICA == 1 ~ 'Def. Física',
    IN_DEF_INTELECTUAL == 1 ~ 'Def. Intelectual',
    IN_SURDEZ == 1 ~ 'Def. Auditiva',
    IN_SURDOCEGUEIRA == 1 ~ 'Surdocego', 
    IN_DEF_MULTIPLA == 1 ~ 'Def. Multipla',
    IN_AUTISMO == 1 ~ 'Transtorno Global do Desenvolvimento',
    #IN_SINDROME_ASPERGER== 1 ~ 'Superdotação',
    #IN_SINDROME_RETT== 1 ~ 'Superdotação',
    #IN_TRANSTORNO_DI== 1 ~ 'Superdotação',
    IN_SUPERDOTACAO== 1 ~ 'Superdotação',
  )) %>% 
  group_by(def) %>% 
  count() %>% 
  ungroup() %>% 
  #group_by(def) %>% 
  mutate(p = 100*n/sum(n)) 
```
