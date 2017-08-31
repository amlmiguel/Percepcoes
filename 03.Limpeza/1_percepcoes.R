### Subsistency Project
### Data Correction - Family Members File

### Reference to Source File
### source("1_Family_Members.R",print.eval=TRUE)

###  Definindo diretório
setwd("/home/miguel/Projetos/Percepcoes/03.Limpeza")

### 0. LOADING PACKAGED OR EXTERNAL CODES -----------------------------------
rm(list = ls())
library(dplyr)
library(tidyr)
source('../05.AuxiliaryFunctions/aux-functions.R')

### 1. DEFINING PATHS  ------------------------------------------------------
path_main <- get_path()
path_data <- file.path(path_main, "02.Dados")
path_tidy <- file.path(path_main,  "04.Dadosprocessados")

### 2. READING COMPLETE DATASET ---------------------------------------------
percepcoes_orig <- read.csv(file.path(path_data, "00_percepcoes.csv"),
                    stringsAsFactors = FALSE)
names(percepcoes_orig) <- tolower(names(percepcoes_orig))
### 3. CREATING NEW DATAFRAME FOR PEOPLE ------------------------------------

###Colocando todos os caracteres em caixa baixa
percepcoes_orig <- mutate_all(percepcoes_orig, funs(tolower))

###convertendo números em numeric
percepcoes_orig$tempo.que.vive.nessa.comunidade <- as.numeric(percepcoes_orig$tempo.que.vive.nessa.comunidade)
percepcoes_orig$quanto.tempo.é.casado.ou.mora.junto..anos.<- as.numeric(percepcoes_orig$quanto.tempo.é.casado.ou.mora.junto..anos.)
percepcoes_orig$quanto.tempo.é.casado.ou.mora.junto..anos..1<- as.numeric(percepcoes_orig$quanto.tempo.é.casado.ou.mora.junto..anos..1)
percepcoes_orig$educação..esposa.<-as.numeric(percepcoes_orig$educação..esposa.)
percepcoes_orig$educação..esposa..codif<- as.numeric(percepcoes_orig$educação..esposa..codif)
percepcoes_orig$idade..marido.<- as.numeric(percepcoes_orig$idade..marido.)
percepcoes_orig$educação..marido.<-as.numeric(percepcoes_orig$educação..marido.)
percepcoes_orig$valor.do.aposentadoria<-as.numeric(percepcoes_orig$valor.do.aposentadoria)
percepcoes_orig$desde.que.ano.recebe..aposentadoria. <- as.numeric(percepcoes_orig$desde.que.ano.recebe..aposentadoria.)
percepcoes_orig$desde.que.ano.recebe.bolsa.familia <- as.numeric(percepcoes_orig$desde.que.ano.recebe.bolsa.familia)
percepcoes_orig$renda.de.açai..por.mes<- as.numeric(percepcoes_orig$renda.de.açai..por.mes)
percepcoes_orig$distancia.da.tua.casa.para.o.mercado..horas.de.barco. <- as.numeric(percepcoes_orig$distancia.da.tua.casa.para.o.mercado..horas.de.barco.)
percepcoes_orig$número.de.famílias.morando.na.comunidade <- as.numeric(percepcoes_orig$número.de.famílias.morando.na.comunidade)
percepcoes_orig$tamanho.da.propriedade..m.<-as.numeric(percepcoes_orig$tamanho.da.propriedade..m.)
percepcoes_orig$quantos...do.seu.terreno.é.terra.firme.que.nunca.inunda..m....<-as.numeric(percepcoes_orig$quantos...do.seu.terreno.é.terra.firme.que.nunca.inunda..m....)




### Criando um subconjunto apenas com variáveis nao numéricas
nn.percepcoes<-percepcoes_orig[sapply(percepcoes_orig,is.character)]

fun<-function(n){levels(as.factor(n))}
fun(names(nn.percepcoes))
levels<-sapply(nn.percepcoes, fun)


###Corrigindo levels religião..esposa
percepcoes_orig$religião..esposa.[which(percepcoes_orig$religião..esposa. == "Oeiras")] <- ""

###Corrigindo local.que.nasceu..esposa
percepcoes_orig$local.que.nasceu..esposa.[grep("abaete|abaeté", 
                                percepcoes_orig$local.que.nasceu..esposa., ignore.case =  T)] <- "abaetetuba"
#percepcoes_orig$local.que.nasceu..esposa.[grep("igarapé Miri", 
#                                percepcoes_orig$local.que.nasceu..esposa., ignore.case =  T)] <- "Igarapé Mirim"
percepcoes_orig$local.que.nasceu..esposa.[grep(" urubueua", 
                                percepcoes_orig$local.que.nasceu..esposa., ignore.case =  T)] <- "urubueua"


### Corrigindo local.que.nasceu..marido.
percepcoes_orig$local.que.nasceu..marido.[which(percepcoes_orig$local.que.nasceu..marido. == "abaete")] <- "abaetetuba"
percepcoes_orig$local.que.nasceu..marido.[which(percepcoes_orig$local.que.nasceu..marido. == "belem")] <- "belém"
percepcoes_orig$local.que.nasceu..marido.[which(percepcoes_orig$local.que.nasceu..marido. == "río inembú")] <- "rio inembú"

###Corrigindo como.você.se.identifica
percepcoes_orig$como.você.se.identifica[grep("pescado|pescador ", 
                                             percepcoes_orig$como.você.se.identifica, ignore.case = TRUE )] <- "pescador"
percepcoes_orig$como.você.se.identifica[grep(" ribeirinho|ribeirinho ", 
                                             percepcoes_orig$como.você.se.identifica, ignore.case = TRUE )] <- "ribeirinho"

###Corrigindo como.você.se.identifica.corrigido
percepcoes_orig$como.você.se.identifica.corrigido[grep("pescado|pescador ", 
                                             percepcoes_orig$como.você.se.identifica.corrigido, ignore.case = TRUE )] <- "pescador"
percepcoes_orig$como.você.se.identifica.corrigido[grep(" ribeirinho|ribeirinho ", 
                                             percepcoes_orig$como.você.se.identifica.corrigido, ignore.case = TRUE )] <- "ribeirinho"



###Corrigindo filiado.colonia..marido
percepcoes_orig$filiado.colonia..marido.[which(percepcoes_orig$filiado.colonia..marido. == "não ")] <- "não"

###Corrigindo filiado.colonia..marido
percepcoes_orig$participa.de.alguma.cooperativa..qual.[which(percepcoes_orig$participa.de.alguma.cooperativa..qual. == "não ")] <- "não"

###Corrigindo frequência.que.recebe.o.salario
percepcoes_orig$frequência.que.recebe.o.salario[which(percepcoes_orig$frequência.que.recebe.o.salario == "2011")] <- ""

###Corrigindo frequência.que.recebe.seguro.defeso
percepcoes_orig$frequência.que.recebe.seguro.defeso[grep("4|4 mese|4 meses ",
                                                         percepcoes_orig$frequência.que.recebe.seguro.defeso, 
                                                         ignore.case =  T)] <- "4 meses"
###Corrigindo frequência.que.recebe.seguro.defeso
percepcoes_orig$frequência.que.recebe.bolsa.família[grep("mês",
                                                         percepcoes_orig$frequência.que.recebe.bolsa.família, 
                                                         ignore.case =  T)] <- "mensal"

###Corrigindo frequência.que.recebe.beneficio.saúde
percepcoes_orig$frequência.que.recebe.beneficio.saúde[grep("mês",
                                                         percepcoes_orig$frequência.que.recebe.beneficio.saúde, 
                                                         ignore.case =  T)] <- "mensal"
###Corrigindo frequência.que.recebe.beneficio.saúde
percepcoes_orig$frequência.que.recebe.pronaf[grep("m?s",
                                                           percepcoes_orig$frequência.que.recebe.pronaf, 
                                                           ignore.case =  T)] <- "mensal"
###Corrigindo onde.pesca.o.peixe
percepcoes_orig$onde.pesca.o.peixe[which(percepcoes_orig$onde.pesca.o.peixe == "furo do palheta")] <- "furo da palheta"
percepcoes_orig$onde.pesca.o.peixe[which(percepcoes_orig$onde.pesca.o.peixe == "no rio")] <- "rio"
percepcoes_orig$onde.pesca.o.peixe[which(percepcoes_orig$onde.pesca.o.peixe == " rio tauari")] <- "rio tauari"


###Corrigindo em.que.ambiente
percepcoes_orig$em.que.ambiente[grep(" rio tauari|tauari|tauary",percepcoes_orig$em.que.ambiente, ignore.case = T)] <-"rio tauari"

###Corrigindo onde.pesca.o.camarao
percepcoes_orig$onde.pesca.o.camarão.[grep(" rio tauari|igarapé tauari",percepcoes_orig$onde.pesca.o.camarão., ignore.case = T)] <-"rio tauari"
percepcoes_orig$onde.pesca.o.camarão.[grep("rio, praia",percepcoes_orig$onde.pesca.o.camarão., ignore.case = T)] <-"rio e praia"

###Corrigindo pesca.com.matapi
percepcoes_orig$pesca.com.matapi.[which(percepcoes_orig$pesca.com.matapi. == "sim ")] <- "sim"

###Corrigindo em.que.ambiente.pesca.
percepcoes_orig$em.que.ambiente.pesca.[grep(" rio tauari|tauari|tauary",percepcoes_orig$em.que.ambiente.pesca., ignore.case = T)] <-"rio tauari"

###Corrigindo extração.de.palmito.consume.ou.venda
percepcoes_orig$extração.de.palmito.consume.ou.venda[which(
  percepcoes_orig$extração.de.palmito.consume.ou.venda == "consome e venda")] <- "consome e vende"

###Corrigindo outros.tipos.de.comercio
percepcoes_orig$outros.tipos.de.comercio[which(percepcoes_orig$outros.tipos.de.comercio == "empresxa")] <- "empresa"

###Corrigindo seu.terreno.é.inundado.ou.terra.firme
percepcoes_orig$seu.terreno.é.inundado.ou.terra.firme[which(percepcoes_orig$seu.terreno.é.inundado.ou.terra.firme ==
                                                              "inundado ")] <- "inundado"

###Corrigindo que.medida.tomas.para.reduzir.o.impacto.da.alta.temperatura.na.pesca.de.peixe
percepcoes_orig$que.medida.tomas.para.reduzir.o.impacto.da.alta.temperatura.na.pesca.de.peixe.[grep("evita desmatamento|evitar desmatamento menos|evitar desmatamento menos a natureza|evitar evitar desmatamento| evitar evitar desmatamento a mata| evitar o evitar evitar desmatamento|não desmatar|não desmatar na beira do rio|não evitar desmatamento|o evitar evitar desmatamento|por causa do evitar evitar desmatamento|reduzir desmatamento|reduzir o evitar evitar desmatamento|reflorestar,evitar desmatamento", percepcoes_orig$que.medida.tomas.para.reduzir.o.impacto.da.alta.temperatura.na.pesca.de.peixe., ignore.case = T)] <- "evitar desmatamento"

percepcoes_orig$que.medida.tomas.para.reduzir.o.impacto.da.alta.temperatura.na.pesca.de.peixe.[grep("menos queimada|menos queimadas", percepcoes_orig$que.medida.tomas.para.reduzir.o.impacto.da.alta.temperatura.na.pesca.de.peixe., ignore.case = T)] <- "evitar queimadas"

percepcoes_orig$que.medida.tomas.para.reduzir.o.impacto.da.alta.temperatura.na.pesca.de.peixe.[grep("não soube responder", percepcoes_orig$que.medida.tomas.para.reduzir.o.impacto.da.alta.temperatura.na.pesca.de.peixe., ignore.case = T)] <- "não sabe"

percepcoes_orig$que.medida.tomas.para.reduzir.o.impacto.da.alta.temperatura.na.pesca.de.peixe.[grep("refloresta|reflorestamento", percepcoes_orig$que.medida.tomas.para.reduzir.o.impacto.da.alta.temperatura.na.pesca.de.peixe., ignore.case = T)] <- "reflorestar"

percepcoes_orig$que.medida.tomas.para.reduzir.o.impacto.da.alta.temperatura.na.pesca.de.peixe.[grep("o entrevistado quer criar peixe em cativeriro", percepcoes_orig$que.medida.tomas.para.reduzir.o.impacto.da.alta.temperatura.na.pesca.de.peixe., ignore.case = T)] <- "criação em cativeiro"

percepcoes_orig$que.medida.tomas.para.reduzir.o.impacto.da.alta.temperatura.na.pesca.de.peixe.[grep("proteger as matas e evitar efluentes lançados pelas empresas na vila do conde", percepcoes_orig$que.medida.tomas.para.reduzir.o.impacto.da.alta.temperatura.na.pesca.de.peixe., ignore.case = T)] <- "proteger as matas"


###Corrigindo levels comunidade
percepcoes_orig$comunidade[which(percepcoes_orig$comunidade == "arumanduba")] <- "arumanduba"
percepcoes_orig$comunidade[which(percepcoes_orig$comunidade == "Vila Palheta")] <- "palheta"
percepcoes_orig$comunidade[which(percepcoes_orig$comunidade == "Jupatituba")] <- "ilha jupatituba"
percepcoes_orig$comunidade[grep("ilha ponta|ilha ponta-negra|ponta negra", 
                                percepcoes_orig$comunidade, ignore.case =  T)] <- "ilha ponta negra"
percepcoes_orig$comunidade[grep("periquitão|ilha piriquitão", 
                                percepcoes_orig$comunidade, ignore.case =  T)] <- "ilha periquitão"



fun(percepcoes_orig$o.chefe.de.família.é.homem.ou.mulher)
fun(percepcoes_orig$religião..esposa.)
fun(percepcoes_orig$local.que.nasceu..esposa.)

