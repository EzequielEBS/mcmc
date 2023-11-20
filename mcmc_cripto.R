library('reshape2')
library('ggplot2')
library('ggplot')

reference=readLines("C:/Users/Pedro/Desktop/FGV/GRADUAÇÃO/6º PERÍODO/PROCESSOS ESTOCÁSTICOS/TRABALHO EM GRUPO/CRIPTOGRAFIA/biblia.txt")
reference=toupper(reference)

trans.mat=matrix(0,27,27)
rownames(trans.mat)=colnames(trans.mat)=c(toupper(letters),"")
lastletter=""
for (ln in 1:length(reference)) {
  if (ln %% 1000 ==0) {cat("Line",ln,"\n")}
  for (pos in 1:nchar(reference[ln])) {
    curletter=substring(reference[ln],pos,pos)
    if (curletter %in% toupper(letters)) {
      trans.mat[rownames(trans.mat)==lastletter,
                colnames(trans.mat)==curletter]=
        trans.mat[rownames(trans.mat)==lastletter,
                  colnames(trans.mat)==curletter]+1
      lastletter=curletter
    } else {
      if (lastletter!="") {
        trans.mat[rownames(trans.mat)==lastletter,27]=
          trans.mat[rownames(trans.mat)==lastletter,27]+1
        lastletter=""
      }
    }
  }
  curletter=""
  if (lastletter!="") {
    trans.mat[rownames(trans.mat)==lastletter,27]=
      trans.mat[rownames(trans.mat)==lastletter,27]+1
  }
  lastletter=""
}

trans.prob.mat=sweep(trans.mat+1,1,rowSums(trans.mat+1),FUN="/")

ggplot(melt(trans.prob.mat),aes(Var2,Var1))+geom_tile(aes(fill=value))+
  scale_fill_gradient(low="white",high="black",limits=c(0,1))+
  labs(x="Letra seguinte",y="Letra atual",fill="Prob")+
  scale_y_discrete(limits = rev(levels(melt(trans.prob.mat)$Var1)))+
  coord_equal()

decode <- function(mapping,coded) {
  coded=toupper(coded)
  decoded=coded
  for (i in 1:nchar(coded)) {
    if (substring(coded,i,i) %in% toupper(letters)) {
      substring(decoded,i,i)=toupper(letters[mapping==substring(coded,i,i)])
    }
  }
  decoded
}


log.prob <- function(mapping,decoded) {
  logprob=0
  
  lastletter=""
  for (i in 1:nchar(decoded)) {
    curletter=substring(decoded,i,i)
    if (curletter %in% toupper(letters)) {
      logprob=logprob+log(trans.prob.mat[rownames(trans.mat)==lastletter,
                                         colnames(trans.mat)==curletter])
      lastletter=curletter
    } else {
      if (lastletter!="") {
        logprob=logprob+log(trans.prob.mat[rownames(trans.mat)==lastletter,27])
        lastletter=""
      }
    }
  }
  
  if (lastletter!="") {
    logprob=logprob+log(trans.prob.mat[rownames(trans.mat)==lastletter,27])
    lastletter=""
  }
  logprob
}

#correctTxt="ENTER HAMLET HAM TO BE OR NOT TO BE THAT IS THE QUESTION WHETHER TIS NOBLER IN THE MIND TO SUFFER THE SLINGS AND ARROWS OF OUTRAGEOUS FORTUNE OR TO TAKE ARMS AGAINST A SEA OF TROUBLES AND BY OPPOSING END"
#correctTxt="UM ENORME SORRISO LHE ORNAVA O FOCINHO AS ORELHAS ESTAVAM PARA TRAS GRUDADAS NO CRANIO A SOMBRA CORRIA AO SEU LADO NO CAPIM MANCHADO DE FULIGEM JULIA SE AJOELHOU E ESTENDEU OS BRACOS"
#correctTxt="EU GOSTO MUITO DO RODRIGO POIS ELE IMITA GATOS E SABE FAZER UM QUEIJO QUENTE MUITO GOSTOSO FAZ SENTIDO ELE ENTENDER TANTO SOBRE QUEIJOS AFINAL DE CONTAS ELE E MINEIRO"
#correctTxt="Quella circulazion che sì concetta pareva in te come lume reflesso, da li occhi miei alquanto circunspetta, dentro da sé, del suo colore stesso, mi parve pinta de la nostra effige"
#correctTxt="MAS TALVEZ FOSSE UM ABISMO EM QUE HOMENS MELHORES DO QUE ELE TIVESSEM CAIDO SE ELA TIVESSE RECONSIDERADO TUDO NA SUA VIDA DAI PARA A FRENTE TERIA MUDADO PORQUE ELA DEVE TER CONSEGUIDO SAIR NUNCA MAIS ELE VIU A LOURA DE ROSTO VICOSO NEM O FORD VELHO E SUJO"
#correctTxt = "Ainda cheguei a tempo para incluir no inventário da língua nacional milhares de verbas, que àmanhan estariam sonegadas por essa insaciável cabeça de casal, que se chama civilização, o que, desculpando-se com os seus intuitos nobilíssimos, procura locupletar-se á custa de tudo e apesar de tudo."
correctTxt = "Mi iris al la fundamento hodiau kaj alvenis tute malseka car multe pluvis"
coded=decode(sample(toupper(letters)),correctTxt) # randomly scramble the text

mapping=sample(toupper(letters)) # initialize a random mapping
i=1
iters=1500
cur.decode=decode(mapping,coded)
cur.loglike=log.prob(mapping,cur.decode)
max.loglike=cur.loglike
max.decode=cur.decode
while (i<=iters) {
  proposal=sample(1:26,2) # select 2 letters to switch
  prop.mapping=mapping
  prop.mapping[proposal[1]]=mapping[proposal[2]]
  prop.mapping[proposal[2]]=mapping[proposal[1]]
  
  prop.decode=decode(prop.mapping,coded)
  prop.loglike=log.prob(prop.mapping,prop.decode)
  
  if (runif(1)<exp(prop.loglike-cur.loglike)) {
    mapping=prop.mapping
    cur.decode=prop.decode
    cur.loglike=prop.loglike
    
    if (cur.loglike>max.loglike) {
      max.loglike=cur.loglike
      max.decode=cur.decode
    }
    
    cat(i,cur.decode,"\n")
    i=i+1
  }
}
