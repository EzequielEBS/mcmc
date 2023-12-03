library('reshape2')
library('ggplot2')

# Lê o texto e converte todas as letras para maiúsculas
reference=readLines("C:/Users/Pedro/Desktop/FGV/GRADUAÇÃO/6º PERÍODO/PROCESSOS ESTOCÁSTICOS/TRABALHO EM GRUPO/CRIPTOGRAFIA/la_divina_comedia.txt")
reference=toupper(reference)

# Inicializa uma matriz de transição entre letras do alfabeto.
# Percorre o texto de referência e conta a frequência de transição de uma letra para outra. 
# Essa informação será usada para calcular as probabilidades de transição.
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

# Calcula as probabilidades de transição a partir da matriz de contagem.
trans.prob.mat=sweep(trans.mat+1,1,rowSums(trans.mat+1),FUN="/")

# Cria um heatmap que representa visualmente as probabilidades de transição entre letras do alfabeto.
ggplot(melt(trans.prob.mat),aes(Var2,Var1))+geom_tile(aes(fill=value))+
  scale_fill_gradient(low="skyblue",high="darkblue",limits=c(0,1))+
  labs(x="Letra seguinte",y="Letra atual",fill="Prob")+
  scale_y_discrete(limits = rev(levels(melt(trans.prob.mat)$Var1)))+
  coord_equal()

# Define uma função para decodificar um texto usando um mapeamento específico de letras.
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

# Calcula a log probabilidade de um texto decodificado com base nas probabilidades de transição.
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

# Define um texto original e o codifica aleatoriamente para criar um texto criptografado.

#correctTxt="ENTER HAMLET HAM TO BE OR NOT TO BE THAT IS THE QUESTION WHETHER TIS NOBLER IN THE MIND TO SUFFER THE SLINGS AND ARROWS OF OUTRAGEOUS FORTUNE OR TO TAKE ARMS AGAINST A SEA OF TROUBLES AND BY OPPOSING END"
#correctTxt="UM ENORME SORRISO LHE ORNAVA O FOCINHO AS ORELHAS ESTAVAM PARA TRAS GRUDADAS NO CRANIO A SOMBRA CORRIA AO SEU LADO NO CAPIM MANCHADO DE FULIGEM JULIA SE AJOELHOU E ESTENDEU OS BRACOS"
correctTxt="Quella circulazion che sì concetta pareva in te come lume reflesso, da li occhi miei alquanto circunspetta, dentro da sé, del suo colore stesso, mi parve pinta de la nostra effige"
#correctTxt="MAS TALVEZ FOSSE UM ABISMO EM QUE HOMENS MELHORES DO QUE ELE TIVESSEM CAIDO SE ELA TIVESSE RECONSIDERADO TUDO NA SUA VIDA DAI PARA A FRENTE TERIA MUDADO PORQUE ELA DEVE TER CONSEGUIDO SAIR NUNCA MAIS ELE VIU A LOURA DE ROSTO VICOSO NEM O FORD VELHO E SUJO"
#correctTxt = "Ainda cheguei a tempo para incluir no inventário da língua nacional milhares de verbas, que àmanhan estariam sonegadas por essa insaciável cabeça de casal, que se chama civilização, o que, desculpando-se com os seus intuitos nobilíssimos, procura locupletar-se á custa de tudo e apesar de tudo."
#correctTxt = "Mi iris al la fundamento hodiaŭ kaj alvenis tute malseka ĉar multe pluvis"
coded=decode(sample(toupper(letters)),correctTxt)

# Inicializa um mapeamento aleatório e itera sobre tentativas de permutação.
# Usa o algoritmo Metropolis-Hastings para aceitar ou rejeitar permutações com base em probabilidades.
# O objetivo é encontrar o mapeamento que maximiza a log probabilidade do texto decodificado.
mapping=sample(toupper(letters))
i=1
iters=1500
cur.decode=decode(mapping,coded)
cur.loglike=log.prob(mapping,cur.decode)
max.loglike=cur.loglike
max.decode=cur.decode
while (i<=iters) {
  proposal=sample(1:26,2)
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