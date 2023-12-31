library('reshape2')
library('ggplot2')

# L� o texto e converte todas as letras para mai�sculas
reference=readLines("C:/Users/Pedro/Desktop/FGV/GRADUA��O/6� PER�ODO/PROCESSOS ESTOC�STICOS/TRABALHO EM GRUPO/CRIPTOGRAFIA/sob_a_redoma.txt")
reference=toupper(reference)

# Inicializa uma matriz de transi��o entre letras do alfabeto.
# Percorre o texto de refer�ncia e conta a frequ�ncia de transi��o de uma letra para outra. 
# Essa informa��o ser� usada para calcular as probabilidades de transi��o.
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

# Calcula as probabilidades de transi��o a partir da matriz de contagem.
trans.prob.mat=sweep(trans.mat+1,1,rowSums(trans.mat+1),FUN="/")

# Cria um heatmap que representa visualmente as probabilidades de transi��o entre letras do alfabeto.
ggplot(melt(trans.prob.mat),aes(Var2,Var1))+geom_tile(aes(fill=value))+
  scale_fill_gradient(low="skyblue",high="darkblue",limits=c(0,1))+
  labs(x="Letra seguinte",y="Letra atual",fill="Prob")+
  scale_y_discrete(limits = rev(levels(melt(trans.prob.mat)$Var1)))+
  coord_equal()

# Define uma fun��o para decodificar um texto usando um mapeamento espec�fico de letras.
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

# Calcula a log probabilidade de um texto decodificado com base nas probabilidades de transi��o.
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
correctTxt="A dois mil p�s, onde Claudette Sanders recebia uma aula de voo, a cidade de Chester's Mill
cintilava � luz da manh� como algo que acabou de ficar pronto e de ser ali pousado. Os carros rodavam
pela rua principal, relampejando piscadelas de sol. A torre da Igreja Congregacional parecia t�o aguda
que poderia furar o c�u imaculado. O sol correu pela superf�cie do riacho Prestile quando o Seneca V o
sobrevoou, avi�o e �gua cortando a cidade na mesma rota diagonal"
coded=decode(sample(toupper(letters)),correctTxt)

# Inicializa um mapeamento aleat�rio e itera sobre tentativas de permuta��o.
# Usa o algoritmo Metropolis-Hastings para aceitar ou rejeitar permuta��es com base em probabilidades.
# O objetivo � encontrar o mapeamento que maximiza a log probabilidade do texto decodificado.
mapping=sample(toupper(letters))
i=1
iters=500
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
