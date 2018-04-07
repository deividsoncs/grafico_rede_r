
library(igraph)
require(graphics)
############################################################################################################
#Tratament e Cálculo de distância para os dados brutos
############################################################################################################

mycosine <- function(x,y){
  c <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(c)
}

cosinesim <- function(x) {
  # initialize similarity matrix
  m <- matrix(NA, nrow=ncol(x),ncol=ncol(x),dimnames=list(colnames(x),colnames(x)))
  cos <- as.data.frame(m)
  
  for(i in 1:ncol(x)) {
    for(j in i:ncol(x)) {
      co_rate_1 <- x[which(x[,i] & x[,j]),i]
      co_rate_2 <- x[which(x[,i] & x[,j]),j]  
      cos[i,j]= mycosine(co_rate_1,co_rate_2)
      cos[j,i]=cos[i,j]        
    }
  }
  return(cos)
}

#https://github.com/computationalstylistics/stylo/blob/master/R/dist.cosine.R
dist.cosine = function(x){
  # test if the input dataset is acceptable
  if(is.matrix(x) == FALSE & is.data.frame(x) == FALSE) {
    stop("cannot apply a distance measure: wrong data format!")
  }
  # then, test whether the number of rows and cols is >1
  if(length(x[1,]) < 2 | length(x[,1]) < 2) {
    stop("at least 2 cols and 2 rows are needed to compute a distance!")
  }
  
  # to get Centered Cosine dist (=Pearson Correlation Coeff.), one needs 
  # to normalize the feature vectors by subtracting the vector means
  # x = t( t(x) - colMeans(x) )
  
  # this computes cosine dissimilarity; to have similarity, 1- applies
  #y = 1 - as.dist( x %*% t(x) / (sqrt(rowSums(x^2) %*% t(rowSums(x^2)))) )
  #estou aplicando  a não similaridade
  y = as.dist( x %*% t(x) / (sqrt(rowSums(x^2) %*% t(rowSums(x^2)))) ) 
  # alternative way of approaching it:
  # crossprod(x, y) / sqrt(crossprod(x) * crossprod(y))
  
  return(y)
}

df = read.csv("/home/calixto/Área de Trabalho/Dropbox/Mestrado/Elias/R/Grafico de rede/raw_data.csv",
              row.names = 1, check.names = FALSE)
#https://stat.ethz.ch/R-manual/R-devel/library/stats/html/dist.html argumentos
head(df)
#retira colunas iniciais desnecessárias
df <- df[,-c(1,2,3,4,5,6,7)]
head(df)
# substitui os valores NA por Zeros 
df[is.na(df)] <- 0 
head(df)
View(df)
#numero de rows
#nR = nrow(df)
#numeor de cols
#nC = ncol(df)
#data frame para matriz
m = as.matrix(df)
head(m)

#formula distancia coseno 1
newDfCos <- cosinesim(m)
View(newDfCos)
x <- as.data.frame(newDfCos)
x[is.na(x)] <- 0
head(x)
View(x)
m <- as.matrix(x)

#distância euclidiana
m <- as.matrix(dist(m, method = "euclidean"))
head(m)

#formula distancia coseno 2
m1 <- dist.cosine(m)
mCos <- as.matrix(m1)
head(mCos)

#plotagem normal não admite double
net=graph.adjacency(m,mode="undirected",weighted=TRUE,diag=FALSE)
plot(net)
head(m)
#head(mCos)
#com peso das arestas
#net=graph.adjacency(mCos,mode="undirected",weighted=TRUE,diag=FALSE)
#summary(net)
#E(net)$weight
#plot.igraph(net,vertex.label=V(net)$name,layout=layout.spring, edge.color="black",edge.width=E(net)$weight)

############################################################################################################
#leitura do csv do capítulo 5 do livro Data Smart r-vizinhança
df = read.csv("/home/calixto/Área de Trabalho/Dropbox/Mestrado/Elias/R/Grafico de rede/input-r-vizinho.csv",
              row.names = 1, check.names = FALSE)
#leitura do csv do capítulo 5 do livro Data Smart k-nn
#df = read.csv("/home/calixto/Área de Trabalho/Dropbox/Mestrado/Elias/R/Grafico de rede/imput-knn.csv",
#              row.names = 1, check.names = FALSE)
m = as.matrix(df)
head(m)
m <- as.matrix(df)
View(m)
head(m)
net=graph.adjacency(m,mode="undirected",weighted=TRUE,diag=FALSE)
plot(net)
############################################################################################################

#############################################################################################################
#com peso das arestas
net=graph.adjacency(m,mode="undirected",weighted=TRUE,diag=FALSE)
View(net)
summary(net)
E(net)$weight
plot.igraph(net,vertex.label=V(net)$name,layout=layout.spring, edge.color="black",edge.width=E(net)$weight)
#############################################################################################################

