load("~/mestrado/Analise de Dados/knn.RData")
data = iris

dados = iris[,-5]
rotulos = iris[,5]

d = dados[c(1:10,12:150),]
n = dados[11,]

knn2(d,rotulos, n,3)

iris[11,]



knn2 = function(dados, rotulo, novo, k){
  dist = c()
  for (i in 1:nrow(dados)){
    dist = c(dist,distE(novo,dados[i,]))
  }
  print(sort.list(dist)[1:k])
  rotulo[sort.list(dist)[1:k]]
}

w_knn = function(dados, rotulo, novo, k){
  dist = c()
  for (i in 1:nrow(dados)){
    dist = c(dist,distE(novo,dados[i,]))
  }
  
  ks = sort.list(dist)[1:k]
  unq_rotulos = unique(rotulo[ks])
  
  k_grupo = list()
  k_grupo$rotulo = c()
  k_grupo$pesoRotulo = c()
  
  #names(k_grupo) = unq_rotulos
  i = 1
  for(k in ks){
    k_rotulo = as.character(rotulo[k])
    
    dist = distE(novo, dados[k,])
    
    pesoRotulo = if(dist == 0) 1 else 1/(dist)
    k_grupo$rotulo[i] = k_rotulo
    
    k_grupo$pesoRotulo[i] = sum(k_grupo[k_grupo$rotulo == k_rotulo]$pesoRotulo) + pesoRotulo
    i = i +1;
  }
  k_grupo$rotulo[which(rank$pesoRotulo == max(rank$pesoRotulo))]

}
