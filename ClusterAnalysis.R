ClusterAnalysis <- function(data, max_num_cluster){
  
  ## Definir o total de número de cluster a ser procurado
  #K = 20
  K = max_num_cluster
  cost = vector('numeric', length = (K-1) )
  
  for(i in 2:K){
    
    cluster = kmeans(data,centers=i,nstart=100)
    cost[i-1] = cluster$tot.withinss
    
    
  }
  
  result = data.frame(clusters = 2:K, Custo = cost)
  
  plot(Custo~clusters, data = result, type ='o')
  
  return(result)
  
}