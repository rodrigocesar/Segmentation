## Ler os dados dos indicadores

## Carregar as funções desenvolvidas
source('ClusterAnalysis.R')


classes = c('character','numeric','numeric','numeric','numeric','numeric',
            'numeric', 'numeric')
rawdata = read.csv2(file = 'data.csv', header=TRUE, row.names = 1,
                    colClasses = classes)
summary(rawdata)
str(rawdata)

## Excluir cidades que nao possuem dados completos 
index = complete.cases(rawdata)
complete_data = rawdata[index,]
str(complete_data)

## Excluir cidades com mais de 200.000 habitantes
index = which( (complete_data$Populacao < 200000) & (complete_data$Populacao > 50000) )
data = complete_data[index,]

nrow(rawdata)
nrow(complete_data)
nrow(data)


## Normalizar dados. Media = 0 e desvio padrão = 1.
procdata = scale(data, center=TRUE)
### Verificar Normalização
colMeans(procdata)
apply(procdata, 2, sd)



## Iniciar clusterização
## Gerar grafico custo x clusters
cost = ClusterAnalysis(data = procdata, max_num_cluster = 50)


## Create Clusters
cluster = kmeans(procdata,centers=8,nstart=100)

## Escrever um arquivo com os clusters
write.csv2(sort(cluster$cluster), file = 'clusters.csv')
index = which(cluster$cluster == 7)
xtable( as.table(index))
index = which(cluster$cluster == 1)
xtable( as.table(index))

## Escrever um arquivo com os centros dos clusters
write.csv2(cluster$centers, file = 'centers.csv')
xtable(cluster$centers, digits=2)



### Análise de componentes principais
pc.cr =princomp(procdata, cor = TRUE)
summary(pc.cr)
loadings(pc.cr)
plot(pc.cr)
biplot(pc.cr, cex = 0.3)
coord = pc.cr$scores
plot(coord)

## Criar tabelas
xtable(pc.cr$loadings[,1:3], digits=3)

## Filtra os outliers
index = which(coord[,1] > 0 )

## Plotar pontos filtrados
biplot(coord[index,])
## Escrever um arquivo com os centros dos clusters
write.csv2(coord, file = 'princomp.csv')
