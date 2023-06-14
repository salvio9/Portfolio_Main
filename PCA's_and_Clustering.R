library(psych)
library(corrplot)
library(cluster)
library(tidyverse)
library(caret)
library(factoextra)
library(mclust)
library(magrittr)
library(haven)
library(mclust)


df <-read_sav("STU_QQQ_5.sav")


df_v2 <- df[df$CNT == "FRA",]


df_v3 <-subset(df_v2, select = c("SOIAICT","AUTICT","COMPICT","INTICT","USESCH", "HOMESCH","ENTUSE","ICTRES", "ICTSCH", "ICTHOME", "ICTOUTSIDE","ICTCLASS","WEALTH","ESCS","AGE","ST004D01T","HISCED"))
df_v3


summary(df_v3)

pairs(df_v3, pch = 19, lower.panel = NULL)


df_v4 <- na.omit(df_v3) 
df_v5 <- df_v4[,0:12]




correlation <- cor(df_v5)
par(oma = c(2, 2, 2, 2)) # space around for text
corrplot.mixed(correlation, 
               order = "hclust", #order of variables
               tl.pos = "lt", #text left + top
               upper = "ellipse")


round(correlation, 3)

cortest.bartlett(correlation)
KMO(correlation)


dataZ <- scale(df_v4[,0:12])


pcall <- principal(dataZ, nfactors=12, rotate="none", scores=TRUE) 
pcall


round(pcall$values,3)
#keizer sugere 4

plot(pcall$values, type= "b", main = "Scree plot", xlab = "Number of PC", ylab = "Eigenvalue")

#4 solutions

_____________________________________________

pcall$loadings


pc_5_unro <- principal(dataZ, nfactors=5, rotate="none", scores=TRUE) 
pc_5_unro


pc5r <- principal(dataZ, nfactors=5, rotate="varimax")
pc5r

pc5r$loadings

round(pc5r$communality,2)



pc_4_unro <- principal(dataZ, nfactors=4, rotate="none", scores=TRUE) 
pc_4_unro


pc4r <- principal(dataZ, nfactors=4, rotate="varimax")
pc4r


pc4r$loadings

round(pc4r$communality,2)

__________________________________________________________

#INTERPRETA??O 5 PC?S:

#RC1 - SOIAICT, AUTICT, COMPICT, INTICT - Utiliza??o e conhecimento das ict's na sociedade - UTI_OUT
#RC2 - USESCH, HOMESCH, ENTUSE - Utilizacao das ict's aplicadas a escola - UTI_IN
#RC3 - ICTRES, ICTHOME - acesso as ict's e aos seus recursos - ACC_OUT
#RC4 - ICTOUTSIDE, ICTCLASS - Utiliza??o das ict's dentro ou fora das aulas, relacionado com as mesmas- ICT_USE
#RC5 - ICTSCH - Disponibilade das ics e seus recursos nas escolas - ICT_AVA


pc5sc <- principal(dataZ, nfactors=5, rotate="none", scores=TRUE)
round(pc5sc$scores,5)
                                                                        

mean(pc5sc$scores[,1])


sd(pc5sc$scores[,1])


df_v4$UTI_OUT <- pc5sc$scores[,1]
df_v4$UTI_IN <- pc5sc$scores[,2]
df_v4$ACC_OUT <- pc5sc$scores[,3]
df_v4$ICT_USE <- pc5sc$scores[,4]
df_v4$ICT_AVA <- pc5sc$scores[,5]



head(df_v4)
summary(df_v4)


# Depict the scatterplot of PC2 vs PC4
plot(df_v4$UTI_OUT,df_v4$UTI_IN , pch = 19, xlim = c(-4,5),
     ylim = c(-4,5), xlab = "Utilidade fora", ylab = "Utilidade dentro", 
     main = "Scores: Utilidade fora vs Utilidade dentro")
text(df_v4$UTI_OUT, df_v4$UTI_IN - 0.1, df_v4[,2])
options(repr.plot.width = 10, repr.plot.height = 10, message = FALSE)

plot(df_v4$UTI_OUT,df_v4$ICT_AVA , pch = 19, xlim = c(-4,5),
     ylim = c(-4,5), xlab = "Utilidade fora", ylab = "Disponibilade das ics e seus recursos nas escolas", 
     main = "Scores: Utilidade fora vs Disponibilade das ics e seus recursos nas escolas")
text(df_v4$UTI_OUT, df_v4$ICT_AVA - 0.1, df_v4[,2])
options(repr.plot.width = 10, repr.plot.height = 10, message = FALSE)


## Clustering
#Hierarchical cluster
pc_dist <- dist(df_v4[,18:22])
hclust  <- hclust(pc_dist, method='ward.D2')
plot(hclust, hang=-1, labels=FALSE)

df_v4[,18:22]
# Cut the dendrogram
groups.k6 <- cutree(hclust, k=6) # cut tree into 6 clusters
rect.hclust(hclust, k=6, border="red") 


plot(silhouette(groups.k6, pc_dist))

#Hierarchical cluster method complete
pc_dist <- dist(df_v4[,18:22]) # compute distance (no need of scaling)
hclust  <- hclust(pc_dist,method='complete')
plot(hclust, hang=-1, labels=FALSE)

# Cut the dendrogram
groups.k6_c <- cutree(hclust, k=6) # cut tree into 6 clusters
rect.hclust(hclust, k=6, border="red")

#Silhouette
plot(silhouette(groups.k6_c, pc_dist))
table(groups.k6,groups.k6_c)
# Based on the dendrogram select 6 clusters
# Silhouette shows a very weak separation




# Different results

# K-Means: number of clusters
wssplot <- function(xx, nc=15, seed=1234){
  wss <- (nrow(xx)-1)*sum(apply(xx,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(xx, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
wssplot(df_v4[,18:22], nc=10)

#K-means cluster com K=5
kmeans.k6 <- kmeans(df_v4[,18:22], 5, nstart=100) 
df_v4 = df_v4 %>% mutate(cluster = kmeans.k6$cluster)

table(groups.k6, df_v4$cluster)


#RC1 - SOIAICT, AUTICT, COMPICT, INTICT - Utiliza??o e conhecimento das ict's na sociedade - UTI_OUT
#RC2 - USESCH, HOMESCH, ENTUSE - Utilizacao das ict's aplicadas a escola - UTI_IN
#RC3 - ICTRES, ICTHOME - acesso as ict's e aos seus recursos - ACC_OUT
#RC4 - ICTOUTSIDE, ICTCLASS - Utiliza??o das ict's dentro ou fora das aulas, relacionado com as mesmas- ICT_USE
#RC5 - ICTSCH - Disponibilade das ics e seus recursos nas escolas - ICT_AVA

#Barplot of average score in each principal component within each cluster
barplot(colMeans(subset(df_v4,cluster==1)[,18:22]),main= "Cluster 1 - Average score in each principal component") #Alunos com acesso as ict's mas sem grande interesse nelas acando por so usa las para trabalho escolar - ACC_SCH_ONLY
barplot(colMeans(subset(df_v4,cluster==2)[,18:22]),main= "Cluster 2 - Average score in each principal component") #alunos que apenas têm acesso as ict's fora da escola utilizando-as meramente para fins academicos nao demonstrando grande ineteresse fora disso  - ACCOUT_INT_SCH
barplot(colMeans(subset(df_v4,cluster==3)[,18:22]),main= "Cluster 3 - Average score in each principal component") #Alunos sem interesse, acesso ou utilização- NOACC_NOINT_NOUSE
barplot(colMeans(subset(df_v4,cluster==4)[,18:22]),main= "Cluster 4 - Average score in each principal component") # Alunos sem grande acesso as ict's mas que demonstram grande interesse nelas e ainda que com pouco acesso utilizam nas para fins academicos - NoACC_INT
barplot(colMeans(subset(df_v4,cluster==5)[,18:22]),main= "Cluster 5 - Average score in each principal component") # Alunos com interesse nas ict mas que nao transparecem para o uso escolar - NOSCHOOLINT_OUTINT




### PARA TODAS AS VARIAVEIS PROFILE !!!
#Barplot of genre distribution within each cluster
barplot(prop.table(table(subset(df_v4,cluster==1)[,16])),main= "Cluster 1 vs. Gender ")
barplot(prop.table(table(subset(df_v4,cluster==2)[,16])),main= "Cluster 2 vs.Gender ")
barplot(prop.table(table(subset(df_v4,cluster==3)[,16])),main= "Cluster 3 vs.Gender ")
barplot(prop.table(table(subset(df_v4,cluster==4)[,16])),main= "Cluster 4 vs.Gender ")
barplot(prop.table(table(subset(df_v4,cluster==5)[,16])),main= "Cluster 5 vs.Gender ")


barplot(prop.table(table(subset(df_v4,cluster==1)[,15])),main= "Cluster 1 vs. Age ")
barplot(prop.table(table(subset(df_v4,cluster==2)[,15])),main= "Cluster 2 vs. Age")
barplot(prop.table(table(subset(df_v4,cluster==3)[,15])),main= "Cluster 3 vs. Age")
barplot(prop.table(table(subset(df_v4,cluster==4)[,15])),main= "Cluster 4 vs. Age")
barplot(prop.table(table(subset(df_v4,cluster==5)[,15])),main= "Cluster 5 vs. Age")


barplot(prop.table(table(subset(df_v4,cluster==1)[,14])),main= "Cluster 1 vs. ESCS ")
barplot(prop.table(table(subset(df_v4,cluster==2)[,14])),main= "Cluster 2 vs. ESCS")
barplot(prop.table(table(subset(df_v4,cluster==3)[,14])),main= "Cluster 3 vs. ESCS")
barplot(prop.table(table(subset(df_v4,cluster==4)[,14])),main= "Cluster 4 vs. ESCS")
barplot(prop.table(table(subset(df_v4,cluster==5)[,14])),main= "Cluster 5 vs. ESCS")


barplot(prop.table(table(subset(df_v4,cluster==1)[,13])),main= "Cluster 1 vs. Wealth ")
barplot(prop.table(table(subset(df_v4,cluster==2)[,13])),main= "Cluster 2 vs. wealth")
barplot(prop.table(table(subset(df_v4,cluster==3)[,13])),main= "Cluster 3 vs. wealth")
barplot(prop.table(table(subset(df_v4,cluster==4)[,13])),main= "Cluster 4 vs. wealth")
barplot(prop.table(table(subset(df_v4,cluster==5)[,13])),main= "Cluster 5 vs. wealth")




barplot(prop.table(table(subset(df_v4,cluster==1)[,17])),main= "Cluster 1 vs. Highest education ")
barplot(prop.table(table(subset(df_v4,cluster==2)[,17])),main= "Cluster 2 vs.Highest education ")
barplot(prop.table(table(subset(df_v4,cluster==3)[,17])),main= "Cluster 3 vs.Highest education ")
barplot(prop.table(table(subset(df_v4,cluster==4)[,17])),main= "Cluster 4 vs.Highest education ")
barplot(prop.table(table(subset(df_v4,cluster==5)[,17])),main= "Cluster 5 vs.Highest education ")


# Proportions
prop.table(table(subset(df_v4,cluster==1)[,17]))
prop.table(table(subset(df_v4,cluster==2)[,17]))
prop.table(table(subset(df_v4,cluster==3)[,17]))
prop.table(table(subset(df_v4,cluster==4)[,17]))


### GMM
set.seed(1233)

# Apply GMM with 5 components
results.G5 <- Mclust(df_v4[,18:22], G = 5)
summary(results.G5, parameters = TRUE)

# Some results
results.G5$modelName          # Optimal selected model
results.G5$G                  # Optimal number of cluster
head(results.G5$z, 5)         # Probability to belong to a given cluster
head(results.G5$classification, 5) # Cluster assignment of each observation

# select VEV, different volume, equal shape, different directions
plot(results.G5, what = "classification")
plot(results.G5, what = "uncertainty")


df_v4 = df_v4 %>% mutate(prob_cluster = results.G5$classification)

table(df_v4$cluster,df_v4$prob_cluster)

results.G5$parameters$mean



summary((subset(df_v4,cluster==1))[,13:14])
summary((subset(df_v4,cluster==2))[,13:14])
summary((subset(df_v4,cluster==3))[,13:14])
summary((subset(df_v4,cluster==4))[,13:14])
summary((subset(df_v4,cluster==5))[,13:14])






