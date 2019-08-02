##Clustering#################

infile=read.csv('ClusterData.csv')
head(infile)

kc=kmeans(infile,3)
kc
summary(infile)
scaled_infile=scale(infile)#every time we have to use scale for clustering
summary(scaled_infile)

kc1=kmeans(scaled_infile,3)

age=tapply(infile$age,kc1$cluster,mean)
ht=tapply(infile$height,kc1$cluster,mean)
wt=tapply(infile$weight,kc1$cluster,mean)
ds=tapply(infile$DailySteps,kc1$cluster,mean)

cbind(age,ht,wt,ds)

############################################################################
wss = (nrow(scaled_infile)-1)*sum(apply(scaled_infile,2,var))
for(i in 2:15) wss[i] = sum(kmeans(scaled_infile,centers = i)$withinss)
plot(1:15,wss,type="b",xlab = "Number of clusters",ylab = "within groups sum of square")

#########Hclustering###########################################
#if data structure is small use Herirachical clustrering#########and data are big than use K means clustering

d=dist(scaled_infile,method = "euclidean")
length(d)

hc =hclust(d,method = "ward.D")
plot(hc)
rect.hclust(hc,k=5)
clusterno=cutree(hc,k=5)

age=tapply(infile$age,clusterno,mean)
ht=tapply(infile$height,clusterno,mean)
wt=tapply(infile$weight,clusterno,mean)
ds=tapply(infile$DailySteps,clusterno,mean)

cbind(age,ht,wt,ds)
#####################################
