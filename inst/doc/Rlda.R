## ---- echo=TRUE, message=FALSE-------------------------------------------
library(Rlda)
#Read the Complaints data
data(complaints)
#Create the abundance matrix
library(reshape2)
mat1<- dcast(complaints[ ,c("Company","Issue")], 
       Company~Issue, length, 
       value.var="Issue")
#Create the rowname
rownames(mat1)<- mat1[,1]
#Remove the ID variable
mat1<- mat1[,-1]

## ---- echo=TRUE, message=FALSE-------------------------------------------
#Set seed
set.seed(9292)
#Hyperparameters for each prior distribution
beta<- rep(1,ncol(mat1))
gamma<- 0.01
#Execute the LDA for the Multinomial entry
res<- rlda.multinomial(data=mat1, n_community=30, beta, gamma, n_gibbs=1000,
                       ll_prior=TRUE, display_progress=FALSE)

## ---- echo=TRUE, message=FALSE, fig.width=6, fig.height=2.5--------------
#Get the logLikelihood
ll<- res$logLikelihood
#Plot the log-likelihood
plot(ll, type="l", xlab="Iterations",
          ylab="Log(likel.)+log(prior)")
abline(v=700,col='grey')

## ----eval=FALSE, echo=TRUE, message=FALSE--------------------------------
#  #Get the Theta Estimate
#  Theta<-summary(res)$Theta

## ----eval=FALSE, echo=TRUE, message=FALSE--------------------------------
#  library('rgl')
#  library('car')
#  scatter3d(x=Theta[,'Cluster1'], y=Theta[,'Cluster2'], z=Theta[,'Cluster3'],
#               surface=F, xlab='Cluster 1', ylab='Cluster 2', zlab='Cluster 3',
#               labels=rownames(Theta), id.n=20)

## ----eval=TRUE, echo=TRUE, message=FALSE---------------------------------
#Load data
data(presence)
#Set seed
set.seed(9842)
#Hyperparameters for each prior distribution
gamma <-0.01
alpha0<-0.01
alpha1<-0.01
#Execute the LDA for the Binomial entry
res<-rlda.bernoulli(presence, 10, alpha0, alpha1, gamma,
                    5000, TRUE, FALSE)

## ---- echo=TRUE, message=FALSE, fig.width=6, fig.height=2.5--------------
#Burnout
Phi<-summary(res, burnin=0.1, silent=TRUE)$Phi
#Color
library(RColorBrewer)
myColor<-brewer.pal(n = 10, name = "RdBu")
#Labels
stars(Phi,col.segments=myColor,scale=TRUE,
      draw.segments=TRUE,ncol=4,flip.labels=FALSE,cex=0.6)

