taxa=read.csv("taxa_Chimeno_2022.csv",sep=",")#data file with taxa size
taxa=taxa[,1:11]
#install.packages("BiodiversityR")
#install.packages("reshape")
#install.packages("iNEXT")
#install.packages("SpadeR")
#install.packages("SPECIES")
require(BiodiversityR)
require(vegan)
require(dplyr)
require(ggplot2)
require(reshape2)
require(iNEXT)
require(SpadeR)
require(SPECIES)
#all diptera
taxa$index=rep(c(1), times = 70293)
dpt=dcast(taxa, Project.Code~BIN, value.var="index")
dpt=dpt[,c(1,3:8793)]
All_dpt = dpt[2:8792]
BIN_n=apply(All_dpt[,-1]>0,1,sum)#species number
#Specimens=rowSums(All_chir)
Specimens_BIN=colSums(All_dpt)

Diversity(Specimens_BIN,"abundance",q=c(0,0.5,1,1.5,2)) #Hill estimator fro BIN's here

#DARK TAXA 
#Ok, so I am going to calculate Hill and Chao estimates for every "family"
#then we can write a model lineary interpolating the numbers for the German/Bavarian fauna

ChaoSpecies(ChaoSpeciesData$Abu,"abundance",k=10,conf=0.95)
#Chironomidae diversity curve
chiro=subset(taxa,taxa$Family=="Chironomidae")
chiro$index=rep(c(1), times = 3589)#prescence abscence indexing
chr=dcast(chiro, Project.Code~BIN, value.var="index")
chr=chr[,c(1,3:298)]
SBin<- specnumber(chr)

All_chir = chr[2:297]
BIN_n=apply(All_chir[,-1]>0,1,sum)#species number
#Specimens=rowSums(All_chir)
Specimens_BIN=colSums(All_chir)
ChaoSpecies(Specimens_BIN,"abundance",k=10,conf=0.95)#Chao species estimator here done for BIN's

Diversity(Specimens_BIN,"abundance",q=c(0,0.5,1,1.5,2)) #Hill estimator fro BIN's here


curve_chr = specaccum(All_chir, method = "random", 
                      permutations = 100)

#plot rarefaction curve
sites=curve_chr$sites #extract sites as vector
richness=curve_chr$richness #extract spp richness as vector
sd=curve_chr$sd
curve1=cbind(sites, richness,sd)
curve1=as.data.frame(curve1)
plot(curve_chr)

plot(curve_chr,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", 
     main = "Default: Prettier CI")


#do it with iNext

chiro=as.list(setNames(curve_chr$sites,curve_chr$richness))
All_chir1 <- iNEXT(All_chir, q=0, datatype="abundance")

out <- iNEXT(SBin, q=0, datatype="abundance")
ggiNEXT(out, type=1)


# Sample-size-based R/E curves, separating by "site""
ggiNEXT(out, type=1, facet.var="site")

#and with raremax in vegan

S <- specnumber(chr) # observed number of species/BIN's
(raremax <- min(rowSums(chr[,2:297])))
Srare <- rarefy(chr[,2:297], raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(BCI, step = 20, sample = raremax, col = "blue", cex = 0.6)

