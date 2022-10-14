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

plot(curve_chr)
