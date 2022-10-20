#Introductionary course to R programming and environment, Kharkiv, Nov 30 -Dec 1, 2017
#R programming in 6 hours or less
#1) Basics of R environment
#2)Hadling dataframes
#3)Basic statistics
#4)Basic graphics
#5)Basics of multivariate stats in R  

#this text file is called "script", everything after "#" is  a comment and would not be executed
demo(graphics)
################################################################################
#1) Basics of R environment
################################################################################
#a) Scalar objects
x<-5
y<-2+2
str(y)
#vector objects
#key operators "c"=combine, "$"=attach,"+","-","*","/","^","~" = "a function ~ of b"
a=c(0.1,4,6,8)   # "c" is an very important operator, it means "combine", it used to combine objects - elements into  vectors, vectors in tables etc.
insects=c("Melolonta melolonta","Leptinotarsa decimlineata","Pieris brassica","Lucanus cervus","Chironomus dorsalis", "bugs")
a1=(1:100)
a2=rnorm(1:100)
#call objects or their parts
insects
a
a2[a2<0]
a1[20:40]
insects[3]
insects[2:4]
insects[c(1,6)]
#creation of the simple vectors through the "rep"  and "seq" commands
rep(c(1:5),times=10)
seq(from=1, to=20,by=0.5)
rep(c("a","b","c","d"),times=2)
rep(c("a","b","c","d"),each=2)
#operations with vectors
d1=a1+a2
d2=a1-a2
d3=a1*a2
d4=c(a1,a2)# this will combine two vectors in one longer
#let's check the lenght of the vectors!
length(a1)
length(d4)
# we can get all sorts of information about vectors using simple functions
mean(d4)
max(d4)
sd(d4)
#matrix
m= matrix(rnorm(50),ncol=5,nrow=10) #lets create matrix
m
ncol(m)  #number of columns
nrow(m)  #number of rows
rowSums(m) #sum of all rows
colSums(m) #sum of all columns
rowMeans(m) #mean of all rows
colMeans(m) #mean of all columns
n=t(m)#transpone matrix

# sometimes you needto create which is factor based rather than numeric

species="Troglocladius hajdi"
#nov several factorial objects in a vector
taxa=c("Pantera pardus","Canis familaris", "Sus scrofa", "Capreolus capreolus ","Procyon lotor","Castor amicus","Felis sylvestris")

################################################################################
#3 Functions and repeated commands
################################################################################
#folowing dataset based on Rubner, 1883
mass=as.numeric(c(31.2,24.0,19.8,18.2,9.61,6.5,3.19)) #kg
Svr=as.numeric(c(344,366,379,421,550,573,726))   #S/V_rat/sm2/killo
miS=as.numeric(c(35.68, 40.91,45.87,46.2,65.16,66.07,88.07))#metabolic_intensity_killo/day-1_kkal
miV=as.numeric(c(1036,1112,1207,1097,1183,1153,1212))   #metabolic_intensity_m2/day-1_kkal
trophic=c("carn","carn","herb","herb","carn","herb","carn")
metabolism=cbind(taxa, mass,Svr,miS,miV,trophic)#cbind is a usefull column allowing to combine vectors into a matrix, as a columns
data<-data.frame(metabolism)
attach(data) #by using command "attach" we designating certain  dataset (here: "data")as a definitive for all actions, and dont need tospecify dataset any more
par(mfrow=c(2,2))
plot(Svr~mass,xlab="bodymass,kg",ylab="surface area to volume ratio",pch=17,col="red",cex=5)
plot(miS~mass,xlab="bodymass,kg",ylab="metabolic_intensity_killo/day-1_kkal",pch=7,col="blue",cex=2)
plot(miV~mass,xlab="bodymass,kg",ylab="metabolic_intensity_m2/day-1_kkal",pch=3,col="yellow",cex=2)
plot(miV~Svr,xlab="surface area to volume ratio",ylab="metabolic_intensity_m2/day-1_kkal",pch=13,col="green",cex=2)

plot(Svr~mass,xlab="bodymass,kg",ylab="surface area to volume ratio",pch=17,col="red",cex=2)
lines(Svr~mass)
boxplot(miV~trophic)
str(data)
head(data$mass)
tail(data$mass)
ncol(data)
nrow(data)


cols.num <- c("mass","Svr","miS","miV")
data[cols.num] <- sapply(data[cols.num],as.numeric)
sapply(data, class)


#install.packages("reshape")
require(reshape)
require(reshape2)
mydata_narrow=melt(data,id=c("taxa","trophic"))
mydata_wide1 <-cast(mydata_narrow, taxa+value~variable)

raz= read.table("raz.txt",sep="\t",header=TRUE)
require(reshape2)
raz_wide <- dcast(raz, time~Treatment, value.var="log", fun=sum)# spp level in wide form 
dens=raz_wide[,2:7]
n=6
my_lms <- lapply(1:n, function(x) lm(dens[,x] ~raz_wide$time))
summaries <- lapply(my_lms, summary)
aa<-lapply(summaries, function(x) x$coefficients[, c(1,4)])
xs<-sapply(summaries, function(x) c(r_sq = x$r.squared, adj_r_sq = x$adj.r.squared))

################################################################################
#4 R Graphics
################################################################################
taxa=c("Pantera pardus","Canis familaris", "Sus scrofa", "Capreolus capreolus ","Procyon lotor","Castor amicus","Felis sylvestris")
#folowing dataset based on Rubner, 1883
mass=as.numeric(c(31.2,24.0,19.8,18.2,9.61,6.5,3.19)) #kg
Svr=as.numeric(c(344,366,379,421,550,573,726))   #S/V_rat/sm2/killo
miS=as.numeric(c(35.68, 40.91,45.87,46.2,65.16,66.07,88.07))#metabolic_intensity_killo/day-1_kkal
miV=as.numeric(c(1036,1112,1207,1097,1183,1153,1212))   #metabolic_intensity_m2/day-1_kkal
trophic=c("carn","carn","herb","herb","carn","herb","carn")
metabolism=cbind(taxa, mass,Svr,miS,miV,trophic)#cbind is a usefull column allowing to combine vectors into a matrix, as a columns
data<-data.frame(metabolism)
attach(data) #by using command "attach" we designating certain  dataset (here: "data")as a definitive for all actions, and dont need tospecify dataset any more
par(mfrow=c(2,2))
plot(Svr~mass,xlab="bodymass,kg",ylab="surface area to volume ratio",pch=17,col="red",cex=2)
plot(miS~mass,xlab="bodymass,kg",ylab="metabolic_intensity_killo/day-1_kkal",pch=7,col="blue",cex=2)
plot(miV~mass,xlab="bodymass,kg",ylab="metabolic_intensity_m2/day-1_kkal",pch=3,col="yellow",cex=2)
plot(miV~Svr,xlab="surface area to volume ratio",ylab="metabolic_intensity_m2/day-1_kkal",pch=13,col="green",cex=2)

plot(Svr~mass,xlab="bodymass,kg",ylab="surface area to volume ratio",pch=17,col="red",cex=2)
lines(Svr~mass)
boxplot(miV~trophic)
dev.off()
#install.packages("ggplot2")
require(ggplot2)
p1=ggplot(raz,aes(x=time,y=log,colour=as.factor(Treatment)))+geom_point()
p2=p1+geom_smooth(method="loess",se=FALSE, linetype="dashed")+theme_bw()
p3=p2+xlab("hours, after experiment started") +ylab("log(rru/raz+1)")




#loop basics


# Create a vector filled with random normal values
u1 <- rnorm(30)
print("This loop calculates the square of the first 10 elements of vector u1")

# Initialize `usq`
usq <- 0

for(i in 1:10) {
  # i-th element of `u1` squared into `i`-th position of `usq`
  usq[i] <- u1[i]*u1[i]
  print(usq[i])
}

print(i)


#clean up
dev.off() #switch off the graphic device
rm(list=ls()) #clean r session


taxa=rep(c("Pantera pardus","Canis familaris", "Sus scrofa", "Capreolus capreolus ","Procyon lotor","Castor amicus","Felis sylvestris"),3)
specimen=rep(c(1:3),7)
weight=seq(1,100,by=4.76)
data1=cbind(taxa,specimen, weight)
data1=as.data.frame(data1)



require(reshape2)
#convert to the wide format (using reshape 2 project)
c_data1=dcast(data1,taxa~specimen,value.var="weight")

#conert columns to numeric format
cols.num <- c("1","2","3")
c_data1[cols.num] <- sapply(c_data1[cols.num],as.numeric)
summary(c_data1) #summary statistics of the dataset column

