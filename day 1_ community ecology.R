#install.packages("DataCombine")
#install.packages("vegan")
#install.packages("gtools")
#install.packages("fitdistrplus")
#install.packages("factoextra")
#install.packages("ggfortify")
#install.packages("ggrepel")
#install.packages("reshape2")
#install.packages("plyr")
library(ggrepel)
library(DataCombine)
require(vegan)
library(gtools)
require(nlme)
require(car)
require(fitdistrplus)
library(factoextra)
require(ggfortify)
require(reshape2)
require(plyr)
#load the data file witb abundances of aquatic insects from Breitenbach stream, Hesse
ept1= read.csv("ept_69_2006.csv",sep=";")
#repleace NA  in empty cells with 0
ept1[is.na(ept1)] <- 0 
#extract taxa part (without date part - first three columns)
taxa=ept1[,4:245]
#reading in separate file on Baetis vernus abundance 
t1= read.table("bver1.txt",sep="\t",header = TRUE) #reading in river data
#convert integer values to numeric
t1$II_Bae_ver_m1=as.numeric(t1$II_Bae_ver_m)
#add column with abundance of B. vernus to main taxa file
taxa$II_Bae_ver_m1=t1$II_Bae_ver_m1
taxa=taxa[,2:243]
taxa=taxa[,c(242,1:241)]
#merge males and females data (each spp has 2 columns, m & f abundance, we are making a new column, a sum off two)
output <-data.frame( Bae_ver=apply(taxa[1:2], 1, sum),Bae_roh=apply(taxa[3:4], 1, sum), Bae_sp=apply(taxa[5:6], 1, sum),Eph_muc=apply(taxa[7:8],1, sum),
                     Eph_ign=apply(taxa[9:10],1, sum),Cen_lut=apply(taxa[11:12],1, sum),Par_sub=apply(taxa[13:14],1, sum),Ecd_sp=apply(taxa[15:16],1, sum),
                     Hab_fus=apply(taxa[17:18],1, sum),Hab_lau=apply(taxa[19:20],1, sum),Epe_sp=apply(taxa[21:22],1, sum), Bae_mut=apply(taxa[23:24],1, sum),
                     Bae_fus=apply(taxa[25:26],1, sum),Bae_sca=apply(taxa[27:28],1, sum),Eph_dan=apply(taxa[29:30],1, sum),Rhi_sp=apply(taxa[31:32],1, sum),
                     EEE=apply(taxa[33:35],1, sum),Amp_sta=apply(taxa[36:37],1, sum),Amp_sul=apply(taxa[38:39],1, sum),Bra_ris=apply(taxa[40:41],1, sum),
                     Bra_set=apply(taxa[42:43],1, sum),Iso_gra=apply(taxa[44:45],1, sum),Iso_goe=apply(taxa[46:47],1, sum),Leu_dig=apply(taxa[48:49],1, sum),
                     Leu_fus=apply(taxa[50:51],1, sum),Leu_ine=apply(taxa[52:53],1, sum),Leu_nig=apply(taxa[54:55],1, sum),Leu_pri=apply(taxa[56:57],1, sum),
                     Leu_sp=apply(taxa[58:59],1, sum),Nem_cam=apply(taxa[60:61],1, sum),Nem_cin=apply(taxa[62:63],1, sum),Nem_fle=apply(taxa[64:65],1, sum),
                     Nem_mar=apply(taxa[66:67],1, sum),Nrl_pic=apply(taxa[68:69],1, sum),Nem_sp=apply(taxa[70],1, sum),
                     Per_dis=apply(taxa[71:72],1, sum),Per_mic=apply(taxa[73:74],1, sum),Pro_aub=apply(taxa[75:76],1, sum),Pro_int=apply(taxa[77:78],1, sum),
                     Pro_mey=apply(taxa[79:80],1, sum),Pro_nit=apply(taxa[81:82],1, sum),Sip_tor=apply(taxa[83:84],1, sum),PPP=apply(taxa[85:87],1, sum)
                     ,Adi_fil=apply(taxa[88:89],1, sum),Adi_red=apply(taxa[90:91],1, sum),Aga_fus=apply(taxa[92:93],1, sum),Agr_mul=apply(taxa[94:95],1, sum)
                     ,Ana_ner=apply(taxa[96:97],1, sum),Ann_obs=apply(taxa[98:99],1, sum),Apa_fim=apply(taxa[100:101],1, sum),Ath_ate=apply(taxa[102:103],1, sum),
                     Ber_eid=apply(taxa[104:105],1, sum),Ber_pul=apply(taxa[106:107],1, sum), Bra_sub=apply(taxa[108:109],1, sum),Cer_dis=apply(taxa[110:111],1, sum),
                     Cha_vil=apply(taxa[112:113],1, sum),Che_lep=apply(taxa[114:115],1, sum),Cru_irr=apply(taxa[116:117],1, sum),Cyr_tri=apply(taxa[118:119],1, sum),
                     Dip_fel=apply(taxa[120:121],1, sum),Dru_ann=apply(taxa[122:123],1, sum),Dru_big=apply(taxa[124:125],1, sum),Glo_con=apply(taxa[126:127],1, sum),
                     Gly_pel=apply(taxa[128:129],1, sum), Gra_nig=apply(taxa[130:131],1, sum),Gra_nit=apply(taxa[132:133],1, sum),Hal_dig=apply(taxa[134:135],1, sum),
                     Hal_rad=apply(taxa[136:137],1, sum),Hal_tes=apply(taxa[138:139],1, sum),Hyd_ang=apply(taxa[140:141],1, sum),Hyd_ins=apply(taxa[142:143],1, sum),
                     Hyd_pel=apply(taxa[144:145],1, sum),Hyd_sax=apply(taxa[146:147],1, sum),Hyd_sil=apply(taxa[148:149],1, sum),Hyd_sp=apply(taxa[150:151],1, sum),
                     Hpt_ang=apply(taxa[152:153],1, sum),Hyd_pul=apply(taxa[154:155],1, sum),Hpt_spa=apply(taxa[156:157],1, sum),Las_bas=apply(taxa[158:159],1, sum),
                     Lim_aur=apply(taxa[160:161],1, sum),Lim_bip=apply(taxa[162:163],1, sum),Lim_cen=apply(taxa[164:165],1, sum),Lim_ext=apply(taxa[166:167],1, sum),
                     Lim_fus=apply(taxa[168:169],1, sum),Lim_gri=apply(taxa[170:171],1, sum),Lim_hir=apply(taxa[172:173],1, sum),Lim_lun=apply(taxa[174:175],1, sum),
                     Lim_ign=apply(taxa[176:177],1, sum),Lim_rho=apply(taxa[178:179],1, sum),Lim_spa=apply(taxa[180:181],1, sum),Lyp_pha=apply(taxa[182:183],1, sum),
                     Lip_red=apply(taxa[184:185],1, sum),Mic_lon=apply(taxa[186:187],1, sum),Mpt_lat=apply(taxa[188:189],1, sum),Mpt_nyc=apply(taxa[190:191],1, sum),
                     Mpt_seq=apply(taxa[192:193],1, sum),Mys_lon=apply(taxa[194:195],1, sum),Mys_nig=apply(taxa[196:197],1, sum),Odo_alb=apply(taxa[198:199],1, sum),
                     Par_pic=apply(taxa[200:201],1, sum),Ple_con=apply(taxa[202:203],1, sum),Pol_fla=apply(taxa[204:205],1, sum),Pot_cin=apply(taxa[206:207],1, sum),
                     Pot_lat=apply(taxa[208:209],1, sum),Pot_luc=apply(taxa[210:211],1, sum),Pot_nig=apply(taxa[212:213],1, sum),Psy_pus=apply(taxa[214:215],1, sum),
                     Pti_gra=apply(taxa[216:217],1, sum),Rhy_fas=apply(taxa[218:219],1, sum),Rhy_nub=apply(taxa[220:221],1, sum),Ser_fla=apply(taxa[222:223],1, sum),
                     Ser_per=apply(taxa[224:225],1, sum),Sil_pal=apply(taxa[226:227],1, sum),Ste_per=apply(taxa[228:229],1, sum),Syn_mos=apply(taxa[230:231],1, sum),
                     Tin_pal=apply(taxa[232:233],1, sum),Tin_ros=apply(taxa[234:235],1, sum),Tin_wae=apply(taxa[236:237],1, sum),Wor_occ=apply(taxa[238:239],1, sum),
                     TTT=apply(taxa[240:242],1, sum))
#create a composit date from three first columns of ept1 dataframe
date=ept1[,1:3]
#bind toogether date and taxa dataframes
taxax=cbind(date,output)
#melt the dataset into a long form
eptmelt <- melt(taxax, id=c("II_Jahr","II_Monat","II_Tag"))
#get abundance of each species per year
eptx1=aggregate(eptmelt[, 5], list(eptmelt$II_Jahr,eptmelt$ variable),sum)
#cast the dataset back into the wide form for community analysis
ept_wide1 <- dcast(eptx1,Group.1~Group.2, value.var="x", fun=sum)# spp level in wide form
#extract the names of all species
a1=unique(eptx1$Group.2)
#assign the order identity to each species, Eph - Ephemeroptera, Ple -Plecoptera, Tri -ca
vec=c(rep("Eph",16),rep("Ple",26),rep("Tri",78))
#add order identity to the dataset
a1=data.frame(a1)
a2=cbind(a1,vec)#bind vector with sp names to order names vector
a2$Group.2=a2$a1 #rename species column so it can be used as a comon variable to merge dataframe
#with orders id and main dataframe with spp abundance
a2=a2[,2:3]#remove repeating column with spp names
eptx1<-merge(eptx1,a2, by = "Group.2", all.x = TRUE, all.y = TRUE)
#aggregate abundance per year by order
eptx12=aggregate(eptx1[, 3], list(eptx1$Group.1,eptx1$vec),sum)
#plot abundance of the three orders of aquatic insects
ggplot(eptx12,aes(x=Group.1,y=x,colour=as.factor(Group.2)))+geom_point()+geom_smooth(method="lm")+xlab("year")+ylab("specimens number")+theme_bw()
############################
#do the same procedures for post 2006 data from breitenbach


#ept_wide1$rich=specnumber

ept2= read.csv("eptx2.csv",sep=";") #load post 2006 EPT data

ept2[is.na(ept2)] <- 0 

taxa=ept2[,4:248]
output1<-data.frame( Bae_ver=apply(taxa[1:2], 1, sum),Bae_roh=apply(taxa[3:4], 1, sum), Bae_sp=apply(taxa[5:6], 1, sum),Eph_muc=apply(taxa[7:8],1, sum),
                     Eph_ign=apply(taxa[9:10],1, sum),Cen_lut=apply(taxa[11:12],1, sum),Par_sub=apply(taxa[13:14],1, sum),Ecd_sp=apply(taxa[15:16],1, sum),
                     Hab_fus=apply(taxa[17:18],1, sum),Hab_lau=apply(taxa[19:20],1, sum),Epe_sp=apply(taxa[21:22],1, sum), Bae_mut=apply(taxa[23:24],1, sum),
                     Bae_fus=apply(taxa[25:26],1, sum),Bae_sca=apply(taxa[27:28],1, sum),Eph_dan=apply(taxa[29:30],1, sum),Rhi_sp=apply(taxa[31:32],1, sum),
                     EEE=apply(taxa[33:35],1, sum),Amp_sta=apply(taxa[36:37],1, sum),Amp_sul=apply(taxa[38:39],1, sum),Bra_ris=apply(taxa[40:41],1, sum),
                     Bra_set=apply(taxa[42:43],1, sum),Iso_gra=apply(taxa[44:45],1, sum),Iso_goe=apply(taxa[46:47],1, sum),Leu_dig=apply(taxa[48:49],1, sum),
                     Leu_fus=apply(taxa[50:51],1, sum),Leu_ine=apply(taxa[52:53],1, sum),Leu_nig=apply(taxa[54:55],1, sum),Leu_pri=apply(taxa[56:57],1, sum),
                     Leu_sp=apply(taxa[58:60],1, sum),Nem_cam=apply(taxa[61:62],1, sum),Nem_cin=apply(taxa[63:64],1, sum),Nem_fle=apply(taxa[65:66],1, sum),
                     Nem_mar=apply(taxa[67:68],1, sum),Nrl_pic=apply(taxa[69:70],1, sum),Nem_sp=apply(taxa[71:72],1, sum),
                     Per_dis=apply(taxa[73:74],1, sum),Per_mic=apply(taxa[75:76],1, sum),Pro_aub=apply(taxa[77:78],1, sum),Pro_int=apply(taxa[79:80],1, sum),
                     Pro_mey=apply(taxa[81:82],1, sum),Pro_nit=apply(taxa[83:84],1, sum),Sip_tor=apply(taxa[85:86],1, sum),PPP=apply(taxa[87:90],1, sum)
                     ,Adi_fil=apply(taxa[91:92],1, sum),Adi_red=apply(taxa[93:94],1, sum),Aga_fus=apply(taxa[95:96],1, sum),Agr_mul=apply(taxa[97:98],1, sum)
                     ,Ana_ner=apply(taxa[99:100],1, sum),Ann_obs=apply(taxa[101:102],1, sum),Apa_fim=apply(taxa[103:104],1, sum),Ath_ate=apply(taxa[105:106],1, sum),
                     Ber_eid=apply(taxa[107:108],1, sum),Ber_pul=apply(taxa[109:110],1, sum), Bra_sub=apply(taxa[111:112],1, sum),Cer_dis=apply(taxa[113:114],1, sum),
                     Cha_vil=apply(taxa[115:116],1, sum),Che_lep=apply(taxa[117:118],1, sum),Cru_irr=apply(taxa[119:120],1, sum),Cyr_tri=apply(taxa[121:122],1, sum),
                     Dip_fel=apply(taxa[123:124],1, sum),Dru_ann=apply(taxa[125:126],1, sum),Dru_big=apply(taxa[127:128],1, sum),Glo_con=apply(taxa[129:130],1, sum),
                     Gly_pel=apply(taxa[131:132],1, sum), Gra_nig=apply(taxa[133:134],1, sum),Gra_nit=apply(taxa[135:136],1, sum),Hal_dig=apply(taxa[137:138],1, sum),
                     Hal_rad=apply(taxa[139:140],1, sum),Hal_tes=apply(taxa[141:142],1, sum),Hyd_ang=apply(taxa[143:144],1, sum),Hyd_ins=apply(taxa[145:146],1, sum),
                     Hyd_pel=apply(taxa[147:148],1, sum),Hyd_sax=apply(taxa[149:150],1, sum),Hyd_sil=apply(taxa[151:152],1, sum),Hyd_sp=apply(taxa[153:154],1, sum),
                     Hpt_ang=apply(taxa[155:156],1, sum),Hyd_pul=apply(taxa[157:158],1, sum),Hpt_spa=apply(taxa[159:160],1, sum),Las_bas=apply(taxa[161:162],1, sum),
                     Lim_aur=apply(taxa[163:164],1, sum),Lim_bip=apply(taxa[165:166],1, sum),Lim_cen=apply(taxa[167:168],1, sum),Lim_ext=apply(taxa[169:170],1, sum),
                     Lim_fus=apply(taxa[171:172],1, sum),Lim_gri=apply(taxa[173:174],1, sum),Lim_hir=apply(taxa[175:176],1, sum),Lim_lun=apply(taxa[177:178],1, sum),
                     Lim_ign=apply(taxa[179:180],1, sum),Lim_rho=apply(taxa[181:182],1, sum),Lim_spa=apply(taxa[183:184],1, sum),Lyp_pha=apply(taxa[185:186],1, sum),
                     Lip_red=apply(taxa[187:188],1, sum),Mic_lon=apply(taxa[189:190],1, sum),Mpt_lat=apply(taxa[191:192],1, sum),Mpt_nyc=apply(taxa[193:194],1, sum),
                     Mpt_seq=apply(taxa[195:196],1, sum),Mys_lon=apply(taxa[197:198],1, sum),Mys_nig=apply(taxa[199:200],1, sum),Odo_alb=apply(taxa[201:202],1, sum),
                     Par_pic=apply(taxa[203:204],1, sum),Ple_con=apply(taxa[205:206],1, sum),Pol_fla=apply(taxa[207:208],1, sum),Pot_cin=apply(taxa[209:210],1, sum),
                     Pot_lat=apply(taxa[211:212],1, sum),Pot_luc=apply(taxa[213:214],1, sum),Pot_nig=apply(taxa[215:216],1, sum),Psy_pus=apply(taxa[217:218],1, sum),
                     Pti_gra=apply(taxa[219:220],1, sum),Rhy_fas=apply(taxa[221:222],1, sum),Rhy_nub=apply(taxa[223:224],1, sum),Ser_fla=apply(taxa[225:226],1, sum),
                     Ser_per=apply(taxa[227:228],1, sum),Sil_pal=apply(taxa[229:230],1, sum),Ste_per=apply(taxa[231:232],1, sum),Syn_mos=apply(taxa[233:234],1, sum),
                     Tin_pal=apply(taxa[235:236],1, sum),Tin_ros=apply(taxa[237:238],1, sum),Tin_wae=apply(taxa[239:240],1, sum),Wor_occ=apply(taxa[241:242],1, sum),
                     TTT=apply(taxa[243:245],1, sum))
out=rbind(output,output1)
date1=ept2[,1:3]
taxay=cbind(date1,output1)

taxaf=rbind(taxax,taxay)
taxaf<- subset(taxaf, II_Jahr != "0")


ept2m <- melt(taxaf, id=c("II_Jahr","II_Monat","II_Tag"))

sp= read.table("spp.txt",sep="\t",header = TRUE) #reading full sp names
#create a table for each taxon abundance for years 1969-2010
ml<-merge(sp,ept2m, by = "variable", all.x = TRUE, all.y = TRUE)
#cast taxa abundance in wide form
eptw <- dcast(ml,Taxon~II_Jahr, value.var="value", fun=sum)# spp level in wide form
head(ml)






#Mixed effects models in ecology
########################################################
#load data on the discharge of water in the stream
dis= read.table("discharge.txt",sep="\t",header = TRUE) #reading in river data
dis$year=dis$Jahr
#load data on the water temprature at a sampling site
temp= read.table("temperature_siteB.txt",sep="\t",header=TRUE)
#merge temperature and discharge into a single dataframe
vars<-merge(temp,dis, by = "year", all.x = TRUE, all.y = TRUE)
#read dataset on general abundance
ins=read.table("all sites abund.txt",sep="\t",header = TRUE)
#aggregate insect abundance by year
aggregate(ins$x, by=list(Category=ins$Group.1), FUN=sum)
ins$year=ins$Group.1 #rename a column
#merge into one dataset insect abundance and environmental variables
ins2<-merge(ins,vars, by = "year", all.x = TRUE, all.y = TRUE)

data=ins2#renaming a dataset for the simplicity sake
#scale the variables for the use in the mixed efects models
data$mean_s=scale(data$mean)+10 #mean temperature scaled
data$sab=scale(data$x)+10 #abundance scaled

#start building a mixed effects model
fit <- glm(sab~mean_s+pattern+mean_s*pattern,data=data,family = Gamma)
#alternative approach to a model
md1<- gls(sab~mean_s+pattern+mean_s*pattern,data=data, correlation=corAR1(), method="ML",na.action = na.omit)
mod.gls.1.1 <- update(md1, correlation=corARMA(p=1))#best fit
#we also have tested this models, you can run them, but mod.gls.3 can take several hours to update
mod.gls.3 <- update(md1, correlation=corARMA(p=3))
mod.gls.0 <- update(md1, correlation=NULL)
anova(md1, mod.gls.3, mod.gls.1.1,mod.gls.0) # AR(2) vs AR(1)#Check for minimal AIC criterion
an2=Anova(mod.gls.1.1)#anova on the best fit model with latitude


#dataset of the model residuals
tdat <- data.frame(predicted=predict(fit), residual = residuals(fit))
#residuals distribution 
ggplot(tdat,aes(x=predicted,y=residual)) + geom_point() + geom_hline(yintercept=0, lty=3)
#residuals histogramm
ggplot(tdat,aes(x=residual)) + geom_histogram(bins=20, color="black")
#qqplot
ggplot(tdat,aes(sample=residual)) + stat_qq() + stat_qq_line()

Anova(fit)

#multivariate community analysis


n <- eptw$Taxon

# transpose all but the first column (name)
t_taxon <- as.data.frame(t(eptw[,-1]))
colnames(t_taxon) <-n
t_taxon$myfactor <- factor(row.names(t_taxon))

tax=t_taxon[,1:108]

#diversity indices
H <- diversity(tax)
simp <- diversity(tax, "simpson")
invsimp <- diversity(tax, "inv")
S <- specnumber(tax) ## rowSums(BCI > 0) does the same...
J <- H/log(S)
#combine taxon matrix with environmental variables
t_taxon=t_taxon[1:42,]
t_taxon$year=vars$year
t_taxon<-merge(t_taxon,vars, by = "year", all.x = TRUE, all.y = TRUE)
#simple PCA on taxon matrix
ins.pca <- prcomp(t_taxon[,2:109])

autoplot(ins.pca)
#ind <-  get_pca_ind(ins.pca)
#summary(ins.pca)
#head(ind$coord)
#coord=as.data.frame(ind$coord,rownames=T)
#contr=as.data.frame(ind$contrib,rownames=T)
#contr1=as.data.frame(ind$contrib,rownames=T)
#cont=contr[,1:4]#first 4 PC encompasing 99% of variation, lets extract them
#x4=coord[,1:4]
fc=t_taxon[,c(142, 188)]#Read explanatory variable data
sp=t_taxon[,2:109]#Read response variable data
spp=decostand(sp,method = "hellinger")#Convert response variables
spp[is.na(spp)] <- 0

fcc=log10(fc)#Converting explanatory variables
fcc[is.na(fcc)] <- 0

uu=rda(spp~.,fcc)#RDA Analysis
ii=summary(uu)  #View analysis results
sp=as.data.frame(ii$species[,1:2])*2#Depending on the drawing result, the drawing data can be enlarged or reduced to a certain extent, as follows
st=as.data.frame(ii$sites[,1:2])
yz=as.data.frame(ii$biplot[,1:2])
grp=as.data.frame(c(rep("a",4),rep("b",4),rep("c",4),rep("d",4)))#Grouping by Square Type
colnames(grp)="group"
anova.cca(uu, step = 1000)
sum_ii=anova.cca(uu, step = 1000, by = "term")
anova.cca(uu, step = 1000, by = "axis")
#plot RDA
ordiplot(uu, scaling = 0, main = "RDA - Scaling 0")


#lets do NMDS
ins_NMDS=metaMDS(t_taxon[,2:109],k=2) 
ins_NMDS=metaMDS(t_taxon[,2:109],k=2,trymax=100)
stressplot(ins_NMDS)
plot(ins_NMDS)
ordiplot(ins_NMDS,type="n")
orditorp(ins_NMDS,display="species",col="red",air=0.01)
orditorp(ins_NMDS,display="sites",cex=1.25,air=0.01)



#time series analysis
#TIME SERIES lets return get temperature observations from Schlitz, Germany for 1969-2010, and analyze the timeseries
#TIME SERIES lets return get temperature observations from Schlitz, Germany for 1969-2010, and analyze the timeseries
temp= read.table("mean_monthl_tempsiteB.txt",sep="\t",header=TRUE)
require(reshape2)
tem_melt<- melt(temp, id="Jahr")#melt month into the single column
tem=tem_melt[order(tem_melt$Jahr,tem_melt$variable),] #order by year and month
require(plyr)
tem<-rename(tem,c("Jahr"="Year","variable"="month","value"="TAVG")) #rename variables
#deal with the data seasonality

aver<-ts(tem$TAVG,start=c(1969,1),frequency=12) #create timeseries, specify start of the series and frequency of measurments
length(aver)
#492
index<-1:492
492/41
#12
time<-index/12
model<-lm(tem$TAVG~sin(time*2*pi)+cos(time*2*pi))
plot(time,tem$TAVG, pch=".")
lines(time, predict(model))
summary(model)
plot(model$resid,pch=".")
acf(model$resid)
temp<-ts(as.vector(tapply(tem$TAVG,list(tem$month,tem$Year),mean)))  #investigating montly periodicity
par(mfrow=c(1,1))
acf(temp)

ytemp<-ts(as.vector(tapply(tem$TAVG,tem$Year,mean))) #are the years also showing periodicty?
acf(ytemp)

#lets try a "decomposition" function!
av<-stl(aver,"periodic")#decompose timeseries into trend and seasonal fluctuations
plot(av)
require(trend)
ts=as.data.frame(av$time.series)
mk.test(ts$trend) #lets check the trend
require(mblm)
summary(mblm(TAVG~Year,data=tem)) #lets check the median-based model slope to find out the rate of temperature change per year

#lets transfrom data back into the wide format using "dcast" command
tem_wide <- dcast(tem, Year~ month, value.var="TAVG", fun=sum)# spp level in wide form
av$time.series





#Standard IRIS dataset example
df <- iris[1:4]
pca_res <- prcomp(df, scale. = TRUE)

autoplot(pca_res)

autoplot(pca_res, data = iris, colour = 'Species', label = TRUE, label.size = 3)

autoplot(pca_res, data = iris, colour = 'Species', loadings = TRUE)

library(cluster)
autoplot(clara(iris[-5], 3))
autoplot(fanny(iris[-5], 3), frame = TRUE)
