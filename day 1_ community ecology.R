#install.packages("DataCombine")
#install.packages("vegan")
#install.packages("gtools")
library(DataCombine)
require(vegan)
library(gtools)
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
#
eptmelt <- melt(taxax, id=c("II_Jahr","II_Monat","II_Tag"))

eptx1=aggregate(eptmelt[, 5], list(eptmelt$II_Jahr,eptmelt$ variable),sum)
eptx11=aggregate(eptmelt[, 5], list(eptmelt$II_Jahr),sum)
ept_wide1 <- dcast(eptx1,Group.1~Group.2, value.var="x", fun=sum)# spp level in wide form
a1=unique(eptx1$Group.2)
vec=c(rep("Eph",34),rep("Ple",52),rep("Tri",156))
a1=data.frame(a1)
a2=cbind(a1,vec)
a2$Group.2=a2$a1
eptx1<-merge(eptx1,a2, by = "Group.2", all.x = TRUE, all.y = TRUE)
eptx12=aggregate(eptx1[, 3], list(eptx1$Group.1,eptx1$vec),sum)
ggplot(eptx12,aes(x=Group.1,y=x,colour=as.factor(Group.2)))+geom_point()+geom_smooth(method="lm")


#ept_wide1$rich=specnumber

ept2= read.csv("eptx2.csv",sep=";")

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

sp= read.table("spp.txt",sep="\t",header = TRUE) #reading in river data
ml<-merge(sp,ept2m, by = "variable", all.x = TRUE, all.y = TRUE)

eptw <- dcast(ml,Taxon~II_Jahr, value.var="value", fun=sum)# spp level in wide form
#eptw=eptw[,1:109]
write.table(eptw, file = "eptw.csv")

mtr= read.table("metr.txt",sep="\t",header = TRUE) #reading in river data
troph=mtr[,c(1,46,49:50,52:53)]

trp2m <- melt(troph, id=c("Year"))
######################




dis= read.table("discharge.txt",sep="\t",header = TRUE) #reading in river data
dis$year=dis$Jahr



temp= read.table("temperature_siteB.txt",sep="\t",header=TRUE)
vars<-merge(temp,dis, by = "year", all.x = TRUE, all.y = TRUE)


ins$year=ins$Group.1
ins1=aggregate(ins[,3], list(ins$year),sum)
ins1$year=ins1$Group.1
ins2<-merge(ins1,vars, by = "year", all.x = TRUE, all.y = TRUE)
#ins2<-merge(ins2,dis, by = "Group.1", all.x = TRUE, all.y = TRUE)
#ins2$discharge=ins2$x.y
#DataSlid1 <- slide(ins2, Var = "mean", slideBy = -1)
#DataSlid2<- slide(DataSlid1, Var = "discharge", slideBy = -1)

##DataSlid2$lag_mean=DataSlid2$`mean-1`
#DataSlid2$lag_dis=DataSlid2$`discharge-1`
attach(ins2)#
data=ins2
data$mean_s=scale(data$mean)+10

data$sab=scale(data$x)+10
#attach(data)
require(nlme)
require(car)
fit <- glm(sab~mean_s+pattern+mean_s*pattern,data=data, family = Gamma)
Anova(fit)
trp2m$year=trp2m$Year
vars1<-merge(data,trp2m, by = "year", all.x = TRUE, all.y = TRUE)
vars1=vars1[2:206,]


vars1$mean_s=scale(vars1$mean)+10
vars1$lvalue=logit(vars1$value*0.01) #logit relative troph group abundance
vars1$sval=scale(vars1$lvalue)+10#standartize relative troph group abundance

fit1 <- glm(sval~(mean_s+pattern+mean_s*pattern)*variable,data=vars1, family = Gamma)
Anova(fit1)
ptrop_time=ggplot(vars1,aes(x=mean_s,y=value, colour=as.factor(pattern)))+geom_point()+geom_smooth(method="lm",se=FALSE)+facet_wrap(~variable,scales="free")

ggplot(vars1,aes(x=year,y=value, colour=as.factor(variable)))+geom_point()+geom_smooth(se=FALSE)
trop#p2=p2+ theme(legend.position="top")
df1=smartbind(taxa1, ept2)
df1[is.na(df1)] <- 0


tax=df1[,4:269]




taxaf=taxaf[,c(1,4:123)]

taxaf=aggregate(taxaf[, 2:121], list(taxaf$II_Jahr),sum)


tax=taxaf[,2:121]

H <- diversity(tax)
simp <- diversity(tax, "simpson")
invsimp <- diversity(tax, "inv")
S <- specnumber(tax) ## rowSums(BCI > 0) does the same...
J <- H/log(S)



taxaf$richness=S
taxaf$div=H
taxaf$simp1=simp
taxaf$evennes=J

div=taxaf[,c(1,122:125)]

ab=aggregate(ept2m[, 5], list(ept2m$II_Jahr),sum)
ab$II_Jahr=ab$Group.1
div$II_Jahr=div$Group.1
div.ab<-merge(div,ab, by = "II_Jahr", all.x = TRUE, all.y = TRUE)
div.ab$year=div.ab$II_Jahr
div.ab1<-merge(div.ab,vars1, by = "year", all.x = TRUE, all.y = TRUE)

div.ab1$s_mean=scale(div.ab1$mean)+10
div.ab1$S_rich=scale(div.ab1$richness)+10
div.ab1$S_div=scale(div.ab1$div)+10
div.ab1$S_simp=scale(div.ab1$simp1)+10
div.ab1$S_ev=scale(div.ab1$evennes)+10
div.ab1$S_troph=scale(div.ab1$value)+10
attach(div.ab1)
fit2 <- glm(S_div~(mean_s+pattern+mean_s*pattern)*variable,data=div.ab1, family = Gamma)
fit2.1 <- glm(S_div~mean_s+pattern+mean_s*pattern,data=div.ab1, family = Gamma)
fit2.2 <- glm(S_rich~mean_s+pattern+mean_s*pattern,data=div.ab1, family = Gamma)
dis$Year=dis$Jahr
trp<-merge(dis,troph, by = "Year", all.x = TRUE, all.y = TRUE)
ab1=aggregate(ept2m[, 5], list(ept2m$II_Jahr,ept2m$variable),sum)




attach(data)
require(nlme)
require(car)
fit <- glm(sab~mean_s+pattern+mean_s*pattern, family = Gamma)

require(codyn)
total.res <- turnover(df=ab1,time.var = "Group.1",species.var = "Group.2",abundance.var = "x",replicate.var=NA)
rate=rate_change_interval(ab1,time.var = "Group.1", species.var = "Group.2", abundance.var = "x",replicate.var =NA)


total.ap <- turnover(df=ab1,time.var = "Group.1", species.var = "Group.2",abundance.var = "x",replicate.var=NA,metric="appearance")

total.ds <- turnover(df=ab1,time.var = "Group.1", species.var = "Group.2",abundance.var = "x",replicate.var=NA,metric="disappearance")
total.res$cat=rep("total",41)

total.ap$cat=rep("appearence",41)
total.ds$cat=rep("disappearence",41)

total.res$year=total.res$Group.1
total.ap$year=total.ap$Group.1
total.ds$year=total.ds$Group.1


total.res$turnover=total.res$total
total.res=total.res[,2:5]

total.ap$turnover=total.ap$appearance
total.ap=total.ap[,2:5]

total.ds$turnover=total.ds$disappearance 
total.ds=total.ds[,2:5]

tot.tr=rbind(total.res,total.ap)
tottr=rbind(tot.tr,total.ds)
cols <- c("appearence" = "#CCCC66", "disappearence" = "#333333", "total" = "#9933FF")
turn=ggplot(tottr,aes(x=year,y=turnover,colour=as.factor(cat)))+geom_point(size=3)+geom_smooth(method="lm",se=FALSE)
turn=turn+theme_bw()+xlab("years")+ylab("turnover")+ theme(legend.position="none")
turn=turn+theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+theme(legend.text = element_text(colour="black", size = 20, face = "bold"))
turn=turn+scale_color_manual(values = cols)





div.ab2<-merge(div.ab1,total.res, by = "year", all.x = TRUE, all.y = TRUE)
fit3<- glm(total~mean_s+pattern+mean_s*pattern,data=div.ab2, family = Gamma)

trp2m$year=trp2m$Year
tropm<-merge(trp2m,dis, by = "year", all.x = TRUE, all.y = TRUE)
pdis=ggplot(vars1,aes(x=year,y=mean.disch))+geom_point(aes(color = factor(pattern),size=5))+geom_line(lty=2) #discharge plot
pdis=pdis+theme_bw()+xlab("years")+ylab("mean annual discharge, liters/min-1")
pdis=pdis+theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+theme(legend.text = element_text(colour="black", size = 20, face = "bold"))+ theme(legend.position="none")


ptemp=ggplot(vars1,aes(x=year,y=mean))+geom_point(aes(size=5))+geom_smooth(method="lm",se=FALSE)+geom_line(lty=2) #temperature plot
ptemp=ptemp+theme_bw()+xlab("years")+ylab("mean annual temperature, Â°C")+ theme(legend.position="none")
ptemp=ptemp+theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+theme(legend.text = element_text(colour="black", size = 20, face = "bold"))


pab=ggplot(data,aes(x=year,y=x))+geom_point(aes(size=5))+geom_smooth(method="lm",se=FALSE)+geom_line(lty=2) #abundance vs yrs plot
pab=pab+theme_bw()+xlab("years")+ylab("summ of animal abundance, specimens")+ theme(legend.position="none")
pab=pab+theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+theme(legend.text = element_text(colour="black", size = 20, face = "bold"))


pabt=ggplot(data,aes(x=mean,y=x))+geom_point(aes(size=5))+geom_smooth(method="lm",se=FALSE)+geom_line(lty=2) #abundance vs temp plot
pabt=pabt+theme_bw()+xlab("mean annual temperature, Â°C")+ylab("summ of animal abundance, specimens")+ theme(legend.position="none")
pabt=pabt+theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+theme(legend.text = element_text(colour="black", size = 20, face = "bold"))

grid.arrange(ptemp,pdis,pab,pabt)

#######################################
par(mfrow=c(2,2))
####################################################
pdiv=ggplot(taxaf,aes(x=Group.1,y=div))+geom_point(aes(size=5))+geom_smooth(method="lm",se=FALSE)+geom_line(lty=2) #temperature plot
pdiv=pdiv+theme_bw()+xlab("years")+ylab("Shannon's diversity, H")+ theme(legend.position="none")
pdiv=pdiv+theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+theme(legend.text = element_text(colour="black", size = 20, face = "bold"))


prich=ggplot(taxaf,aes(x=Group.1,y=richness))+geom_point(aes(size=5))+geom_smooth(method="lm",se=FALSE)+geom_line(lty=2) #temperature plot
prich=prich+theme_bw()+xlab("years")+ylab("Species richness")+ theme(legend.position="none")
prich=prich+theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+theme(legend.text = element_text(colour="black", size = 20, face = "bold"))


pev=ggplot(taxaf,aes(x=Group.1,y=evennes))+geom_point(aes(size=5))+geom_smooth(method="lm",se=FALSE)+geom_line(lty=2) #temperature plot
pev=pev+theme_bw()+xlab("years")+ylab("Pielou evennes")+ theme(legend.position="none")
pev=pev+theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+theme(legend.text = element_text(colour="black", size = 20, face = "bold"))

grid.arrange(p5x,prich,pdiv,turn)
taxaf[is.na(taxaf)] <- 0#
summary(mblm(richness~Group.1,data=taxaf))
summary(mblm(div~Group.1,data=taxaf))
summary(mblm(simp1~Group.1,data=taxaf))
summary(mblm(J~Group.1,data=taxaf))



pturn=ggplot(div.ab2,aes(x=mean,y=turnover, colour=as.factor(pattern)))+geom_point(size=5)+geom_smooth(method="lm",se=FALSE)
pturn=pturn+theme_bw()+xlab("mean annual temperature, Â°C")+ylab("turnover")+ theme(legend.position="none")
pturn=pturn+theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+theme(legend.text = element_text(colour="black", size = 20, face = "bold"))

cols <- c("X......Grazers.and.scrapers" = "#009999", "X......Shredders" = "#330099", "X......Gatherers.Collectors" = "#666666", "X......Passive.filter.feeders" = "#CC6600","X......Predators"="#CC3399")
ptroph=ggplot(div.ab2,aes(x=year,y=value, colour=as.factor(variable)))+geom_point(size=5)+geom_smooth(method="lm",se=FALSE)
ptroph=ptroph+theme_bw()+xlab("year")+ylab("relative abundance, %")+ theme(legend.position="none")
ptroph=ptroph+theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+theme(legend.text = element_text(colour="black", size = 20, face = "bold"))+scale_fill_discrete(name="variable")
ptroph=ptroph+scale_color_manual(values = cols)



#px1=ggplot(d7, aes(x=Group.1, y=x, colour=as.factor(Group.2)))+geom_point(aes(size=5))+geom_smooth(method="lm",se = FALSE,lwd=3)+theme_bw()+ylab("number of specimens")+xlab("years")+scale_fill_discrete(name="phenophase shift")+ylim(0, 11000)+theme(axis.text=element_text(size=20),axis.title=element_text(size=20,face="bold"))

#px1=px1+ theme(legend.text = element_text(colour="black", size = 20, face = "bold"))
#px1=px1+ theme(legend.position="top")
#px1=px1+scale_color_manual(values = cols)
#df1=smartbind(taxa1, ept2)




h1=data.frame(H)
h1$year=1969:2010
div.ab2<-merge(h1,div.ab2, by = "year", all.x = TRUE, all.y = TRUE)

pdiv.1=ggplot(div.ab2,aes(x=mean,y=H, colour=as.factor(pattern)))+geom_point(size=5)+geom_smooth(method="lm",se=FALSE)
pdiv.1=pdiv.1+theme_bw()+xlab("mean annual temperature")+ylab("Shannon's diversity, H")+ theme(legend.position="none")
pdiv.1=pdiv.1+theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+theme(legend.text = element_text(colour="black", size = 20, face = "bold"))
#grid.arrange(pturn,pdiv.1,ptroph,gx1)
#grid.arrange(,ptroph,gx1)
grid.arrange(ptroph,pev,pturn,pdiv.1)




bp2005[is.na(bp2005)] <- 0 

summary(mblm(richness~Group.1,data=df2))
summary(mblm(div~Group.1,data=df2))
summary(mblm(simp1~Group.1,data=df2))
summary(mblm(J~Group.1,data=df2))




#abundance through the all sites
abtot= read.table("all sites abund.txt",sep="\t",header = TRUE) #reading in river data
pa=ggplot(abtot,aes(Group.1,x,colour=as.factor(label)))+geom_point()+geom_smooth(span = 0.7,se=FALSE,Lwd=2)+theme_bw()
pa+xlab("years")+ylab("abundance, specimens")



plot(Wor_occ$value~Wor_occ$Year,ylim=c(0,4.5),pch=19,xlab="year",ylab="standatized abundance")
points(Ple_con$value~Ple_con$Year,pch=17,col="red")
points(Bve$value~Bve$Year,pch=17,col="green")
points(Aga$value~Aga$Year,pch=19,col="blue")


#phen

bnx$year=bnx$Group.1
div.ab3<-merge(bnx,div.ab2, by = "year", all.x = TRUE, all.y = TRUE)
div.ab3$s_dur=scale(div.ab3$duration)+10
div.ab3$S_ct=scale(div.ab3$ct.week)+10
div.ab3$S_first=scale(div.ab3$week.first)+10
div.ab3$S_last=scale(div.ab3$week.last)+10

fit4<- glm(s_dur~(mean_s+pattern+mean_s*pattern+sab),data=div.ab3, family = Gamma)
ggplot(div.ab3,aes(x=mean,y=duration,colour=as.factor(pattern)))+geom_point()+geom_smooth(method="lm")
fit5<- glm(S_ct~(mean_s+pattern+mean_s*pattern+sab),data=div.ab3, family = Gamma)
ggplot(div.ab3,aes(x=mean,y=ct.week,colour=as.factor(pattern)))+geom_point()+geom_smooth(method="lm")
fit6<- glm(S_first~(mean_s+pattern+mean_s*pattern+sab),data=div.ab3, family = Gamma)
fit7<- glm(S_last~(mean_s+pattern+mean_s*pattern+sab),data=div.ab3, family = Gamma)
cols <- c("week.first" = "#3366CC", "week.last" = "#33CC99", "ct.week" = "#CC99FF", "duration" = "#666666")
bnxm <- melt(bnx, id=c("Group.1","year"))
bnxm=bnxm[! bnxm$variable %in% c("week.first","week.last"), ]

p5x= ggplot(bnxm, aes(Group.1,value,shape=as.factor(variable)))+geom_point(size=6)+geom_smooth(method="lm",se = FALSE,lwd=1)+theme_bw()+ylab("week, number")+xlab("year")+theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+geom_line(lty=2)
#shift in phenophases over time
p5x=p5x+ theme(legend.text = element_text(colour="black", size = 20, face = "bold"))
p5x=p5x+ theme(legend.position="none")
p5x=p5x
temp=div.ab1[,c(1,24)]
temp=aggregate(temp[,2], list(temp$year),mean)#sum of all selected for phenology taxa
bnxm$year=bnxm$Group.1
temp$year=temp$Group.1
bnxm<-merge(bnxm,temp, by = "Group.1", all.x = TRUE, all.y = TRUE)
bnx1<-merge(bnx,temp, by = "Group.1", all.x = TRUE, all.y = TRUE)
p6x= ggplot(bnxm, aes(x,value,colour=as.factor(variable)))+geom_point(size=6)+geom_smooth(method="lm",se = FALSE,lwd=3)+theme_bw()+ylab("week, number")+xlab("mean annual temperature, CÂ°")+theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))
#shift in phenophases over time
p6x=p6x+ theme(legend.text = element_text(colour="black", size = 20, face = "bold"))
p6x=p6x+ theme(legend.position="top")
p6x=p6x+scale_color_manual(values = cols)
grid.arrange(p5x, p6x,ncol=2)
