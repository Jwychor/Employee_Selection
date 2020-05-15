####Packages####
##setwd('C:/...')
selpack<-c('tidyverse','ggcorrplot','stringr','psych','reshape2','caret','fmsb')
sapply(selpack,require,character=T)
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}
system("cmd.exe /C dir")
system("cmd.exe /C dir /AH")


####Loading/Cleaning####
da<-read.csv('Globex Dataset.csv')

da$Termination[da$Termination=='']<-NA

####Analysis####
#Integrity Scale
alpha(da[,6:12])
alpha(da[,c(6:7,9:12)])

#Scale Scores without Integrity
da$IntegrityScale<-rowMeans(da[,c(6:7,9:12)])

#Create a Column of Terminated or Not
da$Termed<-ifelse(is.na(da$Termination),1,0)

#Predt Baby
predt(da[,c(18,19,20,21,23)],da[,c(5,13,15,16,17)])
colnames(da)

#Predictor Correlation Matrix
bc2<-round(cor(da[,c(5,13,14,15,16,17,23)]),2)
pmat<-cor_pmat(da[,c(5,13,14,15,16,17,23)])
ggc<-ggcorrplot(bc2,outline.color='white',type='lower',
                lab=T,ggtheme=ggplot2::theme_dark(),
                lab_col = 'black',colors=c('black','white','black'),
                show.legend=F)
ggc

#Predictor > Criteria Correlation Matrix
colnames(da)[24]<-'Terminated'
bc<-round(cor(da[,c(5,13,14,15,16,17,23)],da[,c(18,19,20,21,24)],use='pairwise.complete.obs'),2)

ggc2<-ggcorrplot(bc,outline.color='white',
                lab=T,ggtheme=ggplot2::theme_dark(),
                lab_col = 'black',colors=c('black','white','black'),sig.level = .05,
                show.legend=F)
ggc2

#Scaling Predictors
pre<-da[,c(5,13,15,16,17,23)]
colnames(da)
pre[,-c(6,2)]<- pre[,-c(6,2)] %>% mutate_all(funs(scale))
pre$Boldness<-(pre$Boldness-50)/10

#Logistic Regression with Termination
pclm<-glm(Termed~Wonderlic*Boldness*Strategic.Thinking*Interview,
          family='binomial',data=pre)
summary(pclm)

#Scaling Criteria
crit<-da[,c(18,19,20,21,24)]
crit[,c(1,2,4)]<-mutate_all(crit[,c(1,2,4)],funs(scale))

#Create Dichotomized Variable
crit$Accidents<-crit$Accidents*0.75
da$critscore<-rowMeans(crit,na.rm=T)

###Each Predictor Individually####
##Wonderlic
#ANOVA testing if mean Wonderlic scores differ by Gender, Division, or Level
lman<-lm(Wonderlic~Gender+Division+Level,data=da)
anova(lman)

##Integrity
anova(lma<-lm(Wonderlic~IntegrityScale,data=da))
colnames(da)

##Boldness
lma<-lm(Boldness~Gender,data=da)
summary(lma)

####Regression Model 1####
pre2<-pre
pre2$critscore<-da$critscore

cdp<-sample(1:nrow(pre2),size=round(nrow(pre2)*.75,0),replace=F)

ptrain<-pre2[cdp,]
ptest<-pre2[-cdp,]
utrain<-da[cdp,]
utest<-da[-cdp,]

reg1<-lm(critscore~Wonderlic+Boldness+Manage.People+
           Strategic.Thinking+IntegrityScale+Interview,data=ptrain)
r1<-summary(reg1)
r1
r1.5<-lm(critscore~Wonderlic+Boldness+Manage.People+
             Strategic.Thinking+IntegrityScale+Interview,data=utrain)
summary(r1.5)

reg2<-lm(critscore~Wonderlic+Boldness+IntegrityScale+Interview,data=ptrain)
r2<-summary(reg2)
r2
r2.5<-lm(critscore~Wonderlic+Boldness+IntegrityScale+Interview,data=utrain)
summary(r2.5)

#Copy and Paste Models
write.excel(round(r1$coefficients,2),row.names=F,col.names=F)
write.excel(round(r2$coefficients,2),col.names=F)

#Do the Two Models Statistically Differ?
anova(reg1,reg2)

#Estimate Shrinkage
ptest$cv<-predict(reg2,ptest)
tss<-sum((ptest$critscore-mean(ptest$critscore))**2)
rss<-sum((ptest$critscore-ptest$cv)**2)

#Cross Validated R-Squared
1-(rss/tss)

#1000 random sample of data showing that r^2 tends to be smaller for the larger group (???)
te<-vector()
tr<-vector()
for(i in 1:1000){
  split<-sample(1:nrow(pre2),size=round(nrow(pre2)*.75,0),replace=F)
  ptrain<-pre2[split,]
  ptest<-pre2[-split,]
  rr1<-lm(critscore~Wonderlic+Boldness+Manage.People+Strategic.Thinking+IntegrityScale+Interview,data=ptrain)
  rr2<-lm(critscore~Wonderlic+Boldness+Manage.People+Strategic.Thinking+IntegrityScale+Interview,data=ptest)
  tr[i]<-as.numeric(summary(rr1)$r.squared)
  te[i]<-as.numeric(summary(rr2)$r.squared)
}

yyy<-data.frame(tr,te)
colMeans(yyy)
summary(yyy$te)
summary(yyy$tr)

####Examining Selection Model Outcomes
pre$cv<-predict(reg2,pre)

head(da)
pre$Real_Score
pre$gender<-da$Gender
pre<-pre[rev(order(pre$cv)),]

#Prediction outcome ratios lead to adverse impact
table(pre$gender[1:126])

##Plotting Expectancy Table for Model 1
pre<-pre[order(as.numeric(rownames(pre))),]
pre$Real_Score<-da$critscore
cor(pre$Real_Score_Percentile,pre$Predicted_Score_Percentile)

pre$Predicted_Score_Percentile<-percentile(pre$cv)/100
pre$Real_Score_Percentile<-percentile(pre$Real_Score)/100

pre<-pre[order(pre$Real_Score_Percentile),]

#Expectancy Table 1
gg<-ggplot(pre,aes(x=Predicted_Score_Percentile,y=Real_Score_Percentile))
gg+geom_count(color=ifelse(pre$Predicted_Score_Percentile[!duplicated(pre[,c(10,11)])]>=.75,'green',ifelse(pre$Predicted_Score_Percentile[!duplicated(pre[,c(10,11)])]>=.5&pre$Predicted_Score_Percentile[!duplicated(pre[,c(10,11)])]<.75,'blue','red')),show.legend=F)+
  scale_size_continuous(range=c(2,4.5))+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::percent)+
  geom_vline(xintercept=.75,linetype='dashed',color='black',size=1)+
  geom_vline(xintercept=.5,linetype='dashed',color='black',size=1)+
  geom_vline(xintercept=0,color='black',size=1)+
  geom_hline(yintercept=.75,linetype='dashed',color='black',size=1)+
  geom_hline(yintercept=0,color='black',size=1)+
  labs(x='Predicted Score Percentile',y='Actual Score Percentile')+
  theme(axis.title = element_text(size=15))+
  annotate(geom='text',x=.9,y=1,label='71',color='black',size=5,fontface=2)+
  annotate(geom='text',x=.9,y=.3,label='61',color='black',size=5,fontface=2)+
  annotate(geom='text',x=.63,y=1,label='31',color='black',size=5,fontface=2)+
  annotate(geom='text',x=.63,y=.3,label='95',color='black',size=5,fontface=2)+
  annotate(geom='text',x=.25,y=.3,label='217',color='black',size=5,fontface=2)+
  annotate(geom='text',x=.25,y=1,label='29',color='black',size=5,fontface=2)+
  annotate(geom='text',x=.89,y=-.1,label='TP = 53%, TN = 84%, SR = 0.25',color='black',size=3.45,fontface=2)+
  annotate(geom='text',x=.89,y=-.05,label='75th Percentile Cutoff',color='black',size=4,fontface=2)+
  annotate(geom='text',x=.63,y=-.1,label='TP = 40%, TN = 88%, SR = 0.50',color='black',size=3.45,fontface=2)+
  annotate(geom='text',x=.63,y=-.05,label='50th Percentile Cutoff',color='black',size=4,fontface=2)+
  annotate(geom='text',x=.20,y=-.05,label='No Cutoff',color='black',size=4,fontface=2)+
  annotate(geom='text',x=.20,y=-.1,label='TP = 25%, TN = 75%, SR = 1.00',color='black',size=3.45,fontface=2)
                    

#Percentile Estimates
sum(pre$Real_Score_Percentile>=.75&pre$Predicted_Score_Percentile>=.75)
sum(pre$Real_Score_Percentile<.75&pre$Predicted_Score_Percentile>=.75)
sum(pre$Real_Score_Percentile>=.75&pre$Predicted_Score_Percentile<.75&pre$Predicted_Score_Percentile>=.5)
sum(pre$Real_Score_Percentile<.75&pre$Predicted_Score_Percentile<.75&pre$Predicted_Score_Percentile>=.5)
sum(pre$Real_Score_Percentile>=.75&pre$Predicted_Score_Percentile<.5)
sum(pre$Real_Score_Percentile<.75&pre$Predicted_Score_Percentile<.5)

(71+31)/(71+61+31+95)
1-((29)/(29+217))

####Banding####
band<-function(score,rel){
  sem<-sd(score,na.rm=T)*(sqrt(1-rel))
  sed<-sem*sqrt(2)*1.96
  print(paste('Standard Error of Measurement: ',sem))
  print(paste('Band Size: ',sed))
  s_list<-list('sem' = sem,'sed(band_size)' = sed)
  return(s_list)
}
ba<-band(pre$cv,0.89)

ban1<-between(pre$cv,ap-ba$`sed(band_size)`,ap)
ban2<-between(pre$cv,ap-(2*ba$`sed(band_size)`),ap-ba$`sed(band_size)`)
ban3<-between(pre$cv,ap-(3*ba$`sed(band_size)`),ap-2*(ba$`sed(band_size)`))
ban4<-between(pre$cv,ap-(4*ba$`sed(band_size)`),ap-3*(ba$`sed(band_size)`))
ban5<-between(pre$cv,ap-(5*ba$`sed(band_size)`),ap-4*(ba$`sed(band_size)`))
ban6<-between(pre$cv,ap-(6*ba$`sed(band_size)`),ap-5*(ba$`sed(band_size)`))
ban7<-between(pre$cv,ap-(7*ba$`sed(band_size)`),ap-6*(ba$`sed(band_size)`))
ban8<-between(pre$cv,ap-(8*ba$`sed(band_size)`),ap-7*(ba$`sed(band_size)`))
ban9<-between(pre$cv,ap-(9*ba$`sed(band_size)`),ap-8*(ba$`sed(band_size)`))
ban10<-between(pre$cv,ap-(10*ba$`sed(band_size)`),ap-9*(ba$`sed(band_size)`))

sum(ban1,ban2,ban3,ban4,ban5,ban6,ban7,ban8,ban9,ban10)

bann<-list(ban1,ban2,ban3,ban4,ban5,ban6,ban7,ban8,ban9,ban10)

cvband<-vector(length=504)
for(i in 1:length(bann)){
  cvband[which(bann[[i]])]<-i
}

pre$band<-cvband

hist(pre$band)

##Finding Fixed Band Starting Point
#W = 50, Integrity = 5,Boldness = 100,Int = 5

mm<-as.data.frame(matrix(ncol=4,nrow=1))
colnames(mm)<-c('Wonderlic','Boldness','IntegrityScale','Interview')
#Vector of Max scores for All Predictors in Z-Scores
mb<-c(2.811,6.018196,2.741905,2.389854)
r2

ap<-(-0.277)+(0.17553*2.811)+(0.23730*6.018)+(0.12583*2.741905)+(-0.06945*2.389854)

summary(da$Interview)
sd(da$Interview,na.rm=T)
(10-7.188)/1.176641

##Adverse Impact with Banding
nrow(pre[pre$band<7,])
nrow(pre[pre$band<7&pre$gender=='M',])
nrow(pre[pre$band<7&pre$gender=='F',])

preb<-pre[which(pre$band==7),]
preb<-preb[rev(order(preb$Wonderlic)),]
table(preb$gender[1:45])

head(pre)
gm<-ggplot(pre,aes(x=IntegrityScale,fill=gender))
gm+geom_bar(position='dodge')

mr<-(55+19)/(259)
fr<-(26+26)/(245)
fr/mr

####Regression Model 2####
pre2$boldness5<-pre2$Boldness*.5
da$boldness5<-pre2$boldness5
cdp2<-sample(1:nrow(pre2),size=round(nrow(pre2)*.75,0),replace=F)

ptrain2<-pre2[cdp2,]
ptest2<-pre2[-cdp2,]
utrain<-da[cdp2,]
utest<-da[-cdp2,]

reg3<-lm(critscore~Wonderlic+Manage.People+
           Strategic.Thinking+boldness5+IntegrityScale+Interview,data=ptrain2)
r3<-summary(reg3)
r3

reg4<-lm(critscore~Wonderlic+Interview+IntegrityScale+boldness5,data=ptrain2)
reg4u<-lm(critscore~Wonderlic+Interview+IntegrityScale+boldness5,data=utrain)
summary(reg4u)

r4<-summary(reg4)


ptest2$cv<-predict(reg4,ptest2)
tss<-sum((ptest2$critscore-mean(ptest2$critscore))**2)
rss<-sum((ptest2$critscore-ptest2$cv)**2)

#Cross Validated R-Squared
1-(rss/tss)

#Copy and Paste Models
write.excel(round(r3$coefficients,2),row.names=F,col.names=F)
write.excel(round(r4$coefficients,2),row.names=F,col.names=F)

#Significant difference between model 1 and 2
anova(reg2,reg4)
r4

##Analyze Adverse Impact for Model 2
pre$boldness5<-pre2$boldness5
pre$cv2<-predict(reg4,pre)
pre<-pre[rev(order(pre$cv2)),]

#Prediction outcome ratios lead to adverse impact
table(pre$gender[1:126])
66/246
60/260
table(pre$gender)

.23/.27

pre<-pre[order(pre$Boldness),]
da<-da[order(da$Boldness),]

pre$terminated<-da$Terminated
colnames(pre)
gs<-ggplot(pre,aes(x=cv,y=Real_Score,color=gender))
gs+geom_point()

####Expectancy Chart for Model 2####
pre<-pre[order(as.numeric(rownames(pre))),]
pre$cv2p<-percentile(pre$cv2)/100
pre<-na.omit(pre)
pre<-pre[order(pre$Real_Score_Percentile),]

gg<-ggplot(pre,aes(x=cv2p,y=Real_Score_Percentile))
gg+geom_count(color=ifelse(pre$cv2p[!duplicated(pre[,c(11,15)])]>=.75,'green',ifelse(pre$cv2p[!duplicated(pre[,c(11,15)])]>=.5&pre$cv2p[!duplicated(pre[,c(11,15)])]<.75,'blue','red')),show.legend=F)+
  scale_size_continuous(range=c(2,4.5))+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::percent)+
  geom_vline(xintercept=.75,linetype='dashed',color='black',size=1)+
  geom_vline(xintercept=.5,linetype='dashed',color='black',size=1)+
  geom_vline(xintercept=0,color='black',size=1)+
  geom_hline(yintercept=.75,linetype='dashed',color='black',size=1)+
  geom_hline(yintercept=0,color='black',size=1)+
  labs(x='Predicted Score Percentile',y='Actual Score Percentile')+
  theme(axis.title = element_text(size=15))+
  annotate(geom='text',x=.9,y=1,label='50',color='black',size=5,fontface=2)+
  annotate(geom='text',x=.9,y=.3,label='81',color='black',size=5,fontface=2)+
  annotate(geom='text',x=.63,y=1,label='33',color='black',size=5,fontface=2)+
  annotate(geom='text',x=.63,y=.3,label='93',color='black',size=5,fontface=2)+
  annotate(geom='text',x=.25,y=.3,label='199',color='black',size=5,fontface=2)+
  annotate(geom='text',x=.25,y=1,label='49',color='black',size=5,fontface=2)+
  annotate(geom='text',x=.89,y=-.1,label='TP = 38%, TN = 78%, SR = 0.25',color='black',size=3.45,fontface=2)+
  annotate(geom='text',x=.89,y=-.05,label='75th Percentile Cutoff',color='black',size=4,fontface=2)+
  annotate(geom='text',x=.63,y=-.1,label='TP = 32%, TN = 66%, SR = 0.50',color='black',size=3.45,fontface=2)+
  annotate(geom='text',x=.63,y=-.05,label='50th Percentile Cutoff',color='black',size=4,fontface=2)+
  annotate(geom='text',x=.20,y=-.05,label='No Cutoff',color='black',size=4,fontface=2)+
  annotate(geom='text',x=.20,y=-.1,label='TP = 25%, TN = 75%, SR = 1.00',color='black',size=3.45,fontface=2)

#Percentile Estimates
sum(pre$Real_Score_Percentile>=.75&pre$cv2p>=.75)
sum(pre$Real_Score_Percentile<.75&pre$cv2p>=.75)
sum(pre$Real_Score_Percentile>=.75&pre$cv2p<.75&pre$cv2p>=.5)
sum(pre$Real_Score_Percentile<.75&pre$cv2p<.75&pre$cv2p>=.5)
sum(pre$Real_Score_Percentile>=.75&pre$cv2p<.5)
sum(pre$Real_Score_Percentile<.75&pre$cv2p<.5)

50/(50+81)
1-((33+49)/(33+49+199+93))

(50+33)/(50+33+81+93)
1-(49/(49+99))

table(da$Division)
table(da$Level)


