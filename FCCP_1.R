#set working directory
setwd('C:/Users/CP/Box Sync/Family Child Care Provider Data (L. Jeon, Johns Hopkins)/ANALYSIS')

#Load data
FCCP.data<-read.csv(file = "FCCP dataset.csv")

##Variables listed##
#PCMT: professional commitment (Q3a-c; at3a, at3b, at3cr) ; scale of 1-5
#FCMT: FCC commitment (Q2; at2) ; scale of 1-4, categorical
#PENG: profesional engagement (satisfaction, competence, emotional exhaustion) (Q5a-c; at5a-c) ; scale of 1-7
#WORKSTRESS: workplace stress (Q3d-i; at3d, at3e, at3f, at3g, at3h, at3i) ; scale of 1-5

#FAM: relationship with family (Q9a-f; cf9a, cf9b, cf9c, cf9d, cf9e, cf9f) ; scale of 1 - 4
#CLOSENESS: relationship with children (closeness) (Q10a,c,e,f,g,i,o; cf10a, cf10c, cf10e, cf10f, cf10g, cf10i, cf10o), scale of 1-5
#CONFLICT: relationship with children (conflict) (Q10b,d,h,j,k,l,m,n; cf10b, cf10d, cf10h, cf10j, cf10k, cf10l, cf10m, cf10n), scale of 1-5

#CCNES.P: positive reactions (n=9), scale of 1-7
#CCNES.E: encouraging emotional reactions (n=5), scale of 1-7
#CCNES.N: negative reactions (n=8)

#IN.MOTIV: internal movitation (LIST ITEMS, Q1) ; binary. will use sum
#EX.MOTIV: external motivation (LIST ITEMS, Q1) ; binary. will use sum


###############################
##Item analysis for variables##
###############################
library(psych)
library(ggplot2)
library(sjPlot)

##Create dataframes for each variable
PCMT <-FCCP.data[c(20,21,23)]
PENG <-FCCP.data[c(42:44)]
WORKSTRESS <-FCCP.data[c(24:28,30)] #item suggesting stress as FCCP
STRESSORS <-FCCP.data[c(32:40)] #how stressful certain stressors are
FAM <-FCCP.data[c(81:86)]
CLOSENESS <-FCCP.data[c(87,89,91:93,95,101)] 
CONFLICT <-FCCP.data[c(88,90,94,96:100)]

CCNES <-FCCP.data[c(189:193,195:205,207:211)]
CCNES.pos<-FCCP.data[c(191,192,197,201,204,205,207,209)]
CCNES.neg<-FCCP.data[c(189,193,196,198,199,202,210,211)]
CCNES.enc<-FCCP.data[c(190,195,200,230,208)]

FCMT<-FCCP.data[c(19)]
INTMOTIV<-FCCP.data[c(7,10,13,14,15)] 
INTMOTIV.S<-rowSums(INTMOTIV)
#7-enjoy, 10-child dvp, 13-implement own philosophy, 14-EC knowledge, 15-operate own program

EXTMOTIV<-FCCP.data[c(6,8,9,11,12,16,17)]
EXTMOTIV.S<-rowSums(EXTMOTIV)
#6-job security, 8-social status, 9-work schedule, 11-income, 12-friend was seeking care, 16-childcare for own chidlren, 17-income while caring for own children

##Begin item analysis -- DONT CENTER OR SCALE IF I WANT TO MATCH ALPHAS FROM OG STUDY
sjt.itemanalysis(PCMT) #Cronbach's alpha=-0.659
sjt.itemanalysis(PENG) #Cronbach's alpha=-0.15; taking out competence(at5b) icnreases alpha to -0.811
#Running correlation for zPENG
PENG.cor<-cor(PENG,use="pairwise.complete.obs") #defaults to pearson
library(corrplot)
corrplot(PENG.cor, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, method="number")
#low correlation between variables in PENG. Don't aggregate and use as separate items.

#Make sure to check stress mean. look at whether all on same scale or if one should be excluded.
sjt.itemanalysis(WORKSTRESS) #Cronbach's alpha=0.805. Stress aggregate looks good
sjt.itemanalysis(STRESSORS) #Cronbach's alpha=0.826
sjt.itemanalysis(FCCP.data[c(24:28,30,32:40)]) #combined stress Cronbach's alpha=0.876
#Creating new dataframe for combined stress items (Q3d-i, Q4)
STRESS <-FCCP.data[c(24:28,30,32:40)] 
zSTRESS<-scale(STRESS, center = TRUE, scale = TRUE)
sjt.itemanalysis(STRESS)

sjt.itemanalysis(FAM) #Cronbach's alpha=0.781

sjt.itemanalysis(CONFLICT) #Cronbach's alpha=0.694 (item cf10d deleted, alpha=.717)
#Creating new dataframe for conflict with item taken out
CONFLICT <-FCCP.data[c(88,94,96:100)] #new alpha=0.717
zCONFLICT<-scale(CONFLICT, center = TRUE, scale = TRUE)
sjt.itemanalysis(CLOSENESS) #Cronbach's alpha=0.717 

sjt.itemanalysis(CCNES.pos)
sjt.itemanalysis(CCNES.neg)
sjt.itemanalysis(CCNES.enc)

################# 
##CLEANING DATA## 
################# 
##Aggregating variables
#PCMT not aggregated
#PENG not aggregated 
STRESS$STRESS.agg <-rowMeans(STRESS, na.rm=TRUE)
FAM$FAM.agg <-rowMeans(FAM, na.rm=TRUE)
CONFLICT$CONFLICT.agg <-rowMeans(CONFLICT, na.rm=TRUE)
CLOSENESS$CLOSENESS.agg <-rowMeans(CLOSENESS, na.rm=TRUE)
CCNES.enc$CCNES.enc.agg <-rowMeans(CCNES.enc, na.rm=TRUE)
CCNES.neg$CCNES.neg.agg <-rowMeans(CCNES.neg, na.rm=TRUE)
CCNES.pos$CCNES.pos.agg <-rowMeans(CCNES.pos, na.rm=TRUE)

##combine dataframes (only using part of dataset)
FCCP.data1<-cbind(PCMT, PENG, CCNES.pos,CCNES.neg,CCNES.enc,CLOSENESS,CONFLICT,EXTMOTIV,INTMOTIV,FAM,FCMT,PCMT,PENG,STRESS)
head(FCCP.data1)

##Taking out missing data (as 'NA' in dataset)
FCCP.data2 <- na.omit(FCCP.data1)
head(FCCP.data2)

##Z-scoring variabes----- check on this (make sure it is based on FCCP.data2)
zPCMT<-scale(PCMT, center = TRUE, scale = TRUE)
zPENG<-scale(PENG, center = TRUE, scale = TRUE)
zWORKSTRESS<-scale(WORKSTRESS, center = TRUE, scale = TRUE)
zSTRESSORS<-scale(STRESSORS, center=TRUE, scale=TRUE)
zFAM<-scale(FAM, center = TRUE, scale = TRUE)
zCONFLICT<-scale(CONFLICT, center = TRUE, scale = TRUE)
zCLOSENESS<-scale(CLOSENESS, center = TRUE, scale = TRUE)
zCCNES<-scale(CCNES, center = TRUE, scale = TRUE) 
zCCNES.pos<-scale(CCNES.pos, center = TRUE, scale = TRUE)
zCCNES.neg<-scale(CCNES.neg, center = TRUE, scale = TRUE)
zCCNES.enc<-scale(CCNES.enc, center = TRUE, scale = TRUE)

#visualizing variables
library(car)
hist(FCCP.data2$STRESS.agg)
hist(FCCP.data2$FAM.agg)
hist(FCCP.data2$CONFLICT.agg)
hist(FCCP.data2$CLOSENESS.agg)
hist(FCCP.data2$CCNES.enc.agg)
hist(FCCP.data2$CCNES.neg.agg)
hist(FCCP.data2$CCNES.pos.agg)   
                                              
#####################
##FCCP DEMOGRAPHICS##
#####################
library(tableone)
#gender (316, ay40n) 1=female 
#age (357, age2015) 
#race (319, ay43) 1=American Indian/Alaska Native,
#2=White/European American, 3=Asian/Native Hawaiian or Pacific Islander,
#4=Multi-racial, 5=Black/African American (Race recoded, 320, race)
#Hispanic/Latino origin (318, ay41n) 1=Yes
#marital status (326, ay44) 1=Married/Civil Union, 2=Co-habiting/Living Together, 
#3=Single/Never Married, 4=Separated, 5=Divorced, 6=Widowed (Marital status recoded, 327, ay44n)
#educational attainment (108, tpd12) 1=Less than HS/no GED, 2=HS diploma/GED, 3=Some college
#no degree, 4=AA/AS, 5=BA/BS, 6=Grad school/no degree, 7=MA/MS, 8=Beyond Masters
#CD or ECE course beyond HS (110, tpd13n) 1=Yes
#Another Paid jJob (334), 1=Yes)

names(FCCP.data)[316] <-"Female"
names(FCCP.data)[357] <-"Age"
names(FCCP.data)[319] <-"Race"
names(FCCP.data)[318] <-"Hispanic"
names(FCCP.data)[325] <-"MaritalStatus"
names(FCCP.data)[108] <-"Education"
names(FCCP.data)[110] <-"ECE"
names(FCCP.data)[334] <-"PaidJob"
names(FCCP.data)[335] <-"HouseholdIncome" #categorical
names(FCCP.data)[336] <-"FCCPSalary" #categorical

Demographics <-FCCP.data[c(316,357,319,318,325,108,110,334,335,336)]
head(Demographics)
catVars<-c("Race", "MaritalStatus", "Education")
describe(Demographics) #Data exploration of Demographics

#labeling categorical variable scales
Demographics$Race<-factor(Demographics$Race, levels=c(1,2,3,4,5), 
                        labels=c("American Indian/Alaska Native","White","Asian/Pacific Islander",
                                 "Multi-Racial","Black/African-American"))
Demographics$MaritalStatus<-factor(Demographics$MaritalStatus, levels=c(1,2,3,4,5,6), labels=c("Married","Co-habiting",
                                                                        "Single","Separated","Divorced",
                                                                        "Widowed")) 
Demographics$Education<-factor(Demographics$Education, levels=c(1,2,3,4,5,6,7,8), labels=c("Less than hs","HS diploma/GED","Some college","AA/AS","BA/BS","Grad school","MA/MS","PhD/MD/JD/EdD"))

tab1<-CreateTableOne(data=Demographics, factorVars=catVars)
tab1

####################
##FCC PROGRAM INFO##
####################
#HelpFam (248, freq in 250) family members working w FCCP, 1=Yes
#HelpPay (251, freq in 253) paid caregivers working w FCCP, 1=Yes
#Subsidize (275) total number of children receiving subsidized childcare funding

names(FCCP.data)[248] <-"HelpFam"
names(FCCP.data)[251] <-"HelpPay"
names(FCCP.data)[275] <-"Subsidize"

Program.Info <-FCCP.data[c(248,251,275)]
head(Program.Info)
describe(Program.Info)

tab2<-CreateTableOne(data=Program.Info)
tab2

#################### 
##CLUSTER ANALYSIS## 
#################### 
#Clustering variables 
library(ClustOfVar)
#using PCMT, PENG, STRESS.agg, FAM.agg, CONFLICT.agg, CLOSENESS.agg,
#CCNES.enc.agg, CCNES.neg.agg, CCNES.pos.agg in FCCP.data2

xquant <- FCCP.data2[,c(15,24,30,38,46,65,88)] # Numeric variables
str(xquant)
xqual <- FCCP.data2[,c(1:6)]
str(xqual)

tree <- hclustvar(xquant) ##xqual options are not categorical (check on these)
plot(tree)

##Clustering cases##
ClustVars<- FCCP.data2[,c(15,24,30,38,46,65,88)]

#Need to scale so that they are comparable
FCCP.data2 <- scale(FCCP.data2)
head(FCCP.data2)

##The dist() function is used to compute the Euclidean distance between
##observations. Finally, observations are clustered using Ward's method.
#Dissimilarity matrix
d <- dist(ClustVars, method = "euclidean")
# Hierarchical clustering using Ward's method
res.hc <- hclust(d, method = "ward.D2" )
# Plot the obtained dendrogram
plot(res.hc, cex = 0.6, hang = -1)

#The function cutree() is used and it returns a vector containing the cluster
#number of each observation
#Cut tree into 4 groups
grp <- cutree(res.hc, k = 5)
# Number of members in each cluster
table(grp)


##Using agnes() and diana() functions (both in cluster package)
#computing with agnes
library("cluster")
# Compute agnes()
res.agnes <- agnes(ClustVars, method = "ward")
# Agglomerative coefficient
res.agnes$ac
## [1] 0.9755288
# Plot the tree using pltree()
pltree(res.agnes, cex = 0.6, hang = -1,
       main = "Dendrogram of agnes") 

# Compute diana()
res.diana <- diana(ClustVars)
# Plot the tree
pltree(res.diana, cex = 0.6, hang = -1,
       main = "Dendrogram of diana")
##seems to have 4 clusters

#vizualizing clusters #Using the function fviz_cluster() [in factoextra], we can
#also visualize the result in a scatter plot. Observations are represented by
#points in the plot, using principal components. A frame is drawn around each
#cluster.
library(factoextra)
fviz_cluster(list(data = ClustVars, cluster = grp)) ##NOT WORKING ERROR MESSAGE

##################### 
##GROUP DIFFERENCES## 
##################### 
##Group differences in workplace stress

##Group differences in family relationship

##Group differences in child relationship (conflict and closeness)

##Group differences in CCNES indicators (pos, neg, enc)