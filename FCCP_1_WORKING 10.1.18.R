---
  title: "FCCP_1RMD"
author: "Christen Park"
date: "August 26, 2018"
output:
  pdf_document: default
word_document: default
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

##Variables to be used
PCMT: professional commitment (Q3a-c; at3a, at3b, at3cr) ; scale of 1-5
FCMT: FCC commitment (Q2; at2) ; scale of 1-4, categorical
PENG: profesional engagement (satisfaction, competence, emotional exhaustion) (Q5a-c; at5a-c) ; scale of 1-7
WORKSTRESS: workplace stress (Q3d-i; at3d, at3e, at3f, at3g, at3h, at3i) ; scale of 1-5

FAM: relationship with family (Q9a-f; cf9a, cf9b, cf9c, cf9d, cf9e, cf9f) ; scale of 1 - 4
CLOSENESS: relationship with children (closeness) (Q10a,c,e,f,g,i,o; cf10a, cf10c, cf10e, cf10f, cf10g, cf10i, cf10o), scale of 1-5
CONFLICT: relationship with children (conflict) (Q10b,d,h,j,k,l,m,n; cf10b, cf10d, cf10h, cf10j, cf10k, cf10l, cf10m, cf10n), scale of 1-5

CCNES.pos: positive reactions (n=9), scale of 1-7
CCNES.enc: encouraging emotional reactions (n=5), scale of 1-7
CCNES.neg: negative reactions (n=8)

IN.MOTIV: internal movitation (LIST ITEMS, Q1) ; binary, will use individual items 
EX.MOTIV: external motivation (LIST ITEMS, Q1) ; binary, will use individual items

#also explore the three-component model of workplace commitment mindset (meyer & herscovitz)############# (see notepad)


```{r load_packages, include = FALSE}
#set working directory
setwd('C:/Users/CP/Box Sync/Family Child Care Provider Data (L. Jeon, Johns Hopkins)/ANALYSIS')

#Load data
FCCP.data<-read.csv(file = "FCCP dataset.csv")

library(psych)
library(ggplot2)
library(sjPlot)
```

###new dataframes for each variable & item analysis

```{r, echo=FALSE, warning=FALSE, message=FALSE}
##Create dataframes for each variable
PCMT <-FCCP.data[c(20,21,23)]
PENG <-FCCP.data[c(42:44)]
FAM <-FCCP.data[c(81:86)]
CLOSENESS <-FCCP.data[c(87,89,91:93,95,101)] 
CONFLICT <-FCCP.data[c(88,90,94,96:100)]

CCNES <-FCCP.data[c(189:193,195:205,207:211)]
CCNES.pos<-FCCP.data[c(191,192,197,201,204,205,207,209)]
CCNES.neg<-FCCP.data[c(189,193,196,198,199,202,210,211)]
CCNES.enc<-FCCP.data[c(190,195,200,230,208)]

FCMT<-FCCP.data[c(19)]
INTMOTIV<-FCCP.data[c(7,10,13,14,15)] 
#7-enjoy, 10-child dvp, 13-implement own philosophy, 14-EC knowledge, 15-operate own program

EXTMOTIV<-FCCP.data[c(6,8,9,11,12,16,17)]
#6-job security, 8-social status, 9-work schedule, 11-income, 12-friend was seeking care, 16-childcare for own chidlren, 17-income while caring for own children

##Begin item analysis -- DONT CENTER OR SCALE IF I WANT TO MATCH ALPHAS FROM OG STUDY
sjt.itemanalysis(PCMT) #Cronbach's alpha=-0.659
sjt.itemanalysis(PENG) #Cronbach's alpha=-0.15; taking out competence(at5b) icnreases alpha to -0.811
#Running correlation for PENG
PENG.cor<-cor(PENG,use="pairwise.complete.obs") #defaults to pearson
library(corrplot)
corrplot(PENG.cor, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, method="number")
#low correlation between variables in PENG. Don't aggregate and use as separate items.

sjt.itemanalysis(FAM) #Cronbach's alpha=0.781

sjt.itemanalysis(CONFLICT) #Cronbach's alpha=0.694 (item cf10d deleted, alpha=.717)
#Creating new dataframe for conflict with item taken out
CONFLICT <-FCCP.data[c(88,94,96:100)] #new alpha=0.717
sjt.itemanalysis(CLOSENESS) #Cronbach's alpha=0.717 

sjt.itemanalysis(CCNES.pos)
sjt.itemanalysis(CCNES.neg)
sjt.itemanalysis(CCNES.enc)

```

```{r, echo=FALSE, warning=FALSE, message=FALSE}
##item analysis for FCC stress/ job stressors
WORKSTRESS <-FCCP.data[c(24:28,30)] #item suggesting stress as FCCP (Q3d-i)
STRESSORS <-FCCP.data[c(32:40)] #how stressful certain stressors are (Q4)
#Make sure to check stress mean. look at whether all on same scale or if one should be excluded.
sjt.itemanalysis(WORKSTRESS) #Cronbach's alpha=0.805. Stress aggregate looks good
sjt.itemanalysis(STRESSORS) #Cronbach's alpha=.826

#correlation of stress items (all, Q3d-i, Q4)
STRESS <-FCCP.data[c(24:28,30,32:40)] 
STRESS.cor<-cor(STRESS,use="pairwise.complete.obs") #defaults to pearson
corrplot(STRESS.cor, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, method="number")
sjt.itemanalysis(STRESS) #combined stress Cronbach's alpha=0.876

#Looking at different combinations of stress items
STRESS.FAM<-FCCP.data[c(24:28,30,39,40)] #stress or stressors based on FCC own family/home
sjt.itemanalysis(STRESS.FAM) #Cronbach's alpha=.845
STRESS.ADMIN<-FCCP.data[c(32,33,38)] #stress or stressors from administrative tasks
sjt.itemanalysis(STRESS.ADMIN) #Cronbach's alpha=.691
STRESS.FC<-FCCP.data[c(34:37)] #stress or stressors from family/chidlren in care
sjt.itemanalysis(STRESS.FC) #Cronbach's alpha=.723

##Decision on stress items
#Decided to use the composite of all stress measures. Has highest Cronbach's alpha.  

```

##Cleaning data
###Aggregating variables
PCMT and PENG not aggregated
FCMT not included (categorical)

```{r, echo=FALSE, warning=FALSE, message=FALSE}
FCCP.data$STRESS.agg<-rowMeans(FCCP.data[c(24:28,30,39,40)], na.rm=TRUE)
FCCP.data$FAM.agg<-rowMeans(FCCP.data[c(81:86)], na.rm=TRUE)
FCCP.data$CONFLICT.agg<-rowMeans(FCCP.data[c(88,90,94,96:100)], na.rm=TRUE)
FCCP.data$CLOSENESS.agg<-rowMeans(FCCP.data[c(87,89,91:93,95,101)], na.rm=TRUE)
FCCP.data$CCNES.enc.agg<-rowMeans(FCCP.data[c(190,195,200,230,208)], na.rm=TRUE)
FCCP.data$CCNES.neg.agg<-rowMeans(FCCP.data[c(189,193,196,198,199,202,210,211)], na.rm=TRUE)
FCCP.data$CCNES.pos.agg<-rowMeans(FCCP.data[c(191,192,197,201,204,205,207,209)], na.rm=TRUE)

#visualizing variables
library(car)
hist(FCCP.data$STRESS.agg)
hist(FCCP.data$FAM.agg)
hist(FCCP.data$CONFLICT.agg)
hist(FCCP.data$CLOSENESS.agg)
hist(FCCP.data$CCNES.enc.agg)
hist(FCCP.data$CCNES.neg.agg)
hist(FCCP.data$CCNES.pos.agg)  

##FCCP dataset_2 is csv with agg columns added
```

#FCCP Demographics
gender (316, ay40n) 1=female 
age (357, age2015) 
race (319, ay43) 1=American Indian/Alaska Native,
2=White/European American, 3=Asian/Native Hawaiian or Pacific Islander,
4=Multi-racial, 5=Black/African American (Race recoded, 320, race)
Hispanic/Latino origin (318, ay41n) 1=Yes
marital status (326, ay44) 1=Married/Civil Union, 2=Co-habiting/Living Together, 
3=Single/Never Married, 4=Separated, 5=Divorced, 6=Widowed (Marital status recoded, 327, ay44n)
educational attainment (108, tpd12) 1=Less than HS/no GED, 2=HS diploma/GED, 3=Some college
no degree, 4=AA/AS, 5=BA/BS, 6=Grad school/no degree, 7=MA/MS, 8=Beyond Masters
CD or ECE course beyond HS (110, tpd13n) 1=Yes
Another Paid jJob (334), 1=Yes)

```{r, echo=FALSE, warning=FALSE, message=FALSE}
#using original dataset for demographics
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
Demographics$MaritalStatus<-factor(Demographics$MaritalStatus, levels=c(1,2,3,4,5,6), labels=c("Married","Co-habiting","Single","Separated","Divorced","Widowed")) 
Demographics$Education<-factor(Demographics$Education, levels=c(1,2,3,4,5,6,7,8), labels=c("Less than hs","HS diploma/GED","Some college","AA/AS","BA/BS","Grad school","MA/MS","PhD/MD/JD/EdD"))

library(tableone)

tab1<-CreateTableOne(data=Demographics, factorVars=catVars)
tab1

```

#FCCP Program Information
HelpFam (248, freq in 250) family members working w FCCP, 1=Yes
HelpPay (251, freq in 253) paid caregivers working w FCCP, 1=Yes
Subsidize (275) total number of children receiving subsidized childcare funding

```{r, echo=FALSE, warning=FALSE, message=FALSE}
names(FCCP.data)[248] <-"HelpFam"
names(FCCP.data)[251] <-"HelpPay"
names(FCCP.data)[275] <-"Subsidize"

Program.Info <-FCCP.data[c(248,251,275)]
head(Program.Info)
describe(Program.Info)

tab2<-CreateTableOne(data=Program.Info)
tab2

```

##Beginning clustering.
start running regression to determine which variables to include
```{r, echo=FALSE, warning=FALSE, message=FALSE}
#loading data with some variables renamed
FCCP.data1<-read.csv(file = "FCCP dataset_2.csv")

```

Running correlation and hierarchical regression with variables may be related to professional commitment items (to be able to include them in cluster). To see which subscales of professional commitment may be driving the regression/clustering. 


```{r, echo=FALSE, warning=FALSE, message=FALSE}
CV <-FCCP.data1[c(15:20,79:85)]

CV.cor<-cor(CV,use="pairwise.complete.obs") 
corrplot(CV.cor, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, method="number")

#scaling variables for comparability
STRESS.agg.c <-scale(FCCP.data1$STRESS.agg, scale = FALSE)
SATISFACTION.c <-scale(FCCP.data1$SATISFACTION, scale = FALSE)
COMPETENCE.c <-scale(FCCP.data1$COMPETENCE, scale = FALSE)
EMOEXHAUST.c <-scale(FCCP.data1$EMOEXHAUST, scale = FALSE)
X3A_AGAIN.c <-scale(FCCP.data1$X3A_AGAIN, scale = FALSE)
X3B_12MO.c<-scale(FCCP.data1$X3B_12MO, scale = FALSE)
X3C_STR.c<-scale(FCCP.data1$X3C_STR, scale = FALSE)
FAM.agg.c<-scale(FCCP.data1$FAM.agg, scale = FALSE)
CONFLICT.agg.c<-scale(FCCP.data1$CONFLICT.agg, scale = FALSE)
CLOSENESS.agg.c<-scale(FCCP.data1$CLOSENESS.agg, scale = FALSE)
CCNES.enc.agg.c<-scale(FCCP.data1$CCNES.enc.agg, scale = FALSE)
CCNES.neg.agg.c<-scale(FCCP.data1$CCNES.neg.agg, scale = FALSE)
CCNES.pos.agg.c<-scale(FCCP.data1$CCNES.pos.agg, scale = FALSE)

library(MASS)
library(stargazer)
```

```{r, echo=FALSE, warning=FALSE, message=FALSE}
#Relationship models
Var.model1 <-lm(FAM.agg.c~SATISFACTION.c+COMPETENCE.c+EMOEXHAUST.c+X3A_AGAIN.c+X3B_12MO.c+X3C_STR.c+STRESS.agg.c, data=FCCP.data1)

Var.model2a <-lm(CONFLICT.agg.c~SATISFACTION.c+COMPETENCE.c+EMOEXHAUST.c+X3A_AGAIN.c+X3B_12MO.c+X3C_STR.c+STRESS.agg.c, data=FCCP.data1)

Var.model2b <-lm(CLOSENESS.agg.c~SATISFACTION.c+COMPETENCE.c+EMOEXHAUST.c+X3A_AGAIN.c+X3B_12MO.c+X3C_STR.c+STRESS.agg.c, data=FCCP.data1)

library(stargazer)
stargazer(Var.model1,Var.model2a,Var.model2b,type="text",
          intercept.bottom = FALSE, single.row = TRUE,
          star.cutoffs = c(.05,.01,.001), notes.append = FALSE,
          header = FALSE)

#assumption tests
##multicollinearity
vif(Var.model1) > 4 #ok
vif(Var.model2a) > 4 #ok
vif(Var.model2b) > 4 #ok

##homoscedacity
ncvTest(Var.model1) #ok 
ncvTest(Var.model2a) #ok
ncvTest(Var.model2b) #ok

```

```{r, echo=FALSE, warning=FALSE, message=FALSE}
#CCNES models
Var.model3a <-lm(CCNES.enc.agg.c~SATISFACTION.c+COMPETENCE.c+EMOEXHAUST.c+X3A_AGAIN.c+X3B_12MO.c+X3C_STR.c+STRESS.agg.c, data=FCCP.data1)

Var.model3b <-lm(CCNES.neg.agg.c~SATISFACTION.c+COMPETENCE.c+EMOEXHAUST.c+X3A_AGAIN.c+X3B_12MO.c+X3C_STR.c+STRESS.agg.c, data=FCCP.data1)

Var.model3c <-lm(CCNES.pos.agg.c~SATISFACTION.c+COMPETENCE.c+EMOEXHAUST.c+X3A_AGAIN.c+X3B_12MO.c+X3C_STR.c+STRESS.agg.c, data=FCCP.data1)

stargazer(Var.model3a,Var.model3b,Var.model3c, type="text",
          intercept.bottom = FALSE, single.row = TRUE,
          star.cutoffs = c(.05,.01,.001), notes.append = FALSE,
          header = FALSE)

#assumption tests
##multicollinearity
vif(Var.model3a) > 4 #ok
vif(Var.model3b) > 4 #ok
vif(Var.model3c) > 4 #ok

##homoscedacity
ncvTest(Var.model3a) #fails p = 0.4495422 
ncvTest(Var.model3b) #ok
ncvTest(Var.model3c) #ok
plot(Var.model3a)

```

```{r, echo=FALSE, warning=FALSE, message=FALSE}
##Box-Cox transformation for a single variable to corect for heterscedacity (in CCNES.enc.agg.c)
Box = boxcox(FCCP.data1$CCNES.enc.agg ~ 1,    # Transform CCNES.enc.agg as a single vector
             lambda = seq(-6,6,0.1))      # Try values -6 to 6 by 0.1
Cox = data.frame(Box$x, Box$y)            # Create a data frame with the results
Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] # Order the new data frame by decreasing y
Cox2[1,]                                  # Display the lambda with the greatest
#    log likelihood

#     Box.x     Box.y
# 83  2.2    -1393.43

lambda = Cox2[1, "Box.x"]                 # Extract that lambda
CCNES.enc.box = (FCCP.data1$CCNES.enc.agg ^ lambda - 1)/lambda   # Transform the original data

library(rcompanion)
plotNormalHistogram(CCNES.enc.box)

FCCP.data1$CCNES.enc.box<-CCNES.enc.box #add transformed data to dataframe
CCNES.enc.agg.cb<-scale(FCCP.data1$CCNES.enc.box, scale = FALSE) #scale new data

#re-run regression
Var.model3a2 <-lm(CCNES.enc.agg.cb~SATISFACTION.c+COMPETENCE.c+EMOEXHAUST.c+X3A_AGAIN.c+X3B_12MO.c+X3C_STR.c+STRESS.agg.c, data=FCCP.data1)
summary(Var.model3a2)
```

#missing data
```{r, echo=FALSE, warning=FALSE, message=FALSE}
library(mice)
library(VIM)

aggr(CV, col=c('navyblue','red'), numbers=TRUE,
     sortVars=TRUE, labels=names(data),cex.axis=.7,
     gap=3,
     ylab=c("histogram of missing data","Pattern"))
#less than 5% missing per variable

#Variables included in cluster analysis (PENG, PCMT, Stress)
#loading listwise deletion csv
FCCP.data2<-read.csv(file = "FCCP dataset_3.csv")

```

#Cluster Analysis (clustering cases)
Using hierarchical clustering methods
```{r, echo=FALSE, warning=FALSE, message=FALSE}

#Variables included in cluster analysis (PENG, PCMT, Stress)
ClustVars <-FCCP.data2[c(15,16,18:20,79)]
head(ClustVars)
ClustVars <-scale(ClustVars)

```

##Using the dist() function 
The dist() function is used to compute the Euclidean distance between observations. 
Observations are clustered using Ward's method.
```{r, echo=FALSE, warning=FALSE, message=FALSE}
##Dissimilarity matrix
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

##Using average instead of ward
res.hc2 <- hclust(d, method = "average" )
plot(res.hc2, cex = 0.6, hang = -1) #average doesn't make sense, use ward

```

##Using agnes() and diana() functions (both in cluster package)

Benefits of agnes: It yields the agglomerative coefficient (see agnes.object) which measures the amount of clustering structure found; Apart from the usual tree it also provides the banner, a novel graphical display (see plot.agnes).

```{r, echo=FALSE, warning=FALSE, message=FALSE}
#computing with agnes
library("cluster")
# Compute agnes()
res.agnes <- agnes(ClustVars, method = "ward")
# Agglomerative coefficient
res.agnes$ac
## [1] 0.9884888
#values closer to 1 suggest strong clustering structure

res.agnes2 <- agnes(ClustVars, method = "complete")
res.agnes2$ac
## [1] 0.94951 (not as good as ward)

res.agnes3 <- agnes(ClustVars, method = "average")
res.agnes3$ac
## [1] 0.923165 (not as good as ward)

res.agnes4 <- agnes(ClustVars, method = "single")
res.agnes4$ac
## [1] 0.8515801 (not as good as ward)

# Plot the tree using pltree()
pltree(res.agnes, cex = 0.6, hang = -1,
       main = "Dendrogram of agnes") 

#elbow method to determine optimal clusters
library(factoextra)
fviz_nbclust(ClustVars, FUN=hcut, method="wss") #optimal number not as clear
#average silhouette method to determind optimal clusters
fviz_nbclust(ClustVars, FUN=hcut, method="silhouette") #optimal number is 4

grp.ag3 <- cutree(res.agnes, k = 3)
# Number of members in each cluster
table(grp.ag3)

grp.ag4 <- cutree(res.agnes, k = 4)
table(grp.ag4)

grp.ag5 <- cutree(res.agnes, k = 5)
table(grp.ag5)
```

##Assigning cases to clusters 
Using agglomerative hierarhical clustering (agnes) assignment
```{r, echo=FALSE, warning=FALSE, message=FALSE}
ClustVars$cluster1 <- ClustVars$grp.ag3


```

```{r, echo=FALSE, warning=FALSE, message=FALSE}
# Compute diana()
res.diana <- diana(ClustVars)
# Plot the tree
pltree(res.diana, cex = 0.6, hang = -1,
       main = "Dendrogram of diana")
##seems to have 4-5 clusters

grp.di3 <- cutree(res.diana, k = 3)
# Number of members in each cluster
table(grp.di3)

grp.di4 <- cutree(res.diana, k = 4)
# Number of members in each cluster
table(grp.di4)

grp.di5 <- cutree(res.diana, k = 5)
# Number of members in each cluster
table(grp.di5)

```

Creating groups based on diana clustering
```{r, echo=FALSE, warning=FALSE, message=FALSE}
#grp.di4
#  1   2   3   4 
#592  37 228  15 

FCCP.data1.2$cluster1 <- ClustVars$grp.di4

```

```{r, echo=FALSE, warning=FALSE, message=FALSE}
#Kmeans clustering -- REDO
res.km <- eclust(ClustVars, "kmeans", nstart = 25)
pltree(res.km, cex = 0.6, hang = -1,
       main = "Dendrogram of k-means")

```

###Visualizing clusters
Using the function fviz_cluster() [in factoextra], we can also visualize the result in a scatter plot. Observations are represented by
points in the plot, using principal components. A frame is drawn around each cluster.
```{r, echo=FALSE, warning=FALSE, message=FALSE}
fviz_cluster(list(data = ClustVars, cluster = grp)) #hierarchical w/ ward

fviz_cluster(list(data = ClustVars, cluster = grp.ag3)) 

fviz_cluster(list(data = ClustVars, cluster = grp.ag4)) 

fviz_cluster(list(data = ClustVars, cluster = grp.ag5)) 

fviz_cluster(list(data = ClustVars, cluster = grp.di3)) 

fviz_cluster(list(data = ClustVars, cluster = grp.di4)) #diana4, looks best

fviz_cluster(list(data = ClustVars, cluster = grp.di5)) 

```

###Comparing Clustering methods
```{r, echo=FALSE, warning=FALSE, message=FALSE}

##need to edit this code to fit data (https://www.r-bloggers.com/how-to-perform-hierarchical-clustering-using-r/)
library(dendextend)

hc_single <- agnes(ClustVars, method = "single")
hc_complete <- agnes(ClustVars, method = "complete")

# converting to dendogram objects as dendextend works with dendogram objects 
hc_single <- as.dendrogram(hc_single)
hc_complete <- as.dendrogram(hc_complete)

tanglegram(hc_single,hc_complete)

```

#try transforming data
```{r, echo=FALSE, warning=FALSE, message=FALSE}
FCCP.data_2<-read.csv(file = "FCCP dataset_2.csv")

```

```{r, echo=FALSE, warning=FALSE, message=FALSE}


```

```{r, echo=FALSE, warning=FALSE, message=FALSE}


```

```{r, echo=FALSE, warning=FALSE, message=FALSE}


```

```{r, echo=FALSE, warning=FALSE, message=FALSE}


```
Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
