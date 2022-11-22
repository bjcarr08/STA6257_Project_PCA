library(dplyr)
library(ggplot2)
library(data.table)
library(ggfortify)
library(tidyr)
library(paletteer)

#library(AER); data(CASchools); View(CASchools)
#library(MASS); data(bacteria); View(bacteria)

### subset using only scale/factor variables -- group PCA analysis by flatearth scaled groups
# rename headers
# remove missing/NA values
# order/level factors
# standardize all vars
# scatter plot first 2 PCs color points based off flatearth group
# analyze loadings between groups

DKU<- read.table("https://raw.githubusercontent.com/bjcarr08/sampleData/main/CCES18_DKU_OUTPUT_vv.tab", header=T, sep="\t")

keepHeaders<- c("DKU397","educ","race","marstat","votereg","region","CC18_301","CC18_302","CC18_316","CC18_335","ownhome","immstat","employ","urbancity","union","internethome","internetwork","sexuality","trans","religpew","DKU312","DKU313","DKU387","DKU391","birthyr","CC18_308a","CC18_308b","CC18_308c","CC18_308d","CC18_334A","CC18_334D","CC18_334E","CC18_334F","pid3","pid7","pew_religimp","pew_churatd","pew_prayer","ideo5","newsint","faminc_new","DKU305","DKU306","DKU307","DKU308","DKU309","DKU310","DKU311","DKU373","DKU374","DKU375","DKU376","DKU377","DKU378","DKU379","DKU380","DKU398")
DKU2<- DKU[,names(DKU) %in% keepHeaders]

# new header names
names(DKU2)<- c("BirthYear","Education","Race","MaritalStatus","VoterRegistrationStatus","Region","NationalEconomy","PastYearHouseholdIncome","JobApproval_Trump","JobApproval_Congress","JobApproval_SupremeCourt","JobApproval_StateGovernor","Vote2016","IdeologicalPlacement_Yourself","IdeologicalPlacement_DemocraticParty","IdeologicalPlacement_RepublicanParty","IdeologicalPlacement_SupremeCourt","TrumpRussiaCollusion","3PointPartyID","7PointPartyID","HomeOwnership","ImmigrationBackground","EmploymentStatus","TypeOfAreaLivingIn","LaborUnionMember","InternetAccess_Home","InternetAccess_work","SexualOrientation","Transgender","ImportanceOfReligion","ChurchAttendance","FrequencyOfPrayer","Religion","Ideology","PoliticalInterest","FamilyIncome","IssueImportance_Abortion","IssueImportance_DefenseSpending","IssueImportance_GunControl","IssueImportance_Healthcare","IssueImportance_Immigration","IssueImportance_InternationalTrade","IssueImportance_PersonalIncomeTaxes","MostImportant","LeastImportant","DemTrait_Intelligent","DemTrait_OpenMinded","DemTrait_Hardworking","DemTrait_Honest","RepTrait_Intelligent","RepTrait_OpenMinded","RepTrait_Hardworking","RepTrait_Honest","JuvenileCrimeAge","GunOwnership","FlatEarth","PizzaGate")

DescTools::Desc(DKU2)[[1]]$abstract

DKU2$BirthYear<- 2000-DKU2$BirthYear+18

names(DKU2)[1]<- "Age"

DKU2$FlatEarth2<- ifelse(DKU2$FlatEarth<3, "No", ifelse(DKU2$FlatEarth==8, 8, "Yes"))


DKU3<- DKU2 %>% filter(
  FlatEarth!=8, 
  Ideology<6, 
  #PoliticalInterest<7, 
  #FamilyIncome<17, 
  NationalEconomy<6,
  #PastYearHouseholdIncome<8,
  JobApproval_Trump<5
  #JobApproval_StateGovernor<5,
  #IdeologicalPlacement_Yourself<8
)

df<- Affairs[,-c(1,2,5,10)]
autoplot(prcomp(df, scale.=T), data=Affairs, colour="affairs2", 
         loadings=T, loadings.label=T, loadings.colour="darkgray", loadings.label.colour="black")

df<- Affairs[,c(7,9,30,31,32,34,35,c(37:43))]
autoplot(prcomp(df, scale.=T), data=DKU3, colour="FlatEarth2", 
         loadings=T, loadings.label=T, loadings.colour="darkgray", loadings.label.colour="black")

#Flat earth

#Count   Code   Label
#-----   ----   -----
#851      1      Definitely  not true
#76       2      Probably  not true
#53       3      Probably  true
#17       4      Definitely  true
#3        8      skipped





################################################################################################
################################################################################################



# READ & VIEW DATA
conspiracy<- (read.csv("https://raw.githubusercontent.com/bjcarr08/sampleData/main/kaggleConspiracyTheoriesData.csv", stringsAsFactors = T))[,-1]
View(conspiracy)
DescTools::Desc(conspiracy)[[1]]$abstract


# REMOVE ROWS WITH NAs
conspiracy<- conspiracy[complete.cases(conspiracy),]


# STANDARDIZED VARIABLES
conspiracy.Stdz<- conspiracy %>% mutate(across(.cols=truther911:vaportrail, scale))


# LONG DATA
conspiracy.Long<- conspiracy %>% pivot_longer(!y, names_to="conspiracy", values_to="score", values_transform=list(score=as.numeric))
conspiracy.Stdz.Long<- conspiracy.Stdz %>% pivot_longer(!y, names_to="conspiracy", values_to="stdzScore", values_transform=list(stdzScore=as.numeric))


# FREQUENCY HISTOGRAMS
ggplot(conspiracy.Long, aes(score, fill=conspiracy, color=conspiracy)) +
  geom_histogram(alpha=0.2, breaks=seq(0,5,1)) +
  lemon::facet_rep_wrap(.~conspiracy, nrow=4, labeller="label_both", repeat.tick.labels=T) +
  labs(title="Raw Scores") +
  theme_bw() +
  theme(legend.position="none")

ggplot(conspiracy.Stdz.Long, aes(stdzScore, fill=conspiracy, color=conspiracy)) +
  geom_histogram(alpha=0.2, breaks=seq(-2,3,1)) +
  lemon::facet_rep_wrap(.~conspiracy, nrow=4, labeller="label_both", repeat.tick.labels=T) +
  labs(title="Standardized Scores") +
  theme_bw() +
  theme(legend.position="none")



#summary(pc.cr <- princomp(conspiracy.Stdz[,-9])) # 1st 2 PCs account for 63% of variance
#loadings(pc.cr)  # blank cells are small, not zero
#biplot(pc.cr, pc.biplot=T, cex=0.6, xpd=T, main="Biplot")
#screeplot(pc.cr, type="lines", col="blueviolet", main="Screeplot")


conspiracy<- conspiracy %>% filter(y!="Not Sure") # removed rows where participant marked 'not sure' as political party

# Re LEVEL Political Ideology
conspiracy$y<- factor(conspiracy$y, levels=c("Very Liberal", "Liberal", "Somewhat Liberal", "Middle of the Road", "Somewhat Conservative", "Conservative", "Very Conservative"))

Liberal<- c("Very Liberal", "Liberal", "Somewhat Liberal")
Conservative<- c("Somewhat Conservative", "Conservative", "Very Conservative")

conspiracy$y2<- ifelse(conspiracy$y %in% Liberal, "Liberal", ifelse(conspiracy$y %in% Conservative, "Conservative", "Middle"))

df<- conspiracy[,-c(9,10)]
autoplot(prcomp(df, scale.=T), 
         data=conspiracy, colour="y2", frame=T, frame.colour = "y2",
         loadings=T, loadings.label=T, loadings.colour="black", loadings.label.colour="black") + 
  scale_colour_manual(values = paletteer_d("rcartocolor::Temps")[c(7,4,2)]) +
  scale_fill_manual(values = paletteer_d("rcartocolor::Temps")[c(7,4,2)])




autoplot(prcomp(df, scale.=T), 
         data=A, colour="y2", frame=T, frame.colour = "y2",
         loadings=T, loadings.label=T, loadings.colour="black", loadings.label.colour="black") + 
  scale_colour_manual(values = paletteer_d("rcartocolor::Temps")[c(7,4,2)]) +
  scale_fill_manual(values = paletteer_d("rcartocolor::Temps")[c(7,4,2)])




prC<- prcomp(df, scale.=T, retx=F)
prC$rotation


(princomp(scale(df)))$loadings


yLabs<- c("Very Liberal", "Liberal", "Somewhat Liberal", "Middle of the Road", "Somewhat Conservative", "Conservative", "Very Conservative")

conspiracy$y<- factor(conspiracy$y, ordered=T, levels=yLabs)

#conspiracy %>% count(y)

pca<- princomp(conspiracy[,-9])
PC1<- pca$scores[,1]
PC2<- pca$scores[,2]
PCV1<- pca$loadings[,1]
PCV2<- pca$loadings[,2]
politicalView<- conspiracy[,9]
PCVlabs<- row.names(pca$loadings)

require(grid)

ggplot() +
  geom_point(aes(x=PC1, y=PC2, fill=politicalView), colour='black', pch=21, size=2.5, alpha=0.3) + 
  scale_fill_manual(values=c("navy", "blue", "lightblue1", "white", "indianred1", "red", "red4")) +   
  coord_fixed(ratio=1) +
  geom_segment(aes(x=0, y=0, xend=PCV1*12, yend=PCV2*10), arrow=arrow(length=unit(1/2, 'picas')), color="black") + 
  geom_text(aes(x=PCV1*14, y=PCV2*10), label=PCVlabs, size=4) 


summary(pca)
pca$loadings


################################################################################################
################################################################################################


######### NEW (better) Dataset 
######### CCES 2018 DKU


#### upload file to github instead of reading from local file

DKU<- read.table("C:/Users/b/Downloads/CCES18_DKU_OUTPUT_vv.tab", header=T, sep="\t")

#DKU %>% count(caseid, sort=T) %>% filter(n>1) # check for duplicate respondees

prop.table(ftable(DKU$DKU397, DKU$gender), 2)
prop.table(ftable(DKU$DKU397, DKU$race), 2)

DKU$race2<- ifelse(DKU$race==1, "White", "Not White")
DKU$gender2<- ifelse(DKU$gender==1, "Male", "Female")
DKU$flatEarth<- ifelse(DKU$DKU397==1, "False", 
                       ifelse(DKU$DKU397==2, "Probably False",
                              ifelse(DKU$DKU397==3, "Probably True",
                                     ifelse(DKU$DKU397==4, "True", "Skipped"))))


round(100*(prop.table(ftable(DKU$flatEarth[DKU$flatEarth!="Skipped"], DKU$race2[DKU$flatEarth!="Skipped"]), 2)), 0)

round(100*(prop.table(ftable(DKU$CC18_321f, DKU$gender2), 2)), 0)


DKU %>% count(race)

DKU %>% count(CC18_335)

rowSums(((DKU %>% count(CC18_300d_1,
                        CC18_300d_2,
                        CC18_300d_3,
                        CC18_300d_4,
                        CC18_300d_5))-1)[,-6])


## reduce tax rate for households earning >500k
CC18_325f_new == 1 (support)


### subset using only scale/factor variables -- group PCA analysis by flatearth scaled groups
# rename headers
# remove missing/NA values
# order/level factors
# standardize all vars
# scatter plot first 2 PCs color points based off flatearth group
# analyze loadings between groups

DKU<- read.table("https://raw.githubusercontent.com/bjcarr08/sampleData/main/CCES18_DKU_OUTPUT_vv.tab", header=T, sep="\t")

keepHeaders<- c("DKU397","educ","race","marstat","votereg","region","CC18_301","CC18_302","CC18_316","CC18_335","ownhome","immstat","employ","urbancity","union","internethome","internetwork","sexuality","trans","religpew","DKU312","DKU313","DKU387","DKU391","birthyr","CC18_308a","CC18_308b","CC18_308c","CC18_308d","CC18_334A","CC18_334D","CC18_334E","CC18_334F","pid3","pid7","pew_religimp","pew_churatd","pew_prayer","ideo5","newsint","faminc_new","DKU305","DKU306","DKU307","DKU308","DKU309","DKU310","DKU311","DKU373","DKU374","DKU375","DKU376","DKU377","DKU378","DKU379","DKU380","DKU398")
DKU2<- DKU[,names(DKU) %in% keepHeaders]

# new header names
names(DKU2)<- c("BirthYear","Education","Race","MaritalStatus","VoterRegistrationStatus","Region","NationalEconomy","PastYearHouseholdIncome","JobApproval_Trump","JobApproval_Congress","JobApproval_SupremeCourt","JobApproval_StateGovernor","Vote2016","IdeologicalPlacement_Yourself","IdeologicalPlacement_DemocraticParty","IdeologicalPlacement_RepublicanParty","IdeologicalPlacement_SupremeCourt","TrumpRussiaCollusion","3PointPartyID","7PointPartyID","HomeOwnership","ImmigrationBackground","EmploymentStatus","TypeOfAreaLivingIn","LaborUnionMember","InternetAccess_Home","InternetAccess_work","SexualOrientation","Transgender","ImportanceOfReligion","ChurchAttendance","FrequencyOfPrayer","Religion","Ideology","PoliticalInterest","FamilyIncome","IssueImportance_Abortion","IssueImportance_DefenseSpending","IssueImportance_GunControl","IssueImportance_Healthcare","IssueImportance_Immigration","IssueImportance_InternationalTrade","IssueImportance_PersonalIncomeTaxes","MostImportant","LeastImportant","DemTrait_Intelligent","DemTrait_OpenMinded","DemTrait_Hardworking","DemTrait_Honest","RepTrait_Intelligent","RepTrait_OpenMinded","RepTrait_Hardworking","RepTrait_Honest","JuvenileCrimeAge","GunOwnership","FlatEarth","PizzaGate")

DescTools::Desc(DKU2)[[1]]$abstract

DKU2$BirthYear<- 2000-DKU2$BirthYear+18

names(DKU2)[1]<- "Age"

DKU2$FlatEarth2<- ifelse(DKU2$FlatEarth<3, "No", ifelse(DKU2$FlatEarth==8, 8, "Yes"))

library(ggfortify)

df<- (DKU2 %>% filter(FlatEarth!=8))[,c(1,2,3,4,14,21,22,23,24,25,26,28,29,30,31,34,c(37:43),55,57)]
autoplot(prcomp(df, scale.=T), 
         data=DKU2 %>% filter(FlatEarth!=8), 
         colour="FlatEarth2", loadings=T, loadings.label=T, loadings.colour="darkgray", loadings.label.colour="black")

#Flat earth

#Count   Code   Label
#-----   ----   -----
#851      1      Definitely  not true
#76       2      Probably  not true
#53       3      Probably  true
#17       4      Definitely  true
#3        8      skipped





################################################################################################
################################################################################################

# UNUSED CODE
# RE-CODE y AS INT
# conspiracy.Int<- conspiracy %>% 
#                    filter(!(y %like% "Other|Sure")) %>% 
#                    mutate(y = ifelse(y=="Very Liberal", 0, 
#                                      ifelse(y=="Liberal", 1,
#                                             ifelse(y=="Somewhat Liberal", 2, 
#                                                    ifelse(y=="Middle of the Road", 3, 
#                                                           ifelse(y=="Somewhat Conservative", 4, 
#                                                                  ifelse(y=="Conservative", 5, 6)))))))










#########################



# packages
library(dplyr)
library(ggplot2)
library(data.table)
library(ggfortify)
library(MASS)
library(tidyr)
library(paletteer)
library(knitr)

# READ DATA FROM A GITHUB CSV FILE
conspiracy<- (read.csv("https://raw.githubusercontent.com/bjcarr08/sampleData/main/kaggleConspiracyTheoriesData.csv", stringsAsFactors = T))[,-1]

# VARIABLES
str(conspiracy)

# REMOVE ROWS WITH NAs & IMPOSSIBLE VALUES (removed rows where participant marked 'not sure' as political ideology)
conspiracy<- conspiracy[complete.cases(conspiracy),] %>% filter(y!="Not Sure")

# STANDARDIZED
cStdz<- conspiracy %>% mutate(across(.cols=truther911:vaportrail, scale))

# NORMALIZED 
cNorm<- BBmisc::normalize(conspiracy[,-9], method="range", range=c(0, 1))
cNorm<- cbind(cNorm, conspiracy$y)


# SCATTER PLOT MATRICES: TO CHECK LINEARITY ASSUMPTION
pairs(sapply(conspiracy[,-9], function(x) jitter(x, 5)), col=alpha("#8B814C", 0.1), cex.labels="#8B814C")

pairs(sapply(cStdz[,-9], function(x) jitter(x, 5)), col=alpha("#8B814C", 0.1))
pairs(sapply(cNorm[,-9], function(x) jitter(x, 5)), col=alpha("#8B814C", 0.1))


DescTools::PlotPairs(sapply(conspiracy[,-9], function(x) jitter(x, 5)), 
                     g=conspiracy$y, 
                     col=c(alpha(hred, 0.1), alpha(hblue, 0.1), alpha(hgreen, 0.1)), 
                     col.smooth=c("black", hred, hblue, hgreen),
                     cex.labels="#8B814C")

par(col.axis="#8B814C",col.lab="#8B814C",col.main="#8B814C",col.sub="#8B814C",pch=20, col="#8B814C")
DescTools::PlotPairs(sapply(conspiracy[,-9], function(x) jitter(x, 5)), 
                     g=conspiracy$y,
                     col=alpha("#8B814C", 0.1), 
                     col.smooth="#8B814C")

# TRANSFORM TO LONG DATA FOR PLOTS
conspiracy.Long<- conspiracy %>% pivot_longer(!y, names_to="conspiracy", values_to="score", values_transform=list(score=as.numeric))
  
# HISTOGRAMS
  ggplot(conspiracy.Long, aes(score, fill=conspiracy, color=conspiracy)) +
    geom_histogram(alpha=0.2, breaks=seq(0,5,1)) +
    lemon::facet_rep_wrap(.~conspiracy, nrow=2, labeller="label_both", repeat.tick.labels=T) +
    labs(title="Raw Scores") +
    theme_bw() +
    theme(legend.position = "none",
          panel.border = element_rect(color = "#8B814C"),
          strip.background = element_rect(fill = "#EAEAD6", color = "#8B814C"),
          strip.text = element_text(color = "#8B814C"),
          plot.background = element_rect(fill = "#FAFAF5"),
          axis.text = element_text(color = "#8B814C"),
          axis.title = element_text(color = "#8B814C"),
          plot.title = element_text(color = "#8B814C"),
          axis.ticks = element_line(color = "#8B814C"))
 
  

options(width = 100)
  

  
# RE-LEVEL POLITICAL IDEOLOGY (Very Liberal - Very Conservative)
conspiracy$y<- factor(conspiracy$y, levels=c("Very Liberal", "Liberal", "Somewhat Liberal", "Middle of the Road", "Somewhat Conservative", "Conservative", "Very Conservative"))
  
# RE-NAMED VARIABLE 'y'
names(conspiracy)[9]<- "PoliticalIdeology"
  
# DATA FOR PCA FUNCTION (only keep numeric variables)
df<- conspiracy[,-9]
  
# PCA
#pc1<- prcomp(df, scale.=T)
pc1<- prcomp(df)
summary(pc1)

# SCREE-PLOT
plot(pc1, type="line")

# BIPLOT
autoplot(pc1,
           data=conspiracy, 
           colour="PoliticalIdeology", 
           loadings=T, loadings.colour=alpha("#191970", 0.5), 
           loadings.label=T, loadings.label.colour="#191970", loadings.label.size=5, loadings.label.hjust=0) + 
    scale_colour_manual(values = alpha(paletteer_d("rcartocolor::Temps"), 0.5)) +
    theme_bw() +
    theme(legend.key = element_rect(fill = "#FAFAF5"),
          legend.background = element_rect(fill = "#FAFAF5"),
          legend.text = element_text(color = "#8B814C", size = 14),
          legend.title = element_text(color = "#8B814C", size = 16),
          panel.border = element_rect(color = "#8B814C"),
          plot.background = element_rect(fill = "#FAFAF5"),
          axis.text = element_text(color = "#8B814C", size = 14),
          axis.title = element_text(color = "#8B814C", size = 16),
          axis.ticks = element_line(color = "#8B814C"))
  
# LOADINGS
(princomp(df))$loadings

  
