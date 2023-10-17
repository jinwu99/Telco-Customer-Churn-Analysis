rm(list=ls())
library('StatMatch')
library('cluster')
library('rhtmlMoonPlot')
library(MASS)

df <- read.csv('Telco_customer_churn.csv')
# Check out the data features
summary(df)

# Extract the features we use
col_use <- colnames(df)
col_use <- col_use[-c(1:9,28,30:33)]
dt <- df[col_use]

# Change the name of categorical factors (for the ease of visualization)
dt[dt$Senior.Citizen=='Yes','Senior.Citizen'] <- 'Senior'
dt[dt$Senior.Citizen=='No','Senior.Citizen'] <- 'Young'
dt[dt$Partner=='Yes','Partner'] <- 'w/Partner'
dt[dt$Partner=='No','Partner'] <- 'No_Partner'
dt[dt$Dependents=='Yes','Dependents'] <- 'w/Depend'
dt[dt$Dependents=='No','Dependents'] <- 'No_Depend'
dt[dt$Phone.Service=='Yes','Phone.Service'] <- 'Phone_sv'
dt[dt$Phone.Service=='No','Phone.Service'] <- 'No_Phone_sv'
dt[dt$Internet.Service=='No','Internet.Service'] <- 'No_Internet_sv'
dt[dt$Multiple.Lines=='Yes','Multiple.Lines'] <- 'Mult_Lines'
dt[dt$Multiple.Lines=='No phone service','Multiple.Lines'] <- 'No_Mult_Lines'
dt[dt$Multiple.Lines=='No','Multiple.Lines'] <- 'No_Mult_Lines'
dt[dt$Online.Security=='Yes','Online.Security'] <- 'Secure'
dt[dt$Online.Security=='No internet service','Online.Security'] <- 'No_Secure'
dt[dt$Online.Security=='No','Online.Security'] <- 'No_Secure'
dt[dt$Online.Backup=='Yes','Online.Backup'] <- 'Backup'
dt[dt$Online.Backup=='No internet service','Online.Backup'] <- 'No_Backup'
dt[dt$Online.Backup=='No','Online.Backup'] <- 'No_Backup'
dt[dt$Device.Protection=='Yes','Device.Protection'] <- 'Protect'
dt[dt$Device.Protection=='No internet service','Device.Protection'] <- 'No_Protect'
dt[dt$Device.Protection=='No','Device.Protection'] <- 'No_Protect'
dt[dt$Tech.Support=='Yes','Tech.Support'] <- 'Tech_Sup'
dt[dt$Tech.Support=='No internet service','Tech.Support'] <- 'No_Tech_Sup'
dt[dt$Tech.Support=='No','Tech.Support'] <- 'No_Tech_Sup'
dt[dt$Streaming.TV=='Yes','Streaming.TV'] <- 'TV'
dt[dt$Streaming.TV=='No internet service','Streaming.TV'] <- 'No_TV'
dt[dt$Streaming.TV=='No','Streaming.TV'] <- 'No_TV'
dt[dt$Streaming.Movies=='Yes','Streaming.Movies'] <- 'Movie'
dt[dt$Streaming.Movies=='No internet service','Streaming.Movies'] <- 'No_Movie'
dt[dt$Streaming.Movies=='No','Streaming.Movies'] <- 'No_Movie'
dt[dt$Paperless.Billing=='Yes','Paperless.Billing'] <- 'Paperless'
dt[dt$Paperless.Billing=='No','Paperless.Billing'] <- 'No_Paperless'
dt[dt$Churn.Label=='Yes','Churn.Label'] <- 'Churn_Yes'
dt[dt$Churn.Label=='No','Churn.Label'] <- 'Churn_No'

# We first cluster this mixed-type data with gower distance
dist.mat <- gower.dist(dt)

# And cluster with K-medoids method
# Determine the best number of clusters with Silhouette score
k_candidates <- 3:6
silh_scores <-sapply(k_candidates, function(k){
  km.res <- clara(dist.mat,k)
  silh <- silhouette(km.res$clustering, dist.mat)
  return(mean(silh[,3]))
})
plot(k_candidates,silh_scores,type='l',pch=1)

# The optimal number of clusters is 3.
# We attach cluster labels to our data
km.res <- clara(dist.mat,3)
dt$cluster <- km.res$clustering; dt$cluster <- as.factor(dt$cluster)

# Percentage of churn for each cluster
tab <- table(dt$cluster, dt$Churn.Label)
churn_percentage <- tab[,2]/(tab[,1]+tab[,2])
round(churn_percentage,2)

# Transform continuous variables into categorical variables
# to measure the association btw clusters and all the categorical factors
# and we will visualize it with Correspondence Analysis(CA) via Moonplot
tenure_mthly_intv <- seq(min(dt$Tenure.Months),max(dt$Tenure.Months),len=4)
dt$Tenure.Months.cat <- 'tenure_low'
dt[dt$Tenure.Months>tenure_mthly_intv[2] & 
     dt$Tenure.Months<=tenure_mthly_intv[3],'Tenure.Months.cat'] <- 'tenure_mid'
dt[dt$Tenure.Months>tenure_mthly_intv[3] & 
     dt$Tenure.Months<=tenure_mthly_intv[4],'Tenure.Months.cat'] <- 'tenure_high'

charges_mnthly_intv <- seq(min(dt$Monthly.Charges),max(dt$Monthly.Charges),len=4)
dt$Monthly.Charges.cat <- 'pay_low'
dt[dt$Monthly.Charges>charges_mnthly_intv[2] & 
     dt$Monthly.Charges<=charges_mnthly_intv[3],'Monthly.Charges.cat'] <- 'pay_mid'
dt[dt$Monthly.Charges>charges_mnthly_intv[3] & 
     dt$Monthly.Charges<=charges_mnthly_intv[4],'Monthly.Charges.cat'] <- 'pay_high'


# Before doing CA, we need to build two-way contigency table
# to give as an input to CA function and also to do overall chi-square test
cat_vars <- colnames(dt); cat_vars
cat_vars <- cat_vars[-c(5,18,20)]; cat_vars
dt_tb <- table(dt$cluster,dt[,1])
for(st in cat_vars[-1]){
  print(chisq.test(table(dt$cluster,dt[,st]))$p.value)
  dt_tb <- cbind(dt_tb,table(dt$cluster,dt[,st]))
}
t(dt_tb)
dt_chi <- chisq.test(dt_tb)

# Do CA with two-dimension
ct <- corresp(dt_tb, nf=2)

# And plot with the moonplot
# Moonplot : from displayr
moonplot(ct$rscore,ct$cscore,
         core.font.size = 30,
         surface.font.baseSize = 13)
