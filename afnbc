# Title : Fuzzy Naive Bayes Classifier R code
# Author:  Necla Kayaalp
# Date created: 5 February 2016 Last modified: 5 February 2016

# install.packages("evtree")
library(evtree)
data("StatlogHeart")
X=StatlogHeart
names(X)

A=data.frame(X$age, X$chest_pain_type,X$serum_colestoral,X$fasting_blood_sugar,
X$resting_electrocardiographic_results,  X$maximum_heart_rate, X$oldpeak, 
X$slope_of_the_peak, X$resting_blood_pressure, X$thal, X$heart_disease)

colnames(A)=c("age", "chest_pain_type","serum_colestoral","fasting_blood_sugar",
"resting_electrocardiographic_results", "maximum_heart_rate", "oldpeak", 
"slope_of_the_peak", "resting_blood_pressure",  "thal", "heart_disease")

m_false=function(x){if (x< 29){ 
     0
                   } else if (29<=x && x<60){
   (x-29)/31 
                   } else if (60<=x && x<77){
   1
                   }else 
   0
}
m_true=function(x){if (x< 42){ 
     0
                   } else if (42<=x && x<48){
   (x-42)/6 
                   } else if (48<=x && x<71){
      1            } else 
      0    
}
m_normal=function(x){if (x< 126){ 
     0
                   } else if (126<=x && x<226){
   (x-126)/100 
                   } else if (226<=x && x<354){
      1            } else 
      0    
}

m_ST_twaveabnormality=function(x){if (x< 197){ 
     0
                   } else if (197<=x && x<210){
   (x-197)/13 
                   } else if (210<=x && x<327){
      1            } else 
      0    
}
 
m_leftventicularhypertophy=function(x){if (x< 149){ 
     0
                   } else if (149<=x && x<215){
   (x-149)/66 
                   } else if (215<=x && x<564){
      1            } else 
      0    
}
m_upsloping=function(x){if (x< 96){ 
     0
                   } else if (96<=x && x<=202){
      1            } else 
      0    
}

m_flat=function(x){if (x<71){ 
     0
                   } else if (71<=x && x<75){
   (x-71)/4 
                   } else if (75<=x && x<190){
      1            } else 
      0    
}
m_downsloping=function(x){if (x< 96){ 
     0
                   } else if (96<=x && x<=194){
      1            } else 
      0    
}
m_typilangina=function(x){if (x<94){ 
     0
                   } else if (94<=x && x<170){
   (x-94)/76 
                   } else if (170<=x && x<=180){
      1            } else 
      0    
}

m_atypilangina=function(x){if (x<94){ 
     0
                   } else if (94<=x && x<155){
   (x-94)/61 
                   } else if (155<=x && x<=192){
      1            } else 
      0    
}

m_nonanginalpain=function(x){if (x<100){ 
     0
                   } else if (100<=x && x<145){
   (x-100)/45 
                   } else if (145<=x && x<=200){
      1            } else 
      0    
}

m_asymptomatic=function(x){if (x<108){ 
     0
                   } else if (108<=x && x<130){
   (x-108)/22 
                   } else if (130<=x && x<=165){
      1            } else 
      0    
}


m_normal=function(x){if (x>3.6){ 
     0
                   } else if (0<=x && x<1.8){
   (x)/1.8 
                   } else if (1.8<=x && x<=3.6){
      1            } else 
      0    
}

m_fixedefect=function(x){if (x>2.3){ 
     0
                   } else if (0<=x && x<1.3){
   (x)/1.3 
                   } else if (1.3<=x && x<=2.3){
      1            } else 
      0    
}

m_reversabledefect=function(x){if (x>6.2){ 
     0
                   } else if (0<=x && x<1.0){
   (x)/1.0 
                   } else if (1.0<=x && x<=6.2){
      1            } else 
      0    
}



######age~fasting_blood_sugar

for (i in 1:nrow(A)) {

if(A$fasting_blood_sugar[i]=="yes")

                 A$membership_age[i]=m_true(A[i,"age"])
else
A$membership_age[i]=m_false(A[i,"age"])

}
taw_age=matrix(0, nrow = nrow(A), ncol = length(levels(A$fasting_blood_sugar)), byrow = FALSE)
for (i in 1:nrow(A)) {
       
                  taw_age[i,1]=m_false(A[i,"age"])
                  taw_age[i,2]=m_true(A[i,"age"])
}

beta_age=rep(0,nrow(A))
n<- length(levels(A$fasting_blood_sugar))
for (i in 1:nrow(A)) {

            beta_age[i]=((n-2)*taw[i,(n-1)]+(n-1)*taw[i,n])/(taw[i,(n-1)]+taw[i,n])
                               
} 

###converting NAN into zero 
beta_age[is.na(beta_age)] <- 0

beta_fasting_blood_sugar<- rep(0,nrow(A))
for (i in 1:nrow(A)){
                if (A$fasting_blood_sugar[i]=="no")

                beta_fasting_blood_sugar[i]=0

                 else 
                 beta_fasting_blood_sugar[i]=1
}

betas1<- data.frame(beta_age,beta_fasting_blood_sugar)
means1=rowMeans(betas1)

h1=rep(0,nrow(A))
gamma1=rep(0,nrow(A))
for (i in 1:nrow(A))
         
            h1[i]=trunc(means1[i])
            gamma1[i]=means1[i]-h1[i]

d1<- data.frame(betas1,means1,h1,gamma1)


#####thal~oldpeak

for (i in 1:nrow(A)) {

if(A$thal[i]=="normal")
                 A$membership_oldpeak[i]=m_normal(A[i,"oldpeak"])
                
                 else if(A$thal[i]=="fixed defect")

A$membership_oldpeak[i]=m_fixedefect(A[i,"oldpeak"])

    else  
A$membership_oldpeak[i]=m_reversabledefect(A[i,"oldpeak"])
}

taw_oldpeak=matrix(0, nrow = nrow(A), ncol = length(levels(A$thal)), byrow = FALSE)
for (i in 1:nrow(A)) {
       
                  taw_oldpeak[i,1]=m_normal(A[i,"oldpeak"])
                  taw_oldpeak[i,2]=m_fixedefect(A[i,"oldpeak"])
                  taw_oldpeak[i,3]=m_reversabledefect(A[i,"oldpeak"])

}

beta_oldpeak=rep(0,nrow(A))
n<- length(levels(A$thal))
for (i in 1:nrow(A)) {

           beta_oldpeak[i]=((n-3)*taw_oldpeak[i,(n-2)]+(n-2)*taw_oldpeak[i,(n-1)]+(n-1)*taw_oldpeak[i,n])/(taw_oldpeak[i,(n-2)]+taw_oldpeak[i,(n-1)]+taw_oldpeak[i,n])
                               
}
###converting NAN into zero 
beta_oldpeak[is.na(beta_oldpeak)] <- 0

beta_thal<- rep(0,nrow(A))
for (i in 1:nrow(A)){
                if (A$thal[i]=="normal")

                beta_thal[i]=0

                 else if (A$thal[i]=="fixedefect")
                 beta_thal[i]=1
                 else 
                 beta_thal[i]=2
}

betas2<- data.frame(beta_oldpeak,beta_thal)
means2=rowMeans(betas2)

h2=rep(0,nrow(A))
gamma2=rep(0,nrow(A))
for (i in 1:nrow(A))
         
            h2[i]=trunc(means2[i])
            gamma2[i]=means2[i]-h2[i]

d2<- data.frame(betas2,means2,h2,gamma2)

#####resting_electrocardiographic_results~serum_colestoral

for (i in 1:nrow(A)) {

if(A$resting_electrocardiographic_results[i]==0)
                 A$membership_serum[i]=m_normal(A[i,"serum_colestoral"])
                
                 else if(A$resting_electrocardiographic_results[i]==1)

A$membership_serum[i]=m_ST_twaveabnormality(A[i,"serum_colestoral"])

    else  
A$membership_serum[i]=m_leftventicularhypertophy(A[i,"serum_colestoral"])
}

taw_serum_colestoral=matrix(0, nrow = nrow(A), ncol = length(levels(A$thal)), byrow = FALSE)
for (i in 1:nrow(A)) {
       
                  taw_serum_colestoral[i,1]=m_normal(A[i,"serum_colestoral"])
                  taw_serum_colestoral[i,2]=m_ST_twaveabnormality(A[i,"serum_colestoral"])
                  taw_serum_colestoral[i,3]=m_leftventicularhypertophy(A[i,"serum_colestoral"])

}

beta_serum_colestoral=rep(0,nrow(A))
n<- length(levels(A$resting_electrocardiographic_results))
for (i in 1:nrow(A)) {

            beta_serum_colestoral[i]=((n-3)*taw_serum_colestoral[i,(n-2)]+(n-2)*taw_serum_colestoral[i,(n-1)]+(n-1)*taw_serum_colestoral[i,n])/(taw_serum_colestoral[i,(n-2)]+taw_serum_colestoral[i,(n-1)]+taw_serum_colestoral[i,n])
                               
}
###converting NAN into zero 
beta_serum_colestoral[is.na(beta_serum_colestoral)] <- 0

beta_resting_electrocardiographic_results<- rep(0,nrow(A))
for (i in 1:nrow(A)){
                if (A$resting_electrocardiographic_results[i]=="0")

                beta_resting_electrocardiographic_results[i]=0

                 else if (A$resting_electrocardiographic_results[i]=="1")
                 beta_resting_electrocardiographic_results[i]=1
                 else 
                 beta_resting_electrocardiographic_results[i]=2
}

betas3<- data.frame(beta_serum_colestoral,beta_resting_electrocardiographic_results)
means3=rowMeans(betas3)

h3=rep(0,nrow(A))
gamma3=rep(0,nrow(A))
for (i in 1:nrow(A))
         
            h3[i]=trunc(means3[i])
            gamma3[i]=means3[i]-h3[i]

d3<- data.frame(betas3,means3,h3,gamma3)


#####slope_of_the_peak~maximum_heart_rate


for (i in 1:nrow(A)) {

if(A$slope_of_the_peak[i]=="upsloping")
                 A$membership_maximum_heart_rate[i]=m_upsloping(A[i,"maximum_heart_rate"])
                
                 else if(A$slope_of_the_peak[i]=="flat")

A$membership_maximum_heart_rate[i]=m_flat(A[i,"maximum_heart_rate"])

    else  
A$membership_maximum_heart_rate[i]=m_downsloping(A[i,"maximum_heart_rate"])
}


taw_maximum_heart_rate=matrix(0, nrow = nrow(A), ncol = length(levels(A$slope_of_the_peak)), byrow = FALSE)
for (i in 1:nrow(A)) {
       
                  taw_maximum_heart_rate[i,1]=m_upsloping(A[i,"maximum_heart_rate"])
                  taw_maximum_heart_rate[i,2]=m_flat(A[i,"maximum_heart_rate"])
                  taw_maximum_heart_rate[i,3]=m_downsloping(A[i,"maximum_heart_rate"])

}

beta_maximum_heart_rate=rep(0,nrow(A))
n<- length(levels(A$slope_of_the_peak))
for (i in 1:nrow(A)) {

          beta_maximum_heart_rate[i]=((n-3)*taw_maximum_heart_rate[i,(n-2)]+(n-2)*taw_maximum_heart_rate[i,(n-1)]+(n-1)*taw_maximum_heart_rate[i,n])/(taw_maximum_heart_rate[i,(n-2)]+taw_maximum_heart_rate[i,(n-1)]+taw_maximum_heart_rate[i,n])
                               
}
###converting NAN into zero 
beta_maximum_heart_rate[is.na(beta_maximum_heart_rate)] <- 0

beta_slope_of_the_peak<- rep(0,nrow(A))
for (i in 1:nrow(A)){
                if (A$slope_of_the_peak[i]=="upsloping")

                beta_slope_of_the_peak[i]=0

                 else if (A$slope_of_the_peak[i]=="flat")
                 beta_slope_of_the_peak[i]=1
                 else 
                 beta_slope_of_the_peak[i]=2
}

betas4<- data.frame(beta_slope_of_the_peak,beta_maximum_heart_rate)
means4=rowMeans(betas4)

h4=rep(0,nrow(A))
gamma4=rep(0,nrow(A))
for (i in 1:nrow(A))
         
            h4[i]=trunc(means4[i])
            gamma4[i]=means4[i]-h4[i]

d4<- data.frame(betas4,means4,h4,gamma4)

#####chest_pain_type~resting_blood_pressure

for (i in 1:nrow(A)) {

if(A$chest_pain_type[i]=="typical angina")
                 A$membership_resting_blood_pressure[i]=m_typilangina(A[i,"resting_blood_pressure"])
                
                 else if(A$chest_pain_type[i]=="atypical angina")

A$membership_resting_blood_pressure[i]=m_atypilangina(A[i,"resting_blood_pressure"])
             
                 else if(A$chest_pain_type[i]=="non-anginal pain")

A$membership_resting_blood_pressure[i]=m_nonanginalpain(A[i,"resting_blood_pressure"])



    else  
A$membership_resting_blood_pressure[i]=m_asymptomatic(A[i,"resting_blood_pressure"])
}



taw_resting_blood_pressure=matrix(0, nrow = nrow(A), ncol = length(levels(A$chest_pain_type)), byrow = FALSE)
for (i in 1:nrow(A)) {
       
                  taw_resting_blood_pressure[i,1]=m_typilangina(A[i,"resting_blood_pressure"])
                  taw_resting_blood_pressure[i,2]=m_atypilangina(A[i,"resting_blood_pressure"])
                  taw_resting_blood_pressure[i,3]=m_nonanginalpain(A[i,"resting_blood_pressure"])
                  taw_resting_blood_pressure[i,4]=m_asymptomatic(A[i,"resting_blood_pressure"])

}

beta_resting_blood_pressure=rep(0,nrow(A))
n<- length(levels(A$chest_pain_type))
for (i in 1:nrow(A)) {

           beta_resting_blood_pressure[i]=((n-4)*taw_resting_blood_pressure[i,(n-3)]+(n-3)*taw_resting_blood_pressure[i,(n-2)]+(n-2)*taw_resting_blood_pressure[i,(n-1)]+(n-1)*taw_resting_blood_pressure[i,n])/(taw_resting_blood_pressure[i,(n-3)]+taw_resting_blood_pressure[i,(n-2)]+taw_resting_blood_pressure[i,(n-1)]+taw_resting_blood_pressure[i,n])
                               
}
###converting NAN into zero 
beta_resting_blood_pressure[is.na(beta_resting_blood_pressure)] <- 0


beta_maximum_heart_rate=rep(0,nrow(A))
n<- length(levels(A$slope_of_the_peak))
for (i in 1:nrow(A)) {

          beta_maximum_heart_rate[i]=((n-3)*taw_maximum_heart_rate[i,(n-2)]+(n-2)*taw_maximum_heart_rate[i,(n-1)]+(n-1)*taw_maximum_heart_rate[i,n])/(taw_maximum_heart_rate[i,(n-2)]+taw_maximum_heart_rate[i,(n-1)]+taw_maximum_heart_rate[i,n])
                               
}
###converting NAN into zero 
beta_maximum_heart_rate[is.na(beta_maximum_heart_rate)] <- 0

beta_chest_pain_type<- rep(0,nrow(A))
for (i in 1:nrow(A)){
                if (A$chest_pain_type[i]=="typical angina")

                beta_chest_pain_type[i]=0

                 else if (A$chest_pain_type[i]=="atypical angina")
                 beta_chest_pain_type[i]=1
                 else if (A$chest_pain_type[i]=="non-anginal pain")
                 beta_chest_pain_type[i]=2
                 else 
                 beta_chest_pain_type[i]=3
}

betas5<- data.frame(beta_chest_pain_type,beta_maximum_heart_rate)
means5=rowMeans(betas5)

h5=rep(0,nrow(A))
gamma5=rep(0,nrow(A))
for (i in 1:nrow(A))
         
            h5[i]=trunc(means5[i])
            gamma5[i]=means5[i]-h5[i]

d5<- data.frame(betas5,means5,h5,gamma5)



