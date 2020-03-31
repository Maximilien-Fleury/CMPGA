# Population Data


# Import Population Databases
library(readr)
Population_Control <- read_delim("~/Université/Master thesis/Data/Population Control.csv", 
                                 ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1" ))
Population_Test <- read_delim("~/Université/Master thesis/Data/Population Test.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1" ))

Complete_Pop_Con = Population_Control[complete.cases(Population_Control$Reunion),]
Complete_Pop_Tes = Population_Test[complete.cases(Population_Test$Reunion),]

Followup_Con = Population_Control[complete.cases(Population_Control$'OP Hand'),]
Followup_Tes = Population_Test[complete.cases(Population_Test$'OP Hand'),]

#----

# Sex
  # Calculate Sex ratio Control Population
Sex = data.frame('Control'= c(sum(lengths(regmatches(Complete_Pop_Con$Sex, gregexpr("m", Complete_Pop_Con$Sex)))),
                              sum(lengths(regmatches(Complete_Pop_Con$Sex, gregexpr("w", Complete_Pop_Con$Sex))))),
                 'Test'= c(sum(lengths(regmatches(Complete_Pop_Tes$Sex, gregexpr("m", Complete_Pop_Tes$Sex)))),
                           sum(lengths(regmatches(Complete_Pop_Tes$Sex, gregexpr("w", Complete_Pop_Tes$Sex))))),
                 row.names = c('Male','Female'))  

#Age
  # Standard Deviation Test and Control Population
 Age = data.frame('Control'= c(mean(Complete_Pop_Con$Age),sd(Complete_Pop_Con$Age)),'Test'= c(mean(Complete_Pop_Tes$Age),sd(Complete_Pop_Tes$Age)),
                  row.names = c('mean Age','SD Age'))
 Age_Followup = signif(data.frame('Control'=c(mean(Followup_Con$Age),sd(Followup_Con$Age)),'Test'=c(mean(Followup_Tes$Age),sd(Followup_Tes$Age)),
                                  row.names = c('mean Age','SD Age')), digits = 3)
 mean(c(Followup_Con$Age,Followup_Tes$Age))
 sd(c(Followup_Con$Age,Followup_Tes$Age))
 
  # Location
  # Calculate fracture ratio Control Population
  Fracture_Location = data.frame('Control'= c(sum(lengths(regmatches(Complete_Pop_Con$Location, gregexpr("P", Complete_Pop_Con$Location)))),
                                            sum(lengths(regmatches(Complete_Pop_Con$Location, gregexpr("M", Complete_Pop_Con$Location))))),
                                 'Test'= c(sum(lengths(regmatches(Complete_Pop_Tes$Location, gregexpr("P", Complete_Pop_Tes$Location)))),
                                           sum(lengths(regmatches(Complete_Pop_Tes$Location, gregexpr("M", Complete_Pop_Tes$Location))))),
                                 row.names = c('Phalangeal','Metacarpal'))
  
#Postoperative bone union
  #Bone union = 1. No bone union = 0
  #Calculate Bone union ratio in each Population
  NA_Bone_C = sum(is.na(Population_Control$Reunion))
  NA_Bone_T = sum(is.na(Population_Test$Reunion))
  
  Bone_union_ratio_T = signif(sum(Complete_Pop_Tes$Reunion == 1,  na.rm = TRUE)/(length(Complete_Pop_Tes$Reunion)),digits = 3)
  Bone_union_ratio_C = signif(sum(Complete_Pop_Con$Reunion == 1, na.rm = TRUE)/(length(Complete_Pop_Con$Reunion)), digits = 3)
  Bone_union_ratio_All = signif((sum(Complete_Pop_Con$Reunion == 1, na.rm = TRUE)+sum(Complete_Pop_Tes$Reunion ==1, na.rm = TRUE))/
                                  (length(Complete_Pop_Con$Reunion)+length(Complete_Pop_Tes$Reunion)), digits = 3)
  Bone_union = matrix(nrow = 2, ncol = 2,
                         c(sum(Complete_Pop_Con$Reunion ==1,na.rm = TRUE),
                           sum(Complete_Pop_Con$Reunion==0,na.rm = TRUE),
                           sum(Complete_Pop_Tes$Reunion==1,na.rm = TRUE),
                           sum(Complete_Pop_Tes$Reunion ==0,na.rm = TRUE)))
                    
  #Chi square test
  TEST_UNION = chisq.test(x = Bone_union)
  

# Clavien-Dindo Classification

  # CD population description
  for (I in 1:7) {
    if (I==1) {
      CD = c(0,1,2,3,3.5,4,5)
      CD_C =CD_T = 1:7
    }
    CD_C[I] = sum(Complete_Pop_Con$'CD Classification' == CD[I])
    CD_T[I] = sum(Complete_Pop_Tes$'CD Classification' == CD[I])
    if (I==7){
      Clavien_Dindo = data.frame(row.names = CD,CD_C,CD_T)
      colnames(Clavien_Dindo)<- c('Control','Test')
      rm(I,CD,CD_T,CD_C)
    }
  }
  #Reoperation rate
  Reoperation = matrix(nrow = 2, ncol = 2,
                       c(sum(Population_Control$'CD Classification' <3),
                         sum(Population_Control$'CD Classification' >=3) - sum(Population_Control$'CD Classification' >=4),
                         sum(Population_Test$'CD Classification' <3),
                         sum(Population_Test$'CD Classification' >=3) - sum(Population_Test$'CD Classification' >=4)))
  ReoperationR_Control = Reoperation[2,1]/Reoperation[1,1]
  ReoperationR_Test = Reoperation[2,2]/Reoperation[1,2]
  #Chi square test
  TEST_REOPERATION = chisq.test(x = Reoperation)
  
# Dominant Hand
Dominant_Hand = data.frame('Control'= c(sum(Population_Control$'Dominant Hand' == 1, na.rm = TRUE),sum(Population_Control$'Dominant Hand' == 0, na.rm = TRUE)),
                           'Test'= c(sum(Complete_Pop_Tes$'Dominant Hand' == 1, na.rm = TRUE),sum(Complete_Pop_Tes$'Dominant Hand' == 0, na.rm = TRUE)),
                           row.names = c('Dominant Right','Dominant Left'))
#OP Hand
Operated_Hand = data.frame('Control'= c(sum(Population_Control$'OP Hand' == 1, na.rm = TRUE),sum(Population_Control$'OP Hand' == 0, na.rm = TRUE)),
                           'Test'= c(sum(Population_Test$'OP Hand' == 1, na.rm = TRUE),sum(Population_Test$'OP Hand' == 0, na.rm = TRUE))
                           ,row.names = c('Operated Right','Operated Left'))

# OP_ Finger population Control description
for (I in 1:5) {
  if(I==1) {
    OP_Finger_Control = OP_Finger_Test = c(1:5)
  }
  OP_Finger_Control[I] = sum(Population_Control$'OP Finger' == OP_Finger_Control[I], na.rm = TRUE)
  OP_Finger_Test[I]= sum(Population_Test$'OP Finger'== OP_Finger_Test[I],na.rm = TRUE)
  if(I==5){
    Operated_Finger = data.frame(row.names = 1:5, OP_Finger_Control,OP_Finger_Test)
    rm(I,OP_Finger_Control,OP_Finger_Test)
  }
  }
Population_Characteristics = signif(data.frame('Control' = c(Sex$Control,Age$Control,Fracture_Location$Control,Dominant_Hand$Control,Operated_Hand$Control,
                                                             Bone_union[,1],NA_Bone_C,TEST_UNION$p.value,Operated_Finger$OP_Finger_Control),
                                               'Test' = c(Sex$Test,Age$Test,Fracture_Location$Test,Dominant_Hand$Test,Operated_Hand$Test,
                                                          Bone_union[,2],NA_Bone_T,TEST_UNION$p.value,Operated_Finger$OP_Finger_Test),
                                               row.names = c('Male','Female','mean Age','SD Age','Phalangeal','Metacarpal','Dominant Right','Dominant Left',
                                                             'Operated Right','Operated Left','Union','Non-union','NA Union','Union Chisq','Finger: 1',2:5))
                                    ,digits = 3)
#rm(Sex,Age,Fracture_Location,Dominant_Hand,Operated_Hand,Operated_Finger,Bone_union, Population_Control,Population_Test,Complete_Pop_Con,Complete_Pop_Tes)
#Export results
# write.csv2(Population_Characteristics,file = 'Population Characteristics.csv')
# write.csv2(Clavien_Dindo, file = '~/Université/Master thesis/Data/clavien-dindo.csv')
#View(Population_Characteristics) View(Clavien_Dindo) View(TEST_UNION)
library(readxl)
Data_compiled_Dates <- read_excel("~/Université/Master thesis/Data/Data_compiled.xlsx",sheet = 'Dates')
Data_compiled_Dates <- Data_compiled_Dates[complete.cases(Data_compiled_Dates),]
View(Data_compiled_Dates)
Time_to_Followup <- difftime(Data_compiled_Dates$`Follow-up date`, Data_compiled_Dates$`OP Date`)
Mean_Time <- mean(Time_to_Followup)
Mean_Time <- (c(as.integer(Mean_Time/365.25),as.integer((Mean_Time/365-as.integer(Mean_Time/365.25))*12)))
sd(Time_to_Followup)/30
