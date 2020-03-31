# Results
# Import Population Databases
library(readr)
Formated_ontrol <- read.csv2("Formated Control.csv",";", header = TRUE)
Formated_Test <- read.csv2("Formated Test.csv",";", header = TRUE)

#Range of movement
TEST_TJROM = t.test(Formated_control$TJROM,Formated_Test$TJROM)

#Pain
  #Calculate difference between two pain measure in each group
TEST_PAIN = t.test(Formated_control$Operational_Pain,Formated_Test$Operational_Pain)
  
#Strengh Finger
  #Calculate if different
TEST_F_STRENGH = t.test(Formated_control$Finger_Strengh_Difference,Formated_Test$Finger_Strengh_Difference)

#Strengh Hand
#Calculate if different
TEST_H_STRENGH = t.test(Formated_control$Hand_Strengh_Difference,Formated_Test$Hand_Strengh_Difference)
    
#QDASH score
  #Calculate if differente
TEST_QDASH = t.test(Formated_control$QuickDASH,Formated_Test$QuickDASH)

TEST_RESULTS <- capture.output(print(Test_TJROM),print(TEST_PAIN),print(TEST_F_STRENGH),print(TEST_H_STRENGH), print(TEST_QDASH),print(TEST_REOPERATION))
write_lines(TEST_RESULTS,'TEST_RESULTS.txt')

Control_summary= summary(subset(Formated_control, select = -Patient_N.))
Test_summary = summary(subset(Formated_Test, select = -Patient_N.))
Results_summary= data.frame(Control_summary,Test_summary)
colnames(Results_summary) <- c("Test1","Test","Freq Test","Control1","Control","Freq Control")
Results_summary <- subset(Results_summary,select = -c(Test1,Control1))
write.csv2(Results_summary,file = "Results_summary.csv",row.names = FALSE)
