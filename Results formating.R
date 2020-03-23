# Formatage des résultats

#Importation des valeurs brutes
library(readxl)
Results <- read_excel("~/Université/Master thesis/Data/Results.xlsx")
#Importation du tableau de correspondances N0/groupe
library(readr)
CaseN0 <- read_delim("CaseN0.csv", ";", escape_double = FALSE, 
                     trim_ws = TRUE)

#Calcul douleur operationnelle

Pain_at_arrival = Results$`Pain at Arrival Operated`- Results$`Pain at Arrival Non-Operated`
Pain_after_exercises = Results$`Pain Test operated`- Results$`Pain Test Non-operated`
Operational_Pain = abs(Pain_after_exercises - Pain_at_arrival)

#Calcul force doigts
Max_Strength_Finger_Non_Operated =pmax(Results$`Strength Finger Non-operated I`,
                  Results$`Strength Finger Non-operated II`,Results$`Strength Finger Non-operated III`,na.rm = TRUE)
Max_Strength_Finger_Operated =pmax(Results$`Strength Finger operated I`,
                                      Results$`Strength Finger operated II`,Results$`Strength Finger operated III`,na.rm = TRUE)

#Calcul ratio de force entre doigt dominant et non-dom

Strength_R_Finger_Raw = signif(ifelse(test = na.exclude(Results$`Dominant Hand`== Results$`OP Hand`),
                        yes = Max_Strength_Finger_Non_Operated/Max_Strength_Finger_Operated,
                        no = Max_Strength_Finger_Operated/Max_Strength_Finger_Non_Operated),digits = 3)
Strength_ratio_Finger = 1- abs(1-Strength_R_Finger_Raw)
rm(Max_Strength_Finger_Non_Operated,Max_Strength_Finger_Operated)

#Calcul force mains

Max_Strength_Hand_Non_Operated =pmax(Results$`Strength Hand non-operated I`,
                                      Results$`Strength Hand Non-operated II`,Results$`Strength Hand Non-operated III`,na.rm = TRUE)
Max_Strength_Hand_Operated =pmax(Results$`Strength Hand operated I`,
                                  Results$`Strength Hand operated II`,Results$`Strength Hand operated III`,na.rm = TRUE)
#Calcul ratio de force entre main dominante et non-dom

Strength_R_Raw_Hand = signif(ifelse(test = na.exclude(Results$`Dominant Hand`== Results$`OP Hand`),
                            yes = Max_Strength_Hand_Non_Operated/Max_Strength_Hand_Operated,
                            no = Max_Strength_Hand_Operated/Max_Strength_Hand_Non_Operated),digits = 3)
Strength_Ratio_Hand = 1- abs(1-Strength_R_Raw_Hand)

rm(Max_Strength_Hand_Non_Operated,Max_Strength_Hand_Operated)

#Complier Résultats
Formated_results = data.frame('Patient_N°' = Results$`Patient N°`,
                              'QuickDASH'= Results$QuickDASH,
                              'TJROM'= Results$TJROM, Operational_Pain,Strength_ratio_Finger,Strength_Ratio_Hand)

Formated_control = subset(Formated_results, Formated_results$Patient_N. %in% CaseN0$Listcontrol)
Formated_test = subset(Formated_results,Formated_results$Patient_N. %in% CaseN0$Listtest)

View(Formated_results)
rm(Formated_results)

write.csv2(Formated_control,file = "Formated Control.csv", row.names = FALSE)
write.csv2(Formated_test,file = "Formated Test.csv", row.names = FALSE)



