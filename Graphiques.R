#Load libraries
library(readr)
#importation de tabelle (Complications)
#complications_summary <- read_delim("~/Universite/Master thesis/Data/Complications_summary.csv",";", escape_double = FALSE, trim_ws = TRUE, col_names = TRUE)
#View(complications_summary)

#toplot <- matrix(complications_summary[(3:5),(2:3)])
#barplot(toplot)

#prepare Data
Control_graph <- cbind( Formated_control, 'Control'= rep('Proximal anesthesia',times= nrow(Formated_control)))
Test_graph <- cbind(Formated_test, 'Control'=rep('Wrist block', times=nrow(Formated_test)))
Graph_df <- merge.default(Control_graph,Test_graph, all=TRUE)

#Ouvre PDF
pdf('CMPGAbar1.pdf',paper = 'a4')
#Prépare bloc de plots
layout.matrix <- matrix(c(6,4,1,2,5,3,7,8), nrow=4, ncol =2)
margeg = c(2,1,4,1)
marged = c(2,1,4,1)
layout(mat = layout.matrix)

#Boxplot QDASH
par (mar = c(margeg), yaxt = 'n', xaxt ='t',cex =1)

boxplot(Graph_df$QuickDASH ~ Graph_df$Control,
        main = 'Quick DASH',
        horizontal=TRUE,
        yaxt = 'n',xaxt = 'n',
        col= rep(c('lightgray','white'),times = 2))
Axis(side=1, at = seq (0,10, by =1))
#Boxplot TJROM
par(mar=marged)
boxplot(Graph_df$TJROM ~ Graph_df$Control,
        horizontal=TRUE,
        xaxt = 'n',
        main = 'TJROM',
        col= rep(c('lightgray','white'),times = 2))
Axis(side= 1, at= seq(230,280, by = 5))
#Boxplot Pinch Strength Ratio in %
par(mar=marged)
boxplot(100*Graph_df$Strength_ratio_Finger ~ Graph_df$Control,
        main = 'Pinch Strength Ratio in %',
        horizontal=TRUE,
        xaxt ='n',
        col= rep(c('lightgray','white'),times = 2))
Axis(side =1, at = seq(50,100, by =5))
#Boxplot Pinch Strength in Kg
par(mar=margeg)
boxplot(Graph_df$Pinch.Strength ~ Graph_df$Control,
        main = 'Pinch Strength in Kg',
        horizontal=TRUE,
        xaxt='n',
        col= rep(c('lightgray','white'),times = 2))
Axis(side =1,x=c(1:14),at= seq(2,14,by = 1))
#Boxplot Grip Strength Ratio in %
par(mar=marged)
boxplot(100*Graph_df$Strength_Ratio_Hand ~ Graph_df$Control,
        main = 'Grip Strength Ratio in %',
        horizontal=TRUE,
        xaxt ='n',
        col= rep(c('lightgray','white'),times = 2))
Axis(side =1, at=seq(80,100, by= 5))
#Boxplot Grip Strength in Kg
par(mar=margeg)
boxplot(Graph_df$Grip.Strength ~ Graph_df$Control,
        xaxt = 'n',
        main = 'Grip Strength in Kg',
        horizontal=TRUE,
        col= rep(c('lightgray','white'),times = 2))
Axis (side = 1,at=seq(26,54,by =2))
#Boxplot Pain under load
par(mar=margeg)
boxplot(Graph_df$Operational_Pain ~ Graph_df$Control,
        main = 'Pain under load',
        horizontal=TRUE,
        xaxt ='n',
        col= rep(c('lightgray','white'),times = 2))
Axis (side =1, at =seq(0,10,by =1))
#legende
par(mar=c(2,12,2,0),las =1,cex=1.25,xaxt='n',yaxt='t')
barplot(c('Proximal anesthesia' =1,NA,'Wrist block'=1), horiz = TRUE,col= rep(c('lightgray','white','white'),times = 2),
        xlim=c(0,2), width = c(1,0.5,1))
#Ferme pdf
dev.off()

