library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggpubr)

data <- read.csv(file.choose(), header = T) #Load the file to R
data

data$Frame = as.factor(data$Frame) #This step is needed because the column Frame was in the numeric format: 1, 2, 3 and R cannot distinguish this. So this has to be
                                   #converted to factor aka categorical type.

Average_area_10000 <- data$Average_area * 10000 #The area data was in cm^2 so this is to convert to um^2

Average_area_plot <- ggplot(data, aes(x=Frame, y=Average_area_10000, fill=Type))+ 
  geom_boxplot(aes(fill=Type), outlier.shape = NA)+ #exclude plotting the outliers in the boxplot to make it looks nicer
  scale_fill_manual(values = c("orange", "steelblue"))+ #allow to manually colour the boxplot separately
  geom_point(position=position_jitterdodge(), color="purple", size = 1.5, alpha = 0.5)+ #allow to plot all datapoints for each boxplot per subgroup
  #geom_point(aes(fill=Type, colour = Position_ID), position=position_jitterdodge(), size = 1.5, alpha = 0.5)+ #add aes(fill=Type, colour = Position_ID) in the geom_point
                                                                                                               #function to colour by Position_ID and the points align with the boxplot 
  coord_cartesian (ylim =c(0.025, 0.125))+ #manually set the ylimit axis to the desired scale
  #scale_y_continuous(breaks = seq(0.025, 0.125, by=0.01), limits=c(0.025, 0.125))+ #Use this instead of coord_cartesian function to manually set the interval on the y axis
  #facet_wrap(~Frame, ncol = 2, scales = "free_x")+ #this was to split each subgroup into separate plots
  stat_summary(fun = mean, geom="point", colour="darkred", size=2,
               position = position_dodge(width = 0.75))+ #plot the mean value as a darkred dot for each boxplot
   stat_summary(fun = mean, geom="line", aes(group = Type), colour="darkred", size=0.5,
               position = position_dodge(width = 0.75)) #plot a line goes through the mean value of each boxplot according to Type
Average_area_plot_2 <- Average_area_plot + theme(axis.line.x.bottom = element_line(colour = "black", size = 0.5), #set the axis 
                                                 axis.line.y.left = element_line(colour = "black", size = 0.5),
                                                 axis.title.x = element_text(size=10),
                                                 axis.title.y.left = element_text(size=10),
                                                 axis.text.x = element_text(size=10),
                                                 axis.text.y = element_text(size=10))+
  labs(x="Frame", y="Average area (um^2)")
Average_area_plot_2

Average_Perimeter_10000 <- data$Average_Perimeter * 10000
Average_Perimeter_10000_plot <- ggplot(data, aes(x=Frame, y=Average_Perimeter_10000, fill=Type))+ 
  geom_boxplot(aes(fill=Type), outlier.shape = NA)+
  scale_fill_manual(values = c("orange", "steelblue"))+
  geom_point(position=position_jitterdodge(), color="purple", size = 1.5, alpha = 0.5)+
  coord_cartesian (ylim =c(50, 200))+
  #facet_wrap(~Frame, ncol = 2, scales = "free_x")+
  stat_summary(fun = mean, geom="point", colour="darkred", size=2,
               position = position_dodge(width = 0.75))+
  stat_summary(fun = mean, geom="line", aes(group = Type), colour="darkred", size=0.5,
               position = position_dodge(width = 0.75))

Average_Perimeter_10000_plot_2 <- Average_Perimeter_10000_plot + theme(axis.line.x.bottom = element_line(colour = "black", size = 0.5),
                                                 axis.line.y.left = element_line(colour = "black", size = 0.5),
                                                 axis.title.x = element_text(size=10),
                                                 axis.title.y.left = element_text(size=10),
                                                 axis.text.x = element_text(size=10),
                                                 axis.text.y = element_text(size=10))+
  labs(x="Frame", y="Average Perimeter (um)")
Average_Perimeter_10000_plot_2

Average_Circularity_plot <- ggplot(data, aes(x=Frame, y=Average_Circularity, fill=Type))+ 
  geom_boxplot(aes(fill=Type), outlier.shape = NA)+
  scale_fill_manual(values = c("orange", "steelblue"))+
  geom_point(position=position_jitterdodge(), color="purple", size = 1.5, alpha = 0.5)+
  coord_cartesian (ylim =c(0.25, 1))+
  #facet_wrap(~Frame, ncol = 2, scales = "free_x")+
  stat_summary(fun = mean, geom="point", colour="darkred", size=2,
               position = position_dodge(width = 0.75))+
  stat_summary(fun = mean, geom="line", aes(group = Type), colour="darkred", size=0.5,
               position = position_dodge(width = 0.75))

Average_Circularity_plot_2 <- Average_Circularity_plot + theme(axis.line.x.bottom = element_line(colour = "black", size = 0.5),
                                                 axis.line.y.left = element_line(colour = "black", size = 0.5),
                                                 axis.title.x = element_text(size=10),
                                                 axis.title.y.left = element_text(size=10),
                                                 axis.text.x = element_text(size=10),
                                                 axis.text.y = element_text(size=10))+
  labs(x="Frame", y="Average Circularity")
Average_Circularity_plot_2

Solidity_plot <- ggplot(data, aes(x=Frame, y=Solidity, fill=Type))+ 
  geom_boxplot(aes(fill=Type), outlier.shape = NA)+
  scale_fill_manual(values = c("orange", "steelblue"))+
  geom_point(position=position_jitterdodge(), color="purple", size = 1.5, alpha = 0.5)+
  coord_cartesian (ylim =c(0.6, 1))+
  #facet_wrap(~Frame, ncol = 2, scales = "free_x")+
  stat_summary(fun = mean, geom="point", colour="darkred", size=2,
               position = position_dodge(width = 0.75))+
  stat_summary(fun = mean, geom="line", aes(group = Type), colour="darkred", size=0.5,
               position = position_dodge(width = 0.75))

Solidity_plot_2 <- Solidity_plot + theme(axis.line.x.bottom = element_line(colour = "black", size = 0.5),
                                                               axis.line.y.left = element_line(colour = "black", size = 0.5),
                                                               axis.title.x = element_text(size=10),
                                                               axis.title.y.left = element_text(size=10),
                                                               axis.text.x = element_text(size=10),
                                                               axis.text.y = element_text(size=10))+
  labs(x="Frame", y="Solidity")
Solidity_plot_2

data_59 <- subset(data, Frame == 59,
                  select=c(Type, Average_area)) #this command subset out the data from Frame 59 only for the Average_area dataset based on Type (chopped vs intact)
data_59
wilcox.test(data_59$Average_area ~ data_59$Type) #perform wilcox test for this subset data only

data_79 <- subset(data, Frame == 79,
                  select=c(Type, Average_area))
data_79
wilcox.test(data_79$Average_area ~ data_79$Type)

data_99 <- subset(data, Frame == 99,
                  select=c(Type, Average_area))
data_99
wilcox.test(data_99$Average_area ~ data_99$Type)

data_119 <- subset(data, Frame == 119,
                  select=c(Type, Average_area))
data_119
wilcox.test(data_119$Average_area ~ data_119$Type)

data_139 <- subset(data, Frame == 139,
                   select=c(Type, Average_area))
data_139
wilcox.test(data_139$Average_area ~ data_139$Type)

data_159 <- subset(data, Frame == 159,
                   select=c(Type, Average_area))
data_159
wilcox.test(data_159$Average_area ~ data_159$Type)

data_179 <- subset(data, Frame == 179,
                   select=c(Type, Average_area))
data_179
wilcox.test(data_179$Average_area ~ data_179$Type)

data_199 <- subset(data, Frame == 199,
                   select=c(Type, Average_area))
data_199
wilcox.test(data_199$Average_area ~ data_199$Type)

data_219 <- subset(data, Frame == 219,
                   select=c(Type, Average_area))
data_219
wilcox.test(data_219$Average_area ~ data_219$Type)

data_239 <- subset(data, Frame == 239,
                   select=c(Type, Average_area))
data_239
wilcox.test(data_239$Average_area ~ data_239$Type)

data_59_P <- subset(data, Frame == 59,
                  select=c(Type, Average_Perimeter))
data_59_P
wilcox.test(data_59_P$Average_Perimeter ~ data_59_P$Type)

data_79_P <- subset(data, Frame == 79,
                    select=c(Type, Average_Perimeter))
data_79_P
wilcox.test(data_79_P$Average_Perimeter ~ data_79_P$Type)

data_99_P <- subset(data, Frame == 99,
                    select=c(Type, Average_Perimeter))
data_99_P
wilcox.test(data_99_P$Average_Perimeter ~ data_99_P$Type)

data_119_P <- subset(data, Frame == 119,
                    select=c(Type, Average_Perimeter))
data_119_P
wilcox.test(data_119_P$Average_Perimeter ~ data_119_P$Type)

data_139_P <- subset(data, Frame == 139,
                     select=c(Type, Average_Perimeter))
data_139_P
wilcox.test(data_139_P$Average_Perimeter ~ data_139_P$Type)

data_159_P <- subset(data, Frame == 159,
                     select=c(Type, Average_Perimeter))
data_159_P
wilcox.test(data_159_P$Average_Perimeter ~ data_159_P$Type)

data_179_P <- subset(data, Frame == 179,
                     select=c(Type, Average_Perimeter))
data_179_P
wilcox.test(data_179_P$Average_Perimeter ~ data_179_P$Type)

data_199_P <- subset(data, Frame == 199,
                     select=c(Type, Average_Perimeter))
data_199_P
wilcox.test(data_199_P$Average_Perimeter ~ data_199_P$Type)

data_219_P <- subset(data, Frame == 219,
                     select=c(Type, Average_Perimeter))
data_219_P
wilcox.test(data_219_P$Average_Perimeter ~ data_219_P$Type)

data_239_P <- subset(data, Frame == 239,
                     select=c(Type, Average_Perimeter))
data_239_P
wilcox.test(data_239_P$Average_Perimeter ~ data_239_P$Type)

data_59_C <- subset(data, Frame == 59,
                    select=c(Type, Average_Circularity))
data_59_C
wilcox.test(data_59_C$Average_Circularity ~ data_59_C$Type)

data_79_C <- subset(data, Frame == 79,
                    select=c(Type, Average_Circularity))
data_79_C
wilcox.test(data_79_C$Average_Circularity ~ data_79_C$Type)

data_99_C <- subset(data, Frame == 99,
                    select=c(Type, Average_Circularity))
data_99_C
wilcox.test(data_99_C$Average_Circularity ~ data_99_C$Type)

data_119_C <- subset(data, Frame == 119,
                    select=c(Type, Average_Circularity))
data_119_C
wilcox.test(data_119_C$Average_Circularity ~ data_119_C$Type)

data_139_C <- subset(data, Frame == 139,
                     select=c(Type, Average_Circularity))
data_139_C
wilcox.test(data_139_C$Average_Circularity ~ data_139_C$Type)

data_159_C <- subset(data, Frame == 159,
                     select=c(Type, Average_Circularity))
data_159_C
wilcox.test(data_159_C$Average_Circularity ~ data_159_C$Type)

data_179_C <- subset(data, Frame == 179,
                     select=c(Type, Average_Circularity))
data_179_C
wilcox.test(data_179_C$Average_Circularity ~ data_179_C$Type)

data_199_C <- subset(data, Frame == 199,
                     select=c(Type, Average_Circularity))
data_199_C
wilcox.test(data_199_C$Average_Circularity ~ data_199_C$Type)

data_219_C <- subset(data, Frame == 219,
                     select=c(Type, Average_Circularity))
data_219_C
wilcox.test(data_219_C$Average_Circularity ~ data_219_C$Type)

data_239_C <- subset(data, Frame == 239,
                     select=c(Type, Average_Circularity))
data_239_C
wilcox.test(data_239_C$Average_Circularity ~ data_239_C$Type)

data_59_S <- subset(data, Frame == 59,
                    select=c(Type, Solidity))
data_59_S
wilcox.test(data_59_S$Solidity ~ data_59_S$Type)

data_79_S <- subset(data, Frame == 79,
                    select=c(Type, Solidity))
data_79_S
wilcox.test(data_79_S$Solidity ~ data_79_S$Type)

data_99_S <- subset(data, Frame == 99,
                    select=c(Type, Solidity))
data_99_S
wilcox.test(data_99_S$Solidity ~ data_99_S$Type)

data_119_S <- subset(data, Frame == 119,
                    select=c(Type, Solidity))
data_119_S
wilcox.test(data_119_S$Solidity ~ data_119_S$Type)

data_139_S <- subset(data, Frame == 139,
                     select=c(Type, Solidity))
data_139_S
wilcox.test(data_139_S$Solidity ~ data_139_S$Type)

data_159_S <- subset(data, Frame == 159,
                     select=c(Type, Solidity))
data_159_S
wilcox.test(data_159_S$Solidity ~ data_159_S$Type)

data_179_S <- subset(data, Frame == 179,
                     select=c(Type, Solidity))
data_179_S
wilcox.test(data_179_S$Solidity ~ data_179_S$Type)

data_199_S <- subset(data, Frame == 199,
                     select=c(Type, Solidity))
data_199_S
wilcox.test(data_199_S$Solidity ~ data_199_S$Type)

data_219_S <- subset(data, Frame == 219,
                     select=c(Type, Solidity))
data_219_S
wilcox.test(data_219_S$Solidity ~ data_219_S$Type)

data_239_S <- subset(data, Frame == 239,
                     select=c(Type, Solidity))
data_239_S
wilcox.test(data_239_S$Solidity ~ data_239_S$Type)
