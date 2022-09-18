library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggpubr)

data <- read.csv(file.choose(), header = T)
head(data)

data$Frame = as.factor(data$Frame)
data$Cell_ID = as.factor(data$Cell_ID)

Average_area_plot <- ggplot(data = data) +
  geom_boxplot(position=position_dodge(0.75), aes(x=Frame, y=Area_um_2, fill = Type), outlier.shape = NA, width = 0.75) + #Redefine aes for each plot, fill = Type specifies the filled colour according to the Type: compressed vs uncompressed
  scale_fill_manual(values = c("orange", "steelblue"))+
  geom_point(aes(x = Frame, y=Area_um_2, group = Type, color =Replicate), size =1.2, alpha = 0.5, position = position_jitterdodge(0.2))+ #position_jitterdodge() allows to both dodge the datapoint and allign them with the boxplot and also jitter the data points
  scale_color_manual(values = c("mediumorchid", "green4"))+
  scale_y_continuous(breaks = seq(0, 4000, by=500), limits=c(0,4000))+
  stat_summary(fun = mean, geom="point", colour="darkred", aes(x=Frame, y=Area_um_2, fill=Type), size=2, #Because we didn't define aes in the first ggplot() command, we will have to define aes here for stat_summary
               position = position_dodge(width = 0.75))+
  stat_summary(fun = mean, geom="line", colour="darkred", aes(x=Frame, y=Area_um_2, group = Type), size=0.5,
               position = position_dodge(width = 0.75))

Average_area_plot_2 <- Average_area_plot + theme(axis.line.x.bottom = element_line(colour = "black", size = 0.5),
                                                 axis.line.y.left = element_line(colour = "black", size = 0.5),
                                                 axis.title.x = element_text(size=10),
                                                 axis.title.y.left = element_text(size=10),
                                                 axis.text.x = element_text(size=10),
                                                 axis.text.y = element_text(size=10))+
  labs(x="Frame", y="Average area (um^2)")
Average_area_plot_2

Average_perimeter_plot <- ggplot(data = data) +
  geom_boxplot(position=position_dodge(0.75), aes(x=Frame, y=Perimeter, fill = Type), outlier.shape = NA, width = 0.75) + 
  scale_fill_manual(values = c("orange", "steelblue"))+
  geom_point(aes(x = Frame, y=Perimeter, group = Type, color =Replicate), size =1.2, alpha = 0.5, position = position_jitterdodge(0.2))+
  scale_color_manual(values = c("mediumorchid", "green4"))+
  scale_y_continuous(breaks = seq(50, 450, by=50), limits=c(50,450))+
  stat_summary(fun = mean, geom="point", colour="darkred", aes(x=Frame, y=Perimeter, fill=Type), size=2,
               position = position_dodge(width = 0.75))+
  stat_summary(fun = mean, geom="line", colour="darkred", aes(x=Frame, y=Perimeter, group = Type), size=0.5,
               position = position_dodge(width = 0.75))

Average_perimeter_plot_2 <- Average_perimeter_plot + theme(axis.line.x.bottom = element_line(colour = "black", size = 0.5),
                                                 axis.line.y.left = element_line(colour = "black", size = 0.5),
                                                 axis.title.x = element_text(size=10),
                                                 axis.title.y.left = element_text(size=10),
                                                 axis.text.x = element_text(size=10),
                                                 axis.text.y = element_text(size=10))+
  labs(x="Frame", y="Average perimeter (um)")
Average_perimeter_plot_2

Average_circularity_plot <- ggplot(data = data) +
  geom_boxplot(position=position_dodge(0.75), aes(x=Frame, y=Circularity, fill = Type), outlier.shape = NA, width = 0.75) + 
  scale_fill_manual(values = c("orange", "steelblue"))+
  geom_point(aes(x = Frame, y=Circularity, group = Type, color =Replicate), size =1.2, position = position_jitterdodge(0.2), alpha = 0.5)+
  scale_color_manual(values = c("mediumorchid", "green4"))+
  scale_y_continuous(breaks = seq(0.1, 1, by=0.1), limits=c(0.1,1))+
  stat_summary(fun = mean, geom="point", colour="darkred", aes(x=Frame, y=Circularity, fill=Type), size=2,
               position = position_dodge(width = 0.75))+
  stat_summary(fun = mean, geom="line", colour="darkred", aes(x=Frame, y=Circularity, group = Type), size=0.5,
               position = position_dodge(width = 0.75))

Average_circularity_plot_2 <- Average_circularity_plot + theme(axis.line.x.bottom = element_line(colour = "black", size = 0.5),
                                                           axis.line.y.left = element_line(colour = "black", size = 0.5),
                                                           axis.title.x = element_text(size=10),
                                                           axis.title.y.left = element_text(size=10),
                                                           axis.text.x = element_text(size=10),
                                                           axis.text.y = element_text(size=10))+
  labs(x="Frame", y="Average circularity")
Average_circularity_plot_2

Average_AR_plot <- ggplot(data = data) +
  geom_boxplot(position=position_dodge(0.75), aes(x=Frame, y=Aspect_ratio, fill = Type), outlier.shape = NA, width = 0.75) + 
  scale_fill_manual(values = c("orange", "steelblue"))+
  geom_point(aes(x = Frame, y=Aspect_ratio, group = Type, color =Replicate), size =1.2, position = position_jitterdodge(0.2), alpha = 0.5)+
  scale_color_manual(values = c("mediumorchid", "green4"))+
  scale_y_continuous(breaks = seq(1, 6, by=0.5), limits=c(1,6))+
  stat_summary(fun = mean, geom="point", colour="darkred", aes(x=Frame, y=Aspect_ratio, fill=Type), size=2,
               position = position_dodge(width = 0.75))+
  stat_summary(fun = mean, geom="line", colour="darkred", aes(x=Frame, y=Aspect_ratio, group = Type), size=0.5,
               position = position_dodge(width = 0.75))

Average_AR_plot_2 <- Average_AR_plot + theme(axis.line.x.bottom = element_line(colour = "black", size = 0.5),
                                                               axis.line.y.left = element_line(colour = "black", size = 0.5),
                                                               axis.title.x = element_text(size=10),
                                                               axis.title.y.left = element_text(size=10),
                                                               axis.text.x = element_text(size=10),
                                                               axis.text.y = element_text(size=10))+
  labs(x="Frame", y="Average Aspect Ratio")
Average_AR_plot_2

Average_Round_plot <- ggplot(data = data) +
  geom_boxplot(position=position_dodge(0.75), aes(x=Frame, y=Round, fill = Type), outlier.shape = NA, width = 0.75) + 
  scale_fill_manual(values = c("orange", "steelblue"))+
  geom_point(aes(x = Frame, y=Round, group = Type, color =Replicate), size =1.2, position = position_jitterdodge(0.2), alpha = 0.5)+
  scale_color_manual(values = c("mediumorchid", "green4"))+
  scale_y_continuous(breaks = seq(0, 1, by=0.1), limits=c(0,1))+
  stat_summary(fun = mean, geom="point", colour="darkred", aes(x=Frame, y=Round, fill=Type), size=2,
               position = position_dodge(width = 0.75))+
  stat_summary(fun = mean, geom="line", colour="darkred", aes(x=Frame, y=Round, group = Type), size=0.5,
               position = position_dodge(width = 0.75))

Average_Round_plot_2 <- Average_Round_plot + theme(axis.line.x.bottom = element_line(colour = "black", size = 0.5),
                                             axis.line.y.left = element_line(colour = "black", size = 0.5),
                                             axis.title.x = element_text(size=10),
                                             axis.title.y.left = element_text(size=10),
                                             axis.text.x = element_text(size=10),
                                             axis.text.y = element_text(size=10))+
  labs(x="Frame", y="Average Roundness")
Average_Round_plot_2

Average_Solidity_plot <- ggplot(data = data) +
  geom_boxplot(position=position_dodge(0.75), aes(x=Frame, y=Solidity, fill = Type), outlier.shape = NA, width = 0.75) + 
  scale_fill_manual(values = c("orange", "steelblue"))+
  geom_point(aes(x = Frame, y=Solidity, group = Type, color =Replicate), size =1.2, position = position_jitterdodge(0.2), alpha = 0.5)+
  scale_color_manual(values = c("mediumorchid", "green4"))+
  scale_y_continuous(breaks = seq(0.4, 1, by=0.1), limits=c(0.4,1))+
  stat_summary(fun = mean, geom="point", colour="darkred", aes(x=Frame, y=Solidity, fill=Type), size=2,
               position = position_dodge(width = 0.75))+
  stat_summary(fun = mean, geom="line", colour="darkred", aes(x=Frame, y=Solidity, group = Type), size=0.5,
               position = position_dodge(width = 0.75))

Average_Solidity_plot_2 <- Average_Solidity_plot + theme(axis.line.x.bottom = element_line(colour = "black", size = 0.5),
                                                   axis.line.y.left = element_line(colour = "black", size = 0.5),
                                                   axis.title.x = element_text(size=10),
                                                   axis.title.y.left = element_text(size=10),
                                                   axis.text.x = element_text(size=10),
                                                   axis.text.y = element_text(size=10))+
  labs(x="Frame", y="Average Solidity")
Average_Solidity_plot_2
