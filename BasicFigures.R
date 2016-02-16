#Basic scripts for plots in R
#02/10/16
#---------------------------------------------------------------------
#clear memory
rm(list=ls())
#set working directory
setwd("~/path/to/dir")
getwd()
#read-in your data files
MyDat <- read.csv("MYDATAFILE.csv")
attach(MyDat)

#note: all words IN ALL CAPS are for you to fill in with the factor names, file name, or dependent variable (MYDV) from your own data table.
#----------------------------------------------------------------------
#plot my data: scatterplot!
names(MyDat)
attach(MyDat)
#only need to do this once per R installation:
#install.packages("plyr")
#install.packages("ggplot2")
library(plyr); library(ggplot2)
#add a column of mean of your y variable
FigDat <- ddply(MyDat, c("MYFACTOR1", "MYFACTOR2", "MYFACTOR3"), summarise, mLS   = mean(MYDV))
attach(FigDat)
names(FigDat)

#change the names of your values in one factor
FigDat$MYFACTOR3b <- factor(FigDat$MYFACTOR3, labels = c("LABEL1", "LABEL2"))

#draw the plot!
ggplot(FigDat, aes(x = MYFACTOR1, y = MYDV))+
  geom_point()+ #draws the points
  theme_bw()+ #changes background from grey to white
  geom_line(size = 1, aes(color = factor(MYFACTOR2), group = factor(MYFACTOR2)), show.legend = F)+ #adds lines to connect points from same factor level
  theme(text = element_text(size = 24), axis.text.x = element_text(angle = 45, hjust = 1))+ #changes axis label size and angle
  #relabels x and y axis titles:
  labs(y = expression(Lesion ~ Area ~ (cm^{2})), x = element_blank())+
  #adds a best fit line:
  geom_smooth(aes(group = 2), size = 2, method = "lm", se = T)

#-----------------------------------------------------------------------
#barplot with averages
#and SE bars
FigDat2 <- MyDat
names(FigDat2)

#make a new data table with just the summary statistics
FigDat2 <- ddply(FigDat2, c("MYFACTOR1", "MYFACTOR2"), summarise,
               N    = length(MYDV),
               mean = mean(MYDV),
               sd   = sd(MYDV),
               se   = sd / sqrt(N))
#again rename values in a column if needed
FigDat2$MYFACTOR3b <- factor(FigDat2$MYFACTOR3, labels = c("LABEL1", "LABEL2"))
#set the limits for the error bars
limits <- aes(ymax = mean + se, ymin = mean - se)
#draw the plot
ggplot(FigDat2, aes(x = factor(MYFACTOR1), y = mean))+
  geom_bar(stat="identity", fill="dodgerblue3")+ #draw the bars, choose color
  theme_bw()+  #change background from grey to white
  # adjust axis text size and angle for legibility
  theme(text = element_text(size=24), axis.text.x = element_text(angle = 45, hjust = 1))+
  #set x and y axis title
  #use x = element_blank() to get rid of the title
  #expression(blah ~ blah ~ blah) lets you add superscripts, latin, etc.
  labs(y = expression(Mean ~ Area ~ (cm^{2})), x = "Title")+
  #draw error bars
  geom_errorbar(limits, width=0.25)+
  #split the plot depending on MYFACTOR2
  facet_grid(.~MYFACTOR2, scales="free")

#-----------------------------------------------------------------------
#violin plot
library(ggplot2)
names(MyDat)
ggplot (data = MyDat, 
  aes(x = MYFACTOR1, y = MYDV))+
  #draw the violin plots
  #adjust smooths the plot (1 = very smooth)
  geom_violin(adjust = 0.7, scale = "width", fill="darkblue")+
  #change the background from grey to white
  theme_bw()+
  #change axis label orientation and size
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  #add a dot at the median of each violin plot
  stat_summary(fun.y="median", geom="point")+
  #change x and y axis titles
  labs(y=expression(MyLabel~alpha), x=NULL)+
  #use this to color each level of x axis separately
    scale_fill_manual(values = c("#E6F598","#E6F598", "#E6F598"))