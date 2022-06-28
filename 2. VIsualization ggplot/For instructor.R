###############################################
### TREC workshop ###### ggplot2 visualization
###############################################
ls(); rm(list=ls())
library(tidyverse)
library(ggplot2)
library(patchwork) #Dual y axis
library(ggpubr) #make multiple graphs

setwd("F:/XTBG/club meeting/R_Workshop_202109/2. VIsualization ggplot")

###############################################
############ Basic ggplot2 coding #############
###############################################

iris

ggplot()+
  geom_point(data=iris, aes(x=Petal.Length, y=Petal.Width))

#try to change x and y
ggplot()+
  geom_point(data=iris, aes(x=, y=))

ggplot()+
  geom_point(data=iris, aes(x=, y=, color=))

ggplot()+
  geom_point(data=iris, aes(x=, y=), color="red")


#let play with other graph components
ggplot()+
  geom_point(data=iris, aes(x=Species, y=Petal.Width, color=Species), 
                        shape=4, size=1.5)+
  ggtitle("This is for title")+
  ylab("This is for y")+
  xlab("This is for x")+
  ylim(0,5)


#save
ggsave(filename = "~/Desktop/test.png", width=16, height=11, unit="cm", dpi=500)






###############################################
######### Dealing with ggplot2 layers #########
###############################################

##  Help Arisa ###
Arisa=data.frame(Age=c(4,8,12,16,20), Height=c(100,126,148,160,164))

ggplot()+
  geom_text(data=Arisa, aes(x=5, y=110, label="Xiao Arisa"), color="orange", size=5)+
  geom_point(data=Arisa, aes(x=Age, y=Height), size=3)+
  geom_line(data=Arisa, aes(x=Age, y=Height), size=2)+
  geom_text(data=Arisa, aes(x=18, y=163, label="Arisa Jiejie"), color="orange", size=5)




###################
##  Dual Y-axis ##
###################
Mat=data.frame(Age=c(4,8,12,16,20,24), Height=c(103,127,147,162,174,178), Weight=c(17,25,37,45,55,60))

ggplot()+
  geom_line(data=Mat, aes(Age, Height))+
  geom_line(data=Mat, aes(Age, Weight), col="blue")+   
  geom_text(data=Mat, aes(x=max(Age), y=max(Height)+5, label="Height=178"))+
  geom_text(data=Mat, aes(x=max(Age), y=max(Weight)+5, label="Weight=60"), col="blue")+
  ylab("universal unit") + xlim(4,26)


ggplot()+
  geom_line(data=Mat, aes(Age, Height))+
  geom_line(data=Mat, aes(Age, Weight), col="blue")+
  geom_text(data=Mat, aes(x=max(Age), y=max(Height)+5, label="Height=178"))+
  geom_text(data=Mat, aes(x=max(Age), y=max(Weight)+5, label="Weight=60"), col="blue")+
  
  scale_y_continuous(name = "first axis",
    sec.axis = sec_axis( trans=~., name="sec axis"))+
  
  xlim(4,26)



###################
##  Lijiang meteorology
###################
Li=read.csv("2013-2020_LijiangMet.csv")

summary(Li)
Li$Date=as.Date(Li$Date, "%Y/%m/%d")
Li$Month=format(Li$Date, format = "%m")
Li= Li %>% group_by(Month) %>% summarise_all(mean, na.rm=T)
Li$Site="Lijiang"
View(Li)

ggplot(data=Li, aes(group=Site))+
  geom_bar(aes(Month,Precipitation), stat = 'identity', fill="steelblue")+
  geom_point(aes(Month, Tair), col="orange", size=3)+
  geom_line(aes(Month,Tair), col="orange", alpha=0.5, size=3)+
  
  scale_y_continuous(name = "??",
    sec.axis = sec_axis( trans=~., name="??"))




ggplot(data=Li, aes(group=Site))+
  geom_bar(aes(Month,Precipitation/20), stat = 'identity', fill="steelblue")+
  geom_point(aes(Month, Tair), col="orange", size=3)+
  geom_line(aes(Month,Tair), col="orange", alpha=0.5, size=3)+
  
  scale_y_continuous(name = "Air temperature (C)",
                     sec.axis = sec_axis( trans=~.*20, name="Precipitation (mm)"))





ggplot(data=Li, aes(group=Site))+
  geom_bar(aes(Month,Precipitation/20, fill="Precipitation"), stat = 'identity')+
  geom_point(aes(Month, Tair, col="Air temperature"), size=3)+
  geom_line(aes(Month,Tair, col="Air temperature"), alpha=0.5, size=3)+
  scale_fill_manual(values="steelblue")+
  scale_color_manual(values="orange")+
  theme(legend.title = element_blank(), legend.spacing.y = unit(0, 'cm'),)+
  scale_y_continuous(name = "Air temperature (C)",
                     sec.axis = sec_axis( trans=~.*20, name="Precipitation (mm)"))



###################
##  Error bars
###################
ggplot(data=iris , #mean data 
       aes(x=Petal.Length, y=Petal.Width, col=Species))+
  geom_point(size=3)+
  geom_point(data=iris, aes(x=Petal.Length, y=Petal.Width, col=Species), alpha=0.5)


##set error function
se=function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

CI_t=function (x, ci = 0.95){
  qt(ci + (1 - ci)/2, df = length(x) - 1) * sd(x)/sqrt(length(x))
}

CI_z=function (x, ci = 0.95){ 
  abs(qnorm((1-ci)/2)) * sd(x)/sqrt(length(x))
}

test=c(1,2,3,4,5,6,7,8,9,10)
sd(test)
se(test)
CI_t(test)
CI_z(test)



ggplot(data=iris %>% group_by(Species) %>% 
         summarise(Petal.Length_Mean = mean(Petal.Length),
                   Petal.Width_Mean  = mean(Petal.Width),
                   Petal.Length_CI   = CI_t(Petal.Length),
                   Petal.Width_CI    = CI_t(Petal.Width)),
       aes(x=Petal.Length_Mean, y=Petal.Width_Mean, col=Species))+
  
  geom_point(size=3)+
  geom_point(data=iris, aes(x=Petal.Length, y=Petal.Width, col=Species), alpha=0.4)+
  geom_errorbar(aes(ymin=Petal.Width_Mean - Petal.Width_CI,
                    ymax=Petal.Width_Mean + Petal.Width_CI), 
                width=0, size=1)+
  geom_errorbarh(aes(xmax = Petal.Length_Mean + Petal.Length_CI, 
                     xmin = Petal.Length_Mean - Petal.Length_CI), 
                 height=0, size=1)




ggplot(data=iris %>% group_by(Species) %>% 
         summarise(Petal.Length_Mean = mean(Petal.Length),
                   Petal.Width_Mean  = mean(Petal.Width),
                   Petal.Length_CI   = CI_t(Petal.Length),
                   Petal.Width_CI    = CI_t(Petal.Width)),
       aes(x=Species, y=Petal.Width_Mean, col=Species))+
  
  geom_point(data=iris, aes(x=Species, y=Petal.Width), col='black', alpha=0.1)+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=Petal.Width_Mean - Petal.Width_CI, 
                    ymax=Petal.Width_Mean + Petal.Width_CI), 
                width=0, size=1)


ggplot(data=iris %>% group_by(Species) %>%
         summarise(Petal.Length_Mean = mean(Petal.Length),
                   Petal.Width_Mean  = mean(Petal.Width),
                   Petal.Length_CI   = CI_t(Petal.Length),
                   Petal.Width_CI    = CI_t(Petal.Width)),
       aes(x=Species, y=Petal.Width_Mean, fill=Species))+
  
  geom_bar(stat = 'identity')+
  geom_errorbar(aes(ymin=Petal.Width_Mean - Petal.Width_CI,
                    ymax=Petal.Width_Mean+Petal.Width_CI), width=0.3)



ggplot()+
  geom_boxplot(data=iris,aes(x=Species, y=Petal.Width, fill=Species))+
  geom_point(data=iris %>% group_by(Species) %>% summarise_all(mean),
             aes(x=Species, y=Petal.Width), shape=4, size=2)





###################
#Model coefficient
#Confident interval
###################
model=data.frame(Coefficient=c(0.2, 0.1, 0.02), 
                 CI=c(0.1,0.2,0.2),
                 Parameter=c("X1","X2","X3"))

ggplot(data=model)+geom_point(aes(Parameter, Coefficient))+
  geom_errorbar(aes(x=Parameter, y=Coefficient,ymin=Coefficient-CI, ymax=Coefficient+CI), width=0)+
  geom_hline(yintercept = 0)


## what about iris model?
ggplot(data=iris, aes(x=Petal.Length, y=Petal.Width)) + geom_point() #+geom_smooth(method='lm')

#model=lm(y~x, data)
model=lm(Petal.Width~Petal.Length, data=iris)

model

prediction_i <- predict(object = model,
                        newdata = iris,
                        interval = "prediction") %>% as_tibble() %>% 
                        rename_at(vars(lwr,upr), ~paste0(., "_pi"))
confidence_i <- predict(object = model,
                        newdata = iris,
                        interval = "confidence") %>% as_tibble() %>% 
                        rename_at(vars(lwr,upr), ~paste0(., "_ci"))

intervals <- iris %>% 
  bind_cols(prediction_i,
            confidence_i[2:3])

ggplot() + 
  geom_point(data = iris, aes(x=Petal.Length, y=Petal.Width), alpha = 0.5) +
  geom_line(data = intervals, 
            aes(Petal.Length, fit, color = "Regression Fit"), size = 1) +
  geom_line(data = intervals, 
            aes(Petal.Length, lwr_pi, color = "Prediction Interval"), 
            linetype = 2, size = 1) +
  geom_line(data = intervals, 
            aes(Petal.Length, upr_pi, color = "Prediction Interval"), 
            linetype = 2, size = 1) + 
  geom_line(data = intervals, 
            aes(Petal.Length, lwr_ci, color = "Confidence Interval"), 
            linetype = 2, size = 1) + 
  geom_line(data = intervals, 
            aes(Petal.Length, upr_ci, color = "Confidence Interval"), 
            linetype = 2, size = 1) #+geom_smooth(data=iris, aes(x=Petal.Length, y=Petal.Width) ,method='lm')
  







###############################################
######## Dealing with multiple graphs #########
###############################################

Traits=read.csv("Traits.csv")

Traits$Site=as.factor(Traits$Site)
Traits$Species=as.factor(Traits$Species)
Traits$Year=as.factor(Traits$Year)
summary(Traits)


ggplot(data=Traits)+
  geom_boxplot(aes(x=Species, y=Trait1))

## 1 grouping
ggplot(data=Traits)+
  geom_boxplot(aes(x=Species, y=Trait1))+
  facet_wrap(~Site, scales = "free") #ncol=4
#+coord_flip()

ggplot(data=Traits)+
  geom_boxplot(aes(x=Species, y=Trait1))+
  facet_grid(~Site, scales = "free") 

## 2 grouping
ggplot(data=Traits)+
  geom_boxplot(aes(x=Species, y=Trait1))+
  facet_wrap(Site~Year, scales = "free")

ggplot(data=Traits)+
  geom_boxplot(aes(x=Species, y=Trait1))+
  facet_grid(Site~Year, scales = "free")


## 3 grouping
ggplot(data=Traits)+
  geom_point(aes(x=Trait2, y=Trait1))



ggplot(data=Traits)+
  geom_point(aes(x=Trait2, y=Trait1))+
facet_wrap(Site~Year, scales = "free")

ggplot(data=Traits)+
  geom_point(aes(x=Trait2, y=Trait1))+
  facet_wrap(Site~Year+Species, scales = "free")

ggplot(data=Traits)+
  geom_point(aes(x=Trait2, y=Trait1))+
  facet_grid(Site~Year+Species, scales = "free")


###ggarange
G1=ggplot()+
  geom_density(data=iris, aes(Petal.Length, fill=Species))+
  xlim(0,7.5)

G2=ggplot()+
  geom_point(data=iris, aes(x=Petal.Length, y=Petal.Width, color=Species))+
  xlim(0,7.5)+ylim(0,2.6)

G3=ggplot()+
  geom_boxplot(data=iris, aes(x=Species, y=Petal.Width, fill=Species))+
  ylim(0,2.6)

ggarrange(G1,NA,G2,G3,
          common.legend = T, legend = "right")


ggarrange(G1+xlab(NULL), NA, G2, G3+ylab(NULL),
          common.legend = T, legend = "right")


annotate_figure(figure, bottom = textGrob(expression('Tcrit ('*~degree*C*')'), gp = gpar(cex = 1)))









theme_classic() + theme(panel.background = element_rect(fill = NA))



theme(
  axis.text = element_text(color = "white",size=13),
  axis.title = element_text(color = "white",size=15),
  panel.background = element_rect(fill = "transparent"), # bg of the panel
  plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
  #panel.grid.major = element_line(color = "#3D2D11"), # get rid of major grid
  panel.grid.minor = element_blank(), # get rid of minor grid
  #legend.background = element_rect(fill = "transparent"), # get rid of legend bg
  #legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  legend.position = 'none',legend.title = element_blank(), legend.spacing.y = unit(0, 'cm'),legend.text = element_text(color="white")
)


ggsave(filename = "~/Desktop/test.png",  bg = "transparent", width=16, height=11, unit="cm", dpi=500)


theme(
  axis.text = element_text(color = "white",size=13),
  axis.title = element_text(color = "white",size=15),
  panel.background = element_rect(fill = "transparent"), # bg of the panel
  plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
  #panel.grid.major = element_line(color = "#3D2D11"), # get rid of major grid
  panel.grid.minor = element_blank(), # get rid of minor grid
  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
  legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
)+ylab("Height")+xlab("Age")


ggplot(data=Arisa)+
  geom_line(aes(x=Age, y=Height), col="pink", size=2.5, alpha=0.5)+
  geom_point(aes(x=Age, y=Height), size=7, col="pink")+
  geom_point(aes(x=Age, y=Height), size=4, col="white", alpha=1)+
  geom_text(aes(x=20, y=172, label="Arisa Jiejie"), col="pink", size=6)+
  
  theme(
    axis.text = element_text(color = "white",size=13),
    axis.title = element_text(color = "white",size=15),
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    #panel.grid.major = element_line(color = "#3D2D11"), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )+ylab("Height")+xlab("Age")+xlim(4,24)+ylim(100,174)