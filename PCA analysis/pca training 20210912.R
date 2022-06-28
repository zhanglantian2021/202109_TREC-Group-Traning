#install.packages("FactoMineR")
#install.packages("factoextra")
#install.packages("corrplot")

library("FactoMineR") # Do PCA analysis
library("factoextra") #draw figures based on PCA
library("corrplot")

#read the data
limestone<- read.table("pca training 2021.csv",header=T,sep=",")# read the data
head(limestone)
tail(limestone)
summary(limestone)
dim(limestone)

limestone$Phenology<-factor(limestone$Phenology,levels=c(1,2),
                             labels=c("Deciduous","Evergreen"))
str(limestone)

########chose a subset of the whole dataset
test <- limestone[,c(4:6)]
head(test)

##firstly we need to centre and scale the data
test$SLA
mean(test$SLA)
sd(test$SLA)
test$SLA<- test$SLA-156.6429 #center the data
test$SLA <- test$SLA/57.57569  #scale the data

mean(test$SLA) #close to zero
sd(test$SLA) # is 1


#scale the data
 
test$SLA <- scale(test$SLA,scale = T,center = T)#mean is 0, sd is 1.
test$Nm <- scale(test$Nm,scale = T,center = T)
test$Pm <- scale(test$Pm,scale = T,center = T)

# get the covariance matrix
cor(test) #correlation matrix
cov(test) #covariance matrix, they are indentical

##calculate the eigen value and vector
eigen(cor(test))
eigen(cov(test)) #they are also the same

my_pca1 <- PCA(test, scale.unit = TRUE,
              ncp = 3, graph = TRUE)
summary(my_pca1)
my_pca1$eig


#############  PCA ##################
# choose the data for PCA, I log transformed the data
# correlation analysis

cor(limestone[,4:12]) #for all the variables

# making the corrplot
corrplot(cor(limestone[,4:12]),sig.level = 0.05,insig = c( "blank"))
?corrplot()
# PCA analysis 
#?PCA()
#?fviz_pca_var()


my_pca <- PCA(limestone[,4:12], scale.unit = TRUE,
                  ncp = 3, graph = TRUE) # PC 1 45.43%, PC2 26.62%

?PCA
print(my_pca) # to show the results of PCA

my_pca$eig #the eigenvalues

#the results of varialbe PCA
my_pca$var$coord # coordinates of the variable
my_pca$var$cor #correlation efficiencts, equal to my_pca$var$coord
my_pca$var$cos2  # Quality of representation
my_pca$var$contrib  # Contributions to the PCs
my_pca$ind

#the results of species PCA
my_pca$ind$coord #coordinates of the species
my_pca$ind$cos2 # Quality of representation
my_pca$ind$contrib  #Contributions to the PCs


#draw screen plot and to see the contribution of each PCAs
fviz_screeplot(my_pca,addlabels = TRUE, ylim = c(0, 50))
# Total cos2 of variables on Dim.1 and Dim.2
# A high cos2 indicates a good representation 
#  of the variable on the principal component.
fviz_contrib(my_pca, choice = "var", axes = 1) 
fviz_contrib(my_pca, choice = "var", axes = 2)
# for cos2 of species
fviz_contrib(my_pca, choice = "ind", axes = 1) 
fviz_contrib(my_pca, choice = "ind", axes = 2)

### another way to see cos2
par(mfrow=c(1,1))
corrplot(my_pca$var$cos2, is.corr=FALSE)
corrplot(my_pca$ind$cos2, is.corr=FALSE)

# to plot variable PCA
fviz_pca_var(my_pca, col.var = "black",repel = T) # repel =T no overlap
# to plot species PCA
fviz_pca_ind(my_pca, col.var = "black",
             col.ind = limestone$Phenology,repel = T)


#Species PCA figures
species.tiff<- fviz_pca_ind(my_pca,
                            geom.ind = "point", # show points only (nbut not "text")
                            col.ind = limestone$Phenology, # color by groups
                            palette = c("darkgrey", "black"),
                            addEllipses = F, # Concentration ellipses
                            legend.title = "Groups",mean.point = FALSE,axes.linetype = "solid", 
                            text = element_text(size = 7.5),
                            axis.title = element_text(size =7.5),
                            axis.text = element_text(size = 7.5),title="Species - PCA",
                            font.main=10,font.legend=10
)
species.tiff
# Produce high resolution figure.
tiff(file = "species.tiff", width = 1200, height = 800, units = "px", res = 300)
species.tiff
dev.off() # This will make a file that is 3200 x 3200 pixels, with an 800
# resolution gives you 3200/800 = 4 inches.
# to plot variable 
traits.tiff <- fviz_pca_var(my_pca, geom = c("point", "text"),
                            col.var = "black",circlesize=0.5, 
                            pointsize=1, labelsize = 2,repel = T,font.main=10)+# repel =T no overlap
                                  theme(text = element_text(size = 7.5),
                                     axis.title = element_text(size =10),
                                      axis.text = element_text(size = 10))
traits.tiff

tiff(file = "traits.tiff", width = 1200, height = 800, units = "px", res = 300)
traits.tiff
dev.off() 

