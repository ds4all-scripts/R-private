# Dir, libraries and dataset -----------------------------------------------

#Dir
local  ="C:\\Users\\User\\Documents\\Science"
setwd(local)

#Libraries
library(readxl)
library(reshape2)
library(ggplot2)
library(cowplot)
library(Factoshiny)
library(FactoMineR)
library(factoextra)
library(tidyverse)
library(tidyr)
library(rela)
library(psych)

#dataset
dataset = read_excel("PCA.xlsx",sheet = "PCA")
d = as.data.frame(dataset)



# Exploratory data analysis -----------------------------------------------

#CHECK MISSING DATA

summary(d)

#       TC = 7 (delete this variable)
d = d[,-20]

# Fluoride = 1 (imputation)
d.na = randomForest::rfImpute(as.factor(d[,2]) ~ .,
                                    d[,-c(1:3)],
                                    iter=5,
                                    ntree=500)

d = cbind(d[,1,2],d.na)
colnames(d)[1:2] = c("Time", "Treatment")



# #Analysis ---------------------------------------------------------------
A = d[d$Treatment!="Raw water",-c(1,2,19)]
# The plot is also known as variable correlation plots or biplot.
# It shows the relationships between all variables.
# It can be interpreted as follow:
# -> Positively correlated variables are grouped together.
# -> Negatively correlated variables are positioned on opposite
#    sides of the plot origin (opposedquadrants).
# -> Orthogonal variables mean no correlation
# -> The distance between variables and the origin measures the
#    quality of the variables on the factor map.
# -> Variables that are away from the origin are well represented
#    on the factor map;
# The quality of representation of the variables on factor map is called cos2
# (square cosine, squared coordinates)

# Exc.  "Raw water" and "DTC"


res.pca <- PCA(A, graph = FALSE)
print(res.pca)

#Eigenvalues / Variances
eig.val <- get_eigenvalue(res.pca)
eig.val

a =fviz_eig(res.pca,
            addlabels = TRUE,
            ylim = c(0, 50))


biplot = fviz_pca_biplot(res.pca,
                        # col.ind = d$Treatment,
                         addEllipses = F,ellipse.type = "confidence",
                         label = "var", # hide individual labels
                         palette = "npg",
                         title = "",
                         col.var = "black",
                         alpha.var =  "cos2",
                         legend.title = "Concentration")+theme_cowplot()
biplot


res.hcpc <- HCPC(res.pca, graph = FALSE)

plot(res.hcpc, choice = "3D.map",)



