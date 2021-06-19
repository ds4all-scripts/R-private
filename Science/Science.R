# Dir and librarys  -----------------------------------------------
rm(list = ls())
#Dir
local = "C:\\Users\\User\\Documents\\Projects\\R-private\\Science"
setwd(local)

#librarys

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

# #Functions --------------------------------------------------------------

#Percentage NA Function
Perc_NA =  function(data,
                    perc = .1){
  a = data.frame(
    "NA_Percenutal" = apply(data,
                            2,
                            function(x) {
                              1 - round(table(is.na(x)) / sum(table(is.na(x))), 2)[1]
                            }),
    "Variable" = colnames(data)
  )
  cat("Results...\n\n")
  return(a[a[1]>perc,])
}

# Correlation Matrix
panel.cor <- function(x,
                      y,
                      method = "pearson",# To define: Pearson or Spearman?
                      digits = 2,
                      cex.cor,
                      cut.off = .25)#Limit of innaporpriet correlation
{
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <-
    cor(x, y, method = method)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  test <-
    cor.test(x, y, method = method, exact = F)
  Signif <- ifelse(round(test$p.value, 3) < 0.001,
                   "( p<0.001 )",
                   ifelse(
                     round(test$p.value, 3) < 0.05,
                     paste("( p=", round(test$p.value, 3), ")"),
                     paste("( p=", round(test$p.value, 3), "*)")
                   ))
  text(.5, 0.85, paste("r = ", txt), cex = 1.5, font = 2)
  text(.5, .15, Signif)
  men.cut.off <- ifelse(abs(test$p.value) >= cut.off,
                        "INAPPROPRIATE",
                        "APPROPRIATE")
  text(
    .5,
    .45,
    men.cut.off,
    cex = .75,
    font = 2,
    col = ifelse(men.cut.off == "INAPPROPRIATE",
                 "red",
                 "black")
  )
}
panel.smooth <- function (x,
                          y,
                          col = "black",
                          bg = NA,
                          pch = 18,
                          cex = .75,
                          ...)
{

  points(
    x,
    y,
    pch = pch,
    col = col,
    bg = bg,
    cex = cex
  )
  abline(lm(y~x))
}
panel.hist <- function(x, ...)
{
  par(new = TRUE)
  hist(
    x,
    col = "light gray",
    probability = TRUE,
    axes = F,
    main = ""
  )
  lines(density(x, na.rm = TRUE), col = "black", lwd = 1)
  rug(x)
}

DS4All = function(df)
{
  pairs(
    df,
    lower.panel = panel.smooth,
    upper.panel = panel.cor,
    diag.panel = panel.hist
  )
}


#MSA graphic
MSA = function(N,
               leg = 1,
               pos = "top",
               ncol = 1,
               main = "",
               sub = "",
               las = 1)
{
  teste = as.matrix(N)
  paf.pca =  rela::paf(teste,
                       eigcrit = 1,
                       convcrit = .001)
  a = as.data.frame(paf.pca$MSA)
  a$Col = ifelse(a$MSA > .5, "darkgrey",
                 "lightgrey")

  barplot(
    a$MSA,
    names.arg = colnames(N),
    ylab = "MSA",
    col = a$Col,
    ylim = c(0, 1),
    main = main,
    sub = sub,
    las = las
  )
  abline(h = .5, lty = 2)

  if (leg == 1) {
    legend(
      pos,
      legend = c("Retained variables",
                 "Variables not retained"),
      pch  = 15,
      bty = "n",
      col = a$Col,
      ncol = 1
    )
  }
}

# Variable Selection by MSA and KMO
opt.KMO = function(N, lim = .5, graph = 1)
{
  KMO = paf(as.matrix(N))
  lim = lim
  a = which((KMO$MSA) < lim)
  len = length(a)
  if (len > 0) {
    cat(
      " We found",
      length(a),
      "variables with MSA <",
      lim,
      "\n ...",
      "\n Inappropriate variable(s) is(are):\n",
      (colnames(N)[a]),
      "\n ...\n\n"
    )

    List_subset = list()
    for (i in 1:len) {
      List_subset[[i]] = combn(a, i)
    }
    list_KMO = list()
    df.KMO =   data.frame()
    for (j in 1:len) {
      b = dim(List_subset[[j]])[2]
      for (i in 1:b) {
        df.KMO[i, j] = paf(as.matrix(N[,-List_subset[[j]][, i]]))["KMO"]
      }
      list_KMO[[j]] = df.KMO
    }
    c = length(list_KMO)
    d = list_KMO[[c]]
    e = lapply(d, which.max)
    Res = data.frame()
    List_Res = list()
    for (i in 1:len) {
      cat(
        "Removing",
        i,
        "variable(s)...\n",
        "\n the largest KMO will be",
        round(d[[i]][e[[i]]], 2),
        "\n excluding:",
        colnames(N[List_subset[[i]][, e[[i]]]]),
        "\n........\n"
      )
      Res[i, "Exc"] = i
      Res[i, "KMO"] =  round(d[[i]][e[[i]]], 2)
      List_Res[[i]] = as.list(colnames(N[List_subset[[i]][, e[[i]]]]))
      if (graph == 1) {
        print(
          plot(
            Res[, 2],
            type = "o",
            lty = 3,
            pch  = 16,
            ylim = c(0.4, .8),
            ylab = "KMO",
            xlab = "Number of variables not retained"
          )
        )
        abline(h = lim, lty = 2)
        legend(
          "topright",
          legend = c("Acceptable",
                     "Unacceptable"),
          pch  = 16,
          bty = "n",
          col = c("black", "darkgray")
        )
        a = ifelse(Res[, 2] < .5, Res[, 2], NA)
        points(a, col = "darkgrey", pch  = 16)
        locator(1)
      }


    }
    m = which.max(Res[, 2])
    par(mfrow = c(1, 2), mar = c(5.2, 4, 4, 2) * 1.25)
    MSA(N,
        leg = 2,
        sub = "A",
        las = 2)
    MSA(N[, -List_subset[[m]][, e[[m]]]],
        pos = "topright",
        sub = "B",
        las = 2)
    locator(1)
    Saida = list(
      "KMO.value" = round(Res, 1),
      "Var.Unaccep" = List_Res,
      "KMO.list" = list_KMO,
      "List_subset" = List_subset
    )
  } else  {
    Saida = "Use all variables"
  }
  return(Saida)
}

#Graphic
graph2 = function(g,
                  pos = "bottom",
                  pos.leg = "left",
                  tit.leg = " ",
                  tit.pos = "left")
{

  ggplot(g, aes(x = Sample,
                y = value,
                fill = variable, group = factor))+
    geom_bar(stat = "identity")+
    ylab("Percentual")+
    xlab("")+
    guides(fill = guide_legend(title = tit.leg,
                               label.position =
                                 pos.leg,
                               title.position =
                                 tit.pos))+
    theme_cowplot()+
    theme(legend.text = element_text(face = "italic"),
          legend.position = pos,
          panel.background = element_rect(fill =
                                            "white",
                                          colour =
                                            "black",
                                          size =
                                            .5,
                                          linetype =
                                            1))
}


ds_for_graph = function(d,
                        col.agg = 2,
                        n.fac = 1) {
  a = as.data.frame(lapply(d, is.numeric))
  A = d             # All variables
  N = d[, t(a)]     # Numeric variable  only
  C = d[,!t(a)]     # Factor variable only
  begin = 1 + ncol(C)
  mean = a = sd = c = data.frame()
  for (j in begin:NCOL(d)) {
    print(j)
    for (i in levels(d[, n.fac])) {
      print(i)
      a = data.frame("i" = tapply(d[d[,n.fac] == i, colnames(d)[j]],
                                  d[,col.agg][d[,n.fac] == i],
                                  mean))
      c = data.frame("i" = tapply(d[d[,n.fac] == i, colnames(d)[j]],
                                  d[,col.agg][d[,n.fac] == i],
                                 sd))
      mean = rbind(mean, t(a))
      sd = rbind(sd, t(c))
    }
  }
mean$Time = rep(levels(as.factor(d[,n.fac])),
                ncol(N))
mean$Parameter = rep(colnames(N),
                     each = length(levels(as.factor(d[,n.fac]))))

# to allow comparison
sd$Time = mean$Time
sd$Parameter = mean$Parameter

return(list("mean" = mean,
              "sd" = sd,
            "mean.melt" = melt(mean,id.vars = c("Time",
                                                "Parameter")),
            "sd.melt" = melt(sd,id.vars = c("Time",
                                           "Parameter"))
            )
       )


}

ds_for_graph(d1)





# ETL ---------------------------------------------------------------------
# A typical ETL process collects and refines
# different types of data, then delivers the
# data to a destiny.Three steps make up the
# ETL process and enable data to be integrated
# from source to destination. These are data
# extraction, data transformation, and data
# loading.


# 1 - Data extraction: ----------------------------------------------------

FQ <- read_excel("Datasets/FQ.xlsx")

# 2 - Data transformation: ------------------------------------------------

# 2.1 - Define object class and backup
data = as.data.frame(FQ)

# 2.2 - Cleansing (inconsistencies or missing values)

# Variable names
colnames(data) #ok!

# Remove someone?
#Variable: Yes,"Meso"!
data = data[,-2]

#Observation: No!

# Adjust variable type
str(data) # "Time" and "Sample" must be factors
data$Time = as.factor(data$Time)
data$Sample = as.factor(data$Sample)

#Checking factor levels
levels(data$Time)
data$Time = factor(data$Time, levels = c("0 h",
                                         "72 h",
                                         "168 h",
                                         "336 h",
                                         "504 h",
                                         "720 h"))

levels(data$Sample)
data$Sample = factor(data$Sample, levels = c("Raw water",
                                             "Control",
                                             "Treatment"))
# Procedure with NA:if the percentage < 10%, then impute

summary(data) # 2 variables have NA: TC and Fluoride

Perc_NA(data = data)

# Dropping "TC"
data = data[,-19]


# Impute: Fluoride (Random Forest)
df.na = data
df.imputed = randomForest::rfImpute(as.factor(df.na$Sample) ~ .,
                                    df.na,
                                    iter=5,
                                    ntree=500)
data =  cbind(data[,c(1,2)],df.imputed[,-c(1,2)] )

# 2.3 - Standardization (ok!)
# 2.4 - Deduplication (ok!)
# 2.5 - Sorting: order dataset (Column and row)

#Checking factor levels
levels(data$Time)
data$Time = factor(data$Time, levels = c("0 h",
                                         "72 h",
                                         "168 h",
                                         "336 h",
                                         "504 h",
                                         "720 h"))
levels(data$Time) = c( "Day 0",
                       "Day 7",
                      "Day 14",
                      "Day 21",
                      "Day 28",
                      "Day 35")


levels(data$Sample)
data$Sample = factor(data$Sample, levels = c("Raw water",
                                             "Control",
                                             "Treatment"))

# 2.6 - Verification:solve problems with compromised data
#       and spurious or irrelevant observations

# Raw water data is not relevant to this analysis

d1 = data[data$Sample!="Raw water",]

# Time > "Day 14"  data is not relevant to this analysis

d1 = d1[d1$Time == "Day 0" |
            d1$Time == "Day 7" |
            d1$Time == "Day 14",]


# 2. 7 - Other tasks: any additional/optional rules can
#        be applied to improve data quality.


# 3 - Data Loading:

# In this phase, extracted and transformed
# data is loaded into the end target source
# which may be a simple delimited flat file
# or a Data Warehouse depending on the
# requirement of the organization.

# 4 - Exploratory analysis
# Separate numeric variable

a = as.data.frame(lapply(d1, is.numeric))

#dataset used

# Variable "Jar" is not relevant
A = d1                # All variables
N = d1[, t(a)]        # Numeric variable  only
C = d1[, !t(a)]       # Factor variable only

# Data visualisation

# 3.1 Barplot: ---------------------------------------------------------

#Remove inexistent levels
d1$Sample = as.factor(as.character(d1$Sample))
d1$Time = as.factor(as.character(d1$Time))
d1$Time = factor(d1$Time, levels = c("Day 0",
                                     "Day 7",
                                     "Day 14")
                 )
#transform data: MaxMin

d1.t = apply(d1[,-c(1,2)],
             2,
             function (x) {(max(x)-x)/(max(x)-min(x))} )

d1.t = cbind(d1[,c(1,2)],d1.t)
ds_for_graph(d = d1.t,
             col.agg = 1,
             n.fac = 2)

#scale data

d2.t = apply(d1[,-c(1,2)],
             2,
             scale)
d2.t = cbind(d1[,c(1,2)],d2.t)
d = d1.t
d = d2.t



#Sdt desv. bar
mean.melt$sd = sd.melt$value
mean.melt$Time = factor(as.factor(mean.melt$Time),
                        levels =  c("Day 0",  "Day 7",  "Day 14"))



# Bargraph for d1.t -------------------------------------------------------
unique(mean.melt$Parameter)
g = mean.melt[mean.melt$Parameter=="Transparency" |
              mean.melt$Parameter=="Turbidity" |
              mean.melt$Parameter=="Tcolor" |
              mean.melt$Parameter=="Temperature" |
              mean.melt$Parameter=="pH"|
              mean.melt$Parameter=="Conductivity",]
a = ggplot(g, aes(x = as.numeric(Time),
                  y = value,
                  fill = variable,
                  group = variable))+
  geom_bar(stat = "identity",
           position=position_dodge())+
  scale_fill_grey(start = 0.2, end = .8)+
  geom_errorbar(aes(ymin=value-sd*.5,
                    ymax=value+sd*.5),
                width=.2,
                position=position_dodge(.9))+
  ylab("")+
  xlab("")+
  facet_grid(~Parameter,scales = "free")+
  guides(fill = guide_legend(title = "",
                             label.position = "top",
                             title.position = "left"))+
  theme(legend.text = element_text(face = "italic"),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = .5,
                                        linetype = 1),
        legend.position = "n");a

g = mean.melt[mean.melt$Parameter=="DO" |
                mean.melt$Parameter=="254um" |
                mean.melt$Parameter=="Nitrite" |
                mean.melt$Parameter=="Nitrate" |
                mean.melt$Parameter=="Orthophosphate"|
                mean.melt$Parameter=="Sulfate",]

b = ggplot(g, aes(x = Time,
                  y = value,
                  fill = variable))+
  geom_bar(stat = "identity",
           position=position_dodge())+
  scale_fill_grey(start = 0.2, end = .8)+
  geom_errorbar(aes(ymin=value,
                    ymax=value+sd),
                width=.2,
                position=position_dodge(.9))+
  ylab("")+
  xlab("")+
  facet_grid(~Parameter,scales = "free")+
  guides(fill = guide_legend(title = "",
                             label.position = "top",
                             title.position = "left"))+
  theme(legend.text = element_text(face = "italic"),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = .5,
                                        linetype = 1),
        legend.position = "n");b

g = mean.melt[mean.melt$Parameter=="Fluoride" |
                mean.melt$Parameter=="Chloride" |
                mean.melt$Parameter=="TOC" |
                mean.melt$Parameter=="DOC" |
                mean.melt$Parameter=="DTC",]

c = ggplot(g, aes(x = Time,
                  y = value,
                  fill = variable))+
  geom_bar(stat = "identity",
           position=position_dodge())+
  scale_fill_grey(start = 0.2, end = .8)+
  geom_errorbar(aes(ymin=value,
                    ymax=value+sd),
                width=.2,
                position=position_dodge(.9))+
  ylab("")+
  xlab("")+
  facet_grid(~Parameter,scales = "free")+
  guides(fill = guide_legend(title = "",
                             label.position = "top",
                             title.position = "left"))+
  theme(legend.text = element_text(face = "italic"),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = .5,
                                        linetype = 1),
        legend.position = "right");c

d=plot_grid(a,b,c, ncol = 1)

ggsave(filename = "graphics/overview.png",
       plot = d,
       device = "png",
       height = 9,
       width = 13,
       units = "in",
       dpi = "retina" )

# Line graph for d2.t ------------------------------------------------------

g = mean.melt[mean.melt$Parameter=="Transparency" |
                mean.melt$Parameter=="Turbidity" |
                mean.melt$Parameter=="Tcolor" |
                mean.melt$Parameter=="Temperature" |
                mean.melt$Parameter=="pH"|
                mean.melt$Parameter=="Conductivity",]

a = ggplot(g, aes(x = (Time),
                  y = value,
                  fill = variable,
                  group = variable))+
  geom_line(aes(linetype=variable))+geom_point()+
  scale_fill_grey(start = 0.2, end = .8)+
  ylab("")+
  xlab("")+
  facet_grid(~Parameter,scales = "free")+
  guides(fill = guide_legend(title = "",
                             label.position = "top",
                             title.position = "left"))+
  theme(legend.text = element_text(face = "italic"),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = .5,
                                        linetype = 1),
        legend.position = "n");a

g = mean.melt[mean.melt$Parameter=="DO" |
                mean.melt$Parameter=="254um" |
                mean.melt$Parameter=="Nitrite" |
                mean.melt$Parameter=="Nitrate" |
                mean.melt$Parameter=="Orthophosphate"|
                mean.melt$Parameter=="Sulfate",]

b = ggplot(g, aes(x = (Time),
                  y = value,
                  group = variable))+
  geom_line(aes(linetype=variable))+geom_point()+
  scale_fill_grey(start = 0.2, end = .8)+
  ylab("")+
  xlab("")+
  facet_grid(~Parameter,scales = "free")+
  guides(fill = guide_legend(title = "",
                             label.position = "top",
                             title.position = "left"))+
  theme(legend.text = element_text(face = "italic"),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = .5,
                                        linetype = 1),
        legend.position = "n");b

g = mean.melt[mean.melt$Parameter=="Fluoride" |
                mean.melt$Parameter=="Chloride" |
                mean.melt$Parameter=="TOC" |
                mean.melt$Parameter=="DOC" |
                mean.melt$Parameter=="DTC",]

c = ggplot(g, aes(x = (Time),
                  y = value,
                  group = variable))+
  geom_line(aes(linetype=variable))+geom_point()+
  scale_fill_grey(start = 0.2, end = .8)+
  ylab("")+
  xlab("")+
  facet_grid(~Parameter,scales = "free")+
  guides(type = guide_legend(title="my awesome title"))+
  theme(legend.text = element_text(face = "italic"),
        legend.title = element_text(face = "bold"),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = .5,
                                        linetype = 1),
        legend.position = "right");c

d=plot_grid(a,b,c, ncol = 1)

ggsave(filename = "graphics/overview.png",
       plot = d,
       device = "png",
       height = 9,
       width = 13,
       units = "in",
       dpi = "retina" )

# Factor Analysis ---------------------------------------------------------


