# 1 - Dir and librarys  -----------------------------------------------
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

# 2 - Functions --------------------------------------------------------------

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
    for (i in levels(d[, n.fac])) {
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
mean.melt = reshape2::melt(mean,id.vars = c("Time",
                                              "Parameter"))
sd.melt = reshape2::melt(sd,id.vars = c("Time",
                                          "Parameter"))
grap.melt = cbind(mean.melt,sd.melt[,"value"])
colnames(grap.melt)[NCOL(grap.melt)]="sd"
return(list("mean" = mean,
              "sd" = sd,
            "mean.melt" = mean.melt,
            "sd.melt" = sd.melt,
            "grap.melt" = grap.melt
            )
       )


}

# bargraph
graph_bar = function(g, leg = "n") {
  library(ggplot2)
  library(cowplot)
  ggplot(g,
         aes(
           x = (Time),
           y = value,
           fill = variable,
           group = variable
         )) +
    geom_bar(stat = "identity",
             position = position_dodge()) +
    scale_fill_grey(start = 0.2, end = .8) +
    geom_errorbar(
      aes(ymin = value,
          ymax = value + sd),
      width = .2,
      position = position_dodge(.9)
    ) +
    ylab("") +
    xlab("") +
    facet_grid( ~ Parameter, scales = "free") +
    guides(fill = guide_legend(
      title = "",
      label.position = "top",
      title.position = "left"
    )) +
    theme(
      legend.text = element_text(face = "italic"),
      panel.background = element_rect(
        fill = "white",
        colour = "black",
        size = .5,
        linetype = 1
      ),
      legend.position = leg
    )
}


# line graph
graph_line = function(g, leg = "n"){

ggplot(g, aes(x = (Time),
              y = value,
              group = variable))+
  geom_line(aes(linetype=variable))+geom_point()+
  scale_fill_grey(start = 0.2, end = .8)+
  ylab("")+
  xlab("")+
  facet_grid(~Parameter,scales = "free")+
  theme_bw()+
  theme(legend.text = element_text(face = "italic"),
        legend.title=element_blank(),
        legend.position = leg)
}


# 3 - ETL ---------------------------------------------------------------------
# A typical ETL process collects and refines
# different types of data, then delivers the
# data to a destiny.Three steps make up the
# ETL process and enable data to be integrated
# from source to destination. These are data
# extraction, data transformation, and data
# loading.


# 3.1 - Data extraction: ----------------------------------------------------

FQ <- read_excel("Datasets/FQ.xlsx")

# 3.2 - Data transformation: ------------------------------------------------

# 3.2.1 - Define object class and backup
data = as.data.frame(FQ)

# 3.2.2 - Cleansing (inconsistencies or missing values)

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

# .3.3 - Standardization (ok!)
# .3.4 - Deduplication (ok!)
# .3.5 - Sorting: order dataset (Column and row)
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

# 3.6 - Verification:solve problems with compromised data
#       and spurious or irrelevant observations

# Raw water data is not relevant to this analysis

d1 = data[data$Sample!="Raw water",]

# Time > "Day 14"  data is not relevant to this analysis

d1 = d1[d1$Time == "Day 0" |
            d1$Time == "Day 7" |
            d1$Time == "Day 14",]

d1$Time = factor(as.factor(as.character(d1$Time)),
                 levels = c("Day 0",
                            "Day 7", "Day 14" ))
d1$Sample = factor(as.factor(as.character(d1$Sample)),
                 levels = c("Control",
                            "Treatment"))



# 3. 7 - Other tasks: any additional/optional rules can
#        be applied to improve data quality.


# 4 - Data Loading:

# In this phase, extracted and transformed
# data is loaded into the end target source
# which may be a simple delimited flat file
# or a Data Warehouse depending on the
# requirement of the organization.

# 5 - Exploratory analysis
# Separate numeric variable

a = as.data.frame(lapply(d1, is.numeric))

#dataset used

A = d1                # All variables
N = d1[, t(a)]        # Numeric variable  only
C = d1[, !t(a)]       # Factor variable only

# 6 - Data visualisation

# 6.1 - Barplot MaxMin ---------------------------------------------------------

#transform data: MaxMin

d1.t = apply(A[,c(3,4,7,9,17,18)],
             2,
             function (x) {(max(x)-x)/(max(x)-min(x))} )

d1.t = cbind(d1[,c(1,2)],d1.t)

g = ds_for_graph(d = d1.t,
                 col.agg = 2,
                 n.fac = 1)[[5]];g
g$Time  = factor(as.factor(g$Time),
                 levels = c("Day 0",
                            "Day 7",
                            "Day 14"))

unique(g$Parameter)

i = g[g$Parameter=="Transparency" |
      g$Parameter=="Turbidity" |
      g$Parameter== "pH" |
      g$Parameter=="DO",];i

a = graph_bar(i);a

i = g[g$Parameter=="TOC" |
      g$Parameter=="DOC",];i

b = graph_bar(i,"right");b

i = g[g$Parameter=="Fluoride" |
      g$Parameter=="Chloride" |
      g$Parameter=="TOC" |
      g$Parameter=="DOC" |
      g$Parameter=="DTC",];i

c = graph_bar(i,"right");c

e=plot_grid(a,b, ncol = 1);e

ggsave(filename = "graphics/overview_bargraph_pca.png",
       plot = e,
       device = "png",
       height = 9,
       width = 13,
       units = "in",
       dpi = "retina" )

# 6.2 - Line graph for d2.t ------------------------------------------------------

d2.t = apply(A[,c(3,4,7,9,17,18)],
             2,
              scale)

d2.t = cbind(d1[,c(1,2)],d2.t)

g = ds_for_graph(d = d2.t,
                 col.agg = 2,
                 n.fac = 1)[[5]];g
g$Time  = factor(as.factor(g$Time),
                 levels = c("Day 0",
                            "Day 7",
                            "Day 14"))

unique(g$Parameter)
h = g[g$Parameter=="Transparency" |
      g$Parameter=="Turbidity" |
      g$Parameter== "pH" |
      g$Parameter=="DO",];h

a = graph_line(h);a

h = g[g$Parameter=="TOC" |
        g$Parameter=="DOC",];h

b = graph_line(h,leg = "right");b

h = g[g$Parameter=="Fluoride" |
      g$Parameter=="Chloride" |
      g$Parameter=="TOC" |
      g$Parameter=="DOC" |
      g$Parameter=="DTC",]
c = graph_line(h,leg = "right");c

d=plot_grid(a,b, ncol = 1);d

ggsave(filename = "graphics/overview_linegraph_line.png",
       plot = d,
       device = "png",
       height = 9,
       width = 13,
       units = "in",
       dpi = "retina" )

# 7 - Factor Analysis ---------------------------------------------------------

# 7.1 - Summary of descriptive statistics -----------------------------------

A$Time = factor(as.factor(as.character(A$Time)),
                levels = c("Day 0", "Day 7", "Day 14"))
A$Sample = as.factor(as.character(A$Sample))

for (i in 3:dim(A)[2]) {
  for (j in 1:3) {
    cat("\n Variable",
        colnames(A)[i],
        "... Samples:",
        levels(A$Date)[j], "\n")
    print(tapply(A[A$Time == levels(A$Time)[j], i],
                 A[A$Time == levels(A$Time)[j], 2],
                 summary))
    cat(" Std Dev...\n")
    print(tapply(A[A$Time == levels(A$Time)[j], i],
                 A[A$Time == levels(A$Time)[j], 2],
                 sd))
    cat("\n .....................................................\n")
  }
}

# Corplot
DS4All(N)

# Some variables have very low correlations for EFA.
# Therefore, these variables must be selected

# Adequacy of the dataset  ------------------------------------------------------

#Kmo
KMO(cor(N))
# For the sample to be considered satisfactory, KMO> .5
# below 0.50 are 'unacceptable' (Hutcheson & Sofroniou 1999).

#check the MSA of the original base
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) * 1.5)
MSA(N)

#Conclusion: it is necessary to select variables
#to adapt the dataset to FA

# Verificando o melhor KMO
# Procedure for selecting variables based on MSA
png(
  "Var_select.png",
  width = 11,
  height = 6.5,
  res = 500,
  units = "in"
)

# Barros et al.(2016)
N = N[,c("Transparency","Turbidity", "Tcolor",
         "Temperature","pH", "DO","254um",
         "Nitrate", "Orthophosphate", "TOC","DOC")]
N = N[,c(1,2,5,7,8,10,11,17)]
kmo.max = opt.KMO(N)
dev.off()
colnames(N)

N.3 = N[,-c(3,7,5,11)]
N.4 = N[,-c(3,7,5,9,11)]

# Based on previous results, one should remove
# Variables: 3 (254 nm) and 7 (Sal)

# New kMO
max(kmo.max$KMO.value$KMO)

#Bartlett test
bartlett.test(N.3)
bartlett.test(N.4)

#det(cor(dataset))>O
det(cor(N.3)) #OK, The dataset is ready to EFA
det(cor(N.4)) #OK, The dataset is ready to EFA

# 3.5 Selection of No. of Factors  --------------------------------------------------------

# Parallel analyis is an alternative technique that compares the
# scree of factors of the observed data with that of a random data
# matrix of the same size as the originall in order to obtain the
# number of factors that must be used in the EFA.

A = N.3
A= N.4
parallel = fa.parallel(
  cor(A),
  n.obs = 18,
  fa = "fa",
  n.iter = 500,
  fm = "ml",
  main = "",
  show.legend = F
)


#Create data frame from observed eigenvalue data
obs = data.frame(parallel$fa.values)
obs$type = c('Observed Data')
obs$num = c(row.names(obs))
obs$num = as.numeric(obs$num)
colnames(obs) = c('eigenvalue', 'type', 'num')

# simulated
Simulated = apply(parallel$values, 2, function(x)
  quantile(x, .95))

#Create data frame  with simulated eigenvalue data
sim = data.frame(Simulated[11:20])
sim$type = c('Simulated Data')
sim$num = c(row.names(sim))
sim$num = 1:10
colnames(sim) = c('eigenvalue', 'type', 'num')

#Merge the two data frames (obs and sim)
eigendat = rbind(obs, sim)

#Graph Previous settings
apatheme = theme_cowplot() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    text = element_text(family = 'Arial'),
    legend.title = element_blank(),
    legend.position = c(.7, .8),
    axis.line.x = element_line(color = 'black'),
    axis.line.y = element_line(color = 'black')
  )


p = ggplot(eigendat,
           aes(x = num,
               y = eigenvalue,
               shape = type)) +
  geom_line() +
  geom_point(size = 4) +
  scale_y_continuous(name = 'Eigenvalue') +
  scale_x_continuous(name = 'Factor Number',
                     breaks = min(eigendat$num):max(eigendat$num)) +
  scale_shape_manual(values = c(16, 1)) +
  geom_vline(xintercept = parallel$nfact, linetype = 'dashed') +
  apatheme;p

ggsave(
  filename = "graphics/parallel_A4.png",
  device = "png",
  plot = p,
  width = 8,
  height = 4,
  units = "in",
  scale = 1.25
)

# Loading factors, fit and others -----------------------------------------

fm = c("ml",#) # This factoring method was the best
"mle",
"minres",
"uls",
"ols",
"wls",
"pa",
"minrank",
"old.min",
"alfa")

rt = c("promax",##) # This rotation was the best
"quartimax",
"bentlerT",
"equamax",
"varimin",
"geominT" ,
"bifactor")

#nf: number of factors to be tested based
#    on parallel analysis
nf = c(3,4)#c(parallel$nfact-1,
    #  parallel$nfact,
    #  parallel$nfact+1)+1

par(mfrow = c(1, 1))

for (k in nf) {
  for (i in fm) {
    for (j in rt) {
      cat(".....\n", "\n Rotation: ", j, "  fm = ", i, "\n......\n\n")
      m.3 = fa(
        N[,-c(3,4,7,11)],
        nfactors = k,
        rotate = j,
        residuals = T,
        fm = i,
        max.iter = 5,
        alpha = .05
      )
      if (m.3$RMSEA[[1]] < .10 &
          m.3$TLI > .8) {
        #Fit criteria in the model
        cat("\n\nRMSEA = ",m.3$RMSEA[[1]],
        "and TLI = ",m.3$TLI, "Ok....\n\n")
        mens = paste("Factoring method:", i,
                     " | Rotation:", j)
        print(m.3$Vaccounted)
        print(m.3$scores)

        title = paste("Factoring method",
                      i, "_", "Rotation", j, ".png")
        #png(
        #  title,
        #  width = 11,
        #  height = 9,
        #  units = "in",
        #  res = 300
        #)
         #
        fa.diagram(m.3, main = mens)
        #dev.off()
        locator(1)
      } else{
        cat(j, ",", i, "and", k, "Factor(s)
            did not fit well\n","RMSEA = ",m.3$RMSEA[[1]],
            "and TLI = ",m.3$TLI)

      }
    }
  }
}

#loadings
(m.3$loadings)
#Percentage explanation of the model
round(m.3$Vaccounted,2)
write.table(round(m.3$Vaccounted,2),
            "Others/var.accoumted.txt",sep = ";")

#Cumunalidades
m.3$communalities
comm = data.frame(
  "Sample" = c(colnames(A),
               colnames(A)),
  "value" = c(m.3$communalities,
              (1 - m.3$communalities)),
  "variable" = c(rep(c("u2","h2" ),
                     c(length(colnames(A)),
                        length(colnames(A))
                       )
                     )
                 )
  )

fa.diagram(m.3, main = mens)
comm$variable = as.factor(comm$variable)
levels(comm$variable ) = c("Specific variance",
                           "Communality")

comm$variable = factor(comm$variable,
                       levels = c("Specific variance",
                                  "Communality"))



#comm$factor = ifelse(comm$Sample == "pH" |
#                       comm$Sample == "Cyano" |
#                       comm$Sample == "Turb","Factor 1",
#                     ifelse(comm$Sample =="Phyco","Factor 4",
#                            ifelse(comm$Sample=="Green"|
#                                     comm$Sample=="Red", "Factor 3","Factor 2")))
comm = comm[order(comm$factor,decreasing = F),]
comm$Sample = as.factor(comm$Sample)
comm$Sample = factor(comm$Sample,
                     levels = c("Cyano","Turb","pH",
                                "DO", "Temp", "TC","Cond",
                                "Green", "Red",
                                "Phyco"))

Factor1 = graph2(comm[comm$factor =="Factor 1",],
                 pos = "none")+
  scale_fill_manual(values = c(gray(.4,),
                               gray(.7)))+
  facet_grid(~factor)+
  geom_hline(yintercept =  .5);Factor1

Factor2 = graph2(comm[comm$factor =="Factor 2",],
                 pos = "none")+

  scale_fill_manual(values = c(gray(.4,),
                               gray(.7)))+
  facet_grid(~factor)+
  geom_hline(yintercept =  .5);Factor2

Factor3 = graph2(comm[comm$factor =="Factor 3",],pos = "none")+
  scale_fill_manual(values = c(gray(.4,),
                               gray(.7)))+
  facet_grid(~factor)+
  geom_hline(yintercept =  .5);Factor3

Factor4 = graph2(comm[comm$factor =="Factor 4",],
                 pos = "right")+
  scale_fill_manual(values = c(gray(.4,),
                               gray(.7)))+
  facet_grid(~factor)+
  geom_hline(yintercept =  .5);Factor4

cml = plot_grid(Factor1,Factor2,Factor3,Factor4)
ggsave(
  filename = "communalities.png",
  device = "png",
  plot = cml,
  width = 8,
  height = 4,
  units = "in",
  scale = 1.25
)

#Loadings
loadings(m.3, list = (cutoff = .5))

## Loading Factors

Ld<-data.frame(loadings(m.3)[,1:3])
# Col. items
Ld$Items<-row.names(Ld)
# Reorder
Ord <-7:1
Ld$Items <- reorder(Ld$Items, Ord)

colnames(Ld)[1:3]<-c("Factor 1",
                     "Factor 2","Factor 3"
                     )

loadings.m <- melt(Ld, id="Items",
                   measure=c("Factor 1",
                             "Factor 2",
                             "Factor 3"),
                   variable.name="Factors",
                   value.name="Loadings")

colnames(loadings.m) =c("Variables",
                        "Factors",
                        "Loadings")
L = ggplot(loadings.m, aes(Variables,
                           Loadings,
                           fill=abs(Loadings))) +
  facet_wrap(~ Factors, nrow=1) +
  geom_bar(stat="identity") +
  coord_flip() + #change axes
  scale_fill_gradient2(name = "Loadings",
                       high = gray(.2),
                       mid = gray(.5),
                       low = gray(.9),
                       midpoint=0,
                       guide=F) +
  ylab("Loading" ) + #
  theme_bw(base_size=10);L
ggsave(
  filename = "Loading_3_A4.png",
  device = "png",
  plot = L,
  width = 8,
  height = 6,
  units = "in",
  scale = 1.35
)




d = as.data.frame(cbind(C,m.3$scores))
colnames(d) = c("Concentration",
                "Factor 1",
                "Factor 2",
                "Factor 3")
d$Concentration = as.factor(d$Concentration)
levels(d$Concentration) = c("0", "5", "10")



d.melt = melt(d, id="Concentration",
              measure=c("Factor 1",
                        "Factor 2",
                        "Factor 3"),
              variable.name="Factors",
              value.name="Factor scores")




# PCA ---------------------------------------------------------------------

res.pca <- PCA(A[,-c(1:2,5,6,8,10,11,12,13,14,15,16,19)], graph = FALSE)
print(res.pca)

#Eigenvalues / Variances
eig.val <- get_eigenvalue(res.pca)
eig.val

a =fviz_eig(res.pca,
            addlabels = TRUE,
            ylim = c(0, 50))


biplot = fviz_pca_biplot(res.pca,
                         col.ind = A$Time,
                         addEllipses = TRUE,ellipse.type = "confidence",
                         label = "var", # hide individual labels
                         palette = "npg",
                         title = "",
                         col.var = "black",
                         alpha.var =  "cos2",
                         legend.title = "Concentration")+theme_cowplot()
biplot

ggsave(filename = "graphics/PCA_1.png",
       plot = biplot,
       device = "png",
       height = 9,
       width = 13,
       units = "in",
       dpi = "retina" )


biplot2 = fviz_pca_biplot(res.pca,
                         col.ind = A$Sample,
                         addEllipses = TRUE,ellipse.type = "confidence",
                         label = "var", # hide individual labels
                         palette = "npg",
                         title = "",
                         col.var = "black",
                         alpha.var =  "cos2",
                         legend.title = "Concentration")+theme_cowplot()
biplot

ggsave(filename = "graphics/PCA_1.png",
       plot = biplot,
       device = "png",
       height = 9,
       width = 13,
       units = "in",
       dpi = "retina" )

res.hcpc <- HCPC(res.pca, graph = FALSE)

plot(res.hcpc, choice = "3D.map",)

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




