# Dir and librarys  -----------------------------------------------

#Dir
local = "C:\\Users\\User\\Documents\\Projects\\R-private\\Science"
setwd(local)

#librarys
library(readxl)

# #Functions --------------------------------------------------------------
#percentage NA Function
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
data = data[,-2] # Yes,"Meso"!

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
#data = data[,-19]


# Impute: Fluoride (Random Forest)
df.na = data
df.imputed = randomForest::rfImpute(as.factor(df.na$Sample) ~ .,
                                    df.na,
                                    iter=5,
                                    ntree=500)
data =  cbind(data[,c(1,2)],df.imputed[,-c(1,2)] )

#
