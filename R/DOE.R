# This script randomly generates a split-plot and exports it as a csv file in the folder where this script is saved
### Last modif:2022-07-01 R.LOISON
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


DOE_split_plot <- function(bloc_nb=4,MP=c("F1","F2","F3","F4","F5"),SP = c("Arachide","Coton","Sorgho","Maïs","Mil"),nombre=10,name_file="DOE"){
  library(stringr);library(dplyr);library(data.table);library(plyr)

# 1. Choose how many blocks you will use:
#bloc_nb <-  4 # Here, it will generate 4 blocks

# 2. Write the levels of the main plot
#MP <- c("F1","F2","F3","F4","F5") # Here, the levels of the main plot factor are 5 different fertilization; F1 to F5

#3. Write the levels of the subplot
#SP <- c("Arachide","Coton","Sorgho","Maïs","Mil") # Here, the levels of the subplot factor are 5 different crops

#4. Pick a random number randomness
#nombre <- as.numeric(format(Sys.Date(),"%Y")) # Here, the number chosen is the current year

#5. Pick a name for the output file
#name_file <- paste0("plan_experimental_",Sys.Date()) # The output will be store under the name "plan_experimental_" followed by the date

###############################
### Do not modify below this line
bloc<- paste0("B",str_pad(1:bloc_nb,nchar(bloc_nb),side="left",pad="0"))
a<-expand.grid(bloc,MP)
colnames(a)<- c("bloc","MP")
set.seed(nombre)

a$alea_gde_parcelle<- abs(rnorm(nrow(a),mean=10,sd=100))
setDT(a)[, MP_nb := frank(alea_gde_parcelle,ties.method="first"), by=bloc]
a2 <- a[,-3]

b <-expand.grid(bloc,MP,SP)
colnames(b)<- c("bloc","MP","SP")
set.seed(nombre)
b$alea_pte_parcelle<- rnorm(nrow(b),mean=10,sd=100)
setDT(b)[, SP_nb := frank(alea_pte_parcelle,ties.method="first"), by=c("bloc","MP")]
b2 <- b[,-4]

plan_expe <- join(b2,a2)
plan_expe$bloc_num <- as.numeric(gsub("B","",plan_expe$bloc))
plan_expe$PE <- 10000*plan_expe$bloc_num+100*plan_expe$MP_nb +plan_expe$SP_nb
setDT(plan_expe)[, ttt := frank(PE,ties.method="first"), by=bloc]
lg<-nchar(max(plan_expe$ttt))

plan_expe$PE <- as.numeric(paste0(plan_expe$bloc_num,str_pad(plan_expe$ttt,lg,side="left",pad="0")))
plan_expe <- plan_expe[order(plan_expe$PE),c("bloc","MP","SP","PE")]


#rm(list=setdiff(ls(),c("plan_expe","name_file")))
write.csv(file=paste0(name_file,".csv"),plan_expe,row.names=F)
}
