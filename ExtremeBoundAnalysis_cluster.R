#--------------------------------------------------------------------
# BKH Trade Flows Analysis - Extreme Bounds Analysis
#--------------------------------------------------------------------
require(doMC)
require(foreach)
require(parallel)
install.packages('ExtremeBounds', repos='http://cran.us.r-project.org')
require('ExtremeBounds')
require(base)
require(plyr)
library(methods)


numdraws = 2000
options(scipen=50)

#setwd("/usr3/graduate/jhersh/TradeFlows")

# set the number of CPUs to use
nCores <- as.numeric(Sys.getenv("NSLOTS"))
if (is.na(nCores))  nCores <- 1   
registerDoMC(nCores)
paste("Running code using",nCores,"cores") 


# Load Data
load("TradeFlowsCC.Rda")
load("TF_EfeCC.Rda")
load("TF_EMfeCC.Rda")
load("TF_EyearfeCC.Rda")
load("TF_EMYfeCC.Rda")


in.data.sets <- list (EBA_NoFEs = TradeFlowsCC,
                      EBA_ExpFEs = TF_EfeCC,
                      EBA_ExpImpFEs = TF_EMfeCC,
                      EBA_ExpYearFEs = TF_EyearfeCC, 
                      EBA_ExpImpYearFEs = TF_EMYfeCC)

# GeneticDstFST = GeneticDstFST - mean(GeneticDstFST), GeneticDstNEI = GeneticDstNEI - mean(GeneticDstNEI),

freevars <- c("lndistwces", "GDPproduct")

doubtful <- c("Contiguity", "IslandEither", "LandLockedEither", "CommLang_Off", "CommLang_Ethno", "Colony", "CommColonizer", "CommLegal", "ReligiousDist", "HumanCapital_1", "PhysCapital_1", "ArableLandV1", "GATTWTOEither", "RegTradeAgree", "CommCurrency", "CapitalOpenness", "ExchRateVol", "FixedEither", "FixedBoth", "CrawlingPegEither", "CrawlingPegBoth", "MovingBandEither", "MovingBandBoth", "DebtCrisis3yr", "BankingCrisis3yr", "CurrencyCrisis3yr")

labellist <- c("lndistwces"="Ln Dist", "GDPproduct"="Product of GDPs ", "Contiguity"="Continguous", "IslandEither"="Island", "LandLockedEither"="Landlocked", "CommLang_Off"="Share Off. Lang.", "CommLang_Ethno"="9%+ Speak Lang.", "Colony"="Former Colony", "CommColonizer"="Comm Colonizer", "CommLegal"="Comm Legal Orig.", "ReligiousDist"="Religious Dist.", "HumanCapital_1"="Human Capital", "PhysCapital_1"="Physical Capital", "ArableLandV1"="Arable Land ", "GATTWTOEither"="WTO/GATT", "CommCurrency"="Comm Currency", "RegTradeAgree"="Reg. Trade Agrmt", "CapitalOpenness"="Capital Openness",  "ExchRateVol" = "ExchRate Vol", "FixedEither" = "Fixed ExchR Either" , "FixedBoth" = "Fixed ExchR Both", "CrawlingPegEither" = "CrawlPeg Either", "CrawlingPegBoth" = "CrawlPeg Both", "MovingBandEither" = "MoveBand Either", "MovingBandBoth" = "MoveBand Both", "DebtCrisis3yr" = "DebtCris 3yr", "BankingCrisis3yr" = "BankCris 3yr", "CurrencyCrisis3yr" = "Curr Cris 3yr")

### FUNCTION EBA
EBAFit <- function(dataset, yvar, freevars, doubtfulvars, draws) {
  fit <- eba(data = dataset, 
             y = yvar, 
             free = freevars, 
             doubtful = doubtfulvars, 
             draw = draws,
             vif = 10)
  out <- fit[[1]]
  CI <- paste("(",round(out[,8],2),",",round(out[,9],2),")",sep="")
  out2 <- cbind(rownames(out),"UB to LB" = CI,Robust = ifelse(out[,c(10)]=="TRUE","y", "n"))
  return(list("model" = fit, "outtable" = out2))
}


EBA.result <- foreach(i=1:length(in.data.sets), .packages=c('ExtremeBounds','base','plyr','methods')) %dopar% {
  
  EBA <- EBAFit(in.data.sets[[i]], "lnBilatFlowFEEN_r", freevars, doubtful, numdraws)
  
  
  #construct the file name 
  out.file <- paste0(names(in.data.sets)[i],".pdf")
  
  # print histogram
  pdf(out.file)
  hist(EBA[[1]], variables = doubtful, main = labellist)
  dev.off()
  
  return(EBA[[2]])
  
}
  
 coefTable <- cbind(EBA.result[[1]],
                    EBA.result[[2]][,-1],
                    EBA.result[[3]][,-1],
                    EBA.result[[4]][,-1],
                    EBA.result[[5]][,-1])

 write.table(as.matrix(coefTable), file = "EBA_FETable.csv",row.names=T, sep=",")
# 


