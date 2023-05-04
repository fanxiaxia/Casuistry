library(dplyr)

dataall <- read.csv("NIH-public-data_Erosheva-et-al.csv",header = T)
dataall3 <- dataall[,-1]
dataall4 <- dataall3[!duplicated(dataall3),]

sumtable <- dataall %>% group_by(GROUP_ID) %>% dplyr::summarise(nPI = n_distinct(PI_ID),nReviewer  = n_distinct(REVIEWER_ID), nReviews  = n(),nApplications = n_distinct(APPLICATION_ID))

covariates.baseline <- c("SRG","ADMIN_ORG","APPLICATION_TYPE","AMENDED","PI_GENDER","PI_ETHNICITY","PI_RACE",
                         "CAREER_STAGE","DEG_CATEGORY","INSTITUTION_BIN")
criterion <- c("APPROACH_INIT","ENVIRONMENT_INIT","IMPACT_INIT","INVESTIGATOR_INIT","INNOVATION_INIT","SIGNIFICANCE_INIT")

for(j in 1:10)
{
  dataall4[,covariates.baseline[j]] <- as.factor(dataall4[,covariates.baseline[j]])
  print(levels(dataall4[,covariates.baseline[j]]))
}

datause <- dataall4[,c("IRG",covariates.baseline,criterion)]


entire_dataset <- NULL

for(irg in 1:24)
{
  dataset.irg <- datause %>% filter(IRG == irg)
  
  dataset.irg <- dataset.irg[vapply(dataset.irg, function(x) length(unique(x)) > 1, logical(1L))]
  
  #dataset.irg %>% select(where(~n_distinct(.) > 1)) -> dataset.irg
  covariates.irg <- intersect(colnames(dataset.irg),covariates.baseline)
  
  e.star_matrix <- matrix(NA,nrow = nrow(dataset.irg), ncol = length(criterion))
  for(i in 1:length(criterion))
  {
    formula.score <- formula(paste0(criterion[i],"~",paste(covariates.irg,collapse="+")))
    fit.score <- lm(formula.score, data=dataset.irg)
    e.i <- residuals(fit.score)
    e.star_matrix[,i] <- e.i
  }
  resdata <- data.frame(e.star_matrix)
  colnames(resdata) <- criterion
  
  entire_dataset <- rbind(entire_dataset,resdata)
  
  write.csv(resdata,paste0("datairg",irg,".csv"))
}

write.csv(entire_dataset,"entire_dataset.csv")

