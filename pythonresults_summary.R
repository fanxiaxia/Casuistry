##CoLINGAM results from python
res.python <- matrix(NA, nrow = 24, ncol = 6)
res.python[1,] <- c(1, 3, 5, 2, 0, 4)
res.python[2,] <- c(4, 5, 1, 2, 0, 3)
res.python[3,] <- c(1, 3, 4, 5, 2, 0)
res.python[4,] <- c(1, 3, 4, 5, 2, 0)
res.python[5,] <- c(1, 4, 5, 3, 2, 0)
res.python[6,] <- c(1, 3, 4, 5, 2, 0)
res.python[7,] <- c(1, 3, 4, 5, 2, 0)
res.python[8,] <- c(1, 3, 5, 4, 2, 0)
res.python[9,] <- c(1, 3, 4, 5, 2, 0)
res.python[10,] <- c(1, 0, 3, 5, 2, 4)
res.python[11,] <- c(1, 3, 5, 4, 2, 0)
res.python[12,] <- c(1, 3, 4, 5, 2, 0)
res.python[13,] <- c(1, 3, 4, 5, 2, 0)
res.python[14,] <- c(1, 3, 4, 5, 2, 0)
res.python[15,] <- c(1, 3, 5, 4, 2, 0)
res.python[16,] <- c(3, 1, 4, 5, 2, 0)
res.python[17,] <- c(3, 4, 5, 1, 2, 0)
res.python[18,] <- c(1, 4, 5, 3, 2, 0)
res.python[19,] <- c(1, 3, 4, 5, 2, 0)
res.python[20,] <- c(1, 3, 4, 5, 2, 0)
res.python[21,] <- c(1, 5, 4, 3, 2, 0)
res.python[22,] <- c(3, 1, 4, 5, 2, 0)
res.python[23,] <- c(1, 3, 4, 5, 2, 0)
res.python[24,] <- c(1, 4, 5, 3, 2, 0)


res.python <- res.python +1

library(dplyr)


dataall <- read.csv("NIH-public-data_Erosheva-et-al.csv",header = T)
dataall3 <- dataall[,-1]
dataall4 <- dataall3[!duplicated(dataall3),]

sumtable <- dataall %>% group_by(GROUP_ID) %>% dplyr::summarise(nPI = n_distinct(PI_ID),nReviewer  = n_distinct(REVIEWER_ID), nReviews  = n(),nApplications = n_distinct(APPLICATION_ID))

covariates.baseline <- c("SRG","ADMIN_ORG","APPLICATION_TYPE","AMENDED","PI_GENDER","PI_ETHNICITY","PI_RACE",
                         "CAREER_STAGE","DEG_CATEGORY","INSTITUTION_BIN")
criterion <- c("Approach","Environment","Overall Impact","Investigator","Innovation","Significance")


python.table <- python.num <- matrix(NA, nrow = 24,ncol = 9)
for(i in 1:24)
{
  dataset.irg <- dataall4 %>% filter(IRG == i)
  python.table[i,] <- 
    c(i,length(unique(dataset.irg$APPLICATION_ID)),nrow(dataset.irg),criterion[res.python[i,]])
  python.num[i,] <- 
    c(i,length(unique(dataset.irg$APPLICATION_ID)),nrow(dataset.irg),res.python[i,])
}

colnames(python.table) <- c("irg","napp","nrev","R1","R2","R3","R4","R5","R6") 
colnames(python.num) <- c("irg","napp","nrev","R1","R2","R3","R4","R5","R6") 

write.csv(python.table,"pythontable.csv")
write.csv(python.num,"pythonnum.csv")

python.table <- data.frame(python.table)

python.table$napp <- as.numeric(python.table$napp)
python.table$nrev <- as.numeric(python.table$nrev)
xtable(data.frame(t(quantile(python.table$napp))))
xtable(data.frame(t(quantile(python.table$nrev))))

freqtable <- aggregate(irg~.,python.table[,c(1,4:9)],length)

aggregate(napp~.,python.table[,c(2,4:9)],sum)
aggregate(nrev~.,python.table[,c(3,4:9)],sum)


#Table 2
freqtable %>% arrange(desc(irg)) -> freqtable
write.csv(freqtable,"freqtable.csv")

library(xtable)
xtable(freqtable)

##bootstrap frequency tables
##CoLINGAM
boot.list <- NULL
for(i in 1:24)
{
  boot.temp <- read.csv(paste0("bootsirg",i,".csv"),header = T)[,-1]
  boot.temp$from <-   criterion[boot.temp$from+1]
  boot.temp$to <-   criterion[boot.temp$to+1]
  boot.temp$prob <- boot.temp[,3]/1000
  boot.temp8 <- boot.temp[,c(1,2,5)]
  impacttoapproach.temp <- boot.temp[which(boot.temp$from == "Overall Impact" 
                                           & boot.temp$to == "Approach"),c(1,2,5)]
  approachtoimpact.temp <- boot.temp[which(boot.temp$to == "Overall Impact" 
                                           & boot.temp$from == "Approach"),c(1,2,5)]
  
  if(nrow(impacttoapproach.temp)==0){impacttoapproach.temp <- c("Overall Impact","Approach",0)}
  if(nrow(approachtoimpact.temp)==0){approachtoimpact.temp <- c("Approach","Overall Impact",0)}
  
  boot.irg <- rbind(boot.temp8[1:8,],impacttoapproach.temp,approachtoimpact.temp)
  boot.list[[length(boot.list)+1]]  <- boot.irg
  write.csv(boot.irg,paste0("finalbootsIRG",i,".csv"))
}

table1.l <- rbind(boot.list[[1]],NA,boot.list[[2]],NA,boot.list[[3]])
table1.r <- rbind(boot.list[[3]],NA,boot.list[[5]],NA,boot.list[[6]])
table1 <- cbind(table1.l,table1.r)

table2.l <- rbind(boot.list[[7]],NA,boot.list[[8]],NA,boot.list[[9]])
table2.r <- rbind(boot.list[[10]],NA,boot.list[[11]],NA,boot.list[[12]])
table2 <- cbind(table2.l,table2.r)

table3.l <- rbind(boot.list[[13]],NA,boot.list[[14]],NA,boot.list[[15]])
table3.r <- rbind(boot.list[[16]],NA,boot.list[[17]],NA,boot.list[[18]])
table3 <- cbind(table3.l,table3.r)

table4.l <- rbind(boot.list[[19]],NA,boot.list[[20]],NA,boot.list[[21]])
table4.r <- rbind(boot.list[[22]],NA,boot.list[[23]],NA,boot.list[[24]])
table4 <- cbind(table4.l,table4.r)


library(xtable)
print(xtable(table1,digits = 3), include.rownames=FALSE)
print(xtable(table2,digits = 3), include.rownames=FALSE)
print(xtable(table3,digits = 3), include.rownames=FALSE)
print(xtable(table4,digits = 3), include.rownames=FALSE)


##multi-group CoLiNGAM
boot.list <- NULL
for(i in 1:24)
{
  boot.temp <- read.csv(paste0("group_bootsirg",i-1,".csv"),header = T)[,-1]
  boot.temp$from <-   criterion[boot.temp$from+1]
  boot.temp$to <-   criterion[boot.temp$to+1]
  boot.temp$prob <- round(boot.temp[,3]/1000,digits = 2)
  boot.temp8 <- boot.temp[,c(1,2,4)]
  impacttoapproach.temp <- boot.temp[which(boot.temp$from == "Overall Impact" 
                                           & boot.temp$to == "Approach"),c(1,2,4)]
  approachtoimpact.temp <- boot.temp[which(boot.temp$to == "Overall Impact" 
                                           & boot.temp$from == "Approach"),c(1,2,4)]
  
  if(nrow(impacttoapproach.temp)==0){impacttoapproach.temp <- c("Overall Impact","Approach",0)}
  if(nrow(approachtoimpact.temp)==0){approachtoimpact.temp <- c("Approach","Overall Impact",0)}
  
  boot.irg <- rbind(boot.temp8[1:8,],impacttoapproach.temp,approachtoimpact.temp)
  boot.list[[length(boot.list)+1]]  <- boot.irg
  # write.csv(boot.irg,paste0("finalbootsIRG",i,".csv"))
}

table1.l <- rbind(boot.list[[1]],NA,boot.list[[2]],NA,boot.list[[3]])
table1.r <- rbind(boot.list[[3]],NA,boot.list[[5]],NA,boot.list[[6]])
table1 <- cbind(table1.l,table1.r)

table2.l <- rbind(boot.list[[7]],NA,boot.list[[8]],NA,boot.list[[9]])
table2.r <- rbind(boot.list[[10]],NA,boot.list[[11]],NA,boot.list[[12]])
table2 <- cbind(table2.l,table2.r)

table3.l <- rbind(boot.list[[13]],NA,boot.list[[14]],NA,boot.list[[15]])
table3.r <- rbind(boot.list[[16]],NA,boot.list[[17]],NA,boot.list[[18]])
table3 <- cbind(table3.l,table3.r)

table4.l <- rbind(boot.list[[19]],NA,boot.list[[20]],NA,boot.list[[21]])
table4.r <- rbind(boot.list[[22]],NA,boot.list[[23]],NA,boot.list[[24]])
table4 <- cbind(table4.l,table4.r)


library(xtable)
print(xtable(table1), include.rownames=FALSE)
print(xtable(table2), include.rownames=FALSE)
print(xtable(table3), include.rownames=FALSE)
print(xtable(table4), include.rownames=FALSE)


##entire set
boot.all <- read.csv(paste0("bootsirg_all.csv"),header = T)[,-1]
boot.all$from <-   criterion[boot.all$from+1]
boot.all$to <-   criterion[boot.all$to+1]
boot.all$prob <- boot.all[,3]/1000

print(xtable(boot.all[,c(1,2,5)]), include.rownames=FALSE)

