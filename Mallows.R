#install.packages("PerMallows_1.13.tar.gz", type="source")
library(PerMallows)
criterion <- c("Approach","Environment","Overall Impact","Investigator","Innovation","Significance")
num.table <- read.csv("pythonnum.csv",header = T)[,-1]
my.mm <- lmm(data = as.matrix(num.table.perm), dist.name = "cayley",estimation = "exact")
#Modal order
criterion[my.mm$mode]



#this case weight the order by each IRG with their corresponding number of reviews
# nreviews <- sum(num.table$nrev)
# num.table.perm.rep <- matrix(NA, nrow = nreviews,ncol = 6)
# for(i in 1:24)
# {
#   if (i==1){rownumber  <- 1:num.table$nrev[1]}
#   else{rownumber <- (sum(num.table$nrev[1:(i-1)])+1):sum(num.table$nrev[1:i])}
#   num.table.perm.rep[rownumber,]<- rep(as.numeric(num.table[i,-c(1,2,3)]),each = length(rownumber))
# }
# 
# my.mm.rep <- lmm(data = num.table.perm.rep, dist.name = "cayley",estimation = "exact")
# #Modal order
# criterion[my.mm.rep$mode]
