#ALL2
#####Pooed effect of HW all-hos###
library(readxl)
allhos2_result_farmers_20_01 <- read_excel("~/Documents/OneDrive - UMP/Projects/farmer/hw results/hw results prepare for stage 2/hw.20.01/all/untitled folder/allhos2_result_farmers_20.01.xlsx", 
                                           sheet = "all.main")

main.eff<-allhos2_result_farmers_20_01 
main.eff<- as.matrix (main.eff)

library(readxl)
allhos2_result_farmers_20_01 <- read_excel("~/Documents/OneDrive - UMP/Projects/farmer/hw results/hw results prepare for stage 2/hw.20.01/all/untitled folder/allhos2_result_farmers_20.01.xlsx", 
                                           sheet = "all.add")

added.eff<-allhos2_result_farmers_20_01
added.eff<- as.matrix (added.eff)

label <- paste("hw",rep(c(2,4),each=3),rep(c(97,98,99),2),sep=".")
table1 <- matrix(NA,6,7,dimnames=list(label,
                                      c("N comm","Est.main","95%CI.main","P-het.added","Est.added",
                                        "95%CI.added","P-het.added")))

for(i in 1:6) {
  # SET TO MISSING IF NO ESTIMATE FOR ADDED EFFECT
  added.eff[added.eff[,2*i]==0,c(2*i-1,2*i)] <- NA
  main.eff[is.na(added.eff[,2*i]),c(2*i-1,2*i)] <- NA
  # RUN THE META-ANALYSIS
  pool.main <- rma.uni(yi=main.eff[,2*i-1],sei=main.eff[,2*i])
  pool.added <- rma.uni(yi=added.eff[,2*i-1],sei=added.eff[,2*i])
  # FILL TABLE1
  table1[i,] <- c(sum(!is.na(added.eff[,2*i-1])),
                  round(exp(pool.main$b)*100-100,3),
                  paste(round(exp(pool.main$b-1.96*pool.main$se)*100-100,3),"to",
                        round(exp(pool.main$b+1.96*pool.main$se)*100-100,3)),
                  round(pool.main$QEp,3),
                  round(exp(pool.added$b)*100-100,3),
                  paste(round(exp(pool.added$b-1.96*pool.added$se)*100-100,3),"to",
                        round(exp(pool.added$b+1.96*pool.added$se)*100-100,3)),
                  round(pool.added$QEp,3))
}
table1
### Table RR ###
label <- paste("hw",rep(c(2,4),each=3),rep(c(97,98,99),2),sep=".")
table2 <- matrix(NA,6,7,dimnames=list(label,
                                      c("N comm","Est.main","95%CI.main","P-het.added","Est.added",
                                        "95%CI.added","P-het.added")))
for(i in 1:6) {
  # SET TO MISSING IF NO ESTIMATE FOR ADDED EFFECT
  added.eff[added.eff[,2*i]==0,c(2*i-1,2*i)] <- NA
  main.eff[is.na(added.eff[,2*i]),c(2*i-1,2*i)] <- NA
  # RUN THE META-ANALYSIS
  pool.main <- rma.uni(yi=main.eff[,2*i-1],sei=main.eff[,2*i])
  pool.added <- rma.uni(yi=added.eff[,2*i-1],sei=added.eff[,2*i])
  # FILL TABLE1
  table2[i,] <- c(sum(!is.na(added.eff[,2*i-1])),
                  round(exp(pool.main$b),3),
                  paste(round(exp(pool.main$b-1.96*pool.main$se),3),"to",
                        round(exp(pool.main$b+1.96*pool.main$se),3)),
                  round(pool.main$QEp,3),
                  round(exp(pool.added$b),3),
                  paste(round(exp(pool.added$b-1.96*pool.added$se),3),"to",
                        round(exp(pool.added$b+1.96*pool.added$se),3)),
                  round(pool.added$QEp,3))
}
table2
#####Pooed effect of HW all-icdAB###
library(readxl)
icdAB2_result_farmers_20_01 <- read_excel("~/Documents/OneDrive - UMP/Projects/farmer/hw results/hw results prepare for stage 2/hw.20.01/all/untitled folder/icdAB2_result_farmers_20.01.xlsx", 
                                          sheet = "all.main")


main.eff<-icdAB2_result_farmers_20_01
main.eff<- as.matrix (main.eff)

library(readxl)
icdAB2_result_farmers_20_01 <- read_excel("~/Documents/OneDrive - UMP/Projects/farmer/hw results/hw results prepare for stage 2/hw.20.01/all/untitled folder/icdAB2_result_farmers_20.01.xlsx", 
                                          sheet = "all.add")


added.eff<-icdAB2_result_farmers_20_01
added.eff<- as.matrix (added.eff)

label <- paste("hw",rep(c(2,4),each=3),rep(c(97,98,99),2),sep=".")
table1 <- matrix(NA,6,7,dimnames=list(label,
                                      c("N comm","Est.main","95%CI.main","P-het.added","Est.added",
                                        "95%CI.added","P-het.added")))

for(i in 1:6) {
  # SET TO MISSING IF NO ESTIMATE FOR ADDED EFFECT
  added.eff[added.eff[,2*i]==0,c(2*i-1,2*i)] <- NA
  main.eff[is.na(added.eff[,2*i]),c(2*i-1,2*i)] <- NA
  # RUN THE META-ANALYSIS
  pool.main <- rma.uni(yi=main.eff[,2*i-1],sei=main.eff[,2*i])
  pool.added <- rma.uni(yi=added.eff[,2*i-1],sei=added.eff[,2*i])
  # FILL TABLE1
  table1[i,] <- c(sum(!is.na(added.eff[,2*i-1])),
                  round(exp(pool.main$b)*100-100,3),
                  paste(round(exp(pool.main$b-1.96*pool.main$se)*100-100,3),"to",
                        round(exp(pool.main$b+1.96*pool.main$se)*100-100,3)),
                  round(pool.main$QEp,3),
                  round(exp(pool.added$b)*100-100,3),
                  paste(round(exp(pool.added$b-1.96*pool.added$se)*100-100,3),"to",
                        round(exp(pool.added$b+1.96*pool.added$se)*100-100,3)),
                  round(pool.added$QEp,3))
}
table1
### Table RR ###
label <- paste("hw",rep(c(2,4),each=3),rep(c(97,98,99),2),sep=".")
table2 <- matrix(NA,6,7,dimnames=list(label,
                                      c("N comm","Est.main","95%CI.main","P-het.added","Est.added",
                                        "95%CI.added","P-het.added")))
for(i in 1:6) {
  # SET TO MISSING IF NO ESTIMATE FOR ADDED EFFECT
  added.eff[added.eff[,2*i]==0,c(2*i-1,2*i)] <- NA
  main.eff[is.na(added.eff[,2*i]),c(2*i-1,2*i)] <- NA
  # RUN THE META-ANALYSIS
  pool.main <- rma.uni(yi=main.eff[,2*i-1],sei=main.eff[,2*i])
  pool.added <- rma.uni(yi=added.eff[,2*i-1],sei=added.eff[,2*i])
  # FILL TABLE1
  table2[i,] <- c(sum(!is.na(added.eff[,2*i-1])),
                  round(exp(pool.main$b),3),
                  paste(round(exp(pool.main$b-1.96*pool.main$se),3),"to",
                        round(exp(pool.main$b+1.96*pool.main$se),3)),
                  round(pool.main$QEp,3),
                  round(exp(pool.added$b),3),
                  paste(round(exp(pool.added$b-1.96*pool.added$se),3),"to",
                        round(exp(pool.added$b+1.96*pool.added$se),3)),
                  round(pool.added$QEp,3))
}
table2
#####Pooed effect of HW all-icdE###
library(readxl)
icdE2_result_farmers_20_01 <- read_excel("~/Documents/OneDrive - UMP/Projects/farmer/hw results/hw results prepare for stage 2/hw.20.01/all/untitled folder/icdE2_result_farmers_20.01.xlsx", 
                                         sheet = "all.main")


main.eff<-icdE2_result_farmers_20_01
main.eff<- as.matrix (main.eff)

library(readxl)
icdE2_result_farmers_20_01 <- read_excel("~/Documents/OneDrive - UMP/Projects/farmer/hw results/hw results prepare for stage 2/hw.20.01/all/untitled folder/icdE2_result_farmers_20.01.xlsx", 
                                         sheet = "all.add")

added.eff<-icdE2_result_farmers_20_01
added.eff<- as.matrix (added.eff)

label <- paste("hw",rep(c(2,4),each=3),rep(c(97,98,99),2),sep=".")
table1 <- matrix(NA,6,7,dimnames=list(label,
                                      c("N comm","Est.main","95%CI.main","P-het.added","Est.added",
                                        "95%CI.added","P-het.added")))

for(i in 1:6) {
  # SET TO MISSING IF NO ESTIMATE FOR ADDED EFFECT
  added.eff[added.eff[,2*i]==0,c(2*i-1,2*i)] <- NA
  main.eff[is.na(added.eff[,2*i]),c(2*i-1,2*i)] <- NA
  # RUN THE META-ANALYSIS
  pool.main <- rma.uni(yi=main.eff[,2*i-1],sei=main.eff[,2*i])
  pool.added <- rma.uni(yi=added.eff[,2*i-1],sei=added.eff[,2*i])
  # FILL TABLE1
  table1[i,] <- c(sum(!is.na(added.eff[,2*i-1])),
                  round(exp(pool.main$b)*100-100,3),
                  paste(round(exp(pool.main$b-1.96*pool.main$se)*100-100,3),"to",
                        round(exp(pool.main$b+1.96*pool.main$se)*100-100,3)),
                  round(pool.main$QEp,3),
                  round(exp(pool.added$b)*100-100,3),
                  paste(round(exp(pool.added$b-1.96*pool.added$se)*100-100,3),"to",
                        round(exp(pool.added$b+1.96*pool.added$se)*100-100,3)),
                  round(pool.added$QEp,3))
}
table1
### Table RR ###
label <- paste("hw",rep(c(2,4),each=3),rep(c(97,98,99),2),sep=".")
table2 <- matrix(NA,6,7,dimnames=list(label,
                                      c("N comm","Est.main","95%CI.main","P-het.added","Est.added",
                                        "95%CI.added","P-het.added")))
for(i in 1:6) {
  # SET TO MISSING IF NO ESTIMATE FOR ADDED EFFECT
  added.eff[added.eff[,2*i]==0,c(2*i-1,2*i)] <- NA
  main.eff[is.na(added.eff[,2*i]),c(2*i-1,2*i)] <- NA
  # RUN THE META-ANALYSIS
  pool.main <- rma.uni(yi=main.eff[,2*i-1],sei=main.eff[,2*i])
  pool.added <- rma.uni(yi=added.eff[,2*i-1],sei=added.eff[,2*i])
  # FILL TABLE1
  table2[i,] <- c(sum(!is.na(added.eff[,2*i-1])),
                  round(exp(pool.main$b),3),
                  paste(round(exp(pool.main$b-1.96*pool.main$se),3),"to",
                        round(exp(pool.main$b+1.96*pool.main$se),3)),
                  round(pool.main$QEp,3),
                  round(exp(pool.added$b),3),
                  paste(round(exp(pool.added$b-1.96*pool.added$se),3),"to",
                        round(exp(pool.added$b+1.96*pool.added$se),3)),
                  round(pool.added$QEp,3))
}
table2
#####Pooed effect of HW all-icdF###
library(readxl)
icdF2_result_farmers_20_01 <- read_excel("~/Documents/OneDrive - UMP/Projects/farmer/hw results/hw results prepare for stage 2/hw.20.01/all/untitled folder/icdF2_result_farmers_20.01.xlsx", 
                                         sheet = "all.main")


main.eff<-icdF2_result_farmers_20_01
main.eff<- as.matrix (main.eff)

library(readxl)
icdF2_result_farmers_20_01 <- read_excel("~/Documents/OneDrive - UMP/Projects/farmer/hw results/hw results prepare for stage 2/hw.20.01/all/untitled folder/icdF2_result_farmers_20.01.xlsx", 
                                         sheet = "all.add")

added.eff<-icdF2_result_farmers_20_01
added.eff<- as.matrix (added.eff)

label <- paste("hw",rep(c(2,4),each=3),rep(c(97,98,99),2),sep=".")
table1 <- matrix(NA,6,7,dimnames=list(label,
                                      c("N comm","Est.main","95%CI.main","P-het.added","Est.added",
                                        "95%CI.added","P-het.added")))

for(i in 1:6) {
  # SET TO MISSING IF NO ESTIMATE FOR ADDED EFFECT
  added.eff[added.eff[,2*i]==0,c(2*i-1,2*i)] <- NA
  main.eff[is.na(added.eff[,2*i]),c(2*i-1,2*i)] <- NA
  # RUN THE META-ANALYSIS
  pool.main <- rma.uni(yi=main.eff[,2*i-1],sei=main.eff[,2*i])
  pool.added <- rma.uni(yi=added.eff[,2*i-1],sei=added.eff[,2*i])
  # FILL TABLE1
  table1[i,] <- c(sum(!is.na(added.eff[,2*i-1])),
                  round(exp(pool.main$b)*100-100,3),
                  paste(round(exp(pool.main$b-1.96*pool.main$se)*100-100,3),"to",
                        round(exp(pool.main$b+1.96*pool.main$se)*100-100,3)),
                  round(pool.main$QEp,3),
                  round(exp(pool.added$b)*100-100,3),
                  paste(round(exp(pool.added$b-1.96*pool.added$se)*100-100,3),"to",
                        round(exp(pool.added$b+1.96*pool.added$se)*100-100,3)),
                  round(pool.added$QEp,3))
}
table1
### Table RR ###
label <- paste("hw",rep(c(2,4),each=3),rep(c(97,98,99),2),sep=".")
table2 <- matrix(NA,6,7,dimnames=list(label,
                                      c("N comm","Est.main","95%CI.main","P-het.added","Est.added",
                                        "95%CI.added","P-het.added")))
for(i in 1:6) {
  # SET TO MISSING IF NO ESTIMATE FOR ADDED EFFECT
  added.eff[added.eff[,2*i]==0,c(2*i-1,2*i)] <- NA
  main.eff[is.na(added.eff[,2*i]),c(2*i-1,2*i)] <- NA
  # RUN THE META-ANALYSIS
  pool.main <- rma.uni(yi=main.eff[,2*i-1],sei=main.eff[,2*i])
  pool.added <- rma.uni(yi=added.eff[,2*i-1],sei=added.eff[,2*i])
  # FILL TABLE1
  table2[i,] <- c(sum(!is.na(added.eff[,2*i-1])),
                  round(exp(pool.main$b),3),
                  paste(round(exp(pool.main$b-1.96*pool.main$se),3),"to",
                        round(exp(pool.main$b+1.96*pool.main$se),3)),
                  round(pool.main$QEp,3),
                  round(exp(pool.added$b),3),
                  paste(round(exp(pool.added$b-1.96*pool.added$se),3),"to",
                        round(exp(pool.added$b+1.96*pool.added$se),3)),
                  round(pool.added$QEp,3))
}
table2

#####Pooed effect of HW all-icdI###
library(readxl)
icdI2_result_farmers_20_01 <- read_excel("~/Documents/OneDrive - UMP/Projects/farmer/hw results/hw results prepare for stage 2/hw.20.01/all/untitled folder/icdI2_result_farmers_20.01.xlsx", 
                                         sheet = "all.main")

main.eff<-icdI2_result_farmers_20_01
main.eff<- as.matrix (main.eff)

library(readxl)
icdI2_result_farmers_20_01 <- read_excel("~/Documents/OneDrive - UMP/Projects/farmer/hw results/hw results prepare for stage 2/hw.20.01/all/untitled folder/icdI2_result_farmers_20.01.xlsx", 
                                         sheet = "all.add")

added.eff<-icdI2_result_farmers_20_01
added.eff<- as.matrix (added.eff)

label <- paste("hw",rep(c(2,4),each=3),rep(c(97,98,99),2),sep=".")
table1 <- matrix(NA,6,7,dimnames=list(label,
                                      c("N comm","Est.main","95%CI.main","P-het.added","Est.added",
                                        "95%CI.added","P-het.added")))

for(i in 1:6) {
  # SET TO MISSING IF NO ESTIMATE FOR ADDED EFFECT
  added.eff[added.eff[,2*i]==0,c(2*i-1,2*i)] <- NA
  main.eff[is.na(added.eff[,2*i]),c(2*i-1,2*i)] <- NA
  # RUN THE META-ANALYSIS
  pool.main <- rma.uni(yi=main.eff[,2*i-1],sei=main.eff[,2*i])
  pool.added <- rma.uni(yi=added.eff[,2*i-1],sei=added.eff[,2*i])
  # FILL TABLE1
  table1[i,] <- c(sum(!is.na(added.eff[,2*i-1])),
                  round(exp(pool.main$b)*100-100,3),
                  paste(round(exp(pool.main$b-1.96*pool.main$se)*100-100,3),"to",
                        round(exp(pool.main$b+1.96*pool.main$se)*100-100,3)),
                  round(pool.main$QEp,3),
                  round(exp(pool.added$b)*100-100,3),
                  paste(round(exp(pool.added$b-1.96*pool.added$se)*100-100,3),"to",
                        round(exp(pool.added$b+1.96*pool.added$se)*100-100,3)),
                  round(pool.added$QEp,3))
}
table1
### Table RR ###
label <- paste("hw",rep(c(2,4),each=3),rep(c(97,98,99),2),sep=".")
table2 <- matrix(NA,6,7,dimnames=list(label,
                                      c("N comm","Est.main","95%CI.main","P-het.added","Est.added",
                                        "95%CI.added","P-het.added")))
for(i in 1:6) {
  # SET TO MISSING IF NO ESTIMATE FOR ADDED EFFECT
  added.eff[added.eff[,2*i]==0,c(2*i-1,2*i)] <- NA
  main.eff[is.na(added.eff[,2*i]),c(2*i-1,2*i)] <- NA
  # RUN THE META-ANALYSIS
  pool.main <- rma.uni(yi=main.eff[,2*i-1],sei=main.eff[,2*i])
  pool.added <- rma.uni(yi=added.eff[,2*i-1],sei=added.eff[,2*i])
  # FILL TABLE1
  table2[i,] <- c(sum(!is.na(added.eff[,2*i-1])),
                  round(exp(pool.main$b),3),
                  paste(round(exp(pool.main$b-1.96*pool.main$se),3),"to",
                        round(exp(pool.main$b+1.96*pool.main$se),3)),
                  round(pool.main$QEp,3),
                  round(exp(pool.added$b),3),
                  paste(round(exp(pool.added$b-1.96*pool.added$se),3),"to",
                        round(exp(pool.added$b+1.96*pool.added$se),3)),
                  round(pool.added$QEp,3))
}
table2

#####Pooed effect of HW all-icdJ###
library(readxl)
icdJ2_result_farmers_20_01 <- read_excel("~/Documents/OneDrive - UMP/Projects/farmer/hw results/hw results prepare for stage 2/hw.20.01/all/untitled folder/icdJ2_result_farmers_20.01.xlsx", 
                                         sheet = "all.main")


main.eff<-icdJ2_result_farmers_20_01 
main.eff<- as.matrix (main.eff)

library(readxl)
icdJ2_result_farmers_20_01 <- read_excel("~/Documents/OneDrive - UMP/Projects/farmer/hw results/hw results prepare for stage 2/hw.20.01/all/untitled folder/icdJ2_result_farmers_20.01.xlsx", 
                                         sheet = "all.add")

added.eff<-icdJ2_result_farmers_20_01
added.eff<- as.matrix (added.eff)

label <- paste("hw",rep(c(2,4),each=3),rep(c(97,98,99),2),sep=".")
table1 <- matrix(NA,6,7,dimnames=list(label,
                                      c("N comm","Est.main","95%CI.main","P-het.added","Est.added",
                                        "95%CI.added","P-het.added")))

for(i in 1:6) {
  # SET TO MISSING IF NO ESTIMATE FOR ADDED EFFECT
  added.eff[added.eff[,2*i]==0,c(2*i-1,2*i)] <- NA
  main.eff[is.na(added.eff[,2*i]),c(2*i-1,2*i)] <- NA
  # RUN THE META-ANALYSIS
  pool.main <- rma.uni(yi=main.eff[,2*i-1],sei=main.eff[,2*i])
  pool.added <- rma.uni(yi=added.eff[,2*i-1],sei=added.eff[,2*i])
  # FILL TABLE1
  table1[i,] <- c(sum(!is.na(added.eff[,2*i-1])),
                  round(exp(pool.main$b)*100-100,3),
                  paste(round(exp(pool.main$b-1.96*pool.main$se)*100-100,3),"to",
                        round(exp(pool.main$b+1.96*pool.main$se)*100-100,3)),
                  round(pool.main$QEp,3),
                  round(exp(pool.added$b)*100-100,3),
                  paste(round(exp(pool.added$b-1.96*pool.added$se)*100-100,3),"to",
                        round(exp(pool.added$b+1.96*pool.added$se)*100-100,3)),
                  round(pool.added$QEp,3))
}
table1
### Table RR ###
label <- paste("hw",rep(c(2,4),each=3),rep(c(97,98,99),2),sep=".")
table2 <- matrix(NA,6,7,dimnames=list(label,
                                      c("N comm","Est.main","95%CI.main","P-het.added","Est.added",
                                        "95%CI.added","P-het.added")))
for(i in 1:6) {
  # SET TO MISSING IF NO ESTIMATE FOR ADDED EFFECT
  added.eff[added.eff[,2*i]==0,c(2*i-1,2*i)] <- NA
  main.eff[is.na(added.eff[,2*i]),c(2*i-1,2*i)] <- NA
  # RUN THE META-ANALYSIS
  pool.main <- rma.uni(yi=main.eff[,2*i-1],sei=main.eff[,2*i])
  pool.added <- rma.uni(yi=added.eff[,2*i-1],sei=added.eff[,2*i])
  # FILL TABLE1
  table2[i,] <- c(sum(!is.na(added.eff[,2*i-1])),
                  round(exp(pool.main$b),3),
                  paste(round(exp(pool.main$b-1.96*pool.main$se),3),"to",
                        round(exp(pool.main$b+1.96*pool.main$se),3)),
                  round(pool.main$QEp,3),
                  round(exp(pool.added$b),3),
                  paste(round(exp(pool.added$b-1.96*pool.added$se),3),"to",
                        round(exp(pool.added$b+1.96*pool.added$se),3)),
                  round(pool.added$QEp,3))
}
table2
#####Pooed effect of HW all-icdK###
library(readxl)
icdK2_result_farmers_20_01 <- read_excel("~/Documents/OneDrive - UMP/Projects/farmer/hw results/hw results prepare for stage 2/hw.20.01/all/untitled folder/icdK2_result_farmers_20.01.xlsx", 
                                         sheet = "all.main")


main.eff<-icdK2_result_farmers_20_01
main.eff<- as.matrix (main.eff)

library(readxl)
icdK2_result_farmers_20_01 <- read_excel("~/Documents/OneDrive - UMP/Projects/farmer/hw results/hw results prepare for stage 2/hw.20.01/all/untitled folder/icdK2_result_farmers_20.01.xlsx", 
                                         sheet = "all.add")

added.eff<-icdK2_result_farmers_20_01
added.eff<- as.matrix (added.eff)

label <- paste("hw",rep(c(2,4),each=3),rep(c(97,98,99),2),sep=".")
table1 <- matrix(NA,6,7,dimnames=list(label,
                                      c("N comm","Est.main","95%CI.main","P-het.added","Est.added",
                                        "95%CI.added","P-het.added")))

for(i in 1:6) {
  # SET TO MISSING IF NO ESTIMATE FOR ADDED EFFECT
  added.eff[added.eff[,2*i]==0,c(2*i-1,2*i)] <- NA
  main.eff[is.na(added.eff[,2*i]),c(2*i-1,2*i)] <- NA
  # RUN THE META-ANALYSIS
  pool.main <- rma.uni(yi=main.eff[,2*i-1],sei=main.eff[,2*i])
  pool.added <- rma.uni(yi=added.eff[,2*i-1],sei=added.eff[,2*i])
  # FILL TABLE1
  table1[i,] <- c(sum(!is.na(added.eff[,2*i-1])),
                  round(exp(pool.main$b)*100-100,3),
                  paste(round(exp(pool.main$b-1.96*pool.main$se)*100-100,3),"to",
                        round(exp(pool.main$b+1.96*pool.main$se)*100-100,3)),
                  round(pool.main$QEp,3),
                  round(exp(pool.added$b)*100-100,3),
                  paste(round(exp(pool.added$b-1.96*pool.added$se)*100-100,3),"to",
                        round(exp(pool.added$b+1.96*pool.added$se)*100-100,3)),
                  round(pool.added$QEp,3))
}
table1
### Table RR ###
label <- paste("hw",rep(c(2,4),each=3),rep(c(97,98,99),2),sep=".")
table2 <- matrix(NA,6,7,dimnames=list(label,
                                      c("N comm","Est.main","95%CI.main","P-het.added","Est.added",
                                        "95%CI.added","P-het.added")))
for(i in 1:6) {
  # SET TO MISSING IF NO ESTIMATE FOR ADDED EFFECT
  added.eff[added.eff[,2*i]==0,c(2*i-1,2*i)] <- NA
  main.eff[is.na(added.eff[,2*i]),c(2*i-1,2*i)] <- NA
  # RUN THE META-ANALYSIS
  pool.main <- rma.uni(yi=main.eff[,2*i-1],sei=main.eff[,2*i])
  pool.added <- rma.uni(yi=added.eff[,2*i-1],sei=added.eff[,2*i])
  # FILL TABLE1
  table2[i,] <- c(sum(!is.na(added.eff[,2*i-1])),
                  round(exp(pool.main$b),3),
                  paste(round(exp(pool.main$b-1.96*pool.main$se),3),"to",
                        round(exp(pool.main$b+1.96*pool.main$se),3)),
                  round(pool.main$QEp,3),
                  round(exp(pool.added$b),3),
                  paste(round(exp(pool.added$b-1.96*pool.added$se),3),"to",
                        round(exp(pool.added$b+1.96*pool.added$se),3)),
                  round(pool.added$QEp,3))
}
table2

#####Pooed effect of HW all-icdL###

library(readxl)
icdL2_result_farmers_20_01 <- read_excel("~/Documents/OneDrive - UMP/Projects/farmer/hw results/hw results prepare for stage 2/hw.20.01/all/untitled folder/icdL2_result_farmers_20.01.xlsx", 
                                         sheet = "all.main")


main.eff<-icdL2_result_farmers_20_01
main.eff<- as.matrix (main.eff)

library(readxl)
icdL2_result_farmers_20_01 <- read_excel("~/Documents/OneDrive - UMP/Projects/farmer/hw results/hw results prepare for stage 2/hw.20.01/all/untitled folder/icdL2_result_farmers_20.01.xlsx", 
                                         sheet = "all.add")

added.eff<-icdL2_result_farmers_20_01
added.eff<- as.matrix (added.eff)

label <- paste("hw",rep(c(2,4),each=3),rep(c(97,98,99),2),sep=".")
table1 <- matrix(NA,6,7,dimnames=list(label,
                                      c("N comm","Est.main","95%CI.main","P-het.added","Est.added",
                                        "95%CI.added","P-het.added")))

for(i in 1:6) {
  # SET TO MISSING IF NO ESTIMATE FOR ADDED EFFECT
  added.eff[added.eff[,2*i]==0,c(2*i-1,2*i)] <- NA
  main.eff[is.na(added.eff[,2*i]),c(2*i-1,2*i)] <- NA
  # RUN THE META-ANALYSIS
  pool.main <- rma.uni(yi=main.eff[,2*i-1],sei=main.eff[,2*i])
  pool.added <- rma.uni(yi=added.eff[,2*i-1],sei=added.eff[,2*i])
  # FILL TABLE1
  table1[i,] <- c(sum(!is.na(added.eff[,2*i-1])),
                  round(exp(pool.main$b)*100-100,3),
                  paste(round(exp(pool.main$b-1.96*pool.main$se)*100-100,3),"to",
                        round(exp(pool.main$b+1.96*pool.main$se)*100-100,3)),
                  round(pool.main$QEp,3),
                  round(exp(pool.added$b)*100-100,3),
                  paste(round(exp(pool.added$b-1.96*pool.added$se)*100-100,3),"to",
                        round(exp(pool.added$b+1.96*pool.added$se)*100-100,3)),
                  round(pool.added$QEp,3))
}
table1
### Table RR ###
label <- paste("hw",rep(c(2,4),each=3),rep(c(97,98,99),2),sep=".")
table2 <- matrix(NA,6,7,dimnames=list(label,
                                      c("N comm","Est.main","95%CI.main","P-het.added","Est.added",
                                        "95%CI.added","P-het.added")))
for(i in 1:6) {
  # SET TO MISSING IF NO ESTIMATE FOR ADDED EFFECT
  added.eff[added.eff[,2*i]==0,c(2*i-1,2*i)] <- NA
  main.eff[is.na(added.eff[,2*i]),c(2*i-1,2*i)] <- NA
  # RUN THE META-ANALYSIS
  pool.main <- rma.uni(yi=main.eff[,2*i-1],sei=main.eff[,2*i])
  pool.added <- rma.uni(yi=added.eff[,2*i-1],sei=added.eff[,2*i])
  # FILL TABLE1
  table2[i,] <- c(sum(!is.na(added.eff[,2*i-1])),
                  round(exp(pool.main$b),3),
                  paste(round(exp(pool.main$b-1.96*pool.main$se),3),"to",
                        round(exp(pool.main$b+1.96*pool.main$se),3)),
                  round(pool.main$QEp,3),
                  round(exp(pool.added$b),3),
                  paste(round(exp(pool.added$b-1.96*pool.added$se),3),"to",
                        round(exp(pool.added$b+1.96*pool.added$se),3)),
                  round(pool.added$QEp,3))
}
table2

#####Pooed effect of HW all-icdN###
library(readxl)
icdN2_result_farmers_20_01 <- read_excel("~/Documents/OneDrive - UMP/Projects/farmer/hw results/hw results prepare for stage 2/hw.20.01/all/untitled folder/icdN2_result_farmers_20.01.xlsx", 
                                         sheet = "all.main")


main.eff<-icdN2_result_farmers_20_01
main.eff<- as.matrix (main.eff)

library(readxl)
icdN2_result_farmers_20_01 <- read_excel("~/Documents/OneDrive - UMP/Projects/farmer/hw results/hw results prepare for stage 2/hw.20.01/all/untitled folder/icdN2_result_farmers_20.01.xlsx", 
                                         sheet = "all.add")

added.eff<-icdN2_result_farmers_20_01
added.eff<- as.matrix (added.eff)

label <- paste("hw",rep(c(2,4),each=3),rep(c(97,98,99),2),sep=".")
table1 <- matrix(NA,6,7,dimnames=list(label,
                                      c("N comm","Est.main","95%CI.main","P-het.added","Est.added",
                                        "95%CI.added","P-het.added")))

for(i in 1:6) {
  # SET TO MISSING IF NO ESTIMATE FOR ADDED EFFECT
  added.eff[added.eff[,2*i]==0,c(2*i-1,2*i)] <- NA
  main.eff[is.na(added.eff[,2*i]),c(2*i-1,2*i)] <- NA
  # RUN THE META-ANALYSIS
  pool.main <- rma.uni(yi=main.eff[,2*i-1],sei=main.eff[,2*i])
  pool.added <- rma.uni(yi=added.eff[,2*i-1],sei=added.eff[,2*i])
  # FILL TABLE1
  table1[i,] <- c(sum(!is.na(added.eff[,2*i-1])),
                  round(exp(pool.main$b)*100-100,3),
                  paste(round(exp(pool.main$b-1.96*pool.main$se)*100-100,3),"to",
                        round(exp(pool.main$b+1.96*pool.main$se)*100-100,3)),
                  round(pool.main$QEp,3),
                  round(exp(pool.added$b)*100-100,3),
                  paste(round(exp(pool.added$b-1.96*pool.added$se)*100-100,3),"to",
                        round(exp(pool.added$b+1.96*pool.added$se)*100-100,3)),
                  round(pool.added$QEp,3))
}
table1
### Table RR ###
label <- paste("hw",rep(c(2,4),each=3),rep(c(97,98,99),2),sep=".")
table2 <- matrix(NA,6,7,dimnames=list(label,
                                      c("N comm","Est.main","95%CI.main","P-het.added","Est.added",
                                        "95%CI.added","P-het.added")))
for(i in 1:6) {
  # SET TO MISSING IF NO ESTIMATE FOR ADDED EFFECT
  added.eff[added.eff[,2*i]==0,c(2*i-1,2*i)] <- NA
  main.eff[is.na(added.eff[,2*i]),c(2*i-1,2*i)] <- NA
  # RUN THE META-ANALYSIS
  pool.main <- rma.uni(yi=main.eff[,2*i-1],sei=main.eff[,2*i])
  pool.added <- rma.uni(yi=added.eff[,2*i-1],sei=added.eff[,2*i])
  # FILL TABLE1
  table2[i,] <- c(sum(!is.na(added.eff[,2*i-1])),
                  round(exp(pool.main$b),3),
                  paste(round(exp(pool.main$b-1.96*pool.main$se),3),"to",
                        round(exp(pool.main$b+1.96*pool.main$se),3)),
                  round(pool.main$QEp,3),
                  round(exp(pool.added$b),3),
                  paste(round(exp(pool.added$b-1.96*pool.added$se),3),"to",
                        round(exp(pool.added$b+1.96*pool.added$se),3)),
                  round(pool.added$QEp,3))
}
table2
#####Pooed effect of HW all-d_0060###
library(readxl)
d00602_result_farmers_20_01 <- read_excel("~/Documents/OneDrive - UMP/Projects/farmer/hw results/hw results prepare for stage 2/hw.20.01/all/untitled folder/d00602_result_farmers_20.01.xlsx", 
                                          sheet = "all.main")


main.eff<-d00602_result_farmers_20_01
main.eff<- as.matrix (main.eff)

library(readxl)
d00602_result_farmers_20_01 <- read_excel("~/Documents/OneDrive - UMP/Projects/farmer/hw results/hw results prepare for stage 2/hw.20.01/all/untitled folder/d00602_result_farmers_20.01.xlsx", 
                                          sheet = "all.add")

added.eff<-d00602_result_farmers_20_01
added.eff<- as.matrix (added.eff)

label <- paste("hw",rep(c(2,4),each=3),rep(c(97,98,99),2),sep=".")
table1 <- matrix(NA,6,7,dimnames=list(label,
                                      c("N comm","Est.main","95%CI.main","P-het.added","Est.added",
                                        "95%CI.added","P-het.added")))

for(i in 1:6) {
  # SET TO MISSING IF NO ESTIMATE FOR ADDED EFFECT
  added.eff[added.eff[,2*i]==0,c(2*i-1,2*i)] <- NA
  main.eff[is.na(added.eff[,2*i]),c(2*i-1,2*i)] <- NA
  # RUN THE META-ANALYSIS
  pool.main <- rma.uni(yi=main.eff[,2*i-1],sei=main.eff[,2*i])
  pool.added <- rma.uni(yi=added.eff[,2*i-1],sei=added.eff[,2*i])
  # FILL TABLE1
  table1[i,] <- c(sum(!is.na(added.eff[,2*i-1])),
                  round(exp(pool.main$b)*100-100,3),
                  paste(round(exp(pool.main$b-1.96*pool.main$se)*100-100,3),"to",
                        round(exp(pool.main$b+1.96*pool.main$se)*100-100,3)),
                  round(pool.main$QEp,3),
                  round(exp(pool.added$b)*100-100,3),
                  paste(round(exp(pool.added$b-1.96*pool.added$se)*100-100,3),"to",
                        round(exp(pool.added$b+1.96*pool.added$se)*100-100,3)),
                  round(pool.added$QEp,3))
}
table1
### Table RR ###
label <- paste("hw",rep(c(2,4),each=3),rep(c(97,98,99),2),sep=".")
table2 <- matrix(NA,6,7,dimnames=list(label,
                                      c("N comm","Est.main","95%CI.main","P-het.added","Est.added",
                                        "95%CI.added","P-het.added")))
for(i in 1:6) {
  # SET TO MISSING IF NO ESTIMATE FOR ADDED EFFECT
  added.eff[added.eff[,2*i]==0,c(2*i-1,2*i)] <- NA
  main.eff[is.na(added.eff[,2*i]),c(2*i-1,2*i)] <- NA
  # RUN THE META-ANALYSIS
  pool.main <- rma.uni(yi=main.eff[,2*i-1],sei=main.eff[,2*i])
  pool.added <- rma.uni(yi=added.eff[,2*i-1],sei=added.eff[,2*i])
  # FILL TABLE1
  table2[i,] <- c(sum(!is.na(added.eff[,2*i-1])),
                  round(exp(pool.main$b),3),
                  paste(round(exp(pool.main$b-1.96*pool.main$se),3),"to",
                        round(exp(pool.main$b+1.96*pool.main$se),3)),
                  round(pool.main$QEp,3),
                  round(exp(pool.added$b),3),
                  paste(round(exp(pool.added$b-1.96*pool.added$se),3),"to",
                        round(exp(pool.added$b+1.96*pool.added$se),3)),
                  round(pool.added$QEp,3))
}
table2

#####Pooed effect of HW all-d_61###
library(readxl)
d612_result_farmers_20_01 <- read_excel("~/Documents/OneDrive - UMP/Projects/farmer/hw results/hw results prepare for stage 2/hw.20.01/all/untitled folder/d612_result_farmers_20.01.xlsx", 
                                        sheet = "all.main")

main.eff<-d612_result_farmers_20_01
main.eff<- as.matrix (main.eff)

library(readxl)
d612_result_farmers_20_01 <- read_excel("~/Documents/OneDrive - UMP/Projects/farmer/hw results/hw results prepare for stage 2/hw.20.01/all/untitled folder/d612_result_farmers_20.01.xlsx", 
                                        sheet = "all.add")

added.eff<- d612_result_farmers_20_01
added.eff<- as.matrix (added.eff)

label <- paste("hw",rep(c(2,4),each=3),rep(c(97,98,99),2),sep=".")
table1 <- matrix(NA,6,7,dimnames=list(label,
                                      c("N comm","Est.main","95%CI.main","P-het.added","Est.added",
                                        "95%CI.added","P-het.added")))

for(i in 1:6) {
  # SET TO MISSING IF NO ESTIMATE FOR ADDED EFFECT
  added.eff[added.eff[,2*i]==0,c(2*i-1,2*i)] <- NA
  main.eff[is.na(added.eff[,2*i]),c(2*i-1,2*i)] <- NA
  # RUN THE META-ANALYSIS
  pool.main <- rma.uni(yi=main.eff[,2*i-1],sei=main.eff[,2*i])
  pool.added <- rma.uni(yi=added.eff[,2*i-1],sei=added.eff[,2*i])
  # FILL TABLE1
  table1[i,] <- c(sum(!is.na(added.eff[,2*i-1])),
                  round(exp(pool.main$b)*100-100,3),
                  paste(round(exp(pool.main$b-1.96*pool.main$se)*100-100,3),"to",
                        round(exp(pool.main$b+1.96*pool.main$se)*100-100,3)),
                  round(pool.main$QEp,3),
                  round(exp(pool.added$b)*100-100,3),
                  paste(round(exp(pool.added$b-1.96*pool.added$se)*100-100,3),"to",
                        round(exp(pool.added$b+1.96*pool.added$se)*100-100,3)),
                  round(pool.added$QEp,3))
}
table1
### Table RR ###
label <- paste("hw",rep(c(2,4),each=3),rep(c(97,98,99),2),sep=".")
table2 <- matrix(NA,6,7,dimnames=list(label,
                                      c("N comm","Est.main","95%CI.main","P-het.added","Est.added",
                                        "95%CI.added","P-het.added")))
for(i in 1:6) {
  # SET TO MISSING IF NO ESTIMATE FOR ADDED EFFECT
  added.eff[added.eff[,2*i]==0,c(2*i-1,2*i)] <- NA
  main.eff[is.na(added.eff[,2*i]),c(2*i-1,2*i)] <- NA
  # RUN THE META-ANALYSIS
  pool.main <- rma.uni(yi=main.eff[,2*i-1],sei=main.eff[,2*i])
  pool.added <- rma.uni(yi=added.eff[,2*i-1],sei=added.eff[,2*i])
  # FILL TABLE1
  table2[i,] <- c(sum(!is.na(added.eff[,2*i-1])),
                  round(exp(pool.main$b),3),
                  paste(round(exp(pool.main$b-1.96*pool.main$se),3),"to",
                        round(exp(pool.main$b+1.96*pool.main$se),3)),
                  round(pool.main$QEp,3),
                  round(exp(pool.added$b),3),
                  paste(round(exp(pool.added$b-1.96*pool.added$se),3),"to",
                        round(exp(pool.added$b+1.96*pool.added$se),3)),
                  round(pool.added$QEp,3))
}
table2
