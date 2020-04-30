# Paper on temporal Disaggregation
# Settiing working Directory
setwd("E:/publication/Paper3(2019)Denton n CL compare for BD/new-Paper(2019)/paper-3/analysis")
# Install the temporal disaggregation package "tempdisagg"
install.packages("tempdisagg")
# upload the temporal disaggregation package "tempdisagg" from the library to work space
library(tempdisagg)
#convert the xl data as CSV file
#assign the data to Workspace that is read the csv file form working directory as dt
dt<-read.csv("R-TD-data-paper-3-td.csv")
#convert the variable of td as time series data(one by one)
#For yearly export.a as time seriesdata as texport.a
texport.a<-ts(dt$export.a, frequency =1, start = 2009, end = 2018)
#For quarterly export.q as time series data as texport.q
texport.q<-ts(dt$export.q, frequency =4, start = c(2009,1), end = c(2018,4))
#For quarterly indicator.q as time series data as tindicator.q (2019Q1and 2019q2 for exprapolation) )
tindicator.q<-ts(dt$indicator.q, frequency =4, start = c(2009,1), end = c(2019,2))
#temporal disaggegation by Denton additive 1st difference yearly to quarterly with indicator
da1<-td(texport.a~0+tindicator.q,to="quarterly",method="denton",criterion="additive",h=1)
#get quarterly disaggregated results by Denton additive 1st difference
pda1<-predict(da1)
# convert quarterly disaggregated results as Excel (csv).s and save as excel "td.da1.csv in working directory
write.csv(pda1,file="td.da1.csv", row.names=F)
#temporal disaggegation by Denton additive 2nd difference yearly to quarterly with indicator
da2<-td(texport.a~0+tindicator.q,to="quarterly",method="denton",criterion="additive",h=2)
#get quarterly disaggregated results by Denton additive 2nd difference
pda2<-predict(da2)
# convert quarterly disaggregated results as Excel (csv).s and save as excel "td.da2.csv in working directory
write.csv(pda2,file="td.da2.csv", row.names=F)
#temporal disaggegation by Denton proportional 1st difference yearly to quarterly with indicator
dp1<-td(texport.a~0+tindicator.q,to="quarterly",method="denton",criterion="proportional",h=1)
#get quarterly disaggregated results by Denton additive 1st difference
pdp1<-predict(dp1)
# convert quarterly disaggregated results as Excel (csv).s and save as excel "td.dp1.csv in working directory
write.csv(pdp1,file="td.dp1.csv", row.names=F)
#temporal disaggegation by Denton proportional 2nd difference yearly to quarterly with indicator
dp2<-td(texport.a~0+tindicator.q,to="quarterly",method="denton",criterion="proportional",h=2)
#get quarterly disaggregated results by Denton additive 1st difference
pdp2<-predict(dp2)
# convert quarterly disaggregated results as Excel (csv).s and save as excel "td.dp2.csv in working directory
write.csv(pdp2,file="td.dp2.csv", row.names=F)
# combine the temporal disaggegated series by denton with the original given series
td.dall<-cbind(texport.q,pda1, pda2,pdp1,pdp2)
#convert combine  disaggregated denton results as Excel (csv).s and save as excel "td.dall.csv in working directory
write.csv(td.dall,file="td.dall.csv", row.names=T)
#Temporal disaggregation by Denton-cholette method
#temporal disaggegation of yearly to quarterlyby Denton-cholette -additive 1st difference
dca1<-td(texport.a~0+tindicator.q,to="quarterly",method="denton-cholette",criterion="additive",h=1)
#get quarterly disaggregated results by Denton-cholette -additive 1st difference
pdca1<-predict(dca1)
# convert quarterly disaggregated results as Excel (csv).s and save as excel "td.dca1.csv in working directory
write.csv(pdca1,file="td.dca1.csv", row.names=F)
#temporal disaggegation of yearly to quarterlyby Denton-cholette -additive 2nd difference
dca2<-td(texport.a~0+tindicator.q,to="quarterly",method="denton-cholette",criterion="additive",h=2)
#get quarterly disaggregated results by Denton-cholette -additive 2nd difference
pdca2<-predict(dca2)
# convert quarterly disaggregated results as Excel (csv).s and save as excel "td.dca2.csv in working directory
write.csv(pdca2,file="td.dca2.csv", row.names=F)
#temporal disaggegation of yearly to quarterly by Denton-cholette-proportional 1st difference
dcp1<-td(texport.a~0+tindicator.q,to="quarterly",method="denton-cholette",criterion="proportional",h=1)
#get quarterly disaggregated results by Denton-cholette -proportional 1st difference
pdcp1<-predict(dcp1)
# convert quarterly disaggregated results as Excel (csv).s and save as excel "td.dcp1.csv in working directory
write.csv(pdcp1,file="td.dcp1.csv", row.names=F)
#temporal disaggegation of yearly to quarterly by Denton-cholette-proportional 2nd difference
dcp2<-td(texport.a~0+tindicator.q,to="quarterly",method="denton-cholette",criterion="proportional",h=2)
#get quarterly disaggregated results by Denton-cholette -proportional 2nd difference
pdcp2<-predict(dcp2)
# convert quarterly disaggregated results as Excel (csv).s and save as excel "td.dcp2.csv in working directory
write.csv(pdcp2,file="td.dcp2.csv", row.names=F)
# combine the temporal disaggegated series by  denton, denton-chowlette with the original given series
td.dcall<-cbind(texport.q,pdca1, pdca2,pdcp1,pdcp2)
#convert combine  disaggregated denton-cholette results as Excel (csv).s and save as excel "td.dcall.csv in working directory
write.csv(td.dcall,file="td.dcall.csv", row.names=T)

