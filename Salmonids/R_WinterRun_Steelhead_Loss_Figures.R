library(tidyverse)
library(rvest)
library(lubridate)
library(splitstackshape)
library(data.table)
library(readxl)
library(sharpshootR)


##################### Load Salvage Count Data from SacPAS

#Function adjusted from Trinh's code to pull salvage datasets from SacPAS
pull_salvage <- function(salvageURL = "http://www.cbr.washington.edu/sacramento/data/query_loss_detail.html") {
  startingSession <- session(salvageURL)
  startingForm <- html_form(startingSession)[[1]]
  
  df <- lapply(startingForm$fields$year$options, function(x) {
    filledForm <- set_values(startingForm,
                             year = x,
                             species = "1:f")
    
    submittedFormURL <- suppressMessages(submit_form(session = startingSession, 
                                                     form = filledForm, POST = salvageURL)$url)
    
    csvLink <- submittedFormURL
    
    if (length(csvLink) == 0) {
      return(NULL)
    } else {
      csvDownload <- csvLink
    }
    
    df <- csvDownload %>% 
      read_csv() %>% filter(!is.na(nfish)) }) %>%
    bind_rows() 
  df
}

#Run actual function to load data
salvage_data <- suppressWarnings(pull_salvage())

########## Prep salvage data

#Rename columns to make it easier to work in R and divide Loss + Expanded Salvage by nfish
salvage_data_adjusted<- salvage_data %>%
  rename(SampleTime='Sample Time',LAD_Race='LAD Race',SampleFraction='Sample Fraction',ExpandedSalvage='Expanded Salvage',LAD_Loss='LAD Loss') %>%
  mutate(ExpandedSalvage=ExpandedSalvage/nfish, LAD_Loss=LAD_Loss/nfish)


#Multiply rows by nfish
salvage_data_adjusted<- setDT(expandRows(salvage_data_adjusted, "nfish")) 

salvage_data_adjusted<- salvage_data_adjusted%>%
  # build grouping by combination of variables
  dplyr::group_by(SampleTime, LAD_Race, Length) %>%
  # add row number which works per group due to prior grouping
  dplyr::mutate(duplicateID = dplyr::row_number()) %>%
  # ungroup to prevent unexpected behaviour down stream
  dplyr::ungroup()

#Adjust Sample Time to the proper format
salvage_data_adjusted$SampleTime <- as.POSIXlt(salvage_data_adjusted$SampleTime,format='%Y-%m-%d  %H:%M:%S', tz = "UTC")

########Load distribution data from SaMT
distribution_data<-read_excel(file.path("data","DistributionEstimates_WOMT_WY2023.xlsx"),sheet="DATA Dist WOMT Export OMRrange")
str(distribution_data)

at <- seq(from = min(distribution_data$Date), to = max(distribution_data$Date), by = "month")

########Load Freeport data

Freeport_flow<-CDECquery(id='FPT', sensor=20, interval='D', start='2022-10-01', end=Sys.Date())
str(Freeport_flow)

write.csv(Freeport_flow, row.names = F, file=file.path("output","Freeport_CDEC.csv"))
##############################

##Figure: Total loss of natural winter-run Chinook Salmon in WY


#Cumulative Loss data for WR
salvage_data_cum<-salvage_data_adjusted %>% filter(as.Date(SampleTime)>"2022-10-01") %>% filter(LAD_Race=="Winter")
str(salvage_data_cum)

salvage_data_cum<- salvage_data_cum %>% add_row(SampleTime = as.POSIXlt("2022-10-01 12:00:00"), LAD_Race = "Winter", ExpandedSalvage=0, LAD_Loss=0)
salvage_data_cum<- salvage_data_cum %>% add_row(SampleTime = as.POSIXlt("2023-06-30 12:00:00"), LAD_Race = "Winter", ExpandedSalvage=0, LAD_Loss=0)

#Order by date
salvage_data_cum <- salvage_data_cum[order(salvage_data_cum$SampleTime),] %>%
  mutate(CumulativeLoss = cumsum(LAD_Loss))

sum(salvage_data_cum %>% filter(Facility=="SWP") %>% select(LAD_Loss))
sum(salvage_data_cum %>% filter(Facility=="CVP") %>% select(LAD_Loss))
20.56/(sum(salvage_data_cum %>% filter(Facility=="SWP") %>% select(LAD_Loss))+
       sum(salvage_data_cum %>% filter(Facility=="CVP") %>% select(LAD_Loss)))

salvage_data_dailyloss<-salvage_data_cum %>% filter(LAD_Loss>0)

#Print figure
tiff(filename=file.path("output","Figure_Total_loss_winter-run.tiff"),width=10,height = 8, units = "in",  res=300, compression ="lzw")

par(mar = c(5.1, 4.1, 4.1, 4.1))
plot(Natural_WR_YTE ~ Date, distribution_data, xaxt = "n", type = "l", col="black",main="",ylab="Percentage of Population",xlab="")
lines(distribution_data$Date, distribution_data$Natural_WR_ID, col = "black", type = "l", lty = 2)
lines(distribution_data$Date, distribution_data$Natural_WR_E, col = "black", type = "l", lty = 3)
axis(1, at=at, format(at, "%b %d"), cex.axis = .7)
mtext("Natural Winter-Run Chinook Salmon Loss (n)", side = 4, line = 3)
legend(min(distribution_data$Date), 60, legend=c("Yet to enter Delta", "In Delta", "Exited Delta","Annual Cumulative Loss","Daily Loss"),
       col=c("black", "black",'black',"red","black"), lty=c(1,2,3,1,0), lwd=c(1,1,1,2,1), pch=c(NA,NA,NA,NA,16), cex=0.8)
par(new=TRUE)
plot(salvage_data_cum$SampleTime, salvage_data_cum$CumulativeLoss, col = "red", type = "l", lty = 1, lwd= 2, ylim=c(0,120),yaxt="n",ylab="",xaxt="n",xlab="")
#lines(salvage_data_cum$SampleTime, salvage_data_cum$CumulativeLoss, col = "red", type = "l", lty = 1, lwd= 2, ylim=c(0,120))
points(salvage_data_dailyloss$SampleTime, salvage_data_dailyloss$LAD_Loss, pch=19)
axis(side = 4, at = pretty(range(salvage_data_cum$CumulativeLoss))) 

dev.off()

##############################
##Figure: Freeport flows (cfs) and combined natural winter-run Chinook Salmon loss at the CVP and SWP

tiff(filename=file.path("output","Figure_Freeportflow_and_WinterRun.tiff"),width=10,height = 8, units = "in",  res=300, compression ="lzw")

par(mar = c(5.1, 4.1, 4.1, 4.1))
plot(as.Date(Freeport_flow$datetime),Freeport_flow$value, type = "l", lty=2, lwd=2,col="black",main="",ylab="Freeport flow (cfs)",xlab="")
par(new = TRUE)                             # Add new plot
plot(as.Date(salvage_data_cum$SampleTime), salvage_data_cum$CumulativeLoss, col = "red", type = "l", lty = 1, lwd= 2,axes = FALSE, xlab = "", ylab = "")
points(as.Date(salvage_data_dailyloss$SampleTime), salvage_data_dailyloss$LAD_Loss, pch=19)
axis(side = 4, at = pretty(range(salvage_data_cum$CumulativeLoss))) 
mtext("Natural Winter-Run Chinook Salmon Loss (n)", side = 4, line = 3)
legend(as.Date(c("2022-04-30")), 50, legend=c("Freeport flow (cfs)","Annual Cumulative Loss","Daily Loss"),
       col=c('black',"red","black"), lty=c(2,1,0), lwd=c(1,2,1), pch=c(NA,NA,16), cex=0.8)


dev.off()



##############################
##Figure: Freeport flows (cfs) and combined natural Hatchery winter-run loss at the CVP and SWP
### NO HATCHERY WR in WY 2023. SKIP

#Function adjusted from Trinh's code to pull salvage datasets from SacPAS
pull_salvage <- function(salvageURL = "http://www.cbr.washington.edu/sacramento/data/query_loss_detail.html") {
  startingSession <- session(salvageURL)
  startingForm <- html_form(startingSession)[[1]]
  
  df <- lapply(startingForm$fields$year$options, function(x) {
    filledForm <- set_values(startingForm,
                             year = x,
                             species = "1:t")
    
    submittedFormURL <- suppressMessages(submit_form(session = startingSession, 
                                                     form = filledForm, POST = salvageURL)$url)
    
    csvLink <- submittedFormURL
    
    if (length(csvLink) == 0) {
      return(NULL)
    } else {
      csvDownload <- csvLink
    }
    
    df <- csvDownload %>% 
      read_csv() %>% filter(!is.na(nfish)) }) %>%
    bind_rows() 
  df
}

#Run actual function to load data
hatchery_wr_data <- suppressWarnings(pull_salvage())

hatchery_wr_data$SampleTime<-hatchery_wr_data$'Sample Time'
hatchery_wr_data$CWTRace<-hatchery_wr_data$`CWT Race`
hatchery_wr_data$CWTLoss<-hatchery_wr_data$`CWT Loss`
str(hatchery_wr_data)
hatchery_wr_data$SampleTime<-as.POSIXct(hatchery_wr_data$SampleTime)

hatchery_wr_data <- hatchery_wr_data %>% filter(CWTRace=="Winter"&SampleTime>(as.POSIXct("2022-09-30")))


#Order by date
salvage_data_hatchery_wr_cum <- hatchery_wr_data[order(hatchery_wr_data$SampleTime),] %>%
  mutate(CumulativeLoss = cumsum(CWTLoss), Date=as.Date(SampleTime))

distribution_data_hatchery<- left_join(distribution_data,salvage_data_hatchery_wr_cum)
distribution_data_hatchery$`CWT Loss`[is.na(distribution_data_hatchery$`CWT Loss`)]<-0
distribution_data_hatchery$CumulativeLoss<-cumsum(distribution_data_hatchery$`CWT Loss`)

#Print figure
tiff(filename=file.path("output","Figure_Total_loss_winter-run_hatchery.tiff"),width=10,height = 8, units = "in",  res=300, compression ="lzw")

par(mar = c(5.1, 4.1, 4.1, 4.1))
plot(Hatch_WR_YTE ~ Date, distribution_data_hatchery, xaxt = "n", type = "l", col="black",main="Hatchery Winter-Run",ylab="Percentage of Population",xlab="")
lines(distribution_data_hatchery$Date, distribution_data_hatchery$Hatch_WR_ID, col = "black", type = "l", lty = 2)
lines(distribution_data_hatchery$Date, distribution_data_hatchery$Hatch_WR_E, col = "black", type = "l", lty = 3)
par(new = TRUE)                             # Add new plot
plot(distribution_data_hatchery$Date, distribution_data_hatchery$CWTLoss, pch=19,ylim=c(0,10),xlab="",ylab="",xaxt="n",yaxt="n")
lines(distribution_data_hatchery$Date, distribution_data_hatchery$CumulativeLoss, col = "red", type = "l", lty = 1, lwd= 2)
axis(side = 4, at = pretty(range(c(0,10),na.rm = T))) 

axis(1, at=at, format(at, "%b %d"), cex.axis = .7)
mtext("Hatchery Winter-Run Chinook Salmon Loss (n)", side = 4, line = 3)
legend(min(distribution_data$Date), 8, legend=c("Yet to enter Delta", "In Delta", "Exited Delta","Annual Cumulative Loss","Daily Loss"),
       col=c("black", "black",'black',"red","black"), lty=c(1,2,3,1,0), lwd=c(1,1,1,2,1), pch=c(NA,NA,NA,NA,16), cex=0.8)

dev.off()


str(distribution_data)



##############################
##Figure: Freeport flows (cfs) and combined natural Steelhead loss at the CVP and SWP

#Acquire steelhead loss data

#Function adjusted from Trinh's code to pull salvage datasets from SacPAS
pull_salvage <- function(salvageURL = "http://www.cbr.washington.edu/sacramento/data/query_loss_detail.html") {
  startingSession <- session(salvageURL)
  startingForm <- html_form(startingSession)[[1]]
  
  df <- lapply(startingForm$fields$year$options, function(x) {
    filledForm <- set_values(startingForm,
                             year = x,
                             species = "2:f")
    
    submittedFormURL <- suppressMessages(submit_form(session = startingSession, 
                                                     form = filledForm, POST = salvageURL)$url)
    
    csvLink <- submittedFormURL
    
    if (length(csvLink) == 0) {
      return(NULL)
    } else {
      csvDownload <- csvLink
    }
    
    df <- csvDownload %>% 
      read_csv() %>% filter(!is.na(nfish)) }) %>%
    bind_rows() 
  df
}

#Run actual function to load data
sth_salvage_data <- suppressWarnings(pull_salvage())
sth_salvage_data$SampleTime<-sth_salvage_data$'Sample Time'
sth_salvage_data$ExpandedSalvage<-sth_salvage_data$'Expanded Salvage'

#Dec-March
sth_salvage_data_decmar<-sth_salvage_data %>% mutate(SampleTime=as.POSIXlt(SampleTime)) %>% filter(SampleTime>="2022-10-01"&SampleTime<"2023-04-01")
sth_salvage_data_decmar<- sth_salvage_data_decmar %>% add_row(SampleTime = as.POSIXlt("2022-10-01 12:00:00"), ExpandedSalvage=0, Loss=0)
sth_salvage_data_decmar<- sth_salvage_data_decmar %>% add_row(SampleTime = as.POSIXlt("2023-04-01 12:00:00"), ExpandedSalvage=0, Loss=0)

sth_salvage_data_decmar <- sth_salvage_data_decmar[order(sth_salvage_data_decmar$SampleTime),] %>%
  mutate(CumulativeLoss = cumsum(Loss))

#SWP ratio
sum(sth_salvage_data_decmar %>% filter(Facility=="SWP") %>% select(Loss))/(sum(sth_salvage_data_decmar %>% filter(Facility=="SWP") %>% select(Loss))+
         sum(sth_salvage_data_decmar %>% filter(Facility=="CVP") %>% select(Loss)))

sth_salvage_data_decmar_dailyloss<-sth_salvage_data_decmar %>% filter(Loss>0)

#April-June
sth_salvage_data_aprjun<-sth_salvage_data %>% mutate(SampleTime=as.POSIXlt(SampleTime)) %>% filter(SampleTime>="2023-04-01"&SampleTime<="2023-06-15")
sth_salvage_data_aprjun<- sth_salvage_data_aprjun %>% add_row(SampleTime = as.POSIXlt("2023-04-01 12:00:00"), ExpandedSalvage=0, Loss=0)
sth_salvage_data_aprjun<- sth_salvage_data_aprjun %>% add_row(SampleTime = as.POSIXlt("2023-06-16 12:00:00"), ExpandedSalvage=0, Loss=0)

sth_salvage_data_aprjun <- sth_salvage_data_aprjun[order(sth_salvage_data_aprjun$SampleTime),] %>%
  mutate(CumulativeLoss = cumsum(Loss))

#SWP ratio
sum(sth_salvage_data_aprjun %>% filter(Facility=="SWP") %>% select(Loss))/(sum(sth_salvage_data_aprjun %>% filter(Facility=="SWP") %>% select(Loss))+
                                                                             sum(sth_salvage_data_aprjun %>% filter(Facility=="CVP") %>% select(Loss)))

sth_salvage_data_aprjun_dailyloss<-sth_salvage_data_aprjun %>% filter(Loss>0)

#Plot
tiff(filename=file.path("Output","Figure_SteelheadLoss.tiff"),width=10,height = 8, units = "in",  res=300, compression ="lzw")

par(mar = c(5.1, 4.1, 4.1, 4.1))
plot(Natural_SH_YTE ~ Date, distribution_data, xaxt = "n", type = "l", col="black",main="Natural Steelhead",ylab="Percentage of Population",xlab="")
lines(distribution_data$Date, distribution_data$Natural_SH_ID, col = "black", type = "l", lty = 2)
lines(distribution_data$Date, distribution_data$Natural_SH_E, col = "black", type = "l", lty = 3)
axis(1, at=at, format(at, "%b %d"), cex.axis = .7)
legend(min(distribution_data$Date), 50, legend=c("Yet to enter Delta", "In Delta", "Exited Delta","Daily Loss","Annual Cumulative Loss (Dec-Mar)","Annual Cumulative Loss (Apr-Jun)"),
       col=c("black", "black","black","black","red","blue"), lty=c(1,2,3,0,1,1),pch=c(NA,NA,NA,16,NA,NA), cex=0.8)
par(new = TRUE)
#Use whichever period has more loss (in this case Apr-Jun)
plot(Natural_SH_YTE ~ Date, distribution_data, xaxt = "n", yaxt="n", type = "l", col="black",main="Natural Steelhead",ylab="Percentage of Population",xlab="",
     ylim=c(0,max(sth_salvage_data_decmar$CumulativeLoss)))
lines(sth_salvage_data_aprjun$SampleTime, sth_salvage_data_aprjun$CumulativeLoss, col = "blue", type = "l", lty = 1, lwd= 2)
points(sth_salvage_data_aprjun_dailyloss$SampleTime, sth_salvage_data_aprjun_dailyloss$Loss, pch=19)
lines(sth_salvage_data_decmar$SampleTime, sth_salvage_data_decmar$CumulativeLoss, col = "red", type = "l", lty = 1, lwd= 2)
points(sth_salvage_data_decmar_dailyloss$SampleTime, sth_salvage_data_decmar_dailyloss$Loss, pch=19)
axis(side = 4, at = pretty(range(sth_salvage_data_decmar_dailyloss$CumulativeLoss))) 
mtext("Natural Steelhead Loss (n)", side = 4, line = 3)

dev.off()
