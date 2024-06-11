#Code by Nick Bertrand
#nbertrand@usbr.gov

#this script will create the graph for the DCC gate opening and closures which are triggered
#by the catch indexes

library(readxl)
library(tidyverse)
library(scales)

#take in data files from SacPas from site below
#https://www.cbr.washington.edu/sacramento/data/query_sampling_graph.html

#data files pulled for WY2023 imported to R
#files need to be combined into the a single data file

library(readxl)
Controlling_Factors_Table_WY_2023_v4 <- read_excel("DCCgateOpeningsandClosures/Controlling Factors Table WY 2023_v4.xlsx")
#View(Controlling_Factors_Table_WY_2023_v4)

Controlling_Factors_Table_WY_2023_v4$Date <- as.POSIXct(Controlling_Factors_Table_WY_2023_v4$Date, tz="GMT")
class(Controlling_Factors_Table_WY_2023_v4$Date)

WY2023_WR_KLCI <- read_csv("DCCgateOpeningsandClosures/WY2023_WR_KLCI.csv")
#removes SacPas Disclaimer text from dataframe
WY2023_WR_KLCI <- WY2023_WR_KLCI %>% slice(1:243)
#View(WY2023_WR_KLCI)
KLCI <- WY2023_WR_KLCI %>% rename(KLCI = `Raw Knights Landing RST`)

WY2023_WRSCI_Seines <- read_csv("DCCgateOpeningsandClosures/WY2023_WRSCI_Seines.csv")
WY2023_WRSCI_Seines <- WY2023_WRSCI_Seines %>% slice(1:365)
#View(WY2023_WRSCI_Seines)

SCI_Seines <- WY2023_WRSCI_Seines %>% rename(sci_seines = `Index Sacramento Beach Seines (SR080E SR071E SR062E SR057E SR055E SR060E AM001S SR049E)`)

WY2023_WRSCI_Trawls <- read_csv("DCCgateOpeningsandClosures/WY2023_WRSCI_Trawls.csv")
WY2023_WRSCI_Trawls <- WY2023_WRSCI_Trawls %>% slice(1:361)
#View(WY2023_WRSCI_Trawls)

SCI_Trawls <- WY2023_WRSCI_Trawls %>% 
  rename(sci_trawls = `Index Sacramento Trawls (SR055M SR055E SR055W SR055X)`)

#joins all dataframes
dcc_SCI <-left_join(SCI_Seines,SCI_Trawls, by = "Date") 
dcc_SCI$Date <- as.POSIXct(dcc_SCI$Date, tz="GMT")
#View(dcc_SCI)
class((dcc_SCI$Date))

KLCI$Date <- as.POSIXct(KLCI$Date, tz="GMT")

dcc_SCI_KLCI <- left_join(dcc_SCI, KLCI, by = "Date")
#view(dcc_SCI_KLCI)
dcc_SCI_KLCI$Date <- as.POSIXct(dcc_SCI_KLCI$Date, tz="GMT")
class(dcc_SCI_KLCI$Date)

dccgate <- left_join(dcc_SCI_KLCI, Controlling_Factors_Table_WY_2023_v4, by = "Date") %>% 
  mutate(`DCC Gate Open` = ifelse(`DCC Gate Status` == "O", 2,0))
#view(dccgate)

#Delete unnecessary columns, gather by index label, & filter to OMR season dates
dcc_data <- dccgate %>% 
  select(Date, sci_seines, sci_trawls, KLCI, `DCC Gate Open`, `DCC Gate Status`) %>% 
  gather("sci_seines", "sci_trawls", "KLCI", key = "Name", value = "Index") %>% 
  filter(Date >= "2022-10-01" & Date <= "2023-06-30")

#view(dcc_data)
head(dcc_data)

rects1 <- data.frame(xstart = seq(as.Date("2022-10-01"),as.Date("2023-07-01"),1), 
                    xend = seq(as.Date("2022-10-01"),as.Date("2023-07-01"),1)) 
rects <- rects1 %>% mutate(xend2 = xstart + 1)
#View(rects)
status <-dcc_data %>% select(Date, `DCC Gate Status`) 
#view(status)
rects <- left_join(rects,status, by = join_by(xstart == Date))

rects <- rects %>% drop_na()


#View(rects)


dcc_plot <- ggplot()+
  geom_rect(data = rects, inherit.aes=FALSE, 
            mapping=aes(xmin = as.Date(xstart), xmax = as.Date(xend2),
                        ymin = -Inf, ymax = Inf,fill = `DCC Gate Status`), alpha = 0.4)+
  scale_fill_manual(values = alpha(c("white", "grey"), 0.2))+
  geom_point(dcc_data,mapping = aes(x = as.Date(Date), y = Index, shape = Name))+
  theme_classic()+
  scale_x_date(name= "Date", breaks = date_breaks("months"),labels = date_format("%m/%d")) +
  scale_y_continuous(name = "Catch Index", breaks = pretty_breaks(n = 5))+
  theme(axis.text.x = element_text(angle=45),legend.position="right")

dcc_plot







gate_plot <- ggplot(dcc_data, aes(x = as.Date(Date), y = `DCC Gate Open`))+
  geom_bar(stat = 'identity')+
  theme_classic()+
  scale_x_date(name= "Date", breaks = date_breaks("months"),labels = date_format("%m/%d")) +
  theme(axis.text.x = element_text(angle=45),legend.position = "bottom")

gate_plot


