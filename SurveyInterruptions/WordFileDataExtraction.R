##Code by Nick Bertrand
#nbertrand@usbr.gov

#this script will extract the 
#survey interruptions table from the weekly outlooks from the whole season 

#this script also includes the generation of the Sampling disruptions figure.
#this is the only script needed to generate the figure. 

library(docxtractr)
library(tidyverse)

#########################################
#A function to iterate through multiple files in a directory
#creates the dataframe to dump the data in
survey <- data.frame()
#reached to the directory with all the Outlook .docx files and creates a list of file names 
fileNames <- Sys.glob("C:/Users/nbertrand/Desktop/Bertrand/GitHub/OMRSeasonalReport/omr_report_2023/Survey Interruptions/WY2023Outlooks/*.docx")
#view(fileNames)

#function uses file name list
for (fileName in fileNames) {
  
  # to read in the data
  doc <- read_docx(fileName)
  #data is extracted based on table number
  data1 <- docx_extract_tbl(doc, 8)
  
  data2 <- data1 %>% mutate(week = names(data1)[3]) 
  
  data2$week <- gsub("Notes..as.of.", "", data2$week)
  #'.' does not work but for some reason '\\.' does as described by stack overflow
  data2$week <- gsub('\\.','/',data2$week)
  #cleans up date formating
  data2$week <- gsub('/2022/','/2022',data2$week)
  data2$week <- gsub('/2023/','/2023',data2$week)
  #renames Columns
  data3 <- data2 %>% rename(Survey = 1, Region = 2, Notes = 3, Status = 4, Week = 5)
  #binds extracted data back to the dump dataframe
  survey <- rbind(survey,data3)
}
View(survey)

#this formating corrects for the lack of zeros in some of the dates
#survey$Week <- format(as.Date(survey$Week, format = "%m/%d/%Y"), "%d/%m/%Y")
survey$Week <- format(as.POSIXct(survey$Week, format = "%m/%d/%Y"), "%Y-%m-%d")
#watch the date formatting and time zone here 
survey$Week <- as.POSIXct(survey$Week, tz="GMT")

#View(survey)
#class(survey$Week)
#safety check on formatting
survey$Week
#' #########################################
#' #extracts a table from one file, just wanted to save this code
#' #git hub example for read_docx is broken, code below worked
#' doc <- read_docx("C:/Users/nbertrand/Desktop/Bertrand/GitHub/OMRSeasonalReport/omr_report_2023/Survey Interruptions/WY2023Outlooks/20230627 fish and water operations outlook.docx")
#' 
#' #assigns number to all tables by counting
#' docx_tbl_count(doc)
#' #use number assigned in count to extract a specific table.
#' data <- docx_extract_tbl(doc, 8) %>% 
#'   mutate(week = names(data)[3]) 
#' data$week <- gsub("Notes..as.of.", "", data$week)
#' #'.' does not work but for some reason '\\.' does as described by stack overflow
#' data$week <- gsub('\\.','/',data$week)
#' data$week <- as.Date(data$week, "%m/%d/%y")
#' #renames Columns
#' data <- data %>%rename(Survey = 1, Region = 2, Notes = 3, Status = 4, Week = 5)
#' 
#' view(data)
#' 

########################
#the code below converts the extracted data from the outlooks into a format that can be 
#read into the gant chart script

########################

#I created a file in excel with the 7 day intervals in which the outlooks are
#written.

library(readxl)
WY2023StartandEndDates <- read_excel("Survey Interruptions/WY2023StartandEndDates.xlsx")
#View(WY2023StartandEndDates)

#safety check on formatting
survey$Week
#safety check on formatting of the dates
class(WY2023StartandEndDates$Start)

gant <- left_join(survey, WY2023StartandEndDates, join_by(Week == Start), keep = TRUE)

view(gant)

gant_input <- gant %>% 
  select(Start, End, Survey, Notes, Status) %>% 
  rename(start=Start, end = End, group = Survey, notes = Notes, category = Status) 


#minor formatting edits will need to be made in excel 
#adding an event column with no text except the column header

#this is also a good datafile to create a data check in case outputs look weird later

#write.csv(gant_input, file ="C:/Users/nbertrand/Desktop/Bertrand/GitHub/OMRSeasonalReport/omr_report_2023/Survey Interruptions/WY2023dataExtraction.csv" )


############
#GanttInteruptions.R is the script to use from here. 

####################################
###################

library(readr)
WY2023dataFormatted <- read_csv("Survey Interruptions/WY2023dataFormatted.csv")
#View(WY2023dataFormatted)
class(WY2023dataFormatted$category)

#################
#data frame with color codes to assign to status values (renamed category)
library(readr)
ColorCodes <- read_csv("Survey Interruptions/ColorCodes.csv")
View(ColorCodes)
class(ColorCodes$category)

inter_dat <-left_join(WY2023dataFormatted, ColorCodes, by = join_by("category"))
inter_dat
####################################
###############
#######

library(DiagrammeR)
library(htmlwidgets)
library(ggplot2)
library(readr)
library(plotly)
library(readxl)
library(vistime)
library("scales")
library("cowplot")


# library(readxl)
# actions <-  read_csv("C:/Users/nbertrand/Desktop/Bertrand/GitHub/OMRSeasonalReport/omr_report_2023/Survey Interruptions/WY2022_outlookdata6282022format.csv")
# View(actions)

#actions <- actions %>% select(event,group,start,end,color,category,notes) 

actions <- inter_dat

actions$start <- as.Date(actions$start)
actions$end <- as.Date(actions$end)


library(plotly)
d1 <- vistime(actions, optimize_y = T, col.group = "group", col.color = "color", linewidth = 25)
d1 %>% layout(title = "Disruptions of Sampling",
              xaxis = list(title = "Date", fixedrange=F, type = 'date', dtick ="M1",tickformat = "%b"))

#review the first run of this plot for any weeks missing as 
#a result of the dates in the outlook table not being correct.
plot(d1)

#use the nap shot feature in the full expanded viewer in rstudio to get a .png file

#this is the end of the code that much be used.

######################
#############
######
#I have just been resusing the legend already in the port but if not the code below can regnerate.
### Create a legend image 
data_legend <- actions %>%
  distinct(category, .keep_all=T) %>%
  arrange(category) %>% 
  select(group, start, end, color, category, notes) %>% 
  rename("event"= "category") %>% 
  mutate(event = ifelse(event == "0", "No Disruption",
                        ifelse(event == "1", "No Sampling",
                               ifelse(event == "2", "Partial Disruption",
                                      ifelse(event == 3, "Inactive", "no")))))
data_legend

data_legend$start <- as.Date("2020-01-01")
data_legend$end <- as.Date("2020-01-02")
data_legend$Patient <- "Key"
data_legend
plot_legend <- gg_vistime(data = data_legend,
                          col.group = "Patient",
                          col.event = "event",
                          show_labels = TRUE,
                          linewidth = 20,
                          title = "Legend")
plot_legend

# Tweak the legend plot
plot_legend <- plot_legend + theme_void() +
  ggplot2::theme(
    plot.title = element_text(size=11),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank())
plot_legend


### Combine the main plot and legend into a single figure
plot_combined <- plot_grid(plot, plot_legend,
                           rel_widths = c(1, 0.15))
plot_combined


###################################################################################################################
library(readxl)
#actions <- read_excel("WY2021/WY2021_Disruptions.xlsx", 
#                      sheet = "SaMT")
#View(WY2021_Disruptions)

#View(actions)
actions$start <- as.Date(actions$start)
actions$end <- as.Date(actions$end)


library(plotly)
d1 <- vistime(actions, optimize_y = T, col.group = "group", col.color = "color", linewidth = 25)
d1 %>% layout(title = "Disruptions of Sampling",
              xaxis = list(title = "Date", fixedrange=F, type = 'date', dtick ="M1",tickformat = "%b"))

plot(d1)
