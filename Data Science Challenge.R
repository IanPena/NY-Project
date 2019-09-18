###################################################################
####################### Section 1 - Prework #######################
###################################################################

# install.packages("rvest") # <- used for Web scraping
# install.packages("data.table") # <- used for data reading
# install.packages("tidyverse") # <- used for data manipulation and graphs
# install.packages("plotly") # <- used to add interactivity to graphs
# install.packages("DT") # <- used to create interactive tables
# install.packages("randomForest") # used to create a Random Forest Model
# install.packages("ggmap") # <- used to retrieve maps and use them in plots
# install.packages("tmaptools") # <- use to avoid some Google APi requirements.
# install.packages("tigris") # <- use to avoid some Google APi requirements.

# Load the aforementioned packages
library(xml2) # <- needed to load rvest
library(rvest) 
library(data.table)
library(tidyverse)
library(scales)
library(lubridate)
library(plotly)
library(DT)
library(randomForest)
library(ggmap)
library(tmaptools)
library(tigris)



###################################################################
##################### Section 2 - Webscraping #####################
###################################################################

# Save the URL provided on the challenge as a variable
Original_URL<-"http://www.nyc.gov/html/tlc/html/about/trip_record_data.shtml"

# Store the HTML of the orignal URL
O_URL_HTML<-read_html(Original_URL)

# Get the correct link from the HTML 
New_URL<-html_nodes(O_URL_HTML,"a") %>% 
          html_attr("href")

# Store the HTML of the new URL
N_URL_HTML <- read_html(New_URL)

# Create List of all URLs for the CSV files
mynode<-N_URL_HTML %>% 
          html_nodes(".faq-answers") %>% 
          html_nodes("li") %>% 
          html_nodes("a") %>%
          html_attr("href")

# Create variables for, month, year, and type of taxi
Month <-"09"
Year<- "2015"
Taxi<-"green_tripdata"

# Proceed to create filename
filename <- paste(paste(Taxi,Year,sep = "_"),Month,sep="-")

# Create variable that contains the specific URL
SourceURL <- mynode[grepl(filename,mynode)]



###################################################################
###################### Section 3 - Question 1 #####################
###################################################################

############################ Load Data ############################

# Retrieve data from SourceURL and store it.
RawData <- fread(SourceURL)

# Table of first registries of RawData
datatable(head(RawData))

##################### Dimensions of the Dataset #######################

# Creating table that contains data of Rows & Columns count of the RawData
Dimensions<-data.frame("Total_Registries"=format(nrow(RawData),decimal.mark=" ", big.mark=",", small.mark="."),
                       "Total_Fields"=ncol(RawData)
                      )
# Table of Roaw & Columns count
datatable(Dimensions)



###################################################################
###################### Section 4 - Question 2 #####################
###################################################################

################### Histogram of Trip Distance ####################

# Create histogram using Trip Distance variable
Hist_TD<-ggplot(RawData,
                aes(x=Trip_distance)
                ) +
          geom_histogram(binwidth = 15,
                         color="blue",
                         fill=("lightblue")
                        )

# Plot interactive histogram
ggplotly(Hist_TD)

# Trips <= 45 miles
Data_Less45 <- RawData %>% 
                 filter(Trip_distance<=45)

# Trips < 45 miles
Data_More45 <- RawData %>% 
                filter(Trip_distance>45)

# Hist for <=45
Hist_L45<-ggplot(Data_Less45,
                 aes(x=Trip_distance)
                ) + 
            geom_histogram(binwidth=1,
                           color="blue",
                           fill=("lightblue")
                )

# Plot interactive histogram
ggplotly(Hist_L45)

# % of representation of the data below 45 miles of trip distance

# Number of registries in original dataset
n <- nrow(RawData)

# Number of registries in new subset <=45
m1 <- nrow(Data_Less45)

# Percentage of data contained in the subset <=45
percent(m1/n,accuracy =.001)

# Hist for >45
Hist_M45<-ggplot(Data_More45,
                 aes(x=Trip_distance)
) + 
  geom_histogram(binwidth=1,
                 color="blue",
                 fill=("lightblue")
  )

# Plot interactive histogram
ggplotly(Hist_M45)

# % of representation of the data below 45 miles of trip distance

# Number of registries in new subset >45
m2 <- nrow(Data_More45)

# Percentage of data contained in the subset >45
percent(m2/n,accuracy =.001)



###################################################################
###################### Section 5 - Question 3 #####################
###################################################################

################# Mean & Median for trip distance #################

#summary of the data, adding a variable for Hour of the Day, grouping the data by it, and the summarizing data by Mean Trip and Median Trip
Summ_Hour_Trip <- RawData %>% 
                    mutate(HOD=hour(Lpep_dropoff_datetime)) %>% 
                      group_by(HOD) %>% 
                        summarise("Mean Trip"=paste0(round(mean(Trip_distance),2)," miles"),
                                  "Median Trip"=paste0(round(median(Trip_distance),2)," miles")
                                 )
# Interactive data table of the summary
datatable(Summ_Hour_Trip)

########################## Airport Trips ##########################

# Create variables for the latitude and longitude for each airport, also a margin to creat a range to look into.

#JFK
JFK_Lat <- 40.6413111
JFK_Lon <- -73.7781391

#LGA
LGA_Lat <- 40.7769271
LGA_Lon <- -73.8739659

#EWR
EWR_Lat <- 40.6895314
EWR_Lon <- -74.1744624

#Margin
Margin <- 0.01

# Generate an specific variable for each airport, as well if it's a drop or pick up. Also add variables for Trevale duration and Speed.

ModData<-RawData %>% 
          mutate(Pick_JFK_flag=ifelse(between(Pickup_latitude,
                                              JFK_Lat-Margin,
                                              JFK_Lat+Margin
                                      )&
                                        between(Pickup_longitude,
                                                JFK_Lon-Margin,
                                                JFK_Lon+Margin)
                                      ,1,0
                                      ),
                  Drop_JFK_flag=ifelse(between(Dropoff_latitude,
                                               JFK_Lat-Margin,
                                               JFK_Lat+Margin
                                      )&
                                        between(Dropoff_longitude,
                                                JFK_Lon-Margin,
                                                JFK_Lon+Margin)
                                      ,1,0
                                      ),
                  Pick_LGA_flag=ifelse(between(Pickup_latitude,
                                               LGA_Lat-Margin,
                                               LGA_Lat+Margin
                                      )&
                                        between(Pickup_longitude,
                                                LGA_Lon-Margin,
                                                LGA_Lon+Margin)
                                      ,1,0
                                      ),
                  Drop_LGA_flag=ifelse(between(Dropoff_latitude,
                                               LGA_Lat-Margin,
                                               LGA_Lat+Margin
                                      )&
                                        between(Dropoff_longitude,
                                                LGA_Lon-Margin,
                                                LGA_Lon+Margin)
                                      ,1,0
                                      ),
                  Pick_EWR_flag=ifelse(between(Pickup_latitude,
                                               EWR_Lat-Margin,
                                               EWR_Lat+Margin
                                      )&
                                        between(Pickup_longitude,
                                                EWR_Lon-Margin,
                                                EWR_Lon+Margin
                                        )
                                      ,1,0
                                      ),
                  Drop_EWR_flag=ifelse(between(Dropoff_latitude,
                                               EWR_Lat-Margin,
                                               EWR_Lat+Margin
                                      )&
                                        between(Dropoff_longitude,
                                                EWR_Lon-Margin,
                                                EWR_Lon+Margin)
                                      ,1,0
                                      ),
                  "Travel_Time"=round(as.numeric(difftime(as_datetime(Lpep_dropoff_datetime),
                                                          as_datetime(lpep_pickup_datetime),
                                                          units = "hours"))*60,2)
                  ) %>% 
              mutate("Speed"=round(ifelse(Travel_Time==0,0,Trip_distance/(Travel_Time/60)),2))



# Generate an specific variable that takes into account any Airport Flag
ModData <- ModData %>% 
            mutate(Airport_Flag=ifelse(Pick_JFK_flag == 1 |
                                         Drop_JFK_flag == 1 |
                                         Pick_LGA_flag == 1 |
                                         Drop_LGA_flag == 1 |
                                         Pick_EWR_flag == 1 |
                                         Drop_EWR_flag == 1,
                                       1,
                                       0
                                      )
                  )

# We now modify the Flag in order to avoid counting trips that are inside the same airport area. This can help prevent the errors associated with the Margin.
ModData <- ModData %>% 
  mutate(Airport_Flag=ifelse(Pick_JFK_flag == 1 & Drop_JFK_flag == 1,
                             0,
                             ifelse(Pick_LGA_flag == 1 & Drop_LGA_flag == 1,
                                    0,
                                    ifelse(Pick_EWR_flag == 1 & Drop_EWR_flag == 1,
                                           0,
                                           Airport_Flag
                                          )
                                   )
                            )
         )

# Generate subset for only Airport related data
AirportData <- ModData %>% 
                filter(Airport_Flag == 1)

# Generate summary of data facts
AirportSumm<-AirportData %>% 
              summarise("# of Trips" = format(n(),decimal.mark=" ", big.mark=",", small.mark="."),
                        "Avg Fare" = dollar(mean(Fare_amount)),
                        "Min Fare" = dollar(min(Fare_amount)),
                        "Max Fare" = dollar(max(Fare_amount)),
                        "Avg Tip" = dollar(mean(Tip_amount)),
                        "Min Tip" = dollar(min(Tip_amount)),
                        "Max Tip" = dollar(max(Tip_amount)),
                        "Avg Distance" = paste0(round(mean(Trip_distance),2), " miles"),
                        "Min Distance" = paste0(round(min(Trip_distance),2), " miles"),
                        "Max Distance" = paste0(round(max(Trip_distance),2), " miles"),
                        "JFK Trips" = format(sum(Pick_JFK_flag+Drop_JFK_flag),decimal.mark=" ", big.mark=",", small.mark="."),
                        "LGA Trips" = format(sum(Pick_LGA_flag+Drop_LGA_flag),decimal.mark=" ", big.mark=",", small.mark="."),
                        "EWR Trips" = format(sum(Pick_EWR_flag+Drop_EWR_flag),decimal.mark=" ", big.mark=",", small.mark="."),
                        "Avg TT" = paste0(round(mean(Travel_Time),2)," mins"),
                        "Min TT" = paste0(min(Travel_Time)," mins"),
                        "Max TT" = paste0(max(Travel_Time)," mins"),
                        "Avg Speed" = paste0(round(mean(Speed),2)," mph"),
                        "Min Speed" = paste0(min(Speed)," mph"),
                        "Max Speed" = paste0(max(Speed)," mph"),
                      )

# Interactive data table
datatable(AirportSumm)

# Add WeekDay and Hour of the Day
AirportData_DT<-AirportData %>% 
                  mutate(WeekDay=weekdays(as.Date(lpep_pickup_datetime)),
                         HoD=hour(Lpep_dropoff_datetime))

# Replicate summary for new dataset
Airport_DT_Summ<-AirportData_DT %>% 
                  group_by(WeekDay,HoD) %>% 
                    summarise("# of Trips" = format(n(),decimal.mark=" ", big.mark=",", small.mark="."),
                              "Avg Fare" = dollar(mean(Fare_amount)),
                              "Min Fare" = dollar(min(Fare_amount)),
                              "Max Fare" = dollar(max(Fare_amount)),
                              "Avg Tip" = dollar(mean(Tip_amount)),
                              "Min Tip" = dollar(min(Tip_amount)),
                              "Max Tip" = dollar(max(Tip_amount)),
                              "Avg Distance" = paste0(round(mean(Trip_distance),2), " miles"),
                              "Min Distance" = paste0(round(min(Trip_distance),2), " miles"),
                              "Max Distance" = paste0(round(max(Trip_distance),2), " miles"),
                              "JFK Trips" = format(sum(Pick_JFK_flag+Drop_JFK_flag),decimal.mark=" ", big.mark=",", small.mark="."),
                              "LGA Trips" = format(sum(Pick_LGA_flag+Drop_LGA_flag),decimal.mark=" ", big.mark=",", small.mark="."),
                              "EWR Trips" = format(sum(Pick_EWR_flag+Drop_EWR_flag),decimal.mark=" ", big.mark=",", small.mark="."),
                              "Avg TT" = paste0(round(mean(Travel_Time),2)," mins"),
                              "Min TT" = paste0(min(Travel_Time)," mins"),
                              "Max TT" = paste0(max(Travel_Time)," mins"),
                              "Avg Speed" = paste0(round(mean(Speed),2)," mph"),
                              "Min Speed" = paste0(min(Speed)," mph"),
                              "Max Speed" = paste0(max(Speed)," mph"),
                              )

# Format
datatable(Airport_DT_Summ)

###################################################################
###################### Section 6 - Question 4 #####################
###################################################################

###################### variable for Tip/Fare ######################

# Variable for Trip/Fare.
ModData <- ModData %>% 
            mutate("TipFare"=ifelse(Fare_amount==0,
                                    0,
                                    Tip_amount/Fare_amount)
                  )

###################### Predictive Model ######################

# Summary of variables in Dataset.
str(ModData)

RF_Data <- ModData %>% 
            mutate(WeekDay=weekdays(as.Date(lpep_pickup_datetime)),
                   HoD=hour(Lpep_dropoff_datetime),
                   HoP=hour(lpep_pickup_datetime)) %>%
              mutate(week=ifelse(WeekDay== "lunes"|
                                 WeekDay== "martes"|
                                 WeekDay== "miércoles"|
                                 WeekDay== "jueves"|
                                 WeekDay== "viernes",1,0
                                  ),
                     weekend=ifelse(WeekDay== "sábado"|
                                    WeekDay== "domingo",1,0
                                   ),
                     Day_P=ifelse(HoP >= 6 & HoP<=17,1,0),
                     Night_P=ifelse(HoP >= 0 & HoP <= 5|
                                  HoP >= 18 & HoP <= 23,1,0),
                     Day_D=ifelse(HoP >= 6 & HoP<=17,1,0),
                     Night_D=ifelse(HoD >= 0 & HoD <= 5|
                                    HoD >= 18 & HoD <= 23,1,0),
                    ) %>% 
              select(-c(lpep_pickup_datetime,
                        Lpep_dropoff_datetime,
                        Ehail_fee,
                        Store_and_fwd_flag,
                        WeekDay,
                        HoD,
                        HoP)
                     )

str(RF_Data)
              
#Remove missing values
RF_Data <- RF_Data %>% 
            mutate(Trip_type=ifelse(is.na(Trip_type),
                                    mean(Trip_type,na.rm = TRUE),
                                    Trip_type
                                    )
                  )
                
# Variable to get size of subsets
n <- round(nrow(RF_Data)/100,0)

# Generate vector with randomly selected number of registries
set.seed(1234)
SubsetSize <- sample(nrow(RF_Data),n,replace = FALSE)

# Generate Subset
RF_Data_Subset <- RF_Data[SubsetSize,]

# Plot hist of new subset of trip distance, to check that data looks similar.
Hist_RF<-ggplot(RF_Data_Subset,
                aes(x=Trip_distance)
) +
  geom_histogram(binwidth = 1,
                 color="blue",
                 fill=("lightblue")
  )

ggplotly(Hist_RF)

# Random selection of row in a 70:30 ratio
Base_RF <- sample(nrow(RF_Data_Subset),round(0.3*nrow(RF_Data_Subset),0),replace=FALSE)

# Using the previous rows to create a new subset for Train with 70% of the data
Train_RF <- RF_Data_Subset[Base_RF,]

Train_RF_N <- Train_RF %>% 
                select(-c(TipFare))

# Using the previous rows to create a new subset for validate with 30% of data
Validate_RF <- RF_Data_Subset[-Base_RF,]
validate_RF_N <- Validate_RF %>% 
                  select(-c(TipFare))

# Generate Model

RF_Model1 <- randomForest(x = Train_RF_N,
                         y=Train_RF$TipFare)

RF_Model1




# Generate model
RF_Model2 <- randomForest(x = Train_RF_N,
                          y=Train_RF$TipFare,
                          mtry=15,
                          importance=TRUE)
RF_Model2

# Generate predicion
P_Validate <- predict(RF_Model1,Validate_RF_N,type = "class")

# Predict on validate set
P_Validate <- predict(RF_Model1,validate_RF_N,type = "class")

#  Statistics of estimated vs observed
Statistics <- data.frame(mean(P_Validate),
                         mean(Validate_RF$TipFare),
                         sd(P_Validate),
                         sd(Validate_RF$TipFare)
)

# Create more suiting names for the statistics
names(Statistics) <- c("Est_mean","Obs_mean","Est_sd","Obs_sd")

# Interactive data table
datatable(Statistics)

Prepping the results to create graph
comp_data <- data.frame(Validate_RF$TipFare,P_Validate)
names(comp_data) <- c("Observed","Predicted")

# Creating plot
p<-ggplot() +
  geom_point(aes(x=Validate_RF$Fare_amount),y=comp_data$Observed,color="blue") + 
  geom_point(aes(x=Validate_RF$Fare_amount),y=comp_data$Predicted,color="lightblue") +
  ggtitle("Observed (blue) & Estimated (light blue) % of Tip over Fare by Fare Amount") +
  xlab("Fare Amount") + 
  ylab(" % of Tip Over Fare")

# Interactive Plot
ggplotly(p)



###################################################################
###################### Section 7 - Question 5 #####################
###################################################################

# Create a subset of data for an easier work due computational limitations, as well clean the datadaset of illogic locations for pick up or drop off.
MapData <- ModData %>% 
  filter(Pickup_latitude>40 &
           Pickup_longitude < -70 &
           Dropoff_latitude >40 & 
           Dropoff_longitude < -70)

# Random selections of rows
RDM_Map <- sample(nrow(MapData),nrow(ModData)/50,replace = FALSE)

# Subsetting based on random rows
MapData <- MapData[RDM_Map,]

# Retrieve NYC Map
NYC_Map <-ggmap(get_stamenmap(rbind(as.numeric(paste(geocode_OSM("New York City")$bbox))), zoom = 11))

# Add Pick up locations into NYC Map
r<-NYC_Map+
    geom_point(data=MapData,aes(x=Pickup_longitude,y=Pickup_latitude), color="blue",shape=1,size=.1,alpha=0.5) + 
     geom_point(data=MapData,aes(x=Dropoff_longitude,y=Dropoff_latitude), color="green",shape=1,size=.1,alpha=0.5)

# Interactive plot
ggplotly(r)


# Defining ranges for lat - lon of each boroug

MapData_B <- MapData %>% 
  mutate(MH_pick=ifelse(Pickup_longitude >= -74.01097 &
                          Pickup_longitude <= -73.94281 &
                          Pickup_latitude >= 40.70592 &
                          Pickup_latitude <= 40.80052,1,0),
         MH_drop=ifelse(Dropoff_longitude >= -74.01097 &
                          Dropoff_longitude <= -73.94281 &
                          Dropoff_latitude >= 40.70592 &
                          Dropoff_latitude <= 40.80052,1,0),
         Bronx_pick=ifelse(Pickup_longitude >= -74.00001 &
                             Pickup_longitude <= -73.80001 &
                             Pickup_latitude > 40.80052,1,0),
         Bronx_drop=ifelse(Dropoff_longitude >= -74.00001 &
                             Dropoff_longitude <= -73.80001 &
                             Dropoff_latitude > 40.80052,1,0),
         Queens_pick=ifelse(Pickup_longitude >= -74.03500 &
                              Pickup_longitude <= -73.88644 &
                              Pickup_latitude >= 40.57594 &
                              Pickup_latitude <= 40.73012,1,0),
         Queens_drop=ifelse(Dropoff_longitude >= -74.03500 &
                              Dropoff_longitude <= -73.88644 &
                              Dropoff_latitude >= 40.57594 &
                              Dropoff_latitude <= 40.73012,1,0),
         Brook_pick=ifelse(Pickup_longitude >= -74.04000 &
                             Pickup_longitude <= -73.83424 &
                             Pickup_latitude >= 40.7000 &
                             Pickup_latitude <= 40.77312,1,0),
         Brook_drop=ifelse(Dropoff_longitude >= -73.94000 &
                             Dropoff_longitude <= -73.83424 &
                             Dropoff_latitude >= 40.70000 &
                             Dropoff_latitude <= 40.77312,1,0)
         
  ) %>% 
  mutate(Bronx_drop=ifelse(Bronx_drop == 1 & MH_drop == 1,0,Bronx_drop),
         Queens_drop=ifelse(Queens_drop == 1 & MH_drop == 1,0,Queens_drop),
         Brook_drop=ifelse(Brook_drop == 1 & MH_drop == 1,0,Queens_drop)
  )

# Intra borough travels

# Bronx
MapData_intra_Bronx <- MapData_B %>% 
  filter(Bronx_pick==1 & Bronx_drop==1)

# Queens
MapData_intra_Queens <- MapData_B %>% 
  filter(Queens_pick==1 & Queens_drop==1)
# Brooklyn
MapData_intra_Brook <- MapData_B %>% 
  filter(Brook_pick==1 & Brook_drop==1)

# inter borough travels

# Bronx
MapData_inter_Bronx <- MapData_B %>% 
  filter(Bronx_pick==1 & Bronx_drop!=1)
# Queens
MapData_inter_Queens <- MapData_B %>% 
  filter(Queens_pick==1 & Queens_drop!=1)

# Brooklyn
MapData_inter_Brook <- MapData_B %>% 
  filter(Brook_pick==1 & Brook_drop!=1)
# Intra borough travels

# Bronx
MapData_intra_Bronx <- MapData_B %>% 
  filter(Bronx_pick==1 & Bronx_drop==1)

# Queens
MapData_intra_Queens <- MapData_B %>% 
  filter(Queens_pick==1 & Queens_drop==1)
# Brooklyn
MapData_intra_Brook <- MapData_B %>% 
  filter(Brook_pick==1 & Brook_drop==1)

# inter borough travels

# Bronx
MapData_inter_Bronx <- MapData_B %>% 
  filter(Bronx_pick==1 & Bronx_drop!=1)
# Queens
MapData_inter_Queens <- MapData_B %>% 
  filter(Queens_pick==1 & Queens_drop!=1)

# Brooklyn
MapData_inter_Brook <- MapData_B %>% 
  filter(Brook_pick==1 & Brook_drop!=1)

# Bronx Travels

Map_Bronx_Travels <-NYC_Map +
  geom_point(data=MapData_intra_Bronx,
             aes(x=Pickup_longitude,y=Pickup_latitude),
             color="blue",
             shape=1,
             size=.1,
             alpha=0.5) + 
  geom_point(data=MapData_intra_Bronx,
             aes(x=Dropoff_longitude,y=Dropoff_latitude),
             color="lightblue",
             shape=1,
             size=.1,
             alpha=0.5) +
  geom_point(data=MapData_inter_Bronx,
             aes(x=Pickup_longitude,y=Pickup_latitude),
             color="green",
             shape=1,
             size=.1,
             alpha=0.5) + 
  geom_point(data=MapData_inter_Bronx,
             aes(x=Dropoff_longitude,y=Dropoff_latitude),
             color="lightgreen",
             shape=1,
             size=.1,
             alpha=0.5) +
  labs(title = "Bronx Travels - Intra travels (blue) vs Inter travels (Green)",
       subtitle = "Pick ups (solid color) & Dropoffs (light color)")

# Interactive plot
ggplotly(Map_Bronx_Travels)

# Queens Travels

Map_Queens_Travels <-NYC_Map +
  geom_point(data=MapData_intra_Queens,
             aes(x=Pickup_longitude,y=Pickup_latitude),
             color="blue",
             shape=1,
             size=.1,
             alpha=0.5) + 
  geom_point(data=MapData_intra_Queens,
             aes(x=Dropoff_longitude,y=Dropoff_latitude),
             color="lightblue",
             shape=1,
             size=.1,
             alpha=0.5) +
  geom_point(data=MapData_inter_Queens,
             aes(x=Pickup_longitude,y=Pickup_latitude),
             color="green",
             shape=1,
             size=.1,
             alpha=0.5) + 
  geom_point(data=MapData_inter_Queens,
             aes(x=Dropoff_longitude,y=Dropoff_latitude),
             color="lightgreen",
             shape=1,
             size=.1,
             alpha=0.5) +
  labs(title = "Queens Travels - Intra travels (blue) vs Inter travels (Green)", subtitle = "Pick ups (solid color) & Dropoffs (light color)")

# Interactive plot
ggplotly(Map_Queens_Travels)

# Brooklyn Travels

Map_Brook_Travels <-NYC_Map +
  geom_point(data=MapData_intra_Brook,
             aes(x=Pickup_longitude,y=Pickup_latitude),
             color="blue",
             shape=1,
             size=.1,
             alpha=0.5) + 
  geom_point(data=MapData_intra_Brook,
             aes(x=Dropoff_longitude,y=Dropoff_latitude),
             color="lightblue",
             shape=1,
             size=.1,
             alpha=0.5) +
  geom_point(data=MapData_inter_Brook,
             aes(x=Pickup_longitude,y=Pickup_latitude),
             color="green",
             shape=1,
             size=.1,
             alpha=0.5) + 
  geom_point(data=MapData_inter_Brook,
             aes(x=Dropoff_longitude,y=Dropoff_latitude),
             color="lightgreen",
             shape=1,
             size=.1,
             alpha=0.5) +
  labs(title = "Brooklyn Travels - Intra travels (blue) vs Inter travels (Green)",
       subtitle = "Pick ups (solid color) & Dropoffs (light color)")

# Interactive plot
ggplotly(Map_Brook_Travels)