setwd("~/Capstone/")
#install.packages( "xlsx")
require(xlsx)
#install.packages("readxl")
#library(readxl)

###read in the data
yelpRest_df <- read.xlsx("~/Capstone/Yelp_retrieve_Rest.xlsx", sheetName = "Sheet1")
yelpMore_df <- read.xlsx("yelp_morebusinessinfo.xlsx", sheetName = "Sheet1")
yelpCat_df <- read.xlsx("Yelp_retrieve_categories-2.xlsx", sheetName = "Sheet1")

yelpCat_df <-  yelpCat_df[ which(!yelpCat_df$Categories=='$'),]
yelpCat_df <-  yelpCat_df[ which(!yelpCat_df$Categories=='$$'),]
yelpCat_df <-  yelpCat_df[ which(!yelpCat_df$Categories=='$$$'),]

yelp_rest_new <- merge(x = yelpRest_df, y = yelpCat_df, by = "URL", all.x = TRUE)
yelp_rest_new$Categories.x[which(yelp_rest_new$Categories.x == "")] = yelp_rest_new$Categories.y

#install.packages("tidyr")
#install.packages("reshape2")
library(tidyr)
library(reshape2)

###split the categories
yelpRest_df <- separate(yelpRest_df, Categories, c("Category1", "Category2", "Category3", "Category4"), ",")

###split lat and long 
yelpRest_df <- separate(yelpRest_df, LatLong, c("Lat", "Long"), "%2C")

###make the business info wide
yelpMore_df <- dcast(yelpMore_df, URL ~ Label)

yelp_big <- merge(x = yelpRest_df, y = yelpMore_df, by = "URL", all.x = TRUE)
#rm(yelpMore_df, yelpRest_df)

wc_rest_df <- read.csv("TAMU-ALL_DONE.csv")

###connct to SQL Server
#install.packages("RODBC")
library(RODBC)
driver.name <- "ODBC Driver 13 for SQL Server"
db.name <- "DHDViews"
host.name <- "localhost"
port <-""
user.name <-"sa"
pwd <- "P@ssw0rd"
# Use a full connection string to connect to a SAMPLE database
con.text <- paste("DRIVER=",driver.name,
                  ";Database=",db.name,
                  ";Server=",host.name,
                  ";Port=",port,
                  ";PROTOCOL=TCPIP",
                  ";UID=", user.name,
                  ";PWD=",pwd,sep="")
con1 <- odbcDriverConnect(con.text)
#rm('driver.name', 'db.name', 'host.name', 'port', 'user.name', 'pwd')

### add [ActivityID] to underlying SQL query
###res <- sqlQuery(con1, 'select * from information_schema.tables')
wc_insp_df <- sqlQuery(con1, 'SELECT [HSISID], [activityID], [Score], [Date], [Description], [Type], [Inspector], [PermitID]   FROM [DHDViews].[dbo].[vOpenDataInspection]')
wc_violation_df <- sqlQuery(con1, 'SELECT [HSISID], [InspectDate], [category], [StateCode], [critical], [QuestionNo], [ViolationCode], [severity], [shortdesc], [InspectedBy], [comments], [pointValue], [observationType], [violationType], [CDCRiskFactor], [CDCDataItem], [permitID] FROM [DHDViews].[dbo].[vOpenDataViolation]')

#install.packages("lubridate")
library(lubridate)
library(stringr)
wc_insp_df$InspectionID <- paste(as.character(wc_insp_df$HSISID), "-", as.character(year(as.Date(wc_insp_df$Date, format="%m/%d/%Y"))), str_pad(as.character(month(as.Date(wc_insp_df$Date, format="%m/%d/%Y"))), 2, pad="0"), str_pad(as.character(day(as.Date(wc_insp_df$Date, format="%m/%d/%Y"))), 2, pad="0"), sep = "")
wc_violation_df$InspectionID <- paste(as.character(wc_violation_df$HSISID), "-", as.character(year(as.Date(wc_violation_df$InspectDate, format="%m/%d/%Y"))), str_pad(as.character(month(as.Date(wc_violation_df$InspectDate, format="%m/%d/%Y"))), 2, pad="0"), str_pad(as.character(day(as.Date(wc_violation_df$InspectDate, format="%m/%d/%Y"))), 2, pad="0"), sep = "")

#install.packages("data.table")
library(dplyr)
library(data.table)

wc_violation_df$seq <- with(wc_violation_df, ave(QuestionNo, InspectDate, HSISID, FUN = seq_along))
wc_violation_wide_df <- dcast(wc_violation_df, InspectionID ~ ViolationCode, value.var= "pointValue")
#rm(wc_violation_df)

wc_insp_df$year <- as.character(year(as.Date(wc_insp_df$Date, format="%m/%d/%Y")))

#install.packages("sm")
library(sm)

wc_insp_df$year <- as.factor(wc_insp_df$year)

sm.density.compare(wc_insp_df$Score, wc_insp_df$year, xlab="Inspection Score", xlim=c(90, 105))
colfill<-c(2:(2+length(levels(wc_insp_df$year)))) 
legend(locator(1), levels(wc_insp_df$year), fill=colfill)
title(main="Inspection Score Distribution by Year")

wc_inspection <- merge(x = wc_insp_df, y = wc_violation_wide_df, by = "InspectionID", all.x = TRUE)

#yelp_gis <- yelp_big[c(1,2,9:11,14:15,57)]
#write.csv(yelp_gis, "yelp_wifi.csv")

###fix the phones - remove all extra characters
yelp_big$Phone <- gsub("-", "", yelp_big$Phone)
yelp_big$Phone <- gsub("\\(", "", yelp_big$Phone)
yelp_big$Phone <- gsub(")", "", yelp_big$Phone)
yelp_big$Phone <- gsub(" ", "", yelp_big$Phone)

wc_rest_df$PhoneNumber <- gsub("-", "", wc_rest_df$PhoneNumber)
wc_rest_df$PhoneNumber <- gsub("\\(", "", wc_rest_df$PhoneNumber)
wc_rest_df$PhoneNumber <- gsub(")", "", wc_rest_df$PhoneNumber)
wc_rest_df$PhoneNumber <- gsub(" ", "", wc_rest_df$PhoneNumber)

###fix the NA
df <- sapply(yelp_big, as.character)
df[is.na(df)] <- "No Data"
yelp_big <- as.data.frame(df)

yelp_big[yelp_big==""] <- "No Data"
yelp_big[c("Category1")][is.na(yelp_big[c("Category1")])] <- "No Data"

###rename the phone column 
colnames(wc_rest_df)[8] <- "Phone"
colnames(yelp_big)[57] <- "WiFi"

###add sequential indexes
wc_rest_df$Index <- seq_len(nrow(wc_rest_df)) 
yelp_big$Index <- seq_len(nrow(yelp_big)) 

#yelp_big[(yelp_big$Category1=""),]
aggregate(URL~Category1, data=yelp_big, FUN=length)

###inner join on matching phone numbers
merge_phone_df <- merge(x=wc_rest_df, y=yelp_big, by="Phone")

# sum(merge_df$URL != "")
# 
# colSums(!is.na(merge_df))

wc_rest_df$mungeAddress <- with(wc_rest_df, paste0(Address1, City, ", ",  State, " ", PostalCode))
merge_phone_df$mungeAddress <- merge_phone_df$Address 

wc_rest_df$mungeAddress <- toupper(wc_rest_df$mungeAddress)
yelp_big$Address <- toupper(yelp_big$Address)
yelp_big$mungeAddress <- yelp_big$Address

merge_address_df <- merge(x=wc_rest_df, y=yelp_big, by="mungeAddress")
merge3_df <- merge(x=merge_df, y=merge2_df, by="URL")

merge_address_df$Coder <- "Address"
merge_phone_df$Coder <- "Phone"

dupsBetweenGroups <- function (df, idcol) {
  # df: the data frame
  # idcol: the column which identifies the group each row belongs to
  
  # Get the data columns to use for finding matches
  datacols <- setdiff(names(df), idcol)
  
  # Sort by idcol, then datacols. Save order so we can undo the sorting later.
  sortorder <- do.call(order, df)
  df <- df[sortorder,]
  
  # Find duplicates within each id group (first copy not marked)
  dupWithin <- duplicated(df)
  
  # With duplicates within each group filtered out, find duplicates between groups. 
  # Need to scan up and down with duplicated() because first copy is not marked.
  dupBetween = rep(NA, nrow(df))
  dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols])
  dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols], fromLast=TRUE) | dupBetween[!dupWithin]
  
  # ============= Replace NA's with previous non-NA value ==============
  # This is why we sorted earlier - it was necessary to do this part efficiently
  
  # Get indexes of non-NA's
  goodIdx <- !is.na(dupBetween)
  
  # These are the non-NA values from x only
  # Add a leading NA for later use when we index into this vector
  goodVals <- c(NA, dupBetween[goodIdx])
  
  # Fill the indices of the output vector with the indices pulled from
  # these offsets of goodVals. Add 1 to avoid indexing to zero.
  fillIdx <- cumsum(goodIdx)+1
  
  # The original vector, now with gaps filled
  dupBetween <- goodVals[fillIdx]
  
  # Undo the original sort
  dupBetween[sortorder] <- dupBetween
  
  # Return the vector of which entries are duplicated across groups
  return(dupBetween)
}

colnames(merge_address_df)
colnames(merge_phone_df)

colnames(merge_phone_df)[1] <- "Phone.x"

merge_phone_df$Phone.y <- merge_phone_df$Phone.x


df <- rbind(merge_address_df, merge_phone_df)                    # Stick them together

dupRows <- dupsBetweenGroups(df, "Coder")
df <- cbind(df, unique=!dupRows)

sum(df$unique == TRUE)
df <- unique(df)
df <- as.data.frame(df)

colSums(!is.na(merge_df))

library(fuzzyjoin)
# merge_df <- wc_rest_df %>%
#   stringdist_inner_join(yelp_big, by = c(mungeAddress = "mungeAddress"), max_dist=0.05, distance_col="Dist", ignore_case=T, method="jw")

merge_df <- wc_rest_df %>%
  stringdist_inner_join(yelp_big, by = c(mungeAddress = "mungeAddress"), max_dist=1, distance_col="Dist", ignore_case=T)

library(daff)
patch <- diff_data(merge2_df, merge_df)
render_diff(patch, title="compare x and y", pretty = TRUE)
merge_df_patched <- patch_data(merge_df,patch )
rm("dd", "patch")
