# Setup -------------------------------------------------------------------
rm(list=ls()) # Clears workspace

#system("sudo apt install libgeos-dev libproj-dev libgdal-dev libudunits2-dev -y") # Install linux geospatial dependencies 

# Install/call libraries
#install.packages("renv")
#renv::init()

PKG <- c("googledrive","tidyverse", "rgdal","raster","sf","furrr","data.table","filesstrings","exactextractr")

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}

renv::snapshot()
rm(p,PKG)

## Data
# Mass shapefiles https://docs.digital.mass.gov/dataset/massgis-data-state-outlines
dir.create(file.path('Data'), recursive = TRUE)
folder_url<-"https://drive.google.com/open?id=17r7cP7SqSyZM4cnu7xRV8HA41p5M-IWo"
folder<-drive_get(as_id(folder_url))
files<-drive_ls(folder)
dl<-function(files){
  walk(files, ~ drive_download(as_id(.x), overwrite = TRUE))
}
setwd("./Data")
system.time(map(files$id,dl))
system.time(unzip("Data.zip", exdir = "."))
file.remove("Data.zip")
setwd("..")
rm(files, folder, folder_url, dl)

## Analysis
Zones<-st_read("./Data/Zones.gpkg") # Statistical reporting areas in MA
list<-list.files("./Data/MDAT_NARW/", full.names = TRUE)
Whales<-stack(list) # All months MDAT NARW data
Zones<-st_transform(Zones, st_crs(Whales))

# Sum, accounting for coverage fraction
df<-as.data.frame(Zones$AREA_ID)
df$M1<-exact_extract(Whales[[1]],Zones, function(values, coverage_fraction) sum(values * coverage_fraction, na.rm=TRUE)) # https://cran.r-project.org/web/packages/exactextractr/readme/README.html
#df$Jan2<-exact_extract(Whales[[1]],Zones, "sum") # Interestingly, this gives the same result. Double checked and confirmed that these do a weighted sum by extracting all values and calculating
df$M2<-exact_extract(Whales[[2]],Zones, function(values, coverage_fraction) sum(values * coverage_fraction, na.rm=TRUE))
df$M3<-exact_extract(Whales[[3]],Zones, function(values, coverage_fraction) sum(values * coverage_fraction, na.rm=TRUE))
df$M4<-exact_extract(Whales[[4]],Zones, function(values, coverage_fraction) sum(values * coverage_fraction, na.rm=TRUE))
df$M5<-exact_extract(Whales[[5]],Zones, function(values, coverage_fraction) sum(values * coverage_fraction, na.rm=TRUE))
df$M6<-exact_extract(Whales[[6]],Zones, function(values, coverage_fraction) sum(values * coverage_fraction, na.rm=TRUE))
df$M7<-exact_extract(Whales[[7]],Zones, function(values, coverage_fraction) sum(values * coverage_fraction, na.rm=TRUE))
df$M8<-exact_extract(Whales[[8]],Zones, function(values, coverage_fraction) sum(values * coverage_fraction, na.rm=TRUE))
df$M9<-exact_extract(Whales[[9]],Zones, function(values, coverage_fraction) sum(values * coverage_fraction, na.rm=TRUE))
df$M10<-exact_extract(Whales[[10]],Zones, function(values, coverage_fraction) sum(values * coverage_fraction, na.rm=TRUE))
df$M11<-exact_extract(Whales[[11]],Zones, function(values, coverage_fraction) sum(values * coverage_fraction, na.rm=TRUE))
df$M12<-exact_extract(Whales[[12]],Zones, function(values, coverage_fraction) sum(values * coverage_fraction, na.rm=TRUE))

# Accounting for closure in areas 5 - 9, and 11, M2 - M4
df$M2<-ifelse(df$`Zones$AREA_ID`==5 | df$`Zones$AREA_ID`==6 | df$`Zones$AREA_ID`==7 | df$`Zones$AREA_ID`==8 | df$`Zones$AREA_ID`==9 | df$`Zones$AREA_ID`==11,0,df$M2)
df$M3<-ifelse(df$`Zones$AREA_ID`==5 | df$`Zones$AREA_ID`==6 | df$`Zones$AREA_ID`==7 | df$`Zones$AREA_ID`==8 | df$`Zones$AREA_ID`==9 | df$`Zones$AREA_ID`==11,0,df$M3)
df$M4<-ifelse(df$`Zones$AREA_ID`==5 | df$`Zones$AREA_ID`==6 | df$`Zones$AREA_ID`==7 | df$`Zones$AREA_ID`==8 | df$`Zones$AREA_ID`==9 | df$`Zones$AREA_ID`==11,0,df$M4)
df$YR<-df$M1+df$M2+df$M3+df$M4+df$M5+df$M6+df$M7+df$M8+df$M9+df$M10+df$M11+df$M12

# Annual total count in MA waters
sum(df$YR)
