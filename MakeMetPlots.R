# Make some quick figures from Master+Met data and do some simple analysis.

# Load some required packages:
loaded <- require("maps")
if(!loaded){
  print("trying to install package maps")
  install.packages("maps")
  loaded <- require("maps")
  if(loaded){
    print("maps installed and loaded")
    library(maps)
  } 
  else {
    stop("Could not install maps You need to figure out how to install that manually before this function will work!")
  }    
}

loaded <- require("mapdata")
if(!loaded){
  print("trying to install package mapdata")
  install.packages("mapdata")
  loaded <- require("mapdata")
  if(loaded){
    print("mapdata installed and loaded")
    library(mapdata)
  } 
  else {
    stop("Could not install mapdata You need to figure out how to install that manually before this function will work!")
  }    
}


loaded <- require("leaps")
if(!loaded){
  print("trying to install package leaps")
  install.packages("leaps")
  loaded <- require("leaps")
  if(loaded){
    print("leaps installed and loaded")
    library(leaps)
  } 
  else {
    stop("Could not install leaps You need to figure out how to install that manually before this function will work!")
  }    
}

# Open a pdf socket for plot output:
#pdf("MetPlots.pdf", paper="USr")

# Load the data:
master_plus_met <- read.csv("Master_Plus_Met.csv")

# Make a histogram of distances from met station to pheno site:
lon_lat <- data.frame(master_plus_met$Longitude,master_plus_met$Latitude)
unique_lon_lat <- unique(lon_lat)

unique_indices <- as.integer(row.names(unique_lon_lat))

unique_distances <- master_plus_met$Met_Station_Distance_km[unique_indices]
hist(unique_distances)

# Make a map of distances to pheno-site
map("worldHires")
points(unique_lon_lat[unique_distances<10,],pch='.',col="red")
points(unique_lon_lat[unique_distances<25 & unique_distances >= 10,],pch='.',col="orange")
points(unique_lon_lat[unique_distances<50 & unique_distances >= 25,],pch='.',col="yellow")
points(unique_lon_lat[unique_distances<100 & unique_distances >= 50,],pch='.',col="green")
points(unique_lon_lat[unique_distances<150 & unique_distances >= 100,],pch='.',col="blue")


color_list <- c("lightslateblue","green4","maroon3","goldenrod4")
# Plot pheno-day versus a bunch of met variables:
relevant_data <- master_plus_met[c(6,7,8,26,29:63)] # BUT WE ALSO NEED YEAR AND SITE ID!!

# Pheno day vs. Latitude
par(mar=c(5.1,8.1,4.1,2.1))
plot(relevant_data$Latitude,relevant_data$pheno_day,ylab="Pheno Day",xlab="Latitude")

# Pheno day vs. Latitude (Lat > 25)
par(mar=c(5.1,8.1,4.1,2.1))
plot(relevant_data$Latitude[relevant_data$Latitude > 25],
     relevant_data$pheno_day[relevant_data$Latitude > 25],ylab="Pheno Day",xlab="Latitude")

#boxplot(relevant_data$Biome,relevant_data$pheno_day,xlab="Pheno Day",ylab="Latitude")

# Pheno day vs. precip (1-30,31-60, 61-90)
par(mar=c(5.1,8.1,4.1,2.1))
plot(relevant_data$Precip_total_1to30days,relevant_data$pheno_day, 
     ylab="Pheno Day",xlab="Precip [mm]",col=color_list[1],pch=20)
points(relevant_data$Precip_total_31to60days,relevant_data$pheno_day,col=color_list[2],pch=20)
points(relevant_data$Precip_total_61to90days,relevant_data$pheno_day,col=color_list[3],pch=20)
legend(x="bottomright",c("1-30 days","31-60 days","61-90 days"),
       col=color_list,pch=20)

# Pheno day vs. Tmin (1-30,31-60,61-90)
par(mar=c(5.1,8.1,4.1,2.1))
plot(relevant_data$Tmin_mean_1to30days,relevant_data$pheno_day, 
     ylab="Pheno Day",xlab="mean T_min [C]",col=color_list[1],pch=20)
points(relevant_data$Tmin_mean_31to60days,relevant_data$pheno_day,col=color_list[2],pch=20)
points(relevant_data$Tmin_mean_61to90days,relevant_data$pheno_day,col=color_list[3],pch=20)
legend(x="bottomright",c("1-30 days","31-60 days","61-90 days"),
       col=color_list,pch=20)


# Pheno day vs. Tmax (1-30,31-60,61-90)
par(mar=c(5.1,8.1,4.1,2.1))
plot(relevant_data$Tmax_mean_1to30days,relevant_data$pheno_day, 
     ylab="Pheno Day",xlab="mean T_max [C]",col=color_list[1],pch=20)
points(relevant_data$Tmax_mean_31to60days,relevant_data$pheno_day,col=color_list[2],pch=20)
points(relevant_data$Tmax_mean_61to90days,relevant_data$pheno_day,col=color_list[3],pch=20)
legend(x="bottomright",c("1-30 days","31-60 days","61-90 days"),
       col=color_list,pch=20)

# Pheno day vs. Tmean (1-30,31-60,61-90)
par(mar=c(5.1,8.1,4.1,2.1))
plot(relevant_data$Tmean_1to30days,relevant_data$pheno_day, 
     ylab="Pheno Day",xlab="mean T_mean [C]",col=color_list[1],pch=20)
points(relevant_data$Tmean_31to60days,relevant_data$pheno_day,col=color_list[2],pch=20)
points(relevant_data$Tmean_61to90days,relevant_data$pheno_day,col=color_list[3],pch=20)
legend(x="bottomright",c("1-30 days","31-60 days","61-90 days"),
       col=color_list,pch=20)



#### Run a few regressions:
# Get the ending year of studies..
revalue(master_plus_met$Biome, c("tropical deciduous"="tropical_deciduous"))

master_plus_met$Year_Sampled_End[is.na(master_plus_met$Year_Sampled_End)] <-
  master_plus_met$Year_Sampled_Start[is.na(master_plus_met$Year_Sampled_End)]
master_plus_met$Mean_Sample_Year <- 0.5*(master_plus_met$Year_Sampled_Start + 
                                           master_plus_met$Year_Sampled_End)
# First, get rid of some variables we won't regress against:
excluded_cols <- names(master_plus_met) %in% 
  c("X","Student","Paper","Country","Site","Longitude","Study_Type",
    "Experimental_Unit","Exp_Unit_Method","Database","Response_Type","Genus",
    "Species","Reference_NA","Magnitude","Year_Sampled_Start","Year_Sampled_End",
    "Total_Years","Data_Thief","Reported_slope_yr","Reported_slope_C","Site_ID",
    "Met_Station_ID","Met_Station_Distance_km",
    "Tmin_median_1to30days","Tmin_median_31to60days","Tmin_median_61to90days",
    "Tmax_median_1to30days","Tmax_median_31to60days","Tmax_median_61to90days",
    "Range_Years")

regression_data <- master_plus_met[!excluded_cols]
LF50_data <- subset(regression_data, regression_data$pheno_method == "LeafFall50")
LF50_data <- LF50_data[!(names(LF50_data) %in% c("pheno_method"))]
LF80_data <- subset(regression_data, regression_data$pheno_method == "LeafFall80")
LF80_data <- LF80_data[!(names(LF80_data) %in% c("pheno_method"))]
LF100_data <- subset(regression_data, regression_data$pheno_method == "LeafFall100")
LF100_data <- LF100_data[!(names(LF100_data) %in% c("pheno_method"))]
LAI0_data <- subset(regression_data, regression_data$pheno_method == "LAI_zero")
LAI0_data <- LAI0_data[!(names(LAI0_data) %in% c("pheno_method"))]

# Simplest (and dumbest) regression -- use everything:
LF50_model <- lm(pheno_day ~ ., data=LF50_data)
LF80_model <- lm(pheno_day ~ ., data=LF80_data)
LF100_model <- lm(pheno_day ~ ., data=LF100_data)
LAI0_model <- lm(pheno_day ~ ., data=LAI0_data)


## Better fitting:
# LF50:
LF50_subsets <- regsubsets(pheno_day ~ ., data=LF50_data[,-2],nbest=1,nvmax=15) 
LF50_summary <- summary(LF50_subsets)
best_mod_number <- which.min(LF50_summary$bic)
a <- as.data.frame(LF50_summary$which[best_mod_number,])
col_names <- row.names(a)
columns_to_use <- names(LF50_data) %in% c("pheno_day", col_names[a[,1]])
LF50_data_BIC <- LF50_data[columns_to_use]
LF50_model_BIC <- lm(pheno_day ~ ., LF50_data_BIC)

# LF80:
LF80_subsets <- regsubsets(pheno_day ~ ., data=LF80_data[,-2],nbest=1,nvmax=15) 
LF80_summary <- summary(LF80_subsets)
best_mod_number <- which.min(LF80_summary$bic)
a <- as.data.frame(LF80_summary$which[best_mod_number,])
col_names <- row.names(a)
columns_to_use <- names(LF80_data) %in% c("pheno_day", col_names[a[,1]])
LF80_data_BIC <- LF80_data[columns_to_use]
LF80_model_BIC <- lm(pheno_day ~ ., LF80_data_BIC)

# LF100:
LF100_subsets <- regsubsets(pheno_day ~ ., data=LF100_data[,-2],nbest=1,nvmax=15) 
LF100_summary <- summary(LF100_subsets)
best_mod_number <- which.min(LF100_summary$bic)
a <- as.data.frame(LF100_summary$which[best_mod_number,])
col_names <- row.names(a)
columns_to_use <- names(LF100_data) %in% c("pheno_day", col_names[a[,1]])
LF100_data_BIC <- LF100_data[columns_to_use]
LF100_model_BIC <- lm(pheno_day ~ ., LF100_data_BIC)

# LAI0:
LAI0_subsets <- regsubsets(pheno_day ~ ., data=LAI0_data[,-2],nbest=1,nvmax=15) 
LAI0_summary <- summary(LAI0_subsets)
best_mod_number <- which.min(LAI0_summary$bic)
a <- as.data.frame(LAI0_summary$which[best_mod_number,])
col_names <- row.names(a)
columns_to_use <- names(LAI0_data) %in% c("pheno_day", col_names[a[,1]])
LAI0_data_BIC <- LAI0_data[columns_to_use]
LAI0_model_BIC <- lm(pheno_day ~ ., LAI0_data_BIC)

# Plot pheno-day vs. precip AND pheno-day versus expected pheno-day from precip-only linear effect
# (4 plots: unregressed, 30, 60, 90)
par(mar=c(5.1,8.1,4.1,2.1))
plot(LF50_data$Precip_total_1to30days,LF50_data$pheno_day, 
     ylab="Pheno Day",xlab="Precipitation 1-30 days prior [mm]",
     col=color_list[1],pch=20)
tmp_mod <- lm(LF50_data$pheno_day ~ LF50_data$Precip_total_1to30days)
abline(tmp_mod,col=color_list[1])
points(LF80_data$Precip_total_1to30days,LF80_data$pheno_day,
       col=color_list[2],pch=20)
tmp_mod <- lm(LF80_data$pheno_day ~ LF80_data$Precip_total_1to30days)
abline(tmp_mod,col=color_list[2])
points(LF100_data$Precip_total_1to30days,LF100_data$pheno_day,
       col=color_list[3],pch=20)
tmp_mod <- lm(LF100_data$pheno_day ~ LF100_data$Precip_total_1to30days)
abline(tmp_mod,col=color_list[3])
points(LAI0_data$Precip_total_1to30days,LAI0_data$pheno_day,
       col=color_list[4],pch=20)
tmp_mod <- lm(LAI0_data$pheno_day ~ LAI0_data$Precip_total_1to30days)
abline(tmp_mod,col=color_list[4])
legend(x="bottomright",c("LF50","LF80","LF100","LAI0"),
       col=color_list,pch=20)

par(mar=c(5.1,8.1,4.1,2.1))
plot(LF50_data$Precip_total_1to30days,LF50_data$pheno_day - LF50_model_BIC$residuals, 
     ylab="Predicted Pheno Day",xlab="Precipitation 1-30 days prior [mm]",
     col=color_list[1],pch=20)
tmp_mod <- lm(LF50_data$pheno_day  - LF50_model_BIC$residuals~ LF50_data$Precip_total_1to30days)
abline(tmp_mod,col=color_list[1])
points(LF80_data$Precip_total_1to30days,LF80_data$pheno_day - LF80_model_BIC$residuals,
       col=color_list[2],pch=20)
tmp_mod <- lm(LF80_data$pheno_day - LF80_model_BIC$residuals~ LF80_data$Precip_total_1to30days)
abline(tmp_mod,col=color_list[2])
points(LF100_data$Precip_total_1to30days,LF100_data$pheno_day - LF100_model_BIC$residuals,
       col=color_list[3],pch=20)
tmp_mod <- lm(LF100_data$pheno_day - LF100_model_BIC$residuals ~ LF100_data$Precip_total_1to30days)
abline(tmp_mod,col=color_list[3])
points(LAI0_data$Precip_total_1to30days,LAI0_data$pheno_day - LAI0_model_BIC$residuals,
       col=color_list[4],pch=20)
tmp_mod <- lm(LAI0_data$pheno_day-LAI0_model_BIC$residuals ~ LAI0_data$Precip_total_1to30days)
abline(tmp_mod,col=color_list[4])
legend(x="bottomright",c("LF50","LF80","LF100","LAI0"),
       col=color_list,pch=20)


# Plot pheno-day vs. Tmin AND pheno-day versus expected pheno-day from Tmin-only linear effect
# (4 plots: unregressed, 30, 60, 90)

# Plot pheno-day vs. Tmax AND pheno-day versus expected pheno-day from Tmax-only linear effect
# (4 plots: unregressed, 30, 60, 90)

# Plot pheno-day vs. Tmean AND pheno-day versus expected pheno-day from Tmean-only linear effect
# (4 plots: unregressed, 30, 60, 90)

# Plot pheno-day vs. GDD_0 AND pheno-day versus expected pheno-day from GDD_0-only linear effect
# (4 plots: unregressed, 30, 60, 90)

# Plot pheno-day vs. GDD_10 AND pheno-day versus expected pheno-day from GDD_10-only linear effect
# (4 plots: unregressed, 30, 60, 90)

# Plot pheno-day vs. CDD_0 AND pheno-day versus expected pheno-day from CDD_0-only linear effect
# (4 plots: unregressed, 30, 60, 90)

# Plot pheno-day vs. CDD_10 AND pheno-day versus expected pheno-day from CDD_10-only linear effect
# (4 plots: unregressed, 30, 60, 90)

# Plot pheno-day vs. photoperiod AND pheno-day versus expected pheno-day from photoperiod-only linear effect
# (4 plots: unregressed, 30, 60, 90)

# Plot pheno-day versus modeled/expected pheno-day from all met variables.
# (And determine portion of variance explained by meteorology)

# Plot pheno-day versus modeled/expected pheno-day from latitude variables.
# (And determine portion of variance explained by latitude)

## Let's make some partial regression plots (aka 'added variable plots') and some partial residual plots

loaded <- require("car")
if(!loaded){
  print("trying to install package car")
  install.packages("car")
  loaded <- require("car")
  if(loaded){
    print("car installed and loaded")
    library(car)
  } 
  else {
    stop("Could not install car You need to figure out how to install that manually before this function will work!")
  }    
}
avPlots(LF50_model_BIC)
avPlots(LF80_model_BIC)
avPlots(LF100_model_BIC)
avPlots(LAI0_model_BIC)

crPlots(LF50_model_BIC)
crPlots(LF80_model_BIC)
crPlots(LF100_model_BIC)
crPlots(LAI0_model_BIC)


dev.off()
