
# Load slope
slope_dat = read.csv('data_file_011314_slope.csv')

slope_data_start_year = slope_dat$Year_Sampled_Start
slope_data_end_year = slope_dat$Year_Sampled_End
slope_per_degree = as.numeric(as.matrix(slope_dat$Reported_slope_C))
slope_per_year = as.numeric(as.matrix(slope_dat$Reported_slope_yr))
slope_per_year[slope_per_year>50] = NA

pdf("SlopePlots.pdf")
# Plots slope in degrees 
plot(c(slope_data_start_year[1],slope_data_end_year[1]),rep(slope_per_degree[1],2),'l',
     xlim=range(c(slope_data_start_year,slope_data_end_year),na.rm=TRUE), 
     ylim=range(slope_per_degree,na.rm=TRUE),
     ylab="Change in pheno-day/degC",xlab="Year")
for (DATA in 2:length(slope_data_start_year)){
  
  mean_year = (slope_data_start_year[DATA]+slope_data_end_year[DATA])/2
  
  lines(c(slope_data_start_year[DATA],slope_data_end_year[DATA]),rep(slope_per_degree[DATA],2))
  points(mean_year,slope_per_degree[DATA])
}

# Plots slope in year
plot(c(slope_data_start_year[1],slope_data_end_year[1]),rep(slope_per_year[1],2),'l',
     xlim=range(c(slope_data_start_year,slope_data_end_year),na.rm=TRUE), 
     ylim=range(slope_per_year,na.rm=TRUE),
     ylab="Change in pheno-day/year",xlab="Year")

for (DATA in 2:length(slope_data_start_year)){
  
  mean_year = (slope_data_start_year[DATA]+slope_data_end_year[DATA])/2
  
  lines(c(slope_data_start_year[DATA],slope_data_end_year[DATA]),rep(slope_per_year[DATA],2))
  points(mean_year,slope_per_year[DATA])
}
dev.off()