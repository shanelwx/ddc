#### Install and load packages ####
packages <- c("readxl","lubridate","tidyverse","ggplot2", "plotly", "tictoc", "janitor", "gridExtra", "grid")
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

#### Clear environment & Timer ####
rm(list = ls()) ## Clear global environment
tic ()

#### Functions ####
# Radians to degree function
rad <- function(r){
  r * pi / 180
}
#### Source and wd (Import data - Acquire & Geo input csvs) ####
wd_db <- "T:/DDC/R/csv/database"
wd_export <- "T:/DDC/R/csv/exports/DDC" # export location
wd_lfm <- "M:/Final/ACQUIRE/Exports/OKD/LFM"
wd_input <- "T:/DDC/R/csv/DDC" # import location


# Pull survey and collar data from M drive
setwd (wd_lfm)
OKD_surv <- read.csv("LFM_OKD_MGA2020-53_sur.csv")
OKD_col <- read.csv("LFM_OKD_MGA2020-53_col.csv")

# Change directory to read inputs
setwd (wd_input)
# Geo input csvs
Pjt_surv <- read.csv("Projection survey.csv")
plan_csv <- read.csv("Planned Survey.csv")
targ_csv <- read.csv("Targets.csv") # Change to all targets that team uses for LF?

# Initialize and clean dataframes
OKD_surv <- clean_names(OKD_surv)
OKD_col <- clean_names(OKD_col)
Pjt_surv <- clean_names(Pjt_surv)
plan_csv <- clean_names(plan_csv)
colnames(plan_csv)[1] <- "bhid"
targ_csv <- clean_names(targ_csv)
colnames(targ_csv)[5] <- "bhid"
Pjt_col <- OKD_col

#### QAQC check source ####
temp_check1 <- Pjt_surv %>% # Pjt surv
  arrange(desc(md)) %>%
  distinct(bhid, .keep_all = TRUE) %>%
  select(bhid, md)
temp_check2 <- targ_csv %>% # Target points
  select(bhid, pad_id, target_id)
temp_check3 <- plan_csv %>% # Plan surv
  arrange(desc(md)) %>%
  distinct(bhid, .keep_all = TRUE) %>%
  select(bhid, md)

check1 <- left_join(temp_check1, temp_check3, by = "bhid") %>%
  rename(proj_maxmd = md.x) %>%
  rename(plan_maxmd = md.y)
check2 <- check1 %>%
  filter(is.na(plan_maxmd)) %>%
  select(bhid)
if(nrow(check2) != 0){
  print("Missing in Planned Survey.csv") # Bhids in pjt but not in planned csv
  stop(paste0("bhid - ",check2$bhid," "))
}
check3 <- left_join(temp_check1, temp_check2, by = "bhid") %>%
  filter(is.na(target_id)) %>%
  select(bhid)
if(nrow(check3) != 0){
  print("No targets for") # Bhids in pjt but not in target csv
  stop(paste0("bhid - ",check3$bhid," "))
}
check4 <- left_join(check1, temp_check2, by = "bhid") 
print("QAQC on source csvs completed, processing these holes...") # QAQC message checkpoint
print(check4)

#### 1. Produce projection holes - Joining contractors projection to actual surveys in db ####
## Filter 
# Obtain latest md from OKD_surv
Pjt_merge_depth <- OKD_surv %>%
  arrange(desc(depth)) %>%
  distinct(bhid, .keep_all = TRUE) %>%
  select(bhid, depth)
# Merge OKD_surv with Pjtn surv to remove oudated Pjt'ed surv
surv_join1 <- left_join(Pjt_surv, Pjt_merge_depth, by = "bhid")
surv_join2 <- surv_join1 %>% # If Pjtn surv md(i) < OKD suv (i + nth), omit data row
  select(bhid, md, dip, gn_azi, depth) %>%
  mutate(filtering = md - depth) %>%
  filter(filtering > 0) %>%
  select(bhid, md, dip, gn_azi)
surv_append <- OKD_surv %>%
  select(bhid, depth, azimuth, dip)
# Rename
colnames(surv_append)[2] = "md"
colnames(surv_append)[3] = "gn_azi"

# Intialize Pjt_col EOH depth to max md from Pjt_surv
col_join1 <- surv_join2 %>% # Max md of projected survey
  arrange(desc(md)) %>%
  distinct(bhid, .keep_all = TRUE) %>%
  select(bhid, md)
Pjt_col_final <- left_join(Pjt_col, col_join1, by = "bhid") %>%
  mutate(eoh = coalesce(md,eoh)) %>% # Replace eoh with md, if md na, keep eoh
  select(-md)

# Final projection survey and collar, bind OKD_surv and Pjt_surv
Pjt_surv_final <- bind_rows(surv_join2, surv_append) %>%
  group_by(bhid) %>% # Grouping variable for .by_group
  arrange(md, .by_group = TRUE) #sort with bhid grouping

# Filter Pjt_surv_final for only current drilling - Pjt surv distinct
current_drillholes <- distinct(Pjt_surv, bhid) %>%
  mutate(temp = 1)
Pjt_col_final <- left_join(Pjt_col_final, current_drillholes, by = "bhid") %>%
  filter(temp == 1) %>%
  select(-temp)
Pjt_surv_final <- left_join(Pjt_surv_final, current_drillholes, by = "bhid") %>%
  filter(temp == 1) %>%
  select(-temp)

# Export out Pjt col and surv csvs
setwd (wd_export)
write.csv(Pjt_col_final, "Projection collar.csv", row.names = FALSE)
write.csv(Pjt_surv_final, "Projection survey.csv", row.names = FALSE)
print(paste0("Projection collar and survey csvs exported to ", wd_export))

#### 2. Desurveying and Interpolation of projection holes ####
# Subset
temp1 <- Pjt_col_final %>%
  distinct(bhid)

desurv_lerp <- data.frame() # Int LERP df

print("2. Desurveying and interpolation of projection traces for...")
print(temp1) # List of traces to process

for (k in 1:nrow(temp1)){
  temp_col1 <- Pjt_col_final %>% 
    filter(bhid == temp1[k,]) %>%
    select(bhid, east, north, rl)
  temp_surv1 <- Pjt_surv_final %>%
    filter(bhid == temp1[k,])
  
  temp_colsurv <- left_join(temp_surv1, temp_col1, by = "bhid")
  temp_colsurv1 <- temp_colsurv %>%
    mutate(inclination = dip + 90) %>%
    mutate(length = md - lag(md, default = md[1])) # Length [i + 1] - [i]
  
  temp_colsurv2 <- temp_colsurv1 %>%
    mutate(northing = case_when(md == 0 ~ north, TRUE ~ 0))
  
  desurv <- temp_colsurv2 %>%
    mutate(northing = (length/2) * (sin(rad(lag(inclination, default = inclination[1]))) # North
                                    * cos(rad(lag(gn_azi, default = gn_azi[1]))) # Lag to take previous row
                                    + sin(rad(inclination))
                                    * cos(rad(gn_azi)))) %>%
    mutate(northing = cumsum(northing) + north) %>% # Decimals still stored
    mutate(easting = (length/2) * (sin(rad(lag(inclination, default = inclination[1]))) # East
                                   * sin(rad(lag(gn_azi, default = gn_azi[1]))) # Lag to take previous row
                                   + sin(rad(inclination))
                                   * sin(rad(gn_azi)))) %>%
    mutate(easting = cumsum(easting) + east) %>% # Decimals still stored
    mutate(rls = (length/2) * (cos(rad(lag(inclination, default = inclination[1])))
                               + cos(rad(inclination)))) %>%
    mutate(rls = rl - cumsum(rls)) # RL
  
  
  desurv_inverse_filter <- desurv %>% # data filtered out in next few lines kept here
    filter(length >= 1) %>% 
    filter(length < 2) %>% # Keeping desurveyed data that have length = 1 between md points to rbind into final at end
    select(bhid, md, easting, northing, rls)
  
  desurv <- desurv %>%
    filter(case_when(md == 0 ~ length == 0, # When md == 0, filter (take) length == 0
                     TRUE ~ length >= 2 )) # Everything else, filter length >= 2
  # Interpolation
  for (i in 1:nrow(desurv)){
    
    lerp_seq <- as.numeric((desurv[i + 1,9])) # md length
    
    if (is.na(lerp_seq)){ # When i + 1 in length is = 1 skip iteration. to value > from error in seq
      next
    } else if (lerp_seq == 1){ # When i + 1 goes beyond dataframe, skip iteration. NA error in seq
      next
    }
    
    east_dif <- desurv[i, 11] - desurv[i + 1, 11]
    north_dif <- desurv[i, 10] - desurv[i + 1, 10]
    rls_dif <- desurv[i, 12] - desurv[i + 1, 12]
    
    temp_desurv_lerp <- cbind(desurv[i, 1], desurv[i, 2], desurv[i, 11], desurv[i, 10], desurv[i, 12]) # Rbind current i iteration data
    desurv_lerp <- rbind(desurv_lerp, temp_desurv_lerp)
    
    print(paste0("2. Processing > ", temp1[k,], " @ " ,round(i/nrow(desurv), digits = 2) * 100, "%")) # Progress bar
    
    lerp_seq <- seq(1/lerp_seq, 0.999, 2/lerp_seq) # Lin interp even nodes from md length (2/lerp, change x = 2 for lerp resolution)
    
    for (j in 1:length(lerp_seq)){
      j <- lerp_seq[j]
      
      east_dif2 <- desurv[i, 11] - (east_dif * j)
      north_dif2 <- desurv[i, 10] - (north_dif * j)
      rls_dif2 <- desurv[i, 12] - (rls_dif * j)
      temp_md <- desurv[i, 2] + (desurv[i + 1, 9] * j)
      
      all_dif <- cbind(desurv[i,1], temp_md, east_dif2, north_dif2, rls_dif2) # Create single row data cbind
      all_dif <- na.omit(all_dif) # Get rid of na
      desurv_lerp <- rbind(desurv_lerp, all_dif) # Rbind everything within i to i + 1 sequence of iteration
    }
  }
  desurv_lerp <- rbind(desurv_lerp, desurv_inverse_filter) # rbind length = 1 data into interpolated data where length >=2 
}
Pjt_desurv <- desurv_lerp

#### 3. Desurveying and Interpolation of planned holes ####
plan_col <- plan_csv %>% 
  distinct(bhid) %>%
  select(bhid)
plan_col <- left_join(plan_col, OKD_col, by = "bhid") # Create planned collar from planned survey and OKD_col

# Formatting plan_csv as tibble and grouped df for functional use
plan_csv <- plan_csv %>% # Desurv and lerp below acquired from above was done with proj_surv as a tibble format
  group_by(bhid) %>% # Making sure planned section of code has same tibble format
  arrange(md, .by_group = TRUE)

#Subset
temp1 <- plan_col %>%
  distinct(bhid)

desurv_lerp <- data.frame() # Int LERP df

print("3. Desurveying and interpolation of planned traces...")
print(temp1) # List of traces to process

for (k in 1:nrow(temp1)){
  temp_col1 <- plan_col %>% 
    filter(bhid == temp1[k,]) %>%
    select(bhid, east, north, rl)
  temp_surv1 <- plan_csv %>%
    filter(bhid == temp1[k,])
  
  temp_colsurv <- left_join(temp_surv1, temp_col1, by = "bhid")
  temp_colsurv1 <- temp_colsurv %>%
    mutate(inclination = dip + 90) %>%
    mutate(length = md - lag(md, default = md[1])) # Length [i + 1] - [i]
  
  temp_colsurv2 <- temp_colsurv1 %>%
    mutate(northing = case_when(md == 0 ~ north, TRUE ~ 0))
  
  desurv <- temp_colsurv2 %>%
    mutate(northing = (length/2) * (sin(rad(lag(inclination, default = inclination[1]))) # North
                                    * cos(rad(lag(gn_azi, default = gn_azi[1]))) # Lag to take previous row
                                    + sin(rad(inclination))
                                    * cos(rad(gn_azi)))) %>%
    mutate(northing = cumsum(northing) + north) %>% # Decimals still stored
    mutate(easting = (length/2) * (sin(rad(lag(inclination, default = inclination[1]))) # East
                                   * sin(rad(lag(gn_azi, default = gn_azi[1]))) # Lag to take previous row
                                   + sin(rad(inclination))
                                   * sin(rad(gn_azi)))) %>%
    mutate(easting = cumsum(easting) + east) %>% # Decimals still stored
    mutate(rls = (length/2) * (cos(rad(lag(inclination, default = inclination[1])))
                               + cos(rad(inclination)))) %>%
    mutate(rls = rl - cumsum(rls)) # RL
  
  desurv_inverse_filter <- desurv %>% # data filtered out in next few lines kept here
    filter(length >= 1) %>% 
    filter(length < 2) %>% # Keeping desurveyed data that have length = 1 between md points to rbind into final at end
    select(bhid, md, easting, northing, rls)
  
  desurv <- desurv %>%
    filter(case_when(md == 0 ~ length == 0, # When md == 0, filter (take) length == 0
                     TRUE ~ length >= 2 )) # Everything else, filter length >= 2
  
  # Interpolation
  for (i in 1:nrow(desurv)){
    
    lerp_seq <- as.numeric((desurv[i + 1,9])) # md length
    
    if (is.na(lerp_seq)){ # When i + 1 in length is = 1 skip iteration. to value > from error in seq
      next
    } else if (lerp_seq == 1){ # When i + 1 goes beyond dataframe, skip iteration. NA error in seq
      next
    }
    
    east_dif <- desurv[i, 11] - desurv[i + 1, 11]
    north_dif <- desurv[i, 10] - desurv[i + 1, 10]
    rls_dif <- desurv[i, 12] - desurv[i + 1, 12]
    
    temp_desurv_lerp <- cbind(desurv[i, 1], desurv[i, 2], desurv[i, 11], desurv[i, 10], desurv[i, 12]) # Rbind current i iteration data
    desurv_lerp <- rbind(desurv_lerp, temp_desurv_lerp)
    
    print(paste0("3. Processing > ", temp1[k,], " @ " ,round(i/nrow(desurv), digits = 2) * 100, "%")) # Progress bar
    
    lerp_seq <- seq(1/lerp_seq, 0.999, 2/lerp_seq) # Lin interp even nodes from md length (2/lerp, change x = 2 for lerp resolution)
    
    for (j in 1:length(lerp_seq)){
      j <- lerp_seq[j]
      
      east_dif2 <- desurv[i, 11] - (east_dif * j)
      north_dif2 <- desurv[i, 10] - (north_dif * j)
      rls_dif2 <- desurv[i, 12] - (rls_dif * j)
      temp_md <- desurv[i, 2] + (desurv[i + 1, 9] * j)
      
      all_dif <- cbind(desurv[i,1], temp_md, east_dif2, north_dif2, rls_dif2) # Create single row data cbind
      all_dif <- na.omit(all_dif) # Get rid of na
      desurv_lerp <- rbind(desurv_lerp, all_dif) # Rbind everything within i to i + 1 sequence of iteration
    }
  }
  desurv_lerp <- rbind(desurv_lerp, desurv_inverse_filter) # rbind length = 1 data into interpolated data where length >=2 
}
planned_desurv <- desurv_lerp

#### 4. Generate final dataframes for use, error codes needed? - CHECKPOINT ####
# Create act_desurv by filtering Pjt_desurv to OKD_col EOH
temp_col1 <- Pjt_desurv %>%
  distinct(bhid)
act_col <- left_join(temp_col1, OKD_col, by = "bhid") # Filter for bhid in pjt_desurv
act_col <- act_col %>% # Select EOH only
  select(bhid, eoh)
act_desurv <- left_join(Pjt_desurv, act_col, by = "bhid")
act_desurv <- act_desurv %>% # Uses interpolation in 2, which is built from act and proj surveys
  mutate(filter = eoh - md) %>% # Then filter out the proj based on EOH value in OKD_col
  filter(filter >= 0) %>%
  select(-eoh, -filter)
bhid_subset <- Pjt_desurv %>%
  distinct(bhid)

# md seems to be jumbled up after desurv and lerp
Pjt_desurv <- Pjt_desurv %>%
  group_by(bhid) %>%
  arrange(md)
planned_desurv <- planned_desurv %>%
  group_by(bhid) %>%
  arrange(md)
act_desurv <- act_desurv %>%
  group_by(bhid) %>%
  arrange(md)

#### 5. Distance from actual to target ####
distance_remaining <- left_join(act_desurv,targ_csv, by = "bhid") %>% # Distance remaining to target from actual drilling
  mutate(distance = sqrt( # Pythagoras 
    (easting - x)^2 +
      (northing - y)^2 +
      (rls - z)^2)) %>%
  group_by(target_id) %>%
  summarise(distancetotarget = min(distance, na.rm = TRUE))
distance_remaining <- left_join(distance_remaining, targ_csv, by = "target_id") %>% # Add more details onto the table
  select(bhid, pad_id, distancetotarget, target_id, rig_id)
distance_remaining$distancetotarget <- round(distance_remaining$distancetotarget, digits = 1) # Rounding

#### 6. min distance to targets, table, Int pdf file for both 5 and 6. PDF creation starts here ####
target_deviation <- left_join(Pjt_desurv,targ_csv, by = "bhid") %>% # Distance remaining to target from projected drilling
  mutate(distance = sqrt( # Pythagoras 
    (easting - x)^2 +
      (northing - y)^2 +
      (rls - z)^2)) %>%
  group_by(target_id) %>%
  summarise(distancetotarget = min(distance, na.rm = TRUE))
target_deviation <- left_join(target_deviation, targ_csv, by = "target_id") %>% # Add more details onto the table
  select(bhid, pad_id, distancetotarget, target_id)
target_deviation$distancetotarget <- round(target_deviation$distancetotarget, digits = 1) # Rounding

distancereport <- left_join(target_deviation, distance_remaining, by = "target_id") %>%
  select(bhid.x, pad_id.x, distancetotarget.x, target_id, distancetotarget.y, rig_id) %>%
  rename(bhid = bhid.x) %>%
  rename(pad_id = pad_id.x) %>%
  rename(proj_dis_to_targ = distancetotarget.x) %>%
  mutate(remain_dist = distancetotarget.y - proj_dis_to_targ) %>% # Projection is stiched onto actual trace. Once actual is at projection point, then = 0. x < y but never x > y 
  select(-distancetotarget.y) %>%
  relocate(remain_dist, .before = proj_dis_to_targ)

## Initialize pdf file
pdf(file = paste0(wd_export, "/DistanceReport.pdf"))
grid.table(distancereport)
dev.off()

print("DistanceReport.pdf Completed!")

## Append to "database"
setwd (wd_db) # DB wd
db <- read.csv("DistanceReportDB.csv")

distancereport_db <- distancereport %>%
  mutate(AppendDate = as.character.Date(ymd(today())))
db <- rbind(db, distancereport_db)

write.csv(db, "DistanceReportDB.csv",row.names = FALSE)

#### 7. actual + projected vs plan, graphing, min distance algorithm, Int pdf file ####
# Filter for max depth in either Pjt and planned
temp1 <- Pjt_desurv %>%
  group_by(bhid) %>%
  summarise(maxdepth = max(md, na.rm = TRUE))
temp2 <- planned_desurv %>%
  group_by(bhid) %>%
  summarise(maxdepth = max(md, na.rm = TRUE))
temp_maxdepth <- left_join(temp1, temp2, by = "bhid") %>%
  mutate(maxmd = pmin(maxdepth.x, maxdepth.y)) %>% # pmin performs parallel min function across individual elements, min performs across all
  select(bhid, maxmd)
temp_pjtmd <- left_join(Pjt_desurv, temp_maxdepth, by = "bhid") %>%
  mutate(temp_filter = maxmd - md) %>%
  filter(temp_filter >= 0) %>% # Filter out any data beyond max md
  select(-temp_filter)
temp_plannedmd <- left_join(planned_desurv, temp_maxdepth, by = "bhid") %>%
  mutate(temp_filter = maxmd - md) %>%
  filter(temp_filter >= 0) %>% # Filter out any data beyond max md
  select(-temp_filter)

# Vectorized version of minimum distance algorithm
min_distance_target <- left_join(temp_pjtmd, temp_plannedmd, by = c("bhid"), relationship = "many-to-many") %>%
  rename(easting_pjt = easting.x) %>% 
  rename(northing_pjt = northing.x) %>%
  rename(rls_pjt = rls.x) %>%
  rename(easting_plan = easting.y) %>%
  rename(northing_plan = northing.y) %>%
  rename(rls_plan = rls.y) %>%
  na.omit(easting_plan) %>% # QAQC in case of any NAs (Could be removed)
  mutate(distance = sqrt( # Pythagoras 
    (easting_pjt - easting_plan)^2 +
      (northing_pjt - northing_plan)^2 +
      (rls_pjt - rls_plan)^2)) %>%
  group_by(bhid, md.x) %>% # Group by bhid and measured distance
  summarise(distancetotarget = min(distance, na.rm = TRUE)) %>%
  rename(md = md.x) %>%
  rename(distance = distancetotarget)

temp_subset <- min_distance_target %>%
  distinct(bhid, .keep_all = FALSE)

# Intialize pdf file
pdf(file = paste0(wd_export, "/ProjectedvsPlanReport.pdf"))

for (i in 1:nrow(temp_subset)){
  temp_plot <- min_distance_target %>%
    filter(bhid == temp_subset[i,])
  
  plot <- temp_plot %>%
    ggplot(aes(x = md, y = distance, colour = bhid)) +
    # geom_smooth(se = FALSE) + # Smoothed plotting
    geom_point(size = 0.2) + # Raw data plotting
    ylim(0, max(temp_plot$distance)) + # Y scale limit from 0 - EOH
    labs(title = "Projected vs Plan") +
    xlab("Measured Distance") +
    ylab("Distance between planned and projected trace")
  print(plot)
}
dev.off()

print("ProjectedvsPlanReport.pdf Completed!")


#### 8. Section views, Int pdf file ####
temp_subset <- act_desurv %>%
  distinct(bhid, .keep_all = FALSE)

# Intialize pdf file
pdf(file = paste0(wd_export, "/SectionViewReport.pdf"))

for (i in 1:nrow(temp_subset)){
  temp_plot <- act_desurv %>% # Filter actual dataset for subset
    filter(bhid == temp_subset[i,])
  temp_plot1 <- planned_desurv %>% # Filter planned dataset for subset
    filter(bhid == temp_subset[i,])
  temp_plot2 <- targ_csv %>% # Filter target for subset
    filter(bhid == as.character(temp_subset[i,]))
  
  temp_name <- temp_subset[i,] # Plot name
  
  planview_plot <- temp_plot %>%
    ggplot() +
    geom_path(data = temp_plot, aes(easting, northing), colour = "Red", alpha = 1) +
    geom_path(data = temp_plot1, aes(easting, northing), colour = "Black", alpha = 0.2) +
    geom_point(data = temp_plot2, aes(x, y), shape = 4) +
    labs(title = "Plan view", subtitle = temp_name, x = "Easting", y = "Northing") +
    coord_equal() +
    theme_void()
  
  sectionview_plot <- temp_plot %>%
    ggplot() +
    geom_path(data = temp_plot, aes(easting, rls), colour = "Red", alpha = 1) +
    geom_path(data = temp_plot1, aes(easting, rls), colour = "Black", alpha = 0.2) +
    geom_point(data = temp_plot2, aes(x, z), shape = 4) +
    labs(title = "Section view", subtitle = temp_name, x = "Easting", y = "RL") +
    coord_equal() +
    theme_void()
  
  print(planview_plot)
  print(sectionview_plot)
  # planview_plot
  # sectionview_plot
}
dev.off()
print("SectionsViewReport.pdf Completed!")

#### 9. Scatter plot of past distance values ####
setwd (wd_db)
distance_report_db <- read.csv("DistanceReportDB.csv")

distance_report <- clean_names(distance_report_db)
temp_dmy <- suppressWarnings(dmy(distance_report$append_date)) # Lubridate!
temp_ymd <- suppressWarnings(ymd(distance_report$append_date)) # Lubridate! the other way
distance_report <- distance_report %>%
  mutate(append_date = coalesce(temp_dmy, temp_ymd))

# Figuring out which financial year we are in
fy <- make_date(year = year(today()), month = 7, day = 1)
fy_other <- make_date(year = year(today()) - 1, month = 7, day = 1)
fy_start <- as.Date(((ifelse(today() >= fy, fy, fy_other))), origin = "1970/1/1")
date_filter <- fy_start 

distance_report <- distance_report %>%
  mutate(index = 1:nrow(distance_report_db)) %>%
  mutate(unique_key = paste0(pad_id, append_date)) %>% # Create for distinct function
  arrange(desc(index)) %>%
  distinct(unique_key, .keep_all = T) %>%
  filter(proj_dis_to_targ > 0) # Remove -999 errors

distance_report <- distance_report %>%
  filter(append_date > date_filter) %>%
  mutate(rig_id = as.character(rig_id)) # Data type change

# Separate contractors
temp_delim <- distance_report %>%
  select(rig_id, index)

temp_delim <- separate_wider_delim(data = temp_delim, col = rig_id, delim = "-", names = ("contractor"), too_many = "drop")

distance_report <- cbind(distance_report, temp_delim, distance_report$index)
distance_report <- distance_report[,1:10]

distance_report_for <- distance_report %>%
  filter(contractor == "FOR") %>%
  filter(remain_dist == 0) # Requested by ops to change graph to show only results of hitting targets
distance_report_tld <- distance_report %>%
  filter(contractor == "TLD") %>%
  filter(remain_dist == 0) # Requested by ops to change graph to show only results of hitting targets

# Stats and leaderboards for drilling hitting targets < 20
distance_report_merge <- rbind(distance_report_for, distance_report_tld) %>%
  mutate(unique_key_2 = paste0(target_id, proj_dis_to_targ)) %>%
  distinct(unique_key_2, .keep_all = T) %>%
  distinct(target_id, .keep_all = T)
leaderboard_for_full <- distance_report_merge %>%
  filter(contractor == "FOR") %>%
  select(-unique_key, -unique_key_2, -index) %>%
  group_by(rig_id) %>%
  arrange(desc(append_date), .by_group = T) %>%
  ungroup(rig_id)
summary_for <- summary(leaderboard_for_full$proj_dis_to_targ)
leaderboard_tld_full <- distance_report_merge %>%
  filter(contractor == "TLD") %>%
  select(-unique_key, -unique_key_2, -index) %>%
  group_by(rig_id) %>%
  arrange(desc(append_date), .by_group = T) %>%
  ungroup(rig_id)
summary_tld <- summary(leaderboard_tld_full$proj_dis_to_targ)

# Create summarized leaderboard to copy onto C2D slide
leaderboard_for <- leaderboard_for_full %>%
  select(bhid, proj_dis_to_targ, append_date, rig_id) %>%
  rename(distance = proj_dis_to_targ, date = append_date)
leaderboard_tld <- leaderboard_tld_full %>%
  select(bhid, proj_dis_to_targ, append_date, rig_id) %>%
  rename(distance = proj_dis_to_targ, date = append_date)

# easy paste leaderboard to copy onto C2D slide
leaderboard <- rbind(leaderboard_for, leaderboard_tld)

pdf(file = paste0(wd_export, "/leaderboard_images.pdf"))

for (i in unique(leaderboard$rig_id)){
  leaderboard_data <- leaderboard %>%
    filter(leaderboard$rig_id == i)
  printed_leaderboard <- grid.table(leaderboard_data)
  grid.newpage()
}
dev.off()

successrate_for <- round(sum(leaderboard_for_full$proj_dis_to_targ <= 20) / count(leaderboard_for_full), digits = 2)
successrate_tld <- round(sum(leaderboard_tld_full$proj_dis_to_targ <= 20) / count(leaderboard_tld_full), digits = 2)

print(paste0("Contractor distance from target < 20m since: ", date_filter))
print(paste0("Foraco success rate: ", successrate_for * 100, "%"))
print(summary_for)
print(paste0("Titeline success rate: ", successrate_tld * 100, "%"))
print(summary_tld)

setwd(wd_export)
write.csv(leaderboard_for, "leaderboard_for.csv", row.names = F)
write.csv(leaderboard_tld, "leaderboard_tld.csv", row.names = F)
write.csv(leaderboard_for_full, "leaderboard_for_full.csv", row.names = F)
write.csv(leaderboard_tld_full, "leaderboard_tld_full.csv", row.names = F)

# Plot
plot_for <- distance_report_for %>%
  ggplot(aes(x = append_date, y = proj_dis_to_targ, color = rig_id, group = rig_id)) +
  geom_point(size = 2, shape = 15) +
  geom_path() +
  xlab("Date") +
  ylab("Distance from planned target") +
  theme_classic() +
  geom_ribbon(data = NULL, aes(ymin = 0, ymax = 20), fill = "green", alpha = 0.03, color = NA) + # Color NA remove border
  geom_ribbon(data = NULL, aes(ymin = 20, ymax = 100), fill = "red", alpha = 0.03, color = NA) +
  coord_cartesian(ylim = c(0,50)) # Change axis without removing coordinates unlike ylim

setwd (wd_export) # Change wd to export
ggsave(file = "DistanceScatterPlot_Foraco.png", height = 3, width = 7, dpi = 320) # Save plot

plot_tld <- distance_report_tld %>%
  ggplot(aes(x = append_date, y = proj_dis_to_targ, color = rig_id, group = rig_id)) +
  geom_point(size = 2, shape = 15) +
  geom_path() +
  xlab("Date") +
  ylab("Distance from planned target") +
  theme_classic() +
  geom_ribbon(data = NULL, aes(ymin = 0, ymax = 20), fill = "green", alpha = 0.03, color = NA) + # Color NA remove border
  geom_ribbon(data = NULL, aes(ymin = 20, ymax = 100), fill = "red", alpha = 0.03, color = NA) +
  coord_cartesian(ylim = c(0,50)) # Change axis without removing coordinates unlike ylim

setwd (wd_export) # Change wd to export
ggsave(file = "DistanceScatterPlot_Titeline.png", height = 3, width = 7, dpi = 320) # Save plot

toc()
print("All done!, Thank you!")