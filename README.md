# Drillhole Deviation Calculator (DDC)
This process was intially ran with a two part script. The first part produced the necessary inputs for Leapfrog to assist in de-surveying, whilst the second part produced the necessary graphs and outputs used in the final C2D report that is to be sent out to the contractors. The process eventually evolved (2 part merged into 1, with vectorization) to remove the tedious handling of data between scripts and Leapfrog to cater to the usage by the team. This created minor quality issues with the interpolation of de-surveyed drill traces (see appendix/used calculations)
![image](https://github.com/user-attachments/assets/cff7426c-1453-4c4d-954d-ad7d7af8da51)

## <ins>Purpose</ins>
Each week, the drilling contractors send out projections of the active drill holes to the geology department to track the drill holes progress and spatial accuracy (to target and plan). This compliance to plan (C2D) report is generated from the geology department as a formal document to inform the drilling contractors, operations department and any other stakeholders on the progress and drill tracking compliance of each rig/drill hole. 

## <ins>First time setup for Oak Dam Geologists</ins>
After installing Rstudio for the first time, an initial manual install for packages may need happn as the install and load packages FUN might not work. Enter the below chunk into the console. The is a run-once-per-setup
```
packages <- c("readxl","lubridate","tidyverse","ggplot2", "plotly", "tictoc", "janitor", "gridExtra", "grid")
install.packages(packages)
library(readxl)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(plotly)
library(tictoc)
library(janitor)
library(gridExtra)
library(grid)
```
## <ins>Procedure</ins>
Refer to QRG in BHP sharepoint

## <ins>Appendix</ins>
-	The distancereport.pdf values are the same (with exception to rounding) to using the ruler in Leapfrog to measure the smallest distance between the drill trace and the string
-	Section 2 involves the de-surveying chunk of the code which uses the minimum curvature method. Online material showed two formulas for the method. It is to be noted the formula used in the code is different:
  ![image](https://github.com/user-attachments/assets/1325ea2c-425c-4c7f-9b6f-c97eb1b77774)
- Section 2's de-surveying also interpolates intensely between the converted survey - coordinates data to create as many points (increased resolution) for a minimum distance to target to be measured as "accurately" as possible. However due to inconsistent distance between each survey station, writing code to interpolate between stations is complicated (non-divisble lengths between stations)

### <ins>Used calculations</ins>
- De-survey without any ratio factors applied as per minimum curvature method
```
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
```

## WIP
- I do not even remember how I came up with this
```
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
```
