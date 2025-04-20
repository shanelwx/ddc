# Drillhole Deviation Calculator (DDC)
This process was intially ran with a two part script that helped 
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
Ruler in Leapfrog vs distancereport.pdf values
-	The distancereport.pdf values are the same (with exception to rounding) to using the ruler in Leapfrog to measure the smallest distance between the drill trace and the string
-	Section 2 involves the de-surveying chunk of the code which uses the minimum curvature method. Online material showed two formulas for the method. It is to be noted the formula used in the code is different:
  ![image](https://github.com/user-attachments/assets/1325ea2c-425c-4c7f-9b6f-c97eb1b77774)
- Section 2's de-surveying also interpolates intensely between the converted survey - coordinates data to create as many points (increased resolution) for a minimum distance to target to be measured as "accurately" as possible. However due to inconsistent distance between each survey station, writing code to interpolate between stations is complicated (non-divisble lengths between stations)

### <ins>Used calculations</ins>
