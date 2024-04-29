#For setting up detection history using camtrapR:
  
# First to set up a camera operation table:
# CTtable is a data frame with my camera information (which would be just the 
# station information) and includes columns for (I attached the first 10 grid cells of my CTtable as an example):
  # grid ID because we are considering all cameras within a grid cell as a single 'station', needs to match with records table if grouping by grid cells
  # camera ID to individually identify camera setups, this needs to match with the records table if not grouping by grid cells (see below)
  # set date for the setup date and optionally the time of the set (if time not provided camtrapR defaults to assume all are set at noon)
  # retrival date for the date the station was taken down and optionally the time (if time not provided camtrapR defaults to assume all retrivals are at noon)
  # problem columns, these are set so that there are pairs with problem1_from and problem1_to, problem2_from and problem2_to, etc so that each pari represents the start and end time of when the station was inactive for whatever reason
CTtable <- read.csv("./Data/Example_Data/FromJordan_CamTrapR/CTtable_ex.csv")

op_table <- cameraOperation(CTtable = CTtable, 
                            # Used station column to indicate the grid cell
                            stationCol = "grid_id", 
                            cameraCol = "Camera_ID", 
                            # Used session column to indicate the season
                            sessionCol = "yr",
                            setupCol = "ssn_set", 
                            retrievalCol = "ssn_ret",
                            # Indicates that there are problem columns:
                            hasProblems = FALSE, 
                            # byCamera = FALSE allows for lumping cameras by 
                            # 'station', in this case, grid
                            byCamera = FALSE, 
                            # allCamsOn = FALSE, means that not all cameras 
                            # need to be active for the station/grid cell to 
                            # be considered active
                            allCamsOn = FALSE,
                            # camerasIndependent = TRUE allows matrix to 
                            # return the number of operational cameras at 
                            # the station/grid cell on each encounter period
                            camerasIndependent = TRUE, 
                            dateFormat = "%Y-%m-%d",
                            occasionStartTime = 0,
                            writecsv = TRUE, # whether or not to save the outputted camera operation table for later reading
                            outDir = getwd()) # where to save it

# Next for the detection histories:

  # ssn_detect_dt is a data frame with detection information and required columns
  # including:
    # a species column for what species was detected
    # a grid id column that matches with the camera operation table as that is how we grouped cameras
    # a date/time column for the date and time of the detection
ssn_detect_dt <- read.csv("./Data/Example_Data/FromJordan_CamTrapR/recordTable_ex.csv")

cala_dh <- detectionHistory(recordTable = ssn_detect_dt,
                            species = "Canis latrans",
                            camOp = op_table,
                            output = "binary",
                            stationCol = "grid_id",
                            speciesCol = "Species",
                            recordDateTimeCol = "DateTimeOriginal",
                            recordDateTimeFormat = "%Y-%m-%dT%H:%M:%SZ",
                            occasionLength = 7, # how many days per encounter period?
                            # minActiveDaysPerOccasion,
                            # maxNumberDays,
                            day1 = "survey", # check the help file on this one
                            # buffer,
                            includeEffort = TRUE,
                            scaleEffort = FALSE, # I scaled everything later and wanted to have the unscaled amount too
                            # occasionStartTime = "deprecated",
                            datesAsOccasionNames = TRUE, # this way I knew which days each encounter period lined up with
                            timeZone = "UTC", 
                            writecsv = TRUE,# save csvs with the outputted tables for recalling later
                            outDir = getwd(), # where to save
                            unmarkedMultFrameInput = FALSE) # not doing a multiseason so left as false

# Once you have both of those, you can make an unmarked frame with the y 
# argument as your detection table from the detectionHistory() function

# Create UMF multi frame
ssn_umf <- unmarkedFrameOccu(y = cala_dh$detection_history#, 
                             #siteCovs = site_covs, 
                             #obsCovs = obs_covs
                             )
