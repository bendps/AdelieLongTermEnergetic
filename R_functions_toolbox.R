##################################################################
##                      R function toolbox                      ##
##################################################################

# This R script regroups numerous usefull functions written during my PhD
# List of the functions :

# 1. pressure2Depth : Conversion of logger pressuredata to depth, adapted for DDU
# 2. clean0depth : Correction of the 0 level of TDR data
# 3. trip_cleaner : Separate a file in multiple trips based on distance to colony
#bannerCommenter::banner(txt, centre = TRUE, bandChar = "-") %>% bannerCommenter::copy_to_clipboard()

##----------------------------------------------------------------
##                        pressure2depth                        --
##----------------------------------------------------------------

pressure2depth <- function(press) { #pressure un vector contenant pressure en mbar
  atm <- 9.8 # pression atmospherique moyenne a DDU
  x <- (sin(-66.66306 / 57.29578)) ^ 2
  p10 <- press / 100 #put pressure in dbar for below
  g <- 9.780318 * (1 + (5.2788e-3 + 2.36e-5 * x ) * x ) + 1.092e-6 * p10 #p en dbar et g en (m/s-2)
  depth <- (((((-1.82e-15 * p10 + 2.279e-10 )  * p10 -2.2512e-5 ) * p10 +9.72659 ) * p10 ) / g ) - atm  #depth en m        
  
  return(depth)
}

##---------------------------------------------------------------
##                         clean0depth                         --
##---------------------------------------------------------------

clean0depth <- function(depth_data, time_window){
  require(tidyverse)
  require(scattermore)
  require(zoo)
  
  if(missing(time_window)){time_window <- 240} #4min by default
  
  new_zero <- rollapply(data = depth_data, width = time_window, FUN = "min", fill = NA) 
  correc_depth <- depth_data - new_zero
  return(correc_depth)
}

##----------------------------------------------------------------
##                         trip_cleaner                         --
##----------------------------------------------------------------

trip_cleaner <- function(ori_df, c_dist_thresh, trip_dur_thresh){
  require(tidyverse)
  require(data.table)
  
  if(missing(c_dist_thresh)){c_dist_thresh <- 100}
  if(missing(trip_dur_thresh)){trip_dur_thresh <- 12} #In interpolated interval units so 12*15min = 3 hrs
  
  ade_sf <- ori_df
  
  while (max(ade_sf$c_dist > 1000)) {
    #Find the beginning of the trip (i.e the first time bird leave the colony for a trip_dur_thresh and at a c_dist_thresh distance)
    setDT(ade_sf)
    start_trip <- ade_sf[, {above <- c_dist > c_dist_thresh
    ends <- which(above & rowid(rleid(above)) == trip_dur_thresh)
    .(Dep_Time = date[ends[1] - trip_dur_thresh])}]
    start_trip <- c(as_tibble(start_trip))
    
    temp_trip <- ade_sf[which(ade_sf$date == start_trip[1]):nrow(ade_sf)]
    
    #Get return, but need to check if the trip is complete (i.e if the bird is coming back to the colony), else discard
    thresholdObs <- which(temp_trip$c_dist < 100)
    if(length(intersect(thresholdObs, thresholdObs + 1)) >= 1){#If length is 1 or greater, trip is coming back to the colony
      end_trip <- temp_trip$date[min(intersect(thresholdObs, thresholdObs + 1))-1]
      final_trip <- as_tibble(ade_sf[which(ade_sf$date == start_trip):which(ade_sf$date == end_trip),]) #Trip extracted
      ade_sf <- ade_sf[which(ade_sf$date == end_trip):nrow(ade_sf),] #Remaining part of the track to check for other trips
    }else{print("Trip incomplete, discarded")}
  }
}


##----------------------------------------------------------------
##                         model_selection                      --
##----------------------------------------------------------------
generate_models <- function(response, predictors, random_effect) {
  n <- length(predictors)
  comb_list <- lapply(1:n, function(x) combn(predictors, x, simplify = FALSE))
  comb_list <- unlist(comb_list, recursive = FALSE)
  
  formulas <- lapply(comb_list, function(vars) {
    form <- paste(response, "~", paste(vars, collapse = " + "), "+ (1|", random_effect, ")")
    as.formula(form)
  })
  
  # Add the null model formula
  null_formula <- as.formula(paste(response, "~ 1 + (1|", random_effect, ")"))
  formulas <- c(list(null_formula), formulas)  # Add null model to the beginning of the list
  
  return(formulas)
}

# # Example usage
# predictors <- c("predictor1", "predictor2", "predictor3")
# response <- "response"
# random_effect <- "random_effect"
# 
# model_formulas <- generate_models(response, predictors, random_effect)
