###########################################################################
# Copyright         : 2024. Yaginuma et al.
# File Name         : Seleting_testpoint.R
# Description       : Script to select the closest coordinates 
# points from the gridded data to the points of the test data 
# and to calculate the evaluation metrics between the predictions 
# of the Test data and the gridded data.
###########################################################################

rm(list=ls())

# Restoring the savepoint file into the working directory
vals<-readRDS("Savepoint_HybridModel.rds")

# Checking the name of the datalists
names(vals$saved_data)

### Observed data ###
datalist_observed<-"nema_c1_mean_log10" # name of the datalist containing the observed association of each station 
data_observed<-vals$saved_data[[datalist_observed]] # accessing the datalist with the observed associations
coords_observed<-attr(data_observed,'coords') # accessing coordinates-attributes of the datalist
factors_observed<-attr(data_observed,"factors") # accessing factor-attributes of the datalist
colnames(factors_observed) # checking column names of the factor-attribute of the datalist
colname_HC<-"HC6log" # defining the name of the column containing the observed associations
observed<-factors_observed[,colname_HC] # storing the observed association data

### Predicted Grid data ###
datalist_predicted<-"Pred_RF(HC6log~RFsig(+Depth)_grid)"# name of the datalist containing the predicted associations in the 2 km x 2 km grid
data_predicted<-vals$saved_data[[datalist_predicted]] # accessing the datalist with the predicted associations
coords_predicted<-attr(data_predicted,'coords') # accessing coordinates attributes of the datalist
factors_predicted<-attr(data_predicted,"factors") # accessing the factor-attibute of the datalist
colnames(factors_predicted) # Checking column names of the factor-attribute of the datalist
colname_HC_predicted<-"pred_HC6log" # defining the name of the column containing the predicted associations

# Function to find the closest coordinates between the points of the predicted data and the observed data
find_closest_coords <- function(coords, coords_grid) {

  library(sf)
  # Converting data.frames to objects sf
  coords_sf <- st_as_sf(coords, coords = c(colnames(coords)), crs = 4326)
  coords_grid_sf <- st_as_sf(coords_grid, coords = c(colnames(coords)), crs = 4326)
  nearest_indices <- st_nearest_feature(coords_sf, coords_grid_sf)

  # Subset coords_grid_sf using the nearest indices
  nearest_coords_grid_sf <- coords_grid_sf[nearest_indices, ]

  # Add an ID column to coords_sf to keep track of the original points
  coords_sf$id <- 1:nrow(coords_sf)

  # Add the associated ID from coords_sf to the nearest_coords_grid_sf
  nearest_coords_grid_sf$associated_id <- rownames(coords_grid)[nearest_indices]

  return(nearest_coords_grid_sf$associated_id)
}
# Run the fuction
closest_coords_ids<-find_closest_coords(coords_observed,coords_predicted)

predicted<-factors_predicted[closest_coords_ids,colname_HC_predicted] # storing the coordinates found by the function closest_coords_ids

df_pred<-data.frame(pred=predicted,obs=observed) # creating dataframe with the predicted and observed data of points with the closest coordinates

rownames(df_pred)<-rownames(factors_observed) # replacing the rownames of the data frame by the ID of the points

## Results - Simple metrics
caret::postResample(df_pred$pred,df_pred$obs)

# Multiclass
caret::multiClassSummary(df_pred, lev=levels(df_pred$obs))

# Confusion matrix
caret::confusionMatrix(table(df_pred))

## Separating the IDs of the training and test datasets
train_ids<-rownames(factors_observed)[factors_observed$Partition_HC6log=="training"]
test_ids<-rownames(factors_observed)[factors_observed$Partition_HC6log=="test"]

# Separating into training and test dataframes
df_train<-df_pred[train_ids,]
df_test<-df_pred[test_ids,]

# Multiclass and Confusion matrix
caret::multiClassSummary(df_train, lev=levels(observed))
caret::multiClassSummary(df_test, lev=levels(observed))
caret::confusionMatrix(table(df_train))
caret::confusionMatrix(table(df_test))
