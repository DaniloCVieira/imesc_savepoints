source('Auxiliar_functions.R')
library(kohonen)
library(caret)
library(aweSOM)
library(factoextra)
library(randomForestExplainer)
library(sf)
library(raster)
library(data.table)
library(GGally)
## Reading reads data and pre-processing reads data
reads_data<-read.csv("reads_data.csv",sep=";",header=T, row.names=1)
# Apply hellinguer transformation to reduce the effect of dominant taxa
reads_hellinguer<-vegan::decostand(reads_data,"hell")


## Reading reads data and pre-processing coordinates + depth data
coords_depth<-read.csv("Coords_Depth_data.csv",sep=";",row.names=1)
# Load coordinates and depth data and scale the features for consistency
cd_scaled<-scale(coords_depth)

###########################################################
#######                             #######################
#######   UNSUPERVISED LEARNING     #######################
#######                             #######################
###########################################################


## Prepare a list of two layers: environmental features (layer1) and reads data (layer2)
training_list<-list(
  layer1=as.matrix(reads_hellinguer),
  layer2=as.matrix(cd_scaled)
)

## Defining initial values for the codebook vectors: Setting the number of codebook vectors and initialize them randomly from the dataset

ncodes<-36
set.seed(42)
starters<-sample(1:nrow(cd_scaled), ncodes, replace = FALSE)
init<-lapply(training_list, function(x) x[starters, , drop = FALSE])

# Train a Self-Organizing Map (SOM) with two layers using hexagonal grid topology
set.seed(42)

som_model<-kohonen::supersom(
  data=training_list,
  init=init,
  radius=c(3.605551,0),
  alpha=c(0.05, 0.01),
  maxNA.fraction=c(0.001),
  dist.fcts=c("euclidean","euclidean"),
  grid=somgrid(x=6,y=6,topo="hexagonal",neighbourhood.fct="gaussian",toroidal=F),
  rlen=500,
  user.weights=c(0.95,0.05)
)


## SOM Evaluation metrics
# Assign sample names to SOM classification results
names(som_model$unit.classif)<-rownames(cd_scaled)
# Evaluate the SOM quality for layer1 (reads data)
aweSOM::somQuality(som_model,as.matrix(reads_hellinguer))
# Temporarily switch to layer2 (coords_depth data) and re-evaluate SOM quality
som_model_temp<-som_model
som_model_temp$codes[[1]]<-som_model_temp$codes[[2]]
aweSOM::somQuality(som_model_temp,as.matrix(cd_scaled))

## Hierarchical clustering
# Compute distances between SOM codebook vectors
dist_codes<-kohonen::object.distances(som_model,"codes")
# Perform hierarchical clustering with Ward's method into 5 clusters
hc_model<-factoextra::hcut(dist_codes,k=4,isdiss =T,hc_fun="hclust",hc_method = "ward.D2")
hcut_result<-hc_model$cluster
names(hcut_result)<-1:nrow(som_model$codes[[1]])

## Assigning samples to the SOM clusters: mapping each sample to a SOM cluster based on hierarchical clustering results
dfcut<-data.frame(neu=names(hcut_result),hcut_result)
list<-split(data.frame(id=names(som_model$unit.classif),neu=som_model$unit.classif),som_model$unit.classif)
res<-do.call(rbind,lapply(names(list),function(i){
  x<-list[[i]]
  x$hc<- dfcut[i,"hcut_result"]
  x
}))
newclass<-res$hc
names(newclass)<-rownames(res)
clusters<-newclass[rownames(som_model$data[[1]])]

## Ordering cluster levels
Associations<-factor(clusters,levels=c(4,2,1,3),labels=1:4)

hcut_result<-factor(hcut_result,levels=c(4,2,1,3),labels=1:4)


## Dendrogram
gg_dendrogram(hcut_result,hc_model, theme="theme_minimal")



## Indicators

set.seed(42)
indicators<-indicator_multipart(reads_hellinguer, Associations,5,nperm=999)
indicator_taxa<-names(sort(filter_multipart_result(indicators,5)))
summary_indicators<-summary_multipatt(indicators)
#write.table(summary_indicators,'summary_indicators.csv',sep=";", col.names=NA)


###########################################################
#######                             #######################
#######     SUPERVISED LEARNING     #######################
#######                             #######################
###########################################################

# RF CLASSIFICATION - Microbial Associations

# Step 1: Split the data into training (80%) and test (20%) sets, stratifying by clusters
partition<-rep("test",length(Associations))
set.seed(42)
test_obs<-caret::createDataPartition(Associations,p=0.8)
partition[test_obs[[1]]]<-"training"
partition<-factor(partition)
names(partition)<-names(Associations)
environ_data<-read.csv("environ_data.csv",sep=";",header=T, row.names=1)
environ_datalist<-split(environ_data,partition)
Associations_list<-split(Associations,partition)
train_data<-environ_datalist$training
test_data<-environ_datalist$test
train_y<-Associations_list$training
test_y<-Associations_list$test


# Step 2: Train a Random Forest classification model with hyperparameter tuning and cross-validation
set.seed(42)
rf_model<-caret::train(
  x=train_data,
  y=train_y,
  method = "rf", ntree = 500L, replace = TRUE, nodesize = 1L,
  nPerm = 1L, norm.votes = TRUE, localImp = TRUE, keep.forest = TRUE,
  keep.inbag = TRUE,
  trControl = trainControl(method = "repeatedcv",
                           number = 5L,
                           repeats = 1,
                           search = "grid",
                           p = 0.1,
                           initialWindow = NULL,
                           horizon = 1,
                           fixedWindow = TRUE,
                           skip = 0, verboseIter = FALSE,
                           returnData = TRUE,
                           returnResamp = "final",
                           savePredictions = "all"),
  tuneLength =20
)




## Feature importance
imp_rf_classication<-multipimp( rf_model,measures=c(c("mean_min_depth",
                                                      "accuracy_decrease",
                                                      "gini_decrease" ,
                                                      "no_of_nodes",
                                                      "times_a_root"),'p_value','no_of_trees'), mean_sample="top_trees")


# Step 7: Obtain Random Forest predictions
rf_predictions <- predict(rf_model, test_data)

# Step 8: Evaluate Random Forest performance
rf_conf_matrix <- confusionMatrix(as.factor(rf_predictions), test_y)

# Print Random Forest performance
cat("Random Forest Confusion Matrix:\n")
print(rf_conf_matrix)


