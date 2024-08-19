# This script is for the analysis and figure generation of the main results

### Setup ###
# Load the functions
source("analysis_functions.R")
# Load the workspace
load("../fabisearch_examples.RData")
# Load the views file
load("views.rda")
# Set working directory to the plots folder
setwd("../plots")

##### fMRI Data #####
### Gordon Atlas Plots ###
# Plot and save the clust.net results
fmriHeat(clust.net, "clustheat")
# Plot and save the thresh.net elbow plot results
fmriElbow(thresh.net, "threshelbow")
# Plot and save the thresh.net results
fmriHeat(thresh.net, "threshheat", 40)
# Plot the entire 1st stationary segment from the thresh.net list
fmri3D(thresh.net[[1]][[50]], "threshnetALL", views)
# Plot the 1st stationary segment from the thresh.net list, but 
# only the "Visual" and "None" communties, and add node labels
fmri3D(thresh.net[[1]][[50]], "threshnet", views, ROIs = c("Visual", "None"),
       colors = c("#FFEB3B", "#673AB7"), labels = TRUE)

### AAL Atlas Plots ###
# Plot and save the AAL first stationary network results
fmriElbow(AAL.net, "AALelbow")
# Plot the stationary segment
fmri3D(AAL.net[[38]], "AALnetALL", views, coordROIs = AALatlas)
# Plot a heatmap of the stationary segment
fmriHeat(AAL.net[[38]], "AALheat")
# Plot only the first 30 nodes in the stationary segment
fmri3D(AAL.net[[38]], "AALnet1to30", views, coordROIs = AALatlas,
           labels = TRUE, ROIs = 1:30)
#####################

##### Financial Data #####
# Open the png file
png("SP500netsraw.png", height = 2000, width = 3000, res = 275)

# Plot the results
top.15 = colnames(logSP500) %in% tickers
par(mfrow=c(3,2), mar = c(5, 0, 0, 0))
for(i in 1:length(SP500net)){
  G = graph_from_adjacency_matrix(SP500net[[i]][top.15,top.15],
                                  mode = "undirected", diag = FALSE)
  V(G)$color = palette()[components(G)$membership]
  plot(G, layout=layout_with_kk, vertex.size=20,
       vertex.color="white", vertex.frame.color = "white",
       vertex.label.color = V(G)$color, asp = 0.5)
  Sys.sleep(1)
}

# Close the png file
dev.off()
##########################

##### Perspective/Manifold Plots #####
# Plot using the function
maniPlots()