### Analysis functions file ###

# Load libraries
library(rgl)
library(fabisearch)
library(igraph)
library(plot3D)

# This function is for determining whether the original and replicate results are identical
checkIdentical = function(varname){
  # varname = name of variable to be checked - must be the same in 
  #           both the root folder as well as the replicates folder
  
  # Load the original work space in the main root folder
  load("../fabisearch_examples.RData")
  # Rename the original variable
  orig = eval(parse(text = varname))
  # If the variable is a list, remove $compute_time
  if(is.list(orig)){
    # Set $compute_time to null
    orig$compute_time = NULL
  }
  
  # Load the replicate work space from the replicate subfolder
  load("../replicate/fabisearch_examples.RData")
  # Rename the replicate variable
  repl = eval(parse(text = varname))
  # If the variable is a list, remove $compute_time
  if(is.list(repl)){
    # Set $compute_time to null
    repl$compute_time = NULL
  }
  
  # Compare the two
  return(identical(orig, repl))
}

# This function is for plotting and saving the heatmaps for stationary fMRI networks
fmriHeat = function(nets, name, cutoff = NULL){
  # nets = a list of networks for plotting
  # name = name used for the outputs
  # cutoff = for threshold networks, specifies the element number in the list
  
  # If the nets is a list of networks
  if(is.list(nets)){
    # Loop through the nets list
    for(n in 1:length(nets)){
      # Prepare the png file
      png(paste0(name, n, ".png"))
      
      # If the cutoff is not null, then select that element for plotting
      if(is.null(cutoff)){
        # Plot the individual networks as heatmaps
        heatmap(nets[[n]], col = grey(c(0.97,0.1)), symm = TRUE,
                Colv = NA, Rowv = NA, labRow = NA, labCol = NA)
      } else{
        # Plot the individual networks as heatmaps
        heatmap(nets[[n]][[cutoff]], col = grey(c(0.97,0.1)), symm = TRUE,
                Colv = NA, Rowv = NA, labRow = NA, labCol = NA)
      }
      
      # Turn off the graphical device
      dev.off()
    }
  } else{
    # Prepare the png file
    png(paste0(name, n, ".png"))
    
    # If the cutoff is not null, then select that element for plotting
    if(is.null(cutoff)){
      # Plot the individual networks as heatmaps
      heatmap(nets, col = grey(c(0.97,0.1)), symm = TRUE,
              Colv = NA, Rowv = NA, labRow = NA, labCol = NA)
    }
    
    # Turn off the graphical device
    dev.off()
  }
}

# This function is for plotting and saving the elbow plots for stationary fMRI networks
fmriElbow = function(nets, name){
  # nets = a list of networks for plotting
  # name = name used for the outputs
  
  # If there are multiple networks
  if(is.list(nets[[1]])){
    # Prepare the png file
    png(paste0(name, ".png"), width = 3500, height = 1800, res = 200) 
    
    # Plot number of edges as a function of cutoff values
    par(mfrow = (c(1,length(nets))))
    for(n in 1:length(nets)){
      edges = c()
      for(i in 1:length(lambda)){
        curr.net = nets[[n]][[i]]
        edges = c(edges, sum(curr.net[upper.tri(curr.net)]))
      }
      plot(lambda, edges, cex.lab = 2, cex.axis = 2)
    }
  } else{
    # Prepare the png file
    png(paste0(name, ".png"), width = 4000, height = 2000, res = 200) 
    
    # Plot number of edges as a function of cutoff values
    edges = c()
    for(i in 1:length(lambda)){
      curr.net = nets[[i]]
      edges = c(edges, sum(curr.net[upper.tri(curr.net)]))
    }
    plot(lambda, edges, cex.lab = 2, cex.axis = 2)
  }
  
  # Turn off the graphical device
  dev.off()
}

# This function is for plotting and saving the brain frames for stationary fMRI networks
fmri3D = function(net, name, views, colors = NULL, labels = FALSE,
                  ROIs = NULL, coordROIs = NULL){
  # net = network to be plotted
  # name = name used for the outputs
  # views = views to plot, changes the rgl window orientation
  # colors = vector of colors used for community nodes
  # labels = whether to add labels to the 3D plots or not
  # ROIs = nodes/communities to be plotted, in vector format
  # coordROIs = atlas to use (default is Gordon 333*333)
  
  # Plot the network
  net.3dplot(net, ROIs = ROIs, colors = colors, coordROIs = coordROIs, labels = labels)
  
  # Loop through the different views
  for(i in 1:length(views)){
    # Change the view
    view3d(userMatrix = views[[i]], zoom = 0.7)
    
    # Save the plot
    rgl.snapshot(paste0(name, i, ".png"))
  }
  
  # Close the rgl window
  rgl.close()
}

# This function is used to create the colors used by the maniPlots function
newCol = function (n = 100, alpha = 1) 
{
  red <- c(0, 0, 0, 255, 255, 128)
  green <- c(0, 0, 255, 255, 0, 0)
  blue <- c(143, 255, 255, 0, 0, 0)
  x.from <- c(0, seq(0.125, 1, by = 0.25), 1)
  x.to <- seq(0, 1, length.out = n)
  expand <- function(col) approx(x = x.from, y = col, xout = x.to)$y
  return(rev(rgb(expand(red), expand(green), expand(blue), maxColorValue = 255, 
                 alpha = alpha * 255)))
}

# This function is used for plotting the manifold plots from the simulation results
maniPlots = function(rank = 2:6, nruns = c(2, 3, 5, 10, 15, 20, 25, 50, 100, 200), reps = 20, trueT = 100){
  # rank = values of rank used
  # nruns = values of nruns used
  # reps = number of repetitions of the simulation
  # trueT = true change point
  
  # Loop through and load results by rank
  allres = list()
  for(i in rank){
    # Load the current results
    load(paste0("../analysis/sensitivity_analysis/rank", i, "/rank", i, ".rda"))
    
    # Append this to the all.results list
    allres[[paste0("rank", i)]] = output
    
    # Remove the output list
    rm(output)
  }
  
  # Loop through the results and calculate TP and FP rates
  TPrate = c()
  FPrate = c()
  for(i in rank){
    for(j in nruns){
      # Load the current combination of rank and nruns
      currout = allres[[paste0("rank", i)]][[paste0("nruns", j)]]
      
      # Find the number out of 20 that were within 5 time points and outside (TP and FP respectively)
      TP = sum(currout %in% (trueT-5):(trueT+5))/reps
      FP = sum(!currout %in% (trueT-5):(trueT+5))/reps
      
      # Add these results to the respective tables
      TPrate = rbind(TPrate, data.frame(rank = i, nruns = j, TP = TP))
      FPrate = rbind(FPrate, data.frame(rank = i, nruns = j, FP = FP))
    }
  }
  
  # Open the png device
  png("sensitivityplots.png", width = 6000, height = 3000, res = 300)
  
  # Set the two plots to be side by side
  par(mfrow = c(1, 2), mar = c(2, 1.5, 2, 1.5))
  
  # Plot the TPrate plot first
  y = unique(TPrate$nruns)
  x = unique(TPrate$rank)
  z = c()
  for(i in y){
    z = cbind(z, TPrate$TP[TPrate$nruns == i])
  }
  persp3D(x, y, z, cex.lab = 2, col = newCol(), clim = c(0.0, 1.0), 
          ticktype="detailed", xlab="rank", ylab="nruns", zlab="TP rate")
  
  # Plot the FPrate second, reuse x and y values
  x = unique(FPrate$nruns)
  y = unique(FPrate$rank)
  z = c()
  for(i in y){
    z = cbind(z, FPrate$FP[FPrate$rank == i])
  }
  persp3D(x, y, z, cex.lab = 2, clim = c(0.0, 1.0), ticktype="detailed", 
          xlab="nruns", ylab="rank", zlab="FP rate")
  
  # Turn off the png device
  dev.off()
}