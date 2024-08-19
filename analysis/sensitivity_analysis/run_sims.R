# Load libraries and scripts
library(fabisearch)
source("../GenerateFunctions.R")
source("../Sim2.R")

# Define the nruns to try
nruns = c(2, 3, 5, 10, 15, 20, 25, 50, 100, 200)

# Initialize the output variable
output = list()

# Loop through the different values of runs
for(i in nruns){
  # Print the number of runs
  print(paste0("nruns: ", i))
  
  # Loop through 20 iterations of the simulation
  allcpts = c()
  for(j in 1:20){
    # Print the current iteration
    print(j)
    
    # Set the seed
    set.seed(j*123)
    
    # Generate the simulation
    Y = sim2(j) + 100
    
    # Run the change point detection method
    cpts = detect.cps(Y, mindist = 50, nruns = i, rank = rank, alpha = 0.05)
    
    # Save the significant change points
    cpts = cpts$change_points$T[cpts$change_points$stat_test]
    
    # Append this to the allcpts vector
    allcpts = c(allcpts, cpts)
  }
  
  # Save this in the output variable
  output[[paste0("nruns", i)]] = allcpts
  
  # Save as .rda file
  save(output, file = paste0("rank", rank, ".rda"))
}