### Replicate analysis script ###

# Load functions
source("analysis_functions.R")

# Check the change point and network estimation outputs
checkIdentical("fmrioutput")
checkIdentical("AAL.net")
checkIdentical("clust.net")
checkIdentical("SP500out")
checkIdentical("SP500net")