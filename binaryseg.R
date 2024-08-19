# Load libraries
library(devtools)
devtools::load_all("fabisearch_TESTING")

# Run using the regulary binary segmentation
binsegout = detect.cps(sim2, "binseg", rank = 3, ncore = 8)
# Compare with binary search
binseaout = detect.cps(sim2, "binsea", rank = 3, ncore = 8)

# Compare the outputs
print(binsegout)
print(binseaout)