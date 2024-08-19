# Load package
library(fabisearch)

###########################################
###=== 4. Resting-state fMRI Example ===###
###########################################

### 4.1 Data summary and change point detection ###

# Load the data
data("gordfmri", package = "fabisearch")

# Inspect the data/summarize the first 4 time series
print(gordfmri[1:10,1:4])
summary(gordfmri[,1:4])

# Visualize first 4 time series
par(mfrow=c(4,1))
for(i in 1:4){
  plot(gordfmri[,i], type = "l", cex.lab = 1.5, cex.axis = 1.5,
    xlab = "Time", ylab = paste("Node", i))
}

# Run detect.cps()
set.seed(12345)
fmrioutput = detect.cps(gordfmri)
fmrioutput

# Find which change points to keep
finalcpt = fmrioutput$change_points[fmrioutput$change_points[,2]
  < 0.001, 1]
finalcpt

### 4.2 Estimating stationary networks ###
# Estimate stationary networks using the rank estimate
rankest = fmrioutput$rank
clust.net = est.net(gordfmri, lambda = rankest, rank = rankest,
                    changepoints = finalcpt)

# Plot heatmaps of the individual networks
n = 1
heatmap(clust.net[[n]], col = grey(c(0.97,0.1)), symm = TRUE,
        Colv = NA, Rowv = NA, labRow = NA, labCol = NA)

# Estimate stationary networks using different cutoff values
lambda = seq(0.01, 0.99, 0.01)
thresh.net = est.net(gordfmri, lambda = lambda, rank = rankest,
                     changepoints = finalcpt)

# Plot number of edges as a function of cutoff values
par(mfrow = (c(1,3)))
for(n in 1:3){
  edges = c()
  for(i in 1:length(lambda)){
    curr.net = thresh.net[[n]][[i]]
    edges = c(edges, sum(curr.net[upper.tri(curr.net)]))
  }
  plot(lambda, edges, cex.lab = 2, cex.axis = 2)
}
dev.off()

# Plot a heatmap of the individual networks
n = 1
heatmap(thresh.net[[n]][[40]], col = grey(c(0.97,0.1)), symm = TRUE,
        Colv = NA, Rowv = NA, labRow = NA, labCol = NA)

### 4.3 3-dimensional brain network visualization ###
# Network visualization
n = 1
net.3dplot(thresh.net[[n]][[50]])

# Plot only the "None" and "Visual" communities
communities = c("None", "Visual")
colors = c("#FFEB3B", "#673AB7")
n = 1
net.3dplot(thresh.net[[n]][[50]], ROIs = communities,
           colors = colors, labels = TRUE)

# Load the data
data("AALfmri", package = "fabisearch")

# Estimate stationary networks using different cutoff values
lambda = seq(0.01, 0.99, 0.01)
AAL.net = est.net(AALfmri[1:35,], lambda = lambda, rank = rankest)

# Plot number of edges as a function of cutoff values
edges = c()
for(i in 1:length(lambda)){
  curr.net = AAL.net[[i]]
  edges = c(edges, sum(curr.net[lower.tri(curr.net)]))
}
plot(lambda, edges, cex.lab = 1.8, cex.axis = 1.8)

# Plot a heatmap of the individual network, and the 3-dimensional plot
heatmap(AAL.net[[38]], col = grey(c(0.97,0.1)), symm = TRUE,
        Colv = NA, Rowv = NA, labRow = NA, labCol = NA)
net.3dplot(AAL.net[[38]], coordROIs = AALatlas)

# Network visualization of only first 30 nodes
nodes = 1:30
net.3dplot(AAL.net[[38]], coordROIs = AALatlas, 
           labels = TRUE, ROIs = nodes)

#######################################
###=== 5. Financial Data Example ===###
#######################################

### 5.1 Change point detection and data summary ###

# Load the data
data("logSP500", package = "fabisearch")

# Inspect the data/summarize TSLA, JNJ, JPM, and DIS
companies = c("TSLA", "JNJ", "JPM", "DIS")
columns = colnames(logSP500) %in% companies
print(logSP500[1:10, columns])
summary(logSP500[, columns])

# Visualize TSLA, JNJ, JPM, and DIS
par(mfrow=c(4,1))
for(i in 1:4){
  plot(logSP500[,colnames(logSP500) == companies[i]],
       type = "l", cex.lab = 1.5, cex.axis = 1.5, xlab = "Day",
       ylab = companies[i], ylim = c(80, 120))
}

# Run detect.cps()
set.seed(54321)
rank = opt.rank(logSP500, nruns = 100)
SP500out = detect.cps(logSP500, alpha = 0.05, mindist = 100, 
                      rank = rank, nreps = 150)
SP500out

# Final change points
finalcpt = SP500out$change_points$T[SP500out$change_points$stat_test]
finalcpt

# Corresponding dates
rownames(logSP500[finalcpt,])

# Load library and dataset
library(quantmod)
SP500 = getSymbols("^GSPC", env = NULL, auto.asign = FALSE,
                   from = "2018-01-01", to = "2021-03-31")

### 5.2 Estimating stationary networks and visualization ###

# Narrow down selection of nodes to top 15 largest companies, ane estimate these networks
tickers = c("AAPL", "MSFT", "AMZN", "GOOG", "FB", "TSLA", "BRK-B",
             "JPM", "JNJ", "NVDA", "UNH", "V", "HD", "PG", "DIS")
SP500net = est.net(logSP500, rank = rank, lambda = rank, nruns = 50, changepoints = finalcpt)

# Plot the resulting stationary segments
library(igraph)
top.15 = colnames(logSP500) %in% tickers
par(mfrow=c(3,2))
for(i in 1:length(SP500net)){
    G = graph_from_adjacency_matrix(SP500net[[i]][top.15,top.15],
         mode = "undirected", diag = FALSE)
    V(G)$color = palette()[components(G)$membership]
    plot(G, layout=layout_with_kk, vertex.size=20,
         vertex.color="white", vertex.frame.color = "white",
         vertex.label.color = V(G)$color, asp = 0.5)
    Sys.sleep(1)
}

############################################
############# Save Final Image #############
############################################
save.image(file = "fabisearch_examples.RData")