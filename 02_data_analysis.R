Script 2: Data analysis

############################################################
# 1. Summary Statistics
############################################################
# Perform bibliometric performance analysis
results <- biblioAnalysis(
M,
sep = ";"
)
# Generate summary statistics for top-ranked indicators
S <- summary(
object = results,
k = 10,
pause = FALSE
)

############################################################
# 2. Scientific Production Basic Plot
############################################################
# Generate scientific production plot
plot(
x = results,
k = 10,
pause = FALSE
)

############################################################
# 3. Bibliometric Laws
############################################################
# Compute Lotka’s Law for author productivity distribution and generate plot
L <- lotka(M)
print(L)
# Compute Bradford’s Law for core journal distribution and generate plot
B <- bradford(M)
print(B)

############################################################
# 4. Multiple Correspondence Analysis (MCA)
############################################################
# Set seed for reproducibility of clustering results
set.seed(1234)
# Open PDF device to save MCA conceptual map
pdf("MCA.pdf",
width = 7,
height = 6)
# Perform Multiple Correspondence Analysis (MCA)
# using merged keyword field (DE + TI)
CS <- conceptualStructure(
M,
field = "KW_Merged",
method = "MCA",
minDegree = 30,
clust = 5,
stemming = TRUE,
k.max = 8,
documents = 0,
labelsize = 12,
graph = TRUE
)





############################################################
# 4.A Multiple Correspondence Analysis (MCA – Author Keywords)
############################################################
# Set seed for reproducibility
set.seed(1234)
# Exploratory MCA based on Author Keywords (DE)
# Used to assess conceptual stability before the merged model
CS <- conceptualStructure(
M,
field = "DE",
method = "MCA",
minDegree = 10,
clust = 5,
stemming = FALSE,
k.max = 8,
documents = 12,
labelsize = 10,
graph = TRUE
)

############################################################
# 4.B Multiple Correspondence Analysis (MCA – Title Terms)
############################################################
# Set seed for reproducibility
set.seed(1234)
# Exploratory MCA based on Title Keywords (TI)
# Used to assess conceptual stability before the merged model
CS <- conceptualStructure(
M,
field = "TI",
method = "MCA",
minDegree = 10,
clust = 5,
stemming = FALSE,
k.max = 8,
documents = 12,
labelsize = 10,
graph = TRUE
)

############################################################
# 4.C MCA Sensitivity Analysis (minDegree = 20)
############################################################
# Set seed for reproducibility
set.seed(1234)

# Exploratory MCA using a lower frequency threshold
# Used to assess robustness of factorial structure

CS_sensitivity <- conceptualStructure(
M,
field = "KW_Merged",
method = "MCA",
minDegree = 20,
clust = 5,
stemming = TRUE,
k.max = 8,
documents = 0,
labelsize = 12,
graph = TRUE
)





############################################################
# 5. Co-Citation Network Analysis
############################################################
# Load required packages
library(bibliometrix)
library(igraph)

############################################################
# 5.A Co-Citation Matrix Construction
############################################################
# Build reference co-citation matrix from bibliometrix object
NetMatrix_Cocit <- biblioNetwork(
M,
analysis = "co-citation",
network = "references",
sep = ";"
)
############################################################
# 5.B Selection of Core References
############################################################
# Estimate citation frequency and select the most influential references
ref_freq <- sort(rowSums(NetMatrix_Cocit), decreasing = TRUE)
# Select top 30 references
top_refs <- names(ref_freq)[1:30]
# Subset adjacency matrix
Net_top <- NetMatrix_Cocit[top_refs, top_refs]
############################################################
# 5.C Graph Construction
############################################################
# Convert adjacency matrix into a weighted undirected network
g_sub <- graph_from_adjacency_matrix(
Net_top,
mode = "undirected",
weighted = TRUE,
diag = FALSE
)
# Remove isolated vertices
g_sub <- delete_vertices(g_sub, degree(g_sub) == 0)

############################################################
# 5.D Community Detection
############################################################
# Detect clusters using the Louvain modularity optimization method
cl_weighted <- cluster_louvain(g_sub)
# Extract modularity value (Newman Q)
Q_weighted <- modularity(cl_weighted)
cat("Weighted modularity (Q):", Q_weighted, "\n")

############################################################
# 5.E Cluster Assignment
############################################################
# Assign community membership to each reference node
V(g_sub)$cluster <- membership(cl_weighted)

############################################################
# 5.F Centrality Indicators
############################################################
# Compute structural importance indicators for nodes
V(g_sub)$degree <- degree(g_sub)
V(g_sub)$strength <- strength(g_sub)
V(g_sub)$betweenness <- betweenness(g_sub, normalized = TRUE)





############################################################
# 5.G. Layout Definition
############################################################
# Compute network layout using Fruchterman-Reingold algorithm
set.seed(1234)
layout_fr <- layout_with_fr(
g_sub,
niter = 4000
)

############################################################
# 5.H Visual Parameters
############################################################
# Define node and edge graphical properties
V(g_sub)$size <- 8 + (V(g_sub)$degree / max(V(g_sub)$degree)) * 6
E(g_sub)$color <- adjustcolor("grey50", alpha.f = 0.30)
E(g_sub)$width <- 0.6
cluster_colors <- c("#4FA3D1", "#4FB286", "#C05746")
V(g_sub)$color <- cluster_colors[V(g_sub)$cluster]

############################################################
# 5.I Network Visualization Export
############################################################
# Export high-resolution network figure
png("Co_citation_network_final.png",
width = 7.1,
height = 6,
units = "in",
res = 600,
bg = "white")
plot(
g_sub,
layout = layout_fr,
vertex.label = V(g_sub)$name,
vertex.label.cex = 0.5,
vertex.label.dis = 2,
vertex.label.color = "#222222",
vertex.frame.color = "white",
vertex.frame.width = 1,
edge.width = E(g_sub)$width,
edge.color = E(g_sub)$color,
main = "Co-Citation Network"
)
dev.off()
############################################################
# 5.J Cluster Table Export
############################################################
# Export cluster membership and centrality indicators
cluster_table <- data.frame(
Reference = V(g_sub)$name,
Cluster = V(g_sub)$cluster,
Degree = V(g_sub)$degree,
Strength = V(g_sub)$strength,
Betweenness = V(g_sub)$betweenness
)
cluster_table <- cluster_table[order(cluster_table$Cluster), ]
write.csv(
cluster_table,
"Co_citation_clusters_table.csv",
row.names = FALSE
)
ecount(g_sub)
vcount(g_sub)
table(V(g_sub)$cluster)
cluster_table
############################################################
