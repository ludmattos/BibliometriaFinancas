library(dplyr)
library(bibliometrix)

setwd("C:/Users/ludmi/Desktop/")

biblioshiny()

file <- "C:/Users/ludmi/Desktop/cloud.txt"
base <- convert2df(file = file, dbsource = "isi", format = "plaintext")

results <- biblioAnalysis(base, sep = ";")

options(width=100)
S <- summary(object = results, pause = FALSE)
plot(x = results, k = 10, pause = FALSE)

DF <- dominance(results, k = 10)
DF

authors=gsub(","," ",names(results$Authors)[1:10])
indices <- Hindex(base, field = "author", elements=authors, sep = ";", years = 50)
indices$H

# Observed distribution
Observed=L$AuthorProd[,3]

# Keywords co-occurrences
NetMatrix <- biblioNetwork(base, analysis = "co-occurrences", network = "keywords", sep = ";")
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 20, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 6,labelsize=0.9)
# Country collaboration
M <- metaTagExtraction(base, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=1.2,cluster="none")
# Co-citation network
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 20, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=0.9,edgesize = 5)
# Historical Direct Citation Network
options(width=130)
histResults <- histNetwork(M, min.citations = 2, sep = ";")
net <- histPlot(histResults, n=20, size = 10, labelsize=5)

# Conceptual Structure using keywords (method="MCA")
CS <- conceptualStructure(M,field="ID", method="MCA", minDegree=4, clust=4 ,k.max=8, stemming=FALSE, labelsize=10, documents=10)
# Create a historical citation network

histResults <- histNetwork(M, sep = ";")

# Plot a historical co-citation network

net <- histPlot(histResults, size = 10)
