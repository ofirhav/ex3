library(igraph)
linkedinGraph <- read.graph(file = "LinkedinGraph.gml",format = "gml")
linkedinGraph$layout <- layout.fruchterman.reingold(linkedinGraph) 
plot(linkedinGraph,vertex.size=5,vertex.label.cex =0.7,asp=FALSE)
V(linkedinGraph)$Betweeness = betweenness(linkedinGraph)
V(linkedinGraph)$Closeness = closeness(linkedinGraph)
V(linkedinGraph)$EigenVector = evcent(linkedinGraph)$vector

V(linkedinGraph)$label[which(V(linkedinGraph)$Betweeness == max(betweenness(linkedinGraph)))]
V(linkedinGraph)$label[which(V(linkedinGraph)$Closeness == max(closeness(linkedinGraph)))]
V(linkedinGraph)$label[which(V(linkedinGraph)$EigenVector == max(evcent(linkedinGraph)$vector))]


linkedinGraph = simplify(linkedinGraph)

#girvan-newman
ebc <- edge.betweenness.community(linkedinGraph, directed=F)
# color the nodes according to their membership
mods <- sapply(0:ecount(linkedinGraph), function(i){
  g2 <- delete.edges(linkedinGraph, ebc$removed.edges[seq(length=i)])
  cl <- clusters(g2)$membership
  modularity(linkedinGraph,cl)
})
g2<-delete.edges(linkedinGraph, ebc$removed.edges[seq(length=which.max(mods)-1)])
V(linkedinGraph)$color=clusters(g2)$membership

# layout for the graph
linkedinGraph$layout <- layout.fruchterman.reingold
plot(linkedinGraph,vertex.size=5,vertex.label.cex =0.7,asp=FALSE)
plot(mods, pch=30)

#greedy optimization
wtc <- walktrap.community(linkedinGraph)
memb <- cutat(wtc, steps= which.max(wtc$modularity)-1)
plot(linkedinGraph, layout=layout.fruchterman.reingold, vertex.size=5, vertex.color=memb, asp=FALSE)
modularity(wtc)
