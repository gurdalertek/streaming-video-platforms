
#https://journal.r-project.org/archive/2017/RJ-2017-047/RJ-2017-047.pdf

#install.packages("rlang")
#library(rlang)

#library(readr)
#amazon= read_csv("Amazon-Assoc Mining.txt", col_names = FALSE)

# read genres data for amazon
amazon=read.csv("C:/Users/maith/OneDrive/Desktop/Graph analytics/graph-data/BUTTERFLIES/BUTTERFLIES/Amazon-Assoc Mining.txt", quote="\"", comment.char="")
View(amazon)

# read data rows as transactions
library(arules)
amazon=read.transactions("Amazon-Assoc Mining.txt", sep = " ")
amazon
summary(amazon)
#This result is showing the summary of the "amazon" dataset. The dataset contains 10871 rows (transactions) and 19 columns (items/genres) with a density of 0.1208285, which means that there are a lot of zeros in the dataset.
#The most frequent items are drama (appearing in 5511 transactions), followed by comedy (appearing in 3349 transactions), thriller (appearing in 2506 transactions), action (appearing in 2062 transactions), romance (appearing in 1860 transactions), and others (appearing in 9669 transactions).
#The distribution of the length of the transactions shows that most transactions contain 1-3 items. The minimum length is 0 (which means there are some empty transactions), and the maximum length is 9.
#The summary also includes some examples of the extended item information, showing the labels for the first three items/genres.

inspect(amazon[1:3])
itemFrequency(amazon[,1:19])
itemFrequencyPlot(amazon)
itemFrequencyPlot(amazon, topN=19)
itemFrequencyPlot(amazon, topN=5)

#(most freqent ganra) drama has the highest support around 50%,comedy comes in second with 30%, thriler 23%.
#lowest is reality than sport than war .

# run apriori algorithm
#m1=apriori(amazon, parameter = list(support=0.007, confidence=0.25, minlen=2))
m1=apriori(amazon, parameter = list(support=0, confidence=0, minlen=1, maxlen=2,maxtime=0))
m1
summary(m1)
inspect(m1[1:3])

#the first rule states that the presence of "sport" in a transaction is associated with the presence of "documentation" with a support value of 0.008, a confidence value of 0.333, a coverage value of 0.025, a lift value of 2.817, and a count value of 92.
#Similarly, the second rule indicates that the presence of "sport" is associated with the presence of "drama" with a higher confidence value of 0.54, but a lower lift value of 1.06.
#The third rule shows that the presence of "war" is associated with the presence of "history" with a high lift value of 8.89, indicating a strong association between the two items.

#top rules by liftand other metrics
inspect(sort(m1, by="lift")[1:5])
inspect(sort(m1, by="lift")[6:25])
#[7]  {action, horror}     => {scifi} lift of 6.870721, since the mean of lift is 2.5756 , this consider high.
inspect(sort(m1, by="lift")[26:35])

inspect(sort(m1, by="support")[1:5])
inspect(sort(m1, by="support")[1:66])
inspect(sort(m1, by="confidence")[1:5])
inspect(sort(m1, by="confidence")[1:66])

# visualizing rules as graphs
install.packages("arulesViz")
library(arulesViz)
library(vctrs)

plot(m1)
plot(m1, engine = "interactive", jitter = 0 )
plot(m1, engine = "htmlwidget")
plot(m1, method = "matrix", engine = "htmlwidget")
plot(m1, method = "grouped matrix", engine = "interactive")
plot(m1, method = "graph", engine = "htmlwidget", max= 200)# this is the network graph

g1=apriori(amazon, parameter = list(support=0, confidence=0, minlen=1, maxlen=2,maxtime=0))
plot(g1, method = "graph", engine = "interactive")
plot(g1, method = "graph", engine = "interactive",max= 200)
###############################################################
library(ggplot2)
plot(g1, method = "graph", asEdges = TRUE, limit = 30)
plot(g1, method = "graph", asEdges = TRUE, circular = FALSE, limit = 30)

#association rules are saved as graphml file and saved to csv 
saveAsGraph(m1, file = "amazon-asso-rules.graphml")
write.csv(inspect(m1), "apriori_results.csv", row.names = FALSE)#export to csv

###############################################################################################################
#this time, ggplot is used to visualize the association rules

library(ggplot2)

plot(m1, method = "graph", engine = "htmlwidget",max= 500,(aes_string(size = support(m1))))
plot(m1, method = "graph", engine = "htmlwidget",  nodeSize = 2*"support")

g1=apriori(amazon, parameter = list(support=0, confidence=0, minlen=1, maxlen=2,maxtime=0))

graph <- as(g1, "graph")

plot(g1, method = "graph", engine = "interactive")
plot(g1, method = "graph", engine = "interactive",max= 500)

plot(g1, method = "graph", engine = "htmlwidget", max = 500, 
     vertex.size = V(graph)$support/max(V(graph)$support) * 20)



##########centrality measures############################################

g1=apriori(amazon, parameter = list(support=0, confidence=0, minlen=1, maxlen=2,maxtime=0))
plot(g1, method = "graph", engine = "htmlwidget",max= 200)
graph <- as(g1, "graph")

# Calculate degree centrality
dc <- degree(graph)
dc
mean(dc)
# Calculate betweenness centrality
bc <- betweenness(graph)
bc
# Calculate closeness centrality
cc= closeness(graph)
cc

# Set node size based on degree centrality
#V(graph)$size <- dc
# Plot the graph
#plot(graph, vertex.label = V(graph)$name, engine = "htmlwidget")
#plot(graph, vertex.label = V(graph)$name, engine = "interactive")

#################graphs #############################

#Degree Centrality
plot(graph, vertex.label.cex = .6, vertex.label.color = "black", vertex.size = V(graph)$degree, vertex.label.cex = .2) # sized by degree

#Betweenness Centrality
V(graph)$betweenness <- betweenness(graph, directed = F) # assignment

plot(graph, 
     vertex.label.cex = .6, 
     vertex.label.color = "black", 
     vertex.size = V(graph)$betweenness) # sized by betweenness

plot(graph,
     vertex.label.cex = .6, 
     vertex.label.color = "black", 
     vertex.size = V(graph)$betweenness/max(V(graph)$betweenness) * 20)

#Closeness Centrality
V(graph)$closeness <- closeness(graph)
plot(graph,vertex.label.cex = .6, vertex.label.color = "black", vertex.size = V(graph)$closeness/max(V(graph)$closeness) * 20)


###############################################################
# graph where rules are shown as edged, rather than nodes

g2=apriori(amazon, parameter = list(support=0.001, confidence=0.20, minlen=1, maxlen=3,maxtime=0))
library(ggplot2)
plot(g1, method = "graph", asEdges = TRUE, limit = 30)
plot(g1, method = "graph", asEdges = TRUE, circular = FALSE, limit = 30, max.overlaps= 100)
plot(g2, method = "graph", asEdges = TRUE, circular = FALSE, limit = 30, max= 100, (aes_string(size = "support", color = "confidence ")))

     
plot(g1, method = "graph",
     control = list(
       edges = ggraph::geom_edge_link(
         end_cap = ggraph::circle(4, "mm"),
         start_cap = ggraph::circle(4, "mm"),
         color = "black",
         arrow = arrow(length = unit(2, "mm"), angle = 20, type = "closed"),
         alpha = .2
       ),
       nodes = ggraph::geom_node_point(aes_string(size = "support", color = "confidence ")),
       nodetext = ggraph::geom_node_label(aes_string(label = "label"), alpha = .8, repel = TRUE)
     ),
     limit = 10
) +
  scale_color_gradient(low = "yellow", high = "red") +
  scale_size(range = c(2, 10))

    