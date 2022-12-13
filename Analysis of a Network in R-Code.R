----------------# Social Network Analysis #----------------------

## Building the Network ------------------------------

# load packages for exploration
require(igraph)
library(tnet)

# set working directory
setwd("E:/Analysis of a Network in R")
# get working directory
getwd()

# read nodes and ties data into variables
#nodes<- read.csv("nodes.csv")
#ties<- read.csv("ties.csv")
#OR load data
data<-read.csv(file.choose(), header=T)

#build a network from data frames
#net<- graph_from_data_frame(d=ties, directed=FALSE, vertices = nodes)

#''' n<-graph(c(1,2,2,3,3,4,4,1),
#           directed=F,
#           n=7)
#n[]  #show matrix  '''

#extract two cols and create a dataframe
y<-data.frame(data$X1, data$X2)
#create a network graph
netG<-graph.data.frame(data,directed=F)


## Exploring the network ------------------------------

#Access vertices and edges:

#explore the set of nodes
V(netG) 
vcount(netG)               #print/display vertices/nodes
V(netG)$label<-V(netG)$name     #label the vertex
#explore the set of ties
E(netG)
ecount(netG)               #print number of ties

# add the name attribute "M Network" to the network and print it
netG$name<-"M Network"

# add node attribute id and print the node 'id' attribute 
V(netG)$id<-1:vcount(netG)

#Examine atributes
# print the tie/edge's 'weight' attribute
# E(netG)$weight

edge_attr(netG)
vertex_attr(netG)
graph_attr(netG)

# View the properties of network:
summary(netG)


## Network Measures --------------

# Diameter
diameter(netG, directed=F,weights = NA)

# Degree 
#degree<-degree(netG, v=V(netG), mode = 'all', normalized= F)
degree<-degree_w(data)

#V(netG)$degree<-degree(netG)

hist(degree, 
     col='yellow', 
     main='Histogram of Node Degree',
     ylab= 'Frequency',
     xlab='Degree of Nodes')

#Network Diagram only shows Connections
set.seed(222)
plot(netG,
     vertex.color= 'green',
     vertex.size=10,
     edge.color='red',
     edge.arrow.size=0.1,
     vertex.label.cex=0.8)

#weighted details are in this chart
plot(netG, 
     vertex.color= rainbow(52),
     vertex.size=V(netG)$degree*0.4,
     edge.arrow.size=0.1,
     layout = layout.fruchterman.reingold,)

# Edge Density
edge_density(netG, loops=F)
#ecount(netG)/vcount(netG)*(vcount(netG)-1)/2

# Reciprocity
reciprocity(netG)

# Closeness
#closeness(netG, mode ='all', weights=NA)
closeness_w(data)

# Betweennes
#betweenness(netG, directed=T, weights=NA)
betweenness_w(data)


## Visualization --------------

#use a layout'fruchterman.reingold'
layoutfr<layout.fruchterman.reingold(netG)
frlayout<-layout_with_fr(netG)

#select an optimized layout automatically
layoutauto<-layout.auto(netG)

#node/vertex options: color
V(netG)$color<-"maroon"
#edge options: color
E(netG)$color<-"tomato"

#plot graph
plot(netG, vertex.label=V(netG)$City,
     edge.width=E(netG)$weight, vertex.size=10,
     layout=layout_nicely, main=" Weighted Network Visualization",
     edge.arrow.size=2, edge.curved=0,
     vertex.color="green", vertex.frame.color="#555555",
     vertex.label.color="black", vertex.label.cex=1)

#label each vertex
#assign edge width acc to edge's weight value
#title the graph
#specific vertex color,frame color, label color

#Global Clustering
GC<-clustering_w(data, measure = c("bi", "am", "gm","ma","mi"))
