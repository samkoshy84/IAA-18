setwd(('/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Social Network Analysis 2017/SLACK/'))
load("SlackReplies.RData")
install.packages(c("igraph","network","networkD3"))
library(igraph)
library(networkD3)
#################################################################################################
#################################################################################################
#################################################################################################
##############################           igraph PACKAGE            ##############################   
#################################################################################################
#################################################################################################
#################################################################################################

################################################################################################  
#' First two columns of the Network data frame should be the source node and target node.
#' (in that order for a directed network) Any other columns in the network data frame
#' are assumed to be edge attributes. The first column of the users data frame should match 
#' the vertex id's from the source and target columns. All other columns in the user data 
#' frame are assumed to be vertex attributes. 
################################################################################################  
slack = graph_from_data_frame(SlackReplies, directed = TRUE, vertices = users)
# List Vertices
V(slack)
# Vertex Attributes
vertex_attr(slack)
# List Edges
E(slack)
# Edge Attr
edge_attr(slack)
################################################################################################  
#' Base plots are TERRIBLE. You can do better using the provided .pdf (some examples here),
#' or by using networkD3 (toward the end of this tutorial)
################################################################################################  
plot(slack)
slack = delete.vertices(slack, degree(slack)==0)
# Some simple controls you can mess with:
plot(slack, edge.arrow.size = .3, vertex.label=NA,vertex.size=10,
     vertex.color='gray',edge.color='blue')
# Layout usually makes the biggest difference. Many force-directed graph layouts to choose from.
# Fruchterman-Reingold semi-popular.
plot(slack, edge.arrow.size = .3, vertex.label=NA,vertex.size=3,
     vertex.color='lightblue',layout=layout.fruchterman.reingold)
# Help lists other potential layouts
?layout
# You can save the layout as an object and then test out a few and compare.
l = layout.sphere(slack) 
l2 = layout_nicely(slack)
l3 = layout.mds(slack)
l4 = layout_with_gem(slack)
#' par(mfrow=c(2,2),mar=c(1,1,1,1)) Tells the graphics window to sequentially 
#' fill out a 2x2 grid with margins of 1 unit on each side. Must reset these 
#' options with dev.off() when done!
par(mfrow=c(2,2),mar=c(1,1,1,1)) 
plot(slack, edge.arrow.size = .3, vertex.label=NA,vertex.size=4,
     vertex.color='lightblue', layout=l,main="Sphere")
plot(slack, edge.arrow.size = .3, vertex.label=NA,vertex.size=4,
     vertex.color='lightblue', layout=l2,main="Nicely")
plot(slack, edge.arrow.size = .3, vertex.label=NA,vertex.size=4,
     vertex.color='lightblue', layout=l3,main="MDS")
plot(slack, edge.arrow.size = .3, vertex.label=NA,vertex.size=4,
     vertex.color='lightblue', layout=l3,main="GEM")

dev.off() #resets the graphic window options.

#################################################################################################
#################################################################################################
#################################################################################################
###########################           networkD3 PACKAGE            ##############################   
#################################################################################################
#################################################################################################
#################################################################################################

#' This package insists that the label names (indices) of your nodes start from zero. That is 
#' something to be
# aware of when moving between R and python as well!!
# If they don't start from zero the graph just won't render
# you won't even get a warning message!! 

# To use this package, you need a data frame containing
# the edge list and a data frame containing the node data.

# Create numeric IDs from 0 to 106 for each of the 107 students in class
users$ID = 0:(dim(users)[1]-1)
# This edge ID just helps us in the SQL query below
SlackReplies$edgeID = 1:dim(SlackReplies)[1]

library(sqldf)

#data frame with edge data
slackNet = sqldf('select first.source as source, second.target as target
                from
                  (select a.edgeID, b.ID as source 
                    from SlackReplies as a 
                      left join users as b 
                      on a.source = b.userID) as first
                left join 
                 (select c.edgeID, d.ID as target
                    from SlackReplies as c
                      left join users as d 
                      on c.target = d.userID) as second
                on first.edgeID=second.edgeID
                 ')

forceNetwork(Links=slackNet, Nodes=users, Source = "source",
             Target = "target", NodeID="ID", Group="cohort",
             fontSize=12, opacity = 0.9, zoom=T, legend=T)

# You can use the "Charge" of the Algorithm to change the spread of the layout
# A negative charge creates a repulsive force between the nodes, a positive
# charge creates an attractive force.
output = forceNetwork(Links=slackNet, Nodes=users, Source = "source",
             Target = "target", NodeID="name", Group="cohort",
             charge=-100,  fontSize=12, opacity = 0.9, zoom=T, legend=T)

saveNetwork(output, file="SlackSubset.html", selfcontained = TRUE)
