# Load libraries
library("devtools")

# Install Rfacebook package
install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")
library("Rfacebook")

# Access Token "https://developers.facebook.com/tools/explorer"
token <- "CAACEdEose0cBACj22bZAjfowVZC0ZCuGLqK9i7Rafvpz4TVTQyFTycPbuzlYDo40XZCCSbGPeMiHOzr02iC2fGjaxHB6WSOZAygorqRT6dl8hbZAe1JZAdOk5GaUZC8aBnEN4ozZAwtBnfSKbF63NwVPuzQ1b356UYXgOSNMFpEaNthUSuhxWKPAJG0fgEWAgqyYAGYyZBtBcSMKyA6pAKS6NRq6uFC0WSKR4ZD"

# Reach my account
me <- getUsers("me", token = token)

# List my Friends
myFriends  <- getFriends(token, simplify = TRUE)

# Analyse my friends data
myFriendsList <- getUsers(myFriends$id[1:250], token, private_info = TRUE)

# Get network ----
mat <- getNetwork(token, format="adj.matrix")

network <- graph.adjacency(mat, mode="undirected") ## igraph object
fc <- fastgreedy.community(network) ## communities / clusters
set.seed(123)
l <- layout.fruchterman.reingold(network, niter=1000, coolexp=0.5) ## layout

# checking who is in each cluster
cl <- data.frame(name = fc$names, cluster = fc$membership, stringsAsFactors=F)
cl <- cl[order(cl$cluster),]
cl[cl$cluster==5,]


# preparing data for plot
d <- data.frame(l); names(d) <- c("x", "y")
d$cluster <- factor(fc$membership)
# plot with only nodes, colored by cluster
p <- ggplot(d, aes(x=x, y=y, color=cluster))
pq <- p + geom_point()


## too many clusters! let's pick just those with 10 friends or more
large.clusters <- which(table(fc$membership)>=10)
fc$membership[fc$membership %in% large.clusters == FALSE] <- "Others"
d$cluster <- factor(fc$membership)

# plot with only nodes, colored by cluster
p <- ggplot(d, aes(x=x, y=y, color=cluster))
pq <- p + geom_point()
pq

## let's simplify even further by keeping only nodes in giant component
cl <- clusters(network)
gc <- which(cl$membership == 1)
mat <- mat[gc, gc]
network <- graph.adjacency(mat, mode="undirected") ## igraph object
fc <- fastgreedy.community(network) ## communities / clusters
set.seed(123)
l <- layout.fruchterman.reingold(network, niter=1000, coolexp=0.5) ## layout
d <- data.frame(l); names(d) <- c("x", "y")
d$cluster <- factor(fc$membership)
p <- ggplot(d, aes(x=x, y=y, color=cluster))
pq <- p + geom_point()
pq

## now let's add the edges
edgelist <- get.edgelist(network, names=FALSE)
edges <- data.frame(d[edgelist[,1],c("x", "y")], d[edgelist[,2],c("x", "y")])
names(edges) <- c("x1", "y1", "x2", "y2")
pq <- pq + geom_segment(
  aes(x=x1, y=y1, xend=x2, yend=y2),
  data=edges, size=0.25, color="grey", alpha=1/3)
pq

## (note that the order matters!)
p <- ggplot(d, aes(x=x, y=y, color=cluster))
pq <- p + geom_segment(
  aes(x=x1, y=y1, xend=x2, yend=y2),
  data=edges, size=0.25, color="grey", alpha=1/3) +
  geom_point()
pq

## change a few of the theme options to make it look better
pq <- pq + theme(
  # dark background
  panel.background = element_rect(fill = "black"),
  plot.background = element_rect(fill="black"),
  # removing axis lines and ticks
  axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
  axis.title = element_blank(), panel.border = element_blank(),
  panel.grid.major = element_blank(), panel.grid.minor = element_blank()
)
pq
## now let's customize the legend
pq <- pq + theme(
  # dark background to legend
  legend.background = element_rect(colour = "white", fill = "black"),
  legend.key = element_rect(fill = "black", colour = F),
  # text and border in white
  legend.title = element_text(color="white"),
  legend.text = element_text(color="white")
)
pq

## let's also improve the labels by identifying most central node within
## each community (using degree as measure of centrality) and adding labels
## based on what we learn from that
d$degree <- degree(network)
which.max(degree(network)) ## who do I have more friends in common with?
central.nodes <- lapply(communities(fc), function(x) x[which.max(d$degree[x])])
central.names <- fc$names[unlist(central.nodes)] ## names of central nodes
## within each cluster

labels <- c("UFJF", "Amigos e Familiares", "Curso de Verão", "CES", "REBEC", "NYC", "Outros")
pq <- pq + scale_color_discrete(labels=labels)
pq

## we can also add the labels to the plot
d$label <- NA
d$label[unlist(central.nodes)] <- labels
pq <- pq + geom_text(aes(x=x, y=y, label=label), data=d, color="white", size=3)
pq ## let's forget about it for now...
## let's put it all together, with some final touches on how points look
p <- ggplot(d, aes(x=x, y=y, color=cluster))
pq <- p + geom_segment(
  aes(x=x1, y=y1, xend=x2, yend=y2),
  data=edges, size=0.25, color="white", alpha=1/3) +
  ## note that here I add a border to the points
  geom_point(color="grey20", aes(fill=cluster), shape=21, size=2) +
  scale_fill_discrete(labels=labels) +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill="black"),
    axis.line = element_blank(), axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(), panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(colour = F, fill = "black"),
    legend.key = element_rect(fill = "black", colour = F),
    legend.title = element_text(color="white"),
    legend.text = element_text(color="white")
  ) +
  ## changing size of points in legend
  guides(fill = guide_legend(override.aes = list(size=5)))
pq


# Other analyses

getPage("vivasemtabaco", token, n = 100, feed = FALSE)
cigarro  <- searchFacebook("dilma", token, n = 100, since = NULL, until = NULL)
names(cigarro)
head(cigarro$message)
