# Install Rfacebook package
# install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")

# Function
unlistWithNA <- function(lst, field){
  if (length(field)==1){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field]])))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst, '[[', field))
  }
  if (length(field)==2){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]])))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst, function(x) x[[field[1]]][[field[2]]]))
  }
  if (field[1]=="shares"){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]])))
    vect <- rep(0, length(lst))
    vect[notnulls] <- unlist(lapply(lst, function(x) x[[field[1]]][[field[2]]]))
  }
  if (length(field)==3){
    notnulls <- unlist(lapply(lst, function(x)
      tryCatch(!is.null(x[[field[1]]][[field[2]]][[field[3]]]),
               error=function(e) FALSE)))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[field[3]]]))
  }
  if (length(field)==4 & field[1]=="to"){
    notnulls <- unlist(lapply(lst, function(x)
      tryCatch(!is.null(x[[field[1]]][[field[2]]][[as.numeric(field[3])]][[field[4]]]),
               error=function(e) FALSE)))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[as.numeric(field[3])]][[field[4]]]))
  }
  if (field[1] %in% c("comments", "likes") & !is.na(field[2])){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]][[field[3]]])))
    vect <- rep(0, length(lst))
    vect[notnulls] <- unlist(lapply(lst, function(x) x[[field[1]]][[field[2]]][[field[3]]]))
  }
  return(vect)
}


# Load libraries
library("devtools")
library("Rfacebook")
library("RCurl")
library("httr")
library("rjson")

# Access Token "https://developers.facebook.com/tools/explorer" - Remember yourself that the token expires in 2 hours.
token <- "CAACEdEose0cBAPO2AOXpq4tllwcxkLxM6EqnrYLRs9NZAOXtBrO10fHdZAXEdZCFo4S9JYA3fLOYKciOYBvkZCknMDxng4wJNOP6efpO3TZB9RS8cp3ZAelXxZCCixCFdqD07b7UD1dH4m9J3XDCaR3Iaz8NrkZCAHqbxZB93ZBpLgthVyJQS68ZAK9trqwK0IOZCyHP3v9BHUsbkFMIlmqmQrhA95kY4ZCZBvjxMZD"


##########################
# Get groups -------------
##########################

# Group ID
groupID <- "379869335364146"

# Number of cases
n  <- 500

# Create ULR
url  <- paste0("https://graph.facebook.com/", groupID, "?fields=feed.limit(", n, "){from,message,created_time,type,link,comments.summary(true),likes.summary(true)}&locale=pt_BR&access_token=", token)

# Get json file using httr package
getUrl  <- GET(url)

# Convert JSON object into an R
content  <- fromJSON(rawToChar(getUrl$content))

# Loop list
ids  <- do.call("rbind", lapply(content$feed$data, "[[", 1))       # User ID and Name
msgs  <- do.call("rbind", lapply(content$feed$data, "[[", 2))      # Message
time  <- do.call("rbind", lapply(content$feed$data, "[[", 3))      # Time Created

# Create DataFrame
df  <- data.frame(ids = ids, msgs = msgs, time = time, stringsAsFactors = FALSE)

# Function which does it all

getGroup <- function(groupID, n, token) {
  
  url  <- paste0("https://graph.facebook.com/", groupID, "?fields=feed.limit(", n, "){from,message,created_time,type,link,comments.summary(true),likes.summary(true)}&locale=pt_BR&access_token=", token)
  getUrl  <- GET(url)
  content  <- fromJSON(rawToChar(getUrl$content))
  
  ids  <- do.call("rbind", lapply(content$feed$data, "[[", 1))       # User ID and Name
  msgs  <- do.call("rbind", lapply(content$feed$data, "[[", 2))      # Message
  time  <- do.call("rbind", lapply(content$feed$data, "[[", 3))      # Time Created
  
  df  <- data.frame(ids = ids, msgs = msgs, time = time, stringsAsFactors = FALSE)
  
  df$ids.id <- unlist(df$ids.id)
  df$ids.name <- unlist(df$ids.name)
  
  
  return(df)
  
}

df <- getGroup("438253896268176", 1000, token)


# Save for Text Mining
write.csv(df, "group379869335364146.csv")



# My Bin ------

unlist(content$feed$data[[5]]$from$id) # User ID
unlist(content$feed$data[[5]]$from$name) # User Name
unlist(content$feed$data[[5]]$message) # Message
unlist(content$feed$data[[5]]$created_time) # Time created
unlist(content$feed$data[[5]]$type) # Type: photo or status
unlist(content$feed$data[[5]]$likes$summary$total_count) # Total Likes
unlist(content$feed$data[[5]]$comments$summary$total_count) # Total Comments

is.null(content$feed$data[[1]][[8]][[3]][[2]])



# Testar se a lista é nula

unlist(lapply(content$feed$data, function (x) !is.null(x[[8]][[3]][[2]])))



  
!is.null(content$feed$data[[5]]$comments$summary$total_count)

unlist(lapply(content$feed$data, function(x) !is.null(x[[]])    ))






unlist(lapply(content$feed$data, function(x) !is.null(x[[1]])))


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


# Other analyses ---- 


# Reach my account
me <- getUsers("me", token = token)

# List my Friends
myFriends  <- getFriends(token, simplify = TRUE)

# Analyse my friends data
# API has limited users number
myFriendsList1 <- getUsers(myFriends$id[1:250], token, private_info = TRUE)
myFriendsList2 <- getUsers(myFriends$id[251:560], token, private_info = TRUE)

# Combine My Friends List
myFriendsList  <- rbind(myFriendsList1, myFriendsList2)

getPage("vivasemtabaco", token, n = 100, feed = FALSE)
cigarro  <- searchFacebook("dilma", token, n = 100, since = NULL, until = NULL)
names(cigarro)
head(cigarro$message)
