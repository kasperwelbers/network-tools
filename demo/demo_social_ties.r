d = data.frame(conversation=c(1,1,1,1,1,1,2,2,2,2),
               author=c('Alice','Bob','Alice','Charlie','Bob','Bob','Alice','Bob','Alice','Bob'),
               order.nr=c(1,2,3,4,5,6,1,2,3,4))
d

g = adjacency(list(author=d$author), d$conversation, as.graph=T) # In how many conversations did author.X and author.Y communicate?
plot(g, edge.label=E(g)$weight, vertex.size=40, vertex.label=as.character(V(g)$author))
g = adjacency(list(author=d$author), d$conversation, measure='overlap_jacard', as.graph=T) # In what percentage of articles in which author.X communicated did author.Y communicate as well?
plot(g, edge.label=E(g)$weight, vertex.size=40, vertex.label=as.character(V(g)$author))
g = adjacency(list(author=d$author), d$conversation, measure='cosine', as.graph=T) # what is the cosine similarity of authors based on their participation in conversations (takes number of times author participates within the conversation into account)
plot(g, edge.label=round(E(g)$weight,2), vertex.size=40, vertex.label=as.character(V(g)$author))


g = windowed.adjacency(list(authors=d$author), d$order.nr, d$conversation, window.size=2, as.graph=T, direction='up') # how many times did author.X communicate directly after author.Y? 
plot(g, edge.label=E(g)$weight, vertex.size=35)
g = windowed.adjacency(list(authors=d$author), d$order.nr, d$conversation, window.size=3, as.graph=T, direction='up') # larger window size 
plot(g, edge.label=E(g)$weight, vertex.size=35)
g = windowed.adjacency(list(authors=d$author), d$order.nr, d$conversation, window.size=3, as.graph=T) # undirected 
plot(g, edge.label=E(g)$weight, vertex.size=35)
plot(g, edge.label=E(g)$average, vertex.size=35) # using average values 
g = windowed.adjacency(list(authors=d$author), d$order.nr, d$conversation, window.size=3, as.graph=T, count.once=T) # counting unique authors only once
plot(g, edge.label=E(g)$average, vertex.size=35) # using average values, now representing percentage of times author.X communicating within a distance of 2 messages from author.Y
