d = data.frame(conversation=c(1,1,1,1,1,1,2,2,2,2),
               author=c('Alice','Bob','Alice','Charlie','Bob','Bob','Alice','Bob','Alice','Bob'),
               order.nr=c(1,2,3,4,5,6,1,2,3,4))
d

g = author.coincidence.graph(d$conversation, d$author) # In how many conversations did author.X and author.Y communicate?
plot(g, edge.label=E(g)$weight, vertex.size=V(g)$n.conversations*25) 
# Or use alternative similarity measures
g = author.coincidence.graph(d$conversation, d$author, 'overlap_jacard') # Similar to default (coincidence_count) but with direction (by dividing coincidence by number of conversations author participated in)
plot(g, edge.label=E(g)$weight, vertex.size=V(g)$n.messages*15)
g = author.coincidence.graph(d$conversation, d$author, 'cosine') # Cosine can be used to also take into account how many times each author participated within conversations
plot(g, edge.label=E(g)$weight, vertex.size=V(g)$n.messages*15)

g = first.author.graph(d$conversation, d$author, d$order.nr) # in how many conversations initiated by author.Y did author.X communicate?
plot(g, edge.label=E(g)$weight, vertex.size=V(g)$n.conversations*25)
g = first.author.graph(d$conversation, d$author, d$order.nr, once.per.conversation=FALSE) # how many times did author.X communicate in a conversation initiated by author.Y?
plot(g, edge.label=E(g)$weight, vertex.size=V(g)$n.messages*15)
g = first.author.graph(d$conversation, d$author, d$order.nr, direction='undirected') # drop direction author.X and author.Y
plot(g, edge.label=E(g)$weight, vertex.size=V(g)$n.conversations*25)
g = first.author.graph(d$conversation, d$author, d$order.nr, direction='directed.down') # switch direction
plot(g, edge.label=E(g)$weight, vertex.size=V(g)$n.conversations*25)

g = previous.authors.graph(d$conversation, d$author, d$order.nr, lookback=1) # how many times did author.X communicate directly after author.Y? 
plot(g, edge.label=E(g)$weight, vertex.size=V(g)$n.conversations*25)
g = previous.authors.graph(d$conversation, d$author, d$order.nr, lookback=1, once.per.conversation=TRUE) # in how many conversations did author.X communicate directly after author.Y?
plot(g, edge.label=E(g)$weight, vertex.size=V(g)$n.conversations*25)
g = previous.authors.graph(d$conversation, d$author, d$order.nr, lookback=2) # how many times did author.X communicate within two messages after author.Y? 
plot(g, edge.label=E(g)$weight, vertex.size=V(g)$n.conversations*25)

?write.graph

