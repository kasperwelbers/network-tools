library(networktools)

load('demo/parl_proceedings_ordered_words.rdata')

## fun-time in progress: using windowed adjacency to look at co-occurence of words in a given word distance. 
## In this specific demo we look at consequtive POS tags

## In what percentage of the occurence of a POS tag did it occur directly after the other POS tag?
g = windowed.adjacency(list(lemma=tokens$pos1), tokens$id, list(aid=tokens$aid, sentence=tokens$sentence), window.size=2, as.graph=T, count.once=T, direction='down')

V(g)$label = as.character(V(g)$lemma)
V(g)$size = sqrt(V(g)$n)
V(g)$size=0
V(g)$label.cex=2
E(g)$arrow.size = 0.5
V(g)$label.color='black'
g$layout = layout.fruchterman.reingold
E(g)$label = round(E(g)$average.YX,2)

graph.plot(g, edge.weight=E(g)$average.YX, min.edge=0.2)
# In 80% of the times M occured, it occured directly after D

### conditional probability
g = windowed.adjacency(list(lemma=d$author), d$order.nr, d$conversation, window.size=2, as.graph=T, count.once=T, direction='down') 

g = windowed.adjacency(list(lemma=tokens$pos1), tokens$id, list(aid=tokens$aid, sentence=tokens$sentence), window.size=2, as.graph=T, count.once=T, direction='down')

V(g)$label = as.character(V(g)$lemma)
V(g)$size = sqrt(V(g)$n)
V(g)$size=0
V(g)$label.cex=2
E(g)$arrow.size = 0.5
V(g)$label.color='black'
g$layout = layout.fruchterman.reingold
E(g)$label = round(E(g)$average.XY,2)

graph.plot(g, edge.weight=E(g)$average.XY, min.edge=0.2)
# In 80% of the times Q occured, N occured right after


# to what extent can communities be identified based on a comparison of a POS network structure?
# the difference between:
# We must destroy the Sith
# Destroy the Sith we must
