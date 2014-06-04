library(networktools)

## Speech acts in Dutch parliamentary proceedings by two parties (VVD and CDA) from 2003 till 2005 (samples of 100 proceedings per year)

load('demo/parliamentary_proceedings.rdata')

dim(document.topic.matrix) # A sparse matrix of documents X topics (in this case a sample of the results from a topic model (LDA) over 73300 speech acts, with 200 topics)
dim(meta) # meta data for the documents in document.topic.matrix. 

g = similarity.graph(document.topic.matrix, 
                             vertex.grouping.vars=list(party=meta$party, 
                                                       year=format(meta$date, '%Y')), 
                             similarity.measure='correlation')

list.vertex.attributes(g)
list.edge.attributes(g)

g = graph.color.vertices(g, V(g)$party) # color vertices by party
V(g)$label = as.character(V(g)$year) # use year as vertex label

graph.plot(g)
graph.plot(g, min.edge=0.1)

#tkplot(g) # for interactive editing of network

###############################
###############################

## Abstracts in 10 communication sciences journals from Web of Science, with 'social network' as a topic, from 2001 till 2009

load('demo/abstracts_socialnet.rdata')

dim(document.topic.matrix) # A sparse matrix of documents X topics (in this case a sample of the results from a topic model (LDA) over 848 abstracts, with 25 topics)
dim(meta) # meta data for the documents in document.topic.matrix. 

g = similarity.graph(document.topic.matrix, vertex.grouping.vars=list(journal=meta$journal, period=meta$period), similarity.measure='correlation', min.similarity=0)

g = graph.color.vertices(g, V(g)$journal) # color vertices by journal
V(g)$size = V(g)$n*2 # use number of abstracts per node (year X journal) 
V(g)$label = as.character(V(g)$journal) # use journal as vertex name
V(g)$label.cex = 0.5

graph.plot(g, min.edge=0.3, select.vertices=V(g)$period == '2001/2003')
graph.plot(g, min.edge=0.3, select.vertices=V(g)$period == '2004/2006')
graph.plot(g, min.edge=0.3, select.vertices=V(g)$period == '2007/2009')
