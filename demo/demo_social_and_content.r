
load('demo/parliamentary_proceedings.rdata')

head(meta)
meta$author = paste(meta$name, meta$party)

g_soc = previous.authors.graph(meta$meeting, meta$author, meta$pagenr, lookback=10, direction='undirected', once.per.conversation=F) 
g_con = content.similarity.graph(document.topic.matrix, vertex.grouping.vars=list(author=meta$author, party=meta$party), similarity.measure='correlation')

V(g_soc)$name
V(g_con)$name
V(g_con)$name = as.character(V(g_con)$author)

g = graph.union(g_soc, g_con, byname=TRUE)

list.vertex.attributes(g)
list.edge.attributes(g)

E(g)$conversation[is.na(E(g)$conversation)] = 0
summary(lm(E(g)$similarity ~ E(g)$conversation))

sc = spinglass.community(g, E(g)$similarity)
g = graph.color.vertices(g, as.character(sc$membership))

E(g)$weight = E(g)$width = E(g)$conversation / 3
gs = graph.plot(g, min.edge=1, return.graph=T)


