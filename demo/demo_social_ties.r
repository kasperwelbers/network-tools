load('demo/parliamentary_proceedings.rdata')

d = data.frame(conversation=c(1,1,1,1,1,1),
               author=c('Alice','Bob','Alice','Dave','Bob','Bob'),
               order.nr=c(1,2,3,4,5,6))

g = conversation.graph.create(d$conversation, d$author)
plot(g)
g = conversation.graph.create(d$conversation, d$author, d$order.nr, order.lookback='first') # undirected graph by default
plot(g)
g = conversation.graph.create(d$conversation, d$author, d$order.nr, order.lookback='first', direction='directed.up')
plot(g)
g = conversation.graph.create(d$conversation, d$author, d$order.nr, order.lookback=1, direction='directed.up')
plot(g)
g = conversation.graph.create(d$conversation, d$author, d$order.nr, order.lookback=2, direction='directed.up')
plot(g)
