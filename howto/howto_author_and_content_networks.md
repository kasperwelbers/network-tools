Combining author networks and content similarity networks
==========================

The similarity of the content of documents can be expressed as graphs/networks, and communicative ties between authors of documents can be expresssed as social networks. By combining both networks, we can study how `what authors communicate about` and `who they communicate with` relate, and how this changes over time.

In this howto we demonstrate how to combine both networks for this type of analysis.

As demo data we use speech acts in Dutch parliamentary proceedings by two parties (VVD and CDA) from 2003 till 2005 (samples of 100 proceedings per year)


```r
library(networktools)

load("../demo/parliamentary_proceedings.rdata")
meta$author = paste(meta$name, meta$party)
head(meta)
```

```
##             id       date speech_act                 meeting
## 33003 33907051 2003-01-29         48 h-tk-20022003-2793-2802
## 32995 33907175 2003-01-30         22 h-tk-20022003-2821-2849
## 81497 33909339 2003-02-18        248 h-tk-20022003-3055-3080
## 81480 33909445 2003-02-20         37 h-tk-20022003-3153-3161
## 81478 33909462 2003-02-20         54 h-tk-20022003-3153-3161
## 32630 33910920 2003-03-18          8 h-tk-20022003-3316-3327
##                         name party year                     author
## 33003         De heer Blaauw   VVD 2003         De heer Blaauw VVD
## 32995    De heer Van Aartsen   VVD 2003    De heer Van Aartsen VVD
## 81497 Mevrouw Vroonhoven-Kok   CDA 2003 Mevrouw Vroonhoven-Kok CDA
## 81480          De heer Atsma   CDA 2003          De heer Atsma CDA
## 81478          De heer Atsma   CDA 2003          De heer Atsma CDA
## 32630           De heer Zalm   VVD 2003           De heer Zalm VVD
```


The steps for creating the content similarity network and the author network are described in `howto_author_network.md` and `hotwo_content_similarity_network.md`. Here we use the `previous.authors.graph` and `content.similarity.graph`. 


```r
g_soc = previous.authors.graph(meta$meeting, meta$author, meta$pagenr, lookback = 10, 
    direction = "undirected", once.per.conversation = F)
g_con = content.similarity.graph(document.topic.matrix, vertex.grouping.vars = list(author = meta$author, 
    party = meta$party), similarity.measure = "correlation")
```


g_soc is the author (social network) graph, based on which authors participaged in a discussion within a distance of 10 speech acts. 
g_con is the content similarity graph that indicates how similar the topics of author communications are.

In both networks the nodex/vertices represent authors (politicans). Both graphs are in the igraph format. Thanks to igraph, we can easily merge both networks based on the vertex names. For this, we first explicitly set the names of the vertices (V(g)$name) based on the vertex attribute called 'author'.


```r
V(g_soc)$name = as.character(V(g_soc)$author)
V(g_con)$name = as.character(V(g_con)$author)
V(g_soc)$name %in% V(g_con)$name  # see whether nodes in both networks match
```

```
##  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [15] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [29] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [43] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [57] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [71] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [85] TRUE TRUE TRUE TRUE TRUE TRUE
```


We can now use the graph.union() function, from the igraph package, to merge both networks based on vertex names


```r
g = graph.union(g_soc, g_con, byname = TRUE)
list.vertex.attributes(g)
```

```
##  [1] "author_1"    "author_2"    "n.documents" "n.messages"  "party"      
##  [6] "n"           "values_sum"  "size"        "label"       "label.cex"  
## [11] "label.color" "name"
```

```r
list.edge.attributes(g)
```

```
## [1] "weight_1"    "weight_2"    "width_1"     "width_2"     "author.ties"
## [6] "similarity"
```


The merged graph contains the attributes of both graphs. Common attributes (e.g., weight, width) are doubled. In the edge.attributes we both see the ties for the author network (author.ties) and for the content similarity network (similarity)

Now we can analyze their similarities. For a simple indication, we can check the correlation between the tie weights


```r
E(g)$author.ties[is.na(E(g)$author.ties)] = 0  # first change the weight of missing author network ties to zero 
cor.test(E(g)$similarity, E(g)$author.ties)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  E(g)$similarity and E(g)$author.ties
## t = 23.43, df = 4000, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.3198 0.3743
## sample estimates:
##    cor 
## 0.3473
```


There is a clear relation between the author network (who communicate with each other) and the content similarity network (how similar are authors in what they say). This is according to expectation, since people engaged in a conversation are likely to be talking about the same things.

For a visual approximation we can also color the nodes based on communities. For this we first use a community detection alghoritm to find communities of nodes based on their content similarity, and use the community membership to color nodes.


```r
sc = spinglass.community(g, E(g)$similarity)
g = graph.color.vertices(g, as.character(sc$membership))
```

```
##   attribute           color
## 1         7       limegreen
## 2         1          maroon
## 3         3 mediumvioletred
## 4         5     lightyellow
## 5         2       gainsboro
## 6         4    midnightblue
## 7         6       slateblue
```


Now we set the weight and width of the ties of the network to the author.ties, and plot the network


```r
E(g)$weight = E(g)$width = E(g)$author.ties/3
graph.plot(g, min.edge = 1)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 




