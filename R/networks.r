
#### FUNCTIONS FOR CREATING GRAPHS 

#' Cast data.frame to sparse matrix
#' 
#' Create a sparse matrix from matching vectors of row indices, column indices and values
#' 
#' @param rows a vector of row indices: [i,]
#' @param columns a vector of column indices: [,j]
#' @param values a vector of the values for each (non-zero) cell: [i,j] = value
#' @return a sparse matrix of the dgTMatrix class (\code{\link{Matrix}} package) 
#' @export
cast.sparse.matrix <- function(rows, columns, values=NULL) {
  if(is.null(values)) values = rep(1, length(rows))
  unit_index = unique(rows)
  char_index = unique(columns)
  sm = spMatrix(nrow=length(unit_index), ncol=length(char_index),
                match(rows, unit_index), match(columns, char_index), values)
  rownames(sm) = unit_index
  colnames(sm) = char_index
  sm
}


#' Calculates the similarity between rows of a matrix
#' 
#' Calculates the similarity between rows of a matrix using various similarity/distance measures. Output can be a matrix or a graph object in the \code{\link{igraph}} format
#' 
#' @param matrix A matrix object, which can be a sparse matrix. 
#' @param similarity.measure A character string giving a method for computing similarity. Options are: 'correlation', 'cosine','conditional_probability','coincidence_count' and 'overlap_jacard'. 
#' @param output A character string indicating whether output should be a 'matrix' or a 'graph'. The graph is directed if assymetrical similarity measures are used ('conditional_probability' or 'overlap_jacard') 
#' @return An adjacency matrix or graph object 
#' @export
matrix.rowsimilarities <- function(mat, similarity.measure, output.as='matrix'){
  if(similarity.measure=='correlation') output = matrix.correlation(t(mat))
  if(similarity.measure=='cosine') output = matrix.cosine(t(mat))
  if(similarity.measure=='conditional_probability') output=matrix.conprob(t(mat))
  if(similarity.measure=='coincidence_count') output=matrix.coincidence.count(t(mat))
  if(similarity.measure=='overlap_jacard') output=matrix.overlap.jacard(t(mat))
  if(output.as == 'graph') {
    if(similarity.measure %in% c('overlap_jacard','conditional_probability')) edge.type = 'directed' else edge.type = 'undirected'
    colnames(output) = rownames(output) = rownames(mat)
    output = graph.adjacency(as.matrix(output), mode=edge.type, weighted=TRUE, diag=FALSE) 
  }
  output
}

#' Compute coincidence count for matrix columns
#' 
#' Computes the coincidence of all columnso of a matrix
#' 
#' @param m A matrix object. Can be a sparse matrix.
#' @return An adjacency matrix
#' @export
matrix.coincidence.count <- function(mat){
  mat@x[mat@x>0] = 1
  crossprod(mat) 
}

#' Compute jacard for matrix columns
#' 
#' Computes jacard between all columnso of a matrix
#' 
#' @param m A matrix object. Can be a sparse matrix.
#' @return An adjacency matrix
#' @export
matrix.overlap.jacard <- function(mat){
  mat@x[mat@x>0] = 1
  output = crossprod(mat) / colSums(mat)
}

#' Compute correlations for matrix columns
#' 
#' Computes correlations between all columnso of a matrix
#' 
#' @param m A matrix object. Can be a sparse matrix.
#' @return An adjacency matrix
#' @export
matrix.correlation <- function(mat){ 
  covmat = tcrossprod(colMeans(mat), (-2*colSums(mat)+nrow(mat)*colMeans(mat)))
  crossp = crossprod(mat)
  covmat = covmat + crossp
  sdvec = sqrt(diag(covmat))
  covmat / crossprod(t(sdvec))
}

#' Compute cosine for matrix columns
#' 
#' Computes the cosine distance for all columns of a matrix
#' 
#' @param m A matrix object. Can be a sparse matrix.   
#' @return An adjacency matrix
#' @export
matrix.cosine <- function(mat){
  cp = crossprod(mat)
  cp / crossprod(t(diag(sqrt(cp))))
}

#' Compute conditional probability for matrix columns
#' 
#' Computes the conditional probability for all columns of a matrix
#' 
#' @param mat A matrix object. Can be a sparse matrix.
#' @param alpha A weighting value to transform count to probabilities
#' @return An adjacency matrix
#' @export
matrix.conprob <- function(mat, alpha=2){
  mat = 1 - ((1/alpha) ^ mat)
  colsums = colSums(mat)
  mat = crossprod(mat) / colsums
  mat[is.na(mat)] = 0
  mat
}

#' Aggregates a (sparse) matrix
#' 
#' Uses the \code{\link{slam}} package rollup function to efficiently aggregate a sparse matrix. Also allows multiple vectors to be used as grouping vars. 
#' 
#' @param mat A matrix object, can be a sparse matrix
#' @param grouping.vars The grouping elements. Can be a single vector, or a list or data.frame with multiple vectors
#' @param The name of the function to be applied
#' @return A list containing a (sparse) matrix and a data.frame with the variables matching the matrix rows
#' @export
matrix.aggregate <- function(mat, grouping.vars, FUN='sum'){
  grouping.vars = data.frame(grouping.vars)
  index = match(apply(grouping.vars, 1, list), apply(unique(grouping.vars), 1, list)) # silly but efficient way to get unique ids for 1 or more columns
  
  ncolumns = ncol(mat)
  mat = rollup(as.simple_sparse_array(mat), 1L, index, FUN=FUN)
  mat = Matrix(mat, ncol=ncolumns, sparse=T)
  
  grouping.vars = unique(grouping.vars)
  grouping.vars$n = as.numeric(tapply(rep(1,length(index)), index, FUN='sum'))
  list(matrix=mat, vars=grouping.vars)
}


#' Create a content similarity graph
#' 
#' Makes a document similarity graph in the \code{\link{igraph}} format, based on a matrix in which rows are documents, and columns are content characteristics (e.g., terms, issues, topics). 
#' Vertices (i.e. nodes) are defined as unique combinations of vertex.grouping.vars. If vertices cover multiple documents (e.g., authors of several documents) then content characteristics are first aggregated.
#' The (aggregated) content characteristics are used to calculate the similarities between vertices, for which various similarity measures can be used
#' 
#' @param m A (sparse) matrix where rows are documents (e.g., news articles, forum posts) and columns are content characteristics (e.g., terms, topics). Values represent the presence of content characteristics within documents. Examples are: a \code{\link{DocumentTermMatrix}} or the transposed $document_sums (topics by documents matrix) created with \code{\link{lda.collapsed.gibbs.sampler}}. 
#' @param vertex.grouping.vars A data.frame or list with named vectors representing vertex characteristics. Each unique combination of characteristics will be considered a vertex. In the graph object these characteristics are stored as vertex attributes.
#' @param similarity.measure A character string giving a method for computing similarity. Options are: 'correlation', 'cosine','conditional_probability','overlap_count' and 'overlap_jacard'. 
#' @param topicscores.as.vertexmeta Can be used to include the topic values per node per topic. if 'all', all topics will be included. Can also be a numeric vector to select specific topics.
#' @param min.similarity A numeric scalar representing the threshold for similarities. All ties with a value below min.similarity will be deleted. Can be used to reduce the size of large graphs with many weak ties. 
#' @return A graph object in the \code{\link{igraph}} format
#' @export
content.similarity.graph <- function(m, vertex.grouping.vars, similarity.measure='cosine', topicscores.as.vertexmeta=NULL, min.similarity=NULL){
  m = Matrix(m, sparse=T)
  matlist = matrix.aggregate(m, vertex.grouping.vars)
  g = matrix.rowsimilarities(matlist$matrix, similarity.measure, output='graph')
  
  if(!is.null(min.similarity)) g = delete.edges(g, which(E(g)$weight < min.similarity))
  E(g)$similarity = E(g)$weight
  
  matlist$vars$values_sum = rowSums(matlist$matrix) 
  if(!is.null(topicscores.as.vertexmeta)) {
    colnames(matlist$matrix) = paste('Topic:', 1:ncol(matlist$matrix))
    if(topicscores.as.vertexmeta== 'all') matlist$vars = cbind(matlist$vars, as.matrix(matlist$matrix))
    if(class(topicscores.as.vertexmeta) == 'numeric') matlist$vars = cbind(matlist$vars, matlist$matrix[,topicscores.as.vertexmeta, drop=F])
  }
  vertex.attributes(g) = as.list(matlist$vars)
  g = default.graph.attributes(g)
  g
}

#' Sets basic default values for content.similarity.graph
#' 
#' Sets basic default values for content.similarity.graph
#' 
#' @param g A graph object in the \code{\link{igraph}} format
#' @return A graph object in the \code{\link{igraph}} format
#' @export
default.graph.attributes <- function(g){  
  g$layout = layout.fruchterman.reingold
  V(g)$size = log(V(g)$values_sum)*2
  V(g)$label = as.character(1:length(V(g)))
  V(g)$label.cex = V(g)$size / 15
  V(g)$label.color = 'black'
  E(g)$weigth = E(g)$width = E(g)$similarity*10
  g
}



#' Assign vertex colors to a graph
#' 
#' A convenience function to add colors to a graph of the \code{\link{igraph}} format based on a vector of vertex attributes. 
#' 
#' @param g A graph object in the \code{\link{igraph}} format
#' @param attribute A vertex attribute to organize the colors. Either use a categorical vector (factor, character) to color categories in different colors, or use a numerical vector (numerical, integer) to highlight with one color (like a heat map)
#' @param pallete Optional. Add a custom pallete, which should be a character vector with color codes that igraph can use.
#' @return A graph object in the \code{\link{igraph}} format
#' @export
graph.color.vertices <- function(g, attribute, pallete=NULL){
  if(class(attribute) %in% c('character','factor')) {
    if(is.null(pallete)) pallete = colors()[grep('[0-9]', colors(), invert=T)]
    unique.att = unique(attribute)
    if(length(unique.att) > length(pallete)) replace_colors = TRUE else replace_colors = FALSE
    pallete = data.frame(attribute=unique.att, color=sample(pallete, replace=replace_colors, length(unique.att)))
    V(g)$color = as.character(pallete$color[match(attribute, pallete$attribute)])
    print(pallete)
  }
  if(class(attribute) %in% c('numeric','integer')) {
    attribute = attribute
    pallete = colorRampPalette(c("white","red"))(max(attribute))
    V(g)$color = as.character(pallete[attribute])
  }    
  g
}

#' A wrapper for plotting graphs
#' 
#' A wrapper for plotting graphs in the \code{\link{igraph}} format that incorporates the functions to delete edges and vertices
#' 
#' @param g A graph object in the \code{\link{igraph}} format
#' @param min.edge.value Numerical scalar. Edges with a lower similarity will be deleted (note that 'similarity' attribute of the edge is used, not the weight attribute)
#' @param max.edge.value Like min.edge.value, but for the max value
#' @param delete.vertices Either a logical vector (TRUE/FALSE) with TRUE meaning that a vertex will be deleted, or indices for vertices that will be deleted
#' @param select.vertices Same as delete.vertices, but inversed
#' @param min.degree Numerical scalar, indicating the minimum number of edges a vertex needs to have in order to be included in the graph. Set to 1 to ignore unconnected vertices
#' @param use.tkplot Logical. If TRUE, tkplot is used instead of plot, which allows some interactive editing of the network
#' @param return.graph Logical. If TRUE, the function will return the (filtered) graph object 
#' @return if return.graph is TRUE, A graph object in the \code{\link{igraph}} format
#' @export
graph.plot <- function(g, min.edge=NULL, max.edge=NULL, delete.vertices=NULL, select.vertices=NULL, min.degree=NULL, use.tkplot=FALSE, return.graph=FALSE){ 
  if(!is.null(min.edge)) g=delete.edges(g, which(E(g)$similarity < min.edge))
  if(!is.null(max.edge)) g=delete.edges(g, which(E(g)$similarity > max.edge))
  if(!is.null(delete.vertices)){
    if(class(delete.vertices) == 'logical') delete.vertices = which(delete.vertices)
    g=delete.vertices(g, delete.vertices)
  }
  if(!is.null(select.vertices)){
    if(class(select.vertices) == 'logical') select.vertices = which(select.vertices)
    all.vertices = 1:length(V(g))
    g=delete.vertices(g, all.vertices[-select.vertices])
  }
  if(!is.null(min.degree)) g = delete.vertices(g, which(degree(g) < min.degree))
  
  if(use.tkplot == TRUE) tkplot(g) else plot(g)
  if(return.graph == TRUE) g
}

#### SOCIAL NETWORK FUNCTIONS

#' Split strings to rows with matching ids
#' 
#' A convenience function. Mainly designed for preparing data for co-author type networks, if an unknown number of authors per document is stored as a single character string with delimiters.
#' A character string is split by a given character vector. For each part of the string a new row is created. An id value indicates which parts belong together. An order value indicates which part of the original string the stringpart was
#' 
#' @param x A character vector.
#' @param split_by The character vector by which x will be split. By default should be a regular expression (see \code{\link{strsplit}}) but can be changed using the ellipsis parameters
#' @param id The id to be assigned to the rows. If a vector of the same lengt of x is given, these values will be used. 
#' @param ... additional parameters to be passed to the strsplit function
#' @return A data.frame with the columns id, order and substring
#' @export
splitstr.to.rows <- function(x, split_by=';', id=NULL, ...){
  if(is.null(id)) id = 1:length(x)
  tmp = strsplit(as.character(x), split_by, ...)
  nMax = max(sapply(tmp, length))
  cols = t(sapply(tmp, function(i) i[1:nMax]))
  substring = as.vector(t(cols))
  ord = rep(1:nMax, times=length(id))
  id = rep(id, each=nMax)
  d = data.frame(id=id, order=ord, substring=substring)
  unique(d[!is.na(d$substring),])
}


#' Create a graph from conversation data
#' 
#' Create a graph from conversation data
#'   
#' @param conversation.id A vector representing the conversation authors were engaged in (e.g. a thread, meeting)
#' @param author A vector with unique author names/ids
#' @param order.nr Optional. A vector representing the order in which authors communicated within a conversation. 
#' @param order.lookback Requires an order.nr vector. If value is 'first', a tie will be drawn between each author with the first author of a converstation. If value is a (non-negative) numerical scalar (x), ties are drawn between authors with the previous x authors. For example, if value is 1, ties are drawn between an author and the previous author. If value is 2, between an author and the previous 2 authors. etc.  
#' @param direction A character string, indicating the direction of the ties. If 'undirected', ties between authors go both ways. If 'directed.up', ties are directed from author to previous authors (only if order is given). if 'directed.down', ties are directed from author to later authors. 
#' @return A data.frame with the columns id, order and substring
#' @export
conversation.graph.create <- function(conversation, author, order.nr=NULL, order.lookback=NULL, direction='undirected'){  
  if(!is.null(order.nr) & !is.null(order.lookback)){
    if(order.lookback == 'first') g = first.author.graph(conversation, author, order.nr, direction)
    if(class(order.lookback) %in% c('numeric', 'integer')) g = previous.authors.graph(conversation, author, order.nr, order.lookback, direction)
  } else{
    if(!is.null(order.nr)) warning('order.nr is given, but order.lookback is NULL. Order will be ignored')
    g = matrix.rowsimilarities(cast.sparse.matrix(author, conversation), similarity.measure='coincidence_count', output.as='graph') 
  }
  g
}

#' Create a graph from conversation data
#' 
#' Create a graph from conversation data, in which ties are drawn between each authors in the conversation and the first author. 
#'   
#' @param conversation.id A vector representing the conversation authors were engaged in (e.g. a thread, meeting)
#' @param author A vector with unique author names/ids
#' @param order.nr Optional. A vector representing the order in which authors communicated within a conversation. 
#' @param direction A character string, indicating the direction of the ties. If 'undirected', ties between authors go both ways. If 'directed.up', ties are directed from author to previous authors (only if order is given). if 'directed.down', ties are directed from author to later authors. 
#' @return A graph object in the \code{\link{igraph}} format
#' @export
first.author.graph <- function(conversation, author, order.nr, direction){
  index = unique(author)
  author = match(author, unique(author))
  first_author = author[match(apply(cbind(conversation, 1), 1, list), apply(cbind(conversation, order.nr), 1, list))]
  if(direction == 'downward') xy = cbind(x=first_author, y=author) else xy = cbind(x=author, y=first_author) 
  xy = xy[!is.na(xy[,2]),]
  m = spMatrix(nrow=length(index), ncol=length(index), i=xy[,1], j=xy[,2], rep(1, nrow(xy)))
  rownames(m) = colnames(m) = index
  if(direction == 'undirected') {
    m = forceSymmetric(m, uplo='U') + forceSymmetric(m, uplo='L')
    g = graph.adjacency(m, mode='undirected', weighted=TRUE, diag=FALSE)
  }
  if(direction %in% c('directed.up','directed.down')) g = graph.adjacency(m, mode='directed', weighted=TRUE, diag=FALSE)
  g
}

#' Create a graph from conversation data
#' 
#' Create a graph from conversation data, in which ties are drawn between authors and the authors that communicated within a given distance (lookback) before them.
#'   
#' @param conversation.id A vector representing the conversation authors were engaged in (e.g. a thread, meeting)
#' @param author A vector with unique author names/ids
#' @param order.nr Optional. A vector representing the order in which authors communicated within a conversation. 
#' @param lookback The number of previous author with whom a tie with an author should be drawn
#' @param direction A character string, indicating the direction of the ties. If 'undirected', ties between authors go both ways. If 'directed.up', ties are directed from author to previous authors (only if order is given). if 'directed.down', ties are directed from author to later authors. 
#' @return A graph object in the \code{\link{igraph}} format
#' @export
previous.authors.graph <- function(conversation, author, order.nr, lookback, direction){
  index = unique(author)
  author = match(author, unique(author))
  for(lb in 1:lookback){
    previous_author = author[match(apply(cbind(conversation, order.nr-lb), 1, list), apply(cbind(conversation,order.nr), 1, list))]
    xy = cbind(x=author, y=previous_author)
    xy = xy[!is.na(xy[,2]),]
    if(is.null(nrow(xy))) next
    if(lb == 1) m = spMatrix(nrow=length(index), ncol=length(index), i=xy[,1], j=xy[,2], rep(1, nrow(xy)))
    if(lb > 1) m = m + spMatrix(nrow=length(index), ncol=length(index), i=xy[,1], j=xy[,2], rep(1, nrow(xy)))
  }
  rownames(m) = colnames(m) = index
  if(direction == 'undirected') {
    m = forceSymmetric(m, uplo='U') + forceSymmetric(m, uplo='L')
    g = graph.adjacency(m, mode='undirected', weighted=TRUE, diag=FALSE)
  }
  if(direction %in% c('directed.up','directed.down')) g = graph.adjacency(m, mode='directed', weighted=TRUE, diag=FALSE)
  g
}

?graph.adjacency
