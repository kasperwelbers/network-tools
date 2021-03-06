% Generated by roxygen2 (4.0.0): do not edit by hand
\name{coincidence.graph}
\alias{coincidence.graph}
\title{Create a coincidence graph}
\usage{
coincidence.graph(participation.id, author,
  similarity.measure = "coincidence_count")
}
\arguments{
\item{conversation.id}{A vector representing the conversation authors were engaged in (e.g. a thread, meeting)}

\item{author}{A vector with unique author names/ids}
}
\value{
A graph object in the \code{\link{igraph}} format
}
\description{
Create a graph from conversation data
}
\examples{
d = data.frame(conversation=c(1,1,1,1,1,1,2,2,2,2),
              author=c('Alice','Bob','Alice','Dave','Bob','Bob','Alice','Bob','Alice','Bob'),
              order.nr=c(1,2,3,4,5,6,1,2,3,4))
g = author.coincidence.graph(d$conversation, d$author) # In how many conversations did author.X and author.Y communicate?
plot(g, edge.label=E(g)$weight, vertex.size=V(g)$n.documents*25)
g = author.coincidence.graph(d$conversation, d$author, 'overlap_jacard') # Similar to default (coincidence_count) but with direction (by dividing coincidence by number of conversations author participated in)
plot(g, edge.label=E(g)$weight, vertex.size=V(g)$n.messages*15)
g = author.coincidence.graph(d$conversation, d$author, 'cosine') # Cosine can be used to also take into account how many times each author participated within conversations
plot(g, edge.label=E(g)$weight, vertex.size=V(g)$n.messages*15)
}

