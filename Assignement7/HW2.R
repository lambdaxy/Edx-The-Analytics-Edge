library(igraph)

edges = read.csv("edges.csv")
users = read.csv("users.csv")

table(users$locale)
table(users$locale,users$gender)

g = graph.data.frame(d = edges, directed = FALSE, vertices = users)
plot(g, vertex.size=5, vertex.label=NA)

degrees = degree(g)
length(degrees[degrees>=10])

V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "gray"
V(g)$color[V(g)$school == "AB"] = "red"
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "gray"
V(g)$color[V(g)$locale == "B"] = "red"
plot(g, vertex.label=NA)
