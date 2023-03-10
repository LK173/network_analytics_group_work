# Load librarys-----------------------------------------------------------------
library(tidyverse)
library(igraph)
library(data.table)
library(ggplot2)
library(tibble)


# Load data---------------------------------------------------------------------
football.transfer <- read.csv(file = 'data/football-transfers.csv')

# Filtering NA's out of columns market_value & fee------------------------------

df.filtered.data <- na.omit(football.transfer)
df.filtered.data <- rownames_to_column(df.filtered.data, var = "index")
df.filtered.data <- subset(df.filtered.data, select = -index)
dt.filtered.data <- data.table(df.filtered.data)


# Filter the rows of column "movement" that only contain the string "in"--------
# Player transfer from dealing_club to club

df.filtered.data.in <- subset(df.filtered.data, movement == "in")
df.filtered.data.in <- rownames_to_column(df.filtered.data.in, var = "index")
df.filtered.data.in <- subset(df.filtered.data.in, select = -index)
dt.filtered.data.in <- data.table(df.filtered.data.in)

# Filter the rows of column "movement" that only contain the string "out"-------
# Player transfer from club to dealing club

df.filtered.data.out <- subset(df.filtered.data, movement == "out")
df.filtered.data.out <- rownames_to_column(df.filtered.data.out, var = "index")
df.filtered.data.out <- subset(df.filtered.data.out, select = -index)
dt.filtered.data.out <- data.table(df.filtered.data.out)


# Build a bipartite graph of the in movement data/ from dealing club to club----

all.club <- dt.filtered.data.in[, list(name = unique(club),
                                       type=TRUE)]
all.name <- dt.filtered.data.in[, list(name = unique(name), type=FALSE)]

all.vertices <- rbind(all.club, all.name)
g.transfers.in <- graph.data.frame(dt.filtered.data.in[, list(club, 
                                                              name)], 
                                   directed=FALSE, vertices=all.vertices)

# Plot the bipartite graph------------------------------------------------------
V(g.transfers.in)$color <- ifelse(V(g.transfers.in)$type == 1, "red", "blue")

plot(g.transfers.in, layout = layout.bipartite(g.transfers.in), vertex.label = NA)

plot(g.transfers.in, layout = layout_with_fr, vertex.color = V(g.transfers.in)$color,
     vertex.label = NA, vertex.size = 1, margin = -0.1)

# Projection 1 edges = 2 clubs sind verbunden wenn gleichen spieler haben
g.clubs <- bipartite.projection(g.transfers.in)$proj1
plot(g.clubs, layout = layout_with_fr, vertex.color = V(g.clubs)$color, 
     vertex.label = NA, vertex.size = 1, margin = -0.1)
summary(g.clubs)

# Projection 2 edges = 2 spieler sind verbunden wenn im gleichen clubs gespielt haben
g.player <- bipartite.projection(g.transfers.in)$proj2
plot(g.player, layout = layout_with_fr, vertex.color = V(g.player)$color, 
     vertex.label = NA, vertex.size = 1, margin = -0.1)
summary(g.player)