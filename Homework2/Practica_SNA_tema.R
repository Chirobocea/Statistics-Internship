### Needed libraries ###
library(tidyverse, quietly = T)
library(statnet, quietly = T)
library(readxl, quietly = T)
library(GGally)
library(ggtext)
library(Matrix)


### Disable scientific notation
options(scipen = 999)

### Remove all objects to start with a clean environment & console
rm(list = ls())
gc()
cat("\f")


### Set the path for the working folder
setwd("D:/Cursuri/8. Practica/Tema 4")


#############################
### Importing data into R ###
#############################

### The network data ###
mymatrix <- read_excel("network_matrix_tema.xlsx")

# First, we have to remove the present row names and then convert the first column to rows' names.
mymatrix <- mymatrix %>%
  remove_rownames %>% column_to_rownames(var='...1')

# Second, we will convert it from a 'data.frame' to a 'matrix' object.
mymatrix <- as.matrix(mymatrix)

# Third, just to make sure we don't have reflexive ties, we will set the diagonal to 0
diag(mymatrix) <- 0

# Finally, we complete the lower half of the matrix
mymatrix<- as.matrix(forceSymmetric(mymatrix))

#View
view(mymatrix)


### The attribute data ###
attributes <- read_excel("atribute_tema.xlsx")

### Create a 'network' object using the matrix object
my_net <- as.network(mymatrix, directed = FALSE)

# Now we can attach the attributes to have them embedded in the object 'my_net'
my_net%v%'sex'<- attributes$sex
my_net%v%'city'<- attributes$city_residence
my_net%v%'age'<- attributes$age

# See again the summary of the network, with the new attributes
summary(my_net)


##################################################
### Computing various types of node centrality ###
##################################################

# We will create an R tibble where we will bind various measures of centrality and the nodes' IDs

centrality_measures <- tibble(
  # Create a column with the nodes' IDs
  ID = my_net%v%'vertex.names',
  Degree = degree(my_net, gmode = 'graph'),
  Betweenness = betweenness(my_net, gmode = 'graph', cmode = 'undirected'),
  Closeness = closeness(my_net, gmode = 'graph', cmode = 'undirected'),
  Eigenvector = evcent(my_net, gmode = 'graph'))

# Inspect the results
View(centrality_measures)

# For simplicity, we can also change how the results for the 'Eigenvector' column look like,
centrality_measures <- centrality_measures %>%
  mutate(Eigenvector = round(Eigenvector, digits = 2)) %>%
  mutate(Betweenness = round(Betweenness, digits = 2))


#####################################################
### Computing various types of network statistics ###
#####################################################

# We will create an R tibble where we will bind various measures of network level statistics
network_statistics <- tibble(
  nodes = length(my_net%v%'vertex.names'),
  edges = network.edgecount(my_net),
  density = network.density(my_net),
  reciprocity_d = grecip(my_net, measure = 'dyadic'),
  n_components = components(my_net),
  centraliz = centralization(my_net, degree, mode = 'graph')
)

# Let's make the table nicer
network_statistics_table <- as.data.frame(t(network_statistics))

colnames(network_statistics_table) <- "Network statistics"
rownames(network_statistics_table)[1] <- "Number of nodes"
rownames(network_statistics_table)[2] <- "Number of edges"
rownames(network_statistics_table)[3] <- "Network density"
rownames(network_statistics_table)[4] <- "Network reciprocity"
rownames(network_statistics_table)[5] <- "Number of components"
rownames(network_statistics_table)[6] <- "Network centralization"

network_statistics_table$`Network statistics` <- as.character(network_statistics_table$`Network statistics`)

network_statistics_table[3,] <- paste0(round(as.numeric(network_statistics_table[3,])*100, digits = 2), "%")

network_statistics_table[6,] <- substr(network_statistics_table[6,], 1,5)


# Let's have a look at the final table
network_statistics_table


#################################
### Creating the network plot ###
#################################

# Before plotting, we can add some other attributes for the nodes
# We also take care to make this attributes foe an undirected graph 
my_net%v%'degree' <- degree(my_net, gmode = 'graph')
my_net%v%'betw' <- betweenness(my_net, gmode = 'graph', cmode = 'undirected')
my_net%v%'component' <- component.dist(my_net)$membership

# Set the seed so that every time you'll get the same result
set.seed(12)

# Create the plot 
ggnet2(my_net,
       # set the color by 'sex' using different color codes for men and women
       color = 'sex', palette = c("F" = "#f08080", "M" = "#227c9d"),
       
       # set the size of the nodes according to the 'betweenness' score
       # we add 30 for scaling purposes
       size = (my_net%v%'betw' + 30),
       
       # set node labels as TRUE
       # set node label size using a condition: if the betweenness score is higher than 90, then
       # set the label size to 4, else set to 0
       # we also use vertical and horizontal justification (vjust & hjust) to move labels around
       label = T,
       label.size = ifelse(my_net%v%'betw' >= 8, 4, 0),
       vjust = -1.6, hjust = 0.1,
       
       # set the size, color, and type of the lines
       edge.size = 0.5,
       edge.color = 'black', # you can change 'black' with a Hex color code as above (for 'sex')  
       edge.lty = 'solid') +
  
# we add this to get rid of the legend
guides(size = 'none', color = 'none') +
  
# set various labels such as title and caption
# see here vor examples of markdown syntax: https://www.rstudio.com/wp-content/uploads/2015/02/rmarkdown-cheatsheet.pdf
# to start a new line, add two spaces at the end of the line. For example:
# after '__Legend:__  you'll see two blank spaces that create a new line (for __Color__...)
labs(title = '__My network__',
       caption = '___Legend:___  
       __Color__ denotes the sex of the person (men = blue; women = light coral)  
       __Size__ denotes the _betweenness_ centrality score  
       __Labels__ were selected only for nodes with a centrality score higher than 8') + 
  
# We indicate, here, that the title and the caption are 'markdown' elements
# We also indicate the color for the background of the plot
theme(plot.title = element_markdown(),
        plot.caption = element_markdown(hjust = 0),
        plot.background = element_rect(fill = "#fbfbfb"))

# Save the plot
ggsave("my_network.png", width = 14, height = 10)