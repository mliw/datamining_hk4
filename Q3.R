library(tidyverse)
library(arules)  # has a big ecosystem of packages built around it
library(arulesViz)
library(igraph)

# line is all basket
fileName="data/groceries.txt"
con=file(fileName,open="r")
line=readLines(con)
close(con)

# Split lines get variable baskets
baskets = list()
total_length = length(line)
for (i in 1:total_length){
  baskets[[i]]=strsplit(line[[i]], ",")[[1]]
}
baskets = lapply(baskets, unique)
# arules "transactions" class.
playtrans = as(baskets, "transactions")
summary(playtrans)

# Now run the 'apriori' algorithm
musicrules = apriori(playtrans, 
	parameter=list(support=.005, confidence=.1, maxlen=2))
# Pick your own thresholds for lift and confidence; 
# just be clear what these thresholds are and how you picked them. 
# Do your discovered item sets make sense? 
# Present your discoveries in an interesting and concise way.
             
# Look at the output... so many rules!
inspect(musicrules)
## Choose a subset
inspect(subset(musicrules, lift > 2))
inspect(subset(musicrules, confidence > 0.3))
inspect(subset(musicrules, lift > 2 & confidence > 0.3))


# graph-based visualization
sub1 = subset(musicrules, subset=lift > 2 & confidence > 0.3)
jpeg(file = "pics/Q3_rule.png")
plot(sub1, method='graph')
dev.off()

