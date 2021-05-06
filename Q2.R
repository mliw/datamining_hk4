library(ggplot2)
library(corrplot)
social_marketing <- read.csv("data/social_marketing.csv", stringsAsFactors=TRUE,row.names=1)
# data pre-processing
# There are 4 unwanted categories "chatter","uncategorized","adult","spam", we delete them. 
myvars<- names(social_marketing) %in% c("chatter","uncategorized","adult","spam")
newdata<- social_marketing[!myvars]
# Find correlation
mcor <- cor(newdata) # 计算相关矩阵并赋值给mcor
mcor = round(mcor, digits = 2) # 保留两位小数
mcor = as.data.frame(mcor)

# Link matrix
link = mcor>=0.5
for (i in 1:dim(link)[1]){
  link[i,i]=FALSE
  print(i)
}
result = list()
for (name in colnames(link)){
  if (sum(link[,name])>1){
    tem = c(name,rownames(link)[link[,name]])
    result[[name]] <- tem
  }
}
print(result)

# Divide all features in 5 groups of correlated interests.
s_total = colnames(mcor)
s1 = c("travel","politics","computers","news","automotive")
s2 = c("sports_fandom","food","religion","parenting","school")
s3 = c("health_nutrition","outdoors","personal_fitness")
s4 = c("college_uni","online_gaming","sports_playing")
s5 = c("cooking","beauty","fashion")
# Other features aren't important
print(setdiff(s_total,c(s1,s2,s3,s4,s5)))

#1 middle-aged man who like traveling. 
#2 Young parents who raise their kids
#3 Old people who care about their health
#4 Young students worry about their study.
#5 Fashionable woman

# drinks brand  
# 1 relax yourselves
# 3 good for health
# 5 become more beautiful
tem = newdata[,s1]
print("group 1")
print(apply(tem,2,mean))
tem = newdata[,s2]
print("group 2")
print(apply(tem,2,mean))
tem = newdata[,s3]
print("group 3")
print(apply(tem,2,mean))
tem = newdata[,s4]
print("group 4")
print(apply(tem,2,mean))
tem = newdata[,s5]
print("group 5")
print(apply(tem,2,mean))

# Just use the data to come up with some interesting, well-supported insights about the audience and give your client some insight as to how they might position
# their brand to maximally appeal to each market segment.
