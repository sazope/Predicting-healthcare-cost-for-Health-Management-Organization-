library (tidyverse)
library(RCurl)
library(jsonlite)
library(imputeTS)
library(ggplot2) 
library(ggmap)
library (arules)

data<- data.frame(read_csv('HMO_data.csv'))
summary(data)

table(data$location)

hist(data[data$location -- 'MASSACHUSETTS', ]$cost)

boxplot(data$cost)

#Below code is trying to estimate the threshold of being expensive -->
# 5000 was chosen based on professor suggestion

### k-means clustering
x= data$cost
table(discretize(x, "cluster", categories=2))

hist(x, breaks=20, main="K-Means")
abline(v=discretize(x, method="cluster", categories=2, onlycuts=TRUE), col="red")

discretize(x, method="cluster", categories=2, onlycuts=TRUE)

### equal frequency
table(discretize(x, "frequency", categories=3))

hist(x, breaks=20, main="Equal Frequency")
abline(v=discretize(x, method="frequency", categories=3, onlycuts=TRUE), col="red")

discretize(x, method="frequency", categories=3, onlycuts=TRUE)
table(discretize(x, categories=2))

hist(x, breaks=20, main="Equal Interval length") 
abline(v=discretize(x, categories=2, onlycuts=TRUE), col="red")

discretize(x, method=''frequency", categories=2, onlycuts=TRUE)

# Create the expensive table 

data<- transform(
data, expensive= ifelse(cost > 5000, 1,0))
table(data$expensive)


ggplot(data, aes(x=exercise, y=cost, fill= expensive))+ geom_boxplot()

ggplot(data, aes(x=yearly_physical, y=cost, fill= expensive))+ geom_boxplot()

ggplot(data, aes(x=married, y=cost, fill= expensive))+ geom_boxplot()

ggplot(data, aes(x=gender, y=cost, fill= expensive))+ geom_boxplot()

ggplot(data, aes(x= education_level, y=cost, fill= expensive))+ geom_boxplot()

ggplot(data, aes(x= location_type, y=cost, fill= expensive))+ geom_boxplot()

ggplot(data, aes(x= smoker, y=cost, fill= expensive))+ geom_boxplot()

ggplot(data, aes(x= location, y=cost, fill= expensive))+ geom_boxplot()

numerical_data <- data[,c('age','bmi', 'cost')] summary(numerical_data)

res<- cor(numerical_data) 
round(res, 2)

data[is.na(data$bmi) , ]

hist(data$bmi)
data<- data%>% mutate(across(bmi, replace_na(., mean(., na.rm=TRUE))))
getmode <- function(v) { uniqv <- unique(v)
uniqv[which.max(tabulate(match(v, uniqv)))]
}
data_mode <- getmode data<- data%>%
mutate(hypertension if_else(is.na(hypertension),
getmode(hypertension), hypertension))

data[is.na(data$bmi) , ]

numerical_data <- data[,c('age','bmi', 'cost')] 
summary(numerical_data)

res<- cor(numerical_data) 
round(res, 2)

hist(data$age)

#Scattor plot using GGPLOT2
my_plot <- ggplot(data)
my_plot <- my_plot + geom_point(aes(x=age, y= cost, color= factor(expensive),alpha = 1/10 )) my_plot <- my_plot
my_plot

#Scattor plot using GGPLOT2
my_plot <- ggplot(data)
my_plot <- my_plot + geom_point(aes(x=bmi, y= cost, color= factor(expensive),alpha = 1/10 )) my_plot <- my_plot + ggtitle("PositiveCases vs TotalTests - older_Adults")
my_plot

data$children <- as.factor(data$children) 
data$hypertension <- as.factor(data$hypertension)

ggplot(data, aes(x= children, y=cost, fill= expensive)) + 
geom_boxplot()

hist(as.numeric(data$children))

ggplot(data, aes(x= hypertension, y=cost, fill= expensive))+ 
geom_boxplot()

table(data$hypertension)

ggplot(data, aes(x=cost, fill=exercise)) +
geom_histogram( color='#e9ecef', alpha=0.6, position='identity')

table(data$exercise)

data%>% group_by(location) %>% summarise(median = median(cost))

data%>% group_by(gender) %>% summarise(median = median(cost))

data%>% group_by(hypertension) %>% summarise(median =  median(cost))

data%>% group_by(children) %>% summarise(median = median(cost))

hist(as.numeric(data$children))

location_data <-data%>% group_by(location) %>% 
summarise(sum = sum(cost), median= median(cost))
location_data

table(data$location)


us<- map_data("state")
location_data$location <- tolower(location_data$location)
dfStatesWithGeom <- merge(us, location_data, all.x=TRUE, by.x="region", by.y = "location")
d <- merge(us, location_data, by.x="region", by.y = "location")
dfStatesWithGeom <- dfStatesWithGeom %>% arrange(order)

myMap <- ggplot(dfStatesWithGeom) + geom_polygon( color	"black", aes(x=long,y=lat, group =
group, fill =median)) +coord_map()
myMap

bb <- c(left = min(d$lon), 
bottom= min(d$lat), 
right= max(d$lon),
top= max(d$lat))

stateCenter <- data.frame(state= tolower(state.name),
x= state.center$x, y =state.center$y
)

#dfStatesWithGeom
dfStatesWithCenter <- merge(dfStatesWithGeom,stateCenter, by.x="region'', by.y = "state")
dfStatesWithCenter <- dfStatesWithCenter %>% arrange(order)

myMap2 <- ggplot(dfStatesWithCenter)+geom_polygon(fill="white", color= "black", 
                                                  aes(x= long, y = lat, group= group))+ 
geom_point(aes(x=x, y=y,size = median)) +coord_map()
myMap2

map<- get_stamenmap(bbox = bb, zoom= 6)
library(ggmap)

ggmap(map) +geom_polygon(data = dfStatesWithCenter, fill="NA", color= "black'', aes(x= long,y
lat, group= group))+ geom_point(data = dfStatesWithCenter, aes(x=x, y=y,size = median))

ggmap(map) + geom_polygon(data= dfStatesWithGeom, alpha= 0.8,aes(x = long, 
y= lat, group =gr oup,fill = median), color ="black")

HMO_data < - data[, c('children', 'smoker', 'location', 'exercise', 'hypertension', 
'gender', 'expensive', 'cost')]

HMO_data$expensive <- as.factor(HMO_data$expensive)

ggplot(HMO_data, aes(x= smoker, y=cost, fill= expensive))+ geom_boxplot()

ggplot(HMO_data, aes(x= location, y=cost, fill= expensive))+ geom_boxplot()

ggplot(data, aes(x= as.factor(expensive) , y= age, fill= expensive))+ geom_boxplot()




