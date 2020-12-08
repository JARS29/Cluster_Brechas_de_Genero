library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)

library(tidyverse)  # data manipulation
library(cluster)   
library("factoextra")
library(gridExtra)
library(grid)
library(tidyr)
library(RColorBrewer)
library(plotly)
hm.palette <-colorRampPalette(rev(brewer.pal(10, 'RdYlGn')),space='Lab')

#################




######





app <- Dash$new()


####################
df<-read.csv("unimar.csv", header = T, sep = ';',
             encoding = 'UTF-8', stringsAsFactors=T)
df_questions<-subset(df, select = c(19:63))
# df<-read.csv("data/udenar.csv", header = T, sep = ';',
#              encoding = 'UTF-8', stringsAsFactors=T)
# df2<-read.csv("data/cesmag.csv", header = T, sep = ';',
#              encoding = 'UTF-8', stringsAsFactors=T)
# 
# df3<-read.csv("data/aunar.csv", header = T, sep = ';',
#               encoding = 'UTF-8', stringsAsFactors=T)
# 


#df<-na.omit(df)
# df_Q<-subset(df, select = c(14:68)) 
# df_Q<-subset(df_Q, select = -c(2)) #2d question  #Revisar para las demas bases de datos
# df_Q_dec<-subset(df_Q, select = c(8:54))
df_questions<-scale(df_questions)
# 
set.seed(123)
# 
# #distance_d<- get_dist(df_Q_dec)
# #fviz_dist(distance_d, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
#fviz_nbclust(df_questions, kmeans, method = "wss")
fviz_nbclust(df_questions, kmeans, method = "silhouette")
k2 <- kmeans(df_questions, centers = 2, nstart = 25, iter.max = 35)
k3 <- kmeans(df_questions, centers = 3, nstart = 25,iter.max = 35)
k4 <- kmeans(df_questions, centers = 4, nstart = 25,iter.max = 35)
k10 <- kmeans(df_questions, centers = 10, nstart = 25,iter.max = 35)
# # plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df_questions) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df_questions) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df_questions) + ggtitle("k = 4")
p6 <- fviz_cluster(k10, geom = "point",  data = df_questions) + ggtitle("k = 10")
# 
grid.arrange(p1, p2, p3, p6, nrow = 1)
#fviz_cluster(k2, data = df_questions)

center2 <-k2$centers
center3 <-k3$centers
center4 <-k4$centers
center10 <-k10$centers

cluster2 <- c(1: 2)
cluster3 <- c(1: 3)
cluster4 <- c(1: 4)
cluster10 <- c(1:10)
center_df2 <- data.frame(cluster2, center2)
center_df3 <- data.frame(cluster3, center3)
center_df4 <- data.frame(cluster4, center4)
center_df10 <- data.frame(cluster10, center10)


center_reshape2 <- gather(center_df2, features, values, Q7: Q51)
center_reshape3 <- gather(center_df3, features, values, Q7: Q51)
center_reshape4 <- gather(center_df4, features, values, Q7: Q51)
center_reshape10 <- gather(center_df10, features, values, Q7: Q51)

level_order<-colnames(df_questions)



p1<-ggplot(data = center_reshape2, aes(x = factor(features, level = level_order), y = cluster2, fill = values)) +
  scale_y_continuous(breaks = seq(1, 2, by = 1)) +
  geom_tile() +
  #coord_equal() +
  scale_fill_gradientn(colours = hm.palette(90)) +
  theme(axis.text.x = element_text(angle = 90))

p2<-ggplot(data = center_reshape3, aes(x = factor(features, level = level_order), y = cluster3, fill = values)) +
  scale_y_continuous(breaks = seq(1, 3, by = 1)) +
  geom_tile() +
  #coord_equal() +
  scale_fill_gradientn(colours = hm.palette(90)) +
  theme(axis.text.x = element_text(angle = 90))

p3<-ggplot(data = center_reshape4, aes(x = factor(features, level = level_order), y = cluster4, fill = values)) +
  scale_y_continuous(breaks = seq(1, 4, by = 1)) +
  geom_tile() +
  #coord_equal() +
  scale_fill_gradientn(colours = hm.palette(90)) +
  theme(axis.text.x = element_text(angle = 90))

p4<-ggplot(data = center_reshape10, aes(x = factor(features, level = level_order), y = cluster10, fill = values)) +
  scale_y_continuous(breaks = seq(1, 10, by = 1)) +
  geom_tile() +
  #coord_equal() +
  scale_fill_gradientn(colours = hm.palette(90)) +
  theme(axis.text.x = element_text(angle = 90))
################

Categoría <- unique(df$Categoría)

data_val <- with(df,
                               lapply(Categoría,
                                      with(df,
                                 
                                 list(
                                   x =cluster2,
                                   y = factor(features, level = level_order),
                                   opacity=0.7,
                                   #mode = 'markers',
                                   name = cont,
                                   marker = list(size = 15,
                                                 line = list(width = 0.5, color = 'white'))
                                 )+
                                   scale_y_continuous(breaks = seq(1, 3, by = 1)) +
                                   geom_tile() +
                                   #coord_equal() +
                                   scale_fill_gradientn(colours = hm.palette(90)) +
                                   theme(axis.text.x = element_text(angle = 90))
                               )
                                    

                                      
                               )
)

app$layout(
  htmlDiv(
    list(
      dccGraph(
        figure = list(
          data =  data_val,
          layout = list(
            xaxis = list('type' = 'log', 'title' = 'xxxxxxx'),
            yaxis = list('title' = 'xxxxxxxxxxxx'),
            margin = list('l' = 40, 'b' = 40, 't' = 10, 'r' = 10),
            legend = list('x' = 0, 'y' = 1),
            hovermode = 'closest'
          )
        )
      )
    )
  )
)

app$run_server()