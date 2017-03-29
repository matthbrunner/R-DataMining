#-------------------------------------------------------------------------------
# Name:        Aufgabe_2_Clustering.R
# Purpose:     Aufgabe Datenaufbereitung
#
# Author:      Matthias Brunner
#
# Created:     28.03.2017
#-------------------------------------------------------------------------------


# Ausgabe 2 - Clustering -------------------------------------------------------
#  
# 1. Verwenden Sie das bereitgestellte cluster.r script aus der Vorlesung und 
# replizieren die Demonstration indem Sie die Daten vom UCI Machine Learning 
# Repository verwenden
#
# https://archive.ics.uci.edu/ml/datasets/Dataset+for+ADL+Recognition+with+Wrist-worn+Accelerometer#
# 

#set the working directory specific to my machine
setwd("C:/Workspace/Weiterbildung/05_DataMining/R-Projects/DataMining/Aufgabe2/HMP_Dataset")

#create a data frame from all files in specified folder
create_activity_dataframe = function(activityFolder,classId) {
  file_names = dir(activityFolder)
  file_names = lapply(file_names, function(x){ paste(".",activityFolder,x,sep = "/")})
  your_data_frame = do.call(rbind,lapply(file_names,function(x){read.csv(x,header = FALSE,sep = " ")}))
  your_data_frame = cbind(data.frame(rep(classId,nrow(your_data_frame))),your_data_frame)
  your_data_frame = cbind(data.frame(1:nrow(your_data_frame)),your_data_frame)
  colnames(your_data_frame) = c("timestep","class","x","y","z")
  your_data_frame
}
df1 = create_activity_dataframe("Brush_teeth",1)
View(df1)
library(ggplot2)
df1_sample = df1[sample(nrow(df1), 500), ]
ggplot(df1_sample, aes(timestep)) + 
  geom_line(aes(y = x, colour = "x")) + 
  geom_line(aes(y = y, colour = "y")) + 
  geom_line(aes(y = z, colour = "z"))

df2 = create_activity_dataframe("Climb_stairs",2)
df = rbind(df1,df2)
View(df)

write.csv(df,"dsx_movement_pattern.csv")


# Determine number of clusters
determine_number_of_clusters = function(df) {
  wss <- (nrow(df)-1)*sum(apply(df,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(df,
                                       centers=i)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares") 
}

number_of_clusters=2
n = nrow(df)

kmeans(df,centers=number_of_clusters)$centers

df_x_y = cbind(df$x,df$y,df$z)
determine_number_of_clusters(df_x_y)
km = kmeans(df_x_y,centers=number_of_clusters)


truthVector = km$cluster == df$class
good = length(truthVector[truthVector==TRUE])
bad = length(truthVector[truthVector==FALSE])
good/(good+bad)

library(scatterplot3d)
df_sample = df[sample(nrow(df), 1000), ]
with(df_sample, {
  scatterplot3d(x,y,z, color = class)
})

with(data.frame(km$centers), {
  scatterplot3d(x,y,z)
})

centers_df = km$centers
colnames(centers_df) = c("x","y","z")
with(data.frame(centers_df), {
  scatterplot3d(x,y,z)
})

# 
# 2. Versuchen Sie durch generieren zusätzlicher "Features" die Qualität des Clusterings zu erhönen
# 
# Hinweis: http://www.duchess-france.org/analyze-accelerometer-data-with-apache-spark-and-mllib/ (Abschnitt DETERMINE AND COMPUTE FEATURES FOR THE MODEL)

# Average acceleration (for each axis)
# Variance (for each axis)
# Average absolute difference (for each axis)
# Average resultant acceleration (1/n * sum [???(x² + y² + z²)])
# Average time between peaks (max) (Y-axis)

# Berechnen der Mittelwerte, der Varianzen ...
library(dplyr)
df_sample_average_acceleration <- df %>%
  group_by(class) %>%
  mutate(x.mean = mean(x),
         y.mean = mean(y),
         z.mean = mean(z),
         x.var = var(x),
         y.var = var(y),
         z.var = var(z),
         x.abs = abs(x - mean(x)),
         y.abs = abs(y - mean(y)),
         z.abs = abs(z - mean(z))) %>%
  ungroup()

# Betrachten des Data Frames
head(df_sample_average_acceleration)
# Ausgabe der Anzahl Elemente in Data Frame.
nrow(df_sample_average_acceleration)


# Erstellen eines neuen Data Frame mit der x, y, z und den Mittelwerten von x, y und z.
df_x_y_z_mean.x_mean.y_mean.z <- cbind(df_sample_average_acceleration$x,
                                      df_sample_average_acceleration$y,
                                      df_sample_average_acceleration$z,
                                      df_sample_average_acceleration$x.mean,
                                      df_sample_average_acceleration$y.mean,
                                      df_sample_average_acceleration$z.mean)

# Ausgabe des clusters Graphen
determine_number_of_clusters(df_x_y_z_mean.x_mean.y_mean.z)
# Berechnen des kmeans!
km <- kmeans(df_x_y_z_mean.x_mean.y_mean.z, centers=number_of_clusters)
nrow(df)
nrow(df_x_y_z_mean.x_mean.y_mean.z)

# da bei km$cluster die definition zufällig ist muss hier ev. == oder != verwendet werden!
truthVector <- km$cluster == df$class
good = length(truthVector[truthVector==TRUE])
bad = length(truthVector[truthVector==FALSE])
good/(good+bad)

# Ausgabe [1] 0.9036911
# Die Qualität des Clusters konnte angehoben werden.
