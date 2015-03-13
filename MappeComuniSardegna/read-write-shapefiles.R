#setRepositories(ind = c(1,6))
#install.packages('rgdal')
#install.packages('ggmap')

library(rgdal)     # R wrapper around GDAL/OGR
library(ggplot2)   # for general plotting
library(ggmap)    # for fortifying shapefiles
require("maptools")
require("plyr")

mrdf <- read.csv("/Users/matteo/Documents/Projects/R/MappeComuniSardegna/mr_ras_2013_finale.csv", colClasses = "character", sep=";")
#head(mrdf)
#names(mrdf)

#vector of columns that we are going to use in htis function
keepsCol <- c("patologia","comune_residenza")

#new dataframe considering just the selected columns
mrdf_subset = mrdf[keepsCol]

pop_2001_comuni = mrdf[c("comune_residenza","pop_2001")]

##modifico i nomi delle colonne perchè dopo mi servono per fare un join con un altro dataframe
colnames(pop_2001_comuni) <- c("nome","pop_2001")
#head(pop_2001_comuni)
#head(userBookmarkDF)

#head(mrdf_subset)

#table containing frequencies for each user and for each resources
freq_mr_comune <- table(mrdf_subset)  
#head(freq_mr_comune)
#barplot(head(sort(freq_mr_comune, decreasing = TRUE)),las=2)

frequenze_comune <- colSums(freq_mr_comune)
frequenze_comune <- as.data.frame(frequenze_comune)
frequenze_comune$nome <- rownames(frequenze_comune)

##frequenze_comune <- frequenze_comune[-1,] ##remove ****comune non trovato***
rownames(frequenze_comune) <- NULL
colnames(frequenze_comune) <- c("freq_comune", "nome")
head(frequenze_comune)
names(frequenze_comune)

##head(pop_2001_comuni)
pop_2001_comuni <- pop_2001_comuni[!duplicated(pop_2001_comuni), ]

##add column population2001 to dataframe
frequenze_comune = join(pop_2001_comuni, frequenze_comune, by="nome") ##freq comune is the measure I want to plot
#head(frequenze_comune)


##barplot(head(sort(frequenze_comune$freq_comune, decreasing = TRUE)),las=2)
############################
############################
##To sort a data frame in R, use the order( ) function. By default, sorting is ASCENDING. 
#Prepend the sorting variable by a minus sign to indicate DESCENDING order. Here are some examples.
#####################
#####################
#test <- frequenze_comune[order(-frequenze_comune[,1]),]  ##sort by freq_comune  that is the column 1

#head(test)

#test <- head(test, n=10) ##first n comuni with the highest rd patients

##barplot(test$freq_comune, names.arg=test$nome, horiz=TRUE,las=1)

#### start with maps



# First read in the shapefile, using the path to the shapefile and the shapefile name minus the
# extension as arguments
#shapefile <- readOGR('/Users/matteo/Documents/Projects/R/R20_11_ED50/', 'Com2011_ED50')#Italy
shapefile <- readOGR('/Users/matteo/Documents/Projects/R/MappeComuniSardegna/limitiAmministrComunali/', 'limitiAmministrComunali')

shapefile@data$id = rownames(shapefile@data)
shapefile.points = fortify(shapefile)
shapefile.df = join(shapefile.points, shapefile@data, by="id")
head(shapefile.df)
shapefile.df_mr = join(shapefile.df, frequenze_comune, by="nome") ##freq comune is the measure I want to plot

##shapefile.df_mr$prev = shapefile.df_mr$freq_comune / shapefile.df_mr$pop_2001



names(shapefile.df_mr)
head(shapefile.df_mr)
# Now the shapefile can be plotted as either a geom_path or a geom_polygon.
# Paths handle clipping better. Polygons can be filled.
# You need the aesthetics long, lat, and group.

#nclr <- 10
#plotclr <- brewer.pal(nclr,"BuPu")
#class <- classIntervals(plotvar, nclr, style="quantile")
#colcode <- findColours(class, plotclr)

#head(shapefile.df_mr)
#shapefile.df_mr[which(shapefile.df_mr$nome == "SANTA GIUSTA")[1],"freq_comune"]

map <- ggplot(data = shapefile.df_mr, aes(x = long, y = lat, group = group)) +
        geom_path(color = 'white', size = .4)

map <- map + geom_polygon(aes(fill=(freq_comune/as.numeric(pop_2001)))) 

###IF I WANT TO CHANGE COLOR
#map <- map + scale_fill_gradient(low = "#ffffcc", high = "#ff4444", 
#  space = "Lab", na.value = "grey50",
#  guide = "colourbar")

#map + scale_fill_gradient(name="Numero di Pazienti \n Percomune di residenza")
map <- map + scale_fill_gradient(name="Numero di Pazienti \n Percomune di residenza",
 trans = "log",breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 600) )

print(map) 


