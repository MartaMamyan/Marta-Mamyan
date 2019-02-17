df <- read.csv('air_quality_Nov2017.CSV')
library('ggplot2')

head(df)
df$PM10.Hour = gsub('h', '', df$PM10.Hour)
df <- na.omit(df)
df$PM10.Hour <- as.numeric(df$PM10.Hour) #change them into numeric to look better
ggplot(df, aes(x=PM10.Hour)) + geom_histogram(color='red')

hist = ggplot(df, aes(x=PM10.Hour)) + geom_bar(color='red') # with barchart looks better
hist

fliped_hist=hist + coord_flip() #changing the coordinate system
fliped_hist

F = fliped_hist + facet_grid(. ~ df$NO2.Quality) #change the facets, so we dvide the time based on the quality of air
F

Classic = F + theme_classic() #Give them a classic design
Classic

labeled = Classic + ggtitle("Time frequency based on air quality")
labeled

