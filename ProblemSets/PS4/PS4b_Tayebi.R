SparkR

df1 <-(iris)


df <-createDataFrame(df1)

class(df1)

class(df)

head(select(df, df$Sepal_Length, df$Species))
 
head(select(df1, df1$Sepal_Length, df1$Species))

head(filter(df, df$Sepal_Length>5.5))

head(filter(df1, df1$Sepal_Length>5.5))

head(select(filter(df, df$Sepal_Length>5.5)df$Sepal_Length, df$Species))


df2 <-head(summarize(groupBy(df, df$Species)mean=mean(df$Sepal_Length) count=n(df$Sepal_Length)))


head(arrange(df2, asc(df2$Species)))

