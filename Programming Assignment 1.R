#First function draft
#Input data
directory <- 'C:/Users/edgar/Desktop/Cursos/R Programming/specdata/'
pollutant <- 'nitrate'
id <- 70:72

data <- c()
for(i in id){
  if(i<10){
    name <- paste('00',toString(i),'.csv', sep='')
  }else if(i<100){
    name <- paste('0',toString(i),'.csv', sep='')
  }else{
    name <- paste(toString(i),'.csv', sep='')
  }
  directory2 <- paste(directory, name, sep='')
  df <- read.csv(directory2)
  extract <-subset(df, select=c(pollutant))
  data <- c(data, extract[!is.na(extract)])
}
result <- mean(data)
result

#Polluntantmean function
pollutantmean <- function(directory, pollutant, id = 1:332){
  data <- c()
  for(i in id){
    if(i<10){
      name <- paste('00',toString(i),'.csv', sep='')
    }else if(i<100){
      name <- paste('0',toString(i),'.csv', sep='')
    }else{
      name <- paste(toString(i),'.csv', sep='')
    }
    directory2 <- paste(directory, name, sep='')
    df <- read.csv(directory2)
    extract <-subset(df, select=c(pollutant))
    data <- c(data, extract[!is.na(extract)])
  }
  result <- mean(data)
  result
}
#######################################################################
#Second function draft
#Input data
directory <- 'C:/Users/edgar/Desktop/Cursos/R Programming/specdata/'
id <- c(2, 4, 8, 10, 12)
nobs <- c()
for(i in id){
  if(i<10){
    name <- paste('00',toString(i),'.csv', sep='')
  }else if(i<100){
    name <- paste('0',toString(i),'.csv', sep='')
  }else{
    name <- paste(toString(i),'.csv', sep='')
  }
  directory2 <- paste(directory, name, sep='')
  df <- read.csv(directory2)
  df2 <- df[1:nrow(df),2:3]
  contador <- 0
  for(j in 1:nrow(df2)){
    if(!is.na(df2[j,1]) & !is.na(df2[j,2])){
      contador <- contador + 1
    }
  }
  nobs <- c(nobs, contador)
}
cbind(id,nobs)

#Complete function
complete <- function(directory, id = 1:332){
  nobs <- c()
  for(i in id){
    if(i<10){
      name <- paste('00',toString(i),'.csv', sep='')
      }else if(i<100){
        name <- paste('0',toString(i),'.csv', sep='')
      }else{
        name <- paste(toString(i),'.csv', sep='')
      }
    directory2 <- paste(directory, name, sep='')
    df <- read.csv(directory2)
    df2 <- df[1:nrow(df),2:3]
    contador <- 0
    for(j in 1:nrow(df2)){
      if(!is.na(df2[j,1]) & !is.na(df2[j,2])){
        contador <- contador + 1
      }
    }
    nobs <- c(nobs, contador)
  }
  return(cbind(id,nobs))
}

#######################################################################
#Third function draft
#Input data
directory <- 'C:/Users/edgar/Desktop/Cursos/R Programming/specdata/'
directory_ <- 'C:/Users/edgar/Desktop/Cursos/R Programming/specdata/002.csv'
total_obs <- complete(directory, id=1:332)
head(total_obs)
df_filter <- total_obs[total_obs[,2] > 150,]
id_filter <- df_filter[,1]

##Obtener los id con observaciones completas
df_t <- read.csv(directory_)
id_completos <- c()
for(j in 1:nrow(df_t)){
  if(!is.na(df_t[j,2]) & !is.na(df_t[j,3])){
  id_completos <- c(id_completos, j)
    }
}

df_t2 <- df_t[id_completos,]
cor(df_t2[,2], df_t2[,3])

#Corr function
corr <- function(directory, threshold = 0){
  df_filter <- total_obs[total_obs[,2] > threshold,]
  id_filter <- df_filter[,1]
  
  results <- c()
  for(i in id_filter){
    if(i<10){
      name <- paste('00',toString(i),'.csv', sep='')
    }else if(i<100){
      name <- paste('0',toString(i),'.csv', sep='')
    }else{
      name <- paste(toString(i),'.csv', sep='')
    }
    directory2 <- paste(directory, name, sep='')
    df <- read.csv(directory2)
    id_completos <- c()
    for(j in 1:nrow(df)){
      if(!is.na(df[j,2]) & !is.na(df[j,3])){
        id_completos <- c(id_completos, j)
      }
    }
    aux <- df[id_completos,]
    correlation <- cor(aux[,2], aux[,3])
    results <- c(results, correlation)
  }
  results
}

cr <- corr(directory, 150)
head(cr)
#######################################################################

#Quiz answers
#1
pollutantmean(directory, "sulfate", 1:10)
#2
pollutantmean(directory, "nitrate", 70:72)
#3
pollutantmean(directory, "sulfate", 34)
#4
pollutantmean(directory, "nitrate")
#5
cc <- complete(directory, c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)
#6
cc <- complete(directory, 54)
print(cc$nobs)
#7
RNGversion("3.5.1")  
set.seed(42)
cc <- complete(directory, 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])
#8
cr <- corr(directory)                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)
#9
cr <- corr(directory, 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)
#10
cr <- corr(directory, 2000)                
n <- length(cr)                
cr <- corr(directory, 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))