#' Objective: Automated Univariate and Bivariate Plotting
#' @param df           : Input the Data Frame
#' @param y            :The Response variable
#' @param uniCont      : Plot option for Univariate in Continuous Variable
#' @param minCont      : Minim number of unique values for numeric data to be univariate
#' @param bins         : Number of Bins to be used to plot
#' @param MissThreshold: Percentage Missing Value Threshold beyond which Variable will be removed
#' @param CorThresh    : Absolute value of Correlation above which Variables will be eliminated
#' @return Univariate and Bivaraite Plots for Predictors and Response with a list of Plots made
#' @import data.table
#' @import tidyverse
#' @import caret
#' @import dlookr
#' @export

#Overview Function

overview<-function(data)
{
  # #Removing Special Characters and White Spaces from column Names
  # colnames(df)<-sapply(colnames(data),function(x)gsub("[[:punct:][:blank:]]+", "",x))

  print(paste0("Glimpse of dataframe"))
  glimpse(data)                          #Glimpse the Data
  print(paste0("Descriptive Statistics of Numerical Data"))
  print(describe(data))                  #Numerical Summary
  print(paste0("Missing and Unique Data Attributes"))
  print(diagnose(data))                  #NA and unique Count
}


# Format Function creation
format<-function(data,y=NULL,minCont=20)
{
  # #Removing Special Characters and White Spaces from column Names
  # colnames(df)<-sapply(colnames(df),function(x)gsub("[[:punct:][:blank:]]+", "",x))

  if(is.null(y) == TRUE)
  {
    for(i in colnames(data))
    {
      if(class(data[[i]]) %in% c("integer","numeric") & length(unique(data[[i]])) > minCont)
      {data[[i]] <- as.numeric(data[[i]])}

      else if(class(data[[i]]) %in% c("integer","numeric") & length(unique(data[[i]])) <= minCont)
      {data[[i]] <- as.factor(as.character(data[[i]]))}

      else
      {data[[i]] <- as.factor(as.character(data[[i]]))}
    }
  }
  else
  {
    for(i in colnames(data))
    {
      if(class(data[[i]]) %in% c("integer","numeric") & length(unique(data[[i]])) > minCont & i != y)
      {data[[i]] <- as.numeric(data[[i]])}

      else if(class(data[[i]]) %in% c("integer","numeric") & length(unique(data[[i]])) <= minCont & i != y)
      {df[[i]] <- as.factor(as.character(data[[i]]))}

      else if(class(data[[i]]) %in% c("factor","character") & i!= y)
      {data[[i]] <- as.factor(as.character(data[[i]]))}
    }
  }

}

#Variable Selection Function Creation
varSelect<-function(data,y=NULL,MissThreshold=50,corthresh=.9,maxFact=25)
{
  # #Removing Special Characters and White Spaces from column Names
  # colnames(df)<-sapply(colnames(df),function(x)gsub("[[:punct:][:blank:]]+", "",x))

  #Removal Category Variables with High Number of Levels

  HighLevel<-c()

  for(i in colnames(data))
  {
    if(class(data[[i]]) %in% c("factor","character") & length(unique(data[[i]])) >= maxFact)
    {HighLevel<-c(HighLevel,i)}
  }

  if(!(is.null(HighLevel)==TRUE))
  {data<-select(data,-HighLevel)}

  #NearZeroVar Removal
  ZeroVar<-nearZeroVar(data)# Variables having very low variance

  data<-select(data,-ZeroVar)

  #High NA Variables Removal
  B<-data.table(colnames(data),colSums(is.na(data)))
  colnames(B)<-c("Names","Missing")
  HighNA<-B[,PercMiss:=(Missing*100/nrow(data))][,Missing:=NULL][PercMiss>MissThreshold,Names]

  data<-select(data,-HighNA)

  #Correlation Matrix
  num<-vector()
  for(i in colnames(data))
  {
    if(class(data[[i]]) %in% c("numeric","integer","double"))
    {num<-c(num,i)}
  }
  data.num<-select(data,num)

  #High Correlated Variables Identification
  HighCor<-findCorrelation(cor(data.num,use ="complete.obs"),cutoff=corthresh,names=TRUE,exact = TRUE)
  data<-select(data,-HighCor)

  #Final
  if(is.null(y)==TRUE)
  {optimum<-unique(colnames(data))}
  else
  {optimum<-unique(c(colnames(data),y))}

  print(optimum)
}

# Plotting Functions

#1. freqpoly   # Needs a single continuous variable thats divided to bins,shows with lines  #validated
freqpoly_X<- function(data,x,y=NULL,DT=NULL,color = "steelblue",nrBins = 15,alpha = 0.5)
{
  p <- ggplot(data , aes_string(x = x)) +
    geom_freqpoly(bins=nrBins,stat="bin",position="identity",na.rm=TRUE,alpha=alpha, color = color) +
    ggtitle(paste0("Univariate Freqpoly Distributio : ",x)) +
    labs(x = x, y = "Frequency") +  theme(legend.title=element_blank())


  print(p)
  ggsave(paste0("Univariate_",x,".png"))
  #return(p)
  z<-data.table()
  z[,Type:="Univariate"][,Predictor:=x][,Response:=ifelse(is.null(y)==TRUE,"No Response",y)][,Plot:="FreqPlot"]
  DT<-rbind(DT,z)
  print(DT)
}

#2. Histogram # Needs a single continuous variable thats divided to bins,shows with bars #Validated
histogram_X <- function(data,x,y=NULL,DT=NULL,color = "steelblue",nrBins = 15,alpha = 0.5)
{
  p <- ggplot(data , aes_string(x=x)) +
    geom_histogram(bins = nrBins, alpha = alpha, fill = color, color = "gray") +
    ggtitle(paste0("Univariate Histogram Distribution : ",x)) +
    labs(x = x, y = "Count") +
    theme(legend.title=element_blank())


  print(p)
  ggsave(paste0("Univariate_",x,".png"))
  #return(p)
  z<-data.table()
  z[,Type:="Univariate"][,Predictor:=x][,Response:=ifelse(is.null(y)==TRUE,"No Response",y)][,Plot:="Histogram"]
  DT<-rbind(DT,z)
  print(DT)
}

#3. density #If Continuous variable has underlying smooth doistribution  #Validated
density_X <- function(data,x,y=NULL,DT=NULL,color = "steelblue",alpha = 0.5)
{
  p <- ggplot(data , aes_string(x = x)) +
    geom_density(alpha = alpha, fill = color, color = color) +
    ggtitle(paste0("Univariate Density Distribution: ",x)) +
    labs(x = x, y = "Density") +
    theme(legend.title=element_blank())


  print(p)
  ggsave(paste0("Univariate_",x,".png"))
  #return(p)
  z<-data.table()
  z[,Type:="Univariate"][,Predictor:=x][,Response:=ifelse(is.null(y)==TRUE,"No Response",y)][,Plot:="Density"]
  DT<-rbind(DT,z)
  print(DT)
}

#4. Bar Graph   #Validated
bar_X <- function(data,x,y=NULL,DT=NULL,color = "steelblue")
{
  p <- ggplot(data , aes_string(x = x)) +
    geom_bar(fill = "gray", color = color) +
    ggtitle(paste0("Univariate Bar Distribution: ",x)) +
    labs(x = x, y = "Count") +
    theme(legend.title=element_blank())


  print(p)
  ggsave(paste0("Univariate_",x,".png"))
  #return(p)
  z<-data.table()
  z[,Type:="Univariate"][,Predictor:=x][,Response:=ifelse(is.null(y)==TRUE,"No Response",y)][,Plot:="Bar"]
  DT<-rbind(DT,z)
  print(DT)
}

#5. Scatterplot with category-category # Not Validated
scatterplot_cat <- function(data, x, y,DT=NULL,alpha = 0.5)
{
  p <- ggplot(data , aes_string(x = x, y = y, color = y)) +
    geom_count( ) +
    labs(x = x, y = y) +
    ggtitle(paste0("Bivariate Scatter Cat Distribution: ",y," By ",x)) +
    guides(fill = FALSE, colour = FALSE)

  print(p)
  ggsave(paste0("Bivariate_",x,".png"))
  #return(p)
  z<-data.table()
  z[,Type:="Bivariate"][,Predictor:=x][,Response:=ifelse(is.null(y)==TRUE,"No Response",y)][,Plot:="ScatterCat"]
  DT<-rbind(DT,z)
  print(DT)
}

#6.Boxplot  #Validated
boxplot_XY <- function(data, x, y,DT=NULL,alpha = 0.5)
{
  p <- ggplot(data, aes_string(x = y, y = x, color = y)) +
    geom_boxplot(lwd = 1,outlier.colour = "black", outlier.shape = 16, outlier.size = 2, alpha = alpha) +
    labs(x = y, y = x) +
    ggtitle(paste0("Bivariate Boxplot Distribution: ",x," By ",y)) +
    guides(fill = FALSE, colour = FALSE) +
    stat_summary(fun.y = mean, geom="point",colour="gray43", size=3)

  print(p)
  ggsave(paste0("Bivariate_",x,".png"))
  #return(p)
  z<-data.table()
  z[,Type:="Bivariate"][,Predictor:=y][,Response:=ifelse(is.null(x)==TRUE,"No Response",x)][,Plot:="BoxPlot"]
  DT<-rbind(DT,z)
  print(DT)
}

#7. Scatter Plot
scatterplot_cont <- function(data, x, y,DT=NULL,alpha = 0.5)
{
  p <- ggplot(data , aes_string(x = x, y = y, color = y)) +
    geom_point( ) +
    labs(x = x, y = y) +
    ggtitle(paste0("Bivariate Scatter Distribution: ",y," By ",x)) +
    guides(fill = FALSE, colour = FALSE)

  print(p)
  ggsave(paste0("Bivariate_",x,".png"))
  #return(p)
  z<-data.table()
  z[,Type:="Bivariate"][,Predictor:=x][,Response:=ifelse(is.null(y)==TRUE,"No Response",y)][,Plot:="ScatterCont"]
  DT<-rbind(DT,z)
  print(DT)
}

#Univariate Module Creation
univariate<-function(df,DT=NULL,bins=10,color = "steelblue",uniCont="histogram")
{  #Removing Special Characters and White Spaces from column Names
    colnames(df)<-sapply(colnames(df),function(x)gsub("[[:punct:][:blank:]]+", "",x))

  for(i in colnames(df))
  {
    if(class(df[[i]]) %in% c("character","factor"))
    {DT<- unique(rbind(DT,bar_X(data=df,x=i,color=color)))}
    else if(class(df[[i]]) %in% c("integer","numeric"))
    {
      if(uniCont == "histogram")
      {DT<- unique(rbind(DT,histogram_X(data=df,x=i,color = color,nrBins = bins,alpha = 0.5)))}
      else if(uniCont == "density")
      { DT<- unique(rbind(DT,density_X(data=df,x=i,color = color,alpha = 0.5)))}#alpha-transparency, theme = theme
      else if(uniCont == "freqpoly")#Done
      {DT<- unique(rbind(DT,freqpoly_X(data=df,x=i,color = color,nrBins = bins,alpha = 0.5)))}
    }
  }
  print(unique(DT))
}


bivariate<-function(df,r=NULL,DT=NULL) #Remove null when integrating with main
{  #Removing Special Characters and White Spaces from column Names
    colnames(df)<-sapply(colnames(df),function(x)gsub("[[:punct:][:blank:]]+", "",x))
  if(is.null(r)==TRUE)              # Y not provided
  {print(paste0("Response Variable Unavailable"))}
  else
  {
    if(class(df[[r]]) %in% c("character","factor") )
    {
      for(i in colnames(df))
      {
        if(class(df[[i]]) %in% c("character","factor") & i != r)     #Cat Cat
        {DT<-unique(rbind(DT,scatterplot_cat(data=df, x=i, y=r,alpha = 0.5)))}
        else if(class(df[[i]]) %in% c("numeric","integer","double") & i != r) #Cat Met
        {DT <- unique(rbind(DT,boxplot_XY (data=df, x=i, y=r,alpha = 0.5)))}
      }
      #print(unique(DT))
    }

    if (class(df[[r]]) %in% c("numeric","integer","double"))
    {
      for(i in colnames(df))
      {
        if(class(df[[i]]) %in% c("character","factor") & i != r)     # Met-Cat
        {DT<- unique(rbind(DT,boxplot_XY (data=df, x=r, y=i,alpha = 0.5)))}
        else if(class(df[[i]]) %in% c("numeric","integer","double") & i != r)   # Met-Met
        {DT<- unique(rbind(DT,scatterplot_cont(data=df, x=i, y=r,alpha = 0.5)))}
      }
    }
    #print(unique(DT))
  }

  print(unique(DT))
}

#Automatic Graphs
autoGraph<-function (df,
                     y=NULL,
                     uniCont ="histogram",    #Options: histogram,density,freqpoly
                     minCont = 15,
                     bins = 20,
                     MissThreshold=50,
                     corthresh =.9,
                     maxFact=25)

{
  # install/load packages  Module
  packs <- c("tidyverse","data.table","dlookr","caret")
  nopacks <- packs[!packs %in% rownames(installed.packages())]
  if(length(nopacks)>0) install.packages(nopacks , dependencies = T)
  for ( p in packs) library(p, character.only = T)
  rm(packs, nopacks, p)

  #Removing Special Characters and White Spaces from column Names
  colnames(df)<-sapply(colnames(df),function(x)gsub("[[:punct:][:blank:]]+", "",x))

  #Creating Empty Dataframe
  DT<-data.table()

  #Data Overview    Module
  overview(data=df)

  #Formatting Module Creation
  format(data=df,y=y,minCont=20)

  #Variable Selection Module
  print("Variables Selected")
  optimum<-varSelect(data=df,y,MissThreshold = MissThreshold,corthresh = corthresh,maxFact = maxFact)

  #Dataframe with optimum variables
  df<-select(df,optimum)

  #Plots using univariate
  print("Univariate Data")
  DT1<-univariate(df,bins=bins,color="steelblue",uniCont = uniCont)

  #Plots using bivariate
  print("Bivariate Data")
  DT2<-bivariate(df,r=y)

  #Combined
  DT3<-rbind(DT1,DT2)
  print(DT3)
  fwrite(DT3,"file.csv")
 }


