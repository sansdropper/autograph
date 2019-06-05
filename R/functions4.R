#' Objective: Automated Univariate and Bivariate Plotting
#' @param df,data       [dataframe | Requiredl]Input the Data Frame in multiple functions.
#' @param y,r           [character | Optional] The Response variable in multiple Functions.Defaults to  NULL.
#' @param IDFeats       [character | Optional] A vector identifying ID features that will be removed for plotting purposes. Defaults to NULL.
#' @param uniCont       [character | Optional]Plot option for Univariate in Continuous Variable.Defaults to "histogram"
#' @param minCont       [Numerical | Optional]Minim number of unique values for numeric data to be univariate.Defaults to 20.
#' @param bins          [Numerical | Optional]Number of Bins to be used to plot.Defaulkts to 15.
#' @param MissThreshold [Numerical | Optional]Percentage Missing Value Threshold beyond which Variable will be removed.Defaults to 50.
#' @param ID            [Character | Optional]ID Features to be removed.Defaults to NULL.
#' @param capfloor      [logical | Optional] Features with outliers will be capped and floored,only apllies to continuous and discrete features. Defaults to FALSE.
#' @param CorThresh     [Numerical | Optional]Absolute value of Correlation above which Variables will be eliminated.Defaults to .9.
#' @param maxFact       [Numerical | Optional]Maximum number of Factors that a Category is allowed to have.Defaults to 25.
#' @param color         [character | Optional]Plot outline color.Sefaults to 'SteelBlue'
#' @param alpha         [Numerical | Optional]Transparency.Defaults to 0.5.
#' @param fill          [character | Optional]Plot Fill Color.Defaults to color.
#' @param DT            [character | Optional]Interim DataTable to showcase results.Defaults to NULL.
#' @return Univariate and Bivaraite Plots for Predictors and Response with a list of Plots made
#' @import data.table
#' @import tidyverse
#' @import caret
#' @import dlookr
#' @export

#Removing Special Characters and White Spaces from column Names
colclean<-function(df){sapply(colnames(df),function(x)gsub("[[:punct:][:blank:]]+", "",x))}


#Overview Function

overview<-function(data)
{
  # Removing Special Characters and White Spaces from column Names
  colnames(data)<-colclean(df=data)

  print(paste0("Glimpse of dataframe"))
  print(glimpse(data) )                         #Glimpse the Data



  print(paste0("Descriptive Statistics of Numerical Data"))
  print(describe(data))                  #Numerical Summary


  print(paste0("Missing and Unique Data Attributes"))
  print(diagnose(data))                  #NA and unique Count

}


# Format Function creation
format<-function(data,y=NULL,minCont=20)
{
  #Removing Special Characters and White Spaces from column Names
  colnames(data)<-colclean(df=data)

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

  return(data)
}

#Variable Selection Function Creation
varSelect<-function(data,y=NULL,MissThreshold=50,corthresh=.9,maxFact=25,ID=NULL)
{
  #Removing Special Characters and White Spaces from column Names
  colnames(data)<-colclean(df=data)

  #Removing ID Features
  if(is.null(ID)==FALSE)
  {data<-select(data,-ID)}

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
  ZeroVar<-caret::nearZeroVar(data)# Variables having very low variance

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

  #print(optimum)
  return(optimum)
}

# #Capping and Flooring

outclip <-function(data, y=NULL,capfloor=TRUE)

{#Removing Special Characters and White Spaces from column Names
  colnames(data)<-colclean(df=data)
   if(capfloor==TRUE)
      {
          if(is.null(y) == TRUE)
              {for(i in colnames(data))
                  {
                      if(class(data[[i]]) == "numeric")
                          {
                                Q3<-quantile(data[[i]],.99,type = 7,na.rm = TRUE)
                                Q1<-quantile(data[[i]],.01,type = 7,na.rm = TRUE)
                                data[[i]]<-sapply(data[[i]],function(x)ifelse(x < Q1,Q1,x))
                                data[[i]]<-sapply(data[[i]],function(x)ifelse(x > Q3,Q3,x))
                          }
                  }
              }
          else
              {for(i in colnames(data) )
                  {
                        if(class(data[[i]]) == "numeric" & i!=y)
                            {
                                Q3<-quantile(data[[i]],.99,type = 7,na.rm = TRUE)
                                Q1<-quantile(data[[i]],.01,type = 7,na.rm=TRUE)
                                data[[i]]<-sapply(data[[i]],function(x)ifelse(x < Q1,Q1,x))
                                data[[i]]<-sapply(data[[i]],function(x)ifelse(x > Q3,Q3,x))
                            }
                  }
              }
      }
      return(data)
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
  #print(DT)
  return(DT)
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
  #print(DT)
  return(DT)
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
  #print(DT)
  return(DT)
}

#4. Percentage Distribution
percFreq_X<-function(df,x,y=NULL,DT=NULL,nrBins=10,color="steelblue",alpha=.6)
    {

      if (class(df[[x]]) == "numeric")
         {  df[[x]]<-sapply(df[[x]],function(x)ifelse(is.na(x)==TRUE,0,x))
            df[,paste0(x,"bin"):= as.numeric(cut_interval(df[[x]],nrBins))]
            p<-ggplot(df, aes_string(x = paste0(x,"bin"))) +
            geom_bar(aes(y=(..count..)/sum(..count..)),alpha = alpha, fill = color, color = "gray") + labs(y="Percentage")+scale_y_continuous(labels=scales::percent)+ggtitle(paste0(x,"d Distribution"))+theme(axis.text.x = element_text(face="bold",size=6, angle=45,hjust=1))
            print(p)
            ggsave(paste0("Univariate_",x,".png"))
            #return(p)
            z<-data.table()
            z[,Type:="Univariate"][,Predictor:=x][,Response:=ifelse(is.null(y)==TRUE,"No Response",y)][,Plot:="Percentage Frequency"]
            DT<-rbind(DT,z)
            #print(DT)
            return(DT)
         }
    }

#5. Bar Graph   #Validated
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
  #print(DT)
  return(DT)
}

#6. Scatterplot with category-category # Not Validated
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
  #print(DT)
  return(DT)
}

#7.Boxplot  #Validated
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
  #print(DT)
  return(DT)
}

#8. Scatter Plot
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
  #print(DT)
  return(DT)
}

#Univariate Module Creation
univariate<-function(df,DT=NULL,bins=10,color = "steelblue",uniCont="histogram")
{ #Removing Special Characters and White Spaces from column Names
  colnames(df)<-colclean(df=df)

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
      else if(uniCont == "percFreq")#Done
      {DT<- unique(rbind(DT,percFreq_X(df=df,x=i,color = color,nrBins = bins,alpha = 0.5)))}
    }
  }

  return(unique(DT))
}


bivariate<-function(df,r=NULL,DT=NULL) #Remove null when integrating with main
{  #Removing Special Characters and White Spaces from column Names
  colnames(df)<-colclean(df=df)
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

  #print(unique(DT))
  return(unique(DT))
}

#Automatic Graphs
autoGraph<-function (df,
                     y=NULL,
                     uniCont ="histogram",    #Options: histogram,density,freqpoly,percFreq
                     minCont = 15,
                     bins = 20,
                     capfloor=TRUE,
                     MissThreshold=50,
                     corthresh =.9,
                     ID=NULL,
                     maxFact=25)

{
  # install/load packages  Module
  packs <- c("tidyverse","data.table","dlookr","caret")
  nopacks <- packs[!packs %in% rownames(installed.packages())]
  if(length(nopacks)>0) install.packages(nopacks , dependencies = T)
  for ( p in packs) library(p, character.only = T)
  rm(packs, nopacks, p)

  #Removing Special Characters and White Spaces from column Names
  colnames(df)<-colclean(df)

  #Creating Empty Dataframe
  DT<-data.table()

  #Data Overview    Module
  overview(data=df)

  #Formatting Module Creation

  df<-format(data=df,y=y,minCont=20)

  #Variable Selection Module
  print("Variables Selected")
  optimum<-varSelect(data=df,y,MissThreshold = MissThreshold,corthresh = corthresh,maxFact = maxFact,ID=ID)
  print(optimum)

  #Dataframe with optimum variables
  df<-select(df,optimum)

  #Outlier Clipping
  df<-Outclip(data=df, y=y,capfloor==capfloor)

  #Plots using univariate
  print("Univariate Data-Plotting")
  DT1<-univariate(df,bins=bins,color="steelblue",uniCont = uniCont)

  #Plots using bivariate
  print("Bivariate Data-Plotting Bivariates")
  DT2<-bivariate(df,r=y)

  #Combined
  DT3<-rbind(DT1,DT2)
  print(DT3)
  fwrite(DT3,"file.csv")
  return(df)
}

#Automatic Reporting on PDF
autoReport<-function (df,
                      y=NULL,
                      uniCont ="histogram",    #Options: histogram,density,freqpoly,percfreq
                      minCont = 15,
                      bins = 20,
                      capfloor=TRUE,
                      MissThreshold=50,
                      corthresh =.9,
                      ID=NULL,
                      maxFact=25)
{
  output_file <- "output.pdf"
  output_dir <-getwd()

  pathname<-paste0("rmd_template/report.Rmd")
  report_dir <- system.file(pathname, package = "autoGraph")

  suppressWarnings(render(input = report_dir, output_file = output_file,
                          output_dir = output_dir, intermediates_dir = output_dir,
                          params = list(df = df,
                                        y=y,
                                        minCont = minCont,
                                        bins = bins,
                                        capfloor = capfloor,
                                        MissThreshold = MissThreshold,
                                        corthresh = corthresh,
                                        ID = ID,
                                        maxFact = maxFact
                          )))



  report_path <- file.path(output_dir, output_file)
  system(paste0('open "', report_path, '"'))
}

