
#functions for converting RGB and
#calculating green cover
GLI <- function(img,i,j,k){
  r<-getValues(img[[i]])
  g<-getValues(img[[j]])
  b<-getValues(img[[k]])
  sumras<-r+g+b
  r1<-r/sumras;
  g1<-g/sumras;
  b1<-b/sumras;
  GLI <- (2*g1-b1-r1)/(2*g1+b1+r1);
  GLI_ras<-img[[1]];
  values(GLI_ras)<-GLI
  return(GLI_ras);
}
VARI <-  function(img,i,j,k){
  r<-getValues(img[[i]])
  g<-getValues(img[[j]])
  b<-getValues(img[[k]])
  sumras<-r+g+b
  r1<-r/sumras;
  g1<-g/sumras;
  b1<-b/sumras;
  VARI <- (g1-r1)/(g1+r1-b1);
  VARI_ras<-img[[1]];
  values(VARI_ras)<-VARI
  return(VARI_ras);
}

senteraNDVI <-  function(img,i,j,k){
  r<-getValues(img[[i]])
  g<-getValues(img[[j]])
  b<-getValues(img[[k]])
  NDVI <- (1.236*b-0.188*r)/(b+0.044*r);
  NDVI_ras<-img[[1]];
  values(NDVI_ras)<-NDVI
  return(NDVI_ras);
}


pctgreen <- function(files,dates,threshold){
  
  # Create a Progress object
  progress <- shiny::Progress$new()
  progress$set(message = "Analysing", value = 0)
  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress$close())
  
  pctg <- matrix(ncol = 2, nrow = length(files))
  
  for (i in seq_along(files)){
    
    progress$inc(1/seq_along(files), detail = paste("image", i))
   
    myraster<-brick(files[i])
    
    pctgreen1=(sum(getValues(myraster>threshold),na.rm = TRUE)/ncell(myraster))*100
    
    pctg[i,]<-c(pctgreen1,dates[i])
    
  }
  pctg<-as.data.frame(pctg)
  names(pctg)<-c("pct","date")
  pctg$date<-as.Date(pctg$date,origin="1970-01-01")
  names(pctg)<-c("pct","date")
  return(pctg);
  
}

pctgreenNDVI <- function(files,dates,threshold){
  
  # Create a Progress object
  progress <- shiny::Progress$new()
  progress$set(message = "Analysing", value = 0)
  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress$close())
  
  pctg <- matrix(ncol = 2, nrow = length(files))
  
  for (i in seq_along(files)){
    
    progress$inc(1/seq_along(files), detail = paste("image", i))
    
    myraster<-brick(files[i])
    
    pctgreen1=(cellStats(myraster>threshold,'sum')/ncell(myraster))*100
    
    pctg[i,]<-c(pctgreen1,dates[i])
    
  }
  pctg<-as.data.frame(pctg)
  names(pctg)<-c("pct","date")
  pctg$date<-as.Date(pctg$date,origin="1970-01-01")
  names(pctg)<-c("pct","date")
  return(pctg);
  
}


##statistics of raster pixels above given threshold##
thrshav <- function(files,dates,threshold,stat){
  
  pctg <- matrix(ncol = 2, nrow = length(files))
  
  for (i in seq_along(files)){
    
    myraster<-brick(files[i])
    myraster[myraster<threshold]<- 0
    av_abv_t=cellStats(myraster, stat)
    
    pctg[i,]<-c(av_abv_t,dates[i])
    
  }
  pctg<-as.data.frame(pctg)
  names(pctg)<-c("mean","date")
  pctg$date<-as.Date(pctg$date,origin="1970-01-01")
  
  
  return(pctg);
  
}

shiny_busy <- function() {
  # use &nbsp; for some alignment, if needed
  HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", paste0(
    '<span data-display-if="',
    '$(&#39;html&#39;).attr(&#39;class&#39;)==&#39;shiny-busy&#39;',
    '">',
    '<i class="fa fa-spinner fa-pulse fa-fw" style="color:orange"></i>',
    '</span>'
  ))
}