#' Importing Excel Document Function
#'
#' This Function converts multi sheet excel doc to single data-frame with m/z,intensity, 
#' @param .xls file
#' @keywords import
#' @export
#' @examples
#' importWorksheets() 

#load XLConnect package to read in Excel files
importWorksheets <- function(file) {
  # filename: name of Excel file
  workbook <- loadWorkbook(file)
  sheet_names <- getSheets(workbook)
  names(sheet_names) <- sheet_names
  sheet_list <- lapply(sheet_names, function(.sheet){
    readWorksheet(object=workbook, .sheet)})
}



#'This function converts all of the sheets from an excel file as a single list and returns a list called spectra.list containing all sheets in excel file to global enironment it also returns sheet names so user can create dataframes containing peak lists of interest
#'@param file 
#'@keywords spectra
#'@export
#'@examples
#'Return.spectra.list()

Return.spectra.list<-function(file){
  spectra.list<-importWorksheets(file)
  spectra.list<-as.vector(spectra.list)
  spectra.names<<-names(spectra.list)
  return(spectra.names)
}


#' This function formats spectra data so that dataframe contains only m/z time and intensity
#'@param dataframe, sample, feature
#'@keywords format spectra
#'@examples
#' format.spectra.df()
#takes dataframe and sample name (can be different than dataframe name)
#as argument and deletes unnecessary info

format.spectra.df<-function(data.frame, sample, feature){
  data.frame<-data.frame[ ,-c(4:7,9:11)] #remove columns that aren't needed
  data.frame<-data.frame[-c(1:2), ] #remove rows that aren't needed
  names(data.frame)<-NULL #delete the variable names since they are wrong
  colnames(data.frame)<-c("m/z","time","intensity","rel.intensity") #rename
  data.frame$sample<-sample #make put the sample name in the sample column
  data.frame$feature<-feature #add the ribotype or other feature
  write.table(data.frame,file=(paste0(sample, ".txt"))) #write this to a txt file in working directory
}

#


#'This function uses elements of MALDIquant to baseline subtract from m.z intensity values and create MassSpectrum object from dataframe
#'@param dataframe name
#'@keywords baseline
#'@export
#'@examples
#' baseline.sub.ms()

baseline.sub.ms<-function(df, name){
  MSo<-createMassSpectrum(df$m.z, df$intensity, metaData=list(name=paste0(name, " Spectrum"))) 
  # transform intensity to remove variance from the mean
  MSo1<- transformIntensity(MSo, method="sqrt")
  #smooth intensity to smooth the spectra
  MSo2 <- smoothIntensity(MSo1, method="MovingAverage")
  #estimate the baseline noise
  baseline<- estimateBaseline(MSo2, method="SNIP",iterations=150)
  #plot before correction and where the baseline is
  plot(MSo2)
  lines(baseline, col="red", lwd=2)
  #remove the baseline and save this as a global variable
  MSo3<<-removeBaseline(MSo2, method="SNIP",iterations=150)
  #plot resulting spectra
  plot(MSo3)  
}

#'This function exports MassSpectrum class objects to a .csv file
#'@param MassSpectrum-class file
#'@keywords export
#'@export 
#'@examples
#' export.msO()
##arguments are a massspectrum class object and a file to export to

export.msO<-function(msobject, file){
  export(msobject, file=paste0(file,".csv"))
}

#'This function plots relative intensity by m.z values
#'@param dataframe
#'@keywords plot
#'@export 
#'@examples
#' plot.spectra.rel.intens()
plot.spectra.rel.intens<-function(df=data.frame) {
  plot(df$rel.intensity~df$m.z, type="l", main="Relative Intensity by M/Z", xlab="M/Z", ylab="Relative Intensity" , xlim=c(2000,13000), ylim=c(0,1),col="black")
}

#'This function plots intensity by m.z values
#'@param dataframe
#'@keywords plot
#'@export 
#'@examples
#' Plot.spectra.intens()

Plot.spectra.intens<-function(df=data.frame) {
  plot(df$intensity~df$m.z, type="l", main="Intensity by M/Z", xlab="M/Z", ylab="Intensity" , xlim=c(2000,13000), ylim=c(0,15000),col="black")
}