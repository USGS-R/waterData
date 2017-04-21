#' Function to import daily hydrologic time series 
#' data given a USGS streamgage identification number.
#'
#' This function will import data from a WaterML2 service (current USGS 
#' hydrological data standard).  It will retrieve daily streamflow and 
#' continuous water-quality data from the USGS Daily Values Site Web 
#' Service \url{https://waterservices.usgs.gov/rest/DV-Service.html}
#' (U.S. Geological Survey, 2017d).
#'
#' @name importDVs
#' @title Imports daily USGS hydrologic times series data
#' @param staid is the USGS site identification number,  
#' usually eight digits long, but can be longer.  Users may search for 
#' surface-water sites and obtain station identification numbers using the 
#' USGS Site Web Service, 
#' \url{https://waterservices.usgs.gov/rest/Site-Service.html} (USGS, 2017e); 
#' using the National Water Information System: Mapper, 
#' \url{https://maps.waterdata.usgs.gov/mapper/index.html} (U.S. Geological Survey, 2017a); 
#' or using the National Water Information System: Web Interface to daily 
#' surface-water data, 
#' \url{https://waterdata.usgs.gov/nwis/dv/?referred_module=sw} 
#' (U.S. Geological Survey, 2012f).  The site identification number needs to 
#' be entered as a character, that is in quotes, because many USGS 
#' streamgage numbers begin with zero and the leading zero is necessary.
#' @param code is the USGS parameter code, a 5-digit number 
#' used in the USGS computerized data system, National Water 
#' Information System (NWIS), to uniquely identify a specific hydrologic 
#' property or constituent.  A list of paramater codes is available at 
#' \url{https://nwis.waterdata.usgs.gov/usa/nwis/pmcodes} (U.S. Geological Survey, 2017b).
#' @param stat is the USGS statistics code, a 5-digit number 
#' used in the USGS computerized data system, National Water 
#' Information System (NWIS), to uniquely identify specific statistics, such
#' as daily mean, daily maximum, and daily minimum.  The default,  
#' 00003,  is the mean daily value.  A list of statistics codes is available at 
#' \url{https://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table} 
#' (U.S. Geological Survey, 2017c).
#' Not all statistics are available at every gage.
#' @param sdate is the start date of the time series, in the format 
#' yyyy-mm-dd, optional.
#' @param edate is the end date of the time series, in the format yyyy-mm-dd, 
#' optional.
#' @return a data frame containing daily streamflow or other hydrologic data 
#' for the site specified during the dates specified (inclusive).  The USGS 
#' parameter code, code, and the statistics code, stat, are attributes of the
#' data frame.
#' @references 
#' U.S. Geological Survey, 2017a, National Water Information System: Mapper, 
#' accessed January 3, 2017, at 
#' \url{https://maps.waterdata.usgs.gov/mapper/index.html}.
#' 
#' U.S. Geological Survey, 2017b, Parameter code definition, 
#' National Water Information System: Web Interface, accessed January 3, 
#' 2017, at \url{https://nwis.waterdata.usgs.gov/usa/nwis/pmcodes}.
#' 
#' U.S. Geological Survey, 2017c, Stat codes (stat_cd), 
#' National Water Information System: Web Interface, accessed January 3, 
#' 2017, at 
#' \url{https://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}.
#' 
#' U.S. Geological Survey, 2017d, USGS daily values site web service: 
#' REST Web Services, accessed January 3, 2017, at 
#' \url{https://waterservices.usgs.gov/rest/DV-Service.html}.
#' 
#' U.S. Geological Survey, 2017e, USGS site web service: 
#' REST Web Services, accessed January 3, 2017, at 
#' \url{https://waterservices.usgs.gov/rest/Site-Service.html}.
#' 
#' U.S. Geological Survey, 2017f, USGS surface-water daily data for the Nation: 
#' National Water Information System: Web Interface, accessed January 3, 
#' 2017, at \url{http://waterdata.usgs.gov/nwis/dv/?referred_module=sw}.
#' @keywords ts IO
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_find_first
#' @importFrom xml2 xml_text
#' @importFrom xml2 xml_attr
#' @importFrom lubridate parse_date_time
#' @importFrom dataRetrieval getWebServiceData
#' @export
#' @format The returned data frame has the following columns \cr
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr 
#' staid \tab factor \tab USGS station identification number \cr
#' val \tab numeric \tab The value of the hydrologic variable \cr
#' dates \tab Date \tab Date of daily value \cr
#' qualcode \tab factor \tab Qualification code
#' }
#' @examples
#' # import mean daily streamflow for Red River of the North at Fargo, ND
#' q05054000 <- importDVs("05054000", sdate="2000-01-01", edate="2010-12-31")
#' head(q05054000)
#' # additional examples of how to this function follow
#' # import mean daily gage height for Red River of the North at Grand Forks, ND
#' gh05082500 <- importDVs("05082500", code="00065", sdate="2000-01-01", edate="2010-12-31")
#' # import mean daily specific conductance for Red River of the North at Grand Forks, ND
#' sc05082500<- importDVs("05082500", code="00095", sdate="2000-01-01", edate="2010-12-31")
#' # import mean daily water temperature for Red River of the North at Fargo, ND
#' temp05054000<- importDVs("05054000", code="00010", sdate="2000-01-01", edate="2010-12-31")
#' # import median daily pH for Red River of the North at Fargo, ND
#' pH05054000<- importDVs("05054000", code="00400", stat="00008", 
#' sdate="2000-01-01", edate="2010-12-31")
#' # examine the attributes of the data frame to show that the parameter code 
#' # and statistics code are saved with the data frame
#' attributes(pH05054000)[c("code","stat")]
#' # import mean daily oxygen for Red River of the North at Fargo, ND
#' do05054000 <- importDVs("05054000", code="00300", sdate="2000-01-01", edate="2010-12-31")
#' # import mean daily turbidity for Red River of the North at Fargo, ND
#' turb05054000 <- importDVs("05054000", code="63680", sdate="2000-01-01", edate="2010-12-31")
importDVs <- function(staid, code="00060", stat="00003", sdate="1851-01-01", 
                      edate=as.Date(Sys.Date(), format="%Y-%m-%d")) {
  if (is.character(staid) == FALSE ) stop("staid needs to have quotes around it")
  if (nchar(staid) < 8) stop ("staid must be at least 8 characters")
  base_url <- "https://waterservices.usgs.gov/nwis/dv/?format=waterml,2.0"
  url <- paste(base_url, "&site=", staid, "&parameterCd=", code, "&statCd=", 
               stat, sep = "")
  url <- paste(url, "&startDt=", sdate, "&endDt=", edate, sep="")
  
  # modified from importWaterML2 function of package dataRetrieval version 2.6.3
  asDateTime <- TRUE
  raw <- FALSE
  if (class(url) == "character" && file.exists(url)) {
    returnedDoc <- read_xml(url)
  } else if(class(url) == 'raw') {
    returnedDoc <- read_xml(url)
    raw <- TRUE
  } else {
    returnedDoc <- xml_root(getWebServiceData(url, encoding = 'gzip'))
  }
  
  timeSeries <- xml_find_all(returnedDoc, "//wml2:Collection") # each parameter/site combo
  
  if (0 == length(timeSeries)) {
    df <- data.frame()
    if (!raw) {
      attr(df, "url") <- url
    }
    return(df)
  }
  
  TVP <- xml_find_all(timeSeries, ".//wml2:MeasurementTVP") #time-value pairs
  time <- xml_text(xml_find_all(TVP, ".//wml2:time"))

  if (asDateTime) {
    time <- parse_date_time(time, c("%Y","%Y-%m-%d","%Y-%m-%dT%H:%M","%Y-%m-%dT%H:%M:%S",
                                    "%Y-%m-%dT%H:%M:%OS","%Y-%m-%dT%H:%M:%OS%z"), 
                            exact = TRUE)
  }
  
  values <- as.numeric(xml_text(xml_find_all(TVP, ".//wml2:value")))

  idents <- xml_text(xml_find_all(timeSeries, ".//gml:identifier"))
  idents <- strsplit(idents, "[.]")[[1]][2]
  useIdents <- rep(idents, length(values))

  tvpQuals <- xml_text(xml_find_first(TVP, ".//swe:value"))

  df <- cbind.data.frame(staid = useIdents, val = values, dates = time, 
                           qualcode = tvpQuals, stringsAsFactors = FALSE)
  attributes(df)$code<-code
  attributes(df)$stat<-stat
  return(df)
}

#' Function to plot hydrologic times series.  
#' Will plot more than one site at a time.
#'
#' @name plotParam
#' @title Plot Streamflow and Continous Water-Quality Data
#' @param data is the data frame in the format of that returned by 
#' \link{importDVs}.
#' @param metric USGS streamflow data are usually in cubic feet per second;   
#' however it may be converted to cubic meters per second for publication.  
#' Likewise, gage height is usually in feet, but could be converted to 
#' meters.  The metric argument only has an effect on streamflow and gage 
#' height.
#' @param logscale is a logical indicating whether or not the y-scale should be 
#' log 10.  Streamflow generally is plotted with a log scale and this only has 
#' an effect on the plotting of streamflow data.
#' @param ylabel optionally allows user to pass a y-axis label.
#' @param ... further arguments to be passed to plotting method (see \link{par}). 
#' (see \link{xyplot}).
#' @return a lattice plot 
#' @importFrom lattice xyplot
#' @importFrom latticeExtra yscale.components.log10ticks
#' @export
#' @examples 
#' data(exampleWaterData)
#' plotParam(misQ05054000, code="00060", stat="00003", logscale=TRUE)
#' plotParam(misQ05054000, code=attributes(misQ05054000)$code, 
#' stat=attributes(misQ05054000)$stat, logscale=TRUE)
#' @keywords hplot ts univar
plotParam<-function(data, logscale=FALSE, metric=FALSE, ylabel=NULL, ...) {
  if (missing(ylabel) ) {
    if ( is.null(attributes(data)$stat) | is.null(attributes(data)$code) ) {
      stop("The data frame needs to have either stat and code attributes or 
           the ylabel argument needs to be used.")
    } else {
      stat<-attributes(data)$stat
      code<-attributes(data)$code
      if (stat=="00003") { stat.txt <- "Daily mean" } else
        if (stat=="00001") { stat.txt <- "Daily maximum" } else
          if (stat=="00002") { stat.txt <- "Daily minimum" } else
            if (stat=="00008") { stat.txt <- "Daily median" } else {
              message("Unknown stat code, a label should be passed using the 
                      ylabel argument.")
            }
      
      if (code=="00060") {
        if ( logscale== TRUE ) {
          if (metric==TRUE) {
            my.ylab <- paste(stat.txt,"streamflow, cubic meters per second", 
                             sep=" ")
            my.plot<-xyplot((val*0.0283)~dates | staid, data=data, typ="l", 
                            scales=list(x = list(tck = -1), y=list(log=TRUE, 
                                                                   tck = -1)), 
                            ylab=my.ylab, xlab="",
                            yscale.components = yscale.components.log10ticks,
                            ...)
          }  
          else if (metric=="FALSE") {
            my.ylab<-paste(stat.txt,"streamflow, cubic feet per second", 
                           sep=" ")
            my.plot<-xyplot(val~dates|staid, data=data, typ="l", 
                            scales=list(x = list(tck = -1), y=list(log=logscale, 
                                                                   tck = -1)), 
                            ylab=my.ylab, xlab="", 
                            yscale.components = yscale.components.log10ticks,
                            ...) 
          } 
          else {
            stop("metric must be TRUE for cubic meters per second or FALSE
             for cubic feet per second")
          }
        }  
        if ( logscale==FALSE ) {
          if (metric==TRUE) {
            my.ylab <- paste(stat.txt,"streamflow, cubic meters per second", 
                             sep=" ")
            my.plot<-my.plot<-xyplot((val*0.0283)~dates | staid, data=data, 
                                     typ="l", scales=list(x = list(tck = -1), 
                                                          y=list(tck = -1)), 
                                     ylab=my.ylab, xlab="", ...)
          }  
          else if (metric=="FALSE") {
            my.ylab<-paste(stat.txt,"streamflow, cubic feet per second", 
                           sep=" ")
            my.plot<-xyplot(val~dates|staid, data=data, typ="l", 
                            scales=list(x = list(tck = -1), y=list(tck = -1)), 
                            ylab=my.ylab, xlab="", ...) 
          } 
          else {
            stop("metric must be TRUE for cubic meters per second or FALSE
           for cubic feet per second")
          }
        }
      }
      if (code=="00065") {
        if (metric==FALSE) {
          my.ylab <- paste(stat.txt, "gage height, feet", sep=" ")
          my.plot<-xyplot(val~dates|staid, data=data, typ="l", 
                          scales=list(x = list(tck = -1), y=list(tck = -1)), 
                          ylab=my.ylab, xlab="", ...)  
        }
        else if (metric==TRUE) {
          my.ylab <- paste(stat.txt, "gage height, meters", sep=" ")
          my.plot<-xyplot((val*0.3048)~dates|staid, data=data, typ="l", 
                          scales=list(x = list(tck = -1), y=list(tck = -1)), 
                          ylab=my.ylab, xlab="", ...)  
        }
        else {
          stop("metric must be TRUE for meters or FALSE for feet.")
        }
      }
      if (code=="00095") {
        my.ylab<-paste(stat.txt, 
                       "specific conductance, water,\nunfiltered, microsiemens per centimeter\nat 25 degrees Celsius", 
                       sep=" ")
        my.plot<-xyplot(val~dates|staid, data=data, typ="l", 
                        scales=list(x = list(tck = -1), y=list(tck = -1)), 
                        ylab=my.ylab, xlab="", ...) 
      }
      if (code=="00010") {
        my.ylab<-paste(stat.txt, "temperature, water, degrees Celsius", sep=" ")
        my.plot<-xyplot(val~dates|staid, data=data, typ="l", 
                        scales=list(x = list(tck = -1), y=list(tck = -1)), 
                        ylab=my.ylab, xlab="", ...)
      }
      if (code=="00300") {
        my.ylab <- paste(stat.txt, 
                         "dissolved oxygen, water, unfiltered, milligrams per liter", 
                         sep=" ")
        my.plot<-xyplot(val~dates|staid, data=data, typ="l", 
                        scales=list(x = list(tck = -1), y=list(tck = -1)), 
                        ylab=my.ylab, xlab="", ...)  
      }
      if (code=="00400") {
        my.ylab <- paste(stat.txt, 
                         "pH, water, unfiltered, field, standard units", 
                         sep=" ")
        my.plot<-xyplot(val~dates|staid, data=data, typ="l", 
                        scales=list(x = list(tck = -1), y=list(tck = -1)), 
                        ylab=my.ylab, xlab="", ...)  
      }
      if (code=="63680") {
        my.ylab <- paste(stat.txt, 
                         "turbidity, water, unfiltered,\nmonochrome near infra-red LED light,\n780-900 nm, detection angle 90 +/ -2.5 degrees,\nformazin nephelometric units (FNU)", 
                         sep=" ")
        my.plot<-xyplot(val~dates|staid, data=data, typ="l", 
                        scales=list(x = list(tck = -1), y=list(tck = -1)), 
                        ylab=my.ylab, xlab="", ...) 
      }
    }  
  } else {
    my.ylab <- ylabel
    if ( logscale==TRUE ) {
      my.plot<-xyplot(val~dates | staid, data=data, typ="l", 
                      scales=list(x = list(tck = -1), y=list(log=TRUE, 
                                                               tck = -1)), 
                      ylab=my.ylab, xlab="", 
                      yscale.components = yscale.components.log10ticks, ...)
    } else {
      my.plot<-xyplot(val~dates | staid, data=data, typ="l", 
                      scales=list(x = list(tck = -1), y=list(log=FALSE, 
                                                             tck = -1)), 
                      ylab=my.ylab, xlab="")
    }   
  }
  my.plot
}

#' Function that returns USGS Daily Values Site Service URL for troubleshooting or 
#' building a URL for other purposes.
#'
#' @name tellMeURL
#' @title USGS Daily Values Site Service URL
#' @param staid is the USGS site identification number, which 
#' is usually eight digits long, but can be longer.  Users may search for 
#' surface-water sites and obtain station identification numbers using the 
#' USGS Site Web Service, 
#' \url{https://waterservices.usgs.gov/rest/Site-Service.html} (U.S. Geological 
#' Survey, 2017d); using the National Water Information System: Mapper, 
#' \url{https://maps.waterdata.usgs.gov/mapper/index.html} (U.S. Geological Survey, 2017a); 
#' or using the National Water Information System: Web Interface to daily 
#' surface-water data, 
#' \url{https://waterdata.usgs.gov/nwis/dv/?referred_module=sw} (U.S. 
#' Geological Survey, 2017e).  The site identification number needs to be 
#' entered as a character, that is in quotes, because many USGS streamgage 
#' numbers begin with zero and the leading zero is necessary.
#' @param code is the USGS parameter code, a 5-digit number 
#' used in the USGS computerized data system, National Water 
#' Information System (NWIS), to uniquely identify a specific hydrologic 
#' property or constituent.  A list of paramater codes is available at 
#' \url{https://nwis.waterdata.usgs.gov/usa/nwis/pmcodes} (U.S. Geological 
#' Survey, 2017b).
#' @param stat is the USGS statistics code, a 5-digit number 
#' used in the USGS computerized data system, NWIS, to uniquely identify 
#' specific statistics, such as daily mean, daily maximum, and daily minimum.  
#' The default,  00003,  is the mean daily value.  A list of statistics codes 
#' is available at 
#' \url{https://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table} 
#' (U.S. Geological Survey, 2017c).
#' Not all statistics are available at every gage.
#' @param sdate is the start date of the time series, in the format yyyy-mm-dd.
#' @param edate is the end date of the time series, in the format yyyy-mm-dd.
#' @keywords utilities
#' @export
#' @references
#' U.S. Geological Survey, 2017a, National Water Information System: Mapper, 
#' accessed January 3, 2017, at 
#' \url{https://maps.waterdata.usgs.gov/mapper/index.html}.
#' 
#' U.S. Geological Survey, 2017b, Parameter code definition, 
#' National Water Information System: Web Interface, accessed January 3, 
#' 2017, at \url{https://nwis.waterdata.usgs.gov/usa/nwis/pmcodes}.
#' 
#' U.S. Geological Survey, 2017c, Stat codes (stat_cd), 
#' National Water Information System: Web Interface, accessed January 3, 
#' 2017, at 
#' \url{https://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}.
#' 
#' U.S. Geological Survey, 2017d, USGS site web service: 
#' REST Web Services, accessed January 3, 2017, at 
#' \url{https://waterservices.usgs.gov/rest/Site-Service.html}.
#' 
#' U.S. Geological Survey, 2017e, USGS surface-water daily data for the Nation: 
#' National Water Information System: Web Interface, accessed January 3, 
#' 2017, at \url{http://waterdata.usgs.gov/nwis/dv/?referred_module=sw}.
#' @return URL for USGS data
#' @examples 
#' tellMeURL("05054000", code="00060", stat="00003", sdate="2000-01-01", 
#'  edate=as.Date(Sys.Date(), format="%Y-%m-%d"))
tellMeURL <- function(staid, code="00060", stat="00003", sdate="1851-01-01",  
                      edate=as.Date(Sys.Date(), format="%Y-%m-%d")) {
  if (is.character(staid) == FALSE ) stop("staid needs to have quotes around it")
  if (nchar(staid) < 8) stop ("staid must be at least 8 characters")
  base_url <- "https://waterservices.usgs.gov/nwis/dv/?format=waterml,2.0"
  url <- paste(base_url, "&site=", staid, "&parameterCd=", code, "&statCd=", 
               stat, sep = "")
  url <- paste(url, "&startDt=", sdate, sep="")
  url <- paste(url, "&endDt=", edate, sep="")
  url
}

#' Function to retrieve information about a USGS streamgage site
#' 
#' This provides some limited metadata about the USGS streamgage site.
#' @name siteInfo
#' @title Retrieve site information
#' @param staid is a character vector containing USGS site
#' identification number(s).  USGS site numbers are usually eight digits long, 
#' but can be longer.  Users may search for surface-water sites and obtain 
#' station identification numbers using the USGS Site Web Service, 
#' \url{https://waterservices.usgs.gov/rest/Site-Service.html} (U.S. Geological 
#' Survey, 2017b); using the National Water Information System: Mapper, 
#' \url{https://maps.waterdata.usgs.gov/mapper/index.html} (U.S. Geological Survey, 2017a); 
#' or using the National Water Information System: Web Interface to daily 
#' surface-water data, 
#' \url{https://waterdata.usgs.gov/nwis/dv/?referred_module=sw} (U.S. 
#' Geological Survey, 2017c).  The site identification number needs to be 
#' entered as a character, that is in quotes, because many USGS streamgage 
#' numbers begin with zero and the leading zero is necessary.
#' @keywords datagen
#' @references
#' U.S. Geological Survey, 2017a, National Water Information System: Mapper, 
#' accessed January 3, 2017, at 
#' \url{https://maps.waterdata.usgs.gov/mapper/index.html}.
#' 
#' U.S. Geological Survey, 2017b, USGS site web service: 
#' REST Web Services, accessed January 3, 2017, at 
#' \url{https://waterservices.usgs.gov/rest/Site-Service.html}.
#' 
#' U.S. Geological Survey, 2017c, USGS surface-water daily data for the Nation: 
#' National Water Information System: Web Interface, accessed January 3, 2017, 
#' at \url{https://waterdata.usgs.gov/nwis/dv/?referred_module=sw}.
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_root
#' @importFrom xml2 xml_children
#' @importFrom xml2 xml_attr
#' @importFrom dataRetrieval getWebServiceData
#' @export
#' @return a data frame containing the station identification number(s), the 
#' USGS streamgage name(s), the decimal latitude(s), and decimal longitude(s).
#' @format a data frame with the following columns:
#'   \tabular{llll}{
#' Name \tab Type \tab Description \cr 
#' staid \tab factor \tab USGS station identification number \cr
#' staname \tab character \tab USGS station name \cr
#' lat \tab numeric \tab Decimal latitude \cr
#' lng \tab numeric \tab Decimal longitude
#' }
#' @note Information retrieved using this function can be used to create a map of
#' multiple streamgage sites---see package vignette.
#' @examples
#' staInfo <- siteInfo("05054000")
#' staInfo
#' staInfo <- siteInfo(c("05054000", "05082500", "06342500"))
#' staInfo
#' # a list with an invalid station identification number
#' staInfo <- siteInfo(c("05054000", "05082500", "0642501"))
siteInfo<-function(staid) {
  # modified from whatNWISsites function of package dataRetrieval version 2.6.3
  retVal <- NULL
  for (i in 1:length(staid)) {
    if (is.character(staid[i]) == FALSE ) stop("staid needs to have quotes 
                                               around it")
    if (nchar(staid[i]) < 8) stop ("staid must be at least 8 characters")
    
    base_url <-"https://waterservices.usgs.gov/nwis/site?format=mapper&sites="
    url <- paste(base_url, staid[i],
                 "&siteOutput=expanded&seriesCatalogOutput=true&outputDataTypeCd=all", 
                 sep = "")
    
    if (class(url) == "character" && file.exists(url)) {
      returnedDoc <- read_xml(url)
    } else if(class(url) == 'raw') {
      returnedDoc <- read_xml(url)
      raw <- TRUE
    } else {
      returnedDoc <- xml_root(getWebServiceData(url, encoding = 'gzip'))
    }
    
    siteDat <- xml_find_all(returnedDoc, "//site") # each parameter/site combo

    doc <- xml_root(siteDat)
    siteCategories <- xml_children(doc)
  
    for (sc in siteCategories) {
      sites <- xml_children(sc)
      singlestaid <- xml_attr(sites, "sno")
      staname <- xml_attr(sites, "sna")
      lat <- as.numeric(xml_attr(sites, "lat"))
      lng <- as.numeric(xml_attr(sites, "lng"))

      df <- data.frame(staid=singlestaid, staname, lat, lng, stringsAsFactors = FALSE) 
      
      if (is.null(retVal)) {
        retVal <- df
      } else {
        retVal <- rbind.data.frame(retVal, df)
      }
    }
  }
  retVal
}

#' Function that returns USGS Site Information Service URL for troubleshooting or 
#' building a URL for other purposes.
#'
#' @name tellMeSiteURL
#' @title USGS Site Information Service URL
#' @param staid is the USGS site identification number, which 
#' is usually eight digits long, but can be longer.  Users may search for 
#' surface-water sites and obtain station identification numbers using the 
#' USGS Site Web Service, 
#' \url{https://waterservices.usgs.gov/rest/Site-Service.html} (U.S. Geological 
#' Survey, 2017b); using the National Water Information System: Mapper, 
#' \url{https://maps.waterdata.usgs.gov/mapper/index.html} (U.S. Geological Survey, 2017a); 
#' or using the National Water Information System: Web Interface to daily 
#' surface-water data, 
#' \url{https://waterdata.usgs.gov/nwis/dv/?referred_module=sw} (U.S. 
#' Geological Survey, 2017c).  The site identification number needs to be 
#' entered as a character, that is in quotes, because many USGS streamgage 
#' numbers begin with zero and the leading zero is necessary.
#' @keywords utilities
#' @return URL for USGS site information
#' @export
#' @references
#' U.S. Geological Survey, 2017a, National Water Information System: Mapper, 
#' accessed January 3, 2017, at 
#' \url{https://maps.waterdata.usgs.gov/mapper/index.html}.
#' 
#' U.S. Geological Survey, 2017b, USGS site web service: 
#' REST Web Services, accessed January 3, 2017, at 
#' \url{https://waterservices.usgs.gov/rest/Site-Service.html}.
#' 
#' U.S. Geological Survey, 2017c, USGS surface-water daily data for the Nation: 
#' National Water Information System: Web Interface, accessed January 3, 2017, 
#' at \url{https://waterdata.usgs.gov/nwis/dv/?referred_module=sw}.
#' @examples 
#' tellMeSiteURL("05054000")
tellMeSiteURL <- function(staid) {
if (is.character(staid) == FALSE ) stop("Station number needs to have quotes around it")
  if (nchar(staid) < 8) stop ("Station number must be at least 8 characters")
  base_url <-"https://waterservices.usgs.gov/nwis/site?format=mapper&sites="
  url <- paste(base_url, staid,
               "&siteOutput=expanded&seriesCatalogOutput=true&outputDataTypeCd=all", 
               sep = "")
  url
}
