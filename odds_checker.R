
odds_checker <- function(url, startdate, filename="", shrink=TRUE){

  ## load required packages
  require(rvest)
  require(zoo)
  require(reshape2)
  
  # create list of scenarios
  mainhtml <- read_html(url)
  scenarios <- html_text(html_nodes(mainhtml,".nm"))
  
  # generate date list
  maindates <- seq.Date(from = as.Date(startdate, format = "%Y-%m-%d"), to = Sys.Date(), by = "day")
  
  # Create main DF
  maindf <- setNames(as.data.frame(matrix(nrow=length(maindates), ncol=length(scenarios)), row.names=as.character(maindates)), scenarios)
  maindf <- maindf[order(rownames(maindf), decreasing=TRUE),]
  
  for(scenario in scenarios){
    # Read in scenario data
    scenariourl <- paste0(url,"/bet-history/",gsub(" ","-",scenario),"/all-history")
    scenariohtml <- read_html(scenariourl)
    
    # create list of sites
    sites <- character()
    sitesrow <- html_nodes(scenariohtml, xpath = '//*[@id="all-history"]/table/thead/tr[@class="eventTableHeader"]/td[position()>1]/span')
    for(row in sitesrow){
      site <- html_attr(row, name = "data-bk")
      sites[length(sites)+1] <- site
    }
    
    # Scenario dates
    datedoc <- html_nodes(scenariohtml, xpath = '//*[@id="all-history"]/table/tbody/tr/td[1]')
    scenariodates <- html_text(datedoc)
    scenariodates <- scenariodates[scenariodates != "Date"]
    
    # Check for valid data
    if (length(scenariodates) == 0) next
    
    # Create scenario data frame
    scenariodf <- setNames(as.data.frame(matrix(nrow=length(scenariodates), ncol=length(sites))), sites)
    rownames(scenariodf) <- scenariodates
    
    # Add data to DF
    for (r in 1:length(scenariodates)) {
      for (c in 1:length(sites)) {
        
        path <- paste0('//*[@id="all-history"]/table/tbody/tr[',r,']/td[',c+1,']/div[1]')
        rawodd <- html_nodes(scenariohtml, xpath = path)
        rawodd <- html_text(rawodd)
        
        scenariodf[r,c] <- 
          if(length(rawodd) == 0) {
            NA 
          } else if (length(grep("/",rawodd)) != 0) {
            # split character fractions by "/" then divide numerator by denominator
            odd <- as.numeric(unlist(strsplit(rawodd, split = "/"))[1]) / as.numeric(unlist(strsplit(rawodd, split = "/"))[2])
            odd <- 1/(odd+1)
          } else if (rawodd == "SUSP") {
            "SUSP"
          } else {
            odd <- as.numeric(rawodd)
            odd <- 1/(odd+1)
          }
      }
    }
    
    ## create full daily scenario df
    dailydf <- setNames(as.data.frame(matrix(nrow=length(maindates), ncol=length(sites)), row.names=as.character(maindates)), sites)
    dailydf <- dailydf[order(rownames(dailydf), decreasing = TRUE),]
    
    ## merge candidate df and daily candidate df
    dailydf[match(rownames(scenariodf), rownames(dailydf)),] <- scenariodf
    
    ## fill in leading NAs with prior data
    dailydf <- apply(dailydf, 2, function(x) na.locf(x, na.rm=FALSE, fromLast=TRUE))
    
    ## convert daily candidate df to numeric
    suppressWarnings(class(dailydf) <- "numeric")
    
    ## calculate row means and insert into UKConservative df
    maindf[,scenario] <- replace(rowMeans(dailydf, na.rm = TRUE),is.nan(rowMeans(dailydf, na.rm = TRUE)),NA)
  }
  
  if(shrink){
    maindf[] <- t(apply(maindf, 1, function(x) x / sum(x, na.rm = TRUE)))
  }
  
  if(filename!="") {
    write.csv(maindf, paste0(filename,".csv"))
  } else {
    return(maindf)
  }
  
}
  