library(Rcrawler)
Listlength <- Rcrawler:::Listlength
library(doParallel)
library(foreach)
library(parallel)
library(doParallel)
library(selectr)
library(webdriver)
library(httr)
library(data.table)
#library(xml12)
library(utils)



Rcrawler_modified <- function(Website, no_cores,no_conn, MaxDepth, DIR, RequestsDelay=0,Obeyrobots=FALSE,
                     Useragent, use_proxy=NULL, Encod, Timeout=5, URLlenlimit=255, urlExtfilter,
                     dataUrlfilter, crawlUrlfilter, crawlZoneCSSPat=NULL, crawlZoneXPath=NULL,
                     ignoreUrlParams,ignoreAllUrlParams=FALSE, KeywordsFilter,KeywordsAccuracy,FUNPageFilter,
                     ExtractXpathPat, ExtractCSSPat, PatternsNames, ExcludeXpathPat, ExcludeCSSPat,
                     ExtractAsText=TRUE, ManyPerPattern=FALSE, saveOnDisk=TRUE, NetworkData=FALSE, NetwExtLinks=FALSE,
                     statslinks=FALSE, Vbrowser=FALSE, LoggedSession) {
  
  if (missing(DIR)) DIR<-getwd()
  if (missing(KeywordsAccuracy)) KeywordsAccuracy<-1
  if (missing(ignoreUrlParams)) ignoreUrlParams<-""
  if (missing(MaxDepth)) MaxDepth<-10
  if (missing(no_cores)) no_cores<-parallel::detectCores()-1
  if (missing(no_conn)) no_conn<-no_cores
  if (missing(Obeyrobots)) Obeyrobots<-FALSE
  
  
  if (missing(dataUrlfilter)){
    dataUrlfilter<-".*"
    dataUrlfilterMissing<-TRUE
  }
  else {
    dataUrlfilter<-paste(dataUrlfilter,collapse="|")
    dataUrlfilterMissing<-FALSE
  }
  
  if (missing(crawlUrlfilter)){
    crawlUrlfilter <-".*"
    crawlUrlfilterMissing<-TRUE
  }
  else {
    crawlUrlfilter<-paste(crawlUrlfilter,collapse="|")
    crawlUrlfilterMissing<-FALSE
  }
  
  if(missing(Useragent)) {Useragent="Mozilla/5.0 (Windows NT 6.3; WOW64; rv:42.0) Gecko/20100101 Firefox/42.0"}
  if(missing(Encod)) {
    Encod<- Getencoding(Website)
    if (length(Encod)!=0){
      if(Encod=="NULL") Encod="UTF-8" ;
    }
  }
  
  if (!is.null(use_proxy) && (!missing(Vbrowser) || !missing(LoggedSession))) stop("webdriver cant be configured to use a proxy")
  
  
  if (!missing(FUNPageFilter)){
    if (!is.function(FUNPageFilter)) stop("FUNPageFilter parameter must be a function")
  }
  
  if(!missing(KeywordsFilter)){
    if(!is.vector(KeywordsFilter)){
      stop("KeywordsFilter parameter must be a vector with at least one element !")
    }
  }
  if(!is.numeric(KeywordsAccuracy)){
    stop ("KeywordsAccuracy parameter must be a numeric value between 1 and 100")
  } else {
    if(KeywordsAccuracy<=0 && KeywordsAccuracy>100) {
      stop ("KeywordsAccuracy parameter must be a numeric value between 1 and 100")
    }
  }
  if(!missing(KeywordsFilter) && !missing(FUNPageFilter) ){
    stop("Please supply KeywordsFilter or FUNPageFilter, not both !")
  }
  
  if(!missing(ExcludeXpathPat) && !missing(ExcludeCSSPat) ){
    stop("Please supply ExcludeXpathPat or ExcludeCSSPat, not both !")
  }
  if ((!missing(ExcludeXpathPat) || !missing(ExcludeCSSPat)) && (missing(ExtractXpathPat) && missing(ExtractCSSPat))){
    stop("ExcludeXpathPat or ExcludeCSSPat should work only if ExtractXpathPat or ExtractCSSPat are used !")
  }
  if(!missing(ExtractXpathPat) && !missing(ExtractCSSPat) ){
    stop("Please supply ExtractXpathPat or ExtractCSSPat, not both !")
  }
  if(!missing(ExtractCSSPat)) {
    if(is.vector(ExtractCSSPat)){
      ExtractXpathPat<- unlist(lapply(ExtractCSSPat, FUN = function(x) { tryCatch(selectr::css_to_xpath(x, prefix = "//") ,error=function(e) stop("Unable to translate supplied css selector, Please check ExtractCSSPat syntax !"))}))
    } else {
      stop("ExtractCSSPat parameter must be a vector with at least one element !")
    }
  }
  if(!missing(ExcludeCSSPat)) {
    if(is.vector(ExcludeCSSPat)){
      ExcludeXpathPat<- unlist(lapply(ExcludeCSSPat, FUN = function(x) { tryCatch(selectr::css_to_xpath(x, prefix = "//") ,error=function(e) stop("Unable to translate supplied css selector, Please check ExcludeCSSPat syntax !"))}))
    }
  }
  if(missing(ExcludeCSSPat) && missing(ExcludeXpathPat) ){
    ExcludeXpathPat=NULL
  }
  if(!is.null(crawlZoneCSSPat)){
    crawlZoneXPath<- unlist(lapply(crawlZoneCSSPat, FUN = function(x) { tryCatch(selectr::css_to_xpath(x, prefix = "//") ,error=function(e) stop("Unable to translate supplied css selector, Please check CrawlZoneCSSPat syntax !"))}))
    
  }
  
  if(missing(urlExtfilter)){ urlExtfilter<-c("flv","mov","swf","txt","xml","js","css","zip","gz","rar","7z","tgz","tar","z","gzip","bzip","tar","mp3","mp4","aac","wav","au","wmv","avi","mpg","mpeg","pdf","doc","docx","xls","xlsx","ppt","pptx","jpg","jpeg","png","gif","psd","ico","bmp","odt","ods","odp","odb","odg","odf") }
  keywordCheck<-FALSE
  if(missing(KeywordsFilter) ){
    keywordCheck<-FALSE
  } else {
    if(is.vector(KeywordsFilter)) {
      keywordCheck<-TRUE
    }
  }
  if(!missing(LoggedSession)){
    if(length(LoggedSession)!=3){
      stop("Error in LoggedSession Argurment, use run_browser() and LogginSession() functions to create a valid browser session object")
    }
  }
  domain<-strsplit(gsub("http://|https://|www\\.", "", Website), "/")[[c(1, 1)]]
  if (Obeyrobots) {
    rules<-RobotParser(Website,Useragent)
    urlbotfiler<-rules[[2]]
    urlbotfiler<-gsub("^\\/", paste("http://www.",domain,"/", sep = ""), urlbotfiler , perl=TRUE)
    urlbotfiler<-gsub("\\*", ".*", urlbotfiler , perl=TRUE)
  } else {urlbotfiler=" "}
  
  
  IndexErrPages<-c(200)
  
  #create repository
  tryCatch(curdate<-format(Sys.time(), "%d%H%M"),error=function(e) curdate<-sample(1000:9999, 1) )
  if(saveOnDisk){
    foldename<-paste(domain,"-",curdate,sep = "")
    path<-paste(DIR,"/", foldename ,sep = "")
    dir.create(path, recursive = TRUE, mode = "0777")
  }
  #if(Backup) {
  #   Fileindex <- file(paste(path,"/","index.csv", sep = ""), "w")
  #  Filefrontier <- file(paste(path,"/","frontier.csv", sep = ""), "w")
  #  Filestat<-file(paste(path,"/","state.txt", sep = ""), "w")
  #  }
  
  if(!missing(ExtractXpathPat)) {
    if(saveOnDisk){
      Filecontent <- file(paste(path,"/","extracted_data.csv", sep = ""), "w")
    }
  }
  duplicatedetect<-FALSE
  #create Dataframe
  id<-vector()
  urls<-vector()
  links<-vector()
  status<-vector()
  level<-vector()
  inn<-numeric()
  out<-numeric()
  httpstat<-vector()
  contenttype<-vector()
  encoding<-vector()
  hashcode<-vector()
  Accuracy<-vector()
  allpaquet<-list()
  
  pkg.env <- new.env()
  if (!missing(ExtractXpathPat)) { pkg.env$Exdata<-list() }
  pkg.env$shema<-data.frame(id,urls,status,level,out,inn,httpstat,contenttype,encoding,Accuracy)
  names(pkg.env$shema) <- c("Id","Url","Stats","Level","OUT","IN","Http Resp","Content Type","Encoding","Accuracy")
  if(NetworkData){
    FromNode<-vector()
    ToNode<-vector()
    Weight<-vector()
    Type<-vector()
    pkg.env$GraphINDEX<-vector()
    pkg.env$GraphINDEX<-c(pkg.env$GraphINDEX,Website)
    pkg.env$GraphEgdes<-data.frame(FromNode,ToNode,Weight,Type)
    names(pkg.env$GraphEgdes) <- c("From","To","Weight","Type")
  }
  
  pkg.env$Lbrowsers<-list()
  
  
  if(!missing(LoggedSession)){
    
    no_conn<-no_cores
    cat("Preparing browser process ")
    pkg.env$Lbrowsers[[1]]<-LoggedSession
    
    if(no_cores>=2){
      for(i in 2:no_cores){
        pkg.env$Lbrowsers[[i]]<-run_browser()
        pkg.env$Lbrowsers[[i]]<-LoginSession(Browser = pkg.env$Lbrowsers[[i]], LoginURL = LoggedSession$loginInfo$LoginURL, LoginCredentials =LoggedSession$loginInfo$LoginCredentials,
                                             cssLoginFields = LoggedSession$loginInfo$cssLoginFields,cssLoginButton =LoggedSession$loginInfo$cssLoginButton,cssRadioToCheck = LoggedSession$loginInfo$cssRadioToCheck,
                                             XpathLoginFields = LoggedSession$loginInfo$XpathLoginFields, XpathLoginButton = LoggedSession$loginInfo$XpathLoginButton, XpathRadioToCheck = LoggedSession$loginInfo$XpathRadioToCheck)
        cat("browser:",i," port: ",pkg.env$Lbrowsers[[i]]$process$port)
        Sys.sleep(1)
        cat("..")
        flush.console()
      }
    }
  } else if(Vbrowser){
    #if(RequestsDelay==0) RequestsDelay=2
    no_conn<-no_cores
    cat("Preparing browser process ")
    for(i in 1:no_cores){
      pkg.env$Lbrowsers[[i]]<-run_browser()
      cat("browser:"+i+" port:"+pkg.env$Lbrowsers[[i]]$process$port)
      Sys.sleep(1)
      cat(".")
      flush.console()
    }
  }
  
  
  cat("\n")
  #timev<<- vector()
  #timef<<-vector()
  Error403 <- vector()
  shemav <- vector()
  shemav<-c(shemav,Website)
  Lshemav<-list(shemav)
  M=Rcrawler:::Listlength(Lshemav)
  lev<-0
  t<-1
  posx<-0
  i<-0
  posenv <- 1
  chunksize<-10000
  envi = as.environment(posenv)
  #cluster initialisation
  cl <- makeCluster(no_cores)
  
  cat("Preparing multihreading cluster .. ")
  registerDoParallel(cl)
  
  clusterEvalQ(cl, library(xml2))
  clusterEvalQ(cl, library(httr))
  clusterEvalQ(cl, library(data.table))
  clusterEvalQ(cl, library(webdriver))
  clusterEvalQ(cl, library(jsonlite))
  clusterExport(cl, c("LinkExtractor","LinkNormalization","Drv_fetchpage"))
  clusterExport(cl, c("shema","Lbrowsers"), envir = pkg.env)
  #tmparallelreq<<-vector()
  #tmparallel<<-vector()
  #tminsertion<<-vector()
  #tminsertionreq<<-vector()
  Iter<-0
  while (t<=Rcrawler:::Listlength(Lshemav) && MaxDepth>=lev){
    
    Iter<-Iter+1
    # extraire les liens sur la page
    rest<-Rcrawler:::Listlength(Lshemav)-t
    #if(rest==0){ rest<-rest+1 }
    if (no_conn<=rest){
      l<-t+no_conn-1
    } else {
      l<-t+rest
    }
    #cat(t,"to",l,"size:",length(shemav))
    #get links & pageinfo
    if(missing(Vbrowser) && missing(LoggedSession)){
      if (RequestsDelay!=0) {
        Sys.sleep(RequestsDelay)
      }
    }
    #ptm <- proc.time()
    if(t<chunksize){
      tt<-t
      VposT<-1
    } else {
      VposT<-(t%/%chunksize)+1
      tt<-t%%(chunksize*(t%/%chunksize))+1
    }
    
    if(l<chunksize){
      ll<-l
      VposL<-1
    }else{
      VposL<-(l%/%chunksize)+1
      ll<-l%%(chunksize*(l%/%chunksize))+1
    }
    tmpshemav<-vector()
    if(VposT!=VposL){
      for(k in tt:(chunksize-1)) {
        #bcat("k:",k)
        tmpshemav<-c(tmpshemav,Lshemav[[VposT]][[k]])
      }
      for(r in 1:ll){
        tmpshemav<-c(tmpshemav,Lshemav[[VposL]][[r]])
        #cat("r:",r)
      }
      
      #topshemav<<-tmpshemav
      #cat("VposT :",VposT," tt :",tt, " VposL :",VposL, " ll :",ll," tmpshema:",length(tmpshemav))
      if(Vbrowser || !missing(LoggedSession)){
        allpaquet <- foreach(i=1:length(tmpshemav),  .verbose=FALSE, .inorder=FALSE, .errorhandling='pass')  %dopar%
          {
            LinkExtractor(url = tmpshemav[[i]], id = i,lev = lev, IndexErrPages = IndexErrPages, Useragent = Useragent, Timeout = Timeout, URLlenlimit = URLlenlimit, urlExtfilter=urlExtfilter, encod = Encod, urlbotfiler = urlbotfiler, removeparams=ignoreUrlParams, ExternalLInks=NetwExtLinks, Browser =pkg.env$Lbrowsers[[1]], RenderingDelay = RequestsDelay, removeAllparams = ignoreAllUrlParams, urlregexfilter =crawlUrlfilter, urlsZoneXpath = crawlZoneXPath)
          }
      }else{
        allpaquet <- foreach(i=1:length(tmpshemav),  .verbose=FALSE, .inorder=FALSE, .errorhandling='pass')  %dopar%
          {
            LinkExtractor(url = tmpshemav[[i]], id = i,lev = lev, IndexErrPages = IndexErrPages, Useragent = Useragent, Timeout = Timeout, URLlenlimit = URLlenlimit, urlExtfilter=urlExtfilter, encod = Encod, urlbotfiler = urlbotfiler, removeparams=ignoreUrlParams, ExternalLInks=NetwExtLinks, removeAllparams = ignoreAllUrlParams, use_proxy = use_proxy, urlregexfilter = crawlUrlfilter, urlsZoneXpath = crawlZoneXPath)
          }
      }
    } else {
      if(Vbrowser || !missing(LoggedSession) ){
        #cat("\n VposT :",VposT," tt :",tt, " VposL :",VposL, " ll :",ll,"((j%%tt)+1)=",((tt%%tt)+1) , " \n")
        j<-0
        #qq<<-LinkExtractor(url = Lshemav[[VposT]][1],id = i,lev = lev, IndexErrPages = IndexErrPages, Useragent = Useragent, Timeout = Timeout, URLlenlimit = URLlenlimit, urlExtfilter=urlExtfilter, encod = Encod, urlbotfiler = urlbotfiler, removeparams=ignoreUrlParams, ExternalLInks=NetwExtLinks, VBrowser =pkg.env$Lbrowsers[[1]])
        allpaquet <- foreach(j=tt:ll,  .verbose=FALSE, .inorder=FALSE, .errorhandling='pass')  %dopar%
          {
            LinkExtractor(url = Lshemav[[VposT]][j],id = i,lev = lev, IndexErrPages = IndexErrPages, Useragent = Useragent, Timeout = Timeout, URLlenlimit = URLlenlimit, urlExtfilter=urlExtfilter, encod = Encod, urlbotfiler = urlbotfiler, removeparams=ignoreUrlParams, ExternalLInks=NetwExtLinks, Browser =pkg.env$Lbrowsers[[((j%%tt)+1)]], removeAllparams = ignoreAllUrlParams, urlregexfilter = crawlUrlfilter, urlsZoneXpath = crawlZoneXPath)
          }
      } else{
        #for(k in tt:ll ){
        #  cat("\n -",Lshemav[[VposT]][k])
        #}
        # cat("\n VposT :",VposT," tt :",tt, " VposL :",VposL, " ll :",ll, " \n")
        j<-0
        allpaquet <- foreach(j=tt:ll,  .verbose=FALSE, .inorder=FALSE, .errorhandling='pass')  %dopar%
          {
            LinkExtractor(url = Lshemav[[VposT]][j],id = i,lev = lev, IndexErrPages = IndexErrPages, Useragent = Useragent, Timeout = Timeout, URLlenlimit = URLlenlimit, urlExtfilter=urlExtfilter, encod = Encod, urlbotfiler = urlbotfiler, removeparams=ignoreUrlParams, ExternalLInks=NetwExtLinks, removeAllparams = ignoreAllUrlParams, use_proxy = use_proxy, urlregexfilter = crawlUrlfilter, urlsZoneXpath = crawlZoneXPath)
          }
      }
      
    }
    
    
    #deb<<-allpaquet
    #for (j in t:l){
    #    cat(shemav[i]);
    #}
    #tmparallelreq<<-c(tmparallelreq,(proc.time() - ptm )[3])
    #tmparallel<<-c(tmparallel,format(Sys.time(), "%M,%S"))
    cat("In process : ")
    #if (no_conn<=rest){
    #  f<-no_conn
    #} else if (no_conn>rest) {
    #  f<-rest
    #}
    #if(0==rest) {
    #  f<-l-t+1
    #}
    #cat("f:",f,"t:",t,"conn",no_conn,"r:",rest)
    #if(f==0){f<-f+1}
    # combine all links.package in one list & insert pageinfo in shema one-by-one
    Error403[[Iter]]<-0
    for (s in 1:length(allpaquet)){
      # pos :  Global positon (regarding to shemav)
      pos<-s+t-1
      #cat("s1",s)
      cat(pos,"..", sep = "")
      flush.console()
      # timev[pos]<<-Sys.time()
      # timef[pos]<<-format(Sys.time(), "%M,%S")
      # Les page null ne sont pas ajouter au shema
      #debugg<<-allpaquet
      #debugg2<<-shemav
      #cat("x :",allpaquet[[s]]$Info$Url,"\n");
      
      if(!is.null(allpaquet[[s]]$InternalLinks) && !("call" %in% names(allpaquet[[s]]$InternalLinks))) {
        #cat("x2");
        if(allpaquet[[s]]$Info$Status_code==403){
          Error403[[Iter]]<-1
        }
        
        if (NetworkData) {
          tmplinks<-vector()
          tmplinks<-c(tmplinks,unlist(allpaquet[[s]][2]))
          #tmplinks<-c(tmplinks,debugg[[s]][[1]][[2]])
          if(length(tmplinks) > 0){
            pkg.env$GraphINDEX<-c( pkg.env$GraphINDEX , tmplinks[ ! tmplinks %chin% pkg.env$GraphINDEX ] )
            for(NodeElm in tmplinks){
              posNodeFrom<-chmatch(c(allpaquet[[s]][[1]][[2]]),pkg.env$GraphINDEX)
              pkg.env$GraphEgdes[nrow(pkg.env$GraphEgdes) + 1,]<-c(posNodeFrom,chmatch(c(NodeElm),pkg.env$GraphINDEX),lev,1)
            }
          }
          if(NetwExtLinks){
            tmplinks2<-vector()
            tmplinks2<-c(tmplinks2,unlist(allpaquet[[s]][3]))
            if(length(tmplinks2) > 0){
              pkg.env$GraphINDEX<-c( pkg.env$GraphINDEX , tmplinks2[ ! tmplinks2 %chin% pkg.env$GraphINDEX ] )
              for(NodeElm in tmplinks2){
                posNodeFrom<-chmatch(c(allpaquet[[s]][[1]][[2]]),pkg.env$GraphINDEX)
                pkg.env$GraphEgdes[nrow(pkg.env$GraphEgdes) + 1,]<-c(posNodeFrom,chmatch(c(NodeElm),pkg.env$GraphINDEX),lev,2)
              }
            }
          }
        }
        if (statslinks){
          tmplinks<-vector()
          tmplinks<-c(tmplinks,unlist(allpaquet[[s]][[2]]))
          if(length(tmplinks) > 0 && length(pkg.env$shema[[2]])>0){
            for(NodeElm in tmplinks){
              index<-chmatch(c(NodeElm),pkg.env$shema[[2]])
              if(!is.na(index)){
                pkg.env$shema[[6]][index]<-as.numeric(pkg.env$shema[[6]][index])+1
              }
            }
          }
        }
        #cat("s2")
        if (!dataUrlfilterMissing && !crawlUrlfilterMissing){
          if(length(allpaquet[[s]]$InternalLinks)>0){
            if(!grepl(pattern = dataUrlfilter,x = allpaquet[[s]]$Info$Url)){
              links<-c(links,allpaquet[[s]]$InternalLinks)
            }
          }
        } else {
          links<-c(links,allpaquet[[s]]$InternalLinks)
        }
        #cat("s3")
        #debugg2<<-allpaquet[[s]][2]
        #amdebugg3<<-allpaquet[[s]][1]
        if (allpaquet[[s]][[1]][[3]]!="NULL" && allpaquet[[s]][[1]][[10]]!="NULL" ){
          #index URL filter
          if (grepl(dataUrlfilter,allpaquet[[s]][[1]][[2]]) && (allpaquet[[s]]$Info$Status_code %in% IndexErrPages)){
            
            if(!missing(FUNPageFilter)){
              contentx<-allpaquet[[s]][[1]][[10]]
              Notagcontentx<-RemoveTags(contentx)
              isPagevalid<-FUNPageFilter(allpaquet[[s]])
              if(!is.logical(isPagevalid)) stop ("FUNPageFilter function must return a logical value TRUE/FALSE")
              if (isPagevalid){
                if (!missing(ExtractXpathPat)) {
                  excontent2<-ContentScraper(HTmlText = allpaquet[[s]][[1]][[10]],XpathPatterns = ExtractXpathPat, PatternsName = PatternsNames, ManyPerPattern=ManyPerPattern, astext = ExtractAsText, encod=Encod)
                  posx<-posx+1
                  pkg.env$shema[posx,]<-c(posx,allpaquet[[s]][[1]][[2]],"finished",allpaquet[[s]][[1]][[4]],allpaquet[[s]][[1]][[5]],"",allpaquet[[s]][[1]][[7]],allpaquet[[s]][[1]][[8]],allpaquet[[s]][[1]][[9]],paste0(Accuracy,"%"))
                  DES<-isTarget(excontent2)
                  if(DES){
                    excontent2<-c(posx,excontent2)
                    pkg.env$Exdata<-c(pkg.env$Exdata, list(excontent2))
                    assign("DATA", pkg.env$Exdata)
                  }
                  if(saveOnDisk){
                    filename<-paste0(posx,".html")
                    filepath<-paste(path,"/",filename, sep = "")
                    filepath<-file(filepath, open = "w",  encoding = Encod)
                    write(allpaquet[[s]][[1]][[10]],filepath)
                    close(filepath)
                    if(DES){
                      write.table(NormalizeForExcel(excontent2), file = Filecontent, sep = ";", qmethod="double" ,row.names = FALSE, col.names = FALSE, na = "NA" )
                    }
                  }
                }
                else {
                  posx<-posx+1
                  pkg.env$shema[posx,]<-c(posx,allpaquet[[s]][[1]][[2]],"finished",allpaquet[[s]][[1]][[4]],allpaquet[[s]][[1]][[5]],"",allpaquet[[s]][[1]][[7]],allpaquet[[s]][[1]][[8]],Encod,paste0(Accuracy,"%"))
                  if(saveOnDisk){
                    filename<-paste(posx,".html")
                    filepath<-paste(path,"/",filename, sep = "")
                    filepath<-file(filepath, open = "w",  encoding = Encod)
                    write(allpaquet[[s]][[1]][[10]],filepath)
                    close(filepath)
                  }
                }
              }
            }
            else if(keywordCheck){
              #check if page content contain some specific keywords
              contentx<-allpaquet[[s]][[1]][[10]]
              Notagcontentx<-tolower(gsub("\\W", " ",RemoveTags(contentx), perl=TRUE))
              
              AccuracyResult <- foreach(i=1:length(KeywordsFilter),  .verbose=FALSE, .inorder=FALSE, .errorhandling='pass', .combine=c)  %dopar%
                {
                  Precifunc(KeywordsFilter[i],length(KeywordsFilter),Notagcontentx)
                }
              Accuracy<-sum(AccuracyResult)
              if (Accuracy>=KeywordsAccuracy){
                #if(containtag) {
                #check for duplicate webpage & checksum calculation
                # if (duplicatedetect==TRUE){
                # hash<-getsimHash(contentx,128)
                # Ajouter au shema uniqument les liens non-repete
                # if (!(hash %in% pkg.env$shema$hashcode)){
                # posx, actual position of DF shema
                #  posx<-posx+1
                #  pkg.env$shema[posx,]<-c(posx,allpaquet[[s]][[1]][[2]],"finished",allpaquet[[s]][[1]][[4]],allpaquet[[s]][[1]][[5]],"",allpaquet[[s]][[1]][[7]],allpaquet[[s]][[1]][[8]],allpaquet[[s]][[1]][[9]],hash)
                #  filename<-paste(posx,".html")
                #  filepath<-paste(path,"/",filename, sep = "")
                #  write(allpaquet[[s]][[1]][[10]],filepath) }
                #  } else {
                if (!missing(ExtractXpathPat)) {
                  excontent2<-ContentScraper(HTmlText = allpaquet[[s]][[1]][[10]],XpathPatterns = ExtractXpathPat,PatternsName = PatternsNames, ManyPerPattern=ManyPerPattern, astext = ExtractAsText, encod=Encod)
                  posx<-posx+1
                  pkg.env$shema[posx,]<-c(posx,allpaquet[[s]][[1]][[2]],"finished",allpaquet[[s]][[1]][[4]],allpaquet[[s]][[1]][[5]],"",allpaquet[[s]][[1]][[7]],allpaquet[[s]][[1]][[8]],allpaquet[[s]][[1]][[9]],paste0(Accuracy,"%"))
                  DES<-isTarget(excontent2)
                  if(DES){
                    excontent2<-c(posx,excontent2)
                    pkg.env$Exdata<-c(pkg.env$Exdata, list(excontent2))
                    assign("DATA", pkg.env$Exdata)
                  }
                  if(saveOnDisk){
                    filename<-paste(posx,".html")
                    filepath<-paste(path,"/",filename, sep = "")
                    filepath<-file(filepath, open = "w",  encoding = Encod)
                    write(allpaquet[[s]][[1]][[10]],filepath)
                    close(filepath)
                    if(DES){
                      write.table(NormalizeForExcel(excontent2), file = Filecontent, sep = ";", qmethod="double" ,row.names = FALSE, col.names = FALSE, na = "NA" )
                    }
                  }
                }
                else {
                  posx<-posx+1
                  pkg.env$shema[posx,]<-c(posx,allpaquet[[s]][[1]][[2]],"finished",allpaquet[[s]][[1]][[4]],allpaquet[[s]][[1]][[5]],"",allpaquet[[s]][[1]][[7]],allpaquet[[s]][[1]][[8]],Encod,paste0(format(round(Accuracy, 2),nsmall = 2),"%"))
                  if(saveOnDisk){
                    filename<-paste(posx,".html")
                    filepath<-paste(path,"/",filename, sep = "")
                    filepath<-file(filepath, open = "w",  encoding = Encod)
                    write(allpaquet[[s]][[1]][[10]],filepath)
                    close(filepath)
                  }
                }
              }
            }
            else {
              if (!missing(ExtractXpathPat)) {
                excontent2<-ContentScraper(HTmlText = allpaquet[[s]][[1]][[10]],XpathPatterns = ExtractXpathPat, PatternsName = PatternsNames, ManyPerPattern = ManyPerPattern, astext = ExtractAsText, ExcludeXpathPat = ExcludeXpathPat, encod=Encod)
                #if(isTarget(excontent)){
                posx<-posx+1
                pkg.env$shema[posx,]<-c(posx,allpaquet[[s]][[1]][[2]],"finished",allpaquet[[s]][[1]][[4]],allpaquet[[s]][[1]][[5]],"",allpaquet[[s]][[1]][[7]],allpaquet[[s]][[1]][[8]],Encod,'')
                DES<-isTarget(excontent2)
                if(DES){
                  excontent2<-c(PageID=posx,excontent2)
                  pkg.env$Exdata<-c(pkg.env$Exdata, list(excontent2))
                  assign("DATA", pkg.env$Exdata)
                }
                # save on html and data on file
                if(saveOnDisk){
                  filename<-paste(posx,".html")
                  filepath<-paste(path,"/",filename, sep = "")
                  filepath<-file(filepath, open = "w",  encoding = Encod)
                  write(allpaquet[[s]][[1]][[10]],filepath)
                  close(filepath)
                  if(DES){
                    #excontent2<<-excontent2
                    #Normexcontent2<<-NormalizeForExcel(excontent2)
                    write.table(NormalizeForExcel(excontent2), file = Filecontent, sep = ";", qmethod="double" ,row.names = FALSE, col.names = FALSE, na = "NA" )
                  }
                }
                #}
              }
              else {
                posx<-posx+1
                pkg.env$shema[posx,]<-c(posx,allpaquet[[s]][[1]][[2]],"finished",allpaquet[[s]][[1]][[4]],allpaquet[[s]][[1]][[5]],1,allpaquet[[s]][[1]][[7]],allpaquet[[s]][[1]][[8]],Encod,'')
                if(saveOnDisk){
                  filename<-paste(posx,".html")
                  filepath<-paste(path,"/",filename, sep = "")
                  filepath<-file(filepath, open = "w",  encoding = Encod)
                  write(allpaquet[[s]][[1]][[10]],filepath)
                  close(filepath)
                }
              }
            }
          }
        }
      }
      
      if(pos==M){
        lev<-lev+1;
        getNewM<-TRUE
      }
    }
    
    cat("\n")
    links <- unlist(links)
    links <- unique(links)
    # ptm <- proc.time()
    # remplir le shema
    if (length(links)>0){
      #for (i in 1:length(links)){
      # Ajouter au shema uniqument les urls non repete
      # if (!(links[i] %in% shemav) ){
      #  shemav<-c(shemav,links[i])
      #shema[length(shemav),]<<-c(length(shemav),links[i],"waiting","","","1","","","")
      #}}
      #shemav<-c( shemav , links[ ! links %chin% shemav ] )
      
      for (L in links){
        LshemaSize<-length(Lshemav)
        s<-length(Lshemav[[LshemaSize]])+1
        if (s<chunksize){
          dec<-1
          for (vec in Lshemav){
            if( L %chin% vec) dec<-bitwAnd(0,dec)
          }
          if(dec==1) {
            Lshemav[[LshemaSize]][s]<-L
            s<-s+1
          }
        } else {
          LshemaSize<-LshemaSize+1
          s<-1
          dec<-1
          for (vec in Lshemav){
            if( L %chin% vec) dec<-bitwAnd(0,dec)
          }
          if(dec==1){
            Lshemav<-c(Lshemav,c(L))
            s<-s+1
          }
        }
      }
      #Lshemavv<<-Lshemav
      #cat ("shemav:", length(shemav), " Lshema:",Listlength(Lshemav))
      
      
      
    }
    Error403[is.na(Error403)]<-0
    if(length(Error403)>4){
      #cat(Error403[Iter])
      #cat(Error403[Iter-1])
      #cat(Error403[Iter-2])
      #cat(Error403[Iter-3])
      
      if(Error403[Iter]==Error403[Iter-1] && Error403[Iter-1]==Error403[Iter-2] &&
         Error403[Iter-2]==Error403[Iter-3] && Error403[Iter-3]==1)
        cat("Warning !! Many concurrent requests are blocked (403 forbidden error). Use less parallel requests in no_cores and no_conn to avoid overloading the website server.")
    }
    
    #calculate level
    if(getNewM){
      M=Rcrawler:::Listlength(Lshemav)
      getNewM<-FALSE
    }
    #tminsertion<<-c(tminsertion,(proc.time() - ptm )[3])
    #tminsertionreq<<-c(tminsertionreq,format(Sys.time(), "%M,%S"))
    cat("Progress:",format(round(((t/Rcrawler:::Listlength(Lshemav))*100), 2),nsmall = 2),"%  : ",t, " parssed from ",Rcrawler:::Listlength(Lshemav)," | Collected pages:",length(pkg.env$shema$Id)," | Level:",lev,"\n")
    # t<-l+1
    t<-t+length(allpaquet)
    if(NetworkData){
      assign("NetwEdges", pkg.env$GraphEgdes)
      assign("NetwIndex", pkg.env$GraphINDEX)
    }
    assign("INDEX", pkg.env$shema)
    #tmp<<-shemav
  }
  if(!missing(ExtractXpathPat)) {
    if(saveOnDisk){
      close(Filecontent)
    }
  }
  
  if(Vbrowser){
    cat("Shutting-down browsers ")
    for(i in 1:no_cores){
      stop_browser(pkg.env$Lbrowsers[[i]])
      Sys.sleep(1)
      cat(".")
    }
    rm(pkg.env$Lbrowsers)
  }
  #assign("Browsers", pkg.env$browsers, envir = envi )
  #rm(Browsers, envir = envi)
  #cat("Shutting-down multihrading cluster ..")
  #save(shema, file="masterma2.rda")
  stopCluster(cl)
  stopImplicitCluster()
  rm(cl)
  
  #return(pkg.env$shema)
  cat("+ Check INDEX dataframe variable to see crawling details \n")
  cat("+ Collected web pages are stored in Project folder \n" )
  if(saveOnDisk){
    cat("+ Project folder name :", foldename,"\n")
    cat("+ Project folder path :", path,"\n")
  }
  if(!missing(ExtractXpathPat)){
    cat("+ Scraped data are stored in a variable named : DATA \n")
    if(saveOnDisk){
      cat("+ Scraped data are stored in a CSV file named : extracted_data.csv \n")
    }
  }
  if(NetworkData){
    cat("+ Network nodes are stored in a variable named : NetwIndex \n")
    cat("+ Network eadges are stored in a variable named : NetwEdges \n")
  }
return(list("NetwEdges" = NetwEdges,"NetwIndex" = NetwIndex,"INDEX" = INDEX))  
}