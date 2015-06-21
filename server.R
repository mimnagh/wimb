

library("XML")
library(plyr)
library(reshape2)
institution<-"Walters"
ns<-c(tei="http://www.tei-c.org/ns/1.0")  

getFirstMatch<-function(xmlData,xpath){
  getNodeSet(xmlData,xpath,ns,xmlValue)[[1]]
}
loadFile<-function(filename){
  print(paste("Loading ",filename))
  dataInt = xmlParse(filename)
  #http://purl.org/dc/elements/1.1/identifier
  msId<-xpathSApply(dataInt,"/tei:TEI/tei:teiHeader/tei:fileDesc/tei:sourceDesc/tei:msDesc/tei:msIdentifier/tei:idno",xmlValue,namespaces=ns)
  
  if(length(msId)>0){
    idVal=paste(institution,msId,sep=".")
    sigla=sub("[.]",x=msId,replacement="")
    jpg=paste(sigla,"_000001_thumb.jpg",sep="")
    thumbNail=paste("http://thedigitalwalters.org/Data/WaltersManuscripts",sigla,"data",msId,"thumb",jpg,sep="/")
    
    df<-data.frame(element=idVal,idVal=idVal,name="dcterms:identifier")
    
    df<- rbind(df,data.frame(element="dbpedia:Illuminated_manuscript",idVal=idVal,name="a" ))
    
    df<- rbind(df,data.frame(element=thumbNail,idVal=idVal,name="dctype:Image" ))
    
    #literal
    df<-bindElements(df,"/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@type='common']",dataInt,"dcterms:title",idVal)
    
    df<-bindElements(df,"/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@type='common']",dataInt,"rdfs:label",idVal)
    
    df<-bindElements(df,"/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@type='common']",dataInt,"rdfs:comment",idVal)
    
    
    #literal 
    df<-bindElements(df,"/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author/tei:name[@type='authority']",dataInt,"dc:creator",idVal)
    #literal
    df<-bindElements(df,"/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author/tei:name[@type='supplied']",dataInt,"dc:contributor",idVal)
    
    #non-literal FOAF
    #df<-bindElements(df,"/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author/tei:name[@type='authority']",dataInt,"dcterms:creator",idVal)
    #non-literal FOAF
    #df<-bindElements(df,"/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author/tei:name[@type='supplied']",dataInt,"dcterms:contributor",idVal)
    
    
    #http://dublincore.org/documents/2012/06/14/dcmi-terms/?v=terms#provenance
    #non-literal ex. [ rdfs:label "Originally located at the entrance to the Luxor temple the obelisk came to Paris in 1836 as a gift by Muhammad Ali Pasha." ]
    df<-bindElements(df,"/tei:TEI/tei:teiHeader/tei:fileDesc/tei:sourceDesc/tei:msDesc/tei:history/tei:provenance",dataInt,"dcterms:provenance",idVal)
    
    df<-bindElements(df,"/tei:TEI/tei:teiHeader/tei:fileDesc/tei:sourceDesc/tei:msDesc/tei:history/tei:acquisition",dataInt,"acquisition",idVal)
    
    #non-literal rdfs:label
    #  df<-bindElements(df,"/tei:TEI/tei:teiHeader/tei:profileDesc/tei:textClass/tei:keywords/tei:list/tei:item",dataInt,"dcterms:subject",idVal)
    
    #literal
    df<-bindElements(df,"/tei:TEI/tei:teiHeader/tei:profileDesc/tei:textClass/tei:keywords/tei:list/tei:item",dataInt,"dc:subject",idVal)
    
    
    #http://purl.org/dc/elements/1.1/language
    #non-literal dcterms:LinguisticSystem
    #  df<-bindAttribute(df,"/tei:TEI/tei:teiHeader/tei:fileDesc/tei:sourceDesc/tei:msDesc/tei:msContents/tei:textLang/@mainLang",dataInt,"dcterms:language",idVal)
    
    #literal
    df<-bindAttribute(df,"/tei:TEI/tei:teiHeader/tei:fileDesc/tei:sourceDesc/tei:msDesc/tei:msContents/tei:textLang/@mainLang",dataInt,"dc:language",idVal)
    
    
    #http://purl.org/dc/terms/publisher
    #non-literal
    #df<-bindElements(df,"/tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:publisher",dataInt,"dcterms:publisher",idVal)
    
    #literal
    df<-bindElements(df,"/tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:publisher",dataInt,"dc:publisher",idVal)
    
    #http://purl.org/dc/elements/1.1/date
    #http://purl.org/dc/terms/temporal
    #non-literal dcterms:temporal dcterms:PeriodOfTime
    df<-bindAttribute(df,"/tei:TEI/tei:teiHeader/tei:fileDesc/tei:sourceDesc/tei:msDesc/tei:history/tei:origin/@notBefore",dataInt,"dateRangeStart",idVal)
    df<-bindAttribute(df,"/tei:TEI/tei:teiHeader/tei:fileDesc/tei:sourceDesc/tei:msDesc/tei:history/tei:origin/@notAfter",dataInt,"dateRangeStop",idVal)
    df<-bindElements(df,"/tei:TEI/tei:teiHeader/tei:fileDesc/tei:sourceDesc/tei:msDesc/tei:history/tei:origin/tei:origDate",dataInt,"dateRangeDescription",idVal)
    
    #literal
    df<-bindElements(df,"/tei:TEI/tei:teiHeader/tei:fileDesc/tei:sourceDesc/tei:msDesc/tei:history/tei:origin/tei:origDate",dataInt,"dc:coverage",idVal)
    
  }
  df
}
bindElements<-function(df,xpath,dataInt,name,idVal){
  #  ns<-c(tei="http://www.tei-c.org/ns/1.0")  
  elements=xpathSApply(dataInt,xpath,xmlValue,namespaces=ns)
  if(length(elements)>0){
    dfElements<-data.frame(element=elements)
    dfElements$idVal<-idVal
    dfElements$name<-name
    df<-rbind(df,dfElements)  
  }
  df
}

bindAttribute<-function(df,xpath,dataInt,name,idVal){
  #  ns<-c(tei="http://www.tei-c.org/ns/1.0")  
  attrs=xpathSApply(dataInt,xpath,namespaces=ns)
  if(length(attrs)>0){
    dfAttrs<-data.frame(element=attrs)
    dfAttrs$idVal<-idVal
    dfAttrs$name<-name
    df<-rbind(df,dfAttrs)  
  }
  df
}

loadAllFiles<-function(dir){
  library(plyr)
  allFiles<-list.files(dir,full.name=TRUE,pattern="*.xml")
  ldply(allFiles,loadFile)
}

df<-loadAllFiles("source/")
dfDates<-subset(df,df$name %in% c("dateRangeStart","dateRangeStop","dateRangeDescription","dcterms:title"))
dfDates<-dcast(dfDates,idVal ~ name,value.var="element")
dfDates$dateRangeStart<-as.numeric(dfDates$dateRangeStart)
dfDates$dateRangeStop<-as.numeric(dfDates$dateRangeStop)
dfKeywords<-subset(df,df$name == "dc:subject")
keywords<-unique(subset(df,df$name=="dc:subject",element)[,"element"])
kw<-as.character(keywords)
names(kw)<-kw


shinyServer(function(input, output,session) {
  
  updateSelectizeInput(session, 'keywords', server = FALSE, choices =  kw)
  library(reshape2)
  output$view <- DT::renderDataTable({
    validIds=dfDates$idVal
    if(is.null(input$keywords)==FALSE){
      validIds=subset(dfKeywords,dfKeywords$element %in% as.list(input$keywords),idVal)
    }
    subset(dfDates,dfDates$dateRangeStart>as.numeric(input$dateRange[1]) & dfDates$dateRangeStop<as.numeric(input$dateRange[2]) & dfDates$idVal %in% validIds,select=-c(dateRangeStart,dateRangeStop))
  })
})