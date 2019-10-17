library(xml2)
library(readr)

# setwd("~/prosjekter/R/MMPI")

doc <- "ANXM_38_MMPI-2-RF_201909151056.docx"


target <- tempdir()


unzip (doc,exdir=target)

xmlmain <- paste0(target,"/word/document.xml")
xmlheader <- paste0(target,"/word/header3.xml")
body <- read_xml(xmlmain)
header <- read_xml(xmlheader)

docspec <- '<document space="preserve" xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006" xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math" xmlns:v="urn:schemas-microsoft-com:vml" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:w10="urn:schemas-microsoft-com:office:word" xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture">'





#xml_replace (
#  
#(xml_find_all(xmlmail,".//w:t[text()='" . fra  .  "']"))[[1]],
#
#read_xml(paste0(docspec,"<w:t>" . til . "</w:t>","</document>"))
#
#)


### main doc

dict <- read_csv("dict.csv",col_names=F)

res <- apply(
  data.frame(dict),
  1,
  function(X){
    
    fra <- (xml_find_all(body,paste0(".//w:t[text()='", X[[1]], "']")))
    
    for (f in X[2]){
      til <- xml_child(read_xml(paste0(docspec,paste0("<w:t>" ,f,  "</w:t>"),"</document>")))
    }
    xml_replace(fra,til)
    
    
  }
)

write_xml(body,xmlmain)

### headers

#dict <- read_csv("dict.csv",col_names=F)

#res <- apply(
#  data.frame(dict),
#  1,
#  function(X){
#    
#    fra <- (xml_find_all(header,paste0(".//w:t[text()='", X[[1]], "']")))[[1]]
#    til <- xml_child(read_xml(paste0(docspec,paste0("<w:t>" , X[[2]],  "</w:t>"),"</document>")))
#
#
#   xml_replace(fra,til)
#    
#    
#  }
#)
#
#write_xml(header,xmlmain)


srcdir <- getwd()
setwd(target)

zip(paste0(srcdir,"/result.docx"),files=".",flags="-r9X")

setwd(srcdir)
