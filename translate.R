library(xml2)
library(readr)

                                        # setwd("~/prosjekter/R/MMPI")

doc <- "~/prosjekter/adopsjonsforum/rapporter/2019/03/ANXM_38_MMPI-2-RF_201909151056.docx"

                                        #xml_replace (
                                        #  
                                        #(xml_find_all(xmlmail,".//w:t[text()='" . fra  .  "']"))[[1]],
                                        #
                                        #read_xml(paste0(docspec,"<w:t>" . til . "</w:t>","</document>"))
                                        #
                                        #)


### main doc

translate_doc <- function (.doc, .dict, .xmlfile="document.xml", .docspec=c('<document space="preserve" xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006" xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math" xmlns:v="urn:schemas-microsoft-com:vml" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:w10="urn:schemas-microsoft-com:office:word" xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture">', '</document>')) {
  library(xml2)
  library(readr)
  library(tools)

  target <- tempdir()
  
  if ( is.character(.dict) && file.exists(.dict) ) {
    dict <- read_csv("dict.csv",col_names=F)
  } else if (is.vector(.dict)) {
    .dict = tibble(matrix(.dict,ncol=2))
  } else {
    stop ("dict must be cvs or vector")
  }

  if ( is.character(.doc) && file.exists(.doc) ) {
    unzip (.doc, exdir=target)
  } else {
    stop ("doc must be path to document")
  }

  
  destination <- paste0(file_path_sans_ext(.doc),"-EN.docx") 
  
  body <- read_xml(paste0(target,"/word/",.xmlfile))

  res <- apply(
    data.frame(dict),
    1,
    function(X){
      needle   <- X[[1]]
      haystack <- X[[2]]

      if ( substring(needle,1,1) == "~" ) {
        needle <- substring(needle,2)
        fra <-  (xml_find_all(body,paste0(".//w:t[contains(text(),'", needle, "')]")))

        if ( needle == "Dato" ) {
          haystack <- paste0()
        }

        

        
      } else {

        
        fra <- (xml_find_all(body,paste0(".//w:t[text()='", needle, "']")))
        til <- xml_child(read_xml(paste0(.docspec[1],paste0("<w:t>", haystack,  "</w:t>"),.docspec[2])))

        for  (f in fra) {
          xml_replace(f, til)
        }
      }
    }
  )

  write_xml(body,paste0(target,"/word/",.xmlfile))
  print(target)

  currdir <- getwd()
  setwd(target)
  
  zip(paste0(destination),files=".",flags="-r9X")

  setwd(currdir)

}

translate_doc(doc,"dict.csv")

