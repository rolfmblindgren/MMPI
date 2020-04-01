library(xml2)
library(readr)
options(warn=1)

                                        # setwd("~/prosjekter/R/MMPI")





                                        #xml_replace (
                                        #  
                                        #(xml_find_all(xmlmail,".//w:t[text()='" . fra  .  "']"))[[1]],
                                        #
                                        #read_xml(paste0(docspec,"<w:t>" . til . "</w:t>","</document>"))
                                        #
                                        #)


### main doc

.translate_doc <- function (.doc, .dict, .xmlfile="document.xml", .docspec=c('<document space="preserve" xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006" xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math" xmlns:v="urn:schemas-microsoft-com:vml" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:w10="urn:schemas-microsoft-com:office:word" xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture">', '</document>')) {
  library(xml2)
  library(readr)
  library(tools)

  print(1)
  
  target <- tempdir()
  
  if ( is.character(.dict) && file.exists(.dict) ) {
    dict <- read_csv(.dict,col_names=F)
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

  sapply(.xmlfile,function(.xmlfile){

    
    body <- read_xml(paste0(target,"/word/",.xmlfile))
    
    tabs <-  xml_find_all(body,'.//w:ind[contains(@w:right,1974)]')
    if (length(tabs)>0) {
      
      fra <- tabs[[1]]
      til <- xml_child(read_xml(paste0(.docspec[1],'<w:ind w:left="6514" w:right="1372" w:firstLine="0"/>',.docspec[2])))
      xml_replace(fra, til)
    }


    tabs <-  xml_find_all(body,'.//w:ind[contains(@w:right,2366)]')
    if (length(tabs)>0) {
      
      fra <- tabs[[1]]
      til <- xml_child(read_xml(paste0(.docspec[1],'<w:ind w:left="6514" w:right="1372" w:firstLine="0"/>',.docspec[2])))
      xml_replace(fra, til)
      
    }

    res <- apply(
      data.frame(dict),
      1,
      function(X){
        needle   <- X[[1]]
        haystack <- X[[2]]

        if ( nchar(needle) > 1 && substring(needle,1,1) == "~" ) {

          needle <- substring(needle,2)
          fra <-  xml_find_all(body,paste0(".//w:t[contains(text(),'", needle, "')]"))
          
          haystack <- gsub(needle,haystack,xml_text(fra))
          
        } else {
          fra <- (xml_find_all(body,paste0(".//w:t[text()='", needle, "']")))
        }

        til <- xml_child(read_xml(paste0(.docspec[1],"<w:t>", haystack,  "</w:t>",.docspec[2])))

        for  (f in fra) {
          xml_replace(f, til)
        }


        bm <-  xml_find_all(body,paste0('.//w:bookmarkStart[@w:name="',needle,'"]'))

        if (length(bm)>0){
          
          print(bm)
          print(needle)
          
          bm.id <- xml_attrs(bm)[[1]]["id"]
          bm.name <-  xml_attrs(bm)[[1]]["name"]
          
          if ( needle == bm.name ) {
            bm.til <- xml_child(read_xml(paste0(
              .docspec[1],
              '<w:bookmarkStart w:name="', haystack ,'" w:id="', bm.id ,'"/>',
              .docspec[2])))
            xml_replace(bm[[1]],bm.til)
            
          }
        }
      }
      
    )

    write_xml(body,paste0(target,"/word/",.xmlfile))

  })

  currdir <- getwd()
  
  if ( (images <- regexpr("MMPI|NEO-PI",.doc))[1] > 0 ) {

    thefilesdir <- paste0("./media/", substr(.doc,images[1],images[1]+attr(images,"match.length")-1))
    thefiles <- list.files(thefilesdir,
                           full.names=TRUE)
    
    print(thefilesdir)
    
    mediadir <- paste0(target,"/word/media/")
    
    sapply(thefiles,function(X){
      dest <- paste0(mediadir,basename(X))
      print("Copy to mediadir")
      print(X)
      print(dest)
      file.copy(X,
                dest,overwrite=TRUE)
      print("Copied to mediadir")
    })
  }
  
  setwd(target)
  zip(paste0(destination),files=".",flags="-r9X")
  setwd(currdir)

  print(target)
}

translate_doc <- function (.doc, .dict, .xmlfile="document.xml", .docspec=c('<document space="preserve" xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006" xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math" xmlns:v="urn:schemas-microsoft-com:vml" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:w10="urn:schemas-microsoft-com:office:word" xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture">', '</document>')) {
  print(.doc)
  sapply(.doc,function(X){
    print(X)
    .translate_doc(X,.dict,.xmlfile,.docspec)})
  
}

mmpidocs <- list.files("~/prosjekter/adopsjonsforum/rapporter/2020/01",pattern="MMPI.*docx",full.names=TRUE)

neopidocs <- list.files("~/prosjekter/adopsjonsforum/rapporter/2020/01",pattern="NEO-PI.*docx",full.names=TRUE)


translate_doc(neopidocs,"neo-pidict.csv",c("document.xml","header3.xml","footer1.xml"))

translate_doc(mmpidocs,"mmpidict.csv",c("document.xml","header3.xml","footer1.xml"))
