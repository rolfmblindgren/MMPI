mmpi <- read_delim(input$file1$datapath, 
                         "\t", escape_double = FALSE, trim_ws = TRUE, col_names = input$header)
mmpi <- rename_all(mmpi,list(~str_replace(.,"dimension.","")))
mmpi <- rename_all(mmpi,list(~str_replace(.,".norm","")))

terse <- input$terse


res <- sapply(
  mmpi.dims.no,
  function(X){
    d <- t(mmpi[X])
    ret <- switch(which(mmpi.dims.no==X),
                  "Name"
                  =
                  d

                 ,
                  "VRIN-r"
                  =
                  sapply(d,
                         function(X){
                           if ( X >= 80) {
                             "Protocol is invalid due to excessive variable response inconsistency."
                           }
                           else if (X >= 70) {
                             "There is some evidence of variable response inconsistency."
                           } else if (X >= 39) {
                             ifelse ( terse,"","There is evidence of consistent responding.")
                             
                           } else if (X >= 30) {
                             "There is evidence of remarkably consistent responding."
                           } else {}
                           
                           
                         })
                  
                 ,
                  "TRIN-r"
                  =
                  sapply(d,function(X){
                    
                    if ( X >= 80 ) {
                      "Protocol is invalid due to excessive fixed, content-inconsistent true responding."
                    } else if ( X >= 70 ) {
                      "There is some evidence of fixed, content-inconsistent true responding."
                    } else if ( X >= 50 ) {
                      ifelse(terse,"", "There is evidence of consistent responding.")
                    } else {}
                    
                  })
                 ,
                  "F-r"
                  =
                  sapply(d,function(X){
                    
                    if ( X >= 120 ) {
                      "Protocol is invalid. Over-reporting is reflected in an excessive number of infrequent responses."
                    } else if ( X >= 100 ) {
                      "Protocol is invalid. Overreporting of psychological dysfunction is indicated by a considerably larger than average number of infrequent responses."
                    } else if ( X >= 90 ) {
                      "Possible overreporting of psychological dysfunction is indicated by a much larger than average number of infrequent responses."
                    } else if ( X >= 79 ) {
                      "Possible overreporting of psychological dysfunction is indicated by an above-average number of infrequent responses."
                    } else {
                      ifelse(terse,"", "There is no evidence of overreporting.")
                    }
                  })
                  
                 ,
                  "Fp-r"
                  =
                  sapply(d,function(X){
                    
                    if ( X >= 100 ) {
                      "Protocol is invalid. Over-reporting is reflected in a considerably larger than average number of responses rarely endores by individuals with genuine, severe psychopathology."
                    } else if ( X >= 80 ) {
                      "Possible overreporting of psychological dysfunction is indicated by a much larger than average number of of responses rarely endorsed by individuals with genuine, severe psychopathology."
                    } else if ( X >= 70 ) {
                      "Possible overreporting of psychological dysfunction is indicated by an above-average number of responses rarely endorsed by individuals with genuine, severe psychopathology."
                    } else  {
                      ifelse(terse, "", "There is no evidence of overreporting.")
                    }
                  })
                 ,
                  "Fs"
                  =
                  sapply(d,function(X){
                    
                    if ( X >= 100 ) {
                      "Scores on the somatic/Cognitive Scales may be invalid. Over-reporting is indicated by a very unusual combination of responses that is associated with noncredible reporting of somatic and/or cognitive symptoms."
                    } else if ( X >= 80 ) {
                      "Possible overreporting is indicated by an unusual combination of responses that is associated with non-credible reporting of somatic and/or cognitive symptoms."
                    } else  {
                      ifelse(terse,"","There is no evidence of overreporting.")
                    }
                  })
                 ,
                  "FBS-r"
                  =
                  sapply(d,function(X){
                    
                    if ( X >= 100 ) {
                      "Scores on the somatic/Cognitive Scales may be invalid. Over-reporting is indicated by a very unusual combination of responses that is associated with noncredible reporting of somatic and/or cognitive symptoms"
                    } else if ( X >= 80 ) {
                      "Possible overreporting is indicated by an unusual combination of responses that is associated with non-credible reporting of somatic and/or cognitive symptoms."
                    } else  {
                      ifelse(terse,"","There is no evidence of overreporting.")
                    }
                  })
                 ,
                  "RBS"
                  =
                  sapply(d,function(X){
                    
                    if ( X >= 100 ) {
                      "Scores on the Cognitive Complaints (COG) Scale may be invalid. Over-reporting is indicated by a very unusual combination of responses that is associated with noncredible memory complaints."
                    } else if ( X >= 80 ) {
                      "Possible overreporting is indicated by an unusual combination of responses associated with noncredible memory complaints."
                    } else  {
                      ifelse(terse,"","There is no evidence of overreporting.")
                    }
                  })
                  
                 ,
                  "L-r"
                  =
                  sapply(d,function(X){
                    
                    if ( X >= 80 ) {
                      "The protocol may be invalid. Underreporting is indicated by the test taker presenting himself or herself in an extremely positive light by denying minor faults and shortcomings that most people acknowledge."
                    } else if ( X >= 70 ) {
                      "Possible underreporting is indicated by the test taker presenting himself or herself in a very positive light by denying several minor faults and shortcomings that most people acknowledge."
                    } else if ( X >= 60 ) {
                      "Possible underreporting is indicated by the test taker presenting himself or herself in a positive light by denying some minor faults that and shortcomings that most people acknowledge."
                    } else {
                      ifelse(terse,"", "There is no evidence of underreporting.")
                    }
                  })

                 ,
                  "K-r"
                  =
                  sapply(d,function(X){
                    
                    if ( X >= 70 ) {
                      "Underreporting is indicated by the test taker presenting himself or herself as remarkably well adjusted."
                    } else if ( X >= 66 ) {
                      "Possible underreporting is reflected in the test taker presenting himself or herself as very well adjusted."
                    } else if ( X >= 60 ) {
                      "Possible underreporting is indicated by the test taker presenting himself or herself as well adjusted."
                    } else {
                      ifelse(terse,"", "There is no evidence of underreporting.")
                    }
                  })

                 ,
                  "RC1"
                  =
                  sapply(d,function(X){
                    
                    if ( X >= 80 ) {
                      "Client reports a diffuse pattern of physical complaints that span multiple bodily symptoms."
                    } else if ( X >= 65 ) {
                      "Client reports multiple physical complaints that may include gastrointestinal, head pain, and neurological symptoms."
                    } else if ( X >= 40 ) {
                      ""
                    } else {
                      "Client reports a sense of physical well-being."
                    }
                  })


                 ,
                  "RC2"
                  =
                  sapply(d,function(X){
                    if ( X >= 80 ) {
                      "Client's responses indicate considerable emotional distress that is likely to be perceived as a crisis."
                    } else if ( X >= 65 ) {
                      "Client's responses indicate significant emotional distress."
                    } else if ( X >= 40 ) {
                      ""
                    } else {
                      "Client reports better-than average level of emotional adjustment."
                    }
                  })

                 ,
                  "RC8"
                  =
                  sapply(d,function(X){
                    if ( X >= 75 ) {
                      "Client reports  a large number of unusual thoughs and perceptions."
                    } else if ( X >= 65 ) {
                      "Client reports various unusual thoughs and perceptual processes."
                    } else {
                      ""
                    }
                  })

                 ,
                  "SUI"
                  =
                  sapply(d,function(X){
                    if ( X >= 100 ) {
                      "Client reports current suicidal ideation and a history of suicidal ideation and attempts."
                    } else if ( X >= 65 ) {
                      "Client reports a history of suicidal ideation and/or attempts."
                    } else {
                      ""
                    }
                  })

                 ,
                  "HLP"
                  =
                  sapply(d,function(X){
                    if ( X >= 80 ) {
                      "Client reports believeing he or she cannot change and overcome his or her problems and is thus incapable of reaching his or her life goals."
                    } else if ( X >= 65 ) {
                      "Client reports feeling hopeless and pessimistic."
                    } else {
                      ""
                    }
                  })
                  
                 ,
                  "ANGST"
                  =
                  sapply(d,function(X){
                    if ( X >= 100 ) {
                      "Client reports feeling constantly anxious, often feeling that something dreadful is about to happen, being frightened by something every day, and having frequent nightmares."
                    } else if ( X >= 65 ) {
                      "Client reports feeling anxious."
                    } else {
                      ""
                    }
                  })

                 ,
                  "VEP"
                  =
                  sapply(d,function(X){
                    if ( X >= 90 ) {
                      "Client reports multiple fears that significantly restrict normal activity in and outside the home, including fears of leaving home, open spaces, small spaces, the dark, sharp objects, and handling money."
                    } else if ( X >= 65 ) {
                      "Client reports multiple fears that significantly restrict normal activity in and outside the home."
                    } else {
                      ""
                    }
                  })

                 ,
                  "AKT"
                  =
                  sapply(d,function(X){
                    if ( X >= 80 ) {
                      "Client reports episodes of heightened excitement and energy level, uncontrollable mood swings, and lack of sleep."
                    } else if ( X >= 65 ) {
                      "Client reports episodes of heightened excitement and energy level."
                    } else if (X >= 39)  {
                      ""
                    } else {
                      "Client reports a below-average level of energy and activation."
                      }
                  })

                 ,
                  "IPP"
                  =
                  sapply(d,function(X){
                    if ( X >= 80 ) {
                      "Client reports episodes of heightened excitement and energy level, uncontrollable mood swings, and lack of sleep."
                    } else if ( X >= 65 ) {
                      "Client reports episodes of heightened excitement and energy level."
                    } else if (X >= 39)  {
                      ""
                    } else {
                      "Client reports a below-average level of energy and activation."
                      }
                  })

                  )

    if (terse && all(ret=="")) {
      ret <- NULL
    }
    ret
  }

)

mmpi.dims <- which(sapply(res,is.null))

df <- data.frame(matrix(unlist(res),ncol=length(unlist(res))/2),stringsAsFactors=FALSE)


colnames(df) <- mmpi.dims.no[-mmpi.dims]

names <- df[["Name"]]

res <- t(df[-1])
colnames(res) <- names


