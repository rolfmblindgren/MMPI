mmpi <- read_delim(input$file1$datapath, 
                   "\t",
                   escape_double = FALSE,
                   trim_ws = TRUE,
                   col_names = input$header)

mmpi <- rename_all(mmpi,list(~str_replace(.,"dimension.","")))
mmpi <- rename_all(mmpi,list(~str_replace(.,".norm","")))

## hor
mmpi <- rename_all(mmpi,list(~str_replace(.,"HOI","EID")))
mmpi <- rename_all(mmpi,list(~str_replace(.,"HOD","THD")))
mmpi <- rename_all(mmpi,list(~str_replace(.,"HOE","BXD")))

## int
mmpi <- rename_all(mmpi,list(~str_replace(.,"SZW","SFD")))
mmpi <- rename_all(mmpi,list(~str_replace(.,"INE","NFC")))
mmpi <- rename_all(mmpi,list(~str_replace(.,"STR","STW")))
mmpi <- rename_all(mmpi,list(~str_replace(.,"ANGST","AXY")))
mmpi <- rename_all(mmpi,list(~str_replace(.,"AERG","ANP")))
mmpi <- rename_all(mmpi,list(~str_replace(.,"VEP","BRF")))
mmpi <- rename_all(mmpi,list(~str_replace(.,"MSP","MSF")))

print("MMPI")
print(mmpi)

terse <- input$terse

res <- sapply(
  mmpi.dims,
  function(X){
    d <- t(mmpi[X])
    ret <- switch(which(mmpi.dims==X),
                  "Name"
                  =
                  d
                 ,
                  
                  "VRIN-r"
                  =
                  sapply(d,function(X){

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
                    } else if ( X >= 65 ) {
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
                  "EID" #EID
                  =
                  sapply(d,function(X){
                    
                    if ( X >= 80 ) {
                      "Client’s responses indicate considerable emotional distress that is likely to be perceived as a crisis."
                    } else if ( X >= 65 ) {
                      "Client’s responses indicate significant emotional distress."
                    } else if ( X >= 39 ) { "" }
                    else {
                      "Client reports better-than-average level of emotional adjustment."
                    } })


                 ,
                  "THD" #THD
                  =
                  sapply(d,function(X){
                    
                    if ( X >= 80 ) {
                      "Client’s responses indicate serious thought dysfunction."
                    } else if ( X >= 65 ) {
                      "Client’s responses indicate significant thought dysfunction."
                    } else {
                      ""
                    }
                  })


                 ,
                  "BXD" #BXD
                  =
                  sapply(d,function(X){
                    
                    if ( X >= 80 ) {
                      "Client’s responses indicate considerable externalizing, acting-out behavior that is very likely to result in marked dysfunction and to have gotten him or her into difficulties."
                    } else if ( X >= 65 ) {
                      "Client’s responses indicate significant externalizing, acting-out behavior, which is likely to have gotten him or her into difficulties."
                    } else if ( X >= 39 ) { "" }
                    else {
                      "Client’s responses indicate a higher-than-average level of behavioral constraint; he or she is unlikely to engage in externalizing, acting-out behavior."
                    } })

                 ,
                  "RCd"
                  =
                  sapply(d,function(X){
                    
                    if ( X >= 80 ) {
                      "Client’s responses indicate he or she is experiencing considerable emotional turmoil, is feeling overwhelmed, and is extremely unhappy, sad, and dissatisfied with his or her life."
                    } else if ( X >= 65 ) {
                      "Client reports being sad and unhappy and being dissatisfied with his or her current life circumstances.."
                    } else if ( X >= 39 ) { "" } else {
                                                 "Client reports a higher-than-average level of morale and life satisfaction. "
                                               } })

                  
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


                  "RC3"
                  =
                  sapply(d,function(X){
                    if ( X >= 65 ) {
                      "Client reports having cynical beliefs about others, being distrustful of others, and believing others look out only for their own interests."
                    } else if ( X >= 40 ) {
                      ""
                    } else {
                      "Client describes others as well intentioned and trustworthy and disavows cynical beliefs about them."
                    }
                  })
                 ,


                  "RC4"
                  =
                  sapply(d,function(X){
                    if ( X >= 65 ) {
                      "Client reports a significant history of past antisocial behavior."
                    } else if ( X >= 40 ) {
                      ""
                    } else {
                      "Client reports a below-average level of past antisocial behavior."
                    }
                  })
                 ,

                  "RC6"
                  =
                  sapply(d,function(X){
                    if ( X >= 80 ) {
                      "At this level, the persecutory thinking likely rises to the level of paranoid delusions."
                    } else if ( X >= 65 ) {
                      "Client’s responses suggest significant persecutory ideation, such as believing that others are seeking to harm him or her."
                    } else {
                      ""
                    }
                  })
                 ,
                  
                  "RC7"
                  =
                  sapply(d,function(X){
                    if ( X >= 65 ) {
                      "Client reports various negative emotional experiences, including anxiety, anger, and fear."
                    } else if ( X >= 40 ) {
                      ""
                    } else {
                      "Client reports a below-average level of negative emotional experience. "
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


                  "RC9"
                  =
                  sapply(d,function(X){
                    if ( X >= 75 ) {
                      "Client reports a considerably above-average level of activation and engagement with his or her environment."
                    } else if ( X >= 65 ) {
                      "Client reports an above-average level of activation and engagement with his or her environment."
                    } else if ( X >= 39 ) {
                      ""
                    } else {
                      "Client reports a below-average level of activation and engagement with his or her environment."
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

                  "NFC"
                  =
                  sapply(d,function(X){
                    if ( X >= 80 ) {
                      "Client reports being very indecisive and inefficacious, believing he or she is incapable of making decisions and dealing effectively with crises, and/or having difficulties dealing with small, inconsequential matters."
                    } else if ( X >= 65 ) {
                      "Client reports being passive, indecisive, and inefficacious and believing he or she is incapable of coping with current difficulties."
                    } else if ( X > 39 ) {
                      ""
                    } else {
                      "Client did not endorse items on indecisiveness and ineffectualness."
                    }
                  })
                 ,
                  
                  "STW"
                  =
                  sapply(d,function(X){
                    if ( X >= 80 ) {
                      "worry, including preoccupation with disappointments, difficulties with time pressure, and specific worries about misfortune and finances."
                    } else if ( X >= 65 ) {
                      "Client reports an above-average level of stress and worry."
                    } else if ( X > 39 ) {
                      ""
                    } else {
                      "Client reports a below-average level of stress and worry."
                    }
                  })
                 ,
                  
                  "AXY"
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
                  "ANP"
                  =
                  sapply(d,function(X){
                    if ( X == 80 ) {
                      "Client reports getting upset easily, being impatient with others, becoming easily angered, and being sometimes overcome by anger."
                    } else if ( X >= 65 ) {
                      "Client reports being anger prone."
                    } else {
                      ""
                    }
                  })
                  
                 ,
                  "BRF"
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
                  "JVP"
                  =
                  sapply(d,function(X){
                    if ( X >= 80 ) {
                      "Client reports a history of juvenile conduct problems such as problematic behavior at school, stealing, and being influenced negatively by peers."
                    } else if ( X >= 65 ) {
                      "Client reports a history of problematic behavior at school."
                    } else {
                      ""
                    }
                  })


                 ,
                  "AGG"
                  =
                  sapply(d,function(X){
                    if ( X >= 80 ) {
                      "Client reports engaging in physically aggressive, violent behavior, including explosive behavior and physical altercations, and enjoying intimidating others."
                    } else if ( X >= 65 ) {
                      "Client reports engaging in physically aggressive, violent behavior and losing control."
                    } else if (X >= 39)  {
                      ""
                    } else {
                      "Client reports a below-average level of aggressive behavior."
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
                  
                  "FML"
                  =
                  sapply(d,function(X){
                    if ( X >= 80 ) {
                      "Client reports conflictual family relationships and lack of support from family members. Negative family attitudes and experiences include having frequent quarrels, disliking family members, feeling unappreciated by family members, and feeling that family members cannot be counted on in times of need."
                    } else if ( X >= 65) {
                      "Client reports conflictual family relationships and lack of support from family members."
                    } else if (X >= 39)  {
                      ""
                    } else {
                      "Client reports a relatively conflict-free past and current family environment."
                    }
                  })
                 ,

                  
                  "IPP"
                  =
                  sapply(d,function(X){
                    if ( X >= 80 ) {
                      "Client reports being unassertive and submissive, not liking to be in charge, failing to stand up for himself or herself, and being ready to give in to others."
                    } else if ( X >= 65 && X <= 69) {
                      "Client reports being unassertive."
                    } else if (X >= 39)  {
                      ""
                    } else {
                      "Client describes himself or herself as having strong opinions, standing up for himself or herself, being assertive and direct, and/or being able to lead others."
                    }
                  })
                 ,


                  "SMV"
                  =
                  sapply(d,function(X){
                    if ( X >= 80 ) {
                      "Client reports not enjoying social events and avoiding social situations, including parties and other events where crowds are likely to gather."
                    } else if ( X >= 65) {
                      "Client reports not enjoying social events and avoiding social situations."
                    } else if (X >= 39)  {
                      ""
                    } else {
                      "Client reports enjoying social situations and events."
                    }
                    
                  })
                 ,

                  "SHY"
                  =
                  sapply(d,function(X){
                    if ( X >= 65 ) {
                      "Client reports being shy, easily embarrassed, and uncomfortable around others."
                    } else if ( X >= 39) {
                      ""
                    } else {
                      "Client reports little or no social anxiety."
                    } 
                  })
                 ,

                  
                  "MIS"
                  =
                  sapply(d,function(X){
                    if ( X == 100 ) {
                      "Client reports disliking people and being around them, preferring to be alone and never having had a close relationship."
                    } else if ( X >= 80) {
                      "Client reports disliking people and being around them, preferring to be alone."
                    } else if (X >= 65)  {
                      "Client reports disliking people and being around them."
                    } else {
                      ""
                    }
                  })

                 ,
                  "AGGR-r"
                  =
                  sapply(d,function(X){
                    if ( X >= 65 ) {
                      "Client reports being interpersonally aggressive and assertive."
                    } else if ( X >= 39) {
                      ""
                    } else {
                      "Client reports being interpersonally passive and submissive."
                    }
                  })


                 ,
                  "PSYC-r"
                  =
                  sapply(d,function(X){
                    if ( X >= 65 ) {
                      "Client reports various experiences associated with thought dysfunction."
                    } else if ( X >= 39) {
                      ""
                    } else {
                      "Client reports no experiences of thought disturbance."
                    }
                  })

                 ,
                  "DISC-r"
                  =
                  sapply(d,function(X){
                    if ( X >= 65 ) {
                      "Client reports various manifestations of disconstrained behavior"
                    } else if ( X >= 39) {
                      ""
                    } else {
                      "Client reports overly constrained behavior."
                    }
                  })
                 ,

                  "NEGE-r"
                  =
                  sapply(d,function(X){
                    if ( X >= 65 ) {
                      "Client reports various negative emotional experiences."
                    } else if ( X >= 39) {
                      ""
                    } else {
                      "Client reports a below-average level of negative emotional experiences."
                    }
                  })
                 ,

                  "INTR-r"
                  =
                  sapply(d,function(X){
                    if ( X >= 65 ) {
                      "Client reports a lack of positive emotional experiences and a tendency to avoid social situations."
                    } else if ( X <= 65 && X >= 39) {
                      ""
                    } else {
                      "Client reports feeling energetic and having many emotionally positive experiences."
                    }
                  })

                  )

    if (terse && all(ret=="")) {
      ret <- NULL
    }
    ret
  }

)

mmpi.dims.active <- mmpi.dims[-which(sapply(res,is.null))]

print("MMPI")
print(mmpi.dims.active)
print(mmpi[mmpi.dims])

df <- data.frame(
  matrix(unlist(res),
         ncol=length(unlist(res))/nrow(mmpi)),
  stringsAsFactors=FALSE)

if ( terse ) {
  colnames(df) <- mmpi.dims.active
} else {
  colnames(df) <- mmpi.dims
}



names <- df[["Name"]]

res <- t(df[-1])
colnames(res) <- names


