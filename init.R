library(shiny)
library(semantic.dashboard)
library(proxy)
library(DT)
options(DT.options = list(pageLength = 60,orientation="landscape"))
library(readr)
library(stringr)
library(dplyr)
library(tools)
library(xtable)

mmpi.dims.no <- list(

  name = c("Name"),

  validity = c("VRIN-r",
               "TRIN-r",
               "F-r",
               "Fp-r",
               "Fs",
               "FBS-r",
               "RBS",
               "L-r",
               "K-r"),
  hor = c(
    ## "HOI",
    ## "HOD",
    ## "HOE"
  ),
  rcs = c(
    "RCd",
    "RC1",
    "RC2",
    ## "RC3",
    ## "RC4",
    ## "RC6",
    "RC7",
    "RC8"
    ## "RC9"
  ),
  som = c(
    ## "UWS",
    ## "MAG",
    ## "KOP",
    ## "NRO",
    ## "KOG"
  ),
  int = c(
    "SUI",
    "HLP",
    ## "SZW",
    ## "INE",
    "STR",
    "ANGST",
    "AERG",
    "VEP"
    ## "MSP"
  ),
  ekst = c(
    "JVP",
    ## "SUB",
    "AGG",
    "AKT"
  ),
  intp = c(
    "FML",
    "IPP",
    ## "SMV",
    ## "SHY",
    "MIS"
  ),
  intr = c(
    ## "AES",
    ##  "MEC"
  ),
  pers = c(
    "AGGR-r",
    "PSYC-r",
    "DISC-r",
    ## "NEGE-r",
    "INTR-r"
  )
)

mmpi.dims <- as.vector(unlist(mmpi.dims.no))


mmpi.dims.en <- c(
  "Name",
  "VRIN-r",
  "TRIN-r",
  "F-r",
  "Fp-r",
  "Fs",
  "FBS-r",
  "RBS",
  "L-r",
  "K-r",
  "EID",
  "THD",
  "BXD",
  "RCd",
  "RC1",
  "RC2",
  "RC3",
  "RC4",
  "RC6",
  "RC7",
  "RC8",
  "RC9",
  "UWS",
  "MAG",
  "KOP",
  "NRO",
  "KOG",
  "SUI",
  "HLP",
  "SZW",
  "INE",
  "STR",
  "ANGST",
  "AERG",
  "VEP",
  "MSP",
  "JVP",
  "SUB",
  "AGG",
  "AKT",
  "FML",
  "IPP",
  "SMV",
  "SHY",
  "MIS",
  "AES",
  "MEC",
  "AGGR-r",
  "PSYC-r",
  "DISC-r",
  "NEGE-r",
  "INTR-r"
)


