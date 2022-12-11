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

    validity = c(
        "VRIN-r",
        "TRIN-r",
        "F-r",
        "Fp-r",
        "Fs",
        "FBS-r",
        "RBS",
        "L-r",
        "K-r"),
    hor = c(
        "EID",
        "THD",
        "BXD"
    ),
    rcs = c(
        "RCd",
        "RC1",
        "RC2",
        "RC3",
        "RC4",
        "RC6",
        "RC7",
        "RC8",
        "RC9"
    ),
    som = c(
        "MLS",
        "GIC",
        "HPC",
        "NUC",
        "COG"
    ),
    int = c(
        "SUI",
        "HLP",
        "SFD",
        "NFC",
        "STW", # STR
        "AXY", # ANGST
        "ANP", #AERG
        "BRF", # VEP
        "MSF"
    ),
    ekst = c(
        "JCP",
        "SUB",
        "AGG",
        "ACT"
    ),
    intp = c(
        "FML",
        "IPP",
        "SAV",
        "SHY",
        "DSF"
    ),
    intr = c(
        "AES",
        "MEC"
    ),
    pers = c(
        "AGGR-r",
        "PSYC-r",
        "DISC-r",
        "NEGE-r",
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
    "JCP",
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


