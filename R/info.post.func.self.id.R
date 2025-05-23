#? ### ### ### ### ### ### ###
#' @title Compile & print custom function Self-ID Information
#' @description
#' A <tiny> Helper Function that compiles and prints the self-identification 
#' information (i.e. self-id, type, caller, run-time duration etc.) of a custom 
#' R function. This custom function was intended to mainly support the MFMR Suite 
#' of R Functions, but can be utilized as a standalone function in other R packages.
#'
#' @param ssProjID a character (string) identifier of the R Project <script>
#'                 file that called the custom R function execution.
#' @param ssFuncSelfID a character (text string) identifier for the active R 
#'                     Function being called (or being executed). If set to NULL
#'                     the hard-coded "info.post.func.self.id()" function ID 
#'                     (i.e. "MFMR-Self.ID") will be used in the printed result. 
#' @param ssFuncCallerID a character (string) value that identifies the parent
#'                       function that invoked that execution of the active or
#'                       current function.
#' @param siFuncMode01 an integer (long) value that identifies the two run-time 
#'                     states of this `Self-ID` custom function. A value of `1L` 
#'                     sets the function mode to the `ENTER` (start) state; while 
#'                     a value of `0L` sets the function mode to the `EXIT` (stop) 
#'                     run-time state.
#' @param ssFuncType a character (string) value that defines the function type 
#'                   of the active function being executed. This value can be
#'                   compiled with the `MFMRutils::code.classify.func()` function.
#' @param csColorProjID a character (string) value that sets the `ProjID` text
#'                      colour in the formatted text output.
#' @param csColorFuncType a character (string) value that sets the `FuncType` text
#'                        colour in the formatted text output.
#' @param csColorCallerID a character (string) value that sets the `CallerID` text
#'                        colour in the formatted text output.
#' @param csTimeStart a character (string) value that defines the `START-Time` 
#'                    of the active function.
#' @param csTimeStop a character (string) value that defines the `STOP-Time` 
#'                   of the active function.
#' @param sbPrintPretty a logical (boolean) value that defines whether the output
#'                      text (Self-ID Function Info) should be formatted when
#'                      printed to the R Console.
#' @param csFormatDT a character (string) value that specifies how `DateTime` info 
#'                   should be formatted when printed out to the R Console.
#' @param csIconCarat a character (string) value that defines the leading icon 
#'                    (carat) object to be used when printing the `Self-ID` 
#'                    information to the R Console.
#' @param csColorCarat a character (string) value that sets the `Carat Icon` object
#'                     colour in the formatted text output.
#' @param csIconSplit a character (string) value that defines the middle <split> 
#'                    icon (object) that should be used for printing the `Self-ID` 
#'                    information to the R Console.
#' @param csColorMain a character (string) value that sets the `Main` information
#'                    text colour in the formatted text output.
#' @param csColorSplit a character (string) value that sets the `Split` information
#'                     text colour in the formatted text output.
#' @param csColorTimeStamp a character (string) value that sets the `TimeStamp` 
#'                         information text colour in the formatted text output.
#'
#' @returns This function returns a single <abbreviated> character value that
#'          classifies five sizes (or classes) of custom R functions: ...
#'          * "TNY" -> a "Tiny" R Function (less than 50 lines of code);
#'
#' @examples
#' ### Classify your Custom R Function as follows:
#' require(MFMRutils)   # -> Ensures the "MFMRutils" library is installed & loaded
#'
#' info.post.func.self.id()   # -> Set this Code Editor Line Number `CELN`as the 
#'                            #    `siFuncStopCELN` arg value !!!
#'
#' @export
#? ### ### ###
"info.post.func.self.id" <- function(
  ssProjID=NULL, ssFuncCallerID=NULL, siFuncMode01=1L, ssFuncType=NULL,
  csIconCarat=NULL, csColorCarat=NULL, csIconSplit=NULL, csColorSplit=NULL,
  csTimeStart=NULL, csTimeStop=NULL, csFormatDT=NULL, csColorTimeStamp=NULL,
  csColorProjID=NULL, csColorFuncType=NULL, csColorCallerID=NULL, csColorMain=NULL,
  sbPrintPretty=TRUE, ssFuncSelfID=NULL
) {

  ### STEP 01 - Define the "Function Self-ID" tag ... ####
  #            ( this👆 is THE ONLY FUNCTION [in the MFMR Suite of R Functions]
  #            THAT DOES NOT SELF-IDENTIFY !!! )
  ssFuncSelfID_ <- "MFMR-Self.ID";
  ### csTimeSTART_ <- base::Sys.time();
  ##### -> siFuncStartCELN_ <- 10; siFuncStopCELN_ <- 146; 

  
  
  ### STEP 02 - Capture the Dots Function Arguments ... ####
  #            ( the "dots-args" will be handed over in subsequent steps ) ...
  ## vsDotsArgs_ <- base::list(...);

  
  
  ### STEP 03 - Internalize ALL Function Arguments here ... ####
  #            ( i.e. hand-over all to func-args to func-local variables )
  csTimeStamp_ <- NULL;
  coListFuncRes_ <- NULL;   # -> The <final> function outputs <results> object.
  ssFuncType_ <- ssFuncType;
  csBoldANSI_ <- MFMRutils::MFMRFormat$BOLD;
  csResetANSI_ <- MFMRutils::MFMRFormat$RESET; 
  csIconSplit_ <- csIconSplit; csIconCarat_ <- csIconCarat; 
  ssFuncCallerID_ <- ssFuncCallerID; siFuncMode01_ <- siFuncMode01; 
  
  
  
  ### STEP 04 - Prime "Func-Self-ID" tag ... ####
  if (!base::is.null(ssFuncSelfID)) {
    ssFuncSelfID_ <- ssFuncSelfID;
  }
  
  
  
  ### STEP 05 - Prime the DateTime Values ... ####
  csFormatDT_ <- base::ifelse(
    base::is.null(csFormatDT), MFMRutils::MFMRDates$LONGv03, csFormatDT
  );
  csTimeStart_ <- base::ifelse(
    base::is.null(csTimeStart), base::Sys.time(), csTimeStart
  );
  csTimeStop_ <- base::ifelse(
    base::is.null(csTimeStop), base::Sys.time(), csTimeStop
  );
  csTimeStartFORMATTED_ <- base::strftime(
    x = csTimeStart_, format = csFormatDT_
  );
  csTimeStopFORMATTED_ <- base::strftime(
    x = csTimeStop_, format = csFormatDT_
  );
  
  
  
  ### STEP 06 - Prime NB Colour Values ... ####
  csColorMain_ <- base::ifelse(
    base::is.null(csColorMain), 
    MFMRutils::MFMRColors$CyanFORE, csColorMain
  );
  csColorCarat_ <- base::ifelse(
    base::is.null(csColorCarat), 
    MFMRutils::MFMRColors$YellowFORE, csColorCarat
  );
  csColorSplit_ <- base::ifelse(
    base::is.null(csColorSplit), 
    MFMRutils::MFMRColors$YellowFORE, csColorSplit
  );
  csColorProjID_ <- base::ifelse(
    base::is.null(csColorProjID), 
    MFMRutils::MFMRColors$GreenFORE, csColorProjID
  );
  csColorFuncType_ <- base::ifelse(
    base::is.null(csColorFuncType), 
    MFMRutils::MFMRColors$YellowFORE, csColorFuncType
  );
  csColorCallerID_ <- base::ifelse(
    base::is.null(csColorCallerID), 
    MFMRutils::MFMRColors$MagentaFORE, csColorCallerID
  );
  csColorTimeStamp_ <- base::ifelse(
    base::is.null(csColorTimeStamp), 
    MFMRutils::MFMRColors$YellowFORE, csColorTimeStamp
  );
  
  
  
  ### STEP 07 - Extract the String Formatting Setting ... ####
  sbPrintPretty_ <- sbPrintPretty; 
  ## base::ifelse(
  ##   base::is.null(vsDotsArgs_[["sbPrintPretty"]]), 
  ##   FALSE, vsDotsArgs_[["sbPrintPretty"]]
  ## );
  rcf_calc.time.delta_ <- function(csTimeStart, csTimeStop) {
    csTimeDeltaRAW_ <- base::as.numeric(
      csTimeStop - csTimeStart, units = "secs"
    );
    csTimeDelta_ <- base::as.numeric(csTimeDeltaRAW_[[1]]);
    csTimeDeltaRESULT_ <- NULL;
    csTimeDeltaROUND_ <- base::round(csTimeDelta_, 3);
    if (csTimeDeltaROUND_ <= 0.999) {
      ssFloatVals_ <- base::abs(
        csTimeDeltaROUND_ - base::trunc(csTimeDeltaROUND_)
      );
      ssFloatsAsInts_ <- base::sub(
        "^0\\.", "", base::format(ssFloatVals_, scientific = FALSE)
      );
      csTimeDeltaRESULT_ <- base::paste0(
        ssFloatsAsInts_, " milli-secs"
      );
    } else if (csTimeDeltaROUND_ > 0.999 && csTimeDeltaROUND_ <= 60.0) {
      ssIntsONLY_ <- base::trunc(csTimeDeltaROUND_);
      csTimeDeltaRESULT_ <- base::paste0(
        ssIntsONLY_, " secs"
      );
    } else if (csTimeDeltaROUND_ > 60.0 && csTimeDeltaROUND_ <= 3600) {
      ssIntsONLY_ <- base::trunc(csTimeDeltaROUND_);
      ssDeltaSecs_ <- ssIntsONLY_ %% 60;
      ssDeltaMins_ <- base::trunc(ssIntsONLY_ / 60);
      csTimeDeltaRESULT_ <- base::paste0(
        ssDeltaMins_, " mins, ", ssDeltaSecs_, " secs"
      );
    } else if (csTimeDeltaROUND_ > 3600 && csTimeDeltaROUND_ <= 216000) {
      ssIntsONLY_ <- base::trunc(csTimeDeltaROUND_);
      ssDeltaSecs_ <- ssIntsONLY_ %% 60;
      ssDeltaMins_ <- base::trunc(ssIntsONLY_ / 60);
      ssDeltaHrs_ <- base::trunc(ssIntsONLY_ / (60 * 60));
      csTimeDeltaRESULT_ <- base::paste0(
        ssDeltaHrs_, " hrs, ", ssDeltaMins_, " mins, ", ssDeltaSecs_, " secs"
      );
    } else if (csTimeDeltaROUND_ > 216000 && csTimeDeltaROUND_ <= 5184000) {
      ssIntsONLY_ <- base::trunc(csTimeDeltaROUND_);
      ssDeltaSecs_ <- ssIntsONLY_ %% 60;
      ssDeltaMins_ <- base::trunc(ssIntsONLY_ / 60);
      ssDeltaHrs_ <- base::trunc(ssIntsONLY_ / (60 * 60));
      ssDeltaDays_ <- base::trunc(ssIntsONLY_ / (60 * 60 * 24));
      csTimeDeltaRESULT_ <- base::paste0(
        ssDeltaDays_, " days, ", ssDeltaHrs_, " hrs, ", 
        ssDeltaMins_, " mins, ", ssDeltaSecs_, " secs"
      );
    }
    base::return(csTimeDeltaRESULT_);
  }
  
  
  
  ### STEP 08 - Apply the Carat Icon Setting ... ####
  if (!base::is.null(csIconCarat_)) {   # <- Check that "IconCarat" is NOT NULL !!!
    if (sbPrintPretty_) {
      if (csIconCarat_ == "=>" || csIconCarat_ == " => " ||
          csIconCarat_ == "->" || csIconCarat_ == " -> ") {
        csIconCarat_ <- base::paste0(
          " ",                               # -> Add "leading" white space ...
          csColorCarat_,                     # -> Apply specified text colour ...
          MFMRutils::MFMRIcons$ArrowRIGHT,   # -> Assign the MFMR Arrow Icon !!!
          " ",                               # -> Add "trailing" white space ...
          csResetANSI_                       # -> Deactivate text formatting !!!
        );
      } else {
        csIconCarat_ <- base::paste0(
          " ",             # -> Add "leading" white space ...
          csColorCarat_,   # -> Apply specified text colour ...
          csIconCarat_,    # -> Assign the specified Carat Icon !!!
          " ",             # -> Add "trailing" white space ...
          csResetANSI_     # -> Deactivate text formatting !!!
        );
      }
    }
  } else {
    if (sbPrintPretty_) {
      csIconCarat_ <- base::paste0(
        " ",                               # -> Add "leading" white space ...
        csColorCarat_,                     # -> Apply specified text colour ...
        MFMRutils::MFMRIcons$ArrowRIGHT,   # -> Assign the MFMR Arrow Icon !!!
        " ",                               # -> Add "trailing" white space ...
        csResetANSI_                       # -> Deactivate text formatting !!!
      );
    } else {
      csIconCarat_ <- " => ";   # -> Apply a simple <default> Carat Icon !!!
    }
  }
  
  
  
  ### STEP 09 - Apply the "Project-ID" Text Formatting ... ####
  if (!base::is.null(ssProjID)) {
    ssProjID_ <- ssProjID;
  } else {
    ssProjID_ <- base::get0(   # -> Searches the Global Environment of the
      "rssTagProjID_",               #    Active R Session for the <somewhat>
      envir = .GlobalEnv,            #    unique variable name "rssTagProjID_"
      ifnotfound = "UNK. Proj. ID"   #    and extracts the value contained in
    );                               #    that variable (if it exists) ... or
  }                                  #    else returns the "NOT-FOUND" value.
  if (sbPrintPretty_) {
    ssProjID_ <- base::paste0(
      csBoldANSI_,      # -> Apply a BOLD text formatting ... 
      csColorProjID_,   # -> Apply the specified text colour ... 
      ssProjID_,        # -> Add the "Caller-ID" string value !!!
      csResetANSI_      # -> Deactivate text formatting !!!
    );
  }
  
  
  
  ### STEP 10 - Apply the "Split-Icon" Text Formatting ... ####
  if (!base::is.null(csIconSplit_)) {
    if (sbPrintPretty_) {
      csIconSplit_ <- base::paste0(
        csBoldANSI_,     # -> Apply a BOLD text formatting ... 
        csColorSplit_,   # -> Apply the specified text colour ... 
        csIconSplit_,    # -> Add the "Split-Icon" string value !!!
        csResetANSI_     # -> Deactivate text formatting !!!
      );
    }
  } else {
    if (sbPrintPretty_) {
      csIconSplit_ <- base::paste0(
        csBoldANSI_,     # -> Apply a BOLD text formatting ... 
        csColorSplit_,   # -> Apply the specified text colour ... 
        " | ",           # -> Add the <default> "Split-Icon" string value !!!
        csResetANSI_     # -> Deactivate text formatting !!!
      );
    } else {
      csIconSplit_ <- " | ";   # -> Add a <basic> "Split-Icon" string value !!!
    }
  }
  
  
  
  ### STEP 11 - Apply the "Func-Type" Text Formatting ... ####
  if (!base::is.null(ssFuncType_)) {
    if (sbPrintPretty_) {
      ssFuncType_ <- base::paste0(
        csBoldANSI_,        # -> Apply a BOLD text formatting ... 
        csColorFuncType_,   # -> Apply the specified text colour ... 
        ssFuncType_,        # -> Add the "Func-Type" string value !!!
        csResetANSI_        # -> Deactivate text formatting !!!
      );
    }
  } else {
    if (sbPrintPretty_) {
      ssFuncType_ <- base::paste0(
        csBoldANSI_,     # -> Apply a BOLD text formatting ... 
        csColorSplit_,   # -> Apply the specified text colour ... 
        "UNK.",          # -> Add the <default> "Func-Type" string value !!!
        csResetANSI_     # -> Deactivate text formatting !!!
      );
    } else {
      ssFuncType_ <- "UNK.";   # -> Add a <basic> "Func-Type" string value !!!
    }
  }
  
  
  
  ### STEP 12 - Apply the "Caller-ID" Text Formatting ... ####
  if (!base::is.null(ssFuncCallerID_)) {
    if (sbPrintPretty_) {
      ssFuncCallerID_ <- base::paste0(
        csBoldANSI_,        # -> Apply a BOLD text formatting ... 
        csColorCallerID_,   # -> Apply the specified text colour ... 
        ssFuncCallerID_,    # -> Add the "Caller-ID" string value !!!
        csResetANSI_        # -> Deactivate text formatting !!!
      );
    }
  } else {
    if (sbPrintPretty_) {
      ssFuncCallerID_ <- base::paste0(
        csBoldANSI_,        # -> Apply a BOLD text formatting ... 
        csColorCallerID_,   # -> Apply the specified text colour ... 
        "UNK.",             # -> Add the <default> "Caller-ID" string value !!!
        csResetANSI_        # -> Deactivate text formatting !!!
      );
    } else {
      ssFuncCallerID_ <- "UNK.";   # -> Add a <basic> "Caller-ID" string value !!!
    }
  }
  
  
  
  ### STEP 13 - Apply the "Time-Stamp" Text Formatting ... ####
  if (siFuncMode01_ == 1L) {   # -> Apply the ENTER function Info !!!
    if (sbPrintPretty_) {
      csTimeStamp_ <- base::paste0(
        csBoldANSI_,             # -> Apply a BOLD text formatting ... 
        csColorTimeStamp_,       # -> Apply the specified text colour ... 
        csTimeStartFORMATTED_,   # ...
        csResetANSI_             # -> Deactivate text formatting !!!
      );
    } else {
      csTimeStamp_ <- csTimeStartFORMATTED_;
    }
  } else if (siFuncMode01_ == 0L) {   # -> Apply the EXIT function Info !!!
    if (sbPrintPretty_) {
      csTimeStamp_ <- base::paste0(
        csBoldANSI_,            # -> Apply a BOLD text formatting ... 
        csColorTimeStamp_,      # -> Apply the specified text colour ... 
        csTimeStopFORMATTED_,   # ...
        csResetANSI_            # -> Deactivate text formatting !!!
      );
    } else {
      csTimeStamp_ <- csTimeStopFORMATTED_;
    }
  }
  
  
  
  # 4.3.1.2 - Post the `ENTER` notification (Func-Self-ID) text ...
  if (siFuncMode01_ == 1L) {   # -> Apply the ENTER function Info Post !!!
    if (sbPrintPretty_) {
      base::cat(
        base::paste0(
          csIconCarat_, ssProjID_, csIconSplit_,
          base::paste0(
            csBoldANSI_, csColorMain_, "F-START { F-ID: '", csResetANSI_
          ),
          base::paste0(
            csBoldANSI_, csColorCallerID_, ssFuncSelfID_, csResetANSI_
          ),
          base::paste0(
            csBoldANSI_, csColorMain_, "'  <F-Type: '", csResetANSI_
          ), 
          ssFuncType_,
          base::paste0(
            csBoldANSI_, csColorMain_, "'>  Caller: '", csResetANSI_
          ), 
          ssFuncCallerID_,
          base::paste0(
            csBoldANSI_, csColorMain_, "'  <Time: ", csResetANSI_
          ), 
          csTimeStamp_,
          base::paste0(
            csBoldANSI_, csColorMain_, ">  }\n", csResetANSI_
          )
        )
      );
    } else {
      base::cat(
        base::paste0(
          csIconCarat_, ssProjID_, csIconSplit_,
          "F-START {  F-ID: '", ssFuncSelfID_, 
          "'  <F-Type: '", ssFuncType_, "'> ",
          " Caller: '", ssFuncCallerID_,
          "'  <Time: ", csTimeStamp_, ">  }\n"
        )
      );
    }
  } else if (siFuncMode01_ == 0L) {   # -> Apply the EXIT function Info Post !!!
     csDeltaTIME_ <- rcf_calc.time.delta_(csTimeStart_, csTimeStop_);
     if (sbPrintPretty_) {
      base::cat(
        base::paste0(
          csIconCarat_, ssProjID_, csIconSplit_,
          base::paste0(
            csBoldANSI_, csColorMain_, "F-STOP { F-ID: '", csResetANSI_
          ),
          base::paste0(
            csBoldANSI_, csColorCallerID_, ssFuncSelfID_, csResetANSI_
          ),
          base::paste0(
            csBoldANSI_, csColorMain_, "'  <Caller: '", csResetANSI_
          ), 
          ssFuncCallerID_,
          base::paste0(
            csBoldANSI_, csColorMain_, "'>  Time: ", csResetANSI_
          ), 
          csTimeStamp_,
          base::paste0(
            csBoldANSI_, csColorMain_, " ( F-Dur: ", csResetANSI_
          ),
          base::paste0(
            csBoldANSI_, csColorTimeStamp_, csDeltaTIME_, csResetANSI_
          ),
          base::paste0(
            csBoldANSI_, csColorMain_, " ) }\n", csResetANSI_
          )
        )
      );
    } else {
      base::cat(
        base::paste0(
          csIconCarat_, ssProjID_, csIconSplit_,
          "F-STOP { F-ID: '", ssFuncSelfID_, 
          "'  <Caller: '", ssFuncCallerID_, "'> ",
          " Time: ", csTimeStamp_, "",
          " ( F-Dur: ", csDeltaTIME_, " ) }\n"
        )
      );
    }
  }
  
  # 4.3.1.3 - Output the `ENTER` "Func-Self-ID' properties ...
  coListFuncRes_ <- base::list(
    "FuncID" = ssFuncSelfID_, "FuncType" = ssFuncType_, 
    "ProjID" = ssProjID_, "CallerID" = ssFuncCallerID_,
    "FuncSTART" = csTimeStart_, "FuncSTOP" = csTimeStop_
  );
  base::invisible(base::list("SelfID" = coListFuncRes_));
}


