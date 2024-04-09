*:**************************************************************************
*: Program file  : APCAT10.PRG
*: Program desc. : Vandor File Master Conversion 
*: System        : Aria Advantage Series.
*: Module        : Account Payable (AP)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Reference     : C102297
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*

*lcTextDir  --> Variable to hold the directory and path of the Text file.
*lnTextHand --> Variable to check if the Text file is opened correctly. 
*llFactOpen --> Variable Flag to detect whether Factor Master file is opened or not.
*llVendOpen --> Variable Flag to detect whether Vendor Master file is opened or not.
*llVenHstOp --> Variable Flag to detect whether Vendor History file is opened or not.
*llFisHdOpn --> Variable Flag to detect whether Fiscal Header file is opened or not.
*llCodeOpen --> Variable Flag to detect whether Codes file is opened or not.
*lcPrevYear --> Variable to hold the Previous Fiscal Year.
*lcCurrYear --> Variable to hold the Current Fiscal Year.
*lcNextYear --> Variable to hold the Next Fiscal Year.
*llAccExist --> Variable Flag to detect that this account , store exist for the same Customer.
*llVenExist --> Variable Flag to check if Vendor already exist.
*lcExceptTb --> Variable to hold the table for the exceptional file.
*lcTextLine --> Variable to hold the current Text line number.
*lcVendCode --> Variable to hold the current Vendor code.
*lcVencomp  --> Variable to hold the current Vendor company name.
*llVendLoop --> Variable Flag to be used when vendor already exist , loop till next vendor.

PRIVATE lcTextDir , lnTextHand , llVendOpen , llVenHstOp , llFisHdOpn , llCodeOpen , ;
        lcPrevYear , lcCurrYear , lcNextYear , llAccExist , lcExceptTb , llVenExist , ;
        lcTextLine , lcVendCode , lcVencomp , lcAddress1 , lcAddress2 , lcAddress3 , ;
        lcAddress4 , lcAddress5 , lcFactAdd1 , lcFactAdd2 , lcFactAdd3 , lcFactAdd4 ,;
        lcFactAdd5 , llVendLoop , llFactOpen , lcReason , lnCount
        
lcTextDir = GETFILE('TXT','Select your Text File','Select')

*-- Checking if User pressed Cancel.
IF EMPTY(lcTextDir)
  *-- Message : < Operation has been cancelled >
  *-- Buttons : <             OK               >
  =gfModalGen("TRM000000B00000","DIALOG",'','','Operation has been cancelled.')  
  RETURN
ENDIF

*-- Checking if User selected File not of Type Excel Extension.
IF !(RIGHT(lcTextDir,3)=="TXT")
  *-- Message : < Only Text files are allowed for conversion >
  *-- Buttons : <                      OK                     >
  =gfModalGen("TRM000000B00000","DIALOG",'','','Only Text files are allowed for conversion.')  
  RETURN
ENDIF

*-- Opening Fiscal Year file.
llFisHdOpn = gfOpenFile(gcDataDir+'FisHd','','SH')

*-- First we need to get Current Year from Fiscal Header file
SELECT FisHd
LOCATE FOR cFisYStat = "C"
IF FOUND()
  lcCurrYear = cFisFYear
  lcPrevYear = ALLTRIM(STR(VAL(cFisFYear)-1))
  lcNextYear = ALLTRIM(STR(VAL(cFisFYear)+1))
ELSE
  *-- Message : <Current year not found in fiscal year file, Check this file and try again!>
  *-- Buttons : <                                    OK                                    >
  =gfModalGen("TRM000000B00000","DIALOG",'','','Current year not found in fiscal year file, Check this file and try again!')  
  
  *-- Close Fiscal year file bebore closing the progarm.
  IF llFisHdOpn
    USE IN FisHd
  ENDIF
  RETURN
ENDIF

*-- Close Fiscal Year File (not used anymore).
IF llFisHdOpn
  USE IN FisHd
ENDIF

*-- Open the Text file. 
lnTextHand = FOPEN(lcTextDir,0)

*-- If Text file not opened successfully
IF lnTextHand < 0
  *-- Message : <Text file was not opened successfully, Check if this file is already opened and try again!>
  *-- Buttons : <                                    OK                                    >
  =gfModalGen("TRM000000B00000","DIALOG",'','','Text file was not opened successfully, Check if this file is already opened and try again!')  
  RETURN
ENDIF

*-- Create the Exceptional file
lcExceptTb = gfTempName()
CREATE TABLE (gcWorkDir+lcExceptTb) (cVendCode C(8) , cReason C(150))

*-- Opening Codes file to check for some Codes
llCodeOpen = gfOpenFile(gcDataDir+'Codes',gcDataDir+'cCode_No','SH')

*-- Opening Factor Master , Vendor Master , Vendor History files.
llFactOpen = gfOpenFile(gcSysHome+'SycFact',gcSysHome+'cFacCode','SH')
llVendOpen = gfOpenFile(gcDataDir+'ApVendor',gcDataDir+'VenCode','SH')
llVenHstOp = gfOpenFile(gcDataDir+'ApVenHst','','SH')

STORE .F. TO llVendLoop , llVenExist     && First there is no Vendor exist before 

*-- Do While loop till end of text file. 
DO WHILE !FEOF(lnTextHand)
  lcTextLine = FGETS(lnTextHand)

  IF llVendLoop
    IF OCCURS("VENDOR # - ", lcTextLine) > 0
      llVendLoop = .F.      
    ELSE
      LOOP          && Loop again till we find the next starting vendor line.
    ENDIF
  ENDIF  

  DO CASE
    *-- Case word 'VENDOR # -' is found.
    CASE OCCURS("VENDOR # - ", lcTextLine) > 0
      lcVendCode = ALLTRIM(SUBSTR(lcTextLine,12,8))
      lcVenComp  = ALLTRIM(SUBSTR(lcTextLine,27))      
      
      *-- If this vendor code already exist. 
      IF SEEK(lcVendCode,'ApVendor')
        llVenExist = .T.
        llVendLoop = .T.
        *we have to add a record in the exceptional file to inform the user of this      
        lcReason = "Vendor # " + lcVendCode + " already exists in Vendor file."
        INSERT INTO (lcExceptTb) (cVendCode , cReason ) VALUES (lcVendCode , lcReason) 
      ELSE
        SELECT ApVendor
        APPEND BLANK
        REPLACE cVendCode WITH lcVendCode , cVenComp WITH lcVenComp , ;
                cCont_Code WITH gcContCode , cAddress6 WITH gccontCode , ;
                cVenPrior WITH "1" , cVenPmeth WITH "P", cTaxType WITH 'T' ,;
                cCurrCode WITH gcBaseCurr , cVenPyType WITH "A" 

        *-- Adding the Vendors data in Vendor History file.
        INSERT INTO ApVenHst (cVendCode , cFisFyear) VALUES (lcVendCode , lcPrevYear)
        INSERT INTO ApVenHst (cVendCode , cFisFyear) VALUES (lcVendCode , lcCurrYear)
        INSERT INTO ApVenHst (cVendCode , cFisFyear) VALUES (lcVendCode , lcNextYear)

        WAIT WINDOW "Adding Vendor # " + lcVendCode + " to Vendor file." NOWAIT
      ENDIF
      
      LOOP
    *-- Case word 'SHIP FROM:' is found.
    CASE OCCURS("SHIP FROM:", lcTextLine) > 0
      STORE '' TO lcAddress1 , lcAddress2 , lcAddress3 , lcAddress4 , lcAddress5 , ;
                  lcFactAdd1 , lcFactAdd2 , lcFactAdd3 , lcFactAdd4 , lcFactAdd5
      lcTextLine = FGETS(lnTextHand)             && Get the whole line first.
      lcAddress1 = ALLTRIM(SUBSTR(lcTextLine,1,50))
      lcFactAdd1 = ALLTRIM(SUBSTR(lcTextLine,51)) 

      lcTextLine = FGETS(lnTextHand)             && Get the whole line first.
      lcAddress2 = ALLTRIM(SUBSTR(lcTextLine,1,50))
      lcFactAdd2 = ALLTRIM(SUBSTR(lcTextLine,51)) 

      lcTextLine = FGETS(lnTextHand)             && Get the whole line first.
      lcAddress3 = ALLTRIM(SUBSTR(lcTextLine,1,50))
      lcFactAdd3 = ALLTRIM(SUBSTR(lcTextLine,51)) 

      lcTextLine = FGETS(lnTextHand)             && Get the whole line first.
      lcAddress5 = ALLTRIM(SUBSTR(lcTextLine,1,50))
      lcFactAdd5 = ALLTRIM(SUBSTR(lcTextLine,51)) 

      *-- If Factor addresses are the same as vendor addresses.
      *IF (lcFactAdd1 = lcVenComp OR lcFactAdd1 $ lcAddress1 OR lcFactAdd2 $ lcAddress1 OR ;
      *   lcAddress1 $ lcFactAdd1 OR lcAddress1 $ lcFactAdd2 OR lcFactAdd1 $ lcAddress2 OR ;
      *   lcAddress2 $ lcFactAdd1)
      *  IF !EMPTY(lcFactAdd1) AND !(lcFactAdd1 = lcVenComp)
      *    lcAddress1 = lcFactAdd1
      *    lcAddress2 = lcFactAdd2
      *    lcAddress3 = lcFactAdd3
      *    lcAddress4 = lcFactAdd4
      *    lcAddress5 = lcFactAdd5
      *  ENDIF
      *ELSE
      *  *-- we have to add a new factor code
      *  IF !SEEK(SUBSTR(lcFactAdd1,1,6),'SycFact')
      *    DO lpCheckAdr WITH "lcFactAdd" , "Factor"
      *    SELECT SycFact
      *    APPEND BLANK
      *    REPLACE cFacCode WITH SUBSTR(lcFactAdd1,1,6) , cAddress1 WITH lcFactAdd1 , ;
      *            cAddress2 WITH lcFactAdd2 , cAddress3 WITH lcFactAdd3 , ;
      *            cAddress4 WITH lcFactAdd4 , cAddress5 WITH lcFactAdd5          
      *  ENDIF  
      *ENDIF
      
      DO lpCheckAdr WITH "lcAddress" , "Vendor"
      SELECT ApVendor      
      REPLACE cAddress1 WITH lcAddress1 , cAddress2 WITH lcAddress2 , ;
              cAddress3 WITH lcAddress3 , cAddress4 WITH lcAddress4 , ;
              cAddress5 WITH lcAddress5      
      
      LOOP
    *-- Case word 'TELE:' is found.
    CASE OCCURS("TELE:", lcTextLine) > 0
      SELECT ApVendor
      *-- Remove any Hyphens "-" from the telephone number
      REPLACE cPhoneNo WITH STRTRAN(ALLTRIM(SUBSTR(lcTextLine,6,16)),"-","") , ;
              cVenCont WITH ALLTRIM(SUBSTR(lcTextLine,36))
      LOOP    
    *-- Case line is empty or anything else.
    OTHERWISE
      LOOP
  ENDCASE
  
ENDDO
*-- EndDo While loop of the text file 

= FCLOSE(lnTextHand)      && close the text file with low level function
WAIT CLEAR

*-- Export the Excel File to Excel after notifing the User.
IF llVenExist
  PRIVATE lcExcptDir

  *-- Message : < Some Vendors have duplicate record , So you will be prompted to export
  *--             this to an Excel file. >
  *-- Buttons : <                                  OK                                   >
  =gfModalGen("TRM000000B00000","DIALOG",'','','The VENDOR file contains some duplicate vendors or has missing data; you have to export it to an Excel file.')  

  SELECT (lcExceptTb)
  lcExcptDir = PUTFILE('Exceptional file','Except.XLS','XLS')

  IF EMPTY(lcExcptDir)
    *-- Message : < Operation has been cancelled >
    *-- Buttons : <             OK               >
    =gfModalGen("TRM000000B00000","DIALOG",'','','Operation has been cancelled.')  
  ELSE
    EXPORT TO (lcExcptDir) TYPE XLS
  ENDIF

ENDIF

*-- Close the Factor & Vendor Master file.
IF llFactOpen
  USE IN SycFact
ENDIF

IF llVendOpen
  USE IN ApVendor
ENDIF

*-- Close Codes file if opened
IF llCodeOpen
  USE IN CODES
ENDIF

*-- Close the Exceptional File and Erase it
IF USED(lcExceptTb)
  USE IN (lcExceptTb)
ENDIF
ERASE (gcWorkDir + lcExceptTb + ".DBF")
ERASE (gcWorkDir + lcExceptTb + ".CDX")

*-- Notify that the conversion is completed.
*-- Message : < Conversion has been completed successfully. >
*-- Buttons : <                      OK                     >
=gfModalGen("TRM000000B00000","DIALOG",'','','Conversion has been completed successfully.')
*-- End of Program

*!***************************************************************************
*! Name      : lpCheckAdr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/21/2001
*! Purpose   : Check for Address validation whatever Vendor address or Factor 
*!***************************************************************************
*! Passed Parameters : 
*!                    lcAddrLine --> whether Vendor or Factor addresses.
*!                    lcVendFact --> whether Vendor or Factor.
*!***************************************************************************
*! Example   : lpCheckAdr
*!***************************************************************************
*
PROCEDURE lpCheckAdr
PARAMETERS lcAddrLine , lcVendFact
PRIVATE lcEvalAdr1 , lcEvalAdr2 , lcEvalAdr3 , lcEvalAdr4 , lcEvalAdr5

lcEvalAdr1 = lcAddrLine + "1"
lcEvalAdr2 = lcAddrLine + "2"
lcEvalAdr3 = lcAddrLine + "3"
lcEvalAdr4 = lcAddrLine + "4"
lcEvalAdr5 = lcAddrLine + "5"

*-- Checking if Address line 3 is empty.
IF EMPTY(&lcEvalAdr3)

  *-- Checking for the Comma sign in Address line 2.
  IF "," $ &lcEvalAdr2
    &lcEvalAdr3 = ALLTRIM(SUBSTR(&lcEvalAdr2,1,ATC(',',&lcEvalAdr2)-1))
    &lcEvalAdr4 = ALLTRIM(SUBSTR(&lcEvalAdr2,ATC(',',&lcEvalAdr2)+1))
    &lcEvalAdr2 = ''
          
    *-- If length of address 4 greater than 2 
    IF LEN(&lcEvalAdr4) > 2
      *-- Checking for State in Codes file
      IF SEEK('N'+PADR('STATE',10),'Codes')
        SELECT Codes
        LOCATE FOR ALLTRIM(UPPER(cDiscrep)) = &lcEvalAdr4
        IF FOUND()
          &lcEvalAdr4 = ALLTRIM(cCode_No)  
        ELSE
          &lcEvalAdr4 = ''
          *-- we have to add a code in the excepional file.
          lcReason = "Address 4 is not added for " + lcVendFact + " # " + lcVendCode + " because it is a state description and it is not found in the codes file."
          INSERT INTO (lcExceptTb) (cVendCode , cReason ) VALUES (lcVendCode , lcReason) 
          llVenExist = .T.
        ENDIF
      ENDIF                    
    ENDIF
  
  ELSE        && there is no comma sign.
    lnCount = 1
    DO WHILE .T.

      &lcEvalAdr4  = ALLTRIM(SUBSTR(&lcEvalAdr2,RAT(SPACE(1),&lcEvalAdr2,lnCount)))

      *-- If length of address 4 greater than 2 
      IF LEN(&lcEvalAdr4) > 2            
        IF SEEK('N'+PADR('STATE',10),'Codes')
          SELECT Codes
          LOCATE FOR ALLTRIM(UPPER(cDiscrep)) = &lcEvalAdr4
          IF FOUND()
            &lcEvalAdr3 = ALLTRIM(SUBSTR(&lcEvalAdr2,1,RAT(SPACE(1),&lcEvalAdr2,lnCount)))
            &lcEvalAdr4 = ALLTRIM(cCode_No)  
            &lcEvalAdr2 = ''
          ELSE
            IF lnCount = 1
              lnCount = 2
              STORE '' TO &lcEvalAdr3 , &lcEvalAdr4
              LOOP
            ENDIF  
          ENDIF
        ENDIF
      ELSE
        *-- If found in Codes file we'll consider it as a state code.
        IF SEEK('N'+PADR('STATE',10)+&lcEvalAdr4,'Codes')
          &lcEvalAdr3 = ALLTRIM(SUBSTR(&lcEvalAdr2,1,RAT(SPACE(1),&lcEvalAdr2,lnCount)))
          &lcEvalAdr2 = ''            
        ELSE
          IF lnCount = 1
            lnCount = 2
            STORE '' TO &lcEvalAdr3 , &lcEvalAdr4
            LOOP
          ENDIF  
        ENDIF
      ENDIF
      EXIT
    ENDDO
  ENDIF
  *-- Checking for the Comma sign in Address line 2.
     
ELSE      && Address line 3 is not empty
  *-- Checking for the Comma sign.
  IF "," $ &lcEvalAdr3        
    &lcEvalAdr4 = ALLTRIM(SUBSTR(&lcEvalAdr3,ATC(',',&lcEvalAdr3)+1))
    &lcEvalAdr3 = ALLTRIM(SUBSTR(&lcEvalAdr3,1,ATC(',',&lcEvalAdr3)-1))
          
    *-- If length of address 4 greater than 2 
    IF LEN(&lcEvalAdr4) > 2
      *-- Checking for State in Codes file
      IF SEEK('N'+PADR('STATE',10),'Codes')
        SELECT Codes
        LOCATE FOR ALLTRIM(UPPER(cDiscrep)) = &lcEvalAdr4
        IF FOUND()
          &lcEvalAdr4 = ALLTRIM(cCode_No)  
        ELSE
          &lcEvalAdr4 = ''
          *-- we have to add a code in the excepional file.
          lcReason = "Address 4 is not added for " + lcVendFact + " # " + lcVendCode + " because it is a state code and it is not found in the codes file."
          INSERT INTO (lcExceptTb) (cVendCode , cReason ) VALUES (lcVendCode , lcReason)                 
          llVenExist = .T.
        ENDIF
      ENDIF                    
    ENDIF
  ELSE        && there is no comma sign.

    lnCount = 1
    DO WHILE .T.

      &lcEvalAdr4 = ALLTRIM(SUBSTR(&lcEvalAdr3,RAT(SPACE(1),&lcEvalAdr3,lnCount)))
      &lcEvalAdr3 = ALLTRIM(SUBSTR(&lcEvalAdr3,1,RAT(SPACE(1),&lcEvalAdr3,lnCount)))

      *-- If length of address 4 greater than 2 
      IF LEN(&lcEvalAdr4) > 2
        IF SEEK('N'+PADR('STATE',10),'Codes')
          SELECT Codes
          LOCATE FOR ALLTRIM(UPPER(cDiscrep)) = &lcEvalAdr4
          IF FOUND()
            &lcEvalAdr4 = ALLTRIM(cCode_No)  
          ELSE
            &lcEvalAdr4 = ''
            
            IF lnCount = 1
              lnCount = 2
              LOOP      
            ELSE
              *-- we have to add a code in the excepional file.
              lcReason = "Address 4 is not added for " + lcVendFact + " # " + lcVendCode + " because it is a state description and it is not found in the codes file."
              INSERT INTO (lcExceptTb) (cVendCode , cReason ) VALUES (lcVendCode , lcReason) 
              llVenExist = .T.            
            ENDIF  
          ENDIF
        ENDIF
      ELSE
        *-- If found in Codes file we'll consider it as a state code.
        IF !SEEK('N'+PADR('STATE',10)+&lcEvalAdr4,'Codes')
          &lcEvalAdr4 = '' 
        
          IF lnCount = 1
            lnCount = 2
            LOOP      
          ELSE
            *-- we have to add a code in the excepional file.
            lcReason = "Address 4 is not added for " + lcVendFact + " # " + lcVendCode + " because it is a state code and it is not found in the codes file."
            INSERT INTO (lcExceptTb) (cVendCode , cReason ) VALUES (lcVendCode , lcReason) 
            llVenExist = .T.
          ENDIF  
        ENDIF
      ENDIF
      EXIT
    ENDDO  
    
  ENDIF
ENDIF
*-- Endif of line 3 is empty
*-- End of lpCheckAdr.