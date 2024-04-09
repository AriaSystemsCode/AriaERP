*:************************************************************************
*:
*: Procedure file: ALPKTKEG.PRG
*:
*:         System: ARIA 2.7
*:         Module: Allocation
*:         Author: Hend Ghanem (HBG)
*:      Copyright (c) 
*:  Last modified: 06/09/2000
*:
*:  Procs & Fncts: lfwRepWhen
*:               : lfvOptMsg
*:               : lfwOldVal
*:               : lfvPikTkt
*:               : lfvcustom
*:               : lfvWareCod
*:               : lfGTmpOrdL
*:               : lfAFields
*:               : lfSolSpAdr
*:               : lfAdrShift
*:               : lfEndGroup
*:
*:************************************************************************
*: For custom# C101962  
*:***************************************************************************
*: *B803798,1 HBG 11/7/2000  Fix several bugs in the form layout
*: *B604118,1 AME 01/15/2001  FIX THE bugs of printing the billing address instead of ship to address
*: *B604118,1                 and two bugs in the form layout   
*:***************************************************************************

PRIVATE lnCount,lnOldpLen 

lcTime = TIME()          && Variable to hold the Time

lcSolTName = ''        && Variable to hold the Sold to name
lcShpTName = ''        && Variable to hold the Ship to name
lcShipVia = ''         && Variable to hold the Ship Via Description
lcSeason = ''          && Variable to hold the Season Description
lcSpcInst = ''         && Variable to hold the Special Instructions Description
lcTerms = ''           && Variable to hold the Terms Description
lcDivLName = ''        && Variable to hold the Division long name
llEndGroup = .F.       && Flag to know if we are at the end of the Group
llEndPik = .F.         && Flag to know if we are at the end of this Piktkt
lcPik  = ""            && Variabel Hold the piktkt to check if piktkt change

DECLARE laCompAdd[5,1] , laSoldTo[5,1] , laShipTo[5,1] , laDivLName[1,2]

laCompAdd = ''          && Array to hold the Company address
laSoldTo = ''           && Array to hold the Sold To address
laShipTo = ''           && Array to hold the Ship To address
laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'


SELECT SYCCOMP
SEEK gcAct_Comp
lcCompName = cCom_Name
lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
lcCompFax = cCom_Fax               && Variable to hold the Company Fax
= gfGetAdr('SYCCOMP' , '' , '' , '' , @laCompAdd)

lcCompFax = TRANSFORM(lcCompFax , lcPhonPict)  && Fax No. Pic
lcXphone = TRANSFORM(lcCompPhon , lcPhonPict) && variable hold the phone format to use it
                          && in the forms befor shifting the array.

=lfAdrShift('laCompAdd')
SET ORDER TO WHSLOCST IN WHSLOC

=lfGTmpOrdL()

*B803798,1 HBG 11/7/2000 Calculate the # ogf pages for each Piktkt or Order [End]
SELECT (lcTmpOrdL)
SET ORDER TO TAG (lcTmpOrdL)
SET ORDER TO TAG (lcTmpOrdH) IN &lcTmpOrdH
=lfGetPagNo()
*B803798,1 [End]

SET RELATION TO Order + PikTkt INTO &lcTmpOrdH
SET RELATION TO PikTkt INTO PIKTKT ADDITIVE
SET RELATION TO 'O' + Order INTO ORDHDR ADDITIVE


SET RELATION TO Style INTO STYLE ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SELECT PIKTKT
SET RELATION TO cWareCode INTO WAREHOUS
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE

SELECT (lcTmpOrdL)
LOCATE

IF !USED(lcOGTmpForm)

  *-- To Fix structural CDX not found bug until making global fix [Begin] 
  lcSvErrHan = ON('ERROR')
  ON ERROR
  *-- To Fix structural CDX not found bug until making global fix [END  ] 
      
  USE &gcWorkDir.&lcOGTmpForm..FRX IN 0 ORDER 0 EXCLUSIVE

  *-- To Fix structural CDX not found bug until making global fix [Begin] 
  ON ERROR &lcSvErrHan
  *-- To Fix structural CDX not found bug until making global fix [End  ] 

ENDIF

*-- Replace pagebreak with .T. to print new page per account group.
SELECT (lcOGTmpForm)
LOCATE FOR OBJTYPE = 17
REPLACE PICTURE WITH '"' + gcBmpHome + IIF(RIGHT(gcBmpHome,1)="\","","\") + 'EELOGO.BMP"' 

USE IN (lcOGTmpForm)  && Close file.



SELECT (lcTmpOrdL)
lcPik  = PikTkt
DO gfDispRe WITH EVALUATE ('lcRpName')

*-- HDM Added for Running @SAY[end]

SELECT PIKTKT
SET RELATION TO

*-- Check if the user select to print the piktkt or not to update the prtflag field.[START]
IF gcDevice <> 'SCREEN'
  SELECT (lcTmpOrdL)
  SET ORDER TO TAG (lcTmpOrdU)
  SCAN
    IF SEEK(EVAL(lcTmpOrdL+'.PIKTKT'),"PIKTKT")
      SELECT PIKTKT
      REPLACE PIKTKT.PrtFlag WITH 'P'
    ENDIF  
  ENDSCAN
ENDIF    && End of IF

*--IF The Temp. Order Line file is Opened in one of the work areas
IF USED(lcTmpOrdL)
  SELECT (lcTmpOrdL)
  SET RELATION TO
  USE
ENDIF    && End of IF

*--IF The Temp. Order Header file is Opened in one of the work areas
IF USED(lcTmpOrdH)
  SELECT (lcTmpOrdH)
  SET RELATION TO
  USE
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Hend Ghanem
*! Date      : 14/09/2000
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : gfADel() , lfAFields() , gfGetMemVar() , lfOGShowGet()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfwRepWhen
PRIVATE lnWareHElm , lnVrFltElm , llRefresh

IF !llMultWare
  
  lnWareHElm = ASCAN(laOGFieldN , 'PIKTKT.CWARECODE')      && Variable to hold the number of the array elment for the Warehouse in the Option grid Available for felter array
  
  *--IF The Warehouse is in the Available for felter fields
  IF lnWareHElm > 0
    =gfADel(@laOGFieldN , lnWareHElm , 1)
    =gfADel(@laOGFieldH , lnWareHElm , 1)
  ENDIF    && End of IF
    
  llRefresh = .F.          && Flag to know if we need to refresh the Option Grid or not
  
  *--DO WHILE Loop to loop until the Warehouse is no more in the Variable
  *--filter
  DO WHILE .T.
    lnVrFltElm = ASCAN(laOGVrFlt , 'PIKTKT.CWARECODE')      && Variable to hold the number of the array elment for the Warehouse in the Option grid Variable felter array
    lnVrFltElm = IIF(lnVrFltElm = 0 , 0 , ASUBSCRIPT(laOGVrFlt , lnVrFltElm , 1))
  
    *--IF There is a row for the Warehouse in the Variable filter
    IF lnVrFltElm > 0
      =gfADel(@laOGVrFlt , lnVrFltElm , 1)
      lnOGVarFl = lnOGVarFl - 1        && Variable to hold the number of rows in the variable filter [Used by Option grid]
      llRefresh = .T.
    ELSE
      EXIT
    ENDIF    && End of IF
  ENDDO

  *--IF We are to refresh the option grid
  IF llRefresh
    CLEAR READ
    RETURN
  ENDIF    && End of IF
ENDIF    && End of IF

*--IF The Add notes to order lines seting is set to No
IF gfGetMemVar('M_OR_NOTE') <> 'Y'
  llRpOrdLNt = .F.
  laOGObjCnt[4] = .F.
  =lfOGShowGet('llRpOrdLNt')          && Disable the Print order line note pad Object
ENDIF    && End of IF

DIMENSION laTmpOrdLS[1,4]

=lfAFields('laTmpOrdLS' , 'ORDLINE' , 'LIKE QTY* , ORD* , PIK* , LINENO' +;
                          ' , SCALE , TOTQTY , TOTPIK , PRICE , GROUP' +;
                          ' , STYLE , DYELOT , CUSTPO')
                         
                          
DIMENSION laTmpOrdLS[ALEN(laTmpOrdLS , 1) + 1 , 4]
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 1] = 'cGrupDetal'
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 2] = 'C'
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 3] = 1
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 4] = 0

*-- 2 fields to hold the location "Bin" & Color
DIMENSION laTmpOrdLS[ALEN(laTmpOrdLS , 1) + 1 , 4]
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 1] = 'Location'
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 2] = 'C'
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 3] = 10
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 4] = 0
 
DIMENSION laTmpOrdLS[ALEN(laTmpOrdLS , 1) + 1 , 4]
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 1] = 'Color'
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 2] = 'C'
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 3] = 6
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 4] = 0

DIMENSION laTmpOrdLS[ALEN(laTmpOrdLS , 1) + 1 , 4]
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 1] = 'Desc'
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 2] = 'C'
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 3] = 20
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 4] = 0

*B604118,1 AME [START] INLARGE ARRAY TO ADD CARTOONS FIELD
*DIMENSION laTmpOrdHS[4,4]
DIMENSION laTmpOrdHS[5,4]
*B604118,1 AME [END]
=ACOPY(laTmpOrdLS , laTmpOrdHS , ASCAN(laTmpOrdLS , 'ORDER') , 4 , 1)
=ACOPY(laTmpOrdLS , laTmpOrdHS , ASCAN(laTmpOrdLS , 'PIKTKT') , 4 , 5)
laTmpOrdHS[3 , 1] = 'NWEIGHT'
laTmpOrdHS[3 , 2] = 'N'
laTmpOrdHS[3 , 3] = 10
laTmpOrdHS[3 , 4] = 2
laTmpOrdHS[4 , 1] = 'STATUS'
laTmpOrdHS[4 , 2] = 'C'
laTmpOrdHS[4 , 3] = 1
laTmpOrdHS[4 , 4] = 0

*B604118,1 AME [START] ADD CARTOONS FIELD
laTmpOrdHS[5 , 1] = 'CARTONS'
laTmpOrdHS[5 , 2] = 'N'
laTmpOrdHS[5 , 3] = 5
laTmpOrdHS[5 , 4] = 0
*B604118,1 AME [END]

*B803798,1 HBG 11/7/2000 Add field to hold the # of the pages for each PikTkt or Order [Begin]
*-- Field for the 2nd part of the page no.
DIMENSION laTmpOrdHS[ALEN(laTmpOrdHS, 1) + 1 , 4]
laTmpOrdHS[ALEN(laTmpOrdHS, 1) , 1] = 'PageOf'
laTmpOrdHS[ALEN(laTmpOrdHS, 1) , 2] = 'N'
laTmpOrdHS[ALEN(laTmpOrdHS, 1) , 3] = 6
laTmpOrdHS[ALEN(laTmpOrdHS, 1) , 4] = 0
*B803798,1 [End]

*B803798,1 HBG 11/7/2000 Add field to hold the # of the pages for each PikTkt or Order [Begin]
*-- Field for the 2nd part of the page no.
DIMENSION laTmpOrdHS[ALEN(laTmpOrdHS, 1) + 1 , 4]
laTmpOrdHS[ALEN(laTmpOrdHS, 1) , 1] = 'nLastRec'
laTmpOrdHS[ALEN(laTmpOrdHS, 1) , 2] = 'N'
laTmpOrdHS[ALEN(laTmpOrdHS, 1) , 3] = 6
laTmpOrdHS[ALEN(laTmpOrdHS, 1) , 4] = 0
*B803798,1 [End]

SET ORDER TO PIKTKT IN PIKTKT
SET ORDER TO CUSTOMER IN CUSTOMER
SET ORDER TO WAREHOUS IN WAREHOUS
SET ORDER TO PIKLINE IN PIKLINE
SET ORDER TO ORDHDR IN ORDHDR
SET ORDER TO ORDLINE IN ORDLINE
SET ORDER TO NOTEPAD IN NOTEPAD

*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : Hend Ghanem
*! Date      : 14/09/2000
*! Purpose   : Function to get Optional Message from the User
*!             [Validation function for the Push button Optional Message]
*!*************************************************************
*! Called from : Option Grid    [Optional Message option]
*!*************************************************************
*! Calls       : gfOptMsg()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvOptMsg

PRIVATE laOptMsg
DECLARE laOptMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen

laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[2,1] = 'lcRpMsg2'        && 2nd. line Variable
laOptMsg[3,1] = 'lcRpMsg3'        && 3rd. line Variable
laOptMsg[1,2] = 75                && Line length

=gfOptMsg('laOptMsg')

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Hend Ghanem
*! Date      : 14/09/2000
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfwOldVal

laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value


*!*************************************************************
*! Name      : lfvcustom
*! Developer : Hend Ghanem
*! Date      : 14/09/2000
*! Purpose   : Validation function for the Customer Account field
*!*************************************************************
*! Called from : Customer Account field [Option Grid]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvcustom

PRIVATE lcObjName , lcObjVal , llObjRet

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*--IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvWareCod
*! Developer : Hend Ghanem
*! Date      : 14/09/2000
*! Purpose   : Validation function for the Warehouse Code field
*!*************************************************************
*! Called from : Warehouse Code field [Option Grid]
*!*************************************************************
*! Calls       : gfBrowWare()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvWareCod

PRIVATE lcObjName , lcObjVal

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*--IF The user want to Browse or if the Warehouse he entered is not in the
*--file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , laOldVal , lcObjVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfGTmpOrdL
*! Developer : Hend Ghanem
*! Date      : 14/09/2000
*! Purpose   : Function to colect the neede data in a temp. Order lines file
*!*************************************************************
*! Called from : ALPKTK.PRG
*!*************************************************************
*! Calls       : gfThermo()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
 *! Return      : None
*!*************************************************************
*
FUNCTION lfGTmpOrdL

PRIVATE lcForExp , lnTotRec , lnCurRec , lnRest , lnSavRec , lcOrder,;
 	    lcCDXName, lcFullPath

lcForExp = '!EOF("PIKTKT")' + IIF(EMPTY(lcRpExp) , '' ,;
           ' .AND. (' + lcRpExp + ')')        && Variable to hold the FOR Condition to SCAN for on the ORDLINE file


*--IF The Temp. Bitmap file is Opened in one of the work areas


*--IF The Temp. Order Line file is Opened in one of the work areas
IF USED(lcTmpOrdL)
  USE IN (lcTmpOrdL)
ENDIF    && End of IF

CREATE CURSOR (lcTmpOrdL) FROM ARRAY laTmpOrdLS

ZAP
lcFullPath = SET("FULLPATH")
SET FULLPATH ON
lcCDXName  = STRTRAN(DBF() , ".TMP" , ".CDX")

SET FULLPATH &lcFullPath

*-- Index to use it in updating the prtflag field.[START]
IF gcDevice <> 'SCREEN'
  INDEX ON PikTkt TAG (lcTmpOrdU) UNIQUE
ENDIF  
*-- [END]


INDEX ON PikTkt + Order + Location + cGrupDetal + STR(LineNo , 6) ;
	 TAG (lcTmpOrdL) OF (lcCDXName)


*--IF The Temp. Order Header file is Opened in one of the work areas
IF USED(lcTmpOrdH)
  USE IN (lcTmpOrdH)
ENDIF    && End of IF

CREATE CURSOR (lcTmpOrdH) FROM ARRAY laTmpOrdHS
ZAP
lcFullPath = SET("FULLPATH")
SET FULLPATH ON
lcCDXName = STRTRAN(DBF() , ".TMP" , ".CDX")
SET FULLPATH &lcFullPath
INDEX ON Order + PikTkt ; 
	 TAG (lcTmpOrdH) OF (lcCDXName)

SELECT (lcTmpOrdH)
SET ORDER TO


*-- IF Select by Pick Ticket set the relation between 
*-- PIKTKT table and the needed table
IF lcRpSelBy = "P"
  SELECT PIKTKT
  SET ORDER TO
  SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE
  SET RELATION TO 'O' + Order INTO ORDHDR ADDI
  *--IF We are to Print Order Lines Note Pad
  IF llRpOrdNot
    SET RELATION TO 'B' + Order INTO NotePad ADDITIVE
  ENDIF    && End of IF

ELSE && IF Select by Order set the relation between ORDHDR table and the needed table
  SELECT ORDHDR
  SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE
  SET RELATION TO 'O' + Order INTO ORDLINE ADDI
  *--IF We are to Print Order Note Pad
  IF llRpOrdNot
    SET RELATION TO 'B' + Order INTO NotePad ADDITIVE
  ENDIF    && End of IF

ENDIF

IF lcRpSelBy = "P"
  WAIT 'Selecting picking tickets...' WINDOW NOWAIT
ELSE
  WAIT 'Selecting Sales Orders...' WINDOW NOWAIT
ENDIF  

*-- Scan Selected Alias either Pick Ticket or Order Header

SCAN FOR &lcRpExp
  
     
  *-- IF select by Order , scan ordline for allocated lines
  IF lcRpSelBy = "S"
    SELECT ORDLINE
    SCAN REST WHILE cOrdType + Order + STR(LineNo,6) = ORDHDR.cOrdType + ORDHDR.Order
      IF PIKTKT = "******"
        IF llRpInHOrd
          INSERT INTO (lcTmpOrdH) (Order , PikTkt , Status) VALUES;
                                  (ORDHDR.Order,"******",ORDHDR.Status)
        ELSE
          IF ORDHDR.Status <> 'H'
            INSERT INTO (lcTmpOrdH) (Order , PikTkt , Status) VALUES;
                                    (ORDHDR.Order,"******",ORDHDR.Status)
          ENDIF                              
        ENDIF  
      ENDIF
    ENDSCAN
  ELSE   && IF select by Pick ticket 
    IF llRpInHOrd
      INSERT INTO (lcTmpOrdH) (Order , PikTkt , Status) VALUES;
                              (PIKTKT.Order,PIKTKT.PikTkt,PIKTKT.Status)
    ELSE
      IF ORDHDR.Status <> 'H'
        INSERT INTO (lcTmpOrdH) (Order , PikTkt , Status) VALUES;
                              (PIKTKT.Order,PIKTKT.PikTkt,PIKTKT.Status)
      ENDIF                           
    ENDIF  
  ENDIF      
ENDSCAN

IF lcRpSelBy = "P"
  SET ORDER TO TAG PIKTKT
ENDIF
SET RELATION TO
  
SELECT ORDLINE
SET RELATION TO Style INTO STYLE ADDITIVE
*B604118,1 AME[Start] Add relations between ordline & both of ordhdr & ptitkt
SET RELATION TO "O"+ORDER INTO ORDHDR ADDITIVE
SET RELATION TO PIKTKT INTO PIKTKT ADDITIVE
*B604118,1 AME[End]
SELECT (lcTmpOrdH)
SET ORDER TO TAG (lcTmpOrdH)
lnTotRec = RECCOUNT()      && Varible to hold the Total count to be done [For the thermometer]
lnCurRec = 0               && Varible to hold the current count to be done [For the thermometer]

*--SCAN Loop to scan the Temp. Order Header file FOR Status <> 'X'
*--[Not Released]
SCAN FOR Status <> 'X'
  
    
  lnCurRec = lnCurRec + 1
  lnSavRec = RECNO()
  
  *--IF There is one or more records for this Order in the ORDLINE file
  IF SEEK('O' + &lcTmpOrdH..Order , 'ORDLINE')

    M.cGrupDetal = 'D'
    lcOrder = Order

    SELECT ORDLINE
    *--SCAN Loop to scan the ORDLINE file FOR The current Order and FOR the
    *--Option Grid condition
    *B604118,1 AME [START] SCAN JUST FOR NEEDED RECORDS
    *SCAN REST WHILE cordtype+order+STR(lineno,6) = 'O' + &lcTmpOrdH..Order 
    SCAN REST WHILE cordtype+order+STR(lineno,6) = 'O' + &lcTmpOrdH..Order  FOR IIF(lcRpSelBy = "S",PIKTKT = "******",(!EMPTY(PIKTKT) AND PIKTKT <> "******"))
    *B604118,1 AME [END]
     
      IF !EMPTY(PIKTKT)
        llCont = IIF(PIKTKT = "******",SEEK("O"+ORDER,"ORDHDR"),SEEK(PikTkt,"PIKTKT"))
        IF llCont AND &lcRpExp

          SELECT (lcTmpOrdH)
          SEEK ORDLINE.Order + ORDLINE.PikTkt
          REPLACE nWeight WITH nWeight + (STYLE.nStyWeight * ORDLINE.totpik)
          *B604118,1 AME[START] COUNT CARTONS NUMBER
          REPLACE Cartons WITH Cartons + IIF(STYLE.Qty_Ctn=0,1,CEILING(ORDLINE.totpik/STYLE.Qty_Ctn))
          *B604118,1 AME[END]
          SELECT ORDLINE
          SCATTER MEMVAR MEMO
          IF SEEK(Style+SPACE(6)+cWareCode,'whsloc')
            *B604118,1 ABD - Get the first numeric bin in the whsloc file. [Begin]
            *M.Location = whsloc.cLocation
            lcKEyLoc = Style+SPACE(6)+cWareCode
            SELECT whsloc
            LOCATE REST WHILE Style+Color+cWareCode = lcKEyLoc FOR !EMPTY(cLocation)
            M.Location = whsloc.cLocation
            SCAN REST WHILE Style+Color+cWareCode = lcKEyLoc FOR !EMPTY(cLocation)
                IF ALLTRIM(STR(INT(val(whsloc.cLocation)))) = whsloc.cLocation
                 M.Location = whsloc.cLocation
               ENDIF
            ENDSCAN
            SELECT ORDLINE            
            *B604118,1 ABD [End]
          ELSE
            M.Location = "" 
          ENDIF
          IF SEEK(Style,'STYLE')
            m.Style = STYLE.cstymajor
            m.Color = RIGHT(STYLE.STYLE,6)
            m.Desc  = STYLE.Desc
          ENDIF
          INSERT INTO (lcTmpOrdL)  FROM MEMVAR
        ENDIF
      ENDIF
    ENDSCAN
    
        
    SCATTER MEMVAR MEMO BLANK
    SELECT (lcTmpOrdH)
    
    IF BETWEEN(lnSavRec,1,RECCOUNT())
      GO lnSavRec
    ELSE
      LOCATE
    ENDIF  
    
    STORE Order TO m.Order, lcActOrder
    
    *--IF We are to Print the Order Note Pad
    IF llRpOrdNot
     M.cGrupDetal = 'H'
     lnCurRec = lnCurRec - 1
      SET RELATION TO 'B' + Order INTO NotePad ADDITIVE
      *--SCAN Loop to scan the Temp. Order Header file FOR the current Order
      *--and FOR Status <> 'X' [Not Released]
      SCAN REST WHILE Order = lcActOrder FOR Status <> 'X' AND Status <> 'C'
        lnCurRec = lnCurRec + 1
      ENDSCAN    && End of SCAN Loop
      SET RELATION TO
    ELSE    && Else
      LOCATE REST FOR Order + PikTkt > lcActOrder
    ENDIF    && End of IF
    SKIP -1
  ENDIF    && End of IF
  =gfThermo(lnTotRec , lnCurRec , "Selecting order lines for  " ,;
                       IIF(lcRpSelBy = "P",'pick tickets. Pick ticket # :' + PikTkt,'Orders . Order # :' + Order))
ENDSCAN    && End of SCAN Loop

SELECT ORDLINE
SET RELATION TO

SELECT PIKLINE
*B604118,1 AME [START] MAKE RELATIONS ADDITIVE TO PREVENT OVER LAPPING.
*SET RELATION TO Style INTO STYLE
*SET RELATION TO 'O'+ Order INTO ORDLINE
SET RELATION TO Style INTO STYLE ADDITIVE
SET RELATION TO 'O'+ Order INTO ORDLINE ADDITIVE
*B604118,1 AME [END]

SELECT (lcTmpOrdH)

*--SCAN Loop to scan the Temp. Order Header file FOR Status = 'X' [Released]

SCAN FOR Status $ 'XC'
  
  lnCurRec = lnCurRec + 1
  
  *--IF There is one or more records for this Order in the PIKLINE file
  IF SEEK(&lcTmpOrdH..PikTkt + &lcTmpOrdH..Order , 'PIKLINE')
    M.cGrupDetal = 'D'
    SELECT PIKLINE
    
    *--SCAN Loop to scan the PIKLINE file FOR The current Pick Ticket & Order
    SCAN REST WHILE PikTkt + Order = &lcTmpOrdH..PikTkt + &lcTmpOrdH..Order
    
      SELECT (lcTmpOrdH)
      REPLACE nWeight WITH nWeight + (STYLE.nStyWeight * PIKLINE.totpik)
      *B604118,1 AME[START] COUNT CARTONS NUMBER
      REPLACE Cartons WITH Cartons + IIF(STYLE.Qty_Ctn=0,1,CEILING(PIKLINE.totpik/STYLE.Qty_Ctn))
      *B604118,1 AME[END]
      SELECT PIKLINE
      SCATTER MEMVAR MEMO
      IF SEEK(Style+SPACE(6)+ORDLINE.cWareCode,'whsloc')
        M.Location = whsloc.cLocation
      ELSE
        M.Location = "" 
      ENDIF
      IF SEEK(Style,'STYLE')
        m.Style = STYLE.cstymajor
        m.Color = RIGHT(STYLE.STYLE,6)
        m.Desc  = STYLE.Desc
      ENDIF
      INSERT INTO (lcTmpOrdL)  FROM MEMVAR
    ENDSCAN    && End of SCAN Loop
  ENDIF    && End of IF

  =gfThermo(lnTotRec , lnCurRec , "Selecting order lines for  " ,;
                       IIF(lcRpSelBy = "P",'pick tickets. Pick ticket # :' + PikTkt,'Orders. Order # :' + Order))
ENDSCAN    && End of SCAN Loop


SELECT PIKLINE
SET RELATION TO

*--IF The number of records Processed is less than the Total count to be done
IF lnTotRec > lnCurRec .AND. lnCurRec <> 0
  
  *--FOR Loop to loop for the remains of Total count to be done
  FOR lnRest = lnCurRec TO lnTotRec
    =gfThermo(lnTotRec , lnRest ,;
              "Selecting order lines for  " ,;
              IIF(lcRpSelBy = "P",'pick tickets.','Orders.'))
  ENDFOR    && End of FOR Loop
ENDIF    && End of IF


*!*************************************************************
*! Name      : lfAFields
*! Developer : Hend Ghanem
*! Date      : 14/09/2000
*! Purpose   : Function to places information about the structure of the
*!             a selected table into an array for a selected fields
*!*************************************************************
*! Called from : lfwRepWhen()
*!*************************************************************
*! Calls       : gfTempName()
*!*************************************************************
*! Passed Parameters : 1) The Array name
*!                     2) The File name
*!                     3) The Fields to get information about 
*!                        [You can use keleton <skel>]
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfAFields

PARAMETERS lcArrayNam , lcFile , lcFields

PRIVATE lcOldAlias , lcTempName

*--IF The Array name Parameter is not of type character or empty
IF TYPE('lcArrayNam') <> 'C' .OR. EMPTY(lcArrayNam)
  RETURN
ENDIF    && End of IF

lcFile = IIF(TYPE('lcFile') <> 'C' , '' , lcFile)
lcFields = IIF(TYPE('lcFields') <> 'C' .OR. EMPTY(lcFields) , '' ,;
               'FIELDS ' + lcFields)

lcOldAlias = ALIAS()               && Variable to save the old Alis

*--IF The File name Parameter is not empty and this file is Opened in one
*--of the work areas
IF !EMPTY(lcFile) .AND. USED(lcFile)
  SELECT (lcFile)
ENDIF    && End of IF

*--IF The Fields Parameter is empty
IF EMPTY(lcFields)
  =AFIELDS(&lcArrayNam)
ELSE    && Else
  lcTempName = gfTempName()          && Variable to hold a temp name
  COPY STRUCTURE EXTENDED TO &gcWorkDir.&lcTempName &lcFields
  
  SELECT * FROM &gcWorkDir.&lcTempName INTO ARRAY &lcArrayNam
  
  
  *--IF The temp. file (lcTempName) is Opened in one of the work areas
  *--[The file we used for COPY STRUCTURE EXTENDED]
  IF USED(lcTempName)
    USE IN &lcTempName
  ENDIF    && End of IF
  
  *--IF The temp. file (lcTempName) exist in the Temp. Dir.
  *--[The file we used for COPY STRUCTURE EXTENDED]
  IF FILE(gcWorkDir + lcTempName + '.DBF')
    ERASE &gcWorkdir.&lcTempName..DBF
  ENDIF    && End of IF
ENDIF    && End of IF

*--IF The Variable we used to save the old Alias is not empty
IF !EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfSolSpAdr
*! Developer : Hend Ghanem
*! Date      : 14/09/2000
*! Purpose   : Function to Get the Sold to Address & Ship to Address
*!             & the Description of the Ship Via , Season ,
*!             Special Instructions , Terms
*!*************************************************************
*! Called from : ALPKTKTA.FRX
*!*************************************************************
*! Calls       : gfRltFld() , gfCodDes() , gfGetAdr() , lfAdrShift()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*
FUNCTION lfSolSpAdr
PRIVATE lcDistCntr

llEndGroup = .F.
=gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')

SELECT CUSTOMER
*-- Get the ship via code from the customer file if it's multi store order [ Begin ] 
*--IF Select by Pick Ticket use the account and store from PIKTKT
IF lcRpSelBy = "P"
  SEEK IIF(EMPTY(PIKTKT.STORE),'M','S')+ PIKTKT.Account + PIKTKT.STORE 
ELSE  && IF Select by Order use the account and store from ORDHDR
  SEEK IIF(EMPTY(ORDHDR.STORE),'M','S')+ ORDHDR.Account + ORDHDR.STORE 
ENDIF  
lcShipVia = gfCodDes(IIF(CUSTOMER.nBrkWeight <> 0 .AND.;
                         &lcTmpOrdH..nWeight > CUSTOMER.nBrkWeight ,;
                         CUSTOMER.cAltShpvia ,IIF(ORDHDR.ShipVia ='*',CUSTOMER.ShipVia,ORDHDR.ShipVia)), 'SHIPVIA')

lcSeason = gfCodDes(ORDHDR.Season , 'SEASON')
lcSpcInst = gfCodDes(ORDHDR.SpcInst , 'SPCINST')
lcTerms = gfCodDes(ORDHDR.cTermCode , 'CTERMCODE')

SELECT CUSTOMER
*--IF Select by Pick Ticket use the account and store from PIKTKT
IF lcRpSelBy = "P"
  SEEK IIF(EMPTY(PIKTKT.STORE),'M','S')+ PIKTKT.Account + PIKTKT.STORE 
ELSE && IF Select by Order use the account and store from ORDHDR
  SEEK IIF(EMPTY(ORDHDR.STORE),'M','S')+ ORDHDR.Account + ORDHDR.STORE 
ENDIF

lcSolTName = BTName
lcShpTName = IIF(ORDHDR.Alt_ShpTo , ORDHDR.STName , IIF(EMPTY(DBA) , STName , DBA))

=gfGetAdr('CUSTOMER' , '' , '' , '' , @laSoldTo , '2')

*B803798,1 HBG 11/7/2000 Comment this line because we won't print Phone# [Begin]
*--Get Sold To Phone#
*laSoldTo[5] = ALLTRIM(laSoldTo[5])+ ' Phone# '+ TRANSFORM(Customer.Phone1 , lcPhonPict)
*B803798,1 [End]

=lfAdrShift('laSoldTo')

*--IF ORDHDR.Alt_ShpTo = .T.
IF ORDHDR.Alt_ShpTo
  
  SELECT ORDHDR
  lcShpTName = STName
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  *B803798,1 HBG 11/7/2000 Comment this line because we won't print Phone# [Begin]
  *laShipTo[5] = cAddress5
  *B803798,1 [End]
  
ELSE    && Else
  
  SELECT CUSTOMER
  lcDistCntr = CUSTOMER.Dist_Ctr
  
  *--If there is a distribution center
  IF !EMPTY(lcDistCntr)
    SEEK 'S' + PIKTKT.Account + lcDistCntr
  ENDIF
  
  lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
  *B604118,1 AME[Start] GET THE SHIP ADDRESS INSTEAD OF BILL TO ADDRESS
  *=gfGetAdr('CUSTOMER' , '' , '' , '' , @laShipTo, '2')
  laShipTo[1] =gfGetAdr('CUSTOMER' , '' , '' , '' , 1)  
  laShipTo[2] =gfGetAdr('CUSTOMER' , '' , '' , '' , 2)  
  laShipTo[3] =gfGetAdr('CUSTOMER' , '' , '' , '' , 3)  
  laShipTo[4] =gfGetAdr('CUSTOMER' , '' , '' , '' , 4)  
  *B604118,1 AME[END]

  *--Get Ship To Phone#
  *B803798,1 HBG 11/7/2000 Comment this line because we won't print Phone# [Begin]
  *laShipTo[5] = laShipTo[5] + ' Phone#' + TRANSFORM(Customer.Phone1 , lcPhonPict)
  *B803798,1 [End]
  
ENDIF    && End of IF

=lfAdrShift('laShipTo')

SELECT (lcTmpOrdL)
RETURN ''

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Hend Ghanem
*! Date      : 14/09/2000
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : ALPKTKT.PRG , lfSolSpAdr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfAdrShift

PARAMETERS lcArrayNam

*--FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  
  *--IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*--FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  
  *--IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Hend Ghanem
*! Date      : 14/09/2000
*! Purpose   : Function to Update the End of Group flag and to update
*!             the PrtFlag field in the PIKTKT file if the divice is not
*!             Screen
*!*************************************************************
*! Called from : ALPKTKTA.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*
FUNCTION lfEndGroup

llEndGroup = .T.

*--IF The Divice is not Screen
IF gcDevice <> 'SCREEN'
  REPLACE PIKTKT.PrtFlag WITH 'P'
ENDIF    && End of IF

RETURN ''

*!*************************************************************
*! Name      : lfPktktSet
*! Developer : Hend Ghanem
*! Date      : 14/09/2000
*! Purpose   : Function to set and release the relations needed
*!             for the Pick ticket # field [For the In range]
*!*************************************************************
*! Called from : Picking ticket field [Option Grid]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfPktktSet

PARAMETERS lcParm

IF lcParm = 'S'
  SELECT PIKTKT
  SET RELATION TO 'O' + Order INTO ORDHDR
  SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                      'S' + Account + Store) INTO CUSTOMER ADDITIVE
  LOCATE
ELSE
  SELECT PIKTKT
  SET RELATION TO
ENDIF

*!*************************************************************
*! Name      : lfClrRead
*! Developer : HEND GHANEM
*! Date      : 14/09/2000
*! Purpose   : Refresh the filters in the option grid and select
*!           : the FRX due to the report format.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfClrRead ()
*!*************************************************************
FUNCTION lfClrRead

*-- Refresh the filters in the option grid.
CLEAR READ
*-- End of lfClrRead.


*!*************************************************************
*! Name      : lfSROrd   
*! Developer : Hend Ghanem
*! Date      : 14/09/2000
*! Purpose   : Function to Go top in Order header file needed
*!             for the Order # field [For the In range]
*!*************************************************************
*! Called from : Picking ticket field [Option Grid]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfSROrd   
PARAMETERS lcParm
DO CASE
  CASE lcParm = "S"
	SELECT ORDHDR
	LOCATE
  OTHERWISE
ENDCASE



*!*************************************************************
*! Name      : lfEndPik
*! Developer : Hend Ghanem
*! Date      : 14/09/2000
*! Purpose   : Function to Update the End of Piktkt flag 
*!*************************************************************
*! Called from : ALPKTKEG.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*
FUNCTION lfEndPik

IF lcPik <> Piktkt
  lcPik = Piktkt
  llEndPik = .T.
ELSE
  llEndPik = .F.
ENDIF  

RETURN ''


*!*************************************************************
*! Name      : lfGetPagNo
*! Developer : Hend Ghanem
*! Date      : 14/09/2000
*! Purpose   : Function to Get the # of pages for each Piktkt or Order
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*B803798
FUNCTION lfGetPagNo
PRIVATE lnActvAlas , lnAllPkLns , lnRecNo

*-- lnAllPkLns   && # of lines for each page 
*--              && (include # of lines for each record + # of lines for note pad)

lnActvAlas = SELECT()

SELECT (lcTmpOrdH)
SET ORDER TO TAG (lcTmpOrdH)

SCAN 

  IF SEEK(&lcTmpOrdH..PikTkt + &lcTmpOrdH..Order,lcTmpOrdL)
    STORE 0 TO lnAllPkLns , lnRecNo
    SELECT (lcTmpOrdL)
    SCAN REST WHILE PikTkt + Order + Location + cGrupDetal + STR(LineNo , 6) =;
                    &lcTmpOrdH..PikTkt + &lcTmpOrdH..Order
      lnAllPkLns = lnAllPkLns + 1
      lnRecNo = RECNO()
    ENDSCAN                    

    *-- Calculate No. of lines in this P/T
    lnAllPkLns = lnAllPkLns * 39/8
    IF llRpOrdNot .AND. SEEK('B' + &lcTmpOrdH..Order , 'NOTEPAD') .AND. !EMPTY(ALLTRIM(NOTEPAD.mNotes))
      *-- The page hold 39 line for the notepad in one page and 8 record for PikTkt.
      lnAllPkLns = lnAllPkLns + MEMLINES(ALLTRIM(NOTEPAD.MNOTES))
    ENDIF
  
    SELECT (lcTmpOrdH)
    REPLACE PageOf WITH CEILING(lnAllPkLns/39) , nLastRec WITH lnRecNo
  ENDIF
ENDSCAN
SELECT (lnActvAlas)
*-- End Of lfGetPagNo.
