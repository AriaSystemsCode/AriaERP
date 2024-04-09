*:***************************************************************************
*: Program file  : RmrAut.PRG
*: Program desc. : Return Authorization
*: System        : Aria 4 XP
*: Module        : Return Merchandise (RM)
*: Developer     : Ayman Mohammed Ahmed (AYM)
*: Issue #       : N037626
*:***************************************************************************
*: Calls :
*:    Procedures :
*:    Functions  :
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO RmrAut
*:***************************************************************************
*: Modifications:
*: B608098,1 HIA 05/07/2007 stop the relation to the customer file based on the main account [T20070507.0003]
*: B608176,1 HIA 08/08/2007 Fix problem that only the first RA has the notes on it, the rest do not. [T20070712.0030]
*: B608754,1 MMT 12/04/2008 Fix bug of Print Buuton is disabled in Ret. Auth. Screen [T20081014.0013]
*: B609017,1 HES 09/10/2009 Fix bug of wrong sold to address on RA at DCC [T20090907.0001]
*: B609356,1 SMA 09/21/2010 Fix bug of creating empty *.cdx files [T20091027.0093]
*: B609924,1 HIA 05/13/2012 Add new option to Sold To Address from Alternate order Address [T20070507.0003]
*:***********************************************************************************************

#INCLUDE R:\Aria4xp\Reports\AR\arsjour.H

llPrinter = .F.
lcStTime   = TIME()    && Time in which we start collect data.
LOCAL lcTmpFile
LOCAL ARRAY laFileStru[1]


lcRetPrtUp= loOgScroll.gfTempName()
CREATE CURSOR (lcRetPrtUp) (RANO C(6))
INDEX ON RANO TAG RANO of (oAriaApplication.WorkDir+lcRetPrtUp)

IF loOgScroll.llOGFltCh
  =lfCollectData()
ELSE
  IF  ! ("'S'+SCALE INTO SCALE" $ UPPER( SET('RELATION')))
    SELECT Style
    SET RELATION TO
    SET RELATION TO 'S'+SCALE INTO SCALE
  ENDIF
  *B608098,1 HIA stop the relation to the customer file based on the main account [T20070507.0003] [Begin]
  *IF   !("'M'+ACCOUNT INTO CUSTOMER" $UPPER( SET('RELATION')))
  *  SELECT RetAuth
  *  SET RELATION TO

    *SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER, cWareCode INTO WareHous, 'Z'+RetAuth.RANO INTO NotePad
  *ENDIF
  IF   !("cWareCode INTO WareHous" $UPPER( SET('RELATION')))
    SELECT RetAuth
    SET RELATION TO

    SET RELATION TO cWareCode INTO WareHous, 'Z'+RetAuth.RANO INTO NotePad
  ENDIF
  *B608098,1 HIA stop the relation to the customer file based on the main account [T20070507.0003] [End]
  IF  !( "RANO INTO RETAUTH" $ SET('RELATION'))
    SELECT RALINE
    SET RELATION TO
    SET RELATION TO RANO INTO RETAUTH, Style INTO Style
  ENDIF
ENDIF

*! Adjust Report Variables
STORE '' TO lcOldTFrm
lcFrmFrx = IIF(TYPE('lcFrmFrx')<>'U' AND !EMPTY(lcFrmFrx),lcFrmFrx,'' )
lcDivLName = ''
lcReason   = ''
lcAccPhone = ''
llMWHouse = .F.
lcFaxNo = ''
lcCode = ''
STORE .F. TO llAgain
STORE 0 TO lnLast , lnLBLCnt , lnCurLbl,lnRec
STORE 1 TO lnCurrRec
llNoRec    = .F.
lcPrgName  = lcRpForm
llPrntComp = TYPE('llPrntComp') = 'L' .AND. llPrntComp
lcLinkWith = (ALLTRIM(gfGetMemVar('M_WAREHOUS',oAriaApplication.ActiveCompanyID)))
IF lcLinkWith = 'Y'
  llMWHouse = .T.
ELSE
  llMWHouse = .F.
ENDIF
*Define logical variable to determine if it's TOCCA formate or not
IF TYPE('lcTocca')= 'C' AND ALLTRIM(lcTocca) = 'Y'
  llTocca = .T.
  STORE '' TO lcVendCD , lcDunsNo , lcDept
ELSE
  llTocca = .F.
ENDIF

DIMENSION laTmp[6,1]
lcTime=TIME()
DECLARE laWareAdd[6,1]

STORE '' TO lcDivLName,lcLines,lcDummyRel ,lcDummySty

STORE 0 TO lnLastRec,lnLines
*B608176  Display the notes for each RA HIA [Begin]
STORE '' TO lnLastLIN
*B608176  Display the notes for each RA HIA [End]

lnPageLns = 26
llLogo = .F.
IF EMPTY(lcCompName)
  PRIVATE lcAliasNow
  lcAliasNow = ALIAS()
  DECLARE laCompAdd[6,1],laDivLName[1,2],laSoldTo[7,1]
  DECLARE laWareAdd[6,1]
  laCompAdd = ''                   && Array to hold the Company address
  laSoldTo  = ''                   && Array to hold the Sold To address
  laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
  laDivLName[1,2] = 'lcDivLName'
  laWareAdd=''
  *-- Get company Address [begin].
  SELECT SYCCOMP
  SEEK oAriaApplication.ActiveCompanyID
  lcCompName = cCom_Name             && Company Name.
  lcCompPhon = cCom_Phon             && Company Phone.
  lcPhonPict = gfPhoneTem()          && Company Phone Picture Format.
  laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
  laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
  laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
  laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
  laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
  laCompAdd[6] = 'Phone# : '+ TRANSFORM(lcCompPhon , '@R ' + lcPhonPict)
  =lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
  * Variable to hold DUNS# For Tocca
  IF llTocca
    lcDunsNo = gfGetMemVar('XDUNS')
  ENDIF
ENDIF

*: B609924,1 HIA 05/13/2012 Add new option to Sold To Address from Alternate order Address [T20070507.0003][Begin]
&& As Per MMT
  =lfOptProg()
*: B609924,1 HIA 05/13/2012 Add new option to Sold To Address from Alternate order Address [T20070507.0003][End]

lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Arsjour_SelectMsg +' '+ ALLTRIM(STR(RECCOUNT('RALINE'))) + LANG_Arsjour_RecInMsg + ALLTRIM(STR(lnInterval,6,2)) + LANG_Arsjour_SecondMsg TIMEOUT 2
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SelectMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_SelectMsg",AHEADERFILE)) +' '+ ALLTRIM(STR(RECCOUNT('RALINE'))) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_RecInMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_RecInMsg",AHEADERFILE)) + ALLTRIM(STR(lnInterval,6,2)) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SecondMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_SecondMsg",AHEADERFILE)) TIMEOUT 2
*N000682,1 11/20/2012 MMT Globlization changes[End]


loogScroll.cCROrientation = 'P'

DO CASE

  CASE lcRpType = 'S'
    =lfPrepHDR()
    lcRpForm = lfGetForm('RS')
    lcOldTFrm = lcOGTmpForm
    IF lcFrmFrx = 'Y'
      lcSavPAdv = _PADVANCE
      _PADVANCE = 'LINEFEED'
      DO gfDispRe WITH EVAL('lcRpForm')
      _PADVANCE = lcSavPAdv
    ELSE
      SELECT (lcDummyRel)
      GO TOP
      DO gfDispRe WITH EVAL('lcRpForm')
    ENDIF
    lcOGTmpForm = lcOldTFrm

  CASE lcRpType = 'R'
    lcRpForm = lfGetForm('')
    lcOldTFrm = lcOGTmpForm
    SELECT RALINE
    DO gfDispRe WITH EVAL('lcRpForm')
    lcOGTmpForm = lcOldTFrm


  CASE lcRpType ='B' && Device Selection
    lcOldTFrm = lcOGTmpForm
    lcRpForm = lfGetForm('')
      = lfRepPltFr(lcRpForm)
      SELECT RALINE
      DO gfDispRe WITH EVAL('lcRpForm')
      SET SKIP TO
      lcOldPltFrm = IIF(TYPE('OGPlatForm') = 'C' , OGPlatForm , IIF(TYPE('lcRepMode') = 'C',lcRepMode,''))
      lcRpForm = lfGetForm('RS')
      = lfRepPltFr(lcRpForm)
      OGPlatForm = IIF(TYPE('lcOGPlatForm') = 'C' , lcOGPlatForm , IIF(TYPE('lcRepMode') = 'C',lcRepMode,''))
      =lfPrepHDR()
      lcOGTmpForm = ''
      loOgScroll.lAdditive = .T.
      IF lcFrmFrx = 'Y'
        lcSavPAdv = _PADVANCE
        _PADVANCE = 'LINEFEED'
        DO gfDispRe WITH EVAL('lcRpForm')
       _PADVANCE = lcSavPAdv
      ELSE
        SELECT (lcDummyRel)
        DO gfDispRe WITH EVAL('lcRpForm')
      ENDIF
      loOgScroll.lAdditive = .F.
      OGPlatForm = lcOldPltFrm
      lcOGTmpForm = lcOldTFrm
      lcRpForm = lfGetForm('')
      =lfRepPltFr(lcRpForm)

ENDCASE

IF llPrinter
  =gfOpenTable('RETAUTH','RETAUTH','SH','RETAUTHTmp')
  SELECT (lcRetPrtUp)
  SCAN
    SELECT RETAUTHTmp
    =gfSEEK(EVAL(lcRetPrtUp+'.RANO'))
    =gfreplace(" Flag WITH 'P' ")
  ENDSCAN
  SELECT  RETAUTHTmp
  =gfTableUpdate()
  =gfCloseTable('RETAUTHTmp')
ENDIF

IF USED('&lcRetPrtUp')
  USE IN (lcRetPrtUp)
ENDIF

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Hossam El Etreby   (HDM)
*! Date      : 04/08/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
loDBFRetH    = CreateObject("RemoteTable","RETAUTH","RETAUTH","TMPRETAUTH",SET("DATASESSION"),.F.,.T.)
loDBFRetL    = CreateObject("RemoteTable","RALINE","RALINE","TMPRALINE",SET("DATASESSION"),.F.,.T.)
gfOpenTable('SYCCOMP' , 'CCOMP_ID' , 'SH')
=lfvNPStat()

RETURN


*!*************************************************************
*! Name      : lfCollectData
*! Developer : Hossam El Etreby (HDM)
*! Date      : 04/08/1998
*! Purpose   : Collects the data from the Tables
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCollectData
LOCAL lcRANO, lcRADate, lcRACust, lnCase, ldDate1, ldDate2, lcTmpFile

SELECT * FROM TmpRetAuth   WHERE .F. INTO CURSOR RetAuth READWRITE
SELECT * FROM TmpRALINE    WHERE .F. INTO CURSOR RALINE READWRITE

lcRANO   = lfCheckFilter(1, 'RETAUTH.RANO')
lcRACust = lfCheckFilter(1, 'RETAUTH.ACCOUNT')
lcRADate = lfCheckFilter(1, 'RETAUTH.RADATE')

lnCase = 0
DO CASE

  CASE !EMPTY(lcRANO) AND RECCOUNT(lcRANO)>0
    SELECT (lcRANO)
    lcTmpFile = loOgScroll.gfTempName()
    SCAN
      lcTemp = RANO
      loDBFRetH.SqlRun("Select * From RETAUTH (INDEX=RETAUTH) Where RANO = '"+lcTemp+"'",lcTmpFile)
      SELECT RetAuth
      IF !EMPTY(lcRADate)
        ldDate1 = CTOD(LEFT(lcRADate,AT('|',lcRADate)-1))
        ldDate2 = CTOD(SUBSTR(lcRADate,AT('|',lcRADate)+1))
        APPEND FROM (DBF(lcTmpFile)) FOR BETWEEN(RADATE,ldDate1,ldDate2)
      ELSE
        APPEND FROM (DBF(lcTmpFile))
      ENDIF
      SELECT (lcRANO)
    ENDSCAN
    lnCase = 1

  CASE !EMPTY(lcRACust) AND RECCOUNT(lcRACust)>0
    SELECT (lcRACust)
    lcTmpFile = loOgScroll.gfTempName()
    SCAN
      lcTemp = ACCOUNT
      loDBFRetH.SqlRun("Select * From RETAUTH (INDEX=RETAUTHA) Where ACCOUNT = '"+lcTemp+"'",lcTmpFile)
      SELECT RetAuth
      IF !EMPTY(lcRADate)
        ldDate1 = CTOD(LEFT(lcRADate,AT('|',lcRADate)-1))
        ldDate2 = CTOD(SUBSTR(lcRADate,AT('|',lcRADate)+1))
        APPEND FROM (DBF(lcTmpFile)) FOR BETWEEN(RADATE,ldDate1 ,ldDate2)
      ELSE
        APPEND FROM (DBF(lcTmpFile))
      ENDIF
      SELECT (lcRACust)
    ENDSCAN
    lnCase = 2

  CASE !EMPTY(lcRADate)
    lcTemp = STRTRAN(lcRADate,"|","' and '")
    loDBFRetH.SqlRun("Select * From RETAUTH Where RETAUTH.RADATE Between '"+lcTemp+"'",'RetAuth')
    lnCase = 3

  CASE EMPTY(lcRADate)
    loDBFRetH.SqlRun("Select * From RETAUTH",'RetAuth')
    lnCase = 4

ENDCASE

SELECT RetAuth
IF lnCase = 1 AND !EMPTY(lcRACust) AND RECCOUNT(lcRACust)>0
  SELECT (lcRACust)
  INDEX on Account TAG Account
  SELECT RetAuth
  SET RELATION TO Account INTO (lcRACust)
  SELECT * FROM RetAuth WHERE !EOF(lcRACust) INTO CURSOR RetAuth READWRITE
ENDIF

lcTmpFile = loOgScroll.gfTempName()
SELECT RetAuth
SCAN
  lcTemp = RANO
  loDBFRetL.SqlRun("Select * From RALINE (INDEX=RALINE) Where [RANO] = '"+lcTemp+"'",lcTmpFile)
  SELECT RALINE
  APPEND FROM (DBF(lcTmpFile))
  SELECT RetAuth
ENDSCAN

SELECT RALINE
CURSORSETPROP("Buffering",3)
INDEX on RANO+STYLE+CRA_LINNO TAG RALINE

SELECT RetAuth
SET RELATION TO RANO INTO RALINE

lcTmpFile = loOgScroll.gfTempName()
*B609356,1 SMA 09/21/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
COPY ALL FOR EOF('RALINE') TO (oAriaApplication.WorkDir+lcTmpFile) && with no lines
*B609356,1 SMA 09/21/2010 remove of clause to prevent empty *.cdx files from creation.....[END]

SELECT RALINE
*B609356,1 SMA 09/21/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
APPEND FROM (oAriaApplication.WorkDir+lcTmpFile)
*B609356,1 SMA 09/21/2010 remove of clause to prevent empty *.cdx files from creation.....[END]

IF RECCOUNT('RALINE')>0 AND CURSORGETPROP("Buffering",'RALINE')>1
  =TABLEUPDATE(.T.,.T.,'RALINE')
ENDIF


SELECT Style
SET RELATION TO 'S'+Scale INTO Scale
IF RECCOUNT('RetAuth')>0 AND CURSORGETPROP("Buffering",'RetAuth')>1
  =TABLEUPDATE(.T.,.T.,'RetAuth')
ENDIF

SELECT RetAuth
*B608098,1 HIA stop the relation to the customer file based on the main account [T20070507.0003] [Begin]
*SET RELATION TO 'M'+Account INTO Customer, cWareCode INTO WareHous, 'Z'+RetAuth.RANO INTO NotePad
SET RELATION TO cWareCode INTO WareHous, 'Z'+RetAuth.RANO INTO NotePad
*B608098,1 HIA stop the relation to the customer file based on the main account [T20070507.0003] [End]
CURSORSETPROP("Buffering",3)
INDEX ON RANO TAG RetAuth

SELECT RALINE
INDEX ON RANO+STYLE+CRA_LINNO TAG RALINE

SET RELATION TO RANO INTO RetAuth, Style INTO Style

RETURN


*!*************************************************************
*! Name      : lfOldVal
*! Developer : Hossam El Etreby (HDM)
*! Date      : 04/08/1998
*! Purpose   : Keeps the old value of the editor when cancel the browse
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfOldVal

laOldVal = GfSYS18()

RETURN


*!*************************************************************
*! Name      : lfvRepType
*! Developer : Hossam El Etreby (HDM)
*! Date      : 04/08/1998
*! Purpose   : Determines Report type according to User Selection
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvRepType

lcRpForm = IIF(lcRpType = 'R' , lfGetForm('') ,;
               IIF(lcRpType = 'B' , lfGetForm('') , lfGetForm('RS')))

= lfRepPltFr(lcRpForm)
*Disable Print NotPad option in option grid if report form is sticker
=lfvNPStat()

RETURN


*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Hossam El Etreby
*! Date      : 03/25/1998
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : RmrAut.Prg
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdrShift()
*!*************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)

  *-- IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    = ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF
ENDFOR

*-- FOR Loop to loop through the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)

  *-- IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

RETURN


*!*************************************************************
*! Name        : lfHeader
*! Developer   : Hossam El Etreby
*! Date        : 04/08/1998
*! Purpose     : Function to fill the approparate data for report header.
*!*************************************************************
*! Called from : RMRAUTB.FRX [Header Band]
*!*************************************************************
*! Calls       : FUNCTION => gfRltFld()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfHeader()
*!*************************************************************
FUNCTION lfHeader
PRIVATE lnPrevAl
= gfRltFld(RETAUTH.CDivision , @laDivLName , 'CDIVISION')  && Get the division long name.

lcReason = gfCodDes(Retauth.reason,'REASON')

* fill Sold To address array

lnPrevAl = SELECT(0)
SELECT(lnPrevAl)
*B608098,1 HIA stop the relation to the customer file based on the main account [T20070507.0003] [Begin]
IF !EMPTY(RetAuth.store)
  =SEEK('S'+RetAuth.ACCOUNT+RetAuth.store,'CUSTOMER')
ELSE
  =SEEK('M'+RetAuth.ACCOUNT,'CUSTOMER')
ENDIF

*B608098,1 HIA stop the relation to the customer file based on the main account [T20070507.0003] [End]
laSoldTo[1] = CUSTOMER.STName

*: B609017,1 HES 09/10/2009 Fix bug of wrong sold to address on RA at DCC [Start]
*!*	laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , IIF(Customer.Type='S','2',''))
*!*	laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , IIF(Customer.Type='S','2',''))
*!*	laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , IIF(Customer.Type='S','2',''))
*!*	laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , IIF(Customer.Type='S','2',''))
*!*	laSoldTo[6] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , IIF(Customer.Type='S','2',''))

laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '')
laSoldTo[6] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '')
*: B609017,1 HES 09/10/2009 Fix bug of wrong sold to address on RA at DCC [End]

laSoldTo[7]  = TRANSFORM(Customer.Phone1, '@R ' + lcPhonPict)

laWareAdd[1]= gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
laWareAdd[2]= gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
laWareAdd[3]= gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
laWareAdd[4]= gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
laWareAdd[5]= gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
laWareAdd[6]= TRANSFORM(WareHous.cphone , '@R ' + lcPhonPict)
= lfAdrShift('laWareAdd')
= lfAdrShift('laSoldTo')    && Shift Sold To address if there is empty line.

IF llTocca
  lcVendCD = Customer.CCUSVEND
  lcDept = IIF(!EMPTY(retauth.INVOICE) AND gfSeek(retauth.INVOICE,'INVHDR'),INVHDR.DEPT,'')

ENDIF
RETURN .T.

*!*************************************************************
*! Name        : lfNoOfLns
*! Developer   : Hossam El Etreby
*! Date        : 04/08/1998
*! Purpose     : Function to Fill the rest of the page with DASHED Lines
*!*************************************************************
*! Called from : RMRAUTB.FRX [Header Band]
*!*************************************************************
*! Calls       : NONE
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfNoOfLns()
*!*************************************************************
FUNCTION lfNoOfLns

LOCAL lcRANO
lcCurAl = SELECT(0)
lcRANO = RALINE.RANO

*B608176  Display the notes for each RA HIA [Begin]
*SELECT * FROM RALINE WHERE RANO = lcRANO INTO CURSOR RALINE_A
loDBFRetl.SqlRun("Select * From RALINE Where RANO = '"+lcRANO +"' order by  rano,style,cra_linno",'RALINE_A')
*B608176  Display the notes for each RA HIA [End]

SELECT RALINE_A

COUNT TO lnLines  && FOR RANO = RALINE.RANO
COUNT TO lnLast
GO BOTTOM

lnLastRec = RECNO('RALINE_A')
*B608176  Display the notes for each RA HIA [Begin]
lnLastLIN = RALINE_A.cra_Linno
*B608176  Display the notes for each RA HIA [End]

IF (lnLines * IIF(llRpScale,4,2)) > lnPageLns
  lnAct = lnLines * IIF(llRpScale,4,2)
  lnPagesNo = lnAct / lnPageLns
  IF lnPagesNo > INT(lnPagesNo)
    lnPagesNo = INT(lnPagesNo+1)
  ENDIF
  lnLines   =  lnAct - (lnPagesNo*lnPageLns)
  IF lnLines < 0
    lnLines = (lnLines *(-1)) - 1
  ENDIF
ELSE
  lnLines = IIF(llRpScale,lnLines * 4,lnLines * 2)
ENDIF

IF lnLines >= 1
  IF llRpScale
    lcDashed = '--------------------------------                   ----              ----              ----              ----             ----              ----              ----              ----              ----'
    lcLines = lcDashed
  ELSE
    lcDashed = '--------------------------------                   -------------------------------------------------------------------------------------------------------------------           ----'
    lcLines = lcDashed
  ENDIF

  IF llRpNotes
    lnLines = lnLines - MEMLINES(NOTEPAD.MNOTES)
  ENDIF

  FOR lnI = 1 TO lnLines
    lcLines = lcLines + IIF(lnI > 1, CHR(13) + lcDashed,'')
  ENDFOR
ENDIF
SELECT (lcCurAl)
RETURN ''

*!*************************************************************
*! Name        : lfDispAdd
*! Developer   : Hossam El Etreby
*! Date        : 04/08/1998
*! Purpose     : Function to Fill Address Array
*!*************************************************************
*! Called from : RMRAUTB.FRX [Header Band],RMRAUTA.LBX
*!*************************************************************
*! Calls       : lfAdrShift()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfDispAdd()
*!*************************************************************

FUNCTION lfDispAdd

IF EMPTY(lcCompName)
  PRIVATE lcAliasNow
  lcAliasNow = ALIAS()

  *-- if this company have a logo, point to logo record,
  *-- else seek put pointer at EOF.


  *= SEEK('*' + 'LOGO' , 'OBJLINK') AND SEEK(OBJLINK.cObject_ID,'OBJECTS')
  = gfSeek('*' + 'LOGO' , 'OBJLINK') AND gfSeek(OBJLINK.cObject_ID,'OBJECTS')


  DECLARE laCompAdd[6,1],laDivLName[1,2],laSoldTo[7,1]
  DECLARE laWareAdd[6,1]
  laCompAdd = ''                   && Array to hold the Company address
  laSoldTo  = ''                   && Array to hold the Sold To address
  laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
  laDivLName[1,2] = ''

  *-- Get company Address [begin].
  SELECT SYCCOMP
  SEEK oAriaApplication.ActiveCompanyID

  lcCompName = cCom_Name             && Company Name.
  lcCompPhon = cCom_Phon             && Company Phone.
  lcPhonPict = gfPhoneTem()          && Company Phone Picture Format.

  laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
  laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
  laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
  laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
  laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
  laCompAdd[6] = 'Phone# : '+ TRANSFORM(lcCompPhon ,'@R ' + lcPhonPict)
  = lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
ENDIF
= ACOPY(laCompAdd,laTmp)
DO 'RMADD.SPR'

*------------------------------------------------------------------------

*------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfChkSysDy
*! Developer : AHMED AMER
*! Date      : 08/20/98
*! Purpose   : To check if the system uses dyelot or not
*!*************************************************************
*! Called from : Option grid (Variable llDyelot)
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .....
*!*************************************************************
*! Return      : Logical
*!*************************************************************
*! Example     : = lfChkSysDy()
*!*************************************************************

FUNCTION lfChkSysDy
RETURN (ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y')



FUNCTION lfRecCNt
IF lnLast = lnCurrRec
  lnCurrRec = 0
ELSE
  lnCurrRec = lnCurrRec + 1
ENDIF
RETURN ''
*!*************************************************************
*! Name        : lfLBHeader
*! Developer   : Hossam El Etreby
*! Date        : 04/08/1998
*! Purpose     : Function to fill the approparate data for report header.
*!*************************************************************
*! Called from : RMRAUTB.FRX [Header Band]
*!*************************************************************
*! Calls       : FUNCTION => gfRltFld()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfLBHeader()
*!*************************************************************


FUNCTION lfLBHeader
PRIVATE lnPrevAl

IF lcCode <> RETAUTH.RANO
  lcCode = RETAUTH.RANO
  lnCurLbl = 0
ENDIF

lnCurLbl = lnCurLbl + 1

= gfRltFld(RETAUTH.CDivision , @laDivLName , 'CDIVISION')  && Get the division long name.
lcReason = gfCodDes(Retauth.reason,'REASON')
* fill Sold To address array
lnPrevAl = SELECT(0)
SELECT(lnPrevAl)
laSoldTo[1] = CUSTOMER.STName

*: B609017,1 HES 09/10/2009 Fix bug of wrong sold to address on RA at DCC [Start]
*!*	laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , IIF(Customer.Type='S','2',''))
*!*	laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , IIF(Customer.Type='S','2',''))
*!*	laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , IIF(Customer.Type='S','2',''))
*!*	laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , IIF(Customer.Type='S','2',''))
*!*	laSoldTo[6] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , IIF(Customer.Type='S','2',''))

laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 ,'')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 ,'')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 ,'')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 ,'')
laSoldTo[6] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 ,'')
*: B609017,1 HES 09/10/2009 Fix bug of wrong sold to address on RA at DCC [End]

laSoldTo[7]  = TRANSFORM(Customer.Phone1,'@R ' +lcPhonPict)

* Update the fax # value
lcFaxNo = CUSTOMER.FAX
*AYM:: I think that Fax could be transformed
*lcFaxNo =TRANSFORM(Customer.FAX,'@R ' +lcPhonPict)
laWareAdd[1]= gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
laWareAdd[2]= gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
laWareAdd[3]= gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
laWareAdd[4]= gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
laWareAdd[5]= gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
laWareAdd[6]= TRANSFORM(WareHous.cphone ,'@R ' + lcPhonPict)

= lfAdrShift('laWareAdd')
= lfAdrShift('laSoldTo')    && Shift Sold To address if there is empty line.


RETURN .T.


*!*************************************************************
*! Name        : lfPrepHDR
*! Developer   : Hossam El Etreby
*! Date        : 04/27/1998
*! Purpose     : Function to craete a dummy file to be used to make
*!               a 1 to M relation according to cartos #
*!*************************************************************
*! Called from : RMRAUT.PRG
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfPrepHDR()
*!*************************************************************
*! Due to bug 802090,1 Print labels according to no. of cartons
*! Called only from RMRAUTA.LBX
*!*************************************************************

FUNCTION lfPrepHDR

lcDummyRel = loOgScroll.gfTempName()
**-- Collect Styles available for these labels
* [Start] If this label is Format 'B' Create a new temp File
*                  Containes 8 Styles from the lines

IF lcFrmFrx = 'Y'
  lcDummySty = loOgScroll.gfTempName()
  CREATE TABLE (oAriaApplication.WorkDir+lcDummySty) (Rano C(6) , Style1 C(19) , TotQty1 N(7),;
                Style2 C(19) , TotQty2 N(7),;
                Style3 C(19) , TotQty3 N(7),;
                Style4 C(19) , TotQty4 N(7),;
                Style5 C(19) , TotQty5 N(7),;
                Style6 C(19) , TotQty6 N(7),;
                Style7 C(19) , TotQty7 N(7),;
                Style8 C(19) , TotQty8 N(7))

  INDEX ON RANO TAG (lcDummySty)
ENDIF


CREATE TABLE (oAriaApplication.WorkDir+lcDummyRel) (Rano C(6))
SELECT (lcDummyRel)
INDEX ON RANO TAG (lcDummyRel)
SELECT RETAUTH
SCAN
* N000682 ,1 Thabet Handle globalization issues [Start]
*  WAIT WINDOW 'Preparing Labels Data' NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Preparing_Labels_Data NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Preparing_Labels_Data,oAriaApplication.GetHeaderText("LANG_Preparing_Labels_Data",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

  * N000682 ,1 Thabet Handle globalization issues [END]
  FOR lnLoop = 1 TO RETAUTH.CARTONS
    INSERT INTO (lcDummyRel) VALUES (RETAUTH.RANO)  && Warning
  ENDFOR
  * [Start] If this label is Format 'B' Fill a new temp File
  *                  with 8 Styles from the lines

  IF lcFrmFrx = 'Y'
    SELECT (lcDummySty)
    APPEND BLANK
    REPLACE RANO WITH RETAUTH.RANO
    SELECT RALINE
    lnStyLoop = 0
    =SEEK(RETAUTH.RANO)
    SCAN REST WHILE RANO = RETAUTH.RANO
      lnStyLoop = lnStyLoop + 1
      IF lnStyLoop > 8
        EXIT
      ENDIF
      lcStyLoop = ALLTRIM(STR(lnStyLoop))
      SELECT (lcDummySty)
      REPLACE STYLE&lcStyLoop  WITH RALINE.STYLE,;
              TotQty&lcStyLoop WITH RALINE.TOTQTY

    ENDSCAN
  ENDIF
ENDSCAN

SET RELATION TO rano INTO (lcDummyRel) ADDITIVE  && Warning
* [Start] If this label is Format 'B' Establish
*                  a relation with RETAUTH to point to
*                  desired styles in the temp file
IF lcFrmFrx = 'Y'
  SELECT RETAUTH
  SET RELATION TO rano INTO (lcDummySty) ADDITIVE  && Warning
ELSE
  SELECT RETAUTH
  *B608098,1 HIA stop the relation to the customer file based on the main account [T20070507.0003] [Begin]
  *SET RELATION TO 'M'+Account INTO Customer, cWareCode INTO WareHous, 'Z'+RetAuth.RANO INTO NotePad
  SET RELATION TO  cWareCode INTO WareHous, 'Z'+RetAuth.RANO INTO NotePad
  *B608098,1 HIA stop the relation to the customer file based on the main account [T20070507.0003] [End]
  SELECT (lcDummyRel)
  SET RELATION TO rano INTO RETAUTH
  GO TOP
ENDIF
*!*************************************************************
*! Name        : lfClrTemp
*! Developer   : Hossam El Etreby
*! Date        : 04/27/1998
*! Purpose     : FUNCTION to clear temporary file created by lfPrepHDR
*!*************************************************************
*! Called from : RMRAUT.PRG
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfClrTemp()
*!*************************************************************
*! Due to bug 802090,1 Print labels according to no. of cartons
*! Called only from RMRAUTA.LBX
*!*************************************************************

FUNCTION lfClrTemp
SELECT RETAUTH
SET RELATION TO
USE IN (lcDummyRel)
ERASE (oAriaApplication.WorkDir + lcDummyRel + '.*')
* If this label is Format 'B' Erase the generated temp File
IF lcFrmFrx = 'Y'
  USE IN (lcDummySty)
  ERASE (oAriaApplication.WorkDir + lcDummySty + '.*')
ENDIF


*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/28/1998
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Called from : [Option Grid] < Close >
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
FUNCTION lfClearRep

*Close INVHDR file if Tocca Format
IF TYPE('lcTocca')='C' AND ALLTRIM(lcTocca) ='Y' AND USED('INVHDR')
  gfCloseTable([IN INVHDR])
 ENDIF


*!*************************************************************
*! Name      : lfvNPStat
*! Developer : Sameh Al-Desouki
*! Date      : 02/29/2000
*! Purpose   : Disable Print NotPad option in option grid
*!             if report form is sticker
*!*************************************************************
*! Called from : RmrAut.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvNPStat()
*!*************************************************************
FUNCTION lfvNPStat
*-- Get the position of the print cost setting in the array to enable or
*-- disable it.
lnPos = ASCAN(laOgObjType,'LLRPNOTES')
IF lnPos > 0
  lnPos= ASUBSCRIPT(laOGObjType,lnPos,1)
  laOGObjCnt[lnPos] = IIF(lcRpType = 'S',.F.,.T.)
  = lfOGShowGet('LLRPNOTES')
ENDIF

*-- End of lfvNPStat.

*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************

FUNCTION lfCheckFilter()
LPARAMETERS lnArrayType, lcFilter
LOCAL lcReturn, lnPOS
lcReturn = ""
DO CASE
CASE lnArrayType = 1
  lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter)
  IF lnPos > 0
    lnPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)


    *: B608754,1 MMT 12/04/2008 Fix bug of Print Buuton is disabled in Ret. Auth. Screen [Start]
    IF (ALLTRIM(UPPER(lcFilter)) = 'RETAUTH.RANO' AND ALLTRIM(UPPER(loOgScroll.laOGFXFlt[lnPOS ,5])) = 'LIKE') AND !EMPTY(loOgScroll.laOGFXFlt[lnPOS,6])
      lcAlias = ALIAS()
      lcTempCur =  gfTempName()
      CREATE CURSOR (lcTempCur) (KEYEXP C(6),RANO C(6))
      SELECT(lcTempCur)
      INDEX on KEYEXP TAG (lcTempCur)
      APPEND BLANK
      REPLACE KEYEXP  WITH loOgScroll.laOGFXFlt[lnPOS,6],;
		      RANO    WITH loOgScroll.laOGFXFlt[lnPOS,6]
		
      loOgScroll.laOGFXFlt[lnPOS,6]	= lcTempCur
      SELECT(lcAlias )
    ENDIF
	*: B608754,1 MMT 12/04/2008 Fix bug of Print Buuton is disabled in Ret. Auth. Screen [End]


    lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]
  ENDIF
CASE lnArrayType = 2
  lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter)
  IF lnPos > 0
    lnPOS = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
    lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]
  ENDIF
CASE lnArrayType = 3
  lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter)
  IF lnPos > 0
    lnPOS = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
    lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]
  ENDIF
ENDCASE
RETURN lcReturn



*!*************************************************************
*! Name      : lfMakeIndex
*: Developer : Mariam Mazhar (MMT)
*: Date      : 02/16/2005
*! Purpose   : function to make index on a temp. file
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfMakeIndex
PARAMETERS lcTempName
PRIVATE laIndex
DIMENSION laIndex[1,2]

lcCursor = lcTempName
lnBuffering = CURSORGETPROP("Buffering",lcCursor)
 =CURSORSETPROP("Buffering",3,lcCursor)
*-- To initialize the indecis that will be created for each file
=lfCrtindex(lcCursor)
SELECT (lcCursor)
FOR lnI = 1 TO ALEN(laIndex,1)
  lcIndex = laIndex[lnI,1]
  lcTag   = laIndex[lnI,2]
  INDEX ON &lcIndex. TAG (lcTag) &&OF (lcCursor)
ENDFOR
lcTag = laIndex[1,2]
SET ORDER TO TAG (lcTag)
REINDEX

*!*************************************************************
*! Name      : lfCrtindex
*: Developer : Mariam Mazhar (MMT)
*: Date      : 02/16/2005
*! Purpose   : function to Set the index for the SQL files
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfCrtindex

LPARAMETERS lcTable
  DO CASE
   CASE UPPER(lcTable) =  "RETAUTH"
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'RANO'
      laIndex[1,2] = 'RETAUTH'

   *--temp. Customer File
   CASE UPPER(lcTable) = 'RALINE'
     DIMENSION laIndex[1,2]
     laIndex[1,1] = 'RANO+STYLE+CRA_LINNO'
     laIndex[1,2] = 'RALINE'
ENDCASE

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 11/12/2003
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Parameters: Start collection date,End collection date
*!*************************************************************
*! Returns   : Spent time.
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- End of lfCollTime.

*!*************************************************************
*! Name        : lfRepFlag
*! Developer   : Hossam El Etreby
*! Date        : 04/08/1998
*! Purpose     : Modefies the printed flag to 'P' If device not SCREEN
*!*************************************************************
*! Called from : RMRAUTB.FRX , RMRAUTA.LBX
*!*************************************************************
*! Calls       : NONE
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfRepFlag()
*!*************************************************************

FUNCTION lfRepFlag
  INSERT INTO (lcRetPrtUp) (RAno) VALUES (RetAuth.RAno)
  RETURN ''


*!*************************************************************
*! Name      : lfUpdate
*! Developer : Ayman M. Radwan (aym)
*! Date      : 15/02/2006
*! Purpose   : CHECK IF RA PRINTED
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Called From : Frx
*!*************************************************************
FUNCTION lfUpdate

IF SYS(2040)='2'
  llPrinter = .T.
ENDIF
RETURN .T.
