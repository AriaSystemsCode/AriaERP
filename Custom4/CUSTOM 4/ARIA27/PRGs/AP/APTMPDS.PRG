*:************************************************************************
*:
*: Procedure file: APTMPDS.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Hisham Ramsis Philips
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: lpFrstShow
*:               : lpShow
*:               : lfDispBrow
*:               : lfwTmpBrow
*:               : lfwDistAcct
*:               : lfvDistAcct
*:               : lfvData_2
*:               : lfObjDisp
*:               : lfvNew
*:               : lfvRemove
*:               : lfvmbase
*:               : lfwDstPrct
*:               : lfvDstPrct
*:               : lfwTaxCode
*:               : lfvTaxCode
*:               : lpSavScr
*:               : lpDelScr
*:               : lfActBrows
*:               : lfDeAct
*:               : lfBrowTrap
*:               : lpDoTrap
*:               : lfBrowUnTrap
*:               : lfDefinePad
*:               : lfPushKey
*:               : lpTab
*:               : lpShfTab
*:               :
*:
*:      Documented 00/00/1994
*:************************************************************************
*B600492,1 Reham On 06/22/95
*B600492,1 Make a browse fields variable under windows for better displaying
*B600691,1 M.H 09/19/95 In Add or Edit mode, whenever the following sequence occurs :
*B600691,1 M.H 09/19/95 1. Press < New > for a new entry, you automatically go to the Account field.
*B600691,1 M.H 09/19/95 2. From the account field, if you browse, and press ESC ( ie do not select an account ),New, Remove, and percent fields are disabled and remain so as we move through the screen.
*B600691,1 M.H 12/03/95 When the user browse the account and return
*B600691,1 M.H 12/03/95 from the browse without sellect anything we
*B600691,1 M.H 12/03/95 have to remove the added line, enable the new
*B600691,1 M.H 12/03/95 remove buttons.
*B600691,1 M.H 12/04/95 We cannot save the template without the distribution line.
*E300643,1  HS 04/15/97 1)Make some changes for we have change the file
*E300643,1              SYCCODES name to CODES and make it a data file
*E300643,1              [Change its dir. from SYSFILES to DBFS]
*E300643,1              2)Make some changes for we have change the function
*E300643,1              [gfCodDes]
*E300643,1              3)Make some changes for we have change the function
*E300643,1              [gfRltFld]
*E300683,1 AHMED 06/04/97 Add screens directory path to the calling of SPRS
*E301077,80 IHB 03/03/1999 opend needed files only
*E300789,4 IHB  07/03/1999 Remove company ID from ACCOD, FISHD, FSPRD, FSHLD, CODES
*B803555,1 SSE 08/08/2000 Program doesn't save the percentage in APINVADT file also change font 
*B803555,1                of Browse screen ApTmpDs2.Scx from FoxFont to standard Ms Sans Serif
*B605162,1 MHM 12/20/2001 Fix the bug of nothing is showing on my detail lines in case of using browse button.
*B607539,1 EIH 01/02/2006 Fix the bug that when we select line item tax option and there is no tax code in 
*B607539,1 EIH            Code file when we try to select tax error message display.
*:************************************************************************
*

EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc
DECLARE laKeyField[2,4],laFileStru[1],laTaxCode[1,2]

** lcBrowName   : Variable to hold Brows Window name. 
** lcReadNam1   : Variable to hold Read Window name.
** lcReadNam2   : Variable to hold Read Window name.
** lcReadNam3   : Variable to hold Read Window name.
** lcReadNam4   : Variable to hold Read Window name.
** lcReadNam5   : Variable to hold Read Window name.
** lcTTaxCode   : Variable to hold word Tax code.
** lcTAcct      : Variable to hold word account.
** lcTDisc      : Variable to hold word discount. 
** lcTDstPrct   : Variable to hold word percent.
** lcTNo        : Variable to hold word No.
** lcBrowStr    : Variable to hold browse fields.
** lcSavOrd     : Variable to hold 
** lcInvADtEx   : Variable to hold the details file index expression
** lcODistAcct  : Variable to hold old distribution account.
** lcObjState   : Variable to hold object state.
** lcAccDisc    : Variable to hold account discription.
** lcBrowFile   : Variable to hold file we browse from.
** lcTBlnkCode  : Variable to hold text not aplicable.
** lcDivDisc    : Variable to hold division discription.
** lcColPair    : Variable to hold color.
** lcTaxDisc    : Variable to hold tax discription.
** lcTaxCode    : Variable to hold tax discription.
** lcOTaxCode   : Variable to hold old tax discription.
** lcOTCode     : Variable to hold old tax code.
** lcTCode      : Variable to hold tax code.
** lcOTCode     : Variable to hold old tax code. 
** lcTmpDt      : Variable to hold temp file name.
** lcPopCol     : Variable to hold popup color.
** lcTaxStat    : Variable to hold objecct state.
** lcBrTtl      : Variable to hold browse title.
** lcCodeFilt   : Variable to hold field filter. 
** lcDistAcct   : Variable to hold distribution account.
**
** lnDstPrct    : Variable to hold distribution percentage.
** lnODstPrct   : Variable to hold old distribution percentage.
** lnTotPrct    : Variable to hold total distribution percentage.
** lnNetTot     : Variable to hold total in case of modify.
** lnNoOfRec    : Variable to hold no of record in the file.
** lnLineNo     : Variable to hold line no.
** lnTmpRcNo    : Variable to save record pointer.
** lnAPDLINNO   : Variable to hold line no in browse.
** nAPDLinNo    : Variable to hold line no.
**
** llFrstShow   : Variable to hold the frist time done lpshow.
** llBrowse     : Variable to hold if field browse happened.
** llNew        : Variable to hold click push button new.
**
** puTax        : Variable to hold popup name under windows.
** puTaxCode    : Variable to hold popup name under windows.
**
** rbmbase      : Variable to hold radio button mbase.
** ibTaxCode    : Variable to hold ib Tax Code.
** ibDummy0     : Variable to hold ladata_1.

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] = .F.
laKeyField[1,3] = 'HTYPCOD'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[2]'
laKeyField[2,2] = .T.
laKeyField[2,3] = 'HTYPCOD'
laKeyField[2,4] = 2

STORE 0.00         TO lnDstPrct   , lnODstPrct , lnTotPrct
STORE 0            TO lnNetTot    , lnNoOfRec  , lnLineNo   , lnTmpRcNo   ,;
                      lnApdLinNo  , nAPDLinNo
STORE 1            TO rbmbase     , ibTaxCode
STORE .F.          TO llFrstShow  , llBrowse   , llNew      , laDefProc[7],;
                      laDefProc[9]
STORE " "          TO lcBrowName  , lcReadNam1 , lcReadNam2 , lcReadNam3  ,;
                      lcReadNam4  , lcReadNam5 , lcTTaxCode , lcTAcct     ,;
                      lcTDisc     , lcTDstPrct , lcTNo      , lcBrowStr   ,;
                      lcSavOrd    , lcInvADtEx , lcODistAcct, lcObjState  ,;
                      lcAccDisc   , lcBrowFile , lcTBlnkCode, lcDivDisc   ,;
                      lcColPair   , lcTaxDisc  , lcTaxCode  , lcOTaxCode  ,;
                      lcOTCode    , lcTCode    , lcOTCode   , lcTmpDt     ,;
                      lcPopCol    , puTax      , puTaxCode  , ibDummy0    ,;
                      lcEsc
                      

STORE "CTAXCODE  " TO lcCodeFilt
STORE 'DISABLE'    TO lcTaxStat
STORE lcEmptyAcc   TO lcDistAcct 
STORE "Add / Edit distribution" TO lcBrTtl

IF !gfSetup()
  RETURN
ENDIF

lcEsc = ON("KEY","ESC")

SELECT APINVAHD

IF !WEXIST(gcBaseWind)  && If enter the base window for the first time.
  llFrstShow  = .T. 

  *E300643,1 Change this line for the changes we have made 
  *          to (gfCodDes) [Begin]
  *lcTBlnkCode = gfCodDes(' ')
  lcTBlnkCode = gfCodDes(' ' , ' ')
  *E300643,1 Change this line [End]
  
  **Fill the array hold related field.
  laTaxCode[1,1] = 'CGLINPACCT'
  laTaxCode[1,2] = 'lcDistAcct'

  lcBrowName = gfTempName()   
  lcReadNam1 = gfTempName()
  lcReadNam2 = gfTempName()
  lcReadNam3 = gfTempName()
  lcReadNam4 = gfTempName()
  lcReadNam5 = gfTempName()
  lcTmpDt    = gfTempName()

  SELECT APINVADT

  *** Get fields of invoice detail file into an array.
  =AFIELDS(laFileStru)

  lnFileStru = ALEN(laFileStru,1)
  *** increase the dimension of the array.
  DIMENSION laFileStru[lnFileStru+2,4]
  *** Add element to the array to keep the record status add, same, modify.
  laFileStru[lnFileStru+1,1] = 'CSTATUS'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 1
  laFileStru[lnFileStru+1,4] = 0
  *** Add element to the array to keey the recode number.
  laFileStru[lnFileStru+2,1] = 'NRECNO'
  laFileStru[lnFileStru+2,2] = 'N'
  laFileStru[lnFileStru+2,3] = 10
  laFileStru[lnFileStru+2,4] = 0
  *** Creat temp  file from the array.
  CREATE TABLE &gcWorkDir.&lcTmpDt FROM ARRAY laFileStru


  IF _WINDOWS
    *** Define pop up for tax code from temp file created.

    *E300643,1 Change this lines for the changes we have made 
    *          to SYCCODES [Begin]
    *puTax = SYCCODES.cdiscrep
    *DEFINE POPUP puTaxCode prompt field SYCCODES.cdiscrep scroll;
    *FROM 2.45,27.75 TO 8.25,58.00;
    *MESSAGE gfObj_msg()
    puTax = CODES.cdiscrep
    DEFINE POPUP puTaxCode prompt field CODES.cdiscrep scroll;
    FROM 2.45,27.75 TO 8.25,58.00;
    MESSAGE gfObj_msg()
    *E300643,1 Change this lines [End]
    
    ON SELECTION POPUP puTaxCode DO lfvTaxCode
  ENDIF

  SCATTER FIELDS &lcScFields MEMO TO laData BLANK  

  laData[1]  = 'T'                 && laData_1 allways keep this value
  laData[3]  = APSETUP.CTAXTYPE    && get value of laData_3 from apsetup.
  rbmbase    = AT(laData[3],"LT")  && let the radio button stand on the right postion.

  lcTaxStat  = "DISABLE"           && get the initial state of tax code and rbmbase.  
  lcBrowFile = "TEMP"              && get the initial browse file name. 
ELSE  && if the base window exists.
  IF _WINDOWS
    puTax = lcTaxCode
  ENDIF
  *** if add or edit mode and there is no distribution lines.
  IF lnLineNo = 0 .AND. laScrMode[3] .OR. laScrMode[4] 
    lcTaxStat = "ENABLE"
  ELSE
    lcTaxStat = "DISABLE"
  ENDIF  
ENDIF

*** Trab some keys before browse. ***
=lfPushKey()

*** Get Codes from codes file.

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

lcSvOrder = ORDER() 
SET ORDER TO 0

*E300789,4 IHB Remove company ID [start]
*SET FILTER TO (CCOMP_ID+CRLTFIELD+CFLD_NAME = gcAct_Comp+'N'+lcCodeFilt) OR;
              (CCOMP_ID+CRLTFIELD+CFLD_NAME ='  '+'N'+'N/A')
SET FILTER TO (CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N'+lcCodeFilt) OR;
              (CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N'+'N/A')
*E300789,4 IHB [end]

SET ORDER TO &lcSvOrder


SELECT APINVAHD

SELECT APINVADT
SET ORDER TO TAG DTYPCOD
*** Current index expression
lcInvADtEx   = SYS(14,VAL(SYS(21)))   

SELECT APINVAHD
SET FILTER TO
SET FILTER TO CAUTMTYPE = "T"

SET ORDER TO TAG HTYPCOD
SET RELATION TO laData[1]+laData[2] INTO APINVADT ADDITIVE


*E300683,1 Call *.SPR from screens directory
*DO ApTmpDs.SPR
DO (gcScrDir + gcWinAppl + '\ApTmpDs.SPR')
*E300683,1 end

***Release the traped keys.

ON KEY

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

SET FILTER TO 
SELECT APINVAHD

***Release the relation.

SET RELATION TO

***Release the filter.

SET FILTER TO

RELEASE PAD _BROWSE OF _MSYSMENU

***Release the browse window.

RELEASE WINDOW (lcBrTtl)

IF glQuitting
  ***Close all temp files and erase them.
  IF USED(lcTmpDt)
    USE IN (lcTmpDt)
  ENDIF

  ERASE (gcWorkDir+lcTmpDt+".DBF")
  ERASE (gcWorkDir+lcTmpDt+".CDX")
  ERASE (gcWorkDir+lcTmpDt+".FPT")

ENDIF  
glFromBrow  = .F.


*!**************************************************************************
*!
*!      Procedure : lpFrstShow
*!
*!**************************************************************************
***Show lpshow for the frist time enter the base window.
PROCEDURE lpFrstShow

IF llFrstShow
  =lpShow()
  llFrstShow  = .F. 
ENDIF

*!**************************************************************************
*!
*!      Procedure : lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow
EXTERNAL ARRAY laScrMode

***Control if display tax code or not.
IF rbmbase = 1
  ACTIVATE WINDOW(lcReadNam2)
ELSE
  ACTIVATE WINDOW(lcReadNam3)
ENDIF

DO CASE 
  CASE laScrMode[1]    && Select mode.
    laData[1]       = 'T'
    laData[3]       = APSETUP.CTAXTYPE

    ** Select the Temp file and zap the old record data
    SELECT(lcTmpDt)
    ZAP
    ***intialize variables.
    lnNoOfRec  = 0  
    lnLineNo   = 0
    lcDistAcct = lcEmptyAcc
    lnDstPrct  = 00.00 
    lcTcode    = " "
    lcTaxCode  = IIF(EMPTY(lcTcode),lcTBlnkCode,lcTaxCode)
    puTax      = lcTaxCode
    lcObjState = "DISABLE"

    SHOW GET rbmbase DISABLE

  CASE laScrMode[2] .OR. laScrMode[3]    && View or Edit mode.
    IF laScrMode[3]  && Edit mode.
      ***fill the temprary file from the main file.
      SELECT *,RECNO() AS 'nRecNo',"S" AS 'cStatus';
        FROM (gcDataDir+"APINVADT");
       WHERE CAUTMTYPE+CAUTMCODE = laData[1]+laData[2];
       ORDER BY CAUTMTYPE,CAUTMCODE,nAPDLinNo;
        INTO DBF (gcWorkDir+lcTmpDt)
      lnNoOfRec   = RECCOUNT(lcTmpDt)   && Variable to hold the no of records in the Temp file.
      lnLineNo    = lnNoOfRec           && Get the last line number.
      lcObjState  = "DISABLE"      
      IF lnLineNo = 0                   && if there is no distribution lines make rbmbase enable.
        SHOW GET rbmbase ENABLE
      ENDIF  
    ELSE && View mode.
      lcTaxCode = IIF(EMPTY(lcTcode),lcTBlnkCode,lcTaxCode)   
      SHOW GET rbmbase DISABLE   

      *E300643,1 Change this line for the changes we have made 
      *          to (gfCodDes) [Begin]
      *lcTaxCode = gfCodDes(APINVADT.cTaxCode)
      lcTaxCode = gfCodDes(APINVADT.cTaxCode , 'CTAXCODE')
      *E300643,1 Change this line [End]
      
      puTax     = lcTaxCode
      lcDistAcct= APINVADT.CAPDGLACT
      lnDstPrct = APINVADT.NAPDAMNT
    ENDIF

    ** If Edit mode & no of records of the Temp file is > 0 we are going
    ** to ENABLE the objects else we are going to DISABLE it.
    lcObjState  = IIF(laScrMode[3] .AND. lnNoOfRec > 0,'ENABLE','DISABLE')

  CASE laScrMode[4]    && Add mode.
    lcObjState  = 'DISABLE'   && Disable the objects screen.
    IF lnLineNo = 0
      SHOW GET rbmbase ENABLE
    ENDIF  
ENDCASE   

rbmbase = AT(laData[3],"LT")  && Get the value of the radio button screen.

=lfvmbase()

=lfObjDisp(lcObjState)

*** If there is no records in the Temp file so we are going to DISABLE
*** the remove button.
lnLineNo   = lnNoOfRec

IF lnNoOfRec = 0 .AND. !laScrMode[1]
  SHOW GET pbRemove   DISABLE
ELSE  
  *** If Add or Edit mode.
  IF laScrMode[3] .OR. laScrMode[4]
    SHOW GET pbRemove ENABLE
  ELSE
    *** If View mode.
    SHOW GET pbRemove   DISABLE
  ENDIF  
ENDIF  

IF laScrMode[3] .OR. laScrMode[4]
  SHOW GET pbNew ENABLE
  IF !EMPTY(lcTcode)
    SHOW GET lcDistAcct DISABLE
    SHOW GET ibDistAcc  DISABLE
  ELSE
    IF !(llFrstShow) .AND. !(laScrMode[4])
      SHOW GET lcDistAcct ENABLE
      SHOW GET ibDistAcc  ENABLE
    ENDIF  
  ENDIF
ELSE
  SHOW GET pbNew      DISABLE
  SHOW GET lcDistAcct DISABLE
  SHOW GET ibDistAcc  DISABLE
ENDIF

SHOW GET lnDstPrct
SHOW GET nAPDLinNo

*** Define popup color. ***
lcPopCol = IIF(laScrMode[1] .OR. laScrMode[2], SCHEME(1,10), SCHEME(1,2))

IF rbmbase = 1
  SHOW GET lcTcode 
ENDIF  

SELECT APINVAHD

*!**************************************************************************
*!
*!      Function : lfDispBrow
*!
*!**************************************************************************
* Display browse.
FUNCTION lfDispBrow

PRIVATE lcCurAlias

lcCurAlias = ALIAS()

DO CASE 
  CASE laScrMode[2]    && Select or View mode.
    lcBrowFile = "MAST"
    SELECT APINVADT
    lcBrTtl     = "View distribution"
   OTHERWISE           && Add or Edit mode. 
    lcBrowFile = "TEMP"
    SELECT (lcTmpDt)
    lcBrTtl     = "Add / Edit distribution"
ENDCASE

IF _DOS
  *B600492,1 Make browse fields variable under windows.
  lcBrowStr = "lnAPDLinNo=nAPDLinNo :R :4 :H=lcTNo :W= .F.,"+;
               IIF(rbmbase=1,;
              "lcTaxDisc =IIF(EMPTY(CTAXCODE),lcTBlnkCode,LOOKUP(SYCCODES.cDiscrep,gcAct_Comp+CTAXCODE,SYCCODES.cCode_No,'CODES')):R :10:H=lcTTaxCode,";
              ,"")+;
              "CAPDGLACT:R :15 :H=lcTAcct,"+;
              IIF(APSETUP.CAPSGLLINK = 'Y',;
              "lcAccDisc=ALLTRIM(LOOKUP(lcLinkChar.CACCNLDES,CAPDGLACT,lcLinkChar.CACCTCODE,'ACCTCODE')):R :25 :H=lcTDisc,";
              ,"")+;
	          "NAPDAMNT:R :13 :H=lcTDstPrct";

  BROWSE FIELDS &lcBrowStr;
            WINDOW (lcBrowName);
            IN WINDOW (gcBaseWind) ;
            WHEN lfwTmpBrow() .AND. lfBrowTrap();
            VALID :F lfvBrowse();
            LOCK 0;
            NOAPPEND;
            NOCLEAR;
            NODELETE;
            NOWAIT;
            NOEDIT;
            SAVE;
            COLOR SCHEME 13;
            TITLE lcBrTtl
ELSE
  *B600492,1 Make browse fields variable under windows.

  *E300643,1 Change this line for the changes we have made 
  *          to SYCCODES and to the index tag CODES [Begin]
  *lcBrowStr = "lnAPDLinNo=nAPDLinNo :R :4 :H=lcTNo :W= .F.,"+;
  *             IIF(rbmbase=1,;
  *            "lcTaxDisc =IIF(EMPTY(CTAXCODE),lcTBlnkCode,LOOKUP(SYCCODES.cDiscrep,gcAct_Comp+CTAXCODE,SYCCODES.cCode_No,'CODES')):R :10:H=lcTTaxCode,";
  *            ,"")+;
  *            "CAPDGLACT:R :15 :H=lcTAcct,"+;
  *             IIF(APSETUP.CAPSGLLINK = 'Y',;
  *            "lcAccDisc=ALLTRIM(LOOKUP(lcLinkChar.CACCNLDES,CAPDGLACT,lcLinkChar.CACCTCODE,'ACCTCODE')):R :21 :H=lcTDisc,";
  *            ,"")+;
  *            "NAPDAMNT:R :13 :H=lcTDstPrct";
  
  lcBrowStr = "lnAPDLinNo=nAPDLinNo :R :4 :H=lcTNo :W= .F.,"+;
               IIF(rbmbase=1,;
              "lcTaxDisc =IIF(EMPTY(CTAXCODE),lcTBlnkCode,LOOKUP(CODES.cDiscrep,gcAct_Comp+CTAXCODE+'N'+'CTAXCODE',CODES.cCode_No,'CODES')):R :10:H=lcTTaxCode,";
              ,"")+;
              "CAPDGLACT:R :15 :H=lcTAcct,"+;
               IIF(APSETUP.CAPSGLLINK = 'Y',;
              "lcAccDisc=ALLTRIM(LOOKUP(lcLinkChar.CACCNLDES,CAPDGLACT,lcLinkChar.CACCTCODE,'ACCTCODE')):R :21 :H=lcTDisc,";
              ,"")+;
	          "NAPDAMNT:R :13 :H=lcTDstPrct";

  *E300643,1 Change this line [End]
  
  BROWSE FIELDS &lcBrowStr;
            WINDOW (lcBrowName) ;
            WHEN lfwTmpBrow() .AND. lfBrowTrap();
            VALID :F lfvBrowse();
            IN WINDOW (gcBaseWind) ;
            LOCK 0;
            NOMENU;
            NOAPPEND;
            NOCLEAR;
            NODELETE;
            NOWAIT;
            NOEDIT;
            SAVE;
            TITLE lcBrTtl
ENDIF

SELECT (lcCurAlias)

*!**************************************************************************
*!
*!        Function : lfvBrowse
*!
*!**************************************************************************
* 
FUNCTION lfvBrowse

IF !WONTOP(lcBrTtl)
  =gfStopBrow()
ELSE
  =lfBrowUnTrap()
  IF !WEXIST(lcBrowName) 
    glFromBrow = .F.
    glQuitting = .T.
    CLEAR READ
    KEYBOARD CHR(13)
    RETURN TO ApTmpDs.SPR
  ENDIF
ENDIF    




*!**************************************************************************
*!
*!      Function : lfwTmpBrow
*!
*!**************************************************************************
* Fill variable from record.
*
FUNCTION lfwTmpBrow

PRIVATE lcCurAlias
lcCurAlias = ALIAS()
DO CASE 
  CASE laScrMode[2]    && View mode.
    SELECT APINVADT
  CASE (laScrMode[1] .OR. laScrMode[3] .OR. laScrMode[4])    && Edit or Add mode.
    SELECT (lcTmpDt)
ENDCASE

SHOW WINDOW (lcBrTtl) REFRESH  

IF (laScrMode[1] .OR. laScrMode[3] .OR. laScrMode[4])
  lcTCode   = &lcTmpDt..CTAXCODE
  lcDistAcct= &lcTmpDt..CAPDGLACT

  *B605162,3 MHM 12/24/2001 [start]
  *lnDstPrct = &lcTmpDt..NAPDAMNT
  IF laScrMode[4]
    IF lnNetTot<100
      lnDstPrct = &lcTmpDt..NAPDAMNT
    ENDIF  
  ELSE
    lnDstPrct = &lcTmpDt..NAPDAMNT
  ENDIF
  *B605162,3  [end]
  
ELSE
  lcTCode   = APINVADT.CTAXCODE
  lcDistAcct= APINVADT.CAPDGLACT
  lnDstPrct = APINVADT.NAPDAMNT
ENDIF  

*E300643,1 Change this line for the changes we have made to (gfCodDes) [Begin]
*lcTaxCode = gfCodDes(cTaxCode)  
lcTaxCode = gfCodDes(cTaxCode , 'CTAXCODE')
*E300643,1 Change this line for the changes we have made to (gfCodDes) [End]

puTax     = lcTaxCode

SHOW GET lcDistAcct
SHOW GET ibDistAcc
SHOW GET lnDstPrct
SHOW GET nAPDLinNo
SHOW GET puTax

IF rbmbase = 1
  SHOW GET lcTcode 
ENDIF  

glFromBrow = .T.

=lfREFRESH()

IF !EMPTY(lcCurAlias)
  SELECT (lcCurAlias)
ENDIF


*!**************************************************************************
*!
*!      Function : lfwDistAcct
*!
*!**************************************************************************
*
FUNCTION lfwDistAcct

lcODistAcct = lcDistAcct
  
*!**************************************************************************
*!
*!      Function : lfvDistAcct
*!
*!**************************************************************************
*
FUNCTION lfvDistAcct

IF !llBrowse .AND. (lcODistAcct = lcDistAcct .OR. EMPTY(lcDistAcct))

  *Begin B600691,1 M.H 09/19/95 
  IF llNew
    *B605162,1 MHM 12/20/2001 no need to change new to false[Start]
    *llNew = .F. 
    *B605162,1 MHM 12/20/2001 [End]
    IF lnLineNo > 0
      SHOW GET pbRemove  ENABLE
    ELSE
      SHOW GET pbRemove  DISABLE
    ENDIF 
    SHOW GET pbNew     ENABLE
    SHOW GET lnDstprct ENABLE
    =lfwTmpBrow()
  ENDIF
  *END   B600691,1 M.H 09/19/95 

  RETURN
ELSE  
  IF llBrowse .OR. !EMPTY(STRTRAN(STRTRAN(lcDistAcct,'-'),'0'))
    RELEASE PAD _BROWSE OF _MSYSMENU
    ON KEY LABEL ALT+B
    IF !lfApAcs(.F.,llBrowse)
      lcDistAcct = lcODistAcct
      *B605162,1 MHM 12/20/2001 in case of did not choose any account from browse [start]
      llNew = .F. 
      *B605162,1 MHM 12/20/2001 [End]
    ENDIF
    SHOW GET lcDistAcct
    SHOW GET ibDistAcc 

    =lfDefinePad() 
  ENDIF
  IF EMPTY(STRTRAN(STRTRAN(lcDistAcct,'-'),'0'))
    lcDistAcct = lcODistAcct
*B600691,1 M.H 12/03/95 When the user browse the account and return
*B600691,1 M.H 12/03/95 from the browse without sellect anything we
*B600691,1 M.H 12/03/95 have to remove the added line, enable the new
*B600691,1 M.H 12/03/95 remove buttons.
    llNew = .F. 
    IF lnLineNo > 0
      SHOW GET pbRemove ENABLE
    ELSE
      SHOW GET pbRemove DISABLE
    ENDIF
    SHOW GET pbNew      ENABLE
    SHOW GET lnDstprct  ENABLE
    SHOW GET lcDistAcct DISABLE
    SHOW GET ibDistAcc  DISABLE
    SHOW GET lnDstPrct  DISABLE
    SHOW GET nAPDLinNo  DISABLE
    SHOW GET puTax      DISABLE
    SHOW GET ibTaxCode  DISABLE
*B600691,1 M.H 12/03/95 
  ENDIF  
ENDIF

SELECT (lcTmpDt)
IF llNew = .T. .AND. EMPTY(lcTcode) .AND. !EMPTY(STRTRAN(STRTRAN(lcDistAcct,'-'),'0')) 
  lnLineNo=lnLineNo+1  
  INSERT INTO(lcTmpDt);
        (cAutMType, cAutMCode,  cAPDGlAct,  NAPDAMNT  ,CTAXCODE, cStatus , nAPDLinNo);
  VALUES(laData[1], laData[2],  lcDistAcct, lnDstPrct , lcTcode, 'A'     , lnLineNo)
  =gfAdd_Info(lcTmpDt)
  lnNoOfRec = RECCOUNT()  && Assign the no of records in the Temp file to a variable.
  IF lnNoOfRec = 0 .AND. !laScrMode[1]
    SHOW GET pbRemove DISABLE
  ELSE  
    SHOW GET pbRemove ENABLE
  ENDIF  
  SHOW GET lnDstprct  ENABLE
  SHOW GET rbMbase    DISABLE
  SHOW GET pbNew      ENABLE
ELSE
  IF !EMPTY(STRTRAN(STRTRAN(lcDistAcct,'-'),'0'))
    REPLACE CAPDGLACT WITH lcDistAcct
    IF CSTATUS <> "A"
      REPLACE CSTATUS WITH "M" 
    ENDIF   
  ENDIF  
ENDIF

SHOW WINDOW (lcBrTtl) REFRESH

SELECT APINVAHD

llNew    = .F.
llBrowse = .F.
SHOW GET lcDistAcct
SHOW GET ibDistAcc   
SHOW GET lnDstPrct

IF rbmbase = 1
  SHOW GET lcTcode 
ENDIF  

=lfREFRESH()

*!**************************************************************************
*!
*!      Function : lfvData_2
*!
*!**************************************************************************
*
FUNCTION lfvData_2

SELECT APINVAHD
laData[2]     = ALLTRIM(laData[2])
laData[2]     = IIF(ISDIGIT(LEFT(laData[2],1)),;
                         PADL(laData[2], FSIZE('cAutMCode','APINVAHD'),'0'),;
                         PADR(laData[2],FSIZE('cAutMCode','APINVAHD')))
SHOW GET laData[2]
IF llBrowse .OR. ATC("?",laData[2]) > 0 .AND. LASTKEY() = 13
  llBrowse = .F.
  =gfBrows()  
ELSE
  IF !EMPTY(laData[2]) .AND. LASTKEY()= 13 
    =gfSeekRec()
    rbmbase    = AT(APSETUP.CTAXTYPE,"LT")  && let the radio button stand on the right postion.
    SHOW GET rbmbase
  ENDIF
ENDIF
laData[1]  =  'T'

=lfRefresh()

*!**************************************************************************
*!
*!      Function : lfObjDisp
*!
*!**************************************************************************
FUNCTION lfObjDisp
PARAMETER lcObjState

IF rbmbase = 1
  SHOW GET ibTaxCode  &lcObjState
  SHOW GET puTax      &lcObjState
ENDIF  
SHOW GET ibDistAcc  &lcObjState
SHOW GET lcDistAcct &lcObjState
SHOW GET lnDstPrct  &lcObjState
SHOW GET puTax

*!**************************************************************************
*!
*!      Function : lfvNew
*!
*!**************************************************************************
* Function to add new checks in the child screen.
*
FUNCTION lfvNew

SELECT(lcTmpDt)

llNew = .T.

lcDistAcct = lcEmptyAcc
lnDstPrct  = 00.00 
lcTcode    = " "
lcTaxCode  = lcTBlnkCode
puTax      = lcTaxCode

SHOW GET pbRemove   DISABLE
SHOW GET pbNew      DISABLE

=lfrefresh()

=lfObjDisp ("ENABLE")

IF rbMbase = 1
  ACTIVATE WINDOW(lcReadNam2) 
  SHOW GET ibTaxCode ENABLE
  SHOW GET puTax     ENABLE
  _CUROBJ = OBJNUM(ibTaxCode) 
ELSE
  SHOW GET lcDistAcct ENABLE
  SHOW GET ibDistAcc  ENABLE 
  _CUROBJ = OBJNUM(lcDistAcct)
ENDIF

SHOW GET lnDstprct DISABLE
SELECT APINVAHD

*!**************************************************************************
*!
*!      Function : lfvRemove
*!
*!**************************************************************************
*
FUNCTION lfvRemove

** MESSAGE : " Are you sure you want to remove it ? **
**           "     ®  Remove  ¯       <  Cancel >         **
IF gfModalGen("QRM00007B00007","ALERT") = 1

  SELECT(lcTmpDt)

  *B605162,1 MHM 12/20/2001 go to appropriate record [Start]
  LOCATE FOR NAPDLINNO = lnLineNo
  *B605162,1 MHM 12/20/2001 [End]

  lnLineNo   = lnLineNo - 1
  REPLACE cStatus WITH SUBSTR("DSD",AT(cStatus,"MAS"),1)

  *** Delete the current record (to be removed )
  *** and update the line number field
  DELETE
  *** Check if you have to go to next record or the top one
  SKIP 
  IF !EOF(lcTmpDt)
    lnRecNo = RECNO()
    REPLACE REST nAPDLinNo WITH nAPDLinNo - 1,;
                 cStatus  WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)
    GO lnRecNo
  ELSE
    SKIP -1  
  ENDIF  
  SHOW WINDOW (lcBrTtl) REFRESH
  IF lnLineNo = 0
    SHOW GET pbRemove DISABLE  
    SHOW GET Rbmbase  ENABLE
    ***Intialize Variables.
    lcDistAcct  = lcEmptyAcc
    lnDstPrct  = 00.00 
    lcTcode    = " "
  ELSE   
    SHOW GET Rbmbase DISABLE 
    ** If Add or Edit mode so we are going to enable the Remove Button.
    IF laScrMode[3] .OR. laScrMode[4]
      SHOW GET pbRemove ENABLE
    ELSE
      ** If View mode so we are going to Disable the Remove Button.
      SHOW GET pbRemove DISABLE
    ENDIF  
    ***Restore the new record values.
    lcTCode   = &lcTmpDt..CTAXCODE
    lcDistAcct= &lcTmpDt..CAPDGLACT
    lnDstPrct = &lcTmpDt..NAPDAMNT
  ENDIF  

  *E300643,1 Change this line for the changes we have made 
  *          to (gfCodDes) [Begin]
  *lcTaxCode = gfCodDes(cTaxCode)  
  lcTaxCode = gfCodDes(cTaxCode , 'CTAXCODE')
  *E300643,1 Change this line [End]
  
  puTax     = lcTaxCode
  =lfRefresh()
  =lfObjDisp ("DISABLE")

  SELECT APINVAHD
ENDIF

*!**************************************************************************
*!
*!      Function : lfvmbase
*!
*!**************************************************************************
* Fuction to remove the records from the checks screen.
*
FUNCTION lfvmbase

laData[3]  = SUBSTR("LT",rbmbase,1)

IF Rbmbase  = 1
  
  *B607539,1 EIH 01/02/2006 Check if there is tax codes or not [Begin].
  *ACTIVATE WINDOW(lcReadNam2)
  *RELEASE WINDOW(lcBrTtl)
  *=lfDispBrow()
  lcCurAlias = ALIAS()
  SELECT CODES
  SKIP
  IF !EOF()
    GOTO 1
    ACTIVATE WINDOW(lcReadNam2)
    RELEASE WINDOW(lcBrTtl)
    =lfDispBrow()
  ELSE
    *lcMsg = 'Ther is no tax code. please enter tax code first  ' 
     =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Ther is no tax code. please enter tax code first  ')
    GOTO 1
    Rbmbase  = 2
    SHOW GET rbmbase 
  ENDIF  
  SELECT (lcCurAlias)
  *B607539,1 EIH [End]
  
ELSE
  RELEASE WINDOW(lcBrTtl)
  =lfDispBrow()
  ACTIVATE WINDOW(lcReadNam3)
  IF laScrMode[3]
    lcTaxCode = lcTBlnkCode
    lcTCode   = " "
    SELECT (lcTmpDt)
    REPLACE CTAXCODE WITH lcTCode
    IF CSTATUS <> "A"
      REPLACE CSTATUS WITH "M" 
    ENDIF   
  ENDIF  
  SELECT APINVAHD
ENDIF

=lfRefresh()

*!**************************************************************************
*!
*!      Function : lfwDstPrct
*!
*!**************************************************************************
*
FUNCTION lfwDstPrct

lnODstPrct = lnDstPrct
lnTotPrct  = 0
SELECT(lcTmpDt)
lnTmpRcNo=RECNO()
SCAN
  lnTotPrct   =lnTotPrct+NAPDAMNT
ENDSCAN

*IHB CHK EOF()
IF !EOF()
  GO lnTmpRcNo
ENDIF  

SELECT APINVAHD

*!**************************************************************************
*!
*!      Function : lfvDstPrct
*!
*!**************************************************************************
*
FUNCTION lfvDstPrct

IF lnDstPrct = lnODstPrct
  RETURN
ENDIF

IF &lcTmpDt..NAPDAMNT<>0
  lnNetTot=lnTotPrct+lnDstPrct-&lcTmpDt..NAPDAMNT
ELSE
  lnNetTot=lnTotPrct+lnDstPrct
ENDIF

IF lnNetTot > 100
  lnDstPrct = lnODstPrct 
  ** MESSAGE : " The Total distributed percent cannot be greater than 100 " **
  **           "       <  Ok  >         **
  =gfModalGen("TRM04088B00000","DIALOG")
  RETURN
ELSE
  IF lnDstPrct < 0
    lnDstPrct = lnODstPrct 
    RETURN
  ELSE
    SELECT (lcTmpDt)
    
    *B803555,1 Get the Record number to update the percentage [Begin]

    *B605162,1 MHM 12/20/2001 Get the right Record number to update the percentage [Start]
    *LOCATE FOR nApdLinNo = lnTmpRcNo
    LOCATE FOR nApdLinNo = lnLineNo
    *B605162,1 MHM 12/20/2001 [End]

    *B803555,1 Get the Record number to update the percentage [End]
    
    REPLACE NAPDAMNT WITH lnDstPrct
    IF CSTATUS <> "A"
      REPLACE CSTATUS WITH "M" 
    ENDIF
    SELECT APINVAHD
  ENDIF  
ENDIF

SHOW WINDOW (lcBrTtl) REFRESH

ACTIVATE WINDOW (lcReadNam5)

SHOW GET pbNew    ENABLE
SHOW GET pbRemove ENABLE

*!**************************************************************************
*!
*!      Function : lfwTaxCode
*!
*!**************************************************************************
*
FUNCTION lfwTaxCode

lcOTaxCode = lcTaxCode
lcOTCode   = lcTCode
lcCodeFilt = "CTAXCODE  "

*!**************************************************************************
*!
*!      Function : lfvTaxCode
*!
*!**************************************************************************
* Get division code. 
*
FUNCTION lfvTaxCode

DO CASE
  CASE _DOS
    lcTCode = gfActPop(0,28,5,60,'SYCCODES','cCode_No','cDiscrep',@lcTaxCode)
  CASE _WINDOWS

    *E300643,1 Change this lines for the changes we have made 
    *          to SYCCODES [Begin]
    *puTax   = SYCCODES.cdiscrep
    *lcTCode = SYCCODES.cCode_No
    puTax   = CODES.cdiscrep
    lcTCode = CODES.cCode_No
    *E300643,1 Change this lines [End]
    
    SHOW GET puTax
    =gfUpdate()
ENDCASE

IF _WINDOWS
  HIDE POPUP puTaxCode
ENDIF

SELECT(lcTmpDt)

IF !EMPTY(lcTcode)
  IF llNew = .T.  && IF NEW RECORD
    lnLineNo   = lnLineNo + 1
    =lfObjDisp ("ENABLE")
    **Function return related fields ->gl input account.

    *E300643,1 Change this line for the changes we have made 
    *          to (gfRltFld) [Begin]
    *=gfRltFld(lcTcode,@laTaxCode)
    =gfRltFld(lcTcode , @laTaxCode , 'CTAXCODE')
    *E300643,1 Change this line [End]
    
    lcDistAcct = SUBSTR(lcDistAcct,1,lnApsAcLen)
    IF APSETUP.CAPSGLLINK = 'Y'
      IF !EMPTY(LOOKUP(lcLinkChar.CACCNLDES,lcDistAcct,lcLinkChar.CACCTCODE,"ACCTCODE"))
        INSERT INTO(lcTmpDt);
           (cAutMType, cAutMCode, NAPDAMNT  ,CTAXCODE, cStatus , nAPDLinNo);
           VALUES(laData[1], laData[2], lnDstPrct , lcTcode, 'A'     , lnLineNo) 
           =gfAdd_Info(lcTmpDt)
        lnNoOfRec = RECCOUNT()  && Assign the no of records in the Temp file to a variable.
        IF lnNoOfRec = 0 .AND. !laScrMode[1]
          SHOW GET pbRemove DISABLE
        ELSE  
          SHOW GET pbRemove ENABLE
          SHOW GET rbmbase DISABLE
        ENDIF  
        SELECT (lcTmpDt)
        REPLACE CAPDGLACT WITH lcDistAcct
        ACTIVATE WINDOW(lcReadNam4)
        SHOW GET lcDistAcct DISABLE
        SHOW GET ibDistAcc  DISABLE
        _CUROBJ = OBJNUM(lnDstPrct)  
      ELSE
        _CUROBJ = OBJNUM(ibTaxCode)
        ** MESSAGE : " G/L input account # for tax code is not found " **
        **           " in the chart of accounts you can not select this tax code. " **
        **           "       <  Ok  >         **
        =gfModalGen("TRM04091B00000","DIALOG",ALLTRIM(lcDistAcct)+"|"+ALLTRIM(lcTaxCode))
        lcTcode   = " "
        lcTaxCode = lcTBlnkCode
        lcDistAcct = lcEmptyAcc
        puTax = lcTBlnkCode
        =lfRefresh()
        lnLineNo   = lnLineNo - 1
        RETURN
      ENDIF    
    ELSE  && if new record and GL not linked.
      SELECT (lcTmpDt)
      INSERT INTO(lcTmpDt);
          (cAutMType, cAutMCode, NAPDAMNT  ,CTAXCODE, cStatus , nAPDLinNo);
          VALUES(laData[1], laData[2], lnDstPrct , lcTcode, 'A'     , lnLineNo) 
          =gfAdd_Info(lcTmpDt)
      lnNoOfRec = RECCOUNT()  && Assign the no of records in the Temp file to a variable.
      SHOW GET pbNew ENABLE
      IF lnNoOfRec = 0 .AND. !laScrMode[1]
        SHOW GET pbRemove DISABLE
      ELSE  
        SHOW GET pbRemove ENABLE
        SHOW GET rbmbase DISABLE
      ENDIF  
      IF !EMPTY(STRTRAN(STRTRAN(lcDistAcct,'-'),'0'))
        REPLACE CAPDGLACT WITH lcDistAcct
      ENDIF  
      ACTIVATE WINDOW(lcReadNam4)
      SHOW GET lcDistAcct DISABLE
      SHOW GET ibDistAcc  DISABLE
      _CUROBJ = OBJNUM(lnDstPrct) 
    ENDIF  
    llNew = .F.
  ELSE && If not a new record.
    **Function return related fields ->gl input account.

    *E300643,1 Change this line for the changes we have made 
    *          to (gfRltFld) [Begin]
    *=gfRltFld(lcTcode,@laTaxCode)
    =gfRltFld(lcTcode , @laTaxCode , 'CTAXCODE')
    *E300643,1 Change this line [End]
    
    lcDistAcct = SUBSTR(lcDistAcct,1,lnApsAcLen)
    IF APSETUP.CAPSGLLINK = 'Y'
      IF !EMPTY(LOOKUP(lcLinkChar.CACCNLDES,lcDistAcct,lcLinkChar.CACCTCODE,"ACCTCODE"))
        SELECT (lcTmpDt)
        REPLACE CTAXCODE  WITH lcTcode
        REPLACE CAPDGLACT WITH lcDistAcct
        SHOW GET ibTaxCode  ENABLE
        SHOW GET puTax      ENABLE
        ACTIVATE WINDOW(lcReadNam4)
        SHOW GET lcDistAcct DISABLE
        SHOW GET ibDistAcc  DISABLE
        _CUROBJ = OBJNUM(lnDstPrct)  
      ELSE
        _CUROBJ = OBJNUM(ibTaxCode)
        ** MESSAGE : " G/L input account # for tax code is not found " **
        **           " in the chart of accounts you can not select this tax code. " **
        **           "       <  Ok  >         **
        =gfModalGen("TRM04091B00000","DIALOG",ALLTRIM(lcDistAcct)+"|"+ALLTRIM(lcTaxCode))
        lcTcode = " "
        SELECT (lcTmpDt)
        lcDistAcct = CAPDGLACT
        lcTaxCode  = lcOTaxCode
        puTax      = lcTBlnkCode
        =lfRefresh()
        RETURN
      ENDIF    
    ELSE  
      SELECT (lcTmpDt)
      REPLACE CTAXCODE  WITH lcTcode
      IF !EMPTY(STRTRAN(STRTRAN(lcDistAcct,'-'),'0'))
        REPLACE CAPDGLACT WITH lcDistAcct
      ENDIF  
      SHOW GET ibTaxCode  ENABLE
      SHOW GET puTax      ENABLE
      ACTIVATE WINDOW(lcReadNam4)
      SHOW GET lcDistAcct DISABLE
      SHOW GET ibDistAcc    DISABLE
      _CUROBJ = OBJNUM(lnDstPrct) 
    ENDIF  
  ENDIF  
  IF CSTATUS <> "A"
    REPLACE CSTATUS WITH "M" 
  ENDIF   

  ACTIVATE WINDOW(lcReadNam4)
  _CUROBJ = OBJNUM(lcDistAcct)  
ELSE && If empty of Tax Code
  IF !(llNew)
    REPLACE CTAXCODE  WITH lcTcode    
  ENDIF  
  SHOW GET ibTaxCode  ENABLE
  SHOW GET puTax      ENABLE
  ACTIVATE WINDOW(lcReadNam4)
  SHOW GET lcDistAcct ENABLE
  SHOW GET ibDistAcc  ENABLE
  _CUROBJ = OBJNUM(lcDistAcct)      
ENDIF

SELECT APINVAHD
SHOW WINDOW (lcBrTtl) REFRESH

=lfrefresh()

IF _WINDOWS
  DEACTIVATE POPUP puTaxCode
ENDIF  

*!**************************************************************************
*!
*!      Procedure : lpSavScr
*!
*!**************************************************************************
* This procedure replaces the default Save procedure for the push button
* "SAVE"

PROCEDURE lpSavScr

EXTERNAL ARRAY laScrMode    
SELECT APINVAHD
*B600691,1 M.H We cannot save the template without the distribution line.
*IF lnNoOfRec = 0 
IF lnNoOfRec = 0 .OR. lnLineNo = 0
  ** MESSAGE : " You can not save the template that has no distribution lines" **
  **           "       <  Ok  >         **
  =gfModalGen("TRM04097B00000","DIALOG")
  llCSave = .F.
  RETURN 
ENDIF

IF laScrMode[4]    && Add mode
  ** If we are in the add mode we are going to add a new blank record to
  ** add the new data.
  SELECT APINVAHD
  APPEND BLANK
ENDIF
  
GATHER FROM laData FIELDS &lcScFields MEMO && Add the new data to the record.

=gfAdd_Info()  && Add the audit information to the record.

SELECT(lcTmpDt)

** If there is any records in the Temp file we are going to add or update
** the checks file.

IF RECCOUNT() > 0
  =gfTmp2Mast('APINVADT',lcTmpDt)  && Function to add or update the temp file.
ENDIF  

SELECT APINVAHD

*!**************************************************************************
*!
*!      Procedure : lpDelScr
*!
*!**************************************************************************
* This procedure replaces the default Delete procedure for the push button
* "DELETE"
PROCEDURE lpDelScr
EXTERNAL ARRAY laScrMode

*** Delete details
SELECT APINVADT
** Make the deleted record blank and then delete.
SCATTER MEMVAR MEMO BLANK
SCAN FOR &lcInvADtEx = laData[1] + laData[2]
  GATHER MEMVAR MEMO
  DELETE
ENDSCAN

*** Delete the user record from the user file
SELECT APINVAHD
** Make the deleted record blank and then delete.
SCATTER MEMVAR MEMO BLANK
GATHER MEMVAR MEMO
DELETE 

** Change the screen mode to the Select mode.
laScrMode    = .F.
laScrMode[1] = .T.

*!**************************************************************************
*!
*!      Function : lfActBrows
*!
*!**************************************************************************
*
FUNCTION lfActBrows

RELEASE PAD _BROWSE OF _MSYSMENU
ON KEY LABEL ALT+B
ACTIVATE WINDOW (lcBrTtl)

*!**************************************************************************
*!
*!      FUNCTION : lfDeAct
*!
*!**************************************************************************
*
FUNCTION lfDeAct

IF WONTOP(lcBrTtl)
  RELEASE PAD _BROWSE OF _MSYSMENU
  ON KEY LABEL ALT+B
  =lfBrowTrap()
  glFromBrow = .T.
ELSE
  =lfBrowUnTrap() 
  ON KEY LABEL ALT+B DO lfActBrows
ENDIF

*!**************************************************************************
*!
*!      FUNCTION : lfBrowTrap
*!
*!**************************************************************************
*
FUNCTION lfBrowTrap

ON KEY LABEL Alt+I    DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+P    DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+N    DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+R    DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL ESC      lnDummy = 1

*!**************************************************************************
*!
*!      PROCEDURE : lpDoTrap
*!
*!**************************************************************************
*
PROCEDURE lpDoTrap
PARAMETER lnLstKey

KEYBOARD '{Z}'
DO CASE
   CASE lnLstKey = 23  && alt+i 
     rbmbase=1 
     =lfvmbase()
   CASE lnLstKey = 25  && alt+p
     rbmbase=2 
     =lfvmbase()
   CASE lnLstKey = 49  && alt+n
     =lfvNew()
   CASE lnLstKey = 19  && alt+r
     =lfvRemove()
ENDCASE

*!**************************************************************************
*!
*!      FUNCTION : lfBrowUnTrap
*!
*!**************************************************************************
*
FUNCTION lfBrowUnTrap

ON KEY LABEL Alt+I
ON KEY LABEL Alt+P
ON KEY LABEL Alt+N
ON KEY LABEL Alt+R
ON KEY LABEL ESC &lcEsc

*!**************************************************************************
*!
*!      Function: lfDefinePad
*!
*!**************************************************************************
*
FUNCTION lfDefinePad

IF _DOS
  DEFINE PAD _BROWSE OF _MSYSMENU PROMPT '\<Browse'
  ON SELECTION PAD _BROWSE OF _msysmenu DO lfActBrows
ENDIF  
=lfBrowUnTrap()
*!**************************************************************************
*!
*!      FUNCTION : lfDeAct
*!
*!**************************************************************************
*
FUNCTION lfDeAct

IF WONTOP(lcBrTtl)
  RELEASE PAD _BROWSE OF _MSYSMENU
  ON KEY LABEL ALT+B
  =lfBrowTrap()
ELSE
  =lfBrowUnTrap() 
  ON KEY LABEL ALT+B DO lfActBrows
ENDIF

*!**************************************************************************
*!
*!      FUNCTION : lfPushKey
*!
*!**************************************************************************
* trap keys.
*
FUNCTION lfPushKey

*** places all current on key labels on a stack in memory.
PUSH KEY
*** deactivate control keys for browse window 
*** ESCAPE, TAB, Ctrl+W, Ctrl+Q, Ctrl+Enter
ON KEY LABEL TAB         DO  lpTab
ON KEY LABEL BACKTAB     DO  lpShfTab 
ON KEY LABEL Ctrl+W      lnDummi = 1
ON KEY LABEL Ctrl+Q      lnDummi = 1
ON KEY LABEL Ctrl+ENTER  lnDummi = 1
ON KEY LABEL Ctrl+HOME   lnDummi = 1
ON KEY LABEL Alt+B      DO  lfActBrows

*!**************************************************************************
*!
*!      Procedure : lpTab
*!
*!**************************************************************************
*
PROCEDURE lpTab

ON KEY LABEL TAB
DO CASE
  CASE WONTOP(lcReadNam1)
    IF _CUROBJ = OBJNUM(rbmbase)+1
      ACTIVATE WINDOW (lcBrTtl)
    ELSE
      _CUROBJ = _CUROBJ + 1
    ENDIF
  CASE WONTOP(lcBrTtl)
    =lfBrowUnTrap()
    IF rbmBase = 1
      ACTIVATE WINDOW (lcReadNam2)
      _CUROBJ=OBJNUM(lcTaxCode)
    ELSE
      ACTIVATE WINDOW (lcReadNam3)
      _CUROBJ=OBJNUM(ibDummy00)  
    ENDIF
  CASE WONTOP(lcReadNam2)  
    IF _CUROBJ = OBJNUM(lcTaxCode)
      =lfvTaxCode()
      ACTIVATE WINDOW (lcReadNam4)
    ELSE
      _CUROBJ = _CUROBJ + 1
    ENDIF
  CASE WONTOP(lcReadNam4)
    IF _CUROBJ = OBJNUM(lnDstPrct)
      =lfvDstPrct()
      ACTIVATE WINDOW (lcReadNam5)
    ELSE
      IF _CUROBJ = OBJNUM(lcDistAcct)
        =lfvDistAcct()
      ENDIF
      _CUROBJ = _CUROBJ + 1
    ENDIF
  CASE WONTOP(lcReadNam5)
    IF _CUROBJ = OBJNUM(pbRemove)
      ACTIVATE WINDOW ("GWCCONTRL1")
    ELSE
      _CUROBJ = _CUROBJ + 1
    ENDIF  
  CASE WONTOP("GWCCONTRL1") 
    IF _CUROBJ=OBJNUM(pbCls)
      IF lascrmode[4] OR lascrmode[1]
        ACTIVATE WINDOW (lcReadNam1)
      ELSE
        IF lascrmode[3] AND EOF(lcTmpDt)
          ACTIVATE WINDOW (lcReadNam1)
        ELSE
          ACTIVATE WINDOW (lcBrTtl)
        ENDIF  
      ENDIF  
    ELSE  
      KEYBOARD "{TAB}" PLAIN
    ENDIF
  OTHERWISE  
    KEYBOARD "{TAB}" PLAIN
ENDCASE
ON KEY LABEL TAB         DO  lpTab

*!**************************************************************************
*!
*!      Procedure : lpShfTab
*!
*!**************************************************************************
*
PROCEDURE lpShfTab

ON KEY LABEL BACKTAB
DO CASE
  CASE WONTOP(lcReadNam1)
    IF _CUROBJ = OBJNUM(laData[2])
      ACTIVATE WINDOW ("GWCCONTRL1")
      _CUROBJ=OBJNUM(pbcls)
    ELSE
      IF lascrmode[3] OR lascrmode[4]
         ACTIVATE WINDOW ("GWCCONTRL1")
         _CUROBJ=OBJNUM(pbcls)
      ELSE
        _CUROBJ = _CUROBJ - 1
      ENDIF        
    ENDIF
  CASE WONTOP(lcBrTtl)
    IF lascrmode[2] OR lascrmode[4]
      ACTIVATE WINDOW ("GWCCONTRL1")
      _CUROBJ=OBJNUM(pbcls)
    ELSE
      IF lascrmode[3] AND !EOF(lcTmpDt)
        ACTIVATE WINDOW ("GWCCONTRL1")
        _CUROBJ=OBJNUM(pbcls)
      ELSE
        ACTIVATE WINDOW (lcReadNam1)
        _CUROBJ=OBJNUM(rbmbase)
      ENDIF  
    ENDIF 
  CASE WONTOP(lcReadNam2)
    IF _CUROBJ = OBJNUM(ibTaxCode)
      ACTIVATE WINDOW (lcBrTtl)
    ELSE
      KEYBOARD "{BACKTAB}" PLAIN    
    ENDIF
  CASE WONTOP(lcReadNam4)  
    IF rbmbase=1 
      IF EMPTY(putax) OR EMPTY(lcTaxCode)
        IF _CUROBJ = OBJNUM(lcDistAcct)
          ACTIVATE WINDOW (lcReadNam2)
        ELSE
          _CUROBJ = _CUROBJ - 1
        ENDIF
      ELSE
        ACTIVATE WINDOW (lcReadNam2)
      ENDIF 
    ELSE
      IF _CUROBJ = OBJNUM(lcDistAcct)
        ACTIVATE WINDOW (lcBrTtl)
      ELSE
        _CUROBJ = _CUROBJ - 1
      ENDIF
    ENDIF 
  CASE WONTOP(lcReadNam5)  
    IF _CUROBJ = OBJNUM(pbNew)
      IF (lascrmode[3] OR lascrmode[4]) AND EOF(lcTmpDt)
        ACTIVATE WINDOW (lcReadNam1)
        _CUROBJ=OBJNUM(rbmbase)
      ELSE
        ACTIVATE WINDOW (lcReadNam4)
        _CUROBJ = OBJNUM(lnDstPrct)
      ENDIF  
    ELSE
      _CUROBJ = _CUROBJ - 1
    ENDIF  
  CASE WONTOP("GWCCONTRL1") 
    IF _CUROBJ=OBJNUM(pbTop) OR _CUROBJ=OBJNUM(pbBtm)
      IF lascrmode[2]
        ACTIVATE WINDOW (lcBrTtl)
      ELSE
        ACTIVATE WINDOW (lcReadNam5) 
      ENDIF
    ELSE  
      KEYBOARD "{BACKTAB}" PLAIN
    ENDIF
  OTHERWISE  
    KEYBOARD "{BACKTAB}" PLAIN
ENDCASE
ON KEY LABEL BACKTAB     DO  lpShfTab 