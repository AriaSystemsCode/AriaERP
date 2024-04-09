*:************************************************************************
*:  Program File: APTMPDS.PRG
*:  Desc.       : Template Distribution
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 02/14/2012
*:  Reference   : E303067,1
*:************************************************************************
*: Modifications
*!B609980,1 SAB 06/27/2012 Fix error in refresh grid and in percentage field [Media R12 Issues]
*:************************************************************************
*- Call the screen
*N000682,1 MMT 12/09/2012 Globalization changes[Start]           
#INCLUDE R:\Aria4xp\prgs\AP\APTMPDS.h
*N000682,1 MMT 12/09/2012 Globalization changes[END]           
lcRunScx = lfGetScx("AP\aptmpds.scx")

DO FORM (lcRunScx)

************************************************************
*! Name      : lfGetScx
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/14/2012
*! Purpose   : Get the scx path to run in SaaS environemt
************************************************************
FUNCTION lfGetScx
PARAMETERS lcScx
LOCAL lcRunScx
IF oAriaApplication.Multiinst AND FILE(oAriaApplication.clientscreenhome+lcScx)
  lcRunScx = oAriaApplication.clientscreenhome+lcScx
ELSE
  lcRunScx = oAriaApplication.screenhome+lcScx
ENDIF
RETURN lcRunScx
 *- End of lfGetScx.

*!*************************************************************
*! Name      : lfFormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/14/2012
*! Purpose   : called from the Screen init Method
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet

*- Set functions to the APMAIN.FXP
lcPath = oAriaapplication.ApplicationHome+oAriaapplication.ActiveModuleID
SET PROCEDURE TO (lcPath+'\APMAIN.FXP') ADDITIVE

*- Open tables
=lfOpenPRGFILES('APTMPDS')

*** Load program base file
=lfAddProp(loFormSet,'lcBaseFile',ALLTRIM(sydObjct.cBaseFile))

*- Define variables
=lfDefineVars(loFormSet)

*- initializations
WITH loFormSet
  .cbrowsetabledbengine   = "NATIVE"
  .nWorkArea                            = .lcBaseFile
  .otoolbar.nWorkArea                   = .lcBaseFile
  .DataEnvironment.InitialSelectedAlias = .lcBaseFile
  .cBrowseFileName        = .lcBaseFile
  .cBrowseIndexExpression = "'T'+CAUTMCODE"
  .cBrowseIndexFields     = "CAUTMTYPE+CAUTMCODE"
  .cBrowseIndexName       = 'HTYPCOD'
  .cBrowseAliasName       = .lcBaseFile
  .cBrowseTableName       = .lcBaseFile
  .cBrowseFilter          = ""
  .BrowseTitle 		  	    = loFormSet.lcFile_Ttl
  .ariaBrFields.edtBrowseFields.Value = gfDbfField(.nWorkArea)
ENDWITH


* Old : EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc
* Old : DECLARE laKeyField[2,4],laFileStru[1],laTaxCode[1,2]

* Old : laKeyField[1,1] = 'laData[1]'
* Old : laKeyField[1,2] = .F.
* Old : laKeyField[1,3] = 'HTYPCOD'
* Old : laKeyField[1,4] = 1
* Old : laKeyField[2,1] = 'laData[2]'
* Old : laKeyField[2,2] = .T.
* Old : laKeyField[2,3] = 'HTYPCOD'
* Old : laKeyField[2,4] = 2

* Old : lcEsc = ON("KEY","ESC")

SELECT APINVAHD

* Old : IF !WEXIST(gcBaseWind)  && If enter the base window for the first time.
  loFormSet.llFrstShow  = .T.

  *E300643,1 Change this line for the changes we have made
  *          to (gfCodDes) [Begin]
  loFormSet.lcTBlnkCode = gfCodDes(' ')
  * Old : lcTBlnkCode = gfCodDes(' ' , ' ')
  *E300643,1 Change this line [End]

  **Fill the array hold related field.

  loFormSet.laTaxCode[1,1] = 'CGLINPACCT'
  loFormSet.laTaxCode[1,2] = 'lcDistAcct'

  * Old : lcBrowName = gfTempName()
  * Old : lcReadNam1 = gfTempName()
  * Old : lcReadNam2 = gfTempName()
  * Old : lcReadNam3 = gfTempName()
  * Old : lcReadNam4 = gfTempName()
  * Old : lcReadNam5 = gfTempName()
  =lfAddProp(loFormSet,'lcTmpDt',gfTempName())

  SELECT APINVADT
  *** Get fields of invoice detail file into an array.
  =AFIELDS(laFileStru)

  lnFileStru = ALEN(laFileStru,1)
  *** increase the dimension of the array.
  *DIMENSION laFileStru[lnFileStru+2,4]
  DIMENSION laFileStru[lnFileStru+2,18]
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

  =lfUpdStruArr(@laFileStru,lnFileStru)

  *** Creat temp  file from the array.
  * Old : CREATE TABLE &gcWorkDir.&lcTmpDt FROM ARRAY laFileStru
  CREATE TABLE (oAriaApplication.WorkDir+loFormSet.lcTmpDt) FROM ARRAY laFileStru

  loFormset.Ariaform1.grdDist.RecordSource = loFormSet.lcTmpDt

  * Old : IF _WINDOWS
    *** Define pop up for tax code from temp file created.

    *E300643,1 Change this lines for the changes we have made
    *          to SYCCODES [Begin]
    *puTax = SYCCODES.cdiscrep
    *DEFINE POPUP puTaxCode prompt field SYCCODES.cdiscrep scroll;
    *FROM 2.45,27.75 TO 8.25,58.00;
    *MESSAGE gfObj_msg()
    * Old : puTax = CODES.cdiscrep
    * Old : DEFINE POPUP puTaxCode prompt field CODES.cdiscrep scroll;
    * Old : FROM 2.45,27.75 TO 8.25,58.00;
    * Old : MESSAGE gfObj_msg()
    * Old : *E300643,1 Change this lines [End]

    * Old : ON SELECTION POPUP puTaxCode DO lfvTaxCode
  * Old : ENDIF

  *SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData BLANK
  loFormSet.laData[1]  = 'T'                 && laData_1 allways keep this value
  loFormSet.laData[2] = SPACE(8)
  loFormSet.laData[3]  = APSETUP.CTAXTYPE    && get value of laData_3 from apsetup.

  loFormSet.Ariaform1.rbmbase.Value;
             = AT(loFormSet.laData[3],"LT")  && let the radio button stand on the right postion.

  lcTaxStat  = "DISABLE"           && get the initial state of tax code and rbmbase.
  loFormset.Ariaform1.lcTaxCode.Enabled = lcTaxStat = 'ENABLE'
  lcBrowFile = "TEMP"              && get the initial browse file name.


* Old : ELSE  && if the base window exists.
* Old :   IF _WINDOWS
* Old :     puTax = lcTaxCode
* Old :   ENDIF
* Old :   *** if add or edit mode and there is no distribution lines.
* Old :   IF lnLineNo = 0 .AND. loFormSet.ActiveMode = 'E' .OR. loFormSet.ActiveMode = 'A'
* Old :     lcTaxStat = "ENABLE"
* Old :   ELSE
* Old :     lcTaxStat = "DISABLE"
* Old :   ENDIF
* Old : ENDIF

*** Trab some keys before browse. ***
* Old : =lfPushKey()

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
lcCodeFilt = "CTAXCODE  "
SET FILTER TO (CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N'+'&lcCodeFilt') OR;
              (CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N'+'N/A')
*E300789,4 IHB [end]

SET ORDER TO &lcSvOrder


SELECT APINVAHD

SELECT APINVADT
SET ORDER TO TAG DTYPCOD
*** Current index expression
loFormSet.lcInvADtEx   = SYS(14,VAL(SYS(21)))

SELECT APINVAHD
SET FILTER TO
SET FILTER TO CAUTMTYPE = "T"

SET ORDER TO TAG HTYPCOD
* Old : SET RELATION TO laData[1]+laData[2] INTO APINVADT ADDITIVE
*lcScFields = 'CAUTMTYPE,CAUTMCODE,CAUTMBASE'
SET RELATION TO CAUTMTYPE+CAUTMCODE INTO APINVADT ADDITIVE


*E300683,1 Call *.SPR from screens directory
*DO ApTmpDs.SPR
* Old : DO (gcScrDir + gcWinAppl + '\ApTmpDs.SPR')
*E300683,1 end

***Release the traped keys.

* Old : ON KEY

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
* Old : SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

* Old : SET FILTER TO
* Old : SELECT APINVAHD
***Release the relation.
* Old : SET RELATION TO
***Release the filter.
* Old : SET FILTER TO
* Old : RELEASE PAD _BROWSE OF _MSYSMENU

***Release the browse window.

* Old : RELEASE WINDOW (lcBrTtl)

* Old : IF glQuitting
* Old :   ***Close all temp files and erase them.
* Old :   IF USED(lcTmpDt)
* Old :     USE IN (lcTmpDt)
* Old :   ENDIF
* Old :
* Old :   ERASE (gcWorkDir+lcTmpDt+".DBF")
* Old :   ERASE (gcWorkDir+lcTmpDt+".CDX")
* Old :   ERASE (gcWorkDir+lcTmpDt+".FPT")
* Old :
* Old : ENDIF
* Old : glFromBrow  = .F.
loFormSet.ChangeMode('S')


*- End of lfFormInit.
************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/14/2012
*! Purpose   : lfDefineVars
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet

=lfAddProp(loFormSet,'ap1',CREATEOBJECT('ap'))

=lfAddProp(loFormSet,'lnDstPrct   , lnODstPrct , lnTotPrct',0.00)

lcVars = 'lnNetTot    , lnNoOfRec  , lnLineNo   , lnTmpRcNo   ,'+;
         'lnApdLinNo  , nAPDLinNo'
=lfAddProp(loFormSet,lcVars,0)

=lfAddProp(loFormSet,'rbmbase     , ibTaxCode',1)


lcVars = 'llFrstShow  , llBrowse   , llNew      , laDefProc[7], laDefProc[9]'
=lfAddProp(loFormSet,lcVars,.F.)

lcVars  =             'lcBrowName  , lcReadNam1 , lcReadNam2 , lcReadNam3  ,'+;
                      'lcReadNam4  , lcReadNam5 , lcTTaxCode , lcTAcct     ,'+;
                      'lcTDisc     , lcTDstPrct , lcTNo      , lcBrowStr   ,'+;
                      'lcSavOrd    , lcInvADtEx , lcODistAcct, lcObjState  ,'+;
                      'lcAccDisc   , lcBrowFile , lcTBlnkCode, lcDivDisc   ,'+;
                      'lcColPair   , lcTaxDisc  , lcTaxCode  , lcOTaxCode  ,'+;
                      'lcOTCode    , lcTCode    , lcOTCode   , '+;
                      'lcPopCol    , puTax      , puTaxCode  , ibDummy0    ,'+;
                      'lcEsc'
=lfAddProp(loFormSet,lcVars," ")

=lfAddProp(loFormSet,'lcCodeFilt',"CTAXCODE  " )
=lfAddProp(loFormSet,'lcTaxStat','DISABLE')
=lfAddProp(loFormSet,'lcDistAcct',loFormSet.ap1.lcEmptyAcc )
=lfAddProp(loFormSet,'lcBrTtl',"Add / Edit distribution")


lcFile_ttl = PROPER(ALLTRIM(LOOKUP(sydFiles.cFile_ttl,loFormSet.lcBaseFile,;
                 sydFiles.cFile_Nam)))
*N000682,1 MMT 12/09/2012 Globalization changes[Start]                 
IF oAriaApplication.oActivelang.cLang_ID <> "EN"
  lcTrnsFile_ttl =  loFormSet.geta27filettl(loFormSet.lcBaseFile)
  lcFile_ttl = IIF(EMPTY(lcTrnsFile_ttl),lcFile_ttl,lcTrnsFile_ttl)
ENDIF
*N000682,1 MMT 12/09/2012 Globalization changes[END]
=lfAddProp(loFormSet,'lcFile_ttl',lcFile_ttl)

=lfAddProp(loFormSet,'lcScFields','CAUTMTYPE,CAUTMCODE,CAUTMBASE')
=lfAddProp(loFormSet,'laData[3]','')
loFormSet.laData[1] = 'T'

=lfAddProp(loFormSet,'laTaxCode[1]','')
DIMENSION loFormSet.laTaxCode[1,2]

*- Define control Source
WITH loFormSet.Ariaform1
  .kbCAUTMCODE.Keytextbox.ControlSource = 'Thisformset.laData[2]'
  *.rbmbase.ControlSource = 'Thisformset.laData[3]'
  .lcDistAcct.Keytextbox.ControlSource = 'Thisformset.lcDistAcct'
  .lnDstPrct.ControlSource = 'Thisformset.lnDstPrct'
ENDWITH

=lfAddProp(loFormSet,'CAUTMTYPE','T')

lcCAUTMCODE_Ttl = ALLTRIM(LOOKUP(sydField.CFLD_HEAD,'CAUTMCODE ',;
                 sydField.CFLD_NAME))
*N000682,1 MMT 12/09/2012 Globalization changes[Start]                        
IF oAriaApplication.oActivelang.cLang_ID <> "EN"
  lcAUTrnsFH =  loFormSet.geta27fldheader('CAUTMCODE ')
  IF !EMPTY(lcAUTrnsFH) 
    lcCAUTMCODE_Ttl  = lcAUTrnsFH
  ENDIF
ENDIF                        
*N000682,1 MMT 12/09/2012 Globalization changes[END]
=lfAddProp(loFormSet,'LCCAUTMCODE_TTL',LCCAUTMCODE_TTL )

*- End of lfDefineVars.

************************************************************
*! Name      : lfChngLine
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/14/2012
*! Purpose   : lfChngLine
************************************************************
FUNCTION lfChngLine
PARAMETERS loFormSet
LOCAL lnSlct,lcKey
lnSlct = SELECT(0)

loFormset.Ariaform1.kbCAUTMCODE.keytextbox.Value = APINVAHD.CAUTMCODE
loFormset.laData[2] = APINVAHD.CAUTMCODE
loFormSet.laData[3] = APINVAHD.CAUTMBASE

loFormset.Ariaform1.kbCAUTMCODE.keytextbox.Valid()

lcKey = APINVAHD.CAUTMTYPE+APINVAHD.CAUTMCODE
SELECT APINVADT
SET KEY TO lcKey
LOCATE

loFormset.Ariaform1.grdDist.AfterRowColChange()

SELECT(lnSlct )

*- End of lfChngLine.
*************************** original code   *********************************

*!**************************************************************************
*!
*!      Procedure : lpFrstShow
*!
*!**************************************************************************
***Show lpshow for the frist time enter the base window.
PROCEDURE lpFrstShow

IF loFormSet.llFrstShow
  =lpShow()
  loFormSet.llFrstShow  = .F.
ENDIF

************************************************************
*! Name      : lfChangeMode
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/14/2012
*! Purpose   : lfChangeMode
************************************************************
FUNCTION lfChangeMode
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)

*EXTERNAL ARRAY laScrMode

IF TYPE('loFormSet.lcDistAcct')='U'
  RETURN
ENDIF

*!*	***Control if display tax code or not.
*!*	IF rbmbase = 1
*!*	  *ACTIVATE WINDOW(lcReadNam2)
*!*	ELSE
*!*	  *ACTIVATE WINDOW(lcReadNam3)
*!*	ENDIF
loFormSet.Ariaform1.kbCAUTMCODE.Enabled = .F.
loFormset.Ariaform1.grdDist.RecordSource = ''

SELECT APINVADT
SET KEY TO

DO CASE
  *CASE loFormSet.ActiveMode = 'S'    && Select mode.
  CASE loFormSet.ActiveMode = 'S'

    loFormSet.laData[1]       = 'T'
    loFormSet.laData[3]       = APSETUP.CTAXTYPE

    ** Select the Temp file and zap the old record data
    SELECT(loFormSet.lcTmpDt)
    ZAP

    ***intialize variables.
    loFormSet.lnNoOfRec  = 0
    loFormSet.lnLineNo   = 0
    loFormSet.lcDistAcct = loFormSet.ap1.lcEmptyAcc
    loFormSet.lnDstPrct  = 00.00
    loFormSet.lcTcode    = " "
    *lcTaxCode  = IIF(EMPTY(lcTcode),lcTBlnkCode,lcTaxCode)
    loFormSet.Ariaform1.lcTaxCode.Value  = ''
    loFormSet.Ariaform1.lcTaxCode.Valid()
    *loFormSet.puTax      = lcTaxCode
    loFormSet.lcObjState = "DISABLE"

    WITH loFormSet.Ariaform1
    .kbCAUTMCODE.Enabled = .T.
    .rbmbase.Enabled = .F. && DISABLE
    .kbCAUTMCODE.Keytextbox.SetFocus()
    ENDWITH


  CASE loFormSet.ActiveMode = 'V' .OR. loFormSet.ActiveMode = 'E'    && View or Edit mode.

    IF loFormSet.ActiveMode = 'E'  && Edit mode.
      ***fill the temprary file from the main file.
      SELECT *,RECNO() AS 'nRecNo',"S" AS 'cStatus';
        FROM (oAriaApplication.DataDir+"APINVADT");
       WHERE CAUTMTYPE+CAUTMCODE = loFormSet.laData[1]+loFormSet.laData[2];
       ORDER BY CAUTMTYPE,CAUTMCODE,nAPDLinNo;
        INTO DBF (oAriaApplication.WorkDir+loFormSet.lcTmpDt)
      loFormSet.lnNoOfRec   = RECCOUNT(loFormSet.lcTmpDt)   && Variable to hold the no of records in the Temp file.
      loFormSet.lnLineNo    = loFormSet.lnNoOfRec           && Get the last line number.
      loFormSet.lcObjState  = "DISABLE"
      IF loFormSet.lnLineNo = 0                   && if there is no distribution lines make rbmbase enable.
        *SHOW GET rbmbase ENABLE
        loFormSet.Ariaform1.rbmbase.ENABLED = .T.
      ENDIF
      loFormset.Ariaform1.grdDist.RecordSource = loFormSet.lcTmpDt
      loFormset.Ariaform1.grdDist.AfterRowColChange()

    ELSE && View mode.

      loFormSet.laData[3] = APINVAHD.CAUTMBASE

      *lcTaxCode = IIF(EMPTY(lcTcode),lcTBlnkCode,lcTaxCode)
      *SHOW GET rbmbase DISABLE
      LOCAL lcKey
      lcKey = APINVAHD.CAUTMTYPE+APINVAHD.CAUTMCODE
      SELECT APINVADT
      SET KEY TO lcKey

      loFormSet.Ariaform1.lcTaxCode.Value = APINVADT.CTAXCODE
      loFormSet.Ariaform1.lcTaxCode.Valid()
      loFormSet.Ariaform1.rbmbase.ENABLED = .F.

      *lcTaxCode = gfCodDes(APINVADT.cTaxCode , 'CTAXCODE')
      *puTax     = lcTaxCode
      loFormSet.lcDistAcct= APINVADT.CAPDGLACT
      loFormSet.lnDstPrct = APINVADT.NAPDAMNT
      loFormset.Ariaform1.grdDist.RecordSource = 'APINVADT'
      loFormset.Ariaform1.grdDist.AfterRowColChange()
    ENDIF

    ** If Edit mode & no of records of the Temp file is > 0 we are going
    ** to ENABLE the objects else we are going to DISABLE it.
    loFormSet.lcObjState  = IIF(loFormSet.ActiveMode = 'E' .AND. loFormSet.lnNoOfRec > 0,'ENABLE','DISABLE')

  CASE loFormSet.ActiveMode = 'A'    && Add mode.
    loFormSet.lcObjState  = 'DISABLE'   && Disable the objects screen.
    IF loFormSet.lnLineNo = 0
      *SHOW GET rbmbase ENABLE
      loFormSet.Ariaform1.rbmbase.ENABLED = .T.
    ENDIF
    loFormset.Ariaform1.grdDist.RecordSource = loFormSet.lcTmpDt
ENDCASE

lcDist = IIF(loFormSet.ActiveMode = 'V' , 'APINVADT',loFormSet.lcTmpDt )
WITH loFormset.Ariaform1.grdDist
  .Column1.ControlSource = '&lcDist..nAPDLinNo'
  .Column2.ControlSource = '&lcDist..cTaxCode'
  .Column3.ControlSource = '&lcDist..cApdGLAct'
  .Column4.ControlSource = 'LOOKUP(GLACCHAR.CACCNLDES,&lcDist..cApdGLAct,GLACCHAR.CACCTCODE)'
  .Column5.ControlSource = '&lcDist..nAPDAmnt'
ENDWITH

loFormSet.ariaform1.rbmbase.Value = AT(loFormSet.laData[3],"LT")  && Get the value of the radio button screen.
=lfvmbase(loFormSet)

=lfObjDisp(loFormSet,loFormSet.lcObjState)

*** If there is no records in the Temp file so we are going to DISABLE
*** the remove button.
loFormSet.lnLineNo   = loFormSet.lnNoOfRec
WITH loFormSet.Ariaform1
IF loFormSet.lnNoOfRec = 0 .AND. !loFormSet.ActiveMode = 'S'
  *SHOW GET pbRemove   DISABLE
  .pbRemove.Enabled = .F.
ELSE
  *** If Add or Edit mode.
  IF loFormSet.ActiveMode = 'E' .OR. loFormSet.ActiveMode = 'A'
    *SHOW GET pbRemove ENABLE
    .pbRemove.Enabled = .T.
  ELSE
    *** If View mode.
    *SHOW GET pbRemove   DISABLE
    .pbRemove.Enabled = .F.
  ENDIF
ENDIF

IF loFormSet.ActiveMode = 'E' .OR. loFormSet.ActiveMode = 'A'
  *SHOW GET pbNew ENABLE
  .pbNew.Enabled = .T.
  IF !EMPTY(.lcTaxCode.Value)
    *SHOW GET lcDistAcct DISABLE
    *SHOW GET ibDistAcc  DISABLE
    .lcDistAcct.Enabled = .F.
  ELSE
    IF !(loFormSet.llFrstShow) .AND. !(loFormSet.ActiveMode = 'A')
      *SHOW GET lcDistAcct ENABLE
      *SHOW GET ibDistAcc  ENABLE
      .lcDistAcct.Enabled = .T.
    ENDIF
  ENDIF
ELSE
  *SHOW GET pbNew      DISABLE
  *SHOW GET lcDistAcct DISABLE
  *SHOW GET ibDistAcc  DISABLE
  .pbNew.Enabled = .F.
  .lcDistAcct.Enabled = .F.
ENDIF
ENDWITH
*SHOW GET lnDstPrct
*SHOW GET nAPDLinNo

*** Define popup color. ***
*lcPopCol = IIF(loFormSet.ActiveMode = 'S' .OR. loFormSet.ActiveMode = 'V', SCHEME(1,10), SCHEME(1,2))

*!*	IF rbmbase = 1
*!*	  SHOW GET lcTcode
*!*	ENDIF

SELECT APINVAHD

SELECT(lnSlct)
*- End of lfChangeMode.

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
  CASE loFormSet.ActiveMode = 'V'    && Select or View mode.
    lcBrowFile = "MAST"
    SELECT APINVADT
    lcBrTtl     = "View distribution"
   OTHERWISE           && Add or Edit mode.
    lcBrowFile = "TEMP"
    SELECT (loFormSet.lcTmpDt)
    lcBrTtl     = "Add / Edit distribution"
ENDCASE

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
    RETURN TO ApTmpDs
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
  CASE loFormSet.ActiveMode = 'V'    && View mode.
    SELECT APINVADT
  CASE (loFormSet.ActiveMode = 'S' .OR. loFormSet.ActiveMode = 'E' .OR. loFormSet.ActiveMode = 'A')    && Edit or Add mode.
    SELECT (loFormSet.lcTmpDt)
ENDCASE

*SHOW WINDOW (lcBrTtl) REFRESH

lcTmpDt = loFormSet.lcTmpDt
IF (loFormSet.ActiveMode = 'S' .OR. loFormSet.ActiveMode = 'E' .OR. loFormSet.ActiveMode = 'A')
  lcTCode   = &lcTmpDt..CTAXCODE
  loFormSet.lcDistAcct = &lcTmpDt..CAPDGLACT

  *B605162,3 MHM 12/24/2001 [start]
  *lnDstPrct = &lcTmpDt..NAPDAMNT
  IF loFormSet.ActiveMode = 'A'
    IF lnNetTot<100
      lnDstPrct = &lcTmpDt..NAPDAMNT
    ENDIF
  ELSE
    lnDstPrct = &lcTmpDt..NAPDAMNT
  ENDIF
  *B605162,3  [end]

ELSE
  lcTCode   = APINVADT.CTAXCODE
  loFormSet.lcDistAcct = APINVADT.CAPDGLACT
  loFormSet.lnDstPrct = APINVADT.NAPDAMNT
ENDIF

*E300643,1 Change this line for the changes we have made to (gfCodDes) [Begin]
*lcTaxCode = gfCodDes(cTaxCode)
lcTaxCode = gfCodDes(cTaxCode , 'CTAXCODE')
*E300643,1 Change this line for the changes we have made to (gfCodDes) [End]

puTax     = lcTaxCode

*!*	SHOW GET lcDistAcct
*!*	SHOW GET ibDistAcc
*!*	SHOW GET lnDstPrct
*!*	SHOW GET nAPDLinNo
*!*	SHOW GET puTax

*!*	IF rbmbase = 1
*!*	  SHOW GET lcTcode
*!*	ENDIF

glFromBrow = .T.

*=lfrefresh()

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
PARAMETERS loFormSet

*!*	IF !llBrowse .AND. (lcODistAcct = lcDistAcct .OR. EMPTY(lcDistAcct))

*!*	  *Begin B600691,1 M.H 09/19/95
*!*	  IF llNew
*!*	    *B605162,1 MHM 12/20/2001 no need to change new to false[Start]
*!*	    *llNew = .F.
*!*	    *B605162,1 MHM 12/20/2001 [End]
*!*	    IF lnLineNo > 0
*!*	      SHOW GET pbRemove  ENABLE
*!*	    ELSE
*!*	      SHOW GET pbRemove  DISABLE
*!*	    ENDIF
*!*	    SHOW GET pbNew     ENABLE
*!*	    SHOW GET lnDstprct ENABLE
*!*	    =lfwTmpBrow()
*!*	  ENDIF
*!*	  *END   B600691,1 M.H 09/19/95

*!*	  RETURN
*!*	ELSE
*!*	  IF llBrowse .OR. !EMPTY(STRTRAN(STRTRAN(lcDistAcct,'-'),'0'))
*!*	    RELEASE PAD _BROWSE OF _MSYSMENU
*!*	    ON KEY LABEL ALT+B
*!*	    IF !lfApAcs(.F.,llBrowse)
*!*	      lcDistAcct = lcODistAcct
*!*	      *B605162,1 MHM 12/20/2001 in case of did not choose any account from browse [start]
*!*	      llNew = .F.
*!*	      *B605162,1 MHM 12/20/2001 [End]
*!*	    ENDIF
*!*	    SHOW GET lcDistAcct
*!*	    SHOW GET ibDistAcc

*!*	    *=lfDefinePad()
*!*	  ENDIF
*!*	  IF EMPTY(STRTRAN(STRTRAN(lcDistAcct,'-'),'0'))
*!*	    lcDistAcct = lcODistAcct
*!*	*B600691,1 M.H 12/03/95 When the user browse the account and return
*!*	*B600691,1 M.H 12/03/95 from the browse without sellect anything we
*!*	*B600691,1 M.H 12/03/95 have to remove the added line, enable the new
*!*	*B600691,1 M.H 12/03/95 remove buttons.
*!*	    llNew = .F.
*!*	    IF lnLineNo > 0
*!*	      SHOW GET pbRemove ENABLE
*!*	    ELSE
*!*	      SHOW GET pbRemove DISABLE
*!*	    ENDIF
*!*	    SHOW GET pbNew      ENABLE
*!*	    SHOW GET lnDstprct  ENABLE
*!*	    SHOW GET lcDistAcct DISABLE
*!*	    SHOW GET ibDistAcc  DISABLE
*!*	    SHOW GET lnDstPrct  DISABLE
*!*	    SHOW GET nAPDLinNo  DISABLE
*!*	    SHOW GET puTax      DISABLE
*!*	    SHOW GET ibTaxCode  DISABLE
*!*	*B600691,1 M.H 12/03/95
*!*	  ENDIF
*!*	ENDIF

lcTcode = loFormSet.ariaform1.lcTaxCode.Value
SELECT (loFormSet.lcTmpDt)
IF loFormSet.llNew = .T. .AND. EMPTY(lcTcode) .AND. !EMPTY(STRTRAN(STRTRAN(loFormSet.lcDistAcct,'-'),'0'))
  loFormSet.lnLineNo=loFormSet.lnLineNo+1
  INSERT INTO(loFormSet.lcTmpDt);
        (cAutMType, cAutMCode,  cAPDGlAct,  NAPDAMNT  ,CTAXCODE, cStatus , nAPDLinNo);
  VALUES(loFormSet.laData[1], loFormSet.laData[2],  loFormSet.lcDistAcct, loFormSet.lnDstPrct , lcTcode, 'A'     , loFormSet.lnLineNo)
  =gfAdd_Info(loFormSet.lcTmpDt)
  loFormSet.lnNoOfRec = RECCOUNT()  && Assign the no of records in the Temp file to a variable.
  IF loFormSet.lnNoOfRec = 0 .AND. !loFormSet.ActiveMode = 'S'
    *SHOW GET pbRemove DISABLE
    loFormSet.ariaform1.pbRemove.Enabled = .F.
  ELSE
    *SHOW GET pbRemove ENABLE
    loFormSet.ariaform1.pbRemove.Enabled = .T.
  ENDIF
  *SHOW GET lnDstprct  ENABLE
  loFormSet.ariaform1.lnDstprct.Enabled = .T.
  *SHOW GET rbMbase    DISABLE
  loFormSet.ariaform1.rbMbase.Enabled = .F.
  *SHOW GET pbNew      ENABLE
  loFormSet.ariaform1.pbNew.Enabled = .T.
ELSE
  IF !EMPTY(STRTRAN(STRTRAN(loFormSet.lcDistAcct,'-'),'0'))
    REPLACE CAPDGLACT WITH loFormSet.lcDistAcct
    IF CSTATUS <> "A"
      REPLACE CSTATUS WITH "M"
    ENDIF
  ENDIF
ENDIF

loFormSet.llNew = .F.
loFormset.Ariaform1.grdDist.AfterRowColChange()

*SHOW WINDOW (lcBrTtl) REFRESH

SELECT APINVAHD

*!*	llNew    = .F.
*!*	llBrowse = .F.
*!*	SHOW GET lcDistAcct
*!*	SHOW GET ibDistAcc
*!*	SHOW GET lnDstPrct

*!*	IF rbmbase = 1
*!*	  SHOW GET lcTcode
*!*	ENDIF

*=lfrefresh() llNew

*!**************************************************************************
*!
*!      Function : lfvData_2
*!
*!**************************************************************************
*
FUNCTION lfvData_2
PARAMETERS loFormSet,loFld

LOCAL lnSlct
lnSlct = SELECT(0)

SELECT APINVAHD
WITH loFld.Keytextbox
.Value     = ALLTRIM(.Value)
.Value     = IIF(ISDIGIT(LEFT(.Value,1)),;
                         PADL(.Value, FSIZE('cAutMCode','APINVAHD'),'0'),;
                         PADR(.Value,FSIZE('cAutMCode','APINVAHD')))
loFormSet.laData[2] = .Value
ENDWITH

*!*	SELECT APINVAHD
*!*	loFormSet.laData[2]     = ALLTRIM(loFormSet.laData[2])
*!*	loFormSet.laData[2]     = IIF(ISDIGIT(LEFT(loFormSet.laData[2],1)),;
*!*	                         PADL(loFormSet.laData[2], FSIZE('cAutMCode','APINVAHD'),'0'),;
*!*	                         PADR(loFormSet.laData[2],FSIZE('cAutMCode','APINVAHD')))

*SHOW GET laData[2]
lcFile_Ttl = loFormSet.lcFile_Ttl
lcBrFields = loFormSet.ariaBrFields.edtBrowseFields.Value
llView = .F.
lcBaseFile = loFormSet.lcBaseFile
llBrowse = loFld.Selectedfrombrowse
SELECT (lcBaseFile)

IF llBrowse .OR. !SEEK(loFormSet.CAUTMTYPE+loFld.KeyTextBox.VALUE) .OR. ATC("?",loFld.KeyTextBox.VALUE) > 0
  lnClosRec  = RECNO(0)
  IF BETWEEN(lnClosRec,1,RECCOUNT(lcBaseFile))
    GO lnClosRec
  ELSE
    GO TOP
  ENDIF
  IF llBrowse .OR. ATC("?",loFld.KeyTextBox.VALUE) > 0

    DIMENSION laTemp[1]
    laTemp = ''
    =gfBrows(' ','CAUTMCODE','laTemp',lcFile_Ttl)

    IF !EMPTY(laTemp[1])
      loFld.KeyTextBox.VALUE = ALLTRIM(laTemp[1])
      llView = .T.
    ELSE
      loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
    ENDIF
  ELSE
    lnOption  = gfModalGen('QRM00001B00001','Dialog',;
    loFormSet.lcCAUTMCODE_Ttl + " : " +ALLTRIM(loFld.KeyTextBox.VALUE))

    DO CASE
      CASE lnOption = 1
        DIMENSION laTemp[1]
        laTemp = ''
        =gfBrows(' ','CAUTMCODE','laTemp',lcFile_Ttl)

        IF !EMPTY(laTemp[1])
          loFld.KeyTextBox.VALUE = ALLTRIM(laTemp[1])
          llView = .T.
        ELSE
          loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
        ENDIF
        loFormSet.REFRESH()
      CASE lnOption = 2
        loFormSet.CHangeMode('A')
        RETURN

      CASE lnOption = 3
        loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
        RETURN .F.
    ENDCASE
  ENDIF
ELSE
  loFld.KeyTextBox.VALUE = &lcBaseFile..CAUTMCODE
  llView = .T.
ENDIF

loFormSet.laData[2] = loFld.KeyTextBox.VALUE

IF llView = .T.
  loFormSet.CHangeMode('V')
  loFormset.Ariaform1.rbmbase.Valid()
ENDIF

SELECT (lnSlct)


*=lfrefresh()

*!**************************************************************************
*!
*!      Function : lfObjDisp
*!
*!**************************************************************************
FUNCTION lfObjDisp
PARAMETER loFormSet,lcObjState
WITH loFormSet.Ariaform1
IF .rbmbase.Value = 1
  *SHOW GET ibTaxCode  &lcObjState
  *SHOW GET puTax      &lcObjState
  .lcTaxCode.Enabled = lcObjState = 'ENABLE'
ENDIF
*SHOW GET ibDistAcc  &lcObjState
*SHOW GET lcDistAcct &lcObjState
*SHOW GET lnDstPrct  &lcObjState
.lcDistAcct.Enabled = lcObjState = 'ENABLE'
.lnDstPrct.Enabled = lcObjState = 'ENABLE'
ENDWITH

*SHOW GET puTax

*!**************************************************************************
*!
*!      Function : lfvNew
*!
*!**************************************************************************
* Function to add new checks in the child screen.
*
FUNCTION lfvNew
PARAMETERS loFormSet

SELECT(loFormSet.lcTmpDt)
*APPEND BLANK

loFormSet.llNew = .T.

loFormSet.lcDistAcct = loFormSet.ap1.lcEmptyAcc
loFormSet.lnDstPrct  = 00.00
lcTcode    = " "
loFormSet.ariaform1.lcTaxCode.Value  = ' ' &&lcTBlnkCode
loFormSet.ariaform1.lcTaxCode.Valid()
loFormSet.ariaform1.Refresh()
*puTax      = lcTaxCode

SHOW GET pbRemove   DISABLE
SHOW GET pbNew      DISABLE

*=lfrefresh()

=lfObjDisp (loFormSet,"ENABLE")

IF loFormSet.Ariaform1.rbMbase.Value = 1
  *ACTIVATE WINDOW(lcReadNam2)
  *SHOW GET ibTaxCode ENABLE
  *SHOW GET puTax     ENABLE
  *<*_CUROBJ = OBJNUM(ibTaxCode)
  loFormSet.Ariaform1.lcTaxCode.Enabled = .T.
  loFormSet.Ariaform1.lcTaxCode.Setfocus()
ELSE
  *SHOW GET lcDistAcct ENABLE
  *SHOW GET ibDistAcc  ENABLE
  *<*_CUROBJ = OBJNUM(lcDistAcct)
  loFormSet.Ariaform1.lcDistAcct.Enabled = .T.
  loFormSet.Ariaform1.lcDistAcct.Setfocus()
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
PARAMETERS loFormSet

loFormSet.llNew = .F.

** MESSAGE : " Are you sure you want to remove it ? **
**           "     ®  Remove  ¯       <  Cancel >         **
IF gfModalGen("QRM00007B00007","ALERT") = 1

  SELECT(loFormSet.lcTmpDt)

  *B605162,1 MHM 12/20/2001 go to appropriate record [Start]
  LOCATE FOR NAPDLINNO = loFormSet.lnLineNo
  *B605162,1 MHM 12/20/2001 [End]
  *!B609980,1 SAB 06/27/2012 Fix error in refresh grid and in percentage field [Media R12 Issues]
  IF !FOUND()
    RETURN
  ENDIF
  *!B609980,1 SAB 06/27/2012 Fix error in refresh grid and in percentage field [Media R12 Issues]
  loFormSet.lnLineNo   = loFormSet.lnLineNo - 1
  REPLACE cStatus WITH SUBSTR("DSD",AT(cStatus,"MAS"),1)

  *** Delete the current record (to be removed )
  *** and update the line number field
  DELETE
  *** Check if you have to go to next record or the top one
  SKIP
  IF !EOF(loFormSet.lcTmpDt)
    lnRecNo = RECNO()
    REPLACE REST nAPDLinNo WITH nAPDLinNo - 1,;
                 cStatus  WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)
    GO lnRecNo
  ELSE
    SKIP -1
  ENDIF
  *SHOW WINDOW (lcBrTtl) REFRESH
  IF loFormSet.lnLineNo = 0
    *SHOW GET pbRemove DISABLE
    *SHOW GET Rbmbase  ENABLE
    loFormSet.ariaform1.pbRemove.Enabled = .F.
    loFormSet.ariaform1.rbmBase.Enabled = .T.
    ***Intialize Variables.
    loFormSet.lcDistAcct  = loFormSet.ap1.lcEmptyAcc
    loFormSet.lnDstPrct  = 00.00
    lcTcode    = " "
    *!B609980,1 SAB 06/27/2012 Fix error in refresh grid and in percentage field [Start]
    loFormSet.Ariaform1.txtLineNo.Value    = ''
    loFormSet.Ariaform1.txtglActName.Value = ''
    loFormSet.Ariaform1.lnDstPrct.Value  = 00.00
    loFormSet.Ariaform1.lcDistAcct.Keytextbox.Value = loFormSet.ap1.lcEmptyAcc
    loFormSet.ariaform1.grdDist.Refresh()
    *!B609980,1 SAB 06/27/2012 Fix error in refresh grid and in percentage field [End]
  ELSE
    *SHOW GET Rbmbase DISABLE
    loFormSet.ariaform1.rbmBase.Enabled = .F.
    ** If Add or Edit mode so we are going to enable the Remove Button.
    IF loFormSet.ActiveMode = 'E' .OR. loFormSet.ActiveMode = 'A'
      *SHOW GET pbRemove ENABLE
      loFormSet.ariaform1.pbRemove.Enabled = .T.
    ELSE
      ** If View mode so we are going to Disable the Remove Button.
      *SHOW GET pbRemove DISABLE
      loFormSet.ariaform1.pbRemove.Enabled = .F.
    ENDIF
    ***Restore the new record values.
*!*	    lcTmpDt = loFormSet.lcTmpDt
*!*	    lcTCode   = &lcTmpDt..CTAXCODE
*!*	    loFormSet.lcDistAcct= &lcTmpDt..CAPDGLACT
*!*	    loFormSet.lnDstPrct = &lcTmpDt..NAPDAMNT
    loFormSet.ariaform1.grdDist.AfterRowColChange()
  ENDIF

  *E300643,1 Change this line for the changes we have made
  *          to (gfCodDes) [Begin]
  *lcTaxCode = gfCodDes(cTaxCode)
  *lcTaxCode = gfCodDes(cTaxCode , 'CTAXCODE')
  *E300643,1 Change this line [End]

  *puTax     = lcTaxCode
  *=lfrefresh()
  =lfObjDisp (loFormSet,"DISABLE")

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
PARAMETERS loFormSet

LOCAL o
o = loFormSet.Ariaform1.rbmbase
loFormSet.laData[3]  = SUBSTR("LT",o.Value,1)

*IF Rbmbase  = 1
IF o.Value = 1

  *B607539,1 EIH 01/02/2006 Check if there is tax codes or not [Begin].
  **ACTIVATE WINDOW(lcReadNam2)
  *RELEASE WINDOW(lcBrTtl)
  *=lfDispBrow()
  lcCurAlias = ALIAS()
  SELECT CODES
  LOCATE
  *SKIP
  IF !EOF()
    *GOTO 1
    LOCATE
    *ACTIVATE WINDOW(lcReadNam2)
    *RELEASE WINDOW(lcBrTtl)
    *=lfDispBrow()
  ELSE
    *lcMsg = 'Ther is no tax code. please enter tax code first  '
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *=gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'There is no tax code. please enter tax code first')
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_NOTAXCODE,loformset.GetHeaderText("LANG_NOTAXCODE",loformset.HeaderAlias)))
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
    *GOTO 1
    LOCATE
    *Rbmbase  = 2
    o.Value = 2
    *SHOW GET rbmbase
  ENDIF
  SELECT (lcCurAlias)
  *B607539,1 EIH [End]

ELSE

  *RELEASE WINDOW(lcBrTtl)
  *=lfDispBrow()
  *ACTIVATE WINDOW(lcReadNam3)
  loFormSet.Ariaform1.lcTaxCode.Value = ' ' &&lcTBlnkCode

  IF loFormSet.ActiveMode = 'E'
    lcTCode   = " "
    SELECT (loFormSet.lcTmpDt)
    REPLACE CTAXCODE WITH lcTCode
    IF CSTATUS <> "A"
      REPLACE CSTATUS WITH "M"
    ENDIF
  ENDIF
  SELECT APINVAHD
ENDIF

loFormSet.Ariaform1.lcTaxCode.Visible = o.Value = 1
loFormSet.Ariaform1.lblTaxCode.Visible = o.Value = 1
loFormset.Ariaform1.grdDist.Column2.Width = IIF(o.Value = 1,80,0)

*=lfrefresh()

*!**************************************************************************
*!
*!      Function : lfwDstPrct
*!
*!**************************************************************************
*
FUNCTION lfwDstPrct
PARAMETERS loFormSet

loFormSet.lnODstPrct = loFormSet.lnDstPrct
loFormSet.lnTotPrct  = 0
SELECT(loFormSet.lcTmpDt)
lnTmpRcNo=RECNO()
SCAN
  loFormSet.lnTotPrct   =loFormSet.lnTotPrct+NAPDAMNT
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
PARAMETERS loFormSet,lnFld
*IF lnDstPrct = lnODstPrct
*!B609980,1 SAB 06/27/2012 Fix error in refresh grid and in percentage field [Start]
*IF lnFld.Value = lnFld.OldValue
IF !EMPTY(lnFld.Value) AND lnFld.Value = lnFld.OldValue
*!B609980,1 SAB 06/27/2012 Fix error in refresh grid and in percentage field [End]

  RETURN
ENDIF

lcTmpDt = loFormSet.lcTmpDt
IF &lcTmpDt..NAPDAMNT<>0
  loFormSet.lnNetTot=loFormSet.lnTotPrct+loFormSet.lnDstPrct-&lcTmpDt..NAPDAMNT
ELSE
  loFormSet.lnNetTot=loFormSet.lnTotPrct+loFormSet.lnDstPrct
ENDIF

IF loFormSet.lnNetTot > 100
  *loFormSet.lnDstPrct = loFormSet.lnODstPrct
  loFormSet.lnDstPrct = lnFld.OldValue

  ** MESSAGE : " The Total distributed percent cannot be greater than 100 " **
  **           "       <  Ok  >         **
  =gfModalGen("TRM04088B00000","DIALOG")
  RETURN .F.
ELSE
  IF loFormSet.lnDstPrct < 0
    *lnDstPrct = lnODstPrct
    loFormSet.lnDstPrct = lnFld.OldValue
    RETURN .F.
  ELSE
    SELECT (loFormSet.lcTmpDt)

    *B803555,1 Get the Record number to update the percentage [Begin]

    *B605162,1 MHM 12/20/2001 Get the right Record number to update the percentage [Start]
    *LOCATE FOR nApdLinNo = lnTmpRcNo
    LOCATE FOR nApdLinNo = loFormSet.lnLineNo
    *B605162,1 MHM 12/20/2001 [End]

    *B803555,1 Get the Record number to update the percentage [End]

    REPLACE NAPDAMNT WITH loFormSet.lnDstPrct
    IF CSTATUS <> "A"
      REPLACE CSTATUS WITH "M"
    ENDIF
    SELECT APINVAHD
  ENDIF
ENDIF

*SHOW WINDOW (lcBrTtl) REFRESH

*ACTIVATE WINDOW (lcReadNam5)

*SHOW GET pbNew    ENABLE
*SHOW GET pbRemove ENABLE
loFormSet.Ariaform1.pbNew.Enabled = .T.
loFormSet.Ariaform1.pbRemove.Enabled = .T.

*!**************************************************************************
*!
*!      Function : lfwTaxCode
*!
*!**************************************************************************
*
FUNCTION x_lfwTaxCode

lcOTaxCode = lcTaxCode
lcOTCode   = lcTCode
lcCodeFilt = "CTAXCODE  "

*!**************************************************************************
*!
*!      Function : lfvTaxCode
*!
*!**************************************************************************
* Get division code.
FUNCTION lfvTaxCode
PARAMETERS loFormSet,loFld
LOCAL lnSlct
lnSlct = SELECT(0)

IF EMPTY(loFld.Value)
  RETURN
ENDIF

puTax   = CODES.cdiscrep
lcTCode = CODES.cCode_No

SHOW GET puTax
*=gfUpdate()

DIMENSION laTaxCode[ALEN(loFormSet.laTaxCode,1),ALEN(loFormSet.laTaxCode,2)]
=ACOPY(loFormSet.laTaxCode,laTaxCode)
lnApsAcLen = LEN(loFormSet.ap1.lcEmptyAcc)

SELECT(loFormSet.lcTmpDt)
lcTcode = loFormSet.ariaform1.lcTaxCode.Value
IF !EMPTY(lcTcode)
  IF loFormSet.llNew = .T.  && IF NEW RECORD
    loFormSet.lnLineNo   = loFormSet.lnLineNo + 1
    =lfObjDisp (loFormSet,"ENABLE")
    **Function return related fields ->gl input account.
    lcDistAcct = loFormSet.lcDistAcct
    =gfRltFld(lcTcode , @laTaxCode , 'CTAXCODE')
    loFormSet.lcDistAcct = lcDistAcct

    loFormSet.lcDistAcct = SUBSTR(loFormSet.lcDistAcct,1,lnApsAcLen)
    IF APSETUP.CAPSGLLINK = 'Y'
      IF !EMPTY(LOOKUP(lcLinkChar.CACCNLDES,loFormSet.lcDistAcct,lcLinkChar.CACCTCODE,"ACCTCODE"))
        INSERT INTO(loFormSet.lcTmpDt);
           (cAutMType, cAutMCode, NAPDAMNT  ,CTAXCODE, cStatus , nAPDLinNo);
           VALUES(loFormSet.laData[1], loFormSet.laData[2], loFormSet.lnDstPrct , lcTcode, 'A'     , loFormSet.lnLineNo)
*!*	        SELECT (loFormSet.lcTmpDt)
*!*	        replace cAutMType WITH loFormSet.laData[1],;
*!*	                cAutMCode WITH  loFormSet.laData[2],;
*!*	                NAPDAMNT  WITH  loFormSet.lnDstPrct,;
*!*	                CTAXCODE  WITH  loFormSet.ariaform1.lcTaxCode.Value,;
*!*	                cStatus   WITH   'A'     ,;
*!*	                nAPDLinNo WITH  loFormSet.lnLineNo

           =gfAdd_Info(loFormSet.lcTmpDt)
        loFormSet.lnNoOfRec = RECCOUNT()  && Assign the no of records in the Temp file to a variable.
        IF loFormSet.lnNoOfRec = 0 .AND. !loFormSet.ActiveMode = 'S'
          *SHOW GET pbRemove DISABLE
          loFormSet.Ariaform1.pbRemove.Enabled = .F.
        ELSE
          SHOW GET pbRemove ENABLE
          SHOW GET rbmbase DISABLE
          loFormSet.Ariaform1.pbRemove.Enabled = .T.
          loFormSet.Ariaform1.rbmbase.Enabled = .F.
        ENDIF
        SELECT (loFormSet.lcTmpDt)
        REPLACE CAPDGLACT WITH loFormSet.lcDistAcct
        *ACTIVATE WINDOW(lcReadNam4)
        *SHOW GET lcDistAcct DISABLE
        loFormSet.Ariaform1.lcDistAcct.Enabled = .F.
        *SHOW GET ibDistAcc  DISABLE
        *<*_CUROBJ = OBJNUM(lnDstPrct)
      ELSE
        *<*_CUROBJ = OBJNUM(ibTaxCode)
        ** MESSAGE : " G/L input account # for tax code is not found " **
        **           " in the chart of accounts you can not select this tax code. " **
        **           "       <  Ok  >         **
        =gfModalGen("TRM04091B00000","DIALOG",ALLTRIM(loFormSet.lcDistAcct)+"|"+ALLTRIM(loFormSet.ariaform1.lcTaxCode.Value))
        lcTcode   = " "
        loFormSet.ariaform1.lcTaxCode.Value = ' ' && lcTBlnkCode
        loFormSet.lcDistAcct = loFormSet.ap1.lcEmptyAcc
        *puTax = lcTBlnkCode
        *=lfrefresh()
        loFormSet.lnLineNo   = loFormSet.lnLineNo - 1
        loFormSet.ariaform1.Refresh()
        RETURN
      ENDIF
    ELSE  && if new record and GL not linked.
      SELECT (loFormSet.lcTmpDt)
      INSERT INTO(loFormSet.lcTmpDt);
          (cAutMType, cAutMCode, NAPDAMNT  ,CTAXCODE, cStatus , nAPDLinNo);
          VALUES(loFormSet.laData[1], loFormSet.laData[2], loFormSet.lnDstPrct , lcTcode, 'A'     , loFormSet.lnLineNo)
          =gfAdd_Info(loFormSet.lcTmpDt)
      loFormSet.lnNoOfRec = RECCOUNT()  && Assign the no of records in the Temp file to a variable.
      SHOW GET pbNew ENABLE
      loFormSet.Ariaform1.pbNew.Enabled = .T.
      IF loFormSet.lnNoOfRec = 0 .AND. !loFormSet.ActiveMode = 'S'
        SHOW GET pbRemove DISABLE
        loFormSet.Ariaform1.pbRemove.Enabled = .F.
      ELSE
        SHOW GET pbRemove ENABLE
        SHOW GET rbmbase DISABLE
        loFormSet.Ariaform1.pbRemove.Enabled = .T.
        loFormSet.Ariaform1.rbmbase.Enabled = .F.
      ENDIF
      IF !EMPTY(STRTRAN(STRTRAN(loFormSet.lcDistAcct,'-'),'0'))
        REPLACE CAPDGLACT WITH loFormSet.lcDistAcct
      ENDIF
      *ACTIVATE WINDOW(lcReadNam4)
      *SHOW GET lcDistAcct DISABLE
      *SHOW GET ibDistAcc  DISABLE
      loFormSet.Ariaform1.lcDistAcct.Enabled = .F.

      *<*_CUROBJ = OBJNUM(lnDstPrct)
    ENDIF
    loFormSet.llNew = .F.
  ELSE && If not a new record.
    **Function return related fields ->gl input account.

    lcDistAcct = loFormSet.lcDistAcct
    =gfRltFld(lcTcode , @laTaxCode , 'CTAXCODE')
    loFormSet.lcDistAcct = lcDistAcct

    loFormSet.lcDistAcct = SUBSTR(loFormSet.lcDistAcct,1,lnApsAcLen)
    IF APSETUP.CAPSGLLINK = 'Y'
      IF !EMPTY(LOOKUP(lcLinkChar.CACCNLDES,loFormSet.lcDistAcct,lcLinkChar.CACCTCODE,"ACCTCODE"))
        SELECT (loFormSet.lcTmpDt)
        REPLACE CTAXCODE  WITH lcTcode
        REPLACE CAPDGLACT WITH loFormSet.lcDistAcct
        *SHOW GET ibTaxCode  ENABLE
        *SHOW GET puTax      ENABLE
        *ACTIVATE WINDOW(lcReadNam4)
        *SHOW GET lcDistAcct DISABLE
        loFormSet.Ariaform1.lcDistAcct.Enabled = .F.
        *SHOW GET ibDistAcc  DISABLE
        *<*_CUROBJ = OBJNUM(lnDstPrct)
      ELSE
        *<*_CUROBJ = OBJNUM(ibTaxCode)
        ** MESSAGE : " G/L input account # for tax code is not found " **
        **           " in the chart of accounts you can not select this tax code. " **
        **           "       <  Ok  >         **
        =gfModalGen("TRM04091B00000","DIALOG",ALLTRIM(loFormSet.lcDistAcct)+"|"+ALLTRIM(loFormSet.ariaform1.lcTaxCode.Value))
        lcTcode = " "
        SELECT (loFormSet.lcTmpDt)
        loFormSet.lcDistAcct = CAPDGLACT
        lcTaxCode  = lcOTaxCode
        puTax      = lcTBlnkCode
        *=lfrefresh()
        loFormSet.ariaform1.Refresh()
        RETURN
      ENDIF
    ELSE
      SELECT (loFormSet.lcTmpDt)
      REPLACE CTAXCODE  WITH lcTcode
      IF !EMPTY(STRTRAN(STRTRAN(loFormSet.lcDistAcct,'-'),'0'))
        REPLACE CAPDGLACT WITH loFormSet.lcDistAcct
      ENDIF
      *SHOW GET ibTaxCode  ENABLE
      *SHOW GET puTax      ENABLE
      *ACTIVATE WINDOW(lcReadNam4)
      *SHOW GET lcDistAcct DISABLE
      loFormSet.Ariaform1.lcDistAcct.Enabled = .F.
      *SHOW GET ibDistAcc    DISABLE
      *<*_CUROBJ = OBJNUM(lnDstPrct)
    ENDIF
  ENDIF
  IF CSTATUS <> "A"
    REPLACE CSTATUS WITH "M"
  ENDIF

  *ACTIVATE WINDOW(lcReadNam4)
  *<*_CUROBJ = OBJNUM(lcDistAcct)
ELSE && If empty of Tax Code
  IF !(loFormSet.llNew)
    REPLACE CTAXCODE  WITH lcTcode
  ENDIF
  *SHOW GET ibTaxCode  ENABLE
  *SHOW GET puTax      ENABLE
  *ACTIVATE WINDOW(lcReadNam4)
  *SHOW GET lcDistAcct ENABLE
  loFormSet.ariaform1.lcDistAcct.Enabled = .T.
  *SHOW GET ibDistAcc  ENABLE
  *<*_CUROBJ = OBJNUM(lcDistAcct)
ENDIF

SELECT APINVAHD
*SHOW WINDOW (lcBrTtl) REFRESH
*=lfrefresh()
*IF _WINDOWS
*  DEACTIVATE POPUP puTaxCode
*ENDIF
loFormSet.ariaform1.Refresh()
SELECT (lnSlct)
*- End of lfvTaxCode. return

************************************************************
*! Name      : lfFormBeforeSave
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/14/2012
*! Purpose   : FormBeforeSave
************************************************************
FUNCTION lfFormBeforeSave
PARAMETERS loFormSet
*EXTERNAL ARRAY laScrMode
SELECT APINVAHD
*B600691,1 M.H We cannot save the template without the distribution line.
*IF lnNoOfRec = 0
IF loFormSet.lnNoOfRec = 0 .OR. loFormSet.lnLineNo = 0
  ** MESSAGE : " You can not save the template that has no distribution lines" **
  **           "       <  Ok  >         **
  =gfModalGen("TRM04097B00000","DIALOG")
  llCSave = .F.
  RETURN llCSave
ENDIF

*- End of lfFormBeforeSave.

*!**************************************************************************
*!
*!      Procedure : lpSavScr
*!
*!**************************************************************************
* This procedure replaces the default Save procedure for the push button
* "SAVE"
PROCEDURE lpSavScr
PARAMETERS loFormSet

SELECT APINVAHD
IF loFormSet.ActiveMode = 'A'    && Add mode
  ** If we are in the add mode we are going to add a new blank record to
  ** add the new data.
  APPEND BLANK
ENDIF
lcScFields = loFormSet.lcScFields
GATHER FROM loFormSet.laData FIELDS &lcScFields MEMO && Add the new data to the record.

=gfAdd_Info('APINVAHD')

SELECT(loFormSet.lcTmpDt)

** If there is any records in the Temp file we are going to add or update
** the checks file.

IF RECCOUNT() > 0
  =gfTmp2Mast('APINVADT',loFormSet.lcTmpDt)  && Function to add or update the temp file.
ENDIF

SELECT APINVAHD
=gfTableUpdate()
SELECT APINVADT
=gfTableUpdate()

*- End of lpSavScr.

*!**************************************************************************
*!
*!      Procedure : lpDelScr
*!
*!**************************************************************************
* This procedure replaces the default Delete procedure for the push button
* "DELETE"
PROCEDURE lpDelScr
PARAMETERS loFormSet
*EXTERNAL ARRAY laScrMode

*** Delete details
SELECT APINVADT
** Make the deleted record blank and then delete.
SCATTER MEMVAR MEMO BLANK
lcInvADtEx = loFormSet.lcInvADtEx
SCAN FOR &lcInvADtEx = loFormSet.laData[1] + loFormSet.laData[2]
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
*laScrMode    = .F.
*loFormSet.ActiveMode = 'S'

SELECT APINVAHD
=gfTableUpdate()
SELECT APINVADT
=gfTableUpdate()

loFormSet.ChangeMode('S')
*- End of lpDelScr.

