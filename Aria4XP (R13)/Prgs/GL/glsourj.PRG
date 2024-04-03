*:************************************************************************
*:  Program File: ARIA4XP\PRGS\GL\GLSOURJ.prg
*:  Module      : General Ledger
*:  Desc.       : Source Journal screen
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 10/30/2012
*:  Reference   : *E303276,1
*:************************************************************************
*- Get the screen , call it
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
#INCLUDE R:\ARIA4XP\PRGS\gl\GLSOURJ.H
*N000682,1 MMT 11/22/2012 Globalization changes[END]
lcPrg = JUSTSTEM(SYS(16))
lcRunScx = lfGetScx("GL\&lcPrg..scx")
DO FORM (lcRunScx)

************************************************************
*! Name      : lfGetScx
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
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
*! Date      : 04/05/2012
*! Purpose   : called from the Screen init Method
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet

loFormSet.AddProperty('lcProgName',lcPrg)

*- Set functions to the APMAIN.FXP
lcPath = oAriaapplication.ApplicationHome
SET PROCEDURE TO (lcPath+'AP\APMAIN.FXP') ADDITIVE
SET PROCEDURE TO (lcPath+'GL\GL.FXP') ADDITIVE

*- Open tables
=lfOpenPRGFILES(loFormSet.lcProgName)

IF !lfGL(loFormset)
  RETURN .F.
ENDIF

*** Load program base file
=lfAddProp(loFormSet,'lcBaseFile',ALLTRIM(sydObjct.cBaseFile))

*- initializations
WITH loFormSet
  .cbrowsetabledbengine   = "NATIVE"
  .nWorkArea                            = .lcBaseFile
  .otoolbar.nWorkArea                   = .lcBaseFile
  .DataEnvironment.InitialSelectedAlias = .lcBaseFile
  .cBrowseFileName        = .lcBaseFile
  .cBrowseIndexExpression = "CSRCJRNL"
  .cBrowseIndexFields     = "CSRCJRNL"
  .cBrowseIndexName       = "SRCJRNL"
  .cBrowseAliasName       = .lcBaseFile
  .cBrowseTableName       = .lcBaseFile
  .cBrowseFilter          = ""
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	  .BrowseTitle 		  	  = 'Source Journal'

*!*	  .ariaBrFields.edtBrowseFields.Value = "CSRCJRNL :H='Source journal entry',"+;
*!*	                                        "CJORSHDES :H='Short description',"+;
*!*	                                        "CJORLNDES :H='Long description'"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.BrowseTitle 		  	  = LANG_SOURCEJOURAL
.BrowseTitle 		  	  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOURCEJOURAL,loFormSet.GetHeaderText("LANG_SOURCEJOURAL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]
  .ariaBrFields.edtBrowseFields.Value = "CSRCJRNL :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOURCEJOUNALENTRY,loFormSet.GetHeaderText("LANG_SOURCEJOUNALENTRY",loFormSet.HeaderAlias))+"',"+;
                                        "CJORSHDES :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SHORTDESC,loFormSet.GetHeaderText("LANG_SHORTDESC",loFormSet.HeaderAlias))+"',"+;
   "CJORLNDES :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_LONGDESC,loFormSet.GetHeaderText("LANG_LONGDESC",loFormSet.HeaderAlias)) +"'"
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[END]
ENDWITH

SELECT GLTYPES
GO TOP
IF EOF()
  *** The types and ranges have not ***
  *** been setup yet.  You have to  ***
  *** define the accounts type and ranges first. ***
  *** < Ok > ***
  =gfModalGen("TRM02038B00000","DIALOG")
  glQuitting  = .T.
  RETURN .F.
ENDIF

*** check if the chart of accounts is created.
SELECT GLACCHAR
LOCATE
IF EOF()
  *** The chart of accounts is empty. You have to create. ***
  *** the chart of accounts first...
  *** <  Ok  > ***
  =gfModalGen("TRM02215B00000","DIALOG")
  glQuitting = .T.
  RETURN .F.
ENDIF

*- Define needed variables.
=lfDefineVars(loFormSet)

loFormSet.ChangeMode('S')

************************************************************
*! Name      : lfFormActivate
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/07/2012
*! Purpose   : lfFormActivate method
************************************************************
FUNCTION lfFormActivate
PARAMETERS loFormSet

*- End of lfFormActivate

************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/23/2012
*! Purpose   : Define screen variables
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet

gcDataDir = oAriaApplication.DataDir
gcWorkDir = oAriaApplication.WorkDir


loFormSet.AddProperty('laKeyField [1,4]')
loFormSet.AddProperty('laVdEnt[20,2]')
loFormSet.AddProperty('laFromEnt[1,1]','')
loFormSet.AddProperty('laSource[1]','')
loFormSet.AddProperty('laTarget[1]','')


loFormSet.laKeyField[1,1] = 'laData[1]'
loFormSet.laKeyField[1,2] =.T.
loFormSet.laKeyField[1,3] = 'SRCJRNL'
loFormSet.laKeyField[1,4] = 1

llBrowse     = .F.

lcScFields = 'CSRCJRNL,CJORSHDES,CJORLNDES,MJORNOTES,MSJTRANS'
loFormSet.Addproperty('lcScFields',lcScFields)
lcCnt = ALLTRIM(STR(OCCURS(',',lcScFields)+1))
loFormSet.AddProperty('laData[&lcCnt.]')

SELECT (loformSet.lcbasefile)
SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData BLANK

*--      : Hard coded array elements we initially have
*-- Note : The following items are compared with Valid Entries and
*--      : the new items found in laFromEnt are added to laVdEnt.
*----------------------------------------------
STORE '' TO laFromEnt,laSource,laTarget
loFormSet.laVdEnt[1,2]  = "IN"
loFormSet.laVdEnt[2,2]  = "VI"
loFormSet.laVdEnt[3,2]  = "CR"
loFormSet.laVdEnt[4,2]  = "CA"
loFormSet.laVdEnt[5,2]  = "DA"
loFormSet.laVdEnt[6,2]  = "RM"
loFormSet.laVdEnt[7,2]  = "VR"
loFormSet.laVdEnt[8,2]  = "IP"
loFormSet.laVdEnt[9,2]  = "IA"
loFormSet.laVdEnt[10,2] = "PO"
loFormSet.laVdEnt[11,2] = "CT"
loFormSet.laVdEnt[12,2] = "ZE"
loFormSet.laVdEnt[13,2] = "MP"
loFormSet.laVdEnt[14,2] = "MA"
loFormSet.laVdEnt[15,2] = "MO"
loFormSet.laVdEnt[16,2] = "I"
loFormSet.laVdEnt[17,2] = "P"
loFormSet.laVdEnt[18,2] = "V"
loFormSet.laVdEnt[19,2] = "B"
loFormSet.laVdEnt[20,2] = "A"
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	loFormSet.laVdEnt[1,1]  = "A/R Invoice"
*!*	loFormSet.laVdEnt[2,1]  = "Void Invoice"
*!*	loFormSet.laVdEnt[3,1]  = "Cash Receipt"
*!*	loFormSet.laVdEnt[4,1]  = "Credit Adjustment"
*!*	loFormSet.laVdEnt[5,1]  = "Debit  Adjustment"
*!*	loFormSet.laVdEnt[6,1]  = "Return Merchandise"
*!*	loFormSet.laVdEnt[7,1]  = "Void Returns"
*!*	loFormSet.laVdEnt[8,1]  = "Inventory Physical"
*!*	loFormSet.laVdEnt[9,1]  = "Inventory Adjustment"
*!*	loFormSet.laVdEnt[10,1] = "P/O Receiving"
*!*	loFormSet.laVdEnt[11,1] = "C/T Receiving"
*!*	loFormSet.laVdEnt[12,1] = "Zero Out Stock"
*!*	loFormSet.laVdEnt[13,1] = "Material Inventory Physical  "
*!*	loFormSet.laVdEnt[14,1] = "Material Inventory Adjustment"
*!*	loFormSet.laVdEnt[15,1] = "Material P/O Receiving"
*!*	loFormSet.laVdEnt[16,1] = "Payable Invoices"
*!*	loFormSet.laVdEnt[17,1] = "Vendor Payments"
*!*	loFormSet.laVdEnt[18,1] = "Void Payments"
*!*	loFormSet.laVdEnt[19,1] = "Bank Adjustments"
*!*	loFormSet.laVdEnt[20,1] = "Apply Debit Memo"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laVdEnt[1,1]  = LANG_ARINVOICE
loFormSet.laVdEnt[1,1]  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARINVOICE,loFormSet.GetHeaderText("LANG_ARINVOICE",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laVdEnt[2,1]  = LANG_VOIDINV
loFormSet.laVdEnt[2,1]  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_VOIDINV,loFormSet.GetHeaderText("LANG_VOIDINV",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laVdEnt[3,1]  = LANG_CASHREC
loFormSet.laVdEnt[3,1]  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CASHREC,loFormSet.GetHeaderText("LANG_CASHREC",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laVdEnt[4,1]  = LANG_CREDITADJ
loFormSet.laVdEnt[4,1]  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CREDITADJ,loFormSet.GetHeaderText("LANG_CREDITADJ",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laVdEnt[5,1]  = LANG_DEBITADJU
loFormSet.laVdEnt[5,1]  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DEBITADJU,loFormSet.GetHeaderText("LANG_DEBITADJU",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laVdEnt[6,1]  = LANG_RETMER
loFormSet.laVdEnt[6,1]  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_RETMER,loFormSet.GetHeaderText("LANG_RETMER",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laVdEnt[7,1]  = LANG_VOIDRET
loFormSet.laVdEnt[7,1]  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_VOIDRET,loFormSet.GetHeaderText("LANG_VOIDRET",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laVdEnt[8,1]  = LANG_INVPHY
loFormSet.laVdEnt[8,1]  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVPHY,loFormSet.GetHeaderText("LANG_INVPHY",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laVdEnt[9,1]  = LANG_INVADJ
loFormSet.laVdEnt[9,1]  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVADJ,loFormSet.GetHeaderText("LANG_INVADJ",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laVdEnt[10,1] = LANG_POREC
loFormSet.laVdEnt[10,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POREC,loFormSet.GetHeaderText("LANG_POREC",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laVdEnt[11,1] = LANG_CTREC
loFormSet.laVdEnt[11,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CTREC,loFormSet.GetHeaderText("LANG_CTREC",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laVdEnt[12,1] = LANG_ZEROOUT
loFormSet.laVdEnt[12,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ZEROOUT,loFormSet.GetHeaderText("LANG_ZEROOUT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laVdEnt[13,1] = LANG_MAINVPHY
loFormSet.laVdEnt[13,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAINVPHY,loFormSet.GetHeaderText("LANG_MAINVPHY",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laVdEnt[14,1] = LANG_MAINVADJ
loFormSet.laVdEnt[14,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAINVADJ,loFormSet.GetHeaderText("LANG_MAINVADJ",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laVdEnt[15,1] = LANG_MAPOREC
loFormSet.laVdEnt[15,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPOREC,loFormSet.GetHeaderText("LANG_MAPOREC",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laVdEnt[16,1] = LANG_PAYABLEINV
loFormSet.laVdEnt[16,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PAYABLEINV,loFormSet.GetHeaderText("LANG_PAYABLEINV",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laVdEnt[17,1] = LANG_VENDORPAY
loFormSet.laVdEnt[17,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_VENDORPAY,loFormSet.GetHeaderText("LANG_VENDORPAY",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laVdEnt[18,1] = LANG_VOIDPAY
loFormSet.laVdEnt[18,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_VOIDPAY,loFormSet.GetHeaderText("LANG_VOIDPAY",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laVdEnt[19,1] = LANG_BANKADJ
loFormSet.laVdEnt[19,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BANKADJ,loFormSet.GetHeaderText("LANG_BANKADJ",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laVdEnt[20,1] = LANG_APPLYDEBITMEMO
loFormSet.laVdEnt[20,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APPLYDEBITMEMO,loFormSet.GetHeaderText("LANG_APPLYDEBITMEMO",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/22/2012 Globalization changes[END]
*-- Getting elements of array contains Valid Entries
=gfGetVld('MSJTRANS',@laFromEnt)

lcExact = SET("EXACT")
SET EXACT ON

*-- Check if valid entry array element not in hard coded array,
*-- we add it to hard coded array.
FOR lnI = 1 TO ALEN(laFromEnt,1)
  IF ASCAN(loFormSet.laVdEnt,ALLTRIM(laFromEnt[lnI,2])) = 0
    DIMENSION loFormSet.laVdEnt[ALEN(loFormSet.laVdEnt,1)+1,2]
    loFormSet.laVdEnt[ALEN(loFormSet.laVdEnt,1),1] = laFromEnt[lnI,1]
    loFormSet.laVdEnt[ALEN(loFormSet.laVdEnt,1),2] = laFromEnt[lnI,2]
  ENDIF
ENDFOR

SET EXACT &lcExact

WITH loFormSet.Ariaform1
  *- Set input mask
  .laData1.Keytextbox.MaxLength = FSIZE('CSRCJRNL','GLSUBJOR')
  .laData2.MaxLength = FSIZE('CJORSHDES','GLSUBJOR')
  .laData3.MaxLength = FSIZE('CJORLNDES','GLSUBJOR')
ENDWITH
*- End of lfDefineVars.

************************************************************
*! Name      : lfSetControlSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/05/2012
*! Purpose   : Set the Controls data Source
************************************************************
FUNCTION lfSetControlSource
PARAMETERS loFormSet
WITH loFormSet.Ariaform1
  *- set control Source
  .laData1.Keytextbox.ControlSource = 'Thisformset.laData[1]'
  .laData2.ControlSource = 'Thisformset.laData[2]'
  .laData3.ControlSource = 'Thisformset.laData[3]'
  .laData4.ControlSource = 'Thisformset.laData[4]'
ENDWITH

*- End of lfSetControlSource.


*!**************************************************************************
*!
*!      Procedure: lfChangeMode
*!
*!**************************************************************************
PROCEDURE lfChangeMode
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)

IF TYPE('loFormSet.lcProgName')='U'
  RETURN
ENDIF

lfSetControlSource(loFormSet)

SELECT (loFormSet.lcBaseFile)
lcScFields = loFormSet.lcScFields
IF loFormSet.ActiveMode $ 'VE'
  SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData
ENDIF

DO CASE
  CASE loFormSet.ActiveMode = 'S'
    SELECT (loFormSet.lcBaseFile)
    GO TOP

    SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData BLANK
    WITH loFormSet.Ariaform1
      .laData1.Enabled = .T.
      .laData1.Setfocus()
    ENDWITH

  CASE loFormSet.ActiveMode = 'V'
    loFormSet.otoolbar.cmddelete.Enabled = loFormSet.laData[1] <> loFormSet.lcSj_Def

    *-- I have to call lfLoadTran function that accomplish the
    *-- task of filling laSource and laTarget prepairing call to Mover.
    =lfLoadTran(loFormSet)

    WITH loFormSet.Ariaform1
      .laData1.Enabled = .F.
      .laData4.ReadOnly = .T.
    ENDWITH


  *- Doing the same prior action in both Edit and Add modes.
  CASE loFormSet.ActiveMode = 'E' OR loFormSet.ActiveMode = 'A'
    =lfLoadTran(loFormSet)

    WITH loFormSet.Ariaform1
      .laData1.Enabled = .F.
      .laData4.ReadOnly = .F.
    ENDWITH
ENDCASE

WITH loFormSet.Ariaform1
  .pbTrans.Enabled = loFormSet.ActiveMode <> 'S'
  .cmdZoom.Enabled = loFormSet.ActiveMode <> 'S'
  .laData4.Enabled = .T.
ENDWITH
loFormSet.Ariaform1.Refresh()
SELECT (lnSlct)
RETURN

*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
FUNCTION lfvData_1
PARAMETERS loFormSet,loFld
LOCAL lnSlct

lnSlct = SELECT(0)
IF loFld.Selectedfrombrowse .OR. !EMPTY(loFormSet.laData[1])
  IF ATC("?",loFormSet.laData[1]) > 0 .OR. loFld.Selectedfrombrowse
    IF loFormSet.oToolBar.cmdFind.Click()
      loFormSet.ChangeMode('V')
    ELSE
      loFormSet.laData[1] = ' '
      RETURN .F.
    ENDIF

  ELSE

    IF !SEEK(loFormSet.laData[1],'SYDAPPL')
      IF !SEEK(loFormSet.laData[1],loFormSet.lcBaseFile)
        **** \!\<Browse;\<Add;\?\<Reenter
        *N000682,1 MMT 11/22/2012 Globalization changes[Start]
        *lnResp = gfModalGen('INM00001B02004','DIALOG','Cash Flow Code:'+loFormSet.laData[1])
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lnResp = gfModalGen('INM00001B02004','DIALOG',LANG_CASHFLOW +loFormSet.laData[1])
lnResp = gfModalGen('INM00001B02004','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CASHFLOW,loFormSet.GetHeaderText("LANG_CASHFLOW",loFormSet.HeaderAlias)) +loFormSet.laData[1])
*N000682,1 11/20/2012 MMT Globlization changes[End]

        *N000682,1 MMT 11/22/2012 Globalization changes[END]
        DO case
        CASE lnResp = 1
          IF !loFormSet.oToolBar.cmdFind.Click()
            loFormSet.laData[1] = ' '
            RETURN .F.
          ENDIF
        CASE lnResp = 2
          loFormSet.ChangeMode('A')
        CASE lnResp = 3
          loFormSet.laData[1] = ' '
          RETURN .F.
        ENDCASE
      ELSE
        *- the key is there, go to view mode
        loFormSet.ChangeMode('V')
      ENDIF
    ELSE
      =gfModalGen("TRM02211B00000","Dialog",loFormSet.laData[1])
      STORE SPACE(2) TO loFormSet.laData[1]
      RETURN .F.
    ENDIF
  ENDIF
ENDIF
SELECT (lnSlct)


*!**************************************************************************
*!
*!      PROCEDURE: lpDelScr
*!
*!**************************************************************************
PROCEDURE lpDelScr
PARAMETERS loFormSet

IF loFormSet.laData[1] = loFormSet.lcSj_Def
  loFormSet.otoolbar.cmddelete.Enabled = .F.
  RETURN .F.
ENDIF

gcDataDir = oAriaApplication.DataDir
llUsed     = .F.
lnNOfFiles = 0

SELECT SYDFLFLD

* select the files that used this field from sydflfld into array
SELECT cFile_nam FROM SYDFLFLD WHERE cFld_name = "CSRCJRNL";
 AND cFile_nam <> "GLSUBJOR.DBF";
 GROUP BY cFile_nam INTO ARRAY laFiles

* store the number of array
lnNOfRec = _TALLY

* check if the record information used by another files
FOR lnCount = 1 TO lnNOfRec

  * store the alias of the file
 laFiles[lnCount] = ALLTRIM(laFiles[lnCount])

  IF NOT USED(laFiles[lnCount])
      SELECT 0
    USE gcDataDir+laFiles[lnCount]
    llUsed = .T.
  ENDIF

  SELECT (laFiles[lnCount])

  *  search for the record which match the record information
  LOCATE FOR CSRCJRNL = loFormSet.laData[1]

  IF FOUND()
    lnNOfFiles = lnNOfFiles + 1
    EXIT
  ENDIF
  IF llUsed
    USE IN laFiles[lnCount]
    llUsed = .F.
  ENDIF
ENDFOR

IF NOT USED("GLSUBJOR")
  USE &gcDataDir.GLSUBJOR IN SELECT(1)
ENDIF

SELECT GLSUBJOR

* check if there is any file share information
IF lnNOfFiles > 0

  SELECT SYDFILES
  SELECT CFILE_TTL FROM SYDFILES WHERE CFILE_NAM = (laFiles[lnCount]+".DBF");
  INTO ARRAY laPhiName
  IF NOT USED("GLSUBJOR")
    USE &gcDataDir.GLSUBJOR IN SELECT(1)
  ENDIF

  SELECT GLSUBJOR

  ****   Source journal ð is used by the ð file *******
  =gfModalGen("TRM02172B00000","DIALOG",loFormSet.laData[1]+"|"+ALLTRIM(laPhiName[1]))

  RETURN .F.
ELSE
  DELETE
  gfTableUpdate()
  loFormSet.ChangeMode('S')
ENDIF

************************************************************
*! Name      : lfFormBeforeSave
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/06/2012
*! Purpose   : Before save function
************************************************************
FUNCTION lfFormBeforeSave
PARAMETERS loFormSet

*- End of lfFormBeforeSave.


*!**************************************************************************
*!
*!      PROCEDURE: lpSavScr
*!
*!**************************************************************************
*
PROCEDURE lpSavScr
PARAMETERS loFormSet

** Convert the first letter into upper case
loFormSet.laData[2] = UPPER(SUBSTR(loFormSet.laData[2],1,1))+SUBSTR(loFormSet.laData[2],2)
loFormSet.laData[3] = UPPER(SUBSTR(loFormSet.laData[3],1,1))+SUBSTR(loFormSet.laData[3],2)
loFormSet.laData[4] = UPPER(SUBSTR(loFormSet.laData[4],1,1))+SUBSTR(loFormSet.laData[4],2)

IF loFormSet.ActiveMode = 'A'
  APPEND BLANK
ENDIF

lcScFields = loFormSet.lcScFields
GATHER FROM loFormSet.laData FIELDS &lcScFields MEMO
=gfAdd_Info(alias())
gfTableUpdate()

*!*************************************************************
*! Name      : lfLoadTran
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/22/97
*! Purpose   : Filling laSource and laTarget by desired items
*!           : prepairing call to Mover.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : gfSubStr
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfLoadTran()
*!*************************************************************
FUNCTION lfLoadTran
PARAMETERS loFormSet
*-- lcString   : Hold all data of the (memo field mSJTrans) except this I am in.
*-- laString   : Array holding the date of lcString but in items form.
*-- lcScanCond : Hold Scan condition.
*--            : All variables initially have spaces
PRIVATE lcString,lcScanCond
DIMENSION laString[1],laSource[1],laTarget[1]
STORE '' TO laString,laSource,laTarget,lcString

SELECT GLSUBJOR
lnRec = RECNO()

lcScanCond = IIF(SEEK(loFormSet.laData[1]),"FOR CSRCJRNL <> loFormSet.laData[1]","")

*-- Scan to have a string holds all items in other records.
SCAN &lcScanCond
  IF !EMPTY(ALLTRIM(msjTrans))
    lcString = IIF( EMPTY(ALLTRIM(lcString))         ,;
                    ALLTRIM(msjTrans)                ,;
                    lcString + "~" + ALLTRIM(msjTrans))
  ENDIF
ENDSCAN

*-- Returning cursor to its original position
IF BETWEEN(lnRec,1,RECCOUNT('GLSUBJOR'))
   GO lnRec
ENDIF

*-- If the string not empty concating it filling laString
*-- otherwise fill laSource with all description data in laVdEnt
IF !EMPTY(ALLTRIM(lcString))
  =gfSubStr(ALLTRIM(lcString),@laString,'~')
ELSE
  DIMENSION laSource[ALEN(loFormSet.laVdEnt,1)]
  FOR lnI = 1 TO ALEN(loFormSet.laVdEnt,1)
    laSource[lnI] = loFormSet.laVdEnt[lnI,1]
  ENDFOR
ENDIF

lcExact = SET("EXACT")
SET EXACT ON

*-- If laString have data fill laSource by the equavelent
*-- description data from loFormSet.laVdEnt.
IF !EMPTY(laString[1])
  FOR lnI = 1 TO ALEN(loFormSet.laVdEnt,1)
    IF ASCAN(laString,ALLTRIM(loFormSet.laVdEnt[lnI,2])) = 0
      IF !EMPTY(laSource[1])
        DIMENSION laSource[ALEN(laSource,1)+1]
      ENDIF
      laSource[ALEN(laSource,1)] = loFormSet.laVdEnt[lnI,1]
    ENDIF
  ENDFOR
ENDIF

IF !EMPTY(loFormSet.laData[5])
  *-- Filling laTarget by coded data from laData[5]
  =gfSubStr(ALLTRIM(loFormSet.laData[5]),@laTarget,'~')
ENDIF

*-- Filling laTarget by equavelent description data from loFormSet.laVdEnt,
*-- If we found data in it.
IF !EMPTY(laTarget[1])
  FOR lnI = 1 TO ALEN(laTarget,1)
    *-- We use the CEILING because loFormSet.laVdEnt is a two dimension array
    lnItem = CEILING(ASCAN(loFormSet.laVdEnt,ALLTRIM(laTarget[lnI]))/2)
    laTarget[lnI] = loFormSet.laVdEnt[lnItem,1]
  ENDFOR
ENDIF

SET EXACT &lcExact

*!*	DIMENSION loFormSet.laString[ALEN(laString)]
*!*	ACOPY(laString,loFormSet.laString)
DIMENSION loFormSet.laSource[ALEN(laSource)]
ACOPY(laSource,loFormSet.laSource)
DIMENSION loFormSet.laTarget[ALEN(laTarget)]
ACOPY(laTarget,loFormSet.laTarget)


*!*************************************************************
*! Name      : lfvTrans
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/22/97
*! Purpose   : Validating Selected Transactions after using Mover
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : gfMover
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvTrans()
*!*************************************************************
*E500152,1
*
FUNCTION lfvTrans
PARAMETERS loFormSet
*-- New Message
*-- If no transactions found say that
*-- "All transactions have been assigned to other source journals."
*--                          <  OK  >

*!*	DIMENSION laString[ALEN(loFormSet.laString)]
*!*	ACOPY(loFormSet.laString,laString)
DIMENSION laSource[ALEN(loFormSet.laSource)]
ACOPY(loFormSet.laSource,laSource)
DIMENSION laTarget[ALEN(loFormSet.laTarget)]
ACOPY(loFormSet.laTarget,laTarget)

IF EMPTY(laSource[1])
  =gfModalGen("TRM02248B02014","Dialog")
  RETURN
ENDIF
*--          : Descrip. # 1
*-- llCont   : A flag if it is False this means that we either choice
*--          : < Cancel >, Press ESCAPE or < OK >, Press ENTER but
*--          : does not change any thing in mover
*-- laTemp   : Array holding the Temporary date of laTarget, and after mover
*--          : it compaired with it, if it is not the same continue temporary
*--          : saving, else return because data does not change.
PRIVATE llCont
llCont = .F.
DIMENSION laTemp[ALEN(laTarget,1)]
=ACOPY(laTarget,laTemp)

llEdt = !loFormSet.ActiveMode = 'V'
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*=gfMover(@laSource,@laTarget,"Selected Transactions",llEdt,'',.F.,.F.,loFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfMover(@laSource,@laTarget,LANG_SELECTEDTRANS,llEdt,'',.F.,.F.,loFormSet)
=gfMover(@laSource,@laTarget,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SELECTEDTRANS,loFormSet.GetHeaderText("LANG_SELECTEDTRANS",loFormSet.HeaderAlias)),llEdt,'',.F.,.F.,loFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/22/2012 Globalization changes[END]
DIMENSION loFormSet.laSource[ALEN(laSource)]
ACOPY(laSource,loFormSet.laSource)
DIMENSION loFormSet.laTarget[ALEN(laTarget)]
ACOPY(laTarget,loFormSet.laTarget)

*-- START   : Descrip. # 1

IF EMPTY(laTarget[1]) AND EMPTY(laTemp[1])
  RETURN
ENDIF

IF ALEN(laTemp,1) = ALEN(laTarget,1)
  IF !EMPTY(laTarget[1])
    FOR lnI = 1 TO ALEN(laTarget,1)
      llCont = (ASCAN(laTemp,laTarget[lnI]) = 0)
      IF llCont
        EXIT
      ENDIF
    ENDFOR
  ELSE
    llCont = .T.
  ENDIF
ELSE
  llCont = .T.
ENDIF

IF !llCont
  RETURN
ENDIF
*-- END    : Descrip. # 1

*-- Saving equavelent code data in array if any in laData[5]
*-- separating it by ~ .
IF loFormSet.ActiveMode = 'E' OR loFormSet.ActiveMode = 'A'
  loFormSet.laData[5] = ''
  IF !EMPTY(laTarget[1])
    FOR lnI = 1 TO ALEN(laTarget,1)
      lnItem = CEILING(ASCAN(loFormSet.laVdEnt,ALLTRIM(laTarget[lnI]))/2)
      loFormSet.laData[5] = IIF( EMPTY(ALLTRIM(loFormSet.laData[5]))                 ,;
                       ALLTRIM(loFormSet.laVdEnt[lnItem,2])                ,;
                       ALLTRIM(loFormSet.laData[5]) + "~" + ALLTRIM(loFormSet.laVdEnt[lnItem,2]))
    ENDFOR
  ENDIF
ENDIF

