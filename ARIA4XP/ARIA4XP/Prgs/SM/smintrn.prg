*:************************************************************************
*:  Program File: \ARIA4XP\PRGS\SM\SMINTRN.PRG
*:  Module      : System Manager 
*:  Desc.       : Internationals
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 04/02/2014
*:  Reference   : E303456,1 
*:************************************************************************
#INCLUDE R:\ARIA4XP\PRGS\SM\SMINTRN.H


*- Get the screen , call it
lcRunScx = lfGetScx("SM\SMINTRN.scx")
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

loFormSet.AddProperty('lcProgName','SMINTRN')

*- Set functions to the APMAIN.FXP
lcPath = oAriaapplication.ApplicationHome
SET PROCEDURE TO (lcPath+'AP\APMAIN.FXP') ADDITIVE
SET PROCEDURE TO (lcPath+'GL\GL.FXP') ADDITIVE

*- Open tables
=lfOpenPRGFILES(loFormSet.lcProgName)

*** Load program base file
=lfAddProp(loFormSet,'lcBaseFile',ALLTRIM(sydObjct.cBaseFile))

WITH loFormSet
  .cbrowsetabledbengine   = "NATIVE"
  .nWorkArea                            = .lcBaseFile
  .otoolbar.nWorkArea                   = .lcBaseFile
  .DataEnvironment.InitialSelectedAlias = .lcBaseFile
  .cBrowseFileName        = .lcBaseFile
  .cBrowseIndexExpression = "CCONT_CODE"
  .cBrowseIndexFields     = "CCONT_CODE"
  .cBrowseIndexName       = "CCONTCODE"
  .cBrowseAliasName       = .lcBaseFile
  .cBrowseTableName       = .lcBaseFile
  .cBrowseFilter          = ""
  .BrowseTitle            = LANG_INTERNATIONALS
  .ariaBrFields.edtBrowseFields.Value = .GetBrowseFields(.lcBaseFile)
ENDWITH

=lfDefineVars(loFormSet)

loFormSet.ChangeMode('S')

=lfControlSource(loFormSet)
loFormSet.Ariaform1.Refresh()

RETURN 
*--End of lfFormInit.
************************************************************
*! Name      : lfControlSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/03/2014
*! Purpose   : *- Assign screen control sources
************************************************************
FUNCTION lfControlSource
PARAMETERS loFormSet

WITH loFormSet.Ariaform1

  .laData1.Keytextbox.ControlSource = 'Thisformset.laData[1]'
  .laData2.ControlSource = 'Thisformset.laData[2]'
  .laData27.ControlSource = 'Thisformset.laData[27]'
  .laData28.Keytextbox.ControlSource = 'Thisformset.laData[28]'

  .laData3.ControlSource = 'Thisformset.laData[3]'
  .laData4.ControlSource = 'Thisformset.laData[4]'
  .laData5.ControlSource = 'Thisformset.laData[5]'
  .laData6.ControlSource = 'Thisformset.laData[6]'
  .laData7.ControlSource = 'Thisformset.laData[7]'
  .laData8.ControlSource = 'Thisformset.laData[8]'
  .laData9.ControlSource = 'Thisformset.laData[9]'
  .laData10.ControlSource = 'Thisformset.laData[10]'
  .laData11.ControlSource = 'Thisformset.laData[11]'
  .laData12.ControlSource = 'Thisformset.laData[12]'
  .laData13.ControlSource = 'Thisformset.laData[13]'
  .laData14.ControlSource = 'Thisformset.laData[14]'
  .laData15.ControlSource = 'Thisformset.laData[15]'
  .laData16.ControlSource = 'Thisformset.laData[16]'
  .laData17.ControlSource = 'Thisformset.laData[17]'
  .laData18.ControlSource = 'Thisformset.laData[18]'
  .laData19.ControlSource = 'Thisformset.laData[19]'
  .laData20.ControlSource = 'Thisformset.laData[20]'
  
ENDWITH
*- End of lfControlSource.

************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/03/2014
*! Purpose   : Define Variables used in the screen's life
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet
LOCAL lcLn

loFormSet.AddProperty('laAdr[6,3]')
loFormSet.AddProperty('laYPos[3]')
loFormSet.AddProperty('laOrder[6,2]')
 
loFormSet.AddProperty('lcOrder'      , " ")
loFormSet.AddProperty('lcLabelOrd'   , " ")
loFormSet.AddProperty('ibOrder'      , 1)
loFormSet.AddProperty('llBrowse'     , .F.)

lcScFields = "CCONT_CODE,CCONT_DESC,CPART1LAB,CPART2LAB,CPART3LAB,CPART4LAB,CPART5LAB,CPART6LAB,NPART1LEN,NPART2LEN,NPART3LEN,"+;
             "NPART4LEN,NPART5LEN,NPART6LEN,NPART1ORD,NPART2ORD,NPART3ORD,NPART4ORD,NPART5ORD,NPART6ORD,CCURRENCY,CCURRENCYI,"+;
             "CDATE_TYPE,CCENTURY,CSEPARATOR,CPHONETEMP,LEUROPCOM,CCURRCODE"
loFormSet.AddProperty('lcScFields'    , lcScFields)
lcLn = ALLTRIM(STR(OCCURS(",",lcScFields)+1))

DIMENSION laData[&lcLn]
SELECT (loFormSet.lcBaseFile)
SCATTER FIELDS &lcScFields MEMO TO laData BLANK
loFormSet.AddProperty('laData[&lcLn]')
=ACOPY(laData,loFormSet.laData)
  
SELECT SYDFIELD
FOR lnCount = 1 TO 6
  loFormSet.laAdr[lnCount,1] = lnCount
  loFormSet.laAdr[lnCount,2] = "CADDRESS"+ALLTRIM(STR(lnCount))
  IF SEEK(loFormSet.laAdr[lnCount,2])
    loFormSet.laAdr[lnCount,3] = nfld_wdth
  ELSE
    loFormSet.laAdr[lnCount,3] = 1
  ENDIF
ENDFOR
loFormSet.laData[3]  = "Part 1"
loFormSet.laData[4]  = "Part 2"
loFormSet.laData[5]  = "Part 3"
loFormSet.laData[6]  = "Part 4"
loFormSet.laData[7]  = "Part 5"
loFormSet.laData[8]  = "Part 6"

*** Put the order values in the array. ***  
loFormSet.lcOrder      = loFormSet.laOrder[1,1]
loFormSet.lcLabelOrd   = loFormSet.laOrder[1,2]

SELECT SYCINT
*- End of lfDefineVars.

************************************************************
*! Name      : lfChangeMode
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/03/2014
*! Purpose   : change mode function
************************************************************  
FUNCTION lfChangeMode
PARAMETERS loFormSet
LOCAL lnI

IF TYPE('loFormSet.lcProgName')='U'
  RETURN
ENDIF

DO CASE
  CASE loFormSet.ActiveMode = 'S'  && Select Mode
    loFormSet.laData[3]  = LANG_Part_1
    loFormSet.laData[4]  = LANG_Part_2
    loFormSet.laData[5]  = LANG_Part_3
    loFormSet.laData[6]  = LANG_Part_4
    loFormSet.laData[7]  = LANG_Part_5
    loFormSet.laData[8]  = LANG_Part_6
    
    ** Put defaults in the popup hold the parts objects. **
    loFormSet.lcOrder    = loFormSet.laOrder[1,1]
    loFormSet.lcLabelOrd = loFormSet.laOrder[1,2]
    
    loFormset.Ariaform1.laData1.Enabled = .T.
    loFormset.Ariaform1.laData1.Keytextbox.Setfocus()
    
  CASE loFormSet.ActiveMode = 'V' && View mode.
  
    lnOrder    = IIF(ASCAN(loFormSet.laOrder,loFormSet.lcLabelOrd) > 0,;
                     IIF(ASUBSCRIPT(loFormSet.laOrder,ASCAN(loFormSet.laOrder,loFormSet.lcLabelOrd),1) > 0,;
                     ASUBSCRIPT(loFormSet.laOrder,ASCAN(loFormSet.laOrder,loFormSet.lcLabelOrd),1),1),1)
    loFormSet.ibOrder = lnOrder
    
    *- Update screen fields
    SELECT sycInt
    lcScFields = loFormSet.lcScFields
    SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData
    loFormset.Ariaform1.Refresh()

    *- Refresh disp fields
    =lfRefreshFields(loFormSet)
    loFormset.Ariaform1.cmdLocals.Enabled = .T.
    
    
  CASE loFormSet.ActiveMode = 'E' && Edit mode.
    loFormset.Ariaform1.laData1.Enabled = .F.
    *--   
  
  CASE loFormSet.ActiveMode = 'A' && Add mode.
    ** Put defaults in the popup hold the parts objects. **
    loFormSet.laData[9]   = 30
    loFormSet.laData[10]  = 30     
    loFormSet.laData[11]  = 15   
    loFormSet.laData[12]  = 3   
    loFormSet.laData[13]  = 10   
    loFormSet.laData[14]  = 20   
    
    loFormSet.laData[15]  = 1   
    loFormSet.laData[16]  = 2   
    loFormSet.laData[17]  = 3   
    loFormSet.laData[18]  = 3   
    loFormSet.laData[19]  = 3   
    loFormSet.laData[20]  = 4
        
    loFormSet.lcOrder    = loFormSet.laOrder[1,1]
    loFormSet.lcLabelOrd = loFormSet.laOrder[1,2]
    
    ** Put default for locals screen.
    loFormSet.laData[28]='USD'
    loFormSet.laData[22]=LOOKUP(SYCCURR.CCURRSMBL,loFormSet.laData[28],syccurr.ccurrcode,'Ccurrcode')     
    loFormSet.laData[21]=IIF(!EMPTY(loFormSet.laData[21]),ALLTRIM(loFormSet.laData[21]),SET("CURRENCY"))
    loFormSet.laData[22]=IIF(!EMPTY(loFormSet.laData[22]),ALLTRIM(loFormSet.laData[22]),SET("CURRENCY",2))
    loFormSet.laData[23]=IIF(!EMPTY(loFormSet.laData[23]),ALLTRIM(loFormSet.laData[23]),SET("DATE"))
    loFormSet.laData[24]=IIF(!EMPTY(loFormSet.laData[24]),ALLTRIM(loFormSet.laData[24]),SET("CENTURY"))
    loFormSet.laData[25]=IIF(!EMPTY(loFormSet.laData[25]),ALLTRIM(loFormSet.laData[25]),SET("SEPARATOR"))
    loFormSet.laData[26]=IIF(!EMPTY(loFormSet.laData[26]),ALLTRIM(loFormSet.laData[26]),oAriaApplication.PhoneMask)
    
    loFormSet.Ariaform1.laData2.Value = ''
    =lfRefreshFields(loFormSet)

    loFormset.Ariaform1.laData1.Refresh()
    loFormset.Ariaform1.laData1.Enabled = .F.
ENDCASE

loFormSet.Ariaform1.Refresh()


*- End of lfChangeMode.

************************************************************
*! Name      : lfRefreshFields
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/03/2014
*! Purpose   : lfRefreshFields
************************************************************
FUNCTION lfRefreshFields
PARAMETERS loFormSet
LOCAL lnI,lcI
FOR lnI = 1 TO 6
  lcI = STR(lnI,1)
  loFormSet.Ariaform1.txtPart&lcI..Value = lfDispLine(loFormSet,lnI)
ENDFOR 
*- End of lfRefreshFields.


*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
FUNCTION lfvData_1
PARAMETERS loFormSet,loFld
LOCAL lnSlct

lnSlct = SELECT(0)

WITH loFld.Keytextbox
.Value     = ALLTRIM(.Value)
.Value     = PADR(.Value,LEN(.InputMask))
ENDWITH
lcFile_Ttl = loFormSet.BrowseTitle
lcBrFields = loFormSet.ariaBrFields.edtBrowseFields.Value
llView = .F.
lcBaseFile = loFormSet.lcBaseFile
llBrowse = loFld.Selectedfrombrowse

SELECT (lcBaseFile)
IF llBrowse .OR. (!EMPTY(loFld.KeyTextBox.VALUE) AND !gfSEEK(loFld.KeyTextBox.VALUE)) .OR. ATC("?",loFld.KeyTextBox.VALUE) > 0
  IF llBrowse .OR. ATC("?",loFld.KeyTextBox.VALUE) > 0
    IF loFormSet.oToolBar.cmdFind.Click()
      llView = .T.
    ELSE
      loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
    ENDIF
  ELSE
    lnOption  = gfModalGen('QRM00001B00001','Dialog',;
                   +ALLTRIM(loFld.KeyTextBox.VALUE))

    DO CASE
      CASE lnOption = 1
        IF loFormSet.oToolBar.cmdFind.Click()
          llView = .T.
        ELSE
          loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
        ENDIF
      CASE lnOption = 2
        loFormSet.laData[1] = loFld.KeyTextBox.VALUE
        loFormSet.ChangeMode('A')
        RETURN

      CASE lnOption = 3
        loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
        RETURN .F.
    ENDCASE
  ENDIF
ELSE
  loFld.KeyTextBox.VALUE = &lcBaseFile..CCONT_CODE
  llView = .T.
ENDIF

IF llView = .T.
  loFormSet.CHangeMode('V')
  loFormSet.oToolBar.Navrefresh()
ENDIF

SELECT (lnSlct)
*-- End of lfvData_1.
************************************************************
*! Name      : lfvRange
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/03/2014
*! Purpose   : Range should be 1 to 30
************************************************************
FUNCTION lfvRange
PARAMETERS loFormSet,loFld
LOCAL lcMsg,lnLow,lnMax 
  lnLow = 1
  lnMax = 5

DO CASE 
CASE loFld.InputMask == '99'
  lcMsg =  LANG_RANGE_IS_1_30
  lnLow = 1
  lnMax = 30
CASE loFld.InputMask == '9'
  lcMsg =  LANG_RANGE_IS_1_5
  lnLow = 1
  lnMax = 5
ENDCASE 

IF !BETWEEN(loFld.Value,lnLow,lnMax)  
  WAIT WINDOW NOWAIT lcMsg
  RETURN .F.
ENDIF 
RETURN .T.

*- End of lfvRange.

*!********************************************************************
*!
*!              Function: lfvLabel
*!
*!********************************************************************
FUNCTION lfvLabel

IF ASCAN(loFormSet.laOrder,loFormSet.lcOrder) > 0
  lnOrder = IIF(ASUBSCRIPT(loFormSet.laOrder,ASCAN(loFormSet.laOrder,loFormSet.lcOrder),1) > 0,;
                ASUBSCRIPT(loFormSet.laOrder,ASCAN(loFormSet.laOrder,loFormSet.lcOrder),1),1)
ELSE
  lnOrder = 1
ENDIF

loFormSet.ibOrder = lnOrder

*!********************************************************************
*!
*!              Function: lfvOrder
*!
*!********************************************************************
FUNCTION lfvOrder

loFormSet.lcLabelOrd = loFormSet.laOrder[loFormSet.ibOrder,2]

*!********************************************************************
*!
*!              Function: lfvLocals
*!
*!********************************************************************
FUNCTION lfvLocals
PARAMETERS loFormSet

*- Get the screen , call it
lcRunScx = lfGetScx("SM\SMLOCALS.scx") 
DO FORM (lcRunScx) WITH loFormSet,"U"

************************************************************
*! Name      : lfBeforeSave
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/03/2014
*! Purpose   : lfBeforeSave
************************************************************
FUNCTION lfBeforeSave
PARAMETERS loFormSet

IF EMPTY(loFormSet.laData[2])
  *** You cannot leave the country description empty. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00196B00000","DIALOG")
  loFormset.Ariaform1.laData2.SetFocus()
  RETURN .F.
ENDIF
*- End of lfBeforeSave.

*!**************************************************************************
*!
*!      Function: lpSavScr
*!
*!**************************************************************************
FUNCTION lpSavScr
PARAMETERS loFormSet

SELECT sycInt
IF loFormSet.ActiveMode='A'
  APPEND BLANK
ENDIF
lcScFields = loFormSet.lcScFields
GATHER FROM loFormSet.laData FIELDS &lcScFields MEMO
=gfAdd_Info(loFormSet.lcBaseFile)

IF loFormSet.ActiveMode='E'
  =gfOpenFile(oAriaApplication.SysPath+'SYCCOMP','CCOMP_ID')
  IF SEEK(oAriaApplication.ActiveCompanyID)
    IF SYCCOMP.CCONT_CODE = loFormSet.laData[1]
      IF !EMPTY(loFormSet.laData[24]) AND !EMPTY(loFormSet.laData[21]) AND !EMPTY(loFormSet.laData[23])
        SET CENTURY  (loFormSet.laData[24])
        lcSetCurr = loFormSet.laData[21]
        SET CURRENCY &lcSetCurr
        SET DATE TO ALLTRIM(loFormSet.laData[23])
      ELSE
        SET CENTURY OFF
        SET CURRENCY LEFT
        SET CURRENCY TO   
        SET DATE TO AMERICAN
      ENDIF  
      SET CURRENCY TO IIF(!EMPTY(loFormSet.laData[22]),loFormSet.laData[22],"")
      SET SEPARATOR TO IIF(!EMPTY(loFormSet.laData[25]),loFormSet.laData[25],',')
      oAriaApplication.PhoneMask  = IIF(!EMPTY(loFormSet.laData[26]),loFormSet.laData[26],oAriaApplication.PhoneMask)
      *gnPhonSize = LEN(ALLTRIM(loFormSet.laData[26]))
    ENDIF
  ENDIF
ENDIF
SELECT sycInt
*- End of lpSavScr.

*!********************************************************************
*!
*!              Procedure: lpDelScr
*!
*!********************************************************************
PROCEDURE lpDelScr
PARAMETERS loFormSet

=gfOpenFile(oAriaApplication.SysPath+'SYCCOMP','CCOMP_ID')

LOCATE FOR ccont_code=loFormSet.laData[1]
IF FOUND() OR loFormSet.laData[1] = 'USA' OR loFormSet.laData[1] = 'ENG'
  ** 'You cannot delete this record' message
  *LANG_MSG1 'This record has been defined by the system.'
  *LANG_MSG2 'This Code is currently used by one or more company(s).'
  =gfModalGen('INM00242B00000','ALERT',IIF(loFormSet.laData[1]='USA' OR loFormSet.laData[1] = 'ENG',LANG_MSG1,LANG_MSG2))
    SELECT sycInt
    RETURN .F.
ELSE
  lcSavAlias=SELECT(0)
  *** Delete the international record from the sycInt file
  SELECT sycInt
  lcScFields = loFormSet.lcScFields
  SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData BLANK
  GATHER FROM loFormSet.laData FIELDS &lcScFields MEMO 
  DELETE
  SELECT(lcSavAlias)
  loFormSet.ChangeMode("S")
ENDIF  

SELECT SYCINT
*- End of lpDelScr.

*!********************************************************************
*!
*!              Function: lfFillOrd
*!
*!********************************************************************
FUNCTION lfFillOrd

loFormSet.laOrder[1,1] = IIF(EMPTY(loFormSet.laData[5]),"Part 3",ALLTRIM(loFormSet.laData[5]))+","+;
               IIF(EMPTY(loFormSet.laData[6]),"Part 4",ALLTRIM(loFormSet.laData[6]))+","+;
               IIF(EMPTY(loFormSet.laData[7]),"Part 5",ALLTRIM(loFormSet.laData[7]))
loFormSet.laOrder[1,2] = "345"

loFormSet.laOrder[2,1] = IIF(EMPTY(loFormSet.laData[5]),"Part 3",ALLTRIM(loFormSet.laData[5]))+","+;
               IIF(EMPTY(loFormSet.laData[7]),"Part 5",ALLTRIM(loFormSet.laData[7]))+","+;
               IIF(EMPTY(loFormSet.laData[6]),"Part 4",ALLTRIM(loFormSet.laData[6]))
loFormSet.laOrder[2,2] = "354"

loFormSet.laOrder[3,1] = IIF(EMPTY(loFormSet.laData[6]),"Part 4",ALLTRIM(loFormSet.laData[6]))+","+;
               IIF(EMPTY(loFormSet.laData[5]),"Part 3",ALLTRIM(loFormSet.laData[5]))+","+;
               IIF(EMPTY(loFormSet.laData[7]),"Part 5",ALLTRIM(loFormSet.laData[7]))
loFormSet.laOrder[3,2] = "435"

loFormSet.laOrder[4,1] = IIF(EMPTY(loFormSet.laData[6]),"Part 4",ALLTRIM(loFormSet.laData[6]))+","+;
               IIF(EMPTY(loFormSet.laData[7]),"Part 5",ALLTRIM(loFormSet.laData[7]))+","+;
               IIF(EMPTY(loFormSet.laData[5]),"Part 3",ALLTRIM(loFormSet.laData[5]))
loFormSet.laOrder[4,2] = "534"

loFormSet.laOrder[5,1] = IIF(EMPTY(loFormSet.laData[7]),"Part 5",ALLTRIM(loFormSet.laData[7]))+","+;
               IIF(EMPTY(loFormSet.laData[6]),"Part 4",ALLTRIM(loFormSet.laData[6]))+","+;
               IIF(EMPTY(loFormSet.laData[5]),"Part 3",ALLTRIM(loFormSet.laData[5]))
loFormSet.laOrder[5,2] = "543"

loFormSet.laOrder[6,1] = IIF(EMPTY(loFormSet.laData[7]),"Part 5",ALLTRIM(loFormSet.laData[7]))+","+;
               IIF(EMPTY(loFormSet.laData[5]),"Part 3",ALLTRIM(loFormSet.laData[5]))+","+;
               IIF(EMPTY(loFormSet.laData[6]),"Part 4",ALLTRIM(loFormSet.laData[6]))
loFormSet.laOrder[6,2] = "453"

*!**************************************************************************
*!
*!      Function: lfDispLine
*!
*!**************************************************************************
FUNCTION lfDispLine
PARAMETERS loFormSet,lnLineNo
LOCAL lnCount
lcRetVal=''
IF loFormSet.ActiveMode <> 'S'
  FOR lnCount = 15 TO 20
    IF loFormSet.laData[lnCount] = lnLineNo
      IF !EMPTY(loFormSet.laData[lnCount-12])
        lcRetVal=lcRetVal+IIF(EMPTY(lcRetVal),'',',')+PADR(SUBSTR(loFormSet.laData[lnCount-12],1,loFormSet.laData[lnCount-6]),loFormSet.laData[lnCount-6],"*")
      ELSE  
        lcRetVal=lcRetVal+IIF(EMPTY(lcRetVal),'',',')+PADR(SUBSTR('Part'+STR(lnCount-14,1),1,loFormSet.laData[lnCount-6]),loFormSet.laData[lnCount-6],"*")      
      ENDIF  
    ENDIF
  ENDFOR
ENDIF  
RETURN lcRetVal

*!**************************************************************************
*!
*!      Function: lfvCurrency
*!
*!**************************************************************************
FUNCTION lfvCurrency
PARAMETERS loFormSet, loFld

llBrowse = loFld.Selectedfrombrowse

IF (!EMPTY(loFormSet.laData[28]) AND !SEEK(loFormSet.laData[28],'SYCCURR')) OR llBrowse 
  PRIVATE lcBrFields,lcFile_ttl,lcSelect
  lcSelect = SELECT()
  SELECT SYCCURR
  lcBrFields=gfDbfField('SYCCURR')
  DIMENSION laData1[1]
  STORE '' TO laData1
  lcFile_ttl    = "Currency"
  =gfBrows(.F.,"cCurrCode","laData1")
  loFormSet.laData[28]=laData1[1]
  SELECT (lcSelect)
ENDIF
IF EMPTY(loFormSet.laData[28])
  loFormSet.laData[28] = loFld.Keytextbox.OldValue
ENDIF
IF loFormSet.laData[28] <> loFld.Keytextbox.OldValue
  loFormSet.laData[22]=LOOKUP(SYCCURR.CCURRSMBL,loFormSet.laData[28],syccurr.ccurrcode,'Ccurrcode') 
ENDIF    
llBrowse = .F.
loFormSet.Ariaform1.Refresh()
*=lfRefresh(WOUTPUT())


************************************************************
*! Name      : lfSMLOCALSInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/03/2014
*! Purpose   : lfSMLOCALSInit
************************************************************
FUNCTION lfSMLOCALSInit
PARAMETERS loBranchFormSet
LOCAL llEdit
loFormSet = loBranchFormSet.loFormSet
llEdit = loFormSet.ActiveMode $ 'AE'
WITH loBranchFormSet.AriaForm1
  .cmdCancel.Enabled = llEdit   
  .rbCurr.Enabled = llEdit 
  .laData22.Enabled = llEdit 
  .puDateTyp.Enabled = llEdit 
  .lcDateTemp.Enabled = llEdit 
  .lcDateSep.Enabled = llEdit 
  .pbCent.Enabled = llEdit 
  .laData25.Enabled = llEdit 
  .laData26.Enabled = llEdit 
ENDWITH 

WITH loBranchFormSet.AriaForm1.puDateTyp
  .AddItem('AMERICAN')
  .AddItem('ITALIAN')
  .AddItem('GERMAN')
  .AddItem('BRITISH')
  .AddItem('JAPAN')
ENDWITH 

*- Set control Source of the screen controls
WITH loBranchFormSet.AriaForm1
  *.rbCurr.ControlSource = 'Thisformset.loFormset.laData[21]
  .rbCurr.Value = IIF(SYCINT.CCURRENCY = 'LEFT',1,2)
  .laData22.ControlSource = 'Thisformset.loFormset.laData[22]'
  .puDateTyp.ControlSource = 'Thisformset.loFormset.laData[23]'
   .laData25.ControlSource = 'Thisformset.loFormset.laData[25]'
  .laData26.ControlSource = 'Thisformset.loFormset.laData[26]'
  .Refresh()
ENDWITH 

loBranchFormSet.Addproperty('laSavSet[6]','')
lcLn = ALLTRIM(STR(ALEN(loFormSet.laData)))
loBranchFormSet.Addproperty('laSavData[&lcLn]')

DO lpSavSet

=ACOPY(loFormSet.laData,loBranchFormSet.laSavData)

loBranchFormSet.AriaForm1.lcCurrTemp.Value = TRANSFORM(123456.7890,'@$999999.9999')


loBranchFormSet.Ariaform1.lcDateSep.Value  = SUBSTR(DTOC(oAriaApplication.SystemDate),IIF(SET("DATE")="JAPAN",;
                 IIF(SET("CENTURY")="ON",5,3),3),1)

loFormSet.laData[21] = IIF(!EMPTY(loFormSet.laData[21]),ALLTRIM(loFormSet.laData[21]),SET("CURRENCY"))
loFormSet.laData[22] = IIF(!EMPTY(loFormSet.laData[22]),ALLTRIM(loFormSet.laData[22]),SET("CURRENCY",2))
loFormSet.laData[23] = IIF(!EMPTY(loFormSet.laData[23]),ALLTRIM(loFormSet.laData[23]),SET("DATE"))
loFormSet.laData[24] = IIF(!EMPTY(loFormSet.laData[24]),ALLTRIM(loFormSet.laData[24]),SET("CENTURY"))
loFormSet.laData[25] = IIF(!EMPTY(loFormSet.laData[25]),ALLTRIM(loFormSet.laData[25]),SET("SEPARATOR"))
loFormSet.laData[26] = IIF(!EMPTY(loFormSet.laData[26]),ALLTRIM(loFormSet.laData[26]),oAriaApplication.PhoneMask)

lcCent     = IIF(SET("CENTURY")="ON",LANG_CENT_ON ,LANG_CENT_OFF)
loBranchFormSet.AriaForm1.pbCent.Caption = lcCent

=lfModiSet()

IF loBranchFormSet.lcScrMode<>"C" 
  DO lpRstorSet
ELSE
  *=gpDispStat()  
ENDIF

loBranchFormSet.AddProperty('Decimals',SET("Decimals"))
SET DECIMALS TO 4
loBranchFormSet.AddProperty('SetCENTURY',SET("Century"))
loBranchFormSet.AriaForm1.lcDateTemp.Century = IIF(SET("CENTURY")="ON",1 ,0)
loBranchFormSet.AriaForm1.lcDateTemp.Value = oAriaApplication.SystemDate

*- refresh the currency symbols
loBranchFormSet.AriaForm1.laData22.Valid()
loBranchFormSet.AriaForm1.rbCurr.Valid()
=lfvCent(loBranchFormSet)
RETURN 
*- End of lfSMLOCALSInit.

************************************************************
*! Name      : lfBranDest
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/06/2014
*! Purpose   : lfBranDest
************************************************************
FUNCTION lfBranDest
PARAMETERS loBranchFormSet
LOCAL lcCen
SET DECIMALS TO (loBranchFormSet.Decimals)
lcCen = loBranchFormSet.SetCENTURY
SET CENTURY &lcCen

*- End of lfBranDest.

*!**************************************************************************
*!
*!      Function : lflocShow
*!
*!**************************************************************************
FUNCTION lflocShow
PARAMETERS loFormSet
loFormSet.laData[21] = SET("CURR")
rbCurr     = loFormSet.laData[21]
puDateTyp  = SET("DATE")
loFormSet.laData[25] = PADR(SET("SEPARATOR"),1)
loFormSet.laData[24] = IIF(SET("CENTURY")="ON","ON","OFF")
lcDateTemp = oAriaApplication.SystemDate
loBranchFormSet.AriaForm1.lcCurrTemp.Value = TRANSFORM(123456.7890,'@$999999.9999')

lcDateSep  = SUBSTR(DTOC(oAriaApplication.SystemDate),IIF(pudateTyp="JAPAN",;
                    IIF(SET("CENTURY")="ON",5,3),3),1)
lcCent     = IIF(SET("CENTURY")="ON","\<Century OFF","\<Century ON")


*!**************************************************************************
*!
*!      Function : lfvCurr
*!
*!**************************************************************************
FUNCTION lfvCurr
*E303456,1 TMI 04/06/2014 11:10 [Start] 
PARAMETERS loBranchFormSet,loFld
loFormSet = loBranchFormSet.loFormSet
*E303456,1 TMI 04/06/2014 11:11 [End  ] 

rbCurr = IIF(loBranchFormSet.AriaForm1.rbCurr.Value=1,'LEFT','RIGHT')
*rbCurr = loBranchFormSet.AriaForm1.rbCurr.Value
SET CURRENCY &rbCurr

loBranchFormSet.AriaForm1.lcCurrTemp.Value = TRANSFORM(123456.7890,'@$999999.9999')
************************************************************
*! Name      : lfBranOk
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/06/2014
*! Purpose   : lfBranOk
************************************************************
FUNCTION lfBranOk
PARAMETERS loBranchFormSet,loFld
loFormSet = loBranchFormSet.loFormSet
loFormSet.laData[21] = IIF(loBranchFormSet.AriaForm1.rbCurr.Value=1,'LEFT','RIGHT')

*- End of lfBranOk.
*!**************************************************************************
*!
*!      Function : lfvCurrDat
*!
*!**************************************************************************
FUNCTION lfvCurrDat
*E303456,1 TMI 04/06/2014 11:12 [Start] 
PARAMETERS loBranchFormSet,loFld
loFormSet = loBranchFormSet.loFormSet
*E303456,1 TMI 04/06/2014 11:12 [End  ] 

lnCount = 1
DO WHILE lnCount <= LEN(loFormSet.laData[22])
  IF SUBSTR(loFormSet.laData[22],lnCount,1) $ "0123456789"
    RETURN .F.
  ENDIF
  lnCount = lnCount + 1
ENDDO   
SET CURR TO RTRIM(loFormSet.laData[22])

loBranchFormSet.AriaForm1.lcCurrTemp.Value = TRANSFORM(123456.7890,'@$999999.9999')

*!**************************************************************************
*!
*!      Function : lfvDateTyp
*!
*!**************************************************************************
FUNCTION lfvDateTyp
*E303456,1 TMI 04/06/2014 11:12 [Start] 
PARAMETERS loBranchFormSet,loFld
loFormSet = loBranchFormSet.loFormSet

*E303456,1 TMI 04/06/2014 11:12 [End  ] 
puDateTyp = loBranchFormSet.Ariaform1.puDateTyp.Value
SET DATE TO (puDateTyp)

loBranchFormSet.AriaForm1.lcDateSep.Value = SUBSTR(DTOC(oAriaApplication.SystemDate),IIF(puDateTyp="JAPAN",;
                    IIF(SET("CENTURY")="ON",5,3),3),1)
loFormSet.laData[23] = puDateTyp

*!**************************************************************************
*!
*!      Function : lfvSepar
*!
*!**************************************************************************
FUNCTION lfvSepar
*E303456,1 TMI 04/06/2014 11:12 [Start] 
PARAMETERS loBranchFormSet,loFld
loFormSet = loBranchFormSet.loFormSet
*E303456,1 TMI 04/06/2014 11:12 [End  ] 

IF loFormSet.laData[25] $ "0123456789"
  RETURN .F.
ELSE  
  SET SEPARATOR TO (ALLTRIM(loFormSet.laData[25]))
ENDIF  

*!**************************************************************************
*!
*!      Function : lfvCent
*!
*!**************************************************************************
FUNCTION lfvCent
*E303456,1 TMI 04/06/2014 11:12 [Start] 
PARAMETERS loBranchFormSet
loFormSet = loBranchFormSet.loFormSet

*E303456,1 TMI 04/06/2014 11:12 [End  ] 
lcCent = IIF(SET("CENTURY") = "ON" , "OFF" , "ON") 
SET CENTURY &lcCent

lcCent     = IIF(SET("CENTURY")="ON",LANG_CENT_ON ,LANG_CENT_OFF)
loBranchFormSet.AriaForm1.pbCent.Caption = lcCent

loBranchFormSet.AriaForm1.lcDateTemp.Century = IIF(SET("CENTURY")="ON",1 ,0)
loBranchFormSet.AriaForm1.lcDateTemp.Value = oAriaApplication.SystemDate


*!**************************************************************************
*!
*!      Function : lfvTelTemp
*!
*!**************************************************************************
FUNCTION lfvTelTemp
*E303456,1 TMI 04/06/2014 11:12 [Start] 
PARAMETERS loBranchFormSet,loFld
loFormSet = loBranchFormSet.loFormSet
*E303456,1 TMI 04/06/2014 11:12 [End  ] 

lnCount = 1
DO WHILE lnCount <= LEN(loFormSet.laData[26])
  IF SUBSTR(loFormSet.laData[26],lnCount,1) $ "0123456789"
    RETURN .F.
  ENDIF
  lnCount = lnCount + 1
ENDDO 
IF OCCURS('#',loFormSet.laData[26]) < 7
  RETURN .F.
ENDIF  
oAriaApplication.PhoneMask = loFormSet.laData[26]

*!**************************************************************************
*!
*!      Function : lfvCancel
*!
*!**************************************************************************
FUNCTION lfvCancel
*E303456,1 TMI 04/06/2014 11:10 [Start] 
PARAMETERS loBranchFormSet
loFormSet = loBranchFormSet.loFormSet

=ACOPY(loBranchFormSet.laSavData,loFormSet.laData)

IF loBranchFormSet.lcScrMode = "C"
  DO lpRstorSet
ENDIF    

*!**************************************************************************
*!
*!      Function : lfModiSet
*!
*!**************************************************************************
FUNCTION lfModiSet
LOCAL lcCurr,lcCent

lcCurr = loFormSet.laData[21]
SET CURRENCY &lcCurr 
SET CURRENCY TO (loFormSet.laData[22])
SET DATE TO (loFormSet.laData[23])

LOCAL lcCent
IF !EMPTY(loFormSet.laData[24]) 
  lcCent = loFormSet.laData[24]
  SET CENTURY &lcCent
ELSE
  SET CENTURY OFF
ENDIF 

SET SEPARATOR TO (loFormSet.laData[25])
oAriaApplication.PhoneMask = loFormSet.laData[26]

*!**************************************************************************
*!
*!      PROCEDURE : lpSavSet
*!
*!**************************************************************************
PROCEDURE lpSavSet

loBranchFormSet.laSavSet[1] = SET("CURR")
loBranchFormSet.laSavSet[2] = SET("CURR",2)
loBranchFormSet.laSavSet[3] = SET("DATE")
loBranchFormSet.laSavSet[4] = SET("CENTURY")
loBranchFormSet.laSavSet[5] = SET("SEPA")
loBranchFormSet.laSavSet[6] = oAriaApplication.PhoneMask

IF loFormSet.ActiveMode $ 'AE'
  loFormSet.laData[21] = SET("CURR")
  loFormSet.laData[22] = SET("CURR",2)
  loFormSet.laData[23] = SET("DATE")
  loFormSet.laData[24] = SET("CENTURY")
  *loFormSet.laData[25] = SET("SEPA")
  loFormSet.laData[26] = oAriaApplication.PhoneMask
ENDIF  

*!**************************************************************************
*!
*!      PROCEDURE  : lpRstorSet
*!
*!**************************************************************************
PROCEDURE lpRstorSet
LOCAL lcX
IF loBranchFormSet.lcScrMode<>"C"
  lcX = loBranchFormSet.laSavSet[1]
  SET CURRENCY &lcX
  SET CURRENCY TO loBranchFormSet.laSavSet[2]
  SET DATE TO (loBranchFormSet.laSavSet[3])
  lcX = loBranchFormSet.laSavSet[4]
  SET CENTURY &lcX
  SET SEPARATOR TO (loBranchFormSet.laSavSet[5])
  oAriaApplication.PhoneMask = loBranchFormSet.laSavSet[6]
ENDIF

*!**************************************************************************
