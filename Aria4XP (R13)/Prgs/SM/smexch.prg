*:************************************************************************
*:  Program File: \ARIA4XP\PRGS\SM\SMEXCH.Prg
*:  Module      : System Manager 
*:  Desc.       : Currency Exchange Rate Screen
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 12/30/2012
*:  Reference   : E303335,1 
*:************************************************************************
*- Get the screen , call it 
#INCLUDE R:\ARIA4XP\SCREENS\SM\SMEXCH.H
lcRunScx = lfGetScx("SM\SMEXCH.scx")
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

loFormSet.AddProperty('lcProgName','SMEXCH')

*- Set functions to the APMAIN.FXP
lcPath = oAriaapplication.ApplicationHome
SET PROCEDURE TO (lcPath+'AP\APMAIN.FXP') ADDITIVE 
SET PROCEDURE TO (lcPath+'GL\GL.FXP') ADDITIVE 

*- Open tables 
=lfOpenPRGFILES(loFormSet.lcProgName)

SELECT SYCEXCH
*SET RELATION TO CCURRCODE INTO SYCCURR

*** Load program base file 
loFormSet.AddProperty('lcBaseFile',ALLTRIM(sydObjct.cBaseFile))
loFormSet.AddProperty('lcFile_Ttl')
*- initializations
WITH loFormSet
  .cbrowsetabledbengine   = "NATIVE"
  .nWorkArea                            = .lcBaseFile 
  .otoolbar.nWorkArea                   = .lcBaseFile
  .DataEnvironment.InitialSelectedAlias = .lcBaseFile
  .cBrowseFileName        = .lcBaseFile
  .cBrowseIndexExpression = "SYCCURR.CBASECURR+SYCCURR.CCURRCODE+DTOS(SYCCURR.DRATEDATE)"
  .cBrowseIndexFields     = "CBASECURR,CCURRCODE,DRATEDATE"
  .cBrowseIndexName       = "CURRENCY"
  .cBrowseAliasName       = .lcBaseFile
  .cBrowseTableName       = .lcBaseFile
  .cBrowseFilter          = ""
  .BrowseTitle            = LANG_EXCHANGE_RATE_SCREEN_TTL

  .lcFile_Ttl = LANG_Currency_code
  lcBrFields = "CCURRCODE :R :H= '"+LANG_Currency_code+"'," +;
               "CCURRDESC :R :H= '"+LANG_Description+"',  " +;
               "CCURRSMBL :R :H= '"+LANG_Symbol+"'"
  .ariaBrFields.edtBrowseFields.Value = lcBrFields 
ENDWITH

*- Define several variables needed in the form
lfDefineVars(loFormSet)

*- Create temp file
SELECT SYCEXCH
=AFIELDS(laStructure)
lnStruLen = ALEN(laStructure,1)
DIMENSION laStructure[lnStruLen+3,18]

laStructure[lnStruLen+1,1] = 'nRecNo'
laStructure[lnStruLen+1,2] = 'N'
laStructure[lnStruLen+1,3] = 10
laStructure[lnStruLen+1,4] = 0

laStructure[lnStruLen+2,1] = 'cStatus'
laStructure[lnStruLen+2,2] = 'C'  
laStructure[lnStruLen+2,3] = 1
laStructure[lnStruLen+2,4] = 0
  
laStructure[lnStruLen+3,1] = 'CCURRDESC'
laStructure[lnStruLen+3,2] = 'C'  
laStructure[lnStruLen+3,3] = 30
laStructure[lnStruLen+3,4] = 0
  
FOR lnI = lnStruLen+1 TO ALEN(laStructure,1)
  STORE .F. TO laStructure[lnI,5],laStructure[lnI,6]
  FOR lnJ = 7 TO 16
    laStructure[lnI,lnJ] = ""
  ENDFOR
  STORE 0 TO laStructure[lnI,17],laStructure[lnI,18]
ENDFOR 
  
gcWorkDir = oAriaApplication.WorkDir 
lcExchRate = loFormSet.lcExchRate
CREATE TABLE &gcWorkDir.&lcExchRate ;
      FROM ARRAY laStructure
      
*- Create the indexes
SELECT (loFormSet.lcExchRate)
INDEX ON CBASECURR+CCURRCODE+DTOS(DRATEDATE) TAG CURRENCY   
INDEX ON CBASECURR+DTOS(DRATEDATE)+CCURRCODE TAG EXCHDATE   
SET ORDER TO CURRENCY   

loFormSet.ChangeMode('S')  

*- update grid data source
=lfSetGridDataSource(loFormSet)

************************************************************
*! Name      : lfSetGridDataSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : Set the Grid Data Source
************************************************************
FUNCTION lfSetGridDataSource
PARAMETERS loFormSet

oGrd = loFormset.Ariaform1.grdSMEXCH

lcExchRate = loFormSet.lcExchRate

oGrd.RecordSource = ''
oGrd.RecordSource = lcExchRate

lfSetColumnsProp(oGrd,'1',"&lcExchRate..cCurrCode"  ,LANG_Currency,100)
lfSetColumnsProp(oGrd,'2',"&lcExchRate..CCURRDESC" ,LANG_Currency_Description ,160)
lfSetColumnsProp(oGrd,'3',"&lcExchRate..dRateDate" ,LANG_Date ,100)
lfSetColumnsProp(oGrd,'4',"&lcExchRate..NExRate" ,LANG_Exch_Rate,100)

oGrd.Readonly = .T.

SELECT (lcExchRate)

*- Refresh the bottom of the screen
lfFormAfterRowColumnChange(loFormSet)
oGrd.Refresh()

*- End of lfSetGridDataSource.

************************************************************
*! Name      : lfFormAfterRowColumnChange
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : after row col changes in the grid
************************************************************
FUNCTION lfFormAfterRowColumnChange
PARAMETERS loFormSet


*- End of lfFormAfterRowColumnChange.

************************************************************
*! Name      : lfSetColumnsProp
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : Set Columns Properties
************************************************************
FUNCTION lfSetColumnsProp
PARAMETERS oGrd,lcCol,lcSrc,lcHeader,lnWidth
lnWidth = IIF(EMPTY(lnWidth),50,lnWidth)
WITH oGrd
  .Column&lcCol..Header1.Caption = lcHeader
  .Column&lcCol..ControlSource   = lcSrc
  .Column&lcCol..Width           = lnWidth
ENDWITH 
*- End of lfSetColumnsProp.

************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/30/2012
*! Purpose   : 
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet

loFormSet.AddProperty('lolcBrowTtl' , LANG_EXCHANGE_RATE )&& "Exchange Rates"
loFormSet.AddProperty('lcBrowWind' , gfTempName()  )
loFormSet.AddProperty('lcContWind' , gfTempName()  )
loFormSet.AddProperty('lcExchang'  , gfTempName()  )
loFormSet.AddProperty('lcExchRate'  , gfTempName()  )

loFormSet.AddProperty('lcBaseCurr' , SPACE(3))
loFormSet.AddProperty('lcCurrDesc' , '')
loFormSet.AddProperty('lcCurrency' , '')
loFormSet.AddProperty('lcOldData'  , '')
loFormSet.AddProperty('llNoContrl' , .T.)
loFormSet.AddProperty('llFirstEnt' , .T.)
loFormSet.AddProperty('llBrowse'   , .F.)
loFormSet.AddProperty('ldDate'     , {  /  /  })
loFormSet.AddProperty('lnCurrRec'  , 0)
loFormSet.AddProperty('lnFirstClk' , 0)
loFormSet.AddProperty('rbOrder'  , 1)

loFormSet.AddProperty('laScObj[1,5]' , '')

loFormSet.AddProperty('llClick',.F.)
loFormSet.AddProperty('lnTimelimt' , 0)
loFormSet.AddProperty('lnCurR' , 0)


*!**************************************************************************
*!
*!      Function: lfvNew
*!
*!**************************************************************************
*
FUNCTION lfvNew
PARAMETERS loFormSet,llNew

IF loFormSet.ActiveMode = 'E'
  lcRunScx = lfGetScx("SM\SMUPEXCH.SCX")
  DO FORM (lcRunScx) WITH loFormSet,llNew
ENDIF   


*!**************************************************************************
*!
*!      Function: lfvCurOk
*!
*!**************************************************************************
*
FUNCTION lfvCurOk
PARAMETERS loBranchFormSet

loFormSet  = loBranchFormSet.loFormSet
llNew      = loBranchFormSet.llNew

rbOrder    = loFormSet.Ariaform1.opgOrderBy.Value
lcBaseCurr = loBranchFormSet.loFormSet.Ariaform1.lcBaseCurr.Keytextbox.Value

WITH loBranchFormSet.Ariaform1
  lcCurrCode = .lcCurrCode.KeyTextbox.Value
  ldCurDate  = .ldCurDate.Value
  lnExRate   = .lnExRate.Value
ENDWITH 
lcExchRate = loFormSet.lcExchRate
SELECT (loFormSet.lcExchRate)
IF llNew
  IF EMPTY(lcCurrCode) OR EMPTY(ldCurDate)
    loBranchFormSet.Ariaform1.lcCurrCode.KeyTextbox.Setfocus()
    RETURN gfModalGen('QRM00248B00000','Dialog',IIF(EMPTY(lcCurrCode),LANG_Currency,LANG_Date))
  ENDIF
  lcSeekValue = IIF(rbOrder=1,lcCurrCode+DTOS(ldCurDate),DTOS(ldCurDate)+lcCurrCode)
  IF lnExRate<=0
    loBranchFormSet.Ariaform1.lnExRate.Setfocus()
    RETURN gfModalGen('QRM00247B00000','Dialog')
  ENDIF  
  IF SEEK(lcBaseCurr+lcSeekValue,loFormSet.lcExchRate)
    IF gfModalGen('QRM00246B00012','Dialog')=2
      loBranchFormSet.AriaForm1.ldCurDate.Text1.SetFocus()
      RETURN
    ELSE
      SELECT SYCEXCH
      GO &lcExchRate..nRecNo
      IF gfObj_Lock(.T.)
        SELECT (loFormSet.lcExchRate)
        REPLACE DRATEDATE WITH ldCurDate,;
                cStatus   WITH 'M'
      ELSE
        RETURN 
      ENDIF 
    ENDIF
  ELSE  
    SELECT (loFormSet.lcExchRate)
    APPEND BLANK
    REPLACE cBaseCurr WITH lcBaseCurr,;
            cCurrCode WITH lcCurrCode,;
            DRATEDATE WITH ldCurDate,;
            cStatus   WITH 'N'
  ENDIF
ELSE
  IF lnExRate<=0
    loBranchFormSet.Ariaform1.lnExRate.Setfocus()
    RETURN gfModalGen('QRM00247B00000','Dialog')
  ENDIF  
  SELECT SYCEXCH
  GO &lcExchRate..nRecNo
  IF gfObj_Lock(.T.)
    SELECT (loFormSet.lcExchRate)
    REPLACE cStatus   WITH 'M'
  ELSE
    RETURN 
  ENDIF   
ENDIF
SELECT (loFormSet.lcExchRate)
REPLACE nExRate WITH lnExRate
IF EMPTY(&lcExchRate..CCURRDESC)
  =SEEK(lcCurrCode,'SYCCURR')  
  REPLACE CCURRDESC WITH SYCCURR.CCURRDESC
ENDIF 
= gfAdd_Info(loFormSet.lcExchRate)

loBranchFormSet.loFormset.Ariaform1.grdSMEXCH.Refresh()
=SEEK(lcBaseCurr,'SYCCURR')
loBranchFormSet.Release


*!**************************************************************************
*!
*!      Function: lfvRemove
*!
*!**************************************************************************
* Remove the current record.
FUNCTION lfvRemove
PARAMETERS loFormSet
SELECT (loFormSet.lcExchRate)
lcExchRate = loFormSet.lcExchRate
** Check if the mesurament is been used in the gade rule then
** don't delete.
IF !EOF() 
  GO RECNO()
  IF !DELETED() 
    IF gfModalGen('QRM00007B00007','Alert') = 1      && Confirm to delete.
      
      SELECT SYCEXCH
      GO &lcExchRate..nRecNo
      IF gfObj_Lock(.T.)

        SELECT (loFormSet.lcExchRate)
        replace cStatus WITH 'D'
        DELETE
        SKIP
        IF EOF()
          SKIP -1
        ENDIF 
        loFormset.Ariaform1.grdSMEXCH.Refresh()
      
      ENDIF
      
    ENDIF  
  ELSE
    
    =gfModalGen('QRM00095B00000','Alert') 

  ENDIF
  
ENDIF 
SELECT (loFormSet.lcExchRate)



*!**************************************************************************
*!
*!      Function: lfCurFlter
*!
*!**************************************************************************
*
FUNCTION lfCurFlter
PARAMETERS lcCurrency,ldDate

lcCurrency = ALLTRIM(lcCurrency)
lcDt = ALLT(DTOS((ldDate)))
SELECT (loFormSet.lcExchRate)
lcFlt = "CCURRCODE = '&lcCurrency' AND "+;
        "DTOS(DRATEDATE) = '&lcDt' AND "+;
        "CBASECURR <> CCURRCODE"
SET FILTER TO &lcFlt
LOCATE 
loFormset.Ariaform1.grdSMEXCH.Refresh()

*!**************************************************************************
*!
*!      Function: lfvOrder
*!
*!**************************************************************************
*
FUNCTION lfvOrder
PARAMETERS loFormSet,loFld
IF loFld.Value = 1
  SET ORDER TO CURRENCY IN (loFormSet.lcExchRate)
ELSE
  SET ORDER TO EXCHDATE IN (loFormSet.lcExchRate)
ENDIF  
loFormSet.Refresh()


*!**************************************************************************
*!
*!      Function: lfvCurDate
*!
*!**************************************************************************
*
FUNCTION lfvCurDate
PARAMETERS loBranchFormSet,loFld
loFormSet = loBranchFormSet.loFormSet
rbOrder = loFormSet.Ariaform1.opgOrderBy.Value

lcBaseCurr = loBranchFormSet.loFormset.Ariaform1.lcBaseCurr.Keytextbox.Value
lcCurrCode = loBranchFormSet.AriaForm1.lcCurrCode.keytextbox.Value
ldCurDate = loFld.Value
IF !EMPTY(lcCurrCode)
  SELECT (loFormSet.lcExchRate)
  lcSeekValue = IIF(rbOrder=1,lcCurrCode+DTOS(ldCurDate),DTOS(ldCurDate)+lcCurrCode)
  IF SEEK(lcBaseCurr+lcSeekValue)
    loBranchFormSet.Ariaform1.lnExRate.Value = NExRate
  ENDIF
ENDIF


*!**************************************************************************
*!
*!      Function: lfvCurrency
*!
*!**************************************************************************
FUNCTION lfvCurrency
PARAMETERS loFormSet,loFld,llFilter

lcCurrency = loFld.KeyTextbox.Value
llBrowse  = loFld.Selectedfrombrowse
*lcBaseCurr = loFormset.Ariaform1.lcBaseCurr.Keytextbox.Value

IF llBrowse .OR. (!EMPTY(lcCurrency) .AND. !SEEK(lcCurrency,'SYCCURR')) .OR. ATC("?",lcCurrency) > 0

  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp     = ''
  lcFile_Ttl = LANG_Select_Currency   &&"Select Currency"
  =gfBrows('FOR CCURRCODE <> lcBaseCurr','CCURRCODE','laTemp',lcFile_Ttl)
  IF EMPTY(laTemp[1])
    loFld.KeyTextbox.Value = loFld.KeyTextbox.OldValue
  ELSE
    loFld.KeyTextbox.Value = laTemp[1]
  ENDIF 

ENDIF
IIF( llFilter, lfCurFlter(loFld.KeyTextbox.Value,loFormSet.Ariaform1.scpDate.Value) , '' )

************************************************************
*! Name      : lfvDate
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/31/2012
*! Purpose   : Valid fucntion for Date
************************************************************
FUNCTION lfvDate
PARAMETERS loFormSet,loFld,llFilter

IF loFld.Text1.Value <> loFld.Text1.OldValue 
  IIF( llFilter, lfCurFlter(loFormset.Ariaform1.scpCurr.Keytextbox.Value,loFld.Value) , '' )
ENDIF  

*- End of lfvDate.


************************************************************
*! Name      : lfChangeMode
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/30/2012
*! Purpose   : manipulated different screen modes, no add mode
************************************************************
FUNCTION lfChangeMode
PARAMETERS loFormSet

IF TYPE('loFormSet.lcProgName')='U'
  RETURN 
ENDIF 
LOCAL lnCurAlias
lnCurAlias = SELECT(0)

llEn = loFormSet.ActiveMode $ 'VE'
WITH loFormSet.Ariaform1
  .opgOrderby.setAll('Enabled',llEn)
  .scpCurr.Enabled = llEn
  .scpDate.Enabled = llEn
  
ENDWITH 

IF loFormSet.ActiveMode $ 'SV'  
  *- Clear edit locks if any
  SELECT (loFormSet.lcExchRate)
  lcExchRate = loFormSet.lcExchRate
  SCAN FOR cStatus = 'M'
    SELECT SYCEXCH
    GO &lcExchRate..nRecNo
    =gfObj_Lock(.F.)
  ENDSCAN 
ENDIF 


DO CASE
  *-- Select mode
  CASE loFormSet.ActiveMode = 'S'  
    loFormSet.Ariaform1.scpDate.Value = {}
    loFormset.Ariaform1.lcBaseCurr.Enabled = .T.
    
    SELECT (loFormSet.lcExchRate)
    ZAP

  *-- View mode
  CASE loFormSet.ActiveMode = 'V'
  
    
    WITH loFormset.Ariaform1
      .lcBaseCurr.Enabled = .F.
      .lcBaseCurr.KeyTextbox.Value = SYCCURR.CCURRCODE 
      .txtCurrDesc.Value = SYCCURR.CCURRDESC

      lcCurr = SYCCURR.CCURRCODE 
      *- Get detail lines
      SELECT (loFormSet.lcExchRate)
      ZAP
      =SEEK(lcCurr,'SYCEXCH')
      SELECT SYCEXCH
      lcKey = KEY()
      SCAN REST WHILE &lcKey. = lcCurr
        SCATTER MEMVAR 
        =SEEK(m.CCURRCODE,'SYCCURR')
        m.CCURRDESC = SYCCURR.CCURRDESC
        m.nRecNo = RECNO()
        INSERT INTO (loFormSet.lcExchRate) FROM MEMVAR         
      ENDSCAN 
      SELECT (loFormSet.lcExchRate)
      LOCATE
      =SEEK(lcCurr,'SYCCURR')
      .Refresh()
    ENDWITH 

    *- update grid data source
    =lfSetGridDataSource(loFormSet)
    
  *-- Edit mode
  CASE loFormSet.ActiveMode = 'E'
    WITH loFormSet.Ariaform1
    .opgOrderby.Value = 1
    .scpCurr.KeyTextbox.Value = ''
    .scpDate.Value = {}
    ENDWITH 
    SELECT (loFormSet.lcExchRate)
    SET FILTER TO 
    LOCATE 


ENDCASE

loFormSet.Ariaform1.Refresh()
loFormset.Ariaform1.grdSMEXCH.Refresh()
SELECT (lnCurAlias)

*- End of lfChangeMode.
************************************************************
*! Name      : lfFormBeforeSave
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/31/2012
*! Purpose   : before save method
************************************************************
FUNCTION lfFormBeforeSave
PARAMETERS loFormSet

*- End of lfFormBeforeSave.

************************************************************
*! Name      : lpSavScr
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/31/2012
*! Purpose   : save process
************************************************************
FUNCTION lpSavScr
PARAMETERS loFormSet

LOCAL lcDel
lcDel = SET("Deleted")
SET DELETED OFF

lcExchRate = loFormSet.lcExchRate
SELECT (lcExchRate)
SET FILTER TO 
LOCATE 
SCAN FOR !EMPTY(cStatus)
  SCATTER MEMVAR 
  IF nRecNo > 0
    GO &lcExchRate..nRecNo IN SYCEXCH
    DO CASE 
    CASE cStatus = 'M'
      SELECT SYCEXCH
      GATHER MEMVAR   
    CASE cStatus = 'D'
      SELECT SYCEXCH
      DELETE 
    ENDCASE 
  ELSE
    SELECT SYCEXCH
    APPEND BLANK
    GATHER MEMVAR   
  ENDIF 
ENDSCAN 

SELECT SYCEXCH
=gfTableUpdate()

SET DELETED &lcDel

*- return back to the lcBaseCurr record in syccurr
lcBaseCurr = loFormSet.Ariaform1.lcBaseCurr.Keytextbox.Value
SELECT SYCCURR
=SEEK(lcBaseCurr,'SYCCURR')

*- End of lpSavScr.

************************************************************
*! Name      : lfFormActivate
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : Activate the screen
************************************************************
FUNCTION lfFormActivate
PARAMETERS loFormSet

*- End of lfFormActivate.

************************************************************
*! Name      : lfFormdestroy
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : Destroy function
************************************************************
FUNCTION lfFormdestroy
PARAMETERS loFormSet

*- End of lfFormdestroy.

************************************************************
*! Name      : lfvBaseCurr
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/30/2012
*! Purpose   : Select the base currency symbol
************************************************************
FUNCTION lfvBaseCurr
PARAMETERS loFormSet,loFld
llBrow = loFld.Selectedfrombrowse
IF loFld.Selectedfrombrowse OR !SEEK(loFld.KeyTextbox.Value,'SYCCURR')
  loFormSet.oToolBar.cmdFind.Click()
ELSE
  loFormSet.ChangeMode('V')  
ENDIF 
*- End of lfBaseCurr.

************************************************************
*! Name      : lfSMUPEXCH
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/31/2012
*! Purpose   : init method of the branch form
************************************************************
FUNCTION lfSMUPEXCH
PARAMETERS loBranchForm,loFormSet,llNew

loBranchForm.AddProperty('loFormSet',loFormSet)
loBranchForm.AddProperty('llNew',llNew)

lcExchRate = loFormSet.lcExchRate
WITH loBranchForm.AriaForm1
  .lcCurrCode.Enabled = llNew
  .ldCurDate.Enabled = llNew
  .lcCurrCode.Keytextbox.Value = IIF(llNew,'',&lcExchRate..cCurrCode)
  .ldCurDate.Value = IIF(llNew,DATE(),&lcExchRate..DRATEDATE)
  .lnExRate.Value = IIF(llNew,0.0,&lcExchRate..nExRate)
ENDWITH   
*- End of lfSMUPEXCH.

