*:************************************************************************
*:  Program File: ARIA4XP\PRGS\GL\GLTMPTR.PRG
*:  Module      : General Ledger
*:  Desc.       : Automatic Distribution screen
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 08/02/2012
*:  Reference   : *E303211,1 
*:************************************************************************
* Modifications
*B610082,1 TMI 09/16/2012 [T20120902.0021] fix some problems coming from testing with Tony
*:************************************************************************

*- Call the screen
lcRunScx = lfGetScx("GL\GLDSTTR.scx")
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
*! Date      : 07/24/2012
*! Purpose   : called from the Screen init Method
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet

loFormSet.AddProperty('lcProgName','GLDSTTR')

*- Set functions to the APMAIN.FXP
lcPath = oAriaapplication.ApplicationHome
SET PROCEDURE TO (lcPath+'AP\APMAIN.FXP') ADDITIVE 
SET PROCEDURE TO (lcPath+'GL\GL.FXP') ADDITIVE 



IF !lfGL(loFormset)
  RETURN .F.
ENDIF 

*- Open tables 
=lfOpenPRGFILES(loFormSet.lcProgName)

SELECT GLTYPES
GO TOP
IF EOF()
  =gfModalGen("TRM02038B00000","DIALOG")
  glQuitting  = .T.  
  RETURN 
ENDIF

*** Load program base file 
=lfAddProp(loFormSet,'lcBaseFile',ALLTRIM(sydObjct.cBaseFile))
loFormSet.AddProperty('lcFile_Ttl',' ')
loFormSet.lcFile_Ttl = LOOKUP(SYDFILES.CFILE_TTL,loFormset.lcBaseFile,SYDFILES.CFILE_NAM)

*- initializations
WITH loFormSet
  .cbrowsetabledbengine   = "NATIVE"
  .nWorkArea                            = .lcBaseFile 
  .otoolbar.nWorkArea                   = .lcBaseFile
  .DataEnvironment.InitialSelectedAlias = .lcBaseFile
  .cBrowseFileName        = .lcBaseFile
  .cBrowseIndexExpression = "CAUTTYPE+CAUTCODE"
  .cBrowseIndexFields     = "CAUTTYPE,CAUTCODE"
  .cBrowseIndexName       = "TYPECODE"
  .cBrowseAliasName       = .lcBaseFile
  .cBrowseTableName       = .lcBaseFile
  .cBrowseFilter          = "CAUTTYPE='D'"
  .BrowseTitle 		  	    = loFormSet.lcFile_Ttl 
  .ariaBrFields.edtBrowseFields.Value = gfDbfField(.nWorkArea)
  .Ariaform1.lcAcSegDes.Caption = .lcAcSegDes  
ENDWITH 

*- Define needed variables.
=lfDefineVars(loFormSet)


*- Define input mask for fields
WITH loFormSet.Ariaform1
  .laData2.KeyTextbox.InputMask = REPLICATE('!',FSIZE('CAUTCODE',loFormSet.lcBaseFile))
  .laData3.InputMask = REPLICATE('X',FSIZE('CAUTREF',loFormSet.lcBaseFile))
  .laData4.InputMask = REPLICATE('X',FSIZE('CAUTDES',loFormSet.lcBaseFile))
  .lnAmount.InputMask = '999.99 %'
ENDWITH 

loFormSet.ChangeMode('S')

*- End of lfFormInit.


************************************************************
*! Name      : lfFormAfterRowColumnChange
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/25/2012
*! Purpose   : adjust data value when line changes in the Grid
************************************************************
FUNCTION lfFormAfterRowColumnChange
PARAMETERS loFormSet
lc_TmpDst = loFormSet.lc_TmpDst
WITH loFormSet.Ariaform1
  .lnAmount.Value = &lc_TmpDst..nAmount
  
  .lcAcctcode.KEYTEXTBOX.Value = &lc_TmpDst..CACCTCODE
  .lcAccnlDes.Value = LOOKUP(GLACCHAR.cAccnldes,&lc_TmpDst..cAcctcode,GLACCHAR.cAcctcode,'ACCTCODE')
ENDWITH 

*- End of lfFormAfterRowColumnChange.
************************************************************
*! Name      : lfFormdestroy
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/01/2012
*! Purpose   : Destroy form Event
************************************************************
FUNCTION lfFormdestroy
PARAMETERS loFormSet

gcWorkDir = oAriaApplication.WorkDir
lc_TmpDst = loFormSet.lc_TmpDst

SELECT GLAUTHD

SET FILTER TO  
  *** reset tag of GLAUTDT
  SELECT GLAUTDT
  IF !EMPTY(loFormSet.lcAutDtTg)
    SET ORDER TO TAG (loFormSet.lcAutDtTg)
  ELSE
    SET ORDER TO
  ENDIF
      
  *** Close open temporary files.  
  IF USED(lc_TmpDst)
    USE IN ALIAS(lc_TmpDst)
  ENDIF
  ERASE &gcWorkDir.&lc_TmpDst..dbf  



*- End of lfFormdestroy.
************************************************************
*! Name      : lfFormActivate
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/01/2012
*! Purpose   : Activate screen event
************************************************************
FUNCTION lfFormActivate
PARAMETERS loFormSet

*- End of lfFormActivate.
************************************************************
*! Name      : lfSetGridColumnHeader
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/27/2012
*! Purpose   : Set Grid Columns Header
************************************************************
FUNCTION lfSetGridColumnHeader
PARAMETERS loFormSet,loFld
WITH loFormSet.Ariaform1.grdGLAUTHD
  .Column1.Header1.Caption = ALLTRIM(loFormSet.lcAcSegDes)
  .Column2.Header1.Caption = 'Description'
  .Column3.Header1.Caption = 'Distribution %'
  .Refresh()
ENDWITH 
*- End of lfSetGridColumnHeader.

************************************************************
*! Name      : lfSetGridColumnsource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/27/2012
*! Purpose   : Set Grid Columns Source
************************************************************
FUNCTION lfSetGridColumnsource
PARAMETERS loFormSet
WITH loFormSet.Ariaform1.grdGLAUTHD
  lc_TmpDst = loFormSet.lc_TmpDst
  .RecordSource = loFormSet.lc_TmpDst
  .Column1.ControlSource = "&lc_TmpDst..cAcctcode"
  .Column2.ControlSource = "LOOKUP(GLACCHAR.cAccnldes,&lc_TmpDst..cAcctcode,GLACCHAR.cAcctcode,'ACCTCODE')"
  .Column3.ControlSource = "&lc_TmpDst..nAmount"
ENDWITH 
*- End of lfSetGridColumnsource.

***************** original Code **
************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/01/2012
*! Purpose   : Define needed variables 
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet

DECLARE laAutDtFld [1,4]       && holds the file structure of 'GLAUTDT' file

 
loFormSet.AddProperty('lc_TmpDst'  , ''  )    && temporary file name variable

loFormSet.AddProperty('lcAcctCode' , REPLICATE ("0",loFormSet.lnAcsSegSz)) 
loFormSet.AddProperty('lcCodeType' , ''  )    && variable to hold the current account type 
loFormSet.AddProperty('lcAutDtTg'  , ''  )    && Tag name of GLAUTDT upon entry.      
loFormSet.AddProperty('lcAutDtEx'  , ''  )    && Tag expression of GLAUTDT current tag

loFormSet.AddProperty('lnTmpRcCnt' , 0   )    && number of records in th list 

loFormSet.AddProperty('lnThermRec' , 0   )    && Thermometer counter


loFormSet.AddProperty('lnDstTotal' , 0   )    && variable to hold total of distribution %s.


*B600379,1 Reham On 06/05/95 Add this flag to know if there is any 
*B600379,1 detail line without an account.
loFormSet.AddProperty('llNewDet'   , .F. ) 

*** Variables in screen objects file
loFormSet.AddProperty('lcTDfRef'   , "On" )
loFormSet.AddProperty('lcTDfDesc'  , "Created by" )
loFormSet.AddProperty('lcTDstAmnt' , "distributed amounts" )
loFormSet.AddProperty('lcTDstTr'   , "distribution transaction|distribution transaction" )
loFormSet.AddProperty('lcTTotals'  , "Total of all entries" )

*** Screen display variable :
loFormSet.AddProperty('lcTTotal'   , ""  )    && 'Total :'  


*loFormSet.AddProperty('lcUserName' , oAriaApplication.User_ID)  && gcUserName 
*lcUserName = LOOKUP(SYUUSER.cUsr_Name,gcUser_ID,SYUUSER.cUser_ID,'cUser_ID')
loFormSet.AddProperty('lcCurDate'  , IIF(SET('CENTURY')='ON',DTOC(oAriaApplication.SystemDate),;
                 LEFT(DTOC(oAriaApplication.SystemDate),6)+STR(YEAR(oAriaApplication.SystemDate),4)) )

lcScFields = 'CAUTTYPE,CAUTCODE,CAUTREF,CAUTDES'
loFormSet.AddProperty('lcScFields',lcScFields)

SELECT (loFormset.lcBasefile)
  SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData BLANK
  loFormSet.laData[1]       = 'D'

  *** Create a name for the temporary file 
  loFormSet.lc_TmpDst        = gfTempName()
  
  *** Create the temporary file with the appropriate fields , 
  ***(in this case all fields of file "GLAUTDT" + 2 more fields
  *** for the record number and status('M'odify,'D'elete,'A'dd,'S'ame)
  SELECT GLAUTDT
  
  loFormSet.lcAutDtTg       = SYS(22)
  
  SET ORDER TO TAG TYPCODACC
  
  *** Current tag expression is cAutType + cAutCode + cAcctCode
  loFormSet.lcAutDtEx       = SYS(14,VAL(SYS(21)))
  
  =AFIELDS(laAutDtFld)
  lnAutDtFld = ALEN(laAutDtFld,1)
  DIMENSION laAutDtFld[lnAutDtFld+2,18]

  laAutDtFld[lnAutDtFld+1,1] = 'nRecNo'
  laAutDtFld[lnAutDtFld+1,2] = 'N'
  laAutDtFld[lnAutDtFld+1,3] = 10
  laAutDtFld[lnAutDtFld+1,4] = 0

  laAutDtFld[lnAutDtFld+2,1] = 'cStatus'
  laAutDtFld[lnAutDtFld+2,2] = 'C'  
  laAutDtFld[lnAutDtFld+2,3] = 1
  laAutDtFld[lnAutDtFld+2,4] = 0

  FOR lnI = lnAutDtFld+1 TO ALEN(laAutDtFld,1)
    STORE .F. TO laAutDtFld[lnI,5],laAutDtFld[lnI,6]
    FOR lnJ = 7 TO 16
      laAutDtFld[lnI,lnJ] = ""
    ENDFOR
    STORE 0 TO laAutDtFld[lnI,17],laAutDtFld[lnI,18]
  ENDFOR 
  
  gcWorkDir = oAriaApplication.WorkDir 
  lc_TmpDst = loFormSet.lc_TmpDst

  CREATE TABLE &gcWorkDir.&lc_TmpDst;
      FROM ARRAY laAutDtFld

  *** Prepare lcFieldSt for the list
  SELECT (lc_TmpDst)
  

=lfSetGridColumnHeader(loFormSet)
=lfSetGridColumnsource(loFormSet)
                     
                     

SELECT GLAUTHD
SET FILTER TO

*** Filter on distribution transactions records only
SET FILTER TO cAutType="D"
*B600379,1 Reham On 06/05/95 
*B600379,1 Change the command from "LOCATE" to the next command to prevent
*B600379,1 changing the record no. if leaving the screen to another one &
*B600379,1 back again in the GLAUTHD
LOCATE FOR CAUTCODE = loFormSet.laData[2]

IF loFormSet.ActiveMode = 'S' .OR. loFormSet.ActiveMode = 'V' 
  STORE "DISABLE" TO lcAct_Stat,lcPer_Stat,lcNew_Stat,lcRem_Stat
ELSE
  lcAct_Stat = IIF(loFormset.lnTmpRcCnt = 0   , "DISABLE" , "ENABLE")
  lcPer_Stat = IIF(loFormset.lnTmpRcCnt = 0   , "DISABLE" , "ENABLE")
  
  *B600379,1 Reham On 06/05/95 Add the flag "llNewDet" to the condtion
  *B600379,1 to know if there is detail line without an acc.
  lcNew_Stat = IIF(loFormSet.lnDstTotal = 100 .OR. loFormSet.llNewDet, "DISABLE" , "ENABLE")
  lcRem_Stat = IIF(loFormset.lnTmpRcCnt = 0   , "DISABLE" , "ENABLE")
ENDIF

*** Variables hold the status of objects. ***
loFormSet.AddProperty('lcAct_Stat' , lcAct_Stat  )
loFormSet.AddProperty('lcPer_Stat' , lcPer_Stat  )
loFormSet.AddProperty('lcNew_Stat' , lcNew_Stat  )
loFormSet.AddProperty('lcRem_Stat' , lcRem_Stat  )


*- Set Control Source
WITH loFormSet.Ariaform1
  .laData2.Keytextbox.ControlSource = 'Thisformset.laData[2]'
  .laData3.ControlSource = 'Thisformset.laData[3]'
  .laData4.ControlSource = 'Thisformset.laData[4]'
  .txtSummary.ControlSource = 'ThisFormSet.lnDstTotal'
ENDWITH 
*E300683,5 Call *.SPR from screens directory
* DO GLDstTr.SPR 

*- End of lfDefineVars.

*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lfChangeMode
PARAMETERS loFormSet

IF TYPE('loFormSet.lcProgName')='U'
  RETURN 
ENDIF 

lc_TmpDst = loFormSet.lc_TmpDst
gcDataDir = oAriaApplication.DataDir
gcWorkDir = oAriaApplication.WorkDir

loFormSet.laData[1] = 'D'

DO CASE  
  *** "Select" mode (loFormSet.ActiveMode = 'S'=.T.)
  CASE loFormSet.ActiveMode = 'S' 
    SELECT (lc_TmpDst)

    *** Delete old data ( if any )
    ZAP
    
    *** Disable error handler until the list is refreshed,then
    *** Enable it again

    *** Initialize variables for display
    loFormset.lnTmpRcCnt = 0       && No nonempty records in the temporary file

    loFormSet.Ariaform1.lcAcctcode.Keytextbox.Value = REPLICATE ("0",loFormSet.lnAcsSegSz)
    loFormSet.Ariaform1.lcAccnlDes.Value = SPACE(60)

    loFormSet.Ariaform1.lnAmount.Value   = 0

    loFormSet.lnDstTotal = 0

    
    loFormSet.Ariaform1.laData2.Enabled = .T.
    loFormSet.Ariaform1.laData2.Keytextbox.Setfocus()
    loFormSet.Ariaform1.lcAcctCode.Enabled = .F. && DISABLE
    
    
    loFormSet.Ariaform1.lnAmount.Enabled = .F. &&   DISABLE

    *** Controls
    loFormSet.Ariaform1.pbNew.Enabled = .F.  &&    DISABLE 
    loFormSet.Ariaform1.pbRem.Enabled = .F.  &&    DISABLE 
    
  *** "View" mode (loFormSet.ActiveMode = 'V'=.T.), or "Edit" mode (loFormSet.ActiveMode = 'E'=.T.)
  CASE loFormSet.ActiveMode = 'V'.OR.loFormSet.ActiveMode = 'E' 

    IF loFormSet.ActiveMode = 'V'
      *loFormSet.laData[2] = GLAUTHD.CAUTCODE
      lcScFields = loFormSet.lcScFields
      SCATTER FIELDS &lcScFields TO loFormSet.laData
    ENDIF 

    llFirstRec     = .T.

    SELECT (lc_TmpDst)
    
    lcDStamp = ''
    lcDStamp       = IIF(loFormSet.ActiveMode = 'V', GLAUTHD.cAdd_User+DTOC(GLAUTHD.dAdd_Date);
                        +GLAUTHD.cAdd_Time,lcDStamp)
    lcAutDtEx = loFormSet.lcAutDtEx      
    IF loFormSet.ActiveMode = 'V' .OR. lcDStamp <> GLAUTHD.cAdd_User+DTOC(GLAUTHD.dAdd_Date);
                        +GLAUTHD.cAdd_Time
      SELECT GLAUTDT  

      loFormSet.Ariaform1.grdGLAUTHD.RecordSource = ''
      loFormSet.lnDstTotal   = 0
      SELECT *,RECNO() AS 'nRecNo',	"S" AS 'cStatus';
             FROM &gcDataDir.GLAUTDT;
             INTO DBF &gcWorkDir.&lc_TmpDst;
             WHERE &lcAutDtEx. ="D"+loFormSet.laData[2];
             .AND. lfTotals()
      =lfSetGridColumnHeader(loFormSet)
      =lfSetGridColumnsource(loFormSet)
    ENDIF
    
    SELECT (lc_TmpDst)

    *** Get the type of accounts
    IF loFormSet.ActiveMode = 'E'
      loFormSet.lcCodeType = IIF(LEFT(LOOKUP(GLACCHAR.cTypeCode,;
                      &lc_TmpDst..cAcctCode,;
                      GLACCHAR.cAcctCode,'ACCTCODE'),1)="Y","S","T")    
    ENDIF                             
   
    *** Get the number of records currently in the temporary file
    loFormset.lnTmpRcCnt = RECCOUNT(lc_TmpDst)

    *** Select the previously selected record,( lnOldDsRec)

    *** Considering the fact that there has to be at least one record
    *** in the temporary file,( due to the fact that we cannot "Save"
    *** until there is a total number of distribution %s =100 ),
    *** There seems to be no reason for checking for the number of
    *** Records in the file,or to initialize variables to spaces,hence,
    loFormSet.Ariaform1.lcAcctcode.Keytextbox.Value = cAcctCode
    loFormSet.Ariaform1.lcAccnlDes.Value = LOOKUP(GLACCHAR.cAccnlDes,&lc_TmpDst..cAcctCode,;
                        GLACCHAR.cAcctCode,'ACCTCODE')
    loFormSet.Ariaform1.lnAmount.Value   = nAmount
      
    *** Prepare screen objects
    lcObjState = IIF(loFormSet.ActiveMode = 'E',"ENABLE","DISABLE")
    loFormSet.Ariaform1.lcAcctCode.Enabled =  '&lcObjState.' = 'ENABLE'

    loFormSet.Ariaform1.lnAmount.Enabled   =  '&lcObjState.' = 'ENABLE'
    loFormSet.Ariaform1.lcAccnlDes.Enabled =  '&lcObjState.' = 'ENABLE'

    lcObjState = IIF(loFormSet.ActiveMode = 'E' .AND. !EOF(lc_TmpDst),;
                         "ENABLE","DISABLE")
    loFormSet.Ariaform1.pbRem.Enabled =  '&lcObjState.' = 'ENABLE'
    
    *** Just in case the New button is enabled in EDIT mode,
    *** disable it in VIEW mode 
    IF loFormSet.ActiveMode = 'V'
      loFormSet.Ariaform1.pbNew.Enabled = .F. &&     DISABLE
    ENDIF   

    *** Disable error handler until the list is refreshed,then
    *** Enable it again
  
  *** "Add" mode (loFormSet.ActiveMode = 'A'=.T.) 
  CASE loFormSet.ActiveMode = 'A'
  
    loFormSet.Ariaform1.laData2.Enabled = .F.

    *** Prepare defaults
    *** Remember that lcCurDate's length depends upon the century setting.
    lcDfRef    = loFormSet.lcTDfRef+' '+loFormSet.lcCurDate
    loFormSet.laData[3]  = lcDfRef+SPACE(FSIZE('cAutRef','GLAUTHD')-LEN(lcDfRef))  
    loFormSet.laData[4]  = SUBSTR(loFormSet.lcTDfDesc+' '+oAriaApplication.User_ID,1,;     
                         FSIZE('cAutDes','GLAUTHD'))
    
    loFormSet.Ariaform1.pbNew.Enabled = .T.&&    ENABLE 

ENDCASE 

loFormSet.Ariaform1.Refresh()

SELECT GLAUTHD   
 
*!**************************************************************************
*!
*!      Function: lfTotals
*!
*!**************************************************************************
*    This function gets the total distribution percentages,called
*    while SELECT_SQL command is executed.
*!**************************************************************************
FUNCTION lfTotals

*** Skip first record (SELECT SQL counts it twice),first time
IF llFirstRec
  llFirstRec       = .F.
ELSE  
  loFormSet.lnDstTotal       = loFormSet.lnDstTotal + nAmount
ENDIF 

************************************************************
*! Name      : lfvData_2
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/01/2012
*! Purpose   : Validation function for the ladata[2] field
************************************************************
FUNCTION lfvData_2
PARAMETERS loFormSet,loFld
LOCAL lnSlct

lnSlct = SELECT(0)

WITH loFld.Keytextbox
.Value     = ALLTRIM(.Value)
.Value     = IIF(ISDIGIT(LEFT(.Value,1)),;
                    PADL(.Value, FSIZE('cAutCode','GLAUTHD'),'0'),;
                    PADR(.Value, FSIZE('cAutCode','GLAUTHD'),SPACE(1)))
ENDWITH 
loFormSet.laData[2] = loFld.KeyTextBox.VALUE

lcFile_Ttl = loFormSet.BrowseTitle
lcBrFields = loFormSet.ariaBrFields.edtBrowseFields.Value 
llView = .F.
lcBaseFile = loFormSet.lcBaseFile
llBrowse = loFld.Selectedfrombrowse 

SELECT (lcBaseFile)
IF llBrowse .OR. !SEEK(loFormSet.laData[1]+loFld.KeyTextBox.VALUE) .OR. ATC("?",loFld.KeyTextBox.VALUE) > 0  
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
        loFormSet.CHangeMode('A')
        RETURN
        
      CASE lnOption = 3
        loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
        RETURN .F.
    ENDCASE
  ENDIF
ELSE
  loFld.KeyTextBox.VALUE = &lcBaseFile..CAUTCODE
  llView = .T.
ENDIF

IF llView = .T.
  loFormSet.CHangeMode('V')
ENDIF 

SELECT (lnSlct)
*- End of lfvData_2.
 
*!**************************************************************************
*!
*!      Function: lfvAccCode
*!
*!**************************************************************************
*    VALID function for get field "lcAcctCode" 
*
FUNCTION lfvAccCode
PARAMETERS loFormSet,loFld

PRIVATE lcCodeT

*E303211,1 TMI 08/21/2012 [Start] if came from browse add the '?' sign
IF loFld.Selectedfrombrowse = .T.
  loFld.KeyTextbox.Value = STUFF(loFld.KeyTextbox.Value,1,1,'?')
ENDIF
*E303211,1 TMI 08/21/2012 [End  ] 

lc_TmpDst = loFormSet.lc_TmpDst
lcAcctCode = loFld.KeyTextbox.Value
IF LEFT(LTRIM(lcAcctCode),1)<>'?'.AND. !ISDIGIT(LTRIM(lcAcctCode))
  =gfModalGen("TRM02061B00000","Dialog")
  loFld.KeyTextbox.Value = loFld.Keytextbox.OldValue
  RETURN .F.
ENDIF


  *** This condition is true only if the account code had an old entry
  *** and now it is emptied,just ignore the entry
  IF LEFT(LTRIM(lcAcctCode),1)<>'?'.AND.VAL(STRTRAN(lcAcctCode,'-','')) = 0  

    loFld.KeyTextbox.Value = loFld.Keytextbox.OldValue

  ELSE
    lcCodeT    = ''
    *** If there is only one record in the temporary file,
    *** The Account Type may be changed.
    *** This condition applies for the following cases :
    *** a. A NEW entry ( Add mode, or Edit mode) in an empty list.
    *** b. Removing all records except one.
    *** c. Removing all records ( handled from NEW )
    IF loFormset.lnTmpRcCnt = 1
      loFormSet.lcCodeType = "A"
    ENDIF
  
    lcAccnlDes = loFormSet.Ariaform1.lcAccnlDes.Value
    *B610082,1 TMI 09/16/2012 [Start] be sure that the account is selected 
    *IF lfVldAccnt(loFormSet,loFld,loFormSet.lcCodeType,"C","L",.T.,@lcAccnlDes,@lcCodeT,"")     
    IF lfVldAccnt(loFormSet,loFld,loFormSet.lcCodeType,"C","L",.T.,@lcAccnlDes,@lcCodeT,"") AND !EMPTY(CHRTRAN(loFld.Keytextbox.Value,'-0',''))
      lcAcctCode = loFld.Keytextbox.Value
      *B610082,1 TMI 09/16/2012 [End  ] 
      IF loFormSet.lcCodeType = "A"
        loFormSet.lcCodeType = IIF(LEFT(lcCodeT,1)="Y","S","T")
      ENDIF    

      SELECT (lc_TmpDst)     
       
      *** If previously modified,"M---->M"
      *** If a new entry,        "A---->A"
      *** else                   "S---->M"       
 
      lcStatus  = SUBSTR("MAM",AT(cStatus,"MAS"),1)
 
      REPLACE cAcctcode WITH lcAcctCode ,;
              cStatus   WITH lcStatus

      *** Refresh objects
      
      SELECT GLAUTHD   
      
      *** Enable disabled objects
      *** Since zero entries are allowed in the amounts field,
      *** New button may be enabled as soon as a valid account
      *** is entered unless total is already = 100.
      IF loFormSet.lnDstTotal = 100

      ELSE
        loFormSet.Ariaform1.pbNew.Enabled = .T. &&   ENABLE
        *B600379,1 Reham On 06/05/95 Add this flag to know if there is any 
        *B600379,1 detail line without an account.
        loFormSet.llNewDet   = .F.
      ENDIF
      loFormSet.Ariaform1.lnAmount.Enabled = .T. &&  ENABLE
      loFormSet.Ariaform1.lcAccnlDes.Value = lcAccnlDes
    ELSE  

      loFld.KeyTextbox.Value = loFld.Keytextbox.OldValue

      RETURN .F.
    ENDIF  
  ENDIF  

*!**************************************************************************
*!
*!      Function:  lfvAdtAmnt
*!
*!**************************************************************************
*    VALID function for get field "lnAmount" 
*
FUNCTION lfvAdtAmnt
PARAMETERS loFormSet,loFld

lc_TmpDst = loFormSet.lc_TmpDst
*IF lnAmount = lnOldAmnt
IF loFld.Value = loFld.OldValue
  RETURN
ENDIF

*** Reject negative entries
IF loFld.Value < 0
  =gfModalGen("TRM02036B00000","DIALOG")

  loFld.Value = loFld.OldValue

  RETURN .F.
ELSE 

  SELECT (lc_TmpDst)

  *** If the user inputs an entry in this field,
  IF loFld.Value  > 0  .AND. (loFormSet.lnDstTotal-nAmount+loFld.Value) > 100
    *** If this entry caused the total of the distribution percentages
    *** to exceed 100 %, do not accept the entry,and present a message
    =gfModalGen("TRM02017B00000","DIALOG",loFormSet.lcTDstAmnt)
    loFld.Value = loFld.OldValue
    *** Return to field
    RETURN .F.
  ELSE  
    loFormSet.lnDstTotal = loFormSet.lnDstTotal-nAmount+loFld.Value
    lcStatus   = SUBSTR("MAM",AT(cStatus,"MAS"),1)
    REPLACE nAmount WITH loFld.Value ;
            cStatus WITH lcStatus
    
    *** Adjust controls
    IF VAL(STRTRAN(loFormSet.Ariaform1.lcAcctcode.Keytextbox.Value,'-','')) <> 0
      lcObjState = IIF(loFormSet.lnDstTotal=100,"DISABLE","ENABLE")
      *B600379,1 Reham On 06/05/95
      *B600379,1 Set the flag to know the <NEW> button status.
      loFormSet.llNewDet   = IIF(loFormSet.lnDstTotal=100,.T.,.F.)
      loFormSet.Ariaform1.pbNew.Enabled =  '&lcObjState.' = 'ENABLE'
    ENDIF
 
    SELECT GLAUTHD
  
  ENDIF
ENDIF
RETURN .T.
*!**************************************************************************
*!
*!      Function: lfvNew
*!
*!**************************************************************************
*    VALID function for push button "New" (pbNew).
*
FUNCTION lfvNew
PARAMETERS loFormSet

*** Are there any empty records in the file?
*** If there are,find them and replace them with the new values
*** else Insert a new record and prepare it to be filled by the user ,
*** initializing it with type ("D"),distribution code and status="A" 
*** ( for addition )
lc_TmpDst = loFormSet.lc_TmpDst
gcWorkDir = oAriaApplication.WorkDir

SELECT (lc_TmpDst)

LOCATE FOR EMPTY(cAutType)
IF FOUND()
  REPLACE cAutType WITH loFormSet.laData[1],;
          cAutCode WITH loFormSet.laData[2],;   
          cStatus  WITH 'A'
ELSE
  INSERT INTO &gcWorkDir.&lc_TmpDst.;
              (cAutType,cAutCode,cStatus);
         VALUES (loFormSet.laData[1],loFormSet.laData[2],"A")      
ENDIF

*** Add Audit Information to the newly created record
=gfAdd_Info(lc_TmpDst)

*** The following fields are blanked,waiting for an entry
*** When they are entered,their valid functions take care of their saving 
loFormSet.Ariaform1.lcAcctcode.Keytextbox.Value = REPLICATE ("0",loFormSet.lnAcsSegSz)
loFormSet.Ariaform1.lcAccnlDes.Value = SPACE(60)
loFormSet.Ariaform1.lnAmount.Value   = 0

*** Increase number of records in temporary file
loFormset.lnTmpRcCnt = loFormset.lnTmpRcCnt + 1

*** Select the new record from the list

loFormSet.Ariaform1.lcAcctCode.Enabled = .T. && ENABLE
loFormSet.Ariaform1.lnAmount.Enabled = .T.&&   ENABLE

IF loFormset.lnTmpRcCnt = 1
  loFormSet.Ariaform1.pbRem.Enabled = .T.&&    ENABLE
ENDIF
*** Disable New button until a valid account is ebtered
loFormSet.Ariaform1.pbNew.Enabled = .F.&&      DISABLE

*B600379,1 Reham On 06/05/95 Add this flag to know that there is detail
*B600379,1 line added but its account is not added yet.
loFormSet.llNewDet   = .T.

loFormSet.Ariaform1.lnAmount.Enabled = .F. &&   DISABLE 

*** Do not forget to refresh the list

*** Prepare the user for entry by moving the cursor
*** (activating object) to the cAcctCode field (lcAcctCode object) 
loFormSet.Ariaform1.lcAcctCode.Keytextbox.Setfocus()
loFormSet.Ariaform1.laData3.TabStop = .F.
loFormSet.Ariaform1.laData4.TabStop = .F.
loFormset.Ariaform1.grdGLAUTHD.Refresh()
loFormset.Ariaform1.Refresh()

*** Always return to the original work aria
SELECT GLAUTHD

*!**************************************************************************
*!
*!      Function: lfvRem
*!
*!**************************************************************************
*    VALID function for push button "Remove" (pbRem).
*  
FUNCTION lfvRem
PARAMETERS loFormSet

lc_TmpDst = loFormSet.lc_TmpDst  
*** Confirm Removing of the record
IF gfModalGen("QRM00007B00007","ALERT") = 1

  SELECT (lc_TmpDst)  

  *** If the record is previuosly modified,"M---->D"
  ***   delete it.
  *** If it is a new entry                 "A---->S"
  ***   skip it when saving
  *** else (a "Same" record )              "S---->D" 
  ***   delete it
  REPLACE cStatus WITH SUBSTR("DSD",AT(cStatus,"MAS"),1)

  *** Decrement number of records in list
  loFormset.lnTmpRcCnt = loFormset.lnTmpRcCnt - 1

  *** Adjust total distribution
  loFormSet.lnDstTotal = loFormSet.lnDstTotal - nAmount

  *** Delete the current record (to be removed )
  *** If the removed record is the last one,go top 
  DELETE
  SKIP
  IF EOF(lc_TmpDst)
    GO TOP
  ENDIF  

  *** Refresh objects with contents of the current record,or spaces
  *** if the list is empty
  loFormSet.Ariaform1.lcAcctcode.Keytextbox.Value = IIF(loFormset.lnTmpRcCnt=0,REPLICATE ("0",loFormSet.lnAcsSegSz),cAcctCode)
  loFormSet.Ariaform1.lcAccnlDes.Value = IIF(loFormset.lnTmpRcCnt=0,SPACE(60),LOOKUP(GLACCHAR.cAccnlDes,;
                   &lc_TmpDst..cAcctCode,GLACCHAR.cAcctCode,'ACCTCODE'))
  loFormSet.Ariaform1.lnAmount.Value   = IIF(loFormset.lnTmpRcCnt=0,0,nAmount)                                      

  *** Adjust controls
  lcObjState = IIF(loFormset.lnTmpRcCnt=0,"DISABLE","ENABLE")

  loFormSet.Ariaform1.lcAcctCode.Enabled = '&lcObjState.'  = 'ENABLE'

  loFormSet.Ariaform1.lnAmount.Enabled = '&lcObjState.'  = 'ENABLE'
  loFormSet.Ariaform1.pbRem.Enabled = '&lcObjState.'  = 'ENABLE'

  lcObjState = IIF(loFormSet.lnDstTotal<100,"ENABLE","DISABLE")
  *B600379,1 Reham On 06/05/95
  *B600379,1 Set flag to know the new button status.
  loFormSet.llNewDet   = IIF(loFormSet.lnDstTotal<100,.F.,.T.)
  loFormSet.Ariaform1.pbNew.Enabled = '&lcObjState.'  = 'ENABLE'

  *** Update list contents 

  SELECT GLAUTHD
ENDIF

loFormset.Ariaform1.grdGLAUTHD.Refresh()
loFormset.Ariaform1.Refresh()

************************************************************
*! Name      : lfFormBeforeSave
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/01/2012
*! Purpose   : Before Save 
************************************************************
FUNCTION lfFormBeforeSave
PARAMETERS loFormSet


DO CASE
  *** If the file is empty,i.e. there are no recurring transactions,
  *** "Cannot save an empty transaction" 
  CASE (loFormset.lnTmpRcCnt = 0 )
    =gfModalGen("TRM02035B00000","Dialog",loFormSet.lcTDstTr)
        
    llCSave   = .F.      && Do not change mode

    loFormSet.Ariaform1.pbNew.SetFocus()
    RETURN llCSave

  CASE VAL(STRTRAN(loFormSet.Ariaform1.lcAcctcode.Keytextbox.Value,'-',''))=0
    =gfModalGen("TRM02022B00000","DIALOG")  
    llCSave   = .F.

    loFormSet.Ariaform1.lcAcctCode.KeyTextbox.Setfocus()
    RETURN llCSave

   *** Check if total distribution percentage=100
   *** If not,display an appropriate message and exit
  CASE loFormSet.lnDstTotal <>100 
    =gfModalGen("TRM02018B00000","DIALOG",loFormSet.lcTTotals)   
    llCSave   = .F.   
    RETURN llCSave

ENDCASE 

*- End of lfFormBeforeSave.

*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!
*!*******************************************************************************
*    This procedure handles saving,instead of the global procedure.
*    "Save" option corresponds to the ninth position in laDefProc array,
*    hence it should have previously been assigned the value .F.
*    to disable the global saving procedure.
*      The flag llCSave is a global flag that is to have the value
*    of "FALSE" (.F.) if the record(s) is/are not to be saved.
*
PROCEDURE lpSavScr
PARAMETERS loFormSet 

lc_TmpDst = loFormSet.lc_TmpDst
    *** Attempt to lock files
    *** If 'A'dding a new header, attempt to lock 'GLAUTHD', 'GLAUTDT'
    *** files.
    *** If 'E'diting a record, attempt to lock the current record in 
    *** GLAUTHD file (the edited record), and  'GLAUTDT' file.
    *** If adding a new record,append a blank one
    
    *** If adding a new record,append a blank one
    IF loFormSet.ActiveMode = 'A'
      APPEND BLANK
      *=gfObj_Lock(.T.)
    ENDIF

    *** Store laData values in the current record
    lcScFields = loFormSet.lcScFields
    DIMENSION laData[ALEN(loFormSet.laData)]
    ACOPY(loFormSet.laData,laData)
    GATHER FROM laData fields &lcScFields.
    
    gfTableUpdate()

    *** Now save the data in the temporary file using the following global
    *** function which performs correct translation from the temporary
    *** file lc_TmpDst,and the main file GLAUTDT
    =gfTmp2Mast("GLAUTDT",lc_TmpDst,;
                'Saving automatic distribution '+loFormSet.laData[2]+' ...')  

    SELECT GLAUTDT
    gfTableUpdate()
    
    SELECT GLAUTHD
    IF loFormSet.ActiveMode = 'A'
      *=gfObj_Lock(.F.)
    ENDIF


*!**************************************************************************
*!
*!      Procedure: lpDelScr
*!
*!*******************************************************************************
*    This procedure handles deletion,instead of the global procedure.
*    "Delete" option corresponds to the seventh position in laDefProc array,
*    hence it should have previously been assigned the value .F.
*    to disable the global delete procedure.
*
PROCEDURE lpDelScr 
PARAMETERS loFormSet

lc_TmpDst = loFormSet.lc_TmpDst
SELECT GLAUTHD

*** Check if this record is already deleted by another user from
*** a different station. If it is, the record pointer is no longer
*** on the viewed record which is now actually out of scope if 
*** SET('DELETED')='ON'
IF GLAUTHD.cAutType + GLAUTHD.cAutCode <> loFormSet.laData[1]+loFormSet.laData[2] 
  *** If the record is already deleted, present the following message,
  *** and go to 'Select' mode
  *** Message : "
  
  =gfModalGen("TRM00095B00000","ALERT")
  
  *** Go back to 'Select' mode
  loFormSet.ChangeMode('S')
  RETURN .F.
ENDIF  

lnTotRec           = RECCOUNT(lc_TmpDst)+1
loFormSet.lnThermRec         = 0

*** Delete records belonging to the current header from the master
*** file (GLAUTDT)
*** The temporary file lc_TmpDst is zapped in 'Select' mode
SELECT GLAUTDT
SCATTER MEMVAR MEMO BLANK
lcAutDtEx = loFormSet.lcAutDtEx      
REPLACE cAutType  WITH m.cAutType,;
        cAutCode  WITH m.cAutCode,;
        cAcctCode WITH m.cAcctCode ;        
        FOR &lcAutDtEx. = loFormSet.laData[1]+loFormSet.laData[2] .AND. ;
            lfThermo('Deleting automatic distribution '+loFormSet.laData[2]+' ...')
DELETE FOR &lcAutDtEx. = m.cAutType + m.cAutCode 

SELECT GLAUTHD

*** Then delete the header record
SCATTER MEMVAR MEMO BLANK
GATHER MEMVAR MEMO
DELETE 

gfTableUpdate()


*** Return to "SELECT" mode
loFormSet.ChangeMode('S')
SELECT GLAUTHD

*!**************************************************************************
*!
*!      Function : lfThermo
*!
*!*******************************************************************************
*  This function calls global function gfThermo (thermometer)
*  It takes as a parameter a counter to be incremented and tested 
*  at every call. The thermometer is called at increments of 13
*  instead of 1 for faster processing.
*
FUNCTION lfThermo
PARAMETERS lcThermStr

loFormSet.lnThermRec         = loFormSet.lnThermRec + 1



************************************************************
*! Name      : MSG
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/05/2012
*! Purpose   : MSG, can run only on my PC
************************************************************
FUNCTION MSG
PARAMETERS llDoNotUseStep
IF SYS(0)='DEV4 # tarek'
  ON ERROR
  _SCREEN.Visible=.T.
  IF !llDoNotUseStep
    SET STEP ON
  ENDIF 
ENDIF 
*- End of MSG.

