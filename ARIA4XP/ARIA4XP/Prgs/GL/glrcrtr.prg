*:************************************************************************
*:  Program File: ARIA4XP\PRGS\GL\GLRCRTR.PRG
*:  Module      : General Ledger
*:  Desc.       : Recurring Entry screen
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 08/03/2012
*:  Reference   : *E303211,1 
*:************************************************************************
* Modification
*B610082,1 TMI 09/18/2012 [T20120902.0021] Make the description to show up in recurring entry screen
*:************************************************************************

*- Call the screen
lcRunScx = lfGetScx("GL\GLRCRTR.scx")
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

loFormSet.AddProperty('lcProgName','GLRCRTR')

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
  .cBrowseFilter          = "CAUTTYPE='R'"
  .BrowseTitle 		  	    = loFormSet.lcFile_Ttl 
  .ariaBrFields.edtBrowseFields.Value = gfDbfField(.nWorkArea)    
ENDWITH 

*- Define needed variables.
=lfDefineVars(loFormSet)

*- Define input mask for fields
WITH loFormSet.Ariaform1
  .laData2.KeyTextbox.InputMask = REPLICATE('!',FSIZE('CAUTCODE',loFormSet.lcBaseFile))
  .laData3.InputMask = REPLICATE('X',FSIZE('CAUTREF',loFormSet.lcBaseFile))
  .laData4.InputMask = REPLICATE('X',FSIZE('CAUTDES',loFormSet.lcBaseFile))
  .laData8.InputMask = '999'
ENDWITH 


*- Define input mask for fields
WITH loFormSet.Ariaform1
  .laData2.KeyTextBox.ControlSource = 'Thisformset.laData[2]'
  .laData3.ControlSource = 'Thisformset.laData[3]'
  .laData4.ControlSource = 'Thisformset.laData[4]'
  .laData5.ControlSource = 'Thisformset.laData[5]'
  .laData6.ControlSource = 'Thisformset.laData[6]'
  .laData7.ControlSource = 'Thisformset.laData[7]'
  .puDuration.ControlSource = 'ThisFormSet.laData[9]'
  .laData8.ControlSource = 'Thisformset.laData[8]'
  .laData10.ControlSource = 'Thisformset.laData[10]'
  .puSrcJrnl.ControlSource = 'Thisformset.laData[12]'
ENDWITH 

loFormSet.ChangeMode('S')

*- End of lfFormInit.

************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/03/2012
*! Purpose   : Defining variables 
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet

lcScFields = 'CAUTTYPE,CAUTCODE,CAUTREF,CAUTDES,DAUTBGDAT,DAUTNGDAT,DAUTENDAT,NAUTFRQNO,CAUTFRQUN,DAUTLGDAT,CAUTREV,CSRCJRNL'
loFormSet.AddProperty('lcScFields',lcScFields)

lcLn = ALLTRIM(STR(OCCURS(',',lcScFields )+1))
loFormSet.AddProperty('laData[&lcLn]','')
loFormSet.AddProperty('laTDur[2,2]','')
loFormSet.AddProperty('laTSrcJrnl[1,2]','')


*** Variables in screen object file :
loFormSet.laTDur[1,1] = "Periods"
loFormSet.laTDur[1,2] = "P"
loFormSet.laTDur[2,1] = "Days"
loFormSet.laTDur[2,2] = "D"

loFormset.Ariaform1.puDuration.RowSource = 'Thisformset.laTDur'
*X

loFormSet.AddProperty('lcTDfRef'   , 'On')
loFormSet.AddProperty('lcTDfDesc'  , 'Created by')
loFormSet.AddProperty('lcTFirst'   , 'first')
loFormSet.AddProperty('lcTNext'    , 'next')
loFormSet.AddProperty('lcTRcrTr'   , 'recurring transaction|recurring transaction')
loFormSet.AddProperty('lcTDrOrCr'  , 'either debit or credit'   )
loFormSet.AddProperty('lcTRcr'     , 'Recurring')

*** Screen display variables :
loFormSet.AddProperty('lcTDebit'   , 'Debit')
loFormSet.AddProperty('lcTCredit'  , 'Credit')

*** Begin date of previous fiscal year 
loFormSet.AddProperty('ldPreFBgDat', GOMONTH(loFormSet.ldFisBgDat,-12))

*** Variables declaration 
loFormSet.AddProperty('lsRcrDet'    , 1   )       && List variable ( pointer )
loFormSet.AddProperty('puSrcJrnl'   , 1   )   &&*x
loFormSet.AddProperty('puDuration'  , 1   )       && "Duration " popup variable ,
                                && used in WINDOWS version only.  
loFormSet.AddProperty('cbReverse'   , .F. )       && Check box 'Reverse' (initially
                                                  && unchecked)

loFormSet.AddProperty('lcAcctCode' , REPLICATE ("0",loFormSet.lnAcsSegSz)  )
loFormSet.AddProperty('lcOldAcct'  , ''  )       && Old Account number 

loFormSet.AddProperty('lcAccDesc'  , ''  )       && corresponds to field GLAUTDT.cAccnlDes
loFormSet.AddProperty('lcCodeType' , ''  )       && variable to hold the current account type 
loFormSet.AddProperty('lcRStamp'   , ''  )       && Audit information of current record
loFormSet.AddProperty('lcFieldStr' , ''  )       && list field string
loFormSet.AddProperty('lc_TmpRcr'  , ''  )       && temporary file name variable
loFormSet.AddProperty('LCTRCRDT'   , "Recurring details")
loFormSet.AddProperty('lcDuration' , ''  )       && holds duration popup selection.
loFormSet.AddProperty('lcSrcJrnl'  , ''  )       && holds the selection of the S.J popup
loFormSet.AddProperty('lcAutDtTg'  , ''  )       && Tag name of GLAUTDT upon entry.      
loFormSet.AddProperty('lcAutDtEx'  , ''  )       && Tag expression of GLAUTDT current tag 
loFormSet.AddProperty('lnTmpRcCnt' , 0   )       && number of records in th list
loFormSet.AddProperty('lnOldDr'    , 0   )       && old debit value
loFormSet.AddProperty('lnOldCr'    , 0   )       && old credit value
loFormSet.AddProperty('lnOldData8' , 0   )       && old laData[8]
loFormSet.AddProperty('lnDebit'    , 0   )       && variable to accept debit  value
loFormSet.AddProperty('lnCredit'   , 0   )       && variable to accept credit value
loFormSet.AddProperty('lnTotDr'    , 0   )       && total debits  
loFormSet.AddProperty('lnTotCr'    , 0   )       && total credits
loFormSet.AddProperty('lnDSrcJrn'  , 0   )       && Default source journal array index
loFormSet.AddProperty('lnAutDtFld' , 0   )       && number of fields of 'GLAUTDT' file
loFormSet.AddProperty('lnThermRec' , 0   )       && Thermometer counter
loFormSet.AddProperty('ldOldFDate' , {}  )       && old value of first date
loFormSet.AddProperty('ldOldNDate' , {}  )       && old value of next  date        
loFormSet.AddProperty('ldOldLDate' , {}  )       && old value of last  date

loFormSet.AddProperty('llBrowse'   , .F. )          

loFormSet.AddProperty('llNewDet'   , .F. )               

*** Variables hold the status of the objects. ***
loFormSet.AddProperty('lcAct_Stat' , ""  )    
loFormSet.AddProperty('lcDeb_Stat' , ""  )   
loFormSet.AddProperty('lcCrd_Stat' , ""  )      
loFormSet.AddProperty('lcNew_Stat' , ""  )     
loFormSet.AddProperty('lcRem_Stat' , ""  )     
loFormSet.AddProperty('lcAdj_Stat' , ""  )    

*B610082,1 TMI 09/18/2012 [Start] get the country related currency
*loFormSet.AddProperty('lcCurrSign' , '$' )       && currency sign
loFormSet.AddProperty('lcCurrSign' , gfGetCurSmbl(oAriaApplication.BaseCurrency) )       && currency sign
*B610082,1 TMI 09/18/2012 [End  ] 
lcCurDate = IIF(SET('CENTURY')='ON',DTOC(oAriaApplication.SystemDate),;
                 LEFT(DTOC(oAriaApplication.SystemDate),6)+STR(YEAR(oAriaApplication.SystemDate),4))
loFormSet.AddProperty('lcCurDate'  ,lcCurDate)

SELECT (loFormSet.lcBaseFile)
lcScFields = loFormSet.lcScFields
SCATTER FIELDS &lcScFields TO loFormSet.laData BLANK
loFormSet.laData[1]          = 'R'
*** Create a name for the temporary file
loFormSet.lc_TmpRcr        = gfTempName()

SELECT GLAUTDT

loFormSet.lcAutDtTg       = SYS(22)

SET ORDER TO TAG TYPCODACC

*** Current tag expression is cAutType + cAutCode + cAcctCode
loFormSet.lcAutDtEx       = SYS(14,VAL(SYS(21)))
 *** Create the temporary file with the appropriate fields , 
***(in this case all fields of file "GLAUTDT" + 2 more fields
*** for the record number and status('M'odify,'D'elete,'A'dd,'S'ame)
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
lc_TmpRcr = loFormSet.lc_TmpRcr
CREATE TABLE &gcWorkDir.&lc_TmpRcr;
      FROM ARRAY laAutDtFld

*** Prepare lcFieldStr for the list 
SELECT (lc_TmpRcr)      

*** Create source journal array for the source journals popup
=lfSrcJrnl(loFormSet)

loFormSet.puDuration = 1
loFormSet.lcDuration = loFormSet.laTDur[1,1]
loFormSet.laData[9]  = "P"
loFormSet.laData[11] = "N"
  
loFormSet.puSrcJrnl  = loFormSet.lnDSrcJrn
loFormSet.laData[12] = loFormSet.lcSJ_Def


SELECT GLAUTHD
SET FILTER TO
*** Filter on recurring transactions records only
SET FILTER TO CAUTTYPE = "R"
LOCATE FOR CAUTCODE = loFormSet.laData[2]

loFormSet.cbReverse = IIF(loFormSet.laData[11] = 'Y' , .T. , .F.)

lcVars = 'lcAct_Stat,lcDeb_Stat,lcCrd_Stat,lcNew_Stat,lcRem_Stat,lcAdj_Stat'
=lfAddProp(loFormSet,lcVars,'DISABLE')
 
*- End of lfDefineVars.
************************************************************
*! Name      : lfBtnStates
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/06/2012
*! Purpose   : Detail screen btn states
************************************************************
FUNCTION lfBtnStates
PARAMETERS loDetFormSet
loFormSet = loDetFormSet.loFormSet
WITH loDetFormSet.Ariaform1
IF loFormSet.ActiveMode='S' .OR. loFormSet.ActiveMode='V' 
  .lcAcctcode.Enabled = .F.
  .lnDebit.Enabled    = .F.
  .lnCredit.Enabled   = .F.
  .pbNew.Enabled      = .F.
  .pbRem.Enabled      = .F.
  .pbAdj.Enabled      = .F.
ELSE
  .lcAcctcode.Enabled = IIF(loFormSet.lnTmpRcCnt = 0 , .F. , .T.)
  .lnDebit.Enabled    = IIF(loFormSet.lnTmpRcCnt = 0 , .F. , .T.)
  .lnCredit.Enabled   = IIF(loFormSet.lnTmpRcCnt = 0 , .F. , .T.)
  .pbNew.Enabled      = IIF(loFormSet.lnTmpRcCnt > 0 , IIF(loFormSet.llNewDet , .F. , .T.) , .T.)
  .pbRem.Enabled      = IIF(loFormSet.lnTmpRcCnt = 0 , .F. , .T.)
  .pbAdj.Enabled      = IIF(loFormSet.lnTotDr <> loFormSet.lnTotCr .AND. loFormSet.lnTmpRcCnt > 1 ,.T.,.F.)
ENDIF 
ENDWITH 

*- End of lfBtnStates.

************************************************************
*! Name      : lfFormdestroy
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/03/2012
*! Purpose   : Destroy form
************************************************************
FUNCTION lfFormdestroy
PARAMETERS loFormSet

lc_TmpRcr = loFormSet.lc_TmpRcr
gcWorkDir = oAriaApplication.WorkDir

  *** reset tag of GLAUTDT
SELECT GLAUTDT
IF !EMPTY(loFormSet.lcAutDtTg)
  SET ORDER TO TAG (loFormSet.lcAutDtTg)
ELSE
  SET ORDER TO
ENDIF

*** Close open temporary files.  
IF USED(lc_TmpRcr)
  USE IN ALIAS(lc_TmpRcr)
ENDIF
ERASE &gcWorkDir.&lc_TmpRcr..dbf  
*- End of lfFormdestroy.


************************************************************
*! Name      : lfFormActivate
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/05/2012
*! Purpose   : Activate ariaform1
************************************************************
FUNCTION lfFormActivate
PARAMETERS loFormSet

*- End of lfFormActivate.

************************************************************
*! Name      : lfvEntries
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/05/2012
*! Purpose   : Run the entries screen
************************************************************
FUNCTION lfvEntries
PARAMETERS loFormSet

lcRunScx = lfGetScx("GL\GLRCRDT.scx")
DO FORM (lcRunScx) WITH loFormSet

*- End of lfvEntries.
************************************************************
*! Name      : lfFormDetailInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/05/2012
*! Purpose   : Init method for detail screen
************************************************************
FUNCTION lfFormDetailInit
PARAMETERS loDetFormSet
loFormSet = loDetFormSet.loFormSet
loDetFormSet.Ariaform1.lcAcSegDes.Caption = loFormSet.lcAcSegDes

loDetFormSet.Ariaform1.Caption = loFormSet.LCTRCRDT
WITH loDetFormSet.Ariaform1.grdGLAUTHD
  .Column1.Header1.Caption = ALLTRIM(loFormSet.lcAcSegDes)
  .Column2.Header1.Caption = 'Description'
  .Column3.Header1.Caption = PADL(loFormSet.lcTDebit + ' '+loFormSet.lcCurrSign,15)
  .Column4.Header1.Caption = PADL(loFormSet.lcTCredit+ ' '+loFormSet.lcCurrSign,15)
  .Refresh()
ENDWITH 

lc_TmpRcr = loFormSet.lc_TmpRcr
WITH loDetFormSet.Ariaform1.grdGLAUTHD
  .RecordSource = loFormSet.lc_TmpRcr
  .Column1.ControlSource = "&lc_TmpRcr..cAcctcode"
  .Column2.ControlSource = "LOOKUP(GLACCHAR.cAccnldes,&lc_TmpRcr..cAcctcode,GLACCHAR.cAcctcode,'ACCTCODE')"
  .Column3.ControlSource = "IIF(&lc_TmpRcr..cDrOrCr='D',&lc_TmpRcr..nAmount,0.00)"
  .Column4.ControlSource = "IIF(&lc_TmpRcr..cDrOrCr='D',0.00,&lc_TmpRcr..nAmount)"
ENDWITH

loDetFormSet.Ariaform1.lnDebit.InputMask = REPLICATE('9',FSIZE('NAMOUNT','GLAUTDT')-3)+'.99'
loDetFormSet.Ariaform1.lnCredit.InputMask = REPLICATE('9',FSIZE('NAMOUNT','GLAUTDT')-3)+'.99'

=lfFormAfterRowColumnChange(loDetFormSet)

*- Enable / Disable controls
lfBtnStates(loDetFormSet)

*- End of lfFormDetailInit.


************************************************************
*! Name      : lfFormAfterRowColumnChange
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/25/2012
*! Purpose   : adjust data value when line changes in the Grid
************************************************************
FUNCTION lfFormAfterRowColumnChange
PARAMETERS loDetFormSet
loFormSet = loDetFormSet.loFormSet
lc_TmpRcr = loFormSet.lc_TmpRcr
WITH loDetFormSet.Ariaform1
  .lnDebit.Value  = IIF(&lc_TmpRcr..cDrOrCr='D',&lc_TmpRcr..nAmount,0.00)
  .lnCredit.Value = IIF(&lc_TmpRcr..cDrOrCr='D',0.00,&lc_TmpRcr..nAmount)
  
  .lcAcctcode.KEYTEXTBOX.Value = IIF(!EMPTY(&lc_TmpRcr..CACCTCODE),&lc_TmpRcr..CACCTCODE,REPLICATE ("0",loFormSet.lnAcsSegSz))
  .lcAccDesc.Value = LOOKUP(GLACCHAR.cAccnldes,&lc_TmpRcr..cAcctcode,GLACCHAR.cAcctcode,'ACCTCODE')
  *E303214,4 TMI 08/23/2012 [Start] refresh txtSummary field
  .txtSummary.Value = IIF(loFormSet.lnTmpRcCnt>0,"Totals :"+' '+STR(loFormSet.lnTotDr,15,2)+' '+STR(loFormSet.lnTotCr,15,2),'') 
  *E303214,4 TMI 08/23/2012 [End  ] 
ENDWITH 


*- End of lfFormAfterRowColumnChange.

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

lc_TmpRcr = loFormSet.lc_TmpRcr
gcWorkDir = oAriaApplication.WorkDir
gcDataDir = oAriaApplication.DataDir


loFormSet.laData[1]          = 'R'
DO CASE

  *** "Select" mode (loFormSet.ActiveMode='S'=.T.)
  CASE loFormSet.ActiveMode='S'
    SELECT (lc_TmpRcr)
    loFormSet.lsRcrDet  = 1
    SHOW GET lsRcrDet  ENABLE
    
    *** Delete old data ( if any )
    ZAP
    
    *** Initialize variables for display
    loFormSet.lnTmpRcCnt = 0    && No nonempty records in the temporary file
    
    *** Reset defaults
    loFormSet.laData[9]  = "P"
    loFormSet.laData[11] = "N"
    loFormSet.laData[12] = loFormSet.lcSJ_Def
    
    loFormSet.cbReverse  = .F.
    loFormSet.puDuration = 1
    *loFormSet.puSrcJrnl  = lnDSrcJrn
    
    loFormSet.lcDuration = loFormSet.laTDur[1,1]         && 'Periods'
    loFormSet.Ariaform1.puSrcJrnl.Value = loFormSet.laTSrcJrnl[1,2]
    
    loFormSet.lcAcctCode = REPLICATE ("0",loFormSet.lnAcsSegSz)  
    lcAccDesc  = SPACE(60)
    lcOldAcct  = ''
        
    lnCredit   = 0   
    lnDebit    = 0
    loFormSet.lnTotDr    = 0
    loFormSet.lnTotCr    = 0
    lnOldDr    = 0
    lnOldCr    = 0

    loFormSet.Ariaform1.laData5.Enabled = .F.
    loFormSet.Ariaform1.laData6.Enabled = .F.
    
    SHOW GET lcAcctCode  DISABLE
    SHOW GET ibActBrow   DISABLE 
    SHOW GET lnDebit     DISABLE 
    SHOW GET lnCredit    DISABLE 
    
    *** Controls
    SHOW GET pbNew       DISABLE 
    SHOW GET pbRem       DISABLE 
    SHOW GET pbAdj       DISABLE
    SHOW GET pbEntry     DISABLE 
    
    
    loFormset.Ariaform1.laData2.Enabled = .T.
    loFormset.Ariaform1.laData2.Keytextbox.SetFocus()
    
    
  *** "View" mode (loFormSet.ActiveMode='V'=.T.) ,or "Edit" mode (loFormSet.ActiveMode='E'=.T.)
  CASE loFormSet.ActiveMode='V'.OR.loFormSet.ActiveMode='E' 
  
    IF loFormSet.ActiveMode = 'V'
      SELECT (loFormSet.lcBaseFile)
      lcScFields = loFormSet.lcScFields
      SCATTER FIELDS &lcScFields TO loFormSet.laData
      loFormset.Ariaform1.CAUTREV.Value = CAUTREV='Y'
    ENDIF 
  
    lnDebit    = 0
    lnCredit   = 0
    llFirstRec = .T.
    
    loFormSet.lcRStamp       = IIF(loFormSet.ActiveMode='V', GLAUTHD.cAdd_User+DTOC(GLAUTHD.dAdd_Date);
                        +GLAUTHD.cAdd_Time,loFormSet.lcRStamp)
        
    IF loFormSet.ActiveMode='V' .OR. loFormSet.lcRStamp <> GLAUTHD.cAdd_User+DTOC(GLAUTHD.dAdd_Date);
                        +GLAUTHD.cAdd_Time

      *** The file is selected for lfTotals()   
      SELECT GLAUTDT  
      loFormSet.lnTotDr   = 0
      loFormSet.lnTotCr   = 0
      lcAutDtEx = loFormSet.lcAutDtEx
      SELECT *,RECNO() AS 'nRecNo',	"S" AS 'cStatus';
             FROM &gcDataDir.GLAUTDT;
             INTO DBF &gcWorkDir.&lc_TmpRcr;
             WHERE &lcAutDtEx. = "R"+loFormSet.laData[2];
             .AND. lfTotals()
    ENDIF         
           
    SELECT (lc_TmpRcr)

    *** Get the type of accounts
    IF loFormSet.ActiveMode='E'
      loFormSet.lcCodeType = IIF(LEFT(LOOKUP(GLACCHAR.cTypeCode,&lc_TmpRcr..cAcctCode,;
                       GLACCHAR.cAcctCode,'ACCTCODE'),1)="Y","S","T")
    ENDIF                             

    *** Get the number of records currently in the temporary file
    loFormSet.lnTmpRcCnt = RECCOUNT(lc_TmpRcr)

    *** Considering the fact that there has to be at least one record
    *** in the temporary file,( due to the fact that we cannot "Save"
    *** an empty recurring trasnsaction,there 
    *** seems to be no reason for checking for the number of
    *** records in the file,or to initialize variables to spaces,hence,
    
    loFormSet.lcAcctCode = cAcctCode
    lcAccDesc  = LOOKUP(GLACCHAR.cAccnlDes,&lc_TmpRcr..cAcctCode,;
                        GLACCHAR.cAcctCode,'ACCTCODE')
    IF cDrorCr="D"
      lnDebit   = nAmount
    ELSE
      lnCredit  = nAmount
    ENDIF
    
    loFormSet.cbReverse = IIF(loFormSet.laData[11] = 'Y' , .T. , .F.)
    
    *** Prepare screen objects
    lcObjState = IIF(loFormSet.ActiveMode='E',"ENABLE","DISABLE")
    SHOW GET pbRem      &lcObjState.
    SHOW GET pbNew      &lcObjState.
    
    SHOW GET pbEntry    ENABLE
    
    SHOW GET lcAcctCode &lcObjState.
    SHOW GET ibActBrow  &lcObjState.
    SHOW GET lnDebit    &lcObjState.
    SHOW GET lnCredit   &lcObjState.

    *** If returning to 'View' mode from 'Edit' mode with the Adjust
    *** button enabled,we'd like to disable it.
    IF loFormSet.ActiveMode='V'
      SHOW GET pbAdj   DISABLE    
    ENDIF   
    
    *** Disable error handler until the list is refreshed,then
    *** Enable it again
    lcErrSett      = ON("ERROR")
    ON ERROR lnDum = 1
    SHOW GET lsRcrDet     ENABLE
    ON ERROR &lcErrSett.

    IF loFormSet.ActiveMode='V'
      lcData5St  = 'DISABLE'
      lcData6St  = 'DISABLE'
    ELSE
      lcData5St  = 'DISABLE'
      lcData6St  = 'ENABLE'
    ENDIF

    loFormSet.Ariaform1.laData5.Enabled = lcData5St = 'ENABLE'
    loFormSet.Ariaform1.laData6.Enabled = lcData6St = 'ENABLE'
    loformset.Ariaform1.pbEntry.Enabled = .T.
    

  *** "Add" mode (loFormSet.ActiveMode='A'=.T.) 
  CASE loFormSet.ActiveMode='A' 
    *** Prepare defaults :
    *** Remember that lcCurDate's length depends upon the century setting.

    loFormSet.Ariaform1.laData5.Enabled = .T.
    loFormSet.Ariaform1.laData6.Enabled = .F.

    lcDfRef    = loFormSet.lcTDfRef+' '+loFormSet.lcCurDate
    loFormSet.laData[3]  = lcDfRef+SPACE(FSIZE('cAutRef','GLAUTHD')-LEN(lcDfRef))  
    loFormSet.laData[4]  = SUBSTR(loFormSet.lcTDfDesc+' '+oAriaApplication.User_ID,1,;     
                        FSIZE('cAutDes','GLAUTHD'))
    loFormSet.laData[5]  = loFormSet.ldFisBgDat 
    loFormSet.laData[6]  = loFormSet.ldFisBgDat 
    loFormSet.laData[7]  = loFormSet.ldFisEnDat
    loFormSet.laData[8]  = 1 

    SHOW GET pbRem       DISABLE
    SHOW GET pbAdj       DISABLE
    SHOW GET pbNew       ENABLE
    SHOW GET pbEntry     ENABLE
    SHOW GET lsRcrDet   
    
ENDCASE

loFormSet.CheckNavigation()
loFormSet.OToolbar.NAVrefresh()
loFormSet.Ariaform1.Refresh()

SELECT GLAUTHD 

*!**************************************************************************
*!
*!      Function : lfTotals
*!
*!**************************************************************************
* This function gets the total debits and credits,called
* while SELECT_SQL command is executed.
*
FUNCTION lfTotals

*** Skip first record (SELECT SQL counts it twice),first time
IF llFirstRec
  llFirstRec = .F.
ELSE
  IF cDrOrCr ="D"
    loFormSet.lnTotdr   = loFormSet.lnTotdr  + nAmount
  ELSE
    loFormSet.lnTotcr   = loFormSet.lnTotcr  + nAmount
  ENDIF
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
*!      Function: lfvSrcJrnl
*!
*!**************************************************************************
*
FUNCTION lfvSrcJrnl
PARAMETERS loFormSet,loFld

*=lfSrcJrnl(loFormSet)

*loFormSet.laData[12] = loFormSet.laTSrcJrnl[loFormSet.puSrcJrnl,2]

*!**************************************************************************
*!
*!      Function: lfvData_5
*!
*!**************************************************************************
*    VALID function for the get field "laData[5]"
*    It corresponds to the first transaction date 
*    This field is editable only in ADD mode ( loFormSet.ActiveMode='A' )
FUNCTION lfvData_5
PARAMETERS loFormSet,loFld

loFormSet.Ariaform1.Refresh()
*** If the first date is less than the start date of the previous
*** fiscal year,do not accept the entry,else use the same entry
*** for the next transaction date, which is disabled in this mode.
IF loFormSet.laData[5] < loFormSet.ldPreFBgDat 
  =gfModalGen("TRM02057B00000","DIALOG",loFormSet.lcTFirst)  
  loFld.Value = loFld.OldValue
  RETURN .F.
ELSE
  *** If first date is greater than last transacrion date,do not accept it
  IF loFormSet.laData[5] > loFormSet.laData[7]
    =gfModalGen("TRM02058B00000","DIALOG",loFormSet.lcTFirst)  
    loFld.Value = loFld.OldValue
    RETURN .F.
  ELSE
    loFormSet.laData[6] = loFormSet.laData[5]
*    SHOW GET laData[6]  *x
  ENDIF
ENDIF
loFormSet.Ariaform1.Refresh()

*!**************************************************************************
*!
*!      Function: lfvData_6
*!
*!**************************************************************************
*    VALID function for the get field "laData[6]" 
*    It corresponds to the next transaction date 
*    This field is editable only in EDIT mode ( loFormSet.ActiveMode='E' )

FUNCTION lfvData_6
PARAMETERS loFormSet,loFld

*** If the next date is less than the start date of the previous
*** fiscal year,do not accept the entry,else use the same entry
*** for the next transaction date, which is disabled in this mode.
IF loFormSet.laData[6] < loFormSet.ldPreFBgDat
  =gfModalGen("TRM02057B00000","DIALOG",loFormSet.lcTNext)  
  loFld.Value = loFld.OldValue
  RETURN .F.  
ELSE
  *** If next date is greater than last transacrion date,do not accept it
  IF loFormSet.laData[6] > loFormSet.laData[7]
    =gfModalGen("TRM02058B00000","DIALOG",loFormSet.lcTNext)  
    loFld.Value = loFld.OldValue
    RETURN .F.  
  ELSE
    *** If next transaction date is less than the first transaction date,
    *** Copy the new value to the first transaction date. 
    IF loFormSet.laData[6] < loFormSet.laData[5]
      loFormSet.laData[5] = loFormSet.laData[6]
*      SHOW GET laData[5]  *x
    ENDIF  
  ENDIF
ENDIF
loFormSet.Ariaform1.Refresh()
*!**************************************************************************
*!
*!      Function: lfvData_7
*!
*!**************************************************************************
*    VALID function for the get field "laData[7]"
*    It corresponds to the last transaction date 
*
FUNCTION lfvData_7
PARAMETERS loFormSet,loFld

*** If the last date is less than the next transaction date, 
*** do not accept the entry,
IF loFormSet.laData[7] < loFormSet.laData[6] 
  =gfModalGen("TRM02058B00000","DIALOG",IIF(loFormSet.ActiveMode='A',loFormSet.lcTFirst,loFormSet.lcTNext))  
  loFld.Value = loFld.OldValue
  RETURN .F.
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_8
*!
*!**************************************************************************
*    VALID function for the get field "laData[8]"
*    It corresponds to field nAutFrqNo
*
FUNCTION lfvData_8
PARAMETERS loFormSet,loFld

*** Reject negative or zero entries
IF loFormSet.laData[8] <= 0
  =gfModalGen(IIF(loFormSet.laData[8]<0,"TRM02036B00000","TRM02037B00000"),"DIALOG")
  loFld.Value = loFld.OldValue
  RETURN .F.
ENDIF

*!**************************************************************************
*!
*!      Function: lfvReverse
*!
*!**************************************************************************
*    VALID function for the check box 'Reverse ( cbReverse )
*    It corresponds to field cAutRev ( laData[11] ) 
*
FUNCTION lfvReverse
PARAMETERS loFormSet,loFld

loFormSet.laData[11] = IIF(loFld.Value,'Y','N')

*!**************************************************************************
*!
*!      Function: lfvDur
*!
*!**************************************************************************
*
FUNCTION lfvDur
PARAMETERS loFormSet,loFld

*loFormSet.laData[9] = SUBSTR('PD',loFormSet.puDuration,1)


*!**************************************************************************
*!
*!      Function: lfvAccCode
*!
*!**************************************************************************
*  Valid function for the field lcAcctcode
* 
FUNCTION lfvAccCode
PARAMETERS loDetFormSet,loFld
loFormSet = loDetFormSet.loFormSet

*E303214,1 TMI 08/21/2012 [Start] if came from browse button, add '?' sign
IF loFld.Selectedfrombrowse = .T.
  loFld.KeyTextbox.Value = STUFF(loFld.KeyTextbox.Value,1,1,'?')
ENDIF
*E303214,1 TMI 08/21/2012 [End  ] 

lc_TmpRcr = loFormSet.lc_TmpRcr

PRIVATE lcCodeT
loFormSet.lcAcctCode = loFld.Keytextbox.Value
IF LEFT(LTRIM(loFormSet.lcAcctCode),1) <> '?'.AND. ;
   !ISDIGIT(LTRIM(loFormSet.lcAcctCode))
  =gfModalGen("TRM02061B00000","Dialog")
  loFld.KeyTextbox.Value = loFld.KeyTextbox.OldValue
  RETURN .F.
ENDIF

   
  *** This condition is true only if the account code had an old entry
  *** and now it is emptied,just ignore the entry.
  IF LEFT(LTRIM(loFormSet.lcAcctCode),1)<>'?'.AND.VAL(STRTRAN(loFormSet.lcAcctCode,'-','')) = 0 
    loFld.Keytextbox.Value = loFld.Keytextbox.OldValue
  ELSE
    lcCodeT    = ''
    *** If there is only one record in the temporary file,
    *** The Account Type may be changed.
    *** This condition applies for the following cases :
    *** a. A NEW entry ( Add mode, or Edit mode) in an empty list.
    *** b. Removing all records except one.
    *** c. Removing all records ( handled from NEW )
    IF loFormSet.lnTmpRcCnt = 1
      loFormSet.lcCodeType = "A"
    ENDIF
    lcAccDesc = loDetFormSet.Ariaform1.lcAccDesc.Value
    IF lfVldAccnt(loFormSet,loFld,loFormSet.lcCodeType,"C","L",.T.,@lcAccDesc,@lcCodeT,"") AND !EMPTY(CHRTRAN(loDetFormSet.Ariaform1.lcAcctCode.Keytextbox.Value,'0-',''))
      *B610082,1 TMI 09/18/2012 [Start] Get the selected account description
      *loDetFormSet.Ariaform1.lcAccDesc.Value = lcAccDesc
      loDetFormSet.Ariaform1.lcAccDesc.Value = ALLTRIM(LCLINKCHAR.caccnldes)
      *B610082,1 TMI 09/18/2012 [End  ] 
      loFormSet.lcAcctCode = loDetFormSet.Ariaform1.lcAcctCode.Keytextbox.Value
      IF loFormSet.lcCodeType = "A"
        loFormSet.lcCodeType = IIF(LEFT(lcCodeT,1)="Y","S","T")
      ENDIF    

      SELECT (lc_TmpRcr)

      *** If previously modified,"M---->M"
      *** If a new entry,        "A---->A"
      *** else                   "S---->M"       
      lcStatus = SUBSTR("MAM",AT(cStatus,"MAS"),1)

      REPLACE cAcctcode WITH loFormSet.lcAcctCode ,;
              cStatus   WITH lcStatus

      loDetFormSet.Ariaform1.Refresh()      

      SELECT GLAUTHD
  
      *** Enable "New" push button (pbNew) only if either a debit or a credit
      *** value is entered because in this screen, an entry is not accepted
      *** unless either of the debit or credit fields has a value greater than zero.
      IF loDetFormSet.Ariaform1.lnDebit.Value > 0 .OR. loDetFormSet.Ariaform1.lnCredit.Value > 0
        loFormSet.llNewDet   = .F.
        loDetFormSet.Ariaform1.pbNew.Enabled = .T. 
      ELSE
        loDetFormSet.Ariaform1.lnDebit.Enabled = .T. 
        loDetFormSet.Ariaform1.lnCredit.Enabled = .T. 
      ENDIF  

      lcObjState   = IIF(loFormSet.lnTotDr<>loFormSet.lnTotCr .AND. loFormSet.lnTmpRcCnt > 1,;
                         "ENABLE","DISABLE")
      SHOW GET pbAdj &lcObjState
    ELSE
      loFld.Keytextbox.Value = loFld.Keytextbox.OldValue
      RETURN .F.
    ENDIF
  ENDIF

*!**************************************************************************
*!
*!      Function:  lfvDebit
*!
*!
*!**************************************************************************
*    VALID function for get field "lnDebit".
*    lnTotDr field is used for totals
*   
FUNCTION lfvDebit
PARAMETERS loDetFormSet,loFld
loFormSet=loDetFormSet.loFormSet
lc_TmpRcr = loFormSet.lc_TmpRcr
IF loFld.Value<>loFld.OldValue    
  *** Reject negative entries
  IF loFld.Value<0
    =gfModalGen("TRM02036B00000","DIALOG")
    loFld.Value = loFld.OldValue
    RETURN .F.
  ELSE 

    SELECT (lc_TmpRcr)

    *** If the record has been previously saved as a credit,
    *** and now there is a debit entry,
    IF loFld.Value > 0 .AND. cDrOrCr = "C"
      *** Ignore the credit entry,blank it and adjust Credit totals. 
      loDetFormSet.Ariaform1.lnCredit.Value  = 0
      loFormSet.lnTotCr   = loFormSet.lnTotCr - nAmount  
      loFormSet.lnTotDr   = loFormSet.lnTotDr + loFld.Value
    ELSE 
      loFormSet.lnTotDr   = loFormSet.lnTotDr-nAmount+loFld.Value
    ENDIF
    lcStatus    = SUBSTR("MAM",AT(cStatus,"MAS"),1)
    REPLACE nAmount  WITH loFld.Value ;
            cStatus  WITH lcStatus;
            cDrOrCr  WITH "D" 

    SELECT GLAUTHD
    
    *** Adjust controls
    lcObjState = IIF(loFormSet.lnTotDr <> loFormSet.lnTotCr .AND. loFormSet.lnTmpRcCnt > 1 ,;
                     "ENABLE","DISABLE")
    loDetFormSet.Ariaform1.pbAdj.Enabled = lcObjState = 'ENABLE'
  ENDIF
ENDIF

lcObjState = IIF(VAL(STRTRAN(loFormSet.lcAcctCode,'-','')) > 0 ;
                .AND.(loFld.Value>0 .OR. loDetFormSet.Ariaform1.lnCredit.Value>0),"ENABLE","DISABLE")
*B600380,1 Reham On 06/05/95
*B600380,1 This flag to know the status of the <New> button.
loFormSet.llNewDet = IIF(VAL(STRTRAN(loFormSet.lcAcctCode,'-','')) > 0 ;
                .AND.(loFld.Value>0 .OR. loDetFormSet.Ariaform1.lnCredit.Value>0),.F.,.T.)
loDetFormSet.Ariaform1.pbNew.Enabled = lcObjState = 'ENABLE'

*E303214,4 TMI 08/23/2012 [Start] refresh
=lfFormAfterRowColumnChange(loDetFormSet)
*E303214,4 TMI 08/23/2012 [End  ] 

*!**************************************************************************
*!
*!      Function:  lfvCredit
*!
*!
*!**************************************************************************
* VALID function for get field "lnCredit".
* lnTotCr field is used for totals
*   
FUNCTION lfvCredit
PARAMETERS loDetFormSet,loFld
loFormSet=loDetFormSet.loFormSet
lc_TmpRcr = loFormSet.lc_TmpRcr

IF loFld.Value <> loFld.OldValue
  *** Reject negative entries
  IF loFld.Value  < 0
    =gfModalGen("TRM02036B00000","DIALOG") 
    loFld.Value = loFld.OldValue 
    RETURN .F.
  ELSE 
    SELECT (lc_TmpRcr)
    
    IF loFld.Value  > 0 .AND. cDrOrCr = "D"
        *** If there is a Debit entry for the same record,ignore it,
        *** blank it and adjust Debit totals. 
        loDetFormSet.Ariaform1.lnDebit.Value   = 0
        loFormSet.lnTotDr   = loFormSet.lnTotDr - nAmount  
        loFormSet.lnTotCr   = loFormSet.lnTotCr + loFld.Value
    ELSE
        loFormSet.lnTotCr = loFormSet.lnTotCr - nAmount + loFld.Value  
    ENDIF
    lcStatus    = SUBSTR("MAM",AT(cStatus,"MAS"),1)
    REPLACE nAmount  WITH loFld.Value ;
            cStatus  WITH lcStatus;
            cDrOrCr  WITH "C" 
    
    *** Adjust controls
    lcObjState = IIF(loFormSet.lnTotDr <> loFormSet.lnTotCr .AND. loFormSet.lnTmpRcCnt > 1,;
                     "ENABLE","DISABLE")
    loDetFormSet.Ariaform1.pbAdj.Enabled = lcObjState = 'ENABLE'
    
    SELECT GLAUTHD
  ENDIF
ENDIF

loDbr = loDetFormSet.Ariaform1.lnDebit
loCdr = loDetFormSet.Ariaform1.lnCredit

lcObjState = IIF(VAL(STRTRAN(loFormSet.lcAcctCode,'-','')) > 0 ;
                 .AND.(loDbr.Value>0 .OR. loCdr.Value>0),"ENABLE","DISABLE")
*B600380,1 Reham On 06/05/95
*B600380,1 This flag to know the status of the <New> button.
loFormSet.llNewDet = IIF(VAL(STRTRAN(loFormSet.lcAcctCode,'-','')) > 0 ;
               .AND.(loDetFormSet.Ariaform1.lnDebit.Value>0 .OR. loFld.Value >0),.F.,.T.)
loDetFormSet.Ariaform1.pbNew.Enabled = lcObjState = 'ENABLE'

*E303214,4 TMI 08/23/2012 [Start] refresh
=lfFormAfterRowColumnChange(loDetFormSet)
*E303214,4 TMI 08/23/2012 [End  ] 


*!**************************************************************************
*!
*!      Function: lfvNew
*!
*!**************************************************************************
* VALID function for push button "New" (pbNew).
*
FUNCTION lfvNew
PARAMETERS loDetFormSet
loFormSet = loDetFormSet.loFormSet

*** Are there any empty records in the file?
*** If there are,find them and replace them with the new values
*** else Insert a new record and prepare it to be filled  the user ,
*** initializing it with type ("D"),distribution code and status="A" 
*** ( for addition )

lc_TmpRcr = loFormSet.lc_TmpRcr
gcWorkDir = oAriaApplication.WorkDir
gcDataDir = oAriaApplication.DataDir

SELECT (lc_TmpRcr)

LOCATE FOR EMPTY(cAutType)
IF FOUND()
  REPLACE cAutType WITH loFormSet.laData[1],;
          cAutCode WITH loFormSet.laData[2],;   
          cStatus  WITH 'A'
ELSE
  INSERT INTO &gcWorkDir.&lc_TmpRcr.;
              (cAutType,cAutCode,cStatus);
         VALUES (loFormSet.laData[1],loFormSet.laData[2],"A")      
ENDIF

*** Add Audit Information to the newly created record
=gfAdd_Info(lc_TmpRcr)

*** The following fields are blanked,waiting for an entry
*** When they are entered,their valid functions take care of their saving 
loFormSet.lcAcctCode = REPLICATE ("0",loFormSet.lnAcsSegSz)  
loDetFormSet.Ariaform1.lcAcctCode.Keytextbox.Value = loFormSet.lcAcctCode
loDetFormSet.Ariaform1.lcAccDesc.Value  = SPACE(60)

loDetFormSet.Ariaform1.lnDebit.Value    = 0
loDetFormSet.Ariaform1.lnCredit.Value   = 0

*** Increase number of records in temporary file
loFormSet.lnTmpRcCnt = loFormSet.lnTmpRcCnt + 1

*** Select the new record from the list

loDetFormSet.Ariaform1.lcAcctCode.Enabled = .T.

*** Disable numeric fields until an account is entered.
loDetFormSet.Ariaform1.lnDebit.Enabled = .F.   &&    DISABLE
loDetFormSet.Ariaform1.lnCredit.Enabled = .F.  &&    DISABLE
loDetFormSet.Ariaform1.pbAdj.Enabled = .F.     &&    DISABLE


IF loFormSet.lnTmpRcCnt > 0
  loDetFormSet.Ariaform1.pbRem.Enabled = .T.     &&     ENABLE
ENDIF

*** Disable New button until a valid account is ebtered
loDetFormSet.Ariaform1.pbNew.Enabled = .F.     &&    DISABLE

*B600380,1 Reham On 06/05/95
*B600380,1 This flag is added to know that there is a new detail line
*B600380,1 added with an empty account.
loFormSet.llNewDet = .T.

*** Prepare the user for entry by moving the cursor
*** (activating object) to the cAcctCode field (lcAcctCode object) 
loDetFormSet.Ariaform1.Refresh()
loDetFormSet.Ariaform1.lcAcctCode.Keytextbox.SetFocus()
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
PARAMETERS loDetformset
loFormSet = loDetformset.loFormSet 
lc_TmpRcr = loFormSet.lc_TmpRcr

loDbr = loDetFormSet.Ariaform1.lnDebit
loCdr = loDetFormSet.Ariaform1.lnCredit

*** Confirm Removing of the record
IF gfModalGen("QRM00007B00007","ALERT") = 1
 
  SELECT (lc_TmpRcr)

  *** If the record is previously modified,"M---->D"
  ***   delete it.
  *** If it is a new entry                 "A---->S"
  ***   skip it when saving
  *** else (a "Same" record )              "S---->D" 
  ***   delete it
  REPLACE cStatus WITH SUBSTR("DSD",AT(cStatus,"MAS"),1)

  *** Decrement number of records in list
  loFormSet.lnTmpRcCnt = loFormSet.lnTmpRcCnt - 1
  
  *** Adjust totals
  IF cDrOrCr = "D" 
    loFormSet.lnTotDr = loFormSet.lnTotDr - nAmount
  ELSE 
    loFormSet.lnTotCr = loFormSet.lnTotCr - nAmount
  ENDIF
  
  *** Delete the current record (to be removed )
  *** If the removed record is the last one,go top 
  DELETE
  *** Check if you have to go to next record or the top one
  SKIP 
  IF EOF(lc_TmpRcr)
    GO TOP
  ENDIF  
 
  *** Refresh objects with contents of the current record,or spaces
  *** if the list is empty
  loDetFormSet.Ariaform1.lcAcctCode.Keytextbox.Value = IIF(loFormSet.lnTmpRcCnt=0,REPLICATE ("0",loFormSet.lnAcsSegSz),cAcctCode)
  lcAccDesc  = IIF(loFormSet.lnTmpRcCnt=0,SPACE(60),LOOKUP(GLACCHAR.cAccnlDes,;
                   &lc_TmpRcr..cAcctCode,GLACCHAR.cAcctCode,'ACCTCODE'))
 
  loDbr.Value    = IIF(loFormSet.lnTmpRcCnt=0 .OR. cDrOrCr="C",0,nAmount)
  loCdr.Value   = IIF(loFormSet.lnTmpRcCnt=0 .OR. cDrOrCr="D",0,nAmount)
  
  *** Adjust controls
  lcObjState = IIF(loFormSet.lnTmpRcCnt=0,"DISABLE","ENABLE")

  SHOW GET lcAcctCode &lcObjState.       
  loDbr.Enabled = lcObjState = 'ENABLE'
  loCdr.Enabled = lcObjState = 'ENABLE'

  loDetFormset.ariaform1.pbRem.Enabled = lcObjState = 'ENABLE'
  
  *B600380,1 Reham On 06/05/95
  *B600380,1 This is to know that I removed the added detail line.
  loFormSet.llNewDet = .F.
  loDetFormset.ariaform1.pbNew.Enabled = .T.
 
  lcObjState = IIF(loFormSet.lnTotDr <> loFormSet.lnTotCr .AND. loFormSet.lnTmpRcCnt > 1,;
                   "ENABLE","DISABLE") 
  loDetFormset.ariaform1.pbAdj.Enabled = lcObjState = 'ENABLE'

  SELECT GLAUTHD

ENDIF
loDetFormSet.Ariaform1.Refresh()

*E303214,4 TMI 08/23/2012 [Start] refresh
=lfFormAfterRowColumnChange(loDetFormSet)
*E303214,4 TMI 08/23/2012 [End  ] 


*!**************************************************************************
*!
*!      Function: lfvAdj
*!
*!**************************************************************************
*    VALID function for push button "Adjust" (pbAdj).
*  
FUNCTION lfvAdj
PARAMETERS loDetFormSet
loFormSet = loDetFormSet.loFormSet
lc_TmpRcr = loFormSet.lc_TmpRcr

loDbr = loDetFormSet.Ariaform1.lnDebit
loCdr = loDetFormSet.Ariaform1.lnCredit

SELECT (lc_TmpRcr)

loFormSet.lnTotdr   = loFormSet.lnTotdr - loDbr.Value 
loFormSet.lnTotcr   = loFormSet.lnTotcr - loCdr.Value
lnDiff    = loFormSet.lnTotdr - loFormSet.lnTotcr
lnSign    = SIGN(lnDiff)
lnDiff    = ABS (lnDiff)

*** If the difference is not equal to zero
IF lnDiff <> 0
  *** If the difference is positive,i.e. Debit is greater,
  IF lnSign = 1
    *** Add difference to Credit field
    loFormSet.lnTotcr   = loFormSet.lnTotcr + lnDiff
    loCdr.Value  = lnDiff
    loDbr.Value   = 0 
  ELSE
    *** If the difference is negative,i.e.Credit is greater,
    loFormSet.lnTotdr   = loFormSet.lnTotdr + lnDiff 
    loDbr.Value   = lnDiff
    loCdr.Value  = 0
  ENDIF

  REPLACE  cDrOrCr WITH IIF(loDbr.Value<>0,"D","C");
           nAmount WITH lnDiff ;
           cStatus WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)    

  loDetFormset.Ariaform1.pbAdj.Enabled = .F.

  *** If the difference is zero,
ELSE
  *** Message :  "  By adjusting this entry, both debit     "
  ***            "  and credit will have zero values.       "
  ***            "  You cannot leave both debit and credit  "
  ***            "  with zero value.                        "
  ***                          <   OK >
  =gfModalGen("TRM02059B00000","DIALOG")
  loFormSet.lnTotdr   = loFormSet.lnTotdr + loDbr.Value 
  loFormSet.lnTotcr   = loFormSet.lnTotcr + loCdr.Value
ENDIF

SELECT GLAUTHD

IF VAL(STRTRAN(loFormSet.lcAcctCode,'-',''))>0.AND.(loDbr.Value>0 .OR. loCdr.Value>0)
  *B600380,1 Reham On 06/05/95
  *B600380,1 This is to know that the <new> button status is enable.
  loFormSet.llNewDet = .F.
  loDetformset.ariaform1.pbNew.Enabled = .t. 
ENDIF  


*E303214,4 TMI 08/23/2012 [Start] refresh
=lfFormAfterRowColumnChange(loDetFormSet)
*E303214,4 TMI 08/23/2012 [End  ] 

************************************************************
*! Name      : lfFormBeforeSave
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/05/2012
*! Purpose   : Before save function
************************************************************
FUNCTION lfFormBeforeSave
PARAMETERS loFormSet
lc_TmpRcr = loFormSet.lc_TmpRcr
SELECT (lc_TmpRcr)
llCSave = .T.
DO CASE
  *** If the file is empty,i.e. there are no recurring transactions,
  *** "Cannot save an empty transaction" 
  CASE (loFormSet.lnTmpRcCnt = 0 )
    =gfModalGen("TRM02035B00000","Dialog",loFormSet.lcTRcrTr)
    llCSave = .F.
  CASE VAL(STRTRAN(cAcctCode,'-','')) = 0
    =gfModalGen("TRM02022B00000","DIALOG")  
    llCSave = .F.
  CASE nAmount = 0
    =gfModalGen("TRM02033B00000","DIALOG",loFormSet.lcTDrOrCr)
    llCSave = .F.
  *** If Total debits and credits are not equal,
  *** "Recurring entries must balance"
  CASE ( loFormSet.lnTotcr <> loFormSet.lnTotdr )
    =gfModalGen("TRM02019B00000","Dialog",loFormSet.lcTRcr)
    llCSave = .F.
ENDCASE 
IF llCSave = .F.
  lfvEntries(loFormSet)
ENDIF   
*_CUROBJ = OBJNUM(pbNew)    
RETURN llCSave

*- End of lfFormBeforeSave.

*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!
*!**************************************************************************
*    This procedure handles saving,instead of the global procedure.
*    "Save" option corresponds to the ninth position in laDefProc array,
*    hence it should have previously been assigned the value .F.
*    to disable the global saving procedure.
*      The flag llCSave is a global flag that is to have the value
*    of "FALSE" (.F.) if the record(s) is/are not to be saved.
*
PROCEDURE lpSavScr
PARAMETERS loFormSet

*** If adding a new record,append a blank one
SELECT (loFormSet.lcBasefile)
IF loFormSet.ActiveMode='A'
  APPEND BLANK
ENDIF

*** Store laData values in the current record
lcScFields = loFormSet.lcScFields
GATHER FROM loFormSet.laData fields &lcScFields.
gfTableUpdate()
    
*** Now save the data in the temporary file using the following global
*** function which performs correct translation from the temporary
*** file lc_TmpRcr,and the main file GLAUTDT
=gfTmp2Mast("GLAUTDT",loFormSet.lc_TmpRcr,;
                'Saving recurring entry '+loFormSet.laData[2]+' ...')  
 
SELECT GLAUTHD
gfTableUpdate()

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

*** Check if this record is already deleted by another user from
*** a different station. If it is, the record pointer is no longer
*** on the viewed record which is now actually out of scope if 
*** SET('DELETED')='ON'
IF GLAUTHD.cAutType + GLAUTHD.cAutCode <> loFormSet.laData[1]+loFormSet.laData[2] 
  *** If the record is already deleted, present the following message,
  *** and go to 'Select' mode
  *** Message : "
  =gfModalGen("TRM00095B00000","ALERT")
  
  loFormSet.ChangeMode('S')
  RETURN .F.
ENDIF  

loFormSet.lnThermRec     = 0

*** Delete records belonging to the current header from the master
*** file (GLAUTDT)
*** The temporary file lc_TmpRcr is zapped in 'Select' mode
SELECT GLAUTDT
SCATTER MEMVAR MEMO BLANK
lcAutDtEx = loFormSet.lcAutDtEx
REPLACE cAutType  WITH m.cAutType,;
        cAutCode  WITH m.cAutCode,;
        cAcctCode WITH m.cAcctCode ;        
       FOR &lcAutDtEx. = loFormSet.laData[1]+loFormSet.laData[2];
         .AND. lfThermo(;
            'Deleting recurring entry '+loFormSet.laData[2]+' ...')
DELETE FOR &lcAutDtEx. = m.cAutType + m.cAutCode 

SELECT GLAUTHD

*** Then delete the header record
SCATTER MEMVAR MEMO BLANK
GATHER MEMVAR MEMO
DELETE 
gfTableUpdate()

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

loFormSet.lnThermRec = loFormSet.lnThermRec + 1

*!**************************************************************************
*!
*!      Function: lfSrcJrnl
*!
*!**************************************************************************
*  Creates an array of Source journals for the Source Journals popup
*
FUNCTION lfSrcJrnl
PARAMETERS loFormSet

*** Create an array holding avialable Source Journals from 
*** GLSORJOR.DBF to be displayed in popup (puSrcJrnl/ibSrcJrnl)
lc_TmpRcr = loFormSet.lc_TmpRcr
gcWorkDir = oAriaApplication.WorkDir
gcDataDir = oAriaApplication.DataDir

SELECT ALLTRIM(CSRCJRNL)+'-'+;
       CJORLNDES,CSRCJRNL;
       FROM &gcDataDir.GLSUBJOR;
       INTO ARRAY loFormSet.laTSrcJrnl
loFormset.Ariaform1.puSrcJrnl.RowSource = 'ThisFormSet.laTSrcJrnl'
loFormset.Ariaform1.puSrcJrnl.Value     = loFormSet.lcSJ_Def
