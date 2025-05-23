*:************************************************************************
*:  Program File: \ARIA4XP\PRGS\GL\GLBEGBL.prg
*:  Module      : General Ledger
*:  Desc.       : Batches\Begining Balance Batch
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 10/23/2012
*:  Reference   : *E303281 
*:************************************************************************
*:Modifications
*:
*: B610241,1 HIA 02/12/2013 GL- Beginning Balances issue [T20130123.0034]
**************************************************************************
*- Get the screen , call it 
lcRunScx = lfGetScx("GL\GLBEGBL.scx")
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

loFormSet.AddProperty('lcProgName','GLBEGBL')

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
  .cBrowseIndexExpression = "CBATCHNO"
  .cBrowseIndexFields     = "CBATCHNO"
  .cBrowseIndexName       = "BATCHNO"
  .cBrowseAliasName       = .lcBaseFile
  .cBrowseTableName       = .lcBaseFile
  .cBrowseFilter          = ""
  .BrowseTitle	          = 'Batch'

  .ariaBrFields.edtBrowseFields.Value = "CBATCHNO  :H='GL Batch number',"+;
                                        "CBATSTAT  :H='Batch status',"+;
                                        "CBATTYPE  :H='Batch Type',"+;
                                        "CBATPYR   :H='Posting year',"+;
                                        "DBATPBEG  :H='Batch posting begin date',"+;
                                        "DBATPEND  :H='Batch posting end date',"+;
                                        "CBATREFER :H='Batch reference',"+;
                                        "CBATDESC  :H='Batch description',"+;
                                        "NBATCNTOT :H='Batch control total',"+;
                                        "NBATOTDR  :H='Total debits',"+;
                                        "NBATOTCR  :H='Total credits',"+;
                                        "CSRCMODUL :H='Source module',"+;
                                        "CCOMP_ID  :H='Company ID'"

ENDWITH 

*- Define needed variables.
=lfDefineVars(loFormSet)

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

*** check if creation of beginning balance batches is allowed.
IF ! glSetup.lSetAlBBE
  *** Creating beginning balance batches is not allowed. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02204B00000","DIALOG")
  glQuitting = .T.
  RETURN .F.
ENDIF

*** check if the beginning balance date is out of the posting window. 
IF ASCAN(loFormSet.laFisYear,STR(YEAR(glSetup.dSetBBDat),4)) = 0
  *** The beginning balance date is out of the posting  window. ***
  *** You cannot create any beginning balance batches any more. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02203B00000","DIALOG")
  glQuitting = .T.
  RETURN .F.
ENDIF

*- Set the control sources
=lfSetControlSource(loFormSet)

*- Start with Select Mode

loFormSet.ChangeMode('S')

*lfStrEndPos(loFormSet)

*- Define input mask 
WITH loFormSet.Ariaform1
  .laData1.KeyTextBox.InputMask = 'X'+REPLICATE('9',FSIZE('CBATCHNO','GLBATCH')-1)
  .laData7.InputMask = REPLICATE('9',FSIZE('NBATCNTOT','GLBATCH')-3)+'.99'
  .laData8.MaxLength = FSIZE('CBATREFER','GLBATCH')
  .laData9.MaxLength = FSIZE('CBATDESC' ,'GLBATCH')
  .laData10.InputMask = REPLICATE('9',FSIZE('NBATCNTOT','GLBATCH')-3)+'.99'
  .laData11.InputMask = REPLICATE('9',FSIZE('NBATCNTOT','GLBATCH')-3)+'.99'
  .lnBalance.InputMask = REPLICATE('9',FSIZE('NBATCNTOT','GLBATCH')-3)+'.99'
ENDWITH 

*- Define progress bar variable
loFormSet.AddProperty('oProgress' ,  NULL )


*- End of lfFormInit.
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

loFormSet.AddProperty('laTstatus[7,1]')
loFormSet.AddProperty('laFisYear[3,1]')
loFormSet.AddProperty('laAccTypes[1,1]',' ')
loFormSet.AddProperty('laSourcJor[1,1]')
loFormSet.AddProperty('laFileStru[1]')


loFormSet.AddProperty('laKeyField[1,4]','')
loFormSet.laKeyField[1,1] = 'laData[1]'
loFormSet.laKeyField[1,2] =.T.
loFormSet.laKeyField[1,3] = 'BATCHNO'
loFormSet.laKeyField[1,4] = 1

loFormSet.AddProperty('lcDelMesag'     , "void" )  && holds the prompt name insted of delete.
loFormSet.AddProperty('lc_Title'       ,  "Beginning batch details" )&& holds chield window titel.
loFormSet.AddProperty('lcPrompt'       , ''   )

loFormSet.AddProperty('lnDetRecno'       , 0   )


lcVars =  "lcPostPrd  , lc_TemDet  , lc_MasDet   , lcTranNo   ,"+;
         "lcTranDes  , lcTranRef  , lcTrdtexp   , lcSourcJor ,"+;
         "lcBegDet   , lcAcctCode , lcAccDesc   , lcTypeId   ,"+;
         "lcSelType  , lcActType  , lcStatus    , lcBalncid  ,"+;
         "lcScope    , lcBStamp   , laFisYear  ,"+;
         "laTstatus"
=lfAddProp(loFormSet,lcVars,' ')

loFormSet.laTStatus[1,1] = "Empty"
loFormSet.laTStatus[2,1] = "Out of Balance"
loFormSet.laTStatus[3,1] = "Unposted"
loFormSet.laTStatus[4,1] = "Posted"
loFormSet.laTStatus[5,1] = "Summarized"
loFormSet.laTStatus[6,1] = "Hold"
loFormSet.laTStatus[7,1] = "Approved"

lcVars = "lcYear     , lcObjStatus"
=lfAddProp(loFormSet,lcVars,'')

lcVars = "lsBegDet   , cbInclEmty , lnInclEmty  , "+;
         "ibActtype  "
=lfAddProp(loFormSet,lcVars,1)

lcVars = "llglobShow , laDefProc[7], laDefProc[9]"
=lfAddProp(loFormSet,lcVars,.F.)

lcVars = "lnDebit    , lnCredit   , lnSelected  , lnTotalRec ,lncbHold"
=lfAddProp(loFormSet,lcVars,0)

lcVars = "lnBalance"
=lfAddProp(loFormSet,lcVars,0.00)

loFormSet.AddProperty('llDoLocal', .T. )

loFormSet.laFisYear[1,1]  = STR(loFormSet.lnCurr_yer-1,4) 
loFormSet.laFisYear[2,1]  = STR(loFormSet.lnCurr_yer  ,4) 
loFormSet.laFisYear[3,1]  = STR(loFormSet.lnCurr_yer+1,4) 

loFormSet.AddProperty('lcWinTitl' , "Beginning balance batches"  )  && main window titel.

  *** select all source journal from GLSUBJOR into array laSourcJor
  
  SELECT  cSrcJrnl+"-"+cJorlnDes;
          FROM &gcDataDir.GLSUBJOR;
          INTO ARRAY loFormSet.laSourcJor

  IF _TALLY = 0
    loFormSet.laSourcJor      = " "
    loFormSet.laSourcJor[1,1] = loFormSet.lcSJ_Def
  ELSE
    *** check if the default source journal is found ***
    *** in the array  laSourcJor.  if not found then ***
    *** inserts  the default  source journal into an ***
    *** array...
    IF ASCAN(loFormSet.laSourcJor,loFormSet.lcSJ_Def) = 0
      DIMENSION loFormSet.laSourcJor[ALEN(loFormSet.laSourcJor,1)+1,1]
      =AINS(loFormSet.laSourcJor,1)
      loFormSet.laSourcJor[1,1] = loFormSet.lcSJ_Def
    ENDIF  
  ENDIF
  
loFormSet.lc_TemDet = gfTempName()  
loFormSet.lc_MasDet = gfTempName()  

lc_TemDet = loFormSet.lc_TemDet
lc_MasDet = loFormSet.lc_MasDet 
  
*** create temparary file structure. ***
CREATE DBF (gcWorkDir+lc_TemDet);
           (cAcctCode C(24)   ,;
            cAccNlDes C(65)   ,;
            nAmount   N(15,2) ,;
            nAcBopBal N(18,2) ,;
            cTrdtexp  C(40)   ,;
            cDrOrCr   C(1)  ) 

USE &gcWorkDir.&lc_TemDet EXCLUSIVE
SELECT (lc_TemDet)
INDEX ON  cAcctCode TAG cAccTem
SET ORDER TO TAG cAccTem

SELECT GLTRNSDT
*** create temparary file structure. ***
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,18]

laFileStru[lnFileStru+1,1] = 'NRECNO'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 10
laFileStru[lnFileStru+1,4] = 0

laFileStru[lnFileStru+2,1] = 'CSTATUS'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0
  
FOR i= lnFileStru+1 TO ALEN(laFileStru,1)
  STORE .F. TO laFileStru[i,5],laFileStru[i,6]
  STORE '' TO laFileStru[i,7],laFileStru[i,8],laFileStru[i,9],laFileStru[i,10],laFileStru[i,11],laFileStru[i,12],laFileStru[i,13],laFileStru[i,14],laFileStru[i,15],laFileStru[i,16]
  STORE 0 TO laFileStru[i,17],laFileStru[i,18]    
ENDFOR 

CREATE TABLE &gcWorkDir.&lc_MasDet FROM ARRAY laFileStru 
INDEX ON  cAcctCode TAG cAccMas
SET ORDER TO TAG cAccMas

SELECT GLBATCH
lcScFields = "CBATCHNO,CBATSTAT,CBATTYPE,CBATPYR,DBATPBEG,DBATPEND,NBATCNTOT,CBATREFER,CBATDESC,NBATOTCR,NBATOTDR,CSRCMODUL,CCOMP_ID"
lcCnt = ALLTRIM(STR(OCCURS(',',lcScFields)+1))
loFormSet.AddProperty('laData[&lcCnt.]')
loFormSet.AddProperty('lcScFields',lcScFields)
SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData BLANK
DIMENSION laData[ALEN(loFormSet.laData)]
ACOPY(loFormSet.laData,laData)

SELECT GLBATCH

*** set filter expression to cBatType = 'B' ---> 'beginning batch'
***                       and  cBatStat $ 'EOUHAP'
*** 'empty','out of balanec','hold','approved','posted'

lcScope    = "cBatType = 'B'  .AND. cBatStat $ 'EOUHAP'"
loFormSet.lcScope = lcScope
SET FILTER TO
SET FILTER TO &lcScope

****
loFormSet.AddProperty('lcNewStat'   , IIF(loFormSet.ActiveMode = 'S' .OR. loFormSet.ActiveMode = 'V',"ENABLE","DISABLE"))
loFormSet.AddProperty('lcEntStat'   , IIF(loFormSet.ActiveMode = 'S',"DISABLE","ENABLE"))
loFormSet.AddProperty('lcPrnStat'   , IIF(loFormSet.ActiveMode = 'V' .AND. laData[2]<> 'E',"ENABLE","DISABLE"))
loFormSet.AddProperty('lcPosStat'   , IIF(loFormSet.ActiveMode = 'V' .AND. laData[2]=  'U',"ENABLE","DISABLE"))
loFormSet.AddProperty('lcHldStat'   , IIF(loFormSet.ActiveMode = 'E' .AND. laData[2] $ 'UH',"ENABLE","DISABLE" ))
lcValuStat = IIF((loFormSet.ActiveMode = 'E' .OR. loFormSet.ActiveMode = 'A')  ;
                  .AND. RECCOUNT(lc_TemDet) <> 0 ,"ENABLE","DISABLE")
loFormSet.AddProperty('lcCommStat' , IIF(lcValuStat="ENABLE" .AND. ;
                 (!EMPTY(&lc_TemDet..CDRORCR)),"ENABLE","DISABLE"))

loFormSet.lcObjStatus = IIF(loFormSet.ActiveMode = 'V' .AND. laData[2] $ 'EOUZH',"ENABLE","DISABLE")
loFormSet.cbInclEmty  = loFormSet.lnInclEmty
****
*- End of lfDefineVars.

************************************************************
*! Name      : lfFormActivate
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/07/2012
*! Purpose   : lfFormActivate method 
************************************************************
FUNCTION lfFormActivate
PARAMETERS loFormSet

IF loFormSet.ActiveMode='V' .OR. loFormSet.ActiveMode='E'
  && in case of return to the program after activate the any other 
  && program, Reread the batch status, batch Credit, Batch Debit.
  IF loFormSet.ActiveMode='V'
    loFormSet.laData[2]  = glbatch.cbatstat
    loFormSet.laData[10] = glbatch.nbatotcr
    loFormSet.laData[11] = glbatch.nbatotdr          
  ENDIF 
  loFormSet.lnBalance  = ABS(loFormSet.laData[11] - loFormSet.laData[10])
  loFormSet.lcBalncID  = IIF(loFormSet.laData[11] > loFormSet.laData[10],"Cr",;
              IIF(loFormSet.laData[11] < loFormSet.laData[10],"Dr",""))

  IF loFormSet.laData[2] = 'U' .AND. loFormSet.lncbHold = 1
    loFormSet.Ariaform1.cbHold.Value = 1
    loFormSet.laData[2] = 'H'
  ELSE  
    loFormSet.Ariaform1.cbHold.Value = 0
  ENDIF

  loFormset.lcStatus  = loFormset.laTStatus[AT(loFormSet.laData[2],'EOUPZHA')]
  loFormset.lncbHold = loFormSet.Ariaform1.cbHold.Value
ENDIF
*- End of lfFormActivate.

************************************************************
*! Name      : lfFormRefreshAll
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/11/2012
*! Purpose   : RefreshAll method 
************************************************************
FUNCTION lfFormRefreshAll
PARAMETERS loFormSet
IF loFormSet.ActiveMode = 'V'  
  IF loFormSet.laData[2] $ 'PA'
    loFormSet.oToolBar.cmdEdit.Enabled = .F.
    loFormSet.oToolBar.cmdDelete.Enabled = .F.
  ELSE
    loFormSet.oToolBar.cmdEdit.Enabled = .T.
    loFormSet.oToolBar.cmdDelete.Enabled = .T.
  ENDIF
ENDIF 
loFormSet.Ariaform1.Refresh()
*- End of lfFormRefreshAll.

************************************************************
*! Name      : lfFormDestroy
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/23/2012
*! Purpose   : destroy method function
************************************************************
FUNCTION lfFormDestroy
PARAMETERS loFormSet

SELECT GLBATCH
SET FILTER TO
lc_TemDet = loFormSet.lc_TemDet
lc_MasDet = loFormSet.lc_MasDet 
gcWorkDir = oAriaApplication.WorkDir
 
*** erase all temparary files 
IF USED (lc_TemDet)
  USE  IN ALIAS(lc_TemDet)
ENDIF

ERASE &gcWorkdir.&lc_TemDet..DBF
ERASE &gcWorkdir.&lc_TemDet..CDX

IF USED (lc_MasDet)
  USE IN ALIAS(lc_MasDet)
ENDIF

ERASE &gcWorkdir.&lc_MasDet..DBF
ERASE &gcWorkdir.&lc_MasDet..CDX

*- End of lfFormDestroy.
 
************************************************************
*! Name      : lfSetControlSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/07/2012
*! Purpose   : Set ControlSource for the screen controls
************************************************************
FUNCTION lfSetControlSource
PARAMETERS loFormset
*- fields are ordered according to their appearence in the screen
WITH loFormSet.Ariaform1
  .laData1.Keytextbox.ControlSource = 'Thisformset.laData[1]'
  .laData4.ControlSource            = 'Thisformset.laData[4]'

  .laData7.ControlSource            = 'Thisformset.laData[7]'
  .laData8.ControlSource            = 'Thisformset.laData[8]'
  .laData9.ControlSource            = 'Thisformset.laData[9]'
  .lcStatus.ControlSource           = 'Thisformset.lcStatus'

  .laData10.ControlSource           = 'Thisformset.laData[10]'
  .laData11.ControlSource           = 'Thisformset.laData[11]'
  .lnBalance.ControlSource          = 'Thisformset.lnBalance'
  .lcBalncId.ControlSource          = 'Thisformset.lcBalncId'
  .lcPostPrd.ControlSource          = 'Thisformset.lcPostPrd'
ENDWITH 

*- End of lfSetControlSource.

*!**************************************************************************
*!
*!      FUNCTION: lpShow
*!
*!**************************************************************************
*
FUNCTION lfChangeMode
PARAMETERS loFormSet

IF TYPE('loFormSet.lcProgName')='U'
  RETURN 
ENDIF 

SELECT GLBATCH
gcWorkDir = oAriaApplication.WorkDir
lc_TemDet = loFormSet.lc_TemDet
lc_MasDet = loFormSet.lc_MasDet 

loFormSet.lnDetRecno = 0

IF loFormSet.ActiveMode = 'V'
  && hold the audit information in lcBStamp from the current record.
  loFormSet.lcBStamp  = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time 
ENDIF  

lcBlank = IIF( loFormSet.ActiveMode $ 'AS' , 'BLANK', '' )
lcScFields = loFormSet.lcScFields
SELECT (loFormSet.lcBasefile)
SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData  &lcBlank
DIMENSION laData[ALEN(loFormSet.laData)]
=ACOPY(loFormSet.laData,laData)

=lfCheckRec(loFormSet)  && check the current batch status. 

lfSetControlSource(loformSet)
WITH loFormSet.Ariaform1
  .laData1.Enabled = .F.
ENDWITH 

DO CASE 
  CASE loFormSet.ActiveMode = 'S'           && Select Mode

    *** initialize variables. ***
    loFormSet.lcStatus   = " "
    loFormSet.lcPostPrd  = " "    
    loFormSet.lnBalance  = 0.00
    loFormSet.lcBalncID  = " "
    loFormSet.Ariaform1.cbHold.Value     = 0

    *** initialize child window variables. ***
    loFormSet.lcTranNo   = " "
    loFormSet.lcTranRef  = " " 
    loFormSet.cbInclEmty = 0
    loFormSet.lcTranDes  = " " 
    loFormSet.lcSourcJor = " " 
    loFormSet.lcActType  = " "
    loFormSet.lcTypeID   = " "
    loFormSet.lcAcctCode = " "
    loFormSet.lnDebit    = 0.00
    loFormSet.lcAccDesc  = " " 
    loFormSet.lnCredit   = 0.00
    loFormSet.lcTrdTexp  = " "

    loFormSet.lncbHold   = 0
    loFormSet.lcSelType  = " " 

    SELECT (lc_MasDet)
    ZAP
    SELECT (lc_TemDet)
    ZAP

    loFormSet.oToolBar.cmdDelete.Enabled = .F.
    loFormSet.lcObjStatus = 'DISABLE'
    WITH loFormSet.Ariaform1
      .laData1.Enabled = .T.
      .laData1.Setfocus()
    ENDWITH 
    
  CASE loFormSet.ActiveMode = 'V' .OR. loFormSet.ActiveMode = 'E' 

    loFormset.lcStatus  = loFormset.laTStatus[AT(laData[2],'EOUPZHA')]
    loFormSet.Ariaform1.cbHold.Value    = IIF(laData[2]='H',1,0)
    loFormset.lncbHold = loFormSet.Ariaform1.cbHold.Value
    loFormSet.lnBalance = ABS(laData[11] - laData[10])
    
    loFormSet.lcBalncID = IIF(laData[11] > laData[10],"Cr",;
                IIF(laData[11] < laData[10],"Dr",""))

    lnPostPrd  = gfPeriod(laData[5],oAriaApplication.PrntCompanyid)
    loFormSet.lcPostPrd  = RIGHT("0"+ALLTRIM(STR(lnPostPrd)),2) + '-' + laData[4]

    SELECT GLTRNSHD
    lcExactSet = SET("EXACT")
    SET EXACT OFF
    =SEEK (laData[1],'GLTRNSHD')
    loFormSet.lcTranNo   = GLTRNSHD.cTranNo
    loFormSet.lcTranDes  = GLTRNSHD.cTrnDesc
    loFormSet.lcTranRef  = GLTRNSHD.cTrnRefer 

    lnSourcNo  = ASCAN(loFormSet.laSourcJor,GLTRNSHD.cSrcJrnl)
    IF lnSourcNo > 0
      loFormSet.lcSourcJor = loFormSet.laSourcJor[lnSourcNo]
    ELSE
      loFormSet.lcSourcJor = SPACE(50)
    ENDIF
    SET EXACT &lcExactSet  
      
    loFormSet.lcTypeID   = " "
    loFormSet.lcAcctCode = " " 
    loFormSet.lcAccDesc  = " " 
    loFormSet.lcTrdTexp  = " " 
    loFormSet.lnCredit   = 0.00
    loFormSet.lnDebit    = 0.00
      
    loFormSet.Ariaform1.pbEntries.Enabled = .T.

    IF loFormSet.ActiveMode = 'V'                  && View Mode  
    
      loFormSet.lcBStamp  = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time 
      loFormSet.cbInclEmty = 0
      
      loFormSet.laAccTypes[1,1] = '----------------- All Types -----------------'
      loFormSet.lcActType =  loFormSet.laAccTypes[1,1]

      *** New
      SELECT (lc_TemDet)
      ZAP   && Delete old data ( if any )
      lsBegDet   = 1 
      SELECT GLACCHAR
      loFormSet.lnSelected = 0
      loFormSet.lnTotalRec = RECCOUNT("GLACCHAR")

      SELECT GLTRNSDT.CACCTCODE, GLACCHAR.CACCNLDES, GLACBALS.NACBOPBAL,;
      GLTRNSDT.NAMOUNT AS 'NAMOUNT',GLTRNSDT.CDRORCR AS 'CDRORCR',;
      'S' AS 'CSTATUS',GLTRNSDT.cTrdtexp AS 'cTrdtexp',;
      RECNO() AS 'NRECNO',lfTherm(loFormSet) ;
      FROM GLTRNSDT, GLACCHAR, GLACBALS;
      INTO  DBF &gcWorkDir.&lc_TemDet ;
      WHERE GLACBALS.CACCTCODE +GLACBALS.CFISFYEAR+GLACBALS.CFSPPRDID= ;
      GLACCHAR.CACCTCODE+laData[4]+SUBSTR(loFormSet.lcPostPrd,1,2);
      .AND. GLTRNSDT.cBatchNo+GLTRNSDT.cTranNo =  laData[1]+loFormSet.lcTranNo;
      .AND. GLTRNSDT.CACCTCODE = GLACCHAR.CACCTCODE

      IF loFormSet.lnTotalRec <> loFormSet.lnSelected
        FOR lnCounter = loFormSet.lnSelected TO loFormSet.lnTotalRec STEP 10          
          =lfShowProgress(loFormSet.lnTotalRec,lnCounter,'','')
        ENDFOR   
      ENDIF
      =lfShowProgress(loFormSet.lnTotalRec,loFormSet.lnTotalRec,'','')
 
      SELECT (lc_TemDet)
      INDEX ON  cAcctCode TAG cAccTem
      SET ORDER TO TAG cAccTem

      IF laData[2] $ 'PA'
        loFormSet.Ariaform1.pbPost.Enabled = .F.
        SHOW GET pbCopy DISABLE
        loFormSet.oToolBar.cmdEdit.Enabled = .F.

        loFormSet.oToolBar.cmdDelete.Enabled = .F.
        loFormSet.lcObjStatus = 'DISABLE'
        
      ELSE
        loFormSet.Ariaform1.pbPost.Enabled = laData[2]= "U"
        SHOW GET pbCopy ENABLE 
        loFormSet.oToolBar.cmdEdit.Enabled = .T.
        
        loFormSet.oToolBar.cmdDelete.Enabled = .T.
        loFormSet.lcObjStatus = 'ENABLE'
      ENDIF

    ELSE                       && Edit Mode  
      loFormSet.cbInclEmty = 1
      loFormSet.laAccTypes = .F.
      loFormSet.laAccTypes[1,1] = '----------------- All Types -----------------'
      loFormSet.lcActType  =  loFormSet.laAccTypes[1,1]

      *IF laData[2] $ 'UH'
      *  SHOW GET cbHold  ENABLE  
      *ELSE  
      *  SHOW GET cbHold  DISABLE
      *ENDIF
      *SHOW GET laData[8]  ENABLE     
      *SHOW GET laData[9]  ENABLE  
      loFormSet.Ariaform1.cbHold.Enabled = laData[2] $ 'UH'
      loFormSet.Ariaform1.pbPost.Enabled = .F.
      
      SELECT * ,'S' AS 'CSTATUS',RECNO() AS 'NRECNO';
      FROM GLTRNSDT;
      INTO  DBF &gcWorkDir.&lc_MasDet ;
      WHERE GLTRNSDT.cBatchNo+GLTRNSDT.cTranNo =  laData[1]+loFormSet.lcTranNo 

      SELECT (lc_MasDet)
      INDEX ON  cAcctCode TAG cAccMas
      SET ORDER TO TAG cAccMas
      lsBegDet   = 1 
      SELECT (lc_TemDet)
      ZAP
      loFormSet.lcSelType  = "%" 
      loFormSet.oToolBar.cmdDelete.Enabled = .F.
      loFormSet.lcObjStatus = 'DISABLE'
    ENDIF
 
  CASE loFormSet.ActiveMode = 'A'             && Add Mode
    laData[1]  = ''
    laData[2]  = "E"
    laData[3]  = "B"
    lcYear = loFormSet.lcYear
    =gfPeriod(GLSETUP.dSetBbDat,oAriaApplication.PrntCompanyid,@lcYear)
    loFormSet.lcYear = lcYear    
    laData[4]  = lcYear
    laData[5]  = GLSETUP.dSetBbDat
    laData[6]  = GLSETUP.dSetBbDat
    loFormset.lcStatus   = loFormset.laTStatus[1]
    laData[11] = 0.00
    laData[10] = 0.00
    lnPostPrd  = gfPeriod(laData[5],oAriaApplication.PrntCompanyid)
    loFormSet.lcPostPrd  = RIGHT("0"+ALLTRIM(STR(lnPostPrd)),2) + '-' + laData[4]
    loFormSet.lnBalance  = 0
    loFormSet.lcBalncID  = " "
    laData[7]  = 0.00
    loFormSet.Ariaform1.cbHold.Value     = 0 
    loFormset.lncbHold = loFormSet.Ariaform1.cbHold.Value
    laData[8]  = "On " + IIF(SET('CENTURY')='ON',DTOC(oAriaApplication.SystemDate),;
                         LEFT(DTOC(oAriaApplication.SystemDate),6)+STR(YEAR(oAriaApplication.SystemDate),4))
    laData[9]  = SUBSTR("Created by " + LOOKUP(SYUUSER.cUsr_Name,oAriaApplication.User_ID,SYUUSER.cUser_Id),1,40)
    laData[12] = oAriaApplication.ActiveModuleid
    laData[13] = oAriaApplication.ActiveCompanyID

    && -----> initialize child window variables
    loFormSet.lcTranNo   = " "
    loFormSet.lcTranRef  = "On " + IIF(SET('CENTURY')='ON',DTOC(oAriaApplication.SystemDate),;
                         LEFT(DTOC(oAriaApplication.SystemDate),6)+STR(YEAR(oAriaApplication.SystemDate),4))
    loFormSet.cbInclEmty = 1
    loFormSet.lcTranDes  = "Beginning balances by " + LOOKUP(SYUUSER.cUsr_Name,oAriaApplication.User_ID,SYUUSER.cUser_Id)
    loFormSet.lcSourcJor = loFormSet.laSourcJor[ASCAN(loFormSet.laSourcJor,loFormSet.lcSJ_Def),1] 
    loFormSet.lcSelType  = " "
    loFormSet.laAccTypes = .F.
    *loFormSet.laAccTypes[1,1] = 'No Account Type Selected'
    loFormSet.laAccTypes[1,1] = '----------------- All Types -----------------'
    loFormSet.lcActType  =  loFormSet.laAccTypes[1,1]
    loFormSet.lcTypeID   = " "
    loFormSet.lcAcctCode = " "
    loFormSet.lnDebit    = 0.00
    loFormSet.lcAccDesc  = " " 
    loFormSet.lnCredit   = 0.00
    loFormSet.lcTrdTexp  = " "

    SELECT (lc_MasDet)
    ZAP
    SELECT (lc_TemDet)
    ZAP
    
    WITH loFormSet.Ariaform1
      .cbHold.Enabled = .F.     && DISABLE
      loFormSet.Ariaform1.pbEntries.Enabled = .T.
      .pbPost.Enabled = .F.     && DISABLE
  
    ENDWITH 
    
    loFormSet.oToolBar.cmdDelete.Enabled = .F.
    loFormSet.lcObjStatus = 'DISABLE'

    *laCtrStat[8] = "DISABLE"
ENDCASE

loFormSet.lnInclEmty = loFormSet.cbInclEmty
lcColPair= IIF(loFormSet.ActiveMode = 'S' .OR. loFormSet.ActiveMode = 'V' ,SCHEME(1,10),SCHEME(1,2))
SELECT GLBATCH

=ACOPY(laData,loFormSet.laData)
RETURN 


*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
*
FUNCTION lfvData_1
PARAMETERS loFormSet,loFld
llBrowse = loFld.Selectedfrombrowse 
IF llBrowse .OR. !EMPTY(loFormSet.laData[1]) 
  IF llBrowse
    =loFormSet.oToolBar.cmdFind.Click()
  ELSE
    IF ! '?' $ loFormSet.laData[1]
      loFormSet.laData[1] = RIGHT("000000"+ALLTRIM(loFormSet.laData[1]),6)
    ENDIF  
  
    IF RECNO() <= RECCOUNT() .AND. ! EOF()
      GOTO RECNO()  
    ENDIF      
    IF !SEEK(loFormSet.laData[1],loFormSet.lcBaseFile)
      **** \!\<Browse;\<Add;\?\<Reenter
      IF '?' $ loFormSet.laData[1]
        loFormSet.laData[1] = ''
        lnResp = 1
      ELSE 
        lnResp = gfModalGen('INM00001B02004','DIALOG','GL Batch number:'+loFormSet.laData[2])
      ENDIF 
      DO case
      CASE lnResp = 1        
        IF !loFormSet.oToolBar.cmdFind.Click()
          RETURN .F.
        ENDIF 
      CASE lnResp = 2  
        loFormSet.ChangeMode('A')
      CASE lnResp = 3
        loFormSet.laData[2] = ' '
        RETURN .F.
      ENDCASE 
    ELSE
      *- the key is there, go to view mode
      loFormSet.ChangeMode('V')
    ENDIF 

    IF loFormSet.ActiveMode = 'A'    && add mode
     loFormSet.laData[1] = "      "
     loFormSet.laData[2] = 'E'
    ENDIF
  ENDIF
ENDIF

 
*!**************************************************************************
*!
*!      Function: lfvData_7
*!
*!**************************************************************************
*
FUNCTION lfvData_7
PARAMETERS loFormSet,loFld
 
*** check the value of audit total
IF SIGN(loFormSet.laData[7]) = -1
  *** Negative values are not allowed. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02036B00000","DIALOG")
  loFormSet.laData[7] = 0.00
  loFld.Value = loFld.OldValue
  loFld.Refresh()
  RETURN .F.
ENDIF 
   
*!**************************************************************************
*!
*!      Function: lfvPrint
*!
*!**************************************************************************
*
FUNCTION lfvPrint
PARAMETERS loFormSet

IF ! lfCheckRec(loFormSet)  && check the current batch status.
  RETURN
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvPost
*!
*!**************************************************************************
*
FUNCTION lfvPost
PARAMETERS loFormSet

IF ! lfCheckRec(loFormSet)   && check the current batch status. 
  RETURN
ENDIF  

*** check if the batch year falls in posting window.
IF ASCAN(loFormSet.laFisYear,glBatch.cbatpyr) <> 0 

  *** check if the user print the batch after last updating. 
  IF !EMPTY(glBatch.cBatElUsr) 
    llEditList= IIF ( glBatch.dbaTelDat > glBatch.daDd_Date,.T.,;
                IIF ( glBatch.dbaTelDat = glBatch.daDd_Date .AND.;
                      glBatch.cbAtelTim > glBatch.caDd_Time,.T.,.F.))
  ELSE 
    llEditList = .F. 
  ENDIF    

  *** check the force printing flag in glsetup
  llPrint = IIF (glSetup.lSetForel,;
            IIF (llEditList,.T.,.F.),.T.)
            
  IF llPrint 
    *** Are you sure you want to post this � ?
    *** <  Post  > - <  No  > ***
    IF gfModalGen("TRM02149B02009","DIALOG",'batch') = 1 
      IF ! lfCheckRec(loFormSet)   && check the current batch status.
        RETURN
      ENDIF  
      
      SELECT GLBATCH 
      lcKey = EVALUATE(KEY())
      IF gfObj_Lock(.T.)
        IF ! lfCheckRec(loFormSet)   && check the current batch status.
          RETURN
        ENDIF  
        *** calling posting program.
        IF lfTBPost(loFormSet,'BATCH','','Beginning') <> 0 
          =SEEK(lcKey,'GLBATCH')
          lcScFields = loFormSet.lcScFields          
          SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData
          
          *** update the audit information in the variable lcBStamp. ***
          loFormSet.lcBStamp     = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time 
          loFormSet.ChangeMode('V')
          
        ENDIF
        =gfObj_Lock(.F.)  
      ENDIF

    ENDIF
  ELSE
    *** Printing the edit list is required to post this batch. ***
    *** <  Ok  > ***
    =gfModalGen("TRM02148B00000","Dialog")
  ENDIF  
ELSE   
  *** The � posting year is out of the posting window. ***
  *** You cannot post this �. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02147B00000","Dialog",'batch|batch')
ENDIF

*!**************************************************************************
*!
*!      Function: lfvHold
*!
*!**************************************************************************
*
FUNCTION lfvHold
PARAMETERS loFormSet

loFormset.lncbHold = loFormSet.Ariaform1.cbHold.Value

IF loFormSet.Ariaform1.cbHold.Value = 1
  loFormSet.laData[2]  = 'H'
  loFormset.lcStatus  = loFormset.laTStatus[AT(loFormSet.laData[2],'EOUPZHA')]
 ELSE
  loFormSet.laData[2]  = 'U'
  loFormset.lcStatus  = loFormset.laTStatus[AT(loFormSet.laData[2],'EOUPZHA')]
ENDIF
loFormSet.Ariaform1.lcStatus.Refresh()
  
*!**************************************************************************
*!
*!      Function: lfvComment
*!
*!**************************************************************************
*
FUNCTION lfvComment
PARAMETERS loDetformset,loFld
loFormSet = loDetformset.loFormSet

lc_TemDet = loFormSet.lc_TemDet
SELECT (lc_TemDet)
REPLACE &lc_TemDet..cTrdtexp WITH loFormSet.lcTrdtexp ;
        &lc_TemDet..CSTATUS  WITH 'M'
SELECT GLBATCH


*!**************************************************************************
*!
*!      Function: lfvDebit
*!
*!**************************************************************************
*
FUNCTION lfvDebit
PARAMETERS loDetformset,loFld
loFormSet = loDetformset.loFormSet
loFrm = loDetformset.Ariaform1
loPrFrm = loFormSet.Ariaform1  && parent form

loFrm.lnDebit.Refresh()

*** check if the current account (if it's allowed for posting 
*** and an active account)
IF loFormSet.lnDebit <> 0 .AND. ! lfValidAcct(loDetFormSet)
  RETURN
ENDIF

DIMENSION laData[ALEN(loFormSet.laData)]
ACOPY(loFormSet.laData,laData)

lc_TemDet = loFormSet.lc_TemDet
SELECT (lc_TemDet)
IF loFormSet.lnDebit < 0 
  *** Negative values are not allowed. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02036B00000","DIALOG")
  loFormSet.lnDebit = loFld.OldValue
  *_CUROBJ = OBJNUM(lnDebit)    
  loFrm.lcTrdtexp.Enabled = .T. && ENABLE
  RETURN .F.
ELSE  
  REPLACE &lc_TemDet..CSTATUS WITH 'M'
  IF loFormSet.lnDebit > 0 
    laData[10] = laData[10] - loFormSet.lnCredit
    laData[11] = laData[11] - loFld.OldValue
    laData[11] = laData[11] + loFormSet.lnDebit
    loFormSet.lnCredit  = 0 
    REPLACE &lc_TemDet..NAMOUNT WITH loFormSet.lnDebit ;
            &lc_TemDet..CDRORCR WITH 'D'
    loFrm.lcTrdtexp.Enabled = .T.
  ELSE  
    IF &lc_TemDet..CDRORCR = 'D'
      laData[11] = laData[11] - loFld.OldValue 
      loFormSet.lnDebit   = 0     
      loFormSet.lnCredit  = 0 
      REPLACE &lc_TemDet..NAMOUNT WITH loFormSet.lnDebit ;
              &lc_TemDet..CDRORCR WITH ' '
      loFrm.lcTrdtexp.Enabled = .F.
    ENDIF  
  ENDIF  
ENDIF

loFormSet.lnBalance = ABS(laData[11] - laData[10])
loFormSet.lcBalncID = IIF(laData[11] > laData[10],"Cr",;
            IIF(laData[11] < laData[10],"Dr",""))

IF laData[11] <> laData[10]
  laData[2] = 'O'
  loFormSet.Ariaform1.cbHold.Value = 0
  loFormset.lncbHold = 0
  loPrFrm.cbHold.Enabled = .F.
ELSE
  IF laData[11] = 0 
    laData[2]   = 'E'
    loFormSet.Ariaform1.cbHold.Value = 0
    loFormset.lncbHold = 0
    loPrFrm.cbHold.Enabled = .F.
  ELSE
    IF loFormSet.lncbHold = 1  
      laData[2] = 'H'  
      loPrFrm.cbHold.Enabled = .F.
    ELSE  
      laData[2] = 'U'  
      loPrFrm.cbHold.Enabled = .T.
    ENDIF  
  ENDIF  
ENDIF 

loFormset.lcStatus  = loFormset.laTStatus [AT(laData[2],'EOUPZHA')]

loFrm.lnDebit.Refresh()
loFrm.lnCredit.Refresh()

ACOPY(laData,loFormSet.laData)
=lfwBegDet(loDetFormSet)

*!**************************************************************************
*!
*!      Function: lfvCredit
*!
*!**************************************************************************
*
FUNCTION lfvCredit
PARAMETERS loDetformset,loFld
loFormSet = loDetformset.loFormSet
loFrm = loDetformset.Ariaform1
loPrFrm = loFormSet.Ariaform1  && parent form

loFrm.lnCredit.Refresh()

*** check if the current account (if it's allowed for posting 
*** and an active account)
IF loFormSet.lnCredit <> 0 .AND. ! lfValidAcct(loDetFormSet)
  RETURN
ENDIF

DIMENSION laData[ALEN(loFormSet.laData)]
ACOPY(loFormSet.laData,laData)

lc_TemDet = loFormSet.lc_TemDet
SELECT (lc_TemDet)
IF loFormSet.lnCredit < 0 
  *** Negative values are not allowed. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02036B00000","DIALOG")
  loFormSet.lnCredit = loFld.OldValue
  loFrm.lcTrdtexp.Enabled = .T. && ENABLE
  RETURN .F.
ELSE  
  REPLACE &lc_TemDet..CSTATUS WITH 'M'
  IF loFormSet.lnCredit > 0 
    laData[11] = laData[11] - loFormSet.lnDebit
    laData[10] = laData[10] - loFld.OldValue
    laData[10] = laData[10] + loFormSet.lnCredit
    loFormSet.lnDebit   = 0 
    REPLACE &lc_TemDet..NAMOUNT WITH loFormSet.lnCredit ;
            &lc_TemDet..CDRORCR WITH 'C'
    loFrm.lcTrdtexp.Enabled = .T.
  ELSE  
    IF &lc_TemDet..CDRORCR = 'C'
      laData[10] = laData[10] - loFld.OldValue      
      loFormSet.lnDebit   = 0     
      loFormSet.lnCredit  = 0 
      REPLACE &lc_TemDet..NAMOUNT WITH loFormSet.lnDebit ;
              &lc_TemDet..CDRORCR WITH ' '
    loFrm.lcTrdtexp.Enabled = .F.
    ENDIF  
  ENDIF  
ENDIF

loFormSet.lnBalance = ABS(laData[11] - laData[10])
loFormSet.lcBalncID = IIF(laData[11] > laData[10],"Cr",;
            IIF(laData[11] < laData[10],"Dr",""))

IF laData[11] <> laData[10]
  laData[2] = 'O'
ELSE
  IF laData[11] = 0 
    laData[2]   = 'E'
    loFormSet.Ariaform1.cbHold.Value      = 0 
    loFormSet.lncbHold = 0 
  ELSE
    IF loFormSet.lncbHold = 1  
      laData[2] = 'H'  
    ELSE  
        laData[2] = 'U'  
    ENDIF  
  ENDIF  
ENDIF 

loFormset.lcStatus  = loFormset.laTStatus [AT(laData[2],'EOUPZHA')]

loFrm.lnCredit.Refresh()
loFrm.lnDebit.Refresh()

ACOPY(laData,loFormSet.laData)

=lfwBegDet(loDetFormSet)

*!**************************************************************************
*!
*!      Function: lfwBegDet 
*!
*!**************************************************************************
*
FUNCTION lfwBegDet
PARAMETERS loDetformset
loFormSet = loDetformset.loFormSet
loFrm = loDetformset.Ariaform1

lc_TemDet = loFormSet.lc_TemDet

loFormSet.lcAcctCode = &lc_TemDet..cAcctCode
loFormSet.lcAccDesc  = &lc_TemDet..cAccNlDes
loFormSet.lcTrdtexp  = &lc_TemDet..cTrdtexp 

IF &lc_TemDet..CDRORCR = 'C'
  loFormSet.lnCredit  = &lc_TemDet..nAmount  
  loFormSet.lnDebit   = 0 
ELSE 
  loFormSet.lnDebit   = &lc_TemDet..nAmount  
  loFormSet.lnCredit  = 0 
ENDIF

IF RECCOUNT(lc_TemDet) = 0 .OR. loFormSet.ActiveMode = 'V'
  loFrm.lcTrdtexp.Enabled = .F. && DISABLE
  loFrm.lnDebit.Enabled = .F. &&   DISABLE
  loFrm.lnCredit.Enabled = .F. &&  DISABLE
ELSE
  loFrm.lnDebit.Enabled = .T. &&   ENABLE
  loFrm.lnCredit.Enabled = .T. &&  ENABLE 
  IF &lc_TemDet..CDRORCR $ 'CD'
    loFrm.lcTrdtexp.Enabled = .T. && ENABLE
  ELSE
    loFrm.lcTrdtexp.Enabled = .F. && DISABLE
  ENDIF
ENDIF

IF  EOF(lc_TemDet)
  loFrm.lnDebit.Enabled = .F. &&    DISABLE
  loFrm.lnCredit.Enabled = .F. &&   DISABLE
ENDIF

loFrm.lcAcctCode.Enabled = .F. && DISABLE
loFrm.lcAccDesc.Enabled = .F. &&  DISABLE
SELECT GLBATCH

*!**************************************************************************
*!
*!      Function: lfvDeClose
*!
*!**************************************************************************
*
FUNCTION lfvDeClose
PARAMETERS loDetformset
loFormSet = loDetformset.loFormSet

loFormSet.lnDetRecno = RECNO(loFormSet.lc_TemDet)

=lfSwapT2M(loFormSet)
loDetformset.Release
*=gfChClose()

*!**************************************************************************
*!
*!      Function: lfSwapM2T
*!
*!**************************************************************************
*
FUNCTION lfSwapM2T
PARAMETERS loFormSet

lc_TemDet = loFormSet.lc_TemDet
lc_MasDet = loFormSet.lc_MasDet 

SELECT (lc_TemDet)
SET FILTER TO 

SELECT (lc_MasDet)

SCAN ALL 
  IF SEEK (&lc_MasDet..cAcctCode,lc_TemDet)
    SELECT (lc_TemDet)
    REPLACE ;
      &lc_TemDet..nAmount   WITH &lc_MasDet..nAmount   ;
      &lc_TemDet..cDrOrCr   WITH &lc_MasDet..cDrOrCr   ;
      &lc_TemDet..cTrdtexp  WITH &lc_MasDet..cTrdtexp  
  ENDIF
  SELECT (lc_MasDet)
ENDSCAN

SELECT (lc_TemDet)
IF loFormSet.cbInclEmty = 0 
  SET FILTER TO ! EMPTY(&lc_TemDet..nAmount)
ENDIF  

SELECT GlBATCH

*!**************************************************************************
*!
*!      Function: lfSwapT2M
*!
*!**************************************************************************
*
FUNCTION lfSwapT2M
PARAMETERS loFormSet

lc_TemDet = loFormSet.lc_TemDet
lc_MasDet = loFormSet.lc_MasDet 
gcWorkDir = oAriaApplication.WorkDir

SELECT (lc_TemDet)
SET FILTER TO 

SCAN ALL FOR &lc_TemDet..CSTATUS = 'M' 
  SELECT (lc_MasDet)
  IF SEEK (&lc_TemDet..cAcctCode,lc_MasDet)
    REPLACE ;
      &lc_MasDet..cAcctCode WITH &lc_TemDet..cAcctCode ;
      &lc_MasDet..nAmount   WITH &lc_TemDet..nAmount ;
      &lc_MasDet..cDrOrCr   WITH &lc_TemDet..cDrOrCr ;
      &lc_MasDet..cTrdtexp  WITH &lc_TemDet..cTrdtexp ;
      &lc_MasDet..cTrnpYr   WITH loFormSet.laData[4] ;
      &lc_MasDet..DtrnpDate WITH loFormSet.laData[5] ;
      &lc_MasDet..cTrnpPrd  WITH loFormSet.lcPostPrd ;           
      &lc_MasDet..cStatus   WITH 'M'
  ELSE 
    INSERT INTO &gcWorkDir.&lc_MasDet ;
           (cAcctCode,nAmount,cTrdtexp,cDrOrCr,;
           cTrnpYr,DtrnpDate,cTrnpPrd,cStatus);
    VALUES (&lc_TemDet..cAcctCode, ;
           &lc_TemDet..nAmount, ;
           &lc_TemDet..cTrdtexp, ;              
           &lc_TemDet..cDrOrCr, ;
           loFormSet.laData[4],loFormSet.laData[5], ;
           loFormSet.lcPostPrd,'A')
  ENDIF
  SELECT (lc_TemDet)
ENDSCAN

SELECT (lc_TemDet)
IF loFormSet.cbInclEmty = 0 
  SET FILTER TO ! EMPTY(&lc_TemDet..nAmount)
ENDIF  

SELECT GlBATCH

*!**************************************************************************
*!
*!      Function: lfwSelType
*!
*!**************************************************************************
*
FUNCTION lfwSelType
PARAMETERS loFormSet

SELECT  cTypeCode+" "+cTypeDesc;
  FROM GLTYPES;
  INTO ARRAY loFormSet.laAccTypes

DIMENSION loFormSet.laAccTypes[ALEN(loFormSet.laAccTypes,1)+1,1]
=AINS(loFormSet.laAccTypes,1)
loFormSet.laAccTypes[1,1] = '----------------- All Types -----------------'


*!**************************************************************************
*!
*!      Function: lfvSelType
*!
*!**************************************************************************
*
FUNCTION lfvSelType
PARAMETERS loDetformset,loFld
loFormSet = loDetformset.loFormSet
loFld.Refresh()

lc_TemDet = loFormSet.lc_TemDet
gcWorkDir = oAriaApplication.WorkDir

IF SUBSTR(loFormSet.lcActType,1,3) = loFormSet.lcSelType
  RETURN
ELSE
  =lfSwapT2M(loFormSet)  
ENDIF 

IF SUBSTR(loFormSet.lcActType,1,3) = "---" 
  loFormSet.lcSelType =  '%'
ELSE
  IF SUBSTR(loFormSet.lcActType,2,3) = "00" 
    loFormSet.lcSelType = SUBSTR(loFormSet.lcActType,1,1) + '%'
  ELSE
    loFormSet.lcSelType = SUBSTR(loFormSet.lcActType,1,3)
  ENDIF
ENDIF

IF SUBSTR(loFormSet.lcActType,1,1) $ 'AECT'
  loFormSet.lcTypeId = 'Dr'
ELSE
  IF SUBSTR(loFormSet.lcActType,1,1) $ 'LQSI'   
    loFormSet.lcTypeId = 'Cr'
  ELSE  
    loFormSet.lcTypeId = '  '
  ENDIF
ENDIF    


****  New 
SELECT (lc_TemDet)
ZAP   && Delete old data ( if any )

SELECT GLACCHAR
loFormSet.lnSelected = 0
loFormSet.lnTotalRec = RECCOUNT("GLACCHAR")

SELECT GLACCHAR.CACCTCODE, GLACCHAR.CACCNLDES, GLACBALS.NACBOPBAL,;
    000000000000.00 AS 'NAMOUNT',' ' AS 'CDRORCR','S' AS 'CSTATUS',;
    SPACE(40) AS 'cTrdtexp',RECNO() AS 'NRECNO',lfTherm(loFormSet) ;
    FROM GLACCHAR, GLACBALS;
    INTO  DBF &gcWorkDir.&lc_TemDet ;
    WHERE GLACBALS.CACCTCODE +GLACBALS.CFISFYEAR+GLACBALS.CFSPPRDID= ;
    GLACCHAR.CACCTCODE+loFormSet.laData[4]+SUBSTR(loFormSet.lcPostPrd,1,2);
    .AND. GLACCHAR.CTYPECODE LIKE loFormSet.lcSelType 

IF loFormSet.lnTotalRec <> loFormSet.lnSelected
  FOR lnCounter = loFormSet.lnSelected TO loFormSet.lnTotalRec STEP 10 
    =lfShowProgress(loFormSet.lnTotalRec,lnCounter,'','')
  ENDFOR   
ENDIF
=lfShowProgress(loFormSet.lnTotalRec,loFormSet.lnTotalRec,'','')

IF SUBSTR(loFormSet.lcSelType,2,1) = '%'
  loFormSet.lcSelType = SUBSTR(loFormSet.lcActType,1,1) + '00'
ELSE
  IF SUBSTR(loFormSet.lcSelType,1,1) = '%'
    loFormSet.lcSelType =  '---'
  ENDIF
ENDIF

loFormSet.lnDebit   = 0 
loFormSet.lnCredit  = 0 
loFormSet.lcTrdtexp = " " 

SELECT (lc_TemDet)
INDEX ON  cAcctCode TAG cAccTem
SET ORDER TO TAG cAccTem
LOCATE

=lfSwapM2T(loFormSet)  
=lfvInclEmp(loDetformset)

*- repaint the grid
lfSetGridDataSource(loDetformset)

*: B610241,1 HIA 02/12/2013 GL- Beginning Balances issue [T20130123.0034][Start]
loDetformset.Ariaform1.lnDebit.Enabled = .T.
loDetformset.Ariaform1.lnCredit.Enabled = .T.
*: B610241,1 HIA 02/12/2013 GL- Beginning Balances issue [T20130123.0034][End]
*!**************************************************************************
*!
*!      Function: lfvNew
*!
*!**************************************************************************
*
FUNCTION lfvNew
PARAMETERS loFormSet


IF loFormSet.ActiveMode = 'V'
  lcScFields = loFormSet.lcScFields
  SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData BLANK
ENDIF  
loFormSet.laData[1] = "" 
loFormSet.ChangeMode('A')


*!**************************************************************************
*!
*!      Function: lfvEntries
*!
*!**************************************************************************
*
FUNCTION lfvEntries
PARAMETERS loFormSet

IF ! lfCheckRec(loFormSet)   && check the current batch status.
  RETURN
ENDIF  

*- populate type array
=lfwSelType(loFormSet)

lcRunScx = lfGetScx("GL\GLBGBLDT.scx")
DO FORM (lcRunScx) WITH loFormSet


*!**************************************************************************
*!
*!      Function: lfwSourJor
*!
*!**************************************************************************
*
FUNCTION lfwSourJor
PARAMETERS loFormSet

gcDataDir = oAriaApplication.DataDir
SELECT  cSrcJrnl+" "+cJorlnDes;
        FROM &gcDataDir.GLSUBJOR;
        INTO ARRAY loFormSet.laSourcJor

IF ALEN(loFormSet.laSourcJor,1) = 1 .AND. TYPE('loFormSet.laSourcJor[1]') = 'L'
  loFormSet.laSourcJor = " "
  loFormSet.laSourcJor[1,1] = loFormSet.lcSJ_Def
ELSE
  IF ASCAN(loFormSet.laSourcJor,loFormSet.lcSJ_Def) = 0
    DIMENSION loFormSet.laSourcJor[ALEN(loFormSet.laSourcJor,1)+1,1]
    =AINS(loFormSet.laSourcJor,1)
    loFormSet.laSourcJor[1,1] = loFormSet.lcSJ_Def
  ENDIF  
ENDIF

*!**************************************************************************
*!
*!      Function: lfvSourJor
*!
*!**************************************************************************
*
FUNCTION lfvSourJor
PARAMETERS loDetformset,loFld
loFormSet = loDetformset.loFormSet

*!**************************************************************************
*!
*!      FUNCTION: lfvInclEmp
*!
*!**************************************************************************
*
FUNCTION lfvInclEmp
PARAMETERS loDetformset,loFld
loFormSet = loDetformset.loFormSet

lc_TemDet = loFormSet.lc_TemDet
SELECT (lc_TemDet)

IF loFormSet.cbInclEmty = 0 
  SET FILTER TO ! EMPTY(&lc_TemDet..nAmount)
ELSE 
  SET FILTER TO 
ENDIF  

loFormSet.lnInclEmty = loFormSet.cbInclEmty
=lfwBegDet(loDetFormSet)

*!**************************************************************************
*!
*!      FUNCTION: lpDelScr
*!
*!**************************************************************************
*
FUNCTION lpDelScr
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)

IF lfCheckRec(loFormSet)   && check the current batch status.
  loFormSet.ChangeMode('S')
  SELECT GLBATCH
  REPLACE GLBATCH.cBatStat WITH 'V'
  =gfAdd_Info('GLBATCH')
  gfTableUpdate()
  =gfObj_Lock(.F.) 
ENDIF  

SELECT (lnSlct)
*!**************************************************************************
*!
*!      FUNCTION: lfValidAcct
*!
*!**************************************************************************
*
*
FUNCTION lfValidAcct
PARAMETERS loDetformset
loFormSet = loDetformset.loFormSet
loFrm = loDetformset.Ariaform1

llValid = .T.
lc_TemDet = loFormSet.lc_TemDet
SELECT GLACCHAR
SET ORDER TO ACCTCODE
=SEEK (&lc_TemDet..cAcctCode)

IF GLACCHAR.cSegActiv = 'I'
  *** Account � is Inactive. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02052B00000","DIALOG",ALLTRIM(&lc_TemDet..cAcctCode))
  llValid = .F.
ENDIF

IF GLACCHAR.cSegAlPos = 'N'
  *** Account � is not allowed for posting from G.L. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02054B00000","DIALOG",ALLTRIM(&lc_TemDet..cAcctCode))
  llValid = .F.
ENDIF

IF llValid 
  RETURN llValid
ELSE

  DIMENSION laData[ALEN(loFormSet.laData)]
  ACOPY(loFormSet.laData,laData)

  SELECT (lc_TemDet)
  loFormSet.lnDebit   = 0     
  loFormSet.lnCredit  = 0 

  IF &lc_TemDet..CDRORCR = 'D'
    laData[11] = laData[11] - &lc_TemDet..NAMOUNT 
  ELSE
    laData[10] = laData[10] - &lc_TemDet..NAMOUNT
  ENDIF  

  REPLACE &lc_TemDet..NAMOUNT WITH loFormSet.lnDebit ;
          &lc_TemDet..CDRORCR WITH ' '     ;
          &lc_TemDet..CSTATUS WITH 'M'

  loFormSet.lnBalance = ABS(laData[11] - laData[10])
  loFormSet.lcBalncID = IIF(laData[11] > laData[10],"Cr",;
              IIF(laData[11] < laData[10],"Dr",""))

  IF laData[11] <> laData[10]
    laData[2] = 'O'
    loFormSet.Ariaform1.cbHold.Value      = 0 
    loFormSet.lncbHold    = 0 
    SHOW GET cbHold DISABLE
  ELSE
    IF laData[11] = 0 
      laData[2]   = 'E'
      loFormSet.Ariaform1.cbHold.Value      = 0 
      loFormSet.lncbHold    = 0 
      SHOW GET cbHold DISABLE
    ELSE
      IF loFormSet.lncbHold = 1  
        laData[2] = 'H'  
        SHOW GET cbHold DISABLE
      ELSE  
        laData[2] = 'U'  
        SHOW GET cbHold ENABLE 
      ENDIF  
    ENDIF  
  ENDIF 

  loFormset.lcStatus  = loFormset.laTStatus [AT(laData[2],'EOUPZHA')]
  
  ACOPY(laData,loFormSet.laData)
  
  loFrm.lcTrdtexp.Enabled = .F.  
  =lfwBegDet(loDetFormSet)
  RETURN llValid
ENDIF

*!**************************************************************************
*!
*!      FUNCTION: lfTherm
*!
*!**************************************************************************
*
FUNCTION lfTherm
PARAMETERS loFormSet
loFormSet.lnSelected = loFormSet.lnSelected + 1 

IF loFormSet.lnSelected <> 1    && First Time
  =lfShowProgress(loFormSet.lnTotalRec,loFormSet.lnSelected,'Collecting batch lines...','')
ENDIF  
	
************************************************************
*! Name      : lfShowProgress	
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/08/2012
*! Purpose   : Show the Progress bar
************************************************************	
FUNCTION lfShowProgress	
PARAMETERS lnTotRecs, lnThermRec,lcFirstLabel,lcSndLable
lcFirstLabel  = IIF(EMPTY(lcFirstLabel ), ' ' , lcFirstLabel )
lcSndLable    = IIF(EMPTY(lcSndLable   ), ' ' , lcSndLable   )
IF lnTotRecs = lnThermRec
  loFormSet.oProgress = NULL
  RETURN 
ENDIF 

IF ISNULL(loFormSet.oProgress)
  loFormSet.oProgress = NEWOBJECT('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
ENDIF   

WITH loFormSet.oProgress
  .TotalProgress = lnTotRecs
  .lblFirstLabel.CAPTION = lcFirstLabel
  .lblSecondLabel.CAPTION = lcSndLable
  .CurrentProgress(lnThermRec)
  .SHOW()
ENDWITH   
*- End of lfShowProgress	.

************************************************************
*! Name      : lfFormBeforeSave
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/06/2012
*! Purpose   : Before save fucntion 
************************************************************
FUNCTION lfFormBeforeSave
PARAMETERS loFormSet

IF glSetUp.lSetCnTot .AND. EMPTY(loFormSet.laData[7])
  *** You have to enter the audit total. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02146B00000","DIALOG")
   llcSave = .F.
  loFormSet.Ariaform1.laData7.Setfocus()
  RETURN llcSave
ENDIF
*- End of lfFormBeforeSave.

*!**************************************************************************
*!
*!      FUNCTION: lpSavScr
*!
*!**************************************************************************
*** local FUNCTION to save the current batch
*
FUNCTION lpSavScr
PARAMETERS loFormSet

*** check if the user is forced to enter the audit total filed 
*** in case of ladata[7] is empty.
lc_MasDet = loFormSet.lc_MasDet 
gcDataDir = oAriaApplication.DataDir
gcWorkDir = oAriaApplication.WorkDir
lcScFields = loFormSet.lcScFields

=lfSwapT2M(loFormSet)

SELECT (lc_MasDet)
DELETE ALL FOR EMPTY(&lc_MasDet..nAmount)  .AND.  ;
               EMPTY(&lc_MasDet..nRecNo )
           
IF loFormSet.laData[2] <> 'H'   && HOLD
  IF loFormSet.laData[11] = loFormSet.laData[10]
    IF loFormSet.laData[10] = 0 
      loFormSet.laData[2] = 'E'
    ELSE   
      loFormSet.laData[2] = 'U'
    ENDIF
  ELSE  
    loFormSet.laData[2] = 'O'
  ENDIF  
ENDIF  

lcTime = gfGetTime() 
ldDate = DATE()
SELECT (lc_MasDet)

REPLACE ALL &lc_MasDet..cAdd_User WITH oAriaApplication.User_ID  ;
            &lc_MasDet..dAdd_Date WITH ldDate     ;
            &lc_MasDet..cAdd_Time WITH lcTime     ;
            &lc_MasDet..cStatus  WITH    ;
               IIF(EMPTY(&lc_MasDet..nRecNo ),'A',;
               IIF(EMPTY(&lc_MasDet..nAmount),'D',&lc_MasDet..cStatus))    

IF loFormSet.ActiveMode = 'A'
  SELECT GLBATCH
  APPEND BLANK

  loFormSet.laData[1]  = gfSequence('CBATCHNO')

  loFormSet.lcTranNo   = gfSequence('CTRANNO')

  SELECT (lc_MasDet)
  REPLACE ALL &lc_MasDet..cBatchNo  WITH loFormSet.laData[1] ;
              &lc_MasDet..cTranNo   WITH loFormSet.lcTranNo 

  SELECT GLBATCH  
  GATHER FROM loFormSet.laData FIELDS &lcScFields 
  
  INSERT INTO &gcDataDir.GLTRNSHD   ;
             (cBatchNo,cTranNo,cTrnDesc,cTrnRefer, ;
              dTrnpDate,cTrnpYr,cTrnpPrd,cTrnStat, ;
              cTrnType,nTrnTotDr,nTrnTotCr,cSrcJrnl, ;
              cSrcModul,cComp_ID,cTrnRever,cStandard, ;              
              cAdd_User,dAdd_Date,cAdd_Time) ;
      VALUES (loFormSet.laData[1],loFormSet.lcTranNo,loFormSet.lcTranDes,loFormSet.lcTranRef, ;
              loFormSet.laData[5],loFormSet.laData[4],loFormSet.lcPostPrd,loFormSet.laData[2], ;
              loFormSet.laData[3],loFormSet.laData[11],loFormSet.laData[10],SUBSTR(loFormSet.lcSourcJor,1,2), ;
              oAriaApplication.ActiveModuleid,oAriaApplication.ActiveCompanyID,'N','Y',oAriaApplication.User_ID,ldDate,lcTime)
  

  loFormSet.lcBStamp  = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time 
  SELECT GLTRNSDT
  APPEND FROM &gcWorkDir.&lc_MasDet

  *** Beginning balance batch saved with number �. ***
  *** <  Ok  > 
  =gfModalGen("TRM02161B00000","DIALOG",loFormSet.laData[1])
ELSE
  SELECT (lc_MasDet)
  REPLACE ALL &lc_MasDet..cBatchNo  WITH loFormSet.laData[1] ;
              &lc_MasDet..cTranNo   WITH loFormSet.lcTranNo 
  DELETE FOR  &lc_MasDet..cStatus = "D" 

  SELECT GLBATCH  
  GATHER FROM loFormSet.laData FIELDS &lcScFields 
  
  SELECT  GLTRNSHD
 
  REPLACE GLTRNSHD.cTrnDesc   WITH loFormSet.lcTranDes ;
          GLTRNSHD.cTrnRefer  WITH loFormSet.lcTranRef ;
          GLTRNSHD.cTrnStat   WITH loFormSet.laData[2] ;
          GLTRNSHD.nTrnTotDr  WITH loFormSet.laData[11];
          GLTRNSHD.nTrnTotCr  WITH loFormSet.laData[10];
          GLTRNSHD.cSrcJrnl   WITH SUBSTR(loFormSet.lcSourcJor,1,2) ;
          GLTRNSHD.cTrnRever  WITH 'N'       ;
          GLTRNSHD.cStandard  WITH 'Y'       ; 
          GLTRNSHD.cAdd_User  WITH oAriaApplication.User_ID ;
          GLTRNSHD.dAdd_Date  WITH ldDate    ;
          GLTRNSHD.cAdd_Time  WITH lcTime    ; 

  SELECT GLBATCH
  =gfTmp2Mast('GLTRNSDT',lc_MasDet)
ENDIF

SELECT GLBATCH
RETURN 

*!**************************************************************************
*!
*!      FUNCTION: lfCheckRec
*!
*!**************************************************************************
*
FUNCTION lfCheckRec
PARAMETERS loFormSet

loFormSet.oToolBar.cmdDelete.Enabled = loFormSet.lcObjStatus = 'ENABLE'

lcScFields = loFormSet.lcScFields
*** function to check the batch status.

IF ! loFormSet.ActiveMode = 'V' 
  RETURN
ENDIF  

IF EMPTY(loFormSet.laData[1])
  RETURN
ENDIF

IF RECNO() <= RECCOUNT()
  GOTO RECNO()  
ENDIF  

*** check if the displayed batch is removed (voided) by another workstation.
IF loFormSet.laData[1] <> glbatch.cBatchNo
  *** This � was � by another user . ***
  *** <  Ok  > ***
  =gfModalGen("TRM02187B00000","DIALOG","batch"+'|'+"voided")
  SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData BLANK
  loFormSet.ChangeMode('S')
  
  RETURN .F.
ENDIF

IF ( loFormSet.lcBStamp = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time ) ;
   .AND. ( loFormSet.laData[2] = glbatch.cbatstat )
  RETURN .T.
ENDIF

lcAdd_User = ALLTRIM(LOOKUP(SYUUSER.cUsr_Name,glBatch.cAdd_User,SYUUSER.cUser_Id))
lcAdd_Time = glBatch.cAdd_Time
lcAdd_Date = dtoc(glBatch.dAdd_Date)
lcPos_User = ALLTRIM(LOOKUP(SYUUSER.cUsr_Name,glBatch.cPostUser,SYUUSER.cUser_Id))
lcPos_Time = glBatch.cPostTime
lcPos_Date = dtoc(glBatch.dPostDate)

=gfObj_Lock(.F.) 

IF glbatch.cbatstat = 'V' 
  *** This � was � by user � at �. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02167B00000","DIALOG",;
     "batch"+'|'+"voided"+'|'+lcAdd_User+'|'+lcAdd_Date+' '+lcAdd_Time)
  SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData BLANK
  loFormSet.ChangeMode('S')
  
  RETURN .F.
ENDIF

*** check if the displayed batch was approved by another user 
*** from another or same workstaion.
IF glbatch.cbatstat = 'A'
  *** This � was � by user � at �. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02167B00000","DIALOG",;
     "batch"+'|'+"approved"+'|'+lcAdd_User+'|'+lcAdd_Date+' '+lcAdd_Time)
  SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData 
  loFormSet.lcBStamp     = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time 
  loFormSet.ChangeMode('E')
  
  RETURN .F.
ENDIF

*** check if the displayed batch was posted by another user 
*** from another or same workstaion.
IF glbatch.cbatstat = 'P'
  *** This � was � by user � at �. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02167B00000","DIALOG",;
     "batch"+'|'+"posted"+'|'+lcPos_User+'|'+lcPos_Date+' '+lcPos_Time)
  SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData 
  loFormSet.lcBStamp     = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time 
  loFormSet.ChangeMode('E')
  
  RETURN .F.
ENDIF

IF glbatch.cbatstat $ 'HOEU'
  *** � status has been changed by user � at �. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02181B00000","DIALOG",;
  "batch"+'|'+lcAdd_User+'|'+lcAdd_Date+' '+lcAdd_Time)
  SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData 
  loFormSet.lcBStamp  = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time 
  
  RETURN .F.
ENDIF

*!**************************************************************************
*!
*!      FUNCTION: gfCpEdit
*!
*!**************************************************************************
*
FUNCTION gfCpEdit

IF RECNO() <= RECCOUNT()
  GOTO RECNO()  
ENDIF  

IF glbatch.cbatstat $ 'APV'
  *-
ELSE
  DO gfCpEdit IN AAS_WIN.EXE
ENDIF

************************************************************
*! Name      : lfFormAfterRowColumnChange
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/08/2012
*! Purpose   : AfterRowColumnChange method for the detail grid
************************************************************
FUNCTION lfFormAfterRowColumnChange
PARAMETERS loDetformset,loFld
loFormSet = loDetformset.loFormSet
lc_TemDet = loFormSet.lc_TemDet
WITH loDetformset.Ariaform1
  .lnDebit.Value = IIF(&lc_TemDet..CDRORCR='D',&lc_TemDet..nAmount,0.00)
  .lnCredit.Value = IIF(&lc_TemDet..CDRORCR='C',&lc_TemDet..nAmount,0.00)
  .lcAcctcode.Keytextbox.Value = &lc_TemDet..cAcctCode
  .lcAccDesc.Value = &lc_TemDet..cAccnlDes
  .lcTrdtexp.Value = &lc_TemDet..cTrdtexp
  .lcTrdtexp.Enabled = !EMPTY(&lc_TemDet..NAMOUNT)
ENDWITH 
*- End of lfFormAfterRowColumnChange.

************************************************************
*! Name      : lfDetFormSetInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/08/2012
*! Purpose   : Detail FormSet Init method
************************************************************
FUNCTION lfDetFormSetInit
PARAMETERS loDetformset
loFormSet = loDetformset.loFormSet
lc_TemDet = loFormSet.lc_TemDet

WITH loDetformset.Ariaform1
  .Caption = loFormSet.lc_Title
  
  *- Set variables control sources
  .lnDebit.ControlSource    = 'Thisformset.loFormSet.lnDebit'
  .lnCredit.ControlSource   = 'Thisformset.loFormSet.lnCredit'
  .lcTranNo.ControlSource   = 'Thisformset.loFormSet.lcTranNo'
  .lcTranRef.ControlSource  = 'Thisformset.loFormSet.lcTranRef'
  .lcTranDes.ControlSource  = 'Thisformset.loFormSet.lcTranDes'
  .lcAccDesc.ControlSource  = 'Thisformset.loFormSet.lcAccDesc'
  .lcTrdtexp.ControlSource  = 'Thisformset.loFormSet.lcTrdtexp'
  .lcTrdtexp.MaxLength = LEN(&lc_TemDet..cTrdtexp)

  .laSourcJor.RowSource = 'Thisformset.loFormSet.laSourcJor'
  .laSourcJor.ControlSource = 'Thisformset.loFormSet.lcSourcJor'
  
  .laAccTypes.RowSource = 'Thisformset.loFormSet.laAccTypes'
  .laAccTypes.ControlSource = 'Thisformset.loFormSet.lcActType'
  
  .lcAcSegDes.Caption = loFormSet.lcAcSegDes
  
  *- Adjust columns width in the popups listed
  LOCAL lcW 
  lcW = ALLTRIM(STR(.laAccTypes.Width - 25 ))
  .laAccTypes.ColumnWidths = '&lcW'
  lcW = ALLTRIM(STR(.laSourcJor.Width - 25 ))
  .laSourcJor.ColumnWidths = '&lcW'
  
  .lnDebit.Enabled = loFormSet.ActiveMode $ 'AE'
  .lnCredit.Enabled = loFormSet.ActiveMode $ 'AE'

  .Refresh()

ENDWITH
*: B610241,1 HIA 02/12/2013 GL- Beginning Balances issue [T20130123.0034][Start]
loDetformset.Ariaform1.lnDebit.Enabled = .F.
loDetformset.Ariaform1.lnCredit.Enabled = .F.
*: B610241,1 HIA 02/12/2013 GL- Beginning Balances issue [T20130123.0034][End]
*- Set grid columns sources
lfSetGridDataSource(loDetformset)

*- End of lfDetFormSetInit.

************************************************************
*! Name      : lfSetGridDataSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 22/05/2012
*! Purpose   : Set the Grid Data Source
************************************************************
FUNCTION lfSetGridDataSource
PARAMETERS loDetformset
loFormSet = loDetformset.loFormSet

oGrd = loDetformset.Ariaform1.grdGLBGBLDT
oGrd.ColumnCount = 4

oGrd.RecordSource = ''
oGrd.RecordSource = loFormSet.lc_TemDet

lc_TemDet = loFormSet.lc_TemDet
lfSetColumnsProp(oGrd,'1',"&lc_TemDet..cAcctCode" ,loFormSet.lcAcSegDes,100)
lfSetColumnsProp(oGrd,'2',"&lc_TemDet..cAccnlDes" ,'Description'       ,200)

lcSrc = [IIF(&lc_TemDet..CDRORCR='C','(',' ')+STR(&lc_TemDet..nAmount,15,2)+IIF(&lc_TemDet..CDRORCR='C',')',' ')]
lfSetColumnsProp(oGrd,'3',lcSrc       ,'Add To'                        ,100)

lcSrc = [IIF(SIGN(&lc_TemDet..nAcBopBal)=-1,'(',' ')+STR(ABS(&lc_TemDet..nAcBopBal),18,2)+IIF(SIGN(&lc_TemDet..nAcBopBal)=-1,')',' ')]
lfSetColumnsProp(oGrd,'4',lcSrc       ,'Current balance'               ,100)

oGrd.ReadOnly = .T.

IF loFormSet.lnDetRecno = 0
  GO TOP IN (loFormSet.lc_TemDet)
ELSE
  IF BETWEEN(loFormSet.lnDetRecno,1,RECCOUNT(loFormSet.lc_TemDet))
    GOTO (loFormSet.lnDetRecno) IN (loFormSet.lc_TemDet)
  ENDIF 
ENDIF   

*- Refresh the bottom of the screen
lfFormAfterRowColumnChange(loDetformset)

oGrd.Refresh()

*- End of lfSetGridDataSource.

************************************************************
*! Name      : lfSetColumnsProp
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/23/2012
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

