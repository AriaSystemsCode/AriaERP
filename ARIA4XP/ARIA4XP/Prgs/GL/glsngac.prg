*:************************************************************************
*:  Program File: ARIA4XP\PRGS\GL\GLSNGAC.PRG
*:  Module      : General Ledger
*:  Desc.       : Single Account Screen
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 04/05/2012
*:  Reference   : *E303104,1
*:************************************************************************
PARAMETERS pcAcontCd       && If calling from the account validation.
*- Call the screen
lcRunScx = lfGetScx("GL\GLSNGAC.scx")
IF !EMPTY(pcAcontCd)
  PRIVATE oScr
  DO FORM (lcRunScx) WITH pcAcontCd NAME oScr NOSHOW 
  IF TYPE('oScr')='O' AND !ISNULL(oScr)
    oScr.Show(1)
  ENDIF
ELSE 
  DO FORM (lcRunScx) WITH pcAcontCd
ENDIF   

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

loFormSet.AddProperty('lcProgName','GLSNGAC')

*- Set functions to the APMAIN.FXP
lcPath = oAriaapplication.ApplicationHome
SET PROCEDURE TO (lcPath+'AP\APMAIN.FXP') ADDITIVE 
SET PROCEDURE TO (lcPath+'GL\GL.FXP') ADDITIVE 

*- Open tables 
=lfOpenPRGFILES(loFormSet.lcProgName)

IF !lfGL(loFormset)
  RETURN .F.
ENDIF 

SELECT GLTYPES
GO TOP
IF EOF()
  *** The types and ranges have not ***
  *** been setup yet.  You have to  ***
  *** define the accounts type and ranges first. ***
  *** < Ok > ***
  =gfModalGen("TRM02038B00000","DIALOG")
  
  RETURN .F.
ENDIF

*** Load program base file 
=lfAddProp(loFormSet,'lcBaseFile',ALLTRIM(sydObjct.cBaseFile))
loFormSet.lcFile_Ttl = LOOKUP(SYDFILES.CFILE_TTL,loFormset.lcBaseFile,SYDFILES.CFILE_NAM)

*- initializations
WITH loFormSet
  .cbrowsetabledbengine   = "NATIVE"
  .nWorkArea                            = .lcBaseFile 
  .otoolbar.nWorkArea                   = .lcBaseFile
  .DataEnvironment.InitialSelectedAlias = .lcBaseFile
  .cBrowseFileName        = .lcBaseFile
  .cBrowseIndexExpression = "CACCTCODE"
  .cBrowseIndexFields     = "CACCTCODE"
  .cBrowseIndexName       = "ACCTCODE"
  .cBrowseAliasName       = .lcBaseFile
  .cBrowseTableName       = .lcBaseFile
  .cBrowseFilter          = ""
  .BrowseTitle 		  	    = loFormSet.lcFile_Ttl 
  .ariaBrFields.edtBrowseFields.Value = gfDbfField(.nWorkArea)
  .Ariaform1.lcAcSegDes.Caption = .lcAcSegDes
ENDWITH 


*** To know if the prog. calling from the menu or account validation. ***
*** to know which screen to call the modal screen or the nonmodal one ***
llSngMd = IIF(TYPE("pcAcontCd") $ "UL",.F.,.T.)
lfAddProp(loFormSet,'llSngMd',llSngMd)

**** Single Account Arrays ****
lfAddProp(loFormSet,'laRecNo[1]',0)
DECLARE loFormSet.laRecNo   [1,1]    && Array to hold the record no to delete.

*** The status popup array ***
DECLARE laTStatus [2,2]
laTStatus [1,1] = "Active"
laTStatus [1,2] = "A"
laTStatus [2,1] = "Inactive"
laTStatus [2,2] = "I"
=lfDefineArray(loFormSet,'laTStatus')

*** The categry popup array ***
DECLARE laTCatgry [4,2]
laTCatgry [1,1] = "Cash"
laTCatgry [1,2] = "C"
laTCatgry [2,1] = "Non cash"
laTCatgry [2,2] = "N"
laTCatgry [3,1] = "Depreciation"
laTCatgry [3,2] = "D"
laTCatgry [4,1] = "Amortization"
laTCatgry [4,2] = "A"
=lfDefineArray(loFormSet,'laTCatgry')

*** The term popup array ***
DECLARE laTTerm   [2,2]
laTTerm   [1,1] = "Long term"
laTTerm   [1,2] = "L"
laTTerm   [2,1] = "Current"
laTTerm   [2,2] = "C"
=lfDefineArray(loFormSet,'laTTerm')

****  Entries Arrays ****
DECLARE laCompYrs [1,1]    && Array to hold years for this company.
DECLARE laYerPer  [1,2]    && array to hold all the periods for these years.
DECLARE laPrdNo   [1,1]    && array to hold all the periods for these years.
DECLARE laAppDesc [1,2]    && Collect Applications descriptions.
DECLARE laAppID   [1,1]    && Hold Applications ID.
DECLARE laSJDesc  [1,2]    && Collect source journal descriptions.
DECLARE laSorcJor [1,1]    && Hold source journals ID.

*B120239,1 MMM 12/03/2003 Fix the bug of not showing the entries for the source Journales that 
*B120239,1                Have short description. 
DECLARE laSJorDes [1,1]    && Hold Source Journals Desc
*B120239,1 MMM [End]

DECLARE laAppl    [1,1]    && Hold Applications ID
DECLARE laSorcID  [1,1]    && Hold source journals ID.

**** Balances Arrays ****
DECLARE laYrPrBal [1,2]    && Array to hold all the periods for these years.
lfAddProp(loFormSet,'laBalance[1]','')

*- Define needed variables.
=lfDefineVars(loFormSet)

=lfAddProp(loFormSet,'lcCursor1,lcCursor2','')
loFormSet.lcCursor1  = gfTempName()     && Get a random name for the 1st temp. file.
loFormSet.lcCursor2  = gfTempName()     && Get a random name for the 2nd temp. file.
  
gcDataDir = oAriaApplication.DataDir
lcScfields = loFormset.lcScFields
SELECT GLACCHAR
SCATTER FIELDS &lcScFields MEMO TO laData BLANK
laData[1]  = REPLICATE("0",loFormSet.lnAcsSegSz)
  
*** Get the compny years that available in the fiscal year header file. ***
SELECT cFisfyear FROM &gcDataDir.FISHD;
       INTO ARRAY laCompYrs;
       ORDER BY cFisfyear ;
       DISTINCT
=lfDefineArray(loFormSet,'laCompYrs')  

*** Get the current company years {previous-current-next } and ***
*** the  historical years  and its  periods to use them in the ***
*** balances screen popups in a two dimention array.
SELECT cFisfyear,cFspprdid ;
       FROM &gcDataDir.FSPRD;
       WHERE ASCAN(laCompYrs,cFisfyear) > 0 ;
       INTO ARRAY laYerPer;
       ORDER BY cFisfyear,cFspprdid ;
       DISTINCT
  
*** Delete the years column.
IF !EMPTY(laYerPer[1,1])
  =gfADel(@laYerPer,1,2)
ELSE
  DECLARE laYerPer[1,1]
  laYerPer = " "
ENDIF
=lfDefineArray(loFormSet,'laYerPer')  

*** Get the current company years {previous-current-next } and ***
*** its periods to use them in the balances screen popups in a ***
*** two dimention array.
SELECT DISTINCT CFISFYEAR,CFSPPRDID;
       FROM &gcDataDir.FSPRD;
       WHERE VAL(CFISFYEAR) >= loFormSet.lnCurr_yer - 1;
       ORDER BY CFISFYEAR,CFSPPRDID;
       INTO ARRAY laYrPrBal
=lfDefineArray(loFormSet,'laYrPrBal')  
   
*** collect the no. of periods for each year. ***
SELECT cFisNoPrd,cFisfyear FROM &gcDataDir.FISHD;
       INTO ARRAY laPrdNo;
       ORDER BY cFisfyear

IF !EMPTY(laPrdNo[1,1])
  =gfADel(@laPrdNo,2,2)
ELSE
  DECLARE laPrdNo[1,1]
  laPrdNo = " "
ENDIF
=lfDefineArray(loFormSet,'laPrdNo')

*** Get the available source journals ID and its descriptions. ***
SELECT GLSUBJOR.CSRCJRNL,;
       IIF(EMPTY(cjorshdes),;
       PADR(csrcjrnl,FSIZE('cjorshdes'),' '),;
       cjorshdes) AS 'cjorshdes';
  FROM &gcDataDir.GLSUBJOR ;
  INTO ARRAY laSJDesc

*** Separate the two dimentional arrayinto two arrays each one ***
*** is one dimentional array, one for ID and the other for the ***
*** descriptions.
IF !EMPTY(laSJDesc[1,1])
  DECLARE laSorcID[ALEN(laSJDesc,1),2]
  =ACOPY(laSJDesc,laSorcID)
  =gfADel(@laSorcID,2,2)  &&riptions in a two ***
  =gfADel(@laSJDesc,1,2)
ELSE
  DECLARE laSorcID[1,1],laSJDesc[1,1]
  STORE " " TO laSorcID,laSJDesc
ENDIF

*** Collect the available module and its desc    
*** dimentional array.
SELECT DISTINCT SyGLTran.cSrcModul,SydAppl.cApp_Name ;
  FROM SyGLTran,SydAppl ;
  INTO ARRAY laAppDesc  ;
  WHERE SyGLTran.cSrcModul = SydAppl.cApp_ID

IF !EMPTY(laAppDesc[1,1])
  SELECT SycComp
  PRIVATE lcCurTag
  lcCurTag = ORDER()
  LOCATE FOR CCOMP_ID = oAriaApplication.ActiveCompanyID
  IF FOUND()
    PRIVATE lnI,lnDelCnt,lnArLen
    STORE 0 TO lnDelCnt,lnArLen
      
    *B602645,1 WALID [BEGIN]
    * The following code will not fix the problem of deleting an 
    * elements from the source module array as follow
    * when lnI=8 the condition !(laAppDesc[lnI,1] $ SycComp.mModlSet)
    * is .T. so laAppDesc[8,1] will be deleted and laAppDesc[9,1]
    * laAppDesc[9,1] will automatically be shifted to be laAppDesc[8,1]
    * Now lnI=9 but lnArLen is modified to be 8 so DO WHILE loop
    * will termenated without check laAppDesc[9,1] .
    * 

    DIMENSION laDumm(ALEN(laAppDesc,1),1)
    FOR lnI = 1 TO ALEN(laAppDesc,1)
      *B607401,1 ALB Add SM Modual to Gl source option to display Closing Transaction [Begin]
      *IF !(laAppDesc[lnI,1] $ SycComp.mModlSet)
      IF !(laAppDesc[lnI,1] $ (SycComp.mModlSet+'|SM'))
        laDumm[LNI,1] = .T.
        lnDelCnt = lnDelCnt + 1
      ENDIF
    ENDFOR
    IF lnDelCnt > 0 
      lnDelCnt =0
      FOR lnI = 1 TO ALEN(laDumm,1)
        IF laDumm[LNI,1]
        =ADEL(laAppDesc,lnI)
        lnDelCnt = lnDelCnt+1
        ENDIF
      ENDFOR
      DIMENSION laAppDesc[ALEN(laAppDesc,1)-lnDelCnt,2]
    ENDIF
  ELSE
    STORE " " TO laAppDesc
  ENDIF

  SET ORDER TO &lcCurTag
ENDIF  
  
*** Separate the two dimentional array into two arrays.  each one ***
*** is one dimentional array, one for applications ID & the other ***
*** one for the applications descriptions.
IF !EMPTY(laAppDesc[1,1])
  DECLARE laAppID[ALEN(laAppDesc,1),2]
  =ACOPY(laAppDesc,laAppID)
  =gfADel(@laAppID,2,2)
  =gfADel(@laAppDesc,1,2)
ELSE
  DECLARE laAppID[1,1],laAppDesc[1,1]
  STORE " " TO laAppID,laAppDesc
ENDIF

*** Put all the applications as defaulted selected in the mover ***
*** and in the string that hold the moduls id in the SQL command ***
DECLARE laAppl[ALEN(laAppID,1),1]  
FOR lnCount = 1 TO ALEN(laAppID,1)
  loFormset.lcAppIdStr = IIF(EMPTY(loFormset.lcAppIdStr),loFormset.lcAppIdStr,loFormset.lcAppIdStr+",") + laAppID[lnCount]
 * laAppl[lnCount,1]  = ALLTRIM(LOOKUP(SYDAPPL.cApp_name,laAppID[lnCount,1],SYDAPPL.cApp_id,'cApp_id'))
  laAppl[lnCount,1]  = ALLTRIM(laAppDesc[lnCount,1])
ENDFOR
=lfDefineArray(loFormSet,'laAppDesc')
=lfDefineArray(loFormSet,'laAppID')
=lfDefineArray(loFormSet,'laAppl')

lcSJ_IDStr   = "" 
DIMEN laSorcJor[ALEN(laSJDesc,1),1]
FOR lnCount  = 1 TO ALEN(laSJDesc,1)
    laSorcJor[lnCount,1] = laSJDesc[lnCount,1]
    lcSJ_IDStr = lcSJ_IDStr + IIF(EMPTY(lcSJ_IDStr) , "" , "," );
                 + ALLTRIM(laSorcJor[lnCount,1])
ENDFOR
=lfDefineArray(loFormSet,'laSJDesc')
=lfDefineArray(loFormSet,'laSorcJor')
  
  
*B120239,1 MMM 12/03/2003 Fix the bug of not showing the entries for the source Journales that 
*B120239,1                Have short description. [Start] 
lcSJ_IDStr   = ""
DIMENSION laSJorDes[ALEN(laSorcID,1),1]
FOR lnCount  = 1 TO ALEN(laSorcID,1)
    laSJorDes[lnCount,1] = laSorcID[lnCount,1]
    lcSJ_IDStr = lcSJ_IDStr + IIF(EMPTY(lcSJ_IDStr) , "" , "," );
                 + ALLTRIM(laSJorDes[lnCount,1])
ENDFOR  
=lfDefineArray(loFormSet,'laSJorDes')
=lfDefineArray(loFormSet,'laSorcID')
loFormSet.lcSJ_IDStr = lcSJ_IDStr

*** Get the GLACBALS tag. ***
SELECT GLACBALS
SET ORDER TO TAG ACCYRPRD
loFormSet.lcExp = SYS(14,VAL(SYS(21)))
  
loFormSet.puCompYrs2 = loFormSet.lcCurr_yer
loFormSet.lcCompYrs2 = loFormSet.lcCurr_yer
 
SELECT GLACCHAR
  
loFormSet.llDispFlg = .T.

IF llSngMd

  * Disable the account code field
  loFormset.Ariaform1.laData1.Enabled = .F.
   
  *** If coming from the account validation func., call the modal screen. ***

  *** Put the account code segments in an array to scan in this ***
  *** array to get the  descriptions for each segment  for this ***
  *** account code to make its long description.
  
  DECLARE laAccSeg [1]
  =gfSubStr(ALLTRIM(pcAcontCd),@laAccSeg,"-")

  *** Get the account long description from the segment value file by ***
  *** collecting the short descriptions for the different segments.   ***
  laData[4] = ""
  FOR lnCount = 1 TO ALEN(laAccSeg,1)
    IF SEEK(STR(lnCount,1)+laAccSeg[lnCount],"GLSEGVAL")
      laData[4] = laData[4] + ;
                  IIF(EMPTY(laData[4]) .OR. RIGHT(laData[4],1)='-','','-');
                  + ALLTRIM(GLSEGVAL.cSegshdes)
      IF lnCount = 1        
        laData[1] = pcAcontCd
        *** Get the type of the first segment. ***
        loFormSet.lcTypCode = GLSEGVAL.ctypecode
        laData[3] = loFormSet.lcTypCode
        *** Get the type description. ***
        loFormSet.lcTypDesc = LOOKUP(GLTYPES.cTypedesc,loFormSet.lcTypCode,GLTYPES.cTypecode,'Typecode')

        *** Get the default data from the first segment. ***
        laData[2]  = GLSEGVAL.cSegshdes + SPACE(5)
        
        *B607108,1 KHM 05/07/2003 (Begin) commented out.
        *B804399,1 Get the long description. [Begin]
        *laData[4] = GLSEGVAL.cSegLndes + SPACE(25)
        *B804399,1 Get the long description. [End]
        *B607108,1 KHM 05/07/2003 (End)
        
        laData[5]  = GLSEGVAL.cSegterm
        laData[6]  = GLSEGVAL.cSegaccat
        laData[7]  = GLSEGVAL.cSegactiv
        laData[8]  = GLSEGVAL.cSegalpos
        laData[9]  = GLSEGVAL.cSegratio
        laData[10] = GLSEGVAL.cSegcaflo
        laData[11] = GLSEGVAL.cSegautds
        laData[12] = GLSEGVAL.cSegcocac
        laData[13] = GLSEGVAL.nSegseqn
        laData[14] = GLSEGVAL.nSegconpr
      ENDIF
    ENDIF
  ENDFOR
  laData[4] = ALLTRIM(laData[4]) + SPACE(65 - LEN(laData[4]))

  
  lcPopTerm = IIF(GLSETUP.lSetacate .AND. LEFT(laData[3],1) $ 'AL',;
              "ENABLE","DISABLE")
  lcTerm    = IIF(AT(laData[5],"LC") > 0,;
              laTTerm[AT(laData[5],"LC"),1],"N/A")
  ibPopTerm = IIF(AT(laData[5],"LC") > 0,;
              AT(laData[5],"LC"),"N/A")

  lcPopCatg = IIF(GLSETUP.lSetacate .AND. LEFT(laData[3],1) = 'A',;
              "ENABLE","DISABLE")
  lcCatgry  = IIF(AT(laData[6],"CNDA") > 0 ,;
              laTCatgry[AT(laData[6],"CNDA"),1],"N/A" )
  ibPopCatg = IIF(AT(laData[6],"CNDA") > 0 ,;
              AT(laData[6],"CNDA"),"N/A")

  lcPopStat = IIF(GLSETUP.lSetacate,"ENABLE","DISABLE")
  lcStatus  = IIF(AT(laData[7],"AI") > 0 ,;
              laTStatus[AT(laData[7],"AI"),1],"N/A" )
  ibPopStat = IIF(AT(laData[7],"AI") > 0 ,;
              AT(laData[7],"AI"),"N/A")

  lcPopRato = IIF(GLSETUP.lSetacate .AND. LEFT(laData[3],1) <> 'Y',;
              "ENABLE","DISABLE")
  lcPopCash = IIF(GLSETUP.lSetacate .AND. LEFT(laData[3],1) <> 'Y',;
              "ENABLE","DISABLE")
    
  cbPost = IIF(laData[8] = "Y",1,0)
  loFormSet.Ariaform1.cbPost.Value = cbPost
  
  *- apply enable/disable
  WITH loFormSet.Ariaform1
    .ibPopTerm.Enabled = ( lcPopTerm = 'ENABLE' )
    .ibPopCatg.Enabled = ( lcPopCatg = 'ENABLE' )
    .ibPopStat.Enabled = ( lcPopStat = 'ENABLE' )
    .puRatio.Enabled = ( lcPopRato = 'ENABLE' )
    .puCash.Enabled  = ( lcPopCash = 'ENABLE' )   
  ENDWITH   
  
  SELECT GLRACOD
  
  SELECT CRATDESC,CSEGRATIO FROM GLRACOD WHERE ;
  CRATTYPE = LEFT(laData[3],1) .OR. CRATTYPE = " " ;
  INTO ARRAY loFormSet.laRatio

  SELECT cCfidesc,CSEGCAFLO FROM GLCFITEM INTO ARRAY loFormSet.laCash 
  
  *** Get the ratio group desc. according to ratio group code. ***
  IF !EMPTY(laData[9])
    IF SEEK(LEFT(laData[3],1)+laData[9],"GLRACOD")
      lcRatioDes = GLRACOD.cRatDesc
      puRatio    = GLRACOD.cRatDesc
    ELSE
      lcRatioDes = "N/A"
      puRatio    = "N/A"
    ENDIF
  ELSE
    lcRatioDes = "N/A"
    puRatio    = "N/A"
  ENDIF

  *** Get the cash flow desc. according to cash flow code. ***
  puCash = LOOKUP(GLCFITEM.cCfidesc,laData[10],GLCFITEM.cSegcaflo,'CFICODE')
  puCash = IIF(EMPTY(puCash) , "N/A" , puCash)

  SELECT GLACCHAR

  *** Go directly to the add mode if coming from the account ***
  *** validation func.
  laData[1]    = pcAcontCd
  lcAccStat    = "DISABLE"
  
  DIMENSION loFormSet.laData[ALEN(laData)]
  =ACOPY(laData,loFormset.laData)

  *- Define control source 
  =lfDefControlSource(loFormSet)
  
  loFormSet.ChangeMode('A')

  *- Call GLSNGMD as a Modal form
  WITH loFormset.ariaform1
    .pbBalance.Caption = '\<Ok'
    .pbBalance.Enabled = .T.

    .pbEntry.Caption = '\<Cancel'
    .pbEntry.Enabled = .T.
    .pbEntry.Cancel = .T.    
  ENDWITH  

ELSE
  *** If branched from the menu call the nonmodal screen. ***
  
  lcPopTerm = IIF(loFormSet.ActiveMode=='S' .OR. loFormSet.ActiveMode=='V',"DISABLE",;
              IIF(GLSETUP.lSetacate .AND. LEFT(laData[3],1) $ 'AL',;
              "ENABLE","DISABLE"))
  lcPopCatg = IIF(loFormSet.ActiveMode=='S' .OR. loFormSet.ActiveMode=='V',"DISABLE",;
              IIF(GLSETUP.lSetacate .AND. LEFT(laData[3],1) = 'A',;
              "ENABLE","DISABLE"))
  lcPopRato = IIF(loFormSet.ActiveMode=='S' .OR. loFormSet.ActiveMode=='V',"DISABLE",;
              IIF(GLSETUP.lSetacate .AND. LEFT(laData[3],1) <> 'Y',;
              "ENABLE","DISABLE"))
  lcPopCash = IIF(loFormSet.ActiveMode=='S' .OR. loFormSet.ActiveMode=='V',"DISABLE",;
              IIF(GLSETUP.lSetacate .AND. LEFT(laData[3],1) <> 'Y',;
              "ENABLE","DISABLE"))
  lcPopStat = IIF(loFormSet.ActiveMode=='S' .OR. loFormSet.ActiveMode=='V',"DISABLE",;
              IIF(GLSETUP.lSetacate,"ENABLE","DISABLE"))
  *- apply enable/disable
  WITH loFormSet.Ariaform1
    .ibPopTerm.Enabled = ( lcPopTerm = 'ENABLE' )
    .ibPopCatg.Enabled = ( lcPopCatg = 'ENABLE' )
    .ibPopStat.Enabled = ( lcPopStat = 'ENABLE' )
    .puRatio.Enabled = ( lcPopRato = 'ENABLE' )
    .puCash.Enabled  = ( lcPopCash = 'ENABLE' )
  ENDWITH   
  
  DIMENSION loFormSet.laData[ALEN(laData)]
  =ACOPY(laData,loFormset.laData)

  cbPost = IIF(laData[8] = "Y",1,0)

  SELECT GLRACOD
  SET FILTER TO 

  SELECT CRATDESC,CSEGRATIO FROM GLRACOD WHERE ;
  CRATTYPE = LEFT(laData[3],1) .OR. CRATTYPE = " " ;
  INTO ARRAY loFormSet.laRatio

  SELECT cCfidesc,CSEGCAFLO FROM GLCFITEM INTO ARRAY loFormSet.laCash  
  
  SELECT GLACCHAR

  =lfDefControlSource(loFormSet)
  
  *- Open the screen in Select mode
  loFormset.ChangeMode('S')

ENDIF


************************************************************
*! Name      : lfFormActivate
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/14/2012
*! Purpose   : Form Activate
************************************************************
FUNCTION lfFormActivate
PARAMETERS loFormSet

*- Hide the toolbar in the MODAL mode
IF TYPE('loFormSet.pcAcontCd') = 'C'
  loFormSet.oToolBar.Visible =.F.
  loFormSet.Ariaform1.ControlBox = .F.
ENDIF 

*- End of lfFormActivate.


************************************************************
*! Name      : lfDefControlSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/08/2012
*! Purpose   : Define Control Source
************************************************************
FUNCTION lfDefControlSource
PARAMETERS loFormSet
WITH loFormSet.Ariaform1
  .laData1.KeyTextBox.ControlSource = 'Thisformset.laData[1]'
  .lcTypCode.ControlSource = 'Thisformset.laData[3]'
  .lcTypDesc.ControlSource = 'Thisformset.lcTypDesc'
  .txtSDesc.ControlSource = 'Thisformset.laData[2]'
  .txtLDesc.ControlSource = 'Thisformset.laData[4]'
  
  .ibPopTerm.ControlSource = 'Thisformset.laData[5]'
  .ibPopCatg.ControlSource = 'Thisformset.laData[6]'
  .ibPopStat.ControlSource = 'Thisformset.laData[7]'
  
  **.cbPost.ControlSource = 'Thisformset.laData[8]'
  .cbPost.Value = IIF(loFormSet.laData[8]='Y',1,0)
  .puRatio.ControlSource = 'Thisformset.laData[9]'
  .puCash.ControlSource = 'Thisformset.laData[10]'
  
  .kbAutoDistCode.KeyTextBox.ControlSource = 'Thisformset.laData[11]'
  .txtConsCoAct.ControlSource = 'Thisformset.laData[12]'
  .txtConsCoAct.InputMask = REPLICATE('X', FSIZE('CSEGCOCAC','glacchar') )
  
  .txtRepSeqNo.ControlSource = 'Thisformset.laData[13]'
  .txtConsPrcnt.ControlSource = 'Thisformset.laData[14]'
  
  .ibPopTerm.RowSource = 'Thisformset.laTTerm'
  lfColumnWidthes( .ibPopTerm )
  .ibPopCatg.RowSource = 'Thisformset.laTCatgry'
  lfColumnWidthes( .ibPopCatg )
  .ibPopStat.RowSource = 'Thisformset.laTStatus'
  lfColumnWidthes( .ibPopStat )
  .puRatio.RowSource = 'Thisformset.laRatio'
  lfColumnWidthes( .puRatio )
  .puCash.RowSource = 'Thisformset.laCash'
  lfColumnWidthes( .puCash )
ENDWITH
*- End of lfDefControlSource.

************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : Define vars
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormset

lcScFields = 'CACCTCODE,CACCNSDES,CTYPECODE,CACCNLDES,CSEGTERM,CSEGACCAT,CSEGACTIV,CSEGALPOS,'+;
             'CSEGRATIO,CSEGCAFLO,CSEGAUTDS,CSEGCOCAC,NSEGSEQN,NSEGCONPR,CSTANDARD'
loFormSet.Addproperty('lcScFields',lcScFields)

loFormSet.Addproperty('lcModName'  , "General ledger"   )&& String to store general ladger module name

loFormSet.Addproperty('llDispFlg'  , .F.)    && to display all obj. if 1st seg.

loFormSet.Addproperty('lcTypCode'  , ""   )     && Var to hold code type.
loFormSet.Addproperty('lcTypDesc'  , ""   )     && Var to hold code description.
loFormSet.Addproperty('lcAccDes'   , ""   )     && To combine account desc.
loFormSet.Addproperty('lcOldAuto'  , ""   )     && Hold the old automatic distribution.
loFormSet.Addproperty('lcOldAcc'   , ""   )     && Hold the old account.
loFormSet.Addproperty('llChild'    , .F.  )     && Var use in the child screen.

loFormSet.Addproperty('lcRatioDes' , ""   )     && Ratio group say var.
loFormSet.Addproperty('puRatio'    , ""   )     
loFormSet.Addproperty('puRatioGrp' , ""   )     
loFormSet.Addproperty('lcCashFDes' , ""   )     && Cash flow say var.
loFormSet.Addproperty('puCash'     , ""   )     
loFormSet.Addproperty('puCashFlow' , ""   )     
loFormSet.Addproperty('lcTerm'     , ""   )     && Term say var.
loFormSet.Addproperty('lcCatgry'   , ""   )     && Categry say var.
loFormSet.Addproperty('lcStatus'   , ""   )     && Status say var.
loFormSet.Addproperty('cbPost'     , 1    )     && Default value for allow posting check box.

*** Varibles related to the Entry screen ***    
loFormSet.Addproperty('lcOldYear'  , ""   )     && Var to hold old year.
loFormSet.Addproperty('lcOldPer'   , ""   )     && Var to hold old period.
loFormSet.Addproperty('lnFrstElm'  , 1    )     && Var to hold the first element in the period popup.
loFormSet.Addproperty('lnElmNo'    , 1    )     && Var to hold no. of elements in the period popup.
loFormSet.Addproperty('lcAppIdStr' , ""   )     && Var to hold the App. ID to select the data.
loFormSet.Addproperty('lcSJ_IdStr' , ""   )     && Var to hold the source journal to select the data.
loFormSet.Addproperty('lcBrowEntr' , ""   )     && Var to hold the fields name for the browse.
*loFormSet.Addproperty('lcCursor1'  , ""   )     && Var hold the name of the temp. file used in entries.
loFormSet.Addproperty('lcWid1'     , ""   )     && Width of batch no. field.
loFormSet.Addproperty('lcWid2'     , ""   )     && Width of transaction # field.
loFormSet.Addproperty('lcWid3'     , ""   )     && Width of source modul field.
loFormSet.Addproperty('lcWid4'     , ""   )     && Width of date field.
loFormSet.Addproperty('lcWid5'     , ""   )     && Width of debit or credit amount.
loFormSet.Addproperty('lcWid6'     , ""   )     && Width of source journal field.

*** Varibles related to the Detail screen ***
loFormSet.Addproperty('lcTypCode2' , ""   )     && Var to hold the code type.
loFormSet.Addproperty('lcBrowDet'  , ""   )     && Var to hold the fields name for the browse.
loFormSet.Addproperty('lcCursor2'  , ""   )     && Var to hold name of the temp. file used in details.
loFormSet.Addproperty('lcWidD1'    , ""   )     && Width of debit amount.
loFormSet.Addproperty('lcWidD2'    , ""   )     && Width of credit amount.

*** Varibles related to the Balance screen ***
loFormSet.Addproperty('puCompYrs' , ""   )     && Var of the popup hold the year.

loFormSet.Addproperty('puCompYrs2' , ""   )     && Var of the popup hold the year.
loFormSet.Addproperty('lcCompYrs2' , ""   )     && Var of the popup hold the year.
loFormSet.Addproperty('lcOldYear2' , ""   )     && Var to hold old year.
loFormSet.Addproperty('lcExp'      , ""   )     && To save index expr. of glbalance.
loFormSet.Addproperty('laBalance'  , ""   )     && Put in all the array element none value.
loFormSet.Addproperty('lsBalance'  , 1    )     && default var for the list.
loFormSet.Addproperty('llEndBal'   , .F.  )     && To display specific string in balance screen.
loFormSet.Addproperty('llCUpdate'  , .F.  )     && To be checked in Cancel

*lcTbalance = ""        && Hold the balances screen title.
loFormSet.Addproperty('lcTbalance' , "Balances"   )
loFormSet.Addproperty('lnWid_Fld'  , 16   )      
loFormSet.Addproperty('cbSummary'  , .F.  )     && Variable of the summary check box.
loFormSet.Addproperty('cbSummaryEn', ''   )     && Variable of the summary check box.
loFormSet.Addproperty('llBrowse'   , .F.  )  
loFormSet.Addproperty('lcAccount'  , ''   )     && Variable to hold the account to compair with.

loFormSet.Addproperty('lcPopTerm' , " "  )          
loFormSet.Addproperty('lcPopCatg' , " "  )       
loFormSet.Addproperty('lcPopStat' , " "  )          
loFormSet.Addproperty('lcPopRato' , " "  )       
loFormSet.Addproperty('lcPopCash' , " "  )           

*E302219,1 ALB Display massage to user that the account is closed with zero value [BEGIN]
loFormSet.Addproperty('lcAcctStat', ''   )         
*E302219,1 ALB Display massage to user that the account is closed with zero value [END]

*** Var to hold the child window name. ***

*** Function in the main program ***
*** Intialise all the variables & open all the files needed   ***
*** in this session and controling disabling and enabling of  ***
*** the menu bars and writting the screen names in the window ***
*** bars .

*- Add a variable for the Entries button state
loFormSet.Addproperty('lcBalStat'   , ''   )       

*- Define two arrays to hold the Ration & Cash row source
loFormSet.Addproperty('laRatio[1,2]', ''   )       
loFormSet.Addproperty('laCash[1,2]' , ''   )       

*- Define laBalance array to show up in the Balance screen
loFormSet.Addproperty('laBalance[5]','')

*- Balances title 
loFormSet.Addproperty('lcEntTitl','')

*- End of lfDefineVars.

************************************************************
*! Name      : lfChangeMode
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/12/2012
*! Purpose   : change mode function
************************************************************
Function lfChangeMode
PARAMETERS loFormSet

IF TYPE('loFormSet.lnAcsSegSz')='U'
  RETURN 
ENDIF    

SELECT GLACCHAR
lcScFields = loFormSet.lcScFields
loFormset.Ariaform1.laData1.Enabled = .F.

DO CASE
  CASE loFormSet.ActiveMode='S'
    SCATTER FIELDS &lcScFields TO laData BLANK
    =ACOPY(laData,loFormSet.laData)
  
    *** Select mode ***
    *** Put default for var. & popups & check boxes. ***
    WITH loFormSet.Ariaform1
    .puRatio.Value = '00'
    .puCash.Value = '000'
    
    .ibPopTerm.Value = loFormSet.laTTerm[1,2]
    
    .ibPopCatg.Value = loFormset.laTCatgry[1,2]
    
    .ibPopStat.Value = loFormset.laTStatus[1,2]
    
    .cbPost.Value     = 1
    loFormSet.cbSummary  = .F.
    
    *** Redeclare the balance array to put new data in it. ***
    DIMENSION loFormSet.laBalance [1,1]
    loFormSet.laBalance  = ""
    
    *** Put zeros in the account mask. ***
    loFormSet.laData[1]  = REPLICATE("0",loFormSet.lnAcsSegSz)   
    .laData1.KeyTextBox.Value = loFormSet.laData[1]
    .laData1.Enabled = .T.

    .lcTypCode.Value  = ""
    .lcTypDesc.Value  = ""

    .ibPopTerm.Enabled = .F.
    .ibPopCatg.Enabled = .F.
    .ibPopStat.Enabled = .F.
    .puRatio.Enabled = .F.
    .puCash.Enabled = .F.
    
    ENDWITH 
    loFormset.Ariaform1.laData1.Setfocus()
    
  CASE loFormSet.ActiveMode='V'
    SCATTER FIELDS &lcScFields TO laData
    =ACOPY(laData,loFormSet.laData)
    
    WITH loFormSet.Ariaform1 
    *** View mode ***
    .ibPopTerm.Enabled = .F.
    .ibPopCatg.Enabled = .F.
    .ibPopStat.Enabled = .F.

    .puRatio.Enabled = .F.
    .puCash.Enabled = .F.
    
    cbPost = IIF(laData[8] = "Y",1,0)
    loFormSet.Ariaform1.cbPost.Value = cbPost
  
    .pbBalance.Enabled = .T.
    .pbEntry.Enabled = .T.

    *** Get the type code & desc. ***
    lcTypCode  = laData[3]
    loFormSet.lcTypDesc  = LOOKUP(GLTYPES.cTypedesc,laData[3],GLTYPES.cTypecode,'Typecode')

    *** Gather data from Balance file ***
    loFormSet.cbSummary  = .F.
    loFormSet.puCompYrs2 = loFormSet.lcCurr_yer
    loFormSet.lcCompYrs2 = loFormSet.lcCurr_yer
    lcAccPic   = STRTRAN(loFormSet.lcAcsMask,'9','0')
    IF AT("-",laData[1]) = 0
      lcFrstSeg  = laData[1]
    ELSE
      lcFrstSeg  = SUBSTR(laData[1],1,AT("-",laData[1])-1)
    ENDIF

    loFormSet.lcAccount  = STUFF(lcAccPic,0,AT("-",laData[1])-1,lcFrstSeg)
    .Refresh()
    ENDWITH 
    =lfBalQuery(loFormSet)

    SELECT GLACCHAR
    loFormSet.Checknavigation()
    loFormSet.otoolbar.NavRefresh()

  CASE loFormSet.ActiveMode='E'
  
    DIMENSION laData[ALEN(loFormSet.laData)]
    =ACOPY(loFormSet.laData,laData)

    *** Edit mode ***
    *** Set filter to account type in the ratio group file. ***
    SELECT GLRACOD
    SET FILTER TO 

    SELECT CRATDESC,CSEGRATIO FROM GLRACOD WHERE ;
    CRATTYPE = LEFT(laData[3],1) .OR. CRATTYPE = " " ;
    INTO ARRAY loFormSet.laRatio

    SELECT cCfidesc,CSEGCAFLO FROM GLCFITEM INTO ARRAY loFormSet.laCash  
    
    SELECT GLACCHAR
    
    WITH loFormSet.Ariaform1
    IF GLSETUP.lSetacate .AND. LEFT(laData[3],1) $ 'AL'
      .ibPopTerm.Enabled = .T. &&ENABLE
    ELSE
      .ibPopTerm.Enabled = .F. &&DISABLE
    ENDIF

    IF GLSETUP.lSetacate .AND. LEFT(laData[3],1) = 'A'
      .ibPopCatg.Enabled = .T. &&ENABLE
    ELSE
      .ibPopCatg.Enabled = .F. &&DISABLE
    ENDIF
    IF GLSETUP.lSetacate .AND. LEFT(laData[3],1) <> 'Y'
      .puRatio.Enabled = .T. &&ENABLE
      .puCash.Enabled = .T. &&EnABLE
    ELSE
      .puRatio.Enabled = .F. &&DISABLE
      .puCash.Enabled = .F. &&DISABLE
    ENDIF
    IF GLSETUP.lSetacate
      .ibPopStat.Enabled = .T. &&ENABLE
    ELSE
      .ibPopStat.Enabled = .F. &&DISABLE
    ENDIF

    .pbBalance.Enabled = .T. &&ENABLE
    .pbEntry.Enabled = .T. &&ENABLE
    ENDWITH 

  CASE loFormSet.ActiveMode = 'A'
    DIMENSION laData[ALEN(loFormSet.laData)]
    =ACOPY(loFormSet.laData,laData)

    *** Add mode ***
    llCUpdate = .T.

    *** Copy the default account value from segment ***
    *** file according to the first segment data.
    IF AT("-",laData[1]) = 0
      lcFrstSeg  = laData[1]
    ELSE
      lcFrstSeg = SUBSTR(laData[1],1,AT("-",laData[1])-1)
    ENDIF
    IF SEEK("1"+lcFrstSeg,"GLSEGVAL")
      laData[2]  = GLSEGVAL.cSegshdes + SPACE(5)

      *- Get the long description. [Begin]
      laData[4] = GLSEGVAL.cSegLndes + SPACE(25)
      
      laData[5]  = GLSEGVAL.cSegterm
      laData[6]  = GLSEGVAL.cSegaccat
      laData[7]  = GLSEGVAL.cSegactiv
      laData[8]  = GLSEGVAL.cSegalpos
      laData[9]  = GLSEGVAL.cSegratio
      laData[10] = GLSEGVAL.cSegcaflo
      laData[11] = GLSEGVAL.cSegautds
      laData[12] = GLSEGVAL.cSegcocac
      laData[13] = GLSEGVAL.nSegseqn
      laData[14] = GLSEGVAL.nSegconpr
   ENDIF
    
   laData[3]  = loFormset.lcTypCode
   
   laData[15] = IIF(SUBSTR(laData[3],1,1) = 'Y' , 'N' , 'Y')
   
   cbPost     = IIF(laData[8] = "Y",1,0)
   loFormSet.Ariaform1.cbPost.Value = cbPost
   
   *** Set filter to account type in ratio groups file. ***
   SELECT GLRACOD
   SET FILTER TO

   SELECT CRATDESC,CSEGRATIO FROM GLRACOD WHERE ;
   CRATTYPE = LEFT(laData[3],1) .OR. CRATTYPE = " " ;
   INTO ARRAY loFormSet.laRatio
   
   SELECT cCfidesc,CSEGCAFLO FROM GLCFITEM INTO ARRAY loFormSet.laCash  
     
   SELECT GLACCHAR
   
   WITH loFormSet.Ariaform1
   IF GLSETUP.lSetacate .AND. LEFT(laData[3],1) $ 'AL'
     .ibPopTerm.Enabled = .T. &&ENABLE
   ELSE
     .ibPopTerm.Enabled = .F. &&DISABLE
   ENDIF
 
   IF GLSETUP.lSetacate .AND. LEFT(laData[3],1) = 'A'
     .ibPopCatg.Enabled = .T. &&ENABLE
   ELSE
     .ibPopCatg.Enabled = .F. &&DISABLE
   ENDIF
 
   IF GLSETUP.lSetacate .AND. LEFT(laData[3],1) <> 'Y'
     .puRatio.Enabled = .T. &&ENABLE
     .puCash.Enabled = .T. &&ENABLE
   ELSE
     .puRatio.Enabled = .F. &&DISABLE
     .puCash.Enabled = .F. &&DISABLE
   ENDIF
   IF GLSETUP.lSetacate
     .ibPopStat.Enabled = .T. &&ENABLE
   ELSE
     .ibPopStat.Enabled = .F. &&DISABLE
   ENDIF
     
   .pbEntry.Enabled = .F. &&DISABLE
   .pbBalance.Enabled = .F. &&DISABLE
   ENDWITH 
     
   =ACOPY(laData,loFormset.laData)
 
ENDCASE
  
**** Refresh the child window objects ***
loFormSet.Ariaform1.Refresh()  
loFormSet.lsBalance = 1
  
*** If calling this program from the menu, balance screen ***
*** should  be exist, so enable all  its objects.  But if ***
*** calling from account validation, we cannot do this.
  
*** If there is no balances for any record disable the balances button. ***
IF ALEN(loFormset.laBalance,1) = 1 .AND. EMPTY(loFormset.laBalance[1,1])
  loFormSet.ariaform1.pbBalance.Enabled = .F.
  loFormSet.lcBalStat        = 'DISABLE'
ELSE
  loFormSet.ariaform1.pbBalance.Enabled = .T.
  loFormSet.lcBalStat        =  'ENABLE'
ENDIF
  
IF loFormSet.laData[1] <> loFormSet.lcAccount
  loFormSet.cbSummary = .F.
  loFormSet.cbSummaryEn = 'DISABLE'
ELSE
  loFormSet.cbSummaryEn = 'ENABLE'
ENDIF
=lfwAcctStat(loFormSet)
  
SELECT GLACCHAR
RETURN 
*- End of lfChangeMode.
  

*!**************************************************************************
*!Function: lfvData_1
*! Auther : TMI
*! Date   : 07/14/2012
*! Purpose:*** laData[1] valid function. {The account code}
*!**************************************************************************
FUNCTION lfvData_1 
PARAMETERS loFormSet,loFld

loFormSet.laData[1] = loFld.keytextbox.value
llBrowse = loFld.Selectedfrombrowse
IF llBrowse
  loFormSet.laData[1] = STUFF(loFormSet.laData[1],1,1,'?')
  *- make key field value the same as laData[1]
  loFld.Keytextbox.Value = loFormSet.laData[1] 
ENDIF

IF llBrowse .OR. !EMPTY(CHRTRAN(loFormSet.laData[1],'0-',''))

  llFoundAcn = .F.     && To know if the acc. found in its file or not. ***
  
  *------------------------------------------*
  *** Call the account validation function ***
  *------------------------------------------*
  
  *** To add new account to the chart of account file. ***
  lcAccDes  = loFormSet.lcAccDes  
  lcTypCode = loFormSet.lcTypCode 
  lcTypDesc = loFormSet.lcTypDesc 
  
  IF lfVldAccnt(loFormSet,loFld,'A','A','A',.T.,@lcAccDes,@lcTypCode,@lcTypDesc,.T.)  AND !EMPTY(CHRTRAN(loFld.keytextbox.Value,'-0',''))
  
    *- update the related properties after calling the function 
    loFormSet.lcAccDes  = lcAccDes  
    loFormSet.lcTypCode = lcTypCode 
    loFormSet.lcTypDesc = lcTypDesc 
    loFormSet.laData[1] = loFld.Keytextbox.Value

    SELECT GLACCHAR
    IF llFoundAcn
      *** If the field not empty and found in the main file ***
      *** chart of account , it should skatter all the data ***
      *** from the main file and back to the view mode  ***
      lcScFields = loFormset.lcScFields
      SCATTER FIELDS &lcScFields MEMO TO laData
      DIMENSION loFormSet.laData[ALEN(laData)]
      =ACOPY(laData,loFormset.laData)
      loFormSet.ChangeMode('V')
    ELSE
      *** If the field not empty and not found also in the ***
      *** main file , it means adding new record , so it   ***
      *** should back to the add mode .                  ***
 
      loFormSet.ChangeMode('A')
    ENDIF
    
    WITH loFormset
    lcAccPic  = STRTRAN(.lcAcsMask,'9','0')
    IF AT("-",.laData[1]) = 0
      lcFrstSeg  = .laData[1]
    ELSE
      lcFrstSeg  = SUBSTR(.laData[1],1,AT("-",.laData[1])-1)
    ENDIF
    .lcAccount = STUFF(lcAccPic,0,AT("-",.laData[1])-1,lcFrstSeg)
    .cbSummary = .F.
    IF .laData[1] = .lcAccount
      .cbSummaryEn = 'ENABLE'
    ELSE
     .cbSummaryEn = 'DISABLE'
    ENDIF
    ENDWITH 

    * ReInitialize the Ratio group and cash flow popup. 
    
  ELSE
    *** Return with none, So back to select mode ***
    *** again to enter another account.
    RETURN .F.
  ENDIF
ENDIF
 

*!**************************************************************************
*!Function: lfvData_8
*! Auther : TMI
*! Date   : 07/14/2012
*! Purpose:*** The allow posting for G/L valid function. ***
*!**************************************************************************
FUNCTION lfvData_8
PARAMETERS loFormset,loFld

loFormset.laData[8] = IIF(loFld.Value = 0 , 'N' , 'Y')


*!**************************************************************************
*!Function: lfvData_11
*! Auther : TMI
*! Date   : 07/14/2012
*! Purpose:*** The automatic distribution valid function. ***
*!**************************************************************************
FUNCTION lfvData_11
PARAMETERS loFormSet,loFld

SELECT GLACCHAR
lnRecordNo = RECNO()

llBrowse = loFld.Selectedfrombrowse

DECLARE laAutoInfo[1,1]
WITH loFormSet
.laData[11] = PADR(ALLTRIM(.laData[11]),8)
IF llBrowse .OR. !EMPTY(.laData[11]) 

  IF llBrowse .OR. loFld.KeyTextbox.Value <> loFld.KeyTextbox.OldValue
    IF SEEK("D","GLAUTHD") .OR. llBrowse
  
      lcCurFile = ALIAS()
      SELECT GLAUTHD
      *** If found update the user name from automatic dist. file

      IF !SEEK("D"+.laData[11],"GLAUTHD") .OR. llBrowse
    
        IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
          GO RECNO(0)
        ELSE
          GO TOP
        ENDIF

        lcBrFields     =  "cautcode :H='Distribution code',cautref :H='Distribution reference',"+;
                          "cautdes  :H='Distribution description'"
        lcFile_Ttl     = "Automatic Distribution"
  
        laAutoInfo [1] = .laData[11]

        =gfBrows("'D'","cautcode" ,"laAutoInfo")
    
        *** If cancel from the brow you will come back  with the same id
        IF .laData[11] = laAutoInfo[1] 
          .laData[11] = loFld.Keytextbox.OldValue
        ELSE
          .laData[11]  = laAutoInfo[1]

          SELECT GLAUTDT
          SET ORDER TO TAG TYPCODACC
          SELECT GLACCHAR
          SET ORDER TO TAG ACCTCODE
          SELECT GLAUTDT
          SET RELATION TO glautdt.cacctcode INTO GLACCHAR ADDITIVE
          
          SELECT GLAUTDT
          IF SEEK("D"+.laData[11],"GLAUTDT")
            llAutcd_Ok = .T.
            SCAN REST WHILE GLAUTDT.cAutcode = .laData[11]
              IF GLACCHAR.cSegactiv = "I"
                *** This  automatic  distribution has an ***
                *** inactive account.  You cannot select ***
                *** this automatic distribution code. ***
                *** <  Ok  > ***
                =gfModalGen("TRM02194B00000","DIALOG")
                llAutcd_Ok = .F.
                EXIT
              ENDIF
              
              IF GLACCHAR.cStandard <> .laData[15]
                *** This automatic distribution code ***
                *** has a { standard : statistical } ***
                *** account.  You cannot select this ***
                *** automatic distribution code.
                *** <  Ok  > ***
                lcStandStr = IIF(GLACCHAR.cStandard = "Y","standard","statistical")
                =gfModalGen("TRM02195B00000","DIALOG",lcStandStr)
                llAutcd_Ok = .F.
                EXIT
              ENDIF
              
              SELECT GLAUTDT
            ENDSCAN
            IF !llAutcd_Ok
              loFld.KeyTextbox.Value  = loFld.KeyTextbox.OldValue
            ENDIF 
          ENDIF
          
          SELECT GLAUTDT
          SET RELATION TO

        ENDIF
      ENDIF  
      SELECT (lcCurFile)
    ELSE
      *** There is no records to display. ***
      *** <  Ok  > ***
      =gfModalGen("TRM00052B00000","DIALOG")
      .laData[11] = SPACE(8)
    ENDIF
  ENDIF
ENDIF

*- Refresh the field
loFld.KeyTextbox.Refresh()

SELECT GLACCHAR
IF lnRecordNo > 0 .AND. lnRecordNo <= RECCOUNT()
  GO lnRecordNo
ENDIF
ENDWITH && closing the with loformset at the start of the function


*!**************************************************************************
*!Function: lfvData_13
*! Auther : TMI
*! Date   : 07/14/2012
*! Purpose:*** The report seq. no. ***
*!**************************************************************************
FUNCTION lfvData_13
PARAMETERS loFormSet, loFld

IF loFld.Value < 0
  *** Negative values are not allowed. ***
  =gfModalGen("INM02036B00000","DIALOG")
  loFld.Value = loFld.OldValue
  RETURN .F.
ENDIF


*!**************************************************************************
*!Function: lfvData_14
*! Auther : TMI
*! Date   : 07/14/2012
*! Purpose:*** The consolidation % valid function. ***
*!**************************************************************************
FUNCTION lfvData_14
PARAMETERS loFormset, loFld

*when the user presses ESC to close the screen , the value in this fields turned into character type
IF TYPE('loFld.Value')<>'N'
  RETURN
ENDIF   

IF loFormSet.ActiveMode = 'S'
  RETURN 
ENDIF   

DO CASE
  CASE loFld.Value > 100
    *** Total percentage cannot exceed 100 %. ***
    =gfModalGen("TRM02017B00000","DIALOG","Percentage")
    loFld.Value = loFld.OldValue
    RETURN .F.
  CASE loFld.Value < 0
    *** Negative values are not allowed. ***
    =gfModalGen("TRM02036B00000","DIALOG")
    loFld.Value = loFld.OldValue
    RETURN .F.
ENDCASE
************************************************************
*! Name      : lpSavScr
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/14/2012
*! Purpose   : Save function
************************************************************
FUNCTION lpSavScr
PARAMETERS loFormSet

LOCAL lnSlct
lnSlct = SELECT(0)

SELECT GLACCHAR
*** This array element hold the field cStandard and it put in it ***
*** char. : "Y" if this account has standard type, 
*** and it put char. : "N" if this account has statistical type.
loFormSet.laData[15] = IIF(SUBSTR(loFormSet.lcTypCode,1,1) = 'Y' , 'N' , 'Y')

*** Save the record in the chart of accounts file. ***
IF loFormSet.ActiveMode = 'A'
  APPEND BLANK
ENDIF

gcDataDir = oAriaApplication.DataDir

lcScFields = loFormSet.lcScFields
DIMENSION laData[ALEN(loFormSet.laData)]
=ACOPY(loFormSet.laData,laData)
GATHER FROM laData FIELDS &lcScFields MEMO 
=gfAdd_Info('GLACCHAR')

*** Save this account balances in the balances file. ***
IF loFormSet.ActiveMode = 'A'
  SELECT GLACBALS
  FOR lnCount = 1 TO ALEN(loFormset.laYrPrBal,1)
    INSERT INTO &gcDataDir.GLACBALS ;
           (cAcctCode,cFisfYear,cFspPrdid);
           VALUES (laData[1],loFormset.laYrPrBal[lnCount,1],loFormset.laYrPrBal[lnCount,2])
    =gfAdd_Info('GLACBALS')
  ENDFOR

  *** Call the retained earnings for account major func. ***
  =lfAddRetMj(loFormset)

  SELECT GLACBALS
  =gfTableUpdate()
ENDIF

SELECT GLACCHAR
=gfTableUpdate()

SELECT(lnSlct)
*- End of lpSavScr.
************************************************************
*! Name      : lpDelScr
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/14/2012
*! Purpose   : delete function
************************************************************
FUNCTION lpDelScr
PARAMETERS loFormset
*** Call the global delete function that in the GL.PRG to ***
*** delete this account and its related data.
IF lfDelAcnt(loFormset.laData[1]) &&*<

  SELECT GLACCHAR
  =gfTableUpdate()

  loFormset.ChangeMode('S')
ELSE
  RETURN .F.  
ENDIF

*- End of lpDelScr.

*!**************************************************************************
*!Function: lfvEntry
*! Auther : TMI
*! Date   : 07/14/2012
*! Purpose:entry screen from the main screen.
*!         This is the entry push button valid func. that call the ***
*!**************************************************************************
FUNCTION lfvEntry
PARAMETERS loFormSet

*** Put defaults as current year & current period ***
*** before enter the screen. ***

loFormSet.puCompYrs  = loFormSet.lcCurr_yer
puYerPer   = RIGHT('0'+ALLTRIM(STR(loFormSet.lnCurr_Prd)),2)
lnFrstElm  = VAL(loFormSet.laPrdNo[ALEN(loFormSet.laPrdNo,1)-1])+1
lnElmNo    = VAL(loFormSet.laPrdNo[ALEN(loFormSet.laPrdNo,1)-1])

*** This call the screen with 5 parameters : { 1.current year - ***
*** 2.current period - 3.first element - 4.number of elements - ***
*** 5.flag to say that coming from the main screen}.
=lfGLQENTR(loFormSet,loFormSet.puCompYrs , puYerPer , lnFrstElm , lnElmNo , .T.)

SELECT GLACCHAR

*!**************************************************************************
*!Function: lfvBalance
*! Auther : TMI
*! Date   : 07/14/2012
*! Purpose:the child screen balances.
***        This the balance push button valid function that call ***
*!**************************************************************************
FUNCTION lfvBalance
PARAMETERS loFormSet

*** This is the balances screen title. ***
loFormSet.lcEntTitl  = "Account Balances"

*** Put the current company as a default in the popup ***
*** that exist in the balances screen.

loFormSet.puCompYrs2 = loFormSet.lcCompYrs2

lcRunScx = lfGetScx("GL\glbals.scx")
DO FORM (lcRunScx) WITH loFormSet

SELECT GLACCHAR


*!**************************************************************************
*!Function: lfvComYrsB
*! Auther : TMI
*! Date   : 07/14/2012
*! Purpose:*** The year popup valid function that exist in the balances screen. ***
*!**************************************************************************
FUNCTION lfvComYrsB
PARAMETERS loFormSet,loFld

  loFormSet.lcCompYrs2 = loFormSet.puCompYrs2

IF loFld.OldValue <> loFld.Value

  *** If change the year call the balances query function ***
  *** to collect the new year data in the laBalance array ***
  =lfBalQuery(loFormSet)

  IF ALEN(loFormSet.laBalance,1) = 1 .AND. EMPTY(loFormSet.laBalance[1,1])
    loFld.parent.pbBalEntry.Enabled = .F.
    loFormSet.lcBalStat        = 'DISABLE'
  ELSE
    loFld.parent.pbBalEntry.Enabled = .T.
    loFormSet.lcBalStat        = 'ENABLE'
  ENDIF
  *** Refresh the list exist in the balance screen. ***

  loFld.parent.lsBalance.Enabled = loFormSet.lcBalStat        = 'ENABLE'
  
  *Display massage to user that the account is closed with zero value [BEGIN]
  =lfwAcctStat(loFormSet)
ENDIF

*!**************************************************************************
*!Function: lfBalQuery
*! Auther : TMI
*! Date   : 07/14/2012
*! Purpose: the balances information for the current account code. 
*!          Balances query function that create the list array that hold ***
*!**************************************************************************
FUNCTION lfBalQuery
PARAMETERS loFormSet

DIMENSION laBalance [1,1]
DIMENSION laData[ALEN(loFormSet.laData)]
ACOPY(loFormSet.laData,laData)

laBalance = ""
gcDataDir = oAriaApplication.DataDir
lcExp = loFormSet.lcExp

lnWid_Fld = 14

IF SUBSTR(loFormSet.lcTypCode,1,1) $ ("AQLY")
  IF loFormSet.cbSummary
    lcAccount = loFormSet.lcAccount
    lcAccMaj = SUBSTR(ALLTRIM(lcAccount),1,AT('-',lcAccount)-1)+;
               STRTRAN(SUBSTR(ALLTRIM(lcAccount),AT('-',lcAccount),;
               LEN(ALLTRIM(lcAccount))-AT('-',lcAccount)+1),'0','_')
               
    SELECT CFSPPRDID+'|'+;
           IIF(SUM(NACBOPBAL) < 0,;
              '('+STR(ABS(SUM(NACBOPBAL)),lnWid_FLD,2)+')',;
              ' '+STR(SUM(NACBOPBAL),lnWid_Fld,2)+' ')+'|'+;
           STR(SUM(NACBPTDDR),lnWid_Fld,2)+'|'+;
           STR(SUM(NACBPTDCR),lnWid_FLD,2)+'|'+;
           IIF(SUM(NACBCLBAL) < 0,;
              '('+STR(ABS(SUM(NACBCLBAL)),lnWid_FLD,2)+')',;
              ' '+STR(SUM(NACBCLBAL),lnWid_FLD,2)+' ')+' ';
      FROM &gcDataDir.GLACBALS;
      WHERE GLACBALS.CACCTCODE LIKE lcAccMaj;
            AND GLACBALS.CFISFYEAR = loFormSet.lcCompYrs2;
      GROUP BY GLACBALS.CFSPPRDID;
      ORDER BY GLACBALS.CFSPPRDID;
      INTO ARRAY laBalance
  ELSE
    SELECT GLACBALS.CFSPPRDID+"|"+;
           IIF(GLACBALS.NACBOPBAL<0,'('+STR(ABS(GLACBALS.NACBOPBAL),lnWid_Fld,2)+')',;
           ' '+STR(GLACBALS.NACBOPBAL,lnWid_Fld,2)+' ' ) +"|"+;
           STR(GLACBALS.NACBPTDDR,lnWid_Fld,2)+"|"+STR(GLACBALS.NACBPTDCR,lnWid_Fld,2)+"|"+;
           IIF (GLACBALS.NACBCLBAL<0 ,'('+STR(ABS(GLACBALS.NACBCLBAL),lnWid_Fld,2)+')',;
           ' '+STR(GLACBALS.NACBCLBAL,lnWid_Fld,2)+' ');
      FROM &gcDataDir.GLACBALS ;
      INTO ARRAY laBalance;
      WHERE &lcExp = laData[1] + loFormSet.lcCompYrs2;
      ORDER BY GLACBALS.CFSPPRDID
  ENDIF 
  llEndBal  = .T.
ELSE
  IF loFormSet.cbSummary

    lcAccount = loFormSet.lcAccount
    lcAccMaj = SUBSTR(ALLTRIM(lcAccount),1,AT('-',lcAccount)-1)+;
               STRTRAN(SUBSTR(ALLTRIM(lcAccount),AT('-',lcAccount),;
               LEN(ALLTRIM(lcAccount))-AT('-',lcAccount)+1),'0','_')
               
    SELECT CFSPPRDID+'|'+;
           IIF(SUM(NACBOPBAL) < 0,;
               '('+STR(ABS(SUM(NACBOPBAL)),lnWid_FLD,2)+')',;
               ' '+STR(SUM(NACBOPBAL),lnWid_Fld,2)+' ')+'|'+;
           STR(SUM(NACBPTDDR),lnWid_Fld,2)+'|'+;
           STR(SUM(NACBPTDCR),lnWid_Fld,2)+'|'+;
           IIF(SUM(NACBYTDDR-NACBYTDCR) < 0,;
               '('+STR(ABS(SUM(NACBYTDDR-NACBYTDCR)),lnWid_FLD,2)+')',;
               ' '+STR(SUM(NACBYTDDR-NACBYTDCR),lnWid_Fld,2)+' ')+' ';
      FROM &gcDataDir.GLACBALS;
      WHERE GLACBALS.CACCTCODE LIKE lcAccMaj;
            AND GLACBALS.CFISFYEAR = loFormSet.lcCompYrs2;
      GROUP BY GLACBALS.CFSPPRDID;
      ORDER BY GLACBALS.CFSPPRDID;
      INTO ARRAY laBalance
  ELSE
    SELECT GLACBALS.CFSPPRDID+"|"+;
           IIF(GLACBALS.NACBOPBAL<0,'('+STR(ABS(GLACBALS.NACBOPBAL),lnWid_Fld,2)+')',;
           ' '+STR(GLACBALS.NACBOPBAL,lnWid_Fld,2)+' ' ) +"|"+;
           STR(GLACBALS.NACBPTDDR,lnWid_Fld,2)+"|"+STR(GLACBALS.NACBPTDCR,lnWid_Fld,2)+"|"+;
           IIF(GLACBALS.NACBYTDDR-GLACBALS.NACBYTDCR>0,;
              ' '+STR(GLACBALS.NACBYTDDR-GLACBALS.NACBYTDCR,lnWid_Fld,2)+' ',;
              '('+STR(ABS(GLACBALS.NACBYTDDR-GLACBALS.NACBYTDCR),lnWid_Fld,2)+')'); 
      FROM &gcDataDir.GLACBALS ;
      INTO ARRAY laBalance;
      WHERE &lcExp = laData[1] + loFormSet.lcCompYrs2;
      ORDER BY GLACBALS.CFSPPRDID
  ENDIF
  llEndBal  = .F.
ENDIF

*- Update the loFormSet.laBalance array
DIMENSION loFormSet.laBalance[ALEN(laBalance),5]
LOCAL i,j,lcLn
WITH loFormSet
FOR i = 1 TO ALEN(laBalance)
  lcLn = laBalance[i] + '|'
  FOR j = 1 TO 5
    .laBalance[i,j] = SUBSTR(lcLn,1,AT('|',lcLn)-1)
    lcLn = SUBSTR(lcLn,AT('|',lcLn)+1)
    IF j = 5
      IF VAL(CHRTRAN(.laBalance[i,j],'()',' ')) = 0
        .laBalance[i,j] = chrtran( .laBalance[i,j],'()',' ')
      ENDIF 
    ENDIF 
  ENDFOR 
ENDFOR 
ENDWITH 

*!**************************************************************************
*!Function: lfvBalEntr
*! Auther : TMI
*! Date   : 07/14/2012
*! Purpose:The balances screen the call the entry screen.
*!         This is the entry push button valid function from the ***
*!**************************************************************************
FUNCTION lfvBalEntr
PARAMETERS loBalFormSet

loFormSet = loBalFormSet.Callingform

LOCAL lcSavAls
lcSavAls = ALIAS()

lsBalance = loBalFormSet.Ariaform1.lsBalance

*** Put the year in the balances screen popup as default ***
*** in the entry screen popup and in the SQL command. 
loFormSet.puCompYrs  = loFormSet.lcCompYrs2

*** Put the current period in the list in the balances screen ***
*** as default period in the period popup in the entry screen ***
*** and in the SQL command.
   

puYerPer   = loFormSet.laBalance[lsBalance.ListIndex,1]

*** Define the  first element no. of elements to send as ***
*** parameters to the entry screen according to the year ***
*** if it is previous or current or next.

lnYearNo = ASCAN(loFormSet.laCompYrs,loFormSet.puCompYrs)

IF lnYearNo > 0
  IF lnYearNo = 1
    lnFrstElm = 1
  ELSE
    lnFrstElm = 1
    FOR lnCount = 1 TO lnYearNo - 1
      lnFrstElm = lnFrstElm + VAL(loFormSet.laPrdNo[lnCount])
    ENDFOR
  ENDIF
  lnElmNo = VAL(loFormSet.laPrdNo[lnYearNo])
ELSE
  lnFrstElm = 1
  lnElmNo   = VAL(loFormSet.laPrdNo[1])
ENDIF

*** This call the screen with 5 parameters : { 1.current year - ***
*** 2.current period - 3.first element - 4.number of elements - ***
*** 5.flag to say that coming from the main screen}.
=lfGLQENTR(loFormSet,loFormSet.puCompYrs , puYerPer , lnFrstElm , lnElmNo , .F.)

SELECT (lcSavAls)


*!**************************************************************************
*!FUNCTION: lfAddRetMj
*! Auther : TMI
*! Date   : 07/14/2012
*! Purpose:major in the current account.
*!         This is the retained earning for the current account
*!**************************************************************************
FUNCTION lfAddRetMj
PARAMETERS loFormSet

*** This function will be executed if three condition are true :_
***     1- The account type is one from these types :_
***               { C } Cost of sales.
***               { I } other Income.
***               { T } Taxes.
***               { E } Expenses.
***               { S } Sales.
***     2- The cost center field in the GLSETUP file is greater than 1
***     3- The Retained earning account major in the GLSETUP file is .T.


gcDataDir = oAriaApplication.DataDir
IF LEFT(loFormSet.lcTypCode,1) $ 'CITES' .AND. GLSETUP.NSETCOSTC > 1 .AND. !EMPTY(GLSETUP.CSETRETMJ)

  *** Scan in the account code strucure file to get ***
  *** the account segments no. and position. 
  SELECT ACCOD
  GO TOP
  IF !EOF()
    lnAryLen = ACCOD.NACSNOSEG
    DECLARE laSegment[lnAryLen,2]

    lnCont    = 1
    lnStrtPos = 1
    
    SKIP
  
    *ACCOD now does have company Id in its index [begin
    SCAN REST
      laSegment [lnCont,1] = lnStrtPos
      laSegment [lnCont,2] = ACCOD.NACSSIZE
      lnStrtPos            = lnStrtPos + ACCOD.NACSSIZE + 1
      lnCont               = lnCont + 1 
    ENDSCAN
  ENDIF

  *** Create the account code to be added in the chart of accounts file. ***
  lcTmpAcCod   = loFormSet.lcAcsMask

  lcTmpAcCod   = stuff(lcTmpAcCod,laSegment[1,1],laSegment[1,2],;
                 ALLTRIM(GLSETUP.CSETRETMJ))

  FOR lnCont = 2 TO GLSETUP.NSETCOSTC
    lcTmpAcCod   = stuff(lcTmpAcCod,laSegment[lnCont,1],laSegment[lnCont,2],;
                   SUBSTR(laData[1],laSegment[lnCont,1],laSegment[lnCont,2]))

  ENDFOR

  *** Put the rest of the account code as zeros. ***
  IF GLSETUP.NSETCOSTC < ALEN(laSegment,1)
    FOR lnCount  =  GLSETUP.NSETCOSTC+1 TO ALEN(laSegment,1)
      lcTmpAcCod = stuff(lcTmpAcCod,laSegment[lnCount,1],laSegment[lnCount,2],REPLICATE('0',laSegment[lnCount,2]))
    ENDFOR
  ENDIF

  *** If the created account is not exist in the chart of account file. ***
  IF !SEEK(lcTmpAcCod,"GLACCHAR")

    IF gfModalGen("QRM02060B00006","DIALOG",SUBSTR(laData[1],laSegment[2,1],laSegment[2,2])) = 1

      *** Get the first segment data from the segment  file as ***
      *** default for the new account code record that will be ***
      *** added in the chart of accounts file.
      SELECT GLSEGVAL
      IF SEEK("1"+ALLTRIM(GLSETUP.CSETRETMJ))
        SCATTER MEMVAR
        
        *** Add the new retained earnings for account major ***
        *** in the chart of account.
        SELECT GLACCHAR
        APPEND BLANK
        REPLACE cAcctcode WITH lcTmpAcCod
        GATHER MEMVAR
        =gfAdd_Info('GLACCHAR')
      
        *** Add the balances record for this account code in the ***
        *** balances file,  record for  each period according to ***
        *** the {previous - current - next} years.
        SELECT GLACBALS
        FOR lnCount = 1 TO ALEN(loFormset.laYrPrBal,1)
          INSERT INTO &gcDataDir.GLACBALS ;
                 (cAcctCode,cFisfYear,cFspPrdid);
                 VALUES (lcTmpAcCod,loFormset.laYrPrBal[lnCount,1],loFormset.laYrPrBal[lnCount,2])
          =gfAdd_Info('GLACBALS')
        ENDFOR
      ENDIF
    ENDIF    
  ENDIF
ENDIF


*!**************************************************************************
*! Name      : lfwAcctStat
*! Developer : Albert Raif (MHM)
*! Date      : 08/26/2003
*! Purpose   : Check if the year / Period closed
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfwAcctStat()
*!**************************************************************************
FUNCTION lfwAcctStat
PARAMETERS loFormSet
PRIVATE lcBalAmnt, lcKey
lcBalAmnt = ''
lcKey = loFormSet.laData[1] + loFormSet.lcCompYrs2+STR(loFormSet.lsBalance ,2)
IF loFormSet.lsBalance = ALEN(loFormSet.laBalance,1) AND SUBSTR(loFormSet.lcTypCode,1,1) $ "SETOC" AND SEEK(loFormSet.lcCompYrs2+STR(loFormSet.lsBalance ,2),"FSPRD");
   AND FSPRD.lfspclsds
   =SEEK(lcKey,'GLACBALS')
   IF GLACBALS.Nacbclbal = 0
     lcBalAmnt = 'zero'
   ELSE
     lcBalAmnt = IIF(GLACBALS.Nacbclbal>0 ,'','(')+STR(ABS(GLACBALS.Nacbclbal),18,2)+IIF(GLACBALS.Nacbclbal > 0,'',')')
   ENDIF
  loFormSet.lcAcctStat = "This Account has been closed with "+ALLTRIM(lcBalAmnt) + " balance "
ELSE
  loFormSet.lcAcctStat = ""
ENDIF

RETURN .T.

************************************************************
*! Name      : lfGLQENTR
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/17/2012
*! Purpose   : lfGLQENTR
************************************************************
FUNCTION lfGLQENTR
PARAMETERS loFormSet, puCompYrs , puYerPer , lnFrstElm , lnElmNo , llEntry
PRIVATE    puCompYrs , puYerPer , lnFrstElm , lnElmNo , llEntry

lcTitleEnt = "Account Entries"
lcTitleDet = "Transactions details"
=lfGetQuery(loFormSet)
SELECT (loFormSet.lcCursor1)
GO TOP
IF EOF()
  IF llEntry
    =gfModalGen("TRM02182B00000","DIALOG")
  ELSE
    =gfModalGen("TRM02048B00000","DIALOG")
  ENDIF
ENDIF

lcRunScx = lfGetScx("GL\GLQENTR.scx")  
DO FORM (lcRunScx) WITH loFormSet,loFormSet.puCompYrs , puYerPer , lnFrstElm , lnElmNo , .T.
*- End of lfGLQENTR.



*!**************************************************************************
*!Function: lfgetQuery
*! Auther : TMI
*! Date   : 07/14/2012
*! Purpose:*** The get query function that called when any change in the selct criteria.
*!**************************************************************************
FUNCTION lfGetQuery
PARAMETERS loFormSet

lcAccount = loFormSet.lcAccount
gcDataDir = oAriaApplication.DataDir
lcSJ_IDStr = loFormSet.lcSJ_IDStr
lcAppIdStr = loformset.lcAppIdStr

IF loFormSet.cbSummary
  lcAccMaj = SUBSTR(ALLTRIM(lcAccount),1,AT('-',lcAccount)-1)+;
             STRTRAN(SUBSTR(ALLTRIM(lcAccount),AT('-',lcAccount),;
             LEN(ALLTRIM(lcAccount))-AT('-',lcAccount)+1),'0','_')

  SELECT GLPTRNDT.CBATCHNO,GLPTRNDT.CTRANNO,GLPTRNHD.CSRCMODUL,;
         GLPTRNHD.CSRCJRNL,GLPTRNHD.DTRNPDATE,GLPTRNDT.CTRDTEXP,;
         IIF(GLPTRNDT.CDRORCR = 'D',' '+STR(GLPTRNDT.NAMOUNT,15,2)+' ',;
         '('+STR(GLPTRNDT.NAMOUNT,15,2)+')') AS 'Deb_Crd';
    FROM &gcDataDir.GLPTRNHD,&gcDataDir.GLPTRNDT;
   WHERE GLPTRNDT.CTRANNO    = GLPTRNHD.CTRANNO .AND.;
         (GLPTRNHD.CTRNPPRD  = puYerPer    .AND.;
          GLPTRNHD.CTRNPYR   = puCompYrs   .AND.;
          GLPTRNDT.CACCTCODE LIKE lcAccMaj .AND.;
          GLPTRNHD.CSRCJRNL  $ lcSJ_IDStr  .AND.;
          GLPTRNHD.CSRCMODUL $ lcAppIdStr) .AND.;
          GLPTRNHD.CTRNSTAT  $ 'PZ';
   ORDER BY GLPTRNDT.CBATCHNO, GLPTRNDT.CTRANNO,;
            GLPTRNHD.CSRCMODUL,GLPTRNHD.CSRCJRNL,;
            GLPTRNHD.DTRNPDATE;
    INTO CURSOR (loFormSet.lcCursor1)
ELSE
  SELECT GLPTRNDT.CBATCHNO,GLPTRNDT.CTRANNO,GLPTRNHD.CSRCMODUL,;
         GLPTRNHD.CSRCJRNL,GLPTRNHD.DTRNPDATE,GLPTRNDT.CTRDTEXP,;
         IIF(GLPTRNDT.CDRORCR = 'D',' '+STR(GLPTRNDT.NAMOUNT,15,2)+' ',;
         '('+STR(GLPTRNDT.NAMOUNT,15,2)+')') AS 'Deb_Crd';
    FROM &gcDataDir.GLPTRNHD,&gcDataDir.GLPTRNDT;
    INTO CURSOR (loFormSet.lcCursor1);
   WHERE GLPTRNDT.CTRANNO    = GLPTRNHD.CTRANNO .AND.;
         (GLPTRNHD.CTRNPPRD  = puYerPer    .AND.;
          GLPTRNHD.CTRNPYR   = puCompYrs   .AND.;
          GLPTRNDT.CACCTCODE = loFormSet.laData[1]   .AND.;
          GLPTRNHD.CSRCJRNL  $ lcSJ_IDStr  .AND.;
          GLPTRNHD.CSRCMODUL $ lcAppIdStr) .AND.;
          GLPTRNHD.CTRNSTAT  $ 'PZ';
   ORDER BY GLPTRNDT.CBATCHNO, GLPTRNDT.CTRANNO,;
            GLPTRNHD.CSRCMODUL,GLPTRNHD.CSRCJRNL,;
            GLPTRNHD.DTRNPDATE
ENDIF


*!**************************************************************************
*!Function: lfvCompYrs
*! Auther : TMI
*! Date   : 07/14/2012
*! Purpose:*** The years popup valid function. ***
*!**************************************************************************
FUNCTION lfvCompYrs
PARAMETERS loFormSet,loFld

puCompYrs = loFld.Value
IF loFld.Value <> loFld.OldValue
  
  gcDataDir = oAriaApplication.DataDir
  SELECT cFspprdid   ;
       FROM &gcDataDir.FSPRD;
       WHERE cFisfyear = loFld.Value ;
       INTO ARRAY loFld.Parent.Parent.laYerPer;
       ORDER BY cFspprdid ;
       DISTINCT

  loFld.parent.puYerPer.Value = '01'
  loFld.parent.puYerPer.Refresh()

  =lfGetQuery(loFormSet)
  
ENDIF


*!**************************************************************************
*!Function: lfvYerPer
*! Auther : TMI
*! Date   : 07/14/2012
*! Purpose:*** The years periods valid function. ***
*!**************************************************************************
FUNCTION lfvYerPer
PARAMETERS loFormset,loFld

IF loFld.Value <> loFld.OldValue
  =lfGetQuery(loFormSet)  
ENDIF



*!**************************************************************************
*!Function: lfvSM
*! Auther : TMI
*! Date   : 07/14/2012
*! Purpose:*** The source module push button valid function. ***
*!**************************************************************************
FUNCTION lfvSM
PARAMETERS loFormSet

DIMENSION laAppDesc[ALEN(loFormSet.laAppDesc,1)]
DIMENSION laAppl[ALEN(loFormSet.laAppl,1)]
=ACOPY(loFormSet.laAppl,laAppl)
=ACOPY(loFormSet.laAppDesc,laAppDesc)

=gfMover(@laAppDesc,@laAppl,"Source module",.T.)

lcAppIdStr   = ""

PRIVATE laSource,laTarget
DIMENSION laSource[1],laTarget[1]
= ACOPY(laAppDesc,laSource)
FOR lnI = 1 TO ALEN(laAppDesc,1)
  laSource[lnI] = STRTRAN(laSource[lnI],"\","")
ENDFOR

IF EMPTY(laAppl)
  laTarget = ""
ELSE
  = ACOPY(laAppl,laTarget)
ENDIF  

IF !EMPTY(laTarget[1])
  FOR lnCount  = 1 TO ALEN(laTarget,1)
    lcAppIdStr = lcAppIdStr + IIF(EMPTY(lcAppIdStr) , "" , "," );
                 + ALLTRIM(loFormSet.laAppID[ASCAN(laSource,laTarget[lnCount])])
  ENDFOR
ENDIF  

loFormSet.lcAppIdStr = lcAppIdStr 

DIMENSION loFormSet.laAppDesc[ALEN(laAppDesc,1)]
DIMENSION loFormSet.laAppl[ALEN(laAppl,1)]
=ACOPY(laAppl,loFormSet.laAppl)
=ACOPY(laAppDesc,loFormSet.laAppDesc)

=lfGetQuery(loFormSet)

************************************************************
*! Name      : lfvSJ
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : The source journal push button valid function. ***
************************************************************
FUNCTION  lfvSJ
PARAMETERS loFormSet

IF EMPTY(loFormSet.laSJDesc[1])
  =gfModalGen("TRM02087B00000","Dialog")
ELSE
  
  
  DIMENSION laSJDesc[ALEN(loFormSet.laSJDesc,1),ALEN(loFormSet.laSJDesc,2)]
  DIMENSION laSorcJor[ALEN(loFormSet.laSorcJor,1),ALEN(loFormSet.laSorcJor,2)]
  =ACOPY(loFormSet.laSJDesc,laSJDesc)
  =ACOPY(loFormSet.laSorcJor,laSorcJor)
  =gfMover(@laSJDesc,@laSorcJor,"Source journal",.T.)

  lcSJ_IDStr   = ""
  
  PRIVATE laSource,laTarget
  DIMENSION laSource[1],laTarget[1]
  = ACOPY(laSJDesc,laSource)
  FOR lnI = 1 TO ALEN(laSJDesc,1)
    laSource[lnI] = STRTRAN(laSource[lnI],"\","")
  ENDFOR

  IF EMPTY(laSorcJor)
    laTarget = ""
  ELSE
    = ACOPY(laSorcJor,laTarget)
  ENDIF  

  IF !EMPTY(laTarget[1])
    FOR lnCount  = 1 TO ALEN(laTarget,1)
      lcSJ_IDStr = lcSJ_IDStr + IIF(EMPTY(lcSJ_IDStr) , "" , "," );
                   + ALLTRIM(loFormSet.laSorcID[ASCAN(laSource,laTarget[lnCount])])
    ENDFOR
  ENDIF  
  
  *-redimension the array laSorcJor
  DIMENSION loFormSet.laSorcJor[ALEN(laSorcJor,1),MAX(1,ALEN(laSorcJor,2))]

  loFormSet.lcSJ_IDStr = lcSJ_IDStr
  =ACOPY(laSJDesc,loFormSet.laSJDesc)
  =ACOPY(laSorcJor,loFormSet.laSorcJor)
  =lfGetQuery(loFormSet)
  
ENDIF  


*!**************************************************************************
*!Function: lfvDetail
*! Auther : TMI
*! Date   : 07/14/2012
*! Purpose:*** The details push button valid function. ***
*!**************************************************************************
FUNCTION lfvDetail
PARAMETERS loFormSet

lcSaveDtl = ALIAS()

lcBatchno  = CBATCHNO
lcTransNo  = CTRANNO
loFormSet.lcTypCode2 = loFormSet.lcTypCode

lnTotDeb   = 0
lnTotCrd   = 0
lnTotal    = 0

SELECT GLPTRNDT.CACCTCODE ,;
       GLACCHAR.CACCNLDES ,;
       GLPTRNDT.CDRORCR,;
       IIF(CDRORCR='D',STR(NAMOUNT,15,2),STR(0,15,2)) AS 'DEBIT' ,;
       IIF(CDRORCR='C',STR(NAMOUNT,15,2),STR(0,15,2)) AS 'CREDIT' ;
       INTO CURSOR (loFormSet.lcCursor2);
       WHERE GLPTRNDT.CACCTCODE  = GLACCHAR.CACCTCODE;
       .AND. GLPTRNDT.CBATCHNO   = lcBatchno;
       .AND. GLPTRNDT.CTRANNO    = lcTransNo;
       FROM GLPTRNDT,GLACCHAR;
       ORDER BY GLPTRNDT.CDRORCR
       
lcDesLen = ALLTRIM(STR(33-(loFormset.lnAcsSegSz+2)))
    
lcAcSeg  = " " + loFormSet.lcAcSegDes

SUM VAL(DEBIT),VAL(CREDIT) TO lnTotDeb,lnTotCrd

lnTotal = MAX(lnTotDeb,lnTotCrd)

GO TOP IN (loFormSet.lcCursor2)

lcRunScx = lfGetScx("GL\GLQDETL.scx")
DO FORM (lcRunScx) WITH loFormSet

SELECT (lcSaveDtl)
