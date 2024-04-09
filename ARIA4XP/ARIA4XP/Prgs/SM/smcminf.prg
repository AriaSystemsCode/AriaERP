*:************************************************************************
*:  Program File: :\ARIA4XP\PRGS\SM\SMCMINF.Prg
*:  Module      : System Manager 
*:  Desc.       : SM Setups screen
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 12/31/2012
*:  Reference   : *E303339,1 TMI 
*:************************************************************************
*- Get the screen , call it 
#INCLUDE R:\ARIA4XP\SCREENS\SM\SMCMINF.H

lcRunScx = lfGetScx("SM\SMCmInf.scx")
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
*! Name      : lfSetupsFormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : called from the Screen init Method
*!*************************************************************
FUNCTION lfSetupsFormInit
PARAMETERS loFormSet

loFormSet.AddProperty('lcProgName','SMCMINF')

*- Set functions to the APMAIN.FXP
lcPath = oAriaapplication.ApplicationHome
SET PROCEDURE TO (lcPath+'AP\APMAIN.FXP') ADDITIVE 
SET PROCEDURE TO (lcPath+'GL\GL.FXP') ADDITIVE 

DO msg

*- Open tables 
=lfOpenPRGFILES(loFormSet.lcProgName)

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
  .cBrowseIndexExpression = "CCOMP_ID"
  .cBrowseIndexFields     = "CCOMP_ID"
  .cBrowseIndexName       = "CCOMP_ID"
  .cBrowseAliasName       = .lcBaseFile
  .cBrowseTableName       = .lcBaseFile
  .cBrowseFilter          = ""
  .BrowseTitle            = LANG_COMPANY_INFORMATION && Company Information

  .lcFile_Ttl = LANG_COMPANY_INFORMATION
  .ariaBrFields.edtBrowseFields.Value = .GetBrowseFields(.lcBaseFile)
ENDWITH

*- Define several variables needed in the form
=lfDefineVars(loFormSet)

*- Initial mode
loFormSet.ChangeMode('S')  

*- End of lfSetupsFormInit.

************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/01/2013
*! Purpose   : Define the needed variables in the program
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet

loFormSet.AddProperty('laModules[1]','')
*loFormSet.AddProperty('laCompInfo[2]','')
loFormSet.AddProperty('laAllMod[1]','')
loFormSet.AddProperty('laCountry[1,2]','')
loFormSet.AddProperty('laFileStrc[1,5]','')
loFormSet.AddProperty('laFileCDX[4]','')

loFormSet.AddProperty('llRemovDir'  , .F.)
loFormSet.AddProperty('llFlagValid' , .F.)
loFormSet.AddProperty('llNoComp'    , .F.)
loFormSet.AddProperty('llOpenBy'    , .F.)
loFormSet.AddProperty('llSavMod'    , .F.)
loFormSet.AddProperty('llChngCom'   , .F.)
loFormSet.AddProperty('llSavFis'    , .F.)
loFormSet.AddProperty('llSavAcc'    , .F.)
loFormSet.AddProperty('llError'     , .F.)
loFormSet.AddProperty('llView'      , .F.)
loFormSet.AddProperty('llNewSetup'  , .F.)

*loFormSet.AddProperty('lcDataDir' , '')

*loFormSet.AddProperty('lcInsMdl'    , " ")
loFormSet.AddProperty('lcOldMod'    , " ")
loFormSet.AddProperty('lcPrntName'  , " ")
loFormSet.AddProperty('lcOldPrnt'   , " ")
loFormSet.AddProperty('lcOldPrnN'   , " ")
loFormSet.AddProperty('lcOldCmp'    , " ")
loFormSet.AddProperty('lnComRec'    , 0)
loFormSet.AddProperty('lnCurRec'    , 0)

*loFormSet.AddProperty('lcDirStat'   , " ")

*loFormSet.AddProperty('lcOldActCm'  , "")
*loFormSet.AddProperty('lcOldDataD'  , "")
loFormSet.AddProperty('lcCountry'   , "")

loFormSet.AddProperty('lcDiamond'   , "")
loFormSet.AddProperty('lcPrntStat'  , "")
loFormSet.AddProperty('lcPrompt'    , "")

loFormSet.AddProperty('lcTSelect'   , LANG_lcTSelect) && Hold the word "S\<elect"
loFormSet.AddProperty('lcTUnSelct'  , LANG_lcTUnSelct) && Hold the word "\<Unselect"

loFormSet.AddProperty('lcCurrStat'  , "" ) && hold the Currancy display Status
loFormSet.AddProperty('lcOldCurr'  , '' ) && hold the old value of the currency
loFormSet.AddProperty('lcMultCurr'  , "DISABLE")
loFormSet.AddProperty('lcMultCCnt'  , "DISABLE")

loFormSet.AddProperty('lcSMCmSt'    , "" ) && hold the Setup window name
*loFormSet.AddProperty('lnProgCopy'      , gnProgCopy  )&&	 No of Program copies running at the moment)
*loFormSet.AddProperty('lcCmStTitl'  , "Company Setups "+IIF(lnProgCopy=1,'',' /'+ALLTRIM(STR(lnProgCopy))) ) && hold the setup window title)
loFormSet.AddProperty('lcTempSetup' , gfTempName())

*loFormSet.AddProperty(',lcCurrStat' = 'DISABLE')

lcScFields = 'CCOMP_ID,CCOM_NAME,caddress1,caddress2,caddress3,caddress4,caddress5,caddress6,CCOM_PHON,'+;
             'CCOM_FAX,CCOM_DDIR,CCURR_YER,CCURR_PRD,CCOMPPRNT,MCOMP_MDL,CCONT_CODE,CCURRCODE' 

loFormSet.AddProperty('lcScFields',lcScFields)
lcLn = ALLTRIM(STR(OCCURS(',',lcScFields)+1))
loFormSet.AddProperty('laData[&lcLn.]') 
SELECT (loFormSet.lcBaseFile)
SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData BLANK

SELECT SYCINT
SELECT DISTINCT sycint.ccont_desc,sycint.ccont_code ;
       FROM (oAriaApplication.SysPath+"SYCINT") ;
       INTO ARRAY loFormSet.laCountry
IF _TALLY = 0
  loFormSet.laCountry = " "
ENDIF
  
SELECT sycComp
loFormSet.lcPrompt    = loFormSet.lcTSelect

*- module list
WITH loFormSet
  .AddProperty('laMod[16]')
  .laMod[1 ] = 'SM-Company Setups              '
  .laMod[2 ] = 'IC-Inventory Control Setup     '
  .laMod[3 ] = 'AL-Sales Order Allocation Setup'
  .laMod[4 ] = 'AR-Accounts Receivable Setup   '
  .laMod[5 ] = 'CR-CRP Module Setup            '
  .laMod[6 ] = 'EB-EDI Base Setup              '
  .laMod[7 ] = 'MA-Material Setup              '
  .laMod[8 ] = 'MF-Manufacturing Setup         '
  .laMod[9 ] = 'NC-NC module Setup             '
  .laMod[10] = 'PO-Style Purchase Order Setup  '
  .laMod[11] = 'PS-Point of Sale Setup         '
  .laMod[12] = 'PW-Piece Work Setup            '
  .laMod[13] = 'SO-Sales Order Setup           '
  .laMod[14] = 'SP-SP module Setup             '
  .laMod[15] = 'SR-Sales Representative Setup  '
  .laMod[16] = 'UP-Universal Product Code Setup'
ENDWITH

loFormSet.AddProperty('llFirstComp')

** - create a temp alias for the sydreprt and syrepuvr
* use these two variables to save a copy of the lines of SYDREPT and SYREPUVR
*E303339,1 TMI 01/06/2013 [Start] 
*loFormSet.AddProperty('SYDREPRT',gfTempName())  
*SELECT * FROM (oAriaapplication.caria4syspath+'SYDREPRT') ;
    WHERE crep_id = 'SMSETUPS' ;
    INTO TABLE (oAriaapplication.WorkDir+loFormSet.SYDREPRT)
*IF USED('SYDREPRT')
*  USE IN SYDREPRT
*ENDIF 
*USE IN (loFormSet.SYDREPRT)
*USE (oAriaapplication.WorkDir+loFormSet.SYDREPRT) IN 0 ALIAS SYDREPRT
*- open the SYDREPRT and SYREPUVR tables

*- save the a4 sysfiles path to change it later
loFormSet.AddProperty('cOriginal_cAria4Sysfiles',oAriaApplication.cAria4Sysfiles)

lcSelectStatement = 'SELECT * FROM SYDREPRT WHERE CREP_ID = "SMAPP "'
lnRemResult = oAriaApplication.RemoteSystemData.Execute(lcSelectStatement ,'',"SYDREPRT","",oAriaApplication.cAria4SysFiles,3,"",SET("Datasession"))

*!*	lcSelectStatement = 'SELECT * FROM SYREPUVR WHERE CREP_ID = "SMAPP" Order By cExpType, nVarPos'
*!*	lnRemResult = oAriaApplication.RemoteSystemData.Execute(lcSelectStatement ,'',"SYREPUVR","",oAriaApplication.cAria4SysFiles,3,"",SET("Datasession"))

*lnCurrDataSession = SET("DATASESSION")
*-Create a new connection string the looks at a different path other than the SQLDICTIONARY, in that path get the syrepuvr with the lines updated
loFormSet.AddProperty( 'lcTempFldr' ,  oAriaApplication.WorkDir+gfTempName()+'\' )

MD (loFormSet.lcTempFldr)
SELECT SYDREPRT 
COPY TO (loFormSet.lcTempFldr+'SYDREPRT')
USE IN SYDREPRT 

USE (oariaapplication.caria4syspath+'SYREPUVR')
SELECT SYREPUVR
COPY TO (loFormSet.lcTempFldr+'SYREPUVR') FOR CREP_ID = 'SMAPP' WITH CDX FOX2X
USE IN SYREPUVR 

*E303339,1 TMI 01/06/2013 [End  ] 

*- this table will be populated 
*loFormSet.AddProperty('SYREPUVR',gfTempName())

*puCountry = LOOKUP(SYCINT.CPARt1LAB,IIF(EMPTY(loFormset.laData[16]),oariaapplication.defaultcountry,loFormSet.laData[16]),sycint.ccont_code,'CCONTCODE')

*lcDirStat   = IIF(loFormSet.ActiveMode = 'A',"ENABLE","DISABLE")
*lcPrntStat  = IIF(loFormSet.ActiveMode = 'A',"ENABLE","DISABLE")

*- End of lfDefineVars.

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
  .laData2.ControlSource = 'Thisformset.laData[2]'

  .laData2.MaxLength = FSIZE('CCOM_NAME','SYCCOMP')

  .laData14.ControlSource = 'Thisformset.laData[14]'
  .lcPrntName.ControlSource = 'Thisformset.lcPrntName'
  
  .Address1.txtAdd1.ControlSource = 'Thisformset.laData[3]'
  .Address1.txtAdd2.ControlSource = 'Thisformset.laData[4]'
  .Address1.txtAdd3.ControlSource = 'Thisformset.laData[5]'
  .Address1.txtAdd4.ControlSource = 'Thisformset.laData[6]'
  .Address1.txtAdd5.ControlSource = 'Thisformset.laData[7]'
  .Address1.txtAdd6.ControlSource = 'Thisformset.laData[8]'
  
  .laData9.ControlSource = 'Thisformset.laData[9]'
  .laData10.ControlSource = 'Thisformset.laData[10]'
  
  .laData9.InputMask = gfPhoneTem()
  .laData10.InputMask = gfPhoneTem()
  
  .laData17.Keytextbox.ControlSource = 'Thisformset.laData[17]'
  
  .laData12.ControlSource = 'Thisformset.laData[12]'
  .laData13.ControlSource = 'Thisformset.laData[13]'

  .laData11.ControlSource = 'Thisformset.laData[11]'  
ENDWITH
*- End of lfDefControlSource.

*:************************************************************************ original code



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

*- Define Control Sources 
=lfDefControlSource(loFormSet)

lcScFields = loFormSet.lcScFields
SELECT (loFormSet.lcBaseFile)
DO CASE 
case loFormSet.ActiveMode $ 'VE'
  SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData 
case loFormSet.ActiveMode $ 'S'
  SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData BLANK  
ENDCASE   

WITH loFormset.Ariaform1
  .laData1.Enabled = loFormSet.ActiveMode $ 'S'
  .laData2.Enabled = loFormSet.ActiveMode $ 'AE'
  .laData17.Enabled = loFormSet.ActiveMode = 'A'
ENDWITH 

DO CASE
  CASE loFormSet.ActiveMode = 'S'
    
    SELECT (loFormSet.lcBaseFile)
    SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData BLANK

    loFormset.Ariaform1.laData1.Enabled = .T.
    
    loFormSet.llView = .F.
    *puCountry = LOOKUP(SYCINT.CPARt1LAB,oariaapplication.defaultcountry,sycint.ccont_code,'CCONTCODE')     
    loFormSet.lcPrntName = " "
    DECLARE loFormSet.laModules[1],loFormSet.laAllMod[1]
    STORE " " TO loFormSet.laModules,loFormSet.laAllMod
    STORE 'DISABLE' TO loFormSet.lcMultCCnt,loFormSet.lcMultCurr
    *=lfShowCurr()
    loFormSet.lcOldCmp=loFormSet.laData[11]   && Assign the company directory to variable
*Old :     SHOW GET pbInsSet   DISABLE
*Old :     SHOW GET pbSetup   DISABLE
*Old :     SHOW GET laData[11] DISABLE
*Old :     SHOW GET pbDatDir   DISABLE
*Old :     SHOW GET laData[14] DISABLE
*Old :     lcCurrStat = 'DISABLE'
*Old :     SHOW GET laData[17] DISABLE
*Old :     SHOW GET ibAddrType DISABLE
*Old :     SHOW GET ibCurrKey DISABLE
    IF USED(loFormSet.lcTempSetup)
      SELECT (loFormSet.lcTempSetup)
      ZAP
    ENDIF
    loFormSet.Ariaform1.laData1.KeyTextbox.SetFocus()
    SELECT (loFormSet.lcBaseFile)
    
  CASE loFormSet.ActiveMode = 'V'

    loFormSet.lcCurrStat = 'DISABLE'
    loFormSet.llView     = .F.
    loFormSet.lcOldCmp   = loFormSet.laData[11]   && Assign the company directory to variable
    loFormSet.lcOldMod   = loFormSet.laData[15]
    loFormSet.lnComRec   = RECNO()
    loFormSet.lcPrntName = IIF(EMPTY(loFormSet.laData[14]),'',;
                     LOOKUP(sycComp.cCom_name ,ALLTRIM(loFormSet.laData[14]),;
                            sycComp.cComp_id,'cComp_id'))
    IF loFormSet.lnComRec > 0 .AND. loFormSet.lnComRec <=RECCOUNT()
      GO loFormSet.lnComRec
    ENDIF
    SHOW GET laData[11] DISABLE
    SHOW GET pbDatDir   DISABLE
    SHOW GET laData[14] DISABLE
    IF EMPTY(loFormSet.laData[14])
      ** If this company is a parent compnay. **

      SELECT "  "+cApp_Id+" "+cApp_name+" +"+IIF(lSetReq,"T","F") ;
             FROM (oAriaApplication.SysPath+"SYDAPPL") ;
             WHERE cApp_Id <> "SM" .AND. cApp_Id <> "SY" ;
             INTO ARRAY loFormSet.laAllMod
    ELSE
      ** If this company is a child company. **
      SELECT "  "+cApp_Id+" "+cApp_name+" +"+IIF(lSetReq,"T","F") ;
             FROM (oAriaApplication.SysPath+"SYDAPPL") ;
             WHERE cApp_Id <> "SM" .AND. cApp_Id <> "SY" .AND. !lparntmdl ;
             INTO ARRAY loFormSet.laAllMod
    ENDIF
    

*!*	    FOR lnCount1 = 1 TO ALEN(loFormSet.laCountry,1)
*!*	      IF ALLTRIM(loFormSet.laData[16]) = ALLTRIM(loFormSet.laCountry[lnCount1,2])
*!*	        puCountry = LOOKUP(SYCINT.CPARt1LAB,IIF(EMPTY(loFormSet.laData[16]),oariaapplication.defaultcountry,loFormSet.laData[16]),sycint.ccont_code,'CCONTCODE')     
*!*	        EXIT
*!*	      ENDIF
*!*	    ENDFOR
    STORE 'DISABLE' TO loFormSet.lcMultCCnt,loFormSet.lcMultCurr
    *=lfShowCurr()
    SELECT (loFormSet.lcBaseFile) 
    IF GfGetMemVar('LLHIST',loFormSet.laData[1])
      *SHOW GET pbEdt   DISABLE
      loFormSet.oToolbar.cmdEdit.Enabled = .F.
    ENDIF
    loFormset.Ariaform1.pbSetup.Enabled = .T.
    loFormset.Ariaform1.pbInsSet.Enabled = .T.

    SELECT (loFormSet.lcBaseFile)
    SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData    
    
    *E303339,1 TMI 01/10/2013 [Start] 
    =SEEK(loFormSet.laData[17],'SYCCURR')
    loFormset.Ariaform1.txtCurrency.Value = SYCCURR.CCURRDESC
    *E303339,1 TMI 01/10/2013 [End  ] 
  CASE loFormSet.ActiveMode = 'E'
  
    SELECT SYCCOMP
    CURSORSETPROP("Buffering",5)
    
    loFormSet.lcCurrStat = IIF(EMPTY(loFormSet.laData[17]),'ENABLE','DISABLE')
    loFormSet.lnComRec   = RECNO()
    
    loFormSet.lcPrntName = IIF(EMPTY(loFormSet.laData[14]),'',;
                     LOOKUP(sycComp.cCom_name ,ALLTRIM(loFormSet.laData[14]),;
                            sycComp.cComp_id,'cComp_id'))
    lcDataDir  = gfGetDataDir(ALLTRIM(IIF(EMPTY(loFormSet.laData[14]), loFormSet.laData[11], SYCCOMP.cCom_DDir)))
    
    =lfOpnFiles('ACCOD','ACCSEGNO')
    IF loFormSet.lnComRec > 0 .AND. loFormSet.lnComRec <=RECCOUNT()
      GO loFormSet.lnComRec
    ENDIF
    SHOW GET laData[11] DISABLE
    SHOW GET pbDatDir   DISABLE
    SHOW GET laData[14] DISABLE
    
    IF EMPTY(loFormSet.laData[14])
      ** If this company is a parent compnay. **
      SELECT "  "+cApp_Id+" "+cApp_name+" +"+IIF(lSetReq,"T","F") ;
             FROM (oAriaApplication.SysPath+"SYDAPPL") ;
             WHERE cApp_Id <> "SM" .AND. cApp_Id <> "SY" ;
             INTO ARRAY loFormSet.laAllMod
    ELSE
      ** If this company is a child company. **

      SELECT "  "+cApp_Id+" "+cApp_name+" +"+IIF(lSetReq,"T","F") ;
             FROM (oAriaApplication.SysPath+"SYDAPPL") ;
             WHERE cApp_Id <> "SM" .AND. cApp_Id <> "SY" .AND. !lparntmdl ;
             INTO ARRAY loFormSet.laAllMod
    ENDIF
    SHOW GET ibAddrType ENABLE
    *=lfShowCurr()
  CASE loFormSet.ActiveMode = 'A'
    
    loFormSet.llNewSetup = .T.
    *puCountry = LOOKUP(SYCINT.CPARt1LAB,oariaapplication.defaultcountry,sycint.ccont_code,'CCONTCODE')     
    loFormSet.laData[8]  = LOOKUP(SYCINT.cCont_Desc,oariaapplication.defaultcountry,sycint.ccont_code,'CCONTCODE')
    loFormSet.laData[16] = oariaapplication.defaultcountry
    loFormSet.laData[17] = LOOKUP(SYCINT.cCurrCode,oariaapplication.defaultcountry,sycint.ccont_code,'CCONTCODE')

    *E303339,1 TMI 01/10/2013 [Start] 
    =SEEK(loFormSet.laData[17],'SYCCURR')
    loFormset.Ariaform1.txtCurrency.Value = SYCCURR.CCURRDESC
    *E303339,1 TMI 01/10/2013 [End  ] 
    
    loFormSet.lcPrntName = " "
    ** Build the string of the company directory **
    gcAllCmp = ADDBS(ALLTRIM(sycinst.CINSALLCMP))
    loFormSet.laData[11]=gcAllCmp+UPPER(ALLTRIM(loFormSet.laData[1]))+'\'
*Old :     SHOW GET loFormSet.laData[8]
*Old :     SHOW GET loFormSet.laData[11] ENABLE
*Old :     SHOW GET pbDatDir   ENABLE
*Old :     SHOW GET pbInsSet   DISABLE
*Old :     SHOW GET pbSetup   DISABLE    
    
    GO TOP IN SYCCOMP
    IF EOF('SYCCOMP')
      *SHOW GET loFormSet.laData[14] DISABLE
      loFormSet.Ariaform1.laData14.Enabled = .F.
    ELSE
      *SHOW GET loFormSet.laData[14] ENABLE
      loFormSet.Ariaform1.laData14.Enabled = .T.
    ENDIF  
    loFormSet.lcCurrStat = 'ENABLE'
    *SHOW GET loFormSet.laData[17] ENABLE
    loFormSet.Ariaform1.laData17.Enabled = .T.
    *SHOW GET ibAddrType ENABLE
    *SHOW GET ibCurrKey ENABLE        
    STORE 'ENABLE' TO loFormSet.lcMultCCnt
    loFormset.Ariaform1.pbSetup.Enabled = .F.
    loFormset.Ariaform1.pbInsSet.Enabled = .F.    

    *lcMultCurr=IIF(loFormSet.laData[18],'ENABLE','DISABLE')
    *=lfShowCurr()    
ENDCASE
=lfSortMod(loFormSet)

loFormSet.Ariaform1.Refresh()

SELECT (loFormSet.lcBaseFile)     
*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
***Valid function of the company ID...
FUNCTION lfvData_1
PARAMETERS loFormSet,loFld
LOCAL lnResp

IF loFld.Selectedfrombrowse 

  =loFormSet.oToolBar.cmdFind.Click()
  
ELSE

  IF !EMPTY(loFormSet.laData[1]) AND !SEEK(loFld.KeyTextbox.Value,'SYCCOMP')
    lnResp = gfModalGen('INM00001B02004','DIALOG',LANG_COMPANY_ID+loFormSet.laData[1])
    DO Case
    CASE lnResp = 1
      IF !loFormSet.oToolBar.cmdFind.Click()
        loFormSet.laData[1] = ''
        loFld.KeyTextbox.Value = ''
      ENDIF 
    CASE lnResp = 2  
      loFormSet.ChangeMode('A')
    CASE lnResp = 3
      loFormSet.laData[1] = ' '
    ENDCASE 
  ELSE
    loFormSet.ChangeMode('V')  
  ENDIF 
  
ENDIF   
*- End of lfvData_1.

*!**************************************************************************
*!
*!      Function: lfvDatDir
*!
*!**************************************************************************
** Valid function for the push button < Data Dictionary >...
*
FUNCTION lfvDatDir


*!*	*!**************************************************************************
*!*	*!
*!*	*!      Function: lfwData_11
*!*	*!
*!*	*!**************************************************************************
*!*	** When function for laData[11] field {company path}
*!*	*
*!*	FUNCTION lfwData_11
*!*	loFormSet.lcOldCmp = UPPER(ALLTRIM(loFormSet.laData[11]))

*!*	*!**************************************************************************
*!*	*!
*!*	*!      Function: lfvData_11
*!*	*!
*!*	*!**************************************************************************
*!*	** Valid function for laData[11] field {company path}
*!*	*
*!*	FUNCTION lfvData_11
*!*	PARAMETERS loFormSet,loFld


*!*	IF !EMPTY(loFormSet.laData[11]) 
*!*	  ** Check for the directory if used by onther company ** 
*!*	  IF UPPER(ALLTRIM(loFormSet.lcOldCmp)) <> UPPER(ALLTRIM(loFormSet.laData[11]))
*!*	    lnRecCmp= RECNO("SYCCOMP")
*!*	    GO TOP
*!*	    LOCATE FOR cComp_ID<>loFormSet.laData[1] .AND. ;
*!*	               UPPER(ALLTRIM(cCom_Ddir)) == UPPER(ALLTRIM(loFormSet.laData[11]))
*!*	    IF lnRecCmp > 0 .AND. lnRecCmp <= RECCOUNT("SYCCOMP")
*!*	      GOTO lnRecCmp  
*!*	    ENDIF  

*!*	    IF FOUND()
*!*	      ** Path ð is in use by another company. **
*!*	      ** You cannot use this path...
*!*	      ** <  Ok  > **
*!*	      =gfModalGen("QRM00032B00000","DIALOG",+" "+loFormSet.laData[11])
*!*	      loFormSet.laData[11] = loFormSet.lcOldCmp
*!*	      SHOW GET loFormSet.laData[11]  
*!*	      _CUROBJ=OBJNUM(loFormSet.laData[11])
*!*	      RETURN
*!*	    ENDIF  
*!*	  ENDIF

*!*	  ** The directory is created or used **
*!*	  IF gfValdPath(ALLTRIM(loFormSet.laData[11]))
*!*	    IF UPPER(ALLTRIM(loFormSet.laData[11])) <> UPPER(ALLTRIM(loFormSet.lcOldCmp))
*!*	      loFormSet.llFlagValid = .T. 
*!*	      ** Dir ð already exists. Use it anyway? **
*!*	      ** < Yes > - <  No  > **
*!*	      IF gfModalGen("QRM00033B00006","ALERT",+" "+loFormSet.laData[11]) =  2 
*!*	        loFormSet.laData[11] = loFormSet.lcOldCmp
*!*	        SHOW GET loFormSet.laData[11]
*!*	        _CUROBJ=OBJNUM(loFormSet.laData[11])
*!*	        RETURN
*!*	      ENDIF  
*!*	    ENDIF
*!*	  ELSE
*!*	    RETURN .F.
*!*	  ENDIF
*!*	ENDIF

*!*	*!**************************************************************************
*!*	*!
*!*	*!      Function: lfwData_14
*!*	*!
*!*	*!**************************************************************************
*!*	** When function for the laData[14] {parent company}...
*!*	*
*!*	FUNCTION lfwData_14

*!*	lcOldPrnt = laData[14]
*!*	lcOldPrnN = lcPrntName

*!**************************************************************************
*!
*!      Function: lfvData_14
*!
*!**************************************************************************
** Valid function for the laData[14] {parent company}...
*
FUNCTION lfvData_14
PARAMETERS loFormSet,loFld
loFormSet.Ariaform1.Refresh()

*E303339,1 TMI 01/10/2013 [Start] 
*IF loFormSet.laData[14] = loFormSet.lcOldPrnt
IF loFld.Value = loFld.OldValue
  *E303339,1 TMI 01/10/2013 [End  ] 
  RETURN
ENDIF  
*E303339,1 TMI 01/10/2013 [Start] 
*IF EMPTY(loFormSet.laData[14]) 
IF EMPTY(loFld.Value)  
  *E303339,1 TMI 01/10/2013 [End  ] 
  loFormSet.lcPrntName = ""
  loFormSet.Ariaform1.lcPrntName.Value = ''
*  =lfRefresh()
  RETURN
ENDIF

IF loFormSet.ActiveMode = 'E'
  loFormSet.lnComRec = RECNO()
ENDIF

IF .T.
  IF SEEK(loFormSet.laData[14],"SYCCOMP")
    IF !EMPTY(sycComp.cCompPrnt)
      ** Company {loFormSet.laData[14]} is already a child company. **
      ** You cannot choose a child company as a parent company...
      ** <  Ok  > **
      =gfModalGen("TRM00165B00000","DIALOG",;
                  "( "+loFormSet.laData[14]+"-"+ALLTRIM(sycComp.cCom_name)+" )")
      loFormSet.laData[14] = loFormSet.lcOldPrnt
      SHOW GET loFormSet.laData[14]
      IF loFormSet.ActiveMode = 'E' .AND. loFormSet.lnComRec > 0 .AND. loFormSet.lnComRec <= RECCOUNT()
        GO loFormSet.lnComRec
      ENDIF
      RETURN
    ENDIF
    
    IF loFormSet.laData[14] = loFormSet.laData[1]
      ** You cannot choose the current company **
      ** to be a parent to itself...
      ** <  Ok  > **
      =gfModalGen("TRM00166B00000","DIALOG")
      loFormSet.laData[14] = loFormSet.lcOldPrnt
      IF loFormSet.ActiveMode = 'E' .AND. loFormSet.lnComRec > 0 .AND. loFormSet.lnComRec <= RECCOUNT()
        GO loFormSet.lnComRec
      ENDIF
      RETURN
    ENDIF
    
    lcDataDir  = gfGetDataDir(ALLTRIM(SYCCOMP.cCom_DDir))
    
    =lfOpnFiles('ACCOD','ACCSEGNO')
    IF IIF(AT('?',loFormSet.laData[14]) > 0 ,!SEEK('','ACCOD'),.F.)    
      ** This company does not have an account **
      ** code structure.  You cannot choose it **
      ** as a parent company...
      ** <  Ok  > **
      =gfModalGen("TRM00198B00000","DIALOG")
      loFormSet.laData[14] = loFormSet.lcOldPrnt
      SHOW GET loFormSet.laData[14]
      SELECT SYCCOMP
      RETURN
    ENDIF
    loFormSet.lcPrntName = sycComp.cCom_name
  ELSE
    SELECT sycComp
    SET FILTER TO EMPTY(sycComp.cCompPrnt) .AND. ;
        sycComp.cComp_Id <> loFormSet.laData[1]

    IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF

    lcBrFields    = "cComp_id :H='"+LANG_COMPANY_ID+"',cCom_name :H='"+LANG_Company_Name+"'"
    lcFile_ttl    = LANG_Company_information
    DIMENSION laCompInfo[2]
    laCompInfo[1] = loFormSet.laData[14]
    laCompInfo[2] = loFormSet.lcPrntName

    IF gfBrows(.F.,"cComp_id,cCom_name","laCompInfo")    
      IF loFormSet.laData[14] = laCompInfo[1] 
        loFormSet.laData[14]  = loFormSet.lcOldPrnt
      ELSE
        loFormSet.laData[14] = laCompInfo[1]
        loFormSet.lcPrntName = laCompInfo[2]      
      ENDIF
      =SEEK(loFormSet.laData[14], 'SYCCOMP')
      lcDataDir  = gfGetDataDir(ALLTRIM(SYCCOMP.cCom_DDir))
      
      =lfOpnFiles('ACCOD','ACCSEGNO')
      IF !SEEK("",'ACCOD')
        ** This company does not have an account **
        ** code structure.  You cannot choose it **
        ** as a parent company...
        ** <  Ok  > **
        =gfModalGen("TRM00198B00000","DIALOG")
        loFormSet.laData[14] = loFormSet.lcOldPrnt
        loFormSet.lcPrntName = loFormSet.lcOldPrnN
        SELECT SYCCOMP
      ENDIF
    ELSE
      loFormSet.laData[14] = loFormSet.lcOldPrnt
    ENDIF
  ENDIF
ELSE
  loFormSet.laData[14] = loFormSet.lcOldPrnt
ENDIF

SELECT SYCCOMP
SET FILTER TO
IF loFormSet.ActiveMode = 'E' .AND. loFormSet.lnComRec > 0 .AND. loFormSet.lnComRec <= RECCOUNT()
  GO loFormSet.lnComRec
ENDIF

SHOW GET loFormSet.laData[14]

**=lfRefresh()

IF loFormSet.lcOldPrnt <> loFormSet.laData[14]

  IF EMPTY(loFormSet.laData[14])
    ** If this company is a parent compnay. **
    
    SELECT "  "+cApp_Id+" "+cApp_name+" +"+IIF(lSetReq,"T","F") ;
           FROM (oAriaApplication.SysPath+"SYDAPPL") ;
           WHERE cApp_Id <> "SM" .AND. ;
                 cApp_Id <> "SY" ;
           INTO ARRAY loFormSet.laAllMod

  ELSE
    ** If this company is a child company. **
    SELECT "  "+cApp_Id+" "+cApp_name+" +"+IIF(lSetReq,"T","F") ;
           FROM (oAriaApplication.SysPath+"SYDAPPL") ;
           WHERE cApp_Id <> "SM" .AND. ;
                 cApp_Id <> "SY" .AND. ;
                 !lparntmdl ;
           INTO ARRAY loFormSet.laAllMod
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_16
*!
*!**************************************************************************
*
FUNCTION lfvData_16

    loFormSet.laData[16] = loFormSet.laCountry[ibCountry,2]
    SHOW GET ibComp

IF EMPTY(loFormSet.laData[8])
  loFormSet.laData[8] = lcCountry
  SHOW GET loFormSet.laData[8]
ENDIF

************************************************************
*! Name      : lfSetupsFormBeforeSave
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/31/2012
*! Purpose   : before save method
************************************************************
FUNCTION lfSetupsFormBeforeSave
PARAMETERS loFormSet
** Check if the company name is empty or not...
IF EMPTY(loFormSet.laData[2])
  ** You cannot save this company without name. **
  =gfModalGen("TRM00171B00000","DIALOG")
  llCSave = .F.
  *_CUROBJ = OBJNUM(loFormSet.laData[2])
  loFormSet.Ariaform1.laData2.Setfocus()
  RETURN .F.
ENDIF

lcDbfPath = IIF(RIGHT(ALLTRIM(loFormSet.laData[11]),1)='\',;
                SUBSTR(ALLTRIM(loFormSet.laData[11]) , 1 , LEN(ALLTRIM(loFormSet.laData[11])) - 3),;
                SUBSTR(ALLTRIM(loFormSet.laData[11]) , 1 , LEN(ALLTRIM(loFormSet.laData[11])) - 2))
IF !gfValdPath(lcDbfPath)
  lcDbfPath = IIF(RIGHT(ALLTRIM(lcDbfPath),1)='\',;
                SUBSTR(ALLTRIM(lcDbfPath),1,LEN(ALLTRIM(lcDbfPath))-1),;
                ALLTRIM(lcDbfPath))
  *=gfMkDir(lcDbfPath)
  TRY
    MD (lcDbfPath)
  CATCH
    =gfModalGen("TRM00049B00000","DIALOG")
    RETURN .F. 
  ENDTRY 
ENDIF
    
  ** If the path is not found, create it. **
IF !gfValdPath(loFormSet.laData[11])
  lcNewPath =IIF(RIGHT(ALLTRIM(loFormSet.laData[11]),1)='\',;
                   SUBSTR(ALLTRIM(loFormSet.laData[11]),1,;
                   LEN(ALLTRIM(loFormSet.laData[11]))-1),;
                   ALLTRIM(loFormSet.laData[11]))
  *=gfMkDir(lcNewPath)
  TRY
    MD (lcNewPath)
  CATCH
    =gfModalGen("TRM00049B00000","DIALOG")
    RETURN .F. 
  ENDTRY 
ENDIF



*--------------------------
** Get the path...
loFormSet.laData[11] = IIF(RIGHT(ALLTRIM(loFormSet.laData[11]),1)<>'\',;
                  UPPER(ALLTRIM(loFormSet.laData[11]))+'\',;
                  UPPER(ALLTRIM(loFormSet.laData[11])))

** See if this company or not in the company file **
** for defining the menu popup comany bar...
SELECT syccomp
loFormSet.lnComRec = IIF(RECNO()>RECCOUNT(),0,RECNO())
GO TOP
IF EOF()
  loFormSet.llFirstComp = .T.
ELSE
  loFormSet.llFirstComp = .F.
ENDIF


IF loFormSet.lnComRec > 0
  GOTO loFormSet.lnComRec
ENDIF  

*E303339,1 TMI 01/13/2013 [Start] 
LOCAL lcPath
lcPath = ALLTRIM(SUBSTR(loFormSet.laData[11],1,RAT('\',loFormSet.laData[11],3)))+'SYSFILES'
IF ! lcPath $ DBF('SYCCOMP')
  USE IN SYCCOMP
  lfBuff('SYCCOMP')
ENDIF 
*E303339,1 TMI 01/13/2013 [End  ] 

*E303339,1 TMI 01/13/2013 [Start] get the sql connection data for the active company
SET ORDER TO CCOMP_ID IN SYCCOMP
=SEEK(oAriaApplication.ActiveCompanyID,'SYCCOMP','CCOMP_ID')
DIMENSION laConnInfo[5]
SELECT syccomp 
SCATTER FIELDS CCONDRIVER,CCONSERVER,CCONDBNAME,CCONUSERID,CCONPASWRD TO laConnInfo
laConnInfo[3] = loFormSet.laData[1]
*E303339,1 TMI 01/13/2013 [End  ] 

** Save this company record in the company file. **
SELECT sycComp
IF loFormSet.ActiveMode = 'E'
 SEEK loFormSet.laData[1]
ENDIF
IF loFormSet.ActiveMode = 'A'
  APPEND BLANK
ENDIF
*E303339,1 TMI 01/08/2013 [Start] 
lcScFields = loFormSet.lcScFields
*E303339,1 TMI 01/08/2013 [End  ] 
GATHER FROM loFormSet.laData FIELDS &lcScFields MEMO

IF loFormSet.ActiveMode = 'A'
  SELECT SYCCOMP
  GATHER FIELDS CCONDRIVER,CCONSERVER,CCONDBNAME,CCONUSERID,CCONPASWRD FROM laConnInfo

  SELECT syccomp
  REPLACE lrunfroma4 WITH .T.
  =gfTableUpdate()
  =gfAdd_Info('sycComp')

  *E303339,1 TMI 01/13/2013 [Start] set the company id to the current company to activate the check of the connection screen
  LOCAL lcCompanyID , lcCompanyName
  lcCompanyID = oAriaApplication.ActiveCompanyId
  lcCompanyName = oAriaApplication.ActiveCompanyName

  oAriaApplication.ActiveCompanyId = SYCCOMP.CCOMP_ID
  oAriaApplication.ActiveCompanyName = ALLTRIM(SYCCOMP.CCOM_NAME)
  *E303339,1 TMI 01/13/2013 [End  ] 
  
  DO msg
  x= oAriaApplication.activecompanyconstr
  
  IF !oAriaApplication.RemoteCompanyData.mcreatedatabase(loFormSet.laData[1])
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_CONVERT_DATABASE)
    oAriaApplication.ActiveCompanyId = lcCompanyID 
    oAriaApplication.ActiveCompanyName = lcCompanyName 
  
    SELECT syccomp
    SEEK loFormSet.laData[1]
    DELETE 
    =gfTableUpdate()
    
    RETURN .F.
  ENDIF  
  oAriaApplication.ActiveCompanyId = lcCompanyID 
  oAriaApplication.ActiveCompanyName = lcCompanyName 
ENDIF 
*E303339,1 TMI 01/10/2013 [Start] 

*E303339,1 TMI 01/13/2013 [Start] 
*- Create the sql tables
IF loFormSet.ActiveMode = 'A'
  LOCAL lcSQLDICPATH
  lcSQLDICPATH = oAriaApplication.ClientA4Path+'SQLDictionary\'  && In all cases you are multi-inst
  *!*	LOCAL lnHndl
  *!*	lnHndl = FCREATE(lcSQLDICPATH+'ARIA4XP.txt')
  *!*	IF lnHndl < 0
  *!*	  =gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_CONVERT_DICTNARY)
  *!*	  RETURN .F.
  *!*	ENDIF   
  
  *E303339,1 TMI 01/13/2013 [Start] 
  lcSQLDICPATH = oAriaApplication.ClientA4Path+'SQLDictionary\'  && In all cases you are multi-inst

  lcSQLDICPATH = ADDBS(lcSQLDICPATH)
  IF USED('SYDFILES')
    USE IN SYDFILES
  ENDIF
  USE lcSQLDICPATH +'SYDFILES' SHARED IN 0 ORDER 1
  USE lcSQLDICPATH +'SYDFILES' SHARED IN 0 ALIAS 'UPDATEFLS' AGAIN ORDER 1
  IF USED('SYDFLFLD')
    USE IN 'SYDFLFLD'
  ENDIF
  USE lcSQLDICPATH +'SYDFLFLD' SHARED IN 0 ORDER CFILE_NAM
  IF USED('SYDFIELD')
    USE IN 'SYDFIELD'
  ENDIF
  USE lcSQLDICPATH +'SYDFIELD' SHARED IN 0 ORDER CFLD_NAME
  IF USED('SYDINDEX')
    USE IN 'SYDINDEX'
  ENDIF
  USE lcSQLDICPATH +'SYDINDEX' SHARED IN 0 ORDER CFILE
  =updatetablestructure (oAriaApplication.SysPath,.F.,'SYDFILES','SYDFLFLD' ,'SYDFIELD' ,'SYDINDEX','UPDATEFLS')
  USE IN 'SYDFILES'
  USE IN 'SYDFLFLD'
  USE IN 'SYDFIELD'
  USE IN 'SYDINDEX'
  USE IN 'UPDATEFLS'
ENDIF   
*E303339,1 TMI 01/13/2013 [End  ] 
  

*- End of lfSetupsFormBeforeSave.


************************************************************
*! Name      : updatetablestructure
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/13/2013
*! Purpose   : Update the data tables in the created sql data base while installing a company
************************************************************
FUNCTION updatetablestructure
 LPARAMETERS lca27sysfiles, lbindexonly, sydfiles, sydflfld, sydfield, sydindex, updtables
 USE SHARED ADDBS(lca27sysfiles)+'syccomp.dbf' ALIAS 'syccomp_A' IN 0 AGAIN
 SELECT 'syccomp_A' 
 LOCATE
 objremotedataaccess = CREATEOBJECT("remotedataaccess")
 ndatasessionid = SET("Datasession")
 
 SCAN FOR lrunfroma4=.T. AND ccomp_id = loFormSet.laData[1]
    lccompid = ccomp_id
    SELECT 'syccomp_A' 
    *E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][Start]
    IF EMPTY(ALLTRIM(CCONSERVER)) OR EMPTY(ALLTRIM(CCONDBNAME))
      LOOP
    ENDIF
    *E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][End]
    lcConnStr = "Driver={SQL Server};server="+ALLTRIM(CCONSERVER)+";DATABASE="+ALLTRIM(CCONDBNAME)+;
                ";uid="+ALLTRIM(CCONUSERID)+";pwd="+ALLTRIM(CCONPASWRD)
    *lpcreateconnstr(ccondriver, cconserver, ccondbname, cconuserid, cconpaswrd)
    lnhandle = SQLSTRINGCONNECT(lcconnstr)
    IF lnhandle < 0
      LOOP 
    ENDIF
    SELECT (updtables)
    LOCATE
    SCAN
       lctable = cfile_nam
       lcfile_ttl = cfile_ttl
       WAIT WINDOW TIMEOUT 1 "Update "+lctable+"-"+ALLTRIM(lcfile_ttl)+" In company "+lccompid+", Please Wait."
       IF  .NOT. lbindexonly=.T.
          *E303030,1  BEGIN
          *SET KEY TO PADR(ALLTRIM(lctable), 8) IN (sydflfld)
          SET KEY TO PADR(ALLTRIM(lctable),oAriaApplication.FileW) IN (sydflfld)
          *E303030,1  END
          objremotedataaccess.mcreatetable(lccompid, lctable, lcfile_ttl, sydflfld, sydfield, sydindex, ndatasessionid, lnhandle, .F., lcconnstr)
       ENDIF
       *E303030,1  BEGIN
       *SET KEY TO PADR(ALLTRIM(lctable), 8) IN (sydindex)
       SET KEY TO PADR(ALLTRIM(lctable),oAriaApplication.FileW) IN (sydindex)
       *E303030,1  END
       objremotedataaccess.mcreateindex(lccompid, lctable, lcfile_ttl, sydindex, ndatasessionid, lnhandle, lcconnstr)
       *MEDIA
       SELECT (sydindex)
       SET KEY TO 
       SELECT (sydflfld)
       SET KEY TO 
       *MEDIA
    ENDSCAN
    SELECT 'syccomp_A' 
 ENDSCAN
 RELEASE objremotedataaccess
 USE IN 'syccomp_A' 
 

*! E302857,1 MMT 04/27/2011 Update File Structure for SQL Table[MEDIA][End]
*- End of updatetablestructure.

*!**************************************************************************
*!
*!      Function: lpSavScr
*!
*!**************************************************************************
* Procedure to save the current company record...
*
FUNCTION lpSavScr
PARAMETERS loFormSet

*E303339,1 TMI 01/13/2013 [Start] move to the beforesave validation function
*!*	** Get the path...
*!*	loFormSet.laData[11] = IIF(RIGHT(ALLTRIM(loFormSet.laData[11]),1)<>'\',;
*!*	                  UPPER(ALLTRIM(loFormSet.laData[11]))+'\',;
*!*	                  UPPER(ALLTRIM(loFormSet.laData[11])))

*!*	** See if this company or not in the company file **
*!*	** for defining the menu popup comany bar...
*!*	SELECT syccomp
*!*	loFormSet.lnComRec = IIF(RECNO()>RECCOUNT(),0,RECNO())
*!*	GO TOP
*!*	IF EOF()
*!*	  llFirstComp = .T.
*!*	ELSE
*!*	  llFirstComp = .F.
*!*	ENDIF


*!*	IF loFormSet.lnComRec > 0
*!*	  GOTO loFormSet.lnComRec
*!*	ENDIF  

*!*	*E303339,1 TMI 01/13/2013 [Start] 
*!*	LOCAL lcPath
*!*	lcPath = ALLTRIM(SUBSTR(loFormSet.laData[11],1,RAT('\',loFormSet.laData[11],3)))+'SYSFILES'
*!*	IF ! lcPath $ DBF('SYCCOMP')
*!*	  USE IN SYCCOMP
*!*	  lfBuff('SYCCOMP')
*!*	ENDIF 
*!*	*E303339,1 TMI 01/13/2013 [End  ] 

*!*	*E303339,1 TMI 01/13/2013 [Start] get the sql connection data for the active company
*!*	=SEEK(oAriaApplication.ActiveCompanyID,'SYCCOMP')
*!*	DIMENSION laConnInfo[5]
*!*	SCATTER FIELDS CCONDRIVER,CCONSERVER,CCONDBNAME,CCONUSERID,CCONPASWRD TO laConnInfo
*!*	laConnInfo[3] = loFormSet.laData[1]
*!*	*E303339,1 TMI 01/13/2013 [End  ] 

*!*	** Save this company record in the company file. **
*!*	SELECT sycComp
*!*	IF loFormSet.ActiveMode = 'E'
*!*	 SEEK loFormSet.laData[1]
*!*	ENDIF
*!*	IF loFormSet.ActiveMode = 'A'
*!*	  APPEND BLANK
*!*	ENDIF
*!*	*E303339,1 TMI 01/08/2013 [Start] 
*!*	lcScFields = loFormSet.lcScFields
*!*	*E303339,1 TMI 01/08/2013 [End  ] 
*!*	GATHER FROM loFormSet.laData FIELDS &lcScFields MEMO
*!*	IF loFormSet.ActiveMode = 'A'
*!*	  GATHER FIELDS CCONDRIVER,CCONSERVER,CCONDBNAME,CCONUSERID,CCONPASWRD FROM laConnInfo
*!*	  IF !oAriaApplication.RemoteCompanyData.mcreatedatabase(loFormSet.laData[1])
*!*	    =gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_CONVERT_DATABASE)
*!*	    RETURN .F.
*!*	  ENDIF  
*!*	ENDIF 
*!*	=gfAdd_Info('sycComp')
*!*	*E303339,1 TMI 01/10/2013 [Start] 
*!*	SELECT syccomp
*!*	=gfTableUpdate()
*E303339,1 TMI 01/10/2013 [End  ] 
** This process will be execute if add mode only...
IF loFormSet.ActiveMode = 'A' 
  lcSavDef = FULLPATH(SET("DEFAULT"))

  *E303339,1 TMI 01/09/2013 [Start] move to before save
  *lcDbfPath = IIF(RIGHT(ALLTRIM(loFormSet.laData[11]),1)='\',;
  *              SUBSTR(ALLTRIM(loFormSet.laData[11]) , 1 , LEN(ALLTRIM(loFormSet.laData[11])) - 3),;
  *              SUBSTR(ALLTRIM(loFormSet.laData[11]) , 1 , LEN(ALLTRIM(loFormSet.laData[11])) - 2))
  *IF !gfValdPath(lcDbfPath)
  *  lcDbfPath = IIF(RIGHT(ALLTRIM(lcDbfPath),1)='\',;
  *              SUBSTR(ALLTRIM(lcDbfPath),1,LEN(ALLTRIM(lcDbfPath))-1),;
  *              ALLTRIM(lcDbfPath))
  *      =gfMkDir(lcDbfPath)
  *ENDIF
  *  
  *** If the path is not found, create it. **
  *IF !gfValdPath(loFormSet.laData[11])
  *  lcNewPath =IIF(RIGHT(ALLTRIM(loFormSet.laData[11]),1)='\',;
  *                 SUBSTR(ALLTRIM(loFormSet.laData[11]),1,;
  *                 LEN(ALLTRIM(loFormSet.laData[11]))-1),;
  *                 ALLTRIM(loFormSet.laData[11]))
  *      =gfMkDir(lcNewPath)
  *ENDIF
  *E303339,1 TMI 01/09/2013 [End  ] 
  
  
  lcAliasSel = SELECT()
  =gfOpenFile(oAriaApplication.SysPath+'SYCCONFG')
  COPY TO (ALLT(loFormSet.laData[11])+'\SETUPS') FOR EMPTY(CAPP_ID) WITH CDX
  SELECT (lcAliasSel)
  lcAliasSel = SELECT()
  =lfInstall(loFormSet,'SY')   
  SELECT (lcAliasSel)  
  SET DEFAULT TO &lcSavDef
  
  ** Add this company title in the menu **
  ** popup of the companies...
  IF loFormSet.llFirstComp
    lnBarNo = CNTBAR('_COMPANIES')
  ELSE
    lnBarNo = CNTBAR('_COMPANIES')+1
  ENDIF
  DEFINE BAR lnBarNo OF _COMPANIES PROMPT loFormSet.laData[1]+'-'+ALLTRIM(loFormSet.laData[2])

  lcComp_Id = '"'+loFormSet.laData[1]+'"'
  *E303339,1 TMI 01/13/2013 [Start] 
  *ON SELECTION BAR lnBarNo OF _COMPANIES ;
              DO gfChngComp WITH &lcComp_Id  &&
  *E303339,1 TMI 01/13/2013 [End  ] 
 
  glCmpCreat = .T.
  
  ** Go to the view mode so we can open the **
  ** related program to add a company...
  *laScrMode    = .F.
  *laScrMode[2] = .T.
  loFormSet.ChangeMode('V')
  *SHOW GETS

  ** If this company is not a child company. **
  IF EMPTY(loFormSet.laData[14])
  
    *E303339,1 TMI 01/13/2013 [Start] create the sql database and sql tables
    *x*
    
    *E303339,1 TMI 01/13/2013 [End  ] 
    
    ** Do you want to add Fiscal year for Company ð Now ? **
    ** < Yes > - <  No  > **
    IF gfModalGen("QRM00066B00006","DIALOG",loFormSet.laData[1]) = 1
      ** Save the current environment in the static file. **
      *lcCurrWind = gcBaseWind
      *=gfStatic()
      *lcWindow   = UPPER(lcBaseWind)
      loFormSet.llSavFis   = .F.
      glFirsTime = .T.

      *gcBaseWind ='AWRSMFSYER'

      ** Call the fiscal year program to add the **
      ** fiscal years for  this company  and its **
      ** periods and holidays...
      *lcAppName  = gcAppHome+'SM.APP'
      
      *DO (lcAppName) WITH 'SMFSYER',"'"+loFormSet.laData[1]+"'"    &&
      LOCAL lcSetProc
      lcSetProc = SET("Procedure")
      SET PROCEDURE TO       
      DO (oariaapplication.applicationhome+'SM\SMFSYER.FXP') WITH loFormSet.laData[1]
      SET PROCEDURE TO &lcSetProc       
      
      =gfOpenFile(oAriaApplication.SysPath+"SYCCOMP",'CCOMP_ID')         

      lcSavExact = SET('EXACT')
      SET EXACT ON

      llTempFis = loFormSet.llSavFis
      
      *E303339,1 TMI 01/09/2013 [Start] 
      *IF SEEK ('WIN'+lcWindow+UPPER(gcUser_Id)+gcStation,'syustatc')   
      *  ** Restore data for the company program from static file. **
      *  RESTORE FROM MEMO syustatc.mObj_Data  ADDITIVE
      *ENDIF  
      *E303339,1 TMI 01/09/2013 [End  ] 
      SET EXACT &lcSavExact
      
      * Reposition file pointer in SYCCOMP after restoring loFormSet.laData[1]
      =SEEK(loFormSet.laData[1],'SYCCOMP')

      loFormSet.llSavFis = llTempFis
      glQuitting = .F.
      *gcBaseWind = lcCurrWind
    ENDIF
     SELECT sycComp
    
    ** Refresh the current year and current period **
    ** that displayed in the screen as say fields. **
    loFormSet.laData[12] = syccomp.ccurr_yer
    loFormSet.laData[13] = syccomp.ccurr_prd
    SHOW GETS
    
    ** Do you want to add account code strucure for Company ð Now? **
    ** < Yes > - <  No  > **
    IF gfModalGen("QRM00172B00006","DIALOG",loFormSet.laData[1]) = 1
      ** Save the current environment in the static file. **
      *lcCurrWind = gcBaseWind
      *=gfStatic()
      *lcWindow   = UPPER(lcBaseWind)
      loFormSet.llSavAcc   = .F.
      glFirsTime = .T.

      *gcBaseWind ='AWDSMACCOD'
      
      ** Call the account code strucure program **
      ** to add the account code struc. for the **
      ** added comapny...
      
      *lcAppName  = gcAppHome+'SM.APP'
      *DO (lcAppName) WITH 'SMACCOD',"'"+loFormSet.laData[1]+"'"    
      LOCAL lcSetProc
      lcSetProc = SET("Procedure")
      SET PROCEDURE TO       
      llSavAcc = .F.
      DO (oariaapplication.applicationhome+'SM\SMACCOD.FXP') WITH loFormSet.laData[1]
      loFormSet.llSavAcc = llSavAcc
      SET PROCEDURE TO &lcSetProc 
      
      =gfOpenFile(oAriaApplication.SysPath+"SYCCOMP",'CCOMP_ID')         

      lcSavExact = SET('EXACT')
      SET EXACT ON
      
      llTempAcc = loFormSet.llSavAcc
      
      *E303339,1 TMI 01/09/2013 [Start] 
      *IF SEEK ('WIN'+lcWindow+UPPER(gcUser_Id)+gcStation,'syustatc')   
      *  ** Restore data for the company program from static file. **
      *  RESTORE FROM MEMO syustatc.mObj_Data  ADDITIVE
      *ENDIF  
      *E303339,1 TMI 01/09/2013 [End  ] 
      SET EXACT &lcSavExact
      
      * Reposition file pointer in SYCCOMP after restoring loFormSet.laData[1]
      =SEEK(loFormSet.laData[1],'SYCCOMP')

      loFormSet.llSavAcc = llTempAcc
      glQuitting = .F.
      *gcBaseWind = lcCurrWind
    ENDIF
  ELSE
    ** This company is a child company for **
    ** company loFormSet.laData[14].  So this child **
    ** company will use the the parent company **
    ** fiscal year and account code structure...
    ** <  Ok  > **
    =gfModalGen("TRM00170B00000","DIALOG",;
                "("+loFormSet.laData[14]+"-"+ALLTRIM(loFormSet.lcPrntName)+")")
    loFormSet.llSavFis = .T.
    loFormSet.llSavAcc = .T.
    
    ** If the current company is a child company **
    ** It has to  have its  parent account  code **
    ** strucure...
  ENDIF
  
  SELECT sycComp

  ** Do you want to install the company modules now? **
  ** <  Yes  > - <  No  > **
  IF gfModalGen("QRM00177B00006","DIALOG") = 1
  
    ** Collect all the available modules in the system. **
    DECLARE loFormSet.laAllMod[1]
    loFormSet.laAllMod = " "
    
    IF EMPTY(loFormSet.laData[14])
      ** If this company is a parent compnay. **
      
      SELECT "  "+cApp_Id+" "+cApp_name+" +"+IIF(lSetReq,"T","F") ;
             FROM (oAriaApplication.SysPath+"SYDAPPL") ;
             WHERE cApp_Id <> "SM" .AND. ;
                   cApp_Id <> "SY" ;
             INTO ARRAY loFormSet.laAllMod

    ELSE
      ** If this company is a child company. **
      
      SELECT "  "+cApp_Id+" "+cApp_name+" +"+IIF(lSetReq,"T","F") ;
             FROM (oAriaApplication.SysPath+"SYDAPPL") ;
             WHERE cApp_Id <> "SM" .AND. ;
                   cApp_Id <> "SY" .AND. ;
                   !lparntmdl ;
             INTO ARRAY loFormSet.laAllMod
    ENDIF
    * if sort the modules selected to be installed to make the communication module
    * first module if selected
    =lfSortMod(loFormSet)
    IF _TALLY > 0
      
      ** Call the installtion and setup modules screen. **
      loFormSet.llSavMod  = .T.
      *lnMaxScrn = 68.750
      *lnColPos  = 22
      loFormSet.llView    = .T.
      loFormSet.lcPrompt  = loFormSet.lcTSelect
      lcRunScx = lfGetScx(oAriaApplication.ActiveModuleID + "\SMINSET.scx") 
      DO FORM (lcRunScx) WITH loFormSet,.T.
    ELSE
      ** There is no modules to install its files. **
      ** <  Ok  > **
      =gfModalGen("TRM00178B00000","DIALOG")
    ENDIF
  ELSE
    ** Without installion you cannot access **
    ** any module in this company. **
    ** <  Ok  > **
    =gfModalGen("TRM00182B00000","DIALOG")
  ENDIF
  llCSave = .F.
ENDIF

SELECT SYCCOMP
IF loFormSet.lnComRec > 0
  GOTO loFormSet.lnComRec
ENDIF  

IF loFormSet.llFirstComp
  ** Do you want to set this company as a system default company. **
  ** <  Yes  > - <  Cancel  > **
  IF gfModalGen("TRM00199B00006","DIALOG") = 1
    =gfOpenFile(oAriaApplication.SysPath+'SYCINST')
    *SELECT SYCINST
    REPLACE sycinst.cinsdfcom WITH loFormSet.laData[1]
    SELECT SYCCOMP
  ENDIF
  
  ** Do you want to select the comany now. **
  ** <  Yes  > - <  Cancel  > **
  IF gfModalGen("TRM00191B00006","DIALOG") = 1
    *E303339,1 TMI 01/10/2013 [Start] 
    =gfChngComp(loFormSet.laData[1])
    
  ENDIF
ENDIF


*E303339,1 TMI 01/14/2013 [Start] update the setups table
IF USED('SETUPS')
  SELECT SETUPS
  IF CURSORGETPROP("Buffering")>1  
    gfTableUpdate()
  ENDIF 
ENDIF   
*E303339,1 TMI 01/14/2013 [End  ] 
*E303339,1 TMI 01/14/2013 [Start] 
IF USED('SYCCOMP')
  SELECT SYCCOMP
  IF CURSORGETPROP("Buffering")>1  
    gfTableUpdate()
  ENDIF 
ENDIF   
*E303339,1 TMI 01/14/2013 [End  ] 

*E303339,1 TMI 01/10/2013 [Start] comment this as it happens only in edit mode
*!*	IF loFormSet.laData[1] = oAriaApplication.ActiveCompanyID
*!*	  gcCom_Name = ALLTRIM(sycComp.cCom_Name)
*!*	  
*!*	  gcDataDir  = gfGetDataDir(ALLTRIM(sycComp.cCom_dDir))
*!*	  
*!*	  gcComp_Mdl = ALLTRIM(sycComp.mComp_Mdl)
*!*	  gcComp_Lvl = IIF(EMPTY(sycComp.ccompprnt),'P','C')
*!*	  gcPrnt_Cmp = IIF(!EMPTY(sycComp.ccompprnt),;
*!*	                   sycComp.ccompprnt,sycComp.ccomp_Id)
*!*	  gcContCode = ALLTRIM(loFormSet.laData[8])
*!*	  *E303339,1 TMI 01/08/2013 [Start] comment 
*!*	  *=gfCompSets(ALLTRIM(syccomp.ccont_code))
*!*	  *E303339,1 TMI 01/08/2013 [End  ] 
*!*	ENDIF  
RETURN 
*-- end of lpsavScr. Setups
  ** do form changemo
*!**************************************************************************
*!
*!      Function: lpDelScr
*!
*!**************************************************************************
** Procedure to delete the current company record...
*
FUNCTION lpDelScr
PARAMETERS loFormSet

** Check if any user is using this company. **
=gfOpenTable(oariaapplication.caria4syspath+'SYUSTATC','','SH')
SELECT SYUSTATC
GO TOP
LOCATE FOR ALLTRIM(cComp_id) = ALLTRIM(loFormSet.laData[1])
IF FOUND() .AND. RLOCK()
  ** Company is in use by onther user, Can't Deleted. **
  ** <  Ok  > **
  =gfModalGen("INM00059B00000","ALERT",+" "+loFormSet.laData[1])
  SELECT sycComp
  RETURN
ENDIF


SELECT sycComp
** Check if this company is a parent company to any other companies...
loFormSet.lnComRec = RECNO()

LOCATE FOR sycComp.cCompPrnt = loFormSet.laData[1]
IF FOUND()
  ** This company is a parent company for 1 or more company. **
  ** You cannot delete it...
  ** <  Ok  > **
  =gfModalGen("TRM00167B00000","DIALOG")
  IF loFormSet.lnComRec <= RECCOUNT()
    GO loFormSet.lnComRec
  ENDIF
  RETURN
ENDIF

IF loFormSet.lnComRec <= RECCOUNT()
  GO loFormSet.lnComRec
ENDIF

** Check if this company is the default company for the system...
=gfOpenFile(oAriaApplication.SysPath+'SYCINST')
*SELECT SYCINST

IF SYCINST.cInsdfcom = loFormSet.laData[1]
  ** This company is the system default company. **
  ** < Clear default > - < Cancel deletion > **
  IF gfModalGen("TRM00168B00028","DIALOG") = 1
    REPLACE SYCINST.cInsdfcom WITH "  "
  ELSE
    SELECT sycComp
    RETURN
  ENDIF
ENDIF

** Check if this company is the default company for any user...
SELECT SYUUSER
LOCATE FOR cUsr_dcom = loFormSet.laData[1]
IF FOUND()
  ** This company is the default company for one or more than user. **
  ** < Clear default > - < Cancel deletion > **
  IF gfModalGen("TRM00169B00028","DIALOG") = 1
    SCAN FOR cUsr_dcom = loFormSet.laData[1]
      REPLACE SYUUSER.cUsr_dcom WITH "  "
    ENDSCAN
  ELSE
    SELECT sycComp
    RETURN
  ENDIF
ENDIF

DECLARE laTally[1]
laTally = 0

lcDataDir = gfGetDataDir(ALLTRIM(IIF(SEEK(loFormSet.laData[14],'SYCCOMP'), SYCCOMP.cCom_DDir, loFormSet.laData[11])))

=SEEK(loFormSet.laData[1],'SYCCOMP')
_TALLY = 0
IF FILE(lcDataDir+"ACCOD")
  SELECT COUNT(*) AS nTemp ;
         FROM (lcDataDir+"ACCOD") ;
         WHERE ACCOD.LLOK_STAT 
ENDIF 
IF _TALLY = 0 .AND. FILE(lcDataDir+"FISHD")       

  SELECT COUNT(*) AS nTemp ;
         FROM (lcDataDir+"FISHD") ;
         WHERE FISHD.LLOK_STAT ;
         INTO ARRAY laTally ORDER BY nTemp DESC

ENDIF         

IF laTally[1] > 0
  ** You cannot delete this company.  There **
  ** is a user editing its system data now. **
  ** <  Ok  > **
  =gfModalGen("TRM00214B00000","DIALOG")
  SELECT sycComp
  RETURN
ENDIF

** Delete Company record from company file
SELECT SYCCOMP
IF GfGetMemVar('LLHIST',loFormSet.laData[1])
  IF SEEK(GfGetMemVar('M_Comp_Id',loFormSet.laData[1]))
    lcActDir = ALLTRIM(CCOM_DDIR)
    =gfOpenFile(lcActDir+'setups','modvar','SH','ACTSETUP',.T.)
    IF SEEK (CHR(255)+CHR(255)+'M_COMP_ID','ACTSETUP')
      SELECT ACTSETUP
      REPLACE MDATA_DEF WITH ''
    ENDIF
    =gfCloseFile('ACTSETUP')
    SELECT SYCCOMP
    SEEK loFormSet.laData[1]
  ENDIF
ENDIF
SCATTER MEMVAR MEMO BLANK
GATHER  MEMVAR MEMO 
DELETE
LOCATE
loFormSet.llFirstComp = EOF()
** Remove Company from menu
FOR lnCount3 = 1 TO CNTBAR('_COMPANIES')
  IF SUBSTR(PRMBAR('_COMPANIES',GETBAR('_COMPANIES',lnCount3)),1,2)=ALLTRIM(loFormSet.laData[1])
    RELEASE BAR lnCount3 OF _COMPANIES
    glCmpCreat = .T.
    EXIT
  ENDIF
ENDFOR
IF loFormSet.llFirstComp
  DEFINE BAR 1 OF _COMPANIES;
     PROMPT "No companies available";
     COLOR SCHEME 3
ENDIF
** Delete company or remove files after chang company path
*laScrMode    = .F.
*laScrMode[1] = .T.
loFormSet.ChangeMode('S')

lcExactSt = SET('EXACT')
SET EXACT OFF

** Delete all the users privileges for this company. **
SELECT SYUUSRPR
SCAN FOR cComp_Id = loFormSet.laData[1]
  DELETE
ENDSCAN

** Delete all the tasks managers related to this company. **
SELECT SYUDTASK
SCAN FOR cComp_Id = loFormSet.laData[1]
  DELETE
ENDSCAN
    
SET EXACT &lcExactSt

** Delete company files or not. **
** Are you sure you want to delete the **
** company files in 'loFormSet.laData[11]'?...
** < Yes > - <  No  > **
IF gfModalGen("QRM00055B00006","ALERT",+ALLTRIM(loFormSet.laData[11])) = 1
  =lfDel_Fil()
ENDIF

SELECT SYCCOMP

*!**************************************************************************
*!
*!      Function: lpClsScr
*!
*!**************************************************************************
** Procedure to cancel the current company record...
*
FUNCTION lpClsScr

SELECT SYCCOMP
IF loFormSet.ActiveMode = 'E' .OR. loFormSet.ActiveMode = 'A'
  IF loFormSet.ActiveMode = 'E'
    REPLACE mComp_Mdl WITH loFormSet.laData[15]
  ENDIF
  SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData
ENDIF

*!**************************************************************************
*!
*!      Function: lfvInsSet
*!
*!**************************************************************************
** Valid function for the push button < Installation & Setup >
*
FUNCTION lfvInsSet
PARAMETERS loFormSet

** Check if the account code strucure was **
** entered or not for current company...
LOCAL lcPath
lcPath = ALLTRIM(SUBSTR(loFormSet.laData[11],1,RAT('\',loFormSet.laData[11],3)))+'SYSFILES'
IF ! lcPath $ DBF('SYCCOMP')
  USE IN SYCCOMP
  lfBuff('SYCCOMP')
ENDIF 
SELECT SYCCOMP
SET ORDER TO 1

lcDataDir  = gfGetDataDir(ALLTRIM(IIF(SEEK(loFormSet.laData[14],'SYCCOMP'), SYCCOMP.cCom_DDir, loFormSet.laData[11])))

=lfOpnFiles('ACCOD','ACCSEGNO')
=SEEK(loFormSet.laData[1], 'SYCCOMP')
SELECT ACCOD

loFormSet.llSavAcc = SEEK("")
** Check if the fiscal years was entered **
** or not for current company...
=lfOpnFiles('FISHD','COMPFYEAR')
SELECT FISHD

loFormSet.llSavFis = SEEK("")

** Flag to say I'm coming from edit mode. **
loFormSet.llSavMod = .F.

FOR lnCount4 = 1 TO ALEN(loFormSet.laAllMod,1)
  IF AT(SUBSTR(loFormSet.laAllMod[lnCount4],3,2),loFormSet.lcOldMod) > 0
    loFormSet.laAllMod[lnCount4] = SUBSTR(loFormSet.laAllMod[lnCount4],1,37)+;
                         LANG_Installed+RIGHT(loFormSet.laAllMod[lnCount4],1)
  ELSE
    loFormSet.laAllMod[lnCount4] = SUBSTR(loFormSet.laAllMod[lnCount4],1,37)+;
                         "             "+RIGHT(loFormSet.laAllMod[lnCount4],1)
  ENDIF
ENDFOR

*lnMaxScrn = IIF(loFormSet.ActiveMode = 'V',53,68.750)
*lnColPos  = IIF(loFormSet.ActiveMode = 'V',13,22)
loFormSet.llView    = IIF(loFormSet.ActiveMode = 'V',.F.,.T.)
loFormSet.lcPrompt  = IIF(LEFT(loFormSet.laAllMod[1,1],1) = " ", loFormSet.lcTSelect , loFormSet.lcTUnSelct )

lcRunScx = lfGetScx(oAriaApplication.ActiveModuleID + "\SMINSET.scx")
DO FORM (lcRunScx) WITH loFormSet
SELECT SYCCOMP

*!**************************************************************************
*!
*!      Function: lfvSel
*!
*!**************************************************************************
*
FUNCTION lfvSel
PARAMETERS loBrnFormSet
loFormSet = loBrnFormSet.loFormSet 

=lfvModules(loBrnFormSet)

*!**************************************************************************
*!
*!      Function: lfvAll
*!
*!**************************************************************************
*
FUNCTION lfvAll
PARAMETERS loBrnFormSet
loFormSet = loBrnFormSet.loFormSet 

FOR lnCont1 = 1 TO ALEN(loFormSet.laAllMod,1)
  IF AT(SUBSTR(loFormSet.laAllMod[lnCont1],3,2),loFormSet.lcOldMod) > 0
    LOOP
  ELSE
    IF LEFT(loFormSet.laAllMod[lnCont1],1) = " "
      loFormSet.laAllMod[lnCont1] = loFormSet.lcDiamond + SUBSTR(loFormSet.laAllMod[lnCont1],2)
    ENDIF
  ENDIF
ENDFOR

*SHOW GET lsModules

=lfStatus(loBrnFormSet)

*!**************************************************************************
*!
*!      Function: lfvNon
*!
*!**************************************************************************
*
FUNCTION lfvNon
PARAMETERS loBrnFormSet
loFormSet = loBrnFormSet.loFormSet 

FOR lnCont1 = 1 TO ALEN(loFormSet.laAllMod,1)
  IF LEFT(loFormSet.laAllMod[lnCont1],1) = loFormSet.lcDiamond
    loFormSet.laAllMod[lnCont1] = " " + SUBSTR(loFormSet.laAllMod[lnCont1],2)
  ENDIF
ENDFOR

*SHOW GET lsModules

=lfStatus(loBrnFormSet)

*!**************************************************************************
*!
*!      Function: lfvInv
*!
*!**************************************************************************
*
FUNCTION lfvInv
PARAMETERS loBrnFormSet
loFormSet = loBrnFormSet.loFormSet 

FOR lnCont1 = 1 TO ALEN(loFormSet.laAllMod,1)
  IF AT(SUBSTR(loFormSet.laAllMod[lnCont1],3,2),loFormSet.lcOldMod) > 0
    LOOP
  ELSE
    IF LEFT(loFormSet.laAllMod[lnCont1],1) = " "
      loFormSet.laAllMod[lnCont1] = loFormSet.lcDiamond + SUBSTR(loFormSet.laAllMod[lnCont1],2)
    ELSE
      loFormSet.laAllMod[lnCont1] = " " + SUBSTR(loFormSet.laAllMod[lnCont1],2)
    ENDIF
  ENDIF
ENDFOR

*SHOW GET lsModules

=lfStatus(loBrnFormSet)

*!**************************************************************************
*!
*!      Function: lfStatus
*!
*!**************************************************************************
*
FUNCTION lfStatus
PARAMETERS loBrnFormSet
loFormSet = loBrnFormSet.loFormSet

IF loBrnFormSet.lsModules>0
  IF LEFT(loFormSet.laAllMod[loBrnFormSet.lsModules],1) = " "
    loFormSet.lcPrompt = loFormSet.lcTSelect
    *SHOW GET pbSel,1 PROMPT LANG_lcTSelect
  ELSE
    loFormSet.lcPrompt = loFormSet.lcTUnSelct
    *SHOW GET pbSel,1 PROMPT LANG_lcTUnSelct
  ENDIF
  loFld = loBrnFormSet.Ariaform1.laAllMod
  WITH loBrnFormSet.Ariaform1
    .pbSel.Caption = loFormSet.lcPrompt
    .pbUnIns.Enabled = 'INSTALLED'$UPPER(loFld.Value)
    .pbSet.Enabled = 'INSTALLED'$UPPER(loFld.Value)
  ENDWITH   
ENDIF 
loBrnFormSet.Ariaform1.laAllMod.Refresh()
*!*	*!**************************************************************************
*!*	*!
*!*	*!      Function: lfwModules
*!*	*!
*!*	*!**************************************************************************
*!*	** When function for the list that hold all the modules **
*!*	** linked to this company in the SMINSET screen...
*!*	*
*!*	FUNCTION lfwModules

*!*	IF lsModules = 0 .OR. lsModules > ALEN(laAllMod,1) .OR. !llView
*!*	  RETURN
*!*	ENDIF

*!*	** Check if this module is installed before or not...
*!*	IF AT(SUBSTR(laAllMod[lsModules],3,2),lcOldMod) > 0
*!*	  SHOW GET pbIns   ENABLE
*!*	  SHOW GET pbUnIns ENABLE
*!*	  ** Check if setup required or not...
*!*	  IF RIGHT(laAllMod[lsModules],1) = "T"
*!*	    SHOW GET pbSet ENABLE
*!*	  ELSE
*!*	    SHOW GET pbSet DISABLE
*!*	  ENDIF
*!*	ELSE
*!*	  SHOW GET pbIns   ENABLE
*!*	  SHOW GET pbUnIns DISABLE
*!*	  SHOW GET pbSet   DISABLE
*!*	ENDIF

*!*	=lfStatus()

*!**************************************************************************
*!
*!      Function: lfvModules
*!
*!**************************************************************************
** Valid function for the list that hold all the modules **
** linked to this company in the SMINSET screen...
*
FUNCTION lfvModules
PARAMETERS loBrnFormSet,loFld
loFormSet = loBrnFormSet.loFormSet 

IF loBrnFormSet.lsModules = 0 .OR. loBrnFormSet.lsModules > ALEN(loFormSet.laAllMod,1) .OR. !loFormSet.llView
  RETURN
ENDIF

** If installed before. **
IF AT(SUBSTR(loFormSet.laAllMod[loBrnFormSet.lsModules],3,2),loFormSet.lcOldMod) > 0
  ** This module has been installed before. **
  ** <  Ok  > **
  =gfModalGen("TRM00190B00000","DIALOG")
  RETURN
ELSE
  IF LEFT(loFormSet.laAllMod[loBrnFormSet.lsModules],1) = " "
    loFormSet.laAllMod[loBrnFormSet.lsModules] = loFormSet.lcDiamond + SUBSTR(loFormSet.laAllMod[loBrnFormSet.lsModules],2)
  ELSE
    loFormSet.laAllMod[loBrnFormSet.lsModules] = " " + SUBSTR(loFormSet.laAllMod[loBrnFormSet.lsModules],2)
  ENDIF
ENDIF

_CUROBJ = _CUROBJ
*SHOW GET lsModules

=lfStatus(loBrnFormSet)

*!**************************************************************************
*!
*!      Function: lfvIns
*!
*!**************************************************************************
** Valid function for the push button < Install > OR < Reinstall > ...
*
FUNCTION lfvIns
PARAMETERS loBrnFormSet
loFormSet = loBrnFormSet.loFormSet

LOCAL lnSlct
lnSlct = SELECT(0)

PRIVATE lcReqMod, lcModl, laTempArr
STORE SPACE(0) TO lcReqMod, lcModl
DECLARE laReqMod[1,2]
SELECT '*'+cApp_Id,mReqModul ;
  FROM (oAriaApplication.SysPath+"SYDAPPL") ;
  WHERE IIF(EMPTY(loFormSet.laData(14)),.T.,!lparntmdl) ;
  INTO ARRAY lareqMod

*Create an array laTempArr to have the module level to sort on that 
*level then install higher levels first.
DIMENSION laTempArr[ALEN(loFormSet.laAllMod,1),4]
FOR lnC=1 TO ALEN(loFormSet.laAllMod,1)
  laTempArr[lnC,2] = loFormSet.laAllMod[lnC]
  laTempArr[lnC,3] = SUBSTR(loFormSet.laAllMod[lnC],3,2)
  laTempArr[lnC,4] = lnC
ENDFOR
FOR lnC=1 TO ALEN(loFormSet.laAllMod,1)
  IF AT(loFormSet.lcDiamond,loFormSet.laAllMod[lnC]) = 0
    laTempArr[lnC,1] = 0
  ELSE
   laTempArr[lnC,1] = lfGetLevel(SUBSTR(loFormSet.laAllMod[lnC],3,2),lnC)
  ENDIF
ENDFOR

FOR lnCnt = 1 TO ALEN(laTempArr,1)
  IF AT(loFormSet.lcDiamond,laTempArr[lnCnt,2]) = 0
    LOOP
  ENDIF
  lnCount2 = laTempArr[lnCnt,4]
  lnModPos = ASCAN(laReqMod,'*'+laTempArr[lnCnt,3]) + 1
  IF !EMPTY(laReqMod[lnModPos])
    lcReqMod = SPACE(0)
    lnPos1 = 1
    FOR lnC = 1 TO IIF(OCCURS('|',laReqMod[lnModPos])=0,1,OCCURS('|',laReqMod[lnModPos])+1)
      IF ATC('|',laReqMod[lnModPos],lnC) # 0
        lcModl = SUBSTR(laReqMod[lnModPos],lnPos1,ATC('|',laReqMod[lnModPos],lnC)-lnPos1)
      ELSE
        lcModl = SUBSTR(laReqMod[lnModPos],lnPos1)
      ENDIF
      lnPos1 = ATC('|',laReqMod[lnModPos],lnC)+1
      IF !(lcModl $ loFormSet.lcOldMod) 
        lcReqMod = lcReqMod + IIF(EMPTY(lcReqMod),'',',')+lcModl
      ENDIF
    ENDFOR
    IF !EMPTY(lcReqMod)
      *-- Message "Module "+ laTempArr[lnCnt,3] +" requires the installation 
      *--         of " + lcReqMod + " module(s) prior to it, Cannot install."
      =gfModalGen("INM00320B00000","DIALOG",laTempArr[lnCnt,3]+"|"+lcReqMod)
      lcReqMod = SPACE(0)
      LOOP
    ENDIF
  ENDIF
  
  
  lcCurModul = SUBSTR(loFormSet.laAllMod[lnCount2],3,2)
  lcCurLogNm = SUBSTR(ALLTRIM(LEFT(loFormSet.laAllMod[lnCount2],;
                      LEN(loFormSet.laAllMod[lnCount2])-3)),6,30)
  llSetReq   = IIF(RIGHT(loFormSet.laAllMod[lnCount2],1) = "T",.T.,.F.)

  ** Do you want to install files for module 'laAllMod[lnCount2]'? **
  ** <  Yes  > - <  No  > **
  IF gfModalGen("QRM00179B00006","DIALOG",lcCurLogNm) = 1
            
    ** Function to install files for current module. **
    IF lfInstall(loFormSet,lcCurModul)
     * if the user select yes for installing
     * ic module files then
     * copy file icistru without checking if it is exist in the
     * system files directory so if it does not exit an error
     * occus and the module will not be installed
     *E303339,1 TMI 01/14/2013 [Start] 
     lnSlct = SELECT(0)
     SELECT SYCCOMP
     CURSORSETPROP("Buffering",5)
     SELECT (lnSlct )
     *E303339,1 TMI 01/14/2013 [End  ] 

     IF !GfGetMemVar('LLHIST',loFormSet.laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',loFormSet.laData[1]))
       lnActAlias =  SELECT(0)
       SELECT SYCCOMP
       =SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])))
       lcActPath = loFormSet.laData[11]
       loFormSet.laData[11] = ALLTRIM(SYCCOMP.ccom_ddir)
       =lfInstall(loFormSet,lcCurModul)
       loFormSet.laData[11] = lcActPath
       SELECT SYCCOMP
       =SEEK(ALLTRIM(loFormSet.laData[1]))
       SELECT (lnActAlias)
     ENDIF 	

     IF lcCurModul $ 'IC,PS'
        lcItemFil = oAriaApplication.SysPath + "ICISTRU"      
        lcItemAlias = gfTempName()
        USE (ALLTRIM(loFormSet.laData[11])+"ICISTRU.DBF") AGAIN ALIAS &lcItemAlias IN 0
        SELECT (lcItemAlias)
        DELETE FOR citemrecty <> 'U'
        APPEND FROM (lcItemFil)
        USE IN (lcItemAlias)
      ENDIF
      IF AT(lcCurModul,loFormSet.lcOldMod) = 0
        loFormSet.lcOldMod   = loFormSet.lcOldMod + IIF(EMPTY(loFormSet.lcOldMod),'','|')+lcCurModul
      ENDIF
      loFormSet.laData[15] = loFormSet.laData[15] + IIF(EMPTY(loFormSet.laData[15]),'','|')+lcCurModul
      SELECT sycComp
      REPLACE mComp_mdl WITH loFormSet.laData[15]

      IF !GfGetMemVar('LLHIST',loFormSet.laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',loFormSet.laData[1]))
        =SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])))
        REPLACE mComp_mdl WITH loFormSet.laData[15]
        =SEEK(ALLTRIM(loFormSet.laData[1]))
      ENDIF 	

      loFormSet.laAllMod[lnCount2] = " "+SUBSTR(loFormSet.laAllMod[lnCount2],2,36)+;
                           "Installed    "+RIGHT(loFormSet.laAllMod[lnCount2],1)
            
      IF llSetReq
        ** Setup is required for module 'laAllMod[lnCount2]'. **
        ** Do you want to setup the module now? **
        ** <  Yes  > - <  No  > **
        IF gfModalGen("QRM00181B00006","DIALOG",lcCurLogNm) = 1
          
          ** If fiscal years not added successfully. **
          IF !loFormSet.llSavFis
            ** You cannot setup module {laAllMod[lnCount2]} **
            ** till you add fiscal years for this company **
            ** <  Ok  > **
            =gfModalGen("TRM00175B00000","ALERT",lcCurLogNm)
            LOOP
          ENDIF
                  
          ** If account code strucure not added successfully. **
          IF !loFormSet.llSavAcc
            ** You cannot setup module {laAllMod[lnCount2]} **
            ** till you add account code strucure for this **
            ** company...
            ** <  Ok  > **
            =gfModalGen("TRM00176B00000","ALERT",lcCurLogNm)
            LOOP
          ENDIF
                  
          ** Setup for cuurent module. **
          LOCAL lcComp_mdl
          lcComp_mdl = syccomp.mComp_mdl
          =lfSetModul(lcCurModul)
          REPLACE syccomp.mComp_mdl WITH lcComp_mdl 
          
          * copy apsetup & glsetup for History company[Start]
          IF !GfGetMemVar('LLHIST',loFormSet.laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',loFormSet.laData[1]))
            lnAlias = SELECT(0)
            SELECT SYCCOMP
            =SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])))
            lcSorDir = ALLTRIM(loFormSet.laData[11])
            lcTrgDir = ALLTRIM(SYCCOMP.ccom_ddir)
            =SEEK(ALLTRIM(loFormSet.laData[1]))
            SELECT (lnAlias)
            IF lcCurModul = 'AP'
              COPY FILE lcSorDir+'apsetup.dbf' TO lcTrgDir+'apsetup.dbf'
              COPY FILE lcSorDir+'apsetup.dbf' TO lcTrgDir+'apsetup.CDX'
            ENDIF
            IF lcCurModul = 'GL'
              COPY FILE lcSorDir+'glsetup.dbf' TO lcTrgDir+'glsetup.dbf'
              COPY FILE lcSorDir+'glsetup.fpt' TO lcTrgDir+'glsetup.fpt'
              COPY FILE lcSorDir+'glsetup.fpt' TO lcTrgDir+'glsetup.CDX'
            ENDIF
          ENDIF
                
        ENDIF
      ELSE
        SELECT SYCCOMP
        IF !(lcCurModul $ mModlSet)
          REPLACE mModlSet  WITH mModlSet+IIF(EMPTY(mModlSet),'',',')+lcCurModul
        ENDIF  

        IF !GfGetMemVar('LLHIST',loFormSet.laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',loFormSet.laData[1]))
          =SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])))
          IF !(lcCurModul $ mModlSet)
            REPLACE mModlSet  WITH mModlSet+IIF(EMPTY(mModlSet),'',',')+lcCurModul
          ENDIF  
          =SEEK(ALLTRIM(loFormSet.laData[1]))
        ENDIF 	

        IF loFormSet.laData[1] = oAriaApplication.ActiveCompanyID
          oAriaApplication.companysetupmodules = ALLTRIM(mModlSet)
        ENDIF
      ENDIF
    
    ELSE
      ** Installing files for module [laAllMod[lnCount2] **
      ** not done successfully...
      ** <  Ok  > **
      =gfModalGen("TRM00180B00000","DIALOG",lcCurLogNm)
      loFormSet.laAllMod[lnCount2] = " " + SUBSTR(loFormSet.laAllMod[lnCount2],2)
    ENDIF
    

  ENDIF
ENDFOR

*!*	_CUROBJ = OBJNUM(lsModules)
*!*	SHOW GET lsModules
*MCOMP_MDL
*MMODLSET
*loBrnFormSet.Ariaform1.pbUnIns.Enabled = .T.
*loBrnFormSet.Ariaform1.pbSet.Enabled = .T.
*loBrnFormSet.AriaForm1.laAllMod.Refresh()
=lfStatus(loBrnFormSet)
SELECT (lnSlct)
RETURN 
*!**************************************************************************
*!
*!      Function: lfvUnIns
*!
*!**************************************************************************
** Valid function for the push button < Uninstall > ...
*
FUNCTION lfvUnIns
PARAMETERS loBrnFormSet
loFormSet = loBrnFormSet.loFormSet

** Are you sure you want to uninstall **
** this module ?  You will  loose all **
** files related to this module...
** <  Yes  > - <  No  > **
IF gfModalGen("QRM00184B00006","ALERT") = 2
  RETURN
ENDIF

=gfOpenTable(oariaapplication.caria4syspath+'SYUSTATC','','SH')
SELECT SYUSTATC
GO TOP
LOCATE FOR ALLTRIM(cComp_id) = ALLTRIM(loFormSet.laData[1])
IF FOUND()
  ** There is a user using this company now, **
  ** You cannot uninstall any modul for this **
  ** company now...
  ** <  Ok  > **
  =gfModalGen("INM00216B00000","DIALOG",loFormSet.laData[1])
  SELECT sycComp
  RETURN
ENDIF

SELECT SYCCOMP

** Call the function of Uninstall module. **
=lfUnInstal(SUBSTR(loFormSet.laAllMod[loBrnFormSet.lsModules],3,2))

*loBrnFormSet.Ariaform1.pbUnIns.Enabled = .F.
*loBrnFormSet.Ariaform1.pbSet.Enabled = .F.
*loBrnFormSet.AriaForm1.laAllMod.Refresh()
=lfStatus(loBrnFormSet)

*!**************************************************************************
*!
*!      Function: lfvSet
*!
*!**************************************************************************
** Valid function of the push button < Setup > ...
*
FUNCTION lfvSet
PARAMETERS loBrnFormSet
loFormSet = loBrnFormSet.loFormSet


** Check if the fiscal year data was entered or not. **
IF !loFormSet.llSavFis
  ** You cannot setup module {laAllMod[lsModules]} **
  ** till you add fiscal years for this company **
  ** <  Ok  > **
  =gfModalGen("TRM00175B00000","ALERT",;
          ALLTRIM(SUBSTR(ALLTRIM(LEFT(loFormSet.laAllMod[loBrnFormSet.lsModules],;
          LEN(loFormSet.laAllMod[loBrnFormSet.lsModules])-3)),4,30)))
  RETURN
ENDIF

** Check if the account code strucure data was entered or not. **
IF !loFormSet.llSavAcc
  ** You cannot setup module {laAllMod[lsModules]} **
  ** till you add account code strucure for this **
  ** company...
  ** <  Ok  > **
  =gfModalGen("TRM00176B00000","ALERT",;
          ALLTRIM(SUBSTR(ALLTRIM(LEFT(loFormSet.laAllMod[loBrnFormSet.lsModules],;
          LEN(loFormSet.laAllMod[loBrnFormSet.lsModules])-3)),4,30)))
  RETURN
ENDIF

** Call the setup function. **
lcCurLogNm = SUBSTR(ALLTRIM(LEFT(loFormSet.laAllMod[loBrnFormSet.lsModules],;
                    LEN(loFormSet.laAllMod[loBrnFormSet.lsModules])-3)),6,30)


=lfSetModul(SUBSTR(loFormSet.laAllMod[loBrnFormSet.lsModules],3,2))

*!**************************************************************************
*!
*!      Function: lfInstall
*!
*!**************************************************************************
** Installtion function called from :_
**       Edit mode : _ lfvIns   { valid function of the push button 
**                              < Install > } in SMSELMD screen...
FUNCTION lfInstall
PARAMETERS loFormSet,lcMod2Ins
lcUpGrdLvl = IIF(lcMod2Ins='SY','S','A')
** Get all the files related to this module. **

IF USED('SYDFILES')
  USE IN SYDFILES
ENDIF
USE (oAriaApplication.SysPath+"SYDFILES") IN 0
IF USED('sydfield')
  USE IN sydfield
ENDIF
USE (oAriaApplication.SysPath+"sydfield") IN 0
IF USED('sydflfld')
  USE IN sydflfld
ENDIF
USE (oAriaApplication.SysPath+"sydflfld") IN 0
IF USED('sydindex')
  USE IN sydindex
ENDIF
USE (oAriaApplication.SysPath+"sydindex") IN 0

*SELECT SYDFILES
DECLARE laMdlFiles[1,3]
laMdlFiles = " "
_TALLY     = 0
SELECT CFILE_NAM,CFILE_TTL,MFILE_APP ;
       FROM (oAriaApplication.SysPath+"SYDFILES") ;
       WHERE lcMod2Ins $ (mfile_app) .AND. ;
             LEFT(CFILE_NAM,2) <> "SY" AND CUPGRDLVL=lcUpGrdLvl;
       INTO ARRAY laMdlFiles       
lnRecords = _TALLY

IF _TALLY = 0
  ** There is no files to be installed for this module. **
  ** <  Ok  > **
  =gfModalGen("TRM00186B00000","DIALOG")
  RETURN .F.
ENDIF

DECLARE laFil2Upd[1]
laFil2Upd = " "

* Set llYes2All to .T. to update all file structures without a 
* confirmation message. This variable is used by lfVer_Upd()
llYes2All = .T.

** Loop in the files related to the module **
** to be installed...
oProgress = NULL
FOR lnCount5 = 1 TO ALEN(laMdlFiles,1)
  lcFileNam  = UPPER(ALLTRIM(laMdlFiles[lnCount5,1]))
  lcPathFile = ALLTRIM(loFormSet.laData[11])+lcFileNam
  =gfThermo(lnRecords,lnCount5,"Creating: ",laMdlFiles[lnCount5,2])
  * call global function to create or update file structure
  * insted of creating the file directly
  lcTargDir = ALLTRIM(loFormSet.laData[11])
  lnSelected = lnCount5
  =lfver_upd(lcFileNam,.T.)
  LOOP
  ** Select the file structure in Array **
  DECLARE laFileStrc[1,5]
  laFileStrc = " "
      
  SELECT sydflfld.cfld_name,sydfield.cdata_typ, ;
         sydfield.nfld_wdth,sydfield.nfld_dec, ;
         sydflfld.nfld_pos ;
   FROM  (oAriaApplication.SysPath+"sydflfld"),(oAriaApplication.SysPath+"sydfield") ;
         ORDER BY sydflfld.nfld_pos ;
         GROUP BY sydField.cFld_Name ;
   WHERE UPPER(sydflfld.cfile_nam) = PADR(lcFileNam,8) .AND. ;
         sydfield.cfld_name = sydflfld.cfld_name ;
         INTO ARRAY laFileStrc
  =gfAdel(@laFileStrc,5,2)  && An additional colum holding the position 
                            && of each field has to be removed from the
                            && Array befor verification or building.
  
  ** If function called from add mode override the files. **
  ** If function called from edit mode check first. **
  IF loFormSet.llSavMod 
    ** Overwrite the file or create. **
    =lfCreatFil()
  ELSE
    IF !FILE(lcPathFile+".DBF")
      ** Create the file. **
      =lfCreatFil()
    ELSE
      IF AT(",",laMdlFiles[lnCount5,3]) = 0
        ** If not involved in any other module. overwrite it. **
        =lfCreatFil()
      ELSE
        ** If the file is involved in more than one modul **
        ** update its strucure not to overwrite it...
        =lfCoparStr()
      ENDIF
    ENDIF
  ENDIF
ENDFOR
=lfGetFC(lcMod2Ins,ALLTRIM(loFormSet.laData[11]),'A')

* if intalling ic module then copy file ICISTRU item code struc.
* to the company data dir. with the predefined code struct.
IF lcMod2Ins = "IC"
  lcItemFil = oAriaApplication.SysPath + "ICISTRU"
  * copy file icistru without checking if it is exist in the
  * system files directory so if it does not exit an error
  * occus and the module will not be installed
ENDIF

** If general ledger modul, copy this files **
** to the same path from sysfiles dir. :_
** Cash flow items.  "GLCFITEM"
** Ratio groups.     "GLRACOD"
** Source journals.  "GLSUBJOR"
IF lcMod2Ins = "GL"
  lcItemFil = oAriaApplication.SysPath + "GLCFITEM"
  IF FILE(lcItemFil+".DBF") AND FILE(lcItemFil+".CDX") AND FILE(lcItemFil+".FPT")
    ** Copy Cash flow items. **
    COPY FILE (lcItemFil+".DBF") TO (ALLTRIM(loFormSet.laData[11])+"GLCFITEM.DBF")
    COPY FILE (lcItemFil+".CDX") TO (ALLTRIM(loFormSet.laData[11])+"GLCFITEM.CDX")
    COPY FILE (lcItemFil+".FPT") TO (ALLTRIM(loFormSet.laData[11])+"GLCFITEM.FPT")
  ENDIF
  lcRacFil = oAriaApplication.SysPath + "GLRACOD"
  IF FILE(lcRacFil+".DBF") AND FILE(lcRacFil+".CDX") AND FILE(lcRacFil+".FPT")
    ** Copy Ratio groups. **
    COPY FILE (lcRacFil+".DBF") TO (ALLTRIM(loFormSet.laData[11])+"GLRACOD.DBF")
    COPY FILE (lcRacFil+".CDX") TO (ALLTRIM(loFormSet.laData[11])+"GLRACOD.CDX")
    COPY FILE (lcRacFil+".FPT") TO (ALLTRIM(loFormSet.laData[11])+"GLRACOD.FPT")
  ENDIF
  lcSubFil = oAriaApplication.SysPath + "GLSUBJOR"  &&
  IF FILE(lcSubFil+".DBF") AND FILE(lcSubFil+".CDX") AND FILE(lcSubFil+".FPT")
    ** Copy Source journals. **
    COPY FILE (lcSubFil+".DBF") TO (ALLTRIM(loFormSet.laData[11])+"GLSUBJOR.DBF")
    COPY FILE (lcSubFil+".CDX") TO (ALLTRIM(loFormSet.laData[11])+"GLSUBJOR.CDX")
    COPY FILE (lcSubFil+".FPT") TO (ALLTRIM(loFormSet.laData[11])+"GLSUBJOR.FPT")
  ENDIF
ENDIF

IF !EMPTY(laFil2Upd[1])
  lcRunScx = lfGetScx(oAriaApplication.ActiveModuleID + "\SMDSPFL.scx")
  DO FORM (lcRunScx) WITH loFormSet,.F.
ENDIF

*!*******************************************************************
*!
*!      Function: lfVer_Upd
*!      Function to verify or update data and system files
*!*******************************************************************

FUNCTION lfVer_Upd
PARAMETERS lcFileNam,llUpdate

DECLARE laFileStrc[1,1],laFileAStr[1,1]

llUpdate   = IIF(TYPE('llUpdate')='U',.F.,llUpdate)
lcTmpFNm   = ''
llFileCorr = .F.               && Flage If file Corrupted
llRetFlag  = .T.

*** Dictionary file strc.
*E301077,78 Hesham (Start)
gcSysHome = oAriaApplication.SysPath
llflfld = gfOpenFile(gcSysHome+'sydflfld','','SH')
llfield = gfOpenFile(gcSysHome+'sydfield','','SH')
*E301077,78 Hesham (End)
SET ENGINEBEHAVIOR 70
SELECT sydflfld.cfld_name,sydfield.cdata_typ,;
       sydfield.nfld_wdth,sydfield.nfld_dec,sydflfld.nfld_pos;
       FROM  (gcSyshome+"sydflfld"),(gcSyshome+"sydfield");
       ORDER BY sydflfld.nfld_pos;
       GROUP BY sydField.cFld_Name;
       WHERE UPPER(sydflfld.cfile_nam) =UPPER(PADR(lcFileNam,8));
       AND sydfield.cfld_name = sydflfld.cfld_name;
       INTO ARRAY laFileStrc
SET ENGINEBEHAVIOR 90

*E301077,78 Hesham (Start)
USE IN IIF(llflfld,'sydflfld',0)
USE IN IIF(llfield,'sydfield',0)
*E301077,78 Hesham (End)

*=gfAdel(@laFileStrc,5,2)       && An additional colum holding the position 
ADEL(laFileStrc,5,2)                               && of each field has to be removed from the
                               && Array befor verification or building.
*E301167,1 hesham (Start)
*E301167,1 copy array in another and then sort it
=ACOPY(laFileStrc,laFileFields)
*E301167,1 hesham end
DIME laFileFields[ALEN(laFileStrc,1),ALEN(laFileStrc,2)]

PRIVATE lnCount     
FOR lnCount = 1 TO ALEN(laFileStrc,1)
  laFileStrc[lnCount,1] = ALLT(laFileStrc[lnCount,1])
ENDFOR
*** Check if file exist or build new one
*B602147,1 Get full path for an opened DBF
=ASORT(laFileStrc)
llOpened  = USED(lcFileNam)
lcSetFull = SET('FULL')
SET FULL ON
lcDbfFile = IIF(llOpened,FULL(DBF(lcFileNam)),'')
SET FULL &lcSetFull
*B602147,1 end
IF FILE(lcTargDir+lcFileNam+".DBF")

  *** File was found in target directory
  IF USED(lcFileNam)
    llOpened = .T.
    SELECT(lcFileNam)
    *B602147,1 Open the file from the correct directory
    IF FULL(DBF(lcFileNam))<>lcTargDir+lcFileNam
      USE
      USE (lcTargDir+lcFileNam)       
    ENDIF
    *B602147,1 end
  ELSE  
    llOpened = .F.
    SELECT 0
    USE (lcTargDir+lcFileNam) 
  ENDIF

  *** Actual file structure
  =AFIELDS(laFileAStr)
  IF !EMPTY(laFileStrc[1])            && Found data for this file
    *E301167,1 hesham (Start)
    *E301167,1 check if the struc. of file in dic. is diff. that the phiscal file
    *E301167,1 structure
    WAIT 'Verifying file structure for file '+lcTargDir+lcFileNam Window nowait

    llFileCorr = ALEN(laFileStrc,1)<>ALEN(laFileAStr,1)
    *** Check all fields information
    IF !llFileCorr
        *E301167,1 Hesham (Start)
        *E301167,1 sorting phisc. fields in file
        =ASORT(laFileAStr)
        *E301167,1 Hesham (End)
        FOR lnFieldNo = 1 TO ALEN(laFileStrc,1)     &&All Fields
          lnFieldPos = ASCAN(laFileAStr,laFileStrc[lnFieldNo,1])
          FOR lnFieldPos  = 1 TO ALEN(laFileAStr,1)
            IF ALLTRIM(laFileAStr[lnFieldPos,1]) == ALLTRIM(laFileStrc[lnFieldNo,1])
              EXIT
            ENDIF
          ENDFOR
          IF lnFieldPos > ALEN(laFileAStr,1)
            lnFieldPos = 0
          ENDIF
          IF lnFieldPos = 0
             llFileCorr = .T.
             EXIT
          ELSE   
            IF laFileAStr[lnFieldPos,1] <> laFileStrc[lnFieldNo,1] OR ;
               laFileAStr[lnFieldPos,2] <> laFileStrc[lnFieldNo,2] OR ;
               laFileAStr[lnFieldPos,3] <> laFileStrc[lnFieldNo,3] OR ;
               laFileAStr[lnFieldPos,4] <> laFileStrc[lnFieldNo,4]
               llFileCorr = .T.
               EXIT
            ENDIF   
         ENDIF 
        ENDFOR       
    ENDIF
    *E301167,1 hesham (End)
    IF llFileCorr AND llUpdate
      *E301167,1 hesham (Start) 
      *E301167,1 restore the array to retrieve the fields position
      =ACOPY(laFileFields,laFileStrc)
      *E301167,1 hesham (End)
      IF !llYes2All
        lnOption = 3 &&gfModalGen('QRM00004B00004','Dialog',lcSubsVar)  
        DO CASE
          CASE lnOption = 1
            *** Fix file structure
            =lfUpdate(lcFileNam)
          CASE lnOption = 3
            llYes2All  = .T.
            =lfUpdate(lcFileNam)
          CASE lnOption = 4
            llRetFlag  = .F.
        ENDCASE
      ELSE
        *** Fix file structure
        =lfUpdate(lcFileNam)
      ENDIF  

    ENDIF
  ENDIF    
  IF !llOpened 
     USE
  ENDIF  
ELSE
  *E301167,1 hesham (Start) 
  *E301167,1 restore the array to retrieve the fields position
  =ACOPY(laFileFields,laFileStrc)
  *E301167,1 hesham (End)
 llYes2All  = .T.
 =lfUpdate(lcFileNam,.T.)
ENDIF
*B602147,1 Close DBF
IF USED(lcFileNam)
  USE IN (lcFileNam)
ENDIF  
*B602147,1 end
=lfIndex(lcFileNam) 
*B602147,1 Reopen the file if it was opened before
IF llOpened AND !EMPTY(lcDbfFile)
  USE (lcDbfFile) IN 0
ENDIF
*B602147,1 end
RETURN llRetFlag

*!*******************************************************************
*!
*!      Function: lfUpdate
*!
*!*******************************************************************
*
* This function will build a new data files from the dectionary 
* and their indexes
*
FUNCTION lfUpdate
PARAMETERS lcFileNam,llBldFile

*E301167,1 Hesham (Start)
WAIT 'Updating file structure for file '+lcTargDir+lcFileNam Window nowait
*E301167,1 Hesham (End)

llBldFile = IIF(TYPE('llBldFile')='U',.F.,llBldFile)
lcTempNam = gfTempName()

IF !EMPTY(laFileStrc[1])
  IF llBldFile 
    CREATE DBF  (lcTargDir+lcFileNam) FROM ARRAY laFileStrc
    *B602147,1 Close DBF after creation
    USE
    *B602147,1 end
  ELSE
    CREATE DBF  (lcTargDir+lcTempNam) FROM ARRAY laFileStrc
    APPEND FROM (lcTargDir+lcFileNam)
    USE

    IF USED(lcFileNam)
      USE IN (lcFileNam)
    ENDIF  

    ERASE  (lcTargDir+lcFileNam+".DBF")
    ERASE  (lcTargDir+lcFileNam+".FPT")
    ERASE  (lcTargDir+lcFileNam+".CDX")
            
    RENAME (lcTargDir+lcTempNam+".DBF") TO (lcTargDir+lcFileNam+".DBF")

    IF FILE(lcTargDir+lcTempNam+'.FPT')	
      RENAME (lcTargDir+lcTempNam+".FPT") TO (lcTargDir+lcFileNam+".FPT")
    ENDIF 

    SELECT 0
    USE (lcTargDir+lcFileNam)
  ENDIF

ENDIF  

*!*******************************************************************
*!
*!      Function: lfIndex
*!
*!*******************************************************************
* This function will creat or fix the index for one file
FUNCTION lfIndex
PARAMETERS lcFileNam

DECLARE laFileCDX[1,4]

*E301167,1 Hesham (Start)
WAIT 'Reindexing file '+lcTargDir+lcFileNam Window nowait
*E301167,1 Hesham (End)

lcSavAlias = SELECT(0)
*B602147,1 Clear array
laFileCDX  = ''
*B602147,1 end
*E301077,78 Hesham (Start)
llIndex = gfOpenFile(gcSysHome+'sydindex','','SH')
*E301077,78 Hesham (End)
SELECT ALLTRIM(sydindex.cindx_exp),ALLTRIM(sydindex.cfile_tag),sydindex.lascend,;
       sydindex.lunique;
       FROM (gcSyshome+"sydindex");
       WHERE UPPER(sydindex.cfile_nam) = PADR(lcFileNam,8);
       INTO ARRAY laFileCDX

*E301077,78 Hesham (Start)
USE IN IIF(llIndex,'sydindex',0)
*E301077,78 Hesham (End)

IF !USED(lcFileNam)
  SELECT 0
ELSE
  SELECT (lcFileNam)
ENDIF  
IF FILE(ALLTRIM(lcTargDir)+ALLTRIM(lcFileNam)+'.DBF')
  USE (ALLTRIM(lcTargDir)+ALLTRIM(lcFileNam)) EXCL

  IF !EMPTY(laFileCDX[1])
    *B603497,1 KHM 03/03/2000 (Begin) Adding the following code in order
    *B603497,1                to remove the index that does not exist 
    *B603497,1                in the SydIndex file.
    lnTagCnt = 1
    DO WHILE !EMPTY(TAG(lnTagCnt))
      llDelIndx = .T.
      FOR lnArryCnt = 1 TO ALEN(laFileCDX,1)
        IF UPPER(ALLTRIM(laFileCDX[lnArryCnt,2])) = UPPER(ALLTRIM(TAG(lnTagCnt)))
          llDelIndx = .F.
          EXIT
        ENDIF        
      ENDFOR
      IF llDelIndx
        lcTagNam = TAG(lnTagCnt)
        DELETE TAG &lcTagNam OF ALLTRIM(lcTargDir)+ALLTRIM(lcFileNam)+'.CDX'
      ELSE 
        lnTagCnt = lnTagCnt + 1
      ENDIF 
    ENDDO
    *B603497,1 KHM 03/03/2000 (End)
    
    FOR lnTagNo = 1 TO ALEN(laFileCDX,1)
        IF !EMPTY(laFileCDX[lnTagNo,1])
          lcAscend  =IIF(laFileCDX[lnTagNo,3],'ASCENDING','DESCENDING')
          lcUnique  =IIF(laFileCDX[lnTagNo,4],'UNIQUE','')
          INDEX ON &laFileCDX[lnTagNo,1] TAG &laFileCDX[lnTagNo,2]; 
                ADDITIVE &lcAscend &lcUnique
        ENDIF      
      ENDFOR            
  ENDIF
  USE
ENDIF  

SELECT (lcSavAlias)



************************************************************
*! Name      : gfThermo
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/09/2013
*! Purpose   : show the progress bar
************************************************************
FUNCTION gfThermo
PARAMETERS lnTotRecs, lnThermRec,lcFirstLabel,lcSndLable
lcFirstLabel  = IIF(EMPTY(lcFirstLabel ), ' ' , lcFirstLabel )
lcSndLable    = IIF(EMPTY(lcSndLable   ), ' ' , lcSndLable   )
IF lnTotRecs <= lnThermRec
  oProgress = NULL
  RELEASE oProgress
  RETURN 
ENDIF 

IF ISNULL(oProgress)
  oProgress = NEWOBJECT('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
ENDIF   

WITH oProgress
  .TotalProgress = lnTotRecs
  .lblFirstLabel.CAPTION = lcFirstLabel
  .lblSecondLabel.CAPTION = lcSndLable
  .CurrentProgress(lnThermRec)
  .SHOW()
ENDWITH   

*- End of gfThermo.

*!**************************************************************************
*!
*!      Function: lfCreatFil
*!
*!**************************************************************************
*
FUNCTION lfCreatFil


SELECT SYDINDEX
DECLARE laFileCDX[1,4]
laFileCDX = " "
** Select the file index in Array **
SELECT sydindex.cindx_exp,sydindex.cfile_tag, ;
       sydindex.lascend,sydindex.lunique ;
       FROM (oAriaApplication.SysPath+"sydindex") ;
       WHERE UPPER(sydindex.cfile_nam) = PADR(lcFileNam,8) ;
       INTO ARRAY laFileCDX
CREATE DBF (lcPathFile) FROM ARRAY laFileStrc

IF !EMPTY(laFileCDX[1])
  FOR lnTagNo = 1 TO ALEN(laFileCDX,1)
    lcAscend  =IIF(laFileCDX[lnTagNo,3],'ASCENDING','DESCENDING')
    lcUnique  =IIF(laFileCDX[lnTagNo,4],'UNIQUE','')
    INDEX ON &laFileCDX[lnTagNo,1] TAG &laFileCDX[lnTagNo,2]; 
             ADDITIVE &lcAscend &lcUnique
  ENDFOR            
ENDIF
USE

*!**************************************************************************
*!
*!      Function: lfCoparStr
*!
*!**************************************************************************
*
FUNCTION lfCoparStr

llOpen = .F.
IF !USED(lcFileNam)
  SELECT 0
  USE &lcPathFile
  llOpen = .T.
ELSE
  SELECT (lcFileNam)
ENDIF
** Actual file structure. **
DECLARE laActStrc[1,4]
laActStrc = " "
=AFIELDS('laActStrc')
  
** If opened just now, close this file. **
IF llOpen
  SELECT (lcFileNam)
  USE
ENDIF

** Compare the actual strucure & the dictionary strucure. **
FOR lnCont9 = 1 TO ALEN(laFileStrc,1)
  IF TYPE('laActStrc[lnCont9,1]') <> 'U'
    IF laFileStrc[lnCont9,1] <> laActStrc[lnCont9,1]
      IF !EMPTY(laFil2Upd[1])
        DECLARE laFil2Upd[ALEN(laFil2Upd,1)+1]
      ENDIF
      laFil2Upd[ALEN(laFil2Upd,1)] = lcFileNam + "  " + ;
                                     laMdlFiles[lnCount5,2]
      EXIT
    ENDIF
  ELSE
    IF !EMPTY(laFil2Upd[1])
      DECLARE laFil2Upd[ALEN(laFil2Upd,1)+1]
    ENDIF
    laFil2Upd[ALEN(laFil2Upd,1)] = lcFileNam + "  " + ;
                                   laMdlFiles[lnCount5,2]
    EXIT
  ENDIF
ENDFOR

*!**************************************************************************
*!
*!      Function: lfSetModul
*!
*!**************************************************************************
** Setup function called from :_
**       Edit mode : _ lfvSet   { valid function of the push button 
**                              < Setup > } in SMSELMD screen...
*
FUNCTION lfSetModul
PARAMETERS lcMod2Set,llGlobalSet

*E303339,1 TMI 01/03/2013 [Start] 
gcDataDir = oAriaApplication.DataDir
*E303339,1 TMI 01/03/2013 [End  ] 

lcOldActCm = oAriaApplication.ActiveCompanyID
lcOldDataD = gcDataDir
lcOldActMd = oAriaApplication.ActiveModuleID

gcAct_Appl = lcMod2Set
oAriaApplication.ActiveCompanyID = loFormSet.laData[1]

gcDataDir  = gfGetDataDir(ALLTRIM(loFormSet.laData[11]))

SELECT SYCCOMP
lnRecCmp = RECNO()

IF EMPTY(loFormSet.laData[14])
  lcInsMdl  = loFormSet.laData[15]
ELSE
  IF SEEK(loFormSet.laData[14],"SYCCOMP")
    lcInsMdl  = syccomp.mcomp_mdl
  ELSE
    lcInsMdl  = loFormSet.laData[15]
  ENDIF
ENDIF

*SELECT SYDOBJCT
*SET ORDER TO TAG CAPP_ID

** Check if the program of module setup is **
** exist in the module objects or not...
*llGlobalSet = !SEEK(lcMod2Set+lcMod2Set+'SETUP','SYDOBJCT')
SELECT sycComp
* Save the current environment in the static file. **
*lcCurrWind = gcBaseWind
*=gfStatic()
*lcWindow   = UPPER(lcBaseWind)
glFirsTime = .T.
llGlobalSet = .T.
*E303339,1 TMI 01/03/2013 [Start] call the option grid instead
*!*	IF !llGlobalSet
*!*	  gcBaseWind ='AWD' + lcMod2Set + 'SETUP'
*!*	      
*!*	  ** Call the module setup program  to add **
*!*	  ** the account code struc. for the added **
*!*	  ** comapny...
*!*	  lcDataDir = ALLTRIM(loFormSet.laData[11])

*!*	  lcDataDir = gfGetDataDir(ALLTRIM(loFormSet.laData[11]))

*!*	  lcAppName  = gcAppHome +lcMod2Set + '.APP'  &&
*!*	  DO (lcAppName) WITH '&lcMod2Set.SETUP',;
*!*	                             '.T.,"&loFormSet.laData[1]","&lcDataDir","&lcInsMdl"'                             
*!*	                     
*!*	ELSE
*!*	  lcAppName  = gcAppHome +lcMod2Set + '.APP'
*!*	  * IF the module application file does not exist
*!*	  * then run the SM.app
*!*	  IF !FILE(lcAppName)
*!*	    DO SYCMSET WITH lcMod2Set,gcdataDir,loFormSet.laData[1]
*!*	  ELSE
*!*	    DO (lcAppName) WITH 'SYCMSET WITH "&lcMod2Set","&gcdataDir","&loFormSet.laData[1]"'                            
*!*	  ENDIF
*!*	ENDIF                       

lcExpr = lfShowGrid(lcMod2Set)

*E303339,1 TMI 01/03/2013 [End  ] 
            
*!*	lcSavExact = SET('EXACT')
*!*	SET EXACT ON

*E303339,1 TMI 01/03/2013 [Start] update the SETUPS file with the lines of the selected module

*E303339,1 TMI 01/03/2013 [End  ] 

*!*	IF SEEK ('WIN'+lcWindow+UPPER(gcUser_Id)+gcStation,'syustatc')   
*!*	  ** Restore data for the company program from static file. **
*!*	  RESTORE FROM MEMO syustatc.mObj_Data  ADDITIVE
*!*	ENDIF  
*SET EXACT &lcSavExact
*gcBaseWind = lcCurrWind

*E303339,1 TMI 01/03/2013 [Start] 
IF USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF 
*E303339,1 TMI 01/03/2013 [End  ] 
=gfOpenFile(oAriaApplication.SysPath+"SYCCOMP",'CCOMP_ID')         

glQuitting = .F.

SELECT sycComp
IF lnRecCmp > 0 .AND. lnRecCmp<= RECCOUNT()
  GO lnRecCmp
ENDIF
llModSetUse = .T.
IF !llGlobalSet
  llModSetUse = USED(lcMod2Set+'SETUP')
  IF !llModSetUse
    USE (gcDataDir+lcMod2Set+'SETUP') IN 0
  ENDIF  
  SELECT (lcMod2Set+'SETUP')
  llModuleIns = lSetDon
ELSE
  lcTmpSetAl = gfTempName()  
  SELECT 0
  USE (gcDataDir+'SETUPS') AGAIN ALIAS &lcTmpSetAl
  SET ORDER TO TAG NVARPOS
  llModuleIns = SEEK(lcMod2Set)
  USE IN (lcTmpSetAl)
ENDIF  
SELECT SYCCOMP

*- check if the setuped module in the module list of the company and its history company
IF !(lcMod2Set $ mModlSet)
  REPLACE mModlSet  WITH mModlSet+IIF(llModuleIns,IIF(EMPTY(mModlSet),'',',')+lcMod2Set,'')
ENDIF  
IF !GfGetMemVar('LLHIST',loFormSet.laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',loFormSet.laData[1]))
  =SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])))
  IF !(lcMod2Set $ mModlSet)
    REPLACE mModlSet  WITH mModlSet+IIF(llModuleIns,IIF(EMPTY(mModlSet),'',',')+lcMod2Set,'')
  ENDIF  
  =SEEK(ALLTRIM(loFormSet.laData[1]))
ENDIF 	

IF loFormSet.laData[1] = oAriaApplication.ActiveCompanyID
  *gcCmpModules = ALLTRIM(mModlSet)
  oAriaApplication.companysetupmodules = ALLTRIM(mModlSet)
ENDIF  
IF !llModSetUse
  USE IN (lcMod2Set+'SETUP')
ENDIF  
gcAct_Appl = lcOldActMd
oAriaApplication.ActiveCompanyID = lcOldActCm
gcDataDir  = lcOldDataD

************************************************************
*! Name      : lfShowGrid
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/03/2013
*! Purpose   : Show option grid
************************************************************
FUNCTION lfShowGrid
PARAMETERS lcModId
LOCAL lnPos

*- special modueles that does not have a setup option using option grid 
IF lcModId $ 'AP|GL'
  DO CASE
  CASE lcModId = 'AP'
    lcRunScx = lfGetScx("AP\APSETUP.scx")
    PRIVATE oScr
    DO FORM (lcRunScx) NAME oScr NOSHOW 
    IF TYPE('oScr')='O' AND !ISNULL(oScr)
      oScr.Show(1)
    ENDIF
  
  CASE lcModId = 'GL'
    lcCompDir = gfGetDataDir(ALLTRIM(loFormSet.laData[11]))
    DO (oariaapplication.applicationhome+'GL\GLSETUP') WITH .T.,loFormSet.laData[1],lcCompDir,.F.
    
  ENDCASE
  
  RETURN 
ENDIF 

lnPos = ASCAN(loFormSet.laMod,lcModId+'-')
IF lnPos=0
  RETURN 
ENDIF   

lcModTitle = SUBSTR(loFormSet.laMod[lnPos],4)
lcRepID = 'SMAPP'+lcModId
*- Set the caption of the SYDREPRT file
USE (loFormSet.lcTempFldr+'SYDREPRT') IN 0
SELECT SYDREPRT
LOCATE
REPLACE CREP_ID   WITH lcRepID   ;
        CREP_NAME WITH lcModTitle
        
USE IN SYDREPRT

*- get the related syrepuvr lines
*E303339,1 TMI 01/06/2013 [Start] 
*!*	SELECT * FROM (oAriaapplication.caria4syspath+'SYREPUVR') ;
*!*	  WHERE crep_id = lcRepID ;
*!*	  INTO TABLE (oAriaapplication.WorkDir+loFormSet.SYREPUVR)
*!*	USE IN (loFormSet.SYREPUVR)
*!*	IF USED('SYREPUVR')
*!*	  USE IN SYREPUVR
*!*	ENDIF 
*!*	USE (oAriaapplication.WorkDir+loFormSet.SYREPUVR) IN 0 ALIAS SYREPUVR
*E303339,1 TMI 01/06/2013 [End  ] 

USE (loFormSet.lcTempFldr+'SYREPUVR') IN 0
SELECT SYREPUVR
LOCATE FOR MFLD_NAME = 'lcOgPrvRun'
REPLACE CREP_ID WITH lcRepID

*- only one setups file open at a time
lcDataDir  = UPPER(ALLTRIM(loFormSet.laData[11]))

*- check if this SETUPS file is the required to update
IF USED('SETUPS')
  IF !lcDataDir $ DBF('SETUPS')
    USE IN SETUPS
  ENDIF 
ENDIF 
IF !USED('SETUPS')
  =gfOpenFile(lcDataDir+'SETUPS','NVARPOS','SH')  &&CAPP_ID+STR(NVARPOS)
ENDIF

*- Get the values of the setups file, if the option is not set in the company yet then the SYREPUVR default will be used
SELECT SYREPUVR 
LOCATE
lcSeekID = IIF(lcModId='SM',CHR(255)+CHR(255),lcModId)
SCAN FOR crep_id = 'SMAPP'+lcModId
  IF SEEK(lcSeekID+STR(NVARPOS),'SETUPS')
    REPLACE mdata_def WITH SETUPS.mdata_def
  ENDIF 
ENDSCAN
USE IN SYREPUVR

*E303339,1 TMI 01/06/2013 [Start] 
*!*	lcSafe = SET("Safety")
*!*	SET SAFETY OFF

*!*	SELECT SYREPUVR 
*!*	LOCATE 
*!*	COPY TO (loFormSet.lcTempFldr+'SYREPUVR')
*E303339,1 TMI 01/06/2013 [End  ] 

*E303339,1 TMI 01/14/2013 [Start] set the SETUPS to the buffer mode 5
SELECT SETUPS
CURSORSETPROP("Buffering",5)
*E303339,1 TMI 01/14/2013 [End  ] 

*- Change the A4 syspath
=lfChangeSyspath()
lcTempFldr = loFormSet.lcTempFldr
lnCurrDataSession = SET("DATASESSION")
PRIVATE lcCompanyID,lcCompanyName
lcCompanyID = oAriaApplication.ActiveCompanyId
lcCompanyName = oAriaApplication.ActiveCompanyName

oAriaApplication.ActiveCompanyId = SYCCOMP.CCOMP_ID
oAriaApplication.ActiveCompanyName = ALLTRIM(SYCCOMP.CCOM_NAME)

lcExpr = gfOpGrid(lcRepID,.T.)

*E303339,1 TMI 01/14/2013 [Start] move this to lfRepWhen
*!*	oAriaApplication.ActiveCompanyId = lcCompanyID 
*!*	oAriaApplication.ActiveCompanyName = lcCompanyName 
*E303339,1 TMI 01/14/2013 [End  ] 

*- Restore the A4syspath
oAriaApplication.cAria4Sysfiles = loFormSet.cOriginal_cAria4Sysfiles

*- if in Add/Edit mode and the user clicked ok, update the SETUPS file from the SYREPUVR 
*!*	
*!*	IF lcExpr = '.T.' AND loFormSet.ActiveMode $ 'AE'
*!*	  USE IN 0 (loFormSet.lcTempFldr+'SYREPUVR')
*!*	  SELECT SYREPUVR
*!*	  SCAN
*!*	    SCATTER MEMVAR MEMO
*!*	    IF SEEK(lcModId+STR(NVARPOS),'SETUPS')
*!*	      SELECT SETUPS
*!*	      REPLACE mdata_def WITH SYREPUVR.mdata_def
*!*	    ELSE 
*!*	      SELECT SETUPS
*!*	      APPEND BLANK 
*!*	      GATHER MEMVAR MEMO
*!*	    ENDIF 
*!*	  ENDSCAN 
*!*	  USE IN SYREPUVR
*!*	ENDIF 

RETURN lcExpr
*- End of lfShowGrid.
************************************************************
*! Name      : lfChangeSyspath
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/06/2013
*! Purpose   : Get the syspath of the temp folder , open repuvr from there 
************************************************************
FUNCTION lfChangeSyspath
lnBeforePos = AT('SOURCEDB',UPPER(oAriaApplication.cAria4Sysfiles))
lcBefore = SUBSTR(oAriaApplication.cAria4Sysfiles,1,lnBeforePos-1)
oAriaApplication.cAria4Sysfiles = SUBSTR(oAriaApplication.cAria4Sysfiles,lnBeforePos)
lnAfterPos  = AT(';',oAriaApplication.cAria4Sysfiles)
lcAfter = SUBSTR(oAriaApplication.cAria4Sysfiles,lnAfterPos)
oAriaApplication.cAria4Sysfiles = lcBefore +'SourceDB='+loFormSet.lcTempFldr+lcAfter
*- End of lfChangeSyspath.

************************************************************
*! Name      : lfGetScx
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/02/2012
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

************************************************************
*! Name      : SYCMSET
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/02/2013
*! Purpose   : the setup option grid screen
************************************************************
FUNCTION lfvSYCMSET
PARAMETERS loFormSet

lcExpr = lfShowGrid('SM')

*- End of SYCMSET.



*!**************************************************************************
*!
*!      Function: lfUnInstal
*!
*!**************************************************************************
** Setup function called from :_
**       Edit mode : _ lfvSet   { valid function of the push button 
**                              < Setup > } in SMSELMD screen...
** Using parameteres................:_
**       Module ID.
FUNCTION lfUnInstal
PARAMETERS lcMod2UIns

SELECT SYCCOMP
loFormSet.laData[15] = "|" + loFormSet.laData[15]
loFormSet.laData[15] = STRTRAN(loFormSet.laData[15],"|"+lcMod2UIns,"")
loFormSet.laData[15] = SUBSTR(loFormSet.laData[15],2)
*In case of no active company selected, gcDataDir is empty generating the bug.
*E303339,1 TMI 01/03/2013 [Start] 
gcDataDir = oAriaApplication.DataDir
*E303339,1 TMI 01/03/2013 [End  ] 
lcOldDataD = gcDataDir
gcDataDir  = gfGetDataDir(ALLTRIM(loFormSet.laData[11]))

REPLACE mComp_mdl WITH loFormSet.laData[15] 
IF !GfGetMemVar('LLHIST',loFormSet.laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',loFormSet.laData[1]))
  =SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])))
  REPLACE mComp_mdl WITH loFormSet.laData[15]
  =SEEK(ALLTRIM(loFormSet.laData[1]))
ENDIF 	
*E303339,1 TMI 01/10/2013 [Start] 

IF USED('SYDOBJCT')
  USE IN SYDOBJCT
ENDIF 
=gfOpenFile(oariaapplication.caria4syspath+'SYDOBJCT','CAPP_ID','SH')   && CAPP_ID+CAPOBJNAM
*E303339,1 TMI 01/10/2013 [End  ] 
SELECT SYDOBJCT
SET ORDER TO TAG CAPP_ID
llGlobalSet = !SEEK(lcMod2UIns+lcMod2UIns+'SETUP','SYDOBJCT')
IF llGlobalSet 
  lnLoop = IIF(!GfGetMemVar('LLHIST',loFormSet.laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',loFormSet.laData[1]));
               ,2,1)
  FOR lnI = 1 TO lnLoop
    IF lnI = 2
      lcOldDir = gcDataDir
      SELECT SYCCOMP
      =SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])))
      gcDataDir = ALLTRIM(SYCCOMP.ccom_ddir)
      SELECT SYDOBJCT
    ENDIF
    lcTmpSetAl = gfTempName()  
    SELECT 0
    USE (gcDataDir+'SETUPS') AGAIN ALIAS &lcTmpSetAl
    SET ORDER TO TAG NVARPOS
    IF SEEK(lcMod2UIns)
      DELETE WHILE CAPP_ID+STR(NVARPOS) = lcMod2UIns
    ENDIF
    USE IN (lcTmpSetAl)
    IF lnI = 2
      gcDataDir = lcOldDir
    ENDIF
  ENDFOR
ENDIF
SELECT SYCCOMP
IF lcMod2UIns $ mModlSet
  REPLACE mModlSet  WITH STRTRAN(mModlSet,','+lcMod2UIns)
  REPLACE mModlSet  WITH STRTRAN(mModlSet,lcMod2UIns+',')
  REPLACE mModlSet  WITH STRTRAN(mModlSet,lcMod2UIns)
ENDIF  
IF !GfGetMemVar('LLHIST',loFormSet.laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',loFormSet.laData[1]))
  =SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])))
  IF lcMod2UIns $ mModlSet
    REPLACE mModlSet  WITH STRTRAN(mModlSet,','+lcMod2UIns)
    REPLACE mModlSet  WITH STRTRAN(mModlSet,lcMod2UIns+',')
    REPLACE mModlSet  WITH STRTRAN(mModlSet,lcMod2UIns)
  ENDIF  
  =SEEK(ALLTRIM(loFormSet.laData[1]))
ENDIF 	
loFormSet.lcOldMod = "|" + loFormSet.lcOldMod
loFormSet.lcOldMod = STRTRAN(loFormSet.lcOldMod,"|"+lcMod2UIns,"")
loFormSet.lcOldMod = SUBSTR(loFormSet.lcOldMod,2)

loFormSet.laAllMod[loBrnFormSet.lsModules] = SUBSTR(loFormSet.laAllMod[loBrnFormSet.lsModules],1,37)+;
                         "             "+RIGHT(loFormSet.laAllMod[loBrnFormSet.lsModules],1)

** Do you want to delete files for module ð. **
** <  Delete  > - <  Cancel  > **
IF gfModalGen("QRM00217B00002","ALERT",lcMod2UIns) = 1
  =lfDel_Mdl(lcMod2UIns)
ENDIF
=lfGetFC(lcMod2UIns,ALLTRIM(loFormSet.laData[11]),'D')
gcDataDir  = lcOldDataD
*SHOW GET lsModules

*!**************************************************************************
*!
*!      Function: lfDel_Fil
*!
*!**************************************************************************
*
FUNCTION lfDel_Fil

** Get the files to be deleted. **
SELECT SYDFILES
SET ORDER TO TAG CFILE_NAM

lnRecords = 0

** Calculat No. of files to be deleted. **

COUNT FOR (LEFT(CFILE_NAM,2) <> "SY" .AND. ;
          !EMPTY(MFILE_APP) .AND. ;
          !(MFILE_APP $ "SM,SY,SM$,SY$,SM$,SY,SM,SY$,SM"));
          .OR. ('SY' $ MFILE_APP .AND. LEFT(CFILE_NAM,2) <> 'SY');
          TO lnRecords

IF lnRecords = 0
  ** There is no files to be deleted. **
  ** <  Ok  > **
  =gfModalGen("TRM00197B00000","DIALOG")
  SELECT sycComp
  RETURN
ENDIF

** Trap any error apear in the next loop. **
lcOnError  = ON('ERROR')
ON ERROR DO lpErrTrap WITH ERROR()

lcSavDefa = SET("DEFAULT")
lnCRec    = 1

** Loop to delete company files

oProgress = NULL
SCAN FOR (LEFT(CFILE_NAM,2) <> "SY" .AND. !EMPTY(MFILE_APP) .AND.;
         !(MFILE_APP $ "SM,SY,SM$,SY$,SM$,SY,SM,SY$,SM")); 
         .OR. ('SY' $ MFILE_APP .AND. LEFT(CFILE_NAM,2) <> 'SY')


  lcFilNam  = ALLTRIM(sydFiles.cfile_nam)         && Without ext
  lcFilName = ALLTRIM(sydFiles.cfile_nam)+".DBF"  && With ext
  
  lcDelPath = ALLTRIM(loFormSet.laData[11])
  lcPathFile= lcDelPath+lcFilName          && With ext and path
  
  =gfThermo(lnRecords,lnCRec,"Deleting Company files:",lcPathFile)
  
  IF FILE(lcPathFile) 
    SELECT 0 
    USE (lcPathFile) EXCLUSIVE 
    USE
  
    IF loFormSet.llError
      ** Error Deleting Do you want to continue ? **
      ** < Yes > -<  No  > **
      IF gfModalGen("QRM00034B00006","ALERT",+" "+lcPathFile) = 1
        loFormSet.llError=.F.
      ELSE
        EXIT
      ENDIF   
    ELSE
      ** No one is using this file so delete it
      ERASE (lcDelPath+lcFilNam+".DBF")
      ERASE (lcDelPath+lcFilNam+".CDX")
      ERASE (lcDelPath+lcFilNam+".FPT")
    ENDIF 
  ENDIF
  lnCRec = lnCRec + 1
  SELECT SYDFILES
ENDSCAN  

SET DEFAULT TO &lcSavDefa  
ON ERROR &lcOnError

SELECT sycComp

*!**************************************************************************
*!
*!      Function: lfDel_Mdl
*!
*!**************************************************************************
*
FUNCTION lfDel_Mdl
PARAMETERS lcModul

** Get the files to be deleted. **
SELECT SYDFILES
SET ORDER TO TAG CFILE_NAM

lnRecords = 0

** Calculat No. of files to be deleted. **
COUNT FOR LEFT(CFILE_NAM,2) <> "SY" .AND. ;
          !EMPTY(MFILE_APP) .AND. ;
          (lcModul $ MFILE_APP) .AND. ;
          !("," $ MFILE_APP) ;
          TO lnRecords

IF lnRecords = 0
  ** There is no files to be deleted. **
  ** <  Ok  > **
  =gfModalGen("TRM00197B00000","DIALOG")
  SELECT sycComp
  RETURN
ENDIF

** Trap any error apear in the next loop. **
lcOnError  = ON('ERROR')
ON ERROR DO lpErrTrap WITH ERROR()

lcSavDefa = SET("DEFAULT")
lnCRec    = 1

** Loop to delete company files
oProgress = NULL
SCAN FOR LEFT(CFILE_NAM,2) <> "SY" .AND. !EMPTY(MFILE_APP) .AND.;
         (lcModul $ MFILE_APP) .AND. !("," $ MFILE_APP)

  lcFilNam  = ALLTRIM(sydFiles.cfile_nam)         && Without ext
  lcFilName = ALLTRIM(sydFiles.cfile_nam)+".DBF"  && With ext
  
  lcDelPath = ALLTRIM(loFormSet.laData[11])
  lcPathFile= lcDelPath+lcFilName          && With ext and path
  
  =gfThermo(lnRecords,lnCRec,"Deleting Module "+lcModul+" files:",lcPathFile)
  
  IF FILE(lcPathFile) 
    SELECT 0 
    USE (lcPathFile) EXCLUSIVE 
    USE
  
    IF loFormSet.llError
      ** Error Deleting Do you want to continue ? **
      ** < Yes > -<  No  > **
      IF gfModalGen("QRM00034B00006","ALERT",+" "+lcPathFile) = 1
        loFormSet.llError=.F.
      ELSE
        EXIT
      ENDIF   
    ELSE
      ** No one is using this file so delete it
      ERASE (lcDelPath+lcFilNam+".DBF")
      ERASE (lcDelPath+lcFilNam+".CDX")
      ERASE (lcDelPath+lcFilNam+".FPT")
    ENDIF 
  ENDIF
  lnCRec = lnCRec + 1
  SELECT SYDFILES
ENDSCAN  

SET DEFAULT TO &lcSavDefa  
ON ERROR &lcOnError

SELECT sycComp

*!**************************************************************************
*!
*!      Procedure : lpErrTrap
*!
*!**************************************************************************
*
function lpErrTrap
PARAMETERS ERRNO

IF ERRNO = 108 .OR. ERRNO = 3
  loFormSet.llError = .T.  
ENDIF

*!*	*!**************************************************************************
*!*	*!
*!*	*!      Function: lfvCurrency
*!*	*!
*!*	*!**************************************************************************
*!*	*
*!*	FUNCTION lfwCurrency

*!*	lcOldCurr=loFormSet.laData[17]

*!**************************************************************************
*!
*!      Function: lfvCurrency
*!
*!**************************************************************************
*
FUNCTION lfvCurrency
PARAMETERS loFormSet,loFld
LOCAL lnSlct
lnSlct = SELECT(0)

IF (!EMPTY(loFormSet.laData[17]) AND !SEEK(loFormSet.laData[17],'SYCCURR')) OR loFld.Selectedfrombrowse  
  PRIVATE lcBrFields,lcFile_ttl,lcSelect
  lcSelect = SELECT()
  SELECT SYCCURR
  lcBrFields=gfDbfField('SYCCURR')
  DIMENSION laTemp[1]
  STORE '' TO laTemp
  lcFile_ttl    = "Currency"
  =gfBrows(.F.,"cCurrCode","laTemp",lcFile_ttl)
  loFormSet.laData[17] = laTemp[1]
  SELECT (lcSelect)
  *SHOW GET loFormSet.laData[17]
ENDIF

IF EMPTY(loFormSet.laData[17])
  *E303339,1 TMI 01/10/2013 [Start] 
  *loFormSet.laData[17] = lcOldCurr
  loFormSet.laData[17] = loFld.KeyTextbox.OldValue
  *E303339,1 TMI 01/10/2013 [End  ] 
ENDIF

loFormset.Ariaform1.txtCurrency.Value = SYCCURR.CCURRDESC
loFormSet.Ariaform1.Refresh()
**=lfRefresh(WOUTPUT())

*!**************************************************************************
*!
*!      Function: lfGetCur
*!
*!**************************************************************************
*
FUNCTION lfGetCur

loFormSet.laData[17] = LOOKUP(SYCINT.cCurrCode,loFormSet.laData[16],sycint.ccont_code,'CCONTCODE')
SHOW GET loFormSet.laData[17]
**=lfRefresh(WOUTPUT())

*!**************************************************************************
*!
*!      Function: lfvData_18
*!
*!**************************************************************************
*
FUNCTION lfvData_18

IF loFormSet.laData[18]
  loFormSet.lcMultCurr = 'ENABLE'
  loFormSet.laData[19] = .T.
  loFormSet.laData[20] = .T.
  loFormSet.laData[21] = .F.
  loFormSet.laData[22] = 30
ELSE
  loFormSet.lcMultCurr = 'DISABLE'
  loFormSet.laData[19] = .F.
  loFormSet.laData[20] = .F.
  loFormSet.laData[21] = .F.
  loFormSet.laData[22] = 0
ENDIF
*=lfShowCurr()

*!*	*!**************************************************************************
*!*	*!
*!*	*!      Function: lfShowCurr
*!*	*!
*!*	*!**************************************************************************
*!*	*
*!*	FUNCTION lfShowCurr


*!**************************************************************************
*!
*!      Function: lfOpnFiles
*!
*!**************************************************************************
FUNCTION lfOpnFiles
PARAMETER lcFilToOpn, lcTagToOpn
IF FILE(lcDataDir + lcFilToOpn + '.DBF')
  IF USED(lcFilToOpn)
    USE IN (lcFilToOpn)
  ENDIF  
  SELECT 0
  USE (lcDataDir + lcFilToOpn) AGAIN ORDER TAG (lcTagToOpn) 
ENDIF

*!*************************************************************
*! Name      : lfGetLevel
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 09/06/1998
*! Purpose   : To get the module level in the modules tree
*!*************************************************************
*! Called from : SMCMINF.PRG
*!*************************************************************
*! Calls       : lfGetLevel()
*!*************************************************************
*! Passed Parameters : lcApp      Application Name
*!                     lnArrNum   Application number in the array
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetLevel()
*!*************************************************************
FUNCTION lfGetLevel
PARAMETERS lcApp,lnArrNum
PRIVATE lcApp,lnSeq,laChldSeq,lnCount,lnMax,i,lcMod,lnArrNo, lnModPs

RETURN 1

STORE 0 TO lnArrNo, lnCount
lcExact = SET('EXACT')
SET EXACT ON
lnModPs = ASUBSCRIPT(laReqMod,ASCAN(laReqMod,'*'+lcApp),1)
SET EXACT &lcExact
lcMod   = ALLTRIM(STRTRAN(laReqMod[lnModPs,2],'|'))

DO WHILE !EMPTY(lcMod)
  lnCount = lnCount + 1
  lcExact = SET('EXACT')
  SET EXACT ON
  lnArrNo = ASUBSCRIPT(laTempArr,ASCAN(laTempArr,LEFT(lcMod,2)),1)
  SET EXACT &lcExact
  
  DIMENSION laChldSeq[lnCount]
  laChldSeq[lnCount] = lfGetLevel(LEFT(lcMod,2),lnArrNo)
  lcMod=SUBSTR(lcMod,3)
ENDDO

IF lnCount = 0
  RETURN 1
ELSE
  lnMax = 0
  IF TYPE('laChldSeq')='N'
    FOR i = 1 TO ALEN(laChldSeq)  
      lnMax = MAX(lnMax,laChldSeq[i]) 
    ENDFOR
    RETURN lnMax + 1
  ENDIF  
ENDIF  

*!*************************************************************
*! Name      : lfGetFC
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 09/06/1998
*! Purpose   : Copy or remove the form codes from the DBFS form 
*!             code tables based on the form codes files in the 
*!             sysfiles directory.
*!*************************************************************
*! Called from : SMCMINF.PRG
*!*************************************************************
*! Calls       : lfGetLevel()
*!*************************************************************
*! Passed Parameters : lcApp      Application Name
*!                     lnArrNum   Application number in the array
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetFC("AR","W:\aria27\dbfs\",'A')
*!*************************************************************
FUNCTION lfGetFC
PARAMETERS lcMod2Ins,lcDataDr,lcAddDel
IF FILE(lcDataDr+'FORMCDHD.DBF') .AND. ;
      FILE(lcDataDr+'FORMCDHD.DBF') 
  lnAlias = SELECT(0)
  lcFCDHD = gfTempName()
  lcFCDDT = gfTempName()
  USE (lcDataDr+'FORMCDHD.DBF') AGAIN ALIAS (lcFCDHD) ORDER TAG Formcdhd IN 0
  USE (lcDataDr+'FORMCDDT.DBF') AGAIN ALIAS (lcFCDDT) ORDER TAG Formcddt IN 0

  llOpSYREP = gfOpenFile(oAriaApplication.SysPath+'SYDREPRT','CREP_ID','SH')
  llOpFDT   = gfOpenFile(oAriaApplication.SysPath+'SYFRMCDD','FORMCDDT','SH')
  llOpFHD   = gfOpenFile(oAriaApplication.SysPath+'SYFRMCDH','FORMCDHD','SH')

  IF lcAddDel = 'A'      
    SELECT SYFRMCDD
    SET RELATION TO SYFRMCDD.cRep_Id INTO SYDREPRT ADDITIVE      
    SELECT SYFRMCDH
    SET RELATION TO SYFRMCDH.cformmaj INTO SYFrmCdD ADDITIVE      
    SCAN 
      llRecIns = .F.
      SELECT SYFRMCDD
       SCAN WHILE cformmaj = SYFRMCDH.cformmaj FOR lcMod2Ins $ UPPER(SYDREPRT.mCallMods) .AND. SYDREPRT.cVer<>"A40"
          SCATTER MEMVAR MEMO
          IF !SEEK(m.cformmaj+m.cformcode,lcFCDDT)
            INSERT INTO (lcFCDDT) FROM MEMVAR
          ELSE
            SELECT (lcFCDDT) 
            GATHER MEMVAR MEMO   
          ENDIF
          SELECT SYFRMCDD
          llRecIns = .T.
        ENDSCAN
      IF llRecIns 
        SELECT SYFRMCDH
        SCATTER MEMVAR MEMO
        IF !SEEK(m.cformmaj,lcFCDHD)
          INSERT INTO (lcFCDHD) FROM MEMVAR
        ELSE
          SELECT (lcFCDHD)
          GATHER MEMVAR MEMO FIELDS EXCEPT cCurForm, mFormSets
        ENDIF
      ENDIF  
      SELECT SYFRMCDH
    ENDSCAN
  ELSE
    SELECT (lcFCDDT)
    SET RELATION TO cRep_ID INTO SYDREPRT ADDITIVE      
    SELECT (lcFCDHD)
    SET RELATION TO cformmaj INTO (lcFCDDT) ADDITIVE      
    SET SKIP TO (lcFCDDT),SYDREPRT
    
    SCAN FOR lcMod2Ins $ UPPER(SYDREPRT.mCallMods) .AND. SYDREPRT.cVer<>"A40" 
      llDelMod = .T.
      lcCalMod = STRTRAN(SYDREPRT.mCallMods,lcMod2Ins)
      lcCalMod = STRTRAN(lcCalMod,',')
      IF !EMPTY(lcCalMod)
        FOR lnI = 1 TO LEN(lcCalMod)/2
          IF SUBSTR(lcCalMod,1,2) $ loFormSet.lcOldMod
            llDelMod = .F.
            EXIT
          ELSE
            lcCalMod = SUBSTR(lcCalMod,3)
          ENDIF
        ENDFOR
      ENDIF
      IF llDelMod
        SELECT (lcFCDDT)
        DELETE
      ENDIF
    ENDSCAN
    DELETE FOR EOF(lcFCDDT)      
  ENDIF      
  IF llOpSYREP .AND. USED('SYDREPRT')
    USE IN SYDREPRT
  ENDIF
  USE IN (lcFcdHd)
  USE IN (lcFcdDt)
  IF llOpFHD .AND. USED('SYFRMCDH')
    USE IN SYFRMCDH
  ELSE 
    SELECT SYFRMCDH
    SET RELATION TO
  ENDIF
  IF llOpFDT .AND. USED('SYFRMCDD')
    USE IN SYFRMCDD
  ELSE
    SELECT SYFRMCDD
    SET RELATION TO
  ENDIF
  SELECT (lnAlias)
ENDIF

************************************************************
*! Name      : lfSortMod
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/02/2013
*! Purpose   :if sort the modules selected to be installed to make the communication module 
*             first module if selected
************************************************************
FUNCTION lfSortMod
PARAMETERS loFormSet

IF SEEK("CM","SYDAPPL")
  lccmProm = "  "+sydappl.cApp_Id+" "+sydappl.cApp_name+" +"+IIF(sydappl.lSetReq,"T","F")
  lncmPos = ASCAN(loFormSet.laAllMod,lccmProm)
  IF lncmPos >0
    =ADEL(loFormSet.laAllMod,lncmPos)
    =AINS(loFormSet.laAllMod,1)
    loFormSet.laAllMod[1] = lccmProm
  ENDIF
ENDIF

*- End of lfSortMod.

*!*	*!*************************************************************
*!*	*! Name      : lfvStates
*!*	*! Developer : NADER NABIL (NNA)
*!*	*! Date      : 09/27/2004
*!*	*! Purpose   : Validate USA/CANADA State
*!*	*!*************************************************************
*!*	*! Calls     : AriaBrow
*!*	*!*************************************************************
*!*	*! Passed Parameters  : Main Account State / Store State
*!*	*!*************************************************************
*!*	*! Returns            :  None
*!*	*!*************************************************************
*!*	*! Example            :  =lfvStates()
*!*	*!*************************************************************
*!*	FUNCTION lfvStates
*!*	PRIVATE lcFile_Ttl, lcBrfields, lcStaCode

*!*	lcStaCode = PROPER(LOOKUP(SYCINT.CPARt4LAB,IIF(EMPTY(loFormSet.laData[13]),ALLTRIM(gcContCode),ladata[13]),sycint.ccont_code,'CCONTCODE'))

*!*	*-- if you select country code for USA or for CANADA.
*!*	IF ALLTRIM(ladata[8])='USA' OR ALLTRIM(ladata[8])='CANADA'
*!*	  *-- Open codes file first time you go to this function.
*!*	  gcDataDir  = gfGetDataDir(ALLTRIM(sycComp.cCom_dDir))
*!*	  IF !USED('CODES')
*!*	    =gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')
*!*	  ENDIF
*!*	  *-- IF you did not found state code in codes file.
*!*	  IF !EMPTY(laData[6])  
*!*	    SET ORDER TO CODES IN CODES
*!*	    IF !SEEK('N'+PADR(laData[6],6)+'N'+'STATE','CODES')
*!*	      lnCurAlias = SELECT(0)
*!*	       *-- browse all country codes 
*!*	       SELECT CODES
*!*	       DECLARE laCodeFld[2]
*!*	       lcFile_Ttl = ALLTRIM(lcStaCode) + ' Codes'
*!*	       lcBrfields = 'cCode_No :H=ALLTRIM(lcStaCode)+" Code",cDiscrep:H="Description"'
*!*	      
*!*	       IF gfBrows(' "N" FOR cDefCode+cfld_name+ccode_no+cdiscrep = ;
*!*	         "NSTATE" AND cRltField="N"','cCode_No','laCodeFld')
*!*	         laData[6] = laCodeFld[1]
*!*	       ENDIF
*!*	       SELECT (lnCurAlias)
*!*	    ENDIF
*!*	  ENDIF    
*!*	ENDIF
*!*	RETURN

************************************************************
*! Name      : lfSetupsFormActivate
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : Activate the screen
************************************************************
FUNCTION lfSetupsFormActivate
PARAMETERS loFormSet

*- End of lfSetupsFormActivate.

************************************************************
*! Name      : lfSetupsFormdestroy
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : Destroy function
************************************************************
FUNCTION lfSetupsFormDestroy
PARAMETERS loFormSet

IF USED('ACCOD')
  USE IN ACCOD
ENDIF  
IF USED('FISHD')
  USE IN FISHD
ENDIF  

IF TYPE('loFormSet.lcTempSetup')='C'
  gcWorkDir = oAriaApplication.WorkDir
  lcTempSetup = loFormSet.lcTempSetup
  IF USED(lcTempSetup)
    USE IN (lcTempSetup)
  ENDIF

  ERASE (gcWorkDir+lcTempSetup+'.DBF')
  ERASE (gcWorkDir+lcTempSetup+'.FPT')  
  ERASE (gcWorkDir+lcTempSetup+'.CDX')  
ENDIF 

*- remove the created temp folder for the sydreprt and syrepuvr
*E303339,1 TMI 01/08/2013 [Start] this code does not work for some reason, needs to be rechecked
*!*	ERASE (loFormSet.lcTempFldr+'*.*')
*!*	RD (loFormSet.lcTempFldr)
*E303339,1 TMI 01/08/2013 [End  ] 
*- End of lfSetupsFormdestroy.

************************************************************
*! Name      : lfSMINSET_FormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/02/2013
*! Purpose   : init function for SMINSET screen
************************************************************
FUNCTION lfSMINSET_FormInit
PARAMETERS loBrnFormSet,loFormSet,llFromSaveFn

loBrnFormSet.Addproperty('loFormSet',loFormSet)
loBrnFormSet.Addproperty('llFromSaveFn',llFromSaveFn)

loBrnFormSet.Addproperty('lsModules',0)
 
WITH loBrnFormSet.AriaForm1
  .Caption = SYCCOMP.CCOMP_ID + '-' + ALLTRIM(SYCCOMP.CCOM_NAME) +' - Installation and Setup'
  .laAllMod.RowSource = 'Thisformset.loFormSet.laAllMod'
  *.laAllMod.ColumnWidths = .laAllMod.Width - 10
  
  .pbCls.Visible = loFormSet.ActiveMode = 'V' and !llFromSaveFn
  .pbSel.Visible = loFormSet.ActiveMode <> 'V' OR llFromSaveFn
  .pbAll.Visible = loFormSet.ActiveMode <> 'V' OR llFromSaveFn
  .pbNon.Visible = loFormSet.ActiveMode <> 'V' OR llFromSaveFn
  .pbInv.Visible = loFormSet.ActiveMode <> 'V' OR llFromSaveFn
  IF loFormSet.ActiveMode = 'V' AND !llFromSaveFn
    .Width = .Width - .shpAddEdit.Width - 5
  ENDIF 
ENDWITH
*- End of lfSMINSET_FormInit.

************************************************************
*! Name      : lfSMUPEXCH
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/09/2013
*! Purpose   : lfSMUPEXCH
************************************************************
FUNCTION lfSMUPEXCH
PARAMETERS loBrnFormSet,loFormSet,llNew
loBrnFormSet.Addproperty('loFormSet',loFormSet)
loBrnFormSet.Addproperty('llNew',llNew)

loBrnFormSet.Ariaform1.laFil2Upd.RowSource = 'laFil2Upd'


*- End of lfSMUPEXCH.

********************************************************************************************************
*
*   FUNCTION lfSaveCarrierInfo
*
********************************************************************************************************
FUNCTION lfSaveInfo

*- do not do anything in View mode
IF loFormSet.ActiveMode = 'V'
  RETURN 
ENDIF 
  
LOCAL lnI , lcCarrier,lcStatus,lnSvDataSession 
lnSvDataSession = SET("Datasession")
SET DATASESSION TO lnCurrDataSession

*- Save the Login Information data
IF !USED('SYREPUVR')
  USE (lcTempFldr+'SYREPUVR') IN 0
ENDIF
SELECT SYREPUVR
LOCATE
SCAN FOR SYREPUVR.crep_id = lcRepID AND SYREPUVR.NVARPOS>0
  *E303339,1 TMI 01/14/2013 [Start] 
  *lcFld = EVALUATE('loOgScroll.'+SYREPUVR.MFLD_NAME)
  *lcVal = IIF(!EMPTY(SYREPUVR.MDATA_DEF),EVALUATE('loOgScroll.'+SYREPUVR.MDATA_DEF),'')  
  lcFld = ALLTRIM(SYREPUVR.MFLD_NAME)
  lcVal = loOgScroll.&lcFld.
  *E303339,1 TMI 01/14/2013 [End  ] 
  
  *- always use character data type to replace the value in mdata_def field
  DO CASE
    CASE TYPE('lcVal') = 'L'
      lcVal = IIF(lcVal,'.T.','.F.')
    CASE TYPE('lcVal') = 'N'
      lcVal = ALLTRIM(STR(lcVal,15,4))
    CASE TYPE('lcVal') = 'D'
      lcVal = DTOC(lcVal)
  ENDCASE
  lcSeekId = IIF(lcModId='SM',CHR(255)+CHR(255),lcModId)
  IF SEEK(lcSeekId+STR(NVARPOS),'SETUPS')
    SELECT SETUPS
    REPLACE mdata_def WITH lcVal
  ELSE 
    SCATTER MEMVAR MEMO
    SELECT SETUPS
    APPEND BLANK 
    GATHER MEMVAR MEMO
    REPLACE CAPP_ID   WITH lcSeekId ;
            CFLD_NAME WITH m.MFLD_NAME ;
            mdata_def WITH lcVal
  ENDIF
ENDSCAN

USE IN SYREPUVR
SET DATASESSION TO lnSvDataSession 

*- End of FUNCTION lfSaveCarrierInfo
************************************************************
*! Name      : lfRepWhen
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/08/2013
*! Purpose   : update the 
************************************************************
FUNCTION lfRepWhen

*E303339,1 TMI 01/14/2013 [Start] restore the assinged company id, the reason for this that the gfGetMemvar does not work properly with that assginment
oAriaApplication.ActiveCompanyId = lcCompanyID 
oAriaApplication.ActiveCompanyName = lcCompanyName 
*E303339,1 TMI 01/14/2013 [End  ] 

DO CASE
CASE lcModID = 'SM'
  =lfAdjustSM()
CASE lcModID = 'IC'
  =lfAdjustIC()
CASE lcModID = 'AR'
  =lfAdjustAR()
CASE lcModID = 'MA'
  =lfAdjustMA()
*!*	CASE lcModID = 'MF'
*!*	  =lfAdjustMF()
CASE lcModID = 'PS'
  =lfAdjustPS()
CASE lcModID = 'SO'
  =lfAdjustSO()
ENDCASE 

loOgScroll.Refresh()
*- End of lfRepWhen.

************************************************************
*! Name      : lfAdjustModules
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/09/2013
*! Purpose   : lfAdjustModules
************************************************************
FUNCTION lfAdjustSM

llGlLink = lfLnkModul()
*E301469,1 AMH [Start]
llHist = GfGetMemVar('LLHIST',loFormSet.laData[1])
lcHisCmpID = GfGetMemVar('M_COMP_ID',loFormSet.laData[1])
*E301469,1 AMH [End  ]

*B802792,1 Reham On 04/24/2000  [Start]
*B802792,1 If there is no link default the bar to No else default to the default value.
M_LINK_GL = IIF(!llGlLink , "N" , M_LINK_GL)

DO CASE
CASE loFormSet.ActiveMode = 'E'
 
    llMultCCnt=IIF(LLMULCURR,.F.,.T.) 
    llMultCurr=IIF(LLMULCURR,.T.,.F.)
    *B603468,1 NAD (Start) Don't allow modification in UCC Manufacturer id when UPCs already 
    *B603468,1 NAD         Generated using this manufacturer id
    =lfShowGet('XMANUFID',lfChkUcc())
    *B603468,1 NAD (End)
    =lfShowGet('LLMULCURR',llMultCCnt)
    =lfShowGet('LLEXCHRATE',llMultCurr)
    =lfShowGet('LLSTYPRICE',llMultCurr)
    =lfShowGet('LLEDITEXRA',llMultCurr)
    =lfShowGet('LNEXRATDAY',llMultCurr)    
    *B601012,1 Show LNEXRATACC as well
    =lfShowGet('LNEXRATACC',llMultCurr)
    *B601012,1 end.
    *E300687,1 start
    *B802792,1 WAB - display the option 'Link To Gl' ENABLED if llGllink is true 
    *=lfShowGet('M_LINK_GL',.T.)
    =lfShowGet('M_LINK_GL',llGlLink)
    *B802792,1 WAB - END
    =lfShowGet('M_GL_VERS',M_LINK_GL='Y')
    =lfShowGet('M_SYS_DIR',M_LINK_GL='Y' .AND. M_GL_VERS='S')
    =lfShowGet('M_GL_CO',M_LINK_GL='Y' .AND. M_GL_VERS='S')
    =lfShowGet('M_POST_DET',M_LINK_GL='Y')
    =lfShowGet('M_DIV_LINK',M_LINK_GL='Y')
    *E300687,1 end
    *E301210,1 ASH 04/27/1999 (Begin) Disable Gl cost setting in case of not link with G/L.
    =lfShowGet('M_GL_COST',M_LINK_GL='Y')
    *E301210,1 ASH 04/27/1999 (End)
    *B601911,1 AMM start, Disable the taxes choices if the tax status is NO
    =lfShowGet('M_TAX_DESC',M_TAX = 'Y')    
    =lfShowGet('M_TAX_METH',M_TAX = 'Y')        
    =lfShowGet('M_TAX_RATE',M_TAX = 'Y')    
    =lfShowGet('M_TAX_REFE',M_TAX = 'Y')    
    *B601911,1 AMM end

    *E301469,1 AMH [Start]
    DO msg
    =lfShowGet('LLHIST',IIF(!llHist .AND. EMPTY(lcHisCmpID),.T.,.F.))
    =lfShowGet('M_COMP_ID',llHist .AND. EMPTY(lcHisCmpID))
    *E301469,1 AMH [End  ]
    
    *B607348,1 ABD - Don't print all UK on A4 papper, add seting to print on A4 Paper. [Begin]
    =lfShowGet('M_LLUSEPA4',gcContCode = "ENG")
    *B607348,1 ABD - [End]
CASE loFormSet.ActiveMode = 'A'
    llMultCCnt=.T.
    *B600797,1 M.H 01/09/96 Variable 'LLMULCURR' not found.
    *lcMultCurr=IIF(LLMULCURR,.T.,.F.)
    llMultCurr=IIF(LLMULCURR,.T.,.F.)
    *B600797,1 M.H End.
    *B603468,1 NAD (Start) Don't allow modification in UCC Manufacturer id when UPCs already 
    *B603468,1 NAD         Generated using this manufacturer id
    =lfShowGet('XMANUFID',lfChkUcc())
    *B603468,1 NAD (End)
    =lfShowGet('LLMULCURR',llMultCCnt)
    =lfShowGet('LLEXCHRATE',llMultCurr)
    =lfShowGet('LLSTYPRICE',llMultCurr)
    =lfShowGet('LLEDITEXRA',llMultCurr)
    =lfShowGet('LNEXRATDAY',llMultCurr)
    *B601012,1 Show LNEXRATACC as well
    =lfShowGet('LNEXRATACC',llMultCurr)
    *B601012,1 end.
    *B802792,1 WAB - display the option 'Link To Gl' ENABLED if llGllink is true 
    =lfShowGet('M_LINK_GL',llGlLink)
    *B802792,1 WAB - END
    *B601012,1 Disable objects in View mode

    *E301469,1 AMH [Start]
    =lfShowGet('LLHIST',IIF(!llHist .AND. EMPTY(lcHisCmpID),.T.,.F.))
    =lfShowGet('M_COMP_ID',llHist .AND. EMPTY(lcHisCmpID))
    *E301469,1 AMH [End  ]

    *B607348,1 ABD - Don't print all UK on A4 papper, add seting to print on A4 Paper. [Begin]
    =lfShowGet('M_LLUSEPA4',gcContCode = "ENG")
    *B607348,1 ABD - [End]

CASE loFormSet.ActiveMode = 'V'
    SELECT SYREPUVR
    LOCAL lcVar
    SCAN FOR CREP_ID = 'SMAPPSM'
      lcVar = ALLTRIM(SYREPUVR.mfld_name)
      =lfShowGet(lcVar,.F.)
    ENDSCAN
    
ENDCASE

IF UPPER(ALLTRIM(oAriaApplication.defaultcountry))<>'CANADA'
  =lfShowGet('M_HST_RATE',.F.)
ENDIF  

*- End of lfAdjustSM.

************************************************************
*! Name      : lfAdjustIC
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/09/2013
*! Purpose   : Adjust the controls of the Option Grid of the IC module by enable/disable appropriatly
************************************************************
FUNCTION lfAdjustIC
=lfShowGet('M_WAREHOUS',M_WAREHOUS='N')

=lfShowGet('M_CITYPE1',.F.)
=lfShowGet('MCLRASSCOD',MCLRASSORT = 'Y')
llOpScale = gfOpenFile(ALLTRIM(loFormSet.laData[11])+'SCALE','','SH')
GO TOP
=lfShowGet('M_USEEXSSC',EOF('SCALE'))
=lfShowGet('M_EXTWIDTH',M_USEEXSSC AND EOF('SCALE'))
IF llOpScale
  =gfCloseFile('SCALE')
ENDIF
=lfShowGet('CDEFSTYTAX',gfGetMemVar('M_TAX')='Y')

*llOpBom = gfOpenTable('Bom','','SH')
lfOpenSql('top 10 * ','bom','bom')
SELECT bom
LOCATE 
=lfShowGet('M_CITYPE2',EOF('BOM'))
=lfShowGet('M_CITYPE3',EOF('BOM'))
=lfShowGet('M_CITYPE4',EOF('BOM'))
=lfShowGet('M_CITYPE5',EOF('BOM'))
=lfShowGet('M_CITYPE6',EOF('BOM'))
=lfShowGet('M_CITYPE7',EOF('BOM'))

FOR counter = 1 TO 7
  z = str(counter,1)
  =lfShowGet('M_CMTYPE'+Z,EOF('BOM'))
ENDFOR

IF llOpBom
  USE IN BOM
ENDIF

=lfShowGet('M_STYCNFG',M_DYELOT="Y")
M_STYCNFG = IIF(M_DYELOT="N","N",M_STYCNFG)

=lfShowGet('M_CONFDEF',M_STYCNFG="Y")
M_CONFDEF = IIF(M_DYELOT="N","N",M_CONFDEF)

=lfShowGet('M_CONFCODE',M_STYCNFG="Y")

*- End of lfAdjustIC.

*!*************************************************************
*! Name      : lfOpenSql
*: Developer : Mariam Mazhar (MMT)
*: Date      : 02/16/2005 
*! Purpose   : function to open SQL tables
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfOpenSql

LPARAMETERS lcSelFlds,lcTable,lcCursor,lcWhereCond,llIsInitial
LOCAL lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection
PRIVATE laIndex
DIMENSION laIndex[1,2]

lcSqlStatment   = "SELECT  " + lcSelFlds + "  FROM " + lcTable + IIF(TYPE('lcWhereCond') = 'C' AND !EMPTY(lcWhereCond)," WHERE " + lcWhereCond ,"")

lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
                                      'BROWSE',SET("DATASESSION"))
IF lnConnectionHandlar = 1
  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
  =CURSORSETPROP("Buffering",3,lcCursor)
  *-- To initialize the indecis that will be created for each file
*!*	  *=lfCrtindex(lcCursor)
*!*	  SELECT (lcCursor)
*!*	  FOR lnI = 1 TO ALEN(laIndex,1)
*!*	    lcIndex = laIndex[lnI,1]
*!*	    lcTag   = laIndex[lnI,2]
*!*	    INDEX ON &lcIndex. TAG (lcTag) 
*!*	  ENDFOR
*!*	  lcTag = laIndex[1,2]
*!*	  SET ORDER TO TAG (lcTag)

ELSE
  =loOGScroll.oRDA.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF
*-- end of lfOpenSql.


************************************************************
*! Name      : lfAdjustAR
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/09/2013
*! Purpose   : Adjust the controls of the Option Grid of the AR module by enable/disable appropriatly
************************************************************
FUNCTION lfAdjustAR
= lfShowGet('M_CRLM_DAY',(M_CRDT_LMT = 'Y'))

*- End of lfAdjustAR.

************************************************************
*! Name      : lfAdjustMA
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/09/2013
*! Purpose   : Adjust the controls of the Option Grid of the MA module by enable/disable appropriatly
************************************************************
FUNCTION lfAdjustMA
=lfShowGet('M_TRKROLLS',M_MATCSTMT = 'L')
=lfShowGet('M_GENROLID',M_TRKROLLS = 'Y')

*- End of lfAdjustMA.
************************************************************
*! Name      : lfAdjustPS
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/09/2013
*! Purpose   : Adjust the controls of the Option Grid of the PS module by enable/disable appropriatly
************************************************************
FUNCTION lfAdjustPS
llShowDfLc = (gfGetMemVar('M_WareHouse') = 'Y' .AND. OCCURS('NC',oAriaApplication.companysetupmodules)<>1)
=lfShowGet('M_PSDEFLOC',llShowDfLc)

*- End of lfAdjustPS.
************************************************************
*! Name      : lfAdjustSO
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/09/2013
*! Purpose   : Adjust the controls of the Option Grid of the SO module by enable/disable appropriatly
************************************************************
FUNCTION lfAdjustSO
llMScale   = gfGetMemVar('M_USEEXSSC')
=lfShowGet('M_EXSS_SCR',llMscale)

*- End of lfAdjustSO.

*!*************************************************************
*! Name      : lfLnkModul
*! Developer : WAB - Walid A. Wahab
*! Date      : 01/05/2000
*! Purpose   : check if there are installed modules  can be linked with Gl
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfLnkModul()
*!*************************************************************
*B802792,1 
*!*************************************************************
FUNCTION lfLnkModul
PRIVATE llModFound,lnAlias
*---llModFound---> hold .T. if there any module can linked to GL
*---lnAlias -----> hold current alias
lnAlias= SELECT()

*B802792,1 Reham  on 04/24/2000 [Start]
*llModFound = ('IC' $ gcCmpModules .OR. 'AR' $ gcCmpModules .OR. 'MA' $ gcCmpModules .OR. ;
              'PO' $ gcCmpModules .OR. 'PS' $ gcCmpModules .OR. 'RM' $ gcCmpModules .OR. ;
              'MF' $ gcCmpModules )
*B802792,1 Check the modules of the current company in the company screen not the active running company.
llModFound = ('IC' $ ALLTRIM(loFormSet.laData[15]) OR 'AR' $ ALLTRIM(loFormSet.laData[15]) OR 'MA' $ ALLTRIM(loFormSet.laData[15]) OR ;
              'PO' $ ALLTRIM(loFormSet.laData[15]) OR 'PS' $ ALLTRIM(loFormSet.laData[15]) OR 'RM' $ ALLTRIM(loFormSet.laData[15]) OR ;
              'MF' $ ALLTRIM(loFormSet.laData[15]) )
*B802792,1 Reham  on 04/24/2000 [End]

*---- IF there is no modules can linked to gl check in ap setup if the ap module 
*---- is linked to gl to default the option 'Link to Gl' -->(yes) if it is linked
*B804136,1 SSH Fix APSETUP not found.[Start]
*IF !llModFound
IF !llModFound  .AND. FILE(ALLTRIM(loFormSet.laData[11])+"APSETUP.DBF")
*B804136,1 SSH Fix APSETUP not found.[End]
  IF !USED('APSETUP')
    *B802792,1 Reham  on 04/24/2000 [Start]
    *B802792,1 Open the ApSetup file for the current company in the company screen not the active company.
    *=gfOpenFile(gcDataDir+"APSETUP",'', "SH",'',.T.)
    =gfOpenFile(ALLTRIM(loFormSet.laData[11])+"APSETUP",'', "SH",'',.T.)
    *B802792,1 Reham  on 04/24/2000 [End]
  ENDIF
  M_LINK_GL = APSETUP.cApsGllink 
ENDIF  
SELECT (lnAlias)
RETURN llModFound


*!*************************************************************
*! Name      : lfChkUcc
*! Developer : Nader Anis NAD
*! Date      : 02/22/2000
*! Purpose   : Check if the UPC manufacturer code exist in the
*!           : StyleUpc file  
*! Refer to  : B603468,1
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfChkUcc()
*!*************************************************************
FUNCTION lfChkUcc

*B804136,1 SSH Fix StyleUPC not found.[Start]
PRIVATE lnToRetUc,lcTempDir
lcTempDir  = ALLTRIM(loFormSet.laData[11])
lnToRetUc = .F.

*B605279,1 WAB (Start) - check the file with the extension.
*IF FILE(lcTempDir+"STYLEUPC")
IF FILE(lcTempDir+"STYLEUPC.DBF")
*B605279,1 WAB (End)

*B804136,1 SSH Fix StyleUPC not found.[END]
  IF !USED('StyleUpc')
    *B603744,1 (Begin) In case of no active company selected, gcDataDir is empty generating the bug.
    lcOldDataD = gcDataDir
    gcDataDir  = ALLTRIM(loFormSet.laData[11])
    *B603744,1 (End)
    =gfOpenFile(gcDataDir+"StyleUpc",'StyUpcN', "SH",'',.T.)
    *B603744,1 (Begin) Restore the old data dir
    gcDataDir  = lcOldDataD
    *B603744,1 (End)
  ENDIF
  *B804136,1 SSH Fix StyleUPC not found.[Start]
  *RETURN !SEEK (XManufId,'StyleUpc')
  lnToRetUc = !SEEK (XManufId,'StyleUpc')
ENDIF
RETURN(lnToRetUc)
*B804136,1 SSH Fix StyleUPC not found.[END]

************************************************************
*! Name      : lfBuff
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/08/2013
*! Purpose   : set the buffering mode to 5 of the passed table alias
************************************************************
FUNCTION lfBuff
PARAMETERS lcAlias,lcTag
*E303339,1 TMI 01/08/2013 [Start] open syccomp, set the buffering mode
lnSlct = SELECT(0)
IF !USED(lcAlias)
  =gfOpenFile(oAriaApplication.SysPath+lcAlias,lcTag,'SH')
ENDIF 
SELECT &lcAlias
CURSORSETPROP("Buffering",5)
SELECT (lnSlct)
*E303339,1 TMI 01/08/2013 [End  ] 
*- End of lfBuff.

************************************************************
*! Name      : lfShowGet
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/08/2013
*! Purpose   : enable / disable OG control
************************************************************
FUNCTION lfShowGet
PARAMETERS lcVar,llEnable
LOCAL lnPos
lcVar = ALLTRIM(UPPER(lcVar))
lnPos = ASCAN(LAOGOBJTYPE,lcVar)
IF lnPos>0
  lnPos= ASUBSCRIPT(LAOGOBJTYPE,lnPos,1)
  LAOGOBJCNT[lnPos] = llEnable
  = lfOGShowGet(lcVar)
ENDIF   
*- End of lfShowGet.


*********** FUNCTIONS USED IN OG FROM DIFFERENT MODULES ****************




*!**************************************************************************
*! Name      : lfvChgDye
*! Developer : Ahmed Mohammed
*! Date      : 02/12/1998
*! Purpose   : Validate the changing of dyelot setup from yes to No.
*! REF       : *B601911,1
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : gfModalGen(),gfTempName(),lfShowGet()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvChgDye()
*!**************************************************************************
FUNCTION lfvChgDye
*B601911,1 If changing from Dyelots Yes to No
IF M_DYELOT = 'N'
  lnAlias  = SELECT(0)
  lcTSTYLE = gfTempName()
  SELECT 0
  *E301098,1 Hesham (Start)
  *USE ALLTRIM(SyCCOMP.cCom_DDir)+'STYLE.DBF' AGAIN ALIAS &lcTSTYLE   
  *USE (gfGetDataDir(ALLTRIM(SyCCOMP.cCom_DDir))+'STYLE.DBF') AGAIN ALIAS &lcTSTYLE 
  USE (ALLTRIM(loFormSet.laData[11])+'STYLE.DBF') AGAIN ALIAS &lcTSTYLE 
  *E301098,1 Hesham (End)
  *B601911,1 Do not allow changing from Yes to No if there are any styles
  *B601911,1 using dyelots
  LOCATE FOR cDYE_Flg = 'Y' 
  IF FOUND()
    *B601911,1 AMM the message is  "Some Styles are in dyelots,You need to"+ 
    *B601911,1 AMM "change these Styles to dyelot no before you can change the system setup." 
    =gfModalGen("QRM00291B00000","DIALOG",'Styles|Styles')         
    M_DYELOT = 'Y'
    =lfShowGet('M_DYELOT',.T.)
  ENDIF
  USE IN (lcTSTYLE)
  SELECT (lnAlias)
ENDIF
*N119681,1 SSH Enabled/Disable style configuration  popup
=lfShowGet('M_STYCNFG',M_DYELOT="Y")
M_STYCNFG = IIF(M_DYELOT="N","N",M_STYCNFG)
*N119681,1 SSH Enabled/Disable style configuration  popup

*Assure that SYCCOMP is open

*:*************************************************************
*: Name      : lfvCRLMDay
*: Developer : Abdou Elgendy
*: Date      : 07/29/2003
*: Purpose   : Enable and Disable Credt limit aged days seting.
*: Refer to  : B607038,1 
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfvCRLMDay ()
*:*************************************************************
*:
FUNCTION lfvCRLMDay

IF M_CRDT_LMT = 'Y'
  = lfShowGet('M_CRLM_DAY',.T.)
ELSE
  = lfShowGet('M_CRLM_DAY',.F.)
ENDIF


*-- End OF lfvCRLMDay


*!*************************************************************
*! Name       : lfvCusPass
*! Developer  : Reham Al-Allamy
*! Date       : 06/25/2001
*! Purpose    : Customer Password Function
*!*************************************************************
*! Calls      : None
*!*************************************************************
*! Called From: Setup Screen
*!*************************************************************
*! Parameters : None
*!*************************************************************
*! Returns    :  None.
*!*************************************************************
*! Example    :  =lfvCusPass()
*!*************************************************************
*
FUNCTION lfvCusPass
PRIVATE lcValue , llOpenFld , lcAlias , laField , lcKey , lcOrder

*lcValue   = EVALUATE(VARREAD())
lcValue   = loOgScroll.ActiveControl.Value
llOpenFld = .F.

lcAlias = ALIAS()

IF !USED("SYDFIELD")
  =gfOpenFile(gcSysHome+'SYDFIELD',gcSysHome+'Cfld_name','SH')
  llOpenFld = .T.
ELSE
  SELECT SYDFIELD
  lcOrder = ORDER()
  SET ORDER TO TAG Cfld_name
ENDIF

IF !SEEK(lcValue,"SYDFIELD")
  lcBrFields = [cfld_name:H="Field Code",cfld_head:H="Field Header"]
  lcFile_ttl = "Users' Fields"
  DECLARE laField[1]
  laField[1] = M_CUSTPASS
  lcKey      = "USR_DFND"
  
  *-- Call the browse with the Fields file.
  =gfBrows("lcKey" , "cFld_Name" , "laField")
  M_CUSTPASS = IIF(M_CUSTPASS=laField[1] , SPACE(10) , laField[1])
ENDIF

IF !EMPTY(M_CUSTPASS) AND !EMPTY(M_CONFMAIL) AND (ALLTRIM(M_CUSTPASS) == ALLTRIM(M_CONFMAIL))
  *** "Confirmation e-mail address field must be different from the customer password field." ***
  *** <  Ok  > ***
  =gfModalGen("TRM24000B00000","Dialog")
  RETURN .F.
ENDIF

IF llOpenFld
  =gfCloseFile("SYDFIELD")
ELSE
  SELECT SYDFIELD
  SET ORDER TO &lcOrder
ENDIF
SELECT (lcAlias)

*!*************************************************************
*! Name       : lfvConMail
*! Developer  : Reham Al-Allamy
*! Date       : 06/25/2001
*! Purpose    : Customer Confirmation Mail
*!*************************************************************
*! Calls      : None
*!*************************************************************
*! Called From: Setup Screen
*!*************************************************************
*! Parameters : None
*!*************************************************************
*! Returns    :  None.
*!*************************************************************
*! Example    :  =lfvConMail()
*!*************************************************************
*
FUNCTION lfvConMail
PRIVATE lcValue , llOpenFld , lcAlias , laField , lcKey , lcOrder

*lcValue   = EVALUATE(VARREAD())
lcValue   = loOgScroll.ActiveControl.Value
llOpenFld = .F.

lcAlias = ALIAS()

IF !USED("SYDFIELD")
  =gfOpenFile(gcSysHome+'SYDFIELD',gcSysHome+'Cfld_name','SH')
  llOpenFld = .T.
ELSE
  SELECT SYDFIELD
  lcOrder = ORDER()
  SET ORDER TO TAG Cfld_name
ENDIF


IF !EMPTY(lcValue)
  IF !SEEK(lcValue,"SYDFIELD")
    lcBrFields = [cfld_name:H="Field Code",cfld_head:H="Field Header"]
    lcFile_ttl = "Users' Fields"
    DECLARE laField[1]
    laField[1] = M_CONFMAIL
    lcKey      = "USR_DFND"
    
    *-- Call the browse with the Fields file.
    =gfBrows("lcKey" , "cFld_Name" , "laField")
    M_CONFMAIL = IIF(M_CONFMAIL = laField[1] , SPACE(10) , laField[1])
  ENDIF
  
  IF !EMPTY(M_CUSTPASS) AND !EMPTY(M_CONFMAIL) AND (ALLTRIM(M_CUSTPASS) == ALLTRIM(M_CONFMAIL))
    *** "Confirmation e-mail address field must be different from the customer password field." ***
    *** <  Ok  > ***
    =gfModalGen("TRM24000B00000","Dialog")
    RETURN .F.
  ENDIF
ENDIF

IF llOpenFld
  =gfCloseFile("SYDFIELD")
ELSE
  SELECT SYDFIELD
  SET ORDER TO &lcOrder
ENDIF
SELECT (lcAlias)

*!*************************************************************
*! Name       : lfvLetters
*! Developer  : Reham Al-Allamy
*! Date       : 06/25/2001
*! Purpose    : Letter Id's Function
*!*************************************************************
*! Calls      : None
*!*************************************************************
*! Called From: Setup Screen
*!*************************************************************
*! Parameters : None
*!*************************************************************
*! Returns    :  None.
*!*************************************************************
*! Example    :  =lfvLetters()
*!*************************************************************
*
FUNCTION lfvLetters
PRIVATE lcValue , llOpenLet , lcAlias

*lcValue   = EVALUATE(VARREAD())
lcValue   = loOgScroll.ActiveControl.Value
llOpenLet = .F.

lcAlias = ALIAS()

IF !USED("LETTERS")
  =gfOpenFile(gcDataDir+'LETTERS',gcDataDir+'Cletterid','SH')
  llOpenLet = .T.
ENDIF
*?*
lcVar = loOgScroll.ActiveControl.parent.name
IF !SEEK(lcValue,"LETTERS")
  *=GFVLFLD('LETTERS','cletterid',VARREAD(),'',.F.,.F.,[cletterid:H="Letter ID",cletshdes:H="Letter Description"],'1',.F.,.t.)
  SELECT LETTERS
  lcBrFields = [cletterid:H="Letter ID",cletshdes:H="Letter Description"]
  DECLARE laField[1]
  lcOldVal = &lcVar
  laField[1] = ' '
    
    *-- Call the browse with the Fields file.
  =gfBrows("" , "CLETTERID" , "laField")
  IF !EMPTY(laField)
    &lcVar = laField
  ELSE
    &lcVar = lcOldVal
  ENDIF 
ENDIF

IF llOpenLet
  =gfCloseFile("LETTERS")
ENDIF

SELECT (lcAlias)

*!**************************************************************************
*! Name      : lfvPack
*! Developer : Ahmed Mohammed
*! Date      : 02/12/1998
*! Purpose   : Validate the changing of ''USE STYLE PACKS/SKU' setting.
*! REF       : *B601911,1
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : gfModalGen(),gfTempName(), lfShowGet()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvPack()
*!**************************************************************************
FUNCTION lfvPack
PRIVATE lcTSPCKH,lnAlias
*B601911,1 AMM If the setup changed from YES to NO.
IF M_PACK = 'N'
  lnAlias = SELECT(0)
  lcTSPCKH = gfTempName()
  SELECT 0
  *E301098,1 Hesham (Start)
  *USE ALLTRIM(SyCCOMP.cCom_DDir)+'SPCK_HDR.DBF' AGAIN ALIAS &lcTSPCKH
  USE (ALLTRIM(loFormSet.laData[11])+'SPCK_HDR.DBF') AGAIN ALIAS &lcTSPCKH
  *E301098,1 Hesham (End)
  GO TOP
  *B601911,1 AMM The message is "Pack and/or Sku already exist , Are you sure you want to modify the setup ?"
  IF !EOF() AND gfModalGen("QRM00290B00006","DIALOG",'Pack and\or Sku')=2
    M_PACK = 'Y'
    =lfShowGet('M_PACK',.T.)
  ENDIF 
  USE IN (lcTSPCKH)
  SELECT (lnAlias)
ENDIF


*!**************************************************************************
*! Name      : lfvWareLoc
*! Developer : Ahmed Mohammed
*! Date      : 02/12/1998
*! Purpose   : Validate the changing of WareHouse locations setup from yes to No.
*! REF       : *B601911,1
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : gfModalGen(), gfTempName(),lfShowGet()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvWareLoc()
*!**************************************************************************
FUNCTION lfvWareLoc
PRIVATE lnAlias,lcTWHSLoc 

*B601911,1 AMM If changed from Kepp track of locations YES to NO
IF M_WARELOC = "N"
  lnAlias = SELECT(0)
  lcTWHSLoc = gfTempName()
  SELECT 0
  *E301098,1 Hesham (Start)
  *USE ALLTRIM(SyCCOMP.cCom_DDir)+'WHSLOC.DBF' AGAIN ALIAS &lcTWHSLOC 
  USE (ALLTRIM(loFormSet.laData[11])+'WHSLOC.DBF') AGAIN ALIAS &lcTWHSLOC   
  *E301098,1 Hesham (End)
  GO TOP
  IF !EOF()
    *B601911,1 AMM The message is "Please note that all locations will be lost. Are you sure you want to proceed ?"
    IF gfModalGen("QRM42063B00006","DIALOG",'bins') = 2
      M_WARELOC = 'Y'
      =lfShowGet('M_WARELOC',.T.)
    ELSE
      *B601911,1 AMM Delete the whole warehouse locations file.
      SCATTER MEMVAR MEMO BLANK
      SCAN 
        GATHER MEMVAR MEMO
      ENDSCAN
      DELETE ALL
    ENDIF
  ENDIF  
  USE IN (lcTWHSLOC)
  SELECT (lnAlias)
ENDIF


*!**************************************************************************
*! Name      : lfvUCCDiv
*! Developer : Ahmed Mohammed
*! Date      : 02/12/1998
*! Purpose   : Validate the changing 'Maintain U.C.C. Manf. ID ' setting.
*! REF       : *B601911,1
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : gfModalGen(),gfTempName(),lfShowGet()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvUCCDiv()
*!**************************************************************************
FUNCTION lfvUCCDiv

PRIVATE lnAlias,lcTCodes
*B601911,1 AMM If the setup changed from YES to NO
IF M_UCCDIV = "N"
  lnAlias = SELECT(0)
  lcTCodes = gfTempName()
  SELECT 0
  *E301098,1 Hesham (Start)
  *USE ALLTRIM(SyCCOMP.cCom_DDir)+'CODES.DBF' AGAIN ALIAS &lcTCodes
  USE (ALLTRIM(loFormSet.laData[11])+'CODES.DBF') AGAIN ALIAS &lcTCodes
  *E301098,1 Hesham (End)
  SET ORDER TO TAG IdRltFName
  *B601911,1 AMM Search if there is any devision exist.
  =SEEK (SyCCOMP.cComp_ID+'Y'+'CDIVISION')
  IF !EOF() 
    *B601911,1 AMM search if any devision has a related field 'CUPCMAN' not empty.
    LOCATE REST WHILE ccomp_id+crltfield+cfld_name=SyCCOMP.cComp_ID+'Y'+'CDIVISION';
                FOR cRltd_Nam='CUPCMAN' .AND. !EMPTY(cRltd_Vlu)
               
    *B601911,1 AMM the message is "UPC numbers at the division level already exist , Are you sure you want to change ?"
    IF FOUND() .AND. gfModalGen("QRM00290B00006","DIALOG",'UPC numbers at the division level')=2
      M_UCCDIV = "Y"
      =lfShowGet('M_UCCDIV',.T.)
    ENDIF
  ENDIF
  USE IN (lcTCodes)
  SELECT (lnAlias)
ENDIF

*!*************************************************************
*! Name      : lfvUseExt
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 04/25/1999
*! Purpose   : Enable/Disable Extended size scale width object 
*!*************************************************************
*! Calls     : 
*!             Procedures  : None
*!             Functions   : lfShowGet()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Example   :  =lfvUseExt()
*!*************************************************************
*
FUNCTION lfvUseExt
*E301209,1 Enable/Disable Extended size scale width object [Begin]
M_EXTWIDTH = 1
=lfShowGet('M_EXTWIDTH',M_USEEXSSC)
*E301209,1 Enable/Disable Extended size scale width object [End  ]
*-- end of lfvUseExt.
*:*************************************************************
*! Name     : lfvDrShpWar
*! Developer: Timour A. K.
*! Date     : 10/10/97               
*! Purpose  : Vadidate the drop ship warehouse in P/O setup.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfOpenFile() , gfBrowWare() 
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvDrShpWar()
*!*************************************************************
FUNCTION lfvDrShpWar

IF !EMPTY(M_DROPWARE)
  lnAlias=SELECT()
  = gfOpenFile(gcDataDir+"WAREHOUS","WAREHOUS","SH")
  IF !SEEK(M_DROPWARE,'WAREHOUS')
    M_DROPWARE = gfBrowWare( .T. )
  ENDIF
  SELECT(lnAlias)
  SHOW GET M_DROPWARE
ENDIF

*!**************************************************************************
*! Name      : lfvClrAstm
*! Developer : Ahmed Ibrahim (AMM)
*! Date      : 11/29/1998
*! Purpose   : Validate the changing of "Use color assortments" setting.
*! REF       : *E301073,1 
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : lfShowGet()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvClrAstm()
*!**************************************************************************
FUNCTION lfvClrAstm
IF MCLRASSORT = 'Y'
  *E301073,1 AMM Enable the Color assortment code POPUP
  =lfShowGet('MCLRASSCOD',.T.)
ELSE
  *E301073,1 AMM Initialize and disable Color assortment code POPUP
  MCLRASSCOD = SPACE(6)
  =lfShowGet('MCLRASSCOD',.F.)
ENDIF

*!**************************************************************************
*! Name      : lfvclrasc
*! Developer : Ahmed Ibrahim (AMM)
*! Date      : 11/29/1998
*! Purpose   : Validate the "color assortments code" setting.
*! REF       : *E301073,1 
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : lfShowGet()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvclrasc()
*!**************************************************************************

FUNCTION lfvclrasc
PRIVATE lcBrFields, lcFile_ttl
lcCodTem = gfTempName()
SELECT 0
USE (ALLTRIM(loFormSet.laData[11])+'CODES.DBF') AGAIN ALIAS &lcCodTem
SET ORDER TO Codes
IF !EMPTY(MCLRASSCOD)
  *E301073,1 AMM If the entered color code doesn't exist
  *B602725,1 AMM Adjust to fit the new index
  *IF !SEEK(ALLTRIM(SyCCOMP.cComp_ID)+MCLRASSCOD+"N"+"COLOR") 
  IF !SEEK('N'+MCLRASSCOD+"N"+"COLOR") 
  *B602725,1 AMM end
    SET ORDER TO TAG Idrltfname
    *E301073,1 AMM If there is at least one color code in the file
    *B602725,1 AMM Adjust to fit the new index
    *IF SEEK(ALLTRIM(SyCCOMP.cComp_ID)+'N'+'COLOR')
    IF SEEK('N'+'N'+'COLOR')
    *B602725,1 AMM end
      *E301073,1 AMM browse color codes
      *E301073,1 AMM browse fields
      lcBrFields = "CCODE_NO :H='Code',CDISCREP :H='Description'"
      DIMENSION laTemp[1]
      STORE '' TO laTemp
      *E301073,1 AMM vbrowse title
      lcFile_ttl    = "Select Color"
      *E301073,1 AMM key expression
      *B602725,1 AMM Adjust to fit the new structure
      *lcFilt = ALLTRIM(SyCCOMP.cComp_ID)+'N'+'COLOR'
      lcFilt = 'N'+'N'+'COLOR'
      *B602725,1 AMM end
      =gfBrows("lcFilt",'ccode_no',"laTemp")
      MCLRASSCOD = laTemp[1]
      =lfShowGet('MCLRASSCOD',.T.)
    ELSE
      *E301073,1 AMM            Message
      *E301073,1 AMM     Color xxx doesn't exist
      *E301073,1 AMM        <Add>  <Reenter>
      IF gfModalGen("QRM42155B42006","DIALOG",MCLRASSCOD) = 1
        lcfldOrd = ORDER('SYDFIELD')
        SET ORDER TO TAG cfld_name IN SYDFIELD
        IF SEEK('COLOR', "SYDFIELD")
          lcRltField = UPPER(ALLTRIM(SYDFIELD.mRltFields))
          lcRltField = "|" + lcRltField + "|" 


          INSERT INTO (lcCodTem) ;
             (cDefCode, cfld_name,ccode_no, cdiscrep,crltfield, ;
                    cadd_user,dadd_date,cadd_time) ;
          VALUES ('N' , 'COLOR', MCLRASSCOD, '' , "N" , ;
                   gcUser_ID , DATE() , gfGetTime())
          =gfTraceKey('CODES','N'+PADR('COLOR',6)+MCLRASSCOD+;
                       SPACE(30)+SPACE(10),'A',ALLTRIM(SyCCOMP.cCom_DDir),;
                       SYCCOMP.mModlSet)
          IF !SEEK('D'+'N'+'COLOR') 
            *E301073,1 AMM  Insert the default code record
            INSERT INTO (lcCodTem) ;
             (cDefCode,cfld_name,ccode_no, ;
              cdiscrep,crltfield, ;
              cadd_user,dadd_date,cadd_time) ;
            VALUES ('D','COLOR',MCLRASSCOD, ;
                '',"N", gcUser_ID,DATE(),gfGetTime())
            =gfTraceKey('CODES','D'+PADR('COLOR',6)+MCLRASSCOD,'A',;
             ALLTRIM(SyCCOMP.cCom_DDir),SYCCOMP.mModlSet)
          ENDIF
          *B602725,1 AMM end
          
          *E301073,1 AMM  Get related fields
          SELECT DISTINCT sydField.cFld_name,sydField.cData_typ;
           FROM  sydField ;
           WHERE "|"+UPPER(ALLTRIM(sydField.cFld_name))+"|" $ lcRltField ;
             .OR. "|"+'$'+UPPER(ALLTRIM(sydField.cFld_name))+"|" $ lcRltField ;
           INTO  ARRAY laRelFld
          *E301073,1 AMM  Add related fields records to the code file
          FOR lnCount = 1 TO ALEN(laRelFld,1)
            INSERT INTO (lcCodTem) ;
                   (cDefCode,cfld_name,ccode_no, ;
                    cdiscrep,crltfield, ;
                    cRltd_Nam,cRltd_Typ,cRltd_Vlu,;
                    cadd_user,dadd_date,cadd_time) ;
            VALUES ('N','COLOR',MCLRASSCOD, ;
                    "","Y",laRelFld[lnCount,1],;
                    laRelFld[lnCount,2],"", ;
                    gcUser_ID,DATE(),gfGetTime())
            =gfTraceKey('CODES','N'+PADR('COLOR',6)+;
                         MCLRASSCOD+ SPACE(30)+laRelFld[lnCount,1],'A',ALLTRIM(SyCCOMP.cCom_DDir),;
                       SYCCOMP.mModlSet)
            *B602725,1 AMM end
          ENDFOR
        ENDIF
        SET ORDER TO TAG &lcfldOrd IN SYDFIELD
      ELSE
        *E301073,1 AMM  If the user choose to reenter the color code
        _CUROBJ = _CUROBJ
      ENDIF

    ENDIF
  ENDIF
ENDIF

USE IN (lcCodTem)

************************************************************
*! Name      : lfValConfig
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/08/2013
*! Purpose   : *N119681,2 WSH, [Start] 03/22/2004 Add Aefault configuration Code & Cost Elements 6,7 to IC Setup
************************************************************
FUNCTION lfValConfig
PRIVATE llValid
llValid = (M_STYCNFG="N") OR !Empty(M_CONFCODE)
RETURN llValid
*- End of lfValConfig.
*N119681,2 WSH, [End] 03/22/2004 Add Aefault configuration Code & Cost Elements 6,7 to IC Setup


*!**************************************************************************
*! Name      : lfvChgDyeMA
*! Developer : Ahmed Mohammed
*! Date      : 02/12/1998
*! Purpose   : Validate the changing of dyelot setup from yes to No.
*! REF       : *B601911,1
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : gfModalGen(), gfTempName()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvChgDyeMA()
*!**************************************************************************
FUNCTION lfvChgDyeMA

IF M_MATDYE = 'N'
  lnAlias  = SELECT(0)
  lcTFab   = gfTempName()
  SELECT 0
  *E301098,1 Hesham (Start)
  *USE ALLTRIM(SyCCOMP.cCom_DDir)+'FABRIC.DBF' AGAIN ALIAS (lcTFab)
  USE (ALLTRIM(loFormSet.laData[11])+'FABRIC.DBF' AGAIN ALIAS (lcTFab)
  *E301098,1 Hesham (End)
  *B601911,1 Search for any material using dyelots
  LOCATE FOR cDYE_Flg = 'Y' 
  *B601911,1 If any material using dyelots is found, do not allow
  *B601911,1 changing dyelot from Yes to No
  IF FOUND()
    *B601911,1 AMM the message is "Some Materials are in dyelots,You need to change"+ 
    *B601911,1 AMM "these items to no before you can change the system setup."    
    =gfModalGen("QRM00291B00000","DIALOG",'Materials|items') 
    M_MATDYE = 'Y'
    =lfShowGet('M_MATDYE',.T.)
  ENDIF
  USE IN (lcTFab)
  SELECT (lnAlias)
ENDIF

*!**************************************************************************
*! Name      : lfvmaCsM
*! Developer : Ahmed Mohammed
*! Date      : 11/04/1998
*! Purpose   : Validate the changing of material cost method
*! REF       : *E300930,1 AMM
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : lfShowGet()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvmaCsM()
*!**************************************************************************
FUNCTION lfvmaCsM
IF M_MATCSTMT = 'L'
  *E300930,1 AMM If material cost method is Lot, Enable the keep tracking 
  *E300930,1 AMM of rolls setting
  =lfShowGet('M_TRKROLLS',.T.)
ELSE
  M_TRKROLLS = 'N'
  =lfShowGet('M_TRKROLLS',.F.)
  *E500330,1,E500329,1 WAB - (START) If material cost method is !Lot,DISABLE generate manual Rol ID 
   M_GENROLID = 'N'
  =lfShowGet('M_GENROLID',.F.)
  *E500330,1,E500329,1 WAB - (END)
ENDIF

*!**************************************************************************
*! Name      : lfvGENMONU
*! Developer : Hossam El Etreby
*! Date      : 09/16/1999
*! Purpose   : Valid function of 'Manual Material Manufacturing order no.' 
*!             setting in the option grid.
*! REF       : E301234,1
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : gfModalGen()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvGENMONU()
*!**************************************************************************
FUNCTION lfvGENMONU
IF M_GENMONUM = "N"
  *"Please make sure that the generated sequence"+
  *"number will not overlap existing Cutting Ticket numbers ."
  =gfModalGen('INM00288B00000','DIALOG','Manufacturing Order')
ELSE
  *"Please note that you will not be able to "+
  *"assign a number that already exist ."
  =gfModalGen('INM00289B00000','DIALOG')
ENDIF

*************

*!**************************************************************************
*! Name      : lfvGenRolId
*! Developer : WAB - Walid A. Wahab
*! Date      : 08/07/2000
*! Purpose   : Valid function of 'Keep Track of Mat. Rolls .' 
*!             setting in the option grid.
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvGenRolId()
*!**************************************************************************
*!E500329,1 & E500330,1
*!**************************************************************************
FUNCTION lfvTrkRolls
IF M_TRKROLLS = 'Y'
  *--If Keep Track of Mat. Rolls  . Enable the create manual Rol id
  =lfShowGet('M_GENROLID',.T.)
ELSE
   M_GENROLID = 'N'
  =lfShowGet('M_GENROLID',.F.)
ENDIF

*!**************************************************************************
*! Name      : 
*! Developer : WAB - Walid A. Wahab
*! Date      : 08/07/2000
*! Purpose   : Valid function of 'Manual Rolls ID .' 
*!             setting in the option grid.
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : gfModalGen()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =()
*!**************************************************************************
*!E500329,1 & E500330,1
*!**************************************************************************
FUNCTION lfvGenRolId
IF M_GENROLID = "N"
  *"Please make sure that the generated sequence"+
  *"number will not overlap existing Rolls ID numbers ."
  =gfModalGen('INM00288B00000','DIALOG','Rolls Id')
ELSE
  *"Please note that you will not be able to "+
  *"assign a number that already exist ."
  =gfModalGen('INM00289B00000','DIALOG')
ENDIF

*!**************************************************************************
*! Name      : lfvGenMAPo
*! Developer : WAB - Walid A. Wahab
*! Date      : 08/15/2000
*! Purpose   : Valid function of genereate material po manual 
*!             setting in the option grid.
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvGenMAPo()
*!**************************************************************************
*!*E500349,1 
*!**************************************************************************
FUNCTION lfvGenMAPo
IF M_GENMAPON = "N"
  *"Please make sure that the generated sequence"+
  *"number will not overlap existing Material PO numbers ."
  =gfModalGen('INM00288B00000','DIALOG','Material Purchase Order ')
ELSE
  *"Please note that you will not be able to "+
  *"assign a number that already exist ."
  =gfModalGen('INM00289B00000','DIALOG')
ENDIF
*:*************************************************************
*: Name       : lfvMFGOrdNo
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Valid function of 'Manual Material Manufacturing 
*:            : Rework order no.' setting in the option grid. 
*:            : (key field)
*: REF        : 200254,1
*:*************************************************************
*: Calls      : 
*:            : Procedures : None
*:            :---------------------------
*:            : FUNCTION   : gfModalGen()
*:*************************************************************
*: Passed Parameters  :  None
*:*************************************************************
*: Returns            :  None
*:*************************************************************
*: Example            :  =lfvGENRWNU()
*:*************************************************************
*:
FUNCTION lfvGENRWNU
IF M_GENRWNUM = "N"
  *- Message Text   :- Please make sure that the generated sequence
  *- Message Text   :- number will not overlap existing rework order numbers .
  *- Message No.    :- 00288.
  *- Buttom Message :- Ok.
  *- Buttom Number  :- 00000.
  =gfModalGen('INM00288B00000','DIALOG','Rework Order')
ELSE
  *- Message Text   :- Please note that you will not be able to
  *- Message Text   :- assign a number that already exist.
  *- Message No.    :- 00289.
  *- Buttom Message :- Ok.
  *- Buttom Number  :- 00000.
  =gfModalGen('INM00289B00000','DIALOG')
ENDIF

*-- End of 
************************************************************
*! Name      : lfvGetLink
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/08/2013
*! Purpose   : lfvGetLink < the function is missed from the ma.prg>
************************************************************
FUNCTION lfvGetLink

*- End of lfvGetLink.
*!**************************************************************************
*! Name      : lfvGENCTNU
*! Developer : Ahmed Mohammed
*! Date      : 02/12/1998
*! Purpose   : Valid function of 'Manual cut ticket no.' setting in the option grid.
*! REF       : *B601911,1
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : gfModalGen()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvGENCTNU()
*!**************************************************************************
FUNCTION lfvGENCTNU
*B601911,1 If generating sequence c\t numbers 
IF M_GENCTNUM = "N"
  *B601911,1 AMM the message is "Please make sure that the generated sequence"+
  *B601911,1 AMM "number will not overlap existing Cutting Ticket numbers ."
  =gfModalGen('INM00288B00000','DIALOG','Cutting Ticket')
*B601911,1 If manually generating c\t numbers  
ELSE
  *B601911,1 AMM the message is "Please note that you will not be able to "+
  *B601911,1 AMM "assign a number that already exist ."
  =gfModalGen('INM00289B00000','DIALOG')
ENDIF

*!**************************************************************************
*! Name      : lfvGenPrj
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/04/2002
*! Purpose   : Validate the Generate project setting
*! REF       : E#301869
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvGenPrj()
*!**************************************************************************
*!E301869
FUNCTION lfvGenPrj

_CUROBJ = OBJNUM(M_MFDEFTMP)   

IF M_MFGENPRJ = 'M' 
  llRemDfTmp = .T.
ENDIF  


*!**************************************************************************
*! Name      : lfvGenStoN
*! Developer : Ahmed Mohammed
*! Date      : 02/12/1998
*! Purpose   : Valid function of the 'Enter style PO # manually'  setting in 
*!             the setting grid.
*! REF       : *B601911,1
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : gfModalGen()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvGenStoN()
*!**************************************************************************

FUNCTION lfvGenStoN
*B601911,1 If the users selects to generate a sequence number for orders
IF M_GENSTORN = "N"
  *B601911,1 AMM The message is "Please make sure that the generated sequence number will not overlap existing style Po numbers ."
  =gfModalGen('INM00288B00000','DIALOG','style PO')
*B601911,1 If the users selects to manually add order numbers
ELSE
  *B601911,1 AMM The message is "Please note that you will not be able to assign a number that already exist ."
  =gfModalGen('INM00289B00000','DIALOG')
ENDIF

*!**************************************************************************
*! Name      : lfvGenPrj
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/04/2002
*! Purpose   : Validate the Generate project setting
*! REF       : E#301869
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvGenPrj()
*!**************************************************************************
*!E301869
FUNCTION lfvGenPrj

_CUROBJ = OBJNUM(M_PODEFTMP)   

IF M_POGENPRJ = 'M' 
  llRemDfTmp = .T.
ENDIF  
*!*************************************************************
*! Name      : lfvAtomInt
*! Developer : Timour Abdalla
*! Date      : 09/17/00
*! Purpose   : Called from SMCMSET.PRG, Checks for the validity
*!             of the entered values in the setups grid
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : gfModalGen(),lfShowGet()
*!*************************************************************
FUNCTION lfvAtomInt

IF M_USEATOM = 'N' 
 M_ATOMDIR  = ' '
 M_ENTMETH  = 'M'
 M_ACTNTYPE = 'O'
 * disable the remainder items when deleting the link   
 =lfShowGet('M_ATOMDIR',.F.)
 =lfShowGet('M_ENTMETH',.F.)
 =lfShowGet('M_ACTNTYPE',.F.)
ELSE
 =lfShowGet('M_ATOMDIR',.T.)
 =lfShowGet('M_ENTMETH',.T.)
 =lfShowGet('M_ACTNTYPE',.T.)
ENDIF
=lfShowGet('M_USEATOM',.T.)
RETURN

*!*************************************************************
*! Name      : lfvAtomDir
*! Developer : Timour Abdalla
*! Date      : 09/17/00
*! Purpose   : Called from SMCMSET.PRG, Checks for the validity
*!             of the entered values in the setups grid
*!*************************************************************
FUNCTION lfvAtomDir

IF !EMPTY(M_ATOMDIR) AND FILE(M_ATOMDIR+'ATOM32.EXE')
  RETURN
ENDIF

M_ATOMDIR = GETDIR('','Atomic Software Directory')
IF EMPTY(M_ATOMDIR) OR ! FILE(M_ATOMDIR+'ATOM32.EXE')
  M_USEATOM = 'N'
  =lfvAtomInt()
ELSE
  M_ATOMDIR = ALLT(M_ATOMDIR)
ENDIF
RETURN
*E301467,1 End.
*:*************************************************************
*! Name     : lfvDefLoc
*! Developer: Abdou El-Gendy
*! Date     : 01/02/2001               
*! Purpose  : Vadidate the Def. warehouse in PS setup.
*! Refere   : B#604071.
*:*************************************************************
*! Calls     : 
*!           : Procedures : ....
*!           ; Functions  : gfOpenFile() , gfBrowWare() 
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvDefLoc()
*!*************************************************************

FUNCTION lfvDefLoc

IF !EMPTY(M_PSDEFLOC)
  lnAlias = SELECT()
  = gfOpenFile(gcDataDir+"WAREHOUS","WAREHOUS","SH")
  IF !SEEK(M_PSDEFLOC,'WAREHOUS')
    M_PSDEFLOC = gfBrowWare( .T. )
  ENDIF
  SELECT(lnAlias)
  SHOW GET M_PSDEFLOC
ENDIF
*-- End OF lfvDefLoc
*!*************************************************************

*!**************************************************************************
*! Name      : lfvGenOrNu
*! Developer : Ahmed Mohammed
*! Date      : 02/12/1998
*! Purpose   : Validation of the setting of "Enter the order number manually" in 
*!              the setting grid
*! REF       : *B601911,1
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : gfModalGen()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvGenOrNu()
*!**************************************************************************
FUNCTION lfvGenOrNu
*B601911,1 If generating sequence order numbers 
IF M_GENORNUM = "N"
  *B601911,1 AMM the message is "Please make sure that the generated sequence"+
  *B601911,1 AMM "number will not overlap existing order numbers ."
  =gfModalGen('INM00288B00000','DIALOG','sales order')
*B601911,1 If manually generating order numbers  
ELSE
  *B601911,1 AMM the message is "Please note that you will not be able to "+
  *B601911,1 AMM "assign a number that already exist ."
  =gfModalGen('INM00289B00000','DIALOG')
ENDIF
*!**************************************************************************
*! Name      : lfvGenPrj
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/04/2002
*! Purpose   : Validate the Generate project setting
*! REF       : E#301869
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvGenPrj()
*!**************************************************************************
*!E301869
FUNCTION lfvGenPrj

_CUROBJ = OBJNUM(M_SODEFTMP)   

IF M_SOGENPRJ = 'M' 
  llRemDfTmp = .T.
ENDIF  


*!*************************************************************
*! Name      : lfvHistCmp
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/23/2000
*! Purpose   : Valid function of history company code.
*! Reference : *E301469,1 AMH
*!*************************************************************
*! Called from : SMCMSET.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvHistCmp()
*!*************************************************************

FUNCTION lfvHistCmp

DO msg
IF llHist .AND. !EMPTY(M_Comp_Id)
  IF oAriaApplication.ActiveCompanyID == M_Comp_Id
    =gfModalGen("TRM00000B00000","DIALOG","Company Information",.F.,;
                "The active company cannot be the current one.")
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF
  IF M_Comp_Id == loFormSet.laData[1]
    =gfModalGen("TRM00000B00000","DIALOG","Company Information",.F.,;
                "This is a History Company,"+;
                "select another company to be the Active Company.")
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF
  lnAlias = SELECT(0)
  *E303339,1 TMI 01/14/2013 [Start] 
  lfBuff('SYCCOMP')
  *E303339,1 TMI 01/14/2013 [End  ] 
  SELECT SYCCOMP
  SET ORDER TO 1
  lnRec = RECNO()
  SEEK loFormSet.laData[1]
  lcTrgDir = ALLTRIM(CCOM_DDIR)
  IF SEEK(M_Comp_Id)
    IF !GfGetMemVar('LLHIST',M_Comp_Id) .AND. EMPTY(GfGetMemVar('M_Comp_Id',M_Comp_Id))
      lcMsg = "You are going to make the company " + loFormSet.laData[1] + " a history company for " +;
              "company " + M_COMP_ID + " . This will remove all data in company " + loFormSet.laData[1] +;
              " ! Are you sure you want to continue ?"
      IF gfModalGen("TRM00000B00006","DIALOG","Company Information",.F.,lcMsg) = 2
        LLHIST = .F.
        M_COMP_ID = ''
        *E303339,1 TMI 01/14/2013 [Start] 
        **B604095,1 AMH 01/08/2001 Enable all Setups before lfOGRShow [Start]
        *lnI = 1
        *FOR lnI = 1 TO ALEN(laOGObjType,1)
        *  =lfShowGet(laOGObjType[lnI,1],.T.)
        *ENDFOR
        **B604095,1 AMH [End]
        *=lfOGRShow()
        *E303339,1 TMI 01/14/2013 [End  ] 
        IF lnRec > 0 .AND. lnRec <= RECCOUNT()
          GOTO RECORD lnRec
        ENDIF
        SELECT (lnAlias)
        RETURN
      ENDIF
      lcSorDir = ALLTRIM(CCOM_DDIR)
      lcCompCod = loFormSet.laData[1]
      lcScFields = loFormSet.lcScFields
      SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData
      loFormSet.laData[1] = lcCompCod
      loFormSet.laData[2] = 'History of ' + loFormSet.laData[2]
      loFormSet.laData[11] = lcTrgDir
      loFormSet.laData[15] = STRTRAN(loFormSet.laData[15],'GL|','')
      loFormSet.laData[15] = STRTRAN(loFormSet.laData[15],'GL,','')
      loFormSet.laData[15] = STRTRAN(loFormSet.laData[15],'GL','')
      lcCurModul = STRTRAN(MMODLSET,'GL|','')
      lcCurModul = STRTRAN(lcCurModul,'GL,','')
      lcCurModul = STRTRAN(lcCurModul,'GL','')
      SEEK loFormSet.laData[1]
      REPLACE mModlSet  WITH lcCurModul
      *-- copy modules files.
      IF USED('SETUPS')
        USE IN SETUPS
      ENDIF
      IF USED('ACCOD')
        USE IN ACCOD
      ENDIF
      IF USED('FISHD')
        USE IN FISHD
      ENDIF
      IF USED('FSHLD')
        USE IN FSHLD
      ENDIF
      IF USED('FSPRD')
        USE IN FSPRD
      ENDIF
      IF USED('ICISTRU')
        USE IN ICISTRU
      ENDIF
      IF USED('GLSETUP')
        USE IN GLSETUP
      ENDIF
      IF USED('APSETUP')
        USE IN APSETUP
      ENDIF
      IF USED('GLACCHAR')
        USE IN GLACCHAR
      ENDIF
      DECLARE laInstModl[1]
      =gfSubStr(loFormSet.laData[15],@laInstModl,'|')
      lnI = 1
      FOR lnI = 1 TO ALEN(laInstModl,1)
        =lfInstall(laInstModl[lnI])
      ENDFOR
      *-- copy setups file.
      lcActSetup = gfTempName()
      lcHstSetup = gfTempName()
      =gfOpenFile(lcSorDir+'setups','modvar','SH',@lcActSetup,.T.)
      =gfOpenFile(lcTrgDir+'setups','modvar','SH',@lcHstSetup,.T.)
      SELECT (lcActSetup)
      LOCATE
      SCAN
        SCATTER MEMO MEMVAR
        IF !SEEK(EVALUATE(KEY()),lcHstSetup)
          SELECT (lcHstSetup)
          APPEND BLANK
        ENDIF
        SELECT (lcHstSetup)
        GATHER MEMO MEMVAR
        SELECT (lcActSetup)
      ENDSCAN
      IF SEEK (CHR(255)+CHR(255)+'M_COMP_ID',lcActSetup)
        SELECT (lcActSetup)
        REPLACE MDATA_DEF WITH loFormSet.laData[1]
      ELSE
        IF SEEK (CHR(255)+CHR(255)+SPACE(9)+'2','SYCCONFG')
          SELECT SYCCONFG
          SCATTER MEMO MEMVAR
          SELECT (lcActSetup)
          APPEND BLANK
          GATHER MEMO MEMVAR
          REPLACE MDATA_DEF WITH loFormSet.laData[1]
        ENDIF
      ENDIF
      =gfCloseFile(lcActSetup)
      IF SEEK (CHR(255)+CHR(255)+'M_COMP_ID',lcHstSetup)
        SELECT (lcHstSetup)
        REPLACE MDATA_DEF WITH M_COMP_ID
      ELSE
        IF SEEK (CHR(255)+CHR(255)+SPACE(9)+'2','SYCCONFG')
          SELECT SYCCONFG
          SCATTER MEMO MEMVAR
          SELECT (lcHstSetup)
          APPEND BLANK
          GATHER MEMO MEMVAR
          REPLACE MDATA_DEF WITH M_COMP_ID
        ENDIF
      ENDIF
      IF SEEK (CHR(255)+CHR(255)+'LLHIST',lcHstSetup)
        SELECT (lcHstSetup)
        REPLACE MDATA_DEF WITH IIF(llHist,'.T.','.F.')
      ELSE
        IF SEEK (CHR(255)+CHR(255)+SPACE(9)+'1','SYCCONFG')
          SELECT SYCCONFG
          SCATTER MEMO MEMVAR
          SELECT (lcHstSetup)
          APPEND BLANK
          GATHER MEMO MEMVAR
          REPLACE MDATA_DEF WITH IIF(llHist,'.T.','.F.')
        ENDIF
      ENDIF
      =gfCloseFile(lcHstSetup)
      =gfOpenFile(lcTrgDir+'setups','modvar','SH')
      *-- copy ICISTRU file.
      IF FILE(lcSorDir+'icistru.dbf')
        COPY FILE lcSorDir+'icistru.cdx' TO lcTrgDir+'icistru.cdx'
        COPY FILE lcSorDir+'icistru.dbf' TO lcTrgDir+'icistru.dbf'
      ENDIF
      *-- copy APSETUP file.
      IF FILE(lcSorDir+'apsetup.dbf')
        COPY FILE lcSorDir+'apsetup.dbf' TO lcTrgDir+'apsetup.dbf'
      ENDIF
      *-- copy GLSETUP file.
      IF FILE(lcSorDir+'glsetup.dbf')
        COPY FILE lcSorDir+'glsetup.dbf' TO lcTrgDir+'glsetup.dbf'
        COPY FILE lcSorDir+'glsetup.fpt' TO lcTrgDir+'glsetup.fpt'
        COPY FILE lcSorDir+'glsetup.fpt' TO lcTrgDir+'glsetup.CDX'
      ENDIF
      *-- copy GLACCHAR file.
      IF FILE(lcSorDir+'glacchar.dbf')
        COPY FILE lcSorDir+'glacchar.cdx' TO lcTrgDir+'glacchar.cdx'
        COPY FILE lcSorDir+'glacchar.dbf' TO lcTrgDir+'glacchar.dbf'
      ENDIF
      *-- copy account code file.
      COPY FILE lcSorDir+'accod.cdx' TO lcTrgDir+'accod.cdx'
      COPY FILE lcSorDir+'accod.dbf' TO lcTrgDir+'accod.dbf'
      *-- copy sequence file.
      COPY FILE lcSorDir+'sequence.cdx' TO lcTrgDir+'sequence.cdx'
      COPY FILE lcSorDir+'sequence.dbf' TO lcTrgDir+'sequence.dbf'
      *-- copy fiscal year files.
      COPY FILE lcSorDir+'fishd.cdx' TO lcTrgDir+'fishd.cdx'
      COPY FILE lcSorDir+'fishd.dbf' TO lcTrgDir+'fishd.dbf'
      COPY FILE lcSorDir+'fishd.fpt' TO lcTrgDir+'fishd.fpt'
      COPY FILE lcSorDir+'fshld.cdx' TO lcTrgDir+'fshld.cdx'
      COPY FILE lcSorDir+'fshld.dbf' TO lcTrgDir+'fshld.dbf'
      COPY FILE lcSorDir+'fsprd.cdx' TO lcTrgDir+'fsprd.cdx'
      COPY FILE lcSorDir+'fsprd.dbf' TO lcTrgDir+'fsprd.dbf'
      *E303339,1 TMI 01/14/2013 [Start] 
      *lnI = 1
      *FOR lnI = 1 TO ALEN(laOGObjType,1)
      *  =lfShowGet(laOGObjType[lnI,1],.F.)
      *ENDFOR
      *E303339,1 TMI 01/14/2013 [End  ] 
      SHOW GET pbOGReset DISABLE
      SHOW GET pbCancel DISABLE
      *--- swich to view mode for not hanging when use scroll bal
      laScrMode    = .F.
      laScrMode[2] = .T.
    ELSE
      IF !GfGetMemVar('LLHIST',M_Comp_Id)
        =gfModalGen("TRM00000B00000","DIALOG","companies",.F.,"This Company has a History Company already.")
      ELSE
        =gfModalGen("TRM00000B00000","DIALOG","companies",.F.,"This is a History Company.")
      ENDIF
      loOgScroll.ActiveControl.Value = loOgScroll.ActiveControl.OldValue
      M_COMP_ID = loOgScroll.ActiveControl.OldValue
      loOgScroll.ActiveControl.Refresh()

      _CUROBJ = _CUROBJ
    ENDIF
  ELSE
    =gfModalGen("TRM00000B00000","DIALOG","companies",.F.,"This company does not exist.")
    DO msg
    loOgScroll.ActiveControl.Value = loOgScroll.ActiveControl.OldValue
    M_COMP_ID = loOgScroll.ActiveControl.OldValue
    loOgScroll.ActiveControl.Refresh()
    _CUROBJ = _CUROBJ
  ENDIF
  SELECT (lnAlias)
ELSE
  LLHIST = .F.
  *B604095,1 AMH 01/08/2001 Enable all Setups before lfOGRShow [Start]
  *E303339,1 TMI 01/14/2013 [Start] 
  *lnI = 1
  *FOR lnI = 1 TO ALEN(laOGObjType,1)
  *  =lfShowGet(laOGObjType[lnI,1],.T.)
  *ENDFOR
  **B604095,1 AMH [End]
  *=lfOGRShow()
  *E303339,1 TMI 01/14/2013 [End  ] 
ENDIF
*--- end of lfvHistCmp.

************************************************************
*! Name      : gfCloseFile
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/10/2013
*! Purpose   : as a replacement of the a27 function gfCloseFile
************************************************************
FUNCTION gfCloseFile
PARAMETERS lcAlias
IF USED(lcAlias)
  USE IN (lcAlias)
ENDIF   

*- End of gfCloseFile.
*!*************************************************************
*! Name      : lfvllHist
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/23/2000
*! Purpose   : Valid function of history company.
*! Reference : *E301469,1 AMH
*!*************************************************************
*! Called from : SMCMSET.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvllHist()
*!*************************************************************
FUNCTION lfvllHist
DO msg
IF oAriaApplication.ActiveCompanyID == loFormSet.laData[1]
  =gfModalGen("TRM00000B00000","DIALOG","Company Information",.F.,;
              "You cannot select the current company to be a History Company.")
  LLHIST = .F.
  =lfShowGet(laOGObjType[ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'LLHIST'),1),1],.T.)
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
*E303339,1 TMI 01/14/2013 [Start] 
*lnI = 1
*FOR lnI = 1 TO ALEN(laOGObjType,1)
*  =lfShowGet(laOGObjType[lnI,1],.F.)
*ENDFOR
*E303339,1 TMI 01/14/2013 [End  ] 
=lfShowGet(laOGObjType[ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'LLHIST'   ),1),1],.F.)
=lfShowGet(laOGObjType[ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'M_COMP_ID'),1),1],.T.)
*--- end of lfvllHist.


*!**************************************************************************
*!
*!      Function: lfvMulCurr
*!
*!**************************************************************************
*
FUNCTION lfvMulCurr
IF LLMULCURR
  llMultCurr = .T.
  LLEXCHRATE = .T.
  LLSTYPRICE = .T.
  LLEDITEXRA = .F.
  LNEXRATDAY = 30
ELSE
  llMultCurr = .F.
  LLEXCHRATE = .F.
  LLSTYPRICE = .F.
  LLEDITEXRA = .F.
  LNEXRATDAY = 0
ENDIF
=lfShowGet('LLMULCURR',.T.)
=lfShowGet('LLEXCHRATE',llMultCurr)
=lfShowGet('LLSTYPRICE',llMultCurr)
=lfShowGet('LLEDITEXRA',llMultCurr)
=lfShowGet('LNEXRATDAY',llMultCurr)
*B601012,1 Clear the exch rate diff account field and show according
*B601012,1 to the multi currency state (
LNEXRATACC = SPACE(24)
=lfShowGet('LNEXRATACC',llMultCurr)
*B601012,1 end.



************************************************************
*! Name      : MSG
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/05/2012
*! Purpose   : MSG, can run only on my PC
************************************************************
FUNCTION msg
PARAMETERS llDoNotUseStep
IF SYS(0)='DEV4 # tarek'
  ON ERROR
  _SCREEN.Visible=.T.
  IF !llDoNotUseStep
    IF FILE('C:\TEMP\X.X')
      SET STEP ON
    ENDIF 
  ENDIF 
ENDIF 
*- End of MSG.

