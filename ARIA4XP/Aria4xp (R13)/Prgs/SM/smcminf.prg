*:************************************************************************
*:  Program File: :\ARIA4XP\PRGS\SM\SMCMINF.Prg
*:  Module      : System Manager
*:  Desc.       : SM Setups screen
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 12/31/2012
*:  Reference   : *E303339,1 TMI
*:************************************************************************
* Modifications
*B610292,1 the creation of files will depneds on only one dictionary folder, so we need to differentiate between A27 and A40 files TMI 04/07/2013 [End  ]
*E303296 TMI 06/17/2013 [media] add the user id to the database name
*B610447,1 TMI 07/25/2013 fix problems in fiscal year screen
*B610472,1 TMI 08/20/2013 fix more problems for the information screen [T20130626.0001]
*B610493,1 TMI 09/03/2013 make the two fields of setuped and installed modules the [T20130902.0009 task]
*B610493,1 TMI 09/03/2013 fill in the INVTYPE file
*E303417,1 TMI 09/11/2013 Add deflfxault mandatory data to the newly created company [T20130915.0006(task)]
*B610518,1 TMI 09/16/2013 be sure that the history company exists in SYCCOMP [T20130915.0007(task)]
*B610519,1 TMI 09/17/2013 Do not check on CVER when creating FORMCDHD lines
*B610532,1 TMI 09/27/2013 be sure all open files are closed when doing the validation on company files[T20130926.0011 task]
*B610540,1 MMT 10/03/2013 Modify Company information screen to create FOX and SQL tables in the begining of the  creation process[Start]
*B610524,4 TMI 10/08/2013 add all lines to SETUPS file from the SYCCONFG with default values [T20130919.0007 TASK]
*B610524,4 TMI            comment the messagebox asking for adding fiscal year, account code structure
*B610524,4 TMI            call the wizard screen after the STYLE CODE structure screen
*B610589,1 TMI 11/14/2013 when creating a new company add a line with scale as '*' in SCALE file, issue 13 [T20131114.0004 task]
*B610591,1 TMI 11/18/2013 as there is no action of deleting a company, just comment the code that tries to open SYUUSRPR table [T20130916.0017]
*B610661,1 SAB 01/26/2014 Fix company information screen to save SMTP port without decimal places [T20131118.0006]
*E303495,1 MMT 07/31/2014 Apply SSO changes in Company information screen
*B611043,1 MMT 08/24/2015 Company information saved phone & fax in incorrect format[T20150219.0030]
*E303694,1 MMT 07/18/2016 Add email address to company information screen and invoice form A[T20160630.0009]
*E303949,1 MMT 04/03/2018 Modify the Company setups for the changes of [P20171130.0001]
*B611650,1 HMS 08/27/2018 GL - Unable to print TB report (Dash Clothing Inc) [T20180611.0026]
*B611946,1 Es 10/31/2019 Aria4XP Company information screen:Error 'Access denied to SQL DB' while creating new company 	[T20191027.0003]
*E611985,1 ES 29/12/2019 Company information changes to create new company as a copy of an existing company. [T20191022.0002]
*E612125,1 Es 04/22/2020 Create New Company enhancment [T20200408.0001]
*B612488,1 MMT 11/10/2021 Company information screen gives an error while creating new company copy from existing company if the SQL server folder is inaccessible from the data server[T20211027.0002]
*B612494,1 MMT 11/22/2021 Sometimes Syccomp record left locked after editing company from company information screen[T20211011.0001]
*E612505,1 MMT 03/01/2022 Change the Email setting option in option grid to be a screen[T20211208.0002]
*B612559,1 MMT 05/09/2022 Copying company from another company using company information screen does not copy the SQL table structure correctly[T20220427.0001]
*B612666,1 MMT 03/15/2023 Create Entity related tables while creating new company[T20230216.0001]
*:************************************************************************
*- Get the screen , call it
#INCLUDE R:\Aria4xp\Screens\SM\SMCMINF.H

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
lcPath = oAriaApplication.ApplicationHome
SET PROCEDURE TO (lcPath+'AP\APMAIN.FXP') ADDITIVE
SET PROCEDURE TO (lcPath+'GL\GL.FXP') ADDITIVE

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
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.BrowseTitle            = LANG_COMPANY_INFORMATION && Company Information
	.BrowseTitle            = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COMPANY_INFORMATION,loFormSet.GetHeaderText("LANG_COMPANY_INFORMATION",loFormSet.HeaderAlias)) && Company Information
*N000682,1 11/20/2012 MMT Globlization changes[End]


*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.lcFile_Ttl = LANG_COMPANY_INFORMATION
	.lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COMPANY_INFORMATION,loFormSet.GetHeaderText("LANG_COMPANY_INFORMATION",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

	.ariaBrFields.edtBrowseFields.Value = .GetBrowseFields(.lcBaseFile)
ENDWITH

*- Define several variables needed in the form
=lfDefineVars(loFormSet)


*E611985,1 ES 01/02/2020 Company information changes to create new company as a copy of an existing company. [Start]
DECLARE loFormSet.laPanelObj[1,6]
STORE '' TO loFormSet.laPanelObj
loFormSet.laPanelObj[1,1] = 'pbRefresh'
loFormSet.laPanelObj[1,2] = oAriaApplication.BitMapHome+"RESET.bmp"
loFormSet.laPanelObj[1,3] = 'lfRefereshData'
loFormSet.laPanelObj[1,4] = LANG_BRefresh_Name
loFormSet.laPanelObj[1,5] =  LANG_BRefresh_Data
loFormSet.laPanelObj[1,6] = 'V'
*E611985,1 ES 01/02/2020 Company information changes to create new company as a copy of an existing company. [End]



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

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.AddProperty('lcTSelect'   , LANG_lcTSelect) && Hold the word "S\<elect"
loFormSet.AddProperty('lcTSelect'   , IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_lcTSelect,loFormSet.GetHeaderText("LANG_lcTSelect",loFormSet.HeaderAlias))) && Hold the word "S\<elect"
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.AddProperty('lcTUnSelct'  , LANG_lcTUnSelct) && Hold the word "\<Unselect"
loFormSet.AddProperty('lcTUnSelct'  , IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_lcTUnSelct,loFormSet.GetHeaderText("LANG_lcTUnSelct",loFormSet.HeaderAlias))) && Hold the word "\<Unselect"
*N000682,1 11/20/2012 MMT Globlization changes[End]


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
*E303694,1 MMT 07/18/2016 Add email address to company information screen and invoice form A[T20160630.0009][Start]
lcScFields = lcScFields +  ',cemail_add'
*E303694,1 MMT 07/18/2016 Add email address to company information screen and invoice form A[T20160630.0009][End]
loFormSet.AddProperty('lcScFields',lcScFields)
lcLn = ALLTRIM(STR(OCCURS(',',lcScFields)+1))
loFormSet.AddProperty('laData[&lcLn.]')
SELECT (loFormSet.lcBaseFile)
SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData BLANK

SELECT SYCINT
SELECT DISTINCT SYCINT.ccont_desc,SYCINT.ccont_code ;
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


*N000682,1 04/17/2013 RAS Globalization[START]

*!*	  .laMod[1 ] = 'SM-Company Setups              '
*!*	  .laMod[2 ] = 'IC-Inventory Control Setup     '
*!*	  .laMod[3 ] = 'AL-Sales Order Allocation Setup'
*!*	  .laMod[4 ] = 'AR-Accounts Receivable Setup   '
*!*	  .laMod[5 ] = 'CR-CRP Module Setup            '
*!*	  .laMod[6 ] = 'EB-EDI Base Setup              '
*!*	  .laMod[7 ] = 'MA-Material Setup              '
*!*	  .laMod[8 ] = 'MF-Manufacturing Setup         '
*!*	  .laMod[9 ] = 'NC-NC module Setup             '
*!*	  .laMod[10] = 'PO-Style Purchase Order Setup  '
*!*	  .laMod[11] = 'PS-Point of Sale Setup         '
*!*	  .laMod[12] = 'PW-Piece Work Setup            '
*!*	  .laMod[13] = 'SO-Sales Order Setup           '
*!*	  .laMod[14] = 'SP-SP module Setup             '
*!*	  .laMod[15] = 'SR-Sales Representative Setup  '
*!*	  .laMod[16] = 'UP-Universal Product Code Setup'

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.laMod[1 ] = LANG_SM-Company_Setups
	.laMod[1 ] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SM_Company_Setups,loFormSet.GetHeaderText("LANG_SM_Company_Setups",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.laMod[2 ] = LANG_IC-Inventory_Control_Setup
	.laMod[2 ] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_IC_Inventory_Control_Setup,loFormSet.GetHeaderText("LANG_IC_Inventory_Control_Setup",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.laMod[3 ] = LANG_AL-Sales_Order_Allocation_Setup
	.laMod[3 ] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AL_Sales_Order_Allocation_Setup,loFormSet.GetHeaderText("LANG_AL_Sales_Order_Allocation_Setup",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.laMod[4 ] = LANG_AR-Accounts_Receivable_Setup
	.laMod[4 ] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AR_Accounts_Receivable_Setup,loFormSet.GetHeaderText("LANG_AR_Accounts_Receivable_Setup",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.laMod[5 ] = LANG_CR-CRP_Module_Setup
	.laMod[5 ] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CR_CRP_Module_Setup,loFormSet.GetHeaderText("LANG_CR_CRP_Module_Setup",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.laMod[6 ] = LANG_EB-EDI_Base_Setup
	.laMod[6 ] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_EB_EDI_Base_Setup,loFormSet.GetHeaderText("LANG_EB_EDI_Base_Setup",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.laMod[7 ] = LANG_MA-Material_Setup
	.laMod[7 ] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MA_Material_Setup,loFormSet.GetHeaderText("LANG_MA_Material_Setup",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.laMod[8 ] = LANG_MF-Manufacturing_Setup
	.laMod[8 ] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MF_Manufacturing_Setup,loFormSet.GetHeaderText("LANG_MF_Manufacturing_Setup",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.laMod[9 ] = LANG_NC-NC_module_Setup
	.laMod[9 ] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_NC_NC_module_Setup,loFormSet.GetHeaderText("LANG_NC_NC_module_Setup",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.laMod[10] = LANG_PO-Style_Purchase_Order_Setup
	.laMod[10] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PO_Style_Purchase_Order_Setup,loFormSet.GetHeaderText("LANG_PO_Style_Purchase_Order_Setup",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.laMod[11] = LANG_PS-Point_of_Sale_Setup
	.laMod[11] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PS_Point_of_Sale_Setup,loFormSet.GetHeaderText("LANG_PS_Point_of_Sale_Setup",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.laMod[12] = LANG_PW-Piece_Work_Setup
	.laMod[12] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PW_Piece_Work_Setup,loFormSet.GetHeaderText("LANG_PW_Piece_Work_Setup",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.laMod[13] = LANG_SO-Sales_Order_Setup
	.laMod[13] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SO_Sales_Order_Setup,loFormSet.GetHeaderText("LANG_SO_Sales_Order_Setup",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.laMod[14] = LANG_SP-SP_module_Setup
	.laMod[14] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SP_SP_module_Setup,loFormSet.GetHeaderText("LANG_SP_SP_module_Setup",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.laMod[15] = LANG_SR-Sales_Representative_Setup
	.laMod[15] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SR_Sales_Representative_Setup,loFormSet.GetHeaderText("LANG_SR_Sales_Representative_Setup",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.laMod[16] = LANG_UP-Universal_Product_Code_Setup
	.laMod[16] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_UP_Universal_Product_Code_Setup,loFormSet.GetHeaderText("LANG_UP_Universal_Product_Code_Setup",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/17/2013 RAS Globalization[End  ]
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
lnRemResult = oAriaApplication.RemoteSystemData.Execute(lcSelectStatement ,'',"SYDREPRT","",oAriaApplication.cAria4Sysfiles,3,"",SET("Datasession"))

*!*	lcSelectStatement = 'SELECT * FROM SYREPUVR WHERE CREP_ID = "SMAPP" Order By cExpType, nVarPos'
*!*	lnRemResult = oAriaApplication.RemoteSystemData.Execute(lcSelectStatement ,'',"SYREPUVR","",oAriaApplication.cAria4SysFiles,3,"",SET("Datasession"))

*lnCurrDataSession = SET("DATASESSION")
*-Create a new connection string the looks at a different path other than the SQLDICTIONARY, in that path get the syrepuvr with the lines updated
loFormSet.AddProperty( 'lcTempFldr' ,  oAriaApplication.WorkDir+gfTempName()+'\' )

*E611985,1 ES 29/12/2019 Company information changes to create new company as a copy of an existing company. [Start]
loFormSet.AddProperty('lcCopyFrom'    , "" ) && property for copy from company
*E611985,1 ES 29/12/2019 Company information changes to create new company as a copy of an existing company. [End]


MD (loFormSet.lcTempFldr)
SELECT SYDREPRT
COPY TO (loFormSet.lcTempFldr+'SYDREPRT')
USE IN SYDREPRT

USE (oAriaApplication.caria4syspath+'SYREPUVR')
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

	.laData17.KeyTextBox.ControlSource = 'Thisformset.laData[17]'

	.laData12.ControlSource = 'Thisformset.laData[12]'
	.laData13.ControlSource = 'Thisformset.laData[13]'

	.laData11.ControlSource = 'Thisformset.laData[11]'
*E303694,1 MMT 07/18/2016 Add email address to company information screen and invoice form A[T20160630.0009][Start]
	.laData18.ControlSource = 'Thisformset.laData[18]'
*E303694,1 MMT 07/18/2016 Add email address to company information screen and invoice form A[T20160630.0009][End]
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
*B611043,1 MMT 08/24/2015 Company information saved phone & fax in incorrect format[T20150219.0030][Start]
	loFormSet.laData[9] = TRANSFORM(loFormSet.laData[9], '@R ' + gfPhoneTem())
	loFormSet.laData[10] = TRANSFORM(loFormSet.laData[10], '@R ' + gfPhoneTem())
*B611043,1 MMT 08/24/2015 Company information saved phone & fax in incorrect format[T20150219.0030][end]
case loFormSet.ActiveMode $ 'S'
	SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData BLANK
ENDCASE

WITH loFormSet.Ariaform1
	.laData1.Enabled = loFormSet.ActiveMode $ 'S'
	.laData2.Enabled = loFormSet.ActiveMode $ 'AE'
	.laData17.Enabled = loFormSet.ActiveMode = 'A'
ENDWITH

DO CASE
CASE loFormSet.ActiveMode = 'S'

	SELECT (loFormSet.lcBaseFile)
	SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData BLANK

	loFormSet.Ariaform1.laData1.Enabled = .T.

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
	loFormSet.Ariaform1.laData1.KeyTextBox.SetFocus()
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


*E611985,1 ES 30/12/2019 Company information changes to create new company as a copy of an existing company. [Start]
	IF !EMPTY(sycComp.CSORCOMP)

		IF !USED('SYCCOMP1')
			=gfOpenTable(oAriaApplication.SysPath+"SYCCOMP",'CCOMP_ID','SH','SYCCOMP1')
		ENDIF
		SELECT SYCCOMP1
		IF gfSeek(sycComp.CSORCOMP,'SYCCOMP1')
			loFormSet.Ariaform1.txtSourceCompID.Value=sycComp.CSORCOMP
			loFormSet.Ariaform1.txtSourceCompName.Value=SYCCOMP1.cCom_name
		ENDIF
*E611985,1 ES 03/11/2020 Company information changes to create new company as a copy of an existing company. [Start]
		loFormSet.oToolBar.ChangeButtonStatus('pbRefresh','ENABLED')
	ELSE
		loFormSet.oToolBar.ChangeButtonStatus('pbRefresh','DISABLED')
*E611985,1 ES 03/11/2020 Company information changes to create new company as a copy of an existing company. [End]

	ENDIF
*E611985,1 ES 30/12/2019 Company information changes to create new company as a copy of an existing company. [End]

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
		loFormSet.otoolbar.cmdEdit.Enabled = .F.
	ENDIF
	loFormSet.Ariaform1.pbSetup.Enabled = .T.
	loFormSet.Ariaform1.pbInsSet.Enabled = .T.

	SELECT (loFormSet.lcBaseFile)
	SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData
*B611043,1 MMT 08/24/2015 Company information saved phone & fax in incorrect format[T20150219.0030][Start]
	loFormSet.laData[9] = TRANSFORM(loFormSet.laData[9], '@R ' + gfPhoneTem())
	loFormSet.laData[10] = TRANSFORM(loFormSet.laData[10], '@R ' + gfPhoneTem())
*B611043,1 MMT 08/24/2015 Company information saved phone & fax in incorrect format[T20150219.0030][End]

*E303339,1 TMI 01/10/2013 [Start]
	=SEEK(loFormSet.laData[17],'SYCCURR')
	loFormSet.Ariaform1.txtCurrency.Value = SYCCURR.CCURRDESC
*E303339,1 TMI 01/10/2013 [End  ]




CASE loFormSet.ActiveMode = 'E'

	SELECT sycComp
	CURSORSETPROP("Buffering",5)

	loFormSet.lcCurrStat = IIF(EMPTY(loFormSet.laData[17]),'ENABLE','DISABLE')
	loFormSet.lnComRec   = RECNO()

	loFormSet.lcPrntName = IIF(EMPTY(loFormSet.laData[14]),'',;
	LOOKUP(sycComp.cCom_name ,ALLTRIM(loFormSet.laData[14]),;
	sycComp.cComp_id,'cComp_id'))

	lcDataDir  = gfGetDataDir(ALLTRIM(IIF(EMPTY(loFormSet.laData[14]), loFormSet.laData[11], sycComp.cCom_DDir)))

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
	loFormSet.laData[8]  = LOOKUP(SYCINT.ccont_desc,oAriaApplication.defaultcountry,SYCINT.ccont_code,'CCONTCODE')
	loFormSet.laData[16] = oAriaApplication.defaultcountry
	loFormSet.laData[17] = LOOKUP(SYCINT.cCurrCode,oAriaApplication.defaultcountry,SYCINT.ccont_code,'CCONTCODE')

*E303339,1 TMI 01/10/2013 [Start]
	=SEEK(loFormSet.laData[17],'SYCCURR')
	loFormSet.Ariaform1.txtCurrency.Value = SYCCURR.CCURRDESC
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

	GO TOP IN sycComp
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
	loFormSet.Ariaform1.pbSetup.Enabled = .F.
	loFormSet.Ariaform1.pbInsSet.Enabled = .F.

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
*B610333,1 fix media R13 issues TMI 05/19/13 [Start] if '?'keyed issue the browse
if '?' $ loFld.KeyTextBox.Value
	loFld.Selectedfrombrowse  = .T.
endif
*B610333,1 fix media R13 issues TMI 05/19/13 [End  ]
IF loFld.Selectedfrombrowse

	=loFormSet.otoolbar.cmdFind.Click()

ELSE
	IF !EMPTY(loFormSet.laData[1]) AND !SEEK(loFld.KeyTextBox.Value,'SYCCOMP')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lnResp = gfModalGen('INM00001B02004','DIALOG',LANG_COMPANY_ID+loFormSet.laData[1])
		lnResp = gfModalGen('INM00001B02004','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COMPANY_ID,loFormSet.GetHeaderText("LANG_COMPANY_ID",loFormSet.HeaderAlias))+loFormSet.laData[1])
*N000682,1 11/20/2012 MMT Globlization changes[End]
		DO Case
		CASE lnResp = 1
			IF !loFormSet.otoolbar.cmdFind.Click()
				loFormSet.laData[1] = ''
				loFld.KeyTextBox.Value = ''
			ENDIF
		CASE lnResp = 2
*E611985,1 ES 29/12/2019 Company information changes to create new company as a copy of an existing company. [Start]
			lnSelOp =gfModalGen('QRM54060B00006','DIALOG')
			DO Case
			CASE lnSelOp = 1
				lcRunScx = lfGetScx("SM\smcpycmp.scx")
				DO FORM (lcRunScx) WITH loFormSet
			ENDCASE
*E611985,1 ES 29/12/2019 Company information changes to create new company as a copy of an existing company. [End]
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

		lcDataDir  = gfGetDataDir(ALLTRIM(sycComp.cCom_DDir))

		=lfOpnFiles('ACCOD','ACCSEGNO')
		IF IIF(AT('?',loFormSet.laData[14]) > 0 ,!SEEK('','ACCOD'),.F.)
** This company does not have an account **
** code structure.  You cannot choose it **
** as a parent company...
** <  Ok  > **
			=gfModalGen("TRM00198B00000","DIALOG")
			loFormSet.laData[14] = loFormSet.lcOldPrnt
			SHOW GET loFormSet.laData[14]
			SELECT sycComp
			RETURN
		ENDIF
		loFormSet.lcPrntName = sycComp.cCom_name
	ELSE
		SELECT sycComp
		SET FILTER TO EMPTY(sycComp.cCompPrnt) .AND. ;
		sycComp.cComp_id <> loFormSet.laData[1]

		IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
			GO RECNO(0)
		ELSE
			GO TOP
		ENDIF

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcBrFields    = "cComp_id :H='"+LANG_COMPANY_ID+"',cCom_name :H='"+LANG_Company_Name+"'"
		lcBrFields    = "cComp_id :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COMPANY_ID,loFormSet.GetHeaderText("LANG_COMPANY_ID",loFormSet.HeaderAlias))+"',cCom_name :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Company_Name,loFormSet.GetHeaderText("LANG_Company_Name",loFormSet.HeaderAlias))+"'"
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFile_ttl    = LANG_Company_information
		lcFile_Ttl    = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COMPANY_INFORMATION,loFormSet.GetHeaderText("LANG_Company_information",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

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
			lcDataDir  = gfGetDataDir(ALLTRIM(sycComp.cCom_DDir))

			=lfOpnFiles('ACCOD','ACCSEGNO')
			IF !SEEK("",'ACCOD')
** This company does not have an account **
** code structure.  You cannot choose it **
** as a parent company...
** <  Ok  > **
				=gfModalGen("TRM00198B00000","DIALOG")
				loFormSet.laData[14] = loFormSet.lcOldPrnt
				loFormSet.lcPrntName = loFormSet.lcOldPrnN
				SELECT sycComp
			ENDIF
		ELSE
			loFormSet.laData[14] = loFormSet.lcOldPrnt
		ENDIF
	ENDIF
ELSE
	loFormSet.laData[14] = loFormSet.lcOldPrnt
ENDIF

SELECT sycComp
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

*E611985,1 ES 30/12/2019 Company information changes to create new company as a copy of an existing company. [Start]

IF !EMPTY(loFormSet.lcCopyFrom)
	lcSourceDB=lfGetDbName(loFormSet.lcCopyFrom)
	IF !lfSizeCheck(allt(oAriaApplication.ReadXML()),loFormSet.laData[1],loFormSet.lcCopyFrom,lcSourceDB)
		=gfModalGen("TRM54061B00000",'DIALOG')
		RETURN .F.
	ENDIF
ENDIF
*E611985,1 ES 30/12/2019 Company information changes to create new company as a copy of an existing company. [End]

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
SELECT sycComp
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
	USE IN sycComp
	lfBuff('SYCCOMP')
ENDIF
*E303339,1 TMI 01/13/2013 [End  ]

*E303339,1 TMI 01/13/2013 [Start] get the sql connection data for the active company
SET ORDER TO cComp_id IN sycComp
=SEEK(oAriaApplication.ActiveCompanyID,'SYCCOMP','CCOMP_ID')
DIMENSION laConnInfo[5]
SELECT sycComp
SCATTER FIELDS CCONDRIVER,CCONSERVER,CCONDBNAME,CCONUSERID,CCONPASWRD TO laConnInfo
*E303296 TMI 06/17/2013 [Start] add the user id to the database name
*laConnInfo[3] = loFormSet.laData[1]
*E303495,1 MMT 07/31/2014 Apply SSO changes in Company information screen[Start]
IF !FILE(ADDBS(oAriaApplication.DefaultPath)+"Azure.txt")
*E303495,1 MMT 07/31/2014 Apply SSO changes in Company information screen[End]
	laConnInfo[3] = allt(oAriaApplication.ReadXML())+'_LDB'+loFormSet.laData[1]
*E303495,1 MMT 07/31/2014 Apply SSO changes in Company information screen[Start]
ENDIF
*E303495,1 MMT 07/31/2014 Apply SSO changes in Company information screen[End]
*E303296 TMI 06/17/2013 [End  ]

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
*B611043,1 MMT 08/24/2015 Company information saved phone & fax in incorrect format[T20150219.0030][Start]
loFormSet.laData[9] = STRTRAN(STRTRAN(loFormSet.laData[9],"/",""),"-","")
loFormSet.laData[10] = STRTRAN(STRTRAN(loFormSet.laData[10],"/",""),"-","")
*B611043,1 MMT 08/24/2015 Company information saved phone & fax in incorrect format[T20150219.0030][End]

GATHER FROM loFormSet.laData FIELDS &lcScFields MEMO

IF loFormSet.ActiveMode = 'A'

*!*	  *B610447,1 fix problems in INFORMATION screen TMI 07/21/2013 [Start] if llGetAdminPriv is .T., replace current user and pw with administrator ones
*!*	  if laConnInfo[1] = 'SQL'
*!*	  private lcUid,lcPwd
*!*	  store ' ' to lcUid,lcPwd
*!*	  do form (oAriaApplication.ScreenHome+'SY\SYADMNPRV')
*!*	  if empt(lcUid) or empty(lcPwd)
*!*	    gfModalGen('INM00000B00000',.F.,.F.,.F.,'User ID or Password  can not be empty')
*!*	    return .F.
*!*	  EndIF
*!*	  laConnInfo[4] = allt(lcUid)
*!*	  laConnInfo[5] = allt(lcPwd)
*!*	  endif
*!*	  *B610447,1 fix problems in INFORMATION screen TMI 07/21/2013 [End  ]

	SELECT sycComp
	GATHER FIELDS CCONDRIVER,CCONSERVER,CCONDBNAME,CCONUSERID,CCONPASWRD FROM laConnInfo



	SELECT sycComp
	REPLACE lrunfroma4 WITH .T.
*E611985,1 ES 30/12/2019 Company information changes to create new company as a copy of an existing company. [Start]
	REPLACE  CSORCOMP WITH loFormSet.lcCopyFrom

	IF !EMPTY(loFormSet.lcCopyFrom)
		IF !USED('SYCCOMP1')
			=gfOpenTable(oAriaApplication.SysPath+"SYCCOMP",'CCOMP_ID','SH','SYCCOMP1')
		ENDIF
		SELECT SYCCOMP1
		IF gfSeek(loFormSet.lcCopyFrom,'SYCCOMP1')
			lcmmodlset=SYCCOMP1.mmodlset
			SELECT sycComp
			REPLACE  mmodlset WITH lcmmodlset
		ENDIF
	ENDIF

*E611985,1 ES 30/12/2019 Company information changes to create new company as a copy of an existing company. [End]
	=gfTableUpdate()
	=gfAdd_Info('sycComp')

*E303339,1 TMI 01/13/2013 [Start] set the company id to the current company to activate the check of the connection screen
	LOCAL lcCompanyID , lcCompanyName
	lcCompanyID = oAriaApplication.ActiveCompanyID
	lcCompanyName = oAriaApplication.ActiveCompanyName

*E303417,1 TMI 09/11/2013 [Start] Define property 'lCompanyIsBeingCreated', if exists and true then don't continue running lfChkCoDat function
	IF TYPE('oAriaApplication.lCompanyIsBeingCreated')='U'
		oAriaApplication.AddProperty('lCompanyIsBeingCreated')
	ENDIF
	oAriaApplication.lCompanyIsBeingCreated = .T.
*E303417,1 TMI 09/11/2013 [End  ]

	oAriaApplication.ActiveCompanyID = sycComp.cComp_id
	oAriaApplication.ActiveCompanyName = ALLTRIM(sycComp.cCom_name)
*E303339,1 TMI 01/13/2013 [End  ]

*B610447,1 07/21/2013 [Start] comment this
*x= oAriaApplication.activecompanyconstr
*B610447,1 07/21/2013 [End  ]
*E303495,1 MMT 07/31/2014 Apply SSO changes in Company information screen[Start]
*IF !oAriaApplication.RemoteCompanyData.mcreatedatabase(loFormSet.laData[1])
	IF !IIF(FILE(ADDBS(oAriaApplication.DefaultPath)+"Azure.txt"),lfCreateSchema(allt(oAriaApplication.ReadXML()),;
		loFormSet.laData[1]),oAriaApplication.RemoteCompanyData.mcreatedatabase(loFormSet.laData[1]))
*E303495,1 MMT 07/31/2014 Apply SSO changes in Company information screen[End]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_CONVERT_DATABASE)
		=gfModalGen('INM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_DATABASE,loFormSet.GetHeaderText("LANG_CONVERT_DATABASE",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*B610447,1 07/21/2013 [Start]
*oAriaApplication.ActiveCompanyId = lcCompanyID
*oAriaApplication.ActiveCompanyName = lcCompanyName
*B610447,1 07/21/2013 [End  ]

		SELECT sycComp
		SEEK loFormSet.laData[1]
		DELETE
		=gfTableUpdate()

		RETURN .F.
	ENDIF
*B610447,1 07/21/2013 [Start]
*oAriaApplication.ActiveCompanyId = lcCompanyID
*oAriaApplication.ActiveCompanyName = lcCompanyName
*B610447,1 07/21/2013 [End  ]
ENDIF

*B610447,1 07/21/2013 [Start]
SELECT sycComp
SEEK loFormSet.laData[1]
*B610447,1 07/21/2013 [End  ]
*E303495,1 MMT 07/31/2014 Apply SSO changes in Company information screen[Start]
IF FILE(ADDBS(oAriaApplication.DefaultPath)+"Azure.txt")
	REPLACE CCONUSERID WITH allt(oAriaApplication.ReadXML())+'_U'+loFormSet.laData[1]
	gfTableUpdate()
ENDIF
*E303495,1 MMT 07/31/2014 Apply SSO changes in Company information screen[End]
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

*B610292,1 the creation of files will depneds on only one dictionary folder TMI 04/07/2013 [Start]
*lcSQLDICPATH = ADDBS(lcSQLDICPATH)
	lcSQLDICPATH = oAriaApplication.SysPath
*B610292,1 the creation of files will depneds on only one dictionary folder TMI 04/07/2013 [End  ]
	IF USED('SYDFILES')
		USE IN sydfiles
	ENDIF
	USE lcSQLDICPATH +'SYDFILES' SHARED IN 0 ORDER 1
	USE lcSQLDICPATH +'SYDFILES' SHARED IN 0 ALIAS 'UPDATEFLS' AGAIN ORDER 1
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [Start]
	SELECT UPDATEFLS
	SET FILTER TO !EMPTY(ALLTRIM(cfile_nam))
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [End]

	IF USED('SYDFLFLD')
		USE IN 'SYDFLFLD'
	ENDIF
	USE lcSQLDICPATH +'SYDFLFLD' SHARED IN 0 ORDER cfile_nam
	IF USED('SYDFIELD')
		USE IN 'SYDFIELD'
	ENDIF
	USE lcSQLDICPATH +'SYDFIELD' SHARED IN 0 ORDER CFLD_NAME
*B610532,1 TMI 09/29/2013 [Start] set filt to A40 for sql tables
	SELECT SYDFIELD
	SET FILTER TO cver = '  ' OR cver = 'A40'
	LOCATE
*B610532,1 TMI 09/29/2013 [End  ]
	IF USED('SYDINDEX')
		USE IN 'SYDINDEX'
	ENDIF
	USE lcSQLDICPATH +'SYDINDEX' SHARED IN 0 ORDER CFILE
*B610540,1 MMT 10/03/2013 Modify Company information screen to create FOX and SQL tables in the begining of the  creation process[Start]
*  =updatetablestructure (oAriaApplication.SysPath,.F.,'SYDFILES','SYDFLFLD' ,'SYDFIELD' ,'SYDINDEX','UPDATEFLS')
	IF !updatetablestructure (oAriaApplication.SysPath,.F.,'SYDFILES','SYDFLFLD' ,'SYDFIELD' ,'SYDINDEX','UPDATEFLS')
		USE IN 'SYDFILES'
		USE IN 'SYDFLFLD'
		USE IN 'SYDFIELD'
		USE IN 'SYDINDEX'
		USE IN 'UPDATEFLS'
		lcNewComp =loFormSet.laData[1]
		oAriaApplication.ActiveCompanyID = lcCompanyID
		oAriaApplication.ActiveCompanyName=lcCompanyName
		loFormSet.UNDO(.T.)
		SELECT sycComp
		IF SEEK(lcNewComp)
			DELETE
			=gfTableUpdate()
		ENDIF
		loFormSet.Ariaform1.laData1.KeyTextBox.OldValue = space(2)
		RETURN .F.
*E303495,1 MMT 07/31/2014 Apply SSO changes in Company information screen[Start]
	ELSE
		oAriaApplication.ActiveCompanyID = loFormSet.laData[1]
*E303495,1 MMT 07/31/2014 Apply SSO changes in Company information screen[End]
	ENDIF
*B610540,1 MMT 10/03/2013 Modify Company information screen to create FOX and SQL tables in the begining of the  creation process[End]
	USE IN 'SYDFILES'
	USE IN 'SYDFLFLD'
	USE IN 'SYDFIELD'
	USE IN 'SYDINDEX'
	USE IN 'UPDATEFLS'
*B611946,1 Es 10/31/2019 Aria4XP Company information screen:Error 'Access denied to SQL DB' while creating new company 	[Start]
*!*	ENDIF
	lcCompIdOldConnStr = oAriaApplication.activecompanyconstr
	oAriaApplication.REF4.activecompanyconstr = "Driver={SQL Server};server="+ALLTRIM(laConnInfo[2])+";DATABASE="+ALLTRIM(laConnInfo[3])+;
	";uid="+ALLTRIM(laConnInfo[4])+";pwd="+ALLTRIM(laConnInfo[5])
	oAriaApplication.REF5.activecompanyconstr = "Driver={SQL Server};server="+ALLTRIM(laConnInfo[2])+";DATABASE="+ALLTRIM(laConnInfo[3])+;
	";uid="+ALLTRIM(laConnInfo[4])+";pwd="+ALLTRIM(laConnInfo[5])
*B611946,1 Es 10/31/2019 Aria4XP Company information screen:Error 'Access denied to SQL DB' while creating new company 	[End]

	*B612666,1 MMT 03/15/2023 Create Entity related tables while creating new company[T20230216.0001][Start]
	lnConnHEnt = SQLSTRINGCONNECT(oAriaApplication.REF4.activecompanyconstr)
	IF lnConnHEnt > 0
	  IF FILE(ADDBS(oAriaApplication.DefaultPath)+"EntityTables.sql")
	    lnExEnt = SQLEXEC(lnConnHEnt ,STRTRAN(SUBSTR(FILETOSTR(ADDBS(oAriaApplication.DefaultPath)+"EntityTables.sql"),3),CHR(0),''))
	    IF lnExEnt >0 AND  FILE(ADDBS(oAriaApplication.DefaultPath)+"UpdateEntity.sql")
	       lnExEntUp = SQLEXEC(lnConnHEnt ,STRTRAN(SUBSTR(FILETOSTR(ADDBS(oAriaApplication.DefaultPath)+"UpdateEntity.sql"),3),CHR(0),''))
	    ENDIF
	  ENDIF
	ENDIF
	*B612666,1 MMT 03/15/2023 Create Entity related tables while creating new company[T20230216.0001][End]

*E303339,1 TMI 01/13/2013 [End  ]

*B610493,1 TMI 09/03/2013 [Start] fill in the INVTYPE file
	=gfOpenTable('INVTYPE','CINVTYPE','SH')
	SELECT INVTYPE
*B610519,3 TMI 09/24/2013 [Start] if INVTYPE has data then do not add any
	=gfSeek('')
	LOCATE
	IF EOF()
*B610519,3 TMI 09/24/2013 [End  ]
		gfAppend()
		gfReplace("cdesc with 'Style',cinvtype with '0001',citemstru with 'U',citemtran with 'SBUM',cstatus with 'A'")
		gfAppend()
		gfReplace("cdesc with 'Material',cinvtype with '0002',citemstru with 'M',citemtran with 'BUM',cstatus with 'A'")
		gfTableUpdate(.T.)
*B610519,3 TMI 09/24/2013 [Start]
	ENDIF
*B610519,3 TMI 09/24/2013 [End  ]
	gfCloseTable('INVTYPE')
*B610493,1 TMI 09/03/2013 [End  ]


*B611946,1 Es 10/31/2019 Aria4XP Company information screen:Error 'Access denied to SQL DB' while creating new company 	[Start]
	oAriaApplication.REF4.activecompanyconstr = lcCompIdOldConnStr
	oAriaApplication.REF5.activecompanyconstr = lcCompIdOldConnStr
ENDIF
*B611946,1 Es 10/31/2019 Aria4XP Company information screen:Error 'Access denied to SQL DB' while creating new company 	[End]
*E611985,1 ES 30/12/2019 Company information changes to create new company as a copy of an existing company. [Start]
IF loFormSet.ActiveMode = 'A' AND !EMPTY(loFormSet.lcCopyFrom)
	lcSourceDB=lfGetDbName(loFormSet.lcCopyFrom)
*E611985,1 ES 01/15/2020 Company information changes to create new company as a copy of an existing company. [Start]
	lcSQL = "SELECT DB_NAME(database_id) AS DBName,Name AS Logical_Name, Physical_Name,(size*8)/1024 SizeMB FROM sys.master_files WHERE DB_NAME(database_id) ='"+lcSourceDB+"'"
	lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'lcSizeMB1','', oAriaApplication.activecompanyconstr, 3, 'SAVE', SET("Datasession"))
	SELECT lcSizeMB1
	lnRequiredSpaceSQL=lcSizeMB1.SizeMB
	SET STEP ON

	lcSqlStr="use "+ALLTRIM(laConnInfo[3])+" ALTER DATABASE "+ALLTRIM(laConnInfo[3])+" MODIFY FILE(NAME = "+ALLTRIM(laConnInfo[3])+" ,SIZE = "+ALLTRIM(STR(lnRequiredSpaceSQL))+"MB);"
	lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSqlStr,'','', oAriaApplication.activecompanyconstr, 3, 'SAVE', SET("Datasession"))


*E611985,1 ES 01/15/2020 Company information changes to create new company as a copy of an existing company. [End]


*E611985,1 ES 01/16/2020 Company information changes to create new company as a copy of an existing company. [Start]

*!*		lcSqlStr="use "+ALLTRIM(laConnInfo[3])+;
*!*			" CREATE TABLE [dbo].[ACCOUNT_CARRIERS_T]("+;
*!*			" [ACCOUNT] [char](5) NULL,"+;
*!*			" [STORE] [char](8) NULL,"+;
*!*			" [CARRIER_ID] [char](20) NULL,"+;
*!*			" [Overwritten] [bit] NULL,"+;
*!*			" [CARRIER_ACCOUNT] [char](20) NULL,"+;
*!*			" [ACCOUNT_CARRIERST_KEY] [uniqueidentifier] NULL"+;
*!*			" ) ON [PRIMARY]"+;
*!*			" SET ANSI_PADDING OFF"+;
*!*			" ALTER TABLE [dbo].[ACCOUNT_CARRIERS_T] ADD  DEFAULT (newid()) FOR [ACCOUNT_CARRIERST_KEY]"+;
*!*			" SET ANSI_NULLS ON"+;
*!*			" SET QUOTED_IDENTIFIER ON"+;
*!*			" SET ANSI_PADDING ON"+;
*!*			" CREATE TABLE [dbo].[CARRIER_SHIPMENT_T]("+;
*!*			" [ACCOUNT] [char](5) NOT NULL,"+;
*!*			" [ORDER] [char](6) NULL,"+;
*!*			" [PICK_TICKET] [char](6) NULL,"+;
*!*			" [INVOICE] [char](6) NULL,"+;
*!*			" [CUSTOMER_PO] [char](15) NULL,"+;
*!*			" [CARTON_NO] [numeric](6, 0) NULL,"+;
*!*			" [STATUS] [char](1) NULL,"+;
*!*			" [STORE] [char](8) NULL,"+;
*!*			" [SHIPTO_NAME] [varchar](100) NULL,"+;
*!*			" [SHIPTO_TITLE] [varchar](100) NULL,"+;
*!*			" [SHIPTO_COMPANY] [varchar](100) NULL,"+;
*!*			" [SHIPTO_ADDRESS1] [varchar](100) NULL,"+;
*!*			" [SHIPTO_ADDRESS2] [varchar](100) NULL,"+;
*!*			" [SHIPTO_ADDRESS3] [varchar](100) NULL,"+;
*!*			" [SHIPTO_ADDRESS4] [varchar](100) NULL,"+;
*!*			"[SHIPTO_CITY] [varchar](100) NULL,"+;
*!*			" [SHIPTO_STATE] [varchar](100) NULL,"+;
*!*			" [SHIPTO_ZIP] [varchar](100) NULL,"+;
*!*			" [SHIPTO_COUNTRY] [varchar](100) NULL,"+;
*!*			" [SHIPTO_PHONE] [varchar](100) NULL,"+;
*!*			" [SHIP_FROM_NAME] [varchar](100) NULL,"+;
*!*			" [SHIP_FROM_ADDRESS1] [varchar](100) NULL,"+;
*!*			" [SHIP_FROM_ADDRESS2] [varchar](100) NULL,"+;
*!*			" [SHIP_FROM_ADDRESS3] [varchar](100) NULL,"+;
*!*			" [SHIP_FROM_ADDRESS4] [varchar](100) NULL,"+;
*!*			" [SHIP_FROM_CITY] [varchar](100) NULL,"+;
*!*			" [SHIP_FROM_STATE] [varchar](100) NULL,"+;
*!*			" [SHIP_FROM_ZIP] [varchar](100) NULL,"+;
*!*			" [SHIP_FROM_COUNTRY] [varchar](100) NULL,"+;
*!*			" [SHIP_FROM_PHONE] [varchar](100) NULL,"+;
*!*			" [CARRIER_SERVICE_CODE] [varchar](20) NULL,"+;
*!*			" [CARRIER_SERVICE_TYPE] [varchar](100) NULL,"+;
*!*			" [PACKAGE_TYPE] [varchar](50) NULL,"+;
*!*			" [CARTON_WEIGHT] [decimal](13, 2) NULL,"+;
*!*			" [BILLING_WEIGHT] [decimal](13, 2) NULL,"+;
*!*			" [CARTON_WIDTH] [decimal](6, 2) NULL,"+;
*!*			" [CARTON_LENGTH] [decimal](6, 2) NULL,"+;
*!*			" [CARTON_HEIGHT] [decimal](6, 2) NULL,"+;
*!*			" [TRACKING_ID_TYPE] [varchar](10) NULL,"+;
*!*			" [TRACKING_NO] [varchar](50) NULL,"+;
*!*			" [RETURN_TRACKING_NO] [varchar](50) NULL,"+;
*!*			" [NFREIGHT] [decimal](13, 2) NULL,"+;
*!*			" [CDECL_VAL] [decimal](13, 2) NULL,"+;
*!*			" [CCOD] [decimal](13, 2) NULL,"+;
*!*			" [GOODS_DESCRIPTION] [varchar](60) NULL,"+;
*!*			" [CCOD_AMT] [decimal](13, 2) NULL,"+;
*!*			" [CARRIER_SHIPMENT_KEY] [uniqueidentifier] NULL,"+;
*!*			" [BILLING_ACCOUNT] [varchar](30) NULL,"+;
*!*			" [CARRIER_SHIPMENT_DIGEST] [text] NULL,"+;
*!*			" [CARRIER_SHIPMENT_ID] [varchar](50) NULL,"+;
*!*			" [CARRIER] [varchar](30) NULL,"+;
*!*			" [PACKAGE] [varchar](50) NULL"+;
*!*			" ) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]"+;
*!*			" SET ANSI_PADDING OFF"+;
*!*			" ALTER TABLE [dbo].[CARRIER_SHIPMENT_T] ADD  CONSTRAINT [DF_CARRIER_SHIPMENT_T_CARRIER_SHIPMENT_KEY]  DEFAULT (newid()) FOR [CARRIER_SHIPMENT_KEY]"+;
*!*			" SET ANSI_NULLS ON"+;
*!*			" SET QUOTED_IDENTIFIER ON"+;
*!*			" SET ANSI_PADDING ON"+;
*!*			" CREATE TABLE [dbo].[CARRIER_T]("+;
*!*			" [CARRIER_ID] [varchar](20) NULL,"+;
*!*			" [STATUS] [varchar](1) NULL,"+;
*!*			" [PERIOD_START] [datetime] NULL,"+;
*!*			" [PERIOD_END] [datetime] NULL,"+;
*!*			" [SHIPPED_CARTONS] [int] NULL,"+;
*!*			" [SHIPPED_WEIGHT] [int] NULL,"+;
*!*			" [TOTAL_FREIGHT] [int] NULL,"+;
*!*			" [BALANCE] [int] NULL,"+;
*!*			" [CARRIER_KEY] [bigint] IDENTITY(1,1) NOT NULL"+;
*!*			" ) ON [PRIMARY]"+;
*!*			" SET ANSI_PADDING OFF"+;
*!*			" SET ANSI_NULLS ON"+;
*!*			" SET QUOTED_IDENTIFIER ON"+;
*!*			" SET ANSI_PADDING ON"+;
*!*			" CREATE TABLE [dbo].[CARRIER_LOGIN_INFORMATION_T]("+;
*!*			" [CARRIER_ID] [varchar](20) NULL,"+;
*!*			" [LOGIN_REQUIREMENT_ID] [varchar](20) NULL,"+;
*!*			" [LOGIN_REQUIREMENT_VALUE] [varchar](100) NULL,"+;
*!*			" [LOGIN_REQUIREMENTS_KEY] [bigint] NOT NULL,"+;
*!*			" [CARRIER_LOGIN_INFORMATION_KEY] [bigint] IDENTITY(1,1) NOT NULL"+;
*!*			" ) ON [PRIMARY]"+;
*!*			" SET ANSI_PADDING OFF"

*!*		lnConnHandler=0
*!*		lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSqlStr,'','', oAriaApplication.activecompanyconstr, 3, 'SAVE', SET("Datasession"),.T.,@lnConnHandler)
*!*		IF lnSQLRunResult>0
*!*			SQLCOMMIT(lnConnHandler)
*!*		ENDIF
*E611985,1 ES 01/16/2020 Company information changes to create new company as a copy of an existing company. [End]
	=lfCopyfromCompanyID(lcDbfPath,loFormSet.lcCopyFrom,loFormSet.laData[1],lcSourceDB,laConnInfo[3])
ENDIF
*E611985,1 ES 30/12/2019 Company information changes to create new company as a copy of an existing company. [End]

*- End of lfSetupsFormBeforeSave.


************************************************************
*! Name      : updatetablestructure
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/13/2013
*! Purpose   : Update the data tables in the created sql data base while installing a company
************************************************************
FUNCTION updatetablestructure
LPARAMETERS lca27sysfiles, lbindexonly, sydfiles, sydflfld, SYDFIELD, sydindex, updtables
USE SHARED ADDBS(lca27sysfiles)+'syccomp.dbf' ALIAS 'syccomp_A' IN 0 AGAIN
SELECT 'syccomp_A'
LOCATE
objremotedataaccess = CREATEOBJECT("remotedataaccess")
ndatasessionid = SET("Datasession")
*B610540,1 MMT 10/03/2013 Modify Company information screen to create FOX and SQL tables in the begining of the  creation process[Start]
LOCAL lnhandle , lnhandleFox
lcConnStr = ""
lcSQLDBName = ""
lcDBFsPath = ALLTRIM(loFormSet.laData[11])
lcConnFoxStr =""
*B610540,1 MMT 10/03/2013 Modify Company information screen to create FOX and SQL tables in the begining of the  creation process[End]
SCAN FOR lrunfroma4=.T. AND cComp_id = loFormSet.laData[1]
	lccompid = cComp_id
	SELECT 'syccomp_A'
*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][Start]
	IF EMPTY(ALLTRIM(CCONSERVER)) OR EMPTY(ALLTRIM(CCONDBNAME))
		LOOP
	ENDIF
*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][End]
	lcConnStr = "Driver={SQL Server};server="+ALLTRIM(CCONSERVER)+";DATABASE="+ALLTRIM(CCONDBNAME)+;
	";uid="+ALLTRIM(CCONUSERID)+";pwd="+ALLTRIM(CCONPASWRD)
*lpcreateconnstr(ccondriver, cconserver, ccondbname, cconuserid, cconpaswrd)
*B610540,1 MMT 10/03/2013 Modify Company information screen to create FOX and SQL tables in the begining of the  creation process[Start]
	lcSQLDBName = ALLTRIM(CCONDBNAME)
	lcConnFoxStr = "Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB="+ALLTRIM(loFormSet.laData[11])+;
	";SourceType=DBF;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=No;Deleted=Yes;"

	lnhandleFox = SQLSTRINGCONNECT(lcConnFoxStr)
*B610540,1 MMT 10/03/2013 Modify Company information screen to create FOX and SQL tables in the begining of the  creation process[end]
	lnhandle = SQLSTRINGCONNECT(lcConnStr)
	IF lnhandle < 0
		LOOP
	ENDIF
	SELECT (updtables)
	LOCATE
*B610292,1 the creation of files will depneds on only one dictionary folder TMI 04/07/2013 [Start] get only A40 files
*SCAN
*lctable = cfile_nam
*B610540,1 MMT 10/03/2013 Modify Company information screen to create FOX and SQL tables in the begining of the  creation process[Start]
*SCAN FOR cVer = 'A40'

	SCAN for LEFT(ALLTRIM(cfile_nam),2) <> 'SY'

*B610540,1 MMT 10/03/2013 Modify Company information screen to create FOX and SQL tables in the begining of the  creation process[End]
		lcTable = ALLTRIM(cfile_nam)
*B610292,1 the creation of files will depneds on only one dictionary folder TMI 04/07/2013 [End  ]
		lcFile_Ttl = cfile_ttl
*N000682,1 04/16/2013 THB Globlization changes[Start]
*       WAIT WINDOW TIMEOUT 1 "Update "+lctable+"-"+ALLTRIM(lcfile_ttl)+" In company "+lccompid+", Please Wait."
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW TIMEOUT 1 LANG_Update +lctable+"-"+ALLTRIM(lcfile_ttl)+LANG_Incompany  +lccompid+",LANG_PleaseWait"
*B610532,1 TMI 09/30/2013 [Start] remove the timeout
*WAIT WINDOW TIMEOUT 1 IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Update,loFormSet.GetHeaderText("LANG_Update",loFormSet.HeaderAlias)) +lctable+"-"+ALLTRIM(lcfile_ttl)+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Incompany,loFormSet.GetHeaderText("LANG_Incompany",loFormSet.HeaderAlias))  +lccompid+",LANG_PleaseWait"
		WAIT WINDOW NOWAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Update,loFormSet.GetHeaderText("LANG_Update",loFormSet.HeaderAlias)) +lcTable+"-"+ALLTRIM(lcFile_Ttl)+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Incompany,loFormSet.GetHeaderText("LANG_Incompany",loFormSet.HeaderAlias))  +lccompid+",LANG_PleaseWait"
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 THB Globlization changes[END]

		IF  .NOT. lbindexonly=.T.
*E303030,1  BEGIN
*SET KEY TO PADR(ALLTRIM(lctable), 8) IN (sydflfld)
*B610292,1 use the oApplication.FileW instead of 8 TMI 04/07/2013 [Start]
*SET KEY TO PADR(ALLTRIM(lctable),8) IN (sydflfld)
			SET KEY TO PADR(ALLTRIM(lcTable),oAriaApplication.FileW) IN (sydflfld)
*B610292,1 use the oApplication.FileW instead of 8 TMI 04/07/2013 [End  ]
*E303030,1  END
*B610540,1 MMT 10/03/2013 Modify Company information screen to create FOX and SQL tables in the begining of the  creation process[Start]
*objremotedataaccess.mcreatetable(lccompid, lctable, lcfile_ttl, sydflfld, sydfield, sydindex, ndatasessionid, lnhandle, .F., lcconnstr)
			objremotedataaccess.mcreatetable(lccompid, lcTable, lcFile_Ttl, sydflfld, SYDFIELD, sydindex, ndatasessionid,;
			IIF(&updtables..cver = 'A40',lnhandle,lnhandleFox ), .F., IIF(&updtables..cver = 'A40',lcConnStr,lcConnFoxStr))
*B610540,1 MMT 10/03/2013 Modify Company information screen to create FOX and SQL tables in the begining of the  creation process[End]
		ENDIF
*E303030,1  BEGIN
*SET KEY TO PADR(ALLTRIM(lctable), 8) IN (sydindex)
*B610292,1 use the oApplication.FileW instead of 8 TMI 04/07/2013 [Start]
*SET KEY TO PADR(ALLTRIM(lctable),8) IN (sydindex)
		SET KEY TO PADR(ALLTRIM(lcTable),oAriaApplication.FileW) IN (sydindex)
*B610292,1 use the oApplication.FileW instead of 8 TMI 04/07/2013 [End  ]
*E303030,1  END
*B610540,1 MMT 10/03/2013 Modify Company information screen to create FOX and SQL tables in the begining of the  creation process[Start]
*objremotedataaccess.mcreateindex(lccompid, lctable, lcfile_ttl, sydindex, ndatasessionid, lnhandle, lcconnstr)
		if IIF((&updtables..cver = 'A27'),FILE(addbs(ALLTRIM(loFormSet.laData[11]))+ALLT(lcTable)+".DBF"),.T.)
			objremotedataaccess.mcreateindex(lccompid, lcTable, lcFile_Ttl, sydindex, ndatasessionid, IIF(&updtables..cver = 'A40',lnhandle,lnhandleFox ), IIF(&updtables..cver = 'A40',lcConnStr,lcConnFoxStr))
		ENDIF
*B610540,1 MMT 10/03/2013 Modify Company information screen to create FOX and SQL tables in the begining of the  creation process[End]

*MEDIA
		SELECT (sydindex)
		SET KEY TO
		SELECT (sydflfld)
		SET KEY TO
*MEDIA
	ENDSCAN
	SELECT 'syccomp_A'
ENDSCAN

*B610540,1 MMT 10/03/2013 Modify Company information screen to create FOX and SQL tables in the begining of the  creation process[Start]
WAIT CLEAR
llShowLogFile = .F.
IF USED('TMPSTR')
	USE IN 'TMPSTR'
ENDIF
SELECT 'syccomp_A'
IF SEEK(loFormSet.laData[1],'syccomp_A','CCOMP_ID')
	lcComp_Name  = ALLTRIM(syccomp_A.cCom_name)
ENDIF
SELECT (updtables)
LOCATE

SCAN for LEFT(ALLTRIM(cfile_nam),2) <> 'SY'
	lnSelFile = oAriaApplication.RemoteCompanyData.Execute("Select * from "+ALLTRIM(&updtables..cfile_nam)+"" ,'',;
	"TempResult","",IIF(&updtables..cver = 'A40',lcConnStr,lcConnFoxStr),3,"",SET("Datasession"))
	IF lnSelFile <= 0
		IF !USED('TMPSTR')
			CREATE CURSOR TMPSTR (mStrRep M(10))
			SELECT TMPSTR
			APPEND BLANK
		ENDIF
		SELECT TMPSTR
		REPLACE  mStrRep WITH mStrRep + "File :"+ALLTRIM(&updtables..cfile_nam)+" - "+ALLTRIM(&updtables..cfile_ttl)+" could not be created."+CHR(13)+CHR(10)
		llShowLogFile = .T.
	ENDIF
ENDSCAN
=SQLDISCONNECT(lnhandle)
=SQLDISCONNECT(lnhandleFox)
IF llShowLogFile

	REPLACE  mStrRep WITH mStrRep + SPACE(10)+CHR(13)+CHR(10) IN TMPSTR
	REPLACE  mStrRep WITH mStrRep + SPACE(10)+CHR(13)+CHR(10) IN TMPSTR
	REPLACE  mStrRep WITH mStrRep + 'Company: '+loFormSet.laData[1]+" - "+lcComp_Name+" could not be created."+CHR(13)+CHR(10) IN TMPSTR
	lcWinTitl = 'Company: '+loFormSet.laData[1]+" - "+lcComp_Name+' Error Log'
	DO FORM (oAriaApplication.screenhome + 'SM\SMSTRREP') WITH lcWinTitl
	TRY
		IF TYPE("oAriaApplication.remotecompanydata") ='O'
			IF TYPE("oAriaApplication.remotecompanydata.oconnectionsclass") ='O'
				IF oAriaApplication.RemoteCompanyData.oconnectionsclass.Count > 0
					lnCount = oAriaApplication.RemoteCompanyData.oconnectionsclass.Count
					FOR lnInc = lnCount TO 1 STEP -1
						lcCKey=oAriaApplication.RemoteCompanyData.oconnectionsclass.KEYS(lnInc)
						lnCnkHandle=INT(VAL(SUBSTR(lcCKey,5)))
						lnConnStringValue = SQLGETPROP(lnCnkHandle, "ConnectString")
						IF UPPER(lcSQLDBName) $ UPPER(lnConnStringValue) OR UPPER(lcDBFsPath) $ UPPER(lnConnStringValue)
							oAriaApplication.RemoteCompanyData.oconnectionsclass.Close(lnCnkHandle)
						ENDIF
					ENDFOR
				ENDIF
			ENDIF
		ENDIF
		IF TYPE("objremotedataaccess") ='O'
			IF TYPE("objremotedataaccess.oconnectionsclass") ='O'
				IF objremotedataaccess.oconnectionsclass.Count > 0
					lnCount = objremotedataaccess.oconnectionsclass.Count
					FOR lnInc = lnCount TO 1 STEP -1
						lcCKey=objremotedataaccess.oconnectionsclass.KEYS(lnInc)
						lnCnkHandle=INT(VAL(SUBSTR(lcCKey,5)))
						lnConnStringValue = SQLGETPROP(lnCnkHandle, "ConnectString")
						IF UPPER(lcSQLDBName) $ UPPER(lnConnStringValue) OR UPPER(lcDBFsPath) $ UPPER(lnConnStringValue)
							objremotedataaccess.oconnectionsclass.Close(lnCnkHandle)
						ENDIF
					ENDFOR
				ENDIF
			ENDIF
		ENDIF
		lcConnStrMaster = STRTRAN(lcConnStr,"DATABASE="+ALLTRIM(lcSQLDBName),"DATABASE=master")
		lnhandleMaster = SQLSTRINGCONNECT(lcConnStrMaster)
*!*	     lnXCnnResult = SQLEXEC(lnhandleMaster,"ALTER DATABASE ["+lcSQLDBName+"] SET OFFLINE WITH ROLLBACK IMMEDIATE")
*!*	     IF lnXCnnResult >=1
		lnCnnResult = SQLEXEC(lnhandleMaster,"DROP DATABASE ["+lcSQLDBName+"]")
*!*	     ENDIF
		lcSetDefaultPath = FULLPATH('')
		SET DEFAULT TO (lcDBFsPath)
		ERASE *.*
		SET DEFAULT TO (lcSetDefaultPath)
		RD (lcDBFsPath)
	CATCH
	ENDTRY
ELSE

	IF TYPE("oAriaApplication.remotecompanydata") ='O'
		IF TYPE("oAriaApplication.remotecompanydata.oconnectionsclass") ='O'
			IF oAriaApplication.RemoteCompanyData.oconnectionsclass.Count > 0
				lnCount = oAriaApplication.RemoteCompanyData.oconnectionsclass.Count
				FOR lnInc = lnCount TO 1 STEP -1
					lcCKey=oAriaApplication.RemoteCompanyData.oconnectionsclass.KEYS(lnInc)
					lnCnkHandle=INT(VAL(SUBSTR(lcCKey,5)))
					lnConnStringValue = SQLGETPROP(lnCnkHandle, "ConnectString")
					IF UPPER(lcDBFsPath) $ UPPER(lnConnStringValue)
						oAriaApplication.RemoteCompanyData.oconnectionsclass.Close(lnCnkHandle)
					ENDIF
				ENDFOR
			ENDIF
		ENDIF
	ENDIF

	IF TYPE("objremotedataaccess") ='O'
		IF TYPE("objremotedataaccess.oconnectionsclass") ='O'
			IF objremotedataaccess.oconnectionsclass.Count > 0
				lnCount = objremotedataaccess.oconnectionsclass.Count
				FOR lnInc = lnCount TO 1 STEP -1
					lcCKey=objremotedataaccess.oconnectionsclass.KEYS(lnInc)
					lnCnkHandle=INT(VAL(SUBSTR(lcCKey,5)))
					lnConnStringValue = SQLGETPROP(lnCnkHandle, "ConnectString")
					IF UPPER(lcDBFsPath) $ UPPER(lnConnStringValue)
						objremotedataaccess.oconnectionsclass.Close(lnCnkHandle)
					ENDIF
				ENDFOR
			ENDIF
		ENDIF
	ENDIF

ENDIF
*B610540,1 MMT 10/03/2013 Modify Company information screen to create FOX and SQL tables in the begining of the  creation process[End]

RELEASE objremotedataaccess
USE IN 'syccomp_A'
*B610540,1 MMT 10/03/2013 Modify Company information screen to create FOX and SQL tables in the begining of the  creation process[Start]
IF llShowLogFile
	RETURN .F.
ELSE
	RETURN .T.
ENDIF
*B610540,1 MMT 10/03/2013 Modify Company information screen to create FOX and SQL tables in the begining of the  creation process[End]

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

*B610532,1 TMI 09/30/2013 [Start] set the data dir to the current dir
	oAriaApplication.DataDir = loFormSet.laData[11]
*B610532,1 TMI 09/30/2013 [End  ]
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [Start]
	IF EMPTY(loFormSet.lcCopyFrom)
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [End]
		lcAliasSel = SELECT()
		=gfOpenFile(oAriaApplication.SysPath+'SYCCONFG')

		COPY TO (ALLT(loFormSet.laData[11])+'\SETUPS') FOR EMPTY(cApp_Id) WITH CDX
		SELECT (lcAliasSel)
		lcAliasSel = SELECT()
		=lfInstall(loFormSet,'SY')
		SELECT (lcAliasSel)
		SET DEFAULT TO &lcSavDef
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [Start]
	ENDIF
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [end]
** Add this company title in the menu **
** popup of the companies...
	IF loFormSet.llFirstComp
		lnBarNo = CNTBAR('_COMPANIES')
	ELSE
		lnBarNo = CNTBAR('_COMPANIES')+1
	ENDIF
	DEFINE BAR lnBarNo OF _COMPANIES PROMPT loFormSet.laData[1]+'-'+ALLTRIM(loFormSet.laData[2])

	lcComp_Id = '"'+loFormSet.laData[1]+'"'

*B611946,1 Es 10/31/2019 Aria4XP Company information screen:Error 'Access denied to SQL DB' while creating new company 	[Start]
	ON SELECTION BAR lnBarNo OF _COMPANIES ;
	DO gfChngComp WITH &lcComp_Id
	PUSH MENU _ScreenMain
	POP  MENU _ScreenMain
	PUSH MENU _ScreenMain
	SHOW MENU _ScreenMain
*B611946,1 Es 10/31/2019 Aria4XP Company information screen:Error 'Access denied to SQL DB' while creating new company 	[End]

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

** Do you want to add Fiscal year for Company ð Now ? **
** < Yes > - <  No  > **
*B610524,4 TMI 10/08/2013 [Start] add the fiscal year immediatly
*IF gfModalGen("QRM00066B00006","DIALOG",loFormSet.laData[1]) = 1

*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [Start]
*IF .T.
		IF .T. AND  EMPTY(loFormSet.lcCopyFrom)
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [end]
*B610524,4 TMI 10/08/2013 [End  ]
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

			DO (oAriaApplication.ApplicationHome+'SM\SMFSYER.FXP') WITH loFormSet.laData[1]
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
		loFormSet.laData[12] = sycComp.ccurr_yer
		loFormSet.laData[13] = sycComp.ccurr_prd
		SHOW GETS

** Do you want to add account code strucure for Company ð Now? **
** < Yes > - <  No  > **
*B610524,4 TMI 10/08/2013 [Start] add the account code structure imeediatly
*IF gfModalGen("QRM00172B00006","DIALOG",loFormSet.laData[1]) = 1
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [Start]
*IF .T.
		IF .T. AND EMPTY(loFormSet.lcCopyFrom)
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [End]

*B610524,4 TMI 10/08/2013 [End  ]
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
			DO (oAriaApplication.ApplicationHome+'SM\SMACCOD.FXP') WITH loFormSet.laData[1]
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
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [Start]
		IF EMPTY(loFormSet.lcCopyFrom)
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [end]
			=gfModalGen("TRM00170B00000","DIALOG",;
			"("+loFormSet.laData[14]+"-"+ALLTRIM(loFormSet.lcPrntName)+")")
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [Start]
		ENDIF
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [end]
		loFormSet.llSavFis = .T.
		loFormSet.llSavAcc = .T.

** If the current company is a child company **
** It has to  have its  parent account  code **
** strucure...
	ENDIF

	SELECT sycComp

** Do you want to install the company modules now? **
** <  Yes  > - <  No  > **
*B610524,4 TMI 10/08/2013 [Start] install company modules immediatly
*IF gfModalGen("QRM00177B00006","DIALOG") = 1
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [Start]
*IF .T.
	IF .T. AND  EMPTY(loFormSet.lcCopyFrom)
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [end]

*B610524,4 TMI 10/08/2013 [End  ]

*B610447,1 07/22/2013 [Start]
		lcDir = substr(oAriaApplication.DataDir,1,len(oAriaApplication.DataDir)-3) + oAriaApplication.ActiveCompanyID+'\'
		llCloseFISHD = .F.
		IF !USED('FISHD')
			USE (lcDir+'FISHD') IN 0
			llCloseFISHD = .T.
		ENDIF
		loFormSet.llSavFis = SEEK('','FISHD','COMPFYEAR')
		if llCloseFISHD
			USE IN FISHD
		endif
*B610447,1 07/22/2013 [End  ]

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
*B610524,4 TMI 10/08/2013 [Start] do change company procedure and change the company caption
			IF oAriaApplication.lCompanyIsBeingCreated = .T.
				oAriaApplication.Changecompany(oAriaApplication.ActiveCompanyID)
				loFormSet.Ariaform1.Caption = "("+oAriaApplication.ActiveCompanyID+") - "+oAriaApplication.ActiveCompanyName + ;
				SUBSTR(loFormSet.Ariaform1.Caption,LEN(oAriaApplication.ActiveCompanyID+oAriaApplication.ActiveCompanyName)+1)
				oAriaApplication.lCompanyIsBeingCreated = .F.
			ENDIF
*B610524,4 TMI 10/08/2013 [End  ]

*B610589,1 TMI 11/14/2013 [Start] add the '*' in the scale file to be used for the FABRIC case

			IF USED('SCALE')
				USE IN SCALE
			ENDIF
			=gfOpenTable(oAriaApplication.DataDir+'SCALE','SCALE','SH')
			SELECT SCALE
			IF !SEEK('S*  ')
				APPEND BLANK
				REPLACE TYPE      WITH 'S' ;
				SCALE     WITH '*' ;
				CSCL_DESC WITH 'Nosize    ' ;
				CNT       WITH 1 ;
				SZ1       WITH 'Size1'
				gfTableUpdate(.T.)
			ENDIF
			gfCloseTable('SCALE')

*B610589,1 TMI 11/14/2013 [End  ]


		ELSE
** There is no modules to install its files. **
** <  Ok  > **
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [Start]
			IF EMPTY(loFormSet.lcCopyFrom)
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [end]
				=gfModalGen("TRM00178B00000","DIALOG")
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [Start]
			ENDIF
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [end]
		ENDIF

*B610524,4 TMI 10/09/2013 [Start] fill the setups table
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [Start]
		IF EMPTY(loFormSet.lcCopyFrom)
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [end]
			=lfFillSetups()
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [Start]
		ENDIF
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [end]
*B610524,4 TMI 10/09/2013 [End  ]

	ELSE
** Without installion you cannot access **
** any module in this company. **
** <  Ok  > **
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [Start]
		IF EMPTY(loFormSet.lcCopyFrom)
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [end]
			=gfModalGen("TRM00182B00000","DIALOG")
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [Start]
		ENDIF
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [end]
	ENDIF
	llCSave = .F.

*E303417,1 TMI 09/11/2013 [Start] Do a checklist, if any failed then show a log file, if all failed then show a log and call wizard screen
	oAriaApplication.lCompanyIsBeingCreated = .F.
	=lfChkCoDat(.F.)
*E303417,1 TMI 09/11/2013 [End  ]

ENDIF

SELECT sycComp
IF loFormSet.lnComRec > 0
	GOTO loFormSet.lnComRec
ENDIF
*B612494,1 MMT 11/22/2021 Sometimes Syccomp record left locked after editing company from company information screen[T20211011.0001][Start]
LOCATE FOR CCOMP_ID = loFormSet.laData[1]
*B612494,1 MMT 11/22/2021 Sometimes Syccomp record left locked after editing company from company information screen[T20211011.0001][End]

*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [Start]
*IF loFormSet.llFirstComp
IF loFormSet.llFirstComp AND EMPTY(loFormSet.lcCopyFrom)
*E611985,1 ES 01/14/2020 Company information changes to create new company as a copy of an existing company. [End]
** Do you want to set this company as a system default company. **
** <  Yes  > - <  Cancel  > **
	IF gfModalGen("TRM00199B00006","DIALOG") = 1
		=gfOpenFile(oAriaApplication.SysPath+'SYCINST')
*SELECT SYCINST
		REPLACE sycinst.cinsdfcom WITH loFormSet.laData[1]
		SELECT sycComp
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
	SELECT sycComp
	IF CURSORGETPROP("Buffering")>1
		gfTableUpdate()
	ENDIF
ENDIF
*E303339,1 TMI 01/14/2013 [End  ]
*E303949,1 MMT 04/03/2018 Modify the Company setups for the changes of [P20171130.0001][Start]
IF USED('GLSETUP')
	SELECT glsetup
	IF CURSORGETPROP("Buffering")>1
		gfTableUpdate()
	ENDIF
ENDIF
*E303949,1 MMT 04/03/2018 Modify the Company setups for the changes of [P20171130.0001][End]
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

************************************************************
*! Name      : lfFillSetups
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/09/2013
*! Purpose   : comment calling the option grid and add all lines to SETUPS file from the SYCCONFG with default values
************************************************************
FUNCTION lfFillSetups
*B610524,4 TMI 10/08/2013 [Start]
LOCAL llOpn1,llOpn2,lnSlct
lnSlct = SELECT(0)
STORE .F. TO llOpn1,llOpn2
IF !USED('SYCCONFG')
	gfOpenTable(oAriaApplication.SysPath+'SYCCONFG','APP_ID','SH')
	llOpn1 = .T.
ENDIF
IF !USED('SETUPS')
	gfOpenTable(oAriaApplication.DataDir+'SETUPS','MODVAR','SH')
	llOpn2 = .T.
ENDIF
SET ORDER TO APP_ID   IN SYCCONFG && CAPP_ID
SET ORDER TO MODVAR   IN SETUPS && CAPP_ID+CFLD_NAME
SELECT SYCCONFG
LOCATE
SCAN
	IF !SEEK(cApp_Id+CFLD_NAME,'SETUPS')
		SCATTER MEMVAR MEMO
		INSERT INTO SETUPS FROM MEMVAR
	ENDIF
ENDSCAN

SELECT SETUPS
=gfTableUpdate(.T.)
IF llOpn1
	gfCloseTable('SYCCONFG')
ENDIF
IF llOpn2
	gfCloseTable('SETUPS')
ENDIF

SELECT (lnSlct)
*B610524,4 TMI 10/08/2013 [End  ]
*- End of lfFillSetups  .


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
=gfOpenTable(oAriaApplication.caria4syspath+'SYUSTATC','','SH')
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

IF sycinst.cinsdfcom = loFormSet.laData[1]
** This company is the system default company. **
** < Clear default > - < Cancel deletion > **
	IF gfModalGen("TRM00168B00028","DIALOG") = 1
		REPLACE sycinst.cinsdfcom WITH "  "
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

lcDataDir = gfGetDataDir(ALLTRIM(IIF(SEEK(loFormSet.laData[14],'SYCCOMP'), sycComp.cCom_DDir, loFormSet.laData[11])))

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
SELECT sycComp
IF GfGetMemVar('LLHIST',loFormSet.laData[1])
	IF SEEK(GfGetMemVar('M_Comp_Id',loFormSet.laData[1]))
		lcActDir = ALLTRIM(cCom_DDir)
		=gfOpenFile(lcActDir+'setups','modvar','SH','ACTSETUP',.T.)
		IF SEEK (CHR(255)+CHR(255)+'M_COMP_ID','ACTSETUP')
			SELECT ACTSETUP
			REPLACE MDATA_DEF WITH ''
		ENDIF
		=gfCloseFile('ACTSETUP')
		SELECT sycComp
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


*N000682,1 04/17/2013 RAS Globalization[START]
*!*	DEFINE BAR 1 OF _COMPANIES;
*!*	     PROMPT "No companies available";
*!*	     COLOR SCHEME 3
	DEFINE BAR 1 OF _COMPANIES;
	PROMPT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_No_companies_available,loFormSet.GetHeaderText("LANG_No_companies_available",loFormSet.HeaderAlias))  ;
	COLOR SCHEME 3
*N000682,1 04/17/2013 RAS Globalization[End  ]
ENDIF
** Delete company or remove files after chang company path
*laScrMode    = .F.
*laScrMode[1] = .T.
loFormSet.ChangeMode('S')

lcExactSt = SET('EXACT')
SET EXACT OFF

** Delete all the users privileges for this company. **
*B610591,1 TMI 11/18/2013 17:58 [Start] COMMENT OUT
*SELECT SYUUSRPR
*SCAN FOR cComp_Id = loFormSet.laData[1]
*  DELETE
*ENDSCAN
*B610591,1 TMI 11/18/2013 17:58 [End  ]

** Delete all the tasks managers related to this company. **
SELECT SYUDTASK
SCAN FOR cComp_id = loFormSet.laData[1]
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

SELECT sycComp

*!**************************************************************************
*!
*!      Function: lpClsScr
*!
*!**************************************************************************
** Procedure to cancel the current company record...
*
FUNCTION lpClsScr

SELECT sycComp
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
	USE IN sycComp
	lfBuff('SYCCOMP')
ENDIF
SELECT sycComp
SET ORDER TO 1

lcDataDir  = gfGetDataDir(ALLTRIM(IIF(SEEK(loFormSet.laData[14],'SYCCOMP'), sycComp.cCom_DDir, loFormSet.laData[11])))

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
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*LANG_Installed+RIGHT(loFormSet.laAllMod[lnCount4],1)
		loFormSet.laAllMod[lnCount4] = SUBSTR(loFormSet.laAllMod[lnCount4],1,37)+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Installed,loFormSet.GetHeaderText("LANG_Installed",loFormSet.HeaderAlias))+RIGHT(loFormSet.laAllMod[lnCount4],1)
*N000682,1 11/20/2012 MMT Globlization changes[End]

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
SELECT sycComp

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
INTO ARRAY laReqMod

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
*B610532,1 TMI 10/02/2013 [Start] force installing all modules
*IF gfModalGen("QRM00179B00006","DIALOG",lcCurLogNm) = 1
	IF .T.
*B610532,1 TMI 10/02/2013 [End  ]

** Function to install files for current module. **
		IF lfInstall(loFormSet,lcCurModul)
* if the user select yes for installing
* ic module files then
* copy file icistru without checking if it is exist in the
* system files directory so if it does not exit an error
* occus and the module will not be installed
*E303339,1 TMI 01/14/2013 [Start]
			lnSlct = SELECT(0)
			SELECT sycComp
			CURSORSETPROP("Buffering",5)
			SELECT (lnSlct )
*E303339,1 TMI 01/14/2013 [End  ]

*B610518,1 TMI 09/16/2013 [Start] be sure that the history company exists in SYCCOMP
*IF !GfGetMemVar('LLHIST',loFormSet.laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',loFormSet.laData[1]))
			IF !GfGetMemVar('LLHIST',loFormSet.laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])) AND SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])),'syccomp')
*B610518,1 TMI 09/16/2013 [End  ]
				lnActAlias =  SELECT(0)
				SELECT sycComp
				=SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])))
				lcActPath = loFormSet.laData[11]
				loFormSet.laData[11] = ALLTRIM(sycComp.cCom_DDir)
				=lfInstall(loFormSet,lcCurModul)
				loFormSet.laData[11] = lcActPath
				SELECT sycComp
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
			REPLACE mComp_Mdl WITH loFormSet.laData[15]

*B610518,1 TMI 09/16/2013 [Start]  be sure that the history company exists in SYCCOMP
*IF !GfGetMemVar('LLHIST',loFormSet.laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',loFormSet.laData[1]))
			IF !GfGetMemVar('LLHIST',loFormSet.laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',loFormSet.laData[1]))  AND SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])),'syccomp')
*B610518,1 TMI 09/16/2013 [End  ]
				=SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])))
				REPLACE mComp_Mdl WITH loFormSet.laData[15]
				=SEEK(ALLTRIM(loFormSet.laData[1]))
			ENDIF


*N000682,1 04/17/2013 RAS Globalization[START]
*!*	       loFormSet.laAllMod[lnCount2] = " "+SUBSTR(loFormSet.laAllMod[lnCount2],2,36)+;
*!*	                           "Installed    "+RIGHT(loFormSet.laAllMod[lnCount2],1)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*LANG_Installed  +RIGHT(loFormSet.laAllMod[lnCount2],1)
			loFormSet.laAllMod[lnCount2] = " "+SUBSTR(loFormSet.laAllMod[lnCount2],2,36)+;
			IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Installed,loFormSet.GetHeaderText("LANG_Installed",loFormSet.HeaderAlias))  +RIGHT(loFormSet.laAllMod[lnCount2],1)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/17/2013 RAS Globalization[End  ]


			IF llSetReq
** Setup is required for module 'laAllMod[lnCount2]'. **
** Do you want to setup the module now? **
** <  Yes  > - <  No  > **
*B610532,1 TMI 10/02/2013 [Start] force setup all modules
*IF gfModalGen("QRM00181B00006","DIALOG",lcCurLogNm) = 1
				IF .T.
*B610532,1 TMI 10/02/2013 [End  ]

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
					lcComp_mdl = sycComp.mComp_Mdl
					=lfSetModul(lcCurModul)
					REPLACE sycComp.mComp_Mdl WITH lcComp_mdl

* copy apsetup & glsetup for History company[Start]
*B610518,1 TMI 09/16/2013 [Start]  be sure that the history company exists in SYCCOMP
*IF !GfGetMemVar('LLHIST',loFormSet.laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',loFormSet.laData[1]))
					IF !GfGetMemVar('LLHIST',loFormSet.laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',loFormSet.laData[1]))  AND SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])),'syccomp')
*B610518,1 TMI 09/16/2013 [End  ]
						lnAlias = SELECT(0)
						SELECT sycComp
						=SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])))
						lcSorDir = ALLTRIM(loFormSet.laData[11])
						lcTrgDir = ALLTRIM(sycComp.cCom_DDir)
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
				SELECT sycComp
				IF !(lcCurModul $ mmodlset)
					REPLACE mmodlset  WITH mmodlset+IIF(EMPTY(mmodlset),'',',')+lcCurModul
				ENDIF

*B610518,1 TMI 09/16/2013 [Start]  be sure that the history company exists in SYCCOMP
*IF !GfGetMemVar('LLHIST',loFormSet.laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',loFormSet.laData[1]))
				IF !GfGetMemVar('LLHIST',loFormSet.laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',loFormSet.laData[1]))  AND SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])),'syccomp')
*B610518,1 TMI 09/16/2013 [End  ]
					=SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])))
					IF !(lcCurModul $ mmodlset)
						REPLACE mmodlset  WITH mmodlset+IIF(EMPTY(mmodlset),'',',')+lcCurModul
					ENDIF
					=SEEK(ALLTRIM(loFormSet.laData[1]))
				ENDIF

				IF loFormSet.laData[1] = oAriaApplication.ActiveCompanyID
					oAriaApplication.companysetupmodules = ALLTRIM(mmodlset)
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

*B610493,1 TMI 09/03/2013 [Start] make the two fields MCOMP_MDL, MMODLSET the same
SELECT sycComp
REPLACE mmodlset WITH STRTRAN(sycComp.mComp_Mdl,'|',',')
*B610493,1 TMI 09/03/2013 [End  ]


*E303417,1 TMI 09/12/2013 [Start]
*- call GetCompanyInformation method along with changing the ActiveCompanyId
oAriaApplication.GetCompanyInformation(oAriaApplication.ActiveCompanyID)
*E303417,1 TMI 09/12/2013 [End  ]

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

=gfOpenTable(oAriaApplication.caria4syspath+'SYUSTATC','','SH')
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

SELECT sycComp

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
	USE IN sydfiles
ENDIF
USE (oAriaApplication.SysPath+"SYDFILES") IN 0
IF USED('sydfield')
	USE IN SYDFIELD
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
*B610292,1 the creation of files will depneds on only one dictionary folder TMI 04/07/2013 [Start]
*SELECT CFILE_NAM,CFILE_TTL,MFILE_APP ;
FROM (oAriaApplication.SysPath+"SYDFILES") ;
WHERE lcMod2Ins $ (mfile_app) .AND. ;
LEFT(CFILE_NAM,2) <> "SY" AND CUPGRDLVL=lcUpGrdLvl;
INTO ARRAY laMdlFiles

*E611985,1 ES 31/12/2019 Company information changes to create new company as a copy of an existing company. [Start]
*SELECT CFILE_NAM,CFILE_TTL,MFILE_APP ;
FROM (oAriaApplication.SysPath+"SYDFILES") ;
WHERE lcMod2Ins $ (mfile_app) .AND. ;
LEFT(CFILE_NAM,2) <> "SY" AND CUPGRDLVL=lcUpGrdLvl;
AND sydfiles.cVer = 'A27' ;
INTO ARRAY laMdlFiles
SELECT cfile_nam,cfile_ttl,MFILE_APP ;
FROM (oAriaApplication.SysPath+"SYDFILES") ;
WHERE lcMod2Ins $ (MFILE_APP) .AND. ;
LEFT(cfile_nam,2) <> "SY" AND CUPGRDLVL=lcUpGrdLvl;
AND sydfiles.cver = 'A27'  AND !EMPTY(ALLTRIM(cfile_nam));
INTO ARRAY laMdlFiles
*E611985,1 ES 31/12/2019 Company information changes to create new company as a copy of an existing company. [End]



*B610292,1 the creation of files will depneds on only one dictionary folder TMI 04/07/2013 [End  ]
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
*N000682,1 04/17/2013 RAS Globalization[START]
*!*	  =gfThermo(lnRecords,lnCount5,"Creating: ",laMdlFiles[lnCount5,2])
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfThermo(lnRecords,lnCount5,LANG_Creating  ,laMdlFiles[lnCount5,2])
	=gfThermo(lnRecords,lnCount5,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Creating,loFormSet.GetHeaderText("LANG_Creating",loFormSet.HeaderAlias))  ,laMdlFiles[lnCount5,2])
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/17/2013 RAS Globalization[End  ]
* call global function to create or update file structure
* insted of creating the file directly
	lcTargDir = ALLTRIM(loFormSet.laData[11])
	lnSelected = lnCount5
	=lfver_upd(lcFileNam,.T.)
	LOOP
** Select the file structure in Array **
	DECLARE laFileStrc[1,5]
	laFileStrc = " "

*B610292,1 use the oApplication.FileW instead of 8 TMI 04/07/2013 [Start]
*SELECT sydflfld.cfld_name,sydfield.cdata_typ, ;
sydfield.nfld_wdth,sydfield.nfld_dec, ;
sydflfld.nfld_pos ;
FROM  (oAriaApplication.SysPath+"sydflfld"),(oAriaApplication.SysPath+"sydfield") ;
ORDER BY sydflfld.nfld_pos ;
GROUP BY sydField.cFld_Name ;
WHERE UPPER(sydflfld.cfile_nam) = PADR(lcFileNam,8) .AND. ;
sydfield.cfld_name = sydflfld.cfld_name ;
INTO ARRAY laFileStrc
	SELECT sydflfld.CFLD_NAME,SYDFIELD.cdata_typ, ;
	SYDFIELD.nfld_wdth,SYDFIELD.nfld_dec, ;
	sydflfld.nfld_pos ;
	FROM  (oAriaApplication.SysPath+"sydflfld"),(oAriaApplication.SysPath+"sydfield") ;
	ORDER BY sydflfld.nfld_pos ;
	GROUP BY SYDFIELD.CFLD_NAME ;
	WHERE UPPER(sydflfld.cfile_nam) = PADR(lcFileNam,oAriaApplication.FileW) .AND. ;
	SYDFIELD.CFLD_NAME = sydflfld.CFLD_NAME .AND. ;
	SYDFIELD.cver $ '   |A27' ;
	INTO ARRAY laFileStrc
*B610292,1 use the oApplication.FileW instead of 8 TMI 04/07/2013 [End  ]
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

FUNCTION lfver_upd
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
*B610292,1 use the oApplication.FileW instead of 8 TMI 04/07/2013 [Start]
*SELECT sydflfld.cfld_name,sydfield.cdata_typ,;
sydfield.nfld_wdth,sydfield.nfld_dec,sydflfld.nfld_pos;
FROM  (gcSyshome+"sydflfld"),(gcSyshome+"sydfield");
ORDER BY sydflfld.nfld_pos;
GROUP BY sydField.cFld_Name;
WHERE UPPER(sydflfld.cfile_nam) =UPPER(PADR(lcFileNam,8));
AND sydfield.cfld_name = sydflfld.cfld_name;
INTO ARRAY laFileStrc
SELECT sydflfld.CFLD_NAME,SYDFIELD.cdata_typ,;
SYDFIELD.nfld_wdth,SYDFIELD.nfld_dec,sydflfld.nfld_pos;
FROM  (gcSysHome+"sydflfld"),(gcSysHome+"sydfield");
ORDER BY sydflfld.nfld_pos;
GROUP BY SYDFIELD.CFLD_NAME;
WHERE UPPER(sydflfld.cfile_nam) =UPPER(PADR(lcFileNam,oAriaApplication.FileW));
AND SYDFIELD.CFLD_NAME = sydflfld.CFLD_NAME;
INTO ARRAY laFileStrc
*B610292,1 use the oApplication.FileW instead of 8 TMI 04/07/2013 [End  ]
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
*N000682,1 04/16/2013 THB Globlization changes[Start]

*  WAIT 'Verifying file structure for file '+lcTargDir+lcFileNam Window nowait
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT LANG_Verifying +lcTargDir+lcFileNam Window nowait
		WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Verifying,loFormSet.GetHeaderText("LANG_Verifying",loFormSet.HeaderAlias)) +lcTargDir+lcFileNam Window nowait
*N000682,1 11/20/2012 MMT Globlization changes[End]


*N000682,1 04/16/2013 THB Globlization changes[END]

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
*N000682,1 04/16/2013 THB Globlization changes[Start]
*B610540,1 MMT 10/03/2013 Modify Company information screen to create FOX and SQL tables in the begining of the  creation process[Start]
RETURN
*B610540,1 MMT 10/03/2013 Modify Company information screen to create FOX and SQL tables in the begining of the  creation process[End]
*WAIT 'Updating file structure for file '+lcTargDir+lcFileNam Window nowait
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT LANG_Updating_file_structure_for_file +lcTargDir+lcFileNam Window nowait
WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Updating_file_structure_for_file,loFormSet.GetHeaderText("LANG_Updating_file_structure_for_file",loFormSet.HeaderAlias)) +lcTargDir+lcFileNam Window nowait
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 THB Globlization changes[END]
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
*N000682,1 04/16/2013 THB Globlization changes[Start]

*WAIT 'Reindexing file '+lcTargDir+lcFileNam Window nowait
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT LANG_Reindexing_file +lcTargDir+lcFileNam Window nowait
WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Reindexing_file,loFormSet.GetHeaderText("LANG_Reindexing_file",loFormSet.HeaderAlias)) +lcTargDir+lcFileNam Window nowait
*N000682,1 11/20/2012 MMT Globlization changes[End]


*N000682,1 04/16/2013 THB Globlization changes[END]

*E301167,1 Hesham (End)

lcSavAlias = SELECT(0)
*B602147,1 Clear array
laFileCDX  = ''
*B602147,1 end
*E301077,78 Hesham (Start)
llIndex = gfOpenFile(gcSysHome+'sydindex','','SH')
*E301077,78 Hesham (End)
*B610292,1 use the oApplication.FileW instead of 8 TMI 04/07/2013 [Start]
*SELECT ALLTRIM(sydindex.cindx_exp),ALLTRIM(sydindex.cfile_tag),sydindex.lascend,;
sydindex.lunique;
FROM (gcSyshome+"sydindex");
WHERE UPPER(sydindex.cfile_nam) = PADR(lcFileNam,8);
INTO ARRAY laFileCDX
SELECT ALLTRIM(sydindex.cindx_exp),ALLTRIM(sydindex.cfile_tag),sydindex.lascend,;
sydindex.lunique;
FROM (gcSysHome+"sydindex");
WHERE UPPER(sydindex.cfile_nam) = PADR(lcFileNam,oAriaApplication.FileW);
INTO ARRAY laFileCDX
*B610292,1 use the oApplication.FileW instead of 8 TMI 04/07/2013 [End  ]
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


SELECT sydindex
DECLARE laFileCDX[1,4]
laFileCDX = " "
** Select the file index in Array **
*B610292,1 use the oApplication.FileW instead of 8 TMI 04/07/2013 [Start]
*SELECT sydindex.cindx_exp,sydindex.cfile_tag, ;
sydindex.lascend,sydindex.lunique ;
FROM (oAriaApplication.SysPath+"sydindex") ;
WHERE UPPER(sydindex.cfile_nam) = PADR(lcFileNam,8) ;
INTO ARRAY laFileCDX
SELECT sydindex.cindx_exp,sydindex.cfile_tag, ;
sydindex.lascend,sydindex.lunique ;
FROM (oAriaApplication.SysPath+"sydindex") ;
WHERE UPPER(sydindex.cfile_nam) = PADR(lcFileNam,oAriaApplication.FileW) ;
INTO ARRAY laFileCDX
*B610292,1 use the oApplication.FileW instead of 8 TMI 04/07/2013 [End  ]
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

SELECT sycComp
lnRecCmp = RECNO()

IF EMPTY(loFormSet.laData[14])
	lcInsMdl  = loFormSet.laData[15]
ELSE
	IF SEEK(loFormSet.laData[14],"SYCCOMP")
		lcInsMdl  = sycComp.mComp_Mdl
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

*B610524,4 TMI 10/08/2013 [Start] comment calling the option grid and add all lines to SETUPS file from the SYCCONFG with default values
*B611946,1 Es 10/31/2019 Aria4XP Company information screen:Error 'Access denied to SQL DB' while creating new company 	[Start]

*!*	IF TYPE('oAriaApplication.lCompanyIsBeingCreated')='U' OR ;
*!*	  (TYPE('oAriaApplication.lCompanyIsBeingCreated')='L' AND oAriaApplication.lCompanyIsBeingCreated = .F. )
IF TYPE('oAriaApplication.lCompanyIsBeingCreated')='U' OR ;
	(TYPE('oAriaApplication.lCompanyIsBeingCreated')='L' AND oAriaApplication.lCompanyIsBeingCreated)
*B611946,1 Es 10/31/2019 Aria4XP Company information screen:Error 'Access denied to SQL DB' while creating new company 	[Start]
	lcExpr = lfShowGrid(lcMod2Set)
ENDIF
*B610524,4 TMI 10/08/2013 [End  ]

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
	USE IN sycComp
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
SELECT sycComp

*- check if the setuped module in the module list of the company and its history company
IF !(lcMod2Set $ mmodlset)
	REPLACE mmodlset  WITH mmodlset+IIF(llModuleIns,IIF(EMPTY(mmodlset),'',',')+lcMod2Set,'')
ENDIF
*B610518,1 TMI 09/16/2013 [Start]  be sure that the history company exists in SYCCOMP
*IF !GfGetMemVar('LLHIST',loFormSet.laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',loFormSet.laData[1]))
IF !GfGetMemVar('LLHIST',loFormSet.laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',loFormSet.laData[1]))  AND SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])),'syccomp')
*B610518,1 TMI 09/16/2013 [End  ]
	=SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])))
	IF !(lcMod2Set $ mmodlset)
		REPLACE mmodlset  WITH mmodlset+IIF(llModuleIns,IIF(EMPTY(mmodlset),'',',')+lcMod2Set,'')
	ENDIF
	=SEEK(ALLTRIM(loFormSet.laData[1]))
ENDIF

IF loFormSet.laData[1] = oAriaApplication.ActiveCompanyID
*gcCmpModules = ALLTRIM(mModlSet)
	oAriaApplication.companysetupmodules = ALLTRIM(mmodlset)
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
		DO (oAriaApplication.ApplicationHome+'GL\GLSETUP') WITH .T.,loFormSet.laData[1],lcCompDir,.F.

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
SCAN FOR CREP_ID = 'SMAPP'+lcModId
	IF SEEK(lcSeekID+STR(NVARPOS),'SETUPS')
		REPLACE MDATA_DEF WITH SETUPS.MDATA_DEF
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
lcCompanyID = oAriaApplication.ActiveCompanyID
lcCompanyName = oAriaApplication.ActiveCompanyName

oAriaApplication.ActiveCompanyID = sycComp.cComp_id
oAriaApplication.ActiveCompanyName = ALLTRIM(sycComp.cCom_name)

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

SELECT sycComp
loFormSet.laData[15] = "|" + loFormSet.laData[15]
loFormSet.laData[15] = STRTRAN(loFormSet.laData[15],"|"+lcMod2UIns,"")
loFormSet.laData[15] = SUBSTR(loFormSet.laData[15],2)
*In case of no active company selected, gcDataDir is empty generating the bug.
*E303339,1 TMI 01/03/2013 [Start]
gcDataDir = oAriaApplication.DataDir
*E303339,1 TMI 01/03/2013 [End  ]
lcOldDataD = gcDataDir
gcDataDir  = gfGetDataDir(ALLTRIM(loFormSet.laData[11]))

REPLACE mComp_Mdl WITH loFormSet.laData[15]
*B610518,1 TMI 09/16/2013 [Start]  be sure that the history company exists in SYCCOMP
*IF !GfGetMemVar('LLHIST',loFormSet.laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',loFormSet.laData[1]))
IF !GfGetMemVar('LLHIST',loFormSet.laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',loFormSet.laData[1]))  AND SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])),'syccomp')
*B610518,1 TMI 09/16/2013 [End  ]
	=SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])))
	REPLACE mComp_Mdl WITH loFormSet.laData[15]
	=SEEK(ALLTRIM(loFormSet.laData[1]))
ENDIF
*E303339,1 TMI 01/10/2013 [Start]

IF USED('SYDOBJCT')
	USE IN sydObjct
ENDIF
=gfOpenFile(oAriaApplication.caria4syspath+'SYDOBJCT','CAPP_ID','SH')   && CAPP_ID+CAPOBJNAM
*E303339,1 TMI 01/10/2013 [End  ]
SELECT sydObjct
SET ORDER TO TAG cApp_Id
llGlobalSet = !SEEK(lcMod2UIns+lcMod2UIns+'SETUP','SYDOBJCT')
IF llGlobalSet
*B610518,1 TMI 09/16/2013 [Start]  be sure that the history company exists in SYCCOMP
*lnLoop = IIF(!GfGetMemVar('LLHIST',loFormSet.laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',loFormSet.laData[1]));
,2,1)
	lnLoop = IIF(!GfGetMemVar('LLHIST',loFormSet.laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',loFormSet.laData[1]))   AND SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])),'syccomp') ;
	,2,1)
*B610518,1 TMI 09/16/2013 [End  ]
	FOR lnI = 1 TO lnLoop
		IF lnI = 2
			lcOldDir = gcDataDir
			SELECT sycComp
			=SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])))
			gcDataDir = ALLTRIM(sycComp.cCom_DDir)
			SELECT sydObjct
		ENDIF
		lcTmpSetAl = gfTempName()
		SELECT 0
		USE (gcDataDir+'SETUPS') AGAIN ALIAS &lcTmpSetAl
		SET ORDER TO TAG NVARPOS
		IF SEEK(lcMod2UIns)
			DELETE WHILE cApp_Id+STR(NVARPOS) = lcMod2UIns
		ENDIF
		USE IN (lcTmpSetAl)
		IF lnI = 2
			gcDataDir = lcOldDir
		ENDIF
	ENDFOR
ENDIF
SELECT sycComp
IF lcMod2UIns $ mmodlset
	REPLACE mmodlset  WITH STRTRAN(mmodlset,','+lcMod2UIns)
	REPLACE mmodlset  WITH STRTRAN(mmodlset,lcMod2UIns+',')
	REPLACE mmodlset  WITH STRTRAN(mmodlset,lcMod2UIns)
ENDIF
*B610518,1 TMI 09/16/2013 [Start]  be sure that the history company exists in SYCCOMP
*IF !GfGetMemVar('LLHIST',loFormSet.laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',loFormSet.laData[1]))
IF !GfGetMemVar('LLHIST',loFormSet.laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',loFormSet.laData[1]))  AND SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])),'syccomp')
*B610518,1 TMI 09/16/2013 [End  ]
	=SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])))
	IF lcMod2UIns $ mmodlset
		REPLACE mmodlset  WITH STRTRAN(mmodlset,','+lcMod2UIns)
		REPLACE mmodlset  WITH STRTRAN(mmodlset,lcMod2UIns+',')
		REPLACE mmodlset  WITH STRTRAN(mmodlset,lcMod2UIns)
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
SELECT sydfiles
SET ORDER TO TAG cfile_nam

lnRecords = 0

** Calculat No. of files to be deleted. **

COUNT FOR (LEFT(cfile_nam,2) <> "SY" .AND. ;
!EMPTY(MFILE_APP) .AND. ;
!(MFILE_APP $ "SM,SY,SM$,SY$,SM$,SY,SM,SY$,SM"));
.OR. ('SY' $ MFILE_APP .AND. LEFT(cfile_nam,2) <> 'SY');
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
SCAN FOR (LEFT(cfile_nam,2) <> "SY" .AND. !EMPTY(MFILE_APP) .AND.;
	!(MFILE_APP $ "SM,SY,SM$,SY$,SM$,SY,SM,SY$,SM"));
	.OR. ('SY' $ MFILE_APP .AND. LEFT(cfile_nam,2) <> 'SY')


	lcFilNam  = ALLTRIM(sydfiles.cfile_nam)         && Without ext
	lcFilName = ALLTRIM(sydfiles.cfile_nam)+".DBF"  && With ext

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
	SELECT sydfiles
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
SELECT sydfiles
SET ORDER TO TAG cfile_nam

lnRecords = 0

** Calculat No. of files to be deleted. **
COUNT FOR LEFT(cfile_nam,2) <> "SY" .AND. ;
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
SCAN FOR LEFT(cfile_nam,2) <> "SY" .AND. !EMPTY(MFILE_APP) .AND.;
	(lcModul $ MFILE_APP) .AND. !("," $ MFILE_APP)

	lcFilNam  = ALLTRIM(sydfiles.cfile_nam)         && Without ext
	lcFilName = ALLTRIM(sydfiles.cfile_nam)+".DBF"  && With ext

	lcDelPath = ALLTRIM(loFormSet.laData[11])
	lcPathFile= lcDelPath+lcFilName          && With ext and path

*N000682,1 04/17/2013 RAS Globalization[START]
*!*	  =gfThermo(lnRecords,lnCRec,"Deleting Module "+lcModul+" files:",lcPathFile)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfThermo(lnRecords,lnCRec,LANG_Deleting_Module  +lcModul+LANG_files  ,lcPathFile)
	=gfThermo(lnRecords,lnCRec,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Deleting_Module,loFormSet.GetHeaderText("LANG_Deleting_Module",loFormSet.HeaderAlias))  +lcModul+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_files,loFormSet.GetHeaderText("LANG_files",loFormSet.HeaderAlias))  ,lcPathFile)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/17/2013 RAS Globalization[End  ]

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
	SELECT sydfiles
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
	PRIVATE lcBrFields,lcFile_Ttl,lcSelect
	lcSelect = SELECT()
	SELECT SYCCURR
	lcBrFields=gfDbfField('SYCCURR')
	DIMENSION laTemp[1]
	STORE '' TO laTemp
*N000682,1 04/17/2013 RAS Globalization[START]
*!*	  lcFile_ttl    = "Currency"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFile_ttl    = LANG_Currency
	lcFile_Ttl    = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Currency,loFormSet.GetHeaderText("LANG_Currency",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/17/2013 RAS Globalization[End  ]
	=gfBrows(.F.,"cCurrCode","laTemp",lcFile_Ttl)
	loFormSet.laData[17] = laTemp[1]
	SELECT (lcSelect)
*SHOW GET loFormSet.laData[17]
ENDIF

IF EMPTY(loFormSet.laData[17])
*E303339,1 TMI 01/10/2013 [Start]
*loFormSet.laData[17] = lcOldCurr
	loFormSet.laData[17] = loFld.KeyTextBox.OldValue
*E303339,1 TMI 01/10/2013 [End  ]
ENDIF

loFormSet.Ariaform1.txtCurrency.Value = SYCCURR.CCURRDESC
loFormSet.Ariaform1.Refresh()
**=lfRefresh(WOUTPUT())

*!**************************************************************************
*!
*!      Function: lfGetCur
*!
*!**************************************************************************
*
FUNCTION lfGetCur

loFormSet.laData[17] = LOOKUP(SYCINT.cCurrCode,loFormSet.laData[16],SYCINT.ccont_code,'CCONTCODE')
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
*B610447,1 07/25/2013 [Start]
		if used('SYDREPRT')
			use in SYDREPRT
		endif
		gfOpenFile(oAriaApplication.SysPath+'SYDREPRT','CREP_ID','SH')
*B610447,1 07/25/2013 [End  ]
		SELECT SYFRMCDD
		SET RELATION TO SYFRMCDD.CREP_ID INTO SYDREPRT ADDITIVE
		SELECT SYFRMCDH
		SET RELATION TO SYFRMCDH.cformmaj INTO SYFRMCDD ADDITIVE
		SCAN
			llRecIns = .F.
			SELECT SYFRMCDD
*B610519,1 TMI 09/17/2013 [Start] do not check on CVER when creating FORMCDHD lines
*SCAN WHILE cformmaj = SYFRMCDH.cformmaj FOR lcMod2Ins $ UPPER(SYDREPRT.mCallMods) .AND. SYDREPRT.cVer<>"A40"
			SCAN WHILE cformmaj = SYFRMCDH.cformmaj FOR lcMod2Ins $ UPPER(SYDREPRT.mCallMods)
*B610519,1 TMI 09/17/2013 [End  ]
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
		SET RELATION TO CREP_ID INTO SYDREPRT ADDITIVE
		SELECT (lcFCDHD)
		SET RELATION TO cformmaj INTO (lcFCDDT) ADDITIVE
		SET SKIP TO (lcFCDDT),SYDREPRT

*B610519,1 TMI 09/17/2013 [Start] do not check on CVER when creating FORMCDHD lines
*SCAN FOR lcMod2Ins $ UPPER(SYDREPRT.mCallMods) .AND. SYDREPRT.cVer<>"A40"
		SCAN FOR lcMod2Ins $ UPPER(SYDREPRT.mCallMods)
*B610519,1 TMI 09/17/2013 [End  ]
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
	USE IN (lcFCDHD)
	USE IN (lcFCDDT)
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

WITH loBrnFormSet.Ariaform1
	.Caption = sycComp.cComp_id + '-' + ALLTRIM(sycComp.cCom_name) +' - Installation and Setup'
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

*B610532,1 TMI 10/02/2013 [Start] hide the button
loBrnFormSet.Ariaform1.pbUnIns.Visible = .F.
*B610532,1 TMI 10/02/2013 [End  ]


*B610532,1 TMI 10/02/2013 [Start] force installing all modules in the case of company creation
IF TYPE('oAriaApplication.lCompanyIsBeingCreated') = 'L' AND .T. && don't continue until it be required and designed well
	=lfvAll(loBrnFormSet)
	=lfvIns(loBrnFormSet)
ENDIF
*B610532,1 TMI 10/02/2013 [End  ]

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

*B610447,1 07/24/2013 [Start]
LOCAL lnI , lcCarrier,lcStatus,lnSvDataSession
lnSvDataSession = SET("Datasession")
SET DATASESSION TO lnCurrDataSession
*B610447,1 07/24/2013 [End  ]

*- do not do anything in View mode
*B610447,1 07/24/2013 [Start]
*IF loFormSet.ActiveMode = 'V'
IF loFormSet.ActiveMode = 'V' and SEEK(lcModId,'SETUPS')
*B610472,1 TMI 08/20/2013 [Start] cancel changes if OK is clicked in view mode
	SELECT SETUPS
	TABLEREVERT(.T.)
*B610472,1 TMI 08/20/2013 [End  ]
	SET DATASESSION TO lnSvDataSession
*B610447,1 07/24/2013 [End  ]
	RETURN
ENDIF

*B610447,1 07/24/2013 [Start] copy above
*LOCAL lnI , lcCarrier,lcStatus,lnSvDataSession
*lnSvDataSession = SET("Datasession")
*SET DATASESSION TO lnCurrDataSession
*B610447,1 07/24/2013 [End  ]

*B610661,1 SAB 01/26/2014 Fix company information screen to save SMTP port without decimal places [T20131118.0006][Start]
=gfOpenFile(oAriaApplication.SysPath+'SYCCONFG','VARNAME', 'SH', 'TMPSYCCONFG')
*B610661,1 SAB 01/26/2014 Fix company information screen to save SMTP port without decimal places [T20131118.0006][End]

*- Save the Login Information data
IF !USED('SYREPUVR')
	USE (lcTempFldr+'SYREPUVR') IN 0
ENDIF
SELECT SYREPUVR
LOCATE
SCAN FOR SYREPUVR.CREP_ID = lcRepID AND SYREPUVR.NVARPOS>0
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
*B610661,1 SAB 01/26/2014 Fix company information screen to save SMTP port without decimal places [T20131118.0006][Start]
*lcVal = ALLTRIM(STR(lcVal,15,4))
		=gfSeek(lcFld, 'TMPSYCCONFG')
		lcVal = ALLTRIM(STR(lcVal,15,TMPSYCCONFG.nfld_dec))
*B610661,1 SAB 01/26/2014 Fix company information screen to save SMTP port without decimal places [T20131118.0006][End]
	CASE TYPE('lcVal') = 'D'
		lcVal = DTOC(lcVal)
	ENDCASE
	lcSeekID = IIF(lcModId='SM',CHR(255)+CHR(255),lcModId)
	IF SEEK(lcSeekID+STR(NVARPOS),'SETUPS')
		SELECT SETUPS
		REPLACE MDATA_DEF WITH lcVal
	ELSE
		SCATTER MEMVAR MEMO
		SELECT SETUPS
		APPEND BLANK
		GATHER MEMVAR MEMO
		REPLACE cApp_Id   WITH lcSeekID ;
		CFLD_NAME WITH m.MFLD_NAME ;
		MDATA_DEF WITH lcVal
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
oAriaApplication.ActiveCompanyID = lcCompanyID
oAriaApplication.ActiveCompanyName = lcCompanyName
*E303339,1 TMI 01/14/2013 [End  ]

DO CASE
CASE lcModId = 'SM'
	=lfAdjustSM()
CASE lcModId = 'IC'
	=lfAdjustIC()
CASE lcModId = 'AR'
	=lfAdjustAR()
CASE lcModId = 'MA'
	=lfAdjustMA()
*!*	CASE lcModID = 'MF'
*!*	  =lfAdjustMF()
CASE lcModId = 'PS'
	=lfAdjustPS()
CASE lcModId = 'SO'
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
	*E612505,1 MMT 03/01/2022 Change the Email setting option in option grid to be a screen[T20211208.0002][Start]
	*SCAN FOR CREP_ID = 'SMAPPSM'
	SCAN FOR CREP_ID = 'SMAPPSM' AND ALLTRIM(SYREPUVR.MFLD_NAME) <> 'lnEmailSet'	
	*E612505,1 MMT 03/01/2022 Change the Email setting option in option grid to be a screen[T20211208.0002][End]
		lcVar = ALLTRIM(SYREPUVR.MFLD_NAME)
		=lfShowGet(lcVar,.F.)
	ENDSCAN
*B610472,1 TMI 08/20/2013 [Start] Update M_GL_CO
	=lfShowGet('M_GL_CO',.F.)
*B610472,1 TMI 08/20/2013 [End  ]

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
=lfShowGet('CDEFSTYTAX',GfGetMemVar('M_TAX')='Y')

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
	=lfShowGet('M_CMTYPE'+z,EOF('BOM'))
ENDFOR

*B610447,1 tmi 7/30/2013, [start] do not use this varible
*IF llOpBom
IF USED('BOM')
*B610447,1 tmi 7/30/2013, [end  ]
	USE IN bom
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

lnConnectionHandlar = loOgScroll.oRDA.SQLRun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.activecompanyconstr,3,;
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
	=loOgScroll.oRDA.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
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
llShowDfLc = (GfGetMemVar('M_WareHouse') = 'Y' .AND. OCCURS('NC',oAriaApplication.companysetupmodules)<>1)
=lfShowGet('M_PSDEFLOC',llShowDfLc)

*- End of lfAdjustPS.
************************************************************
*! Name      : lfAdjustSO
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/09/2013
*! Purpose   : Adjust the controls of the Option Grid of the SO module by enable/disable appropriatly
************************************************************
FUNCTION lfAdjustSO
llMScale   = GfGetMemVar('M_USEEXSSC')
=lfShowGet('M_EXSS_SCR',llMScale)

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
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("QRM00291B00000","DIALOG",LANG_Styles+'|'+LANG_Styles)
		=gfModalGen("QRM00291B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Styles,loFormSet.GetHeaderText("LANG_Styles",loFormSet.HeaderAlias))+'|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Styles,loFormSet.GetHeaderText("LANG_Styles",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

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
	SET ORDER TO TAG CFLD_NAME
ENDIF

IF !SEEK(lcValue,"SYDFIELD")
*N000682,1 04/17/2013 RAS Globalization[START]
*!*	  lcBrFields = [cfld_name:H="Field Code",cfld_head:H="Field Header"]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcBrFields = [cfld_name:H=LANG_Field_Code  ,cfld_head:H= LANG_Field_Header]
	lcBrFields = [cfld_name:H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Field_Code,loFormSet.GetHeaderText("LANG_Field_Code",loFormSet.HeaderAlias))+['  ,cfld_head:H= ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Field_Header,loFormSet.GetHeaderText("LANG_Field_Header",loFormSet.HeaderAlias))+[']
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/17/2013 RAS Globalization[End  ]

*N000682,1 04/17/2013 RAS Globalization[START]
*!*	  lcFile_ttl = "Users' Fields"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFile_ttl = LANG_Users_Fields
	lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Users_Fields,loFormSet.GetHeaderText("LANG_Users_Fields",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/17/2013 RAS Globalization[End  ]
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
	SET ORDER TO TAG CFLD_NAME
ENDIF


IF !EMPTY(lcValue)
	IF !SEEK(lcValue,"SYDFIELD")
*N000682,1 04/17/2013 RAS Globalization[START]
*!*	    lcBrFields = [cfld_name:H="Field Code",cfld_head:H="Field Header"]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcBrFields = [cfld_name:H=LANG_Field_Code  ,cfld_head:H=LANG_Field_Header  ]
		lcBrFields = [cfld_name:H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Field_Code,loFormSet.GetHeaderText("LANG_Field_Code",loFormSet.HeaderAlias))+['  ,cfld_head:H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Field_Header,loFormSet.GetHeaderText("LANG_Field_Header",loFormSet.HeaderAlias))+[']
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/17/2013 RAS Globalization[End  ]

*N000682,1 04/17/2013 RAS Globalization[START]
*!*	    lcFile_ttl = "Users' Fields"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFile_ttl = LANG_Users_Fields
		lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Users_Fields,loFormSet.GetHeaderText("LANG_Users_Fields",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/17/2013 RAS Globalization[End  ]
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
*N000682,1 04/17/2013 RAS Globalization[START]
*!*	  lcBrFields = [cletterid:H="Letter ID",cletshdes:H="Letter Description"]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcBrFields = [cletterid:H=LANG_Letter_ID  ,cletshdes:H=LANG_Letter_Description  ]
	lcBrFields = [cletterid:H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Letter_ID,loFormSet.GetHeaderText("LANG_Letter_ID",loFormSet.HeaderAlias))+['  ,cletshdes:H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Letter_Description,loFormSet.GetHeaderText("LANG_Letter_Description",loFormSet.HeaderAlias))+[']
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/17/2013 RAS Globalization[End  ]
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
*N000682,1 04/16/2013 THB Globlization changes[Start]
*IF !EOF() AND gfModalGen("QRM00290B00006","DIALOG",'Pack and\or Sku')=2
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF !EOF() AND gfModalGen("QRM00290B00006","DIALOG",LANG_Pack_Sku)=2
	IF !EOF() AND gfModalGen("QRM00290B00006","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Pack_Sku,loFormSet.GetHeaderText("LANG_Pack_Sku",loFormSet.HeaderAlias)))=2
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 THB Globlization changes[END]
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
	USE (ALLTRIM(loFormSet.laData[11])+'WHSLOC.DBF') AGAIN ALIAS &lcTWHSLoc
*E301098,1 Hesham (End)
	GO TOP
	IF !EOF()
*B601911,1 AMM The message is "Please note that all locations will be lost. Are you sure you want to proceed ?"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF gfModalGen("QRM42063B00006","DIALOG",LANG_bins) = 2
		IF gfModalGen("QRM42063B00006","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_bins,loFormSet.GetHeaderText("LANG_bins",loFormSet.HeaderAlias))) = 2
*N000682,1 11/20/2012 MMT Globlization changes[End]

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
	USE IN (lcTWHSLoc)
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
	=SEEK (sycComp.cComp_id+'Y'+'CDIVISION')
	IF !EOF()
*B601911,1 AMM search if any devision has a related field 'CUPCMAN' not empty.
		LOCATE REST WHILE cComp_id+crltfield+CFLD_NAME=sycComp.cComp_id+'Y'+'CDIVISION';
		FOR cRltd_Nam='CUPCMAN' .AND. !EMPTY(cRltd_Vlu)

*B601911,1 AMM the message is "UPC numbers at the division level already exist , Are you sure you want to change ?"
*N000682,1 04/16/2013 THB Globlization changes[Start]
*    IF FOUND() .AND. gfModalGen("QRM00290B00006","DIALOG",'UPC numbers at the division level')=2
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF FOUND() .AND. gfModalGen("QRM00290B00006","DIALOG",LANG_UPC_numbers )=2
		IF FOUND() .AND. gfModalGen("QRM00290B00006","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_UPC_numbers,loFormSet.GetHeaderText("LANG_UPC_numbers",loFormSet.HeaderAlias)) )=2
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 THB Globlization changes[END]
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
PRIVATE lcBrFields, lcFile_Ttl
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
		SET ORDER TO TAG IdRltFName
*E301073,1 AMM If there is at least one color code in the file
*B602725,1 AMM Adjust to fit the new index
*IF SEEK(ALLTRIM(SyCCOMP.cComp_ID)+'N'+'COLOR')
		IF SEEK('N'+'N'+'COLOR')
*B602725,1 AMM end
*E301073,1 AMM browse color codes
*E301073,1 AMM browse fields
*N000682,1 04/17/2013 RAS Globalization[START]
*!*	      lcBrFields = "CCODE_NO :H='Code',CDISCREP :H='Description'"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcBrFields = "CCODE_NO :H=LANG_Code  ,CDISCREP :H=LANG_Description"
			lcBrFields = "CCODE_NO :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Code,loFormSet.GetHeaderText("LANG_Code",loFormSet.HeaderAlias)) +"'  ,CDISCREP :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Description,loFormSet.GetHeaderText("LANG_Description",loFormSet.HeaderAlias))+"'"
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/17/2013 RAS Globalization[End  ]
			DIMENSION laTemp[1]
			STORE '' TO laTemp
*E301073,1 AMM vbrowse title
*N000682,1 04/17/2013 RAS Globalization[START]
*!*	      lcFile_ttl    = "Select Color"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFile_ttl    = LANG_Select_Color
			lcFile_Ttl    = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Select_Color,loFormSet.GetHeaderText("LANG_Select_Color",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/17/2013 RAS Globalization[End  ]
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
				SET ORDER TO TAG CFLD_NAME IN SYDFIELD
				IF SEEK('COLOR', "SYDFIELD")
					lcRltField = UPPER(ALLTRIM(SYDFIELD.mRltFields))
					lcRltField = "|" + lcRltField + "|"


					INSERT INTO (lcCodTem) ;
					(cDefCode, CFLD_NAME,ccode_no, cdiscrep,crltfield, ;
					cadd_user,dadd_date,cadd_time) ;
					VALUES ('N' , 'COLOR', MCLRASSCOD, '' , "N" , ;
					gcUser_ID , DATE() , gfGetTime())
					=gfTraceKey('CODES','N'+PADR('COLOR',6)+MCLRASSCOD+;
					SPACE(30)+SPACE(10),'A',ALLTRIM(sycComp.cCom_DDir),;
					sycComp.mmodlset)
					IF !SEEK('D'+'N'+'COLOR')
*E301073,1 AMM  Insert the default code record
						INSERT INTO (lcCodTem) ;
						(cDefCode,CFLD_NAME,ccode_no, ;
						cdiscrep,crltfield, ;
						cadd_user,dadd_date,cadd_time) ;
						VALUES ('D','COLOR',MCLRASSCOD, ;
						'',"N", gcUser_ID,DATE(),gfGetTime())
						=gfTraceKey('CODES','D'+PADR('COLOR',6)+MCLRASSCOD,'A',;
						ALLTRIM(sycComp.cCom_DDir),sycComp.mmodlset)
					ENDIF
*B602725,1 AMM end

*E301073,1 AMM  Get related fields
					SELECT DISTINCT SYDFIELD.CFLD_NAME,SYDFIELD.cdata_typ;
					FROM  SYDFIELD ;
					WHERE "|"+UPPER(ALLTRIM(SYDFIELD.CFLD_NAME))+"|" $ lcRltField ;
					.OR. "|"+'$'+UPPER(ALLTRIM(SYDFIELD.CFLD_NAME))+"|" $ lcRltField ;
					INTO  ARRAY laRelFld
*E301073,1 AMM  Add related fields records to the code file
					FOR lnCount = 1 TO ALEN(laRelFld,1)
						INSERT INTO (lcCodTem) ;
						(cDefCode,CFLD_NAME,ccode_no, ;
						cdiscrep,crltfield, ;
						cRltd_Nam,cRltd_Typ,cRltd_Vlu,;
						cadd_user,dadd_date,cadd_time) ;
						VALUES ('N','COLOR',MCLRASSCOD, ;
						"","Y",laRelFld[lnCount,1],;
						laRelFld[lnCount,2],"", ;
						gcUser_ID,DATE(),gfGetTime())
						=gfTraceKey('CODES','N'+PADR('COLOR',6)+;
						MCLRASSCOD+ SPACE(30)+laRelFld[lnCount,1],'A',ALLTRIM(sycComp.cCom_DDir),;
						sycComp.mmodlset)
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
*N000682,1 04/16/2013 THB Globlization changes[Start]
*  =gfModalGen("QRM00291B00000","DIALOG",'Materials|items')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("QRM00291B00000","DIALOG",LANG_Materials+'|'+LANG_items)
		=gfModalGen("QRM00291B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Materials,loFormSet.GetHeaderText("LANG_Materials",loFormSet.HeaderAlias))+'|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_items,loFormSet.GetHeaderText("LANG_items",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 THB Globlization changes[END]
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
*N000682,1 04/16/2013 THB Globlization changes[Start]
*  =gfModalGen('INM00288B00000','DIALOG','Manufacturing Order')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00288B00000','DIALOG',LANG_Manufacturing_Order)
	=gfModalGen('INM00288B00000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Manufacturing_Order,loFormSet.GetHeaderText("LANG_Manufacturing_Order",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 THB Globlization changes[END]
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
*N000682,1 04/16/2013 THB Globlization changes[Start]
*  =gfModalGen('INM00288B00000','DIALOG','Rolls Id')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00288B00000','DIALOG',LANG_Rolls_Id)
	=gfModalGen('INM00288B00000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Rolls_Id,loFormSet.GetHeaderText("LANG_Rolls_Id",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 THB Globlization changes[Start]
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
*N000682,1 04/16/2013 THB Globlization changes[Start]
*  =gfModalGen('INM00288B00000','DIALOG','Material Purchase Order ')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00288B00000','DIALOG',LANG_Material_Purchase_Order)
	=gfModalGen('INM00288B00000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Material_Purchase_Order,loFormSet.GetHeaderText("LANG_Material_Purchase_Order",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 THB Globlization changes[END]
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
*  =gfModalGen('INM00288B00000','DIALOG','Rework Order')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00288B00000','DIALOG',LANG_Rework_Order)
	=gfModalGen('INM00288B00000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Rework_Order,loFormSet.GetHeaderText("LANG_Rework_Order",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

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
*N000682,1 04/16/2013 THB Globlization changes[Start]
*  =gfModalGen('INM00288B00000','DIALOG','Cutting Ticket')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00288B00000','DIALOG',LANG_Cutting_Ticket)
	=gfModalGen('INM00288B00000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Cutting_Ticket,loFormSet.GetHeaderText("LANG_Cutting_Ticket",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 THB Globlization changes[END]
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
*N000682,1 04/16/2013 THB Globlization changes[Start]
*  =gfModalGen('INM00288B00000','DIALOG','style PO')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00288B00000','DIALOG',LANG_style_PO)
	=gfModalGen('INM00288B00000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_style_PO,loFormSet.GetHeaderText("LANG_style_PO",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 THB Globlization changes[END]
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

*N000682,1 04/17/2013 RAS Globalization[START]
*!*	M_ATOMDIR = GETDIR('','Atomic Software Directory')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*M_ATOMDIR = GETDIR('',LANG_Atomic_Software_Directory  )
M_ATOMDIR = GETDIR('',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Atomic_Software_Directory,loFormSet.GetHeaderText("LANG_Atomic_Software_Directory",loFormSet.HeaderAlias))  )
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/17/2013 RAS Globalization[End  ]
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
*N000682,1 04/16/2013 THB Globlization changes[Start]
*  =gfModalGen('INM00288B00000','DIALOG','sales order')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00288B00000','DIALOG',LANG_sales_order)
	=gfModalGen('INM00288B00000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_sales_order,loFormSet.GetHeaderText("LANG_sales_order",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 THB Globlization changes[end]
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

*B610447,1 07/25/2013 [Start] be sure SYCCONFG is open
if !used('SYCCONFG')
	use (oAriaApplication.SysPath+'SYCCONFG') in 0 ORDER NVARPOS
endif
*B610447,1 07/25/2013 [End  ]

*N000682,1 04/16/2013 THB Globlization changes[Start]
IF llHist .AND. !EMPTY(M_Comp_Id)
	IF oAriaApplication.ActiveCompanyID == M_Comp_Id
*N000682,1 04/17/2013 RAS Globalization[START]
*!*	    =gfModalGen("TRM00000B00000","DIALOG","Company Information",.F.,LANG_The_active)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00000B00000","DIALOG",LANG_Company_Information  ,.F.,LANG_The_active)
		=gfModalGen("TRM00000B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COMPANY_INFORMATION,loFormSet.GetHeaderText("LANG_Company_Information",loFormSet.HeaderAlias))  ,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_The_active,loFormSet.GetHeaderText("LANG_The_active",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/17/2013 RAS Globalization[End  ]
*IF llHist .AND. !EMPTY(M_Comp_Id)
* IF oAriaApplication.ActiveCompanyID == M_Comp_Id
*  =gfModalGen("TRM00000B00000","DIALOG","Company Information",.F.,;
"The active company cannot be the current one.")
*N000682,1 04/16/2013 THB Globlization changes[end]
		_CUROBJ = _CUROBJ
		RETURN
	ENDIF
	IF M_Comp_Id == loFormSet.laData[1]
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*    =gfModalGen("TRM00000B00000","DIALOG","Company Information",.F.,;
"This is a History Company,"+;
"select another company to be the Active Company.")
		=gfModalGen("TRM00000B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COMPANY_INFORMATION,loFormSet.GetHeaderText("LANG_Company_Information",loFormSet.HeaderAlias))  ,.F.,;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_This_is_a_History_Company1,loFormSet.GetHeaderText("LANG_This_is_a_History_Company1",loFormSet.HeaderAlias)) + ;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_select,loFormSet.GetHeaderText("LANG_select",loFormSet.HeaderAlias))+"")
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[Start]
		_CUROBJ = _CUROBJ
		RETURN
	ENDIF
	lnAlias = SELECT(0)
*E303339,1 TMI 01/14/2013 [Start]
	lfBuff('SYCCOMP')
*E303339,1 TMI 01/14/2013 [End  ]
	SELECT sycComp
	SET ORDER TO 1
	lnRec = RECNO()
	SEEK loFormSet.laData[1]
	lcTrgDir = ALLTRIM(cCom_DDir)
	IF SEEK(M_Comp_Id)
*B610518,1 TMI 09/16/2013 [Start]  be sure that the history company exists in SYCCOMP
*IF !GfGetMemVar('LLHIST',M_Comp_Id) .AND. EMPTY(GfGetMemVar('M_Comp_Id',M_Comp_Id))
		IF !GfGetMemVar('LLHIST',M_Comp_Id) .AND. EMPTY(GfGetMemVar('M_Comp_Id',M_Comp_Id))  AND SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',loFormSet.laData[1])),'syccomp')
*B610518,1 TMI 09/16/2013 [End  ]


*N000682,1 04/17/2013 RAS Globalization[START]

*!*	      lcMsg = "You are going to make the company " + loFormSet.laData[1] + " a history company for " +;
*!*	              "company " + M_COMP_ID + " . This will remove all data in company " + loFormSet.laData[1] +;
*!*	              " ! Are you sure you want to continue ?"
*!*	      IF gfModalGen("TRM00000B00006","DIALOG","Company Information",.F.,lcMsg) = 2

			lcMsg = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_You_are_going_to_make_the_company,loFormSet.GetHeaderText("LANG_You_are_going_to_make_the_company",loFormSet.HeaderAlias))   + loFormSet.laData[1] + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_a_history_company_for,loFormSet.GetHeaderText("LANG_a_history_company_for",loFormSet.HeaderAlias))   +;
			IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_company,loFormSet.GetHeaderText("LANG_company",loFormSet.HeaderAlias))   + M_Comp_Id +IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_This_will_remove_all_data_in_company,loFormSet.GetHeaderText("LANG_This_will_remove_all_data_in_company",loFormSet.HeaderAlias))  + loFormSet.laData[1] +;
			IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Are_you_sure_you_want_to_continue,loFormSet.GetHeaderText("LANG_Are_you_sure_you_want_to_continue",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*LANG_Are_you_sure_you_want_to_continue
*N000682,1 11/20/2012 MMT Globlization changes[End]

			IF gfModalGen("TRM00000B00006","DIALOG","Company Information",.F.,lcMsg) = 2

*N000682,1 04/17/2013 RAS Globalization[End  ]
				llHist = .F.
				M_Comp_Id = ''
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
			lcSorDir = ALLTRIM(cCom_DDir)
			lcCompCod = loFormSet.laData[1]
			lcScFields = loFormSet.lcScFields
			SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData
			loFormSet.laData[1] = lcCompCod
*N000682,1 04/17/2013 RAS Globalization[START]
*!*	      loFormSet.laData[2] = 'History of ' + loFormSet.laData[2]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laData[2] = LANG_History_of   + loFormSet.laData[2]
			loFormSet.laData[2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_History_of,loFormSet.GetHeaderText("LANG_History_of",loFormSet.HeaderAlias))   + loFormSet.laData[2]
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/17/2013 RAS Globalization[End  ]
			loFormSet.laData[11] = lcTrgDir
			loFormSet.laData[15] = STRTRAN(loFormSet.laData[15],'GL|','')
			loFormSet.laData[15] = STRTRAN(loFormSet.laData[15],'GL,','')
			loFormSet.laData[15] = STRTRAN(loFormSet.laData[15],'GL','')
			lcCurModul = STRTRAN(mmodlset,'GL|','')
			lcCurModul = STRTRAN(lcCurModul,'GL,','')
			lcCurModul = STRTRAN(lcCurModul,'GL','')
			SEEK loFormSet.laData[1]
			REPLACE mmodlset  WITH lcCurModul
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
				USE IN icistru
			ENDIF
			IF USED('GLSETUP')
				USE IN glsetup
			ENDIF
			IF USED('APSETUP')
				USE IN APSETUP
			ENDIF
			IF USED('GLACCHAR')
				USE IN GLACCHAR
			ENDIF
			DECLARE laInstModl[1]

*B610447,1 07/25/2013 [Start]
			loFormSet.laData[15] = sycComp.mComp_Mdl
*B610447,1 07/25/2013 [End  ]
			=gfSubStr(loFormSet.laData[15],@laInstModl,'|')
			lnI = 1
			FOR lnI = 1 TO ALEN(laInstModl,1)
*B610447,1 07/25/2013 [Start] send loFormSet
*=lfInstall(laInstModl[lnI])
				=lfInstall(loFormSet,laInstModl[lnI])
*B610447,1 07/25/2013 [End  ]
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
*B610447,1 07/25/2013 [Start] seek with order NVARPOS
*IF SEEK (CHR(255)+CHR(255)+SPACE(9)+'2','SYCCONFG')
				IF SEEK (CHR(255)+CHR(255)+SPACE(9)+'2','SYCCONFG','NVARPOS')
*B610447,1 07/25/2013 [End  ]
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
				REPLACE MDATA_DEF WITH M_Comp_Id
			ELSE
				IF SEEK (CHR(255)+CHR(255)+SPACE(9)+'2','SYCCONFG')
					SELECT SYCCONFG
					SCATTER MEMO MEMVAR
					SELECT (lcHstSetup)
					APPEND BLANK
					GATHER MEMO MEMVAR
					REPLACE MDATA_DEF WITH M_Comp_Id
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
*B610447,1 07/25/2013 [Start] comment
*laScrMode    = .F.
*laScrMode[2] = .T.
*B610447,1 07/25/2013 [End  ]
		ELSE
			IF !GfGetMemVar('LLHIST',M_Comp_Id)
*N000682,1 04/16/2013 THB Globlization changes[Start]
*        =gfModalGen("TRM00000B00000","DIALOG","companies",.F.,"This Company has a History Company already.")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00000B00000","DIALOG","companies",.F.,LANG_This_Company_has)
				=gfModalGen("TRM00000B00000","DIALOG","companies",.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_This_Company_has,loFormSet.GetHeaderText("LANG_This_Company_has",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 THB Globlization changes[END]
			ELSE
*N000682,1 04/16/2013 THB Globlization changes[START]
*        =gfModalGen("TRM00000B00000","DIALOG","companies",.F.,"This is a History Company.")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00000B00000","DIALOG","companies",.F.,LANG_This_is_a_History_Company)
				=gfModalGen("TRM00000B00000","DIALOG","companies",.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_This_is_a_History_Company,loFormSet.GetHeaderText("LANG_This_is_a_History_Company",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 THB Globlization changes[END]
			ENDIF
			loOgScroll.ActiveControl.Value = loOgScroll.ActiveControl.OldValue
			M_Comp_Id = loOgScroll.ActiveControl.OldValue
			loOgScroll.ActiveControl.Refresh()

			_CUROBJ = _CUROBJ
		ENDIF
	ELSE
*N000682,1 04/16/2013 THB Globlization changes[Start]
*    =gfModalGen("TRM00000B00000","DIALOG","companies",.F.,"This company does not exist.")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00000B00000","DIALOG","companies",.F.,LANG_This_company_does_not_exist)
		=gfModalGen("TRM00000B00000","DIALOG","companies",.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_This_company_does_not_exist,loFormSet.GetHeaderText("LANG_This_company_does_not_exist",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 THB Globlization changes[END]
		loOgScroll.ActiveControl.Value = loOgScroll.ActiveControl.OldValue
		M_Comp_Id = loOgScroll.ActiveControl.OldValue
		loOgScroll.ActiveControl.Refresh()
		_CUROBJ = _CUROBJ
	ENDIF
	SELECT (lnAlias)
ELSE
	llHist = .F.
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

IF oAriaApplication.ActiveCompanyID == loFormSet.laData[1]
*N000682,1 04/16/2013 THB Globlization changes[Start]
*  =gfModalGen("TRM00000B00000","DIALOG","Company Information",.F.,;
"You cannot select the current company to be a History Company.")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*LANG_You_cannot_select_the_current_company_to_be_a_History_Company)
	=gfModalGen("TRM00000B00000","DIALOG","Company Information",.F.,;
	IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_You_cannot_select_the_current_company_to_be_a_History_Company,loFormSet.GetHeaderText("LANG_You_cannot_select_the_current_company_to_be_a_History_Company",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 THB Globlization changes[END]
	llHist = .F.
	=lfShowGet(LAOGOBJTYPE[ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'LLHIST'),1),1],.T.)
	_CUROBJ = _CUROBJ
	RETURN
ENDIF
*E303339,1 TMI 01/14/2013 [Start]
*lnI = 1
*FOR lnI = 1 TO ALEN(laOGObjType,1)
*  =lfShowGet(laOGObjType[lnI,1],.F.)
*ENDFOR
*E303339,1 TMI 01/14/2013 [End  ]
=lfShowGet(LAOGOBJTYPE[ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'LLHIST'   ),1),1],.F.)
=lfShowGet(LAOGOBJTYPE[ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'M_COMP_ID'),1),1],.T.)
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
*! Name      : lfvTaxSt
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/20/2013
*! Purpose   : enable/disable the following based on Use taxes is Yes or No
*!           : Tax Name
*!           : Tax rate
*!           : Tax methos
*!           : Tax Reference
************************************************************
*B610472,1 TMI 08/20/2013 [Start]
FUNCTION lfvTaxSt
IF M_TAX = 'Y'
	M_TAX_RATE = 1.0
	=lfShowGet('M_TAX_DESC',.T.)
	=lfShowGet('M_TAX_METH',.T.)
	=lfShowGet('M_TAX_RATE',.T.)
	=lfShowGet('M_TAX_REFE',.T.)
	=lfShowGet('M_HST_RATE',.T.)
ELSE
	M_TAX_DESC = SPACE(20)
	M_TAX_METH = 'A'
	M_TAX_RATE = 0.0
	M_TAX_REFE = SPACE(30)
	=lfShowGet('M_TAX_DESC',.F.)
	=lfShowGet('M_TAX_METH',.F.)
	=lfShowGet('M_TAX_RATE',.F.)
	=lfShowGet('M_TAX_REFE',.F.)
	=lfShowGet('M_HST_RATE',.F.)
ENDIF
*- Disanable it is any country rather CANADA.

IF UPPER(ALLTRIM(gcContCode))<>'CANADA'
	=lfShowGet('M_HST_RATE',.F.)
ENDIF

*- End of lfvTaxSt.

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
*IF FILE('C:\TEMP\X.X')
*ENDIF
	ENDIF
ENDIF
*- End of MSG.


************************************************************
*! Name      : lfChkCoDat
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/11/2013
*! Purpose   : Do a checklist, if any failed then show a log file, if all failed then show a log and call wizard screen
*! When Called : in ADD mode only when creating a new company
*!             : from the oAriaApplication.ActiveCompanyID_Assign method
*! E303417,1 TMI 09/11/2013
************************************************************
FUNCTION lfChkCoDat
PARAMETERS llShowLog

*B610532,1 TMI 10/02/2013 [Start] if the file ICISTRU didn't found then modules are not installed
IF !FILE(oAriaApplication.DataDir+'ICISTRU.DBF')
	IF TYPE('oAriaApplication.lCompanyIsBeingCreated') = 'U'  && show the message , this condition means that the function is called from the validation not from the company creation
** Without installion you cannot access **
** any module in this company. **
** <  Ok  > **
		=gfModalGen("TRM00182B00000","DIALOG")
	ENDIF
	RETURN
ENDIF
*B610532,1 TMI 10/02/2013 [End  ]

*- don't run this function if the oAriaApplicatin.ActiveCompanyID_Assign is issued while creating a company
IF TYPE('oAriaApplication.lCompanyIsBeingCreated') = 'L' AND oAriaApplication.lCompanyIsBeingCreated = .T.
	RETURN
ENDIF

*E303419,1 TMI 09/15/2013 [Start] check the ICISTRU IN validation mode before any checks
*B610524,4 TMI 10/08/2013 [Start] call the wizard screen after the STYLE CODE structure screen
*IF TYPE('oAriaApplication.lCompanyIsBeingCreated') = 'U'
*B610524,4 TMI 10/08/2013 [End  ]
=lfChkICISTRU()
*B610524,4 TMI 10/08/2013 [Start] comment
*ENDIF
*B610524,4 TMI 10/08/2013 [End  ]

*- Create a new session to open the tables in
LOCAL lnOldSession,loSession
lnOldSession = SET("Datasession")
loSession = NEWOBJECT('Session')
SET DATASESSION TO loSession.DatasessionID
SET MULTILOCKS ON && necessary for buffering
SET DELETED ON

=gfOpenTable(oAriaApplication.DataDir+'ICISTRU','SEGNO','SH')     && CITEMRECTY+CISEGNO
=gfOpenTable(oAriaApplication.DataDir+'SCALE','SCALE','SH')
=gfOpenTable(oAriaApplication.DataDir+'CODES','CCODE_NO','SH')    && CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM
=gfOpenTable(oAriaApplication.DataDir+'WAREHOUS','WAREHOUS','SH') && CWARECODE
=gfOpenTable(oAriaApplication.DataDir+'SYDAPPL','CAPP_ID','SH')
=gfOpenTable(oAriaApplication.SysPath+'salesrep','salesrep','SH')


*- do the checks
llAllDataNotFound = .T.
lcLogStr = ''
lcCr = CHR(13)

IF !gfSeek('','salesrep')
	lcLANG_MSG_SALESREP = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MSG_SALESREP,loFormSet.GetHeaderText("LANG_MSG_SALESREP",loFormSet.HeaderAlias))
	=lfAddToLog(lcLANG_MSG_SALESREP)
ELSE
	llAllDataNotFound = .F.
ENDIF

IF !gfSeek('','WAREHOUS')
	lcLANG_MSG_WAREHOUS = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MSG_WAREHOUS,loFormSet.GetHeaderText("LANG_MSG_WAREHOUS",loFormSet.HeaderAlias))
	=lfAddToLog(lcLANG_MSG_WAREHOUS)
ELSE
	SELECT WAREHOUS
	LOCATE FOR LSTYINV
	IF !FOUND()
		lcLANG_MSG_LSTYINV = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MSG_LSTYINV,loFormSet.GetHeaderText("LANG_MSG_LSTYINV",loFormSet.HeaderAlias))
		=lfAddToLog(lcLANG_MSG_LSTYINV)
	ELSE
		llAllDataNotFound = .F.
	ENDIF
ENDIF

*B610524,4 TMI 10/09/2013 [Start] comment this, not all customers are using materials
*IF gfSeek('MA','SYDAPPL')
*  IF !gfSEEK('','WAREHOUS')
*    *E303419,1 TMI 09/16/2013 [Start] comment this, it is duplicated in the log
*    *lcLANG_MSG_WAREHOUS = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MSG_WAREHOUS,loFormSet.GetHeaderText("LANG_MSG_WAREHOUS",loFormSet.HeaderAlias))
*    *=lfAddToLog(lcLANG_MSG_WAREHOUS)
*    *E303419,1 TMI 09/16/2013 [End  ]
*  ELSE
*    SELECT WAREHOUS
*    LOCATE FOR LMATINV
*    IF !FOUND()
*      lcLANG_MSG_LMATINV = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MSG_LMATINV,loFormSet.GetHeaderText("LANG_MSG_LMATINV",loFormSet.HeaderAlias))
*      =lfAddToLog(lcLANG_MSG_LMATINV)
*    ELSE
*      llAllDataNotFound = .F.
*    ENDIF
*  ENDIF
*ENDIF
*B610524,4 TMI 10/09/2013 [End  ]

*- Commented as per Mariam
*!*	IF !gfSEEK('U','ICISTRU')
*!*	  lcLANG_MSG_U_ICISTRU = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MSG_U_ICISTRU,loFormSet.GetHeaderText("LANG_MSG_U_ICISTRU",loFormSet.HeaderAlias))
*!*	  =lfAddToLog(lcLANG_MSG_U_ICISTRU)
*!*	ELSE
*!*	  llAllDataNotFound = .F.
*!*	ENDIF

*!*	IF !gfSEEK('M','ICISTRU')
*!*	  lcLANG_MSG_M_ICISTRU = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MSG_M_ICISTRU,loFormSet.GetHeaderText("LANG_MSG_M_ICISTRU",loFormSet.HeaderAlias))
*!*	  =lfAddToLog(lcLANG_MSG_M_ICISTRU)
*!*	ELSE
*!*	  llAllDataNotFound = .F.
*!*	ENDIF

SELECT SCALE
IF !gfSeek('S')
	lcLANG_MSG_SCALE = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MSG_SCALE,loFormSet.GetHeaderText("LANG_MSG_SCALE",loFormSet.HeaderAlias))
	=lfAddToLog(lcLANG_MSG_SCALE)
ELSE
	llAllDataNotFound = .F.
ENDIF

*- check color
IF !gfSeek('N'+'COLOR     ','CODES','CCODE_NO')
	lcLANG_MSG_COLOR = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MSG_COLOR,loFormSet.GetHeaderText("LANG_MSG_COLOR",loFormSet.HeaderAlias))
	=lfAddToLog(lcLANG_MSG_COLOR)
ELSE
	llAllDataNotFound = .F.
ENDIF

*- codes needed for customer screen
IF !gfSeek('N'+'CLASS','CODES')
	lcLANG_MSG_CLASS = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MSG_CLASS,loFormSet.GetHeaderText("LANG_MSG_CLASS",loFormSet.HeaderAlias))
	=lfAddToLog(lcLANG_MSG_CLASS)
ELSE
	llAllDataNotFound = .F.
ENDIF


IF !gfSeek('N'+'CTERMCODE','CODES')
	lcLANG_MSG_CTERMCODE = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MSG_CTERMCODE,loFormSet.GetHeaderText("LANG_MSG_CTERMCODE",loFormSet.HeaderAlias))
	=lfAddToLog(lcLANG_MSG_CTERMCODE)
ELSE
	llAllDataNotFound = .F.
ENDIF

IF !gfSeek('N'+'REGION','CODES')
	lcLANG_MSG_REGION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MSG_REGION,loFormSet.GetHeaderText("LANG_MSG_REGION",loFormSet.HeaderAlias))
	=lfAddToLog(lcLANG_MSG_REGION)
ELSE
	llAllDataNotFound = .F.
ENDIF

IF !gfSeek('N'+'SHIPVIA','CODES')
	lcLANG_MSG_SHIPVIA = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MSG_SHIPVIA,loFormSet.GetHeaderText("LANG_MSG_SHIPVIA",loFormSet.HeaderAlias))
	=lfAddToLog(lcLANG_MSG_SHIPVIA)
ELSE
	llAllDataNotFound = .F.
ENDIF

IF !gfSeek('N'+'SPCINST','CODES')
	lcLANG_MSG_SPCINST = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MSG_SPCINST,loFormSet.GetHeaderText("LANG_MSG_SPCINST",loFormSet.HeaderAlias))
	=lfAddToLog(lcLANG_MSG_SPCINST)
ELSE
	llAllDataNotFound = .F.
ENDIF

*E303419,1 TMI 09/18/2013 [Start] comment this as per discussion with Mariam
*                                 Reason : to not bother the user with this code as it is not the same importance as the others in the list
*IF gfSeek('MA','SYDAPPL')
*  IF !gfSEEK('N'+'ITEM_TYPE','CODES')
*    lcLANG_MSG_ITEM_TYPE = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MSG_ITEM_TYPE,loFormSet.GetHeaderText("LANG_MSG_ITEM_TYPE",loFormSet.HeaderAlias))
*    =lfAddToLog(lcLANG_MSG_ITEM_TYPE)
*  ELSE
*    llAllDataNotFound = .F.
*  ENDIF
*ENDIF
*E303419,1 TMI 09/18/2013 [End  ]

LOCAL llRunWizardScreen
llRunWizardScreen = .F.

IF !EMPTY(lcLogStr)

	lcPreparingMsg = '************************************************' + lcCr
	lcPreparingMsg = lcPreparingMsg + "Company '+oAriaApplication.ActiveCompanyID + ' has been created successfully, "+lcCr
	lcPreparingMsg = lcPreparingMsg + "still there are some files need to be filled so the user can open and work in company's screens"+lcCr
	lcPreparingMsg = '************************************************' + lcCr
	lcLogStr = lcPreparingMsg + lcLogStr + lcCr+'************************************************' + lcCr
	CREATE CURSOR TMPSTR (mStrRep M(10))
	APPEND BLANK
	replace mStrRep WITH lcLogStr

	lcWinTitl = 'Creation Company Mandatory initial Data Log'
	IF llShowLog
		DO FORM (oAriaApplication.screenhome + 'SM\SMSTRREP') WITH lcWinTitl
	ENDIF
	USE IN TMPSTR

*- if no any of the mandatory data was found then call the wizard screen
	IF llAllDataNotFound
		IF gfModalGen('INM00000B00006',.F.,.F.,.F.,'Open the wizard screen ?') = 1
			llRunWizardScreen = .T.
		ENDIF
	ENDIF
ENDIF

*- close tables, release session, restore old session
*B610532,1 TMI 09/27/2013 [Start] be sure all files are closed
*gfCloseTable('SCALE')
*gfCloseTable('CODES')
IF USED('ICISTRU')
	=gfCloseTable('ICISTRU')
ENDIF
IF USED('SCALE')
	=gfCloseTable('SCALE')
ENDIF
IF USED('CODES')
	=gfCloseTable('CODES')
ENDIF
IF USED('WAREHOUS')
	=gfCloseTable('WAREHOUS')
ENDIF
IF USED('SYDAPPL')
	=gfCloseTable('SYDAPPL')
ENDIF
IF USED('salesrep')
	=gfCloseTable('salesrep')
ENDIF
*B610532,1 TMI 09/27/2013 [End  ]

*B610524,4 TMI 10/10/2013 [Start] wait clear
Wait Clear
*B610524,4 TMI 10/10/2013 [End  ]

RELEASE loSession
SET DATASESSION TO lnOldSession

*- open the wizard screen
IF llRunWizardScreen
*E303419,1 TMI 09/15/2013 [Start] call the wizard screen as modal
*DO (oAriaApplication.ApplicationHome+'SM\smwmain.fxp')
*B610524,4 TMI 10/10/2013 [Start] release procedure
	lcSetOldProc = SET("Procedure")
	IF 'SMCMINF.FXP' $ UPPER(lcSetOldProc)
		RELEASE PROCEDURE (oAriaApplication.ApplicationHome+'SM\SMCMINF.fxp')
	ENDIF
**B610524,4 TMI 10/10/2013 [End  ]

	DO (oAriaApplication.ApplicationHome+'SM\smwmain.fxp') WITH .T.
*E303419,1 TMI 09/15/2013 [End  ]

*B610524,4 TMI 10/10/2013 [Start]   restore procedure
	IF !EMPTY(ALLTRIM(lcSetOldProc))
		SET PROCEDURE TO &lcSetOldProc.
	ENDIF
*B610524,4 TMI 10/10/2013 [End  ]
ENDIF


*E303419,1 TMI 09/15/2013 [Start] check the ICISTRU in company creation after the wizard
*B610524,4 TMI 10/08/2013 [Start] comment this and call the wizard screen after the STYLE CODE structure screen
*IF TYPE('oAriaApplication.lCompanyIsBeingCreated') = 'L'
*  =lfChkICISTRU()
*ENDIF
*B610524,4 TMI 10/08/2013 [End  ]

*- be sure that the SYUSTATC is open
=gfOpenTable(oAriaApplication.SysPath+'SYUSTATC','CUSER_ID','SH')   && COBJ_TYP+ALLTRIM(COBJ_NAME)+CUSER_ID+CSTATION
*E303419,1 TMI 09/16/2013 [End  ]
*- End of lfChkCoDat.

************************************************************
*! Name      : lfAddToLog
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/11/2013
*! Purpose   : fill in the log cursor
*! E303417,1 TMI 09/11/2013
************************************************************
FUNCTION lfAddToLog
*- write this as a class to reuse
PARAMETERS lcLogParam
lcLogStr = lcLogStr + lcCr
lcLogStr = lcLogStr + lcLogParam + lcCr
lcLogStr = lcLogStr + lcCr

*B610524,4 TMI 10/10/2013 [Start] show a wait window
WAIT WINDOW NOWAIT lcLogParam
*B610524,4 TMI 10/10/2013 [End  ]

*- End of lfAddToLog.

************************************************************
*! Name      : lfChkICISTRU
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/15/2013
*! Purpose   : if U not found in ICISTRU the go into a loop to force the user to enter a code structure
*! E303419,1 TMI 09/15/2013
************************************************************
FUNCTION lfChkICISTRU
LOCAL lnSlct,lnI,laStru
lnSlct = SELECT(0)

DO WHILE .T.
	IF USED('ICISTRU')
		USE IN icistru
	ENDIF
	USE (oAriaApplication.DataDir+'ICISTRU') IN 0 ORDER SEGNO && CITEMRECTY+CISEGNO
	IF SEEK('U','ICISTRU')
		USE IN icistru
		EXIT
	ELSE
*- MSG : Style code structure is missing.
*- BTNS: Setup \<Manually;Use Aria \<Default;\<Quit Aria
		lnResp = gfModalGen('INM54041B54042')
		DO CASE
		CASE lnResp = 1
			USE IN icistru
			LOCAL oCodes
			DO FORM (oAriaApplication.screenhome+'IC\iccodes.scx') NAME oCodes NOSHOW WITH .T.
			oCodes.SHOW(1)
			RELEASE oCodes

		CASE lnResp = 2
			llUSEEXSSC = GfGetMemVar('M_USEEXSSC',oAriaApplication.ActiveCompanyID)
			lcCrit = IIF(llUSEEXSSC,'B','A')
			DIMENSION laStru[1,2]
			SELECT * FROM icistru WHERE citemrecty = lcCrit INTO ARRAY laStru
			FOR lnI = 1 TO ALEN(laStru,1)
				laStru[lnI,1] = 'U'
			ENDFOR
			SELECT icistru
			APPEND FROM ARRAY laStru

		CASE lnResp = 3
			=lfQueryUnload()
			QUIT
			RETURN
		ENDCASE
	ENDIF
ENDDO
SELECT (lnSlct )
*- End of lfChkICISTRU.




*!***********************************************************************************************************************
*! Name      	: QueryUnload
*! Developer 	: TMI
*! Date      	: 09/17/2013
*! Purpose   	: Call Exit,
*!              : I copied this function from QueryUnload method in the main screen to quit from Aria greacefully
*! Tracking   	: *E303419,1
*!***********************************************************************************************************************
FUNCTION lfQueryUnload

*-- Check if the active form is modal
IF (TYPE('_SCREEN.ActiveForm.Parent') = 'O' .AND. ;
	!ISNULL(_SCREEN.ActiveForm.Parent) .AND. ;
	_SCREEN.ActiveForm.Parent.WindowType = 1) .OR. ;
	(TYPE('_SCREEN.ActiveForm.Parent') # 'O' .AND. ;
	TYPE('_SCREEN.ActiveForm') = 'O' .AND. ;
	!ISNULL(_SCREEN.ActiveForm) .AND. ;
	_SCREEN.ActiveForm.WindowType = 1)
	NODEFAULT
	RETURN
ENDIF

*-- Check if active form is of class ARIAFORMSET
LOCAL llNoTerminate

IF TYPE('_SCREEN.ActiveForm') = 'O' .AND. !ISNULL(_SCREEN.ActiveForm)

	LOCAL loForm
	loForm = _SCREEN.ActiveForm

	LOCAL lcCurrentForm
	lcCurrentForm = ''

	LOCAL loCurrentFormSet

	IF TYPE('loForm.Parent.Class') = 'C' .AND. !ISNULL(loForm.Parent.Class) .AND. ;
		UPPER(loForm.Parent.Class) == 'ARIAFORMSET'
		LOCAL loFormSet
		loFormSet            = loForm.Parent
		loFormSet.lCloseAria = .T.
		llNoTerminate        = .T.

		loCurrentFormSet = loFormSet

		IF TYPE('loForm.Parent.cHostFormName') = 'C' .AND. ;
			!EMPTY(loForm.Parent.cHostFormName) .AND. ;
			!ISNULL(loForm.Parent.cHostFormName)
			lcCurrentForm = loForm.Parent.cHostFormName
		ENDIF
	ENDIF
ENDIF

*-- Check if any standard windows is opend.
LOCAL lCannotClose
lCannotClose = .F.

LOCAL alForms[_SCREEN.FormCount]
LOCAL lnForm
FOR lnForm = 1 TO _SCREEN.FormCount
	alForms[lnForm] = _SCREEN.Forms(lnForm)
ENDFOR

LOCAL lnForm
FOR lnForm = 1 TO _SCREEN.FormCount
*IF alForms[lnForm].Name = THIS.Name
*  LOOP
*ENDIF

	IF TYPE('alForms[lnForm]') = 'O' .AND. !ISNULL(alForms[lnForm])
		LOCAL loForm
		loForm = alForms[lnForm]

		IF TYPE('loForm.Parent.Class') = 'C' .AND. !ISNULL(loForm.Parent.Class) .AND. ;
			UPPER(loForm.Parent.Class) == 'ARIAFORMSET'

			LOCAL loFormSet
			loFormSet = loForm.Parent

			IF TYPE('loFormSet.oHostForm') = 'O' .AND. !ISNULL(loFormSet.oHostForm) .AND. ;
				TYPE('loFormSet.cHostFormName') = 'C' .AND. ;
				!ISNULL(loFormSet.cHostFormName) .AND. ;
				!EMPTY(loFormSet.cHostFormName) .AND. ;
				loFormSet.cHostFormName == loForm.Name .AND. ;
				!(loFormSet.cHostFormName == lcCurrentForm)

				loFormSet.oHostForm.Show()

				IF loFormSet.ActiveMode $ 'EA' .AND. !loFormSet.otoolbar.mDoCurrentControlValid(.T.)
					lCannotClose = .T.
					EXIT
				ENDIF

				loFormSet.oHostForm.Show()

				IF !loFormSet.oHostForm.mQueryUnload()
					IF TYPE('loFormSet.oHostForm') = 'O' .AND. !ISNULL(loFormSet.oHostForm)
						lCannotClose = .T.
						EXIT
					ENDIF
				ENDIF
			ENDIF
		ENDIF
	ENDIF
ENDFOR
STORE .NULL. TO alForms,loForm,loFormSet

IF lCannotClose
	IF TYPE('loCurrentFormSet') = 'O' .AND. !ISNULL(loCurrentFormSet)
		loCurrentFormSet.lCloseAria = .F.
	ENDIF

*-- Get QueryUnload fired events
	LOCAL laStack[1]
	ASTACKINFO(laStack)

	FOR lnForm = 1 TO _SCREEN.FormCount
		LOCAL loForm
		loForm = _SCREEN.Forms(lnForm)

		IF TYPE('loForm.Parent.Class') = 'C' .AND. !ISNULL(loForm.Parent.Class) .AND. ;
			UPPER(loForm.Parent.Class) == 'ARIAFORMSET'
			LOCAL loFormSet
			loFormSet = loForm.Parent

			IF TYPE('loFormSet.oHostForm') = 'O' .AND. !ISNULL(loFormSet.oHostForm) .AND. ;
				TYPE('loFormSet.cHostFormName') = 'C' .AND. ;
				!ISNULL(loFormSet.cHostFormName) .AND. ;
				!EMPTY(loFormSet.cHostFormName) .AND. ;
				loFormSet.cHostFormName == loForm.Name
				LOCAL lcFormName
				lcFormName = SYS(1272, loFormSet.oHostForm) + '.queryunload'

				IF ASCAN(laStack, lcFormName, -1, -1, 3, 1 + 2) > 0
					loFormSet.lCancelNextQueryUnload = .T.
				ENDIF
			ENDIF
		ENDIF
	ENDFOR

*NODEFAULT
	RETURN
ENDIF

*-- Terminate the application
IF llNoTerminate
*NODEFAULT
	RETURN
ENDIF

CLEAR WINDOWS

LOCAL lnForms,lnCount
lnForms = 0
IF _SCREEN.FORMCOUNT > 0
	FOR lnCount = 1 TO _SCREEN.FormCount
		IF _SCREEN.Forms(lnCount).Visible .AND. ;
			!(_SCREEN.Forms(lnCount).BaseClass = 'Toolbar' .OR. _SCREEN.Forms(lnCount).Name = "_SCREENMAIN")
			lnForms = 1
			Exit
		ENDIF
	ENDFOR
ENDIF

* MAH
*THIS.Visible = .F.
* MAH

IF lnForms < 1
	ON SHUTDOWN
	IF TYPE('oAriaApplication') = 'O' .AND. !ISNULL(oAriaApplication)
		oAriaApplication.Sysexit()
	ENDIF
ENDIF

_SCREEN.Top         = 100
_SCREEN.WindowState = 2


*E303495,1 MMT 07/31/2014 Apply SSO changes in Company information screen[Start]
*!*************************************************************
*! Name      : lfCreateSchema
*! Developer : MMT - Mariam Mazhar
*! Date      : 07/31/2014
*! Purpose   : Create Schema on Azure DB
*!*************************************************************
FUNCTION lfCreateSchema
LPARAMETERS lcClientID, lccompid
PRIVATE laConnInfo
DIMENSION laConnInfo[5]
laConnInfo = ""
LOCAL lcConStr,llCreateNew
lcConStr = oAriaApplication.mreadconstr(lccompid,.F.,@laConnInfo)

IF EMPTY(lcConStr)
	RETURN .F.
ENDIF

DO CASE
CASE laConnInfo[1] = 'SQL'
	LOCAL lcSchemas, lcMasterConStr, lnDataHand, lnAlias, lnCnnResult
	lcSchemas = gfTempName()
	lcMasterConStr =  STRTRAN(lcConStr, "DATABASE=" + ALLTRIM(laConnInfo[3]), "DATABASE=master")
	lnAlias = SELECT(0)

*-- Check if the requested schema is already exists in the (All Clients DATABASE).
	lnDataHand = oAriaApplication.RemoteCompanyData.oconnectionsclass.Open(3, lcConStr, 'SAVE', .F.)
	IF lnDataHand < 1
		SELECT (lnAlias)
		RETURN .F.
	ENDIF
	lnCnnResult = SQLEXEC(lnDataHand,"SELECT * FROM INFORMATION_SCHEMA.SCHEMATA WHERE SCHEMA_NAME = '" + ALLTRIM(laConnInfo[3]) + "'", lcSchemas)

	IF lnCnnResult < 0
		SELECT (lnAlias)
		RETURN .F.
	ENDIF

	IF RECCOUNT(lcSchemas) = 0
		oAriaApplication.RemoteCompanyData.oconnectionsclass.close (lnDataHand)
*-- Create new login on the master database with the name (CLIENID+'_U'COMPID)
*lnDataHand = This.oConnectionsClass.Open(3, lcMasterConStr, 'SAVE', .F.)
		lnDataHand = oAriaApplication.RemoteCompanyData.oconnectionsclass.Open(3, lcMasterConStr, 'SAVE', .F.)
		lnCnnResult = SQLEXEC(lnDataHand, "CREATE LOGIN "+lcClientID+"_U"+lccompid+" WITH PASSWORD = '" + ALLTRIM(laConnInfo[5]) + "'")
		IF lnCnnResult < 0
			SELECT (lnAlias)
			RETURN .F.
		ENDIF
		lnCnnResult = SQLEXEC(lnDataHand, "CREATE USER "+lcClientID+"_U"+lccompid+" FROM LOGIN " + lcClientID+"_U"+lccompid)
		IF lnCnnResult < 0
			SELECT (lnAlias)
			RETURN .F.
		ENDIF
		oAriaApplication.RemoteCompanyData.oconnectionsclass.close (lnDataHand)

*- Create new Schema on the (All Clients DATABASE) with the name (CLIENID+'_LDB'COMPID)
*- Create new User on the (All Clients DATABASE) with the name (CLIENID+'_U'COMPID) and assign the schema as his default schema
*      lnDataHand = This.oConnectionsClass.Open(3, lcConStr, 'SAVE', .F.)
		lnDataHand = oAriaApplication.RemoteCompanyData.oconnectionsclass.Open(3, lcConStr, 'SAVE', .F.)
		lnCnnResult = SQLEXEC(lnDataHand, "CREATE USER "+lcClientID+"_U"+lccompid+" FROM LOGIN "+lcClientID+"_U"+lccompid+"")
		IF lnCnnResult < 0
			SELECT (lnAlias)
			RETURN .F.
		ENDIF
		lnCnnResult = SQLEXEC(lnDataHand, "CREATE ROLE "+lcClientID+"_R"+lccompid+" AUTHORIZATION dbo")
		IF lnCnnResult < 0
			SELECT (lnAlias)
			RETURN .F.
		ENDIF
		lnCnnResult = SQLEXEC(lnDataHand, "EXEC sp_addrolemember '"+lcClientID+"_R"+lccompid+"', '"+lcClientID+"_U"+lccompid+"'")
		IF lnCnnResult < 0
			SELECT (lnAlias)
			RETURN .F.
		ENDIF
		lnCnnResult = SQLEXEC(lnDataHand, "CREATE SCHEMA "+lcClientID+"_LDB"+lccompid+" AUTHORIZATION dbo")
		IF lnCnnResult < 0
			SELECT (lnAlias)
			RETURN .F.
		ENDIF
		lnCnnResult = SQLEXEC(lnDataHand, "GRANT ALTER, DELETE, EXECUTE, INSERT, REFERENCES, SELECT, UPDATE, VIEW DEFINITION ON SCHEMA::"+lcClientID+"_LDB"+lccompid+" TO "+lcClientID+"_R"+lccompid+"")
		IF lnCnnResult < 0
			SELECT (lnAlias)
			RETURN .F.
		ENDIF
		lnCnnResult = SQLEXEC(lnDataHand, "GRANT CREATE TABLE, CREATE PROCEDURE, CREATE FUNCTION, CREATE VIEW TO "+lcClientID+"_R"+lccompid+"")
		IF lnCnnResult < 0
			SELECT (lnAlias)
			RETURN .F.
		ENDIF
		oAriaApplication.RemoteCompanyData.oconnectionsclass.close (lnDataHand)
		lnDataHand = oAriaApplication.RemoteCompanyData.oconnectionsclass.Open(3, lcConStr, 'SAVE', .F.)
		lnCnnResult = SQLEXEC(lnDataHand, "ALTER USER "+lcClientID+"_U"+lccompid+" WITH DEFAULT_SCHEMA = "+lcClientID+"_LDB"+lccompid+"")
		IF lnCnnResult < 0
			SELECT (lnAlias)
			RETURN .F.
		ENDIF
		oAriaApplication.RemoteCompanyData.oconnectionsclass.close (lnDataHand)

	ENDIF

	SELECT (lnAlias)

CASE laConnInfo[1] = 'FOX'
	IF !DIRECTORY(ALLTRIM(laConnInfo[3]))
		MD ALLTRIM(laConnInfo[3])
	ENDIF
ENDCASE

ENDFUNC
*E303495,1 MMT 07/31/2014 Apply SSO changes in Company information screen[End]


*E303949,1 MMT 04/03/2018 Modify the Company setups for the changes of [P20171130.0001][Start]
*!*************************************************************
*! Name      : lfSetRelGL
*! Developer : MMT - Mariam Mazhar
*! Date      : 04/03/2018
*! Purpose   : Call the GL Release format screen
*!*************************************************************
FUNCTION lfSetRelGL
lnOldDataSes = SET("Datasession" )
SET DATASESSION TO loFormSet.DataSessionID
lcOldA4Sys = oAriaApplication.cAria4Sysfiles
oAriaApplication.cAria4Sysfiles= loFormSet.cOriginal_cAria4Sysfiles
=gfCallForm('SMGLSETUP','SM')
oAriaApplication.cAria4Sysfiles = lcOldA4Sys
SET DATASESSION TO &lnOldDataSes.
*E303949,1 MMT 04/03/2018 Modify the Company setups for the changes of [P20171130.0001][End]


*B611650,1 HMS 08/27/2018 GL - Unable to print TB report (Dash Clothing Inc) [T20180611.0026][Start]
*!*************************************************************
*! Name      : lfvLtoGL
*! Developer : HMS - Heba Magdy Selim
*! Date      : 08/27/2018
*! Purpose   : Unable to print TB report
*!*************************************************************
FUNCTION lfvLtoGL

IF M_LINK_GL = 'Y'
	M_GL_VERS ='A'
ENDIF
*B611650,1 HMS 08/27/2018 GL - Unable to print TB report (Dash Clothing Inc) [T20180611.0026][End]








*E611985,1 ES 30/12/2019 Company information changes to create new company as a copy of an existing company. [Start]

*!**************************************************************************
*!
*!      Function: lfValidateComp
*!
*!**************************************************************************
FUNCTION lfValidateComp
PARAMETERS loFormSet,loFld

LOCAL lnResp
DIMENSION laTemp[1]
STORE '' TO laTemp
lcVal=''

if '?' $ loFld.KeyTextBox.Value
	loFld.Selectedfrombrowse  = .T.
ENDIF

IF !USED('SYCCOMP1')
	=gfOpenTable(oAriaApplication.SysPath+"SYCCOMP",'CCOMP_ID','SH','SYCCOMP1')
ENDIF
IF loFld.Selectedfrombrowse
	SELECT SYCCOMP1
	=gfBrows(.F.,"cComp_id,cCom_name","laTemp")
ELSE
	IF !EMPTY(loFormSet.laData[1]) AND !SEEK(loFld.KeyTextBox.Value,'SYCCOMP1')
		=gfBrows(.F.,"cComp_id,cCom_name","laTemp")
	ELSE
		laTemp[1]=loFld.KeyTextBox.Value
	ENDIF
ENDIF
lcVal =laTemp[1]
RETURN lcVal
*- End of lfValidateComp.
*E611985,1 ES 30/12/2019 Company information changes to create new company as a copy of an existing company. [End]


*E611985,1 ES 30/12/2019 Company information changes to create new company as a copy of an existing company. [Start]
*!**************************************************************************
*!
*!      Function: lfCopyCompanyData
*!
*!**************************************************************************
FUNCTION lfCopyCompanyData
PARAMETERS loFormSet,lcComId

loFormSet.lcCopyFrom=lcComId

IF !USED('SYCCOMP1')
	=gfOpenTable(oAriaApplication.SysPath+"SYCCOMP",'CCOMP_ID','SH','SYCCOMP1')
ENDIF
SELECT SYCCOMP1
IF SEEK(lcComId,'SYCCOMP1')
	loFormSet.Ariaform1.txtSourceCompID.Value=loFormSet.lcCopyFrom
	loFormSet.Ariaform1.txtSourceCompName.Value=cCom_name
	lcVal=loFormSet.laData[1]
	lcScFields = 'cComp_id,CCOM_NAME,caddress1,caddress2,caddress3,caddress4,caddress5,caddress6,CCOM_PHON,'+;
	'CCOM_FAX,CCOM_DDIR,CCURR_YER,CCURR_PRD,CCOMPPRNT,MCOMP_MDL,CCONT_CODE,CCURRCODE'
	SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData
	loFormSet.laData[1]=lcVal

	lcDbfPath = IIF(RIGHT(ALLTRIM(loFormSet.laData[11]),1)='\',;
	SUBSTR(ALLTRIM(loFormSet.laData[11]) , 1 , LEN(ALLTRIM(loFormSet.laData[11])) - 3),;
	SUBSTR(ALLTRIM(loFormSet.laData[11]) , 1 , LEN(ALLTRIM(loFormSet.laData[11])) - 2))

	loFormSet.laData[11]=lcDbfPath+UPPER(ALLTRIM(loFormSet.laData[1]))+'\'

ENDIF
*- End of lfCopyCompanyData.
*E611985,1 ES 30/12/2019 Company information changes to create new company as a copy of an existing company. [End]

*E611985,1 ES 30/12/2019 Company information changes to create new company as a copy of an existing company. [Start]
*!**************************************************************************
*!
*!      Function: lfCopyfromCompanyID
*!
*!**************************************************************************
FUNCTION lfCopyfromCompanyID
PARAMETERS lcDbfPath,lcSourseCompID,lcDistCompID,lcSourceDB,lcDistDB
SET STEP ON
=lfCopyDbfS(lcDbfPath,lcSourseCompID,lcDistCompID,'','','')
=lfCopySQLTable(lcSourceDB,lcDistDB)
=lfRemoveAllocation(allt(oAriaApplication.ReadXML()),lcDistCompID,lcDistDB)
*- End of lfCopyfromCompanyID .
*E611985,1 ES 30/12/2019 Company information changes to create new company as a copy of an existing company. [End]


*E611985,1 ES 01/21/2020 Company information changes to create new company as a copy of an existing company. [Start]
*!**************************************************************************
*!
*!      Function: lfCopyDbfS
*!
*!**************************************************************************
FUNCTION lfCopyDbfS
lPARAMETERS lcDbfPath,lcSourseCompID,lcDistCompID,lcsubfolder,lcSrc ,lcDist

LOCAL lcScrDbfPath,lcDisDBFPath,lcDBFScrPath,lcsubfold,i,j
LOCAL ARRAY aListDbfTable[1]

lcsubfold=lcsubfolder
IF EMPTY(lcsubfold)
	lcScrDbfPath=lcDbfPath+lcSourseCompID
	lcDisDBFPath=lcDbfPath+lcDistCompID
ELSE
	IF !EMPTY(lcSrc) AND !EMPTY(lcDist)
		lcScrDbfPath=lcSrc
		lcDisDBFPath=lcDist
	ELSE
		lcScrDbfPath=lcDbfPath+lcSourseCompID+"\"+lcsubfold
		lcDisDBFPath=lcDbfPath+lcDistCompID+"\"+lcsubfold
	ENDIF
	IF  !DIRECTORY(lcDisDBFPath)
		MD (lcDisDBFPath)
	ENDIF

ENDIF

SET DEFAULT TO (lcScrDbfPath)
ADIR(aListDbfTable, '*.*')

IF EMPTY(lcsubfold)

	lnI=1
	lnMyDataSession=SET("Datasession")
	DO WHILE .T.

		DIMENSION laUsedTable[1]
		laUsedTable[1]=''
		IF lnI=1 OR lnI=lnMyDataSession
			lnI=lnI+1
			LOOP
		ELSE

			TRY
				llret=.T.
				AUSED(laUsedTable,lnI)
				SET DATASESSION TO lnI
			CATCH
				llret=.F.
			ENDTRY

			IF llret
				FOR j=1 TO ALEN(laUsedTable,1)
					IF !EMPTY(laUsedTable(j,1)) AND !INList(UPPER(ALLTRIM(laUsedTable(j,1))),UPPER(ALLTRIM('Entity')),UPPER(ALLTRIM('EntityType')),UPPER(ALLTRIM('EntityTypeSettings')))
						USE IN  (laUsedTable(j,1))
					ENDIF
				ENDFOR
				lnI=lnI+1

			ELSE
				EXIT
			ENDIF
		ENDIF
	ENDDO
	SET DATASESSION TO lnMyDataSession
ENDIF


*E612125,1 Es 04/22/2020 Create New Company enhancment [T20200408.0001][Start]
IF !USED('sydfiles1')
	=gfOpenTable(oAriaApplication.SysPath+"sydfiles",'CFILE_NAM','SH','sydfiles1')
ENDIF
*E612125,1 Es 04/22/2020 Create New Company enhancment [T20200408.0001][End]


*E612125,1 Es 04/22/2020 Create New Company enhancment [T20200408.0001][Start]
oProgress1 = NEWOBJECT('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
oProgress1.TotalProgress = ALEN(aListDbfTable,1)
lcCaptionVal1 = LANG_CopyDbfs
oProgress1.lblFirstLabel.CAPTION = lcCaptionVal1
oProgress1.SHOW()
lnCntRecProg1 = 1
*E612125,1 Es 04/22/2020 Create New Company enhancment [T20200408.0001][End]
FOR i=1 TO ALEN(aListDbfTable,1)
	IF !EMPTY(aListDbfTable(i,1))
		lcDBFScrPath=lcScrDbfPath+'\'+aListDbfTable(i,1)
*E612125,1 Es 04/22/2020 Create New Company enhancment [T20200408.0001][Start]
		llCopy=.T.
		IF FILE(lcDisDBFPath+'\'+aListDbfTable(i,1))
			lcScrDate =FDATE(lcDBFScrPath)
			lcScrTime =FTIME(lcDBFScrPath)

			lcDisDate =FDATE(lcDisDBFPath+'\'+aListDbfTable(i,1))
			lcDisTime =FTIME(lcDisDBFPath+'\'+aListDbfTable(i,1))

			IF (lcScrDate =lcDisDate)  AND (lcScrTime =lcDisTime)
				llCopy=.F.
			ENDIF

		ENDIF
		IF llCopy
*E612125,1 Es 04/22/2020 Create New Company enhancment [T20200408.0001][End]

*E612125,1 Es 04/22/2020 Create New Company enhancment [T20200408.0001][Start]
*	WAIT WINDOW NOWAIT LANG_Copy_Msg +aListDbfTable(i,1)
			SELECT sydfiles1
			lcFileName=SUBSTRC(aListDbfTable(i,1),1,AT('.',aListDbfTable(i,1))-1)
			IF gfSeek(lcFileName,'sydfiles1')
				lcTableName=ALLTRIM(sydfiles1.Cfile_ttl)
			ELSE
				lcTableName=aListDbfTable(i,1)
			ENDIF
			WAIT WINDOW NOWAIT LANG_Copy_Msg +lcTableName
*E612125,1 Es 04/22/2020 Create New Company enhancment [T20200408.0001][End]
			COPY FILE (lcDBFScrPath) TO  (lcDisDBFPath)


*E612125,1 Es 04/22/2020 Create New Company enhancment [T20200408.0001][Start]
		ENDIF
		oProgress1.CurrentProgress(lnCntRecProg1)
		lnCntRecProg1  = lnCntRecProg1 + 1
*E612125,1 Es 04/22/2020 Create New Company enhancment [T20200408.0001][End]
	ENDIF

ENDFOR

LOCAL ARRAY aDbfTable[1]
LOCAL lcSrcDBF , lcDistDbf

ADIR(aDbfTable, '',"D")
FOR j=1 TO ALEN(aDbfTable,1)
	IF !EMPTY(aDbfTable(j,1))
		IF ALLTRIM(aDbfTable(j,1))!='.' AND   ALLTRIM(aDbfTable(j,1))!='..'
			IF !EMPTY(lcsubfold)
				lcSrcDBF=lcDbfPath+lcSourseCompID+"\"+lcsubfold+"\"+aDbfTable(j,1)
				lcDistDbf=lcDbfPath+lcDistCompID+"\"+lcsubfold+"\"+aDbfTable(j,1)
				=lfCopyDbfS(lcDbfPath,lcSourseCompID,lcDistCompID,aDbfTable(j,1),lcSrcDBF,lcDistDbf)
			ELSE
				=lfCopyDbfS(lcDbfPath,lcSourseCompID,lcDistCompID,aDbfTable(j,1),'','')
			ENDIF
		ENDIF
	ENDIF
ENDFOR



*E612125,1 Es 04/22/2020 Create New Company enhancment [T20200408.0001][Start]
oProgress1 = Null
*E612125,1 Es 04/22/2020 Create New Company enhancment [T20200408.0001][End]

*- End of lfCopyDbfS.
*E611985,1 ES 01/21/2020 Company information changes to create new company as a copy of an existing company. [End]



*E611985,1 ES 30/12/2019 Company information changes to create new company as a copy of an existing company. [Start]
*!**************************************************************************
*!
*!      Function: lfCopySQLTable
*!
*!**************************************************************************
FUNCTION lfCopySQLTable
PARAMETERS lcSourceDB,lcDistDB
lcSourceDB=ALLTRIM(lcSourceDB)
lcDistDB=ALLTRIM(lcDistDB)
lcSQL = "use "+lcSourceDB+" Select * from sys.tables"
lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'SQLTablesList','', oAriaApplication.activecompanyconstr, 3, 'SAVE', SET("Datasession"))
lnConnHandler=0
SET STEP ON

*E611985,1 ES 01/19/2020 Company information changes to create new company as a copy of an existing company. [Start]
LOCAL AriaSCRPT
lcMergeSetting = SET("Textmerge")
SET TEXTMERGE OFF
TEXT TO AriaSCRPT NOSHOW
CREATE PROCEDURE [dbo].[spCloneTableStructure]
    @SourceSchema nvarchar(255),
    @SourceTable nvarchar(255),
    @DestinationSchema nvarchar(255),
    @DestinationTable nvarchar(255),
    @RecreateIfExists bit = 0
AS
BEGIN

    SET NOCOUNT ON;
    BEGIN TRANSACTION
    if EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_SCHEMA = @DestinationSchema AND TABLE_NAME = @DestinationTable)
    BEGIN
        if @RecreateIfExists = 1
        BEGIN
            exec('DROP TABLE [' + @DestinationSchema + '].[' + @DestinationTable + ']')
        END
        ELSE
            RETURN
    END
    exec('SELECT TOP (0) * INTO [' + @DestinationSchema + '].[' + @DestinationTable + '] FROM [' + @SourceSchema + '].[' + @SourceTable + ']')
    DECLARE @PKSchema nvarchar(255), @PKName nvarchar(255)
    SELECT TOP 1 @PKSchema = CONSTRAINT_SCHEMA, @PKName = CONSTRAINT_NAME FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS WHERE TABLE_SCHEMA = @SourceSchema AND TABLE_NAME = @SourceTable AND CONSTRAINT_TYPE = 'PRIMARY KEY'
    IF NOT @PKSchema IS NULL AND NOT @PKName IS NULL
    BEGIN
        DECLARE @PKColumns nvarchar(MAX)
        SET @PKColumns = ''
        SELECT @PKColumns = @PKColumns + '[' + COLUMN_NAME + '],'
            FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE
            where TABLE_NAME = @SourceTable and TABLE_SCHEMA = @SourceSchema AND CONSTRAINT_SCHEMA = @PKSchema AND CONSTRAINT_NAME= @PKName
            ORDER BY ORDINAL_POSITION
        SET @PKColumns = LEFT(@PKColumns, LEN(@PKColumns) - 1)
        exec('ALTER TABLE [' + @DestinationSchema + '].[' + @DestinationTable + '] ADD  CONSTRAINT [PK_' + @DestinationTable + '] PRIMARY KEY CLUSTERED (' + @PKColumns + ')');
    END
    DECLARE @IndexId int, @IndexName nvarchar(255), @IsUnique bit, @IsUniqueConstraint bit, @FilterDefinition nvarchar(max)
    DECLARE indexcursor CURSOR FOR
    SELECT index_id, name, is_unique, is_unique_constraint, filter_definition FROM sys.indexes WHERE type = 2 and object_id = object_id('[' + @SourceSchema + '].[' + @SourceTable + ']')
    OPEN indexcursor;
    FETCH NEXT FROM indexcursor INTO @IndexId, @IndexName, @IsUnique, @IsUniqueConstraint, @FilterDefinition;
    WHILE @@FETCH_STATUS = 0
       BEGIN
            DECLARE @Unique nvarchar(255)
            SET @Unique = CASE WHEN @IsUnique = 1 THEN ' UNIQUE ' ELSE '' END
            DECLARE @KeyColumns nvarchar(max), @IncludedColumns nvarchar(max)
            SET @KeyColumns = ''
            SET @IncludedColumns = ''
            select @KeyColumns = @KeyColumns + '[' + c.name + '] ' + CASE WHEN is_descending_key = 1 THEN 'DESC' ELSE 'ASC' END + ',' from sys.index_columns ic
            inner join sys.columns c ON c.object_id = ic.object_id and c.column_id = ic.column_id
            where index_id = @IndexId and ic.object_id = object_id('[' + @SourceSchema + '].[' + @SourceTable + ']') and key_ordinal > 0
            order by index_column_id
            select @IncludedColumns = @IncludedColumns + '[' + c.name + '],' from sys.index_columns ic
            inner join sys.columns c ON c.object_id = ic.object_id and c.column_id = ic.column_id
            where index_id = @IndexId and ic.object_id = object_id('[' + @SourceSchema + '].[' + @SourceTable + ']') and key_ordinal = 0
            order by index_column_id
            IF LEN(@KeyColumns) > 0
                SET @KeyColumns = LEFT(@KeyColumns, LEN(@KeyColumns) - 1)
            IF LEN(@IncludedColumns) > 0
            BEGIN
                SET @IncludedColumns = ' INCLUDE (' + LEFT(@IncludedColumns, LEN(@IncludedColumns) - 1) + ')'
            END
            IF @FilterDefinition IS NULL
                SET @FilterDefinition = ''
            ELSE
                SET @FilterDefinition = 'WHERE ' + @FilterDefinition + ' '
            if @IsUniqueConstraint = 0
                exec('CREATE ' + @Unique + ' NONCLUSTERED INDEX [' + @IndexName + '] ON [' + @DestinationSchema + '].[' + @DestinationTable + '] (' + @KeyColumns + ')' + @IncludedColumns + @FilterDefinition)
            ELSE
                BEGIN
                    SET @IndexName = REPLACE(@IndexName, @SourceTable, @DestinationTable)
                    exec('ALTER TABLE [' + @DestinationSchema + '].[' + @DestinationTable + '] ADD  CONSTRAINT [' + @IndexName + '] UNIQUE NONCLUSTERED (' + @KeyColumns + ')');
                END
            FETCH NEXT FROM indexcursor INTO @IndexId, @IndexName, @IsUnique, @IsUniqueConstraint, @FilterDefinition;
       END;
    CLOSE indexcursor;
    DEALLOCATE indexcursor;
    DECLARE @ConstraintName nvarchar(max), @CheckClause nvarchar(max)
    DECLARE constraintcursor CURSOR FOR
        SELECT REPLACE(c.CONSTRAINT_NAME, @SourceTable, @DestinationTable), CHECK_CLAUSE from INFORMATION_SCHEMA.CONSTRAINT_TABLE_USAGE t
        INNER JOIN INFORMATION_SCHEMA.CHECK_CONSTRAINTS c ON c.CONSTRAINT_SCHEMA = TABLE_SCHEMA AND c.CONSTRAINT_NAME = t.CONSTRAINT_NAME
         WHERE TABLE_SCHEMA = @SourceSchema AND TABLE_NAME = @SourceTable
    OPEN constraintcursor;
    FETCH NEXT FROM constraintcursor INTO @ConstraintName, @CheckClause;
    WHILE @@FETCH_STATUS = 0
       BEGIN
            exec('ALTER TABLE [' + @DestinationSchema + '].[' + @DestinationTable + '] WITH CHECK ADD  CONSTRAINT [' + @ConstraintName + '] CHECK ' + @CheckClause)
            exec('ALTER TABLE [' + @DestinationSchema + '].[' + @DestinationTable + '] CHECK CONSTRAINT [' + @ConstraintName + ']')
            FETCH NEXT FROM constraintcursor INTO @ConstraintName, @CheckClause;
       END;
    CLOSE constraintcursor;
    DEALLOCATE constraintcursor;
    
    --B612559,1 MMT 05/09/2022 Copying company from another company using company information screen does not copy the SQL table structure correctly[T20220427.0001][Start]
    declare @table_name sysname, @new_table sysname, @cmd varchar(max)
    select @table_name = @SourceTable, @cmd = '', @new_table =@SourceTable
    select @cmd = @cmd+'ALTER TABLE [' + @DestinationSchema + '].[' + @DestinationTable + '] ADD CONSTRAINT [DF_' +@new_table+'_'+a.name+'] DEFAULT '+b.definition+' FOR['+a.name+']; 
' 
from sys.columns a 
join sys.default_constraints b on a.object_id = b.parent_object_id and a.column_id = b.parent_column_id
where a.object_id= object_id(@SourceTable)
    exec (@cmd)
    --B612559,1 MMT 05/09/2022 Copying company from another company using company information screen does not copy the SQL table structure correctly[T20220427.0001][End]
    
    COMMIT TRANSACTION
END
ENDTEXT

AriaSCRPT = TEXTMERGE(AriaSCRPT)
SET TEXTMERGE &lcMergeSetting.

lnConnHand=0
lnSQLRunProc = oAriaApplication.RemoteCompanyData.SQLRun("use "+lcSourceDB+"; drop PROCEDURE [dbo].[spCloneTableStructure]" ,'','', oAriaApplication.activecompanyconstr, 3, 'SAVE', SET("Datasession"),.T.,@lnConnHand)
IF lnSQLRunProc >0
	=SQLCOMMIT(lnConnHand)
ENDIF


lnSQLRunProc = oAriaApplication.RemoteCompanyData.SQLRun(AriaSCRPT ,'','', oAriaApplication.activecompanyconstr, 3, 'SAVE', SET("Datasession"),.T.,@lnConnHand)
IF lnSQLRunProc >0
	=SQLCOMMIT(lnConnHand)
ENDIF
*E611985,1 ES 01/19/2020 Company information changes to create new company as a copy of an existing company. [End]

*xxxx
lcSQL = "use "+lcSourceDB+" SELECT  TABLE_NAME FROM INFORMATION_SCHEMA.COLUMNS where  COLUMNPROPERTY(object_id(TABLE_SCHEMA+'.'+TABLE_NAME), COLUMN_NAME, 'IsIdentity') = 1"

lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'SQLIdentityTables','', oAriaApplication.activecompanyconstr, 3, 'SAVE', SET("Datasession"))
SELECT SQLIdentityTables
=CURSORSETPROP("Buffering",3)
INDEX ON TABLE_NAME TAG TABLE_NAME
*xxxx

*E612125,1 Es 04/22/2020 Create New Company enhancment [T20200408.0001][Start]
oProgress2 = NEWOBJECT('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
oProgress2.TotalProgress = RECCOUNT('SQLTablesList')
lcCaptionVal2 = LANG_CopySQL
oProgress2.lblFirstLabel.CAPTION = lcCaptionVal2
oProgress2.SHOW()
lnCntRecProg2 = 1
*E612125,1 Es 04/22/2020 Create New Company enhancment [T20200408.0001][End]
SELECT SQLTablesList
SCAN

**Get Tables Name
	lcTableName=ALLTRIM(SQLTablesList.name)


*E612125,1 Es 04/22/2020 Create New Company enhancment [T20200408.0001][Start]
	llCopyTable=lfCopyTable(lcTableName,lcSourceDB,lcDistDB)
	IF llCopyTable
*E612125,1 Es 04/22/2020 Create New Company enhancment [T20200408.0001][End]

*E611985,1 ES 01/19/2020 Company information changes to create new company as a copy of an existing company. [Start]
		lcSQL = "use "+lcDistDB+" SELECT * from "+lcTableName+""
		lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'','', oAriaApplication.activecompanyconstr, 3, 'SAVE', SET("Datasession"))

		IF lnSQLRunResult >0
			lcSQL = "use "+lcDistDB+" Drop Table "+lcTableName+""
			lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'','', oAriaApplication.activecompanyconstr, 3, 'SAVE', SET("Datasession"),.T.,@lnConnHand)
			IF lnSQLRunResult >0
				=SQLCOMMIT(lnConnHand)
			ENDIF
		ENDIF
		lcSQL ="use "+lcSourceDB+"; exec  [spCloneTableStructure]   @SourceSchema = '"+lcSourceDB+"].[dbo',"+;
		" @SourceTable ='"+lcTableName+"',"+;
		" @DestinationSchema ='"+lcDistDB+"].[dbo',"+;
		" @DestinationTable ='"+lcTableName+"',"+;
		" @RecreateIfExists =1"

		lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'','', oAriaApplication.activecompanyconstr, 3, 'SAVE', SET("Datasession"),.T.,@lnConnHand)
		IF lnSQLRunResult >0
			=SQLCOMMIT(lnConnHand)
		ENDIF
*E611985,1 ES 01/19/2020 Company information changes to create new company as a copy of an existing company. [End]

		lcColumnsStr=''
		lcSQL = "use "+lcSourceDB+" SELECT  COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS where TABLE_NAME = '"+lcTableName+"'"
		lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'SQLTableColumns','', oAriaApplication.activecompanyconstr, 3, 'SAVE', SET("Datasession"))


**Get Columns Name
		SELECT SQLTableColumns
		lcRecNo=reccount()
		SCAN
** Not Last Record
			IF RECNO()!=lcRecNo
				lcColumnsStr=lcColumnsStr+"["+ALLTRIM(SQLTableColumns.COLUMN_NAME)+"],"
			ELSE
				lcColumnsStr=lcColumnsStr+"["+ALLTRIM(SQLTableColumns.COLUMN_NAME)+"]"
			ENDIF
		ENDSCAN
*xxxx
		llfound=.F.
		SELECT SQLIdentityTables
		IF gfseek(lcTableName)
			llfound=.T.
		ENDIF

		IF llfound
			lcSQL = "use "+lcDistDB+" SET IDENTITY_INSERT &lcDistDB..dbo." + lcTableName + " ON ;"
			lcSQL = lcSQL+" INSERT INTO &lcDistDB..dbo."+lcTableName+"("+lcColumnsStr+") Select "+lcColumnsStr+" from &lcSourceDB..dbo."+lcTableName+""
			lcSQL = lcSQL +"  use "+lcDistDB+ "  SET IDENTITY_INSERT &lcDistDB..dbo." + lcTableName + " OFF ;"
		ELSE
			lcSQL = "use "+lcDistDB+" INSERT INTO &lcDistDB..dbo."+lcTableName+"("+lcColumnsStr+") Select "+lcColumnsStr+" from &lcSourceDB..dbo."+lcTableName+""
		ENDIF

*E612125,1 Es 04/22/2020 Create New Company enhancment [T20200408.0001][Start]
*	WAIT WINDOW NOWAIT LANG_Copy_Msg +lcTableName
		IF !USED('sydfiles1')
			=gfOpenTable(oAriaApplication.SysPath+"sydfiles",'CFILE_NAM','SH','sydfiles1')
		ENDIF
		SET STEP ON
		SELECT sydfiles1
		IF gfSeek(UPPER(lcTableName),'sydfiles1')
			lcTable=ALLTRIM(sydfiles1.Cfile_ttl)
		ELSE
			lcTable=lcTableName
		ENDIF
		WAIT WINDOW NOWAIT LANG_Copy_Msg +lcTable
*E612125,1 Es 04/22/2020 Create New Company enhancment [T20200408.0001][Start]

		lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'','', oAriaApplication.activecompanyconstr, 3, 'SAVE', SET("Datasession"),.T.,@lnConnHand)

*E612125,1 Es 04/22/2020 Create New Company enhancment [T20200408.0001][Start]
		oProgress2.CurrentProgress(lnCntRecProg2)
		lnCntRecProg2  = lnCntRecProg2 + 1
	ENDIF
*E612125,1 Es 04/22/2020 Create New Company enhancment [T20200408.0001][End]

ENDSCAN

SQLCOMMIT(lnConnHand)


*E612125,1 Es 04/22/2020 Create New Company enhancment [T20200408.0001][Start]
oProgress2 = Null
*E612125,1 Es 04/22/2020 Create New Company enhancment [T20200408.0001][End]




*- End of lfCopySQLTable.
*E611985,1 ES 30/12/2019 Company information changes to create new company as a copy of an existing company. [End]


*E611985,1 ES 30/12/2019 Company information changes to create new company as a copy of an existing company. [Start]
*!**************************************************************************
*!
*!      Function: lfRemoveAllocation
*!
*!**************************************************************************
FUNCTION lfRemoveAllocation
PARAMETERS lcClientID,lcNewCompanyID,lcDatabase_id
lcSQL = "use [system.master]  update [system.master].[dbo].[SizeAllocation] set Status='C' where ClientID='"+lcClientID +"' and cComp_ID='"+lcNewCompanyID +"'"
lnConnHandler=0
lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'','', oAriaApplication.SystemMasterConnectionString, 3, 'SAVE', SET("Datasession"),.T.,@lnConnHandler)
IF lnSQLRunResult >0
	SQLCOMMIT(lnConnHandler)
ENDIF
*- End of lfRemoveAllocation.
*E611985,1 ES 30/12/2019 Company information changes to create new company as a copy of an existing company. [End]


*E611985,1 ES 30/12/2019 Company information changes to create new company as a copy of an existing company. [Start]
*!**************************************************************************
*!
*!      Function: lfSizeCheck
*!
*!**************************************************************************
FUNCTION lfSizeCheck
PARAMETERS lcClientID,lcNewComp_ID,lcOldComp_ID,lcDatabase_id
lcXMLFile=oAriaApplication.DefaultPath+'\SpaceInfo.XML'
lcXMLFILEStr= FILETOSTR(lcXMLFile)
XMLTOCURSOR(lcXMLFILEStr,"XmlCursor")
SELECT XmlCursor
lcSQLPath=XmlCursor.SQLPath
lcDBFsPath=XmlCursor.DBFsPath

lcSQLPathLastChar= SUBSTR(ALLTRIM(lcSQLPath),LEN(ALLTRIM(lcSQLPath)),1)
lcDBFsPathLastChar= SUBSTR(ALLTRIM(lcDBFsPath),LEN(ALLTRIM(lcDBFsPath)),1)

IF lcSQLPathLastChar!="\"
	lcSQLPath=ALLTRIM(lcSQLPath)+"\"
ENDIF

IF lcDBFsPathLastChar!="\"
	lcDBFsPath=ALLTRIM(lcDBFsPath)+"\"
ENDIF

lcEmail=XmlCursor.Email

*E611985,1 ES 01/12/2019 Company information changes to create new company as a copy of an existing company. [Start]
lcSetDefaultPath = FULLPATH('')
*E611985,1 ES 01/12/2019 Company information changes to create new company as a copy of an existing company. [End]

*Disk Free Space
SET DEFAULT TO (lcDBFsPath+lcOldComp_ID)
lnDBFSFreeSpace=DISKSPACE()
lnDBFSFreeSpace=lnDBFSFreeSpace/1024/1024/1024
*B612488,1 MMT 11/10/2021 Company information screen gives an error while creating new company copy from existing company if the SQL server folder is inaccessible from the data server[T20211027.0002][Start]
*!*	SET DEFAULT TO (lcSQLPath+lcClientID)
*!*	lnSQLFreeSpace=DISKSPACE()
*!*	lnSQLFreeSpace=lnSQLFreeSpace/1024/1024/1024
lnSQLFreeSpace= 0 
lcSQL = "EXEC MASTER..xp_fixeddrives"
lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'SQLDSKSP','', oAriaApplication.activecompanyconstr, 3, 'SAVE', SET("Datasession"))
IF lnSQLRunResult > 0 
 SELECT SQLDSKSP
 LOCATE FOR ALLTRIM(UPPER(Drive)) = UPPER(LEFT(lcSQLPath,1))
 IF FOUND()
   lnSQLFreeSpace = SQLDSKSP.MB_Free
 ENDIF
ENDIF
lnSQLFreeSpace=lnSQLFreeSpace/1024
*!*	*B612488,1 MMT 11/10/2021 Company information screen gives an error while creating new company copy from existing company if the SQL server folder is inaccessible from the data server[T20211027.0002][End]



*E611985,1 ES 01/12/2019 Company information changes to create new company as a copy of an existing company. [Start]
SET DEFAULT TO (lcSetDefaultPath )
*E611985,1 ES 01/12/2019 Company information changes to create new company as a copy of an existing company. [End]


*Min Space DBFs & Min Space SQL

*E612125,1 Es 04/30/2020 Create New Company enhancment [T20200408.0001][Start]
*!*	lnMinSpaceDBFs= IIF(XmlCursor.MinSpaceDBFs=.f.,0,XmlCursor.MinSpaceDBFs)
*!*	lnMinSpaceSQL= IIF(XmlCursor.MinSpaceSQL=.f.,0,XmlCursor.MinSpaceSQL)
lnMinSpaceDBFs=XmlCursor.MinSpaceDBFs
lnMinSpaceSQL=XmlCursor.MinSpaceSQL

IF TYPE("lnMinSpaceDBFs")='L'
	lnMinSpaceDBFs= IIF(lnMinSpaceDBFs=.T.,1,0)
ENDIF
IF TYPE("lnMinSpaceSQL")='L'
	lnMinSpaceSQL= IIF(lnMinSpaceSQL=.T.,1,0)
ENDIF
*E612125,1 Es 04/30/2020 Create New Company enhancment [T20200408.0001][End]


*DBFS Allocation
lnDBFSAllocation =0

*SQL Allocation
lnSQLAllocation =0

lcUserID=oAriaApplication.user_id

*E611985,1 ES 01/12/2019 Company information changes to create new company as a copy of an existing company. [Start]
lcSQL =  "use [system.master] select * from [system.master].[dbo].[SizeAllocation] where Status='O'"
lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'lcTmpSizeAllocation','', oAriaApplication.SystemMasterConnectionString, 3, 'SAVE', SET("Datasession"))
*E611985,1 ES 01/12/2019 Company information changes to create new company as a copy of an existing company. [End]

SELECT lcTmpSizeAllocation
SCAN
	lnDBFSTargetPathSz=lfGetFolderSize(lcTmpSizeAllocation.DBFSTargetPath)
	lnDBFSAllocation =lnDBFSAllocation +lcTmpSizeAllocation.RequiredSpaceDBFS-lnDBFSTargetPathSz

	lnSQLTargetPathSz=lfGetFolderSize(lcTmpSizeAllocation.SQLTargetPath)
	lnSQLAllocation  =lnSQLAllocation +lcTmpSizeAllocation.RequiredSpaceSQL-lnSQLTargetPathSz
ENDSCAN


*DBFS Required Space
lnRequiredSpaceDBFS=lfGetFolderSize(lcDBFsPath+lcOldComp_ID)

*SQL Required Space
lcSQL = "SELECT DB_NAME(database_id) AS DBName,Name AS Logical_Name, Physical_Name,(size*8)/1024 SizeMB FROM sys.master_files WHERE DB_NAME(database_id) ='"+lcDatabase_id+"'"
lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'lcSizeMB','', oAriaApplication.activecompanyconstr, 3, 'SAVE', SET("Datasession"))
SELECT lcSizeMB
lnRequiredSpaceSQL=lcSizeMB.SizeMB
lnRequiredSpaceSQL=lnRequiredSpaceSQL/1024

llret=.F.
IF lnDBFSFreeSpace> (lnMinSpaceDBFs+lnDBFSAllocation +lnRequiredSpaceDBFS)
	IF  lnSQLFreeSpace> (lnMinSpaceSQL+lnSQLAllocation +lnRequiredSpaceSQL)
		llret=.T.
	ELSE
		llret=.F.
	ENDIF
ELSE
	llret=.F.
ENDIF

IF llret
	lcUserID=oAriaApplication.user_id
*E611985,1 ES 01/12/2019 Company information changes to create new company as a copy of an existing company. [Start]
	lcSQL = "use [system.master]  insert into [system.master].[dbo].[SizeAllocation] (ClientID,cComp_ID,RequiredSpaceDBFS,RequiredSpaceSQL,Status,UserID,DBFSTargetPath,SQLTargetPath) values('"+lcClientID+"','"+lcNewComp_ID+"','"+STR(lnRequiredSpaceDBFS)+"','"+STR(lnRequiredSpaceSQL)+"','O','"+lcUserID+"','"+(lcDBFsPath+lcOldComp_ID)+"','"+(lcSQLPath+"\"+lcClientID)+"')"
	lnConnHandler=0
	lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'','', oAriaApplication.SystemMasterConnectionString, 3, 'SAVE', SET("Datasession"),.T.,@lnConnHandler)
	IF lnSQLRunResult >0
		SQLCOMMIT(lnConnHandler)
	ENDIF
	RETURN .T.
*E611985,1 ES 01/12/2019 Company information changes to create new company as a copy of an existing company. [End]


ELSE
*Do SYMAILER With  'erp_support@ariany.com','aria_123','',lcEmail,'Space issue on data server while creating new company',"Hi All, Please note that there is space issue on data server" In ((oAriaApplication.ApplicationHome+'SY\SYMAILER.FXP'))
	Do SYMAILER With  'Aria.Apps.Notifications@ariany.com','Aria@notifications','',lcEmail,LANG_Email_Header ,LANG_Email_Contact  In ((oAriaApplication.ApplicationHome+'SY\SYMAILER.FXP'))
	RETURN .F.
ENDIF
*- End of lfSizeCheck.
*E611985,1 ES 30/12/2019 Company information changes to create new company as a copy of an existing company. [End]

*E611985,1 ES 01/02/2020 Company information changes to create new company as a copy of an existing company. [Start]
*!**************************************************************************
*!
*!      Function: lfGetFolderSize
*!
*!**************************************************************************
FUNCTION lfGetFolderSize
PARAMETERS lcfold
oMyFiler = CREATEOBJECT('Filer.FileUtil')
oMyFiler.SearchPath = lcfold && Search Directory
oMyFiler.SubFolder = 1 && includes sub folders
oMyFiler.Find(1) && cumulate sub-folders
LOCAL nSize
nSize = 0
FOR nFileCount = 1 TO oMyFiler.Files.Count
	nSize=nSize+oMyFiler.Files.Item(nFileCount).Size && add the size
ENDFOR
nSize=nSize/1024/1024/1024
RETURN nSize
*- End of lfGetFolderSize.
*E611985,1 ES 01/02/2020 Company information changes to create new company as a copy of an existing company. [End]


*E611985,1 ES 01/05/2020 Company information changes to create new company as a copy of an existing company. [Start]
*!**************************************************************************
*!
*!      Function: lfRefereshCompData
*!
*!**************************************************************************
FUNCTION lfRefereshCompData
PARAMETERS lccompid,lcCopyFrom,loFormSet

IF !EMPTY(lcCopyFrom)
	lnSelOption =gfModalGen("QRM54062B00006","DIALOG")
	IF lnSelOption =1
		IF !USED('SYUSTATC')
			=gfOpenTable(oAriaApplication.caria4syspath+'SYUSTATC','','SH')
		ENDIF


		llfound=.F.
		SELECT SYUSTATC
		LOCATE FOR cObj_name ='OLDVARS' ;
		.AND. SYUSTATC.cComp_id = ALLTRIM(lccompid)

		DO WHILE FOUND()
			lcOldRep = SET('REPROCESS')
			SET REPROCESS TO 1
			IF RLOCK('SYUSTATC')
				UNLOCK IN ALIAS('SYUSTATC')
				SET REPROCESS TO lcOldRep
				CONTINUE
			ELSE
				SET REPROCESS TO lcOldRep
				llfound=.T.
				EXIT
			ENDIF
		ENDDO
		IF llfound
			=gfModalGen("TRM54063B00000","DIALOG")
			RETURN .F.
		ELSE
			IF !USED('SYCCOMP1')
				=gfOpenTable(oAriaApplication.SysPath+"SYCCOMP",'CCOMP_ID','SH','SYCCOMP1')
			ENDIF
			SELECT SYCCOMP1
			IF gfSeek(lccompid,'SYCCOMP1')
				IF gfObj_lock(.T.)
					lcSourceDB=lfGetDbName(lcCopyFrom)
					IF !lfSizeCheck(allt(oAriaApplication.ReadXML()),lccompid,lcCopyFrom,lcSourceDB)
						=gfModalGen("TRM54061B00000","DIALOG")
						RETURN .F.
					ELSE
						lcDbfPath = IIF(RIGHT(ALLTRIM(loFormSet.laData11.Value),1)='\',;
						SUBSTR(ALLTRIM(loFormSet.laData11.Value) , 1 , LEN(ALLTRIM(loFormSet.laData11.Value)) - 3),;
						SUBSTR(ALLTRIM(loFormSet.laData11.Value) , 1 , LEN(ALLTRIM(loFormSet.laData11.Value)) - 2))
						lcDistDB= lfGetDbName(lccompid)
						=lfCopyfromCompanyID(lcDbfPath,lcCopyFrom,lccompid,lcSourceDB,lcDistDB)
*xx
						SELECT SYCCOMP1
*xx
						=gfObj_lock(.F.)
						=gfModalGen("INM54064B00000","DIALOG")
					ENDIF
				ENDIF

			ENDIF
		ENDIF

	ENDIF
ENDIF
*- End of lfRefereshCompData.
*E611985,1 ES 01/05/2020 Company information changes to create new company as a copy of an existing company. [End]


*E611985,1 ES 01/06/2020 Company information changes to create new company as a copy of an existing company. [Start]
*!**************************************************************************
*!
*!      Function: lfGetDbName
*!
*!**************************************************************************
FUNCTION lfGetDbName
PARAMETERS lccompid
lcDbName=''
IF  !USED('SYCCOMP1')
	=gfOpenTable(oAriaApplication.SysPath+"SYCCOMP",'CCOMP_ID','SH','SYCCOMP1')
ENDIF
SELECT SYCCOMP1
IF gfSeek(lccompid,'SYCCOMP1')
	lcDbName=SYCCOMP1.CCONDBNAME
ENDIF
RETURN lcDbName

*- End of lfGetDbName.
*E611985,1 ES 01/06/2020 Company information changes to create new company as a copy of an existing company. [End]




*E612125,1 Es 04/22/2020 Create New Company enhancment [T20200408.0001][Start]
*!**************************************************************************
*!
*!      Function: lfCopyTable
*!
*!**************************************************************************
FUNCTION lfCopyTable
PARAMETERS lcTableName,lcSourceDB,lcDistDB
SET STEP ON
llCopyTable=.T.
llEqual=.T.

lcSQL = "use "+lcSourceDB+" SELECT index_id,last_user_update FROM  sys.dm_db_index_usage_stats i JOIN sys.tables t ON ( t.object_id = i.object_id ) "+;
" WHERE  database_id = DB_ID()  and name='"+lcTableName+"' order by index_id asc"
lnSQLRunResult1 = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'lcSourceTable','', oAriaApplication.activecompanyconstr, 3, 'SAVE', SET("Datasession"))

lcSQL = "use "+lcDistDB+" SELECT index_id,last_user_update FROM  sys.dm_db_index_usage_stats i JOIN sys.tables t ON ( t.object_id = i.object_id ) "+;
" WHERE  database_id = DB_ID()  and name='"+lcTableName+"' order by index_id asc"
lnSQLRunResult2 = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'lcDistTable','', oAriaApplication.activecompanyconstr, 3, 'SAVE', SET("Datasession"))



IF lnSQLRunResult1 >0 AND RECCOUNT('lcSourceTable') >0
	IF lnSQLRunResult2 >0 AND RECCOUNT('lcDistTable') >0
		SELECT lcSourceTable
		SCAN
			SELECT lcDistTable
			LOCATE FOR lcDistTable.index_id = lcSourceTable.index_id
			IF FOUND()

				IF ISNULL(lcSourceTable.last_user_update)
					REPLACE lcSourceTable.last_user_update WITH {}
				ENDIF

				IF ISNULL(lcDistTable.last_user_update)
					REPLACE lcDistTable.last_user_update WITH {}
				ENDIF


				IF lcDistTable.last_user_update !=lcSourceTable.last_user_update
					llEqual=.F.
					EXIT
				ENDIF
			ENDIF

			IF !llEqual
				EXIT
			ENDIF
		ENDSCAN

		IF llEqual
			llCopyTable=.F.
		ENDIF
	ENDIF
ENDIF
RETURN llCopyTable
*- End of lfCopyTable.
*E612125,1 Es 04/22/2020 Create New Company enhancment [T20200408.0001] [End]

*E612505,1 MMT 03/01/2022 Change the Email setting option in option grid to be a screen[T20211208.0002][Start]
*!*************************************************************
*! Name      : lfvEmailSettings
*! Developer : MMT  - Mariam Mazhar
*! Date      : 03/01/2022
*! Purpose   : called from the SM option grid to setup email
*!*************************************************************
FUNCTION lfvEmailSettings
lcRunScx = lfGetScx("SM\SMMAILST.scx")
DO FORM (lcRunScx) WITH loFormSet
*E612505,1 MMT 03/01/2022 Change the Email setting option in option grid to be a screen[T20211208.0002][End]