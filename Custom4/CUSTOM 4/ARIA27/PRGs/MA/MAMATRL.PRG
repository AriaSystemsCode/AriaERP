*!*************************************************************
*! Program File : MAMATRL.PRG
*! Program Desc : Program to Add, inquire and modify materials.
*! For Screen   : MAMATRL.SPR
*!       System : Aria Apparel Series(A27) 
*!       Module : MATERIAL Module 
*! Developer    : Samah Wilson Kirollos (SWK)
*!*************************************************************
*! Calls        :
*!        Procedures :lpShow,lpOptions
*!        
*!        Functions  : lfActFolder(),lfFillFold(),lfvNotes()
*!                     lfGetFab(),lfvdata1(),lfFabClrs()
*!                     lfvkItem(),lfvData2(),lfvkColor()
*!                     lfvFabrBr(),lfvIType(),lfvVendor()
*!                     lfwVendor(),lfvkVendor(),lfvLink()
*!                     lfvkLink(),lfwDom(),lfvDom()
*!                     lfvUUse(),lfvFPrice(),lfvConv()
*!                     lfvRate(),lfvFret(),lfvTaxP()
*!                     lfvQuotaP(),lfvFretP(),lfInit()
*!                     lfvWare(),lfGo2Brows(),lfvInsWar()
*!                     lfvDelWar(),lfvWCancel(),lfvWarOk()
*!                     lfwWareBrs(),lfKeyTrap(),lfWarActiv()
*!                     lfWarDeact(),lfActBrow(),lfvGLWareH()
*!                     lfvGLk(),lfvInvJrnl(),lfvLoc()
*!                     lfvDyeLots(),lfvStyles(),gfCPBrows()
*!                     lfvQuality(),lfvGrade(),lfvQuaFab()
*!                     lfvKQFab(),lfvQuaClr(),lfwField()
*!                     lfvField(),lfvBomCost(),lfActPad()
*!                     
*!*************************************************************
*! Passed :
*!        Parameters : lcFAb : called as inquire screen for the passed fabric
*!                     lcColr: called as inquire screen for the passed fabric/color 
*!*************************************************************
*! Example :
*!        DO MAMATRL
*!***************************************************************************************
*! E300954,1 SWK 08/09/1998 Add a new button in the tool bar for the arranging of dyelots
*! E300954,1                in case of using dyelots 
*! HDM B602297,1 12/01/1998 if there is no colors in the system cannot proceed
*! HDM B602291,1 12/01/1998 Check that Locations file has material inv. location
*! ARH B801864,1 12/16/1998 Fixed the bug ALIAS MMFGORDD not found 
*! ARH B801864,1            if the user is using single bin
*! HDM B602326,1 12/20/1998 Incorrect index for the style table
*! HDM B602355,1 01/03/1999 Bug Of reading MFGOpr. Codde Instead of Fabric in WIP Browse
*! HDM B602382,1 01/05/1999 Also Copy Cost elements if copyfab is True
*! HDM B602427,1 01/11/1999 In MATINVJL If Returned from issued Qty Should Appear like 'Return'
*! HDM B602430,1 01/11/1999 Bug in Item Type Popup The code has been lost I fount it in Purchasing
*!                          Group PopUp And solved too
*! HDM B602470,1 01/24/1999 Check The Purchase Code Value before Saving to avoid saving null values
*! HDM B602487,1 02/22/1999 If We have a new color Average cost should be replace with the
*!                          standard cost
*! HDM B602283,1 02/22/1999 If user selected not to update warehouse file 
*!                          we should not update FabDye File
*! HDM B602606,1 03/02/1999 We have to check that style color exists in style file case
*!                          fabric color same as style color
*! HDM           03/08/1999 Removed Mod_* Fields and added gfAdd_Info() Instead
*! HDM B602606,1 03/15/1999 Alias Style Not Found
*! E301176,1 HDM 03/22/1999 Prevent programs from displaying notepad icon
*                           as it's now controlled globally
*! B602753,1 HDM 04/07/1999 Stop Calling NotePad Program In lpSavScr as the global save
*! B602753,1 HDM 04/07/1999        Will Call it
*! B802100,1 HDM 04/19/1999 Fixing bug color duplication IN color mover
*! B802184,1 HDM 04/22/1999 Fixing bug of not saving average cost including frieght cost
*! B602827,1 HDM 04/26/1999 Fixing bug of saving new cost in fabdye either the user choose Y/N
*! B602692,1 HDM 04/29/1999 Fixing Bug of not displaying styles using certain material in style folder
*! B602880,1 HDM 05/09/1999 Fixing bug of Record out of range when checking dyelot field
*! B802220,1 HDM 05/10/1999 Enhance the on order data collecting
*! B802302,1 SWK 05/28/1999 Add the Gl_ling to the Screen field changed
*! E301268,1 HDM 06/21/1999 filter vendor browse by sup. type
*! B603071,1 MAN 07/18/1999 Adjust the color length in the scan expr.
*! B603038,1 HDM 08/12/1999 Fixing bug of lsSource Not Found
*!                          Fabric file is not the selected alias when returnning from
*!                          Styles Folder (Select It)
*! E301234,1 HDM  09/19/1999 Call Material Manufacturin order Screen
*! B802766,1 HDM  11/09/1999 Make Sure that fabdye file index is fabdye
*! B802838,1 AMM  12/01/1999 Get the right data in the style folder browse
*! B603324,1 HDM  01/20/2000 - Enable 'UOM Buy' Field and make it 'UOM Make' 
*!                             if domestic material
*!                           - Don't Force conversion factor to 1 if domestic material
*! B803248,1 SSE 06/01/2000 Fix Bug Invalid Subscript reference when pressing
*!                          on Fabric Button in Style Screen (View Mode)
*! B803335,1 ADEL 08/06/2000 Correct spelling of word 'receiving.' when browsing receivings for
*! B803335,1                 an item\color having no receivings.
*! B803404,1 ADEL 07/13/2000 Fix the bug of restoring old values of the User Defined Fileds 
*! B803404,1                 before saving the new ones. (i.e Save edited values).
*! B603768,1 ADEL 07/24/2000 Fix the bug of deleting material even if it is used in style
*! B603768,1                 cost sheet or another matreial cost sheet.
*! B803292,1 WAB  08/08/2000 Fix the bug of the style does not show up in the style folder of the 
*! B803292,1 WAB             materials that were used for the style 
*! B803631,1 WAB  08/31/2000 Fix The bug " Pur Grp popup changes to default when changing cost type popup "
*! B603337,1 ABD  09/21/2000 Fix Bug when copy item that the new generated material will
*! B603337,1 ABD             Be saved with the cost of the privious Material. 
*! B604016,1 MHM 11/14/2000    Fix bug of not copy custom fields or save
*! E301582,1 ABD 03/15/2001 Add a new feature in the material screen to import 
*! E301582,1 ABD            Material that is previously exported from the
*! E301582,1 ABD            PDM module. 
*! B604320,1 ABD 03/25/2001 Add new fields to be supported by the material program
*! B604320,1 ABD            When import item from PDM files.
*! B604329,1 WAB 04/03/2001 fix the bug of not import the material cost element 1 
*! B604408,1 WAB 04/18/2001 fix the bug of file does not exist while inporting from PDM Module
*! B804060,1 TMI 04/19/2001 Fix the bug of 'Variable "LASEG" not found
*! B604573,1 WAB 06/22/2001 change the pdmpatern structure and change the fabric message to be Material
*! B604688,1 WAB 07/19/2001 fix the bug of calling the user define field function and the delete setting is OFF
*! B604398,1 MHM 07/31/2001 Fix the bug of message "color code is not found...."
*! B804360,1 WAB 08/09/2001 fix the bug of variable lnPurC does not exist when we call the material 
*! B804360,1 WAB            screen from the Style screen
*! B604669,1 WAB 08/20/2001 fix the bug of when we type the percenteng in the cost  field is not update the cost value fields
*! B604678,1 MHM 09/04/2001 fix the bug of in case of multi location on order browse data is not correct 
*! B604863,1 MHM 10/14/2001 Fix the bug of dublicated qty in the yield field in the style tab folder   
*! C102467,1 ABD 10/21/2001 Restrict adding "Obsolete" colors to style.
*! E301740,4 ABD 10/28/2001 The export program will put he notes into memo filed name 
*! E301740,4 ABD             MNOTES in PDMATRL so the import feauter in 2.7 will get 
*! E301740,4 ABD             the notes from field PDMATRL.mnotes )
*! B603012,1 ADEL 11/25/01 Check if the privilages of the users enabled them to Add or not.
*! B605189,1 RAE 12/09/2001 The browse of onhand details is very slow
*! E301797,1 ADEL 02/11/02 Add button and field for Intransit qty in Quantities folder.
*! B605437,1 ABD 02/20/2002 Fix problem that the program update custom field all color with 
*! B605437,1 ABD            the default value for first color when add new color (customer field).
*! B605725,1 ABD 03/21/2002 Fix problem that the progam didn't copy any material after he
*! B605725,1 ABD            Try to import any material from material file.
*! C200254,1 ABD 04/23/2002 Show the Material Manufacturing Rework order from on the On Order
*! C200254,1 ABD            qty Browse in this program.
*! B605940,1 KHM 05/07/2002 Fix the problem of disabling the WIP & Usage button
*! B605940,1                when the MF not installed
*! B605854,1 ABD 05/09/2002 Fix problem that the Style folder Not showing assigned style
*! B605854,1 ABD            that we assign in the style cost sheet.
*! E301911,1 ABD 05/14/2002 Add new fields and make some changes in the General,Costing and Quantities folder.
*! B605995,1 KHM 05/22/2002 Displaying the MAT PO in case of TranType="1"
*! N000385,2 TMI [Start] Clear typeahead to locate on the write record when called from somat.prg
*! E301967,1 KHM 07/31/2002 Adding the content field to the fabric browse.
*! B604856,1 AMH 08/21/2002 Fix the bug of wrong on order browse in case of multi location M.M.O.
*! C102684,1 ADEL 09/03/02  Add a new screen for Supplier Item\Colors inquiry.
*! B606517,1 KHM 10/08/2002 Fix the bug of WhsLoc file does not exist if the MF not installed.
*! B606663,1 AMH 11/19/2002 Fix the bug of option pad not exist in case of not keep traking of rolls.
*! B607415,1 ABD 07/21/2003 Recalculate the Cost in Forign currecy again in edit mode,
*! B607415,1 ABD            That because the global scatter the ladata field from fabric file.
*! B607483,1 ABD 08/31/2003 Fix bug that when Changing curruncy rates does not update the used or buy cost.
*! B120349,1 ABD 12/11/2003 Fix bug that when a new color is added, all colors change to the 
*! B120349,1 ABD            Item Type selected under the new color.
*! B120471,1 ABD 12/15/2003 Fix bug that when View transactions by roll shows incorrect data into onhand qty.
*! B037361,1 ABD 12/25/2003 Assign Zero to work in process variable to ignore recalculate the work in
*! B037361,1 ABD            Process every time the user press edits an undo.
*! B122673,1 NNA 05/15/2004 change the Vendmatl.Price field name to be (nFabCost) by add
*! B122673,1 NNA            new record in sydflfld.dbf with (Vendmatl & nFabCost) and Replace 
*! B122673,1 NNA            all (Price) with (nFabCost) in all related prg.
*! B038753,1 WSH 12/20/2004 Update Inventory Jounal file with correct value when cost changes
*! B126598,1 ASH 02/27/2005 Fix the bug of 'record is used by another user' when editing the cost.
*! E126684,1 TMI 03/02/2005 Let the sell price be 5 decimals
*!***************************************************************************************
PARAMETER lcFAb ,lcColr
EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc
DECLARE ladata[56],laKeyField[2,4],laFoldWinds[3,2],laAddress[6,3],laDefProc[10],;
        laVariables[1] ,laColor[1,2],laFabTyp[2]

STORE '' TO lcFolder ,laData,laKeyField,laFoldWinds,lcFabric,lcQuality,;
            lcFabric1,lcFabric2,lcQuaFab,lcQuaClr,lcColor1,lcColor2,lcQua1,lcQua2
STORE '' TO lcClrDesc ,lcClrLD,lcOldIType,lcOldVen,lcVenName,laData[15],;
                    OUBuy,laData[17],lcTypeDes,lcModal,lcWinCh1,lcWinCh2,lcWinCh3,;
                    lcWinCh4,lcWinCh5,lcWinCh7,lcWinC31,lcWinCC,lcWinCC1,lcLinkDesc,;
                    lcFrnSmbl1,lcFrnSmbl2,lcFrn1,lcFrn2,lcIType,lcOldValue,lcOldLink,lcOldData,;
                    lcTempWare,lcStyTemp,lcCursName,lcTempClr,lcTmpLots,lcRateP,lcRateD,lcSFields,;
                    lcCurPoNam , lcIType1 , lcIType2 , lcIType3 , lcIType4 , lcIType5 ,lcPurCd
                    
                    
STORE .F. TO laData[14],cbDyelot,lcOldDye,llTranRoll
STORE 0   TO lnCost1, lnCost2,lnCost3,lnCost4,lnBCost1,lnBCost2,lnBCost3,lnBCost4,;
             lnCst2P,lnCst3P,lnCst4P,laData[20],laData[21],laData[22],laData[23],;
             lnOnOrder, lnOnHand,lnWIP,lnUsage
*E301797,1 (Begin) Initialize.
lnInTran = 0
*E301797,1 (End)
*C102684,1 (Begin) Initialize needed variables.
lcWinTit = "Material Reference"
STORE " " TO lcVenCode,lcVenName,lcVenFab,lcVenCol,lcAriaFab,lcAriaClr,lcWinNo,lcWinNo0,lcWinNo3
lcWinNo  = gfTempName()
lcWinNo0 = gfTempName()
lcWinNo3 = gfTempName()
*--Does the company use "Vendor Material Reference"?
llVenRef   = ALLTRIM(gfGetMemVar('M_VENREF'))= 'Y'
llVendQury = .F.
*C102684,1 (End)


*E301911,1 ABD assign name to tmp Files. [Begin]
STORE 0 TO lnSelOrder , lnSelInvce
lcTmpOrdrs = gfTempName()
lcTmpShips = gfTempName()
*E301911,1 ABD- [End]

*! E301911 ABD Initial Win 8 [Begin]
lcWinCh8 = ''
*! E301911 ABD [END]
*--HDM B602606,1 [Start] Added these 2 variables to hold the color position in style structure
llUpdatWar = .F.
STORE 0   TO lnCPos , lnCLen
*--HDM B602606,1 [End]
*B603768,1 (Begin) Declare a variable which determine that if more than one color cannot be deleted
*B603768,1         when choosing  "Move All" from the mover, that the message will appear once.
llDispMess = .T.
*B603768,1 (End)

STORE .T. TO llInquiry
STORE .F. TO llCostUp
STORE 1   TO laData[16],OConvFact,lnExRateP,lnExRateD,lnUnit1,lnUnit2,lnQuality
STORE 1   TO lnType , lnPurCd
*E301911,1 ABD assign 1 to Sales Conv. fields . [Begin]
laData[27] = 1
*E301911,1 ABD [End]

*B804360,1 WAB (Start) - decalre the varaible lnPurC
lnPurC = 1
*B804360,1 WAB (End)
lcWare  = 'All'
*--HDM B602606,1 Define Opened file flag[Start]
STORE .F. TO llOpnStyle
*--HDM B602606,1 Define Opened file flag[End]
*-- Get the old Escape Trap
lcOldEscTrap = ON('Key','Esc')

laDefProc[3] = .F.
laDefProc[4] = .F.
laDefProc[7] = .F.
laDefProc[9] = .F.

*C102467,1 ABD - Restrict adding "Obsolete" colors to style, define
*C102467,1 ABD - Array to hold the Osbsolete colors. [Begin]
DECLARE laObsColor [1]
*C102467,1 ABD - [End]


*--E301077,81 (Start)
*-- Move this part to be after gfsetup

**-- Retsore some global setting from the setup file 
**-- and load them to local variables 
*llMultiWH  = ALLTRIM(gfGetMemVar('M_WareHouse'))= 'Y'
**--HDM B602107,1[START]
*lcIType1   = gfGetMemVar('M_cIType1')
*lcIType2   = gfGetMemVar('M_cIType2')
*lcIType3   = gfGetMemVar('M_cIType3')
*lcIType4   = gfGetMemVar('M_cIType4')
*lcIType5   = gfGetMemVar('M_cIType5')
*lcCostStr  = lcIType1 + lcIType2 + lcIType3 +lcIType4 + lcIType5
*lcFabElmnt = ALLTRIM(STR(ATC('F' , lcCostStr)))
*lcTrimElm  = ALLTRIM(STR(ATC('T' , lcCostStr)))
*lcMType1   = gfGetMemVar('M_cMType1')
*lcMType2   = gfGetMemVar('M_cMType2')
*lcMType3   = gfGetMemVar('M_cMType3')
*lcMType4   = gfGetMemVar('M_cMType4')
*lcMType5   = gfGetMemVar('M_cMType5')      
*lcMCostStr  = lcMType1 + lcMType2 + lcMType3 +lcMType4 + lcMType5
*lcMFabElmt = ALLTRIM(STR(ATC('F' , lcMCostStr)))
*lcMTrimEl  = ALLTRIM(STR(ATC('T' , lcMCostStr)))
**--HDM B602107,1[END]
*lnTypeNo = 0
*STORE ' ' TO laFabTyp
*STORE ' ' TO M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5
*=gfGetMemVar('M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5')
*FOR lnCount = 1 TO 5
*  lcCount = STR(lnCount,1)
*  lcTypes = ''
*  IF M_CMTYPE&lcCount. $ 'FT' AND ! (M_CMTYPE&lcCount. $ lcTypes)
*    lcTypes            = lcTypes  + M_CMTYPE&lcCount.
*    lnTypeNo           = lnTypeNo + 1
*    laFabTyp[lnTypeNo] = lcCount
*    IF lnTypeNo = 2 
*      EXIT
*    ENDIF
*  ENDIF  
*ENDFOR
*llApIstall = (OCCURS('AP',gcCmpModules)<>0)
*llMFIstall = (OCCURS('MF',gcCmpModules)<>0)
*llGl_Link  = ALLTRIM(gfGetMemVar('M_LINK_GL'))   = 'Y' 
*llDyeLot   = ALLTRIM(gfGetMemVar('M_MATDYE'))   = 'Y' 
*llTrkRolls = ALLTRIM(gfGetMemVar('M_TrkRolls'))= 'Y'
*llMultiLoc = ALLTRIM(gfGetMemVar('M_WareLoc'))= 'Y'
**MAN
**llCostPrv  = gfUserPriv('MA','STY100')
*llCostPrv  = gfUserPriv('IC','ICSTYLE','COSTING')
*lcManSLbl1 = gfGetMemVar('M_cTSLbl1')
*lcManSLbl2 = gfGetMemVar('M_cTSLbl2')
*lcManSLbl3 = gfGetMemVar('M_cTSLbl3')
*lcManSLbl4 = gfGetMemVar('M_cTSLbl4')
*llMultCur   = gfGetMemVar('llMulCurr')  
*--E301077,81 (End)

*B120349,1 ABD - Fix bug that when a new color is added, all colors change to the 
*B120349,1 ABD - Item Type selected under the new color, So we will define new 
*B120349,1 ABD - variable to know if the item type was changed or no. [Begin] 
llModifType = .F.
*B120349,1 ABD - [End]


llCalled    = (!EMPTY(lcFAb) AND !EMPTY(lcColr))
lcInvClr    = space(0)
lcVisClr    = RGBSCHEME(1)
lcfolder     = SYS(2015)       && Folder Window Name
lcfoldprnt   = gcBaseWind   && window parent name for the folder
lcwfoldchng  = '=lfActFolder()'     && function to control shows after change the folder
lcfoldpush   = 'pbFolders'     && push button name for the next folder
lnfoldercend = 103.00
lnfolderrend = 2.00
lcSAlClr     = 'All Colors'
DIMENSION  laType[1], laCodInfo [2,10],laPurc[1]
STORE "" TO laType, laCodInfo ,laPurc
llAllColrs  = .F.

*B605437,1 ABD - Fix problem that the program update custom field all color with 
*B605437,1 ABD - The default value for first color when add new color (customer field).
*B605437,1 ABD - So we will copy the Array that hold the user define file to 
*B605437,1 ABD - compere when saving if the value change. [Begin]
DIMENSION  laCpUrFlds [1]
*B605437,1 ABD - [End]

IF !gfSetup()
  RETURN
ENDIF  

*C102684,1 (Begin) This will be custom for Bratex: Initialize needed variables.
IF ASCAN(laEvntTrig , PADR('INITVAR',10)) <> 0
  =gfDoTriger('MAMATRL',PADR('INITVAR',10))
ENDIF
*C102684,1 (End)

*E301797,1 (Begin) Initialize.     
PUBLIC llFromMat
llFromMat = .F.
*E301797,1 (End)

*--E301077,81 (Start)
*-- Retsore some global setting from the setup file 
*-- and load them to local variables 

*B803292,1 WAB - (START) add element for extended size scale
*DIMENSION laSetUp[6,2]

DIMENSION laSetUp[7,2]

*B803292,1 WAB - (END)

laSetUp[1,1] = 'M_WareHouse'
laSetUp[2,1] = 'M_cIType1'
laSetUp[3,1] = 'M_cIType2'
laSetUp[4,1] = 'M_cIType3'
laSetUp[5,1] = 'M_cIType4'
laSetUp[6,1] = 'M_cIType5'

*B803292,1 WAB - (START) add elemnt for extended size scale 
laSetUp[7,1] = 'M_USEEXSSC'
*B803292,1 WAB - (END)

=gfGetMemVar(@laSetUp)
llMultiWH  = ALLTRIM(laSetUp[1,2])= 'Y'
*B803292,1 WAB - (START) correct the array element no . when assgin value to varaibles ( lctypeX )
*lcIType1   = laSetUp[1,2]
*lcIType2   = laSetUp[2,2]
*lcIType3   = laSetUp[3,2]
*lcIType4   = laSetUp[4,2]
*lcIType5   = laSetUp[5,2]
lcIType1   = laSetUp[2,2]
lcIType2   = laSetUp[3,2]
lcIType3   = laSetUp[4,2]
lcIType4   = laSetUp[5,2]
lcIType5   = laSetUp[6,2]
llExtSS    = laSetUp[7,2]
*B803292,1 WAB - (END)

lcCostStr  = lcIType1 + lcIType2 + lcIType3 +lcIType4 + lcIType5
lcFabElmnt = ALLTRIM(STR(ATC('F' , lcCostStr)))
lcTrimElm  = ALLTRIM(STR(ATC('T' , lcCostStr)))

DIMENSION laSetUp[5,2]

*--B602692,1 [Start] COMMENTED BY HDM wrong Copy/Pase We get also 
*laSetUp[1,1] = 'M_cIType1'
*laSetUp[2,1] = 'M_cIType2'
*laSetUp[3,1] = 'M_cIType3'
*laSetUp[4,1] = 'M_cIType4'
*laSetUp[5,1] = 'M_cIType5'

laSetUp[1,1] = 'M_cMType1'
laSetUp[2,1] = 'M_cMType2'
laSetUp[3,1] = 'M_cMType3'
laSetUp[4,1] = 'M_cMType4'
laSetUp[5,1] = 'M_cMType5'

*--B602692,1 [End]

=gfGetMemVar(@laSetUp)
lcMType1   = laSetUp[1,2]
lcMType2   = laSetUp[2,2]
lcMType3   = laSetUp[3,2]
lcMType4   = laSetUp[4,2]
lcMType5   = laSetUp[5,2]
lcMCostStr  = lcMType1 + lcMType2 + lcMType3 +lcMType4 + lcMType5

lcMFabElmt = ALLTRIM(STR(ATC('F' , lcMCostStr)))
lcMTrimEl  = ALLTRIM(STR(ATC('T' , lcMCostStr)))

*B605854,1 ABD Fix problem that the Style folder Not showing assigned style
*B605854,1 ABD that we assign in the style cost sheet that happen because
*B605854,1 ABD The setup file contain 2 fabrics in the IC setpup module. [Begin]
DIMENSION   laMFabNo[1],laIFabNo[1]
STORE '' TO laMFabNo[1],laIFabNo[1]
STORE 0  TO lnAryCount

*-- this loop for Sty. Mfg Cost Elm. Type 1,2,3,4,5
FOR lnCheck = 1 TO 5
  lcFabVar  = "lcMType" + STR(lnCheck,1)
  IF EVAL(lcFabVar) = 'F'
    lnAryCount = lnAryCount + 1
    IF !EMPTY(laMFabNo[1])
      DECLARE laMFabNo[lnAryCount]
    ENDIF
    laMFabNo[lnAryCount] = STR(lnCheck,1)
  ENDIF
ENDFOR
  
lnAryCount = 0
*-- this loop for Sty. Imp Cost Elm. 1,2,3,4,5
FOR lnCheck = 1 TO 5  
  lcFabVar  = "lcIType" + STR(lnCheck,1)
  IF EVAL(lcFabVar) = 'F'
    lnAryCount = lnAryCount + 1
    IF !EMPTY(laIFabNo[1])
      DECLARE laIFabNo[lnAryCount]
    ENDIF
    laIFabNo[lnAryCount] = STR(lnCheck,1)
  ENDIF
ENDFOR
*B605854,1 ABD [End]


lnTypeNo = 0
STORE ' ' TO laFabTyp
STORE ' ' TO M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5
=gfGetMemVar('M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5')

FOR lnCount = 1 TO 5
  lcCount = STR(lnCount,1)
  lcTypes = ''
  IF M_CMTYPE&lcCount. $ 'FT' AND ! (M_CMTYPE&lcCount. $ lcTypes)
    lcTypes            = lcTypes  + M_CMTYPE&lcCount.
    lnTypeNo           = lnTypeNo + 1
    laFabTyp[lnTypeNo] = lcCount
    IF lnTypeNo = 2 
      EXIT
    ENDIF
  ENDIF  
ENDFOR

llApIstall = (OCCURS('AP',gcCmpModules)<>0)
llMFIstall = (OCCURS('MF',gcCmpModules)<>0)

*B605940,1 KHM 05/07/2002 (Begin) Adding a variable to check if the PO module
*B605940,1                is installed or not.
llPOIstall = (OCCURS('PO',gcCmpModules)<>0)
*B605940,1 KHM 05/07/2002 (End)

llCostPrv  = gfUserPriv('IC','ICSTYLE','COSTING')

*E301582,1 ABD - Add a new feature in the material screen to import 
*E301582,1 ABD - Material that is previously exported from the
*E301582,1 ABD - PDM module If instuall.  [Begin]
llSpIstall = (OCCURS('SP',gcComp_Mdl)<>0)
IF llSpIstall
  *B604408,1 - WAB (Start) check first if the files is exist becuase in case of PDM module is installed 
  *B604408,1 - WAB (Start)  the needed files for inmprtoing still not prepared by PDM team  
  *=gfOpenFile(gcDataDir+'PDMATRL',gcDataDir+'PDMATRL','SH')
  *=gfOpenFile(gcDataDir+'PDMLOG',gcDataDir+'PDMLOG','SH')
  llOpnPdm   = FILE(gcDataDir+'PDMATRL.DBF')
  llOpnPdm   = FILE(gcDataDir+'PDMLOG.DBF')
  IF llOpnPdm 
    =gfOpenFile(gcDataDir+'PDMATRL',gcDataDir+'PDMATRL','SH')
    =gfOpenFile(gcDataDir+'PDMLOG',gcDataDir+'PDMLOG','SH')
  ENDIF
  *B604408,1 - WAB (End)
  *E301740,4 ABD - Define new variables [Begin]
  lcPdmNotes = ''
  lcOldsydky =  lcSydKey
  *E301740,4 ABD - [End]
ENDIF

lcOldsydky =  lcSydKey

*E301582,1 ABD - [End]


DIMENSION laSetUp[9,2]
*laSetUp = .F.
laSetUp[1,1] = 'M_LINK_GL'
laSetUp[2,1] = 'M_MATDYE'
laSetUp[3,1] = 'M_TRKROLLS'
laSetUp[4,1] = 'M_WareLoc'
laSetUp[5,1] = 'M_cTSLbl1'
laSetUp[6,1] = 'M_cTSLbl2'
laSetUp[7,1] = 'M_cTSLbl3'
laSetUp[8,1] = 'M_cTSLbl4'
laSetUp[9,1] = 'llMulCurr'

=gfGetMemVar(@laSetUp)

llGl_Link  = ALLTRIM(laSetUp[1,2]) = 'Y' 
llDyeLot   = ALLTRIM(laSetUp[2,2]) = 'Y' 
llTrkRolls = ALLTRIM(laSetUp[3,2]) = 'Y'
llMultiLoc = ALLTRIM(laSetUp[4,2]) = 'Y'
lcManSLbl1 = laSetUp[5,2]
lcManSLbl2 = laSetUp[6,2]
lcManSLbl3 = laSetUp[7,2]
lcManSLbl4 = laSetUp[8,2]
llMultCur  = laSetUp[9,2]
*--E301077,81 (End)

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.F.
laKeyField[1,3] = 'FABRIC'
laKeyField[1,4] = 1

laKeyField[1,1] = 'laData[2]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'COLOR'
laKeyField[1,4] = 2

*-- Get the fields to be displayed for the ladata array
*E301911,1 ABD Add 3 new fields . [Begin]
*lcScFields   = 'Fabric     , Color      , Desc     , Width    , cFabWeight , Pattern   , CONTENT , LOC ,'+;
               'INTRODUCED , LEADTIME   , cComCode , VENDOR   , LINK_CODE  , lTrkRolls , ' +;
               'UOMBUY     , CONV       , UOMUSE   ,cPriceCur , cDutyCur   , COSTBUY   , CostUse,'+;
               'nAveCstBuy , nfAve_Cost , cPurCode'
               
lcScFields   = 'Fabric     , Color      , Desc      , Width      , cFabWeight ,'+;
               'Pattern    , CONTENT    , LOC       , INTRODUCED , LEADTIME   ,'+;
               'cComCode   , VENDOR     , LINK_CODE , lTrkRolls  , UOMBUY     ,'+;
               'CONV       , UOMUSE     , cPriceCur , cDutyCur   , COSTBUY    ,'+;
               'CostUse    , nAveCstBuy , nfAve_Cost, cPurCode   ,nSellPrice  ,'+;
               'cSellUOM   ,nSellConv ,cSlsGlLink'
               
STORE '' TO lcSlinkcde,lcOldSelLk,lcslsGlLnk,lcOldUOM
llBrowse   = .F.
*E301911,1 ABD [End]

DIMENSION laColor[1,2]

*-- If the screen is called as inquire from another screen not from the menu
IF llCalled
  lnCallAlias = SELECT()
  lcRecentlyOpened = gfTempName()
  lcAlreadyOpened  = gfTempName()
  CREATE TABLE (gcWorkDir+lcAlreadyOpened) ;
  (cFile C(8), cTag C(10), cKey C(40), cRefFile1 C(8), cRefExp1 C(80), ;
   cRefFile2 C(8), cRefExp2 C(80),cRefFile3 C(8), cRefExp3 C(80) )
   INDEX ON cFile TAG (lcAlreadyOpened)
  CREATE TABLE (gcWorkDir+lcRecentlyOpened) (cFile C(8))
ENDIF

lnActobj    = 1
llProceed   = .T.
llCopyFab   = .F.
llLoop      = .T.
lcOldField  = SPACE(1)
lnOldField  = SPACE(1)
lcUnMark    = SPACE(1)
lcMark      = ""
lcImpSLbl1  = 'Price'
lcImpSLbl2  = 'Tax'
lcImpSLbl3  = 'Quota'
lcImpSLbl4  = 'Freight'
lcBrowsButs = SPACE (20)
lcWare_Ttl  = "Location"
lnNomOfFo  = 5

*E301582,1 ABD - Add a new Variable hold True for imported style. [Begin]
llImprtFab = .F.
llNotEmprt = .T.
*E301582,1 ABD - [End]


DIMENSION laFoldWinds[lnNomOfFo,2]
llNothing  = lfFillFold()
lcSWinTtl   = 'Styles ' 
lcClrTtl    = 'Item Colors'
lcoldFold    = IIF(llCalled,lcfoldprnt,SPACE(01))
llNoShow   = .F.
* ----------------------------------------------------------

*-- if the system is linked to GL
IF llGl_Link
  =gfOpenFile(gcDataDir+'GL_LINK',gcDataDir+'GL_LINK1','SH')
  LOCATE
  IF !FOUND()
    *-- There is no General Ledger link codes have been setup
    =gfDialog("I","No General Ledger link codes have been setup ...! Cannot proceed...")     
    llProceed = .F.
  ENDIF
ENDIF

*-- If the manifacturing module is installed

*B606517,1 KHM 10/08/2002 (Begin) Comment the following check.
*IF llMFIstall
*B606517,1 KHM 10/08/2002 (End)

  *-- If the system is setup to multi location
  IF llMultiLoc
    =gfOpenFile(gcDataDir+'WhsLoc',gcDataDir+'WhsLocSt','SH')
    *B801864,1 ARH 12/16/1998 (Start) Commented out the following lines to fix the bug 
    *B801864,1 ARH            ALIAS MMFGORDD not found if the user is using single bin
    *=gfOpenFile(gcDataDir+'MMfgOrdD',gcDataDir+'MMfgOrdD','SH')  
    *=gfOpenFile(gcDataDir+'MMfgOrdH',gcDataDir+'MMfgOrdH','SH')
    *B801864,1 ARH 12/16/1998 (End)
    
  ENDIF
  *B801864,1 ARH 12/16/1998 (Start) Moved the opening of the MMFGORDD & MMFGORDH to be opened 
  *B801864,1 ARH            if the MF module is installed only to fix the bug 
  *B801864,1 ARH            ALIAS MMFGORDD not found if the user is using single bin
  =gfOpenFile(gcDataDir+'MMfgOrdD',gcDataDir+'MMfgOrdD','SH')  
  =gfOpenFile(gcDataDir+'MMfgOrdH',gcDataDir+'MMfgOrdH','SH')

  *B606517,1 KHM 10/08/2002 (Begin) Check if the MF module installed.
  *=gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')
  *=gfOpenFile(gcDataDir+'CUTTKTL',gcDataDir+'CUTTKTL','SH')
  IF llMFIstall
    =gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')
    =gfOpenFile(gcDataDir+'CUTTKTL',gcDataDir+'CUTTKTL','SH')
  ENDIF  
  *B606517,1 KHM 10/08/2002 (End)
  
  *B801864,1 ARH 12/16/1998 (End)

  *C200254,1 ABD - Open Needed file, Open Material rework header & Detail. [Begin]
  llMarwhdr = gfOpenFile(gcDataDir+'Marwhdr',gcDataDir+'Marwhdr','SH')
  llMarwlin = gfOpenFile(gcDataDir+'Marwlin',gcDataDir+'Marwlin','SH')
  lcTmpCurs = gfTempName()
  *C200254,1 ABD - [End]

*B606517,1 KHM 10/08/2002 (Begin)  Commented out.
*ENDIF
*B606517,1 KHM 10/08/2002 (End)
*-- HDM B602291,1 12/01/1998 Check that Locations file has material inv. location[START]
IF !USED('WAREHOUS')
  USE(gcDataDir+'WAREHOUS') SHARED IN 0
ENDIF
SELECT WAREHOUS
GOTO TOP
IF EOF()
  =gfModalGen('QRM42080B42001','DIALOG','Locations file is empty.')
  RETURN
ELSE
  LOCATE FOR LMATINV
  IF !FOUND()
    =gfModalGen('QRM42080B42001','DIALOG','No materia inventory Locations found')
    RETURN
  ENDIF
ENDIF
*-- HDM B602291,1 [END]

*-- HDM B602297,1 12/01/1998 if there is no colors in the system cannot proceed[start]
SELECT CODES
SET ORDER TO TAG CCODE_NO

IF !SEEK('N' + PADR('COLOR',6))
  *--No colors found, Cannot Proceed.
  =gfModalGen('QRM42080B42001','DIALOG','No colors found')
  glQuitting = .T.
  RETURN
ENDIF

IF !SEEK('N' + 'ITEM_TYPE')
  *--No ItemTypes found, Cannot Proceed.
  =gfModalGen('QRM42080B42001','DIALOG','No Item Types Found')
  glQuitting = .T.
  RETURN
ENDIF

*-- HDM B602297,1 12/01/1998 if there is no colors in the system cannot proceed[end]
* ----------------------------------------------------------
IF llMultiWH
  lcAll      = 'All'
  DECLARE laWare[1]
  laWare[1] = "All"
  lcWare    = laWare[1]
ENDIF
DIMENSION laDomestic[2,1]
laDomestic[1,1] = "Manufactured"
laDomestic[2,1] = "Purchased"
lnDomestic      = 2
ibDomestic      = 2
lcDomestic      = laDomestic[ibDomestic]
lcCostStat      = IIF(lnDomestic = 2 AND llMultCur,"DISABLE","ENABLE")
lnOldValue      = lnDomestic
lcOldDom        = lcDomestic

*-- Variables used in the Costing Folder.
lcCostType  = IIF(lnDomestic = 1,'M.F.G. Cost','Purchasing Cost')  
lcCstSLbl1  = IIF(lnDomestic = 1,lcManSLbl1,lcImpSLbl1)
lcCstSLbl2  = IIF(lnDomestic = 1,lcManSLbl2,lcImpSLbl2)
lcCstSLbl3  = IIF(lnDomestic = 1,lcManSLbl3,lcImpSLbl3)
lcCstSLbl4  = IIF(lnDomestic = 1,lcManSLbl4,lcImpSLbl4)

*-- Array for the different material grades
DIMENSION laGrade[3,2]
laGrade[1,1] = 'First Quality'
laGrade[1,2] = '1'
laGrade[2,1] = 'Second Quality'
laGrade[2,2] = '2'
laGrade[3,1] = 'Damage'
laGrade[3,2] = '3'

DIMENSION laQuality[2,3]
laQuality[1,1] = "Second Quality"
laQuality[1,2] = SPACE(07)
laQuality[1,3] = SPACE(06)
laQuality[2,1] = "Damage"
laQuality[2,2] = SPACE(07)
laQuality[2,3] = SPACE(06)

*-- Array for the different material transactions done

*- Abdou
*DIMENSION laTrnType[6]
DIMENSION laTrnType[8]
*Abdou

laTrnType[1] = 'Receive'         && Code = '1'
laTrnType[2] = 'Adjustment'      && Code = '2'
laTrnType[3] = 'Physical Inv.'   && Code = '3'
laTrnType[4] = 'Issue'           && Code = '4'
laTrnType[5] = 'Transfer'        && Code = '6'
laTrnType[6] = 'Return  '        && Code = '4'

*- Abdou
laTrnType[7] = 'Invoice  '       && Code = '7'
laTrnType[8] = 'Void Invoice  '  && Code = '8'
*- Abdou

*-- To add the a new icon for the notes in the tool bar
*E300954,1 SWK 08/09/1998 Add new button in the tool bar in case of dyelots 
*E300954,1                to give the user the ability to arrange dyelots
*DECLARE laPanelObj[2,3]
*:E301176,1 HDM 03/22/1999[Start] Prevent programs from displaying notepad icon
*                           as it's now controlled globally

*IF llDyelot
*  DECLARE laPanelObj[3,3]
*ELSE  
*  DECLARE laPanelObj[2,3]
*ENDIF  

IF llDyelot
  DECLARE laPanelObj[2,3]
ELSE  
  DECLARE laPanelObj[1,3]
ENDIF  

STORE '' TO laPanelObj
*laPanelObj[1,1] = 'pbInvNote'
*laPanelObj[1,2] = gcBmpHome+'NOTES2.BMP'
*laPanelObj[1,3] = [VALID lfvNotes()]

laPanelObj[1,1] = 'pbObjlnk'
laPanelObj[1,2] = gcBmpHome+'RELATE.BMP'
laPanelObj[1,3] = [VALID lfvObjLnk()]

IF llDyelot
  laPanelObj[2,1] = 'pbArrDye'
  laPanelObj[2,2] = gcBmpHome+'ARRDYE.BMP'
  laPanelObj[2,3] = [VALID lfDyeRel()]
ENDIF  
*E301176,1 HDM 03/22/1999[End]
IF !WEXIST(gcBaseWind)
  lcTempWare = gfTempName()  
  lcStyTemp  = gfTempName()
  lcCursName = gfTempName()
  lcTmpLots  = gfTempName()
  lcTempClr  = gfTempName()
  lcTmpWIP   = gfTempName()
  
  lcWinCh1   = gfTempName()
  lcWinCh2   = gfTempName()
  lcWinCh3   = gfTempName()
  lcWinC31   = gfTempName()
  lcWinCh4   = gfTempName()
  lcWinCh5   = gfTempName()
  lcWinCh7   = gfTempName()
  lcWinCC    = gfTempName()
  lcWinCC1   = gfTempName()
  lcfolder   = gfTempName()
             
  *E301911,1 ABD - Add new screen to show it if Domastic material . [Begin]
  lcWinCh8   = gfTempName()
  *E301911,1 ABD - [End]

  laCodInfo[1,01] = "ITEM_TYPE"
  laCodInfo[1,02] = "laType"
  laCodInfo[1,03] = "lnType"
  laCodInfo[1,04] = ""
  laCodInfo[1,05] = .F.
  laCodInfo[1,06] = .F.
  laCodInfo[1,07] = "FABRIC"
  laCodInfo[1,08] = "FABRIC"
  laCodInfo[1,09] = IIF(llAllColrs,"laData[1]","laData[1] + laData[2]")
  laCodInfo[1,10] = "Item_Type"

  *--Purchase array
  laCodInfo[2,01] = "CPURCODE"
  laCodInfo[2,02] = "laPurc"
  laCodInfo[2,03] = "lnPurc"
  laCodInfo[2,04] = ""
  laCodInfo[2,05] = .F.            
  laCodInfo[2,06] = .T.            
  laCodInfo[2,07] = "FABRIC"
  laCodInfo[2,08] = "FABRIC"
  laCodInfo[2,09] = IIF(llAllColrs,"laData[1]","laData[1] + laData[2]")
  laCodInfo[2,10] = "CPURCODE"     
  
  = gfwCodePop( @laCodInfo , "ITEM_TYPE" , "D")
  = gfwCodePop( @laCodInfo , "CPURCODE"  , "D")
  
  SELECT FABRIC
  = AFields(laFileStru)
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'cMarker'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 1
  laFileStru[lnNewFld,4] = 0
  
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'cData'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 30
  laFileStru[lnNewFld,4] = 0
  
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'cStatus'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 1
  laFileStru[lnNewFld,4] = 0

  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'ClrLName'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 50
  laFileStru[lnNewFld,4] = 0

  CREATE TABLE  (gcWorkDir+lcTempClr) FROM ARRAY laFileStru
  INDEX ON  COLOR TAG COLOR OF (gcWorkDir+lcTempClr)

  SELECT ROLLS
  SET ORDER TO TAG ROLLITEM
  
  SELECT FABDYE
  SET ORDER TO TAG Fabdye
  = AFields(laFileStru)
  CREATE TABLE  (gcWorkDir+lcTempWare) FROM ARRAY laFileStru
  INDEX ON FABRIC+COLOR+CWARECODE TAG (lcTempWare) OF (gcWorkDir+lcTempWare)
  SET ORDER TO TAG (lcTempWare)
  
  SELECT MatInvJl
  = AFields(laFileStru)  
  CREATE CURSOR (lcTmpLots) FROM ARRAY laFileStru
    
  IF llMFIstall
    SELECT MMfgOrdD
    = AFields(laFileStru)  
    CREATE TABLE (gcWorkDir+lcCursName) FROM ARRAY laFileStru
  ENDIF
    
  SELECT BOM
  laNothing = AFields(laFileStru)
  CREATE CURSOR (lcStyTemp) FROM ARRAY laFileStru
  INDEX ON cItmMask TAG (lcStyTemp) OF (gcWorkDir+lcStyTemp)
  SET ORDER TO TAG (lcStyTemp)
  
* ----------------------------------------------------------
  SELECT(lcBaseFile)
  SET ORDER TO TAG Fabric 
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  laData[1]  = IIF(llCalled,lcFab,laData[1]) 
  laData[2]  = IIF(llCalled,lcColr,laData[2])   
  SET ORDER TO TAG Fabric
  =lfSameClr()
  laData[1]  = IIF(llCalled,lcFab,laData[1]) 
  laData[2]  = IIF(llCalled,lcColr,laData[2])   
  IF llCalled
    laScrMode[2] = .T.
    laScrMode[1] = .F.
    = lfInit(.T.)
    = lfBColor()

    *E301077,81 (Start)
    *= lfvStyles()
    *E301077,81 (End)
    
    = lfwWare1()
  ENDIF  
  IF llCalled
   =lfCloseFile()
   SELECT (lnCallAlias)
  ENDIF
ENDIF
SELECT(lcBaseFile)
PUSH KEY

*B606663,1 AMH Define the option pad in case of keep track of rolls or Use vendor ref. [Start]
*IF llTrkRolls
IF llTrkRolls .OR. llVenRef
*B606663,1 AMH [End]

  =lfActPad()
ENDIF  
DEFINE PAD _BROWSE OF _MSYSMENU PROMPT "" KEY CTRL+B
ON SELECTION PAD _BROWSE OF _MSYSMENU DO lpTab
*N000385,2 TMI [Start] Clear typeahead to locate on the write record when called from somat.prg
IF CHRSAW()
  CLEAR TYPEAHEAD
ENDIF
*N000385,2 TMI [End  ] Clear typeahead to locate on the write record when called from somat.prg
DO (gcScrDir+gcWinAppl+'\MAMATRL.SPX')
POP KEY
RELEASE PAD _BROWSE OF _MSYSMENU
RELEASE PAD _Option OF _MSYSMENU

*E301582,1 ABD - Close the open file if SP module is instuall. [Begin]
*B604408,1 - WAB (Start) check also if the files exist
*IF llSpIstall 
IF llSpIstall AND llOpnPdm
*B604408,1 - WAB (End)
  =gfCloseFile('PDMATRL')
  =gfCloseFile('PDMLOG')
ENDIF
*E301582,1 ABD - [End]


IF glQuitting
  SELECT (lcTempWare)
  USE
  ERASE (gcWorkDir+lcTempWare+'.DBF')
  ERASE (gcWorkDir+lcTempWare+'.CDX') 
  
  SELECT (lcStyTemp)
  USE
  ERASE (gcWorkDir+lcStyTemp+'.DBF')
  ERASE (gcWorkDir+lcStyTemp+'.CDX') 
   
  IF USED (lcCursName) 
    USE IN (lcCursName) 
  ENDIF  
  ERASE (gcWorkDir+lcCursName+'.DBF')
  ERASE (gcWorkDir+lcCursName+'.CDX') 

  SELECT (lcTmpLots)
  USE
  ERASE (gcWorkDir+lcTmpLots+'.DBF')
  ERASE (gcWorkDir+lcTmpLots+'.CDX') 
  
  SELECT (lcTempClr)
  USE
  ERASE (gcWorkDir+lcTempClr+'.DBF')
  ERASE (gcWorkDir+lcTempClr+'.CDX') 

  *C200254,1 ABD - Close Opened file, close Material rework header & Detail. [Begin]
  IF llMFIstall
    IF llMarwhdr
      = gfCloseFile('MARWHDR')
    ENDIF
   
    IF llMarwlin 
      = gfCloseFile('MARWLIN')
    ENDIF
  ENDIF
  *C200254,1 ABD - [End]
  
ENDIF
*!*************************************************************
*! Name      : lfActFolder
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Activate the selected folder
*!*************************************************************
*! Calls     : lfActClr , lfStyBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfActFolder
PRIVATE lnAlias

lnAlias = SELECT()

DO CASE
  CASE lnactfolder = 1
  	
  	*E301911,1 ABD - Add new screen to show it if Domastic material . [Begin]
  	SHOW GETS WINDOW (lcWinCh8) DISABLE ONLY
  	*E301911,1 ABD - [End]
  	SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
  	SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY 
  	SHOW GETS WINDOW (lcWinCh3) DISABLE ONLY
  	SHOW GETS WINDOW (lcWinC31) DISABLE ONLY
  	SHOW GETS WINDOW (lcWinCC)  DISABLE ONLY
  	SHOW GETS WINDOW (lcWinCC1) DISABLE ONLY    	  	  	
  	SHOW GETS WINDOW (lcWinCh4) DISABLE ONLY   	  	  	
    SHOW WINDOW (lcWinCh5) TOP
    IF laScrMode[1] OR laScrMode[2]
      SHOW GETS WINDOW (lcWinCh5) DISABLE ONLY
    ELSE
      SHOW GETS WINDOW (lcWinCh5) ENABLE ONLY
      IF laScrMode[3]
        SHOW GET lnQuality DISABLE
      ENDIF
    ENDIF

  CASE lnactfolder = 2
  	*E301911,1 ABD - Add new screen to show it if Domastic material . [Begin]
  	SHOW GETS WINDOW (lcWinCh8) DISABLE ONLY
  	*E301911,1 ABD - [End]
  	SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
  	SHOW GETS WINDOW (lcWinCh5) DISABLE ONLY 
  	SHOW GETS WINDOW (lcWinCh3) DISABLE ONLY 
  	SHOW GETS WINDOW (lcWinC31) DISABLE ONLY   	
  	SHOW GETS WINDOW (lcWinCC)  DISABLE ONLY 	
  	SHOW GETS WINDOW (lcWinCC1) DISABLE ONLY   	  	  	
  	SHOW GETS WINDOW (lcWinCh4) DISABLE ONLY
    SHOW WINDOW (lcWinCh2) TOP
    IF laScrMode[2] AND !llAllColrs
      SHOW GETS WINDOW (lcWinCh2) ENABLE ONLY
      SHOW GET lnReorder DISABLE

      *B605940,1 KHM 05/07/2002 (Begin) Adding the checking of the PO also
      *IF llMFIstall
      IF llMFIstall OR llPOIstall
      *B605940,1 KHM 05/07/2002 (End)

        SHOW GET pbWIP       ENABLE
        SHOW GET pbUsage     ENABLE
      ELSE  
        SHOW GET pbWIP       DISABLE
        SHOW GET pbUsage     DISABLE
      ENDIF  
      IF llTrkRolls AND laData[14]
        SHOW GET pbRolls ENABLE
      ELSE
        SHOW GET pbRolls DISABLE      
      ENDIF
      IF llDyeLot AND cbDyelot
        SHOW GET pbDyelot ENABLE
      ELSE
        SHOW GET pbDyelot DISABLE      
      ENDIF
      IF !llMultiLoc
         SHOW GET pbLocation  DISABLE
      ENDIF  
      IF !llMultiWH
        SHOW GET pbWare DISABLE
      ENDIF
    ELSE
      SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
      IF !llAllColrs AND (laScrMode[3] OR laScrMode[4])
        SHOW GET lnReorder ENABLE
      ELSE
        SHOW GET lnReorder DISABLE
      ENDIF  
      IF llMultiWH AND !llAllColrs AND !laScrMode[1]
        SHOW GET pbWare    ENABLE
      ELSE
        SHOW GET pbWare    DISABLE
      ENDIF
      IF llMultiWH AND !llAllColrs AND laScrMode[2]
        SHOW GET lcWare    ENABLE
      ELSE
        SHOW GET lcWare    DISABLE
      ENDIF
    ENDIF
  CASE lnactfolder = 3
  	*E301911,1 ABD - Add new screen to show it if Domastic material . [Begin]
  	SHOW GETS WINDOW (lcWinCh8) DISABLE ONLY
  	*E301911,1 ABD - [End]
  
  	SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
  	SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY 
  	SHOW GETS WINDOW (lcWinCh3) DISABLE ONLY
  	SHOW GETS WINDOW (lcWinC31) DISABLE ONLY
  	SHOW GETS WINDOW (lcWinCC)  DISABLE ONLY
  	SHOW GETS WINDOW (lcWinCC1) DISABLE ONLY    	  	  	
  	SHOW GETS WINDOW (lcWinCh4) DISABLE ONLY   	  	  	
  	SHOW GETS WINDOW (lcWinCh5) DISABLE ONLY   	  	  	
  	IF (laScrMode[3] OR laScrMode[4]) AND llAllColrs
      SHOW GET pbCNew ENABLE
      _CUROBJ = OBJNUM(pbCNew)
  	ENDIF
    =lfActClr()
    
    SELECT(lnAlias)
    SHOW GET ibToColor ENABLE
    
  CASE lnactfolder = 4
  	SHOW GETS WINDOW (lcWinCh5) DISABLE ONLY
  	SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
  	SHOW GETS WINDOW (lcWinCh3) DISABLE ONLY
  	SHOW GETS WINDOW (lcWinC31) DISABLE ONLY    	
  	SHOW GETS WINDOW (lcWinCC)  DISABLE ONLY  	
  	SHOW GETS WINDOW (lcWinCC1) DISABLE ONLY   	  	  	
  	SHOW GETS WINDOW (lcWinCh4) DISABLE ONLY
   
  	*E301911,1 ABD - Add new screen to show it if Domastic material . [Begin]
    *SHOW WINDOW (lcWinCh1) TOP
    IF llMultCur  .AND. lnDomestic = 2
      SHOW GETS WINDOW (lcWinCh8) DISABLE ONLY
      SHOW WINDOW (lcWinCh1) TOP
      laFoldWinds[4,2] = "lcWinCh1"
    ELSE
      SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
      SHOW WINDOW (lcWinCh8) TOP
      laFoldWinds[4,2] = "lcWinCh8"
    ENDIF
    *E301911,1 ABD - [End]
    
    
    IF laScrMode[1] OR laScrMode[2]
      *E301911,1 ABD - Add new screen to show it if Domastic material . [Begin]
      *SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY      
      IF llMultCur  .AND. lnDomestic = 2
        SHOW GETS WINDOW (lcWinCh8) DISABLE ONLY
      ELSE  
        SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
      ENDIF
      *E301911,1 ABD - [End]

    ELSE
      *E301911,1 ABD - Add new screen to show it if Domastic material . [Begin]
      *SHOW GETS WINDOW (lcWinCh1) ENABLE ONLY
      IF llMultCur  .AND. lnDomestic = 2
        SHOW GETS WINDOW (lcWinCh1) ENABLE ONLY
      ELSE
        SHOW GETS WINDOW (lcWinCh8) ENABLE ONLY
      ENDIF
      _CurOBJ = OBJNUM(IBDomestic)
      *E301911,1 ABD - [End]

      *B603324,1 [Start] Always enable UOM Make / Use regardless
      *                  it's imported or manufactured
      *IF lnDomestic = 2
      *  SHOW GET laData[15] ENABLE
      *ELSE
      *  SHOW GET laData[15] DISABLE
      *ENDIF
      SHOW GET laData[15] ENABLE
      *B603324,1 [End]
      IF lnDomestic = 2 AND llMultCur
        SHOW GET lnCost1    ENABLE
        SHOW GET lnCost2    ENABLE
        SHOW GET lnCost3    ENABLE
        SHOW GET lnCost4    ENABLE
        SHOW GET laData[18] ENABLE
        SHOW GET laData[19] ENABLE
        SHOW GET ibPCurr    ENABLE
        SHOW GET ibDCurr    ENABLE
      ELSE
        *E301911,1 ABD - Don't show this field in case ! WINCh 8 on top. [Begin]
        IF !WONTOP(lcWinCh8)
          *E301911,1 ABD - [End]
          SHOW GET lnCost1    DISABLE
          SHOW GET lnCost2    DISABLE
          SHOW GET lnCost3    DISABLE
          SHOW GET lnCost4    DISABLE
          SHOW GET laData[18] DISABLE
          SHOW GET laData[19] DISABLE
          SHOW GET ibPCurr    DISABLE
          SHOW GET ibDCurr    DISABLE
          *E301911,1 ABD - Don't show this field in case ! WINCh 8 on top. [Begin]
         ENDIF 
         *E301911,1 ABD - [End]
      ENDIF  
      SHOW GET lnBCost1   &lcCostStat
      SHOW GET lnBCost2   &lcCostStat
      SHOW GET lnBCost3   &lcCostStat
      SHOW GET lnBCost4   &lcCostStat
      *SHOW GET lnCst2P    &lcCostStat
      *SHOW GET lnCst3P    &lcCostStat
      *SHOW GET lnCst4P    &lcCostStat
      
      *E301911,1 ABD - Don't show this field in case ! WINCh 8 on top. [Begin]
      IF llMultCur  .AND. lnDomestic = 2
        SHOW GETS WINDOW (lcWinCh8) DISABLE ONLY
        SHOW WINDOW (lcWinCh1) TOP
      ELSE
       SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
       SHOW WINDOW (lcWinCh8) TOP
      ENDIF
      *E301911,1 ABD - [End]
      
    ENDIF

    
  CASE lnactfolder = 5
  	SHOW GETS WINDOW (lcWinCh5) DISABLE ONLY
  	SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
  	SHOW GETS WINDOW (lcWinCh3) DISABLE ONLY
  	SHOW GETS WINDOW (lcWinC31) DISABLE ONLY    	
  	SHOW GETS WINDOW (lcWinCC)  DISABLE ONLY  	
  	SHOW GETS WINDOW (lcWinCC1) DISABLE ONLY   	  	  	
  	SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
  	*E301911,1 ABD - Add new screen to show it if Domastic material . [Begin]
  	SHOW GETS WINDOW (lcWinCh8) DISABLE ONLY
  	*E301911,1 ABD - [End]
  	
    SHOW WINDOW (lcWinCh4) TOP
    *B804060 TMI [Start] Check that the Style code structure is created.
    IF lfCKSegInf()
    *B804060 TMI [End  ]
      =lfvStyles()
    *B804060 TMI [Start] end if check
    ENDIF
    *B804060 TMI [End  ]    
    =lfStyBrow()
    SHOW GET ibToSty ENABLE
ENDCASE    

*!*************************************************************
*! Name      : lfFillFold
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : fill the array with screen names of folders
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfFillFold

laFoldWinds[1,1] = "General"
laFoldWinds[1,2] = "lcWinCh5"

laFoldWinds[2,1] = "Quantities"
laFoldWinds[2,2] = "lcWinCh2"

laFoldWinds[3,1] = "Colors"
laFoldWinds[3,2] = "lcWinCh4"

laFoldWinds[4,1] = "Costing"
laFoldWinds[4,2] = "lcWinCh1"

laFoldWinds[5,1] = "Styles"
laFoldWinds[5,2] = "lcWinCh3"

*!*************************************************************
*! Name      : lfvNotes
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Returns the notes for the selected Material
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvNotes

*E301077,81 (Start)
llNoteOpen = gfOpenFile(gcDataDir+'NotePad',gcDataDir+'NotePad','SH')
*E301077,81 (End)
=NotePad("G",SUBSTR(laData[1],1,7))
*E301077,81 (Start)
IF llNoteOpen
  =gfCloseFile('NotePad')
ENDIF  
*E301077,81 (End)
*!*************************************************************
*! Name      : lfGetFab
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Copy the information of an old Fabric in a new fabric
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfGetFab
llRetFlag = .F.
DO (gcScrDir+gcWinAppl+'\MAMATRA.SPX')
RETURN llRetFlag
*!*************************************************************
*! Name      : lpShow
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Show the objects due the screen mode
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
PROCEDURE lpShow

IF llCostPrv
  SHOW GET ibFolder[4] ENABLE
ELSE
  SHOW GET ibFolder[4] DISABLE
ENDIF  
*=ASORT(laType,lnType)

*B120349,1 ABD - Fix bug that when a new color is added, all colors change to the 
*B120349,1 ABD - Item Type selected under the new color, So we will define new 
*B120349,1 ABD - variable to know if the item type was changed or no. [Begin] 
IF !(laScrMode[3])
  llmodiftype = .F.
ENDIF
*B120349,1 ABD - [End]

DO CASE
  CASE laScrMode[1]                    && Select Mode
    *E301740,4 ABD - Restore the old value for variable lcsydkey,
    *E301740,4 ABD - That we empty this variable before that to 
    *E301740,4 ABD - Save the notepade without the message. [Begin]
    IF llSpIstall .AND. EMPTY(lcSydKey)
      lcSydKey = lcOldsydky
    ENDIF  
    *E301740,4 ABD - [End]
    *C102684,1 (Begin) Make llVendQury  = .F. as the user may inquire using Aria Item\Color not
    *C102684,1         vendor ones
    IF ASCAN(laEvntTrig , PADR('FALSEIT',10)) <> 0
      =gfDoTriger('MAMATRL',PADR('FALSEIT',10))
    ENDIF
    *C102684,1 (End)
    SELECT(lcTempClr)
    ZAP
    *B802838,1 AMM Zap the file in the select mode
    IF USED(lcStyTemp)
      SELECT (lcStyTemp)
      ZAP
    ENDIF
    *B802838,1 AMM end    
    SELECT(lcBaseFile)
    laData[13] = "DEFDEF"
    lcOldLink  = "DEFDEF"

    *E301911,1 ABD - Add new variable to hold the default sell code. [Begin]
    lcOldSelLk = "DEF"
    *E301911,1 ABD - [End]

    lcQua1     = 'Second Quality'
    lcQua2     = 'Damage'
    laData[18] = gcbasecurr
    laData[19] = gcbasecurr
    =SEEK(laData[19],"SycCurr")
    lcFrnSmbl1 = syccurr.ccurrsmbl
    lcFrnSmbl2 = syccurr.ccurrsmbl
    lcFrn1     = gfGetExSin("",ALLTRIM(laData[18]))
    lcFrn2     = gfGetExSin("",ALLTRIM(laData[19]))

    *E301911,1 ABD - Didn't show word Rate, or Not Valid Or all Date.  [Begin]
    *lcRateP    = 'Rate                   :'
    *lcRateD    = 'Rate                   :'
    *E301911,1 ABD - [End]
    
    lcWare     = 'ALL'
    lcSAlClr   = "All Colors "
    laData[9]  = DATE()
    lcClrDesc  = SPACE(15)
    lcClrLD    = SPACE(30)
    lcSFields   = SPACE(01)
    lnOnHand   = 0
    lnOnOrder  = 0
    lnReorder  = 0
    lnUsage    = 0
    *E301797,1 (Begin) Initialize.
    lnInTran = 0
    *E301797,1 (End)

    *E301911,1 ABD - assign 0 to Variabls. [Begin]
    STORE 0 TO lnSelOrder , lnSelInvce
    *E301911,1 ABD - [End]
        
    llAllColrs = .F.
    llCopyFab  = .F.
    =lfInit(.F.)
    SHOW GET ibFolder[3] ENABLE
    SHOW GET ibFolder[5] DISABLE        
    SHOW GET pbInvNote   DISABLE        
    SHOW GET pbArrDye    DISABLE        
    SHOW GET pbObjlnk    DISABLE        
    SHOW GET laData[1]   ENABLE
    SHOW GET ibItem      ENABLE
    SHOW GET laData[2]   DISABLE
    SHOW GET ibColor     DISABLE
    SHOW GET pbObjLink   DISABLE        
    SHOW GET pbNext      DISABLE
    SHOW GET pbPrev      DISABLE
    SHOW GET pbFirst     DISABLE
    SHOW GET pbLast      DISABLE
    SHOW GET pbDelete    DISABLE
    SHOW GET pbEdit      DISABLE
    SHOW GET pbSave      DISABLE
    SHOW GET pbSelect    DISABLE
    SHOW GET ibIType     DISABLE
    SHOW GET pbInquire   DISABLE
    SHOW GET pbFolders   DISABLE
    SHOW GET pbWare      DISABLE
    SHOW GET pbOnHand    DISABLE
    SHOW GET pbOnOrder   DISABLE
    SHOW GET pbWIP       DISABLE
    SHOW GET pbUsage     DISABLE
    SHOW GET pbDyelot    DISABLE
    SHOW GET pbRolls     DISABLE
    SHOW GET pbLots      DISABLE
    SHOW GET pbLocation  DISABLE
    SHOW GET cbDyelot    DISABLE
    SHOW GET laData[14]  DISABLE
    SHOW GET ibFolders   DISABLE
    SHOW GET laData[3]   DISABLE
    SHOW GET lnType      DISABLE
    SHOW GET lnReorder   DISABLE
    SHOW GET ibKType     DISABLE
    SHOW GET laData[8]   DISABLE
    SHOW GET laData[12]  DISABLE
    SHOW GET ibVendor    DISABLE
    SHOW GET lcWare      DISABLE
    SHOW GET laData[6]   DISABLE
    SHOW GET laData[10]  DISABLE
    SHOW GET laData[4]   DISABLE
    SHOW GET laData[5]   DISABLE
    SHOW GET laData[13]  DISABLE
    SHOW GET ibGLLink    DISABLE
    SHOW GET laData[7]   DISABLE
    SHOW GET laData[9]   DISABLE
    SHOW GET laData[15]  DISABLE
    SHOW GET laData[17]  DISABLE
    SHOW GET laData[11]  DISABLE
    SHOW GET pbCRemove   DISABLE
    SHOW GET pbCNew      DISABLE
    SHOW GET laData[18]  DISABLE
    SHOW GET laData[19]  DISABLE    
    SHOW GET ibPCurr     DISABLE
    SHOW GET lcDCurr     DISABLE    
    SHOW GET lnExRate    DISABLE
    SHOW GET lnCost1     DISABLE
    SHOW GET lnCost2     DISABLE
    SHOW GET lnCost3     DISABLE
    SHOW GET lnCost4     DISABLE
    SHOW GET lnCst2P     DISABLE
    SHOW GET lnCst3P     DISABLE
    SHOW GET lnCst4P     DISABLE
    SHOW GET laData[20]  DISABLE
    SHOW GET laData[22]  DISABLE
    SHOW GET laData[21]  DISABLE
    SHOW GET laData[16]  DISABLE
    SHOW GET laData[15]  DISABLE
    SHOW GET lnBCost1    DISABLE    
    SHOW GET lnBCost2    DISABLE    
    SHOW GET lnBCost3    DISABLE    
    SHOW GET lnBCost4    DISABLE                
    SHOW GET lnCst2P     DISABLE                    
    SHOW GET lnCst3P     DISABLE                    
    SHOW GET lnCst4P     DISABLE                    
    SHOW GET ibToFold2   DISABLE
    SHOW GET ibToFold5   DISABLE
    SHOW GET ibToFold1   DISABLE
    SHOW GET ibToFoldc   DISABLE
    SHOW GET ibToBrow    DISABLE

    *--HDM Fixing Bug in styles folder[start]
    *E301077,81 (Start)
    *llNthing = lfvStyles()
    *E301077,81 (End)
    *--HDM Fixing Bug in styles folder[end]

    _CUROBJ = OBJNUM(laData[1])
   
  CASE laScrMode[3]                         && Edit Mode
    *B605437,1 ABD - Fix problem that the program update custom field all color with 
    *B605437,1 ABD - The default value for first color when add new color (customer field).
    *B605437,1 ABD - So we will copy the Array that hold the user define file to 
    *B605437,1 ABD - compere when saving if the value change. [Begin]
    = ACOPY(laUsrFields,laCpUrFlds)
    *B605437,1 ABD - [End]
    
    *B607415,1 ABD - Recalculate the Cost in Forign currecy again in edit mode,
    *B607415,1 ABD - That because the global scatter the ladata field from fabric file,
    *B607415,1 ABD - This case happen with multi curreny only. [Begin]
    laData[20] = lnBCost1 + lnBCost2 + lnBCost3 + lnBCost4
    *-- get the COSTBUY after the last calculation.  [Begin]
    laData[21] = (laData[20]/laData[16]) 

    *B607483,1 ABD - get the ladata of 20 and 20 in case the on hand is equal to zero. [Begin]
    laData[23] = IIF(lnOnhand = 0, laData[21], FABRIC.nfAve_Cost)
    laData[22] = IIF(lnOnhand = 0, laData[20], FABRIC.nAveCstBuy)
    *B607483,1 ABD - [End]

    
    *B607415,1 ABD - [End]
    
    IF llAllColrs
      laData[2] = '******'
      SHOW GET ibFolder[5] DISABLE
      SHOW GET ibFolder[3] ENABLE
    ELSE  
      SHOW GET ibFolder[3] DISABLE
    ENDIF
    SHOW GET laData[3]   ENABLE
    SHOW GET laData[4]   ENABLE
    SHOW GET laData[5]   ENABLE
    SHOW GET laData[6]   ENABLE
    SHOW GET laData[1]   DISABLE
    SHOW GET laData[2]   DISABLE
    SHOW GET ibItem      DISABLE
    SHOW GET ibColor     DISABLE
    SHOW GET pbInvNote   ENABLE
    SHOW GET pbArrDye    ENABLE
    SHOW GET pbObjlnk    ENABLE
    SHOW GET lcWare      DISABLE
    IF llMultiWH AND !llAllColrs
      SHOW GET pbWare    ENABLE
    ELSE
      SHOW GET pbWare    DISABLE
    ENDIF
    SHOW GET pbOnHand    DISABLE
    SHOW GET pbOnOrder   DISABLE
    SHOW GET pbWIP       DISABLE
    SHOW GET pbUsage     DISABLE
    SHOW GET pbDyelot    DISABLE
    SHOW GET pbRolls     DISABLE
    SHOW GET pbLots      DISABLE
    SHOW GET pbLocation  DISABLE
    SHOW GET lcWare      DISABLE
    SHOW GET laData[7]   ENABLE
    SHOW GET lnReorder   ENABLE
    SHOW GET pbCNew      ENABLE
    SELECT (lcTempClr)
    IF EOF() 
      SHOW GET pbCRemove DISABLE
    ELSE
      SHOW GET pbCRemove ENABLE
    ENDIF  
    SHOW GET laData[8]   ENABLE
    SHOW GET laData[12]  ENABLE
    SHOW GET ibVendor    ENABLE
    SHOW GET laData[10]  ENABLE
    SHOW GET laData[13]  ENABLE
    SHOW GET lcGLLink    ENABLE
    SHOW GET ibGLLink    ENABLE
    SHOW GET ibDomestic  ENABLE
    SHOW GET laData[11]  ENABLE
    SHOW GET cbDyelot    ENABLE
    SHOW GET laData[14]  ENABLE
    SHOW GET laData[9]   ENABLE
    SHOW GET laData[17]  ENABLE
    SHOW GET laData[18]  &lcCostStat
    SHOW GET laData[19]  &lcCostStat
    SHOW GET ibPCurr     &lcCostStat
    SHOW GET lcDCurr     &lcCostStat
    SHOW GET lnExRate    &lcCostStat
    SHOW GET lnCost1     &lcCostStat    
    SHOW GET lnCost2     &lcCostStat
    SHOW GET lnCost3     &lcCostStat
    SHOW GET lnCost4     &lcCostStat
    SHOW GET lnCst2P     &lcCostStat
    SHOW GET lnCst3P     &lcCostStat
    SHOW GET lnCst4P     &lcCostStat
    SHOW GET laData[20]  &lcCostStat
    SHOW GET laData[22]  &lcCostStat
    SHOW GET laData[21]  &lcCostStat
    SHOW GET laData[16]  &lcCostStat
    SHOW GET laData[15]  &lcCostStat
    IF !llMultCur
      SHOW GET lnBCost1  &lcCostStat
      SHOW GET lnBCost2  &lcCostStat
      SHOW GET lnBCost3  &lcCostStat
      SHOW GET lnBCost4  &lcCostStat
      SHOW GET lnCst2P   &lcCostStat                  
      SHOW GET lnCst3P   &lcCostStat                  
      SHOW GET lnCst4P   &lcCostStat                  
    ELSE
      SHOW GET lnBCost1    DISABLE    
      SHOW GET lnBCost2    DISABLE    
      SHOW GET lnBCost3    DISABLE    
      SHOW GET lnBCost4    DISABLE                
      SHOW GET lnCst2P     DISABLE                    
      SHOW GET lnCst3P     DISABLE                    
      SHOW GET lnCst4P     DISABLE                    
    ENDIF
    
  CASE laScrMode[2]
    =lfInit(.T.)
    IF llAllColrs
      laData[2] = '******'
      SHOW GET ibFolder[3] ENABLE
    ELSE
      SHOW GET ibFolder[3] DISABLE  
    ENDIF
    IF llMultiWH AND !llAllColrs
      SHOW GET pbWare    ENABLE
      SHOW GET lcWare    ENABLE
    ELSE
      SHOW GET pbWare    DISABLE
      SHOW GET lcWare    DISABLE
    ENDIF
    laCodInfo[1,09] = IIF(llAllColrs,"laData[1]","laData[1] + laData[2]")

    *B803631,1 WAB (START) - in case copy from onther fabric assign Fabric code to lacodeInf[2,09]
    laCodInfo[2,09] = IIF(llAllColrs,"laData[1]","laData[1] + laData[2]")

    *B803631,1 WAB (END) 

    IF llAllColrs
      SHOW GET ibFolder[5] DISABLE
      SHOW GET pbOnHand    DISABLE
      SHOW GET pbOnOrder   DISABLE
      SHOW GET pbWIP       DISABLE
      SHOW GET pbUsage     DISABLE
      SHOW GET pbDyelot    DISABLE
      SHOW GET pbRolls     DISABLE
      SHOW GET pbLots      DISABLE
      SHOW GET pbLocation  DISABLE
    ELSE
      SHOW GET ibFolder[5] ENABLE
      SHOW GET pbOnHand    ENABLE
      SHOW GET pbOnOrder   ENABLE

      *B605940,1 KHM 05/07/2002 (Begin) Adding the checking of the PO also
      *IF llMFIstall
      IF llMFIstall OR llPOIstall
      *B605940,1 KHM 05/07/2002 (End)
      
        SHOW GET pbWIP       ENABLE
        SHOW GET pbUsage     ENABLE
      ELSE  
        SHOW GET pbWIP       DISABLE
        SHOW GET pbUsage     DISABLE
      ENDIF  
      IF llDyeLot AND cbDyelot
          SHOW GET pbDyelot  ENABLE
       ELSE
          SHOW GET pbDyelot  DISABLE
      ENDIF  
      IF llTrkRolls AND laData[14]
         SHOW GET pbRolls   ENABLE
      ELSE
          SHOW GET pbRolls   DISABLE
      ENDIF
          SHOW GET pbLots      ENABLE
      IF llMultiLoc
         SHOW GET pbLocation  ENABLE
      ELSE
         SHOW GET pbLocation  DISABLE
      ENDIF
      *C102684,1 (Begin) When moving to previous, next.., Get the correct reference if any.
      IF llVendQury
        SET ORDER TO Matcol IN VENDMATL
        IF SEEK(lcVenCode+SUBSTR(laData[1],1,7)+SUBSTR(laData[2],1,6),'VENDMATL')
          lcVenFab   = VENDMATL.cvenfab
          lcVenCol   = VENDMATL.cvencolr
          lcAriaFab  = VENDMATL.Fabric
          lcAriaClr  = VENDMATL.Color
        ELSE
          STORE " " TO lcVenFab,lcVenCol,lcAriaFab,lcAriaClr
        ENDIF
        *C102684,1 (Begin) Calculate On order for Bratex.
        IF ASCAN(laEvntTrig , PADR('CALCORD',10)) <> 0
           =gfDoTriger('MAMATRL',PADR('CALCORD',10))
        ENDIF
        *C102684,1 (End)
      ENDIF  
      *C102684,1 (End)
    ENDIF
    SHOW GET ibToFold5   ENABLE
    SHOW GET ibToFoldc   ENABLE
    SHOW GET ibToBrow    ENABLE
    SHOW GET ibToFold1   ENABLE
    IF llCalled
      SHOW GET pbInvNote DISABLE
      SHOW GET pbObjlnk  DISABLE
      SHOW GET pbArrDye  DISABLE
    ELSE
      SHOW GET pbInvNote ENABLE
      SHOW GET pbObjlnk  ENABLE
      SHOW GET pbArrDye  ENABLE
    ENDIF

    *--HDM Fixing Bug in styles folder[start]
    *E301077,81 (Start)
    *B802838,1 AMM Remove comment to collect data
    IF USED(lcStyTemp)
      SELECT (lcStyTemp)
      ZAP
    ENDIF
    *B804060 TMI [Start] if Style code stru is created then call lfvStyles()
    *llNthing = lfvStyles()
    llNthing = IIF( lfCKSegInf() , lfvStyles() , .F. )
    *B804060 TMI [End  ]
    *B802838,1 AMM end
    *E301077,81 (End)
    *--HDM Fixing Bug in styles folder[end]

  CASE laScrMode[4]
    laData[2] = '******'
    SHOW GET laData[1]   DISABLE
    SHOW GET laData[2]   DISABLE
    SHOW GET ibColor     DISABLE
    SHOW GET ibItem      DISABLE
    SHOW GET ibFolder[5] DISABLE
    SHOW GET ibFolder[3] ENABLE
    SHOW GET lcWare      DISABLE
    SHOW GET pbOnHand    DISABLE
    SHOW GET pbOnOrder   DISABLE
    SHOW GET pbWIP       DISABLE
    SHOW GET pbUsage     DISABLE
    SHOW GET pbDyelot    DISABLE
    SHOW GET pbRolls     DISABLE
    SHOW GET pbLots      DISABLE
    SHOW GET pbLocation  DISABLE
    SHOW GET pbUsage     DISABLE
    SHOW GET pbObjLink   ENABLE
    SHOW GET laData[3]  ENABLE
    SHOW GET laData[4]     ENABLE
    SHOW GET laData[5]    ENABLE
    SHOW GET laData[6]   ENABLE
    SHOW GET laData[7]   ENABLE
    SHOW GET ibToFold2   ENABLE
    IF llMultiWH AND !llAllColrs
       SHOW GET pbWare    ENABLE
    ELSE
       SHOW GET pbWare    DISABLE
    ENDIF
    SHOW GET ibToFold5   ENABLE
    SHOW GET lnType     ENABLE
    SHOW GET ibIType     ENABLE
    SHOW GET laData[8]  ENABLE
    SHOW GET laData[10]  ENABLE    
    SHOW GET laData[12]    ENABLE
    SHOW GET ibVendor    ENABLE
    SHOW GET laData[10]  ENABLE
    SHOW GET laData[9]    ENABLE
    SHOW GET laData[13]  ENABLE
    SHOW GET laData[11]   ENABLE
    SHOW GET ibGLLink    ENABLE
    SHOW GET cbDyelot    ENABLE
    SHOW GET laData[14]     ENABLE
    SHOW GET ibToFold1   ENABLE
    SHOW GET ibDomestic  ENABLE
    SHOW GET laData[18]  &lcCostStat
    SHOW GET laData[19]  &lcCostStat
    SHOW GET ibDcurr     &lcCostStat
    SHOW GET ibPcurr     &lcCostStat
    SHOW GET lnExRate    &lcCostStat
    SHOW GET lnCost1     &lcCostStat    
    SHOW GET lnCost2     &lcCostStat
    SHOW GET lnCost3     &lcCostStat
    SHOW GET lnCost4     &lcCostStat
    SHOW GET lnCst2P     &lcCostStat
    SHOW GET lnCst3P     &lcCostStat
    SHOW GET lnCst4P     &lcCostStat
    SHOW GET laData[16]  &lcCostStat
    SHOW GET laData[15]  &lcCostStat
    SHOW GET laData[17]  ENABLE
    IF !llMultCur
      SHOW GET lnBCost1  &lcCostStat
      SHOW GET lnBCost2  &lcCostStat
      SHOW GET lnBCost3  &lcCostStat
      SHOW GET lnBCost4  &lcCostStat
      SHOW GET lnCst2P   &lcCostStat  
      SHOW GET lnCst3P   &lcCostStat  
      SHOW GET lnCst4P   &lcCostStat          
    ELSE
      SHOW GET lnBCost1    DISABLE    
      SHOW GET lnBCost2    DISABLE    
      SHOW GET lnBCost3    DISABLE    
      SHOW GET lnBCost4    DISABLE                
      SHOW GET lnCst2P     DISABLE    
      SHOW GET lnCst3P     DISABLE    
      SHOW GET lnCst4P     DISABLE            
    ENDIF
    SHOW GET ibToFoldc   ENABLE
    SHOW GET ibToBrow    ENABLE
    CASE lnActFolder = 3
    IF !llAllColrs
      SHOW GET pbCNew    DISABLE
      SHOW GET pbCRemove DISABLE
      SHOW GET lnReorder ENABLE
    ELSE
      SHOW GET pbCNew     ENABLE
      SHOW GET lnReorder  DISABLE
      IF RECCOUNT(lcTempClr) > 0
        SHOW GET pbCRemove ENABLE
      ELSE
        SHOW GET pbCRemove DISABLE
      ENDIF  
    ENDIF
ENDCASE  
IF laScrMode[4]
  IF llCopyFab
    laCodInfo[1,09] = "lcFabric"
    
    *B803631,1 WAB (START) - in case copy from onther fabric assign Fabric code to lacodeInf[2,09]
    laCodInfo[2,09] = "lcFabric"
    
    *B803631,1 WAB (END)
    
    =gfwCodePop(@laCodInfo, "ITEM_TYPE" , "T")
    =gfwCodePop(@laCodInfo, "CPURCODE" , "T")&&T
  *B803631,1 WAB - commit this line cause if we fill the popup again the pur group & the material 
  *B803631,1 WAB (START) - item popus return to default Element
  *ELSE
  *  =gfwCodePop(@laCodInfo, "ITEM_TYPE" , "D")
  *  =gfwCodePop(@laCodInfo, "CPURCODE" , "D")
  *B803631,1 WAB (END)
   
  ENDIF
ELSE
  =gfwCodePop(@laCodInfo, "ITEM_TYPE" , "L")
  =gfwCodePop(@laCodInfo, "CPURCODE" , "L")
ENDIF
=lfchngfolder(lnactfolder)


*!*************************************************************
*! Name      : lfvdata1
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Validatin of the fabric field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvdata1
PRIVATE lnOption

IF MDOWN()
  RETURN
ENDIF  

IF  !EMPTY(laData[1]) 
  IF SEEK(laData[1],"Fabric")
    SHOW GET laData[2] ENABLE
    SHOW GET ibColor ENABLE
    SHOW GET laData[1]  DISABLE
    SHOW GET ibItem  DISABLE
   _CUROBJ = OBJNUM(laData[2])
  ELSE  
    IF ALLTRIM(laData[1]) ='?'
      lnOption = 2
    ELSE
      lnOption =gfModalGen('QRM36022B36002','ALERT', ALLTRIM(laData[1]))
    ENDIF  
    DO CASE
      CASE lnOption = 1  
         *B603012,1 (Add) Check if the privilages of the users enabled them to Add or not.
         IF !llAddRec
           =gfModalGen("TRM00088B00000","ALERT",'Adding') 
           _CUROBJ = OBJNUM(laData[1])
           laData[1] = ""
           RETURN
         ENDIF  
         *B603012,1 (End)
        GOTO TOP IN FABRIC
        *E301582,1 ABD - Cheak if specification sheet is instuall we will 
        *E301582,1 ABD - import item from PDM File. [Begin]
        *B604408,1 - WAB (Start) check if the files exist
        *IF llSpIstall
        IF llSpIstall AND llOpnPdm
          *B604408,1 - WAB (End)
          DO WHILE !EOF('FABRIC') AND llLoop
            lnChooise =  gfModalGen('QRM36175B36017','ALERT')
            DO CASE
              CASE lnChooise = 1
                llCopyFab = lfGetFab()
                *B605725,1 ABD - Assign true to not import variable. [Begin]
                llNotEmprt = .T.
                *B605725,1 ABD -[End]
                IF LASTKEY()=27
                  EXIT
                ENDIF  
                *-- If we will import the style.
              CASE lnChooise = 2
                DO CASE
                  CASE !SEEK('COMPONENT'+ladata[1],"PDMLOG")
                    *-- Text message : This item does not exist in the Pdm file ; Cannot proceed.
                    *-- Message No.  : 36176.
                    *-- Text Button  : Ok
                    *-- Button No.   : 00000
                    = gfModalGen('QRM36176B00000','ALERT','item')
                    llNotEmprt = .F.
                    EXIT
                  CASE SEEK('COMPONENT'+ladata[1],"PDMLOG") .AND. PDMLOG.cImpstatus <> 'O'
                    *-- Text message : This item is already imported ; Cannot proceed.
                    *-- Message No.  : 36177.
                    *-- Text Button  : OK
                    *-- Button No.   : 0000
                    = gfModalGen('QRM36177B00000','ALERT','item')
                    llNotEmprt = .F.
                    EXIT
                  CASE SEEK('COMPONENT'+ladata[1],"PDMLOG") .AND. PDMLOG.cImpstatus = 'O'
                    = SEEK(ladata[1],"PDMATRL")                  
                    llImprtFab = .T.
                    EXIT
                ENDCASE
              CASE lnChooise = 3
                STORE .F. To llCopyFab , llImprtFab
                *B604320,1 ABD ass. true to this variable. [Begin]
                llNotEmprt = .T.
                *B604320,1 [END]
                EXIT
            ENDCASE
          ENDDO
          *604320,4 AMH Importing in case of empty fabric file [START]
          DO WHILE EOF('FABRIC') AND llLoop
            *B604573,1 WAB (START)  change the message from hard code to be as our standard .
            *lnChooise =  gfModalGen('QRM00000B36001','ALERT',"",.F.,'Do you wish to import the details of this fabric?')
            *-- Text message : Do you wish to import the details of this fabric?'.
            *-- Message No.  : 36178.
            *-- Text Button  : OK
            *-- Button No.   : 0000
            lnChooise =  gfModalGen('QRM36178B36001','ALERT')
            *B604573,1 WAB (END)
            DO CASE
              CASE lnChooise = 1
                DO CASE
                  CASE !SEEK('COMPONENT'+ladata[1],"PDMLOG")
                    *-- Text message : This item does not exist in the Pdm file ; Cannot proceed.
                    *-- Message No.  : 36176.
                    *-- Text Button  : Ok
                    *-- Button No.   : 00000
                    = gfModalGen('QRM36176B00000','ALERT','item')
                    llNotEmprt = .F.
                    EXIT
                  CASE SEEK('COMPONENT'+ladata[1],"PDMLOG") .AND. PDMLOG.cImpstatus <> 'O'
                    *-- Text message : This item is already imported ; Cannot proceed.
                    *-- Message No.  : 36177.
                    *-- Text Button  : OK
                    *-- Button No.   : 0000
                    = gfModalGen('QRM36177B00000','ALERT','item')
                    llNotEmprt = .F.
                    EXIT
                  CASE SEEK('COMPONENT'+ladata[1],"PDMLOG") .AND. PDMLOG.cImpstatus = 'O'
                    llImprtFab = .T.
                    EXIT
                ENDCASE
              CASE lnChooise = 2
                STORE .F. To llImprtFab
                llNotEmprt = .T.
                EXIT
            ENDCASE
          ENDDO
          *604320,4 AMH [END]
          *-- Return to select mode
          IF !llNotEmprt
            laScrmode[1]= .T.
            = lfInit(.F.)
            SHOW GETS
            _CUROBJ = OBJNUM(laData[1])
            RETURN
          ENDIF
        ELSE
          *E301582,1 ABD - [End]
          DO WHILE !EOF('FABRIC') AND llLoop AND gfModalGen('QRM36023B36001','ALERT') = 1
             llCopyFab = lfGetFab()
             IF LASTKEY()=27
               EXIT
             ENDIF  
          ENDDO
          *E301582,1 ABD - End For if statement. [Begin]
        ENDIF
        *E301582,1 ABD - [End]
        laData[2]   = REPLICAT("*",6)
        llAllColrs  = .T.
        lcClrDesc   = lcSAlClr
        lcClrLD     = SPACE(30)
        laScrMode[1]= .F.
        laScrMode[4]= .T.
        lnlastfold = lnactfolder
        lnactfolder  = 1
        =lfchngfolder(lnactfolder)
        IF llCopyFab
          *B604016,1 MHM 11/14/2000 [START]
            FOR j = 1 TO ALEN(laUsrFields,1)
              IF EMPTY(laUsrFields[J,6])

              *E301582,1 ABD - Cheak First if laUsrFields[J,1]) Is empty or not
              *E301582,1 ABD - Because this made a bug "syntax Error" if compay
              *E301582,1 ABD - Have any user def. fields. [Begin]
              *laUsrFields[J,6] = &laUsrFields[J,1]    
               laUsrFields[J,6] = IIF(EMPTY(laUsrFields[J,1]),'',EVAL(laUsrFields[J,1]))
              *E301582,1 ABD [End]

              ELSE
                EXIT
              ENDIF
            ENDFOR
          *B604016,1 MHM 11/14/2000 [END]
          = lfInit(.T.)
          = lfFabClrs()
        ENDIF

        *E301582,1 ABD - if we will import from the PDM file call function with .F. [Begin]
        IF llImprtFab
          = lfInit(.F.)
          = lfFabClrs ()
        ENDIF
        *E301582,1 ABD - [End]
        
        SHOW GETS
        _CUROBJ = OBJNUM(laData[3])
       
      CASE lnOption = 2
         lcBrowItm = laData[1]
         lcBrowClr = "*"    
         laData[2]  = "*"   
         DO FABROW WITH lcBrowItm,lcBrowClr
         laData[1] = lcBrowItm  
         laData[2] = lcBrowClr
         IF !EMPTY(laData[1])        
           laData[2] = SPACE(6)
           SHOW GET laData[2] ENABLE
           SHOW GET ibColor ENABLE
           SHOW GET laData[1]  DISABLE
           SHOW GET ibItem  DISABLE
           _CUROBJ = OBJNUM(laData[2])
         ELSE
           laData[1]  = SPACE(7)
           laData[2] = SPACE(6)
           _CUROBJ = OBJNUM(laData[1])
           laScrMode[1] = .T.
           lnlastfold = lnactfolder
           lnactfolder  = 1
           =lfchngfolder(lnactfolder)
           =lfInit(.F.)
           SHOW GETS
        ENDIF        
      CASE lnOption = 3
          laScrmode[1]= .T.
          lnlastfold = lnactfolder
          =lfchngfolder(lnactfolder)
          = lfInit(.F.)
          SHOW GETS
          _CUROBJ = OBJNUM(laData[1])
      ENDCASE
  ENDIF
ELSE
  laScrMode[1] = .T.
  SHOW GET laData2 DISABLE
  SHOW GET ibColor DISABLE  
ENDIF
*!*************************************************************
*! Name      : lfFabClrs
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Get all the colors of the selected material
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfFabClrs
PRIVATE  lnAlias

lnAlias = SELECT()
*SELECT FABRIC.FABRIC ,FABRIC.COLOR,FABRIC.ONHAND,"A" AS cStatus,;
       SPACE(1) AS cMarker , SPACE(30) AS cData,SPACE(50) AS ClrLName;
       FROM FABRIC;
       WHERE FABRIC.FABRIC = lcFabric;
       ORDER BY FABRIC.COLOR ;
       INTO DBF (gcWorkDir+lcTempClr)

*E301582,1 ABD - Get the color for selected material from imported file. [Begin]
IF llSpIstall .AND. llImprtFab


  * B604329,4 ABD Create the emp file from fabric file . [Begin]
  *SELECT *,"A" AS cStatus,;
  *       SPACE(1) AS cMarker , SPACE(30) AS cData,SPACE(50) AS ClrLName;
  *       FROM PDMATRL;
  *       WHERE PDMATRL.FABRIC = laData[1];
  *       ORDER BY PDMATRL.COLOR ;
  *       INTO DBF (gcWorkDir+lcTempClr)
  *       INDEX ON  COLOR TAG COLOR
  *
  lnOldAlias = SELECT (0)
  SELECT PDMATRL
  =SEEK(laData[1])
  SCAN REST WHILE Fabric+Color = laData[1]
    SELECT (lcTempClr)
    APPEND BLANK
    REPLACE FABRIC WITH Pdmatrl.Fabric     ,;
            Color WITH Pdmatrl.Color       ,;
            cStatus  WITH "A"              ,;
            DESC     WITH Pdmatrl.Desc     ,;
            WIDTH    WITH Pdmatrl.Width    ,;
            Pattern  WITH Pdmatrl.Pattern  ,;
            Content  WITH Pdmatrl.Content  ,;
            nFabCost WITH  Pdmatrl.nFabcost
    SELECT PDMATRL
  ENDSCAN
  SELECT (lnOldAlias)
  * B604329,4 ABD [End]
ELSE
*E301582,1 ABD - [End]
  SELECT *,"A" AS cStatus,;
         SPACE(1) AS cMarker , SPACE(30) AS cData,SPACE(50) AS ClrLName;
         FROM FABRIC;
         WHERE FABRIC.FABRIC = lcFabric;
         ORDER BY FABRIC.COLOR ;
         INTO DBF (gcWorkDir+lcTempClr)
         INDEX ON  COLOR TAG COLOR
       
  *E301582,1 ABD - End For IF statement. [Begin]
ENDIF
*E301582,1 ABD - [End]
       
SELECT (lcTempClr)
lcName = SPACE(50)
laColor[1,1] = 'CLRLNAME'
laColor[1,2] = 'lcName'
SCAN 
  *C102467,1 ABD - Delete colors that copy and the status is obsolete
  *C102467,1 ABD - for this copied Item color. [Begin]
   IF ASCAN(laObsColor,COLOR) > 0
     DELETE
     BLANK
     LOOP
   ENDIF  
  *C102466,1 ABD - [End]

  =gfRltFld(&lcTempClr..COLOR, @laColor, 'COLOR')
  REPLACE cData    WITH gfCodDes(&lcTempClr..COLOR, 'COLOR'),;
          ClrLName WITH lcName 
  lcName = SPACE(50)        
ENDSCAN
SELECT(lnAlias)
*!*************************************************************
*! Name      : lfvkItem
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Validation of the browse key for the fabric field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvkItem
PARAMETER lcVariable

IF lcVariable = "laData[1]"
  laData[1]    = PADR("?",7)
  llNothing = lfvdata1()
ELSE
  lcFabric  = PADR("?",7)
  llNothing = lfvFabrBr()
ENDIF  

*!*************************************************************
*! Name      : lfvData2
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Validation color field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvData2


llMdown = MDOWN()
IF llMdown
  RETURN
ENDIF

IF EMPTY(laData[2])  
  laData[2] = REPLICAT("*",6)
  SHOW GET laData[2] DISABLE
  llAllColrs = .T.
  IF SEEK(laData[1],"Fabric")
    laScrMode[1] = .F.
    laScrMode[2] = .T.
    lnlastfold  = lnactfolder
    lnactfolder = 1
    =lfchngfolder(lnactfolder)
    =lfRefresh()
    =lfInit(.T.)
    SHOW GETS 
  ELSE
    laScrMode[1] = .T.
    lnlastfold = lnactfolder
    lnactfolder = 1
    =lfchngfolder(lnactfolder)
    SHOW GETS 
  ENDIF
ELSE
  IF SEEK(laData[1]+laData[2],"Fabric")
    *--HDM Change the way of seeking in codes file[start]
    *IF SEEK(gcAct_Comp+'COLOR'+SPACE(5)+laData[2],'CODES')
    IF SEEK('N'+'COLOR'+SPACE(5)+laData[2],'CODES')
    *--HDM Change the way of seeking in codes file[END]
      laScrMode[1] = .F.
      laScrMode[2] = .T.
      lnlastfold = lnactfolder
      lnactfolder = 2
      =lfchngfolder(lnactfolder)
      =lfRefresh()
      =lfInit(.T.)
      SHOW GETS
    ELSE       
      =gfModalGen('TRM36024B36000','ALERT')
      laData[2]  = SPACE(6)      
      _CUROBJ  = OBJNUM(laData[2])
      lnlastfold   = lnactfolder
      lnactfolder  = 2
      =lfchngfolder(lnactfolder)
      SHOW GET laData[2] ENABLE
      SHOW GET ibColor ENABLE
    ENDIF
  ELSE
    lcOldItm  = laData[1]
    lcBrowItm = laData[1]
    lcBrowClr = CHR(240)
    laData[2] = CHR(240)
    DO FABROW WITH lcBrowItm,lcBrowClr
    laData[1] = lcBrowItm  
    laData[2] = lcBrowClr
    IF !EMPTY(laData[2])
      *--HDM Change the way of seeking in codes file[start]
      *IF SEEK(gcAct_Comp +'COLOR'+SPACE(5)+laData[2],'CODES')
      *B604398,1 MHM 31/07/2001 [Start]
      lnCAlias = SELECT()
      SELECT CODES
      SET ORDER TO TAG CCODE_NO
      SELECT (lnCAlias)
      *B604398,1 MHM 31/07/2001 [End]
      IF SEEK('N'+'COLOR'+SPACE(5)+laData[2],'CODES')
      *--HDM Change the way of seeking in codes file[END]
        laScrMode[1] = .F.
        laScrMode[2] = .T.
        lnlastfold = lnactfolder
        lnactfolder = 2
        =lfchngfolder(lnactfolder)
        =lfRefresh()
        =lfInit(.T.)
        SHOW GETS
      ELSE 
        =gfModalGen('TRM36024B36000','ALERT')
        laData[2]   = SPACE(6)
        _CUROBJ   = OBJNUM(laData[2])
        laScrMode[1] = .T.
        lnlastfold = lnactfolder
        lnactfolder  = 2
        =lfchngfolder(lnactfolder)
        SHOW GET laData[2] ENABLE
        SHOW GET ibColor ENABLE
      ENDIF
    ELSE
      laData[1] = lcOldItm
      laData[2] = SPACE(6)
      _CUROBJ   = OBJNUM(laData[2])
      laScrMode[1] = .T.
      lnlastfold = lnactfolder
      lnactfolder  = 2
      =lfchngfolder(lnactfolder)
      SHOW GET laData[1]  DISABLE
      SHOW GET laData[2] ENABLE
      SHOW GET ibItem  DISABLE
      SHOW GET ibColor ENABLE
    ENDIF
  ENDIF
ENDIF
IF EMPTY(laData[1])
  laScrMode[1] = .T.
  _CUROBJ = OBJNUM(laData[1])
  SHOW GET laData[2] DISABLE
  SHOW GET ibColor   DISABLE  
  RETURN
ENDIF
IF llAllColrs 
  lcClrDesc = lcSAlClr
  lcClrLD   = SPACE(30)
ELSE
  laColor[1,1] = 'CLRLNAME'
  laColor[1,2] = 'lcClrLD'  
  lcClrDesc  = gfCodDes(laData[2], 'COLOR')
  = gfRltFld(laData[2], @laColor, 'COLOR')
ENDIF
*C102684,1 (Begin) Calculate On order for Bratex.
IF ASCAN(laEvntTrig , PADR('CALCORD',10)) <> 0
  =gfDoTriger('MAMATRL',PADR('CALCORD',10))
ENDIF
*C102684,1 (End)

=lfRefresh()
=lfBColor()
*E301077,81 (Start)
*=lfvStyles()
*E301077,81 (End)
=lfwWare1()
*B802838,1 AMM Empty the file to recollect it with the new data
IF USED(lcStyTemp)
  SELECT (lcStyTemp)
  ZAP
ENDIF
*B802838,1 AMM end

*!*************************************************************
*! Name      : lfvkColor
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Validation browse key color field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvkColor

laData[2]   = PADR("?",6)
llNothing = lfvData2()

*!*************************************************************
*! Name      : lfvFabrBr
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Validation for the fabric field in the copy fabric screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvFabrBr

IF MDOWN()
  RETURN
ENDIF
IF !EMPTY(lcFabric) 
  IF !SEEK(lcFabric,"FABRIC")
    lcOldFab = lcFabric
    laData[2]  = "*"    
    DO FABROW WITH lcFabric,laData[2]
    lcFabric = IIF(EMPTY(lcFabric),SPACE(7),lcFabric)
    _CUROBJ  = OBJNUM(pbOk)
    SHOW GET lcfabric
  ENDIF
ELSE
  _CUROBJ = OBJNUM(pbCancel)
ENDIF

*C102466,1 ABD - Restrict adding "Obsolete" colors to item, call
*C102466,1 ABD - Check function for the copyied colors if status is 
*C102466,1 ABD - Obsolete for one or more color.[Begin]
IF !EMPTY(lcFabric)
  *-- Function to Check for the copyied colors if
  *-- status is obsolete for one or more color.
  = lfObsColor ()
ENDIF
*C102466,1 ABD - [End]



*!*************************************************************
*! Name      : lfvIType
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Validation for the item type field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvIType

*=ASORT(laType,lnType)
lcIType = laType[lnType,2]

*B120349,1 ABD - Fix bug that when a new color is added, all colors change to the 
*B120349,1 ABD - Item Type selected under the new color, So we will define new 
*B120349,1 ABD - variable to know if the item type was changed or no. [Begin] 
IF !(lcIType = item_type) 
  llModifType = .T.
ENDIF
*B120349,1 ABD - [End]

*!*************************************************************
*! Name      : lfvVendor
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Validation for the vendor field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvVendor
PRIVATE lnAlias

lnAlias = SELECT()

IF EMPTY(laData[12]) 
 lcVenName = " "
ELSE  
  SELECT APVENDOR
  IF !SEEK (laData[12])
    lcVendor = laData[12]
    *--E301268,1 HDM [Start] filter vendor browse by sup. type
    *= gfApVnBrow (@lcVendor)
    = gfApVnBrow (@lcVendor,.F.,'M')
    *--E301268,1 HDM [End]
    IF !EMPTY(lcVendor)
      laData[12] = lcVendor
    ELSE
      laData[12] = lcOldVen  
    ENDIF  
    IF !EMPTY(laData[12])
      lcVenName  = cVenComp
    ELSE
      laData[12] = lcOldField
      SHOW GET laData[12]
    ENDIF  
  ELSE
    lcVenName = cVenComp
  ENDIF
ENDIF
=lfvField('laData[12]')
=lfRefresh()
SELECT(lnAlias)
*!*************************************************************
*! Name      : lfwVendor
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : when function for the vendor field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfwVendor

lcOldVen = laData[12]
*!*************************************************************
*! Name      : lfvkVendor
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Validation for the browse key vendor field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvkVendor

laData[12]  = PADR("?", 8)
lcOldData = laData[12]
llNothing = lfvVendor() 
_CUROBJ = OBJNUM(laData[12])
*!*************************************************************
*! Name      : lfvLink
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Validation for the Gl_Link field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvLink
PRIVATE lnAlias

lnAlias = SELECT()

IF !EMPTY(laData[13]) AND !SEEK("04"+laData[13], "GL_Link")
  lcLink = laData[13]
  IF !gfGLBrowse('04',@lcLink ,@lcLinkDesc) 
    laData[13] = lcOldLink
  ELSE
    laData[13] = lcLink 
  ENDIF
  SHOW GET laData[13]
ENDIF
*E301911,1 ABD - Add DEDEF in Case Empty field. [Begin]
laData[13]  =IIF(EMPTY(laData[13]),'DEFDEF',laData[13])
*E301911,1 ABD - [End]


IF !EMPTY (laData[13])
    =SEEK("04"+laData[13], "GL_Link")
    lcLinkDesc = GL_Link.LinkDesc
    *B802302,1 SWK 05/28/1999 Add the Gl_Link to the changed fieds on the screen
    lcSFields  = lcSFields +','+'laData[13]'
    *B802302,1 SWK (END)
    =lfRefresh()
ENDIF

*E301911,1 ABD - Show Get the Field after get the description[Begin]
SHOW GET lcLinkDesc
*E301911,1 ABD - [End]

SELECT(lnAlias)
*!*************************************************************
*! Name      : lfvkLink
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Validation for the browse key Gl_Link field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvkLink

laData[13] = PADR("?",3) 
llNothing  = lfvLink()
_CUROBJ = OBJNUM(laData[13])
*!*************************************************************
*! Name      : lfwDom
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : when function for the domestic field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfwDom

lnOldValue = lnDomestic
lcOldDom   = lcDomestic
*!*************************************************************
*! Name      : lfvDom
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Validation for the domestic field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvDom
PRIVATE llFound,lnAlias




lnAlias    = SELECT()


llFoundB    = .F.
llFoundP    = .F.
lcDomestic = laDomestic[ibDomestic]
lnDomestic = IIF(lcDomestic="Purchased",2,1)

*E301911,1 ABD [Begin]
*=lfRefresh()
*E301911,1 ABD [End]

IF lcOldDom <> lcDomestic AND laScrMode[3]
  IF lnDomestic = 2
    SET ORDER TO TAG BOM IN BOM 
    IF SEEK(laData[1],'BOM') .AND. Bom.lMaterial
      llFoundB = .T.
      IF !llAllColrs AND BOM.cItmMask <> '******'
        SELECT BOM
        llFoundB = .F.
        SCAN WHILE SUBSTR(BOM.cItmMajor,1,7) = laData[1] .AND. Bom.lMaterial
          IF BOM.cItmMask = laData[2]
            llFoundB = .T.
            EXIT
          ENDIF
        ENDSCAN
      ENDIF
    ENDIF 
  ELSE  
    SET ORDER TO TAG POFLNF IN POFLN
    llSeekKey = IIF(llAllColrs,laData[1],laData[1]+laData[2])
    llFoundP = SEEK(llSeekKey,'POFLN')
  ENDIF  
  IF llFoundB
    =gfModalGen('TRM36025B36000','ALERT')
  ENDIF  
  IF llFoundP
    =gfModalGen('TRM36084B36000','ALERT')
  ENDIF  
  IF llFoundB OR llFoundP
    lcDomestic = lcOldDom
    lnDomestic = lnOldValue
    ibDomestic = lnDomestic
    SHOW GET lcDomestic
    SHOW GET ibDomestic

    *E301911,1 ABD Show Gets For the screen. [Begin]
    SHOW GETS 
    *E301911,1 ABD [Begin]
  ENDIF  
ENDIF  
IF lcOldDom <> lcDomestic AND lnDomestic = 1 AND laScrMode[3]
  =lfvBomCost()
  lcSFields = lcSFields +','+'laData[15]'
ENDIF


lcCostType = IIF(lnDomestic = 1,'M.F.G. Cost','Purchasing Cost')  
*B603324,1 [Start] If Domestic Material UOM Make(Buy) is diffrent
*                  than UOM Use
*laData[15] = IIF(lnDomestic = 1,laData[17],laData[15])
*B603324,1 [End]
lnCost1    = IIF(lnDomestic = 1,0,lnCost1)
lnCost2    = IIF(lnDomestic = 1,0,lnCost2)
lnCost3    = IIF(lnDomestic = 1,0,lnCost3)
lnCost4    = IIF(lnDomestic = 1,0,lnCost4)
lnCst2P    = IIF(lnDomestic = 1 AND lnBCost1 = 0,0,lnCst2P)
lnCst3P    = IIF(lnDomestic = 1 AND lnBCost1 = 0,0,lnCst3P)
lnCst4P    = IIF(lnDomestic = 1 AND lnBCost1 = 0,0,lnCst4P)
*B603324,1 [Start] Don't force conversion factor to 1
*                  if manufactored material
*laData[16] = IIF(lnDomestic = 1,1,laData[16])
*B603324,1 [End]
laData[18] = IIF(lnDomestic = 1,gcBaseCurr,laData[18])
laData[19] = IIF(lnDomestic = 1,gcBaseCurr,laData[19])
laData[20] = lnBCost1 + lnBCost2 + lnBCost3 + lnBCost4
laData[21] = (laData[20]/laData[16]) 
laData[23] = IIF(lnDomestic = 1,laData[22],laData[23])
lcCstSLbl1 = IIF(lnDomestic = 1,lcManSLbl1,lcImpSLbl1)
lcCstSLbl2 = IIF(lnDomestic = 1,lcManSLbl2,lcImpSLbl2)
lcCstSLbl3 = IIF(lnDomestic = 1,lcManSLbl3,lcImpSLbl3)
lcCstSLbl4 = IIF(lnDomestic = 1,lcManSLbl4,lcImpSLbl4)
lcCostStat = IIF(lnDomestic = 2 AND llMultCur,"DISABLE","ENABLE")
SHOW GETS 

*E301911,1 ABD [Begin]
= lfChngDoms()
*E301911,1 ABD [End]


SELECT(lnAlias)
*!*************************************************************
*! Name      : lfvUUse
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Validation for the UOM USE field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvUUse

*B603324,1 [Start] Now user can make UOM Make And UOM Use
*                  no need to make them the same
*IF lnDomestic = 1
*  laData[15] = laData[17]
*  SHOW GET laData[15] DISABLE
*ENDIF
*B603324,1 [End]
=lfRefresh()
*!*************************************************************
*! Name      : lfvFPrice
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Validation for the Foreign Price  field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvFPrice
PRIVATE lnAlias,lnOldTag

IF MDOWN()
  RETURN
ENDIF
lnAlias = SELECT()
IF !SEEK(laData[18],"SycCurr")
  lcCurr = laData[18]
  IF !gfcurrbrow(@lcCurr)
    laData[18] = lcoldvalue
  ELSE
    laData[18] = lcCurr
  ENDIF
ENDIF

IF laData[18] = gcBaseCurr
  lnExRateP = 1

  *E301911,1 ABD - Didn't show word Rate, or Not Valid Or all Date.  [Begin]
  *lcRatep = 'Rate                   :'
  lcRateP = ''
  *E301911,1 ABD - [End]

  lnUnit1 = 1
ELSE
  lnAlias    = SELECT()
  SELECT SYCEXCH
  lnOldTag = TAG()
  SET ORDER TO TAG Currency DESCENDING
  IF !SEEK(gcBaseCurr+laData[18]) OR SYCEXCH.nExRate = 0
    lnExRateP = SYCEXCH.nExRate

    *E301911,1 ABD - Didn't show word Rate, or Not Valid Or all Date.  [Begin]
    *lcRatep = 'No valid rate        :'
    lcRatep = 'Invalid'
    *E301911,1 ABD - [End]

    lnUnit1 = 1
  ELSE
    lnExRateP = SYCEXCH.nExRate
    =SEEK(laData[18],"SycCurr")
    lnUnit1   = SycCurr.nCurrUnit

    *E301911,1 ABD - Didn't show word Rate, or Not Valid Or all Date.  [Begin]
    *lcRatep = 'Rate on ' + DTOC(SYCEXCH.dRateDate) +' :'
    lcRatep = LEFT(DTOC(SYCEXCH.dRateDate),5)
    *E301911,1 ABD - [End]

  ENDIF
  SET ORDER TO TAG &lnOldTag
  SELECT(lnAlias)
ENDIF  
lcFrnSmbl1 = IIF(SEEK(laData[18],'syccurr'),syccurr.ccurrsmbl,'')
lcFrn1     = gfGetExSin("",ALLTRIM(laData[18]))
lnBCost1   = IIF(lcFrn1 = '/',ROUND(lnCost1/lnExRateP/lnUnit1,3),ROUND(lnCost1*lnExRateP/lnUnit1,3))
lnCst2P    = IIF(lnBCost1 = 0,0,(lnBCost2/lnBCost1)*100)
lnCst3P    = IIF(lnBCost1 = 0,0,(lnBCost3/lnBCost1)*100)
lnCst4P    = IIF(lnBCost1 = 0,0,(lnBCost4/lnBCost1)*100)    
laData[20] = lnBCost1 + lnBCost2 + lnBCost3 + lnBCost4
laData[21] = (laData[20]/laData[16]) 
laData[22] = IIF(lnOnhand = 0, laData[20], laData[22])
laData[23] = IIF(lnOnhand = 0, laData[21], laData[23])
SHOW GET lnBCost1
SHOW GET lnCst2P
SHOW GET lnCst3P
SHOW GET lnCst4P
=lfRefresh()
*!*************************************************************
*! Name      : lfvConv
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Validation for the Conv. FActor field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvConv
PRIVATE lcMessage

IF laData[16] <= 0
  =gfModalGen('TRM36014B36000','ALERT','Conversion factor')
  laData[16] = OConvFact
  _CUROBJ = OBJNUM(laData[16])
ELSE
  laData[21]  = (laData[20]/laData[16]) 
  laData[23] = IIF(lnOnHand = 0, laData[21] , laData[23])
  llCostUp = .T.
ENDIF  
=lfRefresh()
*!*************************************************************
*! Name      : lfvRate
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Validation for the Exch. Rate field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvRate

IF lnExRate < 1
  lnExRate  = 1
  SHOW GETS
  _CUROBJ = OBJNUM(lnExRate)
ELSE
  laData[20]  = IIF(lnExRate <> 0,ROUND(laData[18]/lnExRate,3), 0.000)
  lnCost1    = IIF(lnExRate <> 0,ROUND(laData[18]/lnExRate,3), 0.000)
  lnCst2P    = IIF(laData[20] = 0,0,(lnCost2/laData[20])*100)  
  lnCst3P    = IIF(laData[20] = 0,0,(lnCost3/laData[20])*100)
  lnCst4P    = IIF(laData[20] = 0,0,(lnCost4/laData[20])*100)
  laData[20]  =  laData[20] + lnCost2 + lnCost3 + lnCost4
  laData[21]  = (laData[20]/laData[16]) 
  laData[22] = IIF(lnOnHand = 0, laData[20], laData[22])
  laData[23] = IIF(lnOnHand = 0, laData[21], laData[23])
  =lfRefresh()
  SHOW GETS
ENDIF  
*!*************************************************************
*! Name      : lfvFret
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Validation for the Freight field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvFret

IF lnCost4 < 0
  lnCost4 = 0
  SHOW GETS
  _CUROBJ = OBJNUM(lnCost4)
ELSE
  lnCst4P    =  IIF(lnCost1 = 0,0,(lnCost4/lnCost1)*100)
  laData[20]  =  lnCost1 + lnCost2 + lnCost3 + lnCost4
  laData[21]  = (laData[20]/laData[16]) 
  laData[22] = IIF(lnOnhand = 0, laData[20], laData[22])
  laData[23] = IIF(lnOnhand = 0, laData[21], laData[23])
  =lfRefresh()
  SHOW GET lnCost4
ENDIF  
SHOW GETS
*!*************************************************************
*! Name      : lfvTaxP
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Calculate the tax percentage field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvTaxP

IF lnCst2P < 0 
  lnCst2P = 0
  _CUROBJ = OBJNUM(lnCst2P)
ELSE
  lnBCost2   = lnBCost1 * (lnCst2P/100)
  laData[20] = lnBCost1 + lnBCost2 + lnBCost3 + lnBCost4
  laData[21] = (laData[20]/laData[16]) 
  laData[22] = IIF(lnOnhand = 0, laData[20], laData[22])
  laData[23] = IIF(lnOnhand = 0, laData[21], laData[23])
  =lfRefresh()
  SHOW GET lnBCost2
  IF laData[18] = laData[19]
    lnCost2 = lnCost1 * (lnCst2P/100)
    SHOW GET lnCost2
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfvQuotaP
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Calculate the Quota percentage field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvQuotaP

IF lnCst3P < 0
  lnCst3P = 0
  _CUROBJ = OBJNUM(lnCst3P)
ELSE
  lnBCost3    = lnBCost1 * (lnCst3P/100)
  laData[20]  =  lnBCost1 + lnBCost2 + lnBCost3 + lnBCost4
  laData[21]  = (laData[20]/laData[16]) 
  laData[22] = IIF(lnOnhand = 0, laData[20], laData[22])
  laData[23] = IIF(lnOnhand = 0, laData[21], laData[23])
  =lfRefresh()
  SHOW GET lnBCost3
  IF laData[18] = laData[19]
    lnCost3 = lnCost1 * (lnCst3P/100)
    SHOW GET lnCost3
  ENDIF
ENDIF  
*!*************************************************************
*! Name      : lfvFretP
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Calculate the Freight percentage field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvFretP

IF lnCst4P < 0
  lnCst4P = 0
  _CUROBJ = OBJNUM(lnCst4P)
ELSE
  lnBCost4    = lnBCost1 * (lnCst4P/100)
  laData[20]  =  lnBCost1 + lnBCost2 + lnBCost3 + lnBCost4
  laData[21]  = (laData[20]/laData[16]) 
  laData[22] = IIF(lnOnhand = 0, laData[20], laData[22])
  laData[23] = IIF(lnOnhand = 0, laData[21], laData[23])
  =lfRefresh()
  SHOW GET lnBCost4
  IF laData[18] = laData[19]
    lnCost4 = lnCost1 * (lnCst4P/100)
    SHOW GET lnCost4
  ENDIF
ENDIF  
*!*************************************************************
*! Name      : lfInit
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Initialize the variables form file or with initial values
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfInit
PARAMETER llFromFile, lnAlias,lnOldTag

lnAlias = SELECT()
SELECT FABRIC
IF llFromFile
  lnOnHand  = 0
  lnOnOrder = 0
  lnReorder = 0
  lnUsage   = 0

  *B037361,1 ABD - Assign Zero to this variable to ignore recalculate the work in
  *B037361,1 ABD - Process every time the user press edits an undo. [Begin]
  lnWip     = 0
  *B037361,1 ABD - [End]
  
  *E301797,1 (Begin) Initialize.
  lnInTran = 0
  *E301797,1 (End)

  *E301911,1 ABD - assign 0 to Variabls. [Begin]
  STORE 0 TO lnSelOrder , lnSelInvce
  *E301911,1 ABD - [End]
  
  
  IF llAllColrs
    IF laScrMode[4] AND llCopyFab
      llNothing =  SEEK(lcFabric,"Fabric")
    ELSE 
      =SEEK(laData[1],"Fabric")
      SCAN WHILE FABRIC.FABRIC = laData[1]
        lnOnHand  = lnOnHand  + FABRIC.OnHand
        lnOnOrder = lnOnOrder + FABRIC.OnOrder
        lnWip     = lnWip     + FABRIC.NMATWIP
*        lnOnOrder = lnOnOrder + FABDYE.OnOrder
        lnReorder = lnReorder + FABRIC.Reorder
        lnUsage   = lnUsage   + FABRIC.Usage
        *E301797,1 (Begin) Calculate
        lnInTran  = lnInTran  + FABRIC.Matotintr
        *E301797,1 (End)
        *E301911,1 ABD - assign Fabric sell order & ship to Variabls. [Begin]
        lnSelOrder = lnSelOrder + FABRIC.nsellorder
        lnSelInvce = lnSelInvce + FABRIC.nsellship
        *E301911,1 ABD - [End]
      ENDSCAN
      IF EOF('FABRIC')
        SHOW GET pbNxt DISABLE
        SHOW GET pbBtm DISABLE
      ENDIF 
      =SEEK(laData[1],"Fabric")
    ENDIF
  ELSE
    laColor[1,1] = 'CLRLNAME'
    laColor[1,2] = 'lcClrLD'  
    lcClrDesc  = gfCodDes(laData[2], 'COLOR')
    = gfRltFld(laData[2], @laColor, 'COLOR')
    =SEEK(laData[1]+laData[2],"Fabric")
    lnOnHand   = FABRIC.OnHand
    lnOnOrder  = FABRIC.OnOrder
    lnWip      = FABRIC.NMATWIP
    lnReorder  = FABRIC.Reorder
    lnUsage    = FABRIC.Usage
    *E301797,1 (Begin) Calculate.
    lnInTran   = FABRIC.Matotintr
    *E301797,1 (End)
    
    *E301911,1 ABD - assign Fabric sell order & ship to Variabls. [Begin]
    lnSelOrder = FABRIC.nsellorder
    lnSelInvce = FABRIC.nsellship
    *E301911,1 ABD - [End]
    
    IF ALLtRIM(UPPER(lcWare)) <> 'ALL'
      
      *SET ORDER TO TAG Fabdye INTO FABDYE
      =SEEK(laData[1]+laData[2]+lcWare,"Fabdye")
      lnOnOrder  = FABDYE.OnOrder
      lnWip      = FABDYE.NMATWIP
    ENDIF

  ENDIF
  laData[3]  = FABRIC.Desc   
  lcIType    = FABRIC.Item_Type

  *--HDM B602430,1 [start]
  *  lnType     = ASCAN(laType,lcIType)  

  *B803248,1 SSE Check for Item Type in The array and fill laType 
  *B803248,1 array with all Item Types                    [Begin]
  =IIF(llCalled,gfwCodePop( @laCodInfo , "ITEM_TYPE" , "L"),.T.)
  *B803248,1 SSE [End]

  lnType = ASUBSCRIPT(laType,ASCAN(laType,lcIType),1)

  IF EMPTY(FABRIC.CPURCODE)
    = gfwCodePop( @laCodInfo , "CPURCODE"  , "N")
  ELSE
    lcPurCD    = FABRIC.CPURCODE
    *B803631,1 WAB - Check for Pur Groupes  in The array and fill lacurr array 
    *B803631,1 WAB (START)  with all pur Groupes
    =IIF(llCalled,gfwCodePop( @laCodInfo , "CPURCODE"  , "L"),.T.)
    *B803631,1 WAB (END)
    lnPurC = ASUBSCRIPT(laPurc,ASCAN(laPurc,lcPurCd),1)

  ENDIF

  *  lnPurC     = ASCAN(laPurc,lcPurCd)
  *--HDM B602430,1 [end]
  
  laData[8] = FABRIC.Loc
  laData[12]= FABRIC.Vendor
  lcFab1    = FABRIC.cFabric1
  lcFab2    = FABRIC.cFabric2
  lcVenName = IIF(!EMPTY(laData[12]),;
                    LOOKUP(ApVendor.cVenComp,laData[12],ApVendor.cvendcode),lcVenName)
  laData[6]  = FABRIC.Pattern
  laData[10] = FABRIC.LeadTime
  laData[4]  = FABRIC.Width
  laData[5]  = FABRIC.cfabweight
  laData[9]  = FABRIC.Introduced
  laData[13] = FABRIC.Link_Code
  cbDyelot   = IIF(FABRIC.cDye_Flg= 'Y',.T.,.F.)
  laData[14] = FABRIC.lTrkRolls
  lcLinkDesc = IIF(llGl_Link .AND. !EMPTY(laData[13]),IIF(SEEK('04'+ laData[13],"GL_LINK"),GL_LINK.LinkDesc ,lcLinkDesc),lcLinkDesc)  
  laData[7]  = FABRIC.Content
  laData[11] = FABRIC.cComCode
  laData[15] = FABRIC.UomBuy
  laData[17] = FABRIC.UomUse
  lnDomestic = IIF(FABRIC.Make,1,2)
  ibDomestic = IIF(FABRIC.Make,1,2)
  lcDomestic = laDomestic[ibDomestic]
  lcCostStat = IIF(lnDomestic = 2 AND llMultCur,"DISABLE","ENABLE")
  lcCostType = IIF(lnDomestic = 1,'M.F.G. Cost','Purchasing Cost')
  
  *E301911,1 ABD Update the new field Sell Link Code. [Begin]
  ladata[25] = FABRIC.nSellPrice
  ladata[26] = FABRIC.cSellUOM
  ladata[27] = FABRIC.nSellConv
  ladata[28] = FABRIC.cSlsGlLink
  lcGllnk3  = laData[28]
  IF llGl_Link AND !EMPTY(lcGllnk3)
    =gfGLbrowse('02',@lcGllnk3,@lcSlinkcde,2)
    laData[28]=lcGllnk3
    lcLinkDesc = IIF(!EMPTY(laData[13]),IIF(SEEK('04'+ laData[13],"GL_LINK"),GL_LINK.LinkDesc ,lcLinkDesc),lcLinkDesc)      
  ENDIF  
  lcSlinkcde = IIF(EMPTY(laData[28]),'',lcSlinkcde)
  *E301911,1 ABD [End]
  
  *B603324,1 [Start] If Domestic Material UOM Make(Buy) is diffrent
  *                  than UOM Use
  *laData[15] = IIF(lnDomestic = 1,laData[17],laData[15])
  *B603324,1 [End]
  
  *--HDM B602382,1 Also Copy Cost elements if copyfab is True[start]
  
  *laData[20] = IIF(llCopyFab,FABRIC.CostBuy,0)
  *laData[21] = IIF(llCopyFab,FABRIC.CostUse,0)
  
  *B603337,1 ABD  Fix Bug when copy item that the new generated material will
  *B603337,1 ABD  Be saved with the cost of the privious Material. [Begin]
  *laData[20] = IIF(llCopyFab,FABRIC.CostBuy,IIF(laScrMode[4],0,FABRIC.CostBuy))
  *laData[21] = IIF(llCopyFab,FABRIC.CostUse,IIF(laScrMode[4],0,FABRIC.CostUse))
  IF llCopyFab
    laData[20] = IIF(lnDomestic = 1,0,FABRIC.CostBuy)
    laData[21] = IIF(lnDomestic = 1,0,FABRIC.CostUse)
  ELSE
    laData[20] = IIF(laScrMode[4],0,FABRIC.CostBuy)
    laData[21] = IIF(laScrMode[4],0,FABRIC.CostUse)
  ENDIF
  *B603337,1 ABD  [End]
  
  
  *--HDM B602382,1 Also Copy Cost elements if copyfab is True[end]
  *B603337,1 ABD  Fix Bug when copy item that the new generated material will
  *B603337,1 ABD  Be saved with the cost of the privious Material. [Begin]
  *lnCost1    = IIF(lnDomestic = 1,FABRIC.nMCost1,FABRIC.nfabcost)
  *lnCost2    = IIF(lnDomestic = 1,FABRIC.nMCost2,FABRIC.nItem_Tax)
  *lnCost3    = IIF(lnDomestic = 1,FABRIC.nMCost3,FABRIC.nItemQuota)
  *lnCost4    = IIF(lnDomestic = 1,FABRIC.nMCost4,FABRIC.nItm_Frt)
  IF llCopyFab 
    lnCost1 = IIF(lnDomestic = 1,0,FABRIC.nfabcost)
    lnCost2 = IIF(lnDomestic = 1,0,FABRIC.nItem_Tax)
    lnCost3 = IIF(lnDomestic = 1,0,FABRIC.nItemQuota)
    lnCost4 = IIF(lnDomestic = 1,0,FABRIC.nItm_Frt)
  ELSE
    lnCost1    = IIF(laScrMode[4],0,IIF(lnDomestic = 1,FABRIC.nMCost1,FABRIC.nfabcost))
    lnCost2    = IIF(laScrMode[4],0,IIF(lnDomestic = 1,FABRIC.nMCost2,FABRIC.nItem_Tax))
    lnCost3    = IIF(laScrMode[4],0,IIF(lnDomestic = 1,FABRIC.nMCost3,FABRIC.nItemQuota))
    lnCost4    = IIF(laScrMode[4],0,IIF(lnDomestic = 1,FABRIC.nMCost4,FABRIC.nItm_Frt))
  ENDIF
  *B603337,1 ABD  [End]
  
  *B603324,1 [Start] Don't force conversion factor to 1
  *                  if manufactured material
  *                  and read it form fabric file
  *laData[16] = IIF(lnDomestic = 1,1,FABRIC.Conv)
  laData[16] = FABRIC.Conv
  *B603324,1 [End]
  laData[18] = IIF(lnDomestic = 1,laData[18],FABRIC.cPriceCur)
  laData[19] = IIF(lnDomestic = 1,laData[19],FABRIC.cDutyCur)  
  IF laData[18] = gcBaseCurr
    lnExRateP = 1

    *E301911,1 ABD - Didn't show word Rate, or Not Valid Or all Date.  [Begin]
    *lcRatep = 'Rate                   :'
    lcRateP  = ''
    *E301911,1 ABD - [End]

  ELSE
    lnAlias    = SELECT()
    SELECT SYCEXCH
    lnOldTag = TAG()
    SET ORDER TO TAG Currency DESCENDING
    IF !SEEK(gcBaseCurr+laData[18]) OR SYCEXCH.nExRate = 0
      lnExRateP = SYCEXCH.nExRate

      *E301911,1 ABD - Didn't show word Rate, or Not Valid Or all Date.  [Begin]
      *lcRatep = 'No valid rate        :'
      lcRatep  = 'Invalid'
      *E301911,1 ABD - [End]

    ELSE
      lnExRateP = SYCEXCH.nExRate

      *E301911,1 ABD - Didn't show word Rate, or Not Valid Or all Date.  [Begin]
      *lcRatep = 'Rate on ' + DTOC(SYCEXCH.dRateDate) +' :'
      lcRatep  = LEFT(DTOC(SYCEXCH.dRateDate),5)
      *E301911,1 ABD - [End]

    ENDIF
    SET ORDER TO TAG &lnOldTag
  ENDIF  
  SELECT(lnAlias)
  IF laData[19] = gcBaseCurr
    lnExRateD = 1

    *E301911,1 ABD - Didn't show word Rate, or Not Valid Or all Date.  [Begin]
    *lcRateD = 'Rate                   :'
     lcRateD  = ''
    *E301911,1 ABD - [End]

  ELSE
    lnAlias    = SELECT()
    SELECT SYCEXCH
    lnOldTag = TAG()
    SET ORDER TO TAG Currency DESCENDING
    IF !SEEK(gcBaseCurr+laData[19]) OR SYCEXCH.nExRate = 0
      lnExRateD = SYCEXCH.nExRate

      *E301911,1 ABD - Didn't show word Rate, or Not Valid Or all Date.  [Begin]
      *lcRateD = 'No valid rate        :'
      lcRateD  = 'Invalid'
      *E301911,1 ABD - [End]

    ELSE
      lnExRateD = SYCEXCH.nExRate

      *E301911,1 ABD - Didn't show word Rate, or Not Valid Or all Date.  [Begin]
      *lcRateD = 'Rate on ' + DTOC(SYCEXCH.dRateDate) +' :'
      lcRateD = LEFT(DTOC(SYCEXCH.dRateDate),5)
      *E301911,1 ABD - [End]

    ENDIF
    SET ORDER TO TAG &lnOldTag
  ENDIF  
  SELECT(lnAlias)
  =SEEK(laData[18],"SycCurr")
  lcFrnSmbl1 = syccurr.ccurrsmbl
  lnUnit1    = syccurr.nCurrUnit
  =SEEK(laData[19],"SycCurr")
  lcFrnSmbl2 = syccurr.ccurrsmbl
  lnUnit2    = syccurr.nCurrUnit
  lcFrn1     =gfGetExSin("",ALLTRIM(laData[18]))
  lcFrn2     =gfGetExSin("",ALLTRIM(laData[19]))
  
  *  lnBCost1   = IIF(llCopyFab,0,IIF(lnDomestic = 1,lnCost1,IIF(lcFrn1 = '/',ROUND(lnCost1/lnExRateP/lnUnit1,3),ROUND(lnCost1*lnExRateP/lnUnit1,3))))
  *  lnBCost2   = IIF(llCopyFab,0,IIF(lnDomestic = 1,lnCost2,IIF(lcFrn2 = '/',ROUND(lnCost2/lnExRateD/lnUnit2,3),ROUND(lnCost2*lnExRateD/lnUnit2,3))))
  *  lnBCost3   = IIF(llCopyFab,0,IIF(lnDomestic = 1,lnCost3,IIF(lcFrn2 = '/',ROUND(lnCost3/lnExRateD/lnUnit2,3),ROUND(lnCost3*lnExRateD/lnUnit2,3))))
  *  lnBCost4   = IIF(llCopyFab ,0,IIF(lnDomestic = 1,lnCost4,IIF(lcFrn2 = '/',ROUND(lnCost4/lnExRateD/lnUnit2,3),ROUND(lnCost4*lnExRateD/lnUnit2,3))))

  *B603337,1 ABD  Remarek the next line [Begin]
  *  lnBCost1   = IIF(lnDomestic = 1,lnCost1,IIF(lcFrn1 = '/',ROUND(lnCost1/lnExRateP/lnUnit1,3),ROUND(lnCost1*lnExRateP/lnUnit1,3)))
  *  lnBCost2   = IIF(lnDomestic = 1,lnCost2,IIF(lcFrn2 = '/',ROUND(lnCost2/lnExRateD/lnUnit2,3),ROUND(lnCost2*lnExRateD/lnUnit2,3)))
  *  lnBCost3   = IIF(lnDomestic = 1,lnCost3,IIF(lcFrn2 = '/',ROUND(lnCost3/lnExRateD/lnUnit2,3),ROUND(lnCost3*lnExRateD/lnUnit2,3)))
  *  lnBCost4   = IIF(lnDomestic = 1,lnCost4,IIF(lcFrn2 = '/',ROUND(lnCost4/lnExRateD/lnUnit2,3),ROUND(lnCost4*lnExRateD/lnUnit2,3)))
  *B603337,1 ABD [End] 
  
  *B603337,1 ABD  Fix Bug when copy item that the new generated material will
  *B603337,1 ABD  Be saved with the cost of the privious Material. [Begin]
  lnBCost1   = IIF(llCopyFab .AND. lnDomestic = 1,0,IIF(lnDomestic = 1,lnCost1,IIF(lcFrn1 = '/',ROUND(lnCost1/lnExRateP/lnUnit1,3),ROUND(lnCost1*lnExRateP/lnUnit1,3))))
  lnBCost2   = IIF(llCopyFab .AND. lnDomestic = 1,0,IIF(lnDomestic = 1,lnCost2,IIF(lcFrn2 = '/',ROUND(lnCost2/lnExRateD/lnUnit2,3),ROUND(lnCost2*lnExRateD/lnUnit2,3))))
  lnBCost3   = IIF(llCopyFab .AND. lnDomestic = 1,0,IIF(lnDomestic = 1,lnCost3,IIF(lcFrn2 = '/',ROUND(lnCost3/lnExRateD/lnUnit2,3),ROUND(lnCost3*lnExRateD/lnUnit2,3))))
  lnBCost4   = IIF(llCopyFab .AND. lnDomestic = 1,0,IIF(lnDomestic = 1,lnCost4,IIF(lcFrn2 = '/',ROUND(lnCost4/lnExRateD/lnUnit2,3),ROUND(lnCost4*lnExRateD/lnUnit2,3))))
  *B603337,1 ABD  [End]
  
  lnCst2P    = (lnBCost2/lnBCost1)*100
  lnCst3P    = (lnBCost3/lnBCost1)*100
  lnCst4P    = (lnBCost4/lnBCost1)*100
  laData[20] = lnBCost1 + lnBCost2 + lnBCost3 + lnBCost4

  *B607415,1 ABD - get the COSTBUY after the last calculation.  [Begin]
  laData[21] = (laData[20]/laData[16]) 
  *B607415,1 ABD - [End]
  *--HDM B602382,1 Also Copy Cost elements if copyfab is True[start]

  *B603337,1 ABD  Fix Bug when copy item that the new generated material will
  *B603337,1 ABD  Be saved with the cost of the privious Material. [Begin]
  *laData[23] = IIF(llCopyFab,FABRIC.nfAve_Cost,IIF(laScrMode[4],0,FABRIC.nfAve_Cost))
  *laData[22] = IIF(llCopyFab,FABRIC.nAveCstBuy,IIF(laScrMode[4],0,FABRIC.nAveCstBuy))
  IF llCopyFab
    laData[22] = IIF(lnDomestic = 1,0,FABRIC.CostBuy)
    laData[23] = IIF(lnDomestic = 1,0,FABRIC.CostUse)
  ELSE
    *B604669,1 WAB (Start) in the view mode the avareg cost used display instead of the average cost buy
    *laData[22] = IIF(laScrMode[4],0,FABRIC.nfAve_Cost)
    *laData[23] = IIF(laScrMode[4],0,FABRIC.nAveCstBuy)
    
    *B607483,1 ABD - get the ladata of 20 and 20 in case the on hand is equal to zero. [Begin]
    *laData[23] = IIF(laScrMode[4],0,FABRIC.nfAve_Cost)
    *laData[22] = IIF(laScrMode[4],0,FABRIC.nAveCstBuy)
    laData[23] = IIF(laScrMode[4],0,IIF(lnOnhand = 0, laData[21], FABRIC.nfAve_Cost))
    laData[22] = IIF(laScrMode[4],0,IIF(lnOnhand = 0, laData[20], FABRIC.nAveCstBuy))
    *B607483,1 ABD - [End]
    
    *B604669,1 WAB (End)
  ENDIF
  *B603337,1 ABD [End]

  *--HDM B602382,1 Also Copy Cost elements if copyfab is True[end]
  
  lcCstSLbl1 = IIF(lnDomestic = 1,lcManSLbl1,lcImpSLbl1)
  lcCstSLbl2 = IIF(lnDomestic = 1,lcManSLbl2,lcImpSLbl2)
  lcCstSLbl3 = IIF(lnDomestic = 1,lcManSLbl3,lcImpSLbl3)
  lcCstSLbl4 = IIF(lnDomestic = 1,lcManSLbl4,lcImpSLbl4)
  *-- B603038,1 [Start] Make sure fabric file is the current alias
  SELECT FABRIC
  *-- B603038,1 [End]
  lcQuality  = FABRIC.cFabGrade
  lnQuality  = IIF(cFabGrade='1',1,IIF(cFabGrade='2',2,3))
  DO CASE
    CASE lnQuality = 1
      lcQua1 = "Second Quality"
      lcQua2 = "Damage"
      DIMENSION laQuality[2,3]
      laQuality[1,1] = "Second Quality"
      laQuality[2,1] = "Damage"

    CASE lnQuality = 2
      lcQua1 = "First Quality"
      lcQua2 = "Damage"
      DIMENSION laQuality[2,3]
      laQuality[1,1] = "First Quality"
      laQuality[2,1] = "Damage"

    CASE lnQuality = 3
      lcQua1 = "First Quality"
      lcQua2 = "Second Quality"
      DIMENSION laQuality[2,3]
      laQuality[1,1] = "First Quality"
      laQuality[2,1] = "Second Quality"
  ENDCASE
  lcFabric1  = cFabric1
  lcFabric2  = cFabric2  
  lcColor1   = cColor1
  lcColor2   = cColor2  
  laQuality[1,2] = lcFabric1
  laQuality[2,2] = lcFabric2
  laQuality[1,3] = lcColor1
  laQuality[2,3] = lcColor2
  lcQuaClr       = lcColor1
  IF !llAllColrs
    
    SELECT * ,'S' AS cStatus        ;  
      FROM FabDye          ;                                         
     WHERE FABRIC+COLOR+CWARECODE+DYELOT+STR(RECNO(),7)= SUBSTR(laData[1],1,7) + SUBSTR(laData[2],1,6);
          .AND. EMPTY(Dyelot)   ;
      INTO DBF (gcWorkDir+lcTempWare)
    INDEX ON FABRIC+COLOR+CWARECODE TAG (lcTempWare)  
  ENDIF
  
ELSE
  laData[14] = .F.
  cbDyelot   = .F.
  llCopyFab  = .F.
  llLoop     = .T.

  *E301582,1 ABD - If we inport from imoprt file don't empty this variable.[Begin]
  IF !llSpIstall .AND. !llImprtFab
    *E301582,1 ABD - [End]
    lcFabric   = SPACE(07)
    laData[1]  = SPACE(07)    
    *E301582,1 ABD - End for if statement. [Begin]
  ENDIF
  *E301582,1 ABD - [End]
  laData[3]  = SPACE(20)
  lcClrDesc  = SPACE(15)
  lcClrLD    = SPACE(30)
  lcIType    = SPACE(02)
  lcOldIType = SPACE(02)
  lcTypeDes  = SPACE(15)
  laData[8]  = SPACE(05)
  laData[12] = SPACE(08)
  lcOldVen   = SPACE(08)
  lcVenName  = SPACE(30)
  laData[6]  = SPACE(10)
  laData[10] = 0
  laData[4]  = SPACE(06)
  laData[5]  = SPACE(15)
  laData[13] = "DEFDEF"
  lcOldLink  = "DEFDEF"

  *E301911,1 ABD - Add New variable to hold the default sell code. [Begin]
  lcOldSelLk = "DEF"
  *E301911,1 ABD - [End]
  
  lcLinkDesc = SPACE(30)
  laData[7]  = SPACE(60)
  laData[11] = SPACE(20)
  lnDomestic = 2
  lcDomestic = laDomestic[lnDomestic,1]
  lcCostStat = IIF(lnDomestic = 2 AND llMultCur,"DISABLE","ENABLE")
  lnOldValue = lnDomestic
  lcOldDom   = lcDomestic
  laData[2]  = SPACE(6)
  lcSAlClr   = "All Colors "
  laData[9]  = DATE()
  laData[15] = SPACE(3)
  laData[17] = SPACE(3)
  lcCostType = IIF(lnDomestic = 1,'M.F.G. Cost','Purchasing Cost')  
  *B603324,1 [Start] If Domestic Material UOM Make(Buy) is diffrent
  *                  than UOM Use
  *laData[15] = IIF(lnDomestic = 1,laData[17],laData[15])
  *B603324,1 [End]

  *E301911,1 ABD Update the new field Sell Link Code. [Begin]
  ladata[25] = 0
  ladata[26] = ''
  ladata[27] = 0
  laData[28] =IIF(EMPTY(laData[28]),'DEF',laData[28])
  lcGllnk3   = laData[28]
  IF llGl_Link AND !EMPTY(lcGllnk3)
    =gfGLbrowse('02',@lcGllnk3,@lcSlinkcde,2)
    laData[28]=lcGllnk3
    lcLinkDesc = IIF(!EMPTY(laData[13]),IIF(SEEK('04'+ laData[13],"GL_LINK"),GL_LINK.LinkDesc ,lcLinkDesc),lcLinkDesc)      
  ENDIF  
  lcSlinkcde = IIF(EMPTY(laData[28]),'',lcSlinkcde)
  *E301911,1 ABD [End]

  lcCstSLbl1 = IIF(lnDomestic = 1,lcManSLbl1,lcImpSLbl1)
  lcCstSLbl2 = IIF(lnDomestic = 1,lcManSLbl2,lcImpSLbl2)
  lcCstSLbl3 = IIF(lnDomestic = 1,lcManSLbl3,lcImpSLbl3)
  lcCstSLbl4 = IIF(lnDomestic = 1,lcManSLbl4,lcImpSLbl4)

  laData[16] = 1
  *E301911,1 ABD assign 1 to Sales Conv. fields . [Begin]
  laData[27] = 1
  *E301911,1 ABD [End]

  OConvFact  = 1 
  laData[18] = gcbasecurr
  laData[19] = gcbasecurr
  lnExRateD  = 1
  lnExRateP  = 1
  =SEEK(laData[19],"SycCurr")
  lcFrnSmbl1 = syccurr.ccurrsmbl
  lcFrnSmbl2 = syccurr.ccurrsmbl
  lcFrn1     =gfGetExSin("",ALLTRIM(laData[18]))
  lcFrn2     =gfGetExSin("",ALLTRIM(laData[19]))
  lnCost1    = 0
  lnCost2    = 0
  lnCost3    = 0
  lnCost4    = 0
  lnCst2P    = 0
  lnCst3P    = 0
  lnCst4P    = 0
  lnBCost1   = 0
  lnBCost2   = 0
  lnBCost3   = 0
  lnBCost4   = 0      
  laData[20] = 0
  laData[22] = 0
  laData[21] = 0
  laData[23] = 0
  lcWareCode = SPACE(6)
  lnReorder  = 0
  lnOnOrder  = 0
  lnOnHand   = 0
  lnWIP      = 0
  lnUsage    = 0
  *E301797,1 (Begin) Initialize.
  lnInTran   = 0
  *E301797,1 (End)
  
  *E301911,1 ABD - assign 0 to Variabls. [Begin]
  STORE 0 TO lnSelOrder , lnSelInvce
  *E301911,1 ABD - [End]
    
  llAllColrs = .F.
  lcWare     = "All"
  lcQuality  = '1'
  lnQuality  = 1
  lcFabric1  = SPACE(07)
  lcFabric2  = SPACE(07)  
  lcColor1   = SPACE(06)
  lcColor2   = SPACE(06)  
  lcQuaClr   = SPACE(06)
  DIMENSION laQuality[2,3]
  laQuality[1,1] = "Second Quality"
  laQuality[2,1] = "Damage"

  
  *E301582,1 ABD - ass. the variables from imoprted style.[Begin]
  IF laScrMode[4] .AND. llSpIstall .AND. llImprtFab
    laData[3]  = Pdmatrl.Desc
    laData[4]  = Pdmatrl.Width
    laData[6]  = Pdmatrl.Pattern
    laData[7]  = Pdmatrl.Content
    *B604329,1 WAB (Start) get the cost elemnt 1 .
    lnBCost1   = Pdmatrl.nFabcost
    *B604329,1 WAB (End)
    llAllColrs = .T.
    
    *E301740,4 ABD - Assign the note pade fiels to this variable. [Begin]
    lcPdmNotes = ALLTRIM(Pdmatrl.Mnotes)
    *E301740,4 ABD - [End]
    
    
    *B604320,1 ABD Add new fields to be supported by the material program
    *B604320,1 ABD When import item from PDM files & if this field is update
    *B604320,1 ABD Before replace with the new value. [Begin]
    IF !EMPTY(Pdmatrl.mCustom)
      *-- Variable hold number of lines.
      STORE 0 To lnMemLins ,lnMemoLn 
      lnOldMemW = SET("MEMOWIDTH")
      SET MEMOWIDTH TO 256
      
      *-- Number of lines in the memo field.
      lnMemLins = MEMLINES(Pdmatrl.mCustom)
      
      *-- Save all line in array
      IF lnMemLins # 0
        DIMENSION laCusmFlds [lnMemLins,2]
        lnMemoLn = 1
        
        FOR lnMemoLn = 1 To lnMemLins
          *-- lcFldnam :- Hold the memo line.
          lcFldnam  = MLINE(Pdmatrl.mCustom,lnMemoLn)
          *--lnPostion :- hold the '= ' Postion.
          lnPostion = ATC('=',lcFldnam)
          IF !EMPTY(ALLTRIM(lcFldnam)) .AND. lnPostion # 0 .AND. lnPostion # len(lcFldnam)
            laCusmFlds [lnMemoLn , 1] = SUBSTR(lcFldnam,lnPostion + 1 ,LEN(lcFldnam))
            laCusmFlds [lnMemoLn , 2] = LEFT(lcFldnam,lnPostion - 1)
          ENDIF
        ENDFOR
        SET MEMOWIDTH TO lnOldMemW
      
        *-- Ass. following fields from imported  to variables.

        *-- DESC
        *-- CONTENT
        *-- LOC
        *-- PATTERN
        *-- WIDTH
        *-- CFABWEIGHT
        *-- UOMBUY
        *-- UOMUSE

        *-- Get the description field.
        lcReturnVl = lfVldPos('DESC')
        laData[3] = IIF (EMPTY(lcReturnVl),laData[3],lcReturnVl)

        *-- Get the Pattern field.
        lcReturnVl = lfVldPos('PATTERN')
        laData[6] = IIF (EMPTY(lcReturnVl),laData[6],lcReturnVl)
        
        *-- Get the Content field.
        lcReturnVl = lfVldPos('CONTENT')
        laData[7] = IIF (EMPTY(lcReturnVl),laData[7],lcReturnVl)
        
        *-- Get the Width field.
        lcReturnVl = lfVldPos('WIDTH')
        laData[4] = IIF (EMPTY(lcReturnVl),laData[4],lcReturnVl)
        
        *-- Get the Loc field.
        lcReturnVl = lfVldPos('LOC')
        laData[8] = IIF (EMPTY(lcReturnVl),laData[8],lcReturnVl)
        
        *-- Get the CfabWeight field.
        lcReturnVl = lfVldPos('CFABWEIGHT')
        laData[5] = IIF (EMPTY(lcReturnVl),laData[5],lcReturnVl)
        
        *-- Get the UomBuy field.
        lcReturnVl = lfVldPos('UOMBUY')
        laData[15] = IIF (EMPTY(lcReturnVl),laData[15],lcReturnVl)
        
        *-- Get the Uomuse field.
        lcReturnVl = lfVldPos('UOMUSE')
        laData[17] = IIF (EMPTY(lcReturnVl),laData[17],lcReturnVl)
        
      ENDIF
    ENDIF  
    *B604320,1 ABD [End]

  ENDIF
  *E301582,1 ABD - [End]
  
  =lfRefresh()
ENDIF
IF llMultiWH
  lcAll     = 'All'
  DECLARE laWare[1]
  laWare[1] = "All"
  lcWare    = laWare[1]
ENDIF
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfVldPos
*! Developer : Abdou Elgndi
*! Date      : 03/26/20001
*! Purpose   : Get the fields Postition.
*!*************************************************************
*! Calls     : From Program
*!*************************************************************
*! Parameters: lcParam
*!*************************************************************
*! Returns   :  NolcFldCont.
*!*************************************************************
*B604320,1
FUNCTION lfVldPos
PARAMETER lcParam
lcFldCont = ''

FOR  I = 1 To ALEN(laCusmFlds,1)
  IF !EMPTY (laCusmFlds [I,2]) .AND. laCusmFlds [I,2] = lcParam
    lcFldCont = laCusmFlds [I,1]
    EXIT
  ENDIF
ENDFOR

RETURN lcFldCont

*--End Of lfVldPos.
*!*************************************************************
*! Name      : lfvWare
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : calls the screen to add new warehouse
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvWare
PRIVATE lnAlias

lnAlias = SELECT()

SELECT (lcTempWare)
SET RELATION TO cWareCode INTO WAREHOUS ADDITIVE
GO TOP
llWarFirst = !EOF()
lcDispMode = IIF((laScrMode[3] OR laScrMode[4]) .AND. !EOF(),'ENABLE','DISABLE')
lnRecNo    = RECNO()
lcGL_Link  = GL_Link

*E301911,1 ABD - Add new variable to hold the Sell link code. [Begin]
lcslsGlLnk  = cSlsGlLink
*E301911,1 ABD - [End]

lcFObject  = IIF(laScrMode[2],"PBOKWAR" , "PBINSWAR"  )
lcLObject  = IIF(laScrMode[2],"PBOKWAR" , "PBCLOSEWAR")
PUSH KEY
=gfClearKey()
ON KEY LABEL TAB        llDummi=lfKeyTrap("FORWORD" ,lcFObject,lcLObject)
ON KEY LABEL RIGHTARROW llDummi=lfKeyTrap("FORWORD" ,lcFObject,lcLObject)
ON KEY LABEL DNARROW    llDummi=lfKeyTrap("FORWORD" ,lcFObject,lcLObject)
ON KEY LABEL BACKTAB    llDummi=lfKeyTrap("BACKWORD",lcFObject,lcLObject)
ON KEY LABEL LEFTARROW  llDummi=lfKeyTrap("BACKWORD",lcFObject,lcLObject)
ON KEY LABEL UPARROW    llDummi=lfKeyTrap("BACKWORD",lcFObject,lcLObject)
ON KEY LABEL CTRL+W     llDummi=1  
ON KEY LABEL CTRL+Q     llDummi=1
DO (gcScrDir+gcWinAppl+'\MAMATRB.SPX')
POP KEY
SET RELATION TO
SELECT(lnAlias)
* --------------------------------------------------------------- 

FUNCTION lfGo2Brows

IF llWarFirst
  llWarFirst = .F.
  KEYBOARD "{BACKTAB}" CLEAR
ENDIF  
*!*************************************************************
*! Name      : lfvInsWar
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Validate the insertion of a warehouse
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvInsWar

lcWareCode = gfBrowWare(.T.)
lcWare_Ttl = "Location"
lcWareCode = IIF(TYPE('lcWareCode')='C',lcWareCode,SPACE(6))
SELECT (lcTempWare)

IF !EMPTY(lcWareCode)
  IF SEEK(laData[1]+laData[2]+lcWareCode)
    =gfModalGen('TRM36026B36000','ALERT')
  ELSE
    APPEND BLANK
    *--HDM 03/08/1999 (B) Add_Date Not found[start]
    *-- We shouldn't add User info in temporary file
    
    *REPLACE Fabric     WITH laData[1]      ,;
            Color      WITH laData[2]     ,;
            Dyelot     WITH SPACE(10)   ,;
            cWareCode  WITH lcWareCode  ,;
            nfAve_Cost WITH laData[23]  ,;
            NaveCstBuy WITH laData[22]  ,;
            Add_Date   WITH DATE()      ,;
            Add_time   WITH TIME()      ,;
            Ad d_UserId WITH gcUser_Id     ,;
            cStatus    WITH 'A' 

    REPLACE Fabric     WITH laData[1]      ,;
            Color      WITH laData[2]     ,;
            Dyelot     WITH SPACE(10)   ,;
            cWareCode  WITH lcWareCode  ,;
            nfAve_Cost WITH laData[23]  ,;
            NaveCstBuy WITH laData[22]  ,;
            cStatus    WITH 'A' 
  *--HDM 03/08/1999 (B) Add_Date Not found[End]





    SHOW GET lcGL_Link  ENABLE
    SHOW GET ibWarGlLnk ENABLE
    SHOW GET pbDelWar   ENABLE

    *E301911,1 ABD - Show the new variable that hold the sell link code. [Begin]
    SHOW GET ibWrGlLink ENABLE
    SHOW GET lcslsGlLnk ENABLE
    *E301911,1 ABD - [End]

    SHOW WINDOW (lcWare_Ttl) REFRESH
    =lfWRefresh()
    =lfwWareBrs()
    IF llGl_Link
      _CUROBJ = OBJNUM(lcGL_Link)
    ENDIF  
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfvDelWar
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Validate the deletion of a warehouse
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvDelWar
PRIVATE lnAlias, lcMessage

lnAlias = SELECT()
SELECT (lcTempWare)
llDelete = .F.
IF gfModalGen('QRM36027B36001','ALERT',ALLTRIM(cWareCode)) = 1 
  lnTotStk = IIF(cStatus <> 'A' .AND.SEEK (SUBSTR(laData[1],1,7)+PADL(SUBSTR(laData[2],1,6),6,' ')+;
                 cWareCode,'FabDye'),FabDye.onhand,0)
  IF lnTotStk > 0
    IF llGl_Link
      =gfModalGen('QRM36028B36000','ALERT',ALLTRIM(laData[1])+'/'+ALLTRIM(laData[2]))
    ELSE
      lcMessage = "Fabric/Color " + ALLTRIM(laData[1])+'/'+ALLTRIM(laData[2]) +;
                  " has inventory, Are you sure you want to remove the Fabric from this Location."
      IF gfModalGen('QRM36029B36001','ALERT',ALLTRIM(laData[1])+'/'+ALLTRIM(laData[2])) = 1
        llDelete = .T.
      ENDIF    
    ENDIF               
  ELSE
    llDelete = .T.
  ENDIF
  IF llDelete
    REPLACE cStatus WITH 'D'
    DELETE
    GO TOP
    IF EOF()
      SHOW GET lcGL_Link  DISABLE
      SHOW GET ibWarGlLnk DISABLE  
      SHOW GET pbDelWar   DISABLE

      *E301911,1 ABD - Show the new variable that hold the sell link code. [Begin]
      SHOW GET ibWrGlLink DISABLE
      SHOW GET lcslsGlLnk DISABLE
      *E301911,1 ABD - [End]

    ENDIF  
    SHOW WINDOW (lcWare_Ttl) REFRESH
    =lfWRefresh()
    =lfwWareBrs()
  ENDIF  
ENDIF
SELECT(lnAlias)
*!*************************************************************
*! Name      : lfvWCancel
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : clear reads of warehouse screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvWCancel

ACTIVATE WINDOW (WOUTPUT())
CLEAR READ
*!*************************************************************
*! Name      : lfvWarOk
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : clear reads of warehouse screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvWarOk

ACTIVATE WINDOW (WOUTPUT())
CLEAR READ
*!*************************************************************
*! Name      : lfwWareBrs
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : browse for avialable warehouses
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfwWareBrs

lnRecNo   = RECNO()
SHOW WINDOW (lcWare_Ttl) REFRESH
lcGL_Link = gl_link

*E301911,1 ABD - New variable that hold the sell link code. [Begin]
lcslsGlLnk = cSlsGlLink
*E301911,1 ABD - [End]

=lfWRefresh()
SHOW GET lcGL_Link

*E301911,1 ABD - Show the new variable that hold the sell link code. [Begin]
SHOW GET lcslsGlLnk
*E301911,1 ABD - [End]

*!*************************************************************
*! Name      : lfKeyTrap
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : trap keys for the warehouse screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfKeyTrap
PARAMETERS lcDirection,lcFirstObj,lcLastObj

lcObjChek = IIF(lcDirection = "FORWORD",UPPER(lcLastObj) ,UPPER(lcFirstObj))
lcObjJump = IIF(lcDirection = "FORWORD",UPPER(lcFirstObj),UPPER(lcLastObj) )
lnFactor  = IIF(lcDirection = "FORWORD",1,-1)

IF WONTOP(WOUTPUT()) .AND. VARREAD()=lcObjChek
  ACTIVATE WINDOW (lcWare_Ttl)
ELSE
  _CUROBJ =IIF(WONTOP(WOUTPUT()),_CUROBJ+(1*lnFactor),OBJNUM(&lcObjJump))
  ACTIVATE WINDOW (WOUTPUT())
ENDIF  
*!*************************************************************
*! Name      : lfWarActiv
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : activate of warehouse screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfWarActiv

ON KEY LABEL RIGHTARROW llDummi=lfKeyTrap("FORWORD" ,lcFObject,lcLObject)
ON KEY LABEL DNARROW    llDummi=lfKeyTrap("FORWORD" ,lcFObject,lcLObject)
ON KEY LABEL LEFTARROW  llDummi=lfKeyTrap("BACKWORD",lcFObject,lcLObject)
ON KEY LABEL UPARROW    llDummi=lfKeyTrap("BACKWORD",lcFObject,lcLObject)
ON KEY LABEL ENTER
ON KEY LABEL Ctrl+ENTER
ON KEY LABEL ESC
*!*************************************************************
*! Name      : lfWarDeact
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : deactivate of warehouse screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfWarDeact

ON KEY LABEL RIGHTARROW 
ON KEY LABEL DNARROW    
ON KEY LABEL LEFTARROW  
ON KEY LABEL UPARROW    
ON KEY LABEL Ctrl+ENTER llDummi=lfvWarOk()
ON KEY LABEL ESC        llDummi=lfvWCancel()
RETURN .F.
*!*************************************************************
*! Name      : lfActBrow
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : browse the assigned warehouses for the selected fabric/color
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfActBrow
PRIVATE lcBrowfields

SELECT (lcTempWare)

*E301911,1 ABD - Add the sell link code to the browse. [Begin]
*lcBrowfields = "dummi=IIF(RECNO()=lnRecNo,'>',' '):H='',cWareCode :H= 'Location'," +;
             "lcWareDesc=Warehous.cDesc:H= 'Description'," +; 
             "OnHand:H= 'OnHand'," + ;
             "Usage :H= 'Usage',"  + ;
             "NaveCstBuy :H= 'Ave.Cost'"+ ;
             IIF(llGl_Link,',GL_Link:H="GL Link":R','')
lcBrowfields = "dummi=IIF(RECNO()=lnRecNo,'>',' '):H='',cWareCode :H= 'Location'," +;
             "lcWareDesc=Warehous.cDesc:H= 'Description'," +; 
             "OnHand:H= 'OnHand'," + ;
             "Usage :H= 'Usage',"  + ;
             "NaveCstBuy :H= 'Ave.Cost'"+ ;
             IIF(llGl_Link,',GL_Link:H="Cost Link Code ":R','')+;
             IIF(llGl_Link,',cSlsGlLink :H="Sell Link Code":R','')
*E301911,1 ABD - [End]

BROWSE FIELDS &lcBrowfields;
       LOCK 0   ;
       NOAPPEND ;
       NOEDIT   ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       WHEN lfwWareBrs()   ;       
       TITLE lcWare_Ttl    ;
       WINDOW MAT100B1 IN WINDOW MAT100_B
*!*************************************************************
*! Name      : lfvGLWareH
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : validate the Gl_Link for the selected warehouse
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION  lfvGLWareH
PRIVATE llDown
llDown = MDOWN()

IF llDown
  RETURN
ENDIF 

IF EMPTY(lcGL_Link)
    * MOD Fields No Longer Used[Start]
    *REPLACE GL_Link    WITH lcGL_Link ;
            cStatus    WITH IIF (cStatus='A','A','M') ;
            Mod_Action WITH 'MODIFIED' ;
            Mod_Date   WITH DATE()     ;
            Mod_Time   WITH TIME()     ;
            Mod_UserID WITH gcUser_Id

    REPLACE GL_Link    WITH lcGL_Link ;
            cStatus    WITH IIF (cStatus='A','A','M')
    * MOD Fields No Longer Used[End]

ELSE
  =gfGLBrowse('04',@lcGL_Link)
  IF !EMPTY(lcGL_Link)
    * MOD Fields No Longer Used[Start]
    *REPLACE GL_Link WITH lcGL_Link ;
            cStatus WITH IIF (cStatus='A','A','M') ;
            Mod_Action WITH 'MODIFIED' ;
            Mod_Date   WITH DATE()     ;
            Mod_Time   WITH TIME()     ;
            Mod_UserID WITH gcUser_Id

    REPLACE GL_Link WITH lcGL_Link ;
            cStatus WITH IIF (cStatus='A','A','M')
    * MOD Fields No Longer Used[End]
  ELSE
    lcGL_Link = lcOldLink
  ENDIF
ENDIF  
SHOW GET lcGL_Link
SHOW WINDOW (lcWare_Ttl) REFRESH
*!*************************************************************
*! Name      : lfvGLk
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : validate the Gl_Link for the selected warehouse
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvGLk

lcGL_Link = PADR("?",3)
llNothing = lfvGLWareH()
*!*************************************************************
*! Name      : lfvInvJrnl
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : browse the Inv Transaction for the selected fabric/color
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************

FUNCTION lfvInvJrnl
PRIVATE lnAlias, lcMessage,lcBrFields
lnAlias   = SELECT()
llBrowRoll = llTrkRolls AND FABRIC.lTrkRolls AND llTranRoll
SELECT MatInvJl
IF SEEK(laData[1]+laData[2]+IIF(lcWare<>'All',lcWare,''))
  SET ORDER TO TAG MtInvSeq
  llDyeLvl   = llDyeLot .AND. (Fabric.cDye_Flg = "Y") .AND. !llMultiWH
  lcFile_Ttl = 'Inventory Journal'
*--HDM E301037,1 11/10/1998[START] Receive return Material Should Be 
*--                                Issue Transaction Not Receive Transaction

*--HDM B602427,1[START]
*  lcBrFields = [TrnTyp = IIF(cTranType $ '12346',]+;
*                         [laTrnType(IIF(AT(cTranType,'12346')= 4 .AND. nIssued < 0, 6, IIF(AT(cTranType,'12346')=1 .AND. nIssued >0 ,4 ,AT(cTranType,'12346')))),'')]
*--HDM B602427,1[END]

  *B605995,1 KHM 05/22/2002 (Begin) Displaying the MAT PO in case of TranType="1"
  *lcBrFields = [TrnTyp = IIF(cTranType $ '12346',]+;
                         [laTrnType(IIF(AT(cTranType,'12346')= 4 .AND. nReceived > 0, 6, IIF(AT(cTranType,'12346')=1 .AND. nIssued >0 ,4 ,AT(cTranType,'12346')))),'')]+;
                         [:H='Trans. Type':13,]+;              
               IIF(llDyeLvl, [cDyeLot:10:H='Dyelot',],[])+;
               IIF(llMultiWH,[cWareCode:10:H='Location',],[])+;
               IIF(llBrowRoll,[ROLLS.CROLLID :H= 'Roll_ID',],[])+;
               [dTranDate:8:H='Date',]+;
               IIF(llCostPrv,;
               [nUntCst=nUntCstBuy/FABRIC.Conv:P='99999.999':9:H='Unit Cost',],[])+;
               [nQuantity = IIF(EMPTY(nReceived), - nIssued, nReceived)]+;
                              [:H='Quantity',]+;
               IIF(llCostPrv,;                         
               [nTAmount = IIF(EMPTY(nReceived),-nIssued, nReceived)*nUntCstBuy/FABRIC.Conv]+;
               [:P='99999999.999':12:H='Total Amount',],[])+;
               [REFERENCE:H='Reference',]+;
               [cTktNo:8:H='Trans#',]+;
               [cTran:8:H=IIF(Fabric.Make,'Order#','Mat PO#'),]+;
               [cTrn_Seq:6:H='Tran.Seq',]+;
               [cRSession:8:H='R.Sess',]+;
               [cISession:8:H='I.Sess']
  *E301911,1 ABD - Get the Round For ROUND(nUntCstBuy/FABRIC.Conv,3) Before * in Issue Or
  *E301911,1 ABD - RECIVE QTY. [Begin]
  *lcBrFields = [TrnTyp = IIF(cTranType $ '12346',]+;
                         [laTrnType(IIF(AT(cTranType,'12346')= 4 .AND. nReceived > 0, 6, IIF(AT(cTranType,'12346')=1 .AND. nIssued >0 ,4 ,AT(cTranType,'12346')))),'')]+;
                         [:H='Trans. Type':13,]+;              
               IIF(llDyeLvl, [cDyeLot:10:H='Dyelot',],[])+;
               IIF(llMultiWH,[cWareCode:10:H='Location',],[])+;
               IIF(llBrowRoll,[ROLLS.CROLLID :H= 'Roll_ID',],[])+;
               [dTranDate:8:H='Date',]+;
               IIF(llCostPrv,;
               [nUntCst=nUntCstBuy/FABRIC.Conv:P='99999.999':9:H='Unit Cost',],[])+;
               [nQuantity = IIF(EMPTY(nReceived), - nIssued, nReceived)]+;
                              [:H='Quantity',]+;
               IIF(llCostPrv,;                         
               [nTAmount = IIF(EMPTY(nReceived),-nIssued, nReceived)*nUntCstBuy/FABRIC.Conv]+;
               [:P='99999999.999':12:H='Total Amount',],[])+;
               [REFERENCE:H='Reference',]+;
               [cTktNo:8:H='Trans#',]+;
               [lcTran=IIF(cTranType="1",cTran,""):8:H=IIF(Fabric.Make,'Order#','Mat PO#'),]+;
               [cTrn_Seq:6:H='Tran.Seq',]+;
               [cRSession:8:H='R.Sess',]+;
               [cISession:8:H='I.Sess']
               
  lcBrFields = [TrnTyp = IIF(cTranType $ '123465',]+;
               [laTrnType(IIF((AT(cTranType,'12346')= 4 .OR. AT(cTranType,'123465')=6).AND. nReceived > 0,]+;
               [IIF(AT(cTranType,'123465')=6,8,6),IIF((AT(cTranType,'12346')=1 .OR. AT(cTranType,'123465')=6 ) .AND. nIssued >0 ,]+;
               [IIF(AT(cTranType,'123465')=6,7,4),AT(cTranType,'12346')))),'')]+;
               [:H='Trans. Type':13,]+;              
               IIF(llDyeLvl, [cDyeLot:10:H='Dyelot',],[])+;
               IIF(llMultiWH,[cWareCode:10:H='Location',],[])+;
               IIF(llBrowRoll,[ROLLS.CROLLID :H= 'Roll_ID',],[])+;
               [dTranDate:8:H='Date']

  *B120471,1 ABD - Fix bug that when View transactions by roll shows incorrect data into onhand qty. [Begin]
  *lcBrFields = lcBrFields + ','+IIF(llCostPrv,;
               [nUntCst=nUntCstBuy/FABRIC.Conv:P='99999.999':9:H='Unit Cost',],[])+;
               [nQuantity = IIF(EMPTY(nReceived), - nIssued, nReceived)]+;
               [:H='Quantity',]+;
               IIF(llCostPrv,;                         
               [nTAmount = IIF(EMPTY(nReceived),-nIssued, nReceived)*ROUND(nUntCstBuy/FABRIC.Conv,3)]+;
               [:P='99999999.999':12:H='Total Amount',],[])+;
               [REFERENCE:H='Reference',]+;
               [lcTranInv = IIF(cTranType="5",cTran,cTktNo):8:H='Trans#',]+;
               [lcTran=IIF(cTranType="1",cTran,""):8:H=IIF(Fabric.Make,'Order#','Mat PO#'),]+;
               [cTrn_Seq:6:H='Tran.Seq',]+;
               [cRSession:8:H='R.Sess',]+;
               [cISession:8:H='I.Sess']
               
  lcBrFields = lcBrFields + ','+IIF(llCostPrv,;
               [nUntCst=nUntCstBuy/FABRIC.Conv:P='99999.999':9:H='Unit Cost',],[])+;
               [nQuantity = IIF(llBrowRoll,IIF(EMPTY(nReceived),-Rolls.nQtyBal,Rolls.nQtyBal),IIF(EMPTY(nReceived), - nIssued, nReceived))]+;
               [:H='Quantity',]+;
               IIF(llCostPrv,;                         
               [nTAmount = (IIF(llBrowRoll,IIF(EMPTY(nReceived),-Rolls.nQtyBal,Rolls.nQtyBal),IIF(EMPTY(nReceived),-nIssued, nReceived)))*ROUND(nUntCstBuy/FABRIC.Conv,3)]+;
               [:P='99999999.999':12:H='Total Amount',],[])+;
               [REFERENCE:H='Reference',]+;
               [lcTranInv = IIF(cTranType="5",cTran,cTktNo):8:H='Trans#',]+;
               [lcTran=IIF(cTranType="1",cTran,""):8:H=IIF(Fabric.Make,'Order#','Mat PO#'),]+;
               [cTrn_Seq:6:H='Tran.Seq',]+;
               [cRSession:8:H='R.Sess',]+;
               [cISession:8:H='I.Sess']
  *B120471,1 ABD - [End]
  
  *E301911,1 ABD - [End]
  *B605995,1 KHM 05/22/2002 (End)
  
*--HDM E301037,1 11/10/1998[END]

  lcItem = laData[1]
  lcClr  = laData[2]
  IF llBrowRoll
    lcRollTag = ORDER('ROLLS')
    SET ORDER TO TAG Session IN ROLLS
    SET RELATION TO MATINVJL.ctrn_seq+MATINVJL.cFabric+MATINVJL.cColor INTO Rolls ADDITIVE
    SET SKIP TO Rolls
  ENDIF

  IF lcWare = 'All'
  *B605189,1 [ BEGIN ] Use the full expression for optimization
  *   =AriaBrow([FOR CFABRIC + CCOLOR = lcItem+lcClr], lcFile_Ttl,;
  *                       gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,;
  *                       '','Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
  *ELSE
  *   =AriaBrow([FOR CFABRIC + CCOLOR = lcItem+lcClr +PADR(lcWare,6)], lcFile_Ttl,;
  *                      gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,;
  *                       '','Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')  

  *B120471,1 ABD - Browse only the received Quantity in case show the rolls transaction for the physically inventory only. [Begin]
  *  =AriaBrow([FOR CFABRIC+CCOLOR+cwarecode+cdyelot+crsession+cisession = lcItem+lcClr], lcFile_Ttl,;
  *                      gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,;
  *                      '','Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
  *ELSE
  *  =AriaBrow([FOR CFABRIC+CCOLOR+cWarecode+cdyelot+crsession+cisession = lcItem+lcClr +PADR(lcWare,6)], lcFile_Ttl,;
  *                      gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,;
  *                      '','Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
    =AriaBrow([FOR CFABRIC+CCOLOR+cwarecode+cdyelot+crsession+cisession = lcItem+lcClr .AND. IIF(llBrowRoll,;
    ((!(nReceived = 0) .OR. !(nIssued  = 0)) .AND. !(nIssued > 0  .AND. cTranType = '3') ),.T.) ], lcFile_Ttl,;
                        gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,;
                        '','Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
  ELSE
    =AriaBrow([FOR CFABRIC+CCOLOR+cWarecode+cdyelot+crsession+cisession = lcItem+lcClr +PADR(lcWare,6) .AND. IIF(llBrowRoll,;
    ((!(nReceived = 0) .OR. !(nIssued  = 0)) .AND. !(nIssued > 0  .AND. cTranType = '3') ),.T.) ], lcFile_Ttl,;
                        gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,;
                        '','Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
  *B120471,1 ABD - [End]
  *B605189,1 [ END ]                        
  ENDIF       
  SELECT MatInvJl
  SET ORDER TO TAG MatInvJl
  IF llBrowRoll
    SET RELATION TO
    SET ORDER TO TAG lcRollTag IN ROLLS
  ENDIF
ELSE
  =gfModalGen('QRM36030B36000','ALERT', 'transaction|'+ALLTRIM(laData[1]) + '/'+ALLTRIM(laData[2]))
ENDIF

SELECT IIF(EMPTY(lnAlias), 0, (lnAlias))
*!*************************************************************
*! Name      : lfvLoc
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : browse the locations for the selected fabric/color
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvLoc
PRIVATE lnAlias,lcBrFields

lnAlias = SELECT()
SELECT WhsLoc
IF SEEK(laData[1]+SPACE(12)+laData[2]+IIF(lcWare<>'All',lcWare,''))
  lcTitle= 'Locations for ' + ALLTRIM(laData[1]) + '/' + ALLTRIM(laData[2]) 
  lcBrFields = IIF(lcWare = 'All' AND llMultiWH,[cWareCode :H='Location',],[])+;
               [cLocation :H= 'Location']
  lcItem   = laData[1] + SPACE(12)
  lcClr    = laData[2]
  IF lcWare = 'All'
    =ARIABROW([FOR Style+Color= lcItem+lcClr],;
                 lcTitle,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
  ELSE            
    = ARIABROW([FOR Style+Color+cWareCode= lcItem+lcClr+lcWare],;
                 lcTitle,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,"",'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
  ENDIF
ELSE
  =gfModalGen('QRM36030B36000','ALERT', 'locations|'+ALLTRIM(laData[1]) + '/'+ALLTRIM(laData[2]))
ENDIF
SELECT(lnAlias)
*!*************************************************************
*! Name      : lfvDyeLots
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : browse the dyelots for the selected fabric/color
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvDyeLots
*B802766,1 [Start] Define a new private variable to hold the
*                  fabdye old index
*PRIVATE lnAlias
PRIVATE lnAlias , lcDyeIndx
*B802766,1 [End]

lnAlias   = SELECT()
llBrowDye = .F.
SELECT FABDYE
*B802766,1 [Start] Make Sure that fabdye file index is fabdye
*get the old fabdye index
lcDyeIndx = ORDER()
SET ORDER TO FABDYE
*B802766,1 [End]
=SEEK(laData[1] + laData[2]+IIF(lcWare<>'All',lcWare,''))
SCAN WHILE FABRIC+COLOR = laData[1] + laData[2]
  IF !EMPTY(DYELOT)
    llBrowDye = .T.
    EXIT
  ENDIF
ENDSCAN
IF llBrowDye 
  lcTitle= 'Dyelots for ' + ALLTRIM(laData[1]) + '/' + ALLTRIM(SUBSTR(laData[2],1,6)) 
  lcBrFields = [Dyelot :H='Dyelot',]+; 
               IIF(lcWare = 'All' AND llMultiWH,[cWareCode :H='Location',],[])+;
               [OnHand :H= 'On Hand', Usage :H='Usage']
  lcItem = laData[1]
  lcClr    = laData[2]
  IF lcWare = 'All'
    = ARIABROW([FOR Fabric+Color = lcItem+lcClr AND  !EMPTY(DYELOT)],;
                 lcTitle,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
  ELSE            
    = ARIABROW([FOR Fabric+Color+cWareCode= lcItem+lcClr+lcWare AND !EMPTY(DYELOT)],;
              lcTitle,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,"",'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
  ENDIF
ELSE
  =gfModalGen('QRM36030B36000','ALERT', 'dyelots|'+ALLTRIM(laData[1]) + '/'+ALLTRIM(laData[2]))
ENDIF
*B802766,1 [Start] Restore fabdye index
SELECT FABDYE
SET ORDER TO lcDyeIndx
*B802766,1 [End]
SELECT(lnAlias)
*!*************************************************************
*! Name      : lfvStyles
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : browse the Styles for the selected fabric/color
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION olfvStyles

lnAlias = SELECT(0)
IF EMPTY(laData[1]) OR EMPTY(laData[2]) OR llAllColrs
  RETURN
ENDIF  

IF USED(lcStyTemp)
  SELECT (lcStyTemp)
  ZAP
ENDIF
SELECT BOM
lcOldTag = TAG()
SET ORDER TO TAG BOMITEM
SEEK ('1'+SUBSTR(laData[1],1,7))
IF !FOUND()
  SEEK ('2'+SUBSTR(laData[1],1,7))
ENDIF
IF !EOF()
  SCAN WHILE ITEM=SUBSTR(laData[1],1,7)
    IF (ICLR <> SUBSTR(laData[2],1,6)) .AND. (ICLR <> '******')
      LOOP
    ENDIF

    IF ICLR='******'
      SELE STYLE
      SEEK BOM.STYLE+SUBSTR(laData[2],1,6)
      IF EOF()
        SELE BOM
        LOOP
      ENDIF
    ENDIF
    SELE BOM
    lcClr =SPACE(1)
    lcDesc=SPACE(1)
    DO CASE
      CASE SCLR='******' .AND. ICLR='******'
        lcClr=SUBSTR(laData[2],1,6)
        SELE STYLE
        IF SEEK (BOM.STYLE+lcClr)
          lcDesc=DESC
        ELSE
          lcDesc=SPACE(1)
        ENDIF
        SELE (lcStyTemp)
        IF !SEEK (BOM.STYLE+lcClr)
          APPEND BLANK
          REPLACE STYLE  WITH BOM.STYLE  ,;
                  SCLR   WITH lcClr       ,;
                  DESC   WITH lcDesc       ,;
                  UOM    WITH BOM.UOM    ,;
                  nbomtotqty WITH BOM.nbomtotqty
        ELSE
          REPLACE TOTQTY WITH TOTQTY+BOM.TOTQTY
        ENDIF

      CASE SCLR='******' .AND. ICLR<>'******'
        SELE STYLE
        SEEK (BOM.STYLE)
        SCAN WHILE STYLE.STYLE = BOM.STYLE
          SELE &lcStyTemp
          IF !SEEK (BOM.STYLE+STYLE.COLOR)
            APPEND BLANK
            REPLACE STYLE  WITH BOM.STYLE  ,;
                    SCLR   WITH STYLE.COLOR,;
                    DESC   WITH STYLE.DESC ,;
                    UOM    WITH BOM.UOM    ,;
                    nbomtotqty WITH BOM.nbomtotqty
          ELSE
            REPLACE TOTQTY WITH TOTQTY+BOM.TOTQTY
          ENDIF
          SELE STYLE
        ENDSCAN

      OTHERWISE
        lcClr=SCLR
        SELE STYLE
        IF SEEK (BOM.STYLE+lcClr)
          lcDesc=DESC
        ELSE
          lcDesc=SPACE(1)
        ENDIF
        SELE &lcStyTemp
        IF !SEEK (BOM.STYLE+lcClr)
          APPEND BLANK
          REPLACE STYLE  WITH BOM.STYLE  ,;
                  SCLR   WITH lcClr       ,;
                  DESC   WITH lcDesc       ,;
                  UOM    WITH BOM.UOM    ,;
                  nbomtotqty WITH BOM.nbomtotqty
        ELSE
          REPLACE TOTQTY WITH TOTQTY+BOM.TOTQTY
        ENDIF
    ENDCASE
    SELE BOM
  ENDSCAN
ENDIF
SET ORDER TO TAG (lcOldTag) IN Bom
SELECT(lnAlias)
*!*************************************************************
*! Name      : lfStyBrow
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : browse the Styles for the selected fabric/color
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfStyBrow
PRIVATE lnAlias

lnAlias = SELECT()
SELECT &lcStyTemp
GOTO TOP
lcSWinTtl   = 'Styles Using ' + ALLTRIM(laData[1]) + '/' + ALLTRIM(laData[2])
lcBrowfilds  = [cItmMask:19:H='Style',]+;
               [DESC:60:H='Description',]+;
               [UOM:H='UOM',]+;
               [nbomtotqty:H='Yield':P='9999.999']
BROWSE FIELDS &lcBrowfilds;
       LOCK 0            ;   
       SAVE              ;
       NOMENU            ;
       NODELETE          ;
       NOAPPEND          ;
       NOEDIT            ;
       NOWAIT            ;
       TITLE lcSWinTtl   ;
       WHEN lfwBrowse()  ;       
       WINDOW (lcWinC31) IN WINDOW (lcWinCh3)
SELECT(lnAlias)
=lfRefresh()
*!*************************************************************
*! Name      : lfvOrder
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : browse the orders for selected fabric/color
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvOrder

IF Fabric.Make
  IF !llMFIstall
    =gfModalGen('QRM36044B36000','ALERT')
  ELSE
    =lfMfgOrdBr()
  ENDIF
ELSE
  DO MAT211
ENDIF  
RETURN
*!*************************************************************
*! Name      : lfMfgOrdBr
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : browse the orders for domestic selected fabric/color
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfMfgOrdBr
PRIVATE lnAlias,lcBrFields


lnAlias = SELECT()
lcPrice = IIF(llCostPrv,",SUM(IIF(TRANCD='1',MMfgOrdD.nCost1+MMfgOrdD.nCost2+;
              MMfgOrdD.nCost3+MMfgOrdD.nCost4,0)) AS nPRICE","")
              
*! B604678,1 MHM 09/04/2001 get warehouse from detail file [start]
*SELECT MMfgOrdD.cMfgOrdNo,MMfgOrdH.Status,MMfgOrdH.cWareCode,MMfgOrdH.Complete,;
       SUM(IIF(MMfgOrdD.TranCd='1',MMfgOrdD.nMFGTotQty,0)) AS nORDERED,;
       SUM(IIF(MMfgOrdD.TranCd='2',MMfgOrdD.nMFGTotQty,0)) AS nRECEIVED,;
       SUM(IIF(MMfgOrdD.TranCd='3',MMfgOrdD.nMFGTotQty,0)) AS nDAMAGED,;
       SUM(IIF(MMfgOrdD.TranCd='4',MMfgOrdD.nMFGTotQty,0)) AS nCANCELLED &lcPrice;
  FROM MMfgOrdD,MMfgOrdH;
  INTO DBF (gcWorkDir+lcCursName);
 WHERE MMfgOrdD.cMfgOrdNo = MMfgOrdH.cMfgOrdNo ;
   AND MMfgOrdD.cFABRIC+MMfgOrdD.Color = SUBSTR(laData[1],1,7)+SUBSTR(laData[2],1,6);
GROUP BY MMfgOrdD.cMfgOrdNo


*C200254,1 ABD - Get the recored from the Material rework header & Detail also 
*C200254,1 ABD - Add new Field to hold type for the trans. type , it 
*C200254,1 ABD - Will show M.F.G Order','Rework order.  [Begin]
*SELECT MMfgOrdD.cMfgOrdNo,MMfgOrdH.Status,MMfgOrdD.cWareCode,MMfgOrdH.Complete,;
       SUM(IIF(MMfgOrdD.TranCd='1',MMfgOrdD.nMFGTotQty,0)) AS nORDERED,;
       SUM(IIF(MMfgOrdD.TranCd='2',MMfgOrdD.nMFGTotQty,0)) AS nRECEIVED,;
       SUM(IIF(MMfgOrdD.TranCd='3',MMfgOrdD.nMFGTotQty,0)) AS nDAMAGED,;
       SUM(IIF(MMfgOrdD.TranCd='4',MMfgOrdD.nMFGTotQty,0)) AS nCANCELLED &lcPrice;
  FROM MMfgOrdD,MMfgOrdH;
  INTO DBF (gcWorkDir+lcCursName);
 WHERE MMfgOrdD.cMfgOrdNo = MMfgOrdH.cMfgOrdNo ;
   AND MMfgOrdD.cFABRIC+MMfgOrdD.Color = SUBSTR(laData[1],1,7)+SUBSTR(laData[2],1,6);
GROUP BY MMfgOrdD.cMfgOrdNo

*B604856,1 AMH Add variable to group data by locaiton [Start]
lcGrpByWar = IIF(lcWare = 'All',"",",MMFGORDD.CWARECODE")
*SELECT 'M' As cTrnType ,MMfgOrdD.cMfgOrdNo,MMfgOrdH.Status,MMfgOrdD.cWareCode,MMfgOrdH.Complete,;
       SUM(IIF(MMfgOrdD.TranCd='1',MMfgOrdD.nMFGTotQty,0)) AS nORDERED,;
       SUM(IIF(MMfgOrdD.TranCd='2',MMfgOrdD.nMFGTotQty,0)) AS nRECEIVED,;
       SUM(IIF(MMfgOrdD.TranCd='3',MMfgOrdD.nMFGTotQty,0)) AS nDAMAGED,;
       SUM(IIF(MMfgOrdD.TranCd='4',MMfgOrdD.nMFGTotQty,0)) AS nCANCELLED &lcPrice;
  FROM MMfgOrdD,MMfgOrdH;
  INTO DBF (gcWorkDir+lcCursName);
 WHERE MMfgOrdD.cMfgOrdNo = MMfgOrdH.cMfgOrdNo ;
   AND MMfgOrdD.cFABRIC+MMfgOrdD.Color = SUBSTR(laData[1],1,7)+SUBSTR(laData[2],1,6);
GROUP BY MMfgOrdD.cMfgOrdNo
SELECT 'M' As cTrnType ,MMfgOrdD.cMfgOrdNo,MMfgOrdH.Status,MMfgOrdD.cWareCode,MMfgOrdH.Complete,;
       SUM(IIF(MMfgOrdD.TranCd='1',MMfgOrdD.nMFGTotQty,0)) AS nORDERED,;
       SUM(IIF(MMfgOrdD.TranCd='2',MMfgOrdD.nMFGTotQty,0)) AS nRECEIVED,;
       SUM(IIF(MMfgOrdD.TranCd='3',MMfgOrdD.nMFGTotQty,0)) AS nDAMAGED,;
       SUM(IIF(MMfgOrdD.TranCd='4',MMfgOrdD.nMFGTotQty,0)) AS nCANCELLED &lcPrice;
  FROM MMfgOrdD,MMfgOrdH;
  INTO DBF (gcWorkDir+lcCursName);
 WHERE MMfgOrdD.cMfgOrdNo = MMfgOrdH.cMfgOrdNo ;
   AND MMfgOrdD.cFABRIC+MMfgOrdD.Color = SUBSTR(laData[1],1,7)+SUBSTR(laData[2],1,6);
GROUP BY MMfgOrdD.cMfgOrdNo &lcGrpByWar.
*B604856,1 AMH [End]

lcRwPrice = IIF(llCostPrv,",SUM(IIF(TRANCD='2',MarwLin.nCost1+MarwLin.nCost2+;
              MarwLin.nCost3+MarwLin.nCost4,0)) AS nPRICE","")

SELECT MarwLin.cMfgrwNum as 'cMfgOrdNo',MarwHdr.Status,MarwLin.cWareCode,MarwHdr.Complete,;
       SUM(IIF(MarwLin.TranCd="2",MarwLin.nMFGTotQty,0)) AS nORDERED,;
       SUM(IIF(MarwLin.TranCd="3",MarwLin.nMFGTotQty,0)) AS nRECEIVED,;
       SUM(IIF(MarwLin.TranCd="6",MarwLin.nMFGTotQty,0)) AS nDAMAGED ,;
       SUM(IIF(MarwLin.TranCd="4",MarwLin.nMFGTotQty,0)) AS nCANCELLED &lcRwPrice;
FROM MarwLin,MarwHdr;
WHERE MarwLin.cMfgrwNum = MarwHdr.cMfgrwNum;
AND MarwLin.cFABRIC+MarwLin.Color = SUBSTR(laData[1],1,7)+SUBSTR(laData[2],1,6);
AND MarwLin.TranCd# "1";
GROUP BY MarwLin.cMfgrwNum;
INTO DBF (gcWorkDir+lcTmpCurs)

SELECT (lcTmpCurs)
SCAN
  SCATTER MEMVAR MEMO
  INSERT INTO (lcCursName) FROM MEMVAR
ENDSCAN  
SELECT (lcCursName)
*C200254,1 ABD - [End]


*! B604678,1 MHM 09/04/2001 [End]

*C200254,1 ABD - check on the number of the recored. [Begin]
*IF _TALLY > 0
IF _TALLY > 0 .OR. RECCOUNT() > 0
  *C200254,1 ABD - [End]
  *E301234,1 [Start] Call MMO Screen
  
  *C200254,1 ABD - Change the titel. [Begin]
  *lcFile_Ttl  = 'Material Manufacturing Order'
   lcFile_Ttl  = 'Item M.F.G (s) / Rework'
  *C200254,1 ABD - [End]
  
  *E301234,1 [End]

  *C200254,1 ABD - Add new Field to hold type for the trans. type , it 
  *C200254,1 ABD - Will show M.F.G Order','Rework order.  [Begin]
  *lcBrFields = [cMfgOrdNo:H='Order#' :8,]+;
               [Status:H='S':2 ,]+;
               [cWareCode:H='Warehous' :11 ,]+;
               [Complete:H='Complete' :8 ,]+;
               IIF(llCostPrv, [nPRICE:H='Tot Cost':P='99999.99' :8,],[])+;
               [nORDERED:H='Ordered':P='999999.99' :9,]+;
               [nOPEN=IIF( (STATUS='X').OR.]+;
                      [ ((nORDERED-nRECEIVED-nDAMAGED-nCANCELLED)<0) ,]+;
                      [ 0, nORDERED-nRECEIVED-nDAMAGED-nCANCELLED)]+;
                      [:P='999999.99':H='Open' :9 ,]+;
               [nRECEIVED:H='Received':P='999999.99' :9,]+;
               [nCANCELLED:H='Canceled':P='99999.99' :8,]+;
               [nDAMAGED:H='Damaged':P='99999.99' :8 ]

  lcBrFields = [dummi=IIF(cTrnType = 'M','M.F.G Order','Rework') :H='Trans. Type',]+;
               [cMfgOrdNo:H='Order#' :8,]+;
               [Status:H='S':2 ,]+;
               [cWareCode:H='Warehous' :11 ,]+;
               [Complete:H='Complete' :8 ,]+;
               IIF(llCostPrv, [nPRICE:H='Tot Cost':P='99999.99' :8,],[])+;
               [nORDERED:H='Ordered':P='999999.99' :9,]+;
               [nOPEN=IIF( (STATUS='X').OR.]+;
                      [ ((nORDERED-nRECEIVED-nDAMAGED-nCANCELLED)<0) ,]+;
                      [ 0, nORDERED-nRECEIVED-nDAMAGED-nCANCELLED)]+;
                      [:P='999999.99':H='Open' :9 ,]+;
               [nRECEIVED:H='Received':P='999999.99' :9,]+;
               [nCANCELLED:H='Canceled':P='99999.99' :8,]+;
               [nDAMAGED:H='Damaged':P='99999.99' :8 ]
               
  *C200254,1 ABD - [End]


  SELECT (lcCursName)
  

  *C200254,1 ABD - try to call the rework Screen in case we select rework order. [Begin]
  *DIMENSION laMMO[1]  
  *STORE '' TO lcMMO
  DIMENSION laMMO[2]
  STORE '' TO lcMMO , lcMMOType
  *C200254,1 ABD - [End]
  
  IF lcWare = 'All'  
    *C200254,1 ABD - Return the No. And the Type to know if we will call rework screen or MMfg Order .[Begin]
    *llWasSel = AriaBrow('', lcFile_Ttl, gnBrHSRow1, gnBrHSCol1,gnBrHSRow2, gnBrHSCol2,'','','cmfgordno','laMMO')
    llWasSel = AriaBrow('', lcFile_Ttl, gnBrHSRow1, gnBrHSCol1,gnBrHSRow2, gnBrHSCol2,'','','cmfgordno,cTrnType','laMMO')
    *C200254,1 ABD - [End]
    IF llWasSel
      lcMMO   = laMMO[1]
      *C200254,1 ABD - Get the type. [Begin]
      lcMMOType   = laMMO[2]
      *C200254,1 ABD - [End]
    ENDIF
    *E301234,1 {Start} Call Material Manufacturin order Screen
    IF !EMPTY(lcMMO)
      *C200254,1 ABD - Try to know if we will call rework screen or MMfg Order .[Begin]
      IF lcMMOType   = 'M'
        *C200254,1 ABD - [End]
        lcParam  = "'" + lcMMO + "'"
        DO gpDoProg WITH 'AWRMAMFORD',.F.,'MA',lcParam

        *C200254,1 ABD - Else For IF statment. [Begin]
      ELSE
        *-- Call the rework screen.
        lcParam  = "'" + lcMMO + "'"
        DO gpDoProg WITH 'AWRMAMFRWK',.F.,'MA',lcParam
      ENDIF
      *C200254,1 ABD - [End]
    ENDIF  
    
    
    *E301234,1 {End}
  ELSE
    llWasSel = AriaBrow([FOR cWareCode= PADR(lcWare,6)], lcFile_Ttl, gnBrHSRow1, gnBrHSCol1,gnBrHSRow2, gnBrHSCol2,'','','cmfgordno','laMMO')
    IF llWasSel
      lcMMO   = laMMO[1]
    ENDIF
    IF !EMPTY(lcMMO)
      lcParam  = "'" + lcMMO + "'"
      DO gpDoProg WITH 'AWRMAMFORD',.F.,'MA',lcParam
    ENDIF
  ENDIF
ELSE
  =gfModalGen('QRM36030B36000','ALERT', 'Manufacturing Orders|'+ALLTRIM(laData[1]) + '/'+ALLTRIM(laData[2]))
ENDIF

*--RERE

SELECT (lnAlias)

*!*************************************************************
*! Name      : MAT211
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : browse the orders for purchassed selected fabric/color
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
PROCEDURE MAT211
PRIVATE lnAlias,lcBrFields,lcPO,laPO,llWasSel,lcType

lnAlias    = SELECT()
lcPO       = SPACE(06)
lcType     = SPACE(01)
*-- HDM B602651,1 Icorrect price calculation[start]
*lcPrice    = IIF(llCostPrv, "SUM(IIF(TRANCD='1',POFLN.nCost1,0)) AS nPRICE","")
*--HDM B802220,1[Start] Enhance the on order data collecting
*  SELECT POFHDR.cMatType ,POFLN.POMAT,POFLN.nCost1 as nPrice, POFHDR.STATUS,POFHDR.cWareCode,POFHDR.VENDOR, POFHDR.COMPLETE,;
         SUM(IIF(TRANCD='1',nFabTOTQTY,0.000)) AS nORDERED,;
         SUM(IIF(TRANCD='2',nFabTOTQTY,0.000)) AS nRECEIVED,;
         SUM(IIF(TRANCD='3',nFabTOTQTY,0.000)) AS nDAMAGED,;
         SUM(IIF(TRANCD='4',nFabTOTQTY,0.000)) AS nCANCELLED ;
    FROM POFLN,POFHDR;
    INTO DBF (gcWorkDir+lcCursName);
   WHERE POFLN.POMAT = POFHDR.POMAT;
     AND (POFLN.cMatType = 'P' OR POFLN.cMatType = 'R') ;
     AND IIF(lcWare <> 'All',POFHDR.cWareCode = lcWare,.T.);
     AND POFLN.FABRIC+POFLN.COLOR = SUBSTR(laData[1],1,7)+SUBSTR(laData[2],1,6);
GROUP BY POFLN.POMAT

  SELECT POFLN.cMatType ,POFLN.POMAT,POFLN.nCost1 as nPrice,;
         SUM(IIF(TRANCD='1',nFabTOTQTY,0.000)) AS nORDERED,;
         SUM(IIF(TRANCD='2',nFabTOTQTY,0.000)) AS nRECEIVED,;
         SUM(IIF(TRANCD='3',nFabTOTQTY,0.000)) AS nDAMAGED,;
         SUM(IIF(TRANCD='4',nFabTOTQTY,0.000)) AS nCANCELLED ;
    FROM POFLN;
    INTO DBF (gcWorkDir+lcCursName);
   WHERE (POFLN.cMatType = 'P' OR POFLN.cMatType = 'R') ;
     AND POFLN.FABRIC+POFLN.COLOR = SUBSTR(laData[1],1,7)+SUBSTR(laData[2],1,6);
GROUP BY POFLN.POMAT

*-- HDM B602651,1 Icorrect price calculation[End]

WAIT CLEAR

IF _TALLY > 0
*  POFHDR.STATUS,POFHDR.cWareCode,POFHDR.VENDOR, POFHDR.COMPLETE
  
    *C200254,1 ABD - Change the titel. [Begin]
    *lcFile_Ttl  = 'M.F.G. Orders'
     lcFile_Ttl  = 'Item PO (s) / Returns'
    *C200254,1 ABD - [End]

    lcBrFields  = [dummi=IIF(cMatType = 'P','Pur.Ord','Return') :H='Trans. Type',]+;
                  [POMAT:8:H='P/O #',]+;
                  [POFHDR.STATUS :2:H='S',]+;
                  [POFHDR.cWareCode:11:H='Location',]+;                  
                  [POFHDR.VENDOR:11:H='Vendor',]+;
                  [POFHDR.COMPLETE:8:H='Complete',]+;
                  IIF(llCostPrv, [nPRICE:H='Price':P='99999.999',],[])+;
                  [nORDERED:H='Ordered':P='999999.999',]+;
                  [nRECEIVED:H='Received':P='999999.999',]+;
                  [nOPEN=IIF( (POFHDR.STATUS='X').OR.]+;
                  [ ((nORDERED-nRECEIVED-nDAMAGED-nCANCELLED)<0) ,]+;
                  [ 0, nORDERED-nRECEIVED-nDAMAGED-nCANCELLED)]+;
                  [:P='999999.999':H='Open']
  DECLARE laPO[2]
  SELECT (lcCursName)
  SET RELA TO cMatType+POMAT INTO POFHDR ADDI
  IF lcWare = 'All'  
*    llWasSel=AriaBrow('', lcFile_Ttl, gnBrHSRow1, gnBrHSCol1,;
              gnBrHSRow2, gnBrHSCol2,"","","POMAT,cMatType","laPO")
  ELSE
    SET FILTER TO POFHDR.CWareCode= PADR(lcWare,6) 
*   llWasSel=AriaBrow([FOR POFHDR.CWareCode= PADR(lcWare,6)], lcFile_Ttl, gnBrHSRow1, gnBrHSCol1,;
              gnBrHSRow2, gnBrHSCol2,'','','POMAT,cMatType','laPO')
  ENDIF
  GO TOP
  IF !EOF()
    llWasSel=AriaBrow('', lcFile_Ttl, gnBrHSRow1, gnBrHSCol1,;
              gnBrHSRow2, gnBrHSCol2,"","","POMAT,cMatType","laPO")
    IF llWasSel
      lcPO   = laPO[1]
      lcCall = laPO[2]
    ENDIF

    IF !EMPTY(lcPO)
      lcParam  = "'"+lcCall+"','"+lcPO + "'"
      DO gpDoProg WITH 'AWRMAPRCAM',.F.,'MA',lcParam
    ENDIF
  ELSE
    =gfModalGen('QRM36030B36000','ALERT', 'purchase orders|'+ALLTRIM(laData[1]) + '/'+ALLTRIM(laData[2]))  
  ENDIF
  SELECT (lcCursName)
  SET FILTER TO
  SET RELA TO
ELSE
  =gfModalGen('QRM36030B36000','ALERT', 'purchase orders|'+ALLTRIM(laData[1]) + '/'+ALLTRIM(laData[2]))
ENDIF
*--HDM B802220,1[end]
SELECT (lnAlias)
*!*************************************************************
*! Name      : lfvRolls
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : browse the rolls selected fabric/color
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvRolls
PRIVATE lnAlias,lcBrFields

lnAlias = SELECT()
SELECT Rolls
SET ORDER TO TAG Rollitem 

LOCATE FOR cRollItem+Color+IIF(lcWare <> 'All',cWareCode,IIF(!llMultiWH,WAREHOUS.cWareCode,''))=laData[1]+laData[2]+IIF(lcWare <> 'All',lcWare,IIF(!llMultiWH,WAREHOUS.cWareCode,'')) AND (nQtyBal > 0 AND TRANCD = "1")
IF FOUND()
  lcTitle= 'Rolls for ' + ALLTRIM(laData[1]) + '/' + ALLTRIM(laData[2]) 
  lcBrFields = [cRollId :H= 'Roll ID',] +;
               IIF(llDyeLot  AND FABRIC.cDye_Flg = 'Y', [Dyelot :H='Dyelot',]  ,[])  +; 
               IIF(lcWare = 'All' AND llMultiWH,[cWareCode :H='Location',],[])+;
               [NQtyBal :H= 'On Hand']
  lcItem = laData[1]
  lcClr    = laData[2]
  IF lcWare = 'All'
    = ARIABROW([FOR cRollItem+Color= lcItem  + lcClr   AND NQtyBal > 0 AND TRANCD = "1"],;
                lcTitle,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
  ELSE            
    = ARIABROW([FOR cRollItem+Color+cWareCode= lcItem  + lcClr +PADR(lcWare,6) AND NQtyBal > 0 AND TRANCD = "1"],;
            lcTitle,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,"",'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
            
  ENDIF
ELSE
  =gfModalGen('QRM36030B36000','ALERT', 'rolls|'+ALLTRIM(laData[1]) + '/'+ALLTRIM(laData[2]))
ENDIF

SELECT(lnAlias)
*!*************************************************************
*! Name      : lfvLots
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : browse the Receivings selected fabric/color
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvLots
PRIVATE lnAlias,lcRollTag,llBrowRoll

lnAlias = SELECT()
llBrowRoll = llTrkRolls AND FABRIC.lTrkRolls AND llTranRoll
lcTitle = 'Receivings for ' + ALLTRIM(laData[1]) + '/' + ALLTRIM(laData[2]) 
SELECT MATINVJL
LOCATE FOR cFabric+cColor+IIF(lcWare <> 'All',cWareCode,'' ) = laData[1]+laData[2]+IIF(lcWare <> 'All',lcWare,'' )
IF FOUND()
  IF llBrowRoll
    lcRollTag = ORDER('ROLLS')
    SET ORDER TO TAG Session IN ROLLS
    SET RELATION TO MATINVJL.ctrn_seq+MATINVJL.cFabric+MATINVJL.cColor INTO Rolls ADDITIVE
    SET SKIP TO Rolls
  ENDIF
  *B120471,1 ABD - Browse only the received Quantity in case show the rolls transaction for the physically inventory only.  [Begin]
  *lcBrFields = [cTran :H= 'Tran',] +;
               IIF(llDyeLot  AND FABRIC.cDye_Flg = 'Y', [cDyelot :H='Dyelot',]  ,[])  +; 
               IIF(lcWare = 'All' AND llMultiWH,[cWareCode :H='Location',],[])+;
               IIF(llBrowRoll,[ROLLS.CROLLID :H= 'Roll_ID',],[])+;
               [cRSession :H='RSession',] +; 
               [cISession :H='ISession',] +; 
               [nUnitCost :H='UnitCost',]+;
               [nReceived :H='Balance',]+;
               [REFERENCE :H='Reference']
  lcBrFields = [cTran :H= 'Tran',] +;
               IIF(llDyeLot  AND FABRIC.cDye_Flg = 'Y', [cDyelot :H='Dyelot',]  ,[])  +; 
               IIF(lcWare = 'All' AND llMultiWH,[cWareCode :H='Location',],[])+;
               IIF(llBrowRoll,[ROLLS.CROLLID :H= 'Roll_ID',],[])+;
               [cRSession :H='RSession',] +; 
               [cISession :H='ISession',] +; 
               [nUnitCost :H='UnitCost',]+;
               [lnBalQty=IIF(llBrowRoll,Rolls.nQtyBAL,nReceived) :H='Balance',]+;               
               [REFERENCE :H='Reference']
  *B120471,1 ABD - [End]
  lcItem    = laData[1]
  lcClr     = laData[2]
  IF lcWare = 'All'
    = ARIABROW([FOR cFabric+cColor= lcItem+lcClr AND nReceived > 0 AND IIF(llBrowRoll,ROLLS.TRANCD $'13',.T.)],;
               lcTitle,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,' ','Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
  ELSE             
    = ARIABROW([FOR cFabric+cColor+cWareCode= lcItem+lcClr+PADR(lcWare,6) AND nReceived> 0 AND IIF(llBrowRoll,ROLLS.TRANCD $'13',.T.)],;
               lcTitle,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,"",'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
  ENDIF
  IF llBrowRoll
    SELECT MATINVJL
    SET RELATION TO
    SET ORDER TO TAG lcRollTag IN ROLLS
  ENDIF
ELSE
  *B803335,1 (Begin) Correct spelling of word 'receiving.'
  *=gfModalGen('QRM36030B36000','ALERT', 'recievings|'+ALLTRIM(laData[1]) + '/'+ALLTRIM(laData[2]))
  =gfModalGen('QRM36030B36000','ALERT', 'receivings|'+ALLTRIM(laData[1]) + '/'+ALLTRIM(laData[2]))
  *B803335,1 (End)
ENDIF  
SELECT(lnAlias)
*************************************************************
FUNCTION lfGetTran
PARAMETER lcField, lcExpr

RETURN LOOKUP(MATINVJL.&lcField,lcExpr,MATINVJL.cfabric,'MATINVJL')
*!*************************************************************
*! Name      : lfvCtTkt
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : browse the Cuttikkets for  selected fabric/color
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Modifications :
*!               HDM Fixing Bugs
*!               1) Usage & W.I.P. (Quantities Folder)
*!                  USAGE SHOULD BE CALCULATED UPON Status (Open|Closed|Completed|Quantities|Actualized)
*!                  WIP   SHOULD BE CALCULATED UPON Status (Open|Actualized)
*!                  Also we have to include POs beside Cutting Tickets
*!*************************************************************
FUNCTION lfvCtTkt
PARAMETERS lcStatus
PRIVATE lnAlias, lnCurTag1, lnCurTag2, lcCurFab, lcCurColor,lcBrFields,;
        laCUTTKT,lcCUTTKT,lcBrStat
*--     Last Var. Added by HDM to be used for STATUS$'OCSA' OR STATUS$'OA'

lnAlias    = SELECT()
lcCUTTKT   = SPACE(06)
SELECT BOM
lnCurTag1 = TAG()
SET ORDER TO BOM

*B605940,1 KHM 05/27/2002 (Begin) Checking the MF installed
IF llMFIstall
*B605940,1 KHM 05/27/2002 (End)
 
  SELECT CUTTKTL
  lnCurTag2 = TAG()
  SET ORDER TO CUTTKTL

*B605940,1 KHM 05/27/2002 (Begin) 
ENDIF
*B605940,1 KHM 05/27/2002 (End)

*--HDM Fixing Bugs[start]
*SELECT DISTINCT CUTTKT FROM CUTTKTL,BOM;
*       WHERE INLIST(TYP+ITEM+ICLR+CITMMAJOR+CITMMASK,;
*                    laFabTyp[1]+PADR(laData[1],19)+laData[2],;
*                    laFabTyp[2]+PADR(laData[1],19)+laData[2],;
*                    laFabTyp[1]+PADR(laData[1],19)+'**',;
*                    laFabTyp[2]+PADR(laData[1],19)+'**');
*            .AND. LIKE(STRTRAN(BOM.CITMMASK,'*','?'), CUTTKTL.STYLE) ;
*       INTO DBF (gcWorkDir+lcCursName)


*--E301077,81 (Start)
*IF !USED('POSHDR')
*  USE (gcDataDir+'POSHDR') IN 0 SHARED
*ENDIF
*IF !USED('POSLN')
*    USE (gcDataDir+'POSLN') IN 0 SHARED
*ENDIF
*IF !USED('BOMCOST')
*    USE (gcDataDir+'BOMCOST') IN 0 SHARED
*ENDIF
*SELECT BOMCOST
*SET ORDER TO TAG BOMCOST
=gfOpenFile(gcDataDir+'POSHDR','','SH')  
=gfOpenFile(gcDataDir+'POSLN','','SH')  
=gfOpenFile(gcDataDir+'BOMCOST','','SH')  
*--E301077,81 (End)

SELECT DISTINCT CTKTNO,CIMTYP FROM BOMCOST WHERE;
                       BOMCOST.ITEM + BOMCOST.ICLR = PADR(laData[1],19)+laData[2] .OR.;
                       BOMCOST.ITEM + BOMCOST.ICLR = PADR(laData[1],19)+'******' ;
                       INTO DBF (gcWorkDir+lcCursName)

IF _TALLY > 0
  lcCurFab   = laData[1]
  lcCurColor = laData[2]
  DECLARE laCUTTKT[1]
  SELECT CTKTBOM
  lnCurTag1  = TAG()
  SET ORDER TO TAG CTKTYP

  SELECT (lcCursName)
  INDEX ON CTKTNO TAG CTKTNO ADDITIVE
  *-- HDM B602355,1 Adjust the relation to point to the fabric only not the whole cut ticket[Start]
  *  SET RELATION TO &lcCursName..cimtyp + &lcCursName..ctktno INTO Ctktbom ADDITIVE
  *-- HDM B602355,1 Adjust the relation to point to the fabric only not the whole cut ticket[End]  

  SET RELATION TO &lcCursName..cimtyp + &lcCursName..ctktno + PADR(laData[1],19) + laData[2] INTO Ctktbom ADDITIVE
  
  *B605940,1 KHM 05/27/2002 (Begin) Adding the Checking of MF installed
  IF llMFIstall
  *B605940,1 KHM 05/27/2002 (End)
    SET RELATION TO &lcCursName..CTKTNO INTO Cuttkth ADDITIVE
  ENDIF  

  SELECT POSHDR
  SET ORDER TO TAG POSHDR

  SELECT (lcCursName)
  SET RELATION TO 'P' + &lcCursName..CTKTNO INTO Poshdr ADDITIVE
  
  SELECT POSLN
  SET ORDER TO TAG POSLN

  SELECT POSHDR
  SET RELATION TO "P" + Poshdr.PO INTO Posln ADDITIVE
  
  *B605940,1 KHM 05/27/2002 (Begin) Adding the Checking of MF installed
  IF llMFIstall
  *B605940,1 KHM 05/27/2002 (End)
    SELECT CUTTKTH
    SET ORDER TO TAG CUTTKTH
    SET RELATION TO CUTTKTH.CUTTKT INTO CUTTKTL ADDITIVE
  ENDIF  

  *--
  SELECT (lcCursName)
  
  lcFile_Ttl = IIF(lcStatus = 'O','Open C/T & P/O','Closed C/T & P/O')
  lcType=' '
  lcBrFields = [lcType=IIF(&lcCursName..CIMTYP='M','C/T','P/O'):H='Type',]+;
               [&lcCursName..CTKTNO:H='Trans. #',]+;
               [lcPCStatus =IIF(&lcCursName..CIMTYP = 'I' ,POSHDR.STATUS , CUTTKTH.STATUS)    :H = 'Status',]+;
               [lcEntered  =IIF(&lcCursName..CIMTYP = 'I' ,POSHDR.ENTERED , CUTTKTH.ENTERED)  :H = 'Entered',]+;
               [lcComplete =IIF(&lcCursName..CIMTYP = 'I' ,POSHDR.COMPLETE , CUTTKTH.COMPLETE):H = 'Complete',]+;
               [CTKTBOM.UOM,]+;
               IIF(llCostPrv,;
               [CTKTBOM.UntCost:H='Unit Cost',],[])+;
               [CTKTBOM.Issue_Qty:H='Issued Qty.',]+;
               [CTKTBOM.Used_Qty:H='Used Qty.']

*--HDM Fixing Bug in status Values[start]
*  llWasSel=AriaBrow([FOR CUTTKTH.Status = lcStatus], lcFile_Ttl, gnBrHSRow1, gnBrHSCol1,;
*            gnBrHSRow2, gnBrHSCol2,"","","CTKTNO","laCUTTKT")

lcBrStat = IIF(lcStatus = 'O','OA','OCSA')
llWasSel=AriaBrow([FOR IIF(&lcCursName..CIMTYP='M',CUTTKTH.Status $(lcBrStat),POSHDR.Status $(lcBrStat))], lcFile_Ttl, gnBrHSRow1, gnBrHSCol1,;
            gnBrHSRow2, gnBrHSCol2,"","","CTKTNO,CIMTYP","laCUTTKT")

  IF llWasSel
    lcCUTTKT = laCUTTKT[1]
  ENDIF
  IF !EMPTY(lcCUTTKT)
   IF laCUTTKT[2]='I'
     lcPOInq  =+"'"+ 'P'+"','"+lcCUTTKT+"'"
     DO gpDoProg WITH 'AWRPOSTYLE',.F.,'PO',lcPOInq
   ELSE
     IF laCUTTKT[2]='M'
       lcPOInq  =[']+lcCUTTKT+"'"
       DO gpDoProg WITH 'AWRMFCUTKT',.F.,'MF',lcPOInq
     ENDIF
   ENDIF
  ENDIF
  
  SELECT (lcCursName)
  
  SET RELATION OFF INTO POSHDR
  *B605940,1 KHM 05/27/2002 (Begin) Adding the checking of MF installed
  IF llMFIstall
  *B605940,1 KHM 05/27/2002 (End)
    SET RELATION OFF INTO CUTTKTH
  
    SELECT CUTTKTH
    SET RELATION OFF INTO CUTTKTL
  ENDIF
  
  SELECT POSHDR
  SET RELATION OFF INTO POSLN
  
  SELECT (lcCursName)
  SET RELATION OFF INTO POSLN
  
  *-- Restore 
  laData[1]    = lcCurFab    
  laData[2]    = lcCurColor  
  SET ORDER TO TAG FABRIC IN FABRIC
  =SEEK(SUBSTR(laData[1],1,7)+SUBSTR(laData[2],1,6), 'FABRIC')
ELSE
  =gfModalGen('QRM36030B36000','ALERT', 'cutting tickets|'+ALLTRIM(laData[1]) + '/'+ALLTRIM(laData[2]))
ENDIF

SELECT (lnAlias)
*!*************************************************************
*! Name      : lfvWare1
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : get the selected warehouse for inquire
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvWare1

lnAlias   = SELECT()
IF lcWare <> 'All'
  SELECT FABDYE
  lnOldOrd  = TAG()
  SET ORDER TO TAG FABDYEW
  =SEEK(lcWare+laData[1]+laData[2])
  lnOnHand = FABDYE.ONHAND
  lnUsage  = FABDYE.USAGE
  lnOnOrder= FABDYE.ONORDER
  lnWip    = FABDYE.NMATWIP
  *E301797,1 (Begin) Calculate.
  lnInTran = FABDYE.Matotintr
  *E301797,1 (End)
  
  *E301911,1 ABD - assign Fabric sell order & ship to Variabls. [Begin]
  lnSelOrder = FABDYE.nsellorder
  lnSelInvce = FABDYE.nsellship
  *E301911,1 ABD - [End]
  
  SET ORDER TO TAG (lnOldOrd)
ELSE
  =lfInit(.T.)
  =lfwWare1()
ENDIF  
=lfRefresh()
*C102684,1 (Begin) Calculate On order for Bratex.
IF ASCAN(laEvntTrig , PADR('CALCORD',10)) <> 0
  =gfDoTriger('MAMATRL',PADR('CALCORD',10))
ENDIF
*C102684,1 (End)

SELECT(lnAlias)
*!*************************************************************
*! Name      : lfBColor
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : get colors for the selected fabric
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfBColor
PRIVATE lnAlias

lnAlias    = SELECT()

IF !EMPTY(laData[1])
  SELECT *,SPACE(15) AS CDATA,;
         SPACE(30) AS CLRLNAME,"S" AS cStatus,;
         SPACE(1) AS cMarker;
    FROM FABRIC;
    WHERE FABRIC.FABRIC = SUBSTR(laData[1],1,7);
    ORDER BY FABRIC.COLOR ;
    INTO DBF (gcWorkDir+lcTempClr)

    IF USED(lcTempClr)
      SELECT (lcTempClr)
      USE
    ENDIF
    USE (gcWorkDir+lcTempClr) IN 0 EXCLUSIVE
    SELECT (lcTempClr)
    INDEX ON  COLOR TAG COLOR      
  SELECT (lcTempClr)
  lcName = SPACE(50)
  laColor[1,1] = 'CLRLNAME'
  laColor[1,2] = 'lcName'
  SCAN 
    =gfRltFld(&lcTempClr..COLOR, @laColor, 'COLOR')
    REPLACE cData    WITH gfCodDes(&lcTempClr..COLOR, 'COLOR'),;
            ClrLName WITH lcName 
    lcName = SPACE(50)        
  ENDSCAN
ENDIF  
SELECT(lnAlias)
*!*************************************************************
*! Name      : lfActClr
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Browse colors for the selected fabric
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfActClr
PRIVATE lnAlias

lnAlias  = SELECT()
lcBrowfields = "cMarker  :H=' ':W=.F.,"+;
             "COLOR    :H ='Color':R:6,"           +;
             "cData    :H='Description'     :R:15,"+;
             "ClrLName :H='Long Description':R:30" 
SELECT (lcTempClr)
GOTO TOP
BROWSE FIELDS &lcBrowfields;
       LOCK 0   ;
       NOEDIT   ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       WHEN lfChMarker() ;
       TITLE lcClrTtl    ;
       WINDOW (lcWinCC1) IN WINDOW (lcWinCh4)
lnAlias = SELECT()
*!*************************************************************
*! Name      : lfChMarker
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : mark the selected record in the browse
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfChMarker

IF !EOF()
  lnNewRec = RECNO()
  REPLACE ALL cMarker WITH lcUnMark
  GO lnNewRec
  REPLACE cMarker WITH lcMark
ENDIF  
*!*************************************************************
*! Name      : lpDelScr
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : delete a selected fabric or selected fabric/color
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
PROCEDURE lpDelScr
PRIVATE lnAlias

lnAlias = SELECT()
lnRecNo = RECNO()
FOR lnType = 1 TO 5 
  Z         = STR(lnType,1)
  lcMtype&Z = gfGetMemVar('M_cMtype&Z')
  lcItype&Z = gfGetMemVar('M_cItype&Z')
ENDFOR
SELECT BOM
SET ORDER TO TAG BOMITEM 
IF !llAllColrs
  llDel = lfDValid()
  *-- HDM B603038,1 [Start] If not deleted unlock the record
  IF !LLDEL
    =gfObj_Lock(.F.)
  ENDIF
  *-- HDM B603038,1 [End]

ELSE
  DIMENSION laSource[1], laTarget[1]
  STORE ' ' TO laSource, laTarget
  SELECT FABRIC.COLOR;
    FROM FABRIC;
    WHERE FABRIC.FABRIC = SUBSTR(laData[1],1,7);
    ORDER BY FABRIC.COLOR ;
    INTO ARRAY laSource            
  *B603768,1 (Begin) When Move All, Cycle over all colors. 
  *=gfMover(@laSource,@laTarget," Item Colors ",.T.,'lfDValid')  
  llDispMess = .T.
  =gfMover(@laSource,@laTarget," Item Colors ",.T.,'lfDValid',.T.)  
  *B603768,1 (End)
ENDIF
IF llAllColrs OR llDel
  =lfActDel()
ENDIF
IF llAllColrs
  llchk = .F.
  FOR i = 1 TO ALEN(latarget)
	IF !EMPTY(latarget[I])
      llchk = .T.
	  EXIT
	ENDIF
  ENDFOR
  IF llchk
    laScrMode[2] = .F.
    laScrMode[1] = .T.    
  ELSE  
    SELECT(lnAlias)
    GOTO lnRecno 
  ENDIF
ELSE
  laScrMode[2] = .F.
  laScrMode[1] = .T.    
  =lfInit(.F.)
  _CUROBJ = OBJNUM(laData[1])
ENDIF
SELECT(lnAlias)
SHOW GETS
*!*************************************************************
*! Name      : lfDValid
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : validation for the deletion
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfDValid
PARAMETERS lnOption

PRIVATE llDelYes
IF llAllColrs AND (lnOption=3 OR lnOption=4)
  RETURN
ENDIF
*--HDM B602606,1 Alias Style Not Found[Start]
IF !USED('STYLE')
  llOpnStyle = gfOpenFile(gcDataDir+'STYLE','Style','SH')
ENDIF
*--HDM B602606,1 Alias Style Not Found[End]

llDelYes = .T.
IF llAllColrs
  lcMessage = ALLTRIM(laData[1]) + "/" + ALLTRIM(laSource[lsSource])
ELSE
  lcMessage = ALLTRIM(laData[1]) + "/" + ALLTRIM(laData[2])
ENDIF
SELECT FABRIC
lcSeekKey=IIF(llAllColrs AND lnOption=2,laData[1],IIF(!llAllColrs,laData[1]+laData[2],laData[1]+laSource[lsSource]))
=SEEK(lcSeekKey)
SCAN WHILE FABRIC+COLOR = lcSeekKey
  IF ONORDER<>0
    ?? CHR(7)
    =gfModalGen('QRM36032B36000','ALERT')
    llDelYes = .F.
    *--HDM B602606,1 Alias Style Not Found. We may need To Close Style File[Start]
    IF llOpnStyle
      USE IN Style
      STORE .F. TO llOpnStyle
    ENDIF
    *--HDM B602606,1 Alias Style Not Found. We may need To Close Style File[End]
    RETURN llDelYes
  ENDIF
  
  *E301911,1 ABD - check before dele the item color onthe nSell Order Qty. [Begin]
  IF nSellOrder # 0
    ?? CHR(7)
    *-- Text Message  :- There is Quantity On hand of this item/color. Can not delete.
    *-- Text Number   :- 36185
    *-- Buttom Text   :- Ok
    *-- Buttom Number :- 00000
    
    =gfModalGen('QRM36185B36000','ALERT','Quantity On hand')
    llDelYes = .F.
    IF llOpnStyle
      USE IN Style
      STORE .F. TO llOpnStyle
    ENDIF
    RETURN llDelYes
  ENDIF
  
  *-- check before dele the item color onthe nSell ship Qty. [Begin]  
  IF nSellShip # 0
    ?? CHR(7)
    *-- Text Message  :- There is Shipped quantity of this item/color. Can not delete.
    *-- Text Number   :- 36185
    *-- Buttom Text   :- Ok
    *-- Buttom Number :- 00000
    
    =gfModalGen('QRM36185B36000','ALERT','Shipped quantity')
    llDelYes = .F.
    IF llOpnStyle
      USE IN Style
      STORE .F. TO llOpnStyle
    ENDIF
    RETURN llDelYes
  ENDIF
  *E301911,1 ABD - [End]
  
  
  IF ONHAND<>0
    ??CHR(7)
    IF llGl_Link
      =gfModalGen('QRM36033B36000','ALERT')
      llDelYes = .F.
    
      *--HDM B602606,1 Alias Style Not Found. We may need To Close Style File[Start]
      IF llOpnStyle
        USE IN Style
        STORE .F. TO llOpnStyle
      ENDIF
      *--HDM B602606,1 Alias Style Not Found. We may need To Close Style File[End]

      RETURN llDelYes
    ELSE
      lnChoice=gfModalGen('QRM36034B36001','ALERT')
      IF lnChoice = 2
       llDelYes = .F.

      *--HDM B602606,1 Alias Style Not Found. We may need To Close Style File[Start]
      IF llOpnStyle
        USE IN Style
        STORE .F. TO llOpnStyle
      ENDIF
      *--HDM B602606,1 Alias Style Not Found. We may need To Close Style File[End]

       RETURN llDelYes
      ENDIF
    ENDIF
  ENDIF
  SELECT BOM
  FOR lnType =1 TO 5
    Z = STR(lnType,1)
    IF lcMtype&Z $ 'FT' .OR. lcItype&Z $ 'FT'
      IF SEEK (Z+Fabric.FABRIC+SPACE(12)+Fabric.COLOR) OR;
        SEEK (Z+Fabric.Fabric+SPACE(12)+'******')
        *--HDM B602606,1[start] Also check that style color exists in style file case
        *--              fabric color same as style color
        
        IF BOM.ICLR = '******'
          *B603768,1 (Begin) Check if the selected color exists in the fabric file for the fabric contains
          *B603768,1         the current color in ins cost sheet
          lcSeekEx = Fabric.Fabric+Fabric.Color
          IF SEEK(SUBSTR(BOM.CITMMAJOR,1,7)+IIF(llAllColrs,laSource[lsSource],laData[2]),'Fabric')
             IF VARREAD() = "PBALL"
               *-- If we move all, diplay its message once, not form every color.
               *-- This variable will be .T. from gfMover when pressing Move All.
               IF llDispMess
                 llDispMess =.F.
                 lnChoice =gfModalGen('QRM36170B42001','ALERT')
               ENDIF
             ELSE  
               *--If we move one by one, display its meassage.
               lnChoice =gfModalGen('QRM36169B42001','ALERT',ALLTRIM(BOM.ITEM)+"\"+IIF(llAllColrs,laSource[lsSource],laData[2]))
             ENDIF
             
             llDelYes = .F.
             RETURN llDelYes
          ENDIF
          =SEEK(lcSeekEx,'Fabric')
          *B603768,1 (End)
          *B804060 TMI [Start] Check that the Style code structure is created.
          IF lfCKSegInf()
          *B804060 TMI [End  ] 
            =lfGetStInf()
            SELECT STYLE
            SCAN FOR CSTYMAJOR = BOM.CITMMAJOR
              *-- HDM B603038,1 [Start] If Deleting specific color we have to assign it's value
              *IF SUBSTR(STYLE.STYLE , lnCPos , lnCLen) = laSource[lsSource]
              IF SUBSTR(STYLE.STYLE , lnCPos , lnCLen) = IIF(llAllColrs,laSource[lsSource],laData[2])
                *-- HDM B603038,1 [End]
                *B603768,1 (Begin) -- Item\color is used in a cost sheet. Cannot delete.
                *lnChoice =gfModalGen('QRM36035B36001','ALERT')
                * IF lnCHOICE = 2
                *llDelYes = .F.
                IF VARREAD() = "PBALL"
                  *-- If we move all, diplay its message once, not form every color.
                  *-- This variable will be .T. from gfMover when pressing Move All.
                  IF llDispMess
                    llDispMess =.F.
                    lnChoice =gfModalGen('QRM36170B42001','ALERT')
                  ENDIF
                ELSE  
                  *--If we move one by one, display its meassage.
                  lnChoice =gfModalGen('QRM36169B42001','ALERT',ALLTRIM(BOM.ITEM)+"\"+IIF(llAllColrs,laSource[lsSource],laData[2]))
                ENDIF
                *B603768,1 (End)
                llDelYes = .F.
                *--HDM B602606,1 Alias Style Not Found. We may need To Close Style File[Start]
                IF llOpnStyle
                  USE IN Style
                  STORE .F. TO llOpnStyle
                ENDIF
                *--HDM B602606,1 Alias Style Not Found. We may need To Close Style File[End]
                *B603768,1 (Begin) Remark.
                *ENDIF  
                *B603768,1 (End)
                RETURN llDelYes
              ENDIF
            ENDSCAN
          *B804060 TMI [Start] end of condition where style code stru is created.
          ENDIF
          *B804060 TMI [End  ]            
          *B603768,1 (Begin) Get the BOM file again to go on seeking in Next For.
          SELECT BOM
          *B603768,1 (End)
        ELSE
        *--HDM B602606,1[end]
          *B603768,1 (Begin) -- Item\color is used in a cost sheet. Cannot delete.
          *lnChoice =gfModalGen('QRM36035B36001','ALERT')
          *IF lnCHOICE = 2
          *  llDelYes = .F.
          lnChoice =gfModalGen('QRM36169B42001','ALERT',ALLTRIM(BOM.ITEM)+"\"+IIF(llAllColrs,laSource[lsSource],laData[2]))
          llDelYes = .F.
          *B603768,1 (End)

            *--HDM B602606,1 Alias Style Not Found. We may need To Close Style File[Start]
            IF llOpnStyle
              USE IN Style
              STORE .F. TO llOpnStyle
            ENDIF
            *--HDM B602606,1 Alias Style Not Found. We may need To Close Style File[End]
          *B603768,1 (Begin) Remark.  
          *ENDIF
          *B603768,1 (End)
          RETURN llDelYes
        ENDIF
      ENDIF  
    ENDIF
  ENDFOR  
ENDSCAN

*--HDM B602606,1 Alias Style Not Found. We may need To Close Style File[Start]
IF llOpnStyle
  USE IN Style
  STORE .F. TO llOpnStyle
ENDIF
*--HDM B602606,1 Alias Style Not Found. We may need To Close Style File[End]

RETURN llDelYes
*!*************************************************************
*! Name      : lfActDel
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : deletion action
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfActDel
PRIVATE lnAlias,lnLastRec,lnFrstRec

*E301077,81 (Start)
llNoteOpen = gfOpenFile(gcDataDir+'NotePad',gcDataDir+'NotePad','SH')
llFInvOpen = gfOpenFile(gcDataDir+'FInvtAdj',gcDataDir+'FInvtAdj','SH')
*E301077,81 (End)

lnAlias = SELECT()
lnFrstRec = 0
lnLastRec = 0
IF llAllColrs
  FOR lnClr = 1 TO ALEN(laTarget)
    SELECT FABRIC
    IF SEEK(laData[1])
      lnFrstRec  = RECNO()
      lnLastRec  = RECNO() 
      SCAN REST WHILE FABRIC+COLOR=laData[1]
        lnFrstRec  = MIN(RECNO(),lnFrstRec)
        lnLastRec  = MAX(RECNO(),lnLastRec)
      ENDSCAN
      IF SEEK(laData[1]+laTarget[lnClr])
        lnRecToDel = RECNO()
        IF lnFrstRec = lnRecToDel AND lnLastRec <> lnFrstRec
          GO lnLastRec
          SCATTER MEMVAR MEMO
          GO lnFrstRec
          GATHER MEMVAR MEMO
          GO lnLastRec
        ENDIF
        IF RLOCK()
          BLANK
          DELETE
          UNLOCK
        ENDIF 
      ENDIF  
    ENDIF  
  
    SELECT FINVTADJ
    SEEK(SUBSTR(laData[1],1,7) + laTarget[lnClr])
    DELETE REST WHILE (Fabric+Color = SUBSTR(laData[1],1,7) +SUBSTR(laData[2],1,6))
  
    SELECT NOTEPAD
    SEEK 'G'+SUBSTR(laData[1],1,7)
    DELETE REST WHILE TYPE+KEY='G'+SUBSTR(laData[1],1,7)
  
    SELECT FABDYE
    SEEK (SUBSTR(laData[1],1,7) + laTarget[lnClr])
    SCAN WHILE (FABRIC+COLOR=SUBSTR(laData[1],1,7)+laTarget[lnClr])
      XR=RLOCK()
      *--HDM MOD Fields No Longer Used[Start]
      *REPL MOD_ACTION WITH 'DELETED'
      *REPL MOD_DATE   WITH DATE()
      *REPL MOD_TIME   WITH TIME()
      *REPL MOD_USERID WITH gcUser_Id
      = gfAdd_Info('FABDYE')
      *--HDM MOD Fields No Longer Used[End]

      UNLOCK
      DELETE NEXT 1
    ENDSCAN
    
    SELECT ROLLS
    SEEK (SUBSTR(laData[1],1,7) + laTarget[lnClr])
    DELETE REST WHILE ITEM+COLOR=SUBSTR(laData[1],1,7)+laTarget[lnClr]
    *C102684,1 (Begin) Delete the Vendor references for this color
    IF !USED('VENDMATL')
      =gfOpenFile(gcDataDir+'VENDMATL',gcDataDir+'Venmat','SH')
    ENDIF 
    SELECT VENDMATL
    DELETE ALL FOR FABRIC+COLOR = SUBSTR(laData[1],1,7)+laTarget[lnClr] 
    *C102684,1 (End)

    
  ENDFOR  
ELSE
    SELECT FABRIC
    =SEEK(laData[1])
    lnFrstRec  = RECNO()
    lnLastRec  = RECNO() 
    SCAN REST WHILE FABRIC+COLOR=laData[1]
      lnFrstRec  = MIN(RECNO(),lnFrstRec)
      lnLastRec  = MAX(RECNO(),lnLastRec)
    ENDSCAN
    IF SEEK(laData[1]+laData[2])
      lnRecToDel = RECNO()
      IF lnFrstRec = lnRecToDel AND lnLastRec <> lnFrstRec
        GO lnLastRec
        SCATTER MEMVAR MEMO
        GO lnFrstRec
        GATHER MEMVAR MEMO
        GO lnLastRec
      ENDIF
      =RLOCK()
      BLANK
      DELETE
      UNLOCK
    ENDIF  
  
  SELECT FINVTADJ
  SEEK(SUBSTR(laData[1],1,7) + SUBSTR(laData[2],1,6))
  DELETE REST WHILE (Fabric+Color = SUBSTR(laData[1],1,7) +SUBSTR(laData[2],1,6))
  
  SELECT NOTEPAD
  SEEK 'G'+SUBSTR(laData[1],1,7)
  DELETE REST WHILE TYPE+KEY='G'+SUBSTR(laData[1],1,7)
  
  SELECT FABDYE
  SEEK (SUBSTR(laData[1],1,7) + SUBSTR(laData[2],1,6))
  SCAN WHILE (FABRIC+COLOR=SUBSTR(laData[1],1,7)+SUBSTR(laData[2],1,6))
    XR=RLOCK()
    *--HDM MOD Fields No Longer Used[Start]

    *REPL MOD_ACTION WITH 'DELETED'
    *REPL MOD_DATE   WITH DATE()
    *REPL MOD_TIME   WITH TIME()
    *REPL MOD_USERID WITH gcUser_Id
    = gfAdd_Info('FABDYE')
    *--HDM MOD Fields No Longer Used[End]

    UNLOCK
    DELETE NEXT 1
  ENDSCAN
  
  SELECT ROLLS
  SEEK (SUBSTR(laData[1],1,7) + SUBSTR(laData[2],1,6))
  DELETE REST WHILE ITEM+COLOR=SUBSTR(laData[1],1,7)+SUBSTR(laData[2],1,6)
  *C102684,1 (Begin) Delete the Vendor references for this color
  IF !USED('VENDMATL')
    =gfOpenFile(gcDataDir+'VENDMATL',gcDataDir+'Venmat','SH')
  ENDIF 
  SELECT VENDMATL
  DELETE ALL FOR FABRIC+COLOR = SUBSTR(laData[1],1,7)+SUBSTR(laData[2],1,6)
  *C102684,1 (End)
ENDIF

*E301077,81 (Start)
IF llNoteOpen 
  =gfCloseFile('NotePad')
ENDIF  
IF llFInvOpen 
  =gfCloseFile('FInvtAdj')
ENDIF  
*E301077,81 (End)

SELECT(lnAlias)
*!*************************************************************
*! Name      : lpSavScr
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : save the modification done
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
PROCEDURE lpSavScr
PRIVATE lnRecNum

SELECT FABRIC
IF !EOF()
  lnRecNum = RECNO()
ENDIF
IF EMPTY(lcIType)
  lcIType = laType[lnType,2]
ENDIF
*--HDM B602470,1 Check The Purchase Code Value before Saving[start]
IF EMPTY(lcPurCd)
  lcPurCd = laPurc[lnPurc,2]
ENDIF
*--HDM B602470,1 Check The Purchase Code Value before Saving[end]
IF EMPTY(laData[18])
  IF llMultCur AND lcDomestic = laDomestic[2]
    =gfModalGen('QRM36036B36000','ALERT', 'Price')
    lnlastfold = lnactfolder
    lnactfolder  = 4
    =lfchngfolder(lnactfolder)
    _CUROBJ = OBJNUM(laData[18])
    llCSave = .F.
    RETURN 
  ELSE
    laData[18]  = gcBaseCurr
  ENDIF  
ENDIF

IF EMPTY(laData[19])
  IF llMultCur AND lcDomestic = laDomestic[2]
    =gfModalGen('QRM36036B36000','ALERT', 'Duty')
    lnlastfold = lnactfolder
    lnactfolder  = 4
    =lfchngfolder(lnactfolder)
    _CUROBJ = OBJNUM(laData[19])
    llCSave = .F.
    RETURN 
  ELSE
    laData[19]  = gcBaseCurr
  ENDIF
  
ENDIF

SELECT (lcTempClr)
GO TOP
IF EOF() AND llAllColrs
  =gfModalGen('QRM36037B36000','ALERT')
  lnlastfold = lnactfolder
  lnactfolder  = 3
  =lfchngfolder(lnactfolder)
  _CUROBJ = OBJNUM(pbcNew)
  *SELECT FABRIC
  SELECT(lcBaseFile)
  llCSave = .F.
  RETURN
ENDIF
SELECT FABRIC

*B038753,1 WSH If Cost Method is Standard and Cost changes, update Inv. Value [Start]
IF laScrMode[3] AND ALLTRIM(gfGetMemVar('M_MATCSTMT')) = 'S'

  lcColorFil = gfTempName()

  SELECT Fabric, Color, Fabric+Color AS Style, CostUse, CostBuy, OnHand ;
   FROM  Fabric ;
   WHERE IIF(llAllColrs, Fabric = laData[1], Fabric+Color = laData[1]+laData[2]) ;
   INTO  TABLE (gcWorkDir+lcColorfil)

  SELECT (lcColorFile)
  INDEX ON Fabric+Color TAG (lcColorFil)
  SET ORDER TO (lcColorFil)

  REPLACE CostBuy WITH laData[20],;
          COSTUse WITH laData[21] ALL

  llContinSave = .F.
  
  DO (gcAppHome+'ICUPDSV') WITH '0002', IIF(llAllColrs, laData[1], laData[1]+laData[2]), lcColorFil
  
  SELECT (lcColorfil)
  USE
  ERASE (gcWorkDir+lcColorfil+'.DBF')
  ERASE (gcWorkDir+lcColorfil+'.CDX')

  IF !llContinSave
    SELECT(lcBaseFile)
    llCSave = .F.
    RETURN
  ENDIF
ENDIF
*B038753,1 WSH [End]
*B126598,1 ASH 02/27/2005 (Begin) Fix the bug of 'record is used by another user' when editing the cost.
SELECT FABRIC
*B126598,1 ASH 02/27/2005 (End)
IF llAllColrs
  DO lpAccFab
  
  *E301582,1 ABD - Delete the imported item that save in the fabric file. [Begin]
  IF llImprtFab
    *-- Delete the seletced item from the PDMMATRL File.
    lnAlias = SELECT (0)
    IF SEEK(ladata[1],'PDMATRL')
      SELECT PDMATRL
      *BB604329,1 ABD Replace the filed with blank . [Begin]
      *DELETE ALL For Fabric = ladata[1]
      SCAN FOR FABRIC  = ladata[1]
        BLANK
        DELETE 
      ENDSCAN
      *BB604329,1 ABD [End]
    ENDIF
    
    *-- Mark the material in the log file as an imported material.
    IF SEEK('COMPONENT'+ladata[1],'PDMLOG')
      SELECT PDMLOG
      REPLACE cImpstatus With 'C'
    ENDIF
    SELECT (lnAlias)

    *E301740,4 ABD - [Begin]
    *-- Add the note pade in case we are in add mode and SP module instuall
    *-- And also when we import the item.
    IF laScrMode[4] .AND. llSpIstall .AND. llImprtFab .AND. !EMPTY(lcPdmNotes)
  
      lnAlias = SELECT (0)
      llNotesfls = .F.
    
      IF !USED('NOTEPAD')
        llNotesfls = gfOpenFile(gcDataDir+'NOTEPAD',gcDataDir+'NOTEPAD','SH')
      ENDIF  
      SELECT NOTEPAD
      APPEND BLANK
      REPLACE type    WITH 'G'                                  ,;
              key     WITH laData[1]                            ,;
              cdesc   WITH 'Notes For Item Number : '+laData[1] ,;
              mnotes  WITh gcUser_id + DTOC(gdsysdate)+ CHR(13)+lcPdmNotes
      IF llNotesfls
        =gfCloseFile('NOTEPAD')
      ENDIF
      *-- Update field has notes with .T.
      SELECT FABRIC
      = SEEK(laData[1])
      REPLACE lhasnotes WITH .T. While Fabric = laData[1]

      SELECT (lnAlias)
      *-- We will empty this Variable to didn't ask the user
      *-- to save the note pade or not , also we will restore this
      *--  variable in the lpshow in View mode laScrMode[1].
      lcSydKey = ''
    ENDIF
    *E301740,4 ABD - [End]

    llImprtFab = .F.
  ENDIF
  *E301582,1 ABD - [End]
ELSE
  DO lpAccClr
ENDIF

*--B602753,1 HDM 04/07/1999[Start] Stop Calling NotePad Program In lpSavScr as the global save
*--B602753,1 HDM 04/07/1999        Will Call it
*IF laScrMode[4]
* IF gfModalGen('INM00271B00006','DIALOG') = 1
*   =lfvNotes()
* ENDIF  
*ENDIF
*--B602753,1 HDM 04/07/1999[End]
*B803404,1 (Begin) Remark SHOW GETS as it calls the global control function which restore old values.
*B803404,1         in Edit mode.
*SHOW GETS
*B803404,1 (End)
WAIT CLEAR
SELECT FABRIC
IF TYPE('lnRecNum') <> 'U' .AND. !laScrMode[4]
  GOTO (lnRecNum)
ENDIF
RETURN


*!*************************************************************
*! Name      : lpAccFab
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : save the modification done for a fabric
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
PROCEDURE lpAccFab
*--HDM B602180,1 [start] Fixing Bug Of incorrect Record Pointing
*-- as in case of all colors we have to keep track of record pointer to allow gpsave
*-- Unlocking the file correctly
PRIVATE lnFabRec
*--HDM B602180,1 [end]

*llUpdatWar = .F.
IF llTrkRolls .AND. !(laData[14]) .AND. SEEK(SUBSTR(laData[1],1,7),'ROLLS')
  IF gfModalGen('QRM36038B36001','ALERT') = 1
    SELECT ROLLS
    DELETE ALL FOR crollitem+COLOR+CWARECODE+DYELOT+CROLLID+TRANCD = laData[1]
    SELECT FABRIC
  ELSE
    SELECT FABRIC
    RETURN        
  ENDIF  
ENDIF

IF (laData[20] <> FABRIC.CostBuy .OR. laData[21] <> FABRIC.Costuse) 
  *SELECT FABDYE.FABRIC, FABDYE.COLOR, FABDYE.ONHAND;
         FROM FABDYE, FABRIC                       ;
         WHERE FABRIC.FABRIC  = FABDYE.FABRIC      ;
         AND FABRIC.COLOR   = FABDYE.COLOR         ;
         AND (FABRIC.FABRIC = laData[1]            ;
         AND EMPTY(FABDYE.DYELOT)                  ;
         AND FABDYE.ONHAND  = 0                    ;
         AND FABRIC.ONHAND  = 0)                   ;
  INTO ARRAY llTemp
  
  SELECT * FROM FABDYE ;
  WHERE FABDYE.FABRIC = laData[1];
  AND fabdye.onhand   = 0 ;
  AND EMPTY(FABDYE.DYELOT) ;
  INTO ARRAY llTemp
  
  IF _TALLY >0
  *B602827,1 [Start] if Single warehouse Don't ask for updating just update
    IF llMultiWH
      IF gfModalGen('QRM36039B36001','ALERT') = 1
        llUpdatWar   = .T.
        SELECT (lcTempClr)
        SET RELATION TO
        SET RELATION TO fabric+color INTO FABDYE ADDITIVE
      ELSE
        llUpdatWar   = .F.
      ENDIF
    ELSE
      llUpdatWar   = .T.
    ENDIF
  ELSE
    llUpdatWar   = .F.
  ENDIF
  *B602827,1 [End]
ENDIF  
*--HDM B602180,1 [start]
SELECT FABRIC
IF !EOF()
  lnFabRec = RECNO()
ENDIF
*--HDM B602180,1 [end]

SELECT (lcTempClr)
SCAN
  lcClr = COLOR
  SELECT FABRIC
  IF !EOF()
    lnFabRec = RECNO()
  ENDIF
  IF SEEK(SUBSTR(laData[1],1,7)+lcClr)
    RECALL
    =lfSvInFab(laData[1],lcClr,.F.)
    IF laScrMode[3]
      REPLACE nfAve_Cost WITH IIF(OnHand=0 .AND. llCostUp ,CostUse,nfAve_Cost)  ;
              nAveCstBuy WITH IIF(OnHand=0 .AND. llCostUp ,CostBuy,nAveCstBuy)
      *--HDM B602283,1 added the check of update warehouse flag [start]
  
      IF llUpdatWar .AND. !EOF('FabDye')
      *--HDM B602283,1 added the check of update warehouse flag [end]
        SELECT FabDye
        *--MOD Fields No Longer Used[Start]
        *REPLACE REST ;
               WHILE FabDye.Fabric = SUBSTR(laData[1],1,7) .AND.     ;
                     FabDye.Color  = &lcTempClr..Color  ;
                 FOR FabDye.Onhand = 0        .AND.     ;
                     EMPTY(FABDYE.DYELOT)               ;
                     nfAve_Cost WITH IIF(OnHand=0 .AND. llCostUp,laData[21],nfAve_Cost)      ;
                     nAveCstBuy WITH IIF(OnHand=0 .AND. llCostUp,laData[20],nAveCstBuy)      ; 
                     Mod_Action WITH 'MODIFIED'         ;
                     Mod_Date   WITH DATE()             ; 
                     Mod_Time   WITH TIME()             ; 
                     Mod_UserID WITH gcUser_Id 

        REPLACE REST ;
               WHILE FabDye.Fabric = SUBSTR(laData[1],1,7) .AND.     ;
                     FabDye.Color  = &lcTempClr..Color  ;
                 FOR FabDye.Onhand = 0        .AND.     ;
                     EMPTY(FABDYE.DYELOT)               ;
                     nfAve_Cost WITH IIF(OnHand=0 .AND. llCostUp,laData[21],nfAve_Cost)   ;
                     nAveCstBuy WITH IIF(OnHand=0 .AND. llCostUp,laData[20],nAveCstBuy)
       =gfAdd_Info('FabDye')
       *--MOD Fields No Longer Used[End]

      *--HDM B602283,1 added the check of update warehouse flag [start]
      ENDIF               
      *--HDM B602283,1 added the check of update warehouse flag [end]

    ENDIF  
  ELSE
    lcSetDel = SET("DELETE")
    SET DELETE OFF
    IF SEEK(SPACE(7)) AND DELETED()
      RECALL
      BLANK
    ELSE
      APPEND BLANK
    ENDIF
    =lfSvInFab(laData[1],lcClr,.T.)
    SET DELETE &lcSetDel 
  ENDIF

  IF llTrkRolls .AND. laData[14]
    IF !SEEK(SUBSTR(laData[1],1,7)+lcClr,'ROLLS')
      =lfAddRolls(SUBSTR(laData[1],1,7),lcClr,&lcTempClr..ONHAND)
    ENDIF  
  ENDIF
ENDSCAN  
SET DELETE ON
SELECT FABRIC
*--HDM B602180,1 [start]
IF TYPE('lnFabRec') <> 'U'
  GO (lnFabRec) IN FABRIC
ENDIF
=SEEK(laData[1])
*--HDM B602180,1 [END]

*!*************************************************************
*! Name      : lpAccClr
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : save the modification done for a fabric with its colors
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
PROCEDURE lpAccClr
PRIVATE lnAlias , lnFabRec

lnSaveRepr   = SET('REPROCESS')
SET REPROCESS TO 1
IF RLOCK()
  IF llTrkRolls
    IF laData[14]
      IF !SEEK(SUBSTR(laData[1],1,7)+SUBSTR(laData[2],1,6),'ROLLS')
        =lfAddRolls(SUBSTR(laData[1],1,7),SUBSTR(laData[2],1,6),lnOnHAnd)
      ENDIF  
    ELSE
      IF SEEK(SUBSTR(laData[1],1,7)+SUBSTR(laData[2],1,6),'ROLLS')
        IF gfModalGen('QRM36038B36001','ALERT') = 1
          SELECT ROLLS
          DELETE ALL FOR crollitem+COLOR+CWARECODE+DYELOT+CROLLID+TRANCD = laData[1] + laData[2]
          SELECT FABRIC
        ELSE
          UNLOCK
          SET REPROCESS TO lnSaveRepr           
          RETURN        
        ENDIF  
      ENDIF
    ENDIF
  ENDIF
  lnPrevTbl = SELECT(0)
  =lfUpdWareH()
  SELECT (lnPrevTbl)
  lcMod_Action = 'MODIFIED'        && New status
  ldMod_Date   = DATE()            && New Date
  ltMod_Time   = TIME()            && New Time 
  lcMod_UserID = gcUser_Id           && New User 
  =SEEK (laData[1] + laData[2], "Fabric")
  IF (laData[20] <> FABRIC.CostBuy .OR. laData[21] <> FABRIC.Costuse) AND ;
      llMultiWH AND SEEK(laData[1]+laData[2], "FabDye") 
    SELECT FABDYE.FABRIC, FABDYE.COLOR, FABDYE.ONHAND;
           FROM FABDYE, FABRIC;
          WHERE FABRIC.FABRIC  = FABDYE.FABRIC;
            AND FABRIC.COLOR   = FABDYE.COLOR;
            AND (FABRIC.FABRIC = laData[1];
            AND FABRIC.COLOR   = laData[2];
            AND EMPTY(FABDYE.DYELOT);
            AND FABDYE.ONHAND  = 0);
           INTO ARRAY llTemp
    
    IF _TALLY > 0
      *B602827,1 [Start] if Single warehouse Don't ask for updating just update
      IF  llMultiWH
        IF gfModalGen('QRM36039B36001','ALERT') = 1
          llUpdatWar   = .T.
        ELSE
          llUpdatWar   = .F.
        ENDIF
      ELSE
        llUpdatWar   = .T.
      ENDIF
      *B602827,1 [End]
    ELSE
      llUpdatWar   = .F.
    ENDIF
    IF llUpdatWar
      lnAlias = SELECT()
      SELECT FabDye
      *-- MOD Fields No Longer Used[Start]
      *REPLACE REST ;
         WHILE FabDye.Fabric = laData[1] .AND. ;
                    FabDye.Color  = laData[2]       ;
                FOR FabDye.Onhand = 0        .AND.  ;
                    EMPTY(FABDYE.DYELOT)            ;
                    nfAve_Cost WITH laData[21]      ;
                    nAveCstBuy WITH laData[20]      ; 
                    Mod_Action WITH 'MODIFIED'      ;
                    Mod_Date   WITH DATE()          ; 
                    Mod_Time   WITH TIME()          ; 
                    Mod_UserID WITH gcUser_Id 


      REPLACE REST ;
                WHILE FabDye.Fabric = laData[1] .AND. ;
                      FabDye.Color  = laData[2]       ;
                  FOR FabDye.Onhand = 0        .AND.  ;
                      EMPTY(FABDYE.DYELOT)            ;
                      nfAve_Cost WITH IIF(llCostUp,laData[21],nfAve_Cost)      ;
                      nAveCstBuy WITH IIF(llCostUp,laData[20],nAveCstBuy)
      =gfAdd_Info('FabDye')
      *-- MOD Fields No Longer Used[End]

      SELECT(lnAlias)
    ELSE
      llUpdatWar   = .F.
    ENDIF                
  ENDIF
  lnPrevTbl = SELECT(0)
  =lfSvInFab(laData[1],laData[2],.F.)
  SELECT (lnPrevTbl)
  UNLOCK
ELSE
  =gfModalGen('QRM36040B36000','ALERT')
ENDIF
SET REPROCESS TO lnSaveRepr
*!*************************************************************
*! Name      : lfSvInFab
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : save the modification done for a fabric in fabric file
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfSvInFab
PARAMETERS lcFItem , lcFColor , llNewClr

*--Added by HDM To Check Cost Updated Fields[start]
*PRIVATE llCostUp
*STORE .F. TO llCostUp
*--HDM B802184,1  we have 4 Cost Items Not 2[Start]
*FOR lnLoop = 1 TO 2
FOR lnLoop = 1 TO 4
*--HDM B802184,1  we have 4 Cost Items Not 2[End]
  lcCstNo = ALLTRIM(STR(lnLoop))
  IF ALLTRIM("lnBcost")+ALLTRIM(lcCstNo) $ lcSFields .OR. ALLTRIM('lnCost')+ALLTRIM(lcCstNo) $ lcSFields
    STORE .T. TO llCostUp
  ENDIF
ENDFOR
*--Added by HDM To Check Cost Updated Fields[end]

lnAlias = SELECT()
SELECT FABRIC

*B607483,1 ABD - Check if the total Cost buy is change or not.  [Begin]
IF laScrMode[3] .AND. !(lnBCost1 + lnBCost2 + lnBCost3 + lnBCost4 = COSTBUY)
  STORE .T. TO llCostUp
ENDIF
*B607483,1 ABD [End]

*B120349,1 ABD - Fix bug that when a new color is added, all colors change to the 
*B120349,1 ABD - Item Type selected under the new color, So we will define new 
*B120349,1 ABD - variable to know if the item type was changed or no. [Begin] 
*REPLACE FABRIC     WITH lcFItem ;
        COLOR      WITH lcFColor; 
        DESC       WITH IIF('laData[3]'  $ lcSFields OR llNewClr,laData[3],DESC);
        MAKE       WITH IIF('ibDomestic' $ lcSFields OR llNewClr,IIF(lnDomestic = 1,.T.,.F.),MAKE);
        CONTENT    WITH IIF('laData[7]'  $ lcSFields OR llNewClr,laData[7],CONTENT);
        LOC        WITH IIF('laData[8]'  $ lcSFields OR llNewClr,laData[8],LOC);
        INTRODUCED WITH IIF('laData[9]'  $ lcSFields OR llNewClr,laData[9],INTRODUCED);
        ITEM_TYPE  WITH lcIType;
        cComCode   WITH IIF('laData[11]' $ lcSFields OR llNewClr,laData[11],cComCode); 
        VENDOR     WITH IIF('laData[12]' $ lcSFields OR llNewClr,laData[12],VENDOR);
        PATTERN    WITH IIF('laData[6]'  $ lcSFields OR llNewClr,laData[6],PATTERN);
        WIDTH      WITH IIF('laData[4]'  $ lcSFields OR llNewClr,laData[4],WIDTH);
        cfabweight WITH IIF('laData[5]'  $ lcSFields OR llNewClr,laData[5],cfabweight);
        UOMBUY     WITH IIF('laData[15]' $ lcSFields OR llNewClr,laData[15],UOMBUY);
        UOMUSE     WITH IIF('laData[17]' $ lcSFields OR llNewClr,laData[17],UOMUSE);
        CONV       WITH IIF('laData[16]' $ lcSFields OR llNewClr OR laScrMode[4],laData[16],CONV);
        LEADTIME   WITH IIF('laData[10]' $ lcSFields OR llNewClr,laData[10],LEADTIME);
        cFabGrade  WITH lcQuality ;
        cFabric1   WITH IIF('lcFabric1'  $ lcSFields OR llNewClr,lcFabric1,cFabric1);
        cFabric2   WITH IIF('lcFabric2'  $ lcSFields OR llNewClr,lcFabric2,cFabric2);
        cColor1    WITH IIF('lcColor1'   $ lcSFields OR llNewClr,lcColor1,cColor1);
        cColor2    WITH IIF('lcColor2'   $ lcSFields OR llNewClr,lcColor2,cColor2);
        CPURCODE   WITH lcPurCd

REPLACE FABRIC     WITH lcFItem ;
        COLOR      WITH lcFColor; 
        DESC       WITH IIF('laData[3]'  $ lcSFields OR llNewClr,laData[3],DESC);
        MAKE       WITH IIF('ibDomestic' $ lcSFields OR llNewClr,IIF(lnDomestic = 1,.T.,.F.),MAKE);
        CONTENT    WITH IIF('laData[7]'  $ lcSFields OR llNewClr,laData[7],CONTENT);
        LOC        WITH IIF('laData[8]'  $ lcSFields OR llNewClr,laData[8],LOC);
        INTRODUCED WITH IIF('laData[9]'  $ lcSFields OR llNewClr,laData[9],INTRODUCED);
        ITEM_TYPE  WITH IIF(llModifType .OR. laScrMode[4] .OR. llNewClr, lcIType,ITEM_TYPE);
        cComCode   WITH IIF('laData[11]' $ lcSFields OR llNewClr,laData[11],cComCode); 
        VENDOR     WITH IIF('laData[12]' $ lcSFields OR llNewClr,laData[12],VENDOR);
        PATTERN    WITH IIF('laData[6]'  $ lcSFields OR llNewClr,laData[6],PATTERN);
        WIDTH      WITH IIF('laData[4]'  $ lcSFields OR llNewClr,laData[4],WIDTH);
        cfabweight WITH IIF('laData[5]'  $ lcSFields OR llNewClr,laData[5],cfabweight);
        UOMBUY     WITH IIF('laData[15]' $ lcSFields OR llNewClr,laData[15],UOMBUY);
        UOMUSE     WITH IIF('laData[17]' $ lcSFields OR llNewClr,laData[17],UOMUSE);
        CONV       WITH IIF('laData[16]' $ lcSFields OR llNewClr OR laScrMode[4],laData[16],CONV);
        LEADTIME   WITH IIF('laData[10]' $ lcSFields OR llNewClr,laData[10],LEADTIME);
        cFabGrade  WITH lcQuality ;
        cFabric1   WITH IIF('lcFabric1'  $ lcSFields OR llNewClr,lcFabric1,cFabric1);
        cFabric2   WITH IIF('lcFabric2'  $ lcSFields OR llNewClr,lcFabric2,cFabric2);
        cColor1    WITH IIF('lcColor1'   $ lcSFields OR llNewClr,lcColor1,cColor1);
        cColor2    WITH IIF('lcColor2'   $ lcSFields OR llNewClr,lcColor2,cColor2);
        CPURCODE   WITH lcPurCd
*B120349,1 ABD - [End]

* B604329,4 ABD Save Fabric as he was work , because the cost element 
* B604329,4 ABD is hold on the variable not in the file. [Begin]
*IF llSpIstall .AND. llImprtFab
*  REPLACE nFabCost   WITH IIF(!llMultCur OR lnDomestic =1,IIF('lnBcost1' $ lcSFields OR llNewClr,&lcTempClr..nFabCost,nFabCost),;
*                                                          IIF('lnCost1' $ lcSFields OR llNewClr,&lcTempClr..nFabCost,nFabCost)) ;
*          nMCost1    WITH IIF(!llMultCur OR lnDomestic =1,IIF('lnBcost1' $ lcSFields OR llNewClr,&lcTempClr..nFabCost,nMCost1),;
*                                                          IIF('lnCost1' $ lcSFields OR llNewClr,&lcTempClr..nFabCost,nMCost1)) ;
*          cPriceCur  WITH IIF(lnDomestic = 1,gcBaseCurr,IIF('laData[18]' $ lcSFields OR llNewClr OR laScrMode[4],laData[18],cPriceCur));
*          cDutyCur   WITH IIF(lnDomestic = 1,gcBaseCurr,IIF('laData[19]' $ lcSFields OR llNewClr OR laScrMode[4],laData[19],cDutyCur))
*ELSE
REPLACE nFabCost   WITH IIF(!llMultCur OR lnDomestic =1,IIF('lnBcost1' $ lcSFields OR llNewClr,lnBcost1,nFabCost),;
                                                        IIF('lnCost1' $ lcSFields OR llNewClr,lnCost1,nFabCost)) ;
        nMCost1    WITH IIF(!llMultCur OR lnDomestic =1,IIF('lnBcost1' $ lcSFields OR llNewClr,lnBCost1,nMCost1),;
                                                        IIF('lnCost1' $ lcSFields OR llNewClr,lnCost1,nMCost1)) ;
        cPriceCur  WITH IIF(lnDomestic = 1,gcBaseCurr,IIF('laData[18]' $ lcSFields OR llNewClr OR laScrMode[4],laData[18],cPriceCur));
        cDutyCur   WITH IIF(lnDomestic = 1,gcBaseCurr,IIF('laData[19]' $ lcSFields OR llNewClr OR laScrMode[4],laData[19],cDutyCur))
*ENDIF
* B604329,4 ABD [End]

*B604669,1 WAB (Start) - fix the bug when we type only the percentage cost field it update the file 
*REPLACE nItm_Frt   WITH IIF(!llMultCur OR lnDomestic =1,IIF('lnBcost4' $ lcSFields OR llNewClr,lnBCost4,nItm_Frt),;
                                                        IIF('lnCost4' $ lcSFields OR llNewClr,lnCost4,nItm_Frt)) ;
        nMCost4    WITH IIF(!llMultCur OR lnDomestic =1,IIF('lnBcost4' $ lcSFields OR llNewClr,lnBCost4,nItm_Frt),;
                                                        IIF('lnCost4' $ lcSFields OR llNewClr,lnCost4,nItm_Frt)) ;
        nItem_Tax  WITH IIF(!llMultCur OR lnDomestic =1,IIF('lnBcost2' $ lcSFields OR llNewClr,lnBCost2,nItem_Tax),;
                                                        IIF('lnCost2' $ lcSFields OR llNewClr,lnCost2,nItem_Tax)) ;
        nMCost2    WITH IIF(!llMultCur OR lnDomestic =1,IIF('lnBcost2' $ lcSFields OR llNewClr,lnBCost2,nMCost2),;
                                                        IIF('lnCost2' $ lcSFields OR llNewClr,lnCost2,nMCost2)) ;
        nItemQuota WITH IIF(!llMultCur OR lnDomestic =1,IIF('lnBcost3' $ lcSFields OR llNewClr,lnBCost3,nItemQuota),;
                                                        IIF('lnCost3' $ lcSFields OR llNewClr,lnCost3,nItemQuota)) ;
        nMCost3    WITH IIF(!llMultCur OR lnDomestic =1,IIF('lnBcost3' $ lcSFields OR llNewClr,lnBCost3,nMCost3),;
                                                        IIF('lnCost3' $ lcSFields OR llNewClr,lnCost3,nMCost3))

REPLACE nItm_Frt   WITH IIF(!llMultCur OR lnDomestic =1,IIF('lnBcost4' $ lcSFields OR 'lnCst4P' $ lcSFields OR llNewClr,lnBCost4,nItm_Frt),;
                                                        IIF('lnCost4' $ lcSFields OR llNewClr,lnCost4,nItm_Frt)) ;
        nMCost4    WITH IIF(!llMultCur OR lnDomestic =1,IIF('lnBcost4' $ lcSFields OR 'lnCst4P' $ lcSFields OR llNewClr,lnBCost4,nItm_Frt),;
                                                        IIF('lnCost4' $ lcSFields OR llNewClr,lnCost4,nItm_Frt)) ;
        nItem_Tax  WITH IIF(!llMultCur OR lnDomestic =1,IIF('lnBcost2' $ lcSFields OR 'lnCst2P' $ lcSFields OR llNewClr,lnBCost2,nItem_Tax),;
                                                        IIF('lnCost2' $ lcSFields OR llNewClr,lnCost2,nItem_Tax)) ;
        nMCost2    WITH IIF(!llMultCur OR lnDomestic =1,IIF('lnBcost2' $ lcSFields OR 'lnCst2P' $ lcSFields OR llNewClr,lnBCost2,nMCost2),;
                                                        IIF('lnCost2' $ lcSFields OR llNewClr,lnCost2,nMCost2)) ;
        nItemQuota WITH IIF(!llMultCur OR lnDomestic =1,IIF('lnBcost3' $ lcSFields OR 'lnCst3P' $ lcSFields OR llNewClr,lnBCost3,nItemQuota),;
                                                        IIF('lnCost3' $ lcSFields OR llNewClr,lnCost3,nItemQuota)) ;
        nMCost3    WITH IIF(!llMultCur OR lnDomestic =1,IIF('lnBcost3' $ lcSFields OR 'lnCst3P' $ lcSFields OR llNewClr,lnBCost3,nMCost3),;
                                                        IIF('lnCost3' $ lcSFields OR llNewClr,lnCost3,nMCost3))
*B604669,1 WAB (End)                                      
*--HDM Fixing Bug check for updated costs when updating

*llCostUp = IIF('lnBcost1' $ lcSFields .OR. 'lnBcost2' $ lcSFields .OR. ;
*               'lnBcost3' $ lcSFields .OR. 'lnBcost4' $ lcSFields .OR. ;
*               'lnBcost5' $ lcSFields .OR. 'lnCost1'  $ lcSFields .OR. 'lnCost2'  $ lcSFields;
*               .OR. 'lnCost3'  $ lcSFields .OR. 'lnCost4'  $ lcSFields .OR. 'lnCost5'  $ lcSFields, .T. , .F.)

*REPLACE COSTBUY    WITH IIF(llCostUp , laData[20] , COSTBUY) ;
*        COSTUSE    WITH IIF(llCostUp , laData[21] , COSTUSE) ;
*        REORDER    WITH lnReorder;
*        cDye_Flg   WITH IIF('cbDyelot' $ lcSFields OR llNewClr OR laScrMode[4],IIF(cbDyelot,'Y','N'),cDye_Flg);
*        lTrkRolls  WITH IIF('laData[14]' $ lcSFields OR llNewClr,laData[14],lTrkRolls);
*        LINK_CODE  WITH IIF('laData[13]' $ lcSFields OR llNewClr,laData[13],LINK_CODE);
*        nfAve_Cost WITH IIF(llNewClr,laData[21],laData[23]) ;
*        NAVECSTBUY WITH IIF(llNewClr,laData[20],laData[22])

*--HDM B602487,1 If We have a new color Average cost 
*--HDM B602487,1 should be replace with the standard cost[start]

*REPLACE COSTBUY    WITH IIF(llCostUp .OR. llNewClr, laData[20],COSTBUY);
        COSTUSE    WITH IIF(llCostUp .OR. llNewClr, laData[21],COSTUSE);
        REORDER    WITH lnReorder;
        cDye_Flg   WITH IIF('cbDyelot'   $ lcSFields OR llNewClr OR laScrMode[4],IIF(cbDyelot,'Y','N'),cDye_Flg);
        lTrkRolls  WITH IIF('laData[14]' $ lcSFields OR llNewClr,laData[14],lTrkRolls);
        LINK_CODE  WITH IIF('laData[13]' $ lcSFields OR llNewClr,laData[13],LINK_CODE);
        nfAve_Cost WITH IIF(llNewClr .OR. llCostUp ,laData[23],nfAve_Cost) ;
        NAVECSTBUY WITH IIF(llNewClr .OR. llCostUp ,laData[22],NAVECSTBUY)


REPLACE COSTBUY    WITH IIF(llCostUp .OR. llNewClr, laData[20],COSTBUY);
        COSTUSE    WITH IIF(llCostUp .OR. llNewClr, laData[21],COSTUSE);
        REORDER    WITH lnReorder;
        cDye_Flg   WITH IIF('cbDyelot'   $ lcSFields OR llNewClr OR laScrMode[4],IIF(cbDyelot,'Y','N'),cDye_Flg);
        lTrkRolls  WITH IIF('laData[14]' $ lcSFields OR llNewClr,laData[14],lTrkRolls);
        LINK_CODE  WITH IIF('laData[13]' $ lcSFields OR llNewClr,laData[13],LINK_CODE);
        nfAve_Cost WITH IIF(llCostUp .AND. ONHAND = 0 ,laData[23],IIF(llNewClr,COSTUSE,nfAve_Cost));
        NAVECSTBUY WITH IIF(llCostUp .AND. ONHAND = 0 ,laData[22],IIF(llNewClr,COSTBUY,NAVECSTBUY))


*E301911,1 ABD Save The new fields into fabric file. [Begin]
REPLACE cSlsGlLink  WITH IIF('laData[28]' $ lcSFields OR llNewClr,laData[28],cSlsGlLink);
        nSellPrice  WITH IIF('laData[25]' $ lcSFields OR llNewClr,laData[25],nSellPrice);
        cSellUOM    WITH IIF('laData[26]' $ lcSFields OR llNewClr,laData[26],cSellUOM)  ;
        nSellConv   WITH IIF('laData[27]' $ lcSFields OR llNewClr,laData[27],nSellConv)
*E301911,1 ABD [End]

*B604016,1 MHM 11/14/2000 [START]
IF !EMPTY(laUsrFields[1,1]) 
  *B604688,1 WAB - (START) save the delete setting and set it to be on
  lcDelSet = SET("DELETE")
  SET DELETE ON
  *B604688,1 WAB - (END)
  FOR lnUsrField = 1 TO ALEN(laUsrFields,1)
    *B605437,1 ABD - Fix problem that the program update custom field all color with 
    *B605437,1 ABD - The default value for first color when add new color (customer field).
    *B605437,1 ABD - So we will copy the Array that hold the user define file to 
    *B605437,1 ABD - compere when saving if the value change. [Begin]
    *=lfSvUsrFld()
    IF laScrMode[4] .OR. llNewClr .OR. ;
       (laScrMode[3] .AND. laCpUrFlds[lnUsrField,6] # laUsrFields[lnUsrField,6])
     =lfSvUsrFld()
    ENDIF
    *B605437,1 ABD - [End]
  ENDFOR
  *B604688,1 WAB - (Start) return back the delete setting to the save one 
  SET DELETE &lcDelSet 
  *B604688,1 WAB - (End)
ENDIF  
*B604016,1 MHM 11/14/2000 [END]
  *--Added gfAdd_Info Calling as it was not called[start]
  = gfAdd_Info('FABRIC')
  *--Added gfAdd_Info Calling as it was not called[End]

*--HDM B602487,1[end]
*SELECT(lnAlias)
*RETURN

IF !llMultiWH
  SELECT FABDYE
  IF llNewClr
    APPEND BLANK
    *--HDM 03/08/1999 (B) Add_Date Not found [start]
    *--                   We should use gfAdd_Info
  ELSE
   =SEEK(lcFItem+lcFColor)
  ENDIF
  IF FABDYE.ONHAND = 0
    REPLACE Fabric     WITH lcFItem   ,;
            Color      WITH lcFColor  ,;
            Dyelot     WITH SPACE(10) ,;
            cWareCode  WITH WAREHOUS.cWarecode,;
            nfAve_Cost WITH IIF(llCostUp .OR. llNewClr,laData[21],nfAve_Cost),;
            NaveCstBuy WITH IIF(llCostUp .OR. llNewClr,laData[20],NaveCstBuy)

    *REPLACE Fabric     WITH lcFItem   ,;
            Color      WITH lcFColor  ,;
            Dyelot     WITH SPACE(10) ,;
            cWareCode  WITH WAREHOUS.cWarecode,;
            nfAve_Cost WITH laData[21],;
            NaveCstBuy WITH laData[20],;
            Add_Date   WITH DATE()    ,;
            Add_time   WITH TIME()    ,;
            Add_UserId WITH gcUser_Id
          
    = gfAdd_Info('FABDYE')
    *--HDM 03/08/1999 (B) Add_Date Not found [end]
  ENDIF
ENDIF            
SELECT(lnAlias)
*!*************************************************************
*! Name      : lfAddRolls
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : add initial record in the rolls file for the selected fabric/color
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfAddRolls
PARAMETERS lcRFabric,lcRColor,lnROnhand

lcCurFile = ALIAS()
IF cbDyelot = .T.
  IF SEEK(lcRFabric+lcRColor,"FABDYE")
    SELECT FABDYE
    lcSession = " "
    SCAN WHILE Fabric+Color = lcRFabric+lcRColor FOR !EMPTY(Dyelot)
      IF FABDYE.Onhand > 0
        IF EMPTY(lcSession)
          lcSession = gfsequence("GLSESSION")
        ENDIF
        SELECT ROLLS
        APPEND BLANK
        REPLACE cRollItem WITH lcRFabric              ;
                Color     WITH lcRColor               ;
                Cwarecode WITH fabdye.cwarecode       ;  
                Dyelot    WITH fabdye.dyelot          ;
                Crollid   WITH ALLTRIM(fabdye.cwarecode)+ALLTRIM(fabdye.dyelot) ;  
                Nqtybal   WITH fabdye.Onhand          ;
                Nqty      WITH fabdye.Onhand          ;
                Trancd    WITH '1'                    ;
                Csession  WITH lcSession  ;
                Crsession WITH lcSession
      ENDIF          
    ENDSCAN
  ENDIF
ELSE
    IF SEEK(lcRFabric+lcRColor,"FABDYE")
      SELECT FABDYE
      lcSession = " "
      SCAN REST WHILE Fabric+Color = lcRFabric+lcRColor FOR EMPTY(Dyelot)
        IF Onhand > 0
          IF EMPTY(lcSession)
            lcSession= gfsequence("GLSESSION")
          ENDIF
          SELECT ROLLS
          APPEND BLANK
          REPLACE cRollItem WITH lcRFabric              ;
                  Color     WITH lcRColor               ;
                  Cwarecode WITH fabdye.cwarecode       ;  
                  Crollid   WITH fabdye.cwarecode       ;  
                  Nqtybal   WITH fabdye.Onhand          ;
                  Nqty      WITH fabdye.Onhand          ;
                  Trancd    WITH '1'                    ;
                  Csession  WITH lcSession  ;
                  Crsession WITH lcSession
        ENDIF          
      ENDSCAN
    ENDIF
  *ELSE
   * IF lnROnhand > 0
    *  SELECT ROLLS
     * APPEND BLANK
      * cRollItem WITH lcRFabric              ;
              Color     WITH lcRColor               ;  
              Crollid   WITH "INITIAL"              ;  
              Nqtybal   WITH lnOnHAnd               ;
              Nqty      WITH lnOnHAnd               ;
              Trancd    WITH '1'                    ;
              Csession  WITH gfsequence("GLSESSION")  ;
              Crsession WITH Csession

    *ENDIF  
  *ENDIF
ENDIF
SELECT (lcCurFile)
*!*************************************************************
*! Name      : lfUpdWareH
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : update the fabdye file with the warehouse details
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfUpdWareH

SELECT (lcTempWare)
DECLARE laTmpArray [FCOUNT()]
SET DELETE OFF
SCAN
  IF DELETED()
    IF SEEK(FABRIC+PADR(COLOR,6,' ')+CWARECODE,'FABDYE')
      SELECT FABDYE
       lcDelKey=Fabric+PADR(Color,6,' ')+cWareCode
       DELETE REST WHILE Fabric+PADR(Color,6,' ')+cWareCode = lcDelKey
    ENDIF
  ELSE
    IF cStatus $ "AM"
      lcKey=Fabric+PADR(Color,6,' ')+cWareCode
      SELECT FABDYE
      IF !SEEK(lcKey)
        IF SEEK(" ")
          RECALL
        ELSE
          APPEND BLANK
        ENDIF
      ELSE
        IF &lcTempWare..cStatus = 'A'
          IF DELETED()
            RECALL
          ENDIF
        ELSE
          SCAN WHILE FABRIC+COLOR+CWARECODE = lcKey
            IF !DELETED()
              EXIT
            ENDIF
          ENDSCAN
        ENDIF
      ENDIF
      SELECT (lcTempWare)
      SCATTER TO laTmpArray
      SELECT FABDYE
      GATHER FROM laTmpArray
    ENDIF
  ENDIF
ENDSCAN
SET DELETE ON
SELECT FABRIC
*!*************************************************************
*! Name      : lfwWare1
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : select all warehouses for selected fabric
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfwWare1

SELECT cWareCode     ;  
FROM FabDye          ;                                         
WHERE FABRIC+COLOR+CWARECODE+DYELOT+STR(RECNO(),7)= SUBSTR(laData[1],1,7) + SUBSTR(laData[2],1,6);
     .AND. EMPTY(Dyelot)   ;
INTO ARRAY laWare

lnCount = _TALLY
lcAll   = 'All'
DIMENSION laWare[lnCount+1,1]
=AINS(laWare, lnCount+1)
laWare[lnCount+1] = lcAll
SHOW GET lcWare
* ---------------------------------------------------------------
FUNCTION lfCloseFile
PRIVATE lnCount, lcCount,lcRelation

SELECT (lcRecentlyOpened)
SCAN
  USE IN ALLTRIM(&lcRecentlyOpened..cFile)
ENDSCAN
SELECT (lcAlreadyOpened)
SCAN
  IF !EMPTY(cTag)
    SET ORDER TO TAG ALLTRIM(&lcAlreadyOpened..cTag) IN ALLTRIM(&lcAlreadyOpened..cFile)
    =SEEK(ALLTRIM(&lcAlreadyOpened..cKey),ALLTRIM(&lcAlreadyOpened..cFile))
  ENDIF
  SELECT (ALLTRIM(&lcAlreadyOpened..cFile))
  SET RELATION TO
  FOR lnCount = 1 TO 3
    lcCount = STR(lnCount,1)
    IF !EMPTY(&lcAlreadyOpened..cRefFile&lcCount)
      lcRelation = ALLTRIM(&lcAlreadyOpened..cRefExp&lcCount)
      SET RELATION TO &lcRelation INTO ALLTRIM(&lcAlreadyOpened..cRefFile&lcCount) ADDITIVE
    ENDIF
  ENDFOR
ENDSCAN
USE IN (lcRecentlyOpened)
USE IN (lcAlreadyOpened)
* ---------------------------------------------------------------
FUNCTION lfvDuty
PRIVATE lnAlias,lnOldTag

IF MDOWN()
  RETURN
ENDIF
lnAlias = SELECT()
IF !SEEK(laData[19],"SycCurr")
  lcCurr = laData[19]
    IF !gfcurrbrow(@lcCurr)
	  laData[19] = lcoldvalue
	ELSE
      laData[19] = lcCurr	  
	ENDIF
ENDIF
IF laData[19] = gcBaseCurr
  lnExRateD = 1
  lnUnit2   = 1 

  *E301911,1 ABD - Didn't show word Rate, or Not Valid Or all Date.  [Begin]
  *lcRateD = 'Rate                   :'
  lcRateD = ''
  *E301911,1 ABD - [End]

ELSE
  lnAlias    = SELECT()
  SELECT SYCEXCH
  lnOldTag = TAG()
  SET ORDER TO TAG Currency DESCENDING
  IF !SEEK(gcBaseCurr+laData[19]) OR SYCEXCH.nExRate = 0
    lnExRateD = SYCEXCH.nExRate
    lnUnit2   = 1

    *E301911,1 ABD - Didn't show word Rate, or Not Valid Or all Date.  [Begin]
    *lcRateD = 'No valid rate        :'
    lcRateD  = 'Invalid'
    *E301911,1 ABD - [End]

  ELSE
    lnExRateD = SYCEXCH.nExRate
    =SEEK(laData[19],"SycCurr")
    lnUnit2   = SycCurr.nCurrUnit

    *E301911,1 ABD - Didn't show word Rate, or Not Valid Or all Date.  [Begin]
    *lcRateD = 'Rate on ' + DTOC(SYCEXCH.dRateDate) +' :'
    lcRateD  = LEFT(DTOC(SYCEXCH.dRateDate),5)
    *E301911,1 ABD - [End]

  ENDIF
  SET ORDER TO TAG &lnOldTag
  SELECT(lnAlias)
ENDIF  
lcFrnSmbl2 = IIF(SEEK(laData[19],'syccurr'),syccurr.ccurrsmbl,'')
lcFrn2     = gfGetExSin("",ALLTRIM(laData[19]))
lnBCost2   = IIF(lcFrn2 = '/',ROUND(lnCost2/lnExRateD/lnUnit2,3),ROUND(lnCost2*lnExRateD/lnUnit2,3))
lnBCost3   = IIF(lcFrn2 = '/',ROUND(lnCost3/lnExRateD/lnUnit2,3),ROUND(lnCost3*lnExRateD/lnUnit2,3))
lnBCost4   = IIF(lcFrn2 = '/',ROUND(lnCost4/lnExRateD/lnUnit2,3),ROUND(lnCost4*lnExRateD/lnUnit2,3))
lnCst2P    =  IIF(lnBCost1 = 0,0,(lnBCost2/lnBCost1)*100)
lnCst3P    =  IIF(lnBCost1 = 0,0,(lnBCost3/lnBCost1)*100)
lnCst4P    =  IIF(lnBCost1 = 0,0,(lnBCost4/lnBCost1)*100)    
laData[20] = lnBCost1 + lnBCost2 + lnBCost3 + lnBCost4
laData[21] = (laData[20]/laData[16]) 
laData[22] = IIF(lnOnhand = 0, laData[20], laData[22])
laData[23] = IIF(lnOnhand = 0, laData[21], laData[23])
SHOW GET lnBCost2 
SHOW GET lnBCost3 
SHOW GET lnBCost4 
SHOW GET lnCst2P
SHOW GET lnCst3P
SHOW GET lnCst4P
=lfRefresh()
* ---------------------------------------------------------------
FUNCTION lfvPrice

lnBCost1 = IIF(lcFrn1 = '/',ROUND(lnCost1/lnExRateP/lnUnit1,3),ROUND(lnCost1*lnExRateP/lnUnit1,3))
lnCst2P    =  IIF(lnBCost1 = 0,0,(lnBCost2/lnBCost1)*100)
lnCst3P    =  IIF(lnBCost1 = 0,0,(lnBCost3/lnBCost1)*100)
lnCst4P    =  IIF(lnBCost1 = 0,0,(lnBCost4/lnBCost1)*100)    
laData[20] = lnBCost1 + lnBCost2 + lnBCost3 + lnBCost4
laData[21] = laData[20]/laData[16]
laData[22] = IIF(lnOnhand = 0, laData[20], laData[22])
laData[23] = IIF(lnOnhand = 0, laData[21], laData[23])

SHOW GET lnBCost1 
SHOW GET lnCst2P
SHOW GET lnCst3P
SHOW GET lnCst4P
=lfRefresh()
*!*************************************************************
*! Name      : lfvTax
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Validation for the Tax field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************

FUNCTION lfvTax

lnBCost2 = IIF(lcFrn2 = '/',ROUND(lnCost2/lnExRateD/lnUnit2,3),ROUND(lnCost2*lnExRated/lnUnit2,3))
lnCst2P  = (lnBCost2/lnBCost1)*100
laData[20] = lnBCost1 + lnBCost2 + lnBCost3 + lnBCost4
laData[21] = laData[20]/laData[16]
laData[22] = IIF(lnOnhand = 0, laData[20], laData[22])
laData[23] = IIF(lnOnhand = 0, laData[21], laData[23])
SHOW GET lnBCost2
SHOW GET lnCst2P
=lfRefresh()
*!*************************************************************
*! Name      : lfvFreight
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Validate the freight amount
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvFreight

lnBCost4 = IIF(lcFrn2 = '/',ROUND(lnCost4/lnExRateD/lnUnit2,3),ROUND(lnCost4*lnExRated/lnUnit2,3))
lnCst4P  = (lnBCost4/lnBCost1)*100
laData[20] = lnBCost1 + lnBCost2 + lnBCost3 + lnBCost4
SHOW GET lnBCost4
SHOW GET lnCst4P
laData[21] = laData[20]/laData[16]
laData[22] = IIF(lnOnhand = 0, laData[20], laData[22])
laData[23] = IIF(lnOnhand = 0, laData[21], laData[23])

=lfRefresh()
*!*************************************************************
*! Name      : lfvQuota
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : Validation for the Quota field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvQuota

lnBCost3 = IIF(lcFrn2 = '/',ROUND(lnCost3/lnExRateD/lnUnit2,3),ROUND(lnCost3*lnExRated/lnUnit2,3))
lnCst3P  = (lnBCost3/lnBCost1)*100
laData[20] = lnBCost1 + lnBCost2 + lnBCost3 + lnBCost4
SHOW GET lnBCost3
SHOW GET lnCst3P
laData[21] = laData[20]/laData[16]
laData[22] = IIF(lnOnhand = 0, laData[20], laData[22])
laData[23] = IIF(lnOnhand = 0, laData[21], laData[23])

=lfRefresh()
*!*************************************************************
*! Name      : lfSameClr
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : initialize the color to disable the fields
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfSameClr

lcSingClr    = "192,192,192"
lcSingPar   = "RGB(" + lcSingClr + "," + lcSingClr + ")"
lcInvClr       = lcSingPar 
FOR lnI = 1 TO 9
  lcInvClr = lcInvClr + "," + lcSingPar
ENDFOR
*!*************************************************************
*! Name      : lfvobjlink
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : to link a picture with the fabric
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvobjlink

lnalais = SELECT()
DO getobj WITH 'M',laData[1]
SELECT (lnalais)
RETURN

*!*************************************************************
*! Name      : lfvcDyelot
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : to check if the fabric has already entries using dyelot or not
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvcDyelot
PRIVATE lnRecord
IF cbDyelot <> lcOldDye 
  lnAlias = SELECT()
  SELECT FABRIC
  lnRecord = RECNO()
  IF !llAllColrs
    IF SEEK(laData[1]+laData[2],'FABRIC')
      IF (FABRIC.ONHAND <> 0 .AND. ;
         gfModalGen('QRM36041B36000','ALERT',ALLTRIM(laData[1])+'/'+ALLTRIM(laData[2])+'|'+'quantity on hand')= 1) .OR. ;
        (FABRIC.USAGE <> 0 .AND. ;
         gfModalGen('QRM36041B36000','ALERT',ALLTRIM(laData[1])+'/'+ALLTRIM(laData[2])+'|'+'usage quantity')= 1) .OR. ;
        (FABRIC.ONORDER <> 0 .AND. ;
         gfModalGen('QRM36041B36000','ALERT',ALLTRIM(laData[1])+'/'+ALLTRIM(laData[2])+'|'+'quantity on order')= 1)
        cbDyelot = lcOldDye 
        SHOW GET cbDyelot ENABLE
        GO(lnRecord)
        SELECT(lnAlias)
        RETURN
      ENDIF  
    ENDIF  
  ELSE
    IF SEEK(laData[1],'FABRIC')  
      SCAN WHILE FABRIC.FABRIC = laData[1]
        IF (FABRIC.ONHAND <> 0 .AND. ;
          gfModalGen('QRM36041B36000','ALERT',ALLTRIM(laData[1])+'/'+ALLTRIM(laData[2])+'|'+'quantity on hand')= 1) .OR. ;
          (FABRIC.USAGE <> 0 .AND. ;
          gfModalGen('QRM36041B36000','ALERT',ALLTRIM(laData[1])+'/'+ALLTRIM(laData[2])+'|'+'usage quantity')= 1) .OR. ;
          (FABRIC.ONORDER <> 0 .AND. ;
          gfModalGen('QRM36041B36000','ALERT',ALLTRIM(laData[1])+'/'+ALLTRIM(laData[2])+'|'+'quantity on order')= 1)
          cbDyelot =  lcOldDye 
          SHOW GET cbDyelot ENABLE
          GO(lnRecord)
          SELECT(lnAlias)
          RETURN
        ENDIF  
      ENDSCAN
    ENDIF  
  ENDIF
  *-- HDM B602880,1 Adding check it's not EOF [Start]
  *GO(lnRecord)
  IF !EOF()
    GO(lnRecord)
  ENDIF
  SELECT(lnAlias)
  *-- HDM B602880,1 Adding check it's not EOF [End]
ENDIF  
*!*************************************************************
*! Name      : lfwcDyelot
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : to the old value of dyelot field in a variable
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfwcDyelot

lcOldDye = cbDyelot
*!*************************************************************
FUNCTION lfvlbCost1

IF lnBCost1 < 0
  lnBCost1 = 0
  _CUROBJ = OBJNUM(lnBCost1)
ELSE
  lnCst2P    = IIF(lnBCost1 = 0,0,(lnBCost2/lnBCost1)*100)
  lnCst3P    = IIF(lnBCost1 = 0,0,(lnBCost3/lnBCost1)*100)
  lnCst4P    = IIF(lnBCost1 = 0,0,(lnBCost4/lnBCost1)*100)    
  laData[20] = lnBCost1 + lnBCost2 + lnBCost3 + lnBCost4
  laData[21] = (laData[20]/laData[16]) 
  laData[22] = IIF(lnOnhand = 0, laData[20], laData[22])
  laData[23] = IIF(lnOnhand = 0, laData[21], laData[23])
  =lfRefresh()
ENDIF  
SHOW GET lnCst2P
SHOW GET lnCst3P
SHOW GET lnCst4P
*!*************************************************************
FUNCTION lfvlbCost2

IF lnBCost2 < 0
  lnBCost2 = 0
  _CUROBJ = OBJNUM(lnBCost2)
ELSE
  lnCst2P    =  IIF(lnBCost1 = 0,0,(lnBCost2/lnBCost1)*100)
  laData[20] =  lnBCost1 + lnBCost2 + lnBCost3 + lnBCost4
  laData[21] = (laData[20]/laData[16]) 
  laData[22] = IIF(lnOnhand = 0, laData[20], laData[22])
  laData[23] = IIF(lnOnhand = 0, laData[21], laData[23])
  =lfRefresh()
ENDIF  
IF lnBCost2 = 0
  _CUROBJ = OBJNUM(lnCst2P)
ENDIF
SHOW GET lnCst2P
*!*************************************************************
FUNCTION lfvlbCost3

IF lnBCost3 < 0
  lnBCost3 = 0
  _CUROBJ = OBJNUM(lnBCost3)
ELSE
  lnCst3P    =  IIF(lnBCost1 = 0,0,(lnBCost3/lnBCost1)*100)
  laData[20]  =  lnBCost1 + lnBCost2 + lnBCost3 + lnBCost4
  laData[21]  = (laData[20]/laData[16]) 
  laData[22] = IIF(lnOnhand = 0, laData[20], laData[22])
  laData[23] = IIF(lnOnhand = 0, laData[21], laData[23])
  =lfRefresh()
ENDIF  
IF lnBCost3 = 0
  _CUROBJ = OBJNUM(lnCst3P)
ENDIF
SHOW GET lnCst3P
*!*************************************************************
FUNCTION lfvlbCost4

IF lnBCost4 < 0
  lnBCost4 = 0
  _CUROBJ = OBJNUM(lnBCost4)
ELSE
  lnCst4P    =  IIF(lnBCost1 = 0,0,(lnBCost4/lnBCost1)*100)
  laData[20]  =  lnBCost1 + lnBCost2 + lnBCost3 + lnBCost4
  laData[21]  = (laData[20]/laData[16]) 
  laData[22] = IIF(lnOnhand = 0, laData[20], laData[22])
  laData[23] = IIF(lnOnhand = 0, laData[21], laData[23])
  =lfRefresh()
ENDIF  
IF lnBCost4 = 0
  _CUROBJ = OBJNUM(lnCst4P)
ENDIF
SHOW GETS
*!*************************************************************
FUNCTION lfvKFPrice

laData[18] = PADR('?',5)
=lfvFPrice()
*!*************************************************************
FUNCTION lfvKDuty

laData[19] = PADR('?',5)
=lfvDuty()
*!*************************************************************
FUNCTION lfwBrowse

glFromBrow = .T.
*!*************************************************************
*! Name      : lfvNewClr
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1996
*! Purpose   : add new color for the fabric
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvNewClr

SELECT CODES
SET ORDER TO TAG Ccode_no
*--HDM B802100,1 04/19/1999 [Start] Fixing bug color duplication
*SELECT cCode_no+"-"+cDiscrep ;
    FROM CODES ;
    WHERE cFld_Name = 'COLOR' AND cRltField = 'N';
    INTO ARRAY laSource

*C102467,1 ABD - Restrict adding "Obsolete" colors to style. [Begin]
*SELECT DISTINCT cCode_no+"-"+cDiscrep ;
*    FROM CODES ;
*    WHERE cFld_Name = 'COLOR' AND cRltField = 'N';
*    AND   cDefCode = 'N';
*    INTO ARRAY laSource

PRIVATE lcTmpCodes , llTmpCodes , lcOldOrder
*-- Open the codes file with anther alias.
lcTmpCodes = gfTempName()
llTmpCodes =gfOpenFile(gcDataDir+'CODES',gcDataDir+'Ccode_no','SH',@lcTmpCodes,.T.) 
GOTO TOP
SELECT CODES
lcOldOrder = ORDER()
SET ORDER TO Idrltfname
lnLenSour = 0
= SEEK("NNCOLOR")
*-- Scan and get colors with Obsolete No only.
SCAN REST WHILE CDEFCODE + cRltField + cFld_Name = "NNCOLOR"
  *-- Add Colors to the array that llObsolete with 'N'.
  *-- cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam
  IF !SEEK('N'+'COLOR     '+ccode_no+SPACE(30)+'LLOBSOLETE',lcTmpCodes) .OR. ALLTRIM(&lcTmpCodes..cRltd_Vlu) = "F"
    DECLARE laSource[lnLenSour + 1]
    laSource[ALEN(laSource)] = cCode_no+"-"+cDiscrep
    lnLenSour = lnLenSour +1
  ENDIF
ENDSCAN
*-- Close code file.
IF llTmpCodes
  = gfCloseFile(lcTmpCodes)
ENDIF

SELECT CODES
SET ORDER TO &lcOldOrder




IF TYPE('laSource') = 'U'
  *- Message Text   :- All the system colors are Obsolete, Can not add new color(s).
  *- Message Number :- 42224.
  *- button message :- OK.
  *- button Number  :- 42001
  = gfModalGen('INM42224B42001','DIALOG','the system'+'|'+'add new color(s).')
  RETURN
ENDIF
*C102467,1 ABD - [End]  
*--HDM B802100,1 04/19/1999 [End]

SELECT (lcTempClr)
GO TOP
IF !EOF()
  SELECT Color+"-"+gfCodDes(Color, 'COLOR') FROM (lcTempClr);
         INTO ARRAY laTarget
ELSE
  DIME laTarget[1]
  laTarget=' '
ENDIF
LOCATE FOR cstatus <> 'A'
IF FOUND()
  SELECT Color+"-"+gfCodDes(Color, 'COLOR') FROM (lcTempClr);
      WHERE cstatus <> 'A' INTO ARRAY laOldTrAr
ELSE
  DIME laOldTrAr[1]
  laOldTrAr=' '
ENDIF
=ASORT(laSource)
=ASORT(laTarget)
=gfMover(@laSource,@laTarget,"Select Colors",.T.,'lfValdClr')

SELECT (lcTempClr)
DELETE ALL FOR cstatus='A'
lcName = SPACE(50)
laColor[1,1] = 'CLRLNAME'
laColor[1,2] = 'lcName'
IF !(ALEN(latarget)=1 AND EMPTY(latarget[1]))
  FOR I=1 TO ALEN(laTarget)
    lcColorCode=SUBSTR(laTarget[I],1,6)
    IF !SEEK(lcColorCode)
     	glupdated=.T.
  	  APPEND BLANK
      REPLACE COLOR   WITH lccolorcode, ;
              Fabric  WITH laData[1]  ,;
              cstatus WITH 'A'
      =gfRltFld(&lcTempClr..COLOR, @laColor, 'COLOR')
      REPLACE cData    WITH gfCodDes(&lcTempClr..COLOR, 'COLOR'),;
              ClrLName WITH lcName 
      lcName = SPACE(50)        
    ENDIF
  ENDFOR
ENDIF  
GO TOP
SHOW WINDOW (lcClrTtl) REFRESH
_CUROBJ = OBJNUM(pbCNew)
RETURN
*!*************************************************************
FUNCTION lfValdClr
PARAMETERS lnOption

DO CASE
  CASE lnOption=3
  IF ASCAN('laOldTrAr',ALLTRIM(laTarget[lsTarget])) > 0
        =gfModalGen('QRM36042B36000','ALERT')
      RETURN .F.
    ENDIF
  CASE lnOption=4 AND laScrMode[3]
        =gfModalGen('QRM36043B36000','ALERT')
    RETURN .F.
ENDCASE  

*******************************
PROCEDURE lpNxtScr

IF llAllColrs
  IF laData[1] = FABRIC.FABRIC
    DO WHILE laData[1] = FABRIC.FABRIC AND !EOF()
      SKIP       
    ENDDO
  ENDIF  
ENDIF
laData[1] = FABRIC.FABRIC
laData[2] = FABRIC.COLOR
SELECT(lcTempClr)
ZAP
=lfBColor()
*!*************************************************************

PROCEDURE lpPrvScr

IF llAllColrs
  IF laData[1] = FABRIC.FABRIC
    DO WHILE laData[1] = FABRIC.FABRIC AND !BOF()
      SKIP -1       
    ENDDO
  ENDIF  
ENDIF
laData[1] = FABRIC.FABRIC
laData[2] = FABRIC.COLOR
SELECT(lcTempClr)
ZAP
=lfBColor()
*!*************************************************************
FUNCTION lfTrapKeys
IF WONTOP() = lcClrTtl .OR. WONTOP() = lcSWinTtl
  ON KEY LABEL TAB        DO lpTab
  ON KEY LABEL BACKTAB    DO lpShiftTab
  ON KEY LABEL ESC        DO gfCPClose
ENDIF  
*!*************************************************************
PROCEDURE lpTab

DO CASE
  CASE !WONTOP(lcClrTtl) AND lnActFolder = 3
    ACTIVATE WINDOW (lcClrTtl)
  CASE WONTOP(lcClrTtl)
    IF (laScrMode[3] OR laScrMode[4]) AND llAllColrs
      ACTIVATE WINDOW (lcWinCC)
      _CUROBJ = OBJNUM(pbCNew)
    ELSE
      ACTIVATE WINDOW gwcContrl1
    ENDIF
  CASE !WONTOP(lcSWinTtl) AND lnActFolder = 5
    ACTIVATE WINDOW (lcSWinTtl)
  CASE WONTOP(lcSWinTtl)
    ACTIVATE WINDOW gwcContrl1
  OTHERWISE  
    _CUROBJ = _CUROBJ + 1
ENDCASE
*!*************************************************************
PROCEDURE lpShiftTab

DO CASE
  CASE WONTOP(lcClrTtl) 
    ACTIVATE WINDOW (lcFolder)
    IF laScrMode[2] AND !llAllColrs
      _CUROBJ = OBJNUM(ibFolder(5))
    ELSE
      _CUROBJ = IIF(llCostPrv,OBJNUM(ibFolder(4)),OBJNUM(ibFolder(3)))
    ENDIF
  CASE WONTOP (lcSWinTtl) 
    ACTIVATE WINDOW (lcFolder)
    _CUROBJ = OBJNUM(ibFolder(5))
ENDCASE

*!*************************************************************
FUNCTION lfwCurrency
PARAMETER lcCurrency

IF lcCurrency = 'P'
  lcOldValue = laData[18]
ELSE
  lcOldValue = laData[19]
ENDIF
*!*************************************************************
FUNCTION lfClearTrap

IF WONTOP() <> lcClrTtl .AND. WONTOP() <> lcSWinTtl
   ON KEY LABEL ESC &lcOldEscTrap
   ON KEY LABEL TAB
   ON KEY LABEL BACKTAB
ENDIF   

****************************************************************************
*B802838,1 AMM Change its name and make a new one with the same name
*FUNCTION lfvStyles
FUNCTION XlfvStyles
*B802838,1 AMM end
PRIVATE lcStyTag

*E301077,81 (Start)
llStyleOpen = gfOpenFile(gcDataDir+'Style',gcDataDir+'Style','SH')
*E301077,81 (End)

IF lnCPos = 0
  = lfGetStInf()
  *lnCLen
ENDIF

lnAlias = SELECT()
IF EMPTY(laData[1]) OR EMPTY(laData[2]) OR llAllColrs
  RETURN
ENDIF
IF USED(lcStyTemp)
  SELECT (lcStyTemp)
  ZAP
ENDIF
SELECT BOM
lcOldTag = TAG()
SET ORDER TO TAG BOMITEM
*HDM B602107,1[START]
*IF !FOUND()
*  SEEK ('2'+laData[1])
*ENDIF
*IF !EOF()

IF (SEEK (lcFabElmnt+laData[1])) OR (SEEK (lcMFabElmt+laData[1])) OR (SEEK (lcMTrimEl+laData[1])) OR (SEEK (lcTrimElm+laData[1])) 
  *  SCAN WHILE ITEM=laData[1]
  *HDM B602107,1[END]

  SCAN FOR ITEM = laData[1]
    IF (ICLR <> laData[2]) .AND. (ICLR <> '******')
      LOOP
    ENDIF
    SELE STYLE
    lcStyTag = TAG()
    SET ORDER TO TAG cSTYLE
    IF !SEEK (BOM.cItmMajor)
      SELE BOM
      LOOP
    ELSE
    
      *.OR.  cItmMajor = STYLE.CSTYMAJOR;

      IF OCCURS('*',BOM.cItmMask) > 0
        SET ORDER TO TAG STYLE
        *--HDM B602692,1 [Start] Modify scan expresion to exclude color if found in major segment
        *SCAN WHILE STYLE.cStyMajor = BOM.cItmMajor ;
             FOR PADR(SUBSTR(ALLTRIM(BOM.CITMMASK),1,ATC('*',BOM.CITMMASK)-1)+LADATA[2],19) = STYLE.STYLE ;
             .OR.  LIKE(STRTRAN(BOM.cItmMask,'*','?'),STYLE.STYLE) ;
             .AND. SUBSTR(STYLE.STYLE,lnCPos,lnCLen)=laData[2]
        *B603071,1 Adjust the color length in the scan expr.
        *SCAN WHILE STYLE.cStyMajor = BOM.cItmMajor ;
             FOR PADR(SUBSTR(ALLTRIM(BOM.CITMMASK),1,ATC('*',BOM.CITMMASK)-1)+LADATA[2],19) = STYLE.STYLE ;
             .OR.  LIKE(STRTRAN(BOM.cItmMask,'*','?'),STYLE.STYLE) ;
             .AND. IIF(lnCPos = 0,BOM.cItmMajor = STYLE.CSTYMAJOR ,;
                       SUBSTR(STYLE.STYLE,lnCPos,lnCLen)=laData[2])
        SCAN WHILE STYLE.cStyMajor = BOM.cItmMajor ;
             FOR PADR(SUBSTR(ALLTRIM(BOM.CITMMASK),1,ATC('*',BOM.CITMMASK)-1)+lcSepr+LADATA[2],19) = STYLE.STYLE ;
             .OR.  LIKE(STRTRAN(BOM.cItmMask,'*','?'),STYLE.STYLE) ;
             .AND. IIF(lnCPos = 0,BOM.cItmMajor = STYLE.CSTYMAJOR ,;
                       SUBSTR(STYLE.STYLE,lnCPos,lnCLen)=LEFT(laData[2],lnCLen))                       
        *--HDM B602692,1 [End]


          SELECT (lcStyTemp)
          IF !SEEK(STYLE.STYLE)
            APPEND BLANK
          ENDIF
          REPLACE cItmMajor  WITH STYLE.cStyMajor,;
                  cItmMask   WITH STYLE.Style    ,;
                  DESC       WITH STYLE.Desc1    ,;
                  UOM        WITH BOM.UOM        ,;
                  nbomtotqty WITH BOM.nbomtotqty,;
                  Typ        WITH BOM.Typ
        ENDSCAN
      ELSE
        *--HDM B602326,1 Incorrect Index in style file[start]
        SET ORDER TO TAG STYLE
        *--HDM B602326,1 Incorrect Index in style file[end]
        IF !SEEK(BOM.cItmMask)
          SELECT BOM
          LOOP
        ELSE
          SELE (lcStyTemp)
          IF !SEEK(BOM.cItmMask)
            APPEND BLANK
          ENDIF  
          REPLACE cItmMajor  WITH STYLE.cStyMajor,;
                  cItmMask   WITH STYLE.Style    ,;
                  DESC       WITH STYLE.Desc1    ,;
                  UOM        WITH BOM.UOM        ,;
                  nbomtotqty WITH BOM.nbomtotqty ,;
                  Typ        WITH BOM.TYP
        ENDIF
      ENDIF  
    ENDIF
  ENDSCAN  
  SELE STYLE
  lcStyTag = TAG()
ENDIF

*E301077,81 (Start)
IF llStyleOpen
  =gfCloseFile('Style')
ENDIF  
*E301077,81 (End)

SELECT(lnAlias)
*!*************************************************************
*! Name    : gfCPBrows()
*! Purpose : ToolBar Browse Icon Valid function.
*!*************************************************************
FUNCTION gfCPBrows
PRIVATE lcBrFields,lnAlias,laTempData

lnAlias=SELECT()
SELECT FABRIC
IF llAllColrs
  SET ORDER TO TAG CFABRIC
ENDIF
*E301967,1 KHM 07/31/2002 (Begin) Adding the content field to the fabric browse.
*lcBrFields = IIF(llAllColrs,[FABRIC:07:H="Fabric",],[FABRIC:07:H="Fabric",])+;
             IIF(llAllColrs,[Color="******":6,],[Color:06:H="Color",])+;     
             [Desc:13:H="Description",]+;
             [Vendor:8:H="Vendor",Pattern:10:H="Pattern"]

lcBrFields = IIF(llAllColrs,[FABRIC:07:H="Fabric",],[FABRIC:07:H="Fabric",])+;
             IIF(llAllColrs,[Color="******":6,],[Color:06:H="Color",])+;     
             [Desc:13:H="Description",]+;
             [Vendor:8:H="Vendor",Pattern:10:H="Pattern",Content:60:H="Contents"]
*E301967,1 KHM 07/31/2002 (End)

DIME laTempData[1]
STORE '' TO laTempData
llShowData = gfBrows(.f.,'FABRIC')

SELECT FABRIC
SET ORDER TO TAG FABRIC  
SELECT(lnalias)
IF llShowData
  SHOW GETS
ENDIF  
RETURN

****************************************************************************
FUNCTION lfvQuality

lcQuality = laGrade[lnQuality,2]
DO CASE
  CASE lcQuality = '1'
    lcQua1 = "Second Quality"
    lcQua2 = "Damage"
    DIMENSION laQuality[2,3]
    laQuality[1,1] = "Second Quality"
    laQuality[2,1] = "Damage"
    
  CASE lcQuality = '2'
    DIMENSION laQuality[2,3]
    laQuality[1,1] = "First Quality"
    laQuality[2,1] = "Damage"
    lcQua1 = "First Quality"
    lcQua2 = "Damage"

  CASE lcQuality = '3'
    DIMENSION laQuality[2,3]
    laQuality[1,1] = "First Quality"
    laQuality[2,1] = "Second Quality"
    lcQua1 = "First Quality"
    lcQua2 = "Second Quality"
ENDCASE
laQuality[1,2] = SPACE(07)
laQuality[2,2] = SPACE(07)
laQuality[1,3] = SPACE(06)
laQuality[2,3] = SPACE(06)
lcQuaClr       = SPACE(06)
lnQuaFab       = 1
SHOW GET lnQuaFab ENABLE
=lfRefresh()
****************************************************************************
FUNCTION lfvGrade

lcQuaClr = laQuality[lnQuaFab,3]
SHOW GET lcQuaClr
****************************************************************************
FUNCTION lfvQuaFab
PARAMETER lcQuaFab
PRIVATE lnQuRecNo

IF MDOWN()
  RETURN
ENDIF
*--HDM B602180,1 Wrong Quality applied[start]
IF lcQua&lcQuaFab = "First Quality"
  lcBrGrad = '1'
ENDIF

IF lcQua&lcQuaFab = "Second Quality"
  lcBrGrad = '2'
ENDIF

IF lcQua&lcQuaFab = "Damage"
  lcBrGrad = '3'
ENDIF
*--HDM B602180,1 Wrong Quality applied[end]

lnQuRecNo = IIF(EOF('FABRIC'),0,RECNO('FABRIC'))
IF !EMPTY(lcFabric&lcQuaFab) AND !SEEK(lcFabric&lcQuaFab,'FABRIC')
  *--HDM B602180,1 Wrong Quality applied[start]
  *DO FABROW WITH lcFabric&lcQuaFab,'*'
  
  DO FABROW WITH lcFabric&lcQuaFab,'*',.F.,"FOR FABRIC.cFabGrade = '"+ lcBrGrad +"'"
  *--HDM B602180,1 Wrong Quality applied[end]
ENDIF
IF !EMPTY(lcFabric&lcQuaFab)
  =SEEK(lcFabric&lcQuaFab,'FABRIC')
  DO CASE
    CASE lnQuality = 1
      IF (lcQuaFab = '1' AND FABRIC.cFabGrade <> '2') OR ;
         (lcQuaFab = '2' AND FABRIC.cFabGrade <> '3')
        *=gfDialog("I","The fabric quality does not match the chosen quality")
        lcFabric&lcQuaFab = SPACE(07)
        IF lnQuRecNo = 0
          GOTO TOP IN FABRIC
        ELSE
          GOTO lnQuRecNo IN FABRIC
        ENDIF  
        RETURN
      ENDIF   
    CASE lnQuality = 2
      IF (lcQuaFab = '1' AND FABRIC.cFabGrade <> '1') OR ;
         (lcQuaFab = '2' AND FABRIC.cFabGrade <> '3')
        *=gfDialog("I","The fabric quality does not match the chosen quality")
        lcFabric&lcQuaFab = SPACE(07)
        IF lnQuRecNo = 0
          GOTO TOP IN FABRIC
        ELSE
          GOTO lnQuRecNo IN FABRIC
        ENDIF  
        RETURN
      ENDIF   
    CASE lnQuality = 3
      IF (lcQuaFab = '1' AND FABRIC.cFabGrade <> '1') OR ;
         (lcQuaFab = '2' AND FABRIC.cFabGrade <> '2')
        *=gfDialog("I","The fabric quality does not match the chosen quality")
        lcFabric&lcQuaFab = SPACE(07)
        IF lnQuRecNo = 0
          GOTO TOP IN FABRIC
        ELSE
          GOTO lnQuRecNo IN FABRIC
        ENDIF  
        RETURN
      ENDIF   
  ENDCASE
ENDIF
_CUROBJ = OBJNUM(lcColor&lcQuaFab)
IF lnQuRecNo = 0
  GOTO TOP IN FABRIC
ELSE
  GOTO lnQuRecNo IN FABRIC
ENDIF  
****************************************************************************
FUNCTION lfvKQFab
PARAMETER lcObjNum

lcFabric&lcObjNum = PADR('?',7)
=lfvQuaFab(lcObjNum)
SHOW GET lcFabric&lcObjNum ENABLE
*!*************************************************************
*! Name      : lfvQuaClr
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1997
*! Purpose   : Validate the entered color for the two other qualities
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvQuaClr
PARAMETER lcQuaFab
PRIVATE lcOldQuaFab

IF MDOWN()
  RETURN
ENDIF  
*--HDM B602180,1 Wrong Quality applied[start]
IF lcQua&lcQuaFab = "First Quality"
  lcBrGrad = '1'
ENDIF

IF lcQua&lcQuaFab = "Second Quality"
  lcBrGrad = '2'
ENDIF

IF lcQua&lcQuaFab = "Damage"
  lcBrGrad = '3'
ENDIF
*--HDM B602180,1 Wrong Quality applied[end]

lnQuRecNo = IIF(EOF('FABRIC'),0,RECNO('FABRIC'))
DO CASE
  CASE EMPTY(lcFabric&lcQuaFab) AND !EMPTY(lcColor&lcQuaFab)
    DO FABROW WITH lcFabric&lcQuaFab,'*',.F.,"FOR FABRIC.cFabGrade = '"+ lcBrGrad +"'"

  CASE !EMPTY(lcFabric&lcQuaFab) AND !EMPTY(lcColor&lcQuaFab) AND !SEEK(lcFabric&lcQuaFab+lcColor&lcQuaFab,'Fabric')
    lcOldQuaFab = lcFabric&lcQuaFab
    lcBrowItm   = lcFabric&lcQuaFab
    lcBrowClr   = CHR(240)    
    DO FABROW WITH lcBrowItm,lcBrowClr
    lcFabric&lcQuaFab = lcBrowItm  
    lcColor&lcQuaFab  = lcBrowClr
    IF EMPTY(lcFabric&lcQuaFab)
      lcFabric&lcQuaFab = lcOldQuaFab
    ENDIF
ENDCASE
IF !EMPTY(lcFabric&lcQuaFab) AND !EMPTY(lcColor&lcQuaFab)
  =SEEK(lcFabric&lcQuaFab+lcColor&lcQuaFab,'FABRIC')
  DO CASE
    CASE lnQuality = 1
      IF (lcQuaFab = '1' AND FABRIC.cFabGrade <> '2') OR ;
         (lcQuaFab = '2' AND FABRIC.cFabGrade <> '3')
        =gfDialog("I","The fabric quality does not match the chosen quality")
        lcFabric&lcQuaFab = SPACE(07)
        IF lnQuRecNo = 0
          GOTO TOP IN FABRIC
        ELSE
          GOTO lnQuRecNo IN FABRIC
        ENDIF  
        RETURN
      ENDIF   
    CASE lnQuality = 2
      IF (lcQuaFab = '1' AND FABRIC.cFabGrade <> '1') OR ;
         (lcQuaFab = '2' AND FABRIC.cFabGrade <> '3')
        =gfDialog("I","The fabric quality does not match the chosen quality")
        lcFabric&lcQuaFab = SPACE(07)
        IF lnQuRecNo = 0
          GOTO TOP IN FABRIC
        ELSE
          GOTO lnQuRecNo IN FABRIC
        ENDIF  
        RETURN
      ENDIF   
    CASE lnQuality = 3
      IF (lcQuaFab = '1' AND FABRIC.cFabGrade <> '1') OR ;
         (lcQuaFab = '2' AND FABRIC.cFabGrade <> '2')
        =gfDialog("I","The fabric quality does not match the chosen quality")
        lcFabric&lcQuaFab = SPACE(07)
        IF lnQuRecNo = 0
          GOTO TOP IN FABRIC
        ELSE
          GOTO lnQuRecNo IN FABRIC
        ENDIF  
        RETURN
      ENDIF   
  ENDCASE
ENDIF
IF !EMPTY(lcFabric&lcQuaFab) AND EMPTY(lcColor&lcQuaFab)
  lcColor&lcQuaFab = '******'
ENDIF
IF lnQuRecNo = 0
  GOTO TOP IN FABRIC
ELSE
  GOTO lnQuRecNo IN FABRIC
ENDIF  
*!*************************************************************
*! Name      : lfwField
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1997
*! Purpose   : called from the when of every get filed to get the old value in a variable
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Modifications :
*!           HDM B602174,1 11/11/1998 Make All Codes Sorted Alpha.
*!           
*!*************************************************************
FUNCTION lfwField

*=ASORT(laType,lnType)
lcOldData = EVALUATE(SYS(18))
*!*************************************************************
*! Name      : lfvField
*! Developer : Samah Wilson Kirollos
*! Date      8 02/11/1997
*! Purpose   : called from the valid of every get filed the show if there is any changes or not
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvField
PARAMETER lcField

*E301911,1 ABD - Didn't show the lcWinch11 in case we show lcWinCh1.  [Begin]
IF 'laData[15]' $ lcField .AND. !MDOWN()
  IF llMultCur .AND. lnDomestic = 2
    SHOW GETS WINDOW (lcWinCh8) DISABLE ONLY
    SHOW WINDOW (lcWinCh1) TOP
    laFoldWinds[4,2] = "lcWinCh1"
  ELSE
    SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
    SHOW WINDOW (lcWinCh8) TOP
    laFoldWinds[4,2] = "lcWinCh8"
  ENDIF
  = lfRefresh()
ENDIF

*E301911,1 ABD - [End]

*IF LASTKEY() = 13
  IF &lcField <> lcOldData
    lcSFields = lcSFields +','+lcField
  ENDIF
*ENDIF
*!*************************************************************
*! Name      : lfvBomCost
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1997
*! Purpose   : get the cost fields from the bom file
*!*************************************************************
*! Calls     : lpOptions 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvBomCost
PRIVATE lnAlias

lnAlias = SELECT()
STORE 0 TO lnBCOST1,lnBCOST2,lnBCOST3,lnBCOST4
SELECT BOM
SET ORDER TO TAG BOM
IF SEEK(laData[1])
  SCAN WHILE BOM.citmMajor = laData[1]
    DO CASE
      CASE TYP = '1'
        lnBCOST1 = lnBCOST1 + TOTCOST
      CASE TYP = '2'
        lnBCOST2 = lnBCOST2 + TOTCOST
      CASE TYP = '3'
        lnBCOST3 = lnBCOST3 + TOTCOST
      CASE TYP = '4'
        lnBCOST4 = lnBCOST4 + TOTCOST
    ENDCASE
  ENDSCAN 
ENDIF
lnCst2P = IIF(lnBCost1 = 0,0,(lnBCost2/lnBCost1)*100)
lnCst3P = IIF(lnBCost1 = 0,0,(lnBCost3/lnBCost1)*100)
lnCst4P = IIF(lnBCost1 = 0,0,(lnBCost4/lnBCost1)*100)
laData[20] =  lnBCost1 + lnBCost2 + lnBCost3 + lnBCost4
laData[21] = (laData[20]/laData[16]) 
laData[22] = IIF(lnOnhand = 0, laData[20], laData[22])
laData[23] = IIF(lnOnhand = 0, laData[21], laData[23])
lcSFields = lcSFields +','+'lnBcost1'+'lnBcost2'+'lnBcost3'+'lnBcost4'
SELECT(lnAlias)
*!*************************************************************
*! Name      : lfActPad
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1997
*! Purpose   : Activate the popup menu option
*!*************************************************************
*! Calls     : lpOptions 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfActPad

DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
DEFINE POPUP _OPTIONPOP MARGIN SHADOW

*B606663,1 AMH Define this bar in case of keep track of rolls only [Start]
*DEFINE BAR 1 OF _OPTIONPOP PROMPT 'View transactions by roll' SKIP FOR EMPTY(laData[2]) OR !laScrMode[2] OR llAllColrs OR !laData[14]
IF llTrkRolls
  DEFINE BAR 1 OF _OPTIONPOP PROMPT 'View transactions by roll' SKIP FOR EMPTY(laData[2]) OR !laScrMode[2] OR llAllColrs OR !laData[14]
ENDIF
*B606663,1 AMH [End]

*C102684,1 (Begin) Add 'Vendor Reference' bar
IF llVenRef
  
  *B606663,1 AMH Get the correct bar number [Start]
  *DEFINE BAR 2 OF _OPTIONPOP PROMPT 'Vendor Reference' SKIP FOR !laScrMode[2] AND !laScrMode[1]
  lnBarNo = 1
  IF llTrkRolls
    lnBarNo = 2
  ENDIF
  DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT 'Vendor Reference' SKIP FOR !laScrMode[2] AND !laScrMode[1]
  *B606663,1 AMH [End]
  
  *C102684,1 (Begin) Add a custom bar 'Vendor On Order' for Bratex.
  IF ASCAN(laEvntTrig , PADR('ADDBAR',10)) <> 0
    =gfDoTriger('MAMATRL',PADR('ADDBAR',10))
  ENDIF
  *C102684,1 (End)
ENDIF  
*C102684,1 (End)

ON SELECTION POPUP _OPTIONPOP DO lpOptions WITH BAR()
ON KEY LABEL ALT+P ACTIVATE POPUP _OPTIONPOP

*!*************************************************************
*! Name      : lpOptions
*! Developer : Samah Wilson Kirollos
*! Date      : 02/11/1997
*! Purpose   : Call the popup option 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
PROCEDURE lpOptions
PARAMETERS lnBar

DO CASE
  CASE lnBar = 1
    
    *B606663,1 AMH Do the correct procedure [Start]
    *llTranRoll = !llTranRoll
    *SET MARK OF BAR 1 OF _OPTIONPOP TO llTranRoll
    IF llTrkRolls
      llTranRoll = !llTranRoll
      SET MARK OF BAR 1 OF _OPTIONPOP TO llTranRoll
    ELSE
      DO lpVendFab
    ENDIF
    *B606663,1 AMH [End]
    
  *C102684,1 (Begin) Add 'Vendor Reference' bar
  CASE lnBar = 2
    
    *B606663,1 AMH Do the correct procedure [Start]
    *DO lpVendFab
    IF llTrkRolls
      DO lpVendFab
    ELSE
      IF ASCAN(laEvntTrig , PADR('RUNSCR',10)) <> 0
        =gfDoTriger('MAMATRL',PADR('RUNSCR',10))
      ENDIF
    ENDIF
    *B606663,1 AMH [End]
    
  *C102684,1 (Begin) Add a custom bar 'Vendor On Order' for Bratex.
  CASE lnBar = 3
    IF ASCAN(laEvntTrig , PADR('RUNSCR',10)) <> 0
      =gfDoTriger('MAMATRL',PADR('RUNSCR',10))
    ENDIF
  *C102684,1 (End)
ENDCASE
*!*************************************************************
*! Function : lfvObjLnk
*! Developer : Samah Wilson Salib
*! Date      : 08/11/98
*! Purpose   : Object link ,Material picture.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvObjLnk

lnAlias=SELECT()
DO GetObj WITH 'M',laData[1]
SELECT(lnalias)
RETURN
*!*************************************************************
*!  Function : lfDyeRel
*! Developer : Samah Wilson Salib
*! Date      : 08/18/98
*! Purpose   : Arrange dyelots
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*E300954,1 SWK 08/09/1998
FUNCTION lfDyeRel

=gfArDyRl(laData[1],IIF(llAllColrs,'',laData[2]),'',IIF(laScrMode[3],.T.,.F.))

*!*************************************************************
FUNCTION lfvPurCd

lcPurCd = laPurc[lnPurc,2]
*!*******************************************************************

*!*************************************************************
*!  Function : lfGetStInf
*! Developer : Hossam El Etreby [HDM]
*! Date      : 03/02/1999
*! Purpose   : Get color positionnin style structure
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetStInf()
*!*************************************************************
*! Due to Bug B602606,1
*!*************************************************************

FUNCTION lfGetStInf

DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

lnMajSeg  = gfItemMask('SM')
FOR lnC = 1 TO ALEN(laMajSeg,1)
  IF laMajSeg[lnC,1] = 'C'
    lnCPos = IIF(lnC <= lnMajSeg , 0 , laMajSeg[lnC,4])
    lnCLen = IIF(lnC <= lnMajSeg , 0 ,LEN(laMajSeg[lnC,3]))
  ENDIF
ENDFOR


*!*************************************************************
*! Name      : lfvStyles
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 12/01/1999
*! Purpose   : browse the Styles for the selected fabric/color
*!*************************************************************
*! Calls     : lfGetStInf(), gfItemMask()
*! Reference : B802838,1 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************

FUNCTION lfvStyles
PRIVATE lcStyTag

lnAlias = SELECT()
llStyleOpen = gfOpenFile(gcDataDir+'Style',gcDataDir+'Style','SH')
IF lnCPos = 0
  = lfGetStInf()
ENDIF
IF EMPTY(laData[1]) OR EMPTY(laData[2]) OR llAllColrs
  RETURN
ENDIF
*B802838,1 AMM Check if the data already exist, don't recollect
IF !EOF(lcStyTemp)
  RETURN
ENDIF
*B802838,1 AMM Get Major length
lnMajLen = LEN(gfItemMask('PM'))
SELECT BOM
lcOldTag = ORDER()
SET ORDER TO TAG BOMITEM

*B605854,1 ABD Fix problem that the Style folder Not showing assigned style
*B605854,1 ABD that we assign in the style cost sheet that happen because
*B605854,1 ABD The setup file contain 2 fabrics in the IC setpup module. 
*B605854,1 ABD so I will remark the next lines.[Begin]
*IF (SEEK (lcFabElmnt+laData[1]))
*! B604863,1 MHM 10/14/2001 Add parameter to function to check Manifactured or not [Start]
*= lfAddStRec(lcFabElmnt)
*  = lfAddStRec(lcFabElmnt,'P')
*! B604863,1 MHM [End]
*ENDIF
*IF (SEEK (lcMFabElmt+laData[1]))
*! B604863,1 MHM 10/14/2001 Add parameter to function to check Manifactured or not [Start]
*= lfAddStRec(lcMFabElmt)
* = lfAddStRec(lcMFabElmt,'M')
*! B604863,1 MHM [End]  
*ENDIF

PRIVATE     InArrayNo
STORE 0  TO InArrayNo
*-- This loop for imported styles.
FOR InArrayNo = 1 To ALEN(laIFabNo,1)
  IF (SEEK (laIFabNo[InArrayNo]+laData[1]))
    = lfAddStRec(laIFabNo[InArrayNo],'P')
  ENDIF
ENDFOR

*- this loop for manfactrin style.
STORE 0  TO InArrayNo
FOR InArrayNo = 1 To ALEN(laMFabNo,1)
  IF (SEEK (laMFabNo[InArrayNo]+laData[1]))
    = lfAddStRec(laMFabNo[InArrayNo],'M')
  ENDIF
ENDFOR
*B605854,1 ABD [End]

IF (SEEK (lcMTrimEl+laData[1]))

*! B604863,1 MHM 10/14/2001 Add parameter to function to check Manifactured or not [Start]
  *= lfAddStRec(lcMTrimEl)
  = lfAddStRec(lcMTrimEl,'M')
*! B604863,1 MHM [End]  

ENDIF
IF (SEEK (lcTrimElm+laData[1]))

*! B604863,1 MHM 10/14/2001 Add parameter to function to check Manifactured or not [Start]
  *= lfAddStRec(lcTrimElm)
  = lfAddStRec(lcTrimElm,'P')
*! B604863,1 MHM [End]

ENDIF
SET ORDER TO TAG (lcOldTag) IN BOM

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfAddStRec
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 12/01/1999
*! Purpose   : browse the Styles for the selected fabric/color
*!*************************************************************
*! Calls     : None
*! Reference : B802838,1 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************

FUNCTION lfAddStRec
PARAMETER lcItTyp , lcMaTyp
*B803292,1 WAB - (START) Intialiase Private variables
*--- lnAlias   ---> Hold the currect alias file
*--- lnSclPos  ---> Hold the scale position in case of extended Size scale
*--- lcSeekExpr---> Hold The seek Experetion in case of (extended size scale/normal size scale) 
PRIVATE lnAlias,lnSclPos,lcExtScl,lcSeekExpr
*B803292,1 WAB - (END)

*! B604863,1 MHM 10/14/2001 Add Variable to check it[Start]
llCheck  = (lcMaTyp = 'M')
*! B604863,1 MHM [End]

SCAN WHILE typ+item+iclr+citmmajor+citmmask = lcItTyp+laData[1] FOR (ICLR = laData[2] .OR. ICLR = '******')

  IF !SEEK (LEFT(BOM.cItmMajor,lnMajLen),'Style')
    LOOP
  ELSE
    *! B604863,4 MHM 10/16/2001 Check for style [Start]
    IF Style.Make = llCheck
    *! B604863,4 MHM 10/16/2001 [End]

    IF OCCURS('*',BOM.cItmMask) > 0
      IF ICLR = '******'
        *B803292,1 WAB - (START) change the seek expretion in case of extende size scale to seek a part of the style
        *IF SEEK(PADR(SUBSTR(ALLTRIM(BOM.CITMMASK),1,ATC('*',BOM.CITMMASK)-1)+LADATA[2],19),'STYLE' )
        IF llExtSS
          *--get the scale from Bom.cItmMask to add it to the seek experation
          lnSclPos = OCCURS('*',BOM.cItmMask) + AT('*',BOM.cItmMask)
          lcExtScl = SUBSTR(BOM.cItmMask,lnSclPos,3)
          lcSeekExpr = "ALLTRIM(SUBSTR(ALLTRIM(BOM.CITMMASK),1,ATC('*',BOM.CITMMASK)-1)+LADATA[2]+lcExtScl)"
        ELSE
          lcSeekExpr = "PADR(SUBSTR(ALLTRIM(BOM.CITMMASK),1,ATC('*',BOM.CITMMASK)-1)+LADATA[2],19)"
        ENDIF
        IF SEEK(&lcSeekExpr.,'STYLE' )
          *--save the cuurent aria & scan inside style file cause in case of extended size scale 
          *--there are many lines hold the style & we need all lines  in case the style cost 
          *--sheet elemnt (fabric) assingne  for all sizes.
          lnAlias = SELECT()
          SELECT STYLE
          SCAN FOR STYLE =  &lcSeekExpr.
        *B803292,1 WAB - (END) 
        
            SELECT (lcStyTemp)
            IF !SEEK(STYLE.STYLE)
              APPEND BLANK
            ELSE
              IF BOM.TYP # &lcStyTemp..Typ .OR. BOM.UOM # &lcStyTemp..UOM
                LOCATE WHILE cItmMask = STYLE.STYLE FOR Typ = BOM.TYP .AND. UOM = BOM.UOM
                IF !FOUND()
                  APPEND BLANK
                ENDIF
              ENDIF
            ENDIF  
            REPLACE cItmMajor  WITH STYLE.cStyMajor,;
                    cItmMask   WITH STYLE.Style    ,;
                    DESC       WITH STYLE.Desc1    ,;
                    UOM        WITH BOM.UOM        ,;
                    nbomtotqty WITH nbomtotqty+BOM.nbomtotqty,;
                    Typ        WITH BOM.Typ
          *B803292,1 WAB - (START) close scan
          ENDSCAN
          SELECT(lnAlias)
          *B803292,1 WAB - (END)
        ENDIF
      ELSE
        SELECT STYLE
        SCAN WHILE LEFT(STYLE.STYLE,lnMajLen)  = LEFT(BOM.cItmMajor,lnMajLen)
          SELECT (lcStyTemp)
          IF !SEEK(STYLE.STYLE)
            APPEND BLANK
          *B802838,1 AMM Get the item whether fabric or trim in the cost sheet
          ELSE
            IF BOM.TYP # &lcStyTemp..Typ .OR. BOM.UOM # &lcStyTemp..UOM
              LOCATE WHILE cItmMask = STYLE.STYLE FOR Typ = BOM.TYP .AND. UOM = BOM.UOM
              IF !FOUND()
                APPEND BLANK
              ENDIF
            ENDIF
          ENDIF
          REPLACE cItmMajor  WITH STYLE.cStyMajor,;
                  cItmMask   WITH STYLE.Style    ,;
                  DESC       WITH STYLE.Desc1    ,;
                  UOM        WITH BOM.UOM        ,;
                  nbomtotqty WITH nbomtotqty+BOM.nbomtotqty,;
                  Typ        WITH BOM.Typ
        ENDSCAN
      ENDIF
    ELSE
      IF SEEK(BOM.cItmMask,'STYLE')
        SELE (lcStyTemp)
        IF !SEEK(BOM.cItmMask)
          APPEND BLANK
        *B802838,1 AMM Get the item whether fabric or trim in the cost sheet
        ELSE
          IF BOM.TYP # &lcStyTemp..Typ .OR. BOM.UOM # &lcStyTemp..UOM
            LOCATE WHILE cItmMask = STYLE.STYLE FOR Typ = BOM.TYP .AND. UOM = BOM.UOM
            IF !FOUND()
              APPEND BLANK
            ENDIF
          ENDIF
        ENDIF  
        REPLACE cItmMajor  WITH STYLE.cStyMajor,;
                cItmMask   WITH STYLE.Style    ,;
                DESC       WITH STYLE.Desc1    ,;
                UOM        WITH BOM.UOM        ,;
                nbomtotqty WITH nbomtotqty+BOM.nbomtotqty ,;
                Typ        WITH BOM.TYP
      ENDIF
    ENDIF
    *! B604863,4 MHM 10/16/2001  [Start]
    ENDIF
    *! B604863,4 MHM [End]
  ENDIF
ENDSCAN  


*:**************************************************************************
*:* Name        : lfCKSegInf
*:* Developer   : TMI - TARE K MOHAMED IBRAHIM
*:* Date        : 04/19/2001
*:* Purpose     : Check if Code Segment Struc is created.
*:***************************************************************************
*:* Called from : lfActFolder(),lfDValid(),lpShow
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : .T. OR .F.
*:***************************************************************************
*:* Example     : = lfCKSegInf()
*:***************************************************************************
FUNCTION lfCKSegInf
PRIVATE llRet,lnAlias
llRet    = .F.
lnAlias  = SELECT(0)
llStruOp = gfOpenFile(gcDataDir+'ICISTRU','Segno','SH')
llRet = SEEK('U1','ICISTRU')
IF USED('ICISTRU') AND llStruOp
  USE IN ICISTRU
ENDIF
RETURN llRet

*!*************************************************************
*! Name      : lfObsColor
*! Developer : Abdou ElGendi _ (ABD)
*! Date      : 11/14/2001
*! Purpose   : Function to Check for the copyied colors if
*! Purpose   : status is obsolete for one or more color.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfObsColor()
*!*************************************************************
*C102467,1 
*!*************************************************************
*
FUNCTION lfObsColor

Private lnAlias , laColorObs , lcColorObs , llColorObs ;
        llAllClrOs , llOneClrOs , llNewClor

DECLARE laColorObs[1,2] ,laObsColor[1]
STORE '' TO lcColorObs
laColorObs[1,1] = 'LLOBSOLETE'
laColorObs[1,2] = 'llColorObs'
STORE .T. TO  llAllClrOs 
STORE .F. TO  llOneClrOs,laObsColor,llNewClor

lnAlias = SELECT(0)
SELECT FABRIC
= SEEK(lcFabric)
SCAN REST WHILE Fabric + Color  = lcFabric
  *-- Store Emty to the next Variable.
  STORE .F. TO llColorObs
  *-- Get the color from the style file.
  lcColorObs = Fabric.Color
  = gfRltFld(lcColorObs , @laColorObs, 'COLOR'))
  *-- check for this color if Obsolete.
  llAllClrOs = IIF(!llAllClrOs,llAllClrOs,llColorObs)
  llOneClrOs = IIF(llOneClrOs,llOneClrOs,llColorObs)
  IF llColorObs
    DECLARE laObsColor [ALEN(laObsColor)+IIF(!llNewClor,0,1)]
    laObsColor[ALEN(laObsColor)] = lcColorObs
    llNewClor = .T.
  ENDIF
ENDSCAN

DO CASE
  *-- Check if all color are obsolete yes , don't let the user to
  *-- Copy this style.
  CASE llAllClrOs
    *- Message Text   :- All Style colors are Obsolete, Can not copy this style.
    *- Message Number :- 42224.
    *- button message :- OK.
    *- button Number  :- 42001
    = gfModalGen('INM42224B42001','DIALOG','item'+'|'+'copy this item.')
    lcFabric = ''
    _CUROBJ  = OBJNUM(lcFabric)

  *-- Check if at least one color was obsolete , let the user know that
  *-- By a message and don't copy this color for the new style.
  CASE llOneClrOs
    *- Message Text   :- One or more of the style colors will not be 
    *- Message Text   :- copied because their statuses are obsolete.
    *- Message Number :- 42225
    *- button message :- OK.
    *- button Number  :- 000000
    = gfModalGen('INM42225B000000','DIALOG','item')
  SELECT (lnAlias)
ENDCASE

RETURN

*-- End OF lfObsColor
*C102467,1 
*!*************************************************************
*! Name      : lfInTran
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/20/02
*! Purpose   : Valid Function For InTransit Button 'Cut & Sold' Folder
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfInTran()
*!*************************************************************
*..E301797,1
FUNCTION lfInTran

PRIVATE lnPrevTbl,lcPrvTag , lcPosLnTag , lcBrFields,lnAlias
lnAlias  = SELECT(0)

DIMENSION laSelected[4]

IF !USED('matshpdt')
  = gfOpenFile(gcDataDir+'matshpdt','matshpdt'  ,'SH')
ENDIF
IF !USED('matshphd')
  = gfOpenFile(gcDataDir+'matshphd','matshphd'  ,'SH')
ENDIF

SELECT MatShpHd.SHIPNO,MatShpHd.STATUS,MatShpHd.ENTERED,MatShpHd.AIRWAYB,MatShpHd.CVESSEL,MatShpHd.CARTONS,;
       MatShpHd.REFERENCE,matshpdt.NFabTOTQTY,matshpdt.CWARECODE;
  FROM MatShpHd,matshpdt ;
  INTO DBF (gcWorkDir+lcCursName);
 WHERE MatShpHd.shipno = matshpdt.shipno ;
   AND matshpdt.FABRIC+matshpdt.Color = SUBSTR(laData[1],1,7)+SUBSTR(laData[2],1,6);
   AND matshpdt.TranCd = '3' AND matshpdt.NFabTOTQTY <> 0;
GROUP BY matshpdt.shipno

WAIT WINDOW 'CALCULATING INTRANSIT' NOWAIT
IF _TALLY > 0
  SELECT (lcCursName)
  DIMENSION laShpNo[1]
  STORE '' TO lcShpNo
  *--Preparing Work Area [end]
  lcFile_Ttl = 'InTransit'
  lcBrFields = "SHIPNO:H = 'Shipment #' ,"
  lcBrFields = lcBrFields + "STATUS :H='Status' ,ENTERED :10:H='Entered',"+;
               "AIRWAYB :15 :H='Airway Bill',CVESSEL :H='Vessel',"+;
               "CARTONS :H='Cartons',nFabTOTQTY :H='Total',"+;
               "REFERENCE :H='Reference'"
  IF lcWare <> 'All'                 
    SET FILTER TO CWareCode= PADR(lcWare,6)
  ENDIF  
  llWasSel = AriaBrow('', lcFile_Ttl, gnBrHSRow1, gnBrHSCol1,gnBrHSRow2, gnBrHSCol2,"","",'SHIPNO','laShpNo')
  IF llWasSel
    lcShpNo = laShpNo[1]
  ENDIF
  IF !EMPTY(lcShpNo)
    lcParam  = "'" + lcShpNo+ "'"    
    llFromMat = .T.
    DO gpDoProg WITH 'AWRPOSHP',.F.,'MA',lcParam
  ENDIF
ELSE
  =gfModalGen('QRM36030B36000','ALERT', 'Shipments|'+ALLTRIM(laData[1]) + '/'+ALLTRIM(laData[2]))
ENDIF
SELECT (lnAlias)
RETURN
*!*************************************************************
*! Name      : lfOldValue
*! Developer : Abdou Elgendy [ABD]
*! Date      : 05/14/2002
*! Purpose   : Function to store old value of the current filed.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*E301911,1 ABD [Begin]
FUNCTION lfoldvalue

lcOldValue = EVALUATE(SYS(18))
RETURN

*-- End OF lfoldvalue.
*!*************************************************************
*! Name      : lfvLink3Cod
*! Developer : Abdou Elgendy [ABD]
*! Date      : 05/14/2002
*! Purpose   : Valid function to validate sales gl link field.
*!*************************************************************
*! Calls     : gfGLBrowse 3chr
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*E301911,1 ABD 
FUNCTION lfvlink3cod

lnAlias=SELECT()


IF llbrowse OR !EMPTY(laData[28])
    
	lcSlinkcde = SPACE(30)
    lcGllnk3 = laData[28]
	IF gfGLbrowse('02',@lcGllnk3,@lcSlinkcde,2)
      laData[28] = lcGllnk3
	ELSE
  	  lcSlinkcde = lcOldValue
  	  laData[28] = lcOldValue
	ENDIF
ENDIF
laData[28]  =IIF(EMPTY(laData[28]),'DEF',laData[28])

IF !EMPTY (laData[28])
   SELECT GL_Link
   lcOldOrder = ORDER ()
   SET ORDER TO SALES
    =SEEK("02"+laData[28], "GL_Link")
    SET ORDER TO &lcOldOrder
    lcSlinkcde = GL_Link.LinkDesc
    lcSFields  = lcSFields +','+'laData[13]'
    =lfRefresh()
ENDIF

SHOW GET laData[28]
SHOW GET lcSlinkcde
llbrowse = .F.
SELECT(lnalias)
RETURN

*-- End of lfvlink3cod.
*E301911,1 ABD - [End]
*!*************************************************************
*! Name      : lfvSelPrce
*! Developer : Abdou Elgendy [ABD]
*! Date      : 05/14/2002
*! Purpose   : Valid function to validate sell price field.
*!*************************************************************
*! Calls     : gfModalGen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*E301911,1 ABD 
FUNCTION lfvSelPrce

IF laData[25] < 0
  *--Negative values are not allowed.
  =gfModalGen('TRM42000B42001','DIALOG')
  laData[25] = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

IF laData[25] # lcOldValue
  lcSFields  = lcSFields +','+'laData[25]'
ENDIF
*-- End OF lfvSelPrce
*!*************************************************************
*! Name      : lfvSellGLK
*! Developer : Abdou Elgendy [ABD]
*! Date      : 05/14/2002
*! Purpose   : Valid function to validate sell code at warehouse.
*!*************************************************************
*! Calls     : None..
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*E301911,1 ABD 
FUNCTION lfvSellGLK

lcslsGlLnk = PADR("?",3)
llNothing  = lfvSlGWarH()

*-- End OF lfvSellGLK
*!*************************************************************
*! Name      : lfvSlGWarH
*! Developer : Abdou Elgendy [ABD]
*! Date      : 05/14/2002
*! Purpose   : Valid function to validate sell code at warehouse.
*!*************************************************************
*! Calls     : None..
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*E301911,1 ABD 
FUNCTION lfvSlGWarH
PRIVATE llDown
llDown = MDOWN()

IF llDown
  RETURN
ENDIF 
IF EMPTY(lcslsGlLnk)
    REPLACE cSlsGlLink  WITH lcslsGlLnk ;
            cStatus     WITH IIF (cStatus='A','A','M')
ELSE
  IF gfGLbrowse('02',@lcslsGlLnk,@lcSlinkcde,2)
    REPLACE cSlsGlLink WITH lcslsGlLnk ;
            cStatus    WITH IIF (cStatus='A','A','M')
  ELSE
    lcslsGlLnk = lcOldValue
  ENDIF
ENDIF  
SHOW GET lcslsGlLnk
SHOW WINDOW (lcWare_Ttl) REFRESH

*-- End OF lfvSlGWarH
*!*************************************************************
*! Name      : lfSellOrdr
*! Developer : Abdou Elgendy [ABD]
*! Date      : 05/14/2002
*! Purpose   : Valid function to get the Sell order for the item.
*!*************************************************************
*! Calls     : None..
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*E301911,1 ABD 
FUNCTION lfSellOrdr
PRIVATE lnAlias,llMaSohdr,llMaSolin,lcORbrowtl,lcOldBFld

llSOInstld = (OCCURS('SO',gcCmpModules)<>0)
IF !llSOInstld
  *--XXX module is not installed, Cannot proceed.
  =gfModalGen('QRM42083B42001','DIALOG','Sales Order')
  RETURN
ENDIF  

DIMENSION laMSO[2]
STORE .F. To llMaSohdr,llMaSolin,llCustomer
STORE '' TO lcFile_Ttl , lcOldBFld , lcMSO , lcMSOType,laMSO

lnAlias = SELECT (0)
*-- Open the Material Sales order header file.
IF !USED('MASOHDR')
  llMaSohdr = gfOpenFile(gcDataDir+'MaSohdr',gcDataDir+'MaSohdr','SH')
ENDIF

*-- Open the Material Sales order line  file.
IF !USED('MaSolin')
  llMaSolin = gfOpenFile(gcDataDir+'MaSolin',gcDataDir+'Masolinf','SH')
ENDIF

*-- Open the customer file.
IF !USED('CUSTOMER')
  llCustomer = gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
ENDIF

SELECT MaSolin
SET RELATION TO cordtype+cMorder INTO MaSohdr ADDITIVE
SET RELATION TO IIF(EMPTY(STORE),'M'+account,'S'+account+STORE) INTO customer ADDITIVE

IF SEEK(SUBSTR(laData[1],1,7)+SUBSTR(laData[2],1,6),'MaSolin')
  SELECT MaSolin
  LOCATE REST WHILE fabric+color+DTOS(complete)+cordtype+cmorder+store+STR(lineno,6) = ;
  SUBSTR(laData[1],1,7)+SUBSTR(laData[2],1,6);
  FOR (MaSohdr.Status $ 'OH' .AND. FabQty > 0) .AND. IIF(lcWare = 'All',.T., MaSolin.cwarecode=PADR(lcWare,6))
  IF FOUND()
    lcOldBFld  = lcBrFields
    *E126684,1  TMI [Start] let the sell price be 5 decimals
    *lcBrFields = "cMorder        :8  :H='Order #'                ,"+;
		         "MaSohdr.STATUS :3  :H='St'                     ,"+;
	             "ACCOUNT        :10 :H='Account'                ,"+;
                 "STORE          :10 :H='Store'                  ,"+;
		         "CUSTOMER.STNAME:35 :H='Name'                   ,"+;
		         "MaSohdr.Start  :11 :H='Start'                  ,"+;
		         "COMPLETE       :11 :H='Complete'               ,"+;
                 "FabQTY         :6  :H='Ord.Qty' :P='999999.999'    ,"+;
		         "nSellNetPr     :10 :P='9999.999' :H='Sell Price',"+;
                 "lcStat=ALLTRIM(CUSTOMER.cAddress4) :H='State'  ,"+;
                 "lcZip =SUBSTR(CUSTOMER.cAddress5,1,5) :H='Zip' ,"+;
                 "lcTel =MaSohdr.Phone :H='Phone' :P=gfPhoneTem()"
    lcBrFields = "cMorder        :8  :H='Order #'                ,"+;
		         "MaSohdr.STATUS :3  :H='St'                     ,"+;
	             "ACCOUNT        :10 :H='Account'                ,"+;
                 "STORE          :10 :H='Store'                  ,"+;
		         "CUSTOMER.STNAME:35 :H='Name'                   ,"+;
		         "MaSohdr.Start  :11 :H='Start'                  ,"+;
		         "COMPLETE       :11 :H='Complete'               ,"+;
                 "FabQTY         :6  :H='Ord.Qty' :P='999999.999'    ,"+;
		         "nSellNetPr     :10 :P='9999.99999' :H='Sell Price',"+;
                 "lcStat=ALLTRIM(CUSTOMER.cAddress4) :H='State'  ,"+;
                 "lcZip =SUBSTR(CUSTOMER.cAddress5,1,5) :H='Zip' ,"+;
                 "lcTel =MaSohdr.Phone :H='Phone' :P=gfPhoneTem()"
    *E126684,1  TMI [End  ]   
  lcFile_Ttl = 'Item Sales Orders'
  *-- 
    = AriaBrow("SUBSTR(laData[1],1,7)+SUBSTR(laData[2],1,6) FOR !(MaSohdr.Status $ 'CX' .OR. FabQty <= 0) .AND. IIF(lcWare = 'All', .T. , MaSolin.cwarecode=PADR(lcWare,6)) ",;
    lcFile_Ttl, gnBrHSRow1, gnBrHSCol1,gnBrHSRow2, gnBrHSCol2,'','','cordtype,cMorder','laMSO')
    lcBrFields = lcOldBFld
  ELSE
     =gfModalGen('QRM36030B36000','ALERT', 'Sales Orders|'+ALLTRIM(laData[1]) + '/'+ALLTRIM(laData[2]))  
  ENDIF
ELSE
  =gfModalGen('QRM36030B36000','ALERT', 'Sales Orders|'+ALLTRIM(laData[1]) + '/'+ALLTRIM(laData[2]))
ENDIF


IF !EMPTY(laMSO[1]) .AND. !EMPTY(laMSO[2])
  lcMSOType = laMSO[1]
  lcMSO     = laMSO[2]
  *-- Call the Material Order Screen.
  lcParam  = "'" + lcMSOType + "','" + lcMSO + "'"
  DO gpDoProg WITH 'AWRSOMAT',.F.,'SO',lcParam
ENDIF

*-- Remove the relation
SELECT MaSolin
SET RELATION TO 

*-- Close the Material Sales order Header file.
IF llMaSohdr
  = gfCloseFile('MaSohdr')
ENDIF

*-- Close the Material Sales order line  file.
IF llMaSolin
  = gfCloseFile('MaSolin')
ENDIF

*-- Close the customer file.
IF llCustomer
  = gfCloseFile('CUSTOMER')
ENDIF

SELECT (lnAlias)

RETURN
*-- End OF lfSellOrdr
*!*************************************************************
*! Name      : lfSellInvc
*! Developer : Abdou Elgendy [ABD]
*! Date      : 05/14/2002
*! Purpose   : Valid function to get the Sell order for the item.
*!*************************************************************
*! Calls     : None..
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*E301911,1 ABD 
FUNCTION lfSellInvc
PRIVATE lnAlias,llMaSohdr,llMaSolin,lcORbrowtl,lcOldBFld

llARInstld = (OCCURS('AR',gcCmpModules)<>0)
IF !llARInstld
  *--XXX module is not installed, Cannot proceed.
  =gfModalGen('QRM42083B42001','DIALOG','Accounts Receivable')
  RETURN
ENDIF  

DIMENSION laMAr[1]
STORE .F. To llMINVHDR ,llMINVLINE,llCustomer
STORE '' TO lcFile_Ttl , lcOldBFld , lcMAr ,laMAr

lnAlias = SELECT (0)
*-- Open the Material Invoice Sales order header file.
IF !USED('MINVHDR')
  llMaSohdr = gfOpenFile(gcDataDir+'MINVHDR',gcDataDir+'MINVHDR','SH')
ENDIF

*-- Open the Material invoice Sales order line  file.
IF !USED('MINVLINE')
  llMaSolin = gfOpenFile(gcDataDir+'MINVLINE',gcDataDir+'MINVLINS','SH')
ENDIF

*-- Open the customer file.
IF !USED('CUSTOMER')
  llCustomer = gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
ENDIF

SELECT MINVLINE
SET RELATION TO cmatinv INTO MINVHDR ADDITIVE
SET RELATION TO IIF(EMPTY(STORE),'M'+account,'S'+account+STORE) INTO customer ADDITIVE


IF SEEK(SUBSTR(laData[1],1,7)+SUBSTR(laData[2],1,6),'MINVLINE')
  SELECT MINVLINE
  LOCATE REST WHILE fabric+color+cmatinv+STR(lineno,6) = ;
  SUBSTR(laData[1],1,7)+SUBSTR(laData[2],1,6) .AND. IIF(lcWare = 'All',.T., MINVLINE.cwarecode = PADR(lcWare,6))
  IF FOUND()
    lcOldBFld  = lcBrFields
    *E126684,1  TMI [Start] Let the decimal for price be 5  
    *lcBrFields = "cmatinv         :8  :H='Invoice'                  ,"+;
                 "MINVHDR.Status  :8  :H='Status'                   ,"+;
                 "INVDATE         :8  :H='Inv.Date'                 ,"+;
                 "ACCOUNT         :7  :H='Acct#'                    ,"+;
   	             "STORE           :10 :H='Store'                    ,"+;
   		         "NAME = SUBSTR(CUSTOMER.STNAME,1,22) :H='Name'     ,"+;
		         "cmOrder         :8  :H='Order'                    ,"+;
                 "FabQTY              :H='Tot Qty' :P='999999.999'      ,"+;
		         "nsellPrice :H='Price':P='99999.999'                ,"+;
                 "lcSRp   = MINVHDR.Rep1:5 :H='Sales Rep.'          ,"+;
                 "lcTerms = MINVHDR.cTermCode :8 :H='Term'          ,"+;
                 "lcTel =CUSTOMER.Phone1 :H='Phone' :P=gfPhoneTem() ,"+;
                 "lcSt=ALLTRIM(CUSTOMER.cAddress42) :H='State'      ,"+;
                 "lcZp=ALLTRIM(CUSTOMER.cAddress52) :H='Zip'"
    lcBrFields = "cmatinv         :8  :H='Invoice'                  ,"+;
                 "MINVHDR.Status  :8  :H='Status'                   ,"+;
                 "INVDATE         :8  :H='Inv.Date'                 ,"+;
                 "ACCOUNT         :7  :H='Acct#'                    ,"+;
   	             "STORE           :10 :H='Store'                    ,"+;
   		         "NAME = SUBSTR(CUSTOMER.STNAME,1,22) :H='Name'     ,"+;
		         "cmOrder         :8  :H='Order'                    ,"+;
                 "FabQTY              :H='Tot Qty' :P='999999.999'      ,"+;
		         "nsellPrice :H='Price':P='99999.99999'             ,"+;
                 "lcSRp   = MINVHDR.Rep1:5 :H='Sales Rep.'          ,"+;
                 "lcTerms = MINVHDR.cTermCode :8 :H='Term'          ,"+;
                 "lcTel =CUSTOMER.Phone1 :H='Phone' :P=gfPhoneTem() ,"+;
                 "lcSt=ALLTRIM(CUSTOMER.cAddress42) :H='State'      ,"+;
                 "lcZp=ALLTRIM(CUSTOMER.cAddress52) :H='Zip'"
    *E126684,1  TMI [End  ] 

  
  lcFile_Ttl = 'Item Invoices'
  *-- 
    = AriaBrow("SUBSTR(laData[1],1,7)+SUBSTR(laData[2],1,6) FOR IIF(lcWare = 'All', .T. , MINVLINE.cwarecode=PADR(lcWare,6)) ",;
    lcFile_Ttl, gnBrHSRow1, gnBrHSCol1,gnBrHSRow2, gnBrHSCol2,'','','cMatinv','laMAR')
    lcBrFields = lcOldBFld
  ELSE
     =gfModalGen('QRM36030B36000','ALERT', 'Invoices|'+ALLTRIM(laData[1]) + '/'+ALLTRIM(laData[2]))  
  ENDIF
ELSE
  =gfModalGen('QRM36030B36000','ALERT', 'Invoices|'+ALLTRIM(laData[1]) + '/'+ALLTRIM(laData[2]))
ENDIF


*-- Untill now we didn't do the direct invoice ,
*-- So I will not call any program
IF !EMPTY(laMAr[1])
  lcMAr     = laMAr[1]
  *-- Call the Material Order Screen.
  lcParam  = "'" + lcMAr + "'"
  DO gpDoProg WITH 'AWRARMDIN',.F.,'AR',lcParam
ENDIF

*-- Remove the relation
SELECT MINVLINE
SET RELATION TO 

*-- Close the Material Sales order Header file.
IF llMINVHDR
  = gfCloseFile('MINVHDR')
ENDIF

*-- Close the Material Sales order line  file.
IF llMINVLINE
  = gfCloseFile('MINVLINE')
ENDIF

*-- Close the customer file.
IF llCustomer
  = gfCloseFile('CUSTOMER')
ENDIF

SELECT (lnAlias)

RETURN
*-- End OF lfSellInvc
*!*************************************************************
*! Name      : lfChngDoms
*! Developer : Abdou Elgendy [ABD]
*! Date      : 05/14/2002
*! Purpose   : Valid function to Change The screen.
*!*************************************************************
*! Calls     : None..
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfChngDoms()
*!*************************************************************
*E301911,1 ABD 
FUNCTION lfChngDoms

IF llMultCur  .AND. lnDomestic = 2
  SHOW GETS WINDOW (lcWinCh8) DISABLE ONLY
  SHOW WINDOW (lcWinCh1) TOP
  laFoldWinds[4,2] = "lcWinCh1"
ELSE
  SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
  SHOW WINDOW (lcWinCh8) TOP
  laFoldWinds[4,2] = "lcWinCh8"
ENDIF
   
IF laScrMode[1] OR laScrMode[2]
  IF llMultCur .AND. lnDomestic = 2
    SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY    
  ELSE
    SHOW GETS WINDOW (lcWinCh8) DISABLE ONLY
  ENDIF
ELSE
  IF llMultCur .AND. lnDomestic = 2
    SHOW GETS WINDOW (lcWinCh1) ENABLE ONLY
  ELSE
    SHOW GETS WINDOW (lcWinCh8) ENABLE ONLY
  ENDIF
  _CurOBJ = OBJNUM(IBDomestic)
  SHOW GET laData[15] ENABLE
  IF lnDomestic = 2 AND llMultCur
    SHOW GET lnCost1    ENABLE
    SHOW GET lnCost2    ENABLE
    SHOW GET lnCost3    ENABLE
    SHOW GET lnCost4    ENABLE
    SHOW GET laData[18] ENABLE
    SHOW GET laData[19] ENABLE
    SHOW GET ibPCurr    ENABLE
    SHOW GET ibDCurr    ENABLE
  ELSE
    IF !WONTOP(lcWinCh8)
      SHOW GET lnCost1    DISABLE
      SHOW GET lnCost2    DISABLE
      SHOW GET lnCost3    DISABLE
      SHOW GET lnCost4    DISABLE
      SHOW GET laData[18] DISABLE
      SHOW GET laData[19] DISABLE
      SHOW GET ibPCurr    DISABLE
      SHOW GET ibDCurr    DISABLE
    ENDIF  
  ENDIF  
  SHOW GET lnBCost1   &lcCostStat
  SHOW GET lnBCost2   &lcCostStat
  SHOW GET lnBCost3   &lcCostStat
  SHOW GET lnBCost4   &lcCostStat

  IF llMultCur  .AND. lnDomestic = 2
    SHOW GETS WINDOW (lcWinCh8) DISABLE ONLY
    SHOW WINDOW (lcWinCh1) TOP
  ELSE
    SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
    SHOW WINDOW (lcWinCh8) TOP
  ENDIF
ENDIF
    
RETURN

*E301911,1 ABD [End]
*-- End OF lfChngDoms.
*!*************************************************************
*! Name      : lfvSlsConv
*! Developer : Abdou Elgendy [ABD]
*! Date      : 05/14/2002
*! Purpose   : Validation for the Sales Conv. FActor field
*!*************************************************************
*! Calls     : None..
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvSlsConv()
*!*************************************************************
*E301911,1 ABD 
FUNCTION lfvSlsConv
PRIVATE lcMessage

IF laData[27] <= 0
  =gfModalGen('TRM36014B36000','ALERT','Conversion factor')
  laData[27] = OConvFact
  _CUROBJ = OBJNUM(laData[27])
ENDIF  
=lfRefresh()
*-- End OF lfvSlsConv
*!*************************************************************
*! Name      : lfStatustl
*! Developer : Abdou Elgendy [ABD]
*! Date      : 05/14/2002
*! Purpose   : Validation for the Sales Conv. FActor field
*!*************************************************************
*! Calls     : None..
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : Status Titel.
*!*************************************************************
*! Example   : =lfStatustl()
*!*************************************************************
*E301911,1 ABD 
FUNCTION lfStatustl
Private lcRetTitel
lcRetTitel =''

DO CASE
  CASE Status  = 'B'
    lcRetTitel = 'Bid      '
  CASE Status  = 'O'
    lcRetTitel = 'Open     '
  CASE Status  = 'H'
    lcRetTitel = 'Hold     '
  CASE Status  = 'X'
    lcRetTitel = 'Cancelled'
  CASE Status  = 'C'
    lcRetTitel = 'Complete '
ENDCASE

RETURN lcRetTitel
*-- End OF lfStatustl
*!*************************************************************

*!*************************************************************
*! Name      : lfvSupVen
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/04/2002
*! Purpose   : Validation for the vendor reference vendor field
*!*************************************************************
*! Calls     : None..
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : Status Titel.
*!*************************************************************
*! Example   : =lfvSupVen()
*!*************************************************************
FUNCTION lfvSupVen
*-C102684,1
PRIVATE lnAlias
lnAlias = SELECT()
PRIVATE lcbrfields
IF !llBrowse AND lcVenCode=lcOldValue
  RETURN
ENDIF
IF llBrowse OR (!EMPTY(lcVenCode) AND (!SEEK(lcVenCode,'APVENDOR') OR ATC('M' , APVENDOR.cVenSupTyp) = 0 ))
  SELECT APVENDOR
  SET ORDER TO TAG VENCODE
  IF llBrowse OR !SEEK(lcVenCode)
    lcBrFields = "CVENDCODE :H='Vendor',"+;
                 "CVENCOMP  :H='Name',"+;
                 "cPhoneNo  :H='Phone',"+;
                 "cVenOurAc :H='Our Account' "
     lcForExpr = " 'M' $ cVenSupTyp "
    IF gfBrows([FOR &lcForExpr],'cVendCode', 'laTempArr','Vendors')
      lcVenCode=APVENDOR.cVendCode
      lcVenName=APVENDOR.cvencomp
    ELSE
      lcVenCode=SPACE(8)
    ENDIF
  ELSE
    lcVenName=APVENDOR.cvencomp  
  ENDIF
  llBrowse = .F.
ENDIF
IF !EMPTY(lcVenCode)
  lcVenName=APVENDOR.cvencomp  
ELSE
  _CUROBJ = OBJNUM(lcVenCode)  
ENDIF
IF !EMPTY(lcVenCode)
  SHOW GET ibSupCode  ENABLE
  SHOW GET lcVenFab   ENABLE
  SHOW GET ibSupColor ENABLE
  SHOW GET lcVenCol   ENABLE
  SET ORDER TO 0 IN VENDMATL

  *B122673,1 NNA 05/15/2004 (Begin) Replace the Price Field with the new name (NFABCOST)
  *SELECT DIST Vendor,cVenFab,cvencolr,Fabric,Color,Price,nFabTotQty,LeadTime FROM VENDMATL WHERE vendor+cvenfab+cvencolr+fabric+color = lcVenCode ;
          INTO DBF (gcWorkDir+'TmpVend') 
  SELECT DIST Vendor,cVenFab,cvencolr,Fabric,Color,nFabCost,nFabTotQty,LeadTime FROM VENDMATL WHERE vendor+cvenfab+cvencolr+fabric+color = lcVenCode ;
          INTO DBF (gcWorkDir+'TmpVend') 
  *B122673,1 NNA (End)

  SELECT TmpVend
  INDEX ON vendor+cvenfab UNIQUE TAG TmpVend
  INDEX ON vendor+cvenfab+cvencolr+fabric+color TAG TmpVend1
ELSE
  STORE " " TO lcVenName,lcVenFab,lcVenCol,lcAriaFab,lcAriaClr
  SHOW GET ibSupCode  DISABLE
  SHOW GET lcVenFab   DISABLE
  SHOW GET ibSupColor DISABLE
  SHOW GET lcVenCol   DISABLE
ENDIF
IF !EMPTY(lcVenCode)
  SET ORDER TO Venmat IN VENDMATL
  IF (!EMPTY(lcVenFab) AND !SEEK(lcVenCode+lcVenFab,'VENDMATL')) OR (!EMPTY(lcVenCol) AND ;
      !SEEK(lcVenCode+lcVenFab+lcVenCol,'VENDMATL'))
    STORE " " TO lcVenName,lcVenFab,lcVenCol,lcAriaFab,lcAriaClr
    SHOW GET ibSupCode  ENABLE
    SHOW GET lcVenFab   ENABLE
    SHOW GET ibSupColor ENABLE
    SHOW GET lcVenCol   ENABLE
  ENDIF
ENDIF
*ADLE
IF !EMPTY(lcVenFab)
*IF !EMPTY(lcVenFab) AND !laScrMode[2]
  SHOW GET pbOk ENABLE
  _CUROBJ = OBJNUM(lcVenCol)
ELSE
  SHOW GET pbOk DISABLE
ENDIF
=lfRefresh('')
SELECT (lnAlias)

*!**********************************************************************************************
*! Name        : lfvVenFab
*! Developer   : Adel Mohammed El Gazzar (AADEL)
*! Date        : 08/19/02
*!**********************************************************************************************
*! Purpose     : Validate Vendor Item
*!**********************************************************************************************
*! Calls       : 
*!**********************************************************************************************
FUNCTION lfvVenFab
*-C102684,1

IF MDOWN() OR (!EMPTY(lcVenFab) AND lcVenFab = lcOldValue)
  RETURN
ENDIF
*-- lcFields   && Holds browsed fields
*-- laBrow     && Holds returned values
PRIVATE lnAlias,lcFields,laBrow
DIMENSION laBrow[5]
STORE SPACE(0) TO lcFields,laBrow

*B122673,1 NNA 05/15/2004 (Begin) Replace the Price Field with the new name (NFABCOST)
*lcFields    = "cVenFab,Fabric,nFabCost,nFabTotQty,LeadTime"
*lcBrFields  = [cvenfab   :25 :H='Supplier Code',]+;
              [fabric    :15 :H='Item Code',]+;
              [price     :8  :H='Price',]+;
              [nfabtotqty:16 :H='Mimimum P. Qty',]+;
              [leadtime  :10 :H='Lead Time']  
lcFields    = "cVenFab,Fabric,Price,nFabTotQty,LeadTime"
lcBrFields  = [cvenfab   :25 :H='Supplier Code',]+;
              [fabric    :15 :H='Item Code',]+;
              [nFabCost  :8  :H='Price',]+;
              [nfabtotqty:16 :H='Mimimum P. Qty',]+;
              [leadtime  :10 :H='Lead Time']  
*B122673,1 NNA (End)

lnAlias = SELECT()
SET ORDER TO TmpVend IN TmpVend
IF !EMPTY(lcVenFab)
  IF !SEEK(lcVenCode+lcVenFab,'TmpVend')
    SELECT TmpVend
    IF AriaBrow('','Supplier Reference',gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,lcFields,"laBrow");  
                gnBrFSRow2, gnBrFSCol2,.F.,.F.,"cvenfab","laBrow")
      lcVenFab  = laBrow[1]
      lcAriaFab = Fabric
      IF !EMPTY(lcVenCol)
        SET ORDER TO TmpVend1 IN TmpVend
        IF !SEEK(lcVenCode+lcVenFab+lcVenCol,'TmpVend')
          lcVenCol  = SPACE(10) 
          lcAriaClr = SPACE(06)
        ELSE
          lcAriaClr = Color  
        ENDIF
      ENDIF
    ELSE
      lcVenFab  = IIF(!EMPTY(lcOldValue),lcOldValue,SPACE(06))
      lcAriaFab = IIF(!EMPTY(lcOldValue),lcAriaFab,SPACE(07))
    ENDIF
  ELSE
    lcVenFab  = TmpVend.cvenfab
    lcAriaFab = TmpVend.Fabric
  ENDIF
ELSE
  lcVenFab  = SPACE(10) 
  lcAriaFab = SPACE(07) 
  lcVenCol  = SPACE(10) 
  lcAriaClr = SPACE(06) 
ENDIF
SHOW GET lcVenFab
SHOW GET lcVenCol
IF !EMPTY(lcVenFab)
  SHOW GET pbOk ENABLE
  _CUROBJ = OBJNUM(lcVenCol)
ELSE
  SHOW GET pbOk DISABLE
ENDIF
=lfRefresh()
SELECT (lnAlias)

*!**********************************************************************************************
*! Name        : lfvVenCol
*! Developer   : Adel Mohammed El Gazzar (AADEL)
*! Date        : 08/19/02
*!**********************************************************************************************
*! Purpose     : Validte Vendor Color
*!**********************************************************************************************
*! Calls       : 
*!**********************************************************************************************
FUNCTION lfvVenCol
*-C102684,1

IF MDOWN() OR (!EMPTY(lcVenCol) AND lcVenCol= lcOldValue)
   RETURN
ENDIF
*-- lcFields   && Holds browsed fields
*-- laBrow     && Holds returned values
PRIVATE lnAlias,lcFields,laBrow
DIMENSION laBrow[7]
STORE SPACE(0) TO lcFields,laBrow

*B122673,1 NNA 05/15/2004 (Begin) Replace the Price Field with the new name (NFABCOST)
*lcFields    = "cVenFab,cvencolr,Fabric,Color,Price,nFabTotQty,LeadTime"
*lcBrFields  = [cvenfab   :25 :H='Supplier Code',]+;
              [cvencolr  :25 :H='Supplier Color',]+;
              [fabric    :15 :H='Item Code',]+;
              [Color     :15 :H='Item Color',]+;
              [price     :8  :H='Price',]+;
              [nfabtotqty:16 :H='Mimimum P. Qty',]+;
              [leadtime  :10 :H='Lead Time']  
lcFields    = "cVenFab,cvencolr,Fabric,Color,nFabCost,nFabTotQty,LeadTime"
lcBrFields  = [cvenfab   :25 :H='Supplier Code',]+;
              [cvencolr  :25 :H='Supplier Color',]+;
              [fabric    :15 :H='Item Code',]+;
              [Color     :15 :H='Item Color',]+;
              [nFabCost  :8  :H='Price',]+;
              [nfabtotqty:16 :H='Mimimum P. Qty',]+;
              [leadtime  :10 :H='Lead Time']  
*B122673,1 NNA (End)

lnAlias = SELECT()
SET ORDER TO TmpVend1 IN TmpVend
IF !EMPTY(lcVenCol)
  IF !SEEK(lcVenCode+lcVenFab+lcVenCol,'TmpVend')
    SELECT TmpVend
    IF AriaBrow(IIF(!EMPTY(lcVenFab),"lcVenCode+lcVenFab" ,""),'Supplier Reference',gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,lcFields,"laBrow");  
                gnBrFSRow2, gnBrFSCol2,.F.,.F.,"cvenfab","laBrow")
      lcVenFab  = laBrow[1]
      lcVenCol  = laBrow[2]
      lcAriaFab = Fabric
      lcAriaClr = Color
    ELSE
      lcVenCol  = IIF(!EMPTY(lcOldValue),lcOldValue,SPACE(06))
      lcAriaClr = IIF(!EMPTY(lcOldValue),lcAriaClr,SPACE(07))
      lcVenCol  = SPACE(10) 
      lcAriaClr = SPACE(06) 
    ENDIF
  ELSE
    lcVenFab  = TmpVend.cvenfab
    lcVenCol  = TmpVend.cvencolr
    lcAriaFab = TmpVend.Fabric
    lcAriaClr = TmpVend.Color
  ENDIF  
ELSE
  lcVenCol  = SPACE(10) 
  lcAriaClr = SPACE(06) 
ENDIF
SHOW GET lcVenFab
SHOW GET lcVenCol
IF !EMPTY(lcVenFab)
  SHOW GET pbOk ENABLE
ELSE
  SHOW GET pbOk DISABLE
ENDIF
IF !EMPTY(lcVenCol)
  _CUROBJ = OBJNUM(pbOk)
ENDIF
=lfRefresh()
SELECT (lnAlias)


*!**********************************************************************************************
*! Name        : lfvVenFb
*! Developer   : Adel Mohammed El Gazzar (AADEL)
*! Date        : 08/19/02
*!**********************************************************************************************
*! Purpose     : Validte Vendor Fabric invisible button
*!**********************************************************************************************
*! Calls       : 
*!**********************************************************************************************
FUNCTION lfvVenFb

lcVenFab = "?"
=lfvVenFab()

*!**********************************************************************************************
*! Name        : lfvVenClr
*! Developer   : Adel Mohammed El Gazzar (AADEL)
*! Date        : 08/19/02
*!**********************************************************************************************
*! Purpose     : Validte Vendor Color invisible button
*!**********************************************************************************************
*! Calls       : 
*!**********************************************************************************************
FUNCTION lfvVenClr
*-C102684,1

lcVenCol = "?"
=lfvVenCol()


*!**********************************************************************************************
*! Name        : lfvVenClr
*! Developer   : Adel Mohammed El Gazzar (AADEL)
*! Date        : 08/19/02
*!**********************************************************************************************
*! Purpose     : Validte Vendor screen Ok button
*!**********************************************************************************************
*! Calls       : 
*!**********************************************************************************************
FUNCTION lfvVendOk
*-C102684,1

llOk  = .T.
*C102684,1 (Begin) True llVendQury
*C102684,1 (Begin) Make llVendQury  = .T. to show it comes from Vendor Reference screen.
IF ASCAN(laEvntTrig , PADR('TRUEIT',10)) <> 0
  =gfDoTriger('MAMATRL',PADR('TRUEIT',10))
ENDIF
*C102684,1 (End)



*!**********************************************************************************************
*! Name        : lpVendFab
*! Developer   : Adel Mohammed El Gazzar (AADEL)
*! Date        : 08/19/02
*!**********************************************************************************************
*! Purpose     : Display the Vendor Reference screen.
*!**********************************************************************************************
*! Calls       : 
*!**********************************************************************************************
PROCEDURE lpVendFab
*-C102684,1

llOk = .F.
IF !USED('VENDMATL')
  =gfOpenFile(gcDataDir+'VENDMATL',gcDataDir+'Venmat','SH')
ENDIF  
IF laScrMode[1]
  STORE " " TO lcVenCode,lcVenName,lcVenFab,lcVenCol,lcAriaFab,lcAriaClr
ENDIF  
lcVenKeySt = IIF(laScrMode[1] ,"ENABLE" ,"DISABLE")
DO (gcScrDir+gcWinAppl+'\MAMATRF.SPX')
IF llOk AND !EMPTY(lcVenFab)
  *-- Color = Vendor Color
  laData[1] = lcAriaFab
  laData[2] = lcAriaClr
  IF SEEK(laData[1],"Fabric")
    SHOW GET laData[1]  DISABLE
    SHOW GET ibItem     DISABLE
    =lfvData2()
  ENDIF
  *C102684,1 (Begin)  Calculate On order for Bratex.
  IF ASCAN(laEvntTrig , PADR('CALCORD',10)) <> 0
    =gfDoTriger('MAMATRL',PADR('CALCORD',10))
  ENDIF
  *C102684,1 (End)
ENDIF  

*!*************************************************************
*! Name      : lpGetVenOrd
*! Developer : Adel Mohammed El Gazzar (AADEL)
*! Date      : 08/19/02
*! Purpose   : browse the orders for purchassed selected fabric/color
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
* --C102684,1
PROCEDURE lpGetVenOrd
PRIVATE lnAlias,lcBrFields,lcPO,laPO,llWasSel,lcType

lnAlias    = SELECT()
lcPO       = SPACE(06)
lcType     = SPACE(01)
SELECT POFLN.cMatType ,POFLN.POMAT,POFLN.nCost1 as nPrice,;
       SUM(IIF(TRANCD='1',nFabTOTQTY,0.000)) AS nORDERED,;
       SUM(IIF(TRANCD='2',nFabTOTQTY,0.000)) AS nRECEIVED,;
       SUM(IIF(TRANCD='3',nFabTOTQTY,0.000)) AS nDAMAGED,;
       SUM(IIF(TRANCD='4',nFabTOTQTY,0.000)) AS nCANCELLED ;
  FROM POFLN;
  INTO DBF (gcWorkDir+lcCursName);
  WHERE (POFLN.cMatType = 'P' OR POFLN.cMatType = 'R') ;
  AND POFLN.FABRIC+POFLN.COLOR = SUBSTR(laData[1],1,7)+SUBSTR(laData[2],1,6);  
  AND POFLN.vendor = lcVenCode;
  GROUP BY POFLN.POMAT

WAIT CLEAR
IF _TALLY > 0
  lcFile_Ttl  = 'Item PO (s) / Returns'
  lcBrFields  = [dummi=IIF(cMatType = 'P','Pur.Ord','Return') :H='Trans. Type',]+;
                [POMAT:8:H='P/O #',]+;
                [POFHDR.STATUS :2:H='S',]+;
                [POFHDR.cWareCode:11:H='Location',]+;                  
                [POFHDR.VENDOR:11:H='Vendor',]+;
                [POFHDR.COMPLETE:8:H='Complete',]+;
                IIF(llCostPrv, [nPRICE:H='Price':P='99999.999',],[])+;
                [nORDERED:H='Ordered':P='999999.999',]+;
                [nRECEIVED:H='Received':P='999999.999',]+;
                [nOPEN=IIF( (POFHDR.STATUS='X').OR.]+;
                [ ((nORDERED-nRECEIVED-nDAMAGED-nCANCELLED)<0) ,]+;
                [ 0, nORDERED-nRECEIVED-nDAMAGED-nCANCELLED)]+;
                [:P='999999.999':H='Open']
  DECLARE laPO[2]
  SELECT (lcCursName)
  SET RELA TO cMatType+POMAT INTO POFHDR ADDI
  IF lcWare = 'All'  
  ELSE
    SET FILTER TO POFHDR.CWareCode= PADR(lcWare,6) 
  ENDIF
  GO TOP
  IF !EOF()
    llWasSel=AriaBrow('', lcFile_Ttl, gnBrHSRow1, gnBrHSCol1,;
              gnBrHSRow2, gnBrHSCol2,"","","POMAT,cMatType","laPO")
    IF llWasSel
      lcPO   = laPO[1]
      lcCall = laPO[2]
    ENDIF

    IF !EMPTY(lcPO)
      lcParam  = "'"+lcCall+"','"+lcPO + "'"
      DO gpDoProg WITH 'AWRMAPRCAM',.F.,'MA',lcParam
    ENDIF
  ELSE
    =gfModalGen('QRM36030B36000','ALERT', 'purchase orders|'+ALLTRIM(laData[1]) + '/'+ALLTRIM(laData[2]))  
  ENDIF
  SELECT (lcCursName)
  SET FILTER TO
  SET RELA TO
ELSE
  =gfModalGen('QRM36030B36000','ALERT', 'purchase orders|'+ALLTRIM(laData[1]) + '/'+ALLTRIM(laData[2]))
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfCalcOnOrd
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 08/21/02
*! Purpose   : Calculate the OnOrder qty for a specific 
*!           : Item\Color\Vendor 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
* --C102684,1
FUNCTION lfCalcOnOrd
PARAMETERS lcVen

PRIVATE lnAlias
lnAlias = SELECT()
SELECT POFLN
lcOldOrd = ORDER()
SET ORDER TO Poflnf
=SEEK(laData[1]+IIF(llAllColrs,'',laData[2]))
SUM REST WHILE fabric+color+cmattype+pomat+trancd = laData[1]+IIF(llAllColrs,'',laData[2]) IIF(TRANCD='1',IIF(POFLN.cMatType = 'P',nFabTOTQTY,-nFabTOTQTY),0.000) TO lnOnOrd FOR (POFLN.cMatType = 'P' OR POFLN.cMatType = 'R') ;
     AND VENDOR = lcVen
SET ORDER TO (lcOldOrd)
SELECT (lnAlias)
RETURN lnOnOrd
