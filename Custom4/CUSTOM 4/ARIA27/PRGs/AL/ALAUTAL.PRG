*:************************************************************************
*: Procedure file: ALAUTAL.PRG
*:
*:         System: ARIA 2.7
*:         Module: Allocation
*:         Author: Haytham el_Sheltawi
*:      Copyright (c) 
*:  Last modified: 05/22/1997
*:
*:  Procs & Fncts: lpShow
*:               : lfOpenFile
*:               : lfvScope
*:               : lfAloScr
*:               : lfRelScr
*:               : lfGenScr
*:               : lfDispBrow
*:               : lfwBrows
*:               : lfwOldVal
*:               : lfvStyle
*:               : lfvWareCode
*:               : lfvDyelot
*:               : lfvAloQty
*:               : lfvSelect
*:               : lfvSelAll
*:               : lfvSelNon
*:               : lfvInvert
*:               : lfvpbSel
*:               : lfwIbBrow
*:               : lfBrowTrap
*:               : lfBrwUnTrp
*:               : lfActPad
*:               : lfvActBar
*:               : lfTraps
*:               : lfFndUnCSe
*:               : lfvAccount
*:               : lfvOGStyle
*:               : lfvOrder
*:               : lfvFabric
*:               : lfvOrdWare
*:               : lfvCutTkt
*:               : lfvPO
*:               : lfvCutUnt
*:               : lfvPikCor
*:               : lfvPikSep
*:               : lfvPkFWrh
*:               : lfvSort1
*:               : lfvSort2
*:               : lfvSort3
*:               : lfvPikCorF
*:               : lfvPikSepF
*:               : lfvScopMod
*:               : lfvOGFrcAl
*:               : lfwOGWhen
*:               : lfCreatExp
*:               : lfGetExp
*:               : lfVarDefVl
*:               : lfSelData
*:               : lfExpFind
*:               : lfAllocate
*:               : lfAloQty
*:               : lfTmpAlo
*:               : lfRelQty
*:               : lfShowGets
*:               : lfGScalCnt
*:               : lfEditLine
*:               : lfGenPikTk
*:               : lfPrepVar
*:               : lfEmpty
*:
*:  Documented    06/00/1997
*:************************************************************************
*E300676,1 This program was coded by HS for the Enhancement
*:************************************************************************
*E300989,1 Mohamed Badran (MAB) 09/09/1998 Do the following
*E300989,1  - Exclude Option <Exclude from selected records>
*E300989,1  - Add  <Exclude> button to ALAUTAL2.SCX
*E300989,1  - Scope option Enable allover.
*E300989,1  - Adjust code for selecting By [P/O or C/T].
*E300989,1  - OG Filter on Style Major instead of Style.
*E300989,1  - OG Filter on Color Segment or Free Segment if found.
*E300989,1  - Change OG llExlStyDy Variable name to llRpExlDye to save its
*E300989,1  - Suppress OG Exclude Dyelot if system does not support dyelots.
*E300989,1  - Suppress OG Filter on P/O if P/O Module not installed.
*E300989,1  - Suppress OG Filter on C/T if C/T Module not installed.
*E300989,1    filter setting value.
*E300989,1  - Add messages to toolbar custom buttons.
*E300989,1  - Change laOGVrFlt To laFiltExp in all of its occurance.
*E301077,24 Mohamed Badran (MAB) 01/07/1999 Open and Close files.
*B801961,1 HS  03/23/1999 Declare some new arrays that will be used in the
*B801961,1                filter expression that will be returned by the
*B801961,1                option grid in the case of "In list" operator.
*B602547,1 Sameh 04/01/99 Correcting the spelling mistakes in 
*B602547,1                =gfModalGen("TRM44004B00000","ALERT")
*B602547,1 "picking tickets having packing lists where not released"
*B602547,1 (list) became (lists) & (where) became (were)
*B602519,1 MAB 04/05/1999 ReAllocate if there is partial allocate and we have availability.
*B602519,1                if there is no generated pick ticket for current line.
*B602748,1 MAB 04/06/1999 if you allocate group and finish skip -1 to start with next group
*B602748,1                after scan finish its job.
*B602570,1 MAB 04/06/1999 All previous allocated records must be selected by default.
*B602574,1 MAB 05/18/1999 Fix bug for variable laString1 not found.
*E301249,1 IHB 06/16/1999 Make priority field C(3) in CUSTOMER 
*E301249,1 				  and ORDHDR files
*C200084,1 MAB 07/06/1999 Add Fresh Produce business rules.
*MAB at 08/22/1999 Release option not in Options menu.
*E301225,1 MAB 08/23/1999 Allow Force allocation option.
*B802267,1 MAB 09/08/1999 Change all in List to In Range Operator.
*B603111,1 MAB 09/12/1999 Generate New/Add Message if pick ticket
*                         created in previous session only.
*B603239,1 MAB 10/27,1999 1- do not erase dyelot field if you release allocation
*                         2- Save current date when you generate Pick Ticket.
*E500304,1 WAB 11/27/1999 default the popoup ( INCLUDE WIP ) in Automatic alocation logic grid
*E500304,1 WAB            TO (YES Or NO) depend on new setting in IC module ( available 
*E500304,1 WAB            Qty = WIP + STQ - ALO OR available Qty = STQ - ALO )
*B802744,1 MAB 12/08/1999 Allocate and Pick non-inventory styles.
*B603457,1 KHM 02/16/2000 Fix the bug of not saving the sort options properly
*B603457,1 KHM            in the SortLevl.MEM file (Fresh P. Custom Process)
*B603464,1 KHM 02/21/2000 Fix the bug of not updating the ordline warehouse
*B603464,1                with the picked from warehouse and not restoring 
*B603464,1                the default warehouse of the ordline when releasing.
*B803078,1 MAB 03/07/2000 Pick Ticket not saved probably.
*B603544,1 MAB 04/09/2000 Complete data filter is on OrdLine.Complete not OrdHdr.Complete
*B603544,1 MAB            Note : all fixes is on SYDREPRT, and SYREPUVR, but I comment it here.
*B803138,1 MAB 05/18/2000 Apply Allocation logic dyelot case to separates also.
*B803138,1 MAB 05/18/2000 i.e. Separates will be allocated like coordinates.
*B603657,1 RAMY 05/28/2000 FIX THE BUG OF "WINDOW 'ORDER' HAS NOT BEEN DEFINED"
*B603691,1 MAB 06/20/2000 Continue allocation until reach end of a group.
*B803379,1 
*B603544,4 HBG 06/01/2000 Complete data filter could be on OrdLine.Complete or OrdHdr.Complete
*B603544,4 HBG            based on the setup of the company..
*B803379,1 HBG 07/05/2000 Fix the bug of when the user select anything for the option "Select By"
*B803379,1                it goes all the way up to the first option in the grid instead of remaining
*B803379,1                in the same field..
*B803340,1 HBG 07/06/2000 Fix the bug of The selection criteria 'Order Priority' not accept 
*B803340,1                more than one priority number for the criteria..If we select
*B803340,1                priority 125 for example the order of this criteria won't be show
*B803340,1                even it is exist..
*B802560,1 HBG 03/09/2000 Change option Title from "Generate nEW picking tickets" to "Generate New pick ticket No." IN OG
*B603691,1 MAB 10/12/2000 Allocate separates
*C102248,1 ABD 05/22/2001 Added option to allow select one or more HOLD REASONS
*C102248,1 ABD            The user will select form the mover the reasons for which the 
*C102248,1 ABD            on hold sales orders will be selected for allocation.
*C102282,1 ABD 05/24/2001 Custom change to prevent the user to allocat order from the program
*B804233,1 HBG 05/08/2001 Check order approve Amount to Allow allocate or not.
*C102424,1 ADEL 10/15/01 Add record for each piktkt in ALSHPINF for Ogio.
*B605077,1 BWA 10/31/2001 Fix the bug of the wrong sorting.
*B606442,1 TMI 09/23/2002 Include only Active customers in automatic allocation.
*B606569,1 ASH 10/28/2002 Display the Terms & ShipVia Desc. instead of codes in order browse. (Changes in SYREPUVR)
*B607414,1 ASH 07/14/2003 Fix the bug of 'Subscript out of bound' in lfGetDyelt function.
*B119304,1 ALB 08/31/2003 can't allocate SO by selected PO "No Records Found"
*C120733,1 WAB 12/13/2003 Add 2 new filters on the Automatic Allocation program ( option grid) 
*C037824,1 TMI 03/14/2004 Add a trigger : a new filter "Style" in case Exclude is "No" for STU10
*C037816,1 MHM 04/06/2004 Custom Bin Location For David Luke
*C124051,1 TMI 11/17/2004 if totstk is less than total allocated for at least one color then ignor
*C124051,1 TMI            the whole style - for NOR01.
*C123847,5 TMI 03/21/2005 Always generate a new Piktkt for DIR03
*E126905,1 EIH 04/11/2005 Update in new field 'CALLOCATB' by 'A'in case of auto allocation or ' ' for cancel allocation .
*E129473,1 MHM 08/21/2005 Generate 940 EDI transaction for pick tickets directed to 3PL providers.
*B039660,1 NNA 02/14/2006 Transfer all the bin location's Programs To Binmain.prg instead of Davmain.prg and Check first if
*B039660,1 NNA            there is enough stock or not before making update in the Bin location Files
*:***********************************************************************************************************************
*
*-- 
EXTERNAL ARRAY laScrMode
** lcStyle      Variable to create the Style GET field on the screen
** lcStore      Variable to create the Store SAY field on the screen
** lcGroup      Variable to create the Group SAY field on the screen
** lcWareCode   Variable to create the Warehouse GET field on the screen
** lcDyelot     Variable to create the Dyelot GET field on the screen
** lcSize1      Variable to hold a Title for the 1st. size column
** lcSize2      Variable to hold a Title for the 2nd. size column
** lcSize3      Variable to hold a Title for the 3rd. size column
** lcSize4      Variable to hold a Title for the 4th. size column
** lcSize5      Variable to hold a Title for the 5th. size column
** lcSize6      Variable to hold a Title for the 6th. size column
** lcSize7      Variable to hold a Title for the 7th. size column
** lcSize8      Variable to hold a Title for the 8th. size column
** lcAutAlCh1   Variable to hold a Temp. name for one of the screens [The 1st. child] in the screen set
** lcAutAlCh2   Variable to hold a Temp. name for one of the screens [The 2nd. child] in the screen set
** lcAutAlCh3   Variable to hold a Temp. name for one of the screens [The 3rd. child] in the screen set
** lcOrdLBrow   Variable to hold the Browse title
** lcTmpOrdLn   Variable to hold a Temp. name for the Temp. Order Lines file
** lcTmpStore   Variable to hold a temp. name for stores file.
** lcSelAllSt   Variable to hold the Select All button status 
** lcSelNonSt   Variable to hold the Select None button status
** lcAloSt      Variable to hold the Allocate selected records button status
** lcRelSt      Variable to hold the Release selected records button status
** laOldVal     Variable to hold the Old value
** lcRpScpMod   Variable to hold the Select By [From the Option grid]
** lcOldScMod   Variable to hold the Old value of Select By [From the Option grid]
** lcRpPkFWrh   Variable to hold the Pick from warehouse
** lcIndexExp   Variable to hold the Index expression to sort the Temp. Order Lines file with
** lcOldIndex   Variable hold old index value.
** lcCh3Stat    Variable to hold the status of the 3rd. child screen in the screen set
** lcDyelotSt   Variable to hold the status of the Dyelot get field

*E300989,1 Now There is no need to lcScopSt variable because Scope is Enabled allover .
** lcScopSt     Variable to hold the status of the Scope button
*E300989,1 But Adding this variable to hold <Exclude> button status.
** lcRpSepCor   Variable to hold Separates or coordinate group.
** lcRpAloNot   Variable hold Allocated / Not Allocated status.
** lnRpGenNew   Generate New pick ticket variable.

** lcInvertSt   Variable to hold the status of the Invert button
** lcForceBmp   Variable to hold the Force allocation button BMP name and path in the case of Force allocation Yes
** lcNForcBmp   Variable to hold the Force allocation button BMP name and path in the case of Force allocation No
** lcCurrObj    Variable to hold the name of the incompleted function 
** lcStyleTtl   Variable to hold the Style field Title
** lcStylePct   Variable to hold the Style field Mask
** lcTmpIndex   Variable to hold a Temp. name to create an Index in the Temp. Order Lines file with it
** lcStyScale   Variable to hold the Scale of the Style in the Style get Field
** lcBrowFild   Variable to hold the Browse fields
** lcSession    Variable to hold the program session number

** lnPrnRecNo   Record no. before printing.
** lnBrRecNo    Variable to hold the Browse Record number
** lnTotPik     Variable to hold the Total Picked quantity
** lnTotQty     Variable to hold the Total Quantity
** lnSelRec     Variable to hold the number of selected records
** lnSelAlo     Variable to hold the number of selected and allocated records
** lnAloRec     Variable to hold the number of allocated records
** lnDellRec    Variable to hold the number of deleted records in the Temp. Order Lines file
** lnRpCutUnt   Variable to hold the Cut of units [From the Option grid]
** lnRpPikCor   Variable to hold the Pick coordinate Min. %
** lnRpPikSep   Variable to hold the Pick separates Min. %
** lnRpSort1    Variable to hold the user 1st. Sort option
** lnRpSort2    Variable to hold the user 2nd. Sort option
** lnRpSort3    Variable to hold the user 3rd. Sort option
** lnRpSort4    Variable to hold the user 4th. Sort option
** laSortAry    Variable to hold user select sorted.
** lnRecNumbr   Variable to hold the number of the undeleted records in the Temp. Order Lines file
** lnChangAlo   Variable to hold the number of the changed allocated quantity in the current record
** lnUnCmRec    Variable to hold the record number of the incompleted session file
** lnStyleWid   Variable to hold the width of the Style field
** lnWareCode   Variable to create the warehouse popup

*E300989,1 The following numeric variables is add by MAB [begin]
*E300989,1 to do OG Filter By Style Major, Color Seg., Free Seg.,
*E300989,1 and style Major titles for all of its fields.
** lnMajorLen   Variable to Hold Major Length.
** lnNonMajSt   Non major (Color/Free) start filter position.
** lnSupMajSt
** lnFreeLen    Free segment length.
** lnColorLen   Color segment length.
** lnMajSeg     Number of major segments.
** lnDummyPos   Position of dummy variable in filter array.
*E300989,1 The following numeric variables is add by MAB [end]

** llForceAlc   Flag to know if we are going to Force the allocation
** llMultWare   Flag to know if the system is multi warehouse
** llUseDyes    Flag to know if the system use Dyelots
** llContinue   Flag to know if there is incompleted session
** llCallScop   Flag to know if we are going to call the Option grid
** llStylRel    Flag to know if we need to establish a relationship between the ORDLINE file and the STYLE file
** llOrdrRel    Flag to know if we need to establish a relationship between the ORDLINE file and the ORDHDR file
** llRpIncHOr   Flag to know if we are going to include the orders in hold
** llRpForAlo   Flag to know if we are going to Force the allocation [From the Option grid]
** llStartSlc   Only .T. if user start from selection grid.
*--llOpnPack    .T. if we open pack header file in this program.
*--llOpnPikLn   .T. if we open Pick Line file in this program.
  
*E300989,1  Now There is no need to this flag because Scope is Enabled allover .
** llScope      Flag to know if the Scope bar in the Option menu is to be disabled

*E300989,1 llExclude  : Flag is .T. when user make Exclude option.
*E300989,1 llRpGdExcl  : Flag is .T. when user press < Exclude > button is selection grid.

*E300989,1 llRpCond   : new OG Flag is .T. if we allocate conditionally.
*E300989,1 llRpIncWip : new OG Flag is .T. if include work process
*E300989,1            : specially (P/O or C/T) case only. 
** llRpExcOrd         : Activate exclude filter.
** llMustLoop         : To loop avoiding nesting error.
*E300989,1 llFabDye   : .T. if material use dyelots.  

** llBrowse     Flag to know if the User is calling the Browse from any of the Fields browse buttons
** llIncOrd     Flag to know if the User has increased any of the Ordered quantities
** llAloRec     Flag to know if the User has made any changes in the allocated quantities or increased any of the Ordered quantities from the Allocated Get Fields
** llRpPikSep   Flag to know if the User want to allocate the Order lines records that dose not have a group
** llRpPikCor   Flag to know if the User want to allocate the Order lines records that have a group
** llRpGenPik   Flag to know if the User want to Generate Pick tickets for the allocated lines [From the Option grid]
** llRpAlocat   Flag to know if the User want allocated lines [From the Option grid]
** llRpExlBlk   Flag to know if the User want to Exclude the Bulk orders

*E300989,1 Change Name of variable llExlStyDy to save its value when 
*E300989,1 making filter setting.
** llExlStyDy   Flag to know if the User want to Exclude Styles that is Dyelot yes
** llRpExlDye   Flag to know if the User want to Exclude Styles that is Dyelot yes

** llCallUnCS   Flag to know if we are going to call lfFndUnCSe from lpShow

** laStorStru   Array to hold store file structure.
** laFileStru   Array to copy the ORDLINE file structure into
** laUsedFile   Array to hold the name of the files to be opened
** laScopExpr   Array to hold the Filter expression of the Scope file [The Select by file]
** laNormExpr   Array to hold the Filter expression of the none Scope files [all but the Select by files]
** laVldOper    Array to hold the Valid Operators for the deferent field Types
** laIndexExp   Array to hold the Valid fields to Sort the Temp. Order Lines file with
** laPik        Array to hold the Allocated quantities and to create the Allocated Get Fields
** laQty        Array to hold the Ordered quantities and to create the Ordered Say Fields
** laPikSt      Array to hold the Allocated Get fields Status
** laVarTSav    Array to hold the Variables to be Saved in the incompleted session file
** laWareHous   Array to hold the style inventory warehouse desc.
** laWareCode   Array to hold the style inventory warehouse codes

** laDyeStru    Array hold Dye_Rel Temp. File structure

*E300989,1 Now There is no need to lcScopSt variable because Scope is Enabled allover .
*STORE '' TO lcStyle , lcStore , lcGroup , lcWareCode , lcDyelot ,;
*            lcSize1 , lcSize2 , lcSize3 , lcSize4 , lcSize5 , lcSize6 ,;
*            lcSize7 , lcSize8 , lcAutAlCh1 , lcAutAlCh2 , lcAutAlCh3 ,;
*            lcOrdLBrow , lcTmpOrdLn , lcSelAllSt , lcSelNonSt , lcAloSt ,;
*            lcRelSt , laOldVal , lcRpScpMod , lcOldScMod , lcRpPkFWrh ,;
*            lcIndexExp ,  lcCh3Stat , lcDyelotSt , lcScopSt , lcInvertSt ,;
*            lcForceBmp , lcCurrObj , lcStyleTtl , lcStylePct , lcTmpIndex ,;
*            lcStyScale , lcBrowFild , lcSession , lcNForcBmp

*E300989,1 The following Character variables is add by MAB [begin]
*E300989,1 Add Initial value for new index expression, and <Exclude> button status.
*E300989,1 lcTmStyTag,lcOldSort
*E300989,1 to do OG Filter By Style Major, Color Seg., Free Seg.,
*E300989,1 and style Major titles for all of its fields.
** lcStyMajor   Variable to hold Style major title   ...
** lcMajorPic   Variable to hold style major picture ...
** lcColorTlt   Variable to hold Color segment title.
** lcNonMajTlt  Variable to hold Non Major title .
** lcNonMajPi   Variable to hold Non major picture.
** lcFree_Clr   Variable to hold 'C' for Color seg. and 'F' for Free seg. ..
** laSlctDesc   Variable to hold Select By array data.
** laSlctVals   Variable to hold Select By array Values.
** laSortDesc   Array hold report sort by description
** laSortVal    Array hold report sort by values.
** lcSeason     Variable Hold Style season title.
** lcDivision   Variable Hold Style division title.
** lcStyGroup   Variable Hold Style group title.
** lcFabTlt     Variable Hold Style Fabric title.
** lcPatTlt     Variable Hold Style pattern title.
** lcRpExSlct   Exclude option Select By.
** lcOldSort    Old Sort By value in Print grid.
*--lcTmStyTag   Name of Temp. index used in collectiong data In Exclude Style case .
*-- laIncExprs  Array that save values of Include Filter.
*-- laExcExprs  Array that save values of Exclude Filter.
*-- llPitch     if another dyelot transfered to forward or backward only.

STORE '' TO lcStyle , lcStore , lcGroup , lcWareCode , lcDyelot ,;
            lcSize1 , lcSize2 , lcSize3 , lcSize4 , lcSize5 , lcSize6 ,;
            lcSize7 , lcSize8 , lcAutAlCh1 , lcAutAlCh2 , lcAutAlCh3 ,;
            lcOrdLBrow , lcTmpOrdLn , lcSelAllSt , lcSelNonSt , lcAloSt ,;
            lcRelSt , laOldVal , lcRpScpMod , lcRpPkFWrh ,;
            lcIndexExp ,  lcCh3Stat , lcDyelotSt , lcInvertSt ,;
            lcCurrObj , lcStyleTtl , lcStylePct , lcTmpIndex ,;
            lcStyScale , lcBrowFild , lcSession , lcRpSepCor ,;
            lcRpAloNot , lcOldIndex , lcCurStyWr,lcTmStyTag,lcOldSort,;
            lcTmpStore , laStorStru

*E300989,1 Initial value for Character OG Filter option 
*E300989,1 variables declared before. [begin]
STORE '' TO lcStyMajor,lcMajorPic,lcColorTlt,lcNonMajTlt,lcNonMajPi,;
            lcFree_Clr,lcSeason,lcDivision,lcStyGroup,lcFabTlt,lcPatTlt,;
            laSlctDesc,laSlctVals,laSortDesc,laSortVal,laFiltExp,;
            laIncExprs,laExcExprs

STORE ' ' TO lcRpExSlct,lcOldSlct,lcOldScMod

*E300989,1 [end]

*B802267,1 Define Sales order Status Variable [Begin]
STORE '' TO lcSOrdStat , lcBulkExp , lcOptmFile
lnRngAlias = 0
*B802267,1 Define Sales order Status Variable [End  ]

*C200084,1 Add Sort4 level required by FP to standard. [Begin]
*STORE 0 TO lnBrRecNo , lnTotPik , lnTotQty , lnSelRec , lnSelAlo ,;
*           lnAloRec , lnDellRec , lnRpCutUnt , lnRpPikCor , lnRpPikSep ,;
*           lnRpSort1 , lnRpSort2 , lnRpSort3 , lnRecNumbr , lnChangAlo ,;
*           lnUnCmRec , lnStyleWid , lnWareCode,lnPrnRecNo,lnMajorLen,;
*           lnMajSeg,lnNonMajSt,lnSupMajSt,lnFreeLen,lnColorLen,lnDummyPos

DIMENSION laSortAry[4,2]
laSortAry = ''
STORE 0 TO lnBrRecNo , lnTotPik , lnTotQty , lnSelRec , lnSelAlo ,;
           lnAloRec , lnDellRec , lnRpCutUnt , lnRpPikCor , lnRpPikSep ,;
           lnRpSort1 , lnRpSort2 , lnRpSort3 , lnRecNumbr , lnChangAlo ,;
           lnUnCmRec , lnStyleWid , lnWareCode,lnPrnRecNo,lnMajorLen,;
           lnMajSeg,lnNonMajSt,lnSupMajSt,lnFreeLen,lnColorLen,lnDummyPos,;
           lnRpSort4
*C200084,1 Add Sort4 level required by FP to standard. [Begin]

lnPitch    = 1    && Select dyelot pitch.(1 or -1)
lnRpGenNew = 2


*E300989,1  Add New Logical variables.
*STORE .F. TO llForceAlc , llMultWare , llUseDyes , llContinue , llCallScop ,;
*             llStylRel , llOrdrRel , llRpIncHOr , llRpForAlo , llScope ,;
*             llBrowse , llIncOrd , llAloRec , llCallUnCS

*-- llPartAlo : .T. if any of selected records have partial allocation.

*B603544,4 HBG 06/04/2000 give intial value to llTotAvlbl,llLinCmplt [Begin]
*STORE .F. TO llMultWare , llUseDyes , llContinue , llCallScop ,;
             llStylRel , llOrdrRel , llRpIncHOr ,;
             llBrowse , llIncOrd , llAloRec , llCallUnCS,llOpnPack,;
             llOpnPikLn,llPitch,llExclude,llRpCond,llRpExcOrd,llCalWip,;
             llRpIncWip,llFabDye,llMustLoop,llStartSlc,;
             llRpGenPik,llRpAlocat,llPartAlo

STORE .F. TO llMultWare , llUseDyes , llContinue , llCallScop ,;
             llStylRel , llOrdrRel , llRpIncHOr ,;
             llBrowse , llIncOrd , llAloRec , llCallUnCS,llOpnPack,;
             llOpnPikLn,llPitch,llExclude,llRpCond,llRpExcOrd,llCalWip,;
             llRpIncWip,llFabDye,llMustLoop,llStartSlc,;
             llRpGenPik,llRpAlocat,llPartAlo,llTotAvlbl,llLinCmplt
*B603544,4 HBG 06/04/2000 give intial value to llTotAvlbl,llLinCmplt [End  ]
             
*E301225,1 Force allocation [Begin]
lcAlwForce = ''
STORE .F. TO llAlwForce , llForceAll
*E301225,1 Force allocation [End  ]

*STORE .T. TO llRpPikSep , llRpPikCor , llRpGenPik , llRpExlBlk
*E300989,1 Exclude dyelot flag have Rp Convension.
STORE .T. TO llRpPikSep , llRpPikCor , llRpExlBlk,llRpGdExcl,llRpExlDye,;
             llRpForAlo
*E300989,1 Initial value for Exclude and OG Allocate conditional Flags.

*E300989,1 Old laVarTSav dimension is 9 but now I change it to 10 to save new index
*E300989,1 laUsedFile length depends on If material module is installed or not.
*E300989,1 also depends on if material use dyelot or no.
*DECLARE laFileStru[1] , laUsedFile[9,3] , laScopExpr[1,2] ,;
*        laNormExpr[1,2] , laVldOper[8] , laIndexExp[5] , laPik[8] ,;
*        laQty[8] , laPikSt[8] , laVarTSav[9,1] , laWareHous[1] ,;
*        laWareCode[1]

DECLARE laFileStru[1] , laScopExpr[1,2] ,laNormExpr[1,2] , laVldOper[8] ,;
        laIndexExp[6] , laPik[8] , laQty[8] , laPikSt[8] , laDyeStru[1] ,;
        laVarTSav[10,1] , laWareHous[1] , laWareCode[1]  , laWip[9]     ,;
        laUsedFile[7,2]

*B801961,1 Add this line to declare some new arrays that will be used in the
*          filter expression that will be returned by the option grid in
*          the case of "In list" operator [Begin]
DIMENSION laHdFlt[1] , laFxFlt[1] , laVrFlt[1]
*B801961,1 Add this line to declare some new arrays that will be used [End]

*lcForceBmp = gcBmpHome + 'FORCALO.BMP'
*lcNForcBmp = gcBmpHome + 'NFORCALO.BMP'
lcSelAllSt = 'DISABLE'
lcSelNonSt = 'DISABLE'
lcAloRelSt = 'DISABLE'
lcAloSt    = 'DISABLE'
lcRelSt    = 'DISABLE'
lcInvertSt = 'DISABLE'

*E300989,1 Now There is no need to lcScopSt variable because Scope is Enabled allover .
*lcScopSt   = 'ENABLE'

lcCh3Stat  = 'DISABLE'
lcDyelotSt = 'DISABLE'
laPikSt    = 'DISABLE'

lcOrdLBrow = 'Order Lines'
lcDyelot   = SPACE(10)

lnSelRec   = 0
lnRpPikCor = 100
lnRpPikSep = 100
lnRpSort1  = 1
lnRpSort2  = 2
lnRpSort3  = 3

*C200084,1 Add Sort4 level required by FP to standard. [Begin]
lnRpSort4  = 4
laSortAry[1,1] = 1
laSortAry[1,2] = [DTOS(COMPLETE)]
laSortAry[2,1] = 2
laSortAry[2,2] = [PRIORITY]
laSortAry[3,1] = 3
laSortAry[3,2] = [DTOS(START)]
laSortAry[4,1] = 4
laSortAry[4,2] = [ORDER]
*C200084,1 Save Default Sort Array to memory file. [Begin]

*B603457,1 KHM 02/16/2000 (Begin) The following lines are commented out.
*IF ASCAN(laEvntTrig,PADR("SAVESORT",10)) <> 0
*  = gfDoTriger('ALAUTAL','SAVESORT')
*ENDIF
*B603457,1 KHM 02/16/2000 (End)

*C200084,1 Save Default Sort Array to memory file. [End  ]

*C200084,1 Add Sort4 level required by FP to standard. [End  ]

laWareHous = ''
laWareCode = ''

laPik = 0
laQty = 0
*E300989,1 laWip : Array save wip values and used if user want to include 
*E300989,1         Work Process.
laWip = 0

laScopExpr[1,1] = 0
laScopExpr[1,2] = ''
laNormExpr[1,1] = 0
laNormExpr[1,2] = ''

laVldOper[1] = 'CMLND'
laVldOper[2] = 'CMND'
laVldOper[3] = 'CMND'
laVldOper[4] = 'CMND'
laVldOper[5] = 'CMND'
laVldOper[6] = 'CND'
laVldOper[7] = 'M'
laVldOper[8] = 'CND'

laIndexExp[1] = 'DTOS(COMPLETE)'
laIndexExp[2] = 'PRIORITY'
laIndexExp[3] = 'DTOS(START)'
laIndexExp[4] = 'ORDER'
laIndexExp[5] = 'ACCOUNT'
laIndexExp[6] = ''
*E301077,24 Order Line file opened from module object [Begin]
*E301077,24 Thus no need to open it here.
laUsedFile[1,1] = 'PIKTKT'
laUsedFile[1,2] = 'PIKTKT'
laUsedFile[2,1] = 'STYLE'
laUsedFile[2,2] = 'STYLE'

*laUsedFile[3,1] = 'ORDLINE'
*laUsedFile[3,2] = 'ORDLINST'
*laUsedFile[4,1] = 'SCALE'
*laUsedFile[4,2] = 'SCALE'
*laUsedFile[5,1] = 'ORDHDR'
*laUsedFile[5,2] = 'ORDHDR'
*laUsedFile[6,1] = 'STYDYE'
*laUsedFile[6,2] = 'STYDYE'
*laUsedFile[7,1] = 'CUSTOMER'
*laUsedFile[7,2] = 'CUSTOMER'

laUsedFile[3,1] = 'SCALE'
laUsedFile[3,2] = 'SCALE'
laUsedFile[4,1] = 'ORDHDR'
laUsedFile[4,2] = 'ORDHDR'
laUsedFile[5,1] = 'STYDYE'
laUsedFile[5,2] = 'STYDYE'
laUsedFile[6,1] = 'CUSTOMER'
laUsedFile[6,2] = 'CUSTOMER'
laUsedFile[7,1] = 'WAREHOUS'
laUsedFile[7,2] = 'WAREHOUS'

laVarTSav[1,1] = 'lnRpCutUnt'
laVarTSav[2,1] = 'lnRpPikCor'
laVarTSav[3,1] = 'lnRpPikSep'
laVarTSav[4,1] = 'lcRpPkFWrh'
laVarTSav[5,1] = 'llRpForAlo'
laVarTSav[6,1] = 'llRpGenPik'

*laVarTSav[7,1] = 'llForceAlc'
laVarTSav[7,1] = 'lcCurrObj'
laVarTSav[8,1] = 'lcTmpIndex'

*E300989,1 Save value of Temporary Style index.
laVarTSav[9,1] = 'lcTmStyTag'
laVarTSav[10,1] = 'llRpAlocat'
*E300989,1

*B603111,1 Name of File hold piktkts generated in this session [Begin]
STORE '' TO lcTmpPkTk , lcTmpRelPk , lcRelLine
*B603111,1 Name of File hold piktkts generated in this session [End  ]

lnSessNo = gnProgCopy        && Varible to hold the Instant number

IF !gfSetup()
  RETURN
ENDIF    && End of IF

*E500304,1 WAB - get llTotAvlbl setting 
*E500304,1 WAB - llTotAvlbl = .F. --> avlbel qty = stq - alocated qty
*E500304,1 WAB - llTotAvlbl = .T. --> avlbel qty = WIP + stq - alocated qty
*E500304,1 WAB - START


*C102282,1 ABD - Add flage to prvent user to allocate qty from this program. [Begin]
*-- This flage to detrimined if this program run into this customer.
llPikNow = .T.
IF ASCAN(laEvntTrig,PADR("LLPIKNOW",10)) <> 0
  llPikNow = .F.
ENDIF  
*C102282,1 ABD - [End]

*C120733,1 WAB (Start) call trigger to get the States Title 
IF ASCAN(laEvntTrig,PADR("STITLE",10)) <> 0
  STORE '' TO lcStores , lcStates
ENDIF
*C120733,1 WAB (End) 

*B603544,4 (HBG) 06/01/2000 comment the flag of the setup 'M_TOTAVLBL' this setup will be
*                           added to the array laSetups ,and the flag will take the value
*                           of this setup from the array [Begin]
*llTotAvlbl = gfGetMemVar('M_TOTAVLBL')  
*B603544,4 (HBG) 06/01/2000 comment the flag of the setup 'M_TOTAVLBL' [End  ]

*E500304,1 WAB - END

*E301077,24 I transfer this block of here optimizing process if
*E301077,24 I must return here.
*E301077,24 Open files after check avoiding open them then colse. [Begin]
*=lfOpenFile(.T.)
*IF There is no lines in the Order lines file
IF RECCOUNT('ORDLINE') = 0
  
  *** Message : "There is no records in the ð file can not proceed."
  ***           "                     < Ok >                       "
  =gfModalGen("TRM44010B00000" , "DIALOG" , "order lines")
  
  *E301077,24 Open files now is ofter this block there is no need to close it.
  *=lfOpenFile(.F.)
  RETURN
ENDIF    && End of IF
*=lfOpenFile(.T.)
*E301077,24 Open files after check avoiding open them them colse. [End  ]

*E301077,24 Call gfGetMemVar Once per all setup variables. [Begin]
*Setups {Begin}
*E301225,1 Allow Force allocation option. [Begin]
*DECLARE laSetups[3,2]

*B603544 (HBG) 06/01/2000 Add 2 rows to the array laSetups [Begin]
*DECLARE laSetups[4,2]
*B804233,1 HBG 05/08/2001 Add row to the array laSetups to get the approve Amount Setup [Begin]
*DECLARE laSetups[6,2]
DECLARE laSetups[7,2]
*B804233,1 [End]
*B603544 (HBG) 06/01/2000 Add 2 rows to the array laSetups [End  ]

laSetups[1,1] = 'M_DYELOT'
laSetups[2,1] = 'M_MATDYE'
laSetups[3,1] = 'M_WareHouse'
laSetups[4,1] = 'M_FORCEALO'

*B603544 (HBG) 06/01/2000 Add 2 Setups (M_TOTAVLBL , M_CMPDOLN) to the array laSetups [Begin]
laSetups[5,1] = 'M_TOTAVLBL'
laSetups[6,1] = 'M_CMPDOLN'
*B603544 (HBG) 06/01/2000 Add 2 Setups (M_TOTAVLBL , M_CMPDOLN) to the array laSetups [End  ]

*B804233,1 HBG 05/08/2001 Get the approve Amount Setup [Begin]
laSetups[7,1] = 'M_CHKAPROV'
*B804233,1 [End]

*E301225,1 Allow Force allocation option. [End  ]

=gfGetMemVar(@laSetups,gcAct_Comp)

*llUseDyes  = IIF(gfGetMemVar('M_DYELOT') = 'Y' , .T. , .F.)
*llFabDye   = ('MA' $ gcCmpModules) AND (gfGetMemVar('M_MATDYE') = 'Y')
*llMultWare = IIF(gfGetMemVar('M_WareHouse') = 'Y' , .T. , .F.)

llUseDyes  = (UPPER(ALLTRIM(laSetups[1,2])) = 'Y')
llFabDye   = ('MA' $ gcCmpModules) AND (UPPER(ALLTRIM(laSetups[2,2])) = 'Y')
llMultWare = (UPPER(ALLTRIM(laSetups[3,2])) = 'Y')

*B603544 (HBG) 06/01/2000 Add Flag to hold the valu of the setup (M_CMPDOLN) [Begin]
llTotAvlbl = laSetups[5,2]
llLinCmplt = laSetups[6,2]
*B603544 (HBG) 06/01/2000 Add Flag to hold the valu of the setup (M_CMPDOLN) [END  ]

*B804233,1 HBG 05/08/2001 Get the approve Amount Setup [Begin]
llChkAprov = laSetups[7,2]
*B804233,1 [END]

*E301225,1 Allow Force allocation option. [Begin]
lcAlwForce = ALLTRIM(laSetups[4,2])

*B039660,1 NNA 02/14/2006 (BEGIN) Stop the force alloction if use bin location set to yes.
IF ASCAN(laEvntTrig,PADR("CHKFORCE",10)) <> 0 AND gfDoTriger('ALAUTAL',PADR("CHKFORCE",10))
  lcAlwForce = "N"
ENDIF
*B039660,1 NNA (END)


llAlwForce = lfSuppForc()
*E301225,1 Allow Force allocation option. [End  ]

*E301077,24 Call gfGetMemVar Once per all setup variables. [End  ]
*Setups {End..}

*E300989,1 Change the following lines because length of laUsedFile depends
*E300989,1 on Installization of Material module.
*laUsedFile[8,1] = 'FABRIC'
*laUsedFile[8,2] = 'CFABRIC'
*laUsedFile[9,1] = 'WAREHOUS'
*laUsedFile[9,2] = 'WAREHOUS'

IF 'MA' $ gcCmpModules
  DIMENSION laUsedFile[ALEN(laUsedFile,1)+1,2]

  laUsedFile[ALEN(laUsedFile,1),1] = 'FABRIC'
  laUsedFile[ALEN(laUsedFile,1),2] = 'CFABRIC'

  IF llFabDye
    DIMENSION laUsedFile[ALEN(laUsedFile,1)+2,2]

    laUsedFile[ALEN(laUsedFile,1)-1,1] = 'DYE_REL'
    laUsedFile[ALEN(laUsedFile,1)-1,2] = 'SEQUENCE'

    laUsedFile[ALEN(laUsedFile,1),1] = 'BOM'
    laUsedFile[ALEN(laUsedFile,1),2] = 'BOM'
  ENDIF
ENDIF  
*E300989,1 [end.]
*E301077,24 Order Line file opened from module object [End  ]

*IF The Manufacturing module is installed
IF 'MF' $ gcCmpModules
  DIMENSION laUsedFile[ALEN(laUsedFile,1)+2,2]
  laUsedFile[ALEN(laUsedFile,1)-1,1] = 'CUTTKTH'
  laUsedFile[ALEN(laUsedFile,1)-1,2] = 'CUTTKTH'

  laUsedFile[ALEN(laUsedFile,1),1] = 'CUTTKTL'
  laUsedFile[ALEN(laUsedFile,1),2] = 'CUTTKTLS'
ENDIF    && End of IF

*IF The Purchase order module is installed
IF 'PO' $ gcCmpModules
  DIMENSION laUsedFile[ALEN(laUsedFile,1)+3,2]
  laUsedFile[ALEN(laUsedFile,1)-2,1] = 'POSHDR'
  laUsedFile[ALEN(laUsedFile,1)-2,2] = 'POSHDR'

  laUsedFile[ALEN(laUsedFile,1)-1,1] = 'APVENDOR'
  laUsedFile[ALEN(laUsedFile,1)-1,2] = ''

  laUsedFile[ALEN(laUsedFile,1),1] = 'POSLN'
  laUsedFile[ALEN(laUsedFile,1),2] = 'POSLNS'
ENDIF    && End of IF

*IF The Manufacturing module or the Purchase order module is installed
IF 'MF' $ gcCmpModules .OR. 'PO' $ gcCmpModules
  DIMENSION laUsedFile[ALEN(laUsedFile,1)+1,2]
  laUsedFile[ALEN(laUsedFile,1),1] = 'CUTPICK'
  laUsedFile[ALEN(laUsedFile,1),2] = 'CUTPICK'
ENDIF    && End of IF

=lfOpenFile()  && Open all files used by this screen.
*IF There is no record with Pick ticket number '******' in the PIKTKT file
IF !SEEK('******' , 'PIKTKT')
  INSERT INTO PIKTKT (PikTkt) VALUES ('******')
  =gfAdd_Info('PIKTKT')
ENDIF    && End of IF

*C200084,1 Fresh produces custom proram [Begin]
lnPanArrLn = 4  && Pannel Array length.

IF ASCAN(laEvntTrig,PADR("SPC_SETS",10)) <> 0
  lcSetSectn = "INI"  && Parameter passed to setting function.
  = gfDoTriger('ALAUTAL','SPC_SETS')
ENDIF  

*C102282,1 ABD - Add flage to prvent user to allocate qty from this program. [Begin]
*-- This flage to detrimined if this program run into this customer.
IF !llPikNow
  lnPanArrLn = lnPanArrLn -1
ENDIF
*C102282,1 ABD - [End]

*DIMENSION laPanelObj(4,3)    && Array to add new buttons to the control panel
DIMENSION laPanelObj[lnPanArrLn,3]    && Array to add new buttons to the control panel
laPanelObj = ""
*C200084,1 Fresh produces custom proram [End  ]

*E300989,1 Now There is no need to lcScopSt variable because Scope is Enabled allover .
*E300989,1 Also I add message to all Buttons.
*laPanelObj[1,3] = 'VALID lfvScope() &lcScopSt'
laPanelObj[1,1] = 'pbScop'
laPanelObj[1,2] = gcBmpHome + 'SCOPE.BMP'
*B804233,1 HBG 05/08/2001 Add function to Zap The temp Aprove file to allow Checking 
*B804233,1                Order Approve Amount agian. [Begin]
*laPanelObj[1,3] = 'VALID lfvScope() MESSAGE "Scope"'
laPanelObj[1,3] = 'VALID lfAprov() AND lfvScope() MESSAGE "Scope"'
*B804233,1 [End]
*E300989,1 [end.]

*E300989,1 Add allocation Logic before allocation.
*laPanelObj[2,3] = 'VALID lfAloScr() MESSAGE "Allocate selected records" &lcAloSt'
laPanelObj[2,1] = 'pbAlo'
laPanelObj[2,2] = gcBmpHome + 'PICK.BMP'
*B804233,1 HBG 05/08/2001 Add function to Zap The temp Aprove file to allow Checking 
*B804233,1                Order Approve Amount agian. [Begin]
*laPanelObj[2,3] = 'VALID lfAlocGrid() AND llRpAlocat AND lfAloScr() AND llRpGenPik AND lfGenScr() MESSAGE "Allocate selected records" &lcAloSt'
laPanelObj[2,3] = 'VALID lfAprov() AND lfAlocGrid() AND llRpAlocat AND lfAloScr() AND llRpGenPik AND lfGenScr() MESSAGE "Allocate selected records" &lcAloSt'
*B804233,1 [End]

laPanelObj[3,1] = 'pbRel'
laPanelObj[3,2] = gcBmpHome + 'RELEASE2.BMP'
laPanelObj[3,3] = 'VALID lfRelScr() MESSAGE "Release selected records" &lcRelSt'

*E300989,1 Add Generate picktkt Logic before generate.
*laPanelObj[4,3] = 'VALID lfGenScr() MESSAGE "Generate picking tickets" &lcRelSt'




*C200084,1 Generate pick ticket button if not FP program [Begin]
IF TYPE("laPanelObj[4,1]") = "C"
    laPanelObj[4,1] = 'pbGen'
    laPanelObj[4,2] = gcBmpHome + 'GENERAT.BMP'
    *B804233,1 HBG 05/08/2001 Add function to Zap The temp Aprove file to allow Checking 
    *B804233,1                Order Approve Amount agian. [Begin]
    *laPanelObj[4,3] = 'VALID lfPickGrid() AND llRpGenPik AND lfGenScr() MESSAGE "Generate picking tickets" &lcRelSt'
    laPanelObj[4,3] = 'VALID lfAprov() AND lfPickGrid() AND llRpGenPik AND lfGenScr() MESSAGE "Generate picking tickets" &lcRelSt'
    *B804233,1 [End]
ENDIF  
*C200084,1 Generate pick ticket button if not FP program [End  ]

IF !WEXIST(gcBaseWind)

  =lfGetWareH()

  *E300989,1 Assign variables for selection and Logic OGs. [Begin
  STORE 'B' TO lcRpSepCor,lcRpAloNot
  STORE ' ' TO lcRpScpMod
  STORE '' TO  laFiltExp,laIncExprs,laExcExprs
  
  STORE .F. TO llStartSlc

  lcOldSlct = ' '
  
  lnRpGenNew = 2
  
  STORE .F. TO llExclude
  llRpGdExcl = .T.

  STORE .F. TO llMustLoop

  lcRpExSlct = ' '
  DIMENSION laWip[9]
  laWip = 0 
  *E300989,1 Assign variables for selection and Logic OGs. [Begin
   
  lcAutAlCh1 = gfTempName()
  lcAutAlCh2 = gfTempName()
  lcAutAlCh3 = gfTempName()

  lcStyleTtl = gfItemMask("HI")
  lcStylePct = gfItemMask("PI")
  lnStyleWid = LEN(lcStylePct)

  *E300989,1 Evaluate OG Filter By [begin]
  *E300989,1 CSTYMAJOR, COLOR Seg. (or) Free Seg. values.
  lcStyMajor  = gfItemMask('HM')
  lcMajorPic  = gfItemMask('PM')
  lnMajorLen  = LEN(lcMajorPic)

  lcSeason   = lcStyMajor + ' season'
  lcDivision = lcStyMajor + ' division'
  lcStyGroup = lcStyMajor + ' group'
  lcFabTlt   = lcStyMajor + ' fabric code'
  lcPatTlt   = lcStyMajor + ' pattern'

  lnMajSeg    = gfItemMask('SM')  && No. of major segments.
  *-- Compute Free/Color Items in Style code Structure. [Begin]
  DIMENSION laMajSegs[1,1]
  = gfItemMask(@laMajSegs)

  *-- Loop Around Non Major elements.
  FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
    IF laMajSegs[lnI,1] $ 'CF'
      lcFree_Clr = laMajSegs[lnI,1]

      lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
      lnSupMajSt = lnNonMajSt
       
      lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                   laMajSegs[lnI,3],;
                   lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])

      lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                   PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                   lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    ENDIF                     

    *-- If you Find Color Type or Find Free Type and current type not Free.
    IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
      EXIT
    ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

  ENDFOR    && end Loop Around Non Major elements.

  STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
  lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
  *-- Compute Free/Color Items in Style code Structure. [End]
  
  *E300989,1 Evaluate OG Filter By [end.]
  
  *E300989,1 Fill Select By array Values depends of installed modules. [begin]
  lnSubtract = 0
  lnSubtract = IIF('MF' $ gcCmpModules,lnSubtract, lnSubtract + 1)
  lnSubtract = IIF('PO' $ gcCmpModules,lnSubtract, lnSubtract + 1)

  DIMENSION laSlctDesc[6 - lnSubtract,1], laSlctVals[6 - lnSubtract,1]
  laSlctDesc[1,1] = 'All'
  laSlctDesc[2,1] = lcStyMajor
  laSlctDesc[3,1] = 'Order'
  laSlctDesc[4,1] = 'Account'

  laSlctVals[1,1] = ' '
  laSlctVals[2,1] = 'S'
  laSlctVals[3,1] = 'O'
  laSlctVals[4,1] = 'A'

  DO CASE
    CASE lnSubtract = 0
      laSlctDesc[5,1] = 'Cutting ticket'
      laSlctDesc[6,1] = 'Purchase order'
  
      laSlctVals[5,1] = 'K'
      laSlctVals[6,1] = 'P'
    CASE lnSubtract = 1
      IF 'MF' $ gcCmpModules
        laSlctDesc[5,1] = 'Cutting ticket'
        laSlctVals[5,1] = 'K'
      ELSE  
        laSlctDesc[5,1] = 'Purchase order'
        laSlctVals[5,1] = 'P'
      ENDIF
  ENDCASE
  *E300989,1 Fill Select By array Values depends of installed modules. [end]
  
  *E300989,1 Fill Sort By array Values used in print OG. [begin]
  DIMENSION laSortDesc[2,1],laSortVal[2,1]
  laSortDesc[1,1] = 'Account/Order'
  laSortDesc[2,1] = lcStyleTtl

  laSortVal[1,1] = 'A'
  laSortVal[2,1] = 'S'
  *E300989,1 Fill Sort By array Values used in print OG. [end]
  
  SELECT ORDLINE
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)        && Varible to hold the number of fields in the ORDLINE file

  *E300989,1 Add Fields Which is not in order line file [begin]
  *E300989,1 but used in filteration of exclude Option Grid < Exclude > and in report.
  *E300989,1 Simply this fields is : (Entered From ORDHDR file) and 
  *E300989,1 (cDivision,cStyGroup,Fabric,Pattern,cStyMajor From STYLE file).
  *E300989,1 (Trancd, CtkTno From CUTPICK file if P/O or C/T modules installed)
  *E300989,1 (cFabColor From FABRIC file if system use dyelots)

  *DIMENSION laFileStru[lnFileStru+3,4]
  lnAddToArr = IIF('MF' $ gcCmpModules .OR. 'PO' $ gcCmpModules,12,10)
  lnAddToArr = IIF(llUseDyes,lnAddToArr+1,lnAddToArr)
  DIMENSION laFileStru[lnFileStru+lnAddToArr,ALEN(laFileStru,2)]
  
  laFileStru[lnFileStru+1,1] = 'PRIORITY'
  laFileStru[lnFileStru+1,2] = 'C'
  *E301249,1 Make priority field C(3) [start]
  *laFileStru[lnFileStru+1,3] = 1
  laFileStru[lnFileStru+1,3] = 3
  *E301249,1 [end]
  laFileStru[lnFileStru+1,4] = 0
  
  laFileStru[lnFileStru+2,1] = 'LLSEL'
  laFileStru[lnFileStru+2,2] = 'L'
  laFileStru[lnFileStru+2,3] = 1
  laFileStru[lnFileStru+2,4] = 0
  
  laFileStru[lnFileStru+3,1] = 'NPROCNO'
  laFileStru[lnFileStru+3,2] = 'N'
  laFileStru[lnFileStru+3,3] = 2
  laFileStru[lnFileStru+3,4] = 0
  
  laFileStru[lnFileStru+4,1] = 'ENTERED'
  laFileStru[lnFileStru+4,2] = 'D'
  laFileStru[lnFileStru+4,3] = 8
  laFileStru[lnFileStru+4,4] = 0
  
  laFileStru[lnFileStru+5,1] = 'CDIVISION'
  laFileStru[lnFileStru+5,2] = 'C'
  laFileStru[lnFileStru+5,3] = 6
  laFileStru[lnFileStru+5,4] = 0

  laFileStru[lnFileStru+6,1] = 'CSTYGROUP'
  laFileStru[lnFileStru+6,2] = 'C'
  laFileStru[lnFileStru+6,3] = 6
  laFileStru[lnFileStru+6,4] = 0

  laFileStru[lnFileStru+7,1] = 'FABRIC'
  laFileStru[lnFileStru+7,2] = 'C'
  laFileStru[lnFileStru+7,3] = 7
  laFileStru[lnFileStru+7,4] = 0

  laFileStru[lnFileStru+8,1] = 'PATTERN'
  laFileStru[lnFileStru+8,2] = 'C'
  laFileStru[lnFileStru+8,3] = 10
  laFileStru[lnFileStru+8,4] = 0

  laFileStru[lnFileStru+9,1] = 'CSTYMAJOR'
  laFileStru[lnFileStru+9,2] = 'C'
  laFileStru[lnFileStru+9,3] = 19
  laFileStru[lnFileStru+9,4] = 0

  laFileStru[lnFileStru+10,1] = 'CSORTFIELD'
  laFileStru[lnFileStru+10,2] = 'C'
  laFileStru[lnFileStru+10,3] = 80
  laFileStru[lnFileStru+10,4] = 0

  *-- if MF or PO modules is installed
  IF 'MF' $ gcCmpModules .OR. 'PO' $ gcCmpModules
    laFileStru[lnFileStru+11,1] = 'TRANCD'
    laFileStru[lnFileStru+11,2] = 'C'
    laFileStru[lnFileStru+11,3] = 1
    laFileStru[lnFileStru+11,4] = 0

    laFileStru[lnFileStru+12,1] = 'CTKTNO'
    laFileStru[lnFileStru+12,2] = 'C'
    laFileStru[lnFileStru+12,3] = 6
    laFileStru[lnFileStru+12,4] = 0
  ENDIF  
  
  *-- if system use dyelots add fabric color field.
  IF llUseDyes
    laFileStru[ALEN(laFileStru,1),1] = 'CFABCOLOR'
    laFileStru[ALEN(laFileStru,1),2] = 'C'
    laFileStru[ALEN(laFileStru,1),3] = 6
    laFileStru[ALEN(laFileStru,1),4] = 0
  ENDIF
  *E300989,1 Add Fields Which is not in order line file [end]
  
  *-- Add 8 ExcCut fields to end of this file structure and TotExcCut field.
  DIMENSION laFileStru[ALEN(laFileStru,1)+9,ALEN(laFileStru,2)]

  laFileStru[ALEN(laFileStru,1),1] = 'TOTEXCCUT'
  laFileStru[ALEN(laFileStru,1),2] = 'N'
  laFileStru[ALEN(laFileStru,1),3] = 7
  laFileStru[ALEN(laFileStru,1),4] = 0
  
  lnThArrPos = ALEN(laFileStru,1) - 9
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    laFileStru[lnThArrPos+lnI,1] = 'EXCCUT'+lcI
    laFileStru[lnThArrPos+lnI,2] = 'N'
    laFileStru[lnThArrPos+lnI,3] = 6
    laFileStru[lnThArrPos+lnI,4] = 0
  ENDFOR

  *-- Add Totwip field to end of this file structure.
  DIMENSION laFileStru[ALEN(laFileStru,1)+1,ALEN(laFileStru,2)]
  laFileStru[ALEN(laFileStru,1),1] = 'TOTWIP'
  laFileStru[ALEN(laFileStru,1),2] = 'N'
  laFileStru[ALEN(laFileStru,1),3] = 7
  laFileStru[ALEN(laFileStru,1),4] = 0

  *B603239,1 Add new field hold old sessions dyelot. [Begin]
  DIMENSION laFileStru[ALEN(laFileStru,1)+1,ALEN(laFileStru,2)]
  laFileStru[ALEN(laFileStru,1),1] = 'cPeggedDye'
  laFileStru[ALEN(laFileStru,1),2] = 'C'
  laFileStru[ALEN(laFileStru,1),3] = 10
  laFileStru[ALEN(laFileStru,1),4] = 0
  *B603239,1 Add new field hold old sessions dyelot. [End  ]
  
  llContinue = lfFndUnCSe()
  lcSession  = IIF(llContinue , lcSession , gfSequence('CSESSION'))    && Varible to hold the Session number

  *IF This is not an incompleted session
  IF !llContinue
    llCallScop = .T.

    *B603111,1 Name of File hold piktkts generated in this session [Begin]
    lcTmpPkTk  = gfTempName()  && New Session pick ticket file.
    lcTmpRelPk = gfTempName()  && Release File and Index.   
    lcRelLine  = gfTempName()
    *B603111,1 Name of File hold piktkts generated in this session [End  ]
    *B804233,1 HBG 05/08/2001 Temp file to Calculate order approve Amount[Begin]
    IF llChkAprov
      lcTmpOrdAp = gfTempName()
    ENDIF  
    *B804233,1 [End]
    lcTmpOrdLn = gfTempName()
    lcTmpIndex = gfTempName()
    lcTmpStore = gfTempName()
    *E300989,1 Temporary name to style index on temp.file
    lcTmStyTag = gfTempName()
    *E300989,1
    
    CREATE TABLE &gcWorkDir.&lcTmpOrdLn FROM ARRAY laFileStru
    *B804233,1 HBG 05/08/2001 Create Temp file to Calculate order approve Amount[Begin]
    IF llChkAprov  
      CREATE CURSOR (lcTmpOrdAp) (Type C(1),Order C(6),AprAmnt N(13,2),TotQty N(7),lExceed L(1))
      INDEX ON Type + Order TAG lcTmpOrdAp ADDITIVE
      SET ORDER TO lcTmpOrdAp 
    ENDIF  
    *B804233,1 [End]

  ENDIF   && End of IF !llContinue
  
  *E129473,1 MHM 08/21/2005 Generate 940 EDI transaction for pick tickets directed to 3PL providers.
  IF 'AS' $ gcCmpModules 
    =gfOpenFile(gcDataDir+'EDIACPRT',gcDataDir+'ACCFACT','SH')
    =gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PARTTRANS','SH')
    =gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')
  ENDIF
  *E129473,1 MHM 08/21/2005 (End)
  
ENDIF    && End of IF  

llAloRec = .F.
llIncOrd = .F.
lnChangAlo = 0

lcBrowFild = "lcMarker=IIF(RECNO()=lnBrRecNo , '>' , ' ') :1 :H= ' ' :W= .F. ,"+;
             "lcSelect=IIF(LLSEL , '»' , ' ') :R :H= '»' ,"+;
             "ORDER     :R :H= 'Order'     :8  ,"+;
             "ACCOUNT   :R :H= 'Account'   :10  ,"+;
             "STORE     :R :H= 'Store'     :8 ,"         +;
             "STYLE :H= lcStyleTtl : " + ALLTRIM(STR(ROUND(lnStyleWid*1.6 , 0))) + " :P= lcStylePct :R ," +;
             IIF(llUseDyes,"DYELOT :H='Dyelot':10:R," , "")+;
             "GROUP :H= 'G' :P= '!' :2 :R,"+; 
             "nAvail = MAX(0, STYDYE.TOTSTK - STYDYE.TOTALO) :H='Avail.':P='999999':R,"+; 
             "TOTQTY:H='Open':P='999999' :R ,"+;
             "OpnAmt = TOTQTY*PRICE :H='Opn Amnt.':P='99999999.99' :R ,"+;
             "PIKTKT :R :H = 'Pick Tkt' :W=.F., "+;
             "TOTPIK :H='Picked':P='999999' :R ,"+;
             "PikAmt = TotPik * PRICE :H='Pik Amnt.':P='99999999.99' :R"


lcBrowStr = gfBrowPref(lcBrowFild , 'ALAUTAL' , .T.)
lcBrowStr = SUBSTR(lcBrowStr , AT('FIELDS' , UPPER(lcBrowStr)))

llNoShow = .F.            && Flag to make the screen call the PROCEDURE lpShow evry time it run
lcScFields = 'ORDER'     && Varible to hold the screen fields
lcBaseFile = lcTmpOrdLn    && Varible that hold the screen main file

SELECT(lcTmpOrdLn)
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcOrdLBrow)
=lfActPad()

DO (gcScrDir + gcWinAppl + '\ALAUTAL.SPR')

ON KEY LABEL ALT+B 
RELEASE PAD _Option OF _MSYSMENU
RELEASE WINDOW (lcOrdLBrow)

*IF Any of the Syle or Warehouse or Dyelot was changed
IF lcStyle + lcWareCode + lcDyeLot <> &lcTmpOrdLn..Style +;
   &lcTmpOrdLn..cWareCode + &lcTmpOrdLn..DyeLot
  
  llAloRec = IIF(lnTotPik = 0 , .F. , .T.)
ELSE    && Else
  llAloRec = IIF(llIncOrd .OR. lnChangAlo > 0 , .T. , .F.)
ENDIF    && End of IF

*IF The current record was edited
IF llAloRec
  =lfAllocate(3 , RECNO(lcTmpOrdLn))
ENDIF    && End of IF

*IF The temp. file lcTmpOrdLn is open
IF USED(lcTmpOrdLn)
  SELECT (lcTmpOrdLn)
  SET RELATION TO
ENDIF    && End of IF

IF glQuitting

  *IF Statment to check if this session has a record in the UNCMSESS file
  IF lnUnCmRec <> 0
    SELECT UNCMSESS
    GO lnUnCmRec
    
    *REPLACE Statment to make the status completed
    REPLACE Status WITH 'C'
    UNLOCK
  ENDIF   && End of IF
  
  *B603111,1 CLOSE piktkts generated in this session [Begin]
  =lfErsBldPk()

  IF USED(lcTmpRelPk)
    USE IN (lcTmpRelPk)
  ENDIF

  IF USED(lcRelLine)
    USE IN (lcRelLine)
  ENDIF

  IF FILE(gcWorkDir+lcRelLine+'.DBF')
    ERASE &gcWorkdir.&lcRelLine..DBF          && Erase the Temp file.
  ENDIF
  *B603111,1 CLOSE piktkts generated in this session [End  ]
  
  *IF Statment to check if the temp file is open
  IF USED(lcTmpOrdLn)  && We are going to close the Temp Order line & Erase it.
    SELECT(lcTmpOrdLn)
    USE IN &lcTmpOrdLn
  ENDIF   && End of IF
  
  *B603111,1 Erase Temporary index file [Begin]
  IF FILE(gcWorkDir+lcTmpRelPk+'.CDX')
    ERASE &gcWorkdir.&lcTmpRelPk..CDX          && Erase the Temp file.
  ENDIF
  *B603111,1 Erase Temporary index file [End  ]

  *IF The Temp file .DBF exist we are going to erase it
  IF FILE(gcWorkdir + lcTmpOrdLn + '.DBF')
    ERASE &gcWorkdir.&lcTmpOrdLn..DBF          && Erase the Temp file.
  ENDIF    && End of IF FILE(gcWorkdir + lcTmpOrdLn + '.DBF')
  
  *IF The Temp file .CDX exist we are going to erase it
  IF FILE(gcWorkdir + lcTmpOrdLn + '.CDX')
    ERASE &gcWorkdir.&lcTmpOrdLn..CDX          && Erase the Temp index.
  ENDIF    && End of IF FILE(gcWorkdir + lcTmpOrdLn + '.CDX')
  
  *IF The Temp file .FPT exist we are going to erase it
  IF FILE(gcWorkdir + lcTmpOrdLn + '.FPT')
    ERASE &gcWorkdir.&lcTmpOrdLn..FPT  && Erase the Temp index.
  ENDIF    && End of IF FILE(gcWorkdir + lcTmpOrdLn + '.FPT')
  
  *E301077,24 Transfer the following line of code after glQuitting block.
  *=lfOpenFile(.F.)  

  *C200084,1 Fresh produces custom proram, Quit [Begin]
  IF ASCAN(laEvntTrig,PADR("SPC_SETS",10)) <> 0
    lcSetSectn = "QUITPRG"  && Parameter passed to setting function.
    = gfDoTriger('ALAUTAL','SPC_SETS')
  ENDIF  
  *C200084,1 Fresh produces custom proram, Quit [End  ]
ENDIF   && End of IF

*E301077,24 The main system close files.
*=lfOpenFile(.F.)  && Line transfered after glQuitting block
*-- end of program code.

*!*************************************************************
*! Name      : lfOpenFile
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Function to open and close the needed files
*!*************************************************************
*! Called from : ALAUTAL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .T. [To open the files] ,
*!                     .F. [To close the files]
*!*************************************************************
*! Return      : None
*!*************************************************************
*E301077,24 MAB 01/07/1999 Change the way used to open and close files.
*
FUNCTION lfOpenFile
*E301077,24 02/27/99 No close code (The main system deal with files in laFileName)
*PARAMETERS llOpen

PRIVATE lnCount

*E301077,24 02/27/99 Comment out the following if else endif block [Begin]
*E301077,24 02/27/99 Because main system close files in laFileName array.
*IF The function is to open the files
*02/27/99 IF llOpen
  *FOR Loop to loop the array laUsedFile
*02/27/99   FOR lnCount = 1 TO ALEN(laUsedFile , 1)
    
    *IF The file is not open
*02/27/99     IF !USED(laUsedFile[lnCount , 1])
 
      *E301077,24 Change the way open files. [Begin]
      *SELECT 0
      *USE (gcDataDir + laUsedFile[lnCount , 1])
      
      *IF The file is to be opened with an Index Tag
      *IF !EMPTY(laUsedFile[lnCount , 2])
      *  SET ORDER TO TAG (laUsedFile[lnCount , 2])
      *ENDIF    && End of IF

*02/27/99       = gfOpenFile(gcDataDir+laUsedFile[lnCount,1],;
*02/27/99                    IIF(EMPTY(laUsedFile[lnCount , 2]),'',gcDataDir+laUsedFile[lnCount , 2]),'SH')
      *E301077,24 Change the way open files. [End  ]

*02/27/99       laUsedFile[lnCount , 3] = .T.

*02/27/99     ELSE    && Else

*02/27/99       SELECT (laUsedFile[lnCount , 1])

      *IF The file is to be opened with an Index Tag
*02/27/99       IF !EMPTY(laUsedFile[lnCount , 2])
*02/27/99         SET ORDER TO TAG (laUsedFile[lnCount , 2])
*02/27/99       ELSE    && Else
*02/27/99         SET ORDER TO
*02/27/99       ENDIF    && End of IF
*02/27/99       SET FILTER TO 
*02/27/99       laUsedFile[lnCount , 3] = .F.
*02/27/99     ENDIF    && End of IF
*02/27/99     GO TOP
*02/27/99   ENDFOR    && End of FOR Loop

*02/27/99 ELSE    && Else

  *FOR Loop to loop the array laUsedFile
*02/27/99   FOR lnCount = 1 TO ALEN(laUsedFile , 1)

    *IF The file was not open befor
*02/27/99     IF laUsedFile[lnCount , 3]

      *E301077,24 Change the way close files. [Begin]
      *USE IN (laUsedFile[lnCount , 1])
*02/27/99       =gfCloseFile(laUsedFile[lnCount , 1])
      *E301077,24 Change the way close files. [End  ]

*02/27/99     ENDIF    && End of IF
*02/27/99   ENDFOR    && End of FOR Loop
*02/27/99 ENDIF    && End of IF
*E301077,24 02/27/99 Comment out the following if else endif block [End  ]

*E301077,24 02/27/99 Adjust code to open files only. [Begin]
*FOR Loop to loop the array laUsedFile
FOR lnCount = 1 TO ALEN(laUsedFile , 1)
    
  *IF The file is not open
  IF !USED(laUsedFile[lnCount , 1])
 
    = gfOpenFile(gcDataDir+laUsedFile[lnCount,1],;
                 IIF(EMPTY(laUsedFile[lnCount , 2]),'',gcDataDir+laUsedFile[lnCount , 2]),'SH')
  ELSE 

    SELECT (laUsedFile[lnCount,1])
    SET ORDER TO (laUsedFile[lnCount,2])

  ENDIF
  
ENDFOR
*E301077,24 02/27/99 Adjust code to open files only. [End  ]
*-- end of lfOpenFile.

*!*************************************************************
*! Name      : lpShow
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Local Show Procedure
*!*************************************************************
*! Called from : Control panel global Show
*!*************************************************************
*! Calls       : lfvScope() , lfShowGets() , lfvpbSel , lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
PROCEDURE lpShow

SHOW GET pbEdt DISABLE
SHOW GET pbBrws DISABLE
SHOW GET pbSlct DISABLE
SHOW GET pbDlt DISABLE

*-- IF it is first time run current session , so it is not incompleted sesion.
*-- We are going to call the Option grid
IF llCallScop
  llCallScop = .F.
  =lfvScope()
ENDIF    && End of IF

*-- IF The user has generated piking tikets for all the order lines in scope
*-- Clear for new selection
IF llCallUnCS
  llCallUnCS = .F.
  lcOldTmpF  = lcTmpOrdLn
  *-- Check if there is an incomplete session
  llContinue = lfFndUnCSe()
  
  *-- If there is another uncomplete session
  IF llContinue
    =lfDispBrow()
  
                     *-- Clear for new selection --* 
    *-- IF Statment to check if the old temp file is open
    IF USED(lcOldTmpF)  && We are going to close the Temp Order line & Erase it.
      USE IN &lcOldTmpF
    ENDIF   && End of IF
    
    *-- IF The old temp file .DBF exist we are going to erase it
    IF FILE(gcWorkdir + lcOldTmpF + '.DBF')
      ERASE &gcWorkdir.&lcOldTmpF..DBF          && Erase the Temp file.
    ENDIF    && End of IF FILE(gcWorkdir + lcOldTmpF + '.DBF')
    
    *-- IF The old temp file .CDX exist we are going to erase it
    IF FILE(gcWorkdir + lcOldTmpF + '.CDX')
      ERASE &gcWorkdir.&lcOldTmpF..CDX          && Erase the Temp index.
    ENDIF    && End of IF FILE(gcWorkdir + lcOldTmpF + '.CDX')
    
    *-- IF The old temp file .FPT exist we are going to erase it
    IF FILE(gcWorkdir + lcOldTmpF + '.FPT')
      ERASE &gcWorkdir.&lcOldTmpF..FPT  && Erase the Temp index.
    ENDIF    && End of IF FILE(gcWorkdir + lcOldTmpF + '.FPT')
    
  ENDIF    && End of IF llContinue
ENDIF    && End of IF llCallUnCS

*-- IF This is an incompleted session and there is a function that should be 
*-- completed 
IF llContinue .AND. !EMPTY(lcCurrObj)
  =&lcCurrObj      && call the incompleted function 
ENDIF    && End of IF

*-- Refresh screen

laCtrStat[7]  = "DISABLE"                && Edit button 
laCtrStat[8]  = "DISABLE"                && Delete button
laCtrStat[9]  = "DISABLE"                && Select button
laCtrStat[10] = "DISABLE"                && Browse button

SHOW MENU _mSysMenu

lnBrRecNo = RECNO(lcTmpOrdLn)
SHOW WINDOW (lcOrdLBrow) REFRESH

=SEEK(Style , 'STYLE')
=SEEK('S' + STYLE.Scale , 'SCALE')

lcStyle = Style
lcStore = Store
lcGroup = Group
lcWareCode = cWareCode
lnWareCode = ASCAN(laWareCode , lcWareCode)
lcDyelot = IIF(llUseDyes , Dyelot , SPACE(10))
lnTotPik = TotPik
lnTotQty = TotQty
SCATTER FIELDS LIKE PIK* TO laPik
SCATTER FIELDS LIKE QTY* TO laQty

*-- IF the current record is selected
IF llSel
  =lfShowGets(.T.)
ELSE    
  lcCh3Stat = 'DISABLE'
  laPikSt   = 'DISABLE'
  SHOW GETS WINDOW (lcAutAlCh3) DISABLE ONLY
  *E300989,1 Comment out next line because, Force allocation 
  *E300989,1 button deleted from screen
  *SHOW GET llForceAlc &lcInvertSt
ENDIF    && End of IF
=lfvpbSel()     && Arange the push button Select prompt
=lfRefresh()
*-- end of lpShow.

*!*************************************************************
*! Name      : lfvScope
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of push button Scope
*!*************************************************************
*! Called from : Control Panel [Push button Scope] , lpShow , Option popup
*!*************************************************************
*! Calls       : gfOpGrid() , gfModalGen() , lfAllocate() ,
*!               lfGenScr() , lfShowGets() , lfRefresh() , lfvpbSel() ,
*!               lfSelData()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvScope

*B603691,1 Intial value for Allocate conditionally [Begin]
llRpCond = .F.
*B603691,1 Intial value for Allocate conditionally [End  ]

DIMENSION laScopExpr[1,2] , laNormExpr[1,2]
laScopExpr = ''
laNormExpr = ''
lcOldScMod = ' '

*IF This is not an incompleted session
IF !llContinue
  ON KEY LABEL ALT+B
  
  *E300989,1 Add the following lines because now we have 3 grids [Begin
  STORE .F. TO lcExpr1,lcExpr2

  *B603657 RAMY [START]
  PUSH KEY
  *B603657 RAMY [END]
  
  *-- Call the selection grid
  lcExpr = gfOpGrid('ALAUTSLC' , .T.)  
  
  *B603657 RAMY [START]
  POP KEY
  *B603657 RAMY [END]
  
  *-- IF lcExpr have a value. which means that the user selecte certain criteria.
  IF TYPE('lcExpr') # 'L'
    llStartSlc = .T.  && User start from selection grid
    llSelData  = .F.  && No data was selected yet.

    *E300989,1 Select normal records [Begin
    *-- Create array have normal filter expression
    DIMENSION laFiltExp[ALEN(laIncExprs,1),ALEN(laIncExprs,2)]
    =ACOPY(laIncExprs,laFiltExp)
    
    *B603544,4 HBG 06/06/2000 Function to check setup complete date by order line if it is yes 
    *                         replace ORDHDR by ORDLINE in the laFiltExp array else replace
    *                         ORDLINE by ORDHDR. Then replace ORDLINE by lcChildFil [Begin]
    =lfChngDate()
    *B603544,4 HBG 06/06/2000 Function to check setup complete date by order line [End  ]
    
    llExclude = .F.
    lcOldSlct  = lcRpScpMod
    = lfCreatExp(1) && Create normal expression.
    
    *B602569,1 Intializing full allocation flag [Begin]
    llPartAlo = .F.
    *B602569,1 Intializing full allocation flag [End  ]
    
    *-- Collect Data for the selection criteria
    llSelData = lfSelData()  && .T. if we have data.
    
    *E300989,1 Select normal records [End..

    *C037824,1  TMI [Start] Remove styles not selected from the new added style browse for STU10
    IF ASCAN(laEvntTrig,PADR("FLTONSTY",10)) <> 0
      =gfDoTriger('ALAUTAL',PADR("FLTONSTY",10))
    ENDIF
    *C037824,1  TMI [End  ] 

    lcRpScpMod  = lcOldSlct  && Restore select by.
    
    llExclude = !llRpGdExcl  && .T. if user want to exclude records.

    *E300989,1 DeSelect exclude records [Begin
    *-- if there are records in Temp. file and its exclude option.
    IF llSelData AND llExclude

      *-- Create array have exclude filter expression
      DIMENSION laFiltExp[ALEN(laExcExprs,1),ALEN(laExcExprs,2)]
      =ACOPY(laExcExprs,laFiltExp)
      
      *B603544,4 HBG 06/06/2000 Function to check setup complete date by order line if it is yes 
      *                         replace ORDHDR by ORDLINE in the laFiltExp array else replace
      *                         ORDLINE by ORDHDR. Then replace ORDLINE by lcChildFil [Begin]
      =lfChngDate()
      *B603544,4 HBG 06/06/2000 Function to check setup complete date by order line [End  ]
      
      *-- Clear the excluded expression from any "DTOS" and "ALLTRIM"
      FOR lnI = 1 TO ALEN(laFiltExp,1)
        IF ('DTOS' $ laFiltExp[lnI,1]) OR ('ALLTRIM' $ laFiltExp[lnI,1])
          lnStartCut = ATC('(',laFiltExp[lnI,1])+1
          lnEndCut   = ATC(')',laFiltExp[lnI,1])
          laFiltExp[lnI,1] = SUBSTR(laFiltExp[lnI,1],lnStartCut,lnEndCut-lnStartCut)
        ENDIF
      ENDFOR

      *-- initialize optimize arrays again.
      DIMENSION laScopExpr[1,2] , laNormExpr[1,2]
      laScopExpr = ''
      laNormExpr = ''
      
      *-- Select by value will be the excluded values
      lcOldSlct  = lcRpScpMod
      lcRpScpMod = lcRpExSlct
      
      = lfCreatExp(2)  && Create exclude expression.
      llSelData  = lfSelData()  && .T. if there if temp. file still have records.
      lcRpScpMod = lcOldSlct    && Restore select by value.
      
    ENDIF  && end if there are records in Temp. file and its exclude option.
    *E300989,1 DeSelect exclude records [End..
    
  ENDIF  
  *E300989,1 Add the following lines because now we have 3 grids [End..
  
  ON KEY LABEL ALT+B ACTIVATE WINDOW (lcOrdLBrow)  
ENDIF    && End of IF

*IF This is not an incompleted session and if the User has selected Cancel
*from the Option grid or there is no records for the selection criteria
*E300989,1 Change lfSelData() fn. to llSelData flag to support run 
*E300989,1 allocation and pick ticket grids.
*IF !llContinue .AND. (TYPE('lcExpr') = 'L' .OR. !lfSelData())
IF !llContinue .AND. (TYPE('lcExpr') = 'L' .OR. !llSelData)

  laScrMode = .F.        && The Array that hold the Screen mode
  laScrMode[1] = .T.
  
  *IF There is no records for the selection criteria
  IF TYPE('lcExpr') <> 'L'

    *E300989,1 if this run does not have data refresh screen., [Begin
    IF !EMPTY(lcStyle)
      = lfRefScr()
    ENDIF
    *E300989,1 if this run does not have data refresh screen., [End..

    *** Message : "There are no records to display...!"
    ***           "              < Ok >               "
    =gfModalGen("TRM00052B00000","DIALOG")
  ENDIF    && End of IF

ELSE    && Else

  *E300989,1 Call Next new grids, then it's not started by selection grid[Begin

  *B605077,1 BWA 10/31/2001 Fix the bug of the wrong sorting.[START]
  llStartSlc = .F.
  *B605077,1 BWA 10/31/2001.[END]

  = lfAlocGrid()  && Next two option grids if user want.
    
  llStartSlc = .F.
  *E300989,1 Call Next new grids, then it's not started by selection grid[End..
  
  *-- Save the name of this session in case of incompleted session ocuers
  lcCurrObj = 'lfvScope()'
  *-- Add record to uncompleted session file
  =lfAdUnCmSR()
  
  *E300989,1 Allocation is done under allocation flag condition [Begin
  *=lfAllocate(1)
  IF llRpAlocat
    lnAloRec = 0
    = lfAllocate(1)

  *MAB NOW
  ELSE
    *-- Get the # of selected and allocated records if no records selected and allocated 
    *-- and under generating pick ticket flag condition
    IF llRpGenPik AND lnSelAlo = 0
      lnSelAlo = RECCOUNT(lcTmpOrdLn)
    ENDIF

    *C200084,1 Cancel unselected lines if it is first run only [Begin]
    IF ASCAN(laEvntTrig,PADR("ALLOCANCEL",10)) <> 0
      = gfDoTriger('ALAUTAL','ALLOCANCEL')
    ENDIF  
    *C200084,1 Cancel unselected lines if it is first run only [End  ]
    
  ENDIF  
  *E300989,1 Allocation is done under allocation flag condition [End..
  
  
  *IF Generate Pick tickets [From the Option grid] is Yes and there is some
  *allocated records
  IF llRpGenPik .AND. lnSelAlo > 0
    =lfGenScr()
    
    *IF There is no records to be Browsed [IF we have Generated Pick tickets
    *for all the records in the temp. Order lines file]
    IF RECCOUNT(lcTmpOrdLn) = lnDellRec
      RETURN
    ENDIF    && End of IF
  ENDIF    && End of IF
  
  laScrMode = .F.
  laScrMode[2] = .T.
  laCtrStat[6]  = "ENABLE"                && Print Button
  GO TOP IN (lcTmpOrdLn)
  SHOW GETS WINDOW gwcContrl1 OFF
  
  *SHOW GETS OFF
  SET ORDER TO TAG STYLE IN STYLE
  SELECT (lcTmpOrdLn)
  SET ORDER TO TAG lcTmpOrdLn
  
  *IF The system use Dyelots
  IF llUseDyes
    SET RELATION TO Style + cWareCode + DyeLot INTO STYDYE
  ELSE    && Else
    SET RELATION TO Style + cWareCode + SPACE(10) INTO STYDYE
  ENDIF    && End of IF
  GO TOP
  lnBrRecNo  = RECNO()
  =SEEK(Style , 'STYLE')
  =SEEK('S' + STYLE.Scale , 'SCALE')

  lcStyle = Style
  lcStore = Store
  lcGroup = Group
  lcWareCode = cWareCode
  lnWareCode = ASCAN(laWareCode , lcWareCode)
  lcDyelot = IIF(llUseDyes , Dyelot , SPACE(10))
  lnTotPik = TotPik
  lnTotQty = TotQty
  SCATTER FIELDS LIKE PIK* TO laPik
  SCATTER FIELDS LIKE QTY* TO laQty
  
  *IF the current record is selected
  IF llSel
    =lfShowGets(.T.)
  ELSE    && Else
    lcCh3Stat = 'DISABLE'
    laPikSt   = 'DISABLE'
    SHOW GETS WINDOW (lcAutAlCh3) DISABLE ONLY
  ENDIF    && End of IF
  =lfRefresh()
  
  lcSelAllSt = IIF(lnSelRec = RECCOUNT() - lnDellRec , 'DISABLE' , 'ENABLE')
  lcSelNonSt = IIF(lnSelRec = 0 , 'DISABLE' , 'ENABLE')
  lcAloSt = lcSelNonSt
  lcRelSt = IIF(lnSelAlo = 0 , 'DISABLE' , 'ENABLE')
  lcInvertSt = 'ENABLE'
  
  =lfvpbSel()
  
  *E300989,1 Comment out next line because 
  *E300989,1 Force allocation button is not in screen.
  *SHOW GET llForceAlc &lcInvertSt
  
  SHOW GET pbInvert &lcInvertSt
  SHOW GET pbSelAll &lcSelAllSt
  SHOW GET pbSelNon &lcSelNonSt
  SHOW GET pbAlo &lcAloSt
  SHOW GET pbRel &lcRelSt
  SHOW GET pbGen &lcRelSt
  
  *E300989,1 Now There is no need to llScope flag and lcScopSt variable 
  *E300989,1 because Scope is Enabled allover .
  *llScope = .T.
  *lcScopSt = 'DISABLE'
  *SHOW GET pbScop &lcScopSt
  *E300989,1  
  
  _CUROBJ = OBJNUM(IbBrow)
  
  lcCurrObj = ''
  =lfAdUnCmSR()
ENDIF    && End of IF
*-- end of lfvScope.

*!*************************************************************
*! Name      : lfAloScr
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of push button Allocate
*!*************************************************************
*! Called from : Control Panel [Push button Allocate] , Option popup
*!*************************************************************
*! Calls       : lfAllocate() , lfvpbSel() , lfShowGets() ,
*!               lfRefresh()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfAloScr

SELECT (lcTmpOrdLn)

*IF Any of the Syle or Warehouse or Dyelot was changed
IF lcStyle + lcWareCode + lcDyeLot <> Style + cWareCode + DyeLot
  llAloRec = IIF(lnTotPik = 0 , .F. , .T.)
ELSE    && Else
  llAloRec = IIF(llIncOrd .OR. lnChangAlo > 0 , .T. , .F.)
ENDIF    && End of IF

*IF The current record was edited
IF llAloRec
  =lfAllocate(3 , RECNO(lcTmpOrdLn))
ENDIF    && End of IF

llAloRec = .F.
llIncOrd = .F.
lnChangAlo = 0

lcCurrObj = 'lfAloScr()'
=lfAdUnCmSR()

lnBrRecNo = RECNO(lcTmpOrdLn)

=lfAllocate(2)

SELECT (lcTmpOrdLn)

*IF The system use Dyelots
IF llUseDyes
  SET RELATION TO Style + cWareCode + DyeLot INTO STYDYE
ELSE    && Else
  SET RELATION TO Style + cWareCode + SPACE(10) INTO STYDYE
ENDIF    && End of IF
GO lnBrRecNo
SHOW WINDOW (lcOrdLBrow) REFRESH

lcSelAllSt = IIF(lnSelRec = RECCOUNT() - lnDellRec , 'DISABLE' , 'ENABLE')
lcSelNonSt = IIF(lnSelRec = 0 , 'DISABLE' , 'ENABLE')
lcAloSt = lcSelNonSt
lcRelSt = IIF(lnSelAlo = 0 , 'DISABLE' , 'ENABLE')

=lfvpbSel()
SHOW GET pbSelAll &lcSelAllSt
SHOW GET pbSelNon &lcSelNonSt
SHOW GET pbAlo &lcAloSt
SHOW GET pbRel &lcRelSt
SHOW GET pbGen &lcRelSt


=SEEK(Style , 'STYLE')
=SEEK('S' + STYLE.Scale , 'SCALE')

lcStyle = Style
lcStore = Store
lcGroup = Group
lcWareCode = cWareCode
lnWareCode = ASCAN(laWareCode , lcWareCode)
lcDyelot = IIF(llUseDyes , Dyelot , SPACE(10))
lnTotPik = TotPik
lnTotQty = TotQty
SCATTER FIELDS LIKE PIK* TO laPik
SCATTER FIELDS LIKE QTY* TO laQty

*IF the current record is selected
IF llSel
  =lfShowGets(.T.)
ELSE    && Else
  lcCh3Stat = 'DISABLE'
  laPikSt   = 'DISABLE'
  SHOW GETS WINDOW (lcAutAlCh3) DISABLE ONLY
  *E300989,1 Comment out next line because force alloc. button is
  *E300989,1 No longer in screen.
  *SHOW GET llForceAlc &lcInvertSt
ENDIF    && End of IF
=lfRefresh()

lcCurrObj = ''
=lfAdUnCmSR()

*!*************************************************************
*! Name      : lfRelScr
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of push button Release
*!*************************************************************
*! Called from : Control Panel [Push button Release] , Option popup
*!*************************************************************
*! Calls       : gfThermo() , lfRelQty() , lfShowGets() , lfRefresh()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfRelScr

PRIVATE lcRelStyle , lnTotRec , lnCurent , lnRecCount ,;
        lnPrepRec

lcCurrObj = 'lfRelScr()'
=lfAdUnCmSR()

*IF Statment to make sure that the user want to release the selected records
*** Message : "Are you sure you want to release the allocation from the selected order lines "
***           "                   < Yes >           < No >                    "
IF gfModalGen("TRM44007B00006","DIALOG") = 1

  SET ORDER TO TAG STYDYE IN STYDYE
  SET ORDER TO TAG STYLE IN STYLE
  SET ORDER TO TAG ORDLINE IN ORDLINE
  SET ORDER TO TAG ORDHDR IN ORDHDR
  SELECT (lcTmpOrdLn)
  SET ORDER TO TAG &lcTmpOrdLn
  SET RELATION TO

  lnBrRecNo = RECNO(lcTmpOrdLn)
  lnTotRec = lnSelAlo          && Varible to hold the Total count to be done for the thermometer
  lnCurent = 0                 && Varible to hold the current count to be done for the thermometer
  
  *IF This is not an incompleted session
  IF !llContinue
    lnRecCount = RECCOUNT() - lnDellRec      && Varible to hold the Total count to be done for the thermometer
    lnPrepRec = 0                            && Varible to hold the current count to be done for the thermometer
    
    *SCAN Loop to scan the temp. Order lines file
    SCAN
      lnPrepRec = lnPrepRec + 1
      REPLACE nProcNo WITH 0
      =gfThermo(lnRecCount , lnPrepRec , "Preparing data files" , '')
    ENDSCAN    && End of SCAN Loop
    =gfThermo(lnRecCount , lnRecCount , "Preparing data files" , '')
  ELSE    && Else
    llContinue = .F.
  ENDIF    && End of IF

  SELECT (lcTmpOrdLn)
  
  *SCAN Loop to scan the temp. Order lines file FOR the selected
  *and allocated records and for nProcNo < 12

  *E300989,1 Change Roll back to be 14 instead of 12 [begin]
  *E300989,1 because we may release Pick Ticket
  *SCAN FOR llSel .AND. TotPik > 0 .AND. nProcNo < 12

  SCAN FOR llSel .AND. TotPik > 0 .AND. nProcNo < 14
  *E300989,1 Change Roll back to be 14 instead of 12 [end]
    lcRelStyle = Style          && Variable to hold the Style
    =lfRelQty()
    lnSelAlo = lnSelAlo - 1
    lnAloRec = lnAloRec - 1
    lnCurent = lnCurent + 1
    =gfThermo(lnTotRec , lnCurent , "Releasing...Order/Style : " ,;
              Order + '/' + lcRelStyle)
  
  ENDSCAN    && End of SCAN Loop
  =gfThermo(lnTotRec , lnTotRec , "Releasing...Order/Style : " , '')

  SELECT (lcTmpOrdLn)
  
  *IF The system use Dyelots
  IF llUseDyes
    SET RELATION TO Style + cWareCode + DyeLot INTO STYDYE
  ELSE    && Else
    SET RELATION TO Style + cWareCode + SPACE(10) INTO STYDYE
  ENDIF    && End of IF
  GO lnBrRecNo
  SHOW WINDOW (lcOrdLBrow) REFRESH
  
  lcRelSt = IIF(lnSelAlo = 0 , 'DISABLE' , 'ENABLE')

  SHOW GET pbRel &lcRelSt
  SHOW GET pbGen &lcRelSt

  =SEEK(Style , 'STYLE')
  =SEEK('S' + STYLE.Scale , 'SCALE')

  lcStyle = Style
  lcStore = Store
  lcGroup = Group
  lcWareCode = cWareCode
  lnWareCode = ASCAN(laWareCode , lcWareCode)
  lcDyelot = IIF(llUseDyes , Dyelot , SPACE(10))
  lnTotPik = TotPik
  lnTotQty = TotQty
  SCATTER FIELDS LIKE PIK* TO laPik
  SCATTER FIELDS LIKE QTY* TO laQty
  
  *IF the current record is selected
  IF llSel
    =lfShowGets(.T.)
  ELSE    && Else
    lcCh3Stat = 'DISABLE'
    laPikSt   = 'DISABLE'
    SHOW GETS WINDOW (lcAutAlCh3) DISABLE ONLY
    *E300989,1 Comment out next line because force alloc. button is
    *E300989,1 No longer in screen.
    *SHOW GET llForceAlc &lcInvertSt
  ENDIF    && End of IF
  =lfRefresh()

ENDIF    && End of IF

lcCurrObj = ''
=lfAdUnCmSR()

*!*************************************************************
*! Name      : lfGenScr
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of push button Generate pick tickets
*!*************************************************************
*! Called from : Control Panel [Push button Generate pick tickets] ,
*!               Option popup , lfvScope()
*!*************************************************************
*! Calls       : gfThermo() , lfGetPkTkt() , lfGenPikTk()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfGenScr

SELECT (lcTmpOrdLn)

PRIVATE lcPikTkt , lnTotRec , lnCurent , lnRecCount , lnPrepRec

*IF Any of the Syle or Warehouse or Dyelot was changed
IF lcStyle + lcWareCode + lcDyeLot <> Style + cWareCode + DyeLot
  llAloRec = IIF(lnTotPik = 0 , .F. , .T.)
ELSE    && Else
  llAloRec = IIF(llIncOrd .OR. lnChangAlo > 0 , .T. , .F.)
ENDIF    && End of IF

*IF The current record was edited
IF llAloRec
  =lfAllocate(3 , RECNO(lcTmpOrdLn))
ENDIF    && End of IF
llAloRec = .F.
llIncOrd = .F.
lnChangAlo = 0

lcCurrObj = 'lfGenScr()'
=lfAdUnCmSR()

SET ORDER TO TAG ORDHDR IN ORDHDR
SELECT (lcTmpOrdLn)
SET ORDER TO TAG &lcTmpOrdLn
SET RELATION TO 'O' + Order INTO ORDHDR

lnTotRec = lnSelAlo          && Varible to hold the Total count to be done for the thermometer
lnCurent = 0                 && Varible to hold the current count to be done for the thermometer

*IF This is not an incompleted session
IF !llContinue
  lnRecCount = RECCOUNT() - lnDellRec      && Varible to hold the Total count to be done for the thermometer
  lnPrepRec = 0                            && Varible to hold the current count to be done for the thermometer
  
  *SCAN Loop to scan the temp. Order lines file
  SCAN
    lnPrepRec = lnPrepRec + 1
    REPLACE nProcNo WITH 0
    =gfThermo(lnRecCount , lnPrepRec , "Preparing data files" , '')
  ENDSCAN    && End of SCAN Loop
  =gfThermo(lnRecCount , lnRecCount , "Preparing data files" , '')
ELSE    && Else
  llContinue = .F.
ENDIF    && End of IF

*SCAN Loop to scan the temp. Order lines file FOR the selected
*and allocated records and for nProcNo < 4

*B039660,1 NNA 02/14/2006 Check first if there is enough stock or not before making update in the Bin location Files
IF ASCAN(laEvntTrig,PADR("CHKFIRST",10)) <> 0 AND !gfDoTriger("ALAUTAL",PADR("CHKFIRST",10))
  RETURN
ENDIF  
*B039660,1 NNA (End)

SCAN FOR llSel .AND. TotPik > 0 .AND. nProcNo < 4

  IF !EMPTY(PIKTKT) AND PIKTKT # '******'
    LOOP
  ENDIF
  lnCurent = lnCurent + 1
  
  *IF the first step for this record
  IF nProcNo = 0
    *lcPikTkt = lfGetPkTkt(Order , ORDHDR.cDivision , Store , cWareCode)    && Variable to hold the Pick ticket number

    *B603111,1 There is at least one line added in this session. [Begin]
    *lcPikTkt = lfGetPkTkt(Order , ORDHDR.cDivision , Store , cWareCode , lnRpGenNew)    && Variable to hold the Pick ticket number
    IF SEEK(Order+Store+cWareCode,lcTmpPkTk)
      lcPikTkt = &lcTmpPkTk..PikTkt
    ELSE
      lcPikTkt = lfGetPkTkt(Order , ORDHDR.cDivision , Store , cWareCode , lnRpGenNew)    && Variable to hold the Pick ticket number
    ENDIF  
    *B603111,1 There is at least one line added in this session. [End  ]

  ENDIF    && End of IF
  
  =lfGenPikTk(lcPikTkt)

  =gfThermo(lnTotRec , lnCurent , "Assigning piktkt number for...Order/Style : " ,;
            Order + '/' + Style)
  
ENDSCAN    && End of SCAN Loop

*IF There was a selected and allocated records
IF lnTotRec > 0
  =gfThermo(lnTotRec , lnTotRec , "Assigning piktkt number for...Order/Style : " , '')
ENDIF    && End of IF

SELECT (lcTmpOrdLn)
SET ORDER TO TAG lcTmpOrdLn

*IF The system use Dyelots
IF llUseDyes
  SET RELATION TO Style + cWareCode + DyeLot INTO STYDYE
ELSE    && Else
  SET RELATION TO Style + cWareCode + SPACE(10) INTO STYDYE
ENDIF    && End of IF

GO TOP
SHOW WINDOW (lcOrdLBrow) REFRESH SAME

*-- IF There is no records in the file [IF we have generated Pick tickets for
*-- all the records]
IF EOF()
  llCallUnCS = .T.
  laScrMode = .F.
  laScrMode[1] = .T.
  lcSelAllSt = 'DISABLE'
  lcSelNonSt = 'DISABLE'
  lcAloSt = 'DISABLE'
  lcRelSt = 'DISABLE'
  lcInvertSt = 'DISABLE'
  
  *E300989,1 Now There is no need to llScope flag and lcScopSt variable 
  *E300989,1 because Scope is Enabled allover .
  *llScope = .F.
  *lcScopSt = 'ENABLE'
  *SHOW GET pbScop &lcScopSt
  *E300989,1

  SHOW MENU _mSysMenu

  SHOW GET pbSelect DISABLE
  
  *IF We are in the Browse
  IF WONTOP() = lcOrdLBrow
    _CUROBJ = OBJNUM(pbScop)
  ENDIF    && End of IF
ELSE    && Else
  lcSelAllSt = IIF(lnSelRec = RECCOUNT() - lnDellRec , 'DISABLE' , 'ENABLE')
  lcSelNonSt = IIF(lnSelRec = 0 , 'DISABLE' , 'ENABLE')
  lcAloSt = lcSelNonSt
  lcRelSt = IIF(lnSelAlo = 0 , 'DISABLE' , 'ENABLE')
  lcInvertSt = 'ENABLE'
ENDIF    && End of IF

*E300989,1 Comment out next line because force alloc. button is
*E300989,1 No longer in screen.
*SHOW GET llForceAlc &lcInvertSt
SHOW GET pbInvert &lcInvertSt
SHOW GET pbSelAll &lcSelAllSt
SHOW GET pbSelNon &lcSelNonSt
SHOW GET pbAlo &lcAloSt
SHOW GET pbRel &lcRelSt
SHOW GET pbGen &lcRelSt

lcCurrObj = ''

=lfAdUnCmSR(llCallUnCS)

SELECT (lcTmpOrdLn)
SET RELATION OFF INTO ORDHDR

SHOW GETS
*-- end of lfGenScr.

*!*************************************************************
*! Name      : lfDispBrow
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Function to create the Browse
*!*************************************************************
*! Called from : Scrren ALAUTA
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfDispBrow

SELECT (lcTmpOrdLn)
lnBrRecNo  = RECNO()

*E300989,1 The following browse command initially have line
*-- PREFERENCE 'ALAUTAL';
*-- and I delete it to be independance of foxuser file.
BROWSE &lcBrowStr;
       WINDOW (lcAutAlCh1) ;
       WHEN lfwBrows() .AND. lfvpbSel() ;
       VALID :F lfvBrow() ;
       IN WINDOW (gcBaseWind) ;
       LOCK 0  ;
       NOAPPEND;
       NOCLEAR ;
       NODELETE;
       NOWAIT  ;
       NOEDIT  ;
       NOMENU  ;
       SAVE    ;
       TITLE lcOrdLBrow
*-- end of lfDispBrow.
       
*!*************************************************************
*! Name      : lfwBrows
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : When function of the Browse [lcOrdLBrow]
*!*************************************************************
*! Called from : The Browse [lcOrdLBrow]
*!*************************************************************
*! Calls       : lfShowGets() , lfRefresh()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfwBrows

*E301225,1 Initialize Force all quantities for this line. [Begin]
llForceAll = .F.
*E301225,1 Initialize Force all quantities for this line. [End  ]

lnBrRecNo  = RECNO(lcTmpOrdLn)

=SEEK(Style , 'STYLE')
=SEEK('S' + STYLE.Scale , 'SCALE')

lcStyle = Style
lcStore = Store
lcGroup = Group
lcWareCode = cWareCode
lnWareCode = ASCAN(laWareCode , lcWareCode)
lcDyelot = IIF(llUseDyes , Dyelot , SPACE(10))
lnTotPik = TotPik
lnTotQty = TotQty
SCATTER FIELDS LIKE PIK* TO laPik
SCATTER FIELDS LIKE QTY* TO laQty

*IF the current record is selected
IF llSel
  =lfShowGets(.T.)
  SHOW GET lcStyle
ELSE    && Else
  lcCh3Stat = 'DISABLE'
  laPikSt   = 'DISABLE'
  SHOW GETS WINDOW (lcAutAlCh3) DISABLE ONLY

  *E300989,1 Comment out next line because force alloc. button is
  *E300989,1 No longer in screen.
  *SHOW GET llForceAlc &lcInvertSt
ENDIF    && End of IF
=lfRefresh()

SHOW WINDOW (lcOrdLBrow) REFRESH
    
llEOF = .F.          && Flag to know if we are at the First record of the temp. Order lines file
llBOF = .F.          && Flag to know if we are at the Last record of the temp. Order lines file

*IF The temp. Order lines file is not empty
IF !EOF() .AND. !BOF()
  SKIP 1
  
  *IF We are at the End of the temp. Order lines file
  IF EOF()
    llEOF = .T.
    SKIP -2
    
    *IF We are at the Begin of the temp. Order lines file
    IF BOF()
      llBOF = .T.
    ELSE    && Else
      SKIP 1
    ENDIF    && End of IF
  ELSE    && Else
    SKIP -2
    
    *IF We are at the Begin of the temp. Order lines file
    IF BOF()
      llBOF = .T.
    ELSE    && Else
      SKIP 1
    ENDIF    && End of IF
  ENDIF    && End of IF
ELSE    && Else
  llEOF = .T.
  llBOF = .T.
ENDIF    && End of IF

laCtrStat[1]  = IIF(llBOF , 'DISABLE' , 'ENABLE')         && First button 
laCtrStat[4]  = laCtrStat[1]                              && Priveus button
laCtrStat[2]  = IIF(llEOF , 'DISABLE' , 'ENABLE')         && Last button
laCtrStat[3] = laCtrStat[2]                               && Next button

SHOW GET pbTop  &laCtrStat[1]
SHOW GET pbPrvs &laCtrStat[4]
SHOW GET pbBtm  &laCtrStat[2]
SHOW GET pbNxt  &laCtrStat[3]
SHOW MENU _mSysMenu

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Get fields and some of the Option grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfwOldVal

laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value

*C124051,1  TMI [Start] Note : I will deny editing lines for NOR01, since this is not compatible 
*C124051,1              with the specs of the new customization , that they need all styles to be 
*C124051,1              allocated or nothing is done  , FOR NOR01
IF ASCAN(laEvntTrig,'WHENRET')<>0
  RETURN gfDoTriger('ALAUTAL','WHENRET')
ENDIF
*C124051,1  TMI [End  ] 

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of the Style
*!*************************************************************
*! Called from :  Screen ALAUTAL [lcAutAlCh3 - Style field]
*!*************************************************************
*! Calls       : lfGScalCnt() , gfStyBrw() , gfModalGen() , gpAdStyWar()
*!               lfShowGets() , lfRefresh()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvStyle

PRIVATE lnOScalCnt , lnNScalCnt , llObjRet , lcOldAlias ,;
        lcOScale , lcNScale , lnSclRecNo

*IF the Style field was empty or not changed and the user dose not
*want to Browse
IF (EMPTY(lcStyle) .OR. lcStyle == laOldVal) .AND. !llBrowse
  lcStyle = laOldVal
  RETURN
ENDIF    && End of IF

lcOldAlias = ALIAS()               && Variable to save the old Alis
lnOScalCnt = lfGScalCnt(Style)     && Variable to save the old Scale number of sizes
lcOScale = STYLE.Scale             && Variable to save the old Scale

*IF The user want to Browse or if the Style he entered is not in the file
IF '?' $ lcStyle .OR. llBrowse .OR. !SEEK(lcStyle , 'STYLE'))
  ON KEY LABEL ALT+B
  lnSclRecNo = IIF(EOF('SCALE') .OR. BOF('SCALE') , 0 , RECNO('SCALE'))    && Variable to save the scale file record number
  lcStyle = gfStyBrw('I',"","",.F.)
  ON KEY LABEL ALT+B ACTIVATE WINDOW (lcOrdLBrow)  
  
  *IF The user selected a Style from the Style Browse
  IF !EMPTY(lcStyle)
    lcStyle = PADR(lcStyle , 19)
  ELSE    && Else
    lcStyle = laOldVal
    
    *IF We have a valid SCALE record number
    IF lnSclRecNo <> 0
      GO lnSclRecNo IN 'SCALE'
    ELSE    && Else
      GO BOTTOM IN 'SCALE'
      SKIP 1 IN 'SCALE'
    ENDIF    && End of IF
  ENDIF    && End of IF
  llBrowse = .F.
ENDIF    && End of IF

*B802744,1 No need to the following condition because we can now pick non-inventory styles.[Begin]
*IF The user has changed the Style and the new style is a non-inventory
*style
*B802744,1 IF lcStyle <> laOldVal .AND. !STYLE.lInvSty
  
    *** Message : "The selected style is a non-inventory style. "
    ***           "cannot proceed.                              "
    ***           "                    < Ok >                   "
    *B802744,1 =gfModalGen('TRM38140B00000', 'DIALOG')
    *B802744,1 lcStyle = laOldVal          && Restore the old value
*B802744,1 ENDIF    && End of IF lcStyle <> laOldVal .AND. !STYLE.lInvSty
*B802744,1 No need to the following condition because we can now pick non-inventory styles.[End  ]

*IF The user has changed the Style
IF lcStyle <> laOldVal
  lnNScalCnt = lfGScalCnt(lcStyle)       && Variable to save the new Scale number of sizes
  lcNScale = STYLE.Scale                 && Variable to save the new Scale
  
  *IF the new Scale number of sizes is less than the old Scale number of sizes
  IF lnNScalCnt < lnOScalCnt
    
    *** Message : "The number of sizes for ð/Scale ð is less than the"
    ***           "number of sizes for ð/Scale ð . Cannot proceed    "
    ***           "                        < Ok >                    "
    =gfModalGen('TRM44009B00000','DIALOG' , lcStyleTtl + '|' + ALLTRIM(lcStyle) + '/' + ALLTRIM(lcNScale) + '|' + lcStyleTtl + '|' + ALLTRIM(laOldVal) + '/' + ALLTRIM(lcOScale))
    lcStyle = laOldVal          && Restore the old value
  ELSE    && Else
    lcStyScale = lcNScale
  ENDIF    && End of IF
ENDIF    && End of IF

*IF The user has changed the Style
IF lcStyle <> laOldVal
  =SEEK('O' + &lcTmpOrdLn..Order , 'ORDHDR')
  
  *IF The Style division is not the same as the Order division
  IF STYLE.cDivision <> ORDHDR.cDivision
    
    *** Message : "ð / ð confilct!                               "
    ***           "                        < Ok >                    "
    =gfModalGen('TRM42086B00000','DIALOG' , lcStyleTtl + '|Division')
    lcStyle = laOldVal          && Restore the old value
  ENDIF    && End of IF
ENDIF    && End of IF

*IF The user has changed the Style
IF lcStyle <> laOldVal
  =SEEK('O' + &lcTmpOrdLn..Order , 'ORDHDR')
  
  *IF The Style season is not the same as the Order season
  IF ORDHDR.Season <> '*' .AND. STYLE.Season <> ORDHDR.Season
    
    *** Message : "Style / ð confilct!                               "
    ***           "                        < Ok >                    "
    =gfModalGen('TRM42086B00000','DIALOG' , lcStyleTtl + '|Season')
    lcStyle = laOldVal          && Restore the old value
  ENDIF    && End of IF
ENDIF    && End of IF

SELECT (lcOldAlias)

=SEEK(lcStyle , 'STYLE')

*IF the Style was changed and the new Style is not assigned to the Warehouse
IF lcStyle <> laOldVal .AND. !SEEK(lcStyle + lcWareCode + SPACE(10) , 'STYDYE')
  
  *IF Statment to check if the user is going to add the Style to the
  *Warehouse
  *** Message : "ð is not assigned to warehouse ð. "
  ***           "     < Yes >           < No >     "
  IF gfModalGen('TRM42001B00006','DIALOG' , lcStyleTtl + ' ' + ALLTRIM(lcStyle) + '|' + ALLTRIM(lcWareCode) + '. Do you wish to assign it') = 1
    =gpAdStyWar(lcStyle , SPACE(10) , lcWareCode)
  ENDIF    && End of IF
ENDIF    && End of IF

=SEEK(lcStyle + lcWareCode + SPACE(10) , 'STYDYE')

*IF the Style was changed 
IF lcStyle <> laOldVal
  =lfShowGets(.F.)
  =lfRefresh()
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvWareCode
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of the Warehouse 
*!*************************************************************
*! Called from :  Screen ALAUTAL [lcAutAlCh3 - Warehouse field]
*!*************************************************************
*! Calls       : gfBrowWare() , gpAdStyWar() , lfShowGets() , lfRefresh()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvWareCode

PRIVATE lcOldWare

lcOldWare = lcWareCode
lcWareCode = laWareCode[lnWareCode]

*IF the Warehouse field was empty or not changed
IF (EMPTY(lcWareCode) .OR. lcWareCode == lcOldWare)
  lcWareCode = lcOldWare
  lnWareCode = laOldVal
  RETURN
ENDIF    && End of IF

*IF the Warehouse was changed and the Style is not assigned to the new
*Warehouse
IF !SEEK(lcStyle + lcWareCode + SPACE(10) , 'STYDYE') .AND. lcWareCode <> lcOldWare
  
  *IF Statment to check if the user is going to add the Style to the
  *Warehouse
  *** Message : "ð is not assigned to warehouse ð. "
  ***           "     < Yes >           < No >     "
  IF gfModalGen('TRM42001B00006','DIALOG' , lcStyleTtl + ' ' + ALLTRIM(lcStyle) + '|' + ALLTRIM(lcWareCode) + '. Do you wish to assign it') = 1
    =gpAdStyWar(lcStyle , SPACE(10) , lcWareCode)
  ENDIF    && End of IF
ENDIF    && End of IF

=SEEK(lcStyle + lcWareCode + lcDyelot , 'STYDYE')

*IF the Warehouse was changed 
IF lcWareCode <> lcOldWare
  =lfShowGets(.F.)
  =lfRefresh()
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvDyelot
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of the Dyelot 
*!*************************************************************
*! Called from :  Screen ALAUTAL [lcAutAlCh3 - Dyelot field]
*!*************************************************************
*! Calls       : lfShowGets() , lfRefresh() , SDyeBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvDyelot

PRIVATE llObjRet , lcOldAlias

*IF the Dyelot field was empty or not changed and the user dose not want to
*Browse
IF (EMPTY(lcDyelot) .OR. lcDyelot == laOldVal) .AND. !llBrowse
  
  *IF The Dyelot field was Changed [if the User removed the Dyelot]
  IF lcDyelot <> laOldVal
    =lfShowGets(.F.)
    =lfRefresh()
  ENDIF    && End of IF
  RETURN
ENDIF    && End of IF

lcOldAlias = ALIAS()               && Variable to save the old Alis

*IF The user want to Browse or if the Dyelot he entered is not in the file
IF '?' $ lcDyelot .OR. llBrowse .OR. !SEEK(lcStyle + lcWareCode + lcDyelot , 'STYDYE')
  llObjRet = SDyeBrow(lcStyle , @lcDyelot , .F. , lcWareCode, .T., .T.)
  lcDyelot = IIF(llObjRet , lcDyelot , laOldVal)
  llBrowse = .F.
ENDIF    && End of IF

SELECT (lcOldAlias)

=SEEK(lcStyle + lcWareCode + lcDyelot , 'STYDYE')

*IF the Dyelot was changed 
IF lcDyelot <> laOldVal
  =lfShowGets(.F.)
  =lfRefresh()
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvAloQty
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of the Allocated Get fields 
*!*************************************************************
*! Called from :  Screen ALAUTAL [lcAutAlCh3 - Allocated Get fields]
*!*************************************************************
*! Calls       : gfModalGen() , lfRefresh()
*!*************************************************************
*! Passed Parameters : [The number of the Allocated Get field to Valid]
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvAloQty

PARAMETERS lnParm

PRIVATE lcParm

*B039660,1 NNA 02/14/2006 (BEGIN) If we're in the edit mode I'll check if this piktkt has a packing list or not
*B039660,1 NNA            to prevent user from changing the piktkt Qty.
IF ASCAN(laEvntTrig , PADR('CHKPACK',10)) <> 0 AND !gfDoTriger('ALAUTAL',PADR('CHKPACK',10))
  RETURN
ENDIF

*IF the Allocated quantity is less than 0
IF laPik[lnParm] < 0
  *-- MAB 01/09/1999 Force user to enter positive values. [Begin]
  *Message : 44081 ==> A negative value is not allowed.
  *Button  : 00000 ==> < Ok >
  =gfModalGen('INM44081B00000', 'DIALOG')
  laPik[lnParm] = laOldVal
  _CUROBJ = _CUROBJ
  RETURN
  *-- MAB 01/09/1999 Force user to enter positive values. [End  ]
  
ENDIF    && End of IF

*-- MAB 09/01/1999 Check Stock quantity (Before Force allocation) [Begin]
PRIVATE lnAvalStk
lcParm = ALLTRIM(STR(lnParm))

*B039660,1 NNA 02/14/2006 (Begin) Fix bug that if The Picked qty = The Available qty. you cann't decrease the picked qty
*lnAvalStk = IIF(EOF('STYLE') , 0 , MAX(STYDYE.Stk&lcParm - STYDYE.Alo&lcParm, 0))
lnAvalStk = IIF(EOF('STYLE') , 0 , MAX(STYDYE.Stk&lcParm - (STYDYE.Alo&lcParm-laOldVal), 0))
*B039660,1 NNA (End)

IF (laPik[lnParm] > lnAvalStk) AND !lfForceMe()
  laPik[lnParm] = laOldVal
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
*-- MAB 09/01/1999 Check Stock quantity (Before Force allocation) [End  ]

*IF the Allocated quantity is greater than the Ordered quantity
IF laPik[lnParm] > laQty[lnParm]
  
  *IF Statment to check if the user is going to Increase the ordered
  *quantity
  *** Message : "Quantity allocated is greater than ordered. Increase "
  ***           "ordered quantity?                                    "
  ***           "                < Yes >           < No >             "
  IF gfModalGen('TRM44002B00006','DIALOG') = 1
    lnTotQty = lnTotQty - laQty[lnParm] + laPik[lnParm]
    laQty[lnParm] = laPik[lnParm]
    llIncOrd = .T.
  ELSE    && Else
    laPik[lnParm] = laOldVal
  ENDIF    && End of IF
ENDIF    && End of IF

*IF the Allocated quantity was changed 
IF laPik[lnParm] <> laOldVal
  lnChangAlo = IIF(laPik[lnParm] = &lcTmpOrdLn..Pik&lcParm , lnChangAlo - 1 ,;
               IIF(laOldVal = &lcTmpOrdLn..Pik&lcParm , lnChangAlo + 1 ,;
               lnChangAlo))
  
  lnTotPik = lnTotPik + laPik[lnParm] - laOldVal

  *C037816,1 MHM 04/06/2004 Custom Release PikTkt from custom table for DL[Start]
   IF ASCAN(laEvntTrig,PADR("ALMODAUT",10)) <> 0
     = gfDoTriger("ALAUTAL",PADR("ALMODAUT",10)) 
   ENDIF 
   *C037816,1 MHM [End]

  =lfRefresh()
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvSelect
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of push button Select
*!*************************************************************
*! Called from : Scrren ALAUTAL [lcAutAlCh2 - Select Push button]
*!*************************************************************
*! Calls       : lfvpbSel() , lfShowGets()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSelect
SELECT (lcTmpOrdLn)
*MAB
= lfHandlSel("SEL_UNSEL")
*-- end of lfvSelect.

*!*************************************************************
*! Name      : lfvSelAll
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of push button Select all
*!*************************************************************
*! Called from : Scrren ALAUTAL [lcAutAlCh2 - Select all Push button]
*!*************************************************************
*! Calls       : lfShowGets()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSelAll
*MAB
= lfHandlSel("SEL_ALL")
*-- end of lfvSelAll.

*!*************************************************************
*! Name      : lfvSelNon
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of push button Select none
*!*************************************************************
*! Called from : Scrren ALAUTAL [lcAutAlCh2 - Select none Push button]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSelNon
*MAB 
= lfHandlSel("SEL_NON")
*-- end of lfvSelNon.

*!*************************************************************
*! Name      : lfvInvert
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of push button Invert
*!*************************************************************
*! Called from : Scrren ALAUTAL [lcAutAlCh2 - Invert Push button]
*!*************************************************************
*! Calls       : lfvpbSel() , lfShowGets()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvInvert
*MAB
= lfHandlSel("INVERT")
*-- end of lfvInvert.

*!*************************************************************
*! Name      : lfvpbSel
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Function to arange the push button Select prompt
*!*************************************************************
*! Called from : lfvSelect() , lfvInvert() , The Browse [lcOrdLBrow]
*!               lpShow() , lfvScope() , lfAloScr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T.
*!*************************************************************
*
FUNCTION lfvpbSel

*IF The record is selected
IF LLSEL
  SHOW GET pbSelect,1 PROMPT 'UnSe\<lect'  &lcInvertSt
ELSE    && Else
  SHOW GET pbSelect,1 PROMPT 'Se\<lect' &lcInvertSt
ENDIF    && End of IF

RETURN .T.

*!*************************************************************
*! Name      : lfwIbBrow
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : When function of the invisible button IbBrow
*!*************************************************************
*! Called from : Scrren ALAUTAL [lcAutAlCh2 - invisible button IbBrow]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T. or .F.
*!*************************************************************
*
FUNCTION lfwIbBrow

*IF The left mouse button is not pressed
IF !MDOWN()
  KEYBOARD "{ALT+B}" CLEAR 
  RETURN .T.
ENDIF    && End of IF

RETURN .F.

*!*************************************************************
*! Name      : lfBrowTrap
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Function to trap the keys for the Browse , 
*!             and save the changes if the current record was edited
*!*************************************************************
*! Called from : Scrren ALUATAL Deactivate Function
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfBrowTrap

*IF The window on top is the Browse
IF WONTOP(lcOrdLBrow)
  glFromBrow = .T.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB DO lfTraps WITH 1
  ON KEY LABEL BACKTAB DO lfTraps WITH 2
  ON KEY LABEL Alt+L DO lfvSelect
  ON KEY LABEL Alt+A DO lfvSelAll
  ON KEY LABEL Alt+N DO lfvSelNon
  ON KEY LABEL Alt+I DO lfvInvert
  ON KEY LABEL ALT+B 
ENDIF    && End of IF

*IF We are coming from the screen [lcAutAlCh3]
IF WLAST(lcAutAlCh3)
  SELECT (lcTmpOrdLn)
  
  *IF Any of the Syle or Warehouse or Dyelot was changed
  IF lcStyle + lcWareCode + lcDyeLot <> Style + cWareCode + DyeLot
    llAloRec = IIF(lnTotPik = 0 , .F. , .T.)
  ELSE    && Else
    llAloRec = IIF(llIncOrd .OR. lnChangAlo > 0 , .T. , .F.)
  ENDIF    && End of IF
  
  *IF The current record was edited
  IF llAloRec
    =lfAllocate(3 , RECNO(lcTmpOrdLn))
    =lfShowGets(.T.)
  ENDIF    && End of IF
  llAloRec = .F.
  llIncOrd = .F.
  lnChangAlo = 0

  *IF The system use Dyelots
  IF llUseDyes
    SET RELATION TO Style + cWareCode + DyeLot INTO STYDYE
  ELSE    && Else
    SET RELATION TO Style + cWareCode + SPACE(10) INTO STYDYE
  ENDIF    && End of IF
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfBrwUnTrp
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Function to untrap the keys for the Browse
*!*************************************************************
*! Called from : Scrren ALAUTAL Activate Function
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfBrwUnTrp

*IF The window on top is not the Browse and coming from the Browse
IF !WONTOP(lcOrdLBrow) .AND. glFromBrow
  = gfStopBrow()
  glFromBrow = .F.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL Alt+L
  ON KEY LABEL Alt+A
  ON KEY LABEL Alt+N
  ON KEY LABEL Alt+I
  ON KEY LABEL ALT+B ACTIVATE WINDOW (lcOrdLBrow)
ENDIF    && End of IF

*IF We are going to the screen [lcAutAlCh3]
IF WONTOP(lcAutAlCh3)
  SELECT (lcTmpOrdLn)
  SET RELATION TO
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfActPad
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Called from : ALAUTAL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfActPad


DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P
ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP

DEFINE POPUP _OPTIONPOP MARGIN SHADOW

*E300989,1 Now There is no need to Skip for llScope flag because Scope
*E300989,1 is Enabled allover .
*DEFINE BAR 4 OF _OPTIONPOP PROMPT '\<Scope'                       SKIP FOR llScope
*E300989,1

DEFINE BAR 1 OF _OPTIONPOP PROMPT '\<Scope'
DEFINE BAR 2 OF _OPTIONPOP PROMPT '\-'                          SKIP FOR .T.
DEFINE BAR 3 OF _OPTIONPOP PROMPT '\<Allocate selected records' SKIP FOR (lnSelRec = 0)

*MAB at 08/22/1999 Release option not in Options menu. [Begin]
DEFINE BAR 4 OF _OPTIONPOP PROMPT '\<Release selected records' SKIP FOR (lnSelAlo = 0)
*MAB at 08/22/1999 Release option not in Options menu. [End  ]

*C200084,1 Generate pick ticket option if not FP program [Begin]
IF TYPE("laPanelObj[4,1]") = "C"
  DEFINE BAR 5 OF _OPTIONPOP PROMPT '\-'                          SKIP FOR .T.
  DEFINE BAR 6 OF _OPTIONPOP PROMPT '\<Generate picking tickets'  SKIP FOR (lnSelAlo = 0)
ENDIF  
*C200084,1 Generate pick ticket option if not FP program [Begin]

ON SELECTION POPUP _OPTIONPOP DO lfvActBar

*!*************************************************************
*! Name      : lfvActBar
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : POPUP _OPTIONPOP SELECTION 
*!*************************************************************
*! Called from : POPUP _OPTIONPOP
*!*************************************************************
*! Calls       : lfAloScr() , lfRelScr() , lfGenScr , lfvScope()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvActBar

*B804233,1 HBG 05/08/2001 Zap The temp Aprove file to allow Checking 
*B804233,1                Order Approve Amount agian. [Begin]
=lfAprov()
*B804233,1 [End]

DO CASE

  CASE BAR() = 1      && Scope
    =lfvScope()

  CASE BAR() = 3      && Allocation logic and Allocate selected records
    =lfAlocGrid() AND llRpAlocat AND lfAloScr() AND llRpGenPik AND lfGenScr() 

  CASE BAR() = 4      && Release selected records
    =lfRelScr()

  CASE BAR() = 6      && Generate picking tickets options and Generate.
    =lfPickGrid() AND llRpGenPik AND lfGenScr()

ENDCASE    && End of DO CASE Statment


*!*************************************************************
*! Name      : lfTraps
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Function to be called from some traped keys
*!*************************************************************
*! Called from : lfBrowTrap()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1 [TAB] , 2 [BACKTAB]
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfTraps

PARAMETERS lnTrap


DO CASE 
  
  *CASE of TAB
  CASE lnTrap = 1
    ACTI WINDOW (lcAutAlCh2) TOP
    _CUROBJ = OBJNUM(pbSelect)

  *CASE of BACKTAB
  CASE lnTrap = 2
    ACTI WINDOW (lcAutAlCh2) TOP  
    _CUROBJ = OBJNUM(ibDum1)

ENDCASE    && End of DO CASE Statment

*!*************************************************************
*! Name      : lfFndUnCSe
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Function to check for the exist of incompleted sessions
*!*************************************************************
*! Called from : ALAUTAL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T. If there is any incompleted sessions.
*!               .F. Otherwise.
*!*************************************************************
*
FUNCTION lfFndUnCSe

PRIVATE llReturn

llReturn = gfUnCompSession('ALAUTAL' , lnSessNo , 'Automatic Allocation')

*-- If there is an incomplete session and user want to Continue it.
IF llReturn
  lcSession  = UNCMSESS.cSession      && Variable to hold the program session number
  lnUnCmRec  = RECNO('UNCMSESS')      && Variable to hold the record number of the incompleted session file
  lcBaseFile = lcTmpOrdLn
  *-- Prepare the needed variabels
  =lfPrepVar()
ENDIF

RETURN llReturn

*!*************************************************************
*! Name      : lfvAccount
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of the Account
*!*************************************************************
*! Called from : Option grid [Account Get field]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvAccount

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvOGStyle
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of the Style
*!*************************************************************
*! Called from : Option grid [Style Get field]
*!*************************************************************
*! Calls       : gfStyBrw()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvOGStyle

*E300989,1 Filter now is on Style Major field instead of Style.
*E300989,1 thus define variables to store style file status.
PRIVATE lnCurSelct,lcStyOrder
lnCurSelct = SELECT(0)
SELECT STYLE
lcStyOrder = ORDER()
SET ORDER TO cStyle 
*E300989,1

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Style he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'STYLE'))

  *E300989,1 Browse major segment only.
  *lcObjVal = gfStyBrw('I',"","",.F.)
  lcObjVal = gfStyBrw('M',"","",.F.)  &&Browse style major only.

  lcObjVal = IIF(!EMPTY(lcObjVal) , lcObjVal , laOldVal)
  &lcObjName = lcObjVal

ENDIF    && End of IF

*E300989,1 restore style file's status.
SELECT STYLE
SET ORDER TO &lcStyOrder
SELECT (lnCurSelct)
*E300989,1
*-- end of lfvOGStyle.


*!*************************************************************
*! Name      : lfvOrder
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of the Order number
*!*************************************************************
*! Called from : Option grid [Order number Get field]
*!*************************************************************
*! Calls       : OrdBrowO()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvOrder

PRIVATE lcObjNam , lcObjVal , llObjRet

lcObjNam = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
lcObjVal = IIF(EMPTY(lcObjVal) , lcObjVal , PADL(ALLTRIM(lcObjVal) , 6 , '0'))

*IF The user want to Browse or if the Order number he entered is not in
*the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('O' + lcObjVal , 'ORDHDR'))
  llBrowse = .T.
  llObjRet = OrdBrowO(@lcObjVal , .F. , 'O')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  llBrowse = .F.
ENDIF    && End of IF
&lcObjNam = lcObjVal

*!*************************************************************
*! Name      : lfvFabric
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of the Fabric
*!*************************************************************
*! Called from : Option grid [Fabric Get field]
*!*************************************************************
*! Calls       : FaBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvFabric

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Fabric he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'FABRIC'))
  llObjRet = FaBrow(@lcObjVal , '*')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvOrdWare
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of the Order Warehouse
*!*************************************************************
*! Called from : Option grid [Order Warehouse Get field]
*!*************************************************************
*! Calls       : gfBrowWare()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvOrdWare

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Warehouse he entered is not in the
*file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , laOldVal , lcObjVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvCutTkt
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of the Cut ticket number
*!*************************************************************
*! Called from : Option grid [Cut ticket number Get field]
*!*************************************************************
*! Calls       : CutBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvCutTkt

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
lcObjVal = IIF(EMPTY(lcObjVal) , lcObjVal , PADL(ALLTRIM(lcObjVal) , 6 , '0'))

*IF The user want to Browse or if the Cut ticket number he entered is not
*in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'CUTTKTH'))
  llObjRet = CutBrow(@lcObjVal)
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
ENDIF    && End of IF
&lcObjName = lcObjVal

*!*************************************************************
*! Name      : lfvPO
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of the PO number
*!*************************************************************
*! Called from : Option grid [PO number Get field]
*!*************************************************************
*! Calls       : PosBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvPO

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
lcObjVal = IIF(EMPTY(lcObjVal) , lcObjVal , PADL(ALLTRIM(lcObjVal) , 6 , '0'))

*IF The user want to Browse or if the PO number he entered is not in the
*file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('P' + lcObjVal , 'POSHDR'))
  llObjRet = PosBrow(@lcObjVal , '' , 'P')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
ENDIF    && End of IF
&lcObjName = lcObjVal

*!*************************************************************
*! Name      : lfvCutUnt
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of the Cut of units
*!*************************************************************
*! Called from : Option grid [Cut of units Get field]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvCutUnt

*IF The Cut of units value is less than 0
IF lnRpCutUnt < 0
  
  *** Message : "ð should be greater than zero."
  ***           "            < Ok >            "
  =gfModalGen("TRM00234B00000" , "DIALOG" , "Cut-off units")
  lnRpCutUnt = laOldVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvPikCor
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of the Pick coordinate Min %
*!*************************************************************
*! Called from : Option grid [Pick coordinate Min % Get field]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvPikCor

*IF The Pick coordinate Min % value is less than 0 or greater than 100
IF lnRpPikCor < 0 .OR. lnRpPikCor > 100
  
  *** Message : "ð range from ð to ð           "
  ***           "            < Ok >            "
  =gfModalGen("TRM00272B00000" , "DIALOG" , "Pick coordinate groups min%|0|100")
  lnRpPikCor = laOldVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvPikSep
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of the Pick Separates Min %
*!*************************************************************
*! Called from : Option grid [Pick Separates Min % Get field]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvPikSep

*IF The Pick Separates Min % value is less than 0 or greater than 100
IF lnRpPikSep < 0 .OR. lnRpPikSep > 100
  
  *** Message : "ð range from ð to ð           "
  ***           "            < Ok >            "
  =gfModalGen("TRM00272B00000" , "DIALOG" , "Pick separates min%|0|100")
  lnRpPikSep = laOldVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvPkFWrh
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of the Pick from warehouse
*!*************************************************************
*! Called from : Option grid [Pick from warehouse Get field]
*!*************************************************************
*! Calls       : lfvOrdWare()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvPkFWrh
=lfvOrdWare()
*--

*!*************************************************************
*! Name      : lfvSort1
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of the Sort 1
*!*************************************************************
*! Called from : Option grid [Sort 1 Option]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSort1
lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field

*IF The Sort option was selected befor in Sort2 or Sort3
IF INLIST(lnRpSort1 , lnRpSort2 , lnRpSort3)
  lnRpSort1 = laOldVal
  &lcObjName = laOldVal
ENDIF    && End of IF
SHOW GET &lcObjName

lcSort = &lcObjName
*CLEAR READ

*!*************************************************************
*! Name      : lfvSort2
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of the Sort 2
*!*************************************************************
*! Called from : Option grid [Sort 2 Option]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSort2

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field

*IF The Sort option was selected befor in Sort1 or Sort3
IF INLIST(lnRpSort2 , lnRpSort1 , lnRpSort3)
  lnRpSort2 = laOldVal
  &lcObjName = laOldVal
ENDIF    && End of IF
SHOW GET &lcObjName

*!*************************************************************
*! Name      : lfvSort3
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of the Sort 3
*!*************************************************************
*! Called from : Option grid [Sort 3 Option]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSort3

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field

*IF The Sort option was selected befor in Sort2 or Sort1
IF INLIST(lnRpSort3 , lnRpSort1 , lnRpSort2)
  lnRpSort3 = laOldVal
  &lcObjName = laOldVal
ENDIF    && End of IF
SHOW GET &lcObjName

*!*************************************************************
*! Name      : lfvPikCorF
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of the Pick coordinate
*!*************************************************************
*! Called from : Option grid [Pick coordinate Option]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvPikCorF
*E300989,1 This function is no longer in use.
laOGObjCnt[1] = llRpPikCor
laOGObjCnt[4] = IIF(llRpForAlo .OR. !llRpPikCor , .F. , .T.)
=lfOGShowGet('llRpPikSep')
=lfOGShowGet('lnRpPikCor')

*!*************************************************************
*! Name      : lfvPikSepF
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of the Pick Separates
*!*************************************************************
*! Called from : Option grid [Pick Separates Option]
*!*************************************************************
*! Calls       : lfOGShowGet
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvPikSepF
*E300989,1 This function is no longer in use.

laOGObjCnt[3] = llRpPikSep
laOGObjCnt[2] = IIF(llRpForAlo .OR. !llRpPikSep , .F. , .T.)
=lfOGShowGet('llRpPikCor')
=lfOGShowGet('lnRpPikSep')

*!*************************************************************
*! Name      : lfvScopMod
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of the Select by
*!*************************************************************
*! Called from : Option grid [Select by Option]
*!*************************************************************
*! Calls       : lfChangeGrid()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvScopMod

*B802267,1 Clear Pre Selected Range. [Begin]
llClrSty1 = (lcRpScpMod # "S")
llClrOrd1 = (lcRpScpMod # "O")
llClrAcc1 = (lcRpScpMod # "A")
*B802267,1 Clear Pre Selected Range. [End  ]

*B803379,1 HBG 07/05/2000 get the focus of current object [Begin] 
lnActivObj = _CUROBJ
*B803379,1 HBG 07/05/2000 get the focus of current object [End   

CLEAR READ
RETURN

*E300989,1 Comment Out the following block because  [begin]
*E300989,1 we use array instead of valid entry. 
*lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
*IF The Select by was changed
*IF lcRpScpMod <> lcOldScMod
  
  *IF Select by Cut tecket and the Manufacturing module is not installed
  *   IF lcRpScpMod = 'K' .AND. !'MF' $ gcCmpModules
  *     lcRpScpMod = lcOldScMod
  *     &lcObjName = laOldVal
  *     SHOW GET &lcObjName
  *     RETURN
  *   ENDIF    && End of IF
  
  *IF Select by PO and the Purchase order module is not installed
  * IF lcRpScpMod = 'P' .AND. !'PO' $ gcCmpModules
  *   lcRpScpMod = lcOldScMod
  *   &lcObjName = laOldVal
  *   SHOW GET &lcObjName
  *   RETURN
  * ENDIF    && End of IF

  *lcOldScMod = lcRpScpMod
  *=lfChangeGrid('ALAUTAL'+lcRpScpMod)
*ENDIF    && End of IF  
*E300989,1 Comment Out the following block because  [end]

*E300989,1 if user change select by from (C/T or P/O) to Other select by.
IF !(lcRpScpMod $ 'KP')
  llRpIncWip = .F.
ENDIF

lcOldScMod = lcRpScpMod
=lfChangeGrid('ALAUTAL'+lcRpScpMod)
*-- end of lfvScopMod.

*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : When function of the Option grid
*!*************************************************************
*! Called from : Option grid
*!*************************************************************
*! Calls       : lfOGShowGet()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfwOGWhen
*E300989,1 This function is no longer in use.

lcOldScMod = lcRpScpMod
laOGObjCnt[1] = llRpPikCor
laOGObjCnt[2] = IIF(llRpForAlo .OR. !llRpPikSep , .F. , .T.)
laOGObjCnt[3] = llRpPikSep
laOGObjCnt[4] = IIF(llRpForAlo .OR. !llRpPikCor , .F. , .T.)
laOGObjCnt[5] = IIF(llRpForAlo , .F. , .T.)

*E300989,1 Variable is [llRpExlDye] instead of [llExlStyDy] to save its 
*E300989,1 contents when restore from pre-defined filter setting.
*E300989,1 The following lines no longer used because I add suppress expression.
*laOGObjCnt[7] = llUseDyes
*=lfOGShowGet('llExlStyDy')
*=lfOGShowGet('llRpExlDye')
*E300989,1

=lfOGShowGet('llRpPikSep')
=lfOGShowGet('llRpPikCor')
=lfOGShowGet('lnRpPikSep')
=lfOGShowGet('lnRpPikCor')
=lfOGShowGet('lnRpCutUnt')

*!*************************************************************
*! Name      : lfCreatExp
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Function to create the selected critirea expression
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfCreatExp
PARAMETERS lnIncExcMd

PRIVATE lnBlok , lnExpArLen , lnScopExpr , lnOrdrExpr , lnArayLen ,;
        lcScopFile , lcScopFild , lcScopOptm , laTmpIndex

lnBlok = 1        && Varible to hold the OR number

lcScopFile = ' '         && Varible to hold the name of the main file of the data selection
lcScopFild = ' '         && Varible to hold the name of the main field of the data selection
lcScopOptm = ' '         && Varible to hold the full index expression for the Optmization

*DO CASE Statment
DO CASE
  
  *CASE of Select By all
  CASE lcRpScpMod = ' '
    
    *DO CASE Statment
    DO CASE
      
      *CASE of there is a selection criteria on the Style
      CASE lfExpFind('STYLE.')
        lcRpScpMod = 'S'
        lcScopFile = 'STYLE.'

      *CASE of there is a selection criteria on the Order header
      CASE lfExpFind('ORDHDR.')
        lcRpScpMod = 'O'
        lcScopFile = 'ORDHDR.'
        lcScopFild = 'ORDER'
        lcScopOptm = 'CORDTYPE + ORDER'

  ENDCASE    && End of DO CASE Statment
    
  *CASE of Select By Style
  CASE lcRpScpMod = 'S'
    lcScopFile = 'STYLE.'
    lcScopFild = 'STYLE'
    lcScopOptm = 'STYLE'

  *CASE of Select By Order
  CASE lcRpScpMod = 'O'
    lcScopFile = 'ORDHDR.'
    lcScopFild = 'ORDER'
    lcScopOptm = 'CORDTYPE + ORDER'

  *CASE of Select By Account
  CASE lcRpScpMod = 'A'
    lcScopFile = 'ORDHDR.'
    lcScopFild = 'ACCOUNT'
    lcScopOptm = 'ACCOUNT + CORDTYPE + ORDER'

  *CASE of Select By Cut ticket
  CASE lcRpScpMod = 'K'
    lcScopFile = 'CUTTKTH.'

  *CASE of Select By PO
  CASE lcRpScpMod = 'P'
    lcScopFile = 'POSHDR.'
    lcScopFild = 'PO'
    lcScopOptm = 'CSTYTYPE + PO'

ENDCASE    && End of DO CASE Statment

*FOR Loop to scan the array laOGVrFlt rows
FOR lnExpArLen = 1 TO ALEN(laFiltExp , 1)
  
  *B802267,1 Skip Range Filters to Optimize while Collect data [Begin]
  *IF laFiltExp[lnExpArLen,7]="R" AND USED(laFiltExp[lnExpArLen,6]) AND ;
  *   RECCOUNT(laFiltExp[lnExpArLen,6]) > 0
  IF laFiltExp[lnExpArLen,7]="R"
    IF USED(laFiltExp[lnExpArLen,6]) AND;
       RECCOUNT(laFiltExp[lnExpArLen,6]) > 0
      lcOptmFile = laFiltExp[lnExpArLen,6]
    ELSE
      lcOptmFile = ''
    ENDIF   
    LOOP
  ENDIF   
  *B802267,1 Skip Range Filters to Optimize while Collect data [End  ]
  
  *IF There is a value for this row or if this is one of the OR rows
  IF !lfEmpty(laFiltExp[lnExpArLen , 6] , laFiltExp[lnExpArLen , 3] = 'D');
     .OR. UPPER(ALLTRIM(laFiltExp[lnExpArLen , 1])) = '.OR.'
    
    *DO CASE Statment
    DO CASE
      
      *CASE of one of the OR rows
      CASE UPPER(ALLTRIM(laFiltExp[lnExpArLen , 1])) = '.OR.'
        lnBlok = lnBlok + 1

      *CASE of one of the main file fields
      CASE lcScopFile $ laFiltExp[lnExpArLen , 1]
        lnScopExpr = ALEN(laScopExpr , 1)
        DIMENSION laScopExpr(lnScopExpr + 1 , 2)
        laScopExpr[lnScopExpr + 1 , 1] = lnBlok
        laScopExpr[lnScopExpr + 1 , 2] = lfGetExp(lnExpArLen , lcScopFile , lcScopFild , lcScopOptm)

      *Otherwise
      OTHERWISE
        lnOrdrExpr = ALEN(laNormExpr , 1)
        DIMENSION laNormExpr(lnOrdrExpr + 1 , 2)
        laNormExpr[lnOrdrExpr + 1 , 1] = lnBlok
        laNormExpr[lnOrdrExpr + 1 , 2] = lfGetExp(lnExpArLen , '' , '' , '')

    ENDCASE    && End of DO CASE Statment
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*IF There is any selection criteria on the main file
IF ALEN(laScopExpr , 1) > 1
  
  *FOR Loop to scan the array laScopExpr rows
  FOR lnArayLen = 2 TO ALEN(laScopExpr , 1)
    laScopExpr[1,2] = laScopExpr[1,2] + IIF(lnArayLen = 2 , '' ,;
                      IIF(laScopExpr[lnArayLen,1] <> laScopExpr[lnArayLen - 1,1] ,;
                       ') .OR. (' , ' .AND. ')) +;
                      laScopExpr[lnArayLen,2]
    
  ENDFOR    && End of FOR Loop
  laScopExpr[1,2] = '(' + laScopExpr[1,2] + ')'
ENDIF    && End of IF

*IF There is any selection criteria at any file but the main file
IF ALEN(laNormExpr , 1) > 1
  
  *FOR Loop to scan the array laNormExpr rows
  FOR lnArayLen = 2 TO ALEN(laNormExpr , 1)
    laNormExpr[1,2] = laNormExpr[1,2] + IIF(lnArayLen = 2 , '' ,;
                      IIF(laNormExpr[lnArayLen,1] <> laNormExpr[lnArayLen - 1,1] ,;
                       ') .OR. (' , ' .AND. ')) +;
                      laNormExpr[lnArayLen,2]
    
  ENDFOR    && End of FOR Loop
  laNormExpr[1,2] = '(' + laNormExpr[1,2] + ')'
ENDIF    && End of IF


llStylRel = IIF(lcRpScpMod <> 'S' , .T. , .F.)
llOrdrRel = IIF(INLIST(lcRpScpMod , 'O' , 'A') , .F. , .T.)

SELECT (lcTmpOrdLn)
SET ORDER TO



*!*************************************************************
*! Name      : lfGetExp
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Function to create expression from the Option grid
*!             variable filter array rows
*!*************************************************************
*! Called from : lfCreatExp()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1)Option grid variable filter array row number ,
*!                     2)Main file for the data selection
*!                     3)Main field for the data selection
*!                     4)Full index expression for the Optmization
*!*************************************************************
*! Return      : The expression string
*!*************************************************************
*
FUNCTION lfGetExp

PARAMETERS lnElmeNumb , lcScopFile , lcScopFild , lcScopOptm

PRIVATE lcReturn , lcField , lcTypeSep1 , lcTypeSep2 , lcVal

lcReturn = ''          && Varible to hold the value to be returened

lcField = ALLTRIM(UPPER(laFiltExp[lnElmeNumb , 1]))        && Varible to hold the field name

*IF There is a Main file for the data selection
IF !EMPTY(lcScopFile)
  lcField = STRTRAN(lcField , lcScopFile , '')
ENDIF    && End of IF

*IF There is a Main field for the data selection
IF !EMPTY(lcScopFild)
  lcField = STRTRAN(lcField , lcScopFild , lcScopOptm)
ENDIF    && End of IF

lcTypeSep1 = IIF(laFiltExp[lnElmeNumb , 3] = 'C', '"' ,;
                 IIF(laFiltExp[lnElmeNumb , 3] = 'D' , '{' , ''))    && Varible to hold the Begin seperator

lcTypeSep2 = IIF(laFiltExp[lnElmeNumb , 3] = 'C', '"' ,;
                 IIF(laFiltExp[lnElmeNumb , 3] = 'D' , '}' , ''))    && Varible to hold the End seperator

*DO CASE Statment  
DO CASE
  CASE lcScopFild = 'PO'
    lcVal = lcTypeSep1 + 'P' + STRTRAN(laFiltExp[lnElmeNumb , 6] , '|' ,;
            lcTypeSep2 + ',' + lcTypeSep1 + 'P') + lcTypeSep2        && Varible to hold the right hand side of the expression
  
  CASE lcScopFild = 'ORDER' .AND.  lcField = lcScopOptm
    lcVal = lcTypeSep1 + 'O' + STRTRAN(laFiltExp[lnElmeNumb , 6] , '|' ,;
            lcTypeSep2 + ',' + lcTypeSep1 + 'O') + lcTypeSep2        && Varible to hold the right hand side of the expression
  
  OTHERWISE
    lcVal = lcTypeSep1 + STRTRAN(lfTStr(laFiltExp[lnElmeNumb , 6]) , '|' ,;
            lcTypeSep2 + ',' + lcTypeSep1) + lcTypeSep2        && Varible to hold the right hand side of the expression

ENDCASE    && End of DO CASE Statment  

*DO CASE Statment  
DO CASE
  
  *CASE The operator is [IS LIKE]
  CASE UPPER(ALLTRIM(laFiltExp[lnElmeNumb , 5])) = 'LIKE'
    lcReturn = lcField + ' ' +;
               IIF(laFiltExp[lnElmeNumb , 4] , '= ' , '<> ') + lcVal

  *CASE The operator is [GREATER THAN]
  CASE UPPER(ALLTRIM(laFiltExp[lnElmeNumb , 5])) = 'GREATER THAN'
    lcReturn = lcField + ' ' +;
               IIF(laFiltExp[lnElmeNumb , 4] , '> ' , '<= ') + lcVal
    
  *CASE The operator is [LESS THAN]
  CASE UPPER(ALLTRIM(laFiltExp[lnElmeNumb , 5])) = 'LESS THAN'
    lcReturn = lcField + ' ' +;
               IIF(laFiltExp[lnElmeNumb , 4] , '< ' , '>= ') + lcVal

  *CASE The operator is [GREATER OR EQUAL]
  CASE UPPER(ALLTRIM(laFiltExp[lnElmeNumb , 5])) = 'GREATER OR EQUAL'
    lcReturn = lcField + ' ' +;
               IIF(laFiltExp[lnElmeNumb , 4] , '>= ' , '< ') + lcVal

  *CASE The operator is [LESS OR EQUAL]
  CASE UPPER(ALLTRIM(laFiltExp[lnElmeNumb , 5])) = 'LESS OR EQUAL'
    lcReturn = lcField + ' ' +;
               IIF(laFiltExp[lnElmeNumb , 4] , '<= ' , '> ') + lcVal

  *CASE The operator is [BETWEEN]
  CASE UPPER(ALLTRIM(laFiltExp[lnElmeNumb , 5])) = 'BETWEEN'
    lcReturn = IIF(laFiltExp[lnElmeNumb , 4] , '' , '!') +;
               'BETWEEN(' + lcField + ',' + lcVal + ')'

  *CASE The operator is [IN LIST]
  CASE UPPER(ALLTRIM(laFiltExp[lnElmeNumb , 5])) = 'IN LIST'
    lcReturn = IIF(laFiltExp[lnElmeNumb , 4] , '' , '!') +;
               'INLIST(' + lcField + ',' + lcVal + ')'

ENDCASE    && End of DO CASE Statment
RETURN lcReturn

*!*************************************************************
*! Name      : lfVarDefVl
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Function to get the default value for the variabels
*!*************************************************************
*! Called from : Option grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : The name of the variable to get its default
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfVarDefVl
PARAMETERS lcVarName

PRIVATE lcReturn

lcReturn = &lcVarName      && Variable to hold the returned value
RETURN lcReturn

*!*************************************************************
*! Name      : lfSelData
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Function to create the temp. Order lines file
*!*************************************************************
*! Called from : lfvScope()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T. if there is any records for the selection criteria
*!               .F. Otherwise
*!*************************************************************
*
FUNCTION lfSelData
PRIVATE lcForCond1 , lcOrdHExp , lcStaus , lcBulk ,;
        lnOldRecCo , laTmpIndex

IF EMPTY(lcIndexExp)
  STORE IIF(EMPTY(lcOldIndex),lfEvalIndx(),lcOldIndex) TO lcOldIndex,lcIndexExp
ENDIF  

*E300989,1 
*-- lnCrSelect : Save current selected alias no.
*-- lcChildFil : Variable to hold name of child loop file[ORDLINE or Temp. ORDLINE]
PRIVATE lnCrSelect,lcChildFil
lcChildFil = IIF(llExclude,lcTmpOrdLn,'ORDLINE')
*E300989,1

SELECT (lcTmpOrdLn)

*E300989,1 Clear file and start new collection if Come from Scope only.
IF !llExclude
  *E300989,1 Refresh all used variables[Begin
  STORE 0 TO lnSelRec,lnSelAlo,lnAloRec,lnDellRec
  STORE 'DISABLE' TO lcAloSt,lcRelSt
  *E300989,1 Refresh all used variables[End..

  USE

  *E300989,1 Old dimension for laTmpIndex[2,2] but after add exclude option
  *E300989,1 I make it [3,2]
  *DIMENSION laTmpIndex[2,2]
  DIMENSION laTmpIndex[3,2]

  *E300989,1 Change index to have Order type before order (Like master file)
  *laTmpIndex[1,1] = 'order+STR(lineno,6)'
  laTmpIndex[1,1]  = 'cordtype+order+STR(lineno,6)'
  laTmpIndex[1,2]  = lcTmpIndex

  *E300989,1 Add new index
  laTmpIndex[2,1] = 'Style+DTOS(complete)+cordtype+order+store+STR(lineno,6)'
  laTmpIndex[2,2] = lcTmStyTag
  *E300989,1

  *E300989,1 I change the file position to row [3] instead of row [2],
  *E300989,1 and also Index expression now for one new temp. field, 
  *E300989,1 not lcIndexExp, to change it smoothing with out problems.
  *laTmpIndex[2,1] = lcIndexExp
  *laTmpIndex[2,2] = lcTmpOrdLn
  laTmpIndex[3,1] = 'cSortField'
  laTmpIndex[3,2] = lcTmpOrdLn
 
  *E300989,1
  =gfCrtTmp(lcTmpOrdLn , @laFileStru , @laTmpIndex)
  *C200084,1 Create new temporary file hold store numbers
  *C200084,1 which and store count [Begin]
  IF ASCAN(laEvntTrig,PADR("SPC_SETS",10)) <> 0
    lcSetSectn = "CRTTMP"  && Parameter passed to setting function.
    = gfDoTriger('ALAUTAL','SPC_SETS')
  ENDIF  
  *C200084,1 which and store count [End  ]

  *B603111,1 Create Temp. File hold piktkts generated in this session [Begin]
  =lfErsBldPk(.T.)
  *B603111,1 Create Temp. File hold piktkts generated in this session [End  ]

  =lfDispBrow()
ENDIF  

SET ORDER TO
GO TOP

lnOldRecCo = RECCOUNT(lcTmpOrdLn)        && Variable to hold the old number of records in temp. Order lines file
M.LLSEL = .F.
M.NPROCNO = 0

*E300989,1 Initial value for new fields added to Temporary file.
STORE '' TO  m.Priority,m.Entered,m.cDivision,m.cStyGroup,m.Fabric,;
             m.Pattern,m.cStyMajor,m.Trancd,m.CtktNo,m.cFabColor,m.cPeggedDye
*E300989,1

lcStaus = IIF(INLIST(lcRpScpMod , 'O' , 'A') , 'Status' , 'ORDHDR.Status')    && Variable to hold the name of the Order status field
lcBulk = IIF(INLIST(lcRpScpMod , 'O' , 'A') , 'Bulk' , 'ORDHDR.Bulk')    && Variable to hold the name of the Order bulk field
lcOrdHExp = '(' + IIF(INLIST(lcRpScpMod , 'O' , 'A') , '' , 'ORDHDR.') +;
            'cOrdType <> "C"' + ' .AND. ' + IIF(llRpIncHOr , 'INLIST(' +;
             lcStaus + ',"O","H")' , lcStaus + ' = "O"') + IIF(llRpExlBlk ,;
            ' .AND. ' + lcBulk + ' <> "Y")' , ')')          && Variable to hold the Order header filter

*E300989,1 if user select by (P/O or C/T) and it's exclude case [begin]
*E300989,1 we do not use lcForCond2, thus there is no need to evaluate it.
*lcForCond2 = '(TotQty <> 0 .AND. (EMPTY(PikTkt) .OR. PikTkt = "******")' +;
*             IIF(llStylRel , ' .AND. !EOF("STYLE") .AND. STYLE.lInvSty ' +;
*             IIF(llUseDyes .AND. llExlStyDy , ' .AND. STYLE.cDye_Flg <> "Y"' , '') , '') +;
*             IIF(llRpPikSep , '' , ' .AND. !EMPTY(Group)') +;
*             IIF(llRpPikCor , '' , ' .AND. EMPTY(Group)') +;
*             IIF(INLIST(lcRpScpMod , 'O' , 'A') , ')' , ' .AND. ' + lcOrdHExp + ')') +;
*             IIF(EMPTY(laNormExpr[1,2]) , '' , ' .AND. ' +;
*             ALLTRIM(laNormExpr[1,2]))        && Variable to hold the filter expression for all files but the Main

*E300989,1 if user select by (P/O or C/T) and it's exclude case

*B603544,4 HBG 07/11/2000 Comment this line to be able to fill "lcForCond2" 
*                         in case of if user select by (P/O or C/T) and it's 
*                         exclude case fill lcForCond2 [Begin]
*IF !((lcRpScpMod $ 'PK') AND llExclude)
*B603544,4 HBG 07/11/2000 Comment this line to be able to fill "lcForCond2" 
*                         in case of if user select by (P/O or C/T) and it's 
*                         exclude case fill lcForCond2 [End  ]
  PRIVATE lcForCond2,lcAlocFlt

  lcAlocFlt = IIF(lcRpAloNot="A",'','!')
  *B802744,1 Include all styles instead of inventory . [Begin]
  *lcForCond2 = '(TotQty <> 0' +;
               IIF(lcRpAloNot="B",'',' .AND. &lcAlocFlt.Picked') +;
               IIF(llStylRel , ' .AND. !EOF("STYLE") .AND. STYLE.lInvSty ' +;
               IIF(llUseDyes .AND. llRpExlDye , ' .AND. STYLE.cDye_Flg <> "Y"' , '') , '') +;
               IIF(llRpPikSep , '' , ' .AND. !EMPTY(Group)') +;
               IIF(llRpPikCor , '' , ' .AND. EMPTY(Group)') +;
               IIF(INLIST(lcRpScpMod , 'O' , 'A') , ')' , ' .AND. ' + lcOrdHExp + ')') +;
               IIF(EMPTY(laNormExpr[1,2]) , '' , ' .AND. ' +;
               ALLTRIM(laNormExpr[1,2]))        && Variable to hold the filter expression for all files but the Main

  *B606442,1 TMI [Start] Include only customers with status 'A' in lcForCond2
  *lcForCond2 = '(TotQty <> 0' +;
               IIF(lcRpAloNot="B",'',' .AND. &lcAlocFlt.Picked') +;
               IIF(llStylRel , ' .AND. !EOF("STYLE") ' +;
               IIF(llUseDyes .AND. llRpExlDye , ' .AND. STYLE.cDye_Flg <> "Y"' , '') , '') +;
               IIF(llRpPikSep , '' , ' .AND. !EMPTY(Group)') +;
               IIF(llRpPikCor , '' , ' .AND. EMPTY(Group)') +;
               IIF(INLIST(lcRpScpMod , 'O' , 'A') , ')' , ' .AND. ' + lcOrdHExp + ')') +;
               IIF(EMPTY(laNormExpr[1,2]) , '' , ' .AND. ' +;
               ALLTRIM(laNormExpr[1,2]))        && Variable to hold the filter expression for all files but the Main
  *B802744,1 Include all styles instead of inventory . [End  ]
  lcForCond2 = '(TotQty <> 0' +;
               IIF(lcRpAloNot="B",'',' .AND. &lcAlocFlt.Picked') +;
               IIF(llStylRel , ' .AND. !EOF("STYLE") ' +;
               IIF(llUseDyes .AND. llRpExlDye , ' .AND. STYLE.cDye_Flg <> "Y"' , '') , '') +;
               IIF(llRpPikSep , '' , ' .AND. !EMPTY(Group)') +;
               IIF(llRpPikCor , '' , ' .AND. EMPTY(Group)') +;
               IIF(INLIST(lcRpScpMod , 'O' , 'A') , ')' , ' .AND. ' + lcOrdHExp + ')') +;
               IIF(EMPTY(laNormExpr[1,2]) , '' , ' .AND. ' +;
               ALLTRIM(laNormExpr[1,2])) +;
               IIF(lcRpScpMod $ ' |S','.AND. CUSTOMER.STATUS = "A"', '')
  *B606442,1 TMI [End  ] Include only customers with status 'A' in lcForCond2
*B603544,4 HBG 07/11/2000 Comment this line to be able to fill "lcForCond2" 
*                         in case of if user select by (P/O or C/T) and it's 
*                         exclude case fill lcForCond2 [Begin]  
*ENDIF  && end if user select by (P/O or C/T) and it's exclude case.
*B603544,4 HBG 07/11/2000 Comment this line to be able to fill "lcForCond2" 
*                         in case of if user select by (P/O or C/T) and it's 
*                         exclude case fill lcForCond2 [End  ]
 
 
*E300989,1 if user select by (P/O or C/T) and it's exclude case [end]

*IF We need a relation between the lcChildFil file and the STYLE file
IF llStylRel
  SELECT STYLE
  SET ORDER TO TAG STYLE
  
  *E300989,1 Select most child file insted of ORDLINE file.
  *SELECT ORDLINE
  SELECT (lcChildFil)
  SET RELATION TO STYLE INTO STYLE
ENDIF    && End of IF

*IF We need a relation between the lcChildFil file and the ORDHDR file
IF llOrdrRel
  SELECT ORDHDR
  SET ORDER TO TAG ORDHDR

  *E300989,1 Select most child file insted of ORDLINE file.
  *SELECT ORDLINE
  SELECT (lcChildFil)
  SET RELATION TO 'O' + Order INTO ORDHDR ADDITIVE
ENDIF    && End of IF

*DO CASE Statment
DO CASE
  
  *CASE of Select by Style
  CASE lcRpScpMod = 'S'
  *>>>Use Both <condition is needed in lcForCond2>
    *B802744,1 Include all styles instead of inventory . [Begin]
    *lcForCond1 = ALLTRIM(laScopExpr[1,2]) + IIF(EMPTY(laScopExpr[1,2]) ,;
                'Status <> "X"' , ' .AND. Status <> "X"') + ' .AND. lInvSty' +;
                IIF(llUseDyes .AND. llRpExlDye ,;
                    ' .AND. STYLE.cDye_Flg <> "Y"' , '')      && Variable to hold the Main file filter expression

    lcForCond1 = ALLTRIM(laScopExpr[1,2]) + IIF(EMPTY(laScopExpr[1,2]) ,;
                'Status <> "X"' , ' .AND. Status <> "X"') +;
                IIF(llUseDyes .AND. llRpExlDye ,;
                    ' .AND. STYLE.cDye_Flg <> "Y"' , '')      && Variable to hold the Main file filter expression
    *B802744,1 Include all styles instead of inventory . [End  ]

    *E300989,1 Select most child file insted of ORDLINE file. [begin]
    PRIVATE lcTmpTag
    lcTmpTag = IIF(llExclude,lcTmStyTag,'ORDLINES')  && Style index.
    
    *SET ORDER TO TAG ORDLINES IN ORDLINE
    SET ORDER TO TAG &lcTmpTag IN (lcChildFil)
    *E300989,1 Select most child file insted of ORDLINE file. [end]

    *B606442,1 TMI [Start] set a relation between ordline and customer file
    SELECT (lcChildFil)
    SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER ADDITIVE
    *B606442,1 TMI [End  ] 

    SELECT STYLE

    *B802267,1 Code is in separate Function to easy tracing [Begin]
    IF EMPTY(lcOptmFile)
      SET ORDER TO
      =lfUpdatSty()
    ELSE
      SET ORDER TO Style
      SELECT(lcOptmFile)
      SCAN
        =lfUpdatSty(.T.)
      ENDSCAN  
    ENDIF  
    SET ORDER TO TAG STYLE IN STYLE
    *B802267,1 Code is in separate Function to easy tracing [End  ]
    
    *B606442,1 TMI [Start] release the relation
    SELECT (lcChildFil)
    SET RELATION OFF INTO CUSTOMER 
    *B606442,1 TMI [End  ] 

  *CASE of Select by Order or Select by Account
  CASE lcRpScpMod = 'O' .OR.  lcRpScpMod = 'A'
  *>>>Use Both - condition is needed in lcForCond1 only
    lcWaitStr = IIF(lcRpScpMod = 'O' , 'order ' , 'account ')
    *B606442,1 TMI [Start] Include only Active customers
    *lcForCond1 = ALLTRIM(laScopExpr[1,2]) + IIF(EMPTY(laScopExpr[1,2]) ,;
                 lcOrdHExp , ' .AND. ' + lcOrdHExp)      && Variable to hold the Main file filter expression
    lcForCond1 = ALLTRIM(laScopExpr[1,2]) + IIF(EMPTY(laScopExpr[1,2]) ,;
                 lcOrdHExp , ' .AND. ' + lcOrdHExp) +;
                 '.AND. CUSTOMER.STATUS = "A" '
    lcForCond2 = STRTRAN(lcForCond2,'.AND. CUSTOMER.STATUS = "A"')
    *B606442,1 TMI [End  ] Include only Active customers     
    *E300989,1 chage index to most child file index insted of ORDLINE file.
    PRIVATE lcTmpTag
    lcTmpTag = IIF(llExclude,lcTmpIndex,'ORDLINE')  && most Child file index.
    *SET ORDER TO TAG ORDLINE IN ORDLINE
    SET ORDER TO TAG &lcTmpTag IN (lcChildFil)
    *E300989,1

    SELECT ORDHDR
    *B802267,1 Code is in separate Function to easy tracing [Begin]
    IF EMPTY(lcOptmFile)
      SET ORDER TO
      =lfUpdatOrd()
    ELSE
      IF lcRpScpMod = 'O'
        SET ORDER TO ORDHDR
      ELSE
        SET ORDER TO Ordacct
      ENDIF  
      SELECT(lcOptmFile)
      SCAN
        =lfUpdatOrd(.T.)
      ENDSCAN  
    ENDIF  
    SET ORDER TO TAG ORDHDR IN ORDHDR
    *B802267,1 Code is in separate Function to easy tracing [End  ]
    
  *CASE of Select by Cut ticket
  CASE lcRpScpMod = 'K'
  *>>> Use both - condition is needed in lcForCond1 only
    *E300989,1 Status field is STATUS not STAUS
    *lcForCond1 = ALLTRIM(laScopExpr[1,2]) + IIF(EMPTY(laScopExpr[1,2]) ,;
    *             'Staus <> "X"' , ' .AND. Status <> "X"')      && Variable to hold the Main file filter expression
    *B606442,1 TMI [Start] Include only active customers
    *lcForCond1 = ALLTRIM(laScopExpr[1,2]) + IIF(EMPTY(laScopExpr[1,2]) ,;
                 'Status <> "X"' , ' .AND. Status <> "X"')      && Variable to hold the Main file filter expression    
    *E300989,1 If it's normal Scope case.
    lcForCond1 = ALLTRIM(laScopExpr[1,2]) + IIF(EMPTY(laScopExpr[1,2]) ,;
                 'Status <> "X"' , ' .AND. Status <> "X"') +;
                 ' .AND. CUSTOMER.STATUS = "A" '
    lcForCond2 = STRTRAN(lcForCond2,'.AND. CUSTOMER.STATUS = "A"')
    *B606442,1 TMI [End  ] Include only active customers
    IF !llExclude
      SET ORDER TO TAG ORDLINE IN ORDLINE
    ENDIF  
    SET ORDER TO TAG CUTPICK IN CUTPICK
    SET ORDER TO TAG (lcTmpIndex) IN (lcTmpOrdLn)

    SELECT CUTTKTH
    *B802267,1 Code is in separate Function to easy tracing [Begin]
    IF EMPTY(lcOptmFile)
      SET ORDER TO
      =lfUpdatCt()
    ELSE
      SET ORDER TO TAG CUTTKTH
      SELECT(lcOptmFile)
      SCAN
        =lfUpdatCt(.T.)
      ENDSCAN  
    ENDIF  
    SET ORDER TO TAG CUTTKTH IN CUTTKTH
    *B802267,1 Code is in separate Function to easy tracing [End  ]

  *CASE of Select by PO
  CASE lcRpScpMod = 'P'
  *>>> Use Both - condition is needed in lcForCond1 only
    *E300989,1 Status field is STATUS not STAUS
    *lcForCond1 = ALLTRIM(laScopExpr[1,2]) + IIF(EMPTY(laScopExpr[1,2]) ,;
    *             'Staus <> "X"' , ' .AND. Status <> "X"')      && Variable to hold the Main file filter expression
    *B606442,1 TMI [Start] Include only active customers
    *lcForCond1 = ALLTRIM(laScopExpr[1,2]) + IIF(EMPTY(laScopExpr[1,2]) ,;
                 'Status <> "X"' , ' .AND. Status <> "X"')      && Variable to hold the Main file filter expression    
    *E300989,1 if it's normal scope mode.
    lcForCond1 = ALLTRIM(laScopExpr[1,2]) + IIF(EMPTY(laScopExpr[1,2]) ,;
                 'Status <> "X"' , ' .AND. Status <> "X"') +;
                 ' .AND. CUSTOMER.STATUS = "A" '
    lcForCond2 = STRTRAN(lcForCond2,'.AND. CUSTOMER.STATUS = "A"')
    *B606442,1 TMI [End  ] Include only active customers
    IF !llExclude
      SET ORDER TO TAG ORDLINE IN ORDLINE
    ENDIF  
    SET ORDER TO TAG CUTPICK IN CUTPICK
    
    *E300989,1 Temporary table must be ordered.
    SET ORDER TO TAG (lcTmpIndex) IN (lcTmpOrdLn)
    *E300989,1

    SELECT POSHDR
    *B802267,1 Code is in separate Function to easy tracing [Begin]
    IF EMPTY(lcOptmFile)
      SET ORDER TO
      =lfUpdatPo()
    ELSE
      SET ORDER TO POSHDR
      SELECT(lcOptmFile)
      SCAN
        =lfUpdatPo(.T.)
      ENDSCAN  
    ENDIF  
    SET ORDER TO TAG POSHDR IN POSHDR
    *B802267,1 Code is in separate Function to easy tracing [End  ]
    
  *CASE of Select by all
  CASE lcRpScpMod = ' '
  *>>> use lcForCond2 only
    *E300989,1 Select most child file insted of ORDLINE file.
    *SELECT ORDLINE
    SELECT (lcChildFil)
    SET ORDER TO
    *B606442,1 TMI [Start] Set the relation between ordline and customer files
    SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER ADDITIVE    
    *B606442,1 TMI [End  ] 
    
    *SCAN Loop to scan the (lcChildFil) file FOR the selected selection criteria 
    SCAN FOR &lcForCond2
      *E300989,1 if Exclude mode
      IF llExclude
        *E300989,1 New code for exclude mode . [begin]
        WAIT WINDOW NOWAIT 'Excluding order ' + Order
        =lfExclRec()
        *E300989,1 New code for exclude mode . [end]
      
      ELSE  && Scope mode
        WAIT WINDOW NOWAIT 'Selecting order ' + Order
        SCATTER MEMVAR MEMO
        =lfUpdAloVr()
        
      ENDIF  
    ENDSCAN    && End of SCAN Loop
    
    *B606442,1 TMI [Start] release the relation 
    SELECT (lcChildFil)
    SET RELATION OFF INTO CUSTOMER
    *B606442,1 TMI [End  ] release the relation 
    
ENDCASE    && End of DO CASE Statment

*E300989,1 Select most child file insted of ORDLINE file, then break relation.
*SELECT ORDLINE
SELECT (lcChildFil)
SET RELATION TO
SET ORDER TO TAG ORDLINST IN ORDLINE

*E300989,1 Go top in temp. file, because in case of Exclude I return 
*E300989,1 EOF instead of old expression, to control enable and disable data.
GO TOP IN (lcTmpOrdLn)
*lnRecNumbr = RECCOUNT(lcTmpOrdLn) - lnOldRecCo
*RETURN RECCOUNT(lcTmpOrdLn) > lnOldRecCo
lnRecNumbr = RECCOUNT(lcTmpOrdLn) - IIF(llExclude,lnDellRec,lnOldRecCo)

*B603544,4 HBG 06/07/2000 Refresh Optimum file value for next loop [Begin]
lcOptmFile = ""
*B603544,4 HBG 06/07/2000 Refresh Optimum file value for next loop [End  ]

RETURN IIF(llExclude,!EOF(lcTmpOrdLn),RECCOUNT(lcTmpOrdLn) > lnOldRecCo)
*E300989,1
*-- end of lfSelData.

*!*************************************************************
*! Name      : lfExpFind
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Function to find if there is any selection criteria
*!             on a specific file
*!*************************************************************
*! Called from : lfCreatExp()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : File name to look for
*!*************************************************************
*! Return      : .T. if found
*!               .F. Otherwise
*!*************************************************************
*
FUNCTION lfExpFind

PARAMETERS lcFile
PRIVATE lnCount

*FOR Loop to scan the array laFiltExp rows
FOR lnCount = 1 to ALEN(laFiltExp , 1)
  
  *IF The field belong to the file we are looking for and the value [The
  *right hand side] is not empty
  IF lcFile $ laFiltExp[lnCount , 1] .AND. !EMPTY(laFiltExp[lnCount , 6])
    RETURN .T.
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

RETURN .F.

*!*************************************************************
*! Name      : lfAllocate
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Function to Allocate one or more line
*!*************************************************************
*! Called from : ALAUTAL.PRG , lfvScope() , lfAloScr() , lfGenScr() ,
*!               lfBrowTrap()
*!*************************************************************
*! Calls       : lfTmpAlo() , lfAloQty() , lfEditLine()
*!*************************************************************
*! Passed Parameters : 1) 1 to Allocate with the Option grid conditions
*!                      , 2 to Allocate the selected records
*!                      , 3 to edit a record
*!                     2) The number of the record to edit
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfAllocate

PARAMETERS lnParm , lnRecNo

PRIVATE lnTotRec , lnCurent , lcForCond , llAloGoup , lcGroupStr ,;
        llGrpFrRec , lnAvlFAlo , lcPikWare , lnRecNumb , llAloSep ,;
        lnOldRec , lnRecCount , lnPrepRec

SELECT (lcTmpOrdLn)

lnTotRec = IIF(lnParm = 1 , lnRecNumbr , lnSelRec)          && Varible to hold the Total count to be done for the thermometer
lnCurent = 0                                                && Varible to hold the current count to be done for the thermometer

*IF Not an incompleted session and not to edit a record
IF !llContinue .AND. lnParm <> 3
  lnRecCount = RECCOUNT() - lnDellRec        && Varible to hold the Total count to be done for the thermometer
  lnPrepRec = 0                              && Varible to hold the current count to be done for the thermometer
  
  *SCAN Loop to scan the temp. Order lines file
  SCAN
    lnPrepRec = lnPrepRec + 1
    REPLACE nProcNo WITH 0
    =gfThermo(lnRecCount , lnPrepRec , "Preparing data files" , '')
  ENDSCAN    && End of SCAN Loop
  =gfThermo(lnRecCount , lnRecCount , "Preparing data files" , '')
ELSE    && Else
  llContinue = IIF(lnParm = 3 , llContinue , .F.)
ENDIF    && End of IF

SET ORDER TO TAG STYDYE IN STYDYE
SET ORDER TO TAG ORDLINE IN ORDLINE
SET ORDER TO TAG STYLE IN STYLE
SET ORDER TO TAG ORDHDR IN ORDHDR
SELECT (lcTmpOrdLn)
SET ORDER TO TAG &lcTmpOrdLn

*IF Not to edit a record
IF lnParm <> 3
  SET RELATION TO Style INTO STYLE
ELSE    && Else
  SET RELATION TO
ENDIF    && End of IF

*DO CASE Statment
DO CASE
  
  *CASE of Allocate with the Option grid conditions
  CASE (lnParm = 1) OR (lnParm = 2)

    *B602574,1 Include work in process is in all cases [Begin]
    *          Thus I transfer this block from function lfDyeAlo to be here
    *          available for both lfDyeAlo and lfNormAlo functions.
    *-- if include wip and select by C/T or P/O and we have C/Ts or P/Os.
    IF llRpIncWip AND lcRpScpMod $ 'KP' AND !EMPTY(laIncExprs[1,6])
      DIMENSION laString1[1]
      =gfSubStr(laIncExprs[1,6],@laString1,'|')
    ENDIF

    *-- if include wip and exclude select by C/T or P/O and we have C/Ts or P/Os.
    IF llRpIncWip AND llExclude AND (lcRpExSlct $ 'KP') AND !EMPTY(laFiltExp[1,6])
      DIMENSION laString2[1]
      =gfSubStr(laFiltExp[1,6],@laString2,'|')
    ENDIF
    *B602574,1 Include work in process is in all cases [End  ]

    *E300989,1 add code to support system dyelot yes allocation [begin]
    IF llUseDyes AND llFabDye AND !llRpExlDye
      lcOldGroup = ''
      = lfDyeAlo(lnParm)
    
    ELSE  && else No dyelot allocation.
    
      = lfNormAlo(lnParm)
    ENDIF
  
  *E300989,1 Now Allocate the selected records is like Allocate [Begin
  *E300989,1 with the Option grid conditions, thus I Comment out this case.
  *CASE of Allocate the selected records
  *CASE lnParm = 2

    *E300989,1 add code to support system dyelot yes allocation [begin]
  *  IF llUseDyes AND llFabDye AND !llRpExlDye
  *    lcOldGroup = ''
  *    = lfDyeAlo(2)

  *  ELSE  && Normal allocation.
    
      *SCAN Loop to scan the temp. Order lines file the Selected records and
      *for nProcNo less than 5 and to skip Style with dyelot Yes and empty
      *dyelot field
      
  *    SCAN FOR llSel .AND. nProcNo < 5 .AND. ;
  *             !(llUseDyes .AND. EMPTY(DyeLot) .AND. STYLE.cDye_Flg = "Y")
    
  *      lnCurent = lnCurent + 1
      
        *IF Totaly picked
  *      IF TotPik = TotQty
  *        LOOP
  *      ENDIF    && End of IF
      
  *      llAloSep = lfTmpAlo(.T. , cWareCode , llForceAlc)      && Flag to know if we are going to allocate this record
          
        *IF We are going to allocate this record
  *      IF llAloSep
  *        =lfAloQty(cWareCode)
  *      ENDIF    && End of IF
  *      =gfThermo(lnTotRec , lnCurent , "Allocating..." ,;
  *                "Order number: " + Order)
      
  *    ENDSCAN    && End of SCAN Loop
  *    =gfThermo(lnTotRec , lnTotRec , "Allocating..." , "Order number: " + '')
  *  ENDIF  
  *E300989,1 Now Allocate the selected records is like Allocate [End..

  *CASE of edit a record
  CASE lnParm = 3
    
    lcCurrObj = 'lfAllocate(3,'+ALLTRIM(STR(lnRecNo))+')' 
    =lfAdUnCmSR()
    
    SELECT (lcTmpOrdLn)
    lnOldRec = RECNO()        && Variable to save the record number
    
    IF BETWEEN(lnRecNo,1,RECCOUNT())
      GO lnRecNo
    ENDIF  
    
    *IF Not an incompleted session
    IF !llContinue
      REPLACE nProcNo WITH 0
    ELSE    && Else
      llContinue = .F.
    ENDIF    && End of IF
    
    =lfEditLine()
    
    IF BETWEEN(lnOldRec,1,RECCOUNT())
      GO lnOldRec
    ENDIF  
    
    SCATTER FIELDS LIKE QTY* TO laQty
    lcRelSt = IIF(lnSelAlo = 0 , 'DISABLE' , 'ENABLE')
    SHOW GET pbRel &lcRelSt
    SHOW GET pbGen &lcRelSt
    =lfRefresh()
    
    lcCurrObj = ''
    =lfAdUnCmSR()

ENDCASE    && End of DO CASE Statment

SELECT (lcTmpOrdLn)
SET RELATION TO

*C200084,1 Adjust selected lines ration [Begin]
IF ASCAN(laEvntTrig,PADR("ADJRATIO",10)) <> 0
  = gfDoTriger('ALAUTAL','ADJRATIO')
ENDIF  
*C200084,1 Adjust selected lines ration [End  ]

*C124051,1  TMI [Start] Show log report for NOR01
IF ASCAN(laEvntTrig,PADR('SHOWLOG',10)) <> 0
  = gfDoTriger('ALAUTAL','SHOWLOG')
ENDIF  
*C124051,1  TMI [End  ] 

*-- end of lfAllocate.

*!*************************************************************
*! Name      : lfAloQty
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Function to update the needed files to Allocate 
*!             the current record of the temp. Order lines file
*!*************************************************************
*! Called from : lfAllocate()
*!*************************************************************
*! Calls       : gfAdd_Info()
*!*************************************************************
*! Passed Parameters : The Warehouse to Allocate from
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfAloQty
*E300989,1 Add new parameter to detect that updating dyelots.
*PARAMETERS lcWareCode
PARAMETERS lcWareCode,llHaveDye
PRIVATE lnProcVal

SELECT (lcTmpOrdLn)

*B804233,1 HBG 05/08/2001 Check order approve Amount[Begin]
IF llChkAprov
  lcExced = lfChkAprov() 
  IF  lcExced  = 'F' OR lcExced = 'E' && If Exceed befor or in this Line
  
    IF lcExced = 'E'  && If Exceed in this Line get the message
  
      *Message 44108 : The approved amount has covered some lines only in order XXXXX.
      *                Cannot allocate the rest.
      =gfModalGen("TRM44108B00000" , "DIALOG",&lcTmpOrdLn..Order)
  
    ENDIF  
    *-- Rest The Alocated Qty of this line
    REPLACE &lcTmpOrdLn..PoAlo1    WITH 0,;
            &lcTmpOrdLn..PoAlo2    WITH 0,;
            &lcTmpOrdLn..PoAlo3    WITH 0,;
            &lcTmpOrdLn..PoAlo4    WITH 0,;
            &lcTmpOrdLn..PoAlo5    WITH 0,;
            &lcTmpOrdLn..PoAlo6    WITH 0,;
            &lcTmpOrdLn..PoAlo7    WITH 0,;
            &lcTmpOrdLn..PoAlo8    WITH 0,;
            &lcTmpOrdLn..Tot_PoAlo WITH 0
    RETURN
  ENDIF
ENDIF  
*B804233,1 [End]

*IF nProcNo [The step number (for the rollback)] equal 0
IF nProcNo = 0
  
  *IF There is a record for this Style and Warehouse in the STYDYE file
  IF SEEK(Style + lcWareCode + SPACE(10) , 'STYDYE')
    SELECT STYDYE
    = RLOCK()
    REPLACE Alo1      WITH Alo1   + &lcTmpOrdLn..PoAlo1 ,;
            Alo2      WITH Alo2   + &lcTmpOrdLn..PoAlo2 ,;
            Alo3      WITH Alo3   + &lcTmpOrdLn..PoAlo3 ,;
            Alo4      WITH Alo4   + &lcTmpOrdLn..PoAlo4 ,;
            Alo5      WITH Alo5   + &lcTmpOrdLn..PoAlo5 ,;
            Alo6      WITH Alo6   + &lcTmpOrdLn..PoAlo6 ,;
            Alo7      WITH Alo7   + &lcTmpOrdLn..PoAlo7 ,;
            Alo8      WITH Alo8   + &lcTmpOrdLn..PoAlo8 ,;
            TotAlo    WITH TotAlo + &lcTmpOrdLn..Tot_PoAlo
    
    =gfAdd_Info()
    UNLOCK
  ENDIF    && End of IF
  SELECT (lcTmpOrdLn)
  REPLACE nProcNo   WITH 1
  =RLOCK()
  UNLOCK
ENDIF    && End of IF
  
*IF nProcNo [The step number (for the rollback)] equal 1
IF nProcNo = 1
  
  *IF There is a record for this Style and Warehouse and Dyelot in the
  * STYDYE file
  IF !EMPTY(DyeLot) .AND. SEEK(Style + lcWareCode + DyeLot , 'STYDYE')

    SELECT STYDYE
    = RLOCK()
    REPLACE Alo1   WITH Alo1   + &lcTmpOrdLn..PoAlo1 ,;
            Alo2   WITH Alo2   + &lcTmpOrdLn..PoAlo2 ,;
            Alo3   WITH Alo3   + &lcTmpOrdLn..PoAlo3 ,;
            Alo4   WITH Alo4   + &lcTmpOrdLn..PoAlo4 ,;
            Alo5   WITH Alo5   + &lcTmpOrdLn..PoAlo5 ,;
            Alo6   WITH Alo6   + &lcTmpOrdLn..PoAlo6 ,;
            Alo7   WITH Alo7   + &lcTmpOrdLn..PoAlo7 ,;
            Alo8   WITH Alo8   + &lcTmpOrdLn..PoAlo8 ,;
            TotAlo WITH TotAlo + &lcTmpOrdLn..Tot_PoAlo
    
    *E300989,1 Also we want to update order quantity for that dyelot record. [Begin]
    IF !&lcTmpOrdLn..Picked
       Replace Ord1 WITH Ord1   + &lcTmpOrdLn..Qty1  ,;
               Ord2 WITH Ord2   + &lcTmpOrdLn..Qty2  ,;
               Ord3 WITH Ord3   + &lcTmpOrdLn..Qty3  ,;
               Ord4 WITH Ord4   + &lcTmpOrdLn..Qty4  ,;
               Ord5 WITH Ord5   + &lcTmpOrdLn..Qty5  ,;
               Ord6 WITH Ord6   + &lcTmpOrdLn..Qty6  ,;
               Ord7 WITH Ord7   + &lcTmpOrdLn..Qty7  ,;
               Ord8 WITH Ord8   + &lcTmpOrdLn..Qty8  ,;
             TotOrd WITH TotOrd + &lcTmpOrdLn..TotQty
    ENDIF
  
    =gfAdd_Info()
    UNLOCK
  ENDIF    && End of IF
  SELECT (lcTmpOrdLn)
  REPLACE nProcNo   WITH 2
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

*IF nProcNo [The step number (for the rollback)] equal 2
IF nProcNo = 2
  
  *IF There is a record for this Style in the STYLE file
  IF !EOF('STYLE')
    SELECT STYLE
    = RLOCK()
    REPLACE Alo1   WITH Alo1   + &lcTmpOrdLn..PoAlo1 ,;
            Alo2   WITH Alo2   + &lcTmpOrdLn..PoAlo2 ,;
            Alo3   WITH Alo3   + &lcTmpOrdLn..PoAlo3 ,;
            Alo4   WITH Alo4   + &lcTmpOrdLn..PoAlo4 ,;
            Alo5   WITH Alo5   + &lcTmpOrdLn..PoAlo5 ,;
            Alo6   WITH Alo6   + &lcTmpOrdLn..PoAlo6 ,;
            Alo7   WITH Alo7   + &lcTmpOrdLn..PoAlo7 ,;
            Alo8   WITH Alo8   + &lcTmpOrdLn..PoAlo8 ,;
            TotAlo WITH TotAlo + &lcTmpOrdLn..Tot_PoAlo
  
    =gfAdd_Info()
    UNLOCK
  ENDIF    && End of IF
  SELECT (lcTmpOrdLn)
  REPLACE nProcNo   WITH 3
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

*IF nProcNo [The step number (for the rollback)] equal 3
IF nProcNo = 3
  
  *IF There is a record for this Order line in the ORDLINE file
  IF SEEK('O' + Order + STR(LineNo , 6) , 'ORDLINE')
    SELECT ORDLINE
    = RLOCK()

    *E300989,1 Add Dyelot field when update dyelots record.
    *REPLACE Pik1      WITH Pik1   + &lcTmpOrdLn..PoAlo1 ,;
    *        Pik2      WITH Pik2   + &lcTmpOrdLn..PoAlo2 ,;
    *        Pik3      WITH Pik3   + &lcTmpOrdLn..PoAlo3 ,;
    *        Pik4      WITH Pik4   + &lcTmpOrdLn..PoAlo4 ,;
    *        Pik5      WITH Pik5   + &lcTmpOrdLn..PoAlo5 ,;
    *        Pik6      WITH Pik6   + &lcTmpOrdLn..PoAlo6 ,;
    *        Pik7      WITH Pik7   + &lcTmpOrdLn..PoAlo7 ,;
    *        Pik8      WITH Pik8   + &lcTmpOrdLn..PoAlo8 ,;
    *        TotPik    WITH TotPik + &lcTmpOrdLn..Tot_PoAlo ,;
    *        PikDate   WITH gdSysDate ,;
    *        PikTkt    WITH '******' ,;
    *        Picked    WITH .T. ,;
    *        cWareCode WITH lcWareCode
    REPLACE Pik1      WITH Pik1   + &lcTmpOrdLn..PoAlo1 ,;
            Pik2      WITH Pik2   + &lcTmpOrdLn..PoAlo2 ,;
            Pik3      WITH Pik3   + &lcTmpOrdLn..PoAlo3 ,;
            Pik4      WITH Pik4   + &lcTmpOrdLn..PoAlo4 ,;
            Pik5      WITH Pik5   + &lcTmpOrdLn..PoAlo5 ,;
            Pik6      WITH Pik6   + &lcTmpOrdLn..PoAlo6 ,;
            Pik7      WITH Pik7   + &lcTmpOrdLn..PoAlo7 ,;
            Pik8      WITH Pik8   + &lcTmpOrdLn..PoAlo8 ,;
            TotPik    WITH TotPik + &lcTmpOrdLn..Tot_PoAlo ,;
            PikDate   WITH gdSysDate ,;
            PikTkt    WITH IIF(EMPTY(PikTkt),'******',PikTkt) ,;
            Picked    WITH .T. ,;
            cWareCode WITH lcWareCode,;
            DYELOT    WITH IIF(llHaveDye,&lcTmpOrdLn..Dyelot,DYELOT)
    *E126905,1 EIH 04/11/2005 [Begin] Case of auto allocation .
    REPLACE cAllocatBy      WITH  IIF( TotPik > 0 , 'A' , ' ')
    *E126905,1 EIH 04/11/2005 [End]  
    =gfAdd_Info()
    UNLOCK
    
  ENDIF    && End of IF
  SELECT (lcTmpOrdLn)
  REPLACE nProcNo   WITH 4
  =RLOCK()
  UNLOCK
ENDIF    && End of IF      

*C200084,1 Update allocate percentage in Ordhdr Master file. [Begin]
lnProcVal = 4
IF ASCAN(laEvntTrig,PADR("MASTALOC",10)) <> 0
  = gfDoTriger('ALAUTAL','MASTALOC')
ENDIF  

*IF nProcNo [The step number (for the rollback)] equal lnProcVal
*IF nProcNo = 4
IF nProcNo = lnProcVal
*C200084,1 Update allocate percentage in Ordhdr Master file. [End  ]
  lnSelRec = IIF(llSel , lnSelRec , lnSelRec + 1)
  lnSelAlo = IIF(llSel .AND. TotPik <> 0 , lnSelAlo , lnSelAlo + 1)
  lnAloRec = IIF(TotPik <> 0 , lnAloRec , lnAloRec + 1)
  
  =SEEK('O' + Order + STR(LineNo , 6) , 'ORDLINE')
  REPLACE Pik1      WITH Pik1   + PoAlo1 ,;
          Pik2      WITH Pik2   + PoAlo2 ,;
          Pik3      WITH Pik3   + PoAlo3 ,;
          Pik4      WITH Pik4   + PoAlo4 ,;
          Pik5      WITH Pik5   + PoAlo5 ,;
          Pik6      WITH Pik6   + PoAlo6 ,;
          Pik7      WITH Pik7   + PoAlo7 ,;
          Pik8      WITH Pik8   + PoAlo8 ,;
          TotPik    WITH TotPik + Tot_PoAlo ,;
          PikDate   WITH IIF(EOF('ORDLINE') , gdSysDate , ORDLINE.PikDate) ,;
          PikTkt    WITH IIF(EMPTY(PikTkt),'******',PikTkt) ,;
          Picked    WITH .T. ,;
          cWareCode WITH lcWareCode ,;
          nProcNo   WITH 99 ,;
          llSel     WITH .T.
    
  =RLOCK()
  UNLOCK

ENDIF    && End of IF
*-- end of lfAloQty.

*!*************************************************************
*! Name      : lfTmpAlo
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Function to Allocate the availabel quantity in the
*!             temp. Order lines file only.
*!*************************************************************
*! Called from : lfAllocate()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) [.T.] to Check if we will be able to Allocate
*!                        first and then Allocate ,
*!                        [.F.] to Allocate without checking 
*!                     2) The Warehouse to Allocate from
*!                     3) [.T.] If we are going to force the allocation
*!                        [.F.] Otherwise
*!                     4) Cut of units
*!                     5) Min. % to Allocate
*!*************************************************************
*! Return      : [.T.] If it was able to Allocate
*!               [.F.] Otherwise
*!*************************************************************
*
FUNCTION lfTmpAlo
PARAMETERS llParm , lcWareCode , llForce , lnCutUnt , lnMinPer

*C200084,1 Add new variables help in evely distribute allocation [Begin]
*PRIVATE llReturn , lnAvlFAlo 
PRIVATE llReturn , lnAvlFAlo , lnI , laOnHand , laWanted , laAllocPer
lnI = 0
DIMENSION laOnHand[8] , laWanted[8] , laAllocPer[8]
STORE 0 TO laOnHand , laWanted
laAllocPer = 1
*C200084,1 Add new variables help in evely distribute allocation [End  ]

llForce = IIF(TYPE('llForce') <> 'L' , .F. ,  llForce)        && Flag to know if we are going to force the Allocation
lnCutUnt = IIF(TYPE('lnCutUnt') <> 'N' , 0 , lnCutUnt)        && Variable to hold the Cut of units
lnMinPer = IIF(TYPE('lnMinPer') <> 'N' , 0 , lnMinPer)        && Variable to hold the Min. % to Allocate
llReturn = .T.            && Variable to hold the returned value
lnAvlFAlo = 0             && Variable to hold the availabel for Allocation quantity
lnTotReq  = 0             && Variable to hold the Required  for Allocation quantity

*IF There is a record for this Style and Warehouse and Dyelot in the STYDYE
*file
IF SEEK(Style + lcWareCode + DyeLot , 'STYDYE')
  
  *IF We are to Check if we will be able to Allocate first and we are not
  *going to force the Allocation
  IF llParm .AND. !llForce
    
    *E300989,1 Add Wips and ExcCuts and (Cuts and/or Qty) [Begin]
    *E300989,1  in allocated and available quantities. 
    
    IF llRpIncWip AND !llCalWip
      =lfEvalWip(Style,lcWareCode)
    ELSE
      IF !llRpIncWip
        laWip = 0
      ENDIF
    ENDIF  
    
    *E300989,1 Add Wips and ExcCuts and (Cuts and/or Qty) [Begin]
    *lnAvlFAlo = MAX(MIN(STYDYE.Stk1 - STYDYE.Alo1 - lnCutUnt , Qty1 - Pik1) , 0) +;
    *            MAX(MIN(STYDYE.Stk2 - STYDYE.Alo2 - lnCutUnt , Qty2 - Pik2) , 0) +;
    *            MAX(MIN(STYDYE.Stk3 - STYDYE.Alo3 - lnCutUnt , Qty3 - Pik3) , 0) +;
    *            MAX(MIN(STYDYE.Stk4 - STYDYE.Alo4 - lnCutUnt , Qty4 - Pik4) , 0) +;
    *            MAX(MIN(STYDYE.Stk5 - STYDYE.Alo5 - lnCutUnt , Qty5 - Pik5) , 0) +;
    *            MAX(MIN(STYDYE.Stk6 - STYDYE.Alo6 - lnCutUnt , Qty6 - Pik6) , 0) +;
    *            MAX(MIN(STYDYE.Stk7 - STYDYE.Alo7 - lnCutUnt , Qty7 - Pik7) , 0) +;
    *            MAX(MIN(STYDYE.Stk8 - STYDYE.Alo8 - lnCutUnt , Qty8 - Pik8) , 0)

    *C200084,1 Fresh produces custom proram Distribute allocation [Begin]
    *lnAvlFAlo = MAX(MIN(STYDYE.Stk1 + laWip[1] - STYDYE.Alo1 - lnCutUnt , MAX(IIF(lcRpScpMod $ 'KP',Cut1,Qty1) - Pik1 - ExcCut1 ,0)) , 0) +;
    *            MAX(MIN(STYDYE.Stk2 + laWip[2] - STYDYE.Alo2 - lnCutUnt , MAX(IIF(lcRpScpMod $ 'KP',Cut2,Qty2) - Pik2 - ExcCut2 ,0)) , 0) +;
    *            MAX(MIN(STYDYE.Stk3 + laWip[3] - STYDYE.Alo3 - lnCutUnt , MAX(IIF(lcRpScpMod $ 'KP',Cut3,Qty3) - Pik3 - ExcCut3 ,0)) , 0) +;
    *            MAX(MIN(STYDYE.Stk4 + laWip[4] - STYDYE.Alo4 - lnCutUnt , MAX(IIF(lcRpScpMod $ 'KP',Cut4,Qty4) - Pik4 - ExcCut4 ,0)) , 0) +;
    *            MAX(MIN(STYDYE.Stk5 + laWip[5] - STYDYE.Alo5 - lnCutUnt , MAX(IIF(lcRpScpMod $ 'KP',Cut5,Qty5) - Pik5 - ExcCut5 ,0)) , 0) +;
    *            MAX(MIN(STYDYE.Stk6 + laWip[6] - STYDYE.Alo6 - lnCutUnt , MAX(IIF(lcRpScpMod $ 'KP',Cut6,Qty6) - Pik6 - ExcCut6 ,0)) , 0) +;
    *            MAX(MIN(STYDYE.Stk7 + laWip[7] - STYDYE.Alo7 - lnCutUnt , MAX(IIF(lcRpScpMod $ 'KP',Cut7,Qty7) - Pik7 - ExcCut7 ,0)) , 0) +;
    *            MAX(MIN(STYDYE.Stk8 + laWip[8] - STYDYE.Alo8 - lnCutUnt , MAX(IIF(lcRpScpMod $ 'KP',Cut8,Qty8) - Pik8 - ExcCut8 ,0)) , 0)
    *E300989,1 Add Wips and ExcCuts and (Cuts and/or Qty) [End]
    
    IF ASCAN(laEvntTrig,PADR("ALLOCATE",10)) <> 0
      =gfDoTriger('ALAUTAL','ALLOCATE')
    ENDIF  

    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      laOnHand[lnI] = ROUND((STYDYE.Stk&lcI+laWip[lnI]-STYDYE.Alo&lcI-lnCutUnt) *;
                            laAllocPer[lnI],0)
      laWanted[lnI] = MAX(IIF(lcRpScpMod $ 'KP',Cut&lcI,Qty&lcI)-Pik&lcI-ExcCut&lcI,0)
      lnAvlFAlo = lnAvlFAlo + MAX(MIN(laOnHand[lnI],laWanted[lnI]),0)
    ENDFOR
    *C200084,1 Fresh produces custom proram Distribute allocation [End  ]
        
    *B602519,1 Allocate remain quantity only [Begin]
    *llReturn = IIF(lnAvlFAlo <> 0 .AND. (lnAvlFAlo/TotQty >= lnMinPer/100) ,;
    *               .T. , .F.)
    lnTotReq = IIF(lcRpScpMod $ 'KP',TotCut,TotQty) - TotPik - TotExcCut
    llReturn = lnAvlFAlo <> 0 .AND. (lnAvlFAlo/lnTotReq >= lnMinPer/100)
    *B602519,1 Allocate remain quantity only [End  ]
  
  ENDIF    && End of IF

  *IF We will be able to Allocate
  IF llReturn
    
    *IF We are not going to force the Allocation
    IF !llForce
      *E300989,1 Add Wips and ExcCuts and (Cuts and/or Qty) [Begin]
      *E300989,1  in allocated and available quantities. 
      *REPLACE PoAlo1    WITH MAX(MIN(STYDYE.Stk1 - STYDYE.Alo1 - lnCutUnt , Qty1 - Pik1) , 0) ,;
      *        PoAlo2    WITH MAX(MIN(STYDYE.Stk2 - STYDYE.Alo2 - lnCutUnt , Qty2 - Pik2) , 0) ,;
      *        PoAlo3    WITH MAX(MIN(STYDYE.Stk3 - STYDYE.Alo3 - lnCutUnt , Qty3 - Pik3) , 0) ,;
      *        PoAlo4    WITH MAX(MIN(STYDYE.Stk4 - STYDYE.Alo4 - lnCutUnt , Qty4 - Pik4) , 0) ,;
      *        PoAlo5    WITH MAX(MIN(STYDYE.Stk5 - STYDYE.Alo5 - lnCutUnt , Qty5 - Pik5) , 0) ,;
      *        PoAlo6    WITH MAX(MIN(STYDYE.Stk6 - STYDYE.Alo6 - lnCutUnt , Qty6 - Pik6) , 0) ,;
      *        PoAlo7    WITH MAX(MIN(STYDYE.Stk7 - STYDYE.Alo7 - lnCutUnt , Qty7 - Pik7) , 0) ,;
      *        PoAlo8    WITH MAX(MIN(STYDYE.Stk8 - STYDYE.Alo8 - lnCutUnt , Qty8 - Pik8) , 0) ,;
      *        Tot_PoAlo WITH PoAlo1 + PoAlo2 + PoAlo3 + PoAlo4 + PoAlo5 + PoAlo6 + PoAlo7 + PoAlo8
      
      *C200084,1 Fresh produces custom proram Distribute allocation [Begin]
      *REPLACE PoAlo1    WITH MAX(MIN(STYDYE.Stk1 + laWip[1] - STYDYE.Alo1 - lnCutUnt , MAX(IIF(lcRpScpMod $ 'KP',Cut1,Qty1) - Pik1 - ExcCut1 ,0)) , 0) ,;
      *        PoAlo2    WITH MAX(MIN(STYDYE.Stk2 + laWip[2] - STYDYE.Alo2 - lnCutUnt , MAX(IIF(lcRpScpMod $ 'KP',Cut2,Qty2) - Pik2 - ExcCut2 ,0)) , 0) ,;
      *        PoAlo3    WITH MAX(MIN(STYDYE.Stk3 + laWip[3] - STYDYE.Alo3 - lnCutUnt , MAX(IIF(lcRpScpMod $ 'KP',Cut3,Qty3) - Pik3 - ExcCut3 ,0)) , 0) ,;
      *        PoAlo4    WITH MAX(MIN(STYDYE.Stk4 + laWip[4] - STYDYE.Alo4 - lnCutUnt , MAX(IIF(lcRpScpMod $ 'KP',Cut4,Qty4) - Pik4 - ExcCut4 ,0)) , 0) ,;
      *        PoAlo5    WITH MAX(MIN(STYDYE.Stk5 + laWip[5] - STYDYE.Alo5 - lnCutUnt , MAX(IIF(lcRpScpMod $ 'KP',Cut5,Qty5) - Pik5 - ExcCut5 ,0)) , 0) ,;
      *        PoAlo6    WITH MAX(MIN(STYDYE.Stk6 + laWip[6] - STYDYE.Alo6 - lnCutUnt , MAX(IIF(lcRpScpMod $ 'KP',Cut6,Qty6) - Pik6 - ExcCut6 ,0)) , 0) ,;
      *        PoAlo7    WITH MAX(MIN(STYDYE.Stk7 + laWip[7] - STYDYE.Alo7 - lnCutUnt , MAX(IIF(lcRpScpMod $ 'KP',Cut7,Qty7) - Pik7 - ExcCut7 ,0)) , 0) ,;
      *        PoAlo8    WITH MAX(MIN(STYDYE.Stk8 + laWip[8] - STYDYE.Alo8 - lnCutUnt , MAX(IIF(lcRpScpMod $ 'KP',Cut8,Qty8) - Pik8 - ExcCut8 ,0)) , 0) ,;
      *        Tot_PoAlo WITH PoAlo1 + PoAlo2 + PoAlo3 + PoAlo4 + PoAlo5 + PoAlo6 + PoAlo7 + PoAlo8
      REPLACE PoAlo1    WITH MAX(MIN(laOnHand[1],laWanted[1]),0) ,;
              PoAlo2    WITH MAX(MIN(laOnHand[2],laWanted[2]),0) ,;
              PoAlo3    WITH MAX(MIN(laOnHand[3],laWanted[3]),0) ,;
              PoAlo4    WITH MAX(MIN(laOnHand[4],laWanted[4]),0) ,;
              PoAlo5    WITH MAX(MIN(laOnHand[5],laWanted[5]),0) ,;
              PoAlo6    WITH MAX(MIN(laOnHand[6],laWanted[6]),0) ,;
              PoAlo7    WITH MAX(MIN(laOnHand[7],laWanted[7]),0) ,;
              PoAlo8    WITH MAX(MIN(laOnHand[8],laWanted[8]),0) ,;
              Tot_PoAlo WITH PoAlo1 + PoAlo2 + PoAlo3 + PoAlo4 + PoAlo5 + PoAlo6 + PoAlo7 + PoAlo8
      *C200084,1 Fresh produces custom proram Distribute allocation [End  ]
      *E300989,1 Add Wips and ExcCuts and (Cuts and/or Qty) [End]
      
  
    ELSE    && Else

      *E300989,1 Add ExcCuts and (Cuts and/or Qty) in allocated quantity. [begin]
      *REPLACE PoAlo1    WITH Qty1 - Pik1 ,;
      *        PoAlo2    WITH Qty2 - Pik2 ,;
      *        PoAlo3    WITH Qty3 - Pik3 ,;
      *        PoAlo4    WITH Qty4 - Pik4 ,;
      *        PoAlo5    WITH Qty5 - Pik5 ,;
      *        PoAlo6    WITH Qty6 - Pik6 ,;
      *        PoAlo7    WITH Qty7 - Pik7 ,;
      *        PoAlo8    WITH Qty8 - Pik8 ,;
      *        Tot_PoAlo WITH PoAlo1 + PoAlo2 + PoAlo3 + PoAlo4 + PoAlo5 + PoAlo6 + PoAlo7 + PoAlo8

      REPLACE PoAlo1    WITH MAX(IIF(lcRpScpMod $ 'KP',Cut1,Qty1) - Pik1 - ExcCut1,0) ,;
              PoAlo2    WITH MAX(IIF(lcRpScpMod $ 'KP',Cut2,Qty2) - Pik2 - ExcCut2,0) ,;
              PoAlo3    WITH MAX(IIF(lcRpScpMod $ 'KP',Cut3,Qty3) - Pik3 - ExcCut3,0) ,;
              PoAlo4    WITH MAX(IIF(lcRpScpMod $ 'KP',Cut4,Qty4) - Pik4 - ExcCut4,0) ,;
              PoAlo5    WITH MAX(IIF(lcRpScpMod $ 'KP',Cut5,Qty5) - Pik5 - ExcCut5,0) ,;
              PoAlo6    WITH MAX(IIF(lcRpScpMod $ 'KP',Cut6,Qty6) - Pik6 - ExcCut6,0) ,;
              PoAlo7    WITH MAX(IIF(lcRpScpMod $ 'KP',Cut7,Qty7) - Pik7 - ExcCut7,0) ,;
              PoAlo8    WITH MAX(IIF(lcRpScpMod $ 'KP',Cut8,Qty8) - Pik8 - ExcCut8,0) ,;
              Tot_PoAlo WITH PoAlo1 + PoAlo2 + PoAlo3 + PoAlo4 + PoAlo5 + PoAlo6 + PoAlo7 + PoAlo8
      *E300989,1 Add ExcCuts and (Cuts and/or Qty) in allocated quantity. [end]

    ENDIF    && End of IF
    
    =RLOCK()
    UNLOCK
  
  ELSE  && Can not allocate this line (Return it again)

    IF llParm AND !llForce AND ASCAN(laEvntTrig,PADR("ADJLINE",10)) <> 0
      =gfDoTriger('ALAUTAL','ADJLINE')
    ENDIF  
  
  ENDIF    && End of IF
  RETURN llReturn
ELSE    && Else
  RETURN .F.
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfRelQty
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Function to update the needed files to Release the allocation
*!             from the current record of the temp. Order lines file
*!*************************************************************
*! Called from : lfRelScr()
*!*************************************************************
*! Calls       : gfAdd_Info()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfRelQty
PRIVATE lnCnT1 , lnCnT2 , lcOScale , lnElm , lcElm

lnCnT1 = 0                  && Variable to hold the number of sizes for the current Style
lnCnT2 = 0                  && Variable to hold the number of sizes for the Original Style
lcOScale = ''               && Variable to hold the Scale code of the Original Style

SELECT (lcTmpOrdLn)

*IF This line Style was substituted befor
IF !EMPTY(AltStyle)
  =SEEK(Style , 'STYLE')
  =SEEK( 'S'+STYLE.Scale ,'SCALE')
  lnCnT1 = SCALE.Cnt
  =SEEK(AltStyle , 'STYLE')
  =SEEK( 'S'+STYLE.Scale ,'SCALE')
  lnCnT2 = SCALE.Cnt
  lcOScale = SCALE.Scale
ENDIF    && End of IF

llLastLine = .F.

*E300989,1 Increase no. of updates by 2 to include release Piked line.[Begin]

*E300989,1 if current released line is a piked line
*-- Update PickLine File.
*IF nProcNo [The step number (for the rollback)] equal 0
IF nProcNo = 0

  IF !EMPTY(PIKTKT) AND (PIKTKT <> '******')
    
    *E301077,24 Open Pack Header file here first time run this block [Begin]
    *E301077,24 of code (Not from module object)
    IF !llOpnPack AND !USED('PACK_HDR')
      lcAlsNow  = SELECT(0)
      llOpnPack = gfOpenFile(gcDataDir+'PACK_HDR',gcDataDir+'PACK_HDR','SH')
      SELECT (lcAlsNow)
    ENDIF  
    *E301077,24 Open Pack Header file here first time run this block [End  ]

    *IF The Pick ticket has a packing list
    SET ORDER TO PACK_HDR IN PACK_HDR
    IF SEEK(PIKTKT,"PACK_HDR")
      *-- Message: 
	  *B602547,1  	
      *-- "picking tickets having packing lists were not released"
      *B602547,1
      *--                         < Ok >   
      =gfModalGen("TRM44004B00000","ALERT")
      RETURN
    ELSE    && Else
      
      *B603111,1 Delete line from temp. pick ticket File. [Begin]
      IF SEEK(Order+Store+cWareCode+PikTkt+STR(LineNo,6),lcTmpPkTk)
        SELECT (lcTmpPkTk)
        DELETE
        SELECT (lcTmpOrdLn)
      ENDIF
      *B603111,1 Delete line from temp. pick ticket File. [End  ]

      llLastLine = lfBldRel(PikTkt)
      
      *B603111,1 Add Lines to Pick line if all pick records was deleted. [Begin]
      IF llLastLine
        
        *SCATTER MEMVAR MEMO

        *E301077,24 Open Pick Line file here first time run this block [Begin]
        IF !llOpnPikLn AND !USED('PIKLINE')
          llOpnPikLn = gfOpenFile(gcDataDir+'PIKLINE','','SH')
        ENDIF  
        *E301077,24 Open Pick Line file here first time run this block [End  ]
       
        *B603111,1 Add all lines to pikline file. [Begin]
        SELECT (lcRelLine)
        SCAN
          SCATTER MEMVAR MEMO
          SELECT PIKLINE
          INSERT INTO ('PIKLINE') FROM MEMVAR
          =gfAdd_Info()
          SELECT (lcRelLine)
          DELETE
        ENDSCAN  
        *B603111,1 Add all lines to pikline file. [End  ]
        
      ENDIF  
      *B603111,1 Add Lines to Pick line if all pick records was deleted. [End  ]

    ENDIF    && End of IF
  ENDIF

  SELECT (lcTmpOrdLn)
  =RLOCK()
  REPLACE nProcNo   WITH 1
  UNLOCK

ENDIF

*-- Update PIKTKT file.
IF nProcNo = 1
 
  IF !EMPTY(PIKTKT) AND (PIKTKT <> '******') AND SEEK(PikTkt,'PIKTKT') AND PIKTKT.STATUS <> 'X'
    *B603111,1 Cancel piktkt if all pick records was deleted. [Begin]
    IF llLastLine
      SELECT PIKTKT
      REPLACE Status WITH "X"
      =gfAdd_Info()
      *C102424,1 (Begin) Add record for each piktkt in ALSHPINF for Ogio.
      IF ASCAN(laEvntTrig,PADR("UPDSHPA",10)) <> 0
        llRelease = .T.
        = gfDoTriger("ALAUTAL",PADR("UPDSHPA",10))
      ENDIF  
      *C102424,1 (Begin)
      *E129473,1 MHM 08/21/2005 Generate 940 EDI transaction for pick tickets directed to 3PL providers.
      IF 'AS' $ gcCmpModules AND SEEK(cWareCode,'WareHous') AND  SEEK('W'+WareHous.cThrdPLPr,'EDIACPRT') AND ;
           SEEK(EDIACPRT.cPartCode+'940','EDIPD') AND SEEK('940'+PADR(PikTkt,40)+'W'+WareHous.cThrdPLPr,'EDITRANS')
        SELECT 'EDITRANS'
        DELETE
      ENDIF
      *E129473,1 MHM 08/21/2005 (End)
      
    ENDIF  
    *B603111,1 Cancel piktkt if all pick records was deleted. [End  ]
  ENDIF  

  SELECT (lcTmpOrdLn)
  =RLOCK()
  REPLACE nProcNo   WITH 2
  UNLOCK

ENDIF

*E300989,1 Increase no. of updates by 2 to include release Piked line.[End]

*C037816,1 MHM 04/06/2004 Custom Release PikTkt from custom table for DL[Start]
IF ASCAN(laEvntTrig,PADR("ALRELAUT",10)) <> 0
  = gfDoTriger("ALAUTAL",PADR("ALRELAUT",10)) 
ENDIF 
*C037816,1 MHM [End]

*E300989,1 because of nProcNo increased by 2 change the following line.   
*IF nProcNo = 0   
IF nProcNo = 2
  *IF There is a record for this Style and Warehouse in the STYDYE file
  IF SEEK(Style + cWareCode + SPACE(10) , 'STYDYE')
    SELECT STYDYE
    = RLOCK()
    REPLACE Alo1   WITH Alo1   - &lcTmpOrdLn..Pik1 ,;
            Alo2   WITH Alo2   - &lcTmpOrdLn..Pik2 ,;
            Alo3   WITH Alo3   - &lcTmpOrdLn..Pik3 ,;
            Alo4   WITH Alo4   - &lcTmpOrdLn..Pik4 ,;
            Alo5   WITH Alo5   - &lcTmpOrdLn..Pik5 ,;
            Alo6   WITH Alo6   - &lcTmpOrdLn..Pik6 ,;
            Alo7   WITH Alo7   - &lcTmpOrdLn..Pik7 ,;
            Alo8   WITH Alo8   - &lcTmpOrdLn..Pik8 ,;
            TotAlo WITH TotAlo - &lcTmpOrdLn..TotPik
    
    =gfAdd_Info()
    UNLOCK
  ENDIF    && End of IF
  SELECT (lcTmpOrdLn)
  *E300989,1 because of nProcNo increased by 2 change the following line.   
  *REPLACE nProcNo   WITH 1
  REPLACE nProcNo   WITH 3
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

*E300989,1 because of nProcNo increased by 2 change the following line.   
*IF nProcNo [The step number (for the rollback)] equal 3
*IF nProcNo = 1
IF nProcNo = 3
  
  *IF There is a record for this Style and Warehouse and Dyelot in the
  * STYDYE file
  IF !EMPTY(DyeLot) .AND. SEEK(Style + cWareCode + DyeLot , 'STYDYE')
    SELECT STYDYE
    = RLOCK()
    *E300989,1 Release Order Quantities pegged to this line. [Begin]
    *REPLACE Alo1   WITH Alo1   - &lcTmpOrdLn..Pik1 ,;
    *        Alo2   WITH Alo2   - &lcTmpOrdLn..Pik2 ,;
    *        Alo3   WITH Alo3   - &lcTmpOrdLn..Pik3 ,;
    *        Alo4   WITH Alo4   - &lcTmpOrdLn..Pik4 ,;
    *        Alo5   WITH Alo5   - &lcTmpOrdLn..Pik5 ,;
    *        Alo6   WITH Alo6   - &lcTmpOrdLn..Pik6 ,;
    *        Alo7   WITH Alo7   - &lcTmpOrdLn..Pik7 ,;
    *        Alo8   WITH Alo8   - &lcTmpOrdLn..Pik8 ,;
    *        TotAlo WITH TotAlo - &lcTmpOrdLn..TotPik
    REPLACE Alo1   WITH Alo1    - &lcTmpOrdLn..Pik1   ,;
            Alo2   WITH Alo2    - &lcTmpOrdLn..Pik2   ,;
            Alo3   WITH Alo3    - &lcTmpOrdLn..Pik3   ,;
            Alo4   WITH Alo4    - &lcTmpOrdLn..Pik4   ,;
            Alo5   WITH Alo5    - &lcTmpOrdLn..Pik5   ,;
            Alo6   WITH Alo6    - &lcTmpOrdLn..Pik6   ,;
            Alo7   WITH Alo7    - &lcTmpOrdLn..Pik7   ,;
            Alo8   WITH Alo8    - &lcTmpOrdLn..Pik8   ,;
            TotAlo WITH TotAlo  - &lcTmpOrdLn..TotPik ,;
            Ord1   WITH Ord1    - &lcTmpOrdLn..Qty1   ,;
            Ord2   WITH Ord2    - &lcTmpOrdLn..Qty2   ,;
            Ord3   WITH Ord3    - &lcTmpOrdLn..Qty3   ,;
            Ord4   WITH Ord4    - &lcTmpOrdLn..Qty4   ,;
            Ord5   WITH Ord5    - &lcTmpOrdLn..Qty5   ,;
            Ord6   WITH Ord6    - &lcTmpOrdLn..Qty6   ,;
            Ord7   WITH Ord7    - &lcTmpOrdLn..Qty7   ,;
            Ord8   WITH Ord8    - &lcTmpOrdLn..Qty8   ,;
          TotOrd   WITH TotOrd  - &lcTmpOrdLn..TotQty
    *E300989,1 Release Order Quantities pegged to this line. [End]
  
    =gfAdd_Info()
    UNLOCK
  ENDIF    && End of IF
  SELECT (lcTmpOrdLn)
  *E300989,1 because of nProcNo increased by 2 change the following line.   
  *REPLACE nProcNo   WITH 2
  REPLACE nProcNo   WITH 4
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

*E300989,1 because of nProcNo increased by 2 change the following line.   
*IF nProcNo [The step number (for the rollback)] equal 2
*IF nProcNo = 2
IF nProcNo = 4
  
  *IF There is a record for this Style in the STYLE file
  IF SEEK(Style , 'STYLE')
    SELECT STYLE
    = RLOCK()
    REPLACE Alo1   WITH Alo1   - &lcTmpOrdLn..Pik1 ,;
            Alo2   WITH Alo2   - &lcTmpOrdLn..Pik2 ,;
            Alo3   WITH Alo3   - &lcTmpOrdLn..Pik3 ,;
            Alo4   WITH Alo4   - &lcTmpOrdLn..Pik4 ,;
            Alo5   WITH Alo5   - &lcTmpOrdLn..Pik5 ,;
            Alo6   WITH Alo6   - &lcTmpOrdLn..Pik6 ,;
            Alo7   WITH Alo7   - &lcTmpOrdLn..Pik7 ,;
            Alo8   WITH Alo8   - &lcTmpOrdLn..Pik8 ,;
            TotAlo WITH TotAlo - &lcTmpOrdLn..TotPik
  
    =gfAdd_Info()
    UNLOCK
  ENDIF    && End of IF
  SELECT (lcTmpOrdLn)

  *E300989,1 because of nProcNo increased by 2 change the following line.   
  *REPLACE nProcNo   WITH 3
  REPLACE nProcNo   WITH 5

  =RLOCK()
  UNLOCK
ENDIF    && End of IF

*E300989,1 because of nProcNo increased by 2 change the following line.   
*IF nProcNo [The step number (for the rollback)] equal 3
*IF nProcNo = 3
IF nProcNo = 5
  
  *IF There is a record for this Order line in the ORDLINE file
  IF SEEK('O' + Order + STR(LineNo , 6) , 'ORDLINE')
    SELECT ORDLINE
    = RLOCK()
    REPLACE Pik1    WITH 0 ,;
            Pik2    WITH 0 ,;
            Pik3    WITH 0 ,;
            Pik4    WITH 0 ,;
            Pik5    WITH 0 ,;
            Pik6    WITH 0 ,;
            Pik7    WITH 0 ,;
            Pik8    WITH 0 ,;
            TotPik  WITH 0 ,;
            PikDate WITH {} ,;
            PikTkt  WITH '' ,;
            Picked  WITH .F. ,;
            Dyelot  WITH &lcTmpOrdLn..cPeggedDye
    *E126905,1 EIH 04/11/2005 [Begin] Case of  Release.
    REPLACE cAllocatBy      WITH  ' '
    *E126905,1 EIH 04/11/2005 [End]  
    *B603464,1 KHM 02/21/2000 (Begin) Seeking in the OrdHdr in order to 
    *B603464,1                get the default warehouse.
    *B603464,1                THEN Check if the OrdLine warehouse
    *B603464,1                has been changed then restore the default 
    *B603464,1                one from the OrdHdr.
    =SEEK('O'+&lcTmpOrdLn..Order,'OrdHdr')
    IF OrdHdr.cWareCode<>cWareCode
      REPLACE cWareCode WITH OrdHdr.cWareCode
    ENDIF  
    *B603464,1 KHM 02/21/2000 (End)

    =gfAdd_Info()
    UNLOCK
  ENDIF    && End of IF
  SELECT (lcTmpOrdLn)

  *E300989,1 because of nProcNo increased by 2 change the following line.   
  *REPLACE nProcNo   WITH 4
  REPLACE nProcNo   WITH 6
  
  =RLOCK()
  UNLOCK
ENDIF    && End of IF      

*E300989,1 because of nProcNo increased by 2 change the following line.   
*IF nProcNo [The step number (for the rollback)] equal 4
*IF nProcNo = 4
IF nProcNo = 6
  
  *IF This line Style was substituted befor and the the number of sizes for
  *the current Style is greater than the number of sizes for the Original
  *Style
  IF !EMPTY(AltStyle) .AND. lnCnT1 > lnCnT2
    lnCanc = 0          && Varible to hold the canceled quantity
    lnCancAmt = 0       && Varible to hold the canceled amount

    *FOR Loop to get the  canceled quantity and amount
    FOR lnElm = lnCnT2 + 1 TO lnCnT1
      lcElm = STR(lnElm , 1)
      lnCanc = lnCanc + Qty&lcElm
    ENDFOR    && End of FOR Loop
    lnCancAmt = lnCanc * Price
    SELECT ORDHDR
    
    *IF There is a record for this Order line in the ORDHDR file
    IF SEEK('O' + &lcTmpOrdLn..Order)
      =RLOCK()
      REPLACE Open      WITH Open - lnCanc ,;
              OpenAmt   WITH OpenAmt - lnCancAmt ,;
              BookAmt   WITH BookAmt - lnCancAmt ,;
              Book      WITH Book - lnCanc
      
      =gfAdd_Info()
      UNLOCK
    ENDIF
  ENDIF    && End of IF
  
  *C200084,1 Update allocate percentage in Ordhdr Master file. [Begin]
  IF ASCAN(laEvntTrig,PADR("MASTRELE",10)) <> 0
    = gfDoTriger('ALAUTAL','MASTRELE')
  ENDIF  
  *C200084,1 Update allocate percentage in Ordhdr Master file. [Begin]
  
  SELECT (lcTmpOrdLn)

  *E300989,1 because of nProcNo increased by 2 change the following line.   
  *REPLACE nProcNo   WITH 5
  REPLACE nProcNo   WITH 7
  
  =RLOCK()
  UNLOCK
ENDIF    && End of IF      

*E300989,1 because of nProcNo increased by 2 change the following line.   
*IF nProcNo [The step number (for the rollback)] equal 5
*IF nProcNo = 5
IF nProcNo = 7

  IF !EMPTY(AltStyle)
    SELECT ORDLINE
    
    *IF There is a record for this Order line in the ORDLINE file
    IF SEEK('O' + Order + STR(LineNo , 6))    
      =RLOCK()
      REPLACE Qty2 WITH IIF(lnCnT2 < 2 , 0 , Qty2) ,;
              Qty3 WITH IIF(lnCnT2 < 3 , 0 , Qty3) ,;
              Qty4 WITH IIF(lnCnT2 < 4 , 0 , Qty4) ,;
              Qty5 WITH IIF(lnCnT2 < 5 , 0 , Qty5) ,;
              Qty6 WITH IIF(lnCnT2 < 6 , 0 , Qty6) ,;
              Qty7 WITH IIF(lnCnT2 < 7 , 0 , Qty7) ,;
              Qty8 WITH IIF(lnCnT2 < 8 , 0 , Qty8) ,;
            TotQty WITH Qty1 + Qty2 + Qty3 + Qty4 + Qty5 + Qty6 + Qty7 + Qty8 ,;
             Book2 WITH IIF(lnCnT2 < 2 , 0 , Book1) ,;
             Book3 WITH IIF(lnCnT2 < 3 , 0 , Book3) ,;
             Book4 WITH IIF(lnCnT2 < 4 , 0 , Book4) ,;
             Book5 WITH IIF(lnCnT2 < 5 , 0 , Book5) ,;
             Book6 WITH IIF(lnCnT2 < 6 , 0 , Book6) ,;
             Book7 WITH IIF(lnCnT2 < 7 , 0 , Book7) ,;
             Book8 WITH IIF(lnCnT2 < 8 , 0 , Book8) ,;
           TotBook WITH Book1 + Book2 + Book3 + Book4 + Book5 + Book6 + Book7 + Book8 ,;
             Style WITH &lcTmpOrdLn..AltStyle ,;
          AltStyle WITH SPACE(12) ,;
          Scale    WITH lcOScale
      
      =gfAdd_Info()
      UNLOCK
    ENDIF    && End of IF
  ENDIF    && End of IF
  SELECT (lcTmpOrdLn)

  *E300989,1 because of nProcNo increased by 2 change the following line.   
  *REPLACE nProcNo   WITH 6
  REPLACE nProcNo   WITH 8
  
  =RLOCK()
  UNLOCK
ENDIF    && End of IF      

*E300989,1 because of nProcNo increased by 2 change the following line.   
*IF nProcNo [The step number (for the rollback)] equal 6
*IF nProcNo = 6
IF nProcNo = 8
  
  *IF This line Style was substituted befor and There is a record for the
  *Current Style and Warehouse in the STYDYE file
  IF !EMPTY(AltStyle) .AND. SEEK(Style + cWareCode + SPACE(10) , 'STYDYE')
    SELECT STYDYE
    = RLOCK()
    REPLACE Ord1 WITH MAX((Ord1 - &lcTmpOrdLn..Qty1) , 0) ,;
            Ord2 WITH MAX((Ord2 - &lcTmpOrdLn..Qty2) , 0) ,;
            Ord3 WITH MAX((Ord3 - &lcTmpOrdLn..Qty3) , 0) ,;
            Ord4 WITH MAX((Ord4 - &lcTmpOrdLn..Qty4) , 0) ,;
            Ord5 WITH MAX((Ord5 - &lcTmpOrdLn..Qty5) , 0) ,;
            Ord6 WITH MAX((Ord6 - &lcTmpOrdLn..Qty6) , 0) ,;
            Ord7 WITH MAX((Ord7 - &lcTmpOrdLn..Qty7) , 0) ,;
            Ord8 WITH MAX((Ord8 - &lcTmpOrdLn..Qty8) , 0) ,;
          TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
    
    =gfAdd_Info()
    UNLOCK

  ENDIF    && End of IF
  SELECT (lcTmpOrdLn)

  *E300989,1 because of nProcNo increased by 2 change the following line.   
  *REPLACE nProcNo   WITH 7
  REPLACE nProcNo   WITH 9
  
  =RLOCK()
  UNLOCK
ENDIF    && End of IF      

*E300989,1 because of nProcNo increased by 2 change the following line.   
*IF nProcNo [The step number (for the rollback)] equal 7
*IF nProcNo = 7
IF nProcNo = 9
  
  *IF This line Style was substituted befor and There is a record for the
  *Original Style and Warehouse in the STYDYE file
  IF !EMPTY(AltStyle) .AND. SEEK(AltStyle + cWareCode + SPACE(10) , 'STYDYE')
    SELECT STYDYE
    = RLOCK()
    REPLACE Ord1 WITH (Ord1 + &lcTmpOrdLn..Qty1),;
            Ord2 WITH (Ord2 + &lcTmpOrdLn..Qty2),;
            Ord3 WITH (Ord3 + &lcTmpOrdLn..Qty3),;
            Ord4 WITH (Ord4 + &lcTmpOrdLn..Qty4),;
            Ord5 WITH (Ord5 + &lcTmpOrdLn..Qty5),;
            Ord6 WITH (Ord6 + &lcTmpOrdLn..Qty6),;
            Ord7 WITH (Ord7 + &lcTmpOrdLn..Qty7),;
            Ord8 WITH (Ord8 + &lcTmpOrdLn..Qty8),;
          TotOrd WITH (TotOrd + &lcTmpOrdLn..TotQty)
    
    =gfAdd_Info()
    UNLOCK

  ENDIF    && End of IF
  SELECT (lcTmpOrdLn)
 
  *E300989,1 because of nProcNo increased by 2 change the following line.   
  *REPLACE nProcNo   WITH 8
  REPLACE nProcNo   WITH 10
  
  =RLOCK()
  UNLOCK
ENDIF    && End of IF      

*E300989,1 because of nProcNo increased by 2 change the following line.   
*IF nProcNo [The step number (for the rollback)] equal 8
*IF nProcNo = 8
IF nProcNo = 10
  
  *IF This line Style was substituted befor and There is a record for the
  *Curren Style in the STYLE file
  IF !EMPTY(AltStyle) .AND. SEEK(Style , 'STYLE')
    SELECT STYLE
    = RLOCK()
    REPLACE Ord1 WITH MAX((Ord1 - &lcTmpOrdLn..Qty1) , 0) ,;
            Ord2 WITH MAX((Ord2 - &lcTmpOrdLn..Qty2) , 0) ,;
            Ord3 WITH MAX((Ord3 - &lcTmpOrdLn..Qty3) , 0) ,;
            Ord4 WITH MAX((Ord4 - &lcTmpOrdLn..Qty4) , 0) ,;
            Ord5 WITH MAX((Ord5 - &lcTmpOrdLn..Qty5) , 0) ,;
            Ord6 WITH MAX((Ord6 - &lcTmpOrdLn..Qty6) , 0) ,;
            Ord7 WITH MAX((Ord7 - &lcTmpOrdLn..Qty7) , 0) ,;
            Ord8 WITH MAX((Ord8 - &lcTmpOrdLn..Qty8) , 0) ,;
          TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
    
    =gfAdd_Info()
    UNLOCK

  ENDIF    && End of IF
  SELECT (lcTmpOrdLn)
  
  *E300989,1 because of nProcNo increased by 2 change the following line.   
  *REPLACE nProcNo   WITH 9
  REPLACE nProcNo   WITH 11
  
  =RLOCK()
  UNLOCK
ENDIF    && End of IF      

*E300989,1 because of nProcNo increased by 2 change the following line.   
*IF nProcNo [The step number (for the rollback)] equal 9
*IF nProcNo = 9
IF nProcNo = 11
  
  *IF This line Style was substituted befor and There is a record for the
  *Original Style in the STYLE file
  IF !EMPTY(AltStyle) .AND. SEEK(AltStyle , 'STYLE')
    SELECT STYLE
    = RLOCK()
    REPLACE Ord1 WITH (Ord1 + &lcTmpOrdLn..Qty1),;
            Ord2 WITH (Ord2 + &lcTmpOrdLn..Qty2),;
            Ord3 WITH (Ord3 + &lcTmpOrdLn..Qty3),;
            Ord4 WITH (Ord4 + &lcTmpOrdLn..Qty4),;
            Ord5 WITH (Ord5 + &lcTmpOrdLn..Qty5),;
            Ord6 WITH (Ord6 + &lcTmpOrdLn..Qty6),;
            Ord7 WITH (Ord7 + &lcTmpOrdLn..Qty7),;
            Ord8 WITH (Ord8 + &lcTmpOrdLn..Qty8),;
          TotOrd WITH (TotOrd + &lcTmpOrdLn..TotQty)
    
    =gfAdd_Info()
    UNLOCK

  ENDIF    && End of IF
  SELECT (lcTmpOrdLn)
  
  *E300989,1 because of nProcNo increased by 2 change the following line.   
  *REPLACE nProcNo   WITH 10
  REPLACE nProcNo   WITH 12
  
  =RLOCK()
  UNLOCK
ENDIF    && End of IF      

*E300989,1 because of nProcNo increased by 2 change the following line.   
*IF nProcNo [The step number (for the rollback)] equal 10
*IF nProcNo = 10
IF nProcNo = 12

  *E300989,1 Release Dyelot allocated to that line record.
  *E300989,1 because of nProcNo increased by 2 change the following line.   
  
  *REPLACE Pik1    WITH 0 ,;
  *        Pik2    WITH 0 ,;
  *        Pik3    WITH 0 ,;
  *        Pik4    WITH 0 ,;
  *        Pik5    WITH 0 ,;
  *        Pik6    WITH 0 ,;
  *        Pik7    WITH 0 ,;
  *        Pik8    WITH 0 ,;
  *        TotPik  WITH 0 ,;
  *        PikDate WITH {} ,;
  *        PikTkt  WITH '' ,;
  *        Picked  WITH .F. ,;
  *        nProcNo WITH 11
  REPLACE Pik1    WITH 0   ,;
          Pik2    WITH 0   ,;
          Pik3    WITH 0   ,;
          Pik4    WITH 0   ,;
          Pik5    WITH 0   ,;
          Pik6    WITH 0   ,;
          Pik7    WITH 0   ,;
          Pik8    WITH 0   ,;
          TotPik  WITH 0   ,;
          PikDate WITH {}  ,;
          PikTkt  WITH ''  ,;
          Picked  WITH .F. ,;
          Dyelot  WITH cPeggedDye  ,;
          nProcNo WITH 13

  *B603464,1 KHM 02/21/2000 (Begin) Seeking in the OrdHdr in order to be
  *B603464,1                able to get the default warehouse.
  *B603464,1                Then When releasing and the warehouse
  *B603464,1                was changed then get the default warehouse.
  =SEEK('O'+&lcTmpOrdLn..Order,'OrdHdr')
  IF OrdHdr.cWareCode<>cWareCode
    REPLACE cWareCode WITH OrdHdr.cWareCode
  ENDIF                            
  *B603464,1 KHM 02/21/2000 (End)
  =RLOCK()
  UNLOCK

ENDIF    && End of IF      

*E300989,1 because of nProcNo increased by 2 change the following line.   
*IF nProcNo [The step number (for the rollback)] equal 11
*IF nProcNo = 11
IF nProcNo = 13
  
  *IF This line Style was substituted befor
  IF !EMPTY(AltStyle)
    REPLACE Qty2 WITH IIF(lnCnT2 < 2 , 0 , Qty2) ,;
            Qty3 WITH IIF(lnCnT2 < 3 , 0 , Qty3) ,;
            Qty4 WITH IIF(lnCnT2 < 4 , 0 , Qty4) ,;
            Qty5 WITH IIF(lnCnT2 < 5 , 0 , Qty5) ,;
            Qty6 WITH IIF(lnCnT2 < 6 , 0 , Qty6) ,;
            Qty7 WITH IIF(lnCnT2 < 7 , 0 , Qty7) ,;
            Qty8 WITH IIF(lnCnT2 < 8 , 0 , Qty8) ,;
          TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
           Style WITH AltStyle ,;
        AltStyle WITH SPACE(12) ,;
        Scale    WITH lcOScale
      
  ENDIF    && End of IF
  REPLACE nProcNo   WITH 99
  =RLOCK()
  UNLOCK

ENDIF    && End of IF      

*B603464,1 KHM 02/21/2000 (Begin) Restoring the default warehouse in the
*B603464,1                in temporary file when releasing.
IF SEEK('O'+&lcTmpOrdLn..Order,'OrdHdr') AND ;
   OrdHdr.cWareCode <> &lcTmpOrdLn..cWareCode
   REPLACE &lcTmpOrdLn..cWareCode WITH OrdHdr.cWareCode
ENDIF   
*B603464,1 KHM 02/21/2000 (End)

*-- end of lfRelQty.

*!*************************************************************
*! Name      : lfShowGets
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Function to control the Enabling and Disabling of
*!             the Objects in the screen [lcAutAlCh3]
*!*************************************************************
*! Called from : lpShow() , lfvScope() , lfAloScr() , lfRelScr() ,
*!               lfwBrows() , lfvStyle() , lfvWareCode() , lfvDyelot() ,
*!               lfvSelect() , lfvSelAll() , lfvInvert() , lfBrowTrap()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  [.T.] IF We want to refresh the Enabling and
*!                             Disabling of all the Objects in the screen
*!                             [lcAutAlCh3]
*!                       [.F.] IF We want to refresh the Enabling and
*!                             Disabling of the Allocated Get fields only
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfShowGets

PARAMETERS llParm

*IF We want to refresh the Enabling and Disabling of all the Objects in the
*screen [lcAutAlCh3]
IF llParm
  lcCh3Stat = IIF(TotPik = 0 , 'ENABLE' , 'DISABLE')
  
  SHOW GET ibPickWare &lcCh3Stat
  SHOW GET lnWareCode &lcCh3Stat

  *IF The system use Dyelots
  IF llUseDyes
    lcDyelotSt = IIF(STYLE.cDye_Flg = 'N' , 'DISABLE' , lcCh3Stat)
    lcDyelot = IIF(STYLE.cDye_Flg = 'N' , SPACE(10) , lcDyelot)
    SHOW GET ibDyelot   &lcDyelotSt
    SHOW GET lcDyelot   &lcDyelotSt
  ELSE    && Else
    SHOW GET ibStyle    &lcCh3Stat
    SHOW GET lcStyle    &lcCh3Stat
  ENDIF    && End of IF
ENDIF    && End of IF

*IF we Are at the end of the STYDYE file or the current Style is Dyelot
*Yes and the Dyelot field is empty
IF EOF('STYDYE') .OR. (llUseDyes .AND. STYLE.cDye_Flg = 'Y' .AND. EMPTY(lcDyelot))
  laPikSt = 'DISABLE'
ELSE    && Else
  laPikSt[1] = IIF(SCALE.Cnt < 1 , 'DISABLE' , 'ENABLE')
  laPikSt[2] = IIF(SCALE.Cnt < 2 , 'DISABLE' , 'ENABLE')
  laPikSt[3] = IIF(SCALE.Cnt < 3 , 'DISABLE' , 'ENABLE')
  laPikSt[4] = IIF(SCALE.Cnt < 4 , 'DISABLE' , 'ENABLE')
  laPikSt[5] = IIF(SCALE.Cnt < 5 , 'DISABLE' , 'ENABLE')
  laPikSt[6] = IIF(SCALE.Cnt < 6 , 'DISABLE' , 'ENABLE')
  laPikSt[7] = IIF(SCALE.Cnt < 7 , 'DISABLE' , 'ENABLE')
  laPikSt[8] = IIF(SCALE.Cnt < 8 , 'DISABLE' , 'ENABLE')
ENDIF    && End of IF

SHOW GET laPik[1] &laPikSt[1]
SHOW GET laPik[2] &laPikSt[2]
SHOW GET laPik[3] &laPikSt[3]
SHOW GET laPik[4] &laPikSt[4]
SHOW GET laPik[5] &laPikSt[5]
SHOW GET laPik[6] &laPikSt[6]
SHOW GET laPik[7] &laPikSt[7]
SHOW GET laPik[8] &laPikSt[8]

*!*************************************************************
*! Name      : lfGScalCnt
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Function to Get the number of sizes for a specific Style
*!*************************************************************
*! Called from : lfvStyle()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  The Style we want to get the number of sizes for
*!*************************************************************
*! Return      : The number of sizes for the specific Style
*!*************************************************************
*
FUNCTION lfGScalCnt

PARAMETERS lcParm

PRIVATE lcScale

= SEEK(lcParm , 'STYLE')
lcScale = STYLE.Scale                 && Variable to hold the Scale code of the Style we want to get the number of sizes for
= SEEK('S' + lcScale , 'SCALE')

RETURN SCALE.Cnt

*!*************************************************************
*! Name      : lfEditLine
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Function to update the needed files when the current 
*!             record of the temp. Order lines file is Edited from
*!             the screen [lcAutAlCh3]
*!*************************************************************
*! Called from : lfAllocate()
*!*************************************************************
*! Calls       : gfAdd_Info()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfEditLine
PRIVATE lcAltStyle , lcOldWare , lnCnT1 , lnCnT2 , lcOScale ,;
        lnCanc , lcOldWare , lnElm , lcElm

lnCnT1 = 8                  && Variable to hold the number of sizes for the current Style
lnCnT2 = 8                  && Variable to hold the number of sizes for the Original Style
lcOScale = ''               && Variable to hold the Scale code of the Original Style

*IF nProcNo [The step number (for the rollback)] equal 0
IF nProcNo = 0
  REPLACE PoAlo1    WITH laPik[1] - Pik1 ,;
          PoAlo2    WITH laPik[2] - Pik2 ,;
          PoAlo3    WITH laPik[3] - Pik3 ,;
          PoAlo4    WITH laPik[4] - Pik4 ,;
          PoAlo5    WITH laPik[5] - Pik5 ,;
          PoAlo6    WITH laPik[6] - Pik6 ,;
          PoAlo7    WITH laPik[7] - Pik7 ,;
          PoAlo8    WITH laPik[8] - Pik8 ,;
          Tot_PoAlo WITH lnTotPik - TotPik ,;
          Book1     WITH laQty[1] ,;
          Book2     WITH laQty[2] ,;
          Book3     WITH laQty[3] ,;
          Book4     WITH laQty[4] ,;
          Book5     WITH laQty[5] ,;
          Book6     WITH laQty[6] ,;
          Book7     WITH laQty[7] ,;
          Book8     WITH laQty[8] ,;
          TotBook   WITH lnTotQty ,;
          lLok_Stat WITH EMPTY(AltStyle) ,;
          AltStyle  WITH IIF(lcStyle = Style , AltStyle , Style) ,;
          Scale     WITH IIF(lcStyle = Style , Scale , lcStyScale) ,;
          Style     WITH lcStyle ,;
          DyeLot    WITH lcDyeLot ,;
          Season    WITH cWareCode ,;
          cWareCode WITH lcWareCode ,;
          nProcNo   WITH 1
  
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

lcAltStyle = IIF(EMPTY(AltStyle) .OR. !lLok_Stat , Style , AltStyle)    && Variable to hold the original Style
lcOldWare = Season          && Variable to hold the old Warehouse

*IF This line Style was substituted befor and the line is to be released
IF !lLok_Stat .AND. Tot_PoAlo + TotPik = 0
  =SEEK(Style , 'STYLE')
  =SEEK( 'S'+STYLE.Scale ,'SCALE')
  lnCnT1 = SCALE.Cnt
  =SEEK(AltStyle , 'STYLE')
  =SEEK( 'S'+STYLE.Scale ,'SCALE')
  lnCnT2 = SCALE.Cnt
  lcOScale = SCALE.Scale
ENDIF    && End of IF

*IF nProcNo [The step number (for the rollback)] equal 1
IF nProcNo = 1
  
  *IF There is a record for the current Style and Warehouse in the STYDYE
  *file
  IF SEEK(Style + cWareCode + SPACE(10) , 'STYDYE')
    SELECT STYDYE
    = RLOCK()
    REPLACE Alo1   WITH Alo1   + &lcTmpOrdLn..PoAlo1 ,;
            Alo2   WITH Alo2   + &lcTmpOrdLn..PoAlo2 ,;
            Alo3   WITH Alo3   + &lcTmpOrdLn..PoAlo3 ,;
            Alo4   WITH Alo4   + &lcTmpOrdLn..PoAlo4 ,;
            Alo5   WITH Alo5   + &lcTmpOrdLn..PoAlo5 ,;
            Alo6   WITH Alo6   + &lcTmpOrdLn..PoAlo6 ,;
            Alo7   WITH Alo7   + &lcTmpOrdLn..PoAlo7 ,;
            Alo8   WITH Alo8   + &lcTmpOrdLn..PoAlo8 ,;
            TotAlo WITH TotAlo + &lcTmpOrdLn..Tot_PoAlo
    
    =gfAdd_Info()
    UNLOCK
  ENDIF    && End of IF
  SELECT (lcTmpOrdLn)
  REPLACE nProcNo   WITH 2
  =RLOCK()
  UNLOCK
ENDIF    && End of IF
  
*IF nProcNo [The step number (for the rollback)] equal 2
IF nProcNo = 2
  
  *IF There is a record for the cuurent Style and Warehouse and Dyelot in
  *the STYDYE file
  IF !EMPTY(DyeLot) .AND. SEEK(Style + cWareCode + DyeLot , 'STYDYE')
    SELECT STYDYE
    = RLOCK()
    REPLACE Alo1   WITH Alo1   + &lcTmpOrdLn..PoAlo1 ,;
            Alo2   WITH Alo2   + &lcTmpOrdLn..PoAlo2 ,;
            Alo3   WITH Alo3   + &lcTmpOrdLn..PoAlo3 ,;
            Alo4   WITH Alo4   + &lcTmpOrdLn..PoAlo4 ,;
            Alo5   WITH Alo5   + &lcTmpOrdLn..PoAlo5 ,;
            Alo6   WITH Alo6   + &lcTmpOrdLn..PoAlo6 ,;
            Alo7   WITH Alo7   + &lcTmpOrdLn..PoAlo7 ,;
            Alo8   WITH Alo8   + &lcTmpOrdLn..PoAlo8 ,;
            TotAlo WITH TotAlo + &lcTmpOrdLn..Tot_PoAlo

    *E300989,1 if editing line release allocation from it. [begin]
    
    IF &lcTmpOrdLn..Picked 
      IF TotAlo = 0
        Replace Ord1 WITH Ord1   - &lcTmpOrdLn..Qty1  ,;
                Ord2 WITH Ord2   - &lcTmpOrdLn..Qty2  ,;
                Ord3 WITH Ord3   - &lcTmpOrdLn..Qty3  ,;
                Ord4 WITH Ord4   - &lcTmpOrdLn..Qty4  ,;
                Ord5 WITH Ord5   - &lcTmpOrdLn..Qty5  ,;
                Ord6 WITH Ord6   - &lcTmpOrdLn..Qty6  ,;
                Ord7 WITH Ord7   - &lcTmpOrdLn..Qty7  ,;
                Ord8 WITH Ord8   - &lcTmpOrdLn..Qty8  ,;
              TotOrd WITH TotOrd - &lcTmpOrdLn..TotQty
      ENDIF
    ELSE
      Replace Ord1 WITH Ord1   + &lcTmpOrdLn..Qty1  ,;
              Ord2 WITH Ord2   + &lcTmpOrdLn..Qty2  ,;
              Ord3 WITH Ord3   + &lcTmpOrdLn..Qty3  ,;
              Ord4 WITH Ord4   + &lcTmpOrdLn..Qty4  ,;
              Ord5 WITH Ord5   + &lcTmpOrdLn..Qty5  ,;
              Ord6 WITH Ord6   + &lcTmpOrdLn..Qty6  ,;
              Ord7 WITH Ord7   + &lcTmpOrdLn..Qty7  ,;
              Ord8 WITH Ord8   + &lcTmpOrdLn..Qty8  ,;
            TotOrd WITH TotOrd + &lcTmpOrdLn..TotQty
    ENDIF

    *E300989,1 if editing line release allocation from it. [end]
  
    =gfAdd_Info()
    UNLOCK
  ENDIF    && End of IF
  SELECT (lcTmpOrdLn)
  REPLACE nProcNo   WITH 3
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

*IF nProcNo [The step number (for the rollback)] equal 3
IF nProcNo = 3
  
  *IF There is a record for the current Style in the STYLE file
  IF SEEK(Style , 'STYLE')
    SELECT STYLE
    = RLOCK()
    REPLACE Alo1   WITH Alo1   + &lcTmpOrdLn..PoAlo1 ,;
            Alo2   WITH Alo2   + &lcTmpOrdLn..PoAlo2 ,;
            Alo3   WITH Alo3   + &lcTmpOrdLn..PoAlo3 ,;
            Alo4   WITH Alo4   + &lcTmpOrdLn..PoAlo4 ,;
            Alo5   WITH Alo5   + &lcTmpOrdLn..PoAlo5 ,;
            Alo6   WITH Alo6   + &lcTmpOrdLn..PoAlo6 ,;
            Alo7   WITH Alo7   + &lcTmpOrdLn..PoAlo7 ,;
            Alo8   WITH Alo8   + &lcTmpOrdLn..PoAlo8 ,;
            TotAlo WITH TotAlo + &lcTmpOrdLn..Tot_PoAlo
  
    =gfAdd_Info()
    UNLOCK
  ENDIF    && End of IF
  SELECT (lcTmpOrdLn)
  REPLACE nProcNo   WITH 4
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

*IF nProcNo [The step number (for the rollback)] equal 4
IF nProcNo = 4
  
  *IF There is a record for the old Style and Warehouse in the STYDYE
  *file
  IF SEEK(lcAltStyle + lcOldWare + SPACE(10) , 'STYDYE')
    SELECT STYDYE
    = RLOCK()
    REPLACE Ord1 WITH MAX((Ord1 - &lcTmpOrdLn..Qty1) , 0) ,;
            Ord2 WITH MAX((Ord2 - &lcTmpOrdLn..Qty2) , 0) ,;
            Ord3 WITH MAX((Ord3 - &lcTmpOrdLn..Qty3) , 0) ,;
            Ord4 WITH MAX((Ord4 - &lcTmpOrdLn..Qty4) , 0) ,;
            Ord5 WITH MAX((Ord5 - &lcTmpOrdLn..Qty5) , 0) ,;
            Ord6 WITH MAX((Ord6 - &lcTmpOrdLn..Qty6) , 0) ,;
            Ord7 WITH MAX((Ord7 - &lcTmpOrdLn..Qty7) , 0) ,;
            Ord8 WITH MAX((Ord8 - &lcTmpOrdLn..Qty8) , 0) ,;
          TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
    
    =gfAdd_Info()
    UNLOCK

  ENDIF    && End of IF
  SELECT (lcTmpOrdLn)
  REPLACE nProcNo   WITH 5
  =RLOCK()
  UNLOCK
ENDIF    && End of IF      

*IF nProcNo [The step number (for the rollback)] equal 5
IF nProcNo = 5
  
  *IF There is a record for the current Style and Warehouse in the STYDYE
  *file
  IF SEEK(Style + cWareCode + SPACE(10) , 'STYDYE')
    SELECT STYDYE
    = RLOCK()
    REPLACE Ord1 WITH (Ord1 + &lcTmpOrdLn..Book1),;
            Ord2 WITH (Ord2 + &lcTmpOrdLn..Book2),;
            Ord3 WITH (Ord3 + &lcTmpOrdLn..Book3),;
            Ord4 WITH (Ord4 + &lcTmpOrdLn..Book4),;
            Ord5 WITH (Ord5 + &lcTmpOrdLn..Book5),;
            Ord6 WITH (Ord6 + &lcTmpOrdLn..Book6),;
            Ord7 WITH (Ord7 + &lcTmpOrdLn..Book7),;
            Ord8 WITH (Ord8 + &lcTmpOrdLn..Book8),;
          TotOrd WITH (TotOrd + &lcTmpOrdLn..TotBook)
    
    =gfAdd_Info()
    UNLOCK

  ENDIF    && End of IF
  SELECT (lcTmpOrdLn)
  REPLACE nProcNo   WITH 6
  =RLOCK()
  UNLOCK
ENDIF    && End of IF      

*IF nProcNo [The step number (for the rollback)] equal 6
IF nProcNo = 6
  
  *IF There is a record for the old Style in the STYLE file
  IF SEEK(lcAltStyle , 'STYLE')
    SELECT STYLE
    = RLOCK()
    REPLACE Ord1 WITH MAX((Ord1 - &lcTmpOrdLn..Qty1) , 0) ,;
            Ord2 WITH MAX((Ord2 - &lcTmpOrdLn..Qty2) , 0) ,;
            Ord3 WITH MAX((Ord3 - &lcTmpOrdLn..Qty3) , 0) ,;
            Ord4 WITH MAX((Ord4 - &lcTmpOrdLn..Qty4) , 0) ,;
            Ord5 WITH MAX((Ord5 - &lcTmpOrdLn..Qty5) , 0) ,;
            Ord6 WITH MAX((Ord6 - &lcTmpOrdLn..Qty6) , 0) ,;
            Ord7 WITH MAX((Ord7 - &lcTmpOrdLn..Qty7) , 0) ,;
            Ord8 WITH MAX((Ord8 - &lcTmpOrdLn..Qty8) , 0) ,;
          TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
    
    =gfAdd_Info()
    UNLOCK

  ENDIF    && End of IF
  SELECT (lcTmpOrdLn)
  REPLACE nProcNo   WITH 7
  =RLOCK()
  UNLOCK
ENDIF    && End of IF      

*IF nProcNo [The step number (for the rollback)] equal 7
IF nProcNo = 7
  
  *IF There is a record for the current Style in the STYLE file
  IF SEEK(Style , 'STYLE')
    SELECT STYLE
    = RLOCK()
    REPLACE Ord1 WITH (Ord1 + &lcTmpOrdLn..Book1),;
            Ord2 WITH (Ord2 + &lcTmpOrdLn..Book2),;
            Ord3 WITH (Ord3 + &lcTmpOrdLn..Book3),;
            Ord4 WITH (Ord4 + &lcTmpOrdLn..Book4),;
            Ord5 WITH (Ord5 + &lcTmpOrdLn..Book5),;
            Ord6 WITH (Ord6 + &lcTmpOrdLn..Book6),;
            Ord7 WITH (Ord7 + &lcTmpOrdLn..Book7),;
            Ord8 WITH (Ord8 + &lcTmpOrdLn..Book8),;
          TotOrd WITH (TotOrd + &lcTmpOrdLn..TotBook)
    
    =gfAdd_Info()
    UNLOCK

  ENDIF    && End of IF
  SELECT (lcTmpOrdLn)
  REPLACE nProcNo   WITH 8
  =RLOCK()
  UNLOCK
ENDIF    && End of IF      

*IF nProcNo [The step number (for the rollback)] equal 8
IF nProcNo = 8
  
  *IF We will need to update the Order Header
  IF lnCnT1 > lnCnT2 .OR. TotBook <> TotQty
    lnCanc = 0          && Varible to hold the canceled quantity

    *FOR Loop to get the  canceled quantity and amount
    IF lnCnT1 > lnCnT2
      FOR lnElm = lnCnT2 + 1 TO lnCnT1
        lcElm = STR(lnElm , 1)
        lnCanc = lnCanc + Book&lcElm
      ENDFOR    && End of FOR Loop
    ENDIF    && End of IF
    SELECT ORDHDR
    
    *IF There is a record for this Order line in the ORDHDR file
    IF SEEK('O' + &lcTmpOrdLn..Order)
      =RLOCK()
      REPLACE Open      WITH Open + &lcTmpOrdLn..TotBook - &lcTmpOrdLn..TotQty - lnCanc ,;
              OpenAmt   WITH OpenAmt + ((&lcTmpOrdLn..TotBook - &lcTmpOrdLn..TotQty - lnCanc) * &lcTmpOrdLn..Price) ,;
              BookAmt   WITH BookAmt + ((&lcTmpOrdLn..TotBook - &lcTmpOrdLn..TotQty - lnCanc) * &lcTmpOrdLn..Price) ,;
              Book      WITH Book + &lcTmpOrdLn..TotBook - &lcTmpOrdLn..TotQty - lnCanc
      
      =gfAdd_Info()
      UNLOCK
    ENDIF
  
  ENDIF    && End of IF

  *C200084,1 Update allocate percentage in Ordhdr Master file. [Begin]
  IF ASCAN(laEvntTrig,PADR("MANUAL",10)) <> 0
    = gfDoTriger('ALAUTAL','MANUAL')
  ENDIF  
  *C200084,1 Update allocate percentage in Ordhdr Master file. [Begin]

  SELECT (lcTmpOrdLn)
  REPLACE nProcNo   WITH 9
  =RLOCK()
  UNLOCK
ENDIF    && End of IF      

*IF nProcNo [The step number (for the rollback)] equal 9
IF nProcNo = 9
  
  *IF There is a record for this Order line in the ORDLINE file
  IF SEEK('O' + Order + STR(LineNo , 6) , 'ORDLINE')
    SELECT ORDLINE
    = RLOCK()
    REPLACE Pik1     WITH Pik1 + &lcTmpOrdLn..PoAlo1 ,;
            Pik2     WITH Pik2 + &lcTmpOrdLn..PoAlo2 ,;
            Pik3     WITH Pik3 + &lcTmpOrdLn..PoAlo3 ,;
            Pik4     WITH Pik4 + &lcTmpOrdLn..PoAlo4 ,;
            Pik5     WITH Pik5 + &lcTmpOrdLn..PoAlo5 ,;
            Pik6     WITH Pik6 + &lcTmpOrdLn..PoAlo6 ,;
            Pik7     WITH Pik7 + &lcTmpOrdLn..PoAlo7 ,;
            Pik8     WITH Pik8 + &lcTmpOrdLn..PoAlo8 ,;
            TotPik   WITH TotPik + &lcTmpOrdLn..Tot_PoAlo ,;
            PikDate  WITH IIF(TotPik = 0 , {} , gdSysDate) ,;
            PikTkt   WITH IIF(TotPik = 0 , '' ,;
                          IIF(EMPTY(PikTkt),'******',PikTkt)) ,;
            Picked   WITH IIF(TotPik = 0 , .F. , .T.)
    
    =gfAdd_Info()
    UNLOCK
  ENDIF    && End of IF
  SELECT (lcTmpOrdLn)
  REPLACE nProcNo   WITH 10
  =RLOCK()
  UNLOCK
ENDIF    && End of IF      

*IF nProcNo [The step number (for the rollback)] equal 10
IF nProcNo = 10
  
  *IF There is a record for this Order line in the ORDLINE file
  IF SEEK('O' + Order + STR(LineNo , 6) , 'ORDLINE')
    SELECT ORDLINE
    = RLOCK()
    REPLACE Book1  WITH Book1 + &lcTmpOrdLn..Book1 - &lcTmpOrdLn..Qty1 ,;
            Book2  WITH IIF(lnCnT2 < 2 , 0 , Book2 + &lcTmpOrdLn..Book2 - &lcTmpOrdLn..Qty2) ,;
            Book3  WITH IIF(lnCnT2 < 3 , 0 , Book3 + &lcTmpOrdLn..Book3 - &lcTmpOrdLn..Qty3) ,;
            Book4  WITH IIF(lnCnT2 < 4 , 0 , Book4 + &lcTmpOrdLn..Book4 - &lcTmpOrdLn..Qty4) ,;
            Book5  WITH IIF(lnCnT2 < 5 , 0 , Book5 + &lcTmpOrdLn..Book5 - &lcTmpOrdLn..Qty5) ,;
            Book6  WITH IIF(lnCnT2 < 6 , 0 , Book6 + &lcTmpOrdLn..Book6 - &lcTmpOrdLn..Qty6) ,;
            Book7  WITH IIF(lnCnT2 < 7 , 0 , Book7 + &lcTmpOrdLn..Book7 - &lcTmpOrdLn..Qty7) ,;
            Book8  WITH IIF(lnCnT2 < 8 , 0 , Book8 + &lcTmpOrdLn..Book8 - &lcTmpOrdLn..Qty8) ,;
            TotBook WITH Book1 + Book2 + Book3 + Book4 + Book5 + Book6 + Book7 + Book8
    
    =gfAdd_Info()
    UNLOCK
  ENDIF    && End of IF
  SELECT (lcTmpOrdLn)
  REPLACE nProcNo   WITH 11
  =RLOCK()
  UNLOCK
ENDIF    && End of IF      


*IF nProcNo [The step number (for the rollback)] equal 11
IF nProcNo = 11
  
  *IF There is a record for this Order line in the ORDLINE file
  IF SEEK('O' + Order + STR(LineNo , 6) , 'ORDLINE')
    SELECT ORDLINE
    = RLOCK()

    REPLACE QTY1      WITH &lcTmpOrdLn..Book1 ,;
            Qty2      WITH IIF(lnCnT2 < 2 , 0 , &lcTmpOrdLn..Book2) ,;
            Qty3      WITH IIF(lnCnT2 < 3 , 0 , &lcTmpOrdLn..Book3) ,;
            Qty4      WITH IIF(lnCnT2 < 4 , 0 , &lcTmpOrdLn..Book4) ,;
            Qty5      WITH IIF(lnCnT2 < 5 , 0 , &lcTmpOrdLn..Book5) ,;
            Qty6      WITH IIF(lnCnT2 < 6 , 0 , &lcTmpOrdLn..Book6) ,;
            Qty7      WITH IIF(lnCnT2 < 7 , 0 , &lcTmpOrdLn..Book7) ,;
            Qty8      WITH IIF(lnCnT2 < 8 , 0 , &lcTmpOrdLn..Book8) ,;
            TotQty    WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
            Style     WITH IIF(TotPik = 0 .AND. !&lcTmpOrdLn..lLok_Stat , &lcTmpOrdLn..AltStyle , &lcTmpOrdLn..Style),;
            AltStyle  WITH IIF(TotPik = 0 , SPACE(12) , &lcTmpOrdLn..AltStyle) ,;
            DyeLot    WITH &lcTmpOrdLn..DyeLot ,;
            cWareCode WITH &lcTmpOrdLn..cWareCode ,;
            Scale     WITH IIF(TotPik = 0 .AND. !lLok_Stat , lcOScale , &lcTmpOrdLn..Scale)
    *E126905,1 EIH 04/11/2005 [Begin] Case of  Modified in allocate QTY.
    REPLACE cAllocatBy      WITH  IIF( TotPik > 0 , 'A' , ' ')
    *E126905,1 EIH 04/11/2005 [End]  
    =gfAdd_Info()
    UNLOCK
  ENDIF    && End of IF
  SELECT (lcTmpOrdLn)
  REPLACE nProcNo   WITH 12
  =RLOCK()
  UNLOCK
ENDIF    && End of IF      

*IF nProcNo [The step number (for the rollback)] equal 12
IF nProcNo = 12
  lnSelAlo = IIF(TotPik = 0 .AND. Tot_PoAlo > 0 , lnSelAlo + 1 ,;
                 IIF(TotPik > 0 .AND. TotPik + Tot_PoAlo = 0 ,;
                 lnSelAlo - 1 , lnSelAlo))
  
  lnAloRec = IIF(TotPik = 0 .AND. Tot_PoAlo > 0 , lnAloRec + 1 ,;
                 IIF(TotPik > 0 .AND. TotPik + Tot_PoAlo = 0 ,;
                 lnAloRec - 1 , lnAloRec))
  
  REPLACE Pik1     WITH Pik1 + PoAlo1 ,;
          Pik2     WITH Pik2 + PoAlo2 ,;
          Pik3     WITH Pik3 + PoAlo3 ,;
          Pik4     WITH Pik4 + PoAlo4 ,;
          Pik5     WITH Pik5 + PoAlo5 ,;
          Pik6     WITH Pik6 + PoAlo6 ,;
          Pik7     WITH Pik7 + PoAlo7 ,;
          Pik8     WITH Pik8 + PoAlo8 ,;
          TotPik   WITH TotPik + Tot_PoAlo ,;
          PikDate  WITH IIF(TotPik = 0 , {} , gdSysDate) ,;
          PikTkt   WITH IIF(TotPik = 0 , '' ,;
                          IIF(EMPTY(PikTkt),'******',PikTkt)) ,;
          Picked   WITH IIF(TotPik = 0 , .F. , .T.) ,;
          QTY1     WITH Book1 ,;
          Qty2     WITH IIF(lnCnT2 < 2 , 0 , Book2) ,;
          Qty3     WITH IIF(lnCnT2 < 3 , 0 , Book3) ,;
          Qty4     WITH IIF(lnCnT2 < 4 , 0 , Book4) ,;
          Qty5     WITH IIF(lnCnT2 < 5 , 0 , Book5) ,;
          Qty6     WITH IIF(lnCnT2 < 6 , 0 , Book6) ,;
          Qty7     WITH IIF(lnCnT2 < 7 , 0 , Book7) ,;
          Qty8     WITH IIF(lnCnT2 < 8 , 0 , Book8) ,;
          TotQty   WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
          Style    WITH IIF(TotPik = 0 .AND. !lLok_Stat , AltStyle , Style),;
          AltStyle WITH IIF(TotPik = 0 , SPACE(12) , AltStyle) ,;
          Scale    WITH IIF(TotPik = 0 .AND. !lLok_Stat , lcOScale , Scale) ,;
          nProcNo  WITH 99
  
  =RLOCK()
  UNLOCK
  
  lcStyle = Style
  lcWareCode = cWareCode
  lnWareCode = ASCAN(laWareCode , lcWareCode)
  lcDyelot = IIF(STYLE.cDye_Flg = 'N' , SPACE(10) , Dyelot)
ENDIF    && End of IF      ORDHDR
*-- end of lfEditLine.

*!*************************************************************
*! Name      : lfGenPikTk
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Function to update the needed files to Generate a Pick
*!             ticket for the current record of the temp. Order lines file
*!*************************************************************
*! Called from : lfGenScr()
*!*************************************************************
*! Calls       : gfAdd_Info()
*!*************************************************************
*! Passed Parameters : Pick ticket number
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfGenPikTk
PARAMETERS lcParm
*IF nProcNo [The step number (for the rollback)] equal 0
IF nProcNo = 0
  REPLACE PikTkt  WITH lcParm ,;
          nProcNo WITH 1
  
  =RLOCK()
  UNLOCK
ENDIF    && End of IF
*E129473,1 MHM 08/21/2005 Generate 940 EDI transaction for pick tickets directed to 3PL providers.
IF 'AS' $ gcCmpModules AND SEEK(&lcTmpOrdLn..cWareCode,'WareHous') AND  SEEK('W'+WareHous.cThrdPLPr,'EDIACPRT') AND ;
    SEEK(EDIACPRT.cPartCode+'940','EDIPD')
  lnUseAlas   = ALIAS()
  SELECT EDITRANS
  IF !SEEK('940'+PADR(&lcTmpOrdLn..PikTkt,40)+'W'+WareHous.cThrdPLPr)
      INSERT INTO 'EDITRANS' (CEDITRNTYP,KEY,TYPE,CPARTNER,lInterComp) VALUES ;
                   ('940',&lcTmpOrdLn..PikTkt,'W',WareHous.cThrdPLPr,EDIACPRT.lInterComp)
  ENDIF
  REPLACE cStatus WITH 'N'
  =gfAdd_Info('EDITRANS')
  SELECT(lnUseAlas)
ENDIF

*E129473,1 MHM 08/21/2005 (End)

*B803078,1 MAB 03/07/2000 [Begin] Pick Ticket not saved probably, because of Error in selected index
PRIVATE lcOrdlOrd
lcOrdlOrd = ORDER("ORDLINE")
SET ORDER TO ORDLINE IN ORDLINE
*B803078,1 MAB 03/07/2000 [End  ] Pick Ticket not saved probably, because of Error in selected index

*IF nProcNo [The step number (for the rollback)] equal 1
IF nProcNo = 1
  
  *IF There is a record for this Order line in the ORDLINE file
  IF SEEK('O' + Order + STR(LineNo , 6) , 'ORDLINE')
    SELECT ORDLINE
    = RLOCK()
    
    *B603239,1 Update pick date when generating pick ticket. [Begin]
    *REPLACE PikTkt  WITH &lcTmpOrdLn..PikTkt
    REPLACE PikTkt  WITH &lcTmpOrdLn..PikTkt , PikDate WITH gdSysDate
    *B603239,1 Update pick date when generating pick ticket. [Begin]
    
    =gfAdd_Info()
    UNLOCK
  ENDIF    && End of IF
  SELECT (lcTmpOrdLn)
  REPLACE nProcNo   WITH 2
  =RLOCK()
  UNLOCK
ENDIF    && End of IF      

*IF nProcNo [The step number (for the rollback)] equal 2
IF nProcNo = 2

  SELECT PIKTKT
  
  *IF There is a no record for this Pick ticket number in the PIKTKT file
  IF !SEEK(&lcTmpOrdLn..PikTkt)
    APPEND BLANK
    = RLOCK()
    REPLACE Account   WITH &lcTmpOrdLn..Account ,;
            Store     WITH &lcTmpOrdLn..Store ,;
            Order     WITH &lcTmpOrdLn..Order ,;
            PikTkt    WITH &lcTmpOrdLn..PikTkt ,;
            Date      WITH gdSysDate ,;
            cWareCode WITH &lcTmpOrdLn..cWareCode ,;
            CustPo    WITH IIF(ORDHDR.MultiPo , &lcTmpOrdLn..CustPo , ORDHDR.CustPo) ,;
            Status    WITH 'O'

    =gfAdd_Info()
    UNLOCK
  ENDIF    && End of IF

  *B603111,1 Add new Piktkt to temp. session file. [Begin]
*  IF !SEEK(Order+Store+cWareCode,lcTmpPkTk)
    m.Order = Order
    m.Store = Store
    m.cWareCode = cWareCode
    m.PikTkt = PikTkt
    m.cLineNo = STR(&lcTmpOrdLn..LineNo,6)
    INSERT INTO (lcTmpPkTk) FROM MEMVAR
*  ENDIF
  *B603111,1 Add new Piktkt to temp. session file. [End  ]

  SELECT (lcTmpOrdLn)
  REPLACE nProcNo   WITH 3
  =RLOCK()
  UNLOCK

ENDIF    && End of IF      

*E300989,1 Now we have pick ticket records in the browse,[Begin
*E300989,1 thus I comment out the following lines.
*IF nProcNo [The step number (for the rollback)] equal 3
*IF nProcNo = 3
  *MAB
  *lnDellRec = lnDellRec + 1
  *lnSelRec = lnSelRec - 1
  *lnSelAlo = lnSelAlo - 1
  *lnAloRec = lnAloRec - 1
  *SELECT (lcTmpOrdLn)
  *MAB
  *DELETE
*ENDIF    && End of IF  (lfGenPikTk,lnSelRec,lnDellRec,delete)
*E300989,1 Now we have pick ticket records in the browse[End..

*B803078,1 MAB 03/07/2000 [Begin] Pick Ticket not saved probably, Restore Ordline order
SET ORDER TO &lcOrdlOrd. IN ORDLINE
*B803078,1 MAB 03/07/2000 [End  ] Pick Ticket not saved probably, Restore Ordline order
*C102424,1 (Begin) Add record for each piktkt in ALSHPINF for Ogio.
IF ASCAN(laEvntTrig,PADR("UPDSHPA",10)) <> 0
  llRelease = .F.
  = gfDoTriger("ALAUTAL",PADR("UPDSHPA",10))
ENDIF  
*C102424,1 (Begin)
*C037816,1 MHM 04/06/2004 Custom Save PikTkt To custom table for DL[Start]
IF ASCAN(laEvntTrig,PADR("ALSAVAUT",10)) <> 0
  = gfDoTriger("ALAUTAL",PADR("ALSAVAUT",10)) 
ENDIF 
*C037816,1 MHM [End]

*-- end of lfGenPikTk.

*!*************************************************************
*! Name      : lfPrepVar
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Function to prepare the needed variabels in case
*!             of incompleted session
*!*************************************************************
*! Called from : ALAUTAL.PRG
*!*************************************************************
*! Calls       : gfThermo()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfPrepVar
PRIVATE lnTotRec , lnCurnRec
SELECT (lcTmpOrdLn)

lnSelRec = 0
lnSelAlo = 0
lnAloRec = 0
lnDellRec = RECCOUNT()
lnTotRec = RECCOUNT()          && Varible to hold the Total count to be done for the thermometer
lnCurnRec = 0                  && Varible to hold the current count to be done for the thermometer

*SCAN Loop to scan the temp. Order lines file
SCAN
  lnCurnRec = lnCurnRec + 1
  lnDellRec = lnDellRec - 1
  lnAloRec = IIF(TotPik = 0 , lnAloRec , lnAloRec + 1)
  
  *IF the current record is selected
  IF llSel
    lnSelRec = lnSelRec + 1
    lnSelAlo = IIF(TotPik = 0 , lnSelAlo , lnSelAlo + 1)
  ENDIF    && End of IF
  =gfThermo(lnTotRec , lnCurnRec , "Preparing data for incompleted session... ")
ENDSCAN    && End of SCAN Loop
GO TOP

*IF All the records in the temp. Order lines file is Deleted
IF RECCOUNT() = lnDellRec
  STORE 'DISABLE' TO lcSelAllSt , lcSelNonSt , lcAloSt , lcRelSt , lcInvertSt

  *E300989,1 Now There is no need to lcScopSt variable because Scope is Enabled allover .
  *lcScopSt = 'ENABLE'
ELSE    && Else
  lcSelAllSt = IIF(lnSelRec = RECCOUNT() - lnDellRec , 'DISABLE' , 'ENABLE')
  lcSelNonSt = IIF(lnSelRec = 0 , 'DISABLE' , 'ENABLE')
  lcAloSt = lcSelNonSt
  lcRelSt = IIF(lnSelAlo = 0 , 'DISABLE' , 'ENABLE')
  lcInvertSt = 'ENABLE'

  *E300989,1 Now There is no need to lcScopSt variable because Scope is Enabled allover .
  *lcScopSt = 'DISABLE'
ENDIF    && End of IF

=gfThermo(lnTotRec , lnTotRec , "Preparing data for incompleted session... ")

*!*************************************************************
*! Name      : lfEmpty
*! Developer : Haytham El_Sheltawi
*! Date      : 09/28/1997
*! Purpose   : Function to check if the Option grid elmint is empty
*!*************************************************************
*! Called from : lfCreatExp()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) The Option grid value array column
*!                     2) .T. if the field is a Date field ,
*!                        .F. Otherwise
*!*************************************************************
*! Return      : .T. if empty Value , .F. Otherwise
*!*************************************************************
*
FUNCTION lfEmpty

PARAMETERS lcParm , llParm

*IF Character
IF TYPE('lcParm') = 'C'
  lcParm = STRTRAN(lcParm , '|' , '')
  lcParm = IIF(llParm , STRTRAN(lcParm , '/' , '') , lcParm)
ENDIF    && End of IF

RETURN EMPTY(lcParm)

*!*************************************************************
*! Name      : lfTStr
*! Developer : Haytham El_Sheltawi
*! Date      : 10/07/1997
*! Purpose   : Function to change any value to a string
*!*************************************************************
*! Called from : lfGetExp()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Value of any type
*!*************************************************************
*! Return      : The passed value into String 
*!*************************************************************
*
FUNCTION lfTStr

PARAMETERS laValTSw

PRIVATE lnNonDicml , lnDicml , lnDecmlVal

*DO CASE Statment
DO CASE
  
  *Case of Character or Memo
  CASE TYPE('laValTSw') = 'C' .OR. TYPE('laValTSw') = 'M'
    *B803340,1 HBG 07/06/2000 Take ALLTRIM for the value of the field expression [Begin]
    *RETURN laValTSw
    RETURN ALLTRIM(laValTSw)
    *B803340,1 HBG 07/06/2000 Take ALLTRIM for the value of the field expression [End  ]
  
  *Case of Logical
  CASE TYPE('laValTSw') = 'L'
    RETURN IIF(laValTSw , '.T.' , '.F.')
    
  *Case of Date
  CASE TYPE('laValTSw') = 'D'
    RETURN DTOC(laValTSw)
  
  *Case of Numeric
  CASE TYPE('laValTSw') = 'N'
    lnNonDicml = LEN(ALLTRIM(STR(laValTSw , 100 , 0)))    && Variable to hold the length of the Non decimal part
    lnDicml = 0                  && Variable to hold the length of the decimal part
    lnDecmlVal = ABS(laValTSw - INT(laValTSw))            && Variable to hold the absolute value of the decimal part
    
    *DO WHILE Loop to get the length of the decimal part
    DO WHILE lnDecmlVal > 0
      lnDicml = lnDicml + 1
      lnDecmlVal = (lnDecmlVal * 10) - INT(lnDecmlVal * 10)
    ENDDO    && End of DO WHILE Loop
    
    lnDicml = IIF(lnDicml = 0 , lnDicml , lnDicml + 1)
    RETURN ALLTRIM(STR(laValTSw , lnNonDicml + lnDicml + 1 , lnDicml))
    
  *Otherwise
  OTHERWISE
    RETURN ''
  
ENDCASE

*!*************************************************************
*! Name      : lfvForcAlo
*! Developer : Haytham El_Sheltawi
*! Date      : 02/16/1998
*! Purpose   : Valid function of push button Force allocation
*!*************************************************************
*! Called from : ALAUTAL3.SCX [Push button Force allocation]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvForcAlo

*E300989,1 This function is no longer in use.
IF llForceAlc
  SHOW GET llForceAlc,1 PROMPT lcNForcBmp
ELSE
  SHOW GET llForceAlc,1 PROMPT lcForceBmp
ENDIF

*!*************************************************************
*! Name      : lfGetWareH
*! Developer : Haytham El_Sheltawi
*! Date      : 05/03/1998
*! Purpose   : Function to get the fill the warehouse desc. and
*!             warehouse code arrays
*!*************************************************************
*! Called from : ALAUTAL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfGetWareH

SELECT cWareCode;
  FROM WAREHOUS;
 WHERE lStyInv;
  INTO ARRAY laWareCode

SELECT cDesc;
  FROM WAREHOUS;
 WHERE lStyInv;
  INTO ARRAY laWareHous

*!*************************************************************
*! Name      : lfAdUnCmSR
*! Developer : Haytham El_Sheltawi
*! Date      : 05/06/98
*! Purpose   : Adding record in uncomplete session file 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfAdUnCmSR()
*!*************************************************************

FUNCTION lfAdUnCmSR

PARAMETERS llComplete

PRIVATE lnAlias

llComplete = IIF(TYPE('llComplete') <> 'L' , .F. , llComplete)
lnAlias = SELECT(0)

SELECT UnCmSess
IF lnUnCmRec = 0
  IF !SEEK('I')
    APPEND BLANK
  ENDIF
  lnUnCmRec = RECNO()
  =RLOCK()
ELSE
  GO lnUnCmRec
ENDIF

BLANK
REPLACE Status     WITH IIF(llComplete , 'C' , 'O')       ,;
        cUTranType WITH 'ALAUTAL' ,;
        cUserId    WITH gcUser_id ,;
        cSession   WITH lcSession ,;
        cProgram   WITH 'ALAUTAL' ,;
        cCurrScr   WITH 'ALAUTAL' ,;
        cCurrObj   WITH ''        ,;
        dTranDate  WITH gdSysDate ,;
        cTranTime  WITH TIME()
        

IF !llComplete
  lcFiles = "lcTmpOrdLn," + lcTmpOrdLn + "," + ORDER(lcTmpOrdLn) + ";"

  *B603111,1 Add Temp. Pick ticket file to uncomplete session files [Begin]
  lcFiles = lcFiles + "lcTmpPkTk,"  + lcTmpPkTk+","  + ORDER(lcTmpPkTk)+";"
  *B603111,1 Add Temp. Pick ticket file to uncomplete session files [End  ]
  
  *C200084,1 Add Temp. file to uncomplete session function [Begin]
  IF ASCAN(laEvntTrig,PADR("UNCOMPLETE",10)) <> 0
    = gfDoTriger('ALAUTAL','UNCOMPLETE')
  ENDIF  
  *C200084,1 Add Temp. file to uncomplete session function [Begin]

  =gfSavSess('ALAUTAL' , lcFiles , @laVarTSav , lcSession)
ENDIF

UNLOCK
=RLOCK()

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvBrow
*! Developer : Haytham El_Sheltawi
*! Date      : 05/14/98
*! Purpose   : The browse valid function
*!*************************************************************
*! Calls     : gfStopBrow()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvBrow()
*!*************************************************************
*
FUNCTION lfvBrow

IF !WONTOP(lcOrdLBrow)
  =gfStopBrow()
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL Alt+L
  ON KEY LABEL Alt+A
  ON KEY LABEL Alt+N
  ON KEY LABEL Alt+I
  ON KEY LABEL ALT+B ACTIVATE WINDOW (lcOrdLBrow)
ENDIF

****************************************************************
****************************************************************
*E300989,1 All of the following section if for this enhancement.
*--------------- Function for dyelot case and fix bugs----------
*-------------------- Mohamed Atia Badran (MAB) ----------------
****************************************************************
****************************************************************

*-------------------Selection OG Functions ---------------
*!*************************************************************
*! Name      : lfwFilter
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 09/10/1998
*! Purpose   : Selection grid when function
*!*************************************************************
*! Called from : Selection OG
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfwFilter()
*!*************************************************************
*E300989,1
*
FUNCTION lfwFilter
lnDummyPos = lfItmPos('llDummy')

*C200084,1 Default FP Setting. [Begin]
IF ASCAN(laEvntTrig,PADR("SPC_SETS",10)) <> 0
  lcSetSectn = "SLCWHEN"  && Parameter passed to setting function.
  = gfDoTriger('ALAUTAL','SPC_SETS')
ENDIF  
*C200084,1 Default FP Setting. [End  ]

*B802267,1 Include Orders on Hold Validation. [Begin]
= lfvIncHold()
= lfvExlBulk()
*B802267,1 Include Orders on Hold Validation. [End  ]
*B803379,1 HBG 07/05/2000 Set Focus to the same option in OG. [begin]
=lfActivObj()
*B803379,1 HBG 07/05/2000 Set Focus to the same option in OG. [End  ]
*-- end of lfwFilter.

*!*************************************************************
*! Name      : lfvSepCor
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 09/10/1998
*! Purpose   : Separates/Coordinate validation
*!*************************************************************
*! Called from : Selection OG
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfvSepCor()
*!*************************************************************
*E300989,1
*
FUNCTION lfvSepCor
llRpPikSep = (lcRpSepCor $ 'BS')
llRpPikCor = (lcRpSepCor $ 'BC')
*-- end of lfvSepCor.

*!*************************************************************
*! Name      : lfvExclOrd
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 09/10/1998
*! Purpose   : Exclude certain orders validation.
*!*************************************************************
*! Called from : Selection OG
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfvExclOrd()
*!*************************************************************
*E300989,1
*
FUNCTION lfvExclOrd
llRpGdExcl = !llRpGdExcl
lcRpExSlct = ' '  && Default select by all.

*B803379,1 HBG 07/05/2000 get the focus of current object [Begin] 
lnActivObj = _CUROBJ
*B803379,1 HBG 07/05/2000 get the focus of current object [End

CLEAR READ        && Activate exclude options.
*-- end of lfvExclOrd.

*!*************************************************************
*! Name      : lfMakeArrs
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 09/10/1998
*! Purpose   : Fill Include/Exclude arrays.
*!*************************************************************
*! Called from : Selection OG
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfMakeArrs()
*!*************************************************************
*E300989,1
*
FUNCTION lfMakeArrs

DIMENSION laIncExprs[1,ALEN(laOGFxFlt,2)],;
          laExcExprs[1,ALEN(laOGFxFlt,2)]
STORE '' TO laIncExprs,laExcExprs

lnJ = 1
FOR lnI = 1 TO ALEN(laOGFxFlt,1)
  lnJ = IIF(lnI = lnDummyPos,1,lnJ)
  IF !INLIST(lnI,lnDummyPos,lnDummyPos+1) AND !EMPTY(laOGFxFlt[lnI,6])
    IF (lnI < lnDummyPos) AND !EMPTY(laIncExprs[1,1])
      lnJ = lnJ + 1
      DIMENSION laIncExprs[lnJ,ALEN(laOGFxFlt,2)]
    ENDIF
    
    IF (lnI > lnDummyPos) AND !EMPTY(laExcExprs[1,1])
      lnJ = lnJ + 1
      DIMENSION laExcExprs[lnJ,ALEN(laOGFxFlt,2)]
    ENDIF

    lcSubArray = IIF(lnI < lnDummyPos,'laIncExprs','laExcExprs') 
    FOR lnK = 1 TO ALEN(laOGFxFlt,2)
      &lcSubArray[lnJ,lnK] = laOGFxFlt[lnI,lnK]
    ENDFOR

  ENDIF
  
ENDFOR
*-- end of lfMakeArrs.

*!*************************************************************
*! Name      : lfvSlctExc
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 09/10/1998
*! Purpose   : Exclude select by validation.
*!*************************************************************
*! Called from : Selection OG
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfvSlctExc()
*!*************************************************************
*E300989,1
*
FUNCTION lfvSlctExc
lcRpExSlct = laOGFxFlt[lnDummyPos+1,6]

*B802267,1 Clear Pre Selected Range. [Begin]
llClrSty2 = (lcRpExSlct # "S")
llClrOrd2 = (lcRpExSlct # "O")
llClrAcc2 = (lcRpExSlct # "A")
*B802267,1 Clear Pre Selected Range. [End  ]

*B803379,1 HBG 07/05/2000 get the focus of current object [Begin] 
lnActivObj = _CUROBJ
*B803379,1 HBG 07/05/2000 get the focus of current object [End   

CLEAR READ
*-- end of lfvSlctExc.

*-------------------Allocation (Logic/Pick Ticket) OG Functions ------
*!*************************************************************
*! Name      : lfAlocGrid
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 09/10/1998
*! Purpose   : Call allocation grid and prepair indecies
*!*************************************************************
*! Called from : Menu,lfvScope
*!*************************************************************
*! Calls       : gfModalGen,gfOpGrid,lfPickGrid.
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfAlocGrid()
*!*************************************************************
*E300989,1
*
FUNCTION lfAlocGrid
*E300989,1 Change index expression if system use dyelots. [begin]
*E300989,1 and user want to include style dyelots and material dyelot setup. 
*lcIndexExp = laIndexExp[lnRpSort1] + ' + ' +laIndexExp[lnRpSort2] + ' + ' +;
*             laIndexExp[lnRpSort3] + IIF(INLIST(4 , lnRpSort1 , lnRpSort2 ,;
*             lnRpSort3) , '' , '+ ORDER') + ' + STORE + GROUP + STR(LINENO , 6)'
lcOldIndex = IIF(EMPTY(lcOldIndex),lfEvalIndx(),lcOldIndex)
lcIndexExp = IIF(EMPTY(lcIndexExp),lcOldIndex,lcIndexExp)
*E300989,1 Change index expression if system use dyelots. [end]

*E500304,1 WAB - llRpIncWip (include WIP) = .T. in case of calculate the available
*E500304,1 WAB - qty = WIP + STQ - ALO and the select By (in option grid) include
*E500304,1 WAB - (C/T or P/O or All)
*E500304,1 WAB - START
llRpIncWip = IIF(llTotAvlbl AND (lcRpScpMod $ 'KP' OR EMPTY(lcRpScpMod)),.T.,.F.)
*E500304,1 WAB - END

*B602569,1 Add IF there allocation must done [Begin]
IF llPartAlo
  *-- Do you wish to Allocate. <Yes>  <No>
  IF gfModalGen('QRM44052B42002','Dialog') = 1

    *-- call allocation logic grid.
    lcExpr1 = gfOpGrid('ALAUTALC' , .T.)  && Allocation logic grid.
    
    *B603457,1 KHM 02/16/2000 (Begin) Adding the custom process for
    *B603457,1                Fresh Produce to save the sort options 
    *B603457,1                in the SortLevl.MEM file
    FOR lnCntr = 1 TO 4
      lcArrayCnt = STR(lnCntr,1)
      laSortAry[lnCntr,1] = lnRpSort&lcArrayCnt
      laSortAry[lnCntr,2] = laIndexExp[lnRpSort&lcArrayCnt]
    ENDFOR
    IF ASCAN(laEvntTrig,PADR("SAVESORT",10)) <> 0
      = gfDoTriger('ALAUTAL','SAVESORT')
    ENDIF  
    *B603457,1 KHM 02/16/2000 (End) 
    *-- if user press run and call allocation grid from menu itself and
    *-- change Sort1/Sort2/Sort3 .
    IF TYPE('lcExpr1') <> 'L'
      lcIndexExp = lfEvalIndx()
      lnSaveRec = RECNO(lcTmpOrdLn)
      GO TOP IN (lcTmpOrdLn)
      IF !llStartSlc AND !EOF(lcTmpOrdLn) AND !(lcIndexExp == lcOldIndex)
        SELECT (lcTmpOrdLn)
        REPLACE ALL cSortField WITH EVALUATE(lcIndexExp)
        GO TOP
      ENDIF
    
      IF BETWEEN(lnSaveRec,1,RECCOUNT(lcTmpOrdLn))
        GO lnSaveRec IN (lcTmpOrdLn)
      ENDIF
    
      lcOldIndex = lcIndexExp
      llRpAlocat = .T.  && user want to allocate.
  
    ELSE

      llRpAlocat = .F.  && Disable Allocate Flag.

    ENDIF  && end if user press OG <Run> .

  ELSE  && User response message with <No>

    llRpAlocat = .F.

  ENDIF

ELSE

  llRpAlocat = .T.

ENDIF
*B602569,1 Add IF there allocation must done [End  ]

*B602569,1 Rise full allocation flag because after now we are in screen, [Begin]
*          i.e. allocate if user want to do.
llPartAlo = .T.
*B602569,1 Rise full allocation flag because after now we are in screen, [End  ]

*C200084,1 If Fresh Produce Custom program [Begin]
IF TYPE("laPanelObj[4,1]") = "C"
  IF llRpAlocat OR (lcRpAloNot # 'N')
    = lfPickGrid() && Call picket ticket quetion.
  ENDIF  
ELSE  && else standard program
  llRpGenPik = .F.
ENDIF
*C200084,1 If Fresh Produce Custom program [End  ]
*-- end of lfAlocGrid.

*!*************************************************************
*! Name      : lfwAloc
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 09/10/1998
*! Purpose   : Allocation logic grid when function.
*!*************************************************************
*! Called from : Allocation logic OG
*!*************************************************************
*! Calls       : lfShowItem
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfwAloc()
*!*************************************************************
*E300989,1
*
FUNCTION lfwAloc

*B603457,1 KHM 02/16/2000 (Begin) Checking if the default then
*B603457,1                restore the default sort options.
IF lnOGSeting = 1
  laSortAry[1,1] = 1
  laSortAry[1,2] = [DTOS(COMPLETE)]
  laSortAry[2,1] = 2
  laSortAry[2,2] = [PRIORITY]
  laSortAry[3,1] = 3
  laSortAry[3,2] = [DTOS(START)]
  laSortAry[4,1] = 4
  laSortAry[4,2] = [ORDER]
  lnRpSort1 = 1
  lnRpSort2 = 2
  lnRpSort3 = 3
  lnRpSort4 = 4
  FOR lnCounter = 1 TO 4
    lcCounter = STR(lnCounter,1)
    =lfOGShowGet('lnRpSort'+lcCounter)
  ENDFOR
ENDIF
*B603457,1 KHM 02/16/2000 (End)
IF llRpForAlo

  *-- Pick separates %
  IF llRpPikSep
    lnRpPikSep = 100
    =lfShowItem('lnRpPikSep')
  ENDIF

  *-- Pick coordinate %
  IF llRpPikCor
    lnRpPikCor = 100
    =lfShowItem('lnRpPikCor')
  ENDIF
  
  *-- cut-off units
  lnRpCutUnt = 0
  =lfShowItem('lnRpCutUnt')
  
  *-- allocate conditionally.

  *B603691,1 Separetes has its own default value [Begin]
  *IF (llUseDyes AND llFabDye) AND (lcRpSepCor<>'S')
  IF (llUseDyes AND llFabDye)
    *llRpCond = .F.
    llRpCond = (lcRpSepCor='S')
  *B603691,1 Separetes has its own default value [End  ]

    =lfShowItem('llRpCond')
  ENDIF

ENDIF
*-- When function for allocation grid.
*-- end of lfwAloc.

*!*************************************************************
*! Name      : lfPickGrid
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 09/10/1998
*! Purpose   : Call allocation grid and prepair indecies
*!*************************************************************
*! Called from : Menu,lfAlocGrid
*!*************************************************************
*! Calls       : gfModalGen,gfOpGrid.
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfPickGrid()
*!*************************************************************
*E300989,1
*
FUNCTION lfPickGrid

*-- Do you wish to Generate pick ticket. <Yes>  <No>
*C102282,1 ABD - Add flage to prvent user to allocate qty from this program. [Begin]

IF llPikNow .AND. gfModalGen('QRM44053B42002','Dialog') = 1
  *C102282,1 ABD [End]
  
  *C123847,5  TMI [Start] Always generate a new Piktkt for DIR03, so no need for the grid to be shown
  IF ASCAN(laEvntTrig,'GENEWPKT')<>0
    =gfDoTriger('ALAUTAL','GENEWPKT')
  ELSE
    *C123847,5  TMI [End  ] 
  
    lcExpr2 = gfOpGrid('ALAUTPIK' , .T.)   && Pick Ticket grid.
    
    IF TYPE('lcExpr2') <> 'L'
      llRpGenPik = .T.
    ELSE
      llRpGenPik = .F.
    ENDIF

    *C123847,5  TMI [End  ] Close the trigger if statement
  ENDIF
  *C123847,5  TMI [End  ] 
ELSE  && User response message with <No>
  llRpGenPik = .F.
ENDIF
*-- end of lfPickGrid.

*!*************************************************************
*! Name      : lfvOGFrcAl
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Valid function of the Force allocation
*!*************************************************************
*! Called from : Option grid [Force allocation Option]
*!*************************************************************
*! Calls       : lfShowItem()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvOGFrcAl
*E300989,1 Comment the following lines and write others instead of it [begin
*--        to fully control logic grid.

*laOGObjCnt[2] = IIF(llRpForAlo .OR. !llRpPikSep , .F. , .T.)
*laOGObjCnt[4] = IIF(llRpForAlo .OR. !llRpPikCor , .F. , .T.)
*laOGObjCnt[5] = IIF(llRpForAlo , .F. , .T.)

*=lfOGShowGet('lnRpPikSep')
*=lfOGShowGet('lnRpPikCor')
*=lfOGShowGet('lnRpCutUnt')
*E300989,1 Comment the following lines and write others instead of it [end..

*-- if user want to force allocation
IF llRpForAlo

  *-- Pick separates %
  IF llRpPikSep
    lnRpPikSep = 100
    =lfShowItem('lnRpPikSep')
  ENDIF

  *-- Pick coordinate %
  IF llRpPikCor
    lnRpPikCor = 100
    =lfShowItem('lnRpPikCor')
  ENDIF
  
  *-- cut-off units
  lnRpCutUnt = 0
  =lfShowItem('lnRpCutUnt')
  
  *-- allocate conditionally.
  *B603691,1 Separetes has its own default value [Begin]
  *IF (llUseDyes AND llFabDye) AND (lcRpSepCor<>'S')
  IF (llUseDyes AND llFabDye)
    *llRpCond = .F.
    llRpCond = (lcRpSepCor='S')
  *B603691,1 Separetes has its own default value [End  ]

    =lfShowItem('llRpCond')
  ENDIF

ELSE  && else user want to unforce allocation.

  *-- Pick separates %
  IF llRpPikSep
    =lfShowItem('lnRpPikSep',.T.)
  ENDIF

  *-- Pick coordinate %
  IF llRpPikCor
    =lfShowItem('lnRpPikCor',.T.)
  ENDIF
  
  *-- cut-off units
  =lfShowItem('lnRpCutUnt',.T.)
  
  *-- allocate conditionally.
  *B603691,1 Separetes has its own default value [Begin]
  *IF (llUseDyes AND llFabDye) AND (lcRpSepCor<>'S')
  IF (llUseDyes AND llFabDye)
  *B603691,1 Separetes has its own default value [End  ]

    =lfShowItem('llRpCond',.T.)
  ENDIF

ENDIF
*-- end of lfvOGFrcAl.

*!*************************************************************
*! Name      : lfShowItem
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 01/14/1999
*! Purpose   : Enable/Disable Allocation grid objects.
*!*************************************************************
*! Called from : lfvOGFrcAl
*!*************************************************************
*! Calls       : lfOGShowGet()
*!*************************************************************
*! Passed Parameters : Object,Object State(.T. or .F.)
*!*************************************************************
*! Return      : None
*!*************************************************************
*E300989,1
*
FUNCTION lfShowItem
PARAMETERS lcItem,llState    
PRIVATE lnItemPos
lnItemPos = ASCAN(laOGObjType,lcItem)
IF lnItemPos > 0 
  lnItemPos = ASUBSCRIPT(laOGObjType,lnItemPos,1)
  laOGObjCnt[lnItemPos] = llState
ENDIF  
=lfOGShowGet(lcItem)
*-- end of lfShowItem.

*!*************************************************************
*! Name      : lfFillMVar
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 09/09/98
*! Purpose   : Fill New temporary fields with its data.
*!*************************************************************
*! Calls     : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfFillMVar()
*!*************************************************************
*E300989,1
*
FUNCTION lfFillMVar
m.Priority   = ORDHDR.Priority
m.Entered    = ORDHDR.Entered
m.cDivision  = STYLE.cDivision
m.cStyGroup  = STYLE.cStyGroup
m.Fabric     = STYLE.Fabric
m.Pattern    = STYLE.Pattern
m.cStyMajor  = STYLE.cStyMajor
m.cPeggedDye = m.Dyelot

*-- if either MF or PO modules is installed
IF ('MF' $ gcCmpModules .OR. 'PO' $ gcCmpModules)
  *-- if user did not select By (C/T or P/O)
  IF !(lcRpScpMod $ 'KP')
    PRIVATE lnAliasNo,lcPickOrd
    lnAliasNo = SELECT(0)
    lcPickOrd = ORDER('CUTPICK')
    SELECT CUTPICK
    SET ORDER TO TAG Cutord
    = SEEK('1'+ORDHDR.Order+STR(ORDLINE.LineNo,6)) OR;
      SEEK('2'+ORDHDR.Order+STR(ORDLINE.LineNo,6))
    SET ORDER TO TAG &lcPickOrd
    SELECT (lnAliasNo)
  ENDIF
  m.Trancd = CUTPICK.Trancd
  m.CtktNo = CUTPICK.CtktNo
ENDIF  

*-- Get primary fabric and its corresponding color, from BOM file[Begin
m.cFabColor = ''
IF llFabDye AND !llRpExlDye AND (STYLE.CDYE_FLG = 'Y')  AND ;
   !EMPTY(STYLE.FABRIC)

  PRIVATE lnAliasNo,lcFabOrd
  lnAliasNo = SELECT(0)
  lcFabOrd  = ORDER('FABRIC')

  IF SEEK(SUBSTR(STYLE.STYLE,1,lnMajorLen),'BOM')
    SELECT BOM 
    LOCATE REST WHILE cItmMajor = SUBSTR(STYLE.STYLE,1,lnMajorLen) ;
           FOR LIKE(STRTRAN(cItmMask,'*','?'),PADR(STYLE.STYLE,19)) AND ;
           cCatgTyp = 'F'
    IF FOUND()
      m.cFabColor = IIF(ICLR # '******' , ICLR , ;
                    IIF(lcFree_Clr='C',;
                    SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),''))
      IF !EMPTY(m.cFabColor)
        SET ORDER TO TAG FABRIC IN FABRIC
        IF !SEEK(STYLE.FABRIC + m.cFabColor,'FABRIC') OR ;
           (FABRIC.CDYE_FLG # 'Y')
          m.cFabColor = '' 
        ENDIF
      ENDIF  
    ENDIF           
  ENDIF                

  SET ORDER TO TAG &lcFabOrd IN FABRIC
  SELECT (lnAliasNo)

ENDIF
*-- Get primary fabric and its corresponding color, from BOM file[End..
*-- end of lfFillMVar.

*!*************************************************************
*! Name      : lfRefScr
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 09/09/98
*! Purpose   : Refresh screen if exclude delete all records.
*!*************************************************************
*! Calls     : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfRefScr()
*!*************************************************************
*E300989,1
*
FUNCTION lfRefScr
SELECT (lcTmpOrdLn)
= lfwBrows()  && Refresh browse and screen.
SHOW GETS WINDOW (lcAutAlCh2) DISABLE ONLY  && disable select control window buttons
SHOW GET ibBrow ENABLE  && Enable invisiable trap button.
SHOW GETS WINDOW (lcAutAlCh3) DISABLE ONLY  && diable editable window
STORE 0 TO lnSelRec,lnSelAlo,lnAloRec,lnDellRec
STORE 'DISABLE' TO lcAloSt,lcRelSt
ACTIVATE WINDOW gwcContrl1  	&& Activate control panel window
SHOW GET pbAlo &lcAloSt       
SHOW GET pbRel &lcRelSt
SHOW GET pbGen &lcRelSt
_CUROBJ = OBJNUM(PBSCOP)         && Scope button on toolbar.
*-- end of lfRefScr.

*!*************************************************************
*! Name      : lfNormAlo
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 09/10/1998
*! Purpose   : allocation with out dyelot (Separates)
*!*************************************************************
*! Called from : lfAllocate
*!*************************************************************
*! Calls       : lfNormProc,gfThermo
*!*************************************************************
*! Passed Parameters : Allocation case (All Records/Selected Records)
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfNormAlo(1)
*!*************************************************************
*E300989,1
*
FUNCTION lfNormAlo
PARAMETERS lnAlcCase

*SCAN Loop to scan the temp. Order lines file for nProcNo less than 5
*and to skip Style with dyelot Yes and empty dyelot field

*E300989,1 Adjust scan expression to suuport all selected records .
*SCAN FOR nProcNo < 5 .AND. ;
*         !(llUseDyes .AND. EMPTY(DyeLot) .AND. STYLE.cDye_Flg = "Y")
SCAN FOR nProcNo < 5 .AND. (STYLE.cDye_Flg # "Y") AND IIF(lnAlcCase=1,.T.,llSel)
  *IF The line is Allocated befor
 
  =lfNormProc()  && Normal process.
  
  *-- if you must skip this record, because it's allocated before.
  IF llMustLoop
    llMustLoop = .F.
    LOOP
  ENDIF    

ENDSCAN    && End of SCAN Loop
=gfThermo(lnTotRec , lnTotRec , "Allocating..." , "Order number: " + '')

*C124051,1  TMI [Start] Release styles with only some color are allocated
IF ASCAN(laEvntTrig,PADR('RELALO',10)) <> 0
  =gfDoTriger('ALAUTAL','RELALO')
ENDIF
*C124051,1  TMI [End  ] 

*-- end of lfNormAlo.

*!*************************************************************
*! Name      : lfNormProc
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 09/10/1998
*! Purpose   : Normal allocation processes.
*!*************************************************************
*! Called from : lfNormAlo
*!*************************************************************
*! Calls       : lfTmpAlo,lfAloQty
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfNormProc()
*!*************************************************************
*E300989,1
*
FUNCTION lfNormProc

*IF there piktkt or totqty > totpik (Cancel allocation)
*note that in dyelot version there is an error (does not update pikline file)

*-- if this record is allocated before, but not picked.
*IF TotPik <> 0
*B602519,1 Do not reallocate picked lines (i.e. Lines that have pick ticket) [Begin]
IF Picked AND !('******' $ PikTkt)
*B602519,1 Do not reallocate picked lines (i.e. Lines that have pick ticket) [End  ]
  =lfMarkLine()
  llMustLoop = .T.
  RETURN
ENDIF    && End of IF

*IF Force allocation is No [in the Option grid] and Pick coordinate
*Min. % is greater than 0 and the Order line group is not empty
IF !llRpForAlo .AND. lnRpPikCor > 0 .AND. !EMPTY(Group)
  lnRecNumb = RECNO()          && Variable to save the record number
  lcGroupStr =  Order + Store + Group      && Variable to hold the Order number + Store + Group 
  llAloGoup = .T.                     && Flag to know if we are going to allocate this group or not
     
  *IF Not an incompleted session and not the begin of the temp. Order
  *lines file and NPROCNO = 0
  IF llContinue .AND. !BOF() .AND. NPROCNO = 0
    SKIP -1
    llGrpFrRec = IIF(Order + Store + Group = lcGroupStr , .F. , .T.)    && Flag to know if this is the first line in the group
      
    *IF Not the begin of the temp. Order lines file
    IF !BOF()
      SKIP 1
    ELSE    && Else
      LOCATE
    ENDIF    && End of IF
  ELSE    && Else
    llGrpFrRec = IIF(NPROCNO = 0 , .T. , .F.)    && Flag to know if this is the first line in the group
  ENDIF    && End of IF

  *E300989,1 lcCurStyWr ==> Style + location for current record.
  *E300989,1 is made for optimizing , I evaluate wip once for the same
  *E300989,1 Style/Location, if user want to include WIP in availability.
  lcCurStyWr = ''

  *SCAN Loop to scan the temp. Order lines file IF this was the first
  *line in the group FOR the same Order and Store and Group
  SCAN REST WHILE llGrpFrRec .AND. Order + Store + Group = lcGroupStr
    lnCurent = lnCurent + 1
       
    *IF We are going to Allocate this group
    IF llAloGoup
      lcPikWare = IIF(EMPTY(lcRpPkFWrh) , cWareCode , lcRpPkFWrh)    && Variable to hold the Warehouse to allocate from

      *E300989,1 Rise flag if you cal. wips for this style before. [1
      llCalWip = (lcCurStyWr = Style + cWareCode)
      
      llAloGoup = lfTmpAlo(.T. , lcPikWare , .F. , lnRpCutUnt , lnRpPikCor)
      
      *E300989,1 Rise flag if you cal. wips for this style before. [2
      lcCurStyWr = Style + cWareCode
      
    ENDIF    && End of IF
  ENDSCAN    && End of SCAN Loop
     
  *IF We are going to Allocate this group
  IF llAloGoup
    lnCurent = IIF(llGrpFrRec , lnCurent , lnCurent + 1)
    GO lnRecNumb
        
    *SCAN Loop to scan the temp. Order lines file FOR the same Order
    *and Store and Group
    SCAN REST WHILE Order + Store + Group = lcGroupStr
      lcPikWare = IIF(EMPTY(lcRpPkFWrh) , cWareCode , lcRpPkFWrh)    && Variable to hold the Warehouse to allocate from
      =lfAloQty(lcPikWare)
    ENDSCAN    && End of SCAN Loop
  
  ENDIF    && End of IF
  SKIP -1
    
ELSE    && Else (Force allocation or pick coordinate group min% = 0 or !empty(group))
  lcPikWare = IIF(EMPTY(lcRpPkFWrh) , cWareCode , lcRpPkFWrh)    && Variable to hold the Warehouse to allocate from
  lnCurent = lnCurent + 1
   *IF We are going to force the allocation
  IF llRpForAlo
    
    *E300989,1 We sure that this is not the same Style/location [begin
    llCalWip = .F.
    *E300989,1 We sure that this is not the same Style/location [end..

    llAloSep = lfTmpAlo(.F. , lcPikWare , .T.)  && Flag to know if we are going to allocate this record
    
    *IF We are going to allocate this record
    IF llAloSep
      =lfAloQty(lcPikWare)
    ENDIF    && End of IF
      
  ELSE    && Else (coordinate group min% > 0 or empty(group))

    llAloSep = lfTmpAlo(.T. , lcPikWare , .F. , lnRpCutUnt ,;
               IIF(EMPTY(Group) , lnRpPikSep , lnRpPikCor))      && Flag to know if we are going to allocate this record
         
    *IF We are going to force the allocation
    IF llAloSep
      =lfAloQty(lcPikWare)
    ENDIF    && End of IF
    
  ENDIF    && End of IF
ENDIF    && End of IF
     
=gfThermo(lnTotRec , lnCurent , "Allocating..." ,;
          "Order number: " + Order)
    
*-- end of lfNormProc.

*!*************************************************************
*! Name      : lfDyeAlo
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 09/10/1998
*! Purpose   : dyelot allocation.
*!*************************************************************
*! Called from : lfAllocate
*!*************************************************************
*! Calls       : lfNormProc , gfThermo
*!*************************************************************
*! Passed Parameters : Allocation case (All Records/Selected Records)
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfDyeAlo(1)
*!*************************************************************
*E300989,1
*
FUNCTION lfDyeAlo
PARAMETERS lnFromOgMn

*B603691,1 Declare values used when allocate separites only  [Begin]
PRIVATE lnActvLine , laDye_Rel
lnActvLine = 0
DECLARE laDye_Rel[1]
laDye_Rel = ""
*B603691,1 Declare values used when allocate separites only  [End  ]

*B602574,1 Comment out the following block because it transfered in the caller [Begin]
*-- if include wip and select by C/T or P/O and we have C/Ts or P/Os.
*IF llRpIncWip AND lcRpScpMod $ 'KP' AND !EMPTY(laIncExprs[1,6])
*  DIMENSION laString1[1]
*  =gfSubStr(laIncExprs[1,6],@laString1,'|')
*ENDIF

*-- if include wip and exclude select by C/T or P/O and we have C/Ts or P/Os.
*IF llRpIncWip AND llExclude AND (lcRpExSlct $ 'KP') AND !EMPTY(laFiltExp[1,6])
*  DIMENSION laString2[1]
*  =gfSubStr(laFiltExp[1,6],@laString2,'|')
*ENDIF
*B602574,1 Comment out the following block because it transfered in the caller [End  ]

*B603691,1 Save Allocate conditionally value  [Begin]
llSaveCond = llRpCond
*B603691,1 Save Allocate conditionally value  [End  ]

*-- scan file for selected records.
SCAN FOR nProcNo < 5 AND IIF(lnFromOgMn = 2,llSel,.T.)

  *B803138,1 Automatically Allocate separates also. [Begin]
  *-- if this is a separate line or not have records in BOM file.
  *IF EMPTY(Group) OR EMPTY(cFabColor) OR !SEEK(Fabric+cFabColor,'DYE_REL')
  IF EMPTY(cFabColor) OR !SEEK(Fabric+cFabColor,'DYE_REL')
  *B803138,1 Automatically Allocate separates also. [End  ]

    *B From Gehan [Begin]
    *= lfNormProc() 
    = (Style.cDye_Flg = "N" OR !EMPTY(Dyelot)) AND lfNormProc() 
    *B From Gehan [End  ]

    llMustLoop = .F.
    LOOP
  ENDIF

  *B803138,1 Automatically Allocate separates also. [Begin]
  
  *B603691,1 if separites Allocate un conditionally else it depends on user choice  [Begin]
  llRpCond = !EMPTY(Group) AND llSaveCond
  *B603691,1 if separites Allocate un conditionally else it depends on user choice  [End  ]
  
  *-- if we want to allocate separate line conditionally only.
  IF EMPTY(Group) AND (lnRpPikSep > 0) AND llRpCond
    PRIVATE lnActivRec
    lnActivRec = RECNO()
    SET FILTER TO RECNO() = lnActivRec
  ENDIF
  *B803138,1 Automatically Allocate separates also. [End  ]

  lcGrpExpr   = laIndexExp[lnRpSort1] + ' + ' +laIndexExp[lnRpSort2] + ' + ' +;
                laIndexExp[lnRpSort3] + IIF(INLIST(4 , lnRpSort1 , lnRpSort2 ,;
                lnRpSort3) , '' , '+ ORDER') +;
                ' + STORE + FABRIC + CFABCOLOR + GROUP'
  lcCurGrpVal = EVALUATE(lcGrpExpr)
  
  *-- if it's the last scaned group and no allocation.
  IF lcOldGroup = lcCurGrpVal
    llMustLoop = .F.
    = lfNormProc() 
    LOOP
  ENDIF

  lcAllWare  = ''   && Allocation location
  lcStartDye = ''   && Start dyelot
  lcPegged   = ''   && pegged dyelot
  lcOldSWare = ''   && old style/location for group lines.
  lcCurStyWr = ''   && old style/location for calculate wips.
  llEndProc  = .F.  && .T. if we end dyelot alloc. logic process.
  llAnOther  = .F.  && .T. if we must search for another dyelot.
  llNoDye    = .F.  && .T. if loop all dyelots and does have matching availability.
  llGrpStart = .T.  && .T. if we are at the start of group.
  llPitch    = .F.  && Pitch dyelot relation table is initially .F. for current group.
  
  *-- laOpnStock : Array hold available quantity.
  DIMENSION laOpnStock[9]
  STORE 0 TO laOpnStock

  *-- Process step value (descriped in the following lines)
  lnProcStep = 1

  *-- lnSkipTo : Skip up and down in dyelot relationship file(-1,+2,-3,+4)
  STORE 1 TO lnSkipTo
  lcLastSkip = 'F'  && last skip 'F' means Forward and 'B' backword.

  *-- loop until finish allocation process or exit.
  DO WHILE !llEndProc
    
    *-- loop Around current group.
    SCAN REST WHILE (&lcGrpExpr = lcCurGrpVal) ;
              FOR IIF(lnFromOgMn = 2,llSel,.T.)  

      *BADRAN 06/28/20000 [BEGIN]
      *WAIT WINDOW "Try to allocate Order : " + Order + CHR(13) + ;
                  ", Style/Color : " + Style + CHR(13) +;
                  " in line : " + str(lineno,6) + CHR(13) + ;
                  "Dye lot : " + lcStartDye NOWAIT
      *BADRAN 06/28/20000 [END  ]

      lcAllWare = IIF(EMPTY(lcRpPkFWrh) , cWareCode , lcRpPkFWrh)    && Variable to hold the Warehouse to allocate from

      *-- save current stydye record.
      lcStyDyeRc = STYDYE.STYLE+STYDYE.CWARECODE+STYDYE.DYELOT

      *-- if all process not finished and not find this dyelot in stydye file.
      IF !llEndProc AND !SEEK(STYLE+lcAllWare,'STYDYE')
        llEndProc = .T.  && end all process
        lnProcStep = 2   && step became 2. 
      ENDIF  && end if all process not finished and not find this dyelot in stydye file.
      = SEEK(lcStyDyeRc,'STYDYE')  && Restore previous stydye record.
             
      DO CASE 
        
        *-- First step check if there is pegged dyelot or start from
        *-- begin of dyelot relationship file.
        CASE lnProcStep = 1  && get pegged dyelot.
          
          *-- previous dyelot does not have sufficient quantity,[Begin
          *-- to allocate then get another one.
          IF llAnOther
            llAnOther  = .F.

            llNoDye = !lfAnother(FABRIC+CFABCOLOR)  && Find another closet dyelot.

            *-- if you still have dyelots.
            IF !llNoDye
              lcStartDye = DYE_REL.DYELOT  && Select Current start dyelot
            ENDIF
            EXIT
            
          ELSE   && have pegged dyelot or start with new dyelot.
          
            lcStartDye    = IIF(EMPTY(lcStartDye),DYELOT,lcStartDye)
            lcPegged      = lcStartDye
            IF !EMPTY(lcStartDye)
              EXIT  && to start check for availability step.
            ENDIF
          ENDIF
          *-- previous dyelot does not have sufficient quantity,[End..

        *-- Second step check availability for current group, for 
        *-- dyelot selected in step 1.
        CASE lnProcStep = 2  && check STYDYE availability for this group.
  
          IF !llEndProc
            *-- if you find this record in stydye calculate its OpnStock.
            *-- and if insufficient select another dyelot.
            IF !lfCalAval(SEEK(STYLE+lcAllWare+lcStartDye,'STYDYE'))
              llAnOther = .T.
              EXIT
            ENDIF
            lcOldSWare = STYLE+CWARECODE
          ENDIF  

        *-- Third step Allocate sufficient group.
        CASE lnProcStep = 3  && Allocate Current group.
          
          *-- if this line have new allocated 
          IF Tot_PoAlo > 0
            = lfAlocMast()   && Allocate in master files.
          
          ELSE  && else if there is previously allocation and came from OG.
          
            *-- if allocated before.
            IF TotPik <> 0 AND lnFromOgMn = 1
              = lfMarkLine()  && Mark record and increment allocation variables.
              LOOP          && Go to next record in current group.
            ENDIF    && End of IF

          ENDIF  

        *-- Fourth step if and only if fail in allocation and user
        *-- want to allocate unconditional.
        CASE lnProcStep = 4  && Unconditional Allocate Current group.
          
          *B603691,1 Reintializing pegged dyelot  [Begin]
          lcPegged = DYELOT
          *B603691,1 Reintializing pegged dyelot  [End  ]
          
          SET ORDER TO TAG DYE_REL IN DYE_REL
          *-- if you find this Fabric/Color pair in Dyelot Relation file.
          IF SEEK(FABRIC+CFABCOLOR+IIF(EMPTY(lcPegged),'',lcPegged),'DYE_REL')
            SET ORDER TO TAG SEQUENCE IN DYE_REL

            *-- llFindDye : .T. if you find your allocation dyelot.
            llFindDye = .F.
            
            *-- if there is no previously pegged dyelot.
            IF EMPTY(lcPegged)
              
              *-- if you find dyelot relation dyelot in stydye[Begin
              *-- exit loop else loop all dyelot relation records for 
              *-- current Fabric/Color pair.
              DO WHILE .T.
                IF SEEK(STYLE+lcAllWare+DYE_REL.DYELOT,'STYDYE')
                  llFindDye = .T.
                  EXIT
                ELSE
                  
                  SKIP 1 IN DYE_REL
                  IF DYE_REL.FABRIC+DYE_REL.COLOR # FABRIC+CFABCOLOR
                    llFindDye = .F.
                    EXIT
                  ENDIF
                ENDIF
              ENDDO  
              *-- if you find dyelot relation dyelot in stydye[End..
            
            ELSE  && else there pegged dyelot  .
            
              *-- The following means if there pegged dyelot to this 
              *-- line or any other start allocation with it when find it
              *-- in stydye file, else try with another.
              llFindDye = SEEK(STYLE+lcAllWare+lcPegged,'STYDYE')
            
            ENDIF  && end if there is no previously pegged dyelot.
          
            STORE .F. TO llNoDye,llPitch

            lcLastSkip = 'F' 
            lnSkipTo   = 1

            *-- loop while you find dyelot. 
            DO WHILE llFindDye
              
              IF lfCalAval(.T.,.T.)
                llFindDye = .F.  && to evaluate the next line.
                
                IF Tot_PoAlo > 0
                  = lfAlocMast()   && Allocate to master files.
                ELSE
                  IF TotPik <> 0 AND lnFromOgMn = 1
                    = lfMarkLine()  && Mark record and increment alloc. variables.
                  ENDIF    && End of IF
                ENDIF
                 
              ELSE
                
                *-- loop to find another dyelots.
                DO WHILE .T.
                  
                  *B603691,1 Check for another dyelots  [Begin]
                  *llNoDye = !lfAnother(FABRIC+CFABCOLOR)  && Find another closet dyelot.
                  *-- if you did not collect this data before.
                  IF (lnActvLine <> RECNO(lcTmpOrdLn))
                    =lfDyeRel(FABRIC+CFABCOLOR)
                  ENDIF
                  llNoDye = EMPTY(laDye_Rel) OR (ALEN(laDye_Rel,1) = 1)
                  *B603691,1 Check for another dyelots  [End  ]
                  
                  *-- if previous dyelot is last one in this group.
                  IF llNoDye
                    
                    EXIT
                  
                  ELSE  && else you still have dyelots in dyelot relation file.
                    
                    *B603691,1 if data errors exit this loop  [Begin]
                    IF EMPTY(lfGetDyelt(FABRIC+CFABCOLOR))
                      llNoDye = .T.
                      EXIT
                    ENDIF
                    *B603691,1 if data errors exit this loop  [End  ]
                                        
                    *-- if you find this dyelot in stydye file.
                    IF SEEK(STYLE+lcAllWare+DYE_REL.DYELOT,'STYDYE')
                      EXIT
                    ELSE  && this dyelot not in stydye file.
                      LOOP
                    ENDIF  && end if you find this dyelot in stydye file.
                  
                  ENDIF    && end if previous dyelot is last one in this group.
                  
                ENDDO  && end loop to find another dyelots.
                
                llFindDye = !llNoDye
              
              ENDIF
            ENDDO
          
          ENDIF  && end if you find this Fabric/Color pair in Dyelot Relation file.

          IF !llSel AND TotPik <> 0 AND lnFromOgMn = 1
            = lfMarkLine()  && Mark record and increment alloc. variables.
          ENDIF    && End of IF

      ENDCASE
    
    ENDSCAN  && end scan current group.
    
    *-- if step is to find dyelot avaialable for whole group.
    IF lnProcStep < 3
      = SEEK(lcCurGrpVal)  && Restore start of group.

    *B602748,1 go to previous record to Scan all rest records [Begin]
    ELSE
      SKIP -1
    *B602748,1 go to previous record to Scan all rest records [End  ]
    ENDIF  && end if step is to find dyelot avaialable for whole group.
      
    DO CASE
      CASE lnProcStep = 1
        
        *-- if no dyelots pegged to this group.
        *-- repeate previous steps with start of dyelot relation file.
        IF EMPTY(lcStartDye)
          
          *-- if Find matching dyelots for this Fabric/Color
          IF SEEK(Fabric+cFabColor,'DYE_REL')
            lcStartDye = DYE_REL.Dyelot  
          
          ELSE  && No Matching dyelots for this Fabric/Color
            llEndProc = .T.
            =SEEK(lcCurGrpVal)
            lcOldGroup = lcCurGrpVal
            llMustLoop = .F.
            = lfNormProc() 
            LOOP
          ENDIF  && end if Find matching dyelots for this Fabric/Color.
        
        ELSE     && Find dyelot pegged to this group.

          IF llGrpStart AND !EMPTY(lcPegged)
            SET ORDER TO TAG DYE_REL IN DYE_REL
            IF SEEK(Fabric+cFabColor+lcStartDye,'DYE_REL')
              llGrpStart = .F.  && do not enter this code again.
            
            ELSE
            
              llEndProc = .T.  && process ended from first step.
              =SEEK(lcCurGrpVal)
              lcOldGroup = lcCurGrpVal
              llMustLoop = .F.
              = lfNormProc() 
              LOOP  && end this group.
            ENDIF
            SET ORDER TO TAG SEQUENCE IN DYE_REL
          ENDIF
        
        ENDIF    && end if no dyelots pegged to this group.
        
        IF (lnProcStep # 4) AND !llNoDye AND !SEEK(STYLE+lcAllWare+lcStartDye,'STYDYE')
          llAnOther = .T.
        ENDIF

      CASE lnProcStep = 2
        
        *-- if no dyelots have availability.
        IF llEndProc  && no stydye records for one or more Style(s).
          LOOP
        ENDIF
      
      *-- current group was allocated.
      CASE lnProcStep = 3 OR lnProcStep = 4
        llEndProc  = .T.
    ENDCASE

    *-- If None of Dye_Rel Dyelots was found in StyDye file OR no. availability.
    IF (lnProcStep # 4) AND llNoDye
      
      *-- if user allocate conditionally and not to force allocation.
      IF !llRpForAlo AND llRpCond

        *-- if we want to allocate separate line conditionally only.
        *B603691,1 MAB 06/20/2000 Continue allocation until reach end of a group. [Begin]
        *LOCATE REST FOR &lcGrpExpr > lcCurGrpVal  && Skip current group
        IF EMPTY(Group) AND (lnRpPikSep > 0)
          SET FILTER TO
          IF BETWEEN(lnActivRec,1,RECCOUNT())
            
            *BADRAN 06/28/20000 [BEGIN]
            *if there is infinite loop may it caused by this block of code
            
            GO lnActivRec    && Record pointer is on tested record
            SKIP  && Skip Current Line
            *BADRAN 06/28/20000 [END  ]

            *-- if it is new group
            IF &lcGrpExpr <> lcCurGrpVal
              llEndProc = .T.
            ENDIF
            GO lnActivRec    && Again Re-Point Active record
            
          ENDIF
          
        ELSE
          LOCATE REST FOR &lcGrpExpr > lcCurGrpVal  && Skip current group
          llEndProc = .T.
        
        ENDIF
        *B603691,1 MAB 06/20/2000 Continue allocation until reach end of a group. [End  ]
      
      ELSE  && User want to force allocation or want to allocate unconditionally.

        lnProcStep = 4  
      ENDIF

    ENDIF    && end If None of Dye_Rel Dyelots was found in StyDye file.
    
    lnProcStep = IIF(lnProcStep = 4,lnProcStep,IIF(llAnOther,1,lnProcStep+1))
  
  ENDDO  && end loop until finish allocation process or exit.
  
  =gfThermo(lnTotRec , lnCurent , "Allocating..." ,;
            "Order number: " + Order)

  *B803138,1 Automatically Allocate separates also. [Begin]
  *-- if we want to allocate separate line conditionally only.
  IF EMPTY(Group) AND (lnRpPikSep > 0) AND llRpCond
    SET FILTER TO
  ENDIF
  *B803138,1 Automatically Allocate separates also. [End  ]

ENDSCAN
=gfThermo(lnTotRec , lnTotRec , "Allocating..." , "Order number: " + '')
*-- end of lfDyeAlo.

*!*************************************************************
*! Name      : lfCalAval
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 09/10/1998
*! Purpose   : Calculate availability.
*!*************************************************************
*! Called from : lfDyeAlo
*!*************************************************************
*! Calls       : lfEvalWip
*!*************************************************************
*! Passed Parameters : .T. to cal. avaialability,.T. if unconditional
*!*************************************************************
*! Return      : .T. or .F.
*!*************************************************************
*! Example     : =lfCalAval(.T.,.F.)
*!*************************************************************
*E300989,1
*
FUNCTION lfCalAval
PARAMETERS llCalAval,llUnCond

*B603691,1 Declare variables used to check if user change allocation  [Begin]
IF lnProcStep = 4
  PRIVATE laPOAlo
  DECLARE laPOAlo[8]
  SCATTER TO laPOAlo FIELDS PoAlo1 , PoAlo2 , PoAlo3 , PoAlo4 ,;
                            PoAlo5 , PoAlo6 , PoAlo7 , PoAlo8
ENDIF
*B603691,1 Declare variables used to check if user change allocation  [End  ]

*C200084,1 Define Some Variables serve allocation [Begin]
PRIVATE laOnHand , laWanted , laAllocPer
DIMENSION laOnHand[8] , laWanted[8] , laAllocPer[8]
STORE 0 TO laOnHand , laWanted
laAllocPer = 1
*C200084,1 Define Some Variables serve allocation [End  ]

PRIVATE llForceIt,lnMinCoPer
*-- llForceIt : Force allocation.
*-- lnMinCoPer: Minium percentage

lnMinCoPer = IIF(TYPE('lnRpPikCor')<>'N',0,lnRpPikCor)
llForceIt = llUnCond OR llRpForAlo

*-- Rise flag if you cal. wips for this style before. [1
llCalWip = (lcCurStyWr = Style + cWareCode)

*-- if not to force allocation and Style/Location was changed and include WIP.
IF !llForceIt AND !(STYLE+CWARECODE == lcOldSWare) AND llRpIncWip AND !llCalWip
  =lfEvalWip(Style,lcAllWare)  && evaluate wips (laWip Array)
ELSE
  IF !llRpIncWip
    laWip = 0
  ENDIF
ENDIF  && end if not to force allocation and Style/Location was changed and include WIP.

*-- Rise flag if you cal. wips for this style before. [2
lcCurStyWr = Style + cWareCode

*-- if not to force allocation.
IF !llForceIt AND llCalAval
  
  PRIVATE lnAvlFAlo
  lnAvlFAlo = 0
  llHavePik = llUnCond OR (EMPTY(DYELOT) OR (lcStartDye == DYELOT))
  *-- Note that :-
  *-- if it's the same Style/Location [Stydye Record is the previous one]
  *-- calcualte avaliability from array else from master record in stydye.
 
  IF ASCAN(laEvntTrig,PADR("ALLOCATE",10)) <> 0
    =gfDoTriger('ALAUTAL','ALLOCATE')
  ENDIF  

  laOpnStock[9] = 0                         
  
  FOR lnI = 1 TO 8

    lcI = STR(lnI,1)
    IF STYLE+CWARECODE==lcOldSWare
      laOnHand[lnI] = laOpnStock[lnI]
    ELSE
      laOnHand[lnI] = ROUND((STYDYE.Stk&lcI + laWip[lnI]-STYDYE.Alo&lcI-;
                             lnRpCutUnt)*laAllocPer[lnI],0)
    ENDIF

    laWanted[lnI]   = MAX(IIF(lcRpScpMod $ 'KP',CUT&lcI,QTY&lcI) -;
                      ExcCut&lcI-IIF(llHavePik,PIK&lcI,0),0)
  
    lnAvlFAlo       = lnAvlFAlo + MAX(MIN(laOnHand[lnI],laWanted[lnI]),0)

    laOpnStock[lnI] = laOnHand[lnI] - MAX(MIN(laOnHand[lnI],laWanted[lnI]),0)
    laOpnStock[9]   = laOpnStock[9] + laOpnStock[lnI]
    
  ENDFOR
  *C200084,1 Fresh produces custom proram Distribute allocation [End  ]

  lnTotReq  = IIF(lcRpScpMod $ 'KP',TotCut,TotQty) - TotExcCut -;
              IIF(llHavePik,TotPik,0) 
  llCalAval = lnAvlFAlo <> 0 .AND. (lnAvlFAlo/lnTotReq >= lnMinCoPer/100)

ENDIF    && end if not to force allocation.

*-- if you have available quantity to allocate this line.
IF llCalAval

  *-- if user force allocation or allocate unconditionally. [Begin]
  IF llForceIt
    laOpnStock[9] = 0
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)

      *-- if Force allocation.
      IF llRpForAlo
        IF lcRpScpMod$'KP'
          laOpnStock[lnI] = MAX((Cut&lcI-Pik&lcI-ExcCut&lcI),0)
        ELSE
          laOpnStock[lnI] = MAX((Qty&lcI-Pik&lcI-ExcCut&lcI),0)
        ENDIF  
      
      ELSE  && if Unconditionally allocation.
      
        IF lcRpScpMod$'KP'
          laOpnStock[lnI] = MAX(MIN(STYDYE.Stk&lcI-STYDYE.Alo&lcI,;
                                    Cut&lcI-Pik&lcI-ExcCut&lcI),0)
        ELSE
          laOpnStock[lnI] = MAX(MIN(STYDYE.Stk&lcI-STYDYE.Alo&lcI,;
                                    Qty&lcI-Pik&lcI-ExcCut&lcI),0)
        ENDIF                            
        
        *MAB HERE FPMAIN
        laOpnStock[lnI] = ROUND(laOpnStock[lnI] * laAllocPer[lnI],0)
        
      ENDIF                            
      laOpnStock[9]   = laOpnStock[9] + laOpnStock[lnI]
    ENDFOR
  ENDIF
  *-- if user force allocation or allocate unconditionally. [End  ]
    
  =RLOCK()
  *-- Temp. Allocate this quantity.
  
  REPLACE PoAlo1    WITH IIF(llForceIt,laOpnStock[1],laOnHand[1]-laOpnStock[1]) ,;
          PoAlo2    WITH IIF(llForceIt,laOpnStock[2],laOnHand[2]-laOpnStock[2]) ,;
          PoAlo3    WITH IIF(llForceIt,laOpnStock[3],laOnHand[3]-laOpnStock[3]) ,;
          PoAlo4    WITH IIF(llForceIt,laOpnStock[4],laOnHand[4]-laOpnStock[4]) ,;
          PoAlo5    WITH IIF(llForceIt,laOpnStock[5],laOnHand[5]-laOpnStock[5]) ,;
          PoAlo6    WITH IIF(llForceIt,laOpnStock[6],laOnHand[6]-laOpnStock[6]) ,;
          PoAlo7    WITH IIF(llForceIt,laOpnStock[7],laOnHand[7]-laOpnStock[7]) ,;
          PoAlo8    WITH IIF(llForceIt,laOpnStock[8],laOnHand[8]-laOpnStock[8]) ,;
          Tot_PoAlo WITH PoAlo1+PoAlo2+PoAlo3+PoAlo4+PoAlo5+PoAlo6+PoAlo7+PoAlo8
  *C200084,1 Fresh produces custom proram Distribute allocation [Begin]

  UNLOCK

ELSE

  IF !llForceIt
  
    IF ASCAN(laEvntTrig,PADR("ADJLINE",10)) <> 0
      =gfDoTriger('ALAUTAL','ADJLINE')
    ENDIF  

  ENDIF

ENDIF  

*B603691,1 Check if user change allocation when he allocates uncondtional [Begin]
*-- This Section will be used if unconditional allocation and it is step 4 only. 
IF llCalAval AND (lnProcStep = 4)
  llCalAval = (PoAlo1+PoAlo2+PoAlo3+PoAlo4+PoAlo5+PoAlo6+PoAlo7+PoAlo8 > 0)

  IF llCalAval
    llCalAval = (PoAlo1 <> laPOAlo[1]) OR (PoAlo2 <> laPOAlo[2]) OR ;
                (PoAlo3 <> laPOAlo[3]) OR (PoAlo4 <> laPOAlo[4]) OR ;
                (PoAlo5 <> laPOAlo[5]) OR (PoAlo6 <> laPOAlo[6]) OR ;
                (PoAlo7 <> laPOAlo[7]) OR (PoAlo8 <> laPOAlo[8])
  ENDIF

  IF !llCalAval
    GATHER FROM laPOAlo FIELDS PoAlo1 , PoAlo2 , PoAlo3 , PoAlo4 ,;
                               PoAlo5 , PoAlo6 , PoAlo7 , PoAlo8
  ENDIF

ENDIF
*B603691,1 Check if user change allocation when he allocates uncondtional [End  ]

RETURN llCalAval
*-- end of lfCalAval.

*!*************************************************************
*! Name      : lfAlocMast
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 09/10/1998
*! Purpose   : Allocate to master files.
*!*************************************************************
*! Called from : lfDyeAlo
*!*************************************************************
*! Calls       : lfRelQty , lfAloQty
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfAlocMast()
*!*************************************************************
*E300989,1
*
FUNCTION lfAlocMast
*-- if This line is allocated before to another dyelot
*-- release this allocation, because the same group must be allocated
*-- to the same dyelot.
IF !EMPTY(DYELOT) AND (STYDYE.DYELOT # DYELOT) AND TOTPIK <> 0
  = lfRelQty()  && release allocation.            
ENDIF
            
*-- replace current dyelot field with selected dyelot.
=RLOCK()
REPLACE Dyelot  WITH STYDYE.DYELOT,;
        nProcNo WITH 0
UNLOCK
=lfAloQty(lcAllWare,.T.)  && Allocate to the desired location.
*-- end of lfAlocMast.
          
*!*************************************************************
*! Name      : lfMarkLine
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 09/10/1998
*! Purpose   : Mark current record because it's allocated before
*!           : then increment allocation variables.
*!*************************************************************
*! Called from : lfDyeAlo
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfMarkLine()
*!*************************************************************
*E300989,1
*
FUNCTION lfMarkLine
*
REPLACE llSel WITH .T.
lnSelRec = lnSelRec + 1
lnAloRec = lnAloRec + 1
lnSelAlo = lnSelAlo + 1
lnCurent = lnCurent + 1
*-- end of lfMarkLine.

*!*************************************************************
*! Name      : lfAnother
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 09/10/1998
*! Purpose   : Get another closest dyelot, to current dyelot.
*!*************************************************************
*! Called from : lfDyeAlo
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : Fabric/Color Pair
*!*************************************************************
*! Return      : .T. if you find another dyelot.
*!*************************************************************
*! Example     : =lfAnother()
*!*************************************************************
*E300989,1
*
FUNCTION lfAnother
PARAMETERS lcFabClrPr

*-- if no pegged dyelot Start with Top of Fabric/Color group,
*-- and step is always 1 and forward.
IF llPitch OR EMPTY(lcPegged)
  
  IF EMPTY(lcPegged)
    lnPitch = 1
  ENDIF
  
  SKIP lnPitch IN DYE_REL
  IF (lnPitch = 1 AND EOF('DYE_REL')) OR BOF('DYE_REL') OR ;
     (DYE_REL.FABRIC+DYE_REL.COLOR # lcFabClrPr)        OR ;
     (DYE_REL.DYELOT = lcStartDye)
    
    llPitch = .F.
    RETURN .F.
  
  ENDIF     
            
ELSE  && else there is pegged dyelot then Start from within Fabric/Color group
  

  IF lcLastSkip = 'F'
    
    SKIP -lnSkipTo IN DYE_REL
    lcLastSkip = 'B' 
    IF BOF('DYE_REL') OR ;
     (DYE_REL.FABRIC+DYE_REL.COLOR # lcFabClrPr) OR ;
     (DYE_REL.DYELOT = lcStartDye)
      
      SKIP lnSkipTo IN DYE_REL
      IF EOF('DYE_REL') OR ;
       (DYE_REL.FABRIC+DYE_REL.COLOR # lcFabClrPr) OR ;
       (DYE_REL.DYELOT = lcStartDye)

        RETURN .F.
      
      ELSE
        
        llPitch = .T.
        lnPitch = 1
      
      ENDIF 
    
    ENDIF 
    
  ELSE
  
    SKIP lnSkipTo IN DYE_REL
    lcLastSkip = 'F' 
    IF EOF('DYE_REL') OR ;
     (DYE_REL.FABRIC+DYE_REL.COLOR # lcFabClrPr) OR ;
     (DYE_REL.DYELOT = lcStartDye)
      
      SKIP -(lnSkipTo+1) IN DYE_REL
      IF BOF('DYE_REL') OR ;
       (DYE_REL.FABRIC+DYE_REL.COLOR # lcFabClrPr) OR ;
       (DYE_REL.DYELOT = lcStartDye)

        RETURN .F.
      
      ELSE
        
        llPitch = .T.
        lnPitch = -1
      
      ENDIF 
    
    ENDIF 
  
  ENDIF
  lnSkipTo = lnSkipTo + 1
ENDIF
RETURN .T.

*-- end of lfAnother.

*!*************************************************************
*! Name      : lfEvalWip
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 09/10/1998
*! Purpose   : Evaluate wip to add it to availablity if user want.
*!*************************************************************
*! Called from : lfCalAval
*!*************************************************************
*! Calls       : lfKPWip
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfEvalWip()
*!*************************************************************
*E300989,1
*
FUNCTION lfEvalWip
PARAMETERS lcWipSty,lcWipWare
PRIVATE lcCurAlias,lcStyDyRec

lcCurAlias = SELECT(0)

laWip = 0

*-- if select By C/T or P/O (Wips for selected only)
IF lcRpScpMod $ 'KP' AND !EMPTY(laIncExprs[1,6])

  = lfKPWip(lcWipSty,lcWipWare,'laIncExprs',1)

  laWip[9] = laWip[1]+laWip[2]+laWip[3]+laWip[4]+;
             laWip[5]+laWip[6]+laWip[7]+laWip[8]

  IF laWip[9] < 0
    laWip = 0
    RETURN
  ENDIF             

ELSE

  SELECT STYDYE
  lcStyDyRec = STYLE+CWARECODE+DYELOT  && Save current stydye record.
  =SEEK(lcWipSty+lcWipWare+SPACE(10))  && Seek master location record.
  SCATTER FIELDS WIP1,WIP2,WIP3,WIP4,WIP5,WIP6,WIP7,WIP8,TOTWIP TO laWip
  =SEEK(lcStyDyRec)                    && Restore current stydye record.
  
ENDIF

*-- if exclude some records(C/t or P/o)
IF (laWip[9] > 0) AND llExclude AND (lcRpExSlct $ 'KP') AND;
   !EMPTY(laFiltExp[1,6])
  = lfKPWip(lcWipSty,lcWipWare,'laFiltExp',-1)
ENDIF

laWip[9] = laWip[1]+laWip[2]+laWip[3]+laWip[4]+;
           laWip[5]+laWip[6]+laWip[7]+laWip[8]

SELECT (lcTmpOrdLn)
=RLOCK()
REPLACE TOTWIP WITH laWip[9]
UNLOCK
SELECT (lcCurAlias)
*-- end of lfEvalWip.

*!*************************************************************
*! Name      : lfKPWip
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 09/10/1998
*! Purpose   : Evaluate wip C/t or P/o Cases.
*!*************************************************************
*! Called from : lfEvalWip
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfKPWip()
*!*************************************************************
*E300989,1
*
FUNCTION lfKPWip
PARAMETERS lcWipSty,lcWipWare,lcFiltArr,lnCalNo

PRIVATE lcLineFile,lcKeyExpr,lcKey,lnSelct
lnSelct = SELECT(0)

lcLineFile = IIF(lcRpScpMod='P','POSLn','CutTktL')
lcKeyExpr = IIF(lcRpScpMod='P','style+cstytype+po+STR(lineno,6)+trancd',;
                               'style+cuttkt+trancd')
SELECT (lcLineFile)
lcJ = IIF(lnCalNo=1,'1','2')

FOR lnStrPos = 1 TO ALEN(laString&lcJ,1)
  lcKey = lcWipSty + IIF(lcRpScpMod='P','P','')+laString&lcJ[lnStrPos]
  IF SEEK(lcKey)
    SCAN REST WHILE &lcKeyExpr = lcKey;
         FOR (cWareCode = lcWipWare)

      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        laWip[lnI] = laWip[lnI] + IIF(Trancd='1',Qty&lcI,-1*Qty&lcI) * lnCalNo
      ENDFOR  
    
    ENDSCAN     
  ENDIF
ENDFOR
SELECT (lnSelct)
*-- end of lfKPWip.

*!*************************************************************
*! Name      : lfUpdAloVr
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 04/07/1999
*! Purpose   : Update allocation variables at start.
*!*************************************************************
*! Called from : ....
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfUpdAloVr()
*!*************************************************************
*E300989,1   B602570,1   B602569,1
*
FUNCTION lfUpdAloVr
PRIVATE lnCurSelAl

*C200084,1 Evaluate adding this line or cancel it and (Deal with stores) [Begin]
*-- If it is FP picked line logic
IF (ASCAN(laEvntTrig,PADR("AUALOSTORE",10)) <> 0)
  lcMangProc = "EVALLINE"  && Parameter passed to allocate by store function.
  IF !gfDoTriger('ALAUTAL','AUALOSTORE')
    RETURN
  ENDIF  
ENDIF
*C200084,1 [End  ]

lnCurSelAl = SELECT(0)
= lfFillMVar() &&Get memory variables that will add to temp. file
INSERT INTO &lcTmpOrdLn FROM MEMVAR

SELECT (lcTmpOrdLn)
REPLACE cSortField WITH EVALUATE(lcIndexExp),llSel WITH Picked
            
IF Picked
  lnAloRec = lnAloRec + 1
  lnSelAlo = lnSelAlo + 1
  lnSelRec = lnSelRec + 1
ENDIF  

*B602569,1 if it's full allocation for previous ones and [Begin]
*          total pick quantity less than total order quantity
IF !llPartAlo AND TOTPIK < TOTQTY
  llPartAlo = .T.
ENDIF
*B602569,1 if it's full allocation for previous ones and [End  ]

*B602570,1 All previous allocated records must be selected by default [End  ]
SELECT (lnCurSelAl)
*-- end of lfUpdAloVr.

*!*************************************************************
*! Name      : lfvSortBy
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/06/1999
*! Purpose   : All Sort by validations
*!*************************************************************
*! Passed Parameters : Sort Number (1,2,3, Or 4)
*!*************************************************************
*! Return      : ....
*!*************************************************************
*!C200084,1
*!
FUNCTION lfvSortBy
PARAMETERS lnSortItem
PRIVATE lcObjName , lnObjVal , lcObjGet , llOldValue , lnI , lcItmObj
llOldValue = .F.
lcObjGet  = SYS(18)
lcObjName = "lnRpSort" + STR(lnSortItem,1)
lnObjVal  = EVALUATE(lcObjName)

lnI = 0
IF lnObjVal = 6
  FOR lnI = lnSortItem + 1 TO 4
  
    lcItmObj = "lnRpSort" + STR(lnI,1)
    IF EVALUATE(lcItmObj) <> 6
      llOldValue = .T.
      EXIT 
    ENDIF
  
  ENDFOR

ELSE
  
  IF lnSortItem > 2
    FOR lnI = lnSortItem-1 TO 2 STEP -1
  
      lcItmObj = "lnRpSort" + STR(lnI,1)
      IF EVALUATE(lcItmObj) = 6
        llOldValue = .T.
        EXIT 
      ENDIF
  
    ENDFOR
  ENDIF
  
ENDIF
llOldValue = IIF(llOldValue,llOldValue,;
                (lnObjVal<> 6) AND (ASCAN(laSortAry,lnObjVal) > 0))

IF llOldValue
  *-- Restore old values.
  STORE laOldVal TO &lcObjName , &lcObjGet 
  SHOW GET &lcObjGet
ELSE
  *-- Sort By Arrays make Sort Index.
  laSortAry[lnSortItem,1] = lnObjVal
  laSortAry[lnSortItem,2] = laIndexExp[lnObjVal]
ENDIF

*C200084,1 Save Sort Array to memory file. [Begin]
*B603457,1 KHM 02/16/2000 (Begin) The following lines are commented out.
*IF ASCAN(laEvntTrig,PADR("SAVESORT",10)) <> 0
*  = gfDoTriger('ALAUTAL','SAVESORT')
*ENDIF  
*B603457,1 KHM 02/16/2000 (End)
*C200084,1 Save Sort Array to memory file. [End  ]
*-- end of lfvSortBy.

*!*************************************************************
*! Name      : lfEvalIndx
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/07/1999
*! Purpose   : Create index expression
*!*************************************************************
*! Return      : Index expression.
*!*************************************************************
*
FUNCTION lfEvalIndx
PRIVATE lnI , lcIndExpr , llHaveOrd
llHaveOrd = .F.
lnI = 0
lcIndExpr = ''
FOR lnI = 1 TO 4
  IF EMPTY(laSortAry[lnI,2])
    EXIT
  ENDIF
  IF laSortAry[lnI,2] = [ORDER]
    llHaveOrd = .T.
  ENDIF
  IF !EMPTY(lcIndExpr)
    lcIndExpr = lcIndExpr + '+'
  ENDIF
  lcIndExpr = lcIndExpr + laSortAry[lnI,2]
ENDFOR
lcIndExpr = lcIndExpr + IIF(llHaveOrd,'',[+ ORDER ])
IF llRpExlDye OR !llFabDye
  lcIndExpr = lcIndExpr + [+STORE+GROUP+STR(LINENO , 6)]
ELSE
  lcIndExpr = lcIndExpr + [+STORE+FABRIC+CFABCOLOR+GROUP+STYLE+CWARECODE]
ENDIF
RETURN lcIndExpr
*-- end of lfEvalIndx.

*!*************************************************************
*! Name      : lfItmPos
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/98
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- end of lfItmPos.

*!*************************************************************
*! Name      : lfExclRec
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/98
*! Purpose   : Exclude this line.
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*!
FUNCTION lfExclRec

*C200084,1 Evaluate delete this line or cancel it and (Deal with stores) [Begin]
IF ASCAN(laEvntTrig,PADR("AUALOSTORE",10)) <> 0
  lcMangProc = "DELESTORE"  && Parameter passed to allocate by store function.
  =gfDoTriger('ALAUTAL','AUALOSTORE')
ENDIF  
*C200084,1 [End  ]

*B602570,1 All previous allocated records must be selected by default [Begin]
*B602570,1 then if you want to delete this record decrement record no. variables
IF Picked
  lnAloRec  = lnAloRec - 1
  lnSelAlo  = lnSelAlo - 1
ENDIF  
*B602570,1 All previous allocated records must be selected by default [End  ]
            
lnDellRec = lnDellRec + 1
DELETE
*E300989,1 New code for exclude mode . [end]
*-- end of lfExclRec.

*!*************************************************************
*! Name      : lfHandlSel
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/98
*! Purpose   : Handle Select / UnSelect Cases.
*!*************************************************************
*! Passed Parameters  : Apply Method
*!*************************************************************
*!
FUNCTION lfHandlSel
PARAMETERS lcApplyMth
*-- Select / UnSelect Record.
IF lcApplyMth = "SEL_UNSEL"
  
  =lfSelUnSel()

ELSE  && Sel All, Sel None, Or Invert
  PRIVATE lcScanExpr
  DO CASE
    
    CASE lcApplyMth = "SEL_ALL"  && Select All Case
      lcScanExpr = [FOR !llSel]
      
    CASE lcApplyMth = "SEL_NON"  && Select None Case
      lcScanExpr = [FOR llSel]

    CASE lcApplyMth = "INVERT"  && Invert Case
      lcScanExpr = []
    
  ENDCASE
  SCAN &lcScanExpr
    =lfSelUnSel()
  ENDSCAN  
  GO lnBrRecNo
ENDIF
=lfvpbSel()
SHOW WINDOW (lcOrdLBrow) REFRESH

lcSelAllSt = IIF(lnSelRec = RECCOUNT() - lnDellRec , 'DISABLE' , 'ENABLE')
lcSelNonSt = IIF(lnSelRec = 0 , 'DISABLE' , 'ENABLE')
lcAloSt = lcSelNonSt
lcRelSt = IIF(lnSelAlo = 0 , 'DISABLE' , 'ENABLE')

SHOW GET pbSelAll &lcSelAllSt
SHOW GET pbSelNon &lcSelNonSt
SHOW GET pbAlo &lcAloSt
SHOW GET pbRel &lcRelSt
SHOW GET pbGen &lcRelSt

*IF The current record is Selected
IF llSel
  =lfShowGets(.T.)
ELSE    && Else
  lcCh3Stat = 'DISABLE'
  laPikSt   = 'DISABLE'
  SHOW GETS WINDOW (lcAutAlCh3) DISABLE ONLY
ENDIF    && End of IF
*-- end of lfHandlSel.

*!*************************************************************
*! Name      : lfSelUnSel
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/98
*! Purpose   : Select / UnSelect Record.
*!*************************************************************
*!
FUNCTION lfSelUnSel
REPLACE llSel WITH !llSel

*C200084,1 Keep track of Select / Unselect [Begin]
IF ASCAN(laEvntTrig,PADR("ALLOCSELCT",10)) <> 0
  = gfDoTriger('ALAUTAL','ALLOCSELCT')
ENDIF  
*C200084,1 Keep track of Select / Unselect [End  ]

*-- if it is now selected record.
IF llSel
  lnSelRec = lnSelRec + 1
  lnSelAlo = IIF(TOTPIK = 0 , lnSelAlo , lnSelAlo + 1)
ELSE  && else if it is now unselected record.
  lnSelRec = lnSelRec - 1
  lnSelAlo = IIF(TOTPIK = 0 , lnSelAlo , lnSelAlo - 1)
ENDIF
*-- end of lfSelUnSel.

*!*************************************************************
*! Name      : lfSuppForc
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/98
*! Purpose   : Suppress Force allocation. (From logic grid...)
*!*************************************************************
*!
FUNCTION lfSuppForc

llAlwForce = .T.
IF lcAlwForce <> "Y"
  *-- No Force allocation done.
  IF lcAlwForce = "N"
    llRpForAlo = .F.
    llAlwForce = .F.  && Suppress line.

  ELSE  && User Prev.
    *-- Call user defined process.  
    llAlwForce = gfUserPriv('AL','ALAUTAL','FORCING')

  ENDIF
ENDIF
RETURN llAlwForce
*-- end of lfSuppForc.

*!*************************************************************
*! Name      : lfForceMe
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/01/99
*! Purpose   : Force allocation. (Manual Force)
*!*************************************************************
*E301225,1
*!
FUNCTION lfForceMe
IF llForceAll
  RETURN
ENDIF

PRIVATE lnOption, llRetVal , lcButton , lcMsgType , lcOptMsg
lnOption = 1 

IF llAlwForce
  lcButton  = "44002"
  lcOptMsg  = "Do you want to force allocation?"
  lcMsgType = "QR"
ELSE
  lcButton = "00000"
  lcOptMsg = " "
  lcMsgType = "TR"
ENDIF
  
lnOption = gfModalGen(lcMsgType+'M44000B'+lcButton, 'DIALOG',;
           'The allocated quantity|the available quantity'+;
           "|"+lcOptMsg)

IF !llAlwForce
  lnOption = 3
ENDIF

llForceAll = (lnOption = 2) && Avoiding entering this function again.
llRetVal   = (lnOption < 3)
RETURN llRetVal
*-- end of lfForceMe.

*!*************************************************************
*! Name      : lfSrSty1
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : Style In Range Filter. 1
*!*************************************************************
*B802267,1
*!
FUNCTION lfSrSty1
PARAMETERS lcParm
=lfSRStyle(lcParm,"1")
*-- end of lfSrSty1.

*!*************************************************************
*! Name      : lfSrSty2
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : Style In Range Filter. 2
*!*************************************************************
*B802267,1
*!
FUNCTION lfSrSty2
PARAMETERS lcParm
=lfSRStyle(lcParm,"2")
*-- end of lfSrSty2.

*!*************************************************************
*! Name      : lfSRStyle
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : Style In Range Filter.
*!*************************************************************
*B802267,1
*!
FUNCTION lfSRStyle
PARAMETERS lcParm,lcFlagNo
DO CASE
  CASE lcParm = 'S'  && Set code
    lnRngAlias = SELECT(0)
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClrSty&lcFlagNo = .F.
    SELECT (lnRngAlias)
ENDCASE
*-- end of lfSRStyle.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*B802267,1
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) = ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  
  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- end of lfStySum.

*!*************************************************************
*! Name      : lfSrOrd1
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : Order In Range Filter. 1
*!*************************************************************
*B802267,1
*!
FUNCTION lfSrOrd1
PARAMETERS lcParm
=lfSROrder(lcParm,"1")
*-- end of lfSrOrd1.

*!*************************************************************
*! Name      : lfSrOrd2
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : Order In Range Filter. 2
*!*************************************************************
*B802267,1
*!
FUNCTION lfSrOrd2
PARAMETERS lcParm
=lfSROrder(lcParm,"2")
*-- end of lfSrOrd2.

*!*************************************************************
*! Name      : lfSROrder
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : Order In Range
*!*************************************************************
*B802267,1
FUNCTION lfSROrder
PARAMETERS lcParm,lcFlagNo
DO CASE
  CASE lcParm = 'S'

    lnRngAlias = SELECT(0)
    SELECT ORDHDR
    lcCustRel = [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)]

    SET ORDER TO Customer IN Customer
    SET RELATION TO &lcCustRel INTO CUSTOMER && To customer file.
    GO TOP
  
  CASE lcParm = 'R'
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER && To customer file.
    llClrOrd&lcFlagNo = .F.
    SELECT (lnRngAlias)

ENDCASE
*-- end of lfSROrder.

*!*************************************************************
*! Name      : lfsrAcc1
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : Account In Range Filter. 1
*!*************************************************************
*B802267,1
*!
FUNCTION lfsrAcc1
PARAMETERS lcParm
=lfsrAcc(lcParm,"1")
*-- end of lfsrAcc1.

*!*************************************************************
*! Name      : lfsrAcc2
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : Account In Range Filter. 2
*!*************************************************************
*B802267,1
*!
FUNCTION lfsrAcc2
PARAMETERS lcParm
=lfsrAcc(lcParm,"2")
*-- end of lfsrAcc2.

*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : Account In Range
*!*************************************************************
*B802267,1
FUNCTION lfsrAcc
PARAMETERS lcParm,lcFlagNo
DO CASE
  CASE lcParm = 'S'
    lnRngAlias = SELECT(0)
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    llClrAcc&lcFlagNo = .F.
    SELECT (lnRngAlias)
ENDCASE
*-- end of lfsrAcc.

*!*************************************************************
*! Name      : lfsrPO1
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : PO In Range Filter. 1
*!*************************************************************
*B802267,1
*!
FUNCTION lfsrPO1
PARAMETERS lcParm
=lfsrPO(lcParm,"1")
*-- end of lfsrPO1.

*!*************************************************************
*! Name      : lfsrPO2
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : PO In Range Filter. 2
*!*************************************************************
*B802267,1
*!
FUNCTION lfsrPO2
PARAMETERS lcParm
=lfsrPO(lcParm,"2")
*-- end of lfsrPO2.

*!*************************************************************
*! Name      : lfsrPO
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : P/O In Range
*!*************************************************************
*B802267,1
FUNCTION lfsrPO
PARAMETERS lcParm,lcFlagNo
DO CASE
  CASE lcParm = 'S'
    lnRngAlias = SELECT(0)
    SET ORDER TO VENCODE IN APVENDOR
    SELECT POSHDR
    SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE
  CASE lcParm = 'R'
    SELECT POSHDR
    SET RELATION OFF INTO APVENDOR
    llClrPO&lcFlagNo = .F.
    SELECT (lnRngAlias)
ENDCASE
*-- end of lfsrPO.


*!*************************************************************
*! Name      : lfrCT1
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : CT In Range Filter. 1
*!*************************************************************
*B802267,1
*!
FUNCTION lfrCT1
PARAMETERS lcParm
=lfrCT(lcParm,"1")
*-- end of lfrCT1.

*!*************************************************************
*! Name      : lfrCT2
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : CT In Range Filter. 2
*!*************************************************************
*B802267,1
*!
FUNCTION lfrCT2
PARAMETERS lcParm
=lfrCT(lcParm,"2")
*-- end of lfrCT2.

*!*************************************************************
*! Name      : lfrCT
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : C/T In Range
*!*************************************************************
*B802267,1
FUNCTION lfrCT
PARAMETERS lcParm,lcFlagNo
llClrCT&lcFlagNo = .F.
*-- end of lfrCT.


*!*************************************************************
*! Name      : lfvIncHold
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : Include Hold Orders.
*!*************************************************************
*B802267,1
FUNCTION lfvIncHold
lcSOrdStat = IIF(llRpIncHor,"OH","O")
*-- end of lfvIncHold.

*!*************************************************************
*! Name      : lfvExlBulk
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : Exclude bulk order Orders.
*!*************************************************************
*B802267,1
FUNCTION lfvExlBulk
lcBulkExp = IIF(llRpExlBlk,""," AND BULK='N'")
*-- end of lfvExlBulk.

*!*************************************************************
*! Name      : lfUpdatSty
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : Update Select by Style.
*!*************************************************************
*B802267,1
FUNCTION lfUpdatSty
PARAMETERS llPointSty
PRIVATE lcScanExpr
IF llPointSty
  lcScanExpr = [REST WHILE Style= PADR(&lcOptmFile..cStyMajor,lnMajorLen)]
  =SEEK(PADR(&lcOptmFile..cStyMajor,lnMajorLen),"Style")
ELSE
  lcScanExpr = []
ENDIF

*SCAN Loop to scan the STYLE file FOR the Style filter expression
SELECT STYLE
SCAN &lcScanExpr FOR &lcForCond1

  *IF There is Order lines for this Style
  IF SEEK(STYLE , lcChildFil)
    =lfStyInLns()
  ENDIF  
ENDSCAN    && End of SCAN Loop
*-- end of lfUpdatSty.

*!*************************************************************
*! Name      : lfStyInLns
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : Scan style in lines file.
*!*************************************************************
*B802267,1
FUNCTION lfStyInLns
SELECT (lcChildFil)
*SCAN Loop to scan the lcChildFil file FOR the rest of the selectio
*criteria filter expression

SCAN REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = STYLE.STYLE ;
          FOR   &lcForCond2
  *E300989,1 if Exclude mode
  IF llExclude
    *E300989,1 New code for exclude mode . [begin]
    WAIT WINDOW NOWAIT 'Excluding style '+ Style
    =lfExclRec()
    *E300989,1 The following variables control enable and disable buttons.
         
  ELSE  && Scope mode
    WAIT WINDOW NOWAIT 'Selecting style '+ Style

    SCATTER MEMVAR MEMO
    =lfUpdAloVr()

  ENDIF
ENDSCAN    && End of SCAN Loop
SELECT STYLE
*-- end of lfStyInLns.


*!*************************************************************
*! Name      : lfUpdatOrd
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : Update Select by (Order/Account).
*!*************************************************************
*B802267,1
FUNCTION lfUpdatOrd
PARAMETERS llPointSty
PRIVATE lcScanExpr
IF llPointSty
  IF lcRpScpMod = 'O'
    =SEEK("O"+&lcOptmFile..Order,"OrdHdr")
    *lcScanExpr = [FOR cOrdType+Order="O"+&lcOptmFile..Order REST WHILE &lcForCond1]
    lcScanExpr = [REST WHILE cOrdType+Order="O"+&lcOptmFile..Order]
  ELSE
    IF SEEK(&lcOptmFile..Account+"O","OrdHdr")
      *lcScanExpr = [FOR account+cordtype+order=&lcOptmFile..Account+"O" REST WHILE &lcForCond1]
      lcScanExpr = [REST WHILE account+cordtype+order=&lcOptmFile..Account+"O"]
    ELSE
      RETURN
    ENDIF  
  ENDIF

ELSE

  *lcScanExpr = [FOR &lcForCond1]
  lcScanExpr = []
ENDIF

*SCAN Loop to scan the ORDHDR file FOR the Order header filter expression
SELECT ORDHDR

*B603544,4 HBG 06/04/2000 If we get the complete date from ORDLINE ,set relation between
*B603544,4                ORDHDR and ORDLINE [Begin]  
IF llLinCmplt
  SET RELATION TO cOrdType + Order INTO ORDLINE ADDITIVE
ENDIF
*B603544,4 HBG 06/04/2000 If we get the complete date from ORDLINE ,set relation between
*B603544,4                ORDHDR and ORDLINE [End  ]  

*C120733,1 ABD - Collect the data from the order line uner new condation.  [Begin]
IF ASCAN(laEvntTrig,PADR("GETORDLN",10)) <> 0
  =gfDoTriger('ALAUTAL','GETORDLN  ')
  RETURN
ENDIF
*C120733,1 ABD - [End]

*B606442,1 TMI [Start] set a relation between ordhdr and customer file to get only active customers
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER ADDITIVE
*B606442,1 TMI [End  ] 
SCAN &lcScanExpr FOR &lcForCond1

  IF SEEK('O' + Order , lcChildFil)
    SELECT (lcChildFil)
        
    *SCAN Loop to scan the (lcChildFil) file FOR the rest of the selection
    *criteria filter expression
    SCAN REST WHILE cordtype+order+STR(lineno,6) = "O"+ORDHDR.ORDER ;
          FOR &lcForCond2
      *E300989,1 if Exclude mode
      IF llExclude
        *E300989,1 New code for exclude mode . [begin]
        WAIT WINDOW NOWAIT 'Excluding ' + lcWaitStr + &lcWaitStr
        =lfExclRec()
        *E300989,1 New code for exclude mode . [end]
         
      ELSE  && Scope mode
        WAIT WINDOW NOWAIT 'Selecting ' + lcWaitStr + &lcWaitStr
            
        SCATTER MEMVAR MEMO
        =lfUpdAloVr()
            
      ENDIF
    ENDSCAN    && End of SCAN Loop
    SELECT ORDHDR
  ENDIF    && End of IF
ENDSCAN    && End of SCAN Loop

*B603544,4 HBG 06/04/2000 relase relation between ORDHDR and ORDLINE [Begin]  
IF llLinCmplt
  SET RELATION OFF INTO ORDLINE
ENDIF  
*B603544,4 HBG 06/04/2000 relase relation between ORDHDR and ORDLINE [End  ] 

*B606442,1 TMI [Start] release the relation 
SELECT ORDHDR
SET RELATION off INTO CUSTOMER             
*B606442,1 TMI [End  ] release the relation  
*-- end of lfUpdatOrd.

*!*************************************************************
*! Name      : lfUpdatPo
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : Update Select by (PO).
*!*************************************************************
*B802267,1
FUNCTION lfUpdatPo
PARAMETERS llPointSty
PRIVATE lcScanExpr
IF llPointSty
  lcScanExpr = [REST WHILE cstytype+po= "P"+&lcOptmFile..PO]
  =SEEK("P"+&lcOptmFile..PO,"PosHdr")
ELSE
  lcScanExpr = []
ENDIF

SELECT POSHDR
*B606442,1 TMI [Start] Set the needed relation based on Poshdr file to apply 
*B606442,1             the condition on customers(to get only active)
SET RELATION TO '2' + POSHDR.PO INTO CUTPICK ADDITIVE
SELECT CUTPICK
SET RELATION TO 'O'+CUTPICK.ORDER INTO ORDLINE ADDITIVE
SELECT ORDLINE
SET RELATION TO 'M'+ORDLINE.ACCOUNT INTO CUSTOMER ADDITIVE
SELECT POSHDR
*B119304,1 ALB can't allocate SO by selected PO "No Records Found" [Begin]
*LOCATE
*B119304,1 ALB can't allocate SO by selected PO "No Records Found" [end]
*B606442,1 TMI [End  ] 

*SCAN Loop to scan the POSHDR file FOR the PO filter expression
SCAN &lcScanExpr FOR &lcForCond1

  *E300989,1 Message is either Selecting or Excluding.
  lcWaitMsg = IIF(llExclude,'Excluding','Selecting')
  WAIT WINDOW NOWAIT lcWaitMsg + ' purchase order ' + PO
      
  *IF There is lines for this PO in the CUTPICK file
  IF SEEK('2' + PO , 'CUTPICK')
    SELECT CUTPICK
        
    *SCAN Loop to scan the CUTPICK file FOR the PO number
    SCAN REST WHILE TRANCD+CTKTNO = '2' + POSHDR.PO
      *E300989,1 if it's exclude case.
      IF llExclude
        *E300989,1 This new code for exclude condition. [Begin]
        IF SEEK('O' + Order + cOrdLine , lcTmpOrdLn)
          SELECT (lcTmpOrdLn)
          *B603544,4 HBG 07/11/2000 Scan for "lcForCond2" [Begin]  
          SCAN REST FOR &lcForCond2
          *B603544,4 HBG 07/11/2000 Scan for "lcForCond2" [End  ]  
            IF (TOTCUT - CUTPICK.TotQty) = 0
              =lfExclRec()
            ELSE

              *C200084,1 Evaluate delete this line or cancel it and (Deal with stores) [Begin]
              IF ASCAN(laEvntTrig,PADR("AUALOSTORE",10)) <> 0
                lcMangProc = "DELESTORE"  && Parameter passed to allocate by store function.
                =gfDoTriger('ALAUTAL','AUALOSTORE')
              ENDIF  
              *C200084,1 [End  ]

              REPLACE Cut1 WITH Cut1 - CUTPICK.Qty1 ,;
                      Cut2 WITH Cut2 - CUTPICK.Qty2 ,;
                      Cut3 WITH Cut3 - CUTPICK.Qty3 ,;
                      Cut4 WITH Cut4 - CUTPICK.Qty4 ,;
                      Cut5 WITH Cut5 - CUTPICK.Qty5 ,;
                      Cut6 WITH Cut6 - CUTPICK.Qty6 ,;
                      Cut7 WITH Cut7 - CUTPICK.Qty7 ,;
                      Cut8 WITH Cut8 - CUTPICK.Qty8 ,;
                   TotCut  WITH TotCut - CUTPICK.TotQty
 
              IF !(lcOldSlct $ 'KP')
                REPLACE ExcCut1 WITH ExcCut1 + CUTPICK.Qty1 ,;
                        ExcCut2 WITH ExcCut2 + CUTPICK.Qty2 ,;
                        ExcCut3 WITH ExcCut3 + CUTPICK.Qty3 ,;
                        ExcCut4 WITH ExcCut4 + CUTPICK.Qty4 ,;
                        ExcCut5 WITH ExcCut5 + CUTPICK.Qty5 ,;
                        ExcCut6 WITH ExcCut6 + CUTPICK.Qty6 ,;
                        ExcCut7 WITH ExcCut7 + CUTPICK.Qty7 ,;
                        ExcCut8 WITH ExcCut8 + CUTPICK.Qty8 ,;
                     TotExcCut  WITH TotExcCut + CUTPICK.TotQty
              ENDIF
  
            ENDIF
          *B603544,4 HBG 07/11/2000 Scan for "lcForCond2" [Begin]  
          ENDSCAN 
          *B603544,4 HBG 07/11/2000 Scan for "lcForCond2" [End  ]  
        ENDIF
        *E300989,1 This new code for exclude condition. [end]

      ELSE      && Normal scope case
           
        *IF There is no record for this CUTPICK order line in the Temp.
        *Order lines file
        IF !SEEK('O' + Order + cOrdLine , lcTmpOrdLn)
              
          *IF There is a record for this CUTPICK order line in the
          *ORDLINE file
          IF SEEK('O' + Order + cOrdLine , 'ORDLINE')
              
            *IF Statment to check the rest of the selection criteria
            *filter expression
              
            *E300989,1 Condition 2 is made for most child File, thus we must
            *E300989,1 select it before check, and then restore old alias.  
            lnCrSelect = SELECT(0)
            SELECT ORDLINE
            *E300989,1 Check for condition 2 after select alias.
            IF &lcForCond2
              *E300989,1 Comment following line and select alias before check.
              SCATTER MEMVAR MEMO
               
              M.Cut1 = CUTPICK.Qty1
              M.Cut2 = CUTPICK.Qty2
              M.Cut3 = CUTPICK.Qty3
              M.Cut4 = CUTPICK.Qty4
              M.Cut5 = CUTPICK.Qty5
              M.Cut6 = CUTPICK.Qty6
              M.Cut7 = CUTPICK.Qty7
              M.Cut8 = CUTPICK.Qty8
              M.TotCut = CUTPICK.TotQty

              =lfUpdAloVr()
                  
            ENDIF    && End of IF

            *E300989,1 Restore old alias.  
            SELECT (lnCrSelect)
            *E300989,1
            
          ENDIF    && End of IF

        ELSE    && Else
          SELECT (lcTmpOrdLn)
          REPLACE Cut1 WITH Cut1 + CUTPICK.Qty1 ,;
                  Cut2 WITH Cut2 + CUTPICK.Qty2 ,;
                  Cut3 WITH Cut3 + CUTPICK.Qty3 ,;
                  Cut4 WITH Cut4 + CUTPICK.Qty4 ,;
                  Cut5 WITH Cut5 + CUTPICK.Qty5 ,;
                  Cut6 WITH Cut6 + CUTPICK.Qty6 ,;
                  Cut7 WITH Cut7 + CUTPICK.Qty7 ,;
                  Cut8 WITH Cut8 + CUTPICK.Qty8 ,;
               TotCut  WITH TotCut + CUTPICK.TotQty
            
        ENDIF    && End of IF
      ENDIF      && end if llExclude
      SELECT CUTPICK
    ENDSCAN    && End of SCAN Loop
    SELECT POSHDR
  ENDIF    && End of IF
ENDSCAN    && End of SCAN Loop

*B606442,1 TMI [Start] release the seted relations
SELECT POSHDR
SET RELATION OFF INTO CUTPICK 
SELECT CUTPICK
SET RELATION OFF INTO ORDLINE
SELECT ORDLINE
SET RELATION OFF INTO CUSTOMER 
SELECT POSHDR
*B606442,1 TMI [End  ] 

*-- end of lfUpdatPo.

*!*************************************************************
*! Name      : lfUpdatCt
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : Update Select by (C/T).
*!*************************************************************
*B802267,1
FUNCTION lfUpdatCt
PARAMETERS llPointSty
PRIVATE lcScanExpr

*C037824,1  TMI [Start] comment and Copy these lines below to be befor the scan rest expression
*IF llPointSty
*  lcScanExpr = [REST WHILE cuttkt= &lcOptmFile..cuttkt]
*  =SEEK(&lcOptmFile..cuttkt,"CUTTKTH")
*ELSE
*  lcScanExpr = []
*ENDIF
*C037824,1  TMI [End  ] 

SELECT CUTTKTH
*B606442,1 TMI [Start] Set the needed relation based on the file cuttkth to 
*B606442,1             apply the condition on customers(to get only active)
SET RELATION TO '1' + CUTTKTH.CUTTKT INTO CUTPICK ADDITIVE
SELECT CUTPICK
SET RELATION TO 'O'+CUTPICK.ORDER INTO ORDLINE ADDITIVE
SELECT ORDLINE
SET RELATION TO 'M'+ORDLINE.ACCOUNT INTO CUSTOMER ADDITIVE
SELECT CUTTKTH
LOCATE
*B606442,1 TMI [End  ] 
*SCAN Loop to scan the CUTTKTH file FOR the Cut ticket header filter
*expression

*C037824,1  TMI [Start] Get the "lcScanExpr" value
IF llPointSty
  lcScanExpr = [REST WHILE cuttkt= &lcOptmFile..cuttkt]
  =SEEK(&lcOptmFile..cuttkt,"CUTTKTH")
ELSE
  lcScanExpr = []
ENDIF
*C037824,1  TMI [End  ] 

SCAN &lcScanExpr FOR &lcForCond1
  lcWaitMsg = IIF(llExclude,'Excluding','Selecting')
  WAIT WINDOW NOWAIT lcWaitMsg + ' cutting ticket ' + CutTkt
      
  *IF There is lines for this Cut ticket in the CUTPICK file
  IF SEEK('1' + CUTTKT , 'CUTPICK')    
    SELECT CUTPICK
        
    *SCAN Loop to scan the CUTPICK file FOR the Cut ticket number
    SCAN REST WHILE TRANCD+CTKTNO = '1' + CUTTKTH.CUTTKT 
   
      *E300989,1 if it's exclude case.
      IF llExclude
        *E300989,1 This new code for exclude condition. [Begin]
        IF SEEK('O' + Order + cOrdLine , lcTmpOrdLn)
          SELECT (lcTmpOrdLn)
          *B603544,4 HBG 07/11/2000 Scan for "lcForCond2" [Begin]  
          SCAN REST FOR &lcForCond2
          *B603544,4 HBG 07/11/2000 Scan for "lcForCond2" [End  ]  
            IF (TOTCUT - CUTPICK.TotQty) = 0
              =lfExclRec()
            ELSE
                
              *C200084,1 Evaluate delete this line or cancel it and (Deal with stores) [Begin]
              IF ASCAN(laEvntTrig,PADR("AUALOSTORE",10)) <> 0
                lcMangProc = "DELESTORE"  && Parameter passed to allocate by store function.
                =gfDoTriger('ALAUTAL','AUALOSTORE')
              ENDIF  
              *C200084,1 [End  ]
  
              REPLACE Cut1 WITH Cut1 - CUTPICK.Qty1 ,;
                      Cut2 WITH Cut2 - CUTPICK.Qty2 ,;
                      Cut3 WITH Cut3 - CUTPICK.Qty3 ,;
                      Cut4 WITH Cut4 - CUTPICK.Qty4 ,;
                      Cut5 WITH Cut5 - CUTPICK.Qty5 ,;
                      Cut6 WITH Cut6 - CUTPICK.Qty6 ,;
                      Cut7 WITH Cut7 - CUTPICK.Qty7 ,;
                      Cut8 WITH Cut8 - CUTPICK.Qty8 ,;
                   TotCut  WITH TotCut - CUTPICK.TotQty
                
              IF !(lcOldSlct $ 'KP')
                REPLACE ExcCut1 WITH ExcCut1 + CUTPICK.Qty1 ,;
                        ExcCut2 WITH ExcCut2 + CUTPICK.Qty2 ,;
                        ExcCut3 WITH ExcCut3 + CUTPICK.Qty3 ,;
                        ExcCut4 WITH ExcCut4 + CUTPICK.Qty4 ,;
                        ExcCut5 WITH ExcCut5 + CUTPICK.Qty5 ,;
                        ExcCut6 WITH ExcCut6 + CUTPICK.Qty6 ,;
                        ExcCut7 WITH ExcCut7 + CUTPICK.Qty7 ,;
                        ExcCut8 WITH ExcCut8 + CUTPICK.Qty8 ,;
                     TotExcCut  WITH TotExcCut + CUTPICK.TotQty
              ENDIF
                
            ENDIF
          *B603544,4 HBG 07/11/2000 Scan for "lcForCond2" [Begin]  
          ENDSCAN  
          *B603544,4 HBG 07/11/2000 Scan for "lcForCond2" [End ]  
        ENDIF
        *E300989,1 This new code for exclude condition. [end.]

      ELSE      && Normal scope case
        *IF There is no record for this CUTPICK order line in the Temp.
        *Order lines file
        IF !SEEK('O' + Order + cOrdLine , lcTmpOrdLn)
            
          *IF There is a record for this CUTPICK order line in the
          *ORDLINE file
            
          IF SEEK('O' + Order + cOrdLine , 'ORDLINE')
            SELECT ORDLINE
              
            *IF Statment to check the rest of the selection criteria
            *filter expression
            IF &lcForCond2
              SCATTER MEMVAR MEMO

              M.Cut1 = CUTPICK.Qty1
              M.Cut2 = CUTPICK.Qty2
              M.Cut3 = CUTPICK.Qty3
              M.Cut4 = CUTPICK.Qty4
              M.Cut5 = CUTPICK.Qty5
              M.Cut6 = CUTPICK.Qty6
              M.Cut7 = CUTPICK.Qty7
              M.Cut8 = CUTPICK.Qty8
              M.TotCut = CUTPICK.TotQty
                  
              =lfUpdAloVr()
                
            ENDIF    && End of IF
            *E300989,1 Select cutpick because it's master file in scan loop.
            SELECT CUTPICK
                
          ENDIF    && End of IF

        ELSE    && Else
          SELECT (lcTmpOrdLn)
          REPLACE Cut1 WITH Cut1 + CUTPICK.Qty1 ,;
                  Cut2 WITH Cut2 + CUTPICK.Qty2 ,;
                  Cut3 WITH Cut3 + CUTPICK.Qty3 ,;
                  Cut4 WITH Cut4 + CUTPICK.Qty4 ,;
                  Cut5 WITH Cut5 + CUTPICK.Qty5 ,;
                  Cut6 WITH Cut6 + CUTPICK.Qty6 ,;
                  Cut7 WITH Cut7 + CUTPICK.Qty7 ,;
                  Cut8 WITH Cut8 + CUTPICK.Qty8 ,;
               TotCut  WITH TotCut + CUTPICK.TotQty
            
        ENDIF    && End of IF
      ENDIF  && end if llExclude
      SELECT CUTPICK
    ENDSCAN    && End of SCAN Loop
    SELECT CUTTKTH
  ENDIF    && End of IF
ENDSCAN    && End of SCAN Loop

*B606442,1 TMI [Start] release the seted relations
SELECT CUTTKTH
SET RELATION OFF INTO CUTPICK
SELECT CUTPICK
SET RELATION OFF INTO ORDLINE
SELECT ORDLINE
SET RELATION OFF INTO CUSTOMER
SELECT CUTTKTH
*B606442,1 TMI [End  ] 

*-- end of lfUpdatCt.

*!*************************************************************
*! Name      : lfErsBldPk
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : Erase and Build temp. Pick file.
*!*************************************************************
*B603111,1
FUNCTION lfErsBldPk
PARAMETERS llBuild
PRIVATE lcCurrAls
lcCurrAls = SELECT(0)

*-- CLOSE piktkts generated in this session [Begin]
IF USED(lcTmpPkTk)
  USE IN (lcTmpPkTk)
ENDIF

IF FILE(gcWorkdir + lcTmpPkTk + '.DBF')
  ERASE &gcWorkdir.&lcTmpPkTk..DBF          && Erase the Temp file.
ENDIF

IF FILE(gcWorkdir + lcTmpPkTk + '.CDX')
  ERASE &gcWorkdir.&lcTmpPkTk..CDX          && Erase the Temp file.
ENDIF

IF llBuild
  CREATE TABLE (gcWorkdir + lcTmpPkTk) ;
      (Order  C(6), Store C(8), cWareCode C(6),PikTkt C(6),cLineNo C(6))
  ZAP
  INDEX ON Order + Store + cWareCode + PikTkt + cLineNo TAG (lcTmpPkTk) OF ;
           (gcWorkDir+lcTmpPkTk+'.CDX')
ENDIF
SELECT (lcCurrAls)
*-- end of lfErsBldPk.

*!*************************************************************
*! Name      : lfBldRel
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : Build temp. rele Pick alias.
*!*************************************************************
*B603111,1
FUNCTION lfBldRel
PARAMETERS lcRelPik
PRIVATE llFindLine , lcRelPik

IF !USED(lcRelLine)
  CREATE TABLE (gcWorkDir+lcRelLine) FROM ARRAY laFileStru
  SELECT (lcRelLine)
  ZAP
ENDIF

SELECT (lcTmpOrdLn)
SCATTER MEMVAR MEMO
INSERT INTO (lcRelLine) FROM MEMVAR

REPLACE PikTkt WITH SPACE(6)

IF !USED(lcTmpRelPk)
  USE (gcWorkDir+lcTmpOrdLn) AGAIN ALIAS (lcTmpRelPk) IN 0
ENDIF

SELECT (lcTmpRelPk)
IF FILE(gcWorkDir+lcTmpRelPk+'.CDX')
  SET ORDER TO TAG (lcTmpRelPk) OF (gcWorkDir+lcTmpRelPk+'.CDX')
ELSE
  INDEX ON PikTkt  TAG (lcTmpRelPk) OF (gcWorkDir+lcTmpRelPk+'.CDX')
ENDIF
             
llFindLine = SEEK(lcRelPik)

SELECT (lcTmpOrdLn)
REPLACE PikTkt WITH lcRelPik

RETURN !llFindLine
*-- end of lfBldRel.
*!******************************************************************************
*! Name      : lfChngDate
*! Developer : HEND GHANEM (HBG)
*! Date      : 06/06/2000
*! Purpose   : Check setup complete date by order line if it is yes 
*!           : replace ORDHDR by ORDLINE in the laFiltExp array else replace
*!           : ORDLINE by ORDHDR. Then replace ORDLINE by lcChildFil 
*!******************************************************************************
*B603544,4 
FUNCTION lfChngDate
PRIVATE lnCompDate

*-- if setup is complete date by order line replace ORDLINE & ORDHDR by lcChildFil in laFiltExp
*-- array, else replace ORDLINE by ORDHDR [Begin]
IF llLinCmplt
  lnCompDate = ASCAN(laFiltExp,'ORDLINE.COMPLETE')
  IF lnCompDate > 0
    laFiltExp[lnCompDate] = '&lcChildFil..COMPLETE'
  ENDIF
  lnCompDate = ASCAN(laFiltExp,'ORDHDR.COMPLETE')
  IF lnCompDate > 0
    laFiltExp[lnCompDate] = '&lcChildFil..COMPLETE'
  ENDIF
ELSE
  lnCompDate = ASCAN(laFiltExp,'ORDLINE.COMPLETE')
  IF lnCompDate > 0
    laFiltExp[lnCompDate] = 'ORDHDR.COMPLETE'
  ENDIF
ENDIF && if setup is complete date by order line [Begin]
*-- end of lfChngDate

*!*************************************************************
*! Name      : lfActivObj
*! Developer : Hend Ghanem (HBG)
*! Date      : 07/05/2000
*! Purpose   : give focuse to current option in option grid
*!*************************************************************
*B803379,1
FUNCTION lfActivObj
PRIVATE lnCurrObj

lnCurrObj = lnActivObj
IF lnActivObj <> 0
  _CUROBJ = lnCurrObj
  lnActivObj = 0
ENDIF
*-- end of lfActivObj.


*!*************************************************************
*! Name      : lfDyeRel
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 10/14/2000
*! Purpose   : Fill Dyelot array with all dyelots for the selected Fabric Color pair
*!*************************************************************
*B603691,1
FUNCTION lfDyeRel
PARAMETERS lcFabClr
PRIVATE lnActAlias , lcDyRelOrd , lcFabClr , lcActivDye
lnActAlias = SELECT(0)
SELECT DYE_REL
lcDyRelOrd = ORDER()
lcActivDye = fabric+color+dyelot

SET ORDER TO 

SELECT Dyelot, cDye_Seq;
 FROM Dye_rel;
 WHERE fabric+color+dyelot = lcFabClr ;
 ORDER BY cDye_Seq INTO ARRAY laDye_Rel

SET ORDER TO Dye_rel
=SEEK(lcActivDye)

SET ORDER TO &lcDyRelOrd
SELECT (lnActAlias)
lnActvLine = RECNO(lcTmpOrdLn)
*-- end of lfDyeRel. 

*!*************************************************************
*! Name      : lfGetDyelt
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 10/14/2000
*! Purpose   : Get another closest dyelot for the selected Fabric Color pair
*!*************************************************************
*B603691,1
FUNCTION lfGetDyelt
PARAMETERS lcFabClrPr
PRIVATE lcFabClrPr , lnInDyeArry , lcRetDye

lcRetDye = ""
lnInDyeArry = ASCAN(laDye_Rel,DYE_REL.DYELOT)
IF lnInDyeArry > 0
  lnInDyeArry = ASUBSCRIPT(laDye_Rel,lnInDyeArry,1)
  =ADEL(laDye_Rel,lnInDyeArry)
  DECLARE laDye_Rel[ALEN(laDye_Rel,1) - 1 , ALEN(laDye_Rel,2)]
  *B607414,1 ASH 07/14/2003 (Begin) recompute this variable after deleting an element from the array to prevent bug 'Subscript out of bound'.
  lnInDyeArry = ASCAN(laDye_Rel,DYE_REL.DYELOT)
  IF lnInDyeArry > 0
  *B607414,1 ASH 07/14/2003 (End)
    IF ALEN(laDye_Rel,1) = 1
      lcRetDye  = laDye_Rel[1,1]
      laDye_Rel = ""
    ELSE
      IF EMPTY(lcPegged) OR (lnInDyeArry = 1)
        lcRetDye  = laDye_Rel[1,1]
      ELSE
        IF lcLastSkip = 'F'
          lcLastSkip = 'B' 
          lcRetDye  = laDye_Rel[lnInDyeArry - 1,1]
        ELSE
          lcLastSkip = 'F' 
          lcRetDye  = laDye_Rel[lnInDyeArry,1]
        ENDIF
      ENDIF
    ENDIF
  *B607414,1 ASH 07/14/2003 (Begin) Fix the bug of 'Subscript out of bound'.
  ENDIF
  *B607414,1 ASH 07/14/2003 (End)
  PRIVATE lnActAlias , lcDyRelOrd
  lnActAlias = SELECT(0)
  SELECT DYE_REL
  lcDyRelOrd = ORDER()
  SET ORDER TO Dye_rel
  =SEEK(lcFabClrPr + lcRetDye , "DYE_REL")
  SET ORDER TO &lcDyRelOrd
  SELECT (lnActAlias)

ENDIF

RETURN lcRetDye
*-- end of lfGetDyelt. 
*:**************************************************************************
*: Name      : lfClearRep
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 02/11/2001
*: Purpose   : Hide and unhide the option of order hold reason.
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*: Return      : ....
*:**************************************************************************
*: Example     : = lfIncOnHld()
*:**************************************************************************
*C#102248
FUNCTION lfIncOnHld

*-- Clear read to Hide and unhide the option of order hold reason.
CLEAR READ

*-- End OF lfIncOnHld.

*:**************************************************************************
*: Name      : lfChkAprov
*: Developer : HEND GHANEM (HBG)
*: Date      : 02/11/2001
*: Purpose   : Check Order Approve Amount.
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*: Return      : ....
*:**************************************************************************
*: Example     : = lfChkAprov()
*:**************************************************************************
*:B804233,1
FUNCTION lfChkAprov

IF !SEEK('O'+Order,lcTmpOrdAp)
  IF SEEK('O'+Order,'OrdHdr')
    INSERT INTO (lcTmpOrdAp) (Type,Order,AprAmnt,TotQty); 
           VALUES ('O',OrdHdr.Order,OrdHdr.ApprAmt,&lcTmpOrdLn..Tot_PoAlo * &lcTmpOrdLn..Price)
  ENDIF
ELSE
  IF &lcTmpOrdAp..lExceed 
    RETURN 'F'  && It Exceeded Before
  ELSE
    REPLACE &lcTmpOrdAp..TotQty WITH &lcTmpOrdAp..TotQty +;
                                     (&lcTmpOrdLn..Tot_PoAlo * &lcTmpOrdLn..Price)
  ENDIF  
ENDIF  

IF &lcTmpOrdAp..TotQty > &lcTmpOrdAp..AprAmnt
  REPLACE &lcTmpOrdAp..lExceed WITH .T.
  RETURN 'E'  && It Ecxeeds Now
ELSE  
  REPLACE &lcTmpOrdAp..lExceed WITH .F.
  RETURN 'N'  && Not Exceed
ENDIF

*:***********************************************************************************
*: Name      : lfAprov
*: Developer : HEND GHANEM (HBG)
*: Date      : 02/11/2001
*: Purpose   : Zap The temp Aprove file to allow Checking Order Approve Amount agian.
*:***********************************************************************************
*: Passed Parameters : None
*:***********************************************************************************
*: Return      : ....
*:***********************************************************************************
*: Example     : = lfChkAprov()
*:***********************************************************************************
*:B804233,1
FUNCTION lfAprov

IF llChkAprov
  lcCurrAlis = ALIAS()
  SELECT (lcTmpOrdAp)
  ZAP
  SELECT (lcCurrAlis)
ENDIF  
*!**************************************************************************
*! Name      : lfVStates
*! Developer : WAB ( Walid A. Wahab)
*! Date      : 12/13/2003
*! Purpose   : functio nto valid the States button i nth option grid
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
*C120733,1 WAB 
*!**************************************************************************
FUNCTION lfVStates
IF ASCAN(laEvntTrig,PADR("VSTATES",10)) <> 0
  =gfDoTriger('ALAUTAL','VSTATES')
ENDIF

*-- End OF lfVStates
*!**************************************************************************
*! Name      : lfVStores
*! Developer : WAB ( Walid A. Wahab)
*! Date      : 12/13/2003
*! Purpose   : functio nto valid the Stores button i nth option grid
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
*C120733,1 WAB 
*!**************************************************************************
FUNCTION lfVStores
IF ASCAN(laEvntTrig,PADR("VSTORES",10)) <> 0
  =gfDoTriger('ALAUTAL','VSTORES')
ENDIF

*-- End OF lfVStores
*!**************************************************************************