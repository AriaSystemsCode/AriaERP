*:************************************************************************
*: Program file  : POSTREC.PRG
*: Program desc. : Style P/o & C/t Receiving.
*: For screen    : POSREC.SPR
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: MF,PO
*:      Developer: TAK - Timour Abdalla Khalil 
*:           Date: 10/10/97               
*:************************************************************************
*: Passed Parameters 
*:                  lcPType =>'I' Receive P/O         
*:                            'S' Receive by Shipment 
*:                            'B' Receive P/O Batch   
*:                            'R' Issue Return P/o  
*:                            'M' Receive C/T         
*:                            'T' Receive C/T Batch   
*:                            'N' Issue Inter-Location P/o
*:                            'O' Receive Inter-Location P/o
*:                            'D' Receive Dye Order
*:                            'A' Issue Adornment order
*:                            'E' Receive Adornment order
*:                            'L' Receive Inter Location P/O Batch'
*:                            'H' Issue Inter Location P/O Batch'
*:************************************************************************
*E300935,4 MAB 08/09/1998 Add code to append new records for dyelots not 
*E300935,4                found into fabdye file, Also append it into dye_rel file
*E300935,4                and then arrange it.
*B602140,1 MAB 11/04/1998 Change browse calculated fields to be 0 if value less than or equal 0.
*B602309,1 TAK 12/29/1998 Added a budjet record in mfgoprdt file for last 
*B602309,1                mfg operation if this operation was not isseud.
*E301077,11 TAK 01/03/1999 Redeuce the number of opened files at start running 
*E301077,11               and open it with object when needed.
*B602485,1 MAB 01/25/1999 Open Fabric and FabDye files under its
*B602485,1                condition as before (not in save procedure)
*B602460,1 MAB 01/25/1999 Close FabDye File instead of MFGOPRHD 
*B602460,1                under its array condition.
*B602554,1 WAM 02/17/1999 Add landed cost records in the bomline even for
*B602554,1                if the system is not setup to use detail costing 
*B602554,1                for importing styles.
*B801970,1  MAB 02/18/1999 browse totcost instead of price(ncost1)
*E301077,11 MAB 03/09/1999 Update odded fields using gfAdd_Info function.
*E301077,11 MAB 03/11/1999 Minimize No. of files oppened at screen entry.
*E301180,1 Walid 03/22/99 Make all needed modification to call 
*E301180,1                po receiveng log report from this program in all
*E301180,1                cases except 'R','M' and'T' for Receive C/T Batch. 
*E301176,1 HDM 03/22/1999 Prevent programs from displaying notepad icon
*E301176,1                as it's now controlled globally
*E301183,1 WALID 03/24/99 Make all needed modification to call 
*E301183,1                po receiveng log report from this program in case of
*E301183,1                receive by shipment to print a new form .
*B602521,1 MAB 03/25/1999 Fix Variable cWareCode not found.
*B602521,1                Fix lfOpn_Rest Function and all of its calling snippets.
*E301182,7 MAB 03/31/1999 Trace cutpick file using master key.
*B602698,1 Walid 04/06/99 Add <open> field to the browse from which
*B602698,1                we select the style/colors in a cutting ticket and PO
*B602698,1                Add two rows to the screen from which we receive by size one for
*B602698,1                the budget and the other for open .
*C101424,1 Walid 04/11/99 Modify Receive cut ticket program to be 
*C101424,1                able to receive  cut tickets belong to selected style .
*E301176,1 HDM 03/14/1999 Assign value to lcBaseFile According to lcPType Parameter Value
*E301206,1 Reham 04/21/99 Add logical parameter in the save procedure to 
*E301206,1                void refreshing browses & windows.
*E301192,1 AHM 04/19/1999 Update reference field in styinvjl with descriptive text
*B602832,1 TAK 04/26/1999 Fixed the bug of all edit fields were disable. 
*B602832,1                Added to disable browse icon, due to adding a base file.
*B602861,1 TAK 05/03/1999 Fixed wrong total P/o amount due to net price rounding.
*E301200,1 AHM 04/15/1999 Add parameter for return lineno to update the added field(lineno)
*E301200,1                in StyInvJl.
*E301220,1 TAK 05/17/1999 Update shipment line record if receive batch by shipment.
*E301223,1 TAK 05/18/1999 Send the first receiving P/O session no to gfstycrl
*E301223,1                in case of issue return p/o and this return from specific p/o.
*B802312,1 TAK 06/03/1999 Call check period if trandate was changed or empty
*B802312,1                GL year and period to avoid empty year and period bug.
*B602965,1 TAK 06/06/1999 Display the open qty as zero if over receive in styles brow.
*B602966,1 TAK 06/07/1999 Wrong update for estemeted cost in P/O if BOM by size.
*B603026,1 HESHAM 06/29/99 Change incomplete session number with every session
*B603062,1 TAK 07/11/1999 Fixed bug of disable new and remove buttons in add mode.
*B802443,1 TAK 07/19/1999 Fixed bug in wrong calculation of landed cost.
*C200080,1 AMM 07/21/1999 consider the case of a new type for Dye orders 
*E301291,4 AMM 08/01/1999 Add a new type for Adornment orders (Issue/Receive).
*B802566,1 TAK 08/29/1999 Fixed infinite loop and browse -ve open quantity.
*B603151,1 TAK 09/08/1999 Fix syntax error in no G/L used.
*B802592,1 TAK 09/20/1999 Fix bug in filter of the browse of P/O and C/t.
*B603204,1 TAK 10/12/1999 Added to not allow to re-print the receiving log
*B603204,1                if point of sale setting is set to no.  
*B603216,1 ASH 10/18/1999 Fix the bug of getting a message asking you to 
*B603216,1                EDIT THE ALLOCATED QTY/ IGNORE / IGNORE ALL even if 
*B603216,1                there is no cancel qty in case of receiving from batch.
*B603230,1 SSH 25/10/1999 Fix the bug of incorrect formula to get the new 
*B603230,1 SSH            allocated qty. Incase of recieve by C/T.
*B603231,1 SSH 25/10/1999 Fix the bug of incorrect formula to get the new 
*B603231,1 SSH            allocated qty. Incase of recieve by Batch.
*B603218,1 SSH 26/10/1999 Fix the bug of geting the 'Dylot Selection'  brows has 
*B603218,1 SSH            the same 2 records with the same qty open qty for the same dylot.
*B603237,1 TAK 27/10/1999 Added a custom process for Fresh Produce to 
*B603237,1                Issue / Receive Adornment order, to have an own
*B603237,1                custom screen and some special updates.
*B802635,1 AMM 11/08/1999 Add the update of dyelot field in Operation management detail table
*B603275,1 AMM 11/21/1999 Fix some bugs in Dyelot distribution Screen
*C101683,1 WAB 11/29/1999 custom program to allow the user to receive cutting tickets automatically 
*C101683,1 WAB            by style/color.
*B603385,1 TAK 01/11/2000 Reveive the last operation if there is only one operation in cost sheet.
*B101704,1 SAM 01/17/2000 Allow alpha numeric PO# manualy
*B603440,1 ADEL 02/08/2000 Fix the bug of not recieving actualized POs when recieving by shipment.
*B803030,1 TAK 02/08/2000  Fix the bug of checking in the status po in case
*B803030,1                 of Detail costing yes only. Fix the bug of 
*B803030,1                 replacing the cost by 0 in case of detail 
*B803030,1                 costing no.
*B802902,1 SSH 03/05/2000 if we select to preview log report, 
*B802902,1 SSH            the reports show the estimated p.price along 
*B802902,1 SSH            (with each style/color being received) 
*B802902,1 SSH            instead of the landed p.price. 
*B803251,1 SSH 04/05/2000 Prevent recive to damage/Cancel if the 
*B803251,1 SSH 04/05/2000 styles have different scales.
*B803254,1 SSH 21/05/2000 Change the caption of the damaged in the cuttkt
*B803254,1 SSH 21/05/2000 Screen to be others
*B603100,1 SSH 22/05/2000 Fix the bug of incoret recieved cost in case of 
*B603100,1 SSH 22/05/2000 recieve by batch.
*B603235,1 SSH 22/05/2000 in corect allocate qty. incase of batch.
*B803337,1 WMA 06/12/2000 error message 'bomline file not found'
*B603775,1 ADEL 08/01/2000 Fix the bug of wrong balance qty in the browse when over receiving PO .
*B603822,1 RAMY 21/08/2000 If we recive to damaged syle with scale count less than the orignal 
*						   style scale recieve the total qty to the first size
*B803628,1 RAMY 10/09/2000 If we recieve by shipment get the right value of the open qty
*B803594,1  MHM 09/10/2000 Add option to print decimal to get some space
*B803590,1  MHM 09/10/2000 Fix the spelling mistake of "receipts".
*B803693,1 HBG 10/18/2000 Fix bug of landed cost not updated if not multi currency[Begin]
*E301480,1  NAD 10/17/2000 Enhancment to temporary receive the Inter-Loc. PO.  
*E301484,1 WAM 11/16/2000 Move saving function to a new separate program POUPDATE to be called 
*E301484,1 WAM            from EDI programs.
*C102172,1 AMH 01/29/2001 custom program to allow the user to receive cutting tickets automatically 
*C102172,1 AMH            by lot or by invoice.
*C102172,1 AMH 03/19/2001 Rework for the Task C102172,1.
*C200170,1 AMH 04/08/2001 Issue Inter-Location P/O Batch.
*C102359,1 AMH 05/24/2001 Custome receive adornment order for JL.
*C102358,4 AMH 06/09/2001 Fix some bugs in issue adornment order for JL.
*C102359,4 AMH 06/09/2001 Fix some bugs in receiveing adornment order for JL.
*B604502,1 MHM 06/11/2001 Fix Bug of not calculating the remaining qty correctly
*B604646,1 MHM 07/17/2001 Fix the bug of duplicate receiving Quantity in case
*B604646,1                of Inter-location .   
*B604677,1 MHM 07/19/2001 Fix the bug of receving damages styles when having
*B604677,1                conflict scale between the damage and original styles              
*B804345,1 AMH 08/07/2001 Fix the bug of checking if the fabric is found in the ware houase of 
*B804345,1                the PO.
*B604878,1 KHM 09/10/2001 Fix the bug of not displaying the correct Previous 
*B604878,1                Receive figure in case of over receiving.
*B603625,1 AMH 10/25/2001 Fix the bug of not taking care of the adornment and 
*B603625,1 AMH            dye order case.
*B605082,1 AMH 11/12/2001 Fix the bug of not taking care of the dye order case.
*B804539,1 AMH 11/20/2001 Fix the bug of issue ador. order for on hold sales order.
*B605331,1 KHM 01/08/2001 Fix the bug of wrong updating of the STK when 
*B605331,1                changing the ware house.
*B605405,1 KHM 01/23/2002 Displaying a notification message when receiving a
*B605405,1                C/T that its items have not been issued.
*B605612,1 KHM 02/28/2002 Fix the bug of wrong update of styqty in the bomline
*B605612,1                file incase of receiving to damage.
*B605514,1 HBG 17/03/2002 IF the Po is included in a shippment , cann't receive by PO 
*C102581,1 AMH 03/25/2002 Add trigger for Robyn Merdith to save the user defined fields.
*B804568,1 AMH 04/08/2002 Prevent recive to damage/Cancel if the styles have different scales.
*B606205,1 KHM 07/03/2002 Fix the bug of displaying all the locations even if they were not
*B606205,1                marked to be a style finished goods inventory.
*B606376,1 KHM 08/13/2002 Fix the bug of preventing the user from receiving a PO that is included
*B606376,1                in an open shipment.
*B606426,1 AMH 08/28/2002 Fix bug of wrong open quantity in the browse lines when recieve.
*B606589,1 KHM 11/05/2002 Fix the bug of incorrect message when edit the allocated qty.
*B607034,1 ARH 03/16/2003 Fixed numeric overflow bug in case of England
*C200488,1 WAB 03/17/2003 Custom program o reciev by carton.
*B607472,1 ALB 08/19/2003 Fix the bug of defualt Bin for style/Color
*B121330,1 NNA 01/25/2004 Fix bug of receiving style PO allows any date when GL not installed
*C037816,1 MHM 04/06/2004 Custom Bin Location For David Luke
*B038431,1 NNA 09/05/2004 fix bug that in the 'issue inter location style PO' when you Choose a PO
*B038431,1 NNA            you will get an error massage 'Variable "lcCarton" not Found'
*B038431,1 MHM 09/13/2004 fix bug that Variable lcCarton Not found if not by carton
*C037957,1 MHM 10/10/2004 Custom Amend to CutPick File
*B124614,1 NNA 10/24/2004 Fix bug that when adding shipment to a batch and approve that batch then
*B124614,1 NNA            reciving it.completly the shipment status does not change to complete.
*C123616,1 TMI 11/10/2004 Add triggers to Open cartons types screen for Memo/nik nak
*C126994,1 NNA 06/29/2005 Add new Trigger to add a new menu pad called [Option]
*E128404,1 TMI 07/12/2005 Automatic receive in the C/T receiving screen               
*C127341,1 TMI 07/25/2005 if shipment is on hold , do not receive
*B039484,1 MMR 07/29/2005 Fix bug of duplicating qty amd cost while recieving C/T.
*C128583,1 TMI 08/23/2005 Update the receiving date for the batch from the field ctktrcvh.drcvdate 
*B130692,1 MHM 12/12/2005  Check for using bin location
*B131443,1 MHM 03/22/2006 check for Cancelled or Danged if found to let user save
*B123103,1 TMI 05/29/2006 Recive the 2nd Quality/Damaged with thier related costs
*B039660,1 NNA 06/06/2006 Convert CP#037816 (Bin Location) to be used within Binmain.Prg.
*T20061003.0005 TMI (B#607795) 10/05/2006 Fix a bug that when receive a CT to multiple WH the open qty is wrong calculated in MFRCVQ screen
*B609021,1 TMI 9/24/2009 adjust the lenght of the loop to get the color pos [T20090921.0017]
*:********************************************************************************************
PARAMETERS lcPType
*--If P/O you call program directly from menu without parameters.
*--Global program variable indicate the type of receiving.
lcPType  = IIF(TYPE('lcPType') $ 'UL','I',lcPType)
*--Global program variable flag indicate if it P/O or C/T receive.
llMFCall = ( lcPType $ 'MT' )
*--Initialize program variables.
STORE ' ' TO lcWinCh0,lcWinCh1,lcWinCh2,lcWinCh3,lcWinCh4,lcWinCh5
STORE 0   TO lnstylewid,lnMjrWid
STORE ' ' TO lcStyHdr,lcStyPict,lcTCode,lcCostMth,lcSOrder,lcAOrder
STORE .F. TO llLinkToGl,llWareHous,llWareLoc,llDyelot,llPOSale
STORE .F. TO llImpCost,llMulCurr,llUseMCurr,llEditExRt,llLoc,llIgnorAll,llCostPrv
STORE ' ' TO lcCostImp,lcISlbl1,lcISlbl2,lcISlbl3,lcISlbl4,lcISlbl5,;
             lcIType1,lcIType2,lcIType3,lcIType4,lcIType5,lcDropLoc
STORE ' ' TO lcPfrnSmbl,lcDfrnSmbl,lcBaseSmbl,lcMasterLn
STORE ' ' TO lcTmpLine,lcTemLoc,lcGlDist,lcBlFields,lcGlSession,lcTmpCtPk,lcTempFile

*!B602698,1 [BEGIN] 
STORE ' ' TO lcTCodeOld &&-- variable to hold the last C/T or PO
*!B602698,1 [END..] 

*C101424,1 lcCurrSty : Variable to save current receive style [Begin]
*C101424,1             (Receive C/T by Style)
*STORE ' ' TO lcShpCode,lcBatch,lcBfields,lcStyle,lcStyDesc,lcOldValue,;
*             lcGlPeriod,lcGLFYear,lcCur1,lcCur2,lcAirWayB,lcShpRef,;
*             lcBStatus,lcBDesc,lcQltyKey,lcWareCode,lcDyelot,;
*             lcBrTpNme,lcLotNo,lcClrLstOp,lcPickIndx

*B038431,1 NNA 09/05/2004 (begin) Add 'lcCarton' To the Defined Variables
*STORE ' ' TO lcShpCode,lcBatch,lcBfields,lcStyle,lcStyDesc,lcOldValue,;
             lcGlPeriod,lcGLFYear,lcCur1,lcCur2,lcAirWayB,lcShpRef,;
             lcBStatus,lcBDesc,lcQltyKey,lcWareCode,lcDyelot,;
             lcBrTpNme,lcLotNo,lcClrLstOp,lcPickIndx,lcCurrSty,lcoSydKey
STORE ' ' TO lcShpCode,lcBatch,lcBfields,lcStyle,lcStyDesc,lcOldValue,;
             lcGlPeriod,lcGLFYear,lcCur1,lcCur2,lcAirWayB,lcShpRef,;
             lcBStatus,lcBDesc,lcQltyKey,lcWareCode,lcDyelot,;
             lcBrTpNme,lcLotNo,lcClrLstOp,lcPickIndx,lcCurrSty,lcoSydKey,lcCarton
*B038431,1 NNA (End)
*C101424,1 lcCurrSty : Variable to save current receive style [Begin]

STORE 0   TO lnTotStk,lnTotDam,lnTotCan,lnCartons,lnBrRecNo,lnPolstln,;
             lnRate1,lnRate2
STORE 1   TO lnCurrUnt1,lnCurrUnt2,lnCrRt1,lnCrRt2
STORE .F. TO llBrowse,llByCarton,llNewItem,llEditLCst,llShpPO,llCont_Upd,llSpecHndl
STORE {}  TO ldEnterd,ldEta,ldBDate
STORE gdSysDate TO ldRcvDate,ldTrDate
llSOInstld = (OCCURS('SO',gcComp_mdl)<>0)
llPOInstld = (OCCURS('PO',gcComp_mdl)<>0)
*E301484,1 Check Id new communication module installed
*llCMInstld = (OCCURS('CM',gcComp_mdl)<>0)
llCMInstld = (OCCURS('NC',gcComp_mdl)<>0)
*E301484,1 (End)


*--Variables of opereation lots.
llSpecLot = .F.
DIME laLotArry[8]
laLotArry = 0

*--Variable for automatic Issue/receive.
lcAuto  = 'M'
llFirst = .T.

*B602698,1[BEGIN] Flag to indicate if the tmp file is created or not .
llFirstTmp = .F.
*B602698,1[END..]

*E300935,4 lcTmDyeRel : Temp. name to indicate our scope withen dye_rel file.
*E300935,4 llFabDye   : .T. if fabric use dyelots.
lcTmDyeRel = ''
llFabDye   = .F.
*E300935,4

*E300935,4 lcFabColor : variable that hold color for current fabric [begin]
lcFabColor = ''
*E300935,4 lcFabColor : variable that hold color for current fabric [end]

*--Screen Arrays.
*E301077,11 MAB No need to laOpFile array (Open with gfOpenFile Open files and  [Begin
*E301077,11 MAB add it to array laFileName).
*DIME laType[1,2],laWare[1],laECost[5],laOpFile[12]
DIME laType[1,2],laWare[1],laECost[5]
*E301077,11 MAB No need to laOpFile array (Open with gfOpenFile Open files and  [Begin

STORE ' ' TO laType,laWare[1]
STORE 0   TO laECost,lnWare
*B038431,1 MHM 09/13/2004 [Start]
lcCarton = " "
*B038431,1 MHM 09/13/2004 [End]
*--Uncomplte session variables(UNCSS).
DECLARE laVariables[15]
lcAProgm   = 'POSTREC'

lcUnCmPgId = 'RECIVING_'+lcPType
lnSessNo   = gnProgCopy
lcSession  = ' '
laVariables= ' '
llContinue = .F.
llGoAndChk = .T.

*--Uncomplete session variables.
laVariables[1]  = 'lcPType'
laVariables[2]  = 'lcShpCode'
laVariables[3]  = 'ldTrDate'
laVariables[4]  = 'ldRcvDate'
laVariables[5]  = 'lnTotStk'
laVariables[6]  = 'lnTotDam'
laVariables[7]  = 'lnTotCan'
laVariables[8]  = 'ldEnterd'
laVariables[9]  = 'lnCartons'
laVariables[10] = 'ldEta'
laVariables[11] = 'lcAirWayB'
laVariables[12] = 'lcShpRef'
laVariables[13] = 'lcGlSession'
laVariables[14] = 'lcBatch'
laVariables[15] = 'llMFCall'

*-- Variable to tell if are we going to check the existance of
*-- any incomplete session in the screen's setup or not.
llChkUnCom = .T.

IF !gfSetup()
  RETURN
ENDIF

*C123616,1  TMI [Start] No need for this trigger , an option menu is added 
**C200488,1 WAB (Start) call triger to create the temp. files 
*IF ASCAN(laEvntTrig,PADR("CREATTMP",10)) <> 0
*  =gfDoTriger("POSTREC",PADR("CREATTMP",10))
*ENDIF
**C200488,1 WAB (End) 
*C123616,1  TMI [End  ] 

*E301077,11 MAB Call gfGetMemVar Once for all memory variables [Begin]
*C200080,1 AMM Dimension the array of 22 anyway.
*DIMENSION laSetups[IIF(llMFCall,11,22),2]

*B101704,1 [Start] add new setup for PO
*DIMENSION laSetups[22,2]
DIMENSION laSetups[23,2]
*B101704,1 [End]

IF lcPType = 'A'
  lcSydKey = SPACE(0)
ENDIF
*C200080,1 AMM end
laSetups[1,1]  = 'M_WareHouse'
laSetups[2,1]  = 'M_WareLoc'
laSetups[3,1]  = 'M_Dyelot'
laSetups[4,1]  = 'M_MATDYE'
laSetups[5,1]  = 'M_Cost_Meth'
laSetups[6,1]  = 'M_Link_GL'
laSetups[7,1]  = 'M_DROPWARE'
laSetups[8,1]  = 'M_SYSTYPE'
laSetups[9,1]  = 'M_LImpCost'
laSetups[10,1] = 'llMulCurr' 
laSetups[11,1] = 'llEditExRa'
*C200080,1 AMM
*IF !llMFCall
*C200080,1 AMM end
laSetups[12,1] = 'M_cCostImp'
laSetups[13,1] = 'M_cISLbl1'
laSetups[14,1] = 'M_cISLbl2'
laSetups[15,1] = 'M_cISLbl3'
laSetups[16,1] = 'M_cISLbl4'
laSetups[17,1] = 'M_cISLbl5'
laSetups[18,1] = 'M_cIType1'
laSetups[19,1] = 'M_cIType2'
laSetups[20,1] = 'M_cIType3'
laSetups[21,1] = 'M_cIType4'
laSetups[22,1] = 'M_cIType5'      

*B101704,1 [Start]
laSetups[23,1] = 'M_GenStOrN'
llGenOrNum = .F.
*B101704,1 [End]
*C200080,1 AMM end if
*ENDIF
*C200080,1 AMM end
=gfGetMemVar(@laSetups,gcAct_Comp)
*E301077,11 MAB Call gfGetMemVar Once for all memory variables [End  ]

*E301077,11 MAB Change all variables corressponding to array col. 2 [Begin]
*--Setup use multi location y/n.
llWareHous = (laSetups[1,2]='Y')
llWareLoc  = (laSetups[2,2]='Y')
llDyelot   = (laSetups[3,2]='Y')
llFabDye   = (laSetups[4,2]='Y')
*E301077,11 MAB Change all variables corressponding to array col. 2 [End  ]

*--Cannot use the inter Location P/o if no multi location is used.
*E301291,4 AMM Consider Issue adornment order case.
*IF lcPType = 'N' AND !llWareHous

*C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
*IF lcPType $ 'NA' AND !llWareHous
IF lcPType $ 'NAH' AND !llWareHous
*C200170,1 AMH [End]

*E301291,4 AMM end
  *--The system has not been setup to use multiple locations. Cannot proceed.
  =gfModalGen('TRM42054B42001','DIALOG')
  RETURN
ENDIF
*E301077,11 MAB Open needed files [Begin


*E301077,11 MAB Open either Po Files or C/T files. [Begin]
*C200080,1 AMM (start)
*IF !llMFCall 
*C200080,1 AMM end
  =gfOpenFile(gcDataDir+'POSHDR','POSHDR','SH')
  =gfOpenFile(gcDataDir+'POSLN','POSLN','SH')
*C200080,1 AMM end if
*ENDIF
*C200080,1 AMM end
*E301077,11 MAB Open either Po Files or C/T files. [End  ]

IF 'MA' $ gcComp_mdl
  = gfOpenFile(gcDataDir+"Fabric","Fabric","SH")
  = gfOpenFile(gcDataDir+"FabDye","FabDye","SH")
ENDIF  

*E301077,11 MAB Open Dyelot relationship table if style and material use dyelots. [Begin]
IF llDyelot AND llFabDye
  =gfOpenFile(gcDataDir+'DYE_REL','DYE_REL','SH')
ENDIF
*E301077,11 MAB Open Dyelot relationship table if style and material use dyelots. [End  ]

*B803337,1 WMA Open BOMLINE File[Begin]
=gfOpenFile(gcDataDir+'BOMLINE','BOMLINE','SH') 
*B803337,1 WMA [End]

*B123103,1  TMI [Start] Open the BOM file
=gfOpenFile(gcDataDir+'BOM','BOM','SH') 
*B123103,1  TMI [End  ] 

*E301077,11 MAB Rise some flags to open files (in Prg not in module object) [Begin
STORE .F. TO llOpnBom,llOpnBomLn,llOpnBmCst,llOpnKtBom
*E301077,11 MAB Rise some flags to open files (in Prg not in module object) [End..

*E301077,11 MAB Open needed files [End..
*E301291,4 AMM Add Issue adornment order 
*IF lcPType = 'R'
  *laType[1,1]='Issue Return P/O'
  *laType[1,2]='R'
*ENDIF
*IF lcPType = 'N'
  *laType[1,1]='Issue Inter Location P/O'
  *laType[1,2]='N'
*ENDIF
DO CASE
  CASE lcPType = 'R'
    laType[1,1]='Issue Return P/O'
    laType[1,2]='R'
  CASE lcPType = 'N'
    laType[1,1]='Issue Inter Location P/O'
    laType[1,2]='N'
  CASE lcPType = 'A'
    laType[1,1]='Issue Adornment P/O'
    laType[1,2]='A'

  *C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
  CASE lcPType = 'H'
    laType[1,1]='Issue Inter Location Batch'
    laType[1,2]='H'
  *C200170,1 AMH [End]

  *C102359,1 AMH Custome receive adornment aorder for JL [Start]
  CASE lcPType = 'E' AND ASCAN(laEvntTrig,PADR("RCVADORD",10)) <> 0
    laType[1,1]='Receive Adornment order'
    laType[1,2]='E'
  *C102359,1 AMH [End]
  
ENDCASE
*E301291,4 AMM end

*E301077,11 MAB No need to laOpFile.
*laOpFile=.F.

lnType  = 1

*--Control pannel additional elements. [begin]
*E301176,1 HDM 03/22/1999 [start] Prevent programs from displaying notepad icon
*                           as it's now controlled globally

*DECLARE laPanelObj[1,3]
*STORE '' TO laPanelObj
*laPanelObj[1,1] = 'pbPONote'
*laPanelObj[1,2] = gcBmpHome+'NOTES2.BMP'
*laPanelObj[1,3] = [VALID lfvNote() MESSAGE 'Notes' DISABLE]
*E301176,1 HDM 03/22/1999 [End]

*--Issue or Recieve operation.
*E301291,4 AMM Consider Issue adornment order case
*llIssue   = (lcPType $ 'RN')

*C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
*llIssue   = (lcPType $ 'RNA')
llIssue   = (lcPType $ 'RNAH')
*C200170,1 AMH [Start]

*E301291,4 AMM end

lcBrDtTtl  = IIF(llIssue,'Issuing Lines','Receiving Lines')

lcMasterLn = IIF(llMFCall,'CUTTKTL','POSLN')
*-Trancd no.
lcOthrTrCd = IIF(llMFCall,'3','4')
lcCanlTrCd = IIF(llMFCall,'4','5')

*E300935,4 lcSeekBomTp : variable that hold type of fabric item [begin]
lcSeekBomTp = ''
*E300935,4 lcSeekBomTp : variable that hold type of fabric item  [end]

IF !WEXIST(gcBaseWind)

  *E300935,4 evaluate fabric type in bomline file [begin] 
  lcStyleTyp = IIF(llMFCall , "M" , "I")
  FOR lnCount = 1 TO 5
    lcCount  = STR(lnCount,1)
    lc&lcStyleTyp.TYPE&lcCount = gfGetMemVar('M_C'+lcStyleTyp+'TYPE'+lcCount , gcAct_Comp)
    IF lc&lcStyleTyp.TYPE&lcCount  = "F"
      lcSeekBomTp = lcCount
      EXIT
    ENDIF
    
  ENDFOR 
  *E300935,4 evaluate fabric type in bomline file [end] 

  *E300935,4 lcTmDyeRel : Temp. name to indicate our scope withen dye_rel file.
  lcTmDyeRel = gfTempName()
  *E300935,4

  *--Program variables.

  *-Style structure.
  lcStyHdr   = gfItemMask('HI')
  lcStyPict  = gfItemMask('PI')
  lnstylewid = LEN(lcStyPict)
  lnMjrWid   = LEN(gfItemMask('PM'))
  
  *E301077,11 MAB Assign setups col2 to all memvar [Begin]
  *-Global setups.
  *lcCostMth  = gfGetMemVar('M_Cost_Meth')
  lcCostMth  = laSetups[5,2]

  *llLinkToGl = gfGetMemVar('M_Link_GL')  ='Y'
  llLinkToGl = (laSetups[6,2] = 'Y')
  
  *--Restore the Drop Ship Location.
  *lcDropLoc  = gfGetMemVar('M_DROPWARE')
  lcDropLoc  = laSetups[7,2]

  *--System type Point of sale. 
  *llPOSale   = gfGetMemVar('M_SYSTYPE') = 'P'
  llPOSale   = (laSetups[8,2] = 'P')


  *-Costing setups.
  *llImpCost  = gfGetMemVar('M_LImpCost')
  llImpCost  = laSetups[9,2]
  IF EMPTY(llImpCost)
    llImpCost = .F.
  ENDIF

  *-Currency setups
  *llMulCurr  = gfGetMemVar('llMulCurr')
  *C200080,1 AMM No multi currency in Adornment or dye order
  *llMulCurr  = laSetups[10,2]
  llMulCurr  = IIF(lcPType $ 'DAE',.F.,laSetups[10,2])
  llUseMCurr = llMulCurr
  *C200080,1 AMM end

  *llEditExRt = gfGetMemVar('llEditExRa')
  llEditExRt = laSetups[11,2]
  *C200080,1 AMM (start) define the variable 
  *IF !llMFCall
  *C200080,1 AMM end
    *lcCostImp  = gfGetMemVar('M_cCostImp')
    lcCostImp  = laSetups[12,2]

    *lcISlbl1   = gfGetMemVar('M_cISLbl1')
    lcISlbl1   = laSetups[13,2]

    *lcISlbl2   = gfGetMemVar('M_cISLbl2')
    lcISlbl2   = laSetups[14,2]

    *lcISlbl3   = gfGetMemVar('M_cISLbl3')
    lcISlbl3   = laSetups[15,2]

    *lcISlbl4   = gfGetMemVar('M_cISLbl4')
    lcISlbl4   = laSetups[16,2]

    *lcISlbl5   = gfGetMemVar('M_cISLbl5')
    lcISlbl5   = laSetups[17,2]

    *lcIType1   = gfGetMemVar('M_cIType1')
    lcIType1   = laSetups[18,2]

    *lcIType2   = gfGetMemVar('M_cIType2')
    lcIType2   = laSetups[19,2]

    *lcIType3   = gfGetMemVar('M_cIType3')
    lcIType3   = laSetups[20,2]

    *lcIType4   = gfGetMemVar('M_cIType4')
    lcIType4   = laSetups[21,2]

    *lcIType5   = gfGetMemVar('M_cIType5')      
    lcIType5   = laSetups[22,2]
  *B101704,1 [Start]
  llGenOrNum = (UPPER(ALLTRIM(laSetups[23,2])) ='Y') 
  *B101704,1 [End]
  *C200080,1 AMM
  *ENDIF
  *C200080,1 AMM end
  *E301077,11 MAB Assign setups col2 to all memvar [End  ]

  llCostPrv  = gfUserPriv('IC','ICSTYLE','COSTING')

  STORE SET('CURRENCY',1) TO lcPfrnSmbl,lcDfrnSmbl,lcBaseSmbl
  IF !llMulCurr
    STORE gcBaseCurr TO lcCur1,lcCur2  
  ENDIF 

  *--Program screen windows.
  lcWinCh0 = gfTempName()
  lcWinCh1 = gfTempName()
  lcWinCh2 = gfTempName()
  lcWinCh3 = gfTempName()
  lcWinCh4 = gfTempName()
  lcWinCh5 = gfTempName()

  *--Get a unique session number, to be used to update the uncomplete 
  *--session file(UNCSS).
  lcSession  = gfsequence('cSession')

  SELECT WAREHOUS
  IF llWareHous  
    IF llCMInstld AND llPOSale
      LOCATE FOR cSiteId = gcCurSite    
      laWare[1] = WAREHOUS.cWareCode+'-'+WAREHOUS.cDesc
      lnWare = 1
    ELSE
      
      *B606205,1 KHM 07/03/2002 (Begin) Get the ware houses that marked to be a style finished 
      *B606205,1                goods inventory only.      
      *SELECT cWareCode+'-'+cDesc FROM Warehous INTO ARRAY laWare
      SELECT cWareCode+'-'+cDesc FROM Warehous WHERE lStyInv INTO ARRAY laWare
      *B606205,1 KHM 07/03/2002 (End)
      
      DECLARE laWare [ALEN(laWare,1)+1,ALEN(laWare,2)] 
      =AINS('laWare',1)
      laWare[1,1] = "              N/A           "
    ENDIF
  ELSE
    GO TOP IN WAREHOUS
    laWare[1] = WAREHOUS.cWareCode+'-'+WAREHOUS.cDesc
  ENDIF

  *--Open needed files.
  *E301077,11 Start. Comment these files.
  *laOpFile[1] = gfOpenFile(gcSysHome+'SyuEror','cSession','SH')
  IF llMFCall OR (llImpCost AND !( lcPType $ 'RN' ))
    *-Used in cost sheet only.
    = gfOpenFile(gcDataDir+"MFGOprDt","MFGOprDt","SH")
*    laOpFile[2] = gfOpenFile(gcDataDir+"MFGOprHd","MFGOprHd","SH")

     *B602485,1 TAK comment out the following two lines before [Begin
     *B602485,1 but now I rewrite them. 
     *    laOpFile[4] = gfOpenFile(gcDataDir+"FabDye","FabDye","SH")
     *    laOpFile[5] = gfOpenFile(gcDataDir+"Fabric","Fabric","SH")
     *= gfOpenFile(gcDataDir+"FabDye","FabDye","SH")
     *= gfOpenFile(gcDataDir+"Fabric","Fabric","SH")
     *B602485,1 TAK comment out the following two lines before [End..

  ENDIF
  
  IF llSOInstld 
     =gfOpenFile(gcDataDir+"CutPick","Cutpick","SH")  
*    laOpFile[7] = gfOpenFile(gcDataDir+"OrdHdr","OrdHdr","SH")  
*    laOpFile[8] = gfOpenFile(gcDataDir+"OrdLine","OrdLine","SH")  
  ENDIF

*mab [Begin
*  IF llLinkToGl
*    laOpFile[9] = gfOpenFile(gcDataDir+'GLDIST','GlDistAc','SH')
*  ENDIF
*mab [End
  *E301077,11 End.

  IF llWareLoc 
    = gfOpenFile(gcDataDir+'WhsLoc','WhsLocSt','SH')  
  ENDIF  
  
  *E301077,11 MAB Comment out the following block open it if you need. [Begin
  *IF llPOInstld
  *  = gfOpenFile(gcDataDir+'ShpmtHdr','ShpmtHdr','SH')
  *ENDIF
  *E301077,11 MAB Comment out the following block open it if you need. [End  

  *--Temprorary files names.
  *B602698,1[BEGIN] Temp name for the Temp file
  lcTmpLine = gfTempName()
  *B602698,1[END..]
  lcTempFile  = gfTempName()
  IF llLinkToGl
    lcGlDist = gfTempName()   
  ENDIF
  IF llWareLoc 
    DIMENSION laSource[1],laTarget[1]
    lcTemLoc = gfTempName()   
  ENDIF  
  lcTmpCtPk = gfTempName()
  *--HDM 04/13/1999 E301176,1[Start] Assign value to lcBaseFile According to lcPType Parameter Value
  DO CASE
    *C200080,1 AMM Add the dye order type 'D'
    *CASE lcPType $ 'IRN'

    *C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
    *CASE lcPType $ 'IRNDAE'
    CASE lcPType $ 'IRNDAEH'
    *C200170,1 AMH [End]

    *C200080,1 AMM end
      lcBaseFile = 'POSHDR'
      lcScFields = 'PO'
      *B802592,1 Added. 
      =lfSetFltr()
      *B802592,1 End. 
    CASE lcPType = 'M'
      lcScFields = 'cuttkt'
      lcBaseFile = 'CUTTKTH'
      *B802592,1 Added. 
      =lfSetFltr()
      *B802592,1 End. 
  ENDCASE
  *--HDM 04/13/1999 E301176,1[End]
  *E301291,4 AMM Get old note key variable
  lcoSydKey = lcSydKey
  *E301291,4 AMM end
ENDIF

EXTERNAL ARRAY laDefProc
laDefProc[9]  = .F.       && Disable the control panel save proc.  (lpSavScr)
laDefProc[10] = .F.       && Disable the control panel close proc. (lpClsScr)
llNoShow = .F.

*--Call screen.
*--HDM 04/13/1999 E301176,1[Start] Assign value to lcBaseFile According to lcPType Parameter Value
*lcBaseFile=""
*--HDM 04/13/1999 E301176,1[End]

PUSH KEY
IF !llMfCall AND (!llImpCost OR lcPType $ 'RN')
  DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
  ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
  *C200080,1 AMM Add the dye order type 'D'
  *DEFINE BAR 1 OF _OPTIONPOP PROMPT 'Edit landed cost per line' SKIP FOR (!(lcPType$'ISR') OR !laScrMode[4] OR EOF(lcTmpLine) OR !llCostPrv)
  DEFINE BAR 1 OF _OPTIONPOP PROMPT 'Edit landed cost per line' SKIP FOR (!(lcPType$'ISRD') OR !laScrMode[4] OR EOF(lcTmpLine) OR !llCostPrv)
  *C200080,1 AMM end
  SET MARK OF BAR 1 OF _OPTIONPOP TO llEditLCst
  ON SELECTION BAR 1 OF _OPTIONPOP DO lpPopSel
ENDIF
*C123616,1  TMI [Start] Define Option bar for posrec.spr screen FOR NIK NAK
IF ASCAN(laEvntTrig,'DEFPAD') <> 0
  =gfDoTriger('POSTREC','DEFPAD')
ENDIF
*C123616,1  TMI [End  ] 

ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrDtTtl)

*C101683,1 WAB - Custom process to Receive By Style/Color.
*C101683,1 WAB - START
IF ASCAN(laEvntTrig,PADR("ADDOPTION",10)) <> 0
  =gfDoTriger("MFRCVCT",PADR("ADDOPTION",10))
ENDIF  
*C101683,1 WAB - END

*C126994,1 NNA 06/29/2005 (BEGIN) Define Option bar for Receiving into Multiple Bins Screen For GPS00
IF ASCAN(laEvntTrig,PADR("PORECPAD",10)) <> 0
  =gfDoTriger("POSTREC",PADR("PORECPAD",10))
ENDIF
*C126994,1 NNA (END)

*C102172,1 AMH Custom process to Receive by lot or by invoice [Start]
IF ASCAN(laEvntTrig,PADR("ADOPTCAT",10)) <> 0
  STORE .F. TO llRecLot, llRecInv
  =gfDoTriger("MFRCVCT",PADR("ADOPTCAT",10))
ENDIF  
*C102172,1 AMH [End]

*C101424,1 MAB Filter on style file [Begin]
SELECT STYLE
*C200080,1 AMM start
*IF llMfCall
*C102358,4 AMH Filter to !make for Jl [Start]
*IF llMfCall .OR. lcPType$'DAE'
IF llMfCall .OR. (lcPType$'DAE' .AND. ASCAN(laEvntTrig,PADR("RCVADORD",10)) = 0)
*C102358,4 AMH [End]
*C200080,1 AMM end
  SET FILTER TO Make
ELSE
  SET FILTER TO !Make
ENDIF  
*C101424,1 MAB Filter on style file [End  ]


*  = gfOpenFile(gcDataDir+"CUTTKTL","CUTTKTL","SH")

*B603237,1 Special Changes in the program.
*B603237,1 Frech Produce has special handel for Issue/Receive Adornmnet order.
llSpecHndl =( ASCAN(laEvntTrig,PADR('SPECL_SET',10)) > 0 ) 
*-Used for FP by MFRCVAP,MFRCVAR,MFRCVCT
*B603237,1
*B603237,1 Added a custom screen process.
*B603237,1 For Fresh Produce to Issue/Receive adornment order.
IF lcPType $ 'AE' AND llSpecHndl
  *--Fresh Produce screen. 
  DO (gcScrDir+"MFSREC.SPX")
ELSE
*B603237,1 End.
  DO (gcScrDir+"POSREC.SPX")
ENDIF



*C101424,1 MAB Filter on style file [Begin]
SELECT STYLE
SET FILTER TO
*C101424,1 MAB Filter on style file [End  ]
*C101683,1 WAB - Release option from sysmenu in case of addoption trigger
*C101683,1 WAB - START
*IF !llMfCall AND (!llImpCost OR lcPType $ 'RN')
*C102172,1 AMH Release option from sysmenu in case of adoptcat trigger [Start]
*IF (!llMfCall AND (!llImpCost OR lcPType $ 'RN')) OR (ASCAN(laEvntTrig,PADR("ADDOPTION",10)) <> 0)

*C126994,1 NNA 06/29/2005 (BEGIN) Release option menu from sysmenu
*IF (!llMfCall AND (!llImpCost OR lcPType $ 'RN')) .OR. (ASCAN(laEvntTrig,PADR("ADDOPTION",10)) <> 0);
   .OR. (ASCAN(laEvntTrig,PADR("ADOPTCAT",10)) <> 0)
IF (!llMfCall AND (!llImpCost OR lcPType $ 'RN')) .OR. (ASCAN(laEvntTrig,PADR("ADDOPTION",10)) <> 0);
   .OR. (ASCAN(laEvntTrig,PADR("ADOPTCAT",10)) <> 0) .OR. (ASCAN(laEvntTrig,PADR("PORECPAD",10)) <> 0)
*C126994,1 NNA (END)

*C102172,1 AMH [End]
*C101683,1 WAB - END
  RELEASE PAD _Option OF _MSYSMENU
ENDIF
RELEASE WINDOW (lcBrDtTtl)
POP KEY

*--Normal exit.
IF glQuitting
  *C123616,1  TMI [Start] release the defined option pad for nik nak ,
  *                       also close temp opened file
  IF ASCAN(laEvntTrig,'RELPAD') <> 0
    =gfDoTriger('POSTREC','RELPAD')
  ENDIF
  *C123616,1  TMI [End  ] 

  *E301077,11 comment out.
  *IF laOpFile[1] AND USED('SyuEror')
  *  USE IN SyuEror
  *ENDIF
  *IF laOpFile[1] AND USED('Codes')
    *USE IN Codes
  *  =gfCloseFile('Codes')
  *ENDIF

  *IF laOpFile[2] AND USED('MFGOprHd')
    *USE IN MFGOprHd
  *  =gfCloseFile('MFGOprHd')
  *ENDIF
 
  *IF laOpFile[3] AND USED('MFGOprDt')
    *USE IN MFGOprDt
  *  =gfCloseFile('MFGOprDt')
  *ENDIF
  
  *IF laOpFile[4] AND USED('FabDye')
    *USE IN FabDye
    *B602460,1 The file must be closed is FabDye not MFGOprHd [Begin
    *=gfCloseFile('MFGOprHd')
  *  =gfCloseFile('FabDye')
    *B602460,1 The file must be closed is FabDye not MFGOprHd [End..
  *ENDIF
  
  *IF laOpFile[5] AND USED('Fabric')
    *USE IN Fabric
  *  =gfCloseFile('Fabric')
  *ENDIF
  
  *IF laOpFile[6] AND USED('CutPick')
    *USE IN CutPick
  *  =gfCloseFile('CutPick')
  *ENDIF
  
  *IF laOpFile[7] AND USED('OrdHdr')
    *USE IN OrdHdr
  *  =gfCloseFile('OrdHdr')
  *ENDIF
  
  *IF laOpFile[8] AND USED('OrdLine')
    *USE IN OrdLine
  *  =gfCloseFile('OrdLine')
  *ENDIF
  
  *IF laOpFile[9] AND USED('GLDIST')
    *USE IN GLDIST
  *  =gfCloseFile('GLDIST')
  *ENDIF
  
  *IF laOpFile[10] AND USED('WhsLoc')
    *USE IN WhsLoc
  *  =gfCloseFile('WhsLoc')
  *ENDIF
  
  *IF laOpFile[11] AND USED('ShpmtHdr')
    *USE IN ShpmtHdr
  *  =gfCloseFile('ShpmtHdr')
  *ENDIF
  
  *IF laOpFile[12] AND USED('SYCCURR')
    *USE IN SYCCURR
  *  =gfCloseFile('SYCCURR')
  *ENDIF


  USE IN (lcTmpLine)
  ERASE (gcWorkDir+lcTmpLine+'.DBF')
  ERASE (gcWorkDir+lcTmpLine+'.CDX')
  IF USED(lcTemLoc)
    USE IN (lcTemLoc)
    ERASE (gcWorkDir+lcTemLoc+'.DBF')
    ERASE (gcWorkDir+lcTemLoc+'.CDX')
  ENDIF

  *B602698,1 [BEGIN]
  IF USED(lcTempFile)
    USE IN (lcTempFile)
    ERASE (gcWorkDir+lcTempFile+'.DBF')
    ERASE (gcWorkDir+lcTempFile+'.CDX')
  ENDIF
  *B602698,1 [END..]

  IF USED(lcTmpCtPk)
    USE IN (lcTmpCtPk)
    ERASE (gcWorkDir+lcTmpCtPk+'.DBF')
    ERASE (gcWorkDir+lcTmpCtPk+'.CDX')
  ENDIF

  *C200488,1 WAB (Start) Call triger to delete the Temp. files
  IF ASCAN(laEvntTrig,PADR("CLOSFILS",10)) <> 0
    =gfDoTriger("POSTREC",PADR("CLOSFILS",10))
  ENDIF
  *C200488,1 WAB (End) 

ENDIF
RETURN
*-End...

*:*************************************************************
*! Name     : lpshow
*! Developer: Timour A. K.
*! Date     : 10/10/97               
*! Purpose  : Show Screen Mode Changes (Select and Add) only.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lpShow()
*!*************************************************************
FUNCTION lpShow

DO CASE
  
  *-- S E L E C T  M O D E.
  CASE laScrMode[1]  && Select mode. 
    *--Check uncomplete session (UNCSS).
    lcOtherTmp = lcTmpLine
    IF llGoAndChk AND lfChkUnComS()
      IF USED(lcOtherTmp)
        USE IN (lcOtherTmp)
      ENDIF
      RETURN
    ENDIF
    *C200488,1 WAB (Start)
    IF ASCAN(laEvntTrig,PADR("ZAPTEMP",10)) <> 0
      =gfDoTriger("POSTREC",PADR("ZAPTEMP",10))
    ENDIF
    *C200488,1 WAB (END)
    IF llFirst
      SHOW GET lnType    DISABLE
      SHOW GET pbKey     DISABLE     
      SHOW GET ldTrDate  ENABLE
      SHOW GET ldRcvDate ENABLE
      _CUROBJ=OBJNUM(ldTrDate)
      llFirst=.F.
    ELSE
      IF !llLinkToGl
        SHOW GET ldTrDate ENABLE
      ENDIF
      lcOldValue=0
      IF ( !llLinkToGl OR !EMPTY(lcGLFYear+lcGLPeriod) )
        =lfvType()
      ENDIF
      SHOW WINDOW (lcBrDtTtl) REFRESH
    ENDIF

    *C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
    IF lcPType = 'H'
      lcOldValue = 0
      =lfvType()
      _CUROBJ = OBJNUM(ldTrDate)
    ENDIF
    *C200170,1 AMH [End]

    *SHOW GET pbPONote DISABLE
    *B602832,1 Added to disable browse icon, due to adding a base file.
    SHOW GET pbBrws   DISABLE
    *B602832,1 End.
    SHOW GET pbDyeArr DISABLE
    STORE 0 TO lnTotStk,lnTotDam,lnTotCan
    llIgnorAll=.F.
    =lfActBrow()
    =lfRefresh(lcWinCh0)
    *C123616,1  TMI [Start] empty files for cartons types for nik nak
    IF ASCAN(laEvntTrig,PADR("EMPTMP",10)) <> 0
      =gfDoTriger("POSTREC",PADR("EMPTMP",10))
    ENDIF
    *C123616,1  TMI [End  ] 

  *-- A D D  M O D E.
  CASE laScrMode[4]  

    llCUpDate  = .T.
    llGoAndChk = .T.
    SHOW GETS WINDOW (lcWinCh0) DISABLE ONLY    
    =lfRefresh(lcWinCh0)
    =lfRefresh(lcWinCh2)
    =lfShowScr()
    *SHOW GET pbPONote ENABLE
    SHOW GET pbDyeArr ENABLE
    *--Create uncmsess record.
    IF !llContinue
      =lfCrUnSess('','',.F.) 
    ENDIF
    llContinue = .F.
    =lfActBrow()
    =lfwBrow()

ENDCASE

*C102094,1 AMH Check if the trigger for Cathy Daniel exist then disable
*C102094,1 AMH the user defined fields button.
IF ASCAN(laEvntTrig , PADR('SELCSFLD',10)) <> 0
  SHOW GET pbUsrFields DISABLE 
ENDIF     
*C102094,1 AMH [End]

RETURN

*:*************************************************************
*! Name    : lfShowScr
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Show all objects in lcWinCh5 in Add Mode.
*:*************************************************************
FUNCTION lfShowScr

SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
SHOW GETS WINDOW (lcWinCh5) DISABLE ONLY
IF lcPType='S'
  SHOW GETS WINDOW (lcWinCh1) ENABLE ONLY      
  SHOW GET pbSType   DISABLE
  SHOW GET lcShpCode DISABLE
ELSE
  SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
ENDIF
DO CASE
  CASE lcPType = 'S'
    SHOW GET pbNew    ENABLE
    IF !EOF(lcTmpLine)
      SHOW GET pbRemove ENABLE
      SHOW GET llLoc    ENABLE
      SHOW GET lcRefer  ENABLE
    ENDIF
  *B603062,1 Changed.
  *CASE lcPType $ 'IRMNO' AND !EMPTY(lcGLFYear+lcGLPeriod)
  *C200080,1 AMM Add the dye order type 'D'
  *CASE lcPType $ 'IRMNO' AND (!llLinkToGl OR !EMPTY(lcGLFYear+lcGLPeriod))
  CASE lcPType $ 'IRMNODAE' AND (!llLinkToGl OR !EMPTY(lcGLFYear+lcGLPeriod))
  *C200080,1 AMM end
  *B603062,1 End.
    SHOW GET pbNew    ENABLE
    IF !EOF(lcTmpLine)
      SHOW GET pbRemove ENABLE
      *E301291,4 AMM Consider Issue adornment case
      *IF lcPType = 'N'
      IF lcPType $ 'NA'
      *E301291,4 AMM end
        SHOW GET lnWare DISABLE
        SHOW GET llLoc  DISABLE
      ELSE
        SHOW GET lnWare ENABLE
        SHOW GET llLoc  ENABLE
      ENDIF
      SHOW GET lcRefer  ENABLE
    ENDIF
ENDCASE

*E301223,1 Disable changing issue location if void received P/o Case.
IF lcPType = 'R' AND !EMPTY(POSHDR.CPONO)
  SHOW GET lnWare DISABLE
  SHOW GET llLoc  DISABLE
ENDIF
*E301223,1 End.
IF ! llMFCall AND llMulCurr AND !EOF(lcTmpLine)
  SHOW GET lnRate1 ENABLE
  SHOW GET lnRate2 ENABLE
ENDIF
IF !EOF(lcTmpLine)
  SHOW GET pbEditQ ENABLE
ENDIF
IF !llLinkToGl OR !EMPTY(lcGLFYear+lcGLPeriod)
  SHOW GET pbKey ENABLE
ENDIF
RETURN

*:*************************************************************
*! Name    : lfTrDate
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Valid Posting date.
*:*************************************************************
FUNCTION lfTrDate

*-- If the user pressed "Enter" and did not use the mouse..
*-- We usually use this condition to validate all the key fields..
IF llLinkToGl
*B802312,1 Call check period if trandate was changed or empty yer and period.
 * IF LASTKEY() = 13 AND !MDOWN()

 *B121330,1 NNA 01/25/2004 [Begin] Stop condition that compare with the oldvalue
 * IF ldTrDate<>lcOldValue OR EMPTY(lcGLFYear+lcGLPeriod)         &&LASTKEY() = 13 AND !MDOWN()
 *B121330,1 NNA 01/25/2004 [End]
*B802312,1 End.
   IF !CHECKPRD(ldTrDate,'lcGLFYear','lcGLPeriod','PO')
      _CUROBJ = OBJNUM(ldTrDate)
    ENDIF
ELSE
  IF !llLinkToGl
    ldRcvDate=ldTrDate
    =lfRvDate()
  ELSE
    SHOW GET ldTrDate  DISABLE
    SHOW GET ldRcvDate ENABLE
    _CUROBJ=OBJNUM(ldRcvDate)
  ENDIF
ENDIF
RETURN


*:*************************************************************
*! Name    : lfRvDate
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Valid Receiving date.
*:*************************************************************
FUNCTION lfRvDate
IF llLinkToGl AND ldRcvDate>ldTrDate
  *--The xxxx date cannot be after than the posting date.
  = gfModalGen('TRM42106B42000','DIALOG',IIF(llIssue,'issuing','receiving'))
  _CUROBJ=OBJNUM(ldRcvDate)
  RETURN
ENDIF
*B121330,1 NNA 01/25/2004 [Begin] Check The receiving date if GL not installed
IF !llLinkToGl OR ldRcvDate>ldTrDate
   IF !lfCHECKPRD(ldRcvDate,'lcGLFYear','lcGLPeriod') 
     _CUROBJ = OBJNUM(ldTrDate)	
	Return				 			
   ENDIF
ENDIF
*B121330,1 NNA 01/25/2004 [End]
llVdTypPop = EMPTY(laType[1,1])
*E301291,4 AMM Consider issue adornment case
*IF !(lcPType $ 'RN')
  *IF llMfCall

*C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
*IF !(lcPType $ 'RNA')
IF !(lcPType $ 'RNAH')
*C200170,1 AMH [End]

*E301291,4 AMM end
  IF llMfCall .OR. lcPType = 'E'
    *C200080,1 AMM
    *DIME laType[2,2]    
    DIME laType[3,2]    
    *C200080,1 AMM end
    laType[1,1]='Receive Cutting Ticket'
    laType[1,2]='M'   
    laType[2,1]='Receive Batch'
    laType[2,2]='T'
    *C200080,1 AMM    
    laType[3,1]='Receive Dye Order'
    laType[3,2]='D'    
    *E301291,4 AMM Add a new item for receiving adornment order
    *B603237,1 No need to recive since there is a custom receive.
    IF llWareHous .AND. gfGetMemvar('M_BOMVAR') AND !llSpecHndl     
      DIME laType[4,2]
      laType[4,1]='Receive Adornment P/O'
      laType[4,2]='E'      
    ENDIF
    IF lcPtype = 'E' AND llSpecHndl
      llVdTypPop = .F.
    ENDIF
    *E301291,4 AMM end
    *C200080,1 AMM end

    *C102359,1 AMH Custome receive adornment aorder for JL [Start]
    IF llWareHous .AND. gfGetMemvar('M_BOMVAR') AND ASCAN(laEvntTrig,PADR("RCVADORD",10)) <> 0
      STORE SPACE(0) TO laType
      laType[1,1]='Receive Adornment order'
      laType[1,2]='E'
    ENDIF  
    *C102359,1 AMH [End]
    
  ELSE
    IF llImpCost AND lcCostImp='S'
      *--Imported styles cost by Shipment therefore you cannot receive by P/O.
      
      *E301480,1 NAD (Start) Add new element for the Inter-location Po batch.
      *DIME laType[2,2]
      DIME laType[3,2]
      *E301480,1 NAD (End) 
      
      laType[1,1]='Receive by Shipment'
      laType[1,2]='S'
      laType[2,1]='Receive P/O Batch'
      laType[2,2]='B'
      
      *E301480,1 NAD (Start) Add the new element for the Inter-location Po batch.
      laType[3,1]='Receive Inter Location P/O Batch'
      laType[3,2]='L'
      *E301480,1 NAD (End) 
      lcPType=IIF(lcPType='I','S',lcPType)
    ELSE
      *E301480,1  NAD (Start) App new element in the Pop Up
      *DIME laType[IIF(!llWareHous,3,4),2]
      DIME laType[IIF(!llWareHous,3,5),2]
      *E301480,1  NAD (End)
      laType[1,1]='Receive by P/O'
      laType[1,2]='I'
      laType[2,1]='Receive by Shipment'
      laType[2,2]='S'
      laType[3,1]='Receive P/O Batch'
      laType[3,2]='B'
      
      IF llWareHous AND !llMfCall 
        *E301480,1 NAD (Start) Add new element for the Inter-location Po batch.
        *laType[4,1]='Receive Inter Location P/O'
        *laType[4,2]='O'    
        laType[4,1]='Receive Inter Location P/O Batch'
        laType[4,2]='L'          
        laType[5,1]='Receive Inter Location P/O'
        laType[5,2]='O'
        *E301480,1 NAD (End)
      ENDIF
    ENDIF
  ENDIF
ENDIF
*E301291,4 AMM Consider case of issue adornment order
*lcnTpMode = IIF(lcPType $ 'RN','DISABLE','ENABLE')

*C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
*lcnTpMode = IIF(lcPType $ 'RNA','DISABLE','ENABLE')
lcnTpMode = IIF(lcPType $ 'RNAH','DISABLE','ENABLE')

*C102359,1 AMH Custome receive adornment aorder for JL [Start]
IF lcPType = 'E' AND ASCAN(laEvntTrig,PADR("RCVADORD",10)) <> 0
  lcnTpMode = 'DISABLE'
ENDIF  
*C102359,1 AMH [End]
    
*C200170,1 AMH [End]

*E301291,4 AMM end
SHOW GET lnType &lcnTpMode

*C200170,1 AMH delect case of issue inter-location P/O Batch [Start]
*IF lnType=1
IF lnType=1 .AND. lcPType <> 'H'
*C200170,1 AMH [End]

  SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
  SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
  IF llImpCost AND lcCostImp='S' AND lcPType ='S'
    SHOW WINDOW (lcWinCh4) TOP
    SHOW WINDOW (lcWinCh1) TOP
    =lfActBrow()
    SHOW GETS WINDOW (lcWinCh5) DISABLE ONLY
    SHOW GET pbSType   ENABLE
    SHOW GET lcShpCode ENABLE

  ELSE
*-- C101424,1 [BEGIN] comment the following two lines to prevent enabling 
*-- befor select receiving method [i.e Receive C/T or Receive C/T Batch]
*-- the mose suitable position for this is in lfvType()
    *SHOW GET lcTCode ENABLE
    *SHOW GET ibTCode ENABLE
*-- C101424,1 [END..]  ENDIF
    *B602832,1 Remove the comment of enabeling the lcTcode.
    SHOW GET lcTCode ENABLE
    SHOW GET ibTCode ENABLE
    *C101424,1 Enable Style field. [Begin]    
    *C101424,1 Enable Style field. [Begin]
    IF llMFCall AND lcPType='M'
      SHOW GET lcStyle ENABLE
      SHOW GET ibStyle ENABLE
    ENDIF 
    *C101424,1 Enable Style field. [End  ]

  ENDIF
ENDIF

IF llVdTypPop
  IF ! (llImpCost AND lcCostImp='S' AND lcPType ='S')
    _CUROBJ=OBJNUM(lnType)
  ENDIF
  *lnType = 0
  *KEYBOARD '{SPACEBAR}'

  *B602832,1 Save and restore the old type.
  lnOldTyp = lnType
  lnType   = 0  && 
  KEYBOARD '{SPACEBAR}'
  lnType   = lnOldTyp
  *B602832,1 End.
ENDIF
RETURN
*-- end of lfRvDate.

*--
FUNCTION lfvType
IF lnType = lcOldValue
  RETURN
ENDIF
*B603237,1 Start.
IF !(llSpecHndl AND lcPtype = 'E')
*B603237,1 End.
  lcPType = laType[lnType,2]
  *C200080,1 AMM Update the value of llMFCall to consider the dye order case
  llMulCurr  = IIF(lcPType $ 'DAE',.F.,llUseMCurr)
  llMFCall = ( lcPType $ 'MT' )
  lcMasterLn = IIF(llMFCall,'CUTTKTL','POSLN')
  *-Trancd no.
  lcOthrTrCd = IIF(llMFCall,'3','4')
  lcCanlTrCd = IIF(llMFCall,'4','5')
  =lfCrtUnComp()
  *E301291,4 AMM Set suitable value in note key variable

  *C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
  *IF lcPType $ 'ADE'
  IF lcPType $ 'ADEH'
  *C200170,1 AMH [End]
  
    lcSydKey = SPACE(0)
  ELSE
    lcSydKey = lcoSydKey
  ENDIF
*E301291,4 AMM end
*C200080,1 AMM end
ENDIF

IF llLinkToGl
  SHOW GET ldTrDate DISABLE
ENDIF
DO CASE
  *C200080,1 AMM  Add the dye order type 'D' *E301291,4 AMM Add types of issue/receive adornment 
  *CASE lcPType $ 'IRMNO'
  CASE lcPType $ 'IRMNODAE'
  *C200080,1 AMM end
    SHOW WINDOW (lcWinCh3) TOP 
  *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
  *CASE lcPType $ 'BT'

  *C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
  *CASE lcPType $ 'BTL'
  CASE lcPType $ 'BTLH'
  *C200170,1 AMH [End]

  *E301480,1 NAD (End)
    SHOW WINDOW (lcWinCh4) TOP
    SHOW WINDOW (lcWinCh2) TOP
  CASE lcPType = 'S'
    SHOW WINDOW (lcWinCh4) TOP
    SHOW WINDOW (lcWinCh1) TOP
ENDCASE
SHOW GETS WINDOW (lcWinCh5) DISABLE ONLY
SHOW GETS WINDOW (lcWinCh4) DISABLE ONLY
SHOW GETS WINDOW (lcWinCh3) DISABLE ONLY
SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
=lfActBrow()
STORE SPACE(6) TO lcBatch,lcShpCode
DO CASE
  *C200080,1 AMM  Add the dye order type 'D' 
  *E301291,4 AMM Add Issue/Receive Adornemt order
  *CASE lcPType $ 'IRMNO'
  CASE lcPType $ 'IRMNODAE'
  *C200080,1 AMM end
    SHOW GET lcTCode ENABLE
    SHOW GET ibTCode ENABLE

    *C101424,1 Enable Style field. [Begin]
    IF llMFCall AND lcPType='M'
      SHOW GET lcStyle ENABLE
      SHOW GET ibStyle ENABLE
    ENDIF 
    *C101424,1 Enable Style field. [End  ]

    _CUROBJ=OBJNUM(lcTCode)
  *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
  *CASE lcPType $ 'BT'

  *C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
  *CASE lcPType $ 'BTL'
  CASE lcPType $ 'BTLH'
  *C200170,1 AMH [End]

  *E301480,1 NAD (End) 
    SHOW GET pbBType ENABLE
    SHOW GET lcBatch ENABLE
    _CUROBJ = OBJNUM(lcBatch)
  CASE lcPType = 'S' 
    SHOW GET pbSType   ENABLE
    SHOW GET lcShpCode ENABLE
    _CUROBJ = OBJNUM(lcShpCode)
ENDCASE

=lfRefresh(lcWinCh0)
=lfRefresh(lcWinCh5)
*E301291,4 AMM Consider Issue adornment order case
*lcnTpMode = IIF(lcPType $ 'RN','DISABLE','ENABLE')

*C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
*lcnTpMode = IIF(lcPType $ 'RNA','DISABLE','ENABLE')
lcnTpMode = IIF(lcPType $ 'RNAH','DISABLE','ENABLE')
*C200170,1 AMH [End]

*E301291,4 AMM
SHOW GET lnType &lcnTpMode
SHOW GET ldRcvDate ENABLE
RETURN


*:*************************************************************
*! Name    : lfActBrow
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Activate screen browse.
*:*************************************************************
FUNCTION lfActBrow

lcBrTpNme = IIF(llMfCall,'C/T #',IIF(lcPtype='R','Ret #','P/O #'))
*C200080,1 AMM Add the dye order type 'D'
*E301291,4 AMM Add Issue/Receive adornment order cases
*lcBrInWin = IIF(lcPType$'IRMNO',lcWinCh3,lcWinCh4)
lcBrInWin = IIF(lcPType$'IRMNODAE' , lcWinCh3,lcWinCh4)
*C200080,1 AMM end
lnBrRecNo = RECNO()

*E300935,4 Now MAX Function for quantities is on the screen and browse
*lcBlFields = "cMarker=IIF(lnBrRecNo=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
*             IIF(llByCarton,"cCarton :R :H='Carton':4,","")+;
*             IIF(llMfCall,"Cuttkt","Po")+" :R :H=lcBrTpNme :8,"+;
*             "Style     :R :H=lcStyHdr :24,"+;
*             IIF(llDyelot,"Dyelot :R :15,","")+;
*             IIF(llWareHous ,"lcWHTtl = IIF(lcPType='N',Vendor,cWareCode) :H=IIF(lcPType='N','Source','Location'):10,","")+;
*             IIF(lcPType='N',"cWareCode :H='Target':10,","")+;
*             "TotQty :R :H='Original':8,"+;
*             "TotStk :R :H='Stock'   :8,"+;
*             "TotDam :R :H='Other'   :8,"+;
*             "TotCan :R :H='Cancel'  :8,"+;
*             "TotBal :R :H='Balance' :8,"+;
*             "Reference :R :19"

*B602140,1 Change browse calculated fields to be 0 if less than or equal 0.
*E301291,4 AMM Consider Issue adornment order case
*lcBlFields = "cMarker=IIF(lnBrRecNo=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
             IIF(llByCarton,"cCarton :R :H='Carton':4,","")+;
             IIF(llMfCall,"Cuttkt","Po")+" :R :H=lcBrTpNme :8,"+;
             "Style     :R :H=lcStyHdr :24,"+;
             IIF(llDyelot,"Dyelot :R :15,","")+;
             IIF(llWareHous ,"lcWHTtl = IIF(lcPType='N',Vendor,cWareCode) :H=IIF(lcPType='N','Source','Location'):10,","")+;
             IIF(lcPType='N',"cWareCode :H='Target':10,","")+;
             "nTotQty = MAX(Qty1,0)+MAX(Qty2,0)+MAX(Qty3,0)+MAX(Qty4,0)+;
                        MAX(Qty5,0)+MAX(Qty6,0)+MAX(Qty7,0)+MAX(Qty8,0) :R :H='Original':8,"+;
             "TotStk :R :H='Stock'   :8,"+;
             "TotDam :R :H='Other'   :8,"+;
             "TotCan :R :H='Cancel'  :8,"+;
             "nTotBal = MAX(MAX(Qty1,0)+MAX(Qty2,0)+MAX(Qty3,0)+MAX(Qty4,0)+;
                        MAX(Qty5,0)+MAX(Qty6,0)+MAX(Qty7,0)+MAX(Qty8,0) - ;
                        TotStk - TotDam - TotCan,0) :R :H='Balance' :8,"+;
             "Reference :R :19"
*B603237,1 Start, Set custom browse fields.
IF lcPType $ 'AE' AND llSpecHndl
  =gfDoTriger('MFRCVAP',PADR('SPECL_SET',10))
ELSE
*B603237,1 End.
*B603775,1 (Begin) Fix the bug of wrong Balance.
*lcBlFields = "cMarker=IIF(lnBrRecNo=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
             IIF(llByCarton,"cCarton :R :H='Carton':4,","")+;
             IIF(llMfCall,"Cuttkt","Po")+" :R :H=lcBrTpNme :8,"+;
             "Style     :R :H=lcStyHdr :24,"+;
             IIF(llDyelot,"Dyelot :R :15,","")+;
             IIF(llWareHous ,"lcWHTtl = IIF(lcPType$'NA',Vendor,cWareCode) :H=IIF(lcPType$'NA','Source','Location'):10,","")+;
             IIF(lcPType$'NA',"cWareCode :H='Target':10,","")+;
             "nTotQty = MAX(Qty1,0)+MAX(Qty2,0)+MAX(Qty3,0)+MAX(Qty4,0)+;
                        MAX(Qty5,0)+MAX(Qty6,0)+MAX(Qty7,0)+MAX(Qty8,0) :R :H='Original':8,"+;
             "TotStk :R :H='Stock'   :8,"+;
             "TotDam :R :H='Other'   :8,"+;
             "TotCan :R :H='Cancel'  :8,"+;
             "nTotBal = MAX(MAX(Qty1,0)+MAX(Qty2,0)+MAX(Qty3,0)+MAX(Qty4,0)+;
                        MAX(Qty5,0)+MAX(Qty6,0)+MAX(Qty7,0)+MAX(Qty8,0) - ;
                        TotStk - TotDam - TotCan,0) :R :H='Balance' :8,"+;
             "Reference :R :19"
lcBlFields = "cMarker=IIF(lnBrRecNo=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
             IIF(llByCarton,"cCarton :R :H='Carton':4,","")+;
             IIF(llMfCall,"Cuttkt","Po")+" :R :H=lcBrTpNme :8,"+;
             "Style     :R :H=lcStyHdr :24,"+;
             IIF(llDyelot,"Dyelot :R :15,","")+;
             IIF(llWareHous ,"lcWHTtl = IIF(lcPType$'NA',Vendor,cWareCode) :H=IIF(lcPType$'NA','Source','Location'):10,","")+;
             IIF(lcPType$'NA',"cWareCode :H='Target':10,","")+;
             "nTotQty = MAX(Qty1,0)+MAX(Qty2,0)+MAX(Qty3,0)+MAX(Qty4,0)+;
                        MAX(Qty5,0)+MAX(Qty6,0)+MAX(Qty7,0)+MAX(Qty8,0) :R :H='Original':8,"+;
             "TotStk :R :H='Stock'   :8,"+;
             "TotDam :R :H='Other'   :8,"+;
             "TotCan :R :H='Cancel'  :8,"+;
             "TotBal :R :H='Balance' :8,"+;
             "Reference :R :19"
*B603775,1 (End)             
ENDIF

*E301291,4 AMM end
SELECT (lcTmpLine)
BROWSE FIELDS &lcBlfields;
       NOAPPEND ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       KEY '1'  ;
       TITLE (lcBrDtTtl) ;
       WHEN lfwBrow()    ;
       VALID :F lfvBrow();         
       WINDOW (lcBrInWin) ;
       IN WINDOW (gcBaseWind)         
RETURN

*!*************************************************************
*! Name      : lfwBrow
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose   : When valid function for browse.
*!*************************************************************
FUNCTION lfwBrow
lnBrRecNo = RECNO()
=lfReadLine(EOF())
glFromBrow = .T.
RETURN

*!*************************************************************
*! Name      : lfvBrow
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose   : TO CHECK IF comming from browse to call gfStopBrow() 
*!             function
*!*************************************************************
FUNCTION lfVBrow
IF WONTOP() # (lcBrDtTtl)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfOldValue
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose   : Function to store old value of the current filed.
*!*************************************************************
FUNCTION lfoldvalue
lcOldValue = EVALUATE(SYS(18))
RETURN

*!*************************************************************
*! Name      : lpPopSel
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose   : Landed cost popup selection.
*!*************************************************************
FUNCTION lpPopSel

llEditLCst = (! llEditLCst )
SET MARK OF BAR 1 OF _OPTIONPOP TO llEditLCst
RETURN


*:*************************************************************
*! Name      : lfvNote
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose   : P/O or C/T Notepad.
*:*************************************************************
FUNCTION lfvNote

*--At save level the temp no will replaced with P/O no.
*--When cancel this session the temp no line will removed from notepad.
lcTikt = IIF(llMFCall,&lcTmpLine..Cuttkt,&lcTmpLine..Po)
IF !EMPTY(lcTikt)
  *--Call P/O NotePad.
  =NOTEPAD( IIF(llMFCall,'I','P') , lcTikt )
ENDIF
RETURN

*:*************************************************************
*! Name    : lfTrapKy
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Trap key.
*:*************************************************************
FUNCTION lfTrapKy
IF WONTOP() = lcBrDtTtl
  glFromBrow = .T.
  ON KEY LABEL TAB     DO lpTab
  ON KEY LABEL BACKTAB DO lpShiftTab
  ON KEY LABEL ENTER   llRun=IIF(!EOF(lcTmpLine) AND !EMPTY(&lcTmpLine..Style),lfvEditQty(),.T.)
ENDIF  
RETURN


*!*************************************************************
*! Name    : lfReadAct
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : READ Activate
*!*************************************************************
FUNCTION lfReadAct

=lfRefresh(lcWinCh0)
*--Clear Trap.
IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ENTER

*C123616,1  TMI [Start] After we come from pocrhdr screen we need to refreh the shipment detail browse
IF ASCAN(laEvntTrig,'REFBRW') <> 0 AND FILE(gcWorkDir+'TEMPNAME.TMP')
  =gfDoTriger('POSTREC','REFBRW')
ENDIF
*C123616,1  TMI [End  ] 

RETURN

*!*************************************************************
*! Name      : lpTab
*! Purpose   : Tab key trapping procedure.
*!*************************************************************
PROCEDURE lptab

IF WONTOP()=lcBrDtTtl
  IF laScrMode[1]
    *C200080,1 AMM Add the dye order type 'D'
    *E301291,4 AMM Add Issue/Receive adornment order cases
    *IF lcPType $ 'IRMNO'
    IF lcPType $ 'IRMNODAE'
    *C200080,1 AMM end
      ACTIVATE WINDOW (lcWinCh0)
      _CUROBJ=OBJNUM(lcTCode)
    ELSE
     ACTIVATE WINDOW (lcWinCh0)
     _CUROBJ=OBJNUM(pbcptask)
    ENDIF
  ELSE
    ACTIVATE WINDOW (lcWinCh5)
    DO CASE
     *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
      CASE lcPType = 'B'
      CASE lcPType $ 'BL'
     *E301480,1 NAD (End) 
        _CUROBJ=IIF(llMulCurr AND llEditExRt,OBJNUM(lnRate1),OBJNUM(pbEditQ))
      CASE lcPType $ 'TAE'
        _CUROBJ=OBJNUM(pbEditQ)
      OTHE
        _CUROBJ=OBJNUM(lcRefer)
    ENDCASE
  ENDIF
ELSE
   ACTIVATE WINDOW (lcBrDtTtl)
ENDIF  
RETURN

*!*************************************************************
*! Name      : lpShiftTab
*! Purpose   : Shift Tab key trapping procedure.
*!*************************************************************
PROCEDURE lpshifttab

IF WONTOP()=lcBrDtTtl
  IF laScrMode[1]
    DO CASE
      *C200080,1 AMM Add the dye order type 'D'
      *E301291,4 AMM Add Issue/Receive adornment order
      *CASE lcPType $ 'IRMNO'
      CASE lcPType $ 'IRMNODAE'
      *C200080,1 AMM end
       ACTIVATE WINDOW (lcWinCh0)
       _CUROBJ=OBJNUM(lnType)
      CASE lcPType = 'S'      
        ACTIVATE WINDOW (lcWinCh1)
       _CUROBJ=OBJNUM(lcShpCode)
       *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
      *CASE lcPType $ 'BT'      
      CASE lcPType $ 'BTL'
      *E301480,1 NAD (End) 
        ACTIVATE WINDOW (lcWinCh2)
       _CUROBJ=OBJNUM(lcBatch)
    ENDCASE
  ELSE
    IF lcPType='S'
      ACTIVATE WINDOW (lcWinCh1)
      _CUROBJ=OBJNUM(lcShpRef)
    ELSE
      ACTIVATE WINDOW (lcWinCh0)
      _CUROBJ=OBJNUM(pbCls)
    ENDIF
  ENDIF
ELSE
  ACTIVATE WINDOW (lcBrDtTtl)
ENDIF
RETURN



*!*************************************************************
*! Name    : lfvTCode
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Tran.Code Validation for S and B types.
*!*************************************************************
FUNCTION lfvTCode
*E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
*IF !llBrowse AND ( (lcPType='S' AND EMPTY(lcShpCode)) OR (lcPType$'BT' AND EMPTY(lcBatch)))

*C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
*IF !llBrowse AND ( (lcPType='S' AND EMPTY(lcShpCode)) OR (lcPType$'BTL' AND EMPTY(lcBatch)))
IF !llBrowse AND ( (lcPType='S' AND EMPTY(lcShpCode)) OR (lcPType$'BTLH' AND EMPTY(lcBatch)))
*C200170,1 AMH [End]

*E301480,1 NAD (End) 
  RETURN
ENDIF
lnAlias =SELECT()
llBrowse=.F.
llAbort =.F.
DO CASE
  CASE lcPType='S' AND !lfvShipmnt()
    llAbort =.T.
  *E301480,1 NAD (Start) Add the Inter location Po batch to the condition. 
  *CASE lcPType $ 'BT' AND !lfvBatch()

  *C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
  *CASE lcPType $ 'BTL' AND !lfvBatch()
  CASE lcPType $ 'BTLH' AND !lfvBatch()
  *C200170,1 AMH [End]

   *E301480,1 NAD (End) 
    llAbort =.T.
ENDCASE
IF llAbort
  STORE SPACE(6) TO lcBatch,lcShpCode
  SELECT(lnAlias)
  IF lcPType = 'S'      
    _CUROBJ=OBJNUM(lcShpCode)
  ELSE
    _CUROBJ=OBJNUM(lcBatch)
  ENDIF
  RETURN
ENDIF
IF llLinkToGl
  SHOW GET ldTrDate   DISABLE
  SHOW GET ldRcvDate  DISABLE
ENDIF
SHOW GET lnType     DISABLE
IF lcPType = 'S'      
  SHOW GET PbSType   DISABLE
  SHOW GET lcShpCode DISABLE
ELSE
  *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
  *IF lcPType $ 'BT' 
  IF lcPType $ 'BTL' 
   *E301480,1 NAD (End) 
    SHOW GET PbBType DISABLE
    SHOW GET lcBatch DISABLE
  ENDIF
ENDIF
=lfRefresh(lcWinCh5)
SELECT(lnAlias)
RETURN


*!*************************************************************
*! Name    : lfvShipmnt
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Validate Shipments.
*!*************************************************************
FUNCTION lfvShipmnt


*E301077,11 MAB Open Shipment file[Begin]
= lfOpn_Rest(gcDatadir,'ShpmtHdr','ShpmtHdr')
*E301077,11 MAB Open Shipment file[End  ]

IF !SEEK(lcShpCode,'SHPMTHDR')
  SELECT ShpmtHdr
  lcBrFields = [ShipNo :8:H='Shipment #',]+;
               [Status :1:H="S",]+;
               [Entered :8:H='Entered',]+;
  	           [Cartons :5:H='Cartons',]+;
	           [AirWayB :15:H='Air-Way Bill #',]+;
    	       [ETA     :8:H='E.T.A.',]+;
    	       [TotQty  :7:H='In-Transit',]+;
    	       [Recv_Stk :6:H='Received',]+;
    	       [Recv_Dam :6:H='Damaged',]+;
    	       [Recv_Can :6:H='Canceled',]+;
    	       [cVessel :30:H='Airline/Vessel',]+;
    	       [Reference :H='Reference']

  SELECT SHPMTHDR
  DIME laTempData[1]
  STORE '' TO laTempData
  =gfBrows(.F.,'ShipNo','laTempData','Shipments')
  lcShpCode=laTempData[1]
ENDIF 
IF EMPTY(lcShpCode) OR EOF('SHPMTHDR')
  RETURN .F.
ENDIF
SHOW GET lcShpCode

*--Shipment validation.
IF SHPMTHDR.Status = 'C'
  *--This shipment has been received complete! unable to proceed.
  = gfModalGen('TRM34078B42000','DIALOG','received complete')
  RETURN .F.
ENDIF
IF SHPMTHDR.Status = 'X'
  *--This shipment has been canceled! unable to proceed.
  = gfModalGen('TRM34078B42000','DIALOG','canceled')
  RETURN .F.
ENDIF
*C127341,13 TMI [Start] 
*C127341,1  TMI [Start] if shipment is on hold , do not receive
IF SHPMTHDR.Status <> 'O'
  *-- The shipment is not Open, can not proceed.
  = gfModalGen('TRM34213B00000','DIALOG')
  RETURN .F.
ENDIF
*C127341,1  TMI [End  ] 
*C127341,13 TMI [End  ] 

*C127341,14 TMI [Start] shipment approval is now standard
*-* IF ASCAN(laEvntTrig,'SHPAPRVD')<>0 .AND. !gfDoTriger('POSTREC','SHPAPRVD')
*-*   RETURN .F.
*-* ENDIF  
*-* *C127341,11 TMI [End  ] 
*C127341,14 TMI [End  ] 

SELECT POSLN
SET ORDER TO TAG POSLNSH
IF !SEEK(lcShpCode)
  *--The shipment lines have not been found! unable to proceed.
  = gfModalGen('TRM34077B42000','DIALOG')
  RETURN .F.
ENDIF

llShpPO = .F.
=lfGetInfo()
SELECT POSLN
SET ORDER TO TAG POSLN
RETURN .T.


*!*************************************************************
*! Name    : lfvBatch
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Validate Temp. Batch.
*!*************************************************************
FUNCTION lfvBatch

*E301077,11 MAB Open Required Batch files [Begin]
=lfOpn_Rest(gcDatadir,'CTKTRCVH','CTKTRCVH')
=lfOpn_Rest(gcDatadir,'CTKTRCVL','CTKTRCVL')
*E301077,11 MAB Open Required Batch files [End  ]

SELECT CTKTRCVH

*E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
*IF !SEEK(IIF(llMfCall,'M','I')+lcBatch)

*C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
*IF !SEEK(IIF(llMfCall,'M',IIF(lcPType='L','N','I'))+lcBatch)
IF !SEEK(IIF(llMfCall,'M',IIF(lcPType$'LH','N','I'))+lcBatch)
  *SET FILTER TO cType=IIF(llMfCall,'M','I')
  *SET FILTER TO cType=IIF(llMfCall,'M',IIF(lcPType='L','N','I'))
  SET FILTER TO cType=IIF(llMfCall,'M',IIF(lcPType$'LH','N','I'))
*C200170,1 AMH [End]

*E301480,1 NAD (End) 

  lcBrFields = [TmpRcvNum :8:H='Recv. #',]+;
               [cStatus:1:H="S",]+;
               [cDesc:24:H="Description",]+;
               [dDate:10:H="Date",]+;
               [nTotStk:8:H="Tot.Stk",]+;
               [nTotDam:8:H="Tot.Oth",]+;
               [nTotCan:8:H="Tot.Can"]
  DIME laTempData[1]
  STORE '' TO laTempData
  =gfBrows(.F.,'TMPRCVNUM','laTempData','Temp. Receive Batchs')
  lcBatch=laTempData[1]
  SET FILTER TO 
ENDIF
IF EMPTY(lcBatch) OR EOF('CTKTRCVH')
  RETURN .F.
ENDIF
SHOW GET lcBatch


*--Batch Validation.
IF CTKTRCVH.cStatus = 'P'
  *--This temporary receiving batch is posted. Cannot proceed.
  = gfModalGen('TRM34070B42000','DIALOG','is posted')
  RETURN .F.
ENDIF

*C200170,1 AMH Add case of issued batch for issue inter-location P/O Batch [Start]
IF lcPType = 'H' .AND. CTKTRCVH.cStatus = 'I'
  *--This temporary receiving batch is posted. Cannot proceed.
  = gfModalGen('TRM34070B42000','DIALOG','is issued')
  RETURN .F.
ENDIF
*C200170,1 AMH Add case of issue inter-location P/O Batch [Start]

*C200170,1 AMH remove case of receive inter-location P/O Batch [Start]
*IF CTKTRCVH.cStatus <> 'A'
IF CTKTRCVH.cStatus <> 'A' .AND. lcPType <> 'L'
*C200170,1 AMH [End]

  *--This temporary receiving batch is not approved. Cannot proceed.
  = gfModalGen('TRM34070B42000','DIALOG','is not approved')
  RETURN .F.
ENDIF

*C200170,1 AMH Check if batch is issued in case of receive inter-location P/O Batch [Start]
IF lcPType = 'L' .AND. CTKTRCVH.cStatus <> 'I'

  *--This temporary receiving batch is not issued. Cannot proceed.
  = gfModalGen('TRM34070B42000','DIALOG','is not issued')
  RETURN .F.
ENDIF
*C200170,1 AMH [End]

SELECT CtKtRcvL

*E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
*IF !SEEK(IIF(llMfCall,'M','I')+lcBatch) 

*C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
*IF !SEEK(IIF(llMfCall,'M',IIF(lcPtype='L','N','I')+lcBatch))
IF !SEEK(IIF(llMfCall,'M',IIF(lcPtype$'LH','N','I')+lcBatch))
*C200170,1 AMH [End]

*E301480,1 NAD (End)
 
  *--This temporary receiving batch has no lines. Cannot proceed.
  = gfModalGen('TRM34070B42000','DIALOG','has no lines')
  RETURN .F.
ENDIF
=lfGetInfo()

*C200170,4 AMH Add case of issue inter-location P/O Batch [Start]
IF lcPType = 'H'
  lcSource = CTKTRCVH.VENDOR
ENDIF
*C200170,4 AMH [End]

*C128583,3  TMI [Start] Update the ldRcvDate variable from the Imported temp file for PAN21
IF ASCAN(laEvntTrig,'UPDRCVDT')<>0
  =gfDoTriger('POSTREC','UPDRCVDT')
ENDIF
*C128583,3  TMI [End  ] 
 

RETURN .T.


*!*************************************************************
*! Name    : lfvPO
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Validate P/o AND Return P/o.
*!*************************************************************
FUNCTION lfvPO

PRIVATE lnAlias, lcBrFields 

lcTCode =IIF(llBrowse,'?',lcTCode )
IF !llBrowse AND ( EMPTY(lcTCode) OR (lcTCode = lcOldValue AND MDOWN()) )
  RETURN
ENDIF

llBrowse=.F.
lnAlias =SELECT()
*C200080,1 AMM Add the dye order type 'D' & Type Issue adornment order 'A'
*E301291,4 AMM Consider Issue/Receive adornment order
*lcSeekTyp = IIF(lcPType $ 'RNO',IIF(lcPType='O','N',lcPType),'P')

*E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
*lcSeekTyp = IIF(lcPType $ 'RNODAE',IIF(lcPType='O','N',IIF(lcPType='E','A',lcPType)),'P')
lcSeekTyp = IIF(lcPType $ 'RNODAEL',IIF(lcPType $ 'OL','N',IIF(lcPType='E','A',lcPType)),'P')
*E301480,1 NAD (End)

*E301291,4 AMM end
*C200080,1 AMM end

IF !SEEK(lcSeekTyp+lcTCode,'POSHDR')
  SELECT POSHDR
  DO POSBrow WITH lcTCode,"",lcSeekTyp
ENDIF 

IF EMPTY(lcTCode) OR EOF('POSHDR')
  SELECT(lnAlias)
  _CUROBJ=OBJNUM(lcTCode)
  RETURN
ENDIF
SHOW GET lcTCode

*--P/o Validation.
llAbort=.F.
*C200080,1 AMM Add the dye order type 'D'
*lcPoTtle=IIF(lcPType='R','Return P/o','P/O')
lcPoTtle=IIF(lcPType='R','Return ',IIF(lcPType='D','Dye ',IIF(lcPType='A','Adornment ','')))+'P/O'
*C200080,1 AMM end
lcTxtMsg1=IIF(llIssue,'issuing','receiving')
lcTxtMsg2=IIF(llIssue,'issue','receive')
DO WHILE .T.
  DO CASE
    CASE POSHDR.Status = 'S'
      *--XXX status is XXX. Therefore,no receivings can be done.
      = gfModalGen('INM34055B42000','DIALOG',lcPoTtle+'|'+'Closed'+'|'+'receivings')
      llAbort=.T.
      EXIT 
    CASE POSHDR.Status = 'B'
      *--XXX status is XXX. Therefore,no receivings can be done.
      = gfModalGen('INM34055B42000','DIALOG',lcPoTtle+'|'+'Bid'+'|'+lcTxtMsg1)
      llAbort=.T.
      EXIT 
    CASE POSHDR.Status = 'H'
      *--XXX status is Hold since a cost sheet has not been created yet. Therefore,no receivings can be done.
      = gfModalGen('INM34056B42000','DIALOG',lcPoTtle)
      llAbort=.T.
      EXIT 
    CASE POSHDR.Status = 'X'
      *--XXX has been canceled. Not allowed to receive.
      = gfModalGen('INM34057B42000','DIALOG',lcPoTtle+'|'+lcTxtMsg2)
      llAbort=.T.
      EXIT 
    CASE POSHDR.Status = 'C'
      *E301291,4 AMM Consider Issue adornment order case
      *IF lcPType = 'N'
      IF lcPType $ 'NA'
      *E301291,4 AMM end
        *--XXX status is complete. Therefore,no issuings can be done.
        = gfModalGen('INM34055B42000','DIALOG',lcPoTtle+'|'+'complete'+'|'+lcTxtMsg1)
        llAbort=.T.
        EXIT
      ELSE
        *--XXX is completely received. Do you wish to continue ?,\<Yes;\<No
        *B604646,1 MHM 07/17/2001 Don't allow Exceed receiving Quantity [Start]
        *IF gfModalGen('INM34058B42002','DIALOG',lcPoTtle) = 2
        *  llAbort=.T.
        *  EXIT 
        *ENDIF
        IF lcPtype = 'O'
          = gfModalGen('INM00000B00000','','','',lcPoTtle +' is completely received. Cannot proceed.')
          llAbort=.T.
          EXIT 
        ELSE
          IF gfModalGen('INM34058B42002','DIALOG',lcPoTtle) = 2
            llAbort=.T.
            EXIT 
          ENDIF
        ENDIF  
        *B604646,1 MHM 07/17/2001 [End]
      ENDIF
  ENDCASE

  *B605514,1 HBG 17/03/2002 IF the Po is included in a shippment , cann't receive by PO [Begin]
  IF SEEK('P'+lcTCode,'POSLN') 
    lcPrvAlis = ALIAS()
    SELECT POSLN
    LOCATE REST WHILE cStytype+Po+Style+STR(Lineno,6)+trancd = 'P'+lcTCode;
           FOR Trancd='3' AND TotQty > 0
    IF FOUND()
      = gfModalGen('INM34181B00000','DIALOG',POSLN.shipno)
      
      *B606376,1 KHM 08/13/2002 (Begin) Commented out in order to allow countinuing the receiving process.
      *llAbort=.T.
      *B606376,1 KHM 08/13/2002 (End)
      
      EXIT
    ENDIF  
    SELECT (lcPrvAlis)
  ENDIF
  *B605514,1 [End]]

  *-- Check if this PO BOM has materials.
  *E301291,4 AMM Consider cases of Dye PO, Issue/Receive Adornment orders
  *IF !(lcPType $ 'RNO') AND llImpCost  
  IF !(lcPType $ 'RNODAE') AND llImpCost  
  *E301291,4 AMM end
    llMatExist = .F.

    *E301077,11 MAB Open Cut Ticket BOM file here [Begin
    IF !llOpnKtBom
      llOpnKtBom = lfOpn_Rest(gcDatadir,'CTKTBOM','CTKTBOM')
    ENDIF  
    *E301077,11 MAB Open Cut Ticket BOM file here [End..

    SELECT cTktBom
    IF SEEK('I'+lcTCode)
      LOCATE REST WHILE cImTyp+CutTkt='I'+lcTCode FOR ;
                        cCatgTyp $ 'FS' .OR. (cCatgTyp = 'T' .AND. Trim_Invt)
      llMatExist = FOUND()
    ENDIF
    *--No cost items have been applied against this P/o. Are you sure you want to receive ?

    IF llMatExist
      *E301077,11 MAB Open BOM Cost file here [Begin
      IF !llOpnBmCst
        llOpnBmCst = lfOpn_Rest(gcDatadir,'BOMCOST','BOMCOST')
      ENDIF  
      *E301077,11 MAB Open BOM Cost file here [End..

      SELECT BOMCOST
      SET ORDER TO TAG Pobomcls
      IF llMatExist .AND. !SEEK('I'+lcTCode,'BOMCOST') .AND. ;
          gfModalGen('QRM34071B42002','DIALOG') = 2
        llAbort=.T.
      ENDIF 
    ENDIF
  ENDIF
  EXIT

ENDDO
SELECT POSHDR

*B804539,1 AMH Fix the bug of issue ador. order for on hold sales order for JL [Start]
IF lcPType = 'A' .AND. ASCAN(laEvntTrig,PADR("VADORORD",10)) <> 0
  =gfDoTriger("PORCVAP",PADR("VADORORD",10))
ENDIF
*B804539,1 AMH [End]

IF llAbort
  lcTCode=SPACE(6)
  SELECT(lnAlias)
  _CUROBJ=OBJNUM(lcTCode)
  RETURN
ENDIF


SELECT POSLN
SET ORDER TO TAG POSLN
lcKey=lcSeekTyp+lcTCode

IF !SEEK(lcKey,'POSLN')
  *-The lines for this 'P/O' are missing ! cannot proceed.
  =gfModalGen('TRM34017B42000','DIALOG','P/O')
  lcTCode=SPACE(6)
  SELECT(lnAlias)
  _CUROBJ=OBJNUM(lcTCode)
  RETURN
ENDIF

*--Check InTransit record.
*E301291,4 AMM Add Receive adornment order type 'E'
*IF lcPType = 'O'
IF lcPType $ 'OE'
*E301291,4 AMM end
  LOCATE REST WHILE cStyType+Po=lcKey FOR Trancd='6'
  IF !FOUND()
    *E301291,4 AMM adjust message 
    *=gfModalGen('TRM34109B42000','DIALOG')
    IF lcPType = 'O'
      *--The inter Location purchase order does not issued from the targer location yet, Unable to proceed!.
      =gfModalGen('TRM34109B42000','DIALOG')
    ELSE
      *--The Adornment purchase order hasn't been issued yet from the targer location, cannot proceed!.
      =gfModalGen('TRM38176B00000','DIALOG')
    ENDIF
    *E301291,4 AMM end
    lcTCode=SPACE(6)
    SELECT(lnAlias)
    _CUROBJ=OBJNUM(lcTCode)
    RETURN
  ENDIF
ENDIF

*E301069,1
*-- Do you wish to select xxxx lines, Manually (by line) or Automatic (all) ?
*--  <Manually>   <Automatic>.
SELECT (lcTmpLine)
LOCATE FOR PO=lcTCode
IF !FOUND()
  *E301223,1 if Voide recieded P/O use always automatic.
  IF lcPType = 'R' AND !EMPTY(POSHDR.CPONO)
    lcAuto = 'A'
  ELSE
  *E301223,1 End.
    lcAuto = IIF(gfModalGen('QRM34126B34008','DIALOG',lcPoTtle) = 1,'M','A')
  ENDIF
ELSE
  lcAuto = IIF(LAUTOMODE,'A','M')
  IF LAUTOMODE
    *--This XXXX already selected.
    = gfModalGen('INM34128B42000','DIALOG',lcPoTtle)
    lcTCode=SPACE(6)
    SELECT(lnAlias)
    _CUROBJ=OBJNUM(lcTCode)
    RETURN
  ENDIF
ENDIF

SELECT POSLN
IF lcAuto = 'A'
  SET RELA TO Style INTO Style
  *E301291,4 AMM Cosider receive adornment order case
  *lcFTrnCd = IIF(lcPType='O','6','1')
  lcFTrnCd = IIF(lcPType$'OE','6','1')
  *E301291,4 AMM end
  SCAN WHILE cStyType+Po=lcKey FOR Trancd=lcFTrnCd
    lcStyle = POSLN.Style
    lcQltyKey = IIF(SEEK(lcStyle,'STYLE'),STYLE.cStyGrade,'1')
    lnRecNo = RECNO('POSLN') 
    IF SEEK('1'+SPACE(3)+po+style+dyelot+cwarecode+STR(lineno,6),lcTmpLine)
      LOOP
    ENDIF
    =lfvStyle(POSLN.LineNo)  
    SELECT POSLN
    GOTO lnRecNo
    IF lcAuto = 'X'
      *--The style xxxxx will be ignored. 
      = gfModalGen('INM34127B42000','DIALOG',ALLT(lcStyle))
      lcAuto = 'A'
    ENDIF 
  ENDSCAN
  SET RELA TO
  SELECT (lcTmpLine)
  GO TOP
  =lfActBrow()
  SELECT(lnAlias)
  RETURN
ENDIF

*B602698,1[BEGIN] Call procedure to calculate open in tmp file
*         Set relation between the master file and the tmp
*         add &lcTempFile..TOTQTY to the lcBrFields        
*SET RELA TO Style INTO Style
*lcBrFields = "Style     :R :H=lcStyHdr :25,"+;
             "Style.Desc1 :R :H='Desc':45,"+;
             "lcShpTTl=IIF(!EMPTY(Account),Account,cWareCode) :H='ShipTo',"+;
             "TotQty      :R :H='Quantity':P='9999999',"+;
             IIF(llCostPrv,"nCost1 :R :H='Price':P='9999999.999',","")+;
             "Reference   :R"

DO lpGetOpen
SET RELA TO Style INTO Style
SET RELA TO cstytype+po+style+STR(lineno,6)+trancd INTO (lcTempFile) ADDI 
*B602965,1 Start.
*lcBrFields = "Style     :R :H=lcStyHdr :25,"+;
             "Style.Desc1 :R :H='Desc':45,"+;
             "lcShpTTl=IIF(!EMPTY(Account),Account,cWareCode) :H='ShipTo',"+;
             "TotQty      :R :H='Quantity':P='9999999',"+;
             IIF(lcPType $ 'NO',"","&lcTempFile..TOTQTY :R :H='Open' :10,")+;
             IIF(llCostPrv,"nCost1 :R :H='Price':P='9999999.999',","")+;
             "Reference   :R"
lcBrFields = "Style :R :H=lcStyHdr :25,"+;
             "Style.Desc1 :R :H='Desc':45,"+;
             "lcShpTTl=IIF(!EMPTY(Account),Account,cWareCode) :H='ShipTo',"+;
             "TotQty :R :H='Quantity':P='9999999',"
*E301291,4 AMM Consider issue/receive adornment order cases
*IF !(lcPType $ 'NO')
IF !(lcPType $ 'NOAE')
*E301291,4 AMM end
  lcBrFields = lcBrFields +"lnOpnQt=IIF(&lcTempFile..TOTQTY<0,0,&lcTempFile..TOTQTY) :R :H='Open' :10,"
ENDIF
IF llCostPrv
  lcBrFields = lcBrFields +"nCost1 :R :H='Price':P='9999999.999',"
ENDIF
lcBrFields = lcBrFields + "Reference :R"
*B602965,1 End.
*B602698,1[END..]

DIMENSION laTemp[1]
laTemp = ''
=lfActBrow()
SELECT POSLN
*E301291,4 AMM Consider Receive adornment order case
*lcFTrnCd = IIF(lcPType='O','6','1')
lcFTrnCd = IIF(lcPType$'OE','6','1')
*E301291,4 AMM end

*B603625,1 AMH Fix the bug of not taking care of adornment order case [Start]
*=ARIABROW([lcKey FOR Trancd=lcFTrnCd],'Select purchase order Line',gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','Style','laTemp')

*B605082,1 AMH Adding the case of dye order case [Start]
*lcBrowTitl = IIF(lcPType='R','return purchase order',IIF(lcPType='N','inter location purchase order' ,IIF(lcPType$'AE','adornment order','purchase order')))
lcBrowTitl = IIF(lcPType='R','return purchase order',IIF(lcPType='N','inter location purchase order' ,IIF(lcPType$'AE','adornment order',IIF(lcPType='D','Dye order','purchase order'))))
*B605082,1 AMH [End]

=ARIABROW([lcKey FOR Trancd=lcFTrnCd],'Select ' + lcBrowTitl + ' Line',gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','Style','laTemp')
*B603625,1 AMH [End]

lcStyle=laTemp[1]
=lfActBrow()
lcQltyKey = IIF(SEEK(POSLN.Style,'STYLE'),STYLE.cStyGrade,'1')
SELECT POSLN
SET RELATION TO

IF EMPTY(lcStyle)
  *E301291,4 AMM Consider issue/receive adornment cases
  *IF lcPType $ 'NO'
  IF lcPType $ 'NOAE'
  *E301291,4 AMM end
    lcTCode=SPACE(6)
    _CUROBJ=OBJNUM(lcTCode)
  ELSE
    SHOW GET ibTCode DISABLE
    SHOW GET lcTCode DISABLE
    SHOW GET ibStyle ENABLE
    SHOW GET lcStyle ENABLE
    lcStyle=POSLN.Style
    _CUROBJ = OBJNUM(lcStyle)
  ENDIF
ELSE
  =lfvStyle(POSLN.LineNo)
ENDIF
SELECT(lnAlias)
RETURN

*!*************************************************************
*! Name    : lfvStyle
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Validate style.
*!*************************************************************
FUNCTION lfvStyle
PARA lnPOLLnNo

*C101424,1 If calling from manufactring module to receive cut ticket By Style [Begin]
IF llMFCall AND lcPType='M'
  IF llBrowse OR (!EMPTY(PADR(lcStyle,lnMjrWid)) AND EMPTY(lcTCode))
    = lfvStyCut()
  ENDIF
  RETURN
ENDIF
*C101424,1 If calling from manufactring module to receive cut ticket By Style [End  ]

lcStyle=IIF(llBrowse,'?',lcStyle)
IF lcAuto = 'M' AND lcStyle = lcOldValue AND MDOWN()
  RETURN
ENDIF
lnAlias =SELECT()
SELECT STYLE
SET ORDER TO TAG StyQlty
IF llBrowse OR (!EMPTY(SUBSTR(lcStyle,1,1)) AND !SEEK(lcQltyKey+lcStyle,'STYLE'))
  llbrowse = .F.
  *C200080,1 AMM Add the dye order type 'D'
  *E301291,4 AMM Consider Issue/Receive adornment order cases
  *SET FILTER TO cStyGrade=lcQltyKey AND Make=llMFCall
  *C102358,4 AMH Filter to !make for Jl [Start]
  *SET FILTER TO cStyGrade=lcQltyKey AND Make = (llMFCall .OR. lcPType $ 'DAE')
  IF ASCAN(laEvntTrig,PADR("RCVADORD",10)) <> 0
    SET FILTER TO cStyGrade=lcQltyKey AND Make = (llMFCall .OR. lcPType = 'D')
  ELSE
    SET FILTER TO cStyGrade=lcQltyKey AND Make = (llMFCall .OR. lcPType $ 'DAE')
  ENDIF
  *C102358,4 AMH [End]
  *C200080,1 AMM end
  GO TOP
  IF EOF()
    *--No styles found for this quality. 
    =gfModalGen('TRM42087B42000','DIALOG')
    lcStyle = SPACE(19)
  ELSE
    lcHdEsc1= ON('KEY','ESC')
    ON KEY LABEL ESC 
    lcStyle = gfStyBrw('I','','',.F.)
    ON KEY LABEL ESC &lcHdEsc1
    =lfActBrow()
  ENDIF 
  SELECT STYLE
  SET FILTER TO 
ENDIF
SET ORDER TO TAG Style

IF EMPTY(SUBSTR(lcStyle,1,1))

  SHOW GET ibTCode ENABLE
  SHOW GET lcTCode ENABLE
  SHOW GET ibStyle DISABLE
  SHOW GET lcStyle DISABLE

  SELECT(lnAlias)
  RETURN
ENDIF

*--Check if it was entered.
*E300935,4 adjust condition because Imported styles can have dyelots.
*IF TYPE('lnPOLLnNo')='N' AND ;
*   SEEK('1'+SPACE(3)+lcTCode+lcStyle+STR(POSLN.LineNo,6),lcTmpLine)
*E301291,4 AMM Consider Issue/Receive adornment order cases
*IF lcAuto = 'M' AND ((llCMInstld AND llPOSale) OR !(llWareHous AND !(lcPType $ 'N'))) AND (!llDyelot OR STYLE.cDye_Flg<>'Y') AND TYPE('lnPOLLnNo')='N' AND ;
   SEEK('1'+SPACE(3)+lcTCode+lcStyle,lcTmpLine)
IF lcAuto = 'M' AND ((llCMInstld AND llPOSale) OR !(llWareHous AND !(lcPType $ 'NA'))) AND (!llDyelot OR STYLE.cDye_Flg<>'Y') AND TYPE('lnPOLLnNo')='N' AND ;
   SEEK('1'+SPACE(3)+lcTCode+lcStyle,lcTmpLine)
*E301291,4 AMM end

   SELECT (lcTmpLine)
   LOCATE REST WHILE TranCd+cCarton+Po+Style+Dyelot+cWareCode+STR(LineNo,6) = ;
                     '1'+SPACE(3)+lcTCode+lcStyle FOR LineNo = lnPOLLnNo
  IF FOUND()
    *E301291,4 AMM Consider Issue/Receive adornment PO
    *lcMTxt= IIF(lcPType='N','issue inter location ','receiving ')+IIF(lcPType='S','shipment',IIF(lcPType='R','return P/O','P/O'))

    *B603625,1 AMH Fix the bug of message style has been entered in this issue P/O in
    *B603625,1     case of issue adornment order [Start]
    *lcMTxt= IIF(lcPType$'NA','issue ','receiving ')+IIF(lcPType='S','shipment',IIF(lcPType='R','return P/O',IIF(lcPType='N','inter location' ,IIF(lcPType='AE','adornment P/O','P/O'))))
    lcMTxt= IIF(lcPType$'NA','issue ','receiving ')+IIF(lcPType='S','shipment',IIF(lcPType='R','return P/O',IIF(lcPType='N','inter location' ,IIF(lcPType$'AE','adornment P/O','P/O'))))
    *B603625,1 AMH [End]
    
    *E301291,4 AMM end
    *-This XXXX has been entered on this XXXX.
    =gfModalGen('TRM42107B42000','DIALOG','Style'+'|'+lcMTxt)
    _CUROBJ=OBJNUM(lcTCode)
    SELECT(lnAlias)
    RETURN
  ENDIF  
  SELECT STYLE

ENDIF

SHOW GET lcStyle

llAbort   = .F.
llNewItem = .F.
=SEEK(lcStyle,'STYLE')

*-- Validation checks loop for Style.....
*- P/O styles check.
DO WHILE .T.

  *C200080,1 AMM  Adjust to fit case of Dye order
  *E301291,4 AMM Adjust to fit in case of issue/receive adornment PO
  *IF !STYLE.Make
  *C102359,4 AMH Don't display this message for JL [Start]
  *IF IIF(lcPType$'DAE',STYLE.Make,!STYLE.Make )
  IF IIF(lcPType$'DAE' .AND. (ASCAN(laEvntTrig,PADR("RCVADORD",10)) = 0),STYLE.Make,!STYLE.Make )
  *C102359,4 AMH [End]
  *C200080,1 AMM end
    *E301291,4 AMM Consider issue/receive adornment PO cases
    *IF llImpCost AND !(lcPType $ 'RNO')
    IF llImpCost AND !(lcPType $ 'RNOAE')
    *E301291,4 AMM end
      IF !llOpnBom
        llOpnBom = lfOpn_Rest(gcDatadir,'BOM','BOM')
      ENDIF  
      *C200080,1 AMM start Validate the style in case of dye order
      IF lcPType = 'D' .AND. !EMPTY(POSHDR.COLOR) .AND. POSHDR.COLOR # '*' 
        DIMENSION laMajSeg[1,1]
        *C200080,1 AMM Variable to get the position of color segment inside the style code structure
        lnColStru = 0
        *C200080,1 AMM Get the style code structure and the color segment position as well
        =gfItemMask(@laMajSeg)
        FOR lnI = 1 TO ALEN(laMajSeg)
          IF laMajSeg[lnI,1] = 'C'
            lnColStru = lnI
            EXIT
          ENDIF
        ENDFOR
        IF  SUBSTR(lcSTYLE,laMajSeg[lnColStru,4],LEN(laMajSeg[lnColStru,3])) # POSHDR.COLOR
          =gfModalGen('TRM38174B00000','DIALOG')
          llAbort=.T.
          EXIT
        ENDIF
      ENDIF
      *C200080,1 AMM end
      IF SEEK(SUBSTR(lcStyle,1,lnMjrWid),'BOM')
          SELECT BOM 
          LOCATE REST WHILE cItmMajor=SUBSTR(lcStyle,1,lnMjrWid) ;
                  FOR LIKE(STRTRAN(cItmMask,'*','?'),PADR(lcStyle,19))
          IF !FOUND()
            *-No cost lines found in the cost sheet, Cannot proceed!
            =gfModalGen('TRM34037B42000','DIALOG')        
            llAbort=.T.
            EXIT
          ENDIF
          *C200080,1 AMM start In case of dye order
          IF lcPType = 'D'
            *C200080,1 AMM Get the dye operation
            lcDyeOpr = gfGetMemvar('M_DYEOPR')
            *C200080,1 AMM check if the style has the dye operation in its cost sheet
            =SEEK(SUBSTR(lcStyle,1,lnMjrWid),'BOM')
            LOCATE FOR mfgcode=lcDyeOpr WHILE citmmajor=SUBSTR(lcStyle,1,lnMjrWid)
            IF !FOUND()
              *C200080,1 AMM Message : 'Style cost sheet has no dye operation ['+lcDyeOpr+'], Cannot accept this style in Dye order.'
              =gfModalGen('TRM38177B00000','DIALOG',lcDyeOpr)
              *C200080,1 AMM end
              llAbort=.T.
              EXIT
            ELSE
              m.Gros_Price = BOM.untCost
              m.nCost1     = BOM.untCost
            ENDIF
          ENDIF
          *C200080,1 AMM end
          
      ELSE
        *-No cost lines found in the cost sheet, Cannot proceed!
        =gfModalGen('TRM34037B42000','DIALOG')        
        llAbort=.T.
        EXIT
      ENDIF
    ENDIF
 
  ELSE 
    *C200080,1 AMM Adjust message
    *-Conflict ! Only purchased styles are allowed to enter here, Cannot proceed!
    *=gfModalGen('TRM34039B42000','DIALOG')
    *C102359,4 AMH Don't display this message for JL [Start]
    *IF lcAType $ 'DA'
    IF lcPType $ 'DAE'
      *-Conflict ! Only manufactored styles are allowed to enter here, Cannot proceed!
      *=gfModalGen('TRM38015B34000','DIALOG','manufactored styles')
      IF (ASCAN(laEvntTrig,PADR("RCVADORD",10)) = 0)
        =gfModalGen('TRM38015B34000','DIALOG','manufactored styles')
      ELSE
        *-Conflict ! Only purchased styles are allowed to enter here, Cannot proceed!
        =gfModalGen('TRM34039B42000','DIALOG')
      ENDIF
    ELSE
      *-Conflict ! Only purchased styles are allowed to enter here, Cannot proceed!
      *=gfModalGen('TRM34039B42000','DIALOG')
      IF (ASCAN(laEvntTrig,PADR("RCVADORD",10)) = 0)
        =gfModalGen('TRM34039B42000','DIALOG')
      ELSE
        *-Conflict ! Only manufactored styles are allowed to enter here, Cannot proceed!
        =gfModalGen('TRM38015B34000','DIALOG','manufactored styles')
      ENDIF
    ENDIF
    *C102359,4 AMH [End]
    *C200080,1 AMM end
    llAbort=.T.
    EXIT
  ENDIF 

  IF STYLE.cDivision <> POSHDR.cDivision
    *-Conflict ! styles restricted to division XXXX, Cannot proceed!
    =gfModalGen('TRM34041B42000','DIALOG',ALLTRIM(POSHDR.cDivision) )
    llAbort=.T.
    EXIT
  ENDIF

  IF STYLE.Status='X'
    *-This is a canceled style. Not allowed to enter here, Cannot proceed!
    =gfModalGen('TRM34040B42000','DIALOG')        
    llAbort=.T.
    EXIT
  ENDIF

  *--Item not on P/O , Modify P/O?
  llNewItem = .F.
  *C200080,1 AMM Add the dye order type 'D'
  *E301291,4 AMM Add Issue/Receive adornment PO types 'AE'
  *lcSeekTyp = IIF(lcPType $ 'RNO',IIF(lcPType='O','N',lcPType),'P')
  
  *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
  *lcSeekTyp = IIF(lcPType $ 'RNODAE',IIF(lcPType='O','N',IIF(lcPType='E','A',lcPType)),'P')
  lcSeekTyp = IIF(lcPType $ 'RNODAEL',IIF(lcPType $ 'OL','N',IIF(lcPType='E','A',lcPType)),'P')
  *E301480,1 NAD (End)
  *C200080,1 AMM end
  IF !SEEK(lcSeekTyp+lcTCode+lcStyle,'POSLN')
     *--Item not on P/O! Modify P/O.<Yes\<No
     IF gfModalGen('QRM34072B42002','DIALOG') = 2
       llAbort = .T.
       EXIT
     ELSE
       llNewItem = .T.
     ENDIF
  ELSE
    IF TYPE('lnPOLLnNo')='N'
      =SEEK(lcSeekTyp+lcTCode+lcStyle+STR(lnPOLLnNo,6),'POSLN')
    ENDIF
  ENDIF
  *E301291,4 AMM start
  *C102359,1 AMH Custome receive adornment aorder for JL [Start]
  *IF lcPType = 'E' .AND. POSLN.nCost2 = 0
  IF lcPType = 'E' .AND. POSLN.nCost2 = 0 .AND. ASCAN(laEvntTrig,PADR("RCVADORD",10)) = 0
  *C102359,1 AMH [End]
    *E301291,4 AMM Message 'No adornment cost has been applied to this line, Cannot receive.'
    =gfModalGen('TRM38182B00000','DIALOG')
    llAbort=.T.
    EXIT
  ENDIF
  *E301291,4 AMM end

   
  =SEEK(lcStyle,'STYLE')
  
  lcWareCode = IIF(POSHDR.lMultiWare AND !llNewItem,POSLN.cWareCode,POSHDR.cWareCode)
  *E301291,4 AMM Consider Case of issue adornment PO
  *lcWareCode = IIF(lcPType = 'N',PADR(POSHDR.Vendor,6),lcWareCode)
  lcWareCode = IIF(lcPType $ 'NA',PADR(POSHDR.Vendor,6),lcWareCode)
  *E301291,4 AMM end
  *E301291,4 AMM Add Issue/Receive adornment PO types 'AE'
  *IF llWareHous AND !(lcPType $ 'NO')
  IF llWareHous AND !(lcPType $ 'NOAE')
  *E301291,4 AMM end
    SELECT STYDYE
    IF !SEEK(PADR(lcStyle,19)+lcWareCode+SPACE(10))
      IF ! (POSHDR.lMultiWare)
        *-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
        IF lcAuto = 'M'
          IF gfModalGen('QRM34048B42006','DIALOG',ALLTRIM(lcStyle)+'|'+lcWareCode) = 1
            DO gpAdStyWar WITH lcStyle,SPACE(10),lcWareCode
          ELSE
            llAbort=.T.
            EXIT
          ENDIF
        ELSE
          DO gpAdStyWar WITH lcStyle,SPACE(10),lcWareCode        
        ENDIF
      ELSE
        lcWareCode=STYLE.cDefWare
      ENDIF
    ENDIF
  ENDIF
  
  *E300935,4 declare lcdyelot because imported styles can have dyelot. [begin]
  lcDyelot = IIF(!llNewItem AND llDyelot AND STYLE.cDye_Flg = 'Y',POSLN.Dyelot,'')
  *E300935,4 declare lcdyelot because imported styles can have dyelot. [end]
  
  *E300935,4 change next condition to support dyelots. [begin]
  *IF llImpCost AND !(lcPType $ 'RNO') AND !gfCkWhsDye('I',lcTCode,lcStyle,'','',lcWareCode)
  *E301291,4 AMM Add Issue/Receive adornment order cases
  *IF llImpCost AND !(lcPType $ 'RNO') AND !gfCkWhsDye('I',lcTCode,lcStyle,'',lcDyelot,lcWareCode)

  *B804345,1 AMH Commenting the following lines because we do not need to check  
  *B804345,1     if the fabric is found in the ware house of the PO [Start]
  *IF llImpCost AND !(lcPType $ 'RNOAE') AND !gfCkWhsDye('I',lcTCode,lcStyle,'',lcDyelot,lcWareCode)
  *E301291,4 AMM end
    *llAbort=.T.
  *ENDIF
  *B804345,1 AMH [End]

  EXIT
ENDDO

IF llAbort
  lcStyle = SPACE(19)
  SHOW GET lcStyle
  _CUROBJ = OBJNUM(lcStyle)
  SELECT(lnAlias)
  lcAuto = IIF(lcAuto = 'A','X',lcAuto)
  RETURN
ENDIF

SHOW GET ibTCode  DISABLE
SHOW GET lcTCode  DISABLE
*SHOW GET ibStyle  DISABLE
*SHOW GET lcStyle  DISABLE

*--Get a lot no and last operation.
llSpecLot  = .F.
lcLotNo    = SPACE(2)
lcClrLstOp = SPACE(6)
*C200080,1 AMM Add the dye order type 'D'
*IF llImpCost AND lcPType='I' AND !llNewItem AND !EMPTY(POSHDR.cLastOpr)
*B603385,1 Start.
*IF llImpCost AND lcPType$'ID' AND !llNewItem AND !EMPTY(POSHDR.cLastOpr)
IF llImpCost AND lcPType$'ID' AND !llNewItem
*C200080,1 AMM end
  *lcLotNo = lfSelLots(STR(POSLN.LineNo,6))
  lcLotNo = lfSelLots(STR(POSLN.LineNo,6),POSHDR.cLastOpr)
ENDIF
*B603385,1 End.

llShpPO = (lcPType='S')

*E300935,4 Check for dyelots before call a function that get line information.
*=lfGetInfo()
*E300935,4 The next block is instead of previous line of code.

*E300935,4 Block -> [Begin]
= SEEK(lcStyle,'STYLE')

*E300935,4 Validate dyelot and location is in dyelot field and location field.
*E301291,4 AMM Consider cases of issue/receive adornment PO
*IF lcAuto = 'M' AND ((!(llCMInstld AND llPOSale) AND llWareHous) OR llDyelot) AND STYLE.cDye_Flg = 'Y' AND !(lcPType $ 'ON')
IF lcAuto = 'M' AND ((!(llCMInstld AND llPOSale) AND llWareHous) OR llDyelot) AND STYLE.cDye_Flg = 'Y' AND !(lcPType $ 'ONAE')
*E301291,4 AMM end
  SHOW GET lcDyelot ENABLE
 _CUROBJ = OBJNUM(lcDyelot)     
ELSE

  *-- MAB
  *E301291,4 AMM Consider cases of issue/receive adornment order
  *IF (lcPType $ 'ON') AND !EMPTY(lcDyelot)
  IF (lcPType $ 'ONAE') AND !EMPTY(lcDyelot)
  *E301291,4 AMM end
    =lfvDyelot()
  ELSE
    SHOW GET lcDyelot DISABLE
    *=lfGetInfo()
    = lfChkLine()    && Check for existance of default location.
  ENDIF
  *-- MAB
  
ENDIF
*E300935,4 Block -> [End]

RETURN


*!*************************************************************
*! Name    : lfvCuttkt
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Validate C/ts.
*!*************************************************************
FUNCTION lfvCuttkt
PRIVATE lnAlias, lcBrFields,llFromSty
*E128404,1  TMI [Start] Define a variable "lcAuto" to make the automatic recieveing
PRIVATE lcAuto
lcAuto = 'A'
*E128404,1  TMI [End  ] 

*C101424,1 Now we can use selected style to validate C/T. [Begin]
IF MDOWN() OR !llBrowse AND EMPTY(lcTCode) AND EMPTY(PADR(lcStyle,lnMjrWid))
*IF !llBrowse AND EMPTY(lcTCode)
*C101424,1 Now we can use selected style to validate C/T. [End  ]

  RETURN
ENDIF

lnAlias   = SELECT(0)
llBrowse  = .F.

*C101424,1  llFromSty : Flag detect that you come from style validation or C/T Validation.
llFromSty = .F.

SELECT CutTktH

*C101424,1 Browse if not found on file or not empty of style [Begin]
*IF !SEEK(lcTCode,"CutTktH"))
IF !EMPTY(PADR(lcStyle,lnMjrWid)) OR !SEEK(lcTCode,"CutTktH"))

  xCuttkt=lcTCode
  *= CutBrow(lcTCode,'')
  IF EMPTY(PADR(lcStyle,lnMjrWid))
  
    = CutBrow(lcTCode,'')
  
  ELSE

    llFromSty = .T.
    = CutBrow(lcTCode,'',[STYLE + CutTkt = PADR(lcStyle,lnMjrWid)])

  ENDIF

*C101424,1 Browse if not found on file or not empty of style [End  ]

  lcTCode=xCuttkt

ENDIF

IF EMPTY(lcTCode) OR EOF('CUTTKTH')
  SELECT(lnAlias)
  _CUROBJ=OBJNUM(lcTCode)
  RETURN
ENDIF
SHOW GET lcTCode

*--C/t Validation.
llAbort=.F.
lcPoTtle = 'Cutting ticket'
DO WHILE .T.
  DO CASE
    CASE CUTTKTH.Status = 'S'
      *--XXX status is XXX. Therefore,no receivings can be done.
      = gfModalGen('INM34055B42000','DIALOG',lcPoTtle+'|'+'Closed'+'|'+'receivings')
      llAbort=.T.
      EXIT 
    CASE CUTTKTH.Status = 'H'
      *--XXX status is Hold since a cost sheet has not been created yet. Therefore,no receivings can be done.
      = gfModalGen('INM34056B42000','DIALOG',lcPoTtle)
      llAbort=.T.
      EXIT 
    CASE CUTTKTH.Status = 'X'
      *--XXX has been canceled. Not allowed to receive.
      = gfModalGen('INM34057B42000','DIALOG',lcPoTtle+'|'+'receive')
      llAbort=.T.
      EXIT 
    CASE CUTTKTH.Status = 'C'
      *--XXX is completely received. Do you wish to continue ?,\<Yes;\<No
      IF gfModalGen('INM34058B42002','DIALOG',lcPoTtle) = 2
        llAbort=.T.
        EXIT 
      ENDIF
  ENDCASE
  EXIT
ENDDO

*B605405,1 KHM 01/23/2002 (Begin) Checking if there was any item that hasn't
*B605405,1                been issued, then display a confirmation message.
IF !llOpnKtBom
  llOpnKtBom = lfOpn_Rest(gcDatadir,'CTKTBOM','CTKTBOM')
ENDIF  

SELECT cTktBom
IF SEEK('M'+lcTCode)
  LOCATE REST WHILE cImTyp+CutTkt='M'+lcTCode FOR ;
                        cCatgTyp $ 'FS' .OR. (cCatgTyp = 'T' .AND. Trim_Invt)
  IF !llOpnBmCst
     llOpnBmCst = lfOpn_Rest(gcDatadir,'BOMCOST','BOMCOST')
  ENDIF  

  SELECT BOMCOST
  SET ORDER TO TAG Pobomcls
  IF !SEEK('M'+lcTCode,'BOMCOST') .AND. ;
     gfModalGen('QRM38240B42002','DIALOG') = 2
     *--No cost items have been applied against this P/o. Are you sure you want to receive ?
     llAbort=.T.
  ENDIF 
ENDIF
*B605405,1 KHM 01/23/2002 (End)

IF llAbort
  lcTCode=SPACE(6)
  SELECT(lnAlias)
  _CUROBJ=OBJNUM(lcTCode)
  RETURN
ENDIF

SELECT CUTTKTL
*C101424,1  Check if it's full style there no need to brow Style withen Cut Tickets [Begin]
PRIVATE llPartSty
llPartSty = .T.
lcCurrSty = lcStyle        && Current receive style

*-- Walid
IF llFromSty AND Style.cDye_Flg = 'N'
  
  SET ORDER TO TAG CUTTKTLS
  llPartSty = !SEEK(lcStyle + lcTCode + '1')

ENDIF  

SET ORDER TO TAG CUTTKTL
*-- if not full style or not found in cut ticket lines file.
IF llPartSty
*C101424,1 Check if it's full style there no need to brow Style withen Cut Tickets [End  ]

  IF SEEK(lcTCode)
    DO lpGetOpen
    SET RELA TO Style INTO Style
    SET RELA TO cuttkt+style+dyelot+trancd INTO (lcTempFile) addi
    *B801970,1 Print total cost instead of price [begin
    *lcBrFields = "Style     :R :H=lcStyHdr :25,"+;
    *             "Style.Desc1 :R :H='Desc':45,"+;
    *             IIF(llDyelot,"Dyelot :R :H='Dyelot':15,","")+;
    *             "TotQty      :R :H='Quantity':P='9999999',"+;
    *             "nCost1 :R :H='Price':P='9999999.999'"
    *B802566,1 Don't display -ve open qty. 
    *lcBrFields = "Style     :R :H=lcStyHdr :25,"+;
                 "Style.Desc1 :R :H='Desc':45,"+;
                 IIF(llDyelot,"Dyelot :R :H='Dyelot':15,","")+;
                 "TotQty      :R :H='Quantity':P='9999999',"+;
                 "&lcTempFile..TOTQTY :R :H='Open' :10,"+;                 
                 "nTotCost=nCost1+nCost2+nCost3+nCost4+nCost5 :R :H='Total Cost':P='9999999.999'"
    lcBrFields = "Style     :R :H=lcStyHdr :25,"+;
                 "Style.Desc1 :R :H='Desc':45,"+;
                 IIF(llDyelot,"Dyelot :R :H='Dyelot':15,","")+;
                 "TotQty      :R :H='Quantity':P='9999999',"+;
                 "lnBOpnQty=IIF(&lcTempFile..TOTQTY<0,0,&lcTempFile..TOTQTY) :R :H='Open' :10,"+;
                 "nTotCost =nCost1+nCost2+nCost3+nCost4+nCost5 :R :H='Total Cost':P='9999999.999'"
    *B802566,1 End.
    *B801970,1 Print total cost instead of price [end..
    *E128404,1  TMI [Start] Show a message asking if the user needs automatic or manual recieving
    SELECT (lcTmpLine)
    LOCATE FOR CUTTKT = lcTCode
    IF !FOUND()
      lcAuto = IIF(gfModalGen('QRM34126B34008','DIALOG',lcPoTtle) = 1,'M','A')
    ELSE
      lcAuto = IIF(LAUTOMODE,'A','M')
      IF LAUTOMODE
        *--This XXXX already selected.
        = gfModalGen('INM34128B42000','DIALOG',lcPoTtle)
        lcTCode=SPACE(6)
        SELECT(lnAlias)
        _CUROBJ=OBJNUM(lcTCode)
        RETURN
      ENDIF
    ENDIF
    IF lcAuto ='M'
      *E128404,1  TMI [End  ] 
      
      DIMENSION laTemp[1]
      laTemp = ''
      =lfActBrow()
      SELECT CUTTKTL
      =ARIABROW([lcTCode FOR Trancd='1'],'Select cutting ticket Line',gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','Style','laTemp')
      lcStyle=laTemp[1]

      *E128404,1  TMI [Start] Get the lcStyle Variable for the first line in the automatic mode
    ELSE
      SELECT CUTTKTL 
      =SEEK(lcTCode)
      LOCATE REST WHILE CUTTKT+STYLE+DYELOT+TRANCD = lcTCode ;
                  FOR TRANCD='1'
      lcStyle = CUTTKTL.STYLE    
    ENDIF
    *E128404,1  TMI [End  ] 
      
  
  ELSE

    *-The lines for this 'C/T' are missing ! cannot proceed.
    =gfModalGen('TRM34017B42000','DIALOG','C/T')
    lcTCode=SPACE(6)
    SELECT(lnAlias)
    _CUROBJ=OBJNUM(lcTCode)
    RETURN

  ENDIF

ENDIF  && end if not full style or not found in cut ticket lines file BY C101424,1.
lcQltyKey = IIF(SEEK(CUTTKTL.Style,'STYLE'),STYLE.cStyGrade,'1')
SET RELA TO

IF EMPTY(PADR(lcStyle,lnMjrWid))

  *C101424,1 Adjust current object [Begin]
  *_CUROBJ=OBJNUM(lcTCode)
  IF llFromSty
    lcStyle = lcCurrSty
    lcTCode = ' '
    _CUROBJ=OBJNUM(lcStyle)
  ELSE
    _CUROBJ=OBJNUM(lcTCode)
  ENDIF  
  SHOW GET lcTCode
  SHOW GET lcStyle
  *C101424,1 Adjust current object [End  ]

  SELECT(lnAlias)
  RETURN

ENDIF

*E300935,4 Validate dyelot and location is in dyelot field and location field.
IF ((llCMInstld AND llPOSale) OR !llWareHous) AND (!llDyelot OR STYLE.cDye_Flg<>'Y') AND ;
  SEEK('1'+SPACE(3)+lcTCode+lcStyle,lcTmpLine)
  *-This XXXX has been entered on this XXXX.
  =gfModalGen('TRM42107B42000','DIALOG','Style'+'|'+'receiving cutting ticket')
  _CUROBJ=OBJNUM(lcTCode)
  SELECT(lnAlias)
  RETURN
ENDIF

*--Get a lot no and last operation.
llSpecLot  = .F.
lcClrLstOp = SPACE(6)
*B603385,1 Start.
*lcLotNo = IIF(!EMPTY(CutTktH.clastopr),lfSelLots(STR(CUTTKTL.LineNo,6)),SPACE(2))
*B603275,1 AMM Get the dyelot before calling select lot browse
lcDyelot = CUTTKTL.Dyelot  
*B603275,1 AMM end
lcLotNo = lfSelLots(STR(CUTTKTL.LineNo,6),CutTktH.clastopr)
*B603385,1 End.
*B603275,1 AMM Move this piece of code above
*lcDyelot = CUTTKTL.Dyelot  
*B603275,1 AMM end
SHOW GET ibTCode  DISABLE
SHOW GET lcTCode  DISABLE
SHOW GET ibStyle  DISABLE
SHOW GET lcStyle  DISABLE

*E128404,1  TMI [Start] if in manual mode add only one line
IF lcAuto = 'M'
  *E128404,1  TMI [End  ] 

  *E300935,4 Validate dyelot and location is in dyelot field and location field.
  IF ((!(llCMInstld AND llPOSale) AND llWareHous) OR llDyelot) AND STYLE.cDye_Flg = 'Y'
    SHOW GET lcDyelot ENABLE
    _CUROBJ=OBJNUM(lcDyelot)     
  ELSE
    SHOW GET lcDyelot DISABLE
    *=lfGetInfo()
    =lfChkLine()  && Check existance of this location.
  ENDIF

  *E128404,1  TMI [Start] start automatic mode
ELSE
  SELECT CUTTKTL 
  =SEEK(lcTCode)
  SCAN REST WHILE CUTTKT+STYLE+DYELOT+TRANCD = lcTCode ;
              FOR TRANCD='1'
    lcStyle  = CUTTKTL.STYLE
    lcDyelot = CUTTKTL.DYELOT
    =lfChkLine()  && Check existance of this location.
    SELECT &lcTmpLine
    REPLACE TOTSTK    WITH TOTQTY ;
            TOTBAL    WITH 0      ;
            LAUTOMODE WITH .T.
    SCATTER MEMVAR 
    M.TRANCD = '2'
    INSERT INTO &lcTmpLine FROM MEMVAR
    lnTotStk = lnTotStk + m.TotStk
  ENDSCAN
  SELECT &lcTmpLine
  GO TOP
  =lfActBrow()
ENDIF
*E128404,1  TMI [End  ] 

SELECT(lnAlias)
RETURN


*!*************************************************************
*! Name    : lfvDyelot
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Validate dyelot.
*!*************************************************************
FUNCTION lfvDyelot

IF EMPTY(lcDyelot)
  *--The dyelot cannot be empty.
  =gfModalGen('TRM38119B42000','DIALOG')
  _CUROBJ=OBJNUM(lcDyelot)     

ELSE

  *E300935,4 Validate dyelot and location is in dyelot field and location field.
  *E301291,4 AMM Consider case of issue adornment order
  *IF ((llCMInstld AND llPOSale) OR !(llWareHous AND !(lcPType $ 'N')))
  IF ((llCMInstld AND llPOSale) OR !(llWareHous AND !(lcPType $ 'NA')))
  *E301291,4 AMM end
    *E300935,4 Adjust seek condition to be suitable for P/O case. [begin]
    lcSeekDye = '1'+SPACE(3)+lcTCode+lcStyle+lcDyelot
    *E300935,4 Adjust seek condition to be suitable for P/O case.
    *IF SEEK('1'+SPACE(3)+lcTCode+lcStyle+lcDyelot,lcTmpLine)
    IF SEEK(lcSeekDye,lcTmpLine)
      SELECT (lcTmpLine)
      LOCATE REST WHILE TranCd+cCarton+IIF(llMfCall,CutTkt,Po)+Style+Dyelot+cWareCode+STR(LineNo,6) = ;
                       '1'+SPACE(3)+lcTCode+lcStyle FOR IIF(llMfCall,.T.,LineNo = PosLn.LineNo) 
      IF FOUND()
        *E300935,4 lcMessage is instead of direct calling.
        *E301291,4 AMM add case of issue adornment order type 'A'
        *lcMessage = IIF(llMfCall,'receiving cutting ticket',;
                    IIF(lcPType $ 'N','issue inter location ','receiving ') + 'purchase order')
        
        *B603625,4 AMH Fix the bug of message style has been entered in this issue P/O in
        *B603625,4     case of issue adornment order [Start]
        *lcMessage = IIF(llMfCall,'receiving cutting ticket',;
                    IIF(lcPType $ 'NA','issue ','receiving ') +IIF(lcPType='N','inter location ',IIF(lcPType='A','adornment',IIF(lcPType='D','dye',''))) + 'purchase order')
        lcMessage = IIF(llMfCall,'receiving cutting ticket',;
                    IIF(lcPType $ 'NA','issue ','receiving ') +IIF(lcPType='N','inter location ',IIF(lcPType='A','adornment',IIF(lcPType='D','dye',''))) + ' purchase order')
        *B603625,4 AMH [End]
        
        *E301291,4 AMM end
        *-This XXXX has been entered on this XXXX.
        =gfModalGen('TRM42107B42000','DIALOG','Style/Dyelot'+'|'+lcMessage)
        lcDyelot = lcOldValue
        _CUROBJ=OBJNUM(lcDyelot)     
        RETURN
      ENDIF  
    
    ENDIF

  ENDIF  
  *=lfGetInfo()
  =lfChkLine()  && Check existance of this location.

  SHOW GET lcDyelot DISABLE
ENDIF
RETURN

*!*************************************************************
*! Name      : lfSelLots
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose   : Function to select lot number to receive to.
*!*************************************************************
*! Parameters: Style line no.
*!*************************************************************
*! Returns   : Selected lot number 
*!*************************************************************
FUNCTION lfSelLots
*B603385,1 Start, added a parameter for last operation.
*PARA lcTktLn
PARA lcTktLn,lcHdrLOpr
*B603385,1 End.

lnAlias = SELECT()
SELECT MfgOprDt
lcMFOldTg = ORDER('MFGOPRDT')
SET ORDER TO TAG TktOpTrn

*IF ! laOpFile[1]
  = gfOpenFile(gcDataDir+"CODES","CODES","SH")
*ENDIF

*B803337,1 WMA [Begin]
*E301077,11 MAB Open BOMLINE file here [Begin
*IF !llOpnBomLn
*  llOpnBomLn = lfOpn_Rest(gcDatadir,'BOMLINE','BOMLINE')
*ENDIF  
*E301077,11 MAB Open BOMLINE file here [End..
*B803337,1 WMA [End]

SELECT BOMLINE
lcBmOldTg = ORDER('BOMLINE')
SET ORDER TO TAG Mfgopr


*B603385,1 Start, Added condition to Read Last operation if sent empty.
IF EMPTY(lcHdrLOpr)
*B603385,1 End.

  *--Get the Last operation for this Style/Color LINE NO.
  STORE ' ' TO lcLastMfgOr
  lnMaxSeq = 0

  *B602309,1 get the last operation from bomline file insted of mfgoprdt.
  SELECT BOMLINE
  *C200080,1 AMM Add the dye order type 'D'
  *SEEK lcPType+lcTCode
  *SCAN WHILE cIMTyp+cTktNo = lcPType+lcTCode ;
          FOR STR(LineNo,6) = lcTktLn AND cCatgTyp='M' AND !EMPTY(MfgCode)
  *khm1
  *SEEK IIF(lcPType='D','I',lcPType)+lcTCode
  *SCAN WHILE cIMTyp+cTktNo = IIF(lcPType='D','I',lcPType)+lcTCode ;
         FOR STR(LineNo,6) = lcTktLn AND cCatgTyp='M' AND !EMPTY(MfgCode)
  SEEK lcPType+lcTCode
  SCAN WHILE cIMTyp+cTktNo = lcPType+lcTCode ;
         FOR STR(LineNo,6) = lcTktLn AND cCatgTyp='M' AND !EMPTY(MfgCode)
  *khm1
  *C200080,1 AMM end

    *--Check if this MFG operation code works as operation.
    llWrkAsOpr = .F.
    lnCurSeqn  = 0
    *B602309,1
    *IF SEEK(gcAct_Comp+BOMLINE.MfgCode+'Y'+'MFGCODE   ','CODES')
    IF SEEK('N'+BOMLINE.MfgCode+'Y'+'MFGCODE   ','CODES')
      SELECT CODES
      *LOCATE REST WHILE ccomp_id+ccode_no+crltfield+cfld_name = ;
      gcAct_Comp+BOMLINE.MfgCode+'Y'+'MFGCODE   ' FOR CRLTD_NAM = 'LMFGOPR'
      LOCATE REST WHILE cDefCode+ccode_no+crltfield+cfld_name = ;
         'N'+BOMLINE.MfgCode+'Y'+'MFGCODE   ' FOR CRLTD_NAM = 'LMFGOPR'

      IF FOUND()
        llWrkAsOpr = ALLT(CODES.cRltd_vlu) = "T"

        *B603385,1 Start, Check the operation sequence.
        =SEEK('N'+BOMLINE.MfgCode+'Y'+'MFGCODE   ','CODES')
        LOCATE REST WHILE cDefCode+ccode_no+crltfield+cfld_name = ;
           'N'+BOMLINE.MfgCode+'Y'+'MFGCODE   ' FOR CRLTD_NAM = 'COPERSEQ'  
        lnCurSeqn = IIF(FOUND(),INT(VAL(ALLT(CODES.cRltd_vlu))),0)
        *B603385,1 End.  
      ENDIF
    ENDIF

    SELECT BOMLINE
    IF !llWrkAsOpr
      LOOP
    ENDIF

    *B603385,1 Start, Read last operation for sequence is last.
    IF lnMaxSeq <= lnCurSeqn
    *B603385,1 End.
      *--Read last operation.
      lcLastMfgOr = BOMLINE.MfgCode
      lnMaxSeq = lnCurSeqn
    ENDIF
    
  ENDSCAN

ELSE
  lcLastMfgOr = lcHdrLOpr
ENDIF


*--If not empty last operation , read receive lot.
lcRecvLot = '  ' 
IF !EMPTY(lcLastMfgOr)
  SELECT MFGOPRDT
  *C200080,1 AMM Add the dye order type 'D'
  *=SEEK(lcPType+lcTCode+lcLastMfgOr+'1')
  *khm1
  *=SEEK(IIF(lcPType='D','I',lcPType)+lcTCode+lcLastMfgOr+'1')
  =SEEK(lcPType+lcTCode+lcLastMfgOr+'1')
  *khm1
  
  *C200080,1 AMM end
  lnNumLots = 0
  *C200080,1 AMM Add the dye order type 'D'
  *COUNT REST TO lnNumLots ;
          WHILE cIMTyp+cTktNo+cOprCode+TranCd = ;
                lcPType+lcTCode+lcLastMfgOr+'1' FOR Item = lcStyle
  *B603275,1 AMM Count lots that has quantity other than zero
  *COUNT REST TO lnNumLots ;
          WHILE cIMTyp+cTktNo+cOprCode+TranCd = ;
                IIF(lcPType='D','I',lcPType)+lcTCode+lcLastMfgOr+'1' FOR Item = lcStyle
  *khm1
  *COUNT REST TO lnNumLots ;
          WHILE cIMTyp+cTktNo+cOprCode+TranCd = ;
                IIF(lcPType='D','I',lcPType)+lcTCode+lcLastMfgOr+'1' FOR Item = lcStyle .AND. nLotTotQty >0
  COUNT REST TO lnNumLots ;
          WHILE cIMTyp+cTktNo+cOprCode+TranCd = ;
                lcPType+lcTCode+lcLastMfgOr+'1' FOR Item = lcStyle .AND. nLotTotQty >0
  *khm1
  *B603275,1 AMM end
  *C200080,1 AMM end
  IF lnNumLots <> 0
    *C200080,1 AMM Add the dye order type 'D'
    *=SEEK(lcPType+lcTCode+lcLastMfgOr+'1')
    *LOCATE REST WHILE cIMTyp+cTktNo+cOprCode+TranCd = ;
                 lcPType+lcTCode+lcLastMfgOr+'1' FOR Item = lcStyle
    *khm1
    *=SEEK(IIF(lcPType='D','I',lcPType)+lcTCode+lcLastMfgOr+'1')
    =SEEK(lcPType+lcTCode+lcLastMfgOr+'1')
    *khm1
    
    *B603275,1 AMM Add dyelot
    *LOCATE REST WHILE cIMTyp+cTktNo+cOprCode+TranCd = ;
                 IIF(lcPType='D','I',lcPType)+lcTCode+lcLastMfgOr+'1' FOR Item = lcStyle
    *khm1
    *LOCATE REST WHILE cIMTyp+cTktNo+cOprCode+TranCd = ;
                 IIF(lcPType='D','I',lcPType)+lcTCode+lcLastMfgOr+'1' FOR Item = lcStyle .AND. cDyelot = lcDyelot
    LOCATE REST WHILE cIMTyp+cTktNo+cOprCode+TranCd = ;
                 lcPType+lcTCode+lcLastMfgOr+'1' FOR Item = lcStyle .AND. cDyelot = lcDyelot
    *khm1
    *B603275,1 AMM end
    *C200080,1 AMM end
    DO CASE
      CASE lnNumLots = 1
        lcRecvLot = cLotNo
      CASE lnNumLots > 1
        *--Do you want to select a lot to receive to or distribute received quantity to all lots?,\<Select;\<Distribute
        IF gfModalGen('QRM42108B42011','DIALOG') = 1
          lcBrFields = [cLotNo    :H='Lot No.',]+;
                       [cOprCode  :H='Operation',]+;
                       [cContCode :H='Cont./Dept.',]+;
                       [cContName :H='Name',]+;             
                       [dTranDate :H='Trans.Date',]+;
                       [dueDate   :H='Due Date',]+;
                       [nLotTotQty :H='Total']
          DIMENSION laTemp[1]
          laTemp = ''
          *C200080,1 AMM Add the dye order type 'D'
          *=ARIABROW([lcPType+lcTCode+lcLastMfgOr+'1' FOR Item=lcStyle],'Lots',gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','cLotNo','laTemp')
          *B603275,1 AMM Add dyelot
          *=ARIABROW([IIF(lcPType='D','I',lcPType)+lcTCode+lcLastMfgOr+'1' FOR Item=lcStyle],'Lots',gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','cLotNo','laTemp')

          *khm1
          *=ARIABROW([IIF(lcPType='D','I',lcPType)+lcTCode+lcLastMfgOr+'1' FOR Item=lcStyle;
                  .AND. cDyelot = lcDyelot],'Lots',gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','cLotNo','laTemp')
          =ARIABROW([lcPType+lcTCode+lcLastMfgOr+'1' FOR Item=lcStyle;
                  .AND. cDyelot = lcDyelot],'Lots',gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','cLotNo','laTemp')
          *khm1
          *B603275,1 AMM end

          *C200080,1 AMM end
          lcRecvLot=laTemp[1]
          IF !EMPTY(lcRecvLot)
            llSpecLot = .T.
            laLotArry = 0
            SELECT MfgOprDt
            *C200080,1 AMM Add the dye order type 'D'
            *SEEK lcPType+lcTCode+lcLastMfgOr+'1'       
            *SCAN WHILE cIMTyp+cTktNo+cOprCode = lcPType+lcTCode+lcLastMfgOr;
                  FOR Item = lcStyle AND cLotNo = lcRecvLot
            *khm1
            *SEEK IIF(lcPType='D','I',lcPType)+lcTCode+lcLastMfgOr+'1'       
            *SCAN WHILE cIMTyp+cTktNo+cOprCode = IIF(lcPType='D','I',lcPType)+lcTCode+lcLastMfgOr;
                  FOR Item = lcStyle AND cLotNo = lcRecvLot
            SEEK lcPType+lcTCode+lcLastMfgOr+'1'       
            SCAN WHILE cIMTyp+cTktNo+cOprCode = lcPType+lcTCode+lcLastMfgOr;
                  FOR Item = lcStyle AND cLotNo = lcRecvLot

            *khm1
            *C200080,1 AMM end

              FOR I=1 TO 8
                Z=STR(I,1)
                laLotArry[I]=laLotArry[I]+(NLOTQTY&Z * IIF(TranCd='1',1,-1))
              ENDFOR
            ENDSCAN
          ENDIF 
        ENDIF                
    ENDCASE      
  ELSE
    lcRecvLot = '01'
  ENDIF
ENDIF

*--Set Order Tags Back.
SELECT MfgOprDt
SET ORDER TO TAG &lcMFOldTg
SELECT BOMLINE
SET ORDER TO TAG &lcBmOldTg
SELECT (lnAlias)

*--Read last operation for color.
lcClrLstOp = lcLastMfgOr
lcRecvLot  = IIF(EMPTY(lcRecvLot),SPACE(2),lcRecvLot)
RETURN(lcRecvLot)



*!*************************************************************
*! Name    : lfGetInfo
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Get information.
*!*************************************************************
FUNCTION lfGetInfo
PARAMETERS llEmpWare
lnAlias =SELECT()

DO CASE

  ******************************
  *--B Y   S H I P M E N T . --*
  ******************************
  CASE lcPType='S' AND !llShpPO
  
    *C127341,14 TMI [Start] Open the new file SHPRLFLD
    IF FILE(gcDataDir+'SHPRLFLD.DBF')
      =gfOpenFile(gcDataDir+'SHPRLFLD','SHPRLFLD','SH')
    ENDIF
    *C127341,14 TMI [End  ] 
    
    *-Header info.
    ldEnterd  =SHPMTHDR.Entered
    lnCartons =SHPMTHDR.Cartons
    ldEta     =SHPMTHDR.Eta
    lcAirWayB =SHPMTHDR.AirWayB
    lcShpRef  =SHPMTHDR.Reference
    DIME laZero[9]
    laZero = 0

    *-Line info.
    lcTPO=SPACE(6)
    
    SELECT POSLN
    SCAN WHILE ShipNo = lcShpCode FOR TranCd = '3'
      IF !SEEK('P'+POSLN.Po,'POSHDR')
        LOOP
      ENDIF  
      WAIT WINDOW 'Shipment style : '+POSLN.Style NOWAIT
      *B803030,1 TAK 02/08/2000 (Begin) Commenting the IF llImpCost and move
      *B803030,1                it after the IF !INLIST(POSHDR.Status,'O','A') 
      *B803030,1                condition in order to check on the PO status 
      *B803030,1                in all cases not only the case of llImpCost.     
      *IF llImpCost
      *B803030,1 TAK 02/08/2000 (End)
     
        *B603440,1 (Begin) Recieve actualized POs also.
        *IF POSHDR.Status <> 'O'
        IF !INLIST(POSHDR.Status,'O','A')
        *B603440,1 (End)
          IF PO=lcTPO
            LOOP
          ENDIF
          IF POSHDR.Status = 'H'
            *--The P/O &lcTPO status is Hold since a P/O cost sheet has not 
            *--been created yet. Therefore,no receivings can be done for 
            *--this P/O. It will be skipped for the shipment.
            =gfModalGen('TRM34073B42000','DIALOG',POSHDR.po)        
          ENDIF 
          IF POSHDR.Status = 'S'
            *--The P/O &lcTPO status is Closed, the P/O will be skipped for the shipment.
            =gfModalGen('TRM34074B42000','DIALOG',POSHDR.po)        
          ENDIF
          *B803030,1 TAK 02/08/2000 (Begin) Assigning lcTPO with the current
          *B803030,1                PosLn in order to completely ignore this PO.
          lcTPO = PosLn.PO
          *B803030,1 TAK 02/08/2000 (End)
          LOOP
        ENDIF 

      *B803030,1 TAK 02/08/2000 (Begin) Putting the IF llImpCost after 
      *B803030,1                checking all the case of the PO's status. 
      IF llImpCost
      *B803030,1 TAK 02/08/2000 (End) 
        IF  PO <> lcTpo
          llMatExist = .F.

          *E301077,11 MAB Open Cut Ticket BOM file here [Begin
          IF !llOpnKtBom
            llOpnKtBom = lfOpn_Rest(gcDatadir,'CTKTBOM','CTKTBOM')
          ENDIF  
          *E301077,11 MAB Open Cut Ticket BOM file here [End..

          IF SEEK('I'+PO,'cTktBom')
            SELECT cTktBom
            SCAN REST WHILE cImTyp+CutTkt='I'+PosLn.PO FOR ;
                    cCatgTyp $ 'FS' .OR. (cCatgTyp = 'T' .AND. Trim_Invt)
              llMatExist = .T.
            ENDSCAN
          ENDIF
          SELECT POSLN
          IF llMatExist

            *E301077,11 MAB Open BOM Cost file here [Begin
            IF !llOpnBmCst
              llOpnBmCst = lfOpn_Rest(gcDatadir,'BOMCOST','BOMCOST')
            ENDIF  
            *E301077,11 MAB Open BOM Cost file here [End..

            SELECT BOMCOST
            SET ORDER TO TAG Pobomcls
            *--No cost items have been applied against this P/o. Are you sure you want to receive ?
            
            
            *E300935,4 Add alias POSLN before PO field in seek command.
            IF !SEEK('I'+POSLN.PO) AND gfModalGen('QRM34071B42002','DIALOG') = 2
              LOOP
            ENDIF 
          ENDIF
          
          *E300935,4 Add alias POSLN before PO field in next command.
          lcTPO = POSLN.PO
        ENDIF 

      ENDIF  && End if detail importing.

      IF llMulCurr
        lcCur1  = POSHDR.cPriceCur
        lnCrRt1 = IIF(lcCur1=gcBaseCurr,1,gfChkRate('lnCurrUnt1',lcCur1,ldRcvDate,llEditExRt,gcAct_comp,.F.)) 
        lcCur2  = POSHDR.cDutyCur
        lnCrRt2 = IIF(lcCur2=gcBaseCurr,1,gfChkRate('lnCurrUnt2',lcCur2,ldRcvDate,llEditExRt,gcAct_comp,.F.)) 
      ENDIF
      IF lnCrRt1=0 OR lnCrRt2=0
        IF !llEditExRt
          *--This line has currency with zero rate, it will be ignored.' 
          = gfModalGen('TRM34079B42000','DIALOG')
          LOOP
        ELSE
          STORE 1 TO lnCrRt1,lnCrRt2
        ENDIF
      ENDIF
      
      *E300935,4 Select alias POSLN before Scatter and gather commands -> begin
      SELECT POSLN
      *E300935,4 Select alias POSLN before Scatter and gather commands -> end
      
      SCATTER MEMVAR
      SCATTER FIELDS nCost1,nCost2,nCost3,nCost4,nCost5 TO laEstiCost
         
      SELECT (lcTmpLine)
      APPEND BLANK
      GATHER MEMVAR
      REPLACE TranCd    WITH '1',;
              ShipNo    WITH lcShpCode,;
              nLanPrRat WITH lnCrRt1,;
              nLanDuRat WITH lnCrRt2,;
              TotStk    WITH TotQty,;
              TotDam    WITH 0,;
              TotCan    WITH 0,;
              TotBal    WITH 0 

      GATHER FROM laEstiCost FIELDS nLan_CST1,nLan_CST2,nLan_CST3,nLan_CST4,nLan_CST5 

      *C127341,14 TMI [Start] use a new file that saved the shipment line rates
      *-* *C127341,7  TMI [Start] Update price rate from the field posln.npricerat for case of reciving by shipmnets
      *-* IF TYPE('POSLN.NPRICERAT')='N' .AND. POSLN.NPRICERAT>0
      *-*   REPLACE nLanPrRat WITH POSLN.NPRICERAT
      *-* ENDIF
      *-* *C127341,7  TMI [End  ] 
      IF FILE(gcDataDir+'SHPRLFLD.DBF') .AND. gfGetMemVar('M_APRVSHIP')
        IF SEEK(POSLN.SHIPNO+POSLN.PO+STR(POSLN.LINENO,6),'SHPRLFLD') 
          REPLACE nLanPrRat WITH SHPRLFLD.NPRICERAT
          *C127341,22 TMI [Start] Update the Duty price rate
          REPLACE NLANDURAT WITH SHPRLFLD.NDUTYRAT
          *C127341,22 TMI [End  ] 

          *C127341,23 TMI [Start] At GPS the duty rate is ALWAYS by the base currency.
          IF ASCAN(laEvntTrig,PADR("SETDTYRT",10)) <> 0
            =gfDoTriger('POSTREC','SETDTYRT')
          ENDIF
          *C127341,23 TMI [End  ] 
          
        ENDIF
      ENDIF
      *C127341,14 TMI [End  ] 

      IF llMulCurr
        =lfGetEqv('12345',nLanPrRat,nLanDuRat,lnCurrUnt1,lnCurrUnt2,;
                  nLan_Cst1,nLan_Cst2,nLan_Cst3,nLan_Cst4,nLan_Cst5)
      ELSE
        =ACOPY(laEstiCost,laECost)
      ENDIF
      GATHER FROM laECost FIELDS nELanCost1,nELanCost2,nELanCost3,nELanCost4,nELanCost5

      lnTotStk = lnTotStk + TotStk
      SCATTER MEMVAR
      APPEND BLANK
      GATHER MEMVAR
      REPLACE TranCd WITH '2'
      GATHER FROM laZero FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
    ENDSCAN
    WAIT CLEAR
    SELECT (lcTmpLine)
    GO TOP


  *****************************
  *-- F R O M    B A T C H. --*
  *****************************
  *E301480,1 NAD (Start) Add the Inter location Po batch to the condition. 
  *CASE lcPType $ 'BT'

  *C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
  *CASE lcPType $ 'BTL' 
  CASE lcPType $ 'BTLH'
  *C200170,1 AMH Add [End]

  *E301480,1 NAD (End)
    *--Header info.  
    llByCarton = CTKTRCVH.Carton
    lcBDesc    = CTKTRCVH.cDesc

    *C200170,1 AMH Batch status is issued in case of recieve inter-location P/O Batch [Start]
    *lcBStatus  = 'APPROVED'
    lcBStatus  = IIF(lcPType='L','ISSUED','APPROVED')
    *C200170,1 AMH [End]

    ldBDate    = CTKTRCVH.dDate

    *B124614,1 NNA 10/24/2004 (Begin) get lnTotStk , lnTotDam and lnTotCan from the CTKTRCVL.dbf 
    *B124614,1 NNA            instead of CTKTRCVH.dbf because they return with zero from CTKTRCVH.dbf
    *lnTotStk   = CTKTRCVH.nTotStk
    *lnTotDam   = CTKTRCVH.nTotDam
    *lnTotCan   = CTKTRCVH.nTotCan
    STORE 0 TO lnTotStk , lnTotDam , lnTotCan
    SELECT CTKTRCVL
    IF SEEK(IIF(llMfCall,'M',IIF(lcPType='B','I','N'))+lcBatch)
      SCAN REST WHILE cType+TmpRcvNum = IIF(lcPType='B','I','N')+lcBatch   
        lnTotStk   = lnTotStk + IIF(TRANCD='2',CTKTRCVL.TotQty,0)
        lnTotDam   = lnTotDam + IIF(TRANCD='4',CTKTRCVL.TotQty,0)
        lnTotCan   = lnTotCan + IIF(TRANCD='5',CTKTRCVL.TotQty,0)
      ENDSCAN
    ENDIF
    *B124614,1 NNA (End)

    *--Line info.
    lcKey=' '

    SELECT CTKTRCVL
    
    *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
    *= SEEK(IIF(llMfCall,'M','I')+lcBatch)    
    *IF lcPType = 'B'
    = SEEK(IIF(llMfCall,'M',IIF(lcPType='B','I','N'))+lcBatch)
  
    *C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
    *IF lcPType $ 'BL'
    IF lcPType $ 'BLH'
    *C200170,1 AMH Add case of issue inter-location P/O Batch [Start]

    PRIVATE lcTypScan,lcTypSeek
    lcTypScan=IIF(lcPType='B','I','N')
    lcTypSeek=IIF(lcPType='B','P','N')
    *E301480,1 NAD (End)
    
      *E300935,4 Change Scan While to scan rest while because it's faster.
      *SCAN WHILE cType+TmpRcvNum = 'I'+lcBatch
          
      *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
      *SCAN REST WHILE cType+TmpRcvNum = 'I'+lcBatch    
      *=SEEK('P'+Cuttkt,'POSHDR')

      *C200170,1 AMH intialize lnTotStk in case of issue inter-location P/O Batch [Start]
      IF lcPType='H'
        lnTotStk   = 0
      ENDIF
      *C200170,1 AMH [End]

      SCAN REST WHILE cType+TmpRcvNum = lcTypScan+lcBatch   
        =SEEK(lcTypSeek+Cuttkt,'POSHDR')      
      *E301480,1 NAD (End)
      
        IF llMulCurr
          lcCur1  = POSHDR.cPriceCur
          lnCrRt1 = IIF(lcCur1=gcBaseCurr,1,gfChkRate('lnCurrUnt1',lcCur1,ldRcvDate,llEditExRt,gcAct_comp,.F.)) 
          lcCur2  = POSHDR.cDutyCur
          lnCrRt2 = IIF(lcCur2=gcBaseCurr,1,gfChkRate('lnCurrUnt2',lcCur2,ldRcvDate,llEditExRt,gcAct_comp,.F.)) 
        ENDIF
        *--Case of batch do not accept saving the batch if there is 0 rates.
        IF lnCrRt1=0 OR lnCrRt2=0
          IF !llEditExRt
            *--One or more lines has currency with zero rate, Unable to proceed.' 
            = gfModalGen('TRM34080B42000','DIALOG')
            STORE ' ' TO lcPo,lcStyle,lcCur1,lcCur2
            SHOW GETS
            RETURN
          ELSE
            STORE 1 TO lnCrRt1,lnCrRt2
          ENDIF
        ENDIF
      
        SELECT POSLN
        
        *E301480,1 NAD (Start) Add the Inter location Po batch to the Expression.
        *= SEEK('P'+CTKTRCVL.Cuttkt+CTKTRCVL.Style)
        = SEEK(lcTypSeek+CTKTRCVL.Cuttkt+CTKTRCVL.Style)
        *E301480,1 NAD (End)
        
        SCATTER FIELDS nCost1,nCost2,nCost3,nCost4,nCost5 TO laEstiCost
        SCATTER FIELDS nECost1,nECost2,nECost3,nECost4,nECost5 TO laEstECost
        SCATTER FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd TO laAlo
        =lfGetEqv('12345',lnCrRt1,lnCrRt2,lnCurrUnt1,lnCurrUnt2,;
                  laEstiCost[1],laEstiCost[2],laEstiCost[3],laEstiCost[4],laEstiCost[5])

        SELECT CTKTRCVL
        =SEEK(Style,'STYLE')

        *E300935,4 Add dyelot field in P/O by batch case.  [begin]
        *SCATTER FIELDS cCarton,Lineno,Style,cWareCode,Reference,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,cStyGrade,nLineNo TO laFields
        *B603100,1 SSH 22/05/2000 Fix the bug of incoret recieved cost in case of 
        *B603100,1 SSH 22/05/2000 recieve by batch.
        *SCATTER FIELDS cCarton,Lineno,Style,Dyelot,cWareCode,Reference,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,cStyGrade,nLineNo TO laFields
        SCATTER FIELDS cCarton,Lineno,Style,Dyelot,cWareCode,Reference,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,Style.cStyGrade,nLineNo TO laFields
        *B603100,1 SSH 22/05/2000 [End]
        *E300935,4 Add dyelot field in P/O by batch case.  [end]

        *E300935,4
        *E300935,4 Add nLineNo To expression Expression and use expression arrays.
        *IF cType+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+IIF(llByCarton,STR(LineNo,6),'') <> lcKey
        IF cuttkt+style+dyelot+ccarton+STR(nLineNo,6)+STR(lineno,6) <> lcKey
          SELECT (lcTmpLine)
          APPEND BLANK
          
          *E301480,1 NAD (Start) cosider the case of  Inter location Po batch 
          *REPLACE cStyType  WITH 'P',;
                  PO        WITH CTKTRCVL.Cuttkt,;
                  Vendor    WITH POSHDR.Vendor,;
                  Scale     WITH STYLE.Scale,; 
                  nLanPrRat WITH lnCrRt1,;
                  nLanDuRat WITH lnCrRt2,;
                  TranCd    WITH '1'
          
           REPLACE cStyType  WITH lcTypSeek,;
                   PO        WITH CTKTRCVL.Cuttkt,;
                   Vendor    WITH POSHDR.Vendor,;
                   Scale     WITH STYLE.Scale,; 
                   nLanPrRat WITH lnCrRt1,;
                   nLanDuRat WITH lnCrRt2,;
                   TranCd    WITH '1'
 
          IF lcPType='L'          
            REPLACE  TotStk WITH CTKTRCVL.TotQty   
          ENDIF
          *E301480,1 NAD (End)

          *E300935,4 Add dyelot field in P/O by batch case.  [begin]
          *GATHER FROM laFields FIELDS cCarton,Lineno,Style,cWareCode,Reference,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,cStyGrade,nLineNo
          GATHER FROM laFields FIELDS cCarton,Lineno,Style,Dyelot,cWareCode,Reference,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,cStyGrade,nLineNo
          *E300935,4 Add dyelot field in P/O by batch case.  [end]

          *C200170,1 AMH Add Get TotStk & Tot Bal in case of issue inter-location P/O Batch [Start]
          IF lcPType='H'
            REPLACE  TotStk WITH CTKTRCVL.TotQty;
                     TotDam WITH 0;
                     TotCan WITH 0;
                     TotBal WITH TotQty-TotStk;
                     Lineno WITH nLineNo
            lnTotStk   = lnTotStk + TotStk
          ENDIF
          *C200170,1 AMH [End]
          
          *C128583,5  TMI [Start] Update TotStk, totBal for Banach PAN21
          IF lcPType='B'
            REPLACE  TotStk WITH CTKTRCVL.TotQty;
                     TotDam WITH 0;
                     TotCan WITH 0;
                     TotBal WITH TotQty-TotStk ;
                     Lineno WITH nLineNo
            *lnTotStk   = lnTotStk + TotStk  && by tmi
          ENDIF
          *C128583,5  TMI [End  ] 

          GATHER FROM laAlo      FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
          GATHER FROM laEstiCost FIELDS nCost1,nCost2,nCost3,nCost4,nCost5          
          GATHER FROM laEstiCost FIELDS nLan_CST1,nLan_CST2,nLan_CST3,nLan_CST4,nLan_CST5 
          GATHER FROM laEstECost FIELDS nECost1,nECost2,nECost3,nECost4,nECost5 
          GATHER FROM laECost    FIELDS nELanCost1,nELanCost2,nELanCost3,nELanCost4,nELanCost5
          SELECT CTKTRCVL
          
          *E300935,4 Change lcKey expression.
          *lcKey = 'I'+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+IIF(llByCarton,STR(LineNo,6),'')
          lcKey = cuttkt+style+dyelot+ccarton+STR(nLineNo,6)+STR(lineno,6)

        ENDIF
        
        SELECT (lcTmpLine)
        APPEND BLANK
        
        *E301480,1 NAD (Start) cosider the case of Inter location Po batch 
        *REPLACE cStyType  WITH 'P',;
                PO        WITH CTKTRCVL.Cuttkt,;
                Vendor    WITH POSHDR.Vendor,;
                Scale     WITH STYLE.Scale,; 
                nLanPrRat WITH lnCrRt1,;
                nLanDuRat WITH lnCrRt2,;
                TranCd    WITH CTKTRCVL.TranCd
        
        REPLACE cStyType  WITH lcTypSeek,;
                PO        WITH CTKTRCVL.Cuttkt,;
                Vendor    WITH POSHDR.Vendor,;
                Scale     WITH STYLE.Scale,; 
                nLanPrRat WITH lnCrRt1,;
                nLanDuRat WITH lnCrRt2,;
                TranCd    WITH CTKTRCVL.TranCd
         
        *E301480,1 NAD (End)
          
        *E300935,4 Add dyelot field in P/O by batch case.  [begin]
        *GATHER FROM laFields FIELDS cCarton,Lineno,Style,cWareCode,Reference,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,cStyGrade,nLineNo
        GATHER FROM laFields FIELDS cCarton,Lineno,Style,Dyelot,cWareCode,Reference,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,cStyGrade,nLineNo
        *E300935,4 Add dyelot field in P/O by batch case.  [end]
        *C200170,1 AMH Add Get TotStk & Tot Bal in case of issue inter-location P/O Batch [Start]
        IF lcPType='H'
          REPLACE  TotStk WITH CTKTRCVL.TotQty;
                   TotDam WITH 0;
                   TotCan WITH 0;
                   TotBal WITH TotQty-TotStk;
                   Lineno WITH nLineNo
        ENDIF
        *C200170,1 AMH [End]

        GATHER FROM laEstiCost FIELDS nCost1,nCost2,nCost3,nCost4,nCost5          
        GATHER FROM laEstiCost FIELDS nLan_CST1,nLan_CST2,nLan_CST3,nLan_CST4,nLan_CST5 
        GATHER FROM laEstECost FIELDS nECost1,nECost2,nECost3,nECost4,nECost5 
        GATHER FROM laECost    FIELDS nELanCost1,nELanCost2,nELanCost3,nELanCost4,nELanCost5
      ENDSCAN

    ELSE  && C/t Batch.   
      SCAN WHILE cType+TmpRcvNum = 'M'+lcBatch    
        SELECT CUTTKTL
        =SEEK(CTKTRCVL.Cuttkt+CTKTRCVL.Style)
        SCATTER FIELDS nCost1,nCost2,nCost3,nCost4,nCost5 TO laEstiCost
        SCATTER FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd TO laAlo
        SELECT CTKTRCVL
        =SEEK(Style,'STYLE')
        *B603100,1 SSH 22/05/2000 Fix the bug of incoret recieved cost in case of 
        *B603100,1 SSH 22/05/2000 recieve by batch.
        *SCATTER FIELDS Cuttkt,TranCd,cCarton,Lineno,Style,Dyelot,cWareCode,Reference,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,cStyGrade,nLineNo TO laFields
        SCATTER FIELDS Cuttkt,TranCd,cCarton,Lineno,Style,Dyelot,cWareCode,Reference,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,STYLE.cStyGrade,nLineNo TO laFields
        *B603100,1 SSH 22/05/2000 [End]
        IF cType+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+IIF(llByCarton,STR(LineNo,6),'') <> lcKey
          SELECT (lcTmpLine)
          APPEND BLANK
          GATHER FROM laFields   FIELDS Cuttkt,TranCd,cCarton,Lineno,Style,Dyelot,cWareCode,Reference,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,cStyGrade,nLineNo
          GATHER FROM laEstiCost FIELDS nCost1,nCost2,nCost3,nCost4,nCost5          
          GATHER FROM laEstiCost FIELDS nLan_CST1,nLan_CST2,nLan_CST3,nLan_CST4,nLan_CST5 
          REPLACE TranCd WITH '1'
          GATHER FROM laAlo    FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
          SELECT CTKTRCVL
          lcKey='M'+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+IIF(llByCarton,STR(LineNo,6),'')
        ENDIF
        SELECT (lcTmpLine)
        APPEND BLANK
        GATHER FROM laFields   FIELDS Cuttkt,TranCd,cCarton,Lineno,Style,Dyelot,cWareCode,Reference,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,cStyGrade,nLineNo
        GATHER FROM laEstiCost FIELDS nCost1,nCost2,nCost3,nCost4,nCost5
        GATHER FROM laEstiCost FIELDS nLan_CST1,nLan_CST2,nLan_CST3,nLan_CST4,nLan_CST5
      ENDSCAN
    
    ENDIF

    *C200170,1 AMH In case of issue inter-location P/O Batch read the lines and return [Start]
    IF lcPType = 'H'
      IF !EOF()
        laScrMode=.F.
        laScrMode[4]=.T.
      ELSE
        GO TOP
        IF !EOF()
            laScrMode=.F.
            laScrMode[4]=.T.
        ENDIF
      ENDIF
      SHOW GETS
      
      *--Read line info.
      SELECT (lcTmpLine)
      =lfReadLine(EOF())

      SELECT(lnAlias)
      RETURN
    ENDIF
    *C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
    
    STORE 0  TO lnTStk,lnTDam,lnTCan
    lcVar1=''
    SELECT CTKTRCVL
    
    *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.   
    *=SEEK(IIF(llMfCall,'M','I')+lcBatch)
    =SEEK(IIF(llMfCall,'M',IIF(lcPType='L','N','I'))+lcBatch)
    *E301480,1 NAD (End)
    
    *E300935,4 Change lcKey to match imported styles with dyelots.
    *lcKey = cType+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+IIF(llByCarton,STR(LineNo,6),'')
    lcKey =  Cuttkt+Style+Dyelot+cCarton+STR(nLineNo,6)+STR(LineNo,6)
    
    *E301480,1 NAD (Start) Add the Inter location Po batch to the condition. 
    *SCAN WHILE cType+TmpRcvNum = IIF(llMfCall,'M','I')+lcBatch
    SCAN WHILE cType+TmpRcvNum = IIF(llMfCall,'M',IIF(lcPType='L','N','I'))+lcBatch
    *E301480,1 NAD (End)
    
      *E300935,4 There is no need to cType+TmpRcvNum because it's in scan command.
      *IF cType+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+IIF(llByCarton,STR(LineNo,6),'') <> lcKey
      IF Cuttkt+Style+Dyelot+cCarton+STR(nLineNo,6)+STR(LineNo,6) <> lcKey
        SELECT (lcTmpLine)
        SEEK '1'+lcVar1
        REPLACE TotStk WITH lnTStk,;
                TotDam WITH lnTDam,;
                TotCan WITH lnTCan
        STORE 0 TO lnTStk,lnTDam,lnTCan
        SELECT CTKTRCVL
        *lcKey=cType+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+IIF(llByCarton,STR(LineNo,6),'')
        lcKey =  Cuttkt+Style+Dyelot+cCarton+STR(nLineNo,6)+STR(LineNo,6)
      ENDIF
    
      *lcVar1=cCarton+Cuttkt+Style+IIF(llMfCall,Dyelot,'')+IIF(llByCarton,STR(LineNo,6),'')
      lcVar1 =cCarton+Cuttkt+Style+Dyelot+IIF(llMfCall,'',STR(LineNo,6))

      DO CASE
        CASE TranCd = '2'
          lnTStk=lnTStk+TotQty
        CASE TranCd = lcOthrTrCd
          lnTDam=lnTDam+TotQty
        CASE TranCd = lcCanlTrCd
          lnTCan=lnTCan+TotQty
      ENDCASE    
    ENDSCAN
    *--Update last record.
    SELECT (lcTmpLine)
    SEEK '1'+lcVar1
    REPLACE TotStk WITH lnTStk,;
            TotDam WITH lnTDam,;
            TotCan WITH lnTCan

    
    *--Get originals.

    SELECT (lcTmpLine)
    IF !llByCarton
    *--In case of not by carton batch update lineNo with original lineNo on P/o or C/t.
    *--In case of by carton batch it will be updated while saving.
      REPLACE ALL LineNo WITH nLineNo
    ENDIF
    DIME laOpnQty[8]
    laOpnQty  =0
    *--Compute previous open balance on style P/o.----------------
    SELECT (lcTmpLine)
    SEEK '1'
    SCAN WHILE TranCd='1'
    
      *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
      *lcMainKy = IIF(llMfCall,Cuttkt+Style+Dyelot,'P'+PO+Style+STR(nLineNo,6))
      lcMainKy = IIF(llMfCall,Cuttkt+Style+Dyelot,IIF(lcPType='L','N','P')+PO+Style+STR(nLineNo,6))
      *E301480,1 NAD (End)
      
      =lfGetOpen( lcMainKy )
      SELECT (lcTmpLine)
      FOR I=1 TO 8
        Z=STR(I,1)
        REPLACE Qty&Z WITH laOpnQty[I]
      ENDFOR 
      REPLACE TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
              TotBal WITH IIF(TotQty-(TotStk+TotDam+TotCan)<0,0,TotQty-(TotStk+TotDam+TotCan)) 
      laOpnQty=0
    ENDSCAN

    *-- Check Batch Line Quantity if the Batch was modifed.
    *-- ( laCan[lnCnxt] > laOrg[lnCnxt] - laAlo[lnCnxt] ).
    WAIT WINDOW 'Validate the batch lines quantity...' NOWAIT
    IF llSOInstld
      SCAN FOR TranCd = '1'
        lnRecNo = RECNO()

        *E300935,4 Change key to have dyelots in P/O and cWarecode (Add Location)
        *lcLKey=cCarton+IIF(llMFCall,Cuttkt,Po)+Style+IIF(llMFCall,IIF(llDyelot,Dyelot,''),STR(LineNo,6))
        lcLKey=cCarton+IIF(llMFCall,Cuttkt,Po)+Style+PADR(Dyelot,10)+PADR(cWareCode,6)+STR(LineNo,6)

        DIME laOrg[8],laCan[8],laAlo[8],laBud[8]
        STORE 0 TO laOrg,laCan,laAlo,laBud
        *--Get Cancel quantity.
        IF SEEK(lcCanlTrCd+lcLKey)
          SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laCan
        ENDIF
        =SEEK('1'+lcLKey)
        SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laOrg
        SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laBud
        SCATTER FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8 TO laAlo
        =SEEK(&lcTmpLine..Style,'STYLE')
        =SEEK('S'+STYLE.Scale,'SCALE')

        FOR lnCnxt = 1 TO 8
          *B603216,1 ASH 10/18/1999 (Begin) Fix the bug of getting wrong message in case of no cancel qty found.
          *IF ( laCan[lnCnxt] > laOrg[lnCnxt] - laAlo[lnCnxt] )
          IF laCan[lnCnxt]<>0 AND ( laCan[lnCnxt] > laOrg[lnCnxt] - laAlo[lnCnxt] )
          *B603216,1 ASH 10/18/1999 (End)
            lcCnxt = STR(lnCnxt,1)
            lcSz&lcCnxt = SCALE.Sz&lcCnxt 
            lcCtPkKey = IIF(llMfCall,'1'+&lcTmpLine..Cuttkt,'2'+&lcTmpLine..PO)+&lcTmpLine..Style
            IF SEEK(lcCtPkKey,'CutPick')
              =lfChkOrdQt(lcCnxt,.T.)
            ENDIF
          ENDIF
        ENDFOR
        SELECT (lcTmpLine)
      
        *E300935,4 Check of phisical record before go to it.
        IF (lnRecNo # 0) AND (lnRecNo <= RECCOUNT())
          GOTO lnRecNo
        ENDIF  
      ENDSCAN
    ENDIF 
    WAIT CLEAR 
    GO TOP

  ************************
  *-- F R O M   C / T. --*
  ************************
  CASE lcPType = 'M'
    *--Get originals.
    DIME laOpnQty[8]
    laOpnQty  =0
    *B603275,1 AMM Declare variable that hold last cuttkt line number
    lnLastLNo = 0
    *B603275,1 AMM end
    SELECT CUTTKTL
    lnSvRc=RECNO()
    SCATTER MEMVAR
    SCATTER FIELDS nCost1,nCost2,nCost3,nCost4,nCost5 TO laEstiCost
    lcLKey = Cuttkt+Style+IIF(llDyelot,lcDyelot,SPACE(10))
    *--Compute previous open balance on C/t style/dyelot.----------------
    =lfGetOpen( lcLKey )
    llNewLn=.F.
    IF llDyelot AND !EMPTY(lcDyelot) AND !SEEK(lcLKey,'CUTTKTL')
      llNewLn =.T.
      *B603275,1 AMM Get last line number
      =SEEK(LEFT(lcLKey,6))
      SCAN REST WHILE CUTTKT = LEFT(lcLKey,6)
        lnLastLNo = MAX(lnLastLNo,LINENO)
      ENDSCAN
      *B603275,1 AMM end
    ENDIF
    
    *E300935,4 Check of phisical record before go to it.
    IF BETWEEN(lnSvRc,1,RECCOUNT())
      GOTO lnSvRc
    ENDIF
    
    SELECT (lcTmpLine)
    *B603275,1 AMM Get last line number
    IF llNewLn 
      lcOldLOrd = ORDER()
      SET ORDER TO TmpLine3
      =SEEK(LEFT(lcLKey,6))
      SCAN REST WHILE CUTTKT = LEFT(lcLKey,6)
        lnLastLNo = MAX(lnLastLNo,LINENO)
      ENDSCAN
      m.LineNo = lnLastLNo + 1
      SET ORDER TO (lcOldLOrd)
    ENDIF
    *B603275,1 AMM end
    APPEND BLANK

    *E300935,4 If this line is found before (make N/A location)
    m.cWareCode = IIF(llEmpWare,'',m.cWareCode)
    
    GATHER MEMVAR
    *--If specific operation lot selected.
    IF llSpecLot
      GATHER FROM laLotArry FIELDS Qty1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8
    ELSE
      GATHER FROM laOpnQty  FIELDS Qty1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8
    ENDIF
    *FOR I=1 TO 8
    *  Z=STR(I,1)
    *  REPLACE Qty&Z WITH laOpnQty[I]
    *ENDFOR 
    
    *C101424,1 Replace cRcvBy according to current receive (C/T or Style) [Begin]
    *REPLACE Dyelot   WITH IIF(llDyelot,lcDyelot,''),;
    *        lNewLn   WITH llNewLn,;
    *        cLotNo   WITH lcLotNo,;
    *        clastopr WITH lcClrLstOp,;
    *        TotQty   WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
    *        TotStk   WITH 0,;
    *        TotDam   WITH 0,;
    *        TotCan   WITH 0,;
    *        TotBal   WITH TotQty
    
    REPLACE Dyelot   WITH IIF(llDyelot,lcDyelot,''),;
            lNewLn   WITH llNewLn,;
            cLotNo   WITH lcLotNo,;
            clastopr WITH lcClrLstOp,;
            TotQty   WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
            TotStk   WITH 0,;
            TotDam   WITH 0,;
            TotCan   WITH 0,;
            TotBal   WITH TotQty,;
            cRcvBy   WITH lcCurrSty
    *C101424,1 Replace cRcvBy according to current receive (C/T or Style) [End  ]

    GATHER FROM laEstiCost FIELDS nLan_Cst1,nLan_Cst2,nLan_Cst3,nLan_Cst4,nLan_Cst5 



  ********************
  *-- B Y   P / O. --*
  ********************
  *C200080,1 AMM Add the dye order type 'D'
  *E301291,4 AMM Add issue/receive adornment order types 'AE'
  *CASE ( lcPType$'IRNO' OR (lcPType='S' AND llShpPO) )
  CASE ( lcPType$'IRNODAE' OR (lcPType='S' AND llShpPO) )
  *C200080,1 AMM end

    IF llMulCurr
      lcCur1  = POSHDR.cPriceCur
      lnCrRt1 = IIF(lcCur1=gcBaseCurr,1,gfChkRate('lnCurrUnt1',lcCur1,ldRcvDate,llEditExRt,gcAct_comp,.F.)) 
      lcCur2  = POSHDR.cDutyCur
      lnCrRt2 = IIF(lcCur2=gcBaseCurr,1,gfChkRate('lnCurrUnt2',lcCur2,ldRcvDate,llEditExRt,gcAct_comp,.F.)) 
      *--Cannot accept zero or -ve rates.
      IF lnCrRt1=0 OR lnCrRt2=0
        IF !llEditExRt
          *--This line has currency with zero rate, it will be ignored.' 
          = gfModalGen('TRM34079B42000','DIALOG')
          STORE ' ' TO lcStyle,lcCur1,lcCur2
          SHOW GETS
          RETURN
        ELSE
          STORE 1 TO lnCrRt1,lnCrRt2
        ENDIF
      ENDIF
    ENDIF

    IF llNewItem
      SELECT STYLE
      SCATTER FIELDS nICost1,nICost2,nICost3,nICost4,nICost5 TO laEstiCost
      SELECT (lcTmpLine)
      lnPolstln=MAX(lnPolstln+1,POSHDR.lastline+1)
      APPEND BLANK
  
      *E300935,4 If this line is found before (make N/A location)
      lcWareCode = IIF(llEmpWare,'',lcWareCode)
      
      *E300935,4 Add Dyelot field to next replace line. [begin]
      *REPLACE cStyType  WITH IIF(lcPType $ 'RN',lcPType,'P'),;
      *        Po        WITH lcTCode,;
      *        TranCd    WITH '1',;
      *        Vendor    WITH POSHDR.Vendor,;
      *        Style     WITH lcStyle,;
      *        Scale     WITH STYLE.Scale,;
      *        LineNo    WITH lnPolstln,;
      *        cWareCode WITH lcWareCode,;              
      *        nLanPrRat WITH lnCrRt1,;
      *        nLanDuRat WITH lnCrRt2,;
      *        ShipNo WITH IIF(llShpPO,lcShpCode,''),;
      *        TotQty WITH 0,;
      *        TotStk WITH 0,;
      *        TotDam WITH 0,;
      *        TotCan WITH 0,;
      *        TotBal WITH 0,;
      *        lNewLn WITH .T.

      *C200080,1 AMM Add the dye order type 'D'
      *E301291,4 AMM add adornment order type 'A'
      REPLACE cStyType  WITH IIF(lcPType $ 'RNDA',lcPType,'P'),;
              Po        WITH lcTCode,;
              TranCd    WITH '1',;
              Vendor    WITH POSHDR.Vendor,;
              Style     WITH lcStyle,;
              Scale     WITH STYLE.Scale,;
              LineNo    WITH lnPolstln,;
              cWareCode WITH lcWareCode,;              
              nLanPrRat WITH lnCrRt1,;
              nLanDuRat WITH lnCrRt2,;
              ShipNo WITH IIF(llShpPO,lcShpCode,''),;
              TotQty WITH 0,;
              TotStk WITH 0,;
              TotDam WITH 0,;
              TotCan WITH 0,;
              TotBal WITH 0,;
              lNewLn WITH .T.,;
              Dyelot WITH lcDyelot

      *E300935,4 Add Dyelot field to next replace line. [end]

      IF lcCur1=STYLE.cPriceCur
        REPLACE nCost1  WITH laEstiCost[1],nLan_Cst1 WITH laEstiCost[1]
        =lfGetEqv('1',nLanPrRat,0,lnCurrUnt1,0,nLan_Cst1,0,0,0,0)
        REPLACE nECost1 WITH laECost[1],nELanCost1 WITH laECost[1]
      ENDIF
      IF lcCur2=STYLE.cDutyCur
        REPLACE nCost2    WITH laEstiCost[2],nCost3    WITH laEstiCost[3],;
                nCost4    WITH laEstiCost[4],nCost5    WITH laEstiCost[5],;
                nLan_Cst2 WITH laEstiCost[2],nLan_Cst3 WITH laEstiCost[3],;
                nLan_Cst4 WITH laEstiCost[4],nLan_Cst5 WITH laEstiCost[5]
        =lfGetEqv('2345',0,nLanDuRat,0,lnCurrUnt2,0,nLan_Cst2,nLan_Cst3,nLan_Cst4,nLan_Cst5)
        GATHER FROM laECost FIELDS nECost2,nECost3,nECost4,nECost5
        GATHER FROM laECost FIELDS nELanCost2,nELanCost3,nELanCost4,nELanCost5
      ENDIF 
    ELSE

      *--Get originals.
      DIME laOpnQty[8]
      laOpnQty  =0
      SELECT POSLN
      lnSvRc=RECNO()
      SCATTER MEMVAR 
      *E301291,4 AMM Get nCost1 only in case of issue adornment PO
      *SCATTER FIELDS nCost1,nCost2,nCost3,nCost4,nCost5 TO laEstiCost
      IF lcPType = 'A'
        SCATTER FIELDS nCost1 TO laEstiCost
      ELSE
        SCATTER FIELDS nCost1,nCost2,nCost3,nCost4,nCost5 TO laEstiCost
      ENDIF
      *E301291,4 AMM end
      *--Compute previous open balance on style P/o.----------------
      *C200080,1 AMM Add the dye order type 'D'
      *E301291,4 AMM Add issue/receive adornment PO types 'AE'
      *lcSeekTyp = IIF(lcPType $ 'RNO',IIF(lcPType='O','N',lcPType),'P')
      
      *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
      *lcSeekTyp = IIF(lcPType $ 'RNODAE',IIF(lcPType='O','N',IIF(lcPType='E','A',lcPType)),'P')
      lcSeekTyp = IIF(lcPType $ 'RNODAEL',IIF(lcPType $'OL','N',IIF(lcPType='E','A',lcPType)),'P')
      *E301480,1 NAD (End)
       
      *C200080,1 AMM end

      = lfGetOpen ( lcSeekTyp+PO+Style+STR(LineNo,6) )
      
      *E300935,4 Check of phisical record before go to it.
      IF BETWEEN(lnSvRc,1,RECCOUNT())
        GOTO lnSvRc
      ENDIF  

      SELECT (lcTmpLine)
      APPEND BLANK

     *E300935,4 If this line is found before (make N/A location)
      m.cWareCode = IIF(llEmpWare,'',m.cWareCode)
      GATHER MEMVAR
      REPLACE TranCd WITH '1'

      *--If specific operation lot selected.
      IF llSpecLot
        GATHER FROM laLotArry FIELDS Qty1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8
      ELSE
        GATHER FROM laOpnQty  FIELDS Qty1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8
      ENDIF
      *FOR I=1 TO 8
      *  Z=STR(I,1)
      *  REPLACE Qty&Z WITH laOpnQty[I]
      *ENDFOR 
      
      *E300935,4 Add Dyelot field to next replace line. [begin]
      *REPLACE nLanPrRat WITH lnCrRt1,;
      *        nLanDuRat WITH lnCrRt2,;
      *        cLotNo    WITH lcLotNo,;
      *        clastopr  WITH lcClrLstOp,;
      *        ShipNo WITH IIF(llShpPO,lcShpCode,''),;
      *        TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
      *        TotStk WITH IIF(lcPType='O',TotQty,0),;
      *        TotDam WITH 0,;
      *        TotCan WITH 0,;
      *        TotBal WITH IIF(lcPType='O',0,TotQty)
      *E301291,4 AMM Add case of receive adornment PO
      *REPLACE nLanPrRat WITH lnCrRt1,;
              nLanDuRat WITH lnCrRt2,;
              cLotNo    WITH lcLotNo,;
              clastopr  WITH lcClrLstOp,;
              ShipNo WITH IIF(llShpPO,lcShpCode,''),;
              TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
              TotStk WITH IIF(lcPType = 'O' OR lcAuto = 'A',TotQty,0),;
              TotDam WITH 0,;
              TotCan WITH 0,;
              TotBal WITH IIF(lcPType = 'O' OR lcAuto = 'A',0,TotQty),;
              Dyelot WITH lcDyelot,;
              LAUTOMODE WITH (lcAuto='A')
      REPLACE nLanPrRat WITH lnCrRt1,;
              nLanDuRat WITH lnCrRt2,;
              cLotNo    WITH lcLotNo,;
              clastopr  WITH lcClrLstOp,;
              ShipNo WITH IIF(llShpPO,lcShpCode,''),;
              TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
              TotStk WITH IIF(lcPType $ 'OE' OR lcAuto = 'A',TotQty,0),;
              TotDam WITH 0,;
              TotCan WITH 0,;
              TotBal WITH IIF(lcPType $ 'OE' OR lcAuto = 'A',0,TotQty),;
              Dyelot WITH lcDyelot,;
              LAUTOMODE WITH (lcAuto='A')
      *E301291,4 AMM end
      *E300935,4 Add Dyelot field to next replace line. [end]

      *E301291,4 AMM Get landed cost1 only in case of issue adornment order
      *GATHER FROM laEstiCost FIELDS nLan_CST1,nLan_CST2,nLan_CST3,nLan_CST4,nLan_CST5 
      IF lcPType = 'A'
        GATHER FROM laEstiCost FIELDS nLan_CST1
      ELSE
        GATHER FROM laEstiCost FIELDS nLan_CST1,nLan_CST2,nLan_CST3,nLan_CST4,nLan_CST5 
      ENDIF
      *E301291,4 AMM end
      
      *E301291,4 AMM Consider case of issue adornment order 
      *IF llMulCurr
      IF llMulCurr .AND. lcPType # 'A'
      *E301291,4 AMM end
        =lfGetEqv('12345',nLanPrRat,nLanDuRat,lnCurrUnt1,lnCurrUnt2,;
                  nLan_Cst1,nLan_Cst2,nLan_Cst3,nLan_Cst4,nLan_Cst5)
      ELSE
        =ACOPY(laEstiCost,laECost)
      ENDIF

      *E301291,4 AMM Get cost1 only in case of issue adornment PO
      *GATHER FROM laECost FIELDS nELanCost1,nELanCost2,nELanCost3,nELanCost4,nELanCost5
      IF lcPType = 'A'
        GATHER FROM laECost FIELDS nELanCost1
      ELSE
        GATHER FROM laECost FIELDS nELanCost1,nELanCost2,nELanCost3,nELanCost4,nELanCost5
      ENDIF
      *E301291,4 AMM end
      *--If recieve inter Location P/o defult recieved qty by intransit qty.
      *--Or If specific operation lot selected.
      *E301291,4 AMM Consider the case of receive adornment PO
      *IF lcPType = 'O' OR lcAuto = 'A'
      IF lcPType $ 'OE' OR lcAuto = 'A'
      *E301291,4 AMM end
        lnTSvRc=RECNO()
        SCATTER MEMVAR
        APPEND BLANK
        GATHER MEMVAR
        REPLACE TranCd WITH '2'
        
        *E300935,4 Check of phisical record before go to it.
        IF BETWEEN(lnTSvRc,1,RECCOUNT())
          GOTO lnTSvRc
        ENDIF  
        
        lnTotStk = lnTotStk + TotStk
      ENDIF

   ENDIF

ENDCASE

*--Swich the mode if there is lines selected.
IF !EOF()
  laScrMode=.F.
  laScrMode[4]=.T.
ELSE
  GO TOP
  IF !EOF()
    laScrMode=.F.
    laScrMode[4]=.T.
  ENDIF
ENDIF
SHOW GETS

*B803030,1 TAK 02/08/2000 (Begin) In case of receive by shipment
*B803030,1                and the screen mode is still in the select mode 
*B803030,1                then return to the select mode. This is done in 
*B803030,1                case you select a shipment that has only a PO 
*B803030,1                with status hold.
IF lcPType = 'S' AND laScrMode[1]
  =lfClearInfo()
  SHOW GET PbSType   ENABLE
  SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
  SHOW GET lcShpCode ENABLE
  SHOW GET pbNew     DISABLE
  llAbort = .T.
  RETURN
ENDIF
*B803030,1 TAK 02/08/2000 (End)
*--Read line info.
SELECT (lcTmpLine)
=lfReadLine(EOF())

SELECT(lnAlias)
RETURN
*-- end of lfGetInfo. lcwarecode

*!*************************************************************
*! Name    : lfClearInfo
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Clear information.
*!*************************************************************
FUNCTION lfClearInfo

*--Clear Header.
STORE ' ' TO lcAirWayB,lcShpRef,lcBDesc,lcBStatus,lcSOrder,lcAOrder
STORE {}  TO ldEnterd,ldEta,ldBDate
STORE 0   TO lnCartons,lnTotStk,lnTotDam,lnTotCan,lnPolstln
STORE .F. TO llByCarton

*C101424,1 [BEGIN]
*-- When <CANCEL> make the user free to receive by C/T or by style
IF llMFCall AND lcPtype='M'
  STORE ' ' TO lcStyle,lcTcode,lcStyDesc
 _CUROBJ=OBJNUM(lcTCode)
ENDIF
*C101424,1 [END..]

*--Clear Line.
=lfReadLine(.T.)
RETURN


*!*************************************************************
*! Name    : lfReadLine
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Read line information.
*!*************************************************************
FUNCTION lfReadLine
PARA llClearLn
lnAlias = SELECT()
IF ! llClearLn
  SELECT (lcTmpLine)
  lnBrRecNo=RECNO()
  lcCarton= cCarton
  lcTCode    = IIF(llMfCall,Cuttkt,Po)
  lcStyle = Style
  *E300935,4 lnware does not changed in Point of sell
  *lnWare  = ASCAN(laWare,cWareCode,1)
  lnWare  = IIF(llCMInstld AND llPOSale,1,ASCAN(laWare,cWareCode,1))
  lcRefer = Reference
 
  *E300935,4 Now Dyelot field is in C/T and P/O.
  *lcDyelot= IIF(llMfCall,Dyelot,'')
  lcDyelot= Dyelot
  *C200080,1 AMM Add the dye order type 'D'
  *E301291,4 AMM Consider cases of issue/receive adornment PO
  *lcSeekTyp = IIF(lcPType $ 'RNO',IIF(lcPType='O','N',lcPType),'P')
 
  *E301480,1 NAD (Start)  Add the Inter location Po batch to the condition.
  lcSeekTyp = IIF(lcPType $ 'RNODAE',IIF(lcPType='O','N',IIF(lcPType='E','A',lcPType)),'P')
  lcSeekTyp = IIF(lcPType $ 'RNODAEL',IIF(lcPType $'OL','N',IIF(lcPType='E','A',lcPType)),'P')
   *E301480,1 NAD (End)
   
  *E301291,4 AMM
  *C200080,1 AMM end
  IF !llMfCall
    =SEEK(lcSeekTyp+lcTCode,'POSHDR')
  ENDIF
  IF llMulCurr AND !llMfCall
    *-Read currency.
    lcCur1  = POSHDR.cPriceCur
    lcCur2  = POSHDR.cDutyCur
    *-Read rate.
    lnRate1 = nLanPrRat
    lnRate2 = nLanDuRat
    *-Read units.
    *=gfChkRate('lnCurrUnt1',lcCur1,ldRcvDate,.F.,gcAct_comp,.F.) 
    *=gfChkRate('lnCurrUnt2',lcCur2,ldRcvDate,.F.,gcAct_comp,.F.) 
    *-Read symble.
    
    *IF !laOpFile[12]
      = lfOpn_Rest(gcSysHome,'SYCCURR','CCURRCODE')
    *ENDIF
    
    lcPfrnsmbl= IIF(SEEK(lcCur1,'SycCurr'),SycCurr.cCurrSmbl,'')
    lcDfrnsmbl= IIF(SEEK(lcCur2,'SycCurr'),SycCurr.cCurrSmbl,'')
  ENDIF
  *E301291,4 AMM Consider case of issue adornment PO
  *lcWareCode = IIF(lcPType = 'N',PADR(POSHDR.Vendor,6),cWareCode)
  lcWareCode = IIF(lcPType $ 'NA',PADR(POSHDR.Vendor,6),cWareCode)
  *E301291,4 AMM end
  lcStyDesc = IIF(SEEK(lcStyle,'STYLE'),STYLE.Desc1,'')
  *B603237,1 Start.
  lcSOrder = Reference
  lcAOrder = IIF(lcPType $ 'AE' AND llSpecHndl,PO,'')
  lcTCode  = IIF(lcPType $ 'AE' AND llSpecHndl,ShipNo,lcTCode)
  *B603237,1 End.

ELSE
  STORE ' ' TO lcCarton,lcTCode,lcStyle,lcRefer,lcCur1,lcCur2,;
               lcStyDesc,lcDyelot,lcPfrnSmbl,lcDfrnSmbl,lcWareCode,lcSOrder,lcAOrder

  *C200170,4 AMH Add case of issue inter-location P/O Batch [Start]
  IF lcPType = 'H'
    lcSource = ''
  ENDIF
  *C200170,4 AMH [End]
  
  STORE 0   TO lnWare,lnRate1,lnRate2
  STORE 1   TO lnCurrUnt1,lnCurrUnt2
ENDIF
=lfShowScr()
*E128404,1  TMI [Start] if the screen "lcBrDtTtl" is released then redefine it
IF !WEXIST(lcBrDtTtl)
  =lfActBrow()
ENDIF
*E128404,1  TMI [End  ] 
SHOW WINDOW (lcBrDtTtl) REFRESH
=lfRefresh(lcWinCh5)
SELECT(lnAlias)
RETURN

*!*************************************************************
*! Name    : lfvNewLn
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Add new line.
*!*************************************************************
FUNCTION lfvNewLn

*--Clear Line.
*E301069,1 Previus po no.
lcPrvTPO = lcTCode

=lfReadLine(.T.)
SHOW GETS WINDOW (lcWinCh5) DISABLE ONLY
SHOW GET pbKey   ENABLE     

*C101424,1 Adjust current objects according to Last receive by [Begin]
*E301069,1 restore po no.
*lcTCode = lcPrvTPO
*_CUROBJ=OBJNUM(lcTCode)

*-- WALID
IF llMFCall AND lcPType='M'

  lnSelAls = SELECT(0)
  SELECT (lcTmpLine)
  
  *-- WALID
  IF EMPTY(PADR(cRcvBy,lnMjrWid))

    *E301069,1 restore po no.
    lcStyle = ' '
    lcTCode = lcPrvTPO
    _CUROBJ=OBJNUM(lcTCode)

  ELSE
    
    *E301069,1 restore po no.
    lcStyle = cRcvBy
    lcTCode = ' '
    _CUROBJ=OBJNUM(lcStyle)
  
  ENDIF

  SHOW GET lcStyle ENABLE
  SHOW GET ibStyle ENABLE
  SELECT (lnSelAls)

ELSE

  *E301069,1 restore po no.
  lcTCode = lcPrvTPO
  _CUROBJ=OBJNUM(lcTCode)

ENDIF
*C101424,1 Adjust current objects according to Last receive by [End  ]

SHOW GET lcTCode ENABLE
SHOW GET ibTCode ENABLE

RETURN


*!*************************************************************
*! Name    : lfvRemLn
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Remove line.
*!*************************************************************
FUNCTION lfvRemLn
*-Are you sure you want to delete this line?
IF gfModalGen('QRM34036B42002','DIALOG') = 1

  *E300935,4 declare array laRemoved to have the total removed quantity
  *B602698,1[BEGIN] Do it in case of C/T                      
  *IF lcPType $ 'IS'
  *C200080,1 AMM Add the dye order type 'D'
  *IF lcPType $ 'ISM'
  IF lcPType $ 'ISMD'
  *C200080,1 AMM end
  *B602698,1[END..]
    PRIVATE laRemoved
    DECLARE laRemoved[8]
    laRemoved = 0
  ENDIF  

  SELECT (lcTmpLine)
  *E300935,4 Change Key expression due to change index expression.
  *lcLKey=SPACE(3)+IIF(llMFCall,Cuttkt,Po)+Style+IIF(llMFCall,IIF(llDyelot,Dyelot,''),STR(LineNo,6))
  lcLKey=SPACE(3)+IIF(llMFCall,Cuttkt,Po)+Style+Dyelot+PADR(cWareCode,6)+IIF(llMFCall,'',STR(LineNo,6))
  
  IF SEEK(lcCanlTrCd+lcLKey)
    *B602698,1[BEGIN] Do it in case of C/T
    *IF lcPType $ 'IS'
    *C200080,1 AMM Add the dye order type 'D'
    *IF lcPType $ 'ISM'
    IF lcPType $ 'ISMD'
    *C200080,1 AMM end
    *B602698,1[END..]
      =lfEvalRemQ()  &&E300935,4 Evaluate removed quantity.
    ENDIF  
    DELETE

  ENDIF
  
  IF SEEK(lcOthrTrCd+lcLKey)
    *E300935,4 Change DELETE command due to change index expression.
    *DELETE REST WHILE Trancd+cCarton+IIF(llMFCall,Cuttkt,Po)+Style+IIF(llMFCall,IIF(llDyelot,Dyelot,''),STR(LineNo,6))=lcOthrTrCd+lcLKey
    SCAN REST WHILE Trancd+cCarton+IIF(llMFCall,Cuttkt,Po)+Style+Dyelot+;
                      PADR(cWareCode,6)+IIF(llMFCall,'',STR(LineNo,6))=lcOthrTrCd+lcLKey
      *B602698,1[BEGIN] Do it in case of C/T
      *IF lcPType $ 'IS'
      *C200080,1 AMM Add the dye order type 'D'
      *IF lcPType $ 'ISM'
      IF lcPType $ 'ISMD'
      *C200080,1 AMM end
      *B602698,1[END..]
        =lfEvalRemQ()  &&E300935,4 Evaluate removed quantity.
      ENDIF  
      DELETE

    ENDSCAN                  

  ENDIF
  
  IF SEEK('2'+lcLKey)
    *B602698,1[BEGIN] Do it in case of C/T
    *IF lcPType $ 'IS'
    *C200080,1 AMM Add the dye order type 'D'
    *IF lcPType $ 'ISM'
    IF lcPType $ 'ISMD'
    *C200080,1 AMM end
    *B602698,1[END..]
      =lfEvalRemQ()  &&E300935,4 Evaluate removed quantity.
    ENDIF  
    DELETE
  ENDIF
  
  IF SEEK('1'+lcLKey)
    lnTotStk=lnTotStk-TotStk
    lnTotDam=lnTotDam-TotDam
    lnTotCan=lnTotCan-TotCan

    *E300935,4 Add removed  quantity to the same lines with another warehouse/dyelots. [begin]
    *B602698,1[BEGIN] Do it in case of C/T
    *IF lcPType $ 'IS'
    *C200080,1 AMM  Add the dye order type 'D'
    *IF lcPType $ 'ISM'
    IF lcPType $ 'ISMD'
    *C200080,1 AMM end
    *B602698,1[END..]  
      lnCurLine = LineNo

      IF llMFCall
        lcScanVar = [TranCd+cCarton+Cuttkt+Style+Dyelot+cWareCode+STR(LineNo,6)]
      ELSE
        lcScanVar = [TranCd+cCarton+Po+Style+Dyelot+cWareCode+STR(LineNo,6)]
      ENDIF
      lcScanVal = ['1' + lcCarton + lcTCode + lcStyle]
      
      *SCAN FOR TranCd+cCarton+Po+Style+Dyelot+cWareCode+STR(LineNo,6) = ;
      *         '1' + lcCarton + lcTCode + lcStyle AND ;
      *         STR(LineNo,6) = STR(lnCurLine,6)  AND ;
      *         Dyelot+cWareCode # lcDyelot+lcWareCode

      SCAN FOR EVALUATE(lcScanVar) = EVALUATE(lcScanVal) AND ;
               STR(LineNo,6) = STR(lnCurLine,6)  AND ;
               Dyelot+cWareCode # lcDyelot+lcWareCode

        FOR lnI = 1 TO 8
            lcZ = STR(lnI,1)  
            REPLACE QTY&lcZ WITH QTY&lcZ + laRemoved[lnI] ,;
                    TOTQTY  WITH TOTQTY  + laRemoved[lnI]
        ENDFOR 
        *E300935,4 Now MAX Function is on the screen.
        *REPLACE TOTBAL WITH MAX(TOTQTY - TOTSTK - TOTDAM - TOTCAN,0)
        REPLACE TOTBAL WITH TOTQTY - TOTSTK - TOTDAM - TOTCAN
      ENDSCAN
      = SEEK('1'+lcLKey)

    ENDIF  
    *E300935,4 Add removed  quantity to the same lines with another warehouse/dyelots. [begin]

    DELETE
  ENDIF
  
  GO TOP
  IF EOF()
    =lfReadLine(.T.)
    SHOW GETS WINDOW (lcWinCh5) DISABLE ONLY
  ELSE
    =lfReadLine(.F.)
  ENDIF
  
  SHOW GET pbNew ENABLE
  SHOW GET pbKey ENABLE
  
  =lfRefresh(lcWinCh0)
  =lfCrUnSess('','',.T.)   
ENDIF
RETURN

*!*************************************************************
*! Name      : lfEvalRemQ
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/06/1998
*! Purpose   : Evaluate remove quantity to add it to another equavilent lines if found.
*!*************************************************************
*! Called from : lfvRemLn
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfEvalRemQ()
*!*************************************************************
*E300935,4
FUNCTION lfEvalRemQ
*E300935,4 Evaluate removed quantity. [begin]
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  laRemoved[lnI] = laRemoved[lnI] + Qty&lcI
ENDFOR
*E300935,4 Evaluate removed quantity. [end]
*-- end of lfEvalRemQ.

*!*************************************************************
*! Name    : lfvWare
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Valid Warehouse (Location).
*!*************************************************************
FUNCTION lfvWare
IF (lnWare = lcOldValue) OR (lcOldValue = 0) OR (llCMInstld AND llPOSale)
  RETURN
ENDIF  

*E300935,4 If N/A and user was not change it, return
IF (lnWare = 1) AND (lnWare <> lcOldValue)
  lnWare = lcOldValue
  lcWareCode=SUBSTR(laWare[lnWare],1,6)
  SHOW GET lnWare
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
 
lcWareCode=SUBSTR(laWare[lnWare],1,6)

*E300935,4 If This Style/Dyelot/location was found before in this P/o.
IF lfExistWar()
  lcMessage1 = 'Style'+IIF(EMPTY(lcDyelot),'','/Dyelot')+'/Location'
  
  *B605082,1 AMH Adding the case of the dye order case [Start]
  *lcMessage2 = IIF(llMfCall,'receiving cutting ticket','receiving purchase order')
  lcMessage2 = IIF(llMfCall,'receiving cutting ticket',IIF(lcPType='D','receiving dye order','receiving purchase order'))
  *B605082,1 AMH [End]
  
  *-This XXXX has been entered on this XXXX.
  = gfModalGen('TRM42107B42000','DIALOG',lcMessage1+'|'+lcMessage2)
  lnWare = lcOldValue
  SHOW GET lnWare
  lcWareCode = PADR(laWare[lnWare],6)
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

IF !SEEK(PADR(lcStyle,19)+lcWareCode+SPACE(10),'STYDYE')
  *-Style: xxx is not assigned to Location: xxx. "\<Add;\<Reenter"
  IF gfModalGen('QRM34048B42006','DIALOG',ALLTRIM(lcStyle)+'|'+lcWareCode) = 1
    DO gpAdStyWar WITH lcStyle,SPACE(10),lcWareCode
  ELSE
    lnWare = lcOldValue
    lcWareCode=SUBSTR(laWare[lnWare],1,6)
    SHOW GET lnWare
    RETURN
  ENDIF
ENDIF

SELECT (lcTmpLine)
lnRecNo= RECNO()

*E300935,4 Change it because Po can have dyelot.
*lcLKey = cCarton+IIF(llMFCall,Cuttkt,Po)+Style+IIF(llMFCall,IIF(llDyelot,Dyelot,''),STR(LineNo,6))
lcLKey = cCarton+IIF(llMFCall,Cuttkt,Po)+Style+Dyelot+PADR(cWareCode,6)+STR(lineNo,6)

SET ORDER TO TAG TmpLine2
SEEK lcLKey

*E300935,4 Change it because Po can have dyelot.
*REPLACE REST cWareCode WITH lcWareCode ;
*       WHILE cCarton+IIF(llMFCall,Cuttkt,Po)+Style+IIF(llMFCall,IIF(llDyelot,Dyelot,''),STR(LineNo,6))=lcLKey

*B605331,1 KHM 01/08/2001 (Begin) Adding the full index expression. seeking 
*B605331,1                and replace again in order to replace the received record.
*REPLACE REST cWareCode WITH lcWareCode ;
       WHILE cCarton+IIF(llMFCall,Cuttkt,Po)+Style+Dyelot+cWareCode+STR(LineNo,6) = lcLKey

REPLACE REST cWareCode WITH lcWareCode ;
       WHILE cCarton+IIF(llMFCall,Cuttkt,Po)+Style+Dyelot+cWareCode+STR(LineNo,6)+TranCd = lcLKey
SEEK lcLKey
REPLACE REST cWareCode WITH lcWareCode ;
       WHILE cCarton+IIF(llMFCall,Cuttkt,Po)+Style+Dyelot+cWareCode+STR(LineNo,6)+TranCd = lcLKey
*B605331,1 KHM 01/08/2001 (End)

SET ORDER TO TAG TmpLine1

*E300935,4 Check of phisical record before go to it.
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF  

SHOW WINDOW (lcBrDtTtl) REFRESH
RETURN

*!*************************************************************
*! Name      : lfvLocWare
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose   : Valid Select locations.
*!*************************************************************
FUNCTION lfvLocWare

llLoc = .F.
SHOW GET llLoc

IF EMPTY(lcWareCode)
  *-- Message : 'First, you must select location.'
  =gfModalGen('TRM42150B42001','DIALOG')
  _CUROBJ = OBJNUM(lnWare)
  RETURN
ENDIF

lcOldAlias = ALIAS() 
SELECT (lcTemLoc)
DELETE ALL FOR cWareCode<>lcWareCode AND Style=lcStyle
SELECT WHSLOC 
SET ORDER TO TAG WhsLoc
llFound=SEEK(lcWareCode)
SET ORDER TO TAG WhsLocSt
IF llFound
  =lfvLocat()
ELSE
  *--No bins have been assigned to location XXX .
  = gfModalGen('TRM42058B42000','DIALOG',ALLTRIM(lcWareCode))
ENDIF  
SELECT (lcOldAlias)
RETURN

*!*************************************************************
*! Name      : lfvLocat
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose   : Do Functions mover to select locations.
*!*************************************************************
FUNCTION lfvLocat

SELECT WHsLoc
SEEK PADR(lcStyle,19)+SPACE(6)+lcWareCode
*B607472,1 ALB Defualt Bin for style/Color that are assigned to warehouse [BEGIN]
IF !SEEK(PADR(lcStyle,19)+lcWareCode,lcTemLoc)
*B607472,1 ALB No defualt Bin for style/Color [END]
  SCAN WHILE STYLE+COLOR+CWARECODE+CLOCATION=PADR(lcStyle,19)+SPACE(6)+lcWareCode
    SCATTER MEMVAR
    SELECT (lcTemLoc)
    *B607472,1 ALB Defualt Bin for style/Color that are assigned to warehouse [BEGIN]
    *IF !SEEK(PADR(lcStyle,19)+lcWareCode)
    IF !SEEK(PADR(lcStyle,19)+lcWareCode+WHsLoc.CLOCATION)
    *B607472,1 ALB Defualt Bin for style/Color that are assigned to warehouse [END]
      APPEND BLANK
      GATHER MEMVAR 
    ENDIF
  ENDSCAN
*B607472,1 ALB (Begin)
ENDIF
*B607472,1 ALB (End)

DIMENSION laSource[1],laTarget[1]
STORE ' ' TO laSource,laTarget
lsSource = 1
SELECT cLocation FROM WHSLOC ;
  WHERE Style+Color+cWareCode == SPACE(19)+SPACE(6)+lcWareCode ;
  INTO ARRAY laSource

SELECT cLocation FROM (lcTemLoc) ;
  WHERE Style+cWareCode+cLocation=PADR(lcStyle,19)+lcWareCode;
  INTO ARRAY laTarget

=gfMover(@laSource,@laTarget,"Assign Bins",.T.,'lfvLoc')
SELECT (lcTemLoc)

DELETE FOR Style+cWareCode+cLocation = PADR(lcStyle,19)+lcWareCode

FOR I = 1 TO ALEN(laTarget)
   APPEND BLANK
   REPLACE STYLE     WITH lcStyle;
           CWARECODE WITH lcWareCode;
           CLOCATION WITH laTarget[i]
ENDFOR
*B607472,1 ALB No defualt Bin for style/Color [BEGIN]
llLoc = IIF(!EMPTY(laTarget) ,.T.,.F.)
SHOW GET llLoc
*B607472,1 ALB No defualt Bin for style/Color [END]

RETURN

*!*************************************************************
*! Name      : lfvLoc
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose   : Validate locations in mover when select.
*!*************************************************************
FUNCTION lfvLoc
PARAMETERS lnOption

DO CASE 
  CASE lnOption=1
    lcLocatin=laSource[lsSource]
    IF !SEEK(PADR(lcStyle,19)+SPACE(6)+lcWareCode+lcLocatin,"WhsLoc")
      *--Bin XXX is not assigned to '+'style XXX in location XXX,'\!\<Assign;\<Cancel
      IF gfModalGen('TRM42064B42007','DIALOG',ALLTRIM(lcLocatin)+'|'+ALLTRIM(lcStyle)+'|'+ALLTRIM(lcWareCode)) = 1
        RETURN .T.
      ELSE
        RETURN .F. 
      ENDIF
    ELSE
      RETURN .T.
    ENDIF
  CASE lnOption=2
    FOR I=1 TO ALEN(laSource,1)
      lcLocatin=laSource[I]
      IF !SEEK(PADR(lcStyle,19)+SPACE(6)+lcWareCode+lcLocatin,"WhsLoc")
        *--One or more Bin(s) are not assigned to '+'style XXX in location XXX,'\!\<Assign;\<Cancel
        IF gfModalGen('TRM42092B42007','DIALOG',ALLTRIM(lcStyle)+'|'+ALLTRIM(lcWareCode)) = 1
          RETURN .T.
        ELSE
          RETURN .F. 
        ENDIF
      ENDIF
    ENDFOR
  CASE (lnOption=3 OR lnOption=4)
ENDCASE
RETURN 


*!*************************************************************
*! Name    : lfvRefer
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Valid reference.
*!*************************************************************
FUNCTION lfvRefer

IF lcRefer = lcOldValue
  RETURN
ENDIF  
SELECT (lcTmpLine)
lnRecNo=RECNO()

*E300935,4 Change it because Po can have dyelot and different locations.
*lcKey=cCarton+IIF(llMFCall,Cuttkt,Po)+Style+IIF(llMFCall,'',STR(LineNo,6))
lcKey = cCarton+IIF(llMFCall,Cuttkt,Po)+Style+Dyelot+PADR(cWareCode,6)+STR(lineNo,6)

SET ORDER TO TAG TmpLine2
SEEK lcKey

*E300935,4 Change it because Po can have dyelot and different locations.
*REPLACE REST Reference WITH lcRefer ;
*       WHILE cCarton+IIF(llMFCall,Cuttkt,Po)+Style+IIF(llMFCall,'',STR(LineNo,6))=lcKey
REPLACE REST Reference WITH lcRefer ;
       WHILE cCarton+IIF(llMFCall,Cuttkt,Po)+Style+Dyelot+PADR(cWareCode,6)+STR(lineNo,6)=lcKey

SET ORDER TO TAG TmpLine1

*E300935,4 Check of phisical record before go to it.
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF
SHOW WINDOW (lcBrDtTtl) REFRESH
RETURN

*!*************************************************************
*! Name    : lfvRate
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Valid Rate.
*!*************************************************************
FUNCTION lfvRate

IF (_CUROBJ=OBJNUM(lnRate1) AND lnRate1 <=0 ) OR ;
   (_CUROBJ=OBJNUM(lnRate2) AND lnRate2 <=0 )
  *--Cannot accept zero or -ve rates.
  = gfModalGen('TRM34081B42000','DIALOG')
  lnRate1=lcOldValue
  RETURN
ENDIF

SELECT (lcTmpLine)
*E300935,4 Change it because Po can have dyelot and different locations.
*lcKey=cCarton+PO+Style+STR(LineNo,6)
lcKey = cCarton+Po+Style+Dyelot+PADR(cWareCode,6)+STR(lineNo,6)

lnRecNo=RECNO()
IF _CUROBJ=OBJNUM(lnRate1) AND lnRate1<>lcOldValue
  SET ORDER TO TAG TmpLine2
  SEEK lcKey
  
  *E300935,4 Change it because Po can have dyelot and different locations.
  *SCAN REST WHILE cCarton+PO+Style+STR(LineNo,6)=lcKey
  SCAN REST WHILE cCarton+Po+Style+Dyelot+PADR(cWareCode,6)+STR(lineNo,6) = lcKey
    =lfGetEqv('1',lnRate1,0,lnCurrUnt1,0,nLan_Cst1,0,0,0,0)
    REPLACE nLanPrRat  WITH lnRate1,;
            nELanCost1 WITH laECost[1]
  ENDSCAN
  SET ORDER TO TAG TmpLine1
ENDIF
IF _CUROBJ=OBJNUM(lnRate2) AND lnRate2<>lcOldValue
  SET ORDER TO TAG TmpLine2
  SEEK lcKey
  *E300935,4 Change it because Po can have dyelot and different locations.
  *SCAN REST WHILE cCarton+PO+Style+STR(LineNo,6)=lcKey
  SCAN REST WHILE cCarton+Po+Style+Dyelot+PADR(cWareCode,6)+STR(lineNo,6)=lcKey
    =lfGetEqv('2345',0,lnRate2,0,lnCurrUnt2,0,nLan_Cst2,nLan_Cst3,nLan_Cst4,nLan_Cst5)
    REPLACE nLanDuRat WITH lnRate2
    GATHER FROM laECost FIELDS nELanCost2,nELanCost3,nELanCost4,nELanCost5
  ENDSCAN
  SET ORDER TO TAG TmpLine1
ENDIF

*E300935,4 Check of phisical record before go to it.
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF  

RETURN


*!*************************************************************
*! Name    : lfvEditQty
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Edit line quantity.
*!*************************************************************
FUNCTION lfvEditQty
PRIVATE lcCuttlOrd , lcCuttlPos , lnCurAlias

*C102172,4 AMH Check if coming for reciving by lot [Start]
IF ASCAN(laEvntTrig,PADR("ADOPTCAT",10)) <> 0 .AND. WEXIST('MFRECLOT')
  lcTmpLine = lcTktSheet
  ON KEY LABEL ESC     &lcHldEsc
ENDIF  
*C102172,4 AMH [End]

IF llWareHous AND EMPTY(lcWareCode)
  *-- Message : 'First, you must select location.'
  =gfModalGen('TRM42150B42001','DIALOG')
  _CUROBJ = OBJNUM(lnWare)
  RETURN
ENDIF


SELECT (lcTmpLine)
SCATTER MEMVAR
*E300935,4 Change key due to change to index expression.
*lcLKey=cCarton+IIF(llMFCall,Cuttkt,Po)+Style+IIF(llMFCall,IIF(llDyelot,Dyelot,''),STR(LineNo,6))
lcLKey=cCarton+IIF(llMFCall,Cuttkt,Po)+Style+Dyelot+PADR(cWareCode,6)+IIF(llMFCall,'',STR(LineNo,6))

DIME laOrg[8],laSok[8],laDam1[8],laDam2[8],laCan[8],laBal[8],laAlo[8],laBud[8],laZero[9]

STORE 0 TO laOrg,laSok,laDam1,laDam2,laCan,laBal,laAlo,laZero,laBud

*E300935,4 declare arrays to hold old output values to update matched lines.
DIMENSION laOldOut[8]
STORE 0 TO laOldOut
DIMENSION laConst[8]    && Original qty.
STORE 0 TO laConst
DIMENSION laRemain[9]   &&-- which carry the last receive qty
STORE 0 TO laRemain

*--New Origenal and order quantity.
=SEEK('1'+lcLKey)
SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laOrg

SCATTER FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8 TO laAlo

*--Initialize the landed cost.
IF llEditLCst
  lnCostVal1=nLan_Cst1
  lnCostVal2=nLan_Cst2
  lnCostVal3=nLan_Cst3
  lnCostVal4=nLan_Cst4
  lnCostVal5=nLan_Cst5
  lnEqCost1 =nELanCost1 
  lnEqCost2 =nELanCost2 
  lnEqCost3 =nELanCost3 
  lnEqCost4 =nELanCost4 
  lnEqCost5 =nELanCost5
ENDIF
IF llMFCall
  lcBdgLine = cuttkt+style+dyelot+trancd
  *--Budjet total ordered quantity.
  SELECT CUTTKTL
ELSE

  *C200170,1 AMH Case of issue inter-location P/O Batch [Start]
  *lcBdgLine = cstytype+po+style+STR(lineno,6)+trancd
  *--Budjet total ordered quantity.
  *SELECT POSLN
  IF lcPType = 'H'
    lcBdgLine = 'N'+lcBatch+po+style+dyelot
    *--Budjet total ordered quantity.
    SELECT CTKTRCVL
  ELSE
    lcBdgLine = cstytype+po+style+STR(lineno,6)+trancd
    *--Budjet total ordered quantity.
    SELECT POSLN
  ENDIF
  *C200170,1 AMH [End]
  
ENDIF
=SEEK(lcBdgLine)

SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laBud
=ACOPY(laBud,laConst)

*--Get last receiving for inter location Issue and receive.
*E301291,4 AMM Consider issue/receive PO
*IF lcPType $ 'ON'
*--B803628,1 RAMY consider the case of the shipment [Start]
*IF lcPType $ 'ONAE'

*E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
*IF lcPType $ 'ONAES'

*B604878,1 KHM 09/10/2001 (Begin) Comment the following lines and use the
*B604878,1                DO CASE command in order to handle all the cases
*B604878,1                of displaying the right figure of the previous receive.
*IF lcPType $ 'ONAESL'
*E301480,1 NAD (End)

*--B803628,1 RAMY [end]
*E301291,4 AMM end
*  lcToseekLn = cstytype+po+style+STR(lineno,6)
  
*  *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
*  *SCAN WHILE cstytype+po+style+STR(lineno,6)=lcToseekLn FOR Trancd $ '245'
*  SCAN WHILE cstytype+po+style+STR(lineno,6)=lcToseekLn FOR Trancd $ '2456'
*  *E301480,1 NAD (End) 
  
*    FOR lnI=1 to 8
*      lcI=STR(lnI,1)     
  
*      *B604502,1 MHM 06/11/2001 (Begin) Taking care of inter-location PO
*      *laRemain[lnI] = laRemain[lnI] + Qty&lcI
*      *B604646,1 MHM 07/17/2001  Add Issue inter-location PO to the condition [Start]
*      *laRemain[lnI] = laRemain[lnI] + IIF(lcPType $ 'O' AND TranCd = '6',0,Qty&lcI)
*      laRemain[lnI] = laRemain[lnI] + IIF(lcPType $ 'ON' AND TranCd = '6',0,Qty&lcI)
*      *B604646,1 MHM 07/17/2001  [End]
*      *B604502,1 MHM 06/11/2001 (End)

*    ENDFOR
*  ENDSCAN
*  laRemain[9] = laRemain[1]+laRemain[2]+laRemain[3]+laRemain[4]+laRemain[5]+laRemain[6]+laRemain[7]+laRemain[8]
*  =SEEK(lcBdgLine)
*ELSE  &&*--Get last receiving.
*  *B602698,1[BEGIN] Calculate the open Qty that appear in the screen .
*  FOR lnI=1 to 8
*    *B603275,1 AMM if receiving from a selected lot, calculate the remaining qty 
*    *B603275,1 AMM from the budget of that lot
*    *laRemain[lnI] = laConst[lnI] - laOrg[lnI]
*    IF llSpecLot
*      laRemain[lnI] = laLotArry[lnI] - laOrg[lnI]
*    ELSE
*      laRemain[lnI] = laConst[lnI] - laOrg[lnI]
*    ENDIF
*    *B603275,1 AMM end

*    laRemain[9] = laRemain[9] + laRemain[lnI] 
*  ENDFOR
*  *B602698,1[END..] Calculate the open Qty that appear in the screen .
*ENDIF

DO CASE
  
  *-- [O] Receive Inter-Location P/o, [N] Issue Inter-Location P/o
  *-- [A] Issue Adornment order, [E] Receive Adornment order
  *-- [S] Receive by Shipment, [L] Receive Inter Location P/O Batch  
  CASE lcPType $ 'ONAESL'
    lcToseekLn = cstytype+po+style+STR(lineno,6)
    SCAN WHILE cstytype+po+style+STR(lineno,6)=lcToseekLn FOR Trancd $ '2456'
      FOR lnI=1 to 8
        lcI=STR(lnI,1)     
        laRemain[lnI] = laRemain[lnI] + IIF(lcPType $ 'ON' AND TranCd = '6',0,Qty&lcI)
      ENDFOR
    ENDSCAN
    laRemain[9] = laRemain[1]+laRemain[2]+laRemain[3]+laRemain[4]+laRemain[5]+laRemain[6]+laRemain[7]+laRemain[8]
    =SEEK(lcBdgLine)
  
  *-- [I] Receive P/O  
  CASE lcPType = 'I'
    IF llSpecLot
      FOR lnI=1 to 8
        laRemain[lnI] = laLotArry[lnI] - laOrg[lnI]
        laRemain[9] = laRemain[9] + laRemain[lnI] 
      ENDFOR  
    ELSE
      lcToseekLn = cstytype+po+style+STR(lineno,6)
      SCAN WHILE cstytype+po+style+STR(lineno,6)=lcToseekLn FOR Trancd $ '245'
        FOR lnI=1 to 8
          lcI=STR(lnI,1)     
          laRemain[lnI] = laRemain[lnI] + IIF(lcPType $ 'ON' AND TranCd = '6',0,Qty&lcI)
        ENDFOR
      ENDSCAN
      laRemain[9] = laRemain[1]+laRemain[2]+laRemain[3]+laRemain[4]+laRemain[5]+laRemain[6]+laRemain[7]+laRemain[8]
      =SEEK(lcBdgLine)      
    ENDIF
    
  *-- [M] Receive C/T  
  CASE lcPType = 'M'
    IF llSpecLot
      FOR lnI=1 to 8
        laRemain[lnI] = laLotArry[lnI] - laOrg[lnI]
        laRemain[9] = laRemain[9] + laRemain[lnI] 
      ENDFOR  
    ELSE
      lcToseekLn = CutTkt+Style+Dyelot
      *T20061003.0005 TMI [Start] add the filter on line
      *SCAN REST WHILE CutTkt+Style+Dyelot+TranCd = lcToseekLn FOR TranCd $ '234'
      SCAN REST WHILE CutTkt+Style+Dyelot+TranCd = lcToseekLn FOR TranCd $ '234' AND LINENO = M.LINENO
        *T20061003.0005 TMI [End  ] 
        FOR lnI=1 to 8
          lcI=STR(lnI,1)     
          laRemain[lnI] = laRemain[lnI] + IIF(lcPType $ 'ON' AND TranCd = '6',0,Qty&lcI)
        ENDFOR
      ENDSCAN
      laRemain[9] = laRemain[1]+laRemain[2]+laRemain[3]+laRemain[4]+laRemain[5]+laRemain[6]+laRemain[7]+laRemain[8]
      =SEEK(lcBdgLine)      
    ENDIF
  
    *C128583,6  TMI [Start] 
  *-- [B] Batch Receiving
  CASE lcPType = 'B'
      lcToseekLn = cstytype+po+style+STR(lineno,6)           
      =ACOPY(laBud,laOrg)
      SCAN REST WHILE CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD = lcToseekLn 
        IF TranCd $ '2'
          FOR lnI=1 to 8
            lcI=STR(lnI,1)     
            laRemain[lnI] = laRemain[lnI] + Qty&lcI
          ENDFOR
        ENDIF
        IF TranCd $ '2456'
          FOR lnI=1 to 8
            lcI=STR(lnI,1)     
            laOrg[lnI] = laOrg[lnI] - Qty&lcI
          ENDFOR
        ENDIF
      ENDSCAN
      laRemain[9] = laRemain[1]+laRemain[2]+laRemain[3]+laRemain[4]+laRemain[5]+laRemain[6]+laRemain[7]+laRemain[8]
      =SEEK(lcBdgLine)      
  *C128583,6  TMI [End  ] 
  OTHERWISE
    FOR lnI=1 to 8
      IF llSpecLot
        laRemain[lnI] = laLotArry[lnI] - laOrg[lnI]
      ELSE
        laRemain[lnI] = laConst[lnI] - laOrg[lnI]
      ENDIF
      laRemain[9] = laRemain[9] + laRemain[lnI] 
    ENDFOR    
ENDCASE
*B604878,1 KHM 09/10/2001 (End)



SELECT (lcTmpLine)
=SEEK(Style,'STYLE')
=SEEK('S'+STYLE.Scale,'SCALE')
lcSz1=SCALE.Sz1
lcSz2=SCALE.Sz2
lcSz3=SCALE.Sz3
lcSz4=SCALE.Sz4
lcSz5=SCALE.Sz5
lcSz6=SCALE.Sz6
lcSz7=SCALE.Sz7
lcSz8=SCALE.Sz8

STORE '' TO lcRetSty1,lcRetSty2,lcRetSHd,lcRetSHd1,lcRetSHd2,lcRetSHd3,lcRetSHd4
lcMStyQlty=STYLE.cStyGrade
lcSndGrd = "2"
lcTrdGrd = "3"
DO CASE
  CASE lcMStyQlty='1'
    lcRetSHd ="1st Quality"
    *C102358,4 AMH Display 2nd & 3rd label in case of JL [Start]
    *IF lcPType<>'A'
    IF lcPType<>'A' .OR. (ASCAN(laEvntTrig,PADR("RCVADORD",10)) <> 0)
    *C102358,4 AMH [End]
      lcRetSHd1="2nd Quality"
      lcRetSHd2="Damaged"
      lcRetSHd3="Second Quality"
      lcRetSHd4="Damaged"
    ENDIF
  CASE lcMStyQlty='2'
    lcRetSHd ="2nd Quality"
    IF lcPType<>'A'
      lcRetSHd1="1st Quality"
      lcRetSHd2="Damaged"
      lcRetSHd3="First Quality"
      lcRetSHd4="Damaged"
    ENDIF
    lcSndGrd = "1"
    lcTrdGrd = "3"
  CASE lcMStyQlty='3'
    lcRetSHd ="Damaged"
    IF lcPType<>'A'
      lcRetSHd1="1st Quality"
      lcRetSHd2="2nd Quality"    
      lcRetSHd3="First Quality"
      lcRetSHd4="Second Quality"
    ENDIF
    lcSndGrd = "1"
    lcTrdGrd = "2"
ENDCASE

lnOldStk=TotStk
lnOldDam=TotDam
lnOldCan=TotCan

*--1) Get Stock quantity.

IF SEEK('2'+lcLKey)
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laSok
  *--Initialize the landed cost.
  IF llEditLCst
    lnCostVal1=nLan_Cst1
    lnCostVal2=nLan_Cst2
    lnCostVal3=nLan_Cst3
    lnCostVal4=nLan_Cst4
    lnCostVal5=nLan_Cst5
    *B803693,1 HBG 10/18/2000 Get the Equivalent landed cost from the temp file [Begin]
    *lnEqCost1 =nECost1 
    *lnEqCost2 =nECost2 
    *lnEqCost3 =nECost3 
    *lnEqCost4 =nECost4 
    *lnEqCost5 =nECost5 
    lnEqCost1 =nELanCost1 
    lnEqCost2 =nELanCost2 
    lnEqCost3 =nELanCost3 
    lnEqCost4 =nELanCost4 
    lnEqCost5 =nELanCost5 
    *B803693,1 [End]     
  ENDIF
ENDIF


*--2) Get Damage quantity.
STORE 'DISABLE' TO lcRSt1Stat,lcRSt2Stat
IF SEEK(lcOthrTrCd+lcLKey)
  *E300935,4 Change scan expr. due to change to index, also scan rest is faster 
  *E300935,4 than scan only.
  *SCAN WHILE TranCd+cCarton+IIF(llMFCall,Cuttkt,Po)+Style+IIF(llMFCall,IIF(llDyelot,Dyelot,''),STR(LineNo,6))=lcOthrTrCd+lcLKey
  SCAN REST WHILE TranCd+cCarton+IIF(llMFCall,Cuttkt,Po)+Style+Dyelot+;
                  PADR(cWareCode,6)+IIF(llMFCall,'',STR(LineNo,6))=lcOthrTrCd+lcLKey
    IF cStyGrade = lcSndGrd
      SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laDam1
      lcRetSty1=cRetSty
      lcRSt1Stat='ENABLE'
    ELSE
      SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laDam2
      lcRetSty2=cRetSty
      lcRSt2Stat='ENABLE'
    ENDIF
  ENDSCAN
ENDIF

*--3) Get Cancel quantity.
IF SEEK(lcCanlTrCd+lcLKey)
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laCan
ENDIF

FOR I=1 TO 8
  *E300935,4 Adjust get balance
  *laBal[I]=IIF( laOrg[I]-(laSok[I]+laDam1[I]+laDam2[I]+laCan[I])<0, 0,;
  *              laOrg[I]-(laSok[I]+laDam1[I]+laDam2[I]+laCan[I]) ) 
  *B602698,1[BEGIN] Do it in case of C/T
  *laBal[I]=IIF(!(lcPType $ 'IS') .AND. laOrg[I]-(laSok[I]+laDam1[I]+laDam2[I]+laCan[I])<0, 0,;
                laOrg[I]-(laSok[I]+laDam1[I]+laDam2[I]+laCan[I]) ) 
  *C200080,1 AMM Add the dye order type 'D'
  *laBal[I]=IIF(!(lcPType $ 'ISM') .AND. laOrg[I]-(laSok[I]+laDam1[I]+laDam2[I]+laCan[I])<0, 0,;
                laOrg[I]-(laSok[I]+laDam1[I]+laDam2[I]+laCan[I]) )  
  *B603775,1 (Begin) When browsing the qty screen without going through any field validation,
  *B603775,1         laBal[]s for over receiving fields are Negative causing TOTBAL to show wrong value.
  *laBal[I]=IIF(!(lcPType $ 'ISMD') .AND. laOrg[I]-(laSok[I]+laDam1[I]+laDam2[I]+laCan[I])<0, 0,;
                laOrg[I]-(laSok[I]+laDam1[I]+laDam2[I]+laCan[I]) )  
  laBal[I]=MAX(laOrg[I]-(laSok[I]+laDam1[I]+laDam2[I]+laCan[I]),0)
  *B603775,1 (End)
  *C200080,1 AMM end
  *B602698,1[END..]
ENDFOR

lcSkMode = 'ENABLE'
lcOtMode = 'ENABLE'
*--Cannot update quantity case of batch (view only).
*E301223,1 Added to disable entering qty if void received P/o Case.
*IF lcPType $ 'BT' OR (llPOSale AND lcPType = 'O')
*E301291,4 AMM Consider the case of receive adornment PO
*IF lcPType $ 'BT' OR (llPOSale AND lcPType = 'O') OR ;
   (lcPType = 'R' AND !EMPTY(POSHDR.CPONO))

*E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
*IF lcPType $ 'BT' OR (llPOSale AND lcPType $ 'OE') OR ;
   (lcPType = 'R' AND !EMPTY(POSHDR.CPONO))
   
*C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
*IF lcPType $ 'BTL' OR (llPOSale AND lcPType $ 'OE') OR ;
   (lcPType = 'R' AND !EMPTY(POSHDR.CPONO))
IF lcPType $ 'BTLH' OR (llPOSale AND lcPType $ 'OE') OR ;
   (lcPType = 'R' AND !EMPTY(POSHDR.CPONO))
*C200170,1 AMH [End]

*E301480,1 NAD (End)

*E301291,4 AMM end
*E301223,1 End.
  lcSkMode = 'DISABLE'
  lcOtMode = 'DISABLE'
  lcRSt1Stat = 'DISABLE'
  lcRSt2Stat = 'DISABLE'
ENDIF
*--Cannot edit the other and cancel quantity case of issue inter Location P/o.
*E301291,4 AMM Consider case of issue adornment PO
*IF lcPType = 'N'

*B604646,1 MHM Cannot edit the other and cancel quantity case of Receive inter Location P/o[START]
IF lcPType $ 'NA'
*IF lcPType $ 'NAO'
*B604646,1 MHM [END].

*E301291,4 AMM end
  lcOtMode = 'DISABLE'
  lcRSt1Stat = 'DISABLE'
  lcRSt2Stat = 'DISABLE'
ENDIF

=SEEK('1'+lcLKey)
lcHldTab = ON('KEY','TAB')
lcHldBtb = ON('KEY','BACKTAB')    
lcHldEnt = ON('KEY','ENTER')
ON KEY LABEL TAB    
ON KEY LABEL BACKTAB
ON KEY LABEL ENTER

*E300935,4 calculate old values of Out quantities from this line.
FOR lnI = 1 TO 8
  laOldOut[lnI] = laSok[lnI]+laDam1[lnI]+laDam2[lnI]+laCan[lnI]
ENDFOR
*E300935,4
*B603275,1 AMM Set the screen title of the quantity screen
lcscrttl = 'Edit Line Quantity'
*B603275,1 AMM end
*--Call line quantity screen.

*B039660,1 NNA 06/06/2006 (Begin) convert next trigger to be done within Binmain.Prg instead of Davmain.prg 
*C037816,1 MHM 04/06/2004 Custom Screen for Bin Location For David Luke[Start]
*DO (gcScrDir+"MFRCVQ.SPX")
IF ASCAN(laEvntTrig,PADR("DLRCVQTY",10)) <> 0 
  =gfDoTriger("POSTREC",PADR("DLRCVQTY",10))
ELSE
  DO (gcScrDir+"MFRCVQ.SPX")
ENDIF
*C037816,1 MHM [End]
*B039660,1 NNA (End)

ON KEY LABEL TAB     &lcHldTab
ON KEY LABEL BACKTAB &lcHldBtb
ON KEY LABEL ENTER   &lcHldEnt

IF lcPType $ 'BT'
  =SEEK('1'+lcLKey)
  RETURN
ENDIF

*--Get Totals.
lnTStk =laSok[1]+laSok[2]+laSok[3]+laSok[4]+laSok[5]+laSok[6]+laSok[7]+laSok[8]
lnTCan =laCan[1]+laCan[2]+laCan[3]+laCan[4]+laCan[5]+laCan[6]+laCan[7]+laCan[8]
lnTDam1=laDam1[1]+laDam1[2]+laDam1[3]+laDam1[4]+laDam1[5]+laDam1[6]+laDam1[7]+laDam1[8]
lnTDam2=laDam2[1]+laDam2[2]+laDam2[3]+laDam2[4]+laDam2[5]+laDam2[6]+laDam2[7]+laDam2[8]
lnTDam =lnTDam1+lnTDam2

*--Edit Landed cost.
IF llEditLCst AND ( lnTStk+lnTDam+lnTCan <> 0 )
  *B803693,1 HBG 10/18/2000 Fix bug of variabel lnEqCost1 not found [Begin]
  lnCostVal1=IIF(TYPE('lnCostVal1') $ 'UL',&lcTmpLine..nLan_Cst1,lnCostVal1)
  lnCostVal2=IIF(TYPE('lnCostVal2') $ 'UL',&lcTmpLine..nLan_Cst2,lnCostVal2)
  lnCostVal3=IIF(TYPE('lnCostVal3') $ 'UL',&lcTmpLine..nLan_Cst3,lnCostVal3)
  lnCostVal4=IIF(TYPE('lnCostVal4') $ 'UL',&lcTmpLine..nLan_Cst4,lnCostVal4)
  lnCostVal5=IIF(TYPE('lnCostVal5') $ 'UL',&lcTmpLine..nLan_Cst5,lnCostVal5)
  lnEqCost1 =IIF(TYPE('lnEqCost1') $ 'UL',&lcTmpLine..nELanCost1,lnEqCost1)
  lnEqCost2 =IIF(TYPE('lnEqCost2') $ 'UL',&lcTmpLine..nELanCost2,lnEqCost2)
  lnEqCost3 =IIF(TYPE('lnEqCost3') $ 'UL',&lcTmpLine..nELanCost3,lnEqCost3)
  lnEqCost4 =IIF(TYPE('lnEqCost4') $ 'UL',&lcTmpLine..nELanCost4,lnEqCost4)
  lnEqCost5 =IIF(TYPE('lnEqCost5') $ 'UL',&lcTmpLine..nELanCost5,lnEqCost5)
  *B803693,1 [END]
  =lfvLCost()
ENDIF

*--1) Update stock quantity.
IF SEEK('2'+lcLKey)
  IF lnTStk<>0
    GATHER FROM laSok FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    REPLACE TotQty WITH lnTStk
    IF llEditLCst
      REPLACE nLan_Cst1 WITH lnCostVal1,nLan_Cst2 WITH lnCostVal2,nLan_Cst3 WITH lnCostVal3,;
              nLan_Cst4 WITH lnCostVal4,nLan_Cst5 WITH lnCostVal5,nELanCost1 WITH lnEqCost1,;
              nELanCost2 WITH lnEqCost2,nELanCost3 WITH lnEqCost3,nELanCost4 WITH lnEqCost4,nELanCost5 WITH lnEqCost5
    ENDIF
  ELSE
    BLANK
    DELETE
  ENDIF
ELSE
  IF lnTStk<>0
    APPEND BLANK
    GATHER MEMVAR
    REPLACE Trancd WITH '2',;
            cStyGrade WITH lcMStyQlty
    GATHER FROM laSok  FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 
    GATHER FROM laZero FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
    REPLACE TotQty WITH lnTStk
    IF llEditLCst
      REPLACE nLan_Cst1 WITH lnCostVal1,nLan_Cst2 WITH lnCostVal2,nLan_Cst3 WITH lnCostVal3,;
              nLan_Cst4 WITH lnCostVal4,nLan_Cst5 WITH lnCostVal5,nELanCost1 WITH lnEqCost1,;
              nELanCost2 WITH lnEqCost2,nELanCost3 WITH lnEqCost3,nELanCost4 WITH lnEqCost4,nELanCost5 WITH lnEqCost5
    ENDIF
  ENDIF
ENDIF


*--2) Update Damaged quantity.
=SEEK(lcOthrTrCd+lcLKey)

*E300935,4 Change locate expression.
*LOCATE REST WHILE Trancd+cCarton+IIF(llMFCall,Cuttkt,Po)+Style+IIF(llMFCall,IIF(llDyelot,Dyelot,''),STR(LineNo,6))=lcOthrTrCd+lcLKey ;
              FOR cStyGrade=lcSndGrd
LOCATE REST WHILE Trancd+cCarton+IIF(llMFCall,Cuttkt,Po)+Style+Dyelot+PADR(cWareCode,6)+IIF(llMFCall,'',STR(LineNo,6))=lcOthrTrCd+lcLKey ;
              FOR cStyGrade=lcSndGrd

IF FOUND()
  IF lnTDam1=0
    BLANK
    DELETE
  ELSE
    GATHER FROM laDam1 FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    REPLACE TotQty  WITH lnTDam1,;
            cRetSty WITH lcRetSty1
    IF llEditLCst
      REPLACE nLan_Cst1 WITH lnCostVal1,nLan_Cst2 WITH lnCostVal2,nLan_Cst3 WITH lnCostVal3,;
              nLan_Cst4 WITH lnCostVal4,nLan_Cst5 WITH lnCostVal5,nECost1 WITH lnEqCost1,;
              nECost2 WITH lnEqCost2,nECost3 WITH lnEqCost3,nECost4 WITH lnEqCost4,nECost5 WITH lnEqCost5
    ENDIF
  ENDIF
ELSE
  IF lnTDam1<>0
    APPEND BLANK
    GATHER MEMVAR
    REPLACE Trancd    WITH lcOthrTrCd,;
            cStyGrade WITH lcSndGrd,;
            cRetSty   WITH lcRetSty1,;
            TotQty    WITH lnTDam1
    GATHER FROM laDam1 FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    GATHER FROM laZero FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
    IF llEditLCst
      REPLACE nLan_Cst1 WITH lnCostVal1,nLan_Cst2 WITH lnCostVal2,nLan_Cst3 WITH lnCostVal3,;
              nLan_Cst4 WITH lnCostVal4,nLan_Cst5 WITH lnCostVal5,nECost1 WITH lnEqCost1,;
              nECost2 WITH lnEqCost2,nECost3 WITH lnEqCost3,nECost4 WITH lnEqCost4,nECost5 WITH lnEqCost5
    ENDIF
  ENDIF
ENDIF

=SEEK(lcOthrTrCd+lcLKey)
*E300935,4 Change locate expression.
*LOCATE REST WHILE Trancd+cCarton+IIF(llMFCall,Cuttkt,Po)+Style+IIF(llMFCall,IIF(llDyelot,Dyelot,''),STR(LineNo,6))=lcOthrTrCd+lcLKey ;
              FOR cStyGrade=lcTrdGrd
LOCATE REST WHILE Trancd+cCarton+IIF(llMFCall,Cuttkt,Po)+Style+Dyelot+PADR(cWareCode,6)+IIF(llMFCall,'',STR(LineNo,6))=lcOthrTrCd+lcLKey ;
              FOR cStyGrade=lcTrdGrd

IF FOUND()
  IF lnTDam2=0
    BLANK
    DELETE
  ELSE
    GATHER FROM laDam2 FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    REPLACE TotQty  WITH lnTDam2,;
            cRetSty WITH lcRetSty2
    IF llEditLCst
      REPLACE nLan_Cst1 WITH lnCostVal1,nLan_Cst2 WITH lnCostVal2,nLan_Cst3 WITH lnCostVal3,;
              nLan_Cst4 WITH lnCostVal4,nLan_Cst5 WITH lnCostVal5,nECost1 WITH lnEqCost1,;
              nECost2 WITH lnEqCost2,nECost3 WITH lnEqCost3,nECost4 WITH lnEqCost4,nECost5 WITH lnEqCost5
    ENDIF
  ENDIF
ELSE
  IF lnTDam2<>0
    APPEND BLANK
    GATHER MEMVAR
    REPLACE Trancd    WITH lcOthrTrCd,;
            cStyGrade WITH lcTrdGrd,;
            cRetSty   WITH lcRetSty2,;
            TotQty    WITH lnTDam2
    GATHER FROM laDam2 FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    GATHER FROM laZero FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
    IF llEditLCst
      REPLACE nLan_Cst1 WITH lnCostVal1,nLan_Cst2 WITH lnCostVal2,nLan_Cst3 WITH lnCostVal3,;
              nLan_Cst4 WITH lnCostVal4,nLan_Cst5 WITH lnCostVal5,nECost1 WITH lnEqCost1,;
              nECost2 WITH lnEqCost2,nECost3 WITH lnEqCost3,nECost4 WITH lnEqCost4,nECost5 WITH lnEqCost5
    ENDIF
  ENDIF
ENDIF


*--3) Update cancel quantity.
IF SEEK(lcCanlTrCd+lcLKey)
  IF lnTCan<>0
    GATHER FROM laCan FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    REPLACE TotQty WITH lnTCan
    IF llEditLCst
      REPLACE nLan_Cst1 WITH lnCostVal1,nLan_Cst2 WITH lnCostVal2,nLan_Cst3 WITH lnCostVal3,;
              nLan_Cst4 WITH lnCostVal4,nLan_Cst5 WITH lnCostVal5,nECost1 WITH lnEqCost1,;
              nECost2 WITH lnEqCost2,nECost3 WITH lnEqCost3,nECost4 WITH lnEqCost4,nECost5 WITH lnEqCost5
    ENDIF
  ELSE
    BLANK 
    DELETE
  ENDIF
ELSE
  IF lnTCan<>0
    APPEND BLANK
    GATHER MEMVAR
    REPLACE Trancd WITH lcCanlTrCd,;
            TotQty WITH lnTCan
    GATHER FROM laCan  FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    GATHER FROM laZero FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
    IF llEditLCst
      REPLACE nLan_Cst1 WITH lnCostVal1,nLan_Cst2 WITH lnCostVal2,nLan_Cst3 WITH lnCostVal3,;
              nLan_Cst4 WITH lnCostVal4,nLan_Cst5 WITH lnCostVal5,nECost1 WITH lnEqCost1,;
              nECost2 WITH lnEqCost2,nECost3 WITH lnEqCost3,nECost4 WITH lnEqCost4,nECost5 WITH lnEqCost5
    ENDIF
  ENDIF
ENDIF


SELECT (lcTmpLine)
IF SEEK('1'+lcLKey)
  REPLACE TotStk WITH lnTStk,;
          TotDam WITH lnTDam,;
          TotCan WITH lnTCan,;
          TotBal WITH laBal[1]+laBal[2]+laBal[3]+laBal[4]+laBal[5]+laBal[6]+laBal[7]+laBal[8]
ENDIF

lnTotStk=lnTotStk+(TotStk-lnOldStk)
lnTotDam=lnTotDam+(TotDam-lnOldDam)
lnTotCan=lnTotCan+(TotCan-lnOldCan)

*E300935,4 Evaluate total out quantity and update another lines. [Begin]
*-- Calculate Out Quantity in this Line. [BEGIN]
*IF !EMPTY(lcDyelot) AND (lcPType $ 'ISM')
*B602698,1[BEGIN] Do it in case of C/T
*IF (lcPType $ 'IS')
*C200080,1 AMM Add the dye order type 'D'
*IF (lcPType $ 'ISM')
IF (lcPType $ 'ISMD')
*C200080,1 AMM end
*B602698,1[END..]
  DIMENSION laOut[8]
  STORE 0 TO lnTotOut,laOut
  FOR lnI = 1 TO 8
    Z = STR(lnI,1)
    laOut[lnI] = laSok[lnI]+laDam1[lnI]+laDam2[lnI]+laCan[lnI]
    lnTotOut   = lnTotOut + laOut[lnI]
  ENDFOR
  *-- Calculate Out Quantity in this Line. [BEGIN]
   
  *lcCurLine =  cCarton+Po+Style+STR(LineNo,6)
  lcCurLine =  cCarton+IIF(llMFCall,Cuttkt,Po)+Style+STR(LineNo,6)
  *-- Subtract out quantity from the same lines with another dyelots.
  

  *SCAN FOR Trancd+cCarton+Po+Style+STR(LineNo,6) = '1' + lcCurLine AND 
  lcScanVar = [Trancd+cCarton+IIF(llMFCall,Cuttkt,Po)+Style+STR(LineNo,6)]
  SCAN FOR EVALUATE(lcScanVar) = '1' + lcCurLine AND ;
           DYELOT+cWareCode # lcDyelot+lcWareCode
    FOR lnI = 1 TO 8
        lcZ = STR(lnI,1)  
        *E300935,4 Now MAX Function is on the screen.
        REPLACE QTY&lcZ WITH QTY&lcZ + laOldOut[lnI] - laOut[lnI] ,;
                TOTQTY  WITH TOTQTY  + laOldOut[lnI] - laOut[lnI]
    ENDFOR 
    REPLACE TOTBAL WITH TOTQTY-TOTSTK-TOTDAM-TOTCAN
  ENDSCAN
  =SEEK('1'+lcLKey)
ENDIF
*E300935,4 Evaluate total out quantity and update another lines. [End]

*C102172,4 AMH Check if coming for reciving by lot [Start]
IF ASCAN(laEvntTrig,PADR("ADOPTCAT",10)) <> 0 .AND. WEXIST('MFRECLOT')
  lcTmpLine = lcTktShee1
  ON KEY LABEL ESC DO lfvReturn
ENDIF  
*C102172,4 AMH [End]

*T20060809.0006   TMI [Start] Update bin fields for 2nd/damaged quality
IF ASCAN(laEvntTrig,'UPBNLOC') <> 0
  =gfDoTriger('POSTREC','UPBNLOC')
ENDIF
*T20060809.0006   TMI [End  ] 

=lfRefresh(lcWinCh0)
=lfCrUnSess('','',.T.)   
SHOW WINDOW (lcBrDtTtl) REFRESH
RETURN


*:*************************************************************
*! Name    : lfvQty
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Quantity valid. 
*:*************************************************************
FUNCTION lfvQty
PARA lnCnxt

*B603237,1 Start, Case of Adorn Order ,dont allow to issue/receive more that existing qty.
*B604646,1 MHM 07/17/2001  Add inter-location PO to the condition [Start]
*IF lcPType $ 'AE' AND laSok[lnCnxt] > laOrg[lnCnxt]
*  *-You cannot increase quantity more that original picked quantity!
*  =gfModalGen('INM38189B00000','DIALOG')
*  lcCnxt = STR(lnCnxt,1)
*  laSok[lnCnxt] = &lcTmpLine..Qty&lcCnxt
*  SHOW GET laSok[lnCnxt]
* _CUROBJ=_CUROBJ
*  RETURN
*ENDIF
IF lcPType $ 'O' AND (laSok[lnCnxt]+laDam1[lnCnxt]+laDam2[lnCnxt]+laCan[lnCnxt]) > laOrg[lnCnxt] 
    =gfModalGen('INM00000B00000','','','','You can not exceed the issued quantity')
    lcCnxt = STR(lnCnxt,1)
    *--mhm2001
    *laSok[lnCnxt] = &lcTmpLine..Qty&lcCnxt
    *SHOW GET laSok[lnCnxt]
    IF _CUROBJ = OBJNUM(laSok[lnCnxt])
      laSok[lnCnxt] = &lcTmpLine..Qty&lcCnxt
      SHOW GET laSok[lnCnxt]
    ENDIF
      
    IF _CUROBJ = OBJNUM(laDam1[lnCnxt])
      laDam1[lnCnxt] = 0
      SHOW GET laDam1[lnCnxt]
    ENDIF

    IF _CUROBJ = OBJNUM(laDam2[lnCnxt])
      laDam2[lnCnxt] =0
      SHOW GET laDam2[lnCnxt]
    ENDIF

    IF _CUROBJ = OBJNUM(laCan[lnCnxt])
      laCan[lnCnxt] = 0
      SHOW GET laCan[lnCnxt]
    ENDIF
    *--mhm2001
   _CUROBJ=_CUROBJ
    RETURN
ELSE
  IF lcPType $ 'AE' AND laSok[lnCnxt] > laOrg[lnCnxt]
    *-You cannot increase quantity more that original picked quantity!
    =gfModalGen('INM38189B00000','DIALOG')
    lcCnxt = STR(lnCnxt,1)
    laSok[lnCnxt] = &lcTmpLine..Qty&lcCnxt
    SHOW GET laSok[lnCnxt]
   _CUROBJ=_CUROBJ
    RETURN
  ENDIF
ENDIF

*B604646,1 MHM 07/17/2001   [End]
*B603237,1 End.

lnNewBal=laOrg[lnCnxt]-(laSok[lnCnxt]+laDam1[lnCnxt]+laDam2[lnCnxt]+laCan[lnCnxt])
IF _CUROBJ<>OBJNUM(laCan[lnCnxt]) AND laCan[lnCnxt] > 0 AND lnNewBal<0
  *--Cancel quantity more that new balance, Therefore it will be redused.
  =gfModalGen('INM34068B42000','DIALOG')
  laBal[lnCnxt]=0
  laCan[lnCnxt]=laOrg[lnCnxt]-(laSok[lnCnxt]+laDam1[lnCnxt]+laDam2[lnCnxt])
  laCan[lnCnxt]=IIF(laCan[lnCnxt]<0,0,laCan[lnCnxt])
  SHOW GET laCan[lnCnxt]
ELSE
  laBal[lnCnxt]=laOrg[lnCnxt]-(laSok[lnCnxt]+laDam1[lnCnxt]+laDam2[lnCnxt]+laCan[lnCnxt])
  laBal[lnCnxt]=IIF(laBal[lnCnxt]<0,0,laBal[lnCnxt])
ENDIF
IF laDam1[1]+laDam1[2]+laDam1[3]+laDam1[4]+laDam1[5]+laDam1[6]+laDam1[7]+laDam1[8]<>0
  IF EMPTY(lcRetSty1)
    lcRetSty1=STYLE.cRetSty
  ENDIF
  SHOW GET lcRetSty1 ENABLE
  SHOW GET ibRtSt1   ENABLE

  *B604677,1 MHM 07/19/2001 Disable the remaning buckets according to the
  *B604677,1                scale [Start]
  IF !EMPTY(lcRetSty1) AND lnCnxt = 1
    =lfChckScal(lcRetSty1,'1')
  ENDIF  
  *B604677,1 MHM 07/19/2001 [End]
ELSE
  lcRetSty1=SPACE(19)
  SHOW GET lcRetSty1 DISABLE
  SHOW GET ibRtSt1   DISABLE
ENDIF
IF laDam2[1]+laDam2[2]+laDam2[3]+laDam2[4]+laDam2[5]+laDam2[6]+laDam2[7]+laDam2[8]<>0
  IF EMPTY(lcRetSty2)
    lcRetSty2=STYLE.cRetSty2
  ENDIF
  SHOW GET lcRetSty2 ENABLE
  SHOW GET ibRtSt2   ENABLE

  *B604677,1 MHM 07/19/2001 Disable the remaning buckets according to the
  *B604677,1                scale [Start]
  IF !EMPTY(lcRetSty2) AND lnCnxt = 1
    =lfChckScal(lcRetSty2,'2')
  ENDIF  
  *B604677,1 MHM 07/19/2001 [End]
ELSE
  lcRetSty2=SPACE(19)
  SHOW GET lcRetSty2 DISABLE
  SHOW GET ibRtSt2   DISABLE
ENDIF

*T20060809.0006   TMI [Start] Enable disable the bin fields for 2nd/damaged quality
IF ASCAN(laEvntTrig,'ENDSBINS') <> 0
  =gfDoTriger('POSTREC','ENDSBINS')
ENDIF
*T20060809.0006   TMI [End  ] 

IF llSOInstld AND !(lcPType $ 'RN') AND _CUROBJ = OBJNUM(laCan[lnCnxt]) AND ;
   ( laCan[lnCnxt] > laBud[lnCnxt] - laAlo[lnCnxt] )
  lcCtPkKey = IIF(llMfCall,'1'+&lcTmpLine..Cuttkt,'2'+&lcTmpLine..PO)+&lcTmpLine..Style
  IF SEEK(lcCtPkKey,'CutPick')
    IF !lfChkOrdQt(STR(lnCnxt,1),.F.)
      laCan[lnCnxt] = lcOldValue
      SHOW GET laCan[lnCnxt]
      _CUROBJ=_CUROBJ
      RETURN
    ENDIF
  ENDIF
*C037957,1 MHM 10/10/2004 Custom Amend CutPick File[Start]
ELSE
  IF ASCAN(laEvntTrig , PADR('POUPSTRC',10)) <> 0
    =gfDoTriger('POSTREC',PADR('POUPSTRC',10))
  ENDIF     
*C037957,1 [End]
  
ENDIF
=lfRefresh("MFRCVQ")

*B039660,1 NNA 06/06/2006 (Begin) convert next trigger to be done within Binmain.Prg instead of Davmain.prg 
*C037816,1 MHM 04/06/2004 Refresh Custom Screen [Start]
IF ASCAN(laEvntTrig,PADR("VLDRFRSH",10)) <> 0 
  =gfDoTriger("POSTREC",PADR("VLDRFRSH",10))
ENDIF
*C037816,1 MHM  [End]
*B039660,1 NNA (End)

RETURN

*:*************************************************************
*! Name    : lfvQtyOk
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Quantity Ok pb. valid. 
*:*************************************************************
FUNCTION lfvQtyOk
IF ! laScrMode[2]
  lnAlias=SELECT()
  *-You cannot leave the XXXX style empty since the XXXX quantity was entered.
  IF laDam1[1]+laDam1[2]+laDam1[3]+laDam1[4]+laDam1[5]+laDam1[6]+laDam1[7]+laDam1[8]<>0 AND ;
     EMPTY(lcRetSty1)
     =gfModalGen('TRM34067B42000','DIALOG',lcRetSHd3+'|'+lcRetSHd3)
    _CUROBJ=OBJNUM(lcRetSty1) 
    SELECT(lnAlias)
    RETURN
  ENDIF
  IF laDam2[1]+laDam2[2]+laDam2[3]+laDam2[4]+laDam2[5]+laDam2[6]+laDam2[7]+laDam2[8]<>0 AND ;
    EMPTY(lcRetSty2)
    =gfModalGen('TRM34067B42000','DIALOG',lcRetSHd4+'|'+lcRetSHd4)
    _CUROBJ=OBJNUM(lcRetSty2) 
    SELECT(lnAlias)
    RETURN
  ENDIF

  IF llWareHous
    IF !EMPTY(lcRetSty1) AND !SEEK(PADR(lcRetSty1,19)+lcWareCode+SPACE(10),'STYDYE')
            
      *-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
      IF gfModalGen('QRM34048B42006','DIALOG',ALLTRIM(lcRetSty1)+'|'+lcWareCode) = 1
        DO gpAdStyWar WITH lcRetSty1,SPACE(10),lcWareCode
      ELSE
        _CUROBJ=OBJNUM(lcRetSty1) 
        SELECT(lnAlias)
        RETURN
      ENDIF
    ENDIF
    
    IF !EMPTY(lcRetSty2) AND !SEEK(PADR(lcRetSty2,19)+lcWareCode+SPACE(10),'STYDYE')
      *-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
      IF gfModalGen('QRM34048B42006','DIALOG',ALLTRIM(lcRetSty2)+'|'+lcWareCode) = 1
        DO gpAdStyWar WITH lcRetSty2,SPACE(10),lcWareCode
      ELSE
        _CUROBJ=OBJNUM(lcRetSty2) 
        SELECT(lnAlias)
        RETURN
      ENDIF
    ENDIF
    
  ENDIF


  *B602178,1 Check if other style is a dyelot yes style but the original style was No.
  IF llDyelot AND EMPTY(lcDyelot)
    IF !EMPTY(lcRetSty1) AND SEEK(lcRetSty1,'STYLE') AND STYLE.CDYE_FLG='Y'
      _CUROBJ=OBJNUM(lcRetSty1)
      *--The style xxx comes in dyelot but the original style xxxx did not use dyelots,
      *--Please make sure the the other quality style has same dyelot usage.
       =gfModalGen('TRM34135B42000','DIALOG',lcRetSty1+'|'+lcStyle)
      SELECT(lnAlias)
      RETURN
    ENDIF
    IF !EMPTY(lcRetSty2) AND SEEK(lcRetSty2,'STYLE') AND STYLE.CDYE_FLG='Y' 
      *--The style xxx comes in dyelot but the original style xxxx did not use dyelots,
      *--Please make sure the the other quality style has same dyelot usage.
      =gfModalGen('TRM34135B42000','DIALOG',lcRetSty2+'|'+lcStyle)
      _CUROBJ=OBJNUM(lcRetSty2)
      SELECT(lnAlias)
      RETURN
    ENDIF
  ENDIF

  *--If Receive Return P/O and costs method FIFO or LIFO.
  *--Not Accept to Issue more than Stock.
  *E301291,4 AMM Consider Issue adornment order case
  *IF lcPType $ 'RN' AND lcCostMth $ 'FL' 
  IF lcPType $ 'RNA' AND lcCostMth $ 'FL' 
  *E301291,4 AMM end
    IF laSok[1]+laSok[2]+laSok[3]+laSok[4]+laSok[5]+laSok[6]+laSok[7]+laSok[8]<>0
      *--Get Current Stock.
      SELECT STYDYE     
      *E300935,4 Seek Dyelot record.
      *=SEEK(PADR(lcStyle,19)+lcWareCode+SPACE(10))
      =SEEK(PADR(lcStyle,19)+lcWareCode+PADR(lcDyelot,10))
      SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 TO laSStk
      FOR I=1 TO 8
        IF laSStk[I] < laSok[I]
          *--Insufficient stock to issue P/O line!
          = gfModalGen('TRM34107B34000','DIALOG')
          _CUROBJ=OBJNUM(laSok[I]) 
          SELECT(lnAlias)
          RETURN 
        ENDIF
      ENDFOR
    ENDIF
    IF laDam1[1]+laDam1[2]+laDam1[3]+laDam1[4]+laDam1[5]+laDam1[6]+laDam1[7]+laDam1[8]<>0
      SELECT STYDYE     
      =SEEK(PADR(lcRetSty1,19)+lcWareCode+SPACE(10))
      SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 TO laSStk
      FOR I=1 TO 8
        IF laSStk[I] < laDam1[I]
          *--Insufficient stock to issue P/O line!
          = gfModalGen('TRM34107B34000','DIALOG')
          _CUROBJ=OBJNUM(laDam1[I]) 
          SELECT(lnAlias)
          RETURN 
        ENDIF
      ENDFOR
    ENDIF
    IF laDam2[1]+laDam2[2]+laDam2[3]+laDam2[4]+laDam2[5]+laDam2[6]+laDam2[7]+laDam2[8]<>0
      SELECT STYDYE     
      =SEEK(PADR(lcRetSty2,19)+lcWareCode+SPACE(10))
      SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 TO laSStk
      FOR I=1 TO 8
        IF laSStk[I] < laDam2[I]
          *--Insufficient stock to issue P/O line!
          = gfModalGen('TRM34107B34000','DIALOG')
          _CUROBJ=OBJNUM(laDam2[I]) 
          SELECT(lnAlias)
          RETURN 
        ENDIF
      ENDFOR
    ENDIF
  ENDIF
  SELECT(lnAlias)
ENDIF
CLEAR READ
RETURN

*:*************************************************************
*! Name    : lfvRetSty
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Return style valid. 
*:*************************************************************
FUNCTION lfvRetSty
PARA lcPQualty

IF !llBrowse AND !EMPTY(SUBSTR(lcRetSty&lcPQualty,1,1)) AND;
                 lcRetSty&lcPQualty=IIF(EMPTY(lcOldValue),SPACE(19),lcOldValue)
  RETURN
ENDIF  
DO CASE
  CASE lcMStyQlty='1'
    lcQualty=IIF(lcPQualty='1','2','3')
  CASE lcMStyQlty='2'
    lcQualty=IIF(lcPQualty='1','1','3')
  CASE lcMStyQlty='3'    
    lcQualty=IIF(lcPQualty='1','1','2')
ENDCASE
lnAlias=SELECT()
SELECT STYLE
lnStyRecNo=RECNO('STYLE')
lcStyScl=Scale
lcBrStyle=IIF(lcPQualty='1',lcRetSty1,lcRetSty2)
SET ORDER TO TAG StyQlty
IF llBrowse OR (!EMPTY(lcBrStyle) AND !SEEK(lcQualty+lcBrStyle,'STYLE'))
  llbrowse = .F.
  SET FILTER TO cStyGrade = lcQualty AND Make = llMFCall
  GO TOP
  IF EOF()
    *--No styles found for this quality. 
    =gfModalGen('TRM42087B42000','DIALOG')
    lcBrStyle = SPACE(19)
  ELSE
    lcHdEsc1 = ON('KEY','ESC')
    ON KEY LABEL ESC 
    lcBrStyle = gfStyBrw('I','','',.F.)
    ON KEY LABEL ESC &lcHdEsc1
  ENDIF 
  SELECT STYLE
  SET FILTER TO 
ENDIF
lcRetSty&lcPQualty = lcBrStyle
SET ORDER TO TAG Style
IF !EMPTY(LCBrStyle) AND Scale<>lcStyScl
  *--The selected style has a different size scale.
  SHOW GET lcRetSty&lcPQualty
  *B803251,1 SSH 04/05/2000 Prevent recive to damage/Cancel if the 
  *B803251,1 SSH 04/05/2000 styles have different scales.
  *=gfModalGen('TRM42089B42000','DIALOG')
  IF !lfChkScal(Scale,PrePak)

    *B804568,1 AMH Prevent recive to damage/Cancel if the styles have different scales [Start]
    *--B603822,1 RAMY [start]
    *lcBrStyle = ''
    *lcRetSty&lcPQualty = ''
    *SHOW GET lcRetSty&lcPQualty
    *IF BETWEEN(lnStyRecNo,1,RECCOUNT('STYLE'))
    *  GOTO lnStyRecNo IN STYLE
    *ENDIF  
    *SELECT(lnAlias)

    *=gfModalGen('TRM42089B42000','DIALOG',"","","Selected style has a different size scale. Can not proceed")
    *RETURN
    *lnReturn = gfModalGen('TRM42089B44009','DIALOG',"","","Selected style has a different size scale. Are you sure you want to proceed")
    *IF lnReturn = 1
    *  IF lcPQualty = '1'
    *    lnTotal   = laDam1[1] + laDam1[2] + laDam1[3] + laDam1[4] + laDam1[5] + laDam1[6] + laDam1[7] + laDam1[8]
    *    laBal[1]  = 0
    *    laDam1[1] = lnTotal
    *    FOR lnI = 2 TO 8
    *      laDam1[lnI] = 0
    *      laBal[lnI]  = laOrg[lnI] - laSok[lnI] - laDam1[lnI] - laDam2[lnI] - laCan[lnI]
    *    ENDFOR
    *    SHOW GETS
    *    *-B604677,1 Disable remaning field if scale is deffrante [START]
    *     =lfChckScal(lcRetSty&lcPQualty,'1')
    *    *-B604677,1 Disable remaning field if scale is deffrante [END]
    *  ELSE
    *    lnTotal   = laDam2[1] + laDam2[2] + laDam2[3] + laDam2[4] + laDam2[5] + laDam2[6] + laDam2[7] + laDam2[8]
    *    laBal[1]  = 0
    *    laDam2[1] = lnTotal
    *    FOR lnI = 2 TO 8
    *      laDam2[lnI] = 0
    *      laBal[lnI]  = laOrg[lnI] - laSok[lnI] - laDam1[lnI] - laDam2[lnI] - laCan[lnI]
    *    ENDFOR
    *    SHOW GETS
    *    *-B604677,1 Disable remaning field if scale is deffrante [START]
    *     =lfChckScal(lcRetSty&lcPQualty,'2')
    *    *-B604677,1 Disable remaning field if scale is deffrante [End]
    *  ENDIF
    *ELSE
    *  lcBrStyle = ''
    *  lcRetSty&lcPQualty = ''
    *  SHOW GET lcRetSty&lcPQualty
    *  IF BETWEEN(lnStyRecNo,1,RECCOUNT('STYLE'))
    *    GOTO lnStyRecNo IN STYLE
    *  ENDIF
    *  SELECT(lnAlias)
    *  RETURN
    *ENDIF
    *--603822,1 RAMY [end]
    lcBrStyle = ''
    lcRetSty&lcPQualty = ''
    SHOW GET lcRetSty&lcPQualty
    IF BETWEEN(lnStyRecNo,1,RECCOUNT('STYLE'))
      GOTO lnStyRecNo IN STYLE
    ENDIF  
    SELECT(lnAlias)

    =gfModalGen('TRM42089B42000','DIALOG',"","","Selected style has a different size scale. Can not proceed")
    RETURN
    *B804568,1 AMH [End]
    
  ELSE
    =gfModalGen('INM42089B42000','DIALOG')
  ENDIF
  *B803251,1 SSH [End]
ENDIF
IF !EMPTY(LCBrStyle) AND llWareHous
  SELECT STYDYE
  IF !SEEK(PADR(LCBrStyle,19)+lcWareCode+SPACE(10))
    *-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
    IF gfModalGen('QRM34048B42006','DIALOG',ALLTRIM(LCBrStyle)+'|'+lcWareCode) = 1
      DO gpAdStyWar WITH LCBrStyle,SPACE(10),lcWareCode
    ELSE
     STORE lcOldValue TO lcRetSty&lcPQualty,LCBrStyle
    ENDIF
  ENDIF
ENDIF

*E300935,4 Check of phisical record before go to it.
IF BETWEEN(lnStyRecNo,1,RECCOUNT('STYLE'))
  GOTO lnStyRecNo IN STYLE
ENDIF  

SELECT(lnAlias)
RETURN


*!*************************************************************
*! Name    : lfvLCost
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Get/Calculate Landed cost.
*!*************************************************************
FUNCTION lfvLCost

lcWinNam = 'P/O Line Landed Cost'

SELECT (lcTmpLine)
lcHldTab = ON('KEY','TAB')
lcHldBtb = ON('KEY','BACKTAB')    
lcHldEnt = ON('KEY','ENTER')
ON KEY LABEL TAB    
ON KEY LABEL BACKTAB
ON KEY LABEL ENTER

DO (gcScrDir+"MFRCVLNC.SPX")

ON KEY LABEL TAB     &lcHldTab
ON KEY LABEL BACKTAB &lcHldBtb
ON KEY LABEL ENTER   &lcHldEnt
RETURN

*!*************************************************************
*! Name      : lfGetEqv
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose   : Get eguevalent costs.
*!*************************************************************
FUNCTION lfGetEqv
PARA lcUpdCsts,lnPRate1,lnDRate2,lnCurUnt1,lnCurUnt2,;
     lnFCost1,lnFCost2,lnFCost3,lnFCost4,lnFCost5

DIME laECost[LEN(lcUpdCsts)]
lnPt = 1
IF '1' $ lcUpdCsts 
  laECost[lnPt] = lfvEquCost('1',lnFCost1,lnPRate1,lnCurUnt1)
  lnPt = lnPt + 1
ENDIF
IF '2' $ lcUpdCsts 
  laECost[lnPt] = lfvEquCost('2',lnFCost2,lnDRate2,lnCurUnt2)
  lnPt = lnPt + 1
ENDIF
IF '3' $ lcUpdCsts 
  laECost[lnPt] = lfvEquCost('3',lnFCost3,lnDRate2,lnCurUnt2)
  lnPt = lnPt + 1
ENDIF
IF '4' $ lcUpdCsts 
  laECost[lnPt] = lfvEquCost('4',lnFCost4,lnDRate2,lnCurUnt2)
  lnPt = lnPt + 1
ENDIF
IF '5' $ lcUpdCsts 
  laECost[lnPt] = lfvEquCost('5',lnFCost5,lnDRate2,lnCurUnt2)
  lnPt = lnPt + 1
ENDIF
RETURN

*!*************************************************************
*! Name      : lfvEquCost
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose   : Get eguevalent cost by pathing the foreign.
*!*************************************************************
FUNCTION lfvEquCost
PARAMETERS lcCstNo,lnFrnCost,lnCurRate,lnCurUnt

lnCstType = lcIType&lcCstNo
IF lnCstType $ 'PMD'
  STORE '' TO lcPMethod,lcPUnMeth,lcDMethod,lcDUnMeth
  IF lnCstType='P'
    lcPMethod = gfGetExSin(@lcPUnMeth,lcCur1)
    lcPMethod = IIF(EMPTY(lcPMethod),'*',lcPMethod)
    lcPUnMeth = IIF(EMPTY(lcPUnMeth),'/',lcPUnMeth)
    lnEquCost = lnFrnCost &lcPMethod lnCurRate &lcPUnMeth lnCurUnt
  ELSE
    lcDMethod = gfGetExSin(@lcDUnMeth,lcCur2)
    lcDMethod = IIF(EMPTY(lcDMethod),'*',lcDMethod)
    lcDUnMeth = IIF(EMPTY(lcDUnMeth),'/',lcDUnMeth)
    lnEquCost = lnFrnCost &lcDMethod lnCurRate &lcDUnMeth lnCurUnt
  ENDIF
ELSE
  lnEquCost = lnFrnCost
ENDIF
*C127341,19 TMI [Start] round to 3 decimals
*lnEquCost = ROUND(lnEquCost,2)
lnEquCost = ROUND(lnEquCost,3)
*C127341,19 TMI [End  ] 
RETURN (lnEquCost)

*!*************************************************************
*! Name    : lpClsScr
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Cancel/Close entery.
*!*************************************************************
FUNCTION lpClsScr
*--Cancel unCmSess record.
=lfCrUnSess('','I',.F.) 
=lfClearInfo()
laScrMode=.F.
laScrMode[1]=.T.
*C102172,1 AMH Custom process to Receive by lot or by invoice [Start]
IF ASCAN(laEvntTrig,PADR("ADOPTCAT",10)) <> 0
  STORE .F. TO llRecLot, llRecInv
ENDIF  
*C102172,1 AMH [End]
RETURN


*!*************************************************************
*! Name     : lpSavScr
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose  : Post receinving (Save and Update).
*!*************************************************************
*! Calls    : Procedures : ....
*!            Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lpSavScr()
*!*************************************************************
*!Modifications :
*!E301180,1 Modificated to call POSTYREC report befor posting
*!E301182,7 MAB 03/31/1999 Trace cutpick file using master key.
*!E301206,1 Reham On 04/21/99
*!E301206,1 Add parameter if called from bar code programs
*!*************************************************************
*!
FUNCTION lpSavScr
*E301206,1 Reham On 04/21/99   *** Begin ***
*E301206,1 True if called from any of the bar code programs.
PARAMETERS llBarCode
*E301206,1 Reham On 04/21/99   *** End   ***
*E301180,1

*E128404,1  TMI [Start] if no qty is applied do not save
*B131443,1 MHM 03/22/2006 include damaged and cancelled Qty in checking[Start]
*IF lnTotStk <= 0
IF (lnTotStk+lnTotCan+lnTotDam) <= 0
*B131443,1 MHM 03/22/2006 [End]

  =gfModalGen('INM34061B42000','DIALOG',lcBrDtTtl)
  STORE .F. TO llShow,llCSave
  RETURN .F.
ENDIF
*E128404,1  TMI [End  ] 

*C123616,1  TMI [Start] no need for this trigger , the screen is added in option menu
*C200488,1 WAB (Start) Call Triger to Call the Carton Screen
*IF ASCAN(laEvntTrig,PADR("CARTNSCR",10)) <> 0
*  IF !gfDoTriger("POSTREC",PADR("CARTNSCR",10))
*    RETURN
*  ENDIF
*ENDIF
**C200488,1 WAB (End) 
*C123616,1  TMI [End  ] 

*B039660,1 NNA 06/06/2006 (Begin) convert next trigger to be done within Binmain.Prg instead of Davmain.prg 
*C037816,1 MHM 04/06/2004 Check for Bin Location [Start]
IF ASCAN(laEvntTrig,PADR("DLCHKSAV",10)) <> 0
  IF gfDoTriger("POSTREC",PADR("DLCHKSAV",10))
    RETURN
  ENDIF
ENDIF
*C037816,1 [End]
*B039660,1 NNA (End)

*C123616,1  TMI [Start] if carton qty is different than qty in the main shipment screen then add a warning
IF ASCAN(laEvntTrig,'SHPWRN')<>0
  IF !gfDoTriger('POSTREC','SHPWRN')
    RETURN .F.
  ENDIF
ENDIF
*C123616,1  TMI [End  ] 

*C102172,1 AMH Deleting records of trancd='5' or '6' for Receive by invoice [Start]
IF ASCAN(laEvntTrig,PADR("ADOPTCAT",10)) <> 0
  =gfDoTriger("MFRCVCT",PADR("SAVERECV",10))
ENDIF  
*C102172,1 AMH [End]

*C102581,1 AMH trigger for Robyn Merdith to save the user defined fields [Start]
IF ASCAN(laEvntTrig,PADR("SAVEFLD",10)) <> 0
  =gfDoTriger("MFRCVCT",PADR("SAVEFLD",10))
ENDIF  
*C102581,1 AMH [End]

*E301484,1 Move saving function to a new separate program POUPDATE to be called from EDI programs.

*B604677,1 MHM 07/19/2001 Clear Filter From style file [Start]
lnCurAlias = SELECT()
SELECT STYLE
lcOldFltr = FILTER()
SET FILTER TO
SELECT(lnCurAlias)
*B604677,1 MHM 07/19/2001 [End]
DO gfSaveRec IN (gcapphome+'POUPDATE.PRG') WITH llBarCode

*C123616,1  TMI [Start] no need for this trigger
**C200488,1 WAB (Start) Call trigger to save the carton file
*IF ASCAN(laEvntTrig,PADR("SAVCRTNS",10)) <> 0
*  =gfDoTriger("POSTREC",PADR("SAVCRTNS",10))
*ENDIF
**C200488,1 WAB (End) 
*C123616,1  TMI [End  ] 

*C123616,1  TMI [Start] Save shipped cartons to the file POCRTNMF
IF ASCAN(laEvntTrig,PADR("SVSHP",10)) <> 0
  =gfDoTriger("POSTREC",PADR("SVSHP",10))
ENDIF  
*C123616,1  TMI [End  ] 

*B604677,1 MHM 07/19/2001 Retrive filter [Start]
lnCurAlias = SELECT()
SELECT STYLE
SET FILTER TO &lcOldFltr
SELECT(lnCurAlias)
*B604677,1 MHM 07/19/2001 [End]

*C128583,1  TMI [Start] copy csv file from Import folder to archive folder for PAN21
IF ASCAN(laEvntTrig,PADR("COPYCSV",10)) <> 0
  =gfDoTriger("POSTREC",PADR("COPYCSV",10))
ENDIF  
*C128583,1  TMI [End  ] 

*E301484,1 (End)


*:*************************************************************
*! Name    : lfGetOpen
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Get original open quantity on P/o and return
*!           the original warehouse.
*:*************************************************************
FUNCTION lfGetOpen
*E300935,4 Add new logical parameter if .T. skip subtract code.
PARA lcParmKey,llSubtNo
IF EMPTY(lcDyelot)
  lcParmKey = ALLTRIM(lcParmKey)
ENDIF

lnAlias = SELECT()
SELECT (lcMasterLn)
*B802566,1 Start, Save the record pointer
PRIVATE lnMsLnRNo 
lnMsLnRNo = RECNO()
*B802566,1 End
SEEK lcParmKey

lcOrjWareH = cWareCode
laOpnQty   = 0
lcWhlCndn  = IIF(llMFCall,"Cuttkt+Style+Dyelot","cStyType+PO+Style+STR(LineNo,6)")+"=lcParmKey"
*E301291,4 AMM Consider receive adornment order case
*IF lcPType <> 'O'

*E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
*IF !(lcPType $ 'OE')
IF !(lcPType $ 'OEL')
*E301480,1 NAD (End)

*E301291,4 AMM end
*B602698,1[BEGIN] Do it in case of C/T
*T20061003.0005 TMI [Start] Add the check on line# in case of llMFCall is .T.
*lcForCndn = IIF(llMFCall,".T.","TranCd <> '3'")
lcForCndn = IIF(llMFCall,"Lineno = m.Lineno","TranCd <> '3'")
*T20061003.0005 TMI [End  ] 
*  lcForCndn = "TranCd <> '3'"
*B602698,1[END..]  
  lcBaseTrCd = '1'
ELSE
  lcForCndn = "TranCd <> '1'"
  lcBaseTrCd = '6'
ENDIF
*B039484,1 MMR 07/29/2005 Fix bug of duplicating qty amd cost while recieving C/T.[Start]
*SCAN REST WHILE &lcWhlCndn FOR &lcForCndn
*T20061003.0005 TMI [Start] remove the fix B039484 and instead update the lcForCndn for the case of Cuttkt by checking on line#
*SCAN REST WHILE &lcWhlCndn FOR &lcForCndn AND cwarecode=lcWareCode
SCAN REST WHILE &lcWhlCndn FOR &lcForCndn
*T20061003.0005 TMI [End  ] 
*B039484,1 MMR.[End]
  FOR I=1 TO 8
    lcCnt=STR(I,1)
    *E300935,4 adjust get open quantity (P/O Batch case)
    *laOpnQty[I]=IIF(TranCd=lcBaseTrCd,laOpnQty[I]+Qty&lcCnt,MAX(laOpnQty[I]-Qty&lcCnt,0))
    *C200080,1 AMM  Add the dye order type 'D'
    *laOpnQty[I]= IIF(TranCd=lcBaseTrCd,laOpnQty[I]+Qty&lcCnt,;
                 IIF(lcPType $ 'IBS' AND !EMPTY(DYELOT),laOpnQty[I]-Qty&lcCnt,;
                 MAX(laOpnQty[I]-Qty&lcCnt,0))) 

    *B604502,1 MHM 06/11/2001 (Begin) Tacking care of inter-location PO
    *laOpnQty[I]= IIF(TranCd=lcBaseTrCd,laOpnQty[I]+Qty&lcCnt,;
                 IIF(lcPType $ 'IBSD' AND !EMPTY(DYELOT),laOpnQty[I]-Qty&lcCnt,;
                 MAX(laOpnQty[I]-Qty&lcCnt,0))) 
    laOpnQty[I]= IIF(TranCd=lcBaseTrCd,laOpnQty[I]+Qty&lcCnt,;
                 IIF(lcPType $ 'IBSD' AND !EMPTY(DYELOT),laOpnQty[I]-Qty&lcCnt,;
                 IIF(lcPType ='O',laOpnQty[I]-Qty&lcCnt,MAX(laOpnQty[I]-Qty&lcCnt,0)))) 
    *B604502,1 MHM 06/11/2001 (End)
    *C200080,1 AMM end
  ENDFOR
ENDSCAN

*B604502,1 MHM 06/11/2001 (Begin) Taking care of inter-location PO
IF lcPType = 'O'
  FOR lnCntr = 1 TO 8
    laOpnQty[lnCntr] = MAX(laOpnQty[lnCntr],0)
  ENDFOR
ENDIF
*B604502,1 MHM 06/11/2001 (End)

*E300935,4 Evaluate remain Open Quantity if there is another lines.[Begin]
*E300935,4 But this in case of system support dyelots and Imported styles or another warehouse.
*IF !llSubtNo AND !EMPTY(Dyelot) AND (lcPType $ 'IBS')
*B602698,1[BEGIN] Do it in case of C/T
*IF !llSubtNo AND (lcPType $ 'IBS')
*C200080,1 AMM Add the dye order type 'D'
*IF !llSubtNo AND (lcPType $ 'IBSM')
IF !llSubtNo AND (lcPType $ 'IBSMD')
*C200080,1 AMM end
*B602698,1[END..]
  SEEK lcParmKey
  SELECT (lcTmpLine)
  *B802566,1 Check on dyelot in case of c/t also.
  lcCurDyelot = IIF(lcPType $ 'BM',Dyelot,lcDyelot)
  lcCurWare   = IIF(lcPType $ 'BM',cWareCode,lcWareCode)
  lcGoAgain   = Trancd+cCarton+IIF(llMFCall,Cuttkt,Po)+Style+Dyelot+PADR(cWareCode,6)+STR(LineNo,6)
  *'TranCd+cCarton+Cuttkt+Style+Dyelot+cWareCode+STR(LineNo,6)'
  lcScanExpr  = IIF(lcPType = 'B',cCarton+IIF(llMFCall,Cuttkt,Po)+Style+STR(LineNo,6),;
                    '   '+IIF(llMFCall,Cuttktl.Cuttkt,POSLN.PO)+lcStyle+STR(IIF(llMFCall,Cuttktl.LINENO,POSLN.LINENO),6))
  *lcForExpr = IIF(lcPType = 'B',"(Dyelot+cWareCode # lcCurDyelot+lcCurWare) AND (Trancd # '1')",;
                                "Trancd # '1'")
  lcForExpr = IIF(lcPType $ 'BM',"(Dyelot+cWareCode # lcCurDyelot+lcCurWare) AND (Trancd # '1')",;
                                "Trancd # '1'")
  *B802566,1 End.
  
  *B602698,1[BEGIN]
  *B603275,1 AMM Adjust the scan condition
  *lcScanCond=cCarton+IIF(llMFCall,Cuttkt,Po)+Style+STR(LineNo,6)
  lcScanCond  = "cCarton"+IIF(llMFCall,"Cuttkt","Po")+"Style"+"STR(LineNo,6)"
  *B603275,1 AMM end

  *B602698,1[END..]
  GO TOP
  IF !EOF()
    SCAN REST WHILE  lcScanCond = lcScanExpr;
         FOR &lcForExpr
      FOR I=1 TO 8
        Z=STR(I,1)
        *E300935,4 Now MAX Function is on the screen.
        *laOpnQty[I] = MAX(laOpnQty[I]-Qty&Z,0)
        laOpnQty[I] = laOpnQty[I]-Qty&Z
      ENDFOR 
    ENDSCAN
    = SEEK(lcGoAgain)
  ENDIF  
ENDIF  
*E300935,4 Evaluate remain Open Quantity if there is another lines.[End]
*B802566,1 MAN Start, Restore the record pointer for the master line file
SELECT (lcMasterLn)
IF BETWEEN(lnMsLnRNo,1,RECCOUNT(lcMasterLn))
 GO lnMsLnRNo 
ENDIF
*B802566,1 End

SELECT(lnAlias)
RETURN (lcOrjWareH)


*:*************************************************************
*! Name     : lfGetLanded
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose  : Calculate landed costs case of detail costing. 
*:*************************************************************
FUNCTION lfGetLanded

*B123103,1  TMI [Start] Get color lenght 
PRIVATE laMajSeg,lnColStru,lnClrPos,lnClrLen
DIMENSION laMajSeg[1,1]
=gfItemMask(@laMajSeg)
*B609021,1 TMI [start] adjust the for loop lenght
*FOR lnI = 1 TO ALEN(laMajSeg)
FOR lnI = 1 TO ALEN(laMajSeg,1)
  *B609021,1 TMI [ end ] adjust the for loop lenght
  IF laMajSeg[lnI,1] = 'C'
    lnColStru = lnI
    EXIT
  ENDIF
ENDFOR
lnClrPos = laMajSeg[lnColStru,4]
lnClrLen = LEN(laMajSeg[lnColStru,3])
*B123103,1  TMI [End  ] 


*B803337,1 WMA [Begin]
*E301077,11 MAB Open BOMLINE file here [Begin
*IF !llOpnBomLn
*  llOpnBomLn = lfOpn_Rest(gcDatadir,'BOMLINE','BOMLINE')
*ENDIF  
*E301077,11 MAB Open BOMLINE file here [End..
*B803337,1 WMA [End]

SELECT BOMLINE
IF lcPType ='S'
  SET ORDER TO TAG BomLnShp
ELSE
  SET ORDER TO TAG BomLine
ENDIF

*B602966,1 Start.
SELECT (lcTmpLine)
SCATTER FIELDS Qty1,qty2,qty3,qty4,qty5,qty6,qty7,qty8 TO laLnQty

FOR lnCnt=1 to 5
  lcCnt=STR(lnCnt,1)
  IF llMFCall
    *B039484,1 MMR 07/29/2005 Fix bug of duplicating qty amd cost while recieving C/T.[Start]
    *lcBomLKey = 'M2'+&lcTmpLine..Cuttkt
    *lcWCondtn = "cImTyp+cType+cTktNo = lcBomLKey"
    lcBomLKey = 'M2'+&lcTmpLine..Cuttkt+STR(&lcTmpLine..LineNo,6)
    lcWCondtn = "cImTyp+cType+cTktNo+STR(LineNo,6) = lcBomLKey"
    *B039484,1 MMR.[End]
    lcFCondtn = "cBomTyp=lcCnt AND Style=&lcTmpLine..Style AND (EMPTY(cRSession) OR cRSession=lcGlSession)"

    *B605612,1 KHM 02/28/2002 (Begin) Adding the checing of the style grade.
    lcFCondtn = lcFCondtn + " .AND. cStyGrade = &lcTmpLine..cStyGrade "
    *B605612,1 KHM 02/28/2002 (End)

  ELSE
    *lcBomLKey = 'I2'+IIF(lcPType='S',&lcTmpLine..Shipno,'')+&lcTmpLine..Po+STR(&lcTmpLine..LineNo,6)+lcCnt+&lcTmpLine..Style
    lcBomLKey = IIF(lcPType='D','D','I')+'2'+IIF(lcPType='S',&lcTmpLine..Shipno,'')+&lcTmpLine..Po+STR(&lcTmpLine..LineNo,6)+lcCnt+&lcTmpLine..Style
    lcWCondtn = "cImTyp+cType+IIF(lcPType='S',ShipNo,'')+cTktNo+STR(LineNo,6)+cBomTyp+Style = lcBomLKey"
    lcFCondtn = "(EMPTY(cRSession) OR cRSession=lcGlSession) AND cStyGrade = &lcTmpLine..cStyGrade"
  ENDIF 
  
  lnNLCs&lcCnt = 0
  lnCurSQt = 0
  
  SELECT BOMLINE
  IF SEEK(lcBomLKey)
    REPLACE REST StyQty  WITH lfBomSzQt(),;
                 ItemQty WITH (StyQty*UnitQty),;
                 ItemAmt WITH (ItemQty*UnitCost);
           WHILE &lcWCondtn FOR &lcFCondtn 
    =SEEK(lcBomLKey)
    *B802443,1 Start, Divide by total qty in the line.
    *SUM REST (UnitCost*UnitQty) WHILE &lcWCondtn FOR &lcFCondtn TO lnNLCs&lcCnt
     SUM REST (UnitCost*UnitQty)*StyQty WHILE &lcWCondtn FOR &lcFCondtn TO lnNLCs&lcCnt
     lnNLCs&lcCnt = IIF(&lcTmpLine..TotQty<>0,(lnNLCs&lcCnt/&lcTmpLine..TotQty),0)
     
     *khalid1
     lnNLCs&lcCnt = IIF(TYPE("lnNLCs"+lcCnt) <> "N",0,lnNLCs&lcCnt)
     *khalid1
     
    *B802443,1 End.
  ENDIF
  
  *B123103,1  TMI [Start] Get the cost for styles not of first quality from the bom file dirctly
  IF !EMPTY(&lcTmpLine..CRETSTY)
    lnNLCs&lcCnt = 0
    IF SEEK(PADR(SUBSTR(&lcTmpLine..CRETSTY,1,lnMjrWid),19)+lcCnt,'BOM')    && CITMMAJOR+TYP+CITMMASK+MFGCODE+ITEM+ICLR
      SELECT BOM
      SUM REST (BOM.UntCost*BOM.NESTBOMQTY) ;
          WHILE CITMMAJOR+TYP+CITMMASK+MFGCODE+ITEM+ICLR = PADR(SUBSTR(&lcTmpLine..CRETSTY,1,lnMjrWid),19)+lcCnt    ;
          FOR IIF(SUBSTR(BOM.CITMMASK,lnClrPos,lnClrLen)=REPL('*',lnClrLen),.T.,&lcTmpLine..CRETSTY = BOM.CITMMASK) ;
          TO lnNLCs&lcCnt
    ENDIF
  ENDIF  
  *B123103,1  TMI [End  ] 
  
  SELECT (lcTmpLine)
  REPLACE nLan_Cst&lcCnt WITH lnNLCs&lcCnt
  IF ! llMFCall
    IF llMulCurr
      
      *C127341,14 TMI [Start] use the fields nLanPrRat,nLanDuRat instead of lnRate1,lnRate2 , this is acurate if the rate is different for lines
      *=lfGetEqv(lcCnt,lnRate1,lnRate2,lnCurrUnt1,lnCurrUnt2,nLan_Cst1,nLan_Cst2,nLan_Cst3,nLan_Cst4,nLan_Cst5)
      =lfGetEqv(lcCnt,nLanPrRat,nLanDuRat,lnCurrUnt1,lnCurrUnt2,nLan_Cst1,nLan_Cst2,nLan_Cst3,nLan_Cst4,nLan_Cst5)      
      *C127341,14 TMI [End  ] 
      *khalid
      laECost[1] = IIF(TYPE("laECost[1]") <> "N", 0, laECost[1])
      *khalid
      
      *B607034,1 ARH 03/16/2003 (Start) Fixed numeric overflow bug in case of England
      laECost[1] = IIF(OCCURS('*',STR(laECost[1])) > 0,0,laECost[1])
      *B607034,1 ARH 03/16/2003 (End)
      
      REPLACE nELanCost&lcCnt WITH laECost[1]
    ELSE
      REPLACE nELanCost&lcCnt WITH lnNLCs&lcCnt
    ENDIF
  ENDIF
ENDFOR

SELECT (lcTmpLine)
RETURN
******************
FUNCTION lfBomSzQt
lnCurSQt = 0
FOR lnI=1 TO 8
  IF STR(lnI,1) $ BOMLINE.CSIZES
    lnCurSQt = lnCurSQt + laLnQty[lnI]
  ENDIF
ENDFOR
RETURN lnCurSQt
*B602966,1 End.

*:*************************************************************
*! Name    : lfAddNewLn
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Preparelines for Post.
*!           If new line add to P/o and update bom.
*:*************************************************************
FUNCTION lfAddNewLn

*--Update For new added P/o Lines.....
SELECT (lcTmpLine)
SCAN FOR TranCd='1'
  IF ! lNewLn
    LOOP
  ENDIF
        
  SCATTER MEMVAR
  *C200080,1 AMM Add the dye order type 'D'
  *=SEEK(IIF(lcPType='R','R','P')+&lcTmpLine..Po,'POSHDR')  
  =SEEK(IIF(lcPType$'RD',lcPType,'P')+&lcTmpLine..Po,'POSHDR')  
  *C200080,1 AMM end

  IF (llImpCost AND lcPType<>'R') AND !lfUpdBom('A')
    SELECT (lcTmpLine)
    LOOP
  ENDIF

  SELECT POSLN
  APPEND BLANK
  GATHER MEMVAR
  REPLACE cOwner WITH ' '
  =gfAdd_Info('POSLN')

  *--Call TraceKey global function.
  *=gfTraceKey('POSLN',POSLN.cstytype+POSLN.po+POSLN.style+STR(POSLN.lineno,6)+POSLN.trancd,'A')
  SELECT POSLN
  =gfTraceKey('POSLN',cStyType+Po+cRsession+Shipno+Style+STR(Lineno,6)+Trancd,'A')

  SELECT POSHDR
  =RLOCK()

  *E301077,11 Update Odded fields using gfAdd_Info Function [Begin]
  *Odded REPLACE LastLine   WITH POSLN.LineNo ,;
  *Odded         Mod_UserId WITH gcUser_id,;
  *Odded         Mod_Time   WITH TIME(),;
  *Odded         Mod_Date   WITH gdSysDate,;
  *Odded         Mod_Action WITH 'MODIFIED'

  REPLACE LastLine   WITH POSLN.LineNo
  =gfAdd_Info('POSHDR')
  *E301077,11 Update Odded fields using gfAdd_Info Function [End  ]

  UNLOCK
 
  SELECT (lcTmpLine)
  REPLACE lNewLn WITH .F.

  *--Call TraceKey global function.
  =gfTraceKey('POSHDR',POSHDR.cStyType+POSHDR.Po,'M')

ENDSCAN
RETURN

*:*************************************************************
*! Name    : lfDyeOvrRcv
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Check the new dyelots and over receive.
*:*************************************************************
FUNCTION lfDyeOvrRcv

SELECT (lcTmpLine)
LOCATE FOR TranCd='1' AND lNewLn
IF FOUND()
  lcDyeTmp= gfTempName()
  COPY STRU TO (gcWorkDir+lcDyeTmp)
  =gfOpenFile(gcWorkDir+lcDyeTmp,'','EX')
  DIMENSION laOpen[8]
  laOpen = 0
ELSE
  RETURN
ENDIF

WAIT CLEAR
SELECT (lcTmpLine)
SET ORDER TO TAG TmpLine3
GO TOP

DO WHILE !EOF()
  
  *--Only for new lines(Dyelots).
  IF TranCd='1' AND lNewLn
    lnSvRec = RECNO()
    SCATTER MEMVAR
    *-Validate the over receiving.

    *E300935,4 Add cwarecode to index
    *IF lfOpnQty(CutTkt+Style+Dyelot , .T. ) > 0
    IF lfOpnQty(CutTkt+Style+Dyelot+PADR(cWareCode,6) , .T. ) > 0
      SELECT (lcDyeTmp)
      APPEND BLANK
      GATHER MEMVAR
      GATHER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 FROM laOpen[8]
      REPLACE TranCd WITH '0',TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 
      SELECT (lcTmpLine)
      
      lcLkey   = CutTkt+Style
      lcNewDye = Dyelot
      
      *--Check the existance of other dyelots to allocate from.
      llOthDye = .F.
      SELECT CutTktL
      =SEEK(lcLkey)

      *SCAN REST WHILE CutTkt+Style = lcLkey  FOR Dyelot <> lcNewDye
      *B603218,1 SSH 26/10/99 Scan for the original line only.
      *SCAN WHILE CutTkt+Style = lcLkey  FOR Dyelot <> lcNewDye
      SCAN WHILE CutTkt+Style = lcLkey  FOR Dyelot <> lcNewDye AND TranCd = '1'
        *B603218,1 SSH (END)
        SCATTER MEMVAR
        
        *E300935,4 Add cwarecode to index
        *IF lfOpnQty(CutTkt+Style+Dyelot,.F.) > 0
        IF lfOpnQty(CutTkt+Style+Dyelot+PADR(cWareCode,6),.F.) > 0
          llOthDye = .T.
          SELECT (lcDyeTmp)
          APPEND BLANK
          GATHER MEMVAR
          GATHER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 FROM laOpen[8]
          REPLACE TranCd WITH '1',TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 
        ENDIF

      ENDSCAN
      
      IF llOthDye
        =lfAloDye()
      ENDIF
    
    ENDIF
    SELECT (lcTmpLine)
    
    *E300935,4 Check of phisical record before go to it.
    IF BETWEEN(lnSvRec,1,RECCOUNT())
      GOTO lnSvRec
    ENDIF  
    
    REPLACE lNewLn WITH .F.
  ENDIF 

  SKIP
ENDDO

IF USED(lcDyeTmp)
  USE IN (lcDyeTmp)
  ERASE (gcWorkDir+lcDyeTmp+'.DBF')
ENDIF
RETURN

*!*************************************************************
*! Name : lfOpnQty.
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpuse  : To comute the open quantity for a spasific 
*!            CT\Style\Dyelot form Tmp and Master receivings.
*!*************************************************************
*! Returned : Open qty.
*!*************************************************************
FUNCTION lfOpnQty
PARA lcPrKey,llOverRcv

lnAlias  = SELECT()
SELECT (lcTmpLine)
lnSavRec = RECNO()
*-- Get the open quantity. 
DIMENSION laOpnQty[8]
laOpnQty = 0

*E300935,4 Change index key to master file key [subtract warehouse code] begin.
lcMastKey = SUBSTR(lcPrKey,1,LEN(lcPrKey)-6)
*=lfGetOpen(lcPrKey)
=lfGetOpen(lcMastKey)
*E300935,4 Change index key to master file key [subtract warehouse code] end.

*-- Get new receiving.
DIMENSION laOpen[8]
laOpen = 0
SELECT (lcTmpLine)
=SEEK(lcPrKey)

*E300935,4 Add cwarecode to expression
*SCAN WHILE CutTkt+Style+Dyelot = lcPrKey FOR TranCd <> '1'
SCAN WHILE CutTkt+Style+Dyelot+PADR(cWareCode,6) = lcPrKey FOR TranCd <> '1'
  FOR I=1 TO 8
    Z=STR(I,1)
    laOpen[I]=laOpen[I]+Qty&Z
  ENDFOR
ENDSCAN

*--Calculate total open from master and temp receiving lines.
FOR I=1 TO 8
  Z=STR(I,1)
  IF llOverRcv
    *--Get over receive ,Zero if no over recv.
    laOpen[I] = ABS(MIN(laOpnQty[I] - laOpen[I],0))
  ELSE
    *--Get open ,zero if negative.
    *E300935,4
    *B602698,1[BEGIN] Do it in case of C/T
    *laOpen[I] = IIF(lcPType $ 'IS',laOpnQty[I] - laOpen[I],MAX(laOpnQty[I] - laOpen[I],0))
    *C200080,1 AMM Add the dye order type 'D'
    *laOpen[I] = IIF(lcPType $ 'ISM',laOpnQty[I] - laOpen[I],MAX(laOpnQty[I] - laOpen[I],0))  
    laOpen[I] = IIF(lcPType $ 'ISMD',laOpnQty[I] - laOpen[I],MAX(laOpnQty[I] - laOpen[I],0))  
    *C200080,1 AMM end
    *B602698,1[END..]
  ENDIF
ENDFOR

SELECT (lcTmpLine)

*E300935,4 Check of phisical record before go to it.
IF BETWEEN(lnSavRec,1,RECCOUNT())
  GOTO lnSavRec
ENDIF  

lnTotDOpn=laOpen[1]+laOpen[2]+laOpen[3]+laOpen[4]+laOpen[5]+laOpen[6]+laOpen[7]+laOpen[8]
SELECT (lnAlias)
RETURN (lnTotDOpn)


*!*************************************************************
*! Name    : lfAloDye
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Function to alocate dyelots qty to new dyelot.
*!*************************************************************
FUNCTION lfAloDye

lnAlias  = SELECT()
SELECT (lcDyeTmp)
LOCATE FOR TranCd = '0'
SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty To laOrv

lcPmCut  = Cuttkt
lcPmSty  = Style
lcDyeKey = lcPmCut+' / '+ALLTRIM(lcPmSty)+' / '+ALLTRIM(lcNewDye)
lcDyeBrTl= 'Dyelots'

=SEEK(lcPmSty,'STYLE')
=SEEK('S'+STYLE.Scale,'SCALE')
lcSz1=SPACE(5-LEN(ALLTRIM(SCALE.Sz1)))+ALLTRIM(SCALE.Sz1)
lcSz2=SPACE(5-LEN(ALLTRIM(SCALE.Sz2)))+ALLTRIM(SCALE.Sz2)
lcSz3=SPACE(5-LEN(ALLTRIM(SCALE.Sz3)))+ALLTRIM(SCALE.Sz3)
lcSz4=SPACE(5-LEN(ALLTRIM(SCALE.Sz4)))+ALLTRIM(SCALE.Sz4)
lcSz5=SPACE(5-LEN(ALLTRIM(SCALE.Sz5)))+ALLTRIM(SCALE.Sz5)
lcSz6=SPACE(5-LEN(ALLTRIM(SCALE.Sz6)))+ALLTRIM(SCALE.Sz6)
lcSz7=SPACE(5-LEN(ALLTRIM(SCALE.Sz7)))+ALLTRIM(SCALE.Sz7)
lcSz8=SPACE(5-LEN(ALLTRIM(SCALE.Sz8)))+ALLTRIM(SCALE.Sz8)
SET FILTER TO TranCd='1'

DO (gcScrDir+gcWinAppl+"\MFALODY.SPX")

SELECT (lcDyeTmp)
ZAP
*B603275,1 AMM Clear filter
SET FILTER TO
*B603275,1 AMM end

SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name    : lfAloDBr
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Function to browse available dyelots to alocate.
*!*************************************************************
FUNCTION lfAloDBr

lcBlDFields = "cCarton :R :2 :H='  ',"+;
              "Dyelot :R :H='Dyelot',"+;
              "Qty1 :R :H=lcSz1+' Qty.',"+;
              IIF(EMPTY(lcSz2),'',"Qty2 :R :H=lcSz2+' Qty.',")+;
              IIF(EMPTY(lcSz3),'',"Qty3 :R :H=lcSz3+' Qty.',")+;
              IIF(EMPTY(lcSz4),'',"Qty4 :R :H=lcSz4+' Qty.',")+;
              IIF(EMPTY(lcSz5),'',"Qty5 :R :H=lcSz5+' Qty.',")+;
              IIF(EMPTY(lcSz6),'',"Qty6 :R :H=lcSz6+' Qty.',")+;
              IIF(EMPTY(lcSz7),'',"Qty7 :R :H=lcSz7+' Qty.',")+;
              IIF(EMPTY(lcSz8),'',"Qty8 :R :H=lcSz8+' Qty.',")+;
              "TotQty :R :H='Total Qty.'"

BROWSE FIELDS &lcBlDFields ;
       NOAPPEND ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       TITLE (lcDyeBrTl) ;
       WHEN lfwDyEBr()   ;
       WINDOW MFALOD2 ;
       IN WINDOW MFALODY
RETURN
*****************
FUNCTION lfwDyEBr
IF EMPTY(cCarton)
  SHOW GET pbSel1  ENABLE
  SHOW GET pbUnSel DISABLE
ELSE
  SHOW GET pbSel1  DISABLE
  SHOW GET pbUnSel ENABLE
ENDIF

*!*************************************************************
*! Name    : lfvSelDye
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Select dyelot.
*!*************************************************************
FUNCTION lfvSelDye

REPLACE cCarton WITH '>>'
=lfwDyEBr()
SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty To   laSelct
GATHER FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd  FROM laSelct
laSelct[9]=0
laOrv[9]  =0
FOR I=1 TO 8
  IF laSelct[I]>=laOrv[I]
    laSelct[I]=laSelct[I]-laOrv[I]
    laOrv[I]=0
  ELSE
    laOrv[I]=laOrv[I]-laSelct[I]
    laSelct[I]=0
  ENDIF
  laSelct[9]=laSelct[9]+laSelct[I]
  laOrv[9]  =laOrv[9]  +laOrv[I]
ENDFOR
GATHER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty FROM laSelct

=lfRefresh('MFALOD1')
SHOW WINDOW (lcDyeBrTl) REFRESH
RETURN

*!*************************************************************
*! Name    : lfvUSlDye
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : UnSelect dyelot.
*!*************************************************************
FUNCTION lfvUSlDye
REPLACE cCarton WITH SPACE(3)
=lfwDyEBr()
laOrv[9]=0
FOR I=1 TO 8
  Z=STR(I,1)
  laOrv[I]=laOrv[I]+(Ord&Z-Qty&Z)
  laOrv[9]=laOrv[9]+laOrv[I]
ENDFOR

SCATTER FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd TO   laSelct
GATHER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty  FROM laSelct
=lfRefresh('MFALOD1')
SHOW WINDOW (lcDyeBrTl) REFRESH
RETURN


*!*************************************************************
*! Name    : lfvPrcDye
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Proceed with selected dyelots.
*!*************************************************************
FUNCTION lfvPrcDye

IF laOrv[9] > 0
  SELECT (lcDyeTmp)
  SUM ALL TotQty TO lnOpnDQty FOR TranCd = '1'
  GO TOP
  IF lnOpnDQty > 0
    *--The over received quantity is not allocated completely. Cannot proceed.
    = gfModalGen('TRM38120B42000','DIALOG')
    llUnblProc = .T.
    CLEAR READ
    RETURN
  ENDIF
ENDIF

*--Create cancel line for dyelots that we alocate from by alocate pieces.
DIME laTotDAlo[9]
laTotDAlo = 0
SELECT (lcDyeTmp)
SCAN FOR TranCd='1' AND !EMPTY(cCarton)
  SCATTER MEMVAR
  SELECT (lcTmpLine)
  APPEND BLANK
  GATHER MEMVAR
  FOR I=1 TO 8
    Z=STR(I,1)
    REPLACE QTY&Z WITH ( &lcDyeTmp..Ord&Z - &lcDyeTmp..Qty&Z ),;
            ORD&Z WITH 0
    laTotDAlo[I]=laTotDAlo[I]+QTY&Z
  ENDFOR  
  REPLACE TranCd WITH '4',;
          TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
          lNewLn WITH .F.           
  laTotDAlo[9]=laTotDAlo[9]+TotQty
ENDSCAN

*--Create original type '1' line for new dyelot with alocated pieces.
SELECT (lcDyeTmp)
SET FILTER TO
LOCATE FOR TranCd = '1'
lcOrgWh = cWareCode
LOCATE FOR TranCd = '0'
SCATTER MEMVAR
SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 To laSelct

*E300935,4 Add cwarecode to expression
*=SEEK(Cuttkt+Style+Dyelot,lcTmpLine)
=SEEK(Cuttkt+Style+Dyelot+PADR(cWareCode,6),lcTmpLine)

SELECT (lcTmpLine)
FOR I=1 TO 8
  Z=STR(I,1)
  REPLACE QTY&Z WITH laTotDAlo[I]
ENDFOR  
REPLACE TotQty   WITH laTotDAlo[9]
SCATTER MEMVAR

SELECT CUTTKTL
APPEND BLANK
GATHER MEMVAR
REPLACE TranCd    WITH '1' ,;
        cWareCode WITH lcOrgWh,; 
        cOwner    WITH ' '
=gfAdd_Info('CUTTKTL')

*--Call TraceKey global function.
=gfTraceKey('CUTTKTL',CUTTKTL.cuttkt+CUTTKTL.style+CUTTKTL.dyelot+CUTTKTL.trancd,'A')

SELECT CUTTKTH
=SEEK(CUTTKTL.Cuttkt)
=RLOCK()
IF Status='A'
  REPLACE Pcs_Act WITH Pcs_Act + CUTTKTL.TotQty
ELSE
  REPLACE Pcs_Bud WITH Pcs_Bud + CUTTKTL.TotQty
ENDIF

*E301077,11 Update Odded fields using gfAdd_Info Function [Begin]
*Odded REPLACE Mod_UserId WITH gcUser_id,;
*Odded         Mod_Time   WITH TIME(),;
*Odded         Mod_Date   WITH gdSysDate,;
*Odded         Mod_Action WITH 'MODIFIED'

=gfAdd_Info('CUTTKTH')
*E301077,11 Update Odded fields using gfAdd_Info Function [End  ]

UNLOCK
*--Call TraceKey global function.
=gfTraceKey('CUTTKTH',CUTTKTH.Cuttkt,'M')

CLEAR READ
RETURN


*!*************************************************************
*! Name    : lfUpdBom
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Function to update po bill of material.
*!*************************************************************
FUNCTION lfUpdBom
PARA lcAction
PRIVATE lnAlias

lnAlias  = SELECT(0)
llRetVal = .T.
lcShtTyp = IIF(llMFCall,'M','I')
lcShtKey = IIF(llMFCall,CUTTKTH.Cuttkt,POSHDR.Po)

*E301077,11 MAB Open Cut Ticket BOM file here [Begin
IF !llOpnKtBom
  llOpnKtBom = lfOpn_Rest(gcDatadir,'CTKTBOM','CTKTBOM')
ENDIF  
*E301077,11 MAB Open Cut Ticket BOM file here [End..

=SEEK(lcShtTyp+lcShtKey,"CTktBom")  

lcLinkCode= CTktBom.Link_Code
lcStyle   = &lcTmpLine..Style
lnPoLNo   = IIF(llMFCall ,0,&lcTmpLine..LineNo)

*E300935,4 Now Dyelot Field is in Both C/T and P/O
* lcShtDye  = IIF(llMFCall ,&lcTmpLine..Dyelot,SPACE(10))
lcShtDye  = &lcTmpLine..Dyelot
*E300935,4

lnPrice   = &lcTmpLine..nCost1
*B603385,1 Start.
*lcLastOpr = IIF(llMFCall ,CUTTKTH.cLastOpr,POSHDR.cLastOpr)
lcLastOpr = IIF(llMFCall,IIF(!EMPTY(CUTTKTH.cLastOpr),CUTTKTH.cLastOpr,&lcTmpLine..cLastOpr),IIF(!EMPTY(POSHDR.cLastOpr),POSHDR.cLastOpr,&lcTmpLine..cLastOpr))
*B603385,1 End.
lcItmWare = IIF(llMFCall ,CUTTKTH.cItemWare,POSHDR.cItemWare)
lcMatWare = IIF(llMFCall ,CUTTKTH.cMatWare,POSHDR.cMatWare)

*E301077,11 open mfgoprhd file.
*IF !laOpFile[2]
  = gfOpenFile(gcDataDir+"MFGOprHd","MFGOprHd","SH")
*ENDIF

SELECT (lcTmpLine)
SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laBomQty
laBomQty[9] = 0

FOR lnSize = 1 TO 8
  laBomQty[9] = laBomQty[9] + laBomQty[lnSize]
ENDFOR

*E300935,4 Pass Temp. Name and Don't Update in Bcakground Parameters
*E300935,4 to gfSheetItem Function to create Type '1' records in BomLine.
*llRetVal = gfSheetItem (lcShtTyp ,lcShtKey ,CTktBom.Link_Code,lcStyle,;
                        ""       , lnPoLNo   , lcShtDye , lcItmWare ,;
                        lcMatWare, @laBomQty,  "Bom" , "CtktBom"  ,;
                        "BomLine", "MFGOprHd", lcLastOpr , lnPrice    ,;
                        0, 0, 0, 0, 0)

*E301077,11 MAB Open BOM file here [Begin
IF !llOpnBom
  llOpnBom = lfOpn_Rest(gcDatadir,'BOM','BOM')
ENDIF  
*E301077,11 MAB Open BOM file here [End..

*B803337,1 WMA [Begin]
*E301077,11 MAB Open BOMLINE file here [Begin
*IF !llOpnBomLn
*  llOpnBomLn = lfOpn_Rest(gcDatadir,'BOMLINE','BOMLINE')
*ENDIF  
*E301077,11 MAB Open BOMLINE file here [End..
*B803337,1 WMA [End]

llRetVal = gfSheetItem (lcShtTyp ,lcShtKey ,CTktBom.Link_Code,lcStyle ,;
                        ""       , lnPoLNo   , lcShtDye , lcItmWare   ,;
                        lcMatWare, @laBomQty,  "Bom" , "CtktBom"      ,;
                        "BomLine", "MFGOprHd", @lcLastOpr , lnPrice   ,;
                        0, 0, 0, 0, 0,lcTmDyeRel,.T.)

*E300935,4 New Block Begin.
*E300935,4 Replace lCostMade field with Return value 
*E300935,4 avoiding another calling to gpAdFabWar function.
SELECT (lcTmpLine)
lnTmpRecNo = RECNO(lcTmpLine)
REPLACE lCostMade WITH llRetVal FOR LineNo = lnPoLNo
IF (lnTmpRecNo # 0) AND (lnTmpRecNo <= RECCOUNT())
  GOTO lnTmpRecNo
ENDIF
*E300935,4 New Block End.

SELECT (lnAlias)
RETURN (llRetVal)

*!*************************************************************
*! Name      : lfUpdLot
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose   : Function to update reciving quantity.
*!*************************************************************
*! Parameters: lcCuTick   ---> P/O or C/T number.
*!             lcOpration ---> Operation code.
*!             lcRecvLot  ---> Selected lot no.
*!             laQuantity ---> Array holding Received quantity
*!             lcRcvItem  ---> Received Style.
*!*************************************************************
FUNCTION lfUpdLot
PARAMETERS lcCTCode, lcOpration, lcRecvLot, laQuantity, lcRcvItem
PRIVATE laAddQty, laOpenQty, lcSaveRecKey, lnCount, lcCount, lcLotNo

DECLARE laAddQty[9], laOpenQty[9]

llLotFound = .F.
*B602309,1 If not found add a budjet record for last operaion.
SELECT MFGOPRDT
SET ORDER TO MFGOPRTR
*B802635,1 AMM Get the right record of style/color/dyelot
*IF !SEEK(IIF(llMFCall,'M','I')+lcCTCode+lcOpration+lcRecvLot+'1')

*khm1
*=SEEK(IIF(llMFCall,'M','I')+lcCTCode+lcOpration+lcRecvLot+'1')
*LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd=;
                IIF(llMFCall,'M','I')+lcCTCode+lcOpration+lcRecvLot+'1' ;
            FOR Item+cDyelot = lcRcvItem
=SEEK(IIF(llMFCall,'M',IIF(lcPType='D','D','I'))+lcCTCode+lcOpration+lcRecvLot+'1')
LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd=;
                IIF(llMFCall,'M',IIF(lcPType='D','D','I'))+lcCTCode+lcOpration+lcRecvLot+'1' ;
            FOR Item+cDyelot = lcRcvItem
*khm1

*B802635,1 AMM If found and not a cancelled record
IF !FOUND()
  IF  !((llMFCall .AND. &lcTmpLine..TranCd = '4' ) .OR. (!llMFCall .AND. &lcTmpLine..TranCd='5'))
*B802635,1 AMM end
    *B802635,1 AMM Add the operation to the seek expression
    *=SEEK(IIF(llMFCall,'M','I')+lcCTCode)
    =SEEK(IIF(llMFCall,'M','I')+lcCTCode+lcOpration)
    *B802635,1 AMM  end
    SCATTER MEMVAR MEMO
    *B602309,1
    SELECT CODES
    =SEEK('N'+lcOpration+'Y'+'MFGCODE   ','CODES')
    LOCATE REST WHILE cDefCode+ccode_no+crltfield+cfld_name = ;
                      'N'+lcOpration+'Y'+'MFGCODE   ' FOR CRLTD_NAM = 'CCONTCODE'
    SELECT MFGOPRDT
    *B802635,1 AMM Get dyelot to update in the file
    m.cDyelot = &lcTmpLine..Dyelot
    *B802635,1 AMM end
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE cIMtyp    WITH IIF(llMFCall,'M','I'),;
            cTktNo    WITH lcCTCode,;
            Item      WITH &lcTmpLine..Style,;
            cLotNo    WITH lcRecvLot ,;
            cOprCode  WITH lcOpration,;
            cContcode WITH IIF(EMPTY(cContcode),ALLT(CODES.cRltd_vlu),cContcode),;
            dTranDate WITH gdSysDate ,;
            DueDate   WITH gdSysDate ,;
            TranCd    WITH '1'       ,;
            cTrgOpr   WITH SPACE(2)  ,;
            cTrgLot   WITH SPACE(2)  ,;
            nLotQty1   WITH 0,;
            nLotQty2   WITH 0,;
            nLotQty3   WITH 0,;
            nLotQty4   WITH 0,;
            nLotQty5   WITH 0,;
            nLotQty6   WITH 0,;
            nLotQty7   WITH 0,;
            nLotQty8   WITH 0,;
            nLotTotQty WITH 0
  *B802635,1 AMM END IF
  ENDIF
  *B802635,1 AMM end
ENDIF
*B602309,1 end.
=SEEK(IIF(llMFCall,'M','I')+lcCTCode+lcOpration)
DO WHILE cIMTyp+cTktNo+cOprCode = IIF(llMFCall,'M','I')+lcCTCode+lcOpration AND !EOF()
  *B802635,1 AMM Add dyelot to the comparing item
  *IF Item <> lcRcvItem
  IF Item+cDyelot <> lcRcvItem
  *B802635,1 AMM end
    SKIP
    LOOP
  ENDIF
  IF !EMPTY(lcRecvLot) AND cLotNo<>lcRecvLot
    SKIP
    LOOP
  ENDIF

  lcLotNo    = cLotNo
  llLotFound = .T.
  *--Check if there is any open quantity for this Opr/Lor/Clr
  STORE 0 TO laOpenQty
  *B802635,1 AMM Add dyelot to the compared item
  *SCAN REST WHILE cIMTyp+cTktNo+cOprCode+cLotNo=IIF(llMFCall,'M','I')+lcCTCode+lcOpration+lcLotNo ;
              FOR Item=lcRcvItem
  SCAN REST WHILE cIMTyp+cTktNo+cOprCode+cLotNo=IIF(llMFCall,'M','I')+lcCTCode+lcOpration+lcLotNo ;
              FOR Item+cDyelot=lcRcvItem
  *B802635,1 AMM end
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      laOpenQty[lnCount] = MAX(laOpenQty[lnCount] + ;
                         IIF(TranCd='1',nLotQty&lcCount,-nLotQty&lcCount),0)
      laOpenQty[9] = laOpenQty[9] + laOpenQty[lnCount]
    ENDFOR
  ENDSCAN  
  *--Comupte quantity to be added for this Opr/Lot/Clr
  STORE 0 TO laAddQty  
  IF laOpenQty[9] > 0  
    FOR lnCount = 1 TO 8
      laAddQty[lnCount]    = MIN(&laQuantity[lnCount],laOpenQty[lnCount])
      &laQuantity[lnCount] = &laQuantity[lnCount] - laAddQty[lnCount]
      laAddQty[9]          = laAddQty[9]    + laAddQty[lnCount]
      &laQuantity[9]       = &laQuantity[9] - laAddQty[lnCount]
    ENDFOR
    IF laAddQty[9]>0
      lnSavRec=RECNO()
      =lfAppDetRec(lcLotNo, &lcTmpLine..TranCd,'laAddQty')
      
      *E300935,4 Check of phisical record before go to it.
      IF BETWEEN(lnSavRec,1,RECCOUNT())
        GOTO lnSavRec
      ENDIF  

    ENDIF
  ENDIF  
ENDDO
*--If there is over received quantity, Update the last lot with the remaind quantity.
*B802635,1 AMM Don't create the cancel record of over receiving
*IF llLotFound AND &laQuantity[9] > 0
IF llLotFound AND &laQuantity[9] > 0 .AND. !((llMFCall .AND. &lcTmpLine..TranCd = '4' ) .OR. (!llMFCall .AND. &lcTmpLine..TranCd='5'))
*B802635,1 AMM end
  =lfAppDetRec(lcLotNo, &lcTmpLine..TranCd,'&laQuantity')
ENDIF
SET ORDER TO MFGOPRDT IN MFGOPRDT
RETURN


*!*************************************************************
*! Name      : lfAppDetRec
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose   : Function to append transaction record in the
*!             MFG operation detailed file.   
*!*************************************************************
*! Parameters: lcLotNum   ---> Lot number
*!             lcTranCode ---> Transaction code
*!             laNewQty   ---> Array holding Quantity to be added
*!*************************************************************
FUNCTION lfAppDetRec
PARAMETERS lcLotNum, lcTranCode, laNewQty

IF !llMFCall
  lcTranCode = IIF(lcTranCode='4','3',IIF(lcTranCode='5','4',lcTranCode))
ENDIF

SELECT MFGOPRDT
=SEEK(IIF(llMFCall,'M','I')+lcCTCode+lcLastOpr+lcLotNum+'1')
*B802635,1 AMM Get the right record of style/color/dyelot
*LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd=;
                  IIF(llMFCall,'M','I')+lcCTCode+lcLastOpr+lcLotNum+'1' ;
              FOR Item = lcRcvItem
LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd=;
                  IIF(llMFCall,'M','I')+lcCTCode+lcLastOpr+lcLotNum+'1' ;
              FOR Item+cDyelot = lcRcvItem
*B802635,1 AMM end
SCATTER MEMVAR MEMO
APPEND BLANK
GATHER  MEMVAR MEMO
REPLACE nLotQty1   WITH &laNewQty[1] ,;
        nLotQty2   WITH &laNewQty[2] ,;
        nLotQty3   WITH &laNewQty[3] ,;
        nLotQty4   WITH &laNewQty[4] ,;
        nLotQty5   WITH &laNewQty[5] ,;
        nLotQty6   WITH &laNewQty[6] ,;
        nLotQty7   WITH &laNewQty[7] ,;
        nLotQty8   WITH &laNewQty[8] ,;
        nLotTotQty WITH &laNewQty[9] ,;
        dTranDate  WITH gdSysDate    ,;
        DueDate    WITH {}           ,;
        TranCd     WITH lcTranCode   ,;
        cTrgOpr    WITH SPACE(2)     ,;
        cTrgLot    WITH SPACE(2)     ,;
        cOwner     WITH ' '
=gfAdd_Info('MFGOPRDT')
*--Call TraceKey global function.
=gfTraceKey('MFGOPRDT',MFGOPRDT.cimtyp+MFGOPRDT.ctktno+MFGOPRDT.coprcode+MFGOPRDT.clotno+MFGOPRDT.trancd,'A')
RETURN(.T.)
*****************

*!*************************************************************
*! Name    : lfChkOrdQt
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Check order allocated quantity.
*!*************************************************************
FUNCTION lfChkOrdQt
PARA lcFldNo,llBachCk
PRIVATE lnAlias, llProceed
lnAlias   = SELECT(0)
llProceed = .T.

*C037957,1 MHM 10/10/2004 Custom Amend CutPick File[Start]
IF ASCAN(laEvntTrig , PADR('POUPCTPK',10)) <> 0
  IF lcProgName = "POSTREC" AND gfDoTriger('POSTREC',PADR('POUPCTPK',10))
    RETURN
  ENDIF
ENDIF     
*C037957,1 [End]

DO WHILE .T.
*--Size xxxxx has xxx pieces allocated from orders.
*--Edit the detail allocated quantity from the order lines to keep 
*--track of the allocated quantity, Ignore tracking the allocation 
*--for only the quantity being modified, or for all later modifications.
*--<Edit Allo. Qty.>/<Ignore>/<Ignore All>/<Cancel>
lcStr = ALLTRIM(lcSz&lcFldNo) + "|" +ALLTRIM(STR(laAlo[lnCnxt]))
lcBotn= IIF(llBachCk,'32006','32002')
lnChoice = IIF(llIgnorAll,3,gfModalGen('QRM32016B'+lcBotn,'ALERT',lcStr) )
IF lnChoice = 4
  RETURN (.F.)  
ENDIF

IF !USED(lcTmpCtPk)
  SELECT CUTPICK
  =AFIELDS(laFStru)
  lnFStru = ALEN(laFStru,1)
  DIMENSION laFStru[lnFStru+9,4]
  FOR I=1 TO 9
   laFStru[lnFStru+I,1] = 'nCurPck'+STR(I,1)
   laFStru[lnFStru+I,2] = 'N'
   laFStru[lnFStru+I,3] = 6
   laFStru[lnFStru+I,4] = 0
  ENDFOR  
  CREATE DBF (gcWorkDir+lcTmpCtPk) FROM ARRAY laFStru
  =gfOpenFile(gcWorkDir+lcTmpCtPk,'','EX')
  INDEX ON Trancd+cTktNo+Style+cOrdLine TAG lcTmpCtPk
ENDIF
SELECT CUTPICK
SCAN REST WHILE TranCd+cTktNo+Style=lcCtPkKey
  SCATTER MEMVAR
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laOdOrd
  IF !SEEK(lcCtPkKey+CUTPICK.cOrdLine,lcTmpCtPk)
    SELECT (lcTmpCtPk)
    APPEND BLANK
    GATHER MEMVAR
    GATHER FROM laOdOrd FIELDS nCurPck1,nCurPck2,nCurPck3,nCurPck4,nCurPck5,nCurPck6,nCurPck7,nCurPck8,nCurPck9
  ENDIF
ENDSCAN

IF lnChoice = 1
  llProceed = lfEdtAloQty(INT(VAL(lcFldNo)))
  laAlo[lnCnxt] = &lcTmpLine..Ord&lcFldNo
  *--Cannnot exit if cancel.
  IF llBachCk AND !llProceed
    LOOP
  ENDIF

ELSE
  SELECT (lcTmpCtPk)      
  =SEEK(lcCtPkKey)
  REPLACE REST WHILE TranCd+cTktNo+Style=lcCtPkKey ;
          Qty&lcFldNo WITH 0,;
          TotQty      WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
  SELECT (lcTmpLine)
  REPLACE Ord&lcFldNo WITH 0,;
          TotOrd      WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8,;
          lAloChg     WITH .T.
  
  IF lnChoice = 3
    llIgnorAll = .T.
  ENDIF
  laAlo[lnCnxt] = 0
ENDIF

EXIT
ENDDO

SELECT(lnAlias)
RETURN (llProceed)


*!*************************************************************
*: Name       : lfEdtAloQty
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*: Purpose    : To edit the allocated orders quantity.
*!*************************************************************
FUNCTION lfEdtAloQty
PARAMETERS lnSizeNo
PRIVATE lnAlias

DIMENSION laObj[8]

llRetVal        = .F.
llFrstTime      = .T.
lcCPBrTtl       = "Quantities Allocated from Order Lines"
laObj           = .F.
laObj[lnSizeNo] = .T.
lcToBrow        = SPACE(0)
lnOldQty        = 0
lnAlias         = SELECT(0)

PUSH KEY CLEAR
ON KEY LABEL TAB        DO lfCPTab
ON KEY LABEL ESC        DO lfCPEsc
ON KEY LABEL BACKTAB    DO lfCPStb
ON KEY LABEL Ctrl+ENTER lnDummy = 1
ON KEY LABEL Ctrl+HOME  lnDummy = 1
ON KEY LABEL Ctrl+W     lnDummy = 1
ON KEY LABEL Ctrl+END   lnDummy = 1
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcCPBrTtl) 

SELECT (lcTmpCtPk)
*B603235,1 SSH 22/05/2000 incorect allocate qty. incase of batch.
IF lcPType $ "MT"
  GOTO TOP
  =lfvCPCan()
ENDIF
*B603235,1 SSH 22/05/2000 [End]
DO (gcScrDir+"MFCtPkTr.SPX")
POP KEY
RELEASE WINDOW (lcCPBrTtl)
SELECT (lnAlias)
RETURN (llRetVal)

*!*************************************************************
*: Name       : lfCtPkTrBr
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*: Purpose    : Browse the allocated orders lines.
*!*************************************************************
FUNCTION lfCtPkTrBr
PRIVATE lcField

GO TOP
lcToBrow = "Order :H='Order# ':R:W=.F."
FOR lnI = 1 TO Scale.Cnt
  lcIStr  = STR(lnI,1)
  lcField = lcTmpCtPk + ".QTY" + lcIStr   
  lcHdr   = "'" + ALLTRIM(Scale.Sz&lcIStr) + "'"
  lcToBrow = lcToBrow + ", &lcField:H=&lcHdr. :W=laObj["+lcIStr+"] AND lfwCPGtFld() :B=0,99999 :V=lfvCPGtFld(&lcIStr)"
ENDFOR     
lcToBrow = lcToBrow + ", &lcTmpCtPk..TOTQTY:H='TotCut':W=.F."

BROWSE FIELDS &lcToBrow KEY lcCtPkKey LOCK 0 ;
       NOAPPEND NOCLEAR NODELETE NOWAIT SAVE NOMENU   ;
       TITLE (lcCPBrTtl) WINDOW CtPkTr2 IN WINDOW CtPkTr1
RETURN

*!*************************************************************
*: Name       : lfwCPGtFld
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*: Purpose    : Save the old value of the current edited allocated
*:              order line quantity.
*!*************************************************************
FUNCTION lfwCPGtFld

lnOldQty = EVAL(VARREAD())
RETURN

*!*************************************************************
*: Name       : lfCPTab
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*: Purpose    : Tab trapping function for the order line 
*:              allocation browse.
*!*************************************************************
FUNCTION lfCPTab

DO CASE
  CASE WONTOP() = (lcCPBrTtl)
    ACTIVATE WINDOW CtPkTr3
    _CUROBJ = OBJNUM(pbOk)
  CASE WONTOP() # (lcCPBrTtl) AND SYS(18) = "PBCAN"
    KEYBOARD "{ALT+B}" 
  OTHERWISE
    _CUROBJ = _CUROBJ + 1
ENDCASE

*!*************************************************************
*: Name       : lfCPStb
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*: Purpose    : Shit+Tab trapping function for the order line 
*:              allocation browse.
*!*************************************************************
FUNCTION lfCPStb

DO CASE
  CASE WONTOP() = (lcCPBrTtl)
    ACTIVATE WINDOW CtPkTr3
    _CUROBJ = OBJNUM(pbCan)
  CASE WONTOP() # (lcCPBrTtl) AND SYS(18) = "PBOK"
    KEYBOARD "{ALT+B}" 
  OTHERWISE
    _CUROBJ = _CUROBJ - 1
ENDCASE

*!*************************************************************
*: Name       : lfCPEsc
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*: Purpose    : Escape trapping function for the order line 
*:              allocation browse.
*!*************************************************************
FUNCTION lfCPEsc

ACTIVATE WINDOW CtPkTr3
_CUROBJ = OBJNUM(pbCan)
KEYBOARD "{ENTER}"

*!*************************************************************
*: Name       : lfvCPGtFld
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*: Purpose    : To validate the edited allocated order line
*:              quantity.
*!*************************************************************
FUNCTION lfvCPGtFld
PARAMETER lnObj
PRIVATE lnObj, lcObj, lnAlias

lnAlias  = SELECT(0)
lcObj    = STR(lnObj,1)
lnNewQty = &lcTmpCtPk..Qty&lcObj
lnCurQty = &lcTmpCtPk..nCurPck&lcObj

SELECT (lcTmpCtPk)
*B603230,1 SSH 25/10/99   Fix the bug of incorrect formula to get the new allocated qty.
*IF lnNewQty > lnCurQty

*B606589,1 KHM 11/05/2002 (Begin) The new qty should be less than or equal to old qty.
*IF IIF(lcPType $ 'SIM',lnNewQty > lnCurQty - laCan[lnObj],;
                     lnNewQty > lnCurQty)
IF lnNewQty > lnCurQty
*B606589,1 KHM 11/05/2002 (End)

*B603230,1 SSH (END)
  *-- The allocated quantity cannot be greater than 9999.
  *-- < Ok > 
  *B603230,1 SSH 25/10/99   Fix the bug of incorrect formula to get the new allocated qty.
  IF  lcPType $ 'SIM'
    *B606589,1 KHM 11/05/2002 (Begin) Changing the message to display the current qty only.
    *= gfModalGen('TRM38107B00000','DIALOG',ALLTRIM(STR(lnCurQty - laCan[lnObj],6)) )
    = gfModalGen('TRM38107B00000','DIALOG',ALLTRIM(STR(lnCurQty,6)))
    *B606589,1 KHM 11/05/2002 (End)
  ELSE
  
    *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
    *IF lcPType $ 'BT'
     IF lcPType $ 'BTL'
    *E301480,1 NAD (End)  
    
      lnCurQty = lnCurQty - laCan[lnObj]
    ENDIF
    = gfModalGen('TRM38107B00000','DIALOG',ALLTRIM(STR(lnCurQty,6)) )
  ENDIF
  *B603230,1 SSH(END)
  REPLACE Qty&lcObj WITH lnOldQty
ELSE                 
  REPLACE TotQty  WITH TotQty - lnOldQty + lnNewQty
ENDIF
SELECT(lnAlias)
RETURN

*!*************************************************************
*: Name       : lfvCPOk
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*: Purpose    : Ok button in the allocated order lines brose screen 
*:              valid function.
*!*************************************************************
FUNCTION lfvCPOk
PRIVATE lnAlias

lnAlias    = SELECT(0)
lcSzNo     = STR(lnSizeNo,1)
lcCurToChk = "nCurPck" + lcSzNo
lnVarToChk = laAlo[lnCnxt] - (laCan[lnCnxt]-(laBud[lnCnxt]-laAlo[lnCnxt]) )
lcFldToChk = "Qty"     + lcSzNo
llGoOut    = .F.
llRetVal   = .F.
*B603231,1 SSH 25/10/99   Fix the bug of incorrect formula to get the new allocated qty. 
*B606589,1 KHM 11/05/2002 (Begin) Commented out.
*lnVarToChk = EVAL(lcCurToChk)-laCan[lnCnxt]
*B606589,1 KHM 11/05/2002 (End)
*B603231,1 SSH(END)

SELECT (lcTmpCtPk)
SUM ALL &lcFldToChk, TotQty, &lcCurToChk, nCurPck9 ;
    FOR TranCd+cTktNo+Style = lcCtPkKey  ;
    TO lnNewVal, lnNewTot, lnOldVal, lnOldTot

*B606589,1 KHM 11/05/2002 (Begin) Changing EVAL(lcCurToChk) with lnOldVal to cover the case
*B606589,1                of having the same line more than once.
lnVarToChk = lnOldVal -laCan[lnCnxt]
*B606589,1 KHM 11/05/2002 (End)

IF lnNewVal > lnVarToChk

  *-- The allocated quantity cannot be greater than 9999.
  *-- < Ok > 
  = gfModalGen('TRM38107B00000','DIALOG',ALLTRIM(STR(lnVarToChk,6)) )
ELSE
  llGoOut  = .T.
  llRetVal = .T.
ENDIF

IF llGoOut
  SELECT (lcTmpLine)
  REPLACE Ord&lcSzNo WITH lnNewVal ,;
          TotOrd     WITH TotOrd - lnOldTot + lnNewTot,;
          lAloChg    WITH .T.  
  CLEAR READ
ENDIF
SELECT(lnAlias)


*!*************************************************************
*: Name       : lfvCPCan
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*: Purpose    : Cancel button in the allocated order lines brose 
*:              screen valid function.
*!*************************************************************
FUNCTION lfvCPCan
PRIVATE lnAlias

lnAlias    = SELECT(0)
llRetVal   = .F.
lcSzNo     = STR(lnSizeNo,1)

SELECT (lcTmpCtPk)
REPLACE ALL Qty&lcSzNo WITH nCurPck&lcSzNo ,;
            TotQty     WITH nCurPck9
SELECT(lnAlias)



*--UNCOMPLETE SESSION FUNCTIONS(UNCSS).
*:*************************************************************
*! Name      : lfGetTPop
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose   : Rebuld popup case of uncomplete session.
*:*************************************************************
FUNCTION lfGetTPop

*--Read Header info.
*E301291,4 AMM consider issue /receive adornment PO cases
*IF !(lcPType $ 'RN')
IF !(lcPType $ 'RNA')
*E301291,4 AMM end
  *C200080,1 AMM Add dye order in the POPUP
  *IF llMfCall
  IF llMfCall .OR. lcPType = 'D'
  *C200080,1 AMM end
    *C200080,1 AMM
    *DIME laType[2,2]
    DIME laType[3,2]
    *C200080,1 AMM end
    laType[1,1]='Receive Cutting Ticket'
    laType[1,2]='M'
    laType[2,1]='Receive Batch'
    laType[2,2]='T'
    *C200080,1 AMM
    laType[3,1]='Dye Order'
    laType[3,2]='D'
    *C200080,1 AMM end

  ELSE
    IF llImpCost AND lcCostImp='S'
      DIME laType[2,2]
      laType[1,1]='Receive by Shipment'
      laType[1,2]='S'
      laType[2,1]='Receive Batch'
      laType[2,2]='B'
    ELSE
      *E301291,4 AMM Add Receive adornment PO type 'E'
      *DIME laType[IIF(!llWareHous,3,4),2]
      DIME laType[IIF(!llWareHous,3,5),2]
      *E301291,4 AMM end
      laType[1,1]='Receive by P/O'
      laType[1,2]='I'
      laType[2,1]='Receive by Shipment'
      laType[2,2]='S'
      laType[3,1]='Receive Batch'
      laType[3,2]='B'
      IF llWareHous AND !llMfCall 
        laType[4,1]='Receive Inter Location P/O'
        laType[4,2]='O'    
      ENDIF
      *E301291,4 AMM Add Receive adornment PO type 'E'
      *B603237,1 No need to recive since there is a custom receive.
      IF llWareHous .AND. gfGetMemvar('M_BOMVAR') AND !llSpecHndl
        DIME laType[4,2]
        laType[4,1]='Receive Adornment P/O'
        laType[4,2]='E'
      ENDIF
      *B603237,1 End.
      *E301291,4 AMM end
    ENDIF
  ENDIF
ELSE
  *E301291,4 AMM Add issue adornment order type 'A'
  *IF lcPType = 'R'
    *laType[1,1]='Issue Return P/O'
    *laType[1,2]='R'
  *ENDIF
  *IF lcPType = 'N'
    *laType[1,1]='Issue Inter Location P/O'
    *laType[1,2]='N'
  *ENDIF
  DO CASE 
    CASE lcPType = 'R'
      laType[1,1]='Issue Return P/O'
      laType[1,2]='R'
    CASE lcPType = 'N'
      laType[1,1]='Issue Inter Location P/O'
      laType[1,2]='N'
    CASE lcPType = 'A'
      laType[1,1]='Issue Adornment P/O'
      laType[1,2]='A'
  ENDCASE
  *E301291,4 AMM end
ENDIF
lnType=1
SHOW GET lnType
DO CASE
  CASE lcPType='S'
    SHOW WINDOW (lcWinCh1) TOP
    =SEEK(lcShpCode,'SHPMTHDR')
    lnType= IIF(llImpCost AND lcCostImp='S',1,2)
    
 *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
  *CASE lcPType $ 'BT'
  CASE lcPType $ 'BTL'             
 *E301480,1 NAD (End)
 
    SHOW WINDOW (lcWinCh2) TOP
    *E301480,1 NAD (Start) Add the Inter location Po batch to the condition
    *=SEEK(IIF(lcPType='B','I','M')+lcBatch,'CTKTRCVH')
    =SEEK(IIF(lcPType='B','I',IIF(lcPType='L','N','M'))+lcBatch,'CTKTRCVH')
    *E301480,1 NAD (End)
    
    llByCarton = CTKTRCVH.Carton
    lcBDesc    = CTKTRCVH.cDesc
    lcBStatus  = 'APPROVED'
    ldBDate    = CTKTRCVH.dDate
    lnTotStk   = CTKTRCVH.nTotStk
    lnTotDam   = CTKTRCVH.nTotDam
    lnTotCan   = CTKTRCVH.nTotCan
    lnType = IIF(llMfCall OR (llImpCost AND lcCostImp='S'),2,3)
ENDCASE  
*--Read line info.
SELECT (lcTmpLine)
GO TOP
=lfReadLine(EOF())
_CUROBJ = OBJNUM(lnType)
RETURN


*!*************************************************************
*! Name    : lfUnComSess
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Check if there an uncomplete session.
*!*************************************************************
FUNCTION lfChkUnComS
PARAMETERS llFrmSetup
PRIVATE    llFondSess

llFondSess = .F.
llGoAndChk = IIF(llFrmSetup, .F., llGoAndChk)
IF (llFrmSetup AND llChkUnCom) OR !llFrmSetup
  llChkUnCom = .F.
  IF gfUnCompSession(lcUnCmPgId, lnSessNo, lcBrDtTtl )
    llFondSess   = .T.
    laScrMode    = .F.
    laScrMode[4] = .T.
    llContinue   = .T.
    llFirst      = .F.
    lcSession    = UNCMSESS.cSession
    lcCurObj     = ALLTRIM(UnCmSess.cCurrObj)
    =lfActBrow()
    IF !EMPTY(lcCurObj) AND lcCurObj = 'pbSav'
       llContinue = .T.
       DO lpSavScr
       laScrMode    = .F.
       laScrMode[1] = .T.
    ENDIF
    SHOW GETS
    =lfGetTPop()
    =lfRefresh()
    llFirst=.F.
  ELSE
    laScrMode    = .F.
    laScrMode[1] = .T.
    =lfCrtUnComp()
  ENDIF
ENDIF
=lfActBrow()
RETURN (llFondSess)


*:*************************************************************
*! Name      : lfCrUnSess
*! Developer : Timour A. K. 
*! Date      : 10/10/97               
*! Purpose   : Create/Update uncomplete session record.
*:*************************************************************
*! Call      : lfUpdVars()
*:*************************************************************
FUNCTION lfCrUnSess
PARA lcPushBt,lcUnSStat,llVarUpd
lnAlias = SELECT()
SELECT UNCMSESS
IF !SEEK('O'+lcUnCmPgId+PADR(gcUser_id,10)+lcSession)
  IF EMPTY(lcUnSStat) AND !llVarUpd
    *MAN Added OR !RLOCK()
    IF !SEEK('I'+lcUnCmPgId+PADR(gcUser_id,10)) OR !RLOCK()
      INSERT INTO UNCMSESS (Status,cUTranType,cUserId,cSession,cProgram,;
        cCurrScr,dTranDate,cTranTime,ccurrobj) VALUES ;
        ('O',lcUnCmPgId,gcUser_id,lcSession,lcAProgm,'',gdSysDate,TIME(),' ')
    ELSE
      REPLACE Status    WITH 'O',;
              cSession  WITH lcSession,;
              dTranDate WITH gdSysDate,;
              cTranTime WITH TIME(),;
              ccurrobj  WITH lcPushBt
    ENDIF
    =lfUpdVars()
    =RLOCK('UNCMSESS')
  ENDIF
ELSE
  *--Update uncomplete session.
  SELECT UNCMSESS
  IF llVarUpd
    =lfUpdVars()
  ELSE
    IF EMPTY(lcUnSStat)
      REPLACE cCurrObj WITH lcPushBt
      =lfUpdVars()
    ELSE
      *B603026,1 Comment the following lines so as not to create a new session
      *B603026,1 number
      *IF lcUnSStat='C'
      *  lcSession = gfsequence('cSession')
      *ENDIF
      *B603026,1 end
      REPLACE Status WITH lcUnSStat
      UNLOCK
      llContinue = .F.
    ENDIF
  ENDIF
ENDIF
SELECT(lnAlias)
RETURN


*!*************************************************************
*! Name      : lfUpdVars
*! Developer : Timour A. K. 
*! Date      : 10/10/97               
*! Purpose   : Function to create a string to be saved in the field
*!             UNCMSESS.mComent and UNCMSESS.mTmpFiles
*!*************************************************************
*! Call      : gfSavSess()
*!*************************************************************
FUNCTION lfUpdVars

lcFiles = "lcTmpLine," + lcTmpLine + "," + ORDER(lcTmpLine) + ";"
IF llWareLoc 
  lcFiles = "lcTmpLine," + lcTmpLine + "," + ORDER(lcTmpLine) + ";" +;
            "lcTemLoc,"  + lcTemLoc  + "," + ORDER(lcTemLoc)  + ";"
ENDIF
IF !llVarUpd AND EMPTY(lcUnSStat) AND llLinkToGl
  lcFiles = lcFiles + "lcGlDist," + lcGlDist + ", ;"
ENDIF
= gfSavSess(lcUnCmPgId, lcFiles, @laVariables,lcSession)
RETURN


*!*************************************************************
*! Name      : lfCrtUnComp
*! Developer : Timour A. K. 
*! Date      : 10/10/97               
*! Purpose   : To create all the uncompete session temprory files
*!             that is needed by this program.
*!*************************************************************
*! Call      : gfCrtTmp()
*!*************************************************************
*! Modification : *E301180,1 03/22/99
FUNCTION lfCrtUnComp

SELECT (lcMasterLn)
=AFIELDS(laFStru)
lnFStru = ALEN(laFStru,1)

*E300935,4 add new field to lcTmpLine file.
*E300935,4 this field is used in saving process to avoid update 
*E300935,4 STYDYE, FABDYE, and DYE_REL file for new lines already updated
*E300935,4 when making its type '1' records in BOMLINE.
* DIMENSION laFStru[lnFStru+11,4]

*C101424,1 Add new field save line received by [Begin]
*DIMENSION laFStru[lnFStru+13,4]
DIMENSION laFStru[lnFStru+IIF(llMFCall,14,13),4]

*C101424,1 Add new field save line received by [End  ]
*E300935,4

laFStru[lnFStru+1,1] = 'TOTSTK'
laFStru[lnFStru+2,1] = 'TOTDAM'
laFStru[lnFStru+3,1] = 'TOTCAN'  
laFStru[lnFStru+4,1] = 'TOTBAL'
laFStru[lnFStru+5,1] = 'CCARTON'
laFStru[lnFStru+6,1] = 'NLINENO'
laFStru[lnFStru+7,1] = 'LNEWLN'
laFStru[lnFStru+8,1] = 'NSTEPS'      
laFStru[lnFStru+9,1] = 'LALOCHG'
laFStru[lnFStru+10,1]= 'CLASTOPR'
laFStru[lnFStru+11,1]= 'LNEWLUPD'

*E300935,4 new field Name .
laFStru[lnFStru+12,1]= 'LCOSTMADE'
*E300935,4
*ETAK
laFStru[lnFStru+13,1]= 'LAUTOMODE'

STORE 'N' TO laFStru[lnFStru+1,2],laFStru[lnFStru+2,2],;
             laFStru[lnFStru+3,2],laFStru[lnFStru+4,2],;
             laFStru[lnFStru+6,2],laFStru[lnFStru+8,2]
STORE 'C' TO laFStru[lnFStru+5,2],laFStru[lnFStru+10,2]

STORE 'L' TO laFStru[lnFStru+7,2],laFStru[lnFStru+9,2],;
             laFStru[lnFStru+11,2],laFStru[lnFStru+13,2]

*E300935,4 new field Type .
laFStru[lnFStru+12,2]= 'L'
*E300935,4

STORE  6  TO laFStru[lnFStru+1,3],laFStru[lnFStru+2,3],;
             laFStru[lnFStru+3,3],laFStru[lnFStru+4,3],;
             laFStru[lnFStru+6,3],laFStru[lnFStru+10,3]
STORE  3  TO laFStru[lnFStru+5,3]  
STORE  2  TO laFStru[lnFStru+8,3]
STORE  1  TO laFStru[lnFStru+7,3],laFStru[lnFStru+9,3],;
             laFStru[lnFStru+11,3],laFStru[lnFStru+13,3]

*E300935,4 new field Width .
laFStru[lnFStru+12,3]= 1
*E300935,4

STORE  0  TO laFStru[lnFStru+1,4],laFStru[lnFStru+2,4],;
             laFStru[lnFStru+3,4],laFStru[lnFStru+4,4],;
             laFStru[lnFStru+5,4],laFStru[lnFStru+6,4],;
             laFStru[lnFStru+7,4],laFStru[lnFStru+8,4],;
             laFStru[lnFStru+9,4],laFStru[lnFStru+10,4],;
             laFStru[lnFStru+11,4],laFStru[lnFStru+13,4]

*E300935,4 new field decimal char. .
laFStru[lnFStru+12,4]= 0

*C101424,1 Add new field save line received by [Begin]
IF llMFCall
  laFStru[lnFStru+14,1] = 'cRcvBy'
  laFStru[lnFStru+14,2] = 'C'
  laFStru[lnFStru+14,3] = 19
  laFStru[lnFStru+14,4] = 0
ENDIF  
*C101424,1 Add new field save line received by [End  ]

*E300935,4
*E301180,1 [START]
*--Index tags array.
DIMENSION laTags[3,2]
IF llMfCall
  *E300935,4 Change index to have cWareCode field
  *E300935,4 simply I add cWareCode field after dyelot
  laTags[1,1]='TranCd+cCarton+Cuttkt+Style+Dyelot+cWareCode+STR(LineNo,6)'
  laTags[2,1]='cCarton+Cuttkt+Style+Dyelot+cWareCode+STR(LineNo,6)+TranCd'
  laTags[3,1]='Cuttkt+Style+Dyelot+cWareCode+STR(LineNo,6)+cCarton+TranCd'
  
  laTags[1,2]='TmpLine1'
  laTags[2,2]='TmpLine2'
  laTags[3,2]='TmpLine3'
ELSE
  *E301180,1 add new index for the TmpLine , this is the index required by
  *-- the report
  *E301183,1 add new index for the TmpLine , this is the index required by
  *-- the report
  DIME laTags[5,2]
  *E300935,4 Change index to have dyelot field and cWareCode field
  *E300935,4 simply I add fieldS after Style .
  laTags[1,1]='TranCd+cCarton+Po+Style+Dyelot+cWareCode+STR(LineNo,6)'
  laTags[2,1]='cCarton+PO+Style+Dyelot+cWareCode+STR(LineNo,6)+TranCd'
  laTags[3,1]='PO+Style+Dyelot+cWareCode+STR(LineNo,6)+cCarton+TranCd'
  laTags[4,1]='cstytype+po+style+STR(lineno,6)+TranCd'
  laTags[5,1]='shipno+cstytype+po+style+STR(lineno,6)+trancd'
  laTags[1,2]='TmpLine1'
  laTags[2,2]='TmpLine2'
  laTags[3,2]='TmpLine3'
  *E301180,1[BEGIN]
  laTags[4,2]='POSLN'
  *E301180,1[END..]
  *E301183,1[BEGIN]
  laTags[5,2]='POSLNSH'
  *E301183,1[END..]
ENDIF
*E301180,1 [END..]

*B039660,1 NNA 06/06/2006 (Begin) convert next trigger to be done within Binmain.Prg instead of Davmain.prg 
*C037816,1 MHM 04/06/2004 Add Custom field for BinClasses [Start]
IF ASCAN(laEvntTrig,PADR("ADDFILDS",10)) <> 0 
  =gfDoTriger("POSTREC",PADR("ADDFILDS",10))
ENDIF
*C037816,1 MHM  [End]
*B039660,1 NNA (End)

=gfCrtTmp(lcTmpLine,@laFStru,@laTags)
SELECT (lcTmpLine)
SET ORDER TO TAG TmpLine1

*C126994,1 NNA (BEGIN) Create new cursor to hold the multi bins for the style
IF ASCAN(laEvntTrig,PADR("CRTBINLN",10)) <> 0 
  =gfDoTriger("POSTREC",PADR("CRTBINLN",10))
ENDIF
*C126994,1 NNA (END)

*--Warehouse location tmp file.
IF llWareLoc 
  SELECT WhsLoc
  =AFIELDS(laFStru)
  =gfCrtTmp(lcTemLoc,@laFStru,'Style+cWareCode+cLocation',lcTemLoc)
ENDIF  
RETURN
*--End...

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/30/1998
*! Purpose   : Get color for primary fabric From BOMLINE file
*!           : which updated in Cost sheet.
*!*************************************************************
*! Called from : lpSavScr
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : Style to get its corressponding color from BOMLINE table.
*!*************************************************************
*! Return      : Color
*!*************************************************************
*! Example     : = lfGetColor()
*!*************************************************************
*E300935,4
FUNCTION lfGetColor
PARAMETERS lcStyFab
PRIVATE lcCurAlias,lcBomOrder,lcStyOrder,lcStyKey
lcCurAlias = ALIAS()

lcStyKey   = STYLE.STYLE
lcStyOrder = ORDER('STYLE')
SET ORDER TO STYLE IN STYLE

= SEEK(lcStyFab,'STYLE')
lcUpDatFab = STYLE.FABRIC

= SEEK(lcStyKey,'STYLE')
SET ORDER TO &lcStyOrder IN STYLE

*E301480,1 NAD (Start) Add the Inter location Po batch to the condition
lcSeekTyp =IIF(llMFCall,'M','I')
lcSeekTyp =IIF(llMFCall,'M',IIF(lcPType='L','N','I'))
*E301480,1 NAD (End)

lcSeekClr = lcSeekTyp+'1'+IIF(llMFCall,&lcTmpLine..Cuttkt,&lcTmpLine..PO)+;
            STR(&lcTmpLine..LineNo,6)+lcSeekBomTp+;
            lcStyFab + SPACE(6) + lcUpDatFab 

*B803337,1 WMA [Begin]
*E301077,11 MAB Open BOMLINE file here [Begin
*IF !llOpnBomLn
*  llOpnBomLn = lfOpn_Rest(gcDatadir,'BOMLINE','BOMLINE')
*ENDIF  
*E301077,11 MAB Open BOMLINE file here [End..
*B803337,1 WMA [End]

SELECT BOMLINE
lcBomOrder = ORDER('BOMLINE')
SET ORDER TO BOMLINE IN BOMLINE

= SEEK(lcSeekClr,'BOMLINE')

SET ORDER TO &lcBomOrder IN BOMLINE

SELECT (lcCurAlias)
RETURN BOMLINE.ICLR
*-- end of lfGetColor.

*!*************************************************************
*! Name      : lfUpDatFab
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/10/1998
*! Purpose   : Update FabDye File.
*!*************************************************************
*! Called from : lpSavScr
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : Color
*!*************************************************************
*! Example     : = lfUpDatFab()
*!*************************************************************
*E300935,4
FUNCTION lfUpDatFab
PARAMETERS lcFabKey,lcClrKey,lcDyeKey,lcWareKey 
PRIVATE lcCurAlias,lcDyeOrder,lcFabOrder,lcMyFabKey
lcCurAlias = ALIAS()
lcDyeOrder = ORDER('FABDYE')
SET ORDER TO FABDYE IN FABDYE

lcFabOrder = ORDER('FABRIC')
lcMyFabKey = FABRIC.FABRIC + FABRIC.COLOR 
SET ORDER TO FABRIC IN FABRIC

SELECT FABDYE
*-- if you does not find location record in fabdye file, add it.
IF llWareHous AND !SEEK(lcFabKey+lcClrKey+lcWareKey)
  DO gpAdFabWar WITH lcFabKey,lcClrKey,SPACE(10),lcWareKey
  =gfTraceKey('FABDYE',lcFabKey+lcClrKey+SPACE(10)+lcWareKey,'A')
ENDIF  && end if you does not find location record in fabdye file, add it.

IF llDyelot AND llFabDye AND SEEK(lcFabKey + lcClrKey,'FABRIC') AND ;
   FABRIC.CDYE_FLG = 'Y' AND !SEEK(lcFabKey+lcClrKey+lcWareKey+lcDyeKey,'FABDYE')
  *-- add dyelot record and save tmpscope to arrange it later.
  DO gpAdFabWar WITH lcFabKey,lcClrKey,lcDyeKey,lcWareKey,lcTmDyeRel
  =gfTraceKey('FABDYE',lcFabKey+lcClrKey+lcDyeKey+lcWareKey,'A')
ENDIF              

SET ORDER TO &lcDyeOrder IN FABDYE

= SEEK(lcMyFabKey,'FABRIC')
SET ORDER TO &lcFabOrder IN FABRIC

SELECT (lcCurAlias)
RETURN
*-- end of lfUpDatFab.

*!*************************************************************
*! Name      : lfExistWar
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/15/1998
*! Purpose   : Check existance of selected location line.
*!*************************************************************
*! Called from : lfvWare
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : .T. if line is exist before.
*!*************************************************************
*! Example     : = lfExistWar()
*!*************************************************************
*E300935,4
FUNCTION lfExistWar
lcCurrRec  = PADR(cCarton,3)+IIF(llMFCall,PADR(CutTkt,6),PADR(PO,6))+PADR(Style,19)+PADR(Dyelot,10)+PADR(cWareCode,6)+STR(LineNo,6)
lcLineUnq  = IIF(llMFCall,STR(CutTktl.LineNo,6),STR(PosLn.LineNo,6))
*-- if this record is found in temp. file before 
llExists = SEEK('1'+PADR(lcCarton,3)+PADR(lcTCode,6)+PADR(lcStyle,19)+PADR(lcDyelot,10)+;
                PADR(lcWareCode,6)+lcLineUnq)
= SEEK('1'+lcCurrRec)
RETURN llExists
*-- end of lfExistWar.

*!*************************************************************
*! Name      : lfChkLine
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/15/1998
*! Purpose   : Check existance of entered location line, then if found check existance of N/A line.
*!*************************************************************
*! Called from : lfvStyle,lfvDyelot,lfvCutTkt
*!*************************************************************
*! Calls       : lfGetInfo
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : .T. if line is exist before.
*!*************************************************************
*! Example     : = lfChkLine()
*!*************************************************************
*E300935,4
FUNCTION lfChkLine
SELECT (lcTmpLine)
llEmpWare = .F.
lcCurrRec  = PADR(cCarton,3)+IIF(llMFCall,PADR(CutTkt,6),PADR(PO,6))+PADR(Style,19)+PADR(Dyelot,10)+PADR(cWareCode,6)+STR(LineNo,6)
lcLineUnq  = IIF(llMFCall,STR(CutTktl.LineNo,6),STR(PosLn.LineNo,6))
lcWareCode = IIF(llMFCall,CutTktl.cWareCode,lcWareCode)
*-- if this record is found in temp. file before 
IF SEEK('1'+PADR(lcCarton,3)+PADR(lcTCode,6)+PADR(lcStyle,19)+PADR(lcDyelot,10)+PADR(lcWareCode,6)+lcLineUnq)
  llEmpWare = .T.
  IF SEEK('1'+PADR(lcCarton,3)+PADR(lcTCode,6)+PADR(lcStyle,19)+PADR(lcDyelot,10)+SPACE(6)+lcLineUnq)
    lnBrRecNo=RECNO()
    lnWare = 1
    =lfwBrow()
    RETURN
  ELSE
    SEEK '1' + lcCurrRec
  ENDIF  
ENDIF
=lfGetInfo(llEmpWare)

*-- end of lfChkLine.

*!*************************************************************
*! Name      : lfOpn_Rest
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/11/1999
*! Purpose   : Open file then restore current alias
*!*************************************************************
*! Calls       : gfOpenFile
*!*************************************************************
*! Passed Parameters : File Directory, File Name, File Tag
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfOpn_Rest()
*!*************************************************************
*E300935,4, B602521,1 Fix Variable cWareCode not found.
FUNCTION lfOpn_Rest
PARAMETERS lcFileDir,lcFileN,lcIndN

PRIVATE lnCurrAls,llOpenIt 
lnCurrAls = SELECT(0)
llOpenIt = gfOpenFile(lcFileDir+lcFileN,lcIndN,'SH')
SELECT (lnCurrAls)
RETURN llOpenIt
*-- end of lfOpn_Rest.

*!*************************************************************
*! Name      : lfTraceTag
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/31/1999
*! Purpose   : Trace cutpick file
*!*************************************************************
*! Calls       : gfOpenFile
*!*************************************************************
*! Passed Parameters : File Name, Communication status.
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfTraceTag()
*!*************************************************************
*E301182,7 Trace cutpick file
FUNCTION lfTraceTag
PARAMETERS lcFileNam,lcCommStat
PRIVATE lcPickVal,lnCurAlias
lnCurAlias = SELECT(0)

IF EMPTY(lcPickIndx)
  =gfOpenFile(gcSysHome+'SYDFILES','Cfile_nam','SH')
  IF SEEK(lcFileNam,'SYDFILES')
    lcPickIndx = SYDFILES.cFile_Tag
  ENDIF

  PRIVATE lnTagNo
  lnTagNo    = 1

  SELECT CUTPICK
  DO WHILE .T.
    IF ALLTRIM(TAG(lnTagNo)) = ALLTRIM(lcPickIndx)
      lcPickIndx = KEY(lnTagNo) 
      EXIT
    ENDIF
    lnTagNo = lnTagNo + 1
  ENDDO

ENDIF

lcPickVal  = EVALUATE(lcPickIndx)
=gfTraceKey(lcFileNam,lcPickVal,lcCommStat)
SELECT (lnCurAlias)
*-- end of lfTraceTag.

*!*************************************************************
*! Name      : lpGetOpen
*! Developer : WALID (WAM)
*! Date      : 03/24/1999
*! Purpose   : Calaulate prv. receive if any per each line
*!           : and brows it in the screen from which we select the
*!           : style/colors in a cutting ticket and PO
*!*************************************************************
*! Called from : lfvCuttkt & lfvPo
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpGetOpen
*!*************************************************************
*!B602698,1

PROCEDURE lpGetOpen
PRIVATE lnRecNo , lnFilePos , laSumAll , lcMastFile , lcScanCond , lcTrancd

*B606426,1 AMH 08/28/2002 Added to hold the received qty (Stock, cancel, and damage) [Start]
PRIVATE laQty
*B606426,1 AMH 08/28/2002 [End]

IF llMFCall
  lcMastFile = 'CUTTKTL'
ELSE
  lcMastFile = 'POSLN'
ENDIF  
SELECT (lcMastFile)
lnFilePos=RECNO()
DIMENSION laSumAll[3]

*B606426,1 AMH 08/28/2002 Added to hold the received qty (Stock, cancel, and damage) [Start]
DIMENSION laQty[4,8]
laQty=0
*B606426,1 AMH 08/28/2002 [End]

laSumAll=0

*------------------ CREATE TEMP. FILE -> [BEGIN]
IF !llFirstTmp &&-- do it only one time  

  DIMENSION laFileStru[1,4]
  
  =AFIELDS(laFileStru)
  DIMENSION laFileStru[ALEN(laFileStru,1) , 4] 
  CREATE TABLE &gcWorkDir.&lcTempFile FROM ARRAY laFileStru
  IF llMFCall
    INDEX ON cuttkt+style+dyelot+trancd TAG (lcTempFile)
  ELSE
    INDEX ON cstytype+po+style+STR(lineno,6)+trancd TAG (lcTempFile)
  ENDIF
  llFirstTmp=.T.
ENDIF  
*-- CREATION OF THE STRUCTURE OF THE TEMP FILE [END]

*-- If we did the same calculations befor don't do it again .
IF  lcTCode = lcTCodeOld
  RETURN
ENDIF

lcTCodeOld = lcTCode    

SELECT (lcTempFile)
SET ORDER TO TAG &lcTempFile  
*C200080,1 AMM Adjust seek expression.
*lcSeekCond = IIF(llMFCall,lcTCode,'P'+lcTCode)

*E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
*lcSeekTyp = IIF(lcPType $ 'RNODAE',IIF(lcPType='O','N',IIF(lcPType='E','A',lcPType)),'P')
lcSeekTyp = IIF(lcPType $ 'RNODAEL',IIF(lcPType $'OL','N',IIF(lcPType='E','A',lcPType)),'P')
*E301480,1 NAD (End)

lcSeekCond = IIF(llMFCall,'',lcSeekTyp)+lcTCode
*C200080,1 AMM end
IF !SEEK(lcSeekCond)

  SELECT (lcMastFile)
  lcScanCond =IIF(llMFCall,[cuttkt],[PO]) 
  
  
  SCAN REST WHILE EVALUATE(lcScanCond) = lcTCode FOR trancd = '1'
    SCATTER MEMVAR MEMO
    
    *** Calculate Received Qty
    lnRecNo= RECNO()
    IF llMFCall
      lcSubSeek= "lcTCode+M.style+M.dyelot+'2','CUTTKTL'"
    ELSE
      *C200080,1 AMM Adjust seek expression
      *lcSubSeek= "'P'+lcTCode+M.style+STR(M.lineno,6)+'2','POSLN'"
      lcSubSeek = "lcSeekTyp+lcTCode+M.style+STR(M.lineno,6)+'2','POSLN'"
      *C200080,1 AMM end
    ENDIF  
    IF SEEK(&lcSubSeek)
    
      *B606426,1 AMH Get the correct received qty [Start]
      *SCAN  FOR EVALUATE(lcScanCond) = lcTCode AND style = M.style WHILE trancd = '2'     
        *laSumAll[1]=laSumAll[1]+IIF(llMFCall,CUTTKTL.Totqty,POSLN.Totqty)
      SUM FOR EVALUATE(lcScanCond) = lcTCode AND style = M.style WHILE trancd = '2';
          QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8;
          TO laQty[1,1],laQty[1,2],laQty[1,3],laQty[1,4],laQty[1,5],laQty[1,6],laQty[1,7],laQty[1,8]
      *ENDSCAN
      *B606426,1 AMH [End]
      
    ENDIF  
    GO lnRecNo
  
    *** Calculate Damage Qty
    lnRecNo= RECNO()
    IF llMFCall
      lcSubSeek= "lcTCode+M.style+M.dyelot+'3','CUTTKTL'"
      lcTrancd ='3'
    ELSE
      *C200080,1 AMM Adjust seek expression
      *lcSubSeek= "'P'+lcTCode+M.style+STR(M.lineno,6)+'4','POSLN'"
      lcSubSeek= "lcSeekTyp+lcTCode+M.style+STR(M.lineno,6)+'4','POSLN'"
      *C200080,1 AMM end
      lcTrancd ='4'
    ENDIF  

    IF SEEK(&lcSubSeek)

     *B606426,1 AMH Get the correct received qty [Start]    
     *SCAN  FOR EVALUATE(lcScanCond) = lcTCode AND style = M.style WHILE trancd = lcTrancd 
        *laSumAll[2]=laSumAll[2]+IIF(llMFCall,CUTTKTL.Totqty,POSLN.Totqty)
      SUM FOR EVALUATE(lcScanCond) = lcTCode AND style = M.style WHILE trancd = lcTrancd;
          QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8;
          TO laQty[2,1],laQty[2,2],laQty[2,3],laQty[2,4],laQty[2,5],laQty[2,6],laQty[2,7],laQty[2,8]
      *ENDSCAN  
      *B606426,1 AMH [End]
        
    ENDIF  
    GO lnRecNo

    *** Calculate Cancel Qty
    lnRecNo= RECNO()
    IF llMFCall
      lcSubSeek= "lcTCode+M.style+M.dyelot+'4','CUTTKTL'"
      lcTrancd ='4'
    ELSE
      *C200080,1 AMM Adjust seek expression
      *lcSubSeek= "'P'+lcTCode+M.style+STR(M.lineno,6)+'5','POSLN'"
      lcSubSeek= "lcSeekTyp+lcTCode+M.style+STR(M.lineno,6)+'5','POSLN'"
      *C200080,1 AMM end
      lcTrancd ='5'      
    ENDIF  
    IF SEEK(&lcSubSeek)
     
     *B606426,1 AMH Get the correct received qty [Start]
      *SCAN  FOR EVALUATE(lcScanCond) = lcTCode AND style = M.style WHILE trancd = lcTrancd  
        *laSumAll[3]=laSumAll[3]+IIF(llMFCall,CUTTKTL.Totqty,POSLN.Totqty)
      SUM FOR EVALUATE(lcScanCond) = lcTCode AND style = M.style WHILE trancd = lcTrancd;
          QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8;
          TO laQty[3,1],laQty[3,2],laQty[3,3],laQty[3,4],laQty[3,5],laQty[3,6],laQty[3,7],laQty[3,8]
      *ENDSCAN  
      *B606426,1 AMH [End]
    
    ENDIF  
    GO lnRecNo

    *B606426,1 AMH Collecting the received quantities [Start]
    FOR lnI = 1 TO 8
      FOR lnJ = 1 TO 3
        laQty[4,lnI] = laQty[4,lnI] + laQty[lnJ,lnI]
      ENDFOR
    ENDFOR
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      laSumAll[1] = laSumAll[1] + MIN(laQty[4,lnI],EVALUATE('m.qty'+lcI))
    ENDFOR
    *B606426,1 AMH [End]
    m.Totqty = m.Totqty - laSumAll[1] - laSumAll[2] - laSumAll[3] 
    INSERT INTO (lcTempFile) FROM MEMVAR
    laSumAll=0   

    *B606426,1 AMH Reinitialize the variable [Start]
    laQty=0
    *B606426,1 AMH [End]
    
  ENDSCAN
ENDIF
SELECT (lcMastFile)
GO lnFilePos


*!*************************************************************
*! Name      : lfvStyCut
*! Developer : WALID (WAM)
*! Date      : 04/11/1999
*! Purpose   : Validate the style field when receive by style . 
*!*************************************************************
*! Called from :lfvStyle
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvStyCut()
*!*************************************************************
*!C101424,1
FUNCTION lfvStyCut
PRIVATE lnAlias,lcCuttOrd

lnAlias    = SELECT(0)
lcCuttOrd  = 'No'  && Save Cutkt header active order.

SELECT STYLE
*-- Validate style
lcSeekSty = IIF(EMPTY(SUBSTR(lcStyle,lnMjrWid+2)),PADR(lcStyle,lnMjrWid),lcStyle)
IF llbrowse OR ATC('?',lcStyle) <> 0 OR (!EMPTY(PADR(lcStyle,lnMjrWid)) .AND. !SEEK(lcSeekSty,'STYLE'))
  llbrowse = .F.

  *-- Call gfStyBrw() to brows all Majors for domestic styles.
  lcStyle = gfStyBrw('M',lcStyle,"",.F.)
  SHOW GET lcStyle

ENDIF
*--

*-- if not a valid code or user press Esc or Cancel from gfStyBrw()  
IF EMPTY(PADR(lcStyle,lnMjrWid))
    
  lcStyle=SPACE(19)
  SHOW GET lcStyle
 _CUROBJ=OBJNUM(lcStyle) 

ELSE  && not empty of lcStyle
  
  *-- if select style from Style brows 
  SELECT CutTktH
  lcCuttOrd = ORDER('CutTktH')
  SET ORDER TO Cuttkths  && Set order to style order
      
  *-- If this style major has a cut ticket call lfvCutTkt( ) to select one of them
  IF SEEK(PADR(lcStyle,lnMjrWid),'CutTktH')

    =lfvCutTkt()

  ELSE &&--  If this style has no cutt ticket 
    *-- There are no cutting tickets for this style .
    =gfModalGen('TRM38170B42001','DIALOG')
    lcStyle=SPACE(19)
    SHOW GET lcStyle
    _CUROBJ=OBJNUM(lcStyle)

  ENDIF &&-- endif * If this style has a cutt ticket 
  
ENDIF &&-- end if EMPTY(lcStyle)

IF lcCuttOrd <> 'No'
  SELECT CutTktH
  SET ORDER TO TAG &lcCuttOrd
ENDIF  
SELECT (lnAlias)
*-- end of lfvStyCut.


*!*************************************************************
*! Name      : lfSetFltr                      *B802592,1 Added. 
*! Developer : TAK
*! Date      : 20/09/1999
*! Purpose   : Build a filter array that used in ariabrow base
*!             on base file defined.
*!*************************************************************
FUNCTION lfSetFltr
PRIVATE lcBFields

IF llMFCall
  lcBFields = "CutTkt,Style,Status,Entered,Complete,Season,cDivision,Pcs_Bud,Pcs_Rec,Pcs_Dam,Pcs_Opn"
ELSE
  lcBFields = "PO,Status,Vendor,Complete,nStyOrder,POTotal,Receive,Open"
ENDIF
llOndffl = gfOpenFile(gcSysHome+'SYDFLFLD','Cfile_nam','SH')
llOndfld = gfOpenFile(gcSysHome+'SYDFIELD','Cfld_name','SH')

DIMENSION gaTmpArray[1,3]
gaTmpArray = ' '

SELECT DIST SYDFIELD.CFLD_NAME, SYDFIELD.cfld_head, SYDFLFLD.NFLD_POS;
       FROM SYDFLFLD, SYDFIELD;
      WHERE SYDFIELD.CFLD_NAME = SYDFLFLD.CFLD_NAME  .AND. ;
              sydflfld.cfile_nam == lcBaseFile       .AND. ;
              UPPER(ALLTRIM(sydfield.cfld_name)) $  UPPER(lcBFields) ;
     ORDER BY SYDFLFLD.NFLD_POS;
     INTO ARRAY gaTmpArray

SELECT SYDFIELD.CFLD_NAME,SPACE(10) ;
    FROM SYDFLFLD, SYDFIELD;
   WHERE SYDFIELD.CFLD_NAME = SYDFLFLD.CFLD_NAME   .AND. ;
           sydflfld.cfile_nam == lcBaseFile        .AND. ;
           UPPER(ALLTRIM(sydfield.cfld_name)) $  UPPER(lcBFields) .AND. ;
           !EMPTY (sydfield.mventries) ;
   INTO ARRAY laArrayes

  gnArrayln = ALEN(gaTmpArray,1)
  DECLARE laField_H [gnArrayln,1] ,laField_N [gnArrayln,1]
  
  FOR lnElem = 1 TO gnArrayln
    laField_N[lnElem,1] = ALLTRIM(gaTmpArray[lnElem,1])
    laField_H[lnElem,1] = IIF(EMPTY(gaTmpArray[lnElem,2]),;
                  ALLTRIM(gaTmpArray[lnElem,1]),ALLTRIM(gaTmpArray[lnElem,2]))
  ENDFOR

IF llOndffl AND USED('SYDFLFLD')
  USE IN sydflfld
ENDIF
IF llOndfld AND USED('SYDFIELD')
  USE IN SYDFIELD
ENDIF
RETURN


*B101704,1 [Start] added
*!**************************************************************************
*! Name      : lfPicture  
*! Developer : Sameh Aldesouki 
*! Date      : 01/17/2000
*! Purpose   : Change Picture of PO.
*!**************************************************************************
FUNCTION lfPicture

IF !llMFCall AND llGenOrNum
  RETURN "@! XXXXXX" 
ELSE
  RETURN "X99999"
ENDIF  
*B101704,1 [End]


*:*************************************************************
*! Name     : lfChekAdj
*! Developer: Ahmed Salah Shalaby - (SSH)
*! Date     : 10/10/97               
*! Purpose  : Check if adjust cost for recieve.. 
*:*************************************************************

FUNCTION lfChekAdj
PARAMETERS lcTrType,lcTket,lcItem,lcColor,lcSess,lcStyQlt
*B802902,1 SSH 03/05/2000 if we select to preview log report, 
*B802902,1 SSH            the reports show the estimated p.price along 
*B802902,1 SSH            (with each style/color being received) 
*B802902,1 SSH            instead of the landed p.price. 
PRIVATE lnOldAls , llToREt
lnOldAls = SELECT(0)
SELECT BOMLINE
lnoldKey = ORDER()
SET ORDER TO TAG BomLine
lcItem=PADR(lcItem,19)

lcSeekKey = IIF(lcTrType='S','I',lcTrType)+'2'+lcTket
lcWhleCnd = "cIMTyp+cType+cTktNo+STR(LineNo,6)=lcSeekKey"

lcForCond = "Style=lcItem AND EMPTY(cRSession)"
lcQltFltr = IIF(TYPE('lcStyQlt') $ 'UL' ,".T.","cStyGrade = lcStyQlt")

=SEEK(lcSeekKey)
LOCATE REST WHILE &lcWhleCnd FOR &lcForCond AND &lcQltFltr

*--If found type '2' Return True.
SELECT BOMLINE
SET ORDER TO lnoldKey
llToREt = FOUND()
SELECT(lnOldAls)
RETURN llToREt

*:*************************************************************
*! Name     : lfPrnLanded
*! Developer: Ahmed Salah Shalaby - (SSH)
*! Date     : 10/10/97               
*! Purpose  : Calculate landed costs case of detail costing. 
*:*************************************************************
FUNCTION lfPrnLanded
*B802902,1 SSH 03/05/2000 If we select to preview log report, 
*B802902,1 SSH            the reports show the estimated p.price along 
*B802902,1 SSH            (with each style/color being received) 
*B802902,1 SSH            instead of the landed p.price. 

*B803337,1 WMA [Begin]
*IF !llOpnBomLn
*  llOpnBomLn = lfOpn_Rest(gcDatadir,'BOMLINE','BOMLINE')
*ENDIF  
*B803337,1 WMA [End]

SELECT BOMLINE
lnOldKey = ORDER()
IF lcPType ='S'
  SET ORDER TO TAG BomLnShp
ELSE
  SET ORDER TO TAG BomLine
ENDIF
SELECT POSLN
SCATTER FIELDS Qty1,qty2,qty3,qty4,qty5,qty6,qty7,qty8 TO laLnQty

FOR lnCnt=1 to 5
  lcCnt=STR(lnCnt,1)
  *khm1
  *lcBomLKey = 'I2'+IIF(lcPType='S',POSLN.Shipno,'')+POSLN.Po+STR(POSLN.LineNo,6)+lcCnt+POSLN.Style
  lcBomLKey = IIF(lcPType='D','D','I')+'2'+IIF(lcPType='S',POSLN.Shipno,'')+POSLN.Po+STR(POSLN.LineNo,6)+lcCnt+POSLN.Style
  *khm1
  
  lcWCondtn = "cImTyp+cType+IIF(lcPType='S',ShipNo,'')+cTktNo+STR(LineNo,6)+cBomTyp+Style = lcBomLKey"
  lcFCondtn = "EMPTY(cRSession) AND cStyGrade = POSLN.cStyGrade"
  
  lnNLCs&lcCnt = 0
  lnCurSQt = 0
  SELECT BOMLINE
  IF SEEK(lcBomLKey)
    SUM REST (UnitCost*UnitQty)*lfBomSzQt() WHILE &lcWCondtn FOR &lcFCondtn TO lnNLCs&lcCnt
    lnNLCs&lcCnt = IIF(POSLN.TotQty<>0,(lnNLCs&lcCnt/POSLN.TotQty),0)
  ENDIF 
  SELECT POSLN
  REPLACE nLan_Cst&lcCnt WITH lnNLCs&lcCnt
  IF !llMFCall
    IF llMulCurr
      =lfGetEqv(lcCnt,lnRate1,lnRate2,lnCurrUnt1,lnCurrUnt2,nLan_Cst1,nLan_Cst2,nLan_Cst3,nLan_Cst4,nLan_Cst5)
      REPLACE nELanCost&lcCnt WITH laECost[1]
    ELSE
      REPLACE nELanCost&lcCnt WITH lnNLCs&lcCnt
    ENDIF
  ENDIF
ENDFOR
SELECT BOMLINE
SET ORDER TO lnOldKey
SELECT POSLN
RETURN



*:*************************************************************
*! Name     : lfChkScal
*! Developer: Ahmed Salah Shalaby - (SSH)
*! Date     : 05/05/2000
*! Purpose  : Chek if the style and recieve to cancel/damage have the same scale. 
*:*************************************************************
FUNCTION lfChkScal
PARAMETER lcCurScl,lcCurrPre
*B803251,1 SSH 04/05/2000 Prevent recive to damage/Cancel if the 
*B803251,1 SSH 04/05/2000 styles have different scales.

PRIVATE lnOldAls,lnToRet,lcOldKey,lnSizCnt
lnSizCnt = 0
lnOldAls = SELECT(0)
SELECT SCALE
lnToRet = .T.
lcOldKey = Type+Scale+PrePak
FOR lnInd = 1 TO 8
  lnSizCnt = IIF(laOrg[lnInd]>0,lnSizCnt+1,lnSizCnt)
ENDFOR
=SEEK('S'+lcCurScl+lcCurrPre)
IF lnSizCnt > cNt
  lnToRet = .F.
ENDIF
=SEEK(lcOldKey)
SELECT(lnOldAls)
RETURN lnToRet

*C102094,1 AMH [Start]
*!**************************************************************************
*! Name      : lfvPackNo
*! Developer : Ahmed Maher (AMH)
*! Date      : 01/04/2001
*! Purpose   : Valid function of Pack No User defined field
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfvPackNo()
*!**************************************************************************
FUNCTION lfvPackNo

RETURN .T.
*-- end of lfvPackNo.
*!**************************************************************************
*! Name      : lfChckScal
*! Developer : Mohamed shokry (MHM)
*! Date      : 07/19/2001
*! Purpose   : Function to check damage And secound Quality style 
*!**************************************************************************
*! Parameters: lcStyRet1 : Damage or 2nd Qualtity Style
*!             lnStyNo   : 1--> 2nd Quality 2--> Damage.
*!**************************************************************************
*! Example   :  =lfChckScal()
*!**************************************************************************
*B604677,1 
FUNCTION lfChckScal
PARAMETER lcStyRet1 , lnStyNo
PRIVATE lcCurAls , lnCurRec ,lnStyScal , lnCurRcSty

lcCurAls = SELECT()
SELECT SCALE
lnCurRec = RECNO()
SELECT STYLE
lcOldFltr = FILTER()
SET FILTER TO
lnCurRcSty = RECNO()
=SEEK(lcStyRet1 ,'STYLE')
=SEEK('S'+STYLE.Scale,'SCALE')
lnScalCnt = SCALE.CNT
SELECT SCALE
IF BETWEEN(lnCurRec,1,RECCOUNT())
  GOTO lnCurRec 
ENDIF  
IF !lfChkScal(STYLE.Scale,PrePak)
  FOR lnCount = 1 TO 8 
    IF lnCount =< lnScalCnt 
      IF lnStyNo = '1'
        SHOW GET laDam1[lnCount] ENABLE
      ELSE
        SHOW GET laDam2[lnCount] ENABLE
      ENDIF  
    ELSE
      IF lnStyNo = '1'
        SHOW GET laDam1[lnCount] DISABLE
      ELSE
        SHOW GET laDam2[lnCount] DISABLE
      ENDIF  
    ENDIF
  ENDFOR
ENDIF
SELECT STYLE
IF BETWEEN(lnCurRcSty,1,RECCOUNT())
  GOTO lnCurRcSty 
ENDIF  
SET FILTER TO &lcOldFltr 
SELECT(lcCurAls)

*!**************************************************************************
*! Name      : lfWMfScr
*! Developer : Mohamed shokry (MHM)
*! Date      : 07/22/2001
*! Purpose   : When Function for line quantity screen
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfWMfScr()
*!**************************************************************************
*B604677,1 
FUNCTION lfWMfScr

IF EMPTY(lcRetSty1)  AND EMPTY(lcRetSty2) 
  lcCurAls = SELECT()
  SELECT STYLE
  lnCurRcSty = RECNO()
  lcOldFltr = FILTER()
  SET FILTER TO  
ENDIF

IF !EMPTY(lcRetSty1) 
  =lfChckScal(lcRetSty1,'1')
ELSE
  =SEEK(m.Style,'STYLE')
  IF !EMPTY(STYLE.cretsty)    
    =lfChckScal(STYLE.cretsty,'1')
  ENDIF
ENDIF  

IF !EMPTY(lcRetSty2) 
  =lfChckScal(lcRetSty2,'2')
ELSE
  =SEEK(m.Style,'STYLE')
  IF !EMPTY(STYLE.cretsty2)
    =lfChckScal(STYLE.cretsty2,'2')
  ENDIF
ENDIF  

IF EMPTY(lcRetSty1)  AND EMPTY(lcRetSty2) 
  SELECT STYLE
  IF BETWEEN(lnCurRcSty,1,RECCOUNT())
    GOTO lnCurRcSty 
  ENDIF  
  SET FILTER TO &lcOldFltr 
  SELECT(lcCurAls)
ENDIF
*C126994,1 NNA 06/29/2005 (BEGIN) if DISBNLOC trigger is found then disable first qty. and bin location fields

*B130692,1 MHM 12/12/2005 (BEGIN) Check for using bin location
*IF ASCAN(laEvntTrig,PADR("DISBNLOC",10)) <> 0 
IF ASCAN(laEvntTrig,PADR("DISBNLOC",10)) <> 0 AND gfGetMemVar('M_DLUSEBIN')
*B130692,1 MHM 12/12/2005 (BEGIN) Check for using bin location
  *T20060809.0006   TMI [Start] 
  *FOR I = 1 TO 8
  *  SHOW GET laBinLoc[I] DISABLE
  *  SHOW GET laSok[I]    DISABLE
  *ENDFOR
  =gfDoTriger("POSTREC",'DISBNLOC')
  *T20060809.0006   TMI [End  ] 
ENDIF
*C126994,1 NNA (END)

*!**************************************************************************
*! Name      : lfCheckprd
*! Developer : Nader Nabil (NNA)
*! Date      : 01/29/2004
*! Purpose   : Function To validate the transaction date
*!**************************************************************************
*! Calls     : gfModalGen,gfOpenFile
*!**************************************************************************
*! Parameters: ldDate   : Transaction date to be check
*!			 : lcPeriod : Transaction Period
*!           : lcFYear  : Transaction Fiscal Year 
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfCheckprd()
*!**************************************************************************
*!B121330
FUNCTION lfCheckprd
PARAMETERS ldDate,lcFYear,lcPeriod

PRIVATE llContinue,lcErrorM1,lnAlias
lnAlias = SELECT()
=gfOpenFile(gcSysHome+'SYCCOMP',gcSysHome+'CCOMP_ID','SH')
=SEEK(gcPrnt_Cmp,'SYCCOMP')
=gfOpenFile(gcDataDir+'FISHD',gcDataDir+'COMPFYEAR','SH')
=gfOpenFile(gcDataDir+'FSPRD',gcDataDir+'COMFYRPRDI','SH')
llContinue = .T.
LOCATE
IF FOUND()
  LOCATE REST FOR BETWEEN(ldDate,Dfsppbgdt,Dfsppendt) WHILE (ldDate >= Dfsppbgdt)                 
ENDIF
IF !FOUND()                  && No period match checked date
  llContinue = .F.
ELSE
  &lcFYear  = Cfisfyear      && Transaction date year
  &lcPeriod = Cfspprdid      && Transaction date period     
ENDIF  
IF !llContinue             && There is an error.
  lcErrorM1  = 'This receiving date '
  lcErrorM1 =  lcErrorM1 + Dtoc(ldRcvDate) + ' does not fall within any period. '
  lcErrorM1 = lcErrorM1 + 'Would you like to continue any way ?'
  IF gfModalGen('INM00274B34001','ALERT',lcErrorM1)=1
    SELECT (lnAlias)
    RETURN(.T.)
  ELSE
    SELECT (lnAlias)
    RETURN(.F.)
  ENDIF
ENDIF
SELECT (lnAlias)
RETURN(.T.)
*****
* End of Function lfCheckPrd
*****

