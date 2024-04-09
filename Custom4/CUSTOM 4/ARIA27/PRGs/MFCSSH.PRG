***********************************************************************
*: Program file  : MfCsSh.PRG
*: Program desc. : Cutting Tickets/Style PO and Material manufacturing 
*:                 cost sheet  .
*:
*:         System: Aria Apparel System
*:      Developer: WAM - Wael Aly Mohamed
*:************************************************************************
*: Calls : 
*:         Functions  : 
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Modifications      :
*:E301037,1 HDM 11/10/1998 Change the returned QTY to be +ve in nRecieved field instead of
*:E301037,1                -ve in nIssued Field
*:B801796,1 WAM 01/11/1998 Return to program after calling other programs
*:B602148,1 HDM 03/11/1998 Make the Cut Ticket Cost sheet update the NMATWIP
*:B602148,1                in FABRIC AND FABDYE FILES
*:E301052,1 WAM 11/08/1998 Allow Issuing more than Required Quantity
*:B602188,1 WAM 11/12/1998 Enable closing for complete tickets
*:B602146,1 WAM 11/12/1998 Move actualize button to Modify operation screen
*:B602146,1                and display ticket summary before actualize
*:B602203,1 WAM 11/12/1998 Update Style WIP upon actualization
*:B602234,1 WAM 11/12/1998 Update Cost Items Required Quantities upon actualization
*:B602179,1 AMM 11/25/1998 Erase temporary files when quitting the program
*:B602269,1 WAM 11/26/1998 Update Material Inventory control as CREDIT while issue materials
*:B602286,1 WAM 11/29/1998 Check if fabric exist in the fabric file before issue
*:B602287,1 WAM 11/29/1998 Display the approperiate message
*:B602320,1 WAM 12/05/1998 Update operation sequence.
*:E301085,1 WAM 12/07/1998 Generate cost sheet automatically
*:E301088,1 WAm 12/09/1998 Display Cost sheet materials on hand.
*:B801929,1 WAM 02/02/1999 sort operation lots by operation sequence.
*:B602482,1 WAM 02/09/1999 Keep track of allocated orders while actualization
*:B602462,1 WAM 02/09/1999 Fix CUTTKTL alias reference while closing
*:B602380,1 WAM 02/09/1999 Update actualized quantity in the ticket header
*:B602380,1                file when total quantity received Actualize. 
*:E301121,4 WAM 02/15/1999 Use the material control global function
*:E301077,55 WAM 02/28/1999 Inhance openning files to speed up transaction
*:E500231,1 WAM 03/07/1999 Create new session number for evry MFG issue.
*:E301182,1 WAM 03/23/1999 Update CT/PO line number in the CUTPICK file.
*:B602712,1 WAM 03/24/1999 Keep track of allocated quantity while cancel 
*:B602712,1                ticket remaining quantity.
*:B602806,1 MAN 04/17/1999 Validate the existance of the BOM for the full style key
*:B802242,1 WAM 05/20/1999 Update balance while actualization
*:B602878,1 WAM 05/24/1999 Do not Show PO contracts, Returns or Inter-Locations
*:B602872,1 TAK 05/24/1999 The cost sheet changes the status to Open even
*:B602872,1                there is no cost sheet geneteted.
*:E301248   AHM 06/03/99 Adding new Tran. type "I" in styinvjl file
*:E301248                for issunig style from PO/CT cost sheet
*:B602966,1 TAK 06/07/1999 Wrong update for estemeted cost in P/O if BOM by size.
*:B602983   AHM 06/14/1999 missing update field ctran in file matinvl
*:B602983                  while issuing material , and wrong tran_no in gldist
*:B602983                  file.
*:E301268,1 HDM 06/21/1999 filter vendor browse by sup. type
*:B802378,1 TAK 06/30/1999 The new items does not added to the cost sheet.
*:B603025,1 TAK 06/30/1999 Fidex operend type mismatch when delete a cost sheet.
*:B603033,1 AHM 07/01/1999 Issue Fabric with the actual cost when using actual
*:B603033,1                cost option
*E301235,1 Walid Abou El-Magd 07/10/99 make all needed modifications to call
*E301235,1 this program from material manufacutre order cost sheet .             
*B802449,1 TAK 07/21/1999 Fixed wrong updating of estemated cost in P/o.
*C200080,1 AMM 07/21/1999 consider the case of a new type for Dye orders 
*B802507,1 AMM 08/05/1999 Fix the bug of not saving added items if cut ticket status is actualized.
*B802542,1 WAB 08/30/1999 Fix The bug of not update Ct line when actualised the recive qty
*E301235,4 WAB 08/30/1999 make all needed modifications to call this program from material 
*E301235,4                manufacutre order cost sheet .             
*B802577,1 WAB 09/05/1999 FiX The bug of dublicate qty in case of Dyelots 
*B802636,1 TAK 09/21/1999 Fix of changing the dyelot when issue or return other dyelot.
*B802636,1                Make the issue/return log browse to work at dyelot level. 
*B802713,1 WAM 10/15/1999 Fix syntax error in the cancel quantity contribution screen.
*B802713,1                Fix computing ticket actual & open quantity and operation detail 
*B802713,1                canceled quantity while actualization 
*B802700,1  WA 10/21/1999 Make the program reflects any modifications in operation sequence. 
*B603243,1 AMM 11/04/1999 Handle operations by dyelot
*B802785,1 AMM 11/17/1999 Reresh the status upon actualization
*B802814,1 AMM 11/21/1999 Fix the bug of variable cost not found
*B603292,1 MAN 11/23/1999 Optimize Screen Open and CT Selection.
*B802877,1 TAK 12/15/1999 Fix variable lcAcntType not found due to Wrong GL category type in Close C/T.
*B802475,1 AMM 12/22/1999 Fix the bug of not updating MATINVJL with operation and Lot #
*B603382,1 WAB 01/04/2000 Fix the bug of alias not found when close material manufactring 
*B603382,1 WAB            order cost sheet sheet
*C101704,1 SAM 01/17/2000 Allow alpha numeric PO# manualy
*B802956,1 AMM 01/23/2000 Fix bugs in actualization in dyelot case
*B802978,1 KHM 01/25/2000 Fix the bug of not replacing the changing of the
*B802978,1                description when modifying the Fabric,Trim and 
*B802978,1                Style Comp. descriptions.
*B803082,1 SSH 06/03/00 Allow enabling cont/dept even if AP not installed
*B803082,1 SSH          to do this we modify ((MFMODOPR.scx))
*B603348,1 ABD 03/20/2000 1)Update the picture of filed unitCost upon Increase width of 
*B603348,1                The filed and filed untCost & TotCost.
*B603348,1                2)Update the picture of filed untcost in screen 'mfitmdet'.
*B603348,1                3)Update the picture of filed nTotCst & nTotAcSt upon Increase 
*B603348,1                  width of The filed.
*B802630,1 SSH 10/04/00 Prevent actualize cutTkt/PO with only one Opr.

*B603504,1 ADEL 12/04/2000 Change the transaction type sent by GLDIST procedure from 'JC' FOR
*B603504,1                 closing matreial MFG to 'MC'

*B602643,1 SSH 11/04/00 Remove ceiling from requirements.
*B602779,1 SSH 09/04/00 Fix the bug Variable lcSeg not found
*B603109,1 SSH 09/04/00 Add dyelot field to the browse of Operation Manegment/Issue Item.
*B802396,1 SSH 11/04/00 Get the onhand from FabDye/StyDye in case of dyelot.
*B603335,1 SSH 11/04/00 Add dyelot to the Detail Requi. Folder.
*B603573,1 SSH 16/04/00 InCorect category key incase of linked with GL
*B603573,1 SSH 16/04/00 and and closig CutTkt/PO..... .
*B602940,1 SSH 01/05/00 Disable new button in case of style use detail costing
*B602940,1 SSH 01/05/00 no.
*B603659,1 SSH 23/05/2000 Remove the check that the system does when closing 
*B603659,1 SSH            cutting ticket that there are AP invoices issued 
*B603659,1 SSH            for all the fab.&trims included on the cost sheet
*E301427,1 SSH 31/05/20 Enhancement this program to support Piece Work
*E301427,1              Module by creating cost sheet per detail operation.
*B603718,1 SSH 24/07/2000 Fix the bug of incorrect update CutTkth when 
*B603718,1 SSH            actualize cuttkt
*B603718,1 SSH            Allow multiple recieving with actualize.
*B603840,1 RAMY 22/08/2000 In case of PO update the Gros Price when change the unit price
*B603847,1 SSH 23/08/2000 Fix of wrong updating in MFGOPRDT.nActQty field.
*B603867,1 RAMY 31/08/2000 Fix the infinite loop while actualizeing operation
*B603867,4 SSH 31/08/2000 fix bug of file PwOperate no found.
*B603862,1 SSE 09/06/2000 Fix Bug of Changing the Type for closing Cost Sheet for MF to 
*B603862,1                have a new type instead of sharing the Same Type with the PO
*B603862,1                in the SyGlTrans system file.
*B603861,1 AMH 09/11/2000 Prevent to issue the AP invoiced cost items when 
*B603861,1                selecting the Automatic issue.
*B803372,1 ABD 11/02/2000  Fix spelling of word 'Remainning' To be 'Remaining'.
*B803372,1                  Fix this Bug in Screen mfisslt2.scx.
*B803830,1 AMH 11/16/2000 Fix the bug of not updating the issued and used
*B803830,1                qty. when automatically issuing a Manufacturing 
*B803830,1                Operation.
*B604056,1 HBG 12/06/2000 Fix bug of variabels 'lcKey' & 'lnRecDetNo' not found
*C102090,1 MHM 12/11/2000 add custom popup to Jonathan Cass to display the
*C102090,1                costs per dozen.
*B604073,1 KHM 12/25/2000 Fixed the bug of wrong Material PO numbers in case
*B604073,1                of using Lot as a costing method.
*C102095,1 AMH 01/01/2001 Add a customizations and trigger for Cathy Daniels.
*B604136,1 KHM 22/01/2001 Fix the bug of fabric not found in case of Ma not installed
*B804081,1 KHM 04/04/2001 Fix the bug of changing the budget qty in case of
*B804081,1                actualizing the qty with less than the budget.
*B604294,1 KHM 04/30/2001 Fix the bug of not re-calculating the duty when
*B604294,1                adding a new dutable cost element.
*B604623,1 AMH 07/05/2001 Add marker field to C/T cost sheet.
*B604641,1 SSH Use the module variable instead of installed modules variable.
*B804351,1 AMH 08/08/2001 Fixing the bug of wrong calculation of duty cost elements if
*B804351,1                there is more than one duty cost element in the PO cost sheet
*B804351,1                and all other cost element are dutable and the duty percentage
*B804351,1                is zero.
*B604790,1 AMH 08/14/2001 Fix the bug of not updating the est. cost in posln
*B604790,1                if there are more than one transaction
*B604818,1 AMH 08/15/2001 Fix the bug of not updating the rest of the styles
*B604818,1                when modifying the unit cost of a cost elements 
*B604818,1                that is exist for more than one style.
*B604861,1 AMH 09/03/2001 Don't Update Wastage field form PDM data.
*B604899,1 AMH 09/11/2001 Update custom fields for Cathy Daniels from PDMMARK file.
*B604974,1 KHM 09/30/2001 Fix the bug of not replacing the Req_Qtyn.
*B804464,1 KHM 10/17/2001 Fix the bug of not getting the right cost when
*B804464,1                issue a fabric that does not have a stock in 
*B804464,1                average costing method.
*B603625,1 AMH 10/24/2001 Fix the bug of the new cost item screen in the dye 
*B603625,1                order cost sheet by display cost elements of 
*B603625,1                Imported Styles instead of Manufacturing Styles
*B603790,1 AMH 10/29/2001 Fix the bug of not updating the operation's open quantity correctly
*B603790,1                and not enabling the issue lot button correctly after
*B603790,1                click the zoomout botton.
*B605224,1 KHM 12/09/2001 Fix the bug of wrong updating the C/T line Qty when
*B605224,1                actualizing a lot for multi-lot cutting tickets.
*C200255,1 ADEL 23/12/01 Update MATINVJL with custom Voucher no for CON10.
*B605240,1 KHM 12/25/2001 Fix the bug of not taking care of the case of 
*B605240,1                style cost sheet by size.
*C200256,1 ADEL 27/12/01 Print Issued material Qty for CON10.
*B605267,1 AMH 01/02/2002 Fix the bug of not issuing style component from different location.
*B605320,1 AMH 01/07/2002 Fix the bug of browsing the Po when using the magnified button.
*B605270,1 KHM 01/08/2001 Fix the bug of not zero out the actual qty when
*B605270,1                deleting the cost sheet.
*B605415,1 AMH 01/07/2002 Fix the bug of variable cost not found.
*B605427,1 KHM 01/28/2002 Fix the bug of not return the style components
*B605427,1                when delete the cost sheet.
*B605480,1 KHM 02/14/2002 Fix the bug of not replacing the nfActCost in the POSHDR
*B605481,1 KHM/SSH 02/14/2002 Changing the replacement of Actualize field
*B605481,1                    in the BOMCOST to be "N" instead of "Y" when 
*B605481,1                    issue Fabric or Style Comp.
*B605607,1 AMH 02/27/2002 Add a trigger for Cathy Daniels to update the Actulized C/T
*B605607,1 AMH            in the old Sstem.
*B605677,1 KHM 03/19/2002 Fix the bug wrong calculating the required qty.
*B605677,1                in case of style cost sheet by size.
*B605726,1 KHM 03/27/2002 Fix the bug of not displaying the operation by
*B605726,1                sequence in case of PW installed.
*B605380,1 AMH 04/03/2002 Fix -ev value of used quantity in issue log screen for style com.
*B605994,1 KHM 05/26/2002 Increase the width of nRecNo.
*B606028,1 AMH 06/03/2002 Correct update nmatwip in fabric & fabdye files when close.
*B606030,1 AMH 06/05/2002 Fix bug of file does not exist when open Notepad screen.
*E301897,1 AMH 06/19/2002 Add new botton in the issue log screen to edit used quantity.
*B606205,1 KHM 07/03/2002 Fix the bug of displaying all the locations even if they were not
*B606205,1                marked to be a style finished goods inventory for styles.
*B606205,1                And the same for the material.
*B606314,1 KHM 07/28/2002 The date of the receive to cancel in the transactions line does
*B606314,1                not get updated with the closing date.
*B606318,1 KHM 07/30/2002 Fix the bug of wrong updating the PO&CT in case of the ware house
*B606318,1 KHM            of the original lines is different than the ware house of the 
*B606318,1 KHM            receiving lines.
*B606340,1 AMH 08/06/2002 Fix the bug of negative quantity in C/T when actualize it.
*B604856,1 AMH 08/22/2002 Fix the bug of wronge canceled quantity when closing M.M.O.
*E302004,1 AMH 10/07/2002 Ability to Actu. C/T with one Oper and allow to change the received
*E302004,1                quantities of operation.
*B606285,1 ABD 10/08/2002 Fix many bugs at the CT and gfmatrcl.
*B606536,1 KHM 10/14/2002 Getting the information of the issue log from the BOMCOST in case
*B606536,1                of Average & Standard
*B606616,1 KHM 11/19/2002 Fix the bug of not getting the unit cost for the correct sizes
*B606643,1 AMH            Fixed under bug # 606800
*C200444,1 ALB 12/23/2002 Add GL entries when recieving Non GL operations (Customization for UK)
*C200449,1 ALB 12/25/2002 Add GL entries when closing Non GL operations (Customization for UK)
*B606782,1 KHM 12/31/2002 Fix the problem of wrong updating the rolls balance quantity if the
*B606782,1                issued quantity is greater than the roll's balance.
*C200477,1 AMH 01/27/2003 Add triger to issue/return rolls for Dare Bare.
*B606621,1 AMH 02/04/2003 Fix bug in menu bars.
*E302010,1 ALB 02/09/2003 Auto Contribute the non contribute lines in the AP invoice
*B607209,1 KHM 05/27/2003 Fix the bug of opening the CUTTKTH & CUTTKTL while MF not installed.
*B607271,1 KHM 06/04/2003 Fix the bug of not displaying the correct warehouse when issuing
*B607271,1                the fabric and not saving the dyelot in case of not selecting a 
*B607271,1                dyelot from the beginning of creating the PO/ and C/T cost sheet.
*B607089,1 AMH 08/11/2003 Fix bug of cwarecode field not update in the ctktbom file
*B607089,1 AMH            in case of generate automatic cost sheet.
*B607465,1 AMH 08/14/2003 Fix bug of incorrect fabric stock when cancel PO cost Sheet.
*B607421,1 AMH 08/19/2003 Fix bug of cannot generate cost sheet while item cost sheet if found.
*B607340,1 AMH 08/28/2003 Fix bug When open both of PO cost sheet and style cost sheet screens.
*B037268,1 ABD 12/09/2003 Amendment to PO cost sheet 'issue/return' to allow return 
*B037268,1 ABD            of trims to a different warehouse.
*B121190,1 NNA 04/19/2004 Fix bug of that if the user have a read only Access but he still can close the PO
*C037960,1 MHM 07/01/2004 Custom Purchase Order Cost Sheet generation based on the sales order cost sheet
*B123168,1 NNA 08/01/2004 Fix bug that if we generate PO from sales order and generate each one
*B123168,1 NNA            after another and that by remain in the program after a Certain Point
*B123168,1 NNA            we will get massage [Too many Files open]
*B123774,1 MHM 10/12/2004 Enhance Speed of openning (ISSUE/RETURN) screen 
*B124929,1 NNA 10/26/2004 Fix bug in the MFITMDED.SCX that when you can modify the unit Qty and the 
*B124929,1 NNA            Pieces Qty. to be Zero and when you do that you'll get error 'Numeric Overflow'
*B124619,1 NNA 11/03/2004 Fix bug that when you make a PO and PO cost Sheet then Received all Qty.
*B124619,1 NNA             As cancelled ,in this case you can't Delete the PO Cost sheet
*C123853,1 MHM 01/15/2005 modification in Custom Bin Location For David Luke
*B126396,1 NNA 03/31/2005 Fix bug that In the material manufcaturing order cost sheet, when you
*B126396,1 NNA            issue operation in the operation management folder then receive it you
*B126396,1 NNA            will not be able to enter decimals, while in the receiving material
*B126396,1 NNA            manufacturing order you are able to enter decimals
*C123847,1 TMI 04/05/2005 change the browse such that to Calculate the onhand qty taking in account the picked qty for DIR03
*B128469,1 EIH 11/13/2005 Fix bug that Can not close cutting tickets to user which have closing C/T sub process.     
*B039660,1 NNA 06/06/2006 Convert CP#123853 (Bin Location) to be used within Binmain.Prg.
*B608010,1 TMI 03/21/2007 1)Sending the lnTranCost new parameter, 
*B608010,1                2)if return cost is changed than the issue cost in standard costing method then add gl-entry with the difference
*B608125,1 TMI 06/14/2007 Fix a bug that if a style is entred twice in a PO then the percent cost elemets are calculated wrong (T20070122.0005)
*B608415,1 MMT 01/27/2008 Fix bug of Cost value when user press on actual cost button[T20080110.0014]
*:**************************************************************************************
*E301085,1 Add new parameter to generate cost sheet automatically
*PARAMETERS lcTranType,lcTranNo
*--- Doc. [Begin]
*--- lcTranType:It's the 1St param. pass to this PRG with the following Type.
*--- 'M' ==> 'CutTkt'
*--- 'I' ==> 'Purchase Order'
*--- 'T' ==> 'Material Manufucturing Order'
*--- 'D' ==> 'Dye Order Cost Sheet'
*--- The last one 'D' is change to 'I' in the program
*--- llAutoGen : Third param to generate Cost sheet Auto. incase of called
*---             from cuttkt screen/PO... .
*---
*--- Program Notes.
*---
*--- Throught this prog. you wil be able to describe preifely each cost item
*--- included in this cuctkt , only Hold status are allowed to generate New
*--- c/t . The Style should also has Cost sheet else the prog. will not 
*--- allowing you to generate C/T cost sheet.
*--- i.e. descripe each item needed to produce this style/Styles.
*--- We have exactly 5 cost item:
*--- Ex.  Fabric/Trim/MFG Code/Style Comp./Misc.
*--- You can change the caption of this item from system manager.
*--- after you create cost sheet from generate cost sheet button in toolbar
*--- the Folder 'Cost Sheet' will dispay all cost item included in this 
*--- cost sheet with the ability to New (Add)/Remove/Modify only these
*--- three butons are enabled till you save this Cost sheet.
*--- when saving the system will ask you if you want to issue first operation
*--- and its cost item (Cost item nedded for this operation Ex. Fabric..)
*--- After saving and in addition to the last three buttons four buttons
*--- will be enabled
*--- Void   : Void cost element (It defferant from remove since
*---          remove you will not be able to call it again
*---          unless you add new one, But here you can easly unvoid it)
*--- Unvoid : Unvoid Cost Element (Only voided one)
*--- Automatic Issue : Issue all cost item (As you select)
*--- Issue/Return    : Issue / Return the cost item from the browse.
*--- The second folder is 'Operation Managments'
*--- You can issue MFG Code from this folder 
*--- we have to Distinguish Issue operation and ite cost item
*--- from this folder you can issud operation <Issue Lot>
*--- or issue its cost item from <Auto. Issue> OR 
*--- issue the operation itself if you are not linked with AP.
*--- if link with AP you must issue its cost from AP.
*--- cost from <Issue Item>.
*--- the third folder is 'Dateil Req.'
*--- Browse all cost item in detail
*--- Summary Folder 
*--- display all Estimated/Actual/Landed cost.
*--- You can not delete If 
*--- 1- some item has been receiving
*--- 2- CutTkt is closed.
*--- Doc. [End]

PARAMETERS lcTranType,lcTranNo,llAutoGen

EXTERNAL ARRAY laData,laKeyField
*C101704,1 [Start] Added one row in laSetups array.
*DECLARE lafolders[4,2],lafoldwinds[4,2],laSetups[17,2],laItemSeg[1],;
        laMfgRFld[7,2],laBrowArr[1],laSort[4,2],laBrowse[3],laStyWare[1,2],;
        laMatWare[1,2]
DECLARE lafolders[4,2],lafoldwinds[4,2],laSetups[18,2],laItemSeg[1],;
        laMfgRFld[7,2],laBrowArr[1],laSort[4,2],laBrowse[3],laStyWare[1,2],;
        laMatWare[1,2]
*C101704,1 [End]
STORE ''  TO lcWinCh0,lcWinCh1,lcWinCh11,lcWinCh12,lcWinCh2,lcWinCh21,;
             lcWinCh22,lcWinCh23,lcWinCh24,lcWinCh25,lcWinCh3,lcWinCh31,lcWinCh32,lcWinCh4
STORE ' ' TO lcLinkCode,laItemSeg,lcStatus,lcSession,lcMfgGlAcnt,laBrowArr,;
             laStyWare,laMatWare
STORE 0   TO lnAveEst,lnAveLnd,lnAveAct,lnLeadTime,lnOprBud,lnOprRec,;
             lnOprCan,lnOprDam,lnClrPos
*B606616,1 KHM 11/19/2002 (Begin) Initialize lnSizePos, lnSizeLen to get the scale position
*B606616,1                in case of extened size scale and get the extended size scale setting.
STORE 0 TO lnSizePos, lnSizeLen 
llExtSizSc = gfGetMemVar('M_USEEXSSC',gcAct_Comp)
*B606616,1 KHM 11/19/2002 (End)

*B603790,1 AMH Add new variable to calculate the open qty [Start]
STORE 0   TO lnOprOpn
*B603790,1 AMH [End]

STORE 1   TO lnActFolder,lnMarker,lnDMarker,lnStyLngth,lnTkStWare,lnTkMtWare,;
             lnTMarker,lnBMarker,lnIMarker,rbSummary,lnRMarker
STORE ''  TO lcTktText,lcFolder,lcScFields,lcMjrHdr,lcMjrMsk,lcItmHdr,;
             lcItmMsk,lcNMjrMsk,lcClrMsk,lcTranFile,lcFileKey
STORE ''  TO lcBrowseTl,lcWSheet,lcDetTitle,lcGlYear,lcGlPeriod,lcOprHdTit,;
             lcOprDtTit,lcLogTit,lcFirstOpr,lcDistrbTit,lcIssLog,lcRollTit,;
             lcTmpTkt,lcAsRollTit,lcConCnTit,lcTmpCutPik
             
STORE ''  TO lcContCode,lcContName,lcOperSeq,lcNewTit,lcModTit,lcRcvTit,lcIssLtTit
STORE ''  TO lcGlDTemp,lcTktSheet,lcDetFile,lcOpenLots,lcOprHdr,lcisslogfile,;
             lcOprDet,lcLotsDet,lcRcvFile,lcIssLtFile,lcOprDet1,;
             lcTmpBom,lcTmpStyle,lcTmpFbric
STORE ''  TO laMfgRFld,lcOldValue,laSort,lcIndExp,lcBaseFild,lcBaseTit,lcpusort
STORE .F. TO llNoShow,llBrowse,llInHouse,llMfgOpr,llZoom,llIssFirst,;
             cbSum1,cbSum2,cbSum3,cbSum4,cbPerSize,llUseDyelot,llStyCost,llMatCost

*E302004,1 AMH Add new variable to know if C/T,PO or MFG order with one oper. or not [Start]
STORE .F. TO llOnlyOne
*E302004,1 AMH [End]

STORE '/' TO lcPExSign,lcDExSign,lcPUntSin,lcDUntSin
laDefProc[7]  = .F.     && Delete procedure(lpDelScr)
laDefProc[9]  = .F.     && Save procedure(lpSavScr)
laDefProc[10] = .F.     && close procedure(lpClsScr)
*C101704,1 [Start] Initialize a variable that determeine if the PO is 
*C101704,1         entered manually or automatically
llGenOrNum = .F.
*C101704,1 [End]
*C200080,1 AMM
lcDyePO = ' '
*C200080,1 AMM end
*E301427,1 SSH 31/05/20 Enhancement this program to support Piece Work
*--- We wil support this program to deal with PW module , if this MFg oper.
*--- is consider as operation and has detail operation we will 
*--- 1- update Pw file [PWCTKTBOM]
*--- 2- Disable Recieve Butt. in the folder Contractor management.
*--- All the last modification will be applied only if PW module installed.
PRIVATE llPwInst
llPwInst  = .F.
llFromCtk = .T.   && Variable to indicate if we are calling detail operation
                  && Screen from this program.
lcPWCtkBom = ''   && Pw Cost sheet temp file.
lcDetLin   = ''   && Pw detail oeration temp file.

*B605726,1 KHM 03/27/2002 (Begin) Adding a new variable to hold the 2nd index
lcDetLin2  = ''
*B605726,1 KHM 03/27/2002 (End)
*E301427,1 SSH [END]

*C102094,1 AMH Add Flag for opening select window [Start].
*C102094,1 AMH This flag will be set to .T. when calling the custom trigger
*C102094,1 AMH for Cathy Daneil. Otherwise it will be .F.
llCloseCs = .F.
llOpenCs = .F.
*C102094,1 AMH [End]

*C200255,1 (Begin) Initialize voucher no
llFrstTime = .F.
*C200255,1 (End)

*B604623,1 AMH Define lcOldMrker to hold the marker old value [Start]
STORE SPACE(10) TO lcOldMrker
*B604623,1 AMH [End]

IF !gfSetup()
  RETURN
ENDIF
*C200080,1 AMM
lcDyePO    = IIF(lcTranType ='D','D',lcDyePO)
*khm1 12/02/2002 *
*lcTranType = IIF(lcTranType = 'D','I',lcTranType)
*khm1*

*C200080,1 AMM end

DO CASE 
  CASE lcTranType = 'M'
    DECLARE laKeyField[1,4]
    laKeyField[1,1] = 'laData[1]'
    laKeyField[1,2] = .T.
    laKeyField[1,3] = 'CUTTKTH'
    laKeyField[1,4] = 1
  *khm1
  *CASE lcTranType = 'I'
  CASE lcTranType $ 'ID'
  *khm1*
    DECLARE laKeyField[2,4]
    laKeyField[1,1] = 'laData[2]'
    laKeyField[1,2] = .F.
    laKeyField[1,3] = 'POSHDR'
    laKeyField[1,4] = 1
    laKeyField[2,1] = 'laData[1]'
    laKeyField[2,2] = .T.
    laKeyField[2,3] = 'POSHDR'
    laKeyField[2,4] = 2
*E#301235,1    
  CASE lcTranType = 'T'
    DECLARE laKeyField[1,4]
    laKeyField[1,1] = 'laData[1]'
    laKeyField[1,2] = .T.
    laKeyField[1,3] = 'MMFGORDH'
    laKeyField[1,4] = 1
ENDCASE
*B602482,1 Edit allocation browse title
lcEditAlo   = 'Allocated order lines'
*B602482,1 (End)

lcWSheet    = 'Cost Sheet Details'
lcDetTitle  = 'Detail Requirements'
lcDistrbTit = 'Contribute Item Detail'
lcLotTitle  = 'Fabrics Open Lots'
lcOprHdTit  = 'Manufacturing Operations'
lcOprDtTit  = 'Operation Lots'
lcBrZoom    = 'Zoom Operation Lots'
lcNewTit    = 'New Lot Details'
lcModTit    = 'Lot Details'
lcRcvTit    = 'Open Lots'
lcIssLtTit  = 'Issue Cost Items'
lcLogTit    = 'Issued Cost Items'
lcIssLog    = 'Items Issue Log'
lcConCnTit  = 'Cancelled Quantity'
lcRollTit   = 'Material Available Rolls'
lcAsRollTit = 'Assigned Rolls'
lafolders[1,1] = 'Cost Sheet'
lafolders[1,2] = 1
lafolders[2,1] = 'Operation Management'
lafolders[2,2] = 2
lafolders[3,1] = 'Detail Requirements'
lafolders[3,2] = 3
lafolders[4,1] = 'Summary'
lafolders[4,2] = 4
lnnofld = 4
lcwfoldchng = '=lfActFolder()'  && function to control shows after change the folder
lnfoldercend = 102.00
lnfolderrend = 2.000
IF !WEXIST(gcBaseWind)
  *E301427,1 SSH 31/05/20 Enhancement this program to support Piece Work
  *B604641,1 SSH Use the module variable instead of installed modules variable.
  *llPwInst = (OCCURS('PW',gcCmpModules)<>0)
  llPwInst = (OCCURS('PW',gcComp_Mdl)<>0)
  *B604641,1 SSH [End]
  *B603867,4 SSH 31/08/2000 [Start Commented Out
  *llPwInst = .T.
  *B603867,4 SSH End]
  llFromCtk = .T.
  *--- There is no need to create temp files if Pw not Installed or the 
  *--- current module is not Mf
  IF llPWInst .AND. lcTranType = "M"
    lcPWCtkBom = gfTempName()
    lcDetLin   = gfTempName()

    *B605726,1 KHM 03/27/2002 (Begin) Temporay variable to hold 2nd index
    lcDetLin2  = gfTempName()
    *B605726,1 KHM 03/27/2002 (End)
    
  ENDIF
  *E301427,1 SSH 31/05/20 [END]

  lcBaseFild = lcBrFields
  lcBaseTit  = lcFile_Ttl
  laMfgRFld[1,1] = 'CCONTCODE'
  laMfgRFld[1,2] = 'lcContCode'
  laMfgRFld[2,1] = 'CCONTNAME'
  laMfgRFld[2,2] = 'lcContName'
  laMfgRFld[3,1] = 'COPERSEQ'
  laMfgRFld[3,2] = 'lcOperSeq'
  laMfgRFld[4,1] = 'LINHOUSE'
  laMfgRFld[4,2] = 'llInHouse'
  laMfgRFld[5,1] = 'LMFGOPR'
  laMfgRFld[5,2] = 'llMfgOpr'
  laMfgRFld[6,1] = 'LEADTIME'
  laMfgRFld[6,2] = 'lnLeadTime'
  laMfgRFld[7,1] = 'GLACCOUNT'
  laMfgRFld[7,2] = 'lcMfgGlAcnt'
  *B602779,1 SSH [Begin] Check item code structure.
  IF !lfCheckMaj()
    *--Item structure not found, Cannot Proceed.
    =gfModalGen('QRM42080B42001','DIALOG','Item structure not found')
    glQuitting = .T.
    RETURN
  ENDIF
  *B602779,1 SSH [End]
  lcSession = gfsequence('GLSESSION')
  =gfItemMask(@laItemSeg)
  FOR lnCount = 1 TO ALEN(laItemSeg,1)
    IF laItemSeg[lnCount,1]='C'
      lcClrMsk = laItemSeg[lnCount,3]
      lnClrPos = laItemSeg[lnCount,4]
    ENDIF

    *B606616,1 KHM 11/19/2002 (Begin) Getting the scale position.
    *--Check for existance of color segment in style structure.
    IF llExtSizSc .AND. laItemSeg[lnCount,1] = 'S'
      *--Get the size length and width.
      lnSizePos = laItemSeg[lnCount,4]
      lnSizeLen = LEN(laItemSeg[lnCount,3])
    ENDIF
    *B606616,1 KHM 11/19/2002 (End)

  ENDFOR
  DO CASE 
    CASE lcTranType = 'M'
      lcMjrHdr    = gfItemMask("HM")
      lcMjrMsk    = gfItemMask("PM")
      lcItmHdr    = gfItemMask("HI")
      lcItmMsk    = gfItemMask("PI")
      lcNMjrMsk   = gfItemMask("PN")
      lcTktText  = 'Cutting Ticket#'
      lcScFields = 'CUTTKT,STYLE,STATUS,ENTERED,COMPLETE,PCS_BUD,PCS_REC,'+;
                   'PCS_CAN,PCS_DAM,PCS_OPN,CWARECODE,NEST_COST1,NEST_COST2,'+;
                   'NEST_COST3,NEST_COST4,NEST_COST5,NLAN_COST1,NLAN_COST2,'+;
                   'NLAN_COST3,NLAN_COST4,NLAN_COST5,NACT_COST1,NACT_COST2,'+;
                   'NACT_COST3,NACT_COST4,NACT_COST5,PCS_ACT,CMULTILOT,LINK_CODE,'+;
                   'CLASTOPR,CITEMWARE,CMATWARE'
      lcTranFile = 'CutTktL'
      lcFileKey  = 'CutTkt'
      SELECT CutTktH
      SCATTER FIELDS &lcScFields TO laData BLANK
    *khm1*
    *CASE lcTranType = 'I'
    CASE lcTranType $ 'ID'
    *khm1*
      lcMjrHdr    = gfItemMask("HM")
      lcMjrMsk    = gfItemMask("PM")
      lcItmHdr    = gfItemMask("HI")
      lcItmMsk    = gfItemMask("PI")
      lcNMjrMsk   = gfItemMask("PN")
      *C200080,1 AMM
      *lcTktText  = 'Purchase Order#'
      lcTktText  = IIF(EMPTY(lcDyePO),'Purchase Order#','Dye Order#')
      *C200080,1 AMM end
      lcScFields = 'PO,CSTYTYPE,Status,Entered,Complete,NSTYORDER,Receive,'+;
                   'Cancel,Damage,Open,cWareCode,NICOST1,NICOST2,NICOST3,'+;
                   'NICOST4,NICOST5,NLAN_COST1,NLAN_COST2,NLAN_COST3,'+;
                   'NLAN_COST4,NLAN_COST5,NACT_COST1,NACT_COST2,NACT_COST3,'+;
                   'NACT_COST4,NACT_COST5,PCS_ACT,CMULTILOT,LINK_CODE,'+;
                   'CLASTOPR,CITEMWARE,CMATWARE'
      lcTranFile = 'PosLn'
      lcFileKey  = 'cSTyType+Po'
      SELECT POSHDR
      SCATTER FIELDS &lcScFields TO laData BLANK
    CASE lcTranType = 'T'
      lcTktText  = 'MFG Order#'
      *E#301235,1 Removed PCS_ACT, Changed CITEMWARE TO CMATWARE
      *E301235,4 WAB - add Fields Pcs_act 
      *E301235,4 WAB - START
      *lcScFields = 'cMfgOrdNo,cFabric,Status,Entered,Complete,nMMgBudget,Received,'+;
                   'Canceled,Damaged,nMMfgOpen,cWareCode,NEST_COST1,NEST_COST2,'+;
                   'NEST_COST3,NEST_COST4,NEST_COST4,NLAN_COST1,NLAN_COST2,'+;
                   'NLAN_COST3,NLAN_COST4,NLAN_COST4,NACT_COST1,NACT_COST2,'+;
                   'NACT_COST3,NACT_COST4,NACT_COST4,CMULTILOT,CMULTILOT,'+;
                   'LINK_CODE,CLASTOPR,CMATWARE,CMATWARE'
      lcScFields = 'cMfgOrdNo,cFabric,Status,Entered,Complete,nMMgBudget,Received,'+;
                   'Canceled,Damaged,nMMfgOpen,cWareCode,NEST_COST1,NEST_COST2,'+;
                   'NEST_COST3,NEST_COST4,NEST_COST4,NLAN_COST1,NLAN_COST2,'+;
                   'NLAN_COST3,NLAN_COST4,NLAN_COST4,NACT_COST1,NACT_COST2,'+;
                   'NACT_COST3,NACT_COST4,NACT_COST4,PCS_ACT,CMULTILOT,'+;
                   'LINK_CODE,CLASTOPR,CMATWARE,CMATWARE'
      *E301235,4 WAB - END
      
      lcTranFile = 'MMfgOrdD'
      lcFileKey  = 'cMfgOrdNo'
      SELECT MMFGORDH
      SCATTER FIELDS &lcScFields TO laData BLANK
  ENDCASE
  laSort[1,1] = 'COPRCODE'
  laSort[1,2] = 'Operation'
  laSort[2,1] = 'CCONTCODE'
  laSort[2,2] = 'Cont./Dept.'
  laSort[3,1] = 'CLOTNO'
  laSort[3,2] = 'Lot#'
  laSort[4,1] = 'ITEM'
  laSort[4,2] = IIF(lcTranType='T','Fabric/Color',lcItmHdr)
  
  DEFINE POPUP lcpusort MARGIN MOVER RELATIVE SCROLL MARK CHR(16)
  DEFINE BAR 1 OF lcpusort PROMPT laSort[1,2]
  DEFINE BAR 2 OF lcpusort PROMPT laSort[2,2]
  DEFINE BAR 3 OF lcpusort PROMPT laSort[3,2]
  DEFINE BAR 4 OF lcpusort PROMPT laSort[4,2]
  laSetups[1,1]  = 'M_LINK_GL'
  laSetups[2,1]  = 'M_WAREHOUSE'
  *C200080,1 AMM (Start) Get cost elements of manufactoring styles in case of dye order
  *laSetups[3,1]  = 'M_C'+lcTranType+'SLBL1'
  *laSetups[4,1]  = 'M_C'+lcTranType+'SLBL2'
  *laSetups[5,1]  = 'M_C'+lcTranType+'SLBL3'
  *laSetups[6,1]  = 'M_C'+lcTranType+'SLBL4'
  *laSetups[7,1]  = IIF(lcTranType='T','','M_C'+lcTranType+'SLBL5')
  laSetups[3,1]  = 'M_C'+IIF(lcDyePO="D","M",lcTranType)+'SLBL1' 
  laSetups[4,1]  = 'M_C'+IIF(lcDyePO="D","M",lcTranType)+'SLBL2'
  laSetups[5,1]  = 'M_C'+IIF(lcDyePO="D","M",lcTranType)+'SLBL3'
  laSetups[6,1]  = 'M_C'+IIF(lcDyePO="D","M",lcTranType)+'SLBL4'
  laSetups[7,1]  = IIF(lcTranType='T','','M_C'+IIF(lcDyePO="D","M",lcTranType)+'SLBL5')
  *C200080,1 AMM End
  
  laSetups[8,1]  = 'M_DYELOT'
  laSetups[9,1]  = 'M_MATCSTMTH'
  laSetups[10,1] = 'M_COST_METH'
  laSetups[11,1] = 'M_TRKROLLS'
  laSetups[12,1] = 'M_MATDYE'
  *C200080,1 AMM Get cost elements of manufactoring styles in case of dye order
  *laSetups[13,1]  = 'M_C'+lcTranType+'TYPE1'
  *laSetups[14,1]  = 'M_C'+lcTranType+'TYPE2'
  *laSetups[15,1]  = 'M_C'+lcTranType+'TYPE3'
  *laSetups[16,1]  = 'M_C'+lcTranType+'TYPE4'
  *laSetups[17,1]  = IIF(lcTranType='T','','M_C'+lcTranType+'TYPE5')
  laSetups[13,1]  = 'M_C'+IIF(lcDyePO="D","M",lcTranType)+'TYPE1'
  laSetups[14,1]  = 'M_C'+IIF(lcDyePO="D","M",lcTranType)+'TYPE2'
  laSetups[15,1]  = 'M_C'+IIF(lcDyePO="D","M",lcTranType)+'TYPE3'
  laSetups[16,1]  = 'M_C'+IIF(lcDyePO="D","M",lcTranType)+'TYPE4'
  laSetups[17,1]  = IIF(lcTranType='T','','M_C'+IIF(lcDyePO="D","M",lcTranType)+'TYPE5')
  *C101704,1 [Start] Get the value of the variable that says if the user 
  *C101704,1         wants to enter the PO number manually or not
  laSetups[18,1] = 'M_GenStOrN'
  *C101704,1 [End]
  *C200080,1 AMM End
  =gfGetMemVar(@laSetups,gcAct_Comp)
  lcTypes=laSetups[13,2]+laSetups[14,2]+laSetups[15,2]+laSetups[16,2]+laSetups[17,2]

  *C101704,1 [Start] Get the value of the variable that says if the user 
  *C101704,1         wants to enter the PO number manually or not
  llGenOrNum = (UPPER(ALLTRIM(laSetups[18,2])) ='Y') 
  *C101704,1 [End]  
  llStyCost = ('S' $ lcTypes)
  llMatCost = ('F' $ lcTypes OR 'T' $ lcTypes)
  llUseDyelot = laSetups[8,2]='Y' .OR. laSetups[12,2]='Y'
  lcTmpCutPik = gfTempName()   && Edit order allocated quantity
  lcTmpTkt    = gfTempName()   && Ticket lines temporary file
  lcDetFile   = gfTempName()   && Ticket sheet details temporary file
  lcTktSheet  = gfTempName()   && Ticket sheet header temporary file
  lcGlDTemp   = gfTempName()   && General Ledger temporary file
  lcOpenLots  = gfTempName()
  lcOprHdr    = gfTempName()   && Operation header temporary file
  lcOprDet    = gfTempName()   && Operation details temporary file
  lcOprDet1   = gfTempName()
  lcLotsDet   = gfTempName()
  lcRcvFile   = gfTempName()
  lcIssLtFile = gfTempName()
  lcisslogfile= gfTempName()
  lcTmpBom    = gfTempName()   && Style cost sheet temporary file
  lcTmpStyle  = gfTempName()   && Style temporary file
  lcTmpFbric  = gfTempName()   && Fabric temporary file 
  lcWinCh0    = gfTempName()
  lcWinCh11   = gfTempName()
  lcWinCh12   = gfTempName()
  lcWinCh31   = gfTempName()
  lcWinCh32   = gfTempName()
  lcWinCh21   = gfTempName()
  lcWinCh22   = gfTempName()
  lcWinCh23   = gfTempName()
  lcWinCh24   = gfTempName()
  lcWinCh25   = gfTempName()
  lcfolder    = gfTempName()      && Folder Window Name
  lcTmpWip    = gfTempName()      && HDM B602148,1
  lcfoldprnt  = gcBaseWind        && window parent name for the folder
  lnactfolder = 1                 && active folder
  FOR lnCount = 1 TO 4
    lcCount = STR(lnCount,1)
    lcWinCh&lcCount = gfTempName()
    lafoldwinds[lnCount,1] = lafolders[lnCount,1]
    lafoldwinds[lnCount,2] = EVAL('lcWinCh'+STR(lnCount,1))
  ENDFOR

  *C102095,1 AMH Add Flag for closing from select window [Start].
  *C102095,1 AMH This flag will be set to .T. when calling the custom trigger
  *C102095,1 AMH for Cathy Daneil. Otherwise it will be .F. and
  *C102095,1 AMH Add a custom trigger for Cathy Daniels to display a screen
  *C102095,1 AMH to allow the user to select a cuttkt.
  *llCloseCs = .F.
  IF TYPE('lcTranNo')<>'C' .AND. !llOpenCs
    IF ASCAN(laEvntTrig , PADR('SELCSFLD',10)) <> 0
      =gfDoTriger('MFCSSH',PADR('SELCSFLD',10))
      llOpenCs = .T.
    ENDIF     
  ENDIF
  *C102095,1 AMH [End]

  *C200080,1 AMM Consider the dye order case
  *IF TYPE('lcTranNo')='C' .AND. SEEK(IIF(lcTranType='I','P','')+PADR(lcTranNo,6),lcBaseFile)
  
  *khm1
  *IF TYPE('lcTranNo')='C' .AND. SEEK(IIF(lcTranType='I',IIF(EMPTY(lcDyePO),'P','D'),'')+PADR(lcTranNo,6),lcBaseFile)
  IF TYPE('lcTranNo')='C' .AND. SEEK(IIF(lcTranType $ 'ID',IIF(EMPTY(lcDyePO),'P','D'),'')+PADR(lcTranNo,6),lcBaseFile)
  *khm1
  
  *C200080,1 AMM end
    SELECT (lcBaseFile)
    STORE .F. TO laScrMode
    STORE .T. TO laScrMode[2]
    SCATTER FIELDS &lcScFields TO laData
  ENDIF
  SELECT CTKTBOM
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+1,4]

  *--- Doc. [Start]
  *--- cShowType == Variable to holds '0' for Header record '1' for detail lines
  *--- Doc. [End]

  laFileStru[lnFileStru+1,1] = 'cShowType'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 1
  laFileStru[lnFileStru+1,4] = 0
  CREATE TABLE (gcWorkDir+lcTktSheet) FROM ARRAY laFileStru
  INDEX ON cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot TAG 'CTKTBOM'
  INDEX ON TYP+cShowType+ITEM+ICLR+MFGCODE+DYELOT TAG (lcTktSheet)  ADDITIVE
  
  *E301077,55 Inhance openning files to speed up transaction
  **E301088,1 Display Materials on hand
  *SET RELATION TO SUBSTR(ITEM,1,7)+ICLR INTO FABRIC
  *SET RELATION TO ITEM INTO STYLE ADDITIVE
  **E301088,1 (End)
  *DIMENSION laFileStru[lnFileStru+3,4]
  *laFileStru[lnFileStru+1,1] = 'cSelect'
  *laFileStru[lnFileStru+1,2] = 'C'
  *laFileStru[lnFileStru+1,3] = 1
  *laFileStru[lnFileStru+1,4] = 0
  *laFileStru[lnFileStru+2,1] = 'nIssue'
  *laFileStru[lnFileStru+2,2] = 'N'
  *laFileStru[lnFileStru+2,3] = 12
  *laFileStru[lnFileStru+2,4] = 3
  *laFileStru[lnFileStru+3,1] = 'nIssPcnt'
  *laFileStru[lnFileStru+3,2] = 'N'
  *laFileStru[lnFileStru+3,3] = 6
  *laFileStru[lnFileStru+3,4] = 2
  *CREATE TABLE (gcWorkDir+lcIssLtFile) FROM ARRAY laFileStru
  *INDEX ON TYP+ITEM+ICLR+MFGCODE+DYELOT TAG (lcIssLtFile) 
  *SELECT BOMLINE
  *=AFIELDS(laFileStru)
  *lnFileStru = ALEN(laFileStru,1)
  *DIMENSION laFileStru[lnFileStru+2,4]
  *laFileStru[lnFileStru+1,1] = 'cShowType'
  *laFileStru[lnFileStru+1,2] = 'C'
  *laFileStru[lnFileStru+1,3] = 1
  *laFileStru[lnFileStru+1,4] = 0
  *laFileStru[lnFileStru+2,1] = 'lHideTit'
  *laFileStru[lnFileStru+2,2] = 'L'
  *laFileStru[lnFileStru+2,3] = 1
  *laFileStru[lnFileStru+2,4] = 0
  *CREATE TABLE (gcWorkDir+lcDetFile) FROM ARRAY laFileStru
  *INDEX ON cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode TAG 'BOMLINE'
  *INDEX ON Style+SClr+STR(LineNo,6)+cBomTyp+cShowType+Item+IClr+MfgCode TAG (lcDetFile) ADDITIVE
  *SELECT MfgOprHd
  *COPY STRUCTURE TO (gcWorkDir+lcOprHdr)
  *=gfOpenFile(gcWorkDir+lcOprHdr,'','EX')
  
  *INDEX ON cIMTyp+cTktNo+cOprCode TAG 'MFGOPRHD'
  *INDEX ON cOperSeq+cOprCode TAG (lcOprHdr) ADDITIVE
  *SELECT MfgOprDt
  *=AFIELDS(laFileStru)
  **B801929,1 sort operation lots by operation sequence.
  *DIMENSION laFileStru[lnFileStru+1,4]
  *laFileStru[lnFileStru+1,1] = 'cOperSeq'
  *laFileStru[lnFileStru+1,2] = 'C'
  *laFileStru[lnFileStru+1,3] = 2
  *laFileStru[lnFileStru+1,4] = 0
  **B801929,1 (End)
  *CREATE TABLE (gcWorkDir+lcOprDet) FROM ARRAY laFileStru
  *INDEX ON cIMTYp+cTktNo+cOprCode+Item+Color TAG 'Item'
  *INDEX ON cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd TAG 'Lot'
  *CREATE TABLE (gcWorkDir+lcOprDet1) ;
  *(cIMTyp C(1), cTktNo C(6),cOprCode C(6), cLotNo C(2), cContCode C(6), Item C(19),Color C(6),;
  * Iss1 N(6),Iss2 N(6),Iss3 N(6),Iss4 N(6),Iss5 N(6),Iss6 N(6),Iss7 N(6),Iss8 N(6),Iss N(7),;
  * Rcv1 N(6),Rcv2 N(6),Rcv3 N(6),Rcv4 N(6),Rcv5 N(6),Rcv6 N(6),Rcv7 N(6),Rcv8 N(6),Rcv N(7),;
  * Can1 N(6),Can2 N(6),Can3 N(6),Can4 N(6),Can5 N(6),Can6 N(6),Can7 N(6),Can8 N(6),Can N(7),;
  * Dmg1 N(6),Dmg2 N(6),Dmg3 N(6),Dmg4 N(6),Dmg5 N(6),Dmg6 N(6),Dmg7 N(6),Dmg8 N(6),Dmg N(7))
  *CREATE TABLE (gcWorkDir+lcLotsDet) ;
  *(cSelect C(1),Item C(19),Color C(6),nBudget1 N(6),nBudget2 N(6),nBudget3 N(6),nBudget4 N(6),;
  * nBudget5 N(6),nBudget6 N(6),nBudget7 N(6),nBudget8 N(6),nTotBudget N(7),;
  * nIssued1 N(6),nIssued2 N(6),nIssued3 N(6),nIssued4 N(6),nIssued5 N(6),;
  * nIssued6 N(6),nIssued7 N(6),nIssued8 N(6),nTotIss N(7),;
  * nToIssue1 N(5),nToIssue2 N(5),nToIssue3 N(5),nToIssue4 N(5),nToIssue5 N(5),;
  * nToIssue6 N(5),nToIssue7 N(5),nToIssue8 N(5),nTotToIss N(6),DueDate D,lCanRemain L)
  *INDEX ON Item+Color TAG (lcLotsDet)
  *E301077,55 (End)

  *C037960,1 MHM Alena Define Variables[Begin]
  IF ASCAN(laEvntTrig , PADR('INTDVAR',10)) <> 0
    =gfDoTriger('POCSSH',PADR('INTDVAR',10)) 
  ENDIF  
  *C037960,1  [End]

   *E301235,4 WAB - add decimals to field ntotqty
   *E301235,4 WAB - START
   *CREATE TABLE (gcWorkDir+lcTmpTkt) ;
   (Item C(19),Color C(6),LineNo N(6), cDyelot C(10),cWareCode C(6),nQty1 N(6),nQty2 N(6),;
   nQty3 N(6),nQty4 N(6),nQty5 N(6),nQty6 N(6),nQty7 N(6),nQty8 N(6),nTotQty N(7),nRecNo N(6),;
   cFrstOpr C(6),nFrstOprSq N(2) )
   
   *B605994,1 KHM 05/26/2002 (Begin) Increase the width of nRecNo field.
   *CREATE TABLE (gcWorkDir+lcTmpTkt) ;
   (Item C(19),Color C(6),LineNo N(6), cDyelot C(10),cWareCode C(6),nQty1 N(6),nQty2 N(6),;
   nQty3 N(6),nQty4 N(6),nQty5 N(6),nQty6 N(6),nQty7 N(6),nQty8 N(6),nTotQty N(10,3),nRecNo N(6),;
   cFrstOpr C(6),nFrstOprSq N(2) )
   
   CREATE TABLE (gcWorkDir+lcTmpTkt) ;
   (Item C(19),Color C(6),LineNo N(6), cDyelot C(10),cWareCode C(6),nQty1 N(6),nQty2 N(6),;
   nQty3 N(6),nQty4 N(6),nQty5 N(6),nQty6 N(6),nQty7 N(6),nQty8 N(6),nTotQty N(10,3),nRecNo N(10),;
   cFrstOpr C(6),nFrstOprSq N(2) )
   *B605994,1 KHM 05/26/2002 (End)
   *E301235,4 WAB - END
   INDEX ON Item+Color+STR(LineNo,6) TAG (lcTmpTkt)
ENDIF
*E301077,55 Inhance openning files to speed up transaction
*llRolls  = laSetups[11,2]='Y' .AND. gfOpenFile(gcDataDir+'ROLLS',gcDataDir+'ROLLS','SH')
*llApVendor = 'AP' $ gcCmpModules .AND. gfOpenFile(gcDataDir+'ApVendor',gcDataDir+'VenCode','SH')
*SELECT SUBSTR(cDesc,1,20),cWareCode FROM WAREHOUS INTO ARRAY laStyWare
*SELECT SUBSTR(cDesc,1,20),cWareCode FROM WAREHOUS INTO ARRAY laMatWare
*E301077,55 (End)
=IIF(laSetups[1,2]='Y',gfOpenFile(gcDataDir+'Gl_Link',gcDataDir+'Gl_Link1','SH'),.T.)
=IIF(laSetups[1,2]='Y',gfOpenFile(gcDataDir+'GlDIST',gcDataDir+'GlDISTAC','SH'),.T.)
IF laSetups[1,2]='Y' .AND. !USED(lcGlDTemp)
  SELECT GLDIST
  COPY STRUCTURE TO (gcWorkDir+lcGlDTemp)
  =gfOpenFile(gcWorkDir+lcGlDTemp,'','EX')
ENDIF

=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

lcKey     = gcBmpHome + "ExtKey.BMP"
lcNew     = gcBmpHome + "NEW.BMP"
lcRemove  = gcBmpHome + "REM.BMP"
lcModify  = gcBmpHome + "MODIFY.BMP"
lcVoid    = gcBmpHome + "VOID.BMP"
lcUnVoid  = gcBmpHome + "UNVOID.BMP"
lcReturn  = gcBmpHome + "RETURNS.BMP"
lcClose   = gcBmpHome + "CLOSE1.BMP"
lcOk      = gcBmpHome + "OK.BMP"
lcCancel  = gcBmpHome + "CAN.BMP"
lcMoveR   = gcBmpHome + "NEXT.BMP"
lcMoveL   = gcBmpHome + "PRIOR.BMP"
*E301427,1 SSH 31/05/20 BMP for detail operation select button.
lcSelDet  = gcBmpHome + "SEL1.BMP"
*E301427,1 SSH 31/05/20 [END]
DECLARE laPanelObj[2,3]
STORE '' TO laPanelObj
laPanelObj[1,1] = 'pbClose'
laPanelObj[1,2] = gcBmpHome+"CLS_Shet.bmp"
laPanelObj[1,3] = [VALID lfvClose() MESSAGE 'Close Cost Sheet']
laPanelObj[2,1] = 'pbGenerate'
laPanelObj[2,2] = gcBmpHome+"Generat.bmp"
laPanelObj[2,3] = [VALID lfvGenerate() MESSAGE 'Generate Cost Sheet']

*E301077,55 Inhance openning files to speed up transaction
*USE (gcDataDir+'MATINVJL') AGAIN ALIAS FABINV ORDER TAG Mtinvseq IN 0
*SELECT STYLE
*SET RELATION TO 'S'+SCALE INTO SCALE ADDITIVE
*E301077,55 (End)

*B606621,1 AMH Dont push menu since it overwrite the menu stack [Start]
*PUSH MENU _MSYSMENU
*B606621,1 AMH [End]

lnEditBar = lfGtSavBar('GFCPEDIT')
lnDelBar  = lfGtSavBar('GFCPDELETE')
SELECT (lcBaseFile)

*E301085,1 Generate cost sheet automatically

*--- Doc. [Start]
*--- llAutoGen == Parameter to generate cost sheet automaticaly
*---              if called from cuttkt/po progeam
*--- Doc. [End]

*C200477,1 AMH Add option menu to issue/return rolls [Start]
IF ASCAN(laEvntTrig,PADR("ADOPTROL",10)) <> 0
  =gfDoTriger("POCSSH",PADR("ADOPTROL",10))
ENDIF
*C200477,1 AMH [End]

IF llAutoGen

  *--- Doc. [Start]
  *--- lfGetInfo   == Function to get the information of the cost sheet (Initilize)
  *--- lfvGenerate == Function to generate cost sheet
  *--- Doc. [End]
  =lfGetInfo() AND lfvGenerate()
  DO lpSavScr
ELSE
  *C102095,1 AMH check if close from select window [Start].
  *C102095,1 AMH That will be done in the custom screen for Cathy Daneil.
  *DO (gcScrDir+"MFCSSH.SPX")
  IF !llCloseCs
    DO (gcScrDir+"MFCSSH.SPX")
  ENDIF
  *C102095,1 AMH [End]
ENDIF 
*E301085,1 (End)

*B606621,1 AMH Dont need to pop menu [Start]
*POP MENU _MSYSMENU
*B606621,1 AMH [End]

*C200477,1 AMH Delete option menu to issue/return rolls [Start]
IF ASCAN(laEvntTrig,PADR("DLOPTROL",10)) <> 0
  =gfDoTriger("POCSSH",PADR("DLOPTROL",10))
ENDIF
*C200477,1 AMH [End]

=lfClearKey()
*E301077,55 Inhance openning files to speed up transaction
*USE IN 'FABINV'
*SELECT STYLE
*SET RELATION OFF INTO SCALE
*E301077,55 (End)

IF WEXIST(lcWSheet)
  HIDE WINDOW (lcWSheet)
  RELEASE WINDOW (lcWSheet)
ENDIF
IF WEXIST(lcDetTitle)
  HIDE WINDOW (lcDetTitle) SAME
  RELEASE WINDOW (lcDetTitle)
ENDIF
IF WEXIST(lcOprHdTit)
  HIDE WINDOW (lcOprHdTit) SAME
  RELEASE WINDOW (lcOprHdTit)
ENDIF
IF WEXIST(lcBrZoom)
  HIDE WINDOW (lcBrZoom) SAME
  RELEASE WINDOW (lcBrZoom)
ENDIF
IF WEXIST(lcOprDtTit)
  HIDE WINDOW (lcOprDtTit) SAME
  RELEASE WINDOW (lcOprDtTit)
ENDIF

*B123168,1 NNA 08/01/2004 (Begin) Remove Temp Files that Created by the Program when we go out of
*B123168,1 NNA                    the program specially when we run Generate PO form Sales Order]
*IF glQuitting
llCHkQut = IIF(llAutoGen,.T.,glQuitting)
IF llCHkQut
*B123168,1 NNA (End)

  IF USED()
    USE IN (lcTktSheet)
    ERASE (gcWorkDir+lcTktSheet+".DBF")
    ERASE (gcWorkDir+lcTktSheet+".CDX")
  ENDIF
  IF USED(lcDetFile)
    USE IN (lcDetFile)
    ERASE (gcWorkDir+lcDetFile+".DBF")
    ERASE (gcWorkDir+lcDetFile+".CDX")
  ENDIF
  IF USED(lcOprHdr)
    USE IN (lcOprHdr)
    ERASE (gcWorkDir+lcOprHdr+".DBF")
    ERASE (gcWorkDir+lcOprHdr+".CDX")
  ENDIF
  IF USED(lcOprDet)
    USE IN (lcOprDet)
    ERASE (gcWorkDir+lcOprDet+".DBF")
    ERASE (gcWorkDir+lcOprDet+".CDX")
  ENDIF
  IF USED(lcOprDet1)
    USE IN (lcOprDet1)
    ERASE (gcWorkDir+lcOprDet1+".DBF")
    ERASE (gcWorkDir+lcOprDet1+".CDX")
  ENDIF
  IF USED(lcLotsDet)
    USE IN (lcLotsDet)
    ERASE (gcWorkDir+lcLotsDet+".DBF")
    ERASE (gcWorkDir+lcLotsDet+".CDX")
  ENDIF
  IF USED(lcIssLtFile)
    USE IN (lcIssLtFile)
    ERASE (gcWorkDir+lcIssLtFile+".DBF")
    ERASE (gcWorkDir+lcIssLtFile+".CDX")
  ENDIF
  IF USED(lcTmpTkt)
    USE IN (lcTmpTkt)
    ERASE (gcWorkDir+lcTmpTkt+".DBF")
    ERASE (gcWorkDir+lcTmpTkt+".CDX")
  ENDIF
  IF USED(lcGlDTemp)
    USE IN (lcGlDTemp)
  ENDIF
  ERASE (gcWorkDir+lcGlDTemp+".DBF")  
  IF USED(lcOpenLots)
    USE IN (lcOpenLots)
  ENDIF
  ERASE (gcWorkDir+lcOpenLots+".DBF")
  ERASE (gcWorkDir+lcOpenLots+".CDX")

  IF USED(lcRcvFile)
    USE IN (lcRcvFile)
  ENDIF
  ERASE (gcWorkDir+lcRcvFile+".DBF")
  ERASE (gcWorkDir+lcRcvFile+".CDX")
  
  IF USED (lcisslogfile)
    USE IN (lcisslogfile)
  ENDIF
  ERASE (gcWorkDir+lcisslogfile+".DBF")
  ERASE (gcWorkDir+lcisslogfile+".CDX")
  
  IF USED(lcTmpStyle)
    USE IN (lcTmpStyle)
  ENDIF
  ERASE (gcWorkDir+lcTmpStyle+".DBF")
  ERASE (gcWorkDir+lcTmpStyle+".CDX")
  
  IF USED(lcTmpFbric)
    USE IN (lcTmpFbric)
  ENDIF
  ERASE (gcWorkDir+lcTmpFbric+".DBF")
  ERASE (gcWorkDir+lcTmpFbric+".CDX")
  
  *B602179,1 AMM Erase temporary files when quitting the program
  IF USED(lcTmpBom)
    USE IN (lcTmpBom)
  ENDIF    
  ERASE (gcWorkDir+lcTmpBom+".DBF")
  ERASE (gcWorkDir+lcTmpBom+".CDX")
  ERASE (gcWorkDir+lcTmpBom+".FPT")
  *B602179,1 AMM end
  
ENDIF

*E301077,55 Inhance openning files to speed up transaction
*IF llGlLInk
*  USE IN Gl_Link
*ENDIF
*IF llRolls
*  USE IN ROLLS
*ENDIF
*IF llApVendor
*  USE IN ApVendor
*ENDIF
*IF llGlDist
*  USE IN GLDIST
*ENDIF
*E301077,55 (End)

*!*************************************************************
*! Name      : lfActFolder
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Change Folders
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfActFolder()
*!*************************************************************
FUNCTION lfActFolder

=lfBrowse()
DO CASE
  CASE lnActFolder = 1
    SHOW GETS WINDOW (lcWinCh22) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh32) DISABLE ONLY  
    SHOW GETS WINDOW (lcWinCh24) DISABLE ONLY
    IF EOF(lcTktSheet) .OR. INLIST(laData[3],'S','X')
      SHOW GETS WINDOW (lcWinCh12) DISABLE ONLY
    ELSE  
     *B602940,1 SSH 01/05/00 Disable new button in case of style use detail costing
     *B602940,1 SSH 01/05/00 no.[Start]
      IF lcTranType='M'
        lnOldInd = ORDER('Style')
        SET ORDER TO Cstyle IN Style
        =SEEK(CutTktH.Style,'Style')
        IF Style.lDetCost 
          SHOW GET pbNew    ENABLE
        ELSE
          SHOW GET pbNew    DISABLE
        ENDIF
        SET ORDER TO lnOldInd IN Style
      ELSE
      *B602940,1 SSH 01/05/00 [End]
        SHOW GET pbNew    ENABLE
      *B602940,1 SSH 01/05/00 no.[Start]
      ENDIF
      *B602940,1 SSH 01/05/00 [End]
      SHOW GET pbRemove ENABLE
      SHOW GET pbModify ENABLE
      IF laScrMode[4]
        SHOW GET pbVoid   DISABLE
        SHOW GET pbUnVoid DISABLE
        SHOW GET pbAuto   DISABLE
        SHOW GET pbIssLog DISABLE
      ELSE
        SHOW GET pbVoid   ENABLE
        SHOW GET pbUnVoid ENABLE
        SHOW GET pbAuto   ENABLE
        SHOW GET pbIssLog ENABLE
      ENDIF
    ENDIF
    SHOW GET ibBSheet ENABLE
    IF laScrMode[1]
      SHOW GET ibFolder[2] DISABLE
      SHOW GET ibFolder[3] DISABLE
      SHOW GET ibFolder[4] DISABLE
    ELSE
      SHOW GET ibFolder[2] ENABLE
      SHOW GET ibFolder[3] ENABLE
      SHOW GET ibFolder[4] ENABLE
    ENDIF  
    =lfwBrow()
  CASE lnActFolder = 2
    SHOW GETS WINDOW (lcWinCh12) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh32) DISABLE ONLY
    lnRecNo = RECNO(lcOprDet)
    IF !SEEK(lcTranType+laData[1],lcOprDet) .OR. INLIST(laData[3],'S','X')
      SHOW GETS WINDOW (lcWinCh24) DISABLE ONLY
      SHOW GET ibOprH2  ENABLE
    ELSE
      IF rbSummary=2
        SHOW GETS WINDOW (lcWinCh24) DISABLE ONLY
        SHOW GET pbDetlvl ENABLE
        SHOW GET pbZoom   ENABLE
        SHOW GET ibOprH2  ENABLE
      ELSE
        SHOW GETS WINDOW (lcWinCh24) ENABLE ONLY
      ENDIF  
    ENDIF
    IF BETWEEN(lnRecNo,1,RECCOUNT(lcOprDet)) 
      GOTO lnRecNo IN (lcOprDet)
    ENDIF
    IF laData[28]='M'
      SHOW GET pbNewLot,1 PROMPT '\<New Lot'
    ELSE
      SHOW GET pbNewLot,1 PROMPT '\<Issue Lot'
    ENDIF
    IF EOF(lcOprHdr) .OR. INLIST(laData[3],'S','X')
      SHOW GETS WINDOW (lcWinCh22) DISABLE ONLY
      SHOW GET ibOprHdr ENABLE
      SHOW GET ibOprDet ENABLE
    ELSE
      SHOW GETS WINDOW (lcWinCh22) ENABLE ONLY
    ENDIF  
    =lfShOprHd()
    =lfShOprDt()
  CASE lnActFolder = 3
    SHOW GETS WINDOW (lcWinCh12) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh22) DISABLE ONLY  
    SHOW GETS WINDOW (lcWinCh24) DISABLE ONLY
    SHOW GET ibDetTitle ENABLE
    =lfShowDet()
  CASE lnActFolder = 4    
    SHOW GETS WINDOW (lcWinCh12) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh22) DISABLE ONLY  
    SHOW GETS WINDOW (lcWinCh24) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh32) DISABLE ONLY
    ACTIVATE WINDOW (lcWinCh4)
    =lfRefresh(lcWinCh4)
ENDCASE

*!*************************************************************
*! Name      : lpShow
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Show Main Screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lpShow()
*!*************************************************************
FUNCTION lpShow

lcBrFields = lcBaseFild
lcFile_Ttl = lcBaseTit
SHOW GET pbEdt DISABLE

*B606621,1 AMH Dont overwrite the edit menu bar defination [Start]
*DEFINE BAR lnEditBar OF P03PU03 PROMPT '\<Edit' SKIP FOR .T.
laCtrStat[7] = 'DISABLE'
*B606621,1 AMH [End]

STORE .T. TO laBrowse
DO CASE

*--- Doc. [Start]
*--- laScrMode   1- Select. Mode Screen
*---             2- View... Mode Screen
*---             3- Edit... Mode Screen
*---             4- Add New Mode Screen
*--- Doc. [End]

  CASE laScrMode[1]
 
    SHOW GET pbClose    DISABLE
    SHOW GET pbGenerate DISABLE
    STORE '' TO lcStatus,lcFirstOpr
    *B801929,1 sort operation lots by operation sequence.
    *lcIndExp = 'cIMTyp+cTktNo+cOprCode+cContCode+cLotNo+Item+Color'
    *B603243,1 AMM Add Dyelot
    *lcIndExp  = 'cIMTyp+cTktNo+cOperSeq+cOprCode+cContCode+cLotNo+Item+Color'
    lcIndExp  = 'cIMTyp+cTktNo+cOperSeq+cOprCode+cContCode+cLotNo+Item+Color+cDyelot'
    *B603243,1 AMM end
    *B801929,1 (End)
    
    rbSummary = 1
    STORE .F. TO cbSum1,cbSum2,cbSum3,cbSum4,cbPerSize,llIssFirst,llZoom
    RELEASE POPUPS lcpusort
    DEFINE POPUP lcpusort MARGIN MOVER RELATIVE SCROLL MARK CHR(16)
    DEFINE BAR 1 OF lcpusort PROMPT laSort[1,2]
    DEFINE BAR 2 OF lcpusort PROMPT laSort[2,2]
    DEFINE BAR 3 OF lcpusort PROMPT laSort[3,2]
    DEFINE BAR 4 OF lcpusort PROMPT laSort[4,2]
    *B802814,1 AMM stART, Don't zap the files here, lfGetInfo() will do it
    *IF WEXIST(lcWSheet)
    *  HIDE WINDOW (lcWSheet)
    *  RELEASE WINDOW (lcWSheet)
    *ENDIF
   * SELECT (lcTktSheet)
    *ZAP
    *B802814,1 AMM end
    *E301077,55 Inhance openning files to speed up transaction
    *IF WEXIST(lcDetTitle)
    *  HIDE WINDOW (lcDetTitle) SAME
    *  RELEASE WINDOW (lcDetTitle)
    *ENDIF
    *SELECT (lcDetFile)
    *ZAP
    *IF WEXIST(lcOprHdTit)
    *  HIDE WINDOW (lcOprHdTit) SAME
    *  RELEASE WINDOW (lcOprHdTit)
    *ENDIF
    *SELECT (lcOprHdr)
    *ZAP
    *IF WEXIST(lcBrZoom)
    *  HIDE WINDOW (lcBrZoom) SAME
    *  SHOW WINDOW (lcWinCh22) TOP
    *  RELEASE WINDOW (lcBrZoom)
    *ENDIF  
    *IF WEXIST(lcOprDtTit)
    *  HIDE WINDOW (lcOprDtTit) SAME
    *  RELEASE WINDOW (lcOprDtTit)
    *ENDIF
    *SELECT (lcOprDet)
    *ZAP
    *SELECT (lcOprDet1)
    *ZAP
    *E301077,55 (End)

    IF lnActFolder <> 1
      lnlastfold = lnactfolder
      *B802814,1 AMM Set the variable to false not to create the browse in lfActFold()
      laBrowse[1] = .F.
      *B802814,1 AMM
      lnActFolder = 1
      =lfChngFolder(lnActFolder)
      *B802814,1 AMM Set this variable with true for the next activate folder function (lfActFolder()  )
      laBrowse[1] = .T.
      *B802814,1 AMM end
    ENDIF  
    
    *E301235,4 WAB - Call function to open files because in case ther no record in tabled we get error
    *E301235,4 WAB - Alias 'FABRIC' Not Found()
    *E301235,4 WAB - START 

    =lfGetInfo()
    
    *E301235,4 WAB - END

    *C102095,1 AMH Add a customized trigger for Cathy Daniels to display [Start]
    *C102095,1 AMH a screen to allow the user to select a cuttkt.
    IF ASCAN(laEvntTrig , PADR('SELCSFLD',10)) <> 0
      =gfDoTriger('MFCSSH',PADR('SELCSFLD',10))
      IF llCloseCs
        =gfCPClose()
        RETURN
      ENDIF
      IF !laScrMode[1]
        laData[1] = lcTranNo
        =lfvTicket()
        DO GPCTRLSHOW
        DO lpShow
      ENDIF
    ENDIF     
    *C102095,1 AMH [End]

  CASE laScrMode[2]
    STORE '/' TO lcPExSign,lcDExSign,lcPUntSin,lcDUntSin
    *khm1*
    *IF lcTranType='I'
    IF lcTranType $ 'ID'
    *khm1*
      lcPExSign = gfGetExSin(@lcPUntSin,POSHDR.cPriceCur)
      lcDExSign = gfGetExSin(@lcDUntSin,POSHDR.cDutyCur)
    ENDIF  
    *B801929,1 sort operation lots by operation sequence.
    *lcIndExp = 'cIMTyp+cTktNo+cOprCode+cContCode+cLotNo+Item+Color'
    *B603243,1 AMM Add dyelot field
    *lcIndExp = 'cIMTyp+cTktNo+cOperSeq+cOprCode+cContCode+cLotNo+Item+Color'
    lcIndExp = 'cIMTyp+cTktNo+cOperSeq+cOprCode+cContCode+cLotNo+Item+Color+cDyelot'
    *B603243,1 AMM end
    *B801929,1 (End)
   
    STORE IIF(EMPTY(laData[31]),laData[11],laData[31]) TO laData[31]
    STORE IIF(EMPTY(laData[32]),laData[11],laData[32]) TO laData[32]
    =gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')

    *B606205,1 KHM 07/03/2002 (Begin) Selecting the style's locations only and material's locations only.
    *SELECT SUBSTR(cDesc,1,20),cWareCode FROM WAREHOUS INTO ARRAY laStyWare
    *SELECT SUBSTR(cDesc,1,20),cWareCode FROM WAREHOUS INTO ARRAY laMatWare
    
    SELECT SUBSTR(cDesc,1,20),cWareCode FROM WAREHOUS WHERE lStyInv INTO ARRAY laStyWare
    SELECT SUBSTR(cDesc,1,20),cWareCode FROM WAREHOUS WHERE lMatInv INTO ARRAY laMatWare
    *B606205,1 KHM 07/03/2002 (End)

    =gfCloseFile('WAREHOUS')
    
    *B605380,1 KHM 04/04/2002 (Begin) Adding the ceiling CEILING
    *lnTkStWare = ASCAN(laStyWare,laData[31])/2
    *lnTkMtWare = ASCAN(laMatWare,laData[32])/2
    lnTkStWare = CEILING(ASCAN(laStyWare,laData[31])/2)
    lnTkMtWare = CEILING(ASCAN(laMatWare,laData[32])/2)
    *B605380,1 KHM 04/04/2002 (End)
    
    SHOW GET lnTkStWare
    SHOW GET lnTkMtWare

    *--- Doc. [Start]
    *--- ladata[3] == Status of the transaction (PO,C/T,Material PO,..)
    *--- Doc. [End]

    DO CASE
      CASE ladata[3] = 'O'
        lcStatus = 'Open'
        SHOW GET pbGenerate DISABLE
        IF gfUserPriv('MF','MFCSSH','TKTCLOSE')
          SHOW GET pbClose ENABLE
        ENDIF  
      CASE ladata[3] = 'H'
        lcStatus = 'Hold'
        SHOW GET pbGenerate ENABLE
        SHOW GET pbClose    DISABLE
      CASE ladata[3] = 'C'
        lcStatus = 'Complete'
        *B602188,1 Enable closing for complete tickets
        *SHOW GET pbClose   DISABLE

        *B121190,1 NNA 04/19/2004 (Begin) if the user have a subprocess to close the po or he is an admin
        *B121190,1 NNA            the push button will be enable           
        
        *B128469,1 EIH 11/13/2005 Fix bug that Can not close cutting tickets to user which have closing C/T sub process.[Begin]     
        *IF gfUserPriv('PO','POCSSH','Closing') .OR. gcUser_lvl='A'
        IF (lcTranType = 'I' AND gfUserPriv('PO','POCSSH','Closing')) .OR. (gcUser_lvl='A')  OR (lcTranType = 'M' ;
        AND gfUserPriv('MF','MFCSSH','TKTCLOSE') )
        
        *B128469,1 EIH 11/13/2005 [End]     
        
        *B121190,1 NNA (End)
          SHOW GET pbClose ENABLE
        *B121190,1 NNA 04/19/2004(Begin)
        ELSE
          SHOW GET pbClose DISABLE        
        ENDIF  
        *B121190,1 NNA (End)

        SHOW GET pbGenerate DISABLE
      CASE ladata[3] = 'S'
        lcStatus = 'Closed'
        SHOW GET pbClose    DISABLE
        SHOW GET pbGenerate DISABLE
      CASE ladata[3] = 'A'
        lcStatus = 'Actualized'

        *B121190,1 NNA 04/19/2004 (Begin) if the user have a subprocess to close the po or he is an admin
        *B121190,1 NNA            the push button will be enable           
        IF gfUserPriv('PO','POCSSH','Closing') .OR. gcUser_lvl='A'
        *B121190,1 NNA (End)
          SHOW GET pbClose ENABLE
        *B121190,1 NNA 04/19/2004 (Begin)
        ELSE
          SHOW GET pbClose DISABLE        
        ENDIF  
        *B121190,1 NNA (End)

        SHOW GET pbGenerate DISABLE
      CASE ladata[3] = 'X'
        lcStatus = 'Canceled'
        SHOW GET pbGenerate DISABLE
        SHOW GET pbClose    DISABLE
    ENDCASE
    IF INLIST(ladata[3],'H','S','X')
      SHOW GET pbDlt DISABLE
      
      *B606621,1 AMH Dont overwrite the delete menu bar defination [Start]
      *DEFINE BAR lnDelBar OF P03PU03 PROMPT '\<Delete' SKIP FOR .T.
      laCtrStat[8] = 'DISABLE'
      *B606621,1 AMH [End]
      
    ELSE
      SHOW GET pbDlt ENABLE
      
      *B606621,1 AMH Dont overwrite the delete menu bar defination [Start]
      *DEFINE BAR lnDelBar OF P03PU03 PROMPT '\<Delete' SKIP FOR .F.
      laCtrStat[8] = 'ENABLE'
      *B606621,1 AMH [End]
      
    ENDIF
    =lfGetInfo()
    =lfRefresh(lcWinCh0)
    IF !EMPTY(lcTranNo)
      IF laData[3]='H'
        =lfvGenerate()
      ENDIF
      STORE SPACE(6) TO lcTranNo
    ENDIF
  CASE laScrMode[4]
    SHOW GET pbClose    DISABLE
    SHOW GET pbGenerate DISABLE
    SHOW GET pbVoid     DISABLE
    SHOW GET pbUnVoid   DISABLE
    SHOW GET pbAuto     DISABLE
    SHOW GET pbIssLog   DISABLE
    *C102095,1 AMH Check if the trigger for Cathy Daniel exist then disable
    *C102095,1 AMH the user defined fields button.
    IF ASCAN(laEvntTrig , PADR('SELCSFLD',10)) <> 0
      SHOW GET pbUsrFields DISABLE 
    ENDIF     
   *C102095,1 AMH [End]

ENDCASE
=lfActFolder()

*!*************************************************************
*! Name      : lfDActive
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Deactivate main screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfDActive()
*!*************************************************************
FUNCTION lfDActive

IF WONTOP()=lcWSheet .OR. WONTOP()=lcDetTitle .OR. WONTOP()=lcOprHdTit .OR. ;
   WONTOP()=lcOprDtTit .OR. WONTOP() = lcBrZoom
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  glFromBrow = .T.
  DO CASE
    CASE WONTOP()=lcWSheet
      ON KEY LABEL TAB     DO lpTab WITH lcWinCh12,'pbNew'
      ON KEY LABEL BACKTAB DO lpBackTab WITH lcFolder,'ibFolder[1]'
    CASE WONTOP()=lcOprHdTit
      ON KEY LABEL TAB     DO lpTab WITH lcWinCh22,'pbModOpr'
      ON KEY LABEL BACKTAB DO lpBackTab WITH lcFolder,'ibFolder[2]'
    CASE WONTOP()=lcOprDtTit
      ON KEY LABEL TAB     DO lpTab WITH lcWinCh24,'pbDetlvl'
      ON KEY LABEL BACKTAB DO lpBackTab WITH lcWinCh22,IIF(laScrMode[1],'ibOprHdr','pbNewLot')
    CASE WONTOP()=lcBrZoom
      ON KEY LABEL TAB     DO lpTab WITH lcWinCh24,'pbDetlvl'
      ON KEY LABEL BACKTAB DO lpBackTab WITH lcFolder,'ibFolder[2]'
    CASE WONTOP()=lcDetTitle
      ON KEY LABEL TAB     DO lpTab WITH 'gwcContrl1','pbTop'
      ON KEY LABEL BACKTAB DO lpBackTab WITH lcFolder,'ibFolder[3]'
  ENDCASE
ELSE
  glFromBrow = .F.
ENDIF
*B801796,1 Return to program after calling other programs
IF lnActFolder=1 AND !INLIST(WOUTPUT(),lcWinCh11,lcWinCh12)
  ACTIVATE WINDOW (lcWinCh12)
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lpTab
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpTab WITH 'lcWinCh32', 'm.Store',.T.
*!*************************************************************
PROCEDURE lpTab
PARAMETERS lcWindName, lcObjName,llToCheck

ON KEY LABEL TAB
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*!*************************************************************
*! Name      : lpBackTab
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpBackTab WITH 'lcWinCh32', 'm.Store',.T.
*!*************************************************************
PROCEDURE lpBackTab
PARAMETERS lcWindName, lcObjName,llToCheck

ON KEY LABEL BACKTAB 
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*!*************************************************************
*! Name      : lfReadAct
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : READ Activate function of SoOrd
*!*************************************************************
*! Calls     : lfClearKey.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfReadAct()
*!*************************************************************
FUNCTION lfReadAct

IF glFromBrow
  =gfStopBrow()
  glFromBrow = .F.
ENDIF
=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

*!*************************************************************
*! Name      : lfClearKey
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Clear Hot Keys
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfClearKey()
*!*************************************************************
FUNCTION lfClearKey

ON KEY LABEL ALT+B
ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+HOME
ON KEY LABEL CTRL+END
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ENTER

*!*************************************************************
*! Name      : lfvTicket
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Browse C/t, PO and Manufacturing Order
*!*************************************************************
*! Calls     : CUTBROW,POSBROW,gfMFGOrdBr,gfSeekRec
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvTicket()
*!*************************************************************
FUNCTION lfvTicket
PRIVATE lcTicket

lcTicket = laData[1]
DO CASE
  *--- DOC.
  *--- Case CutTkt .And.
  *--- llBrowse : Call this function from browsw button.
  *--- DOC.
  CASE lcTranType = 'M' .AND. ;
    (llBrowse .OR. !EMPTY(laData[2]) .OR. (!EMPTY(laData[1]) .AND. !SEEK(laData[1],'CutTktH')))
    =CUTBROW(@lcTicket,laData[2])
  *C200080,1 AMM Consider the dye order case
  *CASE lcTranType = 'I'
  CASE lcTranType = 'I' .AND. EMPTY(lcDyePO)
  *C200080,1 AMM end
    IF llBrowse .OR. (!EMPTY(laData[1]) .AND. !SEEK('P'+laData[1],'POSHDR'))
      =POSBROW(@lcTicket,'','P')
    ENDIF
    laData[2] = 'P'
  *C200080,1 AMM Case of dye order

  *khm1
  *CASE lcTranType = 'I' .AND. lcDyePO = 'D'
  CASE lcTranType = 'D'
  *khm1

    IF llBrowse .OR. (!EMPTY(laData[1]) .AND. !SEEK('D'+laData[1],'POSHDR'))
      =POSBROW(@lcTicket,'','D')
    ENDIF
    *C200080,1 AMM start
    laData[2] = 'D'
  *C200080,1 AMM end
  CASE lcTranType = 'T' .AND. ;
    (llBrowse .OR. !EMPTY(laData[2]) .OR. (!EMPTY(laData[1]) .AND. !SEEK(laData[1],'MMFGORDH')))

    *E301235,4 WAB - If There No record In table MMFGORDH no need to go to next field
    *E301235,4 WAB - START 
    *=gfMFGOrdBr(@lcTicket,laData[2])
 
    llChkRcrd=gfMFGOrdBr(@lcTicket,laData[2])
    IF !llCHkRcrd  .AND. EMPTY(laData[2])
      _CUROBJ = OBJNUM(laData[1])
    ENDIF

    *E301235,4 WAB - END

ENDCASE

laData[1] = lcTicket
llBrowse = .F.
IF !EMPTY(laData[1])
  =gfSeekRec()
  *B602878,1 Do not Show PO contracts, Returns or Inter-Locations
  
  *khm1*
  *IF lcTranType = 'I'
  IF lcTranType $ 'ID'
  *khm1*
  
    SELECT POSHDR
    *C200080,1 AMM Consider the dye order case
    *SET KEY TO 'P'
    *=SEEK('P'+LADATA[1])
    IF EMPTY(lcDyePO)
      SET KEY TO 'P'
      =SEEK('P'+LADATA[1])
    ELSE
      SET KEY TO 'D'
      =SEEK('D'+LADATA[1])
    ENDIF
    *C200080,1 AMM end
    *E301291,4 AMM (start) Disable to behave as the CT cost sheet
    *=gpCtrlShow()
    *E301291,4 AMM end
    SET KEY TO
  ENDIF
  *B602878,1 (End)

  *C102090,1 MHM Adding an option pad to display the costing screen where
  *C102090,1 cost item is calculated per dozen.[Start]
  IF ASCAN(laEvntTrig , PADR('ADDOPTN',10)) <> 0
    =gfDoTriger('MFCSSH',PADR('ADDOPTN',10))
  ENDIF     
  *C102090 MHM add custom popup to jon [End]
ENDIF

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Browse Style
*!*************************************************************
*! Calls     : gfStyBrw
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvStyle()
*!*************************************************************
FUNCTION lfvStyle

*--- Doc. [Start]
*--- Function to select style to browse all cuttkt for this style
*--- Doc. [End]

=gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
IF llBrowse .OR. (!EMPTY(laData[2]) .AND. !SEEK(SUBSTR(laData[2],1,LEN(lcMjrMsk)),'Style'))
  laData[2] = gfStyBrw("M" , "" , "" , .F.)
ENDIF
llBrowse = .F.
IF !EMPTY(laData[2])
  _CUROBJ = OBJNUM(laData[1])
  KEYBOARD "{ENTER}"
ENDIF

*!*************************************************************
*! Name      : lfvFabric
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Browse Fabric
*!*************************************************************
*! Calls     : FABROW
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvFabric()
*!*************************************************************
FUNCTION lfvFabric
PRIVATE lcFabric

IF llBrowse .OR. (!EMPTY(laData[2]) .AND. !SEEK(laData[2],'Fabric'))
  lcFabric =laData[2]
  laData[2] = IIF(FABROW(@lcFabric,'*',.T.),lcFabric,SPACE(7))
ENDIF
llBrowse = .F.
IF !EMPTY(laData[2])
  _CUROBJ = OBJNUM(laData[1])
  KEYBOARD "{ENTER}"
ENDIF

*!*************************************************************
*! Name      : lfvLinkCode
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Browse Link Codes
*!*************************************************************
*! Calls     : gfGLBrowse,lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvLinkCode()
*!*************************************************************
FUNCTION lfvLinkCode
PRIVATE lcGlLink

*--- Doc. [Start]
*--- Function called from Mfgensht.SCX
*--- TO valid default link code.
*--- Doc [End]

IF llBrowse .OR. !SEEK('05'+laData[29],'Gl_Link')
  lcGlLink = laData[29]
  =gfGLBrowse('05',@lcGlLink)
  laData[29] = lcGlLink
ENDIF
=lfRefresh('MFGENSHT')
llBrowse = .F.

*!*************************************************************
*! Name      : lfBrowse
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Browse Cost Sheet
*!*************************************************************
*! Calls     : gfCodDes,lfShOprHd,lfAdjDetLvl,lfBrowLots,lfwBrow,lfShowDet
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfBrowse()
*!*************************************************************
FUNCTION lfBrowse

*--- DOC.
  *--- This function devided in to three case seatement each case of them 
  *--- refere to one of the following three folder
  *--- Cost Cost sheet == labrowse[1]
  *--- Operation manegment == labrowse[2]
  *--- detail requirement == labrowse[3]
*--- DOC.
IF labrowse[1] .AND. lnActFolder=1
  SELECT (lcTktSheet)
  lnMarker = RECNO()
  *E301088,1 Display Materials on hand
  *lcBrowFild = ;
         "cMarker  =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,"+;
         "cItem =IIF(cCatgTyp='M',gfCodDes(MfgCode,'MfgCode'),IIF(cCatgTyp$'FT',SUBSTR(Item,1,7)+'-'+IClr,Item)) :H='Item' :R,"+;
         IIF(llUseDyelot,"Dyelot :H='Dyelot' :R,","")+;
         "Uom       :H='UOM'     :R ,"+;
         "UntQty    :H='Units'   :R,"+;
         "UntCost   :H='Cost'    :R,"+;
         "Req_Qty   :H='Required':R,"+;
         "Issue_Qty :H='Issued'  :R,"+;
         "Used_Qty  :H='Used'    :R"
*  lcBrowFild = ;
         "cMarker  =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,"+;
         "cItem =IIF(cCatgTyp='M',gfCodDes(MfgCode,'MfgCode'),IIF(cCatgTyp$'FT',SUBSTR(Item,1,7)+'-'+IClr,Item)) :H='Item' :R,"+;
         IIF(llUseDyelot,"Dyelot :H='Dyelot' :R,","")+;
         "Uom       :H='UOM'     :R ,"+;
         "UntQty    :H='Units'   :R,"+;
         "UntCost   :H='Cost'    :R,"+;
         "nStk=IIF(cCatgTyp $ 'FT',PADL(FABRIC.ONHAND,12),IIF(cCatgTyp='S',PADL(STYLE.TOTSTK,12),'')):H='OnHand':R,"+;
         "Req_Qty   :H='Required':R,"+;
         "Issue_Qty :H='Issued'  :R,"+;
         "Used_Qty  :H='Used'    :R"

  *B802396,1 SSH 11/04/00 [Begin] Get the onhand from FabDye/StyDye in case of dyelot.
  *lcBrowFild = ;
         "cMarker  =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,"+;
         "cItem =IIF(cCatgTyp='M',gfCodDes(MfgCode,'MfgCode'),IIF(cCatgTyp$'FT',SUBSTR(Item,1,7)+'-'+IClr,Item)) :H='Item' :R,"+;
         IIF(llUseDyelot,"Dyelot :H='Dyelot' :R,","")+;
         "Uom       :H='UOM'     :R ,"+;
         "UntQty    :H='Units'   :R,"+;
         "UntCost   :H='Cost'    :R,"+;
         "nStk=IIF(cCatgTyp $ 'FT' AND !lascrmode[1],FABRIC.ONHAND,IIF(cCatgTyp='S',STYLE.TOTSTK,'')):H='OnHand':R,"+;
         "Req_Qty   :H='Required':R,"+;
         "Issue_Qty :H='Issued'  :R,"+;
         "Used_Qty  :H='Used'    :R"

  lcBrowFild = ;
         "cMarker  =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,"+;
         "cItem =IIF(cCatgTyp='M',gfCodDes(MfgCode,'MfgCode'),IIF(cCatgTyp$'FT',SUBSTR(Item,1,7)+'-'+IClr,Item)) :H='Item' :R,"+;
         IIF(llUseDyelot,"Dyelot :H='Dyelot' :R,","")+;
         "Uom       :H='UOM'     :R ,"+;
         "UntQty    :H='Units'   :R,"+;
         "UntCost   :H='Cost'    :R,"+;
         "nStk=IIF(cCatgTyp $ 'FT' AND !lascrmode[1],IIF(EMPTY(cWarecode),Fabric.OnHand,FABDYE.ONHAND),IIF(cCatgTyp='S',IIF(EMPTY(cWarecode),STYLE.TOTSTK,STYDYE.TOTSTK),'')):H='OnHand':R,"+;
         "Req_Qty   :H='Required':R,"+;
         "Issue_Qty :H='Issued'  :R,"+;
         "Used_Qty  :H='Used'    :R"

  *B802396,1 SSH 11/04/00  [END]
  *E301088,1 (End)

  *C123847,1  TMI [Start] change the browse such that to Calculate the onhand qty taking in account the picked qty for DIR03
  IF ASCAN(laEvntTrig , PADR('ONHANDBR',10)) <> 0
    =gfDoTriger('POCSSH',PADR('ONHANDBR',10))
  ENDIF  
  *C123847,1  TMI [End  ] 

  *B607340,1 AMH Valid before leave the browse to call the gfStopBrow function [Start]
  *BROWSE FIELDS &lcBrowFild ;
         WINDOW (lcWinCh11) ;
         IN WINDOW (lcWinCh1);
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
         NOCLEAR           ;
         WHEN lfwBrow()    ;
         TITLE lcWSheet
  BROWSE FIELDS &lcBrowFild ;
         WINDOW (lcWinCh11) ;
         IN WINDOW (lcWinCh1);
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
         NOCLEAR           ;
         WHEN lfwBrow()    ;
         VALID :F lfvBrows();
         TITLE lcWSheet
  *B607340,1 AMH [End]

  labrowse[1]=.F.
ENDIF
IF labrowse[3]  .AND. lnActFolder=3
  SELECT (lcDetFile)
  lnDMarker = RECNO()
  IF lcTranType = 'T'
    *B603348,1 ABD Update the picture of filed unitCost upon Increase width of The filed.[Begin] 
    *BROWSE FIELDS ;
         cMarker  =IIF(RECNO()=lnDMarker ,'>',' '):H=' ':R:1:W=.F.,;
         cStyle=IIF(cShowType='0' .AND. !lHideTit,Style,'') :H= 'Fabric' :R,;
         cSClr =IIF(cShowType='0' .AND. !lHideTit,SClr,'')  :H= 'Color'  :R,;
         cItem =IIF(cCatgTyp='M',gfCodDes(MfgCode,'MfgCode'),IIF(cCatgTyp$'FT',SUBSTR(Item,1,7)+'-'+IClr,Item)) :H='Item' :R,;
         UnitQty  :H= 'Units'  :R,;
         UnitCost :H= 'Cost'   :P='999.999' :R,;
         ItemQty  :H= 'Item Quantity',;
         ItemAmt  :H= 'Item Amount';
         WINDOW (lcWinCh31)  ;
         IN WINDOW (lcWinCh3);
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
         NOCLEAR           ;
         WHEN lfShowDet()  ;
         TITLE lcDetTitle

    *B603335,1 SSH 11/04/00 [Begin] Add dyelot to the Detail Requi. Folder.
    *BROWSE FIELDS ;
         cMarker  =IIF(RECNO()=lnDMarker ,'>',' '):H=' ':R:1:W=.F.,;
         cStyle=IIF(cShowType='0' .AND. !lHideTit,Style,'') :H= 'Fabric' :R,;
         cSClr =IIF(cShowType='0' .AND. !lHideTit,SClr,'')  :H= 'Color'  :R,;
         cItem =IIF(cCatgTyp='M',gfCodDes(MfgCode,'MfgCode'),IIF(cCatgTyp$'FT',SUBSTR(Item,1,7)+'-'+IClr,Item)) :H='Item' :R,;
         UnitQty  :H= 'Units'  :R,;
         UnitCost :H= 'Cost'   :P='9999999.99999' :R,;
         ItemQty  :H= 'Item Quantity',;
         ItemAmt  :H= 'Item Amount';
         WINDOW (lcWinCh31)  ;
         IN WINDOW (lcWinCh3);
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
         NOCLEAR           ;
         WHEN lfShowDet()  ;
         TITLE lcDetTitle
    
    *B607340,1 AMH Valid before leave the browse to call the gfStopBrow function [Start]
    *BROWSE FIELDS ;
         cMarker  =IIF(RECNO()=lnDMarker ,'>',' '):H=' ':R:1:W=.F.,;
         cStyle=IIF(cShowType='0' .AND. !lHideTit,Style,'') :H= 'Fabric' :R,;
         cSClr =IIF(cShowType='0' .AND. !lHideTit,SClr,'')  :H= 'Color'  :R,;
         cItem =IIF(cCatgTyp='M',gfCodDes(MfgCode,'MfgCode'),IIF(cCatgTyp$'FT',SUBSTR(Item,1,7)+'-'+IClr,Item)) :H='Item' :R,;
         Dyelot   :H= 'Dyelot'  :R,;
         UnitQty  :H= 'Units'  :R,;
         UnitCost :H= 'Cost'   :P='9999999.99999' :R,;
         ItemQty  :H= 'Item Quantity',;
         ItemAmt  :H= 'Item Amount';
         WINDOW (lcWinCh31)  ;
         IN WINDOW (lcWinCh3);
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
         NOCLEAR           ;
         WHEN lfShowDet()  ;
         TITLE lcDetTitle
    BROWSE FIELDS ;
         cMarker  =IIF(RECNO()=lnDMarker ,'>',' '):H=' ':R:1:W=.F.,;
         cStyle=IIF(cShowType='0' .AND. !lHideTit,Style,'') :H= 'Fabric' :R,;
         cSClr =IIF(cShowType='0' .AND. !lHideTit,SClr,'')  :H= 'Color'  :R,;
         cItem =IIF(cCatgTyp='M',gfCodDes(MfgCode,'MfgCode'),IIF(cCatgTyp$'FT',SUBSTR(Item,1,7)+'-'+IClr,Item)) :H='Item' :R,;
         Dyelot   :H= 'Dyelot'  :R,;
         UnitQty  :H= 'Units'  :R,;
         UnitCost :H= 'Cost'   :P='9999999.99999' :R,;
         ItemQty  :H= 'Item Quantity',;
         ItemAmt  :H= 'Item Amount';
         WINDOW (lcWinCh31)  ;
         IN WINDOW (lcWinCh3);
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
         NOCLEAR           ;
         WHEN lfShowDet()  ;
         VALID :F lfvBrows();
         TITLE lcDetTitle
    *B607340,1 AMH [End]
    
    *B603335,1 SSH 11/04/00[End]
    *B603348,1 ABD [End]
  ELSE
    *B603348,1 ABD Update the picture of filed unitCost upon Increase width of The filed.[Begin]   
    *BROWSE FIELDS ;
         cMarker  =IIF(RECNO()=lnDMarker ,'>',' '):H=' ':R:1:W=.F.,;
         cStyle=IIF(cShowType='0' .AND. !lHideTit,Style,'') :H= lcItmHdr :P=lcItmMsk :R,;
         cItem =IIF(cCatgTyp='M',gfCodDes(MfgCode,'MfgCode'),IIF(cCatgTyp$'FT',SUBSTR(Item,1,7)+'-'+IClr,Item)) :H='Item' :R,;
         UnitQty  :H= 'Units'  :R,;
         UnitCost :H= 'Cost'   :P='999.999' :R,;
         StyQty   :H= 'Pieces' :R ,;
         ItemQty  :H= 'Item Quantity',;
         ItemAmt  :H= 'Item Amount';
         WINDOW (lcWinCh31)  ;
         IN WINDOW (lcWinCh3);
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
         NOCLEAR           ;
         WHEN lfShowDet()  ;
         TITLE lcDetTitle

    *B603335,1 SSH 11/04/00[Begin] Add dyelot to the Detail Requi. Folder.
    *BROWSE FIELDS ;
         cMarker  =IIF(RECNO()=lnDMarker ,'>',' '):H=' ':R:1:W=.F.,;
         cStyle=IIF(cShowType='0' .AND. !lHideTit,Style,'') :H= lcItmHdr :P=lcItmMsk :R,;
         cItem =IIF(cCatgTyp='M',gfCodDes(MfgCode,'MfgCode'),IIF(cCatgTyp$'FT',SUBSTR(Item,1,7)+'-'+IClr,Item)) :H='Item' :R,;
         UnitQty  :H= 'Units'  :R,;
         UnitCost :H= 'Cost'   :P='9999999.99999' :R,;
         StyQty   :H= 'Pieces' :R ,;
         ItemQty  :H= 'Item Quantity',;
         ItemAmt  :H= 'Item Amount';
         WINDOW (lcWinCh31)  ;
         IN WINDOW (lcWinCh3);
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
         NOCLEAR           ;
         WHEN lfShowDet()  ;
         TITLE lcDetTitle

    *B607340,1 AMH Valid before leave the browse to call the gfStopBrow function [Start]
    *BROWSE FIELDS ;
         cMarker  =IIF(RECNO()=lnDMarker ,'>',' '):H=' ':R:1:W=.F.,;
         cStyle=IIF(cShowType='0' .AND. !lHideTit,Style,'') :H= lcItmHdr :P=lcItmMsk :R,;
         cItem =IIF(cCatgTyp='M',gfCodDes(MfgCode,'MfgCode'),IIF(cCatgTyp$'FT',SUBSTR(Item,1,7)+'-'+IClr,Item)) :H='Item' :R,;
         Dyelot   :H= 'Dyelot'  :R,;
         UnitQty  :H= 'Units'  :R,;
         UnitCost :H= 'Cost'   :P='9999999.99999' :R,;
         StyQty   :H= 'Pieces' :R ,;
         ItemQty  :H= 'Item Quantity',;
         ItemAmt  :H= 'Item Amount';
         WINDOW (lcWinCh31)  ;
         IN WINDOW (lcWinCh3);
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
         NOCLEAR           ;
         WHEN lfShowDet()  ;
         TITLE lcDetTitle
    BROWSE FIELDS ;
         cMarker  =IIF(RECNO()=lnDMarker ,'>',' '):H=' ':R:1:W=.F.,;
         cStyle=IIF(cShowType='0' .AND. !lHideTit,Style,'') :H= lcItmHdr :P=lcItmMsk :R,;
         cItem =IIF(cCatgTyp='M',gfCodDes(MfgCode,'MfgCode'),IIF(cCatgTyp$'FT',SUBSTR(Item,1,7)+'-'+IClr,Item)) :H='Item' :R,;
         Dyelot   :H= 'Dyelot'  :R,;
         UnitQty  :H= 'Units'  :R,;
         UnitCost :H= 'Cost'   :P='9999999.99999' :R,;
         StyQty   :H= 'Pieces' :R ,;
         ItemQty  :H= 'Item Quantity',;
         ItemAmt  :H= 'Item Amount';
         WINDOW (lcWinCh31)  ;
         IN WINDOW (lcWinCh3);
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
         NOCLEAR           ;
         WHEN lfShowDet()  ;
         VALID :F lfvBrows();
         TITLE lcDetTitle
    *B607340,1 AMH [End]
    
    *B603335,1 SSH 11/04/00[End]
    *B603348,1 ABD [End]
  ENDIF
  labrowse[3]=.F.
ENDIF
IF labrowse[2]  .AND. lnActFolder=2
  SELECT (lcOprHdr)
  lnTMarker = RECNO()
  
  *B607340,1 AMH Valid before leave the browse to call the gfStopBrow function [Start]
  *BROWSE FIELDS ;
       cMarker  =IIF(RECNO()=lnTMarker,'>',' '):H=' ':R:1:W=.F.,;
       cOprCode  :H= 'Operation' :R,;
       cInHouse=IIF(lInHouse,'Y','N') :H='In House' :R,;
       cOperSeq  :H= 'Sequence' :R ,;
       cDesc=gfCodDes(cOprCode,'MFGCODE') :H='Description' :R,;
       cContCode :H= 'Cont./Dept.' :R,;
       cContName :H= 'Name' :R,;
       cOprComnt :H= 'Comment' :R ;
       WINDOW (lcWinCh21)  ;
       IN WINDOW (lcWinCh2);
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       WHEN lfShOprHd()  ;
       TITLE lcOprHdTit
  BROWSE FIELDS ;
       cMarker  =IIF(RECNO()=lnTMarker,'>',' '):H=' ':R:1:W=.F.,;
       cOprCode  :H= 'Operation' :R,;
       cInHouse=IIF(lInHouse,'Y','N') :H='In House' :R,;
       cOperSeq  :H= 'Sequence' :R ,;
       cDesc=gfCodDes(cOprCode,'MFGCODE') :H='Description' :R,;
       cContCode :H= 'Cont./Dept.' :R,;
       cContName :H= 'Name' :R,;
       cOprComnt :H= 'Comment' :R ;
       WINDOW (lcWinCh21)  ;
       IN WINDOW (lcWinCh2);
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       WHEN lfShOprHd()  ;
       VALID :F lfvBrows();
       TITLE lcOprHdTit
  *B607340,1 AMH [End]

  =lfAdjDetLvl()
  =lfBrowLots()
  labrowse[2]=.F.
ENDIF
SELECT (lcBaseFile)

*!*************************************************************
*! Name      : lfBrowLots
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Browse Cost Sheet Operation Lots
*!*************************************************************
*! Calls     : lfShOprDt
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfBrowLots()
*!*************************************************************
FUNCTION lfBrowLots
PRIVATE lcFields

lcFields = "cMarker=IIF(RECNO()=lnBMarker ,'>',' '):H=' ':R:1:W=.F.,"
FOR lnCount = 1 TO CNTBAR('lcpusort')
  DO case
    CASE laSort[GETBAR('lcpusort',lnCount),1] = 'ITEM' .AND. lcTranType='T'
      *B603243,1 AMM Add dyelot to the browse
      *lcFields = lcFields+"F=SUBSTR(Item,1,7) :H='Fabric' :R,Color :R,"
      lcFields = lcFields+"F=SUBSTR(Item,1,7) :H='Fabric' :R,Color :R,cDyelot :H='Dyelot' :R,"
      *B603243,1 AMM end
      
    CASE laSort[GETBAR('lcpusort',lnCount),1] = 'COPRCODE'
      lcFields = lcFields+"cDesc=gfCodDes(cOprCode,'MFGCODE') :17 :H='Operation' :R,"
    
    OTHERWISE
      lcFields = lcFields+laSort[GETBAR('lcpusort',lnCount),1]+;
                        " :H='"+laSort[GETBAR('lcpusort',lnCount),2]+"':R,"
    *B603243,1 AMM Add dyelot to the browse
    IF laSort[GETBAR('lcpusort',lnCount),1] = 'ITEM'
      lcFields = lcFields + "cDyelot :H='Dyelot' :R,"
    ENDIF
    *B603243,1 AMM end
  ENDCASE
ENDFOR

IF rbSummary = 1
  lcFields = lcFields + "dTranDate:H='Trans. Date':R,DueDate:H='Due Date':R,"+;
             "cType=IIF(TranCd='1','Budget',IIF(TranCd='2','Receive',IIF(TranCd='3','Damaged','Canceled'))):H='Type':R,nLotTotQty:H='Tot.Qty.':R"
  IF cbPerSize
    lcFields = lcFields + ",nLotQty1:H='Qty1':R,nLotQty2:H='Qty2':R,nLotQty3:H='Qty3':R,nLotQty4:H='Qty4':R,"+;
               "nLotQty5:H='Qty5':R,nLotQty6:H='Qty6':R,nLotQty7:H='Qty7':R,nLotQty8:H='Qty8':R"
  ENDIF
ELSE
  lcFields = lcFields+"Iss:H='Issued':R,Rcv:H='Received':R,Can:H='Canceled':R,Dmg:H='Damaged':R,n=MAX(Iss-Rcv-Can-Dmg,0):H='Open':R"
  IF cbPerSize
    lcFields = lcFields+",Iss1:R,Iss2:R,Iss3:R,Iss4:R,Iss5:R,Iss6:R,Iss7:R,Iss8:R"
    lcFields = lcFields+",Rcv1:R,Rcv2:R,Rcv3:R,Rcv4:R,Rcv5:R,Rcv6:R,Rcv7:R,Rcv8:R"
    lcFields = lcFields+",Can1:R,Can2:R,Can3:R,Can4:R,Can5:R,Can6:R,Can7:R,Can8:R"
    lcFields = lcFields+",Dmg1:R,Dmg2:R,Dmg3:R,Dmg4:R,Dmg5:R,Dmg6:R,Dmg7:R,Dmg8:R"
    lcFields = lcFields+;
    ",n1=MAX(Iss1-Rcv1-Can1-Dmg1,0):H='Opn1':R,n2=MAX(Iss2-Rcv2-Can2-Dmg2,0):H='Opn2':R"+;
    ",n3=MAX(Iss3-Rcv3-Can3-Dmg3,0):H='Opn3':R,n4=MAX(Iss4-Rcv4-Can4-Dmg4,0):H='Opn4':R"+;
    ",n5=MAX(Iss5-Rcv5-Can5-Dmg5,0):H='Opn5':R,n6=MAX(Iss6-Rcv6-Can6-Dmg6,0):H='Opn6':R"+;
    ",n7=MAX(Iss7-Rcv7-Can7-Dmg7,0):H='Opn7':R,n8=MAX(Iss8-Rcv8-Can8-Dmg8,0):H='Opn8':R"
  ENDIF
ENDIF
SELECT IIF(rbSummary=1,lcOprDet,lcOprDet1)
lnBMarker = RECNO()
IF WEXIST(lcBrZoom)
  HIDE WINDOW (lcBrZoom) SAME
  RELEASE WINDOW (lcBrZoom)
ENDIF
IF WEXIST(lcOprDtTit)
  HIDE WINDOW (lcOprDtTit) SAME
  RELEASE WINDOW (lcOprDtTit)
ENDIF
IF llZoom
  HIDE WINDOW (lcWinCh21) SAME
  SHOW GETS WINDOW (lcWinCh22) DISABLE ONLY
  HIDE WINDOW (lcWinCh22) SAME
  HIDE WINDOW (lcWinCh23) SAME  
  
  *B607340,1 AMH Valid before leave the browse to call the gfStopBrow function [Start]
  *BROWSE FIELDS &lcFields    ;
         WINDOW (lcWinCh25)  ;
         IN WINDOW (lcWinCh2);
         NOMENU              ;         
         NOAPPEND            ;
         NODELETE            ;         
         NOWAIT              ;
         SAVE                ;
         NOCLEAR             ;
         WHEN lfShOprDt()    ;
         TITLE lcBrZoom
  BROWSE FIELDS &lcFields    ;
         WINDOW (lcWinCh25)  ;
         IN WINDOW (lcWinCh2);
         NOMENU              ;         
         NOAPPEND            ;
         NODELETE            ;         
         NOWAIT              ;
         SAVE                ;
         NOCLEAR             ;
         WHEN lfShOprDt()    ;
         VALID :F lfvBrows();
         TITLE lcBrZoom
  *B607340,1 AMH [End]
  
ELSE
  
  *B607340,1 AMH Valid before leave the browse to call the gfStopBrow function [Start]
  *BROWSE FIELDS &lcFields    ;
         WINDOW (lcWinCh23)  ;
         IN WINDOW (lcWinCh2);
         NOMENU              ;         
         NOAPPEND            ;
         NODELETE            ;         
         NOWAIT              ;
         SAVE                ;
         NOCLEAR             ;
         WHEN lfShOprDt()    ;
         TITLE lcOprDtTit
  BROWSE FIELDS &lcFields    ;
         WINDOW (lcWinCh23)  ;
         IN WINDOW (lcWinCh2);
         NOMENU              ;         
         NOAPPEND            ;
         NODELETE            ;         
         NOWAIT              ;
         SAVE                ;
         NOCLEAR             ;
         WHEN lfShOprDt()    ;
         VALID :F lfvBrows();
         TITLE lcOprDtTit
  *B607340,1 AMH [End]
  
ENDIF

*!*************************************************************
*! Name      : lfWBrow
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Refresh Browse
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfwBrow()
*!*************************************************************
FUNCTION lfwBrow
lnMarker = RECNO(lcTktSheet)

*B605415,1 AMH Fix the bug of variable cost not found [Start]
*SHOW WINDOW (lcWSheet) REFRESH SAME
IF WEXIST(lcWSheet)
  SHOW WINDOW (lcWSheet) REFRESH SAME
ENDIF
*B605415,1 AMH [End]

*!*************************************************************
*! Name      : lfShowDet
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Refresh Browse
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfShowDet()
*!*************************************************************
FUNCTION lfShowDet
lnDMarker = RECNO(lcDetFile)
SHOW WINDOW (lcDetTitle) REFRESH SAME

*!*************************************************************
*! Name      : lfShOprHd
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Refresh Browse
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfShOprHd()
*!*************************************************************
FUNCTION lfShOprHd

=lfRefTotal()
lnTMarker = RECNO(lcOprHdr)
SHOW WINDOW (lcOprHdTit) REFRESH SAME
IF !laScrMode[1] .AND. laData[3]='O' .AND. !EMPTY(cOprCode) .AND. lnActFOlder=2 AND ;
  IIF(laData[28]='S',cOprCode=lcFirstOpr AND lnOprBud=0,.T.)
  SHOW GET pbNewLot ENABLE
ELSE
  SHOW GET pbNewLot DISABLE
ENDIF
*E301427,1 SSH 31/05/20 Disable the recieve button if Pw installed
*E301427,1 SSH          and the operation has detail.
IF llPWInst .AND. lcTranType = "M" 
  IF !USED('pwoperat')
    =gfOpenFile(gcDataDir+"pwoperat" , gcDataDir+"pwoperat" , "SH")
  ENDIF
  IF SEEK(&lcOprHdr..cOprCode,'pwoperat')
    SHOW GET pbRecLot DISABLE
  ELSE
    SHOW GET pbRecLot ENABLE
  ENDIF
ENDIF
*E301427,1 SSH [END]

*!*************************************************************
*! Name      : lfRefTotal
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Show Operation's total Budget, Received, Damaged and canceled quantity
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfRefTotal()
*!*************************************************************
FUNCTION lfRefTotal
PRIVATE lnAlias,lcCurrTag,lnRecNo

*--- Doc. [Start]
*--- TranCd = '1' ==> Budget
*--- TranCd = '2' ==> Receive
*--- TranCd = '3' ==> Dameged
*--- TranCd = '4' ==> Cancel
*--- Doc. [End]

lnAlias = SELECT()
SELECT (lcOprDet)
lnRecNo = RECNO()
lcCurrTag = TAG()
SET ORDER TO TAG 'ITEM'

*B603790,1 AMH store zero value to the open qty [Start]
*STORE 0 TO lnOprBud,lnOprRec,lnOprCan,lnOprDam
STORE 0 TO lnOprBud,lnOprRec,lnOprCan,lnOprDam,lnOprOpn
PRIVATE laOprOpn
DECLARE laOprOpn[8]
STORE 0 TO laOprOpn
*B603790,1 AMH [End]

=SEEK(lcTranType+laData[1]+&lcOprHdr..cOprCode)
SCAN REST WHILE cIMTYp+cTktNo+cOprCode = lcTranType+laData[1]+&lcOprHdr..cOprCode
  lnOprBud = lnOprBud + IIF(TranCd = '1',nLotTotQty,0)
  lnOprRec = lnOprRec + IIF(TranCd = '2',nLotTotQty,0)
  lnOprDam = lnOprDam + IIF(TranCd = '3',nLotTotQty,0)
  lnOprCan = lnOprCan + IIF(TranCd = '4',nLotTotQty,0)
  *B603790,1 AMH Calculate the correct open qty per size [Start]
  PRIVATE lnI,lcI
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    laOprOpn[lnI] = MAX(laOprOpn[lnI] + (EVALUATE('nLotQty'+lcI)*IIF(TranCd='1',1,-1)),0)
  ENDFOR
  *B603790,1 AMH [End]
ENDSCAN

*B603790,1 AMH Calculate the correct open qty per size [Start]
FOR lnI = 1 TO 8
  lnOprOpn = lnOprOpn + laOprOpn[lnI]
ENDFOR
*B603790,1 AMH [End]

IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNO
ENDIF
SET ORDER TO TAG lcCurrTag
=lfRefresh(lcWinCh22)
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfShOprDt
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Refresh Browse
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfShOprDt()
*!*************************************************************
FUNCTION lfShOprDt
lnBMarker = IIF(rbSummary=1,RECNO(lcOprDet),RECNO(lcOprDet1))
IF llZoom
  SHOW WINDOW (lcBrZoom) REFRESH SAME
ELSE
  SHOW WINDOW (lcOprDtTit) REFRESH SAME
ENDIF

*!*************************************************************
*! Name      : lfvNew
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Add new Cost Item
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvNew()
*!*************************************************************
FUNCTION lfvNew
PRIVATE laQty,lnEst1,lnEst2,lnEst3,lnEst4,lnEst5,lcStyItem
*--- DOC.
*--- This functino call style cost sheet program to add new cost
*--- item to the CutTkt Cost sheet.
*--- The new cost item will affect only the cuttkt cost sheet 
*--- not the style cost sheet.
*--- DOC.
DECLARE laQty[9]
*-- Call the program that add a new cost item.
*--- DOC.
*--- This select SQL select all the style cost element from the style cost sheet.
*--- DOC.
SELECT *,RECNO() AS nRecNo , "S" AS cStatus FROM BOM ;
WHERE cItmMajor = laData[2] .AND. IIF(lcTranType='T' , lMaterial , !lMaterial) ;
INTO DBF (gcWorkDir+lcTmpBom)

*C037960,1 MHM create temp file from ORDBOM FILE for Alena [Begin]
IF ASCAN(laEvntTrig , PADR('POCRTBOM',10)) <> 0
  =gfDoTriger('POCSSH',PADR('POCRTBOM',10)) 
ENDIF  
*C037960,1  [End]

INDEX ON citmmajor+typ+IIF(.NOT.(ccatgtyp$"PMD"),citmmask,mfgcode)+item+iclr TAG (lcTmpBom)
IF !USED(lcTmpStyle)
  SELECT STYLE
  =AFIELDS(laFileStru)
  CREATE TABLE (gcWorkDir+lcTmpStyle) FROM ARRAY laFileStru
  INDEX ON Style TAG (lcTmpStyle)
ENDIF
IF !USED(lcTmpFbric)
  SELECT FABRIC
  =AFIELDS(laFileStru)
  CREATE TABLE (gcWorkDir+lcTmpFbric) FROM ARRAY laFileStru
  INDEX ON Fabric+Color TAG (lcTmpFbric)
ENDIF
lcStyItem=laData[2]

*khm1*
*IF lcTranType='I'
IF lcTranType $ 'ID'
*khm1*

  SELECT LEFT(ITEM,LEN(lcMjrMsk)) AS cMajor,SUM(nQty1) AS Qty1,;
         SUM(nQty2) AS Qty2,SUM(nQty3) AS Qty3,SUM(nQty4) AS Qty4,;
         SUM(nQty5) AS Qty5,SUM(nQty6) AS Qty6,SUM(nQty7) AS Qty7,;
         SUM(nQty8) AS Qty8,SUM(nTotQty ) AS TotQty FROM (lcTmpTkt) ;
  GROUP BY cMajor INTO CURSOR (lcisslogfile)
  lcBrFields = ;
       "cMajor :H=ALLTRIM(lcMjrHdr) :R,"+;
       "Qty1 :7 :R,Qty2 :7 :R,Qty3 :7 :R,Qty4 :7 :R,Qty5 :7 :R,Qty6 :7 :R,;
        Qty7 :7 :R,Qty8 :7 :R,TotQty :8 :R"
  *C200080,1 AMM Consider the dye order case
  *lcStyItem=IIF(_TALLY=1 .OR. ARIABROW('','Purchase Order '+ALLTRIM(lcMjrHdr),;
                gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','',;
                'CMAJOR','laBrowArr'),CMAJOR,'')
  lcStyItem=IIF(_TALLY=1 .OR. ARIABROW('',IIF(EMPTY(lcDyePO),'Purchase Order ','Dye Order ')+ALLTRIM(lcMjrHdr),;
                gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','',;
                'CMAJOR','laBrowArr'),CMAJOR,'')
  *C200080,1 AMM end
  USE IN (lcisslogfile)
  SELECT (lcTktSheet)
  IF EMPTY(lcStyItem)
    RETURN
  ENDIF  
ENDIF
llChngMode = .F.
IF laScrMode[2]
  STORE .F. TO laScrMode
  STORE .T. TO laScrMode[4],llChngMode
ENDIF

*--- Doc. [Start]
*--- Call Style cost sheet add/new item screen
*--- The lase parameter '.T.' for generate cost sheet by size.
*--- Doc. [End]
*E301427,1 SSH 31/05/20 Preparing for caling style cost sheet.
*--- llFromCtk : Variable For Routing Screen.
*--- Since we are running this screen from either Style Cost Sheet Screen
*--- or C/T Cost sheet screen and there is Two  object we need to them to be 
*--- displayed If called from C/T other wise they will be hidden.
IF llPWInst .AND. lcTranType = "M" 
  llFromCtk = .T.
  PUSH KEY
  ON KEY
  lcPWBom   = ''
ENDIF
*E301427,1 SSH 31/05/20 [END]

*B603625,1 AMH Changing the parameter passing of the first parameter
*B603625,1     by taking the dye order into consideration [Start]
*DO (gcAppHome+"MFCSTSC") WITH lcTranType , lcStyItem , lcTmpBom , ;
                              lcTmpFbric , lcTmpStyle , .T.
DO (gcAppHome+"MFCSTSC") WITH IIF(EMPTY(lcDyePO),lcTranType,'M') , lcStyItem , lcTmpBom , ;
                              lcTmpFbric , lcTmpStyle , .T.
*B603625,1 AMH [End]

*E301427,1 SSH 31/05/20 Restore old keys.
IF llPWInst .AND. lcTranType = "M" 
  POP KEY
ENDIF
*E301427,1 SSH [END]
IF llChngMode
  STORE .F. TO laScrMode
  STORE .T. TO laScrMode[2]
ENDIF

*B802378,1 Start, Add new items before calling gfSheetItem.
=lfAddNewItm()
*B802378,1 End.

SELECT (lcTmpBom)
*--- DOC.
*--- The next command is to delete all the old cost element.
*--- since that we need only to add the new ones
*--- DOC.
DELETE ALL FOR cStatus <> 'A'
LOCATE
IF !EOF()
  SELECT (lcTranFile)
  *C200080,1 AMM Consider the dye order case
  *=SEEK(IIF(lcTranType='I','P','')+laData[1]+IIF(lcTranType='I',lcStyItem,''))
  *SCAN REST WHILE EVAL(lcFileKey)=IIF(lcTranType='I','P','')+laData[1] ;
            FOR TranCd='1' .AND. IIF(lcTranType='I',Style=lcStyItem,.T.)
  
  *khm1*
  *=SEEK(IIF(lcTranType='I',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1]+IIF(lcTranType='I',lcStyItem,''))
  *SCAN REST WHILE EVAL(lcFileKey)=IIF(lcTranType='I',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1] ;
            FOR TranCd='1' .AND. IIF(lcTranType='I',Style=lcStyItem,.T.)
  
  =SEEK(IIF(lcTranType $ 'ID',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1]+IIF(lcTranType $ 'ID',lcStyItem,''))
  SCAN REST WHILE EVAL(lcFileKey)=IIF(lcTranType $ 'ID',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1] ;
            FOR TranCd='1' .AND. IIF(lcTranType $ 'ID',Style=lcStyItem,.T.)
  *khm1*
  *C200080,1 AMM end
    STORE 0 TO laQty,lnEst1,lnEst2,lnEst3,lnEst4,lnEst5
    IF lcTranType <> 'T'
      laQty[1] = Qty1
      laQty[2] = Qty2
      laQty[3] = Qty3
      laQty[4] = Qty4
      laQty[5] = Qty5
      laQty[6] = Qty6
      laQty[7] = Qty7
      laQty[8] = Qty8
    ENDIF
    laQty[9]= IIF(lcTranType='T',nMfgTotQty,TotQty)
    lcLastOpr = laData[30]
    *C200080,1 AMM Consider the dye order case in calling the function
    *IF gfSheetItem(lcTranType,laData[1],laData[29],IIF(lcTranType='T',cFabric,Style),IIF(lcTranType='T',Color,''),;
                IIF(lcTranType='T',0,&lcTranFile..LineNo),Dyelot,laData[31],;
                laData[32],@laQty,lcTmpBom,;
                IIF(laData[3]='O','CTKTBOM',lcTktSheet),;
                IIF(laData[3]='O','BOMLINE',lcDetFile),;
                IIF(laData[3]='O','MFGOPRHD',lcOprHdr),@lcLastOpr,;
                IIF(lcTranType='I',PosLn.nCost1,0),;
                @lnEst1,@lnEst2,@lnEst3,@lnEst4,@lnEst5,lcDetFile)
    *B802507,1 AMM send master files not temporary ones as parameters in all statuses except hold.
    *IF gfSheetItem(IIF(lcDyePO='D','D',lcTranType),laData[1],laData[29],IIF(lcTranType='T',cFabric,Style),IIF(lcTranType='T',Color,''),;
                IIF(lcTranType='T',0,&lcTranFile..LineNo),Dyelot,laData[31],;
                laData[32],@laQty,lcTmpBom,;
                IIF(laData[3]='O','CTKTBOM',lcTktSheet),;
                IIF(laData[3]='O','BOMLINE',lcDetFile),;
                IIF(laData[3]='O','MFGOPRHD',lcOprHdr),@lcLastOpr,;
                IIF(lcTranType='I',PosLn.nCost1,0),;
                @lnEst1,@lnEst2,@lnEst3,@lnEst4,@lnEst5,lcDetFile)
*--- Doc. [Start]
*--- gfSheetItem == Generate, Modify or Delete cost sheet items for style
*--- Doc. [End]
     
     IF gfSheetItem(IIF(lcDyePO='D','D',lcTranType),laData[1],laData[29],IIF(lcTranType='T',cFabric,Style),IIF(lcTranType='T',Color,''),;
                IIF(lcTranType='T',0,&lcTranFile..LineNo),Dyelot,laData[31],;
                laData[32],@laQty,lcTmpBom,;
                IIF(laData[3]<>'H','CTKTBOM',lcTktSheet),;
                IIF(laData[3]<>'H','BOMLINE',lcDetFile),;
                IIF(laData[3]<>'H','MFGOPRHD',lcOprHdr),@lcLastOpr,;
                IIF(lcTranType='I',PosLn.nCost1,0),;
                @lnEst1,@lnEst2,@lnEst3,@lnEst4,@lnEst5,lcDetFile)                

    
    *B802507,1 AMM end
    *C200080,1 AMM end
       laData[12] = laData[12] + lnEst1
       laData[13] = laData[13] + lnEst2
       laData[14] = laData[14] + lnEst3
       laData[15] = laData[15] + lnEst4
       laData[16] = laData[16] + lnEst5
       laData[30] = lcLastOpr
     ENDIF
  ENDSCAN
  *E301427,1 SSH Call PW Function to generate cost sheet for detail operation.
  *E301427,1 Only if PW module installed and Call this program to Generate
  *E301427,1 C/T Cost Sheet.
  llDummy = llPWInst .AND. (lcTranType = "M") .AND. lfPwShtItm('A',.T.)
  *E301427,1 [END]
  *--- DOC.
  *--- Function to update estimated cost.
  *--- DOC.
  =lfUpdEstCst()
  *B802507,1 AMM Handle rest of statuses as open to fix bug of not updating files with added items
  *IF laData[3]='O'
  IF laData[3]<>'H'
  *B802507,1 AMM end
    =lfGetInfo()
    STORE .T. TO labrowse

    *B604294,1 KHM 04/30/2001 (Begin) Calling lfRecCost to recalculate
    *B604294,1                the duty when its a percentage. The
    *B604294,1                first time you create the PO cost sheet.
    IF lcTranType = 'I'
      =lfRecCost(lcTranType,laData[1],'BOMLINE','CTKTBOM')
      SHOW GETS
    ENDIF
    *B604294,1 KHM 04/30/2001 (End)

    =lfBrowse()
  ELSE
    SELECT (lcTktSheet)
    SET ORDER TO TAG (lcTktSheet)
    *--- DOC.
    *--- Start update the temp file that used in the folder cost sheet.
    *--- to brows the cost item from , this temp file is the header cost 
    *--- sheet file.
    *--- DOC.
    FOR lnType = 1 TO 5
      DO WHILE SEEK(STR(lnType,1)+SPACE(1))
        REPLACE cShowType WITH '1'
        IF !SEEK(STR(lnType,1)+'0')
          INSERT INTO (lcTktSheet) (TYP,cShowType,Item) VALUES ;
          (STR(lnType,1),'0',lfLblDesc(laSetups[2+lnType,2]))
        ENDIF
      ENDDO
    ENDFOR
    GO TOP
    SELECT *,RECNO() AS nRecNo FROM (lcDetFile) WHERE EMPTY(cShowType) INTO CURSOR tmpBomLine
    SET ORDER TO TAG (lcDetFile) IN (lcDetFile)
    *--- DOC.
    *--- Start update file tmpBomLine, This file is copy from CTKTBOM file
    *--- DOC.
    SELECT tmpBomLine
    SCAN
      IF !EMPTY(MfgCode) AND SEEK(lcTranType+laData[1]+MfgCode,lcOprHdr) ;
        AND SEEK(Style+SClr+STR(LineNo,6),lcTmpTkt)
        SELECT (lcTmpTkt)
        *B802700,1 Get the smallest op seq[Start] 
        *REPLACE cFrstOpr WITH IIF(nFrstOprSq=0 OR ;
        (VAL(&lcOprHdr..cOperSeq) < nFrstOprSq),&lcOprHdr..cOprCode,cFrstOpr) ,;
                nFrstOprSq WITH VAL(&lcOprHdr..cOperSeq)
        SELECT MIN(cOperSeq) FROM (lcOprHdr) INTO ARRAY laTmp1      
        IF _TALLY > 0
          SELECT coprCode FROM (lcOprHdr) WHERE cOperSeq = laTmp1[1] INTO ARRAY laTmp2        
          REPLACE cFrstOpr WITH laTmp2[1],;
                  nFrstOprSq WITH VAL(laTmp1[1])                
        
        ENDIF
        *B802700,1 Get the smallest op seq[End..]                 
      ENDIF
      SELECT tmpBomLine
      IF !SEEK(Style+SClr+STR(LineNo,6)+cBomTyp+'0',lcDetFile)
        SELECT (lcDetFile)
        =SEEK(tmpBomLine.Style+tmpBomLine.SClr+STR(tmpBomLine.LineNo,6))
        LOCATE REST WHILE Style+SClr+STR(LineNo,6) =;
        tmpBomLine.Style+tmpBomLine.SClr+STR(tmpBomLine.LineNo,6) FOR !EMPTY(cShowType)
        llHideTit = FOUND()
        INSERT INTO (lcDetFile) ;
        (Style,SClr,LineNo,cBomTyp,cShowType,Item,lHideTit) VALUES ;
        (tmpBomLine.Style,tmpBomLine.SClr,tmpBomLine.LineNo,tmpBomLine.cBomTyp,'0',;
         lfLblDesc(laSetups[2+VAL(tmpBomLine.cBomTyp),2]),llHideTit)
      ENDIF
      SELECT (lcDetFile)
      GOTO tmpBomLine.nRecNo
      REPLACE cShowType WITH '1'
      IF !SEEK(Style+SClr+STR(LineNo,6)+cBomTyp+'2',lcDetFile)
        INSERT INTO (lcDetFile) ;
        (Style,SClr,LineNo,cBomTyp,cShowType,Item) VALUES ;
        (tmpBomLine.Style,tmpBomLine.SClr,tmpBomLine.LineNo,tmpBomLine.cBomTyp,'2','***** Total :')
      ENDIF
      SELECT (lcDetFile)
      REPLACE ItemQty  WITH ItemQty + tmpBomLine.ItemQty,;
              ItemAmt  WITH ItemAmt + tmpBomLine.ItemAmt
    ENDSCAN
    USE IN tmpBomLine
    GO TOP IN (lcDetFile)
 
    *B604294,1 KHM 04/30/2001 (Begin) Calling lfRecCost to recalculate
    *B604294,1                the duty when its a percentage. When adding
    *B604294,1                a new cost element.
    IF lcTranType = 'I'
      =lfRecCost(lcTranType,laData[1],lcDetFile,lcTktSheet)
    ENDIF 
    *B604294,1 KHM 04/30/2001  (End)

  ENDIF
  =lfwBrow()
ENDIF
SELECT (lcTktSheet)

*!*************************************************************
*! Name      : lfvRemove
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Remove Cost Item
*!*************************************************************
*! Calls     : gfModalGen,lfwBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvRemove()
*!*************************************************************
FUNCTION lfvRemove
PRIVATE lcType,lnRecNo,lnItemCnt

*--- Doc. [Start]
*--- To remove cost item
*--- 1- Delete its record from CTKTBOM.DBF
*--- 2- Delete all Estimated/Adjusted record from BOMLINE.DBF
*--- 3- Update Estemated Cost. (CutTktH/PoS/POF..)
*--- 4- Delete its record from MFGOPRHD.DBF
*--- Doc. [End]
*B602940,1 SSH 01/05/00 [Start]
SELECT (lcTktSheet)
*B602940,1 SSH 01/05/00 [End]
IF cShowType <> '1'
  RETURN
ENDIF
lnRecNo = RECNO()
COUNT ALL FOR cShowType = '1' TO lnItemCnt
GO lnRECNO
IF lnItemCnt = 1
  *E300725,1 Message : 38056
  *E300725,1 Cannot remove all cost items
  *E300725,1 Button : 00000
  *E300725,1 Ok
  =gfModalGen('TRM38056B00000','ALERT')
  RETURN
ENDIF
IF &lcTktSheet..cCatgTyp = 'P'
  *E300725,1 Message : 38035
  *E300725,1 Cannot remove the purchase cost.
  *E300725,1 Button : 00000
  *E300725,1 Ok
  =gfModalGen('TRM38035B00000','ALERT','remove')
  RETURN
ENDIF
llLanFound = .F.
SELECT BOMLINE
SET ORDER TO BOMPOREC DESCENDING
=SEEK(lcTranType+'2'+&lcTktSheet..TYP+&lcTktSheet..MFGCODE+laData[1])
LOCATE REST WHILE cIMTyp+cType+cBomTyp+MfgCode+cTktNo+cRSession+ShipNo+;
                  STR(lineno,6)+Style+SClr = ;
                  lcTranType+'2'+&lcTktSheet..TYP+&lcTktSheet..MFGCODE+;
                  laData[1] .AND. !EMPTY(cRsession) ;
            FOR   Item+IClr =&lcTktSheet..Item+&lcTktSheet..IClr
IF FOUND()
  *E300725,1 Message : 38038
  *E300725,1 This record has been considered as landed cost for the receiving
  *E300725,1 session 999999. Cannot delete.
  *E300725,1 Button : 00000
  *E300725,1 Ok
  =gfModalGen('TRM38038B00000','ALERT',CRSESSION)
  RETURN
ENDIF
SELECT (lcTktSheet)
IF SEEK(cIMTyp+CutTkt+Typ+Item+IClr+MfgCode+Dyelot,'CTKTBOM') .AND. ;
   cTktBom.Used_Qty <> 0
  *E300725,1 Message : 38039
  *E300725,1 Quantity has been issued from this record.
  *E300725,1 You have to return this quantity before delete.
  *E300725,1 Button : 00000
  *E300725,1 Ok
  =gfModalGen('TRM38039B00000','ALERT')
  RETURN
ENDIF
IF &lcTktSheet..cCatgTyp='M' .AND. SEEK(lcTranType+laData[1]+&lcTktSheet..MfgCode,lcOprDet)
  *E300725,1 Message : 38117
  *E300725,1 Lots have been issued from operation xxxx. Cannot remove.
  *E300725,1 Button : 00000
  *E300725,1 Ok
  =gfModalGen('TRM38117B00000','ALERT',ALLTRIM(gfCodDes(&lcTktSheet..MfgCode,'MfgCode')))
  RETURN
ENDIF
*E300725,1 Message : 38036
*E300725,1 Are you sure you want to remove this record?
*E300725,1 Button : 38002
*E300725,1 Proceed  Cancel
IF gfModalGen('QRM38036B38002','ALERT','remove') = 2
  RETURN
ENDIF
lcType = &lcTktSheet..TYP
IF SEEK(cIMTyp+CutTkt+Typ+Item+IClr+MfgCode+Dyelot,'CTKTBOM')
  SELECT CTKTBOM
  DELETE
ENDIF

*E301427,1 SSH Call function to remove Pw detail operation.
*--- PwCtkBom Key == cuttkt+mfgcode+coprcode+STR(nlineno,6)
llDummy = llPWInst .AND. lcTranType = "M" .AND. lfPwRem(CutTkt , MfgCode)
*E301427,1 SSH [END]

laData[VAL(lcType)+11]=laData[VAL(lcType)+11]-&lcTktSheet..Est_Cost
IF SEEK(lcTranType+'1'+lcType+&lcTktSheet..MFGCODE+laData[1],'BOMLINE')
  SELECT BOMLINE
  DELETE REST WHILE CIMTYP+CTYPE+CBOMTYP+MFGCODE+CTKTNO= ;
                    lcTranType+'1'+lcType+&lcTktSheet..MFGCODE+laData[1] ;
              FOR   Item+IClr =&lcTktSheet..Item+&lcTktSheet..IClr
ENDIF
IF SEEK(lcTranType+'2'+lcType+&lcTktSheet..MFGCODE+laData[1],'BOMLINE')
  SELECT BOMLINE
  DELETE REST WHILE CIMTYP+CTYPE+CBOMTYP+MFGCODE+CTKTNO= ;
                    lcTranType+'2'+lcType+&lcTktSheet..MFGCODE+laData[1] ;
              FOR   Item+IClr =&lcTktSheet..Item+&lcTktSheet..IClr
ENDIF
*--- DOC.
*--- Start update the estimated cost , after remove cost element.
*--- DOC.
=lfUpdEstCst()
*E300725,1 Update the corresponding estimated cost in the header file
IF laScrMode[2]
  DO CASE
    CASE lcTranType = 'M'
      SELECT CUTTKTH
      =SEEK(laData[1])
      REPLACE nEst_Cost&lcType WITH nEst_Cost&lcType - &lcTktSheet..Est_Cost
    
    *khm1*
    *CASE lcTranType = 'I'
    CASE lcTranType $ 'ID'
    *khm1*
        
      SELECT POSHDR
      *C200080,1 AMM Consider the dye order case
      *=SEEK('P'+laData[1])
      =SEEK(IIF(EMPTY(lcDyePO),'P','D')+laData[1])
      *C200080,1 AMM end
      REPLACE nICost&lcType WITH nICost&lcType - &lcTktSheet..Est_Cost
      DO CASE
        CASE &lcTktSheet..cCatgTyp='P'
          REPLACE nFCost&lcType WITH nFCost&lcType - &lcTktSheet..Est_Cost/(1 &lcPExSign POSHDR.nPriceRat &lcPUntSin POSHDR.nCurrUnit)
        CASE INLIST(&lcTktSheet..cCatgTyp,'S','T','F')
          REPLACE nFCost&lcType WITH nFCost&lcType - &lcTktSheet..Est_Cost
        OTHERWISE
           REPLACE nFCost&lcType WITH nFCost&lcType - &lcTktSheet..Est_Cost/(1 &lcDExSign POSHDR.nDutyRat &lcDUntSin POSHDR.nDCurUnit)
      ENDCASE
    CASE lcTranType = 'T'
      SELECT MMFGORDH
      =SEEK(laData[1])
      REPLACE nEst_Cost&lcType WITH nEst_Cost&lcType - &lcTktSheet..Est_Cost
  ENDCASE
ENDIF
IF &lcTktSheet..cCatgTyp='M'
  SELECT (lcOprHdr)
  SET ORDER TO TAG MFGOPRHD
  IF SEEK(lcTranType+laData[1]+&lcTktSheet..MfgCode)
    DELETE
  ENDIF
  SET ORDER TO TAG (lcOprHdr) DESCENDING
  GO TOP
  laData[30] = cOprCode
  SET ORDER TO TAG (lcOprHdr) ASCENDING
  GO TOP
  lcFirstOpr = cOprCode
  IF SEEK(lcTranType+laData[1]+&lcTktSheet..MfgCode,'MFGOPRHD')
    SELECT MFGOPRHD
    DELETE
    SELECT (lcBaseFile)
    REPLACE CLASTOPR WITH laData[30]
  ENDIF
ENDIF
SELECT (lcDetFile)
DELETE ALL FOR cBomTyp+cShowType+Item+IClr+MfgCode = ;
              &lcTktSheet..TYP+'1'+&lcTktSheet..Item+&lcTktSheet..IClr+&lcTktSheet..MfgCode
GO TOP
DO WHILE !EOF()
  lcCurrKey  = Style+SClr+STR(LineNo,6)
  IF !SEEK(lcCurrKey+&lcTktSheet..TYP+'1')
    =SEEK(lcCurrKey+&lcTktSheet..TYP)
    DELETE REST WHILE Style+SClr+STR(LineNo,6)+cBomTyp+cShowType+Item+IClr+MfgCode  = lcCurrKey+&lcTktSheet..TYP
  ELSE
    SUM REST ItemQty,ItemAmt TO lnTotQty,lnTotAmt ;
    WHILE Style+SClr+STR(LineNo,6)+cBomTyp+cShowType+Item+IClr+MfgCode  = lcCurrKey+&lcTktSheet..TYP+'1'
    REPLACE ItemQty WITH lnTotQty ,;
            ItemAmt WITH lnTotAmt 
  ENDIF
  IF SEEK(lcCurrKey)
    REPLACE lHideTit WITH .F.
    SCAN REST WHILE Style+SClr+STR(LineNo,6)+cBomTyp+cShowType+Item+IClr+MfgCode  = lcCurrKey
    ENDSCAN
  ELSE
    GO TOP
  ENDIF
ENDDO
GO TOP
SELECT (lcTktSheet)
DELETE
IF !SEEK(lcType+'1')
  =SEEK(lcType+'0')
  DELETE
ENDIF

*B604294,1 KHM 04/30/2001 (Begin) Calling lfRecCost to recalculate
*B604294,1                the duty when its a percentage. When removing
*B604294,1                a cost element.
IF lcTranType = 'I'
  =lfRecCost(lcTranType,laData[1],IIF(laData[3]<>'H','BOMLINE',lcDetFile),IIF(laData[3]<>'H','CTKTBOM',lcTktSheet))
  SHOW GETS
ENDIF
*B604294,1 KHM 04/30/2001  (End)

GO TOP
=lfwBrow()
SET ORDER TO TAG BOMLINE IN BOMLINE
RETURN

*!*************************************************************
*! Name      : lfvModify
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Modify Cost Item
*!*************************************************************
*! Calls     : MFITMDET.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvModify()
*!*************************************************************
FUNCTION lfvModify

*--- DOC.
*--- Valid function for the modify button in the cost sheet folder
*--- DOC.
SELECT (lcTktSheet)
*--- DOC.
*--- If the current record is the cost element descriptoin record.
*--- the record that define the cost elemt , if MFg, fabric ..
*--- this functoin is tomodify the unit, Pieces and unitcost.
*--- DOC.
IF cShowType = '0'
  RETURN
ENDIF
SCATTER MEMVAR
DO CASE
  CASE INLIST(m.cCatgTyp,'P','D')
    SELECT (lcDetFile)
    LOCATE FOR cBomTyp+Item+IClr+MfgCode+cShowType=m.Typ+m.Item+m.IClr+m.MfgCode+'1'
    SUM StyQty*UnitQty,UnitCost*StyQty*UnitQty TO m.Req_Qty,m.Est_Cost ;
        FOR cBomTyp+Item+IClr+MfgCode+cShowType = m.Typ+m.Item+m.IClr+m.MfgCode+'1'
    m.UntCost = m.Est_Cost/m.Req_Qty
  CASE m.cCatgTyp='S' .AND. SEEK(m.Item,'Style')
    =SEEK('S'+Style.Scale,'Scale')
ENDCASE
*E301427,1 SSH Check if the current Mfg Operation has detail oeration or not.
llHideDet = .T.
*B603867,4 SSH 31/08/2000 [Start Commented Out
IF llPwInst
*B603867,4 SSH 31/08/2000 END]
  PRIVATE lnOldAls
  lnOldAls = SELECT(0)
  =gfOpenFile(gcDataDir+"pwoperat" , gcDataDir+"pwoperat" , "SH")
  llHideDet = !SEEK(&lcTktSheet..mfgCode,'pwoperat')
  SELECT(lnOldAls)
*B603867,4 SSH 31/08/2000 [Start Commented Out
ENDIF
*B603867,4 SSH 31/08/2000 END]
*E301427,1 SSH [END]
DO (gcScrDir+"MFITMDET.SPX")

*B604294,1 KHM 04/30/2001 (Begin) Calling lfRecCost to recalculate
*B604294,1                the duty when its a percentage. When modifying
*B604294,1                a cost element.
IF lcTranType = 'I'
  =lfRecCost(lcTranType,laData[1],IIF(laData[3]<>'H','BOMLINE',lcDetFile),IIF(laData[3]<>'H','CTKTBOM',lcTktSheet))
  SHOW GETS
ENDIF  
*B604294,1 KHM 04/30/2001 (End)

*!*************************************************************
*! Name      : lfvAccMod
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Accept Modification
*!*************************************************************
*! Calls     : gfModalGen,MFDSTRB.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvAccMod()
*!*************************************************************
FUNCTION lfvAccMod
PARAMETER llFrmDetal
*--- New parameter : llFrmDetal:
*E301427,1 SSH Add New parameter to indicate if comming from detail oper. button.
*E301427,1 SSH llOk : variable to clear read if read level exceed 5
*PRIVATE lnAlias,lnLines,lnUnitQty,lnUnitCost,llUQtySame,llUCstSame,lcMsgTit,lcExSign
PRIVATE lnAlias,lnLines,lnUnitQty,lnUnitCost,llUQtySame,llUCstSame,lcMsgTit,lcExSign,llOk
*E301427,1 SSH [END]
lnAlias = SELECT()
STORE '/' TO lcExSign,lcUntSin
STORE 0   TO lnLines,lnPieces,lnOldValue,lnOldEstCost
STORE .T. TO llUQtySame,llUCstSame
SELECT (lcDetFile)
LOCATE FOR cBomTyp+Item+IClr+MfgCode+cShowType=m.Typ+m.Item+m.IClr+m.MfgCode+'1'
lnUnitQty  = UnitQty
lnUnitCost = UnitCost
*--- DOC.
*--- Get the current pieces , qty and unit cost.
*--- DOC.
SCAN REST FOR cBomTyp+Item+IClr+MfgCode+cShowType=m.Typ+m.Item+m.IClr+m.MfgCode+'1'
  lnLines      = lnLines + 1
  lnPieces     = lnPieces + StyQty
  llUQtySame   = IIF(lnUnitQty  <> UnitQty, .F.,llUQtySame)
  llUCstSame   = IIF(lnUnitCost <> UnitCost,.F.,llUCstSame)
  lnOldEstCost = lnOldEstCost + (UnitCost*StyQty*UnitQty)
ENDSCAN
lcMsgTit = IIF(lcTranType='T','Fabric/Color',lcItmHdr)
*E300725,1 Message : 38067
*E300725,1 The yield was the same for all style/colors. Do you wish to apply
*E300725,1 the same modification for all, or edit the yield for each style/color?
*E300725,1 Button : 38009
*E300725,1 Apply to All  Edit

*E300725,1 Message : 38068
*E300725,1 The yield was different for each style/color. Do you wish to edit
*E300725,1 the yield for each style/color, or Apply the same modification for all?
*E300725,1 Button : 38008
*E300725,1 Edit  Apply to All

*E301427,1 SSH Commented out and add conditon if we are calling from detial operaion butt.
*IF lnLines > 1 .AND. ;
  ((&lcTktSheet..Pieces <> m.Pieces)  .OR. ;
   (&lcTktSheet..UntCost <> m.UntCost .AND. !llUCstSame) .OR. ;
   (&lcTktSheet..UntQty  <> m.UntQty  .AND. ;
    ((!llUQtySame .AND. gfModalGen('QRM38068B38008','ALERT',lcMsgTit+'|'+lcMsgTit)=1) .OR. ;
     (llUQtySame  .AND. gfModalGen('QRM38067B38009','ALERT',lcMsgTit+'|'+lcMsgTit)=2))))
IF llFrmDetal .OR. (lnLines > 1 .AND. ;
  ((&lcTktSheet..Pieces <> m.Pieces)  .OR. ;
   (&lcTktSheet..UntCost <> m.UntCost .AND. !llUCstSame) .OR. ;
   (&lcTktSheet..UntQty  <> m.UntQty  .AND. ;
    ((!llUQtySame .AND. gfModalGen('QRM38068B38008','ALERT',lcMsgTit+'|'+lcMsgTit)=1) .OR. ;
     (llUQtySame  .AND. gfModalGen('QRM38067B38009','ALERT',lcMsgTit+'|'+lcMsgTit)=2)))))
*E301427,1 SSH [END]
  SELECT * FROM (lcDetFile) ;
  WHERE cBomTyp+Item+IClr+MfgCode=m.Typ+m.Item+m.IClr+m.MfgCode ;
  INTO DBF (gcWorkDir+lcisslogfile)
  *E301427,1 SSH IF not comming from detail operation butt. call the destributed
  *E301427,1 SSH screen it was.
  IF !llFrmDetal
    DO (gcScrDir+"MFDSTRB.SPX")
  ELSE
    *E301427,1 SSH IF comming from detail operation butt. 
    *E301427,1 SSH call function to check if we are going to open style brow
    IF lfOpnStBrw(m.MfgCode)
      *E301427,1 SSH The following loop because we need to clear read
      *E301427,1 SSH because the read level is 5 now.
      DO WHILE .T.
        llOk  = .F.
        DO (gcScrDir+"MFDSTRB.SPX")
        IF !llOk
          EXIT
        ELSE
          llOk = .F.
          lnTempCost = lfvSelDet()
          *REPLACE UnitCost WITH IIF(lnTempCost = 0,UnitCost,(lnTempCost/ItemQty))
          REPLACE UnitCost WITH IIF(lnTempCost = 0,UnitCost,lnTempCost)
          *--- Function to update cost.
          =lfvUpdate()
        ENDIF
      ENDDO
    ELSE
      *--- Else if we are not going to display style browse.
      *--- So we will directly call detail operation screen.
      lnNewCost = lfvSelDet()
      *--- If the returned value = 0 this mean the user press close/cancel
      *--- from the dateil screen
      IF lnNewCost <> 0
        m.UntCost  = lnNewCost
      ENDIF
      *--- then update the cost sheet files as it was
      SELECT (lcDetFile)
      LOCATE FOR cBomTyp+Item+IClr+MfgCode+cShowType=m.Typ+m.Item+m.IClr+m.MfgCode+'1'
      REPLACE REST ;
              UnitCost WITH m.UntCost ,;
              UnitQty  WITH m.UntQty  ,;
              StyQty   WITH IIF(lnLines=1,m.Pieces,StyQty)    ,;
              ItemQty  WITH StyQty*UnitQty ,;
              ItemAmt  WITH StyQty*UnitQty*UnitCost ;
      FOR cBomTyp+Item+IClr+MfgCode+cShowType=m.Typ+m.Item+m.IClr+m.MfgCode+'1'
      IF laScrMode[2] .AND. SEEK(lcTranType+'1'+laData[1],'BomLine')
        SELECT BomLine
        LOCATE REST WHILE ;
        cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode =;
        lcTranType+'1'+laData[1] ;
        FOR cbomtyp+item+iclr+mfgcode = m.Typ+m.Item+m.IClr+m.MfgCode
        IF FOUND()
          REPLACE StyQty   WITH m.Pieces       ,;
                  UnitCost WITH m.UntCost      ,;
                  UnitQty  WITH m.UntQty       ,;
                  ItemQty  WITH StyQty*UnitQty ,;
                  ItemAmt  WITH StyQty*UnitQty*UnitCost
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  *E301427,1 SSH [END]
  USE IN (lcisslogfile)
  SELECT (lcDetFile)
ELSE
  LOCATE FOR cBomTyp+Item+IClr+MfgCode+cShowType=m.Typ+m.Item+m.IClr+m.MfgCode+'1'
  REPLACE REST ;
          UnitCost WITH m.UntCost ,;
          UnitQty  WITH m.UntQty  ,;
          StyQty   WITH IIF(lnLines=1,m.Pieces,StyQty)    ,;
          ItemQty  WITH StyQty*UnitQty ,;
          ItemAmt  WITH StyQty*UnitQty*UnitCost ;
  FOR cBomTyp+Item+IClr+MfgCode+cShowType=m.Typ+m.Item+m.IClr+m.MfgCode+'1'
  IF laScrMode[2] .AND. SEEK(lcTranType+'1'+laData[1],'BomLine')
    SELECT BomLine
    LOCATE REST WHILE ;
    cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode =;
    lcTranType+'1'+laData[1] ;
    FOR cbomtyp+item+iclr+mfgcode = m.Typ+m.Item+m.IClr+m.MfgCode
    IF FOUND()

      *B604818,1 AMH Updating all the styles that have the modified cost
      *B604818,1     elements [Start]
      *REPLACE StyQty   WITH m.Pieces       ,;
              UnitCost WITH m.UntCost      ,;
              UnitQty  WITH m.UntQty       ,;
              ItemQty  WITH StyQty*UnitQty ,;
              ItemAmt  WITH StyQty*UnitQty*UnitCost
      
      REPLACE REST StyQty   WITH IIF(lnLines=1,m.Pieces,StyQty) ,;
              UnitCost WITH m.UntCost      ,;
              UnitQty  WITH m.UntQty       ,;
              ItemQty  WITH StyQty*UnitQty ,;
              ItemAmt  WITH StyQty*UnitQty*UnitCost ;
      WHILE ;
      cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode =;
      lcTranType+'1'+laData[1] ;
      FOR cbomtyp+item+iclr+mfgcode = m.Typ+m.Item+m.IClr+m.MfgCode
      *B604818,1 AMH [End]
      
    ENDIF
  ENDIF
ENDIF
SELECT (lcDetFile)
SUM StyQty,StyQty*UnitQty,UnitCost*ItemQty TO m.Pieces,m.Req_Qty,m.Est_Cost ;
    FOR cBomTyp+Item+IClr+MfgCode+cShowType = m.Typ+m.Item+m.IClr+m.MfgCode+'1'
lnFEstCst  = m.Est_Cost 
lnOFEstCst = lnOldEstCost
DO CASE
  CASE lcTranType='I' AND m.cCatgTyp='P'
    m.Est_Cost   = m.Est_Cost   &lcPExSign POSHDR.nPriceRat &lcPUntSin POSHDR.nCurrUnit
    lnOldEstCost = lnOldEstCost &lcPExSign POSHDR.nPriceRat &lcPUntSin POSHDR.nCurrUnit
  
  *khm1*
  *CASE lcTranType='I' AND !INLIST(m.cCatgTyp,'S','F','T')
  CASE lcTranType $ 'ID' AND !INLIST(m.cCatgTyp,'S','F','T')
  *khm1*
  
    m.Est_Cost   = m.Est_Cost   &lcDExSign POSHDR.nDutyRat &lcDUntSin POSHDR.nDCurUnit
    lnOldEstCost = lnOldEstCost &lcDExSign POSHDR.nDutyRat &lcDUntSin POSHDR.nDCurUnit
ENDCASE
m.UntCost = m.Est_Cost/m.Req_Qty
m.UntQty  = m.Req_Qty/m.Pieces
laData[VAL(m.Typ)+11] = laData[VAL(m.Typ)+11] - lnOldEstCost + m.Est_Cost
SELECT (lcTktSheet)
IF m.cCatgTyp ='S'
  REPLACE Req_Qty1 WITH m.Req_Qty1 ,;
          Req_Qty2 WITH m.Req_Qty2 ,;
          Req_Qty3 WITH m.Req_Qty3 ,;
          Req_Qty4 WITH m.Req_Qty4 ,;
          Req_Qty5 WITH m.Req_Qty5 ,;
          Req_Qty6 WITH m.Req_Qty6 ,;
          Req_Qty7 WITH m.Req_Qty7 ,;
          Req_Qty8 WITH m.Req_Qty8
ENDIF
REPLACE UntCost  WITH m.UntCost ,;
        UntQty   WITH m.UntQty  ,;
        Pieces   WITH m.Pieces  ,;
        Req_Qty  WITH m.Req_Qty ,;
        Est_Cost WITH m.Est_Cost
*B802978,1 KHM 01/25/2000 (Begin) Replacing the description
REPLACE Desc     WITH m.Desc
*B802978,1 KHM 01/25/2000 (End)

*B604623,1 AMH Saving marker field [Start]
REPLACE CMARKER WITH m.cMarker
*B604623,1 AMH [End]

IF laScrMode[2]
  SELECT (lcBaseFile)
  =RLOCK()
  GATHER FROM laData FIELDS &lcScFields
  
  *khm1*
  *IF lcTranType='I'
  IF lcTranType $ 'ID'
  *khm1*
    
    REPLACE ('NFCOST'+m.Typ) WITH EVAL('NFCOST'+m.Typ) - lnOFEstCst + lnFEstCst
  ENDIF
  UNLOCK
  SELECT cTktBom
  =SEEK(lcTranType+laData[1]+m.typ+m.Item+m.Iclr+m.MfgCode+m.Dyelot)
  =RLOCK()  
  REPLACE UntCost  WITH m.UntCost ,;
          UntQty   WITH m.UntQty  ,;
          Pieces   WITH m.Pieces  ,;
          Req_Qty  WITH m.Req_Qty ,;
          Est_Cost WITH m.Est_Cost
  *B802978,1 KHM 01/25/2000 (Begin) Replacing the description
  REPLACE Desc     WITH m.Desc
  *B802978,1 KHM 01/25/2000 (End)
  *B604623,1 AMH Saving marker field [Start]
  REPLACE CMARKER WITH m.cMarker
  *B604623,1 AMH [End]

  *B604974,1 KHM 09/30/2001 (Begin) Adding the replacement of Req_Qtyn
  REPLACE Req_Qty1 WITH m.Req_Qty1 ,;
          Req_Qty2 WITH m.Req_Qty2 ,;
          Req_Qty3 WITH m.Req_Qty3 ,;
          Req_Qty4 WITH m.Req_Qty4 ,;
          Req_Qty5 WITH m.Req_Qty5 ,;
          Req_Qty6 WITH m.Req_Qty6 ,;
          Req_Qty7 WITH m.Req_Qty7 ,;
          Req_Qty8 WITH m.Req_Qty8
  *B604974,1 KHM 09/30/2001 (End)
  
  UNLOCK IN cTktBom  
ENDIF
=lfUpdEstCst()
SELECT (lcDetFile)
GO TOP
DO WHILE !EOF()
  lcCurrKey = Style+SClr+STR(LineNo,6)+cBomTyp
  SKIP
  SUM REST ItemQty,ItemAmt TO lnTotQty,lnTotAmt WHILE ;
  Style+SClr+STR(LineNo,6)+cBomTyp+cShowType+Item+IClr+MfgCode = lcCurrKey+'1'
  REPLACE ItemQty WITH lnTotQty,;
          ItemAmt WITH lnTotAmt
  SKIP
ENDDO
GO TOP
SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfBrowConCn
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Brwose ticket budget quantity to contribute canceled quantity
*!*************************************************************
*! Calls     : gfModalGen,MFDSTRB.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfBrowConCn()
*!*************************************************************
FUNCTION lfBrowConCn
PRIVATE lcFields

lnDMarker = RECNO()
lcFields = "cMarker  =IIF(RECNO()=lnDMarker ,'>',' '):H=' ':R:1:W=.F.,"+;
           IIF(llUseDyelot,"cDyelot :H='Dyelot' :R,",'')+;
           IIF(laSetups[2,2]='Y',"cWareCode :H='Warehouse' :R,",'')
IF lcTranType = 'T'
  lcFields = lcFields + ;
             "nTotCan :H='TotQty' :W=lfwCanQty('') :V=lfvCanQty('')"
ELSE
  lcFields = lcFields + ;
       "nCan1 :H='Can1' :W=lfwCanQty('1') :V=lfvCanQty('1'),nCan2 :H='Can2' :W=lfwCanQty('2') :V=lfvCanQty('2'),"+;
       "nCan3 :H='Can3' :W=lfwCanQty('3') :V=lfvCanQty('3'),nCan4 :H='Can4' :W=lfwCanQty('4') :V=lfvCanQty('4'),"+;
       "nCan5 :H='Can5' :W=lfwCanQty('5') :V=lfvCanQty('5'),nCan6 :H='Can6' :W=lfwCanQty('6') :V=lfvCanQty('6'),"+;
       "nCan7 :H='Can7' :W=lfwCanQty('7') :V=lfvCanQty('7'),nCan8 :H='Can8' :W=lfwCanQty('8') :V=lfvCanQty('8'),"+;
       "nTotCan :H='TotQty' :R"
ENDIF  
BROWSE FIELDS &lcFields ;
       WINDOW MFCONCN1   ;
       IN WINDOW MFCONCN ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       WHEN lfShowCanBud() ;
       TITLE lcConCnTit

*!*************************************************************
*! Name      : lfwCanQty
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate conributed canceled quantity
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcSize : Style size
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfwCanQty('1')
*!*************************************************************
FUNCTION lfwCanQty
PARAMETERS lcSize
lcOldValue = IIF(EMPTY(lcSize),nTotCan,nCan&lcSize)

*!*************************************************************
*! Name      : lfvCanQty
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate conributed canceled quantity
*!*************************************************************
*! Calls     : gfModalGen,lfRefresh()
*!*************************************************************
*! Parameters: lcSize : Style size
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvCanQty('1')
*!*************************************************************
FUNCTION lfvCanQty
PARAMETERS lcSize
IF IIF(EMPTY(lcSize),nTotCan > m.nTotQty,nCan&lcSize > m.nQty&lcSize)
  REPLACE nCan&lcSize WITH lcOldValue
  *E300725,1 Message : 38073
  *E300725,1 Caneclled quantity cannot exceed budget quantity
  *E300725,1 Button : 00000
  *E300725,1 Ok
  =gfModalGen('TRM38073B00000','ALERT')
  RETURN
ENDIF
IF EMPTY(lcSize)
  lnTotBal = lnTotBal + lcOldValue - nTotCan
ELSE  
  REPLACE nTotCan WITH nTotCan - lcOldValue + nCan&lcSize
  lnBal&lcSize = lnBal&lcSize  + lcOldValue - nCan&lcSize
  lnTotBal = lnTotBal + lcOldValue - nCan&lcSize
ENDIF  
=lfRefresh('MFCONCN0')

*!*************************************************************
*! Name      : lfShowCanBud
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Show conributed canceled quantity
*!*************************************************************
*! Calls     : lfRefresh()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfShowCanBud()
*!*************************************************************
FUNCTION lfShowCanBud
lnDMarker = RECNO(lcisslogfile)
SHOW WINDOW (lcCOnCnTit) REFRESH SAME
SCATTER MEMVAR
=lfRefresh('MFCONCN2')

*!*************************************************************
*! Name      : lfvOkConCn
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Update conributed canceled quantity
*!*************************************************************
*! Calls     : gfModalGen()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvOkConCn()
*!*************************************************************
FUNCTION lfvOkConCn

IF lnBal1<>0 .OR. lnBal2<>0 .OR. lnBal3<>0 .OR. lnBal4<>0 .OR. lnBal5<>0 .OR. ;
   lnBal6<>0 .OR. lnBal7<>0 .OR. lnBal8<>0 .OR. lnTotBal<>0
  *E300725,1 Message : 38074
  *E300725,1 Contributed caneclled quantity does not total ticket cancelled
  *E300725,1 remainning quantity
  *E300725,1 Button : 00000
  *E300725,1 Ok
  =gfModalGen('TRM38074B00000','ALERT')
  GO TOP
  RETURN 
ENDIF
SCAN
  SELECT (lcTmpTkt)
  GO &lcisslogfile..nBudRecNo
  REPLACE nQty1   WITH nQty1   - &lcisslogfile..nCan1 ,;
          nQty2   WITH nQty2   - &lcisslogfile..nCan2 ,;
          nQty3   WITH nQty3   - &lcisslogfile..nCan3 ,;
          nQty4   WITH nQty4   - &lcisslogfile..nCan4 ,;
          nQty5   WITH nQty5   - &lcisslogfile..nCan5 ,;
          nQty6   WITH nQty6   - &lcisslogfile..nCan6 ,;
          nQty7   WITH nQty7   - &lcisslogfile..nCan7 ,;
          nQty8   WITH nQty8   - &lcisslogfile..nCan8 ,;
          nTotQty WITH nTotQty - &lcisslogfile..nTotCan
ENDSCAN
CLEAR READ

*!*************************************************************
*! Name      : lfBrowDstrb
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Browse cost item lines to contribute unit cost and unit quantity
*!*************************************************************
*! Calls     : gfCodDes,lfvUpdate,lfShowDistrb
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfBrowDstrb()
*!*************************************************************
FUNCTION lfBrowDstrb
lnDMarker = RECNO()
*B603348,1 ABD Update the picture of filed unitCost upon Increase width of The filed.[Begin] 
*BROWSE FIELDS ;
       cMarker  =IIF(RECNO()=lnDMarker ,'>',' '):H=' ':R:1:W=.F.,;
       Style :H= lcItmHdr :25 :R,;
       cItem =IIF(cCatgTyp='M',gfCodDes(MfgCode,'MfgCode'),Item+ALLTRIM(IClr)) :H='Item' :R,;
       StyQty   :H='Pieces' :V=lfvUpdate(),;
       UnitQty  :H='Units' :W= cCatgTyp $ 'FTS' :V=lfvUpdate(),;
       UnitCost :H='Cost'  :p='999.999' :V=lfvUpdate(),;
       ItemQty  :H='Item Quantity' :R,;
       ItemAmt  :H='Item Amount' :R ;
       WINDOW MFDSTRB0   ;
       IN WINDOW MFDSTRB ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       WHEN lfShowDistrb()  ;
       TITLE lcDistrbTit
*E301427,1 SSH Add conidtion if we are calling this function from
*E301427,1 SSH detil button or this Mfg code has detila oeration
*E301427,1 SSH then we will make all cost fields in this browse read only.
IF !llFrmDetal .AND. llHideDet
  BROWSE FIELDS ;
         cMarker  =IIF(RECNO()=lnDMarker ,'>',' '):H=' ':R:1:W=.F.,;
         Style :H= lcItmHdr :25 :R,;
         cItem =IIF(cCatgTyp='M',gfCodDes(MfgCode,'MfgCode'),Item+ALLTRIM(IClr)) :H='Item' :R,;
         StyQty   :H='Pieces' :V=lfvUpdate(),;
         UnitQty  :H='Units' :W= cCatgTyp $ 'FTS' :V=lfvUpdate(),;
         UnitCost :H='Cost'  :p='9999999.99999' :V=lfvUpdate(),;
         ItemQty  :H='Item Quantity' :R,;
         ItemAmt  :H='Item Amount' :R ;
         WINDOW MFDSTRB0   ;
         IN WINDOW MFDSTRB ;
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
         NOCLEAR           ;
         WHEN lfShowDistrb()  ;
         TITLE lcDistrbTit
ELSE  &&llFrmDetal
  BROWSE FIELDS ;
         cMarker  =IIF(RECNO()=lnDMarker ,'>',' '):H=' ':R:1:W=.F.,;
         Style :H= lcItmHdr :25 :R,;
         cItem =IIF(cCatgTyp='M',gfCodDes(MfgCode,'MfgCode'),Item+ALLTRIM(IClr)) :H='Item' :R,;
         StyQty   :H='Pieces' :V=lfvUpdate(),;
         UnitQty  :H='Units' :W= cCatgTyp $ 'FTS' :V=lfvUpdate(),;
         UnitCost :H='Cost'  :p='9999999.99999' :R,;
         ItemQty  :H='Item Quantity' :R,;
         ItemAmt  :H='Item Amount' :R ;
         WINDOW MFDSTRB0   ;
         IN WINDOW MFDSTRB ;
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
         NOCLEAR           ;
         WHEN lfShowDistrb()  ;
         TITLE lcDistrbTit
ENDIF
*E301427,1 SSH [END]
*B603348,1 ABD [End]
*!*************************************************************
*! Name      : lfvUpdate
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate contributed unit cost and unit quantity
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvUpdate()
*!*************************************************************
FUNCTION lfvUpdate
REPLACE ItemQty  WITH StyQty*UnitQty,;
        ItemAmt  WITH StyQty*UnitQty*UnitCost

*!*************************************************************
*! Name      : lfShowDistrb
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Refresh contributed cost item lines
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfShowDistrb()
*!*************************************************************
FUNCTION lfShowDistrb
lnDMarker = RECNO(lcisslogfile)
SHOW WINDOW (lcDistrbTit) REFRESH SAME

*!*************************************************************
*! Name      : lfOkDistrb
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Accept contribution of cost item unit cost and quantity
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfOkDistrb()
*!*************************************************************
FUNCTION lfOkDistrb

PRIVATE lnPieces,lnReq,lnAmount,lnRecNo

lnRecNo = RECNO()
SUM StyQty,StyQty*UNITQTY,ItemAmt TO lnPieces,lnReq,lnAmount
GO lnRecNo
*E300725,1 Message : 38069
*E300725,1 Contributed pieces don't equal the total pieces.
*E300725,1 Button : 38010
*E300725,1 Proceed  Resume
IF lnPieces <> m.Pieces .AND. gfModalGen('QRM38069B38010','ALERT') = 2
  RETURN 
ENDIF
*E300725,1 Message : 38070
*E300725,1 The calculated average unit cost 999 don't equal the entered 
*E300725,1 average unit cost 999.
*E300725,1 Button : 38010
*E300725,1 Proceed  Resume
IF !llFrmDetal .AND. llHideDet
  IF ROUND(lnAmount/lnReq,3) <> m.UntCost .AND. ;
    gfModalGen('QRM38070B38010','ALERT',STR(ROUND(lnAmount/lnReq,3),7,3)+'|'+STR(m.UntCost,7,3)) = 2
    RETURN 
  ENDIF   
ENDIF
*E300725,1 Message : 38071
*E300725,1 The calculated average unit quantity 999 don't equal the entered 
*E300725,1 average unit quantity 999.
*E300725,1 Button : 38010
*E300725,1 Proceed  Resume
*B602287,1 Display the approperiate message
*IF ROUND(lnReq/lnPieces,3) <> m.UntQty .AND. ;
  gfModalGen('QRM38070B38010','ALERT',STR(ROUND(lnReq/lnPieces,3),7,3)+'|'+STR(m.UntQty,7,3)) = 2
IF ROUND(lnReq/lnPieces,3) <> m.UntQty .AND. ;
  gfModalGen('QRM38071B38010','ALERT',STR(ROUND(lnReq/lnPieces,3),7,3)+'|'+STR(m.UntQty,7,3)) = 2
*B602287,1 (End)
  RETURN 
ENDIF
SCAN
  SCATTER MEMVAR
  IF SEEK(Style+SClr+STR(LineNo,6)+cBomTyp+cShowType+Item+IClr+MfgCode,lcDetFile)
    SELECT (lcDetFile)
    GATHER MEMVAR
  ENDIF
  IF laScrMode[2] .AND. ;
    SEEK(lcTranType+ctype+laData[1]+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode,'BomLine')
    SELECT BomLine
    GATHER MEMVAR
  ENDIF
ENDSCAN
CLEAR READ
RETURN

*!*************************************************************
*! Name      : lfvVoid
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Void Cost Item
*!*************************************************************
*! Calls     : gfModalGen,lfwBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvVoid()
*!*************************************************************
FUNCTION lfvVoid
PRIVATE lcType,lnRecNo,lnUnVoidRec
*--- DOC.
*--- Valid function for void cost element
*--- this function replace the field LVOID with .T. in the CTKTBOM file
*--- then update the bomline and the cuttkth, file 
*--- DOC.
IF cShowType <> '1'
  RETURN
ENDIF
IF cCatgTyp = 'P'
  *E300725,1 Message : 38035
  *E300725,1 Cannot void the purchase cost.
  *E300725,1 Button : 00000
  *E300725,1 Ok
  =gfModalGen('TRM38035B00000','ALERT','void')
  RETURN
ENDIF
lnRecNo = RECNO()
COUNT TO lnUnVoidRec FOR cShowType='1'
GO lnRecNo
IF lnUnVoidRec = 1
  *E300725,1 Message : 38037
  *E300725,1 You cannot void all cost items.
  *E300725,1 Button : 00000
  *E300725,1 Ok
  =gfModalGen('TRM38037B00000','ALERT')
  RETURN
ENDIF
*E300725,1 Message : 38036
*E300725,1 Are you sure you want to void this record?
*E300725,1 Button : 38002
*E300725,1 Proceed  Cancel
IF gfModalGen('QRM38036B38002','ALERT','void') = 2
  RETURN
ENDIF
lcType = Typ
IF SEEK(cIMTyp+CutTkt+Typ+Item+IClr+MfgCode+Dyelot,'CTKTBOM')
  SELECT CTKTBOM
  =RLOCK()
  REPLACE LVOID WITH .T.
  UNLOCK
ENDIF
IF SEEK(lcTranType+'1'+laData[1],'BomLine')
  SELECT BomLine
  =RLOCK()
  REPLACE REST LVOID WITH .T. ;
  WHILE cIMTyp+CTYPE+CTKTNO = lcTranType+'1'+laData[1] ;
  FOR cBomTyp+Item+IClr+MfgCode = ;
      &lcTktSheet..Typ+&lcTktSheet..Item+&lcTktSheet..IClr+&lcTktSheet..MfgCode
  UNLOCK
ENDIF
IF SEEK(lcTranType+'2'+laData[1],'BomLine')
  SELECT BomLine
  =RLOCK()
  REPLACE REST LVOID WITH .T. ;
  WHILE cIMTyp+CTYPE+CTKTNO = lcTranType+'2'+laData[1] ;
  FOR cBomTyp+Item+IClr+MfgCode = ;
      &lcTktSheet..Typ+&lcTktSheet..Item+&lcTktSheet..IClr+&lcTktSheet..MfgCode
  UNLOCK
ENDIF
=lfUpdEstCst()
laData[VAL(lcType)+11]=laData[VAL(lcType)+11]-&lcTktSheet..Est_Cost
*E300725,1 Update the corresponding estimated cost in the header file
DO CASE
  CASE lcTranType = 'M'
    SELECT CUTTKTH
    IF SEEK(laData[1])
      =RLOCK()
      REPLACE nEst_Cost&lcType WITH nEst_Cost&lcType - &lcTktSheet..Est_Cost
      UNLOCK
    ENDIF
  
  *khm1*
  *CASE lcTranType = 'I'
  CASE lcTranType $ 'ID'  
  *khm1*
    
    SELECT POSHDR
    *C200080,1 AMM Consider the dye order case
    *IF SEEK('P'+laData[1])
    IF SEEK(IIF(EMPTY(lcDyePO),'P','D')+laData[1])
    *C200080,1 AMM end
      =RLOCK()
      REPLACE nICost&lcType WITH nICost&lcType - &lcTktSheet..Est_Cost
      DO CASE
        CASE &lcTktSheet..cCatgTyp='P'
          REPLACE nFCost&lcType WITH nFCost&lcType - &lcTktSheet..Est_Cost/(1 &lcPExSign POSHDR.nPriceRat &lcPUntSin POSHDR.nCurrUnit)
        CASE INLIST(&lcTktSheet..cCatgTyp,'S','T','F')
          REPLACE nFCost&lcType WITH nFCost&lcType - &lcTktSheet..Est_Cost
        OTHERWISE
          REPLACE nFCost&lcType WITH nFCost&lcType - &lcTktSheet..Est_Cost/(1 &lcDExSign POSHDR.nDutyRat &lcDUntSin POSHDR.nDCurUnit)
      ENDCASE
      UNLOCK
    ENDIF  
  CASE lcTranType = 'T'
    SELECT MMFGORDH
    IF SEEK(laData[1])
      =RLOCK()
      REPLACE nEst_Cost&lcType WITH nEst_Cost&lcType - &lcTktSheet..Est_Cost
      UNLOCK
    ENDIF
ENDCASE
SELECT (lcDetFile)
DELETE ALL FOR cBomTyp+cShowType+Item+IClr+MfgCode = ;
              &lcTktSheet..TYP+'1'+&lcTktSheet..Item+&lcTktSheet..IClr+&lcTktSheet..MfgCode
GO TOP
DO WHILE !EOF()
  lcCurrKey  = Style+SClr+STR(LineNo,6)
  IF !SEEK(lcCurrKey+&lcTktSheet..TYP+'1')
    =SEEK(lcCurrKey+&lcTktSheet..TYP)
    DELETE REST WHILE Style+SClr+STR(LineNo,6)+cBomTyp+cShowType+Item+IClr+MfgCode  = lcCurrKey+&lcTktSheet..TYP
  ELSE
    SUM REST ItemQty,ItemAmt TO lnTotQty,lnTotAmt ;
    WHILE Style+SClr+STR(LineNo,6)+cBomTyp+cShowType+Item+IClr+MfgCode  = lcCurrKey+&lcTktSheet..TYP+'1'
    REPLACE ItemQty WITH lnTotQty ,;
            ItemAmt WITH lnTotAmt 
  ENDIF
  IF SEEK(lcCurrKey)
    REPLACE lHideTit WITH .F.
    SCAN REST WHILE Style+SClr+STR(LineNo,6)+cBomTyp+cShowType+Item+IClr+MfgCode  = lcCurrKey
    ENDSCAN
  ELSE
    GO TOP
  ENDIF
ENDDO
GO TOP
SELECT (lcTktSheet)
DELETE
IF !SEEK(lcType+'1')
  =SEEK(lcType+'0')
  DELETE
ENDIF
GO TOP
=lfwBrow()
RETURN

*!*************************************************************
*! Name      : lfvUnVoid
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Unvoid Cost Item
*!*************************************************************
*! Calls     : ARIABROW,lfwBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvUnVoid()
*!*************************************************************
FUNCTION lfvUnVoid
PRIVATE lcBrowFild,lnAlias,lcFile_Ttl
*--- DOC.
*--- Unvoid cost item by replace the fiel lvoid with .T. and update
*--- CutTktH and BOMLINE
*--- this function browse for all the cost element that has been voided
*--- and browse them to select one to un void
*--- DOC.
lnAlias = SELECT()
SELECT *,RECNO() AS nRecNo FROM CTKTBOM ;
WHERE cIMTyp+CutTkt+Typ+Item+IClr+MfgCode+Dyelot=lcTranType+laData[1] .AND. lVoid ;
INTO CURSOR (lcisslogfile)
IF _TALLY > 0 
  lcBrFields = ;
       "cItem =IIF(cCatgTyp='M',gfCodDes(MfgCode,'MfgCode'),Item+ALLTRIM(IClr)) :20 :H='Item' :R,"+;
       IIF(llUseDyelot,"Dyelot :H='Dyelot':R,","")+;
       "Uom       :H='UOM'     :R ,"+;
       "UntQty    :H='Units'   :R,"+;
       "UntCost   :H='Cost'    :R,"+;
       "Req_Qty   :H='Required':R,"+;
       "Issue_Qty :H='Issued'  :R,"+;
       "Used_Qty  :H='Used'    :R"
  IF ARIABROW('','Voided Cost Items',gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','','ITEM','laBrowArr')
    GO &lcisslogfile..nRecNo IN CTKTBOM
    IF !SEEK(Typ+'0',lcTktSheet)
      INSERT INTO (lcTktSheet) (TYP,cShowType,Item) VALUES ;
      (cTktBom.Typ,'0',laSetups[2+VAL(CTKTBOM.Typ),2])
    ENDIF
    SELECT CTKTBOM
    REPLACE lVoid WITH .F.
    SCATTER MEMVAR
    m.cShowType = '1'
    INSERT INTO (lcTktSheet) FROM MEMVAR

    SELECT BOMLINE
    =SEEK(lcTranType+'1'+laData[1])
    SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+;
                     iclr+mfgcode  = lcTranType+'1'+laData[1] ;
              FOR   Item+IClr+MfgCode=CTKTBOM.Item+CTKTBOM.IClr+CTKTBOM.MfgCode .AND. lVoid
     
      IF !SEEK(Style+SClr+STR(LineNo,6)+cBomTyp+'0',lcDetFile)
        llHideTit = SEEK(Style+SClr+STR(LineNo,6),lcDetFile)
        INSERT INTO (lcDetFile) ;
        (Style,SClr,LineNo,cBomTyp,cShowType,Item,lHideTit) VALUES ;
        (BomLine.Style,BomLine.SClr,BomLine.LineNo,BomLine.cBomTyp,'0',;
         laSetups[2+VAL(BOMLINE.cBomTyp),2],llHideTit)
      ENDIF
      REPLACE lVoid WITH .F.
      SCATTER MEMVAR
      m.cShowType = '1'
      INSERT INTO (lcDetFile) FROM MEMVAR
      IF !SEEK(Style+SClr+STR(LineNo,6)+cBomTyp+'2',lcDetFile)
        INSERT INTO (lcDetFile) ;
        (Style,SClr,LineNo,cBomTyp,cShowType,Item) VALUES ;
        (BomLine.Style,BomLine.SClr,BomLine.LineNo,BomLine.cBomTyp,'2','***** Total :')
      ENDIF
      SELECT (lcDetFile)
      REPLACE ItemQty  WITH ItemQty + BomLine.ItemQty,;
              ItemAmt  WITH ItemAmt + BomLine.ItemAmt
    ENDSCAN
  ENDIF
  =lfUpdEstCst()
  *E300725,1 Update the corresponding estimated cost in the header file
  lcType = CTKTBOM.Typ
  laData[VAL(lcType)+11]=laData[VAL(lcType)+11]+ CTKTBOM.Est_Cost
  DO CASE
    CASE lcTranType = 'M'
      SELECT CUTTKTH
      IF SEEK(laData[1])
        =RLOCK()
        REPLACE nEst_Cost&lcType WITH nEst_Cost&lcType + CTKTBOM.Est_Cost
        UNLOCK
      ENDIF  

    *khm1*
    *CASE lcTranType = 'I'
    CASE lcTranType $ 'ID'
    *khm1*
    
      SELECT POSHDR
      *C200080,1 AMM Consider the dye order case
      *IF SEEK('P'+laData[1])
      IF SEEK(IIF(EMPTY(lcDyePO),'P','D')+laData[1])
      *C200080,1 AMM end
        =RLOCK()
        REPLACE nICost&lcType WITH nICost&lcType + CTKTBOM.Est_Cost
        DO CASE
          CASE CTKTBOM.cCatgTyp='P'
            REPLACE nFCost&lcType WITH nFCost&lcType - CTKTBOM.Est_Cost/(1 &lcPExSign POSHDR.nPriceRat &lcPUntSin POSHDR.nCurrUnit)
          CASE INLIST(CTKTBOM.cCatgTyp,'S','T','F')
            REPLACE nFCost&lcType WITH nFCost&lcType - CTKTBOM.Est_Cost
          OTHERWISE
            REPLACE nFCost&lcType WITH nFCost&lcType - CTKTBOM.Est_Cost/(1 &lcDExSign POSHDR.nDutyRat &lcDUntSin POSHDR.nDCurUnit)
        ENDCASE
        UNLOCK
      ENDIF
    CASE lcTranType = 'T'
      SELECT MMFGORDH
      IF SEEK(laData[1])
        =RLOCK()
        REPLACE nEst_Cost&lcType WITH nEst_Cost&lcType +CTKTBOM.Est_Cost
        UNLOCK
      ENDIF  
  ENDCASE
  SELECT (lcDetFile)
  GO TOP
  DO WHILE !EOF()
    lcCurrKey = Style+SClr+STR(LineNo,6)
    REPLACE lHideTit WITH .F.
    SKIP
    REPLACE REST lHideTit WITH .T. WHILE Style+SClr+STR(LineNo,6)+cBomTyp+cShowType+Item+IClr+MfgCode=lcCurrKey
  ENDDO
  GO TOP
  SELECT (lcTktSheet)
  =lfwBrow()
ENDIF  
USE IN (lcisslogfile)
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvIssLog
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Issue/Return Cost Item
*!*************************************************************
*! Calls     : MFISLOG.SPX,lfwBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvIssLog()
*!*************************************************************
FUNCTION lfvIssLog
PRIVATE lnAlias,lnOnHand,lnRequired,lnUsed,lcItem,lcColor,lcCondition,;
        lcRetCond,lnLotNo,laLots
*--- DOC.
*--- Check if the current record in the browse is cost item not title
*--- DOC.
SELECT (lcTktSheet)
IF cShowType <> '1'
  RETURN
ENDIF
DECLARE laLots[1]
lnLotNo = 1
m.cOprCode=cOprCode
IF !EMPTY(m.cOprCode)
  *--- DOC.
  *--- Get the curret operatoin record from mfgoprdt.dbf
  *--- to be sure that this operation has cost item , and lots
  *--- DOC.
  SELECT DIST cLotNo FROM (lcOPrDet) ;
  WHERE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd= lcTranType+laData[1]+m.cOprCode ;
  INTO ARRAY laLots
  IF _TALLY = 0
    *E300725,1 Message : 38089
    *E300725,1 Cost item xxxxx has been assigned to operation xxxxx.
    *E300725,1 No lots has been generated for this operation. Cannot Issue.
    *E300725,1 Button : 00000
    *E300725,1 Ok
    =gfModalGen('TRM38089B00000','ALERT',ALLTRIM(Item)+ALLTRIM(IClr)+'|'+ALLTRIM(gfCodDes(m.cOprCode,'MfgCode')))
    RETURN
  ENDIF
ENDIF
lnAlias = SELECT()
STORE 0 TO lnLMarker
STORE 0 TO lnOnHand
lcCondition = ''
lcRetCond   = ''
SELECT (lcTktSheet)
SCATTER MEMVAR
*B602286,1 Check if fabric exist in the fabric file before issue
DO CASE
  CASE m.cCatgTyp='F' OR (m.cCatgTyp='T' AND m.Trim_Invt)
    IF !SEEK(SUBSTR(m.Item,1,7)+m.IClr,'Fabric')
      *B602286,1 Message : 38156
      *B602286,1 Material XXxX does not exist in the material file
      *B602286,1 Button : 00000
      *B602286,1 Ok
      =gfModalGen('TRM38156B00000','ALERT','Material\Color: '+ALLTRIM(Item)+'\'+ALLTRIM(IClr)+'|Material')
      RETURN
    ENDIF
  CASE m.cCatgTyp='S'
    IF !SEEK(m.Item,'Style')
      *B602286,1 Message : 38156
      *B602286,1 Style XXxX does not exist in the material file
      *B602286,1 Button : 00000
      *B602286,1 Ok
      =gfModalGen('TRM38156B00000','ALERT',ALLTRIM(lcItmHdr)+': '+ALLTRIM(Item)+'|'+ALLTRIM(lcMjrHdr))
      RETURN
    ENDIF
ENDCASE  
*B602286,1 (End)
*E301077,55 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'BOMCOST',gcDataDir+'BOMCOST','SH')
DO CASE
  CASE INLIST(m.cCatgTyp,'F','T')
    =gfOpenFile(gcDataDir+'FABDYE',gcDataDir+'FABDYE','SH')
    =gfOpenFile(gcDataDir+'MATINVJL',gcDataDir+'MATINVJL','SH')
    IF laSetups[9,2]='L'

      *B604073,1 KHM 12/25/2000 (Begin) Changing the Index.
      *USE (gcDataDir+'MATINVJL') AGAIN ALIAS FABINV ORDER TAG Mtinvseq IN 0
      USE (gcDataDir+'MATINVJL') AGAIN ALIAS FABINV ORDER TAG MatInvJl IN 0
      *B604073,1 KHM 12/25/2000 (End)
      
      =gfOpenFile(gcDataDir+'POFHDR',gcDataDir+'POFHDR','SH')
    ENDIF
  CASE m.cCatgTyp='S'
    =gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
    =gfOpenFile(gcDataDir+'STYINVJL',gcDataDir+'STYINVJL','SH')
ENDCASE
SELECT (lcTktSheet)
*E301077,55 (End)

llItemDye =  ;
((m.cCatgTyp='F' OR m.Trim_Invt) AND laSetups[12,2]='Y' AND ;
 SEEK(SUBSTR(m.Item,1,7)+m.IClr,'Fabric') AND Fabric.cDye_Flg='Y') OR ;
(m.cCatgTyp='S' AND laSetups[8,2]='Y' AND SEEK(m.Item,'Style') AND ;
 Style.cDye_Flg='Y')

STORE .F. TO llDispRoll,llDispAct

*E301897,1 AMH Define variable to display edit used botton [Start]
STORE .F. TO llDispUsed
*E301897,1 AMH [End]

DO (gcScrDir+"MFISLOG.SPX")

*E301077,55 Inhance openning files to speed up transaction
IF INLIST(m.cCatgTyp,'F','T') AND laSetups[9,2]='L'
  USE IN FABINV
  =gfCloseFile('POFHDR')
ENDIF
*E301077,55 (End)

SELECT (lnAlias)
=lfwBrow()

*!*************************************************************
*! Name      : lfBrowLog
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Browse Issues/Returns
*!*************************************************************
*! Calls     : lfShowLog,lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfBrowLog()
*!*************************************************************
FUNCTION lfBrowLog
PRIVATE lcFields
SET ORDER TO TAG Bomcstkt IN BOMCOST
DO CASE
*--- DOC.
*--- m.cCatgTyp='F' ==> Fabic
*--- m.cCatgTyp='T' ==> Trim
*--- m.cCatgTyp='S' ==> Style
*--- Browse function to browse the cost item that has been issued before.
*--- DOC.
  CASE m.cCatgTyp='F' .OR. (m.cCatgTyp='T' .AND. m.Trim_Invt)
    =SEEK(SUBSTR(m.Item,1,7)+m.IClr,'Fabric')
    llDispRoll = laSetups[11,2]='Y' .AND. Fabric.lTrkRolls
    llDispAct  = !INLIST(laSetups[9,2],'L','F','I')
    
    *E301897,1 AMH Display the edit used botton [Start]
    llDispUsed = !INLIST(laSetups[9,2],'L','F','I')
    *E301897,1 AMH [End]
    
    *B802396,1 SSH 11/04/00 [Begin] Get the onhand from FabDye.
    *lnOnHand   = FABRIC.OnHand
    lnOnHand   = IIF(EMPTY(cWareCode),FABRIC.OnHand,FABDYE.OnHand)
    *B802396,1 SSH 11/04/00  [End]

    *B606536,1 KHM 10/14/2002 (Begin) Adding IF command to check if the costing method is not
    *B606536,1                average or standard then this part will work as it is, otherwise, 
    *B606536,1                we will display the information from bomcost according the
    *B606536,1                category of the cost sheet details browse. Because we might have
    *B606536,1                more than one cost element set as "F" and a one fabric in more
    *B606536,1                than one category.
    IF !INLIST(laSetups[9,2],"A","S")
    *B606536,1 KHM 10/14/2002 (End)
      lcCondition= 'nIssued > 0'
      lcRetCond  = 'nIssued > 0'
      SELECT MATINVJL
      SET ORDER TO TAG MATINVJL
      
      *B123774,1 MHM 10/12/2004 Enhance Speed of openning (ISSUE/RETURN) screen [Start]
      SELECT *  FROM MATINVJL ;
                WHERE cfabric+ccolor+cwarecode+cdyelot+crsession+cisession = SUBSTR(m.Item,1,7) + m.IClr ;
                INTO CURSOR TmpMATINV ORDER BY cfabric,ccolor,cwarecode,cdyelot,crsession,cisession
      *B123774,1 MHM 10/12/2004 [ENd]

      lnLMarker  = RECNO()
      *HDM E301037,1[START] lcFields = "cMarker =IIF(RECNO()=lnLMarker ,'>',' '):H=' ':R:1:W=.F.,"+;
      *               "cTran=IIF(nIssued >0,'Issue','Return') :H='Type' :10 :R,"
      *lcFields = lcFields + ;
                  IIF(laSetups[2,2]='Y',"cWareCode :H='Warehouse' :R,",'')+;
                  "dTranDate :H='Date' :R,nSession=IIF(nIssued > 0 , cISession , cRSession) :H='Session' :R,"+;
                  "nQty=ABS(IIF(nIssued > 0, nIssued ,nReceived)) :H='TotQty' :R,nUnitCost :H='Unit Cost' :R,"+;
                  IIF('AP' $ gcCmpModules,"cApInvNo :H= 'Invoice':R,",'')+;
                  "cDesc=gfCodDes(cOprCode,'MFGCODE') :H='Operation' :R"+;
                  IIF(laData[28]='M',",cLotNo :H='Lot#' :R",'')

      lcFields = "cMarker =IIF(RECNO()=lnLMarker ,'>',' '):H=' ':R:1:W=.F.,"+;
                 "cTran=IIF(nIssued >0,'Issue',IIF(nReceived > 0,'Return','')) :H='Type' :10 :R,"

      lcFields = lcFields + ;
                 IIF(laSetups[2,2]='Y',"cWareCode :H='Warehouse' :R,",'')+;
                 "dTranDate :H='Date' :R,cTrn_Seq :H='Session' :R,"+;
                 "nQty=ABS(IIF(nIssued > 0, nIssued ,nReceived)) :H='TotQty' :R,nUnitCost :H='Unit Cost' :R,"+;
                 IIF('AP' $ gcCmpModules,"cApInvNo :H= 'Invoice':R,",'')+;
                 "cDesc=gfCodDes(cOprCode,'MFGCODE') :H='Operation' :R"+;
                 IIF(laData[28]='M',",cLotNo :H='Lot#' :R",'')

      *B802636,1 Added cDyelot in FOR expresion to the following browse command.

      *B123774,1 MHM 10/12/2004 remove KEY to enhance Speed[Start]
      *BROWSE FIELDS &lcFields  ;
             WINDOW MFISLOG1   ;
             IN WINDOW MFISLOG ;
             NOMENU            ;         
             NOAPPEND          ;
             NODELETE          ;         
             NOWAIT            ;
             SAVE              ;
             NOCLEAR           ;
             KEY SUBSTR(m.Item,1,7)+m.IClr ;
             FOR cTranType='4' .AND. cTktNo=laData[1] AND cDyelot=IIF(!EMPTY(m.Dyelot),m.Dyelot,'') ;
             WHEN lfShowLog()  ;
             TITLE lcIssLog
      BROWSE FIELDS &lcFields  ;
             WINDOW MFISLOG1   ;
             IN WINDOW MFISLOG ;
             NOMENU            ;         
             NOAPPEND          ;
             NODELETE          ;         
             NOWAIT            ;
             SAVE              ;
             NOCLEAR           ;
             FOR cTranType='4' .AND. cTktNo=laData[1] AND cDyelot=IIF(!EMPTY(m.Dyelot),m.Dyelot,'') ;
             WHEN lfShowLog()  ;
             TITLE lcIssLog

      *B123774,1 MHM 10/12/2004 [End]
    
    *B606536,1 KHM 10/14/2002 (Begin) Case of average and standard costing method.
    ELSE
      lcCondition= 'nTotQty > 0'
      lcRetCond  = 'nTotQty > 0'    
      SELECT BOMCOST
      lnLMarker  = RECNO()
      lcFields = "cMarker =IIF(RECNO()=lnLMarker ,'>',' '):H=' ':R:1:W=.F.,"+;
                 "cTran=IIF(nTotQty >0,'Issue',IIF(nTotQty < 0,'Return','')) :H='Type' :10 :R,"

      lcFields = lcFields + ;
                 IIF(laSetups[2,2]='Y',"cWareCode :H='Warehouse' :R,",'')+;
                 "dTranDate :H='Date' :R,cTrn_Seq=IIF(nTotQty>0,cISession,cRSession) :H='Session' :R,"+;
                 "nQty=ABS(nTotQty) :H='TotQty' :R,nUnitCst :H='Unit Cost' :R,"+;
                 IIF('AP' $ gcCmpModules,"cApInvNo :H= 'Invoice':R,",'')+;
                 "cDesc=gfCodDes(cOprCode,'MFGCODE') :H='Operation' :R"+;
                 IIF(laData[28]='M',",cLotNo :H='Lot#' :R",'')
      BROWSE FIELDS &lcFields  ;
             WINDOW MFISLOG1   ;
             IN WINDOW MFISLOG ;
             NOMENU            ;         
             NOAPPEND          ;
             NODELETE          ;         
             NOWAIT            ;
             SAVE              ;
             NOCLEAR           ;
             KEY m.Typ+lcTranType+laData[1]+PADR(m.Item,19)+m.IClr ;
             FOR cDyelot=IIF(!EMPTY(m.Dyelot),m.Dyelot,'');
             WHEN lfShowLog()  ;
             TITLE lcIssLog         

    ENDIF
    *B606536,1 KHM 10/14/2002 (End)
    
  CASE m.cCatgTyp = 'S' .AND. SEEK(m.Item,'Style')
    *B802396,1 SSH 11/04/00 [Begin] Get the onhand from StyDye.
    *lnOnHand = Style.TotStk
    lnOnHand   = IIF(EMPTY(cWareCode),Style.TotStk,StyDye.TotStk)
    *B802396,1 SSH 11/04/00  [End]
    lcCondition= "cIRType='I'"
    lcRetCond  = "cIRType='I'"
    llDispAct  = !INLIST(laSetups[10,2],'L','F')
    SELECT STYINVJL
    SET ORDER TO TAG STYINVJL

    *B123774,1 MHM 10/12/2004 Enhance Speed of openning (ISSUE/RETURN) screen [Start]
    SELECT *  FROM STYINVJL ;
              WHERE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6)= m.Item ;
              INTO CURSOR TmpSTYINV ORDER BY style,cwarecode,csession
    *B123774,1 MHM 10/12/2004 [End]

    lnLMarker = RECNO()
    lcFields = "cMarker =IIF(RECNO()=lnLMarker ,'>',' '):H=' ':R:1:W=.F.,"+;
               "cTran=IIF(cIRType='I','Issue','Return') :H='Type' :10 :R,"
    lcFields = lcFields + ;
               IIF(laSetups[2,2]='Y',"cWareCode :H='Warehouse' :R,",'')+;
               "dTrDate :H='Date' :R,cSession :H='Session' :R,"+;
               "Stk=ABS(nTotStk) :H='TotQty' :R,nCost :H='Unit Cost' :R,"+;
               "cDesc=gfCodDes(cOprCode,'MFGCODE') :H='Operation' :R,"+;
               IIF(laData[28]='M',"cLotNo :H='Lot#' :R,",'')+;
               "Stk1=ABS(nStk1) :H='Qty1' :R,Stk2=ABS(nStk2) :H='Qty2' :R,Stk3=ABS(nStk3) :H='Qty3' :R,Stk4=ABS(nStk4) :H='Qty4' :R,"+;
               "Stk5=ABS(nStk5) :H='Qty5' :R,Stk6=ABS(nStk6) :H='Qty6' :R,Stk7=ABS(nStk7) :H='Qty7' :R,Stk8=ABS(nStk8) :H='Qty8' :R"
    *E301248 (Start)
    *BROWSE FIELDS &lcFields  ;
           WINDOW MFISLOG1   ;
           IN WINDOW MFISLOG ;
           NOMENU            ;         
           NOAPPEND          ;
           NODELETE          ;         
           NOWAIT            ;
           SAVE              ;
           NOCLEAR           ;
           KEY m.Item        ;
           FOR cTrCode+cTrType = laData[1]+'1' ;
           WHEN lfShowLog()  ;
           TITLE lcIssLog
    
    *B123774,1 MHM 10/12/2004 Enhance Speed of openning (ISSUE/RETURN) screen [Start]
    *BROWSE FIELDS &lcFields  ;
           WINDOW MFISLOG1   ;
           IN WINDOW MFISLOG ;
           NOMENU            ;         
           NOAPPEND          ;
           NODELETE          ;         
           NOWAIT            ;
           SAVE              ;
           NOCLEAR           ;
           KEY m.Item        ;
           FOR cTrCode+cTrType $ laData[1]+'1'+','+laData[1]+'I' ;
           WHEN lfShowLog()  ;
           TITLE lcIssLog
    BROWSE FIELDS &lcFields  ;
           WINDOW MFISLOG1   ;
           IN WINDOW MFISLOG ;
           NOMENU            ;         
           NOAPPEND          ;
           NODELETE          ;         
           NOWAIT            ;
           SAVE              ;
           NOCLEAR           ;
           FOR cTrCode+cTrType $ laData[1]+'1'+','+laData[1]+'I' ;
           WHEN lfShowLog()  ;
           TITLE lcIssLog

   *B123774,1 MHM 10/12/2004 [End]

  *E301248 (End)
  OTHERWISE
    llDispAct  = .T.
    SELECT BOMCOST
    lcCondition= 'nTotQty>0'
    lcRetCond  = 'm.Used_Qty>0'
    =SEEK(m.Typ+lcTranType+laData[1]+m.Item+m.IClr+m.MfgCode)
    *B603348,1 ABD Update the picture of filed nTotCst & nTotAcSt upon Increase width of The filed.[Begin] 
    *lcFields = "cMarker =IIF(RECNO()=lnLMarker ,'>',' '):H=' ':R:1:W=.F.,"+;
    *           "cTran=IIF(nTotQty>0,'Issue','Return') :H='Type' :10 :R,"
    *lcFields = lcFields + ;
    *           "dTranDate :H='Date' :R,nSession=IIF(nTotQty>0,cISession,cRSession) :H='Session' :R,"+;
    *           "QTy=ABS(nTotQty) :H='TotQty' :R,nUnitCst :H='Unit Cost' :R,Amnt=ABS(nTotCst) :H='Amount' :R,"+;
    *           "nUnitAcSt :H='Actual Unit Cost' :R,AAmnt=ABS(nTotAcSt) :H='Actual Amount' :R,"+;
    *           "cDesc=gfCodDes(cOprCode,'MFGCODE') :H='Operation' :R"+;
    *           IIF(laData[28]='M',",cLotNo :H='Lot#' :R",'')
    
    lcFields = "cMarker =IIF(RECNO()=lnLMarker ,'>',' '):H=' ':R:1:W=.F.,"+;
               "cTran=IIF(nTotQty>0,'Issue','Return') :H='Type' :10 :R,"
    lcFields = lcFields + ;
               "dTranDate :H='Date' :R,nSession=IIF(nTotQty>0,cISession,cRSession) :H='Session' :R,"+;
               "QTy=ABS(nTotQty) :H='TotQty' :R,nUnitCst :H='Unit Cost' :R,Amnt=ABS(nTotCst) :H='Amount' :P='999999999999' :R,"+;
               "nUnitAcSt :H='Actual Unit Cost' :R,AAmnt=ABS(nTotAcSt) :H='Actual Amount' :P='999999999999' :R,"+;
               "cDesc=gfCodDes(cOprCode,'MFGCODE') :H='Operation' :R"+;
               IIF(laData[28]='M',",cLotNo :H='Lot#' :R",'')

    *B603348,1 ABD [End]
    *B802636,1 Added cDyelot to FOR expresion in the following browse command.
    BROWSE FIELDS &lcFields  ;
           WINDOW MFISLOG1   ;
           IN WINDOW MFISLOG ;
           NOMENU            ;         
           NOAPPEND          ;
           NODELETE          ;         
           NOWAIT            ;
           SAVE              ;
           NOCLEAR           ;
           KEY m.Typ+lcTranType+laData[1]+m.Item+m.IClr+m.MfgCode ;
           FOR cDyelot=IIF(!EMPTY(m.Dyelot),m.Dyelot,'') ;
           WHEN lfShowLog()  ;
           TITLE lcIssLog
ENDCASE
=lfShowLog()
=lfRefresh('MFISLOG0')

*!*************************************************************
*! Name      : lfShowLog
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Show Issue Log
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfShowLog()
*!*************************************************************
FUNCTION lfShowLog
PRIVATE lcIssStat,lcRetStat

lnLMarker = RECNO()
SHOW WINDOW (lcIssLog) REFRESH SAME
lcIssStat = IIF(EVAL(lcCondition),'ENABLE','DISABLE')
lcRetStat = IIF(EVAL(lcRetCond),'ENABLE','DISABLE')
SHOW GET pbActCost &lcIssStat
SHOW GET pbReturn  &lcRetStat 
SHOW GET pbRolls   &lcIssStat

*!*************************************************************
*! Name      : lfvLotWare
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Get fabric open lots for specefic warehouse
*!*************************************************************
*! Calls     : lfRefresh,lfShowLot
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvLotWare()
*!*************************************************************
FUNCTION lfvLotWare

*lcIssWare = laMatWare[lnIssWare,2]
lcIssWare = IIF(lnIssWare=0,' ',laMatWare[lnIssWare,2])
=SEEK(SUBSTR(m.Item,1,7)+m.IClr+lcIssWare+m.Dyelot,'FABDYE')
lnOnHand = FABDYE.OnHand
=lfRefresh('MFOPLOT0')
SELECT (lcOpenLots)
SET KEY TO lcIssWare+ALLTRIM(m.Dyelot)
=SEEK(lcIssWare+ALLTRIM(m.Dyelot))
=lfShowLot()

*!*************************************************************
*! Name      : lfvIssWare
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate fabric issued Warehouse
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvIssWare()
*!*************************************************************
FUNCTION lfvIssWare

lcIssWare = laMatWare[lnIssWare,2]
=SEEK(SUBSTR(m.Item,1,7)+m.IClr+lcIssWare,'FabDye')

*B037268,1 ABD - Amendment to PO cost sheet 'issue/return' to allow return
*B037268,1 ABD - Of trims to a different warehouse. [Begin]
*m.Issue_Qty=MIN(MAX(m.Req_Qty-m.Used_qty,0),FabDye.OnHand)
*lnIssCost = IIF(laSetups[9,2]='A',FabDye.nAveCstBuy,Fabric.CostBuy)/Fabric.Conv
IF ASCAN(laEvntTrig,PADR("ASSGQTYZ",10)) = 0 
  m.Issue_Qty=MIN(MAX(m.Req_Qty-m.Used_qty,0),FabDye.OnHand)
  lnIssCost = IIF(laSetups[9,2]='A',FabDye.nAveCstBuy,Fabric.CostBuy)/Fabric.Conv
ENDIF
*B037268,1 ABD - [End]

=lfRefresh('MFITMISS')
SHOW GETS WINDOW 'MFITMISS' ONLY

*!*************************************************************
*! Name      : lfvActCost
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Issue Cost Item
*!*************************************************************
*! Calls     : MFITMISS.SPX,MFSTYISS.SPX,MFMFGISS.SPX,lfShowLog
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvIssue()
*!*************************************************************
FUNCTION lfvActCost
PRIVATE lcIssWare,lnIssCost

SELECT (lcTktSheet)
SCATTER MEMVAR
DO CASE 

  *B608415,1 MMT 01/27/2008 Fix bug of Cost value when user press on actual cost button[Start] 
  *CASE INLIST(m.cCatgTyp,'F','T') .AND. ;
       SEEK(MatInvJl.cFabric+MatInvJl.cColor,'Fabric')
  CASE INLIST(m.cCatgTyp,'F','T') and;
       iif(!INLIST(laSetups[9,2],"A","S"), SEEK(MatInvJl.cFabric+MatInvJl.cColor,'Fabric'),.T.)
       
     IF INLIST(laSetups[9,2],"A","S") 
	   lcOldTable = Select()
       Select MatInvJl
       lcOldMatOrd = order('MatInvJl')
       set order to 'MTINVSEQ' in 'MatInvJl'
       =Seek(BOMCOST.Cisession)
       set order to (lcOldMatOrd) in 'MatInvJl'
       Select(lcOldTable)
     endif  

  *B608415,1 MMT 01/27/2008 Fix bug of Cost value when user press on actual cost button[End]  
  
    *B607271,1 KHM 06/04/2003 (Begin) Use the ceiling function.
    *lnIssWare = ASCAN(laMatWare,MatInvJl.cWareCode)/2
    lnIssWare = CEILING(ASCAN(laMatWare,MatInvJl.cWareCode)/2)
    *B607271,1 KHM 06/04/2003 (End)
    
    lcIssWare = MatInvJl.cWareCode
    ldIssDate = MatInvJl.dTranDate
    llItemDye = !EMPTY(MatInvJl.cDyelot)
    lnIssCost = MatInvJl.nUntCstBuy/Fabric.Conv
    lcWareStat  = 'DISABLE'
    m.Issue_Qty = MatInvJl.nIssued
    lnLotNo = IIF(EMPTY(m.cOprCode),1,ASCAN(laLots,MatInvJl.cLotNo))
    DO (gcScrDir+"MFITMISS.SPX") WITH .T.,.T.
  CASE m.cCatgTyp='S' .AND. SEEK(m.Item,'Style') .AND. SEEK('S'+Style.Scale,'Scale')
    
    *B607271,1 KHM 06/04/2003 (Begin) Use the ceiling function.
    *lnIssWare = ASCAN(laStyWare,StyInvJl.cWareCode)/2
    lnIssWare = CEILING(ASCAN(laStyWare,StyInvJl.cWareCode)/2)
    *B607271,1 KHM 06/04/2003 (End)
        
    ldIssDate = StyInvJl.dTrDate
    llItemDye = !EMPTY(StyInvJl.cDyelot)
    lnIssCost = StyInvJl.nCost
    m.Iss_Qty1 = ABS(StyInvJl.nStk1)
    m.Iss_Qty2 = ABS(StyInvJl.nStk2)
    m.Iss_Qty3 = ABS(StyInvJl.nStk3)
    m.Iss_Qty4 = ABS(StyInvJl.nStk4)
    m.Iss_Qty5 = ABS(StyInvJl.nStk5)
    m.Iss_Qty6 = ABS(StyInvJl.nStk6)
    m.Iss_Qty7 = ABS(StyInvJl.nStk7)
    m.Iss_Qty8 = ABS(StyInvJl.nStk8)
    m.Issue_Qty= ABS(StyInvJl.nTotStk)
    lnLotNo = IIF(EMPTY(m.cOprCode),1,ASCAN(laLots,StyInvJl.cLotNo))
    DO (gcScrDir+"MFSTYISS.SPX") WITH .T.,.T.
  OTHERWISE
    lnIssCost   = BomCost.nUnitCst
    m.Issue_Qty = BomCost.nTotQty
    ldIssDate   = BomCost.dTranDate
    lnLotNo = IIF(EMPTY(m.cOprCode),1,ASCAN(laLots,BomCost.cLotNo))
    DO (gcScrDir+"MFMFGISS.SPX") WITH .T.,.T.
ENDCASE
=lfRefresh('MFISLOG0')
*B123774,1 MHM 10/12/2004 Refresh the screen with new temp[Start]
*=lfShowLog()
=lfBrowLog()
*B123774,1 MHM [End]

*!*************************************************************
*! Name      : lfvIssue
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Issue Cost Item
*!*************************************************************
*! Calls     : MFOPLOT.SPX,MFITMISS.SPX,MFSTYISS.SPX,MFMFGISS.SPX,lfShowLog
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvIssue()
*!*************************************************************
FUNCTION lfvIssue
PRIVATE lcIssWare,lnIssCost
ldIssDate = gdSysDate
SELECT (lcTktSheet)
SCATTER MEMVAR

*C200255,1 (Begin) Update MATINVJL with custom Voucher no for CON10.
IF INLIST(m.cCatgTyp,'F','T') .AND. SEEK(SUBSTR(m.Item,1,7)+m.IClr,'Fabric')
  IF ASCAN(laEvntTrig,PADR("GETVOUT",10)) <> 0
    lcVoucNo = ""
    llContVout = .T.
    = gfDoTriger("MAPOREC",PADR("GETVOUT",10))
    IF !llContVout
      RETURN
    ENDIF
    llFrstTime = .F.
  ENDIF  
ENDIF
*C200255,1 (Begin)

DO CASE 
  CASE laSetups[9,2] = 'L' .AND. INLIST(m.cCatgTyp,'F','T') .AND. ;
    SEEK(SUBSTR(m.Item,1,7)+m.IClr,'Fabric')
    *--- DOC.
    *--- Get the Open lots from materail inventory journal to issue cost element from it ,
    *--  if the cost method is LFO
    *--- DOC.
    SELECT cFabric,cColor,cWareCode,cDyelot,cRSession,cISession,nUnitCost,;
           cApInvNo,dTranDate,nRecCost,cTran,SUM(nReceived-nIssued) AS 'nBalance',;
           nUntCstBuy,00000000.000 AS nToIssue FROM MatInvJl ;
    WHERE  cFabric+cColor+cWareCode+cDyelot+cRSession+cISession = ;
           SUBSTR(m.Item,1,7)+m.IClr .AND. IIF(EMPTY(m.Dyelot),.T.,cDyelot=m.Dyelot);
    GROUP BY cFabric,cColor,cWareCode,cDyelot,cRSession ;
    HAVING nBalance > 0 INTO DBF (gcWorkDir+lcOpenLots)
    IF _TALLY > 0
      INDEX ON cWareCode+cDyelot TAG (lcOpenLots)
      
      *B604073,1 KHM 12/25/2000 (Begin) Changing the relation.
      *SET RELATION TO cRSession INTO FABINV
      SET RELATION TO cFabric+cColor+cWareCode+cDyelot+cRSession INTO FABINV
      *B604073,1 KHM 12/25/2000 (End)
      
      SELECT FABINV
      SET RELATION TO 'P'+cTran INTO PofHdr
      lnOMarker = 0
      llItemDye = laSetups[12,2]='Y' .AND. Fabric.cDye_Flg = 'Y'
      lcIssWare = laData[32]
      
      *B607271,1 KHM 06/04/2003 (Begin) Use the ceiling function.
      *lnIssWare = ASCAN(laMatWare,laData[32])/2
      lnIssWare = CEILING(ASCAN(laMatWare,laData[32])/2)
      *B607271,1 KHM 06/04/2003 (End)
          
      DO (gcScrDir+"MFOPLOT.SPX") WITH .F.
    ELSE
      *E300725,1 Message : 38094
      *E300725,1 No open lots found for fabric/color
      *E300725,1 Button : 00000
      *E300725,1 Ok
      =gfModalGen('TRM38094B00000','ALERT','fabric/color: '+SUBSTR(m.Item,1,7)+'/'+m.IClr)
    ENDIF  
    USE IN (lcOpenLots)
    SELECT MATINVJL
  CASE laSetups[9,2] <> 'L' .AND. INLIST(m.cCatgTyp,'F','T') .AND. ;
       SEEK(SUBSTR(m.Item,1,7)+m.IClr,'Fabric')
*--- DOC.
*--- Get the on hand and the cost from fabric file
*--  if the cost method is not LOT
*--- DOC.
    lcIssWare = laData[32]

    *B607271,1 KHM 06/04/2003 (Begin) Use the ceiling function.
    *lnIssWare = ASCAN(laMatWare,laData[32])/2
    lnIssWare = CEILING(ASCAN(laMatWare,laData[32])/2)
    *B607271,1 KHM 06/04/2003 (End)

    =SEEK(SUBSTR(m.Item,1,7)+m.IClr+lcIssWare+SPACE(10),'FabDye')
    llItemDye = laSetups[12,2]='Y' .AND. Fabric.cDye_Flg = 'Y' 

    *B804464,1 KHM 10/17/2001 (Begin) Getting the right cost in case of
    *B804464,1                FabDye.Onhand = 0
    *lnIssCost = IIF(laSetups[9,2]='S',Fabric.CostBuy/IIF(Fabric.Conv=0,1,Fabric.Conv),;
                IIF(FabDye.OnHand=0,0,ABS(FabDye.nStkVal/FabDye.OnHand)))

    lnIssCost = IIF(laSetups[9,2]='S',Fabric.CostBuy/IIF(Fabric.Conv=0,1,Fabric.Conv),;
                IIF(FabDye.OnHand=0,FabDye.nAveCstBuy/IIF(Fabric.Conv=0,1,Fabric.Conv),;
                    ABS(FabDye.nStkVal/FabDye.OnHand)))
    *B804464,1 KHM 10/17/2001 (End)
    
    lcWareStat  = 'ENABLE'
    *B602643,1 SSH 11/04/00 [Begin] Remove ceiling from requirements.
    *m.Issue_Qty = MAX(CEILING(m.Req_Qty)-m.Used_Qty,0)
    m.Issue_Qty = MAX(m.Req_Qty-m.Used_Qty,0)
    *B602643,1 SSH 11/04/00 [End] 
    DO (gcScrDir+"MFITMISS.SPX") WITH .T.
  CASE m.cCatgTyp='S' .AND. SEEK(m.Item,'Style') .AND. SEEK('S'+Style.Scale,'Scale')
*--- DOC.
*--- Case style and style found in style file
*--- DOC.
    lcIssWare = laData[31]
    
    *B607271,1 KHM 06/04/2003 (Begin) Use the ceiling function.
    *lnIssWare = ASCAN(laStyWare,laData[31])/2
    lnIssWare = CEILING(ASCAN(laStyWare,laData[31])/2)
    *B607271,1 KHM 06/04/2003 (End)
    
    =SEEK(m.Item+lcIssWare+SPACE(10),'StyDye')
    llItemDye = laSetups[8,2]='Y' .AND. Style.cDye_Flg = 'Y'
    lnIssCost = IIF(laSetups[10,2]='S',Style.TotCost,;
                IIF(laSetups[2,2]='Y',StyDye.Ave_Cost,Style.Ave_Cost))
    m.Iss_Qty1 = MAX(CEILING(m.Req_Qty1) - m.Used_Qty1,0)
    m.Iss_Qty2 = MAX(CEILING(m.Req_Qty2) - m.Used_Qty2,0)
    m.Iss_Qty3 = MAX(CEILING(m.Req_Qty3) - m.Used_Qty3,0)
    m.Iss_Qty4 = MAX(CEILING(m.Req_Qty4) - m.Used_Qty4,0)
    m.Iss_Qty5 = MAX(CEILING(m.Req_Qty5) - m.Used_Qty5,0)
    m.Iss_Qty6 = MAX(CEILING(m.Req_Qty6) - m.Used_Qty6,0)
    m.Iss_Qty7 = MAX(CEILING(m.Req_Qty7) - m.Used_Qty7,0)
    m.Iss_Qty8 = MAX(CEILING(m.Req_Qty8) - m.Used_Qty8,0)
    m.Issue_Qty= m.Iss_Qty1+m.Iss_Qty2+m.Iss_Qty3+m.Iss_Qty4+;
                 m.Iss_Qty5+m.Iss_Qty6+m.Iss_Qty7+m.Iss_Qty8
  
    *B039660,1 NNA 06/06/2006 (Begin) convert next trigger to be done within Binmain.Prg instead of Davmain.prg 
    *C123853,1 MHM 01/15/2005  open custom screen for David Luke[Start]
    *DO (gcScrDir+"MFSTYISS.SPX") WITH .T.
    IF ASCAN(laEvntTrig,PADR("LDFNPOIS",10)) <> 0 
      =gfDoTriger("POCSSH",PADR("LDFNPOIS",10))
    ELSE
      DO (gcScrDir+"MFSTYISS.SPX") WITH .T.
    ENDIF
    *C123853,1 MHM 01/15/2005  [End]
    *B039660,1 NNA (End)

    
  CASE INLIST(m.cCatgTyp,'M','P','D')
    IF !('AP' $ gcCmpModules) .OR. ;
       (m.cCatGTyp = 'M' .AND. gfRltFld(m.MfgCode,@laMfgRFld,'MFGCODE') ;
       .AND. !EMPTY(lcMfgGlAcnt)) 
*--- DOC.
*--- Case mfg operation , purchase price or duty
*--- and AP module is not installed.
*--- if the AP module installed issue this operation cost from AP module
*--- DOC.
      lnIssCost   = m.UntCost
      m.Issue_Qty = MAX(CEILING(m.Req_Qty)-m.Used_Qty,0)
      DO (gcScrDir+"MFMFGISS.SPX") WITH .T.
    ELSE
      IF m.cCatGTyp $ 'PD'
        *E300725,1 Message : 38091
        *E300725,1 You cannot issue the duty cost items since you are linked
        *E300725,1 with the AP. The duty cost application is done from the AP module.
        *E300725,1 Button : 00000
        *E300725,1 Ok
        *B603861,1 AMH 09/12/2000 add variabel to Message 38091 [Start]
        *=gfModalGen('TRM38091B00000','ALERT',IIF(m.cCatGTyp='D','duty|duty','purchase|purchase'))
        =gfModalGen('TRM38091B00000','ALERT',IIF(m.cCatGTyp='D','issue|duty|duty','issue|purchase|purchase'))
        *B603861,1 AMH 09/12/2000 add variabel to Message 38091 [End  ]
      ELSE
        *E300725,1 Message : 38092
        *E300725,1 This MFG code has been setup to be applied from the A/P.
        *E300725,1 Button : 00000
        *E300725,1 Ok
        =gfModalGen('TRM38092B00000','ALERT')
      ENDIF          
    ENDIF
ENDCASE
=lfRefresh('MFISLOG0')
*B123774,1 MHM 10/12/2004 Refresh the screen with new temp[Start]
*=lfShowLog()
=lfBrowLog()
*B123774,1 MHM [End]

*!*************************************************************
*! Name      : lfvBrowLots
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Browse fabric/colors open lots
*!*************************************************************
*! Calls     : lfShowLot
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvBrowLots()
*!*************************************************************
FUNCTION lfvBrowLots
PARAMETERS llAuto
PRIVATE lcFields

SELECT (lcOpenLots)
SET KEY TO lcIssWare+ALLTRIM(m.Dyelot)
=SEEK(lcIssWare)
lnOMarker = RECNO()
lcFields = "cMarker =IIF(RECNO()=lnOMarker ,'>',' '):H=' ':R:1:W=.F.,"+;
           "PofHdr.PoMat :H= 'PO#' :R,PofHdr.Vendor :H= 'Vendor' :R,"+;
           "cRSession :H= 'Session#' :R,dTranDate :H='Date' :R ,"+IIF(!EMPTY(m.Dyelot),'',"cDyelot :H='Dyelot' :R,")
lcFields = lcFields +"nBalance :H='Quantity' :P='99999999' :R ,nUnitCost :H='Unit Cost' :R,"+;
           "Amount= (nUntCstBuy*nBalance/FABRIC.CONV) :P='9999999999.999' :R"+;
           IIF(llAuto,",nToIssue :H='Issue' :W=lfwLotToIs() :V=lfvLotToIs() :P='99999999.999'",'')+",cApInvNo :H='Invoice' :R"
BROWSE FIELDS &lcFields ;
       WINDOW MFOPLOT1  ;
       IN WINDOW MFOPLOT ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       FOR nBalance > 0  ;
       WHEN lfShowLot()  ;
       TITLE lcLotTitle

*!*************************************************************
*! Name      : lfShowLot
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Show fabric/colors open lots
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfShowLot()
*!*************************************************************
FUNCTION lfShowLot

lnOMarker = RECNO(lcOpenLots)
SHOW WINDOW (lcLotTitle) REFRESH SAME
IF nBalance <= 0
  SHOW GET pbIssue DISABLE
ELSE
  SHOW GET pbIssue ENABLE
ENDIF

FUNCTION lfwLotToIs
lcOldValue = nToIssue

FUNCTION lfvLotToIs
IF nToIssue > nBalance
  *E300725,1 Message : 38086
  *E300725,1 Quantity cannot exceed lot open quantity.
  *E300725,1 Button : 00000
  *E300725,1 Ok
  =gfModalGen('TRM38086B00000','ALERT')
  REPLACE nToIssue WITH lcOldValue
  RETURN
ENDIF
IF m.Used_Qty +lcOldValue - nToIssue < 0
  *E300725,1 Message : 38087
  *E300725,1 Contributed quantity cannot exceed total issued quantity.
  *E300725,1 Button : 00000
  *E300725,1 Ok
  =gfModalGen('TRM38087B00000','ALERT')
  REPLACE nToIssue WITH lcOldValue
  RETURN
ENDIF
m.Used_Qty = m.Used_Qty +lcOldValue - nToIssue
=lfRefresh('MFOPLOT0')

*!*************************************************************
*! Name      : lfvIssLot 
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Issue fabric/color from open lot
*!*************************************************************
*! Calls     : MFITMISS.SPX,lfvItmIss,lfvBrowLots
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvIssLot()
*!*************************************************************
FUNCTION lfvIssLot 
PARAMETERS llAuto
PRIVATE llOk
*--- DOC.
*--- Issue fabric/color by callng function [lfIssRetFab]
*--- DOC.
IF llAuto
  *E300725,1 Message : 38088
  *E300725,1 Contributed quantity does not total the issued quantity. 
  *E300725,1 Would you like to modify the issued quantity?
  *E300725,1 Button : 38010
  *E300725,1 Proceed  Resume
  IF m.Used_Qty = 0 .OR. gfModalGen('QRM38088B38010','ALERT')=1
    SELECT (lcOpenLots)
    SCAN FOR nToIssue > 0
      *E301121,4 Use the material control global function
      *=lfIssRetFab(&lcIssLtFile..Typ,&lcIssLtFile..cCatgTyp,cFabric,cColor,cWareCode,;
                   cDyelot,nToIssue,&lcOpenLots..nUntCstBuy,lcOprCode,;
                   lcLotNo,ldIssDate,&lcOpenLots..cRSession,lcSession)
      =lfIssRetFab(&lcIssLtFile..Typ,&lcIssLtFile..cCatgTyp,cFabric,cColor,cWareCode,;
                   cDyelot,nToIssue,&lcOpenLots..nUntCstBuy,lcOprCode,;
                   lcLotNo,ldIssDate,&lcOpenLots..cRSession,gfsequence('GLSession'))
      *E301121,4 (End)
    ENDSCAN
    CLEAR READ
  ENDIF
  RETURN
ENDIF
ldIssDate   = gdSysDate
lcWareStat  = 'DISABLE'
m.Issue_Qty = MIN(MAX(m.Req_Qty-m.Used_Qty,0),&lcOpenLots..nBalance)
lnIssCost   = &lcOpenLots..nUntCstBuy/Fabric.Conv
m.Dyelot    = &lcOpenLots..cDyelot
lnCurObj    = 0
*--- DOC.
*--- the next loop becaus the rea dlevel exceed the avilable 5
*--- this part if you are issue fabric/color but not automatic issu
*--- DOC.
DO WHILE .T.
  llOk  = .F.
  DO (gcScrDir+"MFITMISS.SPX") WITH .T.
  IF !llOk .OR. lfvItmIss(.T.)
    EXIT
  ENDIF
ENDDO
IF llOk
  SELECT (lcOpenLots)
  REPLACE nBalance WITH MAX(nBalance - m.Issue_Qty,0)
  =lfvLotWare()
ENDIF

*!*************************************************************
*! Name      : lfvMfgIss
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Accept issue/Return MFG
*!*************************************************************
*! Calls     : gfModalGen,CHECKPRD,lfIssRetMfg
*!*************************************************************
*! Parameters: llIssue   : .T.  Issue
*!                         .F.  Return
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvMfgIss()
*!*************************************************************
FUNCTION lfvMfgIss
PARAMETERS llIssue,llActCst
*--- DOC.
*--- Call the function "lfIssRetMfg" to issue or return the MFG operation
*--- DOC.
IF llActCst
  *E500231,1 Create new session number for evry MFG issue.
  *llSameSess = (BomCost.cISession=lcSession)
  *=lfIssRetMfg(m.Typ,m.cCatgTyp,SUBSTR(m.Item,1,7),m.IClr,m.MfgCode,BomCost.cWareCode,;
              m.Dyelot,-m.Issue_Qty,BomCost.nUnitCst,;
              ldIssDate,m.cOprCode,BomCost.cLotNo,;
              IIF(llSameSess,SPACE(6),lcSession),IIF(llSameSess,lcSession,SPACE(6)))
  *=lfIssRetMfg(m.Typ,m.cCatgTyp,SUBSTR(m.Item,1,7),m.IClr,m.MfgCode,BomCost.cWareCode,;
              m.Dyelot,m.Issue_Qty,lnIssCost,ldIssDate,m.cOprCode,BomCost.cLotNo,SPACE(6),lcSession)
  =lfIssRetMfg(m.Typ,m.cCatgTyp,SUBSTR(m.Item,1,7),m.IClr,m.MfgCode,BomCost.cWareCode,;
              m.Dyelot,-m.Issue_Qty,BomCost.nUnitCst,ldIssDate,m.cOprCode,;
              BomCost.cLotNo,gfsequence('GLSession'),SPACE(6))
  =lfIssRetMfg(m.Typ,m.cCatgTyp,SUBSTR(m.Item,1,7),m.IClr,m.MfgCode,BomCost.cWareCode,;
              m.Dyelot,m.Issue_Qty,lnIssCost,ldIssDate,m.cOprCode,;
              BomCost.cLotNo,SPACE(6),gfsequence('GLSession'))
  *E500231,1 (End)
  CLEAR READ
  RETURN
ENDIF
IF !llIssue .AND. m.Issue_Qty > m.Used_Qty
  *E300725,1 Message : 38057
  *E300725,1 Return quantity cannot exceed issued quantity
  *E300725,1 Button : 00000
  *E300725,1 Ok
  =gfModalGen('TRM38057B00000','ALERT')
  m.Issue_Qty = m.Used_Qty
  _CUROBJ = OBJNUM(m.Issue_Qty)
  RETURN
ENDIF
IF !CHECKPRD(ldIssDate,'lcGlYear','lcGlPeriod','IA')
  _CUROBJ = OBJNUM(ldIssDate)
  RETURN
ENDIF
CLEAR READ
*E500231,1 Create new session number for evry MFG issue.
*=lfIssRetMfg(m.Typ,m.cCatgTyp,SUBSTR(m.Item,1,7),m.IClr,m.MfgCode,m.cWareCode,;
             m.Dyelot,IIF(llIssue,m.Issue_Qty,-m.Issue_Qty),lnIssCost,;
             ldIssDate,m.cOprCode,IIF(EMPTY(m.cOprCode),'',laLots[lnLotNo]),;
             IIF(llIssue,SPACE(6),lcSession),IIF(llIssue,lcSession,SPACE(6)))
=lfIssRetMfg(m.Typ,m.cCatgTyp,SUBSTR(m.Item,1,7),m.IClr,m.MfgCode,m.cWareCode,;
             m.Dyelot,IIF(llIssue,m.Issue_Qty,-m.Issue_Qty),lnIssCost,;
             ldIssDate,m.cOprCode,IIF(EMPTY(m.cOprCode),'',laLots[lnLotNo]),;
             IIF(llIssue,SPACE(6),gfsequence('GLSession')),;
             IIF(llIssue,gfsequence('GLSession'),SPACE(6)))
*E500231,1 (End)
m.Used_Qty = m.Used_Qty + IIF(llIssue,m.Issue_Qty,-m.Issue_Qty)

*!*************************************************************
*! Name      : lfvItmIss
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Accept issue/Return Fabric/Color
*!*************************************************************
*! Calls     : gfModalGen,CHECKPRD,lfIssRetFab,gpAdFabWar
*!*************************************************************
*! Parameters: llIssue   : .T.  Issue
*!                         .F.  Return
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvItmIss()
*!*************************************************************
FUNCTION lfvItmIss

*E301897,1 AMH Add parameter for edit used quantity [Start]
*PARAMETERS llIssue,llActCst
PARAMETERS llIssue,llActCst,llEditUsed
*E301897,1 AMH [End]

IF laSetups[2,2]='Y' .AND. ;
  !SEEK (SUBSTR(m.Item,1,7)+m.IClr+lcIssWare+SPACE(10),'FABDYE')
  *E300725,1 Message : 38029
  *E300725,1 Item/Color xxxxx/xxxx is not assigned to warehouse xxxx
  *E300725,1 Button : 38001
  *E300725,1 Add Cancel
*--- DOC.
*--- Check if the FAbric/Color is assigned to the warehouse
*--- DOC.
  IF gfModalGen('QRM38029B38001','ALERT','Item/Color: '+ALLTRIM(m.Item)+'/'+ALLTRIM(m.IClr)+'|'+lcIssWare) = 1
    DO gpAdFabWar WITH SUBSTR(m.Item,1,7),m.IClr,SPACE(10),lcIssWare
  ELSE
    RETURN
  ENDIF   
ENDIF
IF laSetups[12,2]='Y' AND SEEK(SUBSTR(m.Item,1,7)+m.IClr,'Fabric') AND Fabric.cDye_Flg='Y'
  lcDyelot = IIF(EMPTY(m.Dyelot),'?',m.Dyelot)
  *B606285,1 ABD - Remark the next line and write a new code to handel the issue from
  *B606285,1 ABD - other warehouse. [Begin]
  *IF !SEEK(SUBSTR(m.Item,1,7)+m.IClr+lcIssWare+lcDyelot,'FABDYE') AND ;
  *   !FDYEBROW(m.Item,m.IClr,@lcDyelot,.T.,lcIssWare)
  *    RETURN
  *  ENDIF
  IF !SEEK(SUBSTR(m.Item,1,7)+m.IClr+lcIssWare+lcDyelot,'FABDYE')
     = FDYEBROW(m.Item,m.IClr,@lcDyelot,.T.,lcIssWare)
     IF EMPTy(lcDyelot)
       RETURN
    ENDIF
  ENDIF   
  *B606285,1 ABD - [End]
  m.Dyelot = lcDyelot
  SELECT (lcTktSheet)
  REPLACE Dyelot WITH m.Dyelot
  SELECT CTktBom
  *B802636,1 Start, added dyelot.
  *=SEEK(m.cimtyp+m.cuttkt+m.typ+m.item+m.iclr+m.mfgcode)
  
  *B607271,1 KHM 06/04/2003 (Begin) If the user did not select a dyelot from the beginning
  *B607271,1                then issue the fabric the dyelot field in the cTktBom file is 
  *B607271,1                empty and the seek will fail 
  *=SEEK(m.cimtyp+m.cuttkt+m.typ+m.item+m.iclr+m.mfgcode+IIF(!EMPTY(m.Dyelot),m.Dyelot,''))
  IF !SEEK(m.cimtyp+m.cuttkt+m.typ+m.item+m.iclr+m.mfgcode+IIF(!EMPTY(m.Dyelot),m.Dyelot,''))
    =SEEK(m.cimtyp+m.cuttkt+m.typ+m.item+m.iclr+m.mfgcode+SPACE(10))
  ENDIF
  *B607271,1 KHM 06/04/2003 (End)
  
  REPLACE Dyelot WITH m.Dyelot
  *B802636,1 End.

  *B607271,1 KHM 06/04/2003 (Begin) If the user did not select a dyelot from the beginning
  *B607271,1                then issue the fabric the dyelot field in the BomLine file is 
  *B607271,1                empty and it should be updated.
  IF SEEK(m.cimtyp+"1"+m.cuttkt,'BomLine')
    SELECT BomLine
    LOCATE REST WHILE cIMTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+Item+IClr+MfgCode=;
                      m.cIMTyp+"1"+m.CutTkt;
                FOR Item+IClr+MfgCode = m.Item+m.IClr+m.MfgCode AND Dyelot = m.Dyelot
    IF !FOUND()
      =SEEK(m.cimtyp+"1"+m.cuttkt)
      LOCATE REST WHILE cIMTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+Item+IClr+MfgCode=;
                        m.cIMTyp+"1"+m.CutTkt;
                  FOR Item+IClr+MfgCode = m.Item+m.IClr+m.MfgCode
      IF FOUND()
        REPLACE REST Dyelot WITH m.Dyelot ;
                WHILE cIMTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+Item+IClr+MfgCode=;
                      m.cIMTyp;
                FOR cTktNo = m.CutTkt AND Item+IClr+MfgCode = m.Item+m.IClr+m.MfgCode
      ENDIF
    ENDIF
  ENDIF
  *B607271,1 KHM 06/04/2003 (End)
  
ENDIF

IF llActCst
  =SEEK(SUBSTR(m.Item,1,7)+m.IClr,'Fabric')
  *E301121,4 Use the material control global function
  *=lfIssRetFab(m.Typ,m.cCatgTyp,SUBSTR(m.Item,1,7),m.IClr,MatInvJl.cWareCode,;
               m.Dyelot,-m.Issue_Qty,MatInvJl.nUntCstBuy,m.cOprCode,;
               MatInvJl.cLotNo,ldIssDate,lcSession,SPACE(6))
  *=lfIssRetFab(m.Typ,m.cCatgTyp,SUBSTR(m.Item,1,7),m.IClr,MatInvJl.cWareCode,;
               m.Dyelot,m.Issue_Qty,lnIssCost*Fabric.Conv,m.cOprCode,;
               MatInvJl.cLotNo,ldIssDate,SPACE(6),lcSession)
*--- DOC.
*--- Call functoin to issue / return fabri/color
*--- DOC.
  *C200255,1 (Begin) Initialize voucher no
  llFrstTime = .F.
  *C200255,1 (End)

  =lfIssRetFab(m.Typ,m.cCatgTyp,SUBSTR(m.Item,1,7),m.IClr,MatInvJl.cWareCode,;
               m.Dyelot,-m.Issue_Qty,MatInvJl.nUntCstBuy,m.cOprCode,;
               MatInvJl.cLotNo,ldIssDate,gfsequence('GLSession'),SPACE(6))
  *B603033,1 (Start)
  *=lfIssRetFab(m.Typ,m.cCatgTyp,SUBSTR(m.Item,1,7),m.IClr,MatInvJl.cWareCode,;
               m.Dyelot,m.Issue_Qty,lnIssCost*Fabric.Conv,m.cOprCode,;
               MatInvJl.cLotNo,ldIssDate,SPACE(6),gfsequence('GLSession'))
  =lfIssRetFab(m.Typ,m.cCatgTyp,SUBSTR(m.Item,1,7),m.IClr,MatInvJl.cWareCode,;
               m.Dyelot,m.Issue_Qty,lnIssCost*Fabric.Conv,m.cOprCode,;
               MatInvJl.cLotNo,ldIssDate,SPACE(6),gfsequence('GLSession'),.F.,.T.)
  *B603033,1 (End)
  *C200255,1 (Begin) Initialize voucher no
  llFrstTime = .F.
  *C200255,1 (End)
  *E301121,4 (End)
  CLEAR READ
  RELEASE WINDOW 'MFITMISS'
  RETURN
ENDIF
*B606285,1 ABD - set the pointer to the correct warehouse - dyelot for the 
*B606285,1 ABD - current fabric - color, that to get the corrct onhand Qty. [Begin]
= SEEK(LEFT(m.Item,7)+m.IClr+lcIssWare+SPACE(10),'FABDYE')
*B606285,1 ABD - [End]


*E301897,1 AMH Consider case of edit used quantity [Start]
IF llEditUsed
  IF m.Used_Qty <> lnTotQty
    lnIssueQty  = m.Issue_Qty
    m.Issue_Qty = m.Used_Qty - lnTotQty
    IF m.Used_Qty >= lnTotQty
      IF INLIST(laSetups[9,2],'F','I') .AND. m.Issue_Qty > FabDye.OnHand
        *--- Message : 38064
        *--- Issued quantity cannot exceed lot open quantity
        *--- Button : 00000
        *--- Ok
        =gfModalGen('TRM38064B00000','ALERT')
        m.Used_Qty = MIN(m.Used_Qty,FabDye.OnHand)
        m.Issue_Qty = lnIssueQty
        _CUROBJ = OBJNUM(m.Used_Qty)
        RETURN .F.
      ENDIF
      
      IF !CHECKPRD(ldIssDate,'lcGlYear','lcGlPeriod','IA')
        _CUROBJ = OBJNUM(ldIssDate)
        m.Issue_Qty = lnIssueQty
        RETURN .F.
      ENDIF
      
      IF INLIST(laSetups[9,2],'F','I')
        *--- DOC.
        *--- Function to Issue Fabrics and Inventory mantained trims FIFO/LIFO
        *--- DOC.
        =lfIssSeq(laSetups[9,2],m.Typ,m.cCatgTyp,SUBSTR(m.Item,1,7),m.IClr,;
                  lcIssWare,m.Dyelot,m.Issue_Qty,m.cOprCode,;
                  IIF(EMPTY(m.cOprCode),'',laLots[lnLotNo]),ldIssDate)
      ELSE
        =lfIssRetFab(m.Typ,m.cCatgTyp,SUBSTR(m.Item,1,7),m.IClr,lcIssWare,m.Dyelot,;
                     m.Issue_Qty,lnIssCost*Fabric.Conv,m.cOprCode,;
                     IIF(EMPTY(m.cOprCode),'',laLots[lnLotNo]),ldIssDate,;
                     IIF(laSetups[9,2]='L',&lcOpenLots..cRSession,SPACE(6)),;
                     gfsequence('GLSession'))
      ENDIF
      m.Issue_Qty = m.Issue_Qty + lnIssueQty
    ELSE
      IF !CHECKPRD(ldIssDate,'lcGlYear','lcGlPeriod','IA')
        _CUROBJ = OBJNUM(ldIssDate)
        m.Issue_Qty = lnIssueQty
        RETURN
      ENDIF
      =lfIssRetFab(m.Typ,m.cCatgTyp,SUBSTR(m.Item,1,7),m.IClr,lcIssWare,m.Dyelot,;  
                   m.Issue_Qty,lnIssCost*Fabric.Conv,m.cOprCode,m.cLotNo,ldIssDate,;
                   IIF(INLIST(laSetups[9,2],'L','F','I'),BomCost.cRSession,gfsequence('GLSession')),;
                   IIF(INLIST(laSetups[9,2],'L','F','I'),BomCost.cISession,IIF(FABDYE.ONHAND<0,MATINVJL.CISESSION,SPACE(6))))
      m.Issue_Qty = lnIssueQty
    ENDIF
  ENDIF
  CLEAR READ
  RELEASE WINDOW 'MFITMISS'
  RETURN
ENDIF
*E301897,1 AMH [End]

IF llIssue
  IF INLIST(laSetups[9,2],'F','I') .AND. m.Issue_Qty > FabDye.OnHand
    *E300725,1 Message : 38064
    *E300725,1 Issued quantity cannot exceed lot open quantity
    *E300725,1 Button : 00000
    *E300725,1 Ok
    =gfModalGen('TRM38064B00000','ALERT')
    m.Issue_Qty = MIN(MAX(m.Req_Qty-m.Used_Qty,0),FabDye.OnHand)
    _CUROBJ = OBJNUM(m.Issue_Qty)
    RETURN .F.
  ENDIF
  IF laSetups[9,2] = 'L' .AND. m.Issue_Qty > &lcOpenLots..nBalance
    *E300725,1 Message : 38064
    *E300725,1 Issued quantity cannot exceed lot open quantity
    *E300725,1 Button : 00000
    *E300725,1 Ok
    =gfModalGen('TRM38064B00000','ALERT')
    m.Issue_Qty = MIN(MAX(m.Req_Qty-m.Used_Qty,0),&lcOpenLots..nBalance)
    lnCurObj =OBJNUM(m.Issue_Qty)
    _CUROBJ = OBJNUM(m.Issue_Qty)
    RETURN .F.
  ENDIF
  IF !CHECKPRD(ldIssDate,'lcGlYear','lcGlPeriod','IA')
    _CUROBJ = OBJNUM(ldIssDate)
    RETURN .F.
  ENDIF
  IF INLIST(laSetups[9,2],'F','I')
*--- DOC.
*--- Function to Issue Fabrics and Inventory mantained trims FIFO/LIFO
*--- DOC.
    =lfIssSeq(laSetups[9,2],m.Typ,m.cCatgTyp,SUBSTR(m.Item,1,7),m.IClr,;
              lcIssWare,m.Dyelot,m.Issue_Qty,m.cOprCode,;
              IIF(EMPTY(m.cOprCode),'',laLots[lnLotNo]),ldIssDate)
  ELSE
    *E301121,4 Use the material control global function
    *=lfIssRetFab(m.Typ,m.cCatgTyp,SUBSTR(m.Item,1,7),m.IClr,lcIssWare,m.Dyelot,;
                 m.Issue_Qty,lnIssCost*Fabric.Conv,m.cOprCode,;
                 IIF(EMPTY(m.cOprCode),'',laLots[lnLotNo]),ldIssDate,;
                 IIF(laSetups[9,2]='L',&lcOpenLots..cRSession,SPACE(6)),lcSession)
    =lfIssRetFab(m.Typ,m.cCatgTyp,SUBSTR(m.Item,1,7),m.IClr,lcIssWare,m.Dyelot,;
                 m.Issue_Qty,lnIssCost*Fabric.Conv,m.cOprCode,;
                 IIF(EMPTY(m.cOprCode),'',laLots[lnLotNo]),ldIssDate,;
                 IIF(laSetups[9,2]='L',&lcOpenLots..cRSession,SPACE(6)),;
                 gfsequence('GLSession'))
    *E301121,4 (End)
  ENDIF
ELSE
  IF m.Issue_Qty > lnTotQty
    *E300725,1 Message : 38057
    *E300725,1 Return quantity cannot exceed issued quantity
    *E300725,1 Button : 00000
    *E300725,1 Ok
    =gfModalGen('TRM38057B00000','ALERT')
    m.Issue_Qty = lnTotQty
    _CUROBJ = OBJNUM(m.Issue_Qty)
    RETURN
  ENDIF
  IF !CHECKPRD(ldIssDate,'lcGlYear','lcGlPeriod','IA')
    _CUROBJ = OBJNUM(ldIssDate)
    RETURN
  ENDIF
  *E301121,4 Use the material control global function
  *=lfIssRetFab(m.Typ,m.cCatgTyp,SUBSTR(m.Item,1,7),m.IClr,lcIssWare,m.Dyelot,;  
               -m.Issue_Qty,lnIssCost*Fabric.Conv,m.cOprCode,m.cLotNo,ldIssDate,;
               IIF(INLIST(laSetups[9,2],'L','F','I'),BomCost.cRSession,lcSession),;
               IIF(INLIST(laSetups[9,2],'L','F','I'),BomCost.cISession,SPACE(6)))
  
  *E301897,1 AMH Send the issue session in case of negative onhand [Start]
  *=lfIssRetFab(m.Typ,m.cCatgTyp,SUBSTR(m.Item,1,7),m.IClr,lcIssWare,m.Dyelot,;  
               -m.Issue_Qty,lnIssCost*Fabric.Conv,m.cOprCode,m.cLotNo,ldIssDate,;
               IIF(INLIST(laSetups[9,2],'L','F','I'),BomCost.cRSession,gfsequence('GLSession')),;
               IIF(INLIST(laSetups[9,2],'L','F','I'),BomCost.cISession,SPACE(6)))
  =lfIssRetFab(m.Typ,m.cCatgTyp,SUBSTR(m.Item,1,7),m.IClr,lcIssWare,m.Dyelot,;  
               -m.Issue_Qty,lnIssCost*Fabric.Conv,m.cOprCode,m.cLotNo,ldIssDate,;
               IIF(INLIST(laSetups[9,2],'L','F','I'),BomCost.cRSession,gfsequence('GLSession')),;
               IIF(INLIST(laSetups[9,2],'L','F','I'),BomCost.cISession,IIF(FABDYE.ONHAND<0,MATINVJL.CISESSION,SPACE(6))))
  *E301897,1 AMH [End]
  
  *E301121,4 (End)
ENDIF
m.Used_Qty = m.Used_Qty + IIF(llIssue,m.Issue_Qty,-m.Issue_Qty)

IF !llIssue .OR. laSetups[9,2] <> 'L'
  CLEAR READ
ENDIF
RELEASE WINDOW 'MFITMISS'

*!*************************************************************
*! Name      : lfvStyIss
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Accept issue/Return Styles
*!*************************************************************
*! Calls     : gfModalGen,CHECKPRD,lfIssRetSty,gpAdStyWar
*!*************************************************************
*! Parameters: llIssue   : .T.  Issue
*!                         .F.  Return
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvStyIss()
*!*************************************************************
FUNCTION lfvStyIss
PARAMETERS llIssue,llActCst
PRIVATE laIssQty

*B039660,1 NNA 06/06/2006 (Begin) convert next trigger to be done within Binmain.Prg instead of Davmain.prg 
*C123853,1 MHM 01/15/2005 check Bin Location [Start]
IF ASCAN(laEvntTrig,PADR("DLCHKBIN",10)) <> 0 
  IF gfDoTriger("POCSSH",PADR("DLCHKBIN",10))    
    RETURN
  ENDIF
ENDIF
*C123853,1 MHM 01/15/2005  [End]
*B039660,1 NNA (End)

IF llActCst
  DECLARE laIssQty[9]
  laIssQty[1] = m.Iss_Qty1
  laIssQty[2] = m.Iss_Qty2
  laIssQty[3] = m.Iss_Qty3
  laIssQty[4] = m.Iss_Qty4
  laIssQty[5] = m.Iss_Qty5
  laIssQty[6] = m.Iss_Qty6
  laIssQty[7] = m.Iss_Qty7
  laIssQty[8] = m.Iss_Qty8
  laIssQty[9] = m.Issue_Qty
*--- DOC.
*--- Function to issue/return syle by calling style control
*--- DOC.
  =lfIssRetSty(m.Typ,m.cCatgTyp,m.Item,StyInvJl.cWareCode,m.Dyelot,@laIssQty,;
               StyInvJl.nCost,m.cOprCode,StyInvJl.cLotNo,ldIssDate,lcSession,SPACE(6))
  laIssQty[1] = -m.Iss_Qty1
  laIssQty[2] = -m.Iss_Qty2
  laIssQty[3] = -m.Iss_Qty3
  laIssQty[4] = -m.Iss_Qty4
  laIssQty[5] = -m.Iss_Qty5
  laIssQty[6] = -m.Iss_Qty6
  laIssQty[7] = -m.Iss_Qty7
  laIssQty[8] = -m.Iss_Qty8
  laIssQty[9] = -m.Issue_Qty
  =lfIssRetSty(m.Typ,m.cCatgTyp,m.Item,StyInvJl.cWareCode,m.Dyelot,@laIssQty,;
               lnIssCost,m.cOprCode,StyInvJl.cLotNo,ldIssDate,SPACE(6),lcSession)
  CLEAR READ
  RETURN
ENDIF
IF laSetups[2,2]='Y' .AND. ;
  !SEEK (m.Item+lcIssWare+SPACE(10),'STYDYE')
  *E300725,1 Message : 38029
  *E300725,1 Style xxxxx is not assigned to warehouse xxxx
  *E300725,1 Button : 38001
  *E300725,1 Add Cancel
*--- DOC.
*--- Check if the current style is not assigned to the transactoin warehouse
*--- DOC.
  IF gfModalGen('QRM38029B38001','ALERT','Style: '+ALLTRIM(m.Item)+'|'+lcIssWare) = 1
    DO gpAdStyWar WITH m.Item,SPACE(10),lcIssWare
  ELSE
    RETURN
  ENDIF   
ENDIF
IF laSetups[8,2]='Y' AND SEEK(m.Item,'Style') AND Style.cDye_Flg='Y'
  lcDyelot = IIF(EMPTY(m.Dyelot),'?',m.Dyelot)
  IF !SEEK(m.Item+lcIssWare+lcDyelot,'STYDYE') AND ;
     !SDYEBROW(m.Item, @lcDyelot, .T., lcIssWare)
    RETURN
  ENDIF   
  m.Dyelot = lcDyelot
  SELECT (lcTktSheet)
  REPLACE Dyelot WITH m.Dyelot
  SELECT CTktBom
  =SEEK(m.cimtyp+m.cuttkt+m.typ+m.item+m.iclr+m.mfgcode)
  REPLACE Dyelot WITH m.Dyelot
ENDIF
IF !llIssue
  FOR lnCount = 1 TO 8
    lcCount = STR(lnCount,1)
    IF m.Iss_Qty&lcCount > m.Used_qty&lcCount
      *E300725,1 Message : 38057
      *E300725,1 Return quantity cannot exceed issued quantity
      *E300725,1 Button : 00000
      *E300725,1 Ok
      =gfModalGen('TRM38057B00000','ALERT')
      m.Iss_Qty&lcCount = m.Used_qty&lcCount
      _CUROBJ = OBJNUM(m.Iss_Qty&lcCount)
      RETURN
    ENDIF
  ENDFOR
ENDIF
IF !CHECKPRD(ldIssDate,'lcGlYear','lcGlPeriod','IA')
  _CUROBJ = OBJNUM(ldIssDate)
  RETURN
ENDIF
CLEAR READ
DECLARE laIssQty[9]
laIssQty[1] = IIF(llIssue,-m.Iss_Qty1,m.Iss_Qty1)
laIssQty[2] = IIF(llIssue,-m.Iss_Qty2,m.Iss_Qty2)
laIssQty[3] = IIF(llIssue,-m.Iss_Qty3,m.Iss_Qty3)
laIssQty[4] = IIF(llIssue,-m.Iss_Qty4,m.Iss_Qty4)
laIssQty[5] = IIF(llIssue,-m.Iss_Qty5,m.Iss_Qty5)
laIssQty[6] = IIF(llIssue,-m.Iss_Qty6,m.Iss_Qty6)
laIssQty[7] = IIF(llIssue,-m.Iss_Qty7,m.Iss_Qty7)
laIssQty[8] = IIF(llIssue,-m.Iss_Qty8,m.Iss_Qty8)
laIssQty[9] = IIF(llIssue,-m.Issue_Qty,m.Issue_Qty)

*B039660,1 NNA 06/06/2006 (Begin) convert next trigger to be done within Binmain.Prg instead of Davmain.prg 
*C123853,1 MHM 01/15/2005 check Bin Location [Start]
*=lfIssRetSty(m.Typ,m.cCatgTyp,m.Item,lcIssWare,m.Dyelot,@laIssQty,lnIssCost,;
             m.cOprCode,IIF(EMPTY(m.cOprCode),'',laLots[lnLotNo]),ldIssDate,;
             IIF(llIssue,SPACE(6),lcSession),IIF(llIssue,lcSession,SPACE(6)))
IF ASCAN(laEvntTrig,PADR("DVLDPOBN",10)) <> 0 
  =gfDoTriger("POCSSH",PADR("DVLDPOBN",10))
ELSE
  =lfIssRetSty(m.Typ,m.cCatgTyp,m.Item,lcIssWare,m.Dyelot,@laIssQty,lnIssCost,;
               m.cOprCode,IIF(EMPTY(m.cOprCode),'',laLots[lnLotNo]),ldIssDate,;
               IIF(llIssue,SPACE(6),lcSession),IIF(llIssue,lcSession,SPACE(6)))
ENDIF             
*C123853,1 MHM 01/15/2005  [End]
*B039660,1 NNA (End)

*B605380,1 AMH Fix -ev value of used quantity in issue log screen for style com. [Start]
*m.Used_Qty = m.Used_Qty + laIssQty[9]
m.Used_Qty1 = m.Used_Qty1 - laIssQty[1]
m.Used_Qty2 = m.Used_Qty2 - laIssQty[2]
m.Used_Qty3 = m.Used_Qty3 - laIssQty[3]
m.Used_Qty4 = m.Used_Qty4 - laIssQty[4]
m.Used_Qty5 = m.Used_Qty5 - laIssQty[5]
m.Used_Qty6 = m.Used_Qty6 - laIssQty[6]
m.Used_Qty7 = m.Used_Qty7 - laIssQty[7]
m.Used_Qty8 = m.Used_Qty8 - laIssQty[8]
m.Used_Qty = m.Used_Qty - laIssQty[9]
*B605380,1 AMH [End]

*!*************************************************************
*! Name      : lfvIssDye
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validated Issued Dyelot
*!*************************************************************
*! Calls     : SDYEBROW
*!*************************************************************
*! Parameters: lcType   'S' : Style
*!                      'F' : Fabric
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvIssDye('S')
*!*************************************************************
FUNCTION lfvIssDye
PARAMETERS lcType

lcDyelot  = m.Dyelot
IF lcType = 'S'
  IF !SEEK(m.Item+lcIssWare+lcDyelot,'STYDYE') AND ;
     !SDYEBROW(m.Item, @lcDyelot, .T., lcIssWare)
    RETURN
  ENDIF   
ELSE
  IF !SEEK(SUBSTR(m.Item,1,7)+m.IClr+lcIssWare+lcDyelot,'FABDYE') AND ;
     !FDYEBROW(SUBSTR(m.Item,1,7),m.IClr,@lcDyelot,.T.,lcIssWare)
    RETURN
  ENDIF   
ENDIF
m.Dyelot = lcDyelot

*!*************************************************************
*! Name      : lfvReturn
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Return Cost Item
*!*************************************************************
*! Calls     : MFITMISS.SPX,MFSTYISS.SPX,MFMFGISS.SPX,lfShowLog
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvReturn()
*!*************************************************************
FUNCTION lfvReturn
PRIVATE lcBrFields
ldIssDate = gdSysDate
*--- DOC.
*--- Valid function to return and issued cost item
*--- DOC.
DO CASE
  CASE INLIST(m.cCatgTyp,'F','T') .AND. SEEK(SUBSTR(m.Item,1,7)+m.IClr,'Fabric')
    *C200255,1 (Begin) Update MATINVJL with custom Voucher no for CON10.
    IF ASCAN(laEvntTrig,PADR("GETVOUT",10)) <> 0 AND TYPE('lcVoucNo') = 'U'
      lcVoucNo = ""
      llContVout = .T.
      = gfDoTriger("MAPOREC",PADR("GETVOUT",10))
      IF !llContVout
        RETURN
      ENDIF
      llFrstTime = .F.
    ENDIF  
    *C200255,1 (Begin)
    llItemDye = laSetups[12,2]='Y' .AND. Fabric.cDye_Flg = 'Y' 
    llOk = .F.
    SCATTER MEMVAR
    IF INLIST(laSetups[9,2],'L','F','I')
      SELECT BOMCOST
      SET ORDER TO TAG Bomcstkt
      
      IF SEEK(m.Typ+lcTranType+laData[1]+m.Item+m.IClr+m.MfgCode)

        *B037268,1 ABD Amendment to PO cost sheet 'issue/return' to allow return 
        *B037268,1 ABD Of trims to a different warehouse. [Begin]
        IF !(ASCAN(laEvntTrig,PADR("CALCRETN",10)) <> 0 ;
          .AND. gfDoTriger("POCSSH",PADR("CALCRETN",10)))
          *B037268,1 ABD - [End]
          
          lcticket   = m.Typ+lcTranType+laData[1]+m.Item+m.IClr+m.MfgCode
          lcBrFields = "cWareCode:H='Warehouse',dTranDate:H='Date',nTotQty:H='Quantity',nUnitCst:H='Cost',nTotCst:H='Amount',cApInvNo:H='Invoice'"
          *--- DOC.
          *--- Browse for the issued lots
          *--- DOC.
          *B037268,1 ABD - Don't browse all the issue qty. [Begin]
          *IF ARIABROW([lcticket],'Issued Lots',gnBrHSRow1, gnBrHSCol1,gnBrHSRow2,gnBrHSCol2,'','','CUTTKT','laBrowArr')
          *  lcIssWare   = BomCost.cWareCode
          IF ASCAN(laEvntTrig,PADR("CALCRETN",10)) <> 0 .OR. ;
            ARIABROW([lcticket],'Issued Lots',gnBrHSRow1, gnBrHSCol1,gnBrHSRow2,gnBrHSCol2,'','','CUTTKT','laBrowArr')
            *B037268,1 ABD - [End]
            lcIssWare   = BomCost.cWareCode
      
            *B607271,1 KHM 06/04/2003 (Begin) Use the ceiling function.
            *lnIssWare   = ASCAN(laMatWare,lcIssWare)/2
            lnIssWare   = CEILING(ASCAN(laMatWare,lcIssWare)/2)
            *B607271,1 KHM 06/04/2003 (End)
      
            lcWareStat  = 'DISABLE'
          
            *B037268,1 ABD Amendment to PO cost sheet 'issue/return' to allow return 
            *B037268,1 ABD Of trims to a different warehouse. [Begin]
            IF m.Typ = '3' .AND. ASCAN(laEvntTrig,PADR("ENABLWAR",10)) <> 0
              =gfDoTriger("POCSSH",PADR("ENABLWAR",10))
            ENDIF
            *B037268,1 ABD - [End]
          
            lnIssCost   = BomCost.nUnitCst
            STORE BomCost.nTotQty TO m.Issue_Qty,lnTotQty
            DO (gcScrDir+"MFITMISS.SPX") WITH .F.
          ENDIF
          
          *B037268,1 ABD Amendment to PO cost sheet 'issue/return' to allow return 
          *B037268,1 ABD Of trims to a different warehouse, End if For If Statment. [Begin]
        ENDIF
        *B037268,1 ABD - [End]
        
      ENDIF
    ELSE  
    *--- DOC.
    *--- If the cost method is average.
    *--- DOC.
      lcIssWare   = laData[32]

      *B607271,1 KHM 06/04/2003 (Begin) Use the ceiling function.
      *lnIssWare   = ASCAN(laMatWare,lcIssWare)/2
      lnIssWare   = CEILING(ASCAN(laMatWare,lcIssWare)/2)
      *B607271,1 KHM 06/04/2003 (End)

      lcWareStat  = 'ENABLE'
      =SEEK(SUBSTR(m.Item,1,7)+m.IClr+lcIssWare+SPACE(10),'FabDye')
      lnIssCost   = IIF(laSetups[9,2]='A',FabDye.nAveCstBuy,Fabric.CostBuy)/Fabric.Conv
      m.Issue_Qty = 0
      lnTotQty    = m.Used_Qty
      DO (gcScrDir+"MFITMISS.SPX") WITH .F.
    ENDIF
    IF llOk .AND. laSetups[11,2]='Y' .AND. Fabric.lTrkRolls
      =lfvMatRolls('R',m.cRSession,m.cISession,SUBSTR(m.Item,1,7),m.IClr,;
                    m.cWareCode,m.cDyelot,lnTotQty,m.Issue_Qty)
    ENDIF

    *B802396,1 SSH 11/04/00 [Begin] Get the onhand from FabDye.
    *lnOnHand   = FABRIC.OnHand
    lnOnHand   = IIF(EMPTY(cWareCode),FABRIC.OnHand,FABDYE.OnHand)
    *B802396,1 SSH 11/04/00  [End]
  CASE m.cCatgTyp='S' .AND. SEEK(m.Item,'Style') .AND. SEEK('S'+Style.Scale,'Scale')
    lcIssWare   = laData[31]
    
    *B607271,1 KHM 06/04/2003 (Begin) Use the ceiling function.
    *lnIssWare   = ASCAN(laStyWare,lcIssWare)/2
    lnIssWare   = CEILING(ASCAN(laStyWare,lcIssWare)/2)
    *B607271,1 KHM 06/04/2003 (End)

    =SEEK(m.Item+lcIssWare+space(10),'StyDye')
    lnIssCost   = IIF(laSetups[10,2]='A',StyDye.Ave_Cost,Style.TotCost)
    STORE 0 TO m.Iss_Qty1,m.Iss_Qty2,m.Iss_Qty3,m.Iss_Qty4,m.Iss_Qty5,;
               m.Iss_Qty6,m.Iss_Qty7,m.Iss_Qty8,m.Issue_Qty
    lcWareStat  = 'ENABLE'

    *B039660,1 NNA 06/06/2006 (Begin) convert next trigger to be done within Binmain.Prg instead of Davmain.prg 
    *C123853,1 MHM 01/15/2005  open custom screen for David Luke[Start]
    *DO (gcScrDir+"MFSTYISS.SPX") WITH .F.
    IF ASCAN(laEvntTrig,PADR("LDFNPORT",10)) <> 0 
      =gfDoTriger("POCSSH",PADR("LDFNPORT",10))
    ELSE
      DO (gcScrDir+"MFSTYISS.SPX") WITH .F.
    ENDIF
    *C123853,1 MHM 01/15/2005  [End]
    *B039660,1 NNA (End)
    
    *B802396,1 SSH 11/04/00  [Begin]
    *lnOnHand = Style.TotStk
    lnOnHand   = IIF(EMPTY(cWareCode),Style.TotStk,StyDye.TotStk)
    *B802396,1 SSH 11/04/00  [End]
  *B603861,1 AMH 09/12/2000 Prevent to Return the AP invoiced cost items [Start]
  CASE INLIST(m.cCatgTyp,'M','P','D')
    IF !('AP' $ gcCmpModules) .OR. ;
       (m.cCatGTyp = 'M' .AND. gfRltFld(m.MfgCode,@laMfgRFld,'MFGCODE') ;
       .AND. !EMPTY(lcMfgGlAcnt)) 
      lnIssCost   = m.UntCost
      m.Issue_Qty = 0
      DO (gcScrDir+"MFMFGISS.SPX") WITH .F.
    ELSE
      IF m.cCatGTyp $ 'PD'
        *E300725,1 Message : 38091
        *E300725,1 You cannot return the duty cost items since you are linked
        *E300725,1 with the AP. The duty cost application is done from the AP module.
        *E300725,1 Button : 00000
        *E300725,1 Ok
        =gfModalGen('TRM38091B00000','ALERT',IIF(m.cCatGTyp='D','return|duty|duty','return|purchase|purchase'))
      ELSE
        *E300725,1 Message : 38092
        *E300725,1 This MFG code has been setup to be applied from the A/P.
        *E300725,1 Button : 00000
        *E300725,1 Ok
        =gfModalGen('TRM38092B00000','ALERT')
      ENDIF          
    ENDIF
  *B603861,1 AMH 09/12/2000 [End] Prevent to Return the AP invoiced cost items
  OTHERWISE
    lnIssCost   = m.UntCost
    m.Issue_Qty = 0
    DO (gcScrDir+"MFMFGISS.SPX") WITH .F.
ENDCASE
=lfRefresh('MFISLOG0')
*B123774,1 MHM 10/12/2004 Refresh the screen with new temp[Start]
*=lfShowLog()
=lfBrowLog()
*B123774,1 MHM [End]

*!*************************************************************
*! Name      : lfvGenerate
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Generate ticket cost sheet
*!*************************************************************
*! Calls     : MFGENSHT.SPX,gfModalGen,gfSheetItem,lfwBrow,lfShOprHd,;
*!             lfShOprDt,lfShowDet
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvGenerate()
*!*************************************************************
FUNCTION lfvGenerate
PRIVATE lcItemFile,lcSizes,lnTranQty,lcItemType,;
        lnEst1,lnEst2,lnEst3,lnEst4,lnEst5,lnTotEst1,lnTotEst2,lnTotEst3,;
        lnTotEst4,lnTotEst5,llContinue,laQty
DECLARE laQty[9]

*B606030,1 AMH Restore the varible because its changed when open notepad screen [Start]
lcKey     = gcBmpHome + "ExtKey.BMP"
*B606030,1 AMH [End]

*E301077,55 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'FABDYE',gcDataDir+'FABDYE','SH')
=gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
*E301077,55 (End)

laData[29] = IIF(EMPTY(laData[29]),'DEFDEF',laData[29])

*607089,1 AMH Fill the laStyWare and laMatWare [Start]
STORE IIF(EMPTY(laData[31]),laData[11],laData[31]) TO laData[31]
STORE IIF(EMPTY(laData[32]),laData[11],laData[32]) TO laData[32]
=gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')

SELECT SUBSTR(cDesc,1,20),cWareCode FROM WAREHOUS WHERE lStyInv INTO ARRAY laStyWare
SELECT SUBSTR(cDesc,1,20),cWareCode FROM WAREHOUS WHERE lMatInv INTO ARRAY laMatWare

=gfCloseFile('WAREHOUS')
*607089,1 AMH [End]

*B605380,1 KHM 04/04/2002 (Begin) Adding the ceiling
*lnTkStWare = IIF(ASCAN(laStyWare,laData[31])=0,1,lnTkStWare)
*lnTkMtWare = IIF(ASCAN(laMatWare,laData[32])=0,1,lnTkMtWare)
lnTkStWare = IIF(ASCAN(laStyWare,laData[31])=0,1,CEILING(lnTkStWare))
lnTkMtWare = IIF(ASCAN(laMatWare,laData[32])=0,1,CEILING(lnTkMtWare))
*B605380,1 KHM 04/04/2002 (End)

*E301085,1 Generate cost sheet automatically
*IF laSetups[1,2]='Y' .OR. laSetups[2,2]='Y'
*khalid
*IF !llAutoGen AND (laSetups[1,2]='Y' .OR. laSetups[2,2]='Y')
IF !llAutoGen AND (laSetups[1,2]='Y' .OR. laSetups[2,2]='Y') AND (llStyCost OR llMatCost)
*khalid

*E301085,1 (End)
  llContinue = .F.
  DO (gcScrDir+"MFGENSHT.SPX")
  IF !llContinue
    RETURN
  ENDIF
ENDIF

laData[31] = laStyWare[lnTkStWare,2]
laData[32] = laMatWare[lnTkMtWare,2]
lcItemFile = IIF(lcTranType='T','Fabric','Style')
*C200080,1 AMM Consider the dye order case
*IF !SEEK(IIF(lcTranType='I','P','')+laData[1],lcTranFile)

*khm1*
*IF !SEEK(IIF(lcTranType='I',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1],lcTranFile)
IF !SEEK(IIF(lcTranType $ 'ID',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1],lcTranFile)
*khm1*

*C200080,1 AMM end
  *E300725,1 Message : 38065
  *E300725,1 Lines for this ticket not found. Cannot generate cost sheet 
  *E300725,1 Button : 00000
  *E300725,1 Ok
  *C200080,1 AMM Consider the dye order case
  *=gfModalGen('TRM38065B00000','ALERT',IIF(lcTranType='M','cutting ticket',IIF(lcTranType='I','purchase order','order')))
  
  *khm1*
  *=gfModalGen('TRM38065B00000','ALERT',IIF(lcTranType='M','cutting ticket',IIF(lcTranType='I',IIF(EMPTY(lcDyePO),'purchase order','Dye Order'),'order')))
  =gfModalGen('TRM38065B00000','ALERT',IIF(lcTranType='M','cutting ticket',IIF(lcTranType $ 'ID',IIF(EMPTY(lcDyePO),'purchase order','Dye Order'),'order')))  
  *khm1*
  
  *C200080,1 AMM end
  RETURN
ENDIF
IF INLIST(lcTranType,'M','T')
  IF !SEEK(laData[2],'BOM')
    *E300725,1 Message : 38031
    *E300725,1 Cost sheet not found for style: xxx cannot generate cutting 
    *E300725,1 ticket cost sheet
    *E300725,1 Button : 00000
    *E300725,1 Ok
    =gfModalGen('TRM38031B00000','ALERT',IIF(lcTranType='M','style: ','material: ')+ALLTRIM(laData[2])+'|'+IIF(lcTranType='M','cutting ticket','order'))
    RETURN
  ENDIF
ELSE
  
  *B607421,1 AMH Define variable to ckeck if use multi currency or not [Start]
  llMulCurr = gfGetMemVar('llMulCurr')
  *B607421,1 AMH [End]
  
  SELECT PosLn
  *C200080,1 AMM Consider the dye order case
  *SCAN REST WHILE cStyType+PO = 'P'+laData[1] FOR TranCd = '1'
  SCAN REST WHILE cStyType+PO = IIF(EMPTY(lcDyePO),'P','D')+laData[1] FOR TranCd = '1'
  *C200080,1 AMM end
    *C037960,1 MHM check order cost sheet for Alena [Begin]
    IF ASCAN(laEvntTrig , PADR('POCHKORD',10)) <> 0
      IF !gfDoTriger('POCSSH',PADR('POCHKORD',10)) 
        RETURN
      ENDIF
    ELSE
      *C037960,1  [End]
      IF !SEEK(SUBSTR(Style,1,LEN(lcMjrMsk)),'BOM')
        *E300725,1 Message : 38031
        *E300725,1 Cost sheet not found for style: xxx cannot purchase order cost sheet
        *E300725,1 Button : 00000
        *E300725,1 Ok
        *C200080,1 AMM Consider the dye order case
        *=gfModalGen('TRM38031B00000','ALERT','style: '+ALLTRIM(SUBSTR(Style,1,LEN(lcMjrMsk)))+'|'+'purchase order')
        =gfModalGen('TRM38031B00000','ALERT','style: '+ALLTRIM(SUBSTR(Style,1,LEN(lcMjrMsk)))+'|'+IIF(EMPTY(lcDyePO),'purchase order','Dye Order'))
        *C200080,1 AMM end
        RETURN
      ENDIF
    *C037960,1  [End]
    ENDIF  
    *C037960,1  [End]
    *B607421,1 AMH Update Posln and PosHdr with style cost in case of all cost elements 
    *B607421,1     in the posln is zero [Start]
    IF NCOST1+NCOST2+NCOST3+NCOST4+NCOST5 = 0 .AND. EMPTY(lcDyePO)
      =SEEK(POSLN.STYLE,'STYLE')
      REPLACE NCOST1 WITH IIF(!llMulCurr .OR. STYLE.CPRICECUR = POSHDR.CPRICECUR,STYLE.NICOST1,0),;
              NCOST2 WITH IIF(!llMulCurr .OR. STYLE.CDUTYCUR  = POSHDR.CDUTYCUR ,STYLE.NICOST2,0),;
              NCOST3 WITH IIF(!llMulCurr .OR. STYLE.CDUTYCUR  = POSHDR.CDUTYCUR ,STYLE.NICOST3,0),;
              NCOST4 WITH IIF(!llMulCurr .OR. STYLE.CDUTYCUR  = POSHDR.CDUTYCUR ,STYLE.NICOST4,0),;
              NCOST5 WITH IIF(!llMulCurr .OR. STYLE.CDUTYCUR  = POSHDR.CDUTYCUR ,STYLE.NICOST5,0)
    ENDIF
    SELECT POSHDR
    REPLACE NICOST1 WITH NICOST1 + POSLN.NCOST1,;
            NICOST2 WITH NICOST2 + POSLN.NCOST2,;
            NICOST3 WITH NICOST3 + POSLN.NCOST3,;
            NICOST4 WITH NICOST4 + POSLN.NCOST4,;
            NICOST5 WITH NICOST5 + POSLN.NCOST5
    REPLACE POTOTAL WITH NICOST1+NICOST2+NICOST3+NICOST4+NICOST5
    SELECT POSLN
    *B607421,1 AMH [End]
  ENDSCAN
  *C200080,1 AMM Consider the dye order case
  *=SEEK('P'+laData[1])
  =SEEK(IIF(EMPTY(lcDyePO),'P','D')+laData[1])
  *C200080,1 AMM end
ENDIF  
WAIT WINDOW 'Building new cost sheet...' NOWAIT
SELECT (lcTmpTkt)
ZAP
STORE 0 TO lnEst1,lnEst2,lnEst3,lnEst4,lnEst5,lnLastSeq,;
           lnTotEst1,lnTotEst2,lnTotEst3,lnTotEst4,lnTotEst5
lcLastOpr = ''
llContinue = .T.

SELECT (lcTranFile)
*C200080,1 AMM Consider the dye order case
*SCAN REST WHILE EVAL(lcFileKey)=IIF(lcTranType='I','P','')+laData[1] FOR TranCd='1'

*khm1*
*SCAN REST WHILE EVAL(lcFileKey)=IIF(lcTranType='I',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1] FOR TranCd='1'
SCAN REST WHILE EVAL(lcFileKey)=IIF(lcTranType $ 'ID',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1] FOR TranCd='1'
*khm1*

*C200080,1 AMM end
  lcItem   = IIF(lcTranType='T',cFabric,Style)
  lcColor  = IIF(lcTranType='T',Color,'')
  lcDyelot = Dyelot
  llDyelot = SEEK(lcItem+lcColor,lcItemFile) .AND. &lcItemFile..cDye_Flg='Y'
  lcScale  = IIF(lcTranType='T','',Style.Scale)
  STORE 0 TO laQty
  IF lcTranType <> 'T'
    laQty[1] = &lcTranFile..Qty1
    laQty[2] = &lcTranFile..Qty2
    laQty[3] = &lcTranFile..Qty3
    laQty[4] = &lcTranFile..Qty4
    laQty[5] = &lcTranFile..Qty5
    laQty[6] = &lcTranFile..Qty6
    laQty[7] = &lcTranFile..Qty7
    laQty[8] = &lcTranFile..Qty8
  ENDIF
  laQty[9] = IIF(lcTranType='T',&lcTranFile..nMfgTotQty,&lcTranFile..TotQty)
  SELECT (lcTmpTkt)
  APPEND BLANK
  REPLACE Item      WITH lcItem  ,;
          Color     WITH lcColor ,;
          cDyelot   WITH lcDyelot,;
          LineNo    WITH &lcTranFile..LineNo ,;
          cWareCode WITH &lcTranFile..cWareCode ,;
          nQty1     WITH laQty[1] ,;
          nQty2     WITH laQty[2] ,;
          nQty3     WITH laQty[3] ,;
          nQty4     WITH laQty[4] ,;
          nQty5     WITH laQty[5] ,;
          nQty6     WITH laQty[6] ,;
          nQty7     WITH laQty[7] ,;
          nQty8     WITH laQty[8] ,;
          nTotQty   WITH laQty[9] ,;
          nRecNo    WITH RECNO(lcTranFile)
  *B602806,1 MAN Save the temp BOMLINE records count
  lnRecDetNo = RECCOUNT(lcDetFile) 
  
  *wam
*  llContinue = ;
*  gfSheetItem(lcTranType,laData[1],laData[29],lcItem,lcColor,;
*              IIF(lcTranType='T',0,&lcTranFile..LineNo),lcDyelot,laData[31],;
*              laData[32],@laQty,'BOM',lcTktSheet,lcDetFile,lcOprHdr,;
*              @lcLastOpr,IIF(lcTranType='I',PosLn.nCost1,0),;
*              @lnEst1,@lnEst2,@lnEst3,@lnEst4,@lnEst5,lcDetFile)
  *C200080,1 AMM Consider the dye order case in calling the function
  *llContinue = ;
  gfSheetItem(lcTranType,laData[1],laData[29],lcItem,lcColor,;
              &lcTranFile..LineNo,lcDyelot,laData[31],;
              laData[32],@laQty,'BOM',lcTktSheet,lcDetFile,lcOprHdr,;
              @lcLastOpr,IIF(lcTranType='I',PosLn.nCost1,0),;
              @lnEst1,@lnEst2,@lnEst3,@lnEst4,@lnEst5,lcDetFile)
*--- DOC.
*--- Global function to create cost sheet and update BOMLINE,Operatoin header
*--- DOC.

  *C037960,1 MHM Update OrdBom file  For  Alena [Begin]
  IF ASCAN(laEvntTrig , PADR('POUPDBOM',10)) <> 0
    =gfDoTriger('POCSSH',PADR('POUPDBOM',10)) 
  ELSE  

    llContinue = ;
    gfSheetItem(IIF(lcDyePO='D','D',lcTranType),laData[1],laData[29],lcItem,lcColor,;
                &lcTranFile..LineNo,lcDyelot,laData[31],;
                laData[32],@laQty,'BOM',lcTktSheet,lcDetFile,lcOprHdr,;
                @lcLastOpr,IIF(lcTranType='I',PosLn.nCost1,0),;
                @lnEst1,@lnEst2,@lnEst3,@lnEst4,@lnEst5,lcDetFile)
  ENDIF  
  *C037960,1  [End]

  *B602806,1 MAN Check if no records have been added to the cost sheet (Start)
  *IF !llContinue  
  IF !llContinue  OR lnRecDetNo = RECCOUNT(lcDetFile) 
    IF lnRecDetNo = RECCOUNT(lcDetFile) 
      *C200080,1 AMM Consider the dye order case
      *=gfModalGen('TRM38031B00000','ALERT','style: '+ALLTRIM(lcItem)+'|'+IIF(lcTranType='M','cutting ticket',IIF(lcTranType='I','purchase order','order')))    
      *E301235,4 WAB - correct text message in case of material
      *E301235,4 WAB - START
      *=gfModalGen('TRM38031B00000','ALERT','style: '+ALLTRIM(lcItem)+'|'+IIF(lcTranType='M','cutting ticket',IIF(lcTranType='I', IIF(EMPTY(lcDyePO),'purchase order','Dye Order')  ,'order')))    
      
      *khm1*
      *=gfModalGen('TRM38031B00000','ALERT',IIF(lcTranType='T','material: ','style: ')+;
            ALLTRIM(lcItem)+'|'+IIF(lcTranType='M','cutting ticket',;
            IIF(lcTranType='I', IIF(EMPTY(lcDyePO),'purchase order','Dye Order')  ,'order')))    
      =gfModalGen('TRM38031B00000','ALERT',IIF(lcTranType='T','material: ','style: ')+;
            ALLTRIM(lcItem)+'|'+IIF(lcTranType='M','cutting ticket',;
            IIF(lcTranType $ 'ID', IIF(EMPTY(lcDyePO),'purchase order','Dye Order')  ,'order')))    
      *khm1*

      *E301235,4 WAB - END
      *C200080,1 AMM end
      llContinue = .F.
    ENDIF
  *B602806,1 MAN (End)
    
    SELECT (lcTktSheet)
    DELETE ALL
    SELECT (lcDetFile)
    DELETE ALL
    SELECT (lcOprHdr)
    DELETE ALL
    EXIT
  ENDIF
  lnTotEst1 = lnTotEst1 + lnEst1
  lnTotEst2 = lnTotEst2 + lnEst2
  lnTotEst3 = lnTotEst3 + lnEst3
  lnTotEst4 = lnTotEst4 + lnEst4
  lnTotEst5 = lnTotEst5 + lnEst5
ENDSCAN
*E301427,1 SSH Only if PW module installed and Call this program to Generate
*E301427,1 SSH C/T Cost Sheet.
*B604056,1 HBG 12/06/2000 Fix bug of variabel 'lnRecDetNo' not found [Start]
*IF llContinue  .AND. lnRecDetNo <> RECCOUNT(lcDetFile) 
IF llContinue  
*B604056,1 [End]
  llDummy = llPWInst .AND. (lcTranType = "M") .AND. lfPwShtItm('A')
ENDIF
*E301427,1 SSH  [END]

GO TOP IN (lcTktSheet)
GO TOP IN (lcDetFile)
GO TOP IN (lcOprHdr)
WAIT CLEAR
SELECT(lcTktSheet)
IF EOF()
  IF llContinue
    *E300725,1 Message : 38066
    *E300725,1 Cost sheet information not found for ticket lines
    *E300725,1 Button : 00000
    *E300725,1 Ok
    *C200080,1 AMM Consider the dye order case
    *=gfModalGen('TRM38066B00000','ALERT',IIF(lcTranType='M','cutting ticket',IIF(lcTranType='I','purchase order','order')))
    
    *khm1*
    *=gfModalGen('TRM38066B00000','ALERT',IIF(lcTranType='M','cutting ticket',IIF(lcTranType='I', IIF(EMPTY(lcDyePO),'purchase order','dye order') ,'order')))
    =gfModalGen('TRM38066B00000','ALERT',IIF(lcTranType='M','cutting ticket',IIF(lcTranType $ 'ID', IIF(EMPTY(lcDyePO),'purchase order','dye order') ,'order')))
    *khm1*
    
    *C200080,1 AMM end
  ENDIF
ELSE
  SELECT (lcTktSheet)
  SET ORDER TO TAG (lcTktSheet)
  FOR lnType = 1 TO 5
    DO WHILE SEEK(STR(lnType,1)+SPACE(1))
      REPLACE cShowType WITH '1'
*--- DOC.
*--- Call functoin "lfLblDesc" to get the cost element label
*--- DOC.
      IF !SEEK(STR(lnType,1)+'0')
        INSERT INTO (lcTktSheet) (TYP,cShowType,Item) VALUES ;
        (STR(lnType,1),'0',lfLblDesc(laSetups[2+lnType,2]))
      ENDIF
    ENDDO
  ENDFOR
  GO TOP
  SELECT *,RECNO() AS nRecNo FROM (lcDetFile) INTO CURSOR tmpBomLine
  SET ORDER TO TAG (lcDetFile) IN (lcDetFile)
  SELECT tmpBomLine
  SCAN
    lcOprCode = IIF(EMPTY(cOprCode),MfgCode,cOprCode)
    IF !EMPTY(lcOprCode) AND SEEK(lcTranType+laData[1]+lcOprCode,lcOprHdr) ;
       AND SEEK(Style+SClr+STR(LineNo,6),lcTmpTkt)
       SELECT (lcTmpTkt)
       lcFrstOpr   = IIF(nFrstOprSq=0 OR (VAL(&lcOprHdr..cOperSeq) < nFrstOprSq),&lcOprHdr..cOprCode,cFrstOpr)
       lnFrstOprSq = IIF(nFrstOprSq=0 OR (VAL(&lcOprHdr..cOperSeq) < nFrstOprSq),VAL(&lcOprHdr..cOperSeq),nFrstOprSq)
       REPLACE cFrstOpr   WITH lcFrstOpr ,;
               nFrstOprSq WITH lnFrstOprSq
    ENDIF
    SELECT tmpBomLine
    IF !SEEK(Style+SClr+STR(LineNo,6)+cBomTyp+'0',lcDetFile)
      SELECT (lcDetFile)
      =SEEK(tmpBomLine.Style+tmpBomLine.SClr+STR(tmpBomLine.LineNo,6))
      LOCATE REST WHILE Style+SClr+STR(LineNo,6) =;
      tmpBomLine.Style+tmpBomLine.SClr+STR(tmpBomLine.LineNo,6) FOR !EMPTY(cShowType)
      llHideTit = FOUND()
      INSERT INTO (lcDetFile) ;
      (Style,SClr,LineNo,cBomTyp,cShowType,Item,lHideTit) VALUES ;
      (tmpBomLine.Style,tmpBomLine.SClr,tmpBomLine.LineNo,tmpBomLine.cBomTyp,'0',;
       lfLblDesc(laSetups[2+VAL(tmpBomLine.cBomTyp),2]),llHideTit)
    ENDIF
    SELECT (lcDetFile)
    GOTO tmpBomLine.nRecNo
    REPLACE cShowType WITH '1'
    IF !SEEK(Style+SClr+STR(LineNo,6)+cBomTyp+'2',lcDetFile)
      INSERT INTO (lcDetFile) ;
      (Style,SClr,LineNo,cBomTyp,cShowType,Item) VALUES ;
      (tmpBomLine.Style,tmpBomLine.SClr,tmpBomLine.LineNo,tmpBomLine.cBomTyp,'2','***** Total :')
    ENDIF
    SELECT (lcDetFile)
    REPLACE ItemQty  WITH ItemQty + tmpBomLine.ItemQty,;
            ItemAmt  WITH ItemAmt + tmpBomLine.ItemAmt
  ENDSCAN
  USE IN tmpBomLine
  GO TOP IN (lcDetFile)
  SET ORDER TO TAG (lcOprHdr) IN (lcOprHdr)
  GO TOP IN (lcOprHdr)
  *MAN Added the Following IF Condition to prevent Updating the price in 
  *    the Automatic Mode.
  *C200080,1 AMM Consider the dye order case
  *IF !(lcTranType = 'I' AND llAutoGen)
  *khm1*
  *IF !(lcTranType = 'I' AND EMPTY(lcDyePO) AND llAutoGen)
  IF !(lcTranType $ 'ID' AND EMPTY(lcDyePO) AND llAutoGen)
  *khm1*
  
  *C200080,1 AMM end
    laData[12] = lnTotEst1
  ENDIF  
  laData[13] = lnTotEst2
  laData[14] = lnTotEst3
  laData[15] = lnTotEst4
  laData[16] = lnTotEst5
  laData[30] = lcLastOpr
  lcFirstOpr = &lcOprHdr..cOprCode

  *B604294,1 KHM 04/30/2001 (Begin) Calling lfRecCost to recalculate
  *B604294,1                the duty when its a percentage. When generating
  *B604294,1                the PO cost element.
  IF lcTranType = 'I'
    =lfRecCost(lcTranType,laData[1],IIF(laData[3]<>'H','BOMLINE',lcDetFile),IIF(laData[3]<>'H','CTKTBOM',lcTktSheet))
  ENDIF
  *B604294,1 KHM 04/30/2001 (End)

  *E301085,1 Generate cost sheet automatically
  IF !llAutoGen
    STORE .F. TO laScrMode
    laScrMode[4]=.T.
    llCUpdate = .T.
    SHOW GETS
    DO CASE
      CASE lnActFolder = 1
        =lfwBrow()
      CASE lnActFolder = 2
        =lfShOprHd()
        =lfShOprDt()
      CASE lnActFolder = 3
        =lfShowDet()
    ENDCASE
  ENDIF
  *E301085,1 (End)
ENDIF

*!*************************************************************
*! Name      : lfvCloseXX
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Close Cost Sheet
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvClose()
*!*************************************************************
*! B605481,1 SSH/KHM 02/17/2002 Commented out in order to re-write it
*!*************************************************************
FUNCTION lfvCloseXX
PRIVATE lnTActCst,lnTEstCst,lnITActCst,lnITEstCst,lcItem,lnRecNo

*E301077,55 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'BOMCOST',gcDataDir+'BOMCOST','SH')
=gfOpenFile(gcDataDir+'MATINVJL',gcDataDir+'MATINVJL','SH')
*E301077,55 (End)
        
*E300725,1 Check if some issued purchase materials have not been invoices yet
SET ORDER TO TAG POBOMCLS IN BOMCOST
SET ORDER TO TAG Ctktbom  IN CTKTBOM
SET ORDER TO TAG BOMLINE  IN BOMLINE
SET ORDER TO TAG MatInvJl IN MatInvJl

DO CASE
  CASE lcTranType='M'
    lcClsTitle = 'Close Cutting Ticket# '+laData[1]
    lcMessage  = 'cutting ticket'
    SET ORDER TO TAG CUTREC IN CUTTKTL
  CASE lcTranType='I'
    *C200080,1 AMM (start)
    *lcClsTitle  = 'Close Style Purchase Order# '+laData[1]
    *lcMessage  = 'purchase order'
    IF EMPTY(lcDyePO)
      lcClsTitle  = 'Close Style Purchase Order# '+laData[1]
      lcMessage  = 'purchase order'
    ELSE
      lcClsTitle  = 'Close Dye Order# '+laData[1]
      lcMessage  = 'dye order'
    ENDIF
    *C200080,1 AMM end
    SET ORDER TO TAG POSLN IN POSLN
  CASE lcTranType='T'
    *E#301235,1
    *lcClsTitle = 'Close Material Order# '+laData[1]
    lcClsTitle = 'Close Material Manufacturing Order# '+laData[1]
    lcMessage  = 'material order'
ENDCASE

*--- Doc. [Start]
*--- laSetups[9,2] = Matrial Costing Method.
*--- Check If Costing method Is 
*--- Lot|LIFO|FIFO ~ L|I|F.
*--- Doc. [End]

*B603659,1 SSH [Start] comment the following check that the system does when
*B603659,1 SSH         closing cutting ticket that there are AP invoices issued
*B603659,1 SSH         for all the fab.&trims included on the cost sheet
*B603659,1 SSH         incase of FIFO|LIFO|LOT costing Method.
*IF INLIST(laSetups[9,2],'L','F','I')
*  SELECT cTktBom
*  =SEEK(lcTranType+laData[1])
*  SCAN REST WHILE cImTyp+CutTkt+Typ+Item+IClr+MfgCode+Dyelot=;
*                  lcTranType+laData[1] FOR INLIST(cCatgTyp,'F','T')
*    IF SEEK(SUBSTR(cTktBom.Item,1,7)+cTktBom.IClr,'Fabric') .AND. !Fabric.Make ;
*       .AND. SEEK(lcTranType+laData[1]+'Y'+cTktBom.Typ+cTktBom.Item+;
*                  cTktBom.IClr+cTktBom.MfgCode,'BOMCOST')
*      SELECT BOMCOST
*      SCAN REST WHILE cimtyp+ctktno+actualize+cbomtype+item+iclr+mfgcode+;
*                      coprcode+clotno+cisession+crsession = ;
*                      lcTranType+laData[1]+'Y'+cTktBom.Typ+cTktBom.Item+;
*                      cTktBom.IClr+cTktBom.MfgCode ;
*                FOR   EMPTY(cApInvNo)
*        SELECT MatInvJl
*        =SEEK(SUBSTR(BomCost.Item,1,7)+BomCost.IClr+BomCost.cWareCode+;
*              BomCost.cDyelot+BomCost.cRSession+SPACE(6))
*        LOCATE REST WHILE ;
*        cfabric+ccolor+cwarecode+cdyelot+crsession+cisession+STR(RECNO(),7)=;
*        SUBSTR(BomCost.Item,1,7)+BomCost.IClr+BomCost.cWareCode+;
*        BomCost.cDyelot+BomCost.cRSession+SPACE(6) ;
*        FOR cTranType='1'
*
*        IF FOUND()
*          *E300725,1 Message : 38032
*          *E300725,1 Some issued items have not been invoiced. Cannot close.
*          *E300725,1 Button : 00000
*          *E300725,1 Ok
*          =gfModalGen('TRM38032B00000','ALERT')
*          RETURN
*        ENDIF
*      ENDSCAN  
*    ENDIF
*  ENDSCAN
*ENDIF  
*B603659,1 SSH [End]
llClose = .F.
ldClsDate = gdSysDate
DO (gcScrDir+"MFTKTCLS.SPX")
IF !llClose
  =IIF(lnActFolder=1,lfwBrow(),.T.)
  RETURN
ENDIF
IF !CHECKPRD(ldClsDate,'lcGlYear','lcGlPeriod','  ')
  =IIF(lnActFolder=1,lfwBrow(),.T.)
  RETURN
ENDIF

WAIT WINDOW 'Closing ticket '+laData[1]+'.' NOWAIT
*E#301235,1 Added IF Condition to open the stydye
IF lcTranType <> "T"
*E301077,55 Inhance openning files to speed up transaction
  =gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
ENDIF
=gfOpenFile(gcDataDir+'FABDYE',gcDataDir+'FABDYE','SH')
*E301077,55 (End)


*E300725,1 Initialize total actual and estimated cost
STORE 0 TO lnTActCst,lnTEstCst

*--HDM B602148,1 11/03/1998[START]
*--Here we should calculate the WIP before deleting the records from BOMLINE
SELECT BOMCOST
=SEEK(lcTranType+laData[1])
SCAN REST WHIlE cimtyp+ctktno+actualize+cbomtype+item+iclr+mfgcode+;
                coprcode+clotno+cisession+crsession=lcTranType+laData[1] ;
          FOR   INLIST(CCOSTTYPE,'F','T')
  SELECT FABRIC
  =SEEK(PADR(BOMCOST.ITEM,7) + PADR(BOMCOST.ICLR,6))
  REPLACE NMATWIP WITH NMATWIP - BOMCOST.NTOTQTY
    
  SELECT FABDYE
  =SEEK(PADR(BOMCOST.ITEM,7) + PADR(BOMCOST.ICLR,6) + BOMCOST.CWARECODE+SPACE(10))
  REPLACE NMATWIP WITH NMATWIP - BOMCOST.NTOTQTY
    
  SELECT FABDYE
  =SEEK(PADR(BOMCOST.ITEM,7) + PADR(BOMCOST.ICLR,6) + BOMCOST.CWARECODE+BOMCOST.cDyelot)
  REPLACE NMATWIP WITH NMATWIP - BOMCOST.NTOTQTY

ENDSCAN
*--HDM B602148,1  11/03/1998[END]

*E300725,1 Delete non received record from BomLine file
SELECT BOMLINE 
=SEEK(lcTranType+'2'+laData[1])
DELETE REST WHILE ;
cIMTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+Item+IClr+Mfgcode = ;
lcTranType+'2'+laData[1] FOR EMPTY(cRSession)

*E300725,1 Update Actual unit cost in the details file.
SELECT BOMCOST
IF SEEK(lcTranType+laData[1]+'Y')
  DO WHILE cimtyp+ctktno+actualize+cbomtype+item+iclr+mfgcode+coprcode+;
           clotno+cisession+crsession=lcTranType+laData[1]+'Y' .AND. !EOF()

    STORE 0 TO lnItActCst,lnItActQty,lnItEstCst,lnItEstQty

    lcItem  = cBomType+Item+IClr+MfgCode
    *E300725,1 Accumulate Item total actual cost and total actual quantity
*--- DOC.
*--- Sum to get the total cost and total qty for the actualizes
*--- DOC.
    SUM REST nTotCst,nTotQty TO lnItActCst, lnItActQTy ;
        WHILE cimtyp+ctktno+actualize+cbomtype+item+iclr+mfgcode+coprcode+;
           clotno+cisession+crsession=lcTranType+laData[1]+'Y'+lcItem
    SELECT BOMLINE 
    =SEEK(lcTranType+'2'+laData[1])
    *E300725,1 Accumulate Item total estimated cost and total estimated quantity
*--- DOC.
*--- Sum teh estimated cost and estimated qty for revieved line
*--- DOC.
    SUM REST  ItemAmt,ItemQty TO lnItEstCst, lnItEstQty  ;
        WHILE cIMTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+Item+IClr+MfgCode =;
              lcTranType+'2'+laData[1] ;
        FOR   cBomTyp+Item+IClr+MfgCode = lcItem

    *E300725,1 Accumulate total actual cost and total estimated cost
    lnTActCst = lnTActCst + lnItActCst
    lnTEstCst = lnTEstCst + lnItEstCst

    *E300725,1 Create Actual cost records type '3'
    =SEEK(lcTranType+'2'+laData[1])
    SCAN REST WHILE cIMTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+Item+IClr+MfgCode = ;
                    lcTranType+'2'+laData[1] ;
              FOR   cBomTyp+Item+IClr+MfgCode = lcItem
      lnItemAmnt = ITEMAMT
      lnItemQty  = ItemQty
      SCATTER MEMVAR
      m.Ctype    = '3'
      *E300725,1 Contribute item actual cost and actual quantity
      m.ItemAmt  = IIF(lnItEstCst=0,0,lnItemAmnt/lnItEstCst*lnItActCst)
      m.ItemQty  = IIF(lnITEstQty=0,0,lnItemQty/lnITEstQty*lnITActQty)
      m.UnitCost = IIF(m.ItemQty=0,0,;
                   IIF(lnItEstCst=0,0,lnItemAmnt/lnItEstCst*lnItActCst)/m.ItemQty)
      m.UnitQty  = IIF(StyQty=0,0,m.ItemQty/StyQty)
      DO CASE
        *C200080,1 AMM Consider the dye order case
        *CASE lcTranType='I'
        CASE lcTranType='I' .AND. EMPTY(lcDyePO)
        *C200080,1 AMM end
          *E300725,1 Update actual cost for Style Purchase order lines
          IF SEEK('P'+laData[1]+m.Style+STR(m.LineNo,6),'POSLN')
             SELECT POSLN
             COUNT REST  TO lnNoOfRec ;
                   WHILE cSTyType+Po+Style+STR(LineNo,6)+TranCd+STR(RECNO(),7)=;
                         'P'+laData[1]+m.Style+STR(m.LineNo,6) ;
                   FOR   cRSession = m.cRSession
            =SEEK('P'+laData[1]+m.Style+STR(m.LineNo,6))
            REPLACE REST  ('NEACTCOST'+m.cBomTyp) WITH  ;
                          EVAL('NEACTCOST'+m.cBomTyp) + ;
                          m.UnitCost * m.UnitQty / lnNoOfRec;
                    WHILE cSTyType+Po+Style+STR(LineNo,6)+TranCd+STR(RECNO(),7)=;
                          'P'+laData[1]+m.Style+STR(m.LineNo,6) ;
                    FOR   cRSession = m.cRSession
          ENDIF
        *C200080,1 AMM (start) dye order case
        CASE lcTranType='I' .AND. !EMPTY(lcDyePO)
          *E300725,1 Update actual cost for Style Purchase order lines
          IF SEEK('D'+laData[1]+m.Style+STR(m.LineNo,6),'POSLN')
             SELECT POSLN
             COUNT REST  TO lnNoOfRec ;
                   WHILE cSTyType+Po+Style+STR(LineNo,6)+TranCd+STR(RECNO(),7)=;
                         'D'+laData[1]+m.Style+STR(m.LineNo,6) ;
                   FOR   cRSession = m.cRSession
            =SEEK('D'+laData[1]+m.Style+STR(m.LineNo,6))
            REPLACE REST  ('NEACTCOST'+m.cBomTyp) WITH  ;
                          EVAL('NEACTCOST'+m.cBomTyp) + ;
                          m.UnitCost * m.UnitQty / lnNoOfRec;
                    WHILE cSTyType+Po+Style+STR(LineNo,6)+TranCd+STR(RECNO(),7)=;
                          'D'+laData[1]+m.Style+STR(m.LineNo,6) ;
                    FOR   cRSession = m.cRSession
          ENDIF
        *C200080,1 AMM end

        CASE lcTranType='M'
          *E300725,1 Update actual cost for Cutting Ticket lines
          *B602462,1 Fix alias reference
          *SELECT (CutTktL)
          SELECT CutTktL
          *B602462,1 Fix alias reference
                    
          IF SEEK(m.cRSession+laData[1]+m.Style)
            REPLACE REST ('NACT_CST'+m.cBomTyp) WITH     ;
                         EVAL('NACT_CST'+m.cBomTyp)+m.UnitCost * m.UnitQty ;
                   WHILE cRSession+CutTkt+Style+TranCd =  ;
                         m.cRSession+laData[1]+m.Style
          ENDIF
          
      
        *E301235,4 WAB - filed name is lcTranType not lcMIFlg 
        *E301235,4 WAB - START
        *CASE lcMIFlg='T'
        CASE lcTranType='T'
          *E300725,1 Update actual cost for Fabric order lines
          *B603382,1 WAB - file name is MMFGORDD not MMFGORDDT
          *SELECT MMFGORDDT
          SELECT MMFGORDD
          *B603382,1 WAB - END
          IF SEEK(laData[1]+SUBSTR(m.Style,1,7)+m.SClr)
            REPLACE REST ('NACT_COST'+m.cBomTyp) WITH      ;
                          EVAL('NACT_COST'+m.cBomTyp)+m.UnitCost*m.UnitQty ;
                    WHILE cMfgOrdNo+cFabric+Color+Dyelot+TranCd = ;
                          laData[1]+SUBSTR(m.Style,1,7)+m.SClr ;
                    FOR   cRSession = m.cRSession
          ENDIF
      ENDCASE
      lnRecNo = RECNO('BOMLINE')
      INSERT INTO BOMLINE FROM MEMVAR
      GO lnRecNo IN BOMLINE
    ENDSCAN
    SELECT BOMCOST
  ENDDO
ENDIF
*E300725,1 Create Cancelled records in the ticket detailed file with open
*E300725,1 quantities and update Work In Process in the Style file
DO CASE
  CASE lcTranType='M'
    
    lcFile = 'CUTTKTL'
    *E#301235,1 Removed STR(RECNO(),7) to match index expr
    *SELECT * FROM CUTTKTL WHERE CutTkt+Style+Dyelot+TranCd+STR(RECNO(),7)=laData[1] ;
    INTO CURSOR (lcisslogfile) ORDER BY Style,cWareCode,Dyelot
    SELECT * FROM CUTTKTL WHERE CutTkt+Style+Dyelot+TranCd=laData[1] ;
    INTO CURSOR (lcisslogfile) ORDER BY Style,cWareCode,Dyelot
  CASE lcTranType='I'
    lcFile = 'POSLN'

    *E#301235,1 Removed STR(RECNO(),7) to match index expr    
    *SELECT * FROM POSLN WHERE ;
    cStytype+po+style+STR(lineno,6)+trancd+STR(RECNO(),7)='P'+laData[1] ;
    INTO CURSOR (lcisslogfile) ORDER BY Style,cWareCode,Dyelot
    *C200080,1 AMM Consider the dye order case
    *SELECT * FROM POSLN WHERE ;
    cStytype+po+style+STR(lineno,6)+trancd='P'+laData[1] ;
    INTO CURSOR (lcisslogfile) ORDER BY Style,cWareCode,Dyelot
    
    SELECT * FROM POSLN WHERE ;
    cStytype+po+style+STR(lineno,6)+trancd=IIF(EMPTY(lcDyePO),'P','D')+laData[1] ;
    INTO CURSOR (lcisslogfile) ORDER BY Style,cWareCode,Dyelot
    *C200080,1 AMM end
    
  *E#301235,1  
  CASE lcTranType='T'
    lcFile = 'MMFGORDD'          
    SELECT * FROM MMFGORDD WHERE cmfgordno+cfabric+color+dyelot+trancd=laData[1] ;
    INTO CURSOR (lcisslogfile) ORDER BY cfabric,color,cWareCode,Dyelot

ENDCASE
GO TOP
STORE 0 TO lnCanceled
*E#301235,1
IF lcTranType='T'
  DO WHILE !EOF()
    SCATTER MEMVAR
    SUM REST nMfgTotQty TO m.nMfgTotQty;
       WHILE cfabric+color+cWareCode+Dyelot = m.cfabric+m.color+m.cWareCode+m.Dyelot
    m.nMfgTotQty  = MAX(m.nMfgTotQty,0)
    IF m.nMfgTotQty > 0
      IF !EMPTY(m.Dyelot) .AND. SEEK(m.cfabric+m.color+m.cWareCode+m.Dyelot,'FabDye')
        SELECT FabDye
        =RLOCK()
        REPLACE OnOrder WITH MAX(OnOrder - m.nMfgTotQty,0)  
        UNLOCK
      ENDIF
      IF SEEK(m.cfabric+m.color+m.cWareCode+SPACE(10),'FabDye')
        SELECT FabDye
        =RLOCK()
        REPLACE OnOrder WITH MAX(OnOrder - m.nMfgTotQty,0)  
        UNLOCK
      ENDIF
      IF SEEK(m.cfabric+m.color,'FABRIC')
        SELECT FABRIC
        =RLOCK()
        REPLACE OnOrder WITH MAX(OnOrder - m.nMfgTotQty,0)  
        UNLOCK
      ENDIF
      m.TranCd ='4'
      m.cRSession = lcSession
      lnCanceled  = lnCanceled + m.nMfgTotQty
      INSERT INTO (lcFile) FROM MEMVAR
    ENDIF
    SELECT (lcisslogfile)
  ENDDO
ELSE
  DO WHILE !EOF()
    SCATTER MEMVAR
*--- DOC.
*--- Get the open qty
*--- DOC.
    SUM REST IIF(TranCd='1',Qty1,-Qty1),IIF(TranCd='1',Qty2,-Qty2) ,;
             IIF(TranCd='1',Qty3,-Qty3),IIF(TranCd='1',Qty4,-Qty4) ,;
             IIF(TranCd='1',Qty5,-Qty5),IIF(TranCd='1',Qty6,-Qty6) ,;
             IIF(TranCd='1',Qty7,-Qty7),IIF(TranCd='1',Qty8,-Qty8)  ;
         TO  m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8;
    WHILE Style+cWareCode+Dyelot = m.Style+m.cWareCode+m.Dyelot
    m.Qty1  = MAX(m.Qty1,0)
    m.Qty2  = MAX(m.Qty2,0)
    m.Qty3  = MAX(m.Qty3,0)
    m.Qty4  = MAX(m.Qty4,0)
    m.Qty5  = MAX(m.Qty5,0)
    m.Qty6  = MAX(m.Qty6,0)
    m.Qty7  = MAX(m.Qty7,0)
    m.Qty8  = MAX(m.Qty8,0)
    m.TotQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
    IF m.TotQty > 0
      IF !EMPTY(m.Dyelot) .AND. SEEK(m.Style+m.cWareCode+m.Dyelot,'StyDye')
        SELECT StyDye
        =RLOCK()
        REPLACE WIP1   WITH MAX(WIP1 - m.Qty1,0)   ,;
                WIP2   WITH MAX(WIP2 - m.Qty2,0)   ,;
                WIP3   WITH MAX(WIP3 - m.Qty3,0)   ,;
                WIP4   WITH MAX(WIP4 - m.Qty4,0)   ,;
                WIP5   WITH MAX(WIP5 - m.Qty5,0)   ,;
                WIP6   WITH MAX(WIP6 - m.Qty6,0)   ,;
                WIP7   WITH MAX(WIP7 - m.Qty7,0)   ,;
                WIP8   WITH MAX(WIP8 - m.Qty8,0)   ,;
                TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
        UNLOCK
      ENDIF
      IF SEEK(m.Style+m.cWareCode+SPACE(10),'StyDye')
        SELECT StyDye
        =RLOCK()
        REPLACE WIP1   WITH MAX(WIP1 - m.Qty1,0)   ,;
                WIP2   WITH MAX(WIP2 - m.Qty2,0)   ,;
                WIP3   WITH MAX(WIP3 - m.Qty3,0)   ,;
                WIP4   WITH MAX(WIP4 - m.Qty4,0)   ,;
                WIP5   WITH MAX(WIP5 - m.Qty5,0)   ,;
                WIP6   WITH MAX(WIP6 - m.Qty6,0)   ,;
                WIP7   WITH MAX(WIP7 - m.Qty7,0)   ,;
                WIP8   WITH MAX(WIP8 - m.Qty8,0)   ,;
                TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
        UNLOCK
      ENDIF
      IF SEEK(m.Style,'Style')
        SELECT Style
        =RLOCK()
        REPLACE WIP1   WITH MAX(WIP1 - m.Qty1,0)   ,;
                WIP2   WITH MAX(WIP2 - m.Qty2,0)   ,;
                WIP3   WITH MAX(WIP3 - m.Qty3,0)   ,;
                WIP4   WITH MAX(WIP4 - m.Qty4,0)   ,;
                WIP5   WITH MAX(WIP5 - m.Qty5,0)   ,;
                WIP6   WITH MAX(WIP6 - m.Qty6,0)   ,;
                WIP7   WITH MAX(WIP7 - m.Qty7,0)   ,;
                WIP8   WITH MAX(WIP8 - m.Qty8,0)   ,;
                TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
        UNLOCK
      ENDIF
      m.TranCd =IIF(lcTranType='M','4','5')
      m.cRSession = lcSession
      lnCanceled  = lnCanceled + m.TotQty
      INSERT INTO (lcFile) FROM MEMVAR
    ENDIF
    SELECT (lcisslogfile)
  ENDDO
ENDIF
USE IN (lcisslogfile)
SELECT (lcBaseFile)
laData[3]  = 'S'
laData[8]  = lnCanceled
laData[10] = 0
GATHER FROM laData FIELDS &lcScFields
*E300725,1 Post difference between actual and landed cost to GL

*--- Doc. [Begin]
*--- At the next part after IF laSetups[1,2]='Y' (Link to GL)
*--- We will start to update the GlDist file as following :
*--- for each one of the five cost element
*--- if there exist Differences between Actual and landed cost
*--- we will go to insert Gl_Interies with the Deferent
*--- Ex :- IF Actual = 12 , Landed = 10 ==> Gl-Interies = 2.
*--- And we will use the following Category Key for each one of the costing
*--- '022' , '023' , '024' , '025' , '026'
*--- Except for 'Material Manufucturing Order' we will use Category '021'.
*--- if the Gl_Link.CatgDesc empty for any one of the above Keys 
*--- we will use the key '019'  as default one.
*--- Doc. [End]

IF laSetups[1,2]='Y'
  STORE '' TO m.cWipAcnt,m.cVarAcnt1,m.cVarAcnt2,m.cVarAcnt3,m.cVarAcnt4,m.cVarAcnt5
  SET ORDER TO TAG GL_LINK IN GL_LINK
  IF lcTranType = 'T'
    lnLanded=laData[17]+laData[18]+laData[19]+laData[20]
    lnActual=laData[22]+laData[23]+laData[24]+laData[25]
    IF lnLanded <> lnActual
      *B603504,1 (Begin) Remark the following lines and Change the transaction type 
      *B603504,1         sent by GLDIST procedure for closing matreial MFG from 'JC'  to 'MC'.
      *DO GlDist WITH laData[29],'013',(lnLanded-lnActual),'JC',;
                     laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
      *m.cWipAcnt = &lcGlDTemp..GlAccount
      *DO GlDist WITH laData[29],'021',(lnActual-lnLanded),'JC',;
                     laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
      *m.cVarAcnt1 = &lcGlDTemp..GlAccount
      
      DO GlDist WITH laData[29],'013',(lnLanded-lnActual),'MC',;
                     laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
      m.cWipAcnt = &lcGlDTemp..GlAccount
      DO GlDist WITH laData[29],'021',(lnActual-lnLanded),'MC',;
                     laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
      m.cVarAcnt1 = &lcGlDTemp..GlAccount
      *B603504,1 (End)
    ENDIF
  ENDIF
  *--- Doc. [Start]
  
  *--- laData[22] == 1St Cost Element Actual Cost.
  *--- laData[17] == 1St Cost Element Landed Cost.
  
  *--- laData[23] == 2nd Cost Element Actual Cost.
  *--- laData[18] == 2nd Cost Element Landed Cost.

  *--- ladata[24] == 3rd Cost Element Actual Cost.
  *--- ladata[19] == 3rd Cost Element Landed Cost.

  *--- ladata[25] == 4Th Cost Element Actual Cost.
  *--- ladata[20] == 4Th Cost Element Landed Cost.
  
  *--- ladata[26] == 5Th Cost Element Actual Cost.
  *--- ladata[21] == 5Th Cost Element Landed Cost.

  *--- The 1st,2nd,3rd,4th and 5th cost element take an label depending
  *--- on the module setup (IC-Module)
  *--- and the vaiance between Actual & Landed is the variance
  *--- which we will update GL- with.
  
  *--- ladata[29] == Link Code.

  *--- Doc. [End]

  *B603862,1 Define a new variable to hold Type of Cost Sheet (Manuf or Purch) [Begin]
  *B603862,1 make a new Type for Cost Sheet in SyGlTrans in case of Manufacturing
  PRIVATE lcGLTrnTyp
  lcGLTrnTyp = IIF(gcAct_Appl=="MF",'JP','JC')
  *B603862,1 Define a new variable to hold Type of Cost Sheet (Manuf or Purch) [End]

  IF lcTranType <> 'T' .AND. laData[22] <> laData[17]
    *B603573,1 SSH 16/04/00  [Start] Comented Out.
    *=SEEK(ladata[29]+'021','GL_LINK')
    *B603573,1 SSH [End]

    *B603862,1 Adding the right type for each of PO , MF [Begin]
    *DO GlDist WITH laData[29],'013',(laData[17]-laData[22]),'JC',;
    *               laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
    DO GlDist WITH laData[29],'013',(laData[17]-laData[22]),lcGLTrnTyp,;
                   laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
    *B603862,1 Adding the right type for each of PO , MF [End]

    m.cWipAcnt = &lcGlDTemp..GlAccount
    *B603573,1 SSH 16/04/00  [Start] Comented Out.
    *DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','021'),;
                   (laData[22]-laData[17]),'JC',laData[1],ldClsDate,;
                   lcGlYear,lcGlPeriod,lcGlDTemp

    *B603573,1 SSH 16/04/00  [Start] Use the correct categKey.
    =SEEK(ladata[29]+'022','GL_LINK')
    *B603573,1 SSH 16/04/00   [End]
    
    *B603862,1 Adding the right type for each of PO , MF [Begin]
    *DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','022'),;
    *               (laData[22]-laData[17]),'JC',laData[1],ldClsDate,;
    *               lcGlYear,lcGlPeriod,lcGlDTemp
    DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','022'),;
                   (laData[22]-laData[17]),lcGLTrnTyp,laData[1],ldClsDate,;
                   lcGlYear,lcGlPeriod,lcGlDTemp
    *B603862,1 Adding the right type for each of PO , MF [Begin]
                   
    *B603573,1 SSH 16/04/00   [End]
    m.cVarAcnt1 = &lcGlDTemp..GlAccount
  ENDIF
  
  IF lcTranType <> 'T' .AND. laData[23] <> laData[18]
    *B603573,1 SSH 16/04/00  [Start] Commented Out.
    *=SEEK(ladata[29]+'022','GL_LINK')
    *B603573,1 SSH 16/04/00   [End]

    *B603862,1 Adding the right type for each of PO , MF [Begin]
    *DO GlDist WITH laData[29],'013',(laData[18]-laData[23]),'JC',;
    *               laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
    DO GlDist WITH laData[29],'013',(laData[18]-laData[23]),lcGLTrnTyp,;
                   laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
    *B603862,1 Adding the right type for each of PO , MF [Begin]
    
    m.cWipAcnt = &lcGlDTemp..GlAccount
    *B603573,1 SSH 16/04/00  [Start] Commented Out.
    *DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','022'),;
                   (laData[23]-laData[18]),'JC',laData[1],ldClsDate,;
                   lcGlYear,lcGlPeriod,lcGlDTemp
    
    *B603573,1 SSH 16/04/00  [Start] Use the correct CategKey.
    =SEEK(ladata[29]+'023','GL_LINK')
    *B603573,1 SSH 16/04/00  [End]

    *B603862,1 Adding the right type for each of PO , MF [Begin]
    *DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','023'),;
    *               (laData[23]-laData[18]),'JC',laData[1],ldClsDate,;
    *               lcGlYear,lcGlPeriod,lcGlDTemp
    DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','023'),;
                   (laData[23]-laData[18]),lcGLTrnTyp,laData[1],ldClsDate,;
                   lcGlYear,lcGlPeriod,lcGlDTemp
    *B603862,1 Adding the right type for each of PO , MF [Begin]

    *B603573,1 SSH 16/04/00  [End]
    m.cVarAcnt2 = &lcGlDTemp..GlAccount
  ENDIF
  IF lcTranType <> 'T' .AND. laData[24] <> laData[19]
    *B603573,1 SSH 16/04/00  [Start] Commented Out.
    *=SEEK(ladata[29]+'023','GL_LINK')
    *B603573,1 SSH 16/04/00 [End]

    *B603862,1 Adding the right type for each of PO , MF [Begin]
    *DO GlDist WITH laData[29],'013',(laData[19]-laData[24]),'JC',;
    *               laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
    DO GlDist WITH laData[29],'013',(laData[19]-laData[24]),lcGLTrnTyp,;
                   laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
    *B603862,1 Adding the right type for each of PO , MF [Begin]
 
    m.cWipAcnt = &lcGlDTemp..GlAccount
    *B603573,1 SSH 16/04/00  [Start] Commented Out.
    *DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','023'),;
                   (laData[24]-laData[19]),'JC',laData[1],ldClsDate,;
                   lcGlYear,lcGlPeriod,lcGlDTemp
    
    *B603573,1 SSH 16/04/00 [Start] Use the correct CategKey.
    =SEEK(ladata[29]+'024','GL_LINK')
    *B603573,1 SSH 16/04/00 [End]

    *B603862,1 Adding the right type for each of PO , MF [Begin]
    *DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','024'),;
    *               (laData[24]-laData[19]),'JC',laData[1],ldClsDate,;
    *               lcGlYear,lcGlPeriod,lcGlDTemp
    DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','024'),;
                   (laData[24]-laData[19]),lcGLTrnTyp,laData[1],ldClsDate,;
                   lcGlYear,lcGlPeriod,lcGlDTemp
    *B603862,1 Adding the right type for each of PO , MF [Begin]
                       
    *B603573,1 SSH 16/04/00 [End]
    m.cVarAcnt3 = &lcGlDTemp..GlAccount
  ENDIF
  IF lcTranType <> 'T' .AND. laData[25] <> laData[20]
    *B802877,1 Change Seek on 024 insted of 025.
    *=SEEK(ladata[29]+'025','GL_LINK')

    *B603573,1 SSH 16/04/00 [Start] Commented Out.
    *=SEEK(ladata[29]+'024','GL_LINK')
    *B603573,1 SSH 16/04/00  [End]
    *B802877,1 End.

    *B603862,1 Adding the right type for each of PO , MF [Begin]
    *DO GlDist WITH laData[29],'013',(laData[20]-laData[25]),'JC',;
    *               laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
    DO GlDist WITH laData[29],'013',(laData[20]-laData[25]),lcGLTrnTyp,;
                   laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
    *B603862,1 Adding the right type for each of PO , MF [Begin]
                   
    m.cWipAcnt = &lcGlDTemp..GlAccount
    *B802877,1 send '024' insted of '025'.
    *DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','025'),;
                   (laData[25]-laData[20]),'JC',laData[1],ldClsDate,;
                   lcGlYear,lcGlPeriod,lcGlDTemp
    *B603573,1 SSH 16/04/00 [Start] Commented Out.
    *DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','024'),;
                   (laData[25]-laData[20]),'JC',laData[1],ldClsDate,;
                   lcGlYear,lcGlPeriod,lcGlDTemp

    *B603573,1 SSH 16/04/00 [Start] Get the correct CategKey.
    =SEEK(ladata[29]+'025','GL_LINK')
    *B603573,1 SSH 16/04/00  [End]

    *B603862,1 Adding the right type for each of PO , MF [Begin]
    *DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','025'),;
    *               (laData[25]-laData[20]),'JC',laData[1],ldClsDate,;
    *               lcGlYear,lcGlPeriod,lcGlDTemp
    DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','025'),;
                   (laData[25]-laData[20]),lcGLTrnTyp,laData[1],ldClsDate,;
                   lcGlYear,lcGlPeriod,lcGlDTemp    
    *B603862,1 Adding the right type for each of PO , MF [Begin]
                   
    *B603573,1 SSH 16/04/00 [End]
    *B802877,1 End.
    m.cVarAcnt4 = &lcGlDTemp..GlAccount
  ENDIF
  IF lcTranType <> 'T' .AND. laData[26] <> laData[21]
    *B802877,1 Change Seek on 025 insted of 026.
    *=SEEK(ladata[29]+'026','GL_LINK')

    *B603573,1 SSH 16/04/00 [Start] Commented Out
    *=SEEK(ladata[29]+'025','GL_LINK')
    =SEEK(ladata[29]+'026','GL_LINK')
    *B603573,1 SSH 16/04/00  [End]
    *B802877,1 End.

    *B603862,1 Adding the right type for each of PO , MF [Begin]
    *DO GlDist WITH laData[29],'013',(laData[21]-laData[26]),'JC',;
    *               laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
    DO GlDist WITH laData[29],'013',(laData[21]-laData[26]),lcGLTrnTyp,;
                   laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
    *B603862,1 Adding the right type for each of PO , MF [Begin]
                        
    m.cWipAcnt = &lcGlDTemp..GlAccount
    *B802877,1 send '025' insted of '026'.
    *DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','026'),;
                   (laData[26]-laData[21]),'JC',laData[1],ldClsDate,;
                   lcGlYear,lcGlPeriod,lcGlDTemp
    *B603573,1 SSH 16/04/00 [Start] Commented Out.
    *DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','025'),;
                   (laData[26]-laData[21]),'JC',laData[1],ldClsDate,;
                   lcGlYear,lcGlPeriod,lcGlDTemp
    
    *B603573,1 SSH 16/04/00 [Start] Get the correct CategKey.
    =SEEK(ladata[29]+'026','GL_LINK')
    *B603573,1 SSH 16/04/00  [End]

    *B603862,1 Adding the right type for each of PO , MF [Begin]
    *DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','026'),;
    *               (laData[26]-laData[21]),'JC',laData[1],ldClsDate,;
    *               lcGlYear,lcGlPeriod,lcGlDTemp
    DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','026'),;
                   (laData[26]-laData[21]),lcGLTrnTyp,laData[1],ldClsDate,;
                   lcGlYear,lcGlPeriod,lcGlDTemp    
    *B603862,1 Adding the right type for each of PO , MF [Begin]
                   
    *B603573,1 SSH 16/04/00 [End]
    *B802877,1 End.
    m.cVarAcnt5 = &lcGlDTemp..GlAccount
  ENDIF
  DO CASE
    CASE lcTranType = 'M'
      SELECT CUTTKTH
      =RLOCK()
      REPLACE cWipAcnt  WITH m.cWipAcnt  ,;
              cVarAcnt1 WITH m.cVarAcnt1 ,;
              cVarAcnt2 WITH m.cVarAcnt2 ,;
              cVarAcnt3 WITH m.cVarAcnt3 ,;
              cVarAcnt4 WITH m.cVarAcnt4 ,;
              cVarAcnt5 WITH m.cVarAcnt5
      UNLOCK
    CASE lcTranType = 'I'    
      SELECT POSHDR
      =RLOCK()
      REPLACE cWipAcnt  WITH m.cWipAcnt  ,;
              cVarAcnt1 WITH m.cVarAcnt1 ,;
              cVarAcnt2 WITH m.cVarAcnt2 ,;
              cVarAcnt3 WITH m.cVarAcnt3 ,;
              cVarAcnt4 WITH m.cVarAcnt4 ,;
              cVarAcnt5 WITH m.cVarAcnt5
      UNLOCK
    CASE lcTranType = 'T'
      SELECT MMFGORDH
      =RLOCK()
      REPLACE cWipAcnt  WITH m.cWipAcnt  ,;
              cVarAcnt1 WITH m.cVarAcnt1
      UNLOCK
  ENDCASE
  SELECT (lcGlDTemp)
  REPLACE ALL GLSESSION WITH lcSession
  SELECT GLDIST
  APPEND FROM (gcWorkDir+lcGlDTemp)
  SELECT (lcGlDTemp)
  ZAP
  SET ORDER TO TAG GL_LINK1 IN GL_LINK
ENDIF
WAIT CLEAR
SHOW GETS

*!*************************************************************
*! Name      : lpClsScr
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Cancel modification
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lpClsScr()
*!*************************************************************
FUNCTION lpClsScr
*E301427,1 SSH Delete Pw temp files when closing the scren
IF llPWInst .AND. (lcTranType = "M") .AND.  USED(lcPWCtkBom)
  SELECT(lcPWCtkBom)
  ZAP
ENDIF
IF llPWInst .AND. (lcTranType = "M") .AND. USED(lcDetLin)
  SELECT(lcDetLin)
  ZAP
ENDIF
*E301427,1 SSH [END]

*!*************************************************************
*! Name      : lpDelScr
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Delete Cost Sheet
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lpDelScr()
*!*************************************************************
FUNCTION lpDelScr
PRIVATE laEmptyRec,laIssQty

IF laData[3] = 'S'
  *E300725,1 Message : 38059
  *E300725,1 This ticket has been closed. Cannot delete cost sheet.
  *E300725,1 Button : 00000
  *E300725,1 Ok
  *C200080,1 AMM Consider the dye order case
  *=gfModalGen('TRM38059B00000','ALERT',IIF(lcTranType='M','cutting ticket',IIF(lcTranType='I','purchase order','MFG order')))
  
  *khm1
  *=gfModalGen('TRM38059B00000','ALERT',IIF(lcTranType='M','cutting ticket',IIF(lcTranType='I', IIF(EMPTY(lcDyePO),'purchase order','dye order') ,'MFG order')))
  =gfModalGen('TRM38059B00000','ALERT',IIF(lcTranType='M','cutting ticket',IIF(lcTranType $ 'ID', IIF(EMPTY(lcDyePO),'purchase order','dye order') ,'MFG order')))
  *khm1
    
  *C200080,1 AMM end
  RETURN
ENDIF

*B605270,1 KHM 01/08/2001 (Begin) Checking if there are any receiving to
*B605270,1                cancel or to damage then prevent deleting the cost sheet.
llChkRecev = .F.
DO CASE
  CASE lcTranType = 'M'
    IF SEEK(laData[1],'CutTktL')
      SELECT CutTktl
      LOCATE REST WHILE CutTkt+Style+Dyelot+TranCd = laData[1] ;
                  FOR TranCd $ '34'
      llChkRecev = FOUND()                  
    ENDIF

  *khm1
  *CASE lcTranType = 'I'
  CASE lcTranType $ 'ID'
  *khm1
  
    IF SEEK(IIF(EMPTY(lcDyePO),'P','D')+laData[1],'PosLn')
      SELECT PosLn

      *B124619,1 NNA 11/03/2004 (Begin) Check only for Trancd='4' (Second and Damaged) Qty.
      *B124619,1 NNA            to be able to delete the cost sheet if all Qty. Received as Cancelled
      *LOCATE REST WHILE cStytype+Po+Style+STR(LineNo,6)+TranCd = ;
                        IIF(EMPTY(lcDyePO),'P','D')+laData[1] ;
                  FOR TranCd $ '45'
      LOCATE REST WHILE cStytype+Po+Style+STR(LineNo,6)+TranCd = ;
                        IIF(EMPTY(lcDyePO),'P','D')+laData[1] ;
                  FOR TranCd $ '4'
      *B124619,1 NNA (End)

      llChkRecev = FOUND()                         
    ENDIF
ENDCASE
*B605270,1 KHM 01/08/2001 (End)

*B605270,1 KHM 01/08/2001 (Begin) Prevent deleting the cost sheet if there
*B605270,1                is any received to cancel or to damage also.
*IF laData[7] > 0
IF laData[7] > 0 OR llChkRecev
*B605270,1 KHM 01/08/2001 (End)

  *E300725,1 Message : 38060
  *E300725,1 Pieces have been received on this ticket. Cannot delete cost sheet.
  *E300725,1 Button : 00000
  *E300725,1 Ok
  *C200080,1 AMM Consider the dye order case
  *=gfModalGen('TRM38060B00000','ALERT',IIF(lcTranType='M','cutting ticket',IIF(lcTranType='I','purchase order','MFG order')))

  *khm1
  *=gfModalGen('TRM38060B00000','ALERT',IIF(lcTranType='M','cutting ticket',IIF(lcTranType='I', IIF(EMPTY(lcDyePO),'purchase order','dye order') ,'MFG order')))
  =gfModalGen('TRM38060B00000','ALERT',IIF(lcTranType='M','cutting ticket',IIF(lcTranType $ 'ID', IIF(EMPTY(lcDyePO),'purchase order','dye order') ,'MFG order')))
  *khm1
    
  *C200080,1 AMM end
  RETURN
ENDIF
IF SEEK(lcTranType+laData[1],'MFGOPRHD')
  *E300725,1 Message : 38058
  *E300725,1 Manufacturing operation has been setup for this ticket.
  *E300725,1 Button : 38002
  *E300725,1 Proceed  Cancel
  *C200080,1 AMM Consider the dye order case
  *IF gfModalGen('TRM38058B38002','ALERT',IIF(lcTranType='M','cutting ticket',IIF(lcTranType='I','purchase order','MFG order')))=2

  *khm1
  *IF gfModalGen('TRM38058B38002','ALERT',IIF(lcTranType='M','cutting ticket',IIF(lcTranType='I', IIF(EMPTY(lcDyePO),'purchase order','dye order') ,'MFG order')))=2
  IF gfModalGen('TRM38058B38002','ALERT',IIF(lcTranType='M','cutting ticket',IIF(lcTranType $ 'ID', IIF(EMPTY(lcDyePO),'purchase order','dye order') ,'MFG order')))=2
  *khm1
  
  *C200080,1 AMM end
    RETURN
  ENDIF  
ENDIF
*E301077,55 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'BOMCOST',gcDataDir+'POBOMCLS','SH')
*E301077,55 (End)
SET ORDER TO TAG POBOMCLS IN BOMCOST
IF SEEK(lcTranType+laData[1],'BOMCOST')
  *E300725,1 Message : 38033
  *E300725,1 There are cost items have been applied against this xxxx cost 
  *E300725,1 sheet. Deleting the cost sheet will return all issued items
  *E300725,1 Button : 38002
  *E300725,1 Proceed  Cancel
  *C200080,1 AMM Consider the dye order case
  *IF gfModalGen('QRM38033B38002','ALERT',IIF(lcTranType='M','cutting ticket',IIF(lcTranType='I','purchase order','order'))) = 2

  *khm1
  *IF gfModalGen('QRM38033B38002','ALERT',IIF(lcTranType='M','cutting ticket',IIF(lcTranType='I', IIF(EMPTY(lcDyePO),'purchase order','dye order') ,'order'))) = 2
  IF gfModalGen('QRM38033B38002','ALERT',IIF(lcTranType='M','cutting ticket',IIF(lcTranType $ 'ID', IIF(EMPTY(lcDyePO),'purchase order','dye order') ,'order'))) = 2  
  *khm1
  
  *C200080,1 AMM
    RETURN
  ENDIF
ENDIF
IF !CHECKPRD(gdSysDate,'lcGlYear','lcGlPeriod','  ')
  RETURN
ENDIF

*E301077,55 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
=gfOpenFile(gcDataDir+'FABDYE',gcDataDir+'FABDYE','SH')
=gfOpenFile(gcDataDir+'STYINVJL',gcDataDir+'STYINVJL','SH')
IF 'MA' $ gcComp_mdl
  =gfOpenFile(gcDataDir+'MATINVJL',gcDataDir+'MATINVJL','SH')
ENDIF  
*E301077,55 (End)

DECLARE laIssQty[9]
*B603025,1 declare by zero.
laIssQty = 0
WAIT 'Updating the cost sheet details file.' WINDOW NOWAIT
IF SEEK(lcTranType+'1'+laData[1],'BomLine')
  SELECT BomLine
  DELETE REST WHILE CIMTYP+CTYPE+CTKTNO = lcTranType+'1'+laData[1]
ENDIF
IF SEEK(lcTranType+'2'+laData[1],'BomLine')
  SELECT BomLine
  DELETE REST WHILE CIMTYP+CTYPE+CTKTNO = lcTranType+'2'+laData[1]
ENDIF
WAIT 'Updating the cost sheet cost file.' WINDOW NOWAIT

*E300725,1 Return issued styles
*--- DOC.
*--- Get the current cuttkt record from StyInvJl.
*--- DOC.
SELECT StyInvJl
SELECT * FROM StyInvJl ;
WHERE ctrcode+coprcode+clotno+ctrtype+style+cwarecode=laData[1] ;
INTO CURSOR (lcIsslogFile) ;
ORDER BY ctrtype,ctrcode,coprcode,clotno,style,cwarecode,cDyelot

LOCATE FOR ctrtype+ctrcode='1'+laData[1]
IF FOUND()
  DO WHILE ctrtype+ctrcode= '1'+laData[1]
  
    SCATTER MEMVAR
    =SEEK(m.Style,'Style')
    =SEEK(m.Style+m.cWareCode+SPACE(10),'StyDye')
    lnIssCost = IIF(laSetups[10,2]='A',StyDye.Ave_Cost,Style.TotCost)
  
    SUM REST nStk1,nStk2,nStk3,nStk4,nStk5,nStk6,nStk7,nStk8  ;
        TO   laIssQty[1],laIssQty[2],laIssQty[3],laIssQty[4],laIssQty[5],;
             laIssQty[6],laIssQty[7],laIssQty[8]  ;
       WHILE ctrtype+ctrcode+coprcode+clotno+style+cwarecode+cDyelot = ;
             '1'+laData[1]+m.cOprCode+m.cLotNo+m.Style+m.cWareCode+m.cDyelot

    laIssQty[1] = -1*MIN(laIssQty[1],0)
    laIssQty[2] = -1*MIN(laIssQty[2],0)
    laIssQty[3] = -1*MIN(laIssQty[3],0)
    laIssQty[4] = -1*MIN(laIssQty[4],0)
    laIssQty[5] = -1*MIN(laIssQty[5],0)
    laIssQty[6] = -1*MIN(laIssQty[6],0)
    laIssQty[7] = -1*MIN(laIssQty[7],0)
    laIssQty[8] = -1*MIN(laIssQty[8],0)
    laIssQty[9] = laIssQty[1]+laIssQty[2]+laIssQty[3]+laIssQty[4]+;
                  laIssQty[5]+laIssQty[6]+laIssQty[7]+laIssQty[8]
    IF laIssQty[9] > 0
      =lfIssRetSty('','',m.Style,m.cWareCode,m.cDyelot,@laIssQty,lnIssCost,;
                   m.cOprCode,m.cLotNo,gdSysDate,lcSession,'',.T.)
    ENDIF
    SELECT (lcIsslogFile)
  ENDDO
ENDIF

*E301248 (Start)
PRIVATE laIssTypeI
DIMENSION laIssTypeI[9]
*B603025,1 declare by zero.
laIssTypeI=0
LOCATE FOR ctrtype+ctrcode='I'+laData[1]
IF FOUND()
  DO WHILE ctrtype+ctrcode= 'I'+laData[1]
  
    SCATTER MEMVAR
    =SEEK(m.Style,'Style')
    =SEEK(m.Style+m.cWareCode+SPACE(10),'StyDye')
    lnIssCost = IIF(laSetups[10,2]='A',StyDye.Ave_Cost,Style.TotCost)
  
    SUM REST nStk1,nStk2,nStk3,nStk4,nStk5,nStk6,nStk7,nStk8  ;
        TO   laIssTypeI[1],laIssTypeI[2],laIssTypeI[3],laIssTypeI[4],;
             laIssTypeI[5],laIssTypeI[6],laIssTypeI[7],laIssTypeI[8]  ;
       WHILE ctrtype+ctrcode+coprcode+clotno+style+cwarecode+cDyelot = ;
             'I'+laData[1]+m.cOprCode+m.cLotNo+m.Style+m.cWareCode+m.cDyelot

    *B605427,1 KHM 01/28/2002 (Begin) Correct the name of the arrays.
    *laIssTypeI[1] = -1*MIN(laIssQty[1],0)
    *laIssTypeI[2] = -1*MIN(laIssQty[2],0)
    *laIssTypeI[3] = -1*MIN(laIssQty[3],0)
    *laIssTypeI[4] = -1*MIN(laIssQty[4],0)
    *laIssTypeI[5] = -1*MIN(laIssQty[5],0)
    *laIssTypeI[6] = -1*MIN(laIssQty[6],0)
    *laIssTypeI[7] = -1*MIN(laIssQty[7],0)
    *laIssTypeI[8] = -1*MIN(laIssQty[8],0)
    *laIssTypeI[9] = laIssTypeI[1]+laIssTypeI[2]+laIssTypeI[3]+laIssTypeI[4]+;
    *                laIssTypeI[5]+laIssTypeI[6]+laIssTypeI[7]+laIssTypeI[8]

    laIssQty[1] = -1*MIN(laIssTypeI[1],0)
    laIssQty[2] = -1*MIN(laIssTypeI[2],0)
    laIssQty[3] = -1*MIN(laIssTypeI[3],0)
    laIssQty[4] = -1*MIN(laIssTypeI[4],0)
    laIssQty[5] = -1*MIN(laIssTypeI[5],0)
    laIssQty[6] = -1*MIN(laIssTypeI[6],0)
    laIssQty[7] = -1*MIN(laIssTypeI[7],0)
    laIssQty[8] = -1*MIN(laIssTypeI[8],0)
    laIssQty[9] = laIssQty[1]+laIssQty[2]+laIssQty[3]+laIssQty[4]+;
                  laIssQty[5]+laIssQty[6]+laIssQty[7]+laIssQty[8]

    *B605427,1 KHM 01/28/2002 (End)
    IF laIssQty[9] > 0
      =lfIssRetSty('','',m.Style,m.cWareCode,m.cDyelot,@laIssQty,lnIssCost,;
                   m.cOprCode,m.cLotNo,gdSysDate,lcSession,'',.T.)
    ENDIF
    SELECT (lcIsslogFile)
  ENDDO

  *B603025,1 put in if found.
  laIssQty[1] = laIssQty[1] + laIssTypeI[1]
  laIssQty[2] = laIssQty[2] + laIssTypeI[2]
  laIssQty[3] = laIssQty[3] + laIssTypeI[3]
  laIssQty[4] = laIssQty[4] + laIssTypeI[4]
  laIssQty[5] = laIssQty[5] + laIssTypeI[5]
  laIssQty[6] = laIssQty[6] + laIssTypeI[6]
  laIssQty[7] = laIssQty[7] + laIssTypeI[7]
  laIssQty[8] = laIssQty[8] + laIssTypeI[8]
  laIssQty[9] = laIssQty[9] + laIssTypeI[9]

ENDIF
*E301248 (End)

*E300725,1 Return issued materials
IF 'MA' $ gcComp_mdl
  SELECT * FROM MATINVJL ;
  WHERE cTran+cOprCode+cLotNo+cTranType+cFabric+cColor+cWarecode=laData[1] ;
  AND cIMTyp = lcTranType INTO CURSOR (lcIsslogFile) ;
  ORDER BY cTran,cOprCode,cLotNo,cTranType,cFabric,cColor,cWarecode,cDyelot,cRSession,cISession
  IF _TALLY > 0
    IF INLIST(laSetups[9,2],'L','F','I')
      DO WHILE !EOF()
        SCATTER MEMVAR
        =SEEK(m.cFabric+m.cColor,'Fabric')
        *HDM E301037,1 Should Be Updated to nReceived[Start]
        *SUM REST  nIssued * -1 TO lnIssued ;

        *B607465,1 AMH Consider case of custom isuue/return by roll [Start]
        *SUM REST nIssued-nReceived TO lnIssued ;
            WHILE ctran+coprcode+clotno+ctrantype+cfabric+ccolor+cwarecode+cDyelot+cRSession+cISession=;
                  laData[1]+m.cOprCode+m.cLotNo+'4'+m.cFabric+m.cColor+m.cWareCode+m.cDyelot+m.cRSession+m.cISession
        lnIssued = 0
        IF ASCAN(laEvntTrig,PADR("GETRET",10)) <> 0
          =gfDoTriger("POCSSH",PADR("GETRET",10))
        ELSE
          SUM REST nIssued-nReceived TO lnIssued ;
              WHILE ctran+coprcode+clotno+ctrantype+cfabric+ccolor+cwarecode+cDyelot+cRSession+cISession=;
                    laData[1]+m.cOprCode+m.cLotNo+'4'+m.cFabric+m.cColor+m.cWareCode+m.cDyelot+m.cRSession+m.cISession
        ENDIF
        *B607465,1 AMH [End]
        
        *HDM E301037,1 Should Be Updated to nReceived[End]
        
        *B037268,1 ABD Amendment to PO cost sheet 'issue/return' to allow return 
        *B037268,1 ABD Of trims to a different warehouse. [Begin]
        IF ASCAN(laEvntTrig,PADR("GETRTQTY",10)) <> 0 
          = gfDoTriger("POCSSH",PADR("GETRTQTY",10)))
        ENDIF  
        *B037268,1 ABD - [End]

        IF lnIssued > 0
          IF laSetups[11,2]='Y' .AND. Fabric.lTrkRolls
            =lfvMatRolls('R',m.cRSession,m.cISession,m.cFabric,m.cColor,;
                         m.cWareCode,m.cDyelot,lnIssued,lnIssued)
          ENDIF
          =lfIssRetFab('','',m.cFabric,m.cColor,m.cWareCode,m.cDyelot,-lnIssued,;
                       m.nUntCstBuy,m.cOprCode,m.cLotNo,gdSysDate,m.cRSession,m.cISession,.T.)
        ENDIF
        SELECT (lcIsslogFile)
      ENDDO
    ELSE
      DO WHILE !EOF()
        SCATTER MEMVAR
        =SEEK(m.cFabric+m.cColor,'Fabric')
        =SEEK(m.cFabric+m.cColor+m.cWareCode+SPACE(10),'FABDYE')
        lnIssCost = IIF(laSetups[9,2]='S',Fabric.CostBuy,;
                    IIF(laSetups[2,2]='Y',FabDye.nAveCstBuy,Fabric.nAveCstBuy))

        *HDM E301037,1 Also Should Be Updated (nIssued)[Start]
        SUM REST  nIssued-nReceived TO lnIssued ;
            WHILE ctran+coprcode+clotno+ctrantype+cfabric+ccolor+cwarecode+cDyelot+cRSession+cISession=;
                  laData[1]+m.cOprCode+m.cLotNo+'4'+m.cFabric+m.cColor+m.cWareCode+m.cDyelot
        *HDM E301037,1 Also Should Be Updated (nIssued)[End]

        IF lnIssued > 0
          *E301121,4 Use the material control global function
          *=lfIssRetFab('','',m.cFabric,m.cColor,m.cWareCode,m.cDyelot,-lnIssued,;
                       lnIssCost,m.cOprCode,m.cLotNo,gdSysDate,lcSession,SPACE(6),.T.)
          =lfIssRetFab('','',m.cFabric,m.cColor,m.cWareCode,m.cDyelot,-lnIssued,;
                       lnIssCost,m.cOprCode,m.cLotNo,gdSysDate,gfsequence('GLSession'),SPACE(6),.T.)
          *E301121,4 (End)
        ENDIF
        SELECT (lcIsslogFile)
      ENDDO
    ENDIF  
  ENDIF
  USE IN (lcIsslogFile)
ENDIF  
*E300725,1 Return issued manufacturing operations
SELECT BOMCOST

*B605481,1 KHM 02/17/2002 (Begin) Changing the replacement of Actualize
*B605481,1                to "N" instead of "Y".
*IF SEEK(lcTranType+laData[1]+'Y')
IF SEEK(lcTranType+laData[1]+'N')
*B605481,1 KHM 02/17/2002 (End)

  *B605481,1 KHM 02/17/2002 (Begin) Changing the replacement of Actualize
  *B605481,1                to "N" instead of "Y".
  *DO WHILE cimtyp+ctktno+actualize+cbomtype+item+iclr+mfgcode+coprcode+;
           clotno+cisession+crsession=lcTranType+laData[1]+'Y'
  DO WHILE cimtyp+ctktno+actualize+cbomtype+item+iclr+mfgcode+coprcode+;
           clotno+cisession+crsession=lcTranType+laData[1]+'Y'
  *B605481,1 KHM 02/17/2002 (End)
   
    SCATTER MEMVAR
    SUM REST -nTotQty TO lnIssued ;
    WHILE cimtyp+ctktno+actualize+cbomtype+item+iclr+mfgcode+coprcode+clotno+;
          cisession+crsession=;
          m.cimtyp+m.ctktno+m.actualize+m.cbomtype+m.item+;
          m.iclr+m.mfgcode+m.coprcode+m.clotno+m.cISession
    IF (EMPTY(m.Item+m.IClr) .OR. ;
       (m.cCostType='T' AND !SEEK(SUBSTR(m.Item,1,7)+m.IClr,'Fabric'))) .AND. lnIssued < 0 
      *E500231,1 Create new session number for evry MFG issue.
      *=lfIssRetMfg(m.cbomtype,m.cCostType,SUBSTR(m.Item,1,7),m.IClr,m.MfgCode,m.cWareCode,;
      m.cDyelot,lnIssued,m.nUnitCst,gdSysDate,m.cOprCode,m.cLotNo,lcSession,'',.T.)
      =lfIssRetMfg(m.cbomtype,m.cCostType,SUBSTR(m.Item,1,7),m.IClr,m.MfgCode,m.cWareCode,;
                   m.cDyelot,lnIssued,m.nUnitCst,gdSysDate,m.cOprCode,;
                   m.cLotNo,gfsequence('GLSession'),'',.T.)
      *E500231,1 (End)
    ENDIF
    SELECT BOMCOST
  ENDDO
ENDIF
WAIT 'Deleting the cost sheet' WINDOW NOWAIT
SELECT BOMCOST
DO WHILE SEEK(lcTranType+laData[1])
  BLANK
  DELETE
ENDDO
SELECT MFGOPRHD
DO WHILE SEEK(lcTranType+laData[1])
  BLANK
  DELETE
ENDDO
SELECT MFGOPRDT
DO WHILE SEEK(lcTranType+laData[1])
  BLANK
  DELETE
ENDDO
*--- SSH
IF ASCAN(laEvntTrig , PADR('DLTRKHDR',10)) <> 0
  =gfDoTriger('MFCSSH',PADR('DLTRKHDR',10))
ENDIF
*--- SSH
SELECT CTktBom
DO WHILE SEEK(lcTranType+laData[1])
  BLANK
  DELETE
ENDDO
*E301427,1 SSH Delete Pw detail operation
*--- PwCtkBom Key == cuttkt+mfgcode+coprcode+STR(nlineno,6)
llDummy = llPWInst .AND. lcTranType = "M" .AND. lfPwRem(laData[1] , '')
*E301427,1 SSH [END]

STORE 0 TO laData[12],laData[13],laData[14],laData[15],laData[16],;
           laData[22],laData[23],laData[24],laData[25],laData[26]
STORE laData[11] TO laData[31],laData[32]
laData[3] = 'H'
DO CASE 
  CASE lcTranType = 'M'
    SELECT CUTTKTL
    =SEEK(laData[1])
    SCAN REST WHILE CutTKt = laData[1]
      =SEEK(Style,'Style')
      REPLACE nCost1 WITH Style.nMCost1 ,;
              nCost2 WITH Style.nMCost2 ,;
              nCost3 WITH Style.nMCost3 ,;
              nCost4 WITH Style.nMCost4 ,;
              nCost5 WITH Style.nMCost5
      laData[12] = laData[12] + TotQty*nCost1
      laData[13] = laData[13] + TotQty*nCost2
      laData[14] = laData[14] + TotQty*nCost3
      laData[15] = laData[15] + TotQty*nCost4
      laData[16] = laData[16] + TotQty*nCost5
    ENDSCAN
    SELECT CUTTKTH
    =RLOCK()
    GATHER FROM laData FIELDS &lcScFields
    *B605270,1 KHM 01/08/2001 (Begin) Zero out the following fields when
    *B605270,1                deleting the cost sheet.
    REPLACE Pcs_Act WITH 0
    STORE 0 TO laData[27]
    *B605270,1 KHM 01/08/2001 (End)

    UNLOCK
  *khm1
  *CASE lcTranType = 'I'
  CASE lcTranType $ 'ID'
  *khm1
      
    SELECT POSLN
    *C200080,1 AMM (start) Consider the dye order case
    *=SEEK('P'+laData[1])
    *SCAN REST WHILE cStyType+PO = 'P'+laData[1]
    =SEEK(IIF(EMPTY(lcDyePO),'P','D')+laData[1])
    SCAN REST WHILE cStyType+PO = IIF(EMPTY(lcDyePO),'P','D')+laData[1]
    *C200080,1 AMM end
      =SEEK(Style,'Style')
      *MAN Prevent Updating the nCost1
      *REPLACE nCost1 WITH Style.nICost1 ,;
              nCost2 WITH Style.nICost2 ,;
              nCost3 WITH Style.nICost3 ,;
              nCost4 WITH Style.nICost4 ,;
              nCost5 WITH Style.nICost5
      *C200080,1 AMM Update nCost1 in case of dye order cost sheet
      *REPLACE nCost2 WITH Style.nICost2 ,;
              nCost3 WITH Style.nICost3 ,;
              nCost4 WITH Style.nICost4 ,;
              nCost5 WITH Style.nICost5              
      IF EMPTY(lcDyePO)
        REPLACE nCost2 WITH Style.nICost2 ,;
                nCost3 WITH Style.nICost3 ,;
                nCost4 WITH Style.nICost4 ,;
                nCost5 WITH Style.nICost5              
      ELSE
        REPLACE nCost1 WITH Style.nMCost1 ,;
                nCost2 WITH Style.nMCost2 ,;
                nCost3 WITH Style.nMCost3 ,;
                nCost4 WITH Style.nMCost4 ,;
                nCost5 WITH Style.nMCost5 
      ENDIF
      *C200080,1 AMM end
      
      laData[12] = laData[12] + TotQty*nCost1
      laData[13] = laData[13] + TotQty*nCost2
      laData[14] = laData[14] + TotQty*nCost3
      laData[15] = laData[15] + TotQty*nCost4
      laData[16] = laData[16] + TotQty*nCost5
    ENDSCAN
    SELECT POSHDR
    =RLOCK()
    GATHER FROM laData FIELDS &lcScFields
    REPLACE PoTotal WITH laData[12]+laData[13]+laData[14]+laData[15]+laData[16]

    *B605270,1 KHM 01/08/2001 (Begin) Zero out the following fields when
    *B605270,1                deleting the cost sheet.
    REPLACE Pcs_Act WITH 0
    STORE 0 TO  laData[27]
    *B605270,1 KHM 01/08/2001 (End)

    UNLOCK
  CASE lcTranType = 'T'
    SELECT MMFGORDD
    =SEEK(laData[1])

    *E301235,4 WAB - filed name is cMfgOrdNo not cutTkt 
    *E301235,4 WAB - START
    *SCAN REST WHILE CutTKt = laData[1]
    SCAN REST WHILE cMfgOrdNo = laData[1]

    *E301235,4 WAB - END

      =SEEK(cFabric+Color,'Fabric')
      REPLACE nCost1 WITH Fabric.nMCost1 ,;
              nCost2 WITH Fabric.nMCost2 ,;
              nCost3 WITH Fabric.nMCost3 ,;
              nCost4 WITH Fabric.nMCost4
      laData[12] = laData[12] + nMfgTotQty*nCost1
      laData[13] = laData[13] + nMfgTotQty*nCost2
      laData[14] = laData[14] + nMfgTotQty*nCost3
      laData[15] = laData[15] + nMfgTotQty*nCost4
    ENDSCAN
    SELECT MMFGORDH
    =RLOCK()
    GATHER FROM laData FIELDS &lcScFields
    UNLOCK
ENDCASE  
WAIT CLEAR

*!*************************************************************
*! Name      : lfvZoom
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Zoom In/Zoom Out operation details browse
*!*************************************************************
*! Calls     : lfBrowLots
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvZoom()
*!*************************************************************
FUNCTION lfvZoom
llZoom = !llZoom 
IF llZoom 
  HIDE WINDOW (lcWinCh21) SAME
  SHOW GETS WINDOW (lcWinCh22) DISABLE ONLY
  HIDE WINDOW (lcWinCh22) SAME
  HIDE WINDOW (lcWinCh23) SAME  
  =lfBrowLots()
  SHOW GET pbZoom,1 PROMPT '\<Zoom Out'
ELSE
  HIDE WINDOW (lcBrZoom) SAME
  SHOW WINDOW (lcOprHdTit) REFRESH TOP
  SHOW WINDOW (lcWinCh22) TOP
  IF EOF(lcOprHdr) .OR. INLIST(laData[3],'S','X')
    SHOW GET ibOprHdr ENABLE
    SHOW GET ibOprDet ENABLE
  ELSE
    SHOW GETS WINDOW (lcWinCh22) ENABLE ONLY
  ENDIF  
  =lfBrowLots()
  
  *B603790,1 AMH Call the lfShOprHd function to refresh the new lot botton [Start]
  =lfShOprHd()
  *B603790,1 AMH [End]
  
  SHOW GET pbZoom,1 PROMPT '\<Zoom In'
ENDIF

*!*************************************************************
*! Name      : lfvToIss
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Show issued quantity
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvToIss()
*!*************************************************************
FUNCTION lfvToIss
PARAMETERS lcAction

IF UPDATED()
  IF lcTranType<>'T'
    REPLACE nTotToIss WITH nToIssue1+nToIssue2+nToIssue3+nToIssue4+nToIssue5+;
                           nToIssue6+nToIssue7+nToIssue8
  ENDIF
  IF lcAction = 'N'
    =lfRefresh('MFLOTS2')
    SHOW WINDOW (lcNewTit) REFRESH SAME
    IF (lcTranType='T' AND MAX(nTotBudget-nTotIss-nTotToIss,0) > 0) OR ;
       (lcTranType<>'T' AND ;
        MAX(nBudget1-nIssued1-nToIssue1,0)+MAX(nBudget2-nIssued2-nToIssue2,0)+;
        MAX(nBudget3-nIssued3-nToIssue3,0)+MAX(nBudget4-nIssued4-nToIssue4,0)+;
        MAX(nBudget5-nIssued5-nToIssue5,0)+MAX(nBudget6-nIssued6-nToIssue6,0)+;
        MAX(nBudget7-nIssued7-nToIssue7,0)+MAX(nBudget8-nIssued8-nToIssue8,0) > 0)
      SHOW GET lCanRemain ENABLE
    ELSE
      REPLACE lCanRemain WITH .F.
      SHOW GET lCanRemain DISABLE
    ENDIF
  ELSE
    =lfRefresh('MFMODLT2')
    SHOW WINDOW (lcModTit) REFRESH SAME
  ENDIF
ENDIF
*E301235,4 WAB - in case of material manifuctring order refresh the window 'mflosts3'
*E301235,4 WAB - START
IF lcTranType = 'T'
    =lfRefresh('MFLOTS3')
ENDIF
*E301235,4 WAB - END
*!*************************************************************
*! Name      : lfvContCode
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate contractor
*!*************************************************************
*! Calls     : gfApVnBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvContCode()
*!*************************************************************
FUNCTION lfvContCode
PRIVATE lnAlias
lnAlias = SELECT()
=gfOpenFile(gcDataDir+'ApVendor',gcDataDir+'VenCode','SH')
IF llBrowse .OR. (!EMPTY(lcContCode) AND !SEEK(lcContCode,'ApVendor'))
  *E301268,1 HDM [Start] filter vendor browse by sup. type
  *=gfApVnBrow(@lcContCode)
  =gfApVnBrow(@lcContCode,.F.,'C')
  *E301268,1 HDM [End]
ENDIF
lcContName = IIF(EMPTY(lcContCode),'',ApVendor.cVenComp)
SHOW GET lcContName DISABLE
llBrowse = .F.
=gfCloseFile('ApVendor')
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvModOpr
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Modify Operation
*!*************************************************************
*! Calls     : MFMODOPR.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvModOpr()
*!*************************************************************
FUNCTION lfvModOpr
PRIVATE lcContCode,lcOprComnt,llInHouse
SELECT (lcOprHdr)
lcContCode = cContCode
lcOprComnt = cOprComnt
llInHouse  = lInHouse
lcContName = cContName
cbActualize=.F.

*B602320,1 Update Sequence for operations have not been issued.
lcOperSeq = cOperSeq
lcModSeq  = IIF(SEEK(lcTranType+laData[1]+cOprCode,'MFGOPRDT'),'DISABLE','ENABLE')
*B602320,1 (End)

*B606030,1 AMH Restore the varible because its changed when open notepad screen [Start]
lcKey     = gcBmpHome + "ExtKey.BMP"
*B606030,1 AMH [End]

DO (gcScrDir+"MFMODOPR.SPX")

*E302004,1 AMH Restore the default value [Start]
STORE .F. TO llOnlyOne
*E302004,1 AMH [End]

*B802785,1 AMM Refresh the status
IF laData[3] = 'A'
  =lfRefresh(lcWinCh0)
ENDIF
*B802785,1 AMM end
*B602320,1 Update Sequence for operations have not been issued.
GO TOP IN (lcOprHdr)
lcFirstOpr = &lcOprHdr..cOprCode
=lfShOprHd()
*B602320,1 (End)

*!*************************************************************
*! Name      : lfvUpOpr
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Update Operation Modification
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvUpOpr()
*!*************************************************************
FUNCTION lfvUpOpr

IF SEEK(&lcOprHdr..cimtyp+&lcOprHdr..ctktno+&lcOprHdr..coprcode,'mfgoprhd')
  SELECT mfgoprhd
  REPLACE cContCode WITH lcContCode ,;
          cOprComnt WITH lcOprComnt ,;
          lInHouse  WITH llInHouse  ,;
          cContName WITH lcContName ,;
          cOperSeq  WITH lcOperSeq
ENDIF
SELECT (lcOprHdr)
REPLACE cContCode WITH lcContCode ,;
        cOprComnt WITH lcOprComnt ,;
        lInHouse  WITH llInHouse  ,;
        cContName WITH lcContName ,;
        cOperSeq  WITH lcOperSeq
=IIF(cbActualize,lfvActualize(),.T.)
*B802700,1 Reflect any modifications in operation sequence.[Start]
IF lcTranType='M'
  PRIVATE lnTktRecNo,lnBomRecNo,lnAlias
  lnAlias=SELECT(0)
  SELECT (lcTmpTkt)
  lnTktRecNo = RECNO()
  SELECT BOMLINE
  lnBomRecNo = RECNO()
*--- DOC.
*--- Seek and scan for operation and budget record [Trans type ='1']
*--- DOC.
  =SEEK('M'+'1'+laData[1])
  SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+;
                  iclr+mfgcode  = 'M'+'1'+laData[1] FOR !lVoid
    lcOprCode = IIF(EMPTY(BomLine.cOprCode),BomLine.MfgCode,BomLine.cOprCode)
    IF !EMPTY(lcOprCode) AND SEEK(lcTranType+laData[1]+lcOprCode,'MFGOPRHD');
       AND SEEK(BomLine.Style+BomLine.SClr+STR(BomLine.LineNo,6),lcTmpTkt)
       SELECT (lcTmpTkt)
       
*B802700,1 Get the smallest op seq[Start] 
       *lcFrstOpr   = IIF(nFrstOprSq=0 OR (VAL(MFGOPRHD.cOperSeq) < nFrstOprSq),MFGOPRHD.cOprCode,cFrstOpr)
       *lnFrstOprSq = IIF(nFrstOprSq=0 OR (VAL(MFGOPRHD.cOperSeq) < nFrstOprSq),VAL(MFGOPRHD.cOperSeq),nFrstOprSq)
        SELECT MIN(cOperSeq) FROM MFGOPRHD WHERE cimtyp+ctktno+coprcode="M"+LADATA[1] INTO ARRAY laTmp1      
        IF _TALLY > 0
          SELECT coprCode FROM MFGOPRHD  WHERE cimtyp+ctktno+coprcode="M"+LADATA[1] AND cOperSeq = laTmp1[1] INTO ARRAY laTmp2            
          lcFrstOpr = laTmp2[1]
          lnFrstOprSq = VAL(laTmp1[1])                

          REPLACE cFrstOpr   WITH lcFrstOpr ,;
                 nFrstOprSq WITH lnFrstOprSq
        ENDIF       
*B802700,1 Get the smallest op seq[End..] 
    ENDIF
  ENDSCAN
  
  IF BETWEEN(lnBomRecNo,RECCOUNT('BOMLINE'),1)
    GO lnRecNo IN BOMLINE
  ENDIF
  IF BETWEEN(lnTktRecNo,RECCOUNT(lcTmpTkt),1)
    GO lnRecNo IN (lcTmpTkt)
  ENDIF
  SELECT (lnAlias)
ENDIF
*B802700,1 Reflect any modifications in operation sequence.[End..]

*!*************************************************************
*! Name      : lfDMainLot
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Deactivate main screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfDMainLot()
*!*************************************************************
FUNCTION lfDMainLot
IF WONTOP()=lcRcvTit   .OR. WONTOP()=lcNewTit    .OR. WONTOP()=lcModTit   .OR. ;
   WONTOP()=lcIssLtTit .OR. WONTOP()=lcDistrbTit .OR. WONTOP()=lcLotTitle .OR. ;
   WONTOP()=lcIssLog   .OR. WONTOP()=lcConCnTit  .OR. WONTOP()= lcRollTit .OR. ;
   WONTOP()=lcAsRollTit
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  IF WONTOP()=lcRollTit .OR. WONTOP()=lcAsRollTit
    ON KEY LABEL ENTER DO lfMainRoll
  ENDIF  
  DO CASE
    CASE WONTOP()=lcRcvTit
      ON KEY LABEL TAB     DO lpTab WITH 'MFRCVLT2','pbSelect'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'MFRCVLT0',IIF(lcAction='2',IIF(laData[28]='M','rbLotType','puToOpr'),'puLots')
    CASE WONTOP()=lcNewTit 
      *E301235,4 WAB - Trap of tab key. with screen mflot3 in case of material
      *ON KEY LABEL TAB     DO lpTab WITH 'MFLOTS2','pbSelect'
      IF lcTranType = 'T'
        ON KEY LABEL TAB     DO lpTab WITH 'MFLOTS3','pbSelect'
      ELSE
        ON KEY LABEL TAB     DO lpTab WITH 'MFLOTS2','pbSelect'
      ENDIF
      *E301235,4 WAB - END
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'MFLOTS0','lcContCode'
    CASE WONTOP()=lcModTit
      ON KEY LABEL TAB     DO lpTab WITH 'MFMODLT2','pbAddItem'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'MFMODLT0','lcContCode'
    CASE WONTOP()=lcIssLtTit
      ON KEY LABEL TAB     DO lpTab WITH 'MFISSLT2','pbISelect'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'MFISSLT0','puCstTypes'
    CASE WONTOP()=lcDistrbTit
      ON KEY LABEL TAB     DO lpTab WITH 'MFDSTRB1','pbOk'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'MFDSTRB1','pbOk'
    CASE WONTOP()=lcLotTitle  
      ON KEY LABEL TAB     DO lpTab WITH 'MFOPLOT2','pbIssue'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'MFOPLOT0','lnIssWare'
    CASE WONTOP()=lcIssLog
      ON KEY LABEL TAB     DO lpTab WITH 'MFISLOG2','pbIssue'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'MFISLOG2','pbClose'
    CASE WONTOP()=lcConCnTit
      ON KEY LABEL TAB     DO lpTab WITH 'MFCONCN2','pbOK'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'MFCONCN2','pbCancel'
    CASE WONTOP()=lcRollTit
      ON KEY LABEL TAB     DO lpTab WITH 'MFMAROL2','lcRollId'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'MFMAROL2','pbOkRol'
    CASE WONTOP()=lcAsRollTit
      ON KEY LABEL TAB     DO lpTab WITH 'MFMAROL2','lcRRollID'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'MFMAROL2','lcRollId'
  ENDCASE
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfvIssCstItm
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Issue lot cost items
*!*************************************************************
*! Calls     : lfLotCstItm(),MFISSLT.SPX
*!*************************************************************
*! Parameters: lcOprCode : Operation
*!             lcLotNo   : Lot#
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvIssCstItm()
*!*************************************************************
FUNCTION lfvIssCstItm
PARAMETERS lcOprCode,lcLotNo
PRIVATE laCstTypes,puCstTypes,lnAlias,lnRecNo

lnAlias = SELECT()
lnRecNo = RECNO()

*E301077,55 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'BOMCOST',gcDataDir+'BOMCOST','SH')
=gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
=gfOpenFile(gcDataDir+'FABDYE',gcDataDir+'FABDYE','SH')
=gfOpenFile(gcDataDir+'STYINVJL',gcDataDir+'STYINVJL','SH')
IF 'MA' $ gcComp_mdl
  =gfOpenFile(gcDataDir+'MATINVJL',gcDataDir+'MATINVJL','SH')
ENDIF  
IF laSetups[9,2]='L'

  *B604073,1 KHM 12/25/2000 (Begin) Changing the Index
  *USE (gcDataDir+'MATINVJL') AGAIN ALIAS FABINV ORDER TAG Mtinvseq IN 0
  USE (gcDataDir+'MATINVJL') AGAIN ALIAS FABINV ORDER TAG MatInvJl IN 0
  *B604073,1 KHM 12/25/2000 (End)
  
  =gfOpenFile(gcDataDir+'POFHDR',gcDataDir+'POFHDR','SH')
ENDIF
*E301077,55 (End)
*--- DOC.
*--- Get the the 5 cost element types and labels
*--- DOC.
puCstTypes = 1
IF lcTranType='T'
  DECLARE laCstTypes[5]
  laCstTypes[1] = 'All'
  laCstTypes[2] = laSetups[3,2]
  laCstTypes[3] = laSetups[4,2]
  laCstTypes[4] = laSetups[5,2]
  laCstTypes[5] = laSetups[6,2]
ELSE
  DECLARE laCstTypes[6]
  laCstTypes[1] = 'All'
  laCstTypes[2] = laSetups[3,2]
  laCstTypes[3] = laSetups[4,2]
  laCstTypes[4] = laSetups[5,2]
  laCstTypes[5] = laSetups[6,2]
  laCstTypes[6] = laSetups[7,2]
ENDIF
*E301077,55 Inhance openning files to speed up transaction
*SELECT (lcIssLtFile)
*ZAP
SELECT CTKTBOM
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+3,4]
laFileStru[lnFileStru+1,1] = 'cSelect'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'nIssue'
laFileStru[lnFileStru+2,2] = 'N'
laFileStru[lnFileStru+2,3] = 12
laFileStru[lnFileStru+2,4] = 3
laFileStru[lnFileStru+3,1] = 'nIssPcnt'
laFileStru[lnFileStru+3,2] = 'N'
laFileStru[lnFileStru+3,3] = 6
laFileStru[lnFileStru+3,4] = 2
CREATE TABLE (gcWorkDir+lcIssLtFile) FROM ARRAY laFileStru
INDEX ON TYP+ITEM+ICLR+MFGCODE+DYELOT TAG (lcIssLtFile) 
*E301077,55 (End)

IF EMPTY(lcOprCode) .AND. EMPTY(lcLotNo)
  SELECT (lcTktSheet)
  lnRecNo = RECNO()
  SCAN FOR cShowType+cOprCode='1'+SPACE(6)
    SCATTER MEMVAR

    INSERT INTO (lcIssLtFile) FROM MEMVAR

    *B603861,1 AMH [Start] Get the item description in case of P.Price and Duty.
    IF EMPTY(&lcIssLtFile..item)      
      REPLACE &lcIssLtFile..item WITH lfLblDesc(laSetups[2+INT(VAL(&lcIssLtFile..Typ)),2])
    ENDIF
    *B603861,1 [End]
    
  ENDSCAN
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GO lnRecNo
  ENDIF
ELSE
*--- DOC.
*--- Function to issue operation from lot
*--- DOC.
  =lfLotCstItm(lcOprCode,lcLotNo)
ENDIF  
spIssPcnt = 100.00
lcFound   = 'ENABLE'
ldIssDate = gdSysDate
SELECT (lcIssLtFile)
SET KEY TO
GO TOP
IF EOF()
  IF EMPTY(lcOprCode) .AND. EMPTY(lcLotNo)
  ELSE
    *E300725,1 Message : 38118
    *E300725,1 No cost items found for operation xxxxxxxxxx Lot# 99
    *E300725,1 Button : 00000
    *E300725,1 Ok
    =gfModalGen('TRM38118B00000','ALERT',ALLTRIM(gfCodDes(lcOprCode,'MfgCode'))+'|'+lcLotNo)
  ENDIF  
ELSE
  DO (gcScrDir+"MFISSLT.SPX") WITH lcOprCode,lcLotNo
ENDIF
*E301077,55 Inhance openning files to speed up transaction
IF laSetups[9,2]='L'
  USE IN FABINV
  =gfCloseFile('POFHDR')
ENDIF
*E301077,55 (End)

SELECT (lnAlias)
*B603861,1 AMH check if [Start]
*GO lnRecNo
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF
*B603861,1 AMH check if [End  ]
IF lnActFolder=1
  =lfwBrow()
ENDIF
IF lnActFolder=2
  =lfShOprDt()
ENDIF

*!*************************************************************
*! Name      : lfLotCstItm
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Get lot cost items
*!*************************************************************
*! Calls     : gfModalGen(),gpAdFabWar(),gpAdStyWar()
*!*************************************************************
*! Parameters: lcOprCode : Operation
*!             lcLotNo   : Lot#
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfLotCstItm()
*!*************************************************************
FUNCTION lfLotCstItm
PARAMETERS lcOprCode, lcLotNo
*B603867,1 RAMY 31/08/2000 Add new private variable.
*PRIVATE lnAlias,lnSumRecNo,lnDetRecNo,lnOprRecNo
PRIVATE lnAlias,lnSumRecNo,lnDetRecNo,lnOprRecNo,llStyDye
*B603867,1 RAMY END]
lnAlias = SELECT()
lnSumRecNo= RECNO(lcTktSheet)
lnDetRecNo= RECNO(lcDetFile)
lnOprRecNo= RECNO(lcOprDet)
SET ORDER TO TAG 'Lot' IN (lcOprDet)
SET ORDER TO TAG 'Mfgopr'   IN 'STYINVJL'
IF 'MA' $ gcComp_mdl
  SET ORDER TO TAG 'Mfgopr'   IN 'MATINVJL'
ENDIF  
SET ORDER TO TAG 'Pobomcls' IN 'BOMCOST'
SELECT (lcIssLtFile)
ZAP
SELECT (lcTktSheet)
*--- DOC.
*--- Scan for the specific operation records.
*--- DOC.
SCAN FOR cShowType = '1' .AND. (cOprCode = lcOprCode OR MfgCode = lcOprCode)
  SCATTER MEMVAR
  *--- DOC.
  *--- If mfg operatoin get the elated field GL - Account
  *--- DOC.
  IF m.cCatgTyp='M' AND ;
    (!gfRltFld(m.MfgCode,@laMfgRFld,'MFGCODE') OR EMPTY(lcMfgGlAcnt))
    LOOP
  ENDIF
  SELECT (lcOprDet)
  *--- DOC.
  *--- Scan for the budget record in the operation detail file
  *--- DOC.
  =SEEK(lcTranType+laData[1]+lcOprCode+lcLotNo)
  SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
                  lcTranType+laData[1]+lcOprCode+lcLotNo FOR TranCd='1'
    lcItem = Item+Color
    llStyDye = IIF(SEEK(lcItem,'Style'),cDye_Flg='Y',.F.)
    SELECT (lcDetFile)
    =SEEK(lcItem)
    m.Pieces = 0
    *--- DOC.
    *--- Get all the record for current item and operation
    *--- DOC.
    SCAN REST WHILE Style+SClr+STR(LineNo,6)+cBomTyp+cShowType+Item+IClr+MfgCode=lcItem ;
              FOR cBomTyp+cShowType+Item+IClr+MfgCode = m.Typ+'1'+m.Item+m.IClr+m.MfgCode

      *E301235,4 WAB - no scale if its called from material manufactring order cost sheet
      *E301235,4 WAB - so we must calculate the total lot qty. 
      *E301235,4 WAB - START 
      *FOR lnCount = 1 TO 8
      *  lcCount = STR(lnCount,1)
      *  IF lcCount $ cSizes
      *    m.Pieces=m.Pieces+&lcOprDet..nLotQty&lcCount
      *  ENDIF
      *ENDFOR      

      IF lcTranType = 'T'
        m.Pieces = m.Pieces + &lcOprDet..nLotTotQty
      ElSE
        FOR lnCount = 1 TO 8
          lcCount = STR(lnCount,1)
          IF lcCount $ cSizes
            m.Pieces=m.Pieces+&lcOprDet..nLotQty&lcCount
          ENDIF
        ENDFOR
      ENDIF

      *E301235,4 WAB - END

    ENDSCAN          
    IF m.Pieces > 0

      IF !SEEK(m.Typ+m.Item+m.IClr+m.MfgCode+m.Dyelot,lcIssLtFile)
        m.Req_Qty  = m.Pieces*m.UntQty
        m.Est_Cost = m.Req_Qty*m.UntCost

        *B802542,1 - WAB - clalculate  also m.req_qty? to issue the actual qty (case style comp.)
        *B802542,1 - WAB - START
        FOR lnI = 1 TO 8
          lcI = ALLTRIM(STR(lnI))
          m.Req_Qty&lcI =  &lcOprDet..nLotQty&lcI * m.UntQty
        ENDFOR
        *B802542,1 - WAB - END
        

        *B802577,1 WAB - save the total required qty to lnoldpcs
        *B802577,1 WAB - get total required qty from req_qty1,req_qty2......
        *B802577,1 WAB - if there is no sizes return to old value
        *B802577,1 WAB - get total required qty from nqty1 from cut tkte temp table
        *B802577,1 WAB - in case dyelot get required qty from detail file .
        *B802577,1 WAB - ther are more than on line for same fabric        
        *B802577,1 WAB - START
        
        *B605677,1 KHM 03/19/2002 (Begin) Getting the selected sizes
        SELECT (lcDetFile)
        =SEEK(lcItem)
        LOCATE REST WHILE Style+SClr+STR(LineNo,6)+cBomTyp+cShowType+Item+IClr+MfgCode=lcItem ;
               FOR cBomTyp+cShowType+Item+IClr+MfgCode = m.Typ+'1'+m.Item+m.IClr+m.MfgCode
        lcSizeQty = IIF(FOUND(),cSizes,'12345678')
        lcSizeQty = IIF(EMPTY(lcSizeQty),'12345678',lcSizeQty)
        *B605677,1 KHM 03/19/2002 (End)
        
        lnoldpcs = 0
        lnoldpcs = m.Req_Qty 
        m.Req_Qty = 0  
        FOR lnI = 1 TO 8
          *--B603867,1 RAMY [START]
          IF !llStyDye
          *--B603867,1 RAMY [END]
              lcI = ALLTRIM(STR(lnI))

            *B605677,1 KHM 03/19/2002 (Begin) Check if the sizes is selected or not.
            IF lcI $ lcSizeQty
            *B605677,1 KHM 03/19/2002 (End)
            
              m.Req_Qty =  m.Req_Qty + m.Req_Qty&lcI
            *--B603867,1 RAMY [START]
            
            *B605677,1 KHM 03/19/2002 (Begin)
            ENDIF
            *B605677,1 KHM 03/19/2002 (End)  
            
          ENDIF
          *--B603867,1 RAMY [END]
        ENDFOR
        IF m.Req_qty = 0
          m.Req_Qty = lnOldPcs
        ELSE
          m.Est_Cost = m.Req_Qty*m.UntCost
          m.Pieces = m.Req_Qty/m.UntQty
        ENDIF
        IF !EMPTY(m.DYELOT) 
          SELECT (lcDetFile)
          m.Pieces = 0  
          SCAN FOR Style = &lcOprDet..Item  AND Item = m.item AND Dyelot = m.Dyelot
            m.Pieces =  m.Pieces + styqty
          ENDSCAN
          m.Req_Qty  = m.Pieces*m.UntQty
          m.Est_Cost = m.Req_Qty*m.UntCost
        ENDIF
     
       *B802577,1 WAB - END

        DO CASE 
          CASE m.cCatgTyp = 'S'
            SELECT StyInvJl
            =SEEK(laData[1]+lcOprCode+lcLotNo+'1'+m.Item)
            *--- DOC.
            *--- Sum all the issued record to variables m.Used with -ve signe
            *--- DOC.
            SUM REST -nStk1,-nStk2,-nStk3,-nStk4,-nStk5,-nStk6,-nStk7,-nStk8,-nTotStk,;
                     IIF(cIRType='I',-nStk1,0),IIF(cIRType='I',-nStk2,0),;
                     IIF(cIRType='I',-nStk3,0),IIF(cIRType='I',-nStk4,0),;
                     IIF(cIRType='I',-nStk5,0),IIF(cIRType='I',-nStk6,0),;
                     IIF(cIRType='I',-nStk7,0),IIF(cIRType='I',-nStk8,0),;
                     IIF(cIRType='I',-nTotStk,0) ;
            TO    m.Used_Qty1,m.Used_Qty2,m.Used_Qty3,m.Used_Qty4,m.Used_Qty5,;
                  m.Used_Qty6,m.Used_Qty7,m.Used_Qty8,m.Used_Qty,m.Iss_Qty1,;
                  m.Iss_Qty2,m.Iss_Qty3,m.Iss_Qty4,m.Iss_Qty5,m.Iss_Qty6,m.Iss_Qty7,;
                  m.Iss_Qty8,m.Issue_Qty ;
            WHILE ctrcode+coprcode+clotno+ctrtype+style+cwarecode = ;
                  laData[1]+lcOprCode+lcLotNo+'1'+m.Item

            *E301248 (Start)
            PRIVATE Used_QtyI,Iss_QtyI
            DIMENSION Used_QtyI[9],Iss_QtyI[9]
            =SEEK(laData[1]+lcOprCode+lcLotNo+'I'+m.Item)
            SUM REST -nStk1,-nStk2,-nStk3,-nStk4,-nStk5,-nStk6,-nStk7,-nStk8,-nTotStk,;
                     IIF(cIRType='I',-nStk1,0),IIF(cIRType='I',-nStk2,0),;
                     IIF(cIRType='I',-nStk3,0),IIF(cIRType='I',-nStk4,0),;
                     IIF(cIRType='I',-nStk5,0),IIF(cIRType='I',-nStk6,0),;
                     IIF(cIRType='I',-nStk7,0),IIF(cIRType='I',-nStk8,0),;
                     IIF(cIRType='I',-nTotStk,0) ;
            TO    Used_QtyI[1],Used_QtyI[2],Used_QtyI[3],Used_QtyI[4],;
                  Used_QtyI[5],Used_QtyI[6],Used_QtyI[7],Used_QtyI[8],Used_QtyI[9],;
                  Iss_QtyI[1] ,Iss_QtyI[2] ,Iss_QtyI[3] ,Iss_QtyI[4] ,;
                  Iss_QtyI[5] ,Iss_QtyI[6] ,Iss_QtyI[7] ,Iss_QtyI[8] ,Iss_QtyI[9] ;
            WHILE ctrcode+coprcode+clotno+ctrtype+style+cwarecode = ;
                  laData[1]+lcOprCode+lcLotNo+'I'+m.Item

            m.Used_Qty1 = m.Used_Qty1 + Used_QtyI[1]
            m.Used_Qty2 = m.Used_Qty2 + Used_QtyI[2]
            m.Used_Qty3 = m.Used_Qty3 + Used_QtyI[3]
            m.Used_Qty4 = m.Used_Qty4 + Used_QtyI[4]
            m.Used_Qty5 = m.Used_Qty5 + Used_QtyI[5]
            m.Used_Qty6 = m.Used_Qty6 + Used_QtyI[6]
            m.Used_Qty7 = m.Used_Qty7 + Used_QtyI[7]
            m.Used_Qty8 = m.Used_Qty8 + Used_QtyI[8]
            m.Used_Qty  = m.Used_Qty  + Used_QtyI[9]

            m.Iss_Qty1  = m.Iss_Qty1  + Iss_QtyI[1]
            m.Iss_Qty2  = m.Iss_Qty2  + Iss_QtyI[2]
            m.Iss_Qty3  = m.Iss_Qty3  + Iss_QtyI[3]
            m.Iss_Qty4  = m.Iss_Qty4  + Iss_QtyI[4]
            m.Iss_Qty5  = m.Iss_Qty5  + Iss_QtyI[5]
            m.Iss_Qty6  = m.Iss_Qty6  + Iss_QtyI[6]
            m.Iss_Qty7  = m.Iss_Qty7  + Iss_QtyI[7]
            m.Iss_Qty8  = m.Iss_Qty8  + Iss_QtyI[8]
            m.Issue_Qty = m.Issue_Qty + Iss_QtyI[9]
            *E301248 (End)
           
          CASE m.cCatgTyp='F' .OR. (m.cCatgTyp='T' .AND. m.Trim_Invt)
            SELECT MatInvJl
            *--- DOC.
            *--- Get issues qty from materail inventory journal
            *--- for transtype = '4'
            *--- DOC.
            =SEEK(laData[1]+lcOprCode+lcLotNo+'4'+SUBSTR(m.Item,1,7)+m.IClr)
            *B802475,1 AMM Fix the bug in calculation of used quantity
            *SUM REST nIssued,IIF(nIssued>0,nIssued,0) TO m.Used_Qty,m.Issue_Qty ;
            WHILE ctran+coprcode+clotno+ctrantype+cfabric+ccolor+cwarecode =;
                  laData[1]+lcOprCode+lcLotNo+'4'+SUBSTR(m.Item,1,7)+m.IClr
            SUM REST nIssued-nReceived,IIF(nIssued>0,nIssued,0) TO m.Used_Qty,m.Issue_Qty ;
            WHILE ctran+coprcode+clotno+ctrantype+cfabric+ccolor+cwarecode =;
                  laData[1]+lcOprCode+lcLotNo+'4'+SUBSTR(m.Item,1,7)+m.IClr
            *B802475,1 AMM end
          OTHERWISE
            SELECT BOMCOST

            *B605481,1 KHM 02/17/2002 (Begin) Changing the replacement of Actualize
            *B605481,1                to "N" instead of "Y".
            *=SEEK(lcTranType+laData[1]+'Y'+m.Typ+m.Item+m.IClr+m.MfgCode+lcOprCode+lcLotNo)
            *SUM REST nTotQty,IIF(nTotQty>0,nTotQty,0) TO m.Used_Qty,m.Issue_Qty ;
            WHILE cimtyp+ctktno+actualize+cbomtype+item+iclr+mfgcode+coprcode+clotno+cisession+crsession=;
                  lcTranType+laData[1]+'Y'+m.Typ+m.Item+m.IClr+m.MfgCode+lcOprCode+lcLotNo
            
            =SEEK(lcTranType+laData[1]+'N'+m.Typ+m.Item+m.IClr+m.MfgCode+lcOprCode+lcLotNo)
            SUM REST nTotQty,IIF(nTotQty>0,nTotQty,0) TO m.Used_Qty,m.Issue_Qty ;
            WHILE cimtyp+ctktno+actualize+cbomtype+item+iclr+mfgcode+coprcode+clotno+cisession+crsession=;
                  lcTranType+laData[1]+'N'+m.Typ+m.Item+m.IClr+m.MfgCode+lcOprCode+lcLotNo
            *B605481,1 KHM 02/17/2002 (End)
            
        ENDCASE
        INSERT INTO (lcIssLtFile) FROM MEMVAR
      ELSE
        SELECT (lcIssLtFile)
        *--B603867,1 RAMY [START]
        *REPLACE Pieces   WITH Pieces   + m.Pieces ,;
                Req_Qty  WITH Req_Qty  + m.Pieces*m.UntQty ,;
                Est_Cost WITH Est_Cost + m.Pieces*m.UntQty*m.UntCost
      
        REPLACE Pieces   WITH IIF(llStyDye , Pieces   , Pieces   + m.Pieces) ,;
                Req_Qty  WITH IIF(llStyDye , Req_Qty  , Req_Qty  + m.Pieces*m.UntQty) ,;
                Est_Cost WITH IIF(llStyDye , Est_Cost , Est_Cost + m.Pieces*m.UntQty*m.UntCost)
        *--B603867,1 RAMY [END]
      ENDIF  
    ENDIF
  ENDSCAN
ENDSCAN
IF BETWEEN(lnSumRecNo,1,RECCOUNT(lcTktSheet))
  GOTO lnSumRecNo IN (lcTktSheet)
ENDIF
IF BETWEEN(lnDetRecNo,1,RECCOUNT(lcDetFile))
  GOTO lnDetRecNo IN (lcDetFile)
ENDIF
IF BETWEEN(lnOprRecNo,1,RECCOUNT(lcOprDet))
  GOTO lnOprRecNo IN (lcOprDet)
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfBrowIsLt
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Browse lot cost items
*!*************************************************************
*! Calls     : lfShIssLot()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfBrowIsLt()
*!*************************************************************
FUNCTION lfBrowIsLt
PRIVATE lcBrowFild

SELECT (lcIssLtFile)
lnIMarker = RECNO()
lcBrowFild = ;
       "cMarker =IIF(RECNO()=lnIMarker ,'>',' '):H=' ':R:1:W=.F.,"+;
       "cSelect :H= ' ' :R,"+;
       "cItem =IIF(cCatgTyp='M',gfCodDes(MfgCode,'MfgCode'),Item+ALLTRIM(IClr)) :H='Item' :R,"+;
       IIF(llUseDyelot,"Dyelot :H='Dyelot' :R,","")+;
       "Uom       :H='UOM'     :R,"+;
       "Req_Qty   :H='Required':R,"+;
       "Issue_Qty :H='Issued'  :R,"+;
       "Used_Qty  :H='Used'    :R,"+;
       "nIssue    :H='Issue' :W= lfwIssQTy() :V= lfvIssQTy() ,"+;
       "nIssPcnt  :H='Issue Prcnt.' :W= lfwIssPcnt() :V= lfvBIsspcnt(),"+;
       "UntQty    :H='Units'   :R,"+;
       "Pieces    :H='Pieces'  :R"
BROWSE FIELDS &lcBrowFild ;
       WINDOW MFISSLT1   ;
       IN WINDOW MFISSLT ;
       NOMENU            ;
       NOAPPEND          ;
       NODELETE          ;
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       WHEN lfShIssLot() ;
       TITLE lcIssLtTit
=lfShIssLot()

*!*************************************************************
*! Name      : lfAdjiButt
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Select, Unselect, Select all, Select None and Invert lot cost items
*!*************************************************************
*! Calls     : lfShIssLot()
*!*************************************************************
*! Parameters: lcType  'S' : Select
*!                     'A' : Select All
*!                     'N' : Select None
*!                     'V' : Invert
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfAdjiButt()
*!*************************************************************
FUNCTION lfAdjiButt
PARAMETERS lcType
PRIVATE lnRecNo,lcDataSel,lcDataUSel

SELECT (lcIssLtFile)
lnRecNo = RECNO()
DO CASE 
  CASE lcType = 'S'
    *E301052,1 Allow Issuing more than Required Quantity
    *REPLACE cSelect  WITH IIF(cSelect=SPACE(1) AND REQ_QTY-Used_Qty>0 AND lfChkDye(),"»",SPACE(1)) ,;
            nIssue   WITH IIF(cSelect=SPACE(1),0,spIssPcnt*(REQ_QTY-Used_Qty)/100) ,;
            nIssPcnt WITH IIF(cSelect=SPACE(1),0,spIssPcnt)
    REPLACE cSelect  WITH IIF(cSelect=SPACE(1) AND lfChkDye(),"»",SPACE(1)) ,;
            nIssue   WITH IIF(cSelect=SPACE(1),0,spIssPcnt*MAX(REQ_QTY-Used_Qty,0)/100) ,;
            nIssPcnt WITH IIF(cSelect=SPACE(1),0,spIssPcnt)

  CASE lcType = 'A'
    *E301052,1 Allow Issuing more than Required Quantity
    *REPLACE ALL cSelect  WITH IIF(REQ_QTY-Used_Qty>0 AND lfChkDye(),"»",SPACE(1)) ,;
                nIssue   WITH IIF(cSelect=SPACE(1),0,spIssPcnt*(REQ_QTY-Used_Qty)/100) ,;
                nIssPcnt WITH IIF(cSelect=SPACE(1),0,spIssPcnt)
    REPLACE ALL cSelect  WITH IIF(lfChkDye(),"»",SPACE(1)) ,;
                nIssue   WITH IIF(cSelect=SPACE(1),0,spIssPcnt*MAX(REQ_QTY-Used_Qty,0)/100) ,;
                nIssPcnt WITH IIF(cSelect=SPACE(1),0,spIssPcnt)
  CASE lcType = 'N'
    REPLACE ALL cSelect  WITH SPACE(1) ,;
                nIssue   WITH 0 ,;
                nIssPcnt WITH 0
  CASE lcType = 'V'
    *E301052,1 Allow Issuing more than Required Quantity
    *REPLACE ALL cSelect  WITH IIF(cSelect=SPACE(1) AND REQ_QTY-Used_Qty>0  AND lfChkDye(),"»",SPACE(1)) ,;
                nIssue   WITH IIF(cSelect=SPACE(1),0,spIssPcnt*(REQ_QTY-Used_Qty)/100) ,;
                nIssPcnt WITH IIF(cSelect=SPACE(1),0,spIssPcnt)
    REPLACE ALL cSelect  WITH IIF(cSelect=SPACE(1) AND lfChkDye(),"»",SPACE(1)) ,;
                nIssue   WITH IIF(cSelect=SPACE(1),0,spIssPcnt*MAX(REQ_QTY-Used_Qty,0)/100) ,;
                nIssPcnt WITH IIF(cSelect=SPACE(1),0,spIssPcnt)
  OTHERWISE
ENDCASE
LOCATE
lcFound = IIF(FOUND(),'ENABLE','DISABLE')
LOCATE FOR EMPTY(cSelect)
lcDataUSel = IIF(FOUND(),'ENABLE','DISABLE')
LOCATE FOR !EMPTY(cSelect)
lcDataSel  = IIF(FOUND(),'ENABLE','DISABLE')
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF 
IF EMPTY(cSelect)
  SHOW GET pbISelect,1 PROMPT '\<Select'   &lcFound
ELSE
  SHOW GET pbISelect,1 PROMPT 'Un\<Select' &lcFound
ENDIF
SHOW GET pbISelAll  &lcDataUSel
SHOW GET pbISelNone &lcDataSel
SHOW GET pbIInvert  &lcFound
SHOW GET spIssPcnt  &lcDataSel
=lfShIssLot()

*!*************************************************************
*! Name      : lfChkDye
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Check Dyelots for Styles and Fabrics selected for 
*!             automatic issue
*!*************************************************************
*! Calls     : SDYEBROW(),FDYEBROW()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfChkDye()
*!*************************************************************
FUNCTION lfChkDye
*--- DOC.
*--- Check if the system use dyelot then update the field Dyelot 
*--- DOC.
DO CASE
  CASE cCatgTyp = 'S'
    IF laSetups[8,2]='Y' AND SEEK(Item,'Style') AND Style.cDye_Flg='Y' AND ;
      ( EMPTY(Dyelot) OR !SEEK(Item+cWareCode+Dyelot,'STYDYE') )
      lcDyelot = IIF(EMPTY(Dyelot),'?',Dyelot)
      IF !SDYEBROW(Item, @lcDyelot, .T.,cWareCode)
        RETURN(.F.)
      ENDIF
      REPLACE Dyelot WITH lcDyelot
    ENDIF
  CASE cCatgTyp = 'F' OR (cCatgTyp = 'T' AND Trim_Invt)
    IF laSetups[12,2]='Y' AND SEEK(SUBSTR(Item,1,7)+IClr,'Fabric') AND ;
       Fabric.cDye_Flg='Y' AND ( EMPTY(Dyelot) OR ;
       !SEEK(SUBSTR(Item,1,7)+IClr+cWareCode+Dyelot,'FABDYE')  )
      lcDyelot = IIF(EMPTY(Dyelot),'?',Dyelot)
      IF !FDYEBROW(SUBSTR(Item,1,7),IClr,@lcDyelot,.T.,cWareCode) OR EMPTY(lcDyelot)
        RETURN(.F.)
      ENDIF   
      REPLACE Dyelot WITH lcDyelot
    ENDIF
ENDCASE

*!*************************************************************
*! Name      : lfwIssQTy
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : When function for lot cost item issued quantity
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfwIssQTy()
*!*************************************************************
FUNCTION lfwIssQTy
IF EMPTY(cSelect) .OR. cCatgTyp='S'
  RETURN .F.
ENDIF
lcOldValue = nIssue

*!*************************************************************
*! Name      : lfvIssQTy
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Valid function for lot cost item issued quantity
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvIssQTy()
*!*************************************************************
FUNCTION lfvIssQty

*E301052,1 Allow Issuing more than Required Quantity
*IF nIssue > Req_Qty-Used_Qty
*  REPLACE nIssue WITH lcOldValue
*  RETURN
*ENDIF
REPLACE nIssPcnt WITH IIF(Req_Qty-Used_Qty<=0,0,nIssue*100/(Req_Qty-Used_Qty))

*!*************************************************************
*! Name      : lfwIssPcnt
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : When function for lot cost item issued quantity percent
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfwIssPcnt()
*!*************************************************************
FUNCTION lfwIssPcnt
IF EMPTY(cSelect)
  RETURN .F.
ENDIF
lcOldValue = nIssPcnt

*!*************************************************************
*! Name      : lfvBIsspcnt
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Valid function for lot cost item issued quantity percent
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvBIsspcnt()
*!*************************************************************
FUNCTION lfvBIsspcnt

*E301052,1 Allow Issuing more than Required Quantity
*IF nIssPcnt > 100
*  REPLACE nIssPcnt WITH lcOldValue
*  RETURN
*ENDIF
REPLACE nIssue WITH MAX(Req_Qty-Used_Qty,0)*nIssPcnt/100

*!*************************************************************
*! Name      : lfShIssLot
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Show lot cost items
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfShIssLot()
*!*************************************************************
FUNCTION lfShIssLot
lnIMarker = RECNO(lcIssLtFile)
SHOW WINDOW (lcIssLtTit) REFRESH SAME
IF EMPTY(cSelect)
  SHOW GET pbISelect,1 PROMPT '\<Select'  &lcFound
ELSE
  SHOW GET pbISelect,1 PROMPT 'Un\<Select' &lcFound
ENDIF

*!*************************************************************
*! Name      : lfvOkIssLt
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Issue lot cost items
*!*************************************************************
*! Calls     : CHECKPRD(),lfIssRetFab(),lfIssRetSty(),lfIssRetMfg()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvOkIssLt()
*!*************************************************************
FUNCTION lfvOkIssLt
PRIVATE laIssQty

SELECT (lcIssLtFile)
LOCATE FOR !EMPTY(cSelect)
IF FOUND() AND !CHECKPRD(ldIssDate,'lcGlYear','lcGlPeriod','',.F.)
  RETURN
ENDIF
*B603861,1 AMH 09/12/2000 Flage of Message 34167 [Start]
llMessage = .F.
lnSelCount = 0
SCAN FOR !EMPTY(cSelect)
  lnSelCount = lnSelCount + 1
ENDSCAN
*B603861,1 AMH 09/12/2000 Flage of Message 34167 [End  ]

SCAN FOR !EMPTY(cSelect)
  DO CASE
    CASE INLIST(cCatgTyp,'F','T') .AND. SEEK(SUBSTR(Item,1,7)+IClr,'FABRIC')
      =SEEK(SUBSTR(Item,1,7)+IClr+cWareCode+SPACE(10),'FABDYE')
      llItemDye = laSetups[12,2]='Y' .AND. Fabric.cDye_Flg = 'Y'
      DO CASE
        CASE laSetups[9,2]='L'
          lnOMarker = 0
          SCATTER MEMVAR
          lcIssWare = laData[32]

          *B607271,1 KHM 06/04/2003 (Begin) Use the ceiling function.
          *lnIssWare = ASCAN(laMatWare,lcIssWare)/2
          lnIssWare = CEILING(ASCAN(laMatWare,lcIssWare)/2)
          *B607271,1 KHM 06/04/2003 (End)
      
          lnOnHand = FABDYE.OnHand
          STORE nIssue TO m.Used_Qty,m.Req_Qty
          SELECT cFabric,cColor,cWareCode,cDyelot,cRSession,cISession,nUnitCost,;
                 cApInvNo,dTranDate,nRecCost,cTran,SUM(nReceived-nIssued) AS 'nBalance',;
                 nUntCstBuy,00000000.000 AS nToIssue FROM MatInvJl ;
          WHERE  cFabric+cColor+cWareCode+cDyelot+cRSession+cISession = ;
                 SUBSTR(m.Item,1,7)+m.IClr .AND. IIF(EMPTY(m.Dyelot),.T.,cDyelot=m.Dyelot);
          GROUP BY cFabric,cColor,cWareCode,cDyelot,cRSession ;
          HAVING nBalance > 0 INTO DBF (gcWorkDir+lcOpenLots)
          INDEX ON cWareCode+Dyelot TAG (lcOpenLots)

          *B604073,1 KHM 12/25/2000 (Begin) Changing the relation.
          *SET RELATION TO cRSession INTO FABINV
          SET RELATION TO cFabric+cColor+cWareCode+cDyelot+cRSession INTO FABINV
          *B604073,1 KHM 12/25/2000 (End)

          SELECT FABINV
          SET RELATION TO 'P'+cTran INTO PofHdr
          SELECT (lcOpenLots)
          DO (gcScrDir+"MFOPLOT.SPX") WITH .T.
          USE IN (lcOpenLots)
          SELECT (lcIssLtFile)
        CASE INLIST(laSetups[9,2],'F','I')
          =lfIssSeq(laSetups[9,2],Typ,cCatgTyp,SUBSTR(Item,1,7),IClr,;
                    cWareCode,Dyelot,nIssue,lcOprCode,lcLotNo,ldIssDate)
        OTHERWISE
          lnIssCost = IIF(laSetups[9,2]='S',Fabric.CostBuy,;
                      IIF(laSetups[2,2]='Y',FabDye.nAveCstBuy,Fabric.nAveCstBuy))
          *E301121,4 Use the material control global function
          *=lfIssRetFab(Typ,cCatgTyp,SUBSTR(Item,1,7),IClr,cWareCode,Dyelot,;
                       nIssue,lnIssCost,lcOprCode,lcLotNo,ldIssDate,SPACE(6),lcSession)
          =lfIssRetFab(Typ,cCatgTyp,SUBSTR(Item,1,7),IClr,cWareCode,Dyelot,;
                       nIssue,lnIssCost,lcOprCode,lcLotNo,ldIssDate,SPACE(6),gfsequence('GLSession'))
          *E301121,4 (End)
      ENDCASE
    CASE cCatgTyp = 'S'
      DECLARE laIssQty[9]
  
      *B605677,4 KHM 03/19/2002 (Begin) Taking into consideration the case of
      *B605677,1                style cost sheet by size.
      lcNoofSize = IIF(EMPTY(cCompSizes),'12345678',cCompSizes)
      laIssQty =  0      
      *laIssQty[1] = -1*MAX(CEILING((Req_Qty1-Used_Qty1)*nIssPcnt/100),0)
      *laIssQty[2] = -1*MAX(CEILING((Req_Qty2-Used_Qty2)*nIssPcnt/100),0)
      *laIssQty[3] = -1*MAX(CEILING((Req_Qty3-Used_Qty3)*nIssPcnt/100),0)
      *laIssQty[4] = -1*MAX(CEILING((Req_Qty4-Used_Qty4)*nIssPcnt/100),0)
      *laIssQty[5] = -1*MAX(CEILING((Req_Qty5-Used_Qty5)*nIssPcnt/100),0)
      *laIssQty[6] = -1*MAX(CEILING((Req_Qty6-Used_Qty6)*nIssPcnt/100),0)
      *laIssQty[7] = -1*MAX(CEILING((Req_Qty7-Used_Qty7)*nIssPcnt/100),0)
      *laIssQty[8] = -1*MAX(CEILING((Req_Qty8-Used_Qty8)*nIssPcnt/100),0)
      FOR lnCntr = 1 TO 8
        lcCntr = STR(lnCntr,1)
        IF lcCntr $ lcNoofSize
          laIssQty[lnCntr] = -1*MAX(CEILING((Req_Qty&lcCntr-Used_Qty&lcCntr)*nIssPcnt/100),0)
        ENDIF  
      ENDFOR
      *B605677,4 KHM 03/19/2002 (End)
      laIssQty[9] = laIssQty[1]+laIssQty[2]+laIssQty[3]+laIssQty[4]+;
                    laIssQty[5]+laIssQty[6]+laIssQty[7]+laIssQty[8]
      =SEEK(Item,'Style')
      =SEEK(Item+cWareCode+SPACE(10),'StyDye')
      lnIssCost = IIF(laSetups[10,2]='S',Style.TotCost,;
                  IIF(laSetups[2,2]='Y',StyDye.Ave_Cost,Style.Ave_Cost))
      
      *B039660,1 NNA 06/06/2006 (Begin) convert next trigger to be done within Binmain.Prg instead of Davmain.prg 
      *C123853,1 MHM 01/15/2005 check Bin Location [Start]
      *=lfIssRetSty(Typ,cCatgTyp,Item,cWareCode,Dyelot,;
                   @laIssQty,lnIssCost,lcOprCode,lcLotNo,ldIssDate,SPACE(6),lcSession)

      IF ASCAN(laEvntTrig,PADR("DVLDPOAU",10)) <> 0 
        =gfDoTriger("POCSSH",PADR("DVLDPOAU",10))
      ELSE
        =lfIssRetSty(Typ,cCatgTyp,Item,cWareCode,Dyelot,;
                     @laIssQty,lnIssCost,lcOprCode,lcLotNo,ldIssDate,SPACE(6),lcSession)
      ENDIF             
      *C123853,1 MHM 01/15/2005 [End]
      *B039660,1 NNA (End)
      
    *B603861,1 AMH 09/11/2000 Prevent to issue the AP invoiced cost items [Start]
    CASE INLIST(cCatgTyp,'M','P','D')
      IF !('AP' $ gcCmpModules) .OR. ;
         (cCatGTyp = 'M' .AND. gfRltFld(MfgCode,@laMfgRFld,'MFGCODE') ;
         .AND. !EMPTY(lcMfgGlAcnt)) 
        
        *B803830,1 AMH Changing the passing of item and Iclr paramters to be
        *B803830,1 spaces.[Start]
        *=lfIssRetMfg(Typ,cCatgTyp,SUBSTR(Item,1,7),IClr,MfgCode,cWareCode,;
                     Dyelot,nIssue,UntCost,ldIssDate,lcOprCode,lcLotNo,;
                     SPACE(6),gfsequence('GLSession'))

        =lfIssRetMfg(Typ,cCatgTyp,SPACE(7),SPACE(6),MfgCode,cWareCode,;
                     Dyelot,nIssue,UntCost,ldIssDate,lcOprCode,lcLotNo,;
                     SPACE(6),gfsequence('GLSession'))
        *B803830,1 AMH [End]
      ELSE
        IF !llMessage
          *B603861,1 Message : 34175
          *B603861,1 ð cost ð can not be issued since ð linked to the AP module.
          *B603861,1 Button : 00000
          *B603861,1 Ok
          lcMessage = IIF(lnSelCount>1,'One/Some of the selected|items|they are','This|item|it is')
          =gfModalGen('TRM34175B00000','ALERT',lcMessage)
          llMessage = .T.
        ENDIF          
      ENDIF
    *B603861,1 AMH 09/11/2000 Prevent to issue the AP invoiced cost items [End  ]
    OTHERWISE
      *E500231,1 Create new session number for evry MFG issue.
      *=lfIssRetMfg(Typ,cCatgTyp,SUBSTR(Item,1,7),IClr,MfgCode,cWareCode,;
                   Dyelot,nIssue,UntCost,ldIssDate,lcOprCode,lcLotNo,SPACE(6),lcSession)
      =lfIssRetMfg(Typ,cCatgTyp,SUBSTR(Item,1,7),IClr,MfgCode,cWareCode,;
                   Dyelot,nIssue,UntCost,ldIssDate,lcOprCode,lcLotNo,;
                   SPACE(6),gfsequence('GLSession'))
      *E500231,1 (End)
  ENDCASE
  SELECT (lcTktSheet)
  =SEEK(&lcIssLtFile..cimtyp+&lcIssLtFile..cuttkt+&lcIssLtFile..typ+;
        &lcIssLtFile..item+&lcIssLtFile..iclr+&lcIssLtFile..mfgcode)

  *B802577,1 WAB - No need For replace dyelot it's already exist
  *B802577,1 WAB - START
  *REPLACE Dyelot WITH &lcIssLtFile..Dyelot
  *B802577,1 WAB - END

  SELECT CTktBom
  =SEEK(&lcIssLtFile..cimtyp+&lcIssLtFile..cuttkt+&lcIssLtFile..typ+;
        &lcIssLtFile..item+&lcIssLtFile..iclr+&lcIssLtFile..mfgcode)

  *B802577,1 WAB - No need For replace dyelot it's already exist
  *B802577,1 WAB - START
  *REPLACE Dyelot WITH &lcIssLtFile..Dyelot
  *B802577,1 WAB - END

ENDSCAN

*!*************************************************************
*! Name      : lfvIssPcnt
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Lot Issue Percent
*!*************************************************************
*! Calls     : lfShIssLot
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvIssPcnt()
*!*************************************************************
FUNCTION lfvIssPcnt
PRIVATE lnRecNo

SELECT (lcIssLtFile)
lnRecNo = RECNO()
REPLACE ALL nIssPcnt WITH spIssPcnt ;
            nIssue   WITH spIssPcnt*MAX(REQ_QTY-Used_Qty,0)/100 ;
            FOR !EMPTY(cSelect)
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF 
=lfShIssLot()

*!*************************************************************
*! Name      : lfIssRetFab
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Issue/Return Fabrics and Inventory mantained trims
*!*************************************************************
*! Calls     : gfModalGen,GLDIST
*!*************************************************************
*! Parameters: lcItemType   : Cost Item Type
*!             lcCatgType   : Cost Item Category 
*!             lcFabric     : Fabric
*!             lcColor      : Color
*!             lcWareCode   : Warehouse
*!             lcDyelot     : Dyelot
*!             lnIssued     : Issued/Return Quantity
*!             lnBuyIssCost : Issued/Return unit cost buy
*!             lcOprCode    : Operation
*!             lcLotNo      : Lot#
*!             ldIssDate    : Issue/Return Date
*!             lcRSession   : Receiving Session#
*!             lcISession   : Issue Session#
*!             llFromDel    : Called upon Deleting The Cost Sheet,
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfIssRetFab()
*!*************************************************************
FUNCTION lfIssRetFab


*B603033,1 (Start)
*PARAMETERS lcItemType,lcCatgType,lcFabric,lcColor,lcWareCode,lcDyelot,;
           lnIssued,lnBuyIssCost,lcOprCode,lcLotNo,ldIssDate,lcRSession,;
           lcISession,llFromDel
PARAMETERS lcItemType,lcCatgType,lcFabric,lcColor,lcWareCode,lcDyelot,;
           lnIssued,lnBuyIssCost,lcOprCode,lcLotNo,ldIssDate,lcRSession,;
           lcISession,llFromDel,llUseActCost
*B603033,1 (End)           

PRIVATE lcFabLinkCode,lnAmount,lnOldStock,lnOldCost,lcRollID,lnAlias

IF SEEK(lcFabric+lcColor,'Fabric') .AND. lnIssued <> 0
  *C200255,1 (Begin) Update MATINVJL with custom Voucher no for CON10.
  IF !llFrstTime AND ASCAN(laEvntTrig,PADR("GETVOUT",10)) <> 0 AND TYPE('lcVoucNo') = 'U'
    lcVoucNo = ""
    llContVout = .T.
    = gfDoTriger("MAPOREC",PADR("GETVOUT",10))
    IF !llContVout
      RETURN
    ENDIF
  ENDIF  
  *C200255,1 (Begin)
*--- DOC.
*--- Check if this fabric/color are assigned to the selected
*--- warehouse
*--- DOC.
  IF laSetups[2,2]='Y' .AND. !EMPTY(lcWareCode) .AND. ;
    !SEEK (lcFabric+lcColor+lcWareCode+SPACE(10),'FABDYE')
    *E300725,1 Message : 38029
    *E300725,1 Item/Color xxxxx/xxxx is not assigned to warehouse xxxx
    *E300725,1 Button : 38001
    *E300725,1 Add Cancel
    IF gfModalGen('QRM38029B38001','ALERT','Fabric/Color: '+ALLTRIM(lcFabric)+'/'+ALLTRIM(lcColor)+'|'+lcWareCode) = 1
      DO gpAdFabWar WITH lcFabric,lcColor,SPACE(10),lcWareCode
    ELSE
      RETURN
    ENDIF
  ENDIF
  IF laSetups[12,2]='Y' .AND. Fabric.cDye_Flg='Y' AND ;
    !EMPTY(lcDyelot) .AND. !SEEK(lcFabric+lcColor+lcWareCode+lcDyelot,'FabDye') AND;
    !gfSelDyelot(lcTranType,'F','','',lcWareCode,lcDyelot,lcFabric,lcColor)
    RETURN
  ENDIF

  lnAlias = SELECT()
*--- DOC.
*--- Get the Gl Link code such that is the fabric link code is empty then
*--- update it with DEFDEF
*--- DOC.
  lcFabLinkCode = IIF(EMPTY(Fabric.Link_Code),'DEFDEF',Fabric.Link_Code)
  
  *E301121,4 Use the material control global function
  IF SEEK(lcFabric+lcColor+lcWareCode+SPACE(10),'FabDye')
    lcFabLinkCode = IIF(EMPTY(FabDye.Gl_Link),lcFabLinkCode,FabDye.Gl_Link)
  ENDIF
  IF laSetups[1,2]='Y'
*--- DOC.
*--- Update the array that used by material control functoin
*--- DOC.
    DECLARE laGLDistAr[2,13]
    laGLDistAr[1,1] = lcFabLinkCode
    laGLDistAr[2,1] = laData[29]
    laGLDistAr[1,2] = '015'
    laGLDistAr[2,2] = '013'
    laGLDistAr[1,3] = 1
    laGLDistAr[2,3] = -1
    STORE 'MA'       TO laGLDistAr[1,4],laGLDistAr[2,4]

    *B602983 (Start)
    *STORE ''         TO laGLDistAr[1,5],laGLDistAr[2,5]
    STORE laData[1]   TO laGLDistAr[1,5],laGLDistAr[2,5]
    *B602983 (End)

    STORE ldIssDate  TO laGLDistAr[1,6],laGLDistAr[2,6]
    STORE lcGlYear   TO laGLDistAr[1,7],laGLDistAr[2,7]
    STORE lcGlPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
    STORE lcGlDTemp  TO laGLDistAr[1,9],laGLDistAr[2,9]
    STORE ''         TO laGLDistAr[1,10],laGLDistAr[2,10]
  ELSE
    DIME laGLDistAr[1,1]
    laGLDistAr = ''
  ENDIF
  *B602983 (Start)
  *=gfMatCrl('4',lcFabric,lcColor,lcWareCode,lcDyelot,ldIssDate,ldIssDate,;
            SPACE(6),-1*lnIssued,lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv),;
            '','',0,'','',@laGLDistAr,'',lcTranType,laData[1],lcRSession,lcISession)

  *B603033.1 (Start)
  *=gfMatCrl('4',lcFabric,lcColor,lcWareCode,lcDyelot,ldIssDate,ldIssDate,;
            laData[1],-1*lnIssued,lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv),;
            '','',0,'','',@laGLDistAr,'',lcTranType,laData[1],lcRSession,lcISession)
  *B603116 (Start)
  *=gfMatCrl('4',lcFabric,lcColor,lcWareCode,lcDyelot,ldIssDate,ldIssDate,;
            laData[1],-1*lnIssued,lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv),;
            '','',0,'','',@laGLDistAr,'',lcTranType,laData[1],lcRSession,lcISession,;
            '','','',llUseACst)
  PRIVATE laOtherPar
  *B802475,1 AMM Add two new parameters
  *DIMENSION laOtherPar[1,2]
  DIMENSION laOtherPar[3,2]
  *B802475,1 AMM end
*--- DOC.
*--- List of all variable to be used as parameter in the material control
*--- DOC.
  laOtherPar[1,1] = 'llUseACst'
  laOtherPar[1,2] = llUseActCost
  *B802475,1 AMM Send the operation and lot number as new parameters
  laOtherPar[2,1] = 'lcOprCode'
  laOtherPar[2,2] = lcOprCode
  laOtherPar[3,1] = 'lcLotNo'
  laOtherPar[3,2] = lcLotNo
  *B802475,1 AMM end
  
  *B606028,1 AMH Put zero to nmatwip of current C/T or PO [Start]
  *khm1
  *IF lcTranType $ 'MI' .AND. lnIssued < 0
  IF lcTranType $ 'MID' .AND. lnIssued < 0
  *khm1  
    =lfUpdWip()
  ENDIF
  *B606028,1 AMH [End]
  
  =gfMatCrl('4',lcFabric,lcColor,lcWareCode,lcDyelot,ldIssDate,ldIssDate,;
            laData[1],-1*lnIssued,lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv),;
            '','',0,'','',@laGLDistAr,'',lcTranType,laData[1],lcRSession,lcISession,;
            '','','',@laOtherPar)

  *B606028,1 AMH Correct update nmatwip in fabric & fabdye files [Start]
  *khm1
  *IF lcTranType $ 'MI' .AND. lnIssued < 0
  IF lcTranType $ 'MID' .AND. lnIssued < 0
  *khm1
  
    =lfRetWip()
  ENDIF
  *B606028,1 AMH [End]
  
  *C200256,1 (Begin) Receive Qty for CON10.
  IF !llFrstTime AND ASCAN(laEvntTrig,PADR("PRNTIT",10)) <> 0
    llFromCost = .T.
    lnNoOfCop  = 1
    lnPrgnO    = 1  
    lnRep      = 0
    lcTranTit  = "CUT #"
    lnTotQty   = ABS(-1*lnIssued)
    lnCostIt   = lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv)
    lcRecDate  = "ldIssDate"
    IF gfModalGen('QRM00000B00006',.F.,.F.,.F.,'Do you wish to print the received quantities?')=1)
      = gfDoTriger("MAPOREC",PADR("PRNTIT",10))    
    ENDIF
    llFrstTime = .T.
  ENDIF  
  *C200256,1 (Begin)

  *B603116 (End)
  *B603033,1 (End)

  *B602983 (End)
  *E301121,4 Follow lines are commnet out
  *IF laSetups[12,2]='Y' .AND. Fabric.cDye_Flg='Y' .AND. ;
  *  SEEK(lcFabric+lcColor+lcWareCode+lcDyelot,'FabDye')
  *  SELECT FabDye
  *  =RLOCK()
  *  *-- HDM 03/11/1998 B602148,1[START]
  *  *REPLACE OnHand WITH OnHand - lnIssued ,;
  *          USAGE  WITH USAGE  + lnIssued
  *  REPLACE OnHand  WITH OnHand  - lnIssued ,;
  *          USAGE   WITH USAGE   + lnIssued ,;
  *          NMATWIP WITH NMATWIP + lnIssued
  *  *-- HDM 03/11/1998 B602148,1[END]
  *  UNLOCK
  *  IF OnHand < 0
  *    *E300725,1 Message : 38034
  *    *E300725,1 Inventory of fabric/color/dyelot: xxx/xxx/xxx has gone below zero
  *    *E300725,1 Button : 00000
  *    *E300725,1 Ok
  *    =gfModalGen('INM38034B00000','ALERT',TRIM(lcFabric)+'/'+TRIM(lcColor)+'/'+TRIM(lcDyelot))
  *  ENDIF
  *ENDIF
  *IF SEEK(lcFabric+lcColor+lcWareCode+SPACE(10),'FabDye')
  *  SELECT FabDye
  *  lcFabLinkCode = IIF(EMPTY(FabDye.Gl_Link),lcFabLinkCode,FabDye.Gl_Link)
  *  lnOldStock    = OnHand
  *  lnOldCost     = IIF(laSetups[9,2]='S',;
  *                  Fabric.CostBuy/IIF(Fabric.Conv=0,1,Fabric.Conv),;
  *                  IIF(OnHand=0,0,nStkVal/OnHand) )
  *  =RLOCK()
  *  IF lnIssued < 0 AND OnHand < 0 AND OnHand-lnIssued > 0
  *    REPLACE nStkVal WITH (OnHand-lnIssued)*lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv)
  *  ELSE 
  *    REPLACE nStkVal WITH nStkVal - lnIssued*lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv)
  *  ENDIF
  *  *-- HDM 03/11/1998 B602148,1[START]    
  *  *REPLACE nFAve_Cost WITH IIF((OnHand-lnIssued)=0,0,nStkVal/(OnHand-lnIssued)) ,;
  *          nAveCstBuy WITH IIF((OnHand-lnIssued)=0,0,nStkVal*Fabric.Conv/(OnHand-lnIssued)) ,;
  *          OnHand     WITH OnHand - lnIssued ,;
  *          USAGE      WITH USAGE  + lnIssued
  *  REPLACE nFAve_Cost WITH IIF((OnHand-lnIssued)=0,0,nStkVal/(OnHand-lnIssued)) ,;
  *          nAveCstBuy WITH IIF((OnHand-lnIssued)=0,0,nStkVal*Fabric.Conv/(OnHand-lnIssued)) ,;
  *          OnHand     WITH OnHand  - lnIssued ,;
  *          USAGE      WITH USAGE   + lnIssued ,;
  *          NMATWIP    WITH NMATWIP + lnIssued
  *  *-- HDM 03/11/1998 B602148,1[END]
  *  UNLOCK
  *  IF laSetups[2,2]='Y' .AND. OnHand < 0
  *    *E300725,1 Message : 38034
  *    *E300725,1 Inventory of fabric/color: xxx/xxx in warehouse xxx has gone below zero
  *    *E300725,1 Button : 00000
  *    *E300725,1 Ok
  *    =gfModalGen('INM38034B00000','ALERT',TRIM(lcFabric)+'/'+TRIM(lcColor)+' in warehouse: '+lcWareCode)
  *  ENDIF
  *ENDIF
  *SELECT FABRIC
  *=RLOCK()
  *IF lnIssued < 0 AND OnHand < 0 AND OnHand-lnIssued > 0
  *  REPLACE nStkVal WITH (OnHand-lnIssued)*lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv)
  *ELSE 
  *  REPLACE nStkVal WITH nStkVal - lnIssued*lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv)
  *ENDIF
  **-- HDM 03/11/1998 B602148,1[START]
  **REPLACE nFAve_Cost WITH IIF((OnHand-lnIssued)=0,0,nStkVal/(OnHand-lnIssued)) ,;
  *        nAveCstBuy WITH IIF((OnHand-lnIssued)=0,0,nStkVal*Fabric.Conv/(OnHand-lnIssued)) ,;
  *        OnHand     WITH OnHand - lnIssued ,;
  *        USAGE      WITH USAGE  + lnIssued
  *REPLACE nFAve_Cost WITH IIF((OnHand-lnIssued)=0,0,nStkVal/(OnHand-lnIssued)) ,;
  *        nAveCstBuy WITH IIF((OnHand-lnIssued)=0,0,nStkVal*Fabric.Conv/(OnHand-lnIssued)) ,;
  *        OnHand     WITH OnHand  - lnIssued ,;
  *        USAGE      WITH USAGE   + lnIssued ,;
  *        NMATWIP    WITH NMATWIP + lnIssued
  **-- HDM 03/11/1998 B602148[END]
  *UNLOCK
  *IF OnHand < 0
  *  *E300725,1 Message : 38034
  *  *E300725,1 Inventory of fabric/color: xxx/xxx has gone below zero
  *  *E300725,1 Button : 00000
  *  *E300725,1 Ok
  *  =gfModalGen('INM38034B00000','ALERT',TRIM(lcFabric)+'/'+TRIM(lcColor))
  *ENDIF
  *SELECT MatInvJl
  *APPEND BLANK
  **--HDM 11/10/1998 E301037,1 [Start] Change the returned QTY to be +ve in nRecieved field instead of
  **--                         -ve in nIssued Field
  **REPLACE cFabric    WITH lcFabric   ,;
  *         cColor     WITH lcColor    ,;
  *         cWareCode  WITH lcWareCode ,;
  *         cDyelot    WITH lcDyelot   ,;
  *         dTranDate  WITH ldIssDate  ,;
  *         cTranType  WITH '4'        ,;
  *         cRSession  WITH lcRSession ,;
  *         cISession  WITH lcISession ,;
  *         nIssued    WITH lnIssued   ,;
  *         cTrn_Seq   WITH lcSession  ,;
  *         cTktNo     WITH laData[1]  ,; 
  *         cTran      WITH laData[1]  ,;
  *         nUnitCost  WITH lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv) ,;
  *         nUntCstBuy WITH lnBuyIssCost ,;
  *         cOprCode   WITH lcOprCode ,;
  *         cLotNo     WITH lcLotNo
  *REPLACE cFabric    WITH lcFabric                           ,;
  *        cColor     WITH lcColor                            ,;
  *        cWareCode  WITH lcWareCode                         ,;
  *        cDyelot    WITH lcDyelot                           ,;
  *        dTranDate  WITH ldIssDate                          ,;
  *        cTranType  WITH '4'                                ,;
  *        cRSession  WITH lcRSession                         ,;
  *        cISession  WITH lcISession                         ,;
  *        nIssued    WITH IIF(lnIssued > 0 , lnIssued,0)     ,;
  *        nReceived  WITH IIF(lnIssued < 0 , lnIssued * -1,0),;
  *        cTrn_Seq   WITH lcSession                          ,;
  *        cTktNo     WITH laData[1]                          ,; 
  *        cTran      WITH laData[1]                          ,;
  *        nUnitCost  WITH lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv) ,;
  *        nUntCstBuy WITH lnBuyIssCost ,;
  *        cOprCode   WITH lcOprCode ,;
  *        cLotNo     WITH lcLotNo
  **--HDM 11/10/1998 E301037,1 [END] Change the returned QTY to be +ve in nRecieved field instead of
  **--                         -ve in nIssued Field
  *E301121,4 (End)
*--- DOC.
*--- If the system is keep trak of roll and the fabric use rolls
*--- then update ROLLS file
*--- DOC.

  IF laSetups[11,2]='Y' .AND. Fabric.lTrkRolls .AND. lnIssued > 0
    =gfOpenFile(gcDataDir+'ROLLS',gcDataDir+'Rolapl','SH')
    SELECT ROLLS
    SET ORDER TO TAG Rolapl
    lcRollID = '***** N/A *****'
    IF !SEEK(lcRSession+lcISession+lcFabric+lcColor+lcWareCode+lcDyelot+lcRollID)
      APPEND BLANK
    ENDIF
    REPLACE cRollItem   WITH lcFabric           ,; 
            Color       WITH lcColor            ,;
            Cwarecode   WITH lcWareCode         ,;
            Dyelot      WITH lcDyelot           ,;
            Crollid     WITH lcRollID           ,;
            Nqtybal     WITH Nqtybal + lnIssued ,;
            Nqty        WITH Nqty    + lnIssued ,;
            Trancd      WITH '2'                ,;  
            Ctktno      WITH laData[1]          ,;
            Csession    WITH lcISession         ,; 
            Crsession   WITH lcRSession         ,;
            Cisession   WITH lcISession
  ENDIF  

  IF !llFromDel 
    SELECT (lcTktSheet)
    IF SEEK(lcItemType+'1'+PADR(lcFabric,19)+lcColor+SPACE(6)+lcDyelot)
      REPLACE Used_Qty  WITH Used_Qty  + lnIssued ,;
              Issue_Qty WITH Issue_Qty + MAX(lnIssued,0) ,;
              Dyelot    WITH lcDyelot
    ENDIF
    SELECT CTktBom
    IF SEEK(lcTranType+laData[1]+lcItemType+PADR(lcFabric,19)+lcColor+SPACE(6)+lcDyelot)
      REPLACE Used_Qty  WITH Used_Qty  + lnIssued ,;
              Issue_Qty WITH Issue_Qty + MAX(lnIssued,0) ,;
              Dyelot    WITH lcDyelot              
    ENDIF
    SELECT BomCost
    SET ORDER TO TAG Bomcstkt
    SET DELETE OFF
    IF !SEEK(lcItemType+lcTranType+laData[1]+PADR(lcFabric,19)+lcColor+;
             SPACE(6)+lcWareCode+lcDyelot+lcRSession+lcISession)
      APPEND BLANK
    ELSE
      IF DELETED()
        BLANK
        RECALL
      ENDIF
    ENDIF
    SET DELETE ON

    *B605481,1 KHM 02/17/2002 (Begin) Changing the replacement of Actualize
    *B605481,1                to "N" instead of "Y".
    *REPLACE cTktNo    WITH laData[1]  ,;
            cWareCode WITH lcWareCode ,;
            cDyelot   WITH lcDyelot   ,;
            Item      WITH lcFabric   ,;
            IClr      WITH lcColor    ,;
            cBomType  WITH lcItemType ,;
            cIMTyp    WITH lcTranType ,;
            MfgCode   WITH SPACE(6)   ,;
            nTotQty   WITH nTotQty+lnIssued,;
            nTotCst   WITH nTotCst +lnIssued*lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv) ,;
            dTranDate WITH ldIssDate  ,;
            cRSession WITH lcRSession ,;
            cISession WITH lcISession ,;
            cCostType WITH lcCatgType ,;
            nUnitCst  WITH IIF(nTotQty=0,0,nTotCst/nTotQty) ,;
            nUnitACst WITH nUnitCst  ,;
            nTotACst  WITH nTotCst   ,;
            cOprCode  WITH lcOprCode ,;
            cLotNo    WITH lcLotNo   ,;
            Actualize WITH 'Y'
    
    REPLACE cTktNo    WITH laData[1]  ,;
            cWareCode WITH lcWareCode ,;
            cDyelot   WITH lcDyelot   ,;
            Item      WITH lcFabric   ,;
            IClr      WITH lcColor    ,;
            cBomType  WITH lcItemType ,;
            cIMTyp    WITH lcTranType ,;
            MfgCode   WITH SPACE(6)   ,;
            nTotQty   WITH nTotQty+lnIssued,;
            nTotCst   WITH nTotCst +lnIssued*lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv) ,;
            dTranDate WITH ldIssDate  ,;
            cRSession WITH lcRSession ,;
            cISession WITH lcISession ,;
            cCostType WITH lcCatgType ,;
            nUnitCst  WITH IIF(nTotQty=0,0,nTotCst/nTotQty) ,;
            nUnitACst WITH nUnitCst  ,;
            nTotACst  WITH nTotCst   ,;
            cOprCode  WITH lcOprCode ,;
            cLotNo    WITH lcLotNo   ,;
            Actualize WITH 'N'
    *B605481,1 KHM 02/17/2002 (End)
    
    IF nTOtQty = 0
      DELETE
    ENDIF
    laData[22+VAL(lcItemType)-1] = laData[22+VAL(lcItemType)-1] + lnIssued*lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv)
    DO CASE
      CASE lcTranType = 'M'
        SELECT CUTTKTH
        =RLOCK()
        REPLACE nAct_Cost&lcItemType WITH nAct_Cost&lcItemType + lnIssued*lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv)
        UNLOCK
      *khm1
      *CASE lcTranType = 'I'
      CASE lcTranType $ 'ID'
      *khm1
            
        SELECT POSHDR
        =RLOCK()
        REPLACE nAct_Cost&lcItemType WITH nAct_Cost&lcItemType + lnIssued*lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv)
        
        *B605480,1 KHM 02/14/2002 (Begin) Update the nfActCost also.
         DO CASE
           CASE lcCatgType = 'P'
              REPLACE nfActCost&lcItemType WITH (nAct_Cost&lcItemType &lcPExSign POSHDR.nPriceRat &lcPUntSin POSHDR.nCurrUnit)
            CASE !INLIST(lcCatgType ,'S','F','T')
              REPLACE nfActCost&lcItemType WITH (nAct_Cost&lcItemType &lcDExSign POSHDR.nDutyRat &lcDUntSin POSHDR.nDCurUnit)
            OTHERWISE
              REPLACE nfActCost&lcItemType WITH nAct_Cost&lcItemType
          ENDCASE                   
        *B605480,1 KHM 02/14/2002 (End)
        
        UNLOCK
      CASE lcTranType = 'T'
        SELECT MMFGORDH
        =RLOCK()
        REPLACE nAct_Cost&lcItemType WITH nAct_Cost&lcItemType + lnIssued*lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv)
        UNLOCK
    ENDCASE
  ENDIF
  IF laSetups[1,2] = 'Y'
    
    *E301121,4 Follow lines are commnet out
    **B602269,1 Update Material Inventory control as CREDIT while issue materials
    **lnAmount = lnIssued *lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv)
    *lnAmount = -1* lnIssued *lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv)
    **B602269,1 (End)
    **E300725,1 Material Inventory Control ( 015 )
    *DO GLDIST WITH lcFabLinkCode,'015',lnAmount,'MA',laData[1],ldIssDate,;
    *               lcGlYear,lcGlPeriod,lcGlDTemp
    **E300725,1 W.I.P. ( 013 )
    *DO GLDIST WITH laData[29],'013',-lnAmount,'MA',laData[1],ldIssDate,;
    *               lcGlYear, lcGlPeriod, lcGlDTemp
    *IF lnIssued < 0 .AND. lnOldStock < 0 .AND. lnOldCost <> lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv)
    *  lnAmount = MIN(ABS(lnIssued),ABS(lnOldStock)) * (lnOldCost - lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv))
    *  *E300725,1 Material Inventory Control ( 015 )
    *  DO GLDIST WITH lcFabLinkCode,'015',lnAmount,'MA',laData[1],ldIssDate,;
    *                 lcGlYear,lcGlPeriod,lcGlDTemp
    *  *E300725,1 Material Inventory Adjustment ( 016 )
    *  DO GLDIST WITH lcFabLinkCode,'016', -lnAmount,'MA',laData[1],ldIssDate,;
    *                 lcGlYear, lcGlPeriod,lcGlDTemp
    *ENDIF
    *E301121,4 (End)
    
    SELECT (lcGlDTemp)
    REPLACE ALL GlSession WITH lcSession
    SELECT GLDIST
    APPEND FROM (gcWorkDir+lcGlDTemp)
    SELECT (lcGlDTemp)
    ZAP
  ENDIF
  SELECT (lnAlias)
ENDIF

*!*************************************************************
*! Name      : lfIssSeq
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Issue Fabrics and Inventory mantained trims FIFO/LIFO
*!*************************************************************
*! Calls     : gfModalGen,GLDIST
*!*************************************************************
*! Parameters: lcType       : 'F' FIFO
*!                          : 'I' LIFO
*!             lcItemType   : Cost Item Type
*!             lcCatgType   : Cost Item Category 
*!             lcFabric     : Fabric
*!             lcColor      : Color
*!             lcWareCode   : Warehouse
*!             lcDyelot     : Dyelot
*!             lnIssued     : Issued/Return Quantity
*!             lcOprCode    : Operation
*!             lcLotNo      : Lot#
*!             ldIssDate    : Issue/Return Date
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfIssSeq()
*!*************************************************************
FUNCTION lfIssSeq
PARAMETERS lcType,lcItemType,lcCatgType,lcFabric,lcColor,lcWareCode,lcDyelot,;
           lnIssued,lcOprCode,lcLotNo,ldIssDate
PRIVATE lnAlias,lnToIssue

lnAlias = SELECT()
*--- DOC.
*--- Select all material open lots
*--- DOC.
SELECT cFabric,cColor,cWareCode,cDyelot,cRSession,cISession,nUnitCost,;
       cApInvNo,dTranDate,nRecCost,cTran,SUM(nReceived-nIssued) AS 'nBalance',;
       nUntCstBuy FROM MatInvJl ;
WHERE  cFabric+cColor+cWareCode+cDyelot+cRSession+cISession = ;
       lcFabric+lcColor+lcWareCode+ALLTRIM(lcDyelot) ;
GROUP BY cFabric,cColor,cWareCode,cDyelot,cRSession ;
HAVING nBalance > 0 INTO DBF (gcWorkDir+lcOpenLots)
IF _TALLY = 0
  *E300725,1 Message : 38094
  *E300725,1 No open lots found for fabric/color
  *E300725,1 Button : 00000
  *E300725,1 Ok
  =gfModalGen('TRM38094B00000','ALERT','fabric/color: '+lcFabric+'/'+lcColor)
ELSE
  INDEX ON cFabric+cColor+cWareCode+cDyelot+cRSession TAG (lcOpenLots)
  IF lcType = 'I'
    SET ORDER TO TAG (lcOpenLots) DESCENDING
  ENDIF
  GO TOP
  SCAN WHILE lnIssued > 0
    lnToIssue = MIN(lnIssued,nBalance)
    *E301121,4 Use the material control global function
    *=lfIssRetFab(lcItemType,lcCatgType,lcFabric,lcColor,lcWareCode,lcDyelot,;
                 lnToIssue,nUntCstBuy,lcOprCode,lcLotNo,ldIssDate,cRSession,lcSession)
    =lfIssRetFab(lcItemType,lcCatgType,lcFabric,lcColor,lcWareCode,lcDyelot,;
                 lnToIssue,nUntCstBuy,lcOprCode,lcLotNo,ldIssDate,cRSession,gfsequence('GLSession'))
    *E301121,4 (End)
    lnIssued = lnIssued - lnToIssue
  ENDSCAN
ENDIF
USE IN (lcOpenLots)
SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfIssRetSty
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Issue/Return Styles
*!*************************************************************
*! Calls     : gfModalGen(),gfStyCrl()
*!*************************************************************
*! Parameters: lcItemType   : Cost Item Type
*!             lcCatgType   : Cost Item Category 
*!             lcStyle      : Style
*!             lcWareCode   : Warehouse
*!             lcDyelot     : Dyelot
*!             laIssued     : Issued/Return Array Quantity
*!             lnIssCost    : Issued/Return unit cost
*!             lcOprCode    : Operation
*!             lcLotNo      : Lot#
*!             ldIssDate    : Issue/Return Date
*!             lcRSession   : Receiving Session#
*!             lcISession   : Issue Session#
*!             llFromDel    : Called upon Deleting The Cost Sheet
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfIssRetSty()
*!*************************************************************
FUNCTION lfIssRetSty
PARAMETERS lcItemType,lcCatgType,lcStyle,lcWareCode,lcDyelot,;
           laIssued,lnIssCost,lcOprCode,lcLotNo,ldIssDate,lcRSession,;
           lcISession,llFromDel
PRIVATE lcStyLinkCode,lnAmount,lnOldStock,lnOldCost,lnAlias

DECLARE laGlArray[2,13]
STORE '' TO laGlArray
IF laSetups[1,2] = 'Y'
  STORE 'IA'       TO laGlArray[1,4],laGlArray[2,4]
  STORE laData[1]  TO laGlArray[1,5],laGlArray[2,5] 
  STORE ldIssDate  TO laGlArray[1,6],laGlArray[2,6]
  STORE lcGlYear   TO laGlArray[1,7],laGlArray[2,7]
  STORE lcGlPeriod TO laGlArray[1,8],laGlArray[2,8]
  STORE lcGlDTemp  TO laGlArray[1,9],laGlArray[2,9]
  STORE ''         TO laGlArray[1,10],laGlArray[2,10]
  STORE ''         TO laGlArray[1,11],laGlArray[2,11]
  STORE ''         TO laGlArray[1,12],laGlArray[2,12]
  STORE ''         TO laGlArray[1,13],laGlArray[2,13]
ENDIF
lnAlias = SELECT()
*--- DOC.
*--- Check if the style is assigned to the selected warehous before issue it
*--- DOC.
IF SEEK(lcStyle,'Style') .AND. laIssued[9] <> 0
  IF laSetups[2,2]='Y' .AND. !EMPTY(lcWareCode) .AND. ;
    !SEEK (lcStyle+lcWareCode+SPACE(10),'STYDYE')
    *E300725,1 Message : 38029
    *E300725,1 Style xxxxx is not assigned to warehouse xxxx
    *E300725,1 Button : 38001
    *E300725,1 Add Cancel
    IF gfModalGen('QRM38029B38001','ALERT','Style: '+ALLTRIM(lcStyle)+'|'+lcWareCode) = 1
      *--- DOC.
      *--- Call global function to assigne the style to warehouse
      *--- DOC.
      DO gpAdStyWar WITH lcStyle,SPACE(10),lcWareCode
    ELSE
      RETURN
    ENDIF   
  ENDIF
  IF laSetups[8,2]='Y' .AND. Style.cDye_Flg='Y' AND ;
    !EMPTY(lcDyelot) AND !SEEK(lcStyle+lcWareCode+lcDyelot,'StyDye') AND ;
    !gfSelDyelot(lcTranType,'S','','',lcWareCode,lcDyelot,lcStyle)
    RETURN
  ENDIF
  lcStyLinkCode = IIF(EMPTY(Style.Link_Code),'DEFDEF',Style.Link_Code)
  lcStyLinkCode = IIF(EMPTY(StyDye.Gl_Link),lcStyLinkCode,StyDye.Gl_Link)
  IF laSetups[1,2] = 'Y'
    laGlArray[1,1] = lcStyLinkCode
    laGlArray[1,2] = '006'
    laGlArray[1,3] = 1
    laGlArray[2,1] = laData[29]
    laGlArray[2,2] = '013'
    laGlArray[2,3] = -1
  ENDIF
  *E301248 (Start)
  *IF gfStyCrl('1',lcStyle,lcWareCode,lcDyelot,ldIssDate,laData[1],;
              @laIssued,lnIssCost,'',.F.,'',0,'','',@laGlArray)=0
*--- DOC.
*--- Call style control to issue/return the style
*--- DOC.

  *C123847,1  TMI [Start] check (stock - picked) >= issued , and warn if not for DIR03 
  IF ASCAN(laEvntTrig , PADR('STKPKISU',10)) <> 0
    IF !gfDoTriger('POCSSH',PADR('STKPKISU',10)) 
      RETURN
    ENDIF
  ENDIF 
  *C123847,1  TMI [End  ] 

  *B608010,1 TMI [Start] Sending the lnTranCost new parameter, 
  *IF gfStyCrl('I',lcStyle,lcWareCode,lcDyelot,ldIssDate,laData[1],;
              @laIssued,lnIssCost,'',.F.,'',0,'','',@laGlArray)=0  
  PRIVATE lnCost,lnVariance
  lnCost = IIF(laSetups[10,2]='S',STYLE.TOTCOST,lnIssCost)
  IF gfStyCrl('I',lcStyle,lcWareCode,lcDyelot,ldIssDate,laData[1],;
              @laIssued,lnCost   ,'',.F.,'',0,'','',@laGlArray,.F.,.F.,.F.,.F.,lnIssCost) = 0  
    *B608010,1 TMI [End  ] 
    *E301248 (End)
    RETURN
  ENDIF  
  *B608010,3 comment this part as per Wael ,  pls check the ticket # T20070129.0001 , the Frances response dated 04/18/2007 06:26 PM
  *B608010,1 TMI [Start] if return cost is changed than the issue cost in standard/LF costing method then add gl-entry with the difference  
  *-*lnVariance = lnIssCost - STYINVJL.nCost
  *-*IF lnVariance <> 0 
  *-*  PRIVATE lcGLTrnTyp,lcHdrFile,lcSvLnkOrd,lcLnkCod
  *-*  lcSvLnkOrd = ORDER('GL_LINK')
  *-*  SET ORDER TO GL_LINK IN GL_LINK
  *-*  lcHdrFile  = IIF(lcTranType = 'M' ,'CUTTKTH','POSHDR')
  *-*  lcSvCode   = IIF(lcTranType = 'M' ,CUTTKTH.CUTTKT,POSHDR.PO)
  *-*  lcLnkCod   = IIF(!EMPTY(&lcHdrFile..Link_Code),&lcHdrFile..Link_Code,'DEFDEF')
  *-*  =SEEK(lcLnkCod+'022','GL_LINK')
  *-*  DO GlDist WITH lcLnkCod,IIF(EMPTY(GL_LINK.GlAcnt),'019','022'),;
  *-*                 laIssued[9]*lnVariance,'IA',lcSvCode,gdSysDate,;
  *-*                 lcGlYear,lcGlPeriod,lcGlDTemp
  *-*  SET ORDER TO &lcSvLnkOrd IN GL_LINK
  *-*ENDIF
  *-**B608010,1 TMI [End  ] 
  *B608010,3 TMI [End  ] 

  *B039660,1 NNA 06/06/2006 (Begin) convert next trigger to be done within Binmain.Prg instead of Davmain.prg 
  *C123853,1 MHM 01/15/2005  Update Qtys to there files 'BinInvJl & WHBINLOC '[Start]
  IF ASCAN(laEvntTrig , PADR('DLUPDQTY',10)) <> 0
    =gfDoTriger('POCSSH',PADR('DLUPDQTY',10)) 
  ENDIF  
  *C123853,1 MHM 01/15/2005  [End]
  *B039660,1 NNA (End)

  IF laSetups[8,2]='Y' .AND. Style.cDye_Flg='Y' .AND. ;
    SEEK(lcStyle+lcWareCode+lcDyelot,'StyDye') AND StyDye.TotStk < 0
    *E300725,1 Message : 38034
    *E300725,1 Inventory of style/dyelot: xxx/xxx/xxx has gone below zero
    *E300725,1 Button : 00000
    *E300725,1 Ok
    =gfModalGen('INM38034B00000','ALERT','Style/Dyelot : '+TRIM(lcStyle)+'/'+TRIM(lcDyelot))
  ENDIF
  IF laSetups[2,2]='Y' AND SEEK(lcStyle+lcWareCode+SPACE(10),'StyDye') AND StyDye.TotStk <0
    *E300725,1 Message : 38034
    *E300725,1 Inventory of style: xxx/xxx has gone below zero
    *E300725,1 Button : 00000
    *E300725,1 Ok
    =gfModalGen('INM38034B00000','ALERT','Style : '+TRIM(lcStyle)+' in warehouse: '+lcWareCode)
  ENDIF
  IF Style.TotStk < 0
    *E300725,1 Message : 38034
    *E300725,1 Inventory of Style: xxxxx has gone below zero
    *E300725,1 Button : 00000
    *E300725,1 Ok
    =gfModalGen('INM38034B00000','ALERT','Style : '+TRIM(lcStyle))
  ENDIF
  IF !llFromDel 
*--- DOC.
*--- Start update cuttkt cost sheet file (CTKTBOM temp file)
*--- DOC.
    SELECT (lcTktSheet)
    IF SEEK(lcItemType+'1'+lcStyle+SPACE(6)+SPACE(6)+lcDyelot)
      REPLACE Used_Qty1 WITH Used_Qty1 - laIssued[1] ,;
              Used_Qty2 WITH Used_Qty2 - laIssued[2] ,;
              Used_Qty3 WITH Used_Qty3 - laIssued[3] ,;
              Used_Qty4 WITH Used_Qty4 - laIssued[4] ,;
              Used_Qty5 WITH Used_Qty5 - laIssued[5] ,;
              Used_Qty6 WITH Used_Qty6 - laIssued[6] ,;
              Used_Qty7 WITH Used_Qty7 - laIssued[7] ,;
              Used_Qty8 WITH Used_Qty8 - laIssued[8] ,;
              Used_Qty  WITH Used_Qty  - laIssued[9] ,;
              Iss_Qty1  WITH Iss_Qty1  - MIN(laIssued[1],0) ,;
              Iss_Qty2  WITH Iss_Qty2  - MIN(laIssued[2],0) ,;
              Iss_Qty3  WITH Iss_Qty3  - MIN(laIssued[3],0) ,;
              Iss_Qty4  WITH Iss_Qty4  - MIN(laIssued[4],0) ,;
              Iss_Qty5  WITH Iss_Qty5  - MIN(laIssued[5],0) ,;
              Iss_Qty6  WITH Iss_Qty6  - MIN(laIssued[6],0) ,;
              Iss_Qty7  WITH Iss_Qty7  - MIN(laIssued[7],0) ,;
              Iss_Qty8  WITH Iss_Qty8  - MIN(laIssued[8],0) ,;
              Issue_Qty WITH Issue_Qty - MIN(laIssued[9],0)
    ENDIF
    SELECT CTktBom
    IF SEEK(lcTranType+laData[1]+lcItemType+lcStyle+SPACE(6)+SPACE(6)+lcDyelot)
      REPLACE Used_Qty1 WITH Used_Qty1 - laIssued[1] ,;
              Used_Qty2 WITH Used_Qty2 - laIssued[2] ,;
              Used_Qty3 WITH Used_Qty3 - laIssued[3] ,;
              Used_Qty4 WITH Used_Qty4 - laIssued[4] ,;
              Used_Qty5 WITH Used_Qty5 - laIssued[5] ,;
              Used_Qty6 WITH Used_Qty6 - laIssued[6] ,;
              Used_Qty7 WITH Used_Qty7 - laIssued[7] ,;
              Used_Qty8 WITH Used_Qty8 - laIssued[8] ,;
              Used_Qty  WITH Used_Qty  - laIssued[9] ,;
              Iss_Qty1  WITH Iss_Qty1  - MIN(laIssued[1],0) ,;
              Iss_Qty2  WITH Iss_Qty2  - MIN(laIssued[2],0) ,;
              Iss_Qty3  WITH Iss_Qty3  - MIN(laIssued[3],0) ,;
              Iss_Qty4  WITH Iss_Qty4  - MIN(laIssued[4],0) ,;
              Iss_Qty5  WITH Iss_Qty5  - MIN(laIssued[5],0) ,;
              Iss_Qty6  WITH Iss_Qty6  - MIN(laIssued[6],0) ,;
              Iss_Qty7  WITH Iss_Qty7  - MIN(laIssued[7],0) ,;
              Iss_Qty8  WITH Iss_Qty8  - MIN(laIssued[8],0) ,;
              Issue_Qty WITH Issue_Qty - MIN(laIssued[9],0)
    ENDIF
    SELECT BomCost
    SET ORDER TO TAG Bomcstkt
    SET DELETE OFF
    IF !SEEK(lcItemType+lcTranType+laData[1]+lcStyle+SPACE(6)+SPACE(6)+;
             lcWareCode+lcDyelot+lcRSession+lcISession)
      APPEND BLANK
    ELSE 
      IF DELETED()
        BLANK
        RECALL
      ENDIF
    ENDIF
    SET DELETE ON
    
    *B605481,1 KHM 02/17/2002 (Begin) Changing the replacement of Actualize
    *B605481,1                to "N" instead of "Y".
    *REPLACE cTktNo     WITH laData[1]  ,;
            cWareCode  WITH lcWareCode ,;
            cDyelot    WITH lcDyelot   ,;
            Item       WITH lcStyle    ,;
            IClr       WITH SPACE(6)   ,;
            cBomType   WITH lcItemType ,;
            cIMTyp     WITH lcTranType ,;
            MfgCode    WITH SPACE(6)   ,;
            dTranDate  WITH ldIssDate  ,;
            cRSession  WITH lcRSession ,;
            cISession  WITH lcISession ,;
            cCostType  WITH lcCatgType ,;
            nTotQty    WITH nTotQty-laIssued[9] ,;
            nTotCst    WITH nTotCst-laIssued[9]*lnIssCost ,;
            nUnitCst   WITH IIF(nTotQty=0,0,nTotCst/nTotQty) ,;
            nTotACst   WITH nTotACst-laIssued[9]*lnIssCost ,;
            nUnitACst  WITH IIF(nTotQty=0,0,nTotACst/nTotQty) ,;
            cOprCode   WITH lcOprCode ,;
            cLotNo     WITH lcLotNo   ,;
            Actualize  WITH 'Y'
    
    REPLACE cTktNo     WITH laData[1]  ,;
            cWareCode  WITH lcWareCode ,;
            cDyelot    WITH lcDyelot   ,;
            Item       WITH lcStyle    ,;
            IClr       WITH SPACE(6)   ,;
            cBomType   WITH lcItemType ,;
            cIMTyp     WITH lcTranType ,;
            MfgCode    WITH SPACE(6)   ,;
            dTranDate  WITH ldIssDate  ,;
            cRSession  WITH lcRSession ,;
            cISession  WITH lcISession ,;
            cCostType  WITH lcCatgType ,;
            nTotQty    WITH nTotQty-laIssued[9] ,;
            nTotCst    WITH nTotCst-laIssued[9]*lnIssCost ,;
            nUnitCst   WITH IIF(nTotQty=0,0,nTotCst/nTotQty) ,;
            nTotACst   WITH nTotACst-laIssued[9]*lnIssCost ,;
            nUnitACst  WITH IIF(nTotQty=0,0,nTotACst/nTotQty) ,;
            cOprCode   WITH lcOprCode ,;
            cLotNo     WITH lcLotNo   ,;
            Actualize  WITH 'N'
    *B605481,1 KHM 02/17/2002 (End)

    IF nTotQty = 0
      DELETE
    ENDIF
    laData[22+VAL(lcItemType)-1] = laData[22+VAL(lcItemType)-1] - laIssued[9]*lnIssCost
*--- DOC.
*--- Update actual cost
*--- DOC.
    DO CASE
      CASE lcTranType = 'M'
        SELECT CUTTKTH
        =RLOCK()
        REPLACE nAct_Cost&lcItemType WITH nAct_Cost&lcItemType - laIssued[9]*lnIssCost
        UNLOCK
      *khm1
      *CASE lcTranType = 'I'
      CASE lcTranType $ 'ID'
      *khm1      
        SELECT POSHDR
        =RLOCK()
        REPLACE nAct_Cost&lcItemType WITH nAct_Cost&lcItemType - laIssued[9]*lnIssCost
        
        *B605480,1 KHM 02/14/2002 (Begin) Update the nfActCost also.
         DO CASE
           CASE lcCatgType = 'P'
              REPLACE nfActCost&lcItemType WITH (nAct_Cost&lcItemType &lcPExSign POSHDR.nPriceRat &lcPUntSin POSHDR.nCurrUnit)
            CASE !INLIST(lcCatgType ,'S','F','T')
              REPLACE nfActCost&lcItemType WITH (nAct_Cost&lcItemType &lcDExSign POSHDR.nDutyRat &lcDUntSin POSHDR.nDCurUnit)
            OTHERWISE
              REPLACE nfActCost&lcItemType WITH nAct_Cost&lcItemType
          ENDCASE                   
        *B605480,1 KHM 02/14/2002 (End)        
        
        UNLOCK
      CASE lcTranType = 'T'
        SELECT MMFGORDH
        =RLOCK()
        REPLACE nAct_Cost&lcItemType WITH nAct_Cost&lcItemType - laIssued[9]*lnIssCost
        UNLOCK
    ENDCASE
  ENDIF
  IF laSetups[1,2] = 'Y'
    SELECT (lcGlDTemp)
    REPLACE ALL GlSession WITH lcSession
    SELECT GLDIST
    APPEND FROM (gcWorkDir+lcGlDTemp)
    SELECT (lcGlDTemp)
    ZAP
  ENDIF
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfIssRetMfg
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Issue/Return Trims and Mfg operations
*!*************************************************************
*! Calls     : gfRltFld(),GLDIST
*!*************************************************************
*! Parameters: lcItemType   : Cost Item Type
*!             lcCatgType   : Cost Item Category 
*!             lcItem       : Trim
*!             lcColor      : Trim Color
*!             lcMfgCode    : Mfg Code
*!             lcWareCode   : Warehouse
*!             lcDyelot     : Dyelot
*!             lnIssued     : Issued/Return Quantity
*!             lnIssCost    : Issued/Return unit cost
*!             ldIssDate    : Issue/Return Date
*!             lcOprCode    : Operation
*!             lcLotNo      : Lot#
*!             lcRSession   : Receiving Session#
*!             lcISession   : Issue Session#
*!             llFromDel    : Called upon Deleting The Cost Sheet
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfIssRetMfg()
*!*************************************************************
FUNCTION lfIssRetMfg
PARAMETERS lcItemType,lcCatgType,lcItem,lcColor,lcMfgCode,lcWareCode,;
           lcDyelot,lnIssued,lnIssCost,ldIssDate,lcOprCode,lcLotNo,;
           lcRSession,lcISession,llFromDel
PRIVATE lnAlias

lnAlias = SELECT()
IF lnIssued <> 0
*--- DOC.
*--- If this function is called to return/issue not to delete cost sheet
*--- and it is update each of the following
*--- 1) CTKTBOM
*--- 2) BOMLINE
*--- 3) CUTTKTH ,POSHDR ,..
*--- DOC.
  IF !llFromDel
    SELECT CTktBom
    *--- DOC.
    *--- Add the issued qty to the used qty
    *--- DOC.
    IF SEEK(lcTranType+laData[1]+lcItemType+PADR(lcItem,19)+lcColor+lcMfgCode+lcDyelot)
      REPLACE Used_Qty  WITH Used_Qty  + lnIssued ,;
              Issue_Qty WITH Issue_Qty + MAX(lnIssued,0)
    ENDIF
    SELECT (lcTktSheet) 
    IF SEEK(lcItemType+'1'+PADR(lcItem,19)+lcColor+lcMfgCode+lcDyelot)
      REPLACE Used_Qty  WITH Used_Qty  + lnIssued ,;
              Issue_Qty WITH Issue_Qty + MAX(lnIssued,0)
    ENDIF
    SELECT BomCost
    SET DELETE OFF
    IF !SEEK(lcItemType+lcTranType+laData[1]+SPACE(19)+SPACE(6)+lcMfgCode+;
             lcWareCode+lcDyelot+lcRSession+lcISession)
      APPEND BLANK
    ELSE
      IF DELETED()
        BLANK
        RECALL
      ENDIF
    ENDIF
    SET DELETE ON
    *B605481,1 KHM 02/17/2002 (Begin) Changing the replacement of Actualize
    *B605481,1                to "N" instead of "Y".
    *REPLACE cTktNo    WITH laData[1]  ,;
            cWareCode WITH lcWareCode ,;
            cDyelot   WITH lcDyelot   ,;
            Item      WITH lcItem     ,;
            IClr      WITH lcColor    ,;
            cBomType  WITH lcItemType ,;
            cIMTyp    WITH lcTranType ,;
            MfgCode   WITH lcMfgCode  ,;
            dTranDate WITH ldIssDate  ,;
            cRSession WITH lcRSession ,;
            cISession WITH lcISession ,;
            cCostType WITH lcCatgType ,;
            nTotQty   WITH nTotQty+lnIssued ,;
            nTotCst   WITH nTotCst+lnIssued*lnIssCost ,;
            nUnitCst  WITH IIF(nTotQty=0,0,nTotCst/nTotQty) ,;
            nTotACst  WITH nTotACst+lnIssued*lnIssCost ,;
            nUnitACst WITH IIF(nTotQty=0,0,nTotACst/nTotQty) ,;
            cOprCode  WITH lcOprCode  ,;
            cLotNo    WITH lcLotNo    ,;
            Actualize WITH 'Y'

    REPLACE cTktNo    WITH laData[1]  ,;
            cWareCode WITH lcWareCode ,;
            cDyelot   WITH lcDyelot   ,;
            Item      WITH lcItem     ,;
            IClr      WITH lcColor    ,;
            cBomType  WITH lcItemType ,;
            cIMTyp    WITH lcTranType ,;
            MfgCode   WITH lcMfgCode  ,;
            dTranDate WITH ldIssDate  ,;
            cRSession WITH lcRSession ,;
            cISession WITH lcISession ,;
            cCostType WITH lcCatgType ,;
            nTotQty   WITH nTotQty+lnIssued ,;
            nTotCst   WITH nTotCst+lnIssued*lnIssCost ,;
            nUnitCst  WITH IIF(nTotQty=0,0,nTotCst/nTotQty) ,;
            nTotACst  WITH nTotACst+lnIssued*lnIssCost ,;
            nUnitACst WITH IIF(nTotQty=0,0,nTotACst/nTotQty) ,;
            cOprCode  WITH lcOprCode  ,;
            cLotNo    WITH lcLotNo    ,;
            Actualize WITH 'N'
    *B605481,1 KHM 02/17/2002 (End)
    
    IF nTotQty = 0
      DELETE
    ENDIF
    laData[22+VAL(lcItemType)-1] = laData[22+VAL(lcItemType)-1] + lnIssued*lnIssCost
    DO CASE
      CASE lcTranType = 'M'
        SELECT CUTTKTH
        =RLOCK()
        REPLACE nAct_Cost&lcItemType WITH nAct_Cost&lcItemType + lnIssued*lnIssCost
        UNLOCK
      *khm1
      *CASE lcTranType = 'I'
      CASE lcTranType $ 'ID'
      *khm1
      
        SELECT POSHDR
        =RLOCK()
        REPLACE nAct_Cost&lcItemType WITH nAct_Cost&lcItemType + lnIssued*lnIssCost
        UNLOCK
      CASE lcTranType = 'T'
        SELECT MMFGORDH
        =RLOCK()
        REPLACE nAct_Cost&lcItemType WITH nAct_Cost&lcItemType + lnIssued*lnIssCost
        UNLOCK
    ENDCASE
  ENDIF
  IF laSetups[1,2] = 'Y'
    =gfRltFld(lcMfgCode,@laMfgRFld,'MFGCODE')
    *E300725,1 Liability for non material cost ( 018 )
    DO GLDIST WITH laData[29],'018',-(lnIssued*lnIssCost),'NL',laData[1],;
                    ldIssDate,lcGlYear,lcGlPeriod,lcGlDTemp,lcMfgGlAcnt
    *E300725,1 W.I.P. ( 013 )
    DO GLDIST WITH laData[29],'013',lnIssued*lnIssCost,'NL',laData[1],;
                   ldIssDate,lcGlYear,lcGlPeriod,lcGlDTemp 
    m.cWipAcnt = &lcGlDTemp..GlAccount
    SELECT BomCost
    =RLOCK()
    REPLACE cWipAcnt   WITH m.cWipAcnt ,;
            cLbltyAcnt WITH lcMfgGlAcnt
    UNLOCK
    SELECT (lcGlDTemp)
    REPLACE ALL GlSession WITH lcSession
    SELECT GLDIST
    APPEND FROM (gcWorkDir+lcGlDTemp)
    SELECT (lcGlDTemp)
    ZAP
  ENDIF
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvNewLot
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Add new Lot
*!*************************************************************
*! Calls     : gfRltFld,MFLOTS.SPX,lfvGenLot,lfRefTotal,lfShOprDt
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvNewLot()
*!*************************************************************
FUNCTION lfvNewLot

*B605240,1 KHM 12/25/2001 (Begin) Adding lcSizesVal 
*PRIVATE lcOprCode,lcLotNo,llInHouse,lcContCode,ldTranDate,ldDueDate,lcItemDesc,lcCurrTag
PRIVATE lcOprCode,lcLotNo,llInHouse,lcContCode,ldTranDate,ldDueDate,lcItemDesc,lcCurrTag,;
        lcSizesVal
*B605240,1 KHM 12/25/2001 (End)         

lcLotNo    = PADL(&lcOprHdr..nNxtLotNo,2,'0')
lcOprCode  = &lcOprHdr..cOprCode
=gfRltFld(lcOprCode,@laMfgRFld,'MFGCODE')
lcItemDesc = ''
ldTranDate = gdSysDate
ldDueDate  = gdSysDate+lnLeadTime
lcContCode = &lcOprHdr..cContCode
lcContName = &lcOprHdr..cContName
llInHouse  = &lcOprHdr..lInHouse
*--- DOC.
*--- IF the operation related filed not in house and AP module installed
*--- disable the vendor name object
*--- DOC.
lcNameStat= IIF(!llInHouse .AND. 'AP' $ gcCmpModules,'DISABLE','ENABLE')
lcCurrTag = ORDER(lcOprDet)
SET ORDER TO TAG ITEM IN (lcOprDet)
*E301077,55 Inhance openning files to speed up transaction
*SELECT (lcLotsDet)
*ZAP
*B603243,1 AMM  Add the dyelot field and include it in the index
*CREATE TABLE (gcWorkDir+lcLotsDet) ;
(cSelect C(1),Item C(19),Color C(6),nBudget1 N(6),nBudget2 N(6),nBudget3 N(6),nBudget4 N(6),;
 nBudget5 N(6),nBudget6 N(6),nBudget7 N(6),nBudget8 N(6),nTotBudget N(7),;
 nIssued1 N(6),nIssued2 N(6),nIssued3 N(6),nIssued4 N(6),nIssued5 N(6),;
 nIssued6 N(6),nIssued7 N(6),nIssued8 N(6),nTotIss N(7),;
 nToIssue1 N(5),nToIssue2 N(5),nToIssue3 N(5),nToIssue4 N(5),nToIssue5 N(5),;
 nToIssue6 N(5),nToIssue7 N(5),nToIssue8 N(5),nTotToIss N(6),DueDate D,lCanRemain L)
*INDEX ON Item+Color TAG (lcLotsDet)

*B126396,1 NNA 03/31/2005 (Begin) if we're in the Material Manufucturing Order then make the field
*B126396,1 NNA            of total=three decimals.
STORE '' TO lcTotalFld
lcTotalFld = IIF(lcTranType='T','nTotToIss N(6,3)','nTotToIss N(6)')
*CREATE TABLE (gcWorkDir+lcLotsDet) ;
(cSelect C(1),Item C(19),Color C(6),cDyelot C(10),nBudget1 N(6),nBudget2 N(6),nBudget3 N(6),nBudget4 N(6),;
 nBudget5 N(6),nBudget6 N(6),nBudget7 N(6),nBudget8 N(6),nTotBudget N(7),;
 nIssued1 N(6),nIssued2 N(6),nIssued3 N(6),nIssued4 N(6),nIssued5 N(6),;
 nIssued6 N(6),nIssued7 N(6),nIssued8 N(6),nTotIss N(7),;
 nToIssue1 N(5),nToIssue2 N(5),nToIssue3 N(5),nToIssue4 N(5),nToIssue5 N(5),;
 nToIssue6 N(5),nToIssue7 N(5),nToIssue8 N(5),nTotToIss N(6),DueDate D,lCanRemain L)
CREATE TABLE (gcWorkDir+lcLotsDet) ;
(cSelect C(1),Item C(19),Color C(6),cDyelot C(10),nBudget1 N(6),nBudget2 N(6),nBudget3 N(6),nBudget4 N(6),;
 nBudget5 N(6),nBudget6 N(6),nBudget7 N(6),nBudget8 N(6),nTotBudget N(7),;
 nIssued1 N(6),nIssued2 N(6),nIssued3 N(6),nIssued4 N(6),nIssued5 N(6),;
 nIssued6 N(6),nIssued7 N(6),nIssued8 N(6),nTotIss N(7),;
 nToIssue1 N(5),nToIssue2 N(5),nToIssue3 N(5),nToIssue4 N(5),nToIssue5 N(5),;
 nToIssue6 N(5),nToIssue7 N(5),nToIssue8 N(5),&lcTotalFld,DueDate D,lCanRemain L)
*B126396,1 NNA (End)

INDEX ON Item+Color+cDyelot TAG (lcLotsDet)
*B603243,1 AMM  end
*E301077,55 (End)

IF lcTranType='T'
  SELECT (lcTmpTkt)
  SCAN
    IF laData[28] = 'M'
      SELECT (lcLotsDet)
      *B603243,1 AMM  Add dyelot field
      *IF !SEEK(&lcTmpTkt..Item+&lcTmpTkt..Color)
        *INSERT INTO (lcLotsDet) (Item,Color,DueDate) VALUES (&lcTmpTkt..Item,&lcTmpTkt..Color,ldDueDate)
        *IF SEEK(lcTranType+laData[1]+lcOprCode+&lcTmpTkt..Item+&lcTmpTkt..Color,lcOprDet)
      IF !SEEK(&lcTmpTkt..Item+&lcTmpTkt..Color+&lcTmpTkt..cDyelot)
        INSERT INTO (lcLotsDet) (Item,Color,cDyelot,DueDate) VALUES (&lcTmpTkt..Item,&lcTmpTkt..Color,&lcTmpTkt..cDyelot,ldDueDate)
        IF SEEK(lcTranType+laData[1]+lcOprCode+&lcTmpTkt..Item+&lcTmpTkt..Color+&lcTmpTkt..cDyelot,lcOprDet)
      *B603243,1 AMM  end
          SELECT (lcOprDet)
          *B603243,1 AMM  add the dyelot field
          *SCAN REST WHILE cIMTYp+cTktNo+cOprCode+Item+Color = ;
                          lcTranType+laData[1]+lcOprCode+&lcTmpTkt..Item+&lcTmpTkt..Color ;
                    FOR   INLIST(TranCd,'1','4')
          SCAN REST WHILE cIMTYp+cTktNo+cOprCode+Item+Color+cDyelot = ;
                          lcTranType+laData[1]+lcOprCode+&lcTmpTkt..Item+&lcTmpTkt..Color+&lcTmpTkt..cDyelot ;
                    FOR   INLIST(TranCd,'1','4')
          *B603243,1 AMM  end
            SELECT (lcLotsDet)
            REPLACE nTotIss  WITH nTotIss  + IIF(&lcOprDet..TranCd='1',&lcOprDet..nLotTotQty,-&lcOprDet..nLotTotQty)
          ENDSCAN
        ENDIF
      ENDIF
      SELECT (lcLotsDet)
      REPLACE nTotBudget WITH nTotBudget + &lcTmpTkt..nTotQTy
    ELSE
      *B603243,1 AMM  Add dyelot field
      *IF !SEEK(lcTranType+laData[1]+lcOprCode+&lcTmpTkt..Item+&lcTmpTkt..Color,lcOprDet)
        *SELECT (lcLotsDet)
        *IF !SEEK(&lcTmpTkt..Item+&lcTmpTkt..Color)
          *INSERT INTO (lcLotsDet) (Item,Color,DueDate,cSelect) VALUES (&lcTmpTkt..Item,&lcTmpTkt..Color,ldDueDate,'»')
        *ENDIF
      IF !SEEK(lcTranType+laData[1]+lcOprCode+&lcTmpTkt..Item+&lcTmpTkt..Color+&lcTmpTkt..cDyelot,lcOprDet)
        SELECT (lcLotsDet)
        IF !SEEK(&lcTmpTkt..Item+&lcTmpTkt..Color+&lcTmpTkt..cDyelot)
          INSERT INTO (lcLotsDet) (Item,Color,cDyelot,DueDate,cSelect) VALUES (&lcTmpTkt..Item,&lcTmpTkt..Color,&lcTmpTkt..cDyelot,ldDueDate,'»')
        ENDIF
      *B603243,1 AMM  end
        REPLACE nTotToIss WITH nTotToIss + &lcTmpTkt..nTotQty
      ENDIF
    ENDIF
  ENDSCAN
ELSE
  SELECT (lcTmpTkt)
  *B802700,1 Inform the user that he can't issue operation has no seq.[Start]
  PRIVATE llDone
  llDone=.F.
  *B802700,1 Inform the user that he can't issue operation has no seq.[End..]  
  SCAN FOR IIF(laData[28]='M',.T.,cFrstOpr=lcOprCode)
    *B802700,1 Inform the user that he can't issue operation has no seq.[Start]
      llDone=.T.
    *B802700,1 Inform the user that he can't issue operation has no seq.[End..]  
    SELECT (lcDetFile)
    =SEEK(&lcTmpTkt..Item+SPACE(6)+STR(&lcTmpTkt..LineNo,6))
    LOCATE REST WHILE Style+SClr+STR(LineNo,6)+cBomTyp+cShowType+Item+IClr+MfgCode=&lcTmpTkt..Item+SPACE(6)+STR(&lcTmpTkt..LineNo,6) ;
    FOR cShowType+Item+IClr+MfgCode='1'+SPACE(19)+SPACE(6)+lcOprCode
    IF FOUND()
      
      *B605240,1 KHM 12/25/2001 (Begin) Initializing lcSizesVal
      lcSizesVal = IIF(EMPTY(cSizes), '12345678',ALLTRIM(cSizes))
      *B605240,1 KHM 12/25/2001 (End)
      
      IF laData[28] = 'M'
        SELECT (lcLotsDet)
        *B603243,1 AMM  Add the dyelot field
        *IF !SEEK(&lcTmpTkt..Item)
          *INSERT INTO (lcLotsDet) (Item,DueDate) VALUES (&lcTmpTkt..Item,ldDueDate)
          *IF SEEK(lcTranType+laData[1]+lcOprCode+&lcTmpTkt..Item,lcOprDet)
        IF !SEEK(&lcTmpTkt..Item+&lcTmpTkt..Color+&lcTmpTkt..cDyelot)
          INSERT INTO (lcLotsDet) (Item,cDyelot,DueDate) VALUES (&lcTmpTkt..Item,&lcTmpTkt..cDyelot,ldDueDate)
          IF SEEK(lcTranType+laData[1]+lcOprCode+&lcTmpTkt..Item+&lcTmpTkt..Color+&lcTmpTkt..cDyelot,lcOprDet)
        *B603243,1 AMM  end
        
            SELECT (lcOprDet)
            *B603243,1 AMM make the operation done by style/color/dyelot
            *SCAN REST WHILE cIMTYp+cTktNo+cOprCode+Item = ;
                            lcTranType+laData[1]+lcOprCode+&lcTmpTkt..Item ;
                      FOR   INLIST(TranCd,'1','4')
            SCAN REST WHILE cIMTYp+cTktNo+cOprCode+Item+Color+cDyelot = ;
                            lcTranType+laData[1]+lcOprCode+&lcTmpTkt..Item+&lcTmpTkt..Color+&lcTmpTkt..cDyelot ;
                      FOR   INLIST(TranCd,'1','4')
            *B603243,1 AMM  end
              SELECT (lcLotsDet)
*--- DOC.
*--- If the trancd is '1' == budget then increase the issued qty else decrease it.
*--- DOC.
              *B605240,1 KHM 12/25/2001 (Begin) Changing the way of replacing.
              *REPLACE nIssued1 WITH nIssued1 + IIF(&lcOprDet..TranCd='1',&lcOprDet..nLotQty1,-&lcOprDet..nLotQty1) ,;
                      nIssued2 WITH nIssued2 + IIF(&lcOprDet..TranCd='1',&lcOprDet..nLotQty2,-&lcOprDet..nLotQty2) ,;
                      nIssued3 WITH nIssued3 + IIF(&lcOprDet..TranCd='1',&lcOprDet..nLotQty3,-&lcOprDet..nLotQty3) ,;
                      nIssued4 WITH nIssued4 + IIF(&lcOprDet..TranCd='1',&lcOprDet..nLotQty4,-&lcOprDet..nLotQty4) ,;
                      nIssued5 WITH nIssued5 + IIF(&lcOprDet..TranCd='1',&lcOprDet..nLotQty5,-&lcOprDet..nLotQty5) ,;
                      nIssued6 WITH nIssued6 + IIF(&lcOprDet..TranCd='1',&lcOprDet..nLotQty6,-&lcOprDet..nLotQty6) ,;
                      nIssued7 WITH nIssued7 + IIF(&lcOprDet..TranCd='1',&lcOprDet..nLotQty7,-&lcOprDet..nLotQty7) ,;
                      nIssued8 WITH nIssued8 + IIF(&lcOprDet..TranCd='1',&lcOprDet..nLotQty8,-&lcOprDet..nLotQty8)

              FOR lnCntr = 1 TO 8
                lcCntrVal = STR(lnCntr,1)
                IF lcCntrVal $ lcSizesVal
                  REPLACE nIssued&lcCntrVal WITH nIssued&lcCntrVal +;
                                           IIF(&lcOprDet..TranCd='1',;
                                           &lcOprDet..nLotQty&lcCntrVal,;
                                           -&lcOprDet..nLotQty&lcCntrVal)
                ENDIF           
              ENDFOR 
              *B605240,1 KHM 12/25/2001 (End)

            ENDSCAN
          ENDIF  
        ENDIF
        SELECT (lcLotsDet)
*--- DOC.
*--- Replace the issued with the issued if > zero else replace with 0
*--- DOC.
        REPLACE nIssued1 WITH MAX(nIssued1,0),;
                nIssued2 WITH MAX(nIssued2,0),;
                nIssued3 WITH MAX(nIssued3,0),;
                nIssued4 WITH MAX(nIssued4,0),;
                nIssued5 WITH MAX(nIssued5,0),;
                nIssued6 WITH MAX(nIssued6,0),;
                nIssued7 WITH MAX(nIssued7,0),;
                nIssued8 WITH MAX(nIssued8,0),;
                nTotIss  WITH nIssued1+nIssued2+nIssued3+nIssued4+nIssued5+nIssued6+nIssued7+nIssued8
*--- DOC.
*--- add qty from the ticke line temp file to the budget
*--- DOC.
        *B605240,1 KHM 12/25/2001 (Begin) Changing the way of replacement.
        *REPLACE nBudget1   WITH nBudget1   + &lcTmpTkt..nQty1 ,;
                nBudget2   WITH nBudget2   + &lcTmpTkt..nQty2 ,;
                nBudget3   WITH nBudget3   + &lcTmpTkt..nQty3 ,;
                nBudget4   WITH nBudget4   + &lcTmpTkt..nQty4 ,;
                nBudget5   WITH nBudget5   + &lcTmpTkt..nQty5 ,;
                nBudget6   WITH nBudget6   + &lcTmpTkt..nQty6 ,;
                nBudget7   WITH nBudget7   + &lcTmpTkt..nQty7 ,;
                nBudget8   WITH nBudget8   + &lcTmpTkt..nQty8 ,;
                nTotBudget WITH nTotBudget + &lcTmpTkt..nTotQty

        FOR lnCntr = 1 TO 8
          lcCntrVal = STR(lnCntr,1)
          IF lcCntrVal $ lcSizesVal
            REPLACE nBudget&lcCntrVal WITH nBudget&lcCntrVal + &lcTmpTkt..nQty&lcCntrVal
          ENDIF           
        ENDFOR 
        REPLACE nTotBudget WITH nBudget1+nBudget2+nBudget3+nBudget4+nBudget5+;
                                nBudget6+nBudget7+nBudget8 
       *B605240,1 KHM 12/25/2001 (End)
      ELSE
        *B603243,1 AMM  Add dyelot field
        *IF !SEEK(lcTranType+laData[1]+lcOprCode+&lcTmpTkt..Item+SPACE(6),lcOprDet)
          *SELECT (lcLotsDet)
          *IF !SEEK(&lcTmpTkt..Item)
            *INSERT INTO (lcLotsDet) (Item,DueDate,cSelect) VALUES (&lcTmpTkt..Item,ldDueDate,'»')
          *ENDIF
        IF !SEEK(lcTranType+laData[1]+lcOprCode+&lcTmpTkt..Item+SPACE(6)+&lcTmpTkt..cDyelot,lcOprDet)
          SELECT (lcLotsDet)
          IF !SEEK(&lcTmpTkt..Item+&lcTmpTkt..Color+&lcTmpTkt..cDyelot)
            INSERT INTO (lcLotsDet) (Item,cDyelot,DueDate,cSelect) VALUES (&lcTmpTkt..Item,&lcTmpTkt..cDyelot,ldDueDate,'»')
          ENDIF
        *B603243,1 AMM  end
          
          *B605240,1 KHM 12/25/2001 (Being) Changing the way of replacement.
          *REPLACE nToIssue1 WITH nToIssue1 + &lcTmpTkt..nQty1 ,;
                  nToIssue2 WITH nToIssue2 + &lcTmpTkt..nQty2 ,;
                  nToIssue3 WITH nToIssue3 + &lcTmpTkt..nQty3 ,;
                  nToIssue4 WITH nToIssue4 + &lcTmpTkt..nQty4 ,;
                  nToIssue5 WITH nToIssue5 + &lcTmpTkt..nQty5 ,;
                  nToIssue6 WITH nToIssue6 + &lcTmpTkt..nQty6 ,;
                  nToIssue7 WITH nToIssue7 + &lcTmpTkt..nQty7 ,;
                  nToIssue8 WITH nToIssue8 + &lcTmpTkt..nQty8 ,;
                  nTotToIss WITH nTotToIss + &lcTmpTkt..nTotQty

          FOR lnCntr = 1 TO 8
            lcCntrVal = STR(lnCntr,1)
            IF lcCntrVal $ lcSizesVal
              REPLACE nToIssue&lcCntrVal WITH nToIssue&lcCntrVal + &lcTmpTkt..nQty&lcCntrVal
            ENDIF           
          ENDFOR 
          REPLACE nTotToIss WITH nTotToIss + nToIssue1+nToIssue2+nToIssue3+;
                                 nToIssue4+nToIssue5+nToIssue6+nToIssue7+;
                                 nToIssue8   
          *B605240,1 KHM 12/25/2001 (End)
          
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
  *B802700,1 Inform the user that he can't issue operation has no seq.[Start]
  =IIF(llDone,.T.,gfModalGen('INM38186B00000','ALERT'))
  *B802700,1 Inform the user that he can't issue operation has no seq.[End..]    
ENDIF  
SELECT (lcLotsDet)
GO TOP
IF !EOF()
  IF laData[28] = 'M'
    IF lcTranType <> 'T'
      SET RELATION TO ITEM INTO STYLE
    *E#301235,1
    ELSE
      *E301235,4 WAB - the lenght of Fabric.Fabric = 7
      *E301235,4 WAB - START
      *SET RELATION TO ITEM+COLOR INTO FABRIC
      SET RELATION TO PADR(ITEM,7)+COLOR INTO FABRIC
      *E301235,4 WAB - END
    ENDIF  
    DO (gcScrDir+"MFLOTS.SPX") WITH lcOprCode,lcLotNo
    IF lcTranType <> 'T'
      SELECT (lcLotsDet)
      SET RELATION OFF INTO STYLE
    *E#301235,1
    ELSE
      *E301235,4 WAB - release the relation not to create relation
      *E301235,4 WAB - START
      *SET RELATION TO ITEM+COLOR INTO FABRIC
      SELECT (lcLotsDet)
      SET RELATION OFF INTO FABRIC
      *E301235,4 WAB - END
    ENDIF  
  ELSE
    =lfvGenLot()
  ENDIF
ENDIF
IF SEEK(lcTranType+laData[1],lcOprDet)
  SHOW GETS WINDOW (lcWinCh24) ENABLE ONLY
  IF laData[28] = 'S'
    SHOW GET pbNewLot DISABLE
  ENDIF
ENDIF
IF lnActFolder=2
  =lfRefTotal()
  =lfShOprDt()
ENDIF  
SET ORDER TO TAG lcCurrTag IN (lcOprDet)

*!*************************************************************
*! Name      : lfvGenLot
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Generate new lot
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvGenLot()
*!*************************************************************
FUNCTION lfvGenLot

*B602712,1 Keep track of allocated quantity while cancel ticket remaining quantity.
llRelAll = .F.
*B602712,1 (End)

SELECT (lcLotsDet)
SCAN FOR !EMPTY(cSelect)
*--- DOC.
*--- Update the detail operatoin temp file 
*--- DOC.
  SELECT (lcOprDet)
  APPEND BLANK
  *B801929,1 Update operation sequence.
  *B603243,1 AMM  Add the update of the dyelot field
  REPLACE cImTyp     WITH lcTranType ,;
          cTktNo     WITH laData[1]  ,;
          cOprCode   WITH lcOprCode  ,;
          cLotNo     WITH lcLotNo    ,;
          Item       WITH &lcLotsDet..Item ,;
          Color      WITH &lcLotsDet..Color ,;
          lInHouse   WITH llInHouse ,;
          cContCode  WITH lcContCode ,;
          cContName  WITH lcContName ,;
          TranCd     WITH '1'        ,;
          dTranDate  WITH ldTranDate ,;
          DueDate    WITH &lcLotsDet..DueDate ,;
          nLotQty1   WITH &lcLotsDet..nToIssue1 ,;
          nLotQty2   WITH &lcLotsDet..nToIssue2 ,;
          nLotQty3   WITH &lcLotsDet..nToIssue3 ,;
          nLotQty4   WITH &lcLotsDet..nToIssue4 ,;
          nLotQty5   WITH &lcLotsDet..nToIssue5 ,;
          nLotQty6   WITH &lcLotsDet..nToIssue6 ,;
          nLotQty7   WITH &lcLotsDet..nToIssue7 ,;
          nLotQty8   WITH &lcLotsDet..nToIssue8 ,;
          nLotTotQty WITH &lcLotsDet..nTotToIss ,;
          cOperSeq   WITH &lcOprHdr..cOperSeq   ,;
          cDyelot    WITH &lcLotsDet..cDyelot
  IF &lcLotsDet..lCanRemain
  
    SELECT (lcTmpTkt)
    =SEEK(&lcLotsDet..Item+&lcLotsDet..Color)
    *B603243,1 AMM make the operation done by style/color/dyelot
    *COUNT REST WHILE Item+Color = &lcLotsDet..Item+&lcLotsDet..Color TO lnRecords
    COUNT REST WHILE Item+Color = &lcLotsDet..Item+&lcLotsDet..Color ;
               FOR cDyelot = &lcLotsDet..cDyelot TO lnRecords
    *B603243,1 AMM  end
    IF lnRecords=1 
      =SEEK(&lcLotsDet..Item+&lcLotsDet..Color)
      *B603243,1 AMM make the operation done by style/color/dyelot
      IF llUseDyelot
        LOCATE REST WHILE Item+Color = &lcLotsDet..Item+&lcLotsDet..Color ;
                    FOR cDyelot = &lcLotsDet..cDyelot
      ENDIF
      *B603243,1 AMM  end
      REPLACE nQty1   WITH nQty1   - MAX(&lcLotsDet..nBudget1-&lcLotsDet..nIssued1-&lcLotsDet..nToIssue1,0) ,;
              nQty2   WITH nQty2   - MAX(&lcLotsDet..nBudget2-&lcLotsDet..nIssued2-&lcLotsDet..nToIssue2,0) ,;
              nQty3   WITH nQty3   - MAX(&lcLotsDet..nBudget3-&lcLotsDet..nIssued3-&lcLotsDet..nToIssue3,0) ,;
              nQty4   WITH nQty4   - MAX(&lcLotsDet..nBudget4-&lcLotsDet..nIssued4-&lcLotsDet..nToIssue4,0) ,;
              nQty5   WITH nQty5   - MAX(&lcLotsDet..nBudget5-&lcLotsDet..nIssued5-&lcLotsDet..nToIssue5,0) ,;
              nQty6   WITH nQty6   - MAX(&lcLotsDet..nBudget6-&lcLotsDet..nIssued6-&lcLotsDet..nToIssue6,0) ,;
              nQty7   WITH nQty7   - MAX(&lcLotsDet..nBudget7-&lcLotsDet..nIssued7-&lcLotsDet..nToIssue7,0) ,;
              nQty8   WITH nQty8   - MAX(&lcLotsDet..nBudget8-&lcLotsDet..nIssued8-&lcLotsDet..nToIssue8,0) ,;
              nTotQty WITH nTotQty - MAX(&lcLotsDet..nTotBudget-&lcLotsDet..nTotIss-&lcLotsDet..nTotToIss,0)
    ELSE
      *B603243,1 AMM make the operation done by style/color/dyelot
      *lcItem = &lcLotsDet..Item+&lcLotsDet..Color
      *SELECT *,00000 AS nCan1,00000 AS nCan2,00000 AS nCan3,00000 AS nCan4,;
               00000 AS nCan5,00000 AS nCan6,00000 AS nCan7,00000 AS nCan8,;
               000000 AS nTotCan, RECNO() AS nBudRecNo FROM (lcTmpTkt) WHERE ITEM+COLOR=lcItem;
               INTO DBF (gcWorkDir+lcisslogfile)
      lcItem = &lcLotsDet..Item+&lcLotsDet..Color+&lcLotsDet..cDyelot
      SELECT *,00000 AS nCan1,00000 AS nCan2,00000 AS nCan3,00000 AS nCan4,;
               00000 AS nCan5,00000 AS nCan6,00000 AS nCan7,00000 AS nCan8,;
               000000 AS nTotCan, RECNO() AS nBudRecNo FROM (lcTmpTkt) WHERE ITEM+COLOR+CDYELOT=lcItem;
               INTO DBF (gcWorkDir+lcisslogfile)
      *B603243,1 AMM  end
      STORE MAX(&lcLotsDet..nBudget1-&lcLotsDet..nIssued1-&lcLotsDet..nToIssue1,0) TO lnCan1,lnBal1
      STORE MAX(&lcLotsDet..nBudget2-&lcLotsDet..nIssued2-&lcLotsDet..nToIssue2,0) TO lnCan2,lnBal2
      STORE MAX(&lcLotsDet..nBudget3-&lcLotsDet..nIssued3-&lcLotsDet..nToIssue3,0) TO lnCan3,lnBal3
      STORE MAX(&lcLotsDet..nBudget4-&lcLotsDet..nIssued4-&lcLotsDet..nToIssue4,0) TO lnCan4,lnBal4
      STORE MAX(&lcLotsDet..nBudget5-&lcLotsDet..nIssued5-&lcLotsDet..nToIssue5,0) TO lnCan5,lnBal5
      STORE MAX(&lcLotsDet..nBudget6-&lcLotsDet..nIssued6-&lcLotsDet..nToIssue6,0) TO lnCan6,lnBal6
      STORE MAX(&lcLotsDet..nBudget7-&lcLotsDet..nIssued7-&lcLotsDet..nToIssue7,0) TO lnCan7,lnBal7
      STORE MAX(&lcLotsDet..nBudget8-&lcLotsDet..nIssued8-&lcLotsDet..nToIssue8,0) TO lnCan8,lnBal8
      STORE MAX(&lcLotsDet..nTotBudget-&lcLotsDet..nTotIss-&lcLotsDet..nTotToIss,0) TO lnTotCan,lnTotBal
      SCATTER MEMVAR
      =SEEK(m.Item,'Style') .AND. SEEK('S'+Style.Scale,'Scale')

      *B802713,1 Fix syntax error in the cancel quantity contribution screen.
      *lcConCnTit = ALLTRIM(&lcLotsDet..Item)+ALLTRIM(&lcLotsDet..Color)+'Cancelled Quantity'
      lcConCnTit = IIF(lcTranType='T','Material/Color',lcItmHdr)+' '+;
                   ALLTRIM(&lcLotsDet..Item)+ALLTRIM(&lcLotsDet..Color)+'Cancelled Quantity'
      *B802713,1 (End)
      DO (gcScrDir+"MFCONCN.SPX")
      USE IN (lcisslogfile)
    ENDIF
    *B602712,1 Keep track of allocated quantity while cancel ticket remaining quantity.
    IF lcTranType <> 'T'
      lcFile = IIF(lcTranType='M','CUTTKTL','POSLN')
      IF lcTranType='M'
        SET ORDER TO TAG CUTLIN IN (lcFile)
      ELSE
        SET ORDER TO TAG POSLN IN (lcFile)
      ENDIF
      SELECT (lcTmpTkt)
      =SEEK(&lcLotsDet..Item)
      SCAN REST WHILE Item+Color+STR(LineNo,6) = &lcLotsDet..Item
        SELECT (lcFile)
        *C200080,1 AMM Consider the dye order case
        *=SEEK(IIF(lcTranType='I','P','')+laData[1]+&lcTmpTkt..Item+STR(&lcTmpTkt..lineno,6)+'1')
        *khm1
        *=SEEK(IIF(lcTranType='I',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1]+&lcTmpTkt..Item+STR(&lcTmpTkt..lineno,6)+'1')
        =SEEK(IIF(lcTranType $ 'ID',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1]+&lcTmpTkt..Item+STR(&lcTmpTkt..lineno,6)+'1')
        *khm1
                
        *C200080,1 AMM end
        SCATTER FIELDS ORD1,ORD2,ORD3,ORD4,ORD5,ORD6,ORD7,ORD8,TotOrd TO laOrdQty
        IF (Ord1 > &lcTmpTkt..nQty1 OR Ord2 > &lcTmpTkt..nQty2 OR Ord3 > &lcTmpTkt..nQty3 OR ;
            Ord4 > &lcTmpTkt..nQty4 OR Ord5 > &lcTmpTkt..nQty5 OR Ord6 > &lcTmpTkt..nQty6 OR ;
            Ord7 > &lcTmpTkt..nQty7 OR Ord8 > &lcTmpTkt..nQty8)
*--- DOC.
*--- Decrease the open qty variable by the issued qty
*--- DOC.
          lnOpen1 = Qty1 - &lcTmpTkt..nQty1
          lnOpen2 = Qty2 - &lcTmpTkt..nQty2
          lnOpen3 = Qty3 - &lcTmpTkt..nQty3
          lnOpen4 = Qty4 - &lcTmpTkt..nQty4
          lnOpen5 = Qty5 - &lcTmpTkt..nQty5
          lnOpen6 = Qty6 - &lcTmpTkt..nQty6
          lnOpen7 = Qty7 - &lcTmpTkt..nQty7
          lnOpen8 = Qty8 - &lcTmpTkt..nQty8
          IF !lfTktAllo(&lcTmpTkt..Item,&lcFile..Dyelot,&lcFile..LineNo,.T.)
            SELECT (lcTmpTkt)
            REPLACE nQty1   WITH &lcFile..Qty1 ,;
                    nQty2   WITH &lcFile..Qty2 ,;
                    nQty3   WITH &lcFile..Qty3 ,;
                    nQty4   WITH &lcFile..Qty4 ,;
                    nQty5   WITH &lcFile..Qty5 ,;
                    nQty6   WITH &lcFile..Qty6 ,;
                    nQty7   WITH &lcFile..Qty7 ,;
                    nQty8   WITH &lcFile..Qty8 ,;
                    nTotQty WITH &lcFile..TotQty
            SELECT (lcLotsDet)
            REPLACE lCanRemain WITH .F.
          ENDIF
        ENDIF
        
        *khm1
        *SELECT IIF(lcTranType='I','POSHDR','CUTTKTH')
        SELECT IIF(lcTranType $ 'ID','POSHDR','CUTTKTH')
        *khm1
                
        =RLOCK()
        REPLACE TotOrd WITH TotOrd - &lcFile..TotOrd + laOrdQty[9]
        UNLOCK
        SELECT (lcFile)
        =RLOCK()
        REPLACE Ord1   WITH laOrdQty[1] ,;
                Ord2   WITH laOrdQty[2] ,;
                Ord3   WITH laOrdQty[3] ,;
                Ord4   WITH laOrdQty[4] ,;
                Ord5   WITH laOrdQty[5] ,;
                Ord6   WITH laOrdQty[6] ,;
                Ord7   WITH laOrdQty[7] ,;
                Ord8   WITH laOrdQty[8] ,;
                TotOrd WITH laOrdQty[9]
        UNLOCK
      ENDSCAN
    ENDIF
    *B602712,1 (End)
    
    IF laScrMode[2] AND &lcLotsDet..lCanRemain
      *khm1
      *lcFile = IIF(lcTranType='M','CUTTKTL',IIF(lcTranType='I','POSLN','MMFGORDD'))
      lcFile = IIF(lcTranType='M','CUTTKTL',IIF(lcTranType $ 'ID','POSLN','MMFGORDD'))
      *khm1
      
      SELECT (lcTmpTkt)
      lnCanQty = 0
      SCAN FOR Item+Color = &lcLotsDet..Item+&lcLotsDet..Color
        SELECT (lcFile)
        GO &lcTmpTkt..nRecNo
        =RLOCK()
        IF lcTranType='T'
          lnCanQty = lnCanQty + MAX(nMfgTotQty-&lcTmpTkt..nTotQty,0)
          REPLACE nMfgTotQty WITH &lcTmpTkt..nTotQty
        ELSE
          lnCanQty = lnCanQty + MAX(TotQty-&lcTmpTkt..nTotQty,0)
          REPLACE Qty1   WITH &lcTmpTkt..nQty1 ,;
                  Qty2   WITH &lcTmpTkt..nQty2 ,;
                  Qty3   WITH &lcTmpTkt..nQty3 ,;
                  Qty4   WITH &lcTmpTkt..nQty4 ,;
                  Qty5   WITH &lcTmpTkt..nQty5 ,;
                  Qty6   WITH &lcTmpTkt..nQty6 ,;
                  Qty7   WITH &lcTmpTkt..nQty7 ,;
                  Qty8   WITH &lcTmpTkt..nQty8 ,;
                  TotQty WITH &lcTmpTkt..nTotQty
        ENDIF
        UNLOCK
      ENDSCAN
      SELECT (lcBaseFile)
      =RLOCK()
*--- DOC.
*--- Update the cancel and open pieces
*--- DOC.
      DO CASE
        CASE lcTranType='M'
          REPLACE Pcs_Can  WITH Pcs_Can + lnCanQty ,;
                  Pcs_Opn  WITH Pcs_Opn - lnCanQty
          laData[8] = Pcs_Can
          laData[10]= Pcs_Opn
        *khm1
        *CASE lcTranType='I'
        CASE lcTranType $ 'ID'
        *khm1
        
            REPLACE Cancel WITH Cancel + lnCanQty ,;
                    Open   WITH Open   - lnCanQty
          laData[8] = Cancel
          laData[10]= Open
        CASE lcTranType='T'
          REPLACE Canceled  WITH Canceled  + lnCanQty ,;
                  nMMfgOpen WITH nMMfgOpen - lnCanQty
          laData[8] = Canceled
          laData[10]= nMMfgOpen
      ENDCASE
      UNLOCK
    ENDIF
  ENDIF
  IF laScrMode[2]
    SELECT MFGOPRDT
    APPEND BLANK
    *B603243,1 AMM  Add update of the Dyelot field
    REPLACE cImTyp     WITH lcTranType ,;
            cTktNo     WITH laData[1]  ,;
            cOprCode   WITH lcOprCode  ,;
            cLotNo     WITH lcLotNo    ,;
            Item       WITH &lcLotsDet..Item ,;
            Color      WITH &lcLotsDet..Color ,;
            lInHouse   WITH llInHouse ,;
            cContCode  WITH lcContCode ,;
            cContName  WITH lcContName ,;
            TranCd     WITH '1'        ,;
            dTranDate  WITH ldTranDate ,;
            DueDate    WITH &lcLotsDet..DueDate ,;
            nLotQty1   WITH &lcLotsDet..nToIssue1 ,;
            nLotQty2   WITH &lcLotsDet..nToIssue2 ,;
            nLotQty3   WITH &lcLotsDet..nToIssue3 ,;
            nLotQty4   WITH &lcLotsDet..nToIssue4 ,;
            nLotQty5   WITH &lcLotsDet..nToIssue5 ,;
            nLotQty6   WITH &lcLotsDet..nToIssue6 ,;
            nLotQty7   WITH &lcLotsDet..nToIssue7 ,;
            nLotQty8   WITH &lcLotsDet..nToIssue8 ,;
            nLotTotQty WITH &lcLotsDet..nTotToIss ,;
            cDyelot    WITH &lcLotsDet..cDyelot
  ENDIF
ENDSCAN
IF laScrMode[2] .AND. SEEK(lcTranType+laData[1]+lcOprCode,'MFGOPRHD')
  SELECT MFGOPRHD
  REPLACE nNxtLotNo WITH nNxtLotNo+1
ENDIF  
SELECT (lcOprHdr)
REPLACE nNxtLotNo WITH nNxtLotNo+1
llIssFirst = .T.

*!*************************************************************
*! Name      : lfAdjSButt
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Select, Unselect, Select all, Select None and Invert items
*!*************************************************************
*! Calls     : lfShNewLot
*!*************************************************************
*! Parameters: lcType  'S' : Select
*!                     'A' : Select All
*!                     'N' : Select None
*!                     'V' : Invert
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfAdjSButt()
*!*************************************************************
FUNCTION lfAdjSButt
PARAMETERS lcType
PRIVATE lnRecNo,lcDataSel,lcDataUSel

SELECT (lcLotsDet)
lnRecNo = RECNO()
DO CASE 
  CASE lcType = 'S'
    *E301235,4 WAB - in case of material manufactring order replace the "to issue" qty
    *E301235,4 WAB - START
    IF lcTranType='T'
      REPLACE cSelect   WITH IIF(cSelect=SPACE(1),"»",SPACE(1)) ,;
              nTotToIss WITH IIF(cSelect=SPACE(1),0,MAX(nTotBudget-nTotIss,0))
    ELSE
    *E301235,4 WAB - END
      REPLACE cSelect   WITH IIF(cSelect=SPACE(1),"»",SPACE(1)) ,;
              nToIssue1 WITH IIF(cSelect=SPACE(1),0,MAX(nBudget1-nIssued1,0)) ,;
              nToIssue2 WITH IIF(cSelect=SPACE(1),0,MAX(nBudget2-nIssued2,0)) ,;
              nToIssue3 WITH IIF(cSelect=SPACE(1),0,MAX(nBudget3-nIssued3,0)) ,;
              nToIssue4 WITH IIF(cSelect=SPACE(1),0,MAX(nBudget4-nIssued4,0)) ,;
              nToIssue5 WITH IIF(cSelect=SPACE(1),0,MAX(nBudget5-nIssued5,0)) ,;
              nToIssue6 WITH IIF(cSelect=SPACE(1),0,MAX(nBudget6-nIssued6,0)) ,;
              nToIssue7 WITH IIF(cSelect=SPACE(1),0,MAX(nBudget7-nIssued7,0)) ,;
              nToIssue8 WITH IIF(cSelect=SPACE(1),0,MAX(nBudget8-nIssued8,0)) ,;
              nTotToIss WITH IIF(cSelect=SPACE(1),0,nToIssue1+nToIssue2+;
              nToIssue3+nToIssue4+nToIssue5+nToIssue6+nToIssue7+nToIssue8)
     *E301235,4 WAB - END of the  IF STATMENT
     *E301235,4 WAB - START
     ENDIF
     *E301235,4 WAB - END
  CASE lcType = 'A'
    *E301235,4 WAB - in case of material manufactring order replace the "to issue" qty
    *E301235,4 WAB - START
    IF lcTranType='T'
      REPLACE ALL cSelect   WITH "»" ,;
                  nTotToIss WITH MAX(nTotBudget-nTotIss,0)
    ELSE
    *E301235,4 WAB - END
      REPLACE ALL cSelect   WITH "»" ,;
                  nToIssue1 WITH MAX(nBudget1-nIssued1,0) ,;
                  nToIssue2 WITH MAX(nBudget2-nIssued2,0) ,;
                  nToIssue3 WITH MAX(nBudget3-nIssued3,0) ,;
                  nToIssue4 WITH MAX(nBudget4-nIssued4,0) ,;
                  nToIssue5 WITH MAX(nBudget5-nIssued5,0) ,;
                  nToIssue6 WITH MAX(nBudget6-nIssued6,0) ,;
                  nToIssue7 WITH MAX(nBudget7-nIssued7,0) ,;
                  nToIssue8 WITH MAX(nBudget8-nIssued8,0) ,;
                  nTotToIss WITH nToIssue1+nToIssue2+nToIssue3+nToIssue4+;
                                 nToIssue5+nToIssue6+nToIssue7+nToIssue8
     *E301235,4 WAB - END of the  IF STATMENT
     *E301235,4 WAB - START
     ENDIF
     *E301235,4 WAB - END
  CASE lcType = 'N'
    REPLACE ALL cSelect   WITH SPACE(1) ,;
                nToIssue1 WITH 0 ,;
                nToIssue2 WITH 0 ,;
                nToIssue3 WITH 0 ,;
                nToIssue4 WITH 0 ,;
                nToIssue5 WITH 0 ,;
                nToIssue6 WITH 0 ,;
                nToIssue7 WITH 0 ,;
                nToIssue8 WITH 0 ,;
                nTotToIss WITH 0 

  CASE lcType = 'V'
    *E301235,4 WAB - in case of material manufactring order replace the "to issue" qty
    *E301235,4 WAB - START
    IF lcTranType='T'
      REPLACE ALL cSelect   WITH IIF(cSelect=SPACE(1),"»",SPACE(1)) ,;
                  nTotToIss WITH IIF(cSelect=SPACE(1),0,MAX(nTotBudget-nTotIss,0))
    ELSE
    *E301235,4 WAB - END
      REPLACE ALL cSelect   WITH IIF(cSelect=SPACE(1),"»",SPACE(1)) ,;
                  nToIssue1 WITH IIF(cSelect=SPACE(1),0,MAX(nBudget1-nIssued1,0)) ,;
                  nToIssue2 WITH IIF(cSelect=SPACE(1),0,MAX(nBudget2-nIssued2,0)) ,;
                  nToIssue3 WITH IIF(cSelect=SPACE(1),0,MAX(nBudget3-nIssued3,0)) ,;
                  nToIssue4 WITH IIF(cSelect=SPACE(1),0,MAX(nBudget4-nIssued4,0)) ,;
                  nToIssue5 WITH IIF(cSelect=SPACE(1),0,MAX(nBudget5-nIssued5,0)) ,;
                  nToIssue6 WITH IIF(cSelect=SPACE(1),0,MAX(nBudget6-nIssued6,0)) ,;
                  nToIssue7 WITH IIF(cSelect=SPACE(1),0,MAX(nBudget7-nIssued7,0)) ,;
                  nToIssue8 WITH IIF(cSelect=SPACE(1),0,MAX(nBudget8-nIssued8,0)) ,;
                  nTotToIss WITH IIF(cSelect=SPACE(1),0,nToIssue1+nToIssue2+;
                  nToIssue3+nToIssue4+nToIssue5+nToIssue6+nToIssue7+nToIssue8)
     *E301235,4 WAB - END of the  IF STATMENT
     *E301235,4 WAB - START
     ENDIF
     *E301235,4 WAB - END
  OTHERWISE
ENDCASE
LOCATE FOR EMPTY(cSelect)
lcDataUSel = IIF(FOUND(),'ENABLE','DISABLE')
LOCATE FOR !EMPTY(cSelect)
lcDataSel  = IIF(FOUND(),'ENABLE','DISABLE')
GO lnRecNo
SHOW GET pbSelAll  &lcDataUSel
SHOW GET pbSelNone &lcDataSel
SHOW GET pbGenLot  &lcDataSel
=lfShNewLot()

*!*************************************************************
*! Name      : lfvModLot
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Modify existing Lot
*!*************************************************************
*! Calls     : MFMODLT.SPX,lfRefTotal(),lfShOprDt()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvModLot()
*!*************************************************************
FUNCTION lfvModLot
PRIVATE lcOprCode,lcLotNo,llInHouse,lcContCode,ldTranDate,;
        ldDueDate,lcItemDesc,lnLotRcv1,lnLotCan1,lnLotDmg1,lnLotRcv2,;
        lnLotCan2,lnLotDmg2,lnLotRcv3,lnLotCan3,lnLotDmg3,lnLotRcv4,;
        lnLotCan4,lnLotDmg4,lnLotRcv5,lnLotCan5,lnLotDmg5,lnLotRcv6,;
        lnLotCan6,lnLotDmg6,lnLotRcv7,lnLotCan7,lnLotDmg7,lnLotRcv8,;
        lnLotCan8,lnLotDmg8,lnTotRcv,lnTotCan,lnTotDmg,lcCurrTag

STORE 0 TO lnLotRcv1,lnLotCan1,lnLotDmg1,lnLotRcv2,lnLotCan2,lnLotDmg2,;
           lnLotRcv3,lnLotCan3,lnLotDmg3,lnLotRcv4,lnLotCan4,lnLotDmg4,;
           lnLotRcv5,lnLotCan5,lnLotDmg5,lnLotRcv6,lnLotCan6,lnLotDmg6,;
           lnLotRcv7,lnLotCan7,lnLotDmg7,lnLotRcv8,lnLotCan8,lnLotDmg8,;
           lnTotRcv,lnTotCan,lnTotDmg

lcLotNo    = &lcOprDet..cLotNo
lcOprCode  = &lcOprDet..cOprCode
lcItemDesc = ''
ldTranDate = &lcOprDet..dTranDate
ldDueDate  = &lcOprDet..DueDate
lcContCode = &lcOprDet..cContCode
lcContName = &lcOprDet..cContName
llInHouse  = &lcOprDet..lInHouse
lcNameStat= IIF(!llInHouse .AND. 'AP' $ gcCmpModules,'DISABLE','ENABLE')

*E301077,55 Inhance openning files to speed up transaction
*SELECT (lcLotsDet)
*ZAP
*B603243,1 AMM  Add filed cDyelot c(10)

*E302004,1 AMH Add trancd field to allow the user to edit oper. received Qty. [Start]
*CREATE TABLE (gcWorkDir+lcLotsDet) ;
(cSelect C(1),Item C(19),Color C(6),cDyelot C(10),nBudget1 N(6),nBudget2 N(6),nBudget3 N(6),nBudget4 N(6),;
 nBudget5 N(6),nBudget6 N(6),nBudget7 N(6),nBudget8 N(6),nTotBudget N(7),;
 nIssued1 N(6),nIssued2 N(6),nIssued3 N(6),nIssued4 N(6),nIssued5 N(6),;
 nIssued6 N(6),nIssued7 N(6),nIssued8 N(6),nTotIss N(7),;
 nToIssue1 N(5),nToIssue2 N(5),nToIssue3 N(5),nToIssue4 N(5),nToIssue5 N(5),;
 nToIssue6 N(5),nToIssue7 N(5),nToIssue8 N(5),nTotToIss N(6),DueDate D,lCanRemain L)
 
*B126396,1 NNA 03/31/2005 (Begin) if we're in the Material Manufucturing Order then make fieldS
*B126396,1 NNA            have decimals.IF lcTranType='T'
IF lcTranType='T'
  CREATE TABLE (gcWorkDir+lcLotsDet) ;
  (cSelect C(1),Item C(19),Color C(6),cDyelot C(10),nBudget1 N(6,2),nBudget2 N(6,2),nBudget3 N(6,2),nBudget4 N(6,2),;
   nBudget5 N(6,2),nBudget6 N(6,2),nBudget7 N(6,2),nBudget8 N(6,2),nTotBudget N(7,2),;
   nIssued1 N(6,2),nIssued2 N(6,2),nIssued3 N(6,2),nIssued4 N(6,2),nIssued5 N(6,2),;
   nIssued6 N(6,2),nIssued7 N(6,2),nIssued8 N(6,2),nTotIss N(7,2),;
   nToIssue1 N(5,2),nToIssue2 N(5,2),nToIssue3 N(5,2),nToIssue4 N(5,2),nToIssue5 N(5,2),;
   nToIssue6 N(5,2),nToIssue7 N(5,2),nToIssue8 N(5,2),nTotToIss N(6,2),DueDate D,lCanRemain L,TranCd C(1))
ELSE
*B126396,1 NNA (End)
 
  CREATE TABLE (gcWorkDir+lcLotsDet) ;
  (cSelect C(1),Item C(19),Color C(6),cDyelot C(10),nBudget1 N(6),nBudget2 N(6),nBudget3 N(6),nBudget4 N(6),;
   nBudget5 N(6),nBudget6 N(6),nBudget7 N(6),nBudget8 N(6),nTotBudget N(7),;
   nIssued1 N(6),nIssued2 N(6),nIssued3 N(6),nIssued4 N(6),nIssued5 N(6),;
   nIssued6 N(6),nIssued7 N(6),nIssued8 N(6),nTotIss N(7),;
   nToIssue1 N(5),nToIssue2 N(5),nToIssue3 N(5),nToIssue4 N(5),nToIssue5 N(5),;
   nToIssue6 N(5),nToIssue7 N(5),nToIssue8 N(5),nTotToIss N(6),DueDate D,lCanRemain L,TranCd C(1))

*B126396,1 NNA
ENDIF
*B126396,1 NNA
   
*INDEX ON Item+Color+cDyelot TAG (lcLotsDet)
INDEX ON Item+Color+cDyelot+TranCd TAG (lcLotsDet)
*E302004,1 AMH [End]

*B603243,1 AMM  end
*E301077,55 (End)

SELECT (lcOprDet)
lcCurrTag = ORDER(lcOprDet)
lnOprRecNo= RECNO(lcOprDet)
SET ORDER TO TAG Lot
=SEEK(lcTranType+laData[1]+lcOprCode+lcLotNo)
SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cLotNo = ;
                lcTranType+laData[1]+lcOprCode+lcLotNo
  
  *E302004,1 AMH Add trancd field to the replace command. [Start]
  *IF TranCd = '1'
  *  SELECT (lcLotsDet)
  *  APPEND BLANK
  *  *B603243,1 AMM  Add update of dyelot field
  *  REPLACE Item      WITH &lcOprDet..Item     ,;
  *          Color     WITH &lcOprDet..Color    ,;
  *          nToIssue1 WITH &lcOprDet..nLotQty1 ,;
  *          nToIssue2 WITH &lcOprDet..nLotQty2 ,;
  *          nToIssue3 WITH &lcOprDet..nLotQty3 ,;
  *          nToIssue4 WITH &lcOprDet..nLotQty4 ,;
  *          nToIssue5 WITH &lcOprDet..nLotQty5 ,;
  *          nToIssue6 WITH &lcOprDet..nLotQty6 ,;
  *          nToIssue7 WITH &lcOprDet..nLotQty7 ,;
  *          nToIssue8 WITH &lcOprDet..nLotQty8 ,;
  *          nTotToIss WITH &lcOprDet..nLotTotQty,;
  *          cDyelot   WITH &lcOprDet..cDyelot
  *ENDIF
  SELECT (lcLotsDet)
  APPEND BLANK
  REPLACE Item      WITH &lcOprDet..Item     ,;
          Color     WITH &lcOprDet..Color    ,;
          nToIssue1 WITH &lcOprDet..nLotQty1 ,;
          nToIssue2 WITH &lcOprDet..nLotQty2 ,;
          nToIssue3 WITH &lcOprDet..nLotQty3 ,;
          nToIssue4 WITH &lcOprDet..nLotQty4 ,;
          nToIssue5 WITH &lcOprDet..nLotQty5 ,;
          nToIssue6 WITH &lcOprDet..nLotQty6 ,;
          nToIssue7 WITH &lcOprDet..nLotQty7 ,;
          nToIssue8 WITH &lcOprDet..nLotQty8 ,;
          nTotToIss WITH &lcOprDet..nLotTotQty,;
          cDyelot   WITH &lcOprDet..cDyelot,;
          TranCd    WITH &lcOprDet..TranCd
  *E302004,1 AMH [End]
  
ENDSCAN
SELECT (lcLotsDet)
IF lcTranType <> 'T'
  SET RELATION TO ITEM INTO STYLE
ENDIF  
GO TOP
DO (gcScrDir+"MFMODLT.SPX") WITH lcOprCode,lcLotNo
IF lcTranType <> 'T'
  SELECT (lcLotsDet)
  SET RELATION OFF INTO STYLE
ENDIF  
IF !SEEK(lcTranType+laData[1],lcOprDet)
  SHOW GETS WINDOW (lcWinCh24) DISABLE ONLY
ENDIF
=lfRefTotal()
=lfShOprDt()
SELECT (lcOprDet)
SET ORDER TO TAG lcCurrTag
GO lnOprRecNo

*!*************************************************************
*! Name      : lfvOkModLt
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Accept lot modifications
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfvOkModLt()
*!*************************************************************
FUNCTION lfvOkModLt
PRIVATE lcCurrTag

SET ORDER TO TAG MFGOPRDT IN MFGOPRDT
lcCurrTag = ORDER(lcOprDet)
SET ORDER TO TAG 'LOT' IN (lcOprDet)
SELECT (lcLotsDet)
SCAN
  SELECT (lcOprDet)
  *B603243,1 AMM  Add dyelot to seek expression
  *IF SEEK(lcTranType+laData[1]+lcOprCode+lcLotNo+&lcLotsDet..Item+&lcLotsDet..Color+'1')
  
  *E302004,1 AMH Add the trancd to the seek command to update Opr. Received Qty. [Start]
  *IF SEEK(lcTranType+laData[1]+lcOprCode+lcLotNo+&lcLotsDet..Item+&lcLotsDet..Color+'1'+&lcLotsDet..cDyelot)
  IF SEEK(lcTranType+laData[1]+lcOprCode+lcLotNo+&lcLotsDet..Item+&lcLotsDet..Color+&lcLotsDet..TranCd+&lcLotsDet..cDyelot)
  *E302004,1 AMH [End]
  
  *B603243,1 AMM  end
    *B603243,1 AMM  Add the update of dyelot field
*--- DOC
*--- Update the operatoin detail with the new issued qty
*--- DOC
    REPLACE nLotQty1   WITH &lcLotsDet..nToIssue1 ,;
            nLotQty2   WITH &lcLotsDet..nToIssue2 ,;
            nLotQty3   WITH &lcLotsDet..nToIssue3 ,;
            nLotQty4   WITH &lcLotsDet..nToIssue4 ,;
            nLotQty5   WITH &lcLotsDet..nToIssue5 ,;
            nLotQty6   WITH &lcLotsDet..nToIssue6 ,;
            nLotQty7   WITH &lcLotsDet..nToIssue7 ,;
            nLotQty8   WITH &lcLotsDet..nToIssue8 ,;
            nLotTotQty WITH &lcLotsDet..nTotToIss ,;
            dTranDate  WITH ldTranDate ,;
            DueDate    WITH ldDueDate  ,;
            cDyelot    WITH &lcLotsDet..cDyelot
    
    *E302004,1 AMH Add the trancd to the seek command to update Opr. Received Qty. [Start]
    *IF laScrMode[2] .AND. SEEK(lcTranType+laData[1]+lcOprCode+lcLotNo+'1','MFGOPRDT')
    IF laScrMode[2] .AND. SEEK(lcTranType+laData[1]+lcOprCode+lcLotNo+TranCd,'MFGOPRDT')
    *E302004,1 AMH [End]
    
      SELECT MFGOPRDT
      *B603243,1 AMM  Add dyelot to locate expression.
      *LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd =     ;
                        lcTranType+laData[1]+lcOprCode+lcLotNo+'1' ;
                  FOR   Item+Color = &lcLotsDet..Item+&lcLotsDet..Color
      
      *E302004,1 AMH Add the trancd to the seek command to update Opr. Received Qty. [Start]
      *LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd =     ;
                        lcTranType+laData[1]+lcOprCode+lcLotNo+'1' ;
                  FOR   Item+Color+cDyelot = &lcLotsDet..Item+&lcLotsDet..Color+&lcLotsDet..cDyelot
      LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd =     ;
                        lcTranType+laData[1]+lcOprCode+lcLotNo+&lcLotsDet..Trancd ;
                  FOR   Item+Color+cDyelot = &lcLotsDet..Item+&lcLotsDet..Color+&lcLotsDet..cDyelot
      *E302004,1 AMH [End]
      
      *B603243,1 AMM  end
      IF FOUND()
        REPLACE nLotQty1   WITH &lcLotsDet..nToIssue1 ,;
                nLotQty2   WITH &lcLotsDet..nToIssue2 ,;
                nLotQty3   WITH &lcLotsDet..nToIssue3 ,;
                nLotQty4   WITH &lcLotsDet..nToIssue4 ,;
                nLotQty5   WITH &lcLotsDet..nToIssue5 ,;
                nLotQty6   WITH &lcLotsDet..nToIssue6 ,;
                nLotQty7   WITH &lcLotsDet..nToIssue7 ,;
                nLotQty8   WITH &lcLotsDet..nToIssue8 ,;
                nLotTotQty WITH &lcLotsDet..nTotToIss ,;
                dTranDate  WITH ldTranDate ,;
                DueDate    WITH ldDueDate  ,;
                lInHouse   WITH llInHouse  ,;
                cContCode  WITH lcContCode ,;
                cContName  WITH lcContName
      ENDIF
    ENDIF
  ELSE
    APPEND BLANK
    *B603243,1 AMM  Add the update of dyelot field
    
    *E302004,1 AMH Replace the Trancd from the temporary file instead of hard coded by "1"
    *E302004,1 AMH because we allow the user to edit the received qty. [Start]
    *REPLACE cImTyp     WITH lcTranType ,;
            cTktNo     WITH laData[1]  ,;
            cOprCode   WITH lcOprCode  ,;
            cLotNo     WITH lcLotNo    ,;
            Item       WITH &lcLotsDet..Item ,;
            Color      WITH &lcLotsDet..Color ,;
            TranCd     WITH '1'        ,;
            dTranDate  WITH ldTranDate ,;
            DueDate    WITH &lcLotsDet..DueDate ,;
            nLotQty1   WITH &lcLotsDet..nToIssue1 ,;
            nLotQty2   WITH &lcLotsDet..nToIssue2 ,;
            nLotQty3   WITH &lcLotsDet..nToIssue3 ,;
            nLotQty4   WITH &lcLotsDet..nToIssue4 ,;
            nLotQty5   WITH &lcLotsDet..nToIssue5 ,;
            nLotQty6   WITH &lcLotsDet..nToIssue6 ,;
            nLotQty7   WITH &lcLotsDet..nToIssue7 ,;
            nLotQty8   WITH &lcLotsDet..nToIssue8 ,;
            nLotTotQty WITH &lcLotsDet..nTotToIss ,;
            cDyelot    WITH &lcLotsDet..cDyelot
    REPLACE cImTyp     WITH lcTranType ,;
            cTktNo     WITH laData[1]  ,;
            cOprCode   WITH lcOprCode  ,;
            cLotNo     WITH lcLotNo    ,;
            Item       WITH &lcLotsDet..Item ,;
            Color      WITH &lcLotsDet..Color ,;
            TranCd     WITH &lcLotsDet..TranCd,;
            dTranDate  WITH ldTranDate ,;
            DueDate    WITH &lcLotsDet..DueDate ,;
            nLotQty1   WITH &lcLotsDet..nToIssue1 ,;
            nLotQty2   WITH &lcLotsDet..nToIssue2 ,;
            nLotQty3   WITH &lcLotsDet..nToIssue3 ,;
            nLotQty4   WITH &lcLotsDet..nToIssue4 ,;
            nLotQty5   WITH &lcLotsDet..nToIssue5 ,;
            nLotQty6   WITH &lcLotsDet..nToIssue6 ,;
            nLotQty7   WITH &lcLotsDet..nToIssue7 ,;
            nLotQty8   WITH &lcLotsDet..nToIssue8 ,;
            nLotTotQty WITH &lcLotsDet..nTotToIss ,;
            cDyelot    WITH &lcLotsDet..cDyelot
    *E302004,1 AMH [End]
    
    IF laScrMode[2]
      SELECT MFGOPRDT
      
      *E302004,1 AMH Add the trancd to the seek command to update Opr. Received Qty. [Start]
      *=SEEK(lcTranType+laData[1]+lcOprCode+lcLotNo+'1')
      =SEEK(lcTranType+laData[1]+lcOprCode+lcLotNo+&lcLotsDet..TranCd)
      *E302004,1 AMH [End]
      
      *B603243,1 AMM  Add dylot field to locate expression
      *LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd =     ;
                        lcTranType+laData[1]+lcOprCode+lcLotNo+'1' ;
                  FOR   Item+Color = &lcLotsDet..Item+&lcLotsDet..Color
      
      *E302004,1 AMH Add the trancd from the temporary file instead of hard coded by "1"
      *E302004,1     because we allowed the user to edit the received qty.[Start]
      *LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd =     ;
                        lcTranType+laData[1]+lcOprCode+lcLotNo+'1' ;
                  FOR   Item+Color+cDyelot = &lcLotsDet..Item+&lcLotsDet..Color+&lcLotsDet..cDyelot
      LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd =     ;
                        lcTranType+laData[1]+lcOprCode+lcLotNo+&lcLotsDet..TranCd ;
                  FOR   Item+Color+cDyelot = &lcLotsDet..Item+&lcLotsDet..Color+&lcLotsDet..cDyelot
      *E302004,1 AMH [End]
      
      *B603243,1 AMM  end
      IF !FOUND()
        APPEND BLANK
        *B603243,1 AMM  Add the update of dyelot field
        
        *E302004,1 AMH Update the trancd from the temporary file instead of "1" because
        *E302004,1 AMH we allowed the user to edit the received qty. [Start]
        *REPLACE cImTyp     WITH lcTranType ,;
                cTktNo     WITH laData[1]  ,;
                cOprCode   WITH lcOprCode  ,;
                cLotNo     WITH lcLotNo    ,;
                Item       WITH &lcLotsDet..Item ,;
                Color      WITH &lcLotsDet..Color ,;
                TranCd     WITH '1'        ,;
                dTranDate  WITH ldTranDate ,;
                DueDate    WITH &lcLotsDet..DueDate ,;
                nLotQty1   WITH &lcLotsDet..nToIssue1 ,;
                nLotQty2   WITH &lcLotsDet..nToIssue2 ,;
                nLotQty3   WITH &lcLotsDet..nToIssue3 ,;
                nLotQty4   WITH &lcLotsDet..nToIssue4 ,;
                nLotQty5   WITH &lcLotsDet..nToIssue5 ,;
                nLotQty6   WITH &lcLotsDet..nToIssue6 ,;
                nLotQty7   WITH &lcLotsDet..nToIssue7 ,;
                nLotQty8   WITH &lcLotsDet..nToIssue8 ,;
                nLotTotQty WITH &lcLotsDet..nTotToIss ,;
                cDyelot    WITH &lcLotsDet..cDyelot
        REPLACE cImTyp     WITH lcTranType ,;
                cTktNo     WITH laData[1]  ,;
                cOprCode   WITH lcOprCode  ,;
                cLotNo     WITH lcLotNo    ,;
                Item       WITH &lcLotsDet..Item ,;
                Color      WITH &lcLotsDet..Color ,;
                TranCd     WITH &lcLotsDet..TranCd,;
                dTranDate  WITH ldTranDate ,;
                DueDate    WITH &lcLotsDet..DueDate ,;
                nLotQty1   WITH &lcLotsDet..nToIssue1 ,;
                nLotQty2   WITH &lcLotsDet..nToIssue2 ,;
                nLotQty3   WITH &lcLotsDet..nToIssue3 ,;
                nLotQty4   WITH &lcLotsDet..nToIssue4 ,;
                nLotQty5   WITH &lcLotsDet..nToIssue5 ,;
                nLotQty6   WITH &lcLotsDet..nToIssue6 ,;
                nLotQty7   WITH &lcLotsDet..nToIssue7 ,;
                nLotQty8   WITH &lcLotsDet..nToIssue8 ,;
                nLotTotQty WITH &lcLotsDet..nTotToIss ,;
                cDyelot    WITH &lcLotsDet..cDyelot
        *E302004,1 AMH [End]
        
      ENDIF
    ENDIF
  ENDIF
ENDSCAN
SELECT (lcOprDet)
=SEEK(lcTranType+laData[1]+lcOprCode+lcLotNo)
SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd=;
                lcTranType+laData[1]+lcOprCode+lcLotNo
  REPLACE lInHouse   WITH llInHouse  ,;
          cContCode  WITH lcContCode ,; 
          cContName  WITH lcContName 
  IF TranCd='1'
    SELECT (lcLotsDet)  
    LOCATE FOR Item+Color = &lcOprDet..Item+&lcOprDet..Color
    IF !FOUND()
      SELECT MFGOPRDT
      =SEEK(lcTranType+laData[1]+lcOprCode+lcLotNo+'1')

      *B603243,1 AMM  Add dyelot to the locate expression
      *LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd =     ;
                        lcTranType+laData[1]+lcOprCode+lcLotNo+'1' ;
                  FOR   Item+Color = &lcOprDet..Item+&lcOprDet..Color
      LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd =     ;
                        lcTranType+laData[1]+lcOprCode+lcLotNo+'1' ;
                  FOR   Item+Color+cDyelot = &lcOprDet..Item+&lcOprDet..Color+&lcOprDet..cDyelot
      *B603243,1 AMM  end
      IF FOUND()
        DELETE
      ENDIF   
      SELECT (lcOprDet)
      DELETE
    ENDIF
  ENDIF
ENDSCAN
IF laScrMode[2] .AND. SEEK(lcTranType+laData[1]+lcOprCode+lcLotNo,'MFGOPRDT')
  SELECT MFGOPRDT
  REPLACE REST lInHouse   WITH llInHouse  ,;
               cContCode  WITH lcContCode ,;
               cContName  WITH lcContName ;
         WHILE cimtyp+ctktno+coprcode+clotno+trancd= lcTranType+laData[1]+lcOprCode+lcLotNo
ENDIF
SELECT (lcOprDet)
SET ORDER TO TAG lcCurrTag IN (lcOprDet)

*!*************************************************************
*! Name      : lfvOprLot
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Browse new/Modified Lot
*!*************************************************************
*! Calls     : lfShNewLot(),lfShModLot()
*!*************************************************************
*! Parameters: lcAction      'N' : New Lot
*!                           'M' : Modify Lot
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvOprLot()
*!*************************************************************
FUNCTION lfvOprLot
PARAMETERS lcAction
PRIVATE lcFields
SELECT (lcLotsDet)
lnDMarker = RECNO()
IF lcAction = 'N'
  *B603243,1 AMM  Add dyelot to the browse
  *lcFields = "cMarker =IIF(RECNO()=lnDMarker ,'>',' '):H=' ':R:1:W=.F.,"+;
             "cSelect :H= ' ' :R,Item :H= 'Item' :R,"+;
             IIF(lcTranType='T',"Color :H= 'Color' :R,",'')+;
             "nTotToIss :H= 'To Issue' :R"+;
             IIF(lcTranType='T','',",nToIssue1 :H= 'To Issue1' :R,nToIssue2 :H= 'To Issue2' :R,"+;
             "nToIssue3 :H= 'To Issue3' :R,nToIssue4 :H= 'To Issue4' :R,"+;
             "nToIssue5 :H= 'To Issue5' :R,nToIssue6 :H= 'To Issue6' :R,"+;
             "nToIssue7 :H= 'To Issue7' :R,nToIssue8 :H= 'To Issue8' :R")
  lcFields = "cMarker =IIF(RECNO()=lnDMarker ,'>',' '):H=' ':R:1:W=.F.,"+;
             "cSelect :H= ' ' :R,Item :H= 'Item' :R,"+;
             IIF(lcTranType='T',"Color :H= 'Color' :R,",'')+;
             IIF(llUseDyelot,"cDyelot :H= 'Dyelot' :R,",'')+;
             "nTotToIss :H= 'To Issue' :R"+;
             IIF(lcTranType='T','',",nToIssue1 :H= 'To Issue1' :R,nToIssue2 :H= 'To Issue2' :R,"+;
             "nToIssue3 :H= 'To Issue3' :R,nToIssue4 :H= 'To Issue4' :R,"+;
             "nToIssue5 :H= 'To Issue5' :R,nToIssue6 :H= 'To Issue6' :R,"+;
             "nToIssue7 :H= 'To Issue7' :R,nToIssue8 :H= 'To Issue8' :R")
  *B603243,1 AMM  end
  BROWSE FIELDS &lcFields  ;
         WINDOW MFLOTS1    ;
         IN WINDOW MFLOTS  ;
         NOMENU            ;
         NOAPPEND          ;
         NODELETE          ;
         NOWAIT            ;
         SAVE              ;
         NOCLEAR           ;
         WHEN lfShNewLot() ;
         TITLE lcNewTit
  =lfShNewLot()
ELSE
  *B603243,1 AMM  Add dyelot to the browse
  *lcFields = "cMarker =IIF(RECNO()=lnDMarker ,'>',' '):H=' ':R:1:W=.F.,"+;
             "Item :H= 'Item' :R,"+IIF(lcTranType='T',"Color :H= 'Color' :R,",'')+;
             "nTotToIss :H= 'Budget' :R"+;
             IIF(lcTranType='T','',",nToIssue1 :H= 'Budget1' :R,nToIssue2 :H= 'Budget2' :R,"+;
             "nToIssue3 :H= 'Budget3' :R,nToIssue4 :H= 'Budget4' :R,"+;
             "nToIssue5 :H= 'Budget5' :R,nToIssue6 :H= 'Budget6' :R,"+;
             "nToIssue7 :H= 'Budget7' :R,nToIssue8 :H= 'Budget8' :R")
  lcFields = "cMarker =IIF(RECNO()=lnDMarker ,'>',' '):H=' ':R:1:W=.F.,"+;
             "Item :H= 'Item' :R,"+IIF(lcTranType='T',"Color :H= 'Color' :R,",'')+;
             IIF(llUseDyelot,"cDyelot :H= 'Dyelot' :R,",'')+;             
             "nTotToIss :H= 'Budget' :R"+;
             IIF(lcTranType='T','',",nToIssue1 :H= 'Budget1' :R,nToIssue2 :H= 'Budget2' :R,"+;
             "nToIssue3 :H= 'Budget3' :R,nToIssue4 :H= 'Budget4' :R,"+;
             "nToIssue5 :H= 'Budget5' :R,nToIssue6 :H= 'Budget6' :R,"+;
             "nToIssue7 :H= 'Budget7' :R,nToIssue8 :H= 'Budget8' :R")
  *B603243,1 AMM  end
  
  *E302004,1 AMH Brow Issued lines only [Start]
  *BROWSE FIELDS &lcFields  ;
         WINDOW MFMODLT1   ;
         IN WINDOW MFMODLT ;
         NOMENU            ;
         NOAPPEND          ;
         NODELETE          ;
         NOWAIT            ;
         SAVE              ;
         NOCLEAR           ;
         WHEN lfShModLot() ;
         TITLE lcModTit
  BROWSE FIELDS &lcFields  ;
         WINDOW MFMODLT1   ;
         IN WINDOW MFMODLT ;
         FOR TRANCD = '1'  ;
         NOMENU            ;
         NOAPPEND          ;
         NODELETE          ;
         NOWAIT            ;
         SAVE              ;
         NOCLEAR           ;
         WHEN lfShModLot() ;
         TITLE lcModTit
  *E302004,1 AMH [End]
  
  =lfShModLot()
ENDIF

*!*************************************************************
*! Name      : lfShNewLot
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Show new lot details
*!*************************************************************
*! Calls     : lfRefresh()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfShNewLot()
*!*************************************************************
FUNCTION lfShNewLot

=IIF(lcTranType='T',SEEK(SUBSTR(Item,1,7)+Color,'Fabric'),;
                    SEEK(Item,'Style') .AND. SEEK('S'+Style.Scale,'Scale'))
lcItemDesc = IIF(lcTranType='T',Fabric.Desc,Style.Desc1)
lnDMarker = RECNO(lcLotsDet)
SHOW WINDOW (lcNewTit) REFRESH SAME
IF cSelect = SPACE(1)
  SHOW GET pbSelect,1 PROMPT '\<Select'
  SHOW GET DueDate   DISABLE
  IF lcTranType = 'T'
    SHOW GET nTotToIss DISABLE
  ELSE
    SHOW GET nToIssue1 DISABLE
    SHOW GET nToIssue2 DISABLE
    SHOW GET nToIssue3 DISABLE
    SHOW GET nToIssue4 DISABLE
    SHOW GET nToIssue5 DISABLE
    SHOW GET nToIssue6 DISABLE
    SHOW GET nToIssue7 DISABLE
    SHOW GET nToIssue8 DISABLE
  ENDIF
ELSE
  SHOW GET pbSelect,1 PROMPT 'Un\<Select'
  SHOW GET DueDate   ENABLE
  IF lcTranType = 'T'
    SHOW GET nTotToIss ENABLE
    _CUROBJ = OBJNUM(nTotToIss)
  ELSE
    SHOW GET nToIssue1 ENABLE
    SHOW GET nToIssue2 ENABLE
    SHOW GET nToIssue3 ENABLE
    SHOW GET nToIssue4 ENABLE
    SHOW GET nToIssue5 ENABLE
    SHOW GET nToIssue6 ENABLE
    SHOW GET nToIssue7 ENABLE
    SHOW GET nToIssue8 ENABLE
    _CUROBJ = OBJNUM(nToIssue1)
  ENDIF  
ENDIF
IF lcTranType = 'T'
  SHOW GETS WINDOW 'MFLOTS3' ONLY
  =lfRefresh('MFLOTS3')
ELSE
  SHOW GETS WINDOW 'MFLOTS2' ONLY
  =lfRefresh('MFLOTS2')
ENDIF
IF !EMPTY(cSelect) AND ;
   ((lcTranType='T' AND MAX(nTotBudget-nTotIss-nTotToIss,0) > 0) OR ;
   (lcTranType<>'T' AND ;
    MAX(nBudget1-nIssued1-nToIssue1,0)+MAX(nBudget2-nIssued2-nToIssue2,0)+;
    MAX(nBudget3-nIssued3-nToIssue3,0)+MAX(nBudget4-nIssued4-nToIssue4,0)+;
    MAX(nBudget5-nIssued5-nToIssue5,0)+MAX(nBudget6-nIssued6-nToIssue6,0)+;
    MAX(nBudget7-nIssued7-nToIssue7,0)+MAX(nBudget8-nIssued8-nToIssue8,0) > 0))
  SHOW GET lCanRemain ENABLE
ELSE
  REPLACE lCanRemain WITH .F.
  SHOW GET lCanRemain DISABLE
ENDIF


*!*************************************************************
*! Name      : lfShModLot
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Show modified lot details
*!*************************************************************
*! Calls     : lfRefresh()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfShModLot()
*!*************************************************************
FUNCTION lfShModLot
PRIVATE lcCurrTag

=IIF(lcTranType='T',SEEK(SUBSTR(Item,1,7)+Color,'Fabric'),;
                    SEEK(Item,'Style') .AND. SEEK('S'+Style.Scale,'Scale'))
lcItemDesc = IIF(lcTranType='T',Fabric.Desc,Style.Desc1)
lnDMarker = RECNO(lcLotsDet)

*E302004,1 AMH Get Rev/Can/Dam from lcLotsDet file [Start]
*SELECT (lcOprDet)
*lcCurrTag = ORDER(lcOprDet)
*SET ORDER TO TAG 'LOT'
*B603243,1 AMM  Add dyelot to next seek and sum expressions
*=SEEK(lcTranType+laData[1]+lcOprCode+lcLotNo+&lcLotsDet..Item+&lcLotsDet..Color+'2')
*SUM REST nLotQty1,nLotQty2,nLotQty3,nLotQty4,nLotQty5,nLotQty6,nLotQty7,nLotQty8,nLotTotQty TO ;
         lnLotRcv1,lnLotRcv2,lnLotRcv3,lnLotRcv4,lnLotRcv5,lnLotRcv6,lnLotRcv7,lnLotRcv8,lnTotRcv;
WHILE    cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
         lcTranType+laData[1]+lcOprCode+lcLotNo+&lcLotsDet..Item+&lcLotsDet..Color+'2'

*=SEEK(lcTranType+laData[1]+lcOprCode+lcLotNo+&lcLotsDet..Item+&lcLotsDet..Color+'3')
*SUM REST nLotQty1,nLotQty2,nLotQty3,nLotQty4,nLotQty5,nLotQty6,nLotQty7,nLotQty8,nLotTotQty TO ;
         lnLotCan1,lnLotCan2,lnLotCan3,lnLotCan4,lnLotCan5,lnLotCan6,lnLotCan7,lnLotCan8,lnTotCan;
WHILE    cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
         lcTranType+laData[1]+lcOprCode+lcLotNo+&lcLotsDet..Item+&lcLotsDet..Color+'3'

*=SEEK(lcTranType+laData[1]+lcOprCode+lcLotNo+&lcLotsDet..Item+&lcLotsDet..Color+'4')
*SUM REST nLotQty1,nLotQty2,nLotQty3,nLotQty4,nLotQty5,nLotQty6,nLotQty7,nLotQty8,nLotTotQty TO ;
         lnLotDmg1,lnLotDmg2,lnLotDmg3,lnLotDmg4,lnLotDmg5,lnLotDmg6,lnLotDmg7,lnLotDmg8,lnTotDmg;
WHILE    cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
         lcTranType+laData[1]+lcOprCode+lcLotNo+&lcLotsDet..Item+&lcLotsDet..Color+'4'
SELECT (lcLotsDet)
lcItem = Item
lcColor = Color
lcDyelot = cDyelot
*=SEEK(lcTranType+laData[1]+lcOprCode+lcLotNo+&lcLotsDet..Item+&lcLotsDet..Color+'2'+&lcLotsDet..cDyelot)
=SEEK(lcItem+lcColor+lcDyelot+'2')
*SUM REST nLotQty1,nLotQty2,nLotQty3,nLotQty4,nLotQty5,nLotQty6,nLotQty7,nLotQty8,nLotTotQty TO ;
         lnLotRcv1,lnLotRcv2,lnLotRcv3,lnLotRcv4,lnLotRcv5,lnLotRcv6,lnLotRcv7,lnLotRcv8,lnTotRcv;
WHILE    cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd+cDyelot = ;
         lcTranType+laData[1]+lcOprCode+lcLotNo+&lcLotsDet..Item+&lcLotsDet..Color+'2'+&lcLotsDet..cDyelot
lnLotRcv1 = nToIssue1
lnLotRcv2 = nToIssue2
lnLotRcv3 = nToIssue3
lnLotRcv4 = nToIssue4
lnLotRcv5 = nToIssue5
lnLotRcv6 = nToIssue6
lnLotRcv7 = nToIssue7
lnLotRcv8 = nToIssue8
lnTotRcv  = nTotToIss

*=SEEK(lcTranType+laData[1]+lcOprCode+lcLotNo+&lcLotsDet..Item+&lcLotsDet..Color+'3'+&lcLotsDet..cDyelot)
*--B603867,1 RAMY Change the TranCd field value in the WHILE clause [START]
*SUM REST nLotQty1,nLotQty2,nLotQty3,nLotQty4,nLotQty5,nLotQty6,nLotQty7,nLotQty8,nLotTotQty TO ;
*         lnLotCan1,lnLotCan2,lnLotCan3,lnLotCan4,lnLotCan5,lnLotCan6,lnLotCan7,lnLotCan8,lnTotCan;
*WHILE    cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd+cDyelot = ;
*         lcTranType+laData[1]+lcOprCode+lcLotNo+&lcLotsDet..Item+&lcLotsDet..Color+'3'+&lcLotsDet..cDyelot
*=SEEK(lcTranType+laData[1]+lcOprCode+lcLotNo+&lcLotsDet..Item+&lcLotsDet..Color+'3'+&lcLotsDet..cDyelot)

*=SEEK(lcTranType+laData[1]+lcOprCode+lcLotNo+&lcLotsDet..Item+&lcLotsDet..Color+'4'+&lcLotsDet..cDyelot)
*SUM REST nLotQty1,nLotQty2,nLotQty3,nLotQty4,nLotQty5,nLotQty6,nLotQty7,nLotQty8,nLotTotQty TO ;
         lnLotCan1,lnLotCan2,lnLotCan3,lnLotCan4,lnLotCan5,lnLotCan6,lnLotCan7,lnLotCan8,lnTotCan;
WHILE    cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd+cDyelot = ;
         lcTranType+laData[1]+lcOprCode+lcLotNo+&lcLotsDet..Item+&lcLotsDet..Color+'4'+&lcLotsDet..cDyelot
=SEEK(lcItem+lcColor+lcDyelot+'4')
lnLotCan1 = nToIssue1
lnLotCan2 = nToIssue2
lnLotCan3 = nToIssue3
lnLotCan4 = nToIssue4
lnLotCan5 = nToIssue5
lnLotCan6 = nToIssue6
lnLotCan7 = nToIssue7
lnLotCan8 = nToIssue8
lnTotCan  = nTotToIss
*--B603867,1 RAMY [end]




*--B603867,1 RAMY Change the TranCd field value in the WHILE clause [START]
*=SEEK(lcTranType+laData[1]+lcOprCode+lcLotNo+&lcLotsDet..Item+&lcLotsDet..Color+'4'+&lcLotsDet..cDyelot)
*SUM REST nLotQty1,nLotQty2,nLotQty3,nLotQty4,nLotQty5,nLotQty6,nLotQty7,nLotQty8,nLotTotQty TO ;
*         lnLotDmg1,lnLotDmg2,lnLotDmg3,lnLotDmg4,lnLotDmg5,lnLotDmg6,lnLotDmg7,lnLotDmg8,lnTotDmg;
*WHILE    cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd+cDyelot = ;
*         lcTranType+laData[1]+lcOprCode+lcLotNo+&lcLotsDet..Item+&lcLotsDet..Color+'4'+&lcLotsDet..cDyelot

*=SEEK(lcTranType+laData[1]+lcOprCode+lcLotNo+&lcLotsDet..Item+&lcLotsDet..Color+'3'+&lcLotsDet..cDyelot)
*SUM REST nLotQty1,nLotQty2,nLotQty3,nLotQty4,nLotQty5,nLotQty6,nLotQty7,nLotQty8,nLotTotQty TO ;
         lnLotDmg1,lnLotDmg2,lnLotDmg3,lnLotDmg4,lnLotDmg5,lnLotDmg6,lnLotDmg7,lnLotDmg8,lnTotDmg;
WHILE    cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd+cDyelot = ;
         lcTranType+laData[1]+lcOprCode+lcLotNo+&lcLotsDet..Item+&lcLotsDet..Color+'3'+&lcLotsDet..cDyelot
=SEEK(lcItem+lcColor+lcDyelot+'3')
lnLotDmg1 = nToIssue1
lnLotDmg2 = nToIssue2
lnLotDmg3 = nToIssue3
lnLotDmg4 = nToIssue4
lnLotDmg5 = nToIssue5
lnLotDmg6 = nToIssue6
lnLotDmg7 = nToIssue7
lnLotDmg8 = nToIssue8
lnTotDmg  = nTotToIss
*--B603867,1 RAMY [start]

*B603243,1 AMM  end
=SEEK(lcItem+lcColor+lcDyelot+'1')
*E302004,1 AMH [End]

SELECT(lcLotsDet)
SHOW WINDOW (lcModTit) REFRESH SAME
IF lcTranType = 'T'
  SHOW GETS WINDOW 'MFMODLT3' ONLY
  =lfRefresh('MFMODLT3')
ELSE
  SHOW GETS WINDOW 'MFMODLT2' ONLY
  =lfRefresh('MFMODLT2')
ENDIF

*E302004,1 AMH Not need this line [Start]
*SET ORDER TO TAG lcCurrTag IN (lcOprDet)
*E302004,1 AMH [End]

*!*************************************************************
*! Name      : lfvAddItm
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Add Style/ Fabric-Color from a lot
*!*************************************************************
*! Calls     : ARIABROW,lfShModLot
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvAddItm()
*!*************************************************************
FUNCTION lfvAddItm
PRIVATE lcBrFields,lcFile_Ttl

lcCurrTag = ORDER(lcOprDet)
SET ORDER TO TAG ITEM IN (lcOprDet)
*B603243,1 AMM  Add dylot to the select statement
*SELECT Item,Color,SUM(nQty1) AS nQty1,SUM(nQty2) AS nQty2,SUM(nQty3) AS nQty3,;
SUM(nQty4) AS nQty4,SUM(nQty5) AS nQty5,SUM(nQty6) AS nQty6,SUM(nQty7) AS nQty7,;
SUM(nQty8) AS nQty8,SUM(nTotQty) AS nTotQty FROM (lcTmpTkt) ;
WHERE !(Item+Color IN (SELECT Item+Color FROM (lcLotsDet))) ;
GROUP BY Item,Color INTO DBF (gcWorkDir+lcIsslogFile)

SELECT Item,Color,cDyelot,SUM(nQty1) AS nQty1,SUM(nQty2) AS nQty2,SUM(nQty3) AS nQty3,;
SUM(nQty4) AS nQty4,SUM(nQty5) AS nQty5,SUM(nQty6) AS nQty6,SUM(nQty7) AS nQty7,;
SUM(nQty8) AS nQty8,SUM(nTotQty) AS nTotQty FROM (lcTmpTkt) ;
WHERE !(Item+Color+cDyelot IN (SELECT Item+Color+cDyelot FROM (lcLotsDet))) ;
GROUP BY Item,Color,cDyelot INTO DBF (gcWorkDir+lcIsslogFile)
*B603243,1 AMM  end
SELECT (lcIsslogFile)
SCAN
  *B603243,1 AMM  make the operation done by style/color/dyelot
  *IF SEEK(lcTranType+laData[1]+lcOprCode+&lcIsslogFile..Item+&lcIsslogFile..Color,lcOprDet) .AND. &lcOprDet..cLotNo <> lcLotNo
  IF SEEK(lcTranType+laData[1]+lcOprCode+&lcIsslogFile..Item+&lcIsslogFile..Color+&lcIsslogFile..cDyelot,lcOprDet) .AND. &lcOprDet..cLotNo <> lcLotNo
  *B603243,1 AMM  end
    SELECT (lcOprDet)
    *B603243,1 AMM make the operation done by style/color/dyelot
    *SCAN REST WHILE cIMTYp+cTktNo+cOprCode+Item+Color = ;
                    lcTranType+laData[1]+lcOprCode+&lcIsslogFile..Item+&lcIsslogFile..Color ;
              FOR   INLIST(TranCd,'1','4') .AND. cLotNo <> lcLotNo
    SCAN REST WHILE cIMTYp+cTktNo+cOprCode+Item+Color+cDyelot = ;
                    lcTranType+laData[1]+lcOprCode+&lcIsslogFile..Item+&lcIsslogFile..Color+&lcIsslogFile..cDyelot ;
              FOR   INLIST(TranCd,'1','4') .AND. cLotNo <> lcLotNo
    *B603243,1 AMM  end
      SELECT (lcIsslogFile)
      REPLACE nQty1   WITH nQty1 - IIF(&lcOprDet..TranCd='1',&lcOprDet..nLotQty1,-&lcOprDet..nLotQty1) ,;
              nQty2   WITH nQty2 - IIF(&lcOprDet..TranCd='1',&lcOprDet..nLotQty1,-&lcOprDet..nLotQty1) ,;
              nQty3   WITH nQty3 - IIF(&lcOprDet..TranCd='1',&lcOprDet..nLotQty1,-&lcOprDet..nLotQty1) ,;
              nQty4   WITH nQty4 - IIF(&lcOprDet..TranCd='1',&lcOprDet..nLotQty1,-&lcOprDet..nLotQty1) ,;
              nQty5   WITH nQty5 - IIF(&lcOprDet..TranCd='1',&lcOprDet..nLotQty1,-&lcOprDet..nLotQty1) ,;
              nQty6   WITH nQty6 - IIF(&lcOprDet..TranCd='1',&lcOprDet..nLotQty1,-&lcOprDet..nLotQty1) ,;
              nQty7   WITH nQty7 - IIF(&lcOprDet..TranCd='1',&lcOprDet..nLotQty1,-&lcOprDet..nLotQty1) ,;
              nQty8   WITH nQty8 - IIF(&lcOprDet..TranCd='1',&lcOprDet..nLotQty1,-&lcOprDet..nLotQty1) ,;
              nTotQty WITH nTotQty - IIF(&lcOprDet..TranCd='1',&lcOprDet..nLotTotQty,-&lcOprDet..nLotTotQty)
    ENDSCAN
  ENDIF  
ENDSCAN
SET ORDER TO TAG lcCurrTag IN (lcOprDet)

IF lcTranType='T'
  *E301235,4 WAB - the header fo ntotqty must put it between ('')
  *E301235,4 WAB - START
  *lcBrFields = "Item:H='Fabric',Color,nTotQty:H='TotQty"
  lcBrFields = "Item:H='Fabric',Color,nTotQty:H='TotQty'"
  *E301235,4 WAB - END
ELSE
  *B603243,1 AMM make the operation done by style/color/dyelot
  *lcBrFields = "Item:H=lcItmHdr:19,nQty1:H='Qty1':6,nQty2:H='Qty2':6,nQty3:H='Qty3':6,nQty4:H='Qty4':6,nQty5:H='Qty5':6,nQty6:H='Qty6':6,nQty7:H='Qty7':6,nQty8:H='Qty8':6,nTotQty:H='TotQty':7"
  lcBrFields = "Item:H=lcItmHdr:19,cDyelot:H='Dyelot':10,nQty1:H='Qty1':6,nQty2:H='Qty2':6,nQty3:H='Qty3':6,nQty4:H='Qty4':6,nQty5:H='Qty5':6,nQty6:H='Qty6':6,nQty7:H='Qty7':6,nQty8:H='Qty8':6,nTotQty:H='TotQty':7"
  *B603243,1 AMM  end
ENDIF

IF _TALLY > 0 
  IF ARIABROW('','Add New Item',gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','','ITEM','laBrowArr')
    SELECT (lcLotsDet)
    APPEND BLANK
    *B603243,1 AMM  Add the update of dyelot field
    REPLACE Item      WITH &lcisslogfile..Item  ,;
            Color     WITH &lcisslogfile..Color ,;
            nToIssue1 WITH &lcisslogfile..nQty1 ,;
            nToIssue2 WITH &lcisslogfile..nQty2 ,;
            nToIssue3 WITH &lcisslogfile..nQty3 ,;
            nToIssue4 WITH &lcisslogfile..nQty4 ,;
            nToIssue5 WITH &lcisslogfile..nQty5 ,;
            nToIssue6 WITH &lcisslogfile..nQty6 ,;
            nToIssue7 WITH &lcisslogfile..nQty7 ,;
            nToIssue8 WITH &lcisslogfile..nQty8 ,;
            nTotToIss WITH &lcisslogfile..nTotQty ,;
            DueDate   WITH ldDueDate            ,;
            cDyelot   WITH &lcisslogfile..cDyelot
  ENDIF
ENDIF
USE IN (lcisslogfile)
SELECT (lcLotsDet)
=lfShModLot()

*!*************************************************************
*! Name      : lfvRemItm
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Remove Style/ Fabric-Color from a lot
*!*************************************************************
*! Calls     : gfModalGen,lfShModLot
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvRemItm()
*!*************************************************************
FUNCTION lfvRemItm
IF lnTotRcv+lnTotCan+lnTotDmg > 0
  *E300725,1 Message : 38061
  *E300725,1 Cannot delete this lot since the total recived, damaged, and 
  *E300725,1 canceled is greater than zero.
  *E300725,1 Button : 00000
  *E300725,1 Ok
  =gfModalGen('TRM38061B00000','ALERT','style')
  RETURN
ENDIF
*E300725,1 Message : 38036
*E300725,1 Are you sure you want to remove this record
*E300725,1 Button : 38002
*E300725,1 Proceed  Cancel
IF gfModalGen('QRM38036B38002','ALERT','remove')=1
  SELECT (lcLotsDet)
  DELETE
  GO TOP
  =lfShModLot()
ENDIF

*!*************************************************************
*! Name      : lfvRecLot
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Received,Canceled or Damaged operation/lot
*!*************************************************************
*! Calls     : MFRCVLT.SPX,lfRefTotal
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvRecLot()
*!*************************************************************
FUNCTION lfvRecLot
PRIVATE laOpr,laLots,puOpr,puLots,lcOprCode,lcLotNo,lcCurrTag,cbRcvAct,lcItemDesc

lcCurrTag = ORDER(lcOprDet)
SET ORDER TO TAG 'LOT' IN (lcOPrDet)
lnOprRecNo = RECNO(lcOprDet)
lcOprCode = &lcOprDet..cOprCode
lcLotNo   = &lcOprDet..cLotNO
*B603243,1 AMM  Add dyelot
*B126396,1 NNA 03/31/2005 (Begin) if we're in the Material Manufucturing Order then make the Numeric fields
*B126396,1 NNA            =three decimals.
*SELECT SPACE(1) AS cSelect,cIMTyp,cTktNo,cOprCode,cLotNo,Item,Color,cDyelot,;
       gdSysDate AS dTranDate,cContCode,;
       SUM(IIF(TranCd='1',nLotQty1,-nLotQty1)) AS nLotQty1 ,;
       SUM(IIF(TranCd='1',nLotQty2,-nLotQty2)) AS nLotQty2 ,;
       SUM(IIF(TranCd='1',nLotQty3,-nLotQty3)) AS nLotQty3 ,;
       SUM(IIF(TranCd='1',nLotQty4,-nLotQty4)) AS nLotQty4 ,;
       SUM(IIF(TranCd='1',nLotQty5,-nLotQty5)) AS nLotQty5 ,;
       SUM(IIF(TranCd='1',nLotQty6,-nLotQty6)) AS nLotQty6 ,;
       SUM(IIF(TranCd='1',nLotQty7,-nLotQty7)) AS nLotQty7 ,;
       SUM(IIF(TranCd='1',nLotQty8,-nLotQty8)) AS nLotQty8 ,;
       SUM(IIF(TranCd='1',nLotTotQty,-nLotTotQty)) AS nLotTotQty ,;
       00000 AS nReceive1,00000 AS nReceive2,00000 AS nReceive3,;
       00000 AS nReceive4,00000 AS nReceive5,00000 AS nReceive6,;
       00000 AS nReceive7,00000 AS nReceive8,000000 AS nTotRec;
FROM (lcOprDet) GROUP BY cIMTYP,cTktNo,cOprCode,cLotNo,Item,Color,cDyelot ;
INTO DBF (gcWorkDir+lcRcvFile)
lnNumeric = IIF(lcTranType = 'T',000000.000,000000)
SELECT SPACE(1) AS cSelect,cIMTyp,cTktNo,cOprCode,cLotNo,Item,Color,cDyelot,;
       gdSysDate AS dTranDate,cContCode,;
       SUM(IIF(TranCd='1',nLotQty1,-nLotQty1)) AS nLotQty1 ,;
       SUM(IIF(TranCd='1',nLotQty2,-nLotQty2)) AS nLotQty2 ,;
       SUM(IIF(TranCd='1',nLotQty3,-nLotQty3)) AS nLotQty3 ,;
       SUM(IIF(TranCd='1',nLotQty4,-nLotQty4)) AS nLotQty4 ,;
       SUM(IIF(TranCd='1',nLotQty5,-nLotQty5)) AS nLotQty5 ,;
       SUM(IIF(TranCd='1',nLotQty6,-nLotQty6)) AS nLotQty6 ,;
       SUM(IIF(TranCd='1',nLotQty7,-nLotQty7)) AS nLotQty7 ,;
       SUM(IIF(TranCd='1',nLotQty8,-nLotQty8)) AS nLotQty8 ,;
       SUM(IIF(TranCd='1',nLotTotQty,-nLotTotQty)) AS nLotTotQty ,;
       lnNumeric AS nReceive1,lnNumeric AS nReceive2,lnNumeric AS nReceive3,;
       lnNumeric AS nReceive4,lnNumeric AS nReceive5,lnNumeric AS nReceive6,;
       lnNumeric AS nReceive7,lnNumeric AS nReceive8,lnNumeric AS nTotRec;
FROM (lcOprDet) GROUP BY cIMTYP,cTktNo,cOprCode,cLotNo,Item,Color,cDyelot ;
INTO DBF (gcWorkDir+lcRcvFile)
*B126396,1 NNA (End)

INDEX ON cIMTYP+cTktNo+cOprCode+cLotNo+Item+Color+cDyelot TAG (lcRcvFile)
*B603243,1 AMM  end

*B603718,1 SSH  [Start] actualize cuttkt
*B603718,1 SSH          Allow multiple recieving with actualize.
*SELECT DIST ;
gfCodDes(&lcOprHdr..cOprCode,'MfgCode'),&lcOprHdr..cOprCode,&lcOprHdr..cOperSeq;
FROM (lcOprDet),(lcOprHdr);
WHERE &lcOprDet..cOprCode=&lcOprHdr..cOprCode .AND. &lcOprHdr..cOprCode <> laData[30];
AND &lcOprDet..TranCd='1' AND &lcOprDet..nActQty=0 ;
ORDER BY &lcOprHdr..cOperSeq INTO ARRAY laOpr

SELECT DIST ;
gfCodDes(&lcOprHdr..cOprCode,'MfgCode'),&lcOprHdr..cOprCode,&lcOprHdr..cOperSeq;
FROM (lcOprDet),(lcOprHdr);
WHERE &lcOprDet..cOprCode=&lcOprHdr..cOprCode .AND. &lcOprHdr..cOprCode <> laData[30];
AND &lcOprDet..TranCd='1';
ORDER BY &lcOprHdr..cOperSeq INTO ARRAY laOpr

*B603718,1 SSH   [END]
IF _TALLY = 0
  RETURN
ENDIF
lnopr = ASCAN(laOpr,lcOprCode)
lnopr = IIF(lnopr=0,1,ASUBSCRIPT(laOpr,lnopr,1))
puOpr = IIF(lcOprCode=laData[30],1,lnopr)
*B603243,1 AMM  Add dyelot
*B603718,1 SSH  [Start] actualize cuttkt
*B603718,1 SSH          Allow multiple recieving with actualize.
*SELECT DIST cLotNo FROM (lcOPrDet) ;
WHERE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd+cDyelot= lcTranType+laData[1]+laOpr[puOpr,2] ;
AND TranCd='1' AND nActQty=0 INTO ARRAY laLots

SELECT DIST cLotNo FROM (lcOPrDet) ;
WHERE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd+cDyelot= lcTranType+laData[1]+laOpr[puOpr,2] ;
AND TranCd='1'INTO ARRAY laLots

*B603718,1 SSH   [END]
*B603243,1 AMM  end
IF _TALLY =0
  RETURN
ENDIF
puLots = ASCAN(laLots,lcLotNo)
puLots = MAX(puLots,1)

DECLARE laToOpr[1,3]
SELECT gfCodDes(cOprCode,'MfgCode'),cOprCode,nNxtLotNo FROM (lcOprHdr);
WHERE VAL(cOperSeq) > VAL(laOpr[puOpr,3]) ORDER BY cOperSeq INTO ARRAY laToOpr
IF _TALLY =0
  RETURN
ENDIF
puToOpr   = 1
rbLotType = 1
lcToLotNo = laLots[puLots]
IF laData[28]='M'
  STORE SPACE(2)  TO lcToLotNo,lcToNewLot
  STORE 'DISABLE' TO lcToLotSt,lcToNewSt
  IF SEEK(lcTranType+laData[1]+laToOpr[puToOpr,2]+laLots[puLots],lcOprDet)
    lcToLotNo = laLots[puLots]
    rbLotType = 1
    lcToLotSt = 'ENABLE'
  ELSE
    lcToNewLot = laLots[puLots]
    rbLotType  = 2
    lcToNewSt  = 'ENABLE'
  ENDIF
  lcRadioSt = IIF(SEEK(lcTranType+laData[1]+laToOpr[puToOpr,2],lcOprDet),'ENABLE','DISABLE')
ENDIF
SELECT (lcRcvFile)
SET KEY TO lcTranType+laData[1]+laOpr[puOpr,2]+laLots[puLots]
=SEEK(lcTranType+laData[1]+laOpr[puOpr,2]+laLots[puLots])
STORE '' TO lcAction,lcItemDesc
cbRcvAct  = .F.
puTrnType = 1
lcText    = 'Quantity'
DO (gcScrDir+"MFRCVLT.SPX")
*B802785,1 AMM Refresh is acualaized
IF laData[3] = 'A'
  =lfRefresh(lcWinCh0)
ENDIF
*B802785,1 AMM end
=lfRefTotal()
IF llZoom
  SHOW WINDOW (lcBrZoom) REFRESH SAME
ELSE
  SHOW WINDOW (lcOprDtTit) REFRESH SAME
ENDIF
SELECT (lcOprDet)
SET ORDER TO TAG lcCurrTag
GO lnOprRecNo
=lfShOprDt()

*!*************************************************************
*! Name      : lfvRcvBrow
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Browse operations/lots lines to receive/cancel/damage
*!*************************************************************
*! Calls     : lfShRcv
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvRcvBrow()
*!*************************************************************
FUNCTION lfvRcvBrow
PRIVATE lcFields

*B603243,1 AMM Add dyelot field to the browse
*lcFields = "cMarker =IIF(RECNO()=lnDMarker,'>',' '):H=' ':R:1:W=.F.,"+;
           "cSelect :H= ' ' :R,Item :H= IIF(lcTranType='T','Fabric',lcItmHdr) :R,"+;
           IIF(lcTranType='T',"Color :H= 'Color' :R,",'')+"nTotRec :H= 'Tot Rcv' :R"+;
           IIF(lcTranType='T','',",nReceive1 :H= 'Rcv1' :R,"+;
                "nReceive2 :H= 'Rcv2' :R,nReceive3 :H= 'Rcv3' :R,"+;
                "nReceive4 :H= 'Rcv4' :R,nReceive5 :H= 'Rcv5' :R,"+;
                "nReceive6 :H= 'Rcv6' :R,nReceive7 :H= 'Rcv7' :R,nReceive8 :H= 'Rcv8':R")
lcFields = "cMarker =IIF(RECNO()=lnDMarker,'>',' '):H=' ':R:1:W=.F.,"+;
           "cSelect :H= ' ' :R,Item :H= IIF(lcTranType='T','Fabric',lcItmHdr) :R,"+;
           IIF(lcTranType='T',"Color :H= 'Color' :R,",'')+;
           IIF(llUseDyelot,"cDyelot :H='Dyelot',",'')+;
           "nTotRec :H= 'Tot Rcv' :R"+;
           IIF(lcTranType='T','',",nReceive1 :H= 'Rcv1' :R,"+;
                "nReceive2 :H= 'Rcv2' :R,nReceive3 :H= 'Rcv3' :R,"+;
                "nReceive4 :H= 'Rcv4' :R,nReceive5 :H= 'Rcv5' :R,"+;
                "nReceive6 :H= 'Rcv6' :R,nReceive7 :H= 'Rcv7' :R,nReceive8 :H= 'Rcv8':R")
*B603243,1 AMM  end
SELECT (lcRcvFile)
BROWSE FIELDS &lcFields  ;
       WINDOW MFRCVLT1   ;
       IN WINDOW MFRCVLT ;
       NOMENU            ;
       NOAPPEND          ;
       NODELETE          ;
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       WHEN lfShRcv()    ;
       TITLE lcRcvTit
=lfShRcv()

*!*************************************************************
*! Name      : lfShRcv
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Show operations/lots lines to receive/cancel/damage
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfShRcv()
*!*************************************************************
FUNCTION lfShRcv
*E301235,4 WAB - commit this line and get also the fabric.desc in case of MFG ord 
*E301235,4 WAB - START
*=SEEK(Item,'Style') 
*=SEEK('S'+Style.Scale,'Scale')
*lcItemDesc = Style.Desc1

IF lcTranType='T'
  =SEEK(PADR(ITEM,7)+COLOR,'Fabric') 
ELSE
  =SEEK(Item,'Style')    
  =SEEK('S'+Style.Scale,'Scale')
ENDIF
lcItemDesc = IIF(lcTranType='T',Fabric.desc,Style.Desc1)
*E301235,4 WAB - END

=lfRefresh('MFRCVLT2')

lnDMarker = RECNO(lcRcvFile)
SHOW WINDOW (lcRcvTit) REFRESH SAME
SHOW GET nTotRec DISABLE
IF cSelect = SPACE(1)
  lcEStatus = IIF(puTrnType=1,'DISABLE','ENABLE')
  SHOW GET pbSelect,1 PROMPT '\<Select' &lcEStatus
  IF lcTranType <> 'T'
    SHOW GET nReceive1 DISABLE
    SHOW GET nReceive2 DISABLE
    SHOW GET nReceive3 DISABLE
    SHOW GET nReceive4 DISABLE
    SHOW GET nReceive5 DISABLE
    SHOW GET nReceive6 DISABLE
    SHOW GET nReceive7 DISABLE
    SHOW GET nReceive8 DISABLE
  ENDIF
ELSE
  SHOW GET pbSelect,1 PROMPT 'Un\<Select'
  IF lcTranType = 'T'
    SHOW GET nTotRec ENABLE
  ELSE
    SHOW GET nReceive1 ENABLE
    SHOW GET nReceive2 ENABLE
    SHOW GET nReceive3 ENABLE
    SHOW GET nReceive4 ENABLE
    SHOW GET nReceive5 ENABLE
    SHOW GET nReceive6 ENABLE
    SHOW GET nReceive7 ENABLE
    SHOW GET nReceive8 ENABLE
  ENDIF  
ENDIF

*!*************************************************************
*! Name      : lfvTrnType
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : type of transaction to be done to operations/lots lines
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvTrnType()
*!*************************************************************
FUNCTION lfvTrnType
IF puTrnType = 1
  RETURN
ENDIF 
SHOW GET puTrnType DISABLE
SHOW GET puOpr     ENABLE
SHOW GET puLots    ENABLE
IF puTrnType = 2
  SHOW GET puToOpr  ENABLE
  IF laData[28]='M'
    SHOW GET rbLotType  &lcRadioSt
    SHOW GET lcToLotNo  &lcToLotSt
    SHOW GET lcToNewLot &lcToNewSt
  ENDIF  
ENDIF
SHOW GET pbSelect ENABLE 
SHOW GET pbSelAll ENABLE 
SHOW GET pbInvert ENABLE 
SHOW GET pbOk     ENABLE

lcAction = STR(puTrnType,1)
lctext   = IIF(puTrnType=2,'Receive',IIF(puTrnType=3,'Damaged','Canceled'))
=lfRefresh('MFRCVLT2')

*!*************************************************************
*! Name      : lfvOprPop
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate received/Canceled/Damaged operation popup
*!*************************************************************
*! Calls     : lfAdjRButt
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvOprPop()
*!*************************************************************
FUNCTION lfvOprPop
PRIVATE lcCurrTag
IF puOpr <> lcOldValue
  lcCurrTag = ORDER(lcOprDet)
  SET ORDER TO TAG 'LOT' IN (lcOprDet)
  SELECT DIST cLotNo FROM (lcOPrDet) ;
  WHERE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd= lcTranType+laData[1]+laOpr[puOpr,2] ;
  INTO ARRAY laLots
  puLots = 1
  SHOW GET puLots
  
  DECLARE laToOpr[1,3]
  SELECT gfCodDes(cOprCode,'MfgCode'),cOprCode,nNxtLotNo FROM (lcOprHdr);
  WHERE cOperSeq > laOpr[puOpr,3] ORDER BY cOperSeq INTO ARRAY laToOpr
  puToOpr = 1
  SHOW GET puToOpr
  SELECT (lcRcvFile)
  REPLACE ALL cSelect   WITH SPACE(1) ,;
              nReceive1 WITH 0 ,;
              nReceive2 WITH 0 ,;
              nReceive3 WITH 0 ,;
              nReceive4 WITH 0 ,;
              nReceive5 WITH 0 ,;
              nReceive6 WITH 0 ,;
              nReceive7 WITH 0 ,;
              nReceive8 WITH 0 ,;
              nTotRec   WITH 0
  SET KEY TO lcTranType+laData[1]+laOpr[puOpr,2]+laLots[puLots]
  =SEEK(lcTranType+laData[1]+laOpr[puOpr,2]+laLots[puLots])
  =lfAdjRButt('')
  SHOW WINDOW (lcRcvTit) REFRESH SAME
  =IIF(laData[28]='M',lfvToOprPop(),.T.)
  SET ORDER TO TAG lcCurrTag IN (lcOprDet)
ENDIF

*!*************************************************************
*! Name      : lfvLotPop
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate received/Canceled/Damaged Lot popup
*!*************************************************************
*! Calls     : lfAdjRButt
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvLotPop()
*!*************************************************************
FUNCTION lfvLotPop
IF puLots<>lcOldValue
  SELECT (lcRcvFile)
  REPLACE ALL cSelect   WITH SPACE(1) ,;
              nReceive1 WITH 0 ,;
              nReceive2 WITH 0 ,;
              nReceive3 WITH 0 ,;
              nReceive4 WITH 0 ,;
              nReceive5 WITH 0 ,;
              nReceive6 WITH 0 ,;
              nReceive7 WITH 0 ,;
              nReceive8 WITH 0 ,;
              nTotRec   WITH 0
  SET KEY TO lcTranType+laData[1]+laOpr[puOpr,2]+laLots[puLots]
  =SEEK(lcTranType+laData[1]+laOpr[puOpr,2]+laLots[puLots])
  SHOW WINDOW (lcRcvTit) REFRESH SAME
  =lfAdjRButt('')
  =IIF(laData[28]='M',lfvToOprPop(),.T.)
ENDIF

*!*************************************************************
*! Name      : lfvToOprPop
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate received to operation popup
*!*************************************************************
*! Calls     : lfvLotRadio
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvToOprPop()
*!*************************************************************
FUNCTION lfvToOprPop
PRIVATE lcCurrTag

IF laData[28]= 'M' .AND. puToOpr<>lcOldValue
  lcCurrTag = ORDER(lcOprDet)
  SET ORDER TO TAG 'LOT' IN (lcOprDet)
  STORE SPACE(2)  TO lcToLotNo,lcToNewLot
  IF SEEK(lcTranType+laData[1]+laToOpr[puToOpr,2]+laLots[puLots],lcOprDet)
    lcToLotNo = laLots[puLots]
    rbLotType = 1
  ELSE
    lcToNewLot = laLots[puLots]
    rbLotType  = 2
  ENDIF
  IF SEEK(lcTranType+laData[1]+laToOpr[puToOpr,2],lcOprDet)
    SHOW GET rbLotType ENABLE
  ELSE
    SHOW GET rbLotType DISABLE
  ENDIF
  =lfvLotRadio()
  SET ORDER TO TAG lcCurrTag IN (lcOprDet)
ENDIF

*!*************************************************************
*! Name      : lfvLotRadio
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate received to lot radio button
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvLotRadio()
*!*************************************************************
FUNCTION lfvLotRadio

IF rbLotType = 1
  lcToLotNo  = laLots[puLots]
  lcToNewLot = SPACE(2)
  SHOW GET lcToLotNo  ENABLE
  SHOW GET lcToNewLot DISABLE
  _CUROBJ = OBJNUM(lcToLotNo)
ELSE
  lcToLotNo  = SPACE(2)
  lcToNewLot = PADL(laToOpr[1,3],2,'0')
  SHOW GET lcToLotNo  DISABLE
  SHOW GET lcToNewLot ENABLE
  _CUROBJ = OBJNUM(lcToLotNo)
ENDIF

*!*************************************************************
*! Name      : lfAdjRButt
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Select, Unselect, Select all, Select None and Invert items
*!             in the receive/cancel/damaged operation screen 
*!*************************************************************
*! Calls     : lfShRcv
*!*************************************************************
*! Parameters: lcType  'S' : Select
*!                     'A' : Select All
*!                     'N' : Select None
*!                     'V' : Invert
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfAdjRButt()
*!*************************************************************
FUNCTION lfAdjRButt
PARAMETERS lcType
PRIVATE lnRecNo,lcDataSel,lcDataUSel

SELECT (lcRcvFile)
lnRecNo = RECNO()
DO CASE 
  CASE lcType = 'S' AND (cSelect="»" .OR. ;
       lfvCheckOrp(laToOpr[puToOpr,2],Item,Color))
    REPLACE cSelect   WITH IIF(cSelect=SPACE(1),"»",SPACE(1)) ,;
            nReceive1 WITH IIF(cSelect=SPACE(1),0,MAX(nLotQty1,0)) ,;
            nReceive2 WITH IIF(cSelect=SPACE(1),0,MAX(nLotQty2,0)) ,;
            nReceive3 WITH IIF(cSelect=SPACE(1),0,MAX(nLotQty3,0)) ,;
            nReceive4 WITH IIF(cSelect=SPACE(1),0,MAX(nLotQty4,0)) ,;
            nReceive5 WITH IIF(cSelect=SPACE(1),0,MAX(nLotQty5,0)) ,;
            nReceive6 WITH IIF(cSelect=SPACE(1),0,MAX(nLotQty6,0)) ,;
            nReceive7 WITH IIF(cSelect=SPACE(1),0,MAX(nLotQty7,0)) ,;
            nReceive8 WITH IIF(cSelect=SPACE(1),0,MAX(nLotQty8,0)) ,;
            nTotRec   WITH IIF(cSelect=SPACE(1),0,;
                           IIF(lcTranType='T',MAX(nLotTotQty,0),;
                           nReceive1+nReceive2+nReceive3+nReceive4+;
                           nReceive5+nReceive6+nReceive7+nReceive8))
  CASE lcType = 'A'
    SCAN FOR lfvCheckOrp(laToOpr[puToOpr,2],Item,Color)
      REPLACE cSelect   WITH "»" ,;
              nReceive1 WITH MAX(nLotQty1,0) ,;
              nReceive2 WITH MAX(nLotQty2,0) ,;
              nReceive3 WITH MAX(nLotQty3,0) ,;
              nReceive4 WITH MAX(nLotQty4,0) ,;
              nReceive5 WITH MAX(nLotQty5,0) ,;
              nReceive6 WITH MAX(nLotQty6,0) ,;
              nReceive7 WITH MAX(nLotQty7,0) ,;
              nReceive8 WITH MAX(nLotQty8,0) ,;
              nTotRec   WITH IIF(lcTranType='T',MAX(nLotTotQty,0),;
                             nReceive1+nReceive2+nReceive3+nReceive4+;
                             nReceive5+nReceive6+nReceive7+nReceive8)
    ENDSCAN
  CASE lcType = 'N'
    REPLACE ALL cSelect   WITH SPACE(1) ,;
                nReceive1 WITH 0 ,;
                nReceive2 WITH 0 ,;
                nReceive3 WITH 0 ,;
                nReceive4 WITH 0 ,;
                nReceive5 WITH 0 ,;
                nReceive6 WITH 0 ,;
                nReceive7 WITH 0 ,;
                nReceive8 WITH 0 ,;
                nTotRec   WITH 0
  CASE lcType = 'V'
    SCAN FOR IIF(EMPTY(cSelect),lfvCheckOrp(laToOpr[puToOpr,2],Item,Color),.T.)
      REPLACE cSelect   WITH IIF(cSelect=SPACE(1),"»",SPACE(1)) ,;
              nReceive1 WITH IIF(cSelect=SPACE(1),0,MAX(nLotQty1,0)) ,;
              nReceive2 WITH IIF(cSelect=SPACE(1),0,MAX(nLotQty2,0)) ,;
              nReceive3 WITH IIF(cSelect=SPACE(1),0,MAX(nLotQty3,0)) ,;
              nReceive4 WITH IIF(cSelect=SPACE(1),0,MAX(nLotQty4,0)) ,;
              nReceive5 WITH IIF(cSelect=SPACE(1),0,MAX(nLotQty5,0)) ,;
              nReceive6 WITH IIF(cSelect=SPACE(1),0,MAX(nLotQty6,0)) ,;
              nReceive7 WITH IIF(cSelect=SPACE(1),0,MAX(nLotQty7,0)) ,;
              nReceive8 WITH IIF(cSelect=SPACE(1),0,MAX(nLotQty8,0)) ,;
              nTotRec   WITH IIF(cSelect=SPACE(1),0,;
                             IIF(lcTranType='T',MAX(nLotTotQty,0),;
                                 nReceive1+nReceive2+nReceive3+nReceive4+;
                                 nReceive5+nReceive6+nReceive7+nReceive8))
    ENDSCAN
  OTHERWISE
ENDCASE
LOCATE FOR EMPTY(cSelect)
lcDataUSel = IIF(FOUND(),'ENABLE','DISABLE')
LOCATE FOR !EMPTY(cSelect)
IF lcAction = '2'
  IF laOpr[puOpr,2] = lcFirstOpr .AND. FOUND()
    cbRcvAct = .T.
    SHOW GET cbRcvAct ENABLE
  ELSE
    cbRcvAct = .F.
    SHOW GET cbRcvAct DISABLE
  ENDIF
ENDIF  
lcDataSel  = IIF(FOUND(),'ENABLE','DISABLE')
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF
SHOW GET pbSelAll  &lcDataUSel
SHOW GET pbSelNone &lcDataSel
=lfShRcv()

*!*************************************************************
*! Name      : lfwRcvQty
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : When function for received/canceled or damaged quantity
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfwRcvQty()
*!*************************************************************
FUNCTION lfwRcvQty
PARAMETERS lcSize
IF lcTranType<> 'T' .AND. Scale.Cnt < VAL(lcSize)
  RETURN(.F.)
ENDIF
*E301235,4 WAB - case material order man. cost sheet lcSize = .f. so it's sysntax ERROR
*E301235,4 WAB - start
*lcOldValue = IIF(lcTranType='T',nTotRec,nReceive&lcSize)
IF lcTranType='T' 
  lcOldValue = nTotRec
ELSE
  lcOldValue = nReceive&lcSize
ENDIF
*E301235,4 WAB - END

*!*************************************************************
*! Name      : lfvRcvQty
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Validate received/canceled or damaged quantity
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvRcvQty()
*!*************************************************************
FUNCTION lfvRcvQty
PARAMETERS lcSize

*E300725,1 Message : 38078
*E300725,1 Received quantity cannot be less than zero
*E300725,1 Button : 00000
*E300725,1 Ok

*E300725,1 Message : 38078
*E300725,1 Canceled quantity cannot exceed open quantity
*E300725,1 Button : 00000
*E300725,1 Ok
IF lcTranType = 'T'
  IF (nTotRec < 0 .AND. gfModalGen('TRM38078B00000','ALERT',;
     IIF(lcAction='2','Receive',IIF(lcAction='3','Damaged','Canceled'))+'|'+'be less than zero')=1 ).OR. ;
    (lcAction='4' .AND. nTotRec > nLotTotQty .AND. ;
    gfModalGen('TRM38078B00000','ALERT','Canceled'+'|'+'exceed open quantity')=1)
    REPLACE nTotRec WITH lcOldValue
    RETURN
  ENDIF
ELSE
  IF (nReceive&lcSize < 0 .AND. gfModalGen('TRM38078B00000','ALERT',;
     IIF(lcAction='2','Receive',IIF(lcAction='3','Damaged','Canceled'))+'|'+'be less than zero')=1 ).OR. ;
    (lcAction='4' .AND. nReceive&lcSize > nLotQty&lcSize .AND. ;
     gfModalGen('TRM38078B00000','ALERT','Canceled'+'|'+'exceed open quantity')=1)
    REPLACE nReceive&lcSize WITH lcOldValue
    RETURN
  ENDIF
  REPLACE nTotRec WITH nReceive1+nReceive2+nReceive3+nReceive4+nReceive5+;
                       nReceive6+nReceive7+nReceive8
  SHOW GET nTotRec DISABLE
ENDIF  
SHOW WINDOW (lcRcvTit) REFRESH SAME

*!*************************************************************
*! Name      : lfvLotTran
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Accept Lot receive, Cancel or Damaged
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcAction  : '2' Receive
*!                         '3' Damage
*!                         '4' Cancel
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvLotTran()
*!*************************************************************
FUNCTION lfvLotTran
PARAMETERS lcAction
SELECT (lcRcvFile)
SCAN FOR !EMPTY(cSelect) .AND. nTotRec > 0
  *B801929,1 Update operation sequence.
  =SEEK(lcTranType+laData[1]+&lcRcvFile..cOprCode,'MFGOPRHD')
  *B603718,1 SSH 24/07/2000[Start] Accoumulate on the actualized
  llAcomulat = .F.
  lcKeyToSek = cImTyp+cTktNo+cOprCode+cLotNo+Item+Color+'1'+cDyelot
  lcRecToSek = cImTyp+cTktNo+cOprCode+cLotNo+Item+Color+'2'+cDyelot
  SELECT (lcOprDet)
  IF !SEEK(lcKeyToSek) .OR. lcAction <> '2'
  *B603718,1 SSH 24/07/2000[END]
    APPEND BLANK
  *B603718,1 SSH 24/07/2000[Start] Accoumulate on the actualized
  ELSE
    llAcomulat = SEEK(lcRecToSek)
    IF !llAcomulat
      APPEND BLANK
    ENDIF
  ENDIF
  *B603718,1 SSH 24/07/2000[END]

  *B603243,1 AMM  Add dyelot field
  *B603718,1 SSH 24/07/2000[Start] Accoumulate on the actualized
  IF !llAcomulat
  *B603718,1 SSH 24/07/2000[End]
    REPLACE cIMTYp      WITH lcTranType            ,;
            cTktNo      WITH laData[1]             ,;
            cOprCode    WITH &lcRcvFile..cOprCode  ,;
            cLotNo      WITH &lcRcvFile..cLotNo    ,;
            Item        WITH &lcRcvFile..Item      ,;
            Color       WITH &lcRcvFile..Color     ,;
            dTranDate   WITH &lcRcvFile..dTranDate ,;
            cContCode   WITH &lcRcvFile..cContCode ,;
            TranCd      WITH lcAction              ,;
            nLotQty1    WITH &lcRcvFile..nReceive1 ,;
            nLotQty2    WITH &lcRcvFile..nReceive2 ,;
            nLotQty3    WITH &lcRcvFile..nReceive3 ,;
            nLotQty4    WITH &lcRcvFile..nReceive4 ,;
            nLotQty5    WITH &lcRcvFile..nReceive5 ,;
            nLotQty6    WITH &lcRcvFile..nReceive6 ,;
            nLotQty7    WITH &lcRcvFile..nReceive7 ,;
            nLotQty8    WITH &lcRcvFile..nReceive8 ,;
            nLotTotQty  WITH &lcRcvFile..nTotRec   ,;
            cTrgOpr     WITH laToOpr[puToOpr,2]    ,;
            cTrgLot     WITH IIF(rbLotType=1,lcToLotNo,lcToNewLot) ,;
            cOperSeq    WITH MFGOPRHD.cOperSeq,;
            cDyelot     WITH &lcRcvFile..cDyelot
  *B603718,1 SSH 24/07/2000[Start] Accoumulate on the actualized
  ELSE
    REPLACE cIMTYp      WITH lcTranType            ,;
            cTktNo      WITH laData[1]             ,;
            cOprCode    WITH &lcRcvFile..cOprCode  ,;
            cLotNo      WITH &lcRcvFile..cLotNo    ,;
            Item        WITH &lcRcvFile..Item      ,;
            Color       WITH &lcRcvFile..Color     ,;
            dTranDate   WITH &lcRcvFile..dTranDate ,;
            cContCode   WITH &lcRcvFile..cContCode ,;
            TranCd      WITH lcAction              ,;
            nLotQty1    WITH nLotQty1+&lcRcvFile..nReceive1 ,;
            nLotQty2    WITH nLotQty2+&lcRcvFile..nReceive2 ,;
            nLotQty3    WITH nLotQty3+&lcRcvFile..nReceive3 ,;
            nLotQty4    WITH nLotQty4+&lcRcvFile..nReceive4 ,;
            nLotQty5    WITH nLotQty5+&lcRcvFile..nReceive5 ,;
            nLotQty6    WITH nLotQty6+&lcRcvFile..nReceive6 ,;
            nLotQty7    WITH nLotQty7+&lcRcvFile..nReceive7 ,;
            nLotQty8    WITH nLotQty8+&lcRcvFile..nReceive8 ,;
            nLotTotQty  WITH nLotTotQty  +&lcRcvFile..nTotRec   ,;
            cTrgOpr     WITH laToOpr[puToOpr,2]    ,;
            cTrgLot     WITH IIF(rbLotType=1,lcToLotNo,lcToNewLot) ,;
            cOperSeq    WITH MFGOPRHD.cOperSeq,;
            cDyelot     WITH &lcRcvFile..cDyelot
  ENDIF        
  *B603718,1 SSH 24/07/2000[End]
  IF laScrMode[2]
    SELECT MFGOPRDT
    *--- cImTyp+cTktNo+CoprCode+CLotNo+Trancd
    *--- cImTyp+cTktNo+cOprCode+cLotNo+Item+Color+'1'+cDyelot
    =SEEK(&lcRcvFile..cImTyp+laData[1]+&lcRcvFile..cOprCode+&lcRcvFile..CLotNo+'1')
    *B603718,1 SSH 24/07/2000[Start] Accoumulate on the actualized
    llAcomulate = .F.
    LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd = ;
                      &lcRcvFile..cImTyp+laData[1]+&lcRcvFile..cOprCode+;
                      &lcRcvFile..CLotNo+'1' ;
                FOR   Item+Color+cDyelot=&lcRcvFile..Item+&lcRcvFile..Color+&lcRcvFile..cDyelot
    IF !FOUND() .OR. lcAction <> '2'
      *B603718,1 SSH 24/07/2000[End]
      APPEND BLANK
    *B603718,1 SSH 24/07/2000[Start] Accoumulate on the actualized
    ELSE
      llAcomulate = SEEK(&lcRcvFile..cImTyp+laData[1]+&lcRcvFile..cOprCode+&lcRcvFile..CLotNo+'2')
      LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd = ;
                        &lcRcvFile..cImTyp+laData[1]+&lcRcvFile..cOprCode+;
                        &lcRcvFile..CLotNo+'2' ;
                  FOR   Item+Color+cDyelot=&lcRcvFile..Item+&lcRcvFile..Color+&lcRcvFile..cDyelot
      llAcomulate = FOUND()
      IF !llAcomulat
        APPEND BLANK
      ENDIF
    ENDIF
    *B603718,1 SSH 24/07/2000[End]
    *B603243,1 AMM  Add dyelot
  *B603718,1 SSH 24/07/2000[Start] Accoumulate on the actualized
    IF !llAcomulate
  *B603718,1 SSH 24/07/2000[End]
      REPLACE cIMTYp      WITH lcTranType            ,;
              cTktNo      WITH laData[1]             ,;
              cOprCode    WITH &lcRcvFile..cOprCode  ,;
              cLotNo      WITH &lcRcvFile..cLotNo    ,;
              Item        WITH &lcRcvFile..Item      ,;
              Color       WITH &lcRcvFile..Color     ,;
              dTranDate   WITH &lcRcvFile..dTranDate ,;
              cContCode   WITH &lcRcvFile..cContCode ,;
              TranCd      WITH lcAction              ,;
              nLotQty1    WITH &lcRcvFile..nReceive1 ,;
              nLotQty2    WITH &lcRcvFile..nReceive2 ,;
              nLotQty3    WITH &lcRcvFile..nReceive3 ,;
              nLotQty4    WITH &lcRcvFile..nReceive4 ,;
              nLotQty5    WITH &lcRcvFile..nReceive5 ,;
              nLotQty6    WITH &lcRcvFile..nReceive6 ,;
              nLotQty7    WITH &lcRcvFile..nReceive7 ,;
              nLotQty8    WITH &lcRcvFile..nReceive8 ,;
              nLotTotQty  WITH &lcRcvFile..nTotRec   ,;
              cTrgOpr     WITH laToOpr[puToOpr,2]    ,;
              cTrgLot     WITH IIF(rbLotType=1,lcToLotNo,lcToNewLot),;
              cDyelot     WITH &lcRcvFile..cDyelot
  *B603718,1 SSH 24/07/2000[Start] Accoumulate on the actualized
  ELSE
      REPLACE cIMTYp      WITH lcTranType            ,;
              cTktNo      WITH laData[1]             ,;
              cOprCode    WITH &lcRcvFile..cOprCode  ,;
              cLotNo      WITH &lcRcvFile..cLotNo    ,;
              Item        WITH &lcRcvFile..Item      ,;
              Color       WITH &lcRcvFile..Color     ,;
              dTranDate   WITH &lcRcvFile..dTranDate ,;
              cContCode   WITH &lcRcvFile..cContCode ,;
              TranCd      WITH lcAction              ,;
              nLotQty1    WITH nLotQty1+&lcRcvFile..nReceive1 ,;
              nLotQty2    WITH nLotQty2+&lcRcvFile..nReceive2 ,;
              nLotQty3    WITH nLotQty3+&lcRcvFile..nReceive3 ,;
              nLotQty4    WITH nLotQty4+&lcRcvFile..nReceive4 ,;
              nLotQty5    WITH nLotQty5+&lcRcvFile..nReceive5 ,;
              nLotQty6    WITH nLotQty6+&lcRcvFile..nReceive6 ,;
              nLotQty7    WITH nLotQty7+&lcRcvFile..nReceive7 ,;
              nLotQty8    WITH nLotQty8+&lcRcvFile..nReceive8 ,;
              nLotTotQty  WITH nLotTotQty  +&lcRcvFile..nTotRec   ,;
              cTrgOpr     WITH laToOpr[puToOpr,2]    ,;
              cTrgLot     WITH IIF(rbLotType=1,lcToLotNo,lcToNewLot),;
              cDyelot     WITH &lcRcvFile..cDyelot
  ENDIF
  *B603718,1 SSH 24/07/2000[End]
  ENDIF
  IF lcAction = '2'
    SELECT (lcOprHdr)
    =gfRltFld(laToOpr[puToOpr,2],@laMfgRFld,'MFGCODE')
    LOCATE FOR cOprCode = laToOpr[puToOpr,2]
    IF FOUND()
      REPLACE nNxtLotNo WITH MAX(nNxtLotNo,VAL(IIF(rbLotType=1,lcToLotNo,lcToNewLot))+1)
      IF laScrMode[2] .AND. SEEK(lcTranType+laData[1]+laToOpr[puToOpr,2],'MFGOPRHD')
        SELECT MFGOPRHD
        REPLACE nNxtLotNo WITH MAX(nNxtLotNo,VAL(IIF(rbLotType=1,lcToLotNo,lcToNewLot))+1)
      ENDIF
      SELECT (lcOprDet)
      *B603243,1 AMM make the operation done by style/color/dyelot
      *IF !SEEK(lcTranType+laData[1]+laToOpr[puToOpr,2]+IIF(rbLotType=1,lcToLotNo,lcToNewLot)+;
               &lcRcvFile..Item+&lcRcvFile..Color+'1')
      IF !SEEK(lcTranType+laData[1]+laToOpr[puToOpr,2]+IIF(rbLotType=1,lcToLotNo,lcToNewLot)+;
               &lcRcvFile..Item+&lcRcvFile..Color+'1'+&lcRcvFile..cDyelot)
      *B603243,1 AMM  end
        APPEND BLANK
        *B801929,1 Update operation sequence.
        *B603243,1 AMM  Add dyelot
        REPLACE cIMTYp      WITH lcTranType            ,;
                cTktNo      WITH laData[1]             ,;
                cOprCode    WITH laToOpr[puToOpr,2]    ,;
                cLotNo      WITH IIF(rbLotType=1,lcToLotNo,lcToNewLot)  ,;
                Item        WITH &lcRcvFile..Item      ,;
                Color       WITH &lcRcvFile..Color     ,;
                TranCd      WITH '1'                   ,;
                cContCode   WITH &lcOprHdr..cContCode  ,;
                lInHouse    WITH &lcOprHdr..lInHouse   ,;
                cOperSeq    WITH &lcOprHdr..cOperSeq   ,;
                cDyelot     WITH &lcRcvFile..cDyelot
      ENDIF
      REPLACE dTranDate   WITH &lcRcvFile..dTranDate ,;
              DueDate     WITH dTranDate+lnLeadTime  ,;
              nLotQty1    WITH nLotQty1+&lcRcvFile..nReceive1 ,;
              nLotQty2    WITH nLotQty2+&lcRcvFile..nReceive2 ,;
              nLotQty3    WITH nLotQty3+&lcRcvFile..nReceive3 ,;
              nLotQty4    WITH nLotQty4+&lcRcvFile..nReceive4 ,;
              nLotQty5    WITH nLotQty5+&lcRcvFile..nReceive5 ,;
              nLotQty6    WITH nLotQty6+&lcRcvFile..nReceive6 ,;
              nLotQty7    WITH nLotQty7+&lcRcvFile..nReceive7 ,;
              nLotQty8    WITH nLotQty8+&lcRcvFile..nReceive8 ,;
              nLotTotQty  WITH nLotTotQty+&lcRcvFile..nTotRec
      IF laScrMode[2]
        SELECT MFGOPRDT
        =SEEK(lcTranType+laData[1]+laToOpr[puToOpr,2]+IIF(rbLotType=1,lcToLotNo,lcToNewLot)+'1')
        *B603243,1 AMM make the operation done by style/color/dyelot
        *LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd = ;
                          lcTranType+laData[1]+laToOpr[puToOpr,2]+IIF(rbLotType=1,lcToLotNo,lcToNewLot)+'1' ;
                    FOR   Item+Color=&lcRcvFile..Item+&lcRcvFile..Color
        LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd = ;
                          lcTranType+laData[1]+laToOpr[puToOpr,2]+IIF(rbLotType=1,lcToLotNo,lcToNewLot)+'1' ;
                    FOR   Item+Color+cDyelot=&lcRcvFile..Item+&lcRcvFile..Color+&lcRcvFile..cDyelot
        *B603243,1 AMM  end
        IF !FOUND()
          APPEND BLANK
          *B603243,1 AMM  Add dyelot
          REPLACE cIMTYp      WITH lcTranType           ,;
                  cTktNo      WITH laData[1]            ,;
                  cOprCode    WITH laToOpr[puToOpr,2]   ,;
                  cLotNo      WITH IIF(rbLotType=1,lcToLotNo,lcToNewLot) ,;
                  Item        WITH &lcRcvFile..Item     ,;
                  Color       WITH &lcRcvFile..Color    ,;
                  TranCd      WITH '1'                  ,;
                  cContCode   WITH MFGOPRHD.cContCode   ,;
                  lInHouse    WITH MFGOPRHD.lInHouse    ,;
                  cDyelot     WITH &lcRcvFile..cDyelot
        ENDIF
        REPLACE dTranDate   WITH &lcRcvFile..dTranDate ,;
                DueDate     WITH dTranDate+lnLeadTime  ,;
                nLotQty1    WITH nLotQty1+&lcRcvFile..nReceive1 ,;
                nLotQty2    WITH nLotQty2+&lcRcvFile..nReceive2 ,;
                nLotQty3    WITH nLotQty3+&lcRcvFile..nReceive3 ,;
                nLotQty4    WITH nLotQty4+&lcRcvFile..nReceive4 ,;
                nLotQty5    WITH nLotQty5+&lcRcvFile..nReceive5 ,;
                nLotQty6    WITH nLotQty6+&lcRcvFile..nReceive6 ,;
                nLotQty7    WITH nLotQty7+&lcRcvFile..nReceive7 ,;
                nLotQty8    WITH nLotQty8+&lcRcvFile..nReceive8 ,;
                nLotTotQty  WITH nLotTotQty+&lcRcvFile..nTotRec
      ENDIF
    ENDIF
  ENDIF
  *C200444,1 ALB Add GL entries when recieving Non GL operations [Begin]
  IF ASCAN(laEvntTrig , PADR('UPDTOPGL',10)) <> 0
    =gfDoTriger('POCSSH',PADR('UPDTOPGL',10))
  ENDIF
  *C200444,1 ALB Add GL entries when recieving Non GL operations [end]
ENDSCAN
IF cbRcvAct
  lcOprCode = lcFirstOpr
  ldActDate = gdSysDate
  rbActual = 2
  IF ladata[28]='M'
    rbActual = 1
    DECLARE laActLot[1]
    laActLot[1] = laLots[puLots]
    puActLot = 1
  ENDIF
  lcStatColor=''
  STORE 0 TO lnLotbud,lnLotRcv,lnLotCan,lnLotDmg
  =IIF(ladata[28]='M',lfGetSummary(laActLot[1]),lfGetSummary(''))
  DO (gcScrDir+"MFACTUAL.SPX") WITH .T.
  =lfRefresh(lcWinCh0)
ENDIF

*!*************************************************************
*! Name      : lfvActualize
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Actualize the whole cutting ticket or a specefic lot in the 
*!             first operation.
*!*************************************************************
*! Calls     : MFACTUAL.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvActualize()
*!*************************************************************
FUNCTION lfvActualize
PRIVATE ldActDate,puActLot,laActLot,rbActual,lcOprCode,lcCurrTag

lcOprCode = lcFirstOpr
ldActDate = gdSysDate
rbActual  = 2
llCalled = .F.
IF ladata[28]='M'
  lcCurrTag = ORDER(lcOprDet)
  SET ORDER TO TAG 'LOT' IN (lcOprDet)
  rbActual  = 1
  *B603718,1 SSH  [Start] actualize cuttkt
  *B603718,1 SSH          Allow multiple recieving with actualize.
  *SELECT DIST cLotNo FROM (lcOPrDet) ;
  WHERE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
        lcTranType+laData[1]+lcFirstOpr AND TranCd='1' AND nActQty=0 ;
        INTO ARRAY laActLot

  SELECT DIST cLotNo FROM (lcOPrDet) ;
  WHERE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
        lcTranType+laData[1]+lcFirstOpr AND TranCd='1';
        INTO ARRAY laActLot
  *B603718,1 SSH   [End]
  IF _TALLY = 0
    llCalled = .T.
    rbActual = 2
  ENDIF
  puActLot = 1
  SET ORDER TO TAG lcCurrTag IN (lcOprDet)
ENDIF
lcStatColor=''
STORE 0 TO lnLotbud,lnLotRcv,lnLotCan,lnLotDmg

*E302004,1 AMH Ability to Actu. C/T with one oper. [Start]
*=IIF(ladata[28]='M' AND !llCalled,lfGetSummary(laActLot[1]),lfGetSummary(''))
*DO (gcScrDir+"MFACTUAL.SPX") WITH llCalled
IF llOnlyOne
  =lfAct1Opr()
  DO (gcScrDir+"MFACTONE.SPX")
ELSE
  =IIF(ladata[28]='M' AND !llCalled,lfGetSummary(laActLot[1]),lfGetSummary(''))
  DO (gcScrDir+"MFACTUAL.SPX") WITH llCalled
ENDIF
*E302004,1 AMH [End]

*B802785,1 AMM Comment out, Move it to other location
*=lfRefresh(lcWinCh0)
*B802785,1 AMM end
*!*************************************************************
*! Name      : lfvOkAct
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Actualize the whole cutting ticket or a specefic lot in the 
*!             first operation.
*!*************************************************************
*! Calls     : lfBrowLots,lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvOkAct()
*!*************************************************************
FUNCTION lfvOkAct
*B604056,1 HBG 12/06/2000 Change the name of the variabel lcKey
*B604056,1                To fix bug variabel 'lcKey' not found [Begin]
*PRIVATE lnAlias,lcLotNo,lcItem,lcColor,lnOpen1,lnOpen2,lnOpen3,lnOpen4,;
        lnOpen5,lnOpen6,lnOpen7,lnOpen8,lnOpen,lnRecv,llInHouse,;
        lcContCode,lcContName,lcCurrTag,lnUpdYield,lcKey
PRIVATE lnAlias,lcLotNo,lcItem,lcColor,lnOpen1,lnOpen2,lnOpen3,lnOpen4,;
        lnOpen5,lnOpen6,lnOpen7,lnOpen8,lnOpen,lnRecv,llInHouse,;
        lcContCode,lcContName,lcCurrTag,lnUpdYield,lcKeyVal
*B604056,1 [End]        
lnAlias  = SELECT()
lcCurrTag= ORDER(lcOprDet)
SET ORDER TO TAG LOT IN (lcOprDet)

*B602482,1 Keep track of allocated orders while actualization
STORE .F. TO llTktAllo,llRelAll
*B602482,1 (End)

*B602482,1 Keep track of allocated orders while actualization
IF lcTranType <> 'T'
  SELECT (lcTranFile)
  *C200080,1 AMM Consider the dye order case
  *=SEEK(IIF(lcTranType='I','P','')+laData[1])
  *DO WHILE EVAL(lcFileKey) = IIF(lcTranType='I','P','')+laData[1] AND !llTktAllo
    *SUM REST WHILE EVAL(lcFileKey) = IIF(lcTranType='I','P','')+laData[1] ;
    Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO ;
    lnOrd1,lnOrd2,lnOrd3,lnOrd4,lnOrd5,lnOrd6,lnOrd7,lnOrd8,lnQty1,;
    lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8 

  *khm1
  *=SEEK(IIF(lcTranType='I',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1])
  *DO WHILE EVAL(lcFileKey) = IIF(lcTranType='I',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1] AND !llTktAllo
    SUM REST WHILE EVAL(lcFileKey) = IIF(lcTranType='I',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1] ;
    Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO ;
    lnOrd1,lnOrd2,lnOrd3,lnOrd4,lnOrd5,lnOrd6,lnOrd7,lnOrd8,lnQty1,;
    lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8 

  =SEEK(IIF(lcTranType $ 'ID',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1])
  DO WHILE EVAL(lcFileKey) = IIF(lcTranType $ 'ID',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1] AND !llTktAllo
    SUM REST WHILE EVAL(lcFileKey) = IIF(lcTranType $ 'ID',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1] ;
    Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO ;
    lnOrd1,lnOrd2,lnOrd3,lnOrd4,lnOrd5,lnOrd6,lnOrd7,lnOrd8,lnQty1,;
    lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8 
  *khm1
  
  *C200080,1 AMM end
    SELECT (lcOprDet)
    =SEEK(lcTranType+laData[1]+lcFirstOpr+IIF(rbActual=1,laActLot[puActLot],''))
    DO WHILE !llTktAllo AND ;
             cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
             lcTranType+laData[1]+lcFirstOpr+IIF(rbActual=1,laActLot[puActLot],'')
      lcItem  = Item
      lcLotNo = cLotNo
      STORE 0 TO lnOpen1,lnOpen2,lnOpen3,lnOpen4,lnOpen5,lnOpen6,lnOpen7,lnOpen8
      *B603243,1 AMM make the operation done by style/color/dyelot
      *SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
                      lcTranType+laData[1]+lcFirstOpr+lcLotNo+lcItem FOR nActQty=0
      *B603718,1 SSH  [Start] actualize cuttkt
      *B603718,1 SSH          Allow multiple recieving with actualize.
      *SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
                      lcTranType+laData[1]+lcFirstOpr+lcLotNo+lcItem FOR nActQty=0 .AND. cDyelot = &lcOprDet..cDyelot
      SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
                      lcTranType+laData[1]+lcFirstOpr+lcLotNo+lcItem FOR cDyelot = &lcOprDet..cDyelot

      *B603718,1 SSH  [End] actualize cuttkt
      *B603718,1 SSH          Allow multiple recieving with actualize.
      *B603243,1 AMM  end
        FOR lnCount = 1 TO 8
          lcCount = STR(lnCount,1)
          lnOpen&lcCount = lnOpen&lcCount+IIF(TranCd='1',nLotQty&lcCount,-nLotQty&lcCount)
        ENDFOR
      ENDSCAN
      IF (lnOrd1 > lnQty1-lnOpen1 OR lnOrd2 > lnQty2-lnOpen2 OR lnOrd3 > lnQty3-lnOpen3 OR ;
          lnOrd4 > lnQty4-lnOpen4 OR lnOrd5 > lnQty5-lnOpen5 OR lnOrd6 > lnQty6-lnOpen6 OR ;
          lnOrd7 > lnQty7-lnOpen7 OR lnOrd8 > lnQty8-lnOpen8)
        llTktAllo = .T.
      ENDIF
      SELECT (lcOprDet)
    ENDDO
    SELECT (lcTranFile)
  ENDDO
  *B602482,1 Message : 38168
  *B602482,1 Cutting ticket# 999999 has pieces allocated from orders.
  *B602482,1 Proceed with acualization and modify the allocated 
  *B602482,1 quantity from order lines to keep track of this 
  *B602482,1 allocation, or cancel
  *B602482,1 Button : 38022
  *B602482,1 < Proceed> <Cancel>
  IF llTktAllo AND gfModalGen('QRM38168B38002','ALERT',;
     IIF(lcTranType='M','Cutting Ticket#: ','PO#:')+laData[1]) = 2
    SELECT (lnAlias)
    SET ORDER TO TAG lcCurrTag IN (lcOprDet)
    RETURN
  ENDIF
ENDIF
*B602482,1 Keep track of allocated orders while actualization

lnUpdYield = 0

IF SEEK(lcTranType+laData[1]+lcFirstOpr+IIF(rbActual=1,laActLot[puActLot],''),lcOprDet)
  SELECT (lcOprDet)
  DO WHILE !EOF() .AND. cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
           lcTranType+laData[1]+lcFirstOpr+IIF(rbActual=1,laActLot[puActLot],'')
    lcItem     = Item
    lcColor    = Color
    llInHouse  = lInHouse
    lcContCode = cContCode
    lcContName = cContName
    lcLotNo    = cLotNo
    *B603243,1 AMM Get the dyelot field
    lcItDye    = cDyelot
    *B603243,1 AMM  end
    STORE 0 TO lnOpen1,lnOpen2,lnOpen3,lnOpen4,lnOpen5,lnOpen6,lnOpen7,lnOpen8,lnOpen,lnRecv
    STORE 0 TO lnRecv1,lnRecv2,lnRecv3,lnRecv4,lnRecv5,lnRecv6,lnRecv7,lnRecv8

    *B603718,1 SSH  [Start] actualize cuttkt
    *B603718,1 SSH          Allow multiple recieving with actualize.
    *llActualize = (nActQty=0)
    llActualize = .T.
    *B603718,1 SSH   [End]
    *B802956,1 AMM Get next dyelot in the file
    lcNxtDye = lcItDye
    IF llUseDyelot
      SKIP 1
      *--B603867,1 RAMY [start]
      *IF cDyelot # lcItDye 
      IF cDyelot # lcItDye .AND. TranCd = '1'
      *--B603867,1 RAMY [end]
        lcNxtDye = cDyelot
      ENDIF
      SKIP -1
    ENDIF
    *B802956,1 AMM end
    *B603243,1 AMM make the operation done by style/color/dyelot
    *SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
                    lcTranType+laData[1]+lcFirstOpr+lcLotNo+lcItem+lcColor ;
              FOR   llActualize      
    SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
                    lcTranType+laData[1]+lcFirstOpr+lcLotNo+lcItem+lcColor ;
              FOR   llActualize .AND. cDyelot = lcItDye
    *B603243,1 AMM  end
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        lnOpen&lcCount=lnOpen&lcCount+IIF(TranCd='1',nLotQty&lcCount,-nLotQty&lcCount)
        lnRecv&lcCount=lnRecv&lcCount+IIF(TranCd='2',nLotQty&lcCount,0)
      ENDFOR
      *E301235,4 WAB - in case of not M.MFG get sisez 
      *E301235,4 WAB - START
      *IF lcTRanType='T'
      IF lcTRanType<>'T'
      *E301235,4 WAB - END
        lnOpen = MAX(lnOpen1,0)+MAX(lnOpen2,0)+MAX(lnOpen3,0)+MAX(lnOpen4,0)+;
                 MAX(lnOpen5,0)+MAX(lnOpen6,0)+MAX(lnOpen7,0)+MAX(lnOpen8,0)
      ELSE
        lnOpen = lnOpen + IIF(TranCd='1',nLotTotQty,-nLotTotQty)
      ENDIF 
      lnRecv = lnRecv  + IIF(TranCd='2',nLotTotQty,0)
    ENDSCAN
    *B602380,1 continue with actualization if total quantity actualized
    *IF !llActualize OR lnOpen=0
    IF !llActualize
    *B602380,1 (End)
      LOOP
    ENDIF
    *B603243,1 AMM Add the dyelot field
    *lcKey=cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd
    *B604056,1 HBG 12/06/2000 Fix bug variabel 'lcKey' not found [Begin]
    *lcKey=cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd+cDyelot
    lcKeyVal=cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd+cDyelot
    *B604056,1 [End]
    *B603243,1 AMM  end
    SELECT MFGOPRDT
    =SEEK(lcTranType+laData[1]+lcFirstOpr+lcLotNo+'1')
    *B603243,1 AMM make the operation done by style/color/dyelot
    *LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd = ;
                      lcTranType+laData[1]+lcFirstOpr+lcLotNo+'1' ;
                FOR   Item+Color= lcItem+lcColor
    LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd = ;
                      lcTranType+laData[1]+lcFirstOpr+lcLotNo+'1' ;
                FOR   Item+Color+cDyelot = lcItem+lcColor+lcItDye
    *B603243,1 AMM  end
    SELECT (lcTmpTkt)
    
    =SEEK(lcItem+lcColor)
    *B603243,1 AMM make the operation done by style/color/dyelot
    *COUNT REST WHILE Item+Color = lcItem+lcColor TO lnRecords
    COUNT REST WHILE Item+Color = lcItem+lcColor FOR cDyelot = lcItDye TO lnRecords
    *B603243,1 AMM  end
    IF lnRecords=1 
      =SEEK(lcItem+lcColor)
      *B603243,1 AMM make the operation done by style/color/dyelot
      IF llUseDyelot
        LOCATE REST WHILE Item+Color = lcItem+lcColor FOR cDyelot = lcItDye 
      ENDIF
      *B603243,1 AMM  end
       
      *B802542,1 - WAB - MAx(lnOpen?) can not allowed here cause in case receiving is grater than
      *B802542,1 - WAB - Budget lnopen is negative and we must calculate  [nqty? - (- lnopen?) ]
      *B802542,1 - WAB - START
      *REPLACE nQty1   WITH nQty1   - MAX(lnOpen1,0) ,;
              nQty2   WITH nQty2   - MAX(lnOpen2,0) ,;
              nQty3   WITH nQty3   - MAX(lnOpen3,0) ,;
              nQty4   WITH nQty4   - MAX(lnOpen4,0) ,;
              nQty5   WITH nQty5   - MAX(lnOpen5,0) ,;
              nQty6   WITH nQty6   - MAX(lnOpen6,0) ,;
              nQty7   WITH nQty7   - MAX(lnOpen7,0) ,;
              nQty8   WITH nQty8   - MAX(lnOpen8,0) ,;
              nTotQty WITH nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8

      *REPLACE nQty1   WITH nQty1   - lnOpen1 ,;
              nQty2   WITH nQty2   - lnOpen2 ,;
              nQty3   WITH nQty3   - lnOpen3 ,;
              nQty4   WITH nQty4   - lnOpen4 ,;
              nQty5   WITH nQty5   - lnOpen5 ,;
              nQty6   WITH nQty6   - lnOpen6 ,;
              nQty7   WITH nQty7   - lnOpen7 ,;
              nQty8   WITH nQty8   - lnOpen8 ,;
              nTotQty WITH nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8

      *B605224,1 KHM 12/09/2001 (Begin) Changing the replace command
      *REPLACE nQty1   WITH lnRecv1 ,;
              nQty2   WITH lnRecv2 ,;
              nQty3   WITH lnRecv3 ,;
              nQty4   WITH lnRecv4 ,;
              nQty5   WITH lnRecv5 ,;
              nQty6   WITH lnRecv6 ,;
              nQty7   WITH lnRecv7 ,;
              nQty8   WITH lnRecv8 ,;
              nTotQty WITH nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8

      REPLACE nQty1   WITH nQty1   - lnOpen1 ,;
              nQty2   WITH nQty2   - lnOpen2 ,;
              nQty3   WITH nQty3   - lnOpen3 ,;
              nQty4   WITH nQty4   - lnOpen4 ,;
              nQty5   WITH nQty5   - lnOpen5 ,;
              nQty6   WITH nQty6   - lnOpen6 ,;
              nQty7   WITH nQty7   - lnOpen7 ,;
              nQty8   WITH nQty8   - lnOpen8 ,;
              nTotQty WITH nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8
      *B605224,1 KHM 12/09/2001 (End)
      
      *B802542,1 - WAB - START
 
    ELSE
      *B603243,1 AMM  Add dyelot
      *SELECT *,00000 AS nCan1,00000 AS nCan2,00000 AS nCan3,00000 AS nCan4,;
               00000 AS nCan5,00000 AS nCan6,00000 AS nCan7,00000 AS nCan8,;
               000000 AS nTotCan, RECNO() AS nBudRecNo FROM (lcTmpTkt) ;
               WHERE ITEM+COLOR=lcItem+lcColor;
               INTO DBF (gcWorkDir+lcisslogfile)
      SELECT *,00000 AS nCan1,00000 AS nCan2,00000 AS nCan3,00000 AS nCan4,;
               00000 AS nCan5,00000 AS nCan6,00000 AS nCan7,00000 AS nCan8,;
               000000 AS nTotCan, RECNO() AS nBudRecNo FROM (lcTmpTkt) ;
               WHERE ITEM+COLOR+cDyelot = lcItem+lcColor+lcItDye ;
               INTO DBF (gcWorkDir+lcisslogfile)
      *B603243,1 AMM  end
      STORE MAX(lnOpen1,0) TO lnCan1,lnBal1
      STORE MAX(lnOpen2,0) TO lnCan2,lnBal2
      STORE MAX(lnOpen3,0) TO lnCan3,lnBal3
      STORE MAX(lnOpen4,0) TO lnCan4,lnBal4
      STORE MAX(lnOpen5,0) TO lnCan5,lnBal5
      STORE MAX(lnOpen6,0) TO lnCan6,lnBal6
      STORE MAX(lnOpen7,0) TO lnCan7,lnBal7
      STORE MAX(lnOpen8,0) TO lnCan8,lnBal8
      STORE lnCan1+lnCan2+lnCan3+lnCan4+lnCan5+lnCan6+lnCan7+lnCan8 TO lnTotCan,lnTotBal
      SCATTER MEMVAR
      =SEEK(lcItem,'Style') .AND. SEEK('S'+Style.Scale,'Scale')

      *B802713,1 Fix syntax error in the cancel quantity contribution screen.
      *lcConCnTit = ALLTRIM(lcItem)+ALLTRIM(lcColor)+'Cancelled Quantity'
      lcConCnTit = IIF(lcTranType='T','Material/Color',lcItmHdr)+' '+;
                   ALLTRIM(lcItem)+ALLTRIM(lcColor)+'Cancelled Quantity'
      *B802713,1 (End)
      DO (gcScrDir+"MFCONCN.SPX")
      USE IN (lcisslogfile)
    ENDIF
    SELECT (lcTmpTkt)
    =SEEK(lcItem+lcColor)

    *B802713,1 Fix computing operation canceled quantity while actualization 
    STORE 0 TO lnOpen1,lnOpen2,lnOpen3,lnOpen4,lnOpen5,lnOpen6,lnOpen7,lnOpen8
    *B802713,1 (End)
    *B802956,1 AMM Add the dyelot 
    *SCAN REST WHILE Item+Color+STR(LineNo,6) = lcItem+lcColor
    SCAN REST WHILE Item+Color+STR(LineNo,6) = lcItem+lcColor  FOR cDyelot = lcItDye 
    *B802956,1 AMM end
      DO CASE
        CASE lcTranType='M'
          SET ORDER TO TAG CUTLIN IN CUTTKTL
          =SEEK(laData[1]+Item+STR(lineno,6)+'1','CUTTKTL')
          *B802713,1 Fix computing operation canceled quantity while actualization 
          *lnOpen1 = CUTTKTL.Qty1-nQty1
          *lnOpen2 = CUTTKTL.Qty2-nQty2
          *lnOpen3 = CUTTKTL.Qty3-nQty3
          *lnOpen4 = CUTTKTL.Qty4-nQty4
          *lnOpen5 = CUTTKTL.Qty5-nQty5
          *lnOpen6 = CUTTKTL.Qty6-nQty6
          *lnOpen7 = CUTTKTL.Qty7-nQty7
          *lnOpen8 = CUTTKTL.Qty8-nQty8
          lnOpen1 = lnOpen1 + CUTTKTL.Qty1-nQty1
          lnOpen2 = lnOpen2 + CUTTKTL.Qty2-nQty2
          lnOpen3 = lnOpen3 + CUTTKTL.Qty3-nQty3
          lnOpen4 = lnOpen4 + CUTTKTL.Qty4-nQty4
          lnOpen5 = lnOpen5 + CUTTKTL.Qty5-nQty5
          lnOpen6 = lnOpen6 + CUTTKTL.Qty6-nQty6
          lnOpen7 = lnOpen7 + CUTTKTL.Qty7-nQty7
          lnOpen8 = lnOpen8 + CUTTKTL.Qty8-nQty8
          *B802713,1 (End)
          lnOpen  = lnOpen1+lnOpen2+lnOpen3+lnOpen4+lnOpen5+lnOpen6+lnOpen7+lnOpen8

          *B602482,1 Keep track of allocated orders while actualization
          SELECT CUTTKTL
          SCATTER FIELDS ORD1,ORD2,ORD3,ORD4,ORD5,ORD6,ORD7,ORD8,TotOrd TO laOrdQty
          SELECT (lcTmpTkt)
          IF (CUTTKTL.Ord1 > nQty1 OR CUTTKTL.Ord2 > nQty2 OR CUTTKTL.Ord3 > nQty3 OR ;
              CUTTKTL.Ord4 > nQty4 OR CUTTKTL.Ord5 > nQty5 OR CUTTKTL.Ord6 > nQty6 OR ;
              CUTTKTL.Ord7 > nQty7 OR CUTTKTL.Ord8 > nQty8)
            
            *E301182,1 Update CT/PO line number in the CUTPICK file.
            *=lfTktAllo(lcItem)
            =lfTktAllo(lcItem,CutTKtL.Dyelot,CutTKtL.LineNo)
            *E301182,1 (End)
          ENDIF
          SELECT CUTTKTH
          =RLOCK()
          REPLACE TotOrd WITH TotOrd - CutTktl.TotOrd + laOrdQty[9]
          UNLOCK
          *B602482,1 (End)

          *B602203,1 Update Style WIP
          SELECT STYLE
          =SEEK(CUTTKTL.Style)
          =RLOCK()
          
          *B606340,1 AMH Update wip with positive values only by deducting the max 
          *B606340,1 between qty and 0 [Start]
          *REPLACE WIP1   WITH WIP1 - (CUTTKTL.Qty1-&lcTmpTkt..nQty1) ,;
                  WIP2   WITH WIP2 - (CUTTKTL.Qty2-&lcTmpTkt..nQty2) ,;
                  WIP3   WITH WIP3 - (CUTTKTL.Qty3-&lcTmpTkt..nQty3) ,;
                  WIP4   WITH WIP4 - (CUTTKTL.Qty4-&lcTmpTkt..nQty4) ,;
                  WIP5   WITH WIP5 - (CUTTKTL.Qty5-&lcTmpTkt..nQty5) ,;
                  WIP6   WITH WIP6 - (CUTTKTL.Qty6-&lcTmpTkt..nQty6) ,;
                  WIP7   WITH WIP7 - (CUTTKTL.Qty7-&lcTmpTkt..nQty7) ,;
                  WIP8   WITH WIP8 - (CUTTKTL.Qty8-&lcTmpTkt..nQty8) ,;
                  TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
          REPLACE WIP1   WITH WIP1 - (CUTTKTL.Qty1-MAX(&lcTmpTkt..nQty1,0)) ,;
                  WIP2   WITH WIP2 - (CUTTKTL.Qty2-MAX(&lcTmpTkt..nQty2,0)) ,;
                  WIP3   WITH WIP3 - (CUTTKTL.Qty3-MAX(&lcTmpTkt..nQty3,0)) ,;
                  WIP4   WITH WIP4 - (CUTTKTL.Qty4-MAX(&lcTmpTkt..nQty4,0)) ,;
                  WIP5   WITH WIP5 - (CUTTKTL.Qty5-MAX(&lcTmpTkt..nQty5,0)) ,;
                  WIP6   WITH WIP6 - (CUTTKTL.Qty6-MAX(&lcTmpTkt..nQty6,0)) ,;
                  WIP7   WITH WIP7 - (CUTTKTL.Qty7-MAX(&lcTmpTkt..nQty7,0)) ,;
                  WIP8   WITH WIP8 - (CUTTKTL.Qty8-MAX(&lcTmpTkt..nQty8,0)) ,;
                  TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
          *B606340,1 AMH [End]
          
          UNLOCK
          SELECT STYDYE
          =SEEK(CUTTKTL.Style+CutTktl.cWareCode+SPACE(10))
          =RLOCK()
          
          *B606340,1 AMH Update wip with positive values only by deducting the max 
          *B606340,1 between qty and 0 [Start]
          *REPLACE WIP1   WITH WIP1 - (CUTTKTL.Qty1-&lcTmpTkt..nQty1) ,;
                  WIP2   WITH WIP2 - (CUTTKTL.Qty2-&lcTmpTkt..nQty2) ,;
                  WIP3   WITH WIP3 - (CUTTKTL.Qty3-&lcTmpTkt..nQty3) ,;
                  WIP4   WITH WIP4 - (CUTTKTL.Qty4-&lcTmpTkt..nQty4) ,;
                  WIP5   WITH WIP5 - (CUTTKTL.Qty5-&lcTmpTkt..nQty5) ,;
                  WIP6   WITH WIP6 - (CUTTKTL.Qty6-&lcTmpTkt..nQty6) ,;
                  WIP7   WITH WIP7 - (CUTTKTL.Qty7-&lcTmpTkt..nQty7) ,;
                  WIP8   WITH WIP8 - (CUTTKTL.Qty8-&lcTmpTkt..nQty8) ,;
                  TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
          REPLACE WIP1   WITH WIP1 - (CUTTKTL.Qty1-MAX(&lcTmpTkt..nQty1,0)) ,;
                  WIP2   WITH WIP2 - (CUTTKTL.Qty2-MAX(&lcTmpTkt..nQty2,0)) ,;
                  WIP3   WITH WIP3 - (CUTTKTL.Qty3-MAX(&lcTmpTkt..nQty3,0)) ,;
                  WIP4   WITH WIP4 - (CUTTKTL.Qty4-MAX(&lcTmpTkt..nQty4,0)) ,;
                  WIP5   WITH WIP5 - (CUTTKTL.Qty5-MAX(&lcTmpTkt..nQty5,0)) ,;
                  WIP6   WITH WIP6 - (CUTTKTL.Qty6-MAX(&lcTmpTkt..nQty6,0)) ,;
                  WIP7   WITH WIP7 - (CUTTKTL.Qty7-MAX(&lcTmpTkt..nQty7,0)) ,;
                  WIP8   WITH WIP8 - (CUTTKTL.Qty8-MAX(&lcTmpTkt..nQty8,0)) ,;
                  TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
          *B606340,1 AMH [End]
          
          UNLOCK
          *B602203,1 (End)
          SELECT CUTTKTL

          lnTotQty = Totqty
          =RLOCK()
          
          *B606340,1 AMH Update wip with positive values only by deducting the max 
          *B606340,1 between qty and 0 [Start]
          *REPLACE Qty1   WITH &lcTmpTkt..nQty1 ,;
                  Qty2   WITH &lcTmpTkt..nQty2 ,;
                  Qty3   WITH &lcTmpTkt..nQty3 ,;
                  Qty4   WITH &lcTmpTkt..nQty4 ,;
                  Qty5   WITH &lcTmpTkt..nQty5 ,;
                  Qty6   WITH &lcTmpTkt..nQty6 ,;
                  Qty7   WITH &lcTmpTkt..nQty7 ,;
                  Qty8   WITH &lcTmpTkt..nQty8 ,;
                  TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
          REPLACE Qty1   WITH MAX(&lcTmpTkt..nQty1,0) ,;
                  Qty2   WITH MAX(&lcTmpTkt..nQty2,0) ,;
                  Qty3   WITH MAX(&lcTmpTkt..nQty3,0) ,;
                  Qty4   WITH MAX(&lcTmpTkt..nQty4,0) ,;
                  Qty5   WITH MAX(&lcTmpTkt..nQty5,0) ,;
                  Qty6   WITH MAX(&lcTmpTkt..nQty6,0) ,;
                  Qty7   WITH MAX(&lcTmpTkt..nQty7,0) ,;
                  Qty8   WITH MAX(&lcTmpTkt..nQty8,0) ,;
                  TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
          *B606340,1 AMH [End]
          
          *B602482,1 Keep track of allocated orders while actualization
          REPLACE Ord1   WITH laOrdQty[1] ,;
                  Ord2   WITH laOrdQty[2] ,;
                  Ord3   WITH laOrdQty[3] ,;
                  Ord4   WITH laOrdQty[4] ,;
                  Ord5   WITH laOrdQty[5] ,;
                  Ord6   WITH laOrdQty[6] ,;
                  Ord7   WITH laOrdQty[7] ,;
                  Ord8   WITH laOrdQty[8] ,;
                  TotOrd WITH laOrdQty[9]
          *B602482,1 (End)
          UNLOCK
          *B603718,1 SSH  [Start] actualize cuttkt
          *B603718,1 SSH          Allow multiple recieving with actualize.

          *B804081,1 KHM 04/04/2001 (Begin) No changes in the budget qty
          *B804081,1                in case of actualizination qty is less 
          *B804081,1                than the budget.
          *laData[6]  = CutTktH.Pcs_Bud + ( CutTktl.TotQty - lnTotQty )
          laData[6]  = MAX(CutTktH.Pcs_Bud,CutTktH.Pcs_Bud+(CutTktl.TotQty-lnTotQty ))
          *B804081,1 KHM 04/04/2001 (End)
          
          laData[10] = MAX(ladata[6] - (lnLotRcv+ladata[8]+ladata[9]),0)
          SELECT CutTktH
          REPLACE Pcs_Bud WITH laData[6],;
                  PCS_OPN WITH laData[10]
          *B603718,1 SSH 24/07/2000[End]
          SELECT CUTTKTL
          *B603718,1 SSH  [End]
          laData[8]  = laData[8]  + MAX(lnTotQty-CutTktL.TotQty,0)
          laData[12] = laData[12] + (CutTktL.TotQty-lnTotQty)*CutTktL.nCost1
          laData[13] = laData[13] + (CutTktL.TotQty-lnTotQty)*CutTktL.nCost2
          laData[14] = laData[14] + (CutTktL.TotQty-lnTotQty)*CutTktL.nCost3
          laData[15] = laData[15] + (CutTktL.TotQty-lnTotQty)*CutTktL.nCost4
          laData[16] = laData[16] + (CutTktL.TotQty-lnTotQty)*CutTktL.nCost5
        *khm1
        *CASE lcTranType='I'
        CASE lcTranType $ 'ID'
        *khm1
        
          SET ORDER TO TAG POSLN IN POSLN
          *C200080,1 AMM Consider the dye order case
          *=SEEK('P'+laData[1]+lcItem+STR(lineno,6)+'1','POSLN')
          =SEEK(IIF(EMPTY(lcDyePO),'P','D')+laData[1]+lcItem+STR(lineno,6)+'1','POSLN')
          *C200080,1 AMM end
          *B802713,1 Fix computing operation canceled quantity while actualization 
          *lnOpen1 = POSLN.Qty1-nQty1
          *lnOpen2 = POSLN.Qty2-nQty2
          *lnOpen3 = POSLN.Qty3-nQty3
          *lnOpen4 = POSLN.Qty4-nQty4
          *lnOpen5 = POSLN.Qty5-nQty5
          *lnOpen6 = POSLN.Qty6-nQty6
          *lnOpen7 = POSLN.Qty7-nQty7
          *lnOpen8 = POSLN.Qty8-nQty8
          lnOpen1 = lnOpen1 + POSLN.Qty1-nQty1
          lnOpen2 = lnOpen2 + POSLN.Qty2-nQty2
          lnOpen3 = lnOpen3 + POSLN.Qty3-nQty3
          lnOpen4 = lnOpen4 + POSLN.Qty4-nQty4
          lnOpen5 = lnOpen5 + POSLN.Qty5-nQty5
          lnOpen6 = lnOpen6 + POSLN.Qty6-nQty6
          lnOpen7 = lnOpen7 + POSLN.Qty7-nQty7
          lnOpen8 = lnOpen8 + POSLN.Qty8-nQty8
          *B802713,1 (End)
          lnOpen  = lnOpen1+lnOpen2+lnOpen3+lnOpen4+lnOpen5+lnOpen6+lnOpen7+lnOpen8

          *B602482,1 Keep track of allocated orders while actualization
          SELECT POSLN
          SCATTER FIELDS ORD1,ORD2,ORD3,ORD4,ORD5,ORD6,ORD7,ORD8,TotOrd TO laOrdQty
          SELECT (lcTmpTkt)
          IF (POSLN.Ord1 > nQty1 OR POSLN.Ord2 > nQty2 OR POSLN.Ord3 > nQty3 OR ;
              POSLN.Ord4 > nQty4 OR POSLN.Ord5 > nQty5 OR POSLN.Ord6 > nQty6 OR ;
              POSLN.Ord7 > nQty7 OR POSLN.Ord8 > nQty8)
            *E301182,1 Update CT/PO line number in the CUTPICK file.
            *=lfTktAllo(lcItem)
            =lfTktAllo(lcItem,POSLN.Dyelot,POSLN.LineNo)
            *E301182,1 (End)
          ENDIF
          SELECT POSHDR
          =RLOCK()
          REPLACE TotOrd WITH TotOrd - POSLN.TotOrd + laOrdQty[9]
          UNLOCK
          *B602482,1 (End)

          *B602203,1 Update Style WIP
          SELECT STYLE
          =SEEK(POSLN.Style)
          =RLOCK()
          REPLACE WIP1   WITH WIP1 - (POSLN.Qty1-&lcTmpTkt..nQty1) ,;
                  WIP2   WITH WIP2 - (POSLN.Qty2-&lcTmpTkt..nQty2) ,;
                  WIP3   WITH WIP3 - (POSLN.Qty3-&lcTmpTkt..nQty3) ,;
                  WIP4   WITH WIP4 - (POSLN.Qty4-&lcTmpTkt..nQty4) ,;
                  WIP5   WITH WIP5 - (POSLN.Qty5-&lcTmpTkt..nQty5) ,;
                  WIP6   WITH WIP6 - (POSLN.Qty6-&lcTmpTkt..nQty6) ,;
                  WIP7   WITH WIP7 - (POSLN.Qty7-&lcTmpTkt..nQty7) ,;
                  WIP8   WITH WIP8 - (POSLN.Qty8-&lcTmpTkt..nQty8) ,;
                  TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
          UNLOCK
          SELECT STYDYE
          =SEEK(POSLN.Style+POSLN.cWareCode+SPACE(10))
          =RLOCK()
          REPLACE WIP1   WITH WIP1 - (POSLN.Qty1-&lcTmpTkt..nQty1) ,;
                  WIP2   WITH WIP2 - (POSLN.Qty2-&lcTmpTkt..nQty2) ,;
                  WIP3   WITH WIP3 - (POSLN.Qty3-&lcTmpTkt..nQty3) ,;
                  WIP4   WITH WIP4 - (POSLN.Qty4-&lcTmpTkt..nQty4) ,;
                  WIP5   WITH WIP5 - (POSLN.Qty5-&lcTmpTkt..nQty5) ,;
                  WIP6   WITH WIP6 - (POSLN.Qty6-&lcTmpTkt..nQty6) ,;
                  WIP7   WITH WIP7 - (POSLN.Qty7-&lcTmpTkt..nQty7) ,;
                  WIP8   WITH WIP8 - (POSLN.Qty8-&lcTmpTkt..nQty8) ,;
                  TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
          UNLOCK
          SELECT POSLN
          lnTotQty = Totqty
          =RLOCK()
          REPLACE Qty1   WITH &lcTmpTkt..nQty1 ,;
                  Qty2   WITH &lcTmpTkt..nQty2 ,;
                  Qty3   WITH &lcTmpTkt..nQty3 ,;
                  Qty4   WITH &lcTmpTkt..nQty4 ,;
                  Qty5   WITH &lcTmpTkt..nQty5 ,;
                  Qty6   WITH &lcTmpTkt..nQty6 ,;
                  Qty7   WITH &lcTmpTkt..nQty7 ,;
                  Qty8   WITH &lcTmpTkt..nQty8 ,;
                  TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
          *B602482,1 Keep track of allocated orders while actualization
          REPLACE Ord1   WITH laOrdQty[1] ,;
                  Ord2   WITH laOrdQty[2] ,;
                  Ord3   WITH laOrdQty[3] ,;
                  Ord4   WITH laOrdQty[4] ,;
                  Ord5   WITH laOrdQty[5] ,;
                  Ord6   WITH laOrdQty[6] ,;
                  Ord7   WITH laOrdQty[7] ,;
                  Ord8   WITH laOrdQty[8] ,;
                  TotOrd WITH laOrdQty[9]
          *B602482,1 (End)
          UNLOCK
          SELECT POSHDR
          =RLOCK()
          REPLACE nFCost1 WITH nFCost1 + (POSLN.TotQty-lnTotQty)*POSLN.nCost1 ,;
                  nFCost2 WITH nFCost2 + (POSLN.TotQty-lnTotQty)*POSLN.nCost2 ,;
                  nFCost3 WITH nFCost3 + (POSLN.TotQty-lnTotQty)*POSLN.nCost3 ,;
                  nFCost4 WITH nFCost4 + (POSLN.TotQty-lnTotQty)*POSLN.nCost4 ,;
                  nFCost5 WITH nFCost5 + (POSLN.TotQty-lnTotQty)*POSLN.nCost5
          UNLOCK
          laData[8]  = laData[8]  + MAX(lnTotQty-POSLN.TotQty,0)
          laData[12] = laData[12] + (POSLN.TotQty-lnTotQty)*POSLN.nECost1
          laData[13] = laData[13] + (POSLN.TotQty-lnTotQty)*POSLN.nECost2
          laData[14] = laData[14] + (POSLN.TotQty-lnTotQty)*POSLN.nECost3
          laData[15] = laData[15] + (POSLN.TotQty-lnTotQty)*POSLN.nECost4
          laData[16] = laData[16] + (POSLN.TotQty-lnTotQty)*POSLN.nECost5
        CASE lcTranType='T'
          SELECT MMFGORDD
          SET ORDER TO TAG MMFGORDD
          =SEEK(laData[1]+SUBSTR(lcItem,1,7)+lcColor,'MMFGORDD')
          LOCATE REST WHILE cMfgOrdNo+cFabric+Color+Dyelot+TranCd=;
                            laData[1]+SUBSTR(lcItem,1,7)+lcColor FOR TranCd = '1'
          *B602203,1 Update Fabric WIP
          SELECT FABRIC
          =SEEK(MMFGORDD.cFabric+MMFGORDD.Color)
          =RLOCK()
          REPLACE OnOrder WITH OnOrder - lnOpen
          UNLOCK
          SELECT FABDYE
          =SEEK(MMFGORDD.cFabric+MMFGORDD.Color+MMFGORDD.cWareCode+SPACE(10))
          =RLOCK()
          REPLACE OnOrder WITH OnOrder - lnOpen
          UNLOCK
          *B602203,1 (End()
          SELECT MMFGORDD
          lnTotQty = nMfgTotQty
          =RLOCK()
          REPLACE nMfgTotQty WITH MAX(nMfgTotQty - lnOpen,0)
          UNLOCK
          laData[8]  = laData[8]  + MAX(lnOpen,0)
          laData[12] = laData[12] + (MMFGORDD.nMfgTotQty-lnTotQty)*MMFGORDD.nCost1
          laData[13] = laData[13] + (MMFGORDD.nMfgTotQty-lnTotQty)*MMFGORDD.nCost2
          laData[14] = laData[14] + (MMFGORDD.nMfgTotQty-lnTotQty)*MMFGORDD.nCost3
          laData[15] = laData[15] + (MMFGORDD.nMfgTotQty-lnTotQty)*MMFGORDD.nCost4
      ENDCASE
      *B802713,1 Fix computing Actual & open quantity while actualization 
      *laData[27] = laData[27] + MAX(lnRecv-MFGOPRDT.nActQty,0)
      **B802242,1 Update balance while actualization
      *laData[10] = laData[27]
      **B802242,1 (End)
      *B802713,1 (End)
      
      *B602234,1 Update Cost Items Required Quantities upon actualization
      *B602234,1 Message : 38157
      *B602234,1 Modify the cost sheet basaed on the actual quantity?
      *B602234,1 Button : 38006
      *B602234,1 < Yes >  < No >
      
      lnUpdYield = IIF(lnUpdYield=0 AND lnOpen <> 0,;
                       gfModalGen('QRM38157B38006','ALERT'),lnUpdYield)
      IF lnUpdYield = 1

        SELECT BOMLINE
        =SEEK(lcTranType+'1'+laData[1]+STR(&lcTmpTkt..lineno,6))
        SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
                        lcTranType+'1'+laData[1]+STR(&lcTmpTkt..lineno,6)
          STORE 0 TO lnReq1,lnReq2,lnReq3,lnReq4,lnReq5,lnReq6,lnReq7,lnReq8
          IF lcTranType = 'T'
            lnStyQty = lnOpen
          ELSE
            lnStyQty = 0
            FOR lnCount = 1 TO 8
              lcCount = STR(lnCount,1)
              IF lcCount $ cSizes
                lnStyQty   = lnStyQty + lnOpen&lcCount
                lcCompSize = SUBSTR(cCompSizes,AT(lcCount,cSizes),1)
                IF !EMPTY(lcCompSize)
                  lnReq&lcCompSize = lnReq&lcCompSize + lnOpen&lcCount
                ENDIF
              ENDIF
            ENDFOR
          ENDIF
          lnOldQty = ItemQty
          lnOldAmt = ItemAmt
          REPLACE StyQty  WITH StyQty-lnStyQty ,;
                  UnitQty WITH IIF(cCatgtyp='F' AND cOprCode=lcFirstOpr,IIF(StyQty=0,0,ItemQty/StyQty),UnitQty) ,;
                  ItemQty WITH StyQty*UnitQty  ,; 
                  ItemAmt WITH ItemQty*UnitCost
          SELECT (lcDetFile)        
          =SEEK(BomLine.Style+BomLine.SClr+STR(BomLine.LineNo,6)+BomLine.cBomTyp+;
                '1'+BomLine.Item+BomLine.IClr+BomLine.MfgCode)
          REPLACE StyQty  WITH StyQty-lnStyQty ,;
                  UnitQty WITH IIF(cCatgtyp='F' AND cOprCode=lcFirstOpr,IIF(StyQty=0,0,ItemQty/StyQty),UnitQty) ,;
                  ItemQty WITH StyQty*UnitQty  ,; 
                  ItemAmt WITH ItemQty*UnitCost
          =SEEK(BomLine.Style+BomLine.SClr+STR(BomLine.LineNo,6)+BomLine.cBomTyp+'2')
          REPLACE ItemQty WITH ItemQty-lnOldQty+BomLine.ItemQty ,;
                  ItemAmt WITH ItemAmt-lnOldAmt+BomLine.ItemAmt
          lnOldQty = ItemQty
          lnOldAmt = ItemAmt

          SELECT CTKTBOM
          *B802956,1 AMM Add dyelot
          *=SEEK(lcTranType+laData[1]+Bomline.cBomTyp+Bomline.item+Bomline.iclr+Bomline.mfgcode)
          =SEEK(lcTranType+laData[1]+Bomline.cBomTyp+Bomline.item+Bomline.iclr+Bomline.mfgcode+Bomline.Dyelot)
          *B802956,1 AMM end
          REPLACE Pieces   WITH Pieces-lnStyQty ,;
                  UntQty   WITH IIF(cCatgtyp='F' AND cOprCode=lcFirstOpr,IIF(Pieces=0,0,Req_qty/Pieces),UntQty) ,;
                  Req_qty  WITH Pieces*UntQty   ,;
                  Est_Cost WITH Req_qty*UntCost ,;
                  Req_Qty1 WITH Req_Qty1-lnReq1*UntQty ,;
                  Req_Qty2 WITH Req_Qty2-lnReq2*UntQty ,;
                  Req_Qty3 WITH Req_Qty3-lnReq3*UntQty ,;
                  Req_Qty4 WITH Req_Qty4-lnReq4*UntQty ,;
                  Req_Qty5 WITH Req_Qty5-lnReq5*UntQty ,;
                  Req_Qty6 WITH Req_Qty6-lnReq6*UntQty ,;
                  Req_Qty7 WITH Req_Qty7-lnReq7*UntQty ,;
                  Req_Qty8 WITH Req_Qty8-lnReq8*UntQty
          SELECT (lcTktSheet)
          =SEEK(CTKTBOM.TYP+'1'+CTKTBOM.ITEM+CTKTBOM.ICLR+CTKTBOM.MFGCODE+CTKTBOM.DYELOT)

         *B802542,1 - WAB - replace also req_qty? to issue the actual qty
         *B802542,1 - WAB - START
         *REPLACE Pieces   WITH Pieces-lnStyQty ,;
                  UntQty   WITH IIF(cCatgtyp='F' AND cOprCode=lcFirstOpr,IIF(Pieces=0,0,Req_qty/Pieces),UntQty) ,;
                  Req_qty  WITH Pieces*UntQty   ,;
                  Est_Cost WITH Req_qty*UntCost
         REPLACE Pieces   WITH Pieces-lnStyQty ,;
                  UntQty   WITH IIF(cCatgtyp='F' AND cOprCode=lcFirstOpr,IIF(Pieces=0,0,Req_qty/Pieces),UntQty) ,;
                  Req_qty  WITH Pieces*UntQty   		,;
                  Est_Cost WITH Req_qty*UntCost 		,;
         		  Req_Qty1 WITH Req_Qty1-lnReq1*UntQty 	,;
                  Req_Qty2 WITH Req_Qty2-lnReq2*UntQty 	,;
                  Req_Qty3 WITH Req_Qty3-lnReq3*UntQty 	,;
                  Req_Qty4 WITH Req_Qty4-lnReq4*UntQty 	,;
                  Req_Qty5 WITH Req_Qty5-lnReq5*UntQty 	,;
                  Req_Qty6 WITH Req_Qty6-lnReq6*UntQty 	,;
                  Req_Qty7 WITH Req_Qty7-lnReq7*UntQty 	,;
                  Req_Qty8 WITH Req_Qty8-lnReq8*UntQty
                  
         *B802542,1 - WAB - END
         
        ENDSCAN
        GO TOP IN (lcDetFile)
        GO TOP IN (lcTktSheet)
      ENDIF
      *B602234,1 (End)
    ENDSCAN  
    *B802713,1 Fix computing Actual & open quantity while actualization 
    laData[27] = laData[27] + MAX(lnRecv-MFGOPRDT.nActQty,0)
    *B802242,1 Update balance while actualization
    *wab    
    *laData[10] = laData[27]
    laData[10] = MAX(ladata[6] - (ladata[7]+ladata[8]+ladata[9]),0)
    *wab
    *B802242,1 (End)
    *B802713,1 (End)

    SELECT MFGOPRDT
    =RLOCK()
    *B603718,1 SSH  [Start] actualize cuttkt
    *B603718,1 SSH          Allow multiple recieving with actualize.
    *REPLACE nActQty WITH lnRecv
    *B603847,1 SSH  [Start] actualize cuttkt.
    *REPLACE nActQty WITH lnRecv + nActQty
    REPLACE nActQty WITH lnRecv
    *B603847,1 End.
    *B603718,1 SSH   [End]
    UNLOCK
    IF lnOpen > 0
      *B603243,1 AMM Add the dyelot field
      *INSERT INTO MFGOPRDT ;
      (cImTyp,cTktNo,cOprCode,cLotNo,Item,Color,lInHouse,cContCode,cContName,;
       TranCd,dTranDate,nLotQty1,nLotQty2,nLotQty3,nLotQty4,nLotQty5,nLotQty6,;
       nLotQty7,nLotQty8,nLotTotQty) VALUES ;
      (lcTranType,laData[1],lcFirstOpr,lcLotNo,lcItem,lcColor,llInHouse,;
       lcContCode,lcContName,'4',ldActDate,MAX(lnOpen1,0),MAX(lnOpen2,0),;
       MAX(lnOpen3,0),MAX(lnOpen4,0),MAX(lnOpen5,0),MAX(lnOpen6,0),;
       MAX(lnOpen7,0),MAX(lnOpen8,0),lnOpen)
      INSERT INTO MFGOPRDT ;
      (cImTyp,cTktNo,cOprCode,cLotNo,Item,Color,cDyelot,lInHouse,cContCode,cContName,;
       TranCd,dTranDate,nLotQty1,nLotQty2,nLotQty3,nLotQty4,nLotQty5,nLotQty6,;
       nLotQty7,nLotQty8,nLotTotQty) VALUES ;
      (lcTranType,laData[1],lcFirstOpr,lcLotNo,lcItem,lcColor,lcItDye,llInHouse,;
       lcContCode,lcContName,'4',ldActDate,MAX(lnOpen1,0),MAX(lnOpen2,0),;
       MAX(lnOpen3,0),MAX(lnOpen4,0),MAX(lnOpen5,0),MAX(lnOpen6,0),;
       MAX(lnOpen7,0),MAX(lnOpen8,0),lnOpen)
      *B603243,1 AMM end
    ENDIF
    SELECT (lcOprDet)
    *B603243,1 AMM make the operation done by style/color/dyelot
    *=SEEK(lcTranType+laData[1]+lcFirstOpr+lcLotNo+lcItem+lcColor+'1')
    =SEEK(lcTranType+laData[1]+lcFirstOpr+lcLotNo+lcItem+lcColor+'1'+lcItDye)  
    *B603243,1 AMM  end
    *B603718,1 SSH  [Start] actualize cuttkt
    *B603718,1 SSH          Allow multiple recieving with actualize.
    *REPLACE nActQty WITH lnRecv
    *B603847,1 SSH  [Start] actualize cuttkt.
    *REPLACE nActQty WITH lnRecv + nActQty
    REPLACE nActQty WITH lnRecv
    *B603847,1 End.
    *B603718,1 SSH   [End]
    *B802956,1 AMM If there is comind dyelot in the file, go to it, else go to next style/color
    *=SEEK(lcKey)
    IF llUseDyelot .AND. lcNxtDye # lcItDye
      =SEEK(lcTranType+laData[1]+lcFirstOpr+lcLotNo+lcItem+lcColor+'1'+lcNxtDye)
    ELSE
      *B604056,1 HBG 12/06/2000 Fix bug variabel 'lcKey' not found [Begin]
      *=SEEK(lcKey)
      =SEEK(lcKeyVal)
      *B604056,1 [End]
    ENDIF
    *B802956,1 AMM end

  ENDDO
  IF rbActual = 2
    laData[3] = 'A'
    lcStatus = 'Actualized'
  ENDIF  
  SELECT (lcBaseFile)
  =RLOCK()
  GATHER FROM laData FIELDS &lcScFields
  REPLACE Act_Date WITH ldActDate
  UNLOCK
  
  *B605607,1 AMH Trigger for Cathy Daniels to export C/T to the old system [Start]
  IF rbActual = 2 .AND. ASCAN(laEvntTrig , PADR('EXPACTCT',10)) <> 0
    =gfDoTriger('MFCSSH',PADR('EXPACTCT',10))
  ENDIF
  *B605607,1 AMH [End]
  
  SELECT (lcOprDet)
  ZAP
  SELECT MFGOPRDT
  SET ORDER TO TAG MFGOPRDT
  IF SEEK(lcTranType+laData[1])
    SCAN REST WHILE cimtyp+ctktno = lcTranType+laData[1]
      SCATTER MEMVAR
      INSERT INTO (lcOprDet) FROM MEMVAR
    ENDSCAN
  ENDIF
  GO TOP IN (lcOprDet)
  =lfBrowLots()
  SELECT (lnAlias)
ENDIF
SET ORDER TO TAG lcCurrTag IN (lcOprDet)

*!*************************************************************
*! Name      : lfTktAllo 
*! Developer : Wael Aly Mohamed
*! Date      : 02/09/1999
*! Purpose   : Edit ticket line orders allocated quantity while actualization
*!*************************************************************
*! Calls     : gfOpenFile(),lfEditAlo(),gfModalGen()
*!*************************************************************
*! Parameters: lcStyle : ticket style
*!             lcDyelot: ticket dyelot
*!             lnLineNo: ticket line#
*!             llCanRem: Called from cancel ticket remaining quantity
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfTktAllo(lcStyle)
*!*************************************************************
FUNCTION lfTktAllo
*E301182,1 Update CT/PO line number in the CUTPICK file.
*PARAMETERS lcStyle
PARAMETERS lcStyle,lcDyelot,lnLineNo,llCanRem
*E301182,1 (End)

PRIVATE lnAlias
lnAlias = SELECT()

=gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'Cutpkord','SH')
=gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH')
=gfOpenFile(gcDataDir+'ORDLINE',gcDataDir+'ORDLINE','SH')

*E301182,1 Update CT/PO line number in the CUTPICK file.
*SELECT * FROM CUTPICK WHERE TranCd+cTktNo+Style =;
IIF(lcTranType='M','1','2')+laData[1]+lcStyle ;
INTO DBF (gcWorkDir+lcTmpCutPik)



SELECT * FROM CUTPICK WHERE TranCd+cTktNo+cTktLineNo+Order+Style+cOrdLine =;
IIF(lcTranType='M','1','2')+laData[1]+STR(lnLineNo,6) ;
INTO DBF (gcWorkDir+lcTmpCutPik)
*E301182,1 (End)

lnChoice=3
IF !llRelAll
  *B602482,1 Message : 38166
  *B602482,1 Style "XXXXX" has pieces allocated from orders.
  *B602482,1 Edit the allocated quantity from order lines to keep 
  *B602482,1 track of this allocation, release allocation for only 
  *B602482,1 this line, or for all ticket lines.
  *B602482,1 Button : 38022
  *B602482,1 < Edit Allocation > < Release > < Release All >
  IF llCanRem
    lnChoice = gfModalGen("QRM38166B38007","DIALOG",lcItmHdr+'|'+lcStyle+;
               IIF(EMPTY(lcDyelot),'',' '+ALLTRIM(lcDyelot)))
  ELSE
    lnChoice = gfModalGen("QRM38166B38022","DIALOG",lcItmHdr+'|'+lcStyle+;
               IIF(EMPTY(lcDyelot),'',' '+ALLTRIM(lcDyelot)))
  ENDIF
  llRelAll = lnChoice=3
ENDIF
IF lnChoice=4
  RETURN(.F.)
ENDIF
IF lnChoice=1
  =lfEditAlo()
ENDIF
SELECT (lcTmpCutPik)
STORE 0 TO laOrdQty
SCAN
  SCATTER MEMVAR
  IF lnChoice<>1
    STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.totQty
  ENDIF
  *E301182,1 Update CT/PO line number in the CUTPICK file.
  *=SEEK(TranCd+cTktNo+Order+Style+cOrdLine,'CUTPICK')
  =SEEK(TranCd+cTktNo+ctktlineno+Order+Style+cOrdLine,'CUTPICK')
  *E301182,1 (End)

  =SEEK('O'+Order+cOrdLine,'ORDLINE')
  =SEEK('O'+Order,'ORDHDR')  
  SELECT ORDLINE
  =RLOCK()
  REPLACE Cut1   WITH Cut1  - CUTPICK.Qty1  + m.Qty1 ,;
          Cut2   WITH Cut2  - CUTPICK.Qty2  + m.Qty2 ,;
          Cut3   WITH Cut3  - CUTPICK.Qty3  + m.Qty3 ,;
          Cut4   WITH Cut4  - CUTPICK.Qty4  + m.Qty4 ,;
          Cut5   WITH Cut5  - CUTPICK.Qty5  + m.Qty5 ,;
          Cut6   WITH Cut6  - CUTPICK.Qty6  + m.Qty6 ,;
          Cut7   WITH Cut7  - CUTPICK.Qty7  + m.Qty7 ,;
          Cut8   WITH Cut8  - CUTPICK.Qty8  + m.Qty8 ,;
          TotCut WITH TotCut- CUTPICK.TotQty+ m.TotQty
  UNLOCK
  SELECT ORDHDR
  =RLOCK()
  REPLACE TotCut WITH TotCut - CUTPICK.TotQty + m.TotQty
  UNLOCK
  SELECT CUTPICK
  =RLOCK()
  REPLACE Qty1   WITH m.Qty1 ,;
          Qty2   WITH m.Qty2 ,;
          Qty3   WITH m.Qty3 ,;
          Qty4   WITH m.Qty4 ,;
          Qty5   WITH m.Qty5 ,;
          Qty6   WITH m.Qty6 ,;
          Qty7   WITH m.Qty7 ,;
          Qty8   WITH m.Qty8 ,;
          TotQty WITH m.TotQty
  UNLOCK
  laOrdQty[1] = laOrdQty[1] + Qty1
  laOrdQty[2] = laOrdQty[2] + Qty2
  laOrdQty[3] = laOrdQty[3] + Qty3
  laOrdQty[4] = laOrdQty[4] + Qty4
  laOrdQty[5] = laOrdQty[5] + Qty5
  laOrdQty[6] = laOrdQty[6] + Qty6
  laOrdQty[7] = laOrdQty[7] + Qty7
  laOrdQty[8] = laOrdQty[8] + Qty8
  laOrdQty[9] = laOrdQty[9] + TotQty
  IF TotQty = 0
    DELETE
  ENDIF
ENDSCAN
USE IN (lcTmpCutPik)
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfEditAlo
*! Developer : Wael Aly Mohamed
*! Date      : 02/09/1999
*! Purpose   : Edit ticket line orders allocated quantity while actualization
*!*************************************************************
*! Calls     : MFEDALO.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfEditAlo()
*!*************************************************************
FUNCTION lfEditAlo

STORE IIF(lcTranType='M',CuttktL.Qty1,PosLn.Qty1) TO lnCut1
STORE IIF(lcTranType='M',CuttktL.Qty2,PosLn.Qty2) TO lnCut2
STORE IIF(lcTranType='M',CuttktL.Qty3,PosLn.Qty3) TO lnCut3
STORE IIF(lcTranType='M',CuttktL.Qty4,PosLn.Qty4) TO lnCut4
STORE IIF(lcTranType='M',CuttktL.Qty5,PosLn.Qty5) TO lnCut5
STORE IIF(lcTranType='M',CuttktL.Qty6,PosLn.Qty6) TO lnCut6
STORE IIF(lcTranType='M',CuttktL.Qty7,PosLn.Qty7) TO lnCut7
STORE IIF(lcTranType='M',CuttktL.Qty8,PosLn.Qty8) TO lnCut8
=SEEK(STYLE,'STYLE')
llExit = .F.
DO WHILE !llExit
  DO (gcScrDir+"MFEDALO.SPX")
  =IIF(llExit,.T.,lfvCPOk())
ENDDO

*!*************************************************************
*! Name      : lfvAloBrow
*! Developer : Wael Aly Mohamed
*! Date      : 02/09/1999
*! Purpose   : browse ticket allocated order liens
*!*************************************************************
*! Calls     : lfwAloQty()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvAloBrow()
*!*************************************************************
FUNCTION lfvAloBrow

lcFields = "Order :H='Order#' :8:R,Qty1 :H='Qty1' :7:W=lfwAloQty('1') :V=lfvAloQty(),Qty2 :H='Qty2' :7:W=lfwAloQty('2') :V=lfvAloQty(),"+;
           "Qty3 :H='Qty3' :7:W=lfwAloQty('3') :V=lfvAloQty(),Qty4 :H='Qty4' :7:W=lfwAloQty('4') :V=lfvAloQty(),"+;
           "Qty5 :H='Qty5' :7:W=lfwAloQty('5') :V=lfvAloQty(),Qty6 :H='Qty6' :7:W=lfwAloQty('6') :V=lfvAloQty(),"+;
           "Qty7 :H='Qty7' :7:W=lfwAloQty('7') :V=lfvAloQty(),Qty8 :H='Qty8' :7:W=lfwAloQty('8') :V=lfvAloQty(),"+;
           "Totqty :H='TotQty' :R"
BROWSE FIELDS &lcFields  ;
       WINDOW MFEDALO2   ;
       IN WINDOW MFEDALO ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       TITLE lcEditAlo

*!*************************************************************
*! Name      : lfDAloBrow
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Deactivate Edit allocation screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfDAloBrow()
*!*************************************************************
FUNCTION lfDAloBrow

IF WONTOP()=lcEditAlo
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  glFromBrow = .T.
  ON KEY LABEL TAB     DO lpTab WITH 'MFEDALO3','pbOk'
  ON KEY LABEL BACKTAB DO lpBackTab WITH 'MFEDALO3','pbCan'
ELSE
  glFromBrow = .F.
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfwAloQty
*! Developer : Wael Aly Mohamed
*! Date      : 02/09/1999
*! Purpose   : browse ticket allocated order liens
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcSize
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfwAloQty()
*!*************************************************************
FUNCTION lfwAloQty
PARAMETERS lcSize
RETURN(laOrdQty[VAL(lcSize)] > lnCut&lcSize-lnOpen&lcSize)

*!*************************************************************
*! Name      : lfvAloQty
*! Developer : Wael Aly Mohamed
*! Date      : 02/09/1999
*! Purpose   : Validate allocated order liens quantitty
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcSize
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvAloQty()
*!*************************************************************
FUNCTION lfvAloQty
REPLACE TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8

*!*************************************************************
*! Name      : lfvCPOk
*! Developer : Wael Aly Mohamed
*! Date      : 02/09/1999
*! Purpose   : Accept modification to order lines allocation
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcSize
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvCPOk()
*!*************************************************************
FUNCTION lfvCPOk
PRIVATE lnOrd1,lnOrd2,lnOrd3,lnOrd4,lnOrd5,lnOrd6,lnOrd7,lnOrd8

SUM ALL Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO ;
        lnOrd1,lnOrd2,lnOrd3,lnOrd4,lnOrd5,lnOrd6,lnOrd7,lnOrd8
GO TOP IN (lcTmpCutPik)
FOR lnCount = 1 TO Scale.Cnt
  lcCount = STR(lnCount,1)
  IF lnOrd&lcCount > lnCut&lcCount - lnOpen&lcCount
    *B602482,1 Message : 38167
    *B602482,1 Allocated quantity for size xxxxx cannot be greater than 
    *B602482,1 ticket actual quantity.
    *B602482,1 Button : 00000
    *B602482,1 OK
    =gfModalGen('TRM38167B00000','ALERT',Scale.SZ&lcCount)
    RETURN
  ENDIF
ENDFOR
FOR lnCount = 1 TO Scale.Cnt
  lcCount = STR(lnCount,1)
  laOrdQty[lnCount] = lnOrd&lcCount
ENDFOR
llExit = .T.
RELEASE WINDOW 'MFEDALO'

*!*************************************************************
*! Name      : lfvCPCan
*! Developer : Wael Aly Mohamed
*! Date      : 02/09/1999
*! Purpose   : Cancel edit order lines allocation
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvCPCan()
*!*************************************************************
FUNCTION lfvCPCan
lnChoice=2
llExit = .T.
CLEAR READ
RELEASE WINDOW 'MFEDALO'

*!*************************************************************
*! Name      : lfvLogLot
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Show Operation/Lot issued cost items
*!*************************************************************
*! Calls     : MFLOGLT.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvLogLot()
*!*************************************************************
FUNCTION lfvLogLot
PRIVATE lcOprCode,lcLotNo,laCstTypes,puCstTypes,laOpr,puOpr,puLots,laLots,lcCurrTag

lcOprCode = &lcOprDet..cOprCode
lcLotNo   = &lcOprDet..cLotNo

*E300725,1 Prepare costing category popup
puCstTypes = 1
IF lcTranType='T'
  DECLARE laCstTypes[5]
  laCstTypes[1] = 'All'
  laCstTypes[2] = laSetups[3,2]
  laCstTypes[3] = laSetups[4,2]
  laCstTypes[4] = laSetups[5,2]
  laCstTypes[5] = laSetups[6,2]
ELSE
  DECLARE laCstTypes[6]
  laCstTypes[1] = 'All'
  laCstTypes[2] = laSetups[3,2]
  laCstTypes[3] = laSetups[4,2]
  laCstTypes[4] = laSetups[5,2]
  laCstTypes[5] = laSetups[6,2]
  laCstTypes[6] = laSetups[7,2]
ENDIF

*E300725,1 Prepare Operations popup
SELECT DIST ;
gfCodDes(&lcOprHdr..cOprCode,'MfgCode'),&lcOprHdr..cOprCode;
FROM (lcOprDet),(lcOprHdr);
WHERE &lcOprDet..cOprCode=&lcOprHdr..cOprCode ORDER BY &lcOprHdr..cOperSeq INTO ARRAY laOpr

*B607271,1 KHM 06/04/2003 (Begin) Use the ceiling function.
*puOpr = ASCAN(laOpr,lcOprCode)/2
puOpr = CEILING(ASCAN(laOpr,lcOprCode)/2)
*B607271,1 KHM 06/04/2003 (End)

*E300725,1 Prepare Lots popup
lcCurrTag = ORDER(lcOprDet)
SET ORDER TO TAG 'LOT' IN (lcOprDet)
SELECT DIST cLotNo FROM (lcOPrDet) ;
WHERE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = lcTranType+laData[1]+laOpr[puOpr,2] ;
INTO ARRAY laLots
puLots = ASCAN(laLots,lcLotNo)

*E301077,55 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'BOMCOST',gcDataDir+'BOMCOST','SH')

*E300725,1 Get issued cost items for all operations/lots
SELECT * FROM BOMCOST WHERE cimtyp+ctktno = lcTranType+laData[1] .AND. ;
!EMPTY(cOprCode) .AND. !EMPTY(cLotNO) ;
INTO DBF (gcWorkDir+lcisslogfile)
INDEX ON cOprCode+cLotNo+cbomtype+item+iclr+mfgcode+cwarecode+cdyelot+cisession+crsession TAG (lcisslogfile)
SET KEY TO laOpr[puOpr,2]+laLots[puLots]
=SEEK(laOpr[puOpr,2]+laLots[puLots])
DO (gcScrDir+"MFLOGLT.SPX")
USE IN (lcisslogfile)
SELECT (lcOprDet)
SET ORDER TO TAG lcCurrTag IN (lcOprDet)

*!*************************************************************
*! Name      : lfvLogBrow
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Browse Operation/Lot issued cost items
*!*************************************************************
*! Calls     : lfShLogLt
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvLogBrow()
*!*************************************************************
FUNCTION lfvLogBrow
SELECT (lcisslogfile)
lnIMarker = RECNO()
*B603109,1 SSH 09/04/00  [Begin] Add dyelot field to the browse of Operation Manegment/Issue Item.
BROWSE FIELDS ;
       cMarker  =IIF(RECNO()=lnIMarker ,'>',' '):H=' ':R:1:W=.F.,;
       cItem =IIF(cCostType='M',gfCodDes(MfgCode,'MfgCode'),Item+ALLTRIM(IClr)) :H='Item' :R,;
       cDyelot    :H='Dyelot' :R,;
       cWareCode  :H='Warehouse' :R,;
       nTotQty    :H='Tot. Qty'  :R,;
       nUnitCst   :H='Unit Cost' :R,;
       nTotCst    :H='Tot. Cost' :R,;
       nUnitAcSt  :H='Act. Unit Cost' :R,;
       nTotAcSt   :H='Act. Tot. Cost' :R,;
       dTranDate  :H='Date' :R ,;
       cApInvNo   :H='Invoice#' :R ;
       WINDOW MFLOGLT1   ;
       IN WINDOW MFLOGLT ;
       NOMENU            ;
       NOAPPEND          ;
       NODELETE          ;
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       WHEN lfShLogLt() ;
       TITLE lcLogTit

*BROWSE FIELDS ;
       cMarker  =IIF(RECNO()=lnIMarker ,'>',' '):H=' ':R:1:W=.F.,;
       cItem =IIF(cCostType='M',gfCodDes(MfgCode,'MfgCode'),Item+ALLTRIM(IClr)) :H='Item' :R,;
       cWareCode  :H='Warehouse' :R,;
       nTotQty    :H='Tot. Qty'  :R,;
       nUnitCst   :H='Unit Cost' :R,;
       nTotCst    :H='Tot. Cost' :R,;
       nUnitAcSt  :H='Act. Unit Cost' :R,;
       nTotAcSt   :H='Act. Tot. Cost' :R,;
       dTranDate  :H='Date' :R ,;
       cApInvNo   :H='Invoice#' :R ;
       WINDOW MFLOGLT1   ;
       IN WINDOW MFLOGLT ;
       NOMENU            ;
       NOAPPEND          ;
       NODELETE          ;
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       WHEN lfShLogLt() ;
       TITLE lcLogTit

*B603109,1 SSH 09/04/00   [End]
*!*************************************************************
*! Name      : lfShLogLt
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Refresh Operation/Lot issued cost items Browse.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfShLogLt()
*!*************************************************************
FUNCTION lfShLogLt
lnIMarker = RECNO(lcisslogfile)
SHOW WINDOW (lcLogTit) REFRESH SAME

*!*************************************************************
*! Name      : lfvLogOpPp
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Show issued cost items for specific operation
*!*************************************************************
*! Calls     : lfvLogLtPp
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvLogOpPp()
*!*************************************************************
FUNCTION lfvLogOpPp
PRIVATE lcCurrTag
IF puOpr <> lcOldValue
  lcCurrTag = ORDER(lcOprDet)
  SET ORDER TO TAG 'LOT' IN (lcOprDet)
  SELECT DIST cLotNo FROM (lcOPrDet) ;
  WHERE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = lcTranType+laData[1]+laOpr[puOpr,2] ;
  INTO ARRAY laLots
  puLots = 1
  =lfvLogLtPp()
  SET ORDER TO TAG lcCurrTag IN (lcOprDet)
ENDIF

*!*************************************************************
*! Name      : lfvLogLtPp
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Show issued cost items for specific Lot
*!*************************************************************
*! Calls     : lfvLogLtPp
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvLogLtPp()
*!*************************************************************
FUNCTION lfvLogLtPp

SELECT (lcisslogfile)
IF puCstTypes = 1
  SET KEY TO laOpr[puOpr,2]+laLots[puLots]
  =SEEK(laOpr[puOpr,2]+laLots[puLots])
ELSE
  SET KEY TO laOpr[puOpr,2]+laLots[puLots]+STR(puCstTypes-1,1)
  =SEEK(laOpr[puOpr,2]+laLots[puLots]+STR(puCstTypes-1,1))
ENDIF
SHOW WINDOW (lcLogTit) REFRESH SAME

*!*************************************************************
*! Name      : lpSavScr
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Save Generated ticket cost sheet
*!*************************************************************
*! Calls     : gfModalGen,lfvNewLot,lfvIssCstItm
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lpSavScr()
*!*************************************************************
FUNCTION lpSavScr
PRIVATE lcCurrTag,lcFile

*B602872,1 If no cost sheet genereted don't run save procedure.
GO TOP IN (lcTktSheet)
IF EOF(lcTktSheet)
  RETURN
ENDIF  
*B602872,1 End.

*C102095,1 AMH Add a customized trigger for Cathy Daniels to [Start]
*C102095,1 AMH update field bomdate.
IF ASCAN(laEvntTrig , PADR('UPDBOMDT',10)) <> 0
  =gfDoTriger('MFCSSH',PADR('UPDBOMDT',10))
ENDIF     
*C102095,1 AMH [End]

SET ORDER TO TAG Ctktbom IN CTKTBOM
SET ORDER TO TAG BOMLINE IN BOMLINE
SELECT (lcTktSheet)
SCAN FOR cShowType = '1'
  SCATTER MEMVAR
  INSERT INTO CTKTBOM FROM MEMVAR
ENDSCAN
SELECT (lcDetFile)
SCAN FOR  cShowType = '1'
  SCATTER MEMVAR
  INSERT INTO BOMLINE FROM MEMVAR
ENDSCAN

*B604136,1 KHM 22/01/2001 (Begin) Checking if the material module is
*B604136,1                installed.
*=gfArDyRl('' , '' , lcWinCh0,.T.)
*=gfArDyRl('' , '' , lcDetFile,.T.)
IF 'MA' $ gcComp_mdl
  =gfArDyRl('' , '' , lcDetFile,.T.)
ENDIF
*B604136,1 KHM 22/01/2001 (End)

SET ORDER TO TAG (lcOprHdr) IN (lcOprHdr)
GO TOP IN (lcOprHdr)
lcFirstOpr = &lcOprHdr..cOprCode
lcCurrTag  = ORDER(lcOprDet)
SET ORDER TO TAG 'LOT' IN (lcOprDet)
*E301085,1 Generate cost sheet automatically
*IF !EMPTY(lcFirstOpr) .AND. !SEEK(lcTranType+laData[1]+lcFirstOpr,lcOprDet)
IF !llAutoGen AND !EMPTY(lcFirstOpr) .AND. !SEEK(lcTranType+laData[1]+lcFirstOpr,lcOprDet)
*E301085,1 (End)
  *E300725,1 Message : 38054
  *E300725,1 Do you wish to issue operation xxxx
  *E300725,1 Button : 38006
  *E300725,1 Yes,No
  IF gfModalGen('QRM38054B38006','ALERT',ALLTRIM(gfCodDes(lcFirstOpr,'MfgCode')))=1
    GO TOP IN (lcOprHdr)
    =lfvNewLot()
  ENDIF
ENDIF
SELECT (lcOprHdr)
SCAN
  SCATTER MEMVAR
  INSERT INTO MFGOPRHD FROM MEMVAR
ENDSCAN
SELECT (lcOprDet)
SCAN
  SCATTER MEMVAR
  INSERT INTO MFGOPRDT FROM MEMVAR
ENDSCAN
*E300725,1 Message : 38055
*E300725,1 Do you wish to issue cost items for operation xxxx
*E300725,1 Button : 38006
*E300725,1 Yes,No
IF !EMPTY(lcFirstOpr) .AND. SEEK(lcTranType+laData[1]+lcFirstOpr,lcOprDet) .AND. ;
  gfModalGen('QRM38055B38006','ALERT',ALLTRIM(gfCodDes(lcFirstOpr,'MfgCode')))=1
  =lfvIssCstItm(lcFirstOpr,'01')
ENDIF
*khm1
*lcFile = IIF(lcTranType='M','CUTTKTL',IIF(lcTranType='I','POSLN','MMFGORDD'))
lcFile = IIF(lcTranType='M','CUTTKTL',IIF(lcTranType $ 'ID','POSLN','MMFGORDD'))
*khm1
SELECT (lcTmpTkt)
lnCanQty = 0
SCAN
  SELECT (lcFile)
  GO &lcTmpTkt..nRecNo
  =RLOCK()
  IF lcTranType='T'
    lnCanQty = lnCanQty + MAX(nMfgTotQty-&lcTmpTkt..nTotQty,0)
    REPLACE nMfgTotQty WITH &lcTmpTkt..nTotQty
  ELSE
    lnCanQty = lnCanQty + MAX(TotQty-&lcTmpTkt..nTotQty,0)
    REPLACE Qty1   WITH &lcTmpTkt..nQty1 ,;
            Qty2   WITH &lcTmpTkt..nQty2 ,;
            Qty3   WITH &lcTmpTkt..nQty3 ,;
            Qty4   WITH &lcTmpTkt..nQty4 ,;
            Qty5   WITH &lcTmpTkt..nQty5 ,;
            Qty6   WITH &lcTmpTkt..nQty6 ,;
            Qty7   WITH &lcTmpTkt..nQty7 ,;
            Qty8   WITH &lcTmpTkt..nQty8 ,;
            TotQty WITH &lcTmpTkt..nTotQty
  ENDIF
  UNLOCK
ENDSCAN
laData[8] = laData[8]  + lnCanQty
laData[10]= laData[10] - lnCanQty


*B802378,1 Start, Add new items.
=lfAddNewItm()
*--
*IF USED(lcTmpStyle)
*  SELECT (lcTmpStyle)
*  SCAN
*    IF !SEEK(Style,'Style')
*      SCATTER MEMVAR
*      INSERT INTO 'STYLE' FROM MEMVAR
*    ENDIF
*  ENDSCAN
*  ZAP
*ENDIF
*IF USED(lcTmpFbric)
*  SELECT (lcTmpFbric)
*  SCAN
*    IF !SEEK(Fabric+Color,'Fabric')
*      SCATTER MEMVAR
*      INSERT INTO 'FABRIC' FROM MEMVAR
*    ENDIF
*  ENDSCAN
*  ZAP
*ENDIF
*B802378,1 End.

=lfUpdEstCst()
laData[3]='O'
SELECT (lcBaseFile)
*C200080,1 AMM Consider the dye order case
*=SEEK(IIF(lcTranType='I','P','')+laData[1])

*khm1
*=SEEK(IIF(lcTranType='I',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1])
=SEEK(IIF(lcTranType $ 'ID',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1])
*khm1

*C200080,1 AMM end
=RLOCK()
*E301235,4 WAB - in case of material the fields ladata[16],ladata[15] are the same
*E301235,4 WAB - field in the file so must contains the same value
*E301235,4 WAB - START
IF lcTranType = 'T'
  laData[16] = laData[15]
ENDIF
*E301235,4 WAB - END
GATHER FROM laData FIELDS &lcScFields
*khm1
*IF lcTranType='I' 
IF lcTranType $ 'ID' 
*khm1
  FOR lnCount = 1 TO 5
    lcCount = STR(lnCount,1)
    *MAN Start Prevent Updating the price
    IF laSetups[lnCount+12,2] = 'P' AND llAutoGen
      LOOP
    ENDIF 
    *MAN End
    DO CASE
      CASE laSetups[lnCount+12,2] = 'P'
        REPLACE nFCost&lcCount WITH nICost&lcCount /(1 &lcPExSign POSHDR.nPriceRat &lcPUntSin IIF(POSHDR.nCurrUnit=0,1,POSHDR.nCurrUnit))
      CASE !INLIST(laSetups[lnCount+12,2],'F','T','S')
        REPLACE nFCost&lcCount WITH nICost&lcCount /(1 &lcDExSign POSHDR.nDutyRat &lcDUntSin IIF(POSHDR.nDCurUnit=0,1,POSHDR.nDCurUnit))
      OTHERWISE
        REPLACE nFCost&lcCount WITH nICost&lcCount
    ENDCASE
  ENDFOR
  *MAN Update PoTotal 
  REPLACE PoTotal WITH nICost1+nICost2+nICost3+nICost4+nICost5
ENDIF
UNLOCK
*E301085,1 Generate cost sheet automatically
IF !llAutoGen
  STORE .F. TO laScrMode
  STORE .T. TO laScrMode[2]
ENDIF
*E301427,1 SSH Update Pw files.
llDummy = llPWInst .AND. (lcTranType = "M") .AND. lfPwUpdate()
IF llPWInst .AND. (lcTranType = "M") .AND. USED(lcPWCtkBom)
  SELECT (lcPWCtkBom)
  ZAP
ENDIF
IF llPWInst .AND. (lcTranType = "M") .AND. USED(lcDetLin)
  SELECT(lcDetLin)
  ZAP
ENDIF
*E301427,1 SSH [END]
*E301085,1 (End)

SET ORDER TO TAG lcCurrTag IN (lcOprDet)
SELECT (lcBaseFile)

*!*************************************************************
*! Name      : lfUpdEstCst
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Update ticket lines estimated unit cost
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfUpdEstCst()
*!*************************************************************
*B604790,1 AMH Changing the function name to use the old name with the fixed function [Start]
*FUNCTION lfUpdEstCst
FUNCTION lfUpdEstXX
*B604790,1 AMH [End]

SET ORDER TO TAG BOMLINE IN BOMLINE
IF SEEK(lcTranType+'1'+laData[1],'BOMLINE')
  DO CASE
    CASE lcTranType='M'
      SELECT CutTktL
      SET ORDER TO TAG CutTktL
      =SEEK(laData[1])
      REPLACE REST nCost1 WITH 0,nCost2 WITH 0,nCost3 WITH 0,nCost4 WITH 0,nCost5 WITH 0;
      WHILE cuttkt+style+dyelot+trancd+STR(RECNO(),7) = laData[1]
    CASE lcTranType='I'
      SELECT PosLn
      SET ORDER TO TAG PosLn
      *C200080,1 AMM Consider the dye order case
      *=SEEK('P'+laData[1])
      =SEEK(IIF(EMPTY(lcDyePO),'P','D')+laData[1])
      *C200080,1 AMM end
      *MAN Exclude the Price Fields Since the the BOMLINE is updated from these
      *MAN fields in the Automatic mode
      IF !llAutoGen
        REPLACE REST nCost1  WITH 0,nCost2  WITH 0,nCost3  WITH 0,nCost4  WITH 0,;
                     nCost5  WITH 0,nECost1 WITH 0,nECost2 WITH 0,nECost3 WITH 0,;
                     nECost4 WITH 0,nECost5 WITH 0;
        WHILE cStytype+po+style+STR(lineno,6)+trancd+STR(RECNO(),7) = 'P'+laData[1]
      ELSE
        REPLACE REST nCost2  WITH 0,nCost3  WITH 0,nCost4  WITH 0,;
                     nCost5  WITH 0,nECost2 WITH 0,nECost3 WITH 0,;
                     nECost4 WITH 0,nECost5 WITH 0;
        WHILE cStytype+po+style+STR(lineno,6)+trancd+STR(RECNO(),7) = 'P'+laData[1]      
      ENDIF  
      
    CASE lcTranType='T'
      SELECT mmfgordd
      SET ORDER TO TAG MMFGORDD
      =SEEK(laData[1])
      REPLACE REST nCost1 WITH 0,nCost2 WITH 0,nCost3 WITH 0,nCost4 WITH 0;
      WHILE cmfgordno+cfabric+color+dyelot+trancd = laData[1]
  ENDCASE

  SELECT BOMLINE
  SCAN REST WHILE ;
       cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
       lcTranType+'1'+laData[1] FOR !lVoid
    SCATTER MEMVAR
    DO CASE
      CASE lcTranType='M' .AND. SEEK(m.CTktNo+m.Style,'CutTktL')
        SELECT CutTktL
        REPLACE REST ;
        ('nCost'+m.cBomTyp) WITH EVAL('nCost'+m.cBomTyp)+m.UnitQty*m.UnitCost ;
        WHILE cuttkt+style+dyelot+trancd+STR(RECNO(),7) = m.CTktNo+m.Style
      CASE lcTranType='I' 
        lnECost = m.UnitQty*m.UnitCost      
        DO CASE
          CASE m.cCatgTyp = 'P'
            lnECost = m.UnitQty*m.UnitCost &lcPExSign POSHDR.nPriceRat &lcPUntSin POSHDR.nCurrUnit
          CASE !INLIST(m.cCatgTyp,'S','F','T')
            lnECost = m.UnitQty*m.UnitCost &lcDExSign POSHDR.nDutyRat &lcDUntSin POSHDR.nDCurUnit
        ENDCASE
        *MAN Added The Following IF Condition to stop updating the price
        *MAN in the Automatic mode 
        *C200080,1 AMM  Consider the dye order case
        *IF !( m.cBomTyp = "1" AND llAutoGen)
        IF !( m.cCatgTyp = 'P' AND llAutoGen)
        *C200080,1 AMM
          SELECT PosLn
          *C200080,1 AMM Consider the dye order case
          *IF SEEK('P'+m.CTktNo+m.Style+STR(m.LineNo,6)+'1','PosLn')  
          IF SEEK(IIF(EMPTY(lcDyePO),'P','D')+m.CTktNo+m.Style+STR(m.LineNo,6)+'1','PosLn')  
            REPLACE ('nCost'+m.cBomTyp)  WITH EVAL('nCost'+m.cBomTyp) +m.UnitQty*m.UnitCost ,;
                    ('nECost'+m.cBomTyp) WITH EVAL('nECost'+m.cBomTyp)+lnECost
            *--B603840,1 RAMY [start]
            IF m.cBomTyp = '1'
              IF nCost1 > Gros_Price
                REPLACE Gros_Price WITH nCost1 ; 
                        Disc_Pcnt  WITH 0
              ELSE
                REPLACE Disc_Pcnt  WITH IIF(Gros_Price = 0 , 0 , 100 - nCost1 * 100 / Gros_Price)
              ENDIF
            ENDIF
            *--B603840,1 RAMY [end]
          ENDIF 
          *C200080,1 AMM Consider the dye order case
          *IF SEEK('P'+m.CTktNo+m.Style+STR(m.LineNo,6)+'3','PosLn')
          IF SEEK(IIF(EMPTY(lcDyePO),'P','D')+m.CTktNo+m.Style+STR(m.LineNo,6)+'3','PosLn')
          *C200080,1 AMM end
            REPLACE ('nCost'+m.cBomTyp)  WITH EVAL('nCost'+m.cBomTyp) +m.UnitQty*m.UnitCost ,;
                    ('nECost'+m.cBomTyp) WITH EVAL('nECost'+m.cBomTyp)+lnECost
          ENDIF  
        ENDIF  
      CASE lcTranType='T' .AND. SEEK(m.CTktNo+SUBSTR(m.Style,1,7)+m.SClr,'mmfgordd')
        SELECT mmfgordd
        REPLACE REST ;
        ('nCost'+m.cBomTyp) WITH EVAL('nCost'+m.cBomTyp)+m.UnitQty*m.UnitCost ;
        WHILE cmfgordno+cfabric+color+dyelot+trancd = ;
              m.CTktNo+SUBSTR(m.Style,1,7)+m.SClr
    ENDCASE
  ENDSCAN

  *--Update Posln Estemeted cost and cost by size.
  *B602966,1 Start.
  *C200080,1 AMM Consider the dye order case
  *IF lcTranType='I' AND SEEK('P'+laData[1],'POSLN')
  IF lcTranType='I' AND SEEK(IIF(EMPTY(lcDyePO),'P','D')+laData[1],'POSLN')
  *C200080,1 AMM end
    STORE 0 TO lnSCsAmt,lnSCsQty
    SELECT POSLN
    *C200080,1 AMM 
    *SCAN WHILE cStytype+Po='P'+laData[1] FOR TranCd $ '13'
    *FOR lnI=2 TO 5 
    SCAN WHILE cStytype+Po=IIF(EMPTY(lcDyePO),'P','D')+laData[1] FOR TranCd $ '13'
      FOR lnI = IIF(EMPTY(lcDyePO) , 2 , 1) TO 5
    *C200080,1 AMM end
        lcI = STR(lnI,1)
        lcBomLKey = 'I1'+POSLN.PO+STR(POSLN.lineno,6)+lcI+POSLN.style
        IF SEEK(lcBomLKey,'BOMLINE')
          SELECT BOMLINE
          *B802449,1 Start, Divide on total qty in posln insted of bomline. 
          *SUM REST (UnitCost*UnitQty)*StyQty,StyQty WHILE cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style = lcBomLKey TO lnSCsAmt,lnSCsQty
          SUM REST (UnitCost*UnitQty)*StyQty WHILE cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style = lcBomLKey TO lnSCsAmt
          SELECT POSLN
          =RLOCK()
          *REPLACE nCost&lcI  WITH IIF(lnSCsQty<>0,ROUND((lnSCsAmt/lnSCsQty),2),0),;
          *        nECost&lcI WITH (nCost&lcI &lcDExSign POSHDR.nDutyRat &lcDUntSin POSHDR.nDCurUnit)
          *C200080,1 AMM Set round to 3 decimals
          *REPLACE nCost&lcI  WITH IIF(POSLN.TotQty<>0,ROUND((lnSCsAmt/POSLN.TotQty),2),0),;
                  nECost&lcI WITH (nCost&lcI &lcDExSign POSHDR.nDutyRat &lcDUntSin POSHDR.nDCurUnit)
          REPLACE nCost&lcI  WITH IIF(POSLN.TotQty<>0,ROUND((lnSCsAmt/POSLN.TotQty),3),0),;
                  nECost&lcI WITH (nCost&lcI &lcDExSign POSHDR.nDutyRat &lcDUntSin POSHDR.nDCurUnit)
          *C200080,1 AMM end
          UNLOCK
          *B802449,1 End.
        ENDIF
      ENDFOR
    ENDSCAN
  ENDIF
  *B602966,1 End.

ENDIF

*!*************************************************************
*! Name      : lfGetInfo
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Initialize cost sheet information
*!*************************************************************
*! Calls     : lfBrowse
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfGetInfo()
*!*************************************************************
FUNCTION lfGetInfo
PRIVATE lcFile,lcFileKey

*E301077,55 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'MFGOPRHD',gcDataDir+'MFGOPRHD','SH')
=gfOpenFile(gcDataDir+'MFGOPRDT',gcDataDir+'MFGOPRDT','SH')
=gfOpenFile(gcDataDir+'BOMLINE',gcDataDir+'BOMLINE','SH')
=gfOpenFile(gcDataDir+'BOM',gcDataDir+'BOM','SH')
=gfOpenFile(gcDataDir+'BOMCOST',gcDataDir+'BOMCOST','SH')
=gfOpenFile(gcDataDir+'FABRIC',gcDataDir+'FABRIC','SH')
=gfOpenFile(gcDataDir+'FABDYE',gcDataDir+'FABDYE','SH')
=gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')
=gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
=gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
SELECT STYLE
SET RELATION TO 'S'+SCALE INTO SCALE
DO CASE 
  CASE lcTranType = 'M'
    =gfOpenFile(gcDataDir+'CUTTKTL',gcDataDir+'CUTTKTL','SH')
  
  *khm1
  *CASE lcTranType = 'I'
  CASE lcTranType $ 'ID'
  *khm1
    
    =gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLN','SH')
  CASE lcTranType = 'T'
    =gfOpenFile(gcDataDir+'MMfgOrdD',gcDataDir+'Mmfgordd','SH')
ENDCASE
*E301077,55 (End)

*khm1
*lcFile    = IIF(lcTranType='M','CutTktL',IIF(lcTranType='I','PosLn','MMfgOrdD'))
*lcFileKey = IIF(lcTranType='M','CutTkt',IIF(lcTranType='I','cStyType+Po','cMfgOrdNo'))
lcFile    = IIF(lcTranType='M','CutTktL',IIF(lcTranType $ 'ID','PosLn','MMfgOrdD'))
lcFileKey = IIF(lcTranType='M','CutTkt',IIF(lcTranType $ 'ID','cStyType+Po','cMfgOrdNo'))
*khm1

SELECT (lcTmpTkt)
ZAP
SELECT (lcFile)
*C200080,1 AMM Consider the dye order case
*=SEEK(IIF(lcTranType='I','P','')+laData[1])
*SCAN REST WHILE EVAL(lcFileKey)=IIF(lcTranType='I','P','')+laData[1] FOR TranCd='1'

*khm1
*=SEEK(IIF(lcTranType='I',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1])
*SCAN REST WHILE EVAL(lcFileKey)=IIF(lcTranType='I',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1] FOR TranCd='1'
=SEEK(IIF(lcTranType $ 'ID',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1])
SCAN REST WHILE EVAL(lcFileKey)=IIF(lcTranType $ 'ID',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1] FOR TranCd='1'
*khm1

*C200080,1 AMM end
  SELECT (lcTmpTkt)
  APPEND BLANK
  REPLACE Item      WITH IIF(lcTranType='T',&lcFile..cFabric,&lcFile..Style) ,;
          Color     WITH IIF(lcTranType='T',&lcFile..Color,'') ,;
          cDyelot   WITH &lcFile..Dyelot ,;
          cWareCode WITH &lcFile..cWareCode ,;
          LineNo    WITH &lcFile..LineNo ,;
          nQty1     WITH IIF(lcTranType='T',0,&lcFile..Qty1) ,;
          nQty2     WITH IIF(lcTranType='T',0,&lcFile..Qty2) ,;
          nQty3     WITH IIF(lcTranType='T',0,&lcFile..Qty3) ,;
          nQty4     WITH IIF(lcTranType='T',0,&lcFile..Qty4) ,;
          nQty5     WITH IIF(lcTranType='T',0,&lcFile..Qty5) ,;
          nQty6     WITH IIF(lcTranType='T',0,&lcFile..Qty6) ,;
          nQty7     WITH IIF(lcTranType='T',0,&lcFile..Qty7) ,;
          nQty8     WITH IIF(lcTranType='T',0,&lcFile..Qty8) ,;
          nTotQty   WITH IIF(lcTranType='T',&lcFile..nMfgTotQty,&lcFile..TotQty) ,;
          nRecNo    WITH RECNO(lcFile)
ENDSCAN
SELECT (lcTktSheet)
IF WEXIST(lcWSheet)
  HIDE WINDOW (lcWSheet)
  RELEASE WINDOW (lcWSheet)
ENDIF
ZAP
SELECT CTKTBOM
=SEEK(lcTranType+laData[1])
SCAN REST WHILE cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot = lcTranType+laData[1] ;
          FOR !lVoid
  IF !SEEK(Typ+'0',lcTktSheet)
    INSERT INTO (lcTktSheet) (TYP,cShowType,Item) VALUES ;
    (cTktBom.Typ,'0',lfLblDesc(laSetups[2+VAL(CTKTBOM.Typ),2]))
  ENDIF
  SCATTER MEMVAR
  m.cShowType = '1'
  INSERT INTO (lcTktSheet) FROM MEMVAR
ENDSCAN
*E301077,55 Inhance openning files to speed up transaction
SELECT (lcTktSheet)
SET RELATION TO SUBSTR(ITEM,1,7)+ICLR INTO FABRIC
*B802396,1 SSH 11/04/00  [Begin] Add Relation to FabDye/StyDye.
SET RELATION TO SUBSTR(ITEM,1,7)+ICLR+cWareCode+Dyelot INTO FABDYE ADDITIVE
SET RELATION TO ITEM+cWareCode+Dyelot INTO STYDYE ADDITIVE
*B802396,1 SSH 11/04/00  [End]
SET RELATION TO ITEM INTO STYLE ADDITIVE
*E301077,55 (End)

GO TOP IN (lcTktSheet)
IF WEXIST(lcDetTitle)
  HIDE WINDOW (lcDetTitle) SAME
  RELEASE WINDOW (lcDetTitle)
ENDIF
*E301077,55 Inhance openning files to speed up transaction
*SELECT (lcDetFile)
*ZAP
SELECT BOMLINE
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,4]
laFileStru[lnFileStru+1,1] = 'cShowType'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'lHideTit'
laFileStru[lnFileStru+2,2] = 'L'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0
CREATE TABLE (gcWorkDir+lcDetFile) FROM ARRAY laFileStru
INDEX ON cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode TAG 'BOMLINE'
INDEX ON Style+SClr+STR(LineNo,6)+cBomTyp+cShowType+Item+IClr+MfgCode TAG (lcDetFile) ADDITIVE
*E301077,55 (End)

SELECT BOMLINE
=SEEK(lcTranType+'1'+laData[1])
SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+;
                iclr+mfgcode  = lcTranType+'1'+laData[1] FOR !lVoid
  IF !SEEK(Style+SClr+STR(LineNo,6)+cBomTyp+'0',lcDetFile)
    llHideTit = SEEK(Style+SClr+STR(LineNo,6),lcDetFile)
    INSERT INTO (lcDetFile) ;
    (Style,SClr,LineNo,cBomTyp,cShowType,Item,lHideTit) VALUES ;
    (BomLine.Style,BomLine.SClr,BomLine.LineNo,BomLine.cBomTyp,'0',;
     lfLblDesc(laSetups[2+VAL(BOMLINE.cBomTyp),2]),llHideTit)
  ENDIF
  SCATTER MEMVAR
  m.cShowType = '1'
  INSERT INTO (lcDetFile) FROM MEMVAR
  IF !SEEK(Style+SClr+STR(LineNo,6)+cBomTyp+'2',lcDetFile)
    INSERT INTO (lcDetFile) ;
    (Style,SClr,LineNo,cBomTyp,cShowType,Item) VALUES ;
    (BomLine.Style,BomLine.SClr,BomLine.LineNo,BomLine.cBomTyp,'2','***** Total :')
  ENDIF
  SELECT (lcDetFile)
  REPLACE ItemQty  WITH ItemQty + BomLine.ItemQty,;
          ItemAmt  WITH ItemAmt + BomLine.ItemAmt
  lcOprCode = IIF(EMPTY(BomLine.cOprCode),BomLine.MfgCode,BomLine.cOprCode)
  IF !EMPTY(lcOprCode) AND SEEK(lcTranType+laData[1]+lcOprCode,'MFGOPRHD');
     AND SEEK(BomLine.Style+BomLine.SClr+STR(BomLine.LineNo,6),lcTmpTkt)
     SELECT (lcTmpTkt)
     *B802700,1 Get the smallest op seq[Start] 
     *lcFrstOpr   = IIF(nFrstOprSq=0 OR (VAL(MFGOPRHD.cOperSeq) < nFrstOprSq),MFGOPRHD.cOprCode,cFrstOpr)
     *lnFrstOprSq = IIF(nFrstOprSq=0 OR (VAL(MFGOPRHD.cOperSeq) < nFrstOprSq),VAL(MFGOPRHD.cOperSeq),nFrstOprSq)
        *B603861,1 AMH [Start] replace "M" with lcTranType
        *SELECT MIN(cOperSeq) FROM MFGOPRHD WHERE cimtyp+ctktno+coprcode="M"+LADATA[1] INTO ARRAY laTmp1      
        SELECT MIN(cOperSeq) FROM MFGOPRHD WHERE cimtyp+ctktno+coprcode=lcTranType+LADATA[1] INTO ARRAY laTmp1
        IF _TALLY > 0
          *SELECT coprCode FROM MFGOPRHD  WHERE cimtyp+ctktno+coprcode="M"+LADATA[1] AND cOperSeq = laTmp1[1] INTO ARRAY laTmp2            
          SELECT coprCode FROM MFGOPRHD  WHERE cimtyp+ctktno+coprcode=lcTranType+LADATA[1] AND cOperSeq = laTmp1[1] INTO ARRAY laTmp2
          *B603861,1 AMH [End]
          lcFrstOpr = laTmp2[1]
          lnFrstOprSq = VAL(laTmp1[1])      
     
          REPLACE cFrstOpr   WITH lcFrstOpr ,;
               nFrstOprSq WITH lnFrstOprSq
        ENDIF     
     *B802700,1 Get the smallest op seq[End..]                      
  ENDIF
ENDSCAN
GO TOP IN (lcDetFile)

IF WEXIST(lcOprHdTit)
  HIDE WINDOW (lcOprHdTit) SAME
  RELEASE WINDOW (lcOprHdTit)
ENDIF
*E301077,55 Inhance openning files to speed up transaction
*SELECT (lcOprHdr)
*ZAP
*SELECT MFGOPRHD
*IF SEEK(lcTranType+laData[1])
*  SCAN REST WHILE cimtyp+ctktno+coprcode = lcTranType+laData[1]
*    SCATTER MEMVAR
*    INSERT INTO (lcOprHdr) FROM MEMVAR
*  ENDSCAN
*ENDIF

SELECT * FROM MFGOPRHD WHERE cimtyp+ctktno+coprcode = lcTranType+laData[1] ;
INTO DBF (gcWorkDir+lcOprHdr)
INDEX ON cIMTyp+cTktNo+cOprCode TAG 'MFGOPRHD'
INDEX ON cOperSeq+cOprCode TAG (lcOprHdr) ADDITIVE
*E301077,55 (End)

GO TOP IN (lcOprHdr)
lcFirstOpr = &lcOprHdr..cOprCode

IF WEXIST(lcBrZoom)
  HIDE WINDOW (lcBrZoom) SAME
  RELEASE WINDOW (lcBrZoom)
ENDIF
IF WEXIST(lcOprDtTit)
  HIDE WINDOW (lcOprDtTit) SAME
  RELEASE WINDOW (lcOprDtTit)
ENDIF

*E301077,55 Inhance openning files to speed up transaction
*SELECT (lcOprDet)
*ZAP
*SELECT (lcOprDet1)
*ZAP
*SELECT MFGOPRDT
*SET ORDER TO TAG MFGOPRDT
*IF SEEK(lcTranType+laData[1])
*  llIssFirst = .T.
*  SCAN REST WHILE cimtyp+ctktno = lcTranType+laData[1]
*    SCATTER MEMVAR
*    *B801929,1 Update operation sequence.
*    =SEEK(lcTranType+laData[1]+m.cOprCode,'MFGOPRHD')
*    m.cOperSeq=MFGOPRHD.cOperSeq
*    *B801929,1 (End)
*    INSERT INTO (lcOprDet) FROM MEMVAR
*  ENDSCAN
*ENDIF
*B603243,1 AMM make the operation done by style/color/dyelot

*B603292,1 Optimize the query speed .
*SELECT MFGOPRDT.*,MFGOPRHD.cOperSeq FROM MFGOPRDT,MFGOPRHD WHERE ;
MFGOPRDT.cimtyp+MFGOPRDT.ctktno = lcTranType+laData[1] AND ;
MFGOPRHD.cimtyp+MFGOPRHD.ctktno+MFGOPRHD.cOprCode = lcTranType+laData[1]+MFGOPRDT.cOprCode ;
INTO DBF (gcWorkDir+lcOprDet)

  SELECT MFGOPRDT.*,MFGOPRHD.cOperSeq ;
    FROM MFGOPRDT,MFGOPRHD ;
   WHERE MFGOPRHD.cimtyp+MFGOPRHD.ctktno+MFGOPRHD.cOprCode = lcTranType+laData[1] AND ;
         MFGOPRDT.cimtyp+MFGOPRDT.ctktno+MFGOPRDT.coprcode+MFGOPRDT.clotno = lcTranType+laData[1]+MFGOPRHD.cOprCode;
INTO DBF (gcWorkDir+lcOprDet)
*B603243,1 AMM 
*INDEX ON cIMTYp+cTktNo+cOprCode+Item+Color TAG 'Item'
*INDEX ON cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd TAG 'Lot'
INDEX ON cIMTYp+cTktNo+cOprCode+Item+Color+cDyelot TAG 'Item'
INDEX ON cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd+cDyelot TAG 'Lot'
*B603243,1 AMM  end
*B603243,1 AMM Add cdyelot C(10)
CREATE TABLE (gcWorkDir+lcOprDet1) ;
(cIMTyp C(1), cTktNo C(6),cDyelot C(10),cOprCode C(6), cOperSeq C(2),cLotNo C(2), cContCode C(6), Item C(19),Color C(6),;
 Iss1 N(6),Iss2 N(6),Iss3 N(6),Iss4 N(6),Iss5 N(6),Iss6 N(6),Iss7 N(6),Iss8 N(6),Iss N(7),;
 Rcv1 N(6),Rcv2 N(6),Rcv3 N(6),Rcv4 N(6),Rcv5 N(6),Rcv6 N(6),Rcv7 N(6),Rcv8 N(6),Rcv N(7),;
 Can1 N(6),Can2 N(6),Can3 N(6),Can4 N(6),Can5 N(6),Can6 N(6),Can7 N(6),Can8 N(6),Can N(7),;
 Dmg1 N(6),Dmg2 N(6),Dmg3 N(6),Dmg4 N(6),Dmg5 N(6),Dmg6 N(6),Dmg7 N(6),Dmg8 N(6),Dmg N(7))
*E301077,55 (End)
GO TOP IN (lcOprDet)

*!*************************************************************
*! Name      : lfvDetLvl
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Operations Level of Details
*!*************************************************************
*! Calls     : MFDETLVL.SPX,lfBrowLots
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvDetLvl()
*!*************************************************************
FUNCTION lfvDetLvl
PRIVATE lcBtStatus
DO (gcScrDir+"MFDETLVL.SPX")
=lfBrowLots()
lcBtStatus=IIF(rbSummary=1,'ENABLE','DISABLE')
SHOW GET pbIssLot &lcBtStatus
SHOW GET pbLotIss &lcBtStatus
SHOW GET pbModLot &lcBtStatus
SHOW GET pbRecLot &lcBtStatus
SHOW GET pbCanLot &lcBtStatus
SHOW GET pbDamLot &lcBtStatus

*!*************************************************************
*! Name      : lfDfPopup
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Adjust operations level of details popup
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfDfPopup()
*!*************************************************************
FUNCTION lfDfPopup
lnBar = VAL(RIGHT(VARREAD(),1))
IF EVAL(VARREAD())
  DEFINE BAR lnBar OF lcpusort PROMPT laSort[lnBar,2]
ELSE
  RELEASE BAR lnBar OF lcpusort 
ENDIF

*!*************************************************************
*! Name      : lfvOkDetLvl
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate operations level of details screen
*!*************************************************************
*! Calls     : lfAdjDetLvl
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvOkDetLvl()
*!*************************************************************
FUNCTION lfvOkDetLvl

lcIndExp = 'cIMTyp+cTktNo'
FOR lnCount = 1 TO CNTBAR('lcpusort')
  *B801929,1 Sort operation lots by operation sequence.
  *lcIndExp = lcIndExp + '+' + laSort[GETBAR('lcpusort',lnCount),1] + ;
             IIF(lcTranType='T' .AND. laSort[GETBAR('lcpusort',lnCount),1]='ITEM','+COLOR','')
  *B603243,1 AMM Add dyelot field
  *lcIndExp = lcIndExp + '+' + IIF(laSort[GETBAR('lcpusort',lnCount),1]='COPRCODE','COPERSEQ+','')+;
             laSort[GETBAR('lcpusort',lnCount),1] + ;
             IIF(lcTranType='T' .AND. laSort[GETBAR('lcpusort',lnCount),1]='ITEM','+COLOR','')
  lcIndExp = lcIndExp + '+' + IIF(laSort[GETBAR('lcpusort',lnCount),1]='COPRCODE','COPERSEQ+','')+;
             laSort[GETBAR('lcpusort',lnCount),1] + ;
             IIF(lcTranType='T' .AND. laSort[GETBAR('lcpusort',lnCount),1]='ITEM','+COLOR+cDyelot','')
  *B603243,1 AMM end
  *B801929,1 (End)
ENDFOR  
=lfAdjDetLvl()

*!*************************************************************
*! Name      : lfAdjDetLvl
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Create operation details file based on the selected level of details
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfAdjDetLvl()
*!*************************************************************
FUNCTION lfAdjDetLvl
SELECT (lcOprDet)
INDEX ON &lcIndExp TAG (lcOprDet)
IF rbSummary = 2
  lcGroupBy = STRTRAN(lcIndExp,'+',',')
  SELECT (lcOprDet1)
  ZAP
  INDEX ON &lcIndExp TAG (lcOprDet1)
  SELECT (lcOprDet)
  GO TOP
  DO WHILE !EOF()
    lcCurrKey = EVAL(lcIndExp)
    SCATTER MEMVAR
    *B603243,1 AMM Add Dyelot field
    *INSERT INTO (lcOprDet1) ;
     (cImTyp,cTktNo,cOprCode,cOperSeq,cLotNo,cCOntCode,Item,Color) VALUES ;
     (m.cImTyp,m.cTktNo,m.cOprCode,m.cOperSeq,m.cLotNo,m.cCOntCode,m.Item,m.Color)
    INSERT INTO (lcOprDet1) ;
     (cImTyp,cTktNo,cOprCode,cOperSeq,cLotNo,cCOntCode,Item,Color,cDyelot) VALUES ;
     (m.cImTyp,m.cTktNo,m.cOprCode,m.cOperSeq,m.cLotNo,m.cCOntCode,m.Item,m.Color,m.cDyelot)
    *B603243,1 AMM end
    SCAN WHILE EVAL(lcIndExp) = lcCurrKey
      SCATTER MEMVAR
      SELECT (lcOprDet1)
      lcField = IIF(m.TranCd='1','Iss',IIF(m.TranCd='2','Rcv',IIF(m.TranCd='3','Dmg','Can')))
      REPLACE (lcField) WITH EVAL(lcField) + m.nLotTotQty
      IF cbPerSize
        FOR lnCount = 1 TO 8
          REPLACE (lcField+STR(lnCount,1)) WITH EVAL(lcField+STR(lnCount,1)) + EVAL('m.nLotQty'+STR(lnCount,1))
        ENDFOR
      ENDIF
    ENDSCAN
  ENDDO
  GO TOP IN (lcOprDet1)
ENDIF

*!*************************************************************
*! Name      : lfvAssRolls
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Assign rolls
*!*************************************************************
*! Calls     : lfvMatRolls
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvAssRolls()
*!*************************************************************
FUNCTION lfvAssRolls

*E301077,55 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'ROLLS',gcDataDir+'Rolapl','SH')
*E301077,55 (End)
SELECT BOMCOST
SET ORDER TO TAG Bomcstkt
IF SEEK(m.Typ+lcTranType+laData[1]+m.Item+m.IClr+m.MfgCode)
  SET ORDER TO TAG Rolapl IN ROLLS
  SET RELATION TO crsession+cisession+SUBSTR(Item,1,7)+IClr+cwarecode+cDyelot+;
                   '***** N/A *****' INTO ROLLS
  lcticket   = m.Typ+lcTranType+laData[1]+m.Item+m.IClr+m.MfgCode
  lcBrFields = "cWareCode:H='Warehouse',dTranDate:H='Date',nTotQty:H='Quantity',nUnitCst:H='Cost',nTotCst:H='Amount',cApInvNo:H='Invoice' :7,"
  lcBrFields =lcBrFields+ "nAss=ROLLS.NQTYBAL :H='Unassigned' :10"
  IF ARIABROW([lcticket],'Issued Lots',gnBrHSRow1, gnBrHSCol1,gnBrHSRow2,gnBrHSCol2,'','','CUTTKT','laBrowArr')
    =lfvMatRolls('I',BomCost.cRSession,BomCost.cISession,SUBSTR(BomCost.Item,1,7),;
                 BomCost.IClr,BomCost.cWareCode,BomCost.cDyelot,BomCost.nTotQty)
  ENDIF
  SELECT BOMCOST
  SET RELATION OFF INTO ROLLS
ENDIF
SELECT MATINVJL

*!*************************************************************
*! Name      : lfvMatRolls
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Issue/Return rolls
*!*************************************************************
*! Calls     : lfRetAll,MFMAROL.SPX
*!*************************************************************
*! Parameters: lcAction   : 'I' Issue   'R' Return
*!             lcRSession : Receive Session
*!             lcISession : Issue Session
*!             lcFabric   : Fabric
*!             lcColor    : Color
*!             lcWareCode : Warehouse
*!             lcDyelot   : Dyelot
*!             lnIssue    : Issue/Return quantity
*!             llRetAll   : .T.  Return all issued quantity
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvMatRolls()
*!*************************************************************
FUNCTION lfvMatRolls
PARAMETERS lcAction,lcRSession,lcISession,lcFabric,lcColor,lcWareCode,;
           lcDyelot,lnIssue,lnReturn
PRIVATE lnBalance,lnAssigned,lnQty,lcRollId,llProceed,lcRRollId

*E301077,55 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'ROLLS',gcDataDir+'Rolapl','SH')
*E301077,55 (End)
STORE 0 TO lnBalance,lnAssigned,lnQty
STORE '' TO lcRollID,lcRRollID
llProceed = .T.
USE (gcDataDir+'ROLLS') AGAIN ALIAS ROLLS2 ORDER TAG Rollitem IN 0
SELECT ROLLS
SET ORDER TO TAG SESSION
=SEEK(lcIsession+lcFabric+lcColor+lcWareCode+lcDyelot)
LOCATE REST WHILE IIF(EMPTY(cisession),crsession,cisession)+cRollItem+Color+cWareCode+Dyelot+;
       cRollID=lcIsession+lcFabric+lcColor+lcWareCode+lcDyelot ;
FOR CROLLID <> '***** N/A *****'
IF !FOUND()
  IF lcAction = 'R'
    IF SEEK(lcISession+lcFabric+lcColor+lcWareCode+lcDyelot+'***** N/A *****')
      IF lnIssue=lnReturn
        DELETE 
      ELSE
        REPLACE nQtyBal WITH nQtyBal - lnIssue ,;
                nQty    WITH nQty    - lnIssue
      ENDIF 
    ENDIF  
    llProceed = .F.
  ENDIF   
ELSE
  =SEEK(lcIsession+lcFabric+lcColor+lcWareCode+lcDyelot)
  SUM REST NQTY TO lnAssigned ;
  WHILE IIF(EMPTY(cisession),crsession,cisession)+cRollItem+Color+cWareCode+Dyelot+;
        cRollID=lcIsession+lcFabric+lcColor+lcWareCode+lcDyelot ;
  FOR   CROLLID <> '***** N/A *****'
  =SEEK(lcIsession+lcFabric+lcColor+lcWareCode+lcDyelot)
  IF lcAction = 'R'
    IF lnIssue=lnReturn
      =lfRetAll(lcIsession,lcFabric,lcColor,lcWareCode,lcDyelot)
      llProceed = .F.
    ELSE  
      lnIssue=lnIssue-lnReturn
    ENDIF
  ENDIF
ENDIF
IF llProceed
  lcRemStat = IIF(lnAssigned > 0, 'ENABLE', 'DISABLE')
  lnBalance = lnIssue - lnAssigned
  DO (gcScrDir+"MFMAROL.SPX") WITH (lcAction='I')
ENDIF
USE IN ROLLS2

*!*************************************************************
*! Name      : lfBrowRoll
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Browse Issued/Returned rolls
*!*************************************************************
*! Calls     : lfwRollBrs
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfBrowRoll()
*!*************************************************************
FUNCTION lfBrowRoll
SELECT ROLLS2
SET ORDER TO TAG RollItem
BROWSE FIELDS cMarker =IIF(RECNO()=lnDMarker,'>',' '):H='':W=.F.,;
              crollid:H='Roll ID' :R,;
              NQTYBAL:H='Balance' :R ;
              WINDOW MFMAROL0   ;
              IN WINDOW MFMAROL ;
              NOMENU            ;         
              NOAPPEND          ;
              NODELETE          ;
              NOWAIT            ;
              SAVE              ;
              NOCLEAR           ;
              KEY lcFabric+lcColor+lcWareCode+lcDyelot  ;
              WHEN lfwRollBrs() ;
              VALID :F lfvRollBrs() ;
              TITLE lcRollTit   ;
              FOR TranCd = '1' .AND. nQtyBal>0 .AND. ;
              IIF(INLIST(laSetups[9,2],'L','F','I'),cRSession=lcRSession,.T.)
SELECT ROLLS
SET ORDER TO TAG SESSION
BROWSE FIELDS cMarker =IIF(RECNO()=lnRMarker,'>',' '):H='':W=.F.,;
              crollid:H='Roll ID':R,;
              NQTYBAL:H='Assigned':R ;
              WINDOW MFMAROL1   ;
              IN WINDOW MFMAROL ;
              NOMENU            ;         
              NOAPPEND          ;
              NODELETE          ;
              NOWAIT            ;
              SAVE              ;
              NOCLEAR           ;
              KEY lcIsession+lcFabric+lcColor+lcWareCode+lcDyelot ;
              WHEN lfwAsRoll()  ;
              VALID :F lfvRollBrs()  ;
              TITLE lcAsRollTit ;
              FOR TranCd='2' .AND. NQTY>=0 .AND. IIF(llIssue,nQtyBal>0,nQtyBal>=0) .AND. ;
                  cTktNo=laData[1] .AND. CROLLID <> '***** N/A *****'

*!*************************************************************
*! Name      : lfwRollBrs
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Show Available rolls
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwRollBrs()
*!*************************************************************
FUNCTION lfwRollBrs

lndMarker = RECNO('ROLLS2')
SHOW WINDOW (lcRollTit) REFRESH SAME
lcRollID = ROLLS2.crollid
SHOW GET lcRollID

*!*************************************************************
*! Name      : lfwAsRoll
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Show Assigned rolls
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwAsRoll()
*!*************************************************************
FUNCTION lfwAsRoll
lnRMarker = RECNO('ROLLS')
SHOW WINDOW (lcAsRollTit) REFRESH SAME
lnQty     = ROLLS.nQtyBal
lcRRollID = ROLLS.crollid
IF lnAssigned = 0
  lcRRollID = ''
  SHOW GET lcRRollID DISABLE
  SHOW GET lnQty DISABLE
  SHOW GET pbRemRol DISABLE
ELSE
  SHOW GET lcRRollID ENABLE
  SHOW GET lnQty ENABLE
  SHOW GET pbRemRol ENABLE
ENDIF

*!*************************************************************
*! Name      : lfvRollBrs
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Trab Enter key
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvRollBrs()
*!*************************************************************
FUNCTION lfvRollBrs
IF WONTOP() <> lcRollTit .AND. WONTOP() <> lcAsRollTit
  ON KEY LABEL ENTER
ENDIF

*!*************************************************************
*! Name      : lfMainRoll
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Assign/Return Rolls
*!*************************************************************
*! Calls     : lfAssRoll,lfRetRoll
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfMainRoll()
*!*************************************************************
PROCEDURE lfMainRoll

IF WONTOP() = lcRollTit
  =IIF(lcAction='I',lfAssRoll(),.T.)
ELSE
  =lfRetRoll()
ENDIF

*!*************************************************************
*! Name      : lfAssRoll
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Assign Rolls
*!*************************************************************
*! Calls     : gfModalGen,lfRefresh,lfwRollBrs,lfwAsRoll
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfAssRoll()
*!*************************************************************
FUNCTION lfAssRoll
IF lnBalance <= 0 
  *E300725,1 Message : 38079
  *E300725,1 cannot exceed the total issued quantity for this fabric/Color
  *E300725,1 Button : 00000
  *E300725,1 Ok
  =gfModalGen('TRM38079B00000','ALERT','issued')
  RETURN
ENDIF
SELECT ROLLS2
*B606782,1 KHM 12/31/2002 (Begin) Getting the roll balance quantity if the issued is greater
*B606782,1                than the roll's quantity.
lnBalance = MIN(lnBalance,Nqtybal)
*B606782,1 KHM 12/31/2002 

REPLACE Nqtybal WITH Nqtybal-MIN(lnBalance,nQty)
SELECT ROLLS
IF !SEEK(lcISession+ROLLS2.cRollItem+ROLLS2.Color+ROLLS2.cWareCode+ROLLS2.Dyelot+ROLLS2.cRollid)
  APPEND BLANK
  REPLACE CROLLID    WITH ROLLS2.cRollid ,;
          cRollItem  WITH ROLLS2.cRollItem ,;
          Color      WITH ROLLS2.Color   ,;
          CwareCode  WITH ROLLS2.cWareCode ,; 
          Dyelot     WITH ROLLS2.Dyelot  ,;
          Trancd     WITH '2',;
          Ctktno     WITH laData[1] ,;
          Csession   WITH lcIsession ,;
          Crsession  WITH ROLLS2.cRsession ,;
          Cisession  WITH lcIsession
ENDIF
REPLACE NQTY    WITH NQTY    + MIN(ROLLS2.nQty,lnBalance) ,;
        Nqtybal WITH Nqtybal + MIN(ROLLS2.nQty,lnBalance)
lnAssigned = lnAssigned + MIN(ROLLS2.nQty,lnBalance)
lnBalance = lnIssue - lnAssigned
=lfRefresh('MFMAROL2')
=lfwRollBrs() .AND. lfwAsRoll()

*!*************************************************************
*! Name      : lfRetRoll
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Return Rolls
*!*************************************************************
*! Calls     : gfModalGen,lfRefresh,lfwRollBrs,lfwAsRoll
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfRetRoll()
*!*************************************************************
FUNCTION lfRetRoll

IF lcAction = 'I'
  IF NQTY-NQTYBAL <> 0
    *E300725,1 Message : 38083
    *E300725,1 This roll has been returned before you cannot remove it
    *E300725,1 Button : 00000
    *E300725,1 Ok
    =gfModalGen('TRM38083B00000','ALERT')
    RETURN
  ENDIF
  SELECT ROLLS2
  =SEEK(lcFabric+lcColor+lcWareCode+lcDyelot+ROLLS.cRollid+"1"+ROLLS.cRSession)
  REPLACE Nqtybal WITH Nqtybal+ROLLS.nQty
  SELECT ROLLS
  lnAssigned = lnAssigned - nQty
  DELETE 
ELSE
  IF lnReturn = 0
    *E300725,1 Message : 38079
    *E300725,1 You cannot exceed the total returned quantity for this fabric/Color.
    *E300725,1 Button : 00000
    *E300725,1 Ok
    =gfModalGen('TRM38079B00000','ALERT','returned')
    RETURN
  ENDIF
  SELECT ROLLS2
  =SEEK(lcFabric+lcColor+lcWareCode+lcDyelot+ROLLS.cRollid+"1"+ROLLS.cRSession)
  REPLACE Nqtybal WITH Nqtybal+MIN(lnReturn,lnQty)
  SELECT ROLLS
  REPLACE nqtybal WITH nqtybal - MIN(lnReturn,lnQty)
  lnAssigned = lnAssigned - MIN(lnReturn,lnQty)
  SCATTER MEMVAR
  m.nQty    = MIN(lnReturn,lnQty) * -1
  m.nQtyBal = 0
  INSERT INTO ROLLS2 FROM MEMVAR
  lnReturn = lnReturn - MIN(lnReturn,lnQty)
ENDIF
lnBalance = lnIssue - lnAssigned
=lfRefresh('MFMAROL2')
=lfwRollBrs() .AND. lfwAsRoll()

*!*************************************************************
*! Name      : lfwRollID
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Store Roll Id before modification
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwRollID()
*!*************************************************************
FUNCTION lfwRollID
lcOldValue = lcRollId

*!*************************************************************
*! Name      : lfvRollID
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Issued roll ID
*!*************************************************************
*! Calls     : lfAssRoll
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvRollID()
*!*************************************************************
FUNCTION lfvRollID
PARAMETERS llMove
PRIVATE llContinue
IF (llMove .OR. LASTKEY() = 13) .AND. ;
   SEEK(lcFabric+lcColor+lcWareCode+lcDyelot+lcRollId+'1'+;
   IIF(INLIST(laSetups[9,2],'L','F','I'),lcRSession,''),'ROLLS2')
  =lfAssRoll()
  SELECT ROLLS2
  LOCATE REST WHILE cRollItem+Color+cWareCode+Dyelot+cRollId+TranCd+cRSession=;
  lcFabric+lcColor+lcWareCode+lcDyelot ;
  FOR TranCd = '1' .AND. nQtyBal>0 .AND. ;
  IIF(INLIST(laSetups[9,2],'L','F','I'),cRSession=lcRSession,.T.)
  lcRollId = ROLLS2.cRollId
  SHOW GET lcRollId
  _CUROBJ = _CUROBJ
ENDIF  

*!*************************************************************
*! Name      : lfvRRollID
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Returned roll ID
*!*************************************************************
*! Calls     : lfRetRoll
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvRRollID()
*!*************************************************************
FUNCTION lfvRRollID
PARAMETERS llMove
PRIVATE llContinue
IF (llMove .OR. LASTKEY() = 13) .AND. ;
   SEEK(lcFabric+lcColor+lcWareCode+lcDyelot+lcRRollId+'2'+;
   IIF(INLIST(laSetups[9,2],'L','F','I'),lcRSession,''),'ROLLS2')
  =lfRetRoll()
  lcRRollId = ROLLS.cRollId
  SHOW GET lcRRollId
  _CUROBJ = _CUROBJ
ENDIF  

*!*************************************************************
*! Name      : lfRetAll
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Return all roll issued quantity
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfRetAll()
*!*************************************************************
FUNCTION lfRetAll
PARAMETERS lcIsession,lcFabric,lcColor,lcWareCode,lcDyelot

SET ORDER TO TAG ROLLITEM IN ROLLS2
SET ORDER TO TAG SESSION  IN ROLLS
SELECT ROLLS
=SEEK(lcIsession+lcFabric+lcColor+lcWareCode+lcDyelot)
SCAN REST WHILE csession+cRollItem+Color+cWareCode+Dyelot+cRollID=;
                lcIsession+lcFabric+lcColor+lcWareCode+lcDyelot ;
          FOR   CROLLID <> '***** N/A *****' .AND. nqtybal <> 0
  SCATTER MEMVAR
  lcRollid  = crollid
  lnRetqty  = m.nqtybal
  m.NQty    = lnRetqty*-1
  m.nqtybal = 0
  SELECT ROLLS2
  APPEND BLANK
  GATHER MEMVAR
  IF SEEK(lcFabric+lcColor+lcWareCode+lcDyelot+lcRollid+"1"+m.cRSession)
    REPLACE Nqtybal WITH Nqtybal+lnRetqty
  ENDIF  
  SELECT ROLLS
  REPLACE nqtybal WITH 0
ENDSCAN
SELECT ROLLS
IF SEEK(lcISession+lcFabric+lcColor+lcWareCode+lcDyelot+'***** N/A *****')
  DELETE
ENDIF  

*!*************************************************************
*! Name      : lfvQty
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Roll Issued quantity
*!*************************************************************
*! Calls     : lfRefresh,gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvQty()
*!*************************************************************
FUNCTION lfvQty
PRIVATE	lnRecNo

IF lnQty = ROLLS.Nqtybal
  RETURN
ENDIF
lnRecNo = RECNO('ROLLS2')
=SEEK(lcFabric+lcColor+lcWareCode+lcDyelot+ROLLS.cRollid+"1"+ROLLS.cRSession,'ROLLS2')
*E300725,1 Message : 38075
*E300725,1 Roll xxx is completely issued
*E300725,1 Button : 00000
*E300725,1 Ok

*E300725,1 Message : 38078
*E300725,1 Issued quantity cannot be less than zero
*E300725,1 Button : 00000
*E300725,1 Ok

*E300725,1 Message : 38079
*E300725,1 cannot exceed the total issued quantity  for this fabric/Color
*E300725,1 Button : 00000
*E300725,1 Ok

*E300725,1 Message : 38080
*E300725,1 cannot exceed the roll balance
*E300725,1 Button : 00000
*E300725,1 Ok

*E300725,1 Message : 38081
*E300725,1 The assigned quantity cannot be less than the returned quantity
*E300725,1 Button : 00000
*E300725,1 Ok

IF ( lnQty < 0 .AND. ;
     gfModalGen('TRM38078B00000','ALERT','Issued'+'|'+'be less than zero')=1 ) .OR. ;
   ( lnAssigned - Nqtybal + lnQty > lnIssue .AND. ;
     gfModalGen('TRM38079B00000','ALERT','issued')=1) .OR. ;
   ( lnQty > ROLLS2.Nqtybal+Nqtybal .AND. gfModalGen('TRM38080B00000','ALERT')=1 ) .OR. ;
   ( lnQty < nQty - nQtyBal .AND. gfModalGen('TRM38081B00000','ALERT')=1 )
  lnQty = Nqtybal
ENDIF
lnAssigned = lnAssigned - Nqtybal + lnQty
lnBalance  = lnIssue - lnAssigned
SELECT ROLLS2
REPLACE Nqtybal WITH Nqtybal + ROLLS.Nqtybal - lnQty
GO lnRecNo
SELECT ROLLS
REPLACE nQty    WITH nQty - nQtyBal + lnQty ,;
        nQtyBal WITH lnQty
IF nQty = 0
  DELETE
ENDIF
=lfRefresh('MFMAROL2')
=lfwRollBrs() .AND. lfwAsRoll()

*!*************************************************************
*! Name      : lfvRQty
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Roll Returned quantity
*!*************************************************************
*! Calls     : lfRefresh,gfModalGen,lfwRollBrs,lfwAsRoll
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvRQty()
*!*************************************************************
FUNCTION lfvRQty

*E300725,1 Message : 38079
*E300725,1 You cannot exceed the total returned quantity for this fabric/Color.
*E300725,1 Button : 00000
*E300725,1 Ok

*E300725,1 Message : 38082
*E300725,1 The returned quantity for the current roll cannot exceed 9999
*E300725,1 Button : 00000
*E300725,1 Ok

IF ( lnReturn -(nQtyBal-lnQty) < 0 .AND. ;
     gfModalGen('TRM38079B00000','ALERT','returned')=1) .OR. ;
   ( lnQty < 0 .AND. ;
     gfModalGen('TRM38082B00000','ALERT',ALLTRIM(STR(nQtyBal)))=1)
  lnQty = nQtyBal
ENDIF
SELECT ROLLS2 
IF SEEK(lcFabric+lcColor+lcWareCode+lcDyelot+lcRollid+"1"+ROLLS.cRSession)
  REPLACE nQtyBal WITH nQtyBal + (ROLLS.nQtyBal-lnQty)
ENDIF
SELECT ROLLS
lnAssigned = lnAssigned - (nQtyBal-lnQty)
lnBalance  = lnIssue  - lnAssigned
lnReturn   = lnReturn - (nQtyBal-lnQty)
SCATTER MEMVAR
m.nQty    = (nQtyBal-lnQty) * -1
m.nQtyBal = 0
INSERT INTO ROLLS2 FROM MEMVAR
REPLACE nQtyBal WITH lnQty
=lfRefresh('MFMAROL2')
=lfwRollBrs() .AND. lfwAsRoll()

*!*************************************************************
*! Name      : lfvRolOk
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Roll screen
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvRolOk()
*!*************************************************************
FUNCTION lfvRolOk
SELECT ROLLS
IF lcAction = 'I'
  IF !SEEK(lcISession+lcFabric+lcColor+lcWareCode+lcDyelot+'***** N/A *****')
    INSERT INTO ROLLS ;
    (cRollItem,Color,cWareCode,Dyelot,cRollId,nQty,Trancd,cTktNo,cSession,cRSession,cIsession) ;
    VALUES (lcFabric,lcColor,lcWareCode,lcDyelot,'***** N/A *****',;
            lnIssue,'2',laData[1],lcISession,lcRSession,lcISession)
  ENDIF
  REPLACE Nqtybal WITH lnBalance
ELSE
  =SEEK(lcISession+lcFabric+lcColor+lcWareCode+lcDyelot+'***** N/A *****')
  IF lnReturn > 0
    IF nQtyBal >= lnReturn
      *E300725,1 Message : 38085
      *E300725,1 The returned quantity is not completey assigned to rolls
      *E300725,1 Would you like to deduct the unassigned returned quantity 
      *E300725,1 from the unassigned issued quantity?
      *E300725,1 Button : 38006
      *E300725,1 Yes  No
      IF gfModalGen('QRM38085B38006','ALERT') = 1
        REPLACE nQtyBal WITH nQtyBal - lnReturn ,;
                nQty    WITH nQty    - lnReturn
      ELSE
        SELECT ROLLS
        RETURN               
      ENDIF
    ELSE
      *E300725,1 Message : 38084
      *E300725,1 The returned quantity is not completey assigned to rolls
      *E300725,1 Button : 00000
      *E300725,1 Ok
      =gfModalGen('TRM38084B00000','ALERT')
      SELECT ROLLS
      RETURN
    ENDIF  
  ENDIF  
ENDIF  
IF nQtyBal = 0
  DELETE
ENDIF  
CLEAR READ

FUNCTION lfLblDesc
PARAMETERS lcLabel
RETURN('*****'+SPACE(INT((9-LEN(lcLabel))/2))+lcLabel+SPACE(CEILING((9-LEN(lcLabel))/2))+'*****')

*!*************************************************************
*! Name      : lfvCheckOrp
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Check that Item/Color has been assigned to receive to operation
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: lcToOpr : Receive to Operation
*!             lcItem  : Received Item
*!             lcColor : Received Color
*!*************************************************************
*! Returns   :  .F. When Received Item/Clor has not been assign to receive
*!                  to operation.
*!                  operation for this item
*!*************************************************************
*! Example   :  =lfvCheckOrp('000002','MDS123','')
*!*************************************************************
FUNCTION lfvCheckOrp
PARAMETERS lcToOpr,lcItem,lcColor
PRIVATE lnAlias

lnAlias = SELECT()
SELECT (lcDetFile)
=SEEK(lcItem+lcColor)
LOCATE REST WHILE Style+SClr+STR(LineNo,6)+cBomTyp+cShowType+Item+IClr+MfgCode=lcItem+lcColor ;
  FOR cShowType+Item+IClr+MfgCode = '1'+SPACE(19)+SPACE(6)+lcToOpr
SELECT (lnAlias)
IF !FOUND(lcDetFile)
  *E300725,1 Message : 38141
  *E300725,1 Item xxxxx has not been assigned to operation xxxx.
  *E300725,1 Button : 00000
  *E300725,1 Ok
  =gfModalGen('TRM38141B00000','ALERT',ALLTRIM(lcItem)+'|'+ALLTRIM(gfCodDes(lcToOpr,'MfgCode')))
  RETURN(.F.)
ENDIF

*!*************************************************************
*! Name      : lfGtSavBar
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Get Bar number in the system menu
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcBar : Bar Code
*!*************************************************************
*! Returns   :  Bar Number
*!*************************************************************
*! Example   :  =lfGtSavBar('GFCPEDIT')
*!*************************************************************
FUNCTION lfGtSavBar
PARAMETERS lcBar

USE (gcSysHome+"SYCMenu") IN 0 ORDER Pross_ID AGAIN ALIAS MenuFile
lnRetVal = IIF(SEEK(lcBar, "MenuFile"), VAL(MenuFile.cBar_Pos),0)
USE IN MenuFile
RETURN (lnRetVal)

*!*************************************************************
*! Name      : lfGetSummary
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Get Bar number in the system menu
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcLotNo : Bar Code
*!*************************************************************
*! Returns   :  Bar Number
*!*************************************************************
*! Example   :  =lfGetSummary(lcLotNo)
*!*************************************************************
FUNCTION lfGetSummary
PARAMETERS lcLotNo
PRIVATE lnAlias

lnAlias = SELECT()
SELECT (lcOprDet)
lcCurrTag = ORDER(lcOprDet)

*E302004,1 AMH Save the current record No. [Start]
PRIVATE lnRecNo
lnRecNo = RECNO()
*E302004,1 AMH [End]

SET ORDER TO TAG 'LOT'
=SEEK(lcTranType+laData[1]+lcOprCode+ALLTRIM(lcLotNo))
SUM REST IIF(TranCd='1',nLotTotQty,0),IIF(TranCd='2',nLotTotQty,0),;
         IIF(TranCd='3',nLotTotQty,0),IIF(TranCd='4',nLotTotQty,0) TO ;
         lnLotbud,lnLotRcv,lnLotCan,lnLotDmg ;
WHILE    cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
         lcTranType+laData[1]+lcOprCode+ALLTRIM(lcLotNo)

*E302004,1 AMH Save the current record No. [Start]
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF
*E302004,1 AMH [End]

SELECT (lnAlias)
SET ORDER TO TAG lcCurrTag IN (lcOprDet)
lcStatColor=IIF(lnLotbud-lnLotRcv-lnLotCan-lnLotDmg>0,"RGB(255,255,255,255,0,0)",gcObjColor)


*!*************************************************************
*! Name      : gfCPBrows             B602878,1
*! Developer : Wael Aly Mohaed
*! Date      : 05/24/1999
*! Purpose   : Validation of the browse button in the pannel
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION gfCPBrows

SELECT (lcBaseFile)
*C200080,1 AMM Consider the dye order case
*lcKey = IIF(lcTranType='I',['P'],'')
*B604056,1 HBG 12/06/2000 Fix bug variabel 'lcKey' not found [Begin]
*lcKey = IIF(lcTranType='I',IIF(EMPTY(lcDyePO),['P'],['D']) ,'')
*khm1
*lcKeyValue = IIF(lcTranType='I',IIF(EMPTY(lcDyePO),['P'],['D']) ,'')
lcKeyValue = IIF(lcTranType $ 'ID',IIF(EMPTY(lcDyePO),['P'],['D']) ,'')
*khm1
*B604056,1 [End]
*C200080,1 AMM end
*B604056,1 HBG 12/06/2000 Fix bug variabel 'lcKey' not found [Begin]
*=gfBrows(lcKey)
=gfBrows(lcKeyValue)
*B604056,1 [End]
SELECT (lcBaseFile)
*khm1
*IF lcTranType='I'
IF lcTranType $ 'ID'
*khm1

  *C200080,1 AMM Consider the dye order case
  *SET KEY TO 'P'
  SET KEY TO IIF(EMPTY(lcDyePO),'P','D')
  *C200080,1 AMM end
  =gpCtrlShow()
  
  *B605320,1 AMH Call lpshow to show the control panl in the correct status [Start]
  DO lpShow
  *B605320,1 AMH [End]
  
  SET KEY TO
ENDIF

*!*************************************************************
*! Name      : gfCPTop             B602878,1
*! Developer : Wael Aly Mohaed
*! Date      : 05/24/1999
*! Purpose   : Validation of the top button in the pannel
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION  gfCPTop

ACTIVATE WINDOW GWCCONTRL1 TOP
SELECT (lcBaseFile)
*khm1
*IF lcTranType='I'
IF lcTranType $ 'ID'
*khm1

  *C200080,1 AMM Consider the dye order case
  *SET KEY TO 'P'
  =SEEK('P')
  SET KEY TO IIF(EMPTY(lcDyePO),'P','D')
  =SEEK(IIF(EMPTY(lcDyePO),'P','D'))
  *C200080,1 AMM end
ENDIF
GO TOP
SCATTER FIELDS &lcScFields MEMO TO laData
SHOW GETS
SELECT (lcBaseFile)
*khm1
*IF lcTranType='I'
IF lcTranType $ 'ID'
*khm1
  SET KEY TO
ENDIF

*!*************************************************************
*! Name      : gfCPBttm             B602878,1
*! Developer : Wael Aly Mohaed
*! Date      : 05/24/1999
*! Purpose   : Validation of the bottom button in the pannel
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION gfCPBttm

ACTIVATE WINDOW GWCCONTRL1 TOP
SELECT (lcBaseFile)
*khm1
*IF lcTranType='I'
IF lcTranType $ 'ID'
*khm1

  *C200080,1 AMM  Consider the dye order case
  *SET KEY TO 'P'
  *=SEEK('P')
  SET KEY TO IIF(EMPTY(lcDyePO),'P','D')
  =SEEK(IIF(EMPTY(lcDyePO),'P','D'))
  *C200080,1 AMM end
ENDIF
GO BOTTOM
SCATTER FIELDS &lcScFields MEMO TO laData
SHOW GETS
SELECT (lcBaseFile)
*khm1
*IF lcTranType='I'
IF lcTranType $ 'ID'
*khm1

  SET KEY TO
ENDIF

*!*************************************************************
*! Name      : gfCPNext            B602878,1
*! Developer : Wael Aly Mohaed
*! Date      : 05/24/1999
*! Purpose   : Validation of next button in the pannel
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION gfCPNext

ACTIVATE WINDOW GWCCONTRL1 TOP
SELECT (lcBaseFile)
*khm1
*IF lcTranType='I'
IF lcTranType $ 'ID'
*khm1
  *C200080,1 AMM Consider the dye order case
  *SET KEY TO 'P'
  *=SEEK('P'+laData[1])
  SET KEY TO IIF(EMPTY(lcDyePO),'P','D')
  =SEEK(IIF(EMPTY(lcDyePO),'P','D')+laData[1])
  *C200080,1 AMM end
ENDIF
SKIP 
*C200080,1 AMM Adjust
IF EOF()
  SKIP -1
ENDIF
*C200080,1 AMM end
SCATTER FIELDS &lcScFields MEMO TO laData
SHOW GETS
SELECT (lcBaseFile)
*khm1
*IF lcTranType='I'
IF lcTranType $ 'ID'
*khm1

  SET KEY TO
ENDIF

*!*************************************************************
*! Name      : gfCPPrvis             B602878,1
*! Developer : Wael Aly Mohaed
*! Date      : 05/24/1999
*! Purpose   : Validation of the prvios button in the pannel
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION gfCPPrvis
ACTIVATE WINDOW GWCCONTRL1 TOP
SELECT (lcBaseFile)
*khm1
*IF lcTranType='I'
IF lcTranType $ 'ID'
*khm1
  *C200080,1 AMM Consider the dye order case
  *SET KEY TO 'P'
  *=SEEK('P'+laData[1])
  SET KEY TO IIF(EMPTY(lcDyePO),'P','D')
  =SEEK(IIF(EMPTY(lcDyePO),'P','D')+laData[1])
  *C200080,1 AMM end
ENDIF
SKIP -1
*C200080,1 AMM Adjust
IF BOF()
  SKIP
ENDIF
*C200080,1 AMM end
SCATTER FIELDS &lcScFields MEMO TO laData
SHOW GETS
SELECT (lcBaseFile)
*khm1
*IF lcTranType='I'
IF lcTranType $ 'ID'
*khm1
  SET KEY TO
ENDIF




*!*************************************************************
*! Name      : lfAddNewItm                          *B802378,1
*! Developer : Timour A. K.
*! Date      : 06/30/1999
*! Purpose   : Update master style and fabric files with added
*!             materials and style components.
*!*************************************************************
*! Calls From: lfvNew() , lpSavScr()
*!*************************************************************
*! Parameters:  Nome
*!*************************************************************
*! Returns   :  None
*!:************************************************************
FUNCTION lfAddNewItm

IF USED(lcTmpStyle)
  SELECT (lcTmpStyle)
  SCAN
    IF !SEEK(Style,'Style')
      SCATTER MEMVAR
      INSERT INTO 'STYLE' FROM MEMVAR
    ENDIF
    IF !SEEK(Style+laData[31]+SPACE(10),'STYDYE')
      DO gpAdStyWar WITH Style,SPACE(10),laData[31]
    ENDIF
  ENDSCAN
  ZAP
ENDIF
IF USED(lcTmpFbric)
  SELECT (lcTmpFbric)
  SCAN
    IF !SEEK(Fabric+Color,'Fabric')
      SCATTER MEMVAR
      INSERT INTO 'FABRIC' FROM MEMVAR
    ENDIF
    IF !SEEK(Fabric+Color+laData[32]+SPACE(10),'FABDYE')
      DO gpAdFabWar WITH Fabric,Color,SPACE(10),laData[32]
    ENDIF
  ENDSCAN
  ZAP
ENDIF
RETURN

*!*************************************************************
*! Name      : lfvOprSeq                          *B802700,1
*! Developer : Walid Abo El-Magd (WA)
*! Date      : 10/29/1999
*! Purpose   : Validation of the sequance field in modify screen
*!*************************************************************
*! Calls From: gfModalGen()
*!*************************************************************
*! Parameters:  Nome
*!*************************************************************
*! Returns   :  None
*!:************************************************************
FUNCTION lfvOprSeq
IF lcTranType='M' AND EMPTY(lcOperSeq) AND TYPE('lcOperSeq') # 'N'
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Sequance field can not be empty.')
  _CUROBJ = OBJNUM(lcOperSeq)
ENDIF


*C101704,1 [Sart] added
*!**************************************************************************
*! Name      : lfPicture  
*! Developer : Sameh Aldesouki 
*! Date      : 01/17/2000
*! Purpose   : Change Picture of PO .
*!**************************************************************************
FUNCTION lfPicture

IF lcTranType='I' AND llGenOrNum
  RETURN "@! XXXXXX" 
ELSE
  RETURN "X99999"
ENDIF  
*C101704,1 [End]

*!*************************************************************
*! Name      : lfDisActual             B802630,1 SSH
*! Developer : Ahmed Salah Shalaby - SSH
*! Date      : 10/04/00
*! Purpose   : Prevent actualize cutTkt/PO with only one Opr.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : = lfDisActual ()
*!*************************************************************
*B802630,1 SSH [Begin] 10/04/00 Prevent actualize cutTkt/PO with only one Opr.
FUNCTION lfDisActual

PRIVATE lnOld , lcTempKey , lcOldOpr

lnOld = SELECT(0)
lcTempKey = cimtyp+ctktno
lcOldOpr  = coprcode
SELECT mfgoprhd
=SEEK(lcTempKey+lcOldOpr)
LOCATE REST WHILE cimtyp+ctktno+coprcode = lcTempKey;
            FOR   coprcode <> lcOldOpr
IF !FOUND()

  *E302004,1 AMH Don't disable the actu. object [Start]
  *SHOW GET cbActualize DISABLE
  IF ladata[28] = 'M'
    SHOW GET cbActualize DISABLE
  ELSE
    STORE .T. TO llOnlyOne
  ENDIF
  *E302004,1 AMH [End]
  
ELSE
  SHOW GET cbActualize ENABLE
ENDIF
SELECT(lnOld)
*B802630,1 SSH 10/04/00 [End] Prevent actualize cutTkt/PO with only one Opr.

*!*************************************************************
*! Name             : lfCheckMaj   *B602779,1
*! Developer        : Ahmed Salah Shalaby - (SSH)
*! Date             : 11/04/00
*! Purpose          : Check style code structure.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : = lfCheckMaj()
*!*************************************************************
*B602779,1 SSH 09/04/00 *** Begin *** Fix the bug Variable lcSeg not found
FUNCTION lfCheckMaj

PRIVATE lnAlias
lnAlias=SELECT(0)
llStruOp=gfOpenFile(gcDataDir+'ICISTRU','Segno','SH')
IF !SEEK('U1','ICISTRU')
  IF USED('ICISTRU') AND llStruOp
    USE IN ICISTRU
  ENDIF
  SELECT (lnAlias)
  RETURN .F.
ENDIF
SELECT (lnAlias)
*B602779,1 SSH 09/04/00 *** End ***

*!*************************************************************
*! Name      : lfPwShtItm
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 07/07/2000
*! Purpose   : Create detail oeration cost sheet
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfPwShtItm()
*!*************************************************************
*E301427,1 SSH Update Pw files.[Start]
FUNCTION lfPwShtItm
PARAMETER lcNewGen , llNewItm
*--- first parameter if we are generation new cost sheet 
*--- Second one if we are calling to add new item
PRIVATE lnOldAls , lcPwStyle , lcMainMfg , lcOldAsign
lnOldAls = SELECT(0)
lcOldAsign = ''
IF !llNewItm
  lcPwBom = 'Dummy'
ELSE
  PWBOMTAG = gfTempName()
  SELECT (lcTmpBOM)
  INDEX ON citmmajor+typ+citmmask+mfgcode+item+iclr TAG &PWBOMTAG
  SET ORDER TO (PWBOMTAG)
ENDIF
=lfPWTmpFil()
*--- SSH This function will update 'PWCTKTBOM' temp file for detail oper.
*--- SSH using the following key [cuttkt+mfgcode+coprcode+STR(nlineno,6)]
*--- SSH The updates will be as following :
*--- SSH Each detail operation will have its owne record in PWCTKTBOM file
*--- SSH even if the Main Mfg Code assigned more than onece 
*--- SSH (It will has only one record in BOMLINE file with average cost)
*--- SSH but it will has record for each detail operation.
*--- SSH i.e. MfgCode1+Det1 maybe repeate in PWCTKTBOM but with deferant
*--- SSH Line no. while MfgCode1 hase only one record in BOMLIN fiel.
*--- SSH we are going to use PWBOM file to update this temp.
IF llNewItm
  lcNewMfg = gfTempName()
  SELECT (lcTmpBom)
  SELECT DISTINCT mfgCode FROM (lcTmpBom) INTO DBF (gcWorkDir+lcNewMfg)
  SELECT(lcNewMfg)
  INDEX ON mfgCode TAG (lcNewMfg)
  SET ORDER TO CTKTBOM IN (lcTktSheet)  
ENDIF
IF laData[3]<>'H'
  SELECT BOMLINE
  *--- cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode
  =SEEK("M1"+laData[1])
ELSE
  SELECT (lcDetFile)
  GOTO TOP
ENDIF
*--- SCAN For Manufacturing Operation
SCAN REST WHILE IIF(laData[3]<>'H' .AND. !llAutoGen,cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode="M1"+laData[1],.T.);
          FOR   cCatgTyp = 'M' .AND. SEEK(mfgcode , 'pwoperat') .AND.;
                IIF(llNewItm,SEEK(mfgcode,lcNewMfg),IIF(!llAutoGen .AND. laData[3]<>'H',(cShowType = '1'),.T.))
  lcPwStyle = PADR(SUBSTR(Style,1,LEN(lcMjrMsk)),19) 
  lcCtkt    = laData[1]
  lcMainMfg = MfgCode
  lcTmpLin  = ''
  lcANonMjr = SUBSTR(Style,1,LEN(lcMjrMsk))+SUBSTR(lcItmMsk,LEN(lcMjrMsk)+1)
  lcANonMjr = STRTRAN(lcANonMjr,"X","*")
  lcStyClr  = Style
  
  *B606616,1 KHM 11/19/2002 (Begin) Getting the correct major and non major segments.
  IF llExtSizSc 
    *Case style - ****** Scl
    lcExNnMjr1 = STUFF(Style, lnClrPos, LEN(lcClrMsk), REPLICATE("*",LEN(lcClrMsk)))

    *Case Style - ****** ***
    lcExNnMjr2 = STUFF(lcExNnMjr1 , lnSizePos, lnSizeLen,"***")

    * Case style - Color ***
    lcExNnMjr3 = STUFF(Style, lnSizePos, lnSizeLen,"***")
    
  ENDIF    
  *B606616,1 KHM 11/19/2002 (End)
  
  *--- citmmajor+typ+citmmask+mfgcode+item+iclr
  IF llNewItm
    SELECT BOM
    lcCostTyp = IIF(SEEK("M",lcTktSheet),&lcTktSheet..Typ,'')
    IF SEEK(lcPwStyle+lcCostTyp+lcStyClr+lcMainMfg)
      SCAN REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr=;
                      lcPwStyle+lcCostTyp+lcStyClr+lcMainMfg
        lcOldAsign = lcOldAsign+ALLTRIM(STR(nLineNo))
      ENDSCAN
    ENDIF
    IF SEEK(lcPwStyle+lcCostTyp+lcANonMjr+lcMainMfg)
      SCAN REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr=;
                      lcPwStyle+lcCostTyp+lcANonMjr+lcMainMfg
        lcOldAsign = lcOldAsign+ALLTRIM(STR(nLineNo))
      ENDSCAN
    ENDIF
    SELECT (lcTmpBOM)
  ELSE
    SELECT BOM
  ENDIF
  *--- citmmajor+typ+citmmask+mfgcode+item+iclr
  lcCostTyp = IIF(SEEK("M",lcTktSheet),&lcTktSheet..Typ,'')
  
  IF SEEK(lcPwStyle+lcCostTyp+lcStyClr+lcMainMfg)
    SCAN REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr=;
                    lcPwStyle+lcCostTyp+lcStyClr+lcMainMfg
      lcTmpLin = lcTmpLin+ALLTRIM(STR(nLineNo))
    ENDSCAN
  ENDIF
  IF SEEK(lcPwStyle+lcCostTyp+lcANonMjr+lcMainMfg)
    SCAN REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr=;
                    lcPwStyle+lcCostTyp+lcANonMjr+lcMainMfg
      lcTmpLin = lcTmpLin+ALLTRIM(STR(nLineNo))
    ENDSCAN
  ENDIF  

  *B606616,1 KHM 11/19/2002 (Begin) Check the existance of the record by using the correct values
  IF llExtSizSc 
  
    *Case style - ****** Scl
    IF SEEK(lcPwStyle+lcCostTyp+lcExNnMjr1+lcMainMfg)
      SCAN REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr=;
                      lcPwStyle+lcCostTyp+lcExNnMjr1+lcMainMfg
        lcTmpLin = lcTmpLin+ALLTRIM(STR(nLineNo))
      ENDSCAN
    ENDIF
  
    *Case Style - ****** ***
    IF SEEK(lcPwStyle+lcCostTyp+lcExNnMjr2+lcMainMfg)
      SCAN REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr=;
                      lcPwStyle+lcCostTyp+lcExNnMjr2+lcMainMfg
        lcTmpLin = lcTmpLin+ALLTRIM(STR(nLineNo))
      ENDSCAN
    ENDIF

    * Case style - Color ***
    IF SEEK(lcPwStyle+lcCostTyp+lcExNnMjr3+lcMainMfg)
      SCAN REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr=;
                      lcPwStyle+lcCostTyp+lcExNnMjr3+lcMainMfg
        lcTmpLin = lcTmpLin+ALLTRIM(STR(nLineNo))
      ENDSCAN
    ENDIF
  ENDIF  
  *B606616,1 KHM 11/19/2002 (End)
  
  *B606616,1 KHM 11/19/2002 (End)
  
  lcTmpLin = IIF(EMPTY(lcTmpLin),'1',lcTmpLin)
  

  *--- citmmajor+mfgcode+coprcode+STR(nlineno,6)
  IF llNewItm
    SELECT (lcPwBom)
    SET FILTER TO EMPTY(cStatus)
  ELSE
    SELECT PWBOM
  ENDIF
  IF SEEK(lcPwStyle + lcMainMfg)
    SCAN REST WHILE citmmajor+mfgcode+coprcode+STR(nlineno,6)=;
                    lcPwStyle + lcMainMfg;
              FOR   ALLTRIM(STR(nLineNo)) $ lcTmpLin
      IF laData[3]<>'H' .OR. llAutoGen
        SELECT PWCtkBom
      ELSE
        SELECT(lcPWCtkBom)
      ENDIF
      lcTCrsRef = lfGnCrsRef()
      APPEND BLANK
      REPLACE CutTkt      WITH lcCtkt,;
              Style       WITH IIF(laData[3]<>'H'  .AND. !llAutoGen, BOMLINE.Style, &lcDetFile..Style),;
              cSizes      WITH IIF(laData[3]<>'H'  .AND. !llAutoGen, BOMLINE.cSizes, &lcDetFile..cSizes),;
              mfgCode     WITH lcMainMfg,;
              cOprCode    WITH IIF(llNewItm , &lcPwBom..cOprCode  ,PWBOM.cOprCode),;
              cOperSeq    WITH IIF(llNewItm , &lcPWBOM..cOperSeq  ,PWBOM.cOperSeq),;
              cOprAtType  WITH IIF(llNewItm , &lcPWBOM..cOprAtType,PWBOM.cOprAtType),;
              nOperAtPer  WITH IIF(llNewItm , &lcPWBOM..nOperAtPer,PWBOM.nOperAtPer),;
              nOperCost   WITH IIF(llNewItm , &lcPWBOM..nOperCost ,PWBOM.nOperCost),;
              cOverType   WITH "Q",;
              nOverUnit   WITH 0,;
              nLineNo     WITH IIF(llNewItm, &lcPWBOM..nLineNo , PWBOM.nLineNo),;
              NREPNO      WITH LEN(lcTmpLin) +LEN(lcOldAsign) ,;
              nGenNo      WITH lcTCrsRef
      IF laData[3]='H' .AND. !llAutoGen
        REPLACE  cStatus  WITH lcNewGen
      ENDIF
    ENDSCAN
  ENDIF
ENDSCAN
IF llNewItm
  ERASE (gcWorkDir+PWBOMTAG+".CDX")
  USE IN IIF(USED(lcNewMfg),lcNewMfg,0)
  ERASE (gcWorkDir+lcNewMfg+".DBF")
  ERASE (gcWorkDir+lcNewMfg+".CDX")
  SELECT(lcTmpBom)
  SET ORDER TO (lcTmpBom)
ENDIF
IF laData[3]<>'H'  .AND. !llAutoGen
  SET ORDER TO (lcTktSheet) IN (lcTktSheet)
ENDIF
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfPWTmpFil
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 07/07/2000
*! Purpose   : Create detail Operaiton temp files.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfPWTmpFil()
*!*************************************************************
*E301427,1 SSH 
FUNCTION lfPWTmpFil
PRIVATE lnOldAls
=gfOpenFile(gcDataDir+"pwoperat" , gcDataDir+"pwoperat" , "SH")
=gfOpenFile(gcDataDir+'PWcTkBom',gcDataDir+'PWcTkBom','SH')
=gfOpenFile(gcDataDir+'PWBOM',gcDataDir+'PWBOM','SH')
lnOldAls = SELECT(0)
IF !USED(lcPWCtkBom)
  SELECT PWcTkBom
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+1,4]
  laFileStru[lnFileStru+1,1] = 'cStatus'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 1
  laFileStru[lnFileStru+1,4] = 0
  CREATE TABLE (gcWorkDir+lcPWCtkBom) FROM ARRAY laFileStru
  INDEX ON cuttkt+mfgcode+coprcode+Style+STR(nlineno,6) TAG (lcPWCtkBom)
  INDEX ON nGenNo TAG nGenNo
ENDIF
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfPwUpdate
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 07/07/2000
*! Purpose   : Update detail Operaiton Master files.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfPwUpdate()
*!*************************************************************
*E301427,1 SSH 
FUNCTION lfPwUpdate
PRIVATE lnOldAls

lnOldAls = SELECT(0)
SELECT (lcPWCtkBom)
GOTO TOP
SCAN
  SCATTER MEMVAR MEMO
  DO CASE
    CASE  cStatus = 'A'
      INSERT INTO PWCtkBom FROM MEMVAR
    CASE  cStatus = 'M'
      *--- cuttkt+mfgcode+coprcode+STR(nlineno,6)
      IF SEEK(m.cuttkt+m.mfgcode+m.coprcode+m.Style+STR(m.nlineno,6),PWCtkBom)
        SELECT PWCtkBom
        GATHER MEMVAR MEMO
      ENDIF
  ENDCASE
ENDSCAN
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfPwRem
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 07/07/2000
*! Purpose   : Function to remove detail oeration.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfPwRem()
*!*************************************************************
*E301427,1 SSH 
FUNCTION lfPwRem
PARAMETER lcCTkt , lcMainOpr
PRIVATE lnOldAls , llPwClsBom

*--- cuttkt+mfgcode+coprcode+STR(nlineno,6)
lnOldAls = SELECT(0)
llPwClsBom = IIF(!USED('PwCtkBom'),gfOpenFile(gcDataDir+'PWcTkBom',gcDataDir+'PWcTkBom','SH'),.F.)
IF  laData[3]<>'H'
  SELECT PwCtkBom
ELSE
  SELECT(lcPwCtkBom)
ENDIF
IF SEEK( lcCTkt + lcMainOpr)
  DELETE REST WHILE  cuttkt+mfgcode+coprcode+STR(nlineno,6)=;
                     lcCTkt + lcMainOpr
  FLUSH
ENDIF
SELECT(lnOldAls)
USE IN IIF(llPwClsBom,'PwCtkBom',0)

*!*************************************************************
*! Name      : lfDisCost
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 07/07/2000
*! Purpose   : Function to Desable cost if detail oeratin exist.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfDisCost()
*!*************************************************************
*E301427,1 SSH 
FUNCTION lfDisCost

PRIVATE lnOldAls
*B603867,4 SSH 31/08/2000 [Start Commented Out
IF llPwInst
*B603867,4 SSH END]
  lnOldAls = SELECT(0)
  =gfOpenFile(gcDataDir+"pwoperat" , gcDataDir+"pwoperat" , "SH")
  IF llPWInst .AND. lcTranType = "M" .AND. SEEK(&lcTktSheet..mfgCode,'pwoperat')
    SHOW GET m.UntCost DISABLE
  ELSE
    SHOW GET m.UntCost ENABLE
  ENDIF
  USE IN pwoperat
  SELECT(lnOldAls)
*B603867,4 SSH 31/08/2000 [Start Commented Out
ENDIF
*B603867,4 SSH END]
*!*************************************************************
*! Name      : lfOpnStBrw
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 07/07/2000
*! Purpose   : Function to check if we are going to open style browse.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfOpnStBrw()
*!*************************************************************
*E301427,1 SSH 
FUNCTION lfOpnStBrw
PARAMETER lcMainMfg

PRIVATE lnOldAls , llToReturn , lcTStyClr , lnTmpCost , lcTempSizs
lnOldAls = SELECT(0)
=gfOpenFile(gcDataDir+'PWcTkBom',gcDataDir+'PWcTkBom','SH')
llToReturn = .F.
IF laScrMode[2]  && Direct Update Master File
  SELECT PWCTKBOM
ELSE
  SELECT (lcPWCTKBOM)
ENDIF
*--- cuttkt+mfgcode+coprcode+STR(nlineno,6)
IF SEEK(laData[1] + lcMainMfg )
  DO WHILE cuttkt+mfgcode+coprcode+STR(nlineno,6) = laData[1] + lcMainMfg;
    .AND. !llToReturn
    lcTStyClr  = Style
    lnTmpCost  = (nOperCost/nOperAtPer)
    lcTempSizs = cSizes
    lcTempDet  = coprcode
    SCAN REST WHILE cuttkt+mfgcode+coprcode+STR(nlineno,6) = laData[1] + lcMainMfg;
              FOR !llToReturn
      WAIT WINDOW "Checking Detail Operation for Operation : "+mfgcode+" "+lcItmHdr+" "+Style +" Please Wait.!" NOWAIT
      IF Style  <> lcTStyClr
        llToReturn = .T.
      ELSE
        IF  (nOperCost/nOperAtPer) <> lnTmpCost .OR. ;
           cSizes <> lcTempSizs .OR. coprcode <> lcTempDet
          llToReturn = .T.
        ENDIF
      ENDIF
    ENDSCAN
  ENDDO
ENDIF
SELECT(lnOldAls)
IF !llToReturn
  WAIT WINDOW "No deferances in detail operation found !" NOWAIT
ENDIF
RETURN llToReturn

*!*************************************************************
*! Name      : lfvSelDet
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 07/07/2000
*! Purpose   : Valid Function detail operation button.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvSelDet()
*!*************************************************************
*E301427,1 SSH 
FUNCTION lfvSelDet

PRIVATE lnOldAls , lnNwAveCst , lcCurStyle , m.Desc , m.UnitCost , m.Unit
lnOldAls = SELECT(0)
=lfCreDTmp()
lnNwAveCst = 0
lcCurStyle = Style
*--- These PARAMETER lcParntOpr , lcDOprTmp , lcCostVar
*--- Are use in calling detail operation routing screen.
*--- lcParntOpr == Main MfgCode
*--- lcDOprTmp  == Detail Operation Assigned on the Main Mfg.
*--- lcCostVar  == Variable to hold the calculated new average cost.
IF laData[3]<>'H'
  SELECT PwcTkBom
ELSE
  SELECT (lcPwcTkBom)
ENDIF
*--- cuttkt+mfgcode+coprcode+STR(nlineno,6)
IF SEEK(laData[1] + &lcTktSheet..mfgCode )
  SCAN REST WHILE cuttkt+mfgcode+coprcode+Style+STR(nlineno,6)=;
                  laData[1] + &lcTktSheet..mfgCode;
            FOR Style = lcCurStyle
    SCATTER MEMVAR MEMO
    lnNREPNO  = m.NREPNO    
    WAIT WINDOW "Selecting records for operation :"+&lcTktSheet..mfgCode+" Detail operation : "+cOprCode NOWAIT
    =SEEK(&lcTktSheet..mfgCode+cOprCode,'PwOperat')
    m.MfgCode  = &lcTktSheet..mfgCode
    m.ChildOpr = cOprCode
    m.Desc     = PwOperat.cDescRip
    m.WorkCent = PwOperat.cWorkCent
    m.SetUpTim = PwOperat.NSetup_Tim
    m.PersTim  = PwOperat.NEmp_Time
    m.MatchTim = PwOperat.NMach_Time
    m.Unit     = PwOperat.nUnit
    m.Seq      = cOperSeq
    m.RatType  = cOprAtType
    m.RatePer  = nOperatPer
    m.UnitCost = nOperCost
    INSERT INTO (lcDetLin) FROM MEMVAR
  ENDSCAN

  *B605726,1 KHM 03/27/2002 (Begin) Changing the index to be by sequence
  SET ORDER TO TAG (lcDetLin2) IN (lcDetLin)
  *B605726,1 KHM 03/27/2002 (End)

  PUSH KEY
  ON KEY
  DO (gcAppHome+"MFDetOpr") WITH m.MfgCode , lcDetLin , "lnNwAveCst",.F.,.F.,.T.
  lnNwAveCst = (lnNwAveCst/lnNREPNO)
  POP KEY

  *B605726,1 KHM 03/27/2002 (Begin) Restoring the old index.
  SET ORDER TO TAG (lcDetLin) IN (lcDetLin)
  *B605726,1 KHM 03/27/2002 (End)

ENDIF
SELECT(lnOldAls)
RETURN  lnNwAveCst


*!*************************************************************
*! Name      : lfCreDTmp
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 07/07/2000
*! Purpose   : Function to create detail oeration screen temp file.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfCreDTmp()
*!*************************************************************
*E301427,1 SSH 
FUNCTION lfCreDTmp
PRIVATE lnOldAls

lnOldAls = SELECT(0)
=gfOpenFile(gcDataDir+"pwoperat" , gcDataDir+"pwoperat" , "SH")
IF USED(lcDetLin)
  SELECT(lcDetLin)
  ZAP
  USE IN (lcDetLin)
ENDIF
CREATE TABLE &gcWorkDir.&lcDetLin (ChildOpr C(6),Desc C(30),WorkCent C(02),;
                                   SetUpTim N(6),PersTim N(6),MatchTim N(6),;
                                   Unit N(6),Seq C(2),RatType C(6),;
                                   RatePer N(3), UnitCost N(9,2),Tool C(2),MfgCode C(6),cStatus C(1) , nLineNo N(4) , cOverType C(1) , nOverUnit N(7))
*B605726,1 KHM 03/27/2002 (Begin) Adding a new index
INDEX ON MfgCode+Seq+ChildOpr TAG &lcDetLin2
*B605726,1 KHM 03/27/2002 (End)
INDEX ON MfgCode+ChildOpr TAG &lcDetLin
SELECT(lnOldAls)



*!*************************************************************
*! Name      : lfGnCrsRef
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 07/07/2000
*! Purpose   : Function to generate cross referance no.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  =lfGnCrsRef()
*!*************************************************************
*E301427,1 SSH 
FUNCTION lfGnCrsRef
PRIVATE lnOldAls , lcNewGenNo

lnOldAls = SELECT(0)
SELECT PwcTkBom
SET ORDER TO NGenNo
GO BOTTOM
lcNewGenNo  = NGenNo
SELECT (lcPwcTkBom)
SET ORDER TO NGenNo
GO BOTTOM
lcNewGenNo = MAX(NGenNo , lcNewGenNo)
lcNewGenNo = PADL(ALLTRIM(STR(VAL(lcNewGenNo)+1)),10,'0')
DO WHILE SEEK(lcNewGenNo,'PwcTkBom') OR SEEK(lcNewGenNo,lcPwcTkBom)
  lcNewGenNo = PADL(ALLTRIM(STR(VAL(lcNewGenNo)+1)),10,'0')
ENDDO
SELECT PwcTkBom
SET ORDER TO PwcTkBom
SELECT (lcPwcTkBom)
SET ORDER TO (lcPwcTkBom)
SELECT(lnOldAls)
RETURN lcNewGenNo

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
*! Name      : lfwCutWhen
*! Developer : Ahmed Maher (AMH)
*! Date      : 01/09/2001
*! Purpose   : When function of User defined fields of Chathy Daniels.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfwCutWhen()
*!**************************************************************************
FUNCTION lfwCutWhen

RETURN .F.
*-- end of lfwCutWhen.
*C102094,1 AMH [End]

*!**************************************************************************
*! Name      : lfRecCost
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 04/30/2001
*! Purpose   : To recalculate the duty when its a percentage.
*!**************************************************************************
*! Example   :  =lfRecCost()
*!**************************************************************************
*B604294,1 KHM 04/30/2001 
*!**************************************************************************
FUNCTION lfRecCost
PARAMETERS lcimtyp,lctktno,lcDetFile,lcTktSheet, lnAlias
PRIVATE lnAlias,lnPoRecNo
lnAlias = SELECT()

lcLinOrd = ORDER(lcDetFile)
lcHdrOrd = ORDER(lcTktSheet)
SET ORDER TO TAG Ctktbom  IN (lcTktSheet)
SET ORDER TO TAG BomLine  IN (lcDetFile)

SELECT POSLN
lnPoRecNo = RECNO()

=SEEK('P'+laData[1])

*B804351,1 AMH Comment the next line [Start]
*lnItemAmt = 0
*B804351,1 AMH [End]

DO WHILE cstytype+po+style+STR(lineno,6)+trancd = 'P'+laData[1]
  lcStyle = Style
  *B608125,1 TMI [Start] add the line # to the scan
  *SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = 'P'+laData[1]+lcStyle 
  *ENDSCAN
  PRIVATE lcLineNo
  lcLineNo = STR(POSLN.LINENO,6)
  SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = 'P'+laData[1]+lcStyle+lcLineNo
  ENDSCAN
  *B608125,1 TMI [End  ] 
 
  SELECT (lcDetFile)
  *B608125,1 TMI [Start] add the lcLineNo to the sum command
  *=SEEK(lcimtyp+'1'+lctktno)
  *SUM REST ItemAmt TO lnDutyAmt WHILE ;
  *cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=lcimtyp+'1'+lctktno ;
  *FOR cCOstStat = '1' AND Style = lcStyle  
  =SEEK(lcimtyp+'1'+lctktno+lcLineNo)
  SUM REST ItemAmt TO lnDutyAmt WHILE ;
  cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=lcimtyp+'1'+lctktno+lcLineNo ;
  FOR cCostStat = '1' AND Style = lcStyle
  *B608125,1 TMI [End  ] 
  
  *C037960,1 MHM Alena Trigger to sum cCostTyps 1,3 as alena new logic[Begin]
  IF ASCAN(laEvntTrig , PADR('ALEPOSUM',10)) <> 0
    =gfDoTriger('POCSSH',PADR('ALEPOSUM',10)) 
  ENDIF  
  *C037960,1  [End]

  *B804351,1 AMH Comment the following lines and add cCatgTyp = 'D' to the scan [Start]
  *SELECT (lcTktSheet)
  *=SEEK(lcimtyp+lctktno)
  *SCAN REST WHILE cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot = lcimtyp+lctktno ;
  *          FOR  cCatgTyp = 'D'
  *  laData[VAL(Typ)+11] = 0
  
  SELECT (lcDetFile)
  *B608125,1 TMI [Start] add the line # to the seek
  *=SEEK(lcimtyp+'1'+lctktno)
  =SEEK(lcimtyp+'1'+lctktno+lcLineNo)
  *B608125,1 TMI [End  ] 

  *SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode = ;
                    lcimtyp+'1'+lctktno ;
              FOR   cbomtyp = &lcTktSheet..typ AND Style = lcStyle
  *B608125,1 TMI [Start] add the lcLineno to the scan loop
  *SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode = ;
                    lcimtyp+'1'+lctktno ;
              FOR   cCatgTyp = 'D' AND Style = lcStyle
  SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode = ;
                    lcimtyp+'1'+lctktno+lcLineNo ;
              FOR   cCatgTyp = 'D' AND Style = lcStyle
  *B608125,1 TMI [End  ] 
  *B804351,1 AMH [End]
    IF nPercent > 0
        REPLACE ItemAmt  WITH nPercent*lnDutyAmt/100 ,;
                UnitCost WITH IIF(ItemQTy=0,0,ItemAmt/ItemQTy)
    ENDIF
    *B804351,1 AMH Comment the following line [Start]
    *lnItemAmt = lnItemAmt + ItemAmt
    *B804351,1 AMH [End]
  ENDSCAN
  *B804351,1 AMH Comment the following line [Start]
  *SELECT (lcTktSheet)
  *REPLACE Est_Cost WITH lnItemAmt ,;
  *        UntCost  WITH IIF(Req_Qty =0,0,Est_Cost/Req_Qty)
  *laData[VAL(Typ)+11] = laData[VAL(Typ)+11] + Est_Cost
  *ENDSCAN
  *B804351,1 AMH [End]
  SELECT POSLN
ENDDO

*B804351,1 AMH Add the following code to re-calculate the est_cost and
*B804351,1          the duty cost elements
SELECT (lcTktSheet)
=SEEK(lcimtyp+lctktno)
SCAN REST WHILE cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot = lcimtyp+lctktno ;
          FOR  cCatgTyp = 'D'
  lnItemAmt = 0
  laData[VAL(Typ)+11] = 0
  SELECT (lcDetFile)
  =SEEK(lcimtyp+'1'+lctktno)
  SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode = ;
                  lcimtyp+'1'+lctktno ;
            FOR   cbomtyp = &lcTktSheet..typ
    lnItemAmt = lnItemAmt + ItemAmt
  ENDSCAN
  SELECT (lcTktSheet)
  REPLACE Est_Cost WITH lnItemAmt ,;
          UntCost  WITH IIF(Req_Qty =0,0,Est_Cost/Req_Qty)
  laData[VAL(Typ)+11] = laData[VAL(Typ)+11] + Est_Cost
ENDSCAN
*B804351,1 AMH [End]

IF BETWEEN(lnPoRecNo,1,RECCOUNT('POSLN'))
  GOTO lnPoRecNo IN PosLn
ENDIF

SELECT (lcTktSheet)
SET ORDER TO &lcHdrOrd
LOCATE
SELECT (lcDetFile)
SET ORDER TO &lcLinOrd
LOCATE

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfwMarker
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/07/2001
*! Purpose   : When function of Marker field.
*!*************************************************************
*! Example   : = lfwMarker()
*!*************************************************************
*! B604623,1 AMH 07/05/2001.
*!*************************************************************
*!*B604433,1 AMH
FUNCTION lfwMarker

IF TYPE('laAStySeg') # 'U'
  IF !EMPTY(laAStySeg[1,1])
    lcOldMrker = lcMarker7
  ELSE
    lcOldMrker= lcMarker6
  ENDIF
ELSE
  lcOldMrker = m.cMarker
ENDIF
*--end of lfwMarker.

*!*************************************************************
*! Name      : lfvMarker
*! Developer : Ahmed Maher (AMH)
*! Date      : 01/10/2001
*! Purpose   : Valid function of Marker field.
*!*************************************************************
*! Example   : = lfvMarker()
*!*************************************************************
*! B604623,1 AMH 07/05/2001.
*!*************************************************************
*!*B604623,1 AMH
FUNCTION lfvMarker
PARAMETERS llFromEdit

PRIVATE lnAlias, lcFile_Ttl, lcOrder, lcOldBrwFl, lcPattern
DIMENSION laTempData[1]
STORE '' TO laTempData, lcPattern

lnAlias = SELECT(0)
IF !llFromEdit
  IF !EMPTY(laAStySeg[1,1])
    llReturn = (lcOldMrker == lcMarker7)
  ELSE
    llReturn = (lcOldMrker == lcMarker6)
  ENDIF
ELSE
  llReturn = (lcOldMrker == m.cMarker)
ENDIF

IF FILE(gcDataDir+"PDMMARK"+".DBF") .AND. !llReturn
  =gfOpenFile(gcDataDir+"PDMMARK" , gcDataDir+"CFABRIC" , "SH")
ELSE
  RETURN
ENDIF

SELECT STYLE
lcOrder = SET('ORDER')
SET ORDER TO TAG CSTYLE
SEEK(PADR(laData[2],19))
SET ORDER TO &lcOrder

SELECT PDMMARK
IF !llFromEdit
  =SEEK(PADR(lcNFabItem,7))
ELSE
  =SEEK(SUBSTR(m.Item,1,7))
ENDIF

lcOldBrwFl = lcBrFields

lcFile_Ttl = 'Markers'

*B604861,1 AMH Don't browse net unit & wastage % [Start]
*lcBrFields = "CMARK_ID                    :H='Marker'     ,"+;
             "CMARK_DESC                  :H='Description',"+;
             "CPATTERN = ALLTRIM(CPATT_NO)+ALLTRIM(CREV_NO) :H='Pattern# ' ,"+;
             "NTOTGOODS                   :H='Net Unit'   ,"+;
             "NLOSS                       :H='Wastage %'  ,"+;
             "NTOTQTY                     :H='Gross Unit'"
lcBrFields = "CMARK_ID                    :H='Marker'     ,"+;
             "CMARK_DESC                  :H='Description',"+;
             "CPATTERN = ALLTRIM(CPATT_NO)+ALLTRIM(CREV_NO) :H='Pattern# ' ,"+;
             "NTOTQTY                     :H='Gross Unit'"
*B604861,1 AMH [End]

IF !llFromEdit
  IF !EMPTY(laAStySeg[1,1])
    m.cMarker = lcMarker7
  ELSE
    m.cMarker = lcMarker6
  ENDIF
ENDIF

IF !EMPTY(m.cMarker)
  IF EOF()
    =gfModalGen("INM000000B00000","DIALOG",'','',"No markers have been defined for this fabric.")
    IF !llFromEdit
      IF !EMPTY(laAStySeg[1,1])
        lcMarker7 = ''
        SHOW GET lcMarker7
      ELSE
        lcMarker6 = ''
        SHOW GET lcMarker6
      ENDIF
    ENDIF
  ELSE
    *B604861,1 AMH Seek in pattern field with padr of 10 [Start]
    *=SEEK(IIF(!llFromEdit,PADR(lcNFabItem,7),SUBSTR(m.Item,1,7))+;
          PADR(SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),8)+;
          RIGHT(ALLTRIM(STYLE.PATTERN),2))
    =SEEK(IIF(!llFromEdit,PADR(lcNFabItem,7),SUBSTR(m.Item,1,7))+;
          PADR(SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),10)+;
          RIGHT(ALLTRIM(STYLE.PATTERN),2))
    *B604861,1 AMH [End]
    IF EOF()
      =AriaBrow([IIF(!llFromEdit,lcNFabItem,SUBSTR(m.Item,1,7))],;
                lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,.F.,.F.,'CMARK_ID','laTempData')
      m.cMarker = IIF(!llFromEdit,laTempData[1],;
                 IIF(EMPTY(laTempData[1]),lcOldMrker,laTempData[1]))
      lcPattern = PdmMark.cPatt_No+PdmMark.cRev_No
    ELSE
      *B604861,1 AMH Seek in pattern field with padr of 10 [Start]
      *IF !SEEK(IIF(!llFromEdit,PADR(lcNFabItem,7),SUBSTR(m.Item,1,7))+;
          PADR(SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),8)+;
          RIGHT(ALLTRIM(STYLE.PATTERN),2)+m.cMarker)
      IF !SEEK(IIF(!llFromEdit,PADR(lcNFabItem,7),SUBSTR(m.Item,1,7))+;
          PADR(SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),10)+;
          RIGHT(ALLTRIM(STYLE.PATTERN),2)+m.cMarker)
        *=AriaBrow([IIF(!llFromEdit,lcNFabItem,SUBSTR(m.Item,1,7))+;
                  PADR(SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),8)+;
                  RIGHT(ALLTRIM(STYLE.PATTERN),2)],;
                  lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,.F.,.F.,'CMARK_ID','laTempData')
        =AriaBrow([IIF(!llFromEdit,lcNFabItem,SUBSTR(m.Item,1,7))+;
                  PADR(SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),10)+;
                  RIGHT(ALLTRIM(STYLE.PATTERN),2)],;
                  lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,.F.,.F.,'CMARK_ID','laTempData')
      *B604861,1 AMH [End]
        m.cMarker = IIF(!llFromEdit,laTempData[1],;
                   IIF(EMPTY(laTempData[1]),lcOldMrker,laTempData[1]))
        lcPattern = PdmMark.cPatt_No+PdmMark.cRev_No
      ELSE
        lcPattern = PdmMark.cPatt_No+PdmMark.cRev_No
      ENDIF
    ENDIF
  ENDIF
ENDIF

lcBrFields = lcOldBrwFl 

IF !llFromEdit
  IF !EMPTY(laAStySeg[1,1])
    lcMarker7 = m.cMarker
  ELSE
    lcMarker6 = m.cMarker
  ENDIF
ENDIF

IF !EMPTY(m.cMarker) .AND. !EOF('PDMMARK') .AND. !llFromEdit .AND.;
   (PDMMARK.NTOTGOODS<>0 .OR. PDMMARK.NLOSS<>0 .OR. PDMMARK.NTOTQTY<>0) .AND.;
   gfModalGen("QRM00000B00006","DIALOG",'','',"Do you want to update the units from marker data ?")=1

  IF !EMPTY(laAStySeg[1,1])
    *B604861,1 AMH Update wastage with zero [Start]
    *lnNEstUnt7 = PDMMARK.NTOTGOODS
    *lnNWstg7   = PDMMARK.NLOSS
    lnNEstUnt7 = PDMMARK.NTOTQTY
    lnNWstg7   = 0
    *B604861,1 AMH [End]
    lnNUntQty7 = PDMMARK.NTOTQTY
    SHOW GET lnNEstUnt7
    SHOW GET lnNWstg7
    SHOW GET lnNUntQty7
    =lfvNUntQty('7')
  ELSE
    *B604861,1 AMH Update wastage with zero [Start]
    *STORE PDMMARK.NTOTGOODS TO lnNEsUnt6B, lnNEsUnt6A
    *STORE PDMMARK.NLOSS     TO lnNWstg6
    STORE PDMMARK.NTOTQTY   TO lnNEsUnt6B, lnNEsUnt6A
    STORE 0                 TO lnNWstg6
    *B604861,1 AMH [End]
    STORE PDMMARK.NTOTQTY   TO lnNUntQt6B, lnNUntQt6A
    SHOW GET lnNEsUnt6B
    SHOW GET lnNEsUnt6A
    SHOW GET lnNWstg6
    SHOW GET lnNUntQt6B
    SHOW GET lnNUntQt6A
    =lfvNUntQty('6')
  ENDIF
  *B604899,1 AMH Update custom fields for Cathy Daniels from PDMMARK file [Start]
  IF ASCAN(laEvntTrig , PADR('UPDBOMCA',10)) <> 0
    =gfDoTriger('MFCSSH',PADR('UPDBOMCA',10))
  ENDIF
  *B604899,1 AMH [End]
ENDIF
*B604899,1 AMH Update custom fields for Cathy Daniels from PDMMARK file [Start]
IF EMPTY(m.cMarker) .AND. ASCAN(laEvntTrig , PADR('UPDBOMCA',10)) <> 0
  =SEEK(SPACE(27),'PDMMARK')
  =gfDoTriger('MFCSSH',PADR('UPDBOMCA',10))
ENDIF
*B604899,1 AMH [End]

=gfCloseFile('PDMMARK')
SELECT (lnAlias)
*-- end of lfvMarker.

*!**************************************************************************
*! Name      : lfUpdEstCst
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/14/2001
*! Purpose   : To recalculate the estimated costs in for PO,CT and MPO
*!**************************************************************************
*! Example   :  =lfUpdEstCst()
*!**************************************************************************
*!B604790,1 AMH 08/14/2001 
*!**************************************************************************
FUNCTION lfUpdEstCst

SET ORDER TO TAG BOMLINE IN BOMLINE
IF SEEK(lcTranType+'1'+laData[1],'BOMLINE')
  DO CASE

    *-- Initialize the estimated costs with 0 in all the records of the CT.
    CASE lcTranType='M'
      SELECT CutTktL
      SET ORDER TO TAG CutTktL
      =SEEK(laData[1])
      REPLACE REST nCost1 WITH 0,nCost2 WITH 0,nCost3 WITH 0,nCost4 WITH 0,nCost5 WITH 0;
      WHILE cuttkt+style+dyelot+trancd+STR(RECNO(),7) = laData[1]
    
    *-- Initialize the estimated costs with 0 in all the records of the PO.
    *-- or the Adornment Order
    *khm1
    *CASE lcTranType='I'
    CASE lcTranType $ 'ID'
    *khm1
        
      SELECT PosLn
      SET ORDER TO TAG PosLn
      =SEEK(IIF(EMPTY(lcDyePO),'P','D')+laData[1])
      IF !llAutoGen
        REPLACE REST nCost1  WITH 0,nCost2  WITH 0,nCost3  WITH 0,nCost4  WITH 0,;
                     nCost5  WITH 0,nECost1 WITH 0,nECost2 WITH 0,nECost3 WITH 0,;
                     nECost4 WITH 0,nECost5 WITH 0;
        WHILE cStytype+po+style+STR(lineno,6)+trancd+STR(RECNO(),7) = IIF(EMPTY(lcDyePO),'P','D')+laData[1]
      ELSE
        REPLACE REST nCost2  WITH 0,nCost3  WITH 0,nCost4  WITH 0,;
                     nCost5  WITH 0,nECost2 WITH 0,nECost3 WITH 0,;
                     nECost4 WITH 0,nECost5 WITH 0;
        WHILE cStytype+po+style+STR(lineno,6)+trancd+STR(RECNO(),7) = IIF(EMPTY(lcDyePO),'P','D')+laData[1]
      ENDIF  

    *-- Initialize the estimated costs with 0 in all the records of the MPO.      
    CASE lcTranType='T'
      SELECT mmfgordd
      SET ORDER TO TAG MMFGORDD
      =SEEK(laData[1])
      REPLACE REST nCost1 WITH 0,nCost2 WITH 0,nCost3 WITH 0,nCost4 WITH 0;
      WHILE cmfgordno+cfabric+color+dyelot+trancd = laData[1]
  ENDCASE

  SELECT BOMLINE
  lcBomLnKey = EVALUATE(Key())
  DO WHILE cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+Item+IClr+MfgCode=;
           lcTranType+'1'+laData[1] FOR !lVoid
    SCATTER MEMVAR
    lnECost    = 0
    lnTransQty  = 0
    lnBomLLine = LineNo
    lcBomLType = cBomTyp
    lcBomStyle = Style
    lcBomSClr  = SClr
    lcBomItem  = Item
    lcBomIClr  = IClr
    lcBomMCode = MfgCode
    lcBomCatgT = cCatgTyp
    lcBomLnKey = EVALUATE(Key())

    DO CASE
      *khm1
      *CASE lcTranType='I'
      CASE lcTranType $ 'ID'
      *khm1      
        lcAlias  = 'POSLN'
        lcWhlExp = 'cstytype+po+style+STR(lineno,6)+trancd'
        lcScnExp = IIF(EMPTY(lcDyePO),'P','D')+m.CTktNo+m.Style+STR(m.LineNo,6)
      CASE lcTranType='M'
        lcAlias  = 'CUTTKTL'
        lcWhlExp = 'cuttkt+style+dyelot+trancd'
        lcScnExp = m.CTktNo+m.Style
      CASE lcTranType='T'
        lcAlias  = 'MMFGORDD'
        lcWhlExp = 'cmfgordno+cfabric+color+dyelot+trancd'
        lcScnExp = m.CTktNo+SUBSTR(m.Style,1,7)+m.SClr
    ENDCASE
    SELECT (lcAlias)
    =SEEK(lcScnExp)
    SCAN REST WHILE &lcWhlExp. = lcScnExp
      lnEstCost  = 0
      SELECT BOMLINE
      SEEK lcBomLnKey
      *SCAN REST WHILE cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+;
                      Item+IClr+MfgCode=;
                      lcTranType+'1'+laData[1]+STR(lnBomLLine,6)+lcBomLType+;
                      lcBomStyle+lcBomSClr+lcBomItem+lcBomIClr+lcBomMCode
      SCAN REST WHILE cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+;
                      Item+IClr+MfgCode=;
                      lcTranType+'1'+laData[1]+STR(lnBomLLine,6)+lcBomLType+;
                      lcBomStyle+lcBomSClr
         lcBomSizes = IIF(lcTranType='T','1',IIF(EMPTY(cSizes),'12345678',cSizes))
       
         lnTransQty  = 0
         FOR lnCntr = 1 TO 8
           lcCntr   = STR(lnCntr,1)
           IF lcCntr $ lcBomSizes
             DO CASE
               *khm1
               *CASE lcTranType='I'
               CASE lcTranType $ 'ID'
               *khm1
                              
                 lnTransQty = lnTransQty + PosLn.Qty&lcCntr
               CASE lcTranType='M'
                 lnTransQty = lnTransQty + CutTktL.Qty&lcCntr
               CASE lcTranType='T'
                 lnTransQty = lnTransQty + mMfgOrdD.nMfgTotQty
             ENDCASE
           ENDIF
         ENDFOR
       
         lnEstCost  = lnEstCost + ((UnitQty * UnitCost) * lnTransQty)

      ENDSCAN

      DO CASE
        *khm1
        *CASE lcTranType='I'
        CASE lcTranType $ 'ID'
        *khm1        

           lnEstCost = lnEstCost / PosLn.TotQty
        CASE lcTranType='M'
           lnEstCost = lnEstCost / CutTktL.TotQty
        CASE lcTranType='T'
           lnEstCost = lnEstCost / mMfgOrdD.nMfgTotQty
      ENDCASE
    
      DO CASE
        *khm1
        *CASE lcTranType='I'
        CASE lcTranType $ 'ID'
        *khm1
          DO CASE
            CASE lcBomCatgT = 'P'
              lnECost = (lnEstCost &lcPExSign POSHDR.nPriceRat &lcPUntSin POSHDR.nCurrUnit)
            CASE !INLIST(lcBomCatgT ,'S','F','T')
              lnECost = (lnEstCost &lcDExSign POSHDR.nDutyRat &lcDUntSin POSHDR.nDCurUnit)
            OTHERWISE
              lnECost  = lnEstCost
          ENDCASE   
          IF !( lcBomCatgT = 'P' AND llAutoGen)
            SELECT PosLn
            REPLACE ('nCost'+lcBomLType) WITH lnEstCost ,;
                    ('nECost'+m.cBomTyp) WITH lnECost
            IF lcBomCatgT = 'P'
              IF nCost1 > Gros_Price
                REPLACE Gros_Price WITH nCost1 ; 
                        Disc_Pcnt  WITH 0
              ELSE
                REPLACE Disc_Pcnt  WITH IIF(Gros_Price = 0 , 0 , 100 - nCost1 * 100 / Gros_Price)
              ENDIF
            ENDIF
          ENDIF

        CASE lcTranType='M'
          SELECT CutTktL
          REPLACE ('nCost'+m.cBomTyp) WITH lnEstCost

        CASE lcTranType='T'
          SELECT mmfgordd
          REPLACE ('nCost'+m.cBomTyp) WITH lnEstCost
      ENDCASE  
    ENDSCAN
    SELECT BomLine
  ENDDO
  SEEK lcBomLnKey
ENDIF
*--end of lfUpdEstCst.

*!*************************************************************
*! Name      : lfvIsStyWr
*! Developer : Ahmed Maher
*! Date      : 01/02/2002
*! Purpose   : Validate Style issued Warehouse
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvIsStyWr()
*!*************************************************************
*B605267,1 AMH 
*!*************************************************************
FUNCTION lfvIsStyWr

*B039660,1 NNA 06/06/2006 (Begin) convert next trigger to be done within Binmain.Prg instead of Davmain.prg 
*C123853,1 MHM 01/15/2005  validate warehouse for David Luke[Start]
IF ASCAN(laEvntTrig , PADR('DLVLDWRH',10)) <> 0
  =gfDoTriger('POCSSH',PADR('DLVLDWRH',10)) 
  RETURN
ENDIF  
*C123853,1 MHM 01/15/2005  [End]
*B039660,1 NNA (End)

lcIssWare = laStyWare[lnIssWare,2]
=SEEK(m.Item+lcIssWare+space(10),'StyDye')
lnIssCost = IIF(laSetups[10,2]='A',StyDye.Ave_Cost,Style.TotCost)
=lfRefresh('MFSTYISS')
SHOW GETS WINDOW 'MFSTYISS' ONLY
*-- end of lfvIsStyWr.
*B605267,1 AMH [End]


*-- KHM/SSH
*!*************************************************************
*! Name      : lfvClose
*! Developer : Khalid Mohi El-Din / Ahmed Salah Shalaby
*! Date      : 02/19/2002
*! Purpose   : Valid funtion for Close button
*!*************************************************************
*! Example   :  =lfvClose()
*!*************************************************************
*B605447,1
*!*************************************************************
FUNCTION lfvClose

PRIVATE lnTActCst,lnTEstCst,lnITActCst,lnITEstCst,lcItem,lnRecNo, llCloseTr
=gfOpenFile(gcDataDir+'BOMCOST',gcDataDir+'BOMCOST','SH')
IF 'MA' $ gcComp_mdl
  =gfOpenFile(gcDataDir+'MATINVJL',gcDataDir+'MATINVJL','SH')
ENDIF  
SET ORDER TO TAG POBOMCLS IN BOMCOST
SET ORDER TO TAG Ctktbom  IN CTKTBOM
SET ORDER TO TAG BOMLINE  IN BOMLINE
IF 'MA' $ gcComp_mdl
  SET ORDER TO TAG MatInvJl IN MatInvJl
ENDIF  
*--- SSH AP
llApVendor = 'AP' $ gcCmpModules
IF llApVendor
  llCloseTr = .F.
  SELECT BOMCOST
  IF SEEK(lcTranType+laData[1])
    SCAN REST WHILE cImTyp+cTktNo+Actualize+cBomType+Item+Iclr+MfgCode+;
                  cOprCode+cLotNo+cISession+cRSession=lcTranType+laData[1]

      IF Actualize = "N" AND ;
        (INLIST(CCOSTTYPE,'P','D') OR ;
        (cCostType="M" AND gfRltFld(MfgCode,@laMfgRFld,'MFGCODE') .AND. EMPTY(lcMfgGlAcnt)))
        llCloseTr = .T.
        EXIT
      ENDIF
    ENDSCAN              
  ENDIF
  IF llCloseTr
  
    lcMsgCl = "One or more cost items have not been contributed."+;
              " Would you like to automatically contribute?"
    IF gfModalGen('QRM00000B00006','DIALOG',.F., .F., lcMsgCl) = 2
      RETURN
    ENDIF  
  ENDIF

  *E302010,1 ALB Auto Contribute the non contribute lines in the AP invoice [Begin]
  SELECT BOMCOST
  lcOldOrder = ORDER()
  =lfContrbut(lcTranType,laData[1])
  SELECT BOMCOST
  SET ORDER TO (lcOldOrder)
  *E302010,1 ALB Auto Contribute the non contribute lines in the AP invoice [End]
ENDIF
*--- SSH AP

DO CASE
  CASE lcTranType='M'
    lcClsTitle = 'Close Cutting Ticket# '+laData[1]
    lcMessage  = 'cutting ticket'
    SET ORDER TO TAG CUTREC IN CUTTKTL
  *khm1
  *CASE lcTranType='I'
  CASE lcTranType $ 'ID'
  *khm1  
    IF EMPTY(lcDyePO)
      lcClsTitle  = 'Close Style Purchase Order# '+laData[1]
      lcMessage  = 'purchase order'
    ELSE
      lcClsTitle  = 'Close Dye Order# '+laData[1]
      lcMessage  = 'dye order'
    ENDIF
    SET ORDER TO TAG POSLN IN POSLN
  CASE lcTranType='T'
    lcClsTitle = 'Close Material Manufacturing Order# '+laData[1]
    lcMessage  = 'material order'
ENDCASE

*--- Doc. [Start]
*--- laSetups[9,2] = Matrial Costing Method.
*--- Check If Costing method Is 
*--- Lot|LIFO|FIFO ~ L|I|F.
*--- Doc. [End]

llClose = .F.
ldClsDate = gdSysDate

DO (gcScrDir+"MFTKTCLS.SPX")

IF !llClose
  =IIF(lnActFolder=1,lfwBrow(),.T.)
  RETURN
ENDIF
IF !CHECKPRD(ldClsDate,'lcGlYear','lcGlPeriod','  ')
  =IIF(lnActFolder=1,lfwBrow(),.T.)
  RETURN
ENDIF

WAIT WINDOW 'Closing ticket '+laData[1]+'.' NOWAIT
IF lcTranType <> "T"
  =gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
ENDIF
=gfOpenFile(gcDataDir+'FABDYE',gcDataDir+'FABDYE','SH')


STORE 0 TO lnTActCst,lnTEstCst

*B606028,1 AMH Correct update nmatwip in fabric & fabdye files [Start]
*SELECT BOMCOST
*=SEEK(lcTranType+laData[1])
*SCAN REST WHIlE cimtyp+ctktno+actualize+cbomtype+item+iclr+mfgcode+;
*                coprcode+clotno+cisession+crsession=lcTranType+laData[1] ;
*          FOR   INLIST(CCOSTTYPE,'F','T')
*  SELECT FABRIC
*  =SEEK(PADR(BOMCOST.ITEM,7) + PADR(BOMCOST.ICLR,6))
*  REPLACE NMATWIP WITH NMATWIP - BOMCOST.NTOTQTY
*    
*  SELECT FABDYE
*  =SEEK(PADR(BOMCOST.ITEM,7) + PADR(BOMCOST.ICLR,6) + BOMCOST.CWARECODE+SPACE(10))
*  REPLACE NMATWIP WITH NMATWIP - BOMCOST.NTOTQTY
*    
*  SELECT FABDYE
* =SEEK(PADR(BOMCOST.ITEM,7) + PADR(BOMCOST.ICLR,6) + BOMCOST.CWARECODE+BOMCOST.cDyelot)
*  REPLACE NMATWIP WITH NMATWIP - BOMCOST.NTOTQTY

*ENDSCAN
*khm1
*IF lcTranType $ 'MI'
IF lcTranType $ 'MID'
*khm
  =lfUpdWip()
ELSE
  SELECT BOMCOST
  =SEEK(lcTranType+laData[1])
  SCAN REST WHIlE cimtyp+ctktno+actualize+cbomtype+item+iclr+mfgcode+;
                  coprcode+clotno+cisession+crsession=lcTranType+laData[1];
            FOR   INLIST(CCOSTTYPE,'F','T')
    SELECT FABRIC
    =SEEK(PADR(BOMCOST.ITEM,7) + PADR(BOMCOST.ICLR,6))
    REPLACE NMATWIP WITH NMATWIP - BOMCOST.NTOTQTY
    
    SELECT FABDYE
    =SEEK(PADR(BOMCOST.ITEM,7) + PADR(BOMCOST.ICLR,6) + BOMCOST.CWARECODE+SPACE(10))
    REPLACE NMATWIP WITH NMATWIP - BOMCOST.NTOTQTY
    
    IF !EMPTY(BOMCOST.CDYELOT)
      SELECT FABDYE
      =SEEK(PADR(BOMCOST.ITEM,7) + PADR(BOMCOST.ICLR,6) + BOMCOST.CWARECODE+BOMCOST.cDyelot)
      REPLACE NMATWIP WITH NMATWIP - BOMCOST.NTOTQTY
    ENDIF

  ENDSCAN
ENDIF
*B606028,1 AMH [End]

SELECT BOMLINE 
=SEEK(lcTranType+'2'+laData[1])
DELETE REST WHILE ;
cIMTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+Item+IClr+Mfgcode = ;
lcTranType+'2'+laData[1] FOR EMPTY(cRSession)

SELECT BOMCOST
IF SEEK(lcTranType+laData[1]+'N')
  DO WHILE cimtyp+ctktno+actualize+cbomtype+item+iclr+mfgcode+coprcode+;
           clotno+cisession+crsession=lcTranType+laData[1]+'N' .AND. !EOF()
    SELECT BOMCOST
    STORE 0 TO lnItActCst,lnItActQty,lnItEstCst,lnItEstQty

    lcItem  = cBomType+Item+IClr+MfgCode

*--- DOC.
*--- Sum to get the total cost and total qty for the actualizes
*--- DOC.
    SUM REST nTotCst,nTotQty TO lnItActCst, lnItActQTy ;
        WHILE cimtyp+ctktno+actualize+cbomtype+item+iclr+mfgcode+coprcode+;
           clotno+cisession+crsession=lcTranType+laData[1]+'N'+lcItem
    SELECT BOMLINE 
    =SEEK(lcTranType+'2'+laData[1])
*--- DOC.
*--- Sum The estimated cost and estimated qty for revieved line
*--- DOC.
    SUM REST  ItemAmt,ItemQty TO lnItEstCst, lnItEstQty  ;
        WHILE cIMTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+Item+IClr+MfgCode =;
              lcTranType+'2'+laData[1] ;
        FOR   cBomTyp+Item+IClr+MfgCode = lcItem

    lnTActCst = lnTActCst + lnItActCst
    lnTEstCst = lnTEstCst + lnItEstCst

    =SEEK(lcTranType+'2'+laData[1])
    SCAN REST WHILE cIMTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+Item+IClr+MfgCode = ;
                    lcTranType+'2'+laData[1] ;
              FOR   cBomTyp+Item+IClr+MfgCode = lcItem
      lnItemAmnt = ITEMAMT
      lnItemQty  = ItemQty
      SCATTER MEMVAR
      m.Ctype    = '3'
      m.ItemAmt  = IIF(lnItEstCst=0,0,lnItemAmnt/lnItEstCst*lnItActCst)
      m.ItemQty  = IIF(lnITEstQty=0,0,lnItemQty/lnITEstQty*lnITActQty)
      m.UnitCost = IIF(m.ItemQty=0,0,;
                   IIF(lnItEstCst=0,0,lnItemAmnt/lnItEstCst*lnItActCst)/m.ItemQty)
      m.UnitQty  = IIF(StyQty=0,0,m.ItemQty/StyQty)
      DO CASE
        CASE lcTranType='I' .AND. EMPTY(lcDyePO)
          IF SEEK('P'+laData[1]+m.Style+STR(m.LineNo,6),'POSLN')
             SELECT POSLN
             COUNT REST  TO lnNoOfRec ;
                   WHILE cSTyType+Po+Style+STR(LineNo,6)+TranCd+STR(RECNO(),7)=;
                         'P'+laData[1]+m.Style+STR(m.LineNo,6) ;
                   FOR   cRSession = m.cRSession
            =SEEK('P'+laData[1]+m.Style+STR(m.LineNo,6))
            *REPLACE REST  ('NEACTCOST'+m.cBomTyp) WITH  ;
                          EVAL('NEACTCOST'+m.cBomTyp) + ;
                          m.UnitCost * m.UnitQty / lnNoOfRec;
                    WHILE cSTyType+Po+Style+STR(LineNo,6)+TranCd+STR(RECNO(),7)=;
                          'P'+laData[1]+m.Style+STR(m.LineNo,6) ;
                    FOR   cRSession = m.cRSession

            SCAN REST WHILE cSTyType+Po+Style+STR(LineNo,6)+TranCd+STR(RECNO(),7)=;
                            'P'+laData[1]+m.Style+STR(m.LineNo,6) ;
                      FOR cRSession = m.cRSession

              REPLACE ('NEACTCOST'+m.cBomTyp) WITH  ;
                          EVAL('NEACTCOST'+m.cBomTyp) + ;
                          m.UnitCost * m.UnitQty / lnNoOfRec
            ENDSCAN

          ENDIF
        *khm1
        *CASE lcTranType='I' .AND. !EMPTY(lcDyePO)
        CASE lcTranType = 'D'
        *khm1
          IF SEEK('D'+laData[1]+m.Style+STR(m.LineNo,6),'POSLN')
             SELECT POSLN
             COUNT REST  TO lnNoOfRec ;
                   WHILE cSTyType+Po+Style+STR(LineNo,6)+TranCd+STR(RECNO(),7)=;
                         'D'+laData[1]+m.Style+STR(m.LineNo,6) ;
                   FOR   cRSession = m.cRSession
            =SEEK('D'+laData[1]+m.Style+STR(m.LineNo,6))
            *REPLACE REST  ('NEACTCOST'+m.cBomTyp) WITH  ;
                          EVAL('NEACTCOST'+m.cBomTyp) + ;
                          m.UnitCost * m.UnitQty / lnNoOfRec;
                    WHILE cSTyType+Po+Style+STR(LineNo,6)+TranCd+STR(RECNO(),7)=;
                          'D'+laData[1]+m.Style+STR(m.LineNo,6) ;
                    FOR   cRSession = m.cRSession
            
            SCAN REST WHILE cSTyType+Po+Style+STR(LineNo,6)+TranCd+STR(RECNO(),7)=;
                          'D'+laData[1]+m.Style+STR(m.LineNo,6) ;
                    FOR   cRSession = m.cRSession

              REPLACE ('NEACTCOST'+m.cBomTyp) WITH  ;
                            EVAL('NEACTCOST'+m.cBomTyp) + ;
                            m.UnitCost * m.UnitQty / lnNoOfRec
            ENDSCAN        
          ENDIF
        *C200080,1 AMM end

        CASE lcTranType='M'
          *E300725,1 Update actual cost for Cutting Ticket lines
          *B602462,1 Fix alias reference
          *SELECT (CutTktL)
          SELECT CutTktL
          *B602462,1 Fix alias reference
                    
          IF SEEK(m.cRSession+laData[1]+m.Style)
            *REPLACE REST ('NACT_CST'+m.cBomTyp) WITH     ;
                         EVAL('NACT_CST'+m.cBomTyp)+m.UnitCost * m.UnitQty ;
                   WHILE cRSession+CutTkt+Style+TranCd =  ;
                         m.cRSession+laData[1]+m.Style
            SCAN REST WHILE cRSession+CutTkt+Style+TranCd =  ;
                            m.cRSession+laData[1]+m.Style

              REPLACE ('NACT_CST'+m.cBomTyp) WITH     ;
                      EVAL('NACT_CST'+m.cBomTyp)+m.UnitCost * m.UnitQty
            
            ENDSCAN             
          ENDIF
          
      
        *E301235,4 WAB - filed name is lcTranType not lcMIFlg 
        *E301235,4 WAB - START
        *CASE lcMIFlg='T'
        CASE lcTranType='T'
          *E300725,1 Update actual cost for Fabric order lines
          *B603382,1 WAB - file name is MMFGORDD not MMFGORDDT
          *SELECT MMFGORDDT
          SELECT MMFGORDD
          *B603382,1 WAB - END
          IF SEEK(laData[1]+SUBSTR(m.Style,1,7)+m.SClr)
            *REPLACE REST ('NACT_COST'+m.cBomTyp) WITH      ;
                          EVAL('NACT_COST'+m.cBomTyp)+m.UnitCost*m.UnitQty ;
                    WHILE cMfgOrdNo+cFabric+Color+Dyelot+TranCd = ;
                          laData[1]+SUBSTR(m.Style,1,7)+m.SClr ;
                    FOR   cRSession = m.cRSession
            SCAN REST WHILE cMfgOrdNo+cFabric+Color+Dyelot+TranCd = ;
                          laData[1]+SUBSTR(m.Style,1,7)+m.SClr ;
                    FOR   cRSession = m.cRSession

              REPLACE ('NACT_COST'+m.cBomTyp) WITH      ;
                      EVAL('NACT_COST'+m.cBomTyp)+m.UnitCost*m.UnitQty

            ENDSCAN
          ENDIF
      ENDCASE
      lnRecNo = RECNO('BOMLINE')
      INSERT INTO BOMLINE FROM MEMVAR
      GO lnRecNo IN BOMLINE
    ENDSCAN
    SELECT BOMCOST
  ENDDO
ENDIF
*E300725,1 Create Cancelled records in the ticket detailed file with open
*E300725,1 quantities and update Work In Process in the Style file
DO CASE
  CASE lcTranType='M'
    
    lcFile = 'CUTTKTL'
    *E#301235,1 Removed STR(RECNO(),7) to match index expr
    *SELECT * FROM CUTTKTL WHERE CutTkt+Style+Dyelot+TranCd+STR(RECNO(),7)=laData[1] ;
    INTO CURSOR (lcisslogfile) ORDER BY Style,cWareCode,Dyelot
    
    *B606318,1 KHM 07/30/2002 (Begin) Changing the order by to be by LineNo.
    *SELECT * FROM CUTTKTL WHERE CutTkt+Style+Dyelot+TranCd=laData[1] ;
    INTO CURSOR (lcisslogfile) ORDER BY Style,cWareCode,Dyelot
    
    SELECT * FROM CUTTKTL WHERE CutTkt+Style+Dyelot+TranCd=laData[1] ;
    INTO CURSOR (lcisslogfile) ORDER BY LineNo
    *B606318,1 KHM 07/30/2002 (End)

  *khm1  
  *CASE lcTranType='I'
  CASE lcTranType $ 'ID'
  *khm1
  
    lcFile = 'POSLN'

    *E#301235,1 Removed STR(RECNO(),7) to match index expr    
    *SELECT * FROM POSLN WHERE ;
    cStytype+po+style+STR(lineno,6)+trancd+STR(RECNO(),7)='P'+laData[1] ;
    INTO CURSOR (lcisslogfile) ORDER BY Style,cWareCode,Dyelot
    *C200080,1 AMM Consider the dye order case
    *SELECT * FROM POSLN WHERE ;
    cStytype+po+style+STR(lineno,6)+trancd='P'+laData[1] ;
    INTO CURSOR (lcisslogfile) ORDER BY Style,cWareCode,Dyelot
    
    *B606318,1 KHM 07/30/2002 (Begin) Changing the order by to be by LineNo.    
    *SELECT * FROM POSLN WHERE ;
    cStytype+po+style+STR(lineno,6)+trancd=IIF(EMPTY(lcDyePO),'P','D')+laData[1] ;
    INTO CURSOR (lcisslogfile) ORDER BY Style,cWareCode,Dyelot
    
    SELECT * FROM POSLN WHERE ;
    cStytype+po+style+STR(lineno,6)+trancd=IIF(EMPTY(lcDyePO),'P','D')+laData[1] ;
    INTO CURSOR (lcisslogfile) ORDER BY LineNo
    *B606318,1 KHM 07/30/2002 (End)
    
    *C200080,1 AMM end
    
  *E#301235,1  
  CASE lcTranType='T'
    lcFile = 'MMFGORDD'          
    SELECT * FROM MMFGORDD WHERE cmfgordno+cfabric+color+dyelot+trancd=laData[1] ;
    INTO CURSOR (lcisslogfile) ORDER BY cfabric,color,cWareCode,Dyelot

ENDCASE
GO TOP
STORE 0 TO lnCanceled
*E#301235,1
IF lcTranType='T'
  DO WHILE !EOF()
    SCATTER MEMVAR
    
    *B604856,1 AMH Subtracting any receiving quantity from budget quantity [Start]
    *SUM REST nMfgTotQty TO m.nMfgTotQty;
       WHILE cfabric+color+cWareCode+Dyelot = m.cfabric+m.color+m.cWareCode+m.Dyelot
    SUM REST IIF(TranCd='1',nMfgTotQty,-nMfgTotQty) TO m.nMfgTotQty;
       WHILE cfabric+color+cWareCode+Dyelot = m.cfabric+m.color+m.cWareCode+m.Dyelot
    *B604856,1 AMH [End]
    
    m.nMfgTotQty  = MAX(m.nMfgTotQty,0)
    IF m.nMfgTotQty > 0
      IF !EMPTY(m.Dyelot) .AND. SEEK(m.cfabric+m.color+m.cWareCode+m.Dyelot,'FabDye')
        SELECT FabDye
        =RLOCK()
        REPLACE OnOrder WITH MAX(OnOrder - m.nMfgTotQty,0)  
        UNLOCK
      ENDIF
      IF SEEK(m.cfabric+m.color+m.cWareCode+SPACE(10),'FabDye')
        SELECT FabDye
        =RLOCK()
        REPLACE OnOrder WITH MAX(OnOrder - m.nMfgTotQty,0)  
        UNLOCK
      ENDIF
      IF SEEK(m.cfabric+m.color,'FABRIC')
        SELECT FABRIC
        =RLOCK()
        REPLACE OnOrder WITH MAX(OnOrder - m.nMfgTotQty,0)  
        UNLOCK
      ENDIF
      m.TranCd ='4'
      m.cRSession = lcSession

      *B606314,1 KHM 07/28/2002 (Begin) Updating the date in the lines with the closing date.
      m.Date=ldClsDate
      *B606314,1 KHM 07/28/2002 (End)

      lnCanceled  = lnCanceled + m.nMfgTotQty
      INSERT INTO (lcFile) FROM MEMVAR
    ENDIF
    SELECT (lcisslogfile)
  ENDDO
ELSE
  DO WHILE !EOF()
    SCATTER MEMVAR
*--- DOC.
*--- Get the open qty
*--- DOC.
    
    *B606318,1 KHM 07/30/2002 (Begin) Separate the case of PO & MF in order to take care
    *B606318,1                of the shipment lines in case of PO.
    *SUM REST IIF(TranCd='1',Qty1,-Qty1),IIF(TranCd='1',Qty2,-Qty2) ,;
             IIF(TranCd='1',Qty3,-Qty3),IIF(TranCd='1',Qty4,-Qty4) ,;
             IIF(TranCd='1',Qty5,-Qty5),IIF(TranCd='1',Qty6,-Qty6) ,;
             IIF(TranCd='1',Qty7,-Qty7),IIF(TranCd='1',Qty8,-Qty8)  ;
         TO  m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8;
    WHILE Style+cWareCode+Dyelot = m.Style+m.cWareCode+m.Dyelot
    *khm1
    *IF lcTranType='I'
    IF lcTranType $ 'ID'
    *khm1
      SUM REST IIF(TranCd='1',Qty1,-IIF(TranCd='3',0,Qty1)),;
               IIF(TranCd='1',Qty2,-IIF(TranCd='3',0,Qty2)),;
               IIF(TranCd='1',Qty3,-IIF(TranCd='3',0,Qty3)),;
               IIF(TranCd='1',Qty4,-IIF(TranCd='3',0,Qty4)),;
               IIF(TranCd='1',Qty5,-IIF(TranCd='3',0,Qty5)),;
               IIF(TranCd='1',Qty6,-IIF(TranCd='3',0,Qty6)),;
               IIF(TranCd='1',Qty7,-IIF(TranCd='3',0,Qty7)),;               
               IIF(TranCd='1',Qty8,-IIF(TranCd='3',0,Qty8)) ;
           TO  m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8;
      WHILE STR(LineNo,6) = STR(m.LineNo,6)
    ELSE
      SUM REST IIF(TranCd='1',Qty1,-Qty1),IIF(TranCd='1',Qty2,-Qty2) ,;
               IIF(TranCd='1',Qty3,-Qty3),IIF(TranCd='1',Qty4,-Qty4) ,;
               IIF(TranCd='1',Qty5,-Qty5),IIF(TranCd='1',Qty6,-Qty6) ,;
               IIF(TranCd='1',Qty7,-Qty7),IIF(TranCd='1',Qty8,-Qty8)  ;
           TO  m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8;
      WHILE STR(LineNo,6) = STR(m.LineNo,6)
    ENDIF      
    *B606318,1 KHM 07/30/2002 (End)
    
    m.Qty1  = MAX(m.Qty1,0)
    m.Qty2  = MAX(m.Qty2,0)
    m.Qty3  = MAX(m.Qty3,0)
    m.Qty4  = MAX(m.Qty4,0)
    m.Qty5  = MAX(m.Qty5,0)
    m.Qty6  = MAX(m.Qty6,0)
    m.Qty7  = MAX(m.Qty7,0)
    m.Qty8  = MAX(m.Qty8,0)
    m.TotQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
    IF m.TotQty > 0
      IF !EMPTY(m.Dyelot) .AND. SEEK(m.Style+m.cWareCode+m.Dyelot,'StyDye')
        SELECT StyDye
        =RLOCK()
        REPLACE WIP1   WITH MAX(WIP1 - m.Qty1,0)   ,;
                WIP2   WITH MAX(WIP2 - m.Qty2,0)   ,;
                WIP3   WITH MAX(WIP3 - m.Qty3,0)   ,;
                WIP4   WITH MAX(WIP4 - m.Qty4,0)   ,;
                WIP5   WITH MAX(WIP5 - m.Qty5,0)   ,;
                WIP6   WITH MAX(WIP6 - m.Qty6,0)   ,;
                WIP7   WITH MAX(WIP7 - m.Qty7,0)   ,;
                WIP8   WITH MAX(WIP8 - m.Qty8,0)   ,;
                TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
        UNLOCK
      ENDIF
      IF SEEK(m.Style+m.cWareCode+SPACE(10),'StyDye')
        SELECT StyDye
        =RLOCK()
        REPLACE WIP1   WITH MAX(WIP1 - m.Qty1,0)   ,;
                WIP2   WITH MAX(WIP2 - m.Qty2,0)   ,;
                WIP3   WITH MAX(WIP3 - m.Qty3,0)   ,;
                WIP4   WITH MAX(WIP4 - m.Qty4,0)   ,;
                WIP5   WITH MAX(WIP5 - m.Qty5,0)   ,;
                WIP6   WITH MAX(WIP6 - m.Qty6,0)   ,;
                WIP7   WITH MAX(WIP7 - m.Qty7,0)   ,;
                WIP8   WITH MAX(WIP8 - m.Qty8,0)   ,;
                TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
        UNLOCK
      ENDIF
      IF SEEK(m.Style,'Style')
        SELECT Style
        =RLOCK()
        REPLACE WIP1   WITH MAX(WIP1 - m.Qty1,0)   ,;
                WIP2   WITH MAX(WIP2 - m.Qty2,0)   ,;
                WIP3   WITH MAX(WIP3 - m.Qty3,0)   ,;
                WIP4   WITH MAX(WIP4 - m.Qty4,0)   ,;
                WIP5   WITH MAX(WIP5 - m.Qty5,0)   ,;
                WIP6   WITH MAX(WIP6 - m.Qty6,0)   ,;
                WIP7   WITH MAX(WIP7 - m.Qty7,0)   ,;
                WIP8   WITH MAX(WIP8 - m.Qty8,0)   ,;
                TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
        UNLOCK
      ENDIF
      m.TranCd =IIF(lcTranType='M','4','5')
      m.cRSession = lcSession

      *B606314,1 KHM 07/28/2002 (Begin) Updating the date in the lines with the closing date.
      m.Date=ldClsDate
      m.DPostDate = ldClsDate
      *B606314,1 KHM 07/28/2002 (End)

      lnCanceled  = lnCanceled + m.TotQty
      INSERT INTO (lcFile) FROM MEMVAR
    ENDIF
    SELECT (lcisslogfile)
  ENDDO
ENDIF
USE IN (lcisslogfile)
SELECT (lcBaseFile)
laData[3]  = 'S'

*B604856,1 AMH Add the previus canceled quantity to the quantity canceled by closing [Start]
*laData[8]  = lnCanceled
laData[8]  = laData[8] + lnCanceled
*B604856,1 AMH [End]

laData[10] = 0
GATHER FROM laData FIELDS &lcScFields
*E300725,1 Post difference between actual and landed cost to GL

*--- Doc. [Begin]
*--- At the next part after IF laSetups[1,2]='Y' (Link to GL)
*--- We will start to update the GlDist file as following :
*--- for each one of the five cost element
*--- if there exist Differences between Actual and landed cost
*--- we will go to insert Gl_Interies with the Deferent
*--- Ex :- IF Actual = 12 , Landed = 10 ==> Gl-Interies = 2.
*--- And we will use the following Category Key for each one of the costing
*--- '022' , '023' , '024' , '025' , '026'
*--- Except for 'Material Manufucturing Order' we will use Category '021'.
*--- if the Gl_Link.CatgDesc empty for any one of the above Keys 
*--- we will use the key '019'  as default one.
*--- Doc. [End]

IF laSetups[1,2]='Y'
  STORE '' TO m.cWipAcnt,m.cVarAcnt1,m.cVarAcnt2,m.cVarAcnt3,m.cVarAcnt4,m.cVarAcnt5
  SET ORDER TO TAG GL_LINK IN GL_LINK
  IF lcTranType = 'T'
    lnLanded=laData[17]+laData[18]+laData[19]+laData[20]
    lnActual=laData[22]+laData[23]+laData[24]+laData[25]
    IF lnLanded <> lnActual
      *B603504,1 (Begin) Remark the following lines and Change the transaction type 
      *B603504,1         sent by GLDIST procedure for closing matreial MFG from 'JC'  to 'MC'.
      *DO GlDist WITH laData[29],'013',(lnLanded-lnActual),'JC',;
                     laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
      *m.cWipAcnt = &lcGlDTemp..GlAccount
      *DO GlDist WITH laData[29],'021',(lnActual-lnLanded),'JC',;
                     laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
      *m.cVarAcnt1 = &lcGlDTemp..GlAccount
      
      DO GlDist WITH laData[29],'013',(lnLanded-lnActual),'MC',;
                     laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
      m.cWipAcnt = &lcGlDTemp..GlAccount
      DO GlDist WITH laData[29],'021',(lnActual-lnLanded),'MC',;
                     laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
      m.cVarAcnt1 = &lcGlDTemp..GlAccount
      *B603504,1 (End)
    ENDIF
  ENDIF
  *--- Doc. [Start]
  
  *--- laData[22] == 1St Cost Element Actual Cost.
  *--- laData[17] == 1St Cost Element Landed Cost.
  
  *--- laData[23] == 2nd Cost Element Actual Cost.
  *--- laData[18] == 2nd Cost Element Landed Cost.

  *--- ladata[24] == 3rd Cost Element Actual Cost.
  *--- ladata[19] == 3rd Cost Element Landed Cost.

  *--- ladata[25] == 4Th Cost Element Actual Cost.
  *--- ladata[20] == 4Th Cost Element Landed Cost.
  
  *--- ladata[26] == 5Th Cost Element Actual Cost.
  *--- ladata[21] == 5Th Cost Element Landed Cost.

  *--- The 1st,2nd,3rd,4th and 5th cost element take an label depending
  *--- on the module setup (IC-Module)
  *--- and the vaiance between Actual & Landed is the variance
  *--- which we will update GL- with.
  
  *--- ladata[29] == Link Code.

  *--- Doc. [End]

  *B603862,1 Define a new variable to hold Type of Cost Sheet (Manuf or Purch) [Begin]
  *B603862,1 make a new Type for Cost Sheet in SyGlTrans in case of Manufacturing
  PRIVATE lcGLTrnTyp
  lcGLTrnTyp = IIF(gcAct_Appl=="MF",'JP','JC')
  *B603862,1 Define a new variable to hold Type of Cost Sheet (Manuf or Purch) [End]

  *C200449,1 ALB Add GL entries when closing Non GL operations [Begin]
  IF ASCAN(laEvntTrig , PADR('UPDTCLGL',10)) <> 0
    =gfDoTriger('POCSSH',PADR('UPDTCLGL',10))
  ENDIF
  *C200449,1 ALB Add GL entries when closing Non GL operations [end]

  IF lcTranType <> 'T' .AND. laData[22] <> laData[17]
    *B603573,1 SSH 16/04/00  [Start] Comented Out.
    *=SEEK(ladata[29]+'021','GL_LINK')
    *B603573,1 SSH [End]

    *B603862,1 Adding the right type for each of PO , MF [Begin]
    *DO GlDist WITH laData[29],'013',(laData[17]-laData[22]),'JC',;
    *               laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
    DO GlDist WITH laData[29],'013',(laData[17]-laData[22]),lcGLTrnTyp,;
                   laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
    *B603862,1 Adding the right type for each of PO , MF [End]

    m.cWipAcnt = &lcGlDTemp..GlAccount
    *B603573,1 SSH 16/04/00  [Start] Comented Out.
    *DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','021'),;
                   (laData[22]-laData[17]),'JC',laData[1],ldClsDate,;
                   lcGlYear,lcGlPeriod,lcGlDTemp

    *B603573,1 SSH 16/04/00  [Start] Use the correct categKey.
    =SEEK(ladata[29]+'022','GL_LINK')
    *B603573,1 SSH 16/04/00   [End]
    
    *B603862,1 Adding the right type for each of PO , MF [Begin]
    *DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','022'),;
    *               (laData[22]-laData[17]),'JC',laData[1],ldClsDate,;
    *               lcGlYear,lcGlPeriod,lcGlDTemp
    DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','022'),;
                   (laData[22]-laData[17]),lcGLTrnTyp,laData[1],ldClsDate,;
                   lcGlYear,lcGlPeriod,lcGlDTemp
    *B603862,1 Adding the right type for each of PO , MF [Begin]
                   
    *B603573,1 SSH 16/04/00   [End]
    m.cVarAcnt1 = &lcGlDTemp..GlAccount
  ENDIF
  
  IF lcTranType <> 'T' .AND. laData[23] <> laData[18]
    *B603573,1 SSH 16/04/00  [Start] Commented Out.
    *=SEEK(ladata[29]+'022','GL_LINK')
    *B603573,1 SSH 16/04/00   [End]

    *B603862,1 Adding the right type for each of PO , MF [Begin]
    *DO GlDist WITH laData[29],'013',(laData[18]-laData[23]),'JC',;
    *               laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
    DO GlDist WITH laData[29],'013',(laData[18]-laData[23]),lcGLTrnTyp,;
                   laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
    *B603862,1 Adding the right type for each of PO , MF [Begin]
    
    m.cWipAcnt = &lcGlDTemp..GlAccount
    *B603573,1 SSH 16/04/00  [Start] Commented Out.
    *DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','022'),;
                   (laData[23]-laData[18]),'JC',laData[1],ldClsDate,;
                   lcGlYear,lcGlPeriod,lcGlDTemp
    
    *B603573,1 SSH 16/04/00  [Start] Use the correct CategKey.
    =SEEK(ladata[29]+'023','GL_LINK')
    *B603573,1 SSH 16/04/00  [End]

    *B603862,1 Adding the right type for each of PO , MF [Begin]
    *DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','023'),;
    *               (laData[23]-laData[18]),'JC',laData[1],ldClsDate,;
    *               lcGlYear,lcGlPeriod,lcGlDTemp
    DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','023'),;
                   (laData[23]-laData[18]),lcGLTrnTyp,laData[1],ldClsDate,;
                   lcGlYear,lcGlPeriod,lcGlDTemp
    *B603862,1 Adding the right type for each of PO , MF [Begin]

    *B603573,1 SSH 16/04/00  [End]
    m.cVarAcnt2 = &lcGlDTemp..GlAccount
  ENDIF
  IF lcTranType <> 'T' .AND. laData[24] <> laData[19]
    *B603573,1 SSH 16/04/00  [Start] Commented Out.
    *=SEEK(ladata[29]+'023','GL_LINK')
    *B603573,1 SSH 16/04/00 [End]

    *B603862,1 Adding the right type for each of PO , MF [Begin]
    *DO GlDist WITH laData[29],'013',(laData[19]-laData[24]),'JC',;
    *               laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
    DO GlDist WITH laData[29],'013',(laData[19]-laData[24]),lcGLTrnTyp,;
                   laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
    *B603862,1 Adding the right type for each of PO , MF [Begin]
 
    m.cWipAcnt = &lcGlDTemp..GlAccount
    *B603573,1 SSH 16/04/00  [Start] Commented Out.
    *DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','023'),;
                   (laData[24]-laData[19]),'JC',laData[1],ldClsDate,;
                   lcGlYear,lcGlPeriod,lcGlDTemp
    
    *B603573,1 SSH 16/04/00 [Start] Use the correct CategKey.
    =SEEK(ladata[29]+'024','GL_LINK')
    *B603573,1 SSH 16/04/00 [End]

    *B603862,1 Adding the right type for each of PO , MF [Begin]
    *DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','024'),;
    *               (laData[24]-laData[19]),'JC',laData[1],ldClsDate,;
    *               lcGlYear,lcGlPeriod,lcGlDTemp
    DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','024'),;
                   (laData[24]-laData[19]),lcGLTrnTyp,laData[1],ldClsDate,;
                   lcGlYear,lcGlPeriod,lcGlDTemp
    *B603862,1 Adding the right type for each of PO , MF [Begin]
                       
    *B603573,1 SSH 16/04/00 [End]
    m.cVarAcnt3 = &lcGlDTemp..GlAccount
  ENDIF
  IF lcTranType <> 'T' .AND. laData[25] <> laData[20]
    *B802877,1 Change Seek on 024 insted of 025.
    *=SEEK(ladata[29]+'025','GL_LINK')

    *B603573,1 SSH 16/04/00 [Start] Commented Out.
    *=SEEK(ladata[29]+'024','GL_LINK')
    *B603573,1 SSH 16/04/00  [End]
    *B802877,1 End.

    *B603862,1 Adding the right type for each of PO , MF [Begin]
    *DO GlDist WITH laData[29],'013',(laData[20]-laData[25]),'JC',;
    *               laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
    DO GlDist WITH laData[29],'013',(laData[20]-laData[25]),lcGLTrnTyp,;
                   laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
    *B603862,1 Adding the right type for each of PO , MF [Begin]
                   
    m.cWipAcnt = &lcGlDTemp..GlAccount
    *B802877,1 send '024' insted of '025'.
    *DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','025'),;
                   (laData[25]-laData[20]),'JC',laData[1],ldClsDate,;
                   lcGlYear,lcGlPeriod,lcGlDTemp
    *B603573,1 SSH 16/04/00 [Start] Commented Out.
    *DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','024'),;
                   (laData[25]-laData[20]),'JC',laData[1],ldClsDate,;
                   lcGlYear,lcGlPeriod,lcGlDTemp

    *B603573,1 SSH 16/04/00 [Start] Get the correct CategKey.
    =SEEK(ladata[29]+'025','GL_LINK')
    *B603573,1 SSH 16/04/00  [End]

    *B603862,1 Adding the right type for each of PO , MF [Begin]
    *DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','025'),;
    *               (laData[25]-laData[20]),'JC',laData[1],ldClsDate,;
    *               lcGlYear,lcGlPeriod,lcGlDTemp
    DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','025'),;
                   (laData[25]-laData[20]),lcGLTrnTyp,laData[1],ldClsDate,;
                   lcGlYear,lcGlPeriod,lcGlDTemp    
    *B603862,1 Adding the right type for each of PO , MF [Begin]
                   
    *B603573,1 SSH 16/04/00 [End]
    *B802877,1 End.
    m.cVarAcnt4 = &lcGlDTemp..GlAccount
  ENDIF
  IF lcTranType <> 'T' .AND. laData[26] <> laData[21]
    *B802877,1 Change Seek on 025 insted of 026.
    *=SEEK(ladata[29]+'026','GL_LINK')

    *B603573,1 SSH 16/04/00 [Start] Commented Out
    *=SEEK(ladata[29]+'025','GL_LINK')
    =SEEK(ladata[29]+'026','GL_LINK')
    *B603573,1 SSH 16/04/00  [End]
    *B802877,1 End.

    *B603862,1 Adding the right type for each of PO , MF [Begin]
    *DO GlDist WITH laData[29],'013',(laData[21]-laData[26]),'JC',;
    *               laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
    DO GlDist WITH laData[29],'013',(laData[21]-laData[26]),lcGLTrnTyp,;
                   laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp
    *B603862,1 Adding the right type for each of PO , MF [Begin]
                        
    m.cWipAcnt = &lcGlDTemp..GlAccount
    *B802877,1 send '025' insted of '026'.
    *DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','026'),;
                   (laData[26]-laData[21]),'JC',laData[1],ldClsDate,;
                   lcGlYear,lcGlPeriod,lcGlDTemp
    *B603573,1 SSH 16/04/00 [Start] Commented Out.
    *DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','025'),;
                   (laData[26]-laData[21]),'JC',laData[1],ldClsDate,;
                   lcGlYear,lcGlPeriod,lcGlDTemp
    
    *B603573,1 SSH 16/04/00 [Start] Get the correct CategKey.
    =SEEK(ladata[29]+'026','GL_LINK')
    *B603573,1 SSH 16/04/00  [End]

    *B603862,1 Adding the right type for each of PO , MF [Begin]
    *DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','026'),;
    *               (laData[26]-laData[21]),'JC',laData[1],ldClsDate,;
    *               lcGlYear,lcGlPeriod,lcGlDTemp
    DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','026'),;
                   (laData[26]-laData[21]),lcGLTrnTyp,laData[1],ldClsDate,;
                   lcGlYear,lcGlPeriod,lcGlDTemp    
    *B603862,1 Adding the right type for each of PO , MF [Begin]
                   
    *B603573,1 SSH 16/04/00 [End]
    *B802877,1 End.
    m.cVarAcnt5 = &lcGlDTemp..GlAccount
  ENDIF
  DO CASE
    CASE lcTranType = 'M'
      SELECT CUTTKTH
      =RLOCK()
      REPLACE cWipAcnt  WITH m.cWipAcnt  ,;
              cVarAcnt1 WITH m.cVarAcnt1 ,;
              cVarAcnt2 WITH m.cVarAcnt2 ,;
              cVarAcnt3 WITH m.cVarAcnt3 ,;
              cVarAcnt4 WITH m.cVarAcnt4 ,;
              cVarAcnt5 WITH m.cVarAcnt5
      UNLOCK
    *khm1
    *CASE lcTranType = 'I'    
    CASE lcTranType $ 'ID'
    *khm1
      SELECT POSHDR
      =RLOCK()
      REPLACE cWipAcnt  WITH m.cWipAcnt  ,;
              cVarAcnt1 WITH m.cVarAcnt1 ,;
              cVarAcnt2 WITH m.cVarAcnt2 ,;
              cVarAcnt3 WITH m.cVarAcnt3 ,;
              cVarAcnt4 WITH m.cVarAcnt4 ,;
              cVarAcnt5 WITH m.cVarAcnt5
      UNLOCK
    CASE lcTranType = 'T'
      SELECT MMFGORDH
      =RLOCK()
      REPLACE cWipAcnt  WITH m.cWipAcnt  ,;
              cVarAcnt1 WITH m.cVarAcnt1
      UNLOCK
  ENDCASE
  SELECT (lcGlDTemp)
  REPLACE ALL GLSESSION WITH lcSession
  SELECT GLDIST
  APPEND FROM (gcWorkDir+lcGlDTemp)
  SELECT (lcGlDTemp)
  ZAP
  SET ORDER TO TAG GL_LINK1 IN GL_LINK
ENDIF
WAIT CLEAR
SHOW GETS

*!*************************************************************
*! Name      : lfUpdWip
*! Developer : AHMED MAHER (AMH)
*! Date      : 06/03/2002
*! Purpose   : Function to update the nmatwip field in fabric & fabdye files.
*!*************************************************************
*B606028,1
*!*************************************************************
FUNCTION lfUpdWip

*E301897,1 AMH Add variables to save the current key of fabric and fabdye files [Start]
PRIVATE lcFabKey,lcFabDKey,lnAlias
lnAlias = SELECT(0)
SELECT FABRIC
lcFabKey = EVALUATE(KEY())
SELECT FABDYE
lcFabDKey = EVALUATE(KEY())
*E301897,1 AMH [End]

PRIVATE lcTmpWip,lcTmpWip1
lcTmpWip  = gfTempName()
lcTmpWip1 = gfTempName()
CREATE CURSOR (lcTmpWip) (CWARECODE C(6),NMATWIP N(12,3),DTRANDATE D,CISESSION C(6))
INDEX ON DTOS(DTRANDATE)+CWARECODE TAG (lcTmpWip) OF (lcTmpWip)
INDEX ON CWARECODE+CISESSION TAG (lcTmpWip1) OF (lcTmpWip)
SET ORDER TO TAG (lcTmpWip) IN (lcTmpWip)
PRIVATE lcOrder1,lcOrder2,lnRecQty
lcOrder1 = ORDER('CTKTBOM')
lcOrder2 = ORDER('BOMLINE')
SET ORDER TO TAG CTKTYP IN CTKTBOM
SET ORDER TO TAG BOMLINE IN BOMLINE

IF 'MA' $ gcComp_mdl AND !USED('MATINVJL')
  =gfOpenFile(gcDataDir+'MATINVJL','MATINVJL','SH')
ENDIF
IF SEEK(lcTranType+laData[1],'CTKTBOM')
  SELECT CTKTBOM
  SCAN REST WHILE cImTyp+Cuttkt+Item+Iclr+MfgCode+Dyelot = lcTranType+laData[1];
                  FOR CCATGTYP $ 'FT'
    lnRecQty = 0
    IF SEEK(CIMTYP+'2'+CUTTKT,'BOMLINE')
      SELECT BOMLINE
      SUM REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode =;
                     CTKTBOM.CIMTYP+'2'+CTKTBOM.CUTTKT;
               FOR ITEM = CTKTBOM.ITEM .AND. ICLR = CTKTBOM.ICLR .AND.;
                   DYELOT = CTKTBOM.DYELOT .AND. !EMPTY(CRSESSION);
               ITEMQTY TO lnRecQty
    ENDIF
    SELECT (lcTmpWip)
    ZAP
    IF SEEK(PADR(CTKTBOM.ITEM,7) + PADR(CTKTBOM.ICLR,6),'MATINVJL')
      SELECT MATINVJL
      SCAN REST WHILE cfabric+ccolor+cwarecode+cdyelot+crsession+cisession =;
                      PADR(CTKTBOM.ITEM,7) + PADR(CTKTBOM.ICLR,6) FOR CDYELOT = CTKTBOM.DYELOT .AND.;
                      CTRANTYPE = '4' .AND. CIMTYP = CTKTBOM.CIMTYP .AND. CTKTNO = CTKTBOM.CUTTKT
        IF NISSUED > 0
          INSERT INTO (lcTmpWip) (CWARECODE,NMATWIP,DTRANDATE,CISESSION);
                          VALUES (MATINVJL.CWARECODE,MATINVJL.NISSUED,;
                                  MATINVJL.DTRANDATE,MATINVJL.CISESSION)
        ELSE
          SELECT (lcTmpWip)
          SET ORDER TO TAG (lcTmpWip1)
          IF SEEK(MATINVJL.CWARECODE+MATINVJL.CISESSION)
            REPLACE NMATWIP WITH NMATWIP - MATINVJL.NRECEIVED
          ELSE
            IF SEEK(MATINVJL.CWARECODE)
              lnRemender = MATINVJL.NRECEIVED
              SCAN REST WHILE CWARECODE+CISESSION = MATINVJL.CWARECODE
                lnMatWip = NMATWIP
                REPLACE NMATWIP WITH MAX(NMATWIP-lnRemender,0)
                lnRemender = MAX(lnRemender-lnMatWip,0)
                IF lnRemender <= 0
                  EXIT
                ENDIF
              ENDSCAN
            ENDIF
          ENDIF
          SET ORDER TO TAG (lcTmpWip)
        ENDIF
      ENDSCAN
    ENDIF
    IF RECCOUNT(lcTmpWip) > 0
      SELECT (lcTmpWip)
      LOCATE
      lnRemender = lnRecQty
      SCAN
        IF SEEK(PADR(CTKTBOM.ITEM,7) + PADR(CTKTBOM.ICLR,6),'FABRIC')
          REPLACE FABRIC.NMATWIP WITH FABRIC.NMATWIP - MAX(NMATWIP-lnRemender,0)
        ENDIF
        IF SEEK(PADR(CTKTBOM.ITEM,7)+PADR(CTKTBOM.ICLR,6)+CWARECODE+CTKTBOM.DYELOT,'FABDYE')
          REPLACE FABDYE.NMATWIP WITH FABDYE.NMATWIP - MAX(NMATWIP-lnRemender,0)
        ENDIF
        IF !EMPTY(CTKTBOM.DYELOT) .AND. SEEK(PADR(CTKTBOM.ITEM,7)+PADR(CTKTBOM.ICLR,6)+CWARECODE+SPACE(10),'FABDYE')
          REPLACE FABDYE.NMATWIP WITH FABDYE.NMATWIP - MAX(NMATWIP-lnRemender,0)
        ENDIF
        lnRemender = MAX(lnRemender-NMATWIP,0)
      ENDSCAN
    ENDIF
  ENDSCAN
ENDIF
SET ORDER TO TAG &lcOrder1. IN CTKTBOM
SET ORDER TO TAG &lcOrder2. IN BOMLINE
USE IN (lcTmpWip)

*E301897,1 AMH Restore the fabric and fabdye record [Start]
=SEEK(lcFabKey,'FABRIC')
=SEEK(lcFabDKey,'FABDYE')
SELECT (lnAlias)
*E301897,1 AMH [End]

*!*************************************************************
*! Name      : lfRetWip
*! Developer : AHMED MAHER (AMH)
*! Date      : 06/03/2002
*! Purpose   : Function to update the nmatwip field in fabric & fabdye files when return
*!*************************************************************
*B606028,1
*!*************************************************************
FUNCTION lfRetWip

PRIVATE lcTmpWip,lcTmpWip1
lcTmpWip  = gfTempName()
lcTmpWip1 = gfTempName()
CREATE CURSOR (lcTmpWip) (CWARECODE C(6),NMATWIP N(12,3),DTRANDATE D,CISESSION C(6))
INDEX ON DTOS(DTRANDATE)+CWARECODE TAG (lcTmpWip) OF (lcTmpWip)
INDEX ON CWARECODE+CISESSION TAG (lcTmpWip1) OF (lcTmpWip)
SET ORDER TO TAG (lcTmpWip) IN (lcTmpWip)
PRIVATE lcOrder1,lcOrder2,lnRecQty
lcOrder1 = ORDER('CTKTBOM')
lcOrder2 = ORDER('BOMLINE')
SET ORDER TO TAG CTKTYP IN CTKTBOM
SET ORDER TO TAG BOMLINE IN BOMLINE
IF !USED('MATINVJL')
  =gfOpenFile(gcDataDir+'MATINVJL','MATINVJL','SH')
ENDIF
IF SEEK(lcFabric+lcColor,'FABRIC')
  SELECT FABRIC
  REPLACE NMATWIP WITH NMATWIP - lnIssued
ENDIF
IF SEEK(lcFabric+lcColor+lcWareCode+lcDyelot,'FABDYE')
  SELECT FABDYE
  REPLACE NMATWIP WITH NMATWIP - lnIssued
ENDIF
IF !EMPTY(lcDyelot) .AND. SEEK(lcFabric+lcColor+lcWareCode+SPACE(10),'FABDYE')
  SELECT FABDYE
  REPLACE NMATWIP WITH NMATWIP - lnIssued
ENDIF
IF SEEK(lcTranType+laData[1],'CTKTBOM')
  SELECT CTKTBOM
  SCAN REST WHILE cImTyp+Cuttkt+Item+Iclr+MfgCode+Dyelot = lcTranType+laData[1];
                  FOR CCATGTYP $ 'FT'
    lnRecQty = 0
    IF SEEK(CIMTYP+'2'+CUTTKT,'BOMLINE')
      SELECT BOMLINE
      SUM REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode =;
                     CTKTBOM.CIMTYP+'2'+CTKTBOM.CUTTKT;
               FOR ITEM = CTKTBOM.ITEM .AND. ICLR = CTKTBOM.ICLR .AND.;
                   DYELOT = CTKTBOM.DYELOT .AND. !EMPTY(CRSESSION);
               ITEMQTY TO lnRecQty
    ENDIF
    SELECT (lcTmpWip)
    ZAP
    IF SEEK(PADR(CTKTBOM.ITEM,7) + PADR(CTKTBOM.ICLR,6),'MATINVJL')
      SELECT MATINVJL
      SCAN REST WHILE cfabric+ccolor+cwarecode+cdyelot+crsession+cisession =;
                      PADR(CTKTBOM.ITEM,7) + PADR(CTKTBOM.ICLR,6) FOR CDYELOT = CTKTBOM.DYELOT .AND.;
                      CTRANTYPE = '4' .AND. CIMTYP = CTKTBOM.CIMTYP .AND. CTKTNO = CTKTBOM.CUTTKT
        IF NISSUED > 0
          INSERT INTO (lcTmpWip) (CWARECODE,NMATWIP,DTRANDATE,CISESSION);
                          VALUES (MATINVJL.CWARECODE,MATINVJL.NISSUED,;
                                  MATINVJL.DTRANDATE,MATINVJL.CISESSION)
        ELSE
          SELECT (lcTmpWip)
          SET ORDER TO TAG (lcTmpWip1)
          IF SEEK(MATINVJL.CWARECODE+MATINVJL.CISESSION)
            REPLACE NMATWIP WITH NMATWIP - MATINVJL.NRECEIVED
          ELSE
            IF SEEK(MATINVJL.CWARECODE)
              lnRemender = MATINVJL.NRECEIVED
              SCAN REST WHILE CWARECODE+CISESSION = MATINVJL.CWARECODE
                lnMatWip = NMATWIP
                REPLACE NMATWIP WITH MAX(NMATWIP-lnRemender,0)
                lnRemender = MAX(lnRemender-lnMatWip,0)
                IF lnRemender <= 0
                  EXIT
                ENDIF
              ENDSCAN
            ENDIF
          ENDIF
          SET ORDER TO TAG (lcTmpWip)
        ENDIF
      ENDSCAN
    ENDIF
    IF RECCOUNT(lcTmpWip) > 0
      SELECT (lcTmpWip)
      LOCATE
      lnRemender = lnRecQty
      SCAN
        IF SEEK(PADR(CTKTBOM.ITEM,7) + PADR(CTKTBOM.ICLR,6),'FABRIC')
          REPLACE FABRIC.NMATWIP WITH FABRIC.NMATWIP + MAX(NMATWIP-lnRemender,0)
        ENDIF
        IF SEEK(PADR(CTKTBOM.ITEM,7)+PADR(CTKTBOM.ICLR,6)+CWARECODE+CTKTBOM.DYELOT,'FABDYE')
          REPLACE FABDYE.NMATWIP WITH FABDYE.NMATWIP + MAX(NMATWIP-lnRemender,0)
        ENDIF
        IF !EMPTY(CTKTBOM.DYELOT) .AND. SEEK(PADR(CTKTBOM.ITEM,7)+PADR(CTKTBOM.ICLR,6)+CWARECODE+SPACE(10),'FABDYE')
          REPLACE FABDYE.NMATWIP WITH FABDYE.NMATWIP + MAX(NMATWIP-lnRemender,0)
        ENDIF
        lnRemender = MAX(lnRemender-NMATWIP,0)
      ENDSCAN
    ENDIF
  ENDSCAN
ENDIF
SET ORDER TO TAG &lcOrder1. IN CTKTBOM
SET ORDER TO TAG &lcOrder2. IN BOMLINE
USE IN (lcTmpWip)

*!*************************************************************
*! Name      : lfvEditUse
*! Developer : AHMED MAHER
*! Date      : 06/19/2002
*! Purpose   : Edit Used Quantity for Cost Item
*!*************************************************************
*! Calls     : MFITMISS.SPX,MFSTYISS.SPX,MFMFGISS.SPX,lfShowLog
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvEditUse()
*!*************************************************************
*E301897,1 AMH
FUNCTION lfvEditUse
PRIVATE lcBrFields
ldIssDate = gdSysDate
DO CASE
  CASE INLIST(m.cCatgTyp,'F','T') .AND. SEEK(SUBSTR(m.Item,1,7)+m.IClr,'Fabric')
    llItemDye = laSetups[12,2]='Y' .AND. Fabric.cDye_Flg = 'Y' 
    llOk = .F.
    SCATTER MEMVAR
    IF INLIST(laSetups[9,2],'L','F','I')
      *-- undeveloped yet.
    ELSE  
    *--- DOC.
    *--- If the cost method is average.
    *--- DOC.
      lcIssWare   = laData[32]
 
      *B607271,1 KHM 06/04/2003 (Begin) Use the ceiling function.
      *lnIssWare   = ASCAN(laMatWare,lcIssWare)/2
      lnIssWare   = CEILING(ASCAN(laMatWare,lcIssWare)/2)
      *B607271,1 KHM 06/04/2003 (Begin) Use the ceiling function.
 
      lcWareStat  = 'ENABLE'
      =SEEK(SUBSTR(m.Item,1,7)+m.IClr+lcIssWare+SPACE(10),'FabDye')
      lnIssCost   = IIF(laSetups[9,2]='A',FabDye.nAveCstBuy,Fabric.CostBuy)/Fabric.Conv
      lnTotQty    = m.Used_Qty
      DO (gcScrDir+"MFITMISS.SPX") WITH .F.,.F.,.T.
      IF !llOk
        m.Used_Qty = lnTotQty
      ENDIF
    ENDIF
    lnOnHand   = IIF(EMPTY(cWareCode),FABRIC.OnHand,FABDYE.OnHand)
  CASE m.cCatgTyp='S' .AND. SEEK(m.Item,'Style') .AND. SEEK('S'+Style.Scale,'Scale')
    *-- undeveloped yet.
  CASE INLIST(m.cCatgTyp,'M','P','D')
    *-- undeveloped yet.
  OTHERWISE
    *-- undeveloped yet.
ENDCASE
=lfRefresh('MFISLOG0')
*B123774,1 MHM 10/12/2004 Refresh the screen with new temp[Start]
*=lfShowLog()
=lfBrowLog()
*B123774,1 MHM [End]

*!*************************************************************
*! Name      : lfvUsedQty
*! Developer : AHMED MAHER
*! Date      : 06/19/2002
*! Purpose   : Validate the used quantity field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvUsedQty()
*!*************************************************************
*E301897,1 AMH
FUNCTION lfvUsedQty

IF m.Used_Qty < lnTotQty
  SHOW GET lnIssCost ENABLE
ELSE
  SHOW GET lnIssCost DISABLE
ENDIF

*!*************************************************************
*! Name      : lfAct1Opr
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/08/2002
*! Purpose   : Get Bar number in the system menu
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  Bar Number
*!*************************************************************
*! Example   :  =lfAct1Opr()
*!*************************************************************
*E302004,1 AMH
FUNCTION lfAct1Opr

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpTkt)
SUM ALL nTotQty TO lnLotbud

SELECT (lcOprDet)

*E302004,1 AMH Save the current record No. [Start]
PRIVATE lnRecNo
lnRecNo = RECNO()
*E302004,1 AMH [End]

lcCurrTag = ORDER(lcOprDet)
SET ORDER TO TAG 'LOT'
=SEEK(lcTranType+laData[1]+lcOprCode)
SUM REST IIF(TranCd='1',nLotTotQty,0) TO lnLotRcv;
WHILE    cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
         lcTranType+laData[1]+lcOprCode

*E302004,1 AMH Save the current record No. [Start]
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF
*E302004,1 AMH [End]

SELECT (lnAlias)
SET ORDER TO TAG lcCurrTag IN (lcOprDet)
lcStatColor=IIF(lnLotbud-lnLotRcv-lnLotCan-lnLotDmg>0,"RGB(255,255,255,255,0,0)",gcObjColor)

*!*************************************************************
*! Name      : lfvOkAct1
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/08/2002
*! Purpose   : Actualize the whole cutting ticket for one operation.
*!*************************************************************
*! Calls     : lfBrowLots,lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvOkAct()
*!*************************************************************
*E302004,1 AMH
FUNCTION lfvOkAct1

PRIVATE lnAlias,lcLotNo,lcItem,lcColor,laOpen,llInHouse,;
        lcContCode,lcContName,lcCurrTag,lnUpdYield,lcKeyVal
DECLARE laOpen[9],laOrd[8]
lnAlias  = SELECT(0)
lcCurrTag= ORDER(lcOprDet)
SET ORDER TO TAG LOT IN (lcOprDet)
STORE .F. TO llTktAllo,llRelAll
IF lcTranType <> 'T'
  SELECT (lcOprDet)
  =SEEK(lcTranType+laData[1]+lcFirstOpr)
  DO WHILE !llTktAllo AND ;
           cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
           lcTranType+laData[1]+lcFirstOpr
    lcItem  = Item
    lcLotNo = cLotNo
    STORE 0 TO laOpen,laOrd
    SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
                    lcTranType+laData[1]+lcFirstOpr+lcLotNo+lcItem FOR cDyelot = &lcOprDet..cDyelot
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        laOpen[lnCount] = laOpen[lnCount] + EVALUATE('nLotQty'+lcCount)
      ENDFOR
    ENDSCAN
    SELECT (lcTranFile)
    
    *khm1    
    *=SEEK(IIF(lcTranType='I',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1])
    *SUM REST WHILE EVAL(lcFileKey) = IIF(lcTranType='I',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1] ;
    Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8 TO ARRAY laOrd FOR TRANCD = '1' .AND. STYLE = lcItem
    
    =SEEK(IIF(lcTranType $ 'ID',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1])
    SUM REST WHILE EVAL(lcFileKey) = IIF(lcTranType $ 'ID',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1] ;
    Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8 TO ARRAY laOrd FOR TRANCD = '1' .AND. STYLE = lcItem
    *khm1
    
    FOR lnCount = 1 TO 8
      IF laOrd[lnCount] > laOpen[lnCount]
        llTktAllo = .T.
        EXIT
      ENDIF
    ENDFOR
    SELECT (lcOprDet)
  ENDDO
  *-- Message : 38168
  *-- Cutting ticket# 999999 has pieces allocated from orders.
  *-- Proceed with acualization and modify the allocated 
  *-- quantity from order lines to keep track of this 
  *-- allocation, or cancel
  *-- Button : 38022
  *-- < Proceed> <Cancel>
  IF llTktAllo AND gfModalGen('QRM38168B38002','ALERT',;
     IIF(lcTranType='M','Cutting Ticket#: ','PO#:')+laData[1]) = 2
    SELECT (lnAlias)
    SET ORDER TO TAG lcCurrTag IN (lcOprDet)
    RETURN
  ENDIF
ENDIF

lnUpdYield = 0

IF SEEK(lcTranType+laData[1]+lcFirstOpr,lcOprDet)
  SELECT (lcOprDet)
  DO WHILE !EOF() .AND. cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
           lcTranType+laData[1]+lcFirstOpr
    lcItem     = Item
    lcColor    = Color
    llInHouse  = lInHouse
    lcContCode = cContCode
    lcContName = cContName
    lcLotNo    = cLotNo
    lcItDye    = cDyelot
    STORE 0 TO laOpen
    llActualize = .T.
    lcNxtDye = lcItDye
    IF llUseDyelot
      SKIP 1
      IF cDyelot # lcItDye .AND. TranCd = '1'
        lcNxtDye = cDyelot
      ENDIF
      SKIP -1
    ENDIF
    SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
                    lcTranType+laData[1]+lcFirstOpr+lcLotNo+lcItem+lcColor ;
              FOR   llActualize .AND. cDyelot = lcItDye
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        laOpen[lnCount] = laOpen[lnCount] + EVALUATE('nLotQty'+lcCount)
        laOpen[9] = laOpen[9] + laOpen[lnCount]
      ENDFOR
    ENDSCAN
    IF !llActualize
      LOOP
    ENDIF
    lcKeyVal=cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd+cDyelot
    SELECT MFGOPRDT
    =SEEK(lcTranType+laData[1]+lcFirstOpr+lcLotNo+'1')
    LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd = ;
                      lcTranType+laData[1]+lcFirstOpr+lcLotNo+'1' ;
                FOR   Item+Color+cDyelot = lcItem+lcColor+lcItDye
    SELECT (lcTmpTkt)
    =SEEK(lcItem+lcColor)
    COUNT REST WHILE Item+Color = lcItem+lcColor FOR cDyelot = lcItDye TO lnRecords
    IF lnRecords=1 
      =SEEK(lcItem+lcColor)
      IF llUseDyelot
        LOCATE REST WHILE Item+Color = lcItem+lcColor FOR cDyelot = lcItDye 
      ENDIF
      REPLACE nQty1   WITH laOpen[1] ,;
              nQty2   WITH laOpen[2] ,;
              nQty3   WITH laOpen[3] ,;
              nQty4   WITH laOpen[4] ,;
              nQty5   WITH laOpen[5] ,;
              nQty6   WITH laOpen[6] ,;
              nQty7   WITH laOpen[7] ,;
              nQty8   WITH laOpen[8] ,;
              nTotQty WITH nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8
    ELSE
      SELECT *,00000 AS nCan1,00000 AS nCan2,00000 AS nCan3,00000 AS nCan4,;
               00000 AS nCan5,00000 AS nCan6,00000 AS nCan7,00000 AS nCan8,;
               000000 AS nTotCan, RECNO() AS nBudRecNo FROM (lcTmpTkt) ;
               WHERE ITEM+COLOR+cDyelot = lcItem+lcColor+lcItDye ;
               INTO DBF (gcWorkDir+lcisslogfile)
      STORE MAX(laOpen[1],0) TO lnCan1,lnBal1
      STORE MAX(laOpen[2],0) TO lnCan2,lnBal2
      STORE MAX(laOpen[3],0) TO lnCan3,lnBal3
      STORE MAX(laOpen[4],0) TO lnCan4,lnBal4
      STORE MAX(laOpen[5],0) TO lnCan5,lnBal5
      STORE MAX(laOpen[6],0) TO lnCan6,lnBal6
      STORE MAX(laOpen[7],0) TO lnCan7,lnBal7
      STORE MAX(laOpen[8],0) TO lnCan8,lnBal8
      STORE lnCan1+lnCan2+lnCan3+lnCan4+lnCan5+lnCan6+lnCan7+lnCan8 TO lnTotCan,lnTotBal
      SCATTER MEMVAR
      =SEEK(lcItem,'Style') .AND. SEEK('S'+Style.Scale,'Scale')
      lcConCnTit = IIF(lcTranType='T','Material/Color',lcItmHdr)+' '+;
                   ALLTRIM(lcItem)+ALLTRIM(lcColor)+'Cancelled Quantity'
      DO (gcScrDir+"MFCONCN.SPX")
      USE IN (lcisslogfile)
    ENDIF
    SELECT (lcTmpTkt)
    =SEEK(lcItem+lcColor)
    SCAN REST WHILE Item+Color+STR(LineNo,6) = lcItem+lcColor  FOR cDyelot = lcItDye 
      DO CASE
        CASE lcTranType='M'
          SET ORDER TO TAG CUTLIN IN CUTTKTL
          =SEEK(laData[1]+Item+STR(lineno,6)+'1','CUTTKTL')
          SELECT CUTTKTL
          SCATTER FIELDS ORD1,ORD2,ORD3,ORD4,ORD5,ORD6,ORD7,ORD8,TotOrd TO laOrdQty
          SELECT (lcTmpTkt)
          FOR lnCount = 1 TO 8
            lcCount = STR(lnCount,1)
            IF laOrdQty[1] > EVALUATE('nQty'+lcCount)
              lnOpen1 = CUTTKTL.QTY1 - laOpen[1]
              lnOpen2 = CUTTKTL.QTY2 - laOpen[2]
              lnOpen3 = CUTTKTL.QTY3 - laOpen[3]
              lnOpen4 = CUTTKTL.QTY4 - laOpen[4]
              lnOpen5 = CUTTKTL.QTY5 - laOpen[5]
              lnOpen6 = CUTTKTL.QTY6 - laOpen[6]
              lnOpen7 = CUTTKTL.QTY7 - laOpen[7]
              lnOpen8 = CUTTKTL.QTY8 - laOpen[8]
              =lfTktAllo(lcItem,CutTKtL.Dyelot,CutTKtL.LineNo)
              EXIT
            ENDIF
          ENDFOR
          SELECT CUTTKTH
          =RLOCK()
          REPLACE TotOrd WITH TotOrd - CutTktl.TotOrd + laOrdQty[9]
          UNLOCK
          SELECT STYLE
          =SEEK(CUTTKTL.Style)
          =RLOCK()
          FOR lnCount = 1 TO 8
            lcCount = STR(lnCount,1)
            REPLACE ('WIP'+lcCount) WITH EVALUATE('WIP'+lcCount+' - CUTTKTL.Qty'+lcCount+;
                                                  ' + '+lcTmpTkt+'.nQty'+lcCount)
          ENDFOR
          REPLACE TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
          UNLOCK
          SELECT STYDYE
          =SEEK(CUTTKTL.Style+CutTktl.cWareCode+SPACE(10))
          =RLOCK()
          FOR lnCount = 1 TO 8
            lcCount = STR(lnCount,1)
            REPLACE ('WIP'+lcCount) WITH EVALUATE('WIP'+lcCount+' - CUTTKTL.Qty'+lcCount+;
                                                  ' + '+lcTmpTkt+'.nQty'+lcCount)
          ENDFOR
          REPLACE TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
          UNLOCK
          SELECT CUTTKTL
          lnTotQty = Totqty
          =RLOCK()
          FOR lnCount = 1 TO 8
            lcCount = STR(lnCount,1)
            REPLACE ('QTY'+lcCount) WITH EVALUATE(lcTmpTkt+'.nQty'+lcCount)
          ENDFOR
          REPLACE TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
          REPLACE Ord1   WITH laOrdQty[1] ,;
                  Ord2   WITH laOrdQty[2] ,;
                  Ord3   WITH laOrdQty[3] ,;
                  Ord4   WITH laOrdQty[4] ,;
                  Ord5   WITH laOrdQty[5] ,;
                  Ord6   WITH laOrdQty[6] ,;
                  Ord7   WITH laOrdQty[7] ,;
                  Ord8   WITH laOrdQty[8] ,;
                  TotOrd WITH laOrdQty[9]
          UNLOCK
          laData[6]  = MAX(CutTktH.Pcs_Bud,CutTktH.Pcs_Bud+(CutTktl.TotQty-lnTotQty ))
          laData[10] = MAX(ladata[6] - (lnLotRcv+ladata[8]+ladata[9]),0)
          SELECT CutTktH
          REPLACE Pcs_Bud WITH laData[6],;
                  PCS_OPN WITH laData[10]
          SELECT CUTTKTL
          laData[8]  = laData[8]  + MAX(lnTotQty-CutTktL.TotQty,0)
          laData[12] = laData[12] + (CutTktL.TotQty-lnTotQty)*CutTktL.nCost1
          laData[13] = laData[13] + (CutTktL.TotQty-lnTotQty)*CutTktL.nCost2
          laData[14] = laData[14] + (CutTktL.TotQty-lnTotQty)*CutTktL.nCost3
          laData[15] = laData[15] + (CutTktL.TotQty-lnTotQty)*CutTktL.nCost4
          laData[16] = laData[16] + (CutTktL.TotQty-lnTotQty)*CutTktL.nCost5
        *khm1
        *CASE lcTranType='I'
        CASE lcTranType $ 'ID'
        *khm1        
          SET ORDER TO TAG POSLN IN POSLN
          =SEEK(IIF(EMPTY(lcDyePO),'P','D')+laData[1]+lcItem+STR(lineno,6)+'1','POSLN')
          SELECT POSLN
          SCATTER FIELDS ORD1,ORD2,ORD3,ORD4,ORD5,ORD6,ORD7,ORD8,TotOrd TO laOrdQty
          SELECT (lcTmpTkt)
          FOR lnCount = 1 TO 8
            lcCount = STR(lnCount,1)
            IF laOrdQty[1] > EVALUATE('nQty'+lcCount)
              lnOpen1 = POSLN.QTY1 - laOpen[1]
              lnOpen2 = POSLN.QTY2 - laOpen[2]
              lnOpen3 = POSLN.QTY3 - laOpen[3]
              lnOpen4 = POSLN.QTY4 - laOpen[4]
              lnOpen5 = POSLN.QTY5 - laOpen[5]
              lnOpen6 = POSLN.QTY6 - laOpen[6]
              lnOpen7 = POSLN.QTY7 - laOpen[7]
              lnOpen8 = POSLN.QTY8 - laOpen[8]
              =lfTktAllo(lcItem,POSLN.Dyelot,POSLN.LineNo)
              EXIT
            ENDIF
          ENDFOR
          SELECT POSHDR
          =RLOCK()
          REPLACE TotOrd WITH TotOrd - POSLN.TotOrd + laOrdQty[9]
          UNLOCK
          SELECT STYLE
          =SEEK(POSLN.Style)
          =RLOCK()
          FOR lnCount = 1 TO 8
            lcCount = STR(lnCount,1)
            REPLACE ('WIP'+lcCount) WITH EVALUATE('WIP'+lcCount+' - POSLN.Qty'+lcCount+;
                                                  ' + '+lcTmpTkt+'.nQty'+lcCount)
          ENDFOR
          REPLACE TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
          UNLOCK
          SELECT STYDYE
          =SEEK(POSLN.Style+POSLN.cWareCode+SPACE(10))
          =RLOCK()
          FOR lnCount = 1 TO 8
            lcCount = STR(lnCount,1)
            REPLACE ('WIP'+lcCount) WITH EVALUATE('WIP'+lcCount+' - POSLN.Qty'+lcCount+;
                                                  ' + '+lcTmpTkt+'.nQty'+lcCount)
          ENDFOR
          REPLACE TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
          UNLOCK
          SELECT POSLN
          lnTotQty = Totqty
          =RLOCK()
          FOR lnCount = 1 TO 8
            lcCount = STR(lnCount,1)
            REPLACE ('QTY'+lcCount) WITH EVALUATE(lcTmpTkt+'.nQty'+lcCount)
          ENDFOR
          REPLACE TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
          REPLACE Ord1   WITH laOrdQty[1] ,;
                  Ord2   WITH laOrdQty[2] ,;
                  Ord3   WITH laOrdQty[3] ,;
                  Ord4   WITH laOrdQty[4] ,;
                  Ord5   WITH laOrdQty[5] ,;
                  Ord6   WITH laOrdQty[6] ,;
                  Ord7   WITH laOrdQty[7] ,;
                  Ord8   WITH laOrdQty[8] ,;
                  TotOrd WITH laOrdQty[9]
          UNLOCK
          SELECT POSHDR
          =RLOCK()
          REPLACE nFCost1 WITH nFCost1 + (POSLN.TotQty-lnTotQty)*POSLN.nCost1 ,;
                  nFCost2 WITH nFCost2 + (POSLN.TotQty-lnTotQty)*POSLN.nCost2 ,;
                  nFCost3 WITH nFCost3 + (POSLN.TotQty-lnTotQty)*POSLN.nCost3 ,;
                  nFCost4 WITH nFCost4 + (POSLN.TotQty-lnTotQty)*POSLN.nCost4 ,;
                  nFCost5 WITH nFCost5 + (POSLN.TotQty-lnTotQty)*POSLN.nCost5
          UNLOCK
          laData[8]  = laData[8]  + MAX(lnTotQty-POSLN.TotQty,0)
          laData[12] = laData[12] + (POSLN.TotQty-lnTotQty)*POSLN.nECost1
          laData[13] = laData[13] + (POSLN.TotQty-lnTotQty)*POSLN.nECost2
          laData[14] = laData[14] + (POSLN.TotQty-lnTotQty)*POSLN.nECost3
          laData[15] = laData[15] + (POSLN.TotQty-lnTotQty)*POSLN.nECost4
          laData[16] = laData[16] + (POSLN.TotQty-lnTotQty)*POSLN.nECost5
        CASE lcTranType='T'
          SELECT MMFGORDD
          SET ORDER TO TAG MMFGORDD
          =SEEK(laData[1]+SUBSTR(lcItem,1,7)+lcColor,'MMFGORDD')
          LOCATE REST WHILE cMfgOrdNo+cFabric+Color+Dyelot+TranCd=;
                            laData[1]+SUBSTR(lcItem,1,7)+lcColor FOR TranCd = '1'
          SELECT FABRIC
          =SEEK(MMFGORDD.cFabric+MMFGORDD.Color)
          =RLOCK()
          REPLACE OnOrder WITH OnOrder - laOpen[9]
          UNLOCK
          SELECT FABDYE
          =SEEK(MMFGORDD.cFabric+MMFGORDD.Color+MMFGORDD.cWareCode+SPACE(10))
          =RLOCK()
          REPLACE OnOrder WITH OnOrder - laOpen[9]
          UNLOCK
          SELECT MMFGORDD
          lnTotQty = nMfgTotQty
          =RLOCK()
          REPLACE nMfgTotQty WITH MAX(nMfgTotQty - laOpen[9],0)
          UNLOCK
          laData[8]  = laData[8]  + MAX(laOpen[9],0)
          laData[12] = laData[12] + (MMFGORDD.nMfgTotQty-lnTotQty)*MMFGORDD.nCost1
          laData[13] = laData[13] + (MMFGORDD.nMfgTotQty-lnTotQty)*MMFGORDD.nCost2
          laData[14] = laData[14] + (MMFGORDD.nMfgTotQty-lnTotQty)*MMFGORDD.nCost3
          laData[15] = laData[15] + (MMFGORDD.nMfgTotQty-lnTotQty)*MMFGORDD.nCost4
      ENDCASE
      
      *-- Update Cost Items Required Quantities upon actualization
      *-- Message : 38157
      *-- Modify the cost sheet basaed on the actual quantity?
      *-- Button : 38006
      *-- < Yes >  < No >
      
      lnUpdYield = IIF(lnUpdYield=0 AND laOpen[9] <> 0,;
                       gfModalGen('QRM38157B38006','ALERT'),lnUpdYield)
      IF lnUpdYield = 1

        SELECT BOMLINE
        =SEEK(lcTranType+'1'+laData[1]+STR(&lcTmpTkt..lineno,6))
        DECLARE laReq[8]
        SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
                        lcTranType+'1'+laData[1]+STR(&lcTmpTkt..lineno,6)
          STORE 0 TO laReq
          IF lcTranType = 'T'
            lnStyQty = laOpen[9]
          ELSE
            lnStyQty = 0
            FOR lnCount = 1 TO 8
              lcCount = STR(lnCount,1)
              IF lcCount $ cSizes
                lnStyQty   = lnStyQty + laOpen[lnCount]
                lnCompSize = VAL(SUBSTR(cCompSizes,AT(lcCount,cSizes),1))
                IF lnCompSize # 0
                  laReq[lnCompSize] = laReq[lnCompSize] + laOpen[lnCount]
                ENDIF
              ENDIF
            ENDFOR
          ENDIF
          lnOldQty = StyQty
          REPLACE StyQty  WITH lnStyQty ,;
                  UnitQty WITH IIF(cCatgtyp='F' AND cOprCode=lcFirstOpr,IIF(StyQty=0,0,ItemQty/StyQty),UnitQty) ,;
                  ItemQty WITH StyQty*UnitQty  ,; 
                  ItemAmt WITH ItemQty*UnitCost
          SELECT (lcDetFile)        
          =SEEK(BomLine.Style+BomLine.SClr+STR(BomLine.LineNo,6)+BomLine.cBomTyp+;
                '1'+BomLine.Item+BomLine.IClr+BomLine.MfgCode)
          REPLACE StyQty  WITH lnStyQty ,;
                  UnitQty WITH IIF(cCatgtyp='F' AND cOprCode=lcFirstOpr,IIF(StyQty=0,0,ItemQty/StyQty),UnitQty) ,;
                  ItemQty WITH StyQty*UnitQty  ,; 
                  ItemAmt WITH ItemQty*UnitCost

          SELECT CTKTBOM
          =SEEK(lcTranType+laData[1]+Bomline.cBomTyp+Bomline.item+Bomline.iclr+Bomline.mfgcode+Bomline.Dyelot)
          REPLACE Pieces   WITH Pieces - lnOldQty + lnStyQty ,;
                  UntQty   WITH IIF(cCatgtyp='F' AND cOprCode=lcFirstOpr,IIF(Pieces=0,0,Req_qty/Pieces),UntQty) ,;
                  Req_qty  WITH Pieces*UntQty   ,;
                  Est_Cost WITH Req_qty*UntCost ,;
                  Req_Qty1 WITH laReq[1]*UntQty ,;
                  Req_Qty2 WITH laReq[2]*UntQty ,;
                  Req_Qty3 WITH laReq[3]*UntQty ,;
                  Req_Qty4 WITH laReq[4]*UntQty ,;
                  Req_Qty5 WITH laReq[5]*UntQty ,;
                  Req_Qty6 WITH laReq[6]*UntQty ,;
                  Req_Qty7 WITH laReq[7]*UntQty ,;
                  Req_Qty8 WITH laReq[8]*UntQty
          SELECT (lcTktSheet)
          =SEEK(CTKTBOM.TYP+'1'+CTKTBOM.ITEM+CTKTBOM.ICLR+CTKTBOM.MFGCODE+CTKTBOM.DYELOT)
          REPLACE Pieces   WITH Pieces - lnOldQty + lnStyQty ,;
                  UntQty   WITH IIF(cCatgtyp='F' AND cOprCode=lcFirstOpr,IIF(Pieces=0,0,Req_qty/Pieces),UntQty) ,;
                  Req_qty  WITH Pieces*UntQty ,;
                  Est_Cost WITH Req_qty*UntCost ,;
                  Req_Qty1 WITH laReq[1]*UntQty ,;
                  Req_Qty2 WITH laReq[2]*UntQty ,;
                  Req_Qty3 WITH laReq[3]*UntQty ,;
                  Req_Qty4 WITH laReq[4]*UntQty ,;
                  Req_Qty5 WITH laReq[5]*UntQty ,;
                  Req_Qty6 WITH laReq[6]*UntQty ,;
                  Req_Qty7 WITH laReq[7]*UntQty ,;
                  Req_Qty8 WITH laReq[8]*UntQty
        ENDSCAN
        GO TOP IN (lcDetFile)
        GO TOP IN (lcTktSheet)
      ENDIF
    ENDSCAN  
    laData[27] = laData[27] + MAX(laOpen[9]-MFGOPRDT.nActQty,0)
    laData[10] = MAX(ladata[6] - (ladata[7]+ladata[8]+ladata[9]),0)
    SELECT MFGOPRDT
    =RLOCK()
    REPLACE nActQty WITH laOpen[9]
    UNLOCK
    SELECT (lcOprDet)
    =SEEK(lcTranType+laData[1]+lcFirstOpr+lcLotNo+lcItem+lcColor+'1'+lcItDye)  
    REPLACE nActQty WITH laOpen[9]
    IF llUseDyelot .AND. lcNxtDye # lcItDye
      =SEEK(lcTranType+laData[1]+lcFirstOpr+lcLotNo+lcItem+lcColor+'1'+lcNxtDye)
    ELSE
      =SEEK(lcKeyVal)
    ENDIF
  ENDDO
  laData[3] = 'A'
  lcStatus = 'Actualized'
  SELECT (lcBaseFile)
  =RLOCK()
  GATHER FROM laData FIELDS &lcScFields
  REPLACE Act_Date WITH ldActDate
  UNLOCK
  IF ASCAN(laEvntTrig , PADR('EXPACTCT',10)) <> 0
    =gfDoTriger('MFCSSH',PADR('EXPACTCT',10))
  ENDIF
  SELECT (lcOprDet)
  ZAP
  SELECT MFGOPRDT
  SET ORDER TO TAG MFGOPRDT
  IF SEEK(lcTranType+laData[1])
    SCAN REST WHILE cimtyp+ctktno = lcTranType+laData[1]
      SCATTER MEMVAR
      INSERT INTO (lcOprDet) FROM MEMVAR
    ENDSCAN
  ENDIF
  GO TOP IN (lcOprDet)
  =lfBrowLots()
  SELECT (lnAlias)
ENDIF
SET ORDER TO TAG lcCurrTag IN (lcOprDet)

*!*************************************************************
*! Name      : lfvToOther
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/09/2002
*! Purpose   : Show Other quantity
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvToOther()
*!*************************************************************
*E302004,1 MAH
FUNCTION lfvToOther

lcVarName = SYS(18)
lcVarType = UPPER(SUBSTR(lcVarName,6,3))
lcVarSize = RIGHT(lcVarName,1)
SELECT (lcLotsDet)
lcItem = Item
lcColor = Color
lcDyelot = cDyelot
DO CASE
  CASE lcVarType = 'RCV'
    =SEEK(lcItem+lcColor+lcDyelot+'2')
  CASE lcVarType = 'CAN'
    =SEEK(lcItem+lcColor+lcDyelot+'4')
  CASE lcVarType = 'DMG'
    =SEEK(lcItem+lcColor+lcDyelot+'3')
ENDCASE
REPLACE ('nToIssue'+lcVarSize) WITH EVALUATE(lcVarName)
REPLACE nTotToIss WITH nToIssue1+nToIssue2+nToIssue3+nToIssue4+nToIssue5+;
                       nToIssue6+nToIssue7+nToIssue8
=SEEK(lcItem+lcColor+lcDyelot+'1')

*E302010,1 ALB Auto Contribute the non contribute lines in the AP invoice [Begin]
*!*************************************************************
*! Name      : lfContrbut
*! Developer : Albert Raif (ALB)
*! Date      : 01/22/2003
*! Purpose   : Contrbute the AP invoice lines
*! Tracking# : E302010,1
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfContrbut()
*!*************************************************************
*E,1 ALB

FUNCTION lfContrbut
PARAMETER lcTktType,lcPONo

DIMENSION laOpenFile[1,2]
STORE ' ' TO lcAPInvTkt,lcAPvInvDt,lcvInDtKey,lcActCstHd ,lcActCstDt
STORE .F. TO llDoContr,laOpenFile
lcAlias = ALIas()
lcAPInvTkt = gfTempName()
lcAPvInvDt = gfTempName()
lcActCstHd = gfTempName()   && Random name for the "Actual Cost" temp. header file
lcActCstDt = gfTempName()   && Random name for the "Actual Cost" temp. detail file

=lfOpenFiles()
SELECT (lcAPvInvDt)
SET ORDER TO ITEM
LOCATE
SCAN 
  lcInvDtKey = &lcAPvInvDt..cvendcode+&lcAPvInvDt..capinvno+&lcAPvInvDt..capvilno
  lcOprCode = cOprCode
  STORE 0 TO lnTotRec,lnCntrbAmnt
  IF lfvRSession(lcTktType,lcPONo,&lcAPvInvDt..Item,&lcAPvInvDt..Color,&lcAPvInvDt..cBomType) > 0
    SELECT (lcApInvTkt)
    SET ORDER TO (lcApInvTkt)
    IF SEEK(lcInvDtKey)
      SCAN REST WHILE cvendcode+capinvno+capvilno+crsession = ;
                      lcInvDtKey FOR EMPTY(crsession)

        REPLACE cStatus              WITH 'D' ,;
                &lcAPvInvDt..cStatus WITH 'M'
        SELECT RcvSess
        SUM nRecAmnt TO lnTotRec
        LOCATE
        SCAN
          lcRSession  = crsession
          lnContAmnt  = &lcAPvInvDt..nApAprAmnt/lnTotRec*nRecAmnt
          llDoContr   = lfDoApCont(lnContAmnt)
        ENDSCAN
      ENDSCAN
    ENDIF
  ENDIF
ENDSCAN

IF llDoContr AND lfChckACst()
  =lfUpdFiles()
ENDIF
=lfCloseFile()
select (lcAlias)
*!*************************************************************
*! Name      : lfDoApCont
*! Developer : ALBERT RAIF (ALB) 
*! Date      : 1/22/2003
*! Purpose   : Contrbute certen Line in APINVTKT
*! Tracking# : E302010,1
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfDoApCont()
*!*************************************************************
FUNCTION lfDoApCont
PARAMETER lnAmount
PRIVATE lcMfgCode , lcMfgTmp,lnMaxLine
lcVendCode = &lcAPvInvDt..cvendcode
lcApInvNo  = &lcAPvInvDt..capinvno
lcInvLineNo= &lcAPvInvDt..capvilno
lcContItem = &lcAPvInvDt..Item
lcCstType  = &lcAPvInvDt..cBomType
lcshipNo   = &lcAPvInvDt..ShipNo
lcTicket   = &lcAPvInvDt..cTktNo
SELECT (lcAPInvTkt)
lcMfgCode = lcOprCode
lnMaxLine = lfGetMaxLin(cvendcode,capinvno)


IF EMPTY(lcContItem) 
  IF INLIST(lcTktType,'S','I','M') AND lcOprCode=REPLICATE('*',6) AND ;
    IIF(lcTktType='S',SEEK('I2'+lcCstType+PADR('*'+lcCstType,6)+lcshipNo+lcRSession,'BOMLINE') OR ;
    SEEK('I2'+lcCstType+'******'+lcshipNo+lcRSession,'BOMLINE'),;
    SEEK(lcTktType+'2'+lcCstType+PADR('*'+lcCstType,6)+lcTicket+lcRSession,'BOMLINE') OR ;
    SEEK(lcTktType+'2'+lcCstType+'******'+lcTicket+lcRSession,'BOMLINE'))
    lcMfgCode = PADR(BOMLINE.MFGCODE,6)
  ENDIF
  IF INLIST(lcTktType,'S','I','M') AND lcOprCode=REPLICATE('#',6) AND ;
    IIF(lcTktType='S',SEEK('I2'+lcCstType+PADR('*'+lcCstType,6)+lcshipNo+lcRSession,'BOMLINE') OR ;
    SEEK('I2'+lcCstType+'######'+lcshipNo+lcRSession,'BOMLINE'),;
    SEEK(lcTktType+'2'+lcCstType+PADR('*'+lcCstType,6)+lcTicket+lcRSession,'BOMLINE') OR ;
    SEEK(lcTktType+'2'+lcCstType+'######'+lcTicket+lcRSession,'BOMLINE'))
    lcMfgCode = PADR(BOMLINE.MFGCODE,6)
  ENDIF              
ELSE
  IF INLIST(lcTktType,'S','I','M') AND lcOprCode=REPLICATE('*',6)
    =SEEK(PADR(lcContItem,19),'STYLE')
    lcMfgCode=IIF(!STYLE.lDetCost,PADR('*'+lcCstType,6),lcOprCode)
  ENDIF
  IF INLIST(lcTktType,'S','I','M') AND lcOprCode=REPLICATE('#',6)
    =SEEK(PADR(lcContItem,19),'STYLE')
    lcMfgCode=IIF(!STYLE.lDetCost,PADR('*'+lcCstType,6),lcOprCode)
  ENDIF      
ENDIF  

lcBomOrd = ORDER('BOMLINE')
llSeekBom = .F.
DO CASE
  CASE lcTktType  = 'S'
    lcFileInUse = 'POSLN'
    lcLnFldName = 'POSLN.nLan_Cst'
    lcAcFldName = 'POSLN.nAct_Cst'
    lcRcvQty    = 'POSLN.TotQty'
    SELECT POSLN
    SET ORDER TO TAG Poshrec
    lcForCond   = "LOOKUP(POSHDR.STATUS,'P'+BOMLINE.CTKTNO,POSHDR.PO,'POSHDR') $ 'ACO';
                   AND Style=ALLTRIM(&lcAPvInvDt..Item) AND crsession = lcRSession"
    lcBomCond = "Bomline.cimtyp + Bomline.ctype + Bomline.cbomtyp + Bomline.mfgcode +;
                 Bomline.shipno + Bomline.crsession +ctktno+STR(lineno,6)+style+sclr+cstygrade"
    lcBomKey  = 'I2'+lcCstType+lcMfgCode+lcShipNo
    SELECT BOMLINE
    SET ORDER TO TAG Bomshrec
    llSeekBom =SEEK(lcBomKey)
    lcCondition = 'SHIPNO+CRSESSION+CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD'
    lcKey       = lcShipNo+lcRSession
    lcItem   = 'PosLn.Style'
    lcGrade  = 'POSLN.cStyGrade'
    lcColor  = 'SPACE(6)'
    lcDyelot = 'SPACE(10)'
    lcHdrFile= 'POSHDR'
    lcHdrOred= 'Poshdr' 
    lcHdrKey = "P"
  CASE INLIST(lcTktType,'D')
    lcFileInUse = 'POSLN'
    lcLnFldName = 'POSLN.nLan_Cst'
    lcAcFldName = 'POSLN.nAct_Cst'
    lcRcvQty    = 'POSLN.TotQty'
    lcCondition = 'CSTYTYPE+PO+CRSESSION+SHIPNO+STYLE+STR(LINENO,6)+TRANCD'
    lcForCond   = ".T."
    SELECT POSLN
    SET ORDER TO TAG Posrec IN POSLN
    IF SEEK(lcTktType + lcTicket + lcRSession,'PosLn')
      lcShipNo = PosLn.ShipNo
    ENDIF
    lcKey      = lcTktType+lcTicket+lcRSession+lcShipNo+ALLTRIM(&lcAPvInvDt..Item)
    lcBomCond = "Bomline.cimtyp + Bomline.ctype + Bomline.cbomtyp + Bomline.mfgcode +;
                 Bomline.shipno + Bomline.crsession +ctktno+STR(lineno,6)+style+sclr+cstygrade"
    lcBomKey  = 'D2'+lcCstType+lcMfgCode+lcShipNo+lcRSession+lcTicket
    lcForCond = ".T. AND " +IIF(EMPTY(&lcAPvInvDt..Item),'.T.',;
                     "style = '"+&lcAPvInvDt..Item+"'")

    SELECT BOMLINE
    SET ORDER TO TAG Bomshrec
    llSeekBom =SEEK(lcBomKey)
    lcItem   = 'PosLn.Style'
    lcColor  = 'SPACE(6)'
    lcDyelot = 'SPACE(10)'
    lcGrade  = 'POSLN.cStyGrade'
    lcHdrFile= 'POSHDR'
    lcHdrOred= 'Poshdr'
    lcHdrKey = "D"

  CASE INLIST(lcTktType,'I','R')
    lcFileInUse = 'POSLN'
    lcRcvQty    = 'POSLN.TotQty'
    lcLnFldName = 'POSLN.nLan_Cst'
    lcAcFldName = 'POSLN.nAct_Cst'
    lcCondition = 'CSTYTYPE+PO+CRSESSION+SHIPNO+STYLE+STR(LINENO,6)+TRANCD'
    lcForCond   = ".T."
    SELECT POSLN
    SET ORDER TO TAG Posrec IN POSLN
    IF SEEK(IIF(lcTktType='I','P','R') + lcTicket + lcRSession,'PosLn')
      lcShipNo = PosLn.ShipNo
    ENDIF
    lcKey       = IIF(lcTktType='I','P','R')+lcTicket+lcRSession+lcShipNo+ALLTRIM(&lcAPvInvDt..Item)
    lcBomCond = "Bomline.cimtyp + Bomline.ctype + Bomline.cbomtyp + Bomline.mfgcode +;
                 Bomline.shipno + Bomline.crsession +ctktno+STR(lineno,6)+style+sclr+cstygrade"
    lcBomKey  = 'I2'+lcCstType+lcMfgCode+lcShipNo+lcRSession+lcTicket
    lcForCond = ".T. AND " +IIF(EMPTY(&lcAPvInvDt..Item),'.T.',;
                     "style+sclr = '"+&lcAPvInvDt..Item+&lcAPvInvDt..Color+"'")

    SELECT BOMLINE
    SET ORDER TO TAG Bomshrec
    llSeekBom =SEEK(lcBomKey)
    lcItem   = 'PosLn.Style'
    lcColor  = 'SPACE(6)'
    lcDyelot = 'SPACE(10)'
    lcGrade  = 'POSLN.cStyGrade'
    lcHdrFile= 'POSHDR'
    lcHdrOred= 'Poshdr'
    lcHdrKey = "P"
    
  CASE lcTktType = 'M'
    lcFileInUse = 'CUTTKTL'
    lcLnFldName = 'CutTktL.nLan_Cst'
    lcAcFldName = 'CutTktL.nAct_Cst'
    lcRcvQty    = 'CutTktL.TotQty'
    lcCondition = 'CRSESSION+CUTTKT+STYLE+TRANCD'
    lcKey       = lcRSession+lcTicket+ALLTRIM(&lcAPvInvDt..Item)
    lcForCond   = ".T."
    SELECT CUTTKTL
    SET ORDER TO TAG Cutrec IN CutTktL
    lcBomCond = "Bomline.cimtyp + Bomline.ctype + Bomline.cbomtyp + Bomline.mfgcode +Bomline.shipno +Bomline.crsession+Bomline.ctktno"
    lcBomKey  = 'M2'+lcCstType+lcMfgCode+lcShipNo + lcRSession + lcTicket
    lcForCond = ".T. AND " +IIF(EMPTY(&lcAPvInvDt..Item),'.T.',;
                         "style+sclr = '"+&lcAPvInvDt..Item+&lcAPvInvDt..Color+"'")
    SELECT BOMLINE
    SET ORDER TO TAG Bomshrec
    llSeekBom = SEEK(lcBomKey)
    lcItem   = 'CUTTKTL.Style'
    lcColor  = 'SPACE(6)'
    lcDyelot = 'CUTTKTL.Dyelot'
    lcGrade  = 'CUTTKTL.cStyGrade'
    lcHdrFile= 'CUTTKTH'
    lcHdrOred= 'Cuttkth' 
    lcHdrKey = ""

  CASE INLIST(lcTktType,'L','F')
    lcFileInUse = 'POFLN'
    lcLnFldName = 'POFLN.nLan_Cost'
    lcAcFldName = 'POFLN.nAct_Cost'
    lcRcvQty    = 'POFLN.nFabTotQty'
    SET ORDER TO TAG POFREC IN POFLN
    lcCondition = 'cMatType+PoMat+cRSession+Fabric+Color+Trancd'
    lcKey       = IIF(lcTktType='L','P','R')+lcTicket+lcRSession+IIF(EMPTY(&lcAPvInvDt..Item),'',;
                  SUBSTR(&lcAPvInvDt..Item,1,7)+&lcAPvInvDt..Color)
    lcForCond = ".T. AND " +IIF(EMPTY(&lcAPvInvDt..Item),'.T.',;
                         "Fabric+Color = '"+&lcAPvInvDt..Item+&lcAPvInvDt..Color+"'")
    lcItem      = 'POFLN.Fabric'
    lcColor     = 'POFLN.Color'
    lcDyelot    = 'POFLN.Dyelot'
    lcGrade     = 'POFLN.cFabGrade'
    lcHdrFile= 'POFHDR'
    lcHdrOred= 'Pofhdr' 
    lcHdrKey = IIF(lcTktType='L','P','R')
    lcBomKey = ""
    
  CASE lcTktType = 'T'
  
    lcFileInUse = 'MMFGORDD'
    lcLnFldName = 'MMFGORDD.nLan_Cost'
    lcAcFldName = 'MMFGORDD.nAct_Cost'
    lcRcvQty    = 'MMFGORDD.nMfgTotQty'
    lcCondition = 'cRSession+cmfgordno+cfabric+color+dyelot+trancd'
    lcKey       = lcRSession+lcTicket+IIF(EMPTY(&lcAPvInvDt..Item),'',;
                  SUBSTR(&lcAPvInvDt..Item,1,7)+&lcAPvInvDt..Color)
    lcForCond   = ".T." 
    SELECT MMFGORDD
    SET ORDER TO TAG MFGREC IN MMFGORDD
    lcBomCond = "Bomline.cimtyp + Bomline.ctype + Bomline.cbomtyp + Bomline.mfgcode +Bomline.shipno "+;
                 "+Bomline.crsession+Bomline.ctktno+STR(lineno,6)+style+sclr+cstygrade"
                 
    lcBomKey  = 'T2'+lcCstType+lcMfgCode+lcShipNo + lcRSession + lcTicket
    lcForCond = ".T. AND " +IIF(EMPTY(&lcAPvInvDt..Item),'.T.',;
                             "style+sclr = '"+&lcAPvInvDt..Item+&lcAPvInvDt..Color+"'")
    SELECT BOMLINE
    SET ORDER TO TAG Bomshrec
    llSeekBom = SEEK(lcBomKey)
    lcItem      = 'MMFGORDD.cFabric'
    lcColor     = 'MMFGORDD.Color'
    lcDyelot    = 'MMFGORDD.Dyelot'
    lcGrade     = 'MMFGORDD.cFabGrade'
    lcHdrFile= 'MMFGORDH'
    lcHdrOred= 'Mmfgordh' 
    lcHdrKey = ""

ENDCASE
lnTotBud   = 0
lnTItmAmt  = 0
lnTotActCt = 0          && Total Actual Cost
lnTotLanCt = 0          && Total Landed Cost

SELECT (lcFileInUse)
=SEEK(lcKey)
IF !INLIST(lcTktType,'S')
  lcForCond = ".T. "
ELSE
  lcForCond = "LOOKUP(POSHDR.STATUS,'P'+POSLN.PO,POSHDR.PO,'POSHDR') $ 'ACO';
                AND IIF(EMPTY(ALLTRIM(&lcAPvInvDt..Item)),.T.,Style=ALLTRIM(&lcAPvInvDt..Item)) AND crsession = lcRSession"
ENDIF
SUM REST &lcRcvQty WHILE EVAL(lcCondition)=lcKey FOR &lcForCond TO lnTotBud
=SEEK(lcKey)
SCAN REST WHILE EVAL(lcCondition)=lcKey FOR &lcForCond .AND. &lcRcvQty <> 0
  SELECT (lcAPInvTkt)
  APPEND BLANK
  REPLACE cStatus    WITH 'A'           ,;
          cVendCode  WITH lcvendcode    ,;
          cApInvNo   WITH lcapinvno     ,;
          cApVILNo   WITH lcInvLineNo   ,;
          cIMTyp     WITH lcTktType     ,;
          cTktNo     WITH IIF(lcTktType='S',POSLN.PO,lcTicket) ,;
          cRSession  WITH lcRSession    ,;
          cRecvType  WITH IIF(&lcGrade='1','R',IIF(&lcGrade='2','S','D')) ,;
          LineNo     WITH IIF(lcTktType='T',0,&lcFileInUse..LineNo) ,;
          Item       WITH EVAL(lcItem)  ,;
          Color      WITH EVAL(lcColor) ,; 
          cDyelot    WITH EVAL(lcDyelot),;
          nApTAplQty WITH &lcRcvQty     ,;
          nApAPlPric WITH lnAmount/ lnTotBud ,;
          cWareCode  WITH &lcFileInUse..cWareCode ,;
          nApAplAmnt WITH &lcRcvQty*lnAmount/lnTotBud 
  lnMaxLine = lnMaxLine  + 1            
  REPLACE cApdGlAct WITH lfGetWIPAc(lcHdrKey+IIF(lcTktType='S',POSLN.PO,lcTicket),lcHdrFile,lcHdrOred) ;
          cApdLinNo WITH  STR(lnMaxLine,4)


  lnTItmAmt  = lnTItmAmt  + nApAplAmnt
  IF INLIST(lcTktType,'S','I','M','R','A','D')
    REPLACE nApAplQty1 WITH &lcFileInUse..Qty1 ,;
            nApAplQty2 WITH &lcFileInUse..Qty2 ,;
            nApAplQty3 WITH &lcFileInUse..Qty3 ,;
            nApAplQty4 WITH &lcFileInUse..Qty4 ,;
            nApAplQty5 WITH &lcFileInUse..Qty5 ,;
            nApAplQty6 WITH &lcFileInUse..Qty6 ,;
            nApAplQty7 WITH &lcFileInUse..Qty7 ,;
            nApAplQty8 WITH &lcFileInUse..Qty8
  ENDIF
  lnTotLanCt = lnTotLanCt + &lcRcvQty * EVAL(lcLnFldName+lcCstType)
  lnTotActCt = lnTotActCt + &lcRcvQty * EVAL(lcAcFldName+lcCstType)
      
ENDSCAN


IF !INLIST(lcTktType,'L','F')
  SELECT (lcFileInUse)
  SET RELATION OFF INTO BOMLINE
ENDIF
SELECT (lcAPInvTkt)
SET KEY TO
*-- If over contributed.
IF ABS(lnTItmAmt) > ABS(&lcApVInvDt..nAPAprAmnt)
  REPLACE nApAplAmnt WITH nApAplAmnt - (ABS(lnTItmAmt) - ABS(lnAmount))
  lnTItmAmt = lnTItmAmt - (ABS(lnTItmAmt) - ABS(lnAmount))  
  =SEEK(PADR(lcvendcode,8)+PADR(lcApInvNo,12)+lcInvLineNo+SPACE(6))
  REPLACE nApAplAmnt WITH nApAplAmnt - lnTItmAmt  
ELSE    && Else (If not over contributed)
  IF ABS(lnTItmAmt) < ABS(&lcApVInvDt..nAPAprAmnt)
    lnTItmAmt = lnTItmAmt - (ABS(lnTItmAmt) - ABS(lnAmount))      
  ENDIF
  =SEEK(PADR(lcvendcode,8)+PADR(lcApInvNo,12)+lcInvLineNo+SPACE(6))
  REPLACE nApAplAmnt WITH nApAplAmnt - lnTItmAmt  
    
ENDIF    && End of IF ABD(lnTItmAmt) > ABS(nApAplAmnt)

IF nApAplAmnt = 0
  REPLACE cStatus WITH IIF(nRecNo = 0 , 'S' , 'D')
  DELETE
ENDIF
lnCntrbAmnt = lnCntrbAmnt + lnTItmAmt
SET ORDER TO TAG &lcBomOrd IN BOMLINE

RETURN (lnCntrbAmnt > 0)

*!*************************************************************
*! Name      : lfOpenFiles
*! Developer : ALBERT RAIF (ALB) 
*! Date      : 1/22/2003
*! Purpose   : Open needed files
*! Tracking# : E302010,1
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfOpenFiles()
*!*************************************************************
FUNCTION lfOpenFiles
IF !USED('APVINVDT')
  DIMENSION laOpenFile[ALEN(laOpenFile,1)+1,2]
  laOpenFile[ALEN(laOpenFile,1),1] = 'APVINVDT'
  laOpenFile[ALEN(laOpenFile,1),2] = .T.
  =gfOpenFile(gcDataDir+'APVINVDT','ITEM','SH')
ENDIF
SELECT APVINVDT
SELECT *, 'S' AS cStatus,RECNO() AS nRecNo FROM ApVInvDt WHERE ;
      cimtyp+ctktno+clotno+STR(lineno,6)+cbomtype+coprcode+item+color+cdyelot = ;
      lcTktType+lcPONo INTO DBF (gcWorkDir+lcAPvInvDt)
INDEX ON cIMTyp+cTktNo+cLotNo+cBomType+cOprCode+Item+Color+cDyelot TAG ITEM
INDEX ON cVendCode+cApInvNo+cAPVILNo TAG (lcAPvInvDt) ADDITIVE
INDEX ON cApDGlAct TAG ADJUST ADDITIVE

IF !USED('APINVTKT')
  DIMENSION laOpenFile[ALEN(laOpenFile,1)+1,2]
  laOpenFile[ALEN(laOpenFile,1),1] = 'APINVTKT'
  laOpenFile[ALEN(laOpenFile,1),2] = .T.
  =gfOpenFile(gcDataDir+'APINVTKT','LNCONT','SH')
ENDIF
SELECT APINVTKT
SELECT *, 'S' AS cStatus,RECNO() AS nRecNo FROM ApInvTkt WHERE ;
      cimtyp+ctktno+STR(lineno,6)+crsession = lcTktType+lcPONo ;
      INTO DBF (gcWorkDir+lcAPInvTkt)
INDEX ON cIMTyp+cTktNo+STR(LineNo,6)+cRSession TAG Ticket
INDEX ON cVendCode+cApInvNo+cApVILNo+cRSession TAG (lcAPInvTkt) ADDITIVE

IF !USED('BOMLINE')
  DIMENSION laOpenFile[ALEN(laOpenFile,1)+1,2]
  laOpenFile[ALEN(laOpenFile,1),1] = 'BOMLINE'
  laOpenFile[ALEN(laOpenFile,1),2] = .T.
  =gfOpenFile(gcDataDir+'BOMLINE','BOMLINE','SH')
ENDIF
IF !USED('STYINVJL')
  DIMENSION laOpenFile[ALEN(laOpenFile,1)+1,2]
  laOpenFile[ALEN(laOpenFile,1),1] = 'STYINVJL'
  laOpenFile[ALEN(laOpenFile,1),2] = .T.
  =gfOpenFile(gcDataDir+'STYINVJL','STYINVJL','SH')
ENDIF

IF !USED('APINVHDR')
  DIMENSION laOpenFile[ALEN(laOpenFile,1)+1,2]
  laOpenFile[ALEN(laOpenFile,1),1] = 'APINVHDR'
  laOpenFile[ALEN(laOpenFile,1),2] = .T.
  =gfOpenFile(gcDataDir+'APINVHDR','VENDINV','SH')
ENDIF

*B607209,1 KHM 05/27/2003 (Begin) Open the file in case of calling from MF module.
*IF !USED('CUTTKTH')
IF lcTranType = 'M' AND !USED('CUTTKTH')
*B607209,1 KHM 05/27/2003 (End)

  DIMENSION laOpenFile[ALEN(laOpenFile,1)+1,2]
  laOpenFile[ALEN(laOpenFile,1),1] = 'CUTTKTH'
  laOpenFile[ALEN(laOpenFile,1),2] = .T.
  =gfOpenFile(gcDataDir+'CUTTKTH','CUTTKTH','SH')
ENDIF

*B607209,1 KHM 05/27/2003 (Begin) Open the file in case of calling from MF module.
*IF !USED('CUTTKTH')
IF lcTranType = 'M' AND !USED('CUTTKTL')
*B607209,1 KHM 05/27/2003 (End)

  DIMENSION laOpenFile[ALEN(laOpenFile,1)+1,2]
  laOpenFile[ALEN(laOpenFile,1),1] = 'CUTTKTL'
  laOpenFile[ALEN(laOpenFile,1),2] = .T.
  =gfOpenFile(gcDataDir+'CUTTKTL','CUTREC','SH')
ENDIF

*B607209,1 KHM 05/27/2003 (Begin) Open the file in case of MA installed.
*IF !USED('CUTTKTH')
IF 'MA' $ gcComp_mdl AND !USED('MATINVJL')
*B607209,1 KHM 05/27/2003 (End)

  DIMENSION laOpenFile[ALEN(laOpenFile,1)+1,2]
  laOpenFile[ALEN(laOpenFile,1),1] = 'MATINVJL'
  laOpenFile[ALEN(laOpenFile,1),2] = .T.
  =gfOpenFile(gcDataDir+'MATINVJL','MATINVJL','SH')
ENDIF

*B607209,1 KHM 05/27/2003 (Begin) Open the file in case of MA installed.
*IF !USED('POFHDR')
IF 'MA' $ gcComp_mdl  AND !USED('POFHDR')
*B607209,1 KHM 05/27/2003 (End)

  DIMENSION laOpenFile[ALEN(laOpenFile,1)+1,2]
  laOpenFile[ALEN(laOpenFile,1),1] = 'POFHDR'
  laOpenFile[ALEN(laOpenFile,1),2] = .T.
  =gfOpenFile(gcDataDir+'POFHDR','POFHDR','SH')
ENDIF

*B607209,1 KHM 05/27/2003 (Begin) Open the file in case of MA installed.  
*IF !USED('POFLN')
IF 'MA' $ gcComp_mdl  AND !USED('POFLN')
*B607209,1 KHM 05/27/2003 (End)

  DIMENSION laOpenFile[ALEN(laOpenFile,1)+1,2]
  laOpenFile[ALEN(laOpenFile,1),1] = 'POFLN'
  laOpenFile[ALEN(laOpenFile,1),2] = .T.
  =gfOpenFile(gcDataDir+'POFLN','POFREC','SH')
ENDIF

IF !USED('BOMCOST')
  DIMENSION laOpenFile[ALEN(laOpenFile,1)+1,2]
  laOpenFile[ALEN(laOpenFile,1),1] = 'BOMCOST'
  laOpenFile[ALEN(laOpenFile,1),2] = .T.
  =gfOpenFile(gcDataDir+'BOMCOST','BOMCOST')
ENDIF

IF !USED('CTKTBOM')
  DIMENSION laOpenFile[ALEN(laOpenFile,1)+1,2]
  laOpenFile[ALEN(laOpenFile,1),1] = 'CTKTBOM'
  laOpenFile[ALEN(laOpenFile,1),2] = .T.
  =gfOpenFile(gcDataDir+'CTKTBOM','CTKTBOM')
ENDIF

*!*************************************************************
*! Name      : lfGetMaxLin
*! Developer : ALBERT RAIF (ALB) 
*! Date      : 1/22/2003
*! Purpose   : Get the maximum line no from invoicetkt and update
*! Tracking# : E302010,1
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGetMaxLin()
*!*************************************************************
FUNCTION lfGetMaxLin
PARAMETER lcVendCode,lcInvoice
PRIVATE lcSumStr, lcSumKey , lnDistLnNo  , lnRecNo , lnGenType , lcArea 
lcArea = SELECT()
lnDistLnNo = 0 
lnGenType  = 0 
lcSumKey = "cVendCode+cApInvNo"  
lcSumStr = ALLTRIM(lcVendCode) +ALLTRIM(lcInvoice) 

SET DELETE OFF       
SELECT MAX(VAL(cApdLinNo)) FROM (lcAPInvTkt) WHERE ALLTRIM(cVendCode)+ALLTRIM(cApInvNo) = lcSumStr  ;
       INTO ARRAY laMaxDistLn
SET DELETE ON 

IF _TALLY <> 0  AND laMaxDistLn[1] > 0 
  lnDistLnNo = laMaxDistLn[1] 
ENDIF

SELECT (lcAPvInvDt)
lnRecNo = RECNO()
COUNT TO lnGenType FOR cImTyp = 'G' 
IF BETWEEN (lnRecNo , 1 , RECCOUNT())
  GOTO lnRecNo 
ENDIF

SELECT (lcArea)
RETURN lnDistLnNo + lnGenType 
*!*************************************************************
*! Name      : lfvRSession
*! Developer : ALB
*! Date      : 01/22/2003
*! Purpose   : Validate Selected Document Receiving Session
*! Tracking# : E302010,1
*!*************************************************************
*! Calls     : ARIABROW()
*!*************************************************************
*! Passed Parameters  :  lcType    : Document Type
*!                       lcTicket  : Document #
*!                       llClrRead : Terminate Read
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRSession()
*!*************************************************************
FUNCTION lfvRSession
PARAMETERS lcType,lcTicket,lcItem,lcColor,lcCstType
PRIVATE lcBrFields

DO CASE 
  CASE INLIST(lcType,'I','R','D')
    SET ORDER TO TAG Posrec IN POSLN
      SELECT cRSession,Date,SUM(TotQTy) AS nRecQty,;
      SUM(TotQty*nLan_Cst&lcCstType) AS nRecAmnt;
      FROM POSLN WHERE cStyType+Po+cRSession+ShipNo+Style+STR(LineNo,6)+TranCd=IIF(lcType='I','P',lcType)+lcTicket ;
      AND !EMPTY(cRSession) AND Style=ALLTRIM(lcItem) AND IIF(lcType = 'A',trancd<>'6',.T.);
      GROUP BY cStyType,Po,cRSession INTO CURSOR RcvSess
    SET ORDER TO TAG PosLn IN POSLN

  CASE lcType='S'
    SET ORDER TO TAG Poshrec IN POSLN
    IF llBrowse OR (!EMPTY(lcRSession) AND !SEEK(lcTicket+lcRSession,'POSLN'))
      SELECT cRSession,Date,SUM(TotQTy) AS nRecQty,;
      SUM(TotQty*nLan_Cst&lcCstType) AS nRecAmnt;
      FROM POSLN ;
      WHERE ShipNo+cRSession+cStyType+Po+PosLn.Style+;
      STR(LineNo,6)+TranCd = lcTicket AND !EMPTY(cRSession) AND Style=ALLTRIM(lcItem);
      GROUP BY ShipNo,cRSession INTO CURSOR RcvSess
    ENDIF
    SET ORDER TO TAG PosLn IN POSLN
  CASE lcType='M'
    SET ORDER TO TAG Cutrec IN CutTktL
      SELECT cRSession,Date,SUM(TotQTy) AS nRecQty,;
      SUM(TotQty*nCost&lcCstType) AS nRecAmnt;
      FROM CutTktL ;
      WHERE CutTkt+Style+Dyelot+TranCd=lcTicket+ALLTRIM(lcItem) AND !EMPTY(cRSession) ;
      GROUP BY CutTkt,cRSession INTO CURSOR RcvSess

    SET ORDER TO TAG CutTktL IN CutTktL
  CASE INLIST(lcType,'L','F')
    SELECT POFLN
    SET ORDER TO TAG POFREC
      SELECT cRSession,Date,SUM(nFabTotQty) AS nRecQty,;
      SUM(nFabTotQty*nLan_Cost&lcCstType) AS nRecAmnt;
      FROM POFLN ;
      WHERE cMatType+PoMat+Fabric+Color+TranCd=;
      IIF(lcType='L','P','R')+lcTicket+IIF(EMPTY(lcItem),'',SUBSTR(lcItem,1,7)+lcColor) AND ;
      !EMPTY(cRSession) GROUP BY PoMat,cRSession INTO CURSOR RcvSess
  CASE lcType='T'
    SELECT MMFGORDD
    SET ORDER TO TAG MMFGORDD
    SELECT cRSession,Drecvdate as Date,SUM(nmfgtotQty) AS nRecQty,;
      SUM(nmfgtotQty*nLan_Cost&lcCstType) AS nRecAmnt;
      FROM MMFGORDD;
      WHERE Cmfgordno+CFabric+Color+TranCd=;
      lcTicket+IIF(EMPTY(lcItem),'',SUBSTR(lcItem,1,7)+lcColor) AND ;
      !EMPTY(cRSession) GROUP BY cRSession INTO CURSOR RcvSess
      llBrowse = .T.    
ENDCASE
RETURN _TALLY

*!*************************************************************
*! Name      : lfGetWIPAc
*! Developer : ALB
*! Date      : 01/22/2003
*! Purpose   : To get the WIP account based on Gl link code defined in poshdr file 
*! Tracking# : E302010,1
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGetWIPAc()
*!*************************************************************

FUNCTION lfGetWIPAc
PARAMETER  lcHdrKey , lcHdrFile , lcHdrOred

PRIVATE lcGlAcct , lnArea, lcOrder , lnRecNo  

IF EMPTY(lcHdrKey) OR EMPTY(lcHdrFile) OR EMPTY(lcHdrOred)
  RETURN ""
ENDIF

lcGlAcct = ""
lnArea= SELECT()
lcOrder = ORDER()
lnRecNo = RECNO() 

SELECT (lcHdrFile) 
SET ORDER TO TAG (lcHdrOred)
IF SEEK(lcHdrKey)
  lcGlAcct = lfvAASAct(&lcHdrFile..LINK_CODE,"013") 
ENDIF

SELECT (lnArea)
SET ORDER TO (lcOrder)
IF BETWEEN(lnRecNo , 1, RECCOUNT() ) 
  GOTO lnRecNo 
ENDIF 
RETURN lcGlAcct 
*!*************************************************************
*! Name      : lfvAASAct
*! Developer : ALB
*! Date      : 01/22/2003
*! Purpose   : To get the WIP account based on Gl link code defined in poshdr file 
*! Tracking# : E302010,1
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAASAct()
*!*************************************************************
FUNCTION lfvAASAct
PARAMETERS lcLinkCode,lcCatKey
PRIVATE lcGlAcnt
lcGlAcnt= ''
IF SEEK(lcLinkCode+lcCatKey,'GL_LINK')
  lcGlAcnt = GL_LINK.GLACNT
  IF !SEEK(lcGlAcnt,'lcLinkChar') 
    lcGlAcnt = ''
  ENDIF  
ELSE  
  =SEEK('DEFDEF','GL_LINK')
  lcGlAcnt = GL_LINK.GLACNT
ENDIF
RETURN lcGlAcnt 

*!*************************************************************
*! Name      : lfChckACst
*! Developer : Albert Raif (ALB)
*! Date      : 01/23/2003
*! Purpose   : Function to calculate the actual cost and check
*!             that none of the actual cost will have a negative amount.
*! Tracking# : E302010,1
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : .T. If none of the actual cost will have
*!                          a negative amount.
*!                      .F. Otherwise.
*!*************************************************************
*! Example            : =lfChckACst()
*!*************************************************************
*
FUNCTION lfChckACst

PRIVATE llReturn  , lcExStatus , lcFullStat , lcCDXFile , lcMssgText ,;
        lcHdrFile , lcHActFild , lcHFActFld , lcHKeyVal , lnActCstNo ,;
        lcDetFile , lcDActFild , lcDFActFld , lcDKey    , lcDForCond ,;
        lcDKeyVal , lcDQtyFild , m.cIMTyp   , lcBomType , lnAmntChng ,;
        lnCount   , lcCount

llReturn = .T.

STORE '' TO lcExStatus , lcFullStat , lcCDXFile , lcMssgText

lcFullStat = SET('FULLPATH')
SET FULLPATH ON

*-- Create a temp. file to hold the calculated actual cost for PO and CT
*-- header files.
CREATE CURSOR &lcActCstHd;
       (cIMTyp C(1)        , cTktNo     C(6)     , nRecNo     N(10,0) ,;
        nActCost1  N(13,3) , nActCost2  N(13,3)  , nActCost3  N(13,3) ,;
        nActCost4  N(13,3) , nActCost5  N(13,3)  ,;
        nFActCost1 N(13,3) , nFActCost2 N(13,3)  , nFActCost3 N(13,3) ,;
        nFActCost4 N(13,3) , nFActCost5 N(13,3))

ZAP
lcCDXFile = STRTRAN(DBF() , '.TMP' , '.CDX')
INDEX ON cIMTyp + cTktNo TAG (lcActCstHd) OF (lcCDXFile)

*-- Create a temp. file to hold the calculated actual cost for PO and CT
*-- detail files.
CREATE CURSOR &lcActCstDt;
       (cIMTyp     C(1)    , cTktNo     C(6)    , nRecNo     N(10,0) ,;
        cRSession  C(6)    , cShipNo    C(6)    , cItem      C(19)   ,;
        cColor     C(6)    , cLineNo    C(6)    , nTotQty    N(10,3) ,;
        nActCost1  N(13,6) , nActCost2  N(13,6) , nActCost3  N(13,6) ,;
        nActCost4  N(13,6) , nActCost5  N(13,6) ,;
        nFActCost1 N(13,6) , nFActCost2 N(13,6) , nFActCost3 N(13,6) ,;
        nFActCost4 N(13,6) , nFActCost5 N(13,6))
ZAP
lcCDXFile = STRTRAN(DBF() , '.TMP' , '.CDX')
INDEX ON cIMTyp + cRSession + cShipNo + cTKtNo + cItem + cColor + cLineNo;
         TAG (lcActCstDt) OF (lcCDXFile)

SET FULLPATH &lcFullStat

lcExStatus = SET('EXACT')
SET EXACT OFF

*-- Set the needed order tags
IF ('I' $ lcTktType OR 'R' $ lcTktType ) AND USED("POSHDR")
  SET ORDER TO TAG POSHDR IN POSHDR
  SET ORDER TO TAG POSREC IN POSLN
ENDIF
IF 'MF' $ gcCmpModules AND  'M' $ lcTktType
  SET ORDER TO TAG CUTTKTH IN CUTTKTH 
  SET ORDER TO TAG CUTREC  IN CUTTKTL
ENDIF

IF ('L' $ lcTktType OR 'F' $ lcTktType) AND USED("POFHDR")
  SET ORDER TO TAG POFHDR IN POFHDR
  SET ORDER TO TAG POFREC IN POFLN
ENDIF

*-- Recall the deleted records
SELECT (lcAPInvTkt)
RECALL ALL

*-- Recall the deleted records and set the needed order tag
SELECT (lcApVInvDt)
RECALL ALL
SET ORDER TO TAG ITEM
GO TOP

*-- Do while there is no troubles in the records locking and the end of file
*-- has not been reached yet.
DO WHILE llReturn .AND. !EOF()
  
  *-- lcHdrFile   Variable to hold the name of the header file for the
  *--             current document type.
  *-- lcHActFild  Variable to hold the name of the actual cost field in the
  *--             header file for the current document type.
  *-- lcHFActFld  Variable to hold the name of the actual cost in foreign
  *--             currency field in the header file for the current
  *--             document type.
  *-- lcHKeyVal   Variable to hold the key value to be used while searching
  *--             for the record that will be updated in the header file.
  *-- lcDetFile   Variable to hold the name of the detail file for the
  *--             current document type.
  *-- lcDActFild  Variable to hold the name of the actual cost field in the
  *--             detail file for the current document type.
  *-- lcDFActFld  Variable to hold the name of the actual cost in foreign
  *--             currency field in the detail file for the current
  *--             document type.
  *-- lcDKey      Variable to hold the expression that will be used to get
  *--             the key value that will be used for searching for the
  *--             record that will be updated in the detail file.
  *-- lcDForCond  Variable to hold a string of the expression that will be
  *--             used as the FOR expression while searching for the record
  *--             that will be updated in the detail file.
  *-- lcDQtyFild  Variable to hold the name of the total quantity field in
  *--             the detail file for the current document type.
  
  STORE '' TO lcHdrFile  , lcHActFild , lcHFActFld , lcHKeyVal  ,;
              lcDetFile  , lcDActFild , lcDFActFld , lcDKey     ,;
              lcDForCond , lcDQtyFild
  
  
  *-- lnActCstNo  Variable to hold the number of actual cost fields for the
  *--             current document type.
  
  STORE 0  TO lnActCstNo
  
  DO CASE
    *-- Case of document type "General Expense"
    CASE cIMTyp = 'G'
      SEEK 'H'
      IF RECNO(0) <> 0
        GO RECNO(0)
      ENDIF
      LOOP
    
    *-- Case of document types "Style Purchase Order", "Style Purchase
    *-- Order Return" and "Shipment"
    CASE INLIST(cIMTyp , 'I' , 'R' , 'S')
      lcHdrFile  = 'POSHDR'
      lcHActFild = 'nAct_Cost'
      lcHFActFld = 'nFActCost'
      lcHKeyVal  = IIF(cIMTyp = 'R' , 'R' , 'P')
      lnActCstNo  = 5
      lcDetFile  = 'POSLN'
      lcDActFild = 'nEActCost'
      lcDFActFld = 'nAct_Cst'
      IF cIMTyp <> 'S'
        lcDKey     = "IIF(m.cIMTyp = 'R' , 'R' , 'P') + cTKtNo + cRSession " 
      ELSE
        SET ORDER TO TAG POSHREC IN POSLN
        lcDKey = "&lcApVInvDt..ShipNo + cRSession + 'P' "
      ENDIF
      IF cIMTyp <> 'S'
        lcDForCond = "Style = &lcAPInvTkt..Item AND STR(LineNo , 6) = STR(&lcAPInvTkt..LineNo , 6)"
      ELSE
        lcDForCond = "PO = cTKtNo AND Style = &lcAPInvTkt..Item AND STR(LineNo , 6) = STR(&lcAPInvTkt..LineNo , 6)"
      ENDIF  
      
      lcDQtyFild = 'TotQty'
    
    *-- Case of document type "Cutting Ticket"
    CASE cIMTyp = 'M'
      lcHdrFile  = 'CUTTKTH'
      lcHActFild = 'nAct_Cost'
      lcHKeyVal  = ''
      lnActCstNo = 5
      lcDetFile  = 'CUTTKTL'
      lcDActFild = 'nAct_Cst'
      lcDKey     = "cRSession + cTKtNo + Item"
      lcDForCond = "STR(LineNo , 6) = STR(&lcAPInvTkt..LineNo , 6)"
      lcDQtyFild = 'TotQty'

    *-- Case of document types "Material PO" and "Material PO Return"
    CASE INLIST(cIMTyp , 'L' , 'F')    
      lcHdrFile  = 'POFHDR'
      lcHActFild = 'nEActCost'
      lcHFActFld = 'nAct_Cost'
      lcHKeyVal  = IIF(cIMTyp = 'L' , 'P' , 'R')
      lnActCstNo = 4
      lcDetFile  = 'POFLN'
      lcDActFild = 'nEActCost'
      lcDFActFld = 'nAct_Cost'
      lcDKey     = "IIF(m.cIMTyp = 'L' , 'P' , 'R') + cTKtNo + cRSession " +;
                   "+ SUBSTR(ITEM , 1 , 7) + Color"
      
      lcDForCond = "STR(LineNo , 6) = STR(&lcAPInvTkt..LineNo , 6)"
      lcDQtyFild = 'nFabTotQty'
    CASE INLIST(cIMTyp , 'T')
      lcAlias = ALIAS()    
      lcHdrFile  = 'MMFGORDH'
      lcHActFild = 'nAct_Cost'
      lcHKeyVal  = IIF(cIMTyp = 'T' , '' , '')
      lnActCstNo = 4
      lcDetFile  = 'MMFGORDD'
      lcDActFild = 'nAct_Cost'
      lcDKey     = "cRSession + cTKtNo + SUBSTR(ITEM , 1 , 7) + Color"
      lcDForCond = ".T."
      lcDQtyFild = 'nmfgTotQty'
      SELECT(lcDetFile)
      SET ORDER TO TAG Mfgrec
      SELECT (lcAlias)
    CASE INLIST(cIMTyp , 'D' )
      lcHdrFile  = 'POSHDR'
      lcHActFild = 'nAct_Cost'
      lcHFActFld = 'nFActCost'
      lcHKeyVal  = cIMTyp
      lnActCstNo  = 5
      lcDetFile  = 'POSLN'
      lcDActFild = 'nEActCost'
      lcDFActFld = 'nAct_Cst'
      lcDKey     = "'D' + cTKtNo + cRSession + &lcApVInvDt..ShipNo + Item + STR(LineNo , 6)"
      lcDForCond = ".T."
      lcDQtyFild = 'TotQty'
  ENDCASE
  
  m.cIMTyp = cIMTyp
  
  *-- Scan while there no troubles in the records locking and the document
  *-- type didn't change for the records with status <> same
  *-- Note: The seek() function in the for condition is not used as a
  *--       condition it's used to replace the record pointer on the first
  *--       corresponding record in the file (lcAPInvTkt).
  SCAN WHILE llReturn .AND. cIMTyp = m.cIMTyp;
         FOR cStatus <> 'S' .AND.;
             SEEK(cVendCode + cApInvNo + cAPVILNo , lcAPInvTkt)
    lcVendInv = cVendCode + cApInvNo
    SELECT APINVHDR
    STORE '*' TO lcExSin1 ,lcExSin2
    IF SEEK(lcVendInv)
      lcExSin1  = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
      lnExRate  = APINVHDR.nExRate
      lnCurUnit = APINVHDR.nCurrUnit
    ELSE
      lnExRate  = 1
      lnCurUnit = 1
    ENDIF
    SELECT (lcApVInvDt)
    *-- Flag to know if the record has been deleted
    llDeleted = (cStatus='D') OR ((nRecNo = 0) AND (cStatus= 'S'))
    *-- Variable to hold the record number of the corresponding record in
    *-- the master file
    lnRecNo   = nRecNo
    
    *-- Replace the record pointer on the corresponding record in the
    *-- master file.
    IF BETWEEN( lnRecNo ,1, RECCOUNT('APVINVDT'))
      GO lnRecNo IN APVINVDT
    ENDIF    && End of IF lnRecNo > 0
    
    lcBomType  = cBomType
    
    *-- Get the change that has been made to the applied amount for this
    *-- record
    lnAmntChng = IIF(llDeleted   , 0 , nApAprAmnt) -;
                 IIF(lnRecNo = 0 , 0 , APVINVDT.nApAprAmnt)
    
    IF cIMTyp = "S"
      *-- This function is used to update the actual cost for the poshdr
      *-- file in case of invoicing by shimpment.
      =lfUpdPoHAC()
    ELSE
    
      *-- If there is a header file that should be updated for the current
      *-- document type.
      IF !EMPTY(lcHdrFile)
      
        *-- If there is no records for this document number in the actual
        *-- cost temp. header file
        IF !SEEK(m.cIMTyp + cTktNo , lcActCstHd)
          m.cTktNo = cTktNo
         
          *-- Go to the record that will be updated in the header file
          SELECT (lcHdrFile)
          IF SEEK (lcHKeyVal + m.cTktNo)
          
            *-- Attempt to lock the record
            m.nRecNo = RECNO()
          
            STORE 0 TO m.nActCost1  , m.nActCost2  , m.nActCost3 ,;
                       m.nActCost4  , m.nActCost5  ,;
                       m.nFActCost1 , m.nFActCost2 , m.nFActCost3 ,;
                       m.nFActCost4 , m.nFActCost5
          
            *-- If there is no fields for the actual cost in foreign currency
            IF EMPTY(lcHFActFld)
              *-- Get the actual cost
              FOR lnCount = 1 TO lnActCstNo
                lcCount            = ALLTRIM(STR(lnCount))
                m.nActCost&lcCount = &lcHActFild.&lcCount
              ENDFOR    && End of FOR lnCount = 1 TO lnActCstNo
            ELSE    && Else [If there is fields for the actual cost in foreign currency]
              *-- Get the actual cost
              FOR lnCount = 1 TO lnActCstNo
                lcCount             = ALLTRIM(STR(lnCount))
                m.nActCost&lcCount  = &lcHActFild.&lcCount
                m.nFActCost&lcCount = &lcHFActFld.&lcCount
              ENDFOR    && End of FOR lnCount = 1 TO lnActCstNo
            ENDIF    && End of IF EMPTY(lcHFActFld)
          
            *-- Add a corresponding record in the temp. file
            INSERT INTO &lcActCstHd FROM MEMVAR
    
          ENDIF      
          
        ENDIF    && End of IF !SEEK(m.cIMTyp + cTktNo , lcActCstHd)
      
        *-- Update total actual cost in the temp. header file
        SELECT (lcActCstHd)
      
        REPLACE nActCost&lcBomType WITH nActCost&lcBomType + (lnAmntChng;
                                        &lcExSin1 lnExRate &lcExSin2 lnCurUnit)
      
        *-- Update total actual cost in foreign currency in the temp. header
        *-- file
        IF !EMPTY(lcHFActFld)
          REPLACE nFActCost&lcBomType WITH nFActCost&lcBomType + lnAmntChng
        ENDIF    && End of IF !EMPTY(lcHFActFld)
      ENDIF    && End of IF !EMPTY(lcHdrFile)
    
    ENDIF
    SELECT (lcAPInvTkt)
    *-- Scan the corresponding records in the detail file (lcAPInvTkt) with
    *-- receiving session not empty.
    
    SCAN REST;
        WHILE cVendCode + cApInvNo + cApVILNo = &lcApVInvDt..cVendCode +;
              &lcApVInvDt..cApInvNo + &lcApVInvDt..cAPVILNo;
              FOR !EMPTY(cRSession)

      lcVendInv = cVendCode + cApInvNo
      SELECT APINVHDR
      STORE '*' TO lcExSin1 ,lcExSin2
      IF SEEK(lcVendInv)
        lcExSin1  = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
        lnExRate  = APINVHDR.nExRate
        lnCurUnit = APINVHDR.nCurrUnit
      ELSE
        lnExRate  = 1
        lnCurUnit = 1
      ENDIF
      SELECT (lcAPInvTkt)

      *-- Flag to know if the record has been deleted
      llDeleted = (cStatus='D') OR ((nRecNo = 0) AND (cStatus= 'S'))
      *-- Variable to hold the record number of the corresponding record in
      *-- the master file
      lnRecNo   = nRecNo
      
      *-- Replace the record pointer on the corresponding record in the
      *-- master file.
      IF BETWEEN(lnRecNo ,1,RECCOUNT('APINVTKT'))
        GO lnRecNo IN APINVTKT
      ENDIF
      
      *-- Get the change that has been made to the applied amount for this
      *-- record
      
      lnAmntChng = IIF(llDeleted   , 0 , nApAplAmnt) -;
                   IIF(lnRecNo = 0 , 0 , APINVTKT.nApAplAmnt)
      
      *-- If there is no records for this document number in the actual
      *-- cost temp. detail file

      IF !SEEK(cIMTyp + cRSession + &lcApVInvDt..ShipNo + cTKtNo + Item +;
               Color + STR(LineNo,6) , lcActCstDt)
        STORE 0 TO m.nActCost1  , m.nActCost2  , m.nActCost3 ,;
                   m.nActCost4  , m.nActCost5  ,;
                   m.nFActCost1 , m.nFActCost2 , m.nFActCost3 ,;
                   m.nFActCost4 , m.nFActCost5
        
        m.cIMTyp    = cIMTyp
        m.cRSession = cRSession
        m.cShipNo   = &lcApVInvDt..ShipNo
        m.cTktNo    = cTKtNo
        m.cItem     = Item
        m.cColor    = Color
        m.cLineNo   = STR(LineNo,6)
        *-- Go to the record that will be updated in the header file
        lcDKeyVal = EVALUATE(lcDKey)
        SELECT (lcDetFile)
        LLSEEK = SEEK (lcDKeyVal)
        LOCATE REST;
              WHILE EVALUATE(SYS(14,VAL(SYS(21)))) = lcDKeyVal;
                FOR EVALUATE(lcDForCond)
        
        *-- Attempt to lock the record
        m.nRecNo    = RECNO()
        m.nTotQty   = &lcDQtyFild
          
        *-- If there is no fields for the actual cost in foreign currency
        IF EMPTY(lcDFActFld)
            
          *-- Get the actual cost
          FOR lnCount = 1 TO lnActCstNo
            lcCount            = ALLTRIM(STR(lnCount))
            m.nActCost&lcCount = &lcDActFild.&lcCount
          ENDFOR    && End of FOR lnCount = 1 TO lnActCstNo
        ELSE    && Else [If there is fields for the actual cost in foreign currency]
            
          *-- Get the actual cost
          FOR lnCount = 1 TO lnActCstNo
            lcCount             = ALLTRIM(STR(lnCount))
            m.nActCost&lcCount  = &lcDActFild.&lcCount
            m.nFActCost&lcCount = &lcDFActFld.&lcCount
          ENDFOR    && End of FOR lnCount = 1 TO lnActCstNo
        ENDIF    && End of IF EMPTY(lcDFActFld)
          
        *-- Add a corresponding record in the temp. file
        INSERT INTO &lcActCstDt FROM MEMVAR
      ENDIF    && End of IF !SEEK(cIMTyp + cRSession + ...
      
      SELECT (lcActCstDt)
      
      *-- Update total actual cost in the temp. detail file
      REPLACE nActCost&lcBomType WITH nActCost&lcBomType + ((lnAmntChng / nTotQty);
                                      &lcExSin1 lnExRate &lcExSin2 lnCurUnit)

      *-- Update total actual cost in foreign currency in the temp. detail
      *-- file
      IF !EMPTY(lcDFActFld)
        REPLACE nFActCost&lcBomType WITH nFActCost&lcBomType +;
                                         (lnAmntChng / nTotQty)
        
      ENDIF    && End of IF !EMPTY(lcDFActFld)
    ENDSCAN    && End of SCAN REST WHILE cVendCode + cApInvNo + cApVILNo ...
    
    SELECT (lcApVInvDt)
  ENDSCAN    && End of   SCAN WHILE llReturn .AND. cIMTyp = m.cIMTyp ...
ENDDO    && End of DO WHILE llReturn .AND. !EOF()

*-- If there was no troubles found in the records locking
 IF llReturn
  
  *-- Search for a record with negative actual cost in the temp. actual
  *-- cost header file
  SELECT (lcActCstHd)
  LOCATE FOR nActCost1 < 0 .OR.;
             nActCost2 < 0 .OR.;
             nActCost3 < 0 .OR.;
             nActCost4 < 0 .OR.;
             nActCost5 < 0
  
  *-- If there is a record with negative actual cost in the temp. actual
  *-- cost header file
  IF FOUND()
    llReturn  = .F.
    
    *-- Get the variable part of the error message
    DO CASE
      CASE cIMTyp = 'I'
        lcMssgText = 'style purchase order'
      CASE cIMTyp = 'R'
        lcMssgText = 'style purchase order return'
      CASE cIMTyp = 'M'
        lcMssgText = 'cutting ticket'
      CASE cIMTyp = 'L'
        lcMssgText = 'material PO'
      CASE cIMTyp = 'F'
        lcMssgText = 'material PO return'
      CASE cIMTyp = 'T'
        lcMssgText = 'material MFG order'
      CASE cIMTyp = 'D'
        lcMssgText = 'Dye P/O'

    ENDCASE
    lcMssgText = lcMssgText + ' No. ' + cTKtNo
    
    *-- Message : 04188
    *-- "The debit amount applied to (lcMssgText) will produce a negative "
    *-- "actual cost. Cannot proceed with the saving process.             "
    *-- Button : 00000 
    *-- "                          <    Ok    >                           "
    =gfModalGen("TRM04188B00000" , "DIALOG" , lcMssgText)
  ENDIF    && End of IF FOUND()
ENDIF    && End of IF llReturn

*-- If there was no troubles found in the records locking and no records
*-- with negative actual cost found in the temp. actual cost header file.
IF llReturn
  
  *-- Search for a record with negative actual cost in the temp. actual
  *-- cost detail file.
  SELECT (lcActCstDt)
  LOCATE FOR nActCost1 < 0 .OR.;
             nActCost2 < 0 .OR.;
             nActCost3 < 0 .OR.;
             nActCost4 < 0 .OR.;
             nActCost5 < 0
  
  *-- If there is a record with negative actual cost in the temp. actual
  *-- cost detail file
  IF FOUND()
    llReturn  = .F.
    
    *-- Get the variable part of the error message
    DO CASE
      CASE cIMTyp = 'I'
        lcMssgText = 'style purchase order No. ' + cTKtNo +;
                     ', receiving session No. ' + cRSession +;
                     ', line No. ' + ALLTRIM(cLineNo)
        
      CASE cIMTyp = 'R'
        lcMssgText = 'style purchase order return No. ' + cTKtNo +;
                     ', receiving session No. ' + cRSession +;
                     ', line No. ' + ALLTRIM(cLineNo)
        
      CASE cIMTyp = 'S'
        lcMssgText = 'shipment No. ' + cShipNo +;
                     ', receiving session No. ' + cRSession +;
                     ', style purchase order No. ' + cTKtNo +;
                     ', line No. ' + ALLTRIM(cLineNo)
        
      CASE cIMTyp = 'M'
        lcMssgText = 'cutting ticket No. ' + cTKtNo +;
                     ', receiving session No. ' + cRSession +;
                     ', line No. ' + ALLTRIM(cLineNo)
        
      CASE cIMTyp = 'L'
        lcMssgText = 'material PO No. ' + cTKtNo +;
                     ', receiving session No. ' + cRSession +;
                     ', line No. ' + ALLTRIM(cLineNo)
        
      CASE cIMTyp = 'F'
        lcMssgText = 'material PO return No. ' + cTKtNo +;
                     ', receiving session No. ' + cRSession +;
                     ', line No. ' + ALLTRIM(cLineNo)
      CASE cIMTyp = 'T'
        lcMssgText = 'material MFG order No. ' + cTKtNo +;
                     ', receiving session No. ' + cRSession
      CASE cIMTyp = 'D'
        lcMssgText = 'Dying order No. ' + cTKtNo +;
                     ', receiving session No. ' + cRSession +;
                     ', line No. ' + ALLTRIM(cLineNo)

    ENDCASE
    
    *-- Message : 04188
    *-- "The debit amount applied to (lcMssgText) will produce a negative "
    *-- "actual cost. Cannot proceed with the saving process.             "
    *-- Button : 00000 
    *-- "                          <    Ok    >                           "
    =gfModalGen("TRM04188B00000" , "DIALOG" , lcMssgText)
  ENDIF    && End of IF FOUND()
ENDIF    && End of IF llReturn

*-- If troubles where found in the records locking or some of the records
*-- in the temp. actual cost header file or detail file where found.
IF !llReturn
  
  *-- Delete the records that have status same or deleted from the temp.
  *-- files (lcApVInvDt) and (lcAPInvTkt)
  SELECT (lcApVInvDt)
  DELETE ALL FOR INLIST(cStatus,'D','S')
  SELECT (lcAPInvTkt)
  DELETE ALL FOR INLIST(cStatus,'D','S')
  
  *-- Unlock the records that had been locked
  =lfUnLckPOF()
ENDIF    && End of IF !llReturn

RETURN llReturn

*!*************************************************************
*! Name      : lfUnLckPOF
*! Developer : Albert Raif (ALB)
*! Date      : 01/23/2003
*! Purpose   : Function to unlock the records that we have locked
*!             in the PO files and CT files (POSHDR, POSLN,
*!             CUTTKTH, CUTTKTL, POFHDR and POFLN).
*! Tracking# : E302010,1
*!*************************************************************
*! Calls              : None.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            :  =lfUnLckPOF()
*!*************************************************************
*
FUNCTION lfUnLckPOF

PRIVATE lcFile , lcIMTyp , lnRecNo

*-- Unlock the header files [Begin]
SELECT (lcActCstHd)
GO TOP

DO WHILE !EOF()
  STORE '' TO lcFile , lcIMTyp
  
  *-- Get the name of the header file of the current document type to
  *-- unlock the records that had been locked in it
  DO CASE
    *-- Case of document types "Style Purchase Order" and "Style Purchase
    *-- Order Return"
    
    CASE INLIST(cIMTyp , 'I' , 'R','S')
    
      lcFile = 'POSHDR'
    *-- Case of document type "Cutting Ticket"
    CASE cIMTyp = 'M'
      lcFile = 'CUTTKTH'
    *-- Case of document types "Material PO" and "Material PO Return"
    CASE INLIST(cIMTyp , 'L' , 'F')
      lcFile = 'POFHDR'
    CASE INLIST(cIMTyp , 'A','D')
      lcFile = 'POSHDR'
  ENDCASE
  
  lcIMTyp = cIMTyp
  
  *-- Scan while the document type didn't change
  SCAN WHILE cIMTyp = lcIMTyp
    
    *-- Unlock the records that had been locked in the master file
    lnRecNo = nRecNo
    SELECT (lcFile)
    GO lnRecNo
    =gfObj_Lock(.F.)
    SELECT (lcActCstHd)
  ENDSCAN    && End of SCAN WHILE cIMTyp = lcIMTyp
ENDDO    && End of DO WHILE !EOF()

*-- Erase the actual cost header temp. file 
USE

*-- Unlock the header files [End]

*-- Unlock the detail files [Begin]
SELECT (lcActCstDt)
GO TOP

DO WHILE !EOF()
  STORE '' TO lcFile , lcIMTyp
  
  *-- Get the name of the detail file for the current document type to
  *-- unlock the records that had been locked in it
  DO CASE
    *-- Case of document types "Style Purchase Order" and "Style Purchase
    *-- Order Return"
    CASE INLIST(cIMTyp , 'I' , 'R' , 'S','D')
      lcFile = 'POSLN'
    *-- Case of document type "Cutting Ticket"
    CASE cIMTyp = 'M'
      lcFile = 'CUTTKTL'
    *-- Case of document types "Material PO" and "Material PO Return"
    CASE INLIST(cIMTyp , 'L' , 'F')
      lcFile = 'POFLN'
  ENDCASE
  
  lcIMTyp = cIMTyp
  
  *-- Scan while the document type didn't change
  SCAN WHILE cIMTyp = lcIMTyp
    
    *-- Unlock the records that had been locked in the master file
    lnRecNo = nRecNo
    SELECT (lcFile)
    GO lnRecNo
    =gfObj_Lock(.F.)
    SELECT (lcActCstDt)
  ENDSCAN
ENDDO

*-- Erase the actual cost detail temp. file 
USE

*-- Unlock the detail files [End]
*!*************************************************************
*! Name      : lfUpdPoHAC
*! Developer : Albert Raif (ALB)
*! Date      : 01/23/2003
*! Purpose   : To update the actual cost in the poshdr file in case 
*!             of invoicing by shipment.
*! Tracking# : E302010,1
*!*************************************************************
*! Example            :  =lfUpdPoHAC()
*!*************************************************************
*
FUNCTION lfUpdPoHAC
PRIVATE lnAlias
STORE 1 TO lnExRate ,lnCurUnit 
lnAlias = SELECT()

IF !EMPTY(lcHdrFile)
  *-- Scan the corresponding records in the detail file (lcAPInvTkt) with
  *-- receiving session not empty.
  SELECT (lcAPInvTkt)
  
  
  SCAN REST;
        WHILE cVendCode + cApInvNo + cApVILNo = &lcApVInvDt..cVendCode +;
              &lcApVInvDt..cApInvNo + &lcApVInvDt..cAPVILNo

    lcVendInv = cVendCode + cApInvNo
    SELECT APINVHDR
    STORE '*' TO lcExSin1 ,lcExSin2
    IF SEEK(lcVendInv)
      lcExSin1  = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
      lnExRate  = APINVHDR.nExRate
      lnCurUnit = APINVHDR.nCurrUnit
    ELSE
      lnExRate  = 1
      lnCurUnit = 1
    ENDIF
    SELECT (lcAPInvTkt)
    *-- Flag to know if the record has been deleted
    llDeleted = (cStatus='D')
    *-- Variable to hold the record number of the corresponding record in
    *-- the master file
    lnRecNo   = nRecNo
    
    *-- Replace the record pointer on the corresponding record in the
    *-- master file.
    IF BETWEEN(lnRecNo ,1,RECCOUNT('APINVTKT'))
      GO lnRecNo IN APINVTKT
    ENDIF    && End of IF lnRecNo > 0
    lnAmntChng = IIF(llDeleted   , 0 , nApAplAmnt) -;
                 IIF(lnRecNo = 0 , 0 , APINVTKT.nApAplAmnt)


    *-- If there is no records for this document number in the actual
    *-- cost temp. header file
    IF !SEEK(m.cIMTyp + cTktNo , lcActCstHd)
      m.cTktNo = cTktNo
      
      SELECT (lcHdrFile)    
      IF SEEK (lcHKeyVal + m.cTktNo)        
        *-- Attempt to lock the record
        IF gfObj_Lock(.T.)
          m.nRecNo = RECNO()
          
          STORE 0 TO m.nActCost1  , m.nActCost2  , m.nActCost3 ,;
                     m.nActCost4  , m.nActCost5  ,;
                     m.nFActCost1 , m.nFActCost2 , m.nFActCost3 ,;
                     m.nFActCost4 , m.nFActCost5
          
          *-- If there is no fields for the actual cost in foreign currency
          IF EMPTY(lcHFActFld)
            *-- Get the actual cost
            FOR lnCount = 1 TO lnActCstNo
              lcCount            = ALLTRIM(STR(lnCount))
              m.nActCost&lcCount = &lcHActFild.&lcCount
            ENDFOR    && End of FOR lnCount = 1 TO lnActCstNo
          ELSE    && Else [If there is fields for the actual cost in foreign currency]
            *-- Get the actual cost
            FOR lnCount = 1 TO lnActCstNo
              lcCount             = ALLTRIM(STR(lnCount))
              m.nActCost&lcCount  = &lcHActFild.&lcCount
              m.nFActCost&lcCount = &lcHFActFld.&lcCount
            ENDFOR    && End of FOR lnCount = 1 TO lnActCstNo
          ENDIF    && End of IF EMPTY(lcHFActFld)
          
          *-- Add a corresponding record in the temp. file
          INSERT INTO &lcActCstHd FROM MEMVAR
        ELSE    && Else [If the attempt to lock the record failed]
          llReturn = .F.
        ENDIF    && End of IF gfObj_Lock(.T.)
      ENDIF
    ENDIF    && End of IF !SEEK(m.cIMTyp + cTktNo , lcActCstHd)
      
    *-- Update total actual cost in the temp. header file
    SELECT (lcActCstHd)      
    REPLACE nActCost&lcBomType WITH nActCost&lcBomType + (lnAmntChng;
                                    &lcExSin1 lnExRate &lcExSin2 lnCurUnit)
      
    *-- Update total actual cost in foreign currency in the temp. header
    *-- file
    IF !EMPTY(lcHFActFld)
      REPLACE nFActCost&lcBomType WITH nFActCost&lcBomType + lnAmntChng
    ENDIF    && End of IF !EMPTY(lcHFActFld)
  ENDSCAN
ENDIF    && End of IF !EMPTY(lcHdrFile)

SELECT (lcApVInvDt)
=SEEK(cVendCode + cApInvNo + cAPVILNo , lcAPInvTkt)

SELECT(lnAlias)
*!*************************************************************
*! Name      : lfUpdFiles
*! Developer : Albert Raif (ALB)
*! Date      : 01/23/2003
*! Purpose   : Update Master Files
*! Tracking# : E302010,1
*!*************************************************************
*! Calls     : lfOpenFile(),gfTmp2Mast()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfUpdFiles()
*!*************************************************************
FUNCTION lfUpdFiles
PRIVATE lcExStatus

=lfUpdatPOF()

lcExStatus = SET('EXACT')
SET EXACT OFF
SET ORDER TO TAG Bomcstkt IN BOMCOST
SET ORDER TO TAG CTKTBOM IN CTKTBOM
SELECT BOMLINE
SET ORDER TO TAG BOMPOREC IN BOMLINE
SELECT (lcAPInvTkt)
RECALL ALL
SELECT (lcApVInvDt)
RECALL ALL
SCAN FOR cStatus <> 'S' AND SEEK(cVendCode+cApInvNo+cAPVILNo,lcAPInvTkt)
  llDeleted = (cStatus='D') OR (cStatus='S' AND EMPTY(nRecNo))
  lnRecNo   = nRecNo
  =SEEK(&lcAPvInvDt..cvendcode+&lcAPvInvDt..capinvno,'APINVHDR')
  IF BETWEEN(lnRecNo ,1,RECCOUNT('APVINVDT'))
    SELECT APVINVDT
    GO lnRecNo
  ENDIF
  
  IF !INLIST(&lcApVInvDt..cIMTyp,'L','F','R')
  
    IF SEEK(IIF(&lcApVInvDt..cIMTyp="S","I",&lcApVInvDt..cIMTyp)+&lcApVInvDt..ctktno+&lcApVInvDt..cbomtype+;
            SPACE(19)+SPACE(6)+&lcApVInvDt..cOprCode+SPACE(10),'cTktBom')
      SELECT cTktBom
      REPLACE ISSUE_QTY WITH ISSUE_QTY - IIF(lnRecNo=0,0,APVINVDT.nApTAprQty) + IIF(llDeleted,0,&lcApVInvDt..nApTAprQty),;
              USED_QTY  WITH USED_QTY  - IIF(lnRecNo=0,0,APVINVDT.nApTAprQty) + IIF(llDeleted,0,&lcApVInvDt..nApTAprQty)
    ENDIF

    *-- If the Invoice line was deleted or if the uncontributed amount was
    *-- changed.
    IF llDeleted .OR.;
       (EMPTY(&lcAPInvTkt..cRSession) .AND. &lcAPInvTkt..cStatus <> 'S')

      IF BETWEEN(&lcAPInvTkt..nRecNo ,1,RECCOUNT('APINVTKT'))
        GO &lcAPInvTkt..nRecNo IN APINVTKT
      ENDIF
      SELECT BOMCOST
       
      lcKey = &lcApVInvDt..cbomtype+IIF(&lcApVInvDt..cIMTyp="S","I",&lcApVInvDt..cIMTyp)+&lcApVInvDt..ctktno+;
            SPACE(19)+SPACE(6)+&lcApVInvDt..cOprCode+SPACE(6)+SPACE(10)+SPACE(6)+SPACE(6)
             
      =SEEK(&lcApVInvDt..cbomtype+IIF(&lcApVInvDt..cIMTyp="S","I",&lcApVInvDt..cIMTyp)+&lcApVInvDt..ctktno+;
            SPACE(19)+SPACE(6)+&lcApVInvDt..cOprCode+SPACE(6)+SPACE(10)+;
            SPACE(6)+SPACE(6))
      LOCATE REST WHILE ;
      cBomType+cIMTyp+cTktNo+Item+IClr+MfgCode+cWareCode+cDyelot+cRSession+cISession =;
      &lcApVInvDt..cbomtype+IIF(&lcApVInvDt..cIMTyp="S","I",&lcApVInvDt..cIMTyp)+&lcApVInvDt..ctktno+SPACE(19)+;
      SPACE(6)+&lcApVInvDt..cOprCode+SPACE(6)+SPACE(10)+SPACE(6)+SPACE(6) ;
      FOR cVendCode+cApInvNo = PADR(&lcAPvInvDt..cvendcode,8)+PADR(&lcAPvInvDt..capinvno,12)
      

      DO CASE
        
        CASE !llDeleted AND !FOUND() .AND. &lcAPInvTkt..cStatus <> 'D'
          
          APPEND BLANK
          REPLACE CBOMTYPE  WITH &lcApVInvDt..cbomtype ,;
                  CIMTYP    WITH IIF(&lcApVInvDt..cIMTyp="S","I",&lcApVInvDt..cIMTyp),;
                  CTKTNO    WITH IIF(&lcApVInvDt..cIMTyp="S",&lcAPInvTkt..ctktno,&lcApVInvDt..ctktno),;
                  ITEM      WITH ''                    ,;
                  ICLR      WITH ''                    ,;
                  MFGCODE   WITH &lcApVInvDt..cOprCode ,;
                  CWARECODE WITH ''                    ,;
                  CDYELOT   WITH ''                    ,;
                  CRSESSION WITH SPACE(6)  ,;
                  CVENDCODE WITH &lcAPvInvDt..cvendcode     ,;
                  CAPINVNO  WITH PADR(&lcAPvInvDt..capinvno ,;
                  CISESSION WITH ''        ,;
                  SHIPNO    WITH ''        ,;
                  DTRANDATE WITH APINVHDR.DPOSTDATE,;
                  CCOSTTYPE WITH &lcApVInvDt..cCostType  ,;
                  NTOTQTY   WITH &lcAPInvTkt..nAPTAplQty -;
                                 IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                     APINVTKT.nAPTAplQty) ,;
                  NTOTCST   WITH &lcAPInvTkt..nAPAplAmnt -;
                                 IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                     APINVTKT.nAPAplAmnt) ,;
                  NUNITCST  WITH &lcAPInvTkt..nAPAplPric -;
                                 IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                     APINVTKT.nAPAplPric) ,;
                  NTOTACST  WITH NTOTCST  ,;
                  NUNITACST WITH NUNITCST ,;
                  Actualize WITH 'N'
          
        CASE !llDeleted AND FOUND()
          
          REPLACE NTOTQTY   WITH NTOTQTY +;
                                 IIF(&lcAPInvTkt..cStatus = 'D' , 0 ,;
                                     &lcAPInvTkt..nAPTAplQty) -;
                                 IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                     APINVTKT.nAPTAplQty) ,;
                  NTOTCST   WITH NTOTCST +;
                                 IIF(&lcAPInvTkt..cStatus = 'D' , 0 ,;
                                     &lcAPInvTkt..nAPAplAmnt) -;
                                 IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                     APINVTKT.nAPAplAmnt) ,;
                  NUNITCST  WITH IIF(&lcAPInvTkt..cStatus = 'D' , 0 ,;
                                 NTOTCST/NTOTQTY);
                  NTOTACST  WITH NTOTCST  ,;
                  NUNITACST WITH NUNITCST

          IF (&lcAPInvTkt..cStatus = 'D') .OR. (NTOTQTY = 0)
            DELETE
          ENDIF
        
        CASE llDeleted AND FOUND()
          REPLACE NTOTQTY   WITH NTOTQTY -;
                                 IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                     APINVTKT.nAPTAplQty) ,;
                  NTOTCST   WITH NTOTCST -;
                                 IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                     APINVTKT.nAPAplAmnt) ,;
                  NUNITCST  WITH NUNITCST -;
                                 IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                       APINVTKT.nAPAplPric) ,;
                  NTOTACST  WITH NTOTCST  ,;
                  NUNITACST WITH NUNITCST
          
          IF (&lcAPInvTkt..cStatus = 'D') .OR. (NTOTQTY = 0)
            DELETE
          ENDIF
          
      ENDCASE
      
    ENDIF    && End of IF EMPTY(&lcAPInvTkt..cRSession) .AND. ...
    SELECT (lcAPInvTkt)
    SCAN REST WHILE cVendCode+cApInvNo+cAPVILNo = ;
                    &lcApVInvDt..cVendCode+&lcApVInvDt..cApInvNo+&lcApVInvDt..cAPVILNo ;
              FOR   !EMPTY(cRSession)      
      llDeleted = (cStatus='D') OR (cStatus='S' AND EMPTY(nRecNo))
      lnRecNo = nRecNo
      
      IF !llDeleted AND BETWEEN(lnRecNo ,1,RECCOUNT('APINVTKT'))
        SELECT APINVTKT
        GO lnRecNo
      ENDIF
      SELECT BomLine
      =SEEK(&lcApVInvDt..cIMTyp+"3"+&lcApVInvDt..cBomType+&lcApVInvDt..cOprCode+;
            &lcApVInvDt..ctktno+&lcAPInvTkt..cRSession+&lcApVInvDt..shipno+;
            STR(&lcApInvTkt..LineNo,6)+&lcApInvTkt..Item+&lcApInvTkt..Color)

      DO CASE
        CASE !llDeleted AND !FOUND()
           APPEND BLANK
           REPLACE cimtyp    WITH IIF(&lcApVInvDt..cIMTyp="S","I",&lcApVInvDt..cIMTyp) ,;
                   ctype     WITH '3'                   ,;
                   cbomtyp   WITH &lcApVInvDt..cBomType ,;
                   mfgcode   WITH &lcApVInvDt..cOprCode ,;
                   ctktno    WITH IIF(&lcApVInvDt..cIMTyp="S",&lcAPInvTkt..ctktno,&lcApVInvDt..ctktno),;
                   crsession WITH &lcAPInvTkt..cRSession,;
                   shipno    WITH &lcApVInvDt..shipno   ,;
                   lineno    WITH &lcAPInvTkt..LineNo   ,;
                   style     WITH &lcAPInvTkt..Item     ,;
                   sclr      WITH &lcAPInvTkt..Color    ,;
                   cCatgTyp  WITH &lcApVInvDt..cCostType,;
                   UnitQty   WITH  1 ,;
                   StyQty    WITH &lcAPInvTkt..nApTAplQty ,;
                   ItemQty   WITH StyQty ,;
                   ItemAmt   WITH &lcAPInvTkt..nApAplAmnt ,;
                   UnitCost  WITH IIF(ItemQty=0,0,ItemAmt/ItemQty)
        
        
        CASE !llDeleted AND FOUND()
          REPLACE StyQty   WITH StyQty - IIF(lnRecNo=0,0,APINVTKT.nApTAplQty) + &lcAPInvTkt..nApTAplQty ,;
                  ItemQty  WITH StyQty ,;
                  ItemAmt  WITH ItemAmt -IIF(lnRecNo=0,0,APINVTKT.nApAplAmnt) + &lcAPInvTkt..nApAplAmnt ,;
                  UnitCost WITH IIF(ItemQty=0,0,ItemAmt/ItemQty)
        CASE llDeleted AND FOUND()
          REPLACE StyQty   WITH StyQty - &lcAPInvTkt..nApTAplQty ,;
                  ItemQty  WITH StyQty ,;
                  ItemAmt  WITH ItemAmt - &lcAPInvTkt..nApAplAmnt ,;
                  UnitCost WITH IIF(ItemQty=0,0,ItemAmt/ItemQty)
          IF ItemQty = 0
            DELETE
          ENDIF
      ENDCASE
      SELECT BOMCOST
      
      lcKey = &lcApVInvDt..cbomtype+IIF(&lcApVInvDt..cIMTyp="S","I"+&lcApInvTkt..ctktno,&lcApVInvDt..cIMTyp+&lcApVInvDt..ctktno)+;
              SPACE(19)+SPACE(6)+&lcApVInvDt..cOprCode+SPACE(6)+SPACE(10)+&lcAPInvTkt..cRSession+SPACE(6)
            
      =SEEK(&lcApVInvDt..cbomtype+IIF(&lcApVInvDt..cIMTyp="S","I"+&lcApInvTkt..ctktno,&lcApVInvDt..cIMTyp+&lcApVInvDt..ctktno)+;
            SPACE(19)+SPACE(6)+&lcApVInvDt..cOprCode+SPACE(6)+SPACE(10)+;
            &lcAPInvTkt..cRSession+SPACE(6))
            
      LOCATE REST WHILE ;
      cBomType+cIMTyp+cTktNo+Item+IClr+MfgCode+cWareCode+cDyelot+cRSession+cISession =;
      &lcApVInvDt..cbomtype+IIF(&lcApVInvDt..cIMTyp="S","I"+&lcApInvTkt..ctktno,&lcApVInvDt..cIMTyp+&lcApVInvDt..ctktno)+SPACE(19)+;
      SPACE(6)+&lcApVInvDt..cOprCode+SPACE(6)+SPACE(10)+&lcAPInvTkt..cRSession+SPACE(6) ;
      FOR cVendCode+cApInvNo = PADR(&lcAPvInvDt..cvendcode,8)+PADR(&lcAPvInvDt..capinvno,12)
      
      DO CASE
        CASE !llDeleted AND !FOUND()
          APPEND BLANK
          REPLACE CBOMTYPE  WITH &lcApVInvDt..cbomtype ,;
                  CIMTYP    WITH IIF(&lcApVInvDt..cIMTyp="S","I",&lcApVInvDt..cIMTyp) ,;
                  CTKTNO    WITH IIF(&lcApVInvDt..cIMTyp="S", &lcApInvTkt..ctktno ,&lcApVInvDt..ctktno ),;
                  ITEM      WITH ''                    ,;
                  ICLR      WITH ''                    ,;
                  MFGCODE   WITH &lcApVInvDt..cOprCode ,;
                  CWARECODE WITH ''                    ,;
                  CDYELOT   WITH ''                    ,;
                  CRSESSION WITH &lcAPInvTkt..cRSession,;
                  CVENDCODE WITH &lcAPvInvDt..cvendcode ,;
                  CAPINVNO  WITH &lcAPvInvDt..capinvno ,;
                  CISESSION WITH ''        ,;
                  SHIPNO    WITH ''        ,;
                  DTRANDATE WITH APINVHDR.DPOSTDATE,;
                  CCOSTTYPE WITH &lcApVInvDt..cCostType ,;
                  NTOTQTY   WITH &lcAPInvTkt..nApTAplQty ,;
                  NTOTCST   WITH &lcAPInvTkt..nApAplAmnt ,;
                  NUNITCST  WITH IIF(NTOTQTY=0,0,NTOTCST/NTOTQTY)  ,;
                  NTOTACST  WITH NTOTCST  ,;
                  NUNITACST WITH NUNITCST ,;
                  Actualize WITH 'Y'
          
        CASE !llDeleted AND FOUND()
          
          REPLACE NTOTQTY   WITH NTOTQTY - IIF(lnRecNo=0,0,APINVTKT.nApTAplQty) + &lcAPInvTkt..nApTAplQty ,;
                  NTOTCST   WITH NTOTCST - IIF(lnRecNo=0,0,APINVTKT.nApAplAmnt) + &lcAPInvTkt..nApAplAmnt ,;
                  NUNITCST  WITH IIF(NTOTQTY=0,0,NTOTCST/NTOTQTY)  ,;
                  NTOTACST  WITH NTOTCST  ,;
                  NUNITACST WITH NUNITCST ,;
                  Actualize WITH 'Y'
          
        CASE llDeleted AND FOUND()
          REPLACE NTOTQTY   WITH NTOTQTY -  IIF(lnRecNo=0,0,&lcAPInvTkt..nApTAplQty) ,;
                  NTOTCST   WITH NTOTCST -  IIF(lnRecNo=0,0,&lcAPInvTkt..nApAplAmnt) ,;
                  NUNITCST  WITH IIF(NTOTQTY=0,0,NTOTCST/NTOTQTY)  ,;
                  NTOTACST  WITH NTOTCST  ,;
                  NUNITACST WITH NUNITCST ,;
                  Actualize WITH 'Y'
          IF NTOTQTY = 0
            DELETE
          ENDIF
      ENDCASE
    ENDSCAN
  ENDIF
ENDSCAN
SELECT (lcApVInvDt)
DELETE ALL FOR INLIST(cStatus,'D','S')
SELECT (lcAPInvTkt)
DELETE ALL FOR INLIST(cStatus,'D','S')
=gfTmp2Mast('APVINVDT',lcAPvInvDt)
=gfTmp2Mast('APINVTKT',lcAPInvTkt)

SET ORDER TO TAG (lcAPInvTkt) IN (lcAPInvTkt)
SET EXACT &lcExStatus

*!*************************************************************
*! Name      : lfUpDatPOF
*! Developer : Albert Raif (ALB)
*! Date      : 01/23/2003
*! Purpose   : Function to Update the PO files and CT files
*!             (POSHDR, POSLN, CUTTKTH, CUTTKTL, POFHDR and POFLN).
*! Tracking# : E302010,1
*!*************************************************************
*! Calls              : None.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            :  =lfUpDatPOF()
*!*************************************************************
*
FUNCTION lfUpdatPOF
PRIVATE lcFile , lcIMTyp , lnRecNo , lcScatFlds , lcGathFlds , laScatVal

*-- Update and unlock the header files [Begin]
SELECT (lcActCstHd)
GO TOP

DO WHILE !EOF()
  STORE '' TO lcFile , lcIMTyp , lcScatFlds , lcGathFlds
  DIMENSION laScatVal[1]
  laScatVal = 0
  
  *-- Get the name of the header file, the scatter fields and the gather
  *-- fields for the current document type to update it and unlock the
  *-- records that had been locked.
  DO CASE
    *-- Case of document types "Style Purchase Order" and "Style Purchase
    *-- Order Return"

    CASE INLIST(cIMTyp , 'I' , 'R','S','D')
      lcFile     = 'POSHDR'

      lcScatFlds = 'nActCost1 , nActCost2 , nActCost3 , nActCost4 ,;
                    nActCost5 , nFActCost1 , nFActCost2 , nFActCost3, ;
                    nFActCost4 , nFActCost5'

      lcGathFlds = 'LIKE nAct_Cost? , nFActCost?'
    *-- Case of document type "Cutting Ticket"
    CASE cIMTyp = 'M'
      lcFile = 'CUTTKTH'
      lcScatFlds = 'LIKE nActCost?'
      lcGathFlds = 'LIKE nAct_Cost?'
    *-- Case of document types "Material PO" and "Material PO Return"
    CASE INLIST(cIMTyp , 'L' , 'F')
      lcFile = 'POFHDR'
      lcScatFlds = 'nFActCost1 , nFActCost2 , nFActCost3 , nFActCost4 ,;
                    nActCost1 , nActCost2 , nActCost3 , nActCost4'
      lcGathFlds = 'LIKE nEActCost? , nAct_Cost?'
    *-- Case of document type "Material Cutting Ticket"
    CASE cIMTyp = 'T'
      lcFile = 'MMFGORDH'
      lcScatFlds = 'LIKE nActCost?'
      lcGathFlds = 'LIKE nAct_Cost?'
  ENDCASE
  
  lcIMTyp = cIMTyp
  *-- Scan while the document type didn't change
  SCAN WHILE cIMTyp = lcIMTyp
    
    *-- Update the records of the master file
    SCATTER FIELDS &lcScatFlds TO laScatVal
    lnRecNo = nRecNo
    SELECT (lcFile)
    GO lnRecNo
    GATHER FIELDS &lcGathFlds FROM laScatVal
    
    *-- Unlock the records that had been locked in the master file
    =gfObj_Lock(.F.)
    SELECT (lcActCstHd)
  ENDSCAN    && End of SCAN WHILE cIMTyp = lcIMTyp
ENDDO    && End of DO WHILE !EOF()

*-- Erase the actual cost header temp. file 
USE

*-- Update and unlock the header files [End]


*-- Update and unlock the detail files [Begin]
SELECT (lcActCstDt)
GO TOP

DO WHILE !EOF()
  STORE '' TO lcFile , lcIMTyp , lcScatFlds , lcGathFlds
  DIMENSION laScatVal[1]
  laScatVal = 0
  
  *-- Get the name of the detail file, the scatter fields and the gather
  *-- fields for the current document type to update it and unlock the
  *-- records that had been locked.
  DO CASE
    *-- Case of document types "Style Purchase Order" and "Style Purchase
    *-- Order Return"

    CASE INLIST(cIMTyp , 'I' , 'R','S','D')
      lcFile     = 'POSLN'
      lcScatFlds = "nFActCost1 , nFActCost2 , nFActCost3 , nFActCost4 , ;
                    nFActCost5 , nActCost1 , nActCost2 , nActCost3 , ;
                    nActCost4 ,nActCost5"
      lcGathFlds = 'LIKE nEActCost? , nAct_Cst?'
    *-- Case of document type "Cutting Ticket"
    CASE cIMTyp = 'M'
      lcFile = 'CUTTKTL'
      lcScatFlds = 'LIKE nActCost?'
      lcGathFlds = 'LIKE nAct_Cst?'
    *-- Case of document types "Material PO" and "Material PO Return"
    CASE INLIST(cIMTyp , 'L' , 'F')
      lcFile = 'POFLN'
      lcScatFlds = 'nFActCost1  , nFActCost2  , nFActCost3  , nFActCost4 , ' +;
                   'nActCost1 , nActCost2 , nActCost3 , nActCost4'

      lcGathFlds = 'LIKE nEActCost? , nAct_Cost?'
    *-- Case of document type "Material Cutting Ticket"
    CASE cIMTyp = 'T'
      lcFile = 'MMFGORDD'
      lcScatFlds = 'LIKE nActCost?'
      lcGathFlds = 'LIKE nAct_Cost?'
  ENDCASE
  
  lcIMTyp = cIMTyp
  
  *-- Scan while the document type didn't change
  SCAN WHILE cIMTyp = lcIMTyp
    *-- Update the records of the master file
    SCATTER FIELDS &lcScatFlds TO laScatVal
    lnRecNo = nRecNo
    SELECT (lcFile)
    GO lnRecNo
    GATHER FIELDS &lcGathFlds FROM laScatVal
    
    *-- Unlock the records that had been locked in the master file
    =gfObj_Lock(.F.)
    SELECT (lcActCstDt)
  ENDSCAN    && End of SCAN WHILE cIMTyp = lcIMTyp
ENDDO    && End of DO WHILE !EOF()

*-- Erase the actual cost detail temp. file 
USE
*-- Update and unlock the detail files [End]
*!*************************************************************
*! Name      : lfCloseFile
*! Developer : ALBERT RAIF (ALB) 
*! Date      : 1/22/2003
*! Purpose   : Close opened files
*! Tracking# : E302010,1
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfCloseFile()
*!*************************************************************
FUNCTION lfCloseFile

FOR lnCount = 1 TO ALEN(laOpenFile,1)
  IF laOpenFile[lnCount,2]
    SELECT (laOpenFile[lnCount,1])
    USE
  ENDIF
ENDFOR

*!*************************************************************
*! Name      : lfvBrows
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/28/2003
*! Purpose   : Valid function for the browse.
*!*************************************************************
*! Calls     : gfStopBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvBrows()
*!*************************************************************
*B607340,1 AMH
FUNCTION lfvBrows

IF !(WONTOP(lcWSheet) .OR. WONTOP(lcDettitle) .OR. WONTOP(lcOprHdTit) .OR.;
     WONTOP(lcBrZoom) .OR. WONTOP(lcOprDtTit))
  glFromBrow = .T.
  = gfStopBrow()
ENDIF
*-- end of lfvBrows.