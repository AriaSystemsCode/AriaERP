*:************************************************************************
*: Program file  : SoOrd.PRG
*: Program desc. : Sales Orders
*:
*:         System: Aria Apparel System
*:      Developer: WAM - Wael Aly Mohamed
*:************************************************************************
*: Calls : 
*:         Functions  : 
*:*************************************************************
*: Passed Parameters  : lcOrdType : Order type
*:                             'O' : Order 
*:                             'C' : Contract
*:                             'T' : EDI temporary Order 
*:                      lcOrderNo : Order#
*:*************************************************************
*: Modifications      :
*E300834,1 WAM 08/11/98 - Get Style price discount according to style 
*E300834,1                discount code.
*E300834,1              - Get style price according to ordered quantity
*E300834,1                when the customer use price level at quantity.
*E300956,1 WAM 08/16/98 - Add new file to keep a history of order calceled 
*E300956,1                quantities per lines
*B602179,1 AMM 11/25/98 Erase temporary files when quitting the program
*E301086,1 WAM 12/09/98 Add new Type 'T' for EDI temporary Order 
*B801876,1 WAM 12/28/98 Default order priority to customer priority
*E301175,1 HS  12/30/1998 Add the capability to call the sales order Save
*E301175,1                and Delete functions from the EDI
*E301175,1                - from EDIProcessPO Class -.
*E301175,1                Note: to do this i have removed the code of the 
*E301175,1                      functions lpSavScr, lpDelScr and lfGetInfo
*E301175,1                      to a new program called SOUPDATE with the 
*E301175,1                      names lfSavScr, lfDelScr, lfGet_Info.
*B602410,1 WAM 01/03/99 Refresh style-scale relation
*B602418,1 WAM 01/07/99 Update dyelot allocated quantity while cancel order.
*B602446,1 WAM 01/11/1999 Elimenate order amount roundation
*B801941,1 WAM 02/22/1999 Fix store browse
*B602571,1 WAM 02/22/1999 Update order totals properly
*E301142,1 WAM 02/22/1999 Select alternative price level
*B602479,1 WAM 02/23/1999 Confirm removing if order line has quantity allocated.
*E301077,5 WAM 02/28/1999 Inhance openning files to speed up transaction
*B602621,1 WAM 03/04/1999 Do not show voided invoices
*B602653,1 WAM 03/17/1999 Update order line store modification
*E301176,1 HDM 03/22/1999 Prevent programs from displaying notepad icon
*                           as it's now controlled globally
*B602681,1 WAM 03/18/1999 Seek the session record in the uncompeted sessions 
*B602681,1                file after return from other called screen.
*B602622,1 WAM 03/22/1999 Disable sales rep comm when no sales reps entered.
*E301182,1 WAM 03/23/1999 Update CT/PO line number in the CUTPICK file.
*B602636,1 WAM 03/24/1999 Defualt pack quantity when selecting style pack
*B802094,1 WAM 04/01/1999 Disable summary Production, Depletion, Bulk and 
*B802094,1                Cancelled transactions.
*B602740,1 MAN 04/07/1999 If the program is called from another program and you
*B602740,1 MAN            try to edit the current record you get ALIAS not Found
*B802148,1 HDM 04/20/1999 Add Picked Quantity Field To the Details Browse
*                         Case browsing Sizes Display All PIK Quantities
*                         Case browsing Totals Display oly TotPik Field
*C101493,1 AMM 04/25/1999 Call a new screen to edit order line in case the 
*C101493,1 AMM            user typed part of the style or in case of extended 
*C101493,1 AMM            size scale.
*B802187,1 TAK 04/26/1999 Fixed alias codes not found.
*B602820,1 TAK 04/26/1999 Fixed wrong mesage button preview if style on hold.
*B602825,1 TAK 04/26/1999 Added to check the alocated qty in prepak qty changed.
*B602848,1 MAN 04/28/1999 Add Fix error Record is in use by another
*B802231,1 WAM 05/06/1999 Consider order type when enter order# manualy
*B602896,1 TAK 05/12/1999 Added to check if no contracts for this account
*B602896,1                don't check the contracts again, till account changed.
*B802235,1 TAK 05/12/1999 Fixed bug record out of range in temlates screen.
*B602874,1 TAK 05/19/1999 Fixed bug subscrip out of bound in salesrep browse.
*B602925,1 TAK 05/27/1999 Fixed bug Flag not found when return from line note.
*C101557,1 TAK 06/08/1999 Added a new code for Order Category in summary folder.
*E301253,1 TAK 06/10/1999 Display the buyers from available contracts with able to 
*E301253,1                select one or add a new contract on the fly.
*B802324,1 TAK 06/11/1999 Added a new field for sales link code in OrdHdr.
*B802324,1                change all 'lcGlSales' to work with field laData[53].
*B802176,1 TAK 06/21/1999 Fixed wrong second sales rep commision for store.
*B603010,1 AMM 06/23/1999 Fix the bug of "Missing Expression" when editing total line quantity
*B802351,1 TAK 07/18/1999 Add to take the insurence default from customer.
*B802404,1 TAK 07/18/1999 Fixed wrong sizes desc in return from other screen.
*B802431,1 WAM 07/12/1999 Fix update template order lines when distribute bulk order lines.
*C200081,1 Reham On 06/27/1999
*C200081,1 1- Add new custom fields to the order header.
*C200081,1 2- Add an event for the when/valid functions of the start date.
*C200081,1 3- Add an event for the when/valid functions of the complete date.
*E301288,1 Reham On 07/05/1999
*E301288,1 1- Add new push button in the details folder <Configure> , this 
*E301288,1 button call a screen to add positions of name drops or/and 
*E301288,1 design codes for the current order line in the details folder.
*E301288,1 2- Add new object in the header screen for back order yes or no.
*E301288,1 3- Validate the gross price and price if the order line is adornment.
*E301288,1 4- Add new bar to browse the related records from the CutPick file.
*E301288,1 5- Copy the adornment records from the bulk orders.
*B802517,1 WAM 08/05/1999 Fix Alias not found error
*B802436,1 AMM 08/15/1999 Fix some bugs in extended size scale screen
*B802366,1 AMM 08/18/1999 Adjust ship via code POPUP in case of multi store orders
*E301305,1 AMM 08/25/1999 Make the complete date per order line due to a system setting
*E301313,1 AMM 08/29/1999 Add a system setting for editing price and discount per order line.
*B603136,1 TAK 09/01/1999 Fixed the wrong price adder currency in config.
*B802544,1 WAM 08/20/1999 Fix Inquire depleted orders
*B802367,1 AMM 09/02/1999 Fix bug in start date validation
*B802405,1 AMM 09/08/1999 Fix the bug when calling other screens while running sales 
*B802405,1 AMM            order screen and the browse is the active object
*B802588,1 AMM 09/09/1999 Don't go to first record when removing order line
*B802585,1 AMM 09/09/1999 Fix the bug of wronge decimal point in numeric fields 
*B802585,1 AMM            in the extended size scale screen
*E500295,1 AMM 09/22/1999 Add a locate feature to the select store browse 
*E500295,1 AMM            in multi store screen
*E301289,1 WAB 08/17/1999 change the PO lines & CT lines (selling price and gros margin)
*E301289,1 WAB 			  if the user modified the sales order line price
*B802652,1 AMM 09/28/1999 Adjust calculation of booked quantity in orders 
*B802652,1 AMM            created from bulk order in multi store case
*B802650,1 AMM 09/29/1999 Fix bug in miscalulating sales rep comm. when entering order discount
*B603211,4 WAB 10/17/1999 When you enter a adornment on a style/color it is supposed to 
*B603211,4 WAB            retain for subsequent lines.
*B802733,1 AMM 10/27/1999 Fix the bug of not displaying the cancelation reason screen in some cases
*B603255,1 WAB 10/31/1999 Fix the bug of when enters multiple adorned line , the adornment cost
*B603255,1 WAB   		  adder is not being brought forward & the price adder coming from
*B603255,1 WAB 			  The design file is not being coverted in case multi currncy
*B802782,1 ABD 11/17/1999 Fix the bug of not updating the Customer PO when creating an order 
*B802782,1 ABD            from bulk order
*B802805,1 AMM 11/21/1999 Check the sold out date of the style which edited its quantity .
*B802821,1 AMM 11/22/1999 Fix a bug in calculating the depletion/cancel date
*B802663,1 AMM 12/02/1999 Fix the bug of not initializing reorder check box when creating new order
*B802772,4 AMM 12/07/1999 Display the status of modified EDI orders
*B802859,1 SSH 12/14/1999 Fix the bug of automatically bounces back to the
*B802859,1 SSH            original Order.
*B603368,1 AMM 12/27/1999 Don't allow user to edit CT quantity if he has no access.
*B802926,1 ABD 01/10/2000 Fix The bug of when enters orders in multi store case of Specific
*B802926,1 ABD 01/10/2000 Account get the stores of this account not the first account.
*B603391,1 TAK 01/23/2000 Display designs ordered in configure screen popup.
*E500309,1 AMM 01/23/2000 Deduct bulk order quantity from the booked quantity due to a system setting (SOUPDATE.PRG)
*B802613,1 ADEL 02/08/2000 Show shipped qty correctly when showing summary for multi-orders.
*B802913,1 RAMY 02/10/2000 Fix the bug of 'Alias 'STYLE' not found' when returning from other screen
*B603449,1 ABD 02/14/2000 Fix Bug that Sales Order program accetps Negative Quantity.
*B603478,1 ABD 02/29/2000 Fix bug that don't browse orders have the same 
*B603478,1 ABD 02/29/2000 account and Custpo when entering account and Cust. PO.
*B803049,1 ABD 03/05/2000 Fix bug 'alias not found' in the sales order screen.                  
*B803083,1 AMM 03/06/2000 Fix the bug of 'End of file encountered'
*B602713,1 ABD 03/08/2000 The message "Completion date is a Sunday Or Saturday " 
*B602713,1 ABD            Should not appear for Egypt.
*B803068,1 ABD 03/08/2000 Change the sales order screen to allow to make an order for
*B803068,1 ABD            status 'Potential' of accounts.
*B603325,1 ABD 03/12/2000 Fix the bug that slaes order does not update the piktkt file with
*B603325,1                The new CustPO#. (Done in soupdate.prg)
*C101772,1 ADEL 03/21/2000 Add Aria's custom payment schedule screen.
*C200115,1 ADEL 04/20/2000 Update customer profile files and make some custom changhes for Aria.
*B603558,1 AMM  05/07/2000 Fix the bug of terminating the sales order screen after calling the adornment screen.
*C101887,1 KHM 05/26/2000  Adding a new custom process for GMA to add a Bar
*C101887,1 KHM 05/26/2000  'Update Edi Fields'.
*B603421,1 AMM 06/04/2000 Display the cancelation reason screen when removing sales line.
*E301399,1 AMM 06/04/2000 Add the department# in temp EDI orders browse
*B803278,1 ABD 06/05/2000 Fix Bug that when a customer is on hold get a message 
*B803278,1 ABD            And try to cancel this order and get the same message again.
*B802614,1 ABD 06/07/2000 Distribution order should inherit bulk order status.
*E301401,1 ABD 06/07/2000 For EDI Temporary Orders, change price columns to labelled
*E301401,1 ABD            "Transmitted Price" and Amount to labelled "Wholesale Price," 
*E301401,1 ABD            Both on-screen and in outputs.
*B603678,1 AMM 06/07/2000 Fix the bug of wronge updating the sales link code.
*B603506,1 ABD 06/12/2000 Increase amount sizes in summary folder to fit currencies.
*B603506,1 ABD            [BookAmt,ShipAmt,CancelAmt,Openamt]
*B603606,1 ABD 06/13/2000 Prevent the user changing or deleteing style has allocated 
*B603606,1 ABD            Or shipped quantities
*B802891,1 ABD 06/14/2000 Fix bug  the filter in the record option to work for the navigation keys
*B803373,1 AME 06/25/2000 Fx the bug of viewing the store address instead of distribution center add.
*B803403,1 ABD 07/18/2000 Fix bug that wrong update for the boooked Qty when cancelling a bulk order.
*B803403,1 ABD            Fix in program SoUpDate.
*B803414,1 ABD 07/19/2000 Fix bug that Variable "ctktlineno" not found that
*B803414,1 ABD            ctktlineno didn't add to  to memvar Fix in program SoUpDate. 
*B803375,1 AME 07/19/2000 Fix the bug not updating sales rep value
*B603776,1 AMM 07/26/2000 Some bugs in the extended size scale screen.
*B603784,1 AMM 07/31/2000 Enhance the speed of opening the packing screen
*B803418,1 KHM 08/07/2000 Fix the bug of displaying the EDI screen when
*B803418,1                choosing any option of the pad rather than 
*B803418,1                the Update EDI option
*E301452,1 ABD 08/21/2000 Read the GL link code of the factor while creating the sales order. 
*E301452,1 ABD            If there is a factor assgined to the current customer.
*B603838,1 ABD 08/21/2000 Fix bug that when update Qty per size The cancelled qty 
*B603838,1 ABD            Is not getting updated Correct.
*B803646,1 SSE 09/12/2000 Fix Bug if you have a lot of generic packs , System will show only
*B803646,1                The first generic pack. 
*B603890,1 AMH 09/13/2000 Fix bug that Empty GL_Link Field
*B803690,1 AMM 09/21/2000 Add a new status to the modified orders from EDI
*B803681,1 SSH 22/09/00  Enhancement performance.
*B803710,1 ADEL 10/04/2000 - Let the user select any store when entering packs or styles.
*B803702,1 ADEL 10/03/2000 - Add a wait window to show selected packs.
*B803702,1                 - Set the cursor on the first record in the packs browse.
*B803702,1                 - Enhance performance.
*B603963,1 ADEL 10/03/2000 - Fix the bug of getting an incorrect record in packs screen if the
*B603963,1 ADEL 10/03/2000   the account hasn't any pack ids.
*B603933,1 ADEL 10/04/2000 Enhance performance.
*E301478,1 ADEL 10/08/2000 Add a locate feature to the PACKS screen.
*B803739,1 ADEL 10/16/2000 Fix the bug of wrong updating the sales link code for any record
*B803739,1                 added in Edit mode.
*B803728,1 ADEL 10/17/2000 If the SO is set to enetr Complete date per line and we make Multi-Store
*B803728,1                 order by template and enter complete per line deffirent from SO's header
*B803728,1                 complete date the program doesn't save it but it saves all lines with
*B803728,1                 the Header's Complete date.
*B803652,1 ADEL 10/02/2000 - Don't Upadte OrdCanLn when removing a line unless checking depltion date.
*B803652,1                 - Update OrdCanLn with Style,Account,Store and Dyelot.
*B803862,1 AAN 11/26/2000 - The button of store should be disabled until select at least one style.
*:**********************************************************************
PARAMETERS lcOrdType,lcOrderNo

EXTERNAL ARRAY laData,laKeyField , laWobjects

DECLARE lafolders[3,2],laAddress[6,3],laQtyPaste[1,8],lafoldwinds[3,2],;
        laKeyField[2,4],laSetups[14,2] 

*C101557,1 Added a new code array (Order category).
*DECLARE laCodes[6,10],laTerms[1,2],laSeasons[1,2],laDivision[1,2],;
        laShipVia[1,2],laSpcInst[1,2],laWareHouses[1,2],laCanReason[1,2]
DECLARE laCodes[7,10],laTerms[1,2],laSeasons[1,2],laDivision[1,2],;
        laShipVia[1,2],laSpcInst[1,2],laWareHouses[1,2],laCanReason[1,2],;
        laOCtgCd[1,2]        
*C101557,1 End.

DECLARE laDRltFld[2,2],laVariables[6],laOrdStatus[5],laBrowArr[1]
*E300834,1 Declare discount code related fields array
DECLARE laDisRltFld[1,2]
laOrdStatus[1] = 'Bid      '
laOrdStatus[2] = 'Open     '
laOrdStatus[3] = 'Hold     '
laOrdStatus[4] = 'Cancelled'
laOrdStatus[5] = 'Complete '
*E301305,1 AMM Complete date per line logic variable 
llCDPerL = .F.
*E301305,1 AMM end
*E301313,1 AMM Variable to get the system setting (Edit price per order line)
llESTYPSO = .T.
*E301313,1 AMM end
*E301478,1 (Begin) Initialize a new variable.
llClearPck = .T.
llFromPack = .F.
*E301478,1 (End)
*C101557,1 Added a new code (Order category).
*B603449,1 ABD add the initialization of lcOldValue
STORE '' TO laCodes,laTerms,laShipVia,laSpcInst,laSeasons,laDivision,laCanReason,;
            laVariables,lcOrderBr,laWareHouses,lcTranBr,lcTranBr1,lcTranBr2,;
            lcOldCustPo,laOCtgCd,lcOldValue
*C101557,1 End.

*B803373,1 AME[Start]
STORE .F. to llMove 
*B803373,1  AME[End]

laKeyField[1,1] = 'lcOrdType'
laKeyField[1,2] = .F.
laKeyField[1,3] = 'ORDHDR'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[1]'
laKeyField[2,2] = .T.
laKeyField[2,3] = 'ORDHDR'
laKeyField[2,4] = 2

STORE .F. TO laSetups,llRebuild,llIgnorAll,llDomestic,m.lContract
lcDelMesag    = 'cancel'
laDefProc[7]  = .F.     && Cancel/Uncancel procedure(lpDelScr)
laDefProc[9]  = .F.     && Save procedure(lpSavScr)
laDefProc[10] = .F.     && close procedure(lpClsScr)

*E300834,1 Initialize discount percent
STORE 0   TO lnDisc_Pcnt

STORE 0   TO lnMarker,lnTopMark,lnBotMark,lnTMarker,lnStyLngth
STORE ' ' TO lcOrdBrow,lcBrTtl1,lcBrTtl2,lcBrowseTl,lcBrTtlM,lcGlYear,;
             lcGlPeriod,lcODefSes,lcODefDiv,lcPriceLvl,llAlowNew,;
             lcFolder,lcSession,lcScFields,lcScrMode,laBrowArr,lcCurrOrd
STORE ''  TO lcWinCh1,lcWinCh2,lcWinCh3,lcWinCh30,lcWinCh31,lcWinCh32,;
             lcWinCh321,lcWinCh322,lcWinCh33,lcWinCh34,lcWinCh4,;
             lcOrderFile,lcTranFile,lcStyHdr,lcEmptySty
*E301077,5 WAM 02/28/1999 Inhance openning files to speed up transaction
*STORE .F. TO llGl_Link,llCutPick,llInvLine,llSycCurr,llPikTkt,llRep_Div
*STORE .F. TO llSpck_hdr,llSpck_Lin,llPosHdr,llPosLn,llCutTktH,llCutTktL
*E301077,5 (End)

*E301288,1 Reham On 07/05/1999   *** Begin ***
*E301288,1 Flag hold the system setup setting for: "Use Variant Cost Sheet" Yes/No.
llBomVarnt = .F.
*E301288,1 Variable hold the lenght of the the style major.
lnMajorLen = 1
*E301288,1 Variable hold the temp. file name that hold the styles' positions.
lcT_BomVar = ""
*E301288,1 Define variable to hold the prompt of the configure button.
lcConfig   = "\<Configure"
*E301288,1 Add new object in the header screen for back order yes or no.
lcBackOrd  = "No "
*E301288,1 Reham On 07/05/1999   *** End   ***

*C101772,1 (Begin) Initialize variables needed.
STORE 0 TO lnBookAmt,lnBalance
lcPayTmp = gfTempName()
*C101772,1 (End)  
*C200115,1 (Begin) THis flag is to determine whether we will update Customer Pofile files or not.
llOpenOrd = .F.
*C200115,1 (End)

STORE ''  TO   lcBaseFild,lcBaseTit,lcGlSales
lcorderfile = 'ORDLINE'
STORE {} TO ldDefOrdDate
STORE 0  TO lnCartons,lnLines,lnTLines,lnSouComm1,lnSouComm2,lnActFolder
STORE 1  TO lnOrdStatus,lnSortBy,lnOrdTrans,lnShipAddr,lnWareHouse
STORE '' TO lcLastStyle,lcFiles
*B603211,4 WAB - intilise variable for holding lastorderno and lastline
*B603211,4 WAB - and open style position file
*B603211,4 WAB -START
store '' to lclastOrder , lcLastLine
=gfOpenFile(gcDataDir+'ICSTYPOS',gcDataDir+'ICSTYPOS','SH')
=gfOpenFile(gcDataDir+'Icdesign',gcDataDir+'Icdesign','SH')
*B603211,4 WAB -END

*B603255,1 WAB - Open the name drop file.
*B603255,1 WAB - START
=gfOpenFile(gcDataDir+'IcNamDrp',gcDataDir+'IcNamDrp','SH')
*B603255,1 WAB - END
STORE SPACE(8)  TO lcLastStore
STORE '' TO  lcDShpName,lcDShpAdd1,lcDShpAdd2,lcDShpAdd3,lcDShpAdd4,lcDShpAdd5,;
             lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,;
             lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5,;
             lcDBllName,lcDBllAdd1,lcDBllAdd2,lcDBllAdd3,lcDBllAdd4,lcDBllAdd5
             
STORE .F.  TO llBrowse,llZoom,cbSetAsDef,llMulCurr,llEditExRt,llBulk,llInsur,;
              llNewLine,llContinue,llMultiSt,llQtyPSize,llAddLine,llReorder
*B602896,1 Added new flag for checking existing contracts.
*STORE .T.  TO llReBrowse,llDetails,llUpdBook,llConsAcc
STORE .T.  TO llReBrowse,llDetails,llUpdBook,llConsAcc,llCntrExst
*B602896,1 End.

STORE " "  TO lcOrdLine,lcOrdHdr,lcPackHdr,lcTempline,lcSelLines,lcOrdCanLn,;
              lcStores,lcStores1,lcOrdSS,lcOrdTS,lcShipSS,lcShipTS,lcBrTtlT,;
              lcBooked,lcBookSS,lcBookTS,lcDepleted,lcBulkOrd,lcAlocated,lcODefWare
lnSessNo = gnProgCopy
*C101557,1 Added lnOCtgCd.
STORE 1 TO lnTerms,lnShipVia,lnSpcInst,lnSeason,lnDivision,lnCanReason,lnOCtgCd
*C101557,1 End.
STORE 0 TO lnLastMode
STORE .T. TO llGoAndChk,llChkUnCom
*B602621,1 Do not show voided invoices
lcForKey = '.T.'
*B602621,1 (End)

IF !gfSetup()
  RETURN
ENDIF

*B802859,1 SSH Set filter to the selected account when call from customer screen.
IF TYPE('lcOrderNo') = 'C' .AND. SEEK(lcOrdType+lcOrderNo,'ORDHDR')
  SELECT OrdHdr
  lcOldOrd = ORDER()
  SET ORDER TO Ordacct
  PRIVATE lcTempFlt
  lcTempFlt = OrdHdr.Account
  SET FILTER TO account+cordtype+order = lcTempFlt+lcOrdType
  SET ORDER TO &lcOldOrd
  GOTO TOP
ENDIF
*B802859,1 SSH (END)
lcProgID   = 'SOORD'
llReBrowse = .T.
lcOrdBrow  = IIF(lcOrdType='C','Contract Lines','Order Lines')
lcBrTtlZ   = IIF(lcOrdType='C','Zoom Contract Lines','Zoom Order Lines')
lcBrTtlB   = 'Booked Quantities'
lcBrTtlS   = 'Shipped Quantities'
lcBrTtlP   = 'Production Quantities'
lcBrTtlD   = 'Depleted Quantities'
lcBrTtlK   = 'Bulk order details'

*E301288,1 Reham On 07/26/1999   *** Begin ***
*E301288,1 Define variable hold the work order browse title.
lcBrTtlW  = "Work Order"
*E301288,1 Reham On 07/26/1999   *** End   ***

*E300956,1 Browse order line calceled quantity
lcBrTtlC  = 'Canceled Lines'
lcSize1   = 'Size1'
lcSize2   = 'Size2'
lcSize3   = 'Size3'
lcSize4   = 'Size4'
lcSize5   = 'Size5'
lcSize6   = 'Size6'
lcSize7   = 'Size7'
lcSize8   = 'Size8'

lafolders[1,1] = 'Header'
lafolders[1,2] = 1
lafolders[2,1] = 'Details'
lafolders[2,2] = 2
lafolders[3,1] = 'Summary'
lafolders[3,2] = 3
lnnofld = 3

lcwfoldchng = '=lfActFolder()'  && function to control shows after change the folder
lnfoldercend = 102.00
lnfolderrend = 2
llNoShow = .F.

*E301289,1 WAB - Get Setting Variable ( Display seeling Price & gross Margin )
*E301289,1 WAB - START
llPODsPrc = .F.   				
llMFDsPrc = .F.					
llUpdAlo  = .F.
IF 'PO' $ gcCmpModules     && if module PO is installed
  llPODsPrc = gfGetMemVar('M_PoDspPrc')  && setting in Po Module
ENDIF
IF 'MF' $ gcCmpModules	   && if module MF is installed
  llMFDsPrc = gfGetMemVar('M_MFDspPrc')  
ENDIF
llStyMark  = gfGetMemVar('M_stymark')   ='T'	&& setting in MF Module
*E301289,1 WAB - END

IF !WEXIST(gcBaseWind)

  *E301077,5 Inhance openning files to speed up transaction
  =gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
  SELECT cDesc,cWareCode FROM WAREHOUS INTO ARRAY laWareHouses
  =gfCloseFile('WAREHOUS')
  *E301077,5 (End)

  STORE 0 TO lnLastMode
  lcBaseFild = lcBrFields
  lcBaseTit  = lcFile_Ttl
  lcEmptySty = PADR(STRTRAN(GFITEMMASK("PI"),'X',' '),19)
  lcStyHdr   = gfItemMask("HI")
  lnStyLngth = LEN(lcStyHdr)        && Var. hold the length of the style.

  *C101557,1 and B802324,1 Added a new fields (CORDERCAT) & (GL_SALES).
  
  *C101887,1 KHM 05/26/2000 (Begin) Adding new fields (INT_VEND,EVENT_COD,
  *C101887,1                BILLNO,MERC_TYPE,BLANK_ORD,DISTRB_NO,CCLASS)                            
  *lcScFields = 'ORDER,ACCOUNT,STORE,CUSTPO,STATUS,MULTI,MULTIPO,ENTERED,START,'+;
               'COMPLETE,cTermCode,SHIPVIA,SPCINST,SEASON,cDivision,DISC,DEPT,'+;
               'NOTE1,NOTE2,BUYER,PHONE,CINSUR,BULK,CREORDER,PRIORITY,CFACCODE,'+;
               'REP1,COMM1,REP2,COMM2,CWARECODE,LINK_CODE,CCURRCODE,NEXRATE,BOOK,BOOKAMT,'+;
               'SHIP,SHIPAMT,CANCEL,CANCELAMT,OPEN,OPENAMT,CFROMORDER,'+;
               'CANCELLED,DECL_DATE,DECL_CODE,CCANCRESON,APPROVAL,APPRAMT,'+;
               'NCURRUNIT,Alt_ShpTo,CORDERCAT,GL_SALES'
  lcScFields = 'ORDER,ACCOUNT,STORE,CUSTPO,STATUS,MULTI,MULTIPO,ENTERED,START,'+;
               'COMPLETE,cTermCode,SHIPVIA,SPCINST,SEASON,cDivision,DISC,DEPT,'+;
               'NOTE1,NOTE2,BUYER,PHONE,CINSUR,BULK,CREORDER,PRIORITY,CFACCODE,'+;
               'REP1,COMM1,REP2,COMM2,CWARECODE,LINK_CODE,CCURRCODE,NEXRATE,BOOK,BOOKAMT,'+;
               'SHIP,SHIPAMT,CANCEL,CANCELAMT,OPEN,OPENAMT,CFROMORDER,'+;
               'CANCELLED,DECL_DATE,DECL_CODE,CCANCRESON,APPROVAL,APPRAMT,'+;
               'NCURRUNIT,Alt_ShpTo,CORDERCAT,GL_SALES,INT_VEND,EVENT_COD,'+;
               'BILLNO,MERC_TYPE,BLANK_ORD,DISTRB_NO,CCLASS'

  *C101887,1 KHM 05/26/2000 (End)
  *C101557,1 End.
  
  SELECT OrdHdr
  SCATTER FIELDS &lcScFields TO laData BLANK
  lcSession = gfsequence('CSESSION')

  *E301086,1 Add new Type 'T' for EDI temporary Order 
  *llAlowNew = .T.
  llAlowNew = lcOrdType <> 'T'
  
  laSetups[1,1]  = 'M_PACK'           && System has been steup to use packs
  laSetups[2,1]  = 'M_STY_COM'        && Edit sales reps commissions at style level
  laSetups[3,1]  = 'M_OR_NOTE'        && Edit order lines notepad
  laSetups[4,1]  = 'M_LINK_GL'        && System has been linked to GL
  laSetups[5,1]  = 'M_WareHouse'      && System has been steup to use multiple warehouses
  laSetups[6,1]  = 'M_GenOrNum'       && Generate order number manually
  laSetups[7,1]  = 'M_HOLD_ORD'       && Default new order status to hold
  laSetups[8,1]  = 'M_CANAFTER'       && Number of days need to calculate order completion date
  laSetups[9,1]  = 'M_COMPDATE'       && Number of days added to order entered date to get the
                                      && date after which decrease order quqntity considered 
                                      && as cancellation 
  laSetups[10,1] = 'M_DIV_LINK'       && Link code at division level
  laSetups[11,1] = 'M_REP_COMM'       && Sales rep commision at division level
  laSetups[12,1] = 'M_CRDT_LMT'       && Warn if customer is above credit lemit 
  laSetups[13,1] = 'M_lImpCost'       && Use detailed costing for imported styles
  laSetups[14,1] = 'M_DYELOT'         && System has been setup to use dyelots
  
  =gfGetMemVar(@laSetups,gcAct_Comp)
  llMulCurr   = gfGetMemVar('llMulCurr',gcAct_Comp)
  llEditExRt  = gfGetMemVar('LLEDITEXRA',gcAct_Comp)
  *E301288,1 Reham On 07/05/1999   *** Begin ***
  *E301288,1 Get setup setting for: "Use Variant Cost Sheet" Yes/No.
  llBomVarnt = gfGetMemVar('M_BOMVAR' , gcAct_Comp)
  *E301288,1 Get the lenght of the the style major.
  lnMajorLen = LEN(gfItemMask("PM"))
  *E301288,1 Reham On 07/05/1999   *** End   ***

  lcWinCh1    = gfTempName()
  lcWinCh2    = gfTempName()
  lcWinCh3    = gfTempName()
  lcWinCh30   = gfTempName()
  lcWinCh31   = gfTempName()
  lcWinCh32   = gfTempName()
  lcWinCh321  = gfTempName()
  lcWinCh322  = gfTempName()
  lcWinCh33   = gfTempName()
  lcWinCh34   = gfTempName()
  lcWinCh4    = gfTempName()
  lcfolder    = gfTempName()      && Folder Window Name
  lcfoldprnt  = gcBaseWind        && window parent name for the folder
  lnactfolder = 1                 && active folder

  lafoldwinds[1,1] = lafolders[1,1]
  lafoldwinds[1,2] = lcWinCh2
  lafoldwinds[2,1] = lafolders[2,1]
  lafoldwinds[2,2] = lcWinCh3
  lafoldwinds[3,1] = lafolders[3,1]
  lafoldwinds[3,2] = lcWinCh4

  lcOrdHdr   = gfTempName()
  lcOrdLine  = gfTempName()
  lcTempline = gfTempName()
  lcPackHdr  = gfTempName()
  lcSelLines = gfTempName()
  lcStores   = gfTempName()
  lcStores1  = gfTempName()
  lcOrdSS    = gfTempName()
  lcOrdTS    = gfTempName()
  lcShipSS   = gfTempName()
  lcShipTS   = gfTempName()
  lcDepleted = gfTempName()
  lcBulkOrd  = gfTempName()
  lcBooked   = gfTempName()
  lcBookSS   = gfTempName()
  lcBookTS   = gfTempName()
  lcAlocated = gfTempName()
  
  *E301288,1 Reham On 07/05/1999   *** Begin ***
  *E301288,1 Variable hold the temp. file name that hold the styles' positions.
  *E301288,1 This file structure will be copied from BOMVAR file.
  lcT_BomVar = gfTempName()
  *E301288,1 Reham On 07/05/1999   *** End   ***
  
  *E300956,1 New file to store Order lines canceled quantity
  lcOrdCanLn = gfTempName()
  *E301077,5 Inhance openning files to speed up transaction
  *SELECT STYLE
  *SET RELATION TO 'S'+SCALE INTO SCALE
  *E301077,5 (End)

  SELECT OrdLine
  SCATTER MEMVAR BLANK MEMO
  IF TYPE('lcOrderNo') = 'C' .AND. SEEK(lcOrdType+lcOrderNo,'ORDHDR')
    SELECT ORDHDR
    STORE .F. TO laScrMode
    STORE .T. TO laScrMode[2]
    *E301077,5 Inhance openning files to speed up transaction
    *SCATTER FIELDS &lcScFields TO laData
  ENDIF
  laVariables[1] = 'ldDefOrdDate'
  laVariables[2] = 'lcODefSes'  
  laVariables[3] = 'lcODefDiv'
  laVariables[4] = 'lcODefWare'
  laVariables[5] = 'lcScrMode'
  laVariables[6] = 'lcCurrOrd'
    
  laCodes[1,1] = 'CTERMCODE'
  laCodes[1,2] = 'laTerms'
  laCodes[1,3] = 'lnTerms'
  laCodes[1,4] = ''
  laCodes[1,5] = .F.
  laCodes[1,6] = .F.
  laCodes[1,10] = 'cTermCode'

  laCodes[2,1] = 'SHIPVIA'
  laCodes[2,2] = 'laShipVia'
  laCodes[2,3] = 'lnShipVia'
  laCodes[2,4] = ''
  laCodes[2,5] = .F.
  laCodes[2,6] = .F.
  laCodes[2,10] = 'SHIPVIA'

  laCodes[3,1] = 'SPCINST'
  laCodes[3,2] = 'laSpcInst'
  laCodes[3,3] = 'lnSpcInst'
  laCodes[3,4] = ''
  laCodes[3,5] = .F.
  laCodes[3,6] = .F.
  laCodes[3,10] = 'SPCINST'

  laCodes[4,1] = 'SEASON'
  laCodes[4,2] = 'laSeasons'
  laCodes[4,3] = 'lnSeason'
  laCodes[4,4] = ''
  laCodes[4,5] = .T.
  laCodes[4,6] = .F.
  laCodes[4,10] = 'SEASON'

  laCodes[5,1] = 'CDIVISION'
  laCodes[5,2] = 'laDivision'
  laCodes[5,3] = 'lnDivision'
  laCodes[5,4] = ''
  laCodes[5,5] = .F.
  laCodes[5,6] = .F.
  laCodes[5,10] = 'cDivision'

  *B603678,1 AMM adjust field size
  *laDRltFld[1,1] = 'LINK_CODE'
  laDRltFld[1,1] = PADR('LINK_CODE',10)
  *B603678,1 AMM end
  laDRltFld[1,2] = 'laData[32]'
  laDRltFld[2,1] = 'CSLSGLLINK'
  *B802324,1 Read field insted of variable.
  *laDRltFld[2,2] = 'lcGlSales'
  laDRltFld[2,2] = 'laData[53]' 
  *B802324,1 End.
   
  *E300834,1 Declare discount code related fields array
  laDisRltFld[1,1] = 'DISCPCNT'
  laDisRltFld[1,2] = 'lnDisc_Pcnt'

  laCodes[6,1] = 'CCANCRESON'
  laCodes[6,2] = 'laCanReason'
  laCodes[6,3] = 'lnCanReason'
  laCodes[6,4] = ''
  laCodes[6,5] = .F.
  laCodes[6,6] = .F.
  laCodes[6,10] = 'CCANCRESON'

  *C101557,1 New code array elements.
  laCodes[7,1] = 'CORDERCAT'
  laCodes[7,2] = 'laOCtgCd'
  laCodes[7,3] = 'lnOCtgCd'
  laCodes[7,4] = ''
  laCodes[7,5] = .F.
  laCodes[7,6] = .F.
  laCodes[7,10] = 'CORDERCAT'
  *C101557,1 End.
  
  STORE 'lcOrdType+laData[1]' TO ;
  laCodes[1,9],laCodes[2,9],laCodes[3,9],laCodes[4,9],laCodes[5,9],laCodes[7,9]
  *E301305,1 AMM (start) get complete date per line setting.
  llCDPerL = gfGetmEMvAR('M_CMPDOLN')
  *E301305,1 AMM END
  *E301313,1 AMM Get the setting (Edit price)
  llESTYPSO = gfGetMemVar('M_ESTYPSO') 
  *E301313,1 AMM end
ELSE
  *B802913,1 RAMY [START] in Select mode , the file hasn't opened yet.
  IF !laScrMode[1]
  *B802913,1 RAMY [END]
    *B802404,1 Start.
    SELECT STYLE
    SET RELATION TO 'S'+SCALE INTO SCALE
    *B802404,1 End.
  *B802913,1 RAMY [START] 
  ENDIF
  *B802913,1 RAMY [END]
  SELECT IIF((laScrMode[3] .OR. laScrMode[4]),lcOrdLine,'OrdLine')
  SCATTER MEMVAR BLANK MEMO
ENDIF
*E301077,5 Inhance openning files to speed up transaction
*llSpck_hdr = laSetups[1,2]='Y'  .AND. gfOpenFile(gcDataDir+'SPck_Hdr',gcDataDir+'SPck_Hdr','SH')
*llSpck_Lin = laSetups[1,2]='Y'  .AND. gfOpenFile(gcDataDir+'SPck_Lin',gcDataDir+'SPck_Lin','SH')
**E301086,1 Add new record in the EDI transaction file to be send
*llEdiAcPrt = lcOrdType='O' AND 'EB' $ gcCmpModules .AND. gfOpenFile(gcDataDir+'EDIACPRT',gcDataDir+'ACCFACT','SH')
*llEdiPD    = lcOrdType='O' AND 'EB' $ gcCmpModules .AND. gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PARTTRANS','SH')
*llEdiTrans = lcOrdType='O' AND 'EB' $ gcCmpModules .AND. gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')
*E301086,1 (End)
*IF 'PO' $ gcCmpModules
*  llPosHdr = gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
*  llPosLn  = gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLN','SH')
*  SET RELATION TO CSTYTYPE+PO INTO PosHdr
*ENDIF
*IF 'MF' $ gcCmpModules
*  llCutTktH=gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')
*  llCutTktL=gfOpenFile(gcDataDir+'CUTTKTL',gcDataDir+'CUTTKTL','SH')
*  SET RELATION TO CutTkt INTO CutTktH
*ENDIF
*IF 'MF' $ gcCmpModules .OR. 'PO' $ gcCmpModules
*  llCutPick=gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
*  IF 'PO' $ gcCmpModules
*    SET RELATION TO 'P'+cTktNo+Style INTO PoSLn
*  ENDIF
*  IF 'MF' $ gcCmpModules
*    SET RELATION TO cTktNo+Style INTO CutTktL ADDITIVE
*  ENDIF
*ENDIF
*llInvLine  = 'AR' $ gcCmpModules .AND. gfOpenFile(gcDataDir+'InvLine',gcDataDir+'InvLine','SH')
*llPikLine  = 'AL' $ gcCmpModules .AND. gfOpenFile(gcDataDir+'PIKLINE',gcDataDir+'PIKLINE','SH')
*llSycCurr  = llMulCurr .AND. gfOpenFile(gcSysHome+'SycCurr',gcSysHome+'cCurrCode','SH')
*llPikTkt   = 'AL' $ gcCmpModules .AND. gfOpenFile(gcDataDir+'PIKTKT',gcDataDir+'PIKTKT','SH')
*llRep_Div  = laSetups[11,2]='D' .AND. gfOpenFile(gcDataDir+'REP_DIV',gcDataDir+'REP_DIV','SH')
*llGl_Link = laSetups[4,2]='Y' .AND. gfOpenFile(gcDataDir+'Gl_Link',gcDataDir+'Gl_Link1','SH')
*SELECT cDesc,cWareCode FROM WAREHOUS INTO ARRAY laWareHouses
*E301077,5 (End)

=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

lcKeyBmp   = gcBmpHome + "ExtKey.BMP"
lcNew      = gcBmpHome + "new1.bmp"
lctemplate = gcBmpHome + "Template.BMP"
lcPacks    = gcBmpHome + "packs.bmp"
lcRemove   = gcBmpHome + "rem1.bmp"
lcNote     = gcBmpHome + "Notes1.bmp"
lcClose    = gcBmpHome + "Close2.bmp"
lcCopy     = gcBmpHome + "copy.bmp"
lcPaste    = gcBmpHome + "past.bmp"
lcCancel   = gcBmpHome + "Trash.bmp"
lcUnCancel = gcBmpHome + "unTrash.bmp"
lcCan      = gcBmpHome + "Can.bmp"
lcOk       = gcBmpHome + "Ok.bmp"
*E301253,1 Buttom arrow down to display contracts.
lcBPopBmp  = gcBmpHome + "dn3.bmp"
*E301253,1 End.

*E301176,1 HDM 03/22/1999 Prevent programs from displaying notepad icon
*                           as it's now controlled globally
*DECLARE laPanelObj[2,3]
DECLARE laPanelObj[1,3]
STORE '' TO laPanelObj
*laPanelObj[1,1] = 'pbOrdNote'
*laPanelObj[1,2] = gcBmpHome+'NOTES2.BMP'
*laPanelObj[1,3] = [VALID lfvOrdNote() MESSAGE 'Order Notes' DISABLE]
laPanelObj[1,1] = 'pbObjLink'
laPanelObj[1,2] = gcBmpHome+"Relate.bmp"
laPanelObj[1,3] = [VALID lfvObjLink() MESSAGE 'Object Link' DISABLE]
*E301176,1 HDM 03/22/1999 (End)

SELECT ORDHDR
*B802859,1 SSH Add If Cond. to prevent Change filter if called from customer screen.
IF TYPE('lcOrderNo') <> 'C'
  SET FILTER TO cOrdType+Order=lcOrdType
ENDIF
*B802859,1 SSH (END)
DEFINE BAR 9  OF P03PU03 PROMPT '\<Edit' SKIP FOR !(laScrMode[2] .AND. INLIST(laData[5],'O','H'))
DO (gcScrDir+gcWinAppl+"\SOORD.SPX")
=lfClearKey()
DEFINE BAR 9  OF P03PU03 PROMPT '\<Edit' SKIP FOR .T.
DEFINE BAR 10 OF P03PU03 PROMPT '\<Delete' SKIP FOR .T.

IF WEXIST(lcOrdBrow)
  HIDE WINDOW (lcOrdBrow) SAME
  RELEASE WINDOW (lcOrdBrow)
ENDIF
IF WEXIST(lcBrTtlZ)
  HIDE WINDOW (lcBrTtlZ) SAME
  RELEASE WINDOW (lcBrTtlZ)
ENDIF
IF WEXIST(lcBrTtlT)
  HIDE WINDOW (lcBrTtlT) SAME
  RELEASE WINDOW (lcBrTtlT)
ENDIF
IF glQuitting
  IF USED(lcOrdHdr)
    USE IN (lcOrdHdr)
  ENDIF
  ERASE (gcWorkDir+lcOrdHdr+".DBF")
  ERASE (gcWorkDir+lcOrdHdr+".CDX")
  IF USED(lcOrdLine)
    USE IN (lcOrdLine)
  ENDIF
  ERASE (gcWorkDir+lcOrdLine+".DBF")
  ERASE (gcWorkDir+lcOrdLine+".CDX")
  ERASE (gcWorkDir+lcOrdLine+".FPT")
  IF USED(lcTempline)
    USE IN (lcTempline)
  ENDIF
  ERASE (gcWorkDir+lcTempline+".DBF")
  ERASE (gcWorkDir+lcTempline+".CDX")
  ERASE (gcWorkDir+lcTempline+".FPT")
  IF USED(lcSelLines)
    USE IN (lcSelLines)
  ENDIF
  ERASE (gcWorkDir+lcSelLines+".DBF")
  ERASE (gcWorkDir+lcSelLines+".CDX")
  IF USED(lcStores)
    USE IN (lcStores)
  ENDIF
  ERASE (gcWorkDir+lcStores+".DBF")
  ERASE (gcWorkDir+lcStores+".CDX")
  IF USED(lcOrdSS)
    USE IN (lcOrdSS)
  ENDIF
  ERASE (gcWorkDir+lcOrdSS+".DBF")
  ERASE (gcWorkDir+lcOrdSS+".CDX")
  IF USED(lcOrdTS)
    USE IN (lcOrdTS)
  ENDIF
  ERASE (gcWorkDir+lcOrdTS+".DBF")
  ERASE (gcWorkDir+lcOrdTS+".CDX")
  IF USED(lcShipSS)
    USE IN (lcShipSS)
  ENDIF
  ERASE (gcWorkDir+lcShipSS+".DBF")
  ERASE (gcWorkDir+lcShipSS+".CDX")
  IF USED(lcShipTS)
    USE IN (lcShipTS)
  ENDIF
  ERASE (gcWorkDir+lcShipTS+".DBF")
  ERASE (gcWorkDir+lcShipTS+".CDX")
  IF USED(lcDepleted)
    USE IN (lcDepleted)
  ENDIF
  ERASE (gcWorkDir+lcDepleted+".DBF")
  ERASE (gcWorkDir+lcDepleted+".CDX")
  IF USED(lcBulkOrd)
    USE IN (lcBulkOrd)
  ENDIF
  ERASE (gcWorkDir+lcBulkOrd+".DBF")
  ERASE (gcWorkDir+lcBulkOrd+".CDX")
  IF USED(lcBooked)
    USE IN (lcBooked)
  ENDIF
  ERASE (gcWorkDir+lcBooked+".DBF")
  ERASE (gcWorkDir+lcBooked+".CDX")
  IF USED(lcBookSS)
    USE IN (lcBookSS)
  ENDIF
  ERASE (gcWorkDir+lcBookSS+".DBF")
  ERASE (gcWorkDir+lcBookSS+".CDX")
  IF USED(lcBookTS)
    USE IN (lcBookTS)
  ENDIF
  ERASE (gcWorkDir+lcBookTS+".DBF")
  ERASE (gcWorkDir+lcBookTS+".CDX")
  IF USED(lcAlocated)
    USE IN (lcAlocated)
  ENDIF
  ERASE (gcWorkDir+lcAlocated+".DBF")
  ERASE (gcWorkDir+lcAlocated+".CDX")
  *B602179,1 AMM Erase temporary files when quitting the program
  IF USED(lcOrdCanLn)
    USE IN (lcOrdCanLn)
  ENDIF
  ERASE (gcWorkDir+lcOrdCanLn+".DBF")
  ERASE (gcWorkDir+lcOrdCanLn+".CDX")
  *B602179,1 AMM  end
  
  *E301288,1 Reham On 07/06/1999   *** Begin ***
  IF USED(lcT_BomVar)
    USE IN (lcT_BomVar)
  ENDIF
  ERASE (gcWorkDir + lcT_BomVar + ".DBF")
  ERASE (gcWorkDir + lcT_BomVar + ".CDX")
  ERASE (gcWorkDir + lcT_BomVar + ".FPT")
  *E301288,1 Reham On 07/06/1999   *** End   ***
ENDIF
*E301077,5 Inhance openning files to speed up transaction
*SELECT STYLE
*SET RELATION TO
*E301077,5 (End)

SELECT OrdLine
SET RELATION TO
SELECT ORDHDR
SET RELATION TO
SET FILTER TO
*E301077,5 WAM 02/28/1999 Inhance openning files to speed up transaction
*IF llSpck_hdr
*  USE IN SPck_Hdr
*ENDIF
*IF llSpck_Lin
*  USE IN SPck_Lin
*ENDIF
*E301086,1 Add new record in the EDI transaction file to be send
*IF llEdiAcPrt 
*  USE IN EDIACPRT
*ENDIF
*IF llEdiPD
*  USE IN EDIPD
*ENDIF
*IF llEdiTrans
*  USE IN EDITRANS
*ENDIF
*E301086,1 (End)
*IF llPosHdr
*  USE IN POSHDR
*ENDIF
*IF llPosLn
*  USE IN POSLN
*ENDIF
*IF llCutTktH
*  USE IN CUTTKTH
*ENDIF
*IF llCutTktL
*  USE IN CUTTKTL
*ENDIF
*IF llInvLine
*  USE IN InvLine
*ENDIF
*IF llPikLine
*  USE IN PIKLINE
*ENDIF
*IF llSycCurr
*  USE IN SycCurr
*ENDIF
*IF llPikTkt
*  USE IN PIKTKT
*ENDIF
*IF llRep_Div
*  USE IN REP_DIV
*ENDIF
*IF llGl_Link
*  USE IN Gl_Link
*ENDIF
*IF llCutPick
*  USE IN CUTPICK
*ENDIF
*E301077,5 (End)

RELEASE PAD _INQUIRY OF _MSYSMENU

*!*************************************************************
*! Name      : lfActPad
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : lpvInquiry
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfActPad()
*!*************************************************************
FUNCTION lfActPad

DEFINE PAD _INQUIRY OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
SET SKIP OF PAD _INQUIRY OF _MSYSMENU (laScrMode[1])
ON PAD _INQUIRY OF _msysmenu ACTIVATE POPUP _INQURYPOP

DEFINE POPUP _INQURYPOP MARGIN SHADOW
DEFINE BAR 1  OF _INQURYPOP PROMPT '\<Order Notes'      SKIP FOR laScrMode[1] .OR. laScrMode[4]
DEFINE BAR 2  OF _INQURYPOP PROMPT 'Ob\<ject Link'      SKIP FOR laScrMode[1] .OR. laScrMode[4]
DEFINE BAR 3  OF _INQURYPOP PROMPT 'Customer \<Inquire' SKIP FOR EMPTY(laData[2])
DEFINE BAR 4  OF _INQURYPOP PROMPT 'Customer \<Notepad' SKIP FOR EMPTY(laData[2])
DEFINE BAR 5  OF _INQURYPOP PROMPT '\<Aging Summary'    SKIP FOR EMPTY(laData[2])
DEFINE BAR 6  OF _INQURYPOP PROMPT '\<Style Inquire'    SKIP FOR (lnActFolder<>2) .OR. EMPTY(Style) .OR. (lnSortBy=3 .AND. !llDetails)
DEFINE BAR 7  OF _INQURYPOP PROMPT 'Sort By Lin\<e#'    SKIP FOR (lnActFolder<>2) .OR. !llMultiSt
DEFINE BAR 8  OF _INQURYPOP PROMPT 'Sort BY St\<yle'    SKIP FOR (lnActFolder<>2) .OR. !llMultiSt
DEFINE BAR 9  OF _INQURYPOP PROMPT 'Sort By Sto\<re'    SKIP FOR (lnActFolder<>2) .OR. !llMultiSt 
DEFINE BAR 10 OF _INQURYPOP PROMPT 'Show \<Quantity per Size' SKIP FOR (lnActFolder<>2)
*B802094,1 Disable summary Production, Depletion, Bulk and Cancelled transactions.
*DEFINE BAR 11 OF _INQURYPOP PROMPT 'Show Su\<mmary'    SKIP FOR (lnActFolder<>2) .OR. !llMultiSt .OR. (lnSortBy=1) 
DEFINE BAR 11 OF _INQURYPOP PROMPT 'Show Su\<mmary'     SKIP FOR (lnActFolder<>2) .OR. !llMultiSt .OR. (lnSortBy=1) OR INLIST(lnOrdTrans,4,5,6,7)
*B802094,1 (End)
DEFINE BAR 12 OF _INQURYPOP PROMPT 'Order \<Transactions' SKIP FOR (lnActFolder<>2) .OR. llZoom .OR. lcOrdType<>'O'
DEFINE BAR 13 OF _INQURYPOP PROMPT '\<Line Notepad'       SKIP FOR (lnActFolder<>2) .OR. laSetups[3,2]<>'Y' .OR. EMPTY(m.Style) .OR. !llDetails
DEFINE BAR 14 OF _INQURYPOP PROMPT '\<Copy Line'          SKIP FOR (lnActFolder<>2) .OR. (laScrMode[2]) .OR. EMPTY(m.Style) .OR. llZoom
DEFINE BAR 15 OF _INQURYPOP PROMPT '\<Paste Line'         SKIP FOR (lnActFolder<>2) .OR. (laScrMode[2]) .OR. EMPTY(m.Style) .OR. llZoom
DEFINE BAR 16 OF _INQURYPOP PROMPT '\<Zoom Order Lines'   SKIP FOR (lnActFolder<>2)

SET MARK OF BAR 10 OF _INQURYPOP TO llQtyPSize
SET MARK OF BAR 16 OF _INQURYPOP TO llZoom
SET MARK OF BAR lnSortBy+6 OF _INQURYPOP TO .T.
ON SELECTION POPUP _INQURYPOP DO lpvInquiry
ON BAR 12 OF _INQURYPOP ACTIVATE POPUP _ORDTRANS
*C101772,1 (Begin) Add a bar called 'Payment Schedule' for ARIA.
IF ASCAN(laEvntTrig , PADR('ADDBAR',10)) <> 0
  =gfDoTriger('SOORD',PADR('ADDBAR',10))
ENDIF     
*C101772,1 (End)

*C101887,1 KHM 05/26/2000 (Begin) Adding a new process to add the 'Update
*C101887,1                Edi Fields' for GMA.
IF ASCAN(laEvntTrig , PADR('UPDEDIFL',10)) <> 0
  =gfDoTriger('SOORD',PADR('UPDEDIFL',10))
ENDIF     
*C101887,1 KHM 05/26/2000 (End)

*E300956,1 Add new option to view order line cancelled quantity
DEFINE POPUP _ORDTRANS MARGIN SHADOW
DEFINE BAR 1 OF _ORDTRANS PROMPT '\<Open' 
DEFINE BAR 2 OF _ORDTRANS PROMPT '\<Booked'     SKIP FOR (!laScrMode[2])
DEFINE BAR 3 OF _ORDTRANS PROMPT '\<Shipped'    SKIP FOR (!laScrMode[2]) OR !('AR' $ gcCmpModules)
DEFINE BAR 4 OF _ORDTRANS PROMPT '\<Production' SKIP FOR !(laScrMode[2]  AND llDetails AND ('PO' $ gcCmpModules .OR. 'MF' $ gcCmpModules)) 
DEFINE BAR 5 OF _ORDTRANS PROMPT '\<Depletion'  SKIP FOR !(laScrMode[2]  AND llDetails AND laData[23]='Y')
DEFINE BAR 6 OF _ORDTRANS PROMPT 'Bul\<k order' SKIP FOR !(laScrMode[4]  AND llDetails AND !EMPTY(laData[43]))
DEFINE BAR 7 OF _ORDTRANS PROMPT '\<Canceled'   SKIP FOR !(laScrMode[2]  AND llDetails)
*E301288,1 Reham On 07/26/1999   *** Begin ***
*E301288,1 Add new bar to browse the related records from the CutPick file.
DEFINE BAR 8 OF _ORDTRANS PROMPT '\<Work Order' SKIP FOR !(laScrMode[2]  AND llDetails)
*E301288,1 Reham On 07/26/1999   *** End   ***
SET MARK OF BAR lnOrdTrans OF _ORDTRANS TO .T.
ON SELECTION POPUP _ORDTRANS DO lpvOrdTrans



*!*************************************************************
*! Name      : lpvInquiry
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : gpDoProg,NOTEPAD,gfActWind,lfRefresh,lfBrowDet,lfvLineNote
*!             lfvCopyQty,lfvPasteQty,lfvBrowZoom
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpvInquiry()
*!*************************************************************
FUNCTION lpvInquiry
ACTIVATE WINDOW (lcFolder)
DO CASE
  CASE BAR() = 1      && Order Notes
  	=lfvOrdNote()
  CASE BAR() = 2      && Object Link
    =lfvObjLink()
  CASE BAR() = 3      && Customer Inquire
  	lcParameter = "'"+laData[2]+"'"+IIF(EMPTY(laData[3]),'',",'"+laData[3]+"'")
    DO gpDoProg WITH 'AWRARCUST',.F.,'AR',lcParameter
  CASE BAR() = 4      && Customer Notepad
    =NOTEPAD('A',laData[2])  
  CASE BAR() = 5      && Customer Credit
    =SEEK('M'+ladata[2],'Customer')
    DO (gcScrDir+gcWinAppl+"\SOAGING.SPX")
  CASE BAR() = 6      && Style Inquire
  	lcParameter = "'"+m.Style+"',''"
    DO gpDoProg WITH 'AWRICSTYLE',.F.,'IC',lcParameter
  CASE BAR() = 7      && Sort by line#
    lnSortBy = 1
    llDetails =.T.
    SET MARK OF BAR 7  OF _INQURYPOP TO .T.  
    SET MARK OF BAR 8  OF _INQURYPOP TO .F.
    SET MARK OF BAR 9  OF _INQURYPOP TO .F.
    SET MARK OF BAR 11 OF _INQURYPOP TO .F.
    =lfBrowDet(.T.,IIF(lnOrdTrans=1 .OR. llZoom,'O','B'))
  CASE BAR() = 8      && Sort by Style
    lnSortBy = 2
    SET MARK OF BAR 7 OF _INQURYPOP TO .F.  
    SET MARK OF BAR 8 OF _INQURYPOP TO .T.
    SET MARK OF BAR 9 OF _INQURYPOP TO .F.
    =lfBrowDet(.T.,IIF(lnOrdTrans=1 .OR. llZoom,'O','B'))
  CASE BAR() = 9      && Sort by Store
    lnSortBy = 3
    SET MARK OF BAR 7 OF _INQURYPOP TO .F.
    SET MARK OF BAR 8 OF _INQURYPOP TO .F.
    SET MARK OF BAR 9 OF _INQURYPOP TO .T.
    =lfBrowDet(.T.,IIF(lnOrdTrans=1 .OR. llZoom,'O','B'))
  CASE BAR() = 10      && Show Quantity per Size
    llQtyPSize = !llQtyPSize
    SET MARK OF BAR 10 OF _INQURYPOP TO llQtyPSize
    =lfBrowDet(.T.,IIF(lnOrdTrans=1 .OR. llZoom,'O','B'))
  CASE BAR() = 11      && Show Summary
    llDetails = !llDetails
    =lfActFolder()
    SET MARK OF BAR 11 OF _INQURYPOP TO !llDetails
    =lfBrowDet(.T.,IIF(lnOrdTrans=1 .OR. llZoom,'O','B'))
  CASE BAR() = 13      &&  Order Line Notepad
    =lfvLineNote()
  CASE BAR() = 14      &&  Copy Order Line
   =lfvCopyQty()
  CASE BAR() = 15      &&  Paste Order LIne
   =lfvPasteQty()
  CASE BAR() = 16     &&   Zoom Order Browse
    llZoom = !llZoom
    =lfvBrowZoom()
    SET MARK OF BAR 16 OF _INQURYPOP TO llZoom

  *B803418,1 KHM 08/07/2000 (Begin) Adding this case in order to check
  *B803418,1                the bar of added custom process if any.
  CASE BAR() = 17
    IF ASCAN(laEvntTrig , PADR('CALLSCRE',10)) <> 0
      =gfDoTriger('SOORD',PADR('CALLSCRE',10))
    ENDIF     
    IF ASCAN(laEvntTrig , PADR('UPDEDISC',10)) <> 0
      =gfDoTriger('SOORD',PADR('UPDEDISC',10))
    ENDIF     
  *B803418,1 KHM 08/07/2000 (End)
ENDCASE

*B803418,1 KHM 08/07/2000 (Begin) The following lines are commented out
*B803418,1                and moved to CASE BAR() = 17
*C101772,1 (Begin) Show Payment Schedule sxcreen when the uset select Payment Schedule from Options menu.
*IF ASCAN(laEvntTrig , PADR('CALLSCRE',10)) <> 0
*  =gfDoTriger('SOORD',PADR('CALLSCRE',10))
*ENDIF     
*C101772,1 (End)

*C101887,1 KHM 05/26/2000 (Begin) Calling the custom process for GMA to 
*C101887,1                display the screen to edit the EDI fields.
*IF ASCAN(laEvntTrig , PADR('UPDEDISC',10)) <> 0
*  =gfDoTriger('SOORD',PADR('UPDEDISC',10))
*ENDIF     
*C101887,1 KHM 05/26/2000 (End)
*B803418,1 KHM 08/07/2000  (End)

*!*************************************************************
*! Name      : lfvBrowZoom
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Zoom Invoice lines browse
*!*************************************************************
*! Calls     : lfBrowDet
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvBrowZoom()
*!*************************************************************
FUNCTION lfvBrowZoom

IF llZoom 
  =lfBrowDet(.F.,'O')
ELSE
  llReBrowse = .T.
  =lfActFolder()
ENDIF

*!*************************************************************
*! Name      : lpvOrdTrans
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Order transactions
*!*************************************************************
*! Calls     : lfvOrdTrans
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpvOrdTrans()
*!*************************************************************
FUNCTION lpvOrdTrans

IF BAR() = lnOrdTrans
  RETURN
ENDIF  
IF WEXIST(lcBrTtlT)
  HIDE WINDOW (lcBrTtlT) SAME
  RELEASE WINDOW (lcBrTtlT)
ENDIF
*E301077,5 Inhance openning files to speed up transaction
DO CASE
  CASE lnOrdTrans= 3
    =gfCloseFile('InvLine')
    =gfCloseFile('consinvl')
    *B602621,1 Do not show voided invoices
    =gfCloseFile('INVHDR')
    *B602621,1 (End)
  CASE lnOrdTrans= 4
    IF 'PO' $ gcCmpModules
      =gfCloseFile('POSHDR')
      =gfCloseFile('POSLN')
    ENDIF
    IF 'MF' $ gcCmpModules
      =gfCloseFile('CUTTKTH')
      =gfCloseFile('CUTTKTL')
    ENDIF
    =gfCloseFile('CUTPICK')
  CASE lnOrdTrans= 7
    =gfCloseFile('ORDCANLN')
ENDCASE
*E301077,5 (End)

SET MARK OF BAR lnOrdTrans OF _ORDTRANS TO .F.
lnOrdTrans = BAR()
SET MARK OF BAR lnOrdTrans OF _ORDTRANS TO .T.
SET SKIP OF BAR 6 OF _INQURYPOP (lnOrdTrans =4)
SET SKIP OF BAR 7 OF _INQURYPOP (lnOrdTrans =4)
=lfvOrdTrans()

*!*************************************************************
*! Name      : lfvOrdTrans
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Browse transactions
*!*************************************************************
*! Calls     : lfBrowDet
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvOrdTrans()
*!*************************************************************
FUNCTION lfvOrdTrans

IF lnOrdTrans = 1
  *B602410,1 Refresh style-scale relation
  =lfPrepBrow() 
  
  SHOW WINDOW (lcWinCh32),(lcWinCh321),(lcWinCh322)  TOP
  =lfActFolder()
ELSE
  *E301288,1 Reham On 07/27/1999   *** Begin ***
  *E301288,1 Open the cutpick order file.
  IF lnOrdTrans = 8
    =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
  ENDIF
  *E301288,1 Reham On 07/27/1999   *** End   ***
  *B603558,1 AMM The program terminates because we hide the window has the focus WOUTPUT(), 
  *B603558,1 AMM gfstopread() do that.  so activate the folder window 
  ACTIVATE WINDOW (LCFOLDER)
  *B603558,1 AMM end
  HIDE WINDOW (lcWinCh32),(lcWinCh321),(lcWinCh322)  SAME
  SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY
  SHOW GETS WINDOW (lcWinCh321) DISABLE ONLY
  SHOW GETS WINDOW (lcWinCh322) DISABLE ONLY
  =lfBrowDet(.T.,'T')
ENDIF

*!*************************************************************
*! Name      : lfClearKey
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Clear key
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClearKey()
*!*************************************************************
FUNCTION lfClearKey

ON KEY LABEL ALT+B
ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+HOME
ON KEY LABEL CTRL+END
ON KEY LABEL TAB 
ON KEY LABEL BACKTAB
ON KEY LABEL ALT+Z
ON KEY LABEL ALT+A
ON KEY LABEL ALT+C
*E500295,1 AMM Clear trapping of character keys
FOR lnChrToTrap = 32 TO 126
  ON KEY LABEL (CHR(lnChrToTrap))
ENDFOR
*E500295,1 AMM end
            
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
IF WONTOP() <> lcWinCh1 .AND. WONTOP() <> lcFolder .AND. WONTOP() <> 'GWCCONTRL1' .AND. ;
   laScrMode[4] .AND. EMPTY(laData[2])
  _CUROBJ = OBJNUM(laData[2])
  RETURN
ENDIF

*!*************************************************************
*! Name      : lfBrowDet
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Browse Order lines
*!*************************************************************
*! Calls     : lfPrepBrow,lfwBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfBrowDet()
*!*************************************************************
FUNCTION lfBrowDet
PARAMETERS llRebuild, lcBrow

=IIF(llRebuild,lfPrepBrow(),.T.)
IF WEXIST(lcBrTtlZ)
  HIDE WINDOW (lcBrTtlZ) SAME
  RELEASE WINDOW (lcBrTtlZ)
ENDIF
IF WEXIST(lcBrTtlT)
  HIDE WINDOW (lcBrTtlT) SAME
  RELEASE WINDOW (lcBrTtlT)
ENDIF
lcKey=lcOrdType+laData[1]
IF lcBrow $ 'OB'
  SELECT (lcOrderFile)
  =SEEK(lcOrdType+laData[1])
  lnMarker = RECNO()
  IF WEXIST(lcOrdBrow)
    HIDE WINDOW (lcOrdBrow) SAME
    RELEASE WINDOW (lcOrdBrow)
  ENDIF
  IF llZoom
    HIDE WINDOW (lcWinCh32),(lcWinCh321),(lcWinCh322) SAME
    SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh321) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh322) DISABLE ONLY
    *B802405,1 AMM Add valid function to the browse
    *BROWSE FIELDS ;
           cMarker =IIF(RECNO()=lnMarker,'>',' '):H=' ':R:1:W=.F., &lcOrderBr;
           WINDOW (lcWinCh33)  ;
           IN WINDOW (lcWinCh3);
           NOMENU            ;         
           NOAPPEND          ;
           NODELETE          ;         
           NOWAIT            ;
           SAVE              ;
	       NOCLEAR           ;
	       KEY lcKey ;
           WHEN lfwBrow()    ;
           TITLE lcBrTtlZ 
    BROWSE FIELDS ;
           cMarker =IIF(RECNO()=lnMarker,'>',' '):H=' ':R:1:W=.F., &lcOrderBr;
           WINDOW (lcWinCh33)  ;
           IN WINDOW (lcWinCh3);
           NOMENU             ;         
           NOAPPEND           ;
           NODELETE           ;         
           NOWAIT             ;
           SAVE               ;
	       NOCLEAR            ;
	       KEY lcKey          ;
           WHEN lfwBrow()     ;
           VALID :F lfvBrows();
           TITLE lcBrTtlZ 
    *B802405,1 AMM end
  ELSE
    IF lnOrdTrans = 1
      SHOW WINDOW (lcWinCh32),(lcWinCh321),(lcWinCh322) TOP
    ENDIF
    *B802405,1 AMM Add a valid function to the browse
    *BROWSE FIELDS ;
           cMarker =IIF(RECNO()=lnMarker,'>',' '):H=' ':R:1:W=.F., &lcOrderBr;
           WINDOW (lcWinCh31)  ;
           IN WINDOW (lcWinCh3);
           NOMENU            ;         
           NOAPPEND          ;
           NODELETE          ;         
           NOWAIT            ;
           SAVE              ;
	       NOCLEAR           ;
  	       KEY lcKey ;
           WHEN lfwBrow()    ;
           TITLE lcOrdBrow
    BROWSE FIELDS ;
           cMarker =IIF(RECNO()=lnMarker,'>',' '):H=' ':R:1:W=.F., &lcOrderBr;
           WINDOW (lcWinCh31)  ;
           IN WINDOW (lcWinCh3);
           NOMENU            ;         
           NOAPPEND          ;
           NODELETE          ;         
           NOWAIT            ;
           SAVE              ;
	       NOCLEAR           ;
  	       KEY lcKey ;
           WHEN lfwBrow()    ;
           VALID :F lfvBrows()  ;
           TITLE lcOrdBrow
   *B802405,1 AMM end
  ENDIF
  =lfwBrow()
ENDIF  
IF lcBrow $ 'TB'
  *E301288,1 Reham On 07/27/1999  *** Begin ***
  IF lnOrdTrans = 8
    SELECT (lcTranFile)
    lnTMarker = RECNO(lcTranFile)
    *B802405,1 AMM Add a Valid function to the browse
    *BROWSE FIELDS ;
           cMarker =IIF(RECNO()=lnTMarker,'>',' '):H=' ':R:1:W=.F., &lcTranBr;
           WINDOW (lcWinCh34)  ;
           IN WINDOW (lcWinCh3);
           NOMENU           ;         
           NOAPPEND         ;
           NODELETE         ;         
           NOWAIT           ;
           SAVE             ;
           NOCLEAR          ;
           KEY "1"+&lcOrderFile..Order+STR(&lcOrderFile..LineNo,6) , "3"+&lcOrderFile..Order+STR(&lcOrderFile..LineNo,6) ;
           FOR Order = &lcOrderFile..Order .AND. cOrdLine = STR(&lcOrderFile..LineNo,6) ;
           WHEN lfwBrowT()  ;
           TITLE lcBrTtlT
    BROWSE FIELDS ;
           cMarker =IIF(RECNO()=lnTMarker,'>',' '):H=' ':R:1:W=.F., &lcTranBr;
           WINDOW (lcWinCh34)  ;
           IN WINDOW (lcWinCh3);
           NOMENU           ;         
           NOAPPEND         ;
           NODELETE         ;         
           NOWAIT           ;
           SAVE             ;
           NOCLEAR          ;
           KEY "1"+&lcOrderFile..Order+STR(&lcOrderFile..LineNo,6) , "3"+&lcOrderFile..Order+STR(&lcOrderFile..LineNo,6) ;
           FOR Order = &lcOrderFile..Order .AND. cOrdLine = STR(&lcOrderFile..LineNo,6) ;
           WHEN lfwBrowT()  ;
           VALID :F lfvBrows()  ;
           TITLE lcBrTtlT
    *B802405,1 AMM end
  ELSE
  *E301288,1 Reham On 07/27/1999  *** End   ***
    lcKey    = EVAL(RELATION(1,lcOrderFile))
    SELECT (lcTranFile)
    =SEEK(lcKey)
    lnTMarker = RECNO(lcTranFile)
    *B602621,1 Do not show voided invoices
    *BROWSE FIELDS ;
           cMarker =IIF(RECNO()=lnTMarker,'>',' '):H=' ':R:1:W=.F., &lcTranBr;
           WINDOW (lcWinCh34)  ;
           IN WINDOW (lcWinCh3);
           NOMENU           ;         
           NOAPPEND         ;
           NODELETE         ;         
           NOWAIT           ;
           SAVE             ;
           NOCLEAR          ;
           KEY lcKey        ;
           WHEN lfwBrowT()  ;
           TITLE lcBrTtlT
    *B802405,1 AMM Add a valid function to the browse
    *BROWSE FIELDS ;
           cMarker =IIF(RECNO()=lnTMarker,'>',' '):H=' ':R:1:W=.F., &lcTranBr;
           WINDOW (lcWinCh34)  ;
           IN WINDOW (lcWinCh3);
           NOMENU           ;         
           NOAPPEND         ;
           NODELETE         ;         
           NOWAIT           ;
           SAVE             ;
           NOCLEAR          ;
           KEY lcKey        ;
           FOR EVAL(lcForKey);
           WHEN lfwBrowT()  ;
           TITLE lcBrTtlT
    BROWSE FIELDS ;
           cMarker =IIF(RECNO()=lnTMarker,'>',' '):H=' ':R:1:W=.F., &lcTranBr;
           WINDOW (lcWinCh34)  ;
           IN WINDOW (lcWinCh3);
           NOMENU           ;         
           NOAPPEND         ;
           NODELETE         ;         
           NOWAIT           ;
           SAVE             ;
           NOCLEAR          ;
           KEY lcKey        ;
           FOR EVAL(lcForKey);
           WHEN lfwBrowT()  ;
           VALID :F lfvBrows()  ;
           TITLE lcBrTtlT
    *B802405,1 AMM end
    *B602621,1 (End)
  *E301288,1 Reham On 07/27/1999  *** Begin ***
  ENDIF
  *E301288,1 Reham On 07/27/1999  *** End   ***
ENDIF

*!*************************************************************
*! Name      : lfwBrow
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Show line details
*!*************************************************************
*! Calls     : lfBrowDet
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfwBrow()
*!*************************************************************
FUNCTION lfwBrow

SELECT (lcOrderFile)

*E301288,1 Reham On 07/21/1999   *** Begin ***
*E301288,1 Change the prompt of the configure button.
IF llBomVarnt
  lcConfig = IIF(SEEK("SO" + &lcOrderFile..Order + STR(&lcOrderFile..LineNo,6) , lcT_BomVar) , "\<Adornment" , "\<Configure")
ENDIF
*E301288,1 Reham On 07/21/1999   *** End   ***

lnMarker = RECNO()
IF lnOrdTrans = 1
  IF llZoom
    SHOW WINDOW (lcBrTtlZ) REFRESH SAME
  ELSE  
    SHOW WINDOW (lcOrdBrow) REFRESH SAME
    SELECT (lcOrderFile)
    SCATTER MEMVAR MEMO
    SHOW GETS WINDOW (lcWinCh32)  ONLY
    SHOW GETS WINDOW (lcWinCh321) ONLY
    SHOW GETS WINDOW (lcWinCh322) ONLY
    =lfRefresh(lcWinCh32)
    =lfRefresh(lcWinCh321)
  ENDIF
ELSE
  IF lnOrdTrans= 4 .AND. Style.Make <> llDomestic
    llDomestic = Style.Make
    lcTranBr   = IIF(Style.Make,lcTranBr1,lcTranBr2)
    =lfBrowDet(.F.,'T')
    ACTIVATE WINDOW (lcOrdBrow)
  ENDIF 
  IF llZoom
    SHOW WINDOW (lcBrTtlZ) REFRESH SAME
  ELSE  
    SHOW WINDOW (lcOrdBrow) REFRESH SAME
  ENDIF
ENDIF

*E301288,1 Reham On 07/21/1999   *** Begin ***
*E301288,1 Call the browse from the cutpick file.
IF lnOrdTrans = 8
  =lfBrowDet(.F.,'T')
  ACTIVATE WINDOW (lcOrdBrow)
ENDIF
*E301288,1 Change the prompt of the configure button.
SHOW GET pbConfig,1 PROMPT lcConfig 
*E301288,1 Reham On 07/21/1999   *** End   ***

*!*************************************************************
*! Name      : lfwBrowT
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Show line details
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfwBrowT()
*!*************************************************************
FUNCTION lfwBrowT
lnTMarker = RECNO(lcTranFile)
SHOW WINDOW (lcBrTtlT) REFRESH SAME

*!*************************************************************
*! Name      : lfDOrdScr
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : READ Deactivate function of screen SOORD
*!*************************************************************
*! Calls     : lpTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfDOrdScr()
*!*************************************************************
FUNCTION lfDOrdScr

IF (laScrMode[3] OR laScrMode[4]) AND WLAST()=(lcWinCh321) AND !lfChkTotQty(.F.)
  ACTIVATE WINDOW (lcWinCh321)
  _CUROBJ = OBJNUM(m.Qty1)
  RETURN
ENDIF
SET SKIP OF PAD _INQUIRY OF _MSYSMENU laScrMode[1]
IF WONTOP()=lcOrdBrow .OR. WONTOP()=lcBrTtlB .OR. WONTOP()=lcBrTtlS .OR. ;
   WONTOP()=lcBrTtlD .OR. WONTOP()=lcBrTtlP .OR. WONTOP()=lcBrTtlZ .OR. ;
   WONTOP()=lcBrTtlK OR WONTOP()=lcBrTtlC
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  glFromBrow = .T.
  ON KEY LABEL TAB DO lpTab WITH ;
  IIF(WONTOP()=lcOrdBrow,IIF(lnOrdTrans=1,lcWinCh32,lcWinCh30),IIF(WONTOP()=lcBrTtlZ,'gwcContrl1','gwcContrl1')),;
  IIF(WONTOP()=lcOrdBrow,IIF(lnOrdTrans=1,'m.Store','ibBrowTrn'),IIF(WONTOP()=lcBrTtlZ,'pbTop','pbTop'))

  ON KEY LABEL BACKTAB DO lpBackTab WITH ;
  IIF(WONTOP()=lcOrdBrow .OR. WONTOP()=lcBrTtlZ,lcFolder,lcWinCh30),;
  IIF(WONTOP()=lcOrdBrow .OR. WONTOP()=lcBrTtlZ,'ibFolder[2]','ibBrowOrd')
ELSE
  glFromBrow = .F.
ENDIF
IF (laScrMode[3] .OR. laScrMode[4]) .AND. llNewLine AND ;
   !(WONTOP()=(lcWinCh32) OR WONTOP()=(lcWinCh321) OR WONTOP()=(lcWinCh322))
  SELECT (lcOrdLine)
  SCATTER MEMVAR MEMO
  STORE .F.       TO llAddLine,llNewLine
  STORE lcEmptySty TO lcLastStyle
  STORE SPACE(08)  TO lcLastStore
  IF EMPTY(m.Style)
    SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh321) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh322) DISABLE ONLY
    SHOW GET pbNew   ENABLE
    SHOW GET pbPacks ENABLE
    IF llMultiSt
      SHOW GET pbTemplate ENABLE
    ENDIF
  ELSE
    SHOW GETS WINDOW (lcWinCh32)  ENABLE ONLY
    SHOW GETS WINDOW (lcWinCh321) ENABLE ONLY
    SHOW GETS WINDOW (lcWinCh322) ENABLE ONLY
    IF !llMultiSt
      SHOW GET pbTemplate DISABLE
      SHOW GET ibLStore   DISABLE
      SHOW GET m.Store    DISABLE
    ENDIF
    IF lContract
      SHOW GET m.Price      DISABLE
      SHOW GET m.Gros_Price DISABLE
      SHOW GET m.Disc_Pcnt  DISABLE
    ENDIF
  ENDIF
ENDIF

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
*! Name      : lfBrowMulti
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Browse function for order template screen (SOMULTI)
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfBrowMulti()
*!*************************************************************
FUNCTION lfBrowMulti
PRIVATE lcFields

lcFields = [cMarker =IIF(RECNO()=lnMarker,'>',' '):H=' ':R:1:W=.F.,]+;
           [Flag :H=' ' :R,Style :H=lcStyHdr :R,Style.Desc :20 :H='Description':R,]+;
           [Group :H='Group' :R,Gros_Price :H='Gross Price' :R,Disc_Pcnt :H='Discount' :R,]+;
           [Price :H='Net Price' :R,TotQty :H='Pieces' :R,]+;
           [Amount  =Price*TotQty :P= '9999999.99' :H='Amount']
IF laScrMode[4] .AND. !EMPTY(laData[43])
  lcFields = lcFields + [,&lcBulkOrd..TotBook :H= 'Booked',&lcBulkOrd..TotOpen :6:H= 'Open' ,]+;
                        [&lcBulkOrd..TotUsed :6:H= 'Used',nBalance = MAX(&lcBulkOrd..TotOpen-&lcBulkOrd..TotUsed,0) :6:H= 'Balance']
ENDIF
SELECT (lcTempline)
GO TOP
lnMarker = RECNO()
BROWSE FIELDS &lcFields ;
       WINDOW SOMULTI1  ;
       IN WINDOW SOMULTI;
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
	   NOCLEAR          ;
       WHEN lfLineDet() ;
       TITLE lcBrTtlM 

*!*************************************************************
*! Name      : lfDMulti
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/19996
*! Purpose   : Deactivate function for Multi stores screens
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfDMulti()
*!*************************************************************
FUNCTION lfDMulti

IF (laScrMode[3] OR laScrMode[4]) AND WLAST()='SOMULTI4' AND !lfChkTotQty(.T.)
  ACTIVATE WINDOW 'SOMULTI4'
  _CUROBJ = OBJNUM(m.Qty1)
  RETURN .F.
ENDIF
IF WONTOP()=lcBrTtlM 
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL TAB     DO lpTab WITH 'SOMULTI2','pbSelect'
  ON KEY LABEL BACKTAB DO lpBackTab WITH 'SOMULTI5','pbClose'
ENDIF
IF llNewLine AND  !(WONTOP()='SOMULTI3' OR WONTOP()='SOMULTI4' OR WONTOP()='SOMULTI5')
  SELECT (lcTempLine)
  SCATTER MEMVAR MEMO
  STORE .F.        TO llAddLine,llNewLine
  STORE lcEmptySty TO lcLastStyle
  IF EMPTY(m.Style)
    SHOW GETS WINDOW 'SOMULTI2' DISABLE ONLY
    SHOW GETS WINDOW 'SOMULTI3' DISABLE ONLY
    SHOW GETS WINDOW 'SOMULTI4' DISABLE ONLY
    SHOW GETS WINDOW 'SOMULTI5' DISABLE ONLY
    SHOW GET pbNew     ENABLE
    SHOW GET pbPacks   ENABLE
    SHOW GET pbClose   ENABLE
    SHOW GET ibInv200E ENABLE
  ELSE
    SHOW GETS WINDOW 'SOMULTI2' ENABLE ONLY
    SHOW GETS WINDOW 'SOMULTI3' ENABLE ONLY
    SHOW GETS WINDOW 'SOMULTI4' ENABLE ONLY
    SHOW GETS WINDOW 'SOMULTI5' ENABLE ONLY
    IF lContract
      SHOW GET m.Price      DISABLE
      SHOW GET m.Gros_Price DISABLE
      SHOW GET m.Disc_Pcnt  DISABLE
    ENDIF
    =lfAdjButt('')
  ENDIF
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfLineDet
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Show template line details
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfLineDet()
*!*************************************************************
FUNCTION lfLineDet

lnMarker = RECNO(lcTempLine)
SHOW WINDOW (lcBrTtlM ) REFRESH SAME
SCATTER MEMVAR MEMO
SHOW GETS WINDOW 'SOMULTI2' ONLY
SHOW GETS WINDOW 'SOMULTI3' ONLY
SHOW GETS WINDOW 'SOMULTI4' ONLY
SHOW GETS WINDOW 'SOMULTI5' ONLY
=lfRefresh('SOMULTI2')
=lfRefresh('SOMULTI3')
=lfRefresh('SOMULTI4')
IF Flag = SPACE(1)
  SHOW GET pbSelect,1 PROMPT '\<Select'
ELSE
  SHOW GET pbSelect,1 PROMPT 'Un\<Select'
ENDIF
IF !lContract
  SHOW GET m.Price      ENABLE
  SHOW GET m.Gros_Price ENABLE
  SHOW GET m.Disc_Pcnt  ENABLE
ELSE
  SHOW GET m.Price      DISABLE
  SHOW GET m.Gros_Price DISABLE
  SHOW GET m.Disc_Pcnt  DISABLE
ENDIF
IF !EMPTY(m.Prepak)
  SHOW GET m.PPQty ENABLE
ELSE
  SHOW GET m.PPQty DISABLE
ENDIF

*!*************************************************************
*! Name      : lpShow
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/19996
*! Purpose   : Show function
*!*************************************************************
*! Calls     : lfReStart,gfwCodePop,lfGetInfo
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
lcCanBar = "IIF(laData[5]='X','\<Uncancel','\<Cancel')"
DEFINE BAR 10 OF P03PU03 PROMPT &lcCanBar SKIP FOR !laScrMode[2]
*E301077,5 Inhance openning files to speed up transaction
*B602740,1 MAN 04/07/1999 If the program is called from another program and you
*B602740,1 MAN            try to edit the current record you get ALIAS not Found
*IF llContinue OR (TYPE('lcOrderNo') = 'C' .AND. SEEK(lcOrdType+lcOrderNo,'ORDHDR'))

*B802859,1 SSH 12/14/1999 Seek Only at the first time you call this program.
*B802859,1 SSH            to prevent automatically bounces back to the
*B802859,1 SSH            original Order in case of call this program from 
*B802859,1 SSH            Customer > option >Order.
*IF llContinue OR (TYPE('lcOrderNo') = 'C' .AND. SEEK(lcOrdType+lcOrderNo,'ORDHDR') AND laScrMode[2])
IF llContinue .OR. (TYPE('lcOrderNo') = 'C' .AND. EMPTY(laData[1]) .AND. ;
                  SEEK(lcOrdType+lcOrderNo,'ORDHDR') AND laScrMode[2])
*B802859,1 SSH(END)
  SELECT IIF(laScrMode[2],'ORDHDR',lcOrdHdr)
  SCATTER FIELDS &lcScFields TO laData
ENDIF
*B802187,1 Added to open the codes files in select mode also.
=gfOpenFile(gcDataDir+'CODES',gcDataDir+'Ccode_no','SH')
*B802187,1 End.

IF (laScrMode[2] AND lnLastMode <> 2) OR (laScrMode[4] AND lnLastMode <> 4)
  =gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH') 
  =gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
  =gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH') 
  =gfOpenFile(gcDataDir+'SALESREP',gcDataDir+'SALESREP','SH')
  =gfOpenFile(gcSysHome+'SYCFACT',gcSysHome+'CFACCODE','SH')
  *B802187,1 Commend out opening codes file.
  *=gfOpenFile(gcDataDir+'CODES',gcDataDir+'Ccode_no','SH')
  *B802187,1 End.
  =IIF(laSetups[4,2]='Y',gfOpenFile(gcDataDir+'Gl_Link',gcDataDir+'Gl_Link1','SH'),.T.)
  SELECT STYLE
  SET RELATION TO 'S'+SCALE INTO SCALE
ENDIF
=IIF(llContinue,lfGetInfo(),.T.)
*E301077,5 (End)

DO CASE
  CASE laScrMode[1]  .AND. lnLastMode <> 1
    lnLastMode = 1
    =lfClsFiles()
    =lfReStart()
    SHOW GET pbOrdNote DISABLE
    SHOW GET pbObjLink DISABLE
    SHOW GET pbDlt,1 DISABLE PROMPT lcCancel
    IF llGoAndChk AND lfChkUnComS()
      RETURN
    ENDIF
    *E301288,1 Reham On 07/21/1999   *** Begin ***
    *E301288,1 Change the prompt of the configure button.
    lcConfig = "\<Configure"
    *E301288,1 Add new object in the header screen for back order yes or no.
    lcBackOrd = "No "
    *E301288,1 Reham On 07/21/1999   *** End   ***
  CASE laScrMode[2]
    llGoAndChk = .T.
    lnLastMode = 2
    SHOW GET pbOrdNote ENABLE
    SHOW GET pbObjLink ENABLE
    IF INLIST(laData[5],'C','X')
      SHOW GET pbEdt DISABLE
    ELSE
      SHOW GET pbEdt ENABLE
    ENDIF
    *C101557,1 Added 7th element for a new code.
    STORE 'ORDHDR' TO laCodes[1,7],laCodes[1,8],laCodes[2,7],laCodes[2,8],;
                      laCodes[3,7],laCodes[3,8],laCodes[4,7],laCodes[4,8],;
                      laCodes[5,7],laCodes[5,8],laCodes[7,7],laCodes[7,8]

    *E301288,1 Reham On 07/21/1999   *** Begin ***
    *E301288,1 Add new object in the header screen for back order yes or no.
    lcBackOrd = IIF(laData[5] = "O" .AND. laData[37] <> 0 , "Yes" , "No ")
    *E301288,1 Collect all the adornement records for the current order line.
    IF llBomVarnt
      SELECT * , "S" AS cStatus , RECNO() AS nRecNo ;
        FROM (gcDataDir + "BomVar") ;
       WHERE cIdType+cCost_Id+STR(LineNo,6) = "SO" + laData[1] ;
        INTO DBF (gcWorkDir + lcT_BomVar)
      INDEX ON cIdType+cCost_Id+STR(LineNo,6) TAG (lcT_BomVar)
    ENDIF
    *E301288,1 Reham On 07/21/1999   *** End   ***
    
    =lfClsFiles()
    =lfGetInfo()
    SELECT OrdLine
    SET RELATION TO STYLE+laData[31]+DYELOT INTO STYDYE
    SET RELATION TO STYLE INTO STYLE ADDITIVE
    =SEEK(lcOrdType+laData[1])
    *B802324,1 Read field insted of variable.
    *lcGlSales = SUBSTR(OrdLine.Gl_Sales,1,3)
    laData[53] = IIF(!EMPTY(laData[53]),SUBSTR(laData[53],1,3),'DEF')
    *B802324,1 End.
    SELECT ORDHDR
    SET RELATION TO 'M'+Account INTO CUSTOMER
  CASE laScrMode[3]  .AND. lnLastMode <> 3
    llGoAndChk = .T.
    lnLastMode = 3
    llCUpdate = .T.
    STORE 0 TO laQtyPaste
    lnLines = OrdHdr.LastLine

    *E301077,5 Inhance openning files to speed up transaction
    **E300956,1 Clear order lines cancelled quantity file
    *SELECT (lcOrdCanLn)
    *SCAN
    *  BLANK
    * DELETE
    *ENDSCAN
    *E300956,1 (End)
    *SELECT (lcOrdHdr)
    *DELETE
    *SELECT (lcOrdLine)
    *SCAN
    *  BLANK
    *  DELETE
    *ENDSCAN
    =lfCratTemp()
    *E301077,5 (End)
    
    *E301288,1 Reham On 07/21/1999   *** Begin ***
    *E301288,1 Add new object in the header screen for back order yes or no.
    lcBackOrd = IIF(laData[5] = "O" .AND. laData[37] <> 0 , "Yes" , "No ")
    *E301288,1 Collect all the adornement records for the current order line.
    IF llBomVarnt
      SELECT * , "S" AS cStatus , RECNO() AS nRecNo ;
        FROM (gcDataDir + "BomVar") ;
       WHERE cIdType+cCost_Id+STR(LineNo,6) = "SO" + laData[1] ;
        INTO DBF (gcWorkDir + lcT_BomVar)
      INDEX ON cIdType+cCost_Id+STR(LineNo,6) TAG (lcT_BomVar)
    ENDIF
    *E301288,1 Reham On 07/21/1999   *** End   ***
    
    SELECT ORDHDR
    SCATTER MEMVAR
    INSERT INTO (lcOrdHdr) FROM MEMVAR
    SELECT ORDLINE
    =SEEK(lcOrdType+laData[1])
    SCAN REST WHILE cordtype+order+STR(lineno,6) = lcOrdType+laData[1]
      *C101493,1 AMM Scatter with memo to get the line note
      *SCATTER MEMVAR
      SCATTER MEMVAR MEMO
      *C101493,1 AMM end
      INSERT INTO (lcOrdLine) FROM MEMVAR
    ENDSCAN
    SELECT (lcOrdLine)
    SET RELATION TO STYLE+laData[31]+DYELOT INTO STYDYE
    SET RELATION TO STYLE INTO STYLE ADDITIVE
    GO TOP
    SCATTER MEMVAR MEMO
    lcScrMode = 'E'
    SELECT 'UNCMSESS'
    *B602848,1 MAN 04/28/1999 Change the seek Expr 
    *IF SEEK('I')
    IF SEEK('I'+PADR("SOORD",10)+PADR(gcUser_id,10)) AND RLOCK()
    *B602848,1 MAN end
      BLANK
    ELSE
      APPEND BLANK
    ENDIF
    REPLACE Status     WITH 'O'       ,;
            cUTranType WITH lcProgID  ,;
            cUserId    WITH gcUser_id ,;
            cSession   WITH lcSession ,;
            cProgram   WITH 'SOORD'   ,;
            cCurrScr   WITH 'SOORD'   ,;
            dTranDate  WITH gdSysDate ,;
            cTranTime  WITH TIME()
    =RLOCK()
    lcFiles = 'lcOrdHdr,'+lcOrdHdr+','+lcOrdHdr+';'+;
              'lcOrdLine,'+lcOrdLine+',ORDLINE;'
    *E300956,1 Store order line calceled quantity
    lcFiles = lcFiles + 'lcOrdCanLn,'+lcOrdCanLn+','+lcOrdCanLn+';'
    IF ORDHDR.TOTCUT > 0 
      *E301077,5 Inhance openning files to speed up transaction
      IF !USED(lcAlocated)
        =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
        SELECT CutPick
        =AFIELDS(laFileStru)
        =gfCloseFile('CUTPICK')
        lnFileStru = ALEN(laFileStru,1)
        DIMENSION laFileStru[lnFileStru+2,4]
        laFileStru[lnFileStru+1,1] = 'nSteps'
        laFileStru[lnFileStru+1,2] = 'N'
        laFileStru[lnFileStru+1,3] = 2
        laFileStru[lnFileStru+1,4] = 0
        laFileStru[lnFileStru+2,1] = 'cUpdSizes'
        laFileStru[lnFileStru+2,2] = 'C'
        laFileStru[lnFileStru+2,3] = 8
        laFileStru[lnFileStru+2,4] = 0
        DECLARE laIndex[2,2]
        laIndex[1,1] = 'TRANCD+ORDER+CORDLINE'
        laIndex[1,2] = 'CUTORD'
        *E301182,1 Update CT/PO line number in the CUTPICK file.
        *laIndex[2,1] = 'TRANCD+CTKTNO+ORDER+STYLE+CORDLINE'
        laIndex[2,1] = 'TRANCD+CTKTNO+CTKTLINENO+ORDER+STYLE+CORDLINE'
        *E301182,1 (End)
        laIndex[2,2] = 'CUTPKORD'
        =gfCrtTmp(lcAlocated,@laFileStru,@laIndex)
      ENDIF
      *E301077,5 (End)
      lcFiles = lcFiles + 'lcAlocated,'+lcAlocated+',CUTPKORD;'
    ENDIF
    lcCurrOrd = laData[1]
    =gfSavSess(lcProgID, lcFiles, @laVariables,lcSession)
    *C101557,1 Added 7th element for a new code.
    STORE lcOrdHdr TO laCodes[1,7],laCodes[1,8],laCodes[2,7],laCodes[2,8],;
                      laCodes[3,7],laCodes[3,8],laCodes[4,7],laCodes[4,8],;
                      laCodes[5,7],laCodes[5,8],laCodes[7,7],laCodes[7,8]
    IF lnOrdStatus=1
      DECLARE laOrdStatus[3]
      laOrdStatus[1] = 'Bid      '
      laOrdStatus[2] = 'Open     '
      laOrdStatus[3] = 'Hold     '
    ENDIF  
    llUpdBook = laData[5]='B' .OR. (gdSysDate <= (laData[8] + laSetups[8,2]))
    llReBrowse=.T.
  CASE laScrMode[4] 
    IF lnLastMode <> 4
      llGoAndChk = .T.
      STORE 0 TO laQtyPaste
      IF EMPTY(ldDefOrdDate)
        STORE '' TO lcODefSes,lcODefDiv
        STORE 0  TO lnSeason,lnDivision,lnWareHouse
        IF FILE("ORDDEF.MEM")
          RESTORE ADDITIVE FROM ("ORDDEF.MEM")
          STORE 1 TO lnSeason,lnDivision
          =gfwCodePop(@laCodes,'SEASON','L')
          =gfwCodePop(@laCodes,'CDIVISION','L')
          lnSeason   = ASCAN('laSeasons',lcODefSes)
          lnDivision = ASCAN('laDivision',lcODefDiv)
          lnWareHouse= ASCAN(laWareHouses,lcODefWare)
          lnSeason   = IIF(lnSeason=0,1,ASUBSCRIPT(laSeasons,lnSeason,1))
          lnDivision = IIF(lnDivision=0,1,ASUBSCRIPT(laDivision,lnDivision,1))
          lnWareHouse= IIF(lnWareHouse=0,1,ASUBSCRIPT(laWareHouses,lnWareHouse,1))
        ENDIF
        IF lnSeason = 0
          lnSeason = 1
          =gfwCodePop(@laCodes,'SEASON','D')
        ENDIF
        IF lnDivision = 0
          lnDivision = 1
          =gfwCodePop(@laCodes,'CDIVISION','D')
        ENDIF
        lnWareHouse = MAX(lnWareHouse,1)
        ldDefOrdDate = gdSysDate + laSetups[9,2]
        DO (gcScrDir+gcWinAppl+"\SoOrdDef.SPX")
      ENDIF
      *C101557,1 Added 7th element for a new code.
      STORE lcOrdHdr TO laCodes[1,7],laCodes[1,8],laCodes[2,7],laCodes[2,8],;
                        laCodes[3,7],laCodes[3,8],laCodes[4,7],laCodes[4,8],;
                        laCodes[5,7],laCodes[5,8],laCodes[7,7],laCodes[7,8]
      *E301077,5 Inhance openning files to speed up transaction
      *=IIF(llContinue,.T.,lfReStart())
      =IIF(llContinue,.T.,lfCratTemp() AND lfReStart())
      *E301077,5 (End)
      DECLARE laOrdStatus[3]
      laOrdStatus[1] = 'Bid      '
      laOrdStatus[2] = 'Open     '
      laOrdStatus[3] = 'Hold     '
    ENDIF
    *C101557,1 Default a new code.
    lnOCtgCd = 1
    =gfwCodePop(@laCodes,'CORDERCAT','D')
    *C101557,1 End.
    SHOW GET pbOrdNote DISABLE
    SHOW GET pbObjLink DISABLE
    STORE 4 TO lnLastMode
    SELECT (lcOrdLine)
    SET RELATION TO STYLE+laData[31]+DYELOT INTO STYDYE
    SET RELATION TO STYLE INTO STYLE ADDITIVE
    SCATTER MEMVAR MEMO
    SELECT (lcOrdHdr)
    SHOW GET lnOrdStatus
    IF !llContinue AND EMPTY(laData[2])
      =lfRefresh(lcWinCh2)
      SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY
      SHOW GETS WINDOW (lcWinCh321) DISABLE ONLY
      SHOW GETS WINDOW (lcWinCh322) DISABLE ONLY
      SHOW GETS WINDOW (lcWinCh1)   DISABLE ONLY
      SHOW GET ibAccount ENABLE
      SHOW GET laData[2] ENABLE
      _CUROBJ = OBJNUM(laData[2])
    ENDIF
ENDCASE 
*B602896,1 Reset the contracts flag.
llCntrExst = .T.
*B602896,1 end
*B602681,1 Seek the session record in the uncompeted sessions file after 
*B602681,1 return from other called screen.
=IIF(laScrMode[3] OR laScrMode[4],;
     SEEK('O'+PADR(lcProgID,10)+gcUser_id+lcSession,'UNCMSESS'),.T.)
*B602681,1 (End)
=lfActFolder()

*!*************************************************************
*! Name      : lfActFolder
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Change folders
*!*************************************************************
*! Calls     : lfBrowDet
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  lfActFolder()
*!*************************************************************
FUNCTION lfActFolder
DO CASE
  CASE lnActFolder = 1
    ACTIVATE WINDOW (lcWinCh2)
    lcStatus = IIF(laScrMode[3] .OR. laScrMode[4],'ENABLE','DISABLE')
    SHOW GETS WINDOW (lcWinCh30)  DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh321) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh322) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh4)   DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh2) &lcStatus ONLY
    lcLines = IIF( (laScrMode[3] .OR. laScrMode[4]) .AND. EOF(lcOrdLine),'ENABLE','DISABLE')
    IF llMultiSt
      SHOW GET lnShipAddr DISABLE
    ENDIF
    IF lnShipAddr = 1
      SHOW GET lcShipName DISABLE
      SHOW GET lcShipAdd1 DISABLE
      SHOW GET lcShipAdd2 DISABLE
      SHOW GET lcShipAdd3 DISABLE
      SHOW GET lcShipAdd4 DISABLE
      SHOW GET lcShipAdd5 DISABLE
    ENDIF
    SHOW GET lnSeason    &lcLines
    SHOW GET lnDivision  &lcLines
    SHOW GET lnWareHouse &lcLines
    SHOW GET ibCurrency  &lcLines
    SHOW GET laData[33]  &lcLines
    IF !llEditExRt OR laData[33]=gcBaseCurr
      SHOW GET laData[34] DISABLE
    ELSE
      SHOW GET laData[34] &lcLines
    ENDIF
    IF laScrMode[1]
      SHOW GET ibFolder[2] DISABLE
      SHOW GET ibFolder[3] DISABLE
      SHOW GET ibFolder[4] DISABLE
    ELSE
      SHOW GET ibFolder[2] ENABLE
      SHOW GET ibFolder[3] ENABLE
      SHOW GET ibFolder[4] ENABLE
    ENDIF  
  CASE lnActFolder = 2
    SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh4) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh30) ENABLE ONLY
    SELECT IIF(laScrMode[3] .OR. laScrMode[4],lcOrdLine,'OrdLine')
    =IIF(llReBrowse,lfBrowDet(.T.,IIF(lnOrdTrans=1 .OR. llZoom,'O','B')),.T.)
    SHOW GET ibBrowOrd ENABLE
    SHOW GET ibBrowTrn ENABLE
    llReBrowse = .F.
    IF laScrMode[4] .AND. !EMPTY(laData[43]) .AND. !USED(lcBulkOrd)
      WAIT 'Copy bulk order lines... Please standby' WINDOW NOWAIT
      *E301077,5 Inhance openning files to speed up transaction
      CREATE TABLE (gcWorkDir+lcBulkOrd) (Style C(19),LineNo N(6),;
              Book1 N(5),Book2 N(5),Book3 N(5),Book4 N(5),Book5 N(5),;
              Book6 N(5),Book7 N(5),Book8 N(5),TotBook N(6),;
              Open1 N(5),Open2 N(5),Open3 N(5),Open4 N(5),Open5 N(5),;
              Open6 N(5),Open7 N(5),Open8 N(5),TotOpen N(6),;
              Used1 N(5),Used2 N(5),Used3 N(5),Used4 N(5),Used5 N(5),;
              Used6 N(5),Used7 N(5),Used8 N(5),TotUsed N(6))
      =AFIELDS(laFileStru)
      =gfCrtTmp(lcBulkOrd,@laFileStru,[STR(LINENO,6)],lcBulkOrd)
      *E301077,5 (End)

      lcCopyToFile = IIF(llMultiSt,lcTempline,lcOrdLine)
      *B802652,1 AMM If New order from Bulk order and the user make it multi 
      *B802652,1 AMM store, initialize the booked and open quantity
      IF llMultiSt .AND. EOF(lcOrdLine)
        STORE 0 TO laData[35],laData[36],laData[41],laData[42]
      ENDIF
      *B802652,1 AMM end
      
      SELECT ORDLINE
      SET ORDER TO TAG ORDLINE IN ORDLINE
      IF SEEK(lcOrdType+laData[43])
        SCAN REST WHILE cOrdType+ORDER = lcOrdType+laData[43] FOR TOTQTY > 0
          SCATTER FIELDS EXCEPT INV*,PIK*,POALO*,CUT*,TOTPIK,TOT_POALO,;
                                TOTCUT,NPCK*,nPWght MEMVAR MEMO
          INSERT INTO (lcBulkOrd) ;
          (Style,LineNo,Book1,Book2,Book3,Book4,Book5,Book6,Book7,Book8,TotBook,;
           Open1,Open2,Open3,Open4,Open5,Open6,Open7,Open8,TotOpen) VALUES ;
          (m.Style,m.LineNo,m.Book1,m.Book2,m.Book3,m.Book4,m.Book5,;
           m.Book6,m.Book7,m.Book8,m.TotBook,m.Qty1,m.Qty2,m.Qty3,m.Qty4,;
           m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty)
          m.cFromOrder = m.Order
          m.BulkLineNo = m.LineNo
          m.Order      = SPACE(6)
          IF llMultiSt
            STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,;
                       m.Qty8,m.TotQty,m.Book1,m.Book2,m.Book3,m.Book4,;
                       m.Book5,m.Book6,m.Book7,m.Book8,m.TotBook
          ELSE
            m.Book1   = m.Qty1
            m.Book2   = m.Qty2
            m.Book3   = m.Qty3
            m.Book4   = m.Qty4
            m.Book5   = m.Qty5
            m.Book6   = m.Qty6
            m.Book7   = m.Qty7
            m.Book8   = m.Qty8
            m.TotBook = m.TotQty
            m.Flag    = 'N'
            m.Store   = laData[3]
            SELECT (lcBulkOrd)
            =RLOCK()
            REPLACE USED1 WITH m.Qty1 ,;
                    USED2 WITH m.Qty2 ,;
                    USED3 WITH m.Qty3 ,;
                    USED4 WITH m.Qty4 ,;
                    USED5 WITH m.Qty5 ,;
                    USED6 WITH m.Qty6 ,;
                    USED7 WITH m.Qty7 ,;
                    USED8 WITH m.Qty8 ,;
                    TOTUSED WITH m.TotQty
            UNLOCK
          ENDIF
          INSERT INTO (lcCopyToFile) FROM MEMVAR
        ENDSCAN
      ENDIF
      GO TOP IN (lcCopyToFile)
      WAIT CLEAR 
    ENDIF
  
    IF !llZoom
      SCATTER MEMVAR
      SHOW WINDOW (lcWinCh32),(lcWinCh321),(lcWinCh322) REFRESH SAME
      lcStatus = IIF((laScrMode[3] OR laScrMode[4]) AND !EOF(lcOrdLine) AND;
                      llDetails,'ENABLE','DISABLE')
      SHOW GETS WINDOW (lcWinCh32)  &lcStatus ONLY
      SHOW GETS WINDOW (lcWinCh321) &lcStatus ONLY
      SHOW GETS WINDOW (lcWinCh322) &lcStatus ONLY
      IF (laScrMode[3] .OR. laScrMode[4]) AND llDetails
        SHOW GET pbNew   ENABLE 
        SHOW GET pbPacks ENABLE
      ELSE
        SHOW GET pbNew   DISABLE
        SHOW GET pbPacks DISABLE
      ENDIF
      IF (laScrMode[3] .OR. laScrMode[4]) .AND. llMultiSt AND llDetails
        SHOW GET pbTemplate ENABLE 
      ELSE
        SHOW GET pbTemplate DISABLE
      ENDIF
      IF (laScrMode[3] .OR. laScrMode[4]) .AND. llMultiSt .AND. ;
         !EOF(lcOrdLine) AND llDetails
        SHOW GET ibLStore ENABLE
        SHOW GET m.Store  ENABLE
      ELSE
        SHOW GET ibLStore DISABLE
        SHOW GET m.Store  DISABLE
      ENDIF
      IF (laScrMode[3] .OR. laScrMode[4]) .AND. !EMPTY(Style) AND llDetails
        SHOW GET pbRemove ENABLE
      ELSE
        SHOW GET pbRemove DISABLE
      ENDIF
    ENDIF
    *E301313,1 AMM Disable the price objects due to the system setting
    IF (laScrMode[3] .OR. laScrMode[4]) .AND. !llESTYPSO
      SHOW GET m.Gros_Price DISABLE
      SHOW GET m.Disc_Pcnt DISABLE
      SHOW GET m.Price DISABLE
    ENDIF
    *E301313,1 AMM end
    
    SELECT IIF(laScrMode[3] .OR. laScrMode[4],lcOrdLine,'OrdLine')
    =lfwBrow()
    _CUROBJ = IIF(!EMPTY(STYLE) .OR. llZoom,OBJNUM(ibBrowOrd),OBJNUM(pbNew))
  CASE lnActFolder = 3
    lcStatus = IIF(laScrMode[3] .OR. laScrMode[4],'ENABLE','DISABLE')
    SHOW GETS WINDOW (lcWinCh2)   DISABLE  ONLY 
    SHOW GETS WINDOW (lcWinCh30)  DISABLE  ONLY
    SHOW GETS WINDOW (lcWinCh32)  DISABLE  ONLY
    SHOW GETS WINDOW (lcWinCh321) DISABLE  ONLY
    SHOW GETS WINDOW (lcWinCh322) DISABLE  ONLY
    SHOW GETS WINDOW (lcWinCh4)  &lcStatus ONLY
    IF llMultiSt .OR. lcOrdType='C'
      SHOW GET llBulk DISABLE
    ENDIF
    *E301452,1 ABD Read the GL link code of the factor while creating the sales order. 
    *E301452,1 ABD If there is a factor assgined to the current customer.
    *E301452,1 ABD we should read the GL link code of this factor insated of the 
    *E301452,1 ABD gl link code of this customer.  
    *E301452,1 ABD If the factor link code is empty we should not change the current sitation 
    *E301452,1 ABD Enable and disable filed GlLink Code if We have a factor. [Begin]
    lnSavAlias = SELECT (0)
    SELECT IIF(laScrMode[3] .OR. laScrMode[4],lcOrdhdr,'ORDHDR')
    IF !EMPTY(cfacCode)
      SHOW GET ibGlLink   DISABLE
      SHOW GET laData[32] DISABLE
    ELSE
      *AMH 09/06/2000 ENABLE ibGlLink in View or Select Mode [Start]
      *SHOW GET ibGlLink   ENABLE
      *SHOW GET laData[32] ENABLE
      IF laScrMode[3] .OR. laScrMode[4]
        SHOW GET ibGlLink   ENABLE
        SHOW GET laData[32] ENABLE
      ENDIF
      *AMH 09/06/2000 ENABLE ibGlLink in View or Select Mode [End  ]
    ENDIF
    SELECT(lnSavAlias)
    *E301452,1 ABD [End]
    =lfRefresh(lcWinCh4)
ENDCASE
IF (laScrMode[4] AND !EMPTY(laData[2])).OR. (laScrMode[3] .AND. ORDHDR.STATUS='B')
  SHOW GET lnOrdStatus ENABLE
ELSE
  SHOW GET lnOrdStatus DISABLE
ENDIF  
IF (laScrMode[3] .OR. laScrMode[4]) .AND. EOF(lcOrdLine) AND !EMPTY(laData[2])
  SHOW GET llMultiSt ENABLE
ELSE
  SHOW GET llMultiSt DISABLE
ENDIF
IF (laScrMode[3] .OR. laScrMode[4]) .AND. !llMultiSt AND !EMPTY(laData[2])
  SHOW GET ibStore   ENABLE
  SHOW GET laData[3] ENABLE
ELSE
  SHOW GET ibStore   DISABLE
  SHOW GET laData[3] DISABLE
ENDIF
IF (laScrMode[3] .OR. laScrMode[4]) .AND. EOF(lcOrdLine) .AND. llMultiSt AND !EMPTY(laData[2])
  SHOW GET laData[7] ENABLE
ELSE
  SHOW GET laData[7] DISABLE
ENDIF
IF laScrMode[1] OR ((laScrMode[3] .OR. laScrMode[4]) .AND. !laData[7] AND !EMPTY(laData[2]))
  SHOW GET laData[4] ENABLE
ELSE
  SHOW GET laData[4] DISABLE
ENDIF
RETURN

*!*************************************************************
*! Name      : lfvDefaults
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate session defaults
*!*************************************************************
*! Calls     : CODECHK,gfBrowWare
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDefaults()
*!*************************************************************
FUNCTION lfvDefaults

lcODefSes  = ALLTRIM(laSeasons[lnSeason,2])
lcODefDiv  = ALLTRIM(laDivision[lnDivision,2])
lcODefWare = laWareHouses[lnWareHouse,2]
IF EMPTY(ldDefOrdDate)
  *E300420,1 Message : 32022
  *E300420,1 Order copletion date cannot be empty.
  *E300420,1 Button : 00000
  *E300420,1 Ok
  =gfModalGen('TRM32022B00000','ALERT',IIF(lcOrdType='C','Contract','Order'))
  _CUROBJ = OBJNUM(ldDefOrdDate)
  RETURN
ENDIF
IF gdSysDate > ldDefOrdDate
  *E300420,1 Message : 32013
  *E300420,1 The starting date cannot be greater than the completion date.
  *E300420,1 Button : 00000
  *E300420,1 Ok
  =gfModalGen('TRM32013B00000','ALERT')
  ldDefOrdDate = gdSysDate + laSetups[9,2]
  _CUROBJ = OBJNUM(ldDefOrdDate)
  RETURN
ENDIF
*B602713,1 ABD The message "Completion date is a Sunday" 
*B602713,1 ABD Should not appear for Egypt. [ Begin ]
IF  gcContCode # 'EGYPT'
  *B602713,1 ABD [ End]
  IF CDOW(ldDefOrdDate)='Saturday' .OR. CDOW(ldDefOrdDate)='Sunday'
    *E300420,1 Message : 32014
    *E300420,1 The completion date is a xxxxx.
    *E300420,1 Button : 00000
    *E300420,1 Ok
    =gfModalGen('INM32014B00000','ALERT',CDOW(ldDefOrdDate))
  ENDIF
  *B602713,1 ABD  End for if statement. [ Begin ]  
ENDIF
*B602713,1 ABD [ End]

IF cbSetAsDef
  SAVE ALL LIKE lcODef* TO ("ORDDEF.MEM")
ENDIF
CLEAR READ

*!*************************************************************
*! Name      : lpClsScr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Cancel new Order
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpClsScr()
*!*************************************************************
FUNCTION lpClsScr

SELECT unCmSess
REPLACE STATUS WITH 'I'
UNLOCK
llContinue = .F.
llReBrowse =.T.
SELECT ORDHDR
IF laScrMode[3]
  SCATTER FIELDS &lcScFields TO laData
ENDIF
*C101772,1 (Begin) Zap temp file when quiting without saving.
IF ASCAN(laEvntTrig , PADR('ZAPTMP',10)) <> 0
  =gfDoTriger('SOORD',PADR('ZAPTMP',10))
ENDIF     
*C101772,1 (End)


*!*************************************************************
*! Name      : lfGetInfo
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Restore invoice information
*!*************************************************************
*! Calls     : gfGetAdr
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfGetInfo()
*!*************************************************************
FUNCTION lfGetInfo

*E301175,1 I have removed the code of the function lfGetInfo to a new
*E301175,1 program called SOUPDATE with the name lfGet_Info to add the
*E301175,1 capability to call the sales order Save and Delete functions
*E301175,1 from the EDI [Begin]
DO lfGet_Info IN (gcAppHome + 'SO\SOUPDATE.FXP')
*E301175,1 I have removed the code of the functions lfGetInfo [End]

*!*************************************************************
*! Name      : lfReStart
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/19996
*! Purpose   : ReInitialize variables
*!*************************************************************
*! Calls     : gfwCodePop,lfchngfolder
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfReStart()
*!*************************************************************
FUNCTION lfReStart

lcorderfile = 'ORDLINE'
=gfwCodePop(@laCodes,'CTERMCODE','N')
=gfwCodePop(@laCodes,'SHIPVIA','N')
*B802366,1 AMM
IF llMultiSt
  =lfChMSHV()
ENDIF
*B802366,1 AMM end

=gfwCodePop(@laCodes,'SPCINST','N')
=gfwCodePop(@laCodes,'SEASON','N')
=gfwCodePop(@laCodes,'CDIVISION','N')
*C101557,1 Added new code.
=gfwCodePop(@laCodes,'CORDERCAT','N')
*C101557,1 End.
    
STORE 0 TO lnLines,lnTLines,lnSouComm1,lnSouComm2
STORE 1 TO lnOrdStatus,lnShipAddr
STORE lcEmptySty TO lcLastStyle
STORE SPACE(08)  TO lcLastStore
STORE '' TO  lcDShpName,lcDShpAdd1,lcDShpAdd2,lcDShpAdd3,lcDShpAdd4,lcDShpAdd5,;
             lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,;
             lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5,;
             lcDBllName,lcDBllAdd1,lcDBllAdd2,lcDBllAdd3,lcDBllAdd4,lcDBllAdd5
STORE '' TO lcScrMode,lcFiles,lcCurrOrd
*B802663,1 AMM add initialization of llReorder
STORE .F. TO llZoom,llMultiSt,llAddLine,llNewLine,llIgnorAll,llDomestic,llBulk,llReorder

STORE .T. TO llReBrowse,llUpdBook
lcSize1   = 'Size1'
lcSize2   = 'Size2'
lcSize3   = 'Size3'
lcSize4   = 'Size4'
lcSize5   = 'Size5'
lcSize6   = 'Size6'
lcSize7   = 'Size7'
lcSize8   = 'Size8'

DECLARE laOrdStatus[5]
laOrdStatus[1] = 'Bid      '
laOrdStatus[2] = 'Open     '
laOrdStatus[3] = 'Hold     '
laOrdStatus[4] = 'Cancelled'
laOrdStatus[5] = 'Complete '
IF lnactfolder <> 1
  lnlastfold = lnactfolder
  lnactfolder=1
  =lfchngfolder(lnactfolder)
ENDIF

*!*************************************************************
*! Name      : lfClsFiles
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/19996
*! Purpose   : Close opened temporary files
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfClsFiles()
*!*************************************************************
FUNCTION lfClsFiles

IF USED(lcBooked)
  USE IN (lcBooked)
ENDIF
IF USED(lcDepleted)
  USE IN (lcDepleted)
ENDIF
IF USED(lcOrdSS)
  USE IN (lcOrdSS)
ENDIF
IF USED(lcBookSS)
  USE IN (lcBookSS)
ENDIF
IF USED(lcShipSS)
  USE IN (lcShipSS)
ENDIF
IF USED(lcOrdTS)
  USE IN (lcOrdTS)
ENDIF
IF USED(lcBookTS)
  USE IN (lcBookTS)
ENDIF
IF USED(lcShipTS)
  USE IN (lcShipTS)
ENDIF
IF USED(lcAlocated)
  USE IN (lcAlocated)
ENDIF
IF USED(lcBulkOrd)
  USE IN (lcBulkOrd)
ENDIF
SELECT ORDHDR

*!*************************************************************
*! Name      : lfvOrdStatus
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Order status
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvOrdStatus()
*!*************************************************************
FUNCTION lfvOrdStatus

STORE SUBSTR('BOH',lnOrdStatus,1) TO laData[5]
SELECT (lcOrdHdr)
=RLOCK()
REPLACE STATUS WITH laData[5]
*C200115,1 (Begin) Add a bar called 'Payment Schedule' for ARIA.
IF ASCAN(laEvntTrig , PADR('STATCHG',10)) <> 0
  =gfDoTriger('SOORD',PADR('STATCHG',10))
ENDIF     
*C200115,1 (End)
UNLOCK
llCUpDate = .T.

*!*************************************************************
*! Name      : lfvOrder
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Order number
*!*************************************************************
*! Calls     : lfOrdBrow,gfSeekRec
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvOrder()
*!*************************************************************
FUNCTION lfvOrder
PRIVATE lcOrder

*B603933,1 (Begin) Free the file from filter for seeking and browsing.
PRIVATE lnAlias,lcOldOrdFlt
lnAlias = SELECT()
SELECT ORDHDR
lcOldOrdFlt = FILTER()
SET FILTER TO
*B603933,1 (End)

IF llBrowse .OR. (!EMPTY(laData[1]) .AND. !SEEK(lcOrdType+laData[1],'ORDHDR'))
  lcOrder = laData[1]
  =lfOrdBrow(@lcOrder,laData[2],laData[3],laData[4])
  laData[1] = lcOrder 
  llBrowse = .F.
ENDIF

IF !EMPTY(laData[1])
  SELECT ORDHDR
  *B802859,1 SSH Add If Cond. to prevent Change filter if called from customer screen.
  IF TYPE('lcOrderNo') <> 'C'
    *B802891,1 ABD- Save filter in the record [Begin]
    lcSveFilt = FILTER()
    *B802891,1 ABD [End]
    SET FILTER TO
  ENDIF
  *B802859,1 SSH (End)
  =gfSeekRec()
  SELECT ORDHDR
  *B802859,1 SSH Add If Cond. to prevent Change filter if called from customer screen.
  IF TYPE('lcOrderNo') <> 'C'
    *B802891,1 ABD- Set filter again. [Begin]
    *SET FILTER TO cOrdType+Order=lcOrdType
    SET FILTER TO &lcSveFilt
    *B802891,1 ABD- [End]
  ENDIF
  *B802859,1 SSH(End)
ENDIF  
*B603933,1 (Begin) Restore filter.
SELECT ORDHDR
SET FILTER TO &lcOldOrdFlt
SELECT (lnAlias)
*B603933,1 (End)

*!*************************************************************
*! Name      : lfOrdBrow
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Invoice order
*!*************************************************************
*! Calls     : ARIABROW
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfOrdBrow()
*!*************************************************************
FUNCTION lfOrdBrow
PARAMETERS lcOrder,lcAccount,lcStore,lcCustPo

SELECT ORDHDR
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER
*--HDM B802148,1 [Start] Re-arrange order browse
*lcBrFields = [Order:H="Order#",status:1:H="S",lcSesDesc=gfCodDes(Season,'SEASON'):H="Season",lcDivDesc=gfCodDes(cDivision,'CDIVISION'):H="Division",]+;
             [CustPo=IIF(multipo,'*Multi_PO*',custpo):H="Cust. P.O#",]+;
             [ACCOUNT:H="Acct",store=IIF(MULTI='Y','*Multi*',STORE):H="Store",Customer.stname]+;
             [:15:H="Name",Open:H="Open.Qty.",OpenAmt:H="Open.Amt.",Ship:H="Ship.Qty.",Shipamt:H="Ship.Amt.",]+;
             [start:H="Start",Complete:H="Complete",]+;
             [Note1:6:H="Notes"]
*E301399,1 AMM add department# in EDI temp orders
*lcBrFields = [Order:H="Order#",]+;
             [status:1:H="S",]+;
             [ACCOUNT:H="Acct",]+;
             [store=IIF(MULTI='Y','*Multi*',STORE):H="Store",]+;
             [Customer.stname:30:H="Name",]+;
             [CustPo=IIF(multipo,'*Multi_PO*',custpo):H="Cust. P.O#",]+;
             [Open:H="Open.Qty.",]+;
             [OpenAmt:H="Open.Amt.",]+;
             [Ship:H="Ship.Qty.",]+;
             [Shipamt:H="Ship.Amt.",]+;
             [start:H="Start",]+;
             [Complete:H="Complete",]+;
             [lcSesDesc=gfCodDes(Season,'SEASON'):H="Season",]+;
             [lcDivDesc=gfCodDes(cDivision,'CDIVISION'):H="Division",]+;
             [Note1:6:H="Notes"]
lcBrFields = [Order:H="Order#",]+;
             [status:1:H="S",]+;
             [ACCOUNT:H="Acct",]+;
             [store=IIF(MULTI='Y','*Multi*',STORE):H="Store",]+;
             [Customer.stname:30:H="Name",]+;
             IIF(lcOrdType='T',[Dept:5:H="Dept.",],'')+;
             [CustPo=IIF(multipo,'*Multi_PO*',custpo):H="Cust. P.O#",]+;
             [Open:H="Open.Qty.",]+;
             [OpenAmt:H="Open.Amt.",]+;
             [Ship:H="Ship.Qty.",]+;
             [Shipamt:H="Ship.Amt.",]+;
             [start:H="Start",]+;
             [Complete:H="Complete",]+;
             [lcSesDesc=gfCodDes(Season,'SEASON'):H="Season",]+;
             [lcDivDesc=gfCodDes(cDivision,'CDIVISION'):H="Division",]+;
             [Note1:6:H="Notes"]
*E301399,1 AMM end
*--HDM B802148,1 [End]
DO CASE
  CASE !EMPTY(lcOrder) .AND. SEEK(lcOrdType+lcOrder,'OrdHdr')
    RETURN
  CASE !EMPTY(lcAccount) .AND. EMPTY(lcCustPo)
    SET ORDER TO TAG 'OrdAcct' IN OrdHdr
    lcOrder = IIF(ARIABROW("lcAccount+lcOrdType"+' FOR STORE = IIF(EMPTY(lcStore),"",lcStore)',;
                  "Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),;
                  OrdHdr.Order,SPACE(6))
    SET ORDER TO TAG ORDHDR IN OrdHdr
  *B603478,1 ABD Change the Case to Be Only If Store Is Empty. [ Begin ] 
  *CASE !EMPTY(lcAccount) .AND. !EMPTY(lcCustPo)
  CASE !EMPTY(lcAccount) .AND. !EMPTY(lcCustPo) .AND. EMPTY(lcStore)
    *B603478,1 ABD [ End]
    SET ORDER TO TAG 'OrdCust' IN OrdHdr
    *B603478,1 ABD- Fix bug that don't browse orders have the same account and Custpo 
    *B603478,1 ABD- when entering account and Cust. PO. That need to made Function
    *B603478,1 ABD- that cheak if have more recored with the the same account and Custpo
    *B603478,1 ABD- and Modify the filter Expression To Inclued the account And CustPo.[Begin]
    *lcOrder = IIF(SEEK(lcAccount+UPPER(lcCustPo)+lcOrdType,'OrdHdr') OR ;
    *              ARIABROW("lcAccount"+' FOR cOrdType=lcOrdType .AND. STORE = IIF(EMPTY(lcStore),"",lcStore)',;
    *              "Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),;
    *               OrdHdr.Order,SPACE(6))
    PRIVATE lcCstPoExp
    lcCstPoExp = IIF(SEEK(lcAccount+UPPER(lcCustPo)+lcOrdType,'OrdHdr'),;
                     'Account + UPPER(CustPO) = lcAccount+UPPER(lcCustPo)','.T.')
    lcOrder = IIF((SEEK(lcAccount+UPPER(lcCustPo)+lcOrdType,'OrdHdr') AND lfGetNxtPo(OrdHdr.Order)) .OR. ;
              ARIABROW("lcAccount"+' FOR cOrdType=lcOrdType .AND. STORE = IIF(EMPTY(lcStore),"",lcStore);
              .AND. &lcCstPoExp',;
              "Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),;
              OrdHdr.Order,SPACE(6))
    *B603478,1 ABD [ End ]
    SET ORDER TO TAG ORDHDR IN OrdHdr
  *B603478,1 ABD- In Case of account ,custpo and Store Not Empty Cheak if we have this selection
  *B603478,1 ABD- In one order get him more than one order Browse them else if we haven't any 
  *B603478,1 ABD- order math this selection give user message 'No orders matching the selected.'[ Begin ]
  CASE !EMPTY(lcAccount) .AND. !EMPTY(lcCustPo) .AND. !EMPTY(lcStore)
    SET ORDER TO TAG 'OrdCust' IN OrdHdr
    IF SEEK(lcAccount+UPPER(lcCustPo)+lcOrdType,'OrdHdr') .AND. Store = lcStore

      lcorder =IIF(lfGetNxtPo(OrdHdr.Order) .OR. ARIABROW("lcAccount"+' FOR cOrdType=lcOrdType .AND. STORE = lcStore .AND.Account + UPPER(CustPO) = lcAccount+UPPER(lcCustPo)',;
                   "Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),;
                    OrdHdr.Order,SPACE(6))
    ELSE
      *B603478,1 Message : 30238
      *B603478,1 No orders matching the selected. 
      *B603478,1 Button  : 00000
      *B603478,1 Ok
      = gfModalGen('TRM32038B00000','DIALOG','orders matching the')
    ENDIF  
    SET ORDER TO TAG ORDHDR IN OrdHdr
    *B603478,1 ABD [ End ]      
  OTHERWISE
    SET ORDER TO TAG ORDHDR IN OrdHdr
    *B803681,1 SSH 22/09/00  Enhancement performance.
    PRIVATE lcOldFilt
    lcOldFilt = FILTER()
    SET FILTER TO
    *B803681,1 SSH 22/09/00  Enhancement performance.
    lcOrder = IIF(ARIABROW('lcOrdType',"Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),;
                  OrdHdr.Order,SPACE(6))
    *B803681,1 SSH 22/09/00  Enhancement performance.
    SET FILTER TO &lcOldFilt
    *B803681,1 SSH 22/09/00  Enhancement performance.
ENDCASE
SET RELATION OFF INTO CUSTOMER
*!*************************************************************
*! Name      : lfGetNxtPo
*! Developer : Abdou ElGendi
*! Date      : 02/29/2000
*! Purpose   : Cheak IF The Account Have anthor Order With Same 
*!           : CustPo Number.
*! Reference : B603478,1
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : lcPreOrd
*!*************************************************************
*! Returns            : True .OR. False
*!*************************************************************
*B603478,1
FUNCTION lfGetNxtPo
PARAMETER lcPreOrd
PRIVATE llToRet

LOCATE REST WHILE account+UPPER(custpo)+cordtype+order =;
                    lcAccount+UPPER(lcCustPo)+lcOrdType ;
              FOR ORDER <> lcPreOrd .AND. STORE = IIF(EMPTY(lcStore),"",lcStore) 
IF FOUND()
  llToRet = .F.
ELSE
  llToRet = .T.
  =SEEK(lcAccount+UPPER(lcCustPo)+lcOrdType)
ENDIF
RETURN llToRet
*B603478,1(End)
*-- End OF lfGetNxtPo.
*!*************************************************************
*! Name      : lfvDiscount
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate order discount
*!*************************************************************
*! Calls     : lfvCommision
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDiscount()
*!*************************************************************
FUNCTION lfvDiscount
PARAMETERS llPercent

IF &lcOrdHdr..Disc = laData[16]
  RETURN
ENDIF
llCUpdate = .T.
SELECT (lcOrdHdr)
=RLOCK()
REPLACE Disc WITH laData[16]
UNLOCK
lnOldComm1 = laData[28]
lnOldComm2 = laData[30]
=lfvCommision()
*E300408,1 Message : 32047
*E300408,1 Would you like to overwrite order lines commissions with new commissions?
*E300408,1 Button : 32000
*E300408,1 Yes No
IF (lnOldComm1<>laData[28] .OR. lnOldComm2<>laData[30]) .AND. ;
   SEEK(lcOrdType+laData[1],lcOrdLine) .AND. ;
   (laSetups[2,2] <> 'Y' .OR. gfModalGen('QRM32047B32000','ALERT',;
   IIF(lcOrdType='C','contract','order'))=1)
  SELECT (lcOrdLine) 
  SCAN
    =RLOCK()
    REPLACE Comm1 WITH laData[28],;
            Comm2 WITH laData[30]
    UNLOCK
  ENDSCAN
  GO TOP 
  SELECT (lcOrdHdr)
ENDIF

*!*************************************************************
*! Name      : lfvLinkCode
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate sales g.l. link code
*!*************************************************************
*! Calls     : gfGLBrowse
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvLinkCode()
*!*************************************************************
FUNCTION lfvLinkCode
PRIVATE lcLinkCode
IF llBrowse .OR. !SEEK('01'+laData[32],'Gl_Link')
  *B603890,1 AMH Fix bug that Empty GL_Link Field [Start]
  lcLinkCode = laData[32]
  =gfGLBrowse('01',@lcLinkCode)
  *laData[32] = lcLinkCode
  IF EMPTY(lcLinkCode) .AND. !SEEK('01'+laData[32],'Gl_Link')
    laData[32] = Link_Code
  ELSE
    IF !EMPTY(lcLinkCode)
      laData[32] = lcLinkCode
    ENDIF
  ENDIF
  *B603890,1 AMH Fix bug that Empty GL_Link Field [End  ]
ENDIF
SELECT (lcOrdHdr)
=RLOCK()
REPLACE Link_Code WITH laData[32]
UNLOCK
=lfRefresh(lcWinCh4)
llBrowse = .F.

*!*************************************************************
*! Name      : lfvSlsLink
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Order Sales link code
*!*************************************************************
*! Calls     : gfGLBrowse
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvSlsLink()
*!*************************************************************
FUNCTION lfvSlsLink
PRIVATE lcLinkCode

*B802324,1 Read field insted of variable.
*IF llBrowse .OR. !SEEK('02'+lcGlSales,'Gl_Link')
IF llBrowse .OR. !SEEK('02'+SUBSTR(laData[53],1,3),'Gl_Link')
  *lcLinkCode = lcGlSales
  lcLinkCode = SUBSTR(laData[53],1,3)
  =gfGLBrowse('02',@lcLinkCode,'',1)
  *lcGlSales = lcLinkCode
  laData[53] = lcLinkCode
ENDIF
=lfRefresh(lcWinCh4)
llBrowse = .F.

*!*************************************************************
*! Name      : lfvFactor
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate factor
*!*************************************************************
*! Calls     : FacChk
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvFactor()
*!*************************************************************
FUNCTION lfvFactor
*E301452,1 ABD [Begin]
*AMH [Start]
PRIVATE lcFacCode
lcFacCode = &lcOrdHdr..cFacCode
*AMH [End  ]
*E301452,1 ABD [End  ]

IF llBrowse .OR. (!EMPTY(laData[26]) .AND. !SEEK(laData[26],'SycFact'))
  lcBrFields  = [cFacCode:H='Factor',cFacComp:H='Name',cFacCont:H='Contact',cPhoneNo :P= gfPhoneTem() :H='Phone']
  SELECT SycFact
  laData[26]= IIF(ARIABROW('',"Factors",gnBrFSRow1, gnBrFSCol1,;
                  gnBrFSRow2, gnBrFSCol2,'','','cFacCode','laBrowArr'),;
                  SycFact.cFacCode,SPACE(5))
ENDIF
SELECT (lcOrdHdr)
=RLOCK()
REPLACE cFacCode WITH laData[26]
UNLOCK
*E301452,1 ABD Read the GL link code of the factor while creating the sales order. 
*E301452,1 ABD If there is a factor assgined to the current customer.
*E301452,1 ABD we should read the GL link code of this factor insated of the 
*E301452,1 ABD gl link code of this customer.  
*E301452,1 ABD If the factor link code is empty we should not change the current sitation. [Begin]
IF !EMPTY(cFacCode)
  =gfOpenFile(gcDataDir+'Factor',gcDataDir+'Factor','SH')
  IF SEEK(&lcOrdHdr..cFacCode,'FACTOR') .AND. !EMPTY(Factor.Link_Code)
    laData[32] = Factor.Link_Code
  ELSE
    *AMH 09/10/2000 Update with customer link_code if it exist [Start]
    *= SEEK('M'+&lcOrdHdr..ACCOUNT,'CUSTOMER')
    IF laSetups[10,2]='Y'
      =gfRltFld(laData[15],@laDRltFld,'CDIVISION')
      laData[53] = ALLTRIM(laData[53])
      laData[32] = ALLTRIM(laData[32])
    ELSE
      STORE Customer.cSlsGlLink TO laData[53]
    ENDIF
    IF EMPTY(laData[32])
      IF SEEK('M'+&lcOrdHdr..ACCOUNT,'CUSTOMER') .AND. !EMPTY(Customer.Link_Code)
        laData[32] = Customer.Link_Code
      ELSE
        laData[32] = 'DEFDEF'
      ENDIF
    ENDIF
    *AMH 09/10/2000 Update with customer link_code if it exist [End  ]
  ENDIF
  =gfCloseFile('FACTOR')
 SELECT (lcOrdHdr)
  *AMH 09/06/2000 Remark the next lines and add it after the end of If [Start]
  *=RLOCK()
  *REPLACE Link_Code WITH laData[32]
  *UNLOCK
  *AMH 09/06/2000 Remark the next lines and add it after the end of If [End  ]
  SHOW GET ibGlLink   DISABLE
  SHOW GET laData[32] DISABLE
ELSE
  *AMH 09/06/2000 ENABLE ibGlLink in View or Select Mode [Start]
  *SHOW GET ibGlLink   ENABLE
  *SHOW GET laData[32] ENABLE
  IF laScrMode[3] .OR. laScrMode[4]
    SHOW GET ibGlLink   ENABLE
    SHOW GET laData[32] ENABLE
  ENDIF
  IF lcFacCode <> laData[26]
    IF laSetups[10,2]='Y'
      =gfRltFld(laData[15],@laDRltFld,'CDIVISION')
      laData[53] = ALLTRIM(laData[53])
      laData[32] = ALLTRIM(laData[32])
    ELSE
      STORE Customer.cSlsGlLink TO laData[53]
    ENDIF
    IF EMPTY(laData[32])
      IF SEEK('M'+&lcOrdHdr..ACCOUNT,'CUSTOMER') .AND. !EMPTY(Customer.Link_Code)
        laData[32] = Customer.Link_Code
      ELSE
        laData[32] = 'DEFDEF'
      ENDIF
    ENDIF
  ENDIF
  *AMH 09/06/2000 ENABLE ibGlLink in View or Select Mode [End  ]
ENDIF  
*AMH 09/06/2000 update Link_code [Start]
=RLOCK()
REPLACE Link_Code WITH laData[32]
UNLOCK
*AMH 09/06/2000 update Link_code [End  ]
*E301452,1 ABD [END]

=lfRefresh(lcWinCh4)
llBrowse = .F.

*!*************************************************************
*! Name      : lfvAccount
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Order Account
*!*************************************************************
*! Calls     : CUSBROWM,gfGetAdr,gfChkRate,gfModalGen,gfActWind,lfRefresh,ARIABROW
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvAccount()
*!*************************************************************
FUNCTION lfvAccount
PRIVATE xAccount

IF llBrowse .OR. (!EMPTY(laData[2]) .AND. !SEEK('M'+laData[2],'CUSTOMER'))
  xAccount = laData[2]
  SELECT CUSTOMER
  DO CUSBROWM WITH xAccount
  laData[2] = xAccount
ENDIF
=lfRefresh(lcWinCh1)
llBrowse = .F.
IF laScrMode[1]
  IF EMPTY(laData[2])
    SHOW GET ibStore   DISABLE
    SHOW GET laData[3] DISABLE
  ELSE
    SHOW GET ibStore   ENABLE
    SHOW GET laData[3] ENABLE
  ENDIF
ENDIF
IF laScrMode[4] .AND. !EMPTY(laData[2])
  llFromBulk = .F.
  IF Customer.Status <> 'A'
    *B803068,1 ABD Change the sales order screen to allow to make an order for
    *B803068,1 ABD status 'Potential' of accounts. [ Begin ]
    IF Customer.Status <> 'P'
      *B803068,1 ABD [ End ]
      *E300420,1 Message : 32023
      *E300420,1 This a non-active xxxxx. Order entry is not allowed!
      *E300420,1 Button : 00000
      *E300420,1 Ok
      =gfModalGen('TRM32023B00000','ALERT','account'+'|'+IIF(lcOrdType='C','Contract','Order'))
      *B803278,1 ABD  Fix Bug that when a customer is on hold get a message 
      *B803278,1 ABD  And try to cancel this order and get the same message again. [Begin]
      laData[2] = ''
      *B803278,1 ABD  [End]
      _CUROBJ = _CUROBJ
      RETURN
      *B803068,1 ABD Endif for if statement [Begin]
    ENDIF
    *B803068,1 ABD [ End]
  ENDIF  
  *E300420,1 Message : 32011
  *E300420,1 There are open bulk orders for account xxxxx. 
  *E300420,1 Do you wish to use any of them in creating this order?
  *E300420,1 Button : 32000
  *E300420,1 Yes/No
  IF lcOrdType='O' 
    SET ORDER TO TAG Ordbulk IN ORDHDR
    llFromBulk = (SEEK(laData[2]+'OYO','ORDHDR') .OR. SEEK(laData[2]+'HYO','ORDHDR')) .AND. ;
                  gfModalGen('QRM32011B32000','ALERT',laData[2])=1
    SET ORDER TO TAG ORDHDR IN ORDHDR
    IF llFromBulk
      SELECT * FROM ORDHDR ;
      WHERE (account+status+bulk+cordtype+order=laData[2]+'OYO') .OR. ;
            (account+status+bulk+cordtype+order=laData[2]+'HYO') ;
      INTO CURSOR 'BulkOrd'
      lcBrFields = [Order:H="Order#",status:1:H="S",Season:H="SE",cDivision:H="DI",]+;
                   [CustPo :H="Cust. P.O#",]+;
                   [Open:H="Open.Qty.",OpenAmt:H="Open.Amt.",Ship:H="Ship.Qty.",Shipamt:H="Ship.Amt.",]+;
                   [start:H="Start",Complete:H="Complete",Note1:6:H="Notes"]
      llFromBulk = ARIABROW("","Open bulk orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr')
      =IIF(llFromBulk,SEEK('O'+BulkOrd.Order,'OrdHdr'),.T.)
      USE IN 'BulkOrd'
    ENDIF
  ENDIF
  =SEEK('M'+laData[2],'CUSTOMER')
  *E301077,5 Inhance openning files to speed up transaction
  =IIF(laSetups[11,2]='D',gfOpenFile(gcDataDir+'REP_DIV',gcDataDir+'REP_DIV','SH'),.T.)
  *E301077,5 (End)

  *C200081,1 Reham On 06/27/1999 *** Begin ***
  *C200081,1 If there are user fields for this order, Call process Id to
  *C200081,1 Set default for these fields.
  IF ASCAN(laEvntTrig , PADR('INI_VAR',10)) <> 0
    =gfDoTriger('SOORD',PADR('INI_VAR',10))
  ENDIF     
  *C200081,1 Reham On 06/27/1999 *** End   ***
  
  *E301288,1 Reham On 08/10/1999   *** Begin ***
  *E301288,1 Collect all the adornement records for the bulk order.
  IF llFromBulk .AND. llBomVarnt
    lcCurBlkOr = OrdHdr.Order
    SELECT * , "A" AS cStatus , 0 AS nRecNo ;
      FROM (gcDataDir + "BomVar") ;
     WHERE cIdType+cCost_Id+STR(LineNo,6) = "SO" + lcCurBlkOr ;
      INTO DBF (gcWorkDir + lcT_BomVar)
    INDEX ON cIdType+cCost_Id+STR(LineNo,6) TAG (lcT_BomVar)
    REPLACE ALL cCost_Id WITH SPACE(6)
  ENDIF
  *E301288,1 Reham On 08/10/1999   *** End   ***
  
  lcPriceLvl = IIF(!EMPTY(Customer.PriceLvl),Customer.PriceLvl,'A')
  STORE Customer.BtName TO lcBillName,lcDBllName
  =gfGetAdr('Customer','','','',1,'2')
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcDBllAdd&lcCount = lcDBllAdd&lcCount + IIF(EMPTY(lcDBllAdd&lcCount),'',',')+;
      SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
    lcBillAdd&lcCount = lcDBllAdd&lcCount
  ENDFOR  
  *B803373,1 Go To Distribution Center if exist AME [Start]
  IF !EMPTY(Customer.Dist_Ctr)
    llMove = .T.
    lcESeek = Customer.Store
    =SEEK("S"+Customer.Account+Customer.Dist_ctr,"Customer")
  ENDIF
  *B803373,1 AME[End]
  STORE IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba) TO lcDShpName,lcShipName
  =gfGetAdr('CUSTOMER','','','',1,'')
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcDShpAdd&lcCount = lcDShpAdd&lcCount + IIF(EMPTY(lcDShpAdd&lcCount),'',',')+;
    SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
    lcShipAdd&lcCount = lcDShpAdd&lcCount
  ENDFOR  
  *B803373,1 Back to original place if we have beeen moved from it AME[Start]
  IF llMove
    llMove = .F.
    =SEEK("S"+Customer.Account+lcESeek,"Customer")
  ENDIF
  *B803373,1 AME[End]
  *B802782,1 ABD Store the customer PO to the new order if it's from bulk order
  STORE IIF(llFromBulk,OrdHdr.CustPo,'') TO laData[4]
  *B802782,1 ABD [ End ]
  *E500309,1 AMM Get the bulk order approval code to the new order
  STORE IIF(llFromBulk,OrdHdr.Approval,'') TO laData[48]
  *E500309,1 AMM end
 
  STORE gdSysDate TO laData[8],laData[9]
  STORE IIF(llFromBulk,OrdHdr.cFacCode,Customer.cFacCode) TO laData[26]
  STORE IIF(llFromBulk,OrdHdr.ShipVia,;
        IIF(EMPTY(Customer.ShipVia),lfGetDefCode('SHIPVIA'),;
        Customer.ShipVia)) TO laData[12]
  STORE IIF(llFromBulk,OrdHdr.cTermCode,;
        IIF(EMPTY(Customer.cTermCode),lfGetDefCode('CTERMCODE'),;
        Customer.cTermCode))   TO laData[11]
  STORE IIF(llFromBulk,OrdHdr.SpcInst,;
        IIF(EMPTY(Customer.SpcInst),lfGetDefCode('SPCINST'),;
        Customer.SpcInst)) TO laData[13]
        
  STORE IIF(llFromBulk,OrdHdr.Disc,Customer.Disc)     TO laData[16]
  STORE IIF(llFromBulk,OrdHdr.Rep1,Customer.SalesRep) TO laData[27]
  STORE IIF(llFromBulk,OrdHdr.Rep2,Customer.Rep2)     TO laData[29]
  STORE IIF(llFromBulk,OrdHdr.Note1,Customer.Note)    TO laData[18]
  STORE IIF(llFromBulk,OrdHdr.Note2,'')               TO laData[19]
  STORE IIF(llFromBulk,OrdHdr.Buyer,Customer.Buyer)   TO laData[20]
  STORE IIF(llFromBulk,OrdHdr.Phone,Customer.Phone1)  TO laData[21]
  STORE 'N' TO laData[6],laData[23],laData[24]

  *E301452,1 ABD Read the GL link code of the factor while creating the sales order. 
  *E301452,1 ABD If there is a factor assgined to the current customer.
  *E301452,1 ABD we should read the GL link code of this factor insated of the 
  *E301452,1 ABD gl link code of this customer.  
  *E301452,1 ABD If the factor link code is empty we should not change the current sitation. [Begin]
  STORE IIF(llFromBulk,OrdHdr.Link_Code,Customer.Link_Code) TO laData[32]  
  *AMH 09/10/2000 Update with customer link_code if it exist [Start]
  IF EMPTY(Customer.Link_Code)
    lcLink_Cod = 'DEFDEF'
  ELSE
    lcLink_Cod = Customer.Link_Code
  ENDIF
  *AMH 09/10/2000 Update with customer link_code if it exist [End  ]
  STORE IIF(llFromBulk,OrdHdr.Link_Code,lcLink_Cod) TO laData[32]
  *E301452,1 ABD [End]
  
  STORE IIF(llFromBulk,OrdHdr.cDivision,lcODefDiv) TO laData[15]
  IF laSetups[10,2]='Y'
    =gfRltFld(laData[15],@laDRltFld,'CDIVISION')
    *B603678,1 AMM remove the spaces from the variable
    laData[53] = ALLTRIM(laData[53])
    laData[32] = ALLTRIM(laData[32])
    *B603678,1 AMM end
  ELSE
  *B802324,1 store to field insted of variable.
    *STORE Customer.cSlsGlLink TO lcGlSales
    STORE Customer.cSlsGlLink TO laData[53]
  ENDIF
  *E301452,1 ABD [Start]
  *AMH [Start]
  IF !EMPTY(Customer.cFacCode) 
    =gfOpenFile(gcDataDir+'Factor',gcDataDir+'Factor','SH')
    IF SEEK(Customer.cFacCode,'FACTOR') .AND. !EMPTY(Factor.Link_Code)
      laData[32] = Factor.Link_Code
    ENDIF
    =gfCloseFile('FACTOR')    
  ENDIF
  IF EMPTY(laData[32])
    STORE IIF(llFromBulk,OrdHdr.Link_Code,lcLink_Cod) TO laData[32]
  ENDIF
  *AMH [End  ]
  *E301452,1 ABD [End  ]
  *lcGlSales = IIF(EMPTY(lcGlSales),'DEF',lcGlSales)
  laData[53]= IIF(EMPTY(laData[53]),'DEF',laData[53])
  *B802324,1 End.
  *C101557,1 Start.
  laData[52] = SUBSTR(laOCtgCd[lnOCtgCd,2],1,6)  
  *C101557,1 End.
  *B802351,1 Default the insurence from customer.
  llInsur = (CUSTOMER.cInsur='Y')
  laData[22]=IIF(llInsur,'Y','N')
  *B802351,1 End.

  STORE IIF(llFromBulk,OrdHdr.Comm1,;
        IIF(laSetups[11,2]='D' .AND. SEEK(laData[27]+laData[15],'REP_DIV'),;
            REP_DIV.Comm_Rate,Customer.Comm)) TO laData[28],lnSouComm1
  STORE IIF(llFromBulk,OrdHdr.Comm2,;
        IIF(laSetups[11,2]='D' .AND. SEEK(laData[29]+laData[15],'REP_DIV'),;
            REP_DIV.Comm_Rate,Customer.Comm2)) TO laData[30],lnSouComm2
  STORE IIF(llFromBulk,OrdHdr.cCurrCode,Customer.cCurrCode) TO laData[33]
  STORE IIF(llFromBulk,OrdHdr.Open,0)    TO laData[35],laData[41]
  STORE IIF(llFromBulk,OrdHdr.OpenAmt,0) TO laData[36],laData[42]
  STORE ldDefOrdDate TO laData[10]
  STORE IIF(llFromBulk,OrdHdr.Season,lcODefSes)     TO laData[14]
  STORE IIF(llFromBulk,OrdHdr.cWareCode,lcODefWare) TO laData[31]
  STORE IIF(llFromBulk,OrdHdr.Order,'') TO laData[43]
  
  *B802614,1 ABD- Distribution order should inherit bulk order status & give 
  *B802614,1 ABD- User message to ask him if he want to be default . [Begin]
  *STORE IIF( !EMPTY(laData[26]) OR laSetups[7,2]='Y','H','O') TO laData[5]
  STORE IIF( !EMPTY(laData[26]) OR IIF(llFromBulk,lfOrdStats(),laSetups[7,2]='Y') ,'H','O') TO laData[5]
  *B802614,1 ABD  [End]
  STORE IIF(laData[5]='O',2,3) TO lnOrdStatus
  
  *B801876,1 Default order priority to customer priority 
  STORE IIF(llFromBulk,Ordhdr.Priority,Customer.Priority) TO laData[25]
  SHOW GET lnOrdStatus ENABLE
  laData[33] = IIF(EMPTY(laData[33]),gcBaseCurr,laData[33])
  IF laData[33] = gcBaseCurr
    STORE 1 TO laData[34], laData[50]
  ELSE
    laData[34]=gfChkRate('laData[50]',laData[33],gdSysDate,.T.,.F.,.F.,llEditExRt)
    IF laData[34] = 0
      IF llEditExRt
        *E300420,1 Message : 00262
        *E300420,1 A valid xxx to xxx exchange rate could not be found on xxx.
        *E300420,1 Button : 00000
        *E300420,1 Ok
        =gfModalGen('INM00262B00000','ALERT',ALLTRIM(laData[33])+'|'+ALLTRIM(gcBaseCurr)+'|'+DTOC(gdSysDate))
      ELSE
        laData[33] = gcBaseCurr
        STORE 1 TO laData[34], laData[50]
      ENDIF
    ENDIF
  ENDIF
  *E301077,5 Inhance openning files to speed up transaction
  =IIF(laSetups[11,2]='D',gfCloseFile('REP_DIV'),.T.)
  *E301077,5 (End)

  SELECT (lcOrdHdr)
  APPEND BLANK
  =RLOCK()
  GATHER FROM laData FIELDS &lcScFields
  REPLACE cOrdType WITH lcOrdType
  UNLOCK
  =lfvRep1() .AND. lfvRep2()
  SHOW GET ibAccount  DISABLE
  SHOW GET laData[2]  DISABLE
  SHOW GETS WINDOW (lcWinCh2) ENABLE ONLY
  =lfRefresh(lcWinCh2)
  SHOW GET lcShipName DISABLE
  SHOW GET lcShipAdd1 DISABLE
  SHOW GET lcShipAdd2 DISABLE
  SHOW GET lcShipAdd3 DISABLE
  SHOW GET lcShipAdd4 DISABLE
  SHOW GET lcShipAdd5 DISABLE
  =gfwCodePop(@laCodes,'CTERMCODE','T')
  =gfwCodePop(@laCodes,'SHIPVIA','T')
  *B802366,1 AMM
  IF llMultiSt
    =lfChMSHV()
  ENDIF
  *B802366,1 AMM end
  =gfwCodePop(@laCodes,'SPCINST','T')
  =gfwCodePop(@laCodes,'SEASON','T')
  =gfwCodePop(@laCodes,'CDIVISION','T')

  lcScrMode = 'N'
  =IIF(llFromBulk,.T.,lfvCommision())
  SELECT 'UNCMSESS'
  *B602848,1 MAN 04/28/1999 Change the seek Expr 
  *IF SEEK('I')
  IF SEEK('I'+PADR("SOORD",10)+PADR(gcUser_id,10)) AND RLOCK()
  *B602848,1 MAN end
    BLANK
  ELSE
    APPEND BLANK
  ENDIF
  REPLACE Status     WITH 'O'       ,;
          cUTranType WITH lcProgID  ,;
          cUserId    WITH gcUser_id ,;
          cSession   WITH lcSession ,;
          cProgram   WITH 'SOORD'   ,;
          cCurrScr   WITH 'SOORD'   ,;
          dTranDate  WITH gdSysDate ,;
          cTranTime  WITH TIME()
  =RLOCK()
  lcFiles = 'lcOrdHdr,'+lcOrdHdr+','+lcOrdHdr+';'+;
            'lcOrdLine,'+lcOrdLine+',ORDLINE;'+IIF(llFromBulk,;
            'lcBulkOrd'+','+lcBulkOrd+','+lcBulkOrd+';','')
  =gfSavSess(lcProgID, lcFiles, @laVariables,lcSession)
  SELECT (lcOrdHdr)
  SHOW GET lnOrdStatus ENABLE  
  SHOW GET ibStore     ENABLE
  SHOW GET laData[3]   ENABLE
  SHOW GET laData[4]   ENABLE  
  SHOW GET llMultiSt   ENABLE  
  llCUpDate = .T.
  IF llFromBulk
    SHOW GET lnSeason    DISABLE
    SHOW GET lnDivision  DISABLE
    SHOW GET lnWareHouse DISABLE
    SHOW GET laData[33]  DISABLE
    SHOW GET laData[34]  DISABLE
  ENDIF
  IF !llEditExRt OR laData[33]=gcBaseCurr
    SHOW GET laData[34] DISABLE
  ELSE
    SHOW GET laData[34] ENABLE
  ENDIF
  IF (Customer.Age60 <> 0 .OR. Customer.Age90 <> 0;
    .OR. Customer.Age120 <> 0 ) .AND. laSetups[12,2]='Y'
    ?? CHR(7)
    ?? CHR(7)
    ?? CHR(7)
    DO (gcScrDir+gcWinAppl+"\SOAGING.SPX")
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvMultiSt
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate multi stores check box
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvMultiSt()
*!*************************************************************     
FUNCTION lfvMultiSt

IF llMultiSt
  IF !SEEK('S'+laData[2],'Customer')
    *E300420,1 Message : 32048
    *E300420,1 No stores found for account xxxxx
    *E300420,1 Button : 00000 
    *E300420,1 Ok
    =gfModalGen('TRM32048B00000','ALERT',laData[2])
  
    STORE .F. TO llMultiSt
    SHOW GET llMultiSt
    RETURN
  ENDIF
  STORE .F. TO llBulk
  STORE ''  TO lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5
  STORE ''  TO lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5
  STORE SPACE(8) TO laData[3]
  STORE '*' TO laData[12]
  laCodes[2,5] = .T.
  STORE 'At Store Level' TO lcShipName,lcBillName
  SHOW GET ibStore    DISABLE
  SHOW GET laData[3]  DISABLE
  SHOW GET llBulk     DISABLE
  SHOW GET lnShipAddr DISABLE
  SHOW GET laData[7]  ENABLE
  IF lnActFolder = 2
    SHOW GET pbTemplate ENABLE
  ENDIF
ELSE
  laData[12] = Customer.ShipVia
  laCodes[2,5] = .F.
  lcShipName = lcDShpName
  lcShipAdd1 = lcDShpAdd1
  lcShipAdd2 = lcDShpAdd2
  lcShipAdd3 = lcDShpAdd3
  lcShipAdd4 = lcDShpAdd4
  lcShipAdd5 = lcDShpAdd5
  lcBillName = lcDBllName
  lcBillAdd1 = lcDBllAdd1
  lcBillAdd2 = lcDBllAdd2 
  lcBillAdd3 = lcDBllAdd3
  lcBillAdd4 = lcDBllAdd4
  lcBillAdd5 = lcDBllAdd5
  laData[7]  = .F.
  laData[4]  = SPACE(15)
  SHOW GET ibStore    ENABLE
  SHOW GET laData[3]  ENABLE
  IF lcOrdType <> 'C'
    SHOW GET llBulk ENABLE
  ENDIF  
  SHOW GET lnShipAddr ENABLE
  SHOW GET laData[7]  DISABLE
  IF lnActFolder = 2
    SHOW GET pbTemplate DISABLE
  ENDIF
ENDIF
laData[6] = IIF(llMultiSt,'Y','N')
SHOW GET laData[4] ENABLE
lnShipAddr = 1
SHOW GET lnShipAddr
SHOW GET lcShipName DISABLE
SHOW GET lcShipAdd1 DISABLE
SHOW GET lcShipAdd2 DISABLE
SHOW GET lcShipAdd3 DISABLE
SHOW GET lcShipAdd4 DISABLE
SHOW GET lcShipAdd5 DISABLE
=lfRefresh(lcWinCh2)
SHOW GETS WINDOW (lcWinCh2) ONLY
SELECT (lcOrdHdr)
=RLOCK()
REPLACE Multi     WITH IIF(llMultiSt,'Y','N') ,;
        MultiPo   WITH laData[7] ,;
        CustPo    WITH laData[4] ,;
        Store     WITH laData[3] ,;
        Bulk      WITH IIF(llBulk,'Y','N') ,;
        Alt_ShpTo WITH laData[51],;
        StName    WITH SPACE(30) ,;
        cAddress1 WITH SPACE(30) ,;
        cAddress2 WITH SPACE(30) ,;
        cAddress3 WITH SPACE(30) ,;
        cAddress4 WITH SPACE(30) ,;
        cAddress5 WITH SPACE(30) ,;
        ShipVia   WITH laData[12]
UNLOCK
IF llMultiSt
  =gfwCodePop(@laCodes,'SHIPVIA','A')
  *B802366,1 AMM
  IF llMultiSt
    =lfChMSHV()
  ENDIF
  *B802366,1 AMM end
ELSE
  =gfwCodePop(@laCodes,'SHIPVIA','T')
ENDIF

*!*************************************************************
*! Name      : lfvStore
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Invoice store
*!*************************************************************
*! Calls     : CUSBROWS,gfGetAdr,gfChkRate,gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvStore()
*!*************************************************************
FUNCTION lfvStore
PRIVATE xStore,lnAlias
lnAlias = SELECT()
IF llBrowse .OR. (!EMPTY(laData[3]) .AND. ;
                  !SEEK('S'+laData[2]+laData[3],'CUSTOMER'))
  xStore = laData[3]
  SELECT CUSTOMER
  IF !CUSBROWS(laData[2],.T.)
    llBrowse = .F.
    *B803049,1 ABD Fix bug alias not found. [ Begin ]
    *SELECT (lcOrdHdr)
    *STORE Store TO laData[3]
    IF laScrMode[1]
      STORE lcOldValue TO laData[3]
    ELSE
      SELECT (lcOrdHdr)
      STORE Store TO laData[3]
    ENDIF
    *B803049,1 ABD [ End ]
    SELECT (lnAlias)
    RETURN
  ENDIF
  laData[3] = xStore
ENDIF  
llBrowse = .F.
IF !laScrMode[1] .AND. laData[3] = &lcOrdHdr..Store
  SELECT (lnAlias)
  RETURN
ENDIF

IF !EMPTY(laData[3]) .AND. Customer.Status <> 'A' 
  *B803068,1 ABD Change the sales order screen to allow to make an order for
  *B803068,1 ABD status 'Potential' of accounts. [ Begin ]
  IF Customer.Status <> 'P'
    *B803068,1 ABD [ End ]
    *E300420,1 Message : 32023
    *E300420,1 This a non-active xxxxx. Order entry is not allowed!
    *E300420,1 Button : 00000
    *E300420,1 Ok
    =gfModalGen('TRM32023B00000','ALERT','store'+'|'+IIF(lcOrdType='C','Contract','Order'))
    SELECT (lcOrdHdr)
    STORE Store TO laData[3]
    SELECT (lnAlias)
    RETURN
    *B803068,1 ABD Endif for if statement [Begin]
  ENDIF
  *B803068,1 ABD [End]
ENDIF
DO CASE
  CASE laScrMode[4]
    DO CASE 
      CASE EMPTY(laData[3]) .AND. ;
        gfModalGen('QRM32024B32000','ALERT',IIF(lcOrdType='C','contract','order')+'|'+'customer')=1
        *E300420,1 Message : 32024
        *E300420,1 Overwrite order header with xxxxx defaults?
        *E300420,1 Button : 32000
        *E300420,1 Yes;No
        =SEEK('M'+laData[2],'CUSTOMER')
        lcBillName = lcDBllName
        lcBillAdd1 = lcDBllAdd1
        lcBillAdd2 = lcDBllAdd2
        lcBillAdd3 = lcDBllAdd3
        lcBillAdd4 = lcDBllAdd4
        lcBillAdd5 = lcDBllAdd5
        lcShipName = lcDShpName
        lcShipAdd1 = lcDShpAdd1
        lcShipAdd2 = lcDShpAdd2
        lcShipAdd3 = lcDShpAdd3
        lcShipAdd4 = lcDShpAdd4
        lcShipAdd5 = lcDShpAdd5
        lnShipAddr = 1
        SHOW GETS WINDOW (lcWinCh2) ONLY
        *E301077,5 Inhance openning files to speed up transaction
        =IIF(laSetups[11,2]='D',gfOpenFile(gcDataDir+'REP_DIV',gcDataDir+'REP_DIV','SH'),.T.)
        *E301077,5 (End)
        
        STORE Customer.SalesRep  TO laData[27]
        STORE Customer.Rep2      TO laData[29] 
        =lfvRep1() .AND. lfvRep2()
        STORE IIF(laSetups[11,2]='D' .AND. SEEK(laData[27]+laData[15],'REP_DIV'),;
              REP_DIV.Comm_Rate,Customer.Comm) TO laData[28],lnSouComm1
        *B802176,1 Start.
        *STORE IIF(laSetups[11,2]='D' .AND. SEEK(laData[29]+laData[15],'REP_DIV'),;
              REP_DIV.Comm_Rate,Customer.Comm) TO laData[30],lnSouComm2
        STORE IIF(laSetups[11,2]='D' .AND. SEEK(laData[29]+laData[15],'REP_DIV'),;
              REP_DIV.Comm_Rate,Customer.Comm2) TO laData[30],lnSouComm2
        *B802176,1 End.
        lnShipAddr = 1
        =lfRefresh(lcWinCh2)
        STORE Customer.ShipVia  TO laData[12]
        SHOW GETS WINDOW (lcWinCh2) ONLY
        *E301077,5 Inhance openning files to speed up transaction
        =IIF(laSetups[11,2]='D',gfCloseFile('REP_DIV'),.T.)
        *E301077,5 (End)

        SELECT (lcOrdHdr)
        =RLOCK()
        REPLACE Rep1    WITH laData[27] ,;
                Rep2    WITH laData[29] ,;
                Comm1   WITH laData[28] ,;
                Comm2   WITH laData[30] ,;
                ShipVia WITH laData[12]
        UNLOCK
        =gfwCodePop(@laCodes,'SHIPVIA','T')
        *B802366,1 AMM
        IF llMultiSt
          =lfChMSHV()
        ENDIF
        *B802366,1 AMM end

        =lfvCommision()
      CASE !EMPTY(laData[3]) .AND. ;
        gfModalGen('QRM32024B32000','ALERT',IIF(lcOrdType='C','contract','order')+'|'+'store')=1
        *E300420,1 Message : 32024
        *E300420,1 Overwrite order header with xxx defaults?
        *E300420,1 Button : 32000
        *E300420,1 Yes;No
        STORE '' TO lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5,lcBillName,;
                    lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,lcShipName
        lcBilLName = Customer.BtName
        =gfGetAdr('Customer','','','',1,'2')
        FOR lnCount = 1 TO ALEN(laAddress,1)
          lcCount = STR(laAddress[lnCount,1],1)
          lcBillAdd&lcCount = lcBillAdd&lcCount + IIF(EMPTY(lcBillAdd&lcCount),'',',')+;
          SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
        ENDFOR  
        *B803373,1 Go To Distribution Center if exist AME [Start]
        IF !EMPTY(Customer.Dist_Ctr)
          llMove = .T.
          lcESeek = Customer.Store
          =SEEK("S"+Customer.Account+Customer.Dist_ctr,"Customer")
        ENDIF
        *B803373,1 AME[End]
        lcShipName = IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba)
        =gfGetAdr('CUSTOMER','','','',1,'')
        FOR lnCount = 1 TO ALEN(laAddress,1)
          lcCount = STR(laAddress[lnCount,1],1)
          lcShipAdd&lcCount = lcShipAdd&lcCount + IIF(EMPTY(lcShipAdd&lcCount),'',',')+;
          SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
        ENDFOR  
        *B803373,1 Back to original place if we have beeen moved from it AME[Start]
        IF llMove
          llMove = .F.
          =SEEK("S"+Customer.Account+lcESeek,"Customer")
        ENDIF
        *B803373,1 AME[End]

        *E301077,5 Inhance openning files to speed up transaction
        =IIF(laSetups[11,2]='D',gfOpenFile(gcDataDir+'REP_DIV',gcDataDir+'REP_DIV','SH'),.T.)
        *E301077,5 (End)
        STORE Customer.SalesRep TO laData[27]
        STORE Customer.Rep2     TO laData[29] 
        STORE Customer.ShipVia  TO laData[12]
        =lfvRep1() .AND. lfvRep2()
        STORE IIF(laSetups[11,2]='D' .AND. SEEK(laData[27]+laData[15],'REP_DIV'),;
              REP_DIV.Comm_Rate,Customer.Comm) TO laData[28],lnSouComm1
        *B802176,1 Start.
        *STORE IIF(laSetups[11,2]='D' .AND. SEEK(laData[29]+laData[15],'REP_DIV'),;
              REP_DIV.Comm_Rate,Customer.Comm) TO laData[30],lnSouComm2
        STORE IIF(laSetups[11,2]='D' .AND. SEEK(laData[29]+laData[15],'REP_DIV'),;
              REP_DIV.Comm_Rate,Customer.Comm2) TO laData[30],lnSouComm2
        *B802176,1 End.
        
        lnShipAddr = 1
        =lfRefresh(lcWinCh2)
        SHOW GETS WINDOW (lcWinCh2) ONLY
        *E301077,5 Inhance openning files to speed up transaction
        =IIF(laSetups[11,2]='D',gfCloseFile('REP_DIV'),.T.)
        *E301077,5 (End)
        SELECT (lcOrdHdr)
        =RLOCK()
        REPLACE Rep1    WITH laData[27] ,;
                Rep2    WITH laData[29] ,;        
                Comm1   WITH laData[28] ,;
                Comm2   WITH laData[30] ,;
                ShipVia WITH laData[12]
        UNLOCK
        =gfwCodePop(@laCodes,'SHIPVIA','T')
        *B802366,1 AMM
        IF llMultiSt
          =lfChMSHV()
        ENDIF
        *B802366,1 AMM end

        =lfvCommision()
    ENDCASE
  *E300420,1 Message : 32012
  *E300420,1 Change alternate shipto to new store shipto?
  *E300420,1 Button : 32000
  *E300420,1 Yes/No
  CASE laScrMode[3] .AND. lnShipAddr=2 .AND. !EMPTY(laData[3]) .AND. ;
    gfModalGen('QRM32012B32000','ALERT')=1
    
    STORE '' TO lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,lcShipName
    *B803373,1 Go To Distribution Center if exist AME [Start]
    IF !EMPTY(Customer.Dist_Ctr)
      llMove = .T.
      lcESeek = Customer.Store
      =SEEK("S"+Customer.Account+Customer.Dist_ctr,"Customer")
    ENDIF
    *B803373,1 AME[End]    
    
    lcShipName = IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba)
    =gfGetAdr('CUSTOMER','','','',1,'')
    FOR lnCount = 1 TO ALEN(laAddress,1)
      lcCount = STR(laAddress[lnCount,1],1)
      lcShipAdd&lcCount = lcShipAdd&lcCount + IIF(EMPTY(lcShipAdd&lcCount),'',',')+;
      SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
    ENDFOR  
    *B803373,1 Back to original place if we have beeen moved from it AME[Start]
    IF llMove
      llMove = .F.
      =SEEK("S"+Customer.Account+lcESeek,"Customer")
    ENDIF
    *B803373,1 AME[End]

    lnShipAddr=1
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE Alt_ShpTo WITH .F. ,;
            StName    WITH SPACE(30) ,;
            cAddress1 WITH SPACE(30) ,;
            cAddress2 WITH SPACE(30) ,;
            cAddress3 WITH SPACE(30) ,;
            cAddress4 WITH SPACE(30) ,;
            cAddress5 WITH SPACE(30)
    UNLOCK
    SHOW GET lnShipAddr
    SHOW GET lcShipName DISABLE
    SHOW GET lcShipAdd1 DISABLE
    SHOW GET lcShipAdd2 DISABLE
    SHOW GET lcShipAdd3 DISABLE
    SHOW GET lcShipAdd4 DISABLE
    SHOW GET lcShipAdd5 DISABLE
ENDCASE
IF !laScrMode[1]
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE Store WITH laData[3]
  UNLOCK
  IF SEEK(lcOrdType+laData[1],lcOrdLine)
    WAIT 'Update order lines...' WINDOW NOWAIT
    SELECT (lcOrdLine)
    lcCurrRec = EVAL(SYS(14,VAL(SYS(21))))
    SCAN REST WHILE cOrdType+Order = lcOrdType+laData[1]
      =RLOCK()
      REPLACE Store WITH laData[3] ,;
              FLAG  WITH IIF(FLAG <> 'N','M',FLAG)
      UNLOCK
    ENDSCAN
    =SEEK(lcCurrRec)
    WAIT CLEAR 
  ENDIF
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvMultiPo
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate multi customer po number check box
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvMultiPo()
*!*************************************************************
FUNCTION lfvMultiPo

IF laData[7]
  STORE SPACE(10) TO laData[4]
  SHOW GET laData[4] DISABLE
ELSE
  SHOW GET laData[4] ENABLE
ENDIF  
SELECT (lcOrdHdr)
=RLOCK()
REPLACE MultiPo WITH laData[7] ,;
        CustPo  WITH laData[4]
UNLOCK
IF lnActFolder = 2
  =lfBrowDet(.T.,IIF(lnOrdTrans=1 .OR. llZoom,'O','B'))
ELSE
  llReBrowse = .T.
ENDIF

*!*************************************************************
*! Name      : lfvCustPo
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate customer po number
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvCustPo()
*!*************************************************************
FUNCTION lfvCustPo
PRIVATE llCustPo 

DO CASE
  CASE laScrMode[1] .AND. (!EMPTY(laData[2]) .OR. !EMPTY(laData[4]))
    llBrowse = .T.
    _CUROBJ = OBJNUM(laData[1])
    KEYBOARD "{ENTER}"
    RETURN
  CASE laScrMode[4] .AND. laData[4] <> &lcOrdHdr..CustPo
    IF !EMPTY(laData[4])
      SET ORDER TO TAG ORDCUST IN ORDHDR
      llCustPo =SEEK(laData[2]+UPPER(laData[4])+lcOrdType,'OrdHdr')
      SET ORDER TO TAG OrdHdr IN OrdHdr
      *E300420,1 Message : 32025
      *E300420,1 Cust. PO# xxx has been entered before.
      *E300420,1 Button : 32004
      *E300420,1 Proceed;Reenter
      IF llCustPo .AND. gfModalGen('QRM32025B32004','ALERT',ALLTRIM(laData[4]))=2
        _CUROBJ = _CUROBJ
        RETURN
      ENDIF
      SELECT (lcOrdHdr)
      =RLOCK()
      REPLACE CustPo WITH laData[4]
      UNLOCK
      STORE .F. TO laData[7]
      SHOW GET laData[7] DISABLE
    ELSE
      IF llMultiSt
        SHOW GET laData[7] ENABLE
      ENDIF
    ENDIF
ENDCASE

*!*************************************************************
*! Name      : lfvEntered
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid function for order entered date
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvEntered()
*!*************************************************************
FUNCTION lfvEntered

IF laData[8] <> &lcOrdHdr..Entered
  llCUpdate = .T.
  IF laData[33] <> gcBaseCurr
    laData[34] = gfChkRate('laData[50]',laData[33],laData[8],.T.,gcAct_Comp,.F.,llEditExRt)
    IF laData[34] = 0 
      IF llEditExRt
        *E300420,1 Message : 00262
        *E300420,1 A valid xxx to xxx exchange rate could not be found on xxx.
        *E300420,1 Button : 00000
        *E300420,1 Ok
        =gfModalGen('INM00262B00000','ALERT',ALLTRIM(laData[33])+'|'+ALLTRIM(gcBaseCurr)+'|'+DTOC(laData[8]))
      ELSE
        laData[33] = gcBaseCurr
        STORE 1 TO laData[34], laData[50]
      ENDIF  
    ENDIF
  ENDIF
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE cCurrCode WITH laData[33] ,;
          Entered   WITH laData[8]  ,;
          nExRate   WITH laData[34] ,;
          nCurrUnit WITH laData[50]
  UNLOCK
  SHOW GETS WINDOW (lcWinCh2) ONLY
  *B802821,1 AMM Update its value here in case of entered date has changed
  llUpdBook = laData[5]='B' .OR. (gdSysDate <= (laData[8] + laSetups[8,2]))
  *B802821,1 AMM end
ENDIF

*!*************************************************************
*! Name      : lfvStart
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid function for order start date
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvStart()
*!*************************************************************
FUNCTION lfvStart

*C200081,1 Reham On 06/29/1999 *** Begin ***
*C200081,1 If there are user fields for this order, Call process Id for
*C200081,1 when function of the start date
IF ASCAN(laEvntTrig , PADR('VLD_STR',10)) <> 0
  llReturn = gfDoTriger('SOORD',PADR('VLD_STR',10))
  IF !llReturn
    RETURN .F.
  ENDIF
ENDIF     
*C200081,1 Reham On 06/29/1999 *** End   ***

IF laData[9] <> &lcOrdHdr..Start
  IF laData[9] > laData[10]
    *E300420,1 Message : 32013
    *E300420,1 The starting date cannot be greater than the completion date.
    *E300420,1 Button : 00000
    *E300420,1 Ok
    *B802367,1 AMM Change message to 
    *B802367,1 AMM Completion date must follow Start date. It will dedfault to Start date.
    *B802367,1 AMM Change the button to be "Proceed - Reenter", 
    *B802367,1 AMM change the complete date if the user chose Proceed
    *=gfModalGen('TRM32013B00000','ALERT')
    *laData[9] = &lcOrdHdr..Start
    *RETURN
    IF gfModalGen('TRM32013B32004','ALERT',' It will default to Start date.') = 1
      laData[10] = laData[9]
      SHOW GET laData[10]
    ELSE
      laData[9] = &lcOrdHdr..Start
      _CUROBJ = _CUROBJ
      RETURN
    ENDIF
    *B802367,1 AMM end
  ENDIF
  llCUpdate = .T.
  SELECT (lcOrdHdr)
  =RLOCK()
  *B802367,1 AMM Save completion date as well.
  *REPLACE Start WITH laData[9]
  REPLACE Start    WITH laData[9],;
          Complete WITH laData[10]
  *B802367,1 AMM  end
  UNLOCK
ENDIF

*!*************************************************************
*! Name      : lfvComplete
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid function for order completion date
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvComplete()
*!*************************************************************
FUNCTION lfvComplete

*C200081,1 Reham On 06/29/1999 *** Begin ***
*C200081,1 If there are user fields for this order, Call process Id to
*C200081,1 when function of the complete date
IF ASCAN(laEvntTrig , PADR('VLD_CMP',10)) <> 0
  llReturn = gfDoTriger('SOORD',PADR('VLD_CMP',10))
  IF !llReturn
    RETURN .F.
  ENDIF
ENDIF     
*C200081,1 Reham On 06/29/1999 *** End   ***

IF laData[10] <> &lcOrdHdr..Complete
  IF laData[9] > laData[10]
    *E300420,1 Message : 32013
    *E300420,1 The starting date cannot be greater than the completion date.
    *E300420,1 Button : 00000
    *E300420,1 Ok
    =gfModalGen('TRM32013B00000','ALERT')
    laData[10] = &lcOrdHdr..Complete
    RETURN
  ENDIF
  *B602713,1 ABD The message "Completion date is a Sunday" 
  *B602713,1 ABD Should not appear for Egypt. [ Begin ]
  IF  gcContCode # 'EGYPT'
    *B602713,1 ABD [ End]
    IF CDOW(laData[10])='Saturday' .OR. CDOW(laData[10])='Sunday'
      *E300420,1 Message : 32014
      *E300420,1 The completion date is a xxxxx.
      *E300420,1 Button : 00000
      *E300420,1 Ok
      =gfModalGen('INM32014B00000','ALERT',CDOW(laData[10]))
    ENDIF
    *B602713,1 ABD  End for if statement. [ Begin ]  
  ENDIF
  *B602713,1 ABD [ End]
  llCUpdate = .T.
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE Complete WITH laData[10]
  UNLOCK
ENDIF

*!*************************************************************
*! Name      : lfvShipAddr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Switch between default and alternative shipping adresses
*!*************************************************************
*! Calls     : gfActPop
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvShipAddr()
*!*************************************************************
FUNCTION lfvShipAddr
PRIVATE lcShipState

laData[51] = (lnShipAddr = 2)
IF &lcOrdHdr..Alt_ShpTo <> laData[51]
  llCUpdate = .T.
  lcShipState = IIF(laData[51],'ENABLE','DISABLE')
  lcShipName  = IIF(laData[51],lcShipName,lcDShpName)
  lcShipAdd1  = IIF(laData[51],lcShipAdd1,lcDShpAdd1)
  lcShipAdd2  = IIF(laData[51],lcShipAdd2,lcDShpAdd2)
  lcShipAdd3  = IIF(laData[51],lcShipAdd3,lcDShpAdd3)
  lcShipAdd4  = IIF(laData[51],lcShipAdd4,lcDShpAdd4)
  lcShipAdd5  = IIF(laData[51],lcShipAdd5,lcDShpAdd5)
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE Alt_ShpTo WITH laData[51] ,;
          StName    WITH IIF(laData[51],lcShipName,SPACE(30)) ,;
          cAddress1 WITH IIF(laData[51],lcShipAdd1,SPACE(30)) ,;
          cAddress2 WITH IIF(laData[51],lcShipAdd2,SPACE(30)) ,;
          cAddress3 WITH IIF(laData[51],lcShipAdd3,SPACE(30)) ,;
          cAddress4 WITH IIF(laData[51],lcShipAdd4,SPACE(30)) ,;
          cAddress5 WITH IIF(laData[51],lcShipAdd5,SPACE(30))
  UNLOCK
  SHOW GET lcShipName &lcShipState
  SHOW GET lcShipAdd1 &lcShipState
  SHOW GET lcShipAdd2 &lcShipState
  SHOW GET lcShipAdd3 &lcShipState
  SHOW GET lcShipAdd4 &lcShipState
  SHOW GET lcShipAdd5 &lcShipState
ENDIF
RETURN

*!*************************************************************
*! Name      : lfvShipping
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid alternative shipping address information
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcAddressNo : Alternative address line #
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvShipping()
*!*************************************************************
FUNCTION lfvShipping
PARAMETERS lcAddressNo

SELECT (lcOrdHdr)
IF lcAddressNo = '0' .AND. StName <> lcShipName
  =RLOCK()
  REPLACE StName WITH lcShipName
  UNLOCK
  llCUpdate = .T.
ENDIF  
IF lcAddressNo <> '0' .AND. cAddress&lcAddressNo <> lcShipAdd&lcAddressNo
  =RLOCK()
  REPLACE cAddress&lcAddressNo WITH lcShipAdd&lcAddressNo
  UNLOCK
  llCUpdate = .T.
ENDIF

*!*************************************************************
*! Name      : lfvTerms
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid order terms code
*!*************************************************************
*! Calls     : CODECHK
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvTerms()
*!*************************************************************
FUNCTION lfvTerms

IF laData[11] <> ALLTRIM(laTerms[lnTerms,2])
  laData[11] = ALLTRIM(laTerms[lnTerms,2])
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE cTermCode WITH laData[11]
  UNLOCK
  llCUpdate = .T.
ENDIF

*!*************************************************************
*! Name      : lfvShipVia
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate order shipvia
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvShipVia()
*!*************************************************************
PROCEDURE lfvShipVia

IF laData[12] <> ALLTRIM(laShipVia[lnShipVia,2])
  laData[12] = ALLTRIM(laShipVia[lnShipVia,2])
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE ShipVia WITH laData[12]
  UNLOCK
  llCUpdate = .T.
ENDIF

*!*************************************************************
*! Name      : lfvSpcInst
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid order special instruction code
*!*************************************************************
*! Calls     : CODECHK
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvSpcInst()
*!*************************************************************
FUNCTION lfvSpcInst

IF laData[13] <> ALLTRIM(laSpcInst[lnSpcInst,2])
  laData[13] = ALLTRIM(laSpcInst[lnSpcInst,2])
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE SpcInst WITH laData[13]
  UNLOCK
  llCUpdate = .T.
ENDIF

*!*************************************************************
*! Name      : lfvSeason
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid order season code
*!*************************************************************
*! Calls     : CODECHK
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvSeason()
*!*************************************************************
FUNCTION lfvSeason

IF laData[14] <> SUBSTR(laSeasons[lnSeason,2],1,6)
  laData[14] = SUBSTR(laSeasons[lnSeason,2],1,6)
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE Season WITH laData[14]
  UNLOCK
  llCUpdate = .T.
ENDIF

*!*************************************************************
*! Name      : lfvDivision
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid order division code
*!*************************************************************
*! Calls     : CODECHK,lfvCommision
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDivision()
*!*************************************************************
FUNCTION lfvDivision

IF laData[15] <> SUBSTR(laDivision[lnDivision,2],1,6)
  laData[15] = SUBSTR(laDivision[lnDivision,2],1,6)
  *-- Check if link codes at division level
  *E301452,1 ABD [Start]
  *AMH [Start]
  PRIVATE lcLink_cod
  lcLink_cod = laData[32]
  IF laSetups[10,2]='Y'
    =gfRltFld(laData[15],@laDRltFld,'CDIVISION')
    *B603678,1 AMM Remove spaces from the variable.
    laData[53] = ALLTRIM(laData[53])
    laData[32] = ALLTRIM(laData[32])
    *B603678,1 AMM end
  ENDIF
  IF EMPTY(laData[32])
    laData[32] = lcLink_cod
  ENDIF
  IF !EMPTY(Customer.cFacCode) 
    =gfOpenFile(gcDataDir+'Factor',gcDataDir+'Factor','SH')
    IF SEEK(Customer.cFacCode,'FACTOR') .AND. !EMPTY(Factor.Link_Code)
      laData[32] = Factor.Link_Code
    ENDIF
    =gfCloseFile('FACTOR')    
  ENDIF
  *AMH [End  ]
  *E301452,1 ABD [End  ]
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE cDivision  WITH laData[15] ,;
          Link_Code WITH laData[32]
  UNLOCK
  llCUpdate = .T.
  *-- Get sales rep commission for this division
  IF laSetups[11,2]='D'
    *E301077,5 Inhance openning files to speed up transaction
    =gfOpenFile(gcDataDir+'REP_DIV',gcDataDir+'REP_DIV','SH')
    *E301077,5 (End)
    IF SEEK(laData[27]+laData[15],'REP_DIV')
      STORE REP_DIV.Comm_Rate TO laData[28],lnSouComm1
    ENDIF
    IF SEEK(laData[29]+laData[15],'REP_DIV')
      STORE REP_DIV.Comm_Rate TO laData[30],lnSouComm2
    ENDIF
    *E301077,5 Inhance openning files to speed up transaction
    =gfCloseFile('REP_DIV')
    *E301077,5 (End)

    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE Comm1 WITH laData[28] ,;
            Comm2 WITH laData[30]
    UNLOCK
    =lfvCommision()
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvDept
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate department
*!*************************************************************
*! Calls     : gfModalGen,ARIABROW,lfvCommision
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDept()
*!*************************************************************
FUNCTION lfvDept

IF !llBrowse .AND. laData[17] = &lcOrdHdr..Dept
  RETURN
ENDIF
*E301077,5 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'CustDept',gcDataDir+'CustDept','SH')
*E301077,5 (End)
IF llBrowse .OR. (!EMPTY(laData[17]) .AND. !SEEK(laData[2]+laData[17],'CustDept'))
  IF !SEEK(laData[2],'CustDept')
    IF llBrowse
      *E300420,1 Message : 32026
      *E300420,1 No departments assigend to account xxx
      *E300420,1 Button : 00000
      *E300420,1 ok
      =gfModalGen('TRM32026B00000','ALERT',laData[2])
      STORE SPACE(5) TO laData[17]
    ENDIF
    llBrowse = .F.
    RETURN
  ENDIF
  SELECT CustDept
  lcBrFields = [Dept:H="Department",cDeptDesc :H="Description",Rep1,Comm1,Rep2,Comm2,cTermCode]
  laData[17] = IIF(ARIABROW('laData[2]',"Departments",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Dept','laBrowArr'),;
                  CustDept.Dept,laData[17])
ENDIF
llBrowse = .F.
IF laData[17] <> &lcOrdHdr..Dept
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE Dept  WITH laData[17]
  UNLOCK
  *-- If valid epartment, Get new sales rep, commission and payment terms
  IF SEEK(laData[2]+laData[17],'CustDept')
    *E300420,1 Message : 32027
    *E300420,1 Customer defaults will be overwritten by the department defaults.
    *E300420,1 Button : 00000
    *E300420,1 ok
    IF laScrMode[4] .AND. gfModalGen('INM32027B00000','ALERT')=1
      =RLOCK()
      REPLACE Rep1  WITH CustDept.Rep1 ,;
              Rep2  WITH CustDept.Rep1 ,;
              cTermCode WITH CustDept.cTermCode
      UNLOCK
      =gfwCodePop(@laCodes,'CTERMCODE','T')
      STORE CustDept.Rep1  TO laData[27]
      STORE CustDept.Rep2  TO laData[29]
      =lfvRep1() .AND. lfvRep2()
      STORE CustDept.Comm1 TO laData[28],lnSouComm1
      STORE CustDept.Comm2 TO laData[30],lnSouComm2
      SHOW GET laData[27]
      SHOW GET laData[28]
      SHOW GET laData[29]
      SHOW GET laData[30]
      =lfvCommision()
    ENDIF
  ENDIF
  llCUpdate = .T.
ENDIF
*E301077,5 Inhance openning files to speed up transaction
=gfCloseFile('CustDept')
*E301077,5 (End)

*!*************************************************************
*! Name      : lfUpdate
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Update order header other information
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfUpdate()
*!*************************************************************
FUNCTION lfUpdate

IF UPDATE()
  SELECT (lcOrdHdr)
  =RLOCK()  
  GATHER FROM laData FIELDS &lcScFields
  UNLOCK
  llCUpdate = .T.
ENDIF

*!*************************************************************
*! Name      : lfvRep1
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate first salesrep
*!*************************************************************
*! Calls     : REPCHK
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRep1()
*!*************************************************************
FUNCTION lfvRep1

IF llBrowse .OR. (!EMPTY(laData[27]) AND !SEEK(laData[27],'SalesRep'))
  *B602874,1 Open salesrep file.
  SELECT SALESREP
  *B602874,1 End.
  laData[27] = IIF(llBrowse,SPACE(3),laData[27])
  lcRep = laData[27]
  DO REPCHK WITH lcRep
  laData[27] = lcRep
ENDIF  
llBrowse = .F.
SELECT (lcOrdHdr)
IF Rep1 <> laData[27]
  *B803375,1 Replace comm1 with defa. comm for rep1 AME[start] 
  *laData[28] = IIF(EMPTY(laData[27]),0,laData[28])
  IF laSetups[11,2]='D'
    =IIF(laSetups[11,2]='D',gfOpenFile(gcDataDir+'REP_DIV',gcDataDir+'REP_DIV','SH'),.T.)    
    IF SEEK(laData[27]+laData[15],'REP_DIV')
      STORE REP_DIV.Comm_Rate TO laData[28],lnSouComm1
    ELSE
      laData[28] = IIF(EMPTY(laData[27]),0,SalesRep.Comm)
    ENDIF
  ELSE
    laData[28] = IIF(EMPTY(laData[27]),0,IIF(Customer.salesrep # laData[27],SalesRep.Comm,CUSTOMER.Comm))
  ENDIF
  SELECT (lcOrdLine)
  lnRecno = RECNO()
  IF SEEK(&lcOrdHdr..CordType+&lcOrdHdr..Order)
    SCAN WHILE CordType+Order+STR(LineNo,6) = &lcOrdHdr..CordType+&lcOrdHdr..Order
      M.Comm1 = laData[28]
      =lfVComm('1')
    ENDSCAN  
  ENDIF  
  SELECT (LCOrdLine)
  GO TOP
  IF !EOF(lcOrdLine)
    GO lnRecNo
  ENDIF
  =IIF(laSetups[11,2]='D',gfCloseFile('REP_DIV'),.T.)  
  SELECT (lcOrdHdr)
  *B803375,1 AME[End]
  =RLOCK()
  REPLACE Rep1  WITH laData[27] ,;
          Comm1 WITH laData[28]
  UNLOCK
  llCUpdate = .T.
  SHOW GET laData[28]
  =lfRefresh(lcWinCh2)
ENDIF

*!*************************************************************
*! Name      : lfvRep2
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate second salesrep
*!*************************************************************
*! Calls     : REPCHK
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRep2()
*!*************************************************************
FUNCTION lfvRep2
IF llBrowse .OR. (!EMPTY(laData[29]) .AND. !SEEK(laData[29],'SalesRep'))
  laData[29] = IIF(llBrowse,SPACE(3),laData[29])
  lcRep = laData[29]
  DO REPCHK WITH lcRep
  laData[29] = lcRep
ENDIF  
llBrowse = .F.
IF &lcOrdHdr..Rep2 <> laData[29]
  *B803375,1 Replace comm2 with defa. comm for rep2 AME[start] 
  *laData[30] = IIF(EMPTY(laData[29]),0,laData[30])
  IF laSetups[11,2]='D'
    =IIF(laSetups[11,2]='D',gfOpenFile(gcDataDir+'REP_DIV',gcDataDir+'REP_DIV','SH'),.T.)    
    IF SEEK(laData[29]+laData[15],'REP_DIV')
      STORE REP_DIV.Comm_Rate TO laData[30],lnSouComm2
    ELSE
      laData[30] = IIF(EMPTY(laData[29]),0,SalesRep.Comm)
    ENDIF
  ELSE
    laData[30] = IIF(EMPTY(laData[29]),0,IIF(Customer.salesrep # laData[29],SalesRep.Comm,CUSTOMER.Comm2))
  ENDIF
  SELECT (lcOrdLine)
  lnRecno = RECNO()
  IF SEEK(&lcOrdHdr..CordType+&lcOrdHdr..Order)
    SCAN WHILE CordType+Order+STR(LineNo,6) = &lcOrdHdr..CordType+&lcOrdHdr..Order
      M.Comm2 = laData[30]
      =lfVComm('2')
  	ENDSCAN  
  ENDIF  
  SELECT (lcOrdLine)
  GO TOP
  IF !EOF(lcOrdLine)
    GO lnRecNo
  ENDIF
  =IIF(laSetups[11,2]='D',gfCloseFile('REP_DIV'),.T.)  
  SELECT (lcOrdHdr)
  *B803375,1 AME[End]
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE Rep2  WITH laData[29] ,;
          Comm2 WITH laData[30]
  UNLOCK
  llCUpdate = .T.
  SHOW GET laData[30]
  =lfRefresh(lcWinCh2)
ENDIF

*!*************************************************************
*! Name      : lfvWareCode
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate order warehouse code
*!*************************************************************
*! Calls     : gfBrowWare
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvWareCode()
*!*************************************************************
FUNCTION lfvWareCode

laData[31] = laWareHouses[lnWareHouse,2]
IF &lcOrdHdr..cWareCode <> laData[31]
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE cWareCode WITH laData[31]
  UNLOCK
  llCUpdate = .T.
ENDIF

*!*************************************************************
*! Name      : lfvCurrency
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid function for currency fields
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCurrency()
*!*************************************************************
FUNCTION lfvCurrency
PRIVATE lcCurrency

IF llBrowse .OR.  laData[33] <> &lcOrdHdr..cCurrCode
  lcCurrency = laData[33]
  *E301077,5 Inhance openning files to speed up transaction
  =gfOpenFile(gcSysHome+'SycCurr',gcSysHome+'cCurrCode','SH')
  *E301077,5 (End)

  IF !gfCurrBrow(@lcCurrency)
    lcCurrency  = &lcOrdHdr..cCurrCode
  ENDIF
  laData[33] = lcCurrency
  IF laData[33] <> &lcOrdHdr..cCurrCode
    IF laData[33] = gcBaseCurr
      STORE 1 TO laData[50], laData[34]
    ELSE
      laData[34] = gfChkRate('laData[50]',laData[33],laData[8],.T.,gcAct_Comp,.F.,.T.)
      IF laData[34] = 0
        *E300420,1 Message : 00262
        *E300420,1 A valid xxx to xxx exchange rate could not be found on xxx.
        *E300420,1 Button : 00000
        *E300420,1 Ok
        =gfModalGen('INM00262B00000','ALERT',ALLTRIM(laData[33])+'|'+ALLTRIM(gcBaseCurr)+'|'+DTOC(laData[8]))
        laData[33] = &lcOrdHdr..cCurrCode
        laData[34] = &lcOrdHdr..nExRate
      ENDIF  
    ENDIF
    llUpdated = .T.
    SELECT (lcOrdHdr)
    =RLOCK()    
    REPLACE cCurrCode WITH laData[33] ,;
            nExRate   WITH laData[34] ,;
            nCurrUnit WITH laData[50]
    UNLOCK
    llCUpdate = .T.
  ENDIF  
  *E301077,5 Inhance openning files to speed up transaction
  =gfCloseFile('SycCurr')
  *E301077,5 (End)
  IF !llEditExRt OR laData[33]=gcBaseCurr
    SHOW GET laData[34] DISABLE
  ELSE
    SHOW GET laData[34] ENABLE
  ENDIF
ENDIF    
llBrowse = .F.

*!*************************************************************
*! Name      : lfvExRate
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid function for currency exchange rate.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvExRate()
*!*************************************************************
FUNCTION lfvExRate

IF laData[34] <=0
  *E300420,1 Message : 32028
  *E300420,1 The exchange rate must be greater than zero
  *E300420,1 Button : 00000 
  *E300420,1 Ok
  =gfModalGen('TRM32028B00000','ALERT')
  laData[34] = &lcOrdHdr..nExRate
ENDIF
IF &lcOrdHdr..nExRate <> laData[34]
  SELECT (lcOrdHdr)
  =RLOCK()  
  REPLACE nExRate WITH laData[34]
  UNLOCK
  llCUpdate = .T.
ENDIF

*!*************************************************************
*! Name      : lfvNew
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : New Order line
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvNew()
*!*************************************************************
FUNCTION lfvNew
PARAMETERS llStores
*B603255,1 WAB - save last order no and last order line 
*B603255,1 WAB - START
IF SEEK("SO" + m.Order + STR(m.LineNo,6),lcT_BomVar)  
  lclastOrder= m.Order 
  lcLastLine = STR(m.LineNo,6)
ENDIF
*B603255,1 WAB - END
STORE .T. TO llNewline,llAddLine
SCATTER MEMVAR BLANK MEMO
m.Style = lcLastStyle
m.Store = laData[3]
=SEEK(m.Style,'STYLE')
=SEEK(m.Style+laData[31],'STYDYE')
IF llStores
  SHOW GETS WINDOW 'SOMULTI2' DISABLE ONLY
  SHOW GETS WINDOW 'SOMULTI3' DISABLE ONLY
  SHOW GETS WINDOW 'SOMULTI4' DISABLE ONLY
  SHOW GETS WINDOW 'SOMULTI5' DISABLE ONLY
  =lfRefresh('SOMULTI2')
  =lfRefresh('SOMULTI3')
  =lfRefresh('SOMULTI4')
ELSE
  SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY
  SHOW GETS WINDOW (lcWinCh321) DISABLE ONLY
  SHOW GETS WINDOW (lcWinCh322) DISABLE ONLY
  =lfRefresh(lcWinCh32)
  =lfRefresh(lcWinCh321)
ENDIF
IF llMultiSt .AND. !llStores
  m.Store = lcLastStore
  SHOW GET ibLStore ENABLE
  SHOW GET m.Store  ENABLE
  _CUROBJ = OBJNUM(m.Store)
ELSE
  SHOW GET ibStyle ENABLE
  SHOW GET m.Style ENABLE
  _CUROBJ = OBJNUM(m.Style)
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfwCustPo
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : When function for order lines CustPOs
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfwCustPo()
*!*************************************************************
FUNCTION lfwCustPo
lcOldCustPo = CustPo

*!*************************************************************
*! Name      : FUNCTION lfvLCustPo
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate order lines CustPOs
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvLCustPo()
*!*************************************************************
FUNCTION lfvLCustPo
PRIVATE lnCurrTag,lcCurrExp,lcStore,lcCustPo

IF CustPo <> lcOldCustPo
  llCUpDate = .T.
  *E300420,1 Message : 32046
  *E300420,1 Change Custpo# for all lines in store xxxxx.
  *E300420,1 Button : 32000
  *E300420,1 Yes;No
  IF gfModalGen('QRM32046B32000','ALERT',store)=1
    lnCurrTag = VAL(SYS(21))
    lcCurrExp = EVAL(SYS(14,lnCurrTag))
    lcStore  = Store
    lcCustPo = CustPo
    SET ORDER TO TAG ORDLINST
    =SEEK(lcOrdType+laData[1]+lcStore)
    SCAN REST WHILE cOrdType+ORDER+STORE = lcOrdType+laData[1]+lcStore
      =RLOCK()
      REPLACE CustPo WITH lcCustPo ,;
              FLAG   WITH IIF(FLAG <> 'N','M',FLAG)
      UNLOCK
    ENDSCAN
    SET ORDER TO TAG lnCurrTag
    =SEEK(lcCurrExp)
  ELSE
    =RLOCK()
    REPLACE FLAG WITH IIF(FLAG <> 'N','M',FLAG)
    UNLOCK
  ENDIF
ENDIF
RETURN

*!*************************************************************
*! Name      : lfvLStore
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Validate order line store
*!*************************************************************
*! Calls     : CUSBROW
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvLStore()
*!*************************************************************
FUNCTION lfvLStore
IF MDOWN() .AND. !llBrowse
  RETURN
ENDIF
IF llBrowse .OR. ;
   ( (!llAddLine .OR. !EMPTY(m.Store)) .AND. !SEEK('S'+laData[2]+m.Store,'Customer'))
  xStore = m.Store
  *B801941,1 Fix store browse
  SELECT CUSTOMER
  *B801941,1 (End)

  IF !CUSBROWS(laData[2],.T.)
    STORE IIF(llAddLine,SPACE(8),Store) TO m.Store
  ELSE
    m.Store = xStore
  ENDIF
ENDIF
*B602653,1 Update order line store modification
SELECT (lcOrdLine)
*B602653,1 (End)
llBrowse = .F.
IF !EMPTY(m.Store) .AND. Customer.Status <> 'A'
  *B803068,1 ABD Change the sales order screen to allow to make an order for
  *B803068,1 ABD status 'Potential' of accounts. [ Begin ]
  IF Customer.Status <> 'P'
    *B803068,1 ABD [ End ]
     *E300420,1 Message : 32023
    *E300420,1 This a non-active xxxxx. Order entry is not allowed!
    *E300420,1 Button : 00000
    *E300420,1 Ok
    =gfModalGen('TRM32023B00000','ALERT','store'+'|'+IIF(lcOrdType='C','Contract','Order'))
    STORE IIF(llAddLine,SPACE(8),Store) TO m.Store
    *B803068,1 ABD Endif for if statement [Begin]
  ENDIF   
  *B803068,1 ABD [ End ]
ENDIF
IF !llAddLine .AND. m.Store=Store
  RETURN
ENDIF
IF EMPTY(m.Store)
  SCATTER MEMVAR MEMO
  STORE .F.       TO llAddLine,llNewLine
  STORE SPACE(8) TO lcLastStore
  IF EMPTY(m.Store)
    SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh321) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh322) DISABLE ONLY
    SHOW GET pbNew      ENABLE
    SHOW GET pbPacks    ENABLE
    SHOW GET pbTemplate ENABLE 
    _CUROBJ = OBJNUM(pbNew)
  ELSE
    SHOW GETS WINDOW (lcWinCh32)  ENABLE ONLY
    SHOW GETS WINDOW (lcWinCh321) ENABLE ONLY
    SHOW GETS WINDOW (lcWinCh322) ENABLE ONLY
    _CUROBJ = OBJNUM(ibBrowOrd)
  ENDIF
  RETURN
ELSE
  IF llAddLine 
    SHOW GET ibStyle ENABLE
    SHOW GET m.Style ENABLE
    _CUROBJ = OBJNUM(m.Style)
  ELSE
    llCUpDate = .T.
    =RLOCK()
    REPLACE Store WITH m.Store ,;
            FLAG WITH IIF(FLAG <> 'N','M',FLAG)
    *B603255,1 WAB - CALl FUNCTION TO add adornment to new line
    *B603255,1 WAB - START
    =lfAdrLine()
    *B603255,1 WAB - END
    UNLOCK
  ENDIF
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfvStyle
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate entered Styles
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvStyle()
*!*************************************************************
FUNCTION lfvStyle
PARAMETERS llStores

IF MDOWN() .AND. !llBrowse
  RETURN
ENDIF
SELECT IIF(llStores,lcTempline,lcOrdLine)

*B603606,1 ABD- Prevent the user changing or deleteing style has allocated 
*B603606,1 ABD- Or shipped quantities
IF (!lladdline .AND. llBrowse) .OR. laScrMode [3] .AND. !lladdline .AND. Style <> m.Style 
  DO CASE
    CASE PICKED 
     *B603606,1 Message : 32079
     *B603606,1 Order line already XXXX ; Cannot modify style.
     *B603606,1 Button  : 00000
     *B603606,1 Ok
     =gfModalGen('INM32079B00000','ALERT','piked')
     STORE Style TO m.Style
     _CUROBJ = OBJNUM(m.Style)
     llBrowse = .F.  
     RETURN

    CASE TOtQty <> Totbook
      =gfOpenFile(gcDataDir+'ORDCANLN',gcDataDir+'ORDCANLN','SH')
      SELECT IIF(llStores,lcTempline,lcOrdLine)
      IF !SEEK('O'+Order+STR(lineno,6),'Ordcanln') .AND. Totbook <> Ordcanln.TotQty
        *B603606,1 Message : 32079
        *B603606,1 Order line already XXXX ; Cannot modify style.
        *B603606,1 Button  : 00000
        *B603606,1 Ok
        =gfModalGen('INM32079B00000','ALERT','shiped')
        =gfCloseFile('ORDCANLN')
        STORE Style TO m.Style
        _CUROBJ = OBJNUM(m.Style)
        llBrowse = .F.  
        RETURN
     ENDIF
  ENDCASE
  
ENDIF
*B603606,1 ABD- [End]



*C101493,1 AMM Get the major title and length
lcMaj      = gfItemMask('PM')             && Get the major of the style
lnMajSize  = LEN(lcMaj)                   && Length of the major
*B802436,1 AMM Get the item picture
lnItemSz   = gfItemMask('PI')
*B802436,1 AMM end
llExtSS    = gfGetMemVar('M_USEEXSSC')
llCallEx   = .F.
*IF llBrowse .OR. ;
   ( (!llAddLine .OR. m.Style <> lcEmptySty) .AND. !SEEK(m.Style,'Style'))
IF llBrowse .OR. ;
  (!llAddLine .OR. m.Style <> lcEmptySty) 
  *C101493,1 AMM If the style isn't exist.
  IF !SEEK(ALLTRIM(m.Style),'Style'))
  *C101493,1 AMM end
    m.Style = PADR(gfStyBrw("I" , m.Style , "" , .F.),19)
    IF EMPTY(m.Style)
      STORE IIF(llAddLine,lcEmptySty,Style) TO m.Style
      _CUROBJ = OBJNUM(m.Style)
    ELSE
      *C101493,1 AMM Call the ext. size scale screen in case of the system is Using extrended size scale
      llCallEx = llExtSS .AND. !llStores
      IF llCallEx .AND. llExtSS
        *C101493,1 AMM Remove the scale part of the style chosen, to display this style with all its sizes
        m.Style = LEFT(m.STYLE,LEN(m.Style)-3)
      ENDIF
    ENDIF
    llBrowse = .F.
  *C101493,1 AMM If the style exist.
  ELSE
    *C101493,1 AMM If the user typed part of the style
    *B802436,1 AMM start
    *IF !llStores .AND. m.Style # Style.Style .AND. LEN(ALLTRIM(m.STYLE)) > lnMajSize 
    llOneCol = .F.
    IF m.Style # Style.Style .AND. LEFT(m.STYLE,lnMajSize) = LEFT(Style.Style ,lnMajSize)
      SKIP 1 IN STYLE
      IF LEFT(m.STYLE,lnMajSize) # LEFT(Style.Style ,lnMajSize)
        llOneCol = .T.
      ENDIF
      SKIP -1 IN STYLE
      m.Style  = IIF(llOneCol,STYLE.STYLE,m.Style)
    ENDIF
     
    IF !llOneCol
      IF !llStores .AND. m.Style # Style.Style .AND. llExtSS .AND. LEN(ALLTRIM(m.STYLE)) = LEN(lnItemSz)-3
      *B802436,1 AMM end
        llCallEx = .T.
      ELSE
        m.Style = PADR(gfStyBrw("I" , m.Style , "" , .F.),19)
        IF EMPTY(m.Style)
          STORE IIF(llAddLine,lcEmptySty,Style) TO m.Style
          _CUROBJ = OBJNUM(m.Style)
        *B802436,1 AMM Comment out
        *ELSE
          *llCallEx = llExtSS .AND. !llStores
          *IF llCallEx .AND. llExtSS
            *C101493,1 AMM Remove the scale part of the style chosen, to 
            *C101493,1 AMM display this style with all its sizes
            *m.Style = LEFT(m.STYLE,LEN(m.Style)-3)
          *ENDIF
          *B802436,1 AMM end
        ENDIF
        llBrowse = .F.
      ENDIF
    *B802436,1 AMM (Start)
    ENDIF
    *B802436,1 AMM end

  ENDIF
  IF llCallEx
    lnBrR    = 0
    *C101493,1 AMM Create the numeric fields array, to make character fields 
    *C101493,1 AMM corresponding to them
    DIMENSION laQtyArr[10,3]
    laQtyArr[1,1]  = 'QTY1'
    laQtyArr[1,2]  = 6
    laQtyArr[1,3]  = 0
    laQtyArr[2,1]  = 'QTY2'
    laQtyArr[2,2]  = 6
    laQtyArr[2,3]  = 0
    laQtyArr[3,1]  = 'QTY3'
    laQtyArr[3,2]  = 6
    laQtyArr[3,3]  = 0
    laQtyArr[4,1]  = 'QTY4'
    laQtyArr[4,2]  = 6
    laQtyArr[4,3]  = 0
    laQtyArr[5,1]  = 'QTY5'
    laQtyArr[5,2]  = 6
    laQtyArr[5,3]  = 0
    laQtyArr[6,1]  = 'QTY6'
    laQtyArr[6,2]  = 6
    laQtyArr[6,3]  = 0
    laQtyArr[7,1]  = 'QTY7'
    laQtyArr[7,2]  = 6
    laQtyArr[7,3]  = 0
    laQtyArr[8,1]  = 'QTY8'
    laQtyArr[8,2]  = 6
    laQtyArr[8,3]  = 0
    laQtyArr[9,1]  = 'TOTQTY'
    laQtyArr[9,2]  = 7
    laQtyArr[9,3]  = 0
    *B802585,1 AMM Treat these commented fields as numeric not characters
    *laQtyArr[10,1] = 'GROS_PRICE'
    *laQtyArr[10,2]  = 8
    *laQtyArr[10,3]  = 2
    *laQtyArr[11,1] = 'DISC_PCNT'
    *laQtyArr[11,2]  = 6
    *laQtyArr[11,3]  = 2
    *laQtyArr[12,1] = 'PRICE'
    *laQtyArr[12,2]  = 9
    *laQtyArr[12,3]  = 2
    *laQtyArr[13,1] = 'PPQTY'
    *laQtyArr[13,2]  = 4
    *laQtyArr[13,3]  = 0
    *laQtyArr[14,1] = 'COMM1'
    *laQtyArr[14,2]  = 5
    *laQtyArr[14,3]  = 2
    *laQtyArr[15,1] = 'COMM2'
    *laQtyArr[15,2]  = 5
    *laQtyArr[15,3]  = 2
    
    laQtyArr[10,1]  = 'PPQTY'
    laQtyArr[10,2]  = 4
    laQtyArr[10,3]  = 0
    *B802585,1 AMM end
    
    DIMENSION laTitArr[6,2]
    laTitArr[1,1] = 'STYLE'
    laTitArr[1,2] = lcStyHdr
    laTitArr[2,1] = 'SCALE'
    laTitArr[2,2] = 'S'
    laTitArr[3,1] = 'CPrepak'
    laTitArr[3,2] = 'P'
    laTitArr[4,1] = 'CPPQTY'
    laTitArr[4,2] = 'P. Qty.'
    *B802585,1 AMM No titels for these numeric fields
    *laTitArr[5,1] = 'CGROS_PRICE'
    *laTitArr[5,2] = 'GPrice'
    *laTitArr[6,1] = 'CDISC_PCNT'
    *laTitArr[6,2] = 'Disc'
    *laTitArr[7,1] = 'CPRICE'
    *laTitArr[7,2] = 'Price'
    *laTitArr[8,1] = 'CCOMM1'
    *laTitArr[8,2] = 'CM1'
    *laTitArr[9,1] = 'CCOMM2'
    *laTitArr[9,2] = 'CM2'
    *laTitArr[10,1] = 'PACK_ID'
    *laTitArr[10,2] = 'Pack'
    *laTitArr[11,1] = 'Group'
    *laTitArr[11,2] = 'Group'
    laTitArr[5,1] = 'PACK_ID'
    laTitArr[5,2] = 'Pack'
    laTitArr[6,1] = 'Group'
    laTitArr[6,2] = 'Group'
    *B802585,1 AMM end
    
    *C101493,1 AMM Form the browse string of ext. size scale screen
    *B802436,1 AMM 
    lcQtyPic = '999999'
    *lcBrowStr = "cMk=IIF(RECNO()=lnBrR,'',' '):1:H=' ':W=.F.,Style:H=lcStyHdr:R,Prepak:H='P':W=lfwOV():V=lfvPrePack(.F.,.T.):P='!'," +;
      "cPpQty:H='P. Qty.':W=lfwOV():V=lfCtoN().AND.lfvPPakQty(.F.,.T.):P='9999'," +;
      "cTotQty:H='Pieces':W=lfwOV():P='9999999':V=lfCtoN().AND.lfvPieces(.F.,.T.)," 
    *B802585,1 AMM (Start)
    *lcBrowStr = "cMk=IIF(RECNO()=lnBrR,'',' '):1:H=' ':W=.F.,Scale :H='Scl':R,Prepak:H='P':W=lfwOV():V=lfvPrePack(.F.,.T.):P='!'," +;
      "cPpQty:H='P. Qty.':W=lfwOV():V=lfCtoN().AND.lfvPPakQty(.F.,.T.):P='9999'," +;
      "cTotQty:H='Pieces':W=lfwOV():P='9999999':V=lfCtoN().AND.lfvPieces(.F.,.T.)," 
    lcBrowStr = "cMk=IIF(RECNO()=lnBrR,'',' '):1:H=' ':W=.F.,Scale :H='Scl':R:W=.F.,Prepak:H='P':W=lfwOV():V=lfvPrePack(.F.,.T.):P='!'," +;
      "cPpQty:H='P. Qty.':W=lfwOV():V=lfCtoN().AND.lfvPPakQty(.F.,.T.):P='9999'," +;
      "cTotQty:H='TOT QTY':W=lfwOV():P='9999999':V=lfCtoN().AND.lfvPieces(.F.,.T.)," 
    *B802585,1 AMM end
    *B802436,1 AMM end 
    
    *B802585,1 AMM (start)
    *lcBrowStr = lcBrowStr +;
      "Pack_id:H='Pack':W=lfwOV():V=lfvStyPack(.F.,.T.) , Group :P='!' :V=lfvf(),"+;
      "cQty1:H='Qty1':V=lfCtoN():P=lcQtyPic,cQty2:H='Qty2':W=laScl[2]:V=lfCtoN():P=lcQtyPic,cQty3:H='Qty3':W=laScl[3]:V=lfCtoN():P=lcQtyPic,cQty4:H='Qty4':W=laScl[4]:V=lfCtoN():P=lcQtyPic,"
    lcBrowStr = lcBrowStr +;
      "cQty1:H='Qty1':V=lfCtoN():P=lcQtyPic, cQty2:H='Qty2':W=laScl[2]:V=lfCtoN():P=lcQtyPic,cQty3:H='Qty3':W=laScl[3]:V=lfCtoN():P=lcQtyPic,cQty4:H='Qty4':W=laScl[4]:V=lfCtoN():P=lcQtyPic,"
    *B802585,1 AMM end
    *E301313,1 AMM Edit price and discount due to the system setting
    *lcBrowStr = lcBrowStr +;
      "cQty5:H='Qty5':W=laScl[5]:V=lfCtoN():P=lcQtyPic,cQty6:H='Qty6':W=laScl[6]:V=lfCtoN():P=lcQtyPic,cQty7:H='Qty7':W=laScl[7]:V=lfCtoN():P=lcQtyPic,cQty8:H='Qty8':W=laScl[8]:V=lfCtoN():P=lcQtyPic," +;
      "cGros_Price:H='GPrice':W=lfwOV():V=lfCtoN(8,2).AND.lfvGPrice(.F.,.T.):P='999999.99',"  +;
      "CDISC_PCNT:H='Disc':W=lfwOV():V=lfCtoN(6,2).AND.lfvPrcDisc(.F.,.T.):P='9999.99',cPrice:H='Price':W=lfwOV():V=lfCtoN(9,2).AND.lfvNPrice(.F.,.F.,.T.):P='9999999.99'" +;
      IIF(EMPTY(laData[27]), "" , ",cComm1:H='Rep':W=lfwOV():V=lfCtoN(5,2).AND.lfvComm('1',.F.,.T.):P='999.99'" ) +;
      IIF(EMPTY(laData[29]),"",",cComm2:H='Rep2':W=lfwOV():V=lfCtoN(5,2).AND.lfvComm('2',.F.,.T.):P='999.99'")
    *B802585,1 AMM (start)
    *lcBrowStr = lcBrowStr +;
      "cQty5:H='Qty5':W=laScl[5]:V=lfCtoN():P=lcQtyPic,cQty6:H='Qty6':W=laScl[6]:V=lfCtoN():P=lcQtyPic,cQty7:H='Qty7':W=laScl[7]:V=lfCtoN():P=lcQtyPic,cQty8:H='Qty8':W=laScl[8]:V=lfCtoN():P=lcQtyPic," +;
      "cGros_Price:H='GPrice':W=lfwOV().AND.llESTYPSO:V=lfCtoN(8,2).AND.lfvGPrice(.F.,.T.):P='999999.99',"  +;
      "CDISC_PCNT:H='Disc':W=lfwOV().AND.llESTYPSO:V=lfCtoN(6,2).AND.lfvPrcDisc(.F.,.T.):P='9999.99',cPrice:H='Price':W=lfwOV().AND.llESTYPSO:V=lfCtoN(9,2).AND.lfvNPrice(.F.,.F.,.T.):P='9999999.99'" +;
      IIF(EMPTY(laData[27]), "" , ",cComm1:H='Rep':W=lfwOV():V=lfCtoN(5,2).AND.lfvComm('1',.F.,.T.):P='999.99'" ) +;
      IIF(EMPTY(laData[29]),"",",cComm2:H='Rep2':W=lfwOV():V=lfCtoN(5,2).AND.lfvComm('2',.F.,.T.):P='999.99'")
    lcBrowStr = lcBrowStr +;
      "cQty5:H='Qty5':W=laScl[5]:V=lfCtoN():P=lcQtyPic,cQty6:H='Qty6':W=laScl[6]:V=lfCtoN():P=lcQtyPic,cQty7:H='Qty7':W=laScl[7]:V=lfCtoN():P=lcQtyPic,cQty8:H='Qty8':W=laScl[8]:V=lfCtoN():P=lcQtyPic," +;
      "Gros_Price:H='GPrice':W=lfwOV().AND.llESTYPSO:V=lfvF(),"  +;
      "DISC_PCNT:H='Disc':W=lfwOV().AND.llESTYPSO:V=lfvF() ,Price:H='Price':W=lfwOV().AND.llESTYPSO:V=lfvF()" +;
      IIF(EMPTY(laData[27]), "" , ",Comm1:H='Rep':W=lfwOV():V=lfvF()" ) +;
      IIF(EMPTY(laData[29]),"",",Comm2:H='Rep2':W=lfwOV():V=lfvF()")+;
      IIF(laSetups[1,2]='Y',",Pack_id:H='Pack':W=lfwOV():V=lfvStyPack(.F.,.T.)",'')+;
      ",Group :P='!' :V=lfvf():F"
    *B802585,1 AMM end
    *E301313,1 AMM end

    SET ORDER TO TAG ORDLINST IN (lcOrdLine)
    *C101493,1 AMM Run the screen to edit all styles begin with what the user typed
    *C101493,1 AMM Parameters:
    *C101493,1 AMM 1- m.style -> the part of the style the user typed
    *C101493,1 AMM 2- Store , if the order is multi store
    *C101493,1 AMM 3- temporary file the SO screen work with.
    *C101493,1 AMM 4- Browse string
    *C101493,1 AMM 5- Seek expression
    *C101493,1 AMM 6- Quantity array (Array holds the numeric fields in the called screen browse )
    *C101493,1 AMM 7- Title array (each field with its title in the called screen browse)
      
    *B802436,1 AMM start
    *DO (gcAppHome+gcWinAppl+'\SOESSQTY.FXP') WITH ALLTRIM(m.Style),m.store,;
     IIF(llStores,lcTempline,lcOrdLine), lcBrowStr,lcOrdType+laData[1]+m.Store,laQtyArr,laTitArr
    DO (gcAppHome+'SOESSQTY.FXP') WITH ALLTRIM(m.Style),m.store,;
     IIF(llStores,lcTempline,lcOrdLine), lcBrowStr,lcOrdType+laData[1]+m.Store,laQtyArr,laTitArr,'SO'
    *B802436,1 AMM end
     SELECT IIF(llStores,lcTempline,lcOrdLine)
    llReBrowse = .T.
    =lfActFolder()
    RETURN
  ENDIF
  *C101493,1 AMM end
ENDIF
IF !llAddLine .AND. m.Style=Style
  RETURN
ENDIF
IF m.Style=lcEmptySty
  STORE m.Style TO lcLastStyle
  SCATTER MEMVAR MEMO
  STORE .F.       TO llAddLine,llNewLine
  STORE SPACE(08) TO lcLastStore
  IF EMPTY(m.Style)
    IF llStores
      SHOW GETS WINDOW 'SOMULTI2' DISABLE ONLY
      SHOW GETS WINDOW 'SOMULTI3' DISABLE ONLY
      SHOW GETS WINDOW 'SOMULTI4' DISABLE ONLY
      SHOW GETS WINDOW 'SOMULTI5' DISABLE ONLY
      SHOW GET ibInv200E ENABLE
      SHOW GET pbClose   ENABLE
    ELSE
      SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY
      SHOW GETS WINDOW (lcWinCh321) DISABLE ONLY
      SHOW GETS WINDOW (lcWinCh322) DISABLE ONLY
      IF llMultiSt 
        SHOW GET pbTemplate ENABLE 
      ENDIF
    ENDIF
    SHOW GET pbNew   ENABLE
    SHOW GET pbPacks ENABLE
    _CUROBJ = OBJNUM(pbNew)
  ELSE
    IF llStores
      SHOW GETS WINDOW 'SOMULTI2' ENABLE ONLY
      SHOW GETS WINDOW 'SOMULTI3' ENABLE ONLY
      SHOW GETS WINDOW 'SOMULTI4' ENABLE ONLY
      SHOW GETS WINDOW 'SOMULTI5' ENABLE ONLY
      =lfAdjButt('')
    ELSE
      SHOW GETS WINDOW (lcWinCh32)  ENABLE ONLY
      SHOW GETS WINDOW (lcWinCh321) ENABLE ONLY
      SHOW GETS WINDOW (lcWinCh322) ENABLE ONLY
      IF !llMultiSt
        SHOW GET pbTemplate DISABLE
        SHOW GET ibLStore   DISABLE
        SHOW GET m.Store    DISABLE
      ENDIF
    ENDIF
    IF lContract
      SHOW GET m.Price      DISABLE
      SHOW GET m.Gros_Price DISABLE
      SHOW GET m.Disc_Pcnt  DISABLE
    ENDIF
    _CUROBJ = IIF(llStores,OBJNUM(ibInv200E),OBJNUM(ibBrowOrd))
  ENDIF
  RETURN
ENDIF

IF SEEK(m.Style,'Style')
  *E300408,1 Message : 32017
  *E300408,1 Style scale not found in the scale file. Cannot accept.
  *E300408,1 Button : 00000 
  *E300408,1 Ok

  *E300408,1 Message : 32018
  *E300408,1 This is a canceled style. Cannot accept.
  *E300408,1 Button : 00000 
  *E300408,1 Ok

  *E300408,1 Message : 32019
  *E300408,1 This style/color is on hold. 
  *E300408,1 Button : 32003
  *E300408,1 Accept Reenter

  *E300408,1 Message : 32020
  *E300408,1 Styles restricted to XXX!
  *E300408,1 Button : 00000
  *E300408,1 Ok

  *E300408,1 Message : 32021
  *E300408,1 Style XXX date is 
  *E300408,1 Button : 32003
  *E300408,1 Accept Reenter

  SET RELATION OFF INTO STYLE
  *B602820,1 Changed the button number from B3003 to B32003 in the follwing
  *B602820,1 If command when checking for the style status is Hold.
  *IF (!SEEK('S'+Style.Scale,'Scale') .AND. ;
      gfModalGen('TRM32017B00000','ALERT')=1) ;
   .OR. (Style.Status='X' .AND. ;
        gfModalGen('TRM32018B00000','ALERT')=1) ;
   .OR. (Style.Status='H' .AND. ;
        gfModalGen('QRM32019B32003','ALERT')=2) ;
   .OR. (Style.cDivision <> laData[15] .AND. ;
        gfModalGen('TRM32020B00000','ALERT','division '+ALLTRIM(laDivision[lnDivision,1]))=1) ;
   .OR. (ALLTRIM(laData[14])<>'*' AND TRIM(Style.Season)<>'Y' AND Style.Season<>laData[14] .AND. ;
        gfModalGen('TRM32020B00000','ALERT','season '+ALLTRIM(laSeasons[lnSeason,1]))=1) ;
   .OR. (!EMPTY(Style.Start) .AND. Style.Start > laData[9] .AND. ;
        gfModalGen('QRM32021B32003','ALERT','start|'+DTOC(Style.Start))=2) ;
   .OR. (!EMPTY(Style.SoldOut) .AND. Style.SoldOut < laData[9] .AND. ;
        gfModalGen('QRM40010B40001','ALERT','sold out|'+DTOC(Style.SoldOut))=2)
  IF (!SEEK('S'+Style.Scale,'Scale') .AND. ;
      gfModalGen('TRM32017B00000','ALERT')=1) ;
   .OR. (Style.Status='X' .AND. ;
        gfModalGen('TRM32018B00000','ALERT')=1) ;
   .OR. (Style.Status='H' .AND. ;
        gfModalGen('QRM32019B32003','ALERT')=2) ;
   .OR. (Style.cDivision <> laData[15] .AND. ;
        gfModalGen('TRM32020B00000','ALERT','division '+ALLTRIM(laDivision[lnDivision,1]))=1) ;
   .OR. (ALLTRIM(laData[14])<>'*' AND TRIM(Style.Season)<>'Y' AND Style.Season<>laData[14] .AND. ;
        gfModalGen('TRM32020B00000','ALERT','season '+ALLTRIM(laSeasons[lnSeason,1]))=1) ;
   .OR. (!EMPTY(Style.Start) .AND. Style.Start > laData[9] .AND. ;
        gfModalGen('QRM32021B32003','ALERT','start|'+DTOC(Style.Start))=2) ;
   .OR. (!EMPTY(Style.SoldOut) .AND. Style.SoldOut < laData[9] .AND. ;
        gfModalGen('QRM40010B40001','ALERT','sold out|'+DTOC(Style.SoldOut))=2)
    *B602820,1 End.
    STORE IIF(llAddLine,lcEmptySty,Style) TO m.Style

    SET RELATION TO STYLE INTO STYLE ADDITIVE
    _CUROBJ = OBJNUM(m.Style)
    RETURN
  ENDIF
  *E300834,1 Get style price according to ordered quantity
  *m.Gros_Price = IIF(!llMulCurr .OR. laData[33]=gcBaseCurr,Style.Price&lcPriceLvl,;
                   gfStyPrice(m.Style,lcPriceLvl,laData[33]))
  *E301086,1 Add new Type 'T' for EDI temporary Order 
  *m.lContract = lcOrdType='O' .AND. lfvContPri(m.Style,'m.Gros_Price','m.Price','m.Disc_Pcnt')
  m.lContract = lcOrdType<>'C' .AND. lfvContPri(m.Style,'m.Gros_Price','m.Price','m.Disc_Pcnt')
  IF !m.lContract
    *E301142,1 Select alternative price level
    *lcPriceCatg  = IIF(lcPriceLvl='Q',IIF(m.TotQty>=Style.nAtQtyC,'C',;
    *                   IIF(m.TotQty>=Style.nAtQtyB,'B','A')),lcPriceLvl)
    *m.Gros_Price = IIF(!llMulCurr OR laData[33]=gcBaseCurr,Style.Price&lcPriceCatg,;
    *                   gfStyPrice(m.Style,lcPriceCatg,laData[33]))
    m.Gros_Price = lfGetprice(m.Style,lcPriceLvl,m.TotQty)
    *E301142,1 (End)
  ENDIF
  *E300834,1 (End)
  
  SELECT IIF(llStores,lcTempline,lcOrdLine)
  IF m.Gros_Price < 0
    STORE IIF(llAddLine,lcEmptySty,Style) TO m.Style
    SET RELATION TO STYLE INTO STYLE ADDITIVE
    _CUROBJ = OBJNUM(m.Style)
    RETURN
  ENDIF
  IF laSetups[5,2]='Y' .AND. !SEEK(m.Style+laData[31]+SPACE(10),'StyDye')
    *E300408,1 Message : 40012
    *E300408,1 Style/color xxx is not assigned to warehouse xxx
    *E300408,1 Button : 40002
    *E300408,1 Add Reenter
    IF gfModalGen('QRM40012B40002','ALERT',TRIM(m.Style)+'|'+TRIM(laData[31]))=1
      DO gpAdStyWar WITH m.Style,SPACE(10),laData[31]
    ELSE
      STORE IIF(llAddLine,lcEmptySty,&lcOrdLine..Style) TO m.Style
      SET RELATION TO STYLE INTO STYLE ADDITIVE
      _CUROBJ = OBJNUM(m.Style)
      RETURN
    ENDIF
  ENDIF
  m.Desc1    = Style.Desc1
  m.Scale    = Style.Scale
  m.PrePak   = Style.PrePak
  m.Comm1    = IIF(Style.Commission,laData[28],0)
  m.Comm2    = IIF(Style.Commission,laData[30],0) 
  m.Season   = Style.Season
  *B802324,1 Read field insted of variable.
  *m.Gl_Sales = lcGlSales+Style.cSlsGlLink
  *B803739,1 (Begin) Remove all spaces from LADATA[53].
  *m.Gl_Sales = LADATA[53]+Style.cSlsGlLink
  m.Gl_Sales = ALLTRIM(laData[53])+Style.cSlsGlLink
  *B803739,1 (End)
  *B802324,1 End.
  
  m.Gl_Sales = IIF(laSetups[4,2]='Y' AND SEEK('02'+m.Gl_Sales,'Gl_Link'),m.Gl_Sales,'DEFDEF')
  m.Flag     = IIF(llStores,'',IIF(llAddLine OR m.Flag='N','N','M'))
  *E300834,1 Get Style discount percent and Calculate net price
  IF !m.lContract
    lnDisc_Pcnt = m.Disc_Pcnt
    =IIF(EMPTY(Style.CDISCCODE),.T.,gfRltFld(Style.CDISCCODE,@laDisRltFld,'CDISCCODE'))
    m.Disc_Pcnt = lnDisc_Pcnt
    m.Price     = m.Gros_Price*(100-m.Disc_Pcnt)/100
  ENDIF
  SKIP IN STYLE
  lcLastStyle=STYLE.Style
  SELECT IIF(llStores,lcTempline,lcOrdLine)
  SET ORDER TO TAG ORDLINST
  IF SEEK(lcOrdType+laData[1]+m.Store+m.Style)
    *E300408,1 Message : 32029
    *E300408,1 This style/color had been entered on this order
    *E300408,1 Button : 00000
    *E300408,1 Ok
    =gfModalGen('INM32029B00000','ALERT',IIF(lcOrdType='C','contract','order'))
  ENDIF
  SET ORDER TO TAG ORDLINE
  =SEEK(lcOrdType+laData[1]+STR(m.LineNo,6))
  IF llAddLine
    lnLines     = lnLines + 1
    m.LineNo    = lnLines
    m.Account   = laData[2]
    m.Order     = laData[1]
    m.cOrdType  = lcOrdType
    m.cWareCode = laData[31]
    *E301305,1 AMM Default complete date .
    m.Complete  = laData[10]
    *E301305,1 AMM end
    SHOW GET llMultiSt DISABLE
    APPEND BLANK
  ELSE
    *B602571,1 Update order totals properly
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      STORE IIF(lnCount > Scale.cnt,0,Qty&lcCount)  TO m.Qty&lcCount
      STORE IIF(lnCount > Scale.cnt,0,Book&lcCount) TO m.Book&lcCount
    ENDFOR
    m.TotQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
    m.TotBook= m.Book1+m.Book2+m.Book3+m.Book4+m.Book5+m.Book6+m.Book7+m.Book8
    IF !EMPTY(m.Prepak) AND SEEK('P'+m.Scale+m.Prepak,'Scale') AND ;
       MOD(m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,Scale.PPTot) <> 0 
      m.Prepak=''
      m.PpQty = 0 
    ENDIF
    =SEEK('S'+m.Scale,'Scale')
    =lfvNPrice(llStores) AND lfChkTotQty(llStores)
    *B602571,1 (End)
  ENDIF
  *B603255,1 WAB - Call function to add adarnment to new line
  *B603255,1 WAB - END
  =lfAdrLine()
  *B603255,1 WAB - END
  =RLOCK()
  GATHER MEMVAR
  UNLOCK
  llAddLine  = .F.
  llCUpdate  = .T.
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  IF llStores
    SHOW WINDOW (lcBrTtlM) REFRESH SAME
    SHOW GETS WINDOW 'SOMULTI2' ENABLE ONLY
    SHOW GETS WINDOW 'SOMULTI3' ENABLE ONLY
    SHOW GETS WINDOW 'SOMULTI4' ENABLE ONLY
    SHOW GETS WINDOW 'SOMULTI5' ENABLE ONLY
    =lfAdjButt('')
    =lfRefresh('SOMULTI2')
    =lfRefresh('SOMULTI3')
    =lfRefresh('SOMULTI4')
  ELSE
    SHOW WINDOW (lcOrdBrow) REFRESH SAME
    SHOW GETS WINDOW (lcWinCh32)  ENABLE ONLY
    SHOW GETS WINDOW (lcWinCh321) ENABLE ONLY
    SHOW GETS WINDOW (lcWinCh322) ENABLE ONLY
    =lfRefresh(lcWinCh32)
    =lfRefresh(lcWinCh321)
  ENDIF
  IF !llMultiSt
    SHOW GET pbTemplate DISABLE
    SHOW GET ibLStore   DISABLE
    SHOW GET m.Store    DISABLE
  ENDIF
  *E301313,1 AMM Disable price and discount if the system is setup not to edit them
  *IF lContract
  IF lContract .OR. !llESTYPSO
  *E301313,1 AMM end
    SHOW GET m.Price      DISABLE
    SHOW GET m.Gros_Price DISABLE
    SHOW GET m.Disc_Pcnt  DISABLE
  ENDIF
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfvStyGrp
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style group
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvStyGrp()
*!*************************************************************
FUNCTION lfvStyGrp
PARAMETERS llStores

IF Group <> m.Group
  llCUpdate = .T.
  =RLOCK()
  REPLACE Group WITH m.Group
  UNLOCK
  IF llStores
    SHOW WINDOW (lcBrTtlM) REFRESH SAME
  ELSE
    =RLOCK()
    REPLACE Flag WITH IIF(Flag='N','N','M')
    UNLOCK
    SHOW WINDOW (lcOrdBrow) REFRESH SAME
  ENDIF
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfvPrePack
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style prepak
*!*************************************************************
*! Calls     : gfPrePBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvPrePack()
*!*************************************************************
FUNCTION lfvPrePack
*C101493,1 AMM Add parameter to indicate if called from the extended size scale screen
*PARAMETERS llStores
PARAMETERS llStores,llFrmGBrw
*C101493,1 AMM end
PRIVATE lcBrFields,lcFile_Ttl
*C101493,1 AMM If called from the ESS screen, put prepack in the memory variable
IF llFrmGBrw
  m.Prepak = EVAL(VARREAD())
ENDIF
*C101493,1 AMM end
IF llBrowse .OR. (!EMPTY(m.Prepak) .AND. !SEEK('P'+m.Scale+m.Prepak,'Scale'))
  =gfPrePBrow(Scale,@m.Prepak)
ENDIF
llBrowse = .F.
=SEEK('S'+m.Scale,'Scale')
*C101493,1 AMM Adjust the condition to fit calling from ESS screen
*IF Prepak <> m.Prepak
IF (!llFrmGBrw .AND. Prepak <> m.Prepak) .OR. (llFrmGBrw .AND. Prepak <> lcOldVal)
*C101493,1 AMM end
  llCUpdate = .T.
  =RLOCK()
  REPLACE Prepak WITH m.Prepak
  *C101493,1 AMM start
  IF llFrmGBrw
    REPLACE cStatus WITH IIF(cStatus='N',cStatus,'M')
  ENDIF
  *C101493,1 AMM end
  UNLOCK
  IF llStores
    SHOW GETS WINDOW 'SOMULTI2' ONLY
    SHOW GETS WINDOW 'SOMULTI3' ONLY
    SHOW GETS WINDOW 'SOMULTI4' ONLY
    SHOW GETS WINDOW 'SOMULTI5' ONLY
  ELSE
    =RLOCK()
    REPLACE Flag WITH IIF(Flag='N','N','M')
    UNLOCK
    SHOW GETS WINDOW (lcWinCh32)  ONLY
    SHOW GETS WINDOW (lcWinCh321) ONLY
    SHOW GETS WINDOW (lcWinCh322) ONLY
  ENDIF
ENDIF
IF EMPTY(Prepak)
  =RLOCK()
  REPLACE PPQty WITH 0
  *C101493,1 AMM start
  IF llFrmGBrw
    REPLACE cPPqty WITH '0',;
            cStatus WITH IIF(cStatus='N',cStatus,'M')
  ENDIF
  *C101493,1 AMM end
  UNLOCK
  m.PPQTY = 0
  SHOW GET m.PPQty DISABLE
ELSE
  SHOW GET m.PPQty ENABLE
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfvPPakQty
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style prepak quantity
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvPPakQty()
*!*************************************************************
FUNCTION lfvPPakQty
*C101493,1 AMM start
*PARAMETERS llStores
PARAMETERS llStores,llFrmGBrw
*C101493,1 AMM change condition to fit calling from the ESS screen
*IF m.PPQty = PPQty
IF (!llFrmGBrw .AND. m.PPQty = PPQty) .OR. (llFrmGBrw .AND. m.PPQTY = VAL(lcOldVal) )
*C101493,1 AMM end
  RETURN
ENDIF
IF m.PPQty <> 0
  =SEEK('P'+m.Scale+m.Prepak,'Scale')
  IF !llStores .AND. Picked .AND. ;
     (m.PPQty*Scale.PP1 < Pik1 .OR. ;
      m.PPQty*Scale.PP2 < Pik2 .OR. ;
      m.PPQty*Scale.PP3 < Pik3 .OR. ;
      m.PPQty*Scale.PP4 < Pik4 .OR. ;
      m.PPQty*Scale.PP5 < Pik5 .OR. ;
      m.PPQty*Scale.PP6 < Pik6 .OR. ;
      m.PPQty*Scale.PP7 < Pik7 .OR. ;
      m.PPQty*Scale.PP8 < Pik8)
    *E300420,1 Message : 32015
    *E300420,1 Quantity cannot be below than what has been picked for this style/color
    *E300420,1 Button : 00000
    *E300420,1 Ok
    =gfModalGen('TRM32015B00000','ALERT')
    STORE PPQty TO m.PPQty
    =SEEK('S'+m.Scale,'Scale')
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF
  *B602825,1 Start. Added alocation qty check when prepack qty was changed.
  *m.Qty1   = m.PPQty*Scale.PP1
  *m.Qty2   = m.PPQty*Scale.PP2
  *m.Qty3   = m.PPQty*Scale.PP3
  *m.Qty4   = m.PPQty*Scale.PP4
  *m.Qty5   = m.PPQty*Scale.PP5
  *m.Qty6   = m.PPQty*Scale.PP6
  *m.Qty7   = m.PPQty*Scale.PP7
  *m.Qty8   = m.PPQty*Scale.PP8
  *m.TotQty = m.PPQty*Scale.PPTot
  
  lnPPQ1 = Scale.PP1
  lnPPQ2 = Scale.PP2
  lnPPQ3 = Scale.PP3
  lnPPQ4 = Scale.PP4
  lnPPQ5 = Scale.PP5
  lnPPQ6 = Scale.PP6
  lnPPQ7 = Scale.PP7
  lnPPQ8 = Scale.PP8
  
  =SEEK('S'+m.Scale,'Scale')
  FOR lnI = 1 TO 8
    lcCnt = STR(lnI,1)
    m.Qty&lcCnt = m.PPQty * lnPPQ&lcCnt
    =lfvSizeQty(lcCnt,llStores)
  ENDFOR
  
  m.TotQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
  * =SEEK('S'+m.Scale,'Scale')
  *B602825,1 End.

  SHOW GET m.TotQty DISABLE
  =SEEK('S'+m.Scale,'Scale')
ELSE
  IF !Picked
    STORE 0 TO m.TotQty
    SHOW GET m.TotQty ENABLE
  ENDIF
ENDIF
*C101493,1 AMM start
*SELECT IIF(llStores,lcTempLine,lcOrdLine)
*=lfChkTotQty(llStores)
IF llFrmGBrw
  SELECT (lcTmpSize)
  =lfChkTQty()
ELSE
  SELECT IIF(llStores,lcTempLine,lcOrdLine)
  =lfChkTotQty(llStores)
ENDIF
*C101493,1 AMM end


=RLOCK()
*B802431,1 update template order lines when distribute bulk order lines.
*REPLACE PPQty WITH m.PPQty ,;
        FLAG  WITH IIF(llStores,'',IIF(Flag='N','N','M'))
REPLACE PPQty WITH m.PPQty ,;
        FLAG  WITH IIF(llStores,Flag,IIF(Flag='N','N','M'))
*B802431,1 (End)
UNLOCK  
*C101493,1 AMM start 
IF llFrmGBrw
  FOR lnC = 1 TO ALEN(laQtyArr,1)
    m.c&laQtyArr[lnC,1] = STR(m.&laQtyArr[lnC,1],laQtyArr[lnC,2]+IIF(laQtyArr[lnC,3]>0,1,0),laQtyArr[lnC,3])
  ENDFOR
  m.cStatus = IIF(&lcTmpSize..cStatus='N','N','M')
  SELECT (lcTmpSize)
  GATHER MEMVAR
ELSE
*C101493,1 AMM end
  IF llStores
    SHOW GETS WINDOW 'SOMULTI2' ONLY
    SHOW GETS WINDOW 'SOMULTI3' ONLY
    SHOW GETS WINDOW 'SOMULTI4' ONLY
    SHOW GETS WINDOW 'SOMULTI5' ONLY
    =lfRefresh('SOMULTI2')
    =lfRefresh('SOMULTI3')
    =lfRefresh('SOMULTI4')
  ELSE
    SHOW GETS WINDOW (lcWinCh32)  ONLY
    SHOW GETS WINDOW (lcWinCh321) ONLY
    SHOW GETS WINDOW (lcWinCh322) ONLY
    =lfRefresh(lcWinCh321)
  ENDIF
*C101493,1 AMM start
ENDIF
*C101493,1 AMM end
*!*************************************************************
*! Name      : FUNCTION lfvPieces
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style pieces
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvPieces()
*!*************************************************************
FUNCTION lfvPieces
*C101493,1 AMM add a new parameter
*PARAMETERS llStores
PARAMETERS llStores,llFrmGBrw

*B603449,1 ABD If statement to cheak if user inter Negative values. [ Begin ]
IF m.TotQty < 0 
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.TotQty = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
*B603449,1 ABD [ End ]

*C101493,1 AMM Adjust condition
*IF EMPTY(m.Prepak) AND Scale.Cnt > 1
  *RETURN
*IF EMPTY(m.Prepak) AND IIF(!llFrmGBrw,Scale.Cnt > 1,&lcTmpScl..Cnt > 1)

IF EMPTY(m.Prepak) 
  IF  !llFrmGBrw .AND. Scale.Cnt > 1 
    RETURN
  ELSE
    *B603010,1 AMM Start, Change not to cause an error
    *IF llFrmGBrw .AND. &lcTmpScl..Cnt > 1
    IF llFrmGBrw .AND. EVAL(lcTmpScl+'.Cnt') > 1
    *B603010,1 AMM end
        RETURN
    ENDIF
  ENDIF
*C101493,1 AMM end
ENDIF
IF !EMPTY(m.Prepak) .AND. SEEK('P'+m.Scale+m.Prepak,'Scale')
  *E300408,1 Message : 32030
  *E300408,1 Total quantity is not evenly divisible by the prepak quantity xxx
  *E300408,1 Button : 00000 
  *E300408,1 Ok
  IF MOD(m.TotQty,Scale.PPTot) <> 0 .AND. ;
    gfModalGen('TRM32030B00000','ALERT',STR(Scale.PPTot,4))=1
    *C101493,1 AMM Restore old values
    IF llFrmGBrw
      REPLACE cTotQty WITH lcOldVal,;
              TotQty  WITH VAL(lcOldVal)
    ENDIF
    *C101493,1 AMM end
    _CUROBJ = _CUROBJ
    =SEEK('S'+m.Scale,'Scale')
    RETURN
  ENDIF
  m.Qty1 = m.TotQty*Scale.PP1/Scale.PPTot
  m.Qty2 = m.TotQty*Scale.PP2/Scale.PPTot
  m.Qty3 = m.TotQty*Scale.PP3/Scale.PPTot
  m.Qty4 = m.TotQty*Scale.PP4/Scale.PPTot
  m.Qty5 = m.TotQty*Scale.PP5/Scale.PPTot
  m.Qty6 = m.TotQty*Scale.PP6/Scale.PPTot
  m.Qty7 = m.TotQty*Scale.PP7/Scale.PPTot
  m.Qty8 = m.TotQty*Scale.PP8/Scale.PPTot
  IF Picked .AND.  ;
     (m.Qty1 < Pik1 .OR. m.Qty2 < Pik2 .OR. m.Qty3 < Pik3 .OR. m.Qty4 < Pik4 .OR. ;
      m.Qty5 < Pik5 .OR. m.Qty6 < Pik6 .OR. m.Qty7 < Pik7 .OR. m.Qty8 < Pik8)
    *E300420,1 Message : 32015
    *E300420,1 Quantity cannot be below than what has been picked for this style/color
    *E300420,1 Button : 00000
    *E300420,1 Ok
    =gfModalGen('TRM32015B00000','ALERT')
    SCATTER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty
    _CUROBJ = _CUROBJ
    =SEEK('S'+m.Scale,'Scale')
    RETURN
  ENDIF
ELSE
  IF Scale.Cnt = 1
    m.Qty1 = m.TotQty
  ENDIF
ENDIF
=SEEK('S'+m.Scale,'Scale')
IF m.Qty1=Qty1 AND m.Qty2=Qty2 AND m.Qty3=Qty3 AND m.Qty4=Qty4 AND ;
   m.Qty5=Qty5 AND m.Qty6=Qty6 AND m.Qty7=Qty7 AND m.Qty8=Qty8 AND m.TotQty=TotQty
  RETURN
ENDIF
IF llFrmGBrw
  =lfChkTQty()
ELSE
  =lfChkTotQty(llStores)
ENDIF

*C101493,1 AMM If called from the browse validation, update the character fields
IF llFrmGBrw
  m.cQty1 = STR(m.Qty1,6)
  m.cQty2 = STR(m.Qty2,6)
  m.cQty3 = STR(m.Qty3,6)
  m.cQty4 = STR(m.Qty4,6)
  m.cQty5 = STR(m.Qty5,6)
  m.cQty6 = STR(m.Qty6,6)
  m.cQty7 = STR(m.Qty7,6)
  m.cQty8 = STR(m.Qty8,6)
  m.cStatus = IIF(&lcTmpSize..cStatus='N','N','M')
  SELECT (lcTmpSize)
  GATHER MEMVAR
ENDIF
*C101493,1 AMM end
IF llStores
  SHOW GETS WINDOW 'SOMULTI2' ONLY
  SHOW GETS WINDOW 'SOMULTI3' ONLY
  SHOW GETS WINDOW 'SOMULTI4' ONLY
  SHOW GETS WINDOW 'SOMULTI5' ONLY
  =lfRefresh('SOMULTI2')
  =lfRefresh('SOMULTI3')
  =lfRefresh('SOMULTI4')
ELSE
  SHOW GETS WINDOW (lcWinCh32)  ONLY
  SHOW GETS WINDOW (lcWinCh321) ONLY
  SHOW GETS WINDOW (lcWinCh322) ONLY
  =lfRefresh(lcWinCh32)
  =lfRefresh(lcWinCh321)
ENDIF

*!**********39582*1***************************************************
*! Name      : FUNCTION lfvSizeQty
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style size quantity
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: lnSize   : Size 
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvSizeQty()
*!*************************************************************
FUNCTION lfvSizeQty
PARAMETERS lcSize,llStores
PRIVATE lnChoice 

*B603449,1 ABD If statement to cheak if user inter Negative values. [ Begin ]
IF m.Qty&lcSize < 0 
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.Qty&lcSize = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
*B603449,1 ABD [ End ]

IF m.Qty&lcSize <> Qty&lcSize AND !llStores
  IF m.Picked AND !EMPTY(m.PikTKt) AND m.Qty&lcSize < m.Pik&lcSize
    *E300420,1 Message : 32015
    *E300420,1 Quantity cannot be below than what has been picked for this style/color
    *E300420,1 Button : 00000
    *E300420,1 Ok
    =gfModalGen('TRM32015B00000','ALERT')
    m.Qty&lcSize = &lcOrdLine..Qty&lcSize
    RETURN
  ENDIF
  *B802805,1 AMM Check the sold out date of the style edited its quantity
  *B802805,1 AMM Message :Style sold out date is XX/XX/XXXX
  *B802805,1 AMM   < Accept >  < Reenter >
  IF !EMPTY(Style.SoldOut) .AND. Style.SoldOut < laData[9] .AND. ;
       gfModalGen('QRM40010B40001','ALERT','sold out|'+DTOC(Style.SoldOut))=2
    m.Qty&lcSize = &lcOrdLine..Qty&lcSize
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF
  *B802805,1 AMM end
  IF m.Qty&lcSize < Cut&lcSize
    *B603368,1 AMM If the user has no access on CT, inform the user and ignore all.
    IF !gfUserPriv('MF','MFCUTKT')
      llIgnorAll = .T.  
      *-- Message: This order has an allocated quantity, you may need to edit the 
      *-- quantities on the cutting ticket, but the access to do this is denied.'
      =gfModalGen('INM32075B00000','DIALOG')
    ENDIF
    *B603368,1 AMM end
    *E300420,1 Message : 32016
    *E300420,1 Size xxxxx has xxx pieces allocated from orders.
    *E300420,1 Edit the detail allocated quantity from the order lines to keep 
    *E300420,1 track of the allocated quantity, Ignore tracking the allocation 
    *E300420,1 for only the quantity being modified, or for all later modifications.
    *E300420,1 Button : 32002
    *E300420,1 Edit Allo. Qty./Ignore/Ignore All/Cancel
    
    lnChoice = IIF(llIgnorAll,3,;
    gfModalGen('QRM32016B32002','ALERT',ALLTRIM(SCALE.Sz&lcSize)+'|'+ALLTRIM(STR(Cut&lcSize,5))))
    llIgnorAll = (lnChoice=3)
    IF lnChoice =4
      STORE Qty&lcSize TO m.Qty&lcSize
      RETURN
    ENDIF
    lnAlias = SELECT()
    *E301077,5 Inhance openning files to speed up transaction
    =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
    *E301077,5 (End)
    SET ORDER TO TAG 'CUTORD' IN (lcAlocated)
    IF !SEEK(IIF(Style.Make,'1','2')+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6),lcAlocated) .AND. ;
       SEEK(IIF(Style.Make,'1','2')+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6),'CutPick')
      SELECT CutPick
      SCAN REST WHILE TranCd+Order+cOrdLine = ;
        IIF(Style.Make,'1','2')+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6)
        SCATTER TO laCutPick
        INSERT INTO (lcAlocated) FROM ARRAY laCutPick
      ENDSCAN
    ENDIF
    *E301077,5 Inhance openning files to speed up transaction
    =gfCloseFile('CUTPICK')
    *E301077,5 (End)
    DO CASE
      CASE lnChoice =1
        IF !lfEditAlo(lcSize)
          m.Qty&lcSize = &lcOrdLine..Qty&lcSize
        ENDIF    
      OTHERWISE
        SELECT (lcAlocated)
        =SEEK(IIF(Style.Make,'1','2')+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6))
        SCAN REST WHILE TranCd+Order+cOrdLine = ;
            IIF(Style.Make,'1','2')+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6)
          SELECT (lcOrdHdr)
          =RLOCK()
          REPLACE TotCut WITH TotCut - &lcAlocated..Qty&lcSize
          UNLOCK
          SELECT (lcOrdLine)
          =RLOCK()
          REPLACE Cut&lcSize WITH Cut&lcSize - &lcAlocated..Qty&lcSize ,;
                  TotCut     WITH TotCut     - &lcAlocated..Qty&lcSize
          UNLOCK
          SELECT (lcAlocated)
          =RLOCK()
          REPLACE TotQty     WITH TotQty - Qty&lcSize ,;
                  Qty&lcSize WITH 0
          UNLOCK
        ENDSCAN
    ENDCASE
    SELECT (lnAlias)
  ENDIF
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfChkTotQty
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Check total pieces againest total quantity entered
*!*************************************************************
*! Calls     : gfModalGen(),lfvNPrice()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfChkTotQty()
*!*************************************************************
FUNCTION lfChkTotQty
PARAMETERS llStores

*E300408,1 Message : 32031
*E300408,1 Quantity out of balance! xxx pieces ok
*E300408,1 Button : 32000 
*E300408,1 Yes NO
IF m.TotQty <> m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8 AND ;
  gfModalGen('QRM32031B32000','ALERT',STR(m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,5))=2
  RETURN(.F.)
ENDIF
*E300408,1 Message : 32030
*E300408,1 Total quantity is not evenly divisible by the prepak quantity xxx
*E300408,1 Button : 00000 
*E300408,1 Ok
*IF !EMPTY(m.Prepak) AND SEEK('P'+m.Scale+m.Prepak,'Scale') AND ;
*   MOD(m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,Scale.PPTot) <> 0 .AND. ;
*   gfModalGen('TRM32030B00000','ALERT',STR(Scale.PPTot,4))=1
*  =SEEK('S'+m.Scale,'Scale')
*  RETURN(.F.)
*ENDIF
IF Qty1 <> m.Qty1 OR Qty2 <> m.Qty2 OR Qty3 <> m.Qty3 OR Qty4 <> m.Qty4 OR ;
   Qty5 <> m.Qty5 OR Qty6 <> m.Qty6 OR Qty7 <> m.Qty7 OR Qty8 <> m.Qty8
  
  STORE .T. TO llCUpdate
  m.TotQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
  *E300834,1 Get style price according to ordered quantity
  IF !m.lContract AND lcPriceLvl='Q'
    *E301142,1 Select alternative price level
    *lcPriceCatg  = IIF(m.TotQty >= Style.nAtQtyC,'C',IIF(m.TotQty >= Style.nAtQtyB,'B','A'))
    *m.Gros_Price = IIF(!llMulCurr OR laData[33]=gcBaseCurr,Style.Price&lcPriceCatg,;
                       gfStyPrice(m.Style,lcPriceCatg,laData[33]))
    m.Gros_Price = lfGetprice(m.Style,lcPriceLvl,m.TotQty)
    *E301142,1 (End)
    m.Price = m.Gros_Price*(100-m.Disc_Pcnt)/100
  ENDIF
  *E300834,1 (End)

  IF llStores
    lnTotPcs = lnTotPcs - TotQty+m.TotQty
    lnTotAmt = lnTotAmt - TotQty*Price + m.TotQty*m.Price
  ELSE
    IF laScrMode[4] AND !EMPTY(cFromOrder) .AND. SEEK(STR(BulkLineNo,6),lcBulkOrd)
      SELECT (lcBulkOrd)
      =RLOCK()
      REPLACE USED1   WITH USED1 - &lcOrdLine..Qty1 + m.Qty1 ,;
              USED2   WITH USED2 - &lcOrdLine..Qty2 + m.Qty2 ,;
              USED3   WITH USED3 - &lcOrdLine..Qty3 + m.Qty3 ,;
              USED4   WITH USED4 - &lcOrdLine..Qty4 + m.Qty4 ,;
              USED5   WITH USED5 - &lcOrdLine..Qty5 + m.Qty5 ,;
              USED6   WITH USED6 - &lcOrdLine..Qty6 + m.Qty6 ,;
              USED7   WITH USED7 - &lcOrdLine..Qty7 + m.Qty7 ,;
              USED8   WITH USED8 - &lcOrdLine..Qty8 + m.Qty8 ,;
              TOTUSED WITH Used1+Used2+Used3+Used4+Used5+Used6+Used7+Used8
      UNLOCK
      *C101493,1 AMM Select the right file
      *SELECT (lcOrdLine)
      IF TYPE('LLFRMGBRW')='L' .AND. llFrmGBrw
        SELECT(lcTmpSize)
      ELSE
        SELECT (lcOrdLine)
      ENDIF
      *C101493,1 AMM end      
    ENDIF
    =SEEK(cOrdType+Order+STR(LineNo,6),'OrdLine')
    laData[35] = laData[35] - TotBook
    laData[36] = laData[36] - TotBook*Price
    =RLOCK()
    REPLACE Book1   WITH MAX(OrdLine.Book1-OrdLine.Qty1+m.Qty1,IIF(llUpdBook,0,OrdLine.Book1))  ,;
            Book2   WITH MAX(OrdLine.Book2-OrdLine.Qty2+m.Qty2,IIF(llUpdBook,0,OrdLine.Book2))  ,;
            Book3   WITH MAX(OrdLine.Book3-OrdLine.Qty3+m.Qty3,IIF(llUpdBook,0,OrdLine.Book3))  ,;
            Book4   WITH MAX(OrdLine.Book4-OrdLine.Qty4+m.Qty4,IIF(llUpdBook,0,OrdLine.Book4))  ,;
            Book5   WITH MAX(OrdLine.Book5-OrdLine.Qty5+m.Qty5,IIF(llUpdBook,0,OrdLine.Book5))  ,;
            Book6   WITH MAX(OrdLine.Book6-OrdLine.Qty6+m.Qty6,IIF(llUpdBook,0,OrdLine.Book6))  ,;
            Book7   WITH MAX(OrdLine.Book7-OrdLine.Qty7+m.Qty7,IIF(llUpdBook,0,OrdLine.Book7))  ,;
            Book8   WITH MAX(OrdLine.Book8-OrdLine.Qty8+m.Qty8,IIF(llUpdBook,0,OrdLine.Book8))  ,;
            TotBook WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8 ,;
            FLAG WITH IIF(FLAG='N','N','M')
    UNLOCK
    
    *E300956,1 Store order line calceled quantity
    IF Flag <> 'N' AND !llUpdBook
      IF OrdLine.Qty1 > m.Qty1 OR OrdLine.Qty2 > m.Qty2 OR OrdLine.Qty3 > m.Qty3 OR ;
         OrdLine.Qty4 > m.Qty4 OR OrdLine.Qty5 > m.Qty5 OR OrdLine.Qty6 > m.Qty6 OR  ;
         OrdLine.Qty7 > m.Qty7 OR OrdLine.Qty8 > m.Qty8
        =gfwCodePop(@laCodes,'CCANCRESON','D')
        INSERT INTO (lcOrdCanLn) ;
        (cOrdType,Order,LineNo,Cancelled,cCancReson) VALUES ;
        (lcOrdType,laData[1],m.LineNo,gdSysDate,laCanReason[lnCanReason,2])
      ENDIF
      IF SEEK(lcOrdType+laData[1]+STR(m.LineNo,6),lcOrdCanLn)
        SELECT (lcOrdCanLn)
        REPLACE Qty1 WITH MAX(OrdLine.Qty1- m.Qty1,0) ,;
                Qty2 WITH MAX(OrdLine.Qty2- m.Qty2,0) ,;
                Qty3 WITH MAX(OrdLine.Qty3- m.Qty3,0) ,;
                Qty4 WITH MAX(OrdLine.Qty4- m.Qty4,0) ,;
                Qty5 WITH MAX(OrdLine.Qty5- m.Qty5,0) ,;
                Qty6 WITH MAX(OrdLine.Qty6- m.Qty6,0) ,;
                Qty7 WITH MAX(OrdLine.Qty7- m.Qty7,0) ,;
                Qty8 WITH MAX(OrdLine.Qty8- m.Qty8,0) ,;
                TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
        *B803652,1 (Begin) Update Style,Account,Store,Dyelot also.
        REPLACE ACCOUNT WITH ORDLINE.ACCOUNT,;
                STYLE   WITH ORDLINE.STYLE,;
                STORE   WITH ORDLINE.STORE,;
                DYELOT  WITH ORDLINE.DYELOT
        *B803652,1 (End
        IF TotQty > 0
          STORE lcOrdCanLn TO laCodes[6,7],laCodes[6,8]
          STORE 'lcOrdType+laData[1]+STR(m.LineNo,6)' TO laCodes[6,9]
          =gfwCodePop(@laCodes,'CCANCRESON','T')
          STORE Cancelled TO m.Cancelled
          DO (gcScrDir+"SOORDCLN.SPX")
          STORE '' TO laCodes[6,7],laCodes[6,8],laCodes[6,9]
        ELSE
          DELETE
        ENDIF
        *C101493,1 AMM Select the right file
        *SELECT (lcOrdLine)
        IF TYPE('LLFRMGBRW')='L' .AND. llFrmGBrw
          SELECT(lcTmpSize)
        ELSE
          SELECT (lcOrdLine)
        ENDIF
        *C101493,1 AMM end
      ENDIF
    ENDIF
    *E300956,1 (End)
    laData[35] = laData[35] + TotBook
    laData[36] = laData[36] + TotBook*m.Price
    IF FLAG <> 'N' AND !llUpdBook
      *B603838,1 ABD Fix bug that when update Qty per size The cancelled qty 
      *B603838,1 ABD Is not getting updated Correct. [Begin]
      *laData[39] = MAX(laData[39] + MIN(TotQty,OrdLine.TotQty)-m.TotQty,0)
      *laData[40] = MAX(laData[40] + (MIN(TotQty,OrdLine.TotQty)-m.TotQty)*Price,0)
      lnTotCanQty = 0
      For I = 1 To Scale.Cnt
        lcScalSize = STR(I,1)
        lnTotCanQty = lnTotCanQty + MAX(Qty&lcScalSize - m.Qty&lcScalSize,0)
      ENDFOR
      laData[39] = MAX(laData[39] + lnTotCanQty,0)
      laData[40] = MAX(laData[40] + lnTotCanQty*Price,0)
      *B603838,1 ABD [End]
    ENDIF  

    laData[41] = laData[41] - TotQty+m.TOtQty
    laData[42] = laData[42] - TotQty*Price+ m.TOtQty*m.Price
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE BOOK      WITH laData[35] ,;
            BOOKAMT   WITH laData[36] ,;
            CANCEL    WITH laData[39] ,;
            CANCELAMT WITH laData[40] ,;
            OPEN      WITH laData[41] ,;
            OPENAMT   WITH laData[42]
    UNLOCK
    *C101493,1 AMM Select the right file
    *SELECT (lcOrdLine)
    IF TYPE('LLFRMGBRW')='L' .AND. llFrmGBrw
      SELECT(lcTmpSize)
    ELSE
      SELECT (lcOrdLine)
    ENDIF
    *C101493,1 AMM end
  ENDIF
  =IIF(llStores,lfRefresh('SOMULTI4'),lfRefresh(lcWinCh321))
  =RLOCK()
  GATHER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,;
                       Gros_Price,Price
  UNLOCK
  SCATTER MEMVAR FIELDS Book1,Book2,Book3,Book4,Book5,Book6,Book7,Book8,TotBook
ENDIF
SCATTER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty

*!*************************************************************
*! Name      : FUNCTION lfvOrdCanLn
*! Developer : Wael Aly Mohamed
*! Date      : 08/16/1998
*! Purpose   : Update Order line canceled reason and date
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvOrdCanLn()
*!*************************************************************
FUNCTION lfvOrdCanLn
CLEAR READ
SELECT (lcOrdCanLn)
=SEEK(lcOrdType+laData[1]+STR(m.LineNo,6))
REPLACE cCancReson WITH laCanReason[lnCanReason,2],;
        Cancelled  WITH m.Cancelled
SELECT (lcOrdLine)

*!*************************************************************
*! Name      : FUNCTION lfvStyDesc
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style description
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvStyDesc()
*!*************************************************************
FUNCTION lfvStyDesc
PARAMETERS llStores

IF Desc1 <> m.Desc1
  llCUpdate = .T.
  =RLOCK()  
  REPLACE Desc1 WITH m.Desc1 ,;
          Flag  WITH IIF(Flag='N' .OR. llStores,Flag,'M')
  UNLOCK
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfvComm
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style salesrep commissions
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lnSalesRep   : Salesrep number
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvComm()
*!*************************************************************
FUNCTION lfvComm
*C101493,1 AMM start
*PARAMETERS lcSalesRep,llStores
PARAMETERS lcSalesRep,llStores,llFrmGBrw
*C101493,1 AMM end
*IF Comm&lcSalesRep <> m.Comm&lcSalesRep
*B802585,1 AMM (Start) The field is treated as numeric in the extended size scale screen
*IF (!llFrmGBrw .AND. Comm&lcSalesRep <> m.Comm&lcSalesRep) .OR. (llFrmGBrw .AND. m.Comm&lcSalesRep # VAL(lcOldVal))
IF (!llFrmGBrw .AND. Comm&lcSalesRep <> m.Comm&lcSalesRep) .OR. (llFrmGBrw .AND. m.Comm&lcSalesRep # lcOldVal)
*B802585,1 AMM end
  llCUpdate = .T.
  =RLOCK()  
  REPLACE Comm&lcSalesRep WITH m.Comm&lcSalesRep ,;
          Flag  WITH IIF(Flag='N' .OR. llStores,Flag,'M')
  *C101493,1 AMM start
  IF llFrmGBrw
    *B802585,1 AMM the field treated as numeric in the extended size scale screen
    *REPLACE cComm&lcSalesRep WITH STR(m.Comm&lcSalesRep,5,2),;
            cStatus          WITH IIF(cStatus='N','N','M')
    REPLACE cStatus WITH IIF(cStatus='N','N','M')
    *B802585,1 AMM end
  ENDIF
  *C101493,1 AMM end
  
  UNLOCK
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfwGPrice
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : When function for Style Gross price
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfwGPrice()
*!*************************************************************
FUNCTION lfwGPrice
PARAMETERS llStores
PRIVATE lcPriceCatg,lnPrice,lcOrdAlias

IF m.lContract 
  RETURN(.F.)
ENDIF

*E301288,1 Reham On 07/29/1999   *** Begin ***
IF llBomVarnt
  lcOrdAlias = ALIAS()
  SELECT (lcT_BomVar)
  *E301288,1 Collect the price adder for the style positions.
  SUM nPriceAdd FOR cIdType+cCost_Id+STR(LineNo,6) = "SO" + laData[1] + STR(m.LineNo,6) TO lnAddPrice
  SELECT (lcOrdAlias)
  *E301288,1 If the editted price less than the price adder, inform the user.
  IF lnAddPrice > 0
    RETURN
  ENDIF
ENDIF
*E301288,1 Reham On 07/29/1999   *** End   ***

*E300408,1 Message : 32032
*E300408,1 Style price xxx does not match order price.  Default to style price?
*E300408,1 Button : 32000
*E300408,1 Yes No
lcPriceCatg = IIF(lcPriceLvl='Q',;
              IIF(Style.nAtQtyC>0 AND m.TotQty >= Style.nAtQtyC,'C',;
              IIF(Style.nAtQtyB>0 AND m.TotQty >= Style.nAtQtyB,'B','A')),;
              IIF(INLIST(lcPriceLvl,'A','B','C'),lcPriceLvl,'A'))
lnPrice = IIF(!llMulCurr OR laData[33]=gcBaseCurr,Style.Price&lcPriceCatg,;
                       gfStyPrice(m.Style,lcPriceCatg,laData[33]))

IF lnPrice>0 .AND. m.Gros_Price <> lnPrice .AND. ;
  gfModalGen('QRM32032B32000','ALERT',ALLTRIM(STR(lnPrice,12,2))+;
  '|'+IIF(lcOrdType='C','contract','order'))=1

  m.Gros_Price = lnPrice
  m.Price = ROUND(m.Gros_Price*(100-m.Disc_Pcnt)/100,2)
  =lfvNPrice(llStores)
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfvGPrice
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style Gross price
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvGPrice()
*!*************************************************************
FUNCTION lfvGPrice
*C101493,1 AMM (start) New parameter for the new calling from extended size scale screen
*PARAMETERS llStores
PARAMETERS llStores,llFrmGBrw
*C101493,1 AMM end
PRIVATE lcOrdAlias

*B603449,1 ABD If statement to cheak if user inter Negative values. [ Begin ]
IF m.Gros_Price < 0 
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.Gros_Price = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
*B603449,1 ABD [ End ]

*B602446,1 Elimenate order amount roundation
*m.Price = m.Gros_Price*(100-m.Disc_Pcnt)/100
*C101493,1 AMM start
*IF m.Gros_Price <> Gros_Price
*B802585,1 AMM The field treated as numeric in extended size scale screen
*IF (!llFrmGBrw .AND. m.Gros_Price <> Gros_Price) .OR. (llFrmGBrw .AND. m.Gros_Price <> VAL(lcOldVal) )
IF (!llFrmGBrw .AND. m.Gros_Price <> Gros_Price) .OR. (llFrmGBrw .AND. m.Gros_Price <> lcOldVal )
*B802585,1 AMM end
*C101493,1 AMM end
  *E301288,1 Reham On 07/25/1999   *** Begin ***
  *E301288,1 Do not allow the user to decrease the price to be less than the price added.
  *B802585,1 AMM consider calling from extended size scale screen
  *IF llBomVarnt .AND. m.Gros_Price < Gros_Price
  IF llBomVarnt .AND. (!llFrmGBrw .AND. m.Gros_Price < Gros_Price) .OR. (llFrmGBrw .AND. m.Gros_Price < lcOldVal )
  *B802585,1 AMM end
    lcOrdAlias = ALIAS()
    SELECT (lcT_BomVar)
    *E301288,1 Collect the price adder for the style positions.
    SUM nPriceAdd FOR cIdType+cCost_Id+STR(LineNo,6) = "SO" + laData[1] + STR(m.LineNo,6) TO lnAddPrice
    SELECT (lcOrdAlias)
    *E301288,1 If the editted price less than the price adder, inform the user.
    IF m.Gros_Price < lnAddPrice
      *** There is style positions for this line, you cannot ***
      *** change the price to be less than the price adder.  ***
      *** <  Ok  > ***
      =gfModalGen("INM32073B00000" , "DIALOG")
      *E301288,1 Restore the price adder.
      *B802585,1 AMM  Consider calling from extended size scale screen.
      *m.Gros_Price = Gros_Price
      m.Gros_Price = IIF(llFrmGBrw,lcOldVal,Gros_Price)
      *B802585,1 AMM end
      SHOW GET m.Gros_Price
      RETURN
    ENDIF
  ENDIF
  *E301288,1 Reham On 07/25/1999   *** End   ***
  m.Price = ROUND(m.Gros_Price*(100-m.Disc_Pcnt)/100,2)
  *B602446,1 (End)
  *C101493,1 AMM start
  *=lfvNPrice(llStores)
  =lfvNPrice(llStores,.F.,llFrmGBrw)
  *C101493,1 AMM end
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfvPrcDisc
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style price discount
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvPrcDisc()
*!*************************************************************
FUNCTION lfvPrcDisc
*C101493,1 AMM start
*PARAMETERS llStores
PARAMETERS llStores,llFrmGBrw
*C101493,1 AMM end

*B602446,1 Elimenate order amount roundation
*m.Price = m.Gros_Price*(100-m.Disc_Pcnt)/100
*=lfvNPrice(llStores)
*C101493,1 AMM start
*IF m.Disc_Pcnt <> Disc_Pcnt
*B802585,1 AMM The field is treated as numeric in extended size scale screen
*IF (!llFrmGBrw .AND. m.Disc_Pcnt <> Disc_Pcnt) .OR. (llFrmGBrw .AND. m.Disc_Pcnt#VAL(lcOldVal) )
IF (!llFrmGBrw .AND. m.Disc_Pcnt <> Disc_Pcnt) .OR. (llFrmGBrw .AND. m.Disc_Pcnt# lcOldVal )
*B802585,1 AMM end
*C101493,1 AMM end
  m.Price = ROUND(m.Gros_Price*(100-m.Disc_Pcnt)/100,2)
  *C101493,1 AMM start
  *=lfvNPrice(llStores,.T.)
  =lfvNPrice(llStores,.T.,llFrmGBrw)
  *C101493,1 AMM end
  *B602446,1 (End)
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfvNPrice
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style Net price
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvNPrice()
*!*************************************************************
FUNCTION lfvNPrice
*C101493,1 AMM start
*PARAMETERS llStores,llPercent
PARAMETERS llStores,llPercent,llFrmGBrw

*B603449,1 ABD If statement to cheak if user inter Negative values. [ Begin ]
IF m.Price < 0 
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.Price = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
*B603449,1 ABD [ End ]

*B802585,1 AMM The field treated as numeric in the extended size scale screen
*IF llFrmGBrw .AND. UPPER(VARREAD()) = 'CPRICE'
  *REPLACE Price WITH VAL(lcoldVal)
*ENDIF
IF llFrmGBrw .AND. UPPER(VARREAD()) = 'PRICE'
  REPLACE Price WITH lcoldVal
ENDIF
*B802585,1 AMM end

*C101493,1 AMM end

m.Price = IIF(m.Price>m.Gros_Price,Price,m.Price)
IF m.Price <> Price
  STORE .T. TO llCUpdate
  *C101493,1 AMM start
  *SELECT IIF(llStores,lcTempLine,lcOrdLine)
  SELECT IIF(llFrmGBrw,lcTmpSize,IIF(llStores,lcTempLine,lcOrdLine))
  *C101493,1 AMM end
  IF llStores
    lnTotAmt = lnTotAmt + TotQty*(m.Price - Price)
  ELSE
    laData[36] = laData[36] + TotBook*(m.Price - Price)
    laData[42] = laData[42] + TotQty*(m.Price - Price)
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE BOOKAMT WITH laData[36] ,;
            OPENAMT WITH laData[42]
    UNLOCK
    SELECT (lcOrdLine)
  ENDIF
  *B602446,1 Elimenate order amount roundation
  *m.Disc_Pcnt= IIF(m.Gros_Price=0,0,100-m.Price*100/m.Gros_Price)
  m.Disc_Pcnt = IIF(llPercent,m.Disc_Pcnt,IIF(m.Gros_Price=0,0,100-m.Price*100/m.Gros_Price))
  *B602446,1 (End)
  *C101493,1 AMM start
  IF llFrmGBrw
    SELECT(lcTmpSize)
    *B802585,1 AMM These fields treated as numeric in the extended size scale screen
    *REPLACE cStatus WITH IIF(cStatus = 'N',cStatus,'M'),;
            cGros_Price WITH STR(m.Gros_Price,9,2) ,;
            cDisc_Pcnt  WITH STR(m.Disc_Pcnt,7,2)  ,;
            cPrice      WITH STR(m.Price,10,2)
    REPLACE cStatus WITH IIF(cStatus = 'N',cStatus,'M')
    *B802585,1 AMM end
  ENDIF
  *C101493,1 AMM end
  =RLOCK()
  REPLACE Gros_Price WITH m.Gros_Price ,;
          Disc_Pcnt  WITH m.Disc_Pcnt  ,;
          Price      WITH m.Price      ,;
          Flag       WITH IIF(Flag='N' .OR. llStores,Flag,'M')
  UNLOCK
  SHOW GET m.Gros_Price
  SHOW GET m.Disc_Pcnt
  SHOW GET m.Price
  IF llStores
    =lfrefresh('SOMULTI2')
    SHOW WINDOW (lcBrTtlM) REFRESH SAME
  ELSE
    SHOW WINDOW (lcOrdBrow) REFRESH SAME
  ENDIF
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfvStyPack
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate order lines Style packs
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvStyPack()
*!*************************************************************
FUNCTION lfvStyPack
*C101493,1 AMM start
*PARAMETERS llStores
PARAMETERS llStores,llFrmGBrw
IF llFrmGBrw
  m.Pack_id = EVAL(VARREAD())
ENDIF
*C101493,1 AMM end
PRIVATE lcStyClr

IF llBrowse .OR. !EMPTY(m.Pack_Id)
  *E301077,5 WAM 02/28/1999 Inhance openning files to speed up transaction
  =gfOpenFile(gcDataDir+'SPck_Hdr',gcDataDir+'SPck_Hdr','SH')
  =gfOpenFile(gcDataDir+'SPck_Lin',gcDataDir+'SPCKLNST','SH')
  *E301077,5 (End)

  IF llBrowse OR ;
    (!SEEK('P'+m.Style+laData[2]+m.Pack_Id,'SPck_Lin') .AND. ;
     !SEEK('P'+m.Style+'*****'+m.Pack_Id,'SPck_Lin'))

    IF !SEEK('P'+m.Style+laData[2],'SPck_Lin') .AND. ;
       !SEEK('P'+m.Style+'*****','SPck_Lin')
      *E300420,1 Message : 32042
      *E300420,1 No packs found for style/color xxxx
      *E300420,1 Button : 00000 
      *E300420,1 Ok
      =gfModalGen('TRM32042B00000','DIALOG',ALLTRIM(m.Style))
      STORE SPACE(16) TO m.Pack_Id
    ELSE
      lcStyClr = m.Style
      SELECT SPck_Lin
      SET RELATION TO Type+Account+Pack_Id INTO SPck_Hdr
      lcBrFields = [Account :H="Account",Pack_Id :H="Pack Id",SPck_Hdr.Desc :H='Description',TotQty :H='Quantity']
      m.Pack_Id = IIF(ARIABROW("'P'+lcStyClr"+" FOR ACCOUNT=laData[2] OR ACCOUNT='*****'","Style packs",;
                  gnBrFSRow1, gnBrFSCol1,gnBrFSRow2, gnBrFSCol2,'','','Pack_id','laBrowArr'),SPck_Lin.Pack_Id,&lcOrdLine..Pack_Id)
    ENDIF  
  ENDIF
  *B602636,1 Defualt pack quantity when selecting style pack
  IF !EMPTY(m.Pack_Id) AND m.TotQty = 0
    m.Qty1 = Spck_lin.Qty1
    m.Qty2 = Spck_lin.Qty2
    m.Qty3 = Spck_lin.Qty3
    m.Qty4 = Spck_lin.Qty4
    m.Qty5 = Spck_lin.Qty5
    m.Qty6 = Spck_lin.Qty6
    m.Qty7 = Spck_lin.Qty7
    m.Qty8 = Spck_lin.Qty8
    m.TotQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
    *C101493,1 AMM Make a new function to check quantities
    *=lfChkTotQty(llStores)
    IF llFrmGBrw
      =lfChkTQty()
    ELSE
      =lfChkTotQty(llStores)
    ENDIF
    *C101493,1 AMM end
    IF llStores
      SHOW WINDOW (lcBrTtlM) REFRESH SAME
      SHOW GETS WINDOW 'SOMULTI3' ONLY
      SHOW GETS WINDOW 'SOMULTI4' ONLY
      =lfRefresh('SOMULTI2')
      =lfRefresh('SOMULTI4')
    ELSE
      SHOW WINDOW (lcOrdBrow) REFRESH SAME
      SHOW GETS WINDOW (lcWinCh32)  ONLY
      SHOW GETS WINDOW (lcWinCh321) ONLY
      =lfRefresh(lcWinCh321)
    ENDIF
  ENDIF
  *B602636,1 (End)
  
  *E301077,5 WAM 02/28/1999 Inhance openning files to speed up transaction
  =gfCloseFile('SPck_Hdr')
  =gfCloseFile('SPck_Lin')
  *E301077,5 (End)
ENDIF
llBrowse = .F.
*C101493,1 AMM start 
*SELECT IIF(llStores,lcTempline,lcOrdLine)
SELECT IIF(llFrmGBrw,lcTmpSize,IIF(llStores,lcTempline,lcOrdLine))
*C101493,1 AMM end

IF Pack_Id <> m.Pack_Id
  llCUpdate = .T.
  =RLOCK()  
  REPLACE Pack_Id WITH m.Pack_Id ,;
          Flag  WITH IIF(Flag='N' .OR. llStores,Flag,'M')
  *C101493,1 AMM start
  IF llFrmGBrw
    REPLACE cStatus WITH IIF(cStatus = 'N',cStatus,'M')
  ENDIF
  *C101493,1 AMM end
  
  UNLOCK
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfvCopyQty
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Copy line quantities
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvCopyQty()
*!*************************************************************
FUNCTION lfvCopyQty
SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laQtyPaste

*!*************************************************************
*! Name      : FUNCTION lfvPasteQty
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Paste line quantities
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvPasteQty()
*!*************************************************************
FUNCTION lfvPasteQty
PARAMETERS llStores

IF laQtyPaste[1]+laQtyPaste[2]+laQtyPaste[3]+laQtyPaste[4]+;
   laQtyPaste[5]+laQtyPaste[6]+laQtyPaste[7]+laQtyPaste[8]=0
  *E300408,1 Message : 32033
  *E300408,1 No line quantities copied to paste!
  *E300408,1 Button : 00000 
  *E300408,1 ok
  =gfModalGen('TRM32033B00000','ALERT')
  RETURN
ENDIF
SELECT IIF(llStores,lcTempLine,lcOrdLine)
STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty
FOR lnCount = 1 TO Scale.Cnt
  lcCount = STR(lnCount,1)
  m.Qty&lcCount = laQtyPaste[lnCount]
  m.TotQty = m.TotQty + m.Qty&lcCount
ENDFOR
=lfChkTotQty(llStores)
SCATTER MEMVAR
IF llStores
  SHOW GETS WINDOW 'SOMULTI2' ONLY
  SHOW GETS WINDOW 'SOMULTI3' ONLY
  SHOW GETS WINDOW 'SOMULTI4' ONLY
  SHOW GETS WINDOW 'SOMULTI5' ONLY
  =lfRefresh('SOMULTI2')
  =lfRefresh('SOMULTI3')
  =lfRefresh('SOMULTI4')
  SHOW WINDOW (lcBrTtlM) REFRESH SAME
ELSE
  SHOW GETS WINDOW (lcWinCh32)  ONLY
  SHOW GETS WINDOW (lcWinCh321) ONLY
  SHOW GETS WINDOW (lcWinCh322) ONLY
  =lfRefresh(lcWinCh321)
  SHOW WINDOW (lcOrdBrow) REFRESH SAME
ENDIF
_CUROBJ=OBJNUM(m.Qty1)

*!*************************************************************
*! Name      : lfvLineNote
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Order line notepad
*!*************************************************************
*! Calls     : ARLNOTES.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvLineNote()
*!*************************************************************
FUNCTION lfvLineNote
PARAMETERS llStores

DO (gcScrDir+"ARLNOTES.SPX")

*B602925,1 Replace only if the file was open specialy in view mode.
*=RLOCK()
*REPLACE Flag WITH IIF(Flag='N' .OR. llStores,Flag,'M')
*UNLOCK
IF llStores AND USED(lcTempline) OR USED(lcOrdLine)
  SELECT IIF(llStores,lcTempline,lcOrdLine)
  =RLOCK()
  REPLACE Flag WITH IIF(Flag='N' .OR. llStores,Flag,'M')
  UNLOCK
ENDIF
*B602925,1 End.
*!*************************************************************
*! Name      : lfvOrdNote
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Order notepad
*!*************************************************************
*! Calls     : NOTEPAD
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvOrdNote()
*!*************************************************************
FUNCTION lfvOrdNote

*E301086,1 Add new Type 'T' for EDI temporary Order 
*=NOTEPAD('B',laData[1])
=NOTEPAD('B',IIF(lcOrdType='T','T','')+laData[1])

*!*************************************************************
*! Name      : lfvRemove
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Remove Order line
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvMRemove()
*!*************************************************************
FUNCTION lfvRemove
PARAMETERS llStores

IF m.Picked AND !EMPTY(m.PikTkt)
  *E300408,1 Message : 32056
  *E300408,1 This XXXXX has been picked. To remove first release the pick ticket.
  *E300408,1 Button : 00000 
  *E300408,1 Ok
  =gfModalGen('TRM32056B00000','ALERT',lcStyHdr)
  RETURN
ENDIF

*E300408,1 Message : 32035
*E300408,1 Are you sure you want to remove this order line?
*E300408,1 Button : 32000
*E300408,1 Yes No
IF gfModalGen('QRM32035B32000','ALERT',IIF(lcOrdType='C','contract','order'))= 1

  *B602479,1 Confirm removing if order line has quantity allocated.
  *B602479,1 Message : 32062
  *B602479,1 Order line has allocated quantity.
  *B602479,1 Allocation will be released.
  *B602479,1 Button : 32005
  *B602479,1 Proceed Cancel
  IF TotCut > 0 AND gfModalGen('QRM32062B32005','ALERT') = 2
    RETURN
  ENDIF
  *B602479,1 (End)

  IF llStores
    lnTotPcs = lnTotPcs - m.TotQty
    lnTotAmt = lnTotAmt - m.TotQty*m.Price
  ELSE
    SELECT (lcOrdLine)
    =RLOCK()    
    REPLACE FLAG WITH IIF(FLAG='N',FLAG,'M')
    UNLOCK

    IF TotCut > 0 
      *E301077,5 Inhance openning files to speed up transaction
      =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
      *E301077,5 (End)
      =SEEK(IIF(Style.Make,'1','2')+m.Order+STR(m.LineNo,6),'CutPick')
      SET ORDER TO TAG 'CUTPKORD' IN (lcAlocated)
      SELECT CutPick
      SCAN REST WHILE TranCd+Order+cOrdLine = ;
                      IIF(Style.Make,'1','2')+m.Order+STR(m.LineNo,6)
        *E301182,1 Update CT/PO line number in the CUTPICK file.
        *IF !SEEK(CutPick.TranCd+CutPick.cTktNo+CutPick.Order+CutPick.cOrdLine,lcAlocated)
        *  INSERT INTO (lcAlocated) (cTktno,TranCd,Order,cOrdLine,Style) VALUES ;
        *  (CutPick.cTktNo,CutPick.Trancd,CutPick.Order,CutPick.cOrdLine,CutPick.Style)
        *ENDIF
        IF !SEEK(CutPick.TranCd+CutPick.cTktNo+CutPick.cTktLineNo+;
                 CutPick.Order+CutPick.cOrdLine,lcAlocated)
          INSERT INTO (lcAlocated) ;
          (cTktno,TranCd,cTktLineNo,Order,cOrdLine,Style) VALUES ;
          (CutPick.cTktNo,CutPick.Trancd,CutPick.cTktLineNo,CutPick.Order,;
           CutPick.cOrdLine,CutPick.Style)
        ENDIF
        *E301182,1 (End)
        SELECT (lcAlocated)
        =RLOCK()
        REPLACE Qty1   WITH 0 ,;
                Qty2   WITH 0 ,;
                Qty3   WITH 0 ,;
                Qty4   WITH 0 ,;
                Qty5   WITH 0 ,;
                Qty6   WITH 0 ,;
                Qty7   WITH 0 ,;
                Qty8   WITH 0 ,;
                TotQty WITH 0
        UNLOCK
        SELECT (lcOrdHdr)
        =RLOCK()
        REPLACE TotCut WITH TotCut - CutPick.TotQty
        UNLOCK
      ENDSCAN
      *E301077,5 Inhance openning files to speed up transaction
      =gfCloseFile('CUTPICK')
      *E301077,5 (End)
      SELECT (lcOrdLine)
    ENDIF
    IF laScrMode[4] .AND. !EMPTY(laData[43]) .AND. ;
       SEEK(STR(BulkLineNo,6),lcBulkOrd) 
      SELECT (lcBulkOrd)
      =RLOCK()
      REPLACE USED1 WITH USED1 - m.Qty1 ,;
              USED2 WITH USED2 - m.Qty2 ,;
              USED3 WITH USED3 - m.Qty3 ,;
              USED4 WITH USED4 - m.Qty4 ,;
              USED5 WITH USED5 - m.Qty5 ,;
              USED6 WITH USED6 - m.Qty6 ,;
              USED7 WITH USED7 - m.Qty7 ,;
              USED8 WITH USED8 - m.Qty8 ,;
              TOTUSED WITH TOTUSED - m.TotQty
      UNLOCK
    ENDIF
    laData[35] = laData[35] - IIF(FLAG='N' OR llUpdBook,m.TotQty,0)
    laData[36] = laData[36] - IIF(FLAG='N' OR llUpdBook,m.TotQty*m.Price,0)
    laData[39] = laData[39] + IIF(FLAG='N' OR llUpdBook,0,m.TotQTy)
    laData[40] = laData[40] + IIF(FLAG='N' OR llUpdBook,0,m.TotQTy)*m.Price
    laData[41] = laData[41] - m.TotQty
    laData[42] = laData[42] - m.TotQty*m.Price
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE BOOK      WITH laData[35] ,;
            BOOKAMT   WITH laData[36] ,;
            CANCEL    WITH laData[39] ,;
            CANCELAMT WITH laData[40] ,;
            OPEN      WITH laData[41] ,;
            OPENAMT   WITH laData[42]
    UNLOCK
    *B603421,1 AMM display the cancelation reason and add record to ORDCANLN table
    *B803652,1 (Begin) Don't Upadte OrdCanLn when removing a line unless checking depltion date.
    *IF laScrMode[3]
    IF laScrMode[3] AND !llUpdBook
    *B803652,1 (End)
      =gfwCodePop(@laCodes,'CCANCRESON','D')
      *B803652,1 (Begin) Update Style,Account,Store,Dyelot also.
      *INSERT INTO (lcOrdCanLn) ;
       (cOrdType,Order,LineNo,Cancelled,cCancReson,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty) VALUES ;
       (lcOrdType,laData[1],m.LineNo,gdSysDate,laCanReason[lnCanReason,2],m.Qty1,m.Qty2,m.Qty3,;
        m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8)
      INSERT INTO (lcOrdCanLn) ;
       (cOrdType,Order,LineNo,Cancelled,cCancReson,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,;
        Style,Account,Store,Dyelot) VALUES ;
       (lcOrdType,laData[1],m.LineNo,gdSysDate,laCanReason[lnCanReason,2],m.Qty1,m.Qty2,m.Qty3,;
        m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,;
        m.Style,m.Account,m.Store,m.Dyelot)
      *B803652,1 (End)        
      SELECT (lcOrdCanLn)
      STORE lcOrdCanLn TO laCodes[6,7],laCodes[6,8]
      STORE 'lcOrdType+laData[1]+STR(m.LineNo,6)' TO laCodes[6,9]
      =gfwCodePop(@laCodes,'CCANCRESON','T')
      STORE gdSysDate TO m.Cancelled
      DO (gcScrDir+"SOORDCLN.SPX")
      STORE '' TO laCodes[6,7],laCodes[6,8],laCodes[6,9]
      =gfAdd_Info(lcOrdCanLn)
    ENDIF 
    *B603421,1 AMM end
  ENDIF
  
  *E301288,1 Reham On 07/21/1999    *** Begin ***
  IF llBomVarnt
    SELECT (lcT_BomVar)
    =SEEK("SO"+laData[1]+STR(&lcOrdLine..LineNo,6))
    SCAN REST WHILE cIdType+cCost_Id+STR(LineNo,6) = "SO" + laData[1] + STR(&lcOrdLine..LineNo,6)
      REPLACE cStatus WITH SUBSTR('DDS',AT(cStatus,'SMA'),1)
    ENDSCAN
    DELETE ALL FOR cIdType+cCost_Id+STR(LineNo,6) = "SO" + laData[1] + STR(&lcOrdLine..LineNo,6)
  ENDIF
  *E301288,1 Reham On 07/21/1999    *** End   ***
  
  SELECT IIF(llStores,lcTempLine,lcOrdLine)
  DELETE
  *B802588,1 AMM Go to previous record
  *GO TOP
  SKIP -1
  IF BOF()
    GO TOP
  ENDIF
  *B802588,1 AMM end
  lcDispDet = IIF(EOF(),'DISABLE','')
  SCATTER MEMVAR
  IF llStores
    SHOW WINDOW (lcBrTtlM) REFRESH SAME
    SHOW GETS WINDOW 'SOMULTI2' &lcDispDet ONLY
    SHOW GETS WINDOW 'SOMULTI3' &lcDispDet ONLY
    SHOW GETS WINDOW 'SOMULTI4' &lcDispDet ONLY
    SHOW GETS WINDOW 'SOMULTI5' &lcDispDet ONLY
    =lfRefresh('SOMULTI2')
    =lfRefresh('SOMULTI3')
    =lfRefresh('SOMULTI4')
    SHOW GET pbClose ENABLE
    _CUROBJ = OBJNUM(ibInv200E)
  ELSE
    SHOW WINDOW (lcOrdBrow) REFRESH SAME
    SHOW GETS WINDOW (lcWinCh32)  &lcDispDet ONLY
    SHOW GETS WINDOW (lcWinCh321) &lcDispDet ONLY
    SHOW GETS WINDOW (lcWinCh322) &lcDispDet ONLY
    =lfRefresh(lcWinCh321)
    _CUROBJ = OBJNUM(ibBrowOrd)
  ENDIF
  IF EOF()
    SHOW GET llMultiSt ENABLE
    SHOW GET pbNew     ENABLE 
    SHOW GET pbPacks   ENABLE
    IF llMultiSt
      SHOW GET pbTemplate ENABLE
    ENDIF
   _CUROBJ = OBJNUM(pbNew)
  ENDIF
  STORE .T. TO llCUpdate
ENDIF

*!*************************************************************
*! Name      : lfvTemplate
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Template order lines
*!*************************************************************
*! Calls     : SOMILTI.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvTemplate()
*!*************************************************************
FUNCTION lfvTemplate
PRIVATE lnOrdRecNo

SELECT (lcOrdline)
lnOrdRecNo = RECNO()
SET RELATION TO
SELECT (lcTempline)
SUM TotQty,TotQty*Price TO lnTotPcs,lnTotAmt
SET RELATION TO Style INTO Style
SET RELATION TO Style+laData[31]+SPACE(10) INTO StyDye ADDITIVE

*B802517,1 Fix Alias not found error
*IF !EMPTY(laData[43])
IF laScrMode[4] AND !EMPTY(laData[43])
*B802517,1 (End)
  SET RELATION TO STR(BulkLineNo,6) INTO (lcBulkOrd) ADDITIVE
ENDIF
*B802431,1 update template order lines when distribute bulk order lines.
SET ORDER TO TAG ORDLINE
*B802431,1 (End)
GO TOP
SCATTER MEMVAR
DO (gcScrDir+gcWinAppl+"\SOMULTI.SPX")
SELECT (lcTempline)
IF SEEK(lcOrdType+laData[1])
  SHOW GET llMultiSt DISABLE
  =gfSavSess(lcProgID,lcFiles+'lcTempline,'+lcTempline+',ORDLINE;',@laVariables,lcSession)
ELSE
  =gfSavSess(lcProgID,lcFiles,@laVariables,lcSession)
ENDIF
SET RELATION TO
SELECT (lcOrdline)
SET RELATION TO Style INTO Style
SET RELATION TO Style+laData[31]+SPACE(10) INTO StyDye ADDITIVE
IF BETWEEN(lnOrdRecNo,1,RECCOUNT())
  GO lnOrdRecNo
ELSE
 GO TOP
ENDIF
SCATTER MEMVAR
=lfActFolder()

*!*************************************************************
*! Name      : lfvShowBulk
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Show bulk order booked, open, used and balance quantities
*!*************************************************************
*! Calls     : ORD100B.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvShowBulk()
*!*************************************************************
FUNCTION lfvShowBulk
PRIVATE lnAlias
lnAlias  = SELECT()
SELECT (lcBulkOrd)
DO (gcScrDir+gcWinAppl+"\SoBulk.SPX")
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvCommision
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Recalculate order commissions
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCommision()
*!*************************************************************
FUNCTION lfvCommision

IF SEEK(laData[27],'SalesRep')
  *B802650,1 AMM Sort the reduce commesion of sales rep
  *DO CASE
    *CASE laData[16] >= SalesRep.Discnt4
      *laData[28] = lnSouComm1 * (1-SalesRep.Redcom4/100)
    *CASE laData[16] >= SalesRep.Discnt3
      *laData[28] = lnSouComm1 * (1-SalesRep.Redcom3/100)
    *CASE laData[16] >= SalesRep.Discnt2
      *laData[28] = lnSouComm1 * (1-SalesRep.Redcom2/100)
    *CASE laData[16] >= SalesRep.Discnt1
      *laData[28] = lnSouComm1 * (1-SalesRep.Redcom1/100)
  *ENDCASE
  DIMENSION laDiscnt[4,2]
  laDiscnt[1,1] = SalesRep.Discnt1
  laDiscnt[1,2] = SalesRep.Redcom1
  laDiscnt[2,1] = SalesRep.Discnt2
  laDiscnt[2,2] = SalesRep.Redcom2
  laDiscnt[3,1] = SalesRep.Discnt3
  laDiscnt[3,2] = SalesRep.Redcom3
  laDiscnt[4,1] = SalesRep.Discnt4
  laDiscnt[4,2] = SalesRep.Redcom4
  
  =ASORT(laDiscnt)
  DO CASE
    CASE laData[16] >= laDiscnt[4,1]
      laData[28] = laData[28] * (1-laDiscnt[4,2]/100)
    CASE laData[16] >= laDiscnt[3,1]
      laData[28] = laData[28] * (1-laDiscnt[3,2]/100)
    CASE laData[16] >= laDiscnt[2,1]
      laData[28] = laData[28] * (1-laDiscnt[2,2]/100)
    CASE laData[16] >= laDiscnt[1,1]
      laData[28] = laData[28] * (1-laDiscnt[1,2]/100)
  ENDCASE
  *B802650,1 AMM end
  
  
  IF laData[28] <> &lcOrdHdr..Comm1
    *E300420,1 Message : 32036
    *E300420,1 xxx commission decreased to xxx%
    *E300420,1 Button : 00000
    *E300420,1 Ok
    =gfModalGen('INM32036B00000','ALERT',laData[27]+'|'+STR(laData[28],5,2))
  ENDIF
ENDIF
IF SEEK(laData[29],'SalesRep')
  *B802650,1 AMM Sort the reduce commesion of sales rep
  *DO CASE
    *CASE laData[16] >= SalesRep.Discnt4
      *laData[30] = lnSouComm2 * (1-SalesRep.Redcom4/100)
    *CASE laData[16] >= SalesRep.Discnt3
      *laData[30] = lnSouComm2 * (1-SalesRep.Redcom3/100)
    *CASE laData[16] >= SalesRep.Discnt2
      *laData[30] = lnSouComm2 * (1-SalesRep.Redcom2/100)
    *CASE laData[16] >= SalesRep.Discnt1
      *laData[30] = lnSouComm2 * (1-SalesRep.Redcom1/100)
  *ENDCASE
  DIMENSION laDiscnt[4,2]
  laDiscnt[1,1] = SalesRep.Discnt1
  laDiscnt[1,2] = SalesRep.Redcom1
  laDiscnt[2,1] = SalesRep.Discnt2
  laDiscnt[2,2] = SalesRep.Redcom2
  laDiscnt[3,1] = SalesRep.Discnt3
  laDiscnt[3,2] = SalesRep.Redcom3
  laDiscnt[4,1] = SalesRep.Discnt4
  laDiscnt[4,2] = SalesRep.Redcom4
  =ASORT(laDiscnt)
  DO CASE
    CASE laData[16] >= laDiscnt[4,1]
      laData[30] = laData[30] * (1-laDiscnt[4,2]/100)
    CASE laData[16] >= laDiscnt[3,1]
      laData[30] = laData[30] * (1-laDiscnt[3,2]/100)
    CASE laData[16] >= laDiscnt[2,1]
      laData[30] = laData[30] * (1-laDiscnt[2,2]/100)
    CASE laData[16] >= laDiscnt[1,1]
      laData[30] = laData[30] * (1-laDiscnt[1,2]/100)
  ENDCASE
  *B802650,1 AMM end

  
  IF laData[30] <> &lcOrdHdr..Comm2
    *E300420,1 Message : 32036
    *E300420,1 xxx commission decreased to xxx%
    *E300420,1 Button : 00000
    *E300420,1 Ok
    =gfModalGen('INM32036B00000','ALERT',laData[29]+'|'+STR(laData[30],5,2))
  ENDIF
ENDIF
SELECT (lcOrdHdr)
=RLOCK()
REPLACE Comm1 WITH laData[28] ,;
        Comm2 WITH laData[30]
UNLOCK
SHOW GET laData[28]
SHOW GET laData[30]

*!*************************************************************
*! Name      : lfvApp2St
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Apply template order lines to stores
*!*************************************************************
*! Calls     : SOSTORE.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvApp2St()
*!*************************************************************
FUNCTION lfvApp2St

STORE 0 TO lnTopMark,lnBotMark,lnSTotPcs,lnSTotAmt
*E500295,1 AMM Dimention The key array expression for the locate feature
DIMENSION laKeyExp[1]
laKeyExp[1] = 'ACCOUNT'
*E500295,1 AMM end 
SELECT *,000000 AS nSesUsed FROM (lcTempline) WHERE Flag = "�" INTO DBF (gcWorkDir+lcSelLines)
SUM TotQty,TotQty*Price TO lnSelPcs,lnSelAmt
SET RELATION TO Style INTO Style
GO TOP
*E301077,5 Inhance openning files to speed up transaction
=gfOpenFile(gcSysHome+'SycInt',gcSysHome+'Ccontcode','SH')
*E301077,5 (End)

*B802926,1 ABD Commend out Cheak if the file is used or not.  [Begin]
*IF !USED(lcStores)
=SEEK(gcContCode,'SycInt')
*E301452,1 ABD [Start]
*AMH Select Active and Potential Stores [Start]
*SELECT Account,Store,StName,cAddress3,cAddress4,cAddress5,000000 AS Pieces,;
*0000000.00 AS AMOUNT, SPACE(15) AS CustPo,SPACE(1) AS cSelect, 000 AS Count;
*FROM Customer WHERE Type+Account+Store='S'+laData[2] .AND. Status='A';
*INTO DBF(gcWorkDir+lcStores)
SELECT Account,Store,StName,cAddress3,cAddress4,cAddress5,000000 AS Pieces,;
0000000.00 AS AMOUNT, SPACE(15) AS CustPo,SPACE(1) AS cSelect, 000 AS Count;
FROM Customer WHERE Type+Account+Store='S'+laData[2] .AND. Status$'AP';
INTO DBF(gcWorkDir+lcStores)
*AMH Select Active and Potential Stores [End  ]
*E301452,1 ABD [End  ]
INDEX ON cSelect+Account+Store TAG (lcStores1)
INDEX ON Account+Store TAG (lcStores)
*ENDIF
*B802926,1 ABD  [End]

DO (gcScrDir+gcWinAppl+"\SOSTORE.SPX")
*E301077,5 Inhance openning files to speed up transaction
=gfCloseFile('SycInt')
*E301077,5 (End)
USE IN (lcSelLines)
*B803083,1 AMM  Rebuild the browse window after comming from the stores screen
=lfBrowMulti()
*B803083,1 AMM  end

*!*************************************************************
*! Name      : lfWStores
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Browse function for stores screen (SOSTORE)
*!*************************************************************
*! Calls     : lfShowStore
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfWStores()
*!*************************************************************
FUNCTION lfWStores
PRIVATE lcFields 

lcFields = [cMarker =IIF(RECNO()=lnTopMark,'>',' '):H=' ':R:1:W=.F.,]+;
           [Style  :H=lcStyHdr :R :19,]
IF laScrMode[4] .AND. !EMPTY(laData[43])
  lcFields = lcFields + [TotQty :H='Pieces' :R :6,]+;
                        [Amount =Price*TotQty :P= '999999.99' :H='Amount',]+;
                        [&lcBulkOrd..TotBook :H= 'Booked',]+;
                        [&lcBulkOrd..TotOpen :H= 'Open' ,]+;
                        [X= &lcBulkOrd..TotUsed+nSesUsed :H= 'Used',]+;
                        [nBalance = MAX(&lcBulkOrd..TotOpen-&lcBulkOrd..TotUsed-nSesUsed,0) :H= 'Balance',]+;
                        [Style.Desc :H='Description':R :20,]+;
                        [Group :H='Grp' :R,]+;
                        [Price :H='Price' :R :9]
ELSE
  lcFields = lcFields + [Style.Desc :H='Description':R :20,]+;
                        [Group :H='Group' :R,Gros_Price :H='Gross Price' :R,]+;
                        [Disc_Pcnt :H='Discount' :R,Price :H='Net Price' :R :9,]+;
                        [TotQty :H='Pieces' :R :6,]+;
                        [Amount  =Price*TotQty :P= '999999.99' :H='Amount']
ENDIF                      
SELECT (lcSelLines)
lnTopMark = RECNO()
BROWSE FIELDS &lcFields ;
       WINDOW SOSTORE1  ;
       IN WINDOW SOSTORE;
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
	   NOCLEAR          ;
       WHEN lfShowStore();
       TITLE lcBrTtS1    ;
       VALID :F lfVBrTtS1()

SELECT (lcStores)
lnBotMark = RECNO()
lcBrFields = "cMarker =IIF(RECNO()=lnBotMark,'>',' '):H=' ':R:1:W=.F.,;
              cSelect :H=' ' :R, Store :H='Store' :8:R, StName :H='Name':R :30,"
lcBrFields = lcBrFields + IIF(laData[7],"CustPo :H='Cust. PO.' :15 :W= cSelect='�',",'')
lcBrFields = lcBrFields + "n=Pieces*Count :H='Pieces' :P='999999' :R,;
             n1=Amount*Count :H='Amount' :P='9999999.99' :R,"
lcBrFields =  lcBrFields + "cAddress3 :H=SycInt.cPart3Lab :R :P=REPLICATE('X',SycInt.nPart3Len),;
              cAddress4 :H=SycInt.cPart4Lab :R :P=REPLICATE('X',SycInt.nPart4Len),;
              cAddress5 :H=SycInt.cPart5Lab :R :P=REPLICATE('X',SycInt.nPart5Len)"

BROWSE FIELDS &lcBrFields ;
       WINDOW SOSTORE3  ;
       IN WINDOW SOSTORE;
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
	   NOCLEAR          ;
       WHEN lfShowStore();
       TITLE lcBrTtS2    ;
       VALID :F lfVBrTtS2()

*!*************************************************************
*! Name      : lfShowStore
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Show function for stores screen (soStore)
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfShowStore()
*!*************************************************************
FUNCTION lfShowStore

lnTopMark = RECNO(lcSelLines)
SHOW WINDOW (lcBrTtS1) REFRESH SAME
lnBotMark = RECNO(lcStores)
SHOW WINDOW (lcBrTtS2) REFRESH SAME
IF &lcStores..cSelect = SPACE(1)
  SHOW GET pbSSelect,1 PROMPT '\<Select'
  SHOW GET pbIncrease DISABLE
  SHOW GET pbDecrease DISABLE
ELSE
  SHOW GET pbSSelect,1 PROMPT 'Un\<Select'
  SHOW GET pbIncrease ENABLE
  SHOW GET pbDecrease ENABLE
ENDIF

*E301452,1 ABD [Start]
*AMH Disable all buttons if there is no Stores [Start]
lnAlias = SELECT(0)
SELECT (lcStores)
LOCATE
IF EOF()
  SHOW GET pbSSelect  DISABLE
  SHOW GET pbSSelAll  DISABLE
  SHOW GET pbSSelNone DISABLE
  SHOW GET pbSInvert  DISABLE
  SHOW GET pbIncrease DISABLE
  SHOW GET pbDecrease DISABLE
  SHOW GET pbSApply   DISABLE
  
*B803710,1 (Begin) Get the selected record.
ELSE
  GO lnBotMark
  SHOW WINDOW (lcBrTtS2) REFRESH SAME
*B803710,1 (End)

ENDIF
SELECT (lnAlias)
*AMH Disable all buttons if there is no Stores [End  ]
*E301452,1 ABD [End  ]

*!*************************************************************
*! Name      : lfDStores
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Deactivate function for stores screen (soStore)
*!*************************************************************
*! Calls     : lpTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfDStores()
*!*************************************************************
FUNCTION lfDStores

IF WONTOP()=lcBrTtS1 .OR. WONTOP()=lcBrTtS2 
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  DO CASE 
    CASE WONTOP() = lcBrTtS1 
      ON KEY LABEL TAB     DO lpTab WITH 'SOSTORE4','ibOrd100S2'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'SOSTORE4','pbClose'
    CASE WONTOP() = lcBrTtS2 
      ON KEY LABEL TAB     DO lpTab WITH 'SOSTORE4','pbSSelect'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'SOSTORE2','ibOrd100S1'
      *E500295,1 AMM Trap keyboard keys
      lcExpToSeek=""
      lcOrdExpr=""
      lnStartTrap = 32
      lnEndTrap   = 126
      FOR lnChrToTrap = lnStartTrap TO lnEndTrap
        ON KEY LABEL (CHR(lnChrToTrap)) DO lfIncSearch &&lfGetRange
      ENDFOR
      *E500295,1 AMM end
  ENDCASE 
ENDIF

RETURN .F.


*!*************************************************************
*! Name      : lfVBrTtS1
*! Developer : Ahmed Ibrahim
*! Date      : 09/22/1999
*! Purpose   : Valid function Of the first browse in MULTI store screen
*! Reference : *E500295,1 AMM
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  
*!*************************************************************

FUNCTION lfVBrTtS1
IF WONTOP() = lcBrTtS2
  lcBrowseTl=lcBrTtS2
  =lfReadAct()
  =lfDStores()
ENDIF

*!*************************************************************
*! Name      : lfVBrTtS2
*! Developer : Ahmed Ibrahim
*! Date      : 09/22/1999
*! Purpose   : Valid function Of the second browse in MULTI store screen
*! Reference : *E500295,1 AMM
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  
*!*************************************************************

FUNCTION lfVBrTtS2
IF WONTOP() = lcBrTtS1
  lcBrowseTl=lcBrTtS1
  =lfReadAct()
  =lfDStores()  
ENDIF



*!*************************************************************
*! Name      : lfAdjSButt
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Select, Unselect, Select all, Select None and Invert stores
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfAdjSButt()
*!*************************************************************
FUNCTION lfAdjSButt
PARAMETERS lcType
PRIVATE lcKey,lcDataSel,lcDataUSel,lnRecNo
SELECT (lcStores)
lcKey = Account+Store
DO CASE 
  CASE lcType = 'S'
    REPLACE cSelect  WITH IIF(cSelect=SPACE(1),"�",SPACE(1)) ,;
            CustPo   WITH IIF(cSelect=SPACE(1),SPACE(15),CustPo) ,;
            Pieces   WITH IIF(cSelect=SPACE(1),0,lnSelPcs) ,;
            Amount   WITH IIF(cSelect=SPACE(1),0,lnSelAmt) ,;
            Count    WITH IIF(cSelect=SPACE(1),0,1)
    SKIP
    IF EOF()
      SKIP -1
    ENDIF
    lcKey = Account+Store
  CASE lcType = 'A'
    REPLACE ALL cSelect WITH "�",;
                Pieces  WITH lnSelPcs ,;
                Amount  WITH lnSelAmt,;
                Count   WITH 1
  CASE lcType = 'N'
    REPLACE ALL cSelect WITH SPACE(1) ,;
                CustPo  WITH SPACE(15) ,;
                Pieces  WITH 0 ,;
                Amount  WITH 0 ,;
                Count   WITH 0
  CASE lcType = 'V'
    REPLACE ALL cSelect WITH IIF(cSelect=SPACE(1),"�",SPACE(1)) ,;
                CustPo  WITH IIF(cSelect=SPACE(1),SPACE(15),CustPo) ,;
                Pieces  WITH IIF(cSelect=SPACE(1),0,lnSelPcs) ,;
                Amount  WITH IIF(cSelect=SPACE(1),0,lnSelAmt) ,;
                Count   WITH IIF(cSelect=SPACE(1),0,1)
  CASE lcType = 'I'
    REPLACE Count WITH Count + 1
  CASE lcType = 'D'
    REPLACE Count WITH Count - 1 ,;
            cSelect WITH IIF(Count=0,SPACE(1),cSelect)
  OTHERWISE
ENDCASE
SET ORDER TO TAG (lcStores1)
lcDataSel  = IIF(SEEK("�"),'ENABLE','DISABLE')
SUM REST Pieces*Count,Amount*Count,Count TO lnSTotPcs,lnSTotAmt,lnStCount ;
    WHILE cSelect = "�"
lcDataUSel = IIF(SEEK(' '),'ENABLE','DISABLE')
SET ORDER TO TAG (lcStores)
=SEEK(lcKey)
lcData = IIF(EOF(),'DISABLE','ENABLE')
SHOW GET pbSSelAll  &lcDataUSel
SHOW GET pbSSelNone &lcDataSel
SHOW GET pbSApply   &lcDataSel
IF cSelect = SPACE(1)
  SHOW GET pbSSelect,1 PROMPT '\<Select'
  SHOW GET pbIncrease DISABLE
  SHOW GET pbDecrease DISABLE
ELSE
  SHOW GET pbSSelect,1 PROMPT 'Un\<Select'
  SHOW GET pbIncrease ENABLE
  SHOW GET pbDecrease ENABLE
ENDIF
SELECT (lcSelLines)
lnRecNo = RECNO()
REPLACE ALL nSesUsed WITH TotQty*lnStCount
GO lnRecNo
SELECT (lcStores)
SHOW GETS WINDOW 'SoStore2' OFF

*!*************************************************************
*! Name      : lfvSApply
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Append selected stores to the order line temp file
*!*************************************************************
*! Calls     : lfAdjSButt
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvSApply()
*!*************************************************************
FUNCTION lfvSApply

SET ORDER TO TAG (lcTempline) IN (lcTempline)
SELECT (lcStores)
SET ORDER TO TAG (lcStores1)
IF SEEK("�")
  llCUpdate = .T.
  SCAN REST WHILE cSelect = "�"
    lnCount=Count
    SELECT (lcTempline)
    =SEEK("�")
    SCAN REST WHILE Flag = "�"
      SCATTER MEMVAR MEMO
      WAIT 'Adding store/style : '+&lcStores..Store+'/'+m.Style WINDOW NOWAIT
      lnLines = lnLines + 1
      IF laScrMode[4] .AND. !EMPTY(cFromOrder) .AND. ;
        SEEK(STR(BulkLineNo,6),lcBulkOrd)
        SELECT (lcBulkOrd)
        =RLOCK()
        REPLACE Used1 WITH Used1 + m.Qty1*lnCount ,;
                Used2 WITH Used2 + m.Qty2*lnCount ,;
                Used3 WITH Used3 + m.Qty3*lnCount ,;
                Used4 WITH Used4 + m.Qty4*lnCount ,;
                Used5 WITH Used5 + m.Qty5*lnCount ,;
                Used6 WITH Used6 + m.Qty6*lnCount ,;
                Used7 WITH Used7 + m.Qty7*lnCount ,;
                Used8 WITH Used8 + m.Qty8*lnCount ,;
                TotUsed WITH TotUsed + m.TotQty*lnCount
        UNLOCK
      ENDIF
      *B803728,1 (Begin) Update complete date also.
      *INSERT INTO (lcOrdLine) ;
      (cOrdType,Order,Account,LineNo,Store,Style,Desc1,Season,Scale,Group,PrePak,PPQty,Price,;
       Gros_Price,Disc_Pcnt,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,;
       Book1,Book2,Book3,Book4,Book5,Book6,Book7,Book8,TotBook,Note_Mem,;
       Comm1,Comm2,Pack_Id,cFromOrder,BulkLineNo,CustPo,Flag,cWareCode,Gl_Sales) VALUES ;
      (lcOrdType,laData[1],laData[2],lnLines,&lcStores..Store,m.Style,m.Desc1,;
       m.Season,m.Scale,m.Group,m.PrePak,m.PPQty,m.Price,m.Gros_Price,m.Disc_Pcnt,;
       m.Qty1*lnCount,m.Qty2*lnCount,m.Qty3*lnCount,m.Qty4*lnCount,m.Qty5*lnCount,;
       m.Qty6*lnCount,m.Qty7*lnCount,m.Qty8*lnCount,m.TotQty*lnCount,m.Qty1*lnCount,;
       m.Qty2*lnCount,m.Qty3*lnCount,m.Qty4*lnCount,m.Qty5*lnCount,m.Qty6*lnCount,;
       m.Qty7*lnCount,m.Qty8*lnCount,m.TotQty*lnCount,m.Note_Mem,m.Comm1,;
       m.Comm2,m.Pack_Id,m.cFromOrder,m.BulkLineNo,&lcStores..CustPo,'N',;
       laData[31],m.Gl_Sales)
      INSERT INTO (lcOrdLine) ;
      (cOrdType,Order,Account,LineNo,Store,Style,Desc1,Season,Scale,Group,PrePak,PPQty,Price,;
       Gros_Price,Disc_Pcnt,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,;
       Book1,Book2,Book3,Book4,Book5,Book6,Book7,Book8,TotBook,Note_Mem,;
       Comm1,Comm2,Pack_Id,cFromOrder,BulkLineNo,CustPo,Flag,cWareCode,Gl_Sales,Complete) VALUES ;
      (lcOrdType,laData[1],laData[2],lnLines,&lcStores..Store,m.Style,m.Desc1,;
       m.Season,m.Scale,m.Group,m.PrePak,m.PPQty,m.Price,m.Gros_Price,m.Disc_Pcnt,;
       m.Qty1*lnCount,m.Qty2*lnCount,m.Qty3*lnCount,m.Qty4*lnCount,m.Qty5*lnCount,;
       m.Qty6*lnCount,m.Qty7*lnCount,m.Qty8*lnCount,m.TotQty*lnCount,m.Qty1*lnCount,;
       m.Qty2*lnCount,m.Qty3*lnCount,m.Qty4*lnCount,m.Qty5*lnCount,m.Qty6*lnCount,;
       m.Qty7*lnCount,m.Qty8*lnCount,m.TotQty*lnCount,m.Note_Mem,m.Comm1,;
       m.Comm2,m.Pack_Id,m.cFromOrder,m.BulkLineNo,&lcStores..CustPo,'N',;
       laData[31],m.Gl_Sales,m.Complete)
       *B803728,1 (Begin)
      laData[35] = laData[35] + m.TotQty*lnCount
      laData[36] = laData[36] + m.TotQty*lnCount*m.Price
      laData[41] = laData[41] + m.TotQty*lnCount
      laData[42] = laData[42] + m.TotQty*lnCount*m.Price
    ENDSCAN
    *B803083,1 AMM Go top , not to be in EOF() !!
    GO TOP
    *B803083,1 AMM  end
  ENDSCAN
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE BOOK    WITH laData[35] ,;
          BOOKAMT WITH laData[36] ,;
          OPEN    WITH laData[41] ,;
          OPENAMT WITH laData[42]
  UNLOCK
  SELECT (lcSelLines)
  REPLACE ALL nSesUsed WITH 0         
  GO TOP  
  SELECT (lcStores)
  REPLACE ALL cSelect WITH SPACE(1) ,;
              CustPo  WITH SPACE(15),;
              Pieces  WITH 0 ,;
              Amount  WITH 0 ,;
              Count   WITH 0
  GO TOP
  WAIT CLEAR
ENDIF
SET ORDER TO TAG (lcStores) IN (lcStores)
SET ORDER TO TAG ORDLINE IN (lcTempline)
=lfAdjSButt('')

*!*************************************************************
*! Name      : lfAdjButt
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Adjust buttons for order template screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfAdjButt()
*!*************************************************************
FUNCTION lfAdjButt
PARAMETERS lcType
PRIVATE lcKey,lcDataSel,lcDataUSel,lcData

SELECT (lcTempLine)
lcKey = cOrdType+Order+STR(LineNo,6)
DO CASE
  CASE lcType = 'S'
    *B802235,1 (Start) added.
    IF EOF()
      GO TOP
    ENDIF  
    *B802235,1 End.
    REPLACE Flag WITH IIF(Flag=SPACE(1),"�",SPACE(1))
    SKIP
    IF EOF()
      SKIP -1
    ENDIF
    lcKey = cOrdType+Order+STR(LineNo,6)
  CASE lcType = 'A'
    REPLACE ALL Flag WITH "�"
  CASE lcType = 'N'
    REPLACE ALL Flag WITH SPACE(1)
  CASE lcType = 'I'
    REPLACE ALL Flag WITH IIF(Flag=SPACE(1),"�",SPACE(1))
  CASE lcType = 'L'
    *E300420,1 Message : 32043
    *E300420,1 Are you sure you want to clear template order lines?
    *E300420,1 Button : 32000 
    *E300420,1 Yes/No
    IF gfModalGen('QRM32043B32000','DIALOG',IIF(lcOrdType='C','contract','order'))=2
      RETURN
    ENDIF
    SELECT (lcTempLine)
    DELETE ALL
    SCATTER MEMVAR BLANK MEMO
    STORE 0 TO lnTotPcs,lnTotAmt
    SHOW WINDOW (lcBrTtlM) REFRESH SAME
    SHOW GETS WINDOW 'SOMULTI2' DISABLE ONLY
    SHOW GETS WINDOW 'SOMULTI3' DISABLE ONLY
    SHOW GETS WINDOW 'SOMULTI4' DISABLE ONLY
    SHOW GETS WINDOW 'SOMULTI5' DISABLE ONLY
    =lfRefresh('SOMULTI2')
    =lfRefresh('SOMULTI3')
    =lfRefresh('SOMULTI42')
    SHOW GET pbNew   ENABLE
    SHOW GET pbPacks ENABLE
    SHOW GET pbClose ENABLE
    _CUROBJ = OBJNUM(pbNew)
    RETURN
  OTHERWISE
ENDCASE  
SET ORDER TO TAG (lcTempLine)
lcDataSel   = IIF(SEEK("�"),'ENABLE','DISABLE')
lcDataUSel  = IIF(SEEK(' '),'ENABLE','DISABLE')
SET ORDER TO TAG OrdLine
=SEEK(lcKey)
lcData  = IIF(EOF(),'DISABLE','ENABLE')
SHOW GET pbSelAll  &lcDataUSel
SHOW GET pbSelNone &lcDataSel
SHOW GET pbStores  &lcDataSel
SHOW GET pbClear   &lcData
SHOW GET pbInvert  &lcData
IF Flag = SPACE(1)
  SHOW GET pbSelect,1 PROMPT '\<Select' &lcData
ELSE
  SHOW GET pbSelect,1 PROMPT 'Un\<Select' &lcData
ENDIF

*!*************************************************************
*! Name      : lfvPacks
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Calls packs screen
*!*************************************************************
*! Calls     : lfAccPacks,gfModalGen,ORD100P.SPX,lfAdjMButt
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvPacks()
*!*************************************************************
FUNCTION lfvPacks
PARAMETERS llStores
PRIVATE lnAlias,lcDataSel

lnAlias = SELECT()

*E301077,5 WAM 02/28/1999 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'SPck_Hdr',gcDataDir+'SPck_Hdr','SH')
=gfOpenFile(gcDataDir+'SPck_Lin',gcDataDir+'SPck_Lin','SH')
*E301077,5 (End)
SELECT SPck_Lin
SET RELATION TO Style INTO Style
*B603784,1 AMM
*SET RELATION TO Style INTO StyDye ADDITIVE
*B603784,1 AMM end

IF !lfAccPacks()
  *E300408,1 Message : 32037
  *E300408,1 There are neither generic packs nor packs for account xxx.
  *E300408,1 Button : 00000
  *E300408,1 Ok
  =gfModalGen('TRM32037B00000','ALERT',laData[2])
  SELECT (lnAlias)
  RETURN
ENDIF
lcAccount = laData[2]
SELECT (lcPackHdr)
SET RELATION TO Type+Account+Pack_Id INTO SPck_Lin
*E301478,1 (Begin) Dimention The key array expression for the locate feature
DIMENSION laKeyExp[1]
laKeyExp[1] = 'ACCOUNT'
*E301478,1 (End)
DO (gcScrDir+"arPacks.SPX") WITH llStores
SELECT (lcPackHdr)
USE
ERASE (gcWorkDir+lcPackHdr+".DBF")
ERASE (gcWorkDir+lcPackHdr+".CDX")

*E301077,5 WAM 02/28/1999 Inhance openning files to speed up transaction
*SELECT SPck_Lin
*SET RELATION OFF INTO Style
*SET RELATION OFF INTO StyDye
=gfCloseFile('SPck_Hdr')
=gfCloseFile('SPck_Lin')
*E301077,5 (End)

*B803862,1 AAN search if there are any selected styles[Start].
SELECT (LcTempLine)
lcoldtag = ORDER()
SET ORDER TO TAG (lcTempLine)
lcDataSel   = IIF(SEEK("�"),'ENABLE','DISABLE')
SET ORDER TO &lcoldtag
*B803862,1 AAN search if there are any selected styles[End].

SELECT (lnAlias)
IF !llStores .AND. llMultiSt .AND. SEEK(lcOrdType+laData[1],lcTempline)
  SHOW GETS WINDOW 'SOMULTI2' ENABLE ONLY
  *B803862,1 AAN Disable or enable a stores button[Start].
  SHOW GET pbStores  &lcDataSel
  *B803862,1 AAN Disable or enable a stores button[End].
  SHOW GETS WINDOW 'SOMULTI3' ENABLE ONLY
  SHOW GETS WINDOW 'SOMULTI4' ENABLE ONLY
  SHOW GETS WINDOW 'SOMULTI5' ENABLE ONLY
  _CUROBJ = OBJNUM(ibInv200E)
ENDIF
IF (llStores .OR. !llMultiSt ) .AND. SEEK(lcOrdType+laData[1],lcOrdline)
  =lfActFolder()
ENDIF

*!*************************************************************
*! Name      : lfAccPacks
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Create temporary file holding account packs and generic packs
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfAccPacks()
*!*************************************************************
FUNCTION lfAccPacks
PRIVATE lnPacks

CREATE DBF (gcWorkDir+lcPackHdr) ;
        (Type C(1),Account C(5),Pack_Id C(16),Desc C(20),Season C(6),;
         cDivision C(6),TotWip N(6),TotStk N(6),TotOrd N(6),TotQty N(6),PackQty N(6))
INDEX ON Account+Pack_Id TAG (lcPackHdr)
*B603784,1 AMM
*SELECT SPck_Hdr
*SET RELATION TO Type+Account+Pack_Id INTO SPck_Lin
SELECT Spck_Lin
SET RELATION TO Type+Account+Pack_Id INTO SPck_Hdr ADDITIVE
*B603784,1 AMM end

lnPacks = 0
*B603784,1 AMM Comment out and write it another way to enhance speed
*IF SEEK('P'+laData[2],'SPck_Hdr')
  *SCAN REST WHILE Type+Account='P'+laData[2] ;
       FOR IIF(ALLTRIM(laData[14])='*',.T.,Season=laData[14]) .AND. cDivision=laData[15]
    *SELECT SPck_Lin
    *SUM REST WHILE Type+Account+Pack_Id='P'+laData[2]+Spck_Hdr.Pack_Id ;
                   Style.TotStk,Style.TotOrd,Style.TotWip, Spck_Lin.TotQty ;
                   TO lnPackStk, lnPackOrd, lnPackWip, lnPackQty
    
    *INSERT INTO (lcPackHdr) ;
    (Type,Account,Pack_Id,Desc,Season,cDivision,TotWip,TotStk,TotOrd,TotQty);
     VALUES ('P',SPck_Hdr.Account,SPck_Hdr.Pack_Id,SPck_Hdr.Desc,SPck_Hdr.Season,;
            SPck_Hdr.cDivision,lnPackWip,lnPackStk,lnPackOrd,lnPackQty)
    *lnPacks = lnPacks + 1
  *ENDSCAN
*ENDIF

*IF SEEK('P*****','SPck_Hdr')
  *SCAN REST WHILE Type+Account='P*****' ;
       FOR IIF(ALLTRIM(laData[14])='*',.T.,Season=laData[14]) .AND. cDivision=laData[15]
    *SELECT SPck_Lin
    *SUM REST WHILE Type+Account+Pack_Id='P*****'+Spck_Hdr.Pack_Id ;
                   Style.TotStk,Style.TotOrd,Style.TotWip, Spck_Lin.TotQty ;
                   TO lnPackStk, lnPackOrd, lnPackWip, lnPackQty
    *INSERT INTO (lcPackHdr) ;
    (Type,Account,Pack_Id,Desc,Season,cDivision,TotWip,TotStk,TotOrd,TotQty);
     VALUES ('P',SPck_Hdr.Account,SPck_Hdr.Pack_Id,SPck_Hdr.Desc,SPck_Hdr.Season,;
            SPck_Hdr.cDivision,lnPackStk, lnPackOrd, lnPackWip, lnPackQty)
    *lnPacks = lnPacks + 1
  *ENDSCAN
*ENDIF

*B803702,1 (Begin) Put a wait window.
WAIT WINDOW 'Collecting data. Please wait.....' NOWAIT
*B803702,1 (End)
IF SEEK('P'+laData[2],'SPck_Lin')
  *B603963,1 (Begin) Get the first record to be updated under needed condition (if any) so that
  *B603963,1         the following variables will belong to it not to unproper record.
  LOCATE REST WHILE Type+Account='P'+laData[2] ;
              FOR IIF(ALLTRIM(laData[14])='*',.T.,SPCK_HDR.Season=laData[14]) .AND. SPCK_HDR.cDivision=laData[15]
  lnPacks = IIF(FOUND(),1,0)
  *B603963,1 (End)              
  SELECT SPck_Lin
  lcPack_Id = SPck_Lin.Pack_Id
  m.Desc = SPck_Hdr.Desc
  m.Season = SPck_Hdr.Season
  m.cDivision = SPck_Hdr.cDivision
  STORE 0 TO lnPackStk, lnPackOrd, lnPackWip, lnPackQty  
  *B803702,1 (Begin)  Let the check of Season and Dividion be on SPCK_HDR not Style for optimization.
  *SCAN REST WHILE Type+Account='P'+laData[2] ;
     FOR !EOF('STYLE') .AND. IIF(ALLTRIM(laData[14])='*',.T.,STYLE.Season=laData[14]) .AND. STYLE.cDivision=laData[15]
  SCAN REST WHILE Type+Account='P'+laData[2] ;
            FOR IIF(ALLTRIM(laData[14])='*',.T.,SPCK_HDR.Season=laData[14]) .AND. SPCK_HDR.cDivision=laData[15]
    WAIT WINDOW lcStyHdr+Style+"\"+Pack_id NOWAIT
    *B803702,1 (End)
    IF lcPack_Id # SPck_Lin.Pack_Id
      INSERT INTO (lcPackHdr) ;
      (Type,Account,Pack_Id,Desc,Season,cDivision,TotWip,TotStk,TotOrd,TotQty);
       VALUES ('P',laData[2],lcPack_Id,m.Desc,m.Season,;
            m.cDivision,lnPackWip,lnPackStk,lnPackOrd,lnPackQty)
      lcPack_Id = SPck_Lin.Pack_Id
      m.Desc = SPck_Hdr.Desc
      m.Season = SPck_Hdr.Season
      m.cDivision = SPck_Hdr.cDivision
      lnPacks = lnPacks + 1
      STORE 0 TO lnPackStk, lnPackOrd, lnPackWip, lnPackQty
    ENDIF
    lnPackQty = lnPackQty+Spck_Lin.TotQty
    lnPackWip = lnPackWip+Style.TotWip
    lnPackOrd = lnPackOrd+Style.TotOrd
    lnPackStk = lnPackStk+Style.TotStk
  ENDSCAN
  *B603963,1 (Begin) Don't insert any record unless it gets into the Scan loop.
  *INSERT INTO (lcPackHdr) ;
  *(Type,Account,Pack_Id,Desc,Season,cDivision,TotWip,TotStk,TotOrd,TotQty);
  *VALUES ('P',laData[2],lcPack_Id,m.Desc,m.Season,;
  *     m.cDivision,lnPackWip,lnPackStk,lnPackOrd,lnPackQty)
  *B603963,1 (End)

  *B803646,1 lnPacks is zero , this means there's only one Account Pack , so display it [Begin]
  *B603963,1 (Begin) Don't insert any record unless it gets into the Scan loop.
  *IF lnPacks =0
  *lnPacks = 1
  *ENDIF
  IF lnPacks > 0
    INSERT INTO (lcPackHdr) ;
                (Type,Account,Pack_Id,Desc,Season,cDivision,TotWip,TotStk,TotOrd,TotQty);
          VALUES ('P',laData[2],lcPack_Id,m.Desc,m.Season,;
                 m.cDivision,lnPackWip,lnPackStk,lnPackOrd,lnPackQty)
  ENDIF
  *B603963,1 (End)
  *B803646,1 lnPacks is zero , this means there's only one Account Pack , so display it [End]  
ENDIF
*B803702,1 (Begin) Put a wait window.
WAIT WINDOW 'Collecting data. Please wait.....' NOWAIT
*B803702,1 (End)
IF SEEK('P*****','SPck_Lin')
  SELECT SPck_Lin
  *B603963,1 (Begin) Get the first record to be updated under needed condition (if any) so that
  *B603963,1         the following variables will belong to it not to unproper record.
  LOCATE REST WHILE Type+Account='P*****';
              FOR IIF(ALLTRIM(laData[14])='*',.T.,SPCK_HDR.Season=laData[14]) .AND. SPCK_HDR.cDivision=laData[15]
  lnPacks = IIF(FOUND(),1,0)              
  *B603963,1 (End)              
  lcPack_Id = SPck_Lin.Pack_Id
  m.Desc = SPck_Hdr.Desc
  m.Season = SPck_Hdr.Season
  m.cDivision = SPck_Hdr.cDivision

  STORE 0 TO lnPackStk, lnPackOrd, lnPackWip, lnPackQty
  *B803702,1 (Begin)  Let the check of Season and Dividion be on SPCK_HDR not Style for optimization.
  *SCAN REST WHILE Type+Account='P*****' ;
      FOR !EOF('STYLE') .AND. IIF(ALLTRIM(laData[14])='*',.T.,STYLE.Season=laData[14]) .AND. STYLE.cDivision=laData[15]
  SCAN REST WHILE Type+Account='P*****' ;
     FOR IIF(ALLTRIM(laData[14])='*',.T.,SPCK_HDR.Season=laData[14]) .AND. SPCK_HDR.cDivision=laData[15]
     WAIT WINDOW lcStyHdr+Style+"\"+Pack_id NOWAIT
     *B803702,1 (End)
    IF lcPack_Id # SPck_Lin.Pack_Id
      *B803646,1 Replace laData[2] which is Account code with '*****' [Begin]
      *INSERT INTO (lcPackHdr) ;
      *(Type,Account,Pack_Id,Desc,Season,cDivision,TotWip,TotStk,TotOrd,TotQty);
      * VALUES ('P',laData[2],lcPack_Id,m.Desc,m.Season,;
      *      m.cDivision,lnPackWip,lnPackStk,lnPackOrd,lnPackQty)
      INSERT INTO (lcPackHdr) ;
      (Type,Account,Pack_Id,Desc,Season,cDivision,TotWip,TotStk,TotOrd,TotQty);
       VALUES ('P','*****',lcPack_Id,m.Desc,m.Season,;
            m.cDivision,lnPackWip,lnPackStk,lnPackOrd,lnPackQty)
      *B803646,1 Replace laData[2] which is Account code with '*****' [End]

      lcPack_Id = SPck_Lin.Pack_Id
      m.Desc = SPck_Hdr.Desc
      m.Season = SPck_Hdr.Season
      m.cDivision = SPck_Hdr.cDivision
      lnPacks = lnPacks + 1
      STORE 0 TO lnPackStk, lnPackOrd, lnPackWip, lnPackQty
    ENDIF
    lnPackQty = lnPackQty+Spck_Lin.TotQty
    lnPackWip = lnPackWip+Style.TotWip
    lnPackOrd = lnPackOrd+Style.TotOrd
    lnPackStk = lnPackStk+Style.TotStk
  ENDSCAN
  *B603963,1 (Begin) Don't insert any record unless it gets into the Scan loop.
  *INSERT INTO (lcPackHdr) ;
               (Type,Account,Pack_Id,Desc,Season,cDivision,TotWip,TotStk,TotOrd,TotQty);
         VALUES ('P','*****',lcPack_Id,m.Desc,m.Season,;
                 m.cDivision,lnPackWip,lnPackStk,lnPackOrd,lnPackQty)
  *B603963,1 (End)
  *B803646,1 lnPacks is zero , this means there's only one generic Pack , so display it [Begin]
  *B603963,1 (Begin) Don't insert any record unless it gets into the Scan loop.
  *IF lnPacks =0
  *lnPacks = 1
  *ENDIF
  IF lnPacks > 0
    INSERT INTO (lcPackHdr) ;
                (Type,Account,Pack_Id,Desc,Season,cDivision,TotWip,TotStk,TotOrd,TotQty);
           VALUES ('P','*****',lcPack_Id,m.Desc,m.Season,;
                   m.cDivision,lnPackWip,lnPackStk,lnPackOrd,lnPackQty)
  ENDIF
  *B603963,1 (End)
  *B803646,1 lnPacks is zero , this means there's only one generic Pack , so display it [End]
ENDIF
*B603784,1 AMM end


*E301077,5 WAM 02/28/1999 Inhance openning files to speed up transaction
*SELECT SPck_Hdr
*SET RELATION OFF INTO SPck_Lin
*E301077,5 (End)
*B803702,1 (Begin) Put the condition upon the reccount() of the temp file.
*RETURN(lnPacks>0)
RETURN(RECCOUNT(lcPackHdr)>0)
*B803702,1 (End)


*!*************************************************************
*! Name      : lfwAccPacks
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : READ WHEN function for packs screen (ORD100P)
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfwAccPacks()
*!*************************************************************
FUNCTION lfwAccPacks

SELECT (lcPackHdr)
*E301478,1 (Begin)
LOCATE
*E301478,1 (End)
lnTopMark = RECNO()
*E301478,1 (Begin) Remove FREEZE clause to activate window in fields other than PackQty field.
*E301478,1         Also add a valid function.
*BROWSE FIELDS ;
       cMarker  =IIF(RECNO()=lnTopMark,'>',' '):H=' ':R:1:W=.F.,;
       Account  :H='Account' :R,;
       Pack_Id  :H='Pack Id' :R ,;
       Desc     :H='Description':R :20,;
       PackQty  :H='Pack' ,;
       TotWip   :H='Wip':R,;
       TotStk   :H='Stock':R,;
       TotOrd   :H='Ordered' :R,;
       TotQty   :H='Qty.' :R,;
       lcSesDesc=gfCodDes(Season,'SEASON') :H= 'Season' :20 :R ,;
       lcDivDesc=gfCodDes(cDivision,'CDIVISION') :H= 'Division' :20 :R ;
       WINDOW arpacks1  ;
       IN WINDOW arpacks;
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
	   NOCLEAR          ;
	   FREEZE 'PackQty' ;
       WHEN lfSAccPack() ;
       TITLE lcBrTtl1
*BROWSE FIELDS ;
       cMarker  =IIF(RECNO()=lnTopMark,'>',' '):H=' ':R:1:W=.F.,;
       Account  :H='Account' :R,;
       Pack_Id  :H='Pack Id' :R,;
       Desc     :H='Description':R :20,;
       PackQty  :H='Pack' :W =lfTrapKeys(.F.) :V=lfTrapKeys(.T.) ,;
       TotWip   :H='Wip':R,;
       TotStk   :H='Stock':R,;
       TotOrd   :H='Ordered' :R,;
       TotQty   :H='Qty.' :R,;
       lcSesDesc=gfCodDes(Season,'SEASON') :H= 'Season' :20 :R ,;
       lcDivDesc=gfCodDes(cDivision,'CDIVISION') :H= 'Division' :20 :R ;
       WINDOW arpacks1  ;
       IN WINDOW arpacks;
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
	   NOCLEAR          ;
       WHEN lfSAccPack() ;
       TITLE lcBrTtl1 ;
       VALID :F lfVPacksWin()
BROWSE FIELDS ;
       cMarker  =IIF(RECNO()=lnTopMark,'>',' '):H=' ':R:1:W=.F.,;
       Account  :H='Account' :R :W=lfVPacksWin() :V=lfVPacksWin() :F,;
       Pack_Id  :H='Pack Id' :R :W=lfVPacksWin() :V=lfVPacksWin() :F,;
       Desc     :H='Description':R :20 :W=lfVPacksWin() :V=lfVPacksWin() :F,;
       PackQty  :H='Pack' :W =lfTrapKeys(.F.) :V= PackQty >= 0 AND lfTrapKeys(.T.),;
       TotWip   :H='Wip':R :W=lfVPacksWin() :V=lfVPacksWin() :F,;
       TotStk   :H='Stock':R :W=lfVPacksWin() :V=lfVPacksWin() :F,;
       TotOrd   :H='Ordered' :R :W=lfVPacksWin() :V=lfVPacksWin() :F,;
       TotQty   :H='Qty.' :R :W=lfVPacksWin() :V=lfVPacksWin() :F,;
       lcSesDesc=gfCodDes(Season,'SEASON') :H= 'Season' :20 :R :W=lfVPacksWin() :V=lfVPacksWin() :F,;
       lcDivDesc=gfCodDes(cDivision,'CDIVISION') :H= 'Division' :20 :R :W=lfVPacksWin() :V=lfVPacksWin() :F;
       WINDOW arpacks1  ;
       IN WINDOW arpacks;
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
	   NOCLEAR          ;
       WHEN lfSAccPack() ;
       TITLE lcBrTtl1 ;
       VALID :F lfVPacksWin()
*E301478,1 (End)       

SELECT Spck_Lin
lnBotMark = RECNO()
*BROWSE FIELDS ;
       cMarker  =IIF(RECNO()=lnBotMark,'>',' '):H=' ':R:1:W=.F.,;
       Style :H = lcStyHdr :R ,;
       Style.TotWip :H= 'WIP' :R ,;
       Style.TotStk :H= 'Stock' :R ,;
       Style.TotOrd :H= 'Ordered' :R ,;
       TotQty :H='Qty.' :R ;
       WINDOW arpacks2   ;
       IN WINDOW arpacks ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
	   NOCLEAR           ;
       WHEN lfPackDet() ;
       TITLE lcBrTtl2
BROWSE FIELDS ;
       cMarker  =IIF(RECNO()=lnBotMark,'>',' '):H=' ':R:1:W=.F.,;
       Style :H = lcStyHdr :R :W=lfTrapKeys(.F.) :V=lfTrapKeys(.F.) :F,;
       Style.TotWip :H= 'WIP' :R :W=lfTrapKeys(.F.) :V=lfTrapKeys(.F.) :F,;
       Style.TotStk :H= 'Stock' :R :W=lfTrapKeys(.F.) :V=lfTrapKeys(.F.) :F,;
       Style.TotOrd :H= 'Ordered' :R :W=lfTrapKeys(.F.) :V=lfTrapKeys(.F.) :F,;
       TotQty :H='Qty.' :R :W=lfTrapKeys(.F.) :V=lfTrapKeys(.F.) :F;
       WINDOW arpacks2   ;
       IN WINDOW arpacks ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
	   NOCLEAR           ;
       WHEN lfPackDet() ;
       TITLE lcBrTtl2 ;
       VALID :F lfTrapKeys(.F.)

*!*************************************************************
*! Name      : lfPackDet
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : When function for packs details browse
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfPackDet()
*!*************************************************************
FUNCTION lfPackDet
*lnBotMark = RECNO('Spck_Lin')
*SHOW WINDOW (lcBrTtl2) REFRESH SAME

*E301478,1 (Begin) Clear trapping of character keys
IF llClearPck
  llClearPck = .F.
  FOR lnChrToTrap = 32 TO 126
    ON KEY LABEL (CHR(lnChrToTrap))
  ENDFOR
ENDIF
*E301478,1 (End)

*!*************************************************************
*! Name      : lfSAccPack
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Show function for packs screen (ORD100P)
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfSAccPack()
*!*************************************************************
FUNCTION lfSAccPack
lnTopMark = RECNO(lcPackHdr)
SHOW WINDOW (lcBrTtl1) REFRESH SAME
lnBotMark = RECNO('Spck_Lin')
SHOW WINDOW (lcBrTtl2) REFRESH SAME
*E301478,1 (Begin) True it.
llClearPck = .T.
*E301478,1 (End)

*!*************************************************************
*! Name      : lfDAccPack
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Deactivate function for packs screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfDAccPack()
*!*************************************************************
FUNCTION lfDAccPack

IF WONTOP() = lcBrTtl1 OR WONTOP() = lcBrTtl2
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL ALT+Z DO lpTab WITH 'arPacks3','pbZoom'
  ON KEY LABEL ALT+A DO lpTab WITH 'arPacks3','pbApply'
  ON KEY LABEL ALT+C DO lpTab WITH 'arPacks3','pbCancel'
  DO CASE
    CASE WONTOP() = lcBrTtl1
      ON KEY LABEL TAB     DO lpTab WITH 'arPacks3','ibPacks2'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'arPacks3','pbCancel'
    CASE WONTOP() = lcBrTtl2
      ON KEY LABEL TAB     DO lpTab WITH 'arPacks3','pbZoom'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'arPacks3','ibPacks1'
  ENDCASE
ENDIF  
RETURN .F.

*!*************************************************************
*! Name      : lfvZoom
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Show pack line details
*!*************************************************************
*! Calls     : arpacksz.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvZoom()
*!*************************************************************
FUNCTION lfvZoom
DO (gcScrDir+"arpacksz.SPX")

*!*************************************************************
*! Name      : lfvPckApply
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Apply selected packs to order lines
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvPckApply()
*!*************************************************************
FUNCTION lfvPckApply
PARAMETERS llStores,lcStore

IF llStores .AND. EMPTY(lcStore)
  RETURN
ENDIF
SELECT (lcPackHdr)
LOCATE FOR PackQty <> 0
IF !FOUND()
  *E300408,1 Message : 32038
  *E300408,1 No packs selected
  *E300408,1 Button : 00000
  *E300408,1 Ok
  =gfModalGen('TRM32038B00000','ALERT','packs')
  GO TOP
  RETURN
ENDIF
STORE .T. TO llCUpdate
SCAN FOR PackQty <> 0
  IF !lfValidPack()
    LOOP
  ENDIF
  SELECT Spck_Lin
  SCAN REST WHILE Type+Account+Pack_Id=&lcPackHdr..Type+&lcPackHdr..Account+&lcPackHdr..Pack_Id
    WAIT 'Adding '+ALLTRIM(Pack_ID)+'/'+ALLTRIM(Style) WINDOW NOWAIT
    =SEEK(Style,'STYLE')

    IF laSetups[5,2]='Y' .AND. !SEEK(Style+laData[31]+SPACE(10),'StyDye')
      DO gpAdStyWar WITH Style,SPACE(10),laData[31]
    ENDIF
    *E300834,1 Get style price according to ordered quantity
    *lnPrice = IIF(!llMulCurr,Style.Price&lcPriceLvl,;
                  gfStyPrice(Style,lcPriceLvl,laData[33]))
    STORE 0 TO m.Gros_Price,m.Price,m.Disc_Pcnt
    *E301086,1 Add new Type 'T' for EDI temporary Order 
    *m.lContract = lcOrdType='O' .AND. lfvContPri(Style,'m.Gros_Price','m.Price','m.Disc_Pcnt')
    m.lContract = lcOrdType<>'C' .AND. lfvContPri(Style,'m.Gros_Price','m.Price','m.Disc_Pcnt')

    IF !m.lContract
      *E301142,1 Select alternative price level
      *lcPriceCatg  = IIF(lcPriceLvl='Q',IIF(TotQty*&lcPackHdr..PackQty>=Style.nAtQtyC,'C',;
      *                   IIF(TotQty*&lcPackHdr..PackQty >=Style.nAtQtyB,'B','A')),lcPriceLvl)
      *m.Gros_Price = IIF(!llMulCurr OR laData[33]=gcBaseCurr,Style.Price&lcPriceCatg,;
      *                   gfStyPrice(Style,lcPriceCatg,laData[33]))
      m.Gros_Price= lfGetprice(Style,lcPriceLvl,TotQty*&lcPackHdr..PackQty)
      *E301142,1 (End)

      lnDisc_Pcnt = 0
      =IIF(EMPTY(Style.CDISCCODE),.T.,gfRltFld(Style.CDISCCODE,@laDisRltFld,'CDISCCODE'))
      m.Disc_Pcnt = lnDisc_Pcnt
      m.Price     = m.Gros_Price*(100-m.Disc_Pcnt)/100
    ENDIF
   *E300834,1 (End)
   *B802324,1 Read field insted of variable.
   *m.Gl_Sales = lcGlSales+Style.cSlsGlLink
   *B803739,1 (Begin) Remove all spaces from LADATA[53].
   *m.Gl_Sales = LADATA[53]+Style.cSlsGlLink
   m.Gl_Sales = ALLTRIM(laData[53])+Style.cSlsGlLink
   *B803739,1 (End)
   *B802324,1 End.
    
    IF laSetups[4,2]='Y' AND !SEEK('02'+m.Gl_Sales,'Gl_Link')
      m.Gl_Sales = 'DEFDEF'
    ENDIF
    IF !llStores .AND. llMultiSt
      SELECT (lcTempLine)
      lnTLines = lnTLines + 1
      lnLineNo = lnTLines
    ELSE
      lnLines  = lnLines + 1
      lnLineNo = lnLines
      SELECT (lcOrdLine)
    ENDIF
    APPEND BLANK
    =RLOCK()
    REPLACE cOrdType   WITH lcOrdType ,;
            Order      WITH laData[1] ,; 
            Account    WITH laData[2] ,;
            Store      WITH IIF(llStores,lcStore,laData[3]) ,;
            LineNo     WITH lnLineNo  ,;
            Style      WITH Spck_Lin.Style ,;
            Desc1      WITH Style.Desc1 ,;
            Scale      WITH Style.Scale ,;
            Season     WITH Style.Season ,;
            Gros_Price WITH m.Gros_Price ,;
            Price      WITH m.Price ,;
            Disc_Pcnt  WITH m.Disc_Pcnt
    REPLACE Qty1       WITH Spck_Lin.Qty1*&lcPackHdr..PackQty,;
            Qty2       WITH Spck_Lin.Qty2*&lcPackHdr..PackQty,;
            Qty3       WITH Spck_Lin.Qty3*&lcPackHdr..PackQty,;
            Qty4       WITH Spck_Lin.Qty4*&lcPackHdr..PackQty,;
            Qty5       WITH Spck_Lin.Qty5*&lcPackHdr..PackQty,;
            Qty6       WITH Spck_Lin.Qty6*&lcPackHdr..PackQty,;
            Qty7       WITH Spck_Lin.Qty7*&lcPackHdr..PackQty,;
            Qty8       WITH Spck_Lin.Qty8*&lcPackHdr..PackQty,;
            TotQty     WITH Spck_Lin.TotQty*&lcPackHdr..PackQty ,;
            Comm1      WITH laData[28] ,;
            Comm2      WITH laData[30] ,;
            cWareCode  WITH laData[31] ,;
            Gl_Sales   WITH m.Gl_Sales ,;
            Pack_Id    WITH &lcPackHdr..Pack_Id ,;
            lContract  WITH m.lContract
    IF !llStores .AND. llMultiSt
      lnTotPcs = lnTotPcs + TotQty
      lnTotAmt = lnTotAmt + TotQty*Price
    ELSE
      REPLACE Flag    WITH 'N' ,;
              Book1   WITH Spck_Lin.Qty1*&lcPackHdr..PackQty,;
              Book2   WITH Spck_Lin.Qty2*&lcPackHdr..PackQty,;
              Book3   WITH Spck_Lin.Qty3*&lcPackHdr..PackQty,;
              Book4   WITH Spck_Lin.Qty4*&lcPackHdr..PackQty,;
              Book5   WITH Spck_Lin.Qty5*&lcPackHdr..PackQty,;
              Book6   WITH Spck_Lin.Qty6*&lcPackHdr..PackQty,;
              Book7   WITH Spck_Lin.Qty7*&lcPackHdr..PackQty,;
              Book8   WITH Spck_Lin.Qty8*&lcPackHdr..PackQty,;
              TotBook WITH Spck_Lin.TotQty*&lcPackHdr..PackQty
      laData[35] = laData[35] + TotQty
      laData[36] = laData[36] + TotQty*Price
      laData[41] = laData[41] + TotQty
      laData[42] = laData[42] + TotQty*Price
    ENDIF
    UNLOCK
  ENDSCAN
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE BOOK    WITH laData[35] ,;
          BOOKAMT WITH laData[36] ,;
          OPEN    WITH laData[41] ,;
          OPENAMT WITH laData[42]
  UNLOCK
ENDSCAN
WAIT CLEAR
CLEAR READ

*!*************************************************************
*! Name      : lfValidPack
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Validate packs before add to the order lines
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfValidPack()
*!*************************************************************
FUNCTION lfValidPack
PRIVATE llValidPack,llOnHoldAll,llAssignAll

llValidPack = .T.
STORE .F. TO llOnHoldAll,llAssignAll
SELECT Spck_Lin
SCAN REST WHILE Type+Account+Pack_Id=&lcPackHdr..Type+&lcPackHdr..Account+&lcPackHdr..Pack_Id

  IF !SEEK('S'+Style.Scale,'Scale')
    *E300408,1 Message : 32039
    *E300408,1 Pack xxx contains one or more xxx. xxx
    *E300408,1 Button : 00000
    *E300408,1 Ok
    =gfModalGen('TRM32039B00000','ALERT',&lcPackHdr..Pack_Id+'|'+'style with non existing scale'+'|'+'Pack will be ignored.')
    llValidPack = .F.
    EXIT
  ENDIF  
  IF Style.Status = 'X'
    *E300408,1 Message : 32039
    *E300408,1 Pack xxx contains one or more xxx. xxx
    *E300408,1 Button : 00000
    *E300408,1 Ok
    =gfModalGen('TRM32039B00000','ALERT',&lcPackHdr..Pack_Id+'|'+'canceled style'+'|'+'Pack will be ignored.')
    llValidPack = .F.
    EXIT
  ENDIF
  IF !llOnHoldAll .AND. Style.Status = 'H'
    *E300408,1 Message : 32040
    *E300408,1 Pack xxx contains one or more on-hold style/color.
    *E300408,1 Button : 32005
    *E300408,1 Proceed Cancel
    IF gfModalGen('QRM32040B32005','ALERT',&lcPackHdr..Pack_Id)=1
      llOnHoldAll = .T.
    ELSE
      llValidPack= .F.
      EXIT
    ENDIF
  ENDIF
  IF laSetups[5,2]='Y' .AND. !llAssignAll .AND. ;
     !SEEK(Style+laData[31]+SPACE(10),'StyDye')
    *E300408,1 Message : 32041
    *E300408,1 Pack xxx contains one or more style/color that is not assign
    *E300408,1 to warehouse xxx. Assign all styles/colors to this warehouse?
    *E300408,1 Button : 32000
    *E300408,1 Yes No
    IF gfModalGen('QRM32041B32000','ALERT',&lcPackHdr..Pack_Id+'|'+laData[31])=1
      llAssignAll = .T.
    ELSE
      llValidPack = .F.
      EXIT
    ENDIF
  ENDIF   
  *E301142,1 Select alternative price level if zero price
  **E300834,1 Get style price according to ordered quantity
  **lnPrice = IIF(!llMulCurr,Style.Price&lcPriceLvl,;
  *              gfStyPrice(Style,lcPriceLvl,laData[33]))
  *STORE 0 TO lnPrice,m.Price,m.Disc_Pcnt
  **E301086,1 Add new Type 'T' for EDI temporary Order 
  **m.lContract = lcOrdType='O' .AND. lfvContPri(Style,'lnPrice','m.Price','m.Disc_Pcnt')
  *m.lContract = lcOrdType<>'C' .AND. lfvContPri(Style,'lnPrice','m.Price','m.Disc_Pcnt')
  *IF !m.lContract
  *  lcPriceCatg  = IIF(lcPriceLvl='Q',IIF(TotQty*&lcPackHdr..PackQty>=Style.nAtQtyC,'C',;
  *                     IIF(TotQty*&lcPackHdr..PackQty >=Style.nAtQtyB,'B','A')),lcPriceLvl)
  *  lnPrice = IIF(!llMulCurr OR laData[33]=gcBaseCurr,Style.Price&lcPriceCatg,;
  *                     gfStyPrice(Style,lcPriceCatg,laData[33]))
  *ENDIF  
  **E300834,1 (End)
  *IF lnPrice < 0
  *  llValidPack = .F.
  *  EXIT
  *ENDIF
  *E301142,1 (End)
ENDSCAN
=SEEK(&lcPackHdr..Type+&lcPackHdr..Account+&lcPackHdr..Pack_Id)
RETURN(llValidPack)

*!*************************************************************
*! Name      : lfvPStore
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Order store
*!*************************************************************
*! Calls     : CUSBROW,gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvPStore()
*!*************************************************************
FUNCTION lfvPStore
PARAMETERS xStore

IF llBrowse .OR. (!EMPTY(xStore) .AND. ;
                  !SEEK('S'+laData[2]+xStore,'CUSTOMER'))
  IF !CUSBROWS(laData[2],.T.)
    llBrowse = .F.
    STORE SPACE(8) TO xStore
    RETURN
  ENDIF
ENDIF  
llBrowse = .F.
IF !EMPTY(xStore) .AND. Customer.Status <> 'A' 
  *E300420,1 Message : 32023
  *E300420,1 This a non-active xxxxx. Order entry is not allowed!
  *E300420,1 Button : 00000
  *E300420,1 Ok
  =gfModalGen('TRM32023B00000','ALERT','store'+'|'+IIF(lcOrdType='C','Contract','Order'))
  STORE SPACE(8) TO xStore
  RETURN
ENDIF

*!*************************************************************
*! Name      : lpSavScr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Save new or modified order
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpSavScr()
*!*************************************************************
FUNCTION lpSavScr

*E301175,1 I have removed the code of the function lpSavScr to a new
*E301175,1 program called SOUPDATE with the name lfSavScr to add the
*E301175,1 capability to call the sales order Save function from
*E301175,1 the EDI [Begin]
DO lfSavScr IN (gcAppHome + 'SO\SOUPDATE.FXP')
*E301175,1 I have removed the code of the functions lpSavScr [End]

*!*************************************************************
*! Name      : lfGtOrder
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Read order number manually
*!*************************************************************
*! Calls     : ARORDER.SPX
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  Order#
*!*************************************************************
*! Example            :  =lfGtOrder()
*!*************************************************************
FUNCTION lfGtOrder
PRIVATE lcOrderNo

STORE SPACE(6) TO lcOrderNo
*B802231,1 Consider order type when enter order# manualy
*DO (gcScrDir+"ARORDER.SPX") WITH 'O','lcOrderNo'
DO (gcScrDir+"ARORDER.SPX") WITH 'O','lcOrderNo',lcOrdType
*B802231,1 (End)

RETURN(lcOrderNo)

*!*************************************************************
*! Name      : lfPrepBrow
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Prepare files and relations according to current selected 
*!             Transaction, sort, total or per size qty and details or summary 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfPrepBrow()
*!*************************************************************
FUNCTION lfPrepBrow
WAIT 'Colecting data...Please standby.' WINDOW NOWAIT
lcForKey = '.T.'
*E301077,5 Inhance openning files to speed up transaction
DO CASE
  *-- Show Shipped Transactions
  CASE lnOrdTrans= 3
    *B602621,1 Do not show voided invoices
    =gfOpenFile(gcDataDir+'InvHdr',gcDataDir+'InvHdr','SH')
    lcForKey = "INVHDR.STATUS <> 'V'"
    *B602621,1 (End)
    =gfOpenFile(gcDataDir+'InvLine',gcDataDir+'InvLine','SH')
    =gfOpenFile(gcDataDir+'consinvl',gcDataDir+'CINVLINE','SH')
  *-- Show Production Transactions
  CASE lnOrdTrans= 4
    IF 'PO' $ gcCmpModules
      =gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
      =gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLN','SH')
      SET RELATION TO CSTYTYPE+PO INTO PosHdr
    ENDIF
    IF 'MF' $ gcCmpModules
      =gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')
      =gfOpenFile(gcDataDir+'CUTTKTL',gcDataDir+'CUTTKTL','SH')
      SET RELATION TO CutTkt INTO CutTktH
    ENDIF
    =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
    IF 'PO' $ gcCmpModules
      SET RELATION TO 'P'+cTktNo+Style INTO PoSLn
    ENDIF
    IF 'MF' $ gcCmpModules
      SET RELATION TO cTktNo+Style INTO CutTktL ADDITIVE
    ENDIF
  *-- Show Cancelled Transactions
  CASE lnOrdTrans= 7
    =gfOpenFile(gcDataDir+'ORDCANLN',gcDataDir+'ORDCANLN','SH')
  *E301288,1 Reham On 07/27/1999    *** Begin ***
  *E301288,1 Open the cutpick file.
  *-- Show Work Order
  CASE lnOrdTrans= 8
    =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
  *E301288,1 Reham On 07/27/1999    *** End   ***
ENDCASE
*E301077,5 (End)
DO CASE
  *-- Sort by line# - Total quantity
  CASE lnSortBy = 1 .AND. !llQtyPSize
    lcOrderFile = IIF(laScrMode[2] .OR. laScrMode[1],'ORDLINE',lcOrdLine)
    SET ORDER TO TAG 'ORDLINE' IN (lcOrderFile)
    *--HDM B802148,1 04/20/1999 [Start] Add Picked Quantity Field To the Details Browse
    *lcOrderBr = [STYLE :R :H=lcStyHdr,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                IIF(llMultiSt,[STORE :R ,],'') +;
                IIF(laData[7],IIF(laScrMode[2],[CustPo  :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [GROUP :H='Group' :R,Gros_Price :H='Gross Price' :R,] +;
                [Disc_Pcnt :H='Discount' :R,PRICE :P='999999.99' :R,] +;
                [TOTQTY :H= 'Qty.' :P='999999':R ,] +;
                [lnAmount=PRICE*TOTQTY :H='Amount':R:P='9999999.99',] +;
                [PIKTKT :H='Piktkt' :R ,] +;
                [PIKDATE :H='Date' :R]

    *B802772,4 AMM Add a status field in case of EDI modified orders
    *lcOrderBr = [STYLE :R :H=lcStyHdr,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                IIF(llMultiSt,[STORE :R ,],'') +;
                IIF(laData[7],IIF(laScrMode[2],[CustPo  :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [GROUP :H='Group' :R,Gros_Price :H='Gross Price' :R,] +;
                [Disc_Pcnt :H='Discount' :R,PRICE :P='999999.99' :R,] +;
                [TOTQTY :H= 'Qty.' :P='999999':R ,] +;
                [lnAmount=PRICE*TOTQTY :H='Amount':R:P='9999999.99',] +;
                [PIKTKT :H='Piktkt' :R ,] +;
                [TOTPIK :H='Picked' :R ,] +;
                [PIKDATE :H='Date' :R]
                
    *E301401,1 ABD For EDI Temporary Orders, change price columns to labelled
    *E301401,1 ABD "Transmitted Price" and Amount to labelled "Wholesale Price," 
    *E301401,1 ABD Both on-screen and in outputs. [Begin]
    *lcOrderBr = [STYLE :R :H=lcStyHdr,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                IIF(llMultiSt,[STORE :R ,],'') +;
                IIF(laData[7],IIF(laScrMode[2],[CustPo  :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [GROUP :H='Group' :R,Gros_Price :H='Gross Price' :R,] +;
                [Disc_Pcnt :H='Discount' :R,PRICE :P='999999.99' :R,] +;
                [TOTQTY :H= 'Qty.' :P='999999':R ,] +;
                [lnAmount=PRICE*TOTQTY :H='Amount':R:P='9999999.99',] +;
                IIF(ORDHDR.cOrdType='T' .AND. ORDHDR.MON_FLG='G' ,;
                "Status=lfEDIStat() :H='Status' :R :18",;
                [PIKTKT :H='Piktkt' :R ,] +;
                [TOTPIK :H='Picked' :R ,] +;
                [PIKDATE :H='Date' :R])
                
    lcOrderBr = [STYLE :R :H=lcStyHdr,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                IIF(llMultiSt,[STORE :R ,],'') +;
                IIF(laData[7],IIF(laScrMode[2],[CustPo  :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [GROUP :H='Group' :R,Gros_Price :H='Gross Price' :R,] +;
                [Disc_Pcnt :H='Discount' :R,PRICE :H=IIF(ORDHDR.cOrdType='T','Transmitted Price','Price'):R:P='999999.99' :R,] +;
                [TOTQTY :H= 'Qty.' :P='999999':R ,] +;
                [lnAmount=PRICE*TOTQTY :H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='9999999999.99',] +;
                IIF(ORDHDR.cOrdType='T' .AND. ORDHDR.MON_FLG='G' ,;
                "Status=lfEDIStat() :H='Status' :R :18",;
                [PIKTKT :H='Piktkt' :R ,] +;
                [TOTPIK :H='Picked' :R ,] +;
                [PIKDATE :H='Date' :R])
    *E301401,1 ABD [End]
    *B802772,4 AMM end
    *--HDM B802148,1 04/20/1999 [End]
    DO CASE
      *-- Open quantity
      CASE lnOrdTrans= 1
        SELECT (lcOrderFile)
        IF !laScrmode[1]
          SET RELATION TO Style INTO Style
          SET RELATION TO Style+laData[31]+SPACE(10) INTO StyDye ADDITIVE
        ENDIF
      *-- Booked quantity
      CASE lnOrdTrans= 2
        IF !USED(lcBooked)
          *B802405,1 AMM Open file with gfOpenFile()
          *USE (gcDataDir+'ORDLINE') AGAIN ALIAS (lcBooked) IN 0
          =gfOpenFile(gcDataDir+'ORDLINE','','SH',@lcBooked,.T.)
          *B802405,1 AMM end
        ENDIF
        lcBrTtlT   = lcBrTtlB
        lcTranFile = lcBooked
        SET ORDER TO TAG 'ORDLINE' IN (lcBooked)
        SELECT (lcOrderFile)
        SET RELATION TO cOrdType+ORDER+STR(LINENO,6) INTO (lcBooked)
        lcTranBr = [STYLE :R ,STORE :R ,] +;
                   [GROUP :H='Grp' :R ,PRICE :R ,TOTBOOK :H='Qty.' :R ,] +;
                   [lnAmount=PRICE*TOTBOOK :H='Amount':R:P='9999999.99']
      
      *-- Shipped quantity
      CASE lnOrdTrans= 3
        lcBrTtlT = lcBrTtlS
        SET ORDER TO TAG 'CINVLINE' IN 'CONSINVL'
        IF SEEK(laData[1],'CONSINVL')
          lcTranFile = 'CONSINVL'
        ELSE
          lcTranFile = 'INVLINE'
          SET ORDER TO TAG 'INVLINEO' IN 'INVLINE'
        ENDIF
        *B602621,1 Do not show voided invoices
        SELECT (lcTranFile)
        SET RELATION TO INVOICE INTO INVHDR
        *B602621,1 (End)
        SELECT (lcOrderFile)
        SET RELATION TO ORDER+STR(LINENO,6) INTO (lcTranFile)
        lcTranBr = [INVOICE:H='Invoice#' :R ,INVDATE:H='Date' :R,] +;
                   [STORE :H='Store' :R,TOTQTY :H='Quantity' :R,PRICE :H='Price' :R,] +;
                   [lnAmount=PRICE*TOTQTY :H='Amount':R:P='9999999.99']
      
      *-- Production quantity
      CASE lnOrdTrans= 4
        SELECT (lcOrderFile)
        SET RELATION TO Style INTO STYLE
        SET RELATION TO IIF(Style.Make,"1","2")+Order+STR(LineNo,6) INTO CutPick ADDITIVE
        =SEEK(lcOrdType+laData[1])
        lcBrTtlT  = lcBrTtlP
        lcTranFile = 'CutPick'
        lcTranBr1 = [CutPick.cTktNo :H='Tran #' :R ,] +;
                    [Type1='C/T' :H='Type' :R ,] +;
                    [CutTktH.ENTERED :R,CutTktH.Complete :R,] +;
                    [CutPick.TotQty :H='Qty. Alo.' :R ,] +;
                    [CutTktL.TotQty :H='Qty. Ord.' :R ,] +;
                    [lnTotQty=CutTktL.TotQty-CutTktL.TotOrd :H='Qty. Opn.':R] 

        lcTranBr2 = [CutPick.cTktNo :H='Tran #' :R ,] +;
                    [Type2='P/O' :H='Type' :R ,] +;
                    [PosHdr.ENTERED :R,PosHdr.Complete :R,] +;
                    [PosHdr.Vendor  :H='Vendor' :R,] +;
                    [CutPick.TotQty :H='Qty. Alo.' :9:R,] +;
                    [PosLn.TotQty   :H='Qty. Ord.' :9:R ,] +;
                    [lnTotQty=PosLn.TotQty-PosLn.TotOrd   :H='Qty. Opn.' :9:R]
        lcTranBr=IIF(Style.Make,lcTranBr1,lcTranBr2)
        llDomestic = Style.Make
      
      *-- Depleted orders
      CASE lnOrdTrans= 5
        IF !USED(lcDepleted) .OR. !SEEK(laData[1],lcDepleted)
          SET ORDER TO TAG ORDACCT IN ORDHDR
          SET ORDER TO TAG ORDLINE IN ORDLINE
          *B802544,1 Fix Inquire depleted orders
          *SELECT ORDHDR.ORDER,ORDLINE.STYLE,ORDLINE.PRICE,;
          ORDLINE.STORE,ORDLINE.BOOK1,ORDLINE.BOOK2,ORDLINE.BOOK3,ORDLINE.BOOK4,;
          ORDLINE.BOOK5,ORDLINE.BOOK6,ORDLINE.BOOK7,ORDLINE.BOOK8,ORDLINE.TOTBOOK,;
          ORDLINE.CFROMORDER,ORDLINE.BULKLINENO,ORDLINE.COMM1,ORDLINE.COMM2,;
          ORDLINE.CUSTPO,ORDLINE.GROUP,ORDLINE.PIKTKT,ORDLINE.PIKDATE;
          FROM ORDHDR,ORDLINE;
          WHERE ORDHDR.ACCOUNT+ORDHDR.cOrdType+ORDHDR.ORDER=laData[2]+lcOrdType AND ;
          ORDHDR.cFromOrder=laData[1] AND ;
          ORDLINE.cOrdType+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)=;
          lcOrdType+ORDHDR.ORDER INTO CURSOR (lcDepleted)

          SELECT ORDHDR.ORDER,ORDLINE.STYLE,ORDLINE.PRICE,;
          ORDLINE.STORE,ORDLINE.BOOK1,ORDLINE.BOOK2,ORDLINE.BOOK3,ORDLINE.BOOK4,;
          ORDLINE.BOOK5,ORDLINE.BOOK6,ORDLINE.BOOK7,ORDLINE.BOOK8,ORDLINE.TOTBOOK,;
          ORDLINE.CFROMORDER,ORDLINE.BULKLINENO,ORDLINE.COMM1,ORDLINE.COMM2,;
          ORDLINE.CUSTPO,ORDLINE.GROUP,ORDLINE.PIKTKT,ORDLINE.PIKDATE;
          FROM ORDHDR,ORDLINE;
          WHERE ORDHDR.ACCOUNT+ORDHDR.cOrdType+ORDHDR.ORDER=laData[2]+lcOrdType AND ;
          ORDLINE.cFromOrder=laData[1] AND ;
          ORDLINE.cOrdType+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)=;
          lcOrdType+ORDHDR.ORDER INTO CURSOR (lcDepleted)
          *B802544,1 (End)

          INDEX ON CFROMORDER+STR(BULKLINENO,6) TAG (lcDepleted)
          SET ORDER TO TAG ORDHDR IN ORDHDR          
        ENDIF
        lcBrTtlT   = lcBrTtlD
        lcTranFile = lcDepleted
        SELECT (lcOrderFile)
        SET RELATION TO ORDER+STR(LINENO,6) INTO (lcDepleted)
        lcTranBr = [ORDER :R ,STYLE :R ,STORE :R ,] +;
                   [GROUP :H='Grp' :R ,PRICE :R ,TOTBOOK :H= 'Qty.' :R ,] +;
                   [lnAmount=PRICE*TOTBOOK :10:H='Amount':R:P='9999999.99',] +;
                   [PIKTKT :H='Piktkt' :6:R ,PIKDATE :H='Date' :8:R]
      
      *-- Bulk order details
      CASE lnOrdTrans= 6
        lcBrTtlT   = lcBrTtlK
        lcTranFile = lcBulkOrd
        SELECT (lcOrderFile)
        SET RELATION TO STR(BulkLineNo,6) INTO (lcBulkOrd)
        lcTranBr = [STYLE :R ,TotBook :H= 'Tot. Book' :10 :R ,] +;
                   [TotOpen :H= 'Tot. Open' :10 :R ,TotUsed :H= 'Tot. Used' :10 :R ,] +;
                   [Balance = MAX(TotOpen-TotUsed,0) :H= 'Balance' :10:R]
      *E300956,1 Order lines canceled quantity
      CASE lnOrdTrans= 7
        lcBrTtlT   = lcBrTtlC
        lcTranFile = IIF(laScrMode[2],'ORDCANLN',lcOrdCanLn)
        SELECT (lcOrderFile)
        SET RELATION TO cOrdType+Order+STR(LineNo,6) INTO (lcTranFile)
        lcTranBr = [Cancelled :R :H='Date',TotQty :H= 'Total Canceld' :R,]+;
                   [cDesc=gfCodDes(cCancReson,'CCANCRESON') :R :H='Cancelation Reason']
      *E301288,1 Reham On 07/26/1999   *** Begin ***
      *E301288,1 Add new bar to dsplay related records from CutPick file.
      CASE lnOrdTrans = 8
        lcBrTtlT   = lcBrTtlW
        lcTranFile = "CutPick"
        lcTranBr = [lnDummy=IIF(TranCd='1','CT #',IIF(TranCd='2','PO #','Adornment')) :R:10:H='Tran. Type',]+;
                   [cTktNo:R:H='Tran. #',Order :R:H='Order #', Style:R:20, Qty1:R, Qty2:R,]+;
                   [Qty3:R, Qty4:R, Qty5:R, Qty6:R, Qty7:R, Qty8:R, TotQty:R]
      *E301288,1 Reham On 07/26/1999   *** End   ***
    ENDCASE
  *-- Sort by line# - Quantity per size
  CASE lnSortBy = 1 .AND. llQtyPSize
    lcOrderFile = IIF(laScrMode[2],'ORDLINE',lcOrdLine)
    SET ORDER TO TAG 'ORDLINE' IN (lcOrderFile)
    *--HDM B802148,1 04/20/1999 [Start] Add Picked Quantity Field To the Details Browse
    *lcOrderBr = [STYLE :H=lcStyHdr :R,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +IIF(llMultiSt,[STORE :8 :R,],'') +;
                IIF(laData[7],IIF(lcScrMode='V',[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [TOTQTY :P='999999' :R,lnAmount=PRICE*TOTQTY :10:H='Amount':R:P='9999999.99',]+;
                [Qty1 :P='99999':R,Qty2 :P='99999':R,Qty3 :P='99999':R,Qty4 :P='99999':R,]+;
                [Qty5 :P='99999':R,Qty6 :P='99999':R,Qty7 :P='99999':R,Qty8 :P='99999':R]+;
                IIF(laSetups[2,2]='Y',",n=laData[27] :H='Rep 1':R ,COMM1 :H='Comm 1' :R,n1=laData[29] :H='Rep 2':R ,COMM2 :H='Comm 2' :R",'')+;
                IIF(laData[7],",CUSTPO :H='Cust. PO#':R",'')
    *E301401,1 ABD For EDI Temporary Orders, change price columns to labelled
    *E301401,1 ABD "Transmitted Price" and Amount to labelled "Wholesale Price," 
    *E301401,1 ABD Both on-screen and in outputs. [Begin]
    *lcOrderBr = [STYLE :H=lcStyHdr :R,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +IIF(llMultiSt,[STORE :8 :R,],'') +;
                IIF(laData[7],IIF(lcScrMode='V',[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [TOTQTY :P='999999' :R, lnAmount=PRICE*TOTQTY :10:H='Amount':R:P='9999999.99',TOTPIK :H='Tot.Pik.' :R ,]+;
                [Qty1 :P='99999':R,Qty2 :P='99999':R,Qty3 :P='99999':R,Qty4 :P='99999':R,]+;
                [Qty5 :P='99999':R,Qty6 :P='99999':R,Qty7 :P='99999':R,Qty8 :P='99999':R,]+;
                [PIK1 :P='99999':R,PIK2 :P='99999':R,PIK3 :P='99999':R,PIK4 :P='99999':R,]+;
                [PIK5 :P='99999':R,PIK6 :P='99999':R,PIK7 :P='99999':R,PIK8 :P='99999':R,TOTPIK :H='Tot.Pik.']+;
                IIF(laSetups[2,2]='Y',",n=laData[27] :H='Rep 1':R ,COMM1 :H='Comm 1' :R,n1=laData[29] :H='Rep 2':R ,COMM2 :H='Comm 2' :R",'')+;
                IIF(laData[7],",CUSTPO :H='Cust. PO#':R",'')

    lcOrderBr = [STYLE :H=lcStyHdr :R,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +IIF(llMultiSt,[STORE :8 :R,],'') +;
                IIF(laData[7],IIF(lcScrMode='V',[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [TOTQTY :P='999999' :R, lnAmount=PRICE*TOTQTY :10:H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='9999999999.99',TOTPIK :H='Tot.Pik.' :R ,]+;
                [Qty1 :P='99999':R,Qty2 :P='99999':R,Qty3 :P='99999':R,Qty4 :P='99999':R,]+;
                [Qty5 :P='99999':R,Qty6 :P='99999':R,Qty7 :P='99999':R,Qty8 :P='99999':R,]+;
                [PIK1 :P='99999':R,PIK2 :P='99999':R,PIK3 :P='99999':R,PIK4 :P='99999':R,]+;
                [PIK5 :P='99999':R,PIK6 :P='99999':R,PIK7 :P='99999':R,PIK8 :P='99999':R,TOTPIK :H='Tot.Pik.']+;
                IIF(laSetups[2,2]='Y',",n=laData[27] :H='Rep 1':R ,COMM1 :H='Comm 1' :R,n1=laData[29] :H='Rep 2':R ,COMM2 :H='Comm 2' :R",'')+;
                IIF(laData[7],",CUSTPO :H='Cust. PO#':R",'')
    *E301401,1 ABD [End]
    *--HDM B802148,1 04/20/1999 [End]

    DO CASE
      *-- Open quantity
      CASE lnOrdTrans= 1
        SELECT (lcOrderFile)
        SET RELATION TO Style INTO Style
        SET RELATION TO Style+laData[31]+SPACE(10) INTO StyDye ADDITIVE
      *-- Booked quantity
      CASE lnOrdTrans= 2
        IF !USED(lcBooked)
          *B802405,1 AMM Open File with gfOpenFile()
          *USE (gcDataDir+'ORDLINE') AGAIN ALIAS (lcBooked) IN 0
          =gfOpenFile(gcDataDir+'ORDLINE','','SH',@lcBooked,.T.)
          *B802405,1 AMM end
        ENDIF
        lcBrTtlT   = lcBrTtlB
        lcTranFile = lcBooked
        SET ORDER TO TAG 'ORDLINE' IN (lcBooked)
        SELECT (lcOrderFile)
        SET RELATION TO cOrdType+ORDER+STR(LINENO,6) INTO (lcBooked)
        lcTranBr = [STYLE :R,STORE :R,BOOK1 :R,BOOK2 :R,BOOK3 :R,] +;
                   [BOOK4 :R,BOOK5 :R,BOOK6 :R,BOOK7 :R,BOOK8 :R,TOTBOOK :6:R ,] +;
                   [lnAmount=PRICE*TOTBOOK :9:H='Amount':R:P='999999.99']+;
                   IIF(laSetups[2,2]='Y', ;
                   ",n=laData[27] :H='Rep 1':5:R ,COMM1 :H='Comm 1' :6:R ,"+;
                   "n1=laData[29]:H='Rep 2':5:R ,COMM2 :H='Comm 2' :6:R",'')+;
                   IIF(laData[7],[,CUSTPO :15:H='Cust P/O':R],'')
      *-- Shipped quantity
      CASE lnOrdTrans= 3
        lcBrTtlT   = lcBrTtlS
        SET ORDER TO TAG 'CINVLINE' IN 'CONSINVL'
        IF SEEK(laData[1],'CONSINVL')
          lcTranFile = 'CONSINVL'
        ELSE
          lcTranFile = 'INVLINE'
          SET ORDER TO TAG 'INVLINEO' IN 'INVLINE'
        ENDIF
        SELECT (lcOrderFile)
        SET RELATION TO ORDER+STR(LINENO,6) INTO (lcTranFile)
        lcTranBr = [INVOICE:H='Invoice#' :R ,INVDATE:H='Date' :R,STORE :R ,] +;
                   [TOTQTY :H='Qty.' :P='9999999':R ,PRICE :R ,] +;
                   [lnAmount=PRICE*TOTQTY :10:H='Amount':R:P='9999999.99',]+; 
                   [Qty1 :6 :R,Qty2 :6 :R,Qty3 :6 :R,Qty4 :6 :R,] +;
                   [Qty5 :6 :R,Qty6 :6 :R,Qty7 :6 :R,Qty8 :6 :R]
      *-- Production quantity
      CASE lnOrdTrans= 4
        SELECT (lcOrderFile)
        SET RELATION TO Style INTO STYLE
        SET RELATION TO IIF(Style.Make,"1","2")+Order+STR(LineNo,6) INTO CutPick ADDITIVE
        =SEEK(lcOrdType+laData[1])
        lcBrTtlT   = lcBrTtlP
        lcTranFile = 'CutPick'
        lcTranBr1= [CutPick.cTktNo :H='Tran #' :R ,] +;
                   [Type1='C/T' :H='Type' :R ,] +;
                   [CutTktH.ENTERED  :R,CutTktH.Complete :R,] +;
                   [CutPick.Qty1 :H='Size1':6:R ,] +;
                   [CutPick.Qty2 :H='Size2':6:R ,] +;
                   [CutPick.Qty3 :H='Size3':6:R ,] +;
                   [CutPick.Qty4 :H='Size4':6:R ,] +;
                   [CutPick.Qty5 :H='Size5':6:R ,] +;
                   [CutPick.Qty6 :H='Size6':6:R ,] +;
                   [CutPick.Qty7 :H='Size7':6:R ,] +;
                   [CutPick.Qty8 :H='Size8':6:R ,]
        lcTranBr1=lcTranBr1+;
                   [CutPick.TotQty :H='Qty. Alo.' :9:R ,] +;
                   [CutTktL.Qty1 :H='Size1':6:R ,] +;
                   [CutTktL.Qty2 :H='Size2':6:R ,] +;
                   [CutTktL.Qty3 :H='Size3':6:R ,] +;
                   [CutTktL.Qty4 :H='Size4':6:R ,] +;
                   [CutTktL.Qty5 :H='Size5':6:R ,] +;
                   [CutTktL.Qty6 :H='Size6':6:R ,] +;
                   [CutTktL.Qty7 :H='Size7':6:R ,] +;
                   [CutTktL.Qty8 :H='Size8':6:R ,] +;
                   [CutTktL.TotQty   :H='Qty. Ord.' :9:R ,] +;
                   [lnTotQty=CutTktL.TotQty-CutTktL.TotOrd :9:H='Qty. Opn.':R] 
        lcTranBr2= [CutPick.cTktNo :H='Tran #' :R ,] +;
                   [Type2='P/O' :H='Type' :R ,] +;
                   [PosHdr.ENTERED :R,PosHdr.Complete :R,] +;
                   [PosHdr.Vendor :H='Vendor ' :R ,] +;
                   [CutPick.Qty1 :H='Size1':6:R ,] +;
                   [CutPick.Qty2 :H='Size2':6:R ,] +;
                   [CutPick.Qty3 :H='Size3':6:R ,] +;
                   [CutPick.Qty4 :H='Size4':6:R ,] +;
                   [CutPick.Qty5 :H='Size5':6:R ,] +;
                   [CutPick.Qty6 :H='Size6':6:R ,] +;
                   [CutPick.Qty7 :H='Size7':6:R ,] +;
                   [CutPick.Qty8 :H='Size8':6:R ,]
        lcTranBr2=lcTranBr2+;
                   [CutPick.TotQty  :H='Qty. Alo.' :9:R,] +;
                   [PosLn.Qty1 :H='Size1':6:R ,] +;
                   [PosLn.Qty2 :H='Size2':6:R ,] +;
                   [PosLn.Qty3 :H='Size3':6:R ,] +;
                   [PosLn.Qty4 :H='Size4':6:R ,] +;
                   [PosLn.Qty5 :H='Size5':6:R ,] +;
                   [PosLn.Qty6 :H='Size6':6:R ,] +;
                   [PosLn.Qty7 :H='Size7':6:R ,] +;
                   [PosLn.Qty8 :H='Size8':6:R ,] +;
                   [PosLn.TotQty :H='Qty. Ord.' :9:R ,] +;
                   [lnTotQty=PosLn.TotQty-PosLn.TotOrd :H='Qty. Opn.' :9:R]
        lcTranBr=IIF(Style.Make,lcTranBr1,lcTranBr2)
        llDomestic = Style.Make
      
      *-- Depleted orders
      CASE lnOrdTrans= 5
        IF !USED(lcDepleted) .OR. !SEEK(laData[1],lcDepleted)
          SET ORDER TO TAG ORDACCT IN ORDHDR
          SET ORDER TO TAG ORDLINE IN ORDLINE
          *B802544,1 Fix Inquire depleted orders
          *SELECT ORDHDR.ORDER,ORDLINE.STYLE,ORDLINE.PRICE,;
          ORDLINE.STORE,ORDLINE.BOOK1,ORDLINE.BOOK2,ORDLINE.BOOK3,ORDLINE.BOOK4,;
          ORDLINE.BOOK5,ORDLINE.BOOK6,ORDLINE.BOOK7,ORDLINE.BOOK8,ORDLINE.TOTBOOK,;
          ORDLINE.CFROMORDER,ORDLINE.BULKLINENO,ORDLINE.COMM1,ORDLINE.COMM2,;
          ORDLINE.CUSTPO,ORDLINE.GROUP,ORDLINE.PIKTKT,ORDLINE.PIKDATE;
          FROM ORDHDR,ORDLINE;
          WHERE ORDHDR.ACCOUNT+ORDHDR.cOrdType+ORDHDR.ORDER=lcOrdType+laData[2] AND ;
          ORDHDR.cFromOrder=laData[1] AND ;
          ORDLINE.cOrdType+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)=;
          lcOrdType+ORDHDR.ORDER INTO CURSOR (lcDepleted)

          SELECT ORDHDR.ORDER,ORDLINE.STYLE,ORDLINE.PRICE,;
          ORDLINE.STORE,ORDLINE.BOOK1,ORDLINE.BOOK2,ORDLINE.BOOK3,ORDLINE.BOOK4,;
          ORDLINE.BOOK5,ORDLINE.BOOK6,ORDLINE.BOOK7,ORDLINE.BOOK8,ORDLINE.TOTBOOK,;
          ORDLINE.CFROMORDER,ORDLINE.BULKLINENO,ORDLINE.COMM1,ORDLINE.COMM2,;
          ORDLINE.CUSTPO,ORDLINE.GROUP,ORDLINE.PIKTKT,ORDLINE.PIKDATE;
          FROM ORDHDR,ORDLINE;
          WHERE ORDHDR.ACCOUNT+ORDHDR.cOrdType+ORDHDR.ORDER=lcOrdType+laData[2] AND ;
          ORDLINE.cFromOrder=laData[1] AND ;
          ORDLINE.cOrdType+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)=;
          lcOrdType+ORDHDR.ORDER INTO CURSOR (lcDepleted)
          *B802544,1 (End)
          INDEX ON CFROMORDER+STR(BULKLINENO,6) TAG (lcDepleted)
          SET ORDER TO TAG ORDHDR IN ORDHDR
        ENDIF
        lcBrTtlT   = lcBrTtlD
        lcTranFile = lcDepleted
        SELECT (lcOrderFile)
        SET RELATION TO ORDER+STR(LINENO,6) INTO (lcDepleted)
        lcTranBr = [ORDER :R ,] +;
                   [STYLE ,STORE :R,BOOK1 :R,BOOK2 :R,BOOK3 :R,] +;
                   [BOOK4 :R,BOOK5 :R,BOOK6 :R,BOOK7 :R,BOOK8 :R,TOTBOOK :R ,] +;
                   [lnAmount=PRICE*TOTBOOK :10:H='Amount':R:P='9999999.99',] +;
                   IIF(laSetups[2,2]='Y', ;
                   "n=laData[27] :H='Rep 1' :R ,COMM1 :H='Comm 1' :R ," +;
                   "n1=laData[29] :H='Rep 2':R ,COMM2 :H='Comm 2' :R ",'') +;
                   IIF(laData[7],[,CUSTPO :H='Cust P/O':R],'')
      *-- Bulk order details
      CASE lnOrdTrans= 6
        lcBrTtlT   = lcBrTtlK
        lcTranFile = lcBulkOrd
        SELECT (lcOrderFile)
        SET RELATION TO STR(BulkLineNo,6) INTO (lcBulkOrd)
        lcTranBr = [STYLE :19:R,] +;
                   [Book1 :5 :R,Book2 :5 :R,Book3 :5 :R,Book4 :5 :R,] +;
                   [Book5 :5 :R,Book6 :5 :R,Book7 :5 :R,Book8 :5 :R ,] +;
                   [TotBook :H= 'Tot. Book' :10 :R ,] +;
                   [Open1 :5 :R,Open2 :5 :R,Open3 :5 :R,Open4 :5 :R,] +;
                   [Open5 :5 :R,Open6 :5 :R,Open7 :5 :R,Open8 :5 :R ,] +;
                   [TotOpen :H= 'Tot. Open' :10 :R ,] +;
                   [Used1 :5 :R,Used2 :5 :R,Used3 :5 :R,Used4 :5 :R,] +;
                   [Book5 :5 :R,Used6 :5 :R,Used7 :5 :R,Used8 :5 :R ,] +;
                   [TotUsed :H= 'Tot. Used' :10 :R ,] +;
                   [Bal1=MAX(Open1-Used1,0) :H='Bal1' :5:R,]+;
                   [Bal2=MAX(Open2-Used2,0) :H='Bal2' :5:R,]+;
                   [Bal3=MAX(Open3-Used3,0) :H='Bal3' :5:R,]+;
                   [Bal4=MAX(Open4-Used4,0) :H='Bal4' :5:R,]+;
                   [Bal5=MAX(Open5-Used5,0) :H='Bal5' :5:R,]+;
                   [Bal6=MAX(Open6-Used6,0) :H='Bal6' :5:R,]+;
                   [Bal7=MAX(Open7-Used7,0) :H='Bal7' :5:R,]+;
                   [Bal8=MAX(Open8-Used8,0) :H='Bal8' :5:R,]+;
                   [Balance = MAX(TotOpen-TotUsed,0) :H= 'Balance' :10:R]
      *E300956,1 Update Order lines canceled quantity
      CASE lnOrdTrans= 7
        lcBrTtlT   = lcBrTtlC
        lcTranFile = IIF(laScrMode[2],'ORDCANLN',lcOrdCanLn)
        SELECT (lcOrderFile)
        SET RELATION TO cOrdType+Order+STR(LineNo,6) INTO (lcTranFile)
        lcTranBr = [Cancelled :R :H='Date',TotQty :H= 'Tot. Can.' :R,]+;
                   [cDesc=gfCodDes(cCancReson,'CCANCRESON') :R :H='Cancelation Reason',]+;
                   [Qty1 :R :H='Can1',Qty2 :R :H='Can2',Qty3 :R :H='Can3',Qty4 :R :H='Can4',]+;
                   [Qty5 :R :H='Can5',Qty6 :R :H='Can6',Qty7 :R :H='Can7',Qty8 :R :H='Can8']
      *E301288,1 Reham On 07/26/1999   *** Begin ***
      *E301288,1 Add new bar to dsplay related records from CutPick file.
      CASE lnOrdTrans = 8
        lcBrTtlT   = lcBrTtlW
        lcTranFile = "CutPick"
        lcTranBr   = [lnDummy=IIF(TranCd='1','CT #',IIF(TranCd='2','PO #','Adornment')) :R:10:H='Tran. Type',]+;
                     [cTktNo:R:H='Tran. #',Order :R:H='Order #', Style:R:20, Qty1:R, Qty2:R,]+;
                     [Qty3:R, Qty4:R, Qty5:R, Qty6:R, Qty7:R, Qty8:R, TotQty:R]
      *E301288,1 Reham On 07/26/1999   *** End   ***
    ENDCASE
  *-- Sort by style/color - Total quantity - Details
  CASE lnSortBy = 2 .AND. !llQtyPSize .AND. llDetails
    *E301077,5 Inhance openning files to speed up transaction
    =IIF(!USED(lcOrdLine),lfCratTemp(),.T.)
    *E301077,5 (End)

    IF !SEEK(lcOrdType+laData[1],lcOrdLine)
      SELECT (lcOrdLine)
      DELETE ALL
      SELECT ORDLINE
      SET ORDER TO TAG ORDLINE
      =SEEK(lcOrdType+laData[1])
      SCAN REST WHILE cordtype+order+STR(lineno,6) = lcOrdType+laData[1]
        SCATTER MEMVAR MEMO
        INSERT INTO (lcOrdLine) FROM MEMVAR
      ENDSCAN
    ENDIF  
    lcOrderFile = (lcOrdLine)
    SET ORDER TO TAG ORDLINES IN (lcOrdLine)
    *--HDM B802148,1 04/20/1999 [Start] Add Picked Quantity Field To the Details Browse
    *lcOrderBr = [STYLE :H=lcStyHdr :R,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                [STORE :H='Store' :R,]+;
                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [PRICE :H='Price' :R,TOTQTY :H='Quantity' :R,] +;
                [lnAmount=PRICE*TOTQTY :10:H='Amount':R:P='9999999.99']
    *E301401,1 ABD For EDI Temporary Orders, change price columns to labelled
    *E301401,1 ABD "Transmitted Price" and Amount to labelled "Wholesale Price," 
    *E301401,1 ABD Both on-screen and in outputs. [Begin]
    *lcOrderBr = [STYLE :H=lcStyHdr :R,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                [STORE :H='Store' :R,]+;
                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [PRICE :H='Price' :R,TOTQTY :H='Quantity' :R,] +;
                [lnAmount=PRICE*TOTQTY :10:H='Amount':R:P='9999999.99' ,]+;
                [TOTPIK :H='Tot.Pik.' :R]

    lcOrderBr = [STYLE :H=lcStyHdr :R,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                [STORE :H='Store' :R,]+;
                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [PRICE :H=IIF(ORDHDR.cOrdType='T','Transmitted Price','Price'),TOTQTY :H='Quantity' :R,] +;
                [lnAmount=PRICE*TOTQTY :10:H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='9999999999.99',]+;
                [TOTPIK :H='Tot.Pik.' :R]
    *E301401,1 ABD [End]
    *--HDM B802148,1 04/20/1999 [End]
    DO CASE
      *-- Open quantity
      CASE lnOrdTrans= 1
        SELECT (lcOrderFile)
        SET RELATION TO Style INTO Style
        SET RELATION TO Style+laData[31]+SPACE(10) INTO StyDye ADDITIVE
      *-- Booked quantity
      CASE lnOrdTrans= 2
        IF !USED(lcBooked)
          *B802405,1 AMM Open file with gfOpenFile()
          *USE (gcDataDir+'ORDLINE') AGAIN ALIAS (lcBooked) IN 0
          =gfOpenFile(gcDataDir+'ORDLINE','','SH',@lcBooked,.T.)
          *B802405,1 AMM end
        ENDIF
        lcBrTtlT   = lcBrTtlB
        lcTranFile = (lcBooked)
        SET ORDER TO TAG 'ORDLINE' IN (lcBooked)
        SELECT (lcOrderFile)
        SET RELATION TO cOrdType+ORDER+STR(LINENO,6) INTO (lcBooked)
        lcTranBr = [STYLE :H=lcStyHdr :12:R ,] +;
                   [STORE :H='Store' :8:R ,PRICE :H='Price' :9:R ,] +;
                   [TotBook :H='Quantity' :10:R  ,] +;
                   [lnAmount=PRICE*TotBook :10:H='Amount':R:P='9999999.99']

      *-- Shipped quantity
      CASE lnOrdTrans= 3
        lcBrTtlT = lcBrTtlS
        SET ORDER TO TAG 'CINVLINE' IN 'CONSINVL'
        IF SEEK(laData[1],'CONSINVL')
          lcTranFile = 'CONSINVL'
        ELSE
          lcTranFile = 'INVLINE'
          SET ORDER TO TAG 'INVLINEO' IN 'INVLINE'
        ENDIF
        SELECT (lcOrderFile)
        SET RELATION TO ORDER+STR(LINENO,6) INTO (lcTranFile)
        lcTranBr = [STYLE :H=lcStyHdr :R,STORE :H='Store' :R,PRICE :H='Price' :R,] +;
                   [TOTQTY :H='Quantity' :R,] +;
                   [lnAmount=PRICE*TOTQTY :10:H='Amount':R:P='9999999.99',]+;
                   [INVOICE:H='Invoice' :R ,INVDATE:H='Date' :R]
      *-- Production quantity
      CASE lnOrdTrans= 4
        SELECT (lcOrderFile)
        SET RELATION TO Style INTO STYLE
        SET RELATION TO IIF(Style.Make,"1","2")+Order+STR(LineNo,6) INTO CutPick ADDITIVE
        =SEEK(lcOrdType+laData[1])
        lcBrTtlT = lcBrTtlP
        lcTranFile = 'CutPick'
        lcTranBr1 = [CutPick.cTktNo :H='Tran #' :R ,] +;
                    [Type1='C/T' :H='Type' :R ,] +;
                    [CutTktH.ENTERED :R ,CutTktH.Complete :R,] +;
                    [CutPick.TotQty :H='Qty. Alo.' :9:R ,] +;
                    [CutTktL.TotQty :H='Qty. Ord.' :9:R ,] +;
                    [lnTotQty=CutTktL.TotQty-CutTktL.TotOrd :9:H='Qty. Opn.':R] 
         lcTranBr2= [CutPick.cTktNo :H='Tran #' :R ,] +;
                    [Type2='P/O' :H='Type' :R ,] +;
                    [PosHdr.ENTERED :R,PosHdr.Complete :R,] +;
                    [PosHdr.Vendor :H='Vendor ' :R ,] +;
                    [CutPick.TotQty  :H='Qty. Alo.' :9:R,] +;
                    [PosLn.TotQty   :H='Qty. Ord.' :9:R ,] +;
                    [lnTotQty=PosLn.TotQty-PosLn.TotOrd   :H='Qty. Opn.' :9:R]
        lcTranBr=IIF(Style.Make,lcTranBr1,lcTranBr2)
        llDomestic = Style.Make
      
      *-- Depleted orders
      CASE lnOrdTrans= 5
        IF !USED(lcDepleted) .OR. !SEEK(laData[1],lcDepleted)
          *B802544,1 Fix Inquire depleted orders
          *SELECT ORDHDR.ORDER,ORDLINE.STYLE,ORDLINE.PRICE,;
          ORDLINE.STORE,ORDLINE.BOOK1,ORDLINE.BOOK2,ORDLINE.BOOK3,ORDLINE.BOOK4,;
          ORDLINE.BOOK5,ORDLINE.BOOK6,ORDLINE.BOOK7,ORDLINE.BOOK8,ORDLINE.TOTBOOK,;
          ORDLINE.CFROMORDER,ORDLINE.BULKLINENO,ORDLINE.COMM1,ORDLINE.COMM2,;
          ORDLINE.CUSTPO,ORDLINE.GROUP,ORDLINE.PIKTKT,ORDLINE.PIKDATE;
          FROM ORDHDR,ORDLINE;
          WHERE ORDHDR.ACCOUNT+ORDHDR.cOrdType+ORDHDR.ORDER=laData[2]+lcOrdType AND ;
          ORDHDR.cFromOrder=laData[1] AND ;
          ORDLINE.cOrdType+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)=;
          lcOrdType+ORDHDR.ORDER INTO CURSOR (lcDepleted)

          SELECT ORDHDR.ORDER,ORDLINE.STYLE,ORDLINE.PRICE,;
          ORDLINE.STORE,ORDLINE.BOOK1,ORDLINE.BOOK2,ORDLINE.BOOK3,ORDLINE.BOOK4,;
          ORDLINE.BOOK5,ORDLINE.BOOK6,ORDLINE.BOOK7,ORDLINE.BOOK8,ORDLINE.TOTBOOK,;
          ORDLINE.CFROMORDER,ORDLINE.BULKLINENO,ORDLINE.COMM1,ORDLINE.COMM2,;
          ORDLINE.CUSTPO,ORDLINE.GROUP,ORDLINE.PIKTKT,ORDLINE.PIKDATE;
          FROM ORDHDR,ORDLINE;
          WHERE ORDHDR.ACCOUNT+ORDHDR.cOrdType+ORDHDR.ORDER=laData[2]+lcOrdType AND ;
          ORDLINE.cFromOrder=laData[1] AND ;
          ORDLINE.cOrdType+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)=;
          lcOrdType+ORDHDR.ORDER INTO CURSOR (lcDepleted)
          *B802544,1 (End)
          INDEX ON CFROMORDER+STR(BULKLINENO,6) TAG (lcDepleted)
        ENDIF
        lcBrTtlT   = lcBrTtlD
        lcTranFile = lcDepleted
        SELECT (lcOrderFile)
        SET RELATION TO ORDER+STR(LINENO,6) INTO (lcDepleted)
        lcTranBr = [ORDER :R ,STYLE :R ,STORE :R ,] +;
                   [GROUP :H='Grp' :R ,PRICE :R ,TOTBOOK :H= 'Qty.' :R ,] +;
                   [lnAmount=PRICE*TOTBOOK :10:H='Amount':R:P='9999999.99',] +;
                   [PIKTKT :H='Piktkt' :6:R ,PIKDATE :H='Date' :8:R]
      *E300956,1 Order lines canceled quantity
      CASE lnOrdTrans= 7
        lcBrTtlT   = lcBrTtlC
        lcTranFile = IIF(laScrMode[2],'ORDCANLN',lcOrdCanLn)
        SELECT (lcOrderFile)
        SET RELATION TO cOrdType+Order+STR(LineNo,6) INTO (lcTranFile)
        lcTranBr = [Cancelled :R :H='Date',TotQty :H= 'Total Canceld' :R,]+;
                   [cDesc=gfCodDes(cCancReson,'CCANCRESON') :R :H='Cancelation Reason']
      *E301288,1 Reham On 07/26/1999   *** Begin ***
      *E301288,1 Add new bar to dsplay related records from CutPick file.
      CASE lnOrdTrans = 8
        lcBrTtlT   = lcBrTtlW
        lcTranFile = "CutPick"
        lcTranBr   = [lnDummy=IIF(TranCd='1','CT #',IIF(TranCd='2','PO #','Adornment')) :R:10:H='Tran. Type',]+;
                     [cTktNo:R:H='Tran. #',Order :R:H='Order #', Style:R:20, Qty1:R, Qty2:R,]+;
                     [Qty3:R, Qty4:R, Qty5:R, Qty6:R, Qty7:R, Qty8:R, TotQty:R]
      *E301288,1 Reham On 07/26/1999   *** End   ***
    ENDCASE
  *-- Sort by style - Total quantity - Summary
  CASE lnSortBy = 2 .AND. !llQtyPSize .AND. !llDetails
    IF !USED(lcOrdSS) .OR. !SEEK(lcOrdType+laData[1],lcOrdSS)
      lcFile = IIF(laScrMode[2],'ORDLINE',lcOrdLine)
      SET ORDER TO TAG ORDLINE IN (lcFile)
      *--HDM B802148,1 04/20/1999 [Start] Add Picked Quantity Field To the Details Browse
      *--HDM B802148,1 04/20/1999 [Start] Modify SQL to include totpic too
      *SELECT cOrdType+&lcFile..Order,Style,SUM(Qty1) AS nQty1,SUM(Qty2) AS nQty2,;
      SUM(Qty3) AS nQty3,SUM(Qty4) AS nQty4,SUM(Qty5) AS nQty5,SUM(Qty6) AS nQty6,;
      SUM(Qty7) AS nQty7,SUM(Qty8) AS nQty8,SUM(TOTQTY) AS nTotQTy,SUM(TOTQTY*PRICE) AS nAmount ;
      FROM (lcFile) WHERE cOrdType+&lcFile..ORDER+STR(LINENO,6)=;
      lcOrdType+laData[1] GROUP BY &lcFile..Order,Style INTO CURSOR (lcOrdSS)
      *B802613,1 (Begin) Select cOrdType+&lcFile..Order separately not as exp. 
      *SELECT cOrdType+&lcFile..Order,Style,SUM(Qty1) AS nQty1,SUM(Qty2) AS nQty2,;
      SUM(Qty3) AS nQty3,SUM(Qty4) AS nQty4,SUM(Qty5) AS nQty5,SUM(Qty6) AS nQty6,;
      SUM(Qty7) AS nQty7,SUM(Qty8) AS nQty8,SUM(TOTQTY) AS nTotQTy,SUM(TOTQTY*PRICE) AS nAmount, ;
      SUM(PIK1) AS nPIK1,SUM(PIK2) AS nPIK2,SUM(PIK3) AS nPIK3,SUM(PIK4) AS nPIK4,;
      SUM(PIK5) AS nPIK5,SUM(PIK6) AS nPIK6,SUM(PIK7) AS nPIK7,SUM(PIK8) AS nPIK8,;
      SUM(TOTPIK) AS nTotPIK;
      FROM (lcFile) WHERE cOrdType+&lcFile..ORDER+STR(LINENO,6)=;
      lcOrdType+laData[1] GROUP BY &lcFile..Order,Style INTO CURSOR (lcOrdSS)

      SELECT cOrdType,&lcFile..Order,Style,SUM(Qty1) AS nQty1,SUM(Qty2) AS nQty2,;
      SUM(Qty3) AS nQty3,SUM(Qty4) AS nQty4,SUM(Qty5) AS nQty5,SUM(Qty6) AS nQty6,;
      SUM(Qty7) AS nQty7,SUM(Qty8) AS nQty8,SUM(TOTQTY) AS nTotQTy,SUM(TOTQTY*PRICE) AS nAmount, ;
      SUM(PIK1) AS nPIK1,SUM(PIK2) AS nPIK2,SUM(PIK3) AS nPIK3,SUM(PIK4) AS nPIK4,;
      SUM(PIK5) AS nPIK5,SUM(PIK6) AS nPIK6,SUM(PIK7) AS nPIK7,SUM(PIK8) AS nPIK8,;
      SUM(TOTPIK) AS nTotPIK;
      FROM (lcFile) WHERE cOrdType+&lcFile..ORDER+STR(LINENO,6)=;
      lcOrdType+laData[1] GROUP BY &lcFile..Order,Style INTO CURSOR (lcOrdSS)
      *B802613,1 (End)
      INDEX ON cOrdType+ORDER+STYLE TAG (lcOrdSS)
      *--HDM B802148,1 04/20/1999 [End]
    ENDIF
    SELECT (lcOrdSS)
    SET RELATION TO STYLE INTO STYLE
    lcOrderFile = lcOrdSS
    *--HDM B802148,1 04/20/1999 [Start] Add Picked Quantity Field To the Details Browse
    *lcOrderBr = [STYLE  :H=lcStyHdr :R ,] +;
                [Style.Desc :H= 'Description', nTOTQTY :H='Quantity' :R:P='9999999999',]+;
                [nAMOUNT :H='Amount' :13:R:P='9999999999.99']

    *E301401,1 ABD For EDI Temporary Orders, change price columns to labelled
    *E301401,1 ABD "Transmitted Price" and Amount to labelled "Wholesale Price," 
    *E301401,1 ABD Both on-screen and in outputs. [Begin]
    *lcOrderBr = [STYLE  :H=lcStyHdr :R ,] +;
                [Style.Desc :H= 'Description', nTOTQTY :H='Quantity' :R:P='9999999999',]+;
                [nAMOUNT :H='Amount' :13:R:P='9999999999.99',nTOTPIK :H='Tot.Pik.']

    lcOrderBr = [STYLE  :H=lcStyHdr :R ,] +;
                [Style.Desc :H= 'Description', nTOTQTY :H='Quantity' :R:P='9999999999',]+;
                [nAMOUNT :H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):13:R:P='9999999999.99',nTOTPIK :H='Tot.Pik.']
    *E301401,1 ABD [End]
    *--HDM B802148,1 04/20/1999 [End]
    DO CASE
      *-- Booked quantity
      CASE lnOrdTrans= 2
        IF !USED(lcBookSS) .OR. !SEEK(laData[1],lcBookSS)
          SET ORDER TO TAG ORDLINE IN IIF(laScrMode[2],'ORDLINE',lcOrdLine)
          SELECT Style,SUM(Book1) AS nQty1,SUM(Book2) AS nQty2,;
          SUM(Book3) AS nQty3,SUM(Book4) AS nQty4,SUM(Book5) AS nQty5,SUM(Book6) AS nQty6,;
          SUM(Book7) AS nQty7,SUM(Book8) AS nQty8,SUM(TOTBook) AS nTotQTy,SUM(TOTBook*PRICE) AS nAmount ;
          FROM IIF(laScrMode[2],'ORDLINE',lcOrdLine) WHERE ;
          cOrdType+ORDER+STR(LINENO,6)=lcOrdType+laData[1] ;
          GROUP BY Order,Style INTO CURSOR (lcBookSS)
          INDEX ON ORDER+STYLE TAG (lcBookSS)
        ENDIF
        SELECT (lcOrderFile)
        SET RELATION TO ORDER+STYLE INTO (lcBookSS) ADDITIVE
        lcBrTtlT = lcBrTtlB
        lcTranFile = lcBookSS
        lcTranBr   = [STYLE :H=lcStyHdr :R,nTOTQTY :H='Quantity' :R:P='9999999999',]+;
                     [nAMOUNT :H='Amount' :13:R:P='9999999999.99']
      
      *-- Shipped quantity
      CASE lnOrdTrans= 3
        IF !USED(lcShipSS) .OR. !SEEK(laData[1],lcShipSS)
          SET ORDER TO TAG 'CINVLINE' IN 'CONSINVL'
          IF SEEK(laData[1],'CONSINVL')
            lcFile = 'CONSINVL'
          ELSE
            lcFile = 'INVLINE'
            SET ORDER TO TAG 'INVLINEO' IN 'INVLINE'
          ENDIF
          SELECT &lcFile..Order,Style,SUM(Qty1) AS nQty1,SUM(Qty2) AS nQty2,;
          SUM(Qty3) AS nQty3,SUM(Qty4) AS nQty4,SUM(Qty5) AS nQty5,SUM(Qty6) AS nQty6,;
          SUM(Qty7) AS nQty7,SUM(Qty8) AS nQty8,SUM(TOTQTY) AS nTotQTy,SUM(TOTQTY*PRICE) AS nAmount ;
          FROM (lcFile) WHERE ORDER+STR(LINENO,6)=laData[1] ;
          GROUP BY Order,Style INTO CURSOR (lcShipSS)
          INDEX ON ORDER+STYLE TAG (lcShipSS)
        ENDIF
        SELECT (lcOrderFile)
        SET RELATION TO ORDER+STYLE INTO (lcShipSS)  ADDITIVE
        lcBrTtlT = lcBrTtlS
        lcTranFile = lcShipSS
        lcTranBr   = [STYLE :H=lcStyHdr :R,nTOTQTY :H='Quantity' :R:P='9999999999',]+;
                     [nAMOUNT :H='Amount' :R:P='9999999999.99']
    ENDCASE
  *-- Sort by style - Quantity per size- Details
  CASE lnSortBy = 2 .AND. llQtyPSize .AND. llDetails
    *E301077,5 Inhance openning files to speed up transaction
    =IIF(!USED(lcOrdLine),lfCratTemp(),.T.)
    *E301077,5 (End)
    IF !SEEK(lcOrdType+laData[1],lcOrdLine)
      SELECT (lcOrdLine)
      DELETE ALL
      SELECT ORDLINE
      SET ORDER TO TAG ORDLINE
      =SEEK(lcOrdType+laData[1])
      SCAN REST WHILE cordtype+order+STR(lineno,6) = lcOrdType+laData[1]
        SCATTER MEMVAR MEMO
        INSERT INTO (lcOrdLine) FROM MEMVAR
      ENDSCAN
    ENDIF  
    lcOrderFile = lcOrdLine
    SET ORDER TO TAG ORDLINES IN (lcOrdLine)
    *--HDM B802148,1 04/20/1999 [Start] Add Picked Quantity Field To the Details Browse
    *lcOrderBr = [STYLE :H=lcStyHdr :R ,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                [STORE :H='Store' :R ,] +;
                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [PRICE :H='Price' :R,TOTQTY :H='Quantity' :R,] +;
                [lnAmount=PRICE*TOTQTY :H='Amount':R:P='9999999.99',]+;
                [QTY1 :H='Size1' :R:P='99999',] +;
                [QTY2 :H='Size2' :R:P='99999',] +;
                [QTY3 :H='Size3' :R:P='99999',] +;
                [QTY4 :H='Size4' :R:P='99999',] +;
                [QTY5 :H='Size5' :R:P='99999',] +;
                [QTY6 :H='Size6' :R:P='99999',] +;
                [QTY7 :H='Size7' :R:P='99999',] +;
                [QTY8 :H='Size8' :R:P='99999']
    *E301401,1 ABD For EDI Temporary Orders, change price columns to labelled
    *E301401,1 ABD "Transmitted Price" and Amount to labelled "Wholesale Price," 
    *E301401,1 ABD Both on-screen and in outputs. [Begin]
    *lcOrderBr = [STYLE :H=lcStyHdr :R ,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                [STORE :H='Store' :R ,] +;
                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [PRICE :H='Price' :R,TOTQTY :H='Quantity' :R,] +;
                [lnAmount=PRICE*TOTQTY :H='Amount':R:P='9999999.99',]+;
                [QTY1 :H='Size1' :R:P='99999',] +;
                [QTY2 :H='Size2' :R:P='99999',] +;
                [QTY3 :H='Size3' :R:P='99999',] +;
                [QTY4 :H='Size4' :R:P='99999',] +;
                [QTY5 :H='Size5' :R:P='99999',] +;
                [QTY6 :H='Size6' :R:P='99999',] +;
                [QTY7 :H='Size7' :R:P='99999',] +;
                [QTY8 :H='Size8' :R:P='99999',] +;
                [PIK1 :P='99999':R,PIK2 :P='99999':R,PIK3 :P='99999':R,PIK4 :P='99999':R,]+;
                [PIK5 :P='99999':R,PIK6 :P='99999':R,PIK7 :P='99999':R,PIK8 :P='99999':R,TOTPIK :H='Tot.Pik.']

    lcOrderBr = [STYLE :H=lcStyHdr :R ,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                [STORE :H='Store' :R ,] +;
                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [PRICE :H=IIF(ORDHDR.cOrdType='T','Transmitted Price','Price'):R,TOTQTY :H='Quantity' :R,] +;
                [lnAmount=PRICE*TOTQTY :H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='9999999999.99',]+;
                [QTY1 :H='Size1' :R:P='99999',] +;
                [QTY2 :H='Size2' :R:P='99999',] +;
                [QTY3 :H='Size3' :R:P='99999',] +;
                [QTY4 :H='Size4' :R:P='99999',] +;
                [QTY5 :H='Size5' :R:P='99999',] +;
                [QTY6 :H='Size6' :R:P='99999',] +;
                [QTY7 :H='Size7' :R:P='99999',] +;
                [QTY8 :H='Size8' :R:P='99999',] +;
                [PIK1 :P='99999':R,PIK2 :P='99999':R,PIK3 :P='99999':R,PIK4 :P='99999':R,]+;
                [PIK5 :P='99999':R,PIK6 :P='99999':R,PIK7 :P='99999':R,PIK8 :P='99999':R,TOTPIK :H='Tot.Pik.']
    *E301401,1 ABD [End]
   *--HDM B802148,1 04/20/1999 [End]
    DO CASE
      *-- Open quantity
      CASE lnOrdTrans= 1
        SELECT (lcOrderFile)
        SET RELATION TO Style INTO Style
        SET RELATION TO Style+laData[31]+SPACE(10) INTO StyDye ADDITIVE
      *-- Booked quantity
      CASE lnOrdTrans= 2
        IF !USED(lcBooked)
          *B802405,1 AMM Open file with gfOpenFile()
          *USE (gcDataDir+'ORDLINE') AGAIN ALIAS (lcBooked) IN 0
          =gfOpenFile(gcDataDir+'ORDLINE','','SH',@lcBooked,.T.)
          *B802405,1 AMM end
        ENDIF
        lcBrTtlT   = lcBrTtlB
        lcTranFile = (lcBooked)
        SET ORDER TO TAG 'ORDLINE' IN (lcBooked)
        SELECT ORDLINE
        SET RELATION TO cOrdType+ORDER+STR(LINENO,6) INTO (lcBooked)
        lcTranBr = [STYLE :H=lcStyHdr :19:R ,] +;
                   [STORE :H='Store' :8:R ,] +;
                   [Book1 :H='Size1' :5:R:P='99999' ,] +;
                   [Book2 :H='Size2' :5:R:P='99999' ,] +;
                   [Book3 :H='Size3' :5:R:P='99999' ,] +;
                   [Book4 :H='Size4' :5:R:P='99999' ,] +;
                   [Book5 :H='Size5' :5:R:P='99999' ,] +;
                   [Book6 :H='Size6' :5:R:P='99999' ,] +;
                   [Book7 :H='Size7' :5:R:P='99999' ,] +;
                   [Book8 :H='Size8' :5:R:P='99999' ,] +;
                   [PRICE :H='Price' :9:R ,] +;
                   [TOTBook :H='Quantity' :10:R  ,] +;
                   [lnAmount=PRICE*TOTBook :10:H='Amount':R:P='9999999.99']
      
      *-- Shipped quantity
      CASE lnOrdTrans= 3
        lcBrTtlT = lcBrTtlS
        SET ORDER TO TAG 'CINVLINE' IN 'CONSINVL'
        IF SEEK(laData[1],'CONSINVL')
          lcTranFile = 'CONSINVL'
        ELSE
          lcTranFile = 'INVLINE'
          SET ORDER TO TAG 'INVLINEO' IN 'INVLINE'
        ENDIF
        SELECT (lcOrderFile)
        SET RELATION TO ORDER+STR(LINENO,6) INTO (lcTranFile)
        lcTranBr = [STYLE :H=lcStyHdr :R ,STORE :H='Store' :R ,] +;
                   [Qty1 :5:R ,Qty2 :5:R ,Qty3 :5:R ,Qty4 :5:R,] +;
                   [Qty5 :5:R ,Qty6 :5:R ,Qty7 :5:R ,Qty8 :5:R,] +;
                   [PRICE :H='Price' :8:R ,TOTQTY :H='Qty.' :P='999999':R,] +;
                   [lnAmount=PRICE*TOTQTY :10:H='Amount':R:P='9999999.99',]+;
                   [INVOICE:H='Invoice' :6:R ,INVDATE:H='Date' :8:R ]

      *-- Production quantity
      CASE lnOrdTrans= 4
        SELECT (lcOrderFile)
        SET RELATION TO Style INTO STYLE
        SET RELATION TO IIF(Style.Make,"1","2")+Order+STR(LineNo,6) INTO CutPick ADDITIVE
        =SEEK(lcOrdType+laData[1])
        lcBrTtlT = lcBrTtlP
        lcTranFile = 'CutPick'
        lcTranBr1= [CutPick.cTktNo :H='Tran #' :R ,] +;
                   [Type1='C/T' :H='Type' :R ,] +;
                   [CutTktH.ENTERED  :R ,CutTktH.Complete :R,] +;
                   [CutPick.Qty1 :H='Size1':5:R ,] +;
                   [CutPick.Qty2 :H='Size2':5:R ,] +;
                   [CutPick.Qty3 :H='Size3':5:R ,] +;
                   [CutPick.Qty4 :H='Size4':5:R ,] +;
                   [CutPick.Qty5 :H='Size5':5:R ,] +;
                   [CutPick.Qty6 :H='Size6':5:R ,] +;
                   [CutPick.Qty7 :H='Size7':5:R ,] +;
                   [CutPick.Qty8 :H='Size8':5:R ,]
        lcTranBr1=lcTranBr1+;
                   [CutPick.TotQty :H='Qty. Alo.' :9:R ,] +;
                   [CutTktL.Qty1 :H='Size1':6:R ,] +;
                   [CutTktL.Qty2 :H='Size2':6:R ,] +;
                   [CutTktL.Qty3 :H='Size3':6:R ,] +;
                   [CutTktL.Qty4 :H='Size4':6:R ,] +;
                   [CutTktL.Qty5 :H='Size5':6:R ,] +;
                   [CutTktL.Qty6 :H='Size6':6:R ,] +;
                   [CutTktL.Qty7 :H='Size7':6:R ,] +;
                   [CutTktL.Qty8 :H='Size8':6:R ,] +;
                   [CutTktL.TotQty :H='Qty. Ord.' :9:R ,] +;
                   [lnTotQty=CutTktL.TotQty-CutTktL.TotOrd :9:H='Qty. Opn.':R] 
        lcTranBr2= [CutPick.cTktNo :H='Tran #' :R ,] +;
                   [Type2='P/O' :H='Type' :R ,] +;
                   [PosHdr.ENTERED  :R ,PosHdr.Complete :R,] +;
                   [PosHdr.Vendor :H='Vendor ' :R ,] +;
                   [CutPick.Qty1 :H='Size1':5:R ,] +;
                   [CutPick.Qty2 :H='Size2':5:R ,] +;
                   [CutPick.Qty3 :H='Size3':5:R ,] +;
                   [CutPick.Qty4 :H='Size4':5:R ,] +;
                   [CutPick.Qty5 :H='Size5':5:R ,] +;
                   [CutPick.Qty6 :H='Size6':5:R ,] +;
                   [CutPick.Qty7 :H='Size7':5:R ,] +;
                   [CutPick.Qty8 :H='Size8':5:R ,]
        lcTranBr2=lcTranBr2+;
                   [CutPick.TotQty :H='Qty. Alo.' :9:R,] +;
                   [PosLn.Qty1 :H='Size1':6:R ,] +;
                   [PosLn.Qty2 :H='Size2':6:R ,] +;
                   [PosLn.Qty3 :H='Size3':6:R ,] +;
                   [PosLn.Qty4 :H='Size4':6:R ,] +;
                   [PosLn.Qty5 :H='Size5':6:R ,] +;
                   [PosLn.Qty6 :H='Size6':6:R ,] +;
                   [PosLn.Qty7 :H='Size7':6:R ,] +;
                   [PosLn.Qty8 :H='Size8':6:R ,] +;
                   [PosLn.TotQty :H='Qty. Ord.' :9:R ,] +;
                   [lnTotQty=PosLn.TotQty-PosLn.TotOrd :H='Qty. Opn.' :9:R]
        lcTranBr=IIF(Style.Make,lcTranBr1,lcTranBr2)
        llDomestic = Style.Make

      *-- Depleted orders
      CASE lnOrdTrans= 5
        IF !USED(lcDepleted) .OR. !SEEK(laData[1],lcDepleted)
          *B802544,1 Fix Inquire depleted orders
          *SELECT ORDHDR.ORDER,ORDLINE.STYLE,ORDLINE.PRICE,;
          ORDLINE.STORE,ORDLINE.BOOK1,ORDLINE.BOOK2,ORDLINE.BOOK3,ORDLINE.BOOK4,;
          ORDLINE.BOOK5,ORDLINE.BOOK6,ORDLINE.BOOK7,ORDLINE.BOOK8,ORDLINE.TOTBOOK,;
          ORDLINE.CFROMORDER,ORDLINE.BULKLINENO,ORDLINE.COMM1,ORDLINE.COMM2,;
          ORDLINE.CUSTPO,ORDLINE.GROUP,ORDLINE.PIKTKT,ORDLINE.PIKDATE;
          FROM ORDHDR,ORDLINE;
          WHERE ORDHDR.ACCOUNT+ORDHDR.cOrdType+ORDHDR.ORDER=lcOrdType+laData[2] AND ;
          ORDHDR.cFromOrder=laData[1] AND ;
          ORDLINE.cOrdType+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)=;
          lcOrdType+ORDHDR.ORDER INTO CURSOR (lcDepleted)

          SELECT ORDHDR.ORDER,ORDLINE.STYLE,ORDLINE.PRICE,;
          ORDLINE.STORE,ORDLINE.BOOK1,ORDLINE.BOOK2,ORDLINE.BOOK3,ORDLINE.BOOK4,;
          ORDLINE.BOOK5,ORDLINE.BOOK6,ORDLINE.BOOK7,ORDLINE.BOOK8,ORDLINE.TOTBOOK,;
          ORDLINE.CFROMORDER,ORDLINE.BULKLINENO,ORDLINE.COMM1,ORDLINE.COMM2,;
          ORDLINE.CUSTPO,ORDLINE.GROUP,ORDLINE.PIKTKT,ORDLINE.PIKDATE;
          FROM ORDHDR,ORDLINE;
          WHERE ORDHDR.ACCOUNT+ORDHDR.cOrdType+ORDHDR.ORDER=lcOrdType+laData[2] AND ;
          ORDLINE.cFromOrder=laData[1] AND ;
          ORDLINE.cOrdType+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)=;
          lcOrdType+ORDHDR.ORDER INTO CURSOR (lcDepleted)
          *B802544,1 (End)
          INDEX ON CFROMORDER+STR(BULKLINENO,6) TAG (lcDepleted)
        ENDIF
        lcBrTtlT   = lcBrTtlD
        lcTranFile = lcDepleted
        SELECT (lcOrderFile)
        SET RELATION TO ORDER+STR(LINENO,6) INTO (lcDepleted)
        lcTranBr = [ORDER :R ,] +;
                   [STYLE ,STORE :R,BOOK1 :R,BOOK2 :R,BOOK3 :R,] +;
                   [BOOK4 :R,BOOK5 :R,BOOK6 :R,BOOK7 :R,BOOK8 :R,TOTBOOK :R ,] +;
                   [lnAmount=PRICE*TOTBOOK :10:H='Amount':R:P='9999999.99',] +;
                   IIF(laSetups[2,2]='Y', ;
                   "n=laData[27] :H='Rep 1' :R ,COMM1 :H='Comm 1' :R ," +;
                   "n1=laData[29] :H='Rep 2':R ,COMM2 :H='Comm 2' :R ",'') +;
                   IIF(laData[7],[,CUSTPO :H='Cust P/O':R],'')
      *E300956,1 Update Order lines canceled quantity
      CASE lnOrdTrans= 7
        lcBrTtlT   = lcBrTtlC
        lcTranFile = IIF(laScrMode[2],'ORDCANLN',lcOrdCanLn)
        SELECT (lcOrderFile)
        SET RELATION TO cOrdType+Order+STR(LineNo,6) INTO (lcTranFile)
        lcTranBr = [Cancelled :R :H='Date',TotQty :H= 'Tot. Can.' :R,]+;
                   [cDesc=gfCodDes(cCancReson,'CCANCRESON') :R :H='Cancelation Reason',]+;
                   [Qty1 :R :H='Can1',Qty2 :R :H='Can2',Qty3 :R :H='Can3',Qty4 :R :H='Can4',]+;
                   [Qty5 :R :H='Can5',Qty6 :R :H='Can6',Qty7 :R :H='Can7',Qty8 :R :H='Can8']
      *E301288,1 Reham On 07/26/1999   *** Begin ***
      *E301288,1 Add new bar to dsplay related records from CutPick file.
      CASE lnOrdTrans = 8
        lcBrTtlT   = lcBrTtlW
        lcTranFile = "CutPick"
        lcTranBr   = [lnDummy=IIF(TranCd='1','CT #',IIF(TranCd='2','PO #','Adornment')) :R:10:H='Tran. Type',]+;
                     [cTktNo:R:H='Tran. #',Order :R:H='Order #', Style:R:20, Qty1:R, Qty2:R,]+;
                     [Qty3:R, Qty4:R, Qty5:R, Qty6:R, Qty7:R, Qty8:R, TotQty:R]
      *E301288,1 Reham On 07/26/1999   *** End   ***
    ENDCASE
  *-- Sort by style - Quantity per size- Summary
  CASE lnSortBy = 2 .AND. llQtyPSize .AND. !llDetails
    IF !USED(lcOrdSS) .OR. !SEEK(lcOrdType+laData[1],lcOrdSS)
      lcFile=IIF(laScrMode[2],'ORDLINE',lcOrdLine)
      SET ORDER TO TAG ORDLINE IN (lcFile)
      *--HDM B802148,1 04/20/1999 [Start] Add Picked Quantity Field To the Details Browse
      *--HDM B802148,1 04/20/1999 [Start] Modify the select statement to include PIK fields too
      *SELECT cOrdType,&lcFile..Order,Style,SUM(Qty1) AS nQty1,SUM(Qty2) AS nQty2,;
      SUM(Qty3) AS nQty3,SUM(Qty4) AS nQty4,SUM(Qty5) AS nQty5,SUM(Qty6) AS nQty6,;
      SUM(Qty7) AS nQty7,SUM(Qty8) AS nQty8,SUM(TOTQTY) AS nTotQTy,SUM(TOTQTY*PRICE) AS nAmount ;
      FROM (lcFile) ;
      WHERE cOrdType+&lcFile..ORDER+STR(LINENO,6)=lcOrdType+laData[1] ;
      GROUP BY &lcFile..Order,Style INTO CURSOR (lcOrdSS)
      
      SELECT cOrdType,&lcFile..Order,Style,SUM(Qty1) AS nQty1,SUM(Qty2) AS nQty2,;
      SUM(Qty3) AS nQty3,SUM(Qty4) AS nQty4,SUM(Qty5) AS nQty5,SUM(Qty6) AS nQty6,;
      SUM(Qty7) AS nQty7,SUM(Qty8) AS nQty8,SUM(TOTQTY) AS nTotQTy,SUM(TOTQTY*PRICE) AS nAmount ,;
      SUM(PIK1) AS nPIK1,SUM(PIK2) AS nPIK2,SUM(PIK3) AS nPIK3,SUM(PIK4) AS nPIK4,;
      SUM(PIK5) AS nPIK5,SUM(PIK6) AS nPIK6,SUM(PIK7) AS nPIK7,SUM(PIK8) AS nPIK8,;
      SUM(TOTPIK) AS nTotPIK;
      FROM (lcFile) ;
      WHERE cOrdType+&lcFile..ORDER+STR(LINENO,6)=lcOrdType+laData[1] ;
      GROUP BY &lcFile..Order,Style INTO CURSOR (lcOrdSS)
      *--HDM B802148,1 04/20/1999 end
      INDEX ON cOrdType+ORDER+STYLE TAG (lcOrdSS)
    ENDIF
    lcOrderFile = lcOrdSS
    *--HDM B802148,1 04/20/1999 [Start] Add Picked Quantity Field To the Details Browse
    *lcOrderBr = [STYLE :H=lcStyHdr :R ,] +;
                [nQTY1 :H='Qty1' :6:R:P='999999',]+;
                [nQTY2 :H='Qty2' :6:R:P='999999',]+;
                [nQTY3 :H='Qty3' :6:R:P='999999',]+;
                [nQTY4 :H='Qty4' :6:R:P='999999',]+;
                [nQTY5 :H='Qty5' :6:R:P='999999',]+;
                [nQTY6 :H='Qty6' :6:R:P='999999',]+;
                [nQTY7 :H='Qty7' :6:R:P='999999',]+;
                [nQTY8 :H='Qty8' :6:R:P='999999',]+;
                [nTOTQTY :H='Quantity' :10:R:P='9999999999',] +;
                [nAMOUNT :H='Amount' :13:R:P='9999999999.99'] 
    *E301401,1 ABD For EDI Temporary Orders, change price columns to labelled
    *E301401,1 ABD "Transmitted Price" and Amount to labelled "Wholesale Price," 
    *E301401,1 ABD Both on-screen and in outputs. [Begin]
    *lcOrderBr = [STYLE :H=lcStyHdr :R ,] +;
                [nQTY1 :H='Qty1' :6:R:P='999999',]+;
                [nQTY2 :H='Qty2' :6:R:P='999999',]+;
                [nQTY3 :H='Qty3' :6:R:P='999999',]+;
                [nQTY4 :H='Qty4' :6:R:P='999999',]+;
                [nQTY5 :H='Qty5' :6:R:P='999999',]+;
                [nQTY6 :H='Qty6' :6:R:P='999999',]+;
                [nQTY7 :H='Qty7' :6:R:P='999999',]+;
                [nQTY8 :H='Qty8' :6:R:P='999999',]+;
                [nTOTQTY :H='Quantity' :10:R:P='9999999999',] +;
                [nAMOUNT :H='Amount' :13:R:P='9999999999.99',] +;
                [nPIK1 :H='Pik1' :P='99999':R,]+;
                [nPIK2 :H='Pik2' :P='99999':R,]+;
                [nPIK3 :H='Pik3' :P='99999':R,]+;
                [nPIK4 :H='Pik4' :P='99999':R,]+;
                [nPIK5 :H='Pik5' :P='99999':R,]+;
                [nPIK6 :H='Pik6' :P='99999':R,]+;
                [nPIK7 :H='Pik7' :P='99999':R,]+;
                [nPIK8 :H='Pik8' :P='99999':R,]+;
                [nTotPik :H='Tot.Pik' :R]

    lcOrderBr = [STYLE :H=lcStyHdr :R ,] +;
                [nQTY1 :H='Qty1' :6:R:P='999999',]+;
                [nQTY2 :H='Qty2' :6:R:P='999999',]+;
                [nQTY3 :H='Qty3' :6:R:P='999999',]+;
                [nQTY4 :H='Qty4' :6:R:P='999999',]+;
                [nQTY5 :H='Qty5' :6:R:P='999999',]+;
                [nQTY6 :H='Qty6' :6:R:P='999999',]+;
                [nQTY7 :H='Qty7' :6:R:P='999999',]+;
                [nQTY8 :H='Qty8' :6:R:P='999999',]+;
                [nTOTQTY :H='Quantity' :10:R:P='9999999999',] +;
                [nAMOUNT :H=IIF(ORDHDR.cOrdType='T','Wholesale Price',;
                'Amount'):13:R:P='9999999999.99',] +;
                [nPIK1 :H='Pik1' :P='99999':R,]+;
                [nPIK2 :H='Pik2' :P='99999':R,]+;
                [nPIK3 :H='Pik3' :P='99999':R,]+;
                [nPIK4 :H='Pik4' :P='99999':R,]+;
                [nPIK5 :H='Pik5' :P='99999':R,]+;
                [nPIK6 :H='Pik6' :P='99999':R,]+;
                [nPIK7 :H='Pik7' :P='99999':R,]+;
                [nPIK8 :H='Pik8' :P='99999':R,]+;
                [nTotPik :H='Tot.Pik' :R]
    *E301401,1 ABD [End]
    *--HDM B802148,1 04/20/1999 [End]
    DO CASE
      *-- Booked quantity    
      CASE lnOrdTrans= 2
        IF !USED(lcBookSS) .OR. !SEEK(laData[1],lcBookSS)
          SET ORDER TO TAG ORDLINE IN ORDLINE
          SELECT OrdLine.ORDER,Style,SUM(Book1) AS nQty1,SUM(Book2) AS nQty2,;
          SUM(Book3) AS nQty3,SUM(Book4) AS nQty4,SUM(Book5) AS nQty5,SUM(Book6) AS nQty6,;
          SUM(Book7) AS nQty7,SUM(Book8) AS nQty8,SUM(TOTBook) AS nTotQTy,SUM(TOTBook*PRICE) AS nAmount ;
          FROM ORDLINE WHERE cOrdType+ORDER+STR(LINENO,6)=lcOrdType+laData[1] ;
          GROUP BY Order,Style INTO CURSOR (lcBookSS)
          INDEX ON ORDER+STYLE TAG (lcBookSS)
        ENDIF
        SELECT (lcOrderFile)
        SET RELATION TO ORDER+STYLE INTO (lcBookSS)
        lcBrTtlT   = lcBrTtlB
        lcTranFile = lcBookSS
        lcTranBr  = [STYLE :H=lcStyHdr :R ,] +;
                    [nQTY1 :H='Qty1' :6:R:P='999999',]+;
                    [nQTY2 :H='Qty2' :6:R:P='999999',]+;
                    [nQTY3 :H='Qty3' :6:R:P='999999',]+;
                    [nQTY4 :H='Qty4' :6:R:P='999999',]+;
                    [nQTY5 :H='Qty5' :6:R:P='999999',]+;
                    [nQTY6 :H='Qty6' :6:R:P='999999',]+;
                    [nQTY7 :H='Qty7' :6:R:P='999999',]+;
                    [nQTY8 :H='Qty8' :6:R:P='999999',]+;
                    [nTOTQTY :H='Quantity' :10:R:P='9999999999' ,] +;
                    [nAMOUNT :H='Amount' :13:R:P='9999999999.99']
      
      *-- Shipped quantity
      CASE lnOrdTrans= 3
        IF !USED(lcShipSS) .OR. !SEEK(laData[1],lcShipSS)
          SET ORDER TO TAG 'CINVLINE' IN 'CONSINVL'
          IF SEEK(laData[1],'CONSINVL')
            lcFile = 'CONSINVL'
          ELSE
            lcFile = 'INVLINE'
            SET ORDER TO TAG 'INVLINEO' IN 'INVLINE'
          ENDIF
          SELECT &lcFile..Order,Style,SUM(Qty1) AS nQty1,SUM(Qty2) AS nQty2,;
          SUM(Qty3) AS nQty3,SUM(Qty4) AS nQty4,SUM(Qty5) AS nQty5,SUM(Qty6) AS nQty6,;
          SUM(Qty7) AS nQty7,SUM(Qty8) AS nQty8,SUM(TOTQTY) AS nTotQTy,SUM(TOTQTY*PRICE) AS nAmount ;
          FROM (lcFile) WHERE ORDER+STR(LINENO,6)=laData[1] ;
          GROUP BY Order,Style INTO CURSOR (lcShipSS)
          INDEX ON Order+STYLE TAG (lcShipSS)
        ENDIF
        SELECT (lcOrderFile)
        SET RELATION TO Order+STYLE INTO (lcShipSS)
        lcBrTtlT = lcBrTtlS
        lcTranFile = lcShipSS
        lcTranBr  = [STYLE :H=lcStyHdr :R ,] +;
                    [nQTY1 :H='Qty1' :6:R:P='999999',]+;
                    [nQTY2 :H='Qty2' :6:R:P='999999',]+;
                    [nQTY3 :H='Qty3' :6:R:P='999999',]+;
                    [nQTY4 :H='Qty4' :6:R:P='999999',]+;
                    [nQTY5 :H='Qty5' :6:R:P='999999',]+;
                    [nQTY6 :H='Qty6' :6:R:P='999999',]+;
                    [nQTY7 :H='Qty7' :6:R:P='999999',]+;
                    [nQTY8 :H='Qty8' :6:R:P='999999',]+;
                    [nTOTQTY :H='Quantity' :10:R:P='9999999999' ,] +;
                    [nAMOUNT :H='Amount' :13:R:P='9999999999.99']
    ENDCASE
  *-- Sort by store# - Total quantity - Details
  CASE lnSortBy = 3 .AND. !llQtyPSize .AND. llDetails
    lcOrderFile = IIF(laScrMode[2],'ORDLINE',lcOrdLine)
    SET ORDER TO TAG 'ORDLINST' IN (lcOrderFile)
    *--HDM B802148,1 04/20/1999 [Start] Add Picked Quantity Field To the Details Browse
    *lcOrderBr = [STORE :H='Store' :R,STYLE :H=lcStyHdr :R ,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [PRICE :H='Price' :9:R ,TOTQTY :H='Quantity' :10:R,] +;
                [lnAmount=PRICE*TOTQTY :10:H='Amount':R:P='9999999.99']
    *E301401,1 ABD For EDI Temporary Orders, change price columns to labelled
    *E301401,1 ABD "Transmitted Price" and Amount to labelled "Wholesale Price," 
    *E301401,1 ABD Both on-screen and in outputs. [Begin]
    *lcOrderBr = [STORE :H='Store' :R,STYLE :H=lcStyHdr :R ,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [PRICE :H='Price' :9:R ,TOTQTY :H='Quantity' :10:R,] +;
                [lnAmount=PRICE*TOTQTY :10:H='Amount':R:P='9999999.99',TOTPIK :H='Tot.Pik']

    lcOrderBr = [STORE :H='Store' :R,STYLE :H=lcStyHdr :R ,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [PRICE :H=IIF(ORDHDR.cOrdType='T','Transmitted Price','Price'):9:R ,TOTQTY :H='Quantity' :10:R,] +;
                [lnAmount=PRICE*TOTQTY :10:H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='9999999999.99',TOTPIK :H='Tot.Pik']
    *E301401,1 ABD [End]
    *--HDM B802148,1 04/20/1999 [End]
    DO CASE
      *-- Open quantity
      CASE lnOrdTrans= 1
        SELECT (lcOrderFile)
        SET RELATION TO Style INTO Style
        SET RELATION TO Style+laData[31]+SPACE(10) INTO StyDye ADDITIVE
      *-- Booked quantity
      CASE lnOrdTrans= 2
        IF !USED(lcBooked)
          *B802405,1 AMM Open file with gfOpenFile()
          *USE (gcDataDir+'ORDLINE') AGAIN ALIAS (lcBooked) IN 0
          =gfOpenFile(gcDataDir+'ORDLINE','','SH',@lcBooked,.T.)
          *B802405,1 AMM end
        ENDIF
        lcBrTtlT   = lcBrTtlB
        lcTranFile = (lcBooked)
        SET ORDER TO TAG 'ORDLINE' IN (lcBooked)
        SELECT ORDLINE
        SET RELATION TO cOrdType+ORDER+STR(LINENO,6) INTO (lcBooked)
        lcTranBr = [STORE :H='Store' :R ,STYLE :H=lcStyHdr :R,] +;
                   [PRICE :H='Price' :9:R,TotBook :H='Quantity' :10:R,] +;
                   [lnAmount=PRICE*TotBook :10:H='Amount':R:P='9999999.99']
      *-- Shipped quantity
      CASE lnOrdTrans= 3
        lcBrTtlT   = lcBrTtlS
        SET ORDER TO TAG 'CINVLINE' IN 'CONSINVL'
        IF SEEK(laData[1],'CONSINVL')
          lcTranFile = 'CONSINVL'
        ELSE
          lcTranFile = 'INVLINE'
          SET ORDER TO TAG 'INVLINEO' IN 'INVLINE'
        ENDIF
        SELECT ORDLINE
        SET RELATION TO ORDER+STR(LINENO,6) INTO (lcTranFile)
        lcTranBr   = [STYLE :H=lcStyHdr :R ,STORE :H='Store' :R ,]+;
                     [PRICE :H='Price' :9:R,TOTQTY :H='Quantity' :10:R,] +;
                     [lnAmount=PRICE*TOTQTY :10:H='Amount':R:P='9999999.99',]+;
                     [INVOICE:H='Invoice' :R ,INVDATE:H='Date' :R ]
      
      *-- Production quantity
      CASE lnOrdTrans= 4
        SELECT ORDLINE
        SET RELATION TO Style INTO STYLE
        SET RELATION TO IIF(Style.Make,"1","2")+Order+STR(LineNo,6) INTO CutPick ADDITIVE
        =SEEK(lcOrdType+laData[1])
        lcBrTtlT = lcBrTtlP
        lcTranFile = 'CutPick'
        lcTranBr1 = [CutPick.cTktNo :H='Tran #' :R ,] +;
                    [Type1='C/T' :H='Type' :R ,] +;
                    [CutTktH.ENTERED  :R ,CutTktH.Complete :R,] +;
                    [CutPick.TotQty :H='Qty. Alo.' :9:R ,] +;
                    [CutTktL.TotQty :H='Qty. Ord.' :9:R ,] +;
                    [lnTotQty=CutTktL.TotQty-CutTktL.TotOrd :9:H='Qty. Opn.':R] 
         lcTranBr2= [CutPick.cTktNo :H='Tran #' :R ,] +;
                    [Type2='P/O' :H='Type' :R ,] +;
                    [PosHdr.ENTERED  :R ,PosHdr.Complete :R,] +;
                    [PosHdr.Vendor :H='Vendor ' :R ,] +;
                    [CutPick.TotQty  :H='Qty. Alo.' :9:R,] +;
                    [PosLn.TotQty   :H='Qty. Ord.' :9:R ,] +;
                    [lnTotQty=PosLn.TotQty-PosLn.TotOrd   :H='Qty. Opn.' :9:R]
        lcTranBr=IIF(Style.Make,lcTranBr1,lcTranBr2)
        llDomestic = Style.Make
      *-- Depleted orders
      CASE lnOrdTrans= 5
        IF !USED(lcDepleted) .OR. !SEEK(laData[1],lcDepleted)
          *B802544,1 Fix Inquire depleted orders
          *SELECT ORDHDR.ORDER,ORDLINE.STYLE,ORDLINE.PRICE,;
          ORDLINE.STORE,ORDLINE.BOOK1,ORDLINE.BOOK2,ORDLINE.BOOK3,ORDLINE.BOOK4,;
          ORDLINE.BOOK5,ORDLINE.BOOK6,ORDLINE.BOOK7,ORDLINE.BOOK8,ORDLINE.TOTBOOK,;
          ORDLINE.CFROMORDER,ORDLINE.BULKLINENO,ORDLINE.COMM1,ORDLINE.COMM2,;
          ORDLINE.CUSTPO,ORDLINE.GROUP,ORDLINE.PIKTKT,ORDLINE.PIKDATE;
          FROM ORDHDR,ORDLINE;
          WHERE ORDHDR.ACCOUNT+ORDHDR.cOrdType+ORDHDR.ORDER=laData[2]+lcOrdType AND ;
          ORDHDR.cFromOrder=laData[1] AND ;
          ORDLINE.cOrdType+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)=;
          lcOrdType+ORDHDR.ORDER INTO CURSOR (lcDepleted)

          SELECT ORDHDR.ORDER,ORDLINE.STYLE,ORDLINE.PRICE,;
          ORDLINE.STORE,ORDLINE.BOOK1,ORDLINE.BOOK2,ORDLINE.BOOK3,ORDLINE.BOOK4,;
          ORDLINE.BOOK5,ORDLINE.BOOK6,ORDLINE.BOOK7,ORDLINE.BOOK8,ORDLINE.TOTBOOK,;
          ORDLINE.CFROMORDER,ORDLINE.BULKLINENO,ORDLINE.COMM1,ORDLINE.COMM2,;
          ORDLINE.CUSTPO,ORDLINE.GROUP,ORDLINE.PIKTKT,ORDLINE.PIKDATE;
          FROM ORDHDR,ORDLINE;
          WHERE ORDHDR.ACCOUNT+ORDHDR.cOrdType+ORDHDR.ORDER=laData[2]+lcOrdType AND ;
          ORDLINE.cFromOrder=laData[1] AND ;
          ORDLINE.cOrdType+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)=;
          lcOrdType+ORDHDR.ORDER INTO CURSOR (lcDepleted)
          *B802544,1 (End)
          INDEX ON CFROMORDER+STR(BULKLINENO,6) TAG (lcDepleted)
        ENDIF
        lcBrTtlT   = lcBrTtlD
        lcTranFile = lcDepleted
        SELECT (lcOrderFile)
        SET RELATION TO ORDER+STR(LINENO,6) INTO (lcDepleted)
        lcTranBr = [ORDER :R ,STYLE :R ,STORE :R ,] +;
                   [GROUP :H='Grp' :R ,PRICE :R ,TOTBOOK :H= 'Qty.' :R ,] +;
                   [lnAmount=PRICE*TOTBOOK :10:H='Amount':R:P='9999999.99',] +;
                   [PIKTKT :H='Piktkt' :6:R ,PIKDATE :H='Date' :8:R]
      *E300956,1 Order lines canceled quantity
      CASE lnOrdTrans= 7
        lcBrTtlT   = lcBrTtlC
        lcTranFile = IIF(laScrMode[2],'ORDCANLN',lcOrdCanLn)
        SELECT (lcOrderFile)
        SET RELATION TO cOrdType+Order+STR(LineNo,6) INTO (lcTranFile)
        lcTranBr = [Cancelled :R :H='Date',TotQty :H= 'Total Canceld' :R,]+;
                   [cDesc=gfCodDes(cCancReson,'CCANCRESON') :R :H='Cancelation Reason']
      *E301288,1 Reham On 07/26/1999   *** Begin ***
      *E301288,1 Add new bar to dsplay related records from CutPick file.
      CASE lnOrdTrans = 8
        lcBrTtlT   = lcBrTtlW
        lcTranFile = "CutPick"
        lcTranBr   = [lnDummy=IIF(TranCd='1','CT #',IIF(TranCd='2','PO #','Adornment')) :R:10:H='Tran. Type',]+;
                     [cTktNo:R:H='Tran. #',Order :R:H='Order #', Style:R:20, Qty1:R, Qty2:R,]+;
                     [Qty3:R, Qty4:R, Qty5:R, Qty6:R, Qty7:R, Qty8:R, TotQty:R]
      *E301288,1 Reham On 07/26/1999   *** End   ***
    ENDCASE
  *-- Sort by store# - Total quantity- Summary
  CASE lnSortBy = 3 .AND. !llQtyPSize .AND. !llDetails
    IF !USED(lcOrdTS) .OR. !SEEK(lcOrdType+laData[1],lcOrdTS)
      lcFile = IIF(laScrMode[2],'ORDLINE',lcOrdLine)
      SET ORDER TO TAG ORDLINE IN (lcFile)
      *--HDM B802148,1 04/20/1999 [Start] Add Picked Quantity Field To the Details Browse
      *--HDM B802148,1 04/20/1999 [Start] Modify the select statement to include PIK fields too

      *SELECT cOrdType,&lcFile..Order,Store,SUM(Qty1) AS nQty1,SUM(Qty2) AS nQty2,;
      SUM(Qty3) AS nQty3,SUM(Qty4) AS nQty4,SUM(Qty5) AS nQty5,SUM(Qty6) AS nQty6,;
      SUM(Qty7) AS nQty7,SUM(Qty8) AS nQty8,SUM(TOTQty) AS nTotQTy,SUM(TOTQty*PRICE) AS nAmount ;
      FROM (lcFile) WHERE cOrdType+&lcFile..ORDER+STR(LINENO,6)=lcOrdType+laData[1] ;
      GROUP BY &lcFile..Order,Store INTO CURSOR (lcOrdTS)

      
      SELECT cOrdType,&lcFile..Order,Store,SUM(Qty1) AS nQty1,SUM(Qty2) AS nQty2,;
      SUM(Qty3) AS nQty3,SUM(Qty4) AS nQty4,SUM(Qty5) AS nQty5,SUM(Qty6) AS nQty6,;
      SUM(Qty7) AS nQty7,SUM(Qty8) AS nQty8,SUM(TOTQty) AS nTotQTy,SUM(TOTQty*PRICE) AS nAmount ,;
      SUM(PIK1) AS nPIK1,SUM(PIK2) AS nPIK2,SUM(PIK3) AS nPIK3,SUM(PIK4) AS nPIK4,;
      SUM(PIK5) AS nPIK5,SUM(PIK6) AS nPIK6,SUM(PIK7) AS nPIK7,SUM(PIK8) AS nPIK8,;
      SUM(TOTPIK) AS nTotPIK;
      FROM (lcFile) WHERE cOrdType+&lcFile..ORDER+STR(LINENO,6)=lcOrdType+laData[1] ;
      GROUP BY &lcFile..Order,Store INTO CURSOR (lcOrdTS)

      *--HDM B802148,1 04/20/1999 [End]
      INDEX ON cOrdType+ORDER+STORE TAG (lcOrdTS)
    ENDIF
    SELECT (lcOrdTS)
    SET RELATION TO 'S'+laData[2]+Store INTO Customer
    lcOrderFile = lcOrdTS
    *--HDM B802148,1 04/20/1999 [Start] Add Picked Quantity Field To the Details Browse
    *lcOrderBr = [STORE :H='Store' :R ,] +;
                [Customer.StName :H='Name' :30 ,] +;
                [nTOTQTY :H='Quantity' :10:R:P='9999999999'  ,] +;
                [nAMOUNT :H='Amount' :13:R:P='9999999999.99']

    *E301401,1 ABD For EDI Temporary Orders, change price columns to labelled
    *E301401,1 ABD "Transmitted Price" and Amount to labelled "Wholesale Price," 
    *E301401,1 ABD Both on-screen and in outputs. [Begin]
    *lcOrderBr = [STORE :H='Store' :R ,] +;
                [Customer.StName :H='Name' :30 ,] +;
                [nTOTQTY :H='Quantity' :10:R:P='9999999999'  ,] +;
                [nAMOUNT :H='Amount' :13:R:P='9999999999.99',nTotPIK :H = 'Picked']

    lcOrderBr = [STORE :H='Store' :R ,] +;
                [Customer.StName :H='Name' :30 ,] +;
                [nTOTQTY :H='Quantity' :10:R:P='9999999999'  ,] +;
                [nAMOUNT :H=IIF(ORDHDR.cOrdType='T','Wholesale Price',;
                'Amount'):13:R:P='9999999999.99',nTotPIK :H = 'Picked']
    *E301401,1 ABD [End]
    *--HDM B802148,1 04/20/1999 [End]
    DO CASE
      *-- Booked quantity
      CASE lnOrdTrans= 2
        IF !USED(lcBookTS) .OR. !SEEK(laData[1],lcBookTS)
          lcFile = IIF(laScrMode[2],'ORDLINE',lcOrdLine)
          SET ORDER TO TAG ORDLINE IN (lcFile)
          SELECT &lcFile..Order,Store,SUM(Book1) AS nQty1,SUM(Book2) AS nQty2,;
          SUM(Book3) AS nQty3,SUM(Book4) AS nQty4,SUM(Book5) AS nQty5,SUM(Book6) AS nQty6,;
          SUM(Book7) AS nQty7,SUM(Book8) AS nQty8,SUM(TOTBook) AS nTotQTy,SUM(TOTBook*PRICE) AS nAmount ;
          FROM (lcFile) WHERE &lcFile..cOrdType+&lcFile..ORDER+STR(LINENO,6)=lcOrdType+laData[1] ;
          GROUP BY &lcFile..Order,Store INTO CURSOR (lcBookTS)
          INDEX ON Order+STORE TAG (lcBookTS)
        ENDIF
        SELECT (lcOrderFile)
        SET RELATION TO Order+STORE INTO (lcBookTS) ADDITIVE
        lcTranFile = lcBookTS
        lcBrTtlT   = lcBrTtlB
        lcTranBr   = [STORE :H='Store' :R ,] +;
                     [nTOTQTY :H='Quantity' :10:R:P='9999999999' ,] +;
                     [nAMOUNT :H='Amount' :13:R:P='9999999999.99']

      *-- Shipped quantity
      CASE lnOrdTrans= 3
        IF !USED(lcShipTS) .OR. !SEEK(laData[1],lcShipTS)
          SET ORDER TO TAG 'CINVLINE' IN 'CONSINVL'
          IF SEEK(laData[1],'CONSINVL')
            lcFile = 'CONSINVL'
          ELSE
            lcFile = 'INVLINE'
            SET ORDER TO TAG 'INVLINEO' IN 'INVLINE'
          ENDIF
          SELECT &lcFile..Order,Store,SUM(Qty1) AS nQty1,SUM(Qty2) AS nQty2,;
          SUM(Qty3) AS nQty3,SUM(Qty4) AS nQty4,SUM(Qty5) AS nQty5,SUM(Qty6) AS nQty6,;
          SUM(Qty7) AS nQty7,SUM(Qty8) AS nQty8,SUM(TOTQTY) AS nTotQTy,SUM(TOTQTY*PRICE) AS nAmount ;
          FROM (lcFile) WHERE ORDER+STR(LINENO,6)=laData[1] ;
          GROUP BY Order,Store INTO CURSOR (lcShipTS)
          INDEX ON Order+STORE TAG (lcShipTS)
        ENDIF
        SELECT (lcOrderFile)
        SET RELATION TO Order+STORE INTO (lcShipTS) ADDITIVE
        lcBrTtlT   = lcBrTtlS
        lcTranFile = lcShipTS
        lcTranBr   = [STORE :H='Store' :12:R ,] +;
                     [nTOTQTY :H='Quantity' :10:R:P='9999999999' ,] +;
                     [nAMOUNT :H='Amount' :13:R:P='9999999999.99']
      
    ENDCASE
  *-- Sort by store# - Quantity per size- Details
  CASE lnSortBy = 3 .AND. llQtyPSize .AND. llDetails
    lcOrderFile = IIF(laScrMode[2],'ORDLINE',lcOrdLine)
    SET ORDER TO TAG 'ORDLINST' IN (lcOrderFile)
    *--HDM B802148,1 04/20/1999 [Start] Add Picked Quantity Field To the Details Browse
    *lcOrderBr = [STORE :H='Store' :R ,STYLE :H=lcStyHdr :R ,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'')+;
                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(), ]),'') +;
                [PRICE :H='Price' :9:R,TOTQTY :H='TotQty' :P='999999':R,]+;
                [lnAmount=PRICE*TOTQTY :10:H='Amount':R:P='9999999.99',]+;
                [QTY1 :H='Size1' :R:P='99999',QTY2 :H='Size2' :R:P='99999',] +;
                [QTY3 :H='Size3' :R:P='99999',QTY4 :H='Size4' :R:P='99999',] +;
                [QTY5 :H='Size5' :R:P='99999',QTY6 :H='Size6' :R:P='99999',] +;
                [QTY7 :H='Size7' :R:P='99999',QTY8 :H='Size8' :R:P='99999']

    *E301401,1 ABD For EDI Temporary Orders, change price columns to labelled
    *E301401,1 ABD "Transmitted Price" and Amount to labelled "Wholesale Price," 
    *E301401,1 ABD Both on-screen and in outputs. [Begin]
    *lcOrderBr = [STORE :H='Store' :R ,STYLE :H=lcStyHdr :R ,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'')+;
                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(), ]),'') +;
                [PRICE :H='Price' :9:R,TOTQTY :H='TotQty' :P='999999':R,]+;
                [lnAmount=PRICE*TOTQTY :10:H='Amount':R:P='9999999.99',]+;
                [QTY1 :H='Size1' :R:P='99999',QTY2 :H='Size2' :R:P='99999',] +;
                [QTY3 :H='Size3' :R:P='99999',QTY4 :H='Size4' :R:P='99999',] +;
                [QTY5 :H='Size5' :R:P='99999',QTY6 :H='Size6' :R:P='99999',] +;
                [QTY7 :H='Size7' :R:P='99999',QTY8 :H='Size8' :R:P='99999',]  +;
                [PIK1 :P='99999':R,PIK2 :P='99999':R,PIK3 :P='99999':R,PIK4 :P='99999':R,]+;
                [PIK5 :P='99999':R,PIK6 :P='99999':R,PIK7 :P='99999':R,PIK8 :P='99999':R,TOTPIK:H='Tot.Pik.']

    lcOrderBr = [STORE :H='Store' :R ,STYLE :H=lcStyHdr :R ,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'')+;
                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(), ]),'') +;
                [PRICE :H=IIF(ORDHDR.cOrdType='T','Transmitted Price','Price'):9:R,TOTQTY :H='TotQty' :P='999999':R,]+;
                [lnAmount=PRICE*TOTQTY :10:H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='9999999999.99',]+;
                [QTY1 :H='Size1' :R:P='99999',QTY2 :H='Size2' :R:P='99999',] +;
                [QTY3 :H='Size3' :R:P='99999',QTY4 :H='Size4' :R:P='99999',] +;
                [QTY5 :H='Size5' :R:P='99999',QTY6 :H='Size6' :R:P='99999',] +;
                [QTY7 :H='Size7' :R:P='99999',QTY8 :H='Size8' :R:P='99999',]  +;
                [PIK1 :P='99999':R,PIK2 :P='99999':R,PIK3 :P='99999':R,PIK4 :P='99999':R,]+;
                [PIK5 :P='99999':R,PIK6 :P='99999':R,PIK7 :P='99999':R,PIK8 :P='99999':R,TOTPIK:H='Tot.Pik.']
    *E301401,1 ABD [End]
    *--HDM B802148,1 04/20/1999 [End]
    DO CASE
      *-- Open quantity
      CASE lnOrdTrans= 1
        SELECT (lcOrderFile)
        SET RELATION TO Style INTO Style
        SET RELATION TO Style+laData[31]+SPACE(10) INTO StyDye ADDITIVE
      *-- Booked quantity
      CASE lnOrdTrans= 2
        IF !USED(lcBooked)
          *B802405,1 AMM Open file with gfOpenFile()
          *USE (gcDataDir+'ORDLINE') AGAIN ALIAS (lcBooked) IN 0
          =gfOpenFile(gcDataDir+'ORDLINE','','SH',@lcBooked,.T.)
          *B802405,1 AMM end
        ENDIF
        lcBrTtlT   = lcBrTtlB
        lcTranFile = lcBooked
        SET ORDER TO TAG 'ORDLINE' IN (lcBooked)
        SELECT ORDLINE
        SET RELATION TO cOrdType+ORDER+STR(LINENO,6) INTO (lcBooked)
        lcTranBr = [STORE :H='Store' :R ,STYLE :H=lcStyHdr :R ,] +;
                   [Book1 :H='Size1' :5:R:P='99999',]+;
                   [Book2 :H='Size2' :5:R:P='99999',]+;
                   [Book3 :H='Size3' :5:R:P='99999',]+;
                   [Book4 :H='Size4' :5:R:P='99999',]+;
                   [Book5 :H='Size5' :5:R:P='99999',]+;
                   [Book6 :H='Size6' :5:R:P='99999',]+;
                   [Book7 :H='Size7' :5:R:P='99999',]+;
                   [Book8 :H='Size8' :5:R:P='99999',]+;
                   [PRICE :H='Price' :9:R ,] +;
                   [TOTBook :H='Qty.' :6:R ,] +;
                   [lnAmount=PRICE*TOTBook :10:H='Amount':R:P='9999999.99']

      *-- Shipped quantity
      CASE lnOrdTrans= 3
        lcBrTtlT = lcBrTtlS
        SET ORDER TO TAG 'CINVLINE' IN 'CONSINVL'
        IF SEEK(laData[1],'CONSINVL')
          lcTranFile = 'CONSINVL'
        ELSE
          lcTranFile = 'INVLINE'
          SET ORDER TO TAG 'INVLINEO' IN 'INVLINE'
        ENDIF
        SELECT ORDLINE
        SET RELATION TO ORDER+STR(LINENO,6) INTO (lcTranFile)
        lcTranBr   = [STYLE :H=lcStyHdr :R,STORE :H='Store' :R ,] +;
                     [Qty1 :5:R ,Qty2 :5:R ,Qty3 :5:R ,Qty4 :5:R ,] +;
                     [Qty5 :5:R ,Qty6 :5:R ,Qty7 :5:R ,Qty8 :5:R ,] +;
                     [PRICE :H='Price' :9:R ,TOTQTY :H='Qty.' :P='999999':R ,] +;
                     [lnAmount=PRICE*TOTQTY :10:H='Amount':R:P='9999999.99',]+; 
                     [INVOICE:H='Invoice' :R ,INVDATE:H='Date' :R ]
      
      *-- Production quantity
      CASE lnOrdTrans= 4
        SELECT OrdLine
        SET RELATION TO Style INTO STYLE
        SET RELATION TO IIF(Style.Make,"1","2")+Order+STR(LineNo,6) INTO CutPick ADDITIVE
        =SEEK(lcOrdType+laData[1])
        lcBrTtlT = lcBrTtlP
        lcTranFile = 'CutPick'
        lcTranBr1= [CutPick.cTktNo :H='Tran #' :R ,] +;
                   [Type1='C/T' :H='Type' :R ,] +;
                   [CutTktH.ENTERED  :R ,CutTktH.Complete :R,] +;
                   [CutPick.Qty1 :H='Size1':6:R ,] +;
                   [CutPick.Qty2 :H='Size2':6:R ,] +;
                   [CutPick.Qty3 :H='Size3':6:R ,] +;
                   [CutPick.Qty4 :H='Size4':6:R ,] +;
                   [CutPick.Qty5 :H='Size5':6:R ,] +;
                   [CutPick.Qty6 :H='Size6':6:R ,] +;
                   [CutPick.Qty7 :H='Size7':6:R ,] +;
                   [CutPick.Qty8 :H='Size8':6:R ,]
        lcTranBr1=lcTranBr1+;
                   [CutPick.TotQty :H='Qty. Alo.' :9:R ,] +;
                   [CutTktL.Qty1 :H='Size1':6:R ,] +;
                   [CutTktL.Qty2 :H='Size2':6:R ,] +;
                   [CutTktL.Qty3 :H='Size3':6:R ,] +;
                   [CutTktL.Qty4 :H='Size4':6:R ,] +;
                   [CutTktL.Qty5 :H='Size5':6:R ,] +;
                   [CutTktL.Qty6 :H='Size6':6:R ,] +;
                   [CutTktL.Qty7 :H='Size7':6:R ,] +;
                   [CutTktL.Qty8 :H='Size8':6:R ,] +;
                   [CutTktL.TotQty :H='Qty. Ord.' :9:R ,] +;
                   [lnTotQty=CutTktL.TotQty-CutTktL.TotOrd :9:H='Qty. Opn.':R] 
        lcTranBr2= [CutPick.cTktNo :H='Tran #' :R ,] +;
                   [Type2='P/O' :H='Type' :R ,] +;
                   [PosHdr.ENTERED  :R ,PosHdr.Complete :R,] +;
                   [PosHdr.Vendor :H='Vendor ' :R ,] +;
                   [CutPick.Qty1 :H='Size1':6:R ,] +;
                   [CutPick.Qty2 :H='Size2':6:R ,] +;
                   [CutPick.Qty3 :H='Size3':6:R ,] +;
                   [CutPick.Qty4 :H='Size4':6:R ,] +;
                   [CutPick.Qty5 :H='Size5':6:R ,] +;
                   [CutPick.Qty6 :H='Size6':6:R ,] +;
                   [CutPick.Qty7 :H='Size7':6:R ,] +;
                   [CutPick.Qty8 :H='Size8':6:R ,]
        lcTranBr2=lcTranBr2+;
                   [CutPick.TotQty :H='Qty. Alo.' :9:R,] +;
                   [PosLn.Qty1 :H='Size1':6:R ,] +;
                   [PosLn.Qty2 :H='Size2':6:R ,] +;
                   [PosLn.Qty3 :H='Size3':6:R ,] +;
                   [PosLn.Qty4 :H='Size4':6:R ,] +;
                   [PosLn.Qty5 :H='Size5':6:R ,] +;
                   [PosLn.Qty6 :H='Size6':6:R ,] +;
                   [PosLn.Qty7 :H='Size7':6:R ,] +;
                   [PosLn.Qty8 :H='Size8':6:R ,] +;
                   [PosLn.TotQty :H='Qty. Ord.' :9:R ,] +;
                   [lnTotQty=PosLn.TotQty-PosLn.TotOrd   :H='Qty. Opn.' :9:R]
        lcTranBr=IIF(Style.Make,lcTranBr1,lcTranBr2)
        llDomestic = Style.Make

      *-- Depleted orders
      CASE lnOrdTrans= 5
        IF !USED(lcDepleted) .OR. !SEEK(laData[1],lcDepleted)
          *B802544,1 Fix Inquire depleted orders
          *SELECT ORDHDR.ORDER,ORDLINE.STYLE,ORDLINE.PRICE,;
          ORDLINE.STORE,ORDLINE.BOOK1,ORDLINE.BOOK2,ORDLINE.BOOK3,ORDLINE.BOOK4,;
          ORDLINE.BOOK5,ORDLINE.BOOK6,ORDLINE.BOOK7,ORDLINE.BOOK8,ORDLINE.TOTBOOK,;
          ORDLINE.CFROMORDER,ORDLINE.BULKLINENO,ORDLINE.COMM1,ORDLINE.COMM2,;
          ORDLINE.CUSTPO,ORDLINE.GROUP,ORDLINE.PIKTKT,ORDLINE.PIKDATE;
          FROM ORDHDR,ORDLINE;
          WHERE ORDHDR.ACCOUNT+ORDHDR.cOrdType+ORDHDR.ORDER=lcOrdType+laData[2] AND ;
          ORDHDR.cFromOrder=laData[1] AND ;
          ORDLINE.cOrdType+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)=;
          lcOrdType+ORDHDR.ORDER INTO CURSOR (lcDepleted)

          SELECT ORDHDR.ORDER,ORDLINE.STYLE,ORDLINE.PRICE,;
          ORDLINE.STORE,ORDLINE.BOOK1,ORDLINE.BOOK2,ORDLINE.BOOK3,ORDLINE.BOOK4,;
          ORDLINE.BOOK5,ORDLINE.BOOK6,ORDLINE.BOOK7,ORDLINE.BOOK8,ORDLINE.TOTBOOK,;
          ORDLINE.CFROMORDER,ORDLINE.BULKLINENO,ORDLINE.COMM1,ORDLINE.COMM2,;
          ORDLINE.CUSTPO,ORDLINE.GROUP,ORDLINE.PIKTKT,ORDLINE.PIKDATE;
          FROM ORDHDR,ORDLINE;
          WHERE ORDHDR.ACCOUNT+ORDHDR.cOrdType+ORDHDR.ORDER=lcOrdType+laData[2] AND ;
          ORDLINE.cFromOrder=laData[1] AND ;
          ORDLINE.cOrdType+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)=;
          lcOrdType+ORDHDR.ORDER INTO CURSOR (lcDepleted)
          *B802544,1 (End)
          INDEX ON CFROMORDER+STR(BULKLINENO,6) TAG (lcDepleted)
        ENDIF
        lcBrTtlT   = lcBrTtlD
        lcTranFile = lcDepleted
        SELECT (lcOrderFile)
        SET RELATION TO ORDER+STR(LINENO,6) INTO (lcDepleted)
        lcTranBr = [ORDER :R ,] +;
                   [STYLE ,STORE :R,BOOK1 :R,BOOK2 :R,BOOK3 :R,] +;
                   [BOOK4 :R,BOOK5 :R,BOOK6 :R,BOOK7 :R,BOOK8 :R,TOTBOOK :R ,] +;
                   [lnAmount=PRICE*TOTBOOK :10:H='Amount':R:P='9999999.99',] +;
                   IIF(laSetups[2,2]='Y', ;
                   "n=laData[27] :H='Rep 1' :R ,COMM1 :H='Comm 1' :R ," +;
                   "n1=laData[29] :H='Rep 2':R ,COMM2 :H='Comm 2' :R ",'') +;
                   IIF(laData[7],[,CUSTPO :H='Cust P/O':R],'')
      *E300956,1 Update Order lines canceled quantity
      CASE lnOrdTrans= 7
        lcBrTtlT   = lcBrTtlC
        lcTranFile = IIF(laScrMode[2],'ORDCANLN',lcOrdCanLn)
        SELECT (lcOrderFile)
        SET RELATION TO cOrdType+Order+STR(LineNo,6) INTO (lcTranFile)
        lcTranBr = [Cancelled :R :H='Date',TotQty :H= 'Tot. Can.' :R,]+;
                   [cDesc=gfCodDes(cCancReson,'CCANCRESON') :R :H='Cancelation Reason',]+;
                   [Qty1 :R :H='Can1',Qty2 :R :H='Can2',Qty3 :R :H='Can3',Qty4 :R :H='Can4',]+;
                   [Qty5 :R :H='Can5',Qty6 :R :H='Can6',Qty7 :R :H='Can7',Qty8 :R :H='Can8']
      *E301288,1 Reham On 07/26/1999   *** Begin ***
      *E301288,1 Add new bar to dsplay related records from CutPick file.
      CASE lnOrdTrans = 8
        lcBrTtlT   = lcBrTtlW
        lcTranFile = "CutPick"
        lcTranBr   = [lnDummy=IIF(TranCd='1','CT #',IIF(TranCd='2','PO #','Adornment')) :R:10:H='Tran. Type',]+;
                     [cTktNo:R:H='Tran. #',Order :R:H='Order #', Style:R:20, Qty1:R, Qty2:R,]+;
                     [Qty3:R, Qty4:R, Qty5:R, Qty6:R, Qty7:R, Qty8:R, TotQty:R]
      *E301288,1 Reham On 07/26/1999   *** End   ***
    ENDCASE
  *-- Sort by store# - Quantity per size- Summary
  CASE lnSortBy = 3 .AND. llQtyPSize .AND. !llDetails
    IF !USED(lcOrdTS) .OR. !SEEK(lcOrdType+laData[1],lcOrdTS)
      lcFile = IIF(laScrMode[2],'ORDLINE',lcOrdLine)
      SET ORDER TO TAG ORDLINE IN (lcFile)
      *--HDM B802148,1 04/20/1999 [Start] Add Picked Quantity Field To the Details Browse
      *--HDM B802148,1 04/20/1999 [Start] Modify the select statement to include PIK fields too

      *SELECT cOrdType,&lcFile..Order,Store,SUM(Qty1) AS nQty1,SUM(Qty2) AS nQty2,;
      SUM(Qty3) AS nQty3,SUM(Qty4) AS nQty4,SUM(Qty5) AS nQty5,SUM(Qty6) AS nQty6,;
      SUM(Qty7) AS nQty7,SUM(Qty8) AS nQty8,SUM(TOTQTY) AS nTotQTy,SUM(TOTQTY*PRICE) AS nAmount ;
      FROM (lcFile) WHERE cOrdType+&lcFile..Order+STR(LINENO,6)=lcOrdType+laData[1] ;
      GROUP BY &lcFile..Order,Store INTO CURSOR (lcOrdTS)

      SELECT cOrdType,&lcFile..Order,Store,SUM(Qty1) AS nQty1,SUM(Qty2) AS nQty2,;
      SUM(Qty3) AS nQty3,SUM(Qty4) AS nQty4,SUM(Qty5) AS nQty5,SUM(Qty6) AS nQty6,;
      SUM(Qty7) AS nQty7,SUM(Qty8) AS nQty8,SUM(TOTQTY) AS nTotQTy,SUM(TOTQTY*PRICE) AS nAmount, ;
      SUM(PIK1) AS nPIK1,SUM(PIK2) AS nPIK2,SUM(PIK3) AS nPIK3,SUM(PIK4) AS nPIK4,;
      SUM(PIK5) AS nPIK5,SUM(PIK6) AS nPIK6,SUM(PIK7) AS nPIK7,SUM(PIK8) AS nPIK8,;
      SUM(TOTPIK) AS nTotPIK;
      FROM (lcFile) WHERE cOrdType+&lcFile..Order+STR(LINENO,6)=lcOrdType+laData[1] ;
      GROUP BY &lcFile..Order,Store INTO CURSOR (lcOrdTS)
      
      *--HDM B802148,1 04/20/1999 [End]
      INDEX ON cOrdType+ORDER+STORE TAG (lcOrdTS)
    ENDIF
    lcOrderFile = lcOrdTS
    *--HDM B802148,1 04/20/1999 [Start] Add Picked Quantity Field To the Details Browse
    *lcOrderBr = [STORE :H='Store' :8:R ,] +;
                [nQTY1 :H='Qty1' :6:R:P='999999',]+;
                [nQTY2 :H='Qty2' :6:R:P='999999',]+;
                [nQTY3 :H='Qty3' :6:R:P='999999',]+;
                [nQTY4 :H='Qty4' :6:R:P='999999',]+;
                [nQTY5 :H='Qty5' :6:R:P='999999',]+;
                [nQTY6 :H='Qty6' :6:R:P='999999',]+;
                [nQTY7 :H='Qty7' :6:R:P='999999',]+;
                [nQTY8 :H='Qty8' :6:R:P='999999',]+;
                [nTOTQTY :H='Tot. Qty.' :10:R:P='9999999999' ,] +;
                [nAMOUNT :H='Amount' :13:R:P='9999999999.99'] 

    *E301401,1 ABD For EDI Temporary Orders, change price columns to labelled
    *E301401,1 ABD "Transmitted Price" and Amount to labelled "Wholesale Price," 
    *E301401,1 ABD Both on-screen and in outputs. [Begin]
    *lcOrderBr = [STORE :H='Store' :8:R ,] +;
                [nQTY1 :H='Qty1' :6:R:P='999999',]+;
                [nQTY2 :H='Qty2' :6:R:P='999999',]+;
                [nQTY3 :H='Qty3' :6:R:P='999999',]+;
                [nQTY4 :H='Qty4' :6:R:P='999999',]+;
                [nQTY5 :H='Qty5' :6:R:P='999999',]+;
                [nQTY6 :H='Qty6' :6:R:P='999999',]+;
                [nQTY7 :H='Qty7' :6:R:P='999999',]+;
                [nQTY8 :H='Qty8' :6:R:P='999999',]+;
                [nTOTQTY :H='Tot. Qty.' :10:R:P='9999999999' ,] +;
                [nAMOUNT :H='Amount' :13:R:P='9999999999.99',] +;
                [nPIK1 :H='Pik1' :P='99999':R,]+;
                [nPIK2 :H='Pik2' :P='99999':R,]+;
                [nPIK3 :H='Pik3' :P='99999':R,]+;
                [nPIK4 :H='Pik4' :P='99999':R,]+;
                [nPIK5 :H='Pik5' :P='99999':R,]+;
                [nPIK6 :H='Pik6' :P='99999':R,]+;
                [nPIK7 :H='Pik7' :P='99999':R,]+;
                [nPIK8 :H='Pik8' :P='99999':R,]+;
                [nTotPik :H='Tot.Pik' :P='99999':R]

    lcOrderBr = [STORE :H='Store' :8:R ,] +;
                [nQTY1 :H='Qty1' :6:R:P='999999',]+;
                [nQTY2 :H='Qty2' :6:R:P='999999',]+;
                [nQTY3 :H='Qty3' :6:R:P='999999',]+;
                [nQTY4 :H='Qty4' :6:R:P='999999',]+;
                [nQTY5 :H='Qty5' :6:R:P='999999',]+;
                [nQTY6 :H='Qty6' :6:R:P='999999',]+;
                [nQTY7 :H='Qty7' :6:R:P='999999',]+;
                [nQTY8 :H='Qty8' :6:R:P='999999',]+;
                [nTOTQTY :H='Tot. Qty.' :10:R:P='9999999999' ,] +;
                [nAMOUNT :H=IIF(ORDHDR.cOrdType='T','Transmitted Price',;
                'Price') :13:R:P='9999999999.99',] +;
                [nPIK1 :H='Pik1' :P='99999':R,]+;
                [nPIK2 :H='Pik2' :P='99999':R,]+;
                [nPIK3 :H='Pik3' :P='99999':R,]+;
                [nPIK4 :H='Pik4' :P='99999':R,]+;
                [nPIK5 :H='Pik5' :P='99999':R,]+;
                [nPIK6 :H='Pik6' :P='99999':R,]+;
                [nPIK7 :H='Pik7' :P='99999':R,]+;
                [nPIK8 :H='Pik8' :P='99999':R,]+;
                [nTotPik :H='Tot.Pik' :P='99999':R]
    *E301401,1 ABD [End]
    *--HDM B802148,1 04/20/1999 [End]
    DO CASE
      *-- Booked quantity
      CASE lnOrdTrans= 2
        IF !USED(lcBookTS) .OR. !SEEK(laData[1],lcBookTS)
          lcFile = IIF(laScrMode[2],'ORDLINE',lcOrdLine)
          SET ORDER TO TAG ORDLINE IN (lcFile)
          SELECT &lcFile..Order,Store,SUM(BOOK1) AS nQty1,SUM(BOOK2) AS nQty2,;
          SUM(BOOK3) AS nQty3,SUM(BOOK4) AS nQty4,SUM(BOOK5) AS nQty5,SUM(BOOK6) AS nQty6,;
          SUM(BOOK7) AS nQty7,SUM(BOOK8) AS nQty8,SUM(TOTBOOK) AS nTotQTy,SUM(TOTBOOK*PRICE) AS nAmount ;
          FROM (lcFile) WHERE &lcFile..cOrdType+&lcFile..ORDER+STR(LINENO,6)=lcOrdType+laData[1] ;
          GROUP BY &lcFile..Order,Store INTO CURSOR (lcBookTS)
          INDEX ON Order+STORE TAG (lcBookTS)
        ENDIF
        SELECT (lcOrderFile)
        SET RELATION TO Order+STORE INTO (lcBookTS)
        lcTranFile = lcBookTS
        lcBrTtlT   = lcBrTtlB
        lcTranBr   = [STORE :H='Store' :8:R ,] +;
                     [nQTY1 :H='Qty1' :6:R:P='999999',]+;
                     [nQTY2 :H='Qty2' :6:R:P='999999',]+;
                     [nQTY3 :H='Qty3' :6:R:P='999999',]+;
                     [nQTY4 :H='Qty4' :6:R:P='999999',]+;
                     [nQTY5 :H='Qty5' :6:R:P='999999',]+;
                     [nQTY6 :H='Qty6' :6:R:P='999999',]+;
                     [nQTY7 :H='Qty7' :6:R:P='999999',]+;
                     [nQTy8 :H='Qty8' :6:R:P='999999',]+;
                     [nTOTQTY :H='Tot. Qty.' :10:R:P='9999999999' ,] +;
                     [nAMOUNT :H='Amount'   :13:R:P='9999999999.99']

      *-- Shipped quantity
      CASE lnOrdTrans= 3
        IF !USED(lcShipTS) .OR. !SEEK(laData[1],lcShipTS)
          SET ORDER TO TAG 'CINVLINE' IN 'CONSINVL'
          IF SEEK(laData[1],'CONSINVL')
            lcFile = 'CONSINVL'
          ELSE
            lcFile = 'INVLINE'
            SET ORDER TO TAG 'INVLINEO' IN 'INVLINE'
          ENDIF
          SELECT &lcFile..Order,Store,SUM(Qty1) AS nQty1,SUM(Qty2) AS nQty2,;
          SUM(Qty3) AS nQty3,SUM(Qty4) AS nQty4,SUM(Qty5) AS nQty5,SUM(Qty6) AS nQty6,;
          SUM(Qty7) AS nQty7,SUM(Qty8) AS nQty8,SUM(TOTQTY) AS nTotQTy,SUM(TOTQTY*PRICE) AS nAmount ;
          FROM (lcFile) WHERE ORDER+STR(LINENO,6)=laData[1] ;
          GROUP BY Order,Store INTO CURSOR (lcShipTS)
          INDEX ON Order+STORE TAG (lcShipTS)
        ENDIF
        SELECT (lcOrderFile)
        SET RELATION TO Order+STORE INTO (lcShipTS)
        lcTranFile = lcShipTS
        lcBrTtlT   = lcBrTtlS
        lcTranBr   = [STORE :H='Store' :8:R ,] +;
                     [nQTY1 :H='Qty1' :6:R:P='999999' ,] +;
                     [nQTY2 :H='Qty2' :6:R:P='999999' ,] +;
                     [nQTY3 :H='Qty3' :6:R:P='999999' ,] +;
                     [nQTY4 :H='Qty4' :6:R:P='999999' ,] +;
                     [nQTY5 :H='Qty5' :6:R:P='999999' ,] +;
                     [nQTY6 :H='Qty6' :6:R:P='999999' ,] +;
                     [nQTY7 :H='Qty7' :6:R:P='999999' ,] +;
                     [nQTy8 :H='Qty8' :6:R:P='999999' ,] +;
                     [nTOTQTY :H='Tot. Qty.' :10:R:P='9999999999' ,] +;
                     [nAMOUNT :H='Amount'   :13:R:P='9999999999.99']
    ENDCASE
ENDCASE
WAIT CLEAR

*!*************************************************************
*! Name      : lfCanReason
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Read order cancellation reason
*!*************************************************************
*! Calls     : ARORDER.SPX
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfCanReason()
*!*************************************************************
FUNCTION lfCanReason
PRIVATE lnCanReason

STORE 1  TO lnCanReason
=gfwCodePop(@laCodes,'CCANCRESON','L')

DO (gcScrDir+"ARORDER.SPX") WITH 'X','lnCanReason'
RETURN(ALLTRIM(laCanReason[lnCanReason,2]))

*!*************************************************************
*! Name      : lpDelScr
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Cancel/Uncancel order
*!*************************************************************
*! Calls     : gfModalGen,lfGetInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lpDelScr()
*!*************************************************************
FUNCTION lpDelScr

*E301175,1 I have removed the code of the function lpDelScr to a new
*E301175,1 program called SOUPDATE with the name lfDelScr to add the
*E301175,1 capability to call the sales order Delete function from
*E301175,1 the EDI [Begin]
DO lfDelScr IN (gcAppHome + 'SO\SOUPDATE.FXP')
*E301175,1 I have removed the code of the functions lpDelScr [End]

*!*************************************************************
*! Name      : lfEditAlo
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Edit alocated quantities for a specific size
*!*************************************************************
*! Calls     : SOALO.SPX
*!*************************************************************
*! Passed Parameters  : lcEditSize   : Size
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfEditAlo('1')
*!*************************************************************
FUNCTION lfEditAlo
PARAMETERS lcEditSize
PRIVATE lnAlias,llUpdCTkt

lnAlias    = SELECT()
lcTempCurs = gfTempName()
SELECT (lcAlocated)
=AFIELDS(laFileStru)
CREATE CURSOR (lcTempCurs) FROM ARRAY laFileStru
SELECT (lcAlocated)
SCAN FOR CORDLINE = STR(&lcOrdLine..LineNo,6)
  SCATTER TO laAlocated
  INSERT INTO (lcTempCurs) FROM ARRAY laAlocated
ENDSCAN
SELECT (lcTempCurs)
GO TOP 
llUpdAlo = .F.
SET ORDER TO TAG CUTPKORD IN (lcAlocated)
DO (gcScrDir+gcWinAppl+"\SOALO.SPX")
IF llUpdAlo
  *E300420,1 Message : 32006
  *E300420,1 Cuttkt# xxxxxx status is HOLD. Do you wish to update the generated 
  *E300420,1 cuttkt quantities accordingly?
  *E300420,1 Button : 32000 
  *E300420,1 Yes/No

  *E300420,1 Message : 32007
  *E300420,1 Purchase order# xxxxxx status is HOLD. Do you wish to update the generated 
  *E300420,1 Purchase order quantities accordingly?
  *E300420,1 Button : 32000 
  *E300420,1 Yes/No
  
  *E300420,1 Message : 32008
  *E300420,1 Do you wish to update Purchase order# xxxxxx quantities accordingly?
  *E300420,1 Button : 32000 
  *E300420,1 Yes/No

  *E301077,5 Inhance openning files to speed up transaction
  IF 'PO' $ gcCmpModules
    =gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
  ENDIF
  IF 'MF' $ gcCmpModules
    =gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')
  ENDIF
  *E301077,5 (End)
  SELECT (lcTempCurs)
  SCAN
    *E301182,1 Update CT/PO line number in the CUTPICK file.
    *=SEEK(TRANCD+CTKTNO+ORDER+STYLE+CORDLINE,lcAlocated)
    =SEEK(TRANCD+CTKTNO+CTKTLINENO+ORDER+STYLE+CORDLINE,lcAlocated)
    *E301182,1 (End)
    *E301077,5 Update hold tickets only
    *llUpdCTkt = (TranCd='1' .AND. SEEK(CTKTNO,'CUTTKTH') .AND. ;
                 CUTTKTH.Status = 'H' .AND. ;
                 gfModalGen('QRM32006B32000','ALERT',CTKTNO)=1) .OR. ;
                (TranCd='2' .AND. SEEK('P'+CTKTNO,'POSHDR') .AND. ;
                 ( ( POSHDR.Status='H' .AND. ;
                   gfModalGen('QRM32007B32000','ALERT',CTKTNO)=1) .OR. ;
                  ( POSHDR.Status $ 'OA' .AND. !laSetups[13,2] .AND. ;
                    gfModalGen('QRM32008B32000','ALERT',CTKTNO)=1)))
    llUpdCTkt = (TranCd='1' .AND. SEEK(CTKTNO,'CUTTKTH') .AND. ;
                 CUTTKTH.Status = 'H' .AND. ;
                 gfModalGen('QRM32006B32000','ALERT',CTKTNO)=1) .OR. ;
                (TranCd='2' .AND. SEEK('P'+CTKTNO,'POSHDR') .AND. ;
                 POSHDR.Status='H' .AND. ;
                 gfModalGen('QRM32007B32000','ALERT',CTKTNO)=1)
    *E301077,5 (End)
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE TotCut WITH TotCut - &lcAlocated..Qty&lcEditSize + &lcTempCurs..Qty&lcEditSize
    UNLOCK
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE Cut&lcEditSize WITH Cut&lcEditSize - ;
            &lcAlocated..Qty&lcEditSize + &lcTempCurs..Qty&lcEditSize ,;
            TotCut WITH TotCut - &lcAlocated..Qty&lcEditSize + &lcTempCurs..Qty&lcEditSize
    UNLOCK
    SELECT (lcAlocated)
    =RLOCK()
    REPLACE Qty&lcEditSize WITH &lcTempCurs..Qty&lcEditSize ,;
            TotQty         WITH &lcTempCurs..TotQty 
    IF llUpdCTkt .AND. !(lcEditSize $ cUpdSizes)
      REPLACE cUpdSizes WITH ALLTRIM(cUpdSizes)+lcEditSize
    ENDIF
    IF !llUpdCTkt .AND. (lcEditSize $ cUpdSizes)
      REPLACE cUpdSizes WITH STRTRAN(cUpdSizes,lcEditSize,'')
    ENDIF
    UNLOCK
  ENDSCAN
  *E301077,5 Inhance openning files to speed up transaction
  IF 'PO' $ gcCmpModules
    =gfCloseFile('POSHDR')
  ENDIF
  IF 'MF' $ gcCmpModules
    =gfCloseFile('CUTTKTH')
  ENDIF
  *E301077,5 (End)
ENDIF
SET ORDER TO TAG CUTORD IN (lcAlocated)
SELECT (lnAlias)
RETURN(llUpdAlo)

*!*************************************************************
*! Name      : lfDAlocated
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Deactivate function for edit allocation screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfDAlocated()
*!*************************************************************
FUNCTION lfDAlocated
IF WONTOP() = lcBrTtlL
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL ALT+C   DO lpTap WITH 'SOALO1','pbCancel',.T.
  ON KEY LABEL TAB     DO lpTab WITH 'SOALO1','pbOk'
  ON KEY LABEL BACKTAB DO lpBackTab WITH 'SOALO1','pbCancel'
ENDIF  
RETURN .F.

*!*************************************************************
*! Name      : lfwAlocated
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Browse function for edit allocation screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwAlocated()
*!*************************************************************
FUNCTION lfwAlocated
PRIVATE lcFields,lnCount,lcCount

lcFields = "cTktNo  :H='Ticket#' :R," 
=SEEK('S'+&lcOrdLine..Scale,'Scale')
FOR lnCount = 1 TO 8
  lcCount = STR(lnCount,1)
  IF lcCount = lcEditSize
    lcFields=lcFields+"Qty"+lcCount+" :H='"+Scale.Sz&lcCount+"' :V=lfvGetFld()," 
  ELSE
    lcFields=lcFields+"Qty"+lcCount+" :H='"+Scale.Sz&lcCount+"' :R,"
  ENDIF
ENDFOR
lcFields=lcFields+"TotQty :H='TotQty' :R" 
BROWSE FIELDS &lcFields ;
       WINDOW SOALO0    ;
       IN WINDOW SOALO  ;
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
	   NOCLEAR          ;
       TITLE lcBrTtlL 

*!*************************************************************
*! Name      : lfvGetFld
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validation function for allocated quantity
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvGetFld()
*!*************************************************************
FUNCTION lfvGetFld

IF SEEK(TRANCD+CTKTNO+ORDER+STYLE+CORDLINE,lcAlocated) .AND. ;
   Qty&lcEditSize > &lcAlocated..Qty&lcEditSize
  *E300420,1 Message : 32009
  *E300420,1 The allocated quantity cannot be greater than xxxx .
  *E300420,1 Button : 00000 
  *E300420,1 Ok
  =gfModalGen('TRM32009B00000','ALERT',ALLTRIM(STR(&lcAlocated..Qty&lcEditSize)))

  REPLACE Qty&lcEditSize WITH &lcAlocated..Qty&lcEditSize
ENDIF
REPLACE TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8

*!*************************************************************
*! Name      : lfvUpdAlo
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validation function for Edit allocated quantity screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvUpdAlo()
*!*************************************************************
FUNCTION lfvUpdAlo
PRIVATE lnAloQty

SELECT (lcTempCurs)
SUM Qty&lcEditSize TO lnAloQty
GO TOP
*E300420,1 Message : 32010
*E300420,1 The allocated quantity is not equal to the ordered quantity for 
*E300420,1 size xxxxx.
*E300420,1 Button : 32001 
*E300420,1 Accept/Modify

IF lnAloQty <> m.Qty&lcEditSize .AND. ;
  gfModalGen('QRM32010B32001','ALERT',ALLTRIM(Scale.Sz&lcEditSize))=2
  RETURN
ENDIF
llUpdAlo = .T.
SELECT (lcOrdLine)
m.Qty&lcEditSize = lnAloQty
CLEAR READ

*!*************************************************************
*! Name      : lfvObjLink
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Call the object link screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvObjLink()
*!*************************************************************
FUNCTION lfvObjLink
PRIVATE lnAlais
lnAlais = SELECT()
DO GetObj WITH 'O',laData[1]
SELECT (lnAlais)

*!*************************************************************
*! Name      : lfGetDefCode
*! Developer : Wael ALy MOhamed
*! Date      : 05/14/1997
*! Purpose   : Get defaut code
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcCodeType : Code Type
*!*************************************************************
*! Return      : Default code
*!*************************************************************
FUNCTION lfGetDefCode
PARAMETERS lcCodeType
PRIVATE lcCurrTag

lcCurrTag = ORDER('CODES')
SET ORDER TO TAG 'CCODE_NO' IN 'CODES'
=SEEK('D'+PADR(UPPER(lcCodeType),10),'CODES')
SET ORDER TO TAG (lcCurrTag) IN 'CODES'
RETURN(CODES.CCODE_NO)

*!*************************************************************
*! Name      : lfvContPri
*! Developer : Wael ALy MOhamed
*! Date      : 05/14/1997
*! Purpose   : Get style contract price
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcStyle    : Style code
*!                     lcGPrice   : Gross Price
*!                     lcNPrice   : Net Price
*!                     lcDiscount : Discount Percent
*!*************************************************************
*! Return      : Contract price
*!*************************************************************
FUNCTION lfvContPri
PARAMETERS lcStyle,lcGPrice,lcNPrice,lcDiscount
PRIVATE lnAlias,lnGPrice,lnNPrice,lnDiscount,llContract
lnAlias = SELECT()

lnGPrice   = &lcGPrice
lnNPrice   = &lcNPrice
lnDiscount = &lcDiscount
llContract = .F.
*B602896,1 if no contracts exist don't make this check again.
IF llCntrExst
  llCntrExst = .F.
*B602896,1 End.

  SELECT ORDHDR
  *B802859,1 SSH Add If Cond. to prevent Change filter if called from customer screen.
  IF TYPE('lcOrderNo') <> 'C'
    SET FILTER TO 
  ENDIF
  *B802859,1 SSH (End)
  SET ORDER TO TAG ORDACCT DESCENDING
  *B802733,1 AMM Save current index
  lcOOLnTg = ORDER('ORDLINE')
  *B802733,1 AMM end
  
  SET ORDER TO TAG Ordlines IN ORDLINE
  IF SEEK(laData[2]+'C')
    SCAN REST WHILE Account+cOrdType=laData[2]+'C' ;
       FOR  !INLIST(Status,'X','B') .AND. BETWEEN(laData[8],START,COMPLETE) .AND. CCURRCODE=laData[33]
      *B602896,1 If contract exist make the contracts flag to true.
      llCntrExst = .T.
      *B602896,1 End.
      IF SEEK(lcStyle+DTOS(OrdHdr.Complete)+'C'+OrdHdr.Order+laData[3],'OrdLine') .AND. OrdLine.Price <> 0
        lnGPrice   = OrdLine.Gros_Price
        lnNPrice   = OrdLine.Price
        lnDiscount = OrdLine.Disc_Pcnt
        llContract = .T.
        EXIT
      ENDIF
    ENDSCAN
  ENDIF
  *B802733,1 AMM Restore old index
  SET ORDER TO TAG (lcOOLnTg) IN ORDLINE
  *B802733,1 AMM end
  SET ORDER TO TAG ORDHDR
  *B802859,1 SSH Add If Cond. to prevent Change filter if called from customer screen.
  IF TYPE('lcOrderNo') <> 'C'
    SET FILTER TO cOrdType+Order=lcOrdType
  ENDIF
  *B802859,1 SSH(End)
  =SEEK(lcOrdType+laData[1])
ENDIF

SELECT (lnAlias)
&lcGPrice   = lnGPrice
&lcNPrice   = lnNPrice
&lcDiscount = lnDiscount
RETURN(llContract)

*!*************************************************************
*! Name      : lfChkUnComS
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Check for uncompleted invoice sessions
*!*************************************************************
*! Calls     : gfUnCompSession,lfCratTemp,lfBrowOrd,lfBrowDet
*!*************************************************************
*! Passed Parameters  :  llFrmSetup : .T. Called from Screen setup
*!                                    .F. Called from Screen Show function
*!*************************************************************
*! Returns            :  llContinue : .T. Found an uncompleted session
*!                                        No uncompleted sessions Found
*!*************************************************************
*! Example            :  =lfChkUnComS(.F.)
*!*************************************************************
FUNCTION lfChkUnComS
PARAMETERS llFrmSetup
PRIVATE lcObject

IF (llFrmSetup AND llChkUnCom) OR !llFrmSetup
  llGoAndChk = IIF(llFrmSetup, .F., llGoAndChk)
  llChkUnCom = .F.
  IF gfUnCompSession(lcProgID, lnSessNo, "SOORD")
    STORE .F. TO laScrMode
    DO CASE
      CASE lcScrMode = 'V'
        STORE .T. TO laScrMode[2]
      CASE lcScrMode = 'E'
        STORE .T. TO laScrMode[3]
      CASE lcScrMode = 'N'
        STORE .T. TO laScrMode[4]
    ENDCASE
    STORE .T. TO llContinue,llCUpDate

    SELECT IIF(lcScrMode='V','ORDLINE',lcOrdLine)
    =SEEK(lcOrdType+lcCurrOrd)
    *E301077,5 Inhance openning files to speed up transaction
    *SET RELATION TO STYLE+laData[31]+DYELOT INTO STYDYE
    *SET RELATION TO STYLE INTO STYLE ADDITIVE
    *SCATTER MEMVAR MEMO
    *E301077,5 (End)

    SELECT IIF(lcScrMode='V','ORDHDR',lcOrdHdr)
    =SEEK(lcOrdType+lcCurrOrd)
    *E301077,5 Inhance openning files to speed up transaction
    *SCATTER FIELDS &lcScFields TO laData
    *E301077,5 (End)

    *C101557,1 Added 7th element for a new code.
    STORE IIF(laScrMode[2],'ORDHDR',lcOrdHdr) TO ;
    laCodes[1,7],laCodes[1,8],laCodes[2,7],laCodes[2,8],laCodes[3,7],;
    laCodes[3,8],laCodes[4,7],laCodes[4,8],laCodes[5,7],laCodes[5,8],;
    laCodes[7,7],laCodes[7,8]
    
    lcSession = UNCMSESS.cSession
    *E301077,5 Inhance openning files to speed up transaction
    *=lfGetInfo()
    *E301077,5 (End)

    lcObject=ALLTRIM(UnCmSess.cCurrObj)
    IF !EMPTY(lcObject)
      IF llFrmSetup AND (UPPER(lcObject)="PBSAV" .OR. UPPER(lcObject)="PBDLT")
        =IIF(UPPER(lcObject)="PBSAV",lpSavScr(),lpDelScr())
        *E301077,5 Inhance openning files to speed up transaction
        *=lfCratTemp()
        *E301077,5 (End)
        laScrMode    = .F.
        laScrMode[1] = .T.
      ELSE
        _CUROBJ = OBJNUM(&lcObject)
        KEYBOARD "{ENTER}" PLAIN CLEAR
      ENDIF
      SHOW GETS
    ENDIF
  ELSE
    *E301077,5 Inhance openning files to speed up transaction
    *=lfCratTemp()
    *E301077,5 (End)
  ENDIF
ENDIF
RETURN (llContinue)

*!*************************************************************
*! Name      : lfCratTemp
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Create temp files
*!*************************************************************
*! Calls     : gfCrtTmp
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfCratTemp()
*!*************************************************************
FUNCTION lfCratTemp
PRIVATE laFileStru,lnFileStru,laIndex

IF !laScrMode[2]
  SELECT ORDHDR
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+1,4]
  laFileStru[lnFileStru+1,1] = 'nSteps'
  laFileStru[lnFileStru+1,2] = 'N'
  laFileStru[lnFileStru+1,3] = 2
  laFileStru[lnFileStru+1,4] = 0
  =gfCrtTmp(lcOrdHdr,@laFileStru,[cOrdType+ORDER],lcOrdHdr)
ENDIF
SELECT ORDLINE
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,4]
laFileStru[lnFileStru+1,1] = 'nSteps'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'lContract'
laFileStru[lnFileStru+2,2] = 'L'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0

DECLARE laIndex[3,2]
laIndex[1,1] = 'cOrdType+ORDER+STORE+STYLE+STR(LINENO,6)'
laIndex[1,2] = 'ORDLINST'
laIndex[2,1] = 'cOrdType+ORDER+STYLE+STORE+STR(LINENO,6)'
laIndex[2,2] = 'ORDLINES'
laIndex[3,1] = 'cOrdType+ORDER+STR(LINENO,6)'
laIndex[3,2] = 'ORDLINE'
=gfCrtTmp(lcOrdLine,@laFileStru,@laIndex)
SET ORDER TO TAG 'ORDLINE' IN (lcOrdLine)
SCATTER MEMVAR BLANK MEMO

SELECT OrdLine
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'lContract'
laFileStru[lnFileStru+1,2] = 'L'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0
DECLARE laIndex[3,2]
laIndex[1,1] = 'Flag+ORDER+STR(LINENO,6)'
laIndex[1,2] = lcTempline
laIndex[2,1] = 'cOrdType+ORDER+STORE+STYLE+STR(LINENO,6)'
laIndex[2,2] = 'ORDLINST'
laIndex[3,1] = 'cOrdType+ORDER+STR(LINENO,6)'
laIndex[3,2] = 'ORDLINE'
=gfCrtTmp(lcTempline,@laFileStru,@laIndex)

*E301288,1 Reham On 07/05/1999   *** Begin ***
*E301288,1 Variable hold the temp. file name that 
*E301288,1 This file structure will be copied from BOMVAR file & hold the 
*E301288,1 styles' positions records or variant cost sheet records.
=gfOpenFile(gcDataDir+'BomVar',gcDataDir+'BomVar','SH')
SELECT BomVar
=AFIELDS(laBomVarSt)
lnBomVarSt = ALEN(laBomVarSt,1)
DIMENSION laBomVarSt[lnBomVarSt + 2 , 4]
laBomVarSt[lnBomVarSt+1,1] = 'nRecno'
laBomVarSt[lnBomVarSt+1,2] = 'N'
laBomVarSt[lnBomVarSt+1,3] = 10
laBomVarSt[lnBomVarSt+1,4] = 0
laBomVarSt[lnBomVarSt+2,1] = 'cStatus'
laBomVarSt[lnBomVarSt+2,2] = 'C'
laBomVarSt[lnBomVarSt+2,3] = 1
laBomVarSt[lnBomVarSt+2,4] = 0
=gfCrtTmp(lcT_BomVar,@laBomVarSt,[cIdType+cCost_Id+STR(LineNo,6)],lcT_BomVar)
*B603211,4 WAB - we need the temp file exclusive so we close the file and open
*B603211,4 WAB - usinf gfopenfile with parameter 'EX'
*B603211,4 WAB - START
IF USED(lcT_BomVar)
  USE IN (lcT_BomVar)
ENDIF
=gfOpenFile(gcWorkDir+lcT_BomVar,gcWorkDir+lcT_BomVar,'EX')
*B603211,4 WAB - END
*E301288,1 Reham On 07/05/1999   *** End   ***

*E301077,5 Inhance openning files to speed up transaction
*E300956,1 New file to store Order lines canceled quantity
IF laScrMode[3]
  =gfOpenFile(gcDataDir+'ORDCANLN',gcDataDir+'ORDCANLN','SH')
  =AFIELDS(laFileStru)
  =gfCrtTmp(lcOrdCanLn,@laFileStru,[CORDTYPE+ORDER+STR(LINENO,6)],lcOrdCanLn)
  =gfCloseFile('ORDCANLN')
ENDIF
*IF 'MF' $ gcCmpModules .OR. 'PO' $ gcCmpModules
*  SELECT CutPick
*  =AFIELDS(laFileStru)
*  lnFileStru = ALEN(laFileStru,1)
*  DIMENSION laFileStru[lnFileStru+2,4]
*  laFileStru[lnFileStru+1,1] = 'nSteps'
*  laFileStru[lnFileStru+1,2] = 'N'
*  laFileStru[lnFileStru+1,3] = 2
*  laFileStru[lnFileStru+1,4] = 0
*  laFileStru[lnFileStru+2,1] = 'cUpdSizes'
*  laFileStru[lnFileStru+2,2] = 'C'
*  laFileStru[lnFileStru+2,3] = 8
*  laFileStru[lnFileStru+2,4] = 0
*  DECLARE laIndex[2,2]
*  laIndex[1,1] = 'TRANCD+ORDER+CORDLINE'
*  laIndex[1,2] = 'CUTORD'
*  laIndex[2,1] = 'TRANCD+CTKTNO+ORDER+STYLE+CORDLINE'
*  laIndex[2,2] = 'CUTPKORD'
*  =gfCrtTmp(lcAlocated,@laFileStru,@laIndex)
*ENDIF
*CREATE TABLE (gcWorkDir+lcBulkOrd) (Style C(19),LineNo N(6),;
*              Book1 N(5),Book2 N(5),Book3 N(5),Book4 N(5),Book5 N(5),;
*              Book6 N(5),Book7 N(5),Book8 N(5),TotBook N(6),;
*              Open1 N(5),Open2 N(5),Open3 N(5),Open4 N(5),Open5 N(5),;
*              Open6 N(5),Open7 N(5),Open8 N(5),TotOpen N(6),;
*              Used1 N(5),Used2 N(5),Used3 N(5),Used4 N(5),Used5 N(5),;
*              Used6 N(5),Used7 N(5),Used8 N(5),TotUsed N(6))
*=AFIELDS(laFileStru)
*=gfCrtTmp(lcBulkOrd,@laFileStru,[STR(LINENO,6)],lcBulkOrd)
*E301077,5 (End)

*!*************************************************************
*! Name      : lfGetprice
*! Developer : WAM
*! Date      : 02/22/1999
*! Purpose   : Get Style Price
*!*************************************************************
*! Calls     : lfCheckPri()
*!*************************************************************
*! Passed Parameters  :  lcStyle : Style
*!                       lcLevel : Price level
*!                       lnQuantity : invoice Quantity
*!*************************************************************
*! Returns            :  Alternative price
*!*************************************************************
*! Example            :  =lfGetprice(m.Style,'A',100)
*!*************************************************************
FUNCTION lfGetprice
PARAMETERS lcStyle,lcLevel,lnQuantity
PRIVATE lnPrice

=SEEK(lcStyle,'Style')
IF lcLevel = 'Q'
  DO CASE
    CASE Style.nAtQtyC > 0 AND lnQuantity > Style.nAtQtyC
      lcLevel = 'C'
    CASE Style.nAtQtyB > 0 AND lnQuantity >Style.nAtQtyB
      lcLevel = 'B'
    OTHERWISE
      lcLevel = 'A'
  ENDCASE
ELSE
  lcLevel=IIF(INLIST(lcLevel,'A','B','C'),lcLevel,'A')
ENDIF
lnPrice = IIF(!llMulCurr OR laData[33]=gcBaseCurr,Style.Price&lcLevel,;
                      gfStyPrice(lcStyle,lcLevel,laData[33]))
RETURN(IIF(lnPrice=0,lfCheckPri(lcStyle,lcLevel),lnPrice))


*!*************************************************************
*! Name      : lfCheckPri
*! Developer : WAM
*! Date      : 02/22/1999
*! Purpose   : Select alternative price level
*!*************************************************************
*! Calls     : gfStyPrice(),SOSTYPRI.SPX
*!*************************************************************
*! Passed Parameters  :  lcStyle : Style
*!                       lcLevel : Price level
*!*************************************************************
*! Returns            :  Alternative price
*!*************************************************************
*! Example            :  =lfCheckPri('A')
*!*************************************************************
FUNCTION lfCheckPri
PARAMETERS lcStyle,lcLevel
PRIVATE lcTitle,lcPrompt,lcPrompt1,lcPrompt2,lcPrompt3,lnPrice,lnprice1,;
        lnPrice2,lnPrice3

=SEEK(lcStyle,'Style')
lcTitle  = PROPER(lcStyHdr)+': '+lcStyle
lnPrice1 = IIF(!llMulCurr OR laData[33]=gcBaseCurr,Style.PriceA,;
                       gfStyPrice(lcStyle,'A',laData[33]))
lnPrice2 = IIF(!llMulCurr OR laData[33]=gcBaseCurr,Style.PriceB,;
                       gfStyPrice(lcStyle,'B',laData[33]))
lnPrice3 = IIF(!llMulCurr OR laData[33]=gcBaseCurr,Style.PriceC,;
                       gfStyPrice(lcStyle,'C',laData[33]))
lnPrice   = IIF(lnPrice1 >0,1,IIF(lnPrice2 >0,2,AT(lcLevel,'ABC')))
lcPrompt  = "Price level '"+lcLevel+"' is zero. Proceed with"
lcPrompt1 = 'Price level A, '+ALLTRIM(STR(lnPrice1,12,2))
lcPrompt2 = 'Price level B, '+ALLTRIM(STR(lnPrice2,12,2))
lcPrompt3 = 'Price level C, '+ALLTRIM(STR(lnPrice3,12,2))

DO (gcScrDir+"SOSTYPRI.SPX")
RETURN(EVAL('lnPrice'+STR(lnPrice,1)))


*!*************************************************************
*! Name      : lfvalSty
*! Developer : Ahmed Mohamed Mohamed
*! Date      : 04/28/1999
*! Ref       : *C101493,1
*! Purpose   : Validate the entered style 
*!*************************************************************
*! Calls     : ...
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvalSty()
*!*************************************************************
FUNCTION lfvalSty

m.Style = &lcTmpSty..Style
IF (&lcTmpSty..cDivision <> laData[15] .AND. ;
        gfModalGen('TRM32020B00000','ALERT','division '+ALLTRIM(laDivision[lnDivision,1]))=1) ;
   .OR. (ALLTRIM(laData[14])<>'*' AND TRIM(&lcTmpSty..Season)<>'Y' AND &lcTmpSty..Season<>laData[14] .AND. ;
        gfModalGen('TRM32020B00000','ALERT','season '+ALLTRIM(laSeasons[lnSeason,1]))=1) ;
   .OR. (!EMPTY(&lcTmpSty..Start) .AND. &lcTmpSty..Start > laData[9] .AND. ;
        gfModalGen('QRM32021B32003','ALERT','start|'+DTOC(Style.Start))=2) ;
   .OR. (!EMPTY(&lcTmpSty..SoldOut) .AND. &lcTmpSty..SoldOut < laData[9] .AND. ;
        gfModalGen('QRM40010B40001','ALERT','sold out|'+DTOC(Style.SoldOut))=2)
  RETURN .F.
ENDIF
m.lContract = lcOrdType<>'C' .AND. lfvContPri(m.Style,'m.Gros_Price','m.Price','m.Disc_Pcnt')
IF !m.lContract
  *E301142,1 Select alternative price level
  m.Gros_Price = lfGetprice(m.Style,lcPriceLvl,m.TotQty)
ENDIF
  
IF m.Gros_Price < 0
  RETURN .F.
ENDIF
IF laSetups[5,2]='Y' .AND. !SEEK(m.Style+laData[31]+SPACE(10),'StyDye')
  *E300408,1 Message : 40012
  *E300408,1 Style/color xxx is not assigned to warehouse xxx
  *E300408,1 Button : 40002
  *E300408,1 Add Reenter
  IF gfModalGen('QRM40012B40002','ALERT',TRIM(m.Style)+'|'+TRIM(laData[31]))=1
    DO gpAdStyWar WITH m.Style,SPACE(10),laData[31]
  ELSE
    RETURN .F.
  ENDIF
ENDIF
m.Desc1    = &lcTmpSty..Desc1
m.Scale    = &lcTmpSty..Scale
m.PrePak   = &lcTmpSty..PrePak
m.Comm1    = IIF(&lcTmpSty..Commission,laData[28],0)
m.Comm2    = IIF(&lcTmpSty..Commission,laData[30],0) 
m.Season   = &lcTmpSty..Season
*B802324,1 Read field insted of variable.
*m.Gl_Sales = lcGlSales+&lcTmpSty..cSlsGlLink
*B803739,1 (Begin) Remove all spaces from LADATA[53].
*m.Gl_Sales = LADATA[53]+&lcTmpSty..cSlsGlLink
m.Gl_Sales = ALLTRIM(laData[53])+&lcTmpSty..cSlsGlLink
*B803739,1 (End)
*B802324,1 End.
m.Gl_Sales = IIF(laSetups[4,2]='Y' AND SEEK('02'+m.Gl_Sales,'Gl_Link'),m.Gl_Sales,'DEFDEF')


IF !m.lContract
  lnDisc_Pcnt = m.Disc_Pcnt
  =IIF(EMPTY(Style.CDISCCODE),.T.,gfRltFld(Style.CDISCCODE,@laDisRltFld,'CDISCCODE'))
  m.Disc_Pcnt = lnDisc_Pcnt
  m.Price     = m.Gros_Price*(100-m.Disc_Pcnt)/100
ENDIF
m.Account   = laData[2]
m.Order     = laData[1]
m.cOrdType  = lcOrdType
m.cWareCode = laData[31]



*!*************************************************************
*! Name      : FUNCTION lfChkTQty
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 05/04/1999
*! Purpose   : Check total pieces againest total quantity entered
*!*************************************************************
*! Calls     : gfModalGen(),lfvNPrice()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfChkTQty()
*!*************************************************************
FUNCTION lfChkTQty
PARAMETERS llStores

*E300408,1 Message : 32031
*E300408,1 Quantity out of balance! xxx pieces ok
*E300408,1 Button : 32000 
*E300408,1 Yes NO
IF m.TotQty <> m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8 AND ;
  gfModalGen('QRM32031B32000','ALERT',STR(m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,5))=2
  RETURN(.F.)
ENDIF
IF M.QTY1 # laOldQty[1] .OR. M.QTY2 # laOldQty[2] .OR. M.QTY3 # laOldQty[3] ;
 .OR. M.QTY4 # laOldQty[4] .OR. M.QTY5 # laOldQty[5] .OR. M.QTY6 # laOldQty[6] ;
 .OR. M.QTY7 # laOldQty[7] .OR. M.QTY8 # laOldQty[8] .OR. M.TOTQTY # laOldQty[9]
  
  STORE .T. TO llCUpdate
  m.TotQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
  *-- Get style price according to ordered quantity
  IF !m.lContract AND lcPriceLvl='Q'
    *-- Select alternative price level
    m.Gros_Price = lfGetprice(m.Style,lcPriceLvl,m.TotQty)
    m.Price = m.Gros_Price*(100-m.Disc_Pcnt)/100
  ENDIF

  IF llStores
    lnTotPcs = lnTotPcs - laOldQty[9]+m.TotQty
    lnTotAmt = lnTotAmt - laOldQty[9]*Price + m.TotQty*m.Price
  ELSE
    IF laScrMode[4] AND !EMPTY(cFromOrder) .AND. SEEK(STR(BulkLineNo,6),lcBulkOrd)
      SELECT (lcBulkOrd)
      =RLOCK()
      REPLACE USED1   WITH USED1 - &lcOrdLine..Qty1 + m.Qty1 ,;
              USED2   WITH USED2 - &lcOrdLine..Qty2 + m.Qty2 ,;
              USED3   WITH USED3 - &lcOrdLine..Qty3 + m.Qty3 ,;
              USED4   WITH USED4 - &lcOrdLine..Qty4 + m.Qty4 ,;
              USED5   WITH USED5 - &lcOrdLine..Qty5 + m.Qty5 ,;
              USED6   WITH USED6 - &lcOrdLine..Qty6 + m.Qty6 ,;
              USED7   WITH USED7 - &lcOrdLine..Qty7 + m.Qty7 ,;
              USED8   WITH USED8 - &lcOrdLine..Qty8 + m.Qty8 ,;
              TOTUSED WITH Used1+Used2+Used3+Used4+Used5+Used6+Used7+Used8
      UNLOCK
      SELECT(lcTmpSize)
    ENDIF
    =SEEK(cOrdType+Order+STR(LineNo,6),'OrdLine')
    laData[35] = laData[35] - TotBook
    laData[36] = laData[36] - TotBook*Price
    =RLOCK()
    REPLACE Book1   WITH MAX(OrdLine.Book1-OrdLine.Qty1+m.Qty1,IIF(llUpdBook,0,OrdLine.Book1))  ,;
            Book2   WITH MAX(OrdLine.Book2-OrdLine.Qty2+m.Qty2,IIF(llUpdBook,0,OrdLine.Book2))  ,;
            Book3   WITH MAX(OrdLine.Book3-OrdLine.Qty3+m.Qty3,IIF(llUpdBook,0,OrdLine.Book3))  ,;
            Book4   WITH MAX(OrdLine.Book4-OrdLine.Qty4+m.Qty4,IIF(llUpdBook,0,OrdLine.Book4))  ,;
            Book5   WITH MAX(OrdLine.Book5-OrdLine.Qty5+m.Qty5,IIF(llUpdBook,0,OrdLine.Book5))  ,;
            Book6   WITH MAX(OrdLine.Book6-OrdLine.Qty6+m.Qty6,IIF(llUpdBook,0,OrdLine.Book6))  ,;
            Book7   WITH MAX(OrdLine.Book7-OrdLine.Qty7+m.Qty7,IIF(llUpdBook,0,OrdLine.Book7))  ,;
            Book8   WITH MAX(OrdLine.Book8-OrdLine.Qty8+m.Qty8,IIF(llUpdBook,0,OrdLine.Book8))  ,;
            TotBook WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8 ,;
            FLAG WITH IIF(FLAG='N','N','M')
    UNLOCK
    
    *-- Store order line calceled quantity
    IF Flag <> 'N' AND !llUpdBook
      IF OrdLine.Qty1 > m.Qty1 OR OrdLine.Qty2 > m.Qty2 OR OrdLine.Qty3 > m.Qty3 OR ;
         OrdLine.Qty4 > m.Qty4 OR OrdLine.Qty5 > m.Qty5 OR OrdLine.Qty6 > m.Qty6 OR  ;
         OrdLine.Qty7 > m.Qty7 OR OrdLine.Qty8 > m.Qty8
        =gfwCodePop(@laCodes,'CCANCRESON','D')
        INSERT INTO (lcOrdCanLn) ;
        (cOrdType,Order,LineNo,Cancelled,cCancReson) VALUES ;
        (lcOrdType,laData[1],m.LineNo,gdSysDate,laCanReason[lnCanReason,2])
      ENDIF
      IF SEEK(lcOrdType+laData[1]+STR(m.LineNo,6),lcOrdCanLn)
        SELECT (lcOrdCanLn)
        REPLACE Qty1 WITH MAX(OrdLine.Qty1- m.Qty1,0) ,;
                Qty2 WITH MAX(OrdLine.Qty2- m.Qty2,0) ,;
                Qty3 WITH MAX(OrdLine.Qty3- m.Qty3,0) ,;
                Qty4 WITH MAX(OrdLine.Qty4- m.Qty4,0) ,;
                Qty5 WITH MAX(OrdLine.Qty5- m.Qty5,0) ,;
                Qty6 WITH MAX(OrdLine.Qty6- m.Qty6,0) ,;
                Qty7 WITH MAX(OrdLine.Qty7- m.Qty7,0) ,;
                Qty8 WITH MAX(OrdLine.Qty8- m.Qty8,0) ,;
                TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
        *B803652,1 (Begin) Update Style,Account,Store,Dyelot also.
        REPLACE ACCOUNT WITH ORDLINE.ACCOUNT,;
                STYLE   WITH ORDLINE.STYLE,;
                STORE   WITH ORDLINE.STORE,;
                DYELOT  WITH ORDLINE.DYELOT
        *B803652,1 (End
        IF TotQty > 0
          STORE lcOrdCanLn TO laCodes[6,7],laCodes[6,8]
          STORE 'lcOrdType+laData[1]+STR(m.LineNo,6)' TO laCodes[6,9]
          =gfwCodePop(@laCodes,'CCANCRESON','T')
          STORE Cancelled TO m.Cancelled
          DO (gcScrDir+"SOORDCLN.SPX")
          STORE '' TO laCodes[6,7],laCodes[6,8],laCodes[6,9]
        ELSE
          DELETE
        ENDIF
        SELECT(lcTmpSize)
      ENDIF
    ENDIF
        
    laData[35] = laData[35] + TotBook
    laData[36] = laData[36] + TotBook*m.Price

    IF FLAG <> 'N' AND !llUpdBook
      *B603776,1 AMM Compare with the old value.
      *laData[39] = MAX(laData[39] + MIN(TotQty,OrdLine.TotQty)-m.TotQty,0)
      *laData[40] = MAX(laData[40] + (MIN(TotQty,OrdLine.TotQty)-m.TotQty)*Price,0)
      laData[39] = MAX(laData[39] + MIN(laOldQty[9],OrdLine.TotQty)-m.TotQty,0)
      laData[40] = MAX(laData[40] + (MIN(laOldQty[9],OrdLine.TotQty)-m.TotQty)*Price,0)
      *B603776,1 AMM end
    ENDIF  
    laData[41] = laData[41] - laOldQty[9]+m.TOtQty
    laData[42] = laData[42] - laOldQty[9]*Price+ m.TOtQty*m.Price

    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE BOOK      WITH laData[35] ,;
            BOOKAMT   WITH laData[36] ,;
            CANCEL    WITH laData[39] ,;
            CANCELAMT WITH laData[40] ,;
            OPEN      WITH laData[41] ,;
            OPENAMT   WITH laData[42]
    UNLOCK
    SELECT(lcTmpSize)
  ENDIF

  =RLOCK()
  GATHER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,;
                       Gros_Price,Price
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laOldQty
  UNLOCK
  SCATTER MEMVAR FIELDS Book1,Book2,Book3,Book4,Book5,Book6,Book7,Book8,TotBook
ENDIF
SCATTER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty


*!*************************************************************
*! Name      : lfvOrdCatg                           *C101557,1
*! Developer : TAK
*! Date      : 06/08/1999
*! Purpose   : Valid order category code.
*!*************************************************************
*! Calls     : CODECHK
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvOrdCatg()
*!*************************************************************
FUNCTION lfvOrdCatg

IF laData[52] <> SUBSTR(laOCtgCd[lnOCtgCd,2],1,6)
  laData[52] = SUBSTR(laOCtgCd[lnOCtgCd,2],1,6)
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE cOrderCat WITH laData[52]
  UNLOCK
  llCUpdate = .T.
ENDIF
RETURN

*!*************************************************************
*! Name      : lfvBuyer                            *E301253,1
*! Developer : TAK
*! Date      : 06/10/1999
*! Purpose   : Validate or view buyer(s).
*!*************************************************************
*! Passed Parameters  : llByView-> .T. View , .F. Select/Add
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvBuyer()
*!*************************************************************
FUNCTION lfvBuyer
PARA llByView

lcContKey = 'C'+PADR(laData[2],8)+PADR(laData[3],8)
IF llByView
  lcOnKyEnr=ON('KEY','ENTER')
  =gfOpenFile(gcDataDir+'CONTACT','CONTACT','SH')
  SET KEY TO lcContKey
  GO TOP
  IF !EOF() 
    ON KEY LABEL ENTER DO lpSelBuyer
    DEFINE POPUP lcByrView FROM 3,10.625 TO 12.75,41.3 IN WINDOW (lcWinCh4) ;
	       PROMPT FIELD CONTACT SCROLL
    ACTIVATE POPUP lcByrView
    ON KEY LABEL ENTER &lcOnKyEnr
  ELSE
    _CUROBJ = OBJNUM(laData[20])
  ENDIF
  SET KEY TO 
  =lfUpdate()
ELSE
  IF !EMPTY(laData[20])
    =gfOpenFile(gcDataDir+'CONTACT','CONTACT','SH')
    IF SEEK(lcContKey)
      LOCATE REST WHILE cContType+cCont_id+Store = lcContKey FOR ALLT(UPPER(contact))= ALLT(UPPER(laData[20]))
      llNotExt = !FOUND()
    ELSE
      llNotExt = .T.
    ENDIF
    IF llNotExt AND gfModalGen('QRM32068B32000','ALERT',ALLTRIM(laData[20]))=1
      *--M32068 -- Buyer : xxxxxxxxx does not exist in this customer contracts,
      *--          Do you wish to Add this contract?
      *--B32000 -- Yes / No.
       APPEND BLANK
       REPLACE CCONTTYPE WITH 'C',;
               CCONT_ID  WITH laData[2],;
               STORE     WITH laData[3],;
               CONTACT   WITH laData[20],;
               CCONTTTL  WITH laData[20]
       =gfTraceKey('CONTACT',cContType+cCont_Id+Store+Contact,'M')
    ENDIF
    =lfUpdate()  
  ENDIF
ENDIF
RETURN

*!*************************************************************
*! Name      : lpSelBuyer                            *E301253,1
*! Developer : TAK
*! Date      : 06/10/1999
*! Purpose   : Select buyer(s).
*!*************************************************************
FUNCTION lpSelBuyer

LADATA[20] = CONTACT
SHOW GET LADATA[20]
KEYBOARD '{ESC}'
RETURN

*!*************************************************************
*! Name      : lfwStart                              *C200081,1 
*! Developer : Reham Al-Allamy
*! Date      : 06/29/1999
*! Purpose   : When function for order start date
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwStart()
*!*************************************************************
*
FUNCTION lfwStart

*C200081,1 Reham On 06/29/1999 *** Begin ***
*C200081,1 If there are user fields for this order, Call process Id for
*C200081,1 when function of the start date
IF ASCAN(laEvntTrig , PADR('WHN_STR',10)) <> 0
  llReturn = gfDoTriger('SOORD',PADR('WHN_STR',10))
  IF !llReturn
    RETURN .F.
  ENDIF
ENDIF     
*C200081,1 Reham On 06/29/1999 *** End   ***

*!*************************************************************
*! Name      : lfwComplete                           *C200081,1 
*! Developer : Reham Al-Allamy
*! Date      : 06/29/1999
*! Purpose   : When function for order completion date
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwComplete()
*!*************************************************************
*
FUNCTION lfwComplete

*C200081,1 Reham On 06/29/1999 *** Begin ***
*C200081,1 If there are user fields for this order, Call process Id to
*C200081,1 when function of the complete date
IF ASCAN(laEvntTrig , PADR('WHN_CMP',10)) <> 0
  llReturn = gfDoTriger('SOORD',PADR('WHN_CMP',10))
  IF !llReturn
    RETURN .F.
  ENDIF
ENDIF     
*C200081,1 Reham On 06/29/1999 *** End   ***

*!*************************************************************
*! Name      : lfvConfig                             *E301288,1 
*! Developer : Reham Al-Allamy
*! Date      : 07/04/1999
*! Purpose   : Valid function of the <Configure> push button.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvConfig()
*!*************************************************************
*
FUNCTION lfvConfig
PRIVATE lcCrOrdAlias

IF !EMPTY(m.Style)
  *-- Save current alias.
  lcCrOrdAlias = ALIAS()
  
  *-- Clear the trapped keys.
  PUSH KEY
  ON KEY
  
  *-- Variable hold the order price.
  *B603136,1 Start ,Convert the forign price to be in base.
  *lnRetPrice = m.Gros_Price
  IF llMulCurr AND laData[34]<>1 AND laData[34]<>0
    lcUnMeth   ='/'
    lcMethod   = gfGetExSin(@lcUnMeth,laData[33])
  *B603255,1 WAB - commit thos line so we using the current curruncy when calling
  *B603255,1 WAB - SODS_ND.SCR and get opesit method and opsit amount to used in case 
  *B603255,1 WAB - multiple curruncy
  *B603255,1 WAB -  START
  *  lnRetPrice = m.Gros_Price &lcMethod laData[34] &lcUnMeth laData[50]
  *ELSE
  *   lnRetPrice = m.Gros_Price
  *ENDIF
    lcOpsPMet = IIF(lcMethod='/','*','/')
    lcOpsPUMt = IIF(lcUnMeth='*','/','*')
  ENDIF
  lnRetPrice = m.Gros_Price
  *B603255,1 WAB -  START
  lnGrossPrice = lnRetPrice
 
  *-- Call a dialog screen to display the styles positions & the variant cost sheet.
  *DO (gcAppHome+'SODS_ND.FXP') WITH "SO" , laData[1] , lcT_BomVar , m.Style , m.LineNo , m.Gros_Price , laData[2] , m.Store , lnRetPrice
  *B603211,4 MAN-WAB - Save the menu before calling the display the styles positions & the variant cost sheet.
  *B603211,4 MAN-WAB - and release the option menu 
  *DO  (gcAppHome+'SODS_ND.FXP') WITH "SO" , laData[1] , lcT_BomVar , m.Style , m.LineNo , lnGrossPrice , laData[2] , m.Store , lnRetPrice
  PUSH MENU _MSYSMENU
  RELEASE PAD _INQUIRY OF _MSYSMENU
  DO  (gcAppHome+'SODS_ND.FXP') WITH "SO" , laData[1] , lcT_BomVar , m.Style , m.LineNo , lnGrossPrice , laData[2] , m.Store , lnRetPrice
  POP MENU _MSYSMENU
  *B603211,4 MAN-WAB - END
  *B603136,1 End.
  
  *-- Restore the trapped keys.
  POP KEY
  *B603255,1 WAB - commit this line cause we save last order bo when press new line butt.
  *B603255,1 WAB - START
  **B603211,4 WAB - save last order no and last order line 
  **B603211,4 WAB - START
  *IF SEEK("SO" + &lcOrderFile..Order + STR(&lcOrderFile..LineNo,6),lcT_BomVar)  
  *  lclastOrder= &lcOrderFile..Order 
  *  lcLastLine = STR(&lcOrderFile..LineNo,6)
  *ENDIF
  **B603211,4 WAB - END
  *B603255,1 WAB - END
  *-- Restore current alias.
  SELECT (lcCrOrdAlias)
  
  *E301288,1 Change the prompt of the configure button.
  lcConfig = IIF(SEEK("SO" + &lcOrderFile..Order + STR(&lcOrderFile..LineNo,6) , lcT_BomVar) , "\<Adornment" , "\<Configure")
  SHOW GET pbConfig,1 PROMPT lcConfig ENABLE

  *B603136,1 Start, Put the price back to forign after adding the adder.
  *B603255,1 WAB - Commit this line cause the price is returned with the current currancy
  *B603255,1 WAB - START
  *IF llMulCurr AND laData[34]<>1 AND laData[34]<>0
  *  lcOpsPMet = IIF(lcMethod='/','*','/')
  *  lcOpsPUMt = IIF(lcUnMeth='*','/','*')
  *  lnRetPrice = lnRetPrice &lcOpsPMet laData[34] &lcOpsPUMt laData[50]
  *ENDIF
  *B603255,1 WAB - END
  *B603136,1 End.  
  
  *-- If there is any added price has added to the sales order line.
  IF m.Gros_Price <> lnRetPrice
    *-- Replace the order line price with the new returned price.
    m.Gros_Price = lnRetPrice
    m.Price      = m.Gros_Price*(100-m.Disc_Pcnt)/100
    SHOW GET m.Gros_Price
    SHOW GET m.Price
    
    *-- Call valid function for the gross price.
    =lfvGPrice()
    SHOW GET m.Gros_Price
  ENDIF
ENDIF


*!*************************************************************
*! Name      : lfvConfig                             
*! Developer : Ahmed Ibrahim
*! Date      : 08/18/1999
*! Purpose   : Function to change the title "ALL" of the ship via popup 
*!             to be "At Store Level"
*! Reference : *B802366,1 AMM
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfChMSHV()
*!*************************************************************
FUNCTION lfChMSHV

FOR lnI = 1 TO ALEN(laShipVia,1)
  IF laShipVia[lnI,1] = PADR('ALL',30)
    laShipVia[lnI,1] = 'At Store Level'
    EXIT
  ENDIF
ENDFOR
SHOW GET lnShipVia

*!*************************************************************
*! Name      : lfvCompln
*! Developer : Ahmed Ibrahim
*! Date      : 08/18/1999
*! Purpose   : Valid function of complete date in order line
*! Reference : *E301305,1 AMM
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCompln()
*!*************************************************************
FUNCTION lfvCompln

PARAMETERS llStores
IF m.Complete <> EVAL(IIF(llStores,lcTempline,lcOrdLine)+'.Complete')
  IF laData[9] > m.Complete
    *E300420,1 Message : 32013
    *E300420,1 The starting date cannot be greater than the completion date.
    *E300420,1 Button : 00000
    *E300420,1 Ok
    =gfModalGen('TRM32013B00000','ALERT')
    m.Complete = EVAL(IIF(llStores,lcTempline,lcOrdLine)+'.Complete')
    RETURN
  ENDIF
  *B602713,1 ABD The message "Completion date is a Sunday" 
  *B602713,1 ABD Should not appear for Egypt. [ Begin ]
  IF  gcContCode # 'EGYPT'
    *B602713,1 ABD [ End]
    IF CDOW(m.Complete)='Saturday' .OR. CDOW(m.Complete)='Sunday'
      *E300420,1 Message : 32014
      *E300420,1 The completion date is a xxxxx.
      *E300420,1 Button : 00000
      *E300420,1 Ok
      =gfModalGen('INM32014B00000','ALERT',CDOW(m.Complete))
    ENDIF
    *B602713,1 ABD  End for if statement. [ Begin ]  
  ENDIF
  *B602713,1 ABD [ End]
  llCUpdate = .T.
  SELECT (IIF(llStores,lcTempline,lcOrdLine))
  =RLOCK()
  REPLACE Complete WITH m.Complete,;
          FLAG     WITH IIF(FLAG <> 'N','M',FLAG)
  UNLOCK
ENDIF


*!*************************************************************
*! Name      : lfvBrows
*! Developer : Ahmed Ibrahim
*! Date      : 09/09/1999
*! Purpose   : Valid Browse function.
*! Reference : *B802405,1 AMM
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvBrows()
*!*************************************************************
FUNCTION lfvBrows

IF WONTOP() # (lcOrdBrow) AND WONTOP() # (lcBrTtlZ) .AND. WONTOP() # (lcBrTtlT)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfIncSearch
*! Developer : Ahmed Ibrahim
*! Date      : 09/22/1999
*! Purpose   : Incremental search function
*! Reference : *E500295,1 AMM
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfIncSearch()
*!*************************************************************
FUNCTION lfIncSearch
*-- clear trapping

PUSH KEY
ON KEY
ON KEY LABEL F1 llDumi = .T.
PRIVATE lnBrRecNO,lcOrgTag

CLEAR TYPEAHEAD
*-- get the key was pressed
lcExpToSeek = UPPER(CHR(LASTKEY()))

*-- if the key was pressed is one of "esc,enter,tab,shift+tab" do nothing
*-- else run the incremental search screen
IF !INLIST(LASTKEY(),27,13,9,15)
    DEFINE WINDOW lwIncSrch ;
      AT 21.000, 10.000 ;
      SIZE 4.167,55.444 ;
      FONT "FoxFont", 9 ;
      STYLE "B" TITLE "Aria Apparel System" ;
      FLOAT NOCLOSE SHADOW NOMINIMIZE NONE COLOR RGB(,,,192,192,192)

    ACTIVATE WINDOW lwIncSrch NOSHOW

  CLEAR TYPEAHEAD
  KEYBOARD "{END}"  && go to the end of the edit field
  DO lfActInSr
ENDIF
POP KEY
*-- reinitialize the trapping
=lfDStores()



*!*************************************************************
*! Name      : lfActInSr
*! Developer : Ahmed Ibrahim
*! Date      : 09/22/1999
*! Purpose   : function to get the value of the inc. search and seek in file
*! Reference : *E500295,1 AMM
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfActInSr()
*!*************************************************************
FUNCTION lfActInSr
* initialize the get field
@ 1.500,14.333 GET lcExpToSeek ;
  SIZE 1.167,41.000 PICTURE '@!' FONT "FoxFont", 9 MESSAGE "Press  'ENTER'  To  Locate"
	
@ 1.334,14.111 TO 1.334,51.115 PEN 2, 8 STYLE "1" ;
  COLOR RGB(128,128,128,128,128,128)
	
@ 2.667,14.222 TO 2.667,51.115 PEN 2, 8 STYLE "1" ;
  COLOR RGB(255,255,255,255,255,255)
	
@ 1.417,14.111 TO 2.750,14.111 PEN 2, 8 ;
  COLOR RGB(128,128,128,128,128,128)
	
@ 1.417,50.893 TO 2.750,50.893 PEN 2, 8 ;
  COLOR RGB(255,255,255,255,255,255)
	
@ 1.417,4.333 SAY "Locate         :"  FONT "MS Sans Serif", 8 ;
  STYLE "B"
	
@ 0.000,0.000 TO 0.000,55.444 PEN 1, 8 STYLE "1" ;
  COLOR RGB(255,255,255,255,255,255)
	
@ 0.333,0.444 TO 0.333,55.000 PEN 1, 8 STYLE "1" ;
  COLOR RGB(128,128,128,128,128,128)
	
@ 4.083,0.000 TO 4.083,55.444 PEN 1, 8 STYLE "1" ;
  COLOR RGB(128,128,128,128,128,128)
	
@ 3.750,0.444 TO 3.750,55.000 PEN 1, 8 STYLE "1" ;
  COLOR RGB(255,255,255,255,255,255)
	
@ 0.000,0.000 TO 4.167,0.000 PEN 1, 8 ;
  COLOR RGB(255,255,255,255,255,255)
	
@ 0.333,0.444 TO 3.833,0.444 PEN 1, 8 ;
  COLOR RGB(128,128,128,128,128,128)
	
@ 0.333,54.889 TO 3.833,54.889 PEN 1, 8 ;
  COLOR RGB(255,255,255,255,255,255)
	
@ 0.000,55.333 TO 4.167,55.333 PEN 1, 8 ;
  COLOR RGB(128,128,128,128,128,128)


ACTIVATE WINDOW lwIncSrch TOP

READ MODAL
RELEASE WINDOW lwIncSrch
*E301478,1 (Begin) Save the seek exp.
IF llFromPack
  lcExactExp = lcExpToSeek
ENDIF
*E301478,1 (End)
lcExpToSeek = ALLTRIM(lcExpToSeek)
* if the user exit the window by using the enter
IF LASTKEY() = 13
  * get the current record #
  lnBrRecNO = IIF(RECNO()>RECCOUNT(),0,RECNO())
  * if the used tag is not the tag the programer want to use in
  * the inc. search function then change the active order to use
  * the programer's tag
  IF TYPE("lcSeekTag")='C' AND !EMPTY(lcSeekTag)
    lcOrgTag = SYS(22)
    lcOrgExp = SYS(14,VAL(SYS(21)))
    SET ORDER TO TAG (lcSeekTag)
  ENDIF

  
  * if the index expression is a compound index then seek
  * one by one of the value the user want to find until the
  * end of the expression or value not found in the file  
   FOR lnCount = 1 TO ALEN(laKeyExp,1)
    * if the value found in the file then go to the record that have
    * this value
    *E301478,1 (Begin) If called from Packs screen seek the generic and entered accounts.
    *IF SEEK(&lakeyExp[lnCount]+lcExpToSeek)    
    IF llFromPack
      *lcExpToSeek = PADR(lcExactExp,16)
      lcExpToSeek = RTRIM(lcExactExp)
      lcGenAcct = '*****'
      lcActKey  = IIF(PACK_ID = PADR(lcExpToSeek,16) ,IIF(ACCOUNT = '*****',laData[2],'*****'),laData[2])
      laKeyExp[1] = IIF(PACK_ID = PADR(lcExpToSeek,16) ,IIF(ACCOUNT = '*****','laData[2]','lcGenAcct'),'lcGenAcct')
    ENDIF
    IF SEEK(&lakeyExp[lnCount]+lcExpToSeek)  OR ;
       (llFromPack AND SEEK(lcActKey+lcExpToSeek))
    *E301478,1 (End)
      IF TYPE("lcOrgTag")='C'
        lcOrgKey = &lcOrgExp
        SET ORDER TO TAG lcOrgTag
        
        IF !EMPTY(lcOrgTag)
          SEEK lcOrgKey
        ENDIF  
      ENDIF
      
      CLEAR TYPEAHEAD
      RETURN
    * else if the value does not exist in the file then go back to the
    * previous record  
    ELSE
      IF TYPE("lcOrgTag")='C'
        SET ORDER TO TAG lcOrgTag
      ENDIF
      IF lnBrRecNO>0
        GO lnBrRecNO
      ENDIF  
    ENDIF
  ENDFOR
ELSE
  lcExpToSeek = ''
ENDIF
RETURN


*!*************************************************************
*! Name      : lfAdrLine
*! Developer : Walid Abd Elwahab
*! Date      : 09/22/1999
*! Purpose   :add line or more  for new order line in case there are 
*!           : an style position for order.style 
*! Reference : *B603211,4 WAB
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfAdrLine()
*!*************************************************************
FUNCTION lfAdrLine
IF EMPTY(m.STYLE) 
  RETURN
ENDIF
IF SEEK("SO" + lclastOrder + lcLastLine , lcT_BomVar)  AND !EMPTY(lcLastLine)
  *-- there are adornment data for that order line
  lnDelPrice = 0
  SELECT (lcT_BOMVAR)  
  SUM nPriceAdd FOR cIdType+cCost_ID+STR(LineNo,6)= "SO"+lclastOrder+STR(m.LineNo,6) TO lnDelPrice 
  m.Gros_Price = m.Gros_Price - lnDelPrice
  m.Price      = m.Gros_Price*(100-m.Disc_Pcnt)/100
  DELETE FOR cIdType+cCost_ID + STR(LineNo,6) =  "SO" + lclastOrder + STR(m.LineNo,6)
  SCAN FOR cIdType+cCost_ID + STR(LineNo,6) = "SO" + lclastOrder + lcLastLine 
    IF SEEK(m.Style + cStylePos ,'ICStyPos')
      *-- the style have an style position 
      lcCost_Id = cCost_Id 
      lnLineNo  = LineNo
      lcStylePos= cStylePos
      lcNDrpID  = cNDrpID 
      lcDsgnCode= cDsgnCode
      lnPriceAdd= nPriceAdd
      lnUntCost = UntCost
      lnBomTQty = nBomTotQty
      lnTotCost = TotCost
      *B603255,1 WAB - in case of multi store Check if there are name drop for the 
      *B603255,1 WAB - account+store so add the same bomvar line
      *B603255,1 WAB - START
      DECLARE laDsgnTmp[1],laDsgnCod[1]
      laDsgnTmp = ''
      laDsgnCod = ''
      llChkNmDrp = .F.
      IF SEEK(m.Account+m.Store+lcNDrpID,'IcNAmDrp')
        *laBOMVAR[4] = m.STORE
      *B603255,1 WAB - END
        lnUntCost = ICNamDrp.nNDrpCost
        lnTotCost = lnUntCost
        lnPriceAdd= ICNamDrp.nNDrpPric 
        IF llMulCurr AND laData[34]<>1 AND laData[34]<>0
          lnPriceAdd = lnPriceAdd  &lcOpsPMet laData[34] &lcOpsPUMt laData[50]
        ENDIF
        llChkNmDrp = .T.
      ELSE
        lnUntCost = 0
        lnTotCost = 0
        lnPriceAdd= 0 
        lcNDrpID  = ''
      ENDIF
      IF !EMPTY(lcNDrpID)  
        IF !EMPTY(Icnamdrp.cndrptt) .AND. !EMPTY(Icnamdrp.cndrptb)
          *-- Get the design sizes of the 2 text name-drop memo field.
          =gfSubStr(ALLTRIM(Icstypos.m2nd_dsgn) , @laDsgnTmp , ',')
        ELSE
          *-- If the name-drop is 1 text lines.
          *-- Get the design sizes of the 1 text name-drop memo field.
          =gfSubStr(ALLTRIM(Icstypos.m1nd_dsgn) , @laDsgnTmp , ',')
        ENDIF
      ELSE
        *-- Get the design codes of the "No Name-Drop" memo field into array.
        =gfSubStr(ALLTRIM(Icstypos.m0nd_dsgn) , @laDsgnTmp , ',')
        *-- Select all the design codes concatinating with its names.
      ENDIF
      *B603391,1 Start, Added order by to select.
      *SELECT cdsgnCode FROM (gcDataDir+"IcDesign") ;
               WHERE ASCAN(laDsgnTmp , Icdesign.cDsgnSize) > 0 ;
               INTO ARRAY laDsgnCod
      SELECT cdsgnCode FROM (gcDataDir+"IcDesign") ;
               WHERE ASCAN(laDsgnTmp , Icdesign.cDsgnSize) > 0 ;
               INTO ARRAY laDsgnCod ORDER BY 1
      *B603391,1 End.
      *--if the design code is assign to the style position get the cost price
      IF ASCAN(laDsgnCod , lcDsgnCode) > 0
        lnUntCost = lnUntCost + IIF(SEEK(lcDsgnCode,"ICDesign") ;
                      ,Icdesign.ndsgncst , 0)
        lnTotCost = lnUntCost
        lnDesgnPrc = ICDesign.nDsgnPrc 
        IF llMulCurr AND laData[34]<>1 AND laData[34]<>0
          lnPriceAdd =lnPriceAdd +( lnDesgnPrc &lcOpsPMet laData[34] &lcOpsPUMt laData[50])
        ELSE
          lnPriceAdd =lnPriceAdd + lnDesgnPrc
        ENDIF
      ELSE
        lcDsgnCode = ''
      ENDIF
      lnRecNo = RECNO()
      *-lineno
      lnLineNo = m.LineNo
      APPEND BLANK
      REPLACE cIdType    WITH 'SO'       ,;
              cCost_Id   WITH lcCost_Id  ,;
              LineNo     WITH lnLineNo   ,;
              cStylePos  WITH lcStylePos ,;
              cNDrpID    WITH lcNDrpID   ,;
              cDsgnCode  WITH lcDsgnCode ,;
              nPriceAdd  WITH lnPriceAdd ,;
              UntCost    WITH lnUntCost  ,;
              nBomTotQty WITH lnBomTQty  ,;
              TotCost    WITH lnTotCost  ,;
              cStatus    WITH 'A'        ,;
              nRecNo     WITH 0
      *B603255,1 WAB - add the price adder to gros_price
      *B603255,1 WAB - START 
      m.Gros_price = m.Gros_price + &lcT_BOMVAR..nPriceAdd
      m.Price      = m.Gros_Price*(100-m.Disc_Pcnt)/100
      *B603255,1 WAB - END
      IF BETWEEN(lnRecNo, 1, RECCOUNT())
        GO lnRecNo
      ENDIF
    ENDIF
  ENDSCAN
  SHOW GET m.Gros_Price ENABLE
  SHOW GET m.Price      DISABLE
  SELECT (lcOrdLine)
  REPLACE Gros_Price  WITH m.Gros_Price ,;
          Price       WITH m.Price 
  SHOW WINDOW (lcOrdBrow) REFRESH SAME          
ENDIF


*!*************************************************************
*! Name      : lfEDIStat
*! Developer : Ahmed Ibrahim
*! Date      : 12/07/1999
*! Purpose   : Get the modified EDI order status
*! Reference : *B802772,4 AMM
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  EDI Modification status
*!*************************************************************
*! Example            :  =lfEDIStat()
*!*************************************************************
FUNCTION lfEDIStat
lcRetStat = SPACE(0)
DO CASE
  CASE CLINESTAT = 'AI'
    lcRetStat = 'New Item'
  CASE CLINESTAT = 'DI'
    lcRetStat = 'Deleted Item'
  CASE CLINESTAT = 'QD'  
    lcRetStat = 'Quantity Decreased'
  CASE CLINESTAT = 'QI'
    lcRetStat = 'Quantity Increased'
  CASE CLINESTAT = 'PC'  
    lcRetStat = 'Price Changed'
  *B803690,1 AMM add a status of PQ
  CASE CLINESTAT = 'PQ'  
    lcRetStat = 'Quantity Changed'
  *B803690,1 AMM end
    
ENDCASE

RETURN lcRetStat
*!*************************************************************
*! Name      : lfOldValue
*! Developer : Abdou ElGendi
*! Date      : 02/17/2000
*! Purpose   : When function of size quantities , total and net price
*!           : to store old value of the current filed.
*! Reference : B603449,1
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
FUNCTION lfOldvalue
lcOldValue = EVALUATE('m.' + SYS(18))
*-- End Of lfoldvalue
*!*************************************************************
*! Name      : lfWOldVale
*! Developer : Abdou ElGendi
*! Date      : 02/17/2000
*! Purpose   : To store the old value value
*! Reference : B803049,1
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*B803049,1 
FUNCTION lfWOldVale
lcOldValue = EVALUATE(SYS(18))

*-- End Of lfoldvalue
*!*************************************************************
*! Name      : lfOrdStats
*! Developer : Abdou ElGendi
*! Date      : 02/17/2000
*! Purpose   : To store the old value value
*! Reference : B803049,1
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*B802614,1 
FUNCTION lfOrdStats
*-- If the system setuped to default order status to hold while the bulk status is open
IF  laSetups[7,2]='Y' .AND. ORDHDR.Status = 'O'
  IF Ordhdr.ApprAmt <> 0
    *B802614,1 Message        : Bulk order was approve,
    *B802614,1 Message        : Do you want to approve the new order.
    *B802614,1 Message NO.    : 32078
    *B802614,1 Button Message :  YES -- NO
    *B802614,1 Button         : 32000    
    IF gfModalGen('QRM32078B32000','DIALOG') = 1
      RETURN .F.
    ENDIF
  ELSE
    *B802614,1 Message        : Bulk Order status is 'Open', while order default status 
    *B802614,1 Message        : is set to 'Hold'. Would you like this new order to be 
    *B802614,1 Message        : Open?"
    *B802614,1 Button         : 32000    
    *B802614,1 Button Message : YES -- NO
    *B802614,1 Button         : 32000
    IF gfModalGen('QRM32080B32000','DIALOG') = 1
      RETURN .F.
    ENDIF
  ENDIF
ENDIF

*--End Of lfOrdStats.
*!*************************************************************

****************************************************************************************
* FUNC: lfVPacksWin
* DESC: Trap keys for Packs window when Packs is notn the current get field in browse..
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 10/08/2000
* Refer To : (E301478,1)
****************************************************************************************
FUNCTION lfVPacksWin

IF WONTOP() = lcBrTtl1 AND VARREAD()<> "PackQty"
  lcExpToSeek=""
  lcOrdExpr=""
  lnStartTrap = 32
  lnEndTrap   = 126
  FOR lnChrToTrap = lnStartTrap TO lnEndTrap
    ON KEY LABEL (CHR(lnChrToTrap)) DO lfPckIncSrch
  ENDFOR
ENDIF

****************************************************************************
* FUNC: lfPckIncSrch
* DESC: Inc search for Packs.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 10/08/2000
* Refer To : (E301478,1)
****************************************************************************
FUNCTION lfPckIncSrch

*-- clear trapping
PUSH KEY
ON KEY
ON KEY LABEL F1 llDumi = .T.
PRIVATE lnBrRecNO,lcOrgTag
CLEAR TYPEAHEAD
*-- get the key was pressed
lcExpToSeek = UPPER(CHR(LASTKEY()))
*-- if the key was pressed is one of "esc,enter,tab,shift+tab" do nothing
*-- else run the incremental search screen
IF !INLIST(LASTKEY(),27,13,9,15)
    DEFINE WINDOW lwIncSrch ;
      AT 21.000, 10.000 ;
      SIZE 4.167,55.444 ;
      FONT "FoxFont", 9 ;
      STYLE "B" TITLE "Aria Apparel System" ;
      FLOAT NOCLOSE SHADOW NOMINIMIZE NONE COLOR RGB(,,,192,192,192)
    ACTIVATE WINDOW lwIncSrch NOSHOW
  CLEAR TYPEAHEAD
  KEYBOARD "{END}"  && go to the end of the edit field
  llFromPack = .T.
  DO lfActInSr
ENDIF
POP KEY

****************************************************************************
* FUNC: lfTrapKeys
* DESC: Trap Or Clear keys for Inc Search for Packs.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 10/08/2000
* Refer To : (E301478,1)
****************************************************************************
FUNCTION lfTrapKeys
PARAMETERS llSetKeys

FOR lnChrToTrap = 32 TO 126
  IF llSetKeys
    ON KEY LABEL (CHR(lnChrToTrap)) DO lfPckIncSrch
  ELSE
    ON KEY LABEL (CHR(lnChrToTrap))  
  ENDIF
ENDFOR

