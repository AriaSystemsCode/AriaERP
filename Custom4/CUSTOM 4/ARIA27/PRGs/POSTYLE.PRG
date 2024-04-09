*:************************************************************************
*: Program file  : POSTYLE.PRG
*: Program desc. : Style P/O , Return Style P/O ,
*:                 Contracts and Inter-Location P/O.
*: For screen    : POSTYLE.SPR
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: Style Purchase Orders (PO).
*:      Developer: TAK - Timour Abdalla Khalil 
*:************************************************************************
*: Passed Parameters  : Program type   => 'P' Purchase order.
*:                                        'R' Return P/O.
*:                                        'C' Contract.
*:                                        'N' Inter-Location P/O.
*:                                        'D' Dye Order
*:                                        'A' Adornment Order
*:                      Passed PO num. => For Inquiry
*:************************************************************************
*: Modification :
*E300889,1 TAK 06/23/1998 Added to control the location editing if the
*E300889,1                system was setup to Point of Sale.
*E300935,1 MAB 07/27/1998 Add Dyelot field to screen detail folder .
*E300935,1                Also I Adjust PoSty22.scx screen.
*B602139,1 MAB 11/05/1998 Loop source and target Dyelot/Location validation
*B602139,1                untill both is valid for same dyelot in Inter-Location P/O 
*B602163,1 MAB 11/23/1998 Reenter dyelot if style does not have dyelots in any locations of Inter-Location P/O .
*B602179,1 AMM 11/25/98   Erase temporary files when quitting the program
*B602123,1 TAK 11/26/1998 Default the P/O status to On-Hold if no detail costing.
*B602156,1 TAK 11/26/1998 Donot allow to change Purchase group if lines was entered. 
*E301085,1 WAM 12/07/1998 Generate cost sheet automatically
*E301077,8 TAK 01/05/1999 Redeuce the number of opened files when start running 
*E301077,8                the progrm and open it with object when needed.
*E301077,8 MAB 03/09/1999 Update odded fields.
*B602474,1 MAB 01/25/1999 Check for sales order allocation before cancel P/O.
*B602552,1 MAB 03/01/1999 Dyelot fields is disabled in view mode.
*B602657,1 MAB 03/09/1999 Update Style grade in PosLn File.
*B602619,1 MAB 03/10/1999 Fix UnComplete session bug.
*E301169,1 MAB 03/14,1999 1- Add Gross Price, Percentage, Price to the screen
*E301169,1 MAB 03/14,1999 2- if scale count = 1 add quantity at total quantity level.
*E301176,1 HDM 03/22/1999 Prevent programs from displaying notepad icon
*                           as it's now controlled globally
*E301182,7 MAB 03/31/1999 Trace cutpick file using master key.
*B602646,1 WALID on 03/31/1999 Fix SEEK condition to be able to show the bank name .
*B602670,1 WALID on 04/06/1999 Fix The division code popup to dispplay the vendor's code
*          in case of new PO not the division code of this vendore.
*B602753,1 HDM 04/07/1999 Stop Calling NotePad Program In lpSavScr as the global save
*                         Will Call it
*B802203,1 TAK 04/28/1999 Fixed bug MF.APP does not exist if MF module not installed.
*B802130,1 TAK 05/02/1999 Fixed wrong P/O lineNo replacement due to index.
*B602769,1 TAK 05/03/1999 Fixed wrong status update when uncancel inter P/o.
*B602861,1 TAK 05/03/1999 Fixed wrong total P/o amount due to net price rounding.
*B602884,1 TAK 05/10/1999 Added to not allow to append from more than P/o in return P/o.
*B602884,1                Also don't allow to select differet sessions in append from p/o.
*E301243,1 SSE 05/31/1999 in Summary Folder , add new button that says 'Cost by unit'
*                         upon pressing , all cost figures on this screen should be changed
*                         to reflect the cost by unit and the button text will be changed to 
*                         say 'Cost by P/O'.
*E301264,1 TAK 06/20/1999 Don't allow to assign location to style in case of
*E301264,1                inter location P/O and system setup to point of sale.
*B602968,1 TAK 06/20/1999 Define the gross price from net if no discount.
*B602996,1 TAK 06/20/1999 Wrong deletion of the style from PosLn file.
*E301268,1 HDM 06/21/1999 filter vendor browse by sup. type
*E301274,1 AMM 07/06/1999 Call the extended size scale when editing PO lines if the company setuped to.
*C200080,1 AMM 07/21/1999 Add a new type for Dye orders 
*B802469,1 WAB 07/27/1999 Display E.T.A date from shipment header when browse In-transit transactions 
*E301291,1 AMM 08/01/1999 Add a new type for Adornment orders.
*E500272,1 WAB 08/08/1999 add a new butt. to call Po cost sheet screen 
*E301289,1 WAB 08/17/1999 Adding selling price & gross margin on PO lines & CT lines
*B603182,1 AMM 09/29/1999 Get nonmajor if the user typed a major and selected a color in dye order 
*B603190,1 TAK 10/11/1999 If P/O type was dye order browse contractor supply
*B603190,1                Contractor Services insted of styles.
*C200098,1 RAMY 10/20/1999 Add new field to the screen [cost per lb]
*B603177,1 RAMY 10/31/1999 Fix the bug of browsing the vendor in Default Vendor when 
*B603177,1                 call the program to 'Generat PO from Sales Order'
*B603269,1 TAK 11/09/1999 Don't display the amount in P/Os browse in no cost previlegse acsses.
*B603298,1 TAK 11/23/1999 1)Clear the link between the adornment order and the
*B603298,1                sales order if cancel adornmet order or delete line from it.
*B603298,1                2)Put the stock back if Adornmet Po or Inter location if
*B603298,1                issued line was remove by calling Gfstycrl.
*B603298,1 TAK 11/28/1999 Added a custom process for a lines browse fields of an
*B603298,1                adornment order for fresh Prouce to view order and piktkt.
*C101704,1 SAM 01/17/2000 Allow alpha numeric PO# manualy
*B603530,1 SSH 26/03/00  Fix bug of subscript out of bounds in case of
*B603530,1 SSH           style have no Non-Major.
*B603348,1 ABD           Increse Picture Of G. Price Filed.
*B803163,1 ABD  04/03/2000 The system cancels the style po after openning any screen.
*B803137,1 RAMY 05/22/2000 When the user enter style color and add qty move to the next style color
*E301410,1 RAMY 06/06/2000 Add one more parameter in calling the componenet avil. screen
*B803346,1 ADEL 06/13/2000 Let the user edit sizes when using a prepack.
*B603693,1 WMA  06/25/2000 wrong quantity of the open pieces in PO header.
*B603709,1 TAK  06/28/2000 Don't default the next style in case of Inter Location P/O and Muti ShipTo.
*B603714,1 TAK  06/29/2000 fix no file is in use.
*B603699,1 WMA  07/09/2000 check if style code structure was enter before run the style purchase order.
*B603699,1                 fix bug "Invalid function argument value" by add def. value to some mem. var.
*B803415,1 ADEL 07/16/2000 Add [E.T.A , Ship No, and Reference] fields to Options\Show transactions\Received
*B803415,1                     browse in detail folder.
*B803416,1 ABD 07/24/2000 Change the text in OPTION -> SHOW TRANSACTION menu option,
*B803416,1 ABD            The word 'Open' to be 'Ordered'
*B603849,1 RAMY 08/27/2000 Fix the bug of "invalid subsript reference ." if we coming from another screen
*C101929,1 SSH 06/09/00  Generate separate sequance no for adornment order,Dyeorder or PO.
*B803734,1 KHM 10/12/2000 Fix the bug of wrong checking on the style division
*B803734,1                when selecting a vendor that has a different 
*B803734,1                division than the company's one, then adding a 
*B803734,1                style that has the same division as the vendor.
*B604010,1 KHM 11/02/2000 Fix the bug of soessqty.fxp does not exist.
*B603713,10  HBG 11/15/2000 1-Increase the Net,Price Base price field width in the browse.
*B603713,10                 2- and in the posty22.scx 
*E301484,1 WAM 11/16/2000 Move saving function to a new separate program POUPDATE to be called 
*E301484,1 WAM            from EDI programs.
*B803858,1 HBG 11/23/2000 Display the vendor & warehouse addresses with the correct formate.
*E301520,1 AME 01/04/2001 Add fields of account, customer name, start date And complete date
*E301520,1                To the related Sales Order Browse.
*B604103,1 KHM 02/28/2001 Fix the bug of operator/ Operand type missmatch
*B604103,1                when selecting the PO cost sheet for a PO from 
*B604103,1                the style screen.
*C200162,1 MHM 02/27/2001 Custom Inter-location screen for shamsi
*B604279,1 MHM 03/07/2001 Adding the ship To to the browse in case of Inter-Loc PO 
*C200152,4 AMH 03/20/2001 custom trigger to check if the inter location PO
*C200152,4                found in open or approved batch.
*B604387,1 KHM 04/15/2001 Fix the bug of syntax error when put the focuse in
*B604387,1                a qty bucket using the mouse. While you are in the
*B604387,1                last qty bucket.
*B604420,1 KHM 04/24/2001 Fix the bug of variable nTotCur not found when edit
*B604420,1                a PO that is assigned to multi-store order.
*B604460,1 MHM 04/24/2001 Fix the bug of picking wrong "Term" code for
*B604460,1                the selected vendor.
*C102225,1 MHM 06/05/2001 new Custom Screen "Edit pack information" for GMA
*C102357,1 AMH 06/07/2001 Custom adornment order for JL.
*B804256,1 AMH 06/19/2001 Fix bug in PO cost sheet when remove line from PO.
*B604435,1 KHM 06/26/2001 Fix the bug of displaying the wrong ETA date when
*B604435,1                open the style screen while running the PO screen 
*B604435,1                then trying to disply the in-transit transaction.
*B604618,1 AMH 07/01/2001 Updating the cost element that has been added in PO C.Sh
*C200208,1 AMH 07/11/2001 Custom labelling instructions screen for STU.
*B604736,1 KHM 07/30/2001 Fix the spelling mistake of Source
*B604840,1 AMH 08/22/2001 Fix bug of Labeling Instruction option not avail in Gen PO frm SO.
*C200233,1 KHM 09/23/2001 Adding a new trigger to disable the FOB and Origin 
*C200233,1                fields in the header folder for GMA.
*B604950,1 KHM 09/30/2001 Fix the problem of not being able to access the
*B604950,1                PO cost sheet when the generate PO cost sheet 
*B604950,1                setup is Automatic.
*E301746,1 KHM 10/14/2001 Store the base currency to the duty currency.
*B603625,1 AMH 10/24/2001 Fix the confirmation message of creating cost sheet in case of Dye order.
*B605082,1 AMH 11/12/2001 Fix the bug cost of name drop does not display in case of Adornment order.
*B605105,1 AMH 11/18/2001 Fix the bug of completing the PO instead of cancel when cancel after
*B605105,1 AMH            calling the PO cost sheet screen from the PO screen.
*B605289,1 SSH 12/31/2001 Prevent the user from edit or cancel PO if it is closed
*B605356,1 AMH 01/17/2001 Fix the bug of Doublicates the PO order lines.
*B605768,1 KHM 04/01/2002 Fix the spelling mistake of Recieved.
*E301869,1 HBG 07/04/2002 Generate project for SO acoording to the setup of the module
*B605859,1 KHM 04/17/2002 Fix the bug of changing the vendor that has different
*B605859,1                division than the one in the PO.
*B605932,1 AMH 05/02/2002 Browse shipment records from custom table for KRA10.
*B606366,1 KHM 08/12/2002 Fix the bug of numeric overflow when navigating through PO's.
*E302035,4 ABD 10/14/2002 Call the extened size scale browse.
*B606496,1 AMH 11/03/2002 Fix the bug of Incorrect PO Cost Sheet required qty.
*E301969,1 AMH 12/23/2002 Release allocation when cancel PO from temporary files.
*C200452,1 TMI 01/16/2003 Add a PO quick order entry screen for David Luke
*C200491,1 ALB 01/22/2003 Generate PO lines from SO (GMA)
*B606914,1 KHM 02/03/2003 Re-arrange the extended size scale screen and refresh the detail browse
*B607009,1 AMH 03/02/2003 Fix the bug when saving open PO with style componante
*B607009,1 AMH            has diferant scale that the style scale.
*E302213,1 WAM 08/04/2003 Store pack ID in POSLN file
*B607123,1 TMI 08/04/2003 Don't go to first record when removing order line
*B607345,1 ALB 09/01/2003 Add check if user change the style purching price
*C037017,1 MHM 09/11/2003 Add option Screen to update custom filed for VAN MAR
*B607441,1 MMM 10/09/2003 Fix Bug of wrong title '_Received Lines'.
*B037225,1 NNA 01/06/2004 Fix Bug That The open field is calculated wrongly in case we are in 
*B037225,1 NNA            company extended size scale And Also Fix Bug of not browse the all scales
*B037225,1 NNA            for the selected style
*B120529,1 NNA 03/28/2004 Fix bug of that the system not calculating the number of Prepacks Automatically
*B120529,1 NNA            if we chose the pre. code and typed the total qtys. directly
*B038431,1 NNA 09/05/2004 Fix bug that when you run 'PCS Out' with Style in the PS Module and when
*B038431,1 NNA            you save ,you'll get an error massage 'Variable "lcGenProj" Not Found"
*C038291,1 TMI 08/05/2004 Add 'Purchase Order Instructions and Measurments' screen for Vijay 
*C037959,1 MHM 09/25/2004 Add a Custom menu Bar in Option pad for Customer alena
*C037957,1 MHM 10/10/2004 Custom Amend to CutPick File
*C123847,1 TMI 01/31/2005 Add qty/Ctn field between PPqty and Gross Price for DIR03
*B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index 
*B608125,1 TMI 06/14/2007 Calculate the nCost2,..., nCost5 in the PO as it is calculated in the PO Cost sheet screen (T20070122.0005)
*B608541,1 WAM 05/06/2008 Fix updating cost per line (T20080409.0029)
*:*******************************************************************************************************
PARAMETERS lcAType,lcPoCode

*--If external call for this program (not from menu) set it to True.
llExCalled = (PARAMETERS()=2) OR TYPE('lcPoCode')='C'

EXTERNAL ARRAY laData , laDefProc
*--Define Folders array, Ship from address array , Status array
*--Line Warehouse array, Style Quality array.
DECLARE lafoldwinds[3,2],laVAddrs[6],laStatus[1,2],;
        laStyGrd[3,2],laOpFile[10],laCopySav[9]

*--Initilize screen and program variables restored in setup.
STORE ' ' TO lcOldValue,lcCostImp,lcISlbl1,lcISlbl2,lcISlbl3,lcISlbl4,lcISlbl5,lcIType1,lcIType2,lcIType3,lcIType4,lcIType5,lcDelMesag
STORE ' ' TO lcStatus,lcLCBank,lcBrTtl1,lcBrTtl2,lcBrD1fld,lcBrD2fld,laVAddrs
STORE ' ' TO lcSz1,lcSz2,lcSz3,lcSz4,lcSz5,lcSz6,lcSz7,lcSz8,lcStyDesc,lReference,lcVenSty,;
             lcLastStyle,lcConPo,lcVenStat,lcDropLoc,lcLocPict,lcDefLoc,m.Dyelot,lcPickIndx

*B803137,1 RAMY [START]
lcNexSty = '  '
*B803137,1 RAMY [END]
*-- Screen  variables :-  
STORE ' ' TO lcWinCh0,lcWinCh1,lcWinCh2,lcWinC21,lcWinC22,lcWinC23,lcWinC24,lcWinCh3
*-- Files & folders variables :-
STORE ' ' TO lcScFields,lcfolder,lcfoldprnt,lcPOHdr,lcPOLine,lcPOTran,lcTmpNdNo,lcContrt,lcPOType,lcTmpCtPk,lcTmpPOLn

*B606914,1 KHM 02/03/2003 (Begin) Initializing a vairble to hold using extended size scale screen.
llUseExSSc = gfGetMemVar('M_EXSPO_SC')
*B606914,1 KHM 02/03/2003 (End)

*E301869,1 HBG 07/04/2002 check the setting of generating project & Defult template [Begin]

*E301869,1 AMH Consider diffrance types [Start]
*lcGenProj = gfGetMemVar('M_POGENPRJ')  
*IF lcGenProj $ 'AI'
*  lcDefTemp  = gfGetMemVar('M_PODEFTMP')
*ENDIF  '
*B606940,1 MAN Declare the variables and move the var assignment after the gfSetup to ensure
*B606940,1     restore the parameter value (to handle multi program switching) (Start)
STORE " " TO lcGenProj,lcDefTemp  
STORE " " TO lc_PMPrjDt,lc_PrjHist,lc_PrjAudt,lc_PMPrjRl,lc_Parser 
*DO CASE
*  CASE lcAType $ 'PRN'
*    lcGenProj = gfGetMemVar('M_POGENPRJ')
*    IF lcGenProj $ 'AI'
*      lcDefTemp  = gfGetMemVar('M_PODEFTMP')
*    ENDIF  
*  CASE lcAType $ 'DA'
*    lcGenProj = gfGetMemVar('M_MFGENPRJ')
*    IF lcGenProj $ 'AI'
*      lcDefTemp  = gfGetMemVar('M_MFDEFTMP')
*    ENDIF  
*  OTHERWISE
*    lcGenProj = 'X'
*ENDCASE
*E301869,1 AMH [End]


*IF lcGenProj $ 'AI'
*  STORE " " TO lc_PMPrjDt,lc_PrjHist,lc_PrjAudt,lc_PMPrjRl,lc_Parser 
*ENDIF  
*E301869,1 [End]
*B606940,1 (End)

*-- Arrays to hold setups :-
*--B603849,1 RAMY Add this linr to initialize the arrays before the gfSetup Function [start]
*--Initialize the Terms array, and the divisions array.
DIMENSION  laDiv[1] ,laTerms[1] ,laShpVia[1] ,laPurCode[1]

*B604103,1 KHM 02/28/2001 (Begin) Initializing lcCodInfo whithout the
*B604103,1                check of lcAType.
*DIMENSION laCodInfo [IIF(lcAType='D',5,4),10]
DIMENSION laCodInfo [5,10]
*B604103,1 KHM 02/28/2001 (End)

*--B603849,1 RAMY [end]

STORE ' ' TO laDiv,laTerms,laShpVia,laPurCode,laCodInfo,lcPfrnsmbl,lcDfrnsmbl,lcBaseSmbl,lcCurCod,lcCrtCSH

*C200080,1 AMM (start) Define some variables
*C200080,1 AMM Array to get the style code structure 
DIMENSION laMajSeg[1,1]
*C200080,1 AMM Variable to get the position of color segment inside the style code structure
lnColStru = 0
*C200080,1 AMM lcDyeOpr: Variable to get the Dyeing operation in it.
*C200080,1 AMM laDyeCol: Array to get available colors from codes file to select the dye color
*E301291,1 AMM add declarations of lcT_BomVar (temporary file holds adornment costs)
STORE SPACE(0) TO lcDyeOpr,laDyeCol,lcT_BomVar
*C200080,1 AMM end
*E301291,1 AMM Variable holds the button title
lcConfig   = "\<Configure"
*E301291,1 AMM end
STORE .F. TO llDyelot,llAvgCost,llUsePacks,llWareHous,llGenOrNum,llLinkToGl,llApIstall,llImpCost,llMulCurr,llEditExRt,llMultiWare,llCostPrv
STORE .F. TO llBrowse,llUpdated,llZoom,llQtyPSize,llEdtLnCst,llConCst,llMultiLot,llIgnorAll,laOpFile,llCallShow

*C200162,1 MHM initialize variables[start]

*B605105,1 AMH Check if the type of lcAtype is 'C' [Start]
*IF lcAtype = "N"
IF TYPE('lcAtype') = 'C' .AND. lcAtype = "N"
*B605105,1 AMH [End]

  STORE '' TO lcSize1 , lcSize2 , lcSize3 , lcSize4 , lcSize5 , lcSize6 , lcSize7 , lcSize8 ,lcSrcLct , lcStyLoc
  DIMENSION  laAvalQty[9] ,laPlanqty [9] ,laBalnQty[9],laSource[1],laTarget[1],laOldBalan[9]
  STORE 0 TO laAvalQty, laPlanqty , laBalnQty, laOldBalan
  STORE '' TO laSource, laTarget 
  STORE .F. TO llRetrn, llCalFrmOp
ENDIF  
*C200162,1 MHM initialize variables[End]

*C102225,1 MHM 06/05/2001  Initiailize variables for GMA[start]
STORE 0 TO m.Ninpack,m.Nmpack ,m.Nmpdim ,m.NmPDepth,m.NmPcube,m.NmpWeight,m.lnmpcube,m.cHTSNo
*C102225,1 MHM 06/05/2001 [End]

*E301077,8 MAB Rise some flags to open files (in Prg not in module object) [Begin
STORE .F. TO llOpnBom,llOpnBomLn,llOpnCurr,llOpnKtBom
*E301077,8 MAB Rise some flags to open files (in Prg not in module object) [End..

*E300935,1 llDylt Yes : .T. if current style is dyelot yes.
*E301291,1 AMM add declaration of llBomVarnt
STORE .F. TO llDyltYes,llBomVarnt
*E300935,1

STORE .T. TO llAlowNew,llAddLine,llCUpdate,llRecalTot,llShow,llNoShow
STORE 1   TO lnActFolder,lnOrdTrans,lnGrade,lnShpLoc,lnLShpLoc
STORE 0   TO lnMarker,lnLines,lnLastKey,lnstylewid,lnMjrWid,lnSelWhr,laCopySav
STORE ''  TO lcStyHdr,lcStyPict,lcHldTab,lcHldBtb,lcHldEsc,lcHldAtB,lcHldAtP,m.Scale,m.LineNo
*ahmed Move these line to be after gfsetup()
*E301289,1 WAB - Get Setting Variable ( Display seeling Price & gross Margin )
*E301289,1 WAB - IF lcAType = 'P' ( Purchase Order )
*E301289,1 WAB - initialize variables IF lcAType = 'P' ( Purchase Order )
*E301289,1 WAB - START
*------- llDispPric  -> hold setting variable to display selling price & gross margin
*------- m.nSelPrice -> Hold Selling Price 
*------- m.GrosMrgn  -> Hold Gross Margin
*------- lnOldPrice  -> Hold Old Value from Selling Price
*------- lnOldMrgn   -> Hold Old Value from Gross Margin
*------- lnPriceLvl  -> Hold Price level no ( popoup field )
*------- lcDispFld   -> Hold ( enable / disable )
*------- laPriceLvl  -> array holding level element ( level A , B , C )
*------- lnPoCtSel   -> Hold Total selling Price (selprice * qty ) for alll polines
*------- lnUnitSel   -> Hold avreage selling price for PO.
*------- lnuntMrgn1  -> hold Estimated Gross  Margin
*------- lnuntMrgn2  -> hold Landed  Gross  Margin
*------- lnuntMrgn3  -> hold Actual Gross  Margin
*------- llCallFrm   -> .T. if we call the profitability screen from po
*------- lcDispPric  -> for holding (DISABLE/ENABLE) for 'Po profitability' in option Bar()
*------- llApIstall  -> Variable hold (.T.|.F.) if AP modules instaled.
*------- llMAInstld  -> Variable hold (.T.|.F.) if MA modules instaled. 
*------- llSOInstld  -> Variable hold (.T.|.F.) if SO modules instaled.
*------- llStyMark   -> Variable hold (.T.|.F.) if Style Markup.
*------- lcAProgm    -> Hold dye order program to work with the uncomlete session program.
*------- lcDyeOpr    -> Hold  the dye operation.
*------- llBomVarnt  -> Use Variant Cost Sheet.


*B603699,1 WMA [Begin]
llFileStru  = gfOpenFile(gcDataDir+'ICISTRU','Segno','SH')
llStruFound = SEEK('U1','ICISTRU')
IF USED('ICISTRU') AND llFileStru
  USE IN ICISTRU
ENDIF
IF ! llStruFound
  *--Item structure not found, Cannot Proceed.
  =gfModalGen('QRM42080B42001','DIALOG','Item structure not found')
  RETURN
ENDIF
*B603699,1 WMA [End]

*llDispPric  = .F.
*llStyMark   = .F.
*IF lcAType = 'P' 
  *llDispPric = gfGetMemVar('M_PoDspPrc')  
  *llStyMark  = gfGetMemVar('M_stymark')   ='T'
*ENDIF
*IF lcAType = 'P'  .AND. llDispPric
  *STORE 0 TO m.nSelPrice,m.nGrosMrgn,lnOldPrice,lnOldMrgn,lnPoCtSel,lnUnitSel,;
             lnuntMrgn1,lnuntMrgn2,lnuntMrgn3,lnPriceLvl
  *STORE SPACE(1) TO lcPriceLvl,lcDispFld
  *llCallFrm = .T.
  *lcDispPric = "DISABLE"
  *DECLARE laPriceLvl[4]
  *laPriceLvl[1] = 'Select Price Level'
  *laPriceLvl[2] = 'Level A'
  *laPriceLvl[3] = 'Level B'
  *laPriceLvl[4] = 'Level C'
*ENDIF
*E301289,1 WAB - END

*- initialize variable for button status ( disable/enable)
*E500272,1 WAB - START
lcCostShSt = "DISABLE"
*E500272,1 WAB - END


*--Initialize Curreny variables(CURR).
*--Currency methods for price and duty main and units.
STORE ''  TO lcPMethod,lcDMethod,lcPUnMeth,lcDUnMeth
STORE 1   TO lnCurrUnt1,lnCurrUnt2

*--Check  AP ,MA and SO modules instaled.
llApIstall = (OCCURS('AP',gcComp_mdl)<>0)
llMAInstld = (OCCURS('MA',gcComp_mdl)<>0)
llSOInstld = (OCCURS('SO',gcComp_mdl)<>0)

*--Initialize the Folders needed variables.
lnFolderCEnd  = 102.50            && Folder Column end.
lnFolderREnd  =   2.00            && Folder Row end.
lnNoFld       =   3               && Number of folders.
lcwfoldchng   = '=lfActFolder()'  && function to control shows after change the folder.
lcfoldpush    = 'pbFolders'       && push button name for the next folder.
laDefProc[7]  = .F.               && Disable the control panel delete proc.(lpDelScr)
laDefProc[9]  = .F.               && Disable the control panel save proc.  (lpSavScr)
laDefProc[10] = .F.               && Disable the control panel close proc. (lpClsScr)

*E301243,1 Define new variable in the 3rd Folder [Begin.]
*-- adding this variable to 3rd Folder variables to be used as Flag
*-- to indicate whether the new Added Push Button is pressed or not by
*-- default , its value is False (not pressed)
llByUnit = .F.
*E301243,1 Define new variable in the 3rd Folder [End.]

*--Uncomplte session variables(UNCSS).
DECLARE laVariables[6]
*C200080,1 AMM Assign name to the dye order program to work with the uncomlete session program
*lcAProgm   = IIF(lcAType='R','RETPO',IIF(lcAType='C','CONTR',IIF(lcAType='N','POINTRC','POSTY')))
*E301291,1 AMM (start) Add the case of adornment order
*lcAProgm   = IIF(lcAType='D','DYEPO',IIF(lcAType='R','RETPO',IIF(lcAType='C','CONTR',IIF(lcAType='N','POINTRC','POSTY'))))
*ahmed Move these line to be after gfsetup()
*lcAProgm   = IIF(lcAType='A','ADORPO',IIF(lcAType='D','DYEPO',IIF(lcAType='R','RETPO',IIF(lcAType='C','CONTR',IIF(lcAType='N','POINTRC','POSTY')))))
*C200080,1 AMM end
*lcUnCmPgId = PADR('STYLE_PO'+lcAType,10)
*ahmed end
lnSessNo   = gnProgCopy
lcSession  = ' '
laVariables= ' '
llContinue = .F.
llGoAndChk = .T.
lcSvdPO    = ' '
*-- Variable to tell if are we going to check the existance of
*-- any incomplete session in the screen's setup or not.
llChkUnCom = .T.
*--Uncomplete session variables.
laVariables[1] = 'ldCompDef'
laVariables[2] = 'lnSelWhr'
laVariables[3] = 'laScrMode[2]'
laVariables[4] = 'laScrMode[3]'
laVariables[5] = 'laScrMode[4]'
laVariables[6] = 'lcSvdPO'

lcCodeType= 'A'
ldCompDef = {}

*C102357,1 AMH If Customer J&L create temp file to hold all Thread colors [Start]
STORE '' TO lcTmpThClr , lcTmpColor, lcTmpThClr, lcTmpColor
STORE 0 TO lnStylngth
llOpnThClr = .F.
*C102357,1 AMH [End]
  
*C200208,1 AMH Define variable to open LBLINST file for customer STU [Start]
PRIVATE llOpnLblIn,lcTmpLblIn
llOpnLblIn = .F.
STORE ' ' TO lcTmpLblIn
*C200208,1 AMH [End]

*B605105,1 AMH Define the variable for Cancel/UnCancel button as Bmp needed
*B605105,1     before calling gfsetup function [Start]
lcCancel  = gcBmpHome + "trash.bmp"
lcUncan   = gcBmpHome + "untrash.bmp"
lcPromp   = lcCancel
lcDelMesag= 'Cancel'
lcMenProm  = PROPER(lcDelMesag)
*B605105,1 AMH [End]

*--Global function setup to restore information for multi session usage.
IF !gfSetup()
  RETURN
ENDIF
*B606940,1 MAN Define the variables after gfsetup has restored the parameter values (Start)
DO CASE
  CASE lcAType $ 'PRN'
    lcGenProj = gfGetMemVar('M_POGENPRJ')
    IF lcGenProj $ 'AI'
      lcDefTemp  = gfGetMemVar('M_PODEFTMP')
    ENDIF  
  CASE lcAType $ 'DA'
    lcGenProj = gfGetMemVar('M_MFGENPRJ')
    IF lcGenProj $ 'AI'
      lcDefTemp  = gfGetMemVar('M_MFDEFTMP')
    ENDIF  
  OTHERWISE
    lcGenProj = 'X'
ENDCASE
*B606940,1 (End)
*ahmed Moved here to be after gfSetup()
llDispPric  = .F.
llStyMark   = .F.
IF lcAType = 'P' 
  llDispPric = gfGetMemVar('M_PoDspPrc')  
  *B603699,1 WMA [Begin]
  IF EMPTY(llDispPric)
    llDispPric = .F.
  ENDIF
  *B603699,1 WMA [Begin]
  llStyMark  = gfGetMemVar('M_stymark')   ='T'
ENDIF
IF lcAType = 'P'  .AND. llDispPric
  STORE 0 TO m.nSelPrice,m.nGrosMrgn,lnOldPrice,lnOldMrgn,lnPoCtSel,lnUnitSel,;
             lnuntMrgn1,lnuntMrgn2,lnuntMrgn3,lnPriceLvl
  STORE SPACE(1) TO lcPriceLvl,lcDispFld
  llCallFrm = .T.
  lcDispPric = "DISABLE"
  DECLARE laPriceLvl[4]
  laPriceLvl[1] = 'Select Price Level'
  laPriceLvl[2] = 'Level A'
  laPriceLvl[3] = 'Level B'
  laPriceLvl[4] = 'Level C'
ENDIF
lcAProgm   = IIF(lcAType='A','ADORPO',IIF(lcAType='D','DYEPO',IIF(lcAType='R','RETPO',IIF(lcAType='C','CONTR',IIF(lcAType='N','POINTRC','POSTY')))))
lcUnCmPgId = PADR('STYLE_PO'+lcAType,10)
*ahmed end

*B802469,1 WAB  Open shipment header file for get E.A.T date
*B802469,1 WAB - START
=gfOpenFile(gcDataDir+'Shpmthdr','Shpmthdr','SH')
SET ORDER TO TAG Shpmthdr
*B802469,1 WAB - END

*E302035,4 ABD - Open Needed File. [Begin]
lnScaleLen = 1
lnScaleLen = gfGetMemVar('M_EXTWIDTH')     && Extended size Scale ID Length.
llOpnSclHd = .F.
IF gfGetMemVar('M_USEEXSSC',gcAct_Comp)
  llOpnSclHd = gfOpenFile(gcDataDir+'SCALEHD','EXTSCALE','SH')
ENDIF
**E302035,4 ABD - [End]


*B605932,1 AMH Open custom table for KRA10 [Start]
IF ASCAN(laEvntTrig , PADR('OPENFILE',10)) <> 0
  =gfDoTriger('POSTYLE',PADR('OPENFILE',10))
ENDIF
*B605932,1 AMH [End]

*E500272,1 WAB -  Open Ctckt bill of material file
*E500272,1 WAB - START
IF lcAtype $ 'PD' 
  =gfOpenFile(gcDataDir+'CtktBom','CtktBom','SH')
ENDIF
*E500272,1 WAB - END

lcTrn_Ttl = 'Packs Lines'
*--Program different types.
DO CASE
  CASE lcAType  = 'R'  
    lcPOType = 'Ret. P/O'
  CASE lcAType  = 'C'  
    lcPOType = 'Contract'
  CASE lcAType  $ 'PN'  
    lcPOType = 'P/O'
  *C200080,1 AMM Dye order object title
  CASE lcAType  $ 'D'  
    lcPOType = 'Dye P/O'
  *C200080,1 AMM end
  *E301291,1 AMM Add the case of adornment order
  CASE lcAType = 'A'
    lcPOType = 'Ador P/O'
  *E301291,1 AMM end
ENDCASE
lcFile_ttl = lcPOType+'s' 


*E301077,08 MAB Use gfGetMemVar once per all variables [Begin]
DIMENSION laSetups[23,2]
laSetups[1,1]  = 'M_LImpCost'      && Used detail costing for imprted styles flag.
laSetups[2,1]  = 'M_WareHouse'     && Use multi locations.
laSetups[3,1]  = 'M_SYSTYPE'       && Variable holds the system type setup.
laSetups[4,1]  = 'M_Dyelot'        && Use Dyelots.
laSetups[5,1]  = 'M_Cost_Meth'     && Use Style costing method ("M_Cost_Met" In setup file)
laSetups[6,1]  = 'M_pack'          && Use style packs / SKU.
laSetups[7,1]  = 'M_GenStOrN'      && Enter style PO # manually.
laSetups[8,1]  = 'M_Link_GL'       && If Link to G/L.
laSetups[9,1]  = 'M_cCostImp'      && Receive PO By.
*C200080,1 AMM Get manufactoring cost elements in dye order case
*laSetups[10,1] = 'M_cISLbl1'
*laSetups[11,1] = 'M_cISLbl2'
*laSetups[12,1] = 'M_cISLbl3'
*laSetups[13,1] = 'M_cISLbl4'
*laSetups[14,1] = 'M_cISLbl5'
*laSetups[15,1] = 'M_cIType1'
*laSetups[16,1] = 'M_cIType2'
*laSetups[17,1] = 'M_cIType3'
*laSetups[18,1] = 'M_cIType4'
*laSetups[19,1] = 'M_cIType5'

laSetups[10,1] = IIF(lcAType='D','M_cMSLbl1','M_cISLbl1')
laSetups[11,1] = IIF(lcAType='D','M_cMSLbl2','M_cISLbl2')
laSetups[12,1] = IIF(lcAType='D','M_cMSLbl3','M_cISLbl3')
laSetups[13,1] = IIF(lcAType='D','M_cMSLbl4','M_cISLbl4')
laSetups[14,1] = IIF(lcAType='D','M_cMSLbl5','M_cISLbl5')
laSetups[15,1] = IIF(lcAType='D','M_cMType1','M_cIType1')
laSetups[16,1] = IIF(lcAType='D','M_cMType2','M_cIType2')
laSetups[17,1] = IIF(lcAType='D','M_cMType3','M_cIType3')
laSetups[18,1] = IIF(lcAType='D','M_cMType4','M_cIType4')
laSetups[19,1] = IIF(lcAType='D','M_cMType5','M_cIType5')
*C200080,1 AMM end

laSetups[20,1] = 'llMulCurr'    && Get base currecy.
laSetups[21,1] = 'llEditExRa'   && Change exch. rates.
laSetups[22,1] = 'M_CRTCSTSH'   && Create PO Cost Sheet
laSetups[23,1] = 'M_DROPWARE'   && Drop ship location.
=gfGetMemVar(@laSetups,gcAct_Comp)
*E301077,08 MAB Use gfGetMemVar once per all variables [End..]

*C200080,1 AMM Get the dye operation, don't continue if not entered
IF lcAType = 'D'
  lcDyeOpr = gfGetMemvar('M_DYEOPR')
  IF EMPTY(lcDyeOpr)
    *C200080,1 AMM Message : 'Dyeing operation not found, Please choose operation for dyeing through the company setups program.'
    =gfModalGen('TRM38173B00000','DIALOG')
    glQuitting = .T.
    RETURN
  ENDIF
ENDIF
*C200080,1 AMM end
*B301291,1 AMM If the system not setup to use variant cost sheet, return
IF lcAType='A' .AND. !gfGetMemvar('M_BOMVAR')
  *-- Message "System hasn't been setup to use variant cost sheet, Cannot proceed."
  =gfModalGen('TRM00353B00000','DIALOG')
  glQuitting = .T.
  RETURN
ENDIF
*B301291,1 AMM end

*E301077,08 MAB Assign Variables from lasetups array [Begin]

*--Used detail costing for imprted styles flag.

*llImpCost = gfGetMemVar('M_LImpCost')
llImpCost = laSetups[1,2]
*B603699,1 WMA [Begin]
IF EMPTY(llImpCost)
  llImpCost = .F.
ENDIF
*B603699,1 WMA [End]
*--Use multi locations.
*llWareHous = gfGetMemVar('M_WareHouse')='Y'
llWareHous = (UPPER(ALLTRIM(laSetups[2,2])) = 'Y')

*E301077,08 MAB Assign Variables from lasetups array [End  ]

*--Cannot use the inter Location P/o if no multi location is used.
*E301291,1 AMM Consider adornment order case
*IF lcAType = 'N' AND !llWareHous
IF lcAType $ 'NA' AND !llWareHous
*E301291,1 AMM end
  *--The system has not been setup to use multiple locations. Cannot proceed.
  =gfModalGen('TRM42054B42001','DIALOG')
  glQuitting = .T.
  RETURN
ENDIF

*E300889,1 Initialize variable holds the system type setup.
*E300889,1 Check a point of sale warehouse if setup.

*E301077,08 MAB Assign Variables from lasetups array [Begin]
*lcSysType = gfGetMemVar('M_SYSTYPE')
lcSysType = laSetups[3,2]
*E301077,08 MAB Assign Variables from lasetups array [End  ]

*E301484,1 Check Id new communication module installed
*llCMInstld= (OCCURS('CM',gcComp_mdl)<>0)    
llCMInstld = (OCCURS('NC',gcComp_mdl)<>0)
*E301484,1 (End)

llPosSet  = lcAType = 'P' AND lcSysType = 'P'

llPosSetN = lcAType = 'N' AND lcSysType = 'P'

lcSiteLoc = ""
IF llPosSet OR llPosSetN
  SELECT WAREHOUS
  IF llCMInstld
    LOCATE FOR cSiteId = gcCurSite
  ELSE
    GO TOP
  ENDIF
  lcSiteLoc = WAREHOUS.cWareCode
ENDIF


*--Control pannel additional elements.
*E301176,1 HDM 03/22/1999[Start] Prevent programs from displaying notepad icon
*                           as it's now controlled globally
*DECLARE laPanelObj[IIF(llImpCost AND lcAType='P',3,2),3]

*E500272,1 WAB - To add the "Cost Sheet" button in the control pannel.
*E500272,1 WAB - START
*DECLARE laPanelObj[IIF(llImpCost AND lcAType='P',2,1),3]

DECLARE laPanelObj[IIF(llImpCost AND lcAType='P',3,IIF(lcAtype $ 'PD',2,1)),3]

*E500272,1 WAB - END

STORE '' TO laPanelObj
*--Object link button.
laPanelObj[1,1] = 'pbObjlnk'
laPanelObj[1,2] = gcBmpHome+'RELATE.BMP'
laPanelObj[1,3] = [VALID lfvObjLnk() MESSAGE 'Object link' DISABLE ]

*-- "P/O NotePad" button in the control pannel.
*laPanelObj[2,1] = 'pbPONote'
*laPanelObj[2,2] = gcBmpHome+'NOTES2.BMP'
*laPanelObj[2,3] = [VALID lfvNote() MESSAGE lcPOType+' Notes' DISABLE]

IF llImpCost AND lcAType='P'
  *-- "Generate Lines" button in the control pannel.
  laPanelObj[2,1] = "pbCTGnLns"
  laPanelObj[2,2] = gcBmpHome + "GENRATE.BMP"
  laPanelObj[2,3] = [VALID lfvPOGnLns() MESSAGE 'Generate lines based on components availability' DISABLE]
ENDIF
*E301176,1 HDM 03/22/1999[End]

*E500272,1 WAB - add the "Cost Sheet" button in the control pannel.
*E500272,1 WAB - START
IF lcAtype $ 'PD'
  laPanelObj[IIF(llImpCost AND lcAType='P',3,2),1] = "pbCostSh"
  laPanelObj[IIF(llImpCost AND lcAType='P',3,2),2] = gcBmpHome + "COMM.BMP"

  *B604950,1 KHM 09/30/2001 (Begin) Changing the message and removing 
  *B604950,1                the Disable action.
  *laPanelObj[IIF(llImpCost AND lcAType='P',3,2),3] = [VALID lfvCostSh() MESSAGE 'Cutting Ticket Cost Sheet' DISABLE]
  laPanelObj[IIF(llImpCost AND lcAType='P',3,2),3] = [VALID lfvCostSh() MESSAGE 'Purchase Order Cost Sheet']
  *B604950,1 KHM 09/30/2001 (End)
  
ENDIF
*E500272,1 WAB - END

*--variable to Conrol the disabling or enabling of the Generate Lines button.
lcLnShMod = 'DISABLE'

*--Define the variable for Cancel/UnCancel button as Bmp needed and
*--definision in the menu insted of delete of menu bar.

*B605105,1 AMH Comment the next lines [Start]
*lcCancel  = gcBmpHome + "trash.bmp"
*lcUncan   = gcBmpHome + "untrash.bmp"
*lcPromp   = lcCancel
*lcDelMesag= 'Cancel'
*lcMenProm  = PROPER(lcDelMesag)
*B605105,1 AMH [End]

DEFINE BAR 10 OF P03PU03 PROMPT (lcMenProm) SKIP FOR .T.
SHOW GET pbDlt,1 PROMPT lcPromp 
lcBrTtl1 = SUBSTR(gcBaseWind,2,1)+'_Original Detail'
*B605768,1 KHM 04/01/2002 (Begin) Fix the spelling mistake.
*lcBrTtl2 = SUBSTR(gcBaseWind,2,1)+'_Recieved Lines'
 lcBrTtl2 = SUBSTR(gcBaseWind,2,1)+'_Received Lines'
*B605768,1 KHM 04/01/2002 (End)

IF !WEXIST(gcBaseWind)

  *--Needed Browse fields.
  *E301291,1 AMM Display Source loc. instead of vendor in case of adrnment order (Type A)
  *B603269,1 Start, added condition in amount display.
  *lcBrFields = "PO        :R :H=lcPOType+'#':8,"+;
  	           "Status    :R :H='S':2,"+;
               "Vendor    :R :H=IIF(lcAType$'NA','Source Loc.','Vendor'):11,"+;
               "lcVnName = IIF(lcAType$'NA',WAREHOUS.cDesc,ApVendor.cVenComp) :R :H='Name':18,"+;
	           "Complete  :R :H='Complete':8,"+;
  	           "nStyOrder :R :H='Tot.Qty.':7,"+;
               "POTotal   :R :H='Amount':10,"+;
               "Receive   :R :H='Receive':7,"+;
               "Open      :R :H='Open':7"
  lcBrFields = "PO        :R :H=lcPOType+'#':8,"+;
  	           "Status    :R :H='S':2,"+;
               "Vendor    :R :H=IIF(lcAType$'NA','Source Loc.','Vendor'):11,"+;
               "lcVnName = IIF(lcAType$'NA',WAREHOUS.cDesc,ApVendor.cVenComp) :R :H='Name':18,"+;
	           "Complete  :R :H='Complete':8,"+;
  	           "nStyOrder :R :H='Tot.Qty.':7,"+;
               IIF(llCostPrv,"POTotal   :R :H='Amount':10,","")+;
               "Receive   :R :H='Receive':7,"+;
               "Open      :R :H='Open':7"
  *B603269,1 End.

  *--To know which laData is the field, Use the following expresion.
  *--No = (5*(row-1)) + colum
  ***********=*----1-----*----2-----*----3-----*----4-----*----5-----****
  *C200080,1 AMM add the field COLOR
  *C200098,1 RAMY add the field COST
  lcScFields ='Po        ,Vendor    ,Status    ,cDivision ,Entered   ,'+;
              'Complete  ,Available ,Origin    ,cFob      ,cTermCode ,'+;
              'ShipVia   ,QuotaCat  ,cLcNo     ,LCExpire  ,Contact   ,'+;
              'Phone     ,Insurance ,ShpName   ,cWareCode ,Link_Code ,'+;
              'cOutAddr1 ,cOutAddr2 ,cOutAddr3 ,cOutAddr4 ,cOutAddr5 ,'+;
              'cPriceCur ,cDutyCur  ,nPriceRat ,nDutyRat  ,PoTotal   ,'+;
              'nICost1   ,nICost2   ,nICost3   ,nICost4   ,nICost5   ,'+;
              'nStyOrder ,Receive   ,Damage    ,Cancel    ,Open      ,'+;
              'nAct_Cost1,nAct_Cost2,nAct_Cost3,nAct_Cost4,nAct_Cost5,'+;
              'nLan_Cost1,nLan_Cost2,nLan_Cost3,nLan_Cost4,nLan_Cost5,'+;
              'nFCost1   ,nFCost2   ,nFCost3   ,nFCost4   ,nFCost5   ,'+;
              'nFLanCost1,nFLanCost2,nFLanCost3,nFLanCost4,nFLanCost5,'+;
              'nFActCost1,nFActCost2,nFActCost3,nFActCost4,nFActCost5,'+;
              'cMultiLot ,cPurCode  ,Buyer,Account,Store  ,cPONo,'     +;
              'Color     ,Cost'
  ***********************************************************************

  *--Initialize laData array.
  SELECT POSHDR 
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK

  *--Define Setup variables.

  *E301077,08 MAB Assign Variables from lasetups array [Begin]
*  llDyelot   = gfGetMemVar('M_Dyelot')   ='Y'
  llDyelot   = (UPPER(ALLTRIM(laSetups[4,2])) = 'Y')

*  llAvgCost  = gfGetMemVar('M_Cost_Meth')='A'
  llAvgCost  = (UPPER(ALLTRIM(laSetups[5,2]))='A')

*  llUsePacks = gfGetMemVar('M_pack')     ='Y'
  llUsePacks = (UPPER(ALLTRIM(laSetups[6,2])) ='Y')

*  llGenOrNum = gfGetMemVar('M_GenStOrN') ='Y'
  *B603699,1 WMA [Begin]
  IF EMPTY(laSetups[7,2])
    laSetups[7,2] = 'N'
  ENDIF
  *B603699,1 WMA [End]
  llGenOrNum = (UPPER(ALLTRIM(laSetups[7,2])) ='Y')

*  llLinkToGl = gfGetMemVar('M_Link_GL')  ='Y'
  llLinkToGl = (UPPER(ALLTRIM(laSetups[8,2])) ='Y')

*  lcCostImp  = gfGetMemVar('M_cCostImp')
  *B603699,1 WMA [Begin]
  IF EMPTY(laSetups[9,2])
    laSetups[9,2] = 'P'
  ENDIF
  *B603699,1 WMA [End]
  lcCostImp  = laSetups[9,2]

*  lcISlbl1   = gfGetMemVar('M_cISLbl1')
  *E301291,1 AMM change title in case of adornment order
  *lcISlbl1   = laSetups[10,2]
  lcISlbl1   = IIF(lcAType='A','Finish Goods',laSetups[10,2])
  *E301291,1 AMM end

*  lcISlbl2   = gfGetMemVar('M_cISLbl2')
  *E301291,1 AMM change cost title in case of adornment order
  *lcISlbl2   = laSetups[11,2]
  lcISlbl2   = IIF(lcAType='A','Ador.',laSetups[11,2])
  *E301291,1 AMM end

*  lcISlbl3   = gfGetMemVar('M_cISLbl3')
  lcISlbl3   = laSetups[12,2]

*  lcISlbl4   = gfGetMemVar('M_cISLbl4')
  lcISlbl4   = laSetups[13,2]

*  lcISlbl5   = gfGetMemVar('M_cISLbl5')
  lcISlbl5   = laSetups[14,2]

*  lcIType1   = gfGetMemVar('M_cIType1')
  lcIType1   = laSetups[15,2]

*  lcIType2   = gfGetMemVar('M_cIType2')
  lcIType2   = laSetups[16,2]

*  lcIType3   = gfGetMemVar('M_cIType3')
  lcIType3   = laSetups[17,2]

*  lcIType4   = gfGetMemVar('M_cIType4')
  lcIType4   = laSetups[18,2]

*  lcIType5   = gfGetMemVar('M_cIType5')      
  lcIType5   = laSetups[19,2]

*  llMulCurr  = gfGetMemVar('llMulCurr')
  
  *E301291,1 AMM Use base currecy in case of adornment and dye order
  *llMulCurr  = laSetups[20,2]
  llMulCurr  = IIF(lcAType$'ADN',.F.,laSetups[20,2])
  *E301291,1 AMM

*  llEditExRt = gfGetMemVar('llEditExRa')
  llEditExRt = laSetups[21,2]
*E301077,08 MAB Assign Variables from lasetups array [End  ]

  lcStyHdr   = gfItemMask('HI')
  lcStyPict  = gfItemMask('PI')
  lnstylewid = LEN(lcStyPict)
  lnMjrWid   = LEN(gfItemMask('PM'))
  llCostPrv  = gfUserPriv('IC','ICSTYLE','COSTING')

  *C200080,1 AMM Get the style code structure and the color segment position as well
  = gfItemMask(@laMajSeg)
  *B603530,1 SSH 26/03/00  *** Begin *** Fix bug of subscript out of bounds in case of
  *B603530,1 SSH 26/03/00  Style have no Non-Major.
  *FOR lnI = 1 TO ALEN(laMajSeg)
  FOR lnI = 1 TO ALEN(laMajSeg,1)
  *B603530,1 SSH 26/03/00  *** End ***
    IF laMajSeg[lnI,1] = 'C'
      lnColStru = lnI
      EXIT
    ENDIF
  ENDFOR
  *C200080,1 AMM end

  *E301077,08 MAB Assign Variables from lasetups array [Begin]
  *lcCrtCSH   = gfGetMemVar('M_CRTCSTSH')
  lcCrtCSH   = laSetups[22,2]
  *B603699,1 WMA [Begin]
  IF EMPTY(lcCrtCSH)
    lcCrtCSH = 'L'
  ENDIF
  *B603699,1 WMA [End]
  *E301077,08 MAB Assign Variables from lasetups array [End  ]
  
  *--Initialize the currency defulted from company.
  STORE SET('CURRENCY',1) TO lcPfrnSmbl,lcDfrnSmbl,lcBaseSmbl


  *--Program screen windows temprorary names.
  lcWinCh0    = gfTempName()
  lcWinCh1    = gfTempName()
  lcWinCh2    = gfTempName()
  lcWinC21    = gfTempName()
  lcWinC22    = gfTempName()
  lcWinC23    = gfTempName()
  lcWinC24    = gfTempName()
  lcWinCh3    = gfTempName()            

  lcfolder    = gfTempName()        && Folder Window Name
  lcfoldprnt  = gcBaseWind          && Window parent name for the folder
  lnActFolder = 1                   && Active folder

  *--Folder array definition.
  lafoldwinds[1,1] = 'Header'
  lafoldwinds[1,2] = lcWinCh1
  lafoldwinds[2,1] = 'Details'
  lafoldwinds[2,2] = lcWinCh2
  lafoldwinds[3,1] = 'Summary'
  lafoldwinds[3,2] = lcWinCh3

  *--Program temprorary files names.
  lcPOHdr   = gfTempName()
  lcPOLine  = gfTempName()
  lcPOTran  = gfTempName()
  lcContrt  = gfTempName()
  lcTmpCtPk = gfTempName()
  *B602884,1 Read append from P/O temp file.
  IF lcAType='R'
    lcTmpPOLn = gfTempName()
  ENDIF
  *B602884,1 End.

  *E301869,1 HBG 07/04/2002 Declare the temp files nedded in generating project[Begin]  
  IF lcGenProj $ 'AI'
    lc_PMPrjDt   = gfTempName()
    lc_PrjHist   = gfTempName()
    lc_PrjAudt   = gfTempName()
    lc_PMPrjRl   = gfTempName()
    lc_Parser    = gfTempName()
  ENDIF  
  *E301869,1 [End]

  *C102357,1 AMH If Customer J&L create temp file to hold all Thread colors [Start]
  IF lcAType = 'A' AND ASCAN(laEvntTrig , PADR('DOSCRN',10)) <> 0
    llOpnThClr = .F.
    lnStylngth = LEN(lcStyHdr)
    lcTmpThClr = gfTempName()
    lcTmpColor = gfTempName()
  ENDIF  
  *C102357,1 AMH [End]

  *C200208,1 AMH Open LBLINST file for customer STU [Start]
  IF lcAType = 'P' AND ASCAN(laEvntTrig , PADR('SAVLBLIN',10)) <> 0
    IF !USED('LBLINST')
      llOpnLblIn = gfOpenFile(gcDataDir+'LBLINST',gcDataDir+'LBLINST','SH')
      SCATTER MEMO MEMVAR
      lcTmpLblIn = gfTempName()
    ENDIF
  ENDIF
  *C200208,1 AMH [End]

  *--Get a unique session number, to be used to update the uncomplete 
  *--session file(UNCSS).
  lcSession  = gfsequence('cSession')

  llNoShow = .F.
  
  *-Initialize the status array.
  STORE ' ' TO laStatus

  *-Initialize the style grade array.
  laStyGrd[1,1]   = 'First Quality'
  laStyGrd[1,2]   = '1'
  laStyGrd[2,1]   = 'Second Quality'
  laStyGrd[2,2]   = '2'
  laStyGrd[3,1]   = 'Damaged'
  laStyGrd[3,2]   = '3'

  *** Initialize the "Codes Information" array.
  *C200080,1 AMM add the color code popup in case of dye order
  *DIMENSION laCodInfo [4,10]
  DIMENSION laCodInfo [IIF(lcAType='D',5,4),10]
  *C200080,1 AMM end
  laCodInfo = SPACE(0)

  *** Fill the "Codes Information" array.
  *--For Divisions.
  laCodInfo[1,01] = "CDIVISION"
  laCodInfo[1,02] = "laDiv"
  laCodInfo[1,03] = "lnDiv"
  laCodInfo[1,04] = ""
  laCodInfo[1,05] = .F.            
  laCodInfo[1,06] = .F.            
  laCodInfo[1,07] = "POSHDR"     
  laCodInfo[1,08] = "POSHDR"     
  laCodInfo[1,09] = "lcAType+laData[1]"     
  laCodInfo[1,10] = "cDivision"     
  *--For Terms code.
  laCodInfo[2,01] = "CTERMCODE" 
  laCodInfo[2,02] = "laTerms"      
  laCodInfo[2,03] = "lnTerms"      
  laCodInfo[2,04] = ""            	
  laCodInfo[2,05] = .F.            	
  laCodInfo[2,06] = .F.            	
  laCodInfo[2,07] = "POSHDR"     
  laCodInfo[2,08] = "POSHDR"     
  laCodInfo[2,09] = "lcAType+laData[1]"     
  laCodInfo[2,10] = "cTermCode"     
  *--For Ship Via.
  laCodInfo[3,01] = "SHIPVIA"
  laCodInfo[3,02] = "laShpVia"
  laCodInfo[3,03] = "lnShpVia"
  laCodInfo[3,04] = ""
  laCodInfo[3,05] = .F.    
  laCodInfo[3,06] = .F.     
  laCodInfo[3,07] = "POSHDR"     
  laCodInfo[3,08] = "POSHDR"     
  laCodInfo[3,09] = "lcAType+laData[1]"     
  laCodInfo[3,10] = "ShipVia"     
  *--For Purchase code.
  laCodInfo[4,01] = "CPURCODE"
  laCodInfo[4,02] = "laPurCode"
  laCodInfo[4,03] = "lnPurCode"
  laCodInfo[4,04] = ""
  laCodInfo[4,05] = .F.    
  laCodInfo[4,06] = .F.     
  laCodInfo[4,07] = "POSHDR"     
  laCodInfo[4,08] = "POSHDR"     
  laCodInfo[4,09] = "lcAType+laData[1]"     
  laCodInfo[4,10] = "CPURCODE"
  *C200080,1 AMM Color codes POPUP
  IF lcAType = 'D'
    laCodInfo[5,01] = "COLOR"
    laCodInfo[5,02] = "laDyeCol"
    laCodInfo[5,03] = "lnDyeCol"
    laCodInfo[5,04] = ""
    laCodInfo[5,05] = .T.    
    laCodInfo[5,06] = .F.     
    laCodInfo[5,07] = "POSHDR"     
    laCodInfo[5,08] = "POSHDR"     
    laCodInfo[5,09] = "lcAType+laData[1]"     
    laCodInfo[5,10] = "Color"
    DIMENSION laDyeCol [1]
    STORE SPACE(0) TO laDyeCol
  ENDIF
  *C200080,1 AMM end
  *--Initialize the Terms array, and the divisions array.
  DIMENSION  laDiv[1] ,laTerms[1] ,laShpVia[1] ,laPurCode[1]
  STORE "" TO laDiv,laTerms,laShpVia,laPurCode

  *--Get default warehouse in single location setup.
  IF !llWareHous
    GO TOP IN WAREHOUS
    lcDefLoc = WAREHOUS.cWareCode
  ENDIF
  *--Restore the Drop Ship Location.
  *lcDropLoc = IIF(!llWareHous,lcDefLoc,gfGetMemVar('M_DROPWARE'))
  lcDropLoc = IIF(!llWareHous,lcDefLoc,laSetups[23,2])
  lcLocPict = 'XXXXXX'
  *E301291,1 AMM (START)
  IF lcAType = 'A'
    llBomVarnt = gfGetMemVar('M_BOMVAR' , gcAct_Comp)
    lcT_BomVar = gfTempName()
  ENDIF
  *E301291,1 AMM end

  *--Show only LCs for current vendor.
  IF lcAType ='P'
    *E301077,8 MAB Open LC file here [Begin
    =gfOpenFile(gcDataDir+'LC','LC','SH')
    *E301077,8 MAB Open LC file here [End..
    SELECT LC
    SET FILTER TO Vendor=laData[2]
  ENDIF

ELSE
  llNoShow = IIF(llCallShow,.F.,.T.)
  *--To fix problem of losing object value location due to customer on top.
  IF llNoShow 
    KEYBOARD "{PGDN}"
  *B605105,1 AMH recollecting data after return from calling PO cost sheet screen [Start]
  ELSE

    *B605356,1 AMH Don't call the lfGetInfo function to privent the Doublicates of 
    *B605356,1     PO order lines [Start]
    *= lfGetInfo(.F.)
    SELECT POSHDR 
    SCATTER FIELDS &lcScFields MEMO TO laData
    *B605356,1 AMH [End]
    
    llCallShow = .F.
  *B605105,1 AMH [End]
  ENDIF
ENDIF


*--Set file order tags filters and relations needed in this program.
SELECT POSHDR
SET ORDER TO TAG POSHDR
*--Set relation to see name in browses.
*E301291,1 AMM Consider case of adornment order
*IF lcAType = 'N'
IF lcAType $ 'NA'
*E301291,1 AMM end
  SET RELATION TO PADR(Vendor,6) INTO WareHous
ELSE
  SET RELATION TO Vendor INTO APVendor
ENDIF
*-- Set filter to on type of PO.
SET FILTER TO cStyType = lcAType

SELECT CUTPICK
SET ORDER TO TAG Cutpick
SELECT STYLE
SET ORDER TO TAG STYLE

*--Open needed files array.
*E301077,8 open files only if not opened.
IF !laOpFile[1]
  laOpFile[1]= gfOpenFile(gcDataDir+'Customer','Customer','SH')
ENDIF

IF llSOInstld AND !laOpFile[5] AND SEEK('2','CUTPICK')
  laOpFile[5] = gfOpenFile(gcDataDir+'ORDLINE','ORDLINE','SH')
ENDIF
*E301077,8 End.

SELECT POSHDR
*--Hold the keys value insted of push key.
=lfHoldKey('S')

*--Activate Options pad.
=lfActPad()


*--Create all the uncomplete session temprory files 
*--and seek on the passed P/o record.
IF llExCalled
  llNoShow = .F.
  laScrMode=.F.
  laScrMode[2]=.T.
  SELECT POSHDR
  =SEEK(lcAType+lcPoCode)
  SCATTER FIELDS &lcScFields TO laData
ENDIF

*E500272,1 WAB - can call lpshow after calling po cost sheet
*E500272,1 WAB - START
*B803163,1 ABD - Fix bug that the system cancels the style po after openning any screen
*B803163,1 ABD - Becouse reinitialize this global variable with .F. 
*B803163,1 ABD - To call lpshow after calling Po cost sheet screen .[Begin]
*llNoShow = .F.
*B803163,1 ABD [End]
*E500272,1 WAB - END

*--Call the program main screen.
SELECT (lcBaseFile)
DO (gcScrDir+"POSTYLE.SPX")


*--Restore the keys value that saved in lfHoldKey('S') function.
=lfHoldKey('R')

*--Set back the Delete option in menu insted of Cancel.
DEFINE BAR 10 OF P03PU03 PROMPT '\<Delete' SKIP FOR .T.

*--Close Relations opened in this program.
IF lcAType ='P'
  SELECT LC
  SET FILTER TO
ENDIF
SELECT POSHDR
SET RELATION TO
SET FILTER TO
SELECT (lcPOline)
SET RELATION TO

*--Close and Erase the Temp files with Normal exit.
IF glQuitting
  *--Close files opened in this program.
  IF USED('Customer') AND laOpFile[1]
    *USE IN Customer
    =gfCloseFile('Customer')
  ENDIF
  IF USED('OrdHdr') AND laOpFile[4]
    *USE IN OrdHdr
    =gfCloseFile('OrdHdr')
  ENDIF
  IF USED('OrdLine') AND laOpFile[5]
    *USE IN OrdLine
    =gfCloseFile('OrdLine')
  ENDIF
  IF USED('sPck_Lin') AND laOpFile[2]
    *USE IN sPck_Lin
    =gfCloseFile('sPck_Lin')
  ENDIF
  IF USED('sPck_hdr') AND laOpFile[3]
    *USE IN sPck_hdr
    =gfCloseFile('sPck_hdr')
  ENDIF
  IF USED('Fabric') AND laOpFile[6]
    *USE IN Fabric
    =gfCloseFile('Fabric')
  ENDIF
  *E301077,8 Close the following files when exit.
  IF USED('FabDye') AND laOpFile[7]
    *USE IN FabDye
    =gfCloseFile('FabDye')
  ENDIF
  IF USED('MFGOprHd') AND laOpFile[8]
    *USE IN MFGOprHd
    =gfCloseFile('MFGOprHd')
  ENDIF
  IF USED('NOTEPAD') AND laOpFile[9]
    *USE IN NOTEPAD
    =gfCloseFile('NOTEPAD')
  ENDIF
  IF USED('LCJOURNL') AND laOpFile[10]
    *USE IN LCJOURNL
    =gfCloseFile('LCJOURNL')
  ENDIF
  *E301077,8 End.

**E302035,4 ABD - Close opend file. [Begin]
IF llOpnSclHd
  =gfCloseFile('SCALEHD')
ENDIF
**E302035,4 ABD - [End]


  USE IN (lcPOHdr)
  ERASE (gcWorkDir+lcPOHdr+'.DBF')
  ERASE (gcWorkDir+lcPOHdr+'.CDX')
  *B602179,1 AMM Erase temporary files when quitting the program start
  ERASE (gcWorkDir+lcPOHdr+'.FPT')
  *B602179,1 AMM end

  *B602619 Add IF ENDIF block to avoid erase file not found [Begin]
  IF USED(lcPOLine)
    USE IN (lcPOLine)
    ERASE (gcWorkDir+lcPOLine+'.DBF')
    ERASE (gcWorkDir+lcPOLine+'.CDX')
  ENDIF  
  *B602619 Add IF ENDIF block to avoid erase file not found [End  ]

  USE IN (lcPOTran)
  ERASE (gcWorkDir+lcPOTran+'.DBF')  
  ERASE (gcWorkDir+lcPOTran+'.CDX')  
  IF USED(lcContrt)
    USE IN (lcContrt)
  ENDIF
  IF FILE(gcWorkDir+lcContrt+'.DBF')
    ERASE (gcWorkDir+lcContrt+'.DBF')
    *B602179,1 AMM Erase temporary files when quitting the program start
    ERASE (gcWorkDir+lcContrt+'.CDX')
    *B602179,1 AMM end
  ENDIF
  IF USED(lcTmpCtPk)
    USE IN (lcTmpCtPk)
  ENDIF
  IF FILE(gcWorkDir+lcTmpCtPk+'.DBF')
    ERASE (gcWorkDir+lcTmpCtPk+'.DBF')
  ENDIF
  *B602884,1 Close temp file. 
  IF lcAType='R'
    IF USED(lcTmpPOLn)
      USE IN (lcTmpPOLn)
    ENDIF
    IF FILE(gcWorkDir+lcTmpPOLn+'.DBF')
      ERASE (gcWorkDir+lcTmpPOLn+'.DBF')
      ERASE (gcWorkDir+lcTmpPOLn+'.CDX')
    ENDIF
  ENDIF
  *B602884,1 End.
  *E301291,1 AMM Erase temporary files
  IF lcAType = 'A'
    IF USED(lcT_BomVar)
      USE IN (lcT_BomVar)
    ENDIF
    ERASE (gcWorkDir + lcT_BomVar + ".DBF")
    ERASE (gcWorkDir + lcT_BomVar + ".CDX")
    ERASE (gcWorkDir + lcT_BomVar + ".FPT")
    ENDIF
  *E301291,1 AMM end
  *C038291,3  TMI [Start] Close a temp files when close the postyle screen "for Vijay"
  IF lcAType = 'P' AND ASCAN(laEvntTrig , PADR('OPINSTPD',10)) <> 0
    =gfDoTriger('POSTY',PADR('CLOSETMP',10))
  ENDIF     
  *C038291,3  TMI [End  ] 
ENDIF

*C102357,1 AMH If Customer J&L close Master & Temp thread file. [Start]
IF lcAType = 'A' AND ASCAN(laEvntTrig , PADR('DOSCRN',10)) <> 0 
  IF llOpnThClr
    USE IN ThreadCl
  ENDIF
 
  IF USED(lcTmpThClr)
    USE IN (lcTmpThClr)
  ENDIF
  ERASE (gcWorkDir + lcTmpThClr + ".DBF")
  ERASE (gcWorkDir + lcTmpThClr + ".CDX")
  ERASE (gcWorkDir + lcTmpThClr + ".FPT")
  
  IF USED(lcTmpColor)
    USE IN (lcTmpColor)
  ENDIF
  ERASE (gcWorkDir + lcTmpColor + ".DBF")
  ERASE (gcWorkDir + lcTmpColor + ".CDX")
  ERASE (gcWorkDir + lcTmpColor + ".FPT")  
ENDIF  
*C102357,1 AMH [End]

*C200208,1 AMH Close LBLINST file for customer STU [Start]
IF lcAType = 'P' AND ASCAN(laEvntTrig , PADR('SAVLBLIN',10)) <> 0
  IF llOpnLblIn
    USE IN LBLINST
  ENDIF
ENDIF  
*C200208,1 AMH [End]

*--Release program browses and Option pad also
RELEASE WINDOW (lcBrTtl1)
RELEASE WINDOW (lcTrn_Ttl)
RELEASE WINDOW (lcBrTtl2)
RELEASE PAD _Option OF _MSYSMENU

RETURN
*-End[POStyle]...

*:*************************************************************

*##G#-#-# General functions.

*:*************************************************************
*! Name      : lpshow
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Show folder when Screen Mode Changes.
*:*************************************************************
*! Calls     : 
*!     Procedure :  DO (gcScrDir+"POSTYDF.SPX")
*!     Functions : lfChkUnComS(),lfClearInfo(),lfchngfolder(),
*!                 lfGetInfo(),lfCrUnSess(),lfGetStatus()
*!                 lfvBrow1(),lfvBrow2(),gfwCodePop()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lpShow()
*!*************************************************************
*
FUNCTION lpShow

*C038291,1  TMI [Start] Close a temp file if mode is changed for Vijay
IF lcAType = 'P' AND ASCAN(laEvntTrig , PADR('OPINSTPD',10)) <> 0
  =gfDoTriger('POSTY',PADR('CLOSETMP',10))
ENDIF     
*C038291,1  TMI [End  ] 

SELECT (lcBaseFile)
DO CASE
  *-- S E L E C T   M O D E -----------------------------------------------
  CASE laScrMode[1]  && Select mode. 

    *--Check uncomplete session (UNCSS).
    IF llGoAndChk AND lfChkUnComS()
      RETURN
    ENDIF

    *--Clear information.
    =lfClearInfo()

    lnlastfold = lnActFolder
    lnActFolder= 1
    =lfchngfolder(lnActFolder)

    lcCodeType= "N"    
    lcDelMesag= 'Cancel'
    lcMenProm  = PROPER(lcDelMesag)
    SHOW GET lcPOType  ENABLE 
    SHOW GET ibPo      ENABLE 
    SHOW GET ibVend    ENABLE
    SHOW GET laData[2] ENABLE
    SHOW GET llMultiWare
    SHOW GET pbDlt,1 PROMPT lcPromp DISABLE
    SHOW GET pbKey       DISABLE
    SHOW GET pbObjlnk    DISABLE
    SHOW GET ibFolder(2) DISABLE    
    SHOW GET ibFolder(3) DISABLE    
    *C200080,1 AMM Get the color codes including ALL option
    IF lcAType = 'D'
      =gfwCodePop( @laCodInfo, "COLOR" ,"A" )
    ENDIF
    *C200080,1 AMM end
    
    *E500272,1 WAB - show po cost sheet button disable
    *E500272,1 WAB - START
    
    IF lcAtype $ 'PD'
      lcCostShSt = "DISABLE"
      SHOW GET pbCostSh   &lcCostShSt
    ENDIF
    
    *E500272,1 WAB - END
 
    *E301289,1 WAB - DISABLE  'Po profitability' in option Bar() in select mode 
    lcDispPric = "DISABLE"
    *E301289,1 WAB - END

  *-- V I E W   M O D E -------------------------------------------------
  CASE laScrMode[2]  && View mode.
    *--P/O was changed due to navigation or other pb.
    IF &lcPOhdr..PO <> laData[1] OR llExCalled
      *IF ! llExCalled
        *--Clear old p/o information.
        =lfClearInfo()

        *B606366,1 KHM 08/12/2002 (Begin) Added
        *--Get new P/o information.
        = lfGetInfo(.F.)
        *B606366,1 KHM 08/12/2002 (End)


        IF lnActFolder = 3
          =lfRecalc()
        ENDIF
      *ENDIF

      *B606366,1 KHM 08/12/2002 (Begin) Moved up before calling lfRecalc function.      
      *--Get new P/o information.
      *= lfGetInfo(.F.)
      *B606366,1 KHM 08/12/2002 (End)
      
    ENDIF

    lcVenStat ='DISABLE' 
    SHOW GET ibFolder(2) ENABLE
    SHOW GET ibFolder(3) ENABLE
    SHOW GET ibVend    DISABLE
    SHOW GET laData[2] DISABLE
    SHOW GET pbObjlnk  ENABLE

    SELECT (lcPOline)
    lnlastfold = lnActFolder
    =lfchngfolder(lnactfolder)

    lcPromp   = lcCancel
    lcDelMesag= 'Cancel'
    SHOW GET pbDlt ENABLE
    IF laData[3] = 'X'
      lcPromp    = lcUnCan
      lcDelMesag = 'Uncancel'
      SHOW GET pbEdt DISABLE
    ELSE
      SHOW GET pbEdt ENABLE
    ENDIF
    SHOW GET pbDlt,1 PROMPT lcPromp
    lcMenProm  = PROPER(lcDelMesag)
    SHOW GET llMultiLot
    SHOW GET llMultiWare
    lcCodeType="T"
    *C200080,1 AMM Get the color code
    IF lcAType = 'D'
      =gfwCodePop( @laCodInfo, "COLOR" ,"V,"+POSHDR.COLOR )
    ENDIF
    *C200080,1 AMM end
    
    *E301291,1 AMM Get adornment costs from BOMVAR file
    IF lcAType = 'A'

      *C102357,1 AMH If Customer J&L create temp file to hold all Thread colors. [Start]
      *SELECT * , "S" AS cStatus , RECNO() AS nRecNo ;
      *  FROM (gcDataDir + "BomVar") ;
      * WHERE cIdType+cCost_Id+STR(LineNo,6) = "AD" + laData[1] ;
      *  INTO DBF (gcWorkDir + lcT_BomVar)
      *INDEX ON cIdType+cCost_Id+STR(LineNo,6) TAG (lcT_BomVar)

      IF ASCAN(laEvntTrig , PADR('DOSCRN',10)) <> 0
         *-- If customer is J & L , don't collect data , it will be collected in SOJlInf prg.
        DO lpCreatBom IN (gcAppHome+'SOJLINF.FXP') 
        DO lpCollData WITH "AD" , laData[1] , POSLN.LineNo , lcT_BomVar , '' IN (gcAppHome+'SOJLINF.FXP')
      ELSE                    && Collect data for BomVar file in a standard way.
        SELECT * , "S" AS cStatus , RECNO() AS nRecNo ;
          FROM (gcDataDir + "BomVar") ;
         WHERE cIdType+cCost_Id+STR(LineNo,6) = "AD" + laData[1] ;
          INTO DBF (gcWorkDir + lcT_BomVar)
        INDEX ON cIdType+cCost_Id+STR(LineNo,6) TAG (lcT_BomVar)
      ENDIF  
      *C102357,1 AMH [End]

    ENDIF
    *E301291,1 AMM end    

    *E500272,1 WAB - show po cost sheet button enable /disable
    *E500272,1 WAB - START

    IF lcAtype $ 'PD' 
      lcCostShSt   = "ENABLE"
      IF laData[3] = "X"      && IF Po is Canceled 
        lcCostShSt = IIF(!SEEK(ALLTRIM(lcAType)+laData[1] , "CtktBom") .OR. lcAType = 'A', "DISABLE", "ENABLE")
      ENDIF
      SHOW GET pbCostSh  &lcCostShSt 
    ENDIF

    *E500272,1 WAB - END

    *E301289,1 WAB - ENABLE   'Po profitability' in option Bar() in view mode
    lcDispPric = "ENABLE"
    *E301289,1 WAB - END

    *C200208,1 AMH Get data from LBLINST file [Start]
    IF lcAType = 'P' AND ASCAN(laEvntTrig , PADR('GETLBLIN',10)) <> 0
      *B604840,1 AMH Fix bug of Labeling Instruction option not avail in Gen PO frm SO [Start]
      *=gfDoTriger('POSTY',PADR('GETLBLIN',10))
      IF llExCalled
        =gfDoTriger('POSTYLE',PADR('GETLBLIN',10))
      ELSE
        =gfDoTriger('POSTY',PADR('GETLBLIN',10))
      ENDIF
      *B604840,1 AMH [End]
    ENDIF
    *C200208,1 AMH [End]
  
    *B605289,1  SSH [Begin] Prevent the user from edit or cancel PO if it is closed
    IF laData[3]="S"
      SHOW GET  pbEdt DISABLE
      SHOW GET  pbDlt DISABLE
      *B605289,1 AMH Prenet the menu items for edit also [Start]
      laCtrStat[7]  =  "DISABLE"
      *B605289,1 AMH [End]
    ENDIF
    *B605289,1  SSH [End]
  ***--- E D I T   M O D E ---***  
  CASE laScrMode[3]  && Edit mode. 
    IF laData[3] = 'C'
      SHOW GETS WINDOW (lcWinC22) DISABLE ONLY
    ENDIF
    STORE .T. TO llCUpdate,llUpdated
    STORE .F. TO llCSave,llShow

    =lfchngfolder(lnactfolder)

    lcVenStat ='DISABLE' 
    SHOW GET lcPOType  DISABLE
    SHOW GET pbObjlnk  ENABLE
    IF laData[3] $ 'CX'
      SHOW GET ibVend    DISABLE
      SHOW GET laData[2] DISABLE
    ENDIF
    lnLines   = POSHDR.LastLine

    *--Create uncmsess record.
    IF ! llContinue 
      =lfCrUnSess(' ','O') 
    ENDIF
    lcCodeType = "T"
    *E301291,1 AMM Don't allow user to edit costs in case of adornment orders
    IF lcAType = 'A'
      SHOW GET m.nCost1 DISABLE
    ENDIF
    *E301291,1 AMM end

    *E500272,1 WAB - show po cost sheet button disable
    *E500272,1 WAB - START

    IF lcAtype $ 'PD'
      lcCostShSt = "DISABLE"
      SHOW GET pbCostSh   &lcCostShSt 
    ENDIF
    
    *E500272,1 WAB - END

    *E301289,1 WAB - ENABLE   'Po profitability' in option Bar() in edit mode
    lcDispPric = "ENABLE"
    *E301289,1 WAB - END

  *-- A D D   M O D E -----------------------------------------------------
  CASE laScrMode[4]  && Add mode. 
    STORE .T. TO llCUpdate,llUpdated
    STORE .F. TO llCSave,llShow
    
    IF ! llContinue
      *--Clear information.
      =lfClearInfo()

      *--Call P/O Defaults screen.
      IF EMPTY(ldCompDef)
        ldCompDef = gdSysDate+90
        lnSelWhr  = 0
        lcSctTtle = lcPOType+' defaults'
        DO (gcScrDir+"POSTYDF.SPX")
        llMultiWare=(lnSelWhr=1)
        IF !llWareHous AND llMultiWare
          lnShpLoc = 2
        ENDIF 
        =lfvMult(.T.)
      ENDIF 
      
      *E300889,1 Initialize POS Scource location.
      IF llPosSetN
        lcOldValue = " "
        laData[2] = lcSiteLoc
        =lfvSrcWhr()
      ENDIF
      
      lnlastfold = lnActFolder
      lnActFolder= 1
      =lfchngfolder(lnActFolder)

      laData[5] = gdSysDate
      laData[6] = ldCompDef
      laData[7] = ldCompDef
      lnLines   = 0

      lcVenStat ='ENABLE' 
      SHOW GET ibFolder(2) ENABLE
      SHOW GET ibFolder(3) ENABLE
      SHOW GET lcPOType    DISABLE
      SHOW GET pbObjlnk    DISABLE
      SHOW GET laData[70]  DISABLE
      SHOW GET ibVend      ENABLE
      SHOW GET laData[2]   ENABLE
      SHOW GET laData[5] 
      SHOW GET laData[6] 
      SHOW GET laData[7] 
      SHOW GET llMultiWare
      *C200080,1 AMM Go to color POPUP (the latter object to the PO object)
      *IF _CUROBJ<>OBJNUM(laData[2])
      IF lcAType='D'
        _CUROBJ = OBJNUM(LNDYECOL)
      ELSE
      *C200080,1 AMM end
        IF _CUROBJ<>OBJNUM(laData[2])
          _CUROBJ=OBJNUM(laData[2])
        ENDIF 
      *C200080,1 AMM END IF
      ENDIF
      *C200080,1 AMM end
    ENDIF
    *E301291,1 AMM Get adornment costs from BOMVAR file
    IF lcAType = 'A'

      *C102357,1 AMH If Customer J&L create temp file to hold all Thread colors. [Start]
      *SELECT * , "S" AS cStatus , RECNO() AS nRecNo ;
      *  FROM (gcDataDir + "BomVar") ;
      * WHERE cIdType+cCost_Id+STR(LineNo,6) = "AD" + laData[1] ;
      *  INTO DBF (gcWorkDir + lcT_BomVar)
      *INDEX ON cIdType+cCost_Id+STR(LineNo,6) TAG (lcT_BomVar)
      IF ASCAN(laEvntTrig , PADR('DOSCRN',10)) <> 0
         *-- If customer is J & L , don't collect data , it will be collected in SOJlInf prg.
        DO lpCreatBom IN (gcAppHome+'SOJLINF.FXP') 
        DO lpCollData WITH "AD" , laData[1] , POSLN.LineNo , lcT_BomVar , '' IN (gcAppHome+'SOJLINF.FXP')
      ELSE                    && Collect data for BomVar file in a standard way.
        SELECT * , "S" AS cStatus , RECNO() AS nRecNo ;
          FROM (gcDataDir + "BomVar") ;
         WHERE cIdType+cCost_Id+STR(LineNo,6) = "AD" + laData[1] ;
          INTO DBF (gcWorkDir + lcT_BomVar)
        INDEX ON cIdType+cCost_Id+STR(LineNo,6) TAG (lcT_BomVar)
      ENDIF  
      *C102357,1 AMH [End]

      SHOW GET m.nCost1 DISABLE
    ENDIF
    *E301291,1 AMM end
   
    lcCodeType = IIF(llContinue,"T","D")

    *--Create uncmsess record.
    IF ! llContinue
      =lfCrUnSess(' ','O')
    ENDIF
    
    *E500272,1 WAB - show po cost sheet button disable
    *E500272,1 WAB - START
    
    IF lcAtype $ 'PD'
      lcCostShSt = "DISABLE"
      SHOW GET pbCostSh   &lcCostShSt 
    ENDIF
     
    *E500272,1 WAB - START
    
    *E301289,1 WAB - ENABLE   'Po profitability' in option Bar() in add mode
    lcDispPric = "ENABLE"
    *E301289,1 WAB - END

    *C200162,1 MHM [start] Reinitialize the variables.
    IF lcAType = 'N'
      IF ASCAN(laEvntTrig , PADR('SHOWSCR',10)) <> 0
        =gfDoTriger('POINTRC',PADR('SHOWSCR',10))
      ENDIF     
    ENDIF
    *C200162,1 MHM [End]
    
    *C200208,1 AMH Get data from LBLINST file [Start]
    IF lcAType = 'P' AND ASCAN(laEvntTrig , PADR('GETLBLIN',10)) <> 0
      *B604840,1 AMH Fix bug of Labeling Instruction option not avail in Gen PO frm SO [Start]
      *=gfDoTriger('POSTY',PADR('GETLBLIN',10))
      IF llExCalled
        =gfDoTriger('POSTYLE',PADR('GETLBLIN',10))
      ELSE
        =gfDoTriger('POSTY',PADR('GETLBLIN',10))
      ENDIF
      *B604840,1 AMH [End]
    ENDIF
    *C200208,1 AMH [End]

ENDCASE

*--Show the shipto location and customer fields.
lcDispMode = IIF(!llMultiWare AND (laScrMode[3] OR laScrMode[4]),'ENABLE','DISABLE')
IF lnShpLoc=1
  SHOW GET laData[69] DISABLE
  SHOW GET laData[19] &lcDispMode
ELSE
  SHOW GET laData[19] DISABLE
  SHOW GET laData[69] &lcDispMode
ENDIF
IF llMultiWare
  SHOW GET lnShpLoc DISABLE
  SHOW GET pbShpTo  DISABLE
ENDIF

*--Define Status array.
=lfGetStatus()


*--Re Browse lines.
= lfvBrow1()    
= lfvBrow2()    

*--Define delete menu bar.
*B605289,1 AMH Prenet the menu items for cancel if po is colsed [Start]
*DEFINE BAR 10 OF P03PU03 PROMPT (lcMenProm) SKIP FOR !laScrMode[2]
DEFINE BAR 10 OF P03PU03 PROMPT (lcMenProm) SKIP FOR !laScrMode[2] .OR. (laData[3]='S')
*B605289,1 AMH [End]

*--Uncsessvariable(UNCSS).
llGoAndChk = .T.

lcNoteMod = IIF(laScrMode[1] OR laScrMode[4],'DISABLE','ENABLE')
SHOW GET pbPONote  &lcNoteMod
IF llImpCost AND lcAType='P'
  lcLnShMod = IIF((laScrMode[3] AND laData[3]="H") OR ;
                  (laScrMode[4] AND !lfCkEOF(lcPoLine)),'ENABLE','DISABLE')
  SHOW GET pbCTGnLns &lcLnShMod
ENDIF

*--Default the codes popups.
IF llContinue
  STORE lcPOhdr TO laCodInfo[1,07],laCodInfo[1,08],laCodInfo[2,07],laCodInfo[2,08],laCodInfo[3,07],laCodInfo[3,08],laCodInfo[4,07],laCodInfo[4,08]
ENDIF
= gfwCodePop ( @laCodInfo, "CDIVISION" ,lcCodeType )
laData[4] =laDiv[lnDiv,2]
= gfwCodePop ( @laCodInfo, "CTERMCODE" ,lcCodeType )
laData[10]=laTerms[lnTerms,2]    
= gfwCodePop ( @laCodInfo, "SHIPVIA"   ,lcCodeType )
laData[11]=laShpVia[lnShpVia,2]
= gfwCodePop ( @laCodInfo, "CPURCODE"  ,lcCodeType )
laData[67]=laPurCode[lnPurCode,2]


IF llContinue
 STORE "POSHDR" TO laCodInfo[1,07],laCodInfo[1,08],laCodInfo[2,07],laCodInfo[2,08],laCodInfo[3,07],laCodInfo[3,08],laCodInfo[4,07],laCodInfo[4,08]
ENDIF

*C200162,1 MHM 02/27/2001 (Begin) Displaying the customized screen
*C200162,1                when pressin CTRL+A
IF lcAtype = "N" AND laScrMode[4] AND ASCAN(laEvntTrig , PADR('ADDOPTN',10)) <> 0
  ON KEY LABEL Ctrl+A DO lpDistSty IN ELSMAIN
ENDIF
*C200162,1 MHM 02/27/2001 (End)
  
SELECT (lcPOHdr)
GATHER FROM laData FIELDS &lcScFields MEMO    
IF !llContinue 
  REPLACE nSteps WITH 0
ENDIF

SHOW WINDOW (lcBrTtl1) REFRESH
SELECT (lcBaseFile)
RETURN

*--End Of lpShow.
*:*************************************************************
*! Name      : lfActFolder
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Activate folder when Change.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfCkEOF(),lfGetLine(),lfRecalc()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfActFolder()
*!*************************************************************
*
FUNCTION lfActFolder

IF laScrMode[1]
  RETURN
ENDIF

*C200162,1 MHM 02/27/2001 (Begin) Traping the key ALT+D
IF lcAtype = "N" AND laScrMode[3] OR laScrMode[4]
  IF ASCAN(laEvntTrig , PADR('ADDOPTN',10)) <> 0
    ON KEY LABEL ALT+D
  ENDIF        
ENDIF
*C200162,1 MHM 02/27/2001 (End)

DO CASE
  *--First Folder < H E A D E R >
  CASE lnactfolder = 1
    SHOW WINDOW (lcWinCh1) TOP
    IF laScrMode[2]
      SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
    ELSE
      SHOW GETS WINDOW (lcWinCh1) ENABLE  ONLY
    ENDIF
    SHOW GETS WINDOW (lcWinC22) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh3) DISABLE ONLY

  *C200233,1 KHM 09/23/2001 (Begin) Adding the following trigger for GMA
  *C200233,1                to disable the FOB and Origin fields.
  IF ASCAN(laEvntTrig , PADR('DISAFOOR',10)) <> 0
    =gfDoTriger('POSTY',PADR('DISAFOOR',10))
  ENDIF     
  *C200233,1 KHM 09/23/2001 (End)
  
  *--Second Folder < D E T A I L >
  CASE lnactfolder = 2
    ON KEY LABEL ALT+B DO lfActBrWin    
    SHOW WINDOW (lcWinCh2) TOP

    IF laScrMode[3] OR laScrMode[4]
      *C200162,1 MHM 02/27/2001 (Begin) Setting the focus on the Qty
      *C200162,1                when the user presses ALT+D
      IF lcAtype = "N" AND ASCAN(laEvntTrig , PADR('ADDOPTN',10)) <> 0
        ON KEY LABEL ALT+D DO lfQtyFocus IN ELSMAIN
      ENDIF
      *C200162,1 MHM 02/27/2001 (End)
       
      IF lfCkEOF(lcPoLine) OR llZoom
        SHOW GETS WINDOW (lcWinC22) DISABLE ONLY  
        IF !llZoom AND laData[3]<>'C' AND lnOrdTrans=1
          SHOW GET PBNEW ENABLE
      	  _CUROBJ=OBJNUM(pbNew)
        ENDIF
      ELSE
        IF laData[3]<>'C'
          SHOW GETS WINDOW (lcWinC22) ENABLE ONLY        
        ELSE
          SHOW GETS WINDOW (lcWinC22) DISABLE ONLY          
        ENDIF
      ENDIF
    ELSE
      SHOW GETS WINDOW (lcWinC22) DISABLE ONLY    
    ENDIF
    SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh3) DISABLE ONLY
    SELECT (lcPOline)
    IF !lfCkEOF(lcPoLine)
      =lfGetLine()
    ENDIF
    SHOW GET pbKey ENABLE
 
    *E300935,1 enable / disable dyelot field [begin]
    = lfAdjDye()
    *E300935,1 enable / disable dyelot field [end]

    *B602619,1 Fix UnComplete session bug [Begin]
    SHOW WINDOW (lcBrTtl1) REFRESH
    *B602619,1 Fix UnComplete session bug [End  ]
    *E301291,1 AMM (start)
    IF laScrMode[1]                 && Select Mode
      SHOW GET pbConfig DISABLE     && Push Botton is Disabled 
    ELSE                            && View,Edit,Add Mode
      SHOW GET pbConfig ENABLE      && Push Botton is Enabled 	
    ENDIF                          
    *E301291,1 AMM end

  *--Third Folder < S U M M U R Y >
  CASE lnactfolder = 3
    IF llRecalTot
      =lfRecalc()
    ENDIF
    SHOW WINDOW (lcWinCh3) TOP
    IF laScrMode[2]
      SHOW GETS WINDOW (lcWinCh3) DISABLE ONLY
    ELSE
      SHOW GETS WINDOW (lcWinCh3) ENABLE  ONLY
    ENDIF
    SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
    SHOW GETS WINDOW (lcWinC22) DISABLE ONLY
    =lfRefresh(lcWinCh3)

    *E301243 to enable Cost by Unit PUSH BUTTON every time folder is changed [Begin.]
    IF laScrMode[1]                 && Select Mode
      SHOW GET PbSwitch DISABLE     && Push Botton is Disabled 
    ELSE                            && View,Edit,Add Mode
      SHOW GET PbSwitch ENABLE      && Push Botton is Enabled 	
    ENDIF                          
    *E301243 to enable Cost by Unit PUSH BUTTON every time folder is changed [End.]

ENDCASE
IF llImpCost AND lcAType='P' AND ( (laScrMode[3] AND laData[3]='H') OR laScrMode[4] )
  lcLnShMod = IIF(lfCkEOF(lcPoLine),'DISABLE','ENABLE')
  SHOW GET pbCTGnLns &lcLnShMod
ENDIF
SELECT (lcPOhdr)
GATHER FROM laData FIELDS &lcScFields MEMO
SELECT (lcBaseFile)

RETURN

*-- End Of lfActFolder.
*:*************************************************************
*! Name      : lfTrapKy
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Trap key.
*:*************************************************************
*
FUNCTION lfTrapKy
lcDispMode = IIF(!llMultiWare AND (laScrMode[3] OR laScrMode[4]),'ENABLE','DISABLE')

IF lnShpLoc=1
  SHOW GET laData[69] DISABLE
  SHOW GET laData[19] &lcDispMode
ELSE
  SHOW GET laData[19] DISABLE
  SHOW GET laData[69] &lcDispMode
ENDIF

IF WONTOP() = lcBrTtl1 .OR. WONTOP() = lcBrTtl2
  glFromBrow = .T.
  ON KEY LABEL TAB        DO lptab
  ON KEY LABEL backtab    DO lpshifttab
ENDIF  
*-- End Of lfTrapKy.
*!*************************************************************
*! Name      : lfReadAct
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : READ Activate function of INV700
*!*************************************************************
*
FUNCTION lfReadAct
*--To fix problem of losing object value when navigte.
*--Show the shipto location and customer fields.
lcDispMode = IIF(!llMultiWare AND (laScrMode[3] OR laScrMode[4]),'ENABLE','DISABLE')
IF lnShpLoc=1
  SHOW GET laData[69] DISABLE
  SHOW GET laData[19] &lcDispMode
ELSE
  SHOW GET laData[19] DISABLE
  SHOW GET laData[69] &lcDispMode
ENDIF

IF llMultiWare
  SHOW GET lnShpLoc DISABLE
  SHOW GET pbShpTo  DISABLE

*C200162,1 MHM 02/27/2001 
IF lcAType = 'N' AND ASCAN(laEvntTrig , PADR('ADDOPTN',10)) <> 0
  SHOW GET laData[2]  DISABLE 
  SHOW GET ibVend     DISABLE 
  SHOW GET llMultiWare DISABLE 
ENDIF

ENDIF

* -- Clear Trap.
IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF
ON KEY LABEL TAB
ON KEY LABEL BACKTAB

*--End Of lfReadAct.
*!*************************************************************
*! Name      : lfActBrWin
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : key trapping procedure for activate browse.
*!*************************************************************
*
FUNCTION lfActBrWin 
IF lnOrdTrans<>1
  HIDE WINDOW (lcWinC22)
  IF INLIST(lnLastKey, 9, 24, 13)
    IF !llZoom
      ACTIVATE WINDOW (lcBrTtl2)
    ENDIF
    ACTIVATE WINDOW (lcBrTtl1)
  ELSE
    ACTIVATE WINDOW (lcBrTtl1)
    IF !llZoom
      ACTIVATE WINDOW (lcBrTtl2)
    ENDIF
  ENDIF 
ELSE
  ACTIVATE WINDOW (lcBrTtl1)
ENDIF  
*-- End Of lfActBrWin.
*!*************************************************************
*! Name      : lpTab
*! Purpose   : Tab key trapping procedure.
*!*************************************************************
*
PROCEDURE lptab
IF WONTOP()=lcBrTtl1 
  IF llZoom
    ACTIVATE WINDOW (lcfolder)
    _CUROBJ=1
  ELSE  
    IF lnOrdTrans=1 
      ACTIVATE WINDOW (lcWinC22)
      _CUROBJ=OBJNUM(ibStyle)
    ELSE
      ACTIVATE WINDOW (lcBrTtl2)
    ENDIF  
  ENDIF  
ELSE
  ACTIVATE WINDOW (lcfolder)
  _CUROBJ=1
ENDIF  
RETURN

*-- End Of lpTab
*!*************************************************************
*! Name      : lpShiftTab
*! Purpose   : Shift Tab key trapping procedure.
*!*************************************************************
PROCEDURE lpshifttab

IF WONTOP(lcBrTtl1)
  ACTIVATE WINDOW (lcfolder)
  _CUROBJ = OBJNUM(ibFolder(2))
ELSE
 	ACTIVATE WINDOW (lcBrTtl1)
ENDIF
RETURN


*:*************************************************************
*! Name      : lfGetStatus
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Define status due to screen mode and prog type.
*:*************************************************************
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
FUNCTION lfGetStatus

*-Initialize the status array.
DO CASE
  CASE laScrMode[1]  && Select mode.  
    DIME laStatus[1,2]
    STORE ' ' TO laStatus,lcStatus

  CASE laScrMode[2]   && Veiw mode.  
    DIME laStatus[1,2]
    DO CASE
      CASE laData[3]='H'
        laStatus[1,1] = 'On Hold'
      CASE laData[3]='O'  
        laStatus[1,1] = 'Open'
      CASE laData[3]='X'
        laStatus[1,1] = 'Cancelled'
      CASE laData[3]='B'
        laStatus[1,1] = 'Bid'
      CASE laData[3]='S'
        laStatus[1,1] = 'Closed'
      CASE laData[3]='C'
        laStatus[1,1] = 'Completed'
      CASE laData[3]='A'
        laStatus[1,1] = 'Actualized'
    ENDCASE
    laStatus[1,2] = laData[3]
    lcStatus=laStatus[1,1]

  CASE laScrMode[3]  && Edit mode.  
    IF laData[3]='B'
      DECLARE laStatus[2,2]
      *E301291,1 AMM Consider case of adornment order
      *IF lcAType='P'
      IF lcAType$'PD'
      *E301291,1 AMM end
        laStatus[1,1] = 'Bid'
        laStatus[1,2] = 'B'
        laStatus[2,1] = 'On Hold'
        laStatus[2,2] = 'H'
      ELSE    &&  lcAType $ 'CRN'
        laStatus[1,1] = 'Bid'
        laStatus[1,2] = 'B'
        laStatus[2,1] = 'Open'
        laStatus[2,2] = 'O'
      ENDIF
      lcStatus  = laStatus[1,1]
    ELSE
      IF lcAType='C'
        IF laData[3]='H'
          DECLARE laStatus[2,2]      
          laStatus[1,1] = 'On Hold'
          laStatus[1,2] = 'H'
          laStatus[2,1] = 'Open'
          laStatus[2,2] = 'O'
          lcStatus=laStatus[1,1]
        ENDIF
      ENDIF    
    ENDIF

  CASE laScrMode[4]  && Add mode.  
    DECLARE laStatus[2,2]
    DO CASE
      *C200080,1 AMM Get the Dye order status as the contract status
      *CASE lcAType='P'
      CASE lcAType$'PD'
      *E301291,1 AMM end
        laStatus[1,1] = 'Bid'
        laStatus[1,2] = 'B'
        laStatus[2,1] = 'On Hold'
        laStatus[2,2] = 'H'
      CASE lcAType='C'
        laStatus[1,1] = 'On Hold'
        laStatus[1,2] = 'H'
        laStatus[2,1] = 'Open'
        laStatus[2,2] = 'O'
      *E301291,1 AMM Consider the case of adornment orders
      *CASE lcAType $ 'RN'
      CASE lcAType $ 'RNA'
      *E301291,1 AMM end
        laStatus[1,1] = 'Bid'
        laStatus[1,2] = 'B'
        laStatus[2,1] = 'Open'
        laStatus[2,2] = 'O'
    ENDCASE
    IF llContinue
      DO CASE
        CASE laData[3]='H'
          lcStatus  = 'On Hold'
        CASE laData[3]='O'  
          lcStatus  = 'Open'
        CASE laData[3]='B'
          lcStatus  = 'Bid'
      ENDCASE
    ELSE
      lcStatus  = laStatus[2,1]
      laData[3] = laStatus[2,2]
    ENDIF
ENDCASE
SHOW GET lcStatus 



*:*************************************************************
*! Name      : lfClearInfo
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Clear information.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfClearInfo()
*!*************************************************************
FUNCTION lfClearInfo

STORE ' ' TO laVAddrs,lcConPo,lcLCBank
STORE ' ' TO lcSz1,lcSz2,lcSz3,lcSz4,lcSz5,lcSz6,lcSz7,lcSz8,;
             lcStyDesc,lReference,lcVenSty,lcLastStyle

llMultiLot = .F.
llIgnorAll = .F.
llZoom     = .F.
lnGrade    = 1
laCopySav  = 0
STORE 1 TO lnShpLoc,lnLShpLoc

lcVenStat  ='ENABLE' 
*--Default currency
STORE SET('CURRENCY',1) TO lcPfrnsmbl,lcDfrnsmbl
*SET MARK OF BAR 4 OF _OPTIONPOP TO .F.

llMultiWare=(lnSelWhr=1)
IF !llWareHous AND llMultiWare
  lnShpLoc = 2
ENDIF 

*--Close contracts file.
IF USED(lcContrt)
  SELECT (lcContrt)
  USE
ENDIF

*--Empty lines file.
SELECT (lcPOline)
BLANK ALL
DELETE ALL
SCATTER MEMVAR BLANK

SELECT (lcPOTran)
lcStFlt = SET('FILTER')
SET FILTER TO 
BLANK ALL
DELETE ALL
SET FILTER TO &lcStFlt

*--Blank laData
SELECT POSHDR 
SCATTER FIELDS &lcScFields MEMO TO laData BLANK
*E300889,1 Initialize POS location.
IF llPosSet
  laData[19] = lcSiteLoc
ENDIF
RETURN


*:*************************************************************
*! Name      : lfGetInfo
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Get P/O Information.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : llNeedShow -> Need to show gets after
*!                       changing and getting P/o information.
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetInfo(.T.)
*!*************************************************************
FUNCTION lfGetInfo
PARA llNeedShow

IF !llContinue
  SELECT POSHDR 
  SCATTER FIELDS &lcScFields MEMO TO laData
ENDIF

*E300889,1 Initialize POS Scource location.
IF llPosSetN AND (laScrMode[1] OR laScrMode[4])
  lcOldValue = " "
  laData[2] = lcSiteLoc
  =lfvSrcWhr()
ENDIF

*--Goto right vendor record.
*E301291,1 AMM Consider the case of adornment order
*llVenRec=IIF(lcAType='N',SEEK(PADR(laData[2],6),'WAREHOUS'),;
                         SEEK(laData[2],'APVENDOR') )
llVenRec=IIF(lcAType$'NA',SEEK(PADR(laData[2],6),'WAREHOUS'),;
                         SEEK(laData[2],'APVENDOR') )
*E301291,1 AMM end

llMultiLot = IIF(laData[66]='M',.T.,.F.)
llMultiWare= IIF(llContinue,&lcPOHdr..lMultiWare,POSHDR.lMultiWare)
lnShpLoc   = IIF(!EMPTY(laData[69]),2,1)

=lfvMult(.T.)

*--(CURR).
IF llMulCurr

  *E301077,8 MAB Open Currency file here [Begin
  IF !llOpnCurr
    llOpnCurr = lfOpn_Rest(gcSysHome+'SYCCURR','CCURRCODE')
  ENDIF  
  *E301077,8 MAB Open Currency file here [End..

  lcPfrnsmbl = IIF(SEEK(laData[26],'SycCurr'),SycCurr.cCurrSmbl,'')
  lcDfrnsmbl = IIF(SEEK(laData[27],'SycCurr'),SycCurr.cCurrSmbl,'')
  IF !llContinue AND !laScrMode[2]
    laData[28] = gfChkRate('lnCurrUnt1',laData[26],laData[5],llEditExRt,gcAct_comp,.F.)
    laData[29] = gfChkRate('lnCurrUnt2',laData[27],laData[5],llEditExRt,gcAct_comp,.F.)
  ENDIF
ENDIF

IF !llContinue
  =SEEK(lcAType+laData[1],'POSLN')
  lcCkSty = POSLN.Style
ELSE
  GO TOP IN (lcPOline)
  lcCkSty = &lcPOline..Style
ENDIF
*--Style P/o Quality.
lnGrade = IIF(SEEK(lcCkSty,'STYLE'),INT(VAL(STYLE.cStyGrade)),1)
lnGrade = IIF(lnGrade=0,1,lnGrade)

*B602646,1 [BEGIN] Fix SEEK condition to be able to show the bank name .
*lcLCBank=IIF(lcAType='P' AND SEEK(laData[13],'LC'),LC.cLCBank,SPACE(10))
lcLCBank=IIF(lcAType='P' AND SEEK('S'+laData[13],'LC'),LC.cLCBank,SPACE(10))
*B602646,1 [END..]
laData[20] = IIF(EMPTY(laData[20]),'DEFDEF',laData[20])

*--Fill Temp line files with details.
IF !llContinue
  *--Budject ,received and canceled lines.
  SELECT POSLN
  SCAN WHILE cStyType+Po=lcAType+laData[1] FOR TranCd$'1236'
    SCATTER TO MEMVAR
    DO CASE
      CASE TranCd='1'
        SELECT (lcPOline)
      CASE TranCd $ '236'
        SELECT (lcPOTran)
    ENDCASE
    APPEND BLANK 
    GATHER FROM MEMVAR
    IF !llMultiWare
      REPLACE cWareCode WITH IIF(!EMPTY(Account),Account,cWareCode)
    ENDIF
    *--Show the account in warehouse field if line ship to customer.
    IF llMultiWare AND !EMPTY(Account)
      REPLACE cWareCode WITH Account
    ENDIF
  ENDSCAN
  SELECT POSLN
  =SEEK(lcAType+laData[1])  
  
  *--Allocated orders lines , Use TranCD='7' for this lines.
  IF llSOInstld AND SEEK('2'+laData[1],'CUTPICK')
    SELECT CUTPICK
    SCAN WHILE TranCd+CtktNo = '2'+laData[1]
      SELECT (lcPOTran)
      APPEND BLANK
      *E301291,1 AMM Get the PO line number instead of the order number .
      *REPLACE cStyType WITH lcAType,;
              Po       WITH laData[1],;
              TranCd WITH '7',;
              LineNo WITH INT(VAL(CutPick.cOrdLine)),;
              Vendor WITH CUTPICK.Order,;
              Style  WITH CUTPICK.Style
      REPLACE cStyType WITH lcAType,;
              Po       WITH laData[1],;
              TranCd WITH '7',;
              LineNo WITH INT(VAL(CutPick.cTktLineno)),;
              Vendor WITH CUTPICK.Order,;
              Style  WITH CUTPICK.Style
      *E301291,1 AMM end
      IF SEEK('O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'ORDLINE')
        SELECT ORDLINE

        *E301520,1 AME[Start]  add account, start date and complete date
        *SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laQtyar
        SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,Account,Start,Complete TO laQtyar
        *E301520,1 AME[End]

        SELECT (lcPOTran)

        *E301520,1 AME[Start]  add account, start date and complete date
        *GATHER FROM laQtyar FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty
        GATHER FROM laQtyar FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,Account,Start,Complete
        *E301520,1 AME[End]

        GATHER FROM laQtyar FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
        *E301520,1 AME[Start]  add The customer name field
        =SEEK(IIF(EMPTY(OrdLine.Store),'M'+ORDLINE.Account,'S'+ORDLINE.Account+ORDLINE.Store),'CUSTOMER')
        REPLACE cAccName  WITH CUSTOMER.STName
        *E301520,1 AME[End]
        
      ENDIF
    ENDSCAN
  ENDIF
ENDIF   && Olny if Not Continue.


*--Goto right vendor record.
*E301291,1 AMM Consider the case of adornment order
*llVenRec=IIF(lcAType='N',SEEK(PADR(laData[2],6),'WAREHOUS'),;
                         SEEK(laData[2],'APVENDOR') )
llVenRec=IIF(lcAType='NA',SEEK(PADR(laData[2],6),'WAREHOUS'),;
                         SEEK(laData[2],'APVENDOR') )
*E301291,1 AMM end
*--Define Vendor address or scorce location in Inter Location P/o.
laVAddrs=' '
*E301291,1 AMM Consider the case of adornment order
*laVAddrs[1] = IIF(lcAType = 'N',WareHous.cDesc,APVendor.cVenComp)
*laVAddrs[2] = IIF(lcAType = 'N',WareHous.cAddress1,APVendor.cAddress1)
*laVAddrs[3] = IIF(lcAType = 'N',WareHous.cAddress2,APVendor.cAddress2)
*laVAddrs[4] = IIF(lcAType = 'N',WareHous.cAddress3,APVendor.cAddress3)
*laVAddrs[5] = IIF(lcAType = 'N',WareHous.cAddress4,APVendor.cAddress4)
*laVAddrs[6] = IIF(lcAType = 'N',WareHous.cAddress5,APVendor.cAddress5)

*B803858,1 HBG 11/23/2000 Get The vendore address By using "gfGetAdr" function
*B803858,1                to display it with the correct formate [Begin]
*laVAddrs[1] = IIF(lcAType $ 'NA',WareHous.cDesc,APVendor.cVenComp)
*laVAddrs[2] = IIF(lcAType $ 'NA',WareHous.cAddress1,APVendor.cAddress1)
*laVAddrs[3] = IIF(lcAType $ 'NA',WareHous.cAddress2,APVendor.cAddress2)
*laVAddrs[4] = IIF(lcAType $ 'NA',WareHous.cAddress3,APVendor.cAddress3)
*laVAddrs[5] = IIF(lcAType $ 'NA',WareHous.cAddress4,APVendor.cAddress4)
*laVAddrs[6] = IIF(lcAType $ 'NA',WareHous.cAddress5,APVendor.cAddress5)

IF !(lcAType $ 'NA')
  laVAddrs[1] = APVendor.cVenComp
  laVAddrs[2] = gfGetAdr('APVENDOR' , '' , '' , '' , 1)
  laVAddrs[3] = gfGetAdr('APVENDOR' , '' , '' , '' , 2)
  laVAddrs[4] = gfGetAdr('APVENDOR' , '' , '' , '' , 3)
  laVAddrs[5] = gfGetAdr('APVENDOR' , '' , '' , '' , 4)
  laVAddrs[6] = gfGetAdr('APVENDOR' , '' , '' , '' , 5)
ELSE
  =SEEK(LEFT(PosHdr.Vendor,6),'WAREHOUS')
  laVAddrs[1] = WareHous.cDesc    
  laVAddrs[2] = gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
  laVAddrs[3] = gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
  laVAddrs[4] = gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
  laVAddrs[5] = gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
  laVAddrs[6] = gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
ENDIF
*B803858,1 [End]

*E301291,1 AMM end

SELECT (lcPOhdr)
GATHER FROM laData FIELDS &lcScFields MEMO

GO TOP IN (lcPOTran)
GO TOP IN (lcPOline)

IF llNeedShow
  SHOW GETS  
ENDIF
*--Both fields on the same posision in the screen.
*--Show due to selection location or customer.
IF lnShpLoc=1
  SHOW GET laData[69]
  SHOW GET laData[19]
ELSE
  SHOW GET laData[19]
  SHOW GET laData[69]
ENDIF

SELECT (lcBaseFile)
RETURN



*!*************************************************************
*! Name      : lfActPad
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfActPad()
*!*************************************************************
FUNCTION lfActPad

DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP

DEFINE POPUP _OPTIONPOP MARGIN SHADOW
DEFINE BAR 1 OF _OPTIONPOP PROMPT 'Show Quantity Summary'  SKIP FOR (lnactfolder<>2)
DEFINE BAR 2 OF _OPTIONPOP PROMPT 'Show Quantity per Size' SKIP FOR (lnactfolder<>2)
DEFINE BAR 3 OF _OPTIONPOP PROMPT 'Show Transactions'      SKIP FOR (lnactfolder<>2) OR EMPTY(laData[2]) OR lfCkEOF(lcPoLine) OR llZoom
DEFINE BAR 4 OF _OPTIONPOP PROMPT 'Zoom Lines'             SKIP FOR (lnactfolder<>2)
DEFINE BAR 5 OF _OPTIONPOP PROMPT "\-" SKIP FOR .T.
DEFINE BAR 6 OF _OPTIONPOP PROMPT 'Edit cost per line'     SKIP FOR (lnactfolder<>2) OR laScrMode[1] OR laScrMode[2] OR (!(lcAType$'RCN') AND llImpCost) OR lfCkEOF(lcPoLine) OR !llCostPrv
IF lcAtype = 'R'
  DEFINE BAR 7 OF _OPTIONPOP PROMPT 'Append lines from P/O' SKIP FOR (lnactfolder<>2) OR (lcAType<>'R') OR laScrMode[1] OR laScrMode[2] OR laData[3]='C'
ELSE
  DEFINE BAR 7 OF _OPTIONPOP PROMPT 'Append lines from Pack' SKIP FOR (lnactfolder<>2) OR !llUsePacks OR !laScrMode[4] OR llZoom OR EMPTY(laData[2]) OR EMPTY( IIF(lnShpLoc=1,laData[19],laData[69]) ) OR laData[3]='C'  
ENDIF

*C200152,4 AMH Add condiation for skip copy and paste options [Start]
*DEFINE BAR 8 OF _OPTIONPOP PROMPT 'Copy Line Quantity'  SKIP FOR (lnactfolder<>2) OR laScrMode[1] OR laScrMode[2] OR lfCkEOF(lcPoLine)
*DEFINE BAR 9 OF _OPTIONPOP PROMPT 'Paste Line Quantity'  SKIP FOR (lnactfolder<>2) OR laScrMode[1] OR laScrMode[2] OR lfCkEOF(lcPoLine) AND EMPTY(laCopySav[9])
DEFINE BAR 8 OF _OPTIONPOP PROMPT 'Copy Line Quantity'  SKIP FOR (lnactfolder<>2) OR laScrMode[1] OR laScrMode[2] OR lfCkEOF(lcPoLine) OR (ASCAN(laEvntTrig , PADR('DISBWARE',10)) <> 0)
DEFINE BAR 9 OF _OPTIONPOP PROMPT 'Paste Line Quantity'  SKIP FOR (lnactfolder<>2) OR laScrMode[1] OR laScrMode[2] OR lfCkEOF(lcPoLine) AND EMPTY(laCopySav[9]) OR (ASCAN(laEvntTrig , PADR('DISBWARE',10)) <> 0)
*C200152,4 AMH [End]

IF !llImpCost 
  DEFINE BAR 10 OF _OPTIONPOP PROMPT 'Edit cost for header'   SKIP FOR (lnactfolder<>3) OR !laScrMode[4] OR lfCkEOF(lcPoLine)
    *C200162,1 MHM call option bar[start]
    IF lcAType = 'N'
      IF ASCAN(laEvntTrig , PADR('ADDOPTN',10)) <> 0
        =gfDoTriger('POINTRC',PADR('ADDOPTN',10))
      ENDIF     
    ENDIF
    *C200162,1 MHM call option bar[End]
    
    *C102225,1 MHM 06/05/2001 Triger To call Option [start]
    IF lcAType = 'P' AND ASCAN(laEvntTrig , PADR('ADDOPTIN',10)) <> 0 
      =gfDoTriger('POSTY',PADR('ADDOPTIN',10))
    ENDIF     
   *C102225,1 MHM 06/05/2001 [End]
 
ELSE

  IF lcAType $ 'PD'
    DEFINE BAR 10 OF _OPTIONPOP PROMPT "\-" SKIP FOR .T.
    *E500272,1 WAB - To add  "Po Cost Sheet"  to  the Option Menu.
    *E500272,1 WAB - START
    IF lcAtype $ 'PD'
      DEFINE BAR 11 OF _OPTIONPOP PROMPT "Po Cost Sheet  " SKIP FOR lcCostShSt = "DISABLE"
      IF lcAtype = 'P'
        DEFINE BAR 12 OF _OPTIONPOP PROMPT "Generate Lines Based on Components Availability" SKIP FOR lcLnShMod = 'DISABLE'    
        *E301289,1 WAB - Add New Bar for  'Po profitability' 
        *E301289,1 WAB - START
        IF llDispPric 
           DEFINE BAR 13 OF _OPTIONPOP PROMPT "Po Profitability " SKIP FOR lcDispPric = "DISABLE"
        ENDIF

        *C102225,1 MHM 06/05/2001 Add option [Start]
        IF ASCAN(laEvntTrig , PADR('ADDOPTIN',10)) <> 0
          =gfDoTriger('POSTY',PADR('ADDOPTIN',10))
        ENDIF     
        *C102225,1 MHM 06/05/2001 [End]

        *E301289,1 WAB - END
      ENDIF
    ELSE
      DEFINE BAR 11 OF _OPTIONPOP PROMPT "Generate Lines Based on Components Availability" SKIP FOR lcLnShMod = 'DISABLE'  
    ENDIF
    *E500272,1 WAB - END
  ELSE
    *C200162,1 MHM call option bar[start]
    IF lcAType = 'N'
      IF ASCAN(laEvntTrig , PADR('ADDOPTN',10)) <> 0
        =gfDoTriger('POINTRC',PADR('ADDOPTN',10))
      ENDIF     
    ENDIF
    *C200162,1 MHM call option bar[End]
  
  ENDIF
ENDIF

*C102357,1 AMH Add a Custom menu Bar in Option pad for Customer J & L. [Start]
IF lcAType = 'A' AND ASCAN(laEvntTrig , PADR('JLADDBAR',10)) <> 0
  =gfDoTriger('POADPO',PADR('JLADDBAR',10))
ENDIF     
*C102357,1 AMH [End]

*C200208,1 AMH Add a Custom menu Bar in Option pad for Customer STU [Start]
IF lcAType = 'P' AND ASCAN(laEvntTrig , PADR('STUADBAR',10)) <> 0
  *B604840,1 AMH Fix bug of Labeling Instruction option not avail in Gen PO frm SO [Start]
  *=gfDoTriger('POSTY',PADR('STUADBAR',10))
  IF llExCalled
    =gfDoTriger('POSTYLE',PADR('STUADBAR',10))
  ELSE
    =gfDoTriger('POSTY',PADR('STUADBAR',10))
  ENDIF
  *B604840,1 AMH [End]
ENDIF     
*C200208,1 AMH [End]

*C200452,1 TMI [Start] Add a Custom menu Bar in Option pad for Customer David Luke
IF lcAType = 'P' AND ASCAN(laEvntTrig , PADR('POQORDPD',10)) <> 0
  =gfDoTriger('POSTY',PADR('POQORDPD',10))
ENDIF     
*C200452,1 TMI [End  ] 


*C037017,1 MHM [Start] Add a Custom menu Bar in Option pad for Customer VAN MAR
IF ASCAN(laEvntTrig , PADR('POSTYPD',10)) <> 0
  =gfDoTriger('POSTY',PADR('POSTYPD',10))
ENDIF     
*C037017,1 MHM  [End  ] 

*C038291,1  TMI [Start] Add a Custom menu Bar in Option pad for Customer "Vijay fashions Ltd"
IF lcAType = 'P' AND ASCAN(laEvntTrig , PADR('OPINSTPD',10)) <> 0
  =gfDoTriger('POSTY',PADR('OPINSTPD',10))
ENDIF     
*C038291,1  TMI [End  ] 

*C037959,1 MHM Add a Custom menu Bar in Option pad for Customer alena[Start]
IF ASCAN(laEvntTrig , PADR('POISSUPD',10)) <> 0
  =gfDoTriger('POSTY',PADR('POISSUPD',10))
ENDIF     
*C037959,1 MHM [End  ] 

SET MARK OF BAR 1 OF _OPTIONPOP TO .T.
ON SELECTION POPUP _OPTIONPOP DO lpvActBar
ON BAR 3 OF _OPTIONPOP ACTIVATE POPUP _POTRANS
DEFINE POPUP _POTRANS MARGIN SHADOW

*B803416,1 ABD Change the text in OPTION -> SHOW TRANSACTION menu option,
*B803416,1 ABD The word 'Open' to be 'Ordered'. [Begin]
*DEFINE BAR 1 OF _POTRANS PROMPT 'Open'
DEFINE BAR 1 OF _POTRANS PROMPT 'Ordered'
*B803416,1 ABD [End]

DEFINE BAR 2 OF _POTRANS PROMPT 'In Transit' SKIP FOR ! laScrMode[2] OR lcAType $ 'CR' OR laData[3]='B'
IF lcAType = 'R'
  DEFINE BAR 3 OF _POTRANS PROMPT 'Issued' SKIP FOR ! laScrMode[2] OR laData[3]='B'
ELSE
  DEFINE BAR 3 OF _POTRANS PROMPT 'Received' SKIP FOR ! laScrMode[2] OR lcAType='C' OR laData[3]='B'
ENDIF
DEFINE BAR 4 OF _POTRANS PROMPT 'Sales Orders' SKIP FOR ! laScrMode[2] OR lcAType $ 'CR' OR laData[3]='B' OR ! llSOInstld
SET MARK OF BAR 1 OF _POTRANS TO .T.
ON SELECTION POPUP _POTRANS DO lpvOrdTrans
ON KEY LABEL ALT+P ACTIVATE POPUP _OPTIONPOP


*!*************************************************************
*! Name      : lpvActBar
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Bulid a new menu pad [Options]
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
FUNCTION lpvActBar

DO CASE
  CASE BAR() = 1      && Show Summary
    SET MARK OF BAR 1 OF _OPTIONPOP TO .T.
    SET MARK OF BAR 2 OF _OPTIONPOP TO .F.
    IF llQtyPSize
      llQtyPSize=.F.
      =lfvBrowZoom()
    ENDIF
  CASE BAR() = 2      && Show Quantity per Size
    SET MARK OF BAR 1 OF _OPTIONPOP TO .F.
    SET MARK OF BAR 2 OF _OPTIONPOP TO .T.
    IF !llQtyPSize
      llQtyPSize=.T.
      =lfvBrowZoom()
    ENDIF
  CASE BAR() = 4     &&   Zoom Order Browse
    SET MARK OF BAR 4 OF _OPTIONPOP TO !llZoom
    llZoom = !llZoom
    =lfvBrowZoom()
  CASE BAR() = 6     &&   Edit cost per line.
    llEdtLnCst=!llEdtLnCst
    SET MARK OF BAR 6 OF _OPTIONPOP TO llEdtLnCst
    IF llEdtLnCst
      _CUROBJ = OBJNUM(m.nCost2)
    ENDIF
  CASE BAR() = 7      
    IF lcAtype = 'R'
      =lfAppndPO()    &&   Append form P/O.
    ELSE
      =lfAppndPk()    &&   Append from packs.
    ENDIF
  CASE BAR() = 8     && Copy Line
    =lfCpyPst('C')
  CASE BAR() = 9     && Paste Line
    =lfCpyPst('P')
  CASE BAR() = 10 AND ! llImpCost 
    =lfvEditCst()    &&    Edit cost for header.

  *E500272,1 WAB - the cost sheet bar is no 11 and the lines cost sheet is no 12.
  *E500272,1 WAB - START
  *CASE BAR() = 11  AND llImpCost  AND lcAType='P' 
  *  =lfvPOGnLns()    &&    Generate Lines cost sheet.
  
  CASE BAR() = 11  AND llImpCost  
    IF lcAtype $ 'PD'
      =lfvCostSh()    &&    Call Po cost sheet.
    ELSE
      =lfvPOGnLns()    &&    Generate Lines cost sheet. 
    ENDIF  
  CASE BAR() = 12  AND llImpCost  AND lcAType='P' 

    =lfvPOGnLns()    &&    Generate Lines cost sheet.
  *E500272,1 WAB - END
  
  *E301289,1 WAB - Call function to disp profitability screen
  *E301289,1 WAB - START
  *C200208,1 AMH Don't call profitability screen in case of not llDispPric [Start]
  *CASE BAR() = 13  AND lcAType='P' 
  CASE BAR() = 13  AND lcAType='P' AND llDispPric
  *C200208,1 AMH [End]
    =lfvProfit()    &&    call profitability screen
  *E301289,1 WAB - END

ENDCASE

*C102357,1 AMH Run Custom Design Info. screen from Bar in Option pad for Customer J & L. [Start]
IF lcAType = 'A' AND ASCAN(laEvntTrig , PADR('RUNJLINF',10)) <> 0
  =gfDoTriger('POADPO',PADR('RUNJLINF',10))
ENDIF     
*C102357,1 AMH [End]

*C200208,1 AMH Run Custom labelling instrucions screen from Bar in Option pad for Customer STU [Start]
IF lcAType = 'P' AND ASCAN(laEvntTrig , PADR('RUNLBLIN',10)) <> 0
  *B604840,1 AMH Fix bug of Labeling Instruction option not avail in Gen PO frm SO [Start]
  *=gfDoTriger('POSTY',PADR('RUNLBLIN',10))
  IF llExCalled
    =gfDoTriger('POSTYLE',PADR('RUNLBLIN',10))
  ELSE
    =gfDoTriger('POSTY',PADR('RUNLBLIN',10))
  ENDIF
  *B604840,1 AMH [End]
ENDIF     
*C200208,1 AMH [End]

*!*************************************************************
*! Name      : lpvOrdTrans
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Validate Order transactions
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
FUNCTION lpvOrdTrans

IF BAR() = lnOrdTrans
  RETURN
ENDIF  
lnOrdTrans = BAR()
SET MARK OF BAR 1 OF _POTRANS TO .F.
SET MARK OF BAR 2 OF _POTRANS TO .F.
SET MARK OF BAR 3 OF _POTRANS TO .F.
SET MARK OF BAR 4 OF _POTRANS TO .F.
SET MARK OF BAR lnOrdTrans OF _POTRANS TO .T.
SHOW GET PBKEY DISABLE
SHOW GET PBBrKy1 ENABLE
SHOW GET PBBrKy2 ENABLE
ON KEY LABEL ALT+R ACTIVATE WINDOW (lcBrTtl1)
ON KEY LABEL ALT+S ACTIVATE WINDOW (lcBrTtl2)
DO CASE
  CASE lnOrdTrans=2
   =lfvBrow2()
  CASE lnOrdTrans=3
   =lfvBrow2()
  CASE lnOrdTrans=4
   =lfvBrow2()
  OTHE
   ACTI WINDOW (lcWinC22) IN WINDOW (lcWinCh2)
   SHOW GET PBKEY ENABLE
   SHOW GET PBBrKy1 DISABLE
   SHOW GET PBBrKy2 DISABLE
ENDCASE
RETURN

*:*************************************************************
*! Name     : lfvBrowZoom
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose  : Browse zoom.
*:*************************************************************
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
FUNCTION lfvBrowZoom

IF llZoom 
  =lfvBrow1()
  HIDE WINDOW (lcWinC23) SAME
  SHOW GETS WINDOW (lcWinC22) DISABLE ONLY    
  SHOW GET pbKey ENABLE
ELSE
  HIDE WINDOW (lcBrTtl1) SAME
  =lfvBrow1()  
  DO CASE
    CASE lnOrdTrans=2
     =lfvBrow2()
    CASE lnOrdTrans=3
     =lfvBrow2()
    CASE lnOrdTrans=4
     =lfvBrow2()
    OTHE
     =lfGetLine()
     SHOW WINDOW (lcWinC22) TOP  
     ACTI WINDOW (lcWinC22) IN WINDOW (lcWinCh2)
  ENDCASE
ENDIF
RETURN



*!*************************************************************
*! Name      : lfOldValue
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Function to store old value of the current filed.
*!*************************************************************
FUNCTION lfOldValue
lcOldValue = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfCkEOF
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Function if there is lines for P/O.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : .T. If No line was entered or exist
*!                      .F. Not  Eof() there is a line(s) found.
*!*************************************************************
*! Example   : =lfCkEOF()
*!*************************************************************
FUNCTION lfCkEOF
PARA lcCkFile

lnAlias=SELECT()
SELECT (lcCkFile)
IF EOF()
  GO TOP
  lnRet = EOF()
ELSE
  lnRet = .F.
ENDIF
SELECT(lnAlias)
RETURN lnRet



*:*************************************************************

*##0#-#-# Main functions.

*:*************************************************************
*! Name      : lfvPO
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Valid function P/O #.
*:*************************************************************
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
FUNCTION lfvPO
IF llBrowse OR ( !EMPTY(laData[1]) AND !SEEK(lcAType+laData[1],'POSHDR') )
  lcPo=laData[1]
  SELECT POSHDR
  SET FILTER TO cStyType=lcAType
  DO POSBrow WITH lcPo,ALLTRIM(laData[2]),lcAType
  laData[1]=lcPo
  llBrowse = .F.
ENDIF 
*B301291,1 AMM Reassign the variable value because POSBROW changes it !!
DO CASE
  CASE lcAType  = 'R'  
    lcPOType = 'Ret. P/O'
  CASE lcAType  = 'C'  
    lcPOType = 'Contract'
  CASE lcAType  $ 'PN'  
    lcPOType = 'P/O'
  *C200080,1 AMM Dye order object title
  CASE lcAType  $ 'D'  
    lcPOType = 'Dye P/O'
  *C200080,1 AMM end
  *E301291,1 AMM Add the case of adornment order
  CASE lcAType = 'A'
    lcPOType = 'Ador P/O'
  *E301291,1 AMM end
ENDCASE
*B301291,1 AMM end

IF !EMPTY(laData[1])
  *--Get selected P/O informaion and show it in View mode.
  laScrMode=.F.
  laScrMode[2]=.T.
  =lfGetInfo(.T.)
ENDIF
RETURN



*!*************************************************************
*! Name      : lfvStatus
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Function to validate the status.
*!*************************************************************
*! Call      : gfSavSess()
*!*************************************************************
FUNCTION lfvStatus

DO CASE
  CASE ALLTRIM(lcStatus)='On Hold'
    laData[3]='H'  
  CASE ALLTRIM(lcStatus)='Open'
    laData[3]='O'  
  CASE ALLTRIM(lcStatus)='Cancel'
    laData[3]='X'
  CASE ALLTRIM(lcStatus)='Bid'
    laData[3]='B'
  CASE ALLTRIM(lcStatus)='Complete'
    laData[3]='C'
ENDCASE

IF laScrMode[3] AND laData[3]='B' AND POSHDR.Status<>'B'
  =gfDialog("!","You cannot change the status to bid.")
  lcStatus = lcOldValue
  DO CASE
    CASE ALLTRIM(lcOldValue)='On Hold'
      lcStatus='On Hold'
      laData[3]='H'
    CASE ALLTRIM(lcOldValue)='Open'
      lcStatus='Open'
      laData[3]='O'
  ENDCASE
  RETURN .F.
ENDIF

IF laData[3]='B' AND !EMPTY(laData[13])
  *-Status changed to bid, The letter of credit will be removed.
  =gfModalGen('TRM34099B34000','DIALOG')
  lcLCBank   = SPACE(10)
  laData[13] = SPACE(6)
  laData[14] = {}
  SHOW GET laData[13]
  SHOW GET laData[14]  
  SHOW GET lcLCBank
ENDIF

IF lcAType='C' AND laData[3] = 'O' AND ALLTRIM(lcOldValue)='On Hold'
  SELECT (lcPOLine)
  SCAN
    m.Style=Style
    IF lfVldSCnr()
      *-The Style:valid in another contract for the same vendor and period, Cannot proceed!
      =gfModalGen('TRM34018B34000','DIALOG',m.Style)
      lcStatus = lcOldValue
      laData[3]='H'
      RETURN .F.
    ENDIF
  ENDSCAN
ENDIF
RETURN



*:*************************************************************
*! Name      : lfvVendor
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Valid function vendor code.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvVendor()
*!*************************************************************
FUNCTION lfvVendor
PRIVATE lcbrfields

IF !llBrowse AND laData[2]=lcOldValue
  RETURN
ENDIF
IF laScrMode[3] AND SEEK(lcAType+laData[1],'POSLN')
  SELECT POSLN 
  LOCATE REST WHILE cStyType+Po = lcAType+laData[1] FOR TranCd $ '245'
  IF FOUND()
    *-Cannot change the vendor since the recieving was done.
    =gfModalGen('TRM34025B34000','DIALOG','vendor'+'|'+'recieving')        
    SELECT POSHDR
    laData[2]=lcOldValue
    RETURN
  ENDIF
ENDIF

IF llBrowse OR !EMPTY(laData[2])
  SELECT APVENDOR
  SET ORDER TO TAG VENCODE
  IF llBrowse OR !SEEK(laData[2])
    lcBrFields = "CVENDCODE :H='Vendor',"+;
                 "CVENCOMP  :H='Name',"+;
                 "cPhoneNo  :H='Phone',"+;
                 "cVenOurAc :H='Our Account' "
    
    
    *-- E301268,1 HDM[Start] Filter vendor browse upon sup. type = 'Style'
    *B603190,1 Start.
    IF lcAType = 'D'
      lcForExpr = " 'C' $ cVenSupTyp "
    ELSE
    *B603190,1 End.
      lcForExpr = " 'S' $ cVenSupTyp "
    ENDIF

    *IF gfBrows('','cVendCode', 'laTempArr','Vendors')
    *  laData[2]=APVENDOR.cVendCode
    *ELSE
    *  laData[2]=SPACE(8)
    *ENDIF
    
    IF gfBrows([FOR &lcForExpr],'cVendCode', 'laTempArr','Vendors')
      laData[2]=APVENDOR.cVendCode
    ELSE
      laData[2]=SPACE(8)
    ENDIF
    *-- E301268,1 HDM[End]

  ENDIF
  llBrowse = .F.
ENDIF 

SELECT POSHDR
IF EMPTY(laData[2]) OR laData[2] = lcOldValue
  laData[2] = lcOldValue
  _CUROBJ = OBJNUM(laData[2])
  RETURN
ENDIF


IF laScrMode[1]
  *--View P/o by vendor. (GoTo View Mode.)
  lcPo =laData[1]
  lcVen=laData[2]
  DO POSBROW WITH lcPo,lcVen,lcAType
  laData[1]=lcPo
  laData[2]=lcVen
  IF EMPTY(laData[1])
    RETURN
  ENDIF
  *--Switch to View.
  laScrMode=.F.
  laScrMode[2]=.T.
  =lfGetInfo(.T.)
ELSE
  *B605859,1 KHM 04/17/2002 (Begin) Prevnt changing the vendor that has
  *B605859,1                a different division.
  IF laScrMode[3] AND !EMPTY(ApVendor.cDivision) AND;
     PosHdr.cDivision <> ApVendor.cDivision AND !lfCkEOF(lcPoLine)
     =gfModalGen('TRM34182B34000','DIALOG',ALLTRIM(laData[2])+;
                                  '|'+ALLTRIM(ApVendor.cDivision))        

     laData[2] = lcOldValue
     RETURN
  ENDIF   
  *B605859,1 KHM 04/17/2002 (End)

  *--(Add/Edit Modes.)
  *--Get the vendor information due to vendor change.
  laVAddrs[1] =APVendor.cVenComp
  *B803858,1 HBG 11/23/2000 Get The vendore address By using "gfGetAdr" function
  *B803858,1                to display it with the correct formate [Begin]
  *laVAddrs[2] =APVendor.cAddress1
  *laVAddrs[3] =APVendor.cAddress2
  *laVAddrs[4] =APVendor.cAddress3
  *laVAddrs[5] =APVendor.cAddress4
  *laVAddrs[6] =APVendor.cAddress5
  
  laVAddrs[2] = gfGetAdr('APVENDOR' , '' , '' , '' , 1)
  laVAddrs[3] = gfGetAdr('APVENDOR' , '' , '' , '' , 2)
  laVAddrs[4] = gfGetAdr('APVENDOR' , '' , '' , '' , 3)
  laVAddrs[5] = gfGetAdr('APVENDOR' , '' , '' , '' , 4)
  laVAddrs[6] = gfGetAdr('APVENDOR' , '' , '' , '' , 5)
  *B803858,1 [End]
  
  lcLCBank   = SPACE(10)
  laData[13] = SPACE(6)
  laData[14] = {}
  laData[15] = APVendor.cVenCont
  laData[16] = APVendor.cPhoneNo
  *B602670,1 [BEGIN]
  IF !EMPTY(APVendor.CDIVISION)
    laCodInfo[1,07] = "APVENDOR" 
    laCodInfo[1,08] = "VENCODE"
    laCodInfo[1,09] = "laData[2]" 
    laCodInfo[1,10] = "cDivision"
    =gfwCodePop ( @laCodInfo, "CDIVISION" ,"T" )
    *B602968,1

    *B803734,1 KHM 10/12/2000 (Begin) Assigning the vendor division to laData[4].     
    laData[4] = APVendor.CDIVISION
    *B803734,1 KHM 10/12/2000 (End)
    
    IF ALEN(laDiv,1) <> 1
      lnDiv = (ASCAN(laDiv,APVendor.cDivision)/2)
    ELSE
      lnDiv = 1
    ENDIF
    laCodInfo[1,07] = "POSHDR"     
    laCodInfo[1,08] = "POSHDR"     
    laCodInfo[1,09] = "lcAType+laData[1]"     
    laCodInfo[1,10] = "cDivision"  
  ENDIF
  *B602670,1 [END..]
  
  IF !EMPTY(APVendor.cTermCode)
    =gfwCodePop ( @laCodInfo, "CTERMCODE" ,"L" )
    lnTerms = (ASCAN(laTerms,APVendor.cTermCode)/2)
    *--B603849,1 RAMY Fix the bug if the vendor terms code not found in the codes file [start]
    
    *B604460,1 MHM 04/24/2001 [Start]
    *lnTerms = IIF(lnTerms =0,1,lnTerms)
    lnTerms = IIF(lnTerms < 1,1,CEILING(lnTerms))
    *B604460,1 MHM 04/24/2001 [End]
    
    *--B603849,1 RAMY [end]
  ELSE 
    =gfwCodePop ( @laCodInfo, "CTERMCODE" ,"D" ) 
    lnTerms = 1
  ENDIF

  *B602091, read the term code.
  laData[10]=laTerms[lnTerms,2]    

  *--Currency also releted to vendor but we get it only if no lines.
  IF laScrMode[4] AND lfCkEOF(lcPoLine)
    *--(CURR).

    *E301746,1 KHM 10/14/2001 (Begin) Store the base currency to the 
    *E301746,1                duty currency.
    *STORE APVendor.cCurrCode TO laData[26],laData[27]
    STORE APVendor.cCurrCode TO laData[26]
    STORE gcBaseCurr TO laData[27]
    *E301746,1 KHM 10/14/2001 (End)

    IF llMulCurr AND !EMPTY(laData[26])

      *E301077,8 MAB Open Currency file here [Begin
      IF !llOpnCurr
        llOpnCurr = lfOpn_Rest(gcSysHome+'SYCCURR','CCURRCODE')
      ENDIF  
      *E301077,8 MAB Open Currency file here [End..

      lcPfrnsmbl=IIF(SEEK(laData[26],'SycCurr'),SycCurr.cCurrSmbl,'')
      lcDfrnsmbl=IIF(SEEK(laData[27],'SycCurr'),SycCurr.cCurrSmbl,'')
      laData[28]=gfChkRate('lnCurrUnt1',laData[26],laData[5],llEditExRt,gcAct_comp,.F.)
      laData[29]=gfChkRate('lnCurrUnt2',laData[27],laData[5],llEditExRt,gcAct_comp,.F.)
      *-No valid rate for this date. Default the XXX currency to the base currency?
      IF laData[28] = 0
        =gfModalGen('INM34004B34000','DIALOG','vendor')
        STORE gcBaseCurr TO laData[26],laData[27]
        STORE 1 TO laData[28],laData[29]
      ENDIF
    ELSE
      STORE gcBaseCurr TO laData[26],laData[27]
      STORE 1 TO laData[28],laData[29]
    ENDIF
  ENDIF

  *--Update all line with a new vendor.
  IF !lfCkEOF(lcPoLine)
    SELECT (lcPoLine)
    REPLACE ALL &lcPoLine..Vendor WITH laData[2]
    GO TOP
  ENDIF
  
ENDIF

*--Default the warehous info.
IF lnShpLoc = 1 AND (!llWareHous OR llPosSet)
  lcOldValue = ' '
  *E300889,1 Initialize POS location.
  laData[19] = IIF(llPosSet,lcSiteLoc,lcDefLoc)
  =lfvWareH()
ENDIF

laData[20] = IIF(EMPTY(laData[20]),'DEFDEF',laData[20])

*--Show new vendor information.
SHOW GET laVAddrs[1]
SHOW GET laVAddrs[2]
SHOW GET laVAddrs[3]
SHOW GET laVAddrs[4]
SHOW GET laVAddrs[5]
SHOW GET laVAddrs[6]
SHOW GET lcLCBank   
SHOW GET laData[13] 
SHOW GET laData[14] 
SHOW GET laData[15] 
SHOW GET laData[16] 
SHOW GET lnTerms
SHOW GET laData[26]
SHOW GET laData[27]
SHOW GET laData[28]
SHOW GET laData[29]
SELECT (lcPOHdr)
GATHER FROM laData FIELDS &lcScFields MEMO    
RETURN


*:*************************************************************
*! Name      : lfvSrcWhr
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Valid function for Source location case of 
*:             Inter Location P/O.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvSrcWhr()
*!*************************************************************
FUNCTION lfvSrcWhr

IF !llBrowse AND laData[2]=lcOldValue
  RETURN
ENDIF

IF laScrMode[3] AND SEEK(lcAType+laData[1],'POSLN')
  SELECT POSLN 
  LOCATE REST WHILE cStyType+Po = lcAType+laData[1] FOR TranCd $ '2456'
  IF FOUND()
    *-Cannot change the location since the recieving was done.
    =gfModalGen('TRM34025B34000','DIALOG','location'+'|'+IIF(TranCd='6','Issue','recieving'))
    SELECT POSHDR
    laData[2]=lcOldValue
    RETURN
  ENDIF
ENDIF

IF llBrowse OR !EMPTY(laData[2])
  IF llBrowse OR !SEEK( PADR(laData[2],6),'WAREHOUS' )
    laData[2] = gfBrowWare( .F. )
  ENDIF
  llBrowse = .F.
ENDIF 

IF !EMPTY(laData[19]) AND PADR(laData[2],6) = PADR(laData[19],6)
  *--Scource location cannot be same as the Target location!
  
  *B604736,1 KHM 07/30/2001 (Begin) Fixing the spelling mistake of source.
  *=gfModalGen('TRM34108B34000','DIALOG','Scource'+'|'+'Target')
  =gfModalGen('TRM34108B34000','DIALOG','source'+'|'+'Target')
  *B604736,1 KHM 07/30/2001 (End)
  
  laData[2]=lcOldValue
ENDIF

SELECT POSHDR
IF EMPTY(laData[2]) OR laData[2] = lcOldValue
  laData[2] = lcOldValue
  _CUROBJ = OBJNUM(laData[2])
  RETURN
ENDIF


IF laScrMode[1]
  *--View P/o by Source location. (GoTo View Mode.)
  lcPo =laData[1]
  lcVen=laData[2]
  DO POSBROW WITH lcPo,lcVen,lcAType
  laData[1]=lcPo
  laData[2]=lcVen
  IF EMPTY(laData[1])
    RETURN
  ENDIF
  *--Switch to View.
  laScrMode=.F.
  laScrMode[2]=.T.
  =lfGetInfo(.T.)
ELSE
  *--(Add/Edit Modes.)
  *--Get the source location address.
  laVAddrs[1] = WareHous.cDesc
  *B803858,1 HBG 11/23/2000 Get The source location address By using "gfGetAdr" function to
  *B803858,1                display it with its correct formate [Begin]
  *laVAddrs[2] = WareHous.cAddress1
  *laVAddrs[3] = WareHous.cAddress2
  *laVAddrs[4] = WareHous.cAddress3
  *laVAddrs[5] = WareHous.cAddress4
  *laVAddrs[6] = WareHous.cAddress5
  
  laVAddrs[2] = gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
  laVAddrs[3] = gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
  laVAddrs[4] = gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
  laVAddrs[5] = gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
  laVAddrs[6] = gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
  *B803858,1 [End]

  *--Currency also releted to vendor but we get it only if no lines.
  IF laScrMode[4] AND lfCkEOF(lcPoLine)
    STORE gcBaseCurr TO laData[26],laData[27]
    STORE 1 TO laData[28],laData[29]
  ENDIF

  *--Update all line with a new vendor.
  IF !lfCkEOF(lcPoLine)
    SELECT (lcPoLine)
    SCAN
      IF !SEEK(PADR(Style,19)+laData[2]+SPACE(10),'STYDYE')
        *-Style: xxx is not assigned to location: xxx. \<Add
        =gfModalGen('QRM34048B34003','DIALOG',ALLTRIM(Style)+'|'+ALLTRIM(laData[2]))
        DO gpAdStyWar WITH Style,SPACE(10),laData[2]
      ENDIF
      REPLACE &lcPoLine..Vendor WITH laData[2]
    ENDSCAN
    GO TOP
  ENDIF
  
ENDIF
*--Show new vendor information.
SHOW GET laVAddrs[1]
SHOW GET laVAddrs[2]
SHOW GET laVAddrs[3]
SHOW GET laVAddrs[4]
SHOW GET laVAddrs[5]
SHOW GET laVAddrs[6]
SHOW GET laData[26]
SHOW GET laData[27]
SHOW GET laData[28]
SHOW GET laData[29]
SELECT (lcPOHdr)
GATHER FROM laData FIELDS &lcScFields MEMO    
RETURN



*:*************************************************************
*! Name      : lfvMult
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Valid function use multi ship to locations.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters: llDontChk -> Donot check the multi valid.
*!*************************************************************
*! Returns          : ............
*!*************************************************************
*! Example   : =lfvMult()
*!*************************************************************
FUNCTION lfvMult
PARA llDontChk

lnAlias = SELECT()
IF llMultiWare
  STORE '' TO laData[19],laData[69],laData[70]
  *E300889,1 Initialize POS location.
  laData[19] = IIF(llPosSet,lcSiteLoc,'')

  SHOW GET pbShpTo    DISABLE
  SHOW GET laData[19] DISABLE
  SHOW GET laData[69] DISABLE
  SHOW GET laData[70] DISABLE
  SHOW GET lnShpLoc   DISABLE 
  laData[18] = 'At Line Level'
  STORE '' TO laData[21],laData[22],laData[23],laData[24],laData[25]
  SHOW GET laData[21]
  SHOW GET laData[22]
  SHOW GET laData[23]
  SHOW GET laData[24]
  SHOW GET laData[25]        
ELSE
  IF !llDontChk AND !lfCkEOF(lcPoLine)
    *-You cannot change the lcPOType multiple shipto status to be single,
    *-since the lcPOType lines was entered.
    =gfModalGen('QRM34028B34000','DIALOG',lcPOType+'|'+lcPOType)
    llMultiWare=.T.
    SHOW GET llMultiWare
    RETURN
  ENDIF
  lcDispMode = IIF(laScrMode[2],'DISABLE','ENABLE')
  SHOW GET pbShpTo    &lcDispMode
  IF lnShpLoc=1
    SHOW GET laData[69] DISABLE
    SHOW GET laData[19] &lcDispMode
  ELSE
    SHOW GET laData[19] DISABLE
    SHOW GET laData[69] &lcDispMode
  ENDIF
  SHOW GET lnShpLoc   &lcDispMode
  laData[18] = IIF(laData[18] = 'At Line Level','',laData[18] )
ENDIF
SHOW GET laData[18]
=lfvBrow1()

*B605105,1 AMH Call lfrefresh only if not returned from PO cost sheet screen [Start]
*=lfRefresh(lcWinCh0)
IF !llCallShow
  =lfRefresh(lcWinCh0)
ENDIF
*B605105,1 AMH [End]

SELECT (lcPOHdr)
REPLACE lMultiWare WITH llMultiWare
SELECT(lnAlias)
RETURN


*:*************************************************************
*! Name      : lfvchgShp
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Valid changing the ship to type (location/account).
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvchgShp()
*!*************************************************************
FUNCTION lfvchgShp

IF lnShpLoc=lcOldValue
  RETURN
ENDIF

IF !lfCkEOF(lcPoLine)
  *-You cannot change the lcPOType shipto,
  *-since the lcPOType lines was entered.
  =gfModalGen('QRM34027B34000','DIALOG',lcPOType+'|'+lcPOType)
  lnShpLoc = IIF(lnShpLoc = 1 ,2 ,1)
  SHOW GET lnShpLoc
  RETURN
ENDIF

lcDispMode = IIF(laScrMode[2],'DISABLE','ENABLE')
IF lnShpLoc=1
  IF !llWareHous OR llPosSet
    lcOldValue = ' '
    *E300889,1 Initialize POS location.
    laData[19] = IIF(llPosSet,lcSiteLoc,lcDefLoc)
    =lfvWareH()
  ELSE
    STORE '' TO laData[18],laData[21],laData[22],laData[23],laData[24],laData[25]
  ENDIF
  STORE '' TO laData[69],laData[70]
  SHOW GET laData[69] DISABLE
  SHOW GET laData[70] DISABLE
  SHOW GET laData[19] &lcDispMode
ELSE
  *--There is no drop ship location setup in the system, Cannot ship to customer.
  IF EMPTY(lcDropLoc)
    =gfModalGen('QRM34112B34000','DIALOG')
    lnShpLoc=1
    SHOW GET lnShpLoc
    RETURN
  ENDIF  
  STORE '' TO laData[18],laData[21],laData[22],laData[23],laData[24],laData[25]
  *E300889,1 Initialize POS location.
  laData[19] = IIF(llPosSet,lcSiteLoc,'')

  SHOW GET laData[19] DISABLE
  SHOW GET laData[69] &lcDispMode
  SHOW GET laData[70] &lcDispMode
ENDIF
*E301291,1 AMM Refresh the ship to at line level
lnLShpLoc = lnShpLoc
*E301291,1 AMM end
SHOW GET laData[18]
SHOW GET laData[21]
SHOW GET laData[22]
SHOW GET laData[23]
SHOW GET laData[24]
SHOW GET laData[25]        
RETURN


*:*************************************************************
*! Name      : lfvWareH
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Valid function warehouse code (Trager Location).
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvWareH()
*!*************************************************************
FUNCTION lfvWareH

IF !llBrowse AND !EMPTY(lcOldValue) AND laData[19]=lcOldValue 
  RETURN
ENDIF


IF laScrMode[3] AND SEEK(lcAType+laData[1],'POSLN')
  SELECT POSLN 
  LOCATE REST WHILE cStyType+Po = lcAType+laData[1] FOR TranCd $ '2456'
  IF FOUND()
    *-Cannot change the location since the recieving was done.
    =gfModalGen('TRM34025B34000','DIALOG','location'+'|'+IIF(TranCd='6','Issue','recieving'))
    SELECT POSHDR
    laData[19]=lcOldValue
    RETURN
  ENDIF
ENDIF

IF llBrowse OR !SEEK( laData[19],'WareHous' )
  laData[19] = gfBrowWare( .F. )
ENDIF  

llBrowse = .F.
*E301291,1 AMM Consider the case of adornment order
*IF lcAType = 'N' AND !EMPTY(laData[19]) AND PADR(laData[19],6) = PADR(laData[2],6)
IF lcAType $ 'NA' AND !EMPTY(laData[19]) AND PADR(laData[19],6) = PADR(laData[2],6)
*E301291,1 AMM end
  *--Target location cannot be same as the Scource location!

  *B604736,1 KHM 07/30/2001 (Begin) Fixing the spelling mistake of source.
  *=gfModalGen('TRM34108B34000','DIALOG','Target'+'|'+'Scource')
  =gfModalGen('TRM34108B34000','DIALOG','Target'+'|'+'source')
  *B604736,1 KHM 07/30/2001 (End)
  
  laData[19]=lcOldValue
  _CUROBJ = OBJNUM(laData[19]) 
  RETURN
ENDIF

SHOW GET laData[19]
*--If this P/o has lines.
IF !EMPTY(lcOldValue) AND laData[19]<>lcOldValue AND ! lfCkEOF(lcPoLine)
  *-Do you wish to assign the +lcPOType+ lines to the new shipto Location ?"\<Assign;\<Cancel"
  IF gfModalGen('QRM34026B34002','DIALOG',lcPOType+'|'+'location') = 2  && No
    laData[19]=lcOldValue
    =SEEK( laData[19],'WareHous' )
    SHOW GET laData[19]
    RETURN
  ELSE
    SELECT (lcPoLine)
    lnSvRc=RECNO()
    SCAN
      IF !SEEK(&lcPoLine..Style+laData[19]+SPACE(10),'STYDYE')
        *-Style: xxx is not assigned to location: xxx,"\<Add"
        =gfModalGen('QRM34048B34003','DIALOG',ALLTRIM(&lcPoLine..Style)+'|'+laData[19])

        DO gpAdStyWar WITH &lcPoLine..Style,SPACE(10),laData[19]
      ENDIF
      REPLACE &lcPoLine..cWareCode WITH laData[19],;
              &lcPoLine..Account   WITH SPACE(5),;
              &lcPoLine..Store     WITH SPACE(8)
    ENDSCAN
    GOTO lnSvRc
    =lfGetLine()
  ENDIF    
ENDIF 
SELECT (lcPOHdr)
REPLACE cWareCode WITH laData[19]
*-- Display the new Ship to Location Address.
=SEEK(laData[19],'WAREHOUS')
laData[18]= WareHous.cDesc

*B803858,1 HBG 11/23/2000 Get The Ship to Location address By using "gfGetAdr" function to
*B803858,1                display it with its correcte formate [Begin]
*laData[21]= WareHous.cAddress1
*laData[22]= WareHous.cAddress2
*laData[23]= WareHous.cAddress3
*laData[24]= WareHous.cAddress4
*laData[25]= WareHous.cAddress5

laData[21] = gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
laData[22] = gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
laData[23] = gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
laData[24] = gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
laData[25] = gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
*B803858,1 [End]

SHOW GET laData[19]
SHOW GET laData[18]
SHOW GET laData[21]
SHOW GET laData[22]
SHOW GET laData[23]
SHOW GET laData[24]
SHOW GET laData[25]        
SELECT (lcPOHdr)
GATHER FROM laData FIELDS &lcScFields MEMO    

*C200491,1 ALB Generate PO lines from SO (GMA) [Begin]
IF ASCAN(laEvntTrig , PADR('GENPOFSO',10)) <> 0
  =gfDoTriger('POSTY',PADR('GENPOFSO',10))
ENDIF
*C200491,1 ALB Generate PO lines from SO (GMA) [End]

RETURN



*:*************************************************************
*! Name      : lfvAccount
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Valid function use multi warehouse.
*:*************************************************************
FUNCTION lfvAccount

IF !llBrowse AND laData[69]=lcOldValue 
  RETURN
ENDIF
lnAlias=SELECT()
IF llBrowse OR !SEEK('M'+laData[69],'CUSTOMER')
  SELECT CUSTOMER 
  xAccount = laData[69]
  DO CUSBROWM WITH xAccount
  laData[69] = xAccount
ENDIF
llBrowse= .F.
SELECT(lnAlias)
SHOW GET laData[69]
IF EMPTY(laData[69])
  RETURN
ENDIF
*--If this P/o has lines.
IF !EMPTY(lcOldValue) AND ! lfCkEOF(lcPoLine)
  *-Do you wish to assign the +lcPOType+ lines to the new shipto account?"\<Assign;\<Cancel"
  IF gfModalGen('QRM34026B34002','DIALOG',lcPOType+'|'+'account') = 2  && No
    laData[69]=lcOldValue
    =SEEK('M'+laData[69],'CUSTOMER')
    SHOW GET laData[69]
    RETURN
  ELSE
    SELECT (lcPoLine)
    lnSvRc=RECNO()
    SCAN
      IF !SEEK(&lcPoLine..Style+lcDropLoc+SPACE(10),'STYDYE')
        *-Style: xxx is not assigned to location: xxx,"\<Add"
        =gfModalGen('QRM34048B34003','DIALOG',ALLTRIM(&lcPoLine..Style)+'|'+lcDropLoc)
        DO gpAdStyWar WITH &lcPoLine..Style,SPACE(10),lcDropLoc
      ENDIF
      REPLACE &lcPoLine..Account   WITH laData[69],; 
              &lcPoLine..cWareCode WITH laData[69],;
              &lcPoLine..Store     WITH SPACE(8)
    ENDSCAN
    GOTO lnSvRc
    =lfGetLine()
  ENDIF    
ENDIF 
SELECT (lcPOHdr)
=SEEK('M'+laData[69],'CUSTOMER')
laData[18] = CUSTOMER.StName
laData[21] = CUSTOMER.cAddress1
laData[22] = CUSTOMER.cAddress2
laData[23] = CUSTOMER.cAddress3
laData[24] = CUSTOMER.cAddress4
laData[25] = ALLTRIM(CUSTOMER.cAddress5)+'  '+ALLTRIM(CUSTOMER.cAddress6)
laData[70] = ''
SHOW GET laData[70]
SHOW GET laData[18]
SHOW GET laData[21]
SHOW GET laData[22]
SHOW GET laData[23]
SHOW GET laData[24]
SHOW GET laData[25]        
GATHER FROM laData FIELDS &lcScFields MEMO    
RETURN


*:*************************************************************
*! Name      : lfvStores
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Validate account store.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvStores()
*!*************************************************************
FUNCTION lfvStores

IF !EMPTY(laData[69]) AND !EMPTY(laData[70])
  IF !SEEK('S'+laData[69]+laData[70],'CUSTOMER')
    xStore   = laData[70]
    IF !CUSBROWS(laData[69],.T.)
      STORE SPACE(8) TO xStore
    ENDIF
    laData[70] = xStore
  ENDIF 
  SHOW GET laData[70]
  IF EMPTY(laData[70])
    RETURN
  ENDIF
  IF ! lfCkEOF(lcPoLine)
    SELECT (lcPOLine)
    lnSvRc=RECNO()
    REPLACE ALL Store WITH laData[70]
    GOTO lnSvRc
    =lfGetLine()
  ENDIF
  SELECT (lcPOHdr)
  REPLACE Store WITH laData[70]
  =SEEK('S'+laData[69]+laData[70],'CUSTOMER')
  laData[18] = CUSTOMER.StName
  laData[21] = CUSTOMER.cAddress1
  laData[22] = CUSTOMER.cAddress2
  laData[23] = CUSTOMER.cAddress3
  laData[24] = CUSTOMER.cAddress4
  laData[25] = ALLTRIM(CUSTOMER.cAddress5)+'  '+ALLTRIM(CUSTOMER.cAddress6)
  SHOW GET laData[69]
  SHOW GET laData[18]
  SHOW GET laData[21]
  SHOW GET laData[22]
  SHOW GET laData[23]
  SHOW GET laData[24]
  SHOW GET laData[25]        
  SELECT (lcPOHdr)
  GATHER FROM laData FIELDS &lcScFields MEMO    
ENDIF
RETURN






*:*************************************************************

*##1#-#-# First folder functions.

*!*************************************************************
*! Name      : lfvEntDt
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Validate the entered date.
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
FUNCTION lfvEntDt

IF EMPTY(laData[5])
  *-You cannot leave the entered date empty.
  =gfModalGen('INM34005B34000','DIALOG','entered')
  _CUROBJ = OBJNUM(laData[5])
  laData[5] = lcOldValue
  RETURN
ENDIF  

IF laData[5] > laData[6]
  *-Completion date cannot be prior to entered date.
  =gfModalGen('INM34003B34000','DIALOG','Completion')
  _CUROBJ = OBJNUM(laData[5])
  laData[5] = lcOldValue
  RETURN
ENDIF

IF laData[5] > laData[7]
  *-Available date cannot be prior to entered date.
  =gfModalGen('INM34003B34000','DIALOG','Available')
  _CUROBJ = OBJNUM(laData[5])
  laData[5] = lcOldValue
  RETURN
ENDIF


IF llMulCurr AND laData[5] <> lcOldValue
  lcOldRate=laData[28]
  laData[28] = lfCurRat(laData[5],laData[26],OBJNUM(laData[5]),'lnCurrUnt1',laData[28])
  IF laData[28] <> lcOldRate AND !lfCkEOF(lcPoLine)
    *-The price currency rate was changed, the line costs will be recalculated.
    =gfModalGen('INM34029B34000','DIALOG','price')        
    =lfChgLCst('P')
  ENDIF

  lcOldRate=laData[29]
  laData[29] = lfCurRat(laData[5],laData[27],OBJNUM(laData[5]),'lnCurrUnt2',laData[29])
  IF laData[29] <> lcOldRate AND !lfCkEOF(lcPoLine)
    *-The duty currency rate was changed, the line costs will be recalculated.
    =gfModalGen('INM34029B34000','DIALOG','duty')        
    =lfChgLCst('D')
  ENDIF  
  SHOW GET laData[28]
  SHOW GET laData[29]
ENDIF



*!*************************************************************
*! Name      : lfvComDt
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Validate the completion date.
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
FUNCTION lfvComDt

IF EMPTY(laData[6])
  *-You cannot leave the Completion date empty.
  =gfModalGen('INM34005B34000','DIALOG','Completion')
  _CUROBJ = OBJNUM(laData[6])
  laData[6] = lcOldValue
ELSE
  IF laData[5] > laData[6]
    *-Completion date cannot be prior to entered date.
    =gfModalGen('INM34003B34000','DIALOG','Completion')
    _CUROBJ = OBJNUM(laData[6])
    laData[6] = lcOldValue
  ENDIF
ENDIF  


*!*************************************************************
*! Name      : lfvAvlDt
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Validate the available date.
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
FUNCTION lfvAvlDt

IF EMPTY(laData[7])
  *-You cannot leave the Available date empty.
  =gfModalGen('INM34005B34000','DIALOG','Available')
  _CUROBJ = OBJNUM(laData[7])
  laData[7] = lcOldValue
ELSE
  IF laData[5] > laData[7]
    *-Available date cannot be prior to entered date.
    =gfModalGen('INM34003B34000','DIALOG','Available')
    _CUROBJ = OBJNUM(laData[7])
    laData[7] = lcOldValue
  ENDIF
ENDIF


*!*************************************************************
*! Name      : lfvPurGrp
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Validate the PURCHASE GROUP.
*!*************************************************************
FUNCTION lfvPurGrp

IF !lfCkEOF(lcPoLine) AND laData[67] <> laPurCode[lnPurCode,2]
  *-You cannot change the Purchase group,
  *-since the lcPOType lines was entered.
  =gfModalGen('TRM34129B34000','DIALOG',lcPOType)
  RETURN
ENDIF
laData[67]=laPurCode[lnPurCode,2]
SELECT (lcPOHdr)
GATHER FROM laData FIELDS &lcScFields MEMO    
RETURN


*!*************************************************************
*! Name      : lfvCurrncy
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Validate the currency code. 
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
FUNCTION lfvCurrncy
PARA lcCurrency,lcCurMth

*E301077,8 MAB Open Currency file here [Begin
IF !llOpnCurr
  llOpnCurr = lfOpn_Rest(gcSysHome+'SYCCURR','CCURRCODE')
ENDIF  
*E301077,8 MAB Open Currency file here [End..

IF !EMPTY(lcCurrency) AND lcCurrency=lcOldValue
  RETURN
ENDIF  
=gfcurrbrow(@lcCurrency)
IF !EMPTY(lcCurrency)
  IF lcCurMth=1
    laData[26]=lcCurrency
    lcPfrnsmbl= IIF(SEEK(laData[26],'SycCurr'),SycCurr.cCurrSmbl,'')
    lcOldRate = laData[28]

    laData[28] = lfCurRat(laData[5],laData[26],OBJNUM(laData[26]),'lnCurrUnt1',laData[28])
    IF !lfCkEOF(lcPoLine)
      *-The price currency rate was changed, the line costs will be recalculated.
      =gfModalGen('INM34029B34000','DIALOG','price')        
      =lfChgLCst('P')
    ENDIF

  ELSE
    laData[27] = lcCurrency  
    lcDfrnsmbl= IIF(SEEK(laData[27],'SycCurr'),SycCurr.cCurrSmbl,'')
    lcOldRate=laData[29]
    laData[29] = lfCurRat(laData[5],laData[27],OBJNUM(laData[27]),'lnCurrUnt2',laData[29])
    IF !lfCkEOF(lcPoLine)
      *-The duty currency rate was changed, the line costs will be recalculated.
      =gfModalGen('INM34029B34000','DIALOG','duty')        
      =lfChgLCst('D')
    ENDIF  
  ENDIF
  SHOW GET laData[28]
  SHOW GET laData[29]
ELSE
  IF lcCurMth=1
    laData[26]=lcOldValue
  ELSE
    laData[27]=lcOldValue
  ENDIF
ENDIF


*!*************************************************************
*! Name      : lfCurRat
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Validate the currency Rate. 
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
FUNCTION lfCurRat
PARA ldEntDt,lcCurCod,lnObjNm,lcCurUnt,lnOldRate

IF lcCurCod = gcBaseCurr
  lnCrrRate = 1
  &lcCurUnt = 1
ELSE
  lnCrrRate = gfChkRate(lcCurUnt,lcCurCod,ldEntDt,llEditExRt,gcAct_comp,.F.)
ENDIF
IF lnCrrRate <= 0
  *-No valid rate for this date. Default the XXX currency to the base currency?
  IF laScrMode[4] AND gfModalGen('INM34004B00006','DIALOG',IIF(lcCurUnt = 'lnCurrUnt1','Price','Duty')) = 1
    lcCurCod = gcBaseCurr
    lnCrrRate = 1
    IF lcCurUnt = 'lnCurrUnt1'
      laData[26] = gcBaseCurr
    ELSE
      laData[27] = gcBaseCurr    
    ENDIF
  ELSE
    IF laScrMode[3]
      *-You have to enter date with valid currency rate!
      =gfModalGen('TRM34030B34000','DIALOG')        
    ENDIF 
    IF lnObjNm<>0
      _CUROBJ = lnObjNm
    ENDIF
    RETURN (lnOldRate)
  ENDIF 
ENDIF
SHOW GET laData[26]
SHOW GET laData[27]
RETURN (lnCrrRate)



*!*************************************************************
*! Name      : lfvLinkCod
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function to validate gl link code field.
*!*************************************************************
*! Calls     : gfGLBrowse 6chr 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*!  Called case of return P/O only.
*!*************************************************************
*! Example   : =lfvlinkcod()
*!*************************************************************
FUNCTION lfvlinkcod

lnAlias=SELECT()
IF llbrowse OR !EMPTY(laData[20])
  lcGllnk6   = laData[20]
  =gfGLbrowse('05',@lcGllnk6,'',0)
  IF !EMPTY(lcGllnk6)
    laData[20] = lcGllnk6
  ENDIF
ENDIF
laData[20] = IIF(EMPTY(laData[20]),'DEFDEF',laData[20])
SHOW GET laData[20]
llbrowse = .F.
SELECT(lnalias)
RETURN


*!*************************************************************
*! Name      : lfvGetLC
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Function to browse the lc file and lc bank.
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
FUNCTION lfvGetLC
PARAMETERS llBrwBnk

IF !llBrwBnk
  IF EMPTY(laData[13])
    RETURN  
  ENDIF
  SELECT LC
  IF !SEEK(laData[13])
    lcLCno=laData[13]
    =lfLcBrow(@lcLCBank,@lcLCno)
    laData[13]=lcLCno
    lcLCBank = LC.cLCBank
  ELSE
    lcLCBank = LC.cLCBank
  ENDIF
  SHOW GET lcLCBank
ELSE
  IF EMPTY(lcLCBank)
    RETURN
  ENDIF 
  SELECT LC
  SET ORDER TO TAG LcBnk
  IF !SEEK(lcLCBank)
    lcLCno=laData[13]
    =lfLcBrow(@lcLCBank,@lcLCno)
    laData[13]=lcLCno
  ELSE
    laData[13] = LC.cLCno
  ENDIF
  SELECT LC
  SET ORDER TO TAG Lc
  SHOW GET laData[13]
ENDIF

IF !EMPTY(laData[13]) .OR. !EMPTY(lcLCBank)
  laData[14] = LC.Expire
  SHOW GET laData[14]
ENDIF
RETURN


*!*************************************************************
*! Name      : lfLcBrow
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Browse the lc file and lc bank.
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
FUNCTION lfLcBrow

PARAMETER lcBnkLc,lcLcNo
PRIVATE lcBrFields,lcTitle,laPData,lcVenCode
DECLARE laPData[2] && array to get values from browse
laPData=' '
lcBrFields = "cLCNo   :R :H='LC #':6,"+;
             "cLCBank :R :H='Bank LC #':10,"+;
             "Vendor  :R :H='Vendor':10,"+;
             "ApVendor.cVenComp :R :H='Vendor Name':20,"+;
  	         "EXPIRE  :R :H='Expire':10,"+;
             "US_AMT  :R :H='Amount'"
SELECT LC
GO TOP
IF EOF()
  *-There are no LCs for this vendor!
  =gfModalGen('TRM34031B34000','DIALOG')        
  lcLcNo  = SPACE(06)   
  lcBnkLc = SPACE(10)
  SELECT POSHDR   
  RETURN
ENDIF
SELECT LC
SET RELATION TO VENDOR INTO ApVendor

llWasSel=gfBrows('',"cLCno,cLCBank,Expire","laPData")
IF llWasSel
  lcLcNo     = laPData[1]
  lcBnkLc    = laPData[2]
  laData[14] = laPData[3]
ELSE
  lcLcNo  = SPACE(06)   
  lcBnkLc = SPACE(10)
  laData[14] = {}
ENDIF  

SELECT LC
SET RELATION TO
SELECT POSHDR
RETURN llWasSel






*:*************************************************************

*##2#-#-# Second folder functions.

*!*************************************************************
*! Name      : lfvBrow1
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Original Browse function.
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
FUNCTION lfvBrow1

lcSelect = SELECT()
lcBrTtl1 = SUBSTR(gcBaseWind,2,1)+'_Original Detail'
lnMarker = RECNO()

IF llQtyPSize
  lcBrD1fld= [cMarker =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,]+;
             [Style :30 :H=lcStyHdr :R,]+;
             [Qty1  :H='Size1':R,]+;
             [Qty2  :H='Size2':R,]+;
             [Qty3  :H='Size3':R,]+;
             [Qty4  :H='Size4':R,]+;
             [Qty5  :H='Size5':R,]+;
             [Qty6  :H='Size6':R,]+;
             [Qty7  :H='Size7':R,]+;
             [Qty8  :H='Size8':R,]+;
             [Prepak:H='PP':R,]+;
             [PPQty :H='PPQt':R,]+;
             [Reference :H='Reference':R,]+;
             [cVenSty :H='Vendor Style':R]
ELSE
  *C200162,1 MHM 02/27/2001 (Begin) Changing the browse according to 
  *C200162,1                the existance of the trigger.
  IF lcAtype = "N" AND ASCAN(laEvntTrig , PADR('ADDOPTN',10)) <> 0
    lcBrD1fld= [cMarker =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,]+;
               [Style   :30 :H=lcStyHdr :R,]+;
               [lcShToTTl=IIF(!EMPTY(Account),Account,cWareCode) :H='Ship To':R,]+;
               [TotQty  :H='Qty'            :R:P='9999999']
   IF llCostPrv
      lcBrD1fld = lcBrD1fld+[,Gros_Price :H='Price '+lcPfrnsmbl :R:P='999999999.999',]+;
                            [lnAmount=IIF(Disc_Pcnt=0,nECost1,(Gros_Price*(1-Disc_Pcnt/100)))*TotQty :H='Amount' :P='99999999999.999']
   ENDIF                         

  ELSE
  *C200162,1 MHM 02/27/2001 (End)
  lcBrD1fld= [cMarker =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,]+;
             [Style   :30 :H=lcStyHdr :R,]+;
             [Prepak  :H='P' :P='!':R,]+;
             [PPQty   :H='PPQty':P='9999':R,]

  *B604279,1 MHM Adding the ship To to the browse in case of Inter-Loc PO  [start]
  IF lcAType = 'N'
    lcBrD1fld=lcBrD1fld+[lcShToTTl=IIF(!EMPTY(Account),Account,cWareCode) :H='Ship To':R,]
  ENDIF
  *B604279,1 MHM Adding the ship To to the browse in case of Inter-Loc PO  [END]

  IF llCostPrv
    
    *E301169,1 Change browse style To include gross price and percentage. [Begin]
    *IF !llMulCurr OR (laData[26]=gcBaseCurr AND laData[27]=gcBaseCurr)
    *  lcBrD1fld=lcBrD1fld+[nCost1  :H='Price '+lcPfrnsmbl :R:P='99999999.999',]
    *ENDIF               
    
    *lcBrD1fld=lcBrD1fld+[nECost1 :H='Price' :R:P='99999999.999',]+;
    *                    [TotQty  :H='Qty'  :R:P='9999999',]+;         
    *                    [lnAmount=(nECost1*TotQty) :H='Amount' :P='99999999999.999']
    *B602861,1 get the amount depends on gross_price and discount insted of net price wich is rounded.
    *lcBrD1fld=lcBrD1fld+[Gros_Price :H='G. Price '+lcPfrnsmbl :R:P='99999.99',]+;
    *                    [Disc_Pcnt  :H='Disc.'  :R:P='%99.99',]   +;
    *              IIF(!llMulCurr OR (laData[26]=gcBaseCurr AND laData[27]=gcBaseCurr),;
    *                    [nCost1  :H='Net '+lcPfrnsmbl :R:P='9999999.999',],[]) +;                        
    *                    [TotQty  :H='Qty'            :R:P='9999999',] +;         
    *                    [lnAmount=(nECost1*TotQty) :H='Amount' :P='999999999.999']
    *B603348,1 ABD Increse Picture Of G. Price Filed. [ Begin ]
    *lcBrD1fld=lcBrD1fld+[Gros_Price :H='G. Price '+lcPfrnsmbl :R:P='99999.99',]+;
    *                    [Disc_Pcnt  :H='Disc.'  :R:P='%99.99',]
    *B603713,10 HBG Increase the price field width in the browse (Start) 
    *lcBrD1fld=lcBrD1fld+[Gros_Price :H='G. Price '+lcPfrnsmbl :R:P='9999999.99',]+;
                        [Disc_Pcnt  :H='Disc.'  :R:P='%99.99',]
    lcBrD1fld=lcBrD1fld+[Gros_Price :H='G. Price '+lcPfrnsmbl :R:P='999999999.99',]+;
                        [Disc_Pcnt  :H='Disc.'  :R:P='%99.99',]
   
    *B604279,1 MHM Adding the ship To to the browse in case of Inter-Loc PO  [start]
    IF lcAType <> 'N'
      lcBrD1fld=lcBrD1fld+[Gros_Price :H='G. Price '+lcPfrnsmbl :R:P='999999999.99',]+;
                          [Disc_Pcnt  :H='Disc.'  :R:P='%99.99',]
    ENDIF                      
    *B604279,1 MHM Adding the ship To to the browse in case of Inter-Loc PO  [END]

    *B603713,10 (End)   
    *B603348,1 ABD [ End ]
    IF !llMulCurr OR (laData[26]=gcBaseCurr AND laData[27]=gcBaseCurr) 
     *B603348,1 ABD Increse Picture Of Amount Filed. [ Begin ]
      *lcBrD1fld=lcBrD1fld+[nCost1  :H='Net '+lcPfrnsmbl :R:P='9999999.999',]+;
                          [TotQty  :H='Qty'            :R:P='9999999',]+;         
                          [lnAmount=IIF(Disc_Pcnt=0,nECost1,(Gros_Price*(1-Disc_Pcnt/100)))*TotQty :H='Amount' :P='999999999.999']
      *B603713,10 HBG Increase the Net field width in the browse (Start) 
      *lcBrD1fld=lcBrD1fld+[nCost1  :H='Net '+lcPfrnsmbl :R:P='9999999.999',]+;
                           [TotQty  :H='Qty'            :R:P='9999999',]+;         
                           [lnAmount=IIF(Disc_Pcnt=0,nECost1,(Gros_Price*(1-Disc_Pcnt/100)))*TotQty :H='Amount' :P='99999999999.999']
     
      lcBrD1fld=lcBrD1fld+[nCost1  :H='Net '+lcPfrnsmbl :R:P='999999999.999',]+;
                          [TotQty  :H='Qty'            :R:P='9999999',]+;         
                          [lnAmount=IIF(Disc_Pcnt=0,nECost1,(Gros_Price*(1-Disc_Pcnt/100)))*TotQty :H='Amount' :P='99999999999.999']
    
      *B603713,10 (End) 
      *B603348,1 [ End ]
    ELSE
      lcBrD1fld=lcBrD1fld+[TotQty  :H='Qty'            :R:P='9999999',]+;
                          [lnAmount=(nECost1*TotQty) :H='Amount' :P='999999999.999']
    ENDIF
    *E301169,1 Change browse style To include gross price and percentage. [End  ]

  ELSE
    lcBrD1fld=lcBrD1fld+[TotQty  :H='Qty'  :R:P='9999999']
  ENDIF

  *B604279,1 MHM Adding the ship To to the browse in case of Inter-Loc PO  [start]
  IF lcAType = 'N'
    lcBrD1fld=lcBrD1fld+[,Gros_Price :H='G. Price '+lcPfrnsmbl :R:P='999999999.99',]+;
                        [Disc_Pcnt  :H='Disc.'  :R:P='%99.99']
  ENDIF                      
  *B604279,1 MHM Adding the ship To to the browse in case of Inter-Loc PO  [End]

  *B604279,1 MHM Adding the ship To to the browse in case of Inter-Loc PO  [start]
  *IF llMultiWare
  IF lcAType <> 'N' AND llMultiWare
  *B604279,1 MHM Adding the ship To to the browse in case of Inter-Loc PO  [End]
    lcBrD1fld=lcBrD1fld+[,lcShToTTl=IIF(!EMPTY(Account),Account,cWareCode) :H=IIF(lcAType='R','Ship From','Ship To')]
  ENDIF

  IF llCostPrv
    *B603713,10 HBG Increase the Base price field width in the browse. (Start)
    *lcBrD1fld=lcBrD1fld+[,nECost1 :H='Base Price' :R:P='99999999.999']
    lcBrD1fld=lcBrD1fld+[,nECost1 :H='Base Price' :R:P='999999999.999']
    *B603713,10 (End)
  ENDIF
  *C200162,1 MHM 02/27/2001 (Begin) END IF of the custom trigger.
  ENDIF
  *C200162,1 MHM 02/27/2001 (End)
ENDIF

*E301289,1 WAB - Add Selling Price & Gross Margin to browse fileds IF lcAType = 'P' 
*E301289,1 WAB -(Purchase Order) and the llDispPric(setup for display selling price) is true 
*E301289,1 WAB - START
IF lcAType = 'P' AND llDispPric
  lcBrD1fld = lcBrD1fld + [,nSelPrice :H='Selling Price' :R:P='999999999.99',;
            nGrosMrgn :H='Gross Margin' :R:P='9999.99']
ENDIF
*E301289,1 WAB - END

*E300935,1 Add Dyelot field in the Browse [begin]
lcBrD1fld = lcBrD1fld + IIF(llDyelot,;
                        [,Dyelot :H= 'Dyelot' :R :20],'')
*E300935,1 Add Dyelot field in the Browse [end]
lcBWinTl=IIF(llZoom,lcWinC24,lcWinC21)

*Start, Custom fields in browse of adornment order for fresh Prouce.
IF lcAType = 'A'
  =gfDoTriger('POSTYLE',PADR('ADPOBRFLD',10))
ENDIF
*End.

SELECT (lcPOLine)

*C123847,1  TMI [Start] Add qty/Ctn field between PPqty and Gross Price for DIR03
IF ASCAN(laEvntTrig , PADR('ADQTCFLD',10)) <> 0
  =gfDoTriger('POSTY',PADR('ADQTCFLD',10))
ENDIF
*C123847,1  TMI [End  ] 

BROWSE FIELDS &lcBrD1fld ;
       NOAPPEND ;
	   NOCLEAR  ;
  	   NODELETE ;
       NOMENU   ;
  	   NOWAIT   ;
	   SAVE     ;
  	   TITLE (lcBrTtl1) ;
       WHEN lfwBrow1() ;
       VALID :F lfvBrows() ;
	  WINDOW (lcBWinTl) ;
	  IN WINDOW (lcWinCh2)
SELECT (lcSelect)	


*!*************************************************************
*! Name      : lfvBrow2
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Transactions Browse function.
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
FUNCTION lfvBrow2

IF lnOrdTrans=1 AND ! laScrMode[1]
  ACTIVATE WINDOW (lcWinC22)
  RETURN
ENDIF

*B604435,1 KHM 06/26/2001 (Begin) Check if the relation was cleared because 
*B604435,1                of opening other screens.
SELECT (lcPOline)
IF EMPTY(SET('RELATION'))
   SET RELA TO Style+STR(LINENO,6) INTO (lcPOTran)
ENDIF
SELECT (lcPOTran)
IF EMPTY(SET('RELATION'))
  SET RELATION TO EVAL(lcPOTran+'.shipno') INTO Shpmthdr ADDITIVE
  
  *B605932,1 AMH Make relation with custom table for KRA10 [Start]
  IF ASCAN(laEvntTrig , PADR('MAKREL',10)) <> 0
    =gfDoTriger('POSTYLE',PADR('MAKREL',10))
  ENDIF
  *B605932,1 AMH [End]

ENDIF
*B604435,1 KHM 06/26/2001 (End)

lcSelect = SELECT()
SELECT (lcPOTran)
DO CASE
  CASE lnOrdTrans=3
    *B605768,1 KHM 04/01/2002 (Begin) Fix the spelling mistake.
    *lcBrTtl2 = SUBSTR(gcBaseWind,2,1)+'_Recieved Lines'
    *B607441,1 MMM 10/09/2003 Fix Bug of wrong title '_Received Lines' [Start]    
    *lcBrTtl2 = SUBSTR(gcBaseWind,2,1)+'_Received Lines'
     lcBrTtl2 = SUBSTR(gcBaseWind,2,1)+'_Issued Lines'
    *B607441,1 MMM [End]     
    *B605768,1 KHM 04/01/2002 (End)

    IF llQtyPSize
      *E300935,1 Add Dyelot field in the Browse
      *lcBrD2fld= [Style :30 :H='Style#':R,]+;
      *           [Qty1  :H='Size1':R,]+;
      *           [Qty2  :H='Size2':R,]+;
      *           [Qty3  :H='Size3':R,]+;
      *           [Qty4  :H='Size4':R,]+;
      *           [Qty5  :H='Size5':R,]+;
      *           [Qty6  :H='Size6':R,]+;
      *           [Qty7  :H='Size7':R,]+;
      *           [Qty8  :H='Size8':R,]+;
      *           [Date  :H='Date' :R]
      *B803415,1 (Begin) Add [E.T.A , Ship No, and Reference] fields to the browse.      
      *lcBrD2fld= [LineNo :4 :H='Line#' :R,]+;
                 [Style   :30 :H=lcStyHdr :R,]+;
                 IIF(llDyelot,[Dyelot :H= 'Dyelot' :R :20,],[])+;    
                 [Qty1  :H='Size1':R,]+;
                 [Qty2  :H='Size2':R,]+;
                 [Qty3  :H='Size3':R,]+;
                 [Qty4  :H='Size4':R,]+;
                 [Qty5  :H='Size5':R,]+;
                 [Qty6  :H='Size6':R,]+;
                 [Qty7  :H='Size7':R,]+;
                 [Qty8  :H='Size8':R,]+;
                 [Date  :H='Date' :R]
      lcBrD2fld= [LineNo :4 :H='Line#' :R,]+;
                 [Style   :30 :H=lcStyHdr :R,]+;
                 IIF(llDyelot,[Dyelot :H= 'Dyelot' :R :20,],[])+;    
                 [Qty1  :H='Size1':R,]+;
                 [Qty2  :H='Size2':R,]+;
                 [Qty3  :H='Size3':R,]+;
                 [Qty4  :H='Size4':R,]+;
                 [Qty5  :H='Size5':R,]+;
                 [Qty6  :H='Size6':R,]+;
                 [Qty7  :H='Size7':R,]+;
                 [Qty8  :H='Size8':R,]+;
                 [Date  :H='Date' :R,]+;
                 [Shpmthdr.Eta :H='E.T.A.'  :R,]+;
                 [ShipNo:H='Ship No.':R,]+;
                 [Reference :H='Reference':R]
      *B803415,1 (End)
    ELSE
      *E300935,1 Add Dyelot field in the Browse
      *lcBrD2fld= [Style  :30 :H='Style#':R,]+;
      *           [Date   :H='Date' :R,]+;
      *           [TotQty :H='Qty'  :R:P='9999999']
*      lcBrD2fld= [LineNo :4 :H='Line#' :R,]+;
                 [Style  :30 :H='Style#':R,]+;
                 IIF(llDyelot,[Dyelot :H= 'Dyelot' :R :20,],[])+;    
                 [Date   :H='Date' :R,]+;
                 [TotQty :H='Qty'  :R:P='9999999']
       *B803415,1 (Begin) Add [E.T.A , Ship No, and Reference] fields to the browse.
       *lcBrD2fld= [LineNo :4 :H='Line#' :R,]+;
                 [Style   :30 :H=lcStyHdr :R,]+;
                 IIF(llDyelot,[Dyelot :H= 'Dyelot' :R :20,],[])+; 
                 [Date   :H='Date' :R,]+;
                 [TotQty :H='Qty'  :R:P='9999999',]+;
                 [nELanCost1 :H='Landed Cost' :R:P='999999.999',]+;                 
                 [lnAmt=(TotQty*nELanCost1) :H='Amount'   :R:P='9999999.99']
       lcBrD2fld= [LineNo :4 :H='Line#' :R,]+;
                  [Style   :30 :H=lcStyHdr :R,]+;
                  IIF(llDyelot,[Dyelot :H= 'Dyelot' :R :20,],[])+; 
                  [Date   :H='Date' :R,]+;
                  [TotQty :H='Qty'  :R:P='9999999',]+;
                  [nELanCost1 :H='Landed Cost' :R:P='999999.999',]+;                 
                  [lnAmt=(TotQty*nELanCost1) :H='Amount'   :R:P='9999999.99',]+;
                  [Shpmthdr.Eta :H='E.T.A.'  :R,]+;
                  [ShipNo:H='Ship No.':R,]+;
                  [Reference :H='Reference':R]
       *B803415,1 (End)
    ENDIF
    
    SET FILTER TO TranCd='2'
  CASE lnOrdTrans=4
    lcBrTtl2 = SUBSTR(gcBaseWind,2,1)+'_Sales Orders'
    IF llQtyPSize
      *E301520,1 AME[Start] Add Fields of Account,Customer name, start and complete date    
      *lcBrD2fld= [Style   :30 :H=lcStyHdr :R,]+;
                 [Qty1  :H='Size1':R,]+;
                 [Qty2  :H='Size2':R,]+;
                 [Qty3  :H='Size3':R,]+;
                 [Qty4  :H='Size4':R,]+;
                 [Qty5  :H='Size5':R,]+;
                 [Qty6  :H='Size6':R,]+;
                 [Qty7  :H='Size7':R,]+;
                 [Qty8  :H='Size8':R,]+;
                 [Vendor :H='Order#':R]
                 
      lcBrD2fld= [Style   :30 :H=lcStyHdr :R,]+;
                 [Qty1  :H='Size1':R,]+;
                 [Qty2  :H='Size2':R,]+;
                 [Qty3  :H='Size3':R,]+;
                 [Qty4  :H='Size4':R,]+;
                 [Qty5  :H='Size5':R,]+;
                 [Qty6  :H='Size6':R,]+;
                 [Qty7  :H='Size7':R,]+;
                 [Qty8  :H='Size8':R,]+;
                 [Vendor :H='Order#':R,]+;
                 [Account  :H='Account' :R,]+;
                 [cAccName :H='Name' :R,]+;
                 [Start :H='Start Date'  :R,]+;
                 [Complete:H='Complete Date':R] 
      *E301520,1 AME[End]             

    ELSE
      *E301520,1 AME[Start] Add Fields of Account,Customer name, start and complete date    
      *lcBrD2fld= [Style    :30 :H=lcStyHdr :R,]+;
                 [Vendor   :H='Order#':R,]+;
                 [TotQty   :H='Qty'  :R:P='9999999',]
                 
      lcBrD2fld= [Style    :30 :H=lcStyHdr :R,]+;
                 [Vendor   :H='Order#':R,]+;
                 [TotQty   :H='Qty'  :R:P='9999999',]+;
                 [Account  :H='Account' :R,]+;
                 [cAccName :H='Name' :R,]+;
                 [Start :H='Start Date'  :R,]+;
                 [Complete:H='Complete Date':R] 
      *E301520,1 AME[End]             
    ENDIF
    SET FILTER TO TranCd='7'
  OTHE &&lnOrdTrans=2
    lcBrTtl2 = SUBSTR(gcBaseWind,2,1)+'_In-Transit'
    IF llQtyPSize

      *B802469,1 WAB  change browse style.date to E.A.T from shipemnt header
      *B802469,1 WAB - START
      *lcBrD2fld= [Style   :30 :H=lcStyHdr :R,]+;
      *           [Qty1  :H='Size1':R,]+;
      *           [Qty2  :H='Size2':R,]+;
      *           [Qty3  :H='Size3':R,]+;
      *           [Qty4  :H='Size4':R,]+;
      *           [Qty5  :H='Size5':R,]+;
      *           [Qty6  :H='Size6':R,]+;
      *           [Qty7  :H='Size7':R,]+;
      *           [Qty8  :H='Size8':R,]+;
      *           [Date :H='E.T.A.'  :R,]+;
      *           [ShipNo:H='Ship No.':R,]+;
      *           [cWareCode :H='Ship To' :R]
    *ELSE
      *lcBrD2fld= [lcInTrTTl=IIF(TranCd='6','Inter Location','Shipment') :20 :H='In-Transit',]+;
      *           [Style :30 :H=lcStyHdr  :R,]+;
      *           [Date :H='E.T.A.'  :R,]+;
      *           [ShipNo    :H='Ship No.':R,]+;                 
      *           [cWareCode :H='Ship To' :R,]+;
      *           [TotQty  :H='Qty'  :R:P='9999999']
      *B803415,1 (Begin) Add Reference field to the browse.
      *lcBrD2fld= [Style   :30 :H=lcStyHdr :R,]+;
                 [Qty1  :H='Size1':R,]+;
                 [Qty2  :H='Size2':R,]+;
                 [Qty3  :H='Size3':R,]+;
                 [Qty4  :H='Size4':R,]+;
                 [Qty5  :H='Size5':R,]+;
                 [Qty6  :H='Size6':R,]+;
                 [Qty7  :H='Size7':R,]+;
                 [Qty8  :H='Size8':R,]+;
                 [Shpmthdr.Eta :H='E.T.A.'  :R,]+;
                 [ShipNo:H='Ship No.':R,]+;
                 [cWareCode :H='Ship To' :R]
      lcBrD2fld= [Style   :30 :H=lcStyHdr :R,]+;
                 [Qty1  :H='Size1':R,]+;
                 [Qty2  :H='Size2':R,]+;
                 [Qty3  :H='Size3':R,]+;
                 [Qty4  :H='Size4':R,]+;
                 [Qty5  :H='Size5':R,]+;
                 [Qty6  :H='Size6':R,]+;
                 [Qty7  :H='Size7':R,]+;
                 [Qty8  :H='Size8':R,]+;
                 [Shpmthdr.Eta :H='E.T.A.'  :R,]+;
                 [ShipNo:H='Ship No.':R,]+;
                 [cWareCode :H='Ship To' :R,]+;
                 [Reference :H='Reference':R]
      *B803415,1 (End)                 
    ELSE
      *B803415,1 (Begin) Add Reference field to the browse.
      *lcBrD2fld= [lcInTrTTl=IIF(TranCd='6','Inter Location','Shipment') :20 :H='In-Transit',]+;
                 [Style :30 :H=lcStyHdr  :R,]+;
                 [Shpmthdr.Eta :H='E.T.A.'  :R,]+;
                 [ShipNo    :H='Ship No.':R,]+;      
                 [cWareCode :H='Ship To' :R,]+;
                 [TotQty  :H='Qty'  :R:P='9999999']
      lcBrD2fld= [lcInTrTTl=IIF(TranCd='6','Inter Location','Shipment') :20 :H='In-Transit',]+;
                 [Style :30 :H=lcStyHdr  :R,]+;
                 [Shpmthdr.Eta :H='E.T.A.'  :R,]+;
                 [ShipNo    :H='Ship No.':R,]+; 
                 [cWareCode :H='Ship To' :R,]+;
                 [TotQty  :H='Qty'  :R:P='9999999',]+;
                 [Reference :H='Reference':R]
     *B803415,1 (End)
     *B802469,1 WAB - END      

    ENDIF
    SET FILTER TO TranCd $ '36'

ENDCASE

*B605932,1 AMH Browse from custom table for KRA10 [Start]
IF ASCAN(laEvntTrig , PADR('BROWSE',10)) <> 0
  =gfDoTriger('POSTYLE',PADR('BROWSE',10))
ENDIF
*B605932,1 AMH [End]

HIDE WINDOW (lcWinC22)
BROWSE FIELDS &lcBrD2fld ;
       NOAPPEND ;
	   NOCLEAR  ;
  	   NODELETE ;
       NOMENU   ;
       NOWAIT   ;
	   SAVE     ;
	   TITLE (lcBrTtl2) ;
       WHEN lfwBrow2() ;
       VALID :F lfvBrows() ;
	WINDOW (lcWinC23) ;
	IN WINDOW (lcWinCh2)

SELECT (lcSelect)	


*!*************************************************************
*! Name      : lfwBrow1,lfwBrow2
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : When Browse function.
*!*************************************************************
FUNCTION lfwBrow1
*glFromBrow = .T.
lnMarker = RECNO()
*E300935,1 Now We can the same line with another dyelot thus we must add dyelot to the condition
*IF &lcPOLine..Style+STR(&lcPOLine..LineNo,6)<>m.Style+STR(m.LineNo,6)
IF &lcPOLine..Style+&lcPOLine..Dyelot+STR(&lcPOLine..LineNo,6)<>m.Style+m.Dyelot+STR(m.LineNo,6)
  =lfGetLine()
ENDIF

*E300935,1 enable / disable dyelot field [begin]
IF laScrMode[3] OR laScrMode[4]
  = lfAdjDye()
ENDIF  
*E300935,1 enable / disable dyelot field [end]
*E301291,1 AMM Get the button title
IF lcAType = 'A'
  lcConfig = IIF(SEEK("AD" + &lcPOLine..PO + STR(&lcPOLine..LineNo,6) , lcT_BomVar) , "\<Adornment" , "\<Configure")
ENDIF
SHOW GET pbConfig,1 PROMPT lcConfig 
*E301291,1 AMM end
*C200152,4 AMH custom trigger to check if the inter location PO
*C200152,4     found in approved or open batch [Start]
IF lcAtype = "N" AND laScrMode[3] .AND. ASCAN(laEvntTrig , PADR('DISBWARE',10)) <> 0
  =gfDoTriger('POINTRC',PADR('DISBWARE',10))
ENDIF
*C200152,4 AMH [End]

FUNCTION lfwBrow2
*glFromBrow = .T.
RETURN

*!*************************************************************
*! Name      : lfvBrows
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Valid Browse function.
*!*************************************************************
FUNCTION lfvBrows

IF WONTOP() # (lcBrTtl1) AND WONTOP() # (lcBrTtl2)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF


*!*************************************************************
*! Name      : lfGetLine
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Validate the line information.
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
FUNCTION lfGetLine

SELECT (lcPOLine)
SCATTER MEMVAR
lcStyDesc = IIF(SEEK(m.Style,'STYLE'),STYLE.Desc1,SPACE(20))
llFoundScl=SEEK('S'+m.Scale,'Scale')
lcSz1=IIF(llFoundScl,SCALE.SZ1,'')
lcSz2=IIF(llFoundScl,SCALE.SZ2,'')
lcSz3=IIF(llFoundScl,SCALE.SZ3,'')
lcSz4=IIF(llFoundScl,SCALE.SZ4,'')
lcSz5=IIF(llFoundScl,SCALE.SZ5,'')
lcSz6=IIF(llFoundScl,SCALE.SZ6,'')
lcSz7=IIF(llFoundScl,SCALE.SZ7,'')
lcSz8=IIF(llFoundScl,SCALE.SZ8,'')
lnLShpLoc=IIF(!EMPTY(m.Account),2,1)
IF ( laScrMode[3] OR laScrMode[4] ) AND lnActFolder=2
  IF lfCkEOF(lcPoLine) OR llZoom
    SHOW GETS WINDOW (lcWinC22) DISABLE ONLY
    IF !llZoom AND laData[3]<>'C'
      SHOW GET PBNEW ENABLE
    ENDIF
  ELSE
    IF laData[3]<>'C'
      SHOW GETS WINDOW (lcWinC22) ENABLE ONLY
    ELSE
      SHOW GETS WINDOW (lcWinC22) DISABLE ONLY  
    ENDIF
    IF ! llMultiWare
      SHOW GET lnLShpLoc   DISABLE
      SHOW GET pbLShpTo    DISABLE
      SHOW GET m.cWareCode DISABLE      
      SHOW GET m.Store     DISABLE      
    ENDIF
  ENDIF
  SHOW GET m.Style DISABLE
  SHOW GET ibStyle DISABLE

ELSE
  SHOW GETS WINDOW (lcWinC22) DISABLE ONLY
ENDIF
IF llImpCost AND lcAType='P' AND ( (laScrMode[3] AND laData[3]='H') OR laScrMode[4] )
  lcLnShMod = IIF(lfCkEOF(lcPoLine),'DISABLE','ENABLE')
  SHOW GET pbCTGnLns &lcLnShMod  
ENDIF
SHOW GET pbKey ENABLE
lnMarker = RECNO()
SHOW WINDOW (lcBrTtl1) REFRESH SAME
=lfRefresh(lcWinC22)


*!*************************************************************
*! Name      : lfvNew
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : New po line.
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
FUNCTION lfvNew

IF EMPTY(laData[2])
  *-You have to enter the xxx code.
  *E301291,1 AMM Consider the case of adornment order
  *=gfModalGen('TRM34020B34000','DIALOG',IIF(lcAType='N','source location','vendor')+'|'+' ')
  =gfModalGen('TRM34020B34000','DIALOG',IIF(lcAType$'NA','source location','vendor')+'|'+' ')
  *E301291,1 AMM end
  _CUROBJ = OBJNUM(laData[2])
  RETURN
ENDIF

IF !llMultiWare AND EMPTY(laData[19]) AND EMPTY(laData[69])
  *-You have to enter the xxx code.
  =gfModalGen('TRM34020B34000','DIALOG','ship to location or customer'+'|'+' ')
  _CUROBJ = IIF(lnShpLoc=1,OBJNUM(laData[19]),OBJNUM(laData[69]))
  RETURN
ENDIF

SELECT (lcPOLine)
lcLstSty = m.Style
SCATTER MEMVAR BLANK
IF lcAType='N'
  m.Style = lcLstSty
ENDIF
*E301291,1 AMM Update button title.
IF lcAType = 'A'
  lcConfig = "\<Configure"
ENDIF
SHOW GET pbConfig,1 PROMPT lcConfig 
*E301291,1 AMM end
STORE ' ' TO lcStyDesc
SHOW GETS WINDOW (lcWinC22) DISABLE ONLY
SHOW GET ibStyle ENABLE
SHOW GET m.Style ENABLE

*E300935,1 Enable Dyelot object [Begin]
*SHOW GET m.Dyelot ENABLE
*E300935,1 Enable Dyelot object [end]

=lfRefresh(lcWinC22)

*B803137,1 RAMY [START]
*B603709,1 Start, Added condition.
IF ! (lcAType = 'A' OR (llMultiWare AND lcAType = 'N'))
*B603709,1 End.
  m.Style = lcNexSty
  =SEEK(m.Style , 'STYLE')
ENDIF
_CUROBJ = OBJNUM(m.Style)
*B803137,1 RAMY [END]
RETURN


*!*************************************************************
*! Name      : lfvRemove
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Remove P/O line.
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
FUNCTION lfvRemove

llCantDele = .F.
lnAlias    = SELECT()
DO WHILE laScrMode[3] 
  IF !( laData[3] $ 'OHB' )
    *-Only open,hold or bid +lcPOType+ available to remove the line, cannot delete!
    =gfModalGen('TRM34032B34000','DIALOG',lcPOType )
    llCantDele = .T.
    EXIT
  ENDIF

  *C200152,4 AMH custom trigger to check if the inter location PO
  *C200152,4     found in open or approved batch [Start]
  IF lcAtype = "N" .AND. ASCAN(laEvntTrig , PADR('REMLINE',10)) <> 0
    =gfDoTriger('POINTRC',PADR('REMLINE',10))
    IF llCantDele
      EXIT
    ENDIF
  ENDIF
  *C200152,4 AMH [End]

  lcLineKey=lcAType+laData[1]+&lcPOLine..Style+STR(&lcPOLine..Lineno,6)
  SELECT POSLN
  *B603298,1 Remove the check of not allowing to remove the line if it was already issued.
  *IF SEEK(lcLineKey+'2') OR SEEK(lcLineKey+'4') OR SEEK(lcLineKey+'5') OR SEEK(lcLineKey+'6')
  IF SEEK(lcLineKey+'2') OR SEEK(lcLineKey+'4') OR SEEK(lcLineKey+'5')
  *B603298,1 End.
    *-Style for this line not allowed to delete. Shipment has been received.
    lcTxtMsg = IIF(POSLN.TranCd='6',' There is issued quantity',' Shipment has been received')
    =gfModalGen('TRM34033B34000','DIALOG',lcTxtMsg)
    llCantDele = .T.
    EXIT
  ENDIF 

  *B603298,1 Add check to approve the deletion if issued qty exist.
  IF lcAType $ 'NA' AND SEEK(lcLineKey+'6')
    *--This line has issued quantity, deleting this line will Reverse issued P/O line stock.
    *--Are you sure you want to delete? Yes/No.
    IF gfModalGen('INM34168B34001','DIALOG') = 2
      llCantDele = .T.
      EXIT
    ENDIF
  ENDIF
  *B603298,1 End.

  IF SEEK(lcLineKey+'3')
    *-Style for this line not allowed to delete. Shipment in transit.
    =gfModalGen('TRM34033B34000','DIALOG',' Shipment in transit')
    llCantDele = .T.
    EXIT
  ENDIF
  IF lfStyInBom(&lcPOLine..Lineno,laData[1])
    *-Receiving quantities have been found against this style. Cannot delete.
    =gfModalGen('TRM34034B34000','DIALOG')
    llCantDele = .T.
  ENDIF
  EXIT
ENDDO

*--If cannot delete this line.
IF llCantDele
  SELECT(lnAlias)  
  RETURN
ENDIF


*-Since this line has allocated quantities from order(s),
*-deleting it will release the order(s) allocation.
*-Are you sure you want to delete this line?
IF laScrMode[3] AND lcAType='P' AND SEEK('2'+laData[1]+&lcPOLine..Style,'CUTPICK')
  IF gfModalGen('QRM34035B34001','DIALOG') = 2
    RETURN 
  ENDIF
ELSE
*-Are you sure you want to delete this line?
  IF gfModalGen('QRM34036B34001','DIALOG') = 2
    RETURN
  ENDIF
ENDIF

laData[36] = laData[36] - m.TotQty
laData[40] = laData[40] - m.TotQty
llRecalTot = .T.
SELECT (lcPOLine)
*B607123,1  TMI [Start] 
PRIVATE lnRcno
lnRcno = 0
SKIP -1
IF BOF()
  GOTO TOP  
ELSE
  lnRcno = RECNO()
  SKIP
ENDIF
*B607123,1  TMI [End  ] 
BLANK
DELETE
*B607123,1  TMI [Start] if line deleted go to the immediatly previous one , not to the top of file
*GO TOP
IF lnRcno = 0  
  GOTO TOP
ELSE
  GOTO (lnRcno)
ENDIF  
*B607123,1  TMI [End  ]
*B602884,1 if no lines in return remove P/o # of return P/O.
IF lcAType='R' AND EOF()
  laData[71] = SPACE(6)
ENDIF  
*B602884,1 end.
*E301291,1 AMM Update the status field in case of adornment order
IF lcAType = 'A'
  
  *C102357,1 AMH If Customer J&L create don't delete. [Start]
  *SELECT (lcT_BomVar)
  *=SEEK("AD"+laData[1]+STR(&lcPOLine..LineNo,6))
  *SCAN REST WHILE cIdType+cCost_Id+STR(LineNo,6) = "AD" + laData[1] + STR(&lcPOLine..LineNo,6)
  *  REPLACE cStatus WITH SUBSTR('DDS',AT(cStatus,'SMA'),1)
  *ENDSCAN
  *DELETE ALL FOR cIdType+cCost_Id+STR(LineNo,6) = "AD" + laData[1] + STR(&lcPOLine..LineNo,6)
  IF ASCAN(laEvntTrig , PADR('DOSCRN',10)) <> 0
    *-- Do nothing validation is in JLMAIN
  ELSE
    SELECT (lcT_BomVar)
    =SEEK("AD"+laData[1]+STR(&lcPOLine..LineNo,6))
    SCAN REST WHILE cIdType+cCost_Id+STR(LineNo,6) = "AD" + laData[1] + STR(&lcPOLine..LineNo,6)
      REPLACE cStatus WITH SUBSTR('DDS',AT(cStatus,'SMA'),1)
    ENDSCAN
    DELETE ALL FOR cIdType+cCost_Id+STR(LineNo,6) = "AD" + laData[1] + STR(&lcPOLine..LineNo,6)
  ENDIF     
  *C102357,1 AMH [End]

ENDIF
*E301291,1 AMM end

=lfvBrow1()    
=lfGetLine()
SELECT (lcPOHdr)
GATHER FROM laData FIELDS &lcScFields
SELECT (lcPOLine)
RETURN


*!*************************************************************
*! Name      : lfvStyle
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Validate New style
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
FUNCTION lfvStyle
*B037225,1 NNA 01/06/2004 (Begin) Private Variable to get the extended Size Scale Width
PRIVATE lnExtSzWd
*B037225,1 NNA (End)
*B803346,1 (Begin) If the user wants to browse styles by pressing the 
*B803346,1         the browse button then return.
IF MDOWN() .AND. !llBrowse
  RETURN
ENDIF
*B803346,1 (End)

m.Style=IIF(llBrowse,'?',m.Style)
SELECT STYLE
SET ORDER TO TAG StyQlty
lcQltyKey=laStyGrd[lnGrade,2]
*E301274,1 AMM (start) Get the picture of style
lnItemSz   = gfItemMask('PI')
*IF llBrowse OR (!EMPTY(m.Style) AND !SEEK(lcQltyKey+m.Style,'STYLE'))
*B803346,1 (Begin) Fix the bug of thje style being not empty because of the separator.
*IF llBrowse OR !EMPTY(m.Style) 

llBrowseEx = llbrowse
IF llBrowse OR SUBSTR(m.Style,1,1) <> SPACE(1)
*B803346,1 (End)
  *E301274,1 AMM end
  llbrowse = .F.
  *B037225,1 NNA 01/06/2004 (Begin) Get the extended Size Scale Width
  lnExtSzWd = gfGetMemVar('M_EXTWIDTH')
  *B037225,1 NNA (End)
  *E301274,1 AMM (start) seek on the entered style in the style file
  **E302035,4 ABD - Handel the EX Size Scale. [Begin]
  *IF  !SEEK(lcQltyKey+ALLTRIM(m.Style),'STYLE')
  IF  !SEEK(lcQltyKey+ALLTRIM(m.Style),'STYLE') .AND. !(gfGetMemVar('M_USEEXSSC'))
    **E302035,4 ABD - [End]
  *E301274,1 AMM end
    SET FILTER TO cStyGrade=lcQltyKey
    GO TOP
    IF EOF()
      *--No styles found for this quality. 
      =gfModalGen('TRM42087B42000','DIALOG')
      m.Style = SPACE(19)
    ELSE
      lcHdEsc1= ON('KEY','ESC')
      ON KEY LABEL ESC 
      m.Style = gfStyBrw('I',m.Style,'',.F.)
      ON KEY LABEL ESC &lcHdEsc1
      =lfvBrow1()
    ENDIF 
    SELECT STYLE
    SET FILTER TO 
  *E301274,1 AMM (start) 
  ELSE
    *E301274,1 AMM If the user typed part of the style, call the extended size scale 
    *E301274,1 AMM style line screen .
    *IF m.STYLE # STYLE.STYLE .AND. gfGetMemVar('M_USEEXSSC') .AND. ;
    *       LEN(ALLTRIM(m.STYLE)) = LEN(lnItemSz)-3
    
    *B606914,1 KHM 02/03/2003 (Begin) Adding the checking of using extended size scale screen
    *IF (m.STYLE # STYLE.STYLE .AND. gfGetMemVar('M_USEEXSSC') .AND. ;
           LEN(ALLTRIM(m.STYLE)) = LEN(lnItemSz)-3) .OR. m.STYLE = '?'
    IF llUseExSSc AND ((m.STYLE # STYLE.STYLE .AND. gfGetMemVar('M_USEEXSSC') .AND.  ;
           LEN(ALLTRIM(m.STYLE)) = LEN(lnItemSz)-3) .OR. m.STYLE = '?')
    *B606914,1 KHM 02/03/2003 (End)
        
      **E302035,4 ABD - Call the browse before call the screen. [Begin]
      *-- Check if More Than 1 record.
      IF SEEK(SUBSTR(Style.Scale,1,lnScaleLen),'SCALEHD') .AND. scalehd.nNofCodes = 1
        m.Style= Style.Style
        m.Style = gfStyBrw('I',m.Style,'',.F.)
        =lfvBrow1()
      ELSE
        lcHdEsc1= ON('KEY','ESC')
        ON KEY LABEL ESC 
        *--- SSH
        IF ("?" $ m.Style ) .OR. !SEEK(lcQltyKey+LEFT(m.style,LEN(m.Style)-3))
          m.Style = LEFT(gfStyBrw('I',m.Style,'',.F.),16)
        ENDIF
        *--- SSH
        ON KEY LABEL ESC &lcHdEsc1
        IF EMPTY(m.Style)
          RETURN
        ELSE
          *-- In Case Not Empty & has one line only.
          IF SEEK(SUBSTR(Style.Scale,1,lnScaleLen),'SCALEHD') .AND. scalehd.nNofCodes = 1
            m.Style= Style.Style
            m.Style = gfStyBrw('I',m.Style,'',.F.)
            =lfvBrow1()
          ELSE
            **E302035,4 ABD - [End]
            lnBrR    = 0
            *E301274,1 AMM Create the numeric fields array, to make character fields 
            *E301274,1 AMM corresponding to them
            DIMENSION laQtyArr[17,3]
            laQtyArr[1,1]   = 'QTY1'
            laQtyArr[1,2]   = 6
            laQtyArr[1,3]   = 0
            laQtyArr[2,1]   = 'QTY2'
            laQtyArr[2,2]   = 6
            laQtyArr[2,3]   = 0
            laQtyArr[3,1]   = 'QTY3'
            laQtyArr[3,2]   = 6
            laQtyArr[3,3]   = 0
            laQtyArr[4,1]   = 'QTY4'
            laQtyArr[4,2]   = 6
            laQtyArr[4,3]   = 0
            laQtyArr[5,1]   = 'QTY5'
            laQtyArr[5,2]   = 6
            laQtyArr[5,3]   = 0
            laQtyArr[6,1]   = 'QTY6'
            laQtyArr[6,2]   = 6
            laQtyArr[6,3]   = 0
            laQtyArr[7,1]    = 'QTY7'
            laQtyArr[7,2]   = 6
            laQtyArr[7,3]   = 0
            laQtyArr[8,1]   = 'QTY8'
            laQtyArr[8,2]   = 6
            laQtyArr[8,3]   = 0
            laQtyArr[9,1]   = 'TOTQTY'
            laQtyArr[9,2]   = 7
            laQtyArr[9,3]   = 0
            laQtyArr[10,1]  = 'GROS_PRICE'
            laQtyArr[10,2]  = 8
            laQtyArr[10,3]  = 2
            laQtyArr[11,1]  = 'DISC_PCNT'
            laQtyArr[11,2]  = 4
            laQtyArr[11,3]  = 2
            laQtyArr[12,1]  = 'NCOST1'
            laQtyArr[12,2]  = 9
            laQtyArr[12,3]  = 2
            laQtyArr[13,1]  = 'PPQTY'
            laQtyArr[13,2]  = 4
            laQtyArr[13,3]  = 0
            laQtyArr[14,1]  = 'NCOST2'
            laQtyArr[14,2]  = 9
            laQtyArr[14,3]  = 2
            laQtyArr[15,1]  = 'NCOST3'
            laQtyArr[15,2]  = 9
            laQtyArr[15,3]  = 2
            laQtyArr[16,1]  = 'NCOST4'
            laQtyArr[16,2]  = 9
            laQtyArr[16,3]  = 2
            laQtyArr[17,1]  = 'NCOST5'
            laQtyArr[17,2]  = 9
            laQtyArr[17,3]  = 2 

            DIMENSION laTitArr[16,2]
            laTitArr[1,1] = 'STYLE'
            laTitArr[1,2] = lcStyHdr
            laTitArr[2,1] = 'REFERENCE'
            laTitArr[2,2] = 'Refer'
            laTitArr[3,1] = 'CVENSTY'
            laTitArr[3,2] = 'Ven. Sty.'
            laTitArr[4,1] = 'CPrepak'
            laTitArr[4,2] = 'P'
            laTitArr[5,1] = 'CPPQTY'
            laTitArr[5,2] = 'P. Qty.'
            laTitArr[6,1] = 'CGROS_PRICE'
            laTitArr[6,2] = 'GPrice'
            laTitArr[7,1] = 'CDISC_PCNT'
            laTitArr[7,2] = 'Disc'
            laTitArr[8,1] = 'CNCOST1'
            laTitArr[8,2] = 'Price'
            laTitArr[9,1] = 'cVenSty'
            laTitArr[9,2] = 'Vendor Style'
            laTitArr[10,1] = 'Reference'
            laTitArr[10,2] = 'Reference'
            laTitArr[11,1] = 'CWarCust'
            laTitArr[11,2] = 'S'
            laTitArr[12,1] = 'cWareCode'
            laTitArr[12,2] = 'ShipTo'
            laTitArr[13,1] = 'CNCOST2' 
            laTitArr[13,2] = lcISlbl2
            laTitArr[14,1] = 'CNCOST3'
            laTitArr[14,2] = lcISlbl3
            laTitArr[15,1] = 'CNCOST4'
            laTitArr[15,2] = lcISlbl4
            laTitArr[16,1] = 'CNCOST5'
            laTitArr[16,2] = lcISlbl5
            LNP='999999'
            lcg='9999999.99'
            lcg1='999999.99'
            
            *E302035,4 ABD - Remark the next line and browse fields with new arrange. [Begin]
            *lcBrowStr = "cMk=IIF(RECNO()=lnBrR,'',' '):1:H=' ':W=.F.,Style:H='STYLE':R,"+;
            *  "Reference:H='Reference',cVenSty:H='Vendor Sty.',"+;
            *  IIF(llDyelot .AND. STYLE.cDye_Flg = 'Y',"Dyelot :H='Dyelot':P='@!':W=lfwOV():V=lfvDyelot(.T.),",'')+;
            *  IIF(llMultiWare,"cWarCust :H='SHipTo L/C':P='@!':V=lfvPOPUP(), "+;
            *  "cWareCode:H='Ship To':W=lfwOV():V=lfvF(),Store:H='Store':W=lfwOV():V=lfvF(),",'')+;
            *  "Prepak:H='P':W=lfwOV():V=lfvF():P='!',cPpQty:H='P. Qty.':W=lfwOV():V=lfvF():P='9999',cTotQty:H='Pieces':W=lfwOV():P='9999999':V=lfvF()," 

            *lcBrowStr = lcBrowStr +;
            *  "cQty1:H='Qty1':V=lfvF():P=LNP,cQty2:H='Qty2':W=laScl[2]:V=lfvF():P=LNP,cQty3:H='Qty3':W=laScl[3]:V=lfvF():P=LNP,"+;
            *  "cQty4:H='Qty4':W=laScl[4]:V=lfvF():P=LNP,"
            *lcBrowStr = lcBrowStr +;
            *  "cQty5:H='Qty5':W=laScl[5]:V=lfvF():P=LNP,cQty6:H='Qty6':W=laScl[6]:V=lfvF():P=LNP,"+;
            *  "cQty7:H='Qty7':W=laScl[7]:V=lfvF():P=LNP,cQty8:H='Qty8':W=laScl[8]:V=lfvF():P=LNP," +;
            *  "cGros_Price:H='GPrice':W=lfwOV():V=lfvF():P=lcg1,"  +;
            *  "CDISC_PCNT:H='Disc':W=lfwOV():V=lfvF():P='99.99',cNCost1:H='Price':W=lfwOV():V=lfvF():P=lcg" +;
            *  ",cNCost2:H=lcISlbl2:W=lfwOV():V=lfvF():P=lcg,cNCost3:H=lcISlbl3:W=lfwOV():V=lfvF():P=lcg,cNCost4:H=lcISlbl4:W=lfwOV():V=lfvF():P=lcg,cNCost5:H=lcISlbl5:W=lfwOV():V=lfvF():P=lcg"
      
            lcBrowStr = "cMk=IIF(RECNO()=lnBrR,'',' '):1:H=' ':W=.F.,ScaleDes :H='SclDes':R:W=.F.," +;
            "cTotQty:H='TOT QTY':W=lfwOV():P='9999999':V=lfvF(),"
            
            lcBrowStr = lcBrowStr +;
            "cQty1:H='Qty1':V=lfvF():P=LNP, cQty2:H='Qty2':W=laScl[2]:V=lfvF():P=LNP,cQty3:H='Qty3':W=laScl[3]:V=lfvF():P=LNP,cQty4:H='Qty4':W=laScl[4]:V=lfvF():P=LNP," 
            
            *--- SSH 
            *lcBrowStr = lcBrowStr +;
            "cQty5:H='Qty5':W=laScl[5]:V=lfvF():P=LNP,cQty6:H='Qty6':W=laScl[6]:V=lfvF():P=LNP,cQty7:H='Qty7':W=laScl[7]:V=lfvF():P=LNP,cQty8:H='Qty8':W=laScl[8]:V=lfvF():P=LNP," +;
            "Gros_Price:H='GPrice':W=lfwOV():V=lfvF(),"  +;
            "DISC_PCNT:H='Disc':W=lfwOV():V=lfvF() ,Price:H='Price':W=lfwOV():V=lfvF()"
            
            lcBrowStr = lcBrowStr +;
            "cQty5:H='Qty5':W=laScl[5]:V=lfvF():P=LNP,cQty6:H='Qty6':W=laScl[6]:V=lfvF():P=LNP,cQty7:H='Qty7':W=laScl[7]:V=lfvF():P=LNP,cQty8:H='Qty8':W=laScl[8]:V=lfvF():P=LNP," +;
            "Gros_Price:H='GPrice':W=lfwOV():V=lfvGPrice(.T.),"  +;
            "DISC_PCNT:H='Disc':W=lfwOV():V=lfvPrcDisc(.T.) ,nCost1:H='Net Price':W=lfwOV():V=lfvCost('1',.F.,.T.)"

            *--- SSH
            *E302035,4 ABD - [End]
   
            *E301274,1 AMM Run the screen to edit all styles begin with what the user typed
            *E301274,1 AMM Parameters:
            *E301274,1 AMM 1- m.style -> the part of the style the user typed
            *E301274,1 AMM 2- Store , if the order is multi store
            *E301274,1 AMM 3- temporary file the SO screen work with.
            *E301274,1 AMM 4- Browse string
            *E301274,1 AMM 5- Seek expression
            *E301274,1 AMM 6- Quantity array (Array holds the numeric fields in the called screen browse )
            *E301274,1 AMM 7- Title array (each field with its title in the called screen browse)
       
            *B604010,1 KHM 11/02/2000 (Begin) Changing the calling by removing
            *B604010,1                the slash before SOESSQTY.
             *DO (gcAppHome+'\SOESSQTY.FXP') WITH ALLTRIM(m.Style),m.store,;
             lcPOLine, lcBrowStr,'',laQtyArr,laTitArr,'PO'

		     *B037225,1 NNA 01/06/2004 (Begin) cut one chr.(that represent the No. of scale) from m.style
		     *B037225,1 NNA                    to brow the all scales for this style 
             *DO (gcAppHome+'SOESSQTY.FXP') WITH ALLTRIM(m.Style),m.store,;
             *lcPOLine, lcBrowStr,'',laQtyArr,laTitArr,'PO'
             DO (gcAppHome+'SOESSQTY.FXP') WITH LEFT(m.style,LEN(ALLTRIM(m.style))-(3-lnExtSzWd)),m.store,;
             lcPOLine, lcBrowStr,'',laQtyArr,laTitArr,'PO'
             *B037225,1 NNA           (End) 
            *B604010,1 KHM 11/02/2000 (End)
            *B608125,1 TMI [Start] Update costing 
            =lfCstShUpd()
            *B608125,1 TMI [End  ] 
            =lfRefresh(lcWinC22)
            
            *B606914,1 KHM 02/03/2003 (Begin) Enabling the correct objects.
            *SHOW GETS WINDOW (lcWinC22) DISABLE ONLY            
            SHOW GETS WINDOW (lcWinC22) ENABLE ONLY
            IF ! llMultiWare
              SHOW GET lnLShpLoc   DISABLE
              SHOW GET pbLShpTo    DISABLE
              SHOW GET m.cWareCode DISABLE      
              SHOW GET m.Store     DISABLE      
            ENDIF
            SHOW GET m.Style DISABLE
            SHOW GET ibStyle DISABLE
            *B606914,1 KHM 02/03/2003 (End)

            **E302035,4 ABD - refresh the browse and stop at the first record.[Begin]
            *--- SSH
            SELECT Style
            SET ORDER TO TAG Style
            *--- SSH

            SELECT (lcPOLine)
            LOCATE
            =lfwBrow1()
            
            *B606914,1 KHM 02/03/2003 (Begin) Refresh the browse
            SHOW WINDOW (lcBrTtl1) REFRESH SAME
            *B606914,1 KHM 02/03/2003 (End)
            **E302035,4 ABD - [End]
            SHOW GET pbNew ENABLE
            _CUROBJ = OBJNUM(pbNew)
            RETURN
            **E302035,4 ABD - End if for if statment [Begin]
          ENDIF
        ENDIF
      ENDIF
      **E302035,4 ABD - [End]
    ELSE
      SET FILTER TO cStyGrade=lcQltyKey
      GO TOP
      IF EOF()
        *--No styles found for this quality. 
        =gfModalGen('TRM42087B42000','DIALOG')
        m.Style = SPACE(19)
      ELSE
        lcHdEsc1= ON('KEY','ESC')
        ON KEY LABEL ESC 
        *B603182,1 AMM start
        *m.Style = gfStyBrw('I',m.Style,'',.F.)
        *B603182,1 AMM In case of dye order and the user typed the major only
        IF lcAType = 'D' .AND. m.STYLE # STYLE.STYLE .AND. ;
             LEN(ALLTRIM(m.STYLE)) = LEN(lnItemSz) - LEN(laMajSeg[lnColStru,3]) 
          *B603182,1 AMM If the user chose the dye color, set it as nonmajor part
          IF laData[72]#'*' .AND. SEEK(lcQltyKey+ALLTRIM(m.STYLE)+laData[72],'STYLE')
            m.STYLE = ALLTRIM(m.STYLE)+laData[72]
          ELSE
            *B603182,1 AMM If user chose ALL colors, browse nonmajors of that major only
            lcNonMaj = '?'
            lcNonMaj = gfStyBrw('N', LEFT(ALLTRIM(m.STYLE),lnMjrWid),lcNonMaj,.F.)
            *B603182,1 AMM If the user choose a NonMajor add it to style variable else empty it.
            IF !EMPTY(lcNonMaj)
              m.Style = ALLTRIM(m.STYLE)+lcNonMaj
            ELSE
              m.Style = SPACE(19)
            ENDIF
          ENDIF
        ELSE
          m.Style = gfStyBrw('I',m.Style,'',.F.)
        ENDIF
        *B603182,1 AMM end
        
        ON KEY LABEL ESC &lcHdEsc1
        =lfvBrow1()
      ENDIF 
      SELECT STYLE
      SET FILTER TO 
    ENDIF
  ENDIF
  *E301274,1 AMM end
ENDIF
SET ORDER TO TAG Style
lcStyDesc = IIF(!EMPTY(m.Style) AND !EOF('STYLE'),STYLE.Desc1,'')
SHOW GET lcStyDesc

SELECT POSHDR
*B803346,1 (Begin) If the user clears the style i.e he wants to end selecting lines as 26 then don'y browse
*B803346,1         styles because of the seperator.
*IF EMPTY(m.Style)
IF SUBSTR(m.Style,1,1) = SPACE(1)
*B803346,1 (End)
  RETURN
ENDIF

llAbort =.F.
*-- Validation checks loop for Style.....
DO WHILE .T.
  *C200080,1 AMM (START)  Accept the manufacturing styles in case of dye orders
  *IF ! STYLE.Make
    *IF lcAType='P'
  *E301291,1 AMM Consider the case of adornment order
  *IF IIF(lcAType='D',STYLE.Make,!STYLE.Make)

  *C102357,1 AMH Use purchesed styles in case of JL [Start]
  *IF IIF(lcAType$'DA',STYLE.Make,!STYLE.Make)
  IF IIF(lcAType$'DA' .AND. ASCAN(laEvntTrig , PADR('DOSCRN',10)) = 0,STYLE.Make,!STYLE.Make)
  *C102357,1 AMH [End]
    STORE 0 TO m.nCost1,m.nCost2,m.nCost3,m.nCost4,m.nCost5,m.Gros_Price
  *E301291,1 AMM end
    *C200080,1 AMM If the user chose dye color, selected styles must match it.
    IF lcAType = 'D' .AND. !EMPTY(laData[72]) .AND. laData[72]#'*' AND lnColStru # 0 .AND. ;
              SUBSTR(STYLE.STYLE,laMajSeg[lnColStru,4],LEN(laMajSeg[lnColStru,3])) # laData[72]
      *C200080,1 AMM Message : "Style color doesn't match dye color, cannot accept" 
      =gfModalGen('TRM38174B00000','DIALOG')
      llAbort=.T.
      EXIT
    ENDIF
    *C200080,1 AMM Add the check to Dye order as the normal PO
    IF lcAType$'DP'
  *C200080,1 AMM end
      *E301077,8 MAB Open BOM file here [Begin
      IF !llOpnBom
        llOpnBom = lfOpn_Rest(gcDataDir+'BOM','BOM')
      ENDIF  
      *E301077,8 MAB Open BOM file here [End..

      IF !(laData[3]$'HB')
        IF SEEK(SUBSTR(m.Style,1,lnMjrWid),'BOM')
          SELECT BOM 
          LOCATE REST WHILE cItmMajor=SUBSTR(m.Style,1,lnMjrWid) ;
                 FOR LIKE(STRTRAN(cItmMask,'*','?'),PADR(m.Style,19))
          IF !FOUND()
            *-No cost lines found in the cost sheet, Cannot proceed!
            =gfModalGen('TRM34037B34000','DIALOG')        
            llAbort=.T.
            SELECT POSHDR
            EXIT
          ENDIF
        ELSE
          *-No cost lines found in the cost sheet, Cannot proceed!
          =gfModalGen('TRM34037B34000','DIALOG')        
          llAbort=.T.
          EXIT
        ENDIF
      ELSE 
        IF !SEEK(SUBSTR(m.Style,1,lnMjrWid),'BOM')
          *C200080,1 AMM If the style has no cost sheet, don't continue in case of dye order
          *-Warrning ! This style has no cost sheet.
          *=gfModalGen('INM34038B34000','DIALOG')        
          IF lcAType = 'D' 
            *-No cost lines found in the cost sheet, Cannot proceed!
            =gfModalGen('TRM34037B00000','DIALOG')        
            llAbort=.T.
            EXIT
          ELSE
            =gfModalGen('INM34038B34000','DIALOG')        
          ENDIF
        ELSE
          *C200080,1 AMM check if the style has the dye operation in its cost sheet
          *C200098,1 RAMY Comment these lines to move them 
          *IF lcAType = 'D'
          *  SELECT BOM
          *  LOCATE FOR mfgcode=lcDyeOpr WHILE citmmajor=SUBSTR(m.Style,1,lnMjrWid)
          *  IF !FOUND()
          *    *C200080,1 AMM Message : 'Style cost sheet has no dye operation ['+lcDyeOpr+'], Cannot accept this style in Dye order.'
          *    =gfModalGen('TRM38177B00000','DIALOG',lcDyeOpr)
          *    *C200080,1 AMM end
          *    llAbort=.T.
          *    EXIT
          *  ENDIF
          *ENDIF
        *C200080,1 AMM end
        ENDIF 
      ENDIF
    ENDIF
   
  ELSE 
    *-Conflict ! only purchased styles are allowed to enter here, Cannot proceed!
    *C200080,1 AMM Adjust the message in case of dye order
    *=gfModalGen('TRM34039B34000','DIALOG')
    *E301291,1 AMM Consider the case of adornment order
    *IF lcAType = 'D'
    *C102357,1 AMH Use purchesed styles in case of JL [Start]
    *IF lcAType $ 'DA'
    IF lcAType $ 'DA' .AND. ASCAN(laEvntTrig , PADR('DOSCRN',10)) = 0
    *C102357,1 AMH [End]
    *E301291,1 AMM end
      =gfModalGen('TRM38015B34000','DIALOG','manufactured styles')
    ELSE
      =gfModalGen('TRM34039B34000','DIALOG')
    ENDIF
    *C200080,1 AMM end
    llAbort=.T.
    EXIT
  ENDIF
  
  IF Style.cDivision <> ALLTRIM(laData[4])
    *-Conflict ! styles restricted to division XXXX, Cannot proceed!
    =gfModalGen('TRM34041B34000','DIALOG',ALLTRIM(laDiv[lnDiv,1]) )        
    llAbort=.T.
    EXIT
  ENDIF

  IF !EMPTY(Style.cPurCode) AND Style.cPurCode <> ALLTRIM(laData[67])
    *-Conflict ! styles restricted to purchase XXXX, Cannot proceed!
    =gfModalGen('TRM34110B34000','DIALOG',ALLTRIM(laPurCode[lnPurCode,1]) )
    llAbort=.T.
    EXIT
  ENDIF

  IF STYLE.Status='X'
    *-This is a canceled style. Not allowed to enter here, Cannot proceed!
    =gfModalGen('TRM34040B34000','DIALOG')        
    llAbort=.T.
    EXIT
  ENDIF
  *C200098,1 RAMY Add these lines to chek if the style has a dye operation 
  *C200098,1 RAMY in the cost sheet or not , if so get it`s no. [start]
  IF lcAType = 'D'
    SELECT BOM
    =SEEK(SUBSTR(m.Style,1,lnMjrWid),'BOM')
    LOCATE FOR mfgcode=lcDyeOpr WHILE citmmajor=SUBSTR(m.Style,1,lnMjrWid)
    IF !FOUND()
      *C200080,1 AMM Message : 'Style cost sheet has no dye operation ['+lcDyeOpr+'], Cannot accept this style in Dye order.'
      =gfModalGen('TRM38177B00000','DIALOG',lcDyeOpr)
      *C200080,1 AMM end
      llAbort=.T.
      EXIT
    ENDIF
    lcDyeOpNo = BOM.TYP
  ENDIF
  *C200098,1 RAMY [end]
  *C200080,1 AMM end

  IF lcAType='C' AND lfVldSCnr()
    *-The Style: xxx valid in another contract for the same vendor and period, Cannot proceed!
    =gfModalGen('TRM34018B34000','DIALOG',m.Style)
    llAbort=.T.
    EXIT
  ENDIF
  *E301291,1 AMM Consider the case of adornment order
  *IF lcAType='N' AND !SEEK(PADR(m.Style,19)+PADR(laData[2],6)+SPACE(10),'STYDYE')
  IF lcAType$'NA' AND !SEEK(PADR(m.Style,19)+PADR(laData[2],6)+SPACE(10),'STYDYE')
  *E301291,1 AMM end
    *E301264,1 If system setup to point of sale don't allow to add location.
    IF llPosSetN
      *-Style: xxx is not assigned to location: xxx. "ok"
      =gfModalGen('TRM34048B34000','DIALOG',ALLTRIM(m.Style)+'|'+ALLTRIM(laData[2]))
      llAbort=.T.
      EXIT
    ELSE    
    *E301264,1 End.
    
      *-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
      IF gfModalGen('QRM34048B34004','DIALOG',ALLTRIM(m.Style)+'|'+ALLTRIM(laData[2])) = 1
        DO gpAdStyWar WITH m.Style,SPACE(10),laData[2]
      ELSE
        llAbort=.T.
        EXIT
      ENDIF
    ENDIF
  ENDIF

  IF !llMultiWare
    *--If ship to customer the location will be the ship drop location.
    m.cWareCode = IIF(lnShpLoc = 2,laData[69],laData[19])
    lnLShpLoc   = lnShpLoc
    m.Account   = IIF(lnShpLoc = 2,laData[69],'')
    m.Store     = laData[70]

    IF lnShpLoc = 1
      IF !SEEK(PADR(m.Style,19)+PADR(m.cWareCode,6)+SPACE(10),'STYDYE')
        *-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
        IF gfModalGen('QRM34048B34004','DIALOG',ALLTRIM(m.Style)+'|'+m.cWareCode) = 1
          DO gpAdStyWar WITH m.Style,SPACE(10),m.cWareCode
        ELSE
          llAbort=.T.
          EXIT
        ENDIF
      ENDIF
    
    ELSE  && lnShpLoc = 2
      IF !SEEK(PADR(m.Style,19)+lcDropLoc+SPACE(10),'STYDYE')
        *-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
        IF gfModalGen('QRM34048B34004','DIALOG',ALLTRIM(m.Style)+'|'+lcDropLoc) = 1
          DO gpAdStyWar WITH m.Style,SPACE(10),lcDropLoc
        ELSE
          llAbort=.T.
          EXIT
        ENDIF
      ENDIF

    ENDIF
  ELSE
    IF llWareHous
      *E301291,1 AMM Consider the case of adornment order
      *m.cWareCode = IIF(lcAType='N' AND PADR(laData[2],6)=STYLE.CDefWare,SPACE(6),STYLE.CDefWare)
      m.cWareCode = IIF(lcAType$'NA' AND PADR(laData[2],6)=STYLE.CDefWare,SPACE(6),STYLE.CDefWare)
      *E301291,1 AMM end
    ELSE  
      lnLShpLoc = 2
      m.cWareCode = SPACE(5)
    ENDIF
  ENDIF

  EXIT
ENDDO

IF llAbort
  m.Style   = SPACE(lnStyleWid)
  lcStyDesc = SPACE(0)
  =lfRefresh(lcWinC22)
  SHOW GETS WINDOW (lcWinC22) DISABLE ONLY
  SHOW GET pbNew ENABLE
  _CUROBJ = OBJNUM(pbNew)
  RETURN
ENDIF

SELECT (lcPOLine)
NewLine = .T.
*--Donnot check existance of the style if Inter Location P/o and multi shipto 
*--location ,becouse normally is to send same style to different locations.
*E301291,1 AMM Add adornment order type 'A'
*IF !(lcAType='N' AND llMultiWare AND llMultiWare)
IF !(lcAType$'NA' AND llMultiWare)
*E301291,1 AMM end
  IF SEEK(m.Style)
    *-Warning: This Style has been entered on this '+lcPOType+'!
    =gfModalGen('INM34042B34000','DIALOG',lcPOType)        
  ENDIF
ENDIF

SELECT (lcPOLine)
m.cStyType = lcAType
m.Trancd = '1'
lnLines  = lnLines + 1
m.LineNo = lnLines
m.Po     = laData[1]
m.Vendor = laData[2]
m.Scale  = Style.Scale

*HDM Make it hold buy prepack as default ==>m.PrePak = Style.PrePak
m.PrePak = Style.CbuyPrePk
m.cVenSty= Style.cVenSty

IF lcAtype='C' .OR. !lfFrmCont()
  *E301291,1 AMM Consider the case of adornment order
  IF !(lcAType $ 'AD')
  *E301291,1 AMM end
    m.nCost1 = IIF(!llMulCurr OR STYLE.cPriceCur=laData[26],Style.nICost1,0)
    m.nCost2 = IIF(!llMulCurr OR STYLE.cDutyCur =laData[27],Style.nICost2,0)
    m.nCost3 = IIF(!llMulCurr OR STYLE.cDutyCur =laData[27],Style.nICost3,0)
    m.nCost4 = IIF(!llMulCurr OR STYLE.cDutyCur =laData[27],Style.nICost4,0)
    m.nCost5 = IIF(!llMulCurr OR STYLE.cDutyCur =laData[27],Style.nICost5,0)

    *B602968,1 Start.
    IF !llMulCurr OR STYLE.cPriceCur=laData[26]
      m.Disc_Pcnt  = STYLE.Disc_Pcnt
      m.Gros_Price = IIF(STYLE.Gros_Price=0 AND STYLE.Disc_Pcnt=0,m.nCost1,STYLE.Gros_Price)
    ELSE
      m.Disc_Pcnt  = 0
      m.Gros_Price = m.nCost1
    ENDIF
    *B602968,1 End.
    *E301291,1 AMM Get cost elements

  ELSE
    m.Disc_Pcnt  = 0
    m.Gros_Price = m.nCost1
    *C200098,1 RAMY change these lines to the style cost depending on 
    *C200098,1 RAMY if it has a dye operation in the cost sheet and the 
    *C200098,1 RAMY user entered a value in the 'Cost per lb' field [start]
    *m.nCost1 = Style.nMCost1
    *m.nCost2 = IIF(lcAType='A',0,Style.nMCost2)
    *m.nCost3 = IIF(lcAType='A',0,Style.nMCost3)
    *m.nCost4 = IIF(lcAType='A',0,Style.nMCost4)
    *m.nCost5 = IIF(lcAType='A',0,Style.nMCost5)
    IF lcAType='A'
      m.nCost1 = Style.nMCost1
      STORE 0 TO m.nCost2,m.nCost3,m.nCost4,m.nCost5
    ELSE  && lcAType='D'
      FOR lnI = 1 TO 5
        lcI = STR(lnI,1)
        m.nCost&lcI = IIF(lcDyeOpNo = lcI AND (STYLE.nStyWeight * laData[73])<>0 ,(STYLE.nStyWeight * laData[73]),STYLE.nMCost&lcI )
      ENDFOR
    ENDIF
    *C200098,1 RAMY [end]
  ENDIF
  *E301291,1 AMM end

  llConCst=.F.  
ELSE
  llConCst=.T.  
ENDIF
*E301291,1 AMM  Get costs of adornment orders
IF lcAType = 'A' 
  IF SEEK(STYLE.Style+laData[2]+SPACE(10),'STYDYE') 
    m.Gros_price  = IIF(llAvgCost,;
                 IIF(llWareHous,StyDye.Ave_Cost,Style.Ave_Cost),Style.TotCost)
  ELSE
    m.Gros_price  = IIF(llAvgCost, Style.Ave_Cost,Style.TotCost)
  ENDIF
  m.Disc_Pcnt  = 0
  m.nCost1     = m.Gros_price
ENDIF
*E301291,1 AMM end

*--Read equivalent costs.
m.nECost1= lfvEquCost('1',m.nCost1,laData[28],lnCurrUnt1)
m.nECost2= lfvEquCost('2',m.nCost2,laData[29],lnCurrUnt2)
m.nECost3= lfvEquCost('3',m.nCost3,laData[29],lnCurrUnt2)
m.nECost4= lfvEquCost('4',m.nCost4,laData[29],lnCurrUnt2)
m.nECost5= lfvEquCost('5',m.nCost5,laData[29],lnCurrUnt2)

INSERT INTO (lcPOLine) FROM MEMVAR
REPLACE &lcPOLine..lFlag WITH llConCst

*B608125,1 TMI [Start] Update the cost fields upon the bom cost sheet saved items
=lfCstShUpd()
*B608125,1 TMI [End  ] 

*C102225,1 MHM 06/05/2001 Fill Screen when initialize
IF lcAType = 'P' AND ASCAN(laEvntTrig , PADR('GETVAR',10)) <> 0 
  =gfDoTriger('POSTY',PADR('GETVAR',10))
ENDIF     
*C102225,1 MHM 06/05/2001 [End]

=lfGetLine()

*B803137,1 RAMY [START]
*B603709,1 Start, Added condition.
IF ! (lcAType = 'A' OR (llMultiWare AND lcAType = 'N'))
*B603709,1 End.
  *lnAlias = SELECT(0)
  SELECT STYLE
  SKIP 1
  lcNexSty = STYLE.STYLE
  SKIP -1
  *B603714,1 Start.
  *SELECT (lnAlias)
  SELECT (lcPOLine) 
  *B603714,1 End.
ENDIF
*B803137,1 RAMY [END]
*E300935,1 Enable Dyelot object [Begin]
= lfAdjDye()
*E300935,1 Enable Dyelot object [end]
RETURN
*-- end of lfvStyle.

*:*************************************************************
*! Name      : lfvSelShp
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Valid ShipTo location/Customer on line level.
*:*************************************************************
FUNCTION lfvSelShp
*E301274,1 AMM Add a new parameter to indicate whether called from the detail screen or extended size scale screen
PARAMETERS llFrmGBrw
*E301274,1 AMM end
IF lnLShpLoc = 1
  *E301274,1 AMM Seek in the right file
  *m.cWareCode = IIF(llWarehous,STYLE.CDefWare,SPACE(6))
  IF !llFrmGBrw
    m.cWareCode = IIF(llWarehous,STYLE.CDefWare,SPACE(6))
  ELSE
    m.cWareCode = IIF(llWarehous,&lcTmpSty..CDefWare,SPACE(6))
  ENDIF
  *E301274,1 AMM end
  lcLocPict='XXXXXX'
ELSE
  *--There is no drop ship location setup in the system, Cannot ship to customer.
  IF EMPTY(lcDropLoc)
    =gfModalGen('QRM34112B34000','DIALOG')
    lnLShpLoc = 1
    *E301274,1 AMM
    IF llFrmGBrw this field indicate whether ship to location or customer.
      SELECT (lcTmpSize)
      REPLACE cWarCust  WITH 'L'
    ENDIF
    *E301274,1 AMM end
    
    SHOW GET lnLShpLoc
    RETURN
  ENDIF  
  m.cWareCode = SPACE(6)
  lcLocPict='XXXXX'
ENDIF
m.Store     = SPACE(5)
SHOW GET m.cWareCode
SHOW GET m.Store
*E301274,1 AMM (start) 
*REPLACE &lcPoLine..cWareCode WITH SPACE(6),;
        &lcPoLine..Account   WITH SPACE(5),;
        &lcPoLine..Store     WITH SPACE(8)
*_CUROBJ=OBJNUM(m.cWareCode)
IF !llFrmGBrw
  REPLACE &lcPoLine..cWareCode WITH SPACE(6),;
        &lcPoLine..Account   WITH SPACE(5),;
        &lcPoLine..Store     WITH SPACE(8)
  _CUROBJ=OBJNUM(m.cWareCode)
ELSE
  SELECT (lcTmpSize)
  REPLACE cWareCode WITH SPACE(6),;
          Account   WITH SPACE(5),;
          Store     WITH SPACE(8),;
          cStatus   WITH IIF(cStatus='N','N','M')
ENDIF
*E301274,1 AMM end

RETURN


*:*************************************************************
*! Name      : lfvLShpTo
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Valid line location/account.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvLShpTo()
*!*************************************************************
FUNCTION lfvLShpTo
*E301274,1 AMM Add a new parameter to indicate whether called from 
*E301274,1 AMM details or extended size scale screen
PARAMETERS llFrmGBrw
IF llFrmGBrw
  m.cWareCode = EVAL(VARREAD())
  lnLShpLoc   = IIF(cWarCust='C',2,1)
ENDIF
*E301274,1 AMM end


IF !llBrowse AND m.cWareCode=lcOldValue AND !EMPTY(lcOldValue)
  RETURN
ENDIF
*--Case of Location selected.
IF lnLShpLoc = 1
  lcOldWare = IIF(llBrowse,m.cWareCode,lcOldValue)
  IF llBrowse OR !SEEK(m.cWareCode,'WAREHOUS')
    m.cWareCode = gfBrowWare( .F. )
  ENDIF  
  llBrowse = .F.
  *E301291,1 AMM Consider the case of adornment order
  *IF lcAType = 'N' AND PADR(m.cWareCode,6) = PADR(laData[2],6)
  IF lcAType $ 'NA' AND PADR(m.cWareCode,6) = PADR(laData[2],6)
  *E301291,1 AMM end
    *--Target location cannot be same as the Scource location!

    *B604736,1 KHM 07/30/2001 (Begin) Fixing the spelling mistake of source.
    *=gfModalGen('TRM34108B34000','DIALOG','Target'+'|'+'Scource')
    =gfModalGen('TRM34108B34000','DIALOG','Target'+'|'+'source')
    *B604736,1 KHM 07/30/2001 (End)

    m.cWareCode=lcOldWare
    RETURN
  ENDIF

  *E301291,1 AMM Consider the case of adornment order
  *IF laScrMode[3] AND lcAType = 'N' AND SEEK(lcAType+laData[1],'POSLN')
  IF laScrMode[3] AND lcAType $ 'NA' AND SEEK(lcAType+laData[1],'POSLN')
  *E301291,1 AMM end
    SELECT POSLN 
    LOCATE REST WHILE cStyType+Po = lcAType+laData[1] FOR TranCd $ '2456' AND cWareCode=lcOldWare
    IF FOUND()
      *-Cannot change the location since the recieving was done.
      =gfModalGen('TRM34025B34000','DIALOG','location'+'|'+IIF(TranCd='6','Issue','recieving'))
      m.cWareCode=lcOldWare
      RETURN
    ENDIF
  ENDIF

  IF !SEEK(PADR(m.Style,19)+m.cWareCode+SPACE(10),'STYDYE')
    *-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
    IF gfModalGen('QRM34048B34004','DIALOG',ALLTRIM(m.Style)+'|'+m.cWareCode) = 1
      DO gpAdStyWar WITH m.Style,SPACE(10),m.cWareCode
    ELSE
      m.cWareCode=lcOldValue 
      RETURN
    ENDIF
  ENDIF
  *E301274,1 AMM Select the right file
  *SELECT (lcPoLine)
  SELECT IIF(llFrmGBrw,lcTmpSize,lcPoLine)
  *E301274,1 AMM end
  REPLACE cWareCode WITH m.cWareCode,;
          Account   WITH SPACE(5)

*--Case of Customer selected.
ELSE
  m.cWareCode = PADR(m.cWareCode,5)
  IF llBrowse OR !SEEK('M'+m.cWareCode,'CUSTOMER')
    xAccount = m.cWareCode
    DO CUSBROWM WITH xAccount
    m.cWareCode = xAccount
  ENDIF
  llBrowse= .F.
  IF !SEEK(PADR(m.Style,19)+lcDropLoc+SPACE(10),'STYDYE')
    *-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
    IF gfModalGen('QRM34048B34004','DIALOG',ALLTRIM(m.Style)+'|'+lcDropLoc) = 1
      DO gpAdStyWar WITH m.Style,SPACE(10),lcDropLoc
    ELSE
      m.cWareCode=lcOldValue 
      RETURN
    ENDIF
  ENDIF

  *--Case of customer the field cWareCode will hold the account code till save.
  *E301274,1 AMM Select the right file
  *SELECT (lcPoLine)
  SELECT IIF(llFrmGBrw,lcTmpSize,lcPoLine)
  *E301274,1 AMM end
  REPLACE cWareCode WITH m.cWareCode,;
          Account   WITH m.cWareCode
  IF llFrmGBrw 
    REPLACE cStatus  WITH IIF(cStatus='N','N','M')
  ENDIF          

ENDIF
SHOW GET m.cWareCode
RETURN

*!*************************************************************
*! Name      : lfvLStore
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Validate Stores on line level.
*!*************************************************************
FUNCTION lfvLStore
*E301274,1 AMM Add a new parameter to indicate whether called from Detail or extended size scale screen
PARAMETERS llFrmGBrw
IF llFrmGBrw
  m.Store = EVAL(VARREAD())
ENDIF
*E301274,1 AMM end

IF !EMPTY(m.Store) AND !SEEK('S'+PADR(m.cWareCode,5)+m.Store,'CUSTOMER')
  xStore   = m.Store
  IF !CUSBROWS(PADR(m.cWareCode,5),.T.)
    STORE SPACE(8) TO xStore
  ENDIF
  m.Store = xStore
ENDIF 
*E301274,1 AMM Update table
IF llFrmGBrw
  SELECT (lcTmpSize)
  REPLACE STORE     WITH m.Store,;
          cStatus   WITH IIF(cStatus='N','N','M')
ELSE
*E301274,1 AMM end
  SELECT (lcPoLine)
  REPLACE &lcPoLine..Store WITH m.Store
  SHOW GET m.Store
*E301274,1 AMM end if
ENDIF
*E301274,1 AMM end
RETURN


*!*************************************************************
*! Name      : lfvPrePack
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Validate Style prepak
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
FUNCTION lfvPrePack
*E301274,1 AMM Add a new parameter to indicate whether called from detail or extended size scale screen
PARAMETERS llFrmGBrw
IF llFrmGBrw
  m.Prepak = EVAL(VARREAD())
ENDIF
*E301274,1 AMM end

PRIVATE lcBrFields,lcFile_Ttl

IF (!EMPTY(m.Prepak) .AND. !SEEK('P'+m.Scale+m.Prepak,'Scale'))
  =gfPrePBrow(m.Scale,@m.Prepak)
  =SEEK('S'+m.Scale,'Scale')
ENDIF
*E301274,1 AMM Update table
IF llFrmGBrw
  IF PrePak # lcOldVal
    REPLACE Prepak    WITH m.Prepak,;
            cStatus   WITH IIF(cStatus='N','N','M')
    m.PPQTY = 0
  ENDIF
ELSE
*E301274,1 AMM end
  IF &lcPOLine..Prepak <> m.Prepak
    REPLACE &lcPOLine..Prepak WITH m.Prepak
    IF EMPTY(&lcPOLine..Prepak)
      REPLACE &lcPOLine..PPQty WITH 0
      m.PPQTY = 0
      SHOW GET m.PPQty
    ELSE
      SHOW GET m.Prepak ENABLE
   ENDIF
   SHOW GETS WINDOW (lcWinC22) ONLY
   SHOW WINDOW (lcBrTtl1) REFRESH SAME
   FLUSH
  ENDIF
*E301274,1 AMM End If
ENDIF
*E301274,1 AMM end
*!*************************************************************
*! Name      : lfvPPakQty
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Validate Style prepak quantity
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
FUNCTION lfvPPakQty
*E301274,1 AMM Add a new paramter to indicate whether called from detail or extended size scale screen
PARAMETERS llFrmGBrw

IF llFrmGBrw
  m.PPQty = VAL( EVAL(VARREAD()))
ENDIF
*E301274,1 AMM end

*E301274,1 AMM Adjust condition
*IF m.PPQty = &lcPOLine..PPQty AND m.Prepak <> &lcPOLine..Prepak
IF (!llFrmGBrw .AND. m.PPQty = EVAL(lcPOLine+'.PPQty') AND m.Prepak <> EVAL(lcPOLine+'.Prepak')) ;
     .OR. (llFrmGBrw .AND. m.PPQty = VAL(lcOldVal))
*E301274,1 AMM end
  RETURN
ENDIF

IF !EMPTY(m.Prepak) AND m.PPQty <> 0 AND SEEK('P'+m.Scale+m.Prepak,'Scale')
  lnAlias = SELECT()
  SELECT SCALE
  lnPPakQ=Scale.PPTot
  SCATTER FIELDS PP1,PP2,PP3,PP4,PP5,PP6,PP7,PP8 TO laPPq
  =SEEK('S'+m.Scale)
  SELECT (lnAlias)
  m.TotQty  = m.PPQty * lnPPakQ
  FOR I=1 TO 8
    lcI=STR(I,1)
    m.Qty&lcI = m.PPQty * laPPq[I]
    *E301274,1 AMM Adjust condition
    *IF !lfvSizeQty(I)
    IF (!llFrmGBrw .AND. !lfvSizeQty(I)) .OR. (llFrmGBrw .AND. !lfexSzQty(I) )
    *E301274,1 AMM end
      _CUROBJ = OBJNUM(m.TotQty)
      RETURN .F.
    ENDIF 
  ENDFOR 
 
  SHOW GET m.TotQty
ENDIF
=SEEK('S'+m.Scale,'Scale')
*E301274,1 AMM start
IF !llFrmGBrw
*E301274,1 AMM end
  SELECT (lcPOLine)
  REPLACE PPQty WITH m.PPQty
  SHOW GETS WINDOW (lcWinC22) ONLY
  SHOW WINDOW (lcBrTtl1) REFRESH SAME
  =lfRefresh(lcWinC22)
  FLUSH
*E301274,1 AMM start Update table
ELSE
  REPLACE TotQty    WITH m.TotQty,;
          cTotQty   WITH STR(m.TotQty,7),;
          cStatus   WITH IIF(cStatus='N','N','M')
ENDIF
*E301274,1 AMM end
RETURN


*!*************************************************************
*! Name      :  lfvPieces
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Validate Style pieces
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
*E301169,1
FUNCTION lfvPieces
*E301274,1 AMM Add a new parameter to indicate whether called from detail or extended size scale screen
PARAMETERS llFrmGBrw
IF llFrmGBrw
  m.TotQty = EVAL(VARREAD())
ENDIF
*E301274,1 AMM end

IF !EMPTY(m.Prepak) AND m.TotQty<>0 AND SEEK('P'+m.Scale+m.Prepak,'Scale')
  lnAlias = SELECT()
  SELECT SCALE
  lnPPakQ=Scale.PPTot
  *B120529,1 NNA 03/25/2004 (Begin) Caluclate the Prepack Numbers if the total qty. Compatible with the Prepack
  IF MOD(m.TotQty,lnPPakQ) = 0 
    m.ppQty = (m.TotQty/Scale.PPTot)
    REPLACE &lcPOLine..PPQty WITH m.ppQty
    REPLACE &lcPOLine..TotQty WITH m.TotQty
  ENDIF	
  *B120529,1 NNA (End)

  SCATTER FIELDS PP1,PP2,PP3,PP4,PP5,PP6,PP7,PP8 TO laPPq
  =SEEK('S'+m.Scale)
  SELECT (lnAlias)
  
  IF MOD(m.TotQty,lnPPakQ) <> 0 
    *-Total qty is not evenly divisible by the prepak qty 9999.
    =gfModalGen('TRM34044B34000','DIALOG',STR(lnPPakQ,4) )
    _CUROBJ = OBJNUM(m.Prepak)
    RETURN
  ENDIF
  FOR I=1 TO 8
    lcI=STR(I,1)
    m.Qty&lcI = m.TotQty * ( laPPq[I] / lnPPakQ )
    *E301274,1 AMM start, Update table
    IF llFrmGBrw
      REPLACE Qty&lcI   WITH m.Qty&lcI,;
              cQty&lcI  WITH m.cQty&lcI
    ENDIF
    *E301274,1 AMM adjust
    *IF ! lfvSizeQty(I)
    IF (!llFrmGBrw .AND. !lfvSizeQty(I)) .OR. (llFrmGBrw .AND. !lfexSzQty(I) )    
    *E301274,1 AMM end
      _CUROBJ = OBJNUM(m.TotQty)
      RETURN .F.
    ENDIF 
  ENDFOR 

*E301169,1 No entered prepak. [Begin
ELSE

  *-- if scale has one size.
  IF Scale.Cnt = 1
    m.Qty1 = m.TotQty
    *E301274,1 AMM make a new function and call it when called from extended size scale screen
    *=lfvSizeQty(1) AND gfUpdate()
    IF llFrmGBrw
      =lfexSzQty(1)
    ELSE
      =lfvSizeQty(1) AND gfUpdate()
    ENDIF
    
    *E301274,1 AMM end
  ENDIF  && end if scale has one size.

*E301169,1 No entered prepak. [End..
ENDIF
=SEEK('S'+m.Scale,'Scale')
SHOW GETS WINDOW (lcWinC22) ONLY
SHOW WINDOW (lcBrTtl1) REFRESH SAME
*_CUROBJ=OBJNUM(m.Qty1)
FLUSH


*!*************************************************************
*! Name      : lfvSizeQty
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Validate Style size quantity
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
FUNCTION lfvSizeQty
PARAMETERS lnSize
lcSz = STR(lnSize,1)

*-Cannot accept -ve quantity.
IF m.Qty&lcSz<0
  =gfModalGen('TRM34051B34000','DIALOG')
  m.Qty&lcSz = &lcPOLine..Qty&lcSz
  SHOW GET m.Qty&lcSz
  RETURN .F.
ENDIF
*C200152,4 AMH custom trigger to check if the inter location PO
*C200152,4     found in approved batch [Start]
IF lcAtype = "N" AND laScrMode[3] .AND. m.Qty&lcSz <> &lcPOLine..Qty&lcSz;
                 .AND. ASCAN(laEvntTrig , PADR('EDITSIZE',10)) <> 0
  PRIVATE llNotEdit
  llNotEdit = .F.
  =gfDoTriger('POINTRC',PADR('EDITSIZE',10))
  IF llNotEdit
    m.Qty&lcSz = &lcPOLine..Qty&lcSz
    SHOW GET m.Qty&lcSz
    RETURN .F.
  ENDIF
ENDIF
*C200152,4 AMH [End]

IF m.Qty&lcSz = &lcPOLine..Qty&lcSz
  
  IF lnSize = Scale.Cnt 
    IF m.TotQty <> &lcPOLine..TotQty .AND. ;
      gfModalGen('QRM34045B34001','DIALOG',ALLTRIM(STR(m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,10))) = 2
      *-Quantities out of balance! 9999 Pieces ok?','\<Yes;\?\<No'
      _CUROBJ = OBJNUM(m.Prepak)
    ELSE
      *B120529,1 NNA 03/25/2004 (Begin) If the customer changed in the sizes quantities (after I calculated the prepack Qty.)
      *B120529,1 NNA            I get the prepack Qty. Equal zero Specially if it incompatible
      IF !EMPTY(m.Prepak) .AND. m.TotQty <> &lcPOLine..TotQty
        lnScalRec = Recno('Scale')
        = SEEK('P'+m.Scale+m.Prepak,'Scale')      
        IF MOD(&lcPOLine..TotQty , Scale.PPTot ) <>0
          REPLACE &lcPOLine..PPQty WITH 0 
	      m.PPQTY = 0
		  GOTO lnScalRec In Scale		      
          SHOW GET m.PPQty
          = lfvPPakQty()
          RETURN
        ELSE
		  lnScalRec = Recno('Scale')
          = SEEK('P'+m.Scale+m.Prepak,'Scale')      
          REPLACE &lcPOLine..PPQty WITH (&lcPOLine..TotQty / Scale.PPTot)
          m.PPQTY  = &lcPOLine..PPQty
	      GOTO lnScalRec IN Scale          
          SHOW GET m.PPQty
        ENDIF
      ENDIF  
      *B120529,1 NNA (End)

      m.TotQty = &lcPOLine..TotQty
      SHOW GET m.TotQty
      *B803346,1 (Begin) Don't call lfvNew() if lfvSizeQty() is called from lfvPieces() or lfvPPakQty().
      *B603709,1 Start, Added to condition.
      *IF lcAType <> 'A' .AND. lnSize = SCALE.CNT AND VARREAD() <> 'TOTQTY' AND VARREAD() <> 'PPQTY'
      IF ! (lcAType = 'A' OR (llMultiWare AND lcAType = 'N')) AND ;
           lnSize = SCALE.CNT AND VARREAD() <> 'TOTQTY' AND VARREAD() <> 'PPQTY'
      *B603709,1 End.

         *B604387,1 KHM 04/15/2001 (Begin) Changing the way of calling the lfvNew
         *B604387,1                function because it causes sayntax error when
         *B604387,1                using the mouse.
         *=lfvNew()
         _CUROBJ = OBJNUM(pbnew)
         KEYBOARD CHR(13) CLEAR
         *B604387,1 KHM 04/15/2001 (End_
         
      ENDIF
      *B803137,1 RAMY [END]
    ENDIF
  ENDIF
  RETURN
ENDIF

IF laScrMode[3]
  lnAlias = SELECT()
  lnRecQt = 0
  *MAN added the issued QTY to the scan.
  *IF laData[37]+laData[38]<>0 AND ;
  *  SEEK(lcAType+laData[1]+&lcPOLine..Style+STR(&lcPOLine..Lineno,6),'POSLN')
   IF  SEEK(lcAType+laData[1]+&lcPOLine..Style+STR(&lcPOLine..Lineno,6),'POSLN') 
    SELECT POSLN
    lnSvRec = RECNO()
    *MAN added the issued QTY to the scan.
    *SCAN WHILE cStyType+Po+Style+STR(Lineno,6)=lcAType+laData[1]+&lcPOLine..Style+STR(&lcPOLine..Lineno,6) FOR Trancd $ '245'
    SCAN WHILE cStyType+Po+Style+STR(Lineno,6)=lcAType+laData[1]+&lcPOLine..Style+STR(&lcPOLine..Lineno,6) FOR Trancd $ '2456' 
      lnRecQt = lnRecQt + Qty&lcSz
    ENDSCAN
    GO lnSvRec
  ENDIF  
  SELECT (lnAlias)

  *-Quantity cannot be less than the received quantity.
  IF m.Qty&lcSz < lnRecQt
    =gfModalGen('TRM34043B34000','DIALOG')        
    m.Qty&lcSz = &lcPOLine..Qty&lcSz
    SHOW GET m.Qty&lcSz
    RETURN .F.
  ENDIF

  IF llSOInstld AND lcAType='P' AND laData[3] $ 'OH'
    llFound = SEEK('P'+laData[1]+&lcPOline..Style+STR(&lcPOline..LineNo,6),'POSLN')
    llFound = llFound AND ( m.Qty&lcSz  < POSLN.Ord&lcSz )
    llFound = llFound AND SEEK('2'+laData[1]+&lcPOline..Style,'CutPick')
    IF llFound AND !lfChkOrdQt(lcSz)
      m.Qty&lcSz = &lcPOLine..Qty&lcSz
      SHOW GET m.Qty&lcSz
      _CUROBJ=_CUROBJ
      RETURN
    ELSE
      SHOW GET m.Qty&lcSz
      *C037957,1 MHM 10/10/2004 Custom Amend CutPick File[Start]
      IF lcProgName = "POSTY" AND ASCAN(laEvntTrig , PADR('POUPCTPK',10)) <> 0
        =gfDoTriger('POSTY',PADR('POUPCTPK',10))
      ENDIF     
      *C037957,1 MHM [End]      
    ENDIF
  ENDIF
ENDIF

laData[36] = laData[36] - &lcPOLine..Qty&lcSz + m.Qty&lcSz
laData[40] = laData[40] - &lcPOLine..Qty&lcSz + m.Qty&lcSz
llRecalTot = .T.

SELECT (lcPOHdr)
REPLACE nStyOrder WITH laData[36],;
        Open      WITH laData[40]
SELECT (lcPOLine)
REPLACE Qty&lcSz WITH m.Qty&lcSz ,;
        TotQty   WITh Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 

*B120529,1 NNA 03/25/2004 (Begin) If the customer changed in the sizes quantities (after I calculated the prepack Qty.)
*B120529,1 NNA            I get the prepack Qty. Equal zero Specially if it incompatible
lcMovePos = SYS(18)
IF !EMPTY(m.Prepak) .AND. lcMovePos = 'QTY' + LCSZ
  lnScalRec = Recno('Scale')
  = SEEK('P'+m.Scale+m.Prepak,'Scale')      
  IF MOD(&lcPOLine..TotQty , Scale.PPTot ) <>0
    REPLACE &lcPOLine..PPQty WITH 0 
    m.PPQTY = 0
    m.TotQty = &lcPOLine..TotQty    
    GOTO lnScalRec In Scale		      
    SHOW GET m.PPQty
    SHOW GET m.TotQty
  ELSE
    lnScalRec = Recno('Scale')
    = SEEK('P'+m.Scale+m.Prepak,'Scale')      
    REPLACE &lcPOLine..PPQty WITH (&lcPOLine..TotQty / Scale.PPTot)
    m.PPQTY  = &lcPOLine..PPQty
    m.TotQty = &lcPOLine..TotQty
    GOTO lnScalRec IN Scale          
    SHOW GET m.PPQty
    SHOW GET m.TotQty
  ENDIF
  *-- IF the user insert qty. in a size not included in the prepack sizes I get the prepack qty.= Zero
  lnScalRec = Recno('Scale')
  = SEEK('P'+m.Scale+m.Prepak,'Scale')      
  SCATTER FIELDS PP1,PP2,PP3,PP4,PP5,PP6,PP7,PP8 TO laPPq
  FOR I = 1 TO 8
  IF laPPq[I] = 0 AND m.Qty&lcSz > 0
    REPLACE &lcPOLine..PPQty WITH 0 
    m.PPQTY = 0
    SHOW GET m.PPQty
  ENDIF
  ENDFOR
  GOTO lnScalRec IN Scale            
ENDIF  
*B120529,1 NNA (End)
IF lnSize = Scale.Cnt 
  IF m.TotQty <> &lcPOLine..TotQty .AND. ;
      gfModalGen('QRM34045B34001','DIALOG',ALLTRIM(STR(m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,10))) = 2
      *-Quantities out of balance! 9999 Pieces ok?','\<Yes;\?\<No'
    _CUROBJ = OBJNUM(m.Prepak)
  ELSE
    m.TotQty = &lcPOLine..TotQty
    SHOW GET m.TotQty
  ENDIF
ENDIF
SHOW WINDOW (lcBrTtl1) REFRESH SAME

*B803137,1 RAMY [START]
*B803346,1 (Begin) Don't call lfvNew() if lfvSizeQty() is called from lfvPieces() or lfvPPakQty().
*IF lcAType <> 'A' .AND. lnSize = SCALE.CNT
*B603709,1 Start, Added to condition.
*IF lcAType <> 'A' .AND. lnSize = SCALE.CNT AND VARREAD() <> 'TOTQTY' AND VARREAD() <> 'PPQTY'  AND m.TotQty = &lcPOLine..TotQty
IF ! (lcAType = 'A' OR (llMultiWare AND lcAType = 'N')) AND ;
   lnSize = SCALE.CNT AND VARREAD() <> 'TOTQTY' AND VARREAD() <> 'PPQTY'  AND m.TotQty = &lcPOLine..TotQty
*B603709,1 End.
*B803346,1 (End)
  *B604387,1 KHM 04/15/2001 (Begin) Changing the way of calling the lfvNew
  *B604387,1                function because it causes sayntax error when
  *B604387,1                using the mouse.
  *=lfvNew()
  _CUROBJ = OBJNUM(pbnew)
  KEYBOARD CHR(13) CLEAR
  *B604387,1 KHM 04/15/2001 (End_
ENDIF
*B803137,1 RAMY [END]
RETURN


*!*************************************************************
*! Name      : lfvEquCost
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Get eguevalent cost by pathing the foreign.
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
FUNCTION lfvEquCost
PARAMETERS lcCstNo,lnFrnCost,lnCurRate,lnCurUnt

lnCstType = lcIType&lcCstNo
IF lnCstType $ 'PMD'
  IF lnCstType='P'
    lcPMethod = gfGetExSin(@lcPUnMeth,laData[26])
    lcPMethod = IIF(EMPTY(lcPMethod),'*',lcPMethod)
    lcPUnMeth = IIF(EMPTY(lcPUnMeth),'/',lcPUnMeth)
    lnEquCost = lnFrnCost &lcPMethod lnCurRate &lcPUnMeth lnCurUnt
  ELSE
    lcDMethod = gfGetExSin(@lcDUnMeth,laData[27])
    lcDMethod = IIF(EMPTY(lcDMethod),'*',lcDMethod)
    lcDUnMeth = IIF(EMPTY(lcDUnMeth),'/',lcDUnMeth)
    lnEquCost = lnFrnCost &lcDMethod lnCurRate &lcDUnMeth lnCurUnt
  ENDIF
ELSE
  lnEquCost = lnFrnCost
ENDIF
lnEquCost=ROUND(lnEquCost,3)
RETURN (lnEquCost)


*!*************************************************************
*! Name      : lfvFrnCost
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Get foreign cost by pathing the base.
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
FUNCTION lfvFrnCost
PARAMETERS lcCstNo,lnEquCost,lnCurRate,lnCurUnt

lnCstType = lcIType&lcCstNo
IF lnCstType $ 'PMD'
  IF lnCstType='P'
    lcPMethod = gfGetExSin(@lcPUnMeth,laData[26])
    lcOpsPMet = IIF(lcPMethod='/','*','/')
    lcOpsPUMt = IIF(lcPUnMeth='*','/','*')
    lnFrnCost = lnEquCost &lcOpsPMet lnCurRate &lcOpsPUMt lnCurUnt
  ELSE
    lcDMethod = gfGetExSin(@lcDUnMeth,laData[27])
    lcOpsDMet = IIF(lcDMethod='/','*','/')
    lcOpsDUMt = IIF(lcDUnMeth='*','/','*')
    lnFrnCost = lnEquCost &lcOpsDMet lnCurRate &lcOpsDUMt lnCurUnt
  ENDIF
ELSE
  lnFrnCost = lnEquCost 
ENDIF
RETURN (lnFrnCost)


*!*************************************************************
*! Name      : lfFrmCont
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Get the line costs from contract if found.
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
FUNCTION lfFrmCont

lnAlias = SELECT()
IF !USED(lcContrt)
  *-- MAN
  *SELECT POSHDR
  *SET FILTER TO
  *SELECT DISTINCT POSHDR.PO,POSHDR.Vendor,POSHDR.Entered,POSHDR.Complete,POSHDR.cPriceCur,POSHDR.cDutyCur,POSLN.STYLE,;
    POSLN.nCost1,POSLN.nCost2,POSLN.nCost3,POSLN.nCost4,POSLN.nCost5 ;
    FROM POSHDR,POSLN ;
    WHERE POSHDR.cStyType = 'C' AND POSHDR.Status='O' AND ;
    POSLN.PO = POSHDR.PO ;
    INTO DBF (gcWorkDir+lcContrt)
  SELECT DISTINCT POSHDR.PO,POSHDR.Vendor,POSHDR.Entered,POSHDR.Complete,POSHDR.cPriceCur,POSHDR.cDutyCur,POSLN.STYLE,;
    POSLN.nCost1,POSLN.nCost2,POSLN.nCost3,POSLN.nCost4,POSLN.nCost5 ;
    FROM POSHDR,POSLN ;
    WHERE POSHDR.Vendor+POSHDR.cStyType+POSHDR.PO=laData[2]+"C" AND;
          POSHDR.Status='O' AND POSHDR.cStyType+POSHDR.PO=POSLN.PO;
    INTO DBF (gcWorkDir+lcContrt)    
  INDEX ON STYLE TAG (lcContrt) OF (gcWorkDir+lcContrt)
  *SELECT POSHDR
  *SET FILTER TO cStyType = lcAType
  SELECT (lcContrt)
  *SET FILTER TO Vendor=laData[2] AND ;
                (cPriceCur=laData[26] OR cDutyCur=laData[27]) AND ;
                BETWEEN(Entered,laData[5],laData[6]) AND ;
                BETWEEN(Complete,laData[5],laData[6])
  SET FILTER TO (cPriceCur=laData[26] OR cDutyCur=laData[27]) AND ;
                BETWEEN(Entered,laData[5],laData[6]) AND ;
                BETWEEN(Complete,laData[5],laData[6])
ENDIF

SELECT (lcContrt)
GO TOP
IF !EOF()  &&Contract exist
  IF SEEK(m.Style)
    m.nCost1 = IIF(&lcContrt..cPriceCur<>laData[26],;
       IIF(STYLE.cPriceCur<>laData[26],0,Style.nICost1),&lcContrt..nCost1)
    m.nCost2 = IIF(&lcContrt..cDutyCur<>laData[27],;
       IIF(STYLE.cDutyCur<>laData[27] ,0,Style.nICost2),&lcContrt..nCost2)
    m.nCost3 = IIF(&lcContrt..cDutyCur<>laData[27],;
       IIF(STYLE.cDutyCur<>laData[27] ,0,Style.nICost3),&lcContrt..nCost3)
    m.nCost4 = IIF(&lcContrt..cDutyCur<>laData[27],;
       IIF(STYLE.cDutyCur<>laData[27] ,0,Style.nICost4),&lcContrt..nCost4)
    m.nCost5 = IIF(&lcContrt..cDutyCur<>laData[27],;
       IIF(STYLE.cDutyCur<>laData[27] ,0,Style.nICost5),&lcContrt..nCost5)
    lRet = .T.
  ELSE
    lRet = .F.   
  ENDIF
ELSE
  lRet = .F.
ENDIF
SELECT(lnAlias)
RETURN lRet

*!*************************************************************
*! Name      : lfVldSCnr
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Validate the style contract.
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
FUNCTION lfVldSCnr

lnAlias = SELECT()
lRet=.F.
SELECT POSLN
SET ORDER TO TAG POSLNS
SET RELA TO 'C'+PO INTO POSHDR
IF SEEK(PADR(m.Style,19)+'C')
 LOCATE REST WHILE Style+cStyType=PADR(m.Style,19)+'C' ;
               FOR PO<>laData[1] AND POSHDR.Status='O' AND Vendor=laData[2] AND;
                   BETWEEN(laData[5],POSHDR.Entered,POSHDR.Complete) AND ;
                   BETWEEN(laData[6],POSHDR.Entered,POSHDR.Complete)
 lRet = FOUND()
ENDIF
SET ORDER TO TAG POSLN
SET RELA TO
SELECT POSHDR
SEEK lcAType+laData[1]
SELECT(lnAlias)
RETURN lRet


*!*************************************************************
*! Name      : lfvRates1,lfvRates2
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Valid Rates.
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
FUNCTION lfvRates1

IF laData[28] <> lcOldValue  AND !lfCkEOF(lcPoLine)
  *-The price currency rate was changed, the line costs will be recalculated.
  =gfModalGen('INM34029B34000','DIALOG','price')        
  =lfChgLCst('P')
ENDIF  
FUNCTION lfvRates2
IF laData[29] <> lcOldValue  AND !lfCkEOF(lcPoLine)
  *-The duty currency rate was changed, the line costs will be recalculated.
  =gfModalGen('INM34029B34000','DIALOG','duty')        
  =lfChgLCst('D')
ENDIF  

*!*************************************************************
*! Name      : lfVCost
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Valid Cost elements.
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
*E301169,1
FUNCTION lfVCost
*E301169,1 Add new parameter to evaluate style percentage [Begin
*PARA lcCstNo
*E301274,1 AMM Add a new parameter to indicate whether called from details or extended size scale screen
*PARA lcCstNo,llPercent
PARA lcCstNo,llPercent,llFrmGBrw
*E301274,1 AMM end
*E301169,1 Add new parameter to evaluate style percentage [End
*B602861,1 put condition back.
*E301274,1 AMM Adjust condition
*IF &LCPOLINE..nCost&lcCstNo <> m.nCost&lcCstNo
IF IIF(llFrmGBrw ,EVAL(lcTmpSize+'.nCost'+lcCstNo) ,EVAL(LCPOLINE+'.nCost'+lcCstNo))<> m.nCost&lcCstNo
  *E301274,1 AMM end
  lnAlias = SELECT()
  *E301274,1 AMM Select the right file
  *SELECT (lcPOLine)
  SELECT IIF(llFrmGBrw,lcTmpSize,lcPOLine)
  *E301274,1 AMM end
  *--- SSH
  IF llFrmGBrw
  *--- SSH
    lnOldVal = m.nCost&lcCstNo.
    lnOldEVal= m.nECost&lcCstNo.
    SCATTER MEMVAR MEMO
  ELSE
    lnOldVal =nCost&lcCstNo
    lnOldEVal=nECost&lcCstNo
  ENDIF
  m.nECost&lcCstNo=lfvEquCost(lcCstNo,m.nCost&lcCstNo,IIF(lcCstNo='1',laData[28],laData[29]),IIF(lcCstNo='1',lnCurrUnt1,lnCurrUnt2))
 
  REPLACE nCost&lcCstNo  WITH nCost&lcCstNo  + m.nCost&lcCstNo  - lnOldVal
  REPLACE nECost&lcCstNo WITH nECost&lcCstNo + m.nECost&lcCstNo - lnOldEVal
  FLUSH

  *E301169,1 if style price and changes has been done [Begin
  IF lcCstNo = '1'
    *B602861,1 Change the way of geting the discount.
    *m.Disc_Pcnt = IIF(llPercent,m.Disc_Pcnt,IIF(m.Gros_Price=0,0,100-m.nCost1*100/m.Gros_Price))
    IF llPercent OR ;
       (m.Gros_Price<>0 AND ROUND((m.Gros_Price*(1-m.Disc_Pcnt/100)),3) = m.nCost1)
      m.Disc_Pcnt = m.Disc_Pcnt
    ELSE
      IF !llPercent AND VARREAD() <> "GROS_PRICE"
        m.Disc_Pcnt = MAX(IIF(m.Gros_Price=0,0,100-m.nCost1*100/m.Gros_Price),0)
        IF m.Disc_Pcnt = 0
          m.Gros_Price = m.nCost1
        ENDIF  
      ENDIF  
    ENDIF
    IF m.Gros_Price = 0 AND m.Disc_Pcnt=0
      m.Gros_Price = m.nCost1
    ENDIF

    REPLACE Gros_Price WITH m.Gros_Price ,;
            Disc_Pcnt  WITH m.Disc_Pcnt
    *E301274,1 AMM Update table
    IF llFrmGBrw
      REPLACE cGros_Price WITH STR(m.Gros_Price,9,2),;
              cDisc_Pcnt  WITH STR(m.Disc_Pcnt,5,2),;
              nCost1      WITH m.nCost1            ,;
              cNcost1     WITH STR(m.nCost1,10,2),;
              cStatus     WITH IIF(cStatus='N','N','M')
              
    ENDIF
    *E301274,1 AMM end
    
    SHOW GET m.Gros_Price
    SHOW GET m.Disc_Pcnt
    SHOW GET m.nCost1
  
  ENDIF  
  *E301169,1 if style price and changes has been done [End
   IF lcAType = 'P'  .AND. llDispPric
     =lfvSelPrice(.T.)
   ENDIF 
   *wab
  SHOW GET m.nECost&lcCstNo
  SHOW WINDOW (lcBrTtl1) REFRESH SAME
  llRecalTot = .T.
  *B608125,1 TMI [Start] update the other cost elemnts values when changing the style price price
  SELECT &lcPoLine
  REPLACE lRefCost WITH .F.
  =lfCstShUpd()  
  *B608125,1 TMI [End  ] 

  SELECT(lnAlias)

ENDIF  

*!*************************************************************
*! Name      : lfCcost
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : CHANGE COSTS IN LINES DUE TO RATE CHANGES.
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
FUNCTION lfChgLCst
PARA lcCsTp

lnAlias = SELECT()

*B608125,1 TMI [Start] update costing for all lines if rate is changed
=lfCstShUpd(.T.)
*B608125,1 TMI [End  ] 

SELECT (lcPOLine)
SCAN
  IF lcCsTp='P'
    REPLACE nECost1 WITH lfvEquCost('1',nCost1,laData[28],lnCurrUnt1)
    llRecalTot = .T.
  ENDIF
  IF lcCsTp='D'
    REPLACE nECost2 WITH lfvEquCost('2',nCost2,laData[29],lnCurrUnt2)
    REPLACE nECost3 WITH lfvEquCost('3',nCost3,laData[29],lnCurrUnt2)
    REPLACE nECost4 WITH lfvEquCost('4',nCost4,laData[29],lnCurrUnt2)
    REPLACE nECost5 WITH lfvEquCost('5',nCost5,laData[29],lnCurrUnt2)        
    llRecalTot = .T.
  ENDIF
  *wab
  IF lcAType = 'P'  .AND. llDispPric
    =lfvSelPrice(.T.)
  ENDIF
  *wab
ENDSCAN
SELECT (lcPOHdr)
REPLACE cPriceCur WITH laData[26],;
        cDutyCur  WITH laData[27],;
        nPriceRat WITH laData[28],;
        nDutyRat  WITH laData[29]

SELECT(lnAlias)

*!*************************************************************
*! Name    : lfRecalc
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose : Function to recalculate the costs in header.
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
FUNCTION lfRecalc

lnAlias = SELECT()
SELECT (lcPOLine)
lnSavRc=IIF(!EOF(),RECNO(),0)
*B602861,1 Change the way of sum total estemeted to depend on gross insted of net.
*SUM TotQty,(TotQty*nECost1),(TotQty*nECost2),(TotQty*nECost3),(TotQty*nECost4),(TotQty*nECost5),;
             (TotQty*nCost1),(TotQty*nCost2),(TotQty*nCost3),(TotQty*nCost4),(TotQty*nCost5) TO ;
  laData[36],laData[31],laData[32],laData[33],laData[34],laData[35],;
             laData[51],laData[52],laData[53],laData[54],laData[55]


SUM TotQty,(TotQty*nECost2),(TotQty*nECost3),(TotQty*nECost4),(TotQty*nECost5),;
    (TotQty*IIF(Disc_Pcnt=0,nCost1,Gros_Price*(1-Disc_Pcnt/100))),(TotQty*nCost2),(TotQty*nCost3),(TotQty*nCost4),(TotQty*nCost5) TO ;
    laData[36],laData[32],laData[33],laData[34],laData[35],;
    laData[51],laData[52],laData[53],laData[54],laData[55]

IF llMulCurr
  laData[31] = lfvEquCost('1',laData[51],laData[28],lnCurrUnt1)
ELSE
  laData[31] = laData[51]
ENDIF
*B602861,1 End.

laData[30]=laData[31]+laData[32]+laData[33]+laData[34]+laData[35]
*B603693,1 WMA [Begin]
*laData[40]=MAX((laData[36]-laData[37]-laData[38]-laData[39]),0)
*B603693,1 WMA [End]

IF lnSavRc<>0
  GOTO lnSavRc
ENDIF
SELECT (lcPOHdr)
GATHER FROM laData FIELDS &lcScFields
SELECT(lnAlias)
llRecalTot = .F.
RETURN

*!*************************************************************
*! Name    : lfHoldKey
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose : Save and restore current trapping.
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
FUNCTION lfHoldKey
PARA lcKyOper
DO CASE
  CASE lcKyOper='S'
    lcHldTab = ON('KEY','TAB')
    lcHldBtb = ON('KEY','BACKTAB')    
    lcHldEsc = ON('KEY','ESC')
    lcHldAtB = ON('KEY','ALT+B')
    lcHldAtP = ON('KEY','ALT+P')
  CASE lcKyOper='R'
    ON KEY LABEL TAB     &lcHldTab
    ON KEY LABEL BACKTAB &lcHldBtb
    ON KEY LABEL ESC     &lcHldEsc
    ON KEY LABEL ALT+B   &lcHldAtB
    ON KEY LABEL ALT+P   &lcHldAtP
ENDCASE

*!*************************************************************
*! Name    : lfUpdBom
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose : Function to update po bill of material.
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
FUNCTION lfUpdBom
PARA lcAction
PRIVATE lnAlias

lnAlias  = SELECT(0)
llRetVal = .T.

*E301077,8 MAB Open Cut Ticket BOM file here [Begin
IF !llOpnKtBom
  llOpnKtBom = lfOpn_Rest(gcDataDir+'CTKTBOM','CTKTBOM')
ENDIF  
*E301077,8 MAB Open Cut Ticket BOM file here [End..

llNoThing  = SEEK("I"+laData[1], "CTktBom") 
llNoThing  = SEEK("P"+laData[1], "POSHDR")
lcLinkCode = CTktBom.Link_Code
lcWareCode = CTktBom.cWareCode
lcStyle    = IIF(lcAction='D',POSLN.Style,&lcPOLine..Style)
lnPoLNo    = IIF(lcAction='D',POSLN.LineNo,&lcPOLine..LineNo)
lnPrice    = IIF(lcAction='D',POSLN.nCost1,&lcPOLine..nCost1)
lcLastOpr  = POSHDR.cLastOpr

*E300935,1  Add Dyelot field to cost sheet items function [begin]
lcCostDye = IIF(lcAction='D',PADR(POSLN.Dyelot,10),PADR(&lcPOLine..Dyelot,10))
*E300935,1  Add Dyelot field to cost sheet items function [end]

*E301077,8 open files only if not opened.
IF !laOpFile[8]
  laOpFile[8] = gfOpenFile(gcDataDir+"MFGOprHd","MFGOprHd","SH")
ENDIF
IF !laOpFile[7]
  laOpFile[7] = gfOpenFile(gcDataDir+"FabDye","FabDye","SH")
ENDIF
IF !laOpFile[6]
  laOpFile[6] = gfOpenFile(gcDataDir+"Fabric","Fabric","SH")
ENDIF
*E301077,8 End.

IF lcAction = "D"
  DIME laBomQty[9]
  laBomQty = 0
ELSE
  SELECT (lcPOLine)
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laBomQty
ENDIF
laBomQty[9] = 0

FOR lnSize = 1 TO 8
  IF lcAction # "A"
    lcField = "POSLN.Qty" + STR(lnSize,1)
    laBomQty[lnSize] = laBomQty[lnSize] - &lcField
  ENDIF
  laBomQty[9] = laBomQty[9] + laBomQty[lnSize]
ENDFOR

*E300935,1  Add Dyelot field to cost items function. [Begin]
*llRetVal = gfSheetItem ("I"      , laData[1] , lcLinkCode, lcStyle    ,;
*                        ""       , lnPoLNo   , SPACE(10) , POSHDR.cItemWare ,;
*                        POSHDR.cMatWare, @laBomQty,  "Bom"     , "CtktBom"  ,;
*                        "BomLine", "MFGOprHd", lcLastOpr , lnPrice    ,;
*                        0, 0, 0, 0, 0,lcWinCh0)


*E301077,8 MAB Open BOMLINE file here [Begin
IF !llOpnBomLn
  llOpnBomLn = lfOpn_Rest(gcDataDir+'BOMLINE','BOMLINE')
ENDIF  
*E301077,8 MAB Open BOMLINE file here [End..

*E301077,8 MAB Open BOM file here [Begin
IF !llOpnBom
  llOpnBom = lfOpn_Rest(gcDataDir+'BOM','BOM')
ENDIF  
*E301077,8 MAB Open BOM file here [End..

*C200080,1 AMM Consider case of Dye Order 
*llRetVal = gfSheetItem ("I"      , laData[1] , lcLinkCode, lcStyle    ,;
                        ""       , lnPoLNo   , lcCostDye , POSHDR.cItemWare ,;
                        POSHDR.cMatWare, @laBomQty,  "Bom"     , "CtktBom"  ,;
                        "BomLine", "MFGOprHd", lcLastOpr , lnPrice    ,;
                        0, 0, 0, 0, 0,lcWinCh0)

*B606496,1 AMH Fix the bug of Incorrect PO Cost Sheet required qty because of using the bom file
*B606496,1     instead of bomline. If its a new line then we will use the bom file, otherwise
*B606496,1     we will simulate the bom file from the bomline[Start]
*llRetVal = gfSheetItem (IIF(lcAType="D","D","I"), laData[1] , lcLinkCode, lcStyle    ,;
                        ""       , lnPoLNo   , lcCostDye , POSHDR.cItemWare ,;
                        POSHDR.cMatWare, @laBomQty,  "Bom"     , "CtktBom"  ,;
                        "BomLine", "MFGOprHd", lcLastOpr , lnPrice    ,;
                        0, 0, 0, 0, 0,lcWinCh0)

IF lcAction = 'A'
  llRetVal = gfSheetItem (IIF(lcAType="D","D","I"), laData[1] , lcLinkCode, lcStyle    ,;
                          ""       , lnPoLNo   , lcCostDye , POSHDR.cItemWare ,;
                          POSHDR.cMatWare, @laBomQty,  "Bom"     , "CtktBom"  ,;
                          "BomLine", "MFGOprHd", lcLastOpr , lnPrice    ,;
                          0, 0, 0, 0, 0,lcWinCh0)
ELSE
  =lfModBom()
ENDIF
*B606496,1 AMH [End]

*C200080,1 AMM end

*B804256,1 AMH Call function to delete the related records in BomLIne 
*B804256,1 & CtktBom in case of deleting the style from posln. [Start]
IF lcAction='D'
  =lfRemCsElm(lcStyle)
ENDIF  
*B804256,1 AMH [End]

*B604618,1 AMH Call function to modify the related records in BomLine 
*B604618,1 & CtktBom in case of modifay the style from posln. [Start]
*B606496,1 AMH No need now to modify the related records after calling lfModBom [Start]
*IF lcAction='M'
*  =lfModCsElm(lcStyle)
*ENDIF  
*B606496,1 AMH [End]
*B604618,1 AMH [End]

*E300935,1  Add Dyelot field to cost items function. [Begin]

SELECT (lnAlias)
RETURN (llRetVal)

*!*************************************************************
*! Name    : lfChkOrdQt
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose : Check order allocated quantity.
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
FUNCTION lfChkOrdQt
PARA lcFldNo
PRIVATE lnAlias, llProceed
lnAlias   = SELECT(0)
llProceed = .T.
*C037957,1 MHM 10/10/2004 Custom Amend CutPick File[Start]
IF ASCAN(laEvntTrig , PADR('POUPCTPK',10)) <> 0
  IF lcProgName = "POSTY" AND gfDoTriger('POSTY',PADR('POUPCTPK',10))
    RETURN
  ENDIF
ENDIF     
*C037957,1 MHM [End]

*--Size xxxxx has xxx pieces allocated from orders.
*--Edit the detail allocated quantity from the order lines to keep 
*--track of the allocated quantity, Ignore tracking the allocation 
*--for only the quantity being modified, or for all later modifications.
*--<Edit Allo. Qty.>/<Ignore>/<Ignore All>/<Cancel>
lcAloPQty = ALLTRIM(STR(&lcPOline..Ord&lcFldNo,6))
lcStr     = ALLTRIM(SCALE.Sz&lcFldNo) + "|" +lcAloPQty
lnChoice = IIF(llIgnorAll,3,gfModalGen('QRM32016B32002','ALERT',lcStr) )
IF lnChoice = 4
  RETURN (.F.)  
ENDIF
lcCtPkKey = '2'+laData[1]+&lcPOline..Style

IF !USED(lcTmpCtPk)
  *-Create Temp Cut Pick file. 
  SELECT CUTPICK
  =AFIELDS(laFStru)
  lnFStru = ALEN(laFStru,1)
  DIMENSION laFStru[lnFStru+9,4]
  
  *B604420,1 KHM 04/24/2001 (Begin) add the appropriate field.
  *FOR I=1 TO 9
  * laFStru[lnFStru+I,1] = 'nCurPck'+STR(I,1)
  * laFStru[lnFStru+I,2] = 'N'
  * laFStru[lnFStru+I,3] = 6
  * laFStru[lnFStru+I,4] = 0
  *ENDFOR  
  FOR I=1 TO 8 
   laFStru[lnFStru+I,1] = 'nCurPck'+STR(I,1)
   laFStru[lnFStru+I,2] = 'N'
   laFStru[lnFStru+I,3] = 6
   laFStru[lnFStru+I,4] = 0
  ENDFOR
  laFStru[lnFStru+9,1] = 'nTotCur'
  laFStru[lnFStru+9,2] = 'N'
  laFStru[lnFStru+9,3] = 7
  laFStru[lnFStru+9,4] = 0
  *B604420,1 KHM 04/24/2001 (End)
  
  =gfCrtTmp(lcTmpCtPk,@laFStru,'Trancd+cTktNo+Style+cOrdLine+Order',lcTmpCtPk)
  SELECT (lcTmpCtPk)      
  SET ORDER TO 1

  *--Update umcomplete session file with new created file.
  SELECT UNCMSESS
  IF SEEK('O'+lcUnCmPgId+PADR(gcUser_id,10)+lcSession)
    =lfUpdVars(.T.)
  ENDIF

ENDIF
SELECT CUTPICK
SCAN REST WHILE TranCd+cTktNo+Style=lcCtPkKey
  SCATTER TO laFields
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laOdOrd
  IF !SEEK(lcCtPkKey+CUTPICK.cOrdLine+CUTPICK.Order,lcTmpCtPk)
    SELECT (lcTmpCtPk)
    APPEND BLANK
    GATHER FROM laFields
    *ram
    *GATHER FROM laOdOrd FIELDS nCurPck1,nCurPck2,nCurPck3,nCurPck4,nCurPck5,nCurPck6,nCurPck7,nCurPck8,nCurPck9
    GATHER FROM laOdOrd FIELDS nCurPck1,nCurPck2,nCurPck3,nCurPck4,nCurPck5,nCurPck6,nCurPck7,nCurPck8,nTotCur
  ENDIF
ENDSCAN

IF lnChoice = 1
  llProceed = lfEdtAloQty(INT(VAL(lcFldNo)))
ELSE
  SELECT (lcTmpCtPk)      
  =SEEK(lcCtPkKey)
  REPLACE REST WHILE TranCd+cTktNo+Style=lcCtPkKey ;
          Qty&lcFldNo WITH 0,;
          TotQty      WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
  SELECT (lcPOline)
  REPLACE Ord&lcFldNo WITH 0,;
          TotOrd      WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
  
  IF lnChoice = 3
    llIgnorAll = .T.
  ENDIF
ENDIF

SELECT(lnAlias)
RETURN (llProceed)


*:*************************************************************
*! Name      : lfEdtAloQty
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : To edit the allocated orders quantity.
*:*************************************************************
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
DO (gcScrDir+"MFCtPkTr.SPX")

POP KEY
RELEASE WINDOW (lcCPBrTtl)
SELECT (lnAlias)
RETURN (llRetVal)

*:*************************************************************
*! Name      : lfCtPkTrBr
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Browse the allocated orders lines.
*:*************************************************************
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

*:*************************************************************
*! Name      : lfwCPGtFld
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Save the old value of the current edited allocated
*!             order line quantity.
*:*************************************************************
FUNCTION lfwCPGtFld

lnOldQty = EVAL(VARREAD())
RETURN

*:*************************************************************
*! Name      : lfCPTab
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Tab trapping function for the order line 
*!             allocation browse.
*:*************************************************************
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

*:*************************************************************
*! Name      : lfCPStb
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Shit+Tab trapping function for the order line 
*!             allocation browse.
*:*************************************************************
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

*:*************************************************************
*! Name      : lfCPEsc
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Escape trapping function for the order line 
*!             allocation browse.
*:*************************************************************
FUNCTION lfCPEsc

ACTIVATE WINDOW CtPkTr3
_CUROBJ = OBJNUM(pbCan)
KEYBOARD "{ENTER}"

*:*************************************************************
*! Name      : lfvCPGtFld
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : To validate the edited allocated order line
*!              quantity.
*:*************************************************************
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
FUNCTION lfvCPGtFld
PARAMETER lnObj
PRIVATE lnObj, lcObj, lnAlias

lnAlias  = SELECT(0)
lcObj    = STR(lnObj,1)
lnNewQty = &lcTmpCtPk..Qty&lcObj
lnCurQty = &lcTmpCtPk..nCurPck&lcObj

SELECT (lcTmpCtPk)
IF lnNewQty > lnCurQty
  *-- The allocated quantity cannot be greater than 9999.
  *-- < Ok > 
  = gfModalGen('TRM38107B00000','DIALOG',ALLTRIM(STR(lnCurQty,6)) )
  REPLACE Qty&lcObj WITH lnOldQty
ELSE                 
  REPLACE TotQty  WITH TotQty - lnOldQty + lnNewQty
ENDIF
SELECT(lnAlias)
RETURN

*:*************************************************************
*! Name      : lfvCPOk
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Ok button in the allocated order lines brose screen 
*!             valid function.
*:*************************************************************
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
FUNCTION lfvCPOk
PRIVATE lnAlias

lnAlias    = SELECT(0)
lcSzNo     = STR(lnSizeNo,1)
lcCurToChk = "nCurPck" + lcSzNo
lnVarToChk = m.Qty&lcSzNo
lcFldToChk = "Qty"     + lcSzNo
llGoOut    = .F.
llRetVal   = .F.

SELECT (lcTmpCtPk)
*ram
*SUM ALL &lcFldToChk, TotQty, &lcCurToChk, nCurPck9 ;
    FOR TranCd+cTktNo+Style = lcCtPkKey  ;
    TO lnNewVal, lnNewTot, lnOldVal, lnOldTot

SUM ALL &lcFldToChk, TotQty, &lcCurToChk, nTotCur ;
    FOR TranCd+cTktNo+Style = lcCtPkKey  ;
    TO lnNewVal, lnNewTot, lnOldVal, lnOldTot

IF lnNewVal > lnVarToChk
  *-- The allocated quantity cannot be greater than 9999.
  *-- < Ok > 
  = gfModalGen('TRM38107B00000','DIALOG',ALLTRIM(STR(lnVarToChk,6)) )
ELSE
  llGoOut  = .T.
  llRetVal = .T.
ENDIF

IF llGoOut
  SELECT (lcPOline)
  REPLACE Ord&lcSzNo WITH lnNewVal ,;
          TotOrd     WITh TotOrd - lnOldTot + lnNewTot
  CLEAR READ
ENDIF
SELECT(lnAlias)


*:*************************************************************
*! Name      : lfvCPCan
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Cancel button in the allocated order lines brose 
*!             screen valid function.
*:*************************************************************
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
FUNCTION lfvCPCan
PRIVATE lnAlias

lnAlias    = SELECT(0)
llRetVal   = .F.
lcSzNo     = STR(lnSizeNo,1)

SELECT (lcTmpCtPk)
*ram
*REPLACE ALL Qty&lcSzNo WITH nCurPck&lcSzNo ,;
            TotQty     WITH nCurPck9
REPLACE ALL Qty&lcSzNo WITH nCurPck&lcSzNo ,;
            TotQty     WITH nTotCur

SELECT(lnAlias)



*:*************************************************************
*! Name      : lfvPOGnLns
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Generate Lines Based on Components Availability.
*:*************************************************************
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
FUNCTION lfvPOGnLns
PRIVATE lnAlias

lnAlias    = SELECT()

*--E301410,1 RAMY [start]
*llView     = IIF(laScrMode[3],laData[37]<>0 OR SEEK('2'+laData[1],'CUTPICK'), .F.)
llView     = IIF(laScrMode[3],laData[37]<>0 OR SEEK('2'+laData[1], IIF(USED(lcTmpCtPk) .AND. !EMPTY(EVALUATE(lcTmpCtPk+'.TRANCD')),lcTmpCtPk,'CUTPICK')), .F.)
*--E301410,1 RAMY [end]
lcRetFile  = gfTempName()
lnCtHdrRec = IIF(!EOF("POSHDR"),RECNO("POSHDR"),0)
llGnRetVal = .F.

*B603177,1 RAMY change this line to add the Dyelot parameter [start]
*DO (gcAppHome+"MFGenLns") WITH llView,"I",lcPOline,"Style","",;
                      "","cWareCode","Qty","TotQty",lcRetFile

*--E301410,1 RAMY [start]
*DO (gcAppHome+"MFGenLns") WITH llView,"I",lcPOline,"Style","",;
*                      "Dyelot","cWareCode","Qty","TotQty",lcRetFile

DO (gcAppHome+"MFGenLns") WITH llView,"I",lcPOline,"Style","",;
                      "Dyelot","cWareCode","Qty","TotQty",lcRetFile,lcTmpCtPk
*--E301410,1 RAMY [end]

*B603177,1 RAMY [end]

IF llGnRetVal
  
  *--new
  *SELECT (lcPOline)
  *BLANK ALL
  *DELETE ALL
  *APPEND FROM (gcWorkDir+lcRetFile)
  SELECT (lcRetFile)
  SCAN
    IF SEEK(Style,lcPoLine)
      SELECT (lcRetFile)
      SCATTER FIELDS QTY1 , QTY2 , QTY3 , QTY4 , QTY5 , QTY6 , QTY7 , QTY8 , TOTQTY , Ord1 , Ord2 , Ord3 , Ord4 , Ord5 , Ord6 , Ord7 , Ord8 , TOTOrd TO MEMVAR MEMO
      SELECT (lcPoLine)
      GATHER FIELDS QTY1 , QTY2 , QTY3 , QTY4 , QTY5 , QTY6 , QTY7 , QTY8 , TOTQTY , Ord1 , Ord2 , Ord3 , Ord4 , Ord5 , Ord6 , Ord7 , Ord8 , TOTOrd FROM MEMVAR MEMO
    ENDIF
  ENDSCAN
  
  
  *B602179,1 AMM Erase temporary files when quitting the program start
  *ERASE (gcWorkDir+lcRetFile)

  *B603177,1 RAMY Add the IF condition to close the file if opend before 
  *B603177,1      erase it
  IF FILE(gcWorkDir+lcRetFile+'.DBF')
    USE IN (lcRetFile)
    ERASE (gcWorkDir+lcRetFile+'.DBF')
    ERASE (gcWorkDir+lcRetFile+'.CDX')
    ERASE (gcWorkDir+lcRetFile+'.FPT')
  ENDIF
  *B603177,1 RAMY [end]
  *B602179,1 AMM end
  SELECT (lcPOline)
  SUM TotQty TO lnTotals
  STORE lnTotals TO laData[36],laData[40]

  llRecalTot = .T.
  SELECT (lcPOHdr)
  REPLACE nStyOrder WITH laData[36],;
          Open      WITH laData[40]
ENDIF 

IF lnCtHdrRec <> 0
  GOTO lnCtHdrRec IN POSHDR
ENDIF

SELECT (lcPOline)
GO TOP
=lfGetLine()
SELECT(lnAlias)
RETURN


*--UNCOMPLETE SESSION FUNCTIONS(UNCSS).
*!*************************************************************
*! Name    : lfChkUnComS
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
  laScrMode  = .F.
  *C200080,1 AMM Adjust to fit the dye order case
  *lcMsgTxt   = IIF(lcAType='C','contract',IIF(lcAType='R','return purchase order','purchase order'))
  *E301291,1 AMM Consider the case of adornment order
  *lcMsgTxt   = IIF(lcAType='D','dye order',IIF(lcAType='C','contract',IIF(lcAType='R','return purchase order','purchase order')))
  lcMsgTxt   = IIF(lcAType='A','Adornment order',IIF(lcAType='D','dye order',IIF(lcAType='C','contract',IIF(lcAType='R','return purchase order','purchase order'))))
  *E301291,1 AMM end
  *C200080,1 AMM end
  IF gfUnCompSession(lcUnCmPgId, lnSessNo, lcMsgTxt)
    llFondSess = .T.
    llContinue = .T.
    llShow     = .F.
    llNoShow   = .F.
    lcSession  = UNCMSESS.cSession
    lcCurObj   = ALLTRIM(UnCmSess.cCurrObj)
    SELECT (lcPOHdr)
    SCATTER FIELDS &lcScFields TO laData
    SELECT POSHDR
    SEEK lcAType+laData[1]
    *E301291,1 AMM Consider the case of adornment order
    *= SEEK( laData[2],IIF(lcAType='N','WAREHOUS','ApVendor') )
    = SEEK( laData[2],IIF(lcAType$'NA','WAREHOUS','ApVendor') )
    *E301291,1 AMM end
    SELECT (lcPOline)
    = lfvBrow1()    
    = lfGetInfo(.F.)
    llRecalTot = .T.
    llViewMode = laScrMode[3]
    IF llViewMode
      laScrMode[3]=.F.
      laScrMode[2]=.T.
      = lfGetStatus()
      laScrMode[3]=.T.
      laScrMode[2]=.F.
    ENDIF 
    IF !EMPTY(lcCurObj)
       DO CASE
         CASE lcCurObj = 'pbSav'
           DO lpSavScr
         CASE lcCurObj = 'pbDlt'
           DO lpDelScr
       ENDCASE
       llGoAndChk = .T.
       laScrMode  = .F.
       IF llViewMode
         laScrMode[2] = .T.
       ELSE
         laScrMode[1] = .T.
       ENDIF
    ENDIF
    SHOW GETS 
    IF lnShpLoc=1
      SHOW GET laData[69]
      SHOW GET laData[19]
    ELSE
      SHOW GET laData[19]
      SHOW GET laData[69]
    ENDIF
  ELSE
    IF llExCalled
      laScrMode[2] = .T.   
    ELSE
      laScrMode[1] = .T.
    ENDIF
    =lfCrtTempFl()
  ENDIF
ENDIF
= lfvBrow2()    
= lfvBrow1()
RETURN (llFondSess)


*!*************************************************************
*! Name      : lfCrtTempFl
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : To create all the uncompete session temprory files
*!             that is needed by this program.
*!*************************************************************
*! Call      : gfCrtTmp()
*!*************************************************************
FUNCTION lfCrtTempFl

*-Create PO Header file. 
SELECT POSHDR
=AFIELDS(laStrufile)
lnFileStru = ALEN(laStrufile,1)
DIMENSION laStrufile[lnFileStru+1,4]
laStrufile[lnFileStru+1,1] = 'nSteps'
laStrufile[lnFileStru+1,2] = 'N'
laStrufile[lnFileStru+1,3] = 2
laStrufile[lnFileStru+1,4] = 0
=gfCrtTmp(lcPOHdr,@laStrufile,'cStyType+PO',lcPOHdr)
SELECT (lcPOHdr)
APPEND BLANK 
REPLACE cStyType WITH lcAType,lMultiWare WITH llMultiWare

*-Create line file. 
SELECT POSLN
=AFIELDS(laStrufile)
lnFileStru = ALEN(laStrufile,1)

*E301520,1 AME[Start] Increase the array size to hold new fields(CustName,Start date AND Complete date) 
*DIMENSION laStrufile[lnFileStru+1,4]
DIMENSION laStrufile[lnFileStru+4,4]
*E301520,1 AME[End]

laStrufile[lnFileStru+1,1] = 'nSteps'
laStrufile[lnFileStru+1,2] = 'N'
laStrufile[lnFileStru+1,3] = 2
laStrufile[lnFileStru+1,4] = 0

*E301520,1 AME[Start] Add the elements of Customer name ,start date AND complete date to array. 
laStrufile[lnFileStru+2,1] = 'cAccName'
laStrufile[lnFileStru+2,2] = 'C'
laStrufile[lnFileStru+2,3] = 30
laStrufile[lnFileStru+2,4] = 0
laStrufile[lnFileStru+3,1] = 'Start'
laStrufile[lnFileStru+3,2] = 'D'
laStrufile[lnFileStru+3,3] = 8
laStrufile[lnFileStru+3,4] = 0
laStrufile[lnFileStru+4,1] = 'Complete'
laStrufile[lnFileStru+4,2] = 'D'
laStrufile[lnFileStru+4,3] = 8
laStrufile[lnFileStru+4,4] = 0
*E301520,1 AME[End]

*B608125,1 TMI [Start] Add a field "lRefCost" , used to check if costing of this line is refreshed by the fucntion lfCstShUpd
lnFileStru = ALEN(laStrufile,1) + 1
DIMENSION laStrufile[lnFileStru,4]
laStrufile[lnFileStru,1] = 'lRefCost'
laStrufile[lnFileStru,2] = 'L'
laStrufile[lnFileStru,3] = 1
laStrufile[lnFileStru,4] = 0
*B608125,1 TMI [End  ] 

=gfCrtTmp(lcPOline,@laStrufile,'Style+STR(LineNo,6)',lcPOline)
=gfCrtTmp(lcPOTran,@laStrufile,'Style+STR(LineNo,6)',lcPOTran)

*E301869,1 HBG 07/04/2002 Creating the temp files nedded in generating project[Begin]  
IF lcGenProj $ 'AI'
  =lfCrtprjTm()
ENDIF
*E301869,1 [End]

*B802469,1 WAB  set relation between temp poline and shipment header
*B802469,1 WAB - START

SELECT (lcPOTran)
SET RELATION TO EVAL(lcPOTran+'.shipno') INTO Shpmthdr ADDITIVE

*B802469,1 WAB - END

*B605932,1 AMH Make relation with custom table for KRA10 [Start]
IF ASCAN(laEvntTrig , PADR('MAKREL',10)) <> 0
  =gfDoTriger('POSTYLE',PADR('MAKREL',10))
ENDIF
*B605932,1 AMH [End]

SELECT STYLE
SET ORDER TO TAG STYLE
SELECT (lcPOline)
*E301291,1 AMM Add line number to relation to get the exactly related records
*SET RELA TO Style INTO (lcPOTran)
SET RELA TO Style+STR(LINENO,6) INTO (lcPOTran)
*E301291,1 AMM end
SELECT (lcPOline)
RETURN


*:*************************************************************
*! Name      : lfCrUnSess
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Create uncomplete session record.
*:*************************************************************
*! Call      : lfUpdVars()
*!*************************************************************
*! Passed Parameters  :lcPushBt  -> push botton
*!                     lcUnCmSta -> status
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfCrUnSess()
*!*************************************************************
FUNCTION lfCrUnSess
PARA lcPushBt,lcUnCmSta

lnAlias = SELECT()
SELECT UNCMSESS
IF lcUnCmSta='O'
  IF !SEEK('O'+lcUnCmPgId+PADR(gcUser_id,10)+lcSession)
    IF !SEEK('I'+lcUnCmPgId+PADR(gcUser_id,10)) 
      
      *-- MAN Start
      *B602619,1 Fix uncomplete session bug
	  lcSvdPo    = " "
	  *-- MAN End

      INSERT INTO UNCMSESS ;
      (Status,cUTranType,cUserId,cSession,cProgram,cCurrScr,dTranDate,cTranTime,cCurrObj) VALUES ;
      ('O',lcUnCmPgId,gcUser_id,lcSession,lcAProgm,'',gdSysDate,TIME(),lcPushBt)
    ELSE
      *B602619,1 Fix uncomplete session bug
	  lcSvdPo    = " "
	  
    
      REPLACE Status    WITH 'O',;
              cSession  WITH lcSession,;
              dTranDate WITH gdSysDate,;
              cTranTime WITH TIME(),;
              ccurrobj  WITH lcPushBt
    ENDIF
  ELSE
    REPLACE cCurrObj WITH lcPushBt
  ENDIF
  =lfUpdVars(.F.)
  =RLOCK('UNCMSESS')

ELSE  && lcUnCmSta $ 'IC'
 
  *--Cancel OR Complete unCmSess record.
  IF SEEK('O'+lcUnCmPgId+PADR(gcUser_id,10)+lcSession)

    REPLACE Status WITH lcUnCmSta
    UNLOCK

  ENDIF

ENDIF
SELECT(lnAlias)
RETURN


*!*************************************************************
*! Name      : lfUpdVars
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Function to create a string to be saved in the field
*!             UNCMSESS.mComent and UNCMSESS.mTmpFiles
*!*************************************************************
*! Call      : gfSavSess()
*!*************************************************************
*! Passed Parameters  :llUpdCutPik-> .T. for Update cutpick file.
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfUpdVars()
*!*************************************************************
FUNCTION lfUpdVars
PARA llUpdCutPik

IF llUpdCutPik
  lcFiles = "lcPOHdr,"+lcPOHdr+","+lcPOHdr+";lcPOLine,"+lcPOLine+","+lcPOLine+;
            ";lcPOTran,"+lcPOTran+","+lcPOTran+";lcTmpCtPk,"+lcTmpCtPk+","+lcTmpCtPk+";"
ELSE
  lcFiles = "lcPOHdr,"+lcPOHdr+","+lcPOHdr+";lcPOLine,"+lcPOLine+","+lcPOLine+";lcPOTran,"+lcPOTran+","+lcPOTran+";"
ENDIF
= gfSavSess(lcUnCmPgId, lcFiles, @laVariables,lcSession)
RETURN





*:*************************************************************

*##T#-#-# ToolBar functions.

*!*************************************************************
*! Name      : lpClsScr
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Cancel P/O.
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
FUNCTION lpClsScr

*--Cancel unCmSess record.
=lfCrUnSess(' ','I') 
llContinue = .F.

SELECT POSHDR
IF EMPTY(laData[1])
  GO TOP
ELSE  
  =SEEK(lcAType+laData[1])
ENDIF
IF laScrMode[3]
  *--Empty lines file.
  SELECT (lcPOline)
  BLANK ALL
  DELETE ALL
  SELECT (lcPOTran)
  lcStFlt = SET('FILTER')
  SET FILTER TO 
  BLANK ALL
  DELETE ALL
  SET FILTER TO &lcStFlt
  =lfGetInfo(.F.)
ENDIF
*C200162,1 MHM 02/27/2001 (Begin) Releasing the trapping.
IF lcAtype = "N" AND laScrMode[4] AND ASCAN(laEvntTrig , PADR('ADDOPTN',10)) <> 0
  ON KEY LABEL Ctrl+A
ENDIF
*C200162,1 MHM 02/27/2001 (End)

RETURN


*!*************************************************************
*! Name      : lpDelScr
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Cancel / UnCancel P/O.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lpDelScr()
*!*************************************************************

PROCEDURE lpDelScr

*E301484,1 WAM Call new global save function
DO gfDelPo IN (gcapphome+'POUPDATE.PRG')

*E301969,1 AMH Referesh sales order data after cancel PO [Start]
*--Clear old p/o information.
=lfClearInfo()

*--Get new P/o information.
= lfGetInfo(.F.)
*E301969,1 AMH [End]

*E301869,1 HBG 07/04/2002 Cancel the project on this P/O[Begin]
=lfUpdPrjSt()
*E301869,1 [End]

RETURN


*:*************************************************************
*! Name     : lpSavScr
*! Purpose  : Save/Update P/O.
*:*************************************************************
PROCEDURE lpSavScr

*E301869,1 HBG 07/04/2002 Generate project for this SO acoording to the setup of the module[Begin]
IF laScrMode[4]
  llNewPo = .T.
ELSE
  llNewPo = .F.
ENDIF
*E301869,1 [End]

*E301484,1 WAM Call new global save function
DO gfSavePo IN (gcapphome+'POUPDATE.PRG')

*E301869,1 HBG 07/04/2002 Generate project for this SO acoording to the setup of the module[Begin]

*B038431,1 NNA 09/05/2004 (Begin) Add new Condition to Generate Project that If 'lcGenProj' Defined Before
*IF lcGenProj $ 'AI' AND llNewPo 
IF TYPE('lcGenProj') <> 'U' .AND. lcGenProj $ 'AI' AND llNewPo 
  =lfGenProj()
ENDIF
*B038431,1 NNA (End)

*E301869,1 [END]

RETURN


*!************************************************************************
*! Name : lfGtOrder.
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose : Edit the P/o number manualy  if the system was setup to do so.
*!*************************************************************************
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
FUNCTION lfGtOrder
PRIVATE lcPONum
lcPONum   = SPACE(6)
lcSctTtle = lcPOType+' Number'
lcScrText = 'Please enter the '+lcPOType+' number'
lcHdEsc1= ON('KEY','ESC')
ON KEY LABEL ESC 
DO (gcScrDir+"POGNum.SPX")
ON KEY LABEL ESC &lcHdEsc1
RETURN (lcPONum)

*:*************************************************************
*! Name      : lfvPoNum
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Validate P/O no in entered manualy.
*:*************************************************************
FUNCTION lfvPoNum

llRet=.T.
IF EMPTY(lcPONum)
  *-You cannot leave the "+lcPOType+" number empty.
  =gfModalGen('TRM34021B34000','DIALOG',lcPOType)
  llRet=.F.
ENDIF
IF llRet AND LEN(ALLTRIM(lcPONum)) < 6
  *-lcPOType+" number must be six digits.
  =gfModalGen('TRM34022B34000','DIALOG',lcPOType)
  llRet=.F.
ENDIF
IF llRet AND SEEK(lcAType+lcPONum,"POSHdr")
  *-This "+lcPOType+" number is already exist.
  =gfModalGen('TRM34023B34000','DIALOG',lcPOType)
  llRet=.F.
ENDIF
RETURN llRet


*!*************************************************************
*! Name      : lfLcPo
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Function to update the Letter of Credit information.
*!PARA: 'M'->Modified,'X'->Canceled.
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
FUNCTION lfLcPo
PARAMETERS lcPOKind,lcLCno,lnPOamt

IF !SEEK(lcLCno,'Lc')
  RETURN
ENDIF
LnLcBal = Lc.nLcBalance
*-Get all open POs amount.
SELECT POSLN
lnSavRc1=IIF(!EOF(),RECNO(),0)
SELECT POSHDR
lnSavRc2=RECNO()
lnCurTag=TAG()
SET ORDER TO TAG PosHdrLC
=SEEK(lcLCno)
lnTPOamt = 0
SCAN WHILE Lc=lcLCno FOR cLCStatus='A'
   lnTPOamt = lnTPOamt + lfPoOpnAmt()
ENDSCAN
SET ORDER TO TAG &lnCurTag
GOTO lnSavRc2
SELECT POSLN
IF lnSavRc1<>0
  GOTO lnSavRc1
ENDIF

*-Get available and amendments amounts.
lnAvlAmt=(LnLcBal-lnTPOamt)
IF lcPOKind='X'
  lnAmmdAmt=-(lnPOamt)
ELSE  && 'M'
  lnAmmdAmt=IIF(lnPOamt>lnAvlAmt,(lnPOamt-lnAvlAmt),0)
ENDIF

*-Call LC screen.
lcWindTtl='LC # '+lcLCno
STORE 1 TO pbOk,pbCancel
DO (gcScrDir+"POStyLC.SPX")
RETURN
*:*************************************************************
*! Name      : lfvLCAmam
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Validate LC amount if Ok enable ok button.
*:*************************************************************
FUNCTION lfvLCAmam
SHOW GET pbOk ENABLE

*:*************************************************************
*! Name      : lfvLCPOok
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Validate P/O LC Ok.
*:*************************************************************
FUNCTION lfvLCPOok

SELECT LC  
REPLACE nLcBalance WITH nLcBalance + lnAmmdAmt
*--Call TraceKey global function.
=gfTraceKey('LC',clctype+clcno,'M')

*E301077,8 open files only if not opened.
IF !laOpFile[10]
  laOpFile[10]= gfOpenFile(gcDataDir+'LcJournl','LcJournl','SH')
ENDIF
*E301077,8 End.
SELECT LcJournl
APPEND BLANK
REPLACE clcNo      WITH lcLCno,;
        dTranDate  WITH gdSysDate,;
        nTrnAmunt  WITH ABS(lnAmmdAmt),;
        cReference WITH SPACE(10),;
        cPoNo      WITH laData[1],;
        cTrnType   WITH IIF(lcPOKind='X','4','3'),;
        nLcBal     WITH LnLcBal + lnAmmdAmt
*--Call TraceKey global function.
=gfTraceKey('LCJOURNL',clctype+clcno,'M')
RETURN


*!*************************************************************
*! Name : Function lfPoOpnAmt
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Synopsis : Calcualate the open amount for the current po.
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
FUNCTION lfPoOpnAmt
PRIVATE lnOpnAmnt
STORE 0 TO lnOpnAmnt
SELECT PosLn
SEEK laData[1]
SCAN WHILE Po = laData[1] FOR TranCd <> '3'
  lnOpnAmnt = lnOpnAmnt + IIF(TranCd='1',1,-1)*(PosLn.TotQty * PosLn.nCost1)
ENDSCAN
SELECT POSHDR
RETURN IIF( lnOpnAmnt < 0, 0, lnOpnAmnt )


*!*************************************************************
*! Name : lfStyInBom.
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Synopsis : Check if a specefic style exist in a PO Bill of Material
*!            and a receiving has been occour for this style.
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
FUNCTION lfStyInBom
PARAMETERS lcOldLineNo, lcPo
PRIVATE lcAlias, llStyInBom
lcAlias    = ALIAS()
llStyInBom = .F.

*E301077,8 MAB Open BOMLINE file here [Begin
IF !llOpnBomLn
  llOpnBomLn = lfOpn_Rest(gcDataDir+'BOMLINE','BOMLINE')
ENDIF  
*E301077,8 MAB Open BOMLINE file here [End..

SELECT BomLine
IF SEEK('I2'+lcPo+PADR(lcOldLineNo,4))
  LOCATE REST WHILE Cimtyp+cType+cTktNo+LineNo='2'+lcPo+PADR(lcOldLineNo,4);
  FOR !EMPTY(cRSession) .AND. !lVoid
  IF FOUND()
    llStyInBom = .T.
  ENDIF
ENDIF
SELECT (lcAlias)
RETURN(llStyInBom)


*:*************************************************************
*! Name      : lfvNote
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : P/O Notepad.
*:*************************************************************
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
FUNCTION lfvNote

*--At save level the temp no will replaced with P/O no.
*--When cancel this session the temp no line will removed from notepad.
*E301077,8 open files only if not opened.
IF !laOpFile[9]
  laOpFile[9]= gfOpenFile(gcDataDir+'NOTEPAD','NOTEPAD','SH')
ENDIF
*E301077,8 End.
*--Call P/O NotePad.
=NOTEPAD( 'P' , laData[1] )
RETURN


*!*************************************************************
*! Name      : lfvEditCst
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Function to edit costs for header.
*  (No detail for imported style only)
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
FUNCTION lfvEditCst

lcSctTtle = lcPOType+' Costs'
DO (gcScrDir+"POSCOST.SPX")

IF llRecalTot
  =lfRecalc()
ENDIF

IF laData[32]<>&lcPOhdr..nICost2
  laData[52] = lfvFrnCost('2',laData[32],laData[29],lnCurrUnt2)
ENDIF
IF laData[33]<>&lcPOhdr..nICost3
  laData[53] = lfvFrnCost('3',laData[33],laData[29],lnCurrUnt2)
ENDIF
IF laData[34]<>&lcPOhdr..nICost4
  laData[54] = lfvFrnCost('4',laData[34],laData[29],lnCurrUnt2)
ENDIF
IF laData[35]<>&lcPOhdr..nICost5
  laData[55] = lfvFrnCost('5',laData[35],laData[29],lnCurrUnt2)
ENDIF
SHOW GETS WINDOW (lcWinCh3) ONLY
=lfRefresh(lcWinCh3)

SELECT (LCPOLINE)
SCAN
  IF laData[32]<>&lcPOhdr..nICost2
    REPLACE nECost2 WITH (laData[32]/laData[40])
    REPLACE nCost2  WITH lfvFrnCost('2',nECost2,laData[29],lnCurrUnt2)
  ENDIF
  IF laData[33]<>&lcPOhdr..nICost3
    REPLACE nECost3 WITH (laData[33]/laData[40])
    REPLACE nCost3  WITH lfvFrnCost('3',nECost3,laData[29],lnCurrUnt2)
  ENDIF
  IF laData[34]<>&lcPOhdr..nICost4
    REPLACE nECost4 WITH (laData[34]/laData[40])
    REPLACE nCost4  WITH lfvFrnCost('4',nECost4,laData[29],lnCurrUnt2)
  ENDIF
  IF laData[35]<>&lcPOhdr..nICost5
    REPLACE nECost5 WITH (laData[35]/laData[40])
    REPLACE nCost5  WITH lfvFrnCost('5',nECost5,laData[29],lnCurrUnt2)
  ENDIF
ENDSCAN
=lfRecalc()

SELECT (lcPOHdr)
GATHER FROM laData FIELDS &lcScFields
SELECT POSHDR
RETURN

*!*************************************************************
*!  Function : lfvObjLnk
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Object link.
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
DO GetObj WITH 'P',laData[1]
SELECT(lnalias)
RETURN

*!*************************************************************
*! Name      : lfvGenCstSH
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Function to generate the cost sheet.
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
FUNCTION lfvGenCstSH

IF lcCrtCSH = 'I' 
  *--Would you like to create the purchase order cost sheet ?
  *B603625,1 AMH Fix the confirmation message of creating cost sheet in case of Dye order [Start]
  *IF gfModalGen('QRM34064B34001','DIALOG') = 2
  IF gfModalGen('QRM34064B34001','DIALOG',IIF(lcAType='D','Dye Order','PO')) = 2
  *B603625,1 AMH [End]
    RETURN
  ENDIF  
ENDIF


laScrMode=.F.
laScrMode[1]= .T.
llNoShow    = .F.
llCallShow  = .T.
llShow      = .F.
*E301085,1 Generate cost sheet automatically
*lcParameter = "'" + laData[1] + "'"
lcParameter = "'" + laData[1] + "'"+IIF(lcCrtCSH="T",",.T.",',.F.')
*E301085,1 (End)

*B802203,1 Change the called module id from MF to PO.
*DO gpDoProg WITH "AWRPOCSSH", .F., "MF", lcParameter
*C200080,1 AMM call dye order cost sheet in case of dye order.
*DO gpDoProg WITH "AWRPOCSSH", .F., "PO", lcParameter
IF lcAType <> "D"
  DO gpDoProg WITH "AWRPOCSSH", .F., "PO", lcParameter
ELSE
  DO gpDoProg WITH "AWRMFDCSH", .F., "MF", lcParameter  
ENDIF  
RETURN


*!*************************************************************
*! Name      : lfAppndPk
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Function to append lines form pack.
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
FUNCTION lfAppndPk

lnAlias = SELECT()
*E301077,8 open files only if not opened.
IF !laOpFile[2]
  laOpFile[2]= gfOpenFile(gcDataDir+'sPck_Lin','sPck_Lin','SH')
ENDIF
IF !laOpFile[3]
  laOpFile[3]= gfOpenFile(gcDataDir+'sPck_hdr','sPck_hdr','SH')
ENDIF
*E301077,8 End.

SELECT SPCK_LIN
SET RELATION TO Style INTO Style
SELECT SPCK_HDR
SET FILTER TO Type='P' AND cDivision=ALLTRIM(laData[4])
GO TOP
IF EOF()
  *-No packs found for this division.
  =gfModalGen('TRM34008B34000','DIALOG')
  SET FILTER TO
  SELECT(lnAlias)
  RETURN  
ENDIF

DIME laTempArr[2]
laTempArr=' '
STORE 0 TO PTOTWIP,PTOTSTK,PTOTORD,PTOTQTY
PRIVATE lcBrFields
lcBrfields = "Pack_id :20 :H='Pack Id',"+;
		     "Desc    :20 :H='Description',"+;
             "xAct=IIF(SEEK('M'+Account,'CUSTOMER'),CUSTOMER.BtName,'GENERIC PACK') :20 :H='Account',"+;
        	 "xSsn=gfCodDes(Season,'SEASON') : 20 :H='Season',"+;
        	 "xDiv=gfCodDes(cDivision,'CDIVISION') : 20 :H='Division',"+;
             "xPkQ1=lfGetPQt() : 6 :H='Qty.  '," +;
             "xPkQ2=INT(PTOTWIP) : 6 :H='WIP.  '," +;
             "xPkQ3=INT(PTOTSTK) : 6 :H='Stk.  '," +;
             "xPkQ4=INT(PTOTORD) : 6 :H='Ord.  '"
             
IF !gfBrows('','Account,Pack_Id', 'laTempArr','Packs')
  llRetPak = .T.
ELSE
  llRetPak = .F.
  lcPackid = laTempArr[2]
  lcAcount = laTempArr[1]
  *--Check pack styles quality.
  SELECT STYLE
  SET ORDER TO TAG StyQlty
  lcQltyKey=laStyGrd[lnGrade,2]
  SELECT SPCK_LIN
  SEEK 'P'+lcAcount+lcPackid
  SCAN WHILE Type+Account+Pack_Id='P'+lcAcount+lcPackid
    IF !SEEK(lcQltyKey+SPCK_LIN.Style,'STYLE'))
      *-Some styles in this pack has a different quality, Cannot accept.
      =gfModalGen('TRM34066B34000','DIALOG')
      llRetPak = .T.
      EXIT
    ENDIF
  ENDSCAN
  SELECT STYLE
  SET ORDER TO TAG Style
ENDIF

SELECT SPCK_HDR
SET FILTER TO

IF llRetPak
  SELECT(lnAlias)
  RETURN  
ENDIF

lnPkQty = 1
lcTrn_Ttl = 'Packs Lines'
llPackOk = .F.
DO (gcScrDir+"POAPPAK.SPX")

IF ! llPackOk
  SELECT(lnAlias)
  RETURN  
ENDIF

WAIT WINDOW 'Creating the PO lines from pack lines. Please standby ...' NOWAIT
SELECT SPCK_LIN
SEEK 'P'+lcAcount+lcPackid
SCAN WHILE Type+Account+Pack_Id='P'+lcAcount+lcPackid

  m.Style=SPCK_LIN.Style
  =SEEK(m.Style,'STYLE') 
  *--Check the style status.
  IF STYLE.Status = 'X'
    *-Style:xxStyle was cancelled. Cannot add to the P.O.
    =gfModalGen('TRM34011B34000','DIALOG',SPCK_LIN.Style+'|'+lcPOType)
    LOOP
  ENDIF
  *--Check the style make.
  IF STYLE.Make              
    *-Style: xxStyle is not a purchased style. Cannot add to the P.O.
    =gfModalGen('TRM34010B34000','DIALOG',SPCK_LIN.Style+'|'+lcPOType)
    LOOP
  ENDIF
  IF lcAType='C' AND lfVldSCnr()
    *-The Style:valid in another contract for the same vendor and period, Cannot proceed!
    =gfModalGen('TRM34018B34000','DIALOG',m.Style)
    LOOP
  ENDIF
  *--Check if the style already exist in lines.
  SELECT (lcPOLine)
  IF SEEK(m.Style)
    *-Style: xxStyle already exist in P/O' add/next
    IF gfModalGen('QRM34049B34005','DIALOG',SPCK_LIN.Style+'|'+lcPOType)=2
      LOOP
    ENDIF
  ENDIF
      
  *--Check the style in warehouse.
  IF llWareHous
    IF ! llMultiWare
      IF !SEEK(SPCK_LIN.STYLE+laData[19]+SPACE(10),"STYDYE")
        IF gfModalGen('QRM34046B34001','DIALOG',ALLTRIM(SPCK_LIN.Style)+'|'+ALLTRIM(spck_lin.pack_id)+'|'+laData[19]) = 1
          *-Style: xxx in the pack xxx is not assigned to the location xxx, Assign this Style to the default location?
          WAIT "Assigning Style:"+ALLTRIM(STYLE)+" to location "+laData[19] WINDOW NOWAIT
          DO gpAdStyWar WITH SPCK_LIN.STYLE,SPACE(10),laData[19]
          WAIT CLEAR
        ELSE
          LOOP
        ENDIF
      ENDIF
    ELSE
      IF !SEEK(SPCK_LIN.STYLE+STYLE.CDEFWARE+SPACE(10),"STYDYE")
        IF gfModalGen('QRM34047B34001','DIALOG',ALLTRIM(SPCK_LIN.Style)+'|'+ALLTRIM(spck_lin.pack_id)+'|'+ALLTRIM(STYLE.cDefWare)) = 1
          *-Style: xxx in the pack xxx is not assigned to its default location, Assign this Style to its default location: xxx ?
          WAIT "Assigning Style: "+ALLTRIM(STYLE)+" to location "+STYLE.cDefWare WINDOW NOWAIT
          DO gpAdStyWar WITH SPCK_LIN.STYLE,SPACE(10),STYLE.cDefWare
          WAIT CLEAR
        ELSE
          LOOP
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  SELECT (lcPOLine)
  SCATTER MEMVAR BLANK
  *E302213,1 Store pack ID in POSLN file
  m.pack_id = lcPackid
  *E302213,1 (END)
  m.Style  = SPCK_LIN.Style
  m.cStyType = lcAType
  m.Trancd = '1'
  lnLines  = lnLines + 1
  m.LineNo = lnLines
  m.Po     = laData[1]
  m.Vendor = laData[2]
  m.Scale  = Style.Scale
  m.nCost1 = IIF(!llMulCurr OR STYLE.cPriceCur=laData[26],Style.nICost1,0)
  m.nCost2 = IIF(!llMulCurr OR STYLE.cDutyCur =laData[27],Style.nICost2,0)
  m.nCost3 = IIF(!llMulCurr OR STYLE.cDutyCur =laData[27],Style.nICost3,0)
  m.nCost4 = IIF(!llMulCurr OR STYLE.cDutyCur =laData[27],Style.nICost4,0)
  m.nCost5 = IIF(!llMulCurr OR STYLE.cDutyCur =laData[27],Style.nICost5,0)
  m.nECost1= lfvEquCost('1',m.nCost1,laData[28],lnCurrUnt1)
  m.nECost2= lfvEquCost('2',m.nCost2,laData[29],lnCurrUnt2)
  m.nECost3= lfvEquCost('3',m.nCost3,laData[29],lnCurrUnt2)
  m.nECost4= lfvEquCost('4',m.nCost4,laData[29],lnCurrUnt2)
  m.nECost5= lfvEquCost('5',m.nCost5,laData[29],lnCurrUnt2)
  m.cWareCode=IIF(llWareHous AND llMultiWare,STYLE.cDefWare,laData[19])

  INSERT INTO (lcPOLine) FROM MEMVAR

  SELECT SPCK_LIN
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty MEMVAR
  IF lnPkQty > 1
    m.TotQty=0
    FOR I=1 TO 8
      z=STR(I,1)
      m.Qty&z = m.Qty&z * lnPkQty
      m.TotQty= m.TotQty + m.Qty&z
    ENDFOR 
  ENDIF 
  SELECT (lcPOLine)
  GATHER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty
  laData[36] = laData[36] + &lcPOLine..TotQty
  laData[40] = laData[40] + &lcPOLine..TotQty
  llRecalTot = .T.

ENDSCAN
WAIT CLEAR
SELECT (lcPOLine)
GO TOP
=lfGetLine()

SELECT SPCK_HDR
SET FILTER TO
SELECT(lnAlias)
RETURN  

*:*************************************************************
*! Name      : lfGetPQt
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Get pack Qty to browse.
*:*************************************************************
FUNCTION lfGetPQt

SELECT SPCK_LIN
SEEK 'P'+SPCK_HDR.ACCOUNT + SPCK_HDR.PACK_ID
SUM REST WHILE Type+ACCOUNT+PACK_ID = 'P'+SPCK_HDR.ACCOUNT + SPCK_HDR.PACK_ID ;
         STYLE->TOTWIP,STYLE->TOTSTK,STYLE->TOTORD,SPCK_LIN->TOTQTY ;
         TO PTOTWIP,PTOTSTK,PTOTORD,PTOTQTY
SELECT SPCK_HDR    
RETURN (INT(PTOTQTY))

*:*************************************************************
*! Name      : lfBrSPkLn
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Browse pack line.
*:*************************************************************
FUNCTION lfBrSPkLn

SELECT SPCK_LIN
lcBrPkKey='P'+lcAcount+lcPackid
BROWSE FIELDS Style :R,;
              Qty1  :R,;
              Qty2  :R,;
              Qty3  :R,;
              Qty4  :R,;
              Qty5  :R,;
              Qty6  :R,;
              Qty7  :R,;
              Qty8  :R,;
              TotQty :R ;
       KEY lcBrPkKey LOCK 0 ;
       NOAPPEND NOCLEAR NODELETE NOWAIT SAVE NOMENU   ;
       TITLE (lcTrn_Ttl) WINDOW POAPPK2 IN WINDOW POAPPAK


*!*************************************************************
*! Name      : lfAppndPO
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Function to append lines form P/O.
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
FUNCTION lfAppndPO



lnAlias = SELECT()
lcTrn_Ttl = "Trans_Lines"

*B602884,1 Select new P/O only if no line entered and no P/O# selected.
IF EMPTY(laData[71])
  IF !lfCkEOF(lcPoLine)
    *--You cannot append from P/O, since some lines was entered.
    =gfModalGen('TRM34157B42000','DIALOG')
    RETURN
  ENDIF  
*B602884,1 End.
  lcPOBrow  = laData[1]
  lcVenBrow = laData[2]
  lcOldAType = lcAType 
  lcOldPtype = lcPOType
  DO POSBrow WITH lcPOBrow,lcVenBrow,'P'
  lcAType = lcOldAType
  lcPOType= lcOldPtype
  IF EMPTY(lcPOBrow)
    SELECT (lnAlias)
    RETURN
  ENDIF
ELSE
  lcPOBrow  = laData[71]
  lcVenBrow = laData[2]
ENDIF


IF SEEK('P'+lcPOBrow,'POSLN') AND SEEK(POSLN.Style,'STYLE') AND ;
  INT(VAL(STYLE.cStyGrade)) <> lnGrade
   *--P/O # XXX has a different quality styles than the current return P/O.
   =gfModalGen('TRM34116B42000','DIALOG',lcPOBrow)
  SELECT (lnAlias)
  RETURN
ENDIF

SELECT POSHDR
lnSvRc=RECNO()
SET FILTER TO 
lcDivisn=IIF(SEEK('P'+lcPOBrow),cDivision,'')
SET FILTER TO cStyType = lcAType
GOTO lnSvRc
IF lcDivisn<>ALLTRIM(laData[4])
  *-The selected P/O has a different division, Cannot accept.
  =gfModalGen('TRM34012B34000','DIALOG',lcPOType )
  SELECT (lnAlias)
  RETURN
ENDIF


*B602884,1 Commented, Open this file in program insted on this plase.
*lcTmpPOLn = gfTempName()
*B602884,1 end.

*--Get P/O lines.
=lfSelPoLin()

SELECT(lcTmpPOLn)
GO TOP
IF EOF()
  *-No recieving lines for P/O 999999 .
  =gfModalGen('TRM34006B34000','DIALOG','purchase order'+'|'+lcPOBrow+' remaining' )
  SELECT (lnAlias)
  RETURN
ENDIF


*B602884,1 Added.
GO TOP IN (lcPOLine)
SELECT(lcTmpPOLn)
lcRSess  = cRsession
lcSQlty  = cRetSty
lcTrnCd  = TranCd
LOCATE FOR cRsession <> lcRSess AND cRetSty<>lcSQlty AND TranCd<>lcTrnCd
llAppAll = !FOUND() AND EOF(lcPOLine)
IF llAppAll
  *-Copy all P/O 999999 lines ? Y/N
  llAppAll = (gfModalGen('QRM34007B34001','DIALOG',lcPOType+'|'+lcPOBrow) = 1)
ENDIF    
IF llAppAll
  REPLACE ALL cStatus WITH 'J'
  =lfApplin()
ELSE
  DO (gcScrDir+"POSRPO.SPX")
ENDIF
SELECT (lcPOLine)
GO TOP
IF !EOF()
  =lfGetLine()
ENDIF
*B602884,1 Read the P/O # for return P/O.
laData[71] = POSHDR.PO
*B602884,1
*USE IN (lcTmpPOLn)
*ERASE (gcWorkDir+lcTmpPOLn+'.DBF')
*ERASE (gcWorkDir+lcTmpPOLn+'.CDX')
SELECT (lcTmpPOLn)
*DELETE ALL FOR cStatus ='A'
*B602884,1 End.
GO TOP
SELECT (lnAlias)
RETURN


*:*************************************************************
*! Name      : lfSelPoLin
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Select po lines to copy when appending from P/o.
*:*************************************************************
FUNCTION lfSelPoLin

lnAlias = SELECT()
*B602884,1 added check of rebulding this temp file.
IF USED(lcTmpPOLn) AND laData[71] = lcPOBrow
  RETURN
ENDIF
*B602884,1 End.

SELECT *,SPACE(2) AS cMarker,SPACE(1) AS cStatus FROM POSLN;
      WHERE cStyType='P' AND PO = lcPOBrow AND Trancd $ '24' ;
      INTO DBF (gcWorkDir+lcTmpPOLn)

*E300935,1 Change Temp. Po Lines Index to have dyelot field.
*INDEX ON cStyType+PO+Style+Trancd+STR(RECNO(),7) TAG (lcTmpPOLn) OF (gcWorkDir+lcTmpPOLn)
INDEX ON cStyType+PO+Style+Dyelot+Trancd+STR(RECNO(),7) TAG (lcTmpPOLn) OF (gcWorkDir+lcTmpPOLn)
SELECT (lnAlias)

*:*************************************************************
*! Name      : lfBrPOLin
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Browse po lines to copy when appending from P/o.
*:*************************************************************
FUNCTION lfBrPOLin

lnAlias = SELECT()
lnMarker = RECNO()
*B602884,1 added two new fields in this browse (lineno,and session).
*lcBrflds =  [cMarker :H=' ':R:1:W=.F.,]+;
             [cStatus :H=' ',]+;
             [Style   :H=lcStyHdr :R,]+;
             IIF(llWareHous,[cWareCode :H='Location':R,],[])+;
             [TOTQTY:7:H='Quantity':P='99999999':R,] +;
             [lnAmt=(TotQty*nCost1):11:H='Amount':P='999999999.999':R,]+;
             [Reference :H='Reference':R]
lcBrflds =   [cMarker :H=' ':R:1:W=.F.,]+;
             [cStatus :H=' ',]+;
             [cRsession :H='Session#' :R,]+;
             [Style     :H=lcStyHdr   :R,]+;
             [LineNo    :H='Line No'  :R,]+;
             IIF(llWareHous,[cWareCode :H='Location':R,],[])+;
             [TOTQTY:7:H='Quantity':P='99999999':R,] +;
             [lnAmt=(TotQty*nCost1):11:H='Amount':P='999999999.999':R,]+;
             [Reference :H='Reference':R]
*B602884,1 End.

SELECT(lcTmpPOLn)
GO TOP
BROWSE FIELDS &lcBrflds ;
       LOCK 0   ;
       NOAPPEND ;
       NOEDIT   ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       WHEN lfwPOl();
       TITLE lcTrn_Ttl    ;
       FOR cStatus <> 'A' ;
       WINDOW POSRPO1 IN WINDOW POSRPO
ACTIVATE WINDOW (lcTrn_Ttl) 
SELECT (lnAlias)

*:*************************************************************
*! Name      : lfwPOl
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Po lines to copy Browse When function.
*:*************************************************************
FUNCTION lfwPOl
PRIVATE lnNewRec 

lnAlias = SELECT()
SELECT (lcTmpPOLn)
lnNewRec = RECNO()
REPLACE ALL cMarker WITH ' '
GO lnNewRec
REPLACE cMarker WITH '>'
SHOW WINDOW (lcTrn_Ttl) REFRESH
SELECT (lnAlias)

*:*************************************************************
*! Name      : lfApplin
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Append marked Po lines to copy.
*:*************************************************************
FUNCTION lfApplin
PRIVATE lnAlias 

*E300935,1 Flag detect if this style/dyelot is found before.
PRIVATE llFound,lcModMsg
lcModMsg = ''
llFound = .F.
*E300935,1

lnAlias = SELECT()
SELECT(lcTmpPOLn)
REPLACE ALL cStyType WITH 'R',Po WITH laData[1],TranCd WITH '1'
GO TOP
SCAN FOR cStatus='J'

  REPLACE cStatus WITH IIF(!EMPTY(cStatus),'A','')
  
  *E300935,1 Add This block instead of SEEK only [begin]
  *E300935,1 Change the following line to seek then locate for the same dyelot
  *E300935,1 Because Now Imported styles can have dyelots.
  llFound = .F.
  *B602884,1 Added new checks for session no.
  IF SEEK(Style+STR(LineNo,6),lcPOLine)
    llFound = .T.
    PRIVATE lcCurSty,lcCurDye,lcCurLin,lcRSess,lcSQlty,lcTrnCd
    lcCurSty = STYLE
    lcCurDye = DYELOT
    lcCurLin = STR(LineNo,6)
    lcRSess  = cRsession
    lcSQlty  = cRetSty
    lcTrnCd  = TranCd
    IF llDyelot AND !EMPTY(DYELOT)
      SELECT (lcPOLine)
      LOCATE REST WHILE Style + STR(LineNo,6) = lcCurSty+lcCurLin ;
                  FOR   DYELOT = lcCurDye AND cRsession = lcRSess ;
                        AND cRetSty=lcSQlty AND TranCd =lcTrnCd
    ELSE
      LOCATE REST WHILE Style + STR(LineNo,6) = lcCurSty+lcCurLin ;
                  FOR   cRsession = lcRSess ;
                        AND cRetSty=lcSQlty AND TranCd =lcTrnCd
    ENDIF
    llFound = FOUND()
    SELECT (lcTmpPOLn)
  ENDIF
  *E300935,1 Add This block instead of SEEK only [End]
  *B602884,1 End.
  
  *E300935,1 Now SEEK must be FOUND()
  *IF SEEK( Style , lcPOLine )
  IF llFound
    *E300935,1 This message because we can dyelots in imported styles.
    lcModMsg = ALLTRIM(Style) + IIF(llDyelot AND !EMPTY(DYELOT),;
               ' Dyelot : ' + ALLTRIM(Dyelot),'' )
    *-Style: xxStyle is already found. Do you wish to increase the quantity?,"\<Yes;\<No"
    *E300935,1 Change message to support dyelots.
    *IF gfModalGen('QRM34013B34001','DIALOG',ALLTRIM(Style)) = 1
    IF gfModalGen('QRM34013B34001','DIALOG',lcModMsg) = 1

      SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty MEMVAR
      SELECT (lcPOLine)
      REPLACE QTY1 WITH QTY1+m.Qty1,; 
              QTY2 WITH QTY2+m.Qty2,; 
              QTY3 WITH QTY3+m.Qty3,; 
              QTY4 WITH QTY4+m.Qty4,; 
              QTY5 WITH QTY5+m.Qty5,; 
              QTY6 WITH QTY6+m.Qty6,; 
              QTY7 WITH QTY7+m.Qty7,;                                           
              QTY8 WITH QTY8+m.Qty8,; 
              TOTQTY WITH TOTQTY+m.TotQty
      laData[36] = laData[36] + m.TotQty
      laData[40] = laData[40] + m.TotQty
      llRecalTot = .T.

    ENDIF
  ELSE
    SCATTER MEMVAR
    m.nECost1= lfvEquCost('1',m.nCost1,laData[28],lnCurrUnt1)
    m.nECost2= lfvEquCost('2',m.nCost2,laData[29],lnCurrUnt2)
    m.nECost3= lfvEquCost('3',m.nCost3,laData[29],lnCurrUnt2)
    m.nECost4= lfvEquCost('4',m.nCost4,laData[29],lnCurrUnt2)
    m.nECost5= lfvEquCost('5',m.nCost5,laData[29],lnCurrUnt2)

    INSERT INTO (lcPOLine) FROM MEMVAR

    laData[36] = laData[36] + &lcPOLine..TotQty
    laData[40] = laData[40] + &lcPOLine..TotQty
    llRecalTot = .T.
  ENDIF
ENDSCAN
SELECT(lnAlias)

*:*************************************************************
*! Name      : lfvSelec
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Marked Po lines to copy.
*:*************************************************************
FUNCTION lfvSelec
PARAMETER llAll
PRIVATE lnNewRec 

lnAlias = SELECT()
SELECT (lcTmpPOLn)
lnNewRec = RECNO()
*B602884,1 Added validation on selecting styles to be in same session and type and quality.
lcCurrKey = Style+STR(LineNo,6)+Dyelot
lcRSess   = cRsession
lcTrnCd   = TranCd
lcRSyQlt  = cRetSty
LOCATE FOR cStatus $ 'JA' AND Style+STR(LineNo,6)+Dyelot=lcCurrKey
IF FOUND()
  IF Style+STR(LineNo,6)+Dyelot+cRsession+TranCd+cRetSty <> lcCurrKey+lcRSess+lcTrnCd+lcRSyQlt

    lcModMsg = IIF(cRsession<>lcRSess,'session',IIF(TranCd<>lcTrnCd,'type','quality'))
    IF llAll
      *--Some styles has different receiving '+lcModMsg+', unable to select all.
      =gfModalGen('TRM34161B34000','DIALOG',lcModMsg)
    ELSE
      *--This style has different receiving '+lcModMsg+', unable to select'  
      =gfModalGen('TRM34162B34000','DIALOG',lcModMsg)
    ENDIF
    GO lnNewRec
    RETURN
  ENDIF
ELSE
  IF llAll
    SCAN FOR cStatus<>'A'
      lcCurrKey = Style+STR(LineNo,6)+Dyelot
      lcRSess   = cRsession
      lcTrnCd   = TranCd
      lcRSyQlt  = cRetSty
      LOCATE FOR Style+STR(LineNo,6)+Dyelot=lcCurrKey AND cRsession+TranCd+cRetSty <> lcRSess+lcTrnCd+lcRSyQlt
      IF FOUND()
        lcModMsg = IIF(cRsession<>lcRSess,'session',IIF(TranCd<>lcTrnCd,'type','quality'))
        *--Some styles has different receiving '+lcModMsg+', unable to select all.
        =gfModalGen('TRM34161B34000','DIALOG',lcModMsg)
      ENDIF
      GO lnNewRec
      RETURN
    ENDSCAN        
  ENDIF
ENDIF
*B602884,1 End.
GO lnNewRec
IF llAll
  REPLACE ALL cStatus WITH 'J' FOR cStatus<>'A'
ELSE
  REPLACE cStatus WITH 'J'
ENDIF
SHOW WINDOW (lcTrn_Ttl) REFRESH
SHOW GET pbUSele  ENABLE 
SHOW GET pbUSeleA ENABLE 
SHOW GET pbSele  DISABLE 
SHOW GET pbSeleA DISABLE 
SCAN  FOR cStatus<>'A'
  IF EMPTY(cStatus)
    SHOW GET pbSele ENABLE   
    SHOW GET pbSeleA ENABLE       
    EXIT
  ENDIF
ENDSCAN
GO lnNewRec
SELECT (lnAlias)

*:*************************************************************
*! Name      : lfvUSelec
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : UnMark Po lines to copy.
*:*************************************************************
FUNCTION lfvUSelec
PARAMETER llAll
PRIVATE lnNewRec 

lnAlias = SELECT()
SELECT (lcTmpPOLn)
lnNewRec = RECNO()
IF llAll
  REPLACE ALL cStatus WITH SPACE(1)  FOR cStatus<>'A'
ELSE
  REPLACE cStatus WITH SPACE(1)
ENDIF
SHOW WINDOW (lcTrn_Ttl) REFRESH
SHOW GET pbSele  ENABLE 
SHOW GET pbSeleA ENABLE 
SHOW GET pbUSele  DISABLE 
SHOW GET pbUSeleA DISABLE 
SCAN
  IF !EMPTY(cStatus)
    SHOW GET pbUSele  ENABLE 
    SHOW GET pbUSeleA ENABLE 
    EXIT
  ENDIF
ENDSCAN
GO lnNewRec
SELECT (lnAlias)
RETURN


*:*************************************************************
*! Name      : lfvPPO
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Valid P/O # Production # case of inter -loc p/o.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvPPO()
*!*************************************************************
FUNCTION lfvPPO
lnAlias = SELECT()
SELECT POSHDR
lnPOHRec=IIF(!EOF(),RECNO(),0)
lcCurFltr = SET('FILTER')
SET FILTER TO cStyType='P'
IF llBrowse OR ( !EMPTY(laData[71]) AND !SEEK('P'+laData[71],'POSHDR') )
  lcPPo=laData[71]
  DO POSBrow WITH lcPPo,'','P'
  laData[71]=lcPPo
  llBrowse = .F.
ENDIF 
SET FILTER TO &lcCurFltr
IF lnPOHRec<>0
  GOTO lnPOHRec
ENDIF
SELECT (lnAlias)
RETURN


*:*************************************************************
*! Name      : lfChkProd
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Check production against distribution.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfChkProd()
*!*************************************************************
FUNCTION lfChkProd

lcTmpDistr = gfTempName()
lnPOLRec=IIF(!EOF('POSLN') ,RECNO('POSLN') ,0)
llDifeDist=.F.

WAIT WINDOW 'Start checking the production # '+laData[71] NOWAIT
CREATE CURSOR (lcTmpDistr) (Style C(19),Qty1 N(6),Qty2 N(6),Qty3 N(6),Qty4 N(6),;
              Qty5 N(6),Qty6 N(6),Qty7 N(6),Qty8 N(6),dQty1 N(6),dQty2 N(6),dQty3 N(6),dQty4 N(6),;
              dQty5 N(6),dQty6 N(6),dQty7 N(6),dQty8 N(6),lDOnly L(1) )
INDEX ON Style TAG (lcTmpDistr) OF (gcWorkDir+lcTmpDistr)
*--Calculate the production.
SELECT POSLN
=SEEK('P'+laData[71])
SCAN WHILE cStyType+PO = 'P'+laData[71] FOR TranCd = '2'
  SELECT (lcTmpDistr)
  IF !SEEK(POSLN.Style)
    APPEND BLANK
    REPLACE Style WITH POSLN.Style
  ENDIF
  REPLACE Qty1  WITH Qty1 + POSLN.Qty1,;
          Qty2  WITH Qty2 + POSLN.Qty2,;
          Qty3  WITH Qty3 + POSLN.Qty3,;
          Qty4  WITH Qty4 + POSLN.Qty4,;
          Qty5  WITH Qty5 + POSLN.Qty5,;                                    
          Qty6  WITH Qty6 + POSLN.Qty6,;
          Qty7  WITH Qty7 + POSLN.Qty7,;
          Qty8  WITH Qty8 + POSLN.Qty8
ENDSCAN
*--Calculate the Distribution.
SELECT (lcPOline)
SCAN 
  lcArySty = Style
  SELECT (lcTmpDistr)
  IF SEEK(lcArySty)
    REPLACE dQty1  WITH dQty1 + &lcPOline..Qty1,;
            dQty2  WITH dQty2 + &lcPOline..Qty2,;
            dQty3  WITH dQty3 + &lcPOline..Qty3,;
            dQty4  WITH dQty4 + &lcPOline..Qty4,;
            dQty5  WITH dQty5 + &lcPOline..Qty5,;                                    
            dQty6  WITH dQty6 + &lcPOline..Qty6,;
            dQty7  WITH dQty7 + &lcPOline..Qty7,;
            dQty8  WITH dQty8 + &lcPOline..Qty8
  ELSE
    APPEND BLANK
    REPLACE Style  WITH lcArySty,;
            lDOnly WITH .T.
  ENDIF
ENDSCAN

*--Start comparing.
SELECT (lcTmpDistr)
SCAN
  FOR I=1 TO 8
    Z=STR(I,1)
    IF Qty&Z <> dQty&Z
      llDifeDist = .T.
      *--The total Distribution (XXX) does not equal the Production (XXX)
      *--for the Style XXX in XXX size.
      lcMgTtl = Z+IIF(I=1,'ST',IIF(I=2,'ND',IIF(I=3,'RD','TH')))
      =gfModalGen('INM34114B34000','DIALOG',ALLT(STR(dQty&Z,6))+'|'+ALLT(STR(Qty&Z,6))+'|'+Style+'|'+lcMgTtl)
    ENDIF
  ENDFOR
ENDSCAN

LOCATE FOR lDOnly 
IF FOUND()
  *--Style XXXXX does no exist in production!
  =gfModalGen('INM34115B34000','DIALOG',Style)
  llDifeDist = .T.
ENDIF

SELECT (lcTmpDistr)
USE

SELECT POSLN
IF lnPOLRec<>0
  GOTO lnPOLRec
ENDIF
SELECT (lcPOline)
GO TOP
WAIT CLEAR
RETURN llDifeDist


*:*************************************************************
*! Name      : lfCpyPst
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Copy or Paste Line Quantity.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : 'C' for Copy.
*!                      'P' for Paste. 
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfCpyPst()
*!*************************************************************
FUNCTION lfCpyPst
PARA lcCpyPst

SELECT (lcPOline)
IF lcCpyPst='P'
  FOR I=1 TO 8
    Z=STR(I,1)
    IF I<=SCALE.Cnt
      m.Qty&Z = laCopySav[I]
      REPLACE Qty&Z WITH m.Qty&Z
    ELSE
      m.Qty&Z = 0
    ENDIF  
    SHOW GET m.Qty&Z
  ENDFOR
  m.TotQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
  lnOldTQt = TotQty
  REPLACE TotQty WITH m.TotQty
  SHOW GET m.TotQty
  laData[36] = laData[36] + m.TotQty - lnOldTQt
  laData[40] = laData[40] + m.TotQty - lnOldTQt
  SELECT (lcPOHdr)
  REPLACE nStyOrder WITH laData[36],;
          Open      WITH laData[40]
ELSE
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laCopySav
ENDIF
SELECT (lcPOline)
RETURN

*:*************************************************************
*! Name      : lfvDyelot
*! Developer : Mohammed Atia Badran (MAB)
*! Date      : 07/27/1998
*! Purpose   : The Dyelot valid function.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!                      NONE
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : = lfvDyelot()
*!*************************************************************
*E300935,1 MAB 07/27/1998 Add Dyelot field to screen detail folder .
*
FUNCTION lfvDyelot

*E301274,1 AMM Add a new parameter to indicate whether called from details or extended size scale screen
PARAMETERS llFrmGBrw
IF llFrmGBrw
  m.Dyelot = EVAL(VARREAD())
ENDIF
*E301274,1 AMM end
IF EMPTY(m.Dyelot)
  m.Dyelot = lcOldValue
  SHOW GET m.Dyelot
ELSE  && not empty of m.dyelot

  PRIVATE lnAlias
  lnAlias = SELECT(0)

  *E300935,1 Validation for Dyelot Field if inter-location P/O [Begin]
  *E301291,1 AMM Consider the case of adornment order
  *IF (lcAType='N')
  IF (lcAType$'NA')
  *E301291,1 AMM end
    *B602139,1 Do validation in while loop to valid source if user change dyelot for target. [begin]
    PRIVATE llValdWare,llRetFrom,lcDyelot
    STORE .F. TO llValdWare,llRetFrom

    DO WHILE !llValdWare
    
      *-- if Check for source location is valid, then do it with target. 
      IF !SEEK(PADR(m.Style,19)+PADR(laData[2],6)+PADR(m.Dyelot,10),'STYDYE')
        
        =lfDyeQryRt(PADR(laData[2],6))
        llRetFrom  = EMPTY(m.Dyelot)

      ENDIF  

      lcDyelot = m.Dyelot

      *-- Check target location. 
      IF !llRetFrom
        lcSeekedWr = IIF(llMultiWare,PADR(m.CwareCode,6),PADR(laData[19],6))
        IF !SEEK(PADR(m.Style,19)+lcSeekedWr+PADR(m.Dyelot,10),'STYDYE')
        
          = lfDyeQryRt(lcSeekedWr)
          llRetFrom  =  EMPTY(m.Dyelot)
        
        ENDIF
      
      ENDIF
      
      IF llRetFrom
        EXIT
      ELSE
        llValdWare = (lcDyelot = m.Dyelot)  
      ENDIF

    ENDDO  
    
    IF llRetFrom
      RETURN
    ENDIF  
    *B602139,1 Do validation in while loop to valid source if user change dyelot for target. [begin]

  ENDIF
  *E300935,1 Validation for Dyelot Field if inter-location P/O [End]
  *E301274,1 AMM Select the right file
  *SELECT (lcPoLine)
  SELECT IIF(!llFrmGBrw,lcPoLine,lcTmpSize)
  *E301274,1 AMM end
  llNoThing = RLOCK()
  REPLACE Dyelot WITH m.Dyelot
  *E301274,1 AMM update table
  *UNLOCK
  *= lfwBrow1()
  IF llFrmGBrw
    REPLACE cStatus   WITH IIF(cStatus='N','N','M')
  ELSE
    UNLOCK
    =lfwBrow1()
  ENDIF
  *E301274,1 AMM end
  SELECT(lnAlias)

ENDIF  
*-- end of lfvDyelot.

*:*************************************************************
*! Name      : lfAdjDye
*! Developer : Mohammed Atia Badran (MAB)
*! Date      : 08/12/1998
*! Purpose   : Enable and disable dyelot field according to style case.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!                      NONE
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : = lfAdjDye()
*!*************************************************************
*E300935,1
*
FUNCTION lfAdjDye
*
IF llDyelot
  llDyltYes = (STYLE.cDye_Flg = 'Y')  && Style dyelot yes.
  *E300935,1 Enable/Disable Dyelot object [Begin]
  *B602552,1 MAB 03/01/1999 Dyelot fields is disabled in view mode.
  *lcDyeStat = IIF(llDyltYes ,'ENABLE','DISABLE')
  lcDyeStat = IIF(llDyltYes AND (laScrMode[3] OR laScrMode[4]),'ENABLE','DISABLE')
  SHOW GET m.Dyelot &lcDyeStat
  *E300935,1 Enable/Disable Dyelot object [End  ]
ENDIF  
*-- end of lfAdjDye.

*:*************************************************************
*! Name      : lfDyeQryRt
*! Developer : Mohammed Atia Badran (MAB)
*! Date      : 08/12/1998
*! Purpose   : Valid dyelot/WareHouse in case of Inter-Location P/O.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : Location Code.
*!                      NONE
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfDyeQryRt()
*!*************************************************************
*E300935,1
*
FUNCTION lfDyeQryRt
PARAMETERS lcEvalWare

PRIVATE lcModalMes,lnSelect

lcModalMes = ALLTRIM(m.Style) + ' Dyelot: ' + ALLTRIM(m.Dyelot)
*-Style: xxx Dyelot: xxx is not assigned to location: xxx. "\<Add;\<Browse;\<Reenter"
lnSelect = gfModalGen('QRM34048B42003','DIALOG',lcModalMes+'|'+ALLTRIM(lcEvalWare))

DO CASE

  CASE lnSelect = 1  && Add Button.
    DO gpAdStyWar WITH m.Style,PADR(m.Dyelot,10),lcEvalWare
    
  CASE lnSelect = 2  && Browse Button.
    *B602163 Reenter dyelot if style does not have dyelots in current location.
    *=SDyeBrow(m.Style , @m.Dyelot , .F. , lcEvalWare)
    IF !SDyeBrow(m.Style , @m.Dyelot , .F. , lcEvalWare)
      m.Dyelot = SPACE(10)
    ENDIF
    
  CASE lnSelect = 3  && Reenter Button.
    m.Dyelot = SPACE(10)
    
ENDCASE

SHOW GET m.Dyelot 
IF EMPTY(m.Dyelot)
  _CUROBJ = _CUROBJ
ENDIF
*-- end of lfDyeQryRt.

*!*************************************************************
*! Name      : lfOpn_Rest
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/11/1999
*! Purpose   : Open file then restore current alias
*!*************************************************************
*! Calls       : gfOpenFile
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfOpn_Rest()
*!*************************************************************
*E300935,4
FUNCTION lfOpn_Rest
PARAMETERS lcFileN,lcIndN
PRIVATE lnCurrAls,llOpenIt 
lnCurrAls = SELECT(0)
llOpenIt = gfOpenFile(lcFileN,lcIndN,'SH')
SELECT (lnCurrAls)
RETURN llOpenIt
*-- end of lfOpn_Rest.

*!*************************************************************
*! Name      : FUNCTION lfvGPrice
*! Developer : Mohamed Atia Badran
*! Date      : 03/14/1999
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
*E301169,1
FUNCTION lfvGPrice
*E301274,1 AMM Add a new paramter to indicate whether called from details or extended size scale screen
PARAMETERS llFrmGBrw
IF llFrmGBrw
  *--- SSH  
  *m.Gros_Price = VAL( EVAL(VARREAD()) )
  m.Gros_Price = EVAL(VARREAD())
  *--- SSH
ENDIF
*E301274,1 AMM end
*B607345,1 ALB Add check if user change the style purching price [BEGIN]
llUpdPoPr   = IIF(gfGetMemVar('M_UPDPOPR') = 'Y',.T.,.F.)
IF llUpdPoPr AND STYLE.cPriceCur = laData[26] AND (STYLE.NICOST1 <> 0) AND (m.Gros_Price <> STYLE.NICOST1)
  IF gfModalGen('QRM34183B34001','DIALOG',STR(STYLE.NICOST1,2)) = 2
    m.Gros_Price = STYLE.NICOST1
  ENDIF
ENDIF
*B607345,1 ALB Add check if user change the style purching price [END]

*C200080,1 AMM set the discount to zero
IF lcAType='D'
  m.Disc_Pcnt = 0
ENDIF
*C200080,1 AMM end
*B602861,1 

*E301274,1 AMM Adjust condition
*IF m.Gros_Price <> &lcPOLine..Gros_Price
*--- SSH
*IF (!llFrmGBrw .AND. m.Gros_Price <> EVAL(lcPOLine+'.Gros_Price')) ;
    .OR. (llFrmGBrw .AND. m.Gros_Price # VAL(lcOldVal) )

IF (!llFrmGBrw .AND. m.Gros_Price <> EVAL(lcPOLine+'.Gros_Price')) ;
    .OR. (llFrmGBrw .AND. m.Gros_Price # lcOldVal)

*--- SSH
*E301274,1 AMM end
  m.nCost1 = ROUND(m.Gros_Price*(100-m.Disc_Pcnt)/100,3)
  IF llFrmGBrw
    REPLACE nCost1 WITH m.nCost1 
  ENDIF

  *E301274,1 AMM Call with the new added parameter
  *=lfvCost('1')
  =lfvCost('1',.F.,llFrmGBrw)
  *E301274,1 AMM end
ENDIF

&&>>> 

*!*************************************************************
*! Name      : FUNCTION lfvPrcDisc
*! Developer : Mohamed Atia Badran
*! Date      : 03/14/1999
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
*E301169,1
FUNCTION lfvPrcDisc
*E301274,1 AMM Add a new parameter to indicate whether called from the details or extended size scale screen
PARAMETERS llFrmGBrw
IF llFrmGBrw
  *--- ssh  
  *m.Disc_Pcnt = VAL( EVAL(VARREAD()) )
  m.Disc_Pcnt = EVAL(VARREAD())
  *--- ssh
ENDIF
*E301274,1 AMM end

*B602861,1 
*E301274,1 AMM Adjust condition
*IF m.Disc_Pcnt <> &lcPOLine..Disc_Pcnt
*--- SSH
*IF IIF(llFrmGBrw, m.Disc_Pcnt # VAL(lcOldVal), m.Disc_Pcnt <> EVAL(lcPOLine+'.Disc_Pcnt'))
IF IIF(llFrmGBrw, m.Disc_Pcnt # lcOldVal, m.Disc_Pcnt <> EVAL(lcPOLine+'.Disc_Pcnt'))
*--- SSH
*E301274,1 AMM end
  m.nCost1 = ROUND(m.Gros_Price*(100-m.Disc_Pcnt)/100,3)
  IF llFrmGBrw
    REPLACE nCost1 WITH m.nCost1
  ENDIF
  *E301274,1 AMM Call with the new added parameter
  *=lfvCost('1',.T.)
  =lfvCost('1',.T.,llFrmGBrw)
  *E301274,1 AMM end
ENDIF
*--

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


*:*************************************************************************
*! Name      : lfvSwitch
*! Developer : Sameh (SSE)
*! Date      : 05/31/1999
*! Purpose   : push button to switch between (Cost by P/O & Cost by Unit)
*:*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*:*************************************************************************
*! Passed Parameters  : None
*:*************************************************************************
*! Returns            : None
*:*************************************************************************
*! Example   : = lfvSwitch()
*:*************************************************************************
*E301243,1
FUNCTION lfvSwitch
PRIVATE lnOldAlias

lnOldAlias = SELECT()    && to save the old alias number

SELECT POSHDR

IF llByUnit    && if llByUnit is True (Cost is shown by P/O)
  SHOW GET pbSwitch,1 PROMPT 'Cost by Unit' 
  llByUnit = .F.    && since this is a Flag it must be Toggled (Turned On/Off)

ELSE           && else llByUnit is False (Cost is shown by Unit)
  SHOW GET pbSwitch,1 PROMPT 'Cost by P/O' 
  llByUnit = .T.
ENDIF

= lfRefresh()      && refresh all the labels to show whether (Cost by Unit/cost by C/T) values
SHOW GET PbSwitch  && show whether (Cost by Unit/cost by C/T) caption on pbSwitch push button  

SELECT (lnOldAlias)  && restore old alias number before the end of function
*-- End of lfvSwitch


*!*************************************************************
*! Name      : lfvalSty
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 07/06/1999
*! Purpose   : Validate styles
*! Ref       : *E301274,1 AMM
*!*************************************************************
*! Called From : Extended size scale screen
*! Calls       : gfModalGen()
*!*************************************************************
*! Passed Parameters : File Name, Communication status.
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfvalSty()
*!*************************************************************
FUNCTION lfvalSty

*C200080,1 AMM Accept manufacturing styles in case of Dye Order
*IF !&lcTmpSty..Make
  *IF lcAType='P'
*E301291,1 AMM Consider the case of adornment order
*IF IIF(lcAType='D',&lcTmpSty..Make,!&lcTmpSty..Make)
IF IIF(lcAType$'DA',&lcTmpSty..Make,!&lcTmpSty..Make)
*E301291,1 AMM end
  IF lcAType$'PD'
*C200080,1 AMM end
    IF !llOpnBom
      llOpnBom = lfOpn_Rest(gcDataDir+'BOM','BOM')
    ENDIF  
    IF !(laData[3]$'HB')
      IF SEEK(SUBSTR(&lcTmpSty..Style,1,lnMjrWid),'BOM')
        SELECT BOM 
        LOCATE REST WHILE cItmMajor=SUBSTR(&lcTmpSty..Style,1,lnMjrWid) ;
               FOR LIKE(STRTRAN(cItmMask,'*','?'),PADR(&lcTmpSty..Style,19))
        IF !FOUND()
          *-No cost lines found in the cost sheet, Cannot proceed!
          =gfModalGen('TRM34037B34000','DIALOG')        
          RETURN .F.
        ENDIF
      ELSE
        *-No cost lines found in the cost sheet, Cannot proceed!
        =gfModalGen('TRM34037B34000','DIALOG')        
        RETURN .F.
      ENDIF
    ELSE 
      IF !SEEK(SUBSTR(&lcTmpSty..Style,1,lnMjrWid),'BOM')
        *-Warrning ! This style has no cost sheet.
        =gfModalGen('INM34038B34000','DIALOG')        
      ENDIF 
    ENDIF
  ENDIF
   
ELSE 
  *-Conflict ! only purchased styles are allowed to enter here, Cannot proceed!
  =gfModalGen('TRM34039B34000','DIALOG')        
  RETURN .F.
ENDIF 

IF &lcTmpSty..cDivision <> ALLTRIM(laData[4])
  *-Conflict ! styles restricted to division XXXX, Cannot proceed!
  =gfModalGen('TRM34041B34000','DIALOG',ALLTRIM(laDiv[lnDiv,1]) )        
  RETURN .F.
ENDIF

IF !EMPTY(&lcTmpSty..cPurCode) AND &lcTmpSty..cPurCode <> ALLTRIM(laData[67])
  *-Conflict ! styles restricted to purchase XXXX, Cannot proceed!
  =gfModalGen('TRM34110B34000','DIALOG',ALLTRIM(laPurCode[lnPurCode,1]) )
  RETURN .F.
ENDIF

IF lcAType='C' AND lfVldSCnr()
  *-The Style: xxx valid in another contract for the same vendor and period, Cannot proceed!
  =gfModalGen('TRM34018B34000','DIALOG',&lcTmpSty..Style)
  RETURN .F.
ENDIF
*E301291,1 AMM Consider the case of adornment order
*IF lcAType='N' AND !SEEK(PADR(m.Style,19)+PADR(laData[2],6)+SPACE(10),'STYDYE')
IF lcAType$'NA' AND !SEEK(PADR(m.Style,19)+PADR(laData[2],6)+SPACE(10),'STYDYE')
*E301291,1 AMM end
  *-- If system setup to point of sale don't allow to add location.
  IF llPosSetN
    *-Style: xxx is not assigned to location: xxx. "ok"
    =gfModalGen('TRM34048B34000','DIALOG',ALLTRIM(&lcTmpSty..Style)+'|'+ALLTRIM(laData[2]))
    RETURN .F.
  ELSE    
    *-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
    IF gfModalGen('QRM34048B34004','DIALOG',ALLTRIM(m.Style)+'|'+ALLTRIM(laData[2])) = 1
      DO gpAdStyWar WITH &lcTmpSty..Style,SPACE(10),laData[2]
    ELSE
      RETURN .F.
    ENDIF
  ENDIF
ENDIF

IF !llMultiWare
  *--If ship to customer the location will be the ship drop location.
  m.cWareCode = IIF(lnShpLoc = 2,laData[69],laData[19])
  lnLShpLoc   = lnShpLoc
  m.Account   = IIF(lnShpLoc = 2,laData[69],'')
  m.Store     = laData[70]

  IF lnShpLoc = 1
    IF !SEEK(PADR(&lcTmpSty..Style,19)+PADR(m.cWareCode,6)+SPACE(10),'STYDYE')
      *-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
      IF gfModalGen('QRM34048B34004','DIALOG',ALLTRIM(&lcTmpSty..Style)+'|'+m.cWareCode) = 1
        DO gpAdStyWar WITH &lcTmpSty..Style,SPACE(10),m.cWareCode
      ELSE
        RETURN .F.
      ENDIF
    ENDIF
    
  ELSE  && lnShpLoc = 2
    IF !SEEK(PADR(m.Style,19)+lcDropLoc+SPACE(10),'STYDYE')
      *-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
      IF gfModalGen('QRM34048B34004','DIALOG',ALLTRIM(&lcTmpSty..Style)+'|'+lcDropLoc) = 1
        DO gpAdStyWar WITH &lcTmpSty..Style,SPACE(10),lcDropLoc
      ELSE
        RETURN .F.
      ENDIF
    ENDIF
  ENDIF
ELSE
  IF llWareHous
    *E301291,1 AMM Consider the case of adornment order
    *m.cWareCode = IIF(lcAType='N' AND PADR(laData[2],6)=&lcTmpSty..CDefWare,SPACE(6),&lcTmpSty..CDefWare)
    m.cWareCode = IIF(lcAType$'NA' AND PADR(laData[2],6)=&lcTmpSty..CDefWare,SPACE(6),&lcTmpSty..CDefWare)
    *E301291,1 AMM end
  ELSE  
    m.cWarCust = 'C'
    m.cWareCode = SPACE(5)
  ENDIF
ENDIF

m.cStyType = lcAType
m.Trancd = '1'
lnLines  = lnLines + 1
m.LineNo = lnLines
m.Po     = laData[1]
m.Vendor = laData[2]
m.Scale  = &lcTmpSty..Scale

*-- Make it hold buy prepack as default ==>m.PrePak = &lcTmpSty..PrePak
m.PrePak = &lcTmpSty..CbuyPrePk
m.cVenSty= &lcTmpSty..cVenSty

IF lcAtype='C' .OR. !lfFrmCont()
  m.nCost1 = IIF(!llMulCurr OR &lcTmpSty..cPriceCur=laData[26],&lcTmpSty..nICost1,0)
  m.nCost2 = IIF(!llMulCurr OR &lcTmpSty..cDutyCur =laData[27],&lcTmpSty..nICost2,0)
  m.nCost3 = IIF(!llMulCurr OR &lcTmpSty..cDutyCur =laData[27],&lcTmpSty..nICost3,0)
  m.nCost4 = IIF(!llMulCurr OR &lcTmpSty..cDutyCur =laData[27],&lcTmpSty..nICost4,0)
  m.nCost5 = IIF(!llMulCurr OR &lcTmpSty..cDutyCur =laData[27],&lcTmpSty..nICost5,0)

  IF !llMulCurr OR &lcTmpSty..cPriceCur=laData[26]
    m.Disc_Pcnt  = &lcTmpSty..Disc_Pcnt
    m.Gros_Price = IIF(&lcTmpSty..Gros_Price=0 AND &lcTmpSty..Disc_Pcnt=0,m.nCost1,&lcTmpSty..Gros_Price)
  ELSE
    m.Disc_Pcnt  = 0
    m.Gros_Price = m.nCost1
  ENDIF
  llConCst=.F.  
ELSE
  llConCst=.T.  
ENDIF

*--Read equivalent costs.
m.nECost1= lfvEquCost('1',m.nCost1,laData[28],lnCurrUnt1)
m.nECost2= lfvEquCost('2',m.nCost2,laData[29],lnCurrUnt2)
m.nECost3= lfvEquCost('3',m.nCost3,laData[29],lnCurrUnt2)
m.nECost4= lfvEquCost('4',m.nCost4,laData[29],lnCurrUnt2)
m.nECost5= lfvEquCost('5',m.nCost5,laData[29],lnCurrUnt2)

m.lFlag = llConCst
RETURN


*!*************************************************************
*! Name      : lfexSzQty
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 07/06/1999
*! Purpose   : Validate Style size quantity
*! REF       : *E301274,1 AMM
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfexSzQty()
*!*************************************************************
FUNCTION lfexSzQty
PARAMETERS lnSize
lcSz = STR(lnSize,1)

IF m.Qty&lcSz = laOldQty[lnSize]
  IF lnSize = Scale.Cnt 
    IF m.TotQty <> laOldQty[9] .AND. ;
      gfModalGen('QRM34045B34001','DIALOG',ALLTRIM(STR(m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,10))) = 2
      *-Quantities out of balance! 9999 Pieces ok?','\<Yes;\?\<No'
    ELSE
      m.TotQty = laOldQty[9]
      SELECT (lcTmpSize)
      REPLACE TotQty   WITH m.TotQty,;
              cTotQty  WITH STR(m.TotQty,7)
    ENDIF
  ENDIF
  RETURN
ENDIF

IF laScrMode[3]
  lnAlias = SELECT()
  lnRecQt = 0
  IF laData[37]+laData[38]<>0 AND ;
    SEEK(lcAType+laData[1]+Style+STR(Lineno,6),'POSLN')
    SELECT POSLN
    lnSvRec = RECNO()
    SCAN WHILE cStyType+Po+Style+STR(Lineno,6)=lcAType+laData[1]+Style+STR(Lineno,6) FOR Trancd $ '245'
      lnRecQt = lnRecQt + Qty&lcSz
    ENDSCAN
    GO lnSvRec
  ENDIF  
  SELECT (lnAlias)
  *-Quantity cannot be less than the received quantity.
  IF m.Qty&lcSz < lnRecQt
    =gfModalGen('TRM34043B34000','DIALOG')        
    m.Qty&lcSz = laOldQty[lnSize]
    SELECT (lcTmpSize)
    REPLACE Qty&lcSz   WITH m.Qty&lcSz,;
            cQty&lcSz  WITH STR(m.Qty&lcSz,6)

    RETURN .F.
  ENDIF

  IF llSOInstld AND lcAType='P' AND laData[3] $ 'OH'
    llFound = SEEK('P'+laData[1]+Style+STR(LineNo,6),'POSLN')
    llFound = llFound AND ( m.Qty&lcSz  < POSLN.Ord&lcSz )
    llFound = llFound AND SEEK('2'+laData[1]+Style,'CutPick')
    IF llFound AND !lfChkOrdQt(lcSz)
      m.Qty&lcSz = laOldQty[lnSize]
      SELECT (lcTmpSize)
      REPLACE Qty&lcSz   WITH m.Qty&lcSz,;
              cQty&lcSz  WITH STR(m.Qty&lcSz,6)

      RETURN
    ENDIF
  ENDIF
ENDIF

laData[36] = laData[36] - laOldQty[lnSize] + m.Qty&lcSz
laData[40] = laData[40] - laOldQty[lnSize] + m.Qty&lcSz
llRecalTot = .T.

SELECT (lcPOHdr)
REPLACE nStyOrder WITH laData[36],;
        Open      WITH laData[40]
SELECT (lcTmpSize)
REPLACE Qty&lcSz   WITH m.Qty&lcSz,;
        cQty&lcSz  WITH STR(m.Qty&lcSz,6),;
        TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
        cTotQty    WITH STR(TotQty,7),;
        cStatus    WITH IIF(cStatus='N','N','M')        
RETURN


*!*************************************************************
*! Name      : lfvPOPUP
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 07/06/1999
*! Purpose   : Validate ship to field in the extended size scale screen
*! REF       : *E301274,1 AMM
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfvSelShp()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvPOPUP()
*!*************************************************************
FUNCTION lfvPOPUP

IF !INLIST(EVAL(VARREAD()),'L','C')
  =gfModalGen('INM34086B00000','DIALOG','"L" (Location)/"C" (Customer)')  
  RETURN .F.
ELSE
  lnLShpLoc   = IIF(cWarCust='C',2,1)
  =lfvSelShp(.T.)
ENDIF

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

*C102357,1 AMH If Style is empty Call Custom screen for Customer JL on trigger. [Start]
IF ASCAN(laEvntTrig , PADR('DOSCRN',10)) <> 0  
  IF EMPTY(m.Style)
    DO (gcAppHome+'SOJLINF.FXP') WITH "AD" , laData[1] , m.LineNo , m.Style , laData[2] , m.Store , lcT_BomVar , lnActFolder, .T.
  ENDIF  
ENDIF         
*C102357,1 AMH [End]

IF !EMPTY(m.Style)
  *-- Save current alias.
  lcCrOrdAlias = SELECT(0)
  
  *-- Clear the trapped keys.
  PUSH KEY
  ON KEY
  *-- Variable hold the order price.

  *C102357,1 AMH m.nCost2 equale zero in case of Customer JL [Start]
  IF ASCAN(laEvntTrig , PADR('DOSCRN',10)) <> 0
    m.nCost2 = 0
  ENDIF
  *C102357,1 AMH [End]
  lnRetPrice = m.nCost1 +m.nCost2
  *-- Call a dialog screen to display the styles positions & the variant cost sheet.

  *C102357,1 AMH Call Custom screen for Customer JL on trigger [Start]
  *DO (gcAppHome+'SODS_ND.FXP') WITH "AD" , laData[1] , lcT_BomVar , m.Style , m.LineNo , m.nCost1 +m.nCost2 , laData[2] , m.Store , lnRetPrice
  IF ASCAN(laEvntTrig , PADR('DOSCRN',10)) <> 0  
    DO (gcAppHome+'SOJLINF.FXP') WITH "AD" , laData[1] , m.LineNo , m.Style , laData[2] , m.Store , lcT_BomVar , lnActFolder, .T.
  ELSE
    
    *B605082,1 AMH Fix the bug cost of name drop does not display in case of Adornment order [Start]
    *DO (gcAppHome+'SODS_ND.FXP') WITH "AD" , laData[1] , lcT_BomVar , m.Style , m.LineNo , m.nCost1 +m.nCost2 , laData[2] , m.Store , lnRetPrice
    DO (gcAppHome+'SODS_ND.FXP') WITH "AD" , laData[1] , lcT_BomVar , m.Style , m.LineNo , m.nCost1 , laData[2] , m.Store , lnRetPrice
    *B605082,1 AMH [End]
    
  ENDIF
  *C102357,1 AMH [End]

  *-- Restore the trapped keys.
  POP KEY
  *-- Restore current alias.
  SELECT (lcCrOrdAlias)
  *E301288,1 Change the prompt of the configure button.
  lcConfig = IIF(SEEK("AD" + &lcPOLine..PO + STR(&lcPOLine..LineNo,6) , lcT_BomVar) , "\<Adornment" , "\<Configure")
  SHOW GET pbConfig,1 PROMPT lcConfig ENABLE

  IF !laScrMode[2] .AND. lnRetPrice # m.nCost1 +m.nCost2
    m.nCost2 = lnRetPrice - m.nCost1
    =lfvCost('2')
    SHOW GET m.nCost2
  ENDIF
ENDIF

*E500272,1 WAB - Function to   call po cost sheet
*E500272,1 WAB - START
*!**************************************************************************
*! Name      : lfvCostSh
*! Developer : WAB - Walid A. Wahab
*! Date      : 08/09/1999
*! Purpose   : Call po Cost sheet Screen.
*!**************************************************************************
*! Calls     : gpDoProg
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfvCostSh()
*!**************************************************************************
*-
FUNCTION lfvCostSh
PRIVATE llOpnBom
*-- Call the Po cost sheet program.

*B605105,1 AMH To call lpshow when retrun from PO cost sheet screen [Start]
llNoShow    = .F.
llCallShow  = .T.
llShow      = .F.
*B605105,1 AMH [End]

*B604950,1 KHM 09/30/2001 (Begin) Changing the passed parameters.
*lcParameter = "'" + laData[1] + "'"+IIF(lcCrtCSH="T",",.T.",',.F.')
lcParameter = "'" + laData[1] + "'"+",.F."
*B604950,1 KHM 09/30/2001 (End)
IF lcAType <> "D"		
  DO gpDoProg WITH "AWRPOCSSH", .F., "PO", lcParameter
ELSE
  DO gpDoProg WITH "AWRMFDCSH", .F., "MF", lcParameter  
ENDIF  

*E500272,1 WAB - END



*E301289,1 WAB - START
*!**************************************************************************
*! Name      : lfGetPrice
*! Developer : WAB - Walid A. Wahab
*! Date      : 08/18/1999
*! Purpose   : call Po selling price screen when press Gp bott.
*!**************************************************************************
*! Calls     : Poselprc.spr
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfGetPrice()
*!**************************************************************************
*-
FUNCTION lfGetPrice
lnOldPrice = m.nSelPrice		&& get selling preice old value
lnOldMrgn  = m.nGrosMrgn		&& get gross margin old value
lnPriceLvl = 1
lcDispFld = IIF(Empty(m.nSelPrice),'DISABLE','ENABLED')
DO (gcScrDir+"Poselprc.SPX")

*E301289,1 WAB - END

*E301289,1 WAB - START
*!**************************************************************************
*! Name      : lfvSelPrice
*! Developer : WAB - Walid A. Wahab
*! Date      : 08/18/1999
*! Purpose   : validate  after edit the selling price filed
*!           : to calculate the gross margin 
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfvSelPrice()
*!**************************************************************************
*-
FUNCTION lfvSelPrice
PARAMETER llUpdField
PRIVATE lnRotSub
*--devide the diferent between price and cost  [by cost or by selling price]
*-- depends on llstymark (setup) 
lnTotCost   = &lcPOLine..nEcost1 + &lcPOLine..nEcost2 + &lcPOLine..nEcost3 +;
              &lcPOLine..nEcost4 + &lcPOLine..nEcost5
lnRotSub    = IIF(llStyMark,lnTotCost,m.nSelPrice)
m.nGrosMrgn = IIF(m.nSelPrice=0,0,;
              IIF(lnRotSub=0,0,((m.nSelPrice - lnTotCost)/lnRotSub)*100))
lcDispFld = IIF(Empty(m.nSelPrice),'DISABLE','ENABLED')
IF llUpdField
  REPLACE &lcPOLine..nSelPrice WITH m.nSelPrice ,;
          &lcPOLine..nGrosMrgn WITh m.nGrosMrgn 
ELSE
  SHOW GET pbAmdOk &lcDispFld
  SHOW GET m.nGrosMrgn DISABLE
ENDIF
*E301289,1 WAB - END

*E301289,1 WAB - START
*!**************************************************************************
*! Name      : lfAcpPrice
*! Developer : WAB - Walid A. Wahab
*! Date      : 08/18/1999
*! Purpose   : to replace fileds to po temp lines when the user press ok butt.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfAcpPrice()
*!**************************************************************************
*-
FUNCTION lfAcpPrice
SELECT (lcPOLine)
REPLACE nSelPrice WITH m.nSelPrice ,;
        nGrosMrgn WITh m.nGrosMrgn 
*E301289,1 WAB - END

*E301289,1 WAB - START
*!**************************************************************************
*! Name      : lfCanPrice
*! Developer : WAB - Walid A. Wahab
*! Date      : 08/18/1999
*! Purpose   : to cancel and return the old value to the fields when user press
*!             cancel butt.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfCanPrice()
*!**************************************************************************
*-
FUNCTION lfCanPrice
m.nSelPrice = lnOldPrice 
m.nGrosMrgn = lnOldMrgn  
*E301289,1 WAB - END


*E301289,1 WAB - START
*!**************************************************************************
*! Name      : lfvPriceLvl
*! Developer : WAB - Walid A. Wahab
*! Date      : 08/18/1999
*! Purpose   : to get the selling price from style table Depends on price level
*!**************************************************************************
*! Calls     : lfvSelPrice()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfvPriceLvl()
*!**************************************************************************
*-
FUNCTION lfvPriceLvl
DO CASE 
  CASE lnPriceLvl = 2
    m.nSelPrice = STYLE.PRICEA
  CASE lnPriceLvl = 3
    m.nSelPrice = STYLE.PRICEB
  CASE lnPriceLvl = 4
    m.nSelPrice = STYLE.PRICEC
ENDCASE  
SHOW GET m.nSelPrice ENABLE
=lfvSelPrice()
*E301289,1 WAB - END


*E301289,1 WAB - START
*!**************************************************************************
*! Name      : lfvProfit
*! Developer : WAB - Walid A. Wahab
*! Date      : 08/29/1999
*! Purpose   : Call Profitability screen
*!**************************************************************************
*! Called From : The Option Menu -> Profitability PO
*! Calls     : 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfvProfit()
*!**************************************************************************
FUNCTION lfvProfit
DO (gcScrDir+"POPROFIT.SPX")

*E301289,1 WAB - END

*E301289,1 WAB - START
*!**************************************************************************
*! Name      : lfvProfit
*! Developer : WAB - Walid A. Wahab
*! Date      : 08/29/1999
*! Purpose   : calculate the total selling price & gross margin when call 
*!             Profitability screen
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfvProfit()
*!**************************************************************************
FUNCTION lfGetProfit
Private lnRecNo , lnAvrgPrc,lnTotQty
lnAlias=SELECT()
=lfRecalc()
SELECT (lcPOLine)
lnRecNo = RECNO()
STORE 0 TO lnPoctSel,lnUnitSel,lnTotQty,lnuntMrgn1,lnuntMrgn2,lnuntMrgn3,lnRotSub,lnTotcost

*--- sum ( selling price * total po line qty) for PO
SUM TotQty,nSelPrice *  TotQty TO lnTotQty,lnPoctSel
lnUnitSel = lnPoctSel / lnTotQty 		&& average selling price

*--- ESTIMATED
lnTotcost  = laData[30]				
lnRotSub   = IIF(llStyMark,lnTotcost,lnPoCtSel)
lnuntMrgn1 = IIF(lnRotSub = 0,0,((lnPoCtSel - lnTotcost)/lnRotSub)*100)		&& Gross Margin

*--- LANDED
lnTotcost  = laData[46]+laData[47]+laData[48]+laData[49]+laData[50]		
lnRotSub   = IIF(llStyMark,lnTotcost,lnPoCtSel)
lnuntMrgn2 = IIF(lnRotSub = 0,0,((lnPoCtSel - lnTotcost)/lnRotSub)*100)     && Gross Margin

*--- ACTUAL
lnTotcost  = laData[41]+laData[42]+laData[43]+laData[44]+laData[45]
lnRotSub   = IIF(llStyMark,lnTotcost,lnPoCtSel)
lnuntMrgn3 = IIF(lnRotSub = 0,0,((lnPoCtSel - lnTotcost)/lnRotSub)*100)     && Gross Margin

SELECT (lcPOLine)
IF BETWEEN(lnRecNo, 1, RECCOUNT())
  GO  lnRecNo
ELSE
  GO TOP
ENDIF
SELECT(lnAlias)
*E301289,1 WAB - END


*!**************************************************************************
*! Name      : lfvRatPLb  
*! Developer : RAMY 
*! Date      : 10/20/1999
*! Purpose   : Validate rate per lb.
*!**************************************************************************
*C200098,1 RAMY 10/20/1999
******************
FUNCTION lfvRatPLb  

IF ! (laData[3] $ 'HB')
  =gfModalGen('TRM38187B00000','DIALOG','dye order cost sheet was generated')
  laData[73] = lcOldValue
ELSE
  IF ! lfCkEOF(lcPoLine)
    =gfModalGen('TRM38187B00000','DIALOG','styles was entered')
    laData[73] = lcOldValue
  ENDIF
ENDIF
SHOW GET laData[73]
RETURN
*C101704,1 [Start]
*!**************************************************************************
*! Name      : lfPicture  
*! Developer : Sameh Aldesouki 
*! Date      : 01/17/2000
*! Purpose   : Change Picture of PO.
*!**************************************************************************
FUNCTION lfPicture

IF llGenOrNum
  RETURN "@! XXXXXX" 
ELSE
  RETURN "@! X99999"
ENDIF  
*C101704,1 [End]

*!**************************************************************************
*! Name      : lfRemCsElm
*! Developer : Ahmed Maher (AMH)
*! Date      : 06/19/2001
*! Purpose   : Remove related records in BomLine & CtktBom for removed line of PO.
*!**************************************************************************
*!*B804256,1 AMH
*!**************************************************************************
FUNCTION lfRemCsElm
PARAMETERS lcStyle

PRIVATE lnAlias, lcOldTag

lnAlias = SELECT()
SELECT BomLine
lcOldTag = SET('ORDER')
SET ORDER TO MfgOpr

IF SEEK('I'+laData[1])
  SCAN REST WHILE cImTyp+cTktNo+cOprCode+cType+Style+SClr+;
                  IIF(cCatgTyp$"MDP",PADR(MfgCode,12),Item)+IClr = ;
                  'I'+laData[1] FOR Style = lcStyle          
            *cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot
    IF SEEK("I"+laData[1]+BomLine.cBomTyp+BomLine.Item+BomLine.IClr+;
            BomLine.MfgCode+BomLine.Dyelot,'cTktBom')
      
      SELECT cTktBom
      REPLACE Pieces  WITH Pieces  - BomLine.StyQty ,;
              Req_Qty WITH Req_Qty - (BomLine.StyQty*BomLine.UnitQty),;
              UntQty  WITH IIF(Pieces<>0,Req_Qty/Pieces,0)
      IF Pieces = 0
        DELETE
      ELSE
        REPLACE Est_Cost WITH Est_Cost - BomLine.ItemAmt
        REPLACE UntCost  WITH IIF(Req_Qty<>0,Est_Cost/Req_Qty,0)
      ENDIF
    ENDIF
    SELECT BomLine
    DELETE
  ENDSCAN
ENDIF

SELECT BomLine
SET ORDER TO &lcOldTag.
SELECT(lnAlias)
*B804256,1 [End]

*!**************************************************************************
*! Name      : lfModCsElm
*! Developer : Ahmed Maher (AMH)
*! Date      : 07/01/2001
*! Purpose   : Modifay related records in BomLine & CtktBom for removed line of PO.
*!**************************************************************************
*!*B604618,1 AMH
*!**************************************************************************
FUNCTION lfModCsElm
PARAMETERS lcStyle

*B604618,4 AMH Rework for applay extended size scale case [Start]
*PRIVATE lnAlias, lcOldTag
PRIVATE lnAlias, lcOldTag, llExtSizSc, lnMjorCnt, laStySeg, lnCnt, llColorExt, lnColorStr
PRIVATE lnColorLen, lnSizePos, lnSizeLen, lcStyMask
*B604618,4 AMH [End]

lnAlias = SELECT()

*B604618,4 AMH Rework for applay extended size scale case [Start]
llExtSizSc = gfGetMemVar('M_USEEXSSC',gcAct_Comp)
STORE 0 TO lnSizePos, lnSizeLen
STORE .F. TO llColorExt
*-- Count of the major part.
DECLARE laStySeg[1,1]
lnMjorCnt  = gfItemMask("SM")
=gfItemMask(@laStySeg)    
FOR lnCnt = lnMjorCnt + 1 TO ALEN(laStySeg,1)
  IF laStySeg[lnCnt , 1] = "C"
    *-- Flag to know if there is color in the style code strucure.
    llColorExt = .T.
    *-- Var. hold the start position of the color segment in the style code strucure.
    lnColorStr = laStySeg[lnCnt , 4]
    *-- Var. hold the color segment lenght in the style code strucure.
    lnColorLen = LEN(laStySeg[lnCnt , 3])
  ELSE
    *-- See if there is extended size scale in the style structure or not.
    IF llExtSizSc .AND. laStySeg[lnCnt , 1] = "S"
      lnSizePos  = laStySeg[lnCnt , 4]
      lnSizeLen  = LEN(laStySeg[lnCnt , 3])
    ENDIF
  ENDIF
ENDFOR
*B604618,4 AMH [End]

SELECT BomLine
lcOldTag = SET('ORDER')
SET ORDER TO MfgOpr

IF SEEK('I'+laData[1])
  SCAN REST WHILE cImTyp+cTktNo+cOprCode+cType+Style+SClr+;
                  IIF(cCatgTyp$"MDP",PADR(MfgCode,12),Item)+IClr = ;
                  'I'+laData[1] FOR Style = lcStyle
    *B604618,4 AMH Rework for applay extended size scale case [Start]
    lcStyMask = IIF(llColorExt,STUFF(STYLE,lnColorStr,lnColorLen,REPLICATE('*',lnColorLen)),STYLE)
    lcStyMask = IIF(llExtSizSc,STUFF(lcStyMask,lnSizePos,lnSizeLen,REPLICATE('*',lnSizeLen)),lcStyMask)
    lcItmMask = IIF(llColorExt,STUFF(ITEM,lnColorStr,lnColorLen,REPLICATE('*',lnColorLen)),ITEM)
    lcItmMask = IIF(llExtSizSc,STUFF(lcItmMask,lnSizePos,lnSizeLen,REPLICATE('*',lnSizeLen)),lcItmMask)
    *IF SEEK(PADR(SUBSTR(STYLE,1,lnMjrWid),19)+CBOMTYP+STYLE+MFGCODE+ITEM+ICLR,'BOM') .OR.;
       SEEK(PADR(SUBSTR(STYLE,1,lnMjrWid),19)+CBOMTYP+;
            STUFF(STYLE,laMajSeg[lnColStru,4],LEN(laMajSeg[lnColStru,3]),'******')+;
            MFGCODE+ITEM+ICLR,'BOM') .OR. ;
       SEEK(PADR(SUBSTR(STYLE,1,lnMjrWid),19)+CBOMTYP+STYLE+MFGCODE+ITEM+'******','BOM') .OR.;
       SEEK(PADR(SUBSTR(STYLE,1,lnMjrWid),19)+CBOMTYP+;
            STUFF(STYLE,laMajSeg[lnColStru,4],LEN(laMajSeg[lnColStru,3]),'******')+;
            MFGCODE+ITEM+'******','BOM')
    IF SEEK(PADR(SUBSTR(STYLE,1,lnMjrWid),19)+CBOMTYP+STYLE+MFGCODE+ITEM+ICLR,'BOM') .OR.;
       SEEK(PADR(SUBSTR(STYLE,1,lnMjrWid),19)+CBOMTYP+lcStyMask+MFGCODE+ITEM+ICLR,'BOM') .OR. ;
       SEEK(PADR(SUBSTR(STYLE,1,lnMjrWid),19)+CBOMTYP+STYLE+MFGCODE+ITEM+'******','BOM') .OR.;
       SEEK(PADR(SUBSTR(STYLE,1,lnMjrWid),19)+CBOMTYP+lcStyMask+MFGCODE+ITEM+'******','BOM') .OR.;
       SEEK(PADR(SUBSTR(STYLE,1,lnMjrWid),19)+CBOMTYP+STYLE+MFGCODE+lcItmMask+ICLR,'BOM') .OR.;
       SEEK(PADR(SUBSTR(STYLE,1,lnMjrWid),19)+CBOMTYP+lcStyMask+MFGCODE+lcItmMask+ICLR,'BOM')
    *B604618,4 AMH [End]
      LOOP
    ENDIF
    REPLACE STYQTY  WITH STYQTY + laBomQty[9],;
            ITEMQTY WITH STYQTY  * UNITQTY,;
            ITEMAMT WITH ITEMQTY * UNITCOST
    IF SEEK("I"+laData[1]+BomLine.cBomTyp+BomLine.Item+BomLine.IClr+;
            BomLine.MfgCode+BomLine.Dyelot,'cTktBom')
      
      SELECT cTktBom
      REPLACE Pieces  WITH Pieces  + laBomQty[9] ,;
              Req_Qty WITH Req_Qty + (laBomQty[9]*BomLine.UnitQty),;
              UntQty  WITH IIF(Pieces<>0,Req_Qty/Pieces,0)
      IF Pieces = 0
        DELETE
      ELSE
        REPLACE Est_Cost WITH Est_Cost + (laBomQty[9]*BOMLINE.UNITQTY*BOMLINE.UNITCOST)
        REPLACE UntCost  WITH IIF(Req_Qty<>0,Est_Cost/Req_Qty,0)
      ENDIF
    ENDIF
    SELECT BOMLINE
    IF STYQTY = 0
      DELETE
    ENDIF
  ENDSCAN
ENDIF

SELECT BomLine
SET ORDER TO &lcOldTag.
SELECT(lnAlias)
*B604618,1 [End]

*!*************************************************************
*! Name    : lfModBom
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/14/2002
*! Purpose : Function to update po bill of material for modified and removed lines
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
*B606496,1 AMH
FUNCTION lfModBom

PRIVATE lcTmpBomLn, lcTrTType, lcMjrMsk, lcSizes, lnI
lcMjrMsk  = gfItemMask("PM")

lcTmpBomLn = gfTempName()
SELECT BOM
=AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcTmpBomLn) FROM ARRAY laFileStru
INDEX ON citmmajor+typ+citmmask+mfgcode+item+iclr TAG (lcTmpBomLn)
lcTrTType =  IIF(lcAType="D","D","I")
SELECT BomLine
SEEK lcTrTType+"1"+laData[1]+STR(lnPoLNo,6)
SCAN REST WHILE cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+Item+IClr+MfgCode =;
                lcTrTType+"1"+laData[1]+STR(lnPoLNo,6) ;
            FOR Style = lcStyle
  llTrim_Inv = .F.
  lcRecDesc  = ""
  lcUom      = ''
  lcMarker   = ''
  llBaseOnSz = .F.
  IF SEEK(lcTrTType+laData[1]+cBomtyp+Item+IClr+MfgCode+dyelot,'CTktBom')
    llTrim_Inv = cTktBom.Trim_Invt
    lcRecDesc  = cTktBom.Desc
    lcUom      = cTktBom.Uom
    lcMarker   = cTktBom.cMarker
  ENDIF
  IF SEEK(SUBSTR(lcStyle,1,LEN(lcMjrMsk)),'Bom')
    SELECT Bom
    LOCATE REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr=SUBSTR(lcStyle,1,LEN(lcMjrMsk));
                FOR lBasOnSiz
    llBaseOnSz = FOUND()
  ENDIF
  lcSizes = ''
  
  *B607009,1 AMH Check if the containtes of csizes field not empty [Start]
  *IF LEN(BOMLINE.CSIZES) > 1
  IF LEN(ALLTRIM(BOMLINE.CSIZES)) > 1
  *B607009,1 AMH [End]
  
    FOR lnI = 1 TO LEN(BOMLINE.CSIZES)-1
      lcSizes = lcSizes + SUBSTR(BOMLINE.CSIZES,lnI,1) + ','
    ENDFOR
    lcSizes = lcSizes + SUBSTR(BOMLINE.CSIZES,LEN(BOMLINE.CSIZES),1)
  ELSE
    lcSizes = BOMLINE.CSIZES
  ENDIF
  IF !EMPTY(lcSizes)
    lcSizes = POSLN.SCALE + '~' + lcSizes
  ENDIF
  lcCrsRef = ''
  
  *B607009,1 AMH Check if the containtes of ccompsizes field not empty [Start]
  *IF LEN(BOMLINE.CCOMPSIZES) > 0
    *FOR lnI = 1 TO LEN(BOMLINE.CCOMPSIZES)
  IF LEN(ALLTRIM(BOMLINE.CCOMPSIZES)) > 0
    FOR lnI = 1 TO LEN(ALLTRIM(BOMLINE.CCOMPSIZES))
      =SEEK(BOMLINE.ITEM,'STYLE')
      *lcCrsRef = lcCrsRef+POSLN.SCALE+','+SUBSTR(BOMLINE.CCOMPSIZES,lnI,1)+'~'+STYLE.SCALE+','+;
                 SUBSTR(BOMLINE.CSIZES,lnI,1)+CHR(13)+CHR(10)
      lcCrsRef = lcCrsRef+POSLN.SCALE+','+SUBSTR(BOMLINE.CSIZES,lnI,1)+'~'+STYLE.SCALE+','+;
                 SUBSTR(BOMLINE.CCOMPSIZES,lnI,1)+CHR(13)+CHR(10)
  *B607009,1 AMH [End]
  
    ENDFOR
  ENDIF
  SELECT (lcTmpBomLn)
  APPEND BLANK
  REPLACE cItmMajor  WITH SUBSTR(lcStyle,1,LEN(lcMjrMsk)) ,;
          Typ        WITH BomLine.cBomtyp  ,;
          cItmMask   WITH lcStyle          ,;
          cOprCode   WITH BomLine.cOprCode ,;
          MfgCode    WITH BomLine.MfgCode  ,;
          Item       WITH BomLine.Item     ,;
          IClr       WITH BomLine.IClr     ,;
          Trim_Invt  WITH llTrim_Inv       ,;
          Desc       WITH lcRecDesc        ,;
          lBasOnSiz  WITH llBaseOnSz
  REPLACE MSIZES     WITH lcSizes          ,;
          MSZCROSREF WITH lcCrsRef         ,;
          UOM        WITH lcUom            ,;
          UNTCOST    WITH BOMLINE.UNITCOST ,;
          NPERCENT   WITH BOMLINE.NPERCENT ,;
          NESTBOMQTY WITH BOMLINE.UNITQTY  ,;
          NBOMWASTGE WITH 0                ,;
          NBOMTOTQTY WITH NESTBOMQTY       ,;
          TOTCOST    WITH NESTBOMQTY*UNTCOST,;
          CCOSTSTAT  WITH BOMLINE.CCOSTSTAT
  REPLACE CCATGTYP   WITH BOMLINE.CCATGTYP ,;
          CMARKER    WITH lcMarker
ENDSCAN

llRetVal = gfSheetItem (IIF(lcAType="D","D","I"), laData[1] , lcLinkCode, lcStyle    ,;
                        ""       , lnPoLNo   , lcCostDye , POSHDR.cItemWare ,;
                        POSHDR.cMatWare, @laBomQty,lcTmpBomLn, "CtktBom"  ,;
                        "BomLine", "MFGOprHd", lcLastOpr , lnPrice    ,;
                        0, 0, 0, 0, 0,lcWinCh0)
USE IN (lcTmpBomLn)
ERASE (gcWorkDir+lcTmpBomLn+'.DBF')
ERASE (gcWorkDir+lcTmpBomLn+'.CDX')
ERASE (gcWorkDir+lcTmpBomLn+'.FPT')
*-- end of lfModBom.

*!***********************************************************************************
*! Name      : lfGenProj
*! Developer : Hend Ghanem (HBG)
*! Date      : 07/04/2002
*! Purpose   : Creat temp files needed to generat project
*!***********************************************************************************
*! Calls     : 
*!***********************************************************************************
*! Parameters: None
*!***********************************************************************************
*! Returns   :  None.
*!***********************************************************************************
*! Example   :  =lfGenProj()
*!***********************************************************************************
*!E301869,1
FUNCTION lfCrtprjTm

CREATE CURSOR (lc_Parser);
       (cOprt_Ctg C(3), cOprt_ID C(5), dStrtDate D(8), nDurIndic N(1)) 

=gfOpenFile(gcDataDir + 'PMPRJHD'  , 'PMPRJHD' , 'SH')
=gfOpenFile(gcDataDir + 'PMPRJDT'  , 'PMPRJDT' , 'SH')
=gfOpenFile(gcDataDir + 'PMPRJRL'  , 'PMPRJRL' , 'SH')
*-- Create a table with the same structure as PMPRJDT 
*-- adding two more fields for saving.
SELECT PMPRJDT
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru, 1)
DIMENSION laFileStru(lnFileStru + 3, 4)
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cStatus'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 1
laFileStru[lnFileStru ,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'nRecNo'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 10
laFileStru[lnFileStru ,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cMComplt'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 3
laFileStru[lnFileStru ,4] = 0

CREATE CURSOR (lc_PMPrjDt) FROM ARRAY laFileStru
INDEX ON cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID;
      TAG PMPRJDT OF (lc_PMPrjDt)
INDEX ON cPrj_Typ + cPrj_ID + cStyle + cCtg_Seq + cOprt_Seq;
      TAG PMPRJDTS OF (lc_PMPrjDt)
INDEX ON cPrj_Typ+cPrj_Id+cOprt_Ctg+cOprt_Id+cOprt_Res;
              TAG PMPRJUSR OF (lc_PMPrjDt)
                
CREATE CURSOR (lc_PrjHist) (cOpr_ID C(20),cUser_ID C(10),cRemain C(3),cCompDate C(8),cActnDate C(8),cStatus C(15))
INDEX ON cOpr_ID TAG PMPRJDT OF (lc_PrjHist)
 
CREATE CURSOR (lc_PrjAudt) FROM ARRAY laFileStru
INDEX ON cOprt_Ctg + cOprt_ID TAG PMPRJDT OF (lc_PrjAudt)
  
SELECT PMPRJRL
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru, 1)
DIMENSION laFileStru(lnFileStru + 2, 4)
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cStatus'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 1
laFileStru[lnFileStru ,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'nRecNo'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 10
laFileStru[lnFileStru ,4] = 0
CREATE CURSOR (lc_PMPrjRl) FROM ARRAY laFileStru
INDEX ON cPrj_Typ + cPrj_ID + cStyle + cPrd_Ctg  + cPrd_ID;
       TAG PMPRJRLP OF (lc_PMPrjRl)
INDEX ON cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID;
       TAG PMPRJRL  OF (lc_PMPrjRl)

*!***********************************************************************************
*! Name      : lfGenProj
*! Developer : Hend Ghanem (HBG)
*! Date      : 07/04/2002
*! Purpose   : Generat project for the Ctktkt according to yhe setting of the module
*!***********************************************************************************
*! Calls     : 
*!***********************************************************************************
*! Parameters: None
*!***********************************************************************************
*! Returns   :  None.
*!***********************************************************************************
*! Example   :  =lfGenProj()
*!***********************************************************************************
*!E301869,1
FUNCTION lfGenProj

FOR lnI = 1 TO ALEN(laScrMode,1)
  IF laScrMode[lnI]
    lnScrMode = lnI
  ENDIF
ENDFOR
*-- Open the needed files
=gfOpenFile(gcDataDir + 'PMPTHHD'  , 'PMPTHHD' , 'SH')
=gfOpenFile(gcDataDir + 'PMPTHDT'  , 'PMPTHDT' , 'SH')
=gfOpenFile(gcDataDir + 'PMPTHRL'  , 'PMPTHRL' , 'SH')
=gfOpenFile(gcDataDir + 'AUDTRAIL' , 'AUDTRAIL', 'SH')
=gfOpenFile(gcDataDir + 'PMCTGHD'  , 'PMCTGHD' , 'SH')
=gfOpenFile(gcDataDir + 'PMCTGDT'  , 'PMCTGDT' , 'SH')
=gfOpenFile(gcDataDir + 'PMCALHD'  , 'PMCALHD' , 'SH')
=gfOpenFile(gcDataDir + 'PMCALDT'  , 'PMCALDT' , 'SH')
=gfOpenFile(gcDataDir + 'pmprjntf'  , 'pmprjntf' , 'SH')
=gfOpenFile(gcDataDir + 'pmpthntf'  , 'pmpthntf' , 'SH')
=gfOpenFile(gcSysHome + 'SYUUSER'  , 'Cuser_id', 'SH')
=gfOpenFile(gcSysHome + 'SYSCHDUL' , 'Coprusr' , 'SH')

DECLARE laHolidays[1]
laHolidays = ''
lnRows = 0
SELECT PMCALDT
SCAN
  ldCal_HFrm = dCal_HFrm
  DO WHILE ldCal_HFrm <= dCal_HTo
    lnRows  = lnRows  + 1
    DIMENSION laHolidays[lnRows]
    laHolidays[lnRows] = cCal_ID + DTOC(ldCal_HFrm)
    ldCal_HFrm = ldCal_HFrm + 1
  ENDDO  
ENDSCAN
lVoid = .F.
STORE gdSysDate TO ldCurDate , m.dEst_Strt , m.dclc_strt
m.dEst_Fnsh = {}
llupdstrtd = .F.
DO CASE 
  CASE lcGenProj = 'I'    && Generating Project Inquir
    
    *E301869,1 AMH Consider diffrance types [Start]
    *lnChoice =gfModalGen('INM38251B32000','DIALOG','style purchase order')
    lnChoice =gfModalGen('INM38251B32000','DIALOG',lcPOType)
    *E301869,1 AMH [End]
    
    IF lnChoice = 1
      PUSH KEY
      DO lfvOprtins IN gcAppHome+"MFPROJ" WITH lcAType,laData[1],lcDefTemp,REPLICATE('*', 12)
      POP KEY
      =lfUpdMastr()
    ENDIF  

  CASE lcGenProj = 'A'    && Generating Project Automatic
    PUSH KEY
    DO lfvOprtins IN gcAppHome+"MFPROJ" WITH lcAType,laData[1],lcDefTemp,REPLICATE('*', 12)
    POP KEY
    =lfUpdMastr()
ENDCASE

STORE .F. To laScrMode
laScrMode[lnScrMode] = .T.


*!***********************************************************************************
*! Name      : lfUpdMastr
*! Developer : Hend Ghanem (HBG)
*! Date      : 07/04/2002
*! Purpose   : Update master files of production schedule
*!***********************************************************************************
*! Calls     : 
*!***********************************************************************************
*! Parameters: None
*!***********************************************************************************
*! Returns   :  None.
*!***********************************************************************************
*! Example   :  =lfUpdMastr()
*!***********************************************************************************
*!E301869,1
FUNCTION lfUpdMastr


INSERT INTO PMPRJHD (cprj_typ,cprj_id,cstyle,cprj_sdsc,cpath_id,cprj_stts,;
                    llaststrt,dest_strt,dest_fnsh,dreq_strt,dreq_fnsh,lschedual,llok_stat);
             VALUES (lcAType,laData[1],REPLICATE('*', 12),ALLTRIM(laData[2])+', All Styles',lcDefTemp,'P',;
                    .F.,m.dest_strt,m.dest_fnsh,laData[5],laData[6],.F.,.T.)

SELECT (lc_PMPrjDt)
SCAN
  SCATTER MEMVAR MEMO
  SELECT PMPRJDT
  m.LORGINAL = .T.
  INSERT INTO PMPRJDT FROM MEMVAR

  IF !EMPTY(m.cOprt_res) OR !EMPTY(m.cGroup_Id) 
    *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]
    *IF !SEEK(m.cPrj_Typ+ m.cPrj_ID+m.cOprt_Ctg+m.cOprt_ID,'SYSCHDUL')
    *  INSERT INTO SYSCHDUL;
    *        (cconttype , cseqnumber , ccont_id , csubject,;
    *         ctrantype,nestdur,ccompleted,cUser_Id,dtrandate,dcmpltdate,COPERSTAT,;
    *         lPredComp);
    *  VALUES(m.cPrj_Typ,m.cPrj_ID,m.cOprt_Ctg+m.cOprt_ID,;
    *         m.coprt_dsc,'W',m.nest_dur,'N',;
    *         IIF(EMPTY(m.cOprt_res),m.cGroup_Id,m.cOprt_res),;
    *         IIF(EMPTY(m.dclc_strt),m.dEst_strt,m.dclc_strt),;
    *         IIF(EMPTY(m.dclc_Fnsh),m.dEst_fnsh,m.dclc_Fnsh),'O',.T.)
    IF !SEEK(m.cPrj_Typ+ m.cPrj_ID+m.cStyle+m.cOprt_Ctg+m.cOprt_ID,'SYSCHDUL')
      INSERT INTO SYSCHDUL;
            (cconttype , cseqnumber ,cStyle, ccont_id , csubject,;
             ctrantype,nestdur,ccompleted,cUser_Id,dtrandate,dcmpltdate,COPERSTAT,;
             lPredComp);
      VALUES(m.cPrj_Typ,m.cPrj_ID,m.cStyle,m.cOprt_Ctg+m.cOprt_ID,;
             m.coprt_dsc,'W',m.nest_dur,'N',;
             IIF(EMPTY(m.cOprt_res),m.cGroup_Id,m.cOprt_res),;
             IIF(EMPTY(m.dclc_strt),m.dEst_strt,m.dclc_strt),;
             IIF(EMPTY(m.dclc_Fnsh),m.dEst_fnsh,m.dclc_Fnsh),'O',.T.)
    *B130984,1 HBG 12/04/2006 Add [End]
    ELSE
      REPLACE SYSCHDUL.cUser_Id   WITH IIF(EMPTY(m.cOprt_res),m.cGroup_Id,m.cOprt_res),;
              SYSCHDUL.csubject   WITH m.coprt_dsc,;
              SYSCHDUL.ctrantype  WITH 'W',;
              SYSCHDUL.nestdur    WITH m.nest_dur ,;
              SYSCHDUL.ccompleted WITH 'N'  ,;
              SYSCHDUL.dtrandate  WITH IIF(EMPTY(m.dclc_strt),m.dEst_strt,m.dclc_strt),;
              SYSCHDUL.dcmpltdate WITH IIF(EMPTY(m.dclc_Fnsh),m.dEst_fnsh,m.dclc_Fnsh),;
              SYSCHDUL.COPERSTAT  WITH 'O',;
              SYSCHDUL.lPredComp  WITH .T.                   
    ENDIF
    *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]    
    *IF SEEK(m.cPrj_Typ+ m.cPrj_ID+m.cOprt_Ctg+m.cOprt_ID,'SYSCHDUL')
    IF SEEK(m.cPrj_Typ+ m.cPrj_ID+m.cStyle+m.cOprt_Ctg+m.cOprt_ID,'SYSCHDUL')
    *B130984,1 HBG 12/04/2006 [End]
      IF m.nAct_Dur <> 0 OR !EMPTY(m.dAct_Fnsh)
        REPLACE SYSCHDUL.COPERSTAT  WITH 'C',;
                SYSCHDUL.CCOMPLETED WITH 'Y'    
      ENDIF
      IF m.lVoid
        REPLACE SYSCHDUL.COPERSTAT WITH 'X'
      ENDIF        
    ENDIF  
  ENDIF  

  *-- Update related records for every line.
  SELECT (lc_PMPrjRl)
  *-- Add cOprt_Ctg
  
  SCAN FOR cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg  + cOprt_ID = ;
                       &lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_Id+;
                       &lc_PMPrjDt..cStyle + &lc_PMPrjDt..cOprt_Ctg + &lc_PMPrjDt..cOprt_ID
  
    SCATTER MEMVAR MEMO
    
    DO CASE
      *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]
      *CASE SEEK(m.cPrj_Typ+m.cPrj_Id+m.cPrd_Ctg+m.cPrd_Id+'O','SYSCHDUL') 
      *  =SEEK(m.cPrj_Typ+ m.cPrj_ID+m.coprt_ctg+m.coprt_id+'O','SYSCHDUL')
      CASE SEEK(m.cPrj_Typ+m.cPrj_Id+m.cStyle+m.cPrd_Ctg+m.cPrd_Id+'O','SYSCHDUL') 
        =SEEK(m.cPrj_Typ+ m.cPrj_ID+m.cStyle+m.coprt_ctg+m.coprt_id+'O','SYSCHDUL')
      *B130984,1 HBG 12/04/2006 [End]      
        REPLACE SYSCHDUL.lPredComp WITH .F.
      *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]  
      *CASE SEEK(m.cPrj_Typ+m.cPrj_Id+m.cPrd_Ctg+m.cPrd_Id+'C','SYSCHDUL')
      *  =SEEK(m.cPrj_Typ+ m.cPrj_ID+m.coprt_ctg+m.coprt_id+'O','SYSCHDUL')
      CASE SEEK(m.cPrj_Typ+m.cPrj_Id+m.cStyle+m.cPrd_Ctg+m.cPrd_Id+'C','SYSCHDUL')
        =SEEK(m.cPrj_Typ+ m.cPrj_ID+m.cStyle+m.coprt_ctg+m.coprt_id+'O','SYSCHDUL')
      *B130984,1 HBG 12/04/2006 [End]      
        REPLACE SYSCHDUL.lPredComp WITH .T.
      *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]  
      *CASE SEEK(m.cPrj_Typ+m.cPrj_Id+m.cPrd_Ctg+m.cPrd_Id+'X','SYSCHDUL')
      *  =SEEK(m.cPrj_Typ+ m.cPrj_ID+m.coprt_ctg+m.coprt_id+'O','SYSCHDUL')
      CASE SEEK(m.cPrj_Typ+m.cPrj_Id+m.cStyle+m.cPrd_Ctg+m.cPrd_Id+'X','SYSCHDUL')
        =SEEK(m.cPrj_Typ+ m.cPrj_ID+m.cStyle+m.coprt_ctg+m.coprt_id+'O','SYSCHDUL')
      *B130984,1 HBG 12/04/2006 [End]      
        REPLACE SYSCHDUL.lPredComp WITH .T.
    ENDCASE      

    SELECT PMPRJRL
    INSERT INTO PMPRJRL FROM MEMVAR
  ENDSCAN 
  
ENDSCAN

*-- Update Audit Trail
lcProg  = 'POSTY'
lcKey   = lcAType + laData[1]
DO CASE
  CASE lcAType = 'A'      && Adorment Order
    lcEvent = 'RESCHDPA'      
    lcProg  = 'MFADPO'
    *HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
    lcApObjNam = 'MFADPO'
    *HBG [End]
  CASE lcAType = 'D'      && Dye Order 
    lcEvent = 'RESCHDPD'      
    lcProg  = 'MFDPO'
    *HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
    lcApObjNam = 'MFDPO'
    *HBG [End]
  CASE lcAType = 'P'      && PO
    lcEvent = 'RESCHDPO'
    lcProg  = 'POSTY'
    *HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
    lcApObjNam = 'POSTY'
    *HBG [End]
  CASE lcAType = 'N'      && Inter-Location PO
    lcEvent = 'RESCHDPN'      
    lcProg  = 'POINTRC'
    *HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
    lcApObjNam = 'POINTRC'
    *HBG [End]
  CASE lcAType = 'R'      && Return PO
    lcEvent = 'RESCHDPR'      
    lcProg  = 'RETPO'
    *HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
    lcApObjNam = 'RETPO'
    *HBG [End]
ENDCASE


IF !SEEK(PADR(lcProg,10)+PADR(lcKey,20),'AUDTRAIL')    
  SELECT (lc_PMPrjDt)
  SCAN
    *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]
    *IF SEEK(&lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID + &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID,'SYSCHDUL') 
    IF SEEK(&lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID+&lc_PMPrjDt..cStyle + &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID,'SYSCHDUL') 
    *B130984,1 HBG 12/04/2006 Add [End]
      IF !(&lc_PMPrjDt..lOrginal)
        SELECT SYSCHDUL
        lcStauts = SYSCHDUL.COPERSTAT
        *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]
        *LOCATE REST WHILE cconttype+cseqnumber+ccont_id+coperstat+cuser_id =;
        *                &lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID + &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID ;
        *                FOR COPERSTAT <> lcStauts
        LOCATE REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id =;
                        &lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID + &lc_PMPrjDt..cStyle + &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID ;
                        FOR COPERSTAT <> lcStauts
        *B130984,1 HBG 12/04/2006 Add [End]        
        IF !FOUND()
          *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]
          *=SEEK(&lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID + &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID+lcStauts,'SYSCHDUL') 
          =SEEK(&lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID + &lc_PMPrjDt..cStyle + &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID+lcStauts,'SYSCHDUL') 
          *B130984,1 HBG 12/04/2006 [End]
        ENDIF                    
      ENDIF
   
      IF SYSCHDUL.lPredComp 
        SELECT (lc_PMPrjDt)
        SCATTER MEMVAR MEMO
        ldest_Fnsh = m.dest_Fnsh
        lnrem_dur  = m.nrem_dur
        *HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
        lcInform = ""
        *HBG [End]        
        DO lfUpdAdTrl IN gcAppHome+"MFPROJ" WITH lcAType
        SELECT (lc_PrjAudt)
        *HBG 8/1/2003 Call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
        *IF ASCAN(laEvntTrig , PADR(lcEvent,10)) <> 0
          *=gfDoTriger(lcProg,PADR(lcEvent,10))
        *ENDIF
        DO gcAppHome+"SY\"+"GFAUDTRL" WITH lcProg , lcKey , lcApObjNam ,lcEvent,lcInform        
        *HBG 8/1/2003 Call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
      ENDIF 
    ENDIF   
  ENDSCAN
ENDIF

SELECT PMPRJHD
REPLACE llok_stat WITH .F.


*!***********************************************************************************
*! Name      : lfUpdPrjSt
*! Developer : Hend Ghanem (HBG)
*! Date      : 07/04/2002
*! Purpose   : Update Project Status
*!***********************************************************************************
*! Calls     : 
*!***********************************************************************************
*! Parameters: None
*!***********************************************************************************
*! Returns   :  None.
*!***********************************************************************************
*! Example   :  =lfUpdMastr()
*!***********************************************************************************
*!E301869,1
FUNCTION lfUpdPrjSt

=gfOpenFile(gcDataDir + 'PMPRJHD'  , 'PMPRJHD' , 'SH')
=gfOpenFile(gcDataDir + 'PMPRJDT'  , 'PMPRJDT' , 'SH')
=gfOpenFile(gcSysHome + 'SYSCHDUL' , 'Coprusr' , 'SH')
=gfOpenFile(gcDataDir + 'AUDTRAIL' , 'AUDTRAIL', 'SH')
IF SEEK(lcAType + POSHDR.PO ,'PMPRJHD') AND !(PMPRJHD.cprj_stts $ 'CH')
  IF POSHDR.STATUS = 'X'
    SELECT PMPRJHD
    REPLACE PMPRJHD.cprj_stts WITH 'X'
    lcProg  = 'POSTY'
    lcKey   = lcAType + POSHDR.PO
    SELECT PMPRJDT
    IF SEEK(SUBSTR(PMPRJHD.cPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(PMPRJHD.cPrj_ID,1,LEN(cPrj_ID)) +;
            SUBSTR(PMPRJHD.cStyle,1,LEN(cStyle)))
      SCAN REST WHILE cprj_typ+cprj_id+cstyle+coprt_ctg+coprt_id =;
             SUBSTR(PMPRJHD.cPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(PMPRJHD.cPrj_ID,1,LEN(cPrj_ID)) +;
             SUBSTR(PMPRJHD.cStyle,1,LEN(cStyle))
        *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]     
        *IF SEEK(PMPRJDT.cPrj_Typ + PMPRJDT.cPrj_ID + PMPRJDT.cOprt_Ctg+PMPRJDT.cOprt_ID,'SYSCHDUL')
        IF SEEK(PMPRJDT.cPrj_Typ + PMPRJDT.cPrj_ID + PMPRJDT.cStyle + PMPRJDT.cOprt_Ctg+PMPRJDT.cOprt_ID,'SYSCHDUL')
        *B130984,1 HBG 12/04/2006 [End]
          SELECT SYSCHDUL
          REPLACE SYSCHDUL.COPERSTAT WITH 'X'
        ENDIF
      ENDSCAN
    ENDIF
    SELECT AUDTRAIL
    DELETE FOR capobjnam+key+caudtralid = PADR(lcProg,10)+PADR(lcKey,20) 
  ENDIF
ENDIF
*:**********************************************************


*:**************************************************************************
*:* Name        : lfCstShUpd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/04/2007
*:* Purpose     : create a temp cost sheet lines to calcualte the dutable value 
*               : that based on percent calculations
*:***************************************************************************
*:* Called from : lfvStyle()
*:***************************************************************************
*B608125 TMI 
FUNCTION lfCstShUpd
PARAMETERS llUpdAll

PRIVATE    lcMjrMsk,lcMjrHdr,lcNMjrMsk,laItemSeg,lcClrMsk,lnClrPos,lcItemFile,llDyelot,lcScale,lcItemType,;
           lnTranQty,lcSizes,lnCount,lcCstClr,lnUnitPri,lcCstItm,laMfgRFld,lnLastSeq,lnSizePos,;
           lcTmpBomSh,laReq,lcContCode,lcContName,lcOperSeq,llInHouse,llMfgOpr,lnLeadTime

PRIVATE    lcMfgGlAcnt,lcExSign,lcUntSin,lnAlias,lcPriceCur,lnPriceRate,lnPriceUnit,lcDutyCur,lnDutyRate,;
           lnDutyUnit,lcCostElem,lnOldTrnQt

PRIVATE    lcTranType,lcTicketNo,lcLinkCode,lcItem,lcColor,lnLineNo,lcDyelot,lcStyWare,lcMatWare,laQty,lcTktSheet,;
           lcDetFile,lcOprHdr,lcLastOpr,lnPrice

PRIVATE lnRecno,lnSlct
lnSlct = SELECT(0)

SELECT &lcPoLine
lnRecno = RECNO()

IF llUpdAll
  REPLACE lRefCost WITH .F. ALL
ENDIF

*- try to find records that its related costing needs to be refreshed
LOCATE FOR !&lcPoLine..lRefCost
IF !FOUND()
  IF BETWEEN(lnRecno,1,RECCOUNT(lcPoLine))
    GOTO (lnRecno)
  ENDIF
  SELECT (lnSlct)
  RETURN
ENDIF

*- Open needed files
=gfOpenFile(gcDataDir+'FABRIC','FABRIC','SH')
=gfOpenFile(gcDataDir+'FABDYE','FABDYE','SH')
=gfOpenFile(gcDataDir+'BOM','BOM','SH')
=gfOpenFile(gcDataDir+'Ctktbom','Ctktbom','SH')
=gfOpenFile(gcDataDir+'BomLine','BomLine','SH')
=gfOpenFile(gcDataDir+'MFGOPRHD','MFGOPRHD','SH')
=gfOpenFile(gcDataDir+'BOMLINE','BOMLINE','SH')

lcDetFile = gfTempName()
SELECT BOMLINE
=AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcDetFile) FROM ARRAY laFileStru
INDEX ON cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode TAG 'BOMLINE'

lcTktSheet = gfTempName()
SELECT CTKTBOM
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'cShowType'  && tmi - not used in this funcion
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0
CREATE TABLE (gcWorkDir+lcTktSheet) FROM ARRAY laFileStru
INDEX ON cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot TAG 'CTKTBOM'
INDEX ON TYP+cShowType+ITEM+ICLR+MFGCODE+DYELOT TAG (lcTktSheet)  ADDITIVE

*-- Create needed relations 
SELECT &lcTktSheet
SET RELATION TO ITEM INTO STYLE
SET RELATION TO item+cwarecode+dyelot INTO STYDYE ADDITIVE
SET RELATION TO SUBSTR(item,1,7)+iclr+cwarecode+dyelot INTO FABDYE ADDITIVE
SET RELATION TO SUBSTR(item,1,7)+iclr INTO FABRIC ADDITIVE
SELECT STYLE
SET RELATION TO 'S'+SCALE INTO SCALE

*- use only one peice to calculate the cost elements for each peice 
PRIVATE laQty
DIMENSION laQty[9]
laQty = 0
SELECT &lcPoLine
SCATTER FIELDS QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY TO laQty

lcCostElem = gfTempName()
CREATE CURSOR (lcCostElem) (cTrType C(1), cType C(1), cItem C(19), cIClr C(6),;
                            cMFGCode C(6), cDyelot C(10), cSizes C(8))
INDEX ON cTrType+cType+CItem+CIclr+cMFGCode+cDyelot TAG (lcCostElem) OF (lcCostElem)

PRIVATE lcTranLett
lcticketno = laData[1]
lcTranType = 'I'

lcTranLett = IIF(lcTranType $ "IMTD",lcTranType ,"I")

STORE 1   TO lnPriceRate,lnPriceUnit,lnDutyRate,lnDutyUnit
STORE '/' TO lcExSign,lcUntSin
IF lcTranType = 'I' 
  lcPriceCur  = laData[26]
  lnPriceRate = laData[28]
  lnPriceUnit = lnCurrUnt1                    && IIF(POSHDR.nCurrUnit=0,1,POSHDR.nCurrUnit)
  lcDutyCur   = laData[27]
  lnDutyRate  = laData[29]
  lnDutyUnit  = lnCurrUnt2                    && IIF(POSHDR.nDCurUnit=0,1,POSHDR.nDCurUnit)
ENDIF

STORE ''  TO M_WAREHOUSE,M_DYELOT,M_MATDYE,M_USEEXSSC
=gfGetMemVar('M_WAREHOUSE,M_DYELOT,M_MATDYE,M_USEEXSSC',gcAct_Comp)
lcMjrMsk  = gfItemMask("PM")
lcMjrHdr  = gfItemMask("HM")
lcNMjrMsk = gfItemMask("PN")
lcItmMsk  = gfItemMask("PI")

lnAlias = SELECT()
DECLARE laMfgRFld[7,2],laItemSeg[1],laReq[8]
STORE '' TO lcContCode,lcContName,lcOperSeq,llInHouse,llMfgOpr,lnLeadTime,lcMfgGlAcnt
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

=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lcClrMsk = laItemSeg[lnCount,3]
    lnClrPos = laItemSeg[lnCount,4]
  ENDIF
  IF laItemSeg[lnCount,1]='S'
    lnSizePos = laItemSeg[lnCount,4]
  ENDIF
ENDFOR
lnLastSeq = 0

lcItem = &lcPoLine..STYLE
lcColor = ''

STORE 0 TO lnEst1,lnEst2,lnEst3,lnEst4,lnEst5,lnFEst1,lnFEst2,lnFEst3,lnFEst4,lnFEst5
lcItemFile = IIF(lcTranType='T','Fabric','Style')
llDyelot   = SEEK(lcItem+ALLTRIM(lcColor),lcItemFile) .AND. &lcItemFile..cDye_Flg='Y'
lcScale    = IIF(lcTranType='T','',Style.Scale)

SET ORDER TO TAG Ctktbom  IN (lcTktSheet)
SET ORDER TO TAG BomLine  IN (lcDetFile)

lcTmpBomSh = gfTempName()
lcTmpSheet = 'BOM'

SELECT &lcTmpSheet
=AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcTmpBomSh) FROM ARRAY laFileStru
INDEX ON typ+item+iclr+citmmajor+citmmask TAG (lcTmpBomSh)

SELECT &lcPoLine
LOCATE 

PRIVATE llLooped
llLooped = .F.

SCAN FOR !&lcPoLine..lRefCost

  lcTranType = 'I'
  lcTicketNo = laData[1]
  lcLinkCode = laData[20]
  lnLineNo = &lcPoLine..LINENO
  lnPrice = &lcPoLine..nCost1
  lcItem = &lcPoLine..STYLE
  lcScale = &lcPoLine..SCALE
  lcDyelot = ''
  lcStyWare = ''
  lcMatWare = ''
  lcColor = ''
  
  IF !SEEK(IIF(lcTranType='T',lcItem,SUBSTR(lcItem,1,LEN(lcMjrMsk))),lcTmpSheet)
    LOOP
  ENDIF
  llLooped = .T.
  
  SELECT &lcCostElem
  ZAP
  SELECT &lcTmpBomSh
  ZAP
  
  SELECT (lcTmpSheet)
  =SEEK(PADR(IIF(lcTranType='T',lcItem,SUBSTR(lcItem,1,LEN(lcMjrMsk))),19))
  SCAN REST WHILE cItmMajor = PADR(IIF(lcTranType='T',lcItem,SUBSTR(lcItem,1,LEN(lcMjrMsk))),19)
    SCATTER MEMVAR MEMO
    IF cCatGTyp='S' .AND. M_USEEXSSC .AND. !EMPTY(MSZCROSREF) .AND. ;
       SUBSTR(ALLTRIM(Item),lnSizePos) = STRTRAN(SUBSTR(lcItmMsk,lnSizePos),'X','*')
      FOR lnCount = 1 TO MEMLINES(MSZCROSREF)
        lcLine = MLINE(MSZCROSREF,lnCount)
        IF SUBSTR(lcLine,1,3) <> lcScale
          LOOP
        ENDIF
        m.Item = PADR(SUBSTR(m.Item,1,lnSizePos-1)+SUBSTR(lcLine,AT('~',lcLine)+1,3),19)
        IF !SEEK(m.typ+m.item+m.iclr+m.citmmajor+m.citmmask,lcTmpBomSh)
          INSERT INTO (lcTmpBomSh) FROM MEMVAR
        ENDIF
      ENDFOR
    ELSE
      INSERT INTO (lcTmpBomSh) FROM MEMVAR
    ENDIF
  ENDSCAN
  
  IF USED('STYLE')
    SELECT STYLE
    lcStyFIlt = SET('FILTER')
    SET FILTER TO
  ENDIF
  
  lcGrade = ''
  IF lcTranType <> 'T'
    =SEEK(lcItem,'Style')
    lcGrade = Style.cStyGrade
  ELSE
    =SEEK(lcItem+lcColor,'Fabric')
    lcGrade = Fabric.cFabGrade
  ENDIF
  
  IF lcTranType = 'I' OR lcTranType = 'D'
    *--Update Bom cost elements depend on cost in P/O.
    *B608541,1 WAM 05/06/2008 Don't call function to recalculate cost base on POSLN lines (T20080409.0029)
    *B608541,1 WAM 05/06/2008 Calling the function is commented out
    *=lfUpdPOBom()
    *B608541,1 WAM 05/06/2008 (End)

  ENDIF
  
  *-- Flag to check if the message already appeares dont display it again.
  
  SELECT (lcTmpBomSh)
  SCAN
    *--MAN Added IF EMPTY(cCatGTyp) .OR. EMPTY(Typ)
    IF EMPTY(cCatGTyp) .OR. EMPTY(Typ)
      LOOP
    ENDIF
    
    IF !LIKE(STRTRAN(IIF(lcTranType='T',LEFT(cItmMask,6),cItmMask),'*','?'),IIF(lcTranType='T',lcColor,lcItem)) .OR. ;
       (!EMPTY(MSIZES) .AND. ATCLINE(lcScale+'~',MSIZES)=0) .OR. ;
       (!EMPTY(MSZCROSREF) .AND. ATCLINE(lcScale+',',MSZCROSREF)=0)     
      LOOP
    ENDIF
    lcItemType = Typ
    IF EMPTY(MSIZES)
      lnTranQty = laQty[9]
      lcSizes   = '1,2,3,4,5,6,7,8'
    ELSE
      lcSizes = SUBSTR(MLINE(MSIZES,ATCLINE(lcScale+'~',MSIZES)),5)
      lnTranQty = 0
      FOR lnCount = 1 TO 8
        lnTranQty = lnTranQty + IIF(STR(lnCount,1) $ lcSizes,laQty[lnCount],0)
      ENDFOR
    ENDIF
    *- if no qty is found at least give it the value 1 to calculate cost per peice
    lnTranQty = IIF(lnTranQty = 0 , 1 , lnTranQty)
    
    STORE '' TO lcCstClr,lcCstItm,lcCmSizes
    lcCstItmDye = SPACE(10)
    DO CASE
      CASE cCatGTyp='S'
        lcCmSizes='12345678'
        FOR lnCount = 1 TO 8
          laReq[lnCount]=laQty[lnCount]
        ENDFOR
        lcCstItm=''
        FOR lnCount = 1 TO LEN(ITEM)
          IF SUBSTR(ITEM,lnCount,1)='*'
            lcCstItm = lcCstItm + SUBSTR(lcItem,lnCount,1)
          ELSE
            lcCstItm = lcCstItm + SUBSTR(ITEM,lnCount,1)
          ENDIF
        ENDFOR
        IF !SEEK(lcCstItm,'Style')
          LOOP
        ENDIF
        lcCsItmSc = Style.Scale
        =SEEK(lcItem,'Style')
        IF !EMPTY(MSZCROSREF)
          lcCmSizes = ''
          STORE 0 TO laReq
          FOR lnCount = 1 TO MEMLINES(MSZCROSREF)
            lcLine = MLINE(MSZCROSREF,lnCount)
            IF SUBSTR(lcLine,1,3) = lcScale .AND. SUBSTR(lcLine,7,3)=lcCsItmSc
              laReq[VAL(SUBSTR(lcLine,11,1))] = laReq[VAL(SUBSTR(lcLine,11,1))]+;
              laQty[VAL(SUBSTR(lcLine,5,1))]
              lcCmSizes = lcCmSizes + SUBSTR(lcLine,11,1)
            ENDIF
          ENDFOR
        ENDIF
  
      CASE INLIST(cCatGTyp,'F','T')
        lcCstItm = SUBSTR(Item,1,7)
        IF IClr = '******'
          lcCstClr = IIF(lcTranType='T',lcColor,SUBSTR(lcItem,lnClrPos,LEN(lcClrMsk)))
        ELSE
          lcCstClr = IClr
        ENDIF
        IF (cCatGTyp='F' OR Trim_Invt) AND !SEEK(lcCstItm+lcCstClr,'FABRIC')
          LOOP
        ENDIF
  
        
      CASE cCatGTyp='M'
        IF lcTranType = 'I'
          lcExSign = gfGetExSin(@lcUntSin,lcDutyCur)
        ENDIF  
      CASE cCatGTyp='P'
        lcExSign = gfGetExSin(@lcUntSin,lcPriceCur)
      CASE !INLIST(cCatGTyp,'S','F','T') AND lcTranType = 'I'
        lcExSign = gfGetExSin(@lcUntSin,lcDutyCur)
    ENDCASE
    SELECT (lcTktSheet)
    IF !SEEK(lcTranLett+lcTicketNo+&lcTmpBomSh..Typ+PADR(lcCstItm,19)+;
       PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode+lcCstItmDye)
      APPEND BLANK
      REPLACE CutTkt    WITH lcTicketNo    ,;
              cIMTyp    WITH lcTranLett    ,;
              cCatGTyp  WITH &lcTmpBomSh..cCatGTyp  ,;
              TRIM_INVT WITH &lcTmpBomSh..TRIM_INVT ,;
              cWareCode WITH IIF(cCatGTyp='S',lcStyWare,IIF(cCatGTyp='F' OR ;
                             (cCatGTyp='T' AND TRIM_INVT),lcMatWare,'')) ,;
              Link_Code WITH lcLinkCode            ,;
              cOprCode  WITH &lcTmpBomSh..cOprCode ,;
              TYP       WITH &lcTmpBomSh..Typ      ,;
              ITEM      WITH lcCstItm              ,;
              ICLR      WITH lcCstClr              ,;
              MfgCode   WITH &lcTmpBomSh..MfgCode  ,;
              cOprCode  WITH &lcTmpBomSh..cOprCode ,;
              DESC      WITH &lcTmpBomSh..Desc     ,;
              Dyelot    WITH lcCstItmDye           ,;
              UOM       WITH &lcTmpBomSh..UOM      ,;
              DATE      WITH gdSysDate             ,;
              WIDTH     WITH IIF(cCatGTyp='F' OR (cCatGTyp='T' AND TRIM_INVT),FABRIC.WIDTH,'') 
      IF lcTranType = 'M'
        REPLACE CMARKER WITH &lcTmpBomSh..cMarker
      ENDIF
      *- refresh the relation
      GO RECNO()
    ENDIF
  
    SELECT (lcTktSheet)
    *- Update pieces only when get a new cost element
    IF SEEK('A'+&lcTmpBomSh..Typ+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode+;
                    lcCstItmDye,lcCostElem)
      SELECT (lcCostElem)
      LOCATE REST WHILE cTrType+cType+CItem+CIclr+cMFGCode+cDyelot = 'A'+&lcTmpBomSh..Typ+;
                        PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode+lcCstItmDye;
                    FOR cSizes = STRTRAN(lcSizes,',','')
      IF !FOUND()
        SELECT (lcTktSheet)
        REPLACE Pieces  WITH Pieces  + lnTranQty
        INSERT INTO (lcCostElem) (cTrType,cType,cItem,cIClr,cMFGCode,cDyelot,cSizes);
                             VALUES ('A',&lcTmpBomSh..Typ,PADR(lcCstItm,19),PADR(lcCstClr,6),;
                                         &lcTmpBomSh..MfgCode,lcCstItmDye,STRTRAN(lcSizes,',',''))
      ENDIF
    ELSE
      REPLACE Pieces  WITH Pieces  + lnTranQty
      INSERT INTO (lcCostElem) (cTrType,cType,cItem,cIClr,cMFGCode,cDyelot,cSizes);
                           VALUES ('A',&lcTmpBomSh..Typ,PADR(lcCstItm,19),PADR(lcCstClr,6),;
                                       &lcTmpBomSh..MfgCode,lcCstItmDye,STRTRAN(lcSizes,',',''))
    ENDIF
    SELECT (lcTktSheet)
    REPLACE Req_Qty WITH Req_Qty + lnTranQty * &lcTmpBomSh..nBomTotQty ,;
            UntQty  WITH IIF(Pieces<>0,Req_Qty/Pieces,0)
    lnUntQty = IIF(lnTranQty=0,UNTQTY,EVALUATE(lcTmpBomSh+'.nBomTotQty'))
    *B606496,1 AMH [End]
    
    *--If tranQty was zero take it from pieces.
    *--to make sure that the cost was updated.
    *--Read lnTotCost and lnFTotCost depend on lnTTranQty insted of lnTranQty.
    lnTTranQty = lnTranQty
    IF lnTranQty = 0
      lnTTranQty = Pieces
    ENDIF 
    DO CASE
      CASE cCatGTyp = 'P'
        lnTotCost  = lnPrice * lnTTranQty &lcExSign lnPriceRate &lcUntSin lnPriceUnit
        lnFTotCost = lnPrice * lnTTranQty 
      CASE &lcTmpBomSh..nPercent > 0
        lnTotCost  = lnPrice*lnTTranQty*(&lcTmpBomSh..nPercent/100) &lcExSign lnPriceRate &lcUntSin lnPriceUnit
        lnFTotCost = lnPrice*lnTTranQty*(&lcTmpBomSh..nPercent/100)
      CASE cCatGTyp = 'S'
        lnTotCost  = Style.TotCost * lnTTranQty * &lcTktSheet..UntQty        
        lnFTotCost = lnTotCost
      CASE cCatGTyp = 'F' OR (cCatGTyp='T' AND TRIM_INVT)
        lnTotCost  = Fabric.CostBuy/Fabric.Conv * lnTTranQty * lnUntQty      
        lnFTotCost = lnTotCost
      OTHERWISE
        lnTotCost  = &lcTmpBomSh..TotCost * lnTTranQty &lcExSign lnDutyRate &lcUntSin lnDutyUnit
        lnFTotCost = &lcTmpBomSh..TotCost * lnTTranQty
    ENDCASE
    IF lnTranQty=0
      REPLACE Est_Cost WITH IIF(Est_Cost=0,lnTotCost,Est_Cost)
    ELSE
      REPLACE Est_Cost WITH Est_Cost + lnTotCost
    ENDIF
    REPLACE UntCost  WITH IIF(Req_Qty<>0,Est_Cost/Req_Qty,0)
    
    lnEst&lcItemType = lnEst&lcItemType  + lnTotCost
    lnFEst&lcItemType= lnFEst&lcItemType + lnFTotCost
    IF &lcTmpBomSh..cCatGTyp = 'S'
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        REPLACE REQ_QTY&lcCount WITH REQ_QTY&lcCount+laReq[lnCount]*&lcTktSheet..UntQty
      ENDFOR
    ENDIF
    REPLACE cCompSizes WITH lcCmSizes 
    
    SELECT (lcTmpBomSh)
    DO CASE
      CASE &lcTmpBomSh..CCATGTYP = 'P'
        lnUnitPri = lnPrice
      CASE INLIST(&lcTmpBomSh..CCATGTYP,'M','D')
  
        IF &lcTmpBomSh..nPercent > 0
          lnUnitPri  = lnPrice*(&lcTmpBomSh..nPercent/100)
        ELSE
          lnUnitPri = &lcTmpBomSh..UntCost
        ENDIF  
      CASE INLIST(&lcTmpBomSh..CCATGTYP,'F','T') .AND. SEEK(SUBSTR(lcCstItm,1,7)+lcCstClr,'Fabric')
        lnUnitPri = Fabric.CostBuy/Fabric.Conv
      OTHERWISE
        lnUnitPri = &lcTktSheet..UntCost
    ENDCASE
    SELECT (lcDetFile)
    =SEEK(lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
          PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode)
    LOCATE REST WHILE ;
    cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
    lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
    PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode ;
    FOR cSizes = STRTRAN(lcSizes,',','')
    IF !FOUND()
      *- Update Cost item dyelot in the BOMLINE file
      APPEND BLANK
      REPLACE cIMTyp     WITH lcTranLett     ,;
              cTktNo     WITH lcTicketNo     ,;
              LineNo     WITH lnLineNo       ,;
              cStyGrade  WITH lcGrade        ,;
              Style      WITH lcItem         ,;
              SClr       WITH lcColor        ,;
              cBomTyp    WITH &lcTmpBomSh..Typ,;
              cType      WITH '1'            ,;
              cCatGTyp   WITH &lcTmpBomSh..cCatGTyp   ,;
              cOprCode   WITH &lcTmpBomSh..cOprCode   ,;
              UnitQty    WITH &lcTmpBomSh..nBomTotQty ,;
              UnitCost   WITH lnUnitPri      ,;
              Item       WITH lcCstItm       ,;
              IClr       WITH lcCstClr       ,;
              MfgCode    WITH &lcTmpBomSh..MfgCode    ,;
              Dyelot     WITH lcCstItmDye ,;
              cSizes     WITH STRTRAN(lcSizes,',','') ,;
              cCompSizes WITH lcCmSizes
      REPLACE cCostStat WITH &lcTmpBomSh..cCostStat,;
              nPercent   WITH &lcTmpBomSh..nPercent
    ENDIF
    
    IF lnTranQty = 0
       REPLACE ItemAmt  WITH ItemQty*lnUnitPri ,;
               UnitCost WITH IIF(ItemQty=0,0,ItemAmt/ItemQty)            
    ELSE
      
      IF SEEK('B'+&lcDetFile..cBomTyp+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcDetFile..MfgCode+;
                   lcCstItmDye,lcCostElem)
        SELECT (lcCostElem)
        LOCATE REST WHILE cTrType+cType+CItem+CIclr+cMFGCode+cDyelot = 'B'+&lcDetFile..cBomTyp+;
                          PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcDetFile..MfgCode+lcCstItmDye;
                      FOR cSizes = STRTRAN(lcSizes,',','')
        IF !FOUND()
          SELECT (lcDetFile)
          REPLACE StyQty   WITH StyQty  + lnTranQty
          INSERT INTO (lcCostElem) (cTrType,cType,cItem,cIClr,cMFGCode,cDyelot,cSizes);
                            VALUES ('B',&lcDetFile..cBomTyp,PADR(lcCstItm,19),PADR(lcCstClr,6),;
                                        &lcDetFile..MfgCode,lcCstItmDye,STRTRAN(lcSizes,',',''))
        ENDIF
      ELSE
        REPLACE StyQty   WITH StyQty  + lnTranQty
        INSERT INTO (lcCostElem) (cTrType,cType,cItem,cIClr,cMFGCode,cDyelot,cSizes);
                          VALUES ('B',&lcDetFile..cBomTyp,PADR(lcCstItm,19),PADR(lcCstClr,6),;
                                      &lcDetFile..MfgCode,lcCstItmDye,STRTRAN(lcSizes,',',''))
      ENDIF
      SELECT (lcDetFile)
      REPLACE ItemQty  WITH ItemQty + lnTranQty* &lcTmpBomSh..nBomTotQty  ,;
              UnitQty  WITH IIF(StyQty=0,0,ItemQty/StyQty) ,;
              ItemAmt  WITH ItemAmt + lnTranQty * &lcTmpBomSh..nBomTotQty * lnUnitPri ,;
              UnitCost WITH IIF(ItemQty=0,0,ItemAmt/ItemQty)
      *B606496,1 AMH [End]
             
    ENDIF
  
  ENDSCAN
  
  IF USED('STYLE')
    SELECT STYLE
    SET FILTER TO &lcStyFIlt
  ENDIF
  *:*************************************************************************** up to here is copied from the gfSheetItem global function with slight modifications
  
  *:*************************************************************************** From here down is added by Tarek
  lcimtyp = lcTranType
  lctktno = laData[1]
  
  PRIVATE lnAlias,lnPoRecNo
  lnAlias = SELECT()
  
  lcLinOrd = ORDER(lcDetFile)
  SET ORDER TO TAG BomLine  IN (lcDetFile)
  
  lcStyle = &lcPoLine..STYLE
  PRIVATE lcLineNo
  lcLineNo = STR(lnLineNo,6) 
   
  SELECT (lcDetFile)
  =SEEK(lcimtyp+'1'+lctktno+lcLineNo)
  SUM REST ItemAmt TO lnDutyAmt WHILE ;
  cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=lcimtyp+'1'+lctktno+lcLineNo ;
  FOR cCostStat = '1' AND Style = lcStyle
    
  SELECT (lcDetFile)
  =SEEK(lcimtyp+'1'+lctktno+lcLineNo)
  SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode = ;
                    lcimtyp+'1'+lctktno+lcLineNo ;
              FOR   cCatgTyp = 'D' AND Style = lcStyle
    IF nPercent > 0
        REPLACE ItemAmt  WITH nPercent*lnDutyAmt/100 ,;
                UnitCost WITH IIF(ItemQTy=0,0,ItemAmt/ItemQTy)
    ENDIF
  ENDSCAN
  
  SELECT (lcDetFile)
  SET ORDER TO &lcLinOrd
  LOCATE
  
  *- index on the type to get cost summation
  INDEX ON CBOMTYP TAG CBOMTYP
  LOCATE
  PRIVATE lcI
  STORE 0 TO m.nCost2,m.nCost3,m.nCost4,m.nCost5
  SCAN FOR CBOMTYP $ '2345'
    lcI = CBOMTYP
    m.nCost&lcI = m.nCost&lcI + ITEMAMT
  ENDSCAN
  
  *- Get the cost per one peice
  m.nCost2 = m.nCost2/lnTranQty
  m.nCost3 = m.nCost3/lnTranQty
  m.nCost4 = m.nCost4/lnTranQty
  m.nCost5 = m.nCost5/lnTranQty
  
  SELECT &lcPoLine
  REPLACE lRefCost WITH .T.      ;
          nCost2   WITH m.nCost2 ;
          nCost3   WITH m.nCost3 ;
          nCost4   WITH m.nCost4 ;
          nCost5   WITH m.nCost5 
  
  IF !llUpdAll         
    REPLACE nECost2 WITH lfvEquCost('2',nCost2,laData[29],lnCurrUnt2)
    REPLACE nECost3 WITH lfvEquCost('3',nCost3,laData[29],lnCurrUnt2)
    REPLACE nECost4 WITH lfvEquCost('4',nCost4,laData[29],lnCurrUnt2)
    REPLACE nECost5 WITH lfvEquCost('5',nCost5,laData[29],lnCurrUnt2)        
  ENDIF          

  SELECT &lcDetFile
  ZAP
  SELECT &lcTktSheet
  ZAP
  
ENDSCAN

SELECT &lcPoLine
IF BETWEEN(lnRecno,1,RECCOUNT(lcPoLine))
  GOTO (lnRecno)
  IF llLooped
    SCATTER FIELDS nCost2,nCost3,nCost4,nCost5 MEMVAR
    SHOW GET m.nCost2
    SHOW GET m.nCost3
    SHOW GET m.nCost4
    SHOW GET m.nCost5 
  ENDIF
ENDIF

USE IN &lcTktSheet
ERASE (gcWorkDir+lcTktSheet+'.DBF')
ERASE (gcWorkDir+lcTktSheet+'.CDX')
ERASE (gcWorkDir+lcTktSheet+'.FPT')

USE IN &lcDetFile
ERASE (gcWorkDir+lcDetFile+'.DBF')
ERASE (gcWorkDir+lcDetFile+'.CDX')
ERASE (gcWorkDir+lcDetFile+'.FPT')

USE IN (lcTmpBomSh)
ERASE (gcWorkDir+lcTmpBomSh+'.DBF')
ERASE (gcWorkDir+lcTmpBomSh+'.CDX')
ERASE (gcWorkDir+lcTmpBomSh+'.FPT')

USE IN (lcCostElem)

SELECT STYLE
SET RELATION OFF INTO SCALE

SELECT (lnSlct)
*-- end of lfCstShUpd.