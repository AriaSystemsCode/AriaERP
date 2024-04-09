*:************************************************************************
*: Program file  : MFRCALL.PRG
*: Program desc. : Program To Receive :
*:               :    1- Temp Receiving PO
*:               :    2- Temp Receiving CT
*:               :    3- Create & Issue Inter Location Transfer
*:               :    4- Receive Inter Location Transfer
*: For screen    : MFRCALL/0/1/2/3 .Scx/Sct
*:         System: ARIA BUSINESS SYSTEM
*:         Module: - Manufacturing
*:               : - Style Purchase Order
*:               : - Point of Sale
*:      Developer: Reham Al-Allamy
*:************************************************************************
*: Documented *C101496,1 
*:************************************************************************
*: Calls         : Screens : MFRCALL.Spx
*:*************************************************************
*: Passed Parameters  : lcTranTyp -> 
*:                      "M" -> Temp. Receiving Cutting Ticket
*:                      "I" -> Temp. Receiving Style Purchase Order
*:                      "C" -> Create & Issue Inter Location Transfer
*:                      "R" -> Receiving Inter Location Transfer
*:*************************************************************
*: Example            :
*:  DO gpDoProg WITH 'AWRMFRCALL',.F.,'MF',"P"
*:*************************************************************
*:  Modifications :
*E301220,4 Reham On 05/12/1999
*E301220,4 Add shipment # in the screen to give the ability to scan
*E301220,4 temp. receiving by shipment.
*B602923,1 Reham On 05/23/1999
*B602923,1 1- Use rush more to optimize the speed upon saving.
*B602923,1 2- Give the ability to close the screen in select mode without trapping in location.
*B602923,1 3- Fix error of "Subscript out of bounds" upon saving.
*B602923,1 4- Prevent the user to see the cost if he has no access if Pcs Out.
*B602949,1 Reham On 05/27/1999
*B602949,1 Display the size description in the browse.
*E301264,4 Reham On 06/20/1999
*E301264,4 Prevent assigning the styles to the source location.
*E301279,1 Reham On 07/06/1999
*E301279,1 1- Allow styles with zero prices according to the point of sale 
*E301279,1    setup "Accept items with 0 price".
*E301279,1 2- Allow styles with zero stock according to the point of sale 
*E301279,1    setup "Allow zero inventory".
*E301280,1 Reham On 07/06/1999
*E301280,1 Do not ask the user to assign styles to the target location, 
*E301280,1 assign immediately if current module is point of sale.
*E301283,1 Reham On 07/07/1999
*E301283,1 Receive only the peices in in pcs in.
*B802802,1 MAN 11/17/1999 Fix Alias "" Not found when the system is not setup
*B802802,1                to use bins
*C101704,1 SAM 01/17/2000 Allow alpha numeric PO# manualy
*B803149,1 SSH 26/03/00 Fix the incorrect spelling for word 'Receipts'
*E500331,1 ABD 06/19/2000 Add more feature to menu option.
*B603595,1 ABD 06/25/2000 Fix Messing Word 'remove' in message when remove line.
*C200145,1 ABD 01/08/2001 Add new trigger for custom RSH10 that have 6 chr for
*C200145,1                the UPC number in the Styleupc file.
*B604581,1 SSH 06/24/2002 ADD DIVISION IN THE PCS OUT SCREEN.
*C121114,1 TMI 03/28/2004 Add a trigger for APP10 to ignor entering PO# in "BAR Code Reciving by shipment" screen
*B037997,1 TMI Fix the problem that when record is deleted the pointer go to the top 
*C128583,1 TMI 08/18/2005 Create a batch for PAN21 based on a recieved file with a different format
*:************************************************************************
*
PARAMETER lcTranTyp

*--Cannot use the inter Location P/o if no multi location is used.
IF lcTranTyp $ "C"
 llWareHous = ALLTRIM(gfGetMemVar('M_WareHouse',gcAct_Comp)) = 'Y'
 IF !llWareHous
    *** The system has not been setup to use multiple locations. Cannot proceed. ***
    =gfModalGen('TRM34155B00000','DIALOG')
    glQuitting = .T.
    RETURN
  ENDIF
ENDIF

*B604581,1 SSH 06/24/2002 ADD DIVISION IN THE PCS OUT SCREEN.
IF lcTranTyp="C"
  DECLARE laDivision[1,2],laCodes[1,10]
  lnDivision = 1
  laCodes[1,1] = 'CDIVISION'
  laCodes[1,2] = 'laDivision'
  laCodes[1,3] = 'lnDivision'
  laCodes[1,4] = ''
  laCodes[1,5] = .F.
  laCodes[1,6] = .F.
  laCodes[1,10] = 'cDivision'
ENDIF
*B604581,1 SSH 06/24/2002 ADD DIVISION IN THE PCS OUT SCREEN.

*-- Assign the temporary screens names'.
STORE " " TO lcRcAll0 , lcRcAll1 , lcRcAll2 , lcRcAll3
*E500331,1 ABD define variables to the new temporary screens names.[Begin]
STORE " "  TO lcRcall4 , lcMjrTtl , lcNMjrTl , lcMjrPct ,;
              lcNMjrPt , lcMajor  , lcNonMjr , lcTempRsln,lcStylsDsc
STORE .F. To llSendUPC , llSeltscrn 
llBatExist = .T.
STORE 0 To lnstylelen ,  lncolorlen 
*E500331,1 ABD [End]

*-- Assign the main temprary files names'.
STORE " " TO lcTmpRecv , lcTmpCurs

*-- Assign the temp. files.
STORE " " TO lcPOHdr , lcPOLine , lcPOTran , lcTemLoc , ;
             lcTmpLine , lcGlDist , lcTmpPOLn

*-- Define variables hold the cost types.
STORE "" TO lcIType1 , lcIType2 , lcIType3 , lcIType4 , lcIType5

*-- Get the prompt of the available pictures on the screen.
lcTBrwBmp = gcBmpHome + "ExtKey.BMP"
lcTNewBmp = gcBmpHome + "New1.BMP"
lcTRemBmp = gcBmpHome + "Rem1.BMP"
lcTProBmp = gcBmpHome + "Proceed.BMP"
lcTEdtBmp = gcBmpHome + "Edit3.BMP"

*-- Define arrays.
*-- Array hold separators & its start position & segment lenght.
DECLARE laSepartor[1,3]
*-- Array hold the segmants specifications of the style code structure.
DECLARE laStySeg[1,1]
*-- Array hold the available warehoses.
DECLARE laWare[1,1]
laWare = ""

*E301279,1 Reham On 07/06/1999   *** Begin ***
*E301279,1 Add 2 rows for "Allow styles with 0 prices" & "Allow styles with 0 inventory" setups.
*-- Array hold the setup parameters.
*DECLARE laCrSetups[IIF(lcTranTyp $ "MI" , 10 , 11) , 2]
DECLARE laCrSetups[IIF(lcTranTyp $ "MI" , 10 , 14) , 2] 
*E301279,1 Reham On 07/06/1999   *** End   ***

*-- Arrays to calculate the open qty. in case of receive inter location transfer.
DECLARE laOpn[9]
STORE 0 TO laOpn

*-- Force to local Save procedure  (lpSavScr)
laDefProc[9]  = .F.

*-- Define variables hold needed parameters from setup file.
llWareHous = .F.             && Single warehouse or multi warehouse.
llWareLoc  = .F.             && Warehouse location.
llStyDye   = .F.             && System use Style dyelot or not.
llLoc      = .F.             && If Multiple location or not.
llConfirm  = .F.             && If set confirmation is on or off.
lcStyUPC   = "S"             && If system works by style or UPC.
lcSysType  = ""              && System type if Point of sale.

*E301279,1 Reham On 07/06/1999   *** Begin ***
*E301279,1 Flag hold point of sale setup "Accept items with 0 price".
llAcpZroPc = .F.
*E301279,1 Flag hold point of sale setup "Allow zero inventory".
llAcpZrStk = .F.
lcCostMth  = ' '
*E301279,1 Reham On 07/06/1999   *** End   ***

lcCnfrm    = ""              && Variable hold the confirmation setting.

*-- Define the main variables in all transactions tyes.
llCollect  = .F.             && Flag to define start processing or not.
rbMethod   = 2               && Radio button (Import file , Scan line by line)
puItem     = 1               && Popup (Style , Material)
rbScanBy   = 1               && Radio button Scan by (Style , UPC)
lcItem     = "\<Style      " && Prompt of the item in item radio button (Style , Material)
lcCollect  = IIF(rbMethod = 1 , "\<Collect" , "S\<tart    ")     && Prompt of the collect & Start button.
lcImpFile  = ""              && Variable hold the file to be imported.
llUPC      = (rbScanBy = 2)  && Flag to know if validate item or UPC.
lnMarker   = 0               && Variable hold the record # in the browse.
llBrowse   = .F.             && Flag to know if press the browse icon or not.
llStyle    = .T.             && Flag to know if validating style or material
lcOldValue = ""              && Variable hold old value of any string object.
lnOldValue = 0               && Variable hold old value of any numeric object.
ldDate     = gdSysDate       && Variable hold the receiving date.
lcDesc     = ""              && Variable hold the header description.
lcShip     = ""              && Variable hold the shipment #.

*-- Flag defines if I'm in add mode or not.
llCUpDate = .F.

*-- Define the variables needed for each line.
rbByLin    = 1               && Radio button on the line level to define UPC or item.
lcRefer    = ""              && Variable hold the reference.
lcWareCode = ""              && Variable hold the warehouse code.
lcDyelot   = ""              && Variable hold the dyelot.
lnWare     = 1
lcStyle    = ""              && Variable hold the style.
lcStyDesc  = ""              && Variable hold the style description.
lcUPC      = ""              && Validate UPC #.
llCrStyDye = .F.             && Flag hold if the current style has dyelot or not.
lnQty      = 0               && Variable hold the style qty.
lnLineCnt  = 0               && Liens counter hold the no. of lines in the browse.
lcLinStat  = "DISABLE"       && Variable hold if the lines are enabled or disabled.

*-- Variables related to create & issue or receive inter location transfer.
lcSiteLoc  = ""              && Variable hold source location.
lcTarget   = ""
lcDropLoc  = ""
llGenOrNum = .F.
lcSession  = ""
llMulCurr  = .F.
llLinkToGl = .F.
lnPrice    = 0
ldOldDate  = {}
lcPO       = ""
llImpCost  = .F.
*C101704,1 [Start] initialize variable to generate PO# manually or automatic
llGenPo = .F.
*C101704,1 [End]
STORE gdSysDate TO ldTrDate , ldRcvDate

*-- Define variable to know if saved inter location transfer or not.
llSav_ILT  = .F.

*-- Define variables hold the charcteristics of the style object.
lcStyHdr   = ""              && Variable hold the style header.
lcStyPic   = ""              && Variable hold the style picture.
lnStyLen   = 1               && Variable hold the style lenght.
lnItmLen   = 1               && Variable hold the style lenght.
lcUPCPic   = "X"+REPLICATE("9",11) && Variable hold the UPC pic.

*-- Define the needed variables if temp. receiving PO or CT.
cbByCrtn   = .F.             && Check box to define by Carton or CT/PO.
lcCartTl   = 'Carton'        && Header hold the carton title on the screen.
lcCarton   = ""              && Variable hold the carton object.
lcTktTl    = IIF(lcTranTyp = "M" , "Cuttkt" ,"P/O # ") && Header hold the PO or CT title.
lcCuttkt   = ""              && Variable hold the CT or PO object.
lcMessage  = IIF(lcTranTyp = "M" , "Cutting ticket" , "Purchase order")

*-- Variables hold the black & invisible color.
lcInvColor = "RGB(192,192,192,192,192,192)"   && Invisable color.
lcNorm2C   = "RGB(0,0,,,,,)"                  && Black Color.

*B602923,1 Reham On 05/23/1999   *** Begin ***
*B602923,1 Check the avilablity if the user can see the cost or not.
llCostPrv  = .F.
*B602923,1 Reham On 05/23/1999   *** End   ***

*-- Master files to be use to validate the styles entered.
lcMasterH = IIF(lcTranTyp = "M" , "CutTktH" , "POSHDR")
lcMasterL = IIF(lcTranTyp = "M" , "CutTktL" , "POSLN")

DO CASE
  *-- If the current program is Temp. receiving C/T.
  CASE lcTranTyp = "M"
    *-- Variable hold the Browse name.
    lcTmpBrow  = "Temporary Receiving Cutting Ticket"
    *-- Variable hold the main base file.
    lcMainFile = "CTKTRCVH"
    *-- Seek expression to seek in the CtTktL file.
    lcSeek     = ""
    *B602949,1 Reham On 05/27/1999   *** Begin ***
    *B602949,1 Display the size description in the browse.
    *-- Variable hold browse fields.
    *lcRcvFld = "cMarker=IIF(RECNO()=lnMarker , '>',' '):H=' ':R:1:W=.F. ,"+;
                "PO_CT:H='C/T # ':R:8 , cCarton:R:H='Carton' ,"+;
                "Style:R:20 , Size:R , cStyDesc:R:20:H='Description' , Dyelot:R , "+;
                "lcReject=IIF(lAccepted,'Accepted','Rejected'):H='Status':R:10 ,"+;
                "nQty:R:H='Qty.':6"

    *E500331,1 ABD - Add valid function to browse. [Begin]
    *lcRcvFld = "cMarker=IIF(RECNO()=lnMarker , '>',' '):H=' ':R:1:W=.F. ,"+;
                "PO_CT:H='C/T # ':R:8 , cCarton:R:H='Carton' ,"+;
                "Style:R:28 , SzDesc:R:9:H='Size' , cStyDesc:R:20:H='Description' , Dyelot:R , "+;
                "lcReject=IIF(lAccepted,'Accepted','Rejected'):H='Status':R:10 ,"+;
                "nQty:R:H='Qty.':6"

    lcRcvFld = "cMarker=IIF(RECNO()=lnMarker , '>',' '):H=' ':R:1:W=lfshplins() ,"+;
                "PO_CT:H='C/T # ':R:8 , cCarton:R:H='Carton' ,"+;
                "Style:R:28 , SzDesc:R:9:H='Size' , cStyDesc:R:20:H='Description' , Dyelot:R , "+;
                "lcReject=IIF(lAccepted,'Accepted','Rejected'):H='Status':R:10 ,"+;
                "nQty:R:H='Qty.':6"

    *E500331,1 ABD - [End]
    *B602949,1 Reham On 05/27/1999   *** End   ***
  *-- If the current program is Temp. receiving P/O.
  CASE lcTranTyp = "I"
    *-- Variable hold the Browse name.
    lcTmpBrow  = "Temporary Receiving Style Purchase Order"
    *-- Variable hold the main base file.
    lcMainFile = "CTKTRCVH"
    *-- Seek expression to seek in the PosLn file.
    lcSeek     = "P"
    *E301220,4 Reham On 05/12/1999   *** Begin ***
    *-- Variable hold browse fields.
    *lcRcvFld = "cMarker=IIF(RECNO()=lnMarker , '>',' '):H=' ':R:1:W=.F. ,"+;
                "PO_CT:H='P/O # ':R:8 , cCarton:R:H='Carton' ,"+;
                "Style:R:20 , Size:R , cStyDesc:R:20:H='Description' , Dyelot:R , "+;
                "lcReject=IIF(lAccepted,'Accepted','Rejected'):H='Status':R:10 ,"+;
                "nQty:R:H='Qty.':6"
    *B602949,1 Reham On 05/27/1999   *** Begin ***
    *B602949,1 Display the size description in the browse.
    *-- Variable hold browse fields.
    *lcRcvFld = "cMarker=IIF(RECNO()=lnMarker , '>',' '):H=' ':R:1:W=.F. ,"+;
                "Ship:H='Ship #':R:8, PO_CT:H='P/O # ':R:8 , cCarton:R:H='Carton' ,"+;
                "Style:R:20 , Size:R , cStyDesc:R:20:H='Description' , Dyelot:R , "+;
                "lcReject=IIF(lAccepted,'Accepted','Rejected'):H='Status':R:10 ,"+;
                "nQty:R:H='Qty.':6"
    *E500331,1 ABD - Add valid function to browse. [Begin]
    *lcRcvFld = "cMarker=IIF(RECNO()=lnMarker , '>',' '):H=' ':R:1:W=.F. ,"+;
                "Ship:H='Ship #':R:8, PO_CT:H='P/O # ':R:8 , cCarton:R:H='Carton' ,"+;
                "Style:R:28 , SzDesc:R:9:H='Size' , cStyDesc:R:20:H='Description' , Dyelot:R , "+;
                "lcReject=IIF(lAccepted,'Accepted','Rejected'):H='Status':R:10 ,"+;
                "nQty:R:H='Qty.':6"
             
    lcRcvFld = "cMarker=IIF(RECNO()=lnMarker , '>',' '):H=' ':R:1:W=lfshplins() ,"+;
                "Ship:H='Ship #':R:8, PO_CT:H='P/O # ':R:8 , cCarton:R:H='Carton' ,"+;
                "Style:R:28 , SzDesc:R:9:H='Size' , cStyDesc:R:20:H='Description' , Dyelot:R , "+;
                "lcReject=IIF(lAccepted,'Accepted','Rejected'):H='Status':R:10 ,"+;
                "nQty:R:H='Qty.':6"
             
    *E500331,1 ABD - [End]
    *E301220,4 Reham On 05/12/1999   *** End   ***
    *B602949,1 Reham On 05/27/1999   *** End   ***

    *C128583,1  TMI [Start] Remove the carton field from the browse fields
    IF ASCAN(laEvntTrig , PADR('ENBTNS',10)) <> 0
      =gfDoTriger('PORECPO',PADR('ENBTNS',10))
    ENDIF
    *C128583,1  TMI [End  ] 

  *-- If the current program is create & issue inter location transfer.
  CASE lcTranTyp = "C"
    *-- Variable hold the Browse name.
    lcTmpBrow  = "Pcs Out"
    *-- Variable hold the main base file.
    lcMainFile = "POSHDR"
    *-- Seek expression to seek in the PosLn file.
    lcSeek     = "N"
    *B602949,1 Reham On 05/27/1999   *** Begin ***
    *B602949,1 Display the size description in the browse.
    *-- Variable hold browse fields.
    *lcRcvFld = "cMarker=IIF(RECNO()=lnMarker , '>',' '):H=' ':R:1:W=.F. ,"+;
                "Style:R:20 , Size:R , cStyDesc:R:20:H='Description' , Dyelot:R , cWareCode:R:H='Location',"+;
                "lcReject=IIF(lAccepted,'Accepted','Rejected'):H='Status':R:10 ,"+;
                "nPrice:R:H='Price':10 , nQty:R:H='Qty.':6"
    lcRcvFld = "cMarker=IIF(RECNO()=lnMarker , '>',' '):H=' ':R:1:W=.F. ,"+;
                "Style:R:28 , SzDesc:R:9:H='Size' , cStyDesc:R:20:H='Description' , Dyelot:R , cWareCode:R:H='Location',"+;
                "lcReject=IIF(lAccepted,'Accepted','Rejected'):H='Status':R:10 ,"+;
                "nPrice:R:H='Price':10 , nQty:R:H='Qty.':6"
    *B602949,1 Reham On 05/27/1999   *** End   ***
  *-- If the current program is receive inter location transfer.
  CASE lcTranTyp = "R"
    *-- Variable hold the Browse name.
    lcTmpBrow  = "Pcs In"
    *-- Variable hold the main base file.
    lcMainFile = "POSHDR"
    *-- Seek expression to seek in the PosLn file.
    lcSeek     = "N"
    *B602949,1 Reham On 05/27/1999   *** Begin ***
    *B602949,1 Display the size description in the browse.
    *-- Variable hold browse fields.
    *lcRcvFld = "cMarker=IIF(RECNO()=lnMarker , '>',' '):H=' ':R:1:W=.F. ,"+;
                "Style:R:20 , Size:R , cStyDesc:R:20:H='Description' , Dyelot:R , cWareCode:R:H='Location',"+;
                "lcReject=IIF(lAccepted,'Accepted','Rejected'):H='Status':R:10 ,"+;
                "nQty:R:H='Qty.':6"
    lcRcvFld = "cMarker=IIF(RECNO()=lnMarker , '>',' '):H=' ':R:1:W=.F. ,"+;
                "Style:R:28 , SzDesc:R:9:H='Size' , cStyDesc:R:20:H='Description' , Dyelot:R , cWareCode:R:H='Location',"+;
                "lcReject=IIF(lAccepted,'Accepted','Rejected'):H='Status':R:10 ,"+;
                "nQty:R:H='Qty.':6"
    *B602949,1 Reham On 05/27/1999   *** End   ***
ENDCASE

*-- Call global function in the main program to do the following : _
*-- Intialise all the variables & open all the files needed in
*-- this session and controling disabling and enabling of the
*-- menu bars and writting the screen names in the window bars.
IF !gfSetup()
  RETURN
ENDIF
*B604581,1 SSH 06/24/2002 ADD DIVISION IN THE PCS OUT SCREEN.
IF lcTranTyp="C"
  =gfwCodePop(@laCodes,'CDIVISION','L')
ENDIF
*B604581,1 SSH 06/24/2002 ADD DIVISION IN THE PCS OUT SCREEN.
*C200145,1 ABD- Add new trigger for custom RSH10 that have 6 chr for
*C200145,1 ABD- The UPC number in the Styleupc file & we will change 
*C200145,1 ABD- The picture of the field & len of seek exepresion. [Begin]
*-- This Array will define after call gfSetup function.
IF ASCAN(laEvntTrig , PADR('LLUPCNUM',10)) <> 0
  lcUPCPic   = "X"+REPLICATE("9",5) && Variable hold the UPC pic.
ENDIF     
*C200145,1 ABD - [End]


DO CASE
  *-- If the current program is Temp. receiving C/T or P/O
  CASE lcTranTyp $ "MI"
    *E301220,4 Reham On 05/13/1999   *** Begin ***
    *E301220,4 Add the shipment # to the saved fields in the temp. receiving header file.
    *-- Variable hold the main screen fields.
    *lcScFields = "Tmprcvnum , cDesc , cStatus , Carton , nTotStk , nTotDam , nTotCan , dDate"
    lcScFields = "Tmprcvnum , cDesc , cStatus , Carton , nTotStk , nTotDam , nTotCan , dDate , ShipNo"
    *E301220,4 Reham On 05/13/1999   *** End   ***
  *-- If the current program is create & issue inter location transfer.
  CASE lcTranTyp $ "C"
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
                'cMultiLot ,cPurCode  ,Buyer,Account,Store  ,cPONo'
  *-- If the current program is receive inter location transfer.
  CASE lcTranTyp = "R"
    *-- Variable hold the main screen fields.
    lcScFields = "cStyType"
ENDCASE

llNoShow  = .F.     && Flag to force the execution of the show procedure.

*-- If entering the screen for the first time.
IF !WEXIST(gcBaseWind)


  *E500331,1 ABD Assign to  the new temporary variables.[Begin]
  lcMjrTtl   = gfItemMask('HM')
  lcNMjrTl   = gfItemMask('HN')
  lcMjrPct   = gfItemMask('PM')
  lcNMjrPt   = gfItemMask('PN')
  lnstylelen = LEN(lcMjrPct)
  lncolorlen = LEN(lcNMjrPt)
  *E500331,1 ABD [End]
  
  *B602923,1 Reham On 05/23/1999   *** Begin ***
  *B602923,1 Check the avilablity if the user can see the cost or not.
  llCostPrv  = gfUserPriv('IC','ICSTYLE','COSTING')
  *B602923,1 Reham On 05/23/1999   *** End   ***
  
  *-- Style header.
  lcStyHdr = gfItemMask('HI')
  
  *-- Style picture + Size Picture.
  lcStyPic = gfItemMask('PI')+'9'
  
  *-- Style picture.
  lnItmLen = LEN(gfItemMask('PI'))
  
  *-- Get the style lenght.
  lnStyLen = LEN(lcStyPic)
  *C101704,1 [Start] get value of setup to generate PO# manually or not 
  llGenPo = (ALLTRIM(gfGetMemVar('M_GenStOrN',gcAct_Comp)) = 'Y')
  *C101704,1 [End]
  *-- Fill an array with the segments strucure, & loop in it to know
  *-- if there is separators or not in the style code strucure.
  =gfItemMask(@laStySeg)
  FOR lnCnt = 1 TO ALEN(laStySeg,1)
    IF !EMPTY(laStySeg[lnCnt,6])
      IF !EMPTY(laSepartor[1,1])
        DIMENSION laSepartor[ALEN(laSepartor,1)+1,3]
      ENDIF
      laSepartor[ALEN(laSepartor,1),1] = laStySeg[lnCnt,6]        && Separator.
      laSepartor[ALEN(laSepartor,1),2] = laStySeg[lnCnt,4]        && Start position of the segment.
      laSepartor[ALEN(laSepartor,1),3] = LEN(laStySeg[lnCnt,3])   && Lenght of the segment.
    ENDIF
  ENDFOR
  
  *E301279,1 Reham On 07/06/1999   *** Begin ***
  *E301279,1 Add 2 rows for "Allow styles with 0 prices" & "Allow styles with 0 inventory" setups.
  *DIMENSION laCrSetups[IIF(lcTranTyp $ "MI" , 10 , 11) , 2]
  *-- Call gfGetMemVar once for all variables.
  DIMENSION laCrSetups[IIF(lcTranTyp $ "MI" , 10 , 14) , 2]
  *E301279,1 Reham On 07/06/1999   *** End   ***
  laCrSetups[1,1] = 'M_WareHouse'
  laCrSetups[2,1] = 'M_WareLoc'
  laCrSetups[3,1] = 'M_Dyelot'
  laCrSetups[4,1] = 'M_CONFIRM'
  laCrSetups[5,1] = 'M_STYLEUPC'
  *-- If create & issue inter location transfer.
  IF lcTranTyp $ "CR"
    laCrSetups[6,1] = 'M_SYSTYPE'
    laCrSetups[7,1] = 'M_DROPWARE'
    laCrSetups[8,1] = 'M_GenStOrN'
    laCrSetups[9,1] = 'llMulCurr'
    laCrSetups[10,1]= 'M_Link_GL'
    laCrSetups[11,1]= 'M_LImpCost'
    *E301279,1 Reham On 07/06/1999   *** Begin ***
    *E301279,1 point of sale setup "Accept items with 0 price".
    laCrSetups[12,1]= 'M_ACPZROPC'
    *E301279,1 point of sale setup "Allow zero inventory".
    laCrSetups[13,1]= 'M_ACPZRSTK'
    laCrSetups[14,1]= 'M_Cost_Meth'
    *E301279,1 Reham On 07/06/1999   *** End   ***
  ELSE
    laCrSetups[6,1] = 'M_cIType1'
    laCrSetups[7,1] = 'M_cIType2'
    laCrSetups[8,1] = 'M_cIType3'
    laCrSetups[9,1] = 'M_cIType4'
    laCrSetups[10,1]= 'M_cIType5'
  ENDIF
  
  =gfGetMemVar(@laCrSetups,gcAct_Comp)
  llWareHous = (laCrSetups[1,2] = 'Y')
  llWareLoc  = (laCrSetups[2,2] = 'Y')
  llStyDye   = (laCrSetups[3,2] = 'Y')
  llConfirm  = (ALLTRIM(laCrSetups[4,2]) = "Y")
  lcStyUPC   = (ALLTRIM(laCrSetups[5,2]))
  rbScanBy   = IIF(OCCURS('PS',gcCmpModules) <> 0 , IIF(lcStyUPC = "U" , 2 , 1) , 2)
  rbByLin    = rbScanBy
  *-- If create & issue inter location transfer.
  IF lcTranTyp $ "CR"
    lcSysType  = laCrSetups[6,2]
    llMulCurr  = laCrSetups[9,2]
    llLinkToGl = (laCrSetups[10,2] = 'Y')
    llImpCost  = laCrSetups[11,2]
    *E301279,1 Reham On 07/06/1999   *** Begin ***
    *E301279,1 point of sale setup "Accept items with 0 price".
    llAcpZroPc = (laCrSetups[12,2] = 'Y')
    *E301279,1 point of sale setup "Allow zero inventory".
    llAcpZrStk = (laCrSetups[13,2] = 'Y')
    lcCostMth  = laCrSetups[14,2]
    *E301279,1 Reham On 07/06/1999   *** End   ***
  ELSE
    lcIType1   = laCrSetups[6,2]
    lcIType2   = laCrSetups[7,2]
    lcIType3   = laCrSetups[8,2]
    lcIType4   = laCrSetups[9,2]
    lcIType5   = laCrSetups[10,2]
  ENDIF
  
  *--Fill warehouse array.
  IF llWareHous  
    SELECT cWareCode+'-'+cDesc FROM Warehous INTO ARRAY laWare
  ELSE
    GO TOP IN WAREHOUS
    laWare[1]  = WAREHOUS.cWareCode+'-'+WAREHOUS.cDesc
    lcWareCode = WAREHOUS.cWareCode
  ENDIF
  
  *-- If create & issue inter location transfer.
  IF lcTranTyp $ "CR" .AND. lcSysType = 'P'
    *-- If the global variable of the site ID is not defined.
    IF EMPTY(gcCurSite)
      *-- Flag to know if the istallation file is open or not.
      llOpenInst = .F.
      *-- Check if the installation program is open or not.
      IF !USED("SYCINST")
        *-- Set flag to true to close the istallation file.
        llOpenInst = .T.
        =gfOpenFile(gcSysHome+'SYCINST','','SH')
      ENDIF
      *-- Get the Site ID field.
      SELECT SYCINST
      lcSiteId = IIF(TYPE('SYCINST.CCURSITEID')#'C',SPACE(6),SYCINST.CCURSITEID)
      *-- If the installation file was closed before, close it.
      IF llOpenInst
        USE IN SYCINST
      ENDIF
    ELSE
      *-- If the global variable of the site ID is defined.
      lcSiteId = gcCurSite
    ENDIF
    *-- Get the location that has the site ID.
    SELECT WAREHOUS
    LOCATE FOR cSiteId = lcSiteId
    IF !FOUND()
      GO TOP
    ENDIF
    *-- Get the current site.
    lcSiteLoc = WAREHOUS.cWareCode
  ENDIF
  
  *-- If create & issue inter location transfer.
  IF lcTranTyp = "CR"
    *-- Get a unique session number, to be used to update the uncomplete 
    *-- session file(UNCSS).
    lcSession  = gfsequence('cSession')
  ENDIF
  
  *-- Assign temporary name for the cursor will hold the styles.
  lcTmpCurs = gfTempName()
  CREATE CURSOR (lcTmpCurs) (Item C(20) , lFoundUPC L(1))
  
  *-- Assign the temporary files names.
  lcTmpRecv = gfTempName()
  *B602949,1 Reham On 05/27/1999   *** Begin ***
  *B602949,1 Add the size description in the temp. file to be displayed in the browse.
  *CREATE DBF (gcWorkDir+lcTmpRecv) (cTranTyp C(1) , PO_CT C(6) , ;
              Ship C(6) , Carton L(1) , cStatus C(1) , cCarton C(3) , ;
              cWareCode C(6) , Dyelot C(10) , Style C(19) , Size C(1) , ;
              cStyDesc C(60) , lAccepted L(1) , nQty N(6) , lByUpc L(1) , ;
              lFoundUPC L(1) , Reference C(30) , nLineNo N(6) , ;
              lCrStyDye L(1) , lAdjustPO L(1) , nPrice N(8,2))
  CREATE DBF (gcWorkDir+lcTmpRecv) (cTranTyp C(1) , PO_CT C(6) , ;
              Ship C(6) , Carton L(1) , cStatus C(1) , cCarton C(3) , ;
              cWareCode C(6) , Dyelot C(10) , Style C(19) , Size C(1) , ;
              cStyDesc C(60) , lAccepted L(1) , nQty N(6) , lByUpc L(1) , ;
              lFoundUPC L(1) , Reference C(30) , nLineNo N(6) , ;
              lCrStyDye L(1) , lAdjustPO L(1) , nPrice N(8,2) , SzDesc C(5))
  *B602949,1 Reham On 05/27/1999   *** End   ***
  *-- If receive inter location transfer, create an index.
  IF lcTranTyp = "R"
    INDEX ON PO_CT + Style + STR(nLineNo,6) TAG (lcTmpRecv)
  ENDIF
  
  *-- If company was setup to keep track of bins.
  IF llWareLoc
    *-- Create temp file for bins.
    DIMENSION laSource[1],laTarget[1]
    =gfOpenFile(gcDataDir+'WhsLoc','WhsLocSt','SH')
    =AFIELDS(laStrc)
    lcTemLoc  = gfTempName()
    CREATE DBF (gcWorkDir+lcTemLoc) FROM ARRAY laStrc
    INDEX ON STYLE+CWARECODE+CLOCATION TAG (lcTemLoc)
  ENDIF
  
  lcTmpLine = gfTempName()
  IF lcTranTyp = "C"
    lcPOHdr   = gfTempName()
    lcPOLine  = gfTempName()
    lcPOTran  = gfTempName()
  ENDIF
  IF lcTranTyp = "I"
    lcTmpPOLn = gfTempName()
  ENDIF
  
  *-- If issue or receive inter location transfer.
  IF lcTranTyp $ "CR" .AND. llLinkToGl
    lcGlDist = gfTempName()
  ENDIF
  *-- Call local function to create the temp. files needed for each program.
  =lfTempFile()
  
  *-- Assign the temporary window names.
  lcRcAll0  = gfTempName()
  lcRcAll1  = gfTempName()
  lcRcAll2  = gfTempName()
  lcRcAll3  = gfTempName()

  *E500331,1 ABD Assign the new temporary window names. [Begin]
  lcRcall4   = gfTempName() && New Screen.
  lcTempRsln = gfTempName() && New file hold total rec.
  lcAgnStyle = gfTempName()  && Open Style file again with onther name.

  USE (gcDataDir+'STYLE')  AGAIN ALIAS (lcAgnStyle) ORDER TAG STYLE IN 0  

  *-- Create new table for 
  CREATE DBF (gcWorkDir+lcTempRsln) (STYLE C(19) , cStyDesc C(60), nPoqty1 N(6)               ,;
              nPoqty2   N(6), nPoqty3    N(6), nPoqty4   N(6), nPoqty5   N(6), nPoqty6   N(6) ,;
              nPoqty7   N(6), nPoqty8    N(6), nPototqty N(8), nRecqty1  N(6), nRecqty2  N(6) ,;
              nRecqty3  N(6), nRecqty4   N(6), nRecqty5  N(6), nRecqty6  N(6), nRecqty7  N(6) ,;
              nRecqty8  N(6), nRecTotqty N(6), nBalaqty1 N(6), nBalaqty2 N(6), nBalaqty3 N(6) ,;
              nBalaqty4 N(6), nBalaqty5  N(6), nBalaqty6 N(6), nBalaqty7 N(6),;
              nBalaqty8 N(6), nBalTotqty N(6))
              
  SELECT (lcTempRsln)
  INDEX ON STYLE TAG  (lcTempRsln) OF (gcWorkDir+lcTempRsln+'.CDX')

  *E500331,1 ABD [End]
ENDIF

*B602923,1 Reham On 05/23/1999   *** Begin ***
*B602923,1 Hide the cost from the inter location trasfer if the user has no right to see it.
*-- If the current program is create & issue inter location transfer.
IF lcTranTyp = "C"
  *B602949,1 Reham On 05/27/1999   *** Begin ***
  *B602949,1 Display the size description in the browse.
  *-- Variable hold browse fields.
  *lcRcvFld = "cMarker=IIF(RECNO()=lnMarker , '>',' '):H=' ':R:1:W=.F. ,"+;
              "Style:R:20 , Size:R , cStyDesc:R:20:H='Description' , Dyelot:R , cWareCode:R:H='Location',"+;
              "lcReject=IIF(lAccepted,'Accepted','Rejected'):H='Status':R:10 ,"+;
              IIF(llCostPrv , "nPrice:R:H='Price':10 ," , "")+;
              "nQty:R:H='Qty.':6"
  lcRcvFld = "cMarker=IIF(RECNO()=lnMarker , '>',' '):H=' ':R:1:W=.F. ,"+;
              "Style:R:28 , SzDesc:R:9:H='Size' , cStyDesc:R:20:H='Description' , Dyelot:R , cWareCode:R:H='Location',"+;
              "lcReject=IIF(lAccepted,'Accepted','Rejected'):H='Status':R:10 ,"+;
              IIF(llCostPrv , "nPrice:R:H='Price':10 ," , "")+;
              "nQty:R:H='Qty.':6"
  *B602949,1 Reham On 05/27/1999   *** End   ***
ENDIF
*B602923,1 Reham On 05/23/1999   *** End   ***

*-- Set filter to the suitable styles.
SELECT STYLE
IF lcTranTyp = "M"
  SET FILTER TO Make 
ELSE
  SET FILTER TO !Make 
ENDIF  

*-- Save the confirmation setting.
lcSetConf  = SET('CONFIRM')

*-- Restore if the lines are enabled or disabled.
lcLinStat = IIF(lnLineCnt > 0 .AND. &lcTmpRecv..lAccepted , "ENABLE" , "DISABLE")

SELECT (lcMainFile)

PUSH KEY
PUSH MENU _MSYSMENU

*-- Define bar to activate the browse.
DEFINE BAR 100 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 101 OF P01PU01 PROMPT lcTmpBrow KEY ALT+B
*-- Activate the browse when selecting its bar.
ON SELECTION BAR 101 OF P01PU01 ACTIVATE WINDOW (lcTmpBrow)

*-- Call the screen.
DO (gcScrDir+"MFRCALL.SPX")

*-- Release the browse window.
RELEASE WINDOW (lcTmpBrow)

POP MENU _MSYSMENU
POP KEY

*-- Release the style filter.
SELECT STYLE
SET FILTER TO

*-- Restore confirmation setting.
SET CONFIRM &lcSetConf

*-- If quitting the program erase the temp. files.
IF glQuitting
  DO CASE
    CASE lcTranTyp $ "MI"
      IF USED(lcTmpLine)
        USE IN (lcTmpLine)
      ENDIF
      ERASE (gcWorkDir+lcTmpLine+'.DBF')
      ERASE (gcWorkDir+lcTmpLine+'.CDX')
      IF lcTranTyp = "I"
        IF USED(lcTmpPOLn)
          USE IN (lcTmpPOLn)
        ENDIF
        ERASE (gcWorkDir+lcTmpPOLn+'.DBF')
        ERASE (gcWorkDir+lcTmpPOLn+'.CDX')
      ENDIF
    CASE lcTranTyp = "C"
      IF USED(lcPoHdr)
        USE IN (lcPOHdr)
      ENDIF
      ERASE (gcWorkDir+lcPOHdr+'.DBF')
      ERASE (gcWorkDir+lcPOHdr+'.CDX')
      ERASE (gcWorkDir+lcPOHdr+'.FPT')
      IF USED(lcPOLine)
        USE IN (lcPOLine)
      ENDIF
      ERASE (gcWorkDir+lcPOLine+'.DBF')
      ERASE (gcWorkDir+lcPOLine+'.CDX')
      IF USED(lcPoTran)
        USE IN (lcPOTran)
      ENDIF
      ERASE (gcWorkDir+lcPOTran+'.DBF')  
      ERASE (gcWorkDir+lcPOTran+'.CDX')
  ENDCASE
  
  IF llWareLoc
    IF USED(lcTemLoc)
      USE IN (lcTemLoc)
    ENDIF
    ERASE (gcWorkDir+lcTemLoc+'.DBF')
    ERASE (gcWorkDir+lcTemLoc+'.CDX')
  ENDIF
  
  IF USED(lcTmpRecv)
    USE IN (lcTmpRecv)
  ENDIF
  ERASE &gcWorkDir.&lcTmpRecv..DBF
  ERASE &gcWorkDir.&lcTmpRecv..CDX
  
  *E500331,1 Erase temp file. [Begin]
  IF USED(lcTempRsln)
    USE IN (lcTempRsln)
  ENDIF
  ERASE &gcWorkDir.&lcTempRsln..DBF
  ERASE &gcWorkDir.&lcTempRsln..CDX
  
  *C121114,3  TMI [Start] Ask if variable "lcAgnStyle" is defined
  *IF USED(lcAgnStyle)
  IF TYPE('lcAgnStyle')='C' .AND. USED(lcAgnStyle)
    *C121114,3  TMI [End  ] 
    USE IN (lcAgnStyle)
  ENDIF
  *E500331,1 ABD  [End]
   
  *-- Close the cursor.
  USE IN (lcTmpCurs)
ENDIF

*E500331,1 ABD Relase pade from menu. [Begin]
RELEASE PAD _INQUIRY OF _MSYSMENU
*E500331,1 ABD [End]

*!*************************************************************
*! Name      : lpShow
*! Developer : Reham Al-Allamy
*! Date      : 04/13/1999
*! Purpose   : Show procedure
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lpShow()
*!*************************************************************
*
FUNCTION lpShow

*-- To disable the edit bar in the record pad in the system menu.
laCtrStat[10] = 'DISABLE'

*-- Disable the browse button on the control pannel all the time.
SHOW GET pbBrws DISABLE

*-- Enable the browse invisable button.
SHOW GET ibTab ENABLE
SHOW GET lnDivision ENABLE
DO CASE
  CASE laScrMode[1]
    *-- Define variable to know if saved inter location transfer or not.
    llSav_ILT  = .F.
    
    *-- Blank the temp. files.
    SELECT (lcTmpCurs)
    ZAP
    SELECT (lcTmpRecv)
    ZAP
    
    *E500331,1 ABD -  Blank the temp. file. [Begin]
    SELECT (lcTempRsln)
    ZAP
    *-- ABD Desiable Menu option after saving .[Begin]
    SET SKIP OF PAD _INQUIRY OF _MSYSMENU .T.
    *E500331,1 ABD -  [End]
    
    
    *B802802,1 Added the condition to Zap the Bin file only when the system setup to use bins
    IF llWareLoc 
      SELECT (lcTemLoc)
      ZAP
    ENDIF  
    
    *-- Call local function to create the temp. files related to each program.
    =lfTempFile()
    
    *-- Blank the needed variables.
    lcItem     = "\<Style      " && Prompt of the item in item radio button (Style , Material)
    STORE 2   TO rbMethod 
    *C128583,1  TMI [Start] use the same trigger to select the method of collecting stryles from a text file for panach
    *C121114,3  TMI [Start] Default the screen of "BAR Code Reciving by shipment" to recive by Style    
    IF ASCAN(laEvntTrig , PADR('SLMETHOD',10)) <> 0
      =gfDoTriger('PORECPO',PADR('SLMETHOD',10))
    ENDIF
    *C121114,3  TMI [End  ] 
    lcCollect  = IIF(rbMethod = 1 , "\<Collect" , "S\<tart    ")   && Prompt of the collect & Start button.
    llUPC      = (rbScanBy = 2)  && Flag to know if validate item or UPC.
    *rbScanBy   = IIF(OCCURS('PS',gcCmpModules) <> 0 , IIF(lcStyUPC = "U" , 2 , 1) , 2)
    rbByLin    = rbScanBy
    ldDate     = gdSysDate       && Variable hold the receiving date.
    lcLinStat  = "DISABLE"       && Variable hold if the lines are enabled or disabled.
    llStyle    = .T.
    STORE .F. TO llCollect , llBrowse , llCrStyDye , cbByCrtn
    STORE 1   TO puItem , lnWare
    STORE 0   TO lnMarker , lnOldValue , lnQty , lnLineCnt , lnPrice
    STORE ""  TO lcImpFile , lcOldValue , lcDesc , lcShip , lcWareCode , ;
                 lcDyelot , lcStyle , lcStyDesc , lcUPC , lcRefer , ;
                 lcCarton , lcCuttkt , lcPO
    *-- Flag defines if I'm in add mode or not.
    llCUpDate = .F.
    *-- Blank the needed variables for Create & issue inter location transfer.
    lcTarget = ""
    STORE gdSysdate TO ldTrDate , ldRcvDate
    SHOW GET ldTrDate ENABLE
    SHOW GET ldRcvDate  ENABLE
    IF lcTranTyp = "C" .AND. lcSysType = 'P' 
      IF !EMPTY(lcSiteLoc)
        SHOW GET pbSiteLoc DISABLE
        SHOW GET lcSiteLoc DISABLE
      ELSE
        SHOW GET pbSiteLoc ENABLE
        SHOW GET lcSiteLoc ENABLE
      ENDIF
    ELSE
      SHOW GET pbSiteLoc ENABLE
      SHOW GET lcSiteLoc ENABLE
    ENDIF
    
    SHOW GET pbTarget ENABLE
    SHOW GET lcTarget ENABLE
    SHOW GET ibPO     ENABLE
    SHOW GET lcPO     ENABLE
    
    =lfwMainBrw()
    *-- Refresh the objects in the first window "lcRcAll0"
    SHOW GET cbByCrtn  ENABLE
    SHOW GETS WINDOW (lcRcAll3) DISABLE ONLY
    SHOW GET ibTab    ENABLE
    SHOW GET ldDate   DISABLE
    SHOW GET lcDesc   DISABLE
    ACTIVATE WINDOW (lcRcAll0)
    ACTIVATE WINDOW (lcRcAll1)
  
  CASE laScrMode[4]
    SHOW GET ldTrDate  DISABLE
    SHOW GET ldRcvDate DISABLE
    SHOW GET pbSiteLoc DISABLE
    SHOW GET lcSiteLoc DISABLE
    SHOW GET pbTarget  DISABLE
    SHOW GET lcTarget  DISABLE
    SHOW GET ibPO      DISABLE
    SHOW GET lcPO      DISABLE
    
    SHOW GET cbByCrtn DISABLE
    IF rbByLin = 1
      *-- Activate the style object to add new line.
      SHOW GET lcUPC   DISABLE
      SHOW GET lcStyle ENABLE
      _CUROBJ   = OBJNUM(lcStyle)
    ELSE
      *-- Activate the UPC object to add new line.
      SHOW GET lcStyle DISABLE
      SHOW GET lcUPC   ENABLE
      _CUROBJ   = OBJNUM(lcUPC)
    ENDIF
    *E500331,1 ABD Cheak if window style summary on top. [Begin]
    IF !llSeltscrn
      SHOW GET ldDate   ENABLE
      SHOW GET lcDesc   ENABLE
    ENDIF
    *E500331,1 ABD [End]
ENDCASE

*-- Refresh the objects in the first window "lcRcAll0"
SHOW GET rbMethod  ENABLE
*SHOW GET puItem    ENABLE
SHOW GET puItem    DISABLE
SHOW GET rbScanBy  ENABLE
*B604581,1 SSH 06/24/2002 ADD DIVISION IN THE PCS OUT SCREEN.
IF lcTranTyp="C"
  SHOW GET lnDivision ENABLE
ENDIF
*B604581,1 SSH 06/24/2002 ADD DIVISION IN THE PCS OUT SCREEN.
IF rbMethod = 2
  lcImpFile = ""
  SHOW GET pbGetFil  DISABLE
  SHOW GET lcImpFile DISABLE
ELSE
  SHOW GET pbGetFil  ENABLE
  SHOW GET lcImpFile ENABLE
ENDIF

*-- Refresh the objects in the second window "lcRcAll1"
SHOW GET pbCollect,1 PROMPT lcCollect ENABLE
SHOW GET pbNewCt  DISABLE

IF cbByCrtn
  *-- Enable carton object & disable CT or PO.
  SHOW GET lcCarton  ENABLE
  SHOW GET lcCuttkt  DISABLE
  SHOW GET ibtkt     DISABLE
ELSE
  *-- Disable carton object & enable CT or PO.
  *E500331,1 ABD Cheak if window style summary on top. [Begin]
  IF !llSeltscrn
    SHOW GET lcCarton  DISABLE
    SHOW GET lcCuttkt  ENABLE
    SHOW GET ibtkt     ENABLE
  ENDIF  
ENDIF

*C121114,3  TMI [Start] Disable PO# field
IF ASCAN(laEvntTrig , PADR('DISBL',10)) <> 0
  =gfDoTriger('PORECPO',PADR('DISBL',10))
ENDIF
*C121114,3  TMI [End  ] 

IF !llCollect
  *-- If not start to collect items, enable all the objects in the 1st window.
  *-- Refresh window "MfRcAll0" objects.
  SHOW GET rbMethod  ENABLE
  IF rbMethod = 2
    lcImpFile = ""
    SHOW GET pbGetFil  DISABLE
    SHOW GET lcImpFile DISABLE
  ELSE
    SHOW GET pbGetFil  ENABLE
    SHOW GET lcImpFile ENABLE
  ENDIF
  *SHOW GET puItem    ENABLE
  SHOW GET puItem    DISABLE
  SHOW GET rbScanBy  ENABLE
  *B604581,1 SSH 06/24/2002 ADD DIVISION IN THE PCS OUT SCREEN.
  IF lcTranTyp="C"
    SHOW GET lnDivision ENABLE
  ENDIF
  *B604581,1 SSH 06/24/2002 ADD DIVISION IN THE PCS OUT SCREEN.
  *-- Refresh window "MfRcAll1" objects.
  SHOW GET pbNewCt   DISABLE
  SHOW GET pbCollect ENABLE
  
  *E301220,4 Reham On 05/13/1999   *** Begin ***
  *E301220,4 Give the ability to scan temp. receiving by shipment.
  IF cbByCrtn
    SHOW GET lcShip    DISABLE
    SHOW GET ibShip    DISABLE
  ELSE
  *E500331,1 ABD Cheak if window style summary on top. [Begin]
  IF !llSeltscrn
    SHOW GET lcShip    ENABLE
    SHOW GET ibShip    ENABLE
  ENDIF  
  *E500331,1 ABD [END]      
  ENDIF
  *E301220,4 Reham On 05/13/1999   *** End   ***
  
  *-- Refresh window "MfRcAll2" objects.
  SHOW GET rbByLin   DISABLE
  *-- Point to the right object.
  IF lcTranTyp $ "MI"
    *E301220,4 Reham On 05/13/1999   *** Begin ***
    *E301220,4 Give the ability to scan temp. receiving by shipment.
    *_CUROBJ = IIF(cbByCrtn , OBJNUM(lcCarton) , OBJNUM(lcCutTkt))
    _CUROBJ = IIF(cbByCrtn , OBJNUM(lcCarton) , IIF(lcTranTyp = "I" , OBJNUM(lcShip) , OBJNUM(lcCutTkt)))
    *E301220,4 Reham On 05/13/1999   *** End   ***
  ELSE
    _CUROBJ = IIF(lcTranTyp = "C" , IIF(lcSysType="P" , OBJNUM(lcTarget) , OBJNUM(lcSiteLoc)) , OBJNUM(lcPo))
  ENDIF
ELSE
  *-- If start to collect items, disable all the objects in the 1st window.
  *-- Refresh window "MfRcAll0" objects.
  SHOW GET rbMethod  DISABLE
  SHOW GET pbGetFil  DISABLE
  SHOW GET lcImpFile DISABLE
  SHOW GET puItem    DISABLE
  SHOW GET rbScanBy  DISABLE
  *B604581,1 SSH 06/24/2002 ADD DIVISION IN THE PCS OUT SCREEN.
  IF lcTranTyp="C"
    SHOW GET lnDivision DISABLE
  ENDIF
  *B604581,1 SSH 06/24/2002 ADD DIVISION IN THE PCS OUT SCREEN.
  
  *-- Refresh window "MfRcAll1" objects.
  *E500331,1 ABD Cheak if window style summary on top. [Begin]
  IF !llSeltscrn
    SHOW GET pbNewCt   ENABLE
  ENDIF
  *E500331,1 ABD [END]
  SHOW GET pbCollect DISABLE
  
  *-- Refresh window "MfRcAll2" objects.
  *E500331,1 ABD Cheak if window style summary on top. [Begin]
  IF !llSeltscrn
    SHOW GET rbByLin   ENABLE
  ENDIF 
  *E500331,1 ABD [END]
  *E301220,4 Reham On 05/13/1999   *** Begin ***
  *E301220,4 Give the ability to scan temp. receiving by shipment.
  SHOW GET lcShip    DISABLE
  SHOW GET ibShip    DISABLE
  *E301220,4 Reham On 05/13/1999   *** End   ***
  
  *-- Point to the push button new object.
  IF lcTranTyp $ "MI"
    _CUROBJ = OBJNUM(pbNewCt)
  ENDIF
ENDIF

SELECT (lcMainFile)

*C128583,1  TMI [Start] enable disable needed buttons 
IF ASCAN(laEvntTrig , PADR('ENBTNS',10)) <> 0
  =gfDoTriger('PORECPO',PADR('ENBTNS',10))
ENDIF
*C128583,1  TMI [End  ] 

*!*************************************************************
*! Name      : lfwMainBrw
*! Developer : Reham Al-Allamy
*! Date      : 04/13/1999
*! Purpose   : Function call the browse called from main screen
*!           : Setup.
*!*************************************************************
*! Calls     : lfWhenBrow, lfVldBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfwMainBrw()
*!*************************************************************
*
FUNCTION lfwMainBrw

*-- Display the transactions lines in the browse from the temp. files
SELECT (lcTmpRecv)
lnMarker = RECNO()

BROWSE FIELDS &lcRcvFld ;
       WINDOW  (lcRcAll2) ;
       IN WINDOW (gcBaseWind) ;
       NOMENU           ;
       NOEDIT           ;
       NOAPPEND         ;
       NODELETE         ;
       NOWAIT           ;
       SAVE             ;
	   NOCLEAR          ;
       WHEN lfWhenBrow()  ;
       VALID :F lfVldBrow() ;
       TITLE lcTmpBrow

*!*************************************************************
*! Name      : lfVldBrow
*! Developer : Reham Al-Allamy
*! Date      : 04/13/1999
*! Purpose   : Valid function for the browse.
*!*************************************************************
*! Calls     : gfStopBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfVldBrow()
*!*************************************************************
*
FUNCTION lfVldBrow

*-- Valid function executed inside the browse. to stop the browse if the 
*-- current window not the browse window.

IF !WONTOP(lcTmpBrow)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfWhenBrow
*! Developer : Reham Al-Allamy
*! Date      : 04/13/1999
*! Purpose   : When function for the browse.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfWhenBrow()
*!*************************************************************
*
FUNCTION lfWhenBrow

*-- Refresh the objects in the screen if any movement happened in the browse.
SELECT (lcTmpRecv)
*-- Change the record pointer.
lnMarker   = RECNO()
glFromBrow = .T.

IF BOF() .OR. EOF() .OR. lnLineCnt = 0
  lcLinStat = "DISABLE"
  SHOW GET pbRemLn   DISABLE
ELSE
  lcCutTkt   = PO_CT
  lcCarton   = cCarton
  *E301220,4 Reham On 05/12/1999   *** Begin ***
  lcShip     = Ship
  *E301220,4 Reham On 05/12/1999   *** End   ***
  *rbByLin    = IIF(&lcTmpRecv..lByUpc , 2 , 1)
  lcWareCode = &lcTmpRecv..cWareCode
  lcDyelot   = &lcTmpRecv..Dyelot
  lnQty      = &lcTmpRecv..nQty
  lnPrice    = &lcTmpRecv..nPrice
  lcStyDesc  = &lcTmpRecv..cStyDesc
  lcRefer    = &lcTmpRecv..Reference
  lnWare     = IIF(llWareHous , IIF(ASCAN(laWare,lcWareCode,1)>0 , ASCAN(laWare,lcWareCode,1) , 1) , 1)
  llCrStyDye = &lcTmpRecv..lCrStyDye
  lcLinStat  = IIF(&lcTmpRecv..lAccepted , "ENABLE" , "DISABLE")
  SHOW GET pbRemLn   ENABLE
ENDIF

*E301220,4 Reham On 05/12/1999   *** Begin ***
SHOW GET lcShip
*E301220,4 Reham On 05/12/1999   *** End   ***

*E500331,1 ABD Enable and disable buttoms. [Begin]
IF !llSeltscrn
  SHOW GET lcCutTkt
  SHOW GET lcCarton
  SHOW GET ldDate
  SHOW GET lcDesc
ENDIF
*-- Refresh the objects in window MfRcAll3.

IF llCollect
  SHOW GET pbNewLn   ENABLE
  SHOW GET rbByLin   ENABLE
ELSE
  SHOW GET pbNewLn   DISABLE
  SHOW GET rbByLin   DISABLE
ENDIF

IF rbByLin = 1
  *-- Activate the style object to add new line.
  SHOW GET lcUPC   DISABLE
  SHOW GET lcStyle ENABLE
  _CUROBJ   = OBJNUM(lcStyle)
ELSE
  *-- Activate the UPC object to add new line.
  SHOW GET lcStyle DISABLE
  SHOW GET lcUPC   ENABLE
  _CUROBJ   = OBJNUM(lcUPC)
ENDIF

IF llCrStyDye
  SHOW GET lcDyelot  &lcLinStat
ELSE
  SHOW GET lcDyelot  DISABLE
ENDIF
SHOW GET lcStyDesc &lcLinStat
SHOW GET lnQty     &lcLinStat
SHOW GET lnPrice   &lcLinStat
SHOW GET lnWare    &lcLinStat
SHOW GET llLoc     &lcLinStat
SHOW GET lcRefer   &lcLinStat

*C128583,1  TMI [Start] Disable new line for PAN21
IF ASCAN(laEvntTrig , PADR('ENBTNS',10)) <> 0
  =gfDoTriger('PORECPO',PADR('ENBTNS',10))
ENDIF
*C128583,1  TMI [End  ] 

*-- Call function in the screen spr to refresh the say fields 
*-- in the main screen
=lfRefresh()

SHOW WINDOW (lcTmpBrow) REFRESH SAME

*!*************************************************************
*! Name      : lfActMain
*! Developer : Reham Al-Allamy
*! Date      : 04/13/1999
*! Purpose   : Activate function for the main screen
*!*************************************************************
*! Calls     : gfStopBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfActMain()
*!*************************************************************
*
FUNCTION lfActMain

*-- If coming from the browse, Call global function to stop the browse.
IF glFromBrow
  = gfStopBrow()
  *-- Set the browse flag to false.
  glFromBrow = .F.
ENDIF

*-- If the screen browse is not active then clear the trapped keys.
IF !INLIST(WONTOP(), lcTmpBrow)
  *-- Clear all the trapped keys.
  ON KEY LABEL CTRL+Q
  ON KEY LABEL CTRL+W
  ON KEY LABEL Ctrl+ENTER
  ON KEY LABEL Ctrl+HOME
  ON KEY LABEL Ctrl+END
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
ENDIF

*!*************************************************************
*! Name      : lfDactMain
*! Developer : Reham Al-allamy
*! Date      : 04/13/1999
*! Purpose   : Deactivate function for the main screen
*!*************************************************************
*! Calls     : lpTab , lpShTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfDactMain()
*!*************************************************************
*
FUNCTION lfDactMain

*-- Set the global flag "glFromBrow" to true only the screen browse is active.
glFromBrow = INLIST(WONTOP() , lcTmpBrow)

*-- If any of the screen's browses is active then trap the 
*-- Tab, ShiftTab, Ctrl+Enter, Ctrl+Home and Ctrl+End keys.
IF glFromBrow
  ON KEY LABEL CTRL+Q     lnDummy = 1
  ON KEY LABEL CTRL+W     lnDummy = 1
  ON KEY LABEL Ctrl+HOME  lnDummy = 1
  ON KEY LABEL Ctrl+END   lnDummy = 1
  ON KEY LABEL Ctrl+ENTER lnDummy = 1
  ON KEY LABEL TAB     DO lpTab
  ON KEY LABEL BACKTAB DO lpShTab
ENDIF

*!*************************************************************
*! Name      : lpTab
*! Developer : Reham Al-Allamy
*! Date      : 04/13/1999
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : lpTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lpTab
*!*************************************************************
*
PROCEDURE lpTab
*-- Clear the TAB trap.
ON KEY LABEL TAB

DO CASE
  CASE laScrMode[1]
    ACTIVATE WINDOW gwcContrl1
    _CUROBJ = OBJNUM(pbcptask)
  CASE laScrMode[4]
    ACTIVATE WINDOW (lcRcAll3)
    
    IF rbByLin = 1
      _CUROBJ = OBJNUM(lcStyle)      
    ELSE
      _CUROBJ = OBJNUM(lcUPC)
    ENDIF
ENDCASE

*-- Trap the TAB key.
ON KEY LABEL TAB DO lpTab

*!*************************************************************
*! Name      : lpShTab
*! Developer : Reham Al-Allamy
*! Date      : 04/13/1999
*! Purpose   : Trap of backtab key.
*!*************************************************************
*! Calls     : lpShTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lpShTab
*!*************************************************************
*
PROCEDURE lpShTab
*-- Clear the backtab key.
ON KEY LABEL BACKTAB

DO CASE
  CASE laScrMode[1]
    ACTIVATE WINDOW (lcRcAll1)
    _CUROBJ = OBJNUM(pbCollect)
  CASE laScrMode[4]
    ACTIVATE WINDOW (lcRcAll1)
    IF lcTranTyp $ "MI"
      _CUROBJ = OBJNUM(pbNewCt)
    ELSE
      _CUROBJ = IIF(lcTranTyp = "C" , IIF(lcSysType="P" , OBJNUM(lcTarget) , OBJNUM(lcSiteLoc)) , OBJNUM(lcPO))
    ENDIF
ENDCASE

*-- Trap the BACKTAB key.
ON KEY LABEL BACKTAB DO lpShTab

*!*************************************************************
*! Name      : lfvMethod
*! Developer : Reham Al-Allamy
*! Date      : 04/13/1999
*! Purpose   : Valid function for method radio button:
*!           : (Import file , Scan line by line)
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvMethod()
*!*************************************************************
*
FUNCTION lfvMethod

lcCollect = IIF(rbMethod = 1 , "\<Collect" , "S\<tart    ")

IF rbMethod = 2
  lcImpFile = ""
  SHOW GET pbGetFil  DISABLE
  SHOW GET lcImpFile DISABLE
ELSE
  SHOW GET pbGetFil  ENABLE
  SHOW GET lcImpFile ENABLE
ENDIF
SHOW GET pbCollect,1 PROMPT lcCollect

*!*************************************************************
*! Name      : lfvGetFil
*! Developer : Reham Al-Allamy
*! Date      : 04/13/1999
*! Purpose   : Valid function for Push button : <Get File>
*!           : Valid if select Import file method.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvGetFil()
*!*************************************************************
*
FUNCTION lfvGetFil
*C128583,1  TMI [Start] 
IF ASCAN(laEvntTrig,'VALIDIR')<>0
  =gfDoTriger('PORECPO','VALIDIR')
  RETURN 
ENDIF
*C128583,1  TMI [End  ] 
*-- Get the text file hold the styles.
lcImpFile = GETFILE('TXT', 'Select File', 'Select',1)
*-- Refresh the imported file variable.
SHOW GET lcImpFile

*!*************************************************************
*! Name      : lfvItem
*! Developer : Reham Al-Allamy
*! Date      : 04/13/1999
*! Purpose   : Valid function for Item Popup : (Style or Material)
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvItem()
*!*************************************************************
*
FUNCTION lfvItem

*-- Define the select button prompt according to the current record status.
lcItem  = IIF(puItem = 1 , "\<Style      " , "\<Material")

*-- Flag to determine if validating style or material.
llStyle = IIF(puItem = 1 , .T. , .F.)

*-- Refresh the select button with the right prompt.
SHOW GET rbScanBy,1 PROMPT lcItem

*!*************************************************************
*! Name      : lfvScanBy
*! Developer : Reham Al-Allamy
*! Date      : 04/13/1999
*! Purpose   : Valid function for radio button : (Style/Material or UPC)
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvScanBy()
*!*************************************************************
*
FUNCTION lfvScanBy

*-- Flag to know if validate item or UPC
llUPC   = (rbScanBy = 2)

*-- Refresh the related objects on the lines data window.
rbByLin = rbScanBy
SHOW GET rbByLin
=lfRefresh(lcRcAll3)

*!*************************************************************
*! Name      : lfcOldVal
*! Developer : Reham Al-Allamy
*! Date      : 04/18/1999
*! Purpose   : When function for all string obejcts
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfcOldVal()
*!*************************************************************
*
FUNCTION lfcOldVal

lcOldValue = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfnOldVal
*! Developer : Reham Al-Allamy
*! Date      : 04/18/1999
*! Purpose   : When function for all nymeric obejcts
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfnOldVal()
*!*************************************************************
*
FUNCTION lfnOldVal

lnOldValue = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvByCrtn
*! Developer : Reham Al-Allamy
*! Date      : 04/13/1999
*! Purpose   : Valid function for check box [ ]By Carton.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvByCrtn()
*!*************************************************************
*
FUNCTION lfvByCrtn

*-- Refresh the screen hold the carton objects.
=lfRefresh(lcRcAll1)

IF cbByCrtn
  *-- Enable carton object & disable CT or PO.
  SHOW GET lcCarton  ENABLE
  SHOW GET lcCuttkt  DISABLE
  SHOW GET ibtkt     DISABLE
  *E301220,1 Reham On 05/13/1999   *** Begin ***
  *E301220,1 Give the ability to scan by shipment.
  SHOW GET lcShip    DISABLE
  SHOW GET ibShip    DISABLE
  *E301220,1 Reham On 05/13/1999   *** End   ***
ELSE
  *-- Disable carton object & enable CT or PO.
  SHOW GET lcCarton  DISABLE
  SHOW GET lcCuttkt  ENABLE
  SHOW GET ibtkt     ENABLE
  *E301220,1 Reham On 05/13/1999   *** Begin ***
  *E301220,1 Give the ability to scan by shipment.
  SHOW GET lcShip    ENABLE
  SHOW GET ibShip    ENABLE
  *E301220,1 Reham On 05/13/1999   *** End   ***
ENDIF

*C121114,3  TMI [Start] Disable PO# field
IF ASCAN(laEvntTrig , PADR('DISBL',10)) <> 0
  =gfDoTriger('PORECPO',PADR('DISBL',10))
ENDIF
*C121114,3  TMI [End  ] 

*!*************************************************************
*! Name      : lfvCarton
*! Developer : Reham Al-Allamy
*! Date      : 04/13/1999
*! Purpose   : Valid function for Carton object
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvCarton()
*!*************************************************************
*
FUNCTION lfvCarton

*-- Return if the carton field is empty.
IF EMPTY(lcCarton)
  RETURN
ELSE
  *-- If there is line in the browse, update it with the new carton
  IF laScrMode[4] .AND. llCollect .AND. lnLineCnt > 0
    *-- Update the Qty. object.
    SELECT (lcTmpRecv)
    REPLACE cCarton WITH lcCarton
    SHOW WINDOW (lcTmpBrow) REFRESH
  ENDIF
ENDIF
*-- Disable the check box [ ]By Carton.
SHOW GET cbByCrtn DISABLE
*-- Enable CT/PO object.
SHOW GET lcCuttkt  ENABLE
SHOW GET ibtkt     ENABLE

*E301220,4 Reham on 05/12/1999   *** Begin ***
SHOW GET lcShip    ENABLE
SHOW GET ibShip    ENABLE
*E301220,4 Reham on 05/12/1999   *** End   ***

*!*************************************************************
*! Name      : lfvShip
*! Developer : Reham Al-Allamy
*! Date      : 05/12/1999
*! Purpose   : Valid function for Ship # object.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvShip()
*!*************************************************************
*
FUNCTION lfvShip
PRIVATE lcCurAlias

*-- Save the current alias.
lcCurAlias = ALIAS()

llBrowse = (llBrowse .OR. '?' $ lcShip)
IF llBrowse .OR. !MDOWN()
  SELECT ShpmtHdr
  IF llBrowse .OR. (!EMPTY(lcShip) AND !SEEK(lcShip))
    llBrowse = .F.
    *-- If the shipment # entered not found.
    lcTmpFld   = lcBrFields           && Save browse fields.
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

    DIME laTempData[1]
    STORE '' TO laTempData
    =gfBrows(.F.,'ShipNo','laTempData','Shipments')
    lcShip=laTempData[1]
    IF EMPTY(lcShip)
      lcShip = SPACE(6)
      _CUROBJ  = OBJNUM(lcShip)
      SELECT (lcCurAlias)
      RETURN
    ELSE
      *--Shipment validation.
      IF SHPMTHDR.Status = 'C'
        *--This shipment has been received complete! unable to proceed.
        = gfModalGen('TRM34078B00000','DIALOG','received complete')
        lcShip   = SPACE(6)
         _CUROBJ = OBJNUM(lcShip)
        SELECT (lcCurAlias)
        RETURN
      ENDIF
      IF SHPMTHDR.Status = 'X'
        *--This shipment has been canceled! unable to proceed.
        = gfModalGen('TRM34078B00000','DIALOG','canceled')
        lcShip  = SPACE(6)
        _CUROBJ = OBJNUM(lcShip)
        SELECT (lcCurAlias)
        RETURN
      ENDIF
      SELECT POSLN
      SET ORDER TO TAG POSLNSH
      IF !SEEK(lcShip)
        *--The shipment lines have not been found! unable to proceed.
        = gfModalGen('TRM34077B42000','DIALOG')
        lcShip  = SPACE(6)
        _CUROBJ = OBJNUM(lcShip)
        SET ORDER TO TAG POSLN IN POSLN
        SELECT (lcCurAlias)
        RETURN
      ENDIF
      SET ORDER TO TAG POSLN IN POSLN
    ENDIF
  ENDIF
  SHOW GET lcShpCode
  *C128583,1  TMI [Start] if a shipment is selected then disable the PO field
  IF ASCAN(laEvntTrig,'SHIPSLCT')<>0
    =gfDoTriger('PORECPO','SHIPSLCT')
  ENDIF
  *C128583,1  TMI [End  ] 
ENDIF

SELECT (lcCurAlias)

*!*************************************************************
*! Name      : lfvTkt
*! Developer : Reham Al-Allamy
*! Date      : 04/13/1999
*! Purpose   : Valid function for C/T or P/O object
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvTkt()
*!*************************************************************
*
FUNCTION lfvTkt
PRIVATE lcCurAlias

*-- Save the current alias.
lcCurAlias = ALIAS()

llBrowse = (llBrowse .OR. '?' $ lcCuttkt)
IF llBrowse .OR. !MDOWN()
  SELECT (lcMasterH)
  IF llBrowse .OR. (!EMPTY(lcCuttkt) AND !SEEK(lcSeek+lcCuttkt))
    llBrowse = .F.
    *-- If seek for CutTkt #.
    IF lcTranTyp = 'M'
      =CutBrow(@lcCuttkt,'')
    ELSE
      *-- If seek for PO #.
      DO POSBrow WITH lcCuttkt,'','P'
    ENDIF
    IF EMPTY(lcCuttkt)
      lcCuttkt = SPACE(6)
      _CUROBJ  = OBJNUM(lcCuttkt)
      SELECT (lcCurAlias)
      RETURN
    ENDIF
  ENDIF
  
  *-- Check the validity of the selected CutTkt or PO.
  IF !EMPTY(lcCuttkt)
    IF !lfChkCtkt(lcCuttkt)
      lcCuttkt = SPACE(6)
      _CUROBJ  = OBJNUM(lcCuttkt)
      SELECT (lcCurAlias)
      RETURN
    ELSE
      *RAM
      
    ENDIF 
  ENDIF
ENDIF

*E500331,1 add the po lines in the temp file  [Begin]
IF !EMPTY(lcCuttkt) .AND. lcOldValue <> lcCuttkt 
  SELECT (lcMasterL)
  =SEEK(lcSeek+lcCuttkt))
  STORE 0 To lnTotoPos
  SCAN REST FOR IIF(lcTranTyp = "M" , Cuttkt+Style+Dyelot+Trancd =  lcCutTkt ,;
                    cStyType + Po = lcSeek + lcCuttkt .AND. trancd = '1')
    IF SEEK(&lcMasterL..Style,lcTempRsLn)
      SELECT (lcTempRsln)
      REPLACE nPoqty1    WITH nPoqty1    + &lcMasterL..QTY1  ,;
              nPoqty2    WITH nPoqty2    + &lcMasterL..QTY2  ,;
              nPoqty3    WITH nPoqty3    + &lcMasterL..QTY3  ,;
              nPoqty4    WITH nPoqty4    + &lcMasterL..QTY4  ,;
              nPoqty5    WITH nPoqty5    + &lcMasterL..QTY5  ,;
              nPoqty6    WITH nPoqty6    + &lcMasterL..QTY6  ,;
              nPoqty7    WITH nPoqty7    + &lcMasterL..QTY7  ,;
              nPoqty8    WITH nPoqty8    + &lcMasterL..QTY8  ,;
              nPoTotqty  WITH nPoTotqty  + &lcMasterL..TotQTY,;
              nBalaqty1  WITH nBalaqty1  + &lcMasterL..QTY1  ,;
              nBalaqty2  WITH nBalaqty2  + &lcMasterL..QTY2  ,;
              nBalaqty3  WITH nBalaqty3  + &lcMasterL..QTY3  ,;
              nBalaqty4  WITH nBalaqty4  + &lcMasterL..QTY4  ,;
              nBalaqty5  WITH nBalaqty5  + &lcMasterL..QTY5  ,;
              nBalaqty6  WITH nBalaqty6  + &lcMasterL..QTY6  ,;
              nBalaqty7  WITH nBalaqty7  + &lcMasterL..QTY7  ,;
              nBalaqty8  WITH nBalaqty8  + &lcMasterL..QTY8  ,;
              nBalTotqty WITH nBalTotqty + &lcMasterL..TotQTY
      SELECT (lcMasterL)       
    ELSE
      INSERT INTO &lcTempRsln (STYLE, nPoqty1, nPoqty2, nPoqty3, nPoqty4, nPoqty5,;
                               nPoqty6, nPoqty7, nPoqty8, nPoTotqty,nRecTotqty,; 
                               nBalaqty1,nBalaqty2,nBalaqty3,nBalaqty4,nBalaqty5,;
                               nBalaqty6,nBalaqty7,nBalaqty8,nBalTotqty);
             VALUES (&lcMasterL..STYLE,&lcMasterL..QTY1,&lcMasterL..QTY2,&lcMasterL..QTY3,;
                     &lcMasterL..QTY4 ,&lcMasterL..QTY5,&lcMasterL..QTY6,&lcMasterL..QTY7,;
                     &lcMasterL..QTY8,&lcMasterL..TotQTY,0,;
                     &lcMasterL..QTY1,&lcMasterL..QTY2,&lcMasterL..QTY3,;
                     &lcMasterL..QTY4 ,&lcMasterL..QTY5,&lcMasterL..QTY6,&lcMasterL..QTY7,;
                     &lcMasterL..QTY8,&lcMasterL..TotQTY) 
    ENDIF
    =SEEK(STYLE,'STYLE')
    REPLACE &lcTempRsln..cStyDesc WITH STYLE.Desc1
    lnTotoPos  = lnTotoPos  + &lcMasterL..TotQTY
  ENDSCAN
  IF SEEK('Total',lcTempRsln)
    SELECT (lcTempRsln)
    REPLACE nPoTotqty  WITH nPoTotqty  + lnTotoPos  ,;
            nBalTotqty WITH nPoTotqty  - nRecTotqty
  ELSE
    INSERT INTO &lcTempRsln (STYLE, nPoTotqty, nRecTotqty , nBalTotqty);
           VALUES ('Total',lnTotoPos,0,lnTotoPos)
  ENDIF       
  
ENDIF
*E500331,1 [Begin]
SELECT (lcCurAlias)
*!*************************************************************
*! Name      : lfvPO
*! Developer : Reham Al-Allamy
*! Date      : 04/21/1999
*! Purpose   : Valid function for P/O object if receive inter 
*!           : location transfer.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvPO()
*!*************************************************************
*
FUNCTION lfvPO
PRIVATE lcAlias

lcPO = IIF(llBrowse , '?' , lcPO)
IF !llBrowse .AND. (EMPTY(lcPO) OR (lcPO=lcOldValue .AND. MDOWN()))
  RETURN
ENDIF

llBrowse  = .F.
lcAlias   = ALIAS()
IF !SEEK("N"+lcPO , 'POSHDR')
  SELECT POSHDR
  DO POSBrow WITH lcPO,"","N"
ENDIF 

IF EMPTY(lcPO) OR EOF('POSHDR')
  SELECT (lcAlias)
  _CUROBJ = OBJNUM(lcPO)
  RETURN
ENDIF
SHOW GET lcPO

*--P/o Validation.
llAbort   = .F.

IF !EMPTY(lcPO)
  IF !lfChkCtkt(lcPO)
    SELECT (lcAlias)
    lcPO    = SPACE(6)
    _CUROBJ = OBJNUM(lcPO)
    RETURN
  ENDIF
ENDIF

*--Check InTransit record.
SELECT POSLN
LOCATE REST WHILE cStyType + Po = "N" + lcPO FOR Trancd = '6'
IF !FOUND()
  *** The inter Location purchase order does not issued ***
  *** from the target location yet, Unable to proceed!  ***
  =gfModalGen('TRM34109B00000','DIALOG')
  SELECT (lcAlias)
  lcPO    = SPACE(6)
  _CUROBJ = OBJNUM(lcPO)
  RETURN
ENDIF
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfChkCtkt
*! Developer : Reham Al-Allamy
*! Date      : 04/13/1999
*! Purpose   : Function to check the validity for C/T or P/O
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcTrCode - > lcCutTkt or lcPO
*!*************************************************************
*! Returns   : .T. if Valid - .F. if Invalid.
*!*************************************************************
*! Example   : =lfChkCtkt()
*!*************************************************************
*
FUNCTION lfChkCtkt
PARAMETERS lcTrCode
PRIVATE lcTrCode

*E301220,4 Reham On 05/13/1999   *** Begin ***
*E301220,4 If temp. receive by shipment, check if the entered PO# included in this shipment.
IF lcTranTyp = "I" .AND. !EMPTY(lcShip)
  SELECT POSLN
  SET ORDER TO TAG POSLNSH
  IF !SEEK(lcShip + "P" + lcCuttkt)
    *** PO #: {lcCuttkt} does not belong to shipment #: {lcShip}. ***
    *** <  Ok  > ***
    =gfModalGen("INM34159B00000" , "DIALOG" , lcCuttkt+"|"+lcShip)
    RETURN .F.
  ENDIF
  SELECT POSLN
  SET ORDER TO TAG POSLN
ENDIF
*E301220,4 Reham On 05/13/1999   *** End   ***

DO CASE
  CASE &lcMasterH..Status = 'S'
    *** {lcMessage} status is {Closed}. Therefore, no {receivings} can be done. ***
    *** <  Ok > ***
    = gfModalGen('INM34055B00000','DIALOG',lcMessage+'|'+'Closed'+'|'+'receivings')
    RETURN .F.
  CASE &lcMasterH..Status = 'B'
    *** {lcMessage} status is {Bid}. Therefore, no {receivings} can be done. ***
    *** <  Ok > ***
    = gfModalGen('INM34055B00000','DIALOG',lcMessage+'|'+'Bid'+'|'+'receivings')
    RETURN .F.
  CASE &lcMasterH..Status = 'H'
    *** {lcMessage}  status is Hold since a cost sheet has not  ***
    *** been created yet. Therefore,no receivings can be done. ***
    *** <  Ok > ***
    = gfModalGen('INM34056B00000','DIALOG',lcMessage)
    RETURN .F.
  CASE &lcMasterH..Status = 'X'
    *** {lcMessage} has been canceled. Not allowed to receive. ***
    *** <  Ok > ***
    = gfModalGen('INM34057B00000','DIALOG',lcMessage+'|'+'receive')
    RETURN .F.
  CASE &lcMasterH..Status = 'C'
    *** {lcMessage} is completely received. Do you wish to continue? ***
    *** <  Yes  > - <  No  > ***
    IF gfModalGen('INM34058B00006','DIALOG',lcMessage) = 2
      RETURN .F.
    ENDIF
ENDCASE

*-- Seek for the select CT/PO lines.
IF !SEEK(lcSeek+lcTrCode , lcMasterL)
  *** {lcMessage} lines not found, Cannot receive it.
  = gfModalGen('INM34059B00000','DIALOG',lcMessage)
  RETURN .F.
ENDIF
RETURN .T.

*!*************************************************************
*! Name      : lfvSiteLoc
*! Developer : Reham Al-Allamy
*! Date      : 04/20/1999
*! Purpose   : Valid function for the Source location if current
*!           : program is create & issue inter location transfer.
*!           : & system type is not point of sale.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvSiteLoc()
*!*************************************************************
*
FUNCTION lfvSiteLoc

IF !llBrowse .AND. !EMPTY(lcOldValue) .AND. lcSiteLoc = lcOldValue
  RETURN
ENDIF

*B602923,1 Reham On 05/23/1999   *** Begin ***
*B602923,1 Give the posibility to be out from this object.
*-- Seek for the typed target location in the waehouse file.
*IF llBrowse OR !SEEK(PADR(lcSiteLoc,6) , 'WareHous')
IF llBrowse OR (!EMPTY(lcSiteLoc) .AND. !SEEK(PADR(lcSiteLoc,6) , 'WareHous'))
  *-- Call global browse to validate the target.
  *lcSiteLoc = gfBrowWare(.F.)
  lcSiteLoc = gfBrowWare(.T.)
  *B602923,1 Reham On 05/23/1999   *** End   ***
ENDIF

llBrowse   = .F.
IF !EMPTY(lcSiteLoc) AND PADR(lcTarget,6) = PADR(lcSiteLoc,6)
  *** Target location cannot be same as the Scource location! ***
  *** <  Ok  > ***
  =gfModalGen('TRM34108B00000','DIALOG','Source'+'|'+'Target')
  lcSiteLoc = lcOldValue
  _CUROBJ  = OBJNUM(lcSiteLoc)
  RETURN
ENDIF

SHOW GET lcSiteLoc

*!*************************************************************
*! Name      : lfvTarget
*! Developer : Reham Al-Allamy
*! Date      : 04/18/1999
*! Purpose   : Valid function for the target warehouse if current
*!           : program is create & issue inter location transfer.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvTarget()
*!*************************************************************
*
FUNCTION lfvTarget

IF !llBrowse .AND. !EMPTY(lcOldValue) .AND. lcTarget = lcOldValue
  RETURN
ENDIF

*-- Seek for the typed target location in the waehouse file.
*B602923,1 Reham On 05/23/1999   *** Begin ***
*B602923,1 Give the posibility to be out from this object.
*IF llBrowse OR !SEEK(PADR(lcTarget,6) , 'WareHous')
IF llBrowse OR (!EMPTY(lcTarget) .AND. !SEEK(PADR(lcTarget,6) , 'WareHous'))
  *-- Call global browse to validate the target.
  *lcTarget = gfBrowWare(.F.)
  lcTarget = gfBrowWare(.T.)
  *B602923,1 Reham On 05/23/1999   *** End   ***
ENDIF

llBrowse   = .F.
IF !EMPTY(lcTarget) AND PADR(lcTarget,6) = PADR(lcSiteLoc,6)
  *** Target location cannot be same as the Scource location! ***
  *** <  Ok  > ***
  =gfModalGen('TRM34108B00000','DIALOG','Target'+'|'+'Scource')
  lcTarget = lcOldValue
  _CUROBJ  = OBJNUM(lcTarget)
  RETURN
ENDIF

SHOW GET lcTarget

*!*************************************************************
*! Name      : lfvTrDate
*! Developer : Reham Al-Allamy
*! Date      : 04/21/1999
*! Purpose   : Valid function for posting date.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvTrDate()
*!*************************************************************
*
FUNCTION lfvTrDate

IF llLinkToGl
  IF ldTrDate <> ldOldDate .AND. LASTKEY() = 13
    IF !CHECKPRD(ldTrDate,'lcGLFYear','lcGLPeriod','PO')
      ldTrDate = ldOldDate
      _CUROBJ = OBJNUM(ldTrDate)
      RETURN
    ENDIF
    SHOW GET ldTrDate  DISABLE
    SHOW GET ldRcvDate ENABLE
    _CUROBJ = OBJNUM(ldRcvDate)
  ENDIF
ELSE
  ldRcvDate = ldTrDate
  =lfRvDate()
ENDIF

*!*************************************************************
*! Name      : lfRvDate
*! Developer : Reham Al-Allamy
*! Date      : 04/21/1999
*! Purpose   : Valid function for issuing date.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfRvDate()
*!*************************************************************
*
FUNCTION lfRvDate

IF llLinkToGl AND ldRcvDate > ldTrDate
  *** The xxxx date cannot be after than the posting date. ***
  =gfModalGen('TRM34152B00000','DIALOG',IIF(lcTranTyp = "C" , 'issuing' , 'receiving'))
  ldRcvDate = ldOldDate
  _CUROBJ   = OBJNUM(ldRcvDate)
  RETURN
ENDIF

*!*************************************************************
*! Name      : lfvNewCt
*! Developer : Reham Al-Allamy
*! Date      : 04/13/1999
*! Purpose   : Valid function for push button <New>
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvNewCt()
*!*************************************************************
*
FUNCTION lfvNewCt

llCollect = .F.
lcImpFile = ""
lcCarton  = ""
lcCutTkt  = ""
lcCollect = IIF(rbMethod = 1 , "\<Collect" , "S\<tart    ")

*-- If not start to collect items, enable all the objects in the 1st window.
*E500331,1 ABD Enable filed carton if by carton. [Begin]
*SHOW GET lcCarton  ENABLE
IF cbByCrtn
  *-- Enable carton object & disable CT or PO.
  SHOW GET lcCarton  ENABLE
ENDIF
*E500331,1 ABD [End]
SHOW GET ibTkt
SHOW GET lcCutTkt  ENABLE
SHOW GET ibPO      ENABLE
SHOW GET lcPO      ENABLE
SHOW GET pbNewCt   DISABLE
SHOW GET rbMethod  ENABLE
IF rbMethod = 2
  lcImpFile = ""
  SHOW GET pbGetFil  DISABLE
  SHOW GET lcImpFile DISABLE
ELSE
  SHOW GET pbGetFil  ENABLE
  SHOW GET lcImpFile ENABLE
ENDIF
*SHOW GET puItem    ENABLE
SHOW GET puItem    DISABLE
SHOW GET rbScanBy  ENABLE
*B604581,1 SSH 06/24/2002 ADD DIVISION IN THE PCS OUT SCREEN.
IF lcTranTyp="C"
  SHOW GET lnDivision ENABLE
ENDIF
*B604581,1 SSH 06/24/2002 ADD DIVISION IN THE PCS OUT SCREEN.

SHOW GET pbCollect,1 PROMPT lcCollect ENABLE

*-- If receive inter location transfer.
IF lcTranTyp = "R"
  _CUROBJ = OBJNUM(lcPO)
ELSE
  *-- If temp. receiving CT/PO
  *-- Point to the right object.
  IF cbByCrtn
    _CUROBJ = OBJNUM(lcCarton)
  ELSE
    *E301220,1 Reham On 05/13/1999   *** Begin ***
    *E301220,1 Give the ability to scan by shipment.
    *_CUROBJ = OBJNUM(lcCutTkt)
    _CUROBJ = IIF(lcTranTyp = "I" , OBJNUM(lcShip) , OBJNUM(lcCutTkt))
    *E301220,1 Reham On 05/13/1999   *** End   ***
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvCollect
*! Developer : Reham Al-Allamy
*! Date      : 04/13/1999
*! Purpose   : Valid function for Collect push button.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvCollect()
*!*************************************************************
*
FUNCTION lfvCollect

*C128583,5  TMI [Start] Append data from the PAN21 file format and then update the temp variable name lcImpFile with this imported file 
IF ASCAN(laEvntTrig , PADR('IMPANFL',10)) <> 0 .AND. !gfDoTriger('PORECPO',PADR('IMPANFL',10))
  RETURN .F.
ENDIF
*C128583,5  TMI [Start] 

DO CASE
  CASE lcTranTyp $ "MI"    
    *C121114,1  TMI [Start] assign a value to a field to apply the trigger depending on it.
    IF ASCAN(laEvntTrig , PADR('IGNRPO',10)) <> 0
      =gfDoTriger('PORECPO',PADR('IGNRPO',10))
    ENDIF
    *C121114,1  TMI [End  ] 
    
    *-- If there is no CT or PO entered cananot start.
    IF (!cbByCrtn .AND. EMPTY(lcCuttkt)) .OR. (cbByCrtn .AND. (EMPTY(lcCarton) .OR. EMPTY(lcCuttkt)))
      lcTmpTxt = IIF(cbByCrtn , "carton/" + lcMessage , lcMessage) +"|" + ;
                 IIF(rbMethod = 1 , "collecting items" , "start scanning")
      *** You have to enter carton/CT before collecting items. ***
      =gfModalGen("INM34142B00000" , "DIALOUG" , lcTmpTxt)
      _CUROBJ = OBJNUM(lcCutTkt)
      RETURN
    ENDIF
  CASE lcTranTyp = "C"
    *-- If there is no target location entered.
    IF EMPTY(lcTarget) .OR. (lcSysType <> "P" .AND. EMPTY(lcSiteLoc))
      lcTmpTxt = IIF(EMPTY(lcSiteLoc) , "source location" , "target location") +"|" + IIF(rbMethod = 1 , "collecting items" , "start scanning")
      *** You have to enter carton/CT before collecting items. ***
      =gfModalGen("INM34142B00000" , "DIALOUG" , lcTmpTxt)
      _CUROBJ = IIF(EMPTY(lcSiteLoc) , OBJNUM(lcSiteLoc) , OBJNUM(lcTarget))
      RETURN
    ENDIF
  CASE lcTranTyp = "R"
    IF EMPTY(lcPO)
      lcTmpTxt = lcMessage + "|" + IIF(rbMethod = 1 , "collecting items" , "start scanning")
      *** You have to enter PO before collecting items. ***
      =gfModalGen("INM34142B00000" , "DIALOUG" , lcTmpTxt)
      _CUROBJ = OBJNUM(lcPO)
      RETURN
    ENDIF
ENDCASE

*-- If import data from text file, & there was no text file selected.
IF rbMethod =1 .AND. EMPTY(lcImpFile)
  *** You have to select the file you will use to import your items. ***
  =gfModalGen("INM34143B00000" , "DIALOUG")
  _CUROBJ = OBJNUM(lcImpFile)
  *C121114,3  TMI [Start] Refresh lcCuttkt field
  IF ASCAN(laEvntTrig , PADR('REFRESH',10)) <> 0
    =gfDoTriger('PORECPO',PADR('REFRESH',10))
  ENDIF  
  *C121114,3  TMI [End  ] 
  RETURN
ENDIF

IF rbMethod = 1
  *-- If import items from text file.
  IF !EMPTY(lcImpFile)
    *C121114,3  TMI [Start] Remove ';' from the selected temp file if found
    IF ASCAN(laEvntTrig , PADR('RMVCOMA',10)) <> 0
      =gfDoTriger('PORECPO',PADR('RMVCOMA',10))
    ENDIF
    *C121114,3  TMI [End  ] 
    *-- Append the items into cursor.
    SELECT (lcTmpCurs)
    APPEND FROM (lcImpFile) TYPE DELIMITED
    *C121114,1  TMI [Start] Update the temp file by preparing it to be accepted
    IF ASCAN(laEvntTrig , PADR('UPDTMPFL',10)) <> 0
      *C121114,3  TMI [Start] if wrong file then exit
      *=gfDoTriger('PORECPO',PADR('UPDTMPFL',10))
      IF !gfDoTriger('PORECPO',PADR('UPDTMPFL',10))
        RETURN .F.
      ENDIF
      *C121114,3  TMI [End  ] 
    ENDIF      
    *C121114,1  TMI [End  ] 
    *C128583,4  TMI [Start] move this code to the start of the function, since we now will not select a PO/Ship#
    *-* *C128583,1  TMI [Start] Append data from the PAN21 file format and then update the temp variable name lcImpFile with this imported file 
    *-* IF ASCAN(laEvntTrig , PADR('IMPANFL',10)) <> 0 .AND. !gfDoTriger('PORECPO',PADR('IMPANFL',10))
    *-*   RETURN .F.
    *-* ENDIF
    *-* *C128583,1  TMI [End  ] 
    IF ASCAN(laEvntTrig , PADR('ZAPCURS',10)) <> 0
      =gfDoTriger('PORECPO',PADR('ZAPCURS',10))
    ENDIF
    *C128583,4 TMI [End  ] 
    GO TOP
    IF !EOF()
      *-- If scanning styles not UPC # or materials, get the styles masks.
      llDummy = IIF(!llUPC .AND. llStyle .AND. !EMPTY(laSepartor[1,1]) , lfGetMask() , .T.)
      *-- Call function to get styles for the selected UPC#.
      llDummy = IIF(llUPC , lfGetUPC() , .T.)
      
      *-- Flag defines if I'm in add mode or not.
      llCUpDate = .T.
      llCollect = .T.
      *C121114,1  TMI [Start] collect current shipment into a temp file
      IF ASCAN(laEvntTrig , PADR('SVSHP',10)) <> 0
        =gfDoTriger('PORECPO',PADR('SVSHP',10))
      ENDIF      
      *C121114,1  TMI [End  ] 
      SELECT (lcTmpCurs)
      
      SCAN
        *C121114,1  TMI [Start] Update lcCuttkt variable
        IF ASCAN(laEvntTrig , PADR('UPDCTTKT',10)) <> 0
          =gfDoTriger('PORECPO',PADR('UPDCTTKT',10))
        ENDIF      
        *C121114,1  TMI [End  ]         
        SELECT (lcTmpRecv)        
        APPEND BLANK
        REPLACE cTranTyp  WITH lcTranTyp ;
                PO_CT     WITH IIF(lcTranTyp $ "MI" , lcCuttkt , IIF(lcTranTyp="R" , lcPO , "")) ;
                Ship      WITH IIF(lcTranTyp $ "I"  .AND. !EMPTY(lcShip) , lcShip , "") ;
                Carton    WITH IIF(lcTranTyp $ "MI" , cbByCrtn , .F.) ;
                cStatus   WITH "O" ;
                cCarton   WITH IIF(lcTranTyp $ "MI" , lcCarton , "") ;
                cWareCode WITH IIF(lcTranTyp = "C" , lcTarget , "") ;
                lFoundUPC WITH IIF(llUPC , &lcTmpCurs..lFoundUPC , .T.) ;
                Style     WITH IIF(lFoundUPC , LEFT(RTRIM(&lcTmpCurs..Item) , LEN(RTRIM(&lcTmpCurs..Item))-1) , &lcTmpCurs..Item) ;
                Size      WITH IIF(lFoundUPC , IIF(RIGHT(ALLTRIM(&lcTmpCurs..Item),1) $ '12345678',RIGHT(ALLTRIM(&lcTmpCurs..Item),1),'1') , "") ;
                nQty      WITH 1 ;
                lByUpc    WITH llUPC
   
        *C121114,1  TMI [Start] Update SzDesc field
        IF ASCAN(laEvntTrig , PADR('GTSZDESC',10)) <> 0
          =gfDoTriger('PORECPO',PADR('GTSZDESC',10))
        ENDIF      
        *C121114,1  TMI [End  ] 
                
        lnLineCnt = lnLineCnt + 1
      ENDSCAN
    ENDIF
 
    SELECT (lcTmpCurs)
    ZAP
    
    *-- Call Function to validate styles.
    =lfVldSty()
    
    SELECT (lcTmpRecv)
    LOCATE FOR !lAccepted
    IF FOUND()
      SHOW GET pbVerify ENABLE
    ELSE
      SHOW GET pbVerify DISABLE
    ENDIF
    =lfWhenBrow()
  ENDIF
ELSE
  lnLineCnt  = 0
  llCollect  = .T.
  *-- Flag defines if I'm in add mode or not.
  llCUpDate = .T.
  SHOW GET pbVerify DISABLE
ENDIF

IF !llCollect
  *-- If not start to collect items, enable all the objects in the 1st window.
  SHOW GET pbNewCt   DISABLE
  SHOW GET rbMethod  ENABLE
  IF rbMethod = 2
    lcImpFile = ""
    SHOW GET pbGetFil  DISABLE
    SHOW GET lcImpFile DISABLE
  ELSE
    SHOW GET pbGetFil  ENABLE
    SHOW GET lcImpFile ENABLE
  ENDIF
  *SHOW GET puItem    ENABLE
  SHOW GET puItem    DISABLE
  SHOW GET rbScanBy  ENABLE
  *B604581,1 SSH 06/24/2002 ADD DIVISION IN THE PCS OUT SCREEN.
  IF lcTranTyp="C"
    SHOW GET lnDivision ENABLE
  ENDIF
  *B604581,1 SSH 06/24/2002 ADD DIVISION IN THE PCS OUT SCREEN.

  DO CASE
    CASE lcTranTyp $ "MI"
      *E301220,1 Reham On 05/13/1999   *** Begin ***
      *E301220,1 Give the ability to scan by shipment.
      *-- Point to the right object.
      *_CUROBJ = IIF(cbByCrtn , OBJNUM(lcCarton) , OBJNUM(lcCutTkt))
      _CUROBJ = IIF(cbByCrtn , OBJNUM(lcCarton) , IIF(lcTranTyp = "I" , OBJNUM(lcShip) , OBJNUM(lcCutTkt)))
      *E301220,1 Reham On 05/13/1999   *** End   ***
    CASE lcTranTyp = "C"
      _CUROBJ = OBJNUM(lcTarget)
    CASE lcTranTyp = "R"
      _CUROBJ = OBJNUM(lcPO)
  ENDCASE
ELSE
  IF laScrMode[1]
    laScrMode    = .F.
    laScrMode[4] = .T.
    SHOW GETS
    *C121114,1  TMI [Start] Disable PO# field
    IF ASCAN(laEvntTrig , PADR('DISBL',10)) <> 0
      =gfDoTriger('PORECPO',PADR('DISBL',10))
    ENDIF
    *C121114,1  TMI [End  ] 
  ENDIF
  *-- If start to collect items, disable all the objects in the 1st window.
  SHOW GET pbNewCt   ENABLE
  SHOW GET rbMethod  DISABLE
  SHOW GET pbGetFil  DISABLE
  SHOW GET lcImpFile DISABLE
  SHOW GET puItem    DISABLE
  SHOW GET rbScanBy  DISABLE
  *B604581,1 SSH 06/24/2002 ADD DIVISION IN THE PCS OUT SCREEN.
  IF lcTranTyp="C"
    SHOW GET lnDivision DISABLE
  ENDIF
  *B604581,1 SSH 06/24/2002 ADD DIVISION IN THE PCS OUT SCREEN.

  SHOW GET pbCollect DISABLE
  
  *-- Refresh the objects in window MfRcAll3.
  SHOW GET pbNewLn   ENABLE
  lcLinStat = IIF(lnLineCnt > 0 , "ENABLE" , "DISABLE")
  IF llCrStyDye .AND. lnLineCnt > 0
    SHOW GET lcDyelot  &lcLinStat
  ELSE
    SHOW GET lcDyelot  DISABLE
  ENDIF
  SHOW GET lcStyDesc &lcLinStat
  SHOW GET rbByLin   &lcLinStat
  SHOW GET lnQty     &lcLinStat
  SHOW GET lnWare    &lcLinStat
  SHOW GET llLoc     &lcLinStat
  SHOW GET lcRefer   &lcLinStat
  SHOW GET pbRemLn   &lcLinStat
  SHOW GET lnPrice   &lcLinStat
  IF rbByLin = 1
    *-- Activate the style object to add new line.
    SHOW GET lcUPC   DISABLE
    SHOW GET lcStyle ENABLE
    _CUROBJ   = OBJNUM(lcStyle)
  ELSE
    *-- Activate the UPC object to add new line.
    SHOW GET lcStyle DISABLE
    SHOW GET lcUPC   ENABLE
    _CUROBJ   = OBJNUM(lcUPC)
  ENDIF
ENDIF
SELECT (lcTmpRecv)

*C128583,1  TMI [Start] Disable new line for PAN21
IF ASCAN(laEvntTrig , PADR('ENBTNS',10)) <> 0
  =gfDoTriger('PORECPO',PADR('ENBTNS',10))
ENDIF
*C128583,1  TMI [End  ] 

*!*************************************************************
*! Name      : lfGetMask
*! Developer : Reham Al-Allamy
*! Date      : 04/13/1999
*! Purpose   : Function to get masks for selected styles.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfGetMask()
*!*************************************************************
*
FUNCTION lfGetMask
PRIVATE lcCurSty , lcCurSiz

*C121114,1  TMI [Start] if App10 and (!llUPC .AND. llStyle .AND. !EMPTY(laSepartor[1,1])  then do not enter this funcition
IF ASCAN(laEvntTrig , PADR('EXGTMSK',10)) <> 0
  IF !gfDoTriger('PORECPO',PADR('EXGTMSK',10))
    RETURN
  ENDIF
ENDIF      
*C121114,1  TMI [End  ] 

SELECT (lcTmpCurs)
SCAN
  *-- Current style.
  lcCurSty = LEFT(RTRIM(&lcTmpCurs..Item) , LEN(RTRIM(&lcTmpCurs..Item))-1)
  *-- Current Size.
  lcCurSiz = RIGHT(RTRIM(&lcTmpCurs..Item),1)
  *-- Loop in the separator array.
  FOR lnCnt = 1 TO ALEN(laSepartor,1)
    *-- If the current separator does not exist in its position in the style field.
    IF SUBSTR(lcCurSty , laSepartor[lnCnt,2] , laSepartor[lnCnt,3]) <>  laSepartor[lnCnt,1]
      *-- Insert the separator in its position in the style position.
      lcCurSty = STUFF(lcCurSty , laSepartor[lnCnt,2]+laSepartor[lnCnt,3] , 0 , laSepartor[lnCnt,1])
    ENDIF
  ENDFOR
  *-- Replace the item field with its style with the separators + size.
  REPLACE Item WITH PADR(lcCurSty,19) + lcCurSiz
ENDSCAN

*!*************************************************************
*! Name      : lfGetUPC
*! Developer : Reham Al-Allamy
*! Date      : 04/15/1999
*! Purpose   : Function to get the styles for the current UPC.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfGetUPC()
*!*************************************************************
*
FUNCTION lfGetUPC
PRIVATE lcCurUPC , lcCurSty , llFoundUPC

*-- Validate the items if from the cursor file.
SELECT (lcTmpCurs)
SCAN 
  *-- Get the current item or UPC# from the cursor file.
  lcCurUPC   = &lcTmpCurs..Item
  lcCurSty   = &lcTmpCurs..Item
  llFoundUPC = .F.
  *-- Seek for the current UPC in the style file.
  SELECT STYLEUPC
  IF SEEK(ALLTRIM(&lcTmpCurs..Item) , "STYLEUPC")
    *-- If found the UPC in the file, get its style+size in the cursor.
    lcCurSty   = STYLEUPC.Style+STYLEUPC.Size
    llFoundUPC = .T.
  ENDIF
  *-- If not found the UPC, consider this item as rejected.
  SELECT (lcTmpCurs)
  REPLACE Item      WITH lcCurSty ;
          lFoundUPC WITH llFoundUPC
  *E500331,1 ABD -  call function lfaddlns to Add Lines In temp File for recive lines. [Begin]
  =IIF(lFoundUPC,lfAddLns(.T.),.T.)
  *E500331,1 ABD - [END]
ENDSCAN

*!*************************************************************
*! Name      : lfVldSty
*! Developer : Reham Al-Allamy
*! Date      : 04/15/1999
*! Purpose   : Function to validate styles from style file.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfVldSty()
*!*************************************************************
*
FUNCTION lfVldSty

*-- Validate the items if from the cursor file.
SELECT (lcTmpRecv)
*-- Scan for selected styles and have status rejected.
SCAN FOR !lAccepted .AND. lFoundUPC
  llAccepted = .F.
  llAdjustPO = .F.
  lcStyDesc  = ""
  llCrStyDye = .F.
  lnPrice    = 0
  *-- If validate style not material.
  IF llStyle
    *-- Seek the style in the style file.
    SELECT STYLE
    IF SEEK(&lcTmpRecv..Style , "STYLE")
      lcStyDesc  = STYLE.Desc1
      llCrStyDye = (Style.cDye_Flg = "Y")
      lnPrice    = Style.nICost1
      IF lcTranTyp $ "MIR"
        *E301220,4 Reham On 05/13/1999   *** Begin ***
        *E301220,4 If temp. receiving by shipment.
        IF lcTranTyp = "I" .AND. !EMPTY(lcShip)
          *E301220,4 Check that the current PO + style included for this shipment.
          SELECT POSLN
          SET ORDER TO TAG PoSLnSh
          *C121114,1  TMI [Start] Update the variable lcCuttkt
          IF ASCAN(laEvntTrig , PADR('GTCTTKT',10)) <> 0
            =gfDoTriger('PORECPO',PADR('GTCTTKT',10))
          ENDIF
          *C121114,1  TMI [End  ] 
          IF SEEK(lcShip+"P"+lcCutTkt+&lcTmpRecv..Style)
            *E301220,4 Adjust the receiving record with type accepted.
            llAccepted = .T.
            SELECT (lcTmpRecv)
            REPLACE Dyelot    WITH PosLn.Dyelot ;
                    cWareCode WITH PosLn.cWareCode ;
                    nLineNo   WITH PosLn.LineNo ;
                    Reference WITH PosLn.Reference
          ELSE
            *** Style: {ALLTRIM(&lcTmpRecv..Style)} does not exist in the selected {"Shipment/PO}: {lcShip/lcCuttkt}. ***
            *** <  OK  > ***
            lcTmpStr   = ALLTRIM(&lcTmpRecv..Style) + "|" + "Shipment/PO" + "|" + lcShip+"/"+lcCuttkt
            *C121114,1  TMI [Start] Update the "lcTmpStr" variable in case Ship# only entered
            IF ASCAN(laEvntTrig , PADR('UPDSTR',10)) <> 0
              =gfDoTriger('PORECPO',PADR('UPDSTR',10))
            ENDIF
            *C121114,1  TMI [End  ] 
            =gfModalGen("QRM34144B00000" , "DIALOG" , lcTmpStr)
            llAccepted = .F.
          ENDIF
          SELECT POSLN
          SET ORDER TO TAG PoSLn
        ELSE
        *E301220,4 Reham On 05/13/1999   *** End   ***
          SELECT (lcMasterL)
          IF SEEK(lcSeek + IIF(lcTranTyp="R",lcPO,lcCuttkt) + &lcTmpRecv..Style , lcMasterL)
            llAccepted = .T.
            SELECT (lcTmpRecv)
            REPLACE Dyelot    WITH &lcMasterL..Dyelot ;
                    cWareCode WITH &lcMasterL..cWareCode ;
                    nLineNo   WITH &lcMasterL..LineNo ;
                    Reference WITH &lcMasterL..Reference
          ELSE
            *-- Allow other styles only in case of receive PO.
            IF lcTranTyp = "I"
              *** Style: {ALLTRIM(&lcTmpRecv..Style)} does not exist in the selected {lcMessage}: {lcCuttkt}. ***
              *** < Accepted > - < Rejected > ***
              lcTmpStr   = ALLTRIM(&lcTmpRecv..Style) + "|" + lcMessage + "|" + lcCuttkt
              llAccepted = (gfModalGen("QRM34144B34009" , "DIALOG" , lcTmpStr) = 1)
              llAdjustPO = .T.
              *-- Assign the header warehouse to the added line.
              IF SEEK("P"+lcCuttkt , "POSHDR")
                SELECT (lcTmpRecv)
                REPLACE cWareCode WITH POSHDR.cWareCode
              ENDIF
            ELSE
              *** Style: {ALLTRIM(&lcTmpRecv..Style)} does not exist in the selected {lcMessage}: {lcCuttkt}. ***
              *** <  OK  > ***
              lcTmpStr   = ALLTRIM(&lcTmpRecv..Style) + "|" + lcMessage + "|" + ;
                           IIF(lcTranTyp="M" , lcCuttkt , lcPO)
              =gfModalGen("QRM34144B00000" , "DIALOG" , lcTmpStr)
              llAccepted = .F.
            ENDIF
          ENDIF
        *E301220,4 Reham On 05/13/1999   *** Begin ***
        ENDIF
        *E301220,4 Reham On 05/13/1999   *** End   ***
      ELSE
        *-- If create & issue inter location transfer.
        *-- Check if the style is assigned to the target warehouse.
        IF !SEEK(PADR(&lcTmpRecv..Style,19) + PADR(lcTarget,6) + SPACE(10) , 'STYDYE')
          *E301280,1 Reham On 07/06/1999   *** Begin ***
          *E301280,1 Assign the location without asking the user.
          IF gcAct_Appl <> "PS"
            *** Style: xxx is not assigned to location: xxx. ***
            *** <  Add  > - < Reenter > ***
            IF gfModalGen('QRM34048B34004','DIALOG',ALLTRIM(lcIStyle)+'|'+lcTarget) = 1
              DO gpAdStyWar WITH PADR(&lcTmpRecv..Style,19,19) , SPACE(10) , lcTarget
              llAccepted = .T.
            ELSE
              llAccepted = .F.
            ENDIF
          ELSE
            DO gpAdStyWar WITH PADR(&lcTmpRecv..Style,19,19) , SPACE(10) , lcTarget
            llAccepted = .T.
          ENDIF
          *E301280,1 Reham On 07/06/1999   *** End   ***
        ENDIF
        *-- If create & issue inter location transfer.
        *-- Check if the style is assigned to the source warehouse.
        IF !SEEK(PADR(&lcTmpRecv..Style,19) + PADR(lcSiteLoc,6) + SPACE(10) , 'STYDYE')
          *E301264,4 Reham On 06/20/1999    *** Begin ***
          *E301264,4 Prevent assigning the styles to the source location.
          *** Style: xxx is not assigned to location: xxx. ***
          *** <  Ok  > ***
          =gfModalGen('QRM34048B00000','DIALOG',ALLTRIM(lcIStyle)+'|'+lcSiteLoc)
          *IF gfModalGen('QRM34048B34004','DIALOG',ALLTRIM(lcIStyle)+'|'+lcSiteLoc) = 1
          *  DO gpAdStyWar WITH PADR(&lcTmpRecv..Style,19,19) , SPACE(10) , lcSiteLoc
          *  llAccepted = .T.
          *ELSE
          llAccepted = .F.
          *ENDIF
          *E301264,4 Reham On 06/20/1999    *** End   ***
        ENDIF
        
        *E301279,1 Reham On 07/06/1999   *** Begin ***
        *E301279,1 If the system setup is point of sale.
        *E301279,1 If not allowing zero or negative stock, do not accept the selected style.
        IF lcSysType = 'P'  
          *E301279,1 If not allowing zero or negative price, do not accept the selected style.
          IF !llAcpZroPc .AND. Style.nSugRetPri <= 0
            *** Style: ALLTRIM(&lcTmpRecv..Style) has zero retail price. Cannot accept. ***
            *** <  Ok  > ***
            =gfModalGen("INM34166B00000" , "DIALOG" , ALLTRIM(&lcTmpRecv..Style))
            llAccepted = .F.
          ELSE
            llAccepted = .T.
          ENDIF
          *E301279,1 Check if allow zero or negative stock or not..
          IF !llAcpZrStk
            lcCrRcvSty = &lcTmpRecv..Style
            lcCrRcvSiz = &lcTmpRecv..Size
            *E301279,1 If this style exist in the source warehouse.
            IF SEEK(PADR(lcCrRcvSty,19) + PADR(lcSiteLoc,6) + SPACE(10) , 'STYDYE')
              *E301279,1 Check the stock of the selected style in the source warehouse.
              IF StyDye.Stk&lcCrRcvSiz <= 0
               *** Style: ALLTRIM(lcCrRcvSty) has negative stock. Cannot accept. ***
               *** <  Ok  > ***
                =gfModalGen("INM34165B00000" , "DIALOG" , ALLTRIM(lcCrRcvSty))
                llAccepted = .F.
              ELSE
                llAccepted = .T.
              ENDIF
              *E301279,1 Check if this style was received before in this session.
              SELECT (lcTmpRecv)
              lnRecvNo   = RECNO()
              lnRecvdQty = 0
              *E301279,1 Calculate all the styles qty that was received in this session 
              *E301279,1 for the same size & accepted to be added in this session.
              SUM ALL nQty FOR Style = PADR(lcCrRcvSty,19) .AND. Size = lcCrRcvSiz .AND. lAccepted TO lnRecvdQty
              *E301279,1 Restore the record pointer in the received file.
              IF lnRecvNo > 0 .AND. lnRecvNo <= RECCOUNT()
                GOTO lnRecvNo
              ENDIF
              *E301279,1 Check if the stock qty. - the received qty. a positive #.
              IF StyDye.Stk&lcCrRcvSiz - lnRecvdQty <= 0
                *** Style: ALLTRIM(lcCrRcvSty) has negative stock. Cannot accept. ***
                *** <  Ok  > ***
                =gfModalGen("INM34165B00000" , "DIALOG" , ALLTRIM(lcCrRcvSty))
                llAccepted = .F.
              ELSE
                llAccepted = .T.
              ENDIF
            ELSE
              llAccepted = .F.
            ENDIF
          ELSE
            llAccepted = .T.
          ENDIF
        ELSE
          llAccepted = .T.
        ENDIF
        *E301279,1 Reham On 07/06/1999   *** End   ***
      ENDIF
    ENDIF
    
    *-- Update the status with accepted.
    SELECT (lcTmpRecv)
    *C121114,1  TMI [Start] Reject styles in Hold Po's 
    IF ASCAN(laEvntTrig , PADR('CHKSTATS',10)) <> 0
      =gfDoTriger('PORECPO',PADR('CHKSTATS',10))
    ENDIF
    *C121114,1  TMI [End  ] 
    REPLACE lAccepted WITH llAccepted ;
            lAdjustPO WITH llAdjustPO ;
            cStyDesc  WITH IIF(EMPTY(cStyDesc) , lcStyDesc , cStyDesc) ;
            lCrStyDye WITH llCrStyDye ;
            nPrice    WITH lnPrice
    
    *E500331,1 ABD - call function lfaddlns to Add Lines In temp File for recive lines. [Begin]
    =IIF(lAccepted,lfAddLns(),.T.)
    *E500331,1 ABD - [END]
            
  ENDIF
  SELECT (lcTmpRecv)
ENDSCAN

*!*************************************************************
*! Name      : lfvNewLn
*! Developer : Reham Al-Allamy
*! Date      : 04/18/1999
*! Purpose   : Valid function for <New> line push button.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvNewLn()
*!*************************************************************
*
FUNCTION lfvNewLn

lcStyle = ""
lcUPC   = ""
rbByLin = rbScanBy
SHOW GET rbByLin
=lfRefresh(lcRcAll3)
IF rbByLin = 1
  *-- Activate the style object to add new line.
  SHOW GET lcUPC   DISABLE
  SHOW GET lcStyle ENABLE
  _CUROBJ   = OBJNUM(lcStyle)
ELSE
  *-- Activate the UPC object to add new line.
  SHOW GET lcStyle DISABLE
  SHOW GET lcUPC   ENABLE
  _CUROBJ   = OBJNUM(lcUPC)
ENDIF

*!*************************************************************
*! Name      : lfvRemLn
*! Developer : Reham Al-Allamy
*! Date      : 04/18/1999
*! Purpose   : Valid function for <Remove> line push button.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvRemLn()
*!*************************************************************
*
FUNCTION lfvRemLn

*** Are you sure you want to "Remove" this record? ***
*** < Yes > - < No > ***
*B603595,1 ABD Fix Message paremeters [Begin]
*IF gfModalGen("QRM00002B00006","ALERT") = 1
IF gfModalGen("QRM00002B00006","ALERT","remove") = 1
  *B603595,1 ABD [End]
  *-- Delete current record in the browse.
  *E500331,1 ABD - Remove Received Qty From Temp File. [begin]
  SELECT (lcTmpRecv)
  IF !EOF(lcTmpRecv)
    SELECT (lcTempRsln)
    lcSize = &lcTmpRecv..Size
    REPLACE nRecQty&lcSize   WITH nRecQty&lcSize - &lcTmpRecv..nQty ,;
            nRecTotqty       WITH nRecTotqty     - &lcTmpRecv..nQty ,;
            nBalaqty&lcSize  WITH nPoqty&lcSize  - nRecQty&lcSize ,;
            nBalTotqty       WITH nPoTotqty      - nRecTotqty


    IF SEEK('Total')
       REPLACE nRecTotqty WITH nRecTotqty - &lcTmpRecv..nQty ,;
               nBalTotqty WITH nPoTotqty  - nRecTotqty 
    ENDIF
  ENDIF
  *E500331,1 [End]
  
  SELECT (lcTmpRecv)  
  DELETE
  *B037997,1  TMI [Start] fix the problem that when record is deleted the pointer go to the top 
  *                       of the list
  *GO TOP
  SKIP
  IF EOF()
    SKIP -1
  ENDIF
  *B037997,1  TMI [End  ] 
  *-- Decrease the number of lines with 1 record.
  lnLineCnt = lnLineCnt - 1
  *-- Call the when function to refresh the screen.
  =lfWhenBrow()
  
  *E500331,1 ABD -  Cheak If end of file and style summary screen on top , remove the
  *E500331,1 ABD -  Screen and activete the anthor two screen . [begin]
  IF EOF(lcTmpRecv)
    laScrMode = .F.
    laScrMode[1] = .T.

    llSeltscrn  = .F.
    =lfShwStyls()
    SET SKIP OF PAD _INQUIRY OF _MSYSMENU .T.
  ELSE
    IF llSeltscrn
      =lfshplins()
    ENDIF  
  ENDIF
  *E500331,1 ABD - [End]
  
ENDIF

*!*************************************************************
*! Name      : lfvByLin
*! Developer : Reham Al-Allamy
*! Date      : 04/15/1999
*! Purpose   : Valid function to validate styles or UPC on line level.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvByLin()
*!*************************************************************
*
FUNCTION lfvByLin

IF lnOldValue <> rbByLin
  llUPCLine = (rbByLin = 2)
  
  *-- Update the current line in the browse.
  SELECT (lcTmpRecv)
  REPLACE lByUpc WITH llUPCLine
  
  *-- Refresh the browse.
  =lfWhenBrow()
  IF rbByLin = 1
    SHOW GET lcUPC   DISABLE
    SHOW GET lcStyle ENABLE
    _CUROBJ = OBJNUM(lcStyle)
  ELSE
    SHOW GET lcStyle DISABLE
    SHOW GET lcUPC   ENABLE
    _CUROBJ = OBJNUM(lcUPC)
  ENDIF
  =lfRefresh(lcRcAll3)
ENDIF
*RAM

*!*************************************************************
*! Name      : lfvRefer
*! Developer : Reham Al-Allamy
*! Date      : 04/18/1999
*! Purpose   : Valid function for Reference object.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvRefer()
*!*************************************************************
*
FUNCTION lfvRefer

IF lcRefer <> lcOldValue
  *-- Update the reference object.
  SELECT (lcTmpRecv)
  REPLACE Reference WITH lcRefer
ENDIF

*!*************************************************************
*! Name      : lfvQty
*! Developer : Reham Al-Allamy
*! Date      : 04/18/1999
*! Purpose   : Valid function for Qty. object.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvQty()
*!*************************************************************
*
FUNCTION lfvQty

IF lnQty <> lnOldValue
  *-- Man Start Validate that sotck wan't go below zero
  IF lcSysType = 'P' AND !llAcpZrStk
     SELECT (lcTmpRecv)
     =SEEK(PADR(Style,19) + PADR(lcSiteLoc,6) + SPACE(10) , 'STYDYE')   
     lcChkSty   = Style
     lcChkSiz   = Size
     lnRecvNo   = RECNO()
     lnRecvdQty = 0

     SUM ALL nQty FOR Style = PADR(lcChkSty,19) .AND. Size = lcChkSiz .AND. lAccepted TO lnRecvdQty

     IF lnRecvNo > 0 .AND. lnRecvNo <= RECCOUNT()
        GOTO lnRecvNo
     ENDIF
     lnRecvdQty = lnRecvdQty + lnQty - lnOldValue
     IF StyDye.Stk&lcChkSiz - lnRecvdQty < 0
        *** Style: ALLTRIM(lcChkSty) has negative stock. Cannot accept. ***
        *** <  Ok  > ***
        =gfModalGen("INM34165B00000" , "DIALOG" , ALLTRIM(lcChkSty))
        lnQty = lnOldValue
        RETURN  
     ENDIF
  ENDIF   
  *E500331,1 ABD Update the Received & Total Qty. object.  [Begin]
  *- Update the recevied Qty
  SELECT (lcTempRsln)

  IF SEEK('Total',lcTempRsln)
     REPLACE nRecTotqty WITH nRecTotqty - &lcTmpRecv..nQty + lnQty  ,;
             nBalTotqty WITH nPoTotqty  - nRecTotqty 
  ENDIF
  =SEEK(&lcTmpRecv..Style)
  lcSize = &lcTmpRecv..Size
  REPLACE nRecTotqty       WITH nRecTotqty     - &lcTmpRecv..nQty + lnQty ,;
          nRecQty&lcSize   WITH nRecQty&lcSize - &lcTmpRecv..nQty + lnQty ,;
          nBalaqty&lcSize  WITH nPoqty&lcSize  - nRecQty&lcSize          ,;
          nBalTotqty       WITH nPoTotqty      - nRecTotqty


  *-- if window style summary on to Refrech the scrren.          
  IF llSeltscrn
     =lfshplins()
  ENDIF
  *E500331,1 ABD [End]
  
  *-- Update the Qty. object.
  SELECT (lcTmpRecv)
  REPLACE nQty WITH lnQty
  SHOW WINDOW (lcTmpBrow) REFRESH
  
ENDIF

*!*************************************************************
*! Name      : lfvPrice
*! Developer : Reham Al-Allamy
*! Date      : 04/20/1999
*! Purpose   : Valid function for price object.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvPrice()
*!*************************************************************
*
FUNCTION lfvPrice

IF lnPrice <> lnOldValue
  *-- Update the Qty. object.
  SELECT (lcTmpRecv)
  REPLACE nPrice WITH lnPrice
  SHOW WINDOW (lcTmpBrow) REFRESH
ENDIF

*!*************************************************************
*! Name      : lfvDyelot
*! Developer : Reham Al-Allamy
*! Date      : 04/18/1999
*! Purpose   : Valid function for Dyelot object.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvDyelot()
*!*************************************************************
*
FUNCTION lfvDyelot

IF lcDyelot <> lcOldValue
  *-- Update the dyelot object.
  SELECT (lcTmpRecv)
  REPLACE Dyelot WITH lcDyelot
  SHOW WINDOW (lcTmpBrow) REFRESH
ENDIF

*!*************************************************************
*! Name      : lfvWare
*! Developer : Reham Al-Allamy
*! Date      : 04/18/1999
*! Purpose   : Valid function for warehouse popup.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvWare()
*!*************************************************************
*
FUNCTION lfvWare

*-- If system is setup as single warehouse & keep track of bins.
IF !llWareHous AND llWareLoc
  STORE 1 TO lnWare , lnOldValue
ENDIF

lnWare     = IIF(lnWare=0,1,lnWare)
lcWareCode = SUBSTR(laWare[lnWare],1,6)

*-- If select new warehouse.
IF lnWare <> lnOldValue
  *-- Check if the style was assigned to this warehouse befor or not.
  IF !SEEK(&lcTmpRecv..Style+lcWareCode+SPACE(10),'STYDYE')
    *** Style: xxx is not assigned to warehouse: xxx. ***
    *** <  Add  > - < Reenter > ***
    IF gfModalGen('QRM34048B34004','DIALOG',ALLTRIM(&lcTmpRecv..Style)+'|'+lcWareCode) = 1
      DO gpAdStyWar WITH &lcTmpRecv..Style,SPACE(10),lcWareCode
    ELSE
      *-- Restore old value.
      lnWare  = lnOldValue
      SHOW GET lnWare
      RETURN
    ENDIF
  ENDIF
  *-- Update with the new warehouse.
  SELECT (lcTmpRecv)
  REPLACE cWareCode WITH lcWareCode
ENDIF

*-- If company was setup to keep track of bins.
IF llWareLoc
  SELECT (lcTemLoc)
  DELETE ALL FOR cWareCode <> lcWareCode AND Style = &lcTmpRecv..Style
  SELECT WHSLOC
  SET ORDER TO TAG WhsLoc
  llFound = SEEK(lcWareCode)
  SET ORDER TO TAG WhsLocSt
  IF llFound
    =lfvLocat()
  ELSE
    *** No locations have been assigned to warehouse XXXXXX . ***
    *** <  OK  > ***
    = gfModalGen('TRM34147B00000','DIALOG',ALLTRIM(lcWareCode))
  ENDIF  
  *IF !llWareHous
    llLoc = .F.
    SHOW GET llLoc
  *ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvLocat
*! Developer : Reham Al-Allamy
*! Date      : 04/18/1999
*! Purpose   : Function called from warehouse valid function 
*!           : to validate bins.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvLocat()
*!*************************************************************
*
FUNCTION lfvLocat

*-- Get the available locations.
SELECT WHsLoc
IF SEEK(PADR(&lcTmpRecv..Style,19)+SPACE(6)+lcWareCode)
  SCAN WHILE STYLE+COLOR+CWARECODE+CLOCATION=PADR(&lcTmpRecv..Style,19)+SPACE(6)+lcWareCode
    SCATTER MEMVAR
    SELECT (lcTemLoc)
    IF !SEEK(PADR(&lcTmpRecv..Style,19)+lcWareCode)
      APPEND BLANK
      GATHER MEMVAR 
    ENDIF
  ENDSCAN
ENDIF

*-- Define the source array that hold all bins and the target that hold the selected bins.
DIMENSION laSource[1],laTarget[1]
STORE ' ' TO laSource,laTarget
lsSource = 1
*-- Select all bins in the source array.
SELECT cLocation ;
  FROM WHSLOC ;
 WHERE Style+Color+cWareCode == ;
       SPACE(19)+SPACE(6)+lcWareCode ;
  INTO ARRAY laSource

*-- Select selected bins in the target array.
lcCurStyle = PADR(&lcTmpRecv..Style,19)
SELECT cLocation ;
  FROM (lcTemLoc) ;
 WHERE Style+cWareCode+cLocation = ;
       lcCurStyle+lcWareCode;
  INTO ARRAY laTarget

*-- Call global mover to select bins.
=gfMover(@laSource,@laTarget,"Assign Locations",.T.,'lfvLoc')

*-- Delete all the bins.
SELECT (lcTemLoc)
DELETE ALL FOR Style+cWareCode+cLocation = PADR(&lcTmpRecv..Style,19)+lcWareCode

*-- Add the selected bins in the bins file.
FOR lnCount = 1 TO ALEN(laTarget)
   APPEND BLANK
   REPLACE STYLE     WITH &lcTmpRecv..Style ;
           CWARECODE WITH lcWareCode ;
           CLOCATION WITH laTarget[lnCount]
ENDFOR

*!*************************************************************
*! Name      : lfvLoc
*! Developer : Reham Al-Allamy
*! Date      : 04/18/1999
*! Purpose   : Function called from warehouse valid function 
*!           : to validate bins.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvLoc()
*!*************************************************************
*
FUNCTION lfvLoc
PARAMETERS lnOption

DO CASE 
  CASE lnOption = 1
    lcLocatin = laSource[lsSource]
    IF !SEEK(PADR(&lcTmpRecv..Style,19)+SPACE(6)+lcWareCode+lcLocatin,"WhsLoc")
      *** Location XXX is not assigned to '+'style XXX in warehouse XXX. ***
      *** <  Assign  > - <  Cancel  > ***
      IF gfModalGen('TRM34148B34011','DIALOG',ALLTRIM(lcLocatin)+'|'+ALLTRIM(&lcTmpRecv..Style)+'|'+ALLTRIM(lcWareCode)) = 1
        RETURN .T.
      ELSE
        RETURN .F.
      ENDIF
    ELSE
      RETURN .T.
    ENDIF
  CASE lnOption = 2
    FOR lnCount = 1 TO ALEN(laSource,1)
      lcLocatin=laSource[lnCount]
      IF !SEEK(PADR(&lcTmpRecv..Style,19)+SPACE(6)+lcWareCode+lcLocatin,"WhsLoc")
        *** One or more Location(s) are not assigned to '+'style XXX in warehouse XXX. ***
        *** <  Assign  > - <  Cancel  > ***
        IF gfModalGen('TRM34149B34011','DIALOG',ALLTRIM(&lcTmpRecv..Style)+'|'+ALLTRIM(lcWareCode)) = 1
          RETURN .T.
        ELSE
          RETURN .F. 
        ENDIF
      ENDIF
    ENDFOR
  CASE (lnOption=3 OR lnOption=4)
ENDCASE

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Reham Al-Allamy
*! Date      : 04/18/1999
*! Purpose   : Valid function to validate style or UPC #.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: "S" -> For style.
*!           : "U" -> For UPC.
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvStyle("S") OR =lfvStyle("U")
*!*************************************************************
*
FUNCTION lfvStyle
PARAMETERS lcValTyp
PRIVATE lcOldAlias

*-- Return if there is no entry.
IF (lcValTyp = "S" .AND. EMPTY(lcStyle)) .OR. (lcValTyp = "U" .AND. EMPTY(lcUPC))
  RETURN
ENDIF

*-- If called from the UPC object.
IF lcValTyp = "U"
  lcOldAlias = ALIAS()
  SELECT STYLEUPC
  SET ORDER TO STYUPCN
  *-- If not valid UPC #.
  IF !SEEK(ALLTRIM(lcUPC))
    *** Invalid UPC #. ***
    *** < Browse > - < Reenter > ***
    IF ("?" $ lcUPC) .OR. (gfModalGen("QRM34146B34010" , "DIALOG") = 1)
      *-- Call UPC browse.
      =lfUpcBrow()
    ELSE
      lcStyle = ""
      lcUPC   = ""
      RETURN
    ENDIF
  ELSE
    *-- If valid UPC# , Fill with style & size.
    lcStyle = STYLEUPC.Style+STYLEUPC.Size
  ENDIF
  SET ORDER TO STYLEUPC
  SELECT (lcOldAlias)
  *-- Return if there is no entry.
  IF EMPTY(SUBSTR(lcStyle,1,1))
    lcStyle = ""
    lcUPC   = ""
    RETURN
  ENDIF
ENDIF

*-- Trancate the style & the size values.
lcISize  = RIGHT(ALLTRIM(lcStyle),1)
*lcIStyle = SUBSTR(ALLTRIM(lcStyle),1,LEN(ALLTRIM(lcStyle))-1)
lcIStyle = SUBSTR(lcStyle,1,lnItmLen)

lcStyDesc  = ""
llCrStyDye = .F.
lnPrice    = 0

*-- Validate the style object from the style file.
IF !SEEK(lcIStyle,'STYLE')
  *-- If the style from the upc browse.
  IF lcValTyp = "U"
    *** The style for the selected UPC is invalid. ***
    *** < OK >
    =gfModalGen("QRM34156B00000" , "DIALOG")
    lcStyle = ""
    lcUPC   = ""
    _CUROBJ = _CUROBJ
    RETURN
  ELSE
    *** Invalid Style. ***
    *** < Browse > - < Reenter > ***
    IF ("?" $ lcIStyle) .OR. (gfModalGen("QRM34145B34010" , "DIALOG") = 1)
      *-- Call style browse.
      lcIStyle = gfStyBrw('I',lcIStyle,'',.F.)
      *-- Refresh the lines browse.
      =lfwMainBrw()
    ELSE
      lcStyle = ""
      lcUPC   = ""
      RETURN
    ENDIF
    IF EMPTY(SUBSTR(lcIStyle,1,1))
      lcStyle = ""
      lcUPC   = ""
      RETURN
    ELSE
      *-- Get the style description & dyelot flag.
      lnPrice    = Style.nICost1
      lcStyDesc  = STYLE.Desc1
      llCrStyDye = (Style.cDye_Flg = "Y")
    ENDIF
  ENDIF
ELSE
  *-- Get the style description & dyelot flag.
  lnPrice    = Style.nICost1
  lcStyDesc  = STYLE.Desc1
  llCrStyDye = (Style.cDye_Flg = "Y")
ENDIF

*E500331,1 ABD Add Screen Sizes If Browse style Only. [Begin]
IF rbByLin =1 .AND. SEEK('S'+STYLE.Scale,'SCALE') AND SCALE.Cnt <> 1
  SELECT SCALE
  SCATTER FIELDS SZ1,SZ2,SZ3,SZ4,SZ5,SZ6,SZ7,SZ8 TO laStySizes
  DIME laStySizes[SCALE.Cnt]
  lnStySizes = 1
  =lfSelSize()
  lcISize = ALLTRIM(STR(lnStySizes,1))
ENDIF
*E500331,1 ABD [End]

*-- Validate the size to see if it is a valid size or not.
lcISize = IIF(!EMPTY(lcISize) AND lcISize $ '12345678',lcISize,'1')
lnTCnt  = IIF(SEEK('S'+STYLE.Scale,'SCALE'),SCALE.Cnt,1)

IF lnTCnt < VAL(lcISize)
  *** Invalid size for this style scale. ***
  *** <  OK  > ***
  =gfModalGen('TRM34150B00000','DIALOG','size for this style scale')
  lcStyle = ""
  lcUPC   = ""
  RETURN
ENDIF

lcStyle  = lcIStyle + lcISize

*B604581,1 SSH 06/24/2002 Check style divisoin.
IF lcTranTyp <> "C" .OR. lfCheckDiv(laDivision[lnDivision,2],lcIStyle)
*B604581,1 SSH 06/24/2002 Check style divisoin.

  *-- Validate the style in the selected CT/PO
  llFondLin  = .T.
  llAdjustPO = .F.
  DO CASE
    CASE lcTranTyp $ "MIR"
      *E301220,4 Reham On 05/13/1999   *** Begin ***
      *E301220,4 If temp. receiving by shipment.
      IF lcTranTyp = "I" .AND. !EMPTY(lcShip)
        *E301220,4 Check that the current PO + style included for this shipment.
        SELECT POSLN
        SET ORDER TO TAG PoSLnSh
        *E301220,4 If the scaned style does not exist in for the selected shipment.
        IF !SEEK(lcShip+"P"+lcCutTkt+lcIStyle)
          *** Style: {ALLTRIM(lcIStyle)} does not exist in the selected {"Shipment/PO}: {lcShip/lcCuttkt}. ***
          *** <  OK  > ***
          lcTmpStr   = ALLTRIM(lcIStyle) + "|" + "Shipment/PO" + "|" + lcShip+"/"+lcCuttkt
          =gfModalGen("QRM34144B00000" , "DIALOG" , lcTmpStr)
          lcStyle = ""
          lcUPC   = ""
          RETURN
        ENDIF
        SELECT POSLN
        SET ORDER TO TAG PoSLn
      ELSE
      *E301220,4 Reham On 05/13/1999   *** End   ***
        SELECT (lcMasterL)
        IF !SEEK(lcSeek+IIF(lcTranTyp="R",lcPO,lcCuttkt)+lcIStyle , lcMasterL)
          *-- Allow other styles only in case of receive PO.
          IF lcTranTyp = "I"
            *** Style: {ALLTRIM(lcIStyle)} does not exist in the selected {lcMessage}: {lcCuttkt}. ***
            *** < Accepted > - < Rejected > ***
            lcTmpStr   = ALLTRIM(lcIStyle) + "|" + lcMessage + "|" + lcCuttkt
            IF gfModalGen("QRM34144B34009" , "DIALOG" , lcTmpStr) = 2
              lcStyle = ""
              lcUPC   = ""
              RETURN
            ENDIF
            llAdjustPO = .T.
          ELSE
            *** Style: {ALLTRIM(lcIStyle)} does not exist in the selected {lcMessage}: {lcCuttkt}. ***
            *** <  OK  > ***
            lcTmpStr   = ALLTRIM(lcIStyle) + "|" + lcMessage + "|" + IIF(lcTranTyp="R" , lcPO , lcCuttkt)
            =gfModalGen("QRM34144B00000" , "DIALOG" , lcTmpStr)
            lcStyle = ""
            lcUPC   = ""
            RETURN
          ENDIF
          llFondLin = .F.
        ENDIF
      *E301220,4 Reham On 05/13/1999   *** Begin ***
      ENDIF
      *E301220,4 Reham On 05/13/1999   *** End   ***
      SELECT (lcTmpRecv)
      APPEND BLANK
      *-- If Temp. Receiving PO/CT, update carton, PO/CT & get the dyelot, warehouse, 
      *-- Ref. & line # from the master line file.
      REPLACE PO_CT     WITH IIF(lcTranTyp="R" , lcPO , lcCuttkt) ;
              Carton    WITH cbByCrtn ;
              cCarton   WITH lcCarton ;
              Dyelot    WITH IIF(llFondLin , &lcMasterL..Dyelot , "") ;
              cWareCode WITH IIF(llFondLin , &lcMasterL..cWareCode , "") ;
              nLineNo   WITH IIF(llFondLin , &lcMasterL..LineNo , 0) ;
              Reference WITH IIF(llFondLin , &lcMasterL..Reference , "") ;
              lAdjustPO WITH llAdjustPO
      *E301220,4 Reham On 05/13/1999   *** Begin ***
      IF lcTranTyp = "I" .AND. !EMPTY(lcShip)
        REPLACE Ship WITH lcShip
      ENDIF
      *E301220,4 Reham On 05/13/1999   *** End   ***
      *-- Assign the header warehouse to the added line.
      IF llAdjustPO
        IF SEEK("P"+lcCuttkt , "POSHDR")
          SELECT (lcTmpRecv)
          REPLACE cWareCode WITH POSHDR.cWareCode
        ENDIF
      ENDIF
    CASE lcTranTyp = "C"
      *-- Check if the style is assigned to the source warehouse.
      IF !SEEK(PADR(lcIStyle,19) + PADR(lcSiteLoc,6) + SPACE(10) , 'STYDYE')
        *E301264,4 Reham On 06/20/1999    *** Begin ***
        *E301264,4 Prevent assigning the styles to the source location.
        *** Style: xxx is not assigned to location: xxx. ***
        *** <  OK  > ***
        =gfModalGen('QRM34048B00000','DIALOG',ALLTRIM(lcIStyle)+'|'+lcSiteLoc)
        *IF gfModalGen('QRM34048B34004','DIALOG',ALLTRIM(lcIStyle)+'|'+lcSiteLoc) = 1
        *  DO gpAdStyWar WITH PADR(lcIStyle,19) , SPACE(10) , lcSiteLoc
        *ELSE
          lcStyle = ""
          lcUPC   = ""
          RETURN
        *ENDIF
        *E301264,4 Reham On 06/20/1999    *** End   ***
      ENDIF
      *-- Check if the style is assigned to the target warehouse.
      IF !SEEK(PADR(lcIStyle,19) + PADR(lcTarget,6) + SPACE(10) , 'STYDYE')
        *E301280,1 Reham On 07/06/1999   *** Begin ***
        *E301280,1 Assign the location without asking the user.
        IF gcAct_Appl <> "PS"
          *** Style: xxx is not assigned to location: xxx. ***
          *** <  Add  > - < Reenter > ***
          IF gfModalGen('QRM34048B34004','DIALOG',ALLTRIM(lcIStyle)+'|'+lcTarget) = 1
            DO gpAdStyWar WITH PADR(&lcTmpRecv..Style,19,19) , SPACE(10) , lcTarget
            llAccepted = .T.
          ELSE
            llAccepted = .F.
          ENDIF
        ELSE
          DO gpAdStyWar WITH PADR(lcIStyle,19) , SPACE(10) , lcTarget
        ENDIF
        *E301280,1 Reham On 07/06/1999   *** End   ***
      ENDIF
      *E301279,1 Reham On 07/06/1999   *** Begin ***
      *E301279,1 If the system setup is point of sale.
      *E301279,1 If not allowing zero or negative stock, do not accept the selected style.
      IF lcSysType = 'P'
        *E301279,1 If not allowing zero or negative price, do not accept the selected style.
        IF !llAcpZroPc .AND. Style.nSugRetPri <= 0
          *** Styles with zero prices not accepted. ***
          *** <  Ok  > ***
          =gfModalGen("INM34164B00000" , "DIALOG")
          lcStyle = ""
          lcUPC   = ""
          RETURN
        ENDIF
        *E301279,1 Check if allow zero or negative stock or not..
        IF !llAcpZrStk
          =SEEK(PADR(lcStyle,19) + PADR(lcSiteLoc,6) + SPACE(10) , 'STYDYE')
          *E301279,1 Check the stock of the selected style in the source warehouse.
          IF StyDye.Stk&lcISize <= 0
            *** Styles with zero or negative stock not accepted. ***
            *** <  Ok  > ***
            =gfModalGen("INM34163B00000" , "DIALOG")
            lcStyle = ""
            lcUPC   = ""
            RETURN
          ENDIF
          *E301279,1 Check if this style was received before in this session.
          SELECT (lcTmpRecv)
          lnRecvNo   = RECNO()
          lnRecvdQty = 0
          *E301279,1 Calculate all the styles qty that was received in this session 
          *E301279,1 for the same size & accepted to be added in this session.
          SUM ALL nQty FOR Style = PADR(lcStyle,19) .AND. Size = lcISize .AND. lAccepted TO lnRecvdQty
          *E301279,1 Restore the record pointer in the received file.
          IF lnRecvNo > 0 .AND. lnRecvNo <= RECCOUNT()
            GOTO lnRecvNo
          ENDIF
          *E301279,1 Check if the stock qty. - the received qty. a positive #.
          IF StyDye.Stk&lcISize - lnRecvdQty <= 0
            *** Styles with zero or negative stock not accepted. ***
            *** <  Ok  > ***
            =gfModalGen("INM34163B00000" , "DIALOG")
            lcStyle = ""
            lcUPC   = ""
            RETURN
          ENDIF
        ENDIF
      ENDIF
      *E301279,1 Reham On 07/06/1999   *** End   ***
      *-- Update the status with accepted.
      SELECT (lcTmpRecv)
      APPEND BLANK
      *-- Replace the warehouse with target warehouse.
      REPLACE cWareCode WITH lcTarget
  ENDCASE
  *-- Update style, size, Qty., style desc. & the status with accepted.
  SELECT (lcTmpRecv)
  REPLACE cTranTyp  WITH lcTranTyp ;
          cStatus   WITH "O" ;
          lFoundUPC WITH  .T. ;
          Style     WITH lcIStyle ;
          Size      WITH lcISize ;
          lAccepted WITH .T. ;
          nQty      WITH 1 ;
          cStyDesc  WITH lcStyDesc ;
          lCrStyDye WITH llCrStyDye ;
          nPrice    WITH lnPrice
  *B602949,1 Reham On 05/27/1999   *** Begin ***
  *B602949,1 Replace the size description in the file to be displayed in the browse.
  REPLACE SzDesc WITH Scale.Sz&lcISize
  *B602949,1 Reham On 05/27/1999   *** End   ***
  *E500331,1 ABD - call function lfaddlns to Add Lines In temp File for recive lines. [Begin]
  IF llAdjustPO
    = lfAdjustPo()
  ELSE
    =lfAddLns()
  ENDIF
  *E500331,1 ABD - [END]
  lnLineCnt = lnLineCnt + 1
  lcStyle = ""
  lcUPC   = ""
  *-- Call the when function to refresh the screen.
  =lfWhenBrow()
*B604581,1 SSH 06/24/2002 Check style divisoin
ELSE
  =gfModalGen('TRM32020B00000','ALERT','division '+ALLTRIM(laDivision[lnDivision,1]))
  lcStyle = ""
  lcUPC   = ""
ENDIF
*B604581,1 SSH 06/24/2002 Check style divisoin
*!*************************************************************
*! Name      : lfvVerify
*! Developer : Reham Al-Allamy
*! Date      : 04/18/1999
*! Purpose   : Valid function for <Verify> push button.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvVerify()
*!*************************************************************
*
FUNCTION lfvVerify

IF llUPC .OR. rbScanBy = 2
  SELECT (lcTmpRecv)
  SCAN FOR !lAccepted .AND. !lFoundUPC 
    *-- Seek for the current UPC in the style file.
    SELECT STYLEUPC
    IF SEEK(ALLTRIM(&lcTmpRecv..Style) , "STYLEUPC")
      *-- If found the UPC in the file, get its style+size in the cursor.
      lcCurSty   = STYLEUPC.Style+STYLEUPC.Size
      *-- If not found the UPC, consider this item as rejected.
      SELECT (lcTmpRecv)
      REPLACE Style     WITH STYLEUPC.Style ;
              Size      WITH STYLEUPC.Size ;
              lFoundUPC WITH .T.
    ENDIF
  ENDSCAN
ENDIF

*-- Validate the founded styles.
=lfVldSty()

*!*************************************************************
*! Name      : lfUpcBrow
*! Developer : Reham Al-Allamy
*! Date      : 04/18/99
*! Purpose   : Function to browse UPC number.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfUpcBrow()
*!*************************************************************
*
FUNCTION lfUpcBrow

lcPBrowTitl = "Style UPC Number"   && Browse title.
lcTmpFld    = lcBrFields           && Save browse fields.
lcBrFields  = "cTemp = cUPCNUM1+cUPCNUM2+cUPCNUM3 :R:15:H='UPC',"+;
              "STYLE :R:20:H='Style',"+;
              "Size = IIF(SEEK(STYLE,'STYLE'),IIF(SEEK('S'+STYLE.SCALE,'SCALE'),EVAL('Scale.Sz'+LEFT(Size,1)),''),''):R"

*-- Array hold the style & size values.
DIMENSION latemp[2]
latemp   = ''
*-- Call aria browse to display the available UPC
llReturn = ariabrow([FOR !lUpcPack],lcPBrowTitl, gnBrFsRow1, gnBrFsCol1,;
                    gnBrFsRow2, gnBrFsCol2,'','',;
                    'STYLE,SIZE','laTemp')
*-- Fill with style + size.
lcStyle = latemp[1]+latemp[2]

*-- Restore browse fields.
lcBrFields = lcTmpFld

*!*************************************************************
*! Name      : lpSavScr
*! Developer : Reham Al-Allamy
*! Date      : 04/19/99
*! Purpose   : Save Function.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lpSavScr()
*!*************************************************************
*
FUNCTION lpSavScr

SELECT (lcTmpRecv)
LOCATE FOR lAccepted .AND. nQty > 0
IF !FOUND()
  *** There is no lines. Cannot save! ***
  *** <  Ok  > ***
  =gfModalGen("QRM34154B00000" , "DIALOG")
  STORE .F. TO llCSave
  RETURN
ENDIF

*-- Do not allow empty dyelots.
IF llStyDye
  SELECT (lcTmpRecv)
  LOCATE FOR lAccepted .AND. nQty > 0 .AND. lCrStyDye .AND. EMPTY(Dyelot)
  IF FOUND()
    *** One or more line(s) has empty dyelots. This lines will not be updated. ***
    *** < Continue > - < Cancel > ***
    IF gfModalGen("QRM34151B34012" , "DIALOG") = 2
      STORE .F. TO llCSave
      RETURN
    ENDIF
  ENDIF
ENDIF

DO CASE
  *-- If saving temp. receiving PO/CT.
  CASE lcTranTyp $ "MI"
    *E301220,4 Reham On 05/13/1999   *** Begin ***
    *E301220,4 Add new array element for shipment #.
    *DECLARE laData[8]
    DECLARE laData[9]
    *E301220,4 Reham On 05/13/1999   *** End   ***
    *-- Prepare the array will hold the header data.
    laData[1] = ""
    laData[2] = lcDesc
    laData[3] = "O"
    laData[4] = cbByCrtn
    laData[5] = 0
    laData[6] = 0
    laData[7] = 0
    laData[8] = ldDate
    *E301220,4 Reham On 05/13/1999   *** Begin ***
    *E301220,4 Add new array element for shipment #.
    laData[9] = lcShip
    *E301220,4 Reham On 05/13/1999   *** End   ***
    *-- If temp. receive PO.
    IF lcTranTyp = "I"
      *E301220,4 Reham On 05/13/1999   *** Begin ***
      *E301220,4 No need for this scan if the temp. receiving by shipment because there is no
      *E301220,4 way to add to the POs new lines if receive by shipment.
      IF EMPTY(lcShip)
      *E301220,4 Reham On 05/13/1999   *** End   ***
        SELECT (lcTmpRecv)
        *-- Scan for the accepted received Qty. that need to be added in the PO
        SCAN FOR lAccepted .AND. lAdjustPO .AND. nQty > 0
          *-- Arrays hold the cost fields.
          DECLARE laEstiCost[5] , laECost[5]
          laEstiCost = 0
          *-- Get the warehouse & the line no. from the header file.
          IF SEEK("P" + &lcTmpRecv..PO_CT , "POSHDR")
            lnCurlin = POSHDR.lastline+1
            *-- Get the style from the style file.
            IF SEEK(&lcTmpRecv..Style , "STYLE")
              SELECT STYLE
              SCATTER FIELDS nICost1,nICost2,nICost3,nICost4,nICost5 TO laEstiCost
              *-- Add record in the PO lines file.
              SELECT POSLN
              APPEND BLANK
              REPLACE cStyType  WITH "P" ;
                      Po        WITH &lcTmpRecv..PO_CT ;
                      TranCd    WITH "1" ;
                      Vendor    WITH POSHDR.Vendor ;
                      Style     WITH &lcTmpRecv..Style ;
                      Scale     WITH STYLE.Scale ;
                      LineNo    WITH lnCurLin ;
                      cWareCode WITH &lcTmpRecv..cWareCode ;
                      nLanPrRat WITH POSHDR.nPriceRat ;
                      nLanDuRat WITH POSHDR.nDutyRat ;
                      Reference WITH &lcTmpRecv..Reference ;
                      Dyelot    WITH &lcTmpRecv..Dyelot
              *-- Get the equevelant cost.
              IF POSHDR.cPriceCur = STYLE.cPriceCur
                REPLACE nCost1    WITH laEstiCost[1] ;
                        nLan_Cst1 WITH laEstiCost[1]
                =lfGetEqv('1',nLanPrRat,0,POSHDR.nCurrUnit,0,nLan_Cst1,0,0,0,0)
                REPLACE nECost1    WITH laECost[1] ;
                        nELanCost1 WITH laECost[1]
              ENDIF
              *-- Get the equevelant cost.
              IF POSHDR.cDutyCur = STYLE.cDutyCur
                REPLACE nCost2    WITH laEstiCost[2] ;
                        nCost3    WITH laEstiCost[3] ;
                        nCost4    WITH laEstiCost[4] ;
                        nCost5    WITH laEstiCost[5] ;
                        nLan_Cst2 WITH laEstiCost[2] ;
                        nLan_Cst3 WITH laEstiCost[3] ;
                        nLan_Cst4 WITH laEstiCost[4] ;
                        nLan_Cst5 WITH laEstiCost[5]
                =lfGetEqv('2345',0,nLanDuRat,0,POSHDR.nDcurUnit,0,nLan_Cst2,nLan_Cst3,nLan_Cst4,nLan_Cst5)
                GATHER FROM laECost FIELDS nECost2,nECost3,nECost4,nECost5
                GATHER FROM laECost FIELDS nELanCost2,nELanCost3,nELanCost4,nELanCost5
              ENDIF
              =gfAdd_Info()
              =gfTraceKey("POSLN",cstytype+po+style+STR(lineno,6)+trancd,"A")
              
              SELECT POSHDR
              REPLACE lastline WITH lastline + 1
              =gfTraceKey("POSHDR",cstytype+po,"M")
              SELECT (lcTmpRecv)
              REPLACE nLineNo WITH POSHDR.LastLine
            ENDIF
          ENDIF
          SELECT (lcTmpRecv)
        ENDSCAN
      *E301220,4 Reham On 05/13/1999   *** Begin ***
      ENDIF
      *E301220,4 Reham On 05/13/1999   *** End   ***
    ENDIF
    
    *-- Create the temp file will hold the lines.
    =lfTempFile()
    *-- Prepare file hold the lines of the lines file.
    IF cbByCrtn
      *-- Define line counter.
      lnCountLin = 1
      *-- Scan for the accepted lines & have received Qty.
      SELECT (lcTmpRecv)
      SCAN FOR lAccepted .AND. nQty > 0
        *-- Define variable to hold the size to be updated.
        lcSize = &lcTmpRecv..Size
        SELECT (lcTmpLine)
        APPEND BLANK
        REPLACE cType      WITH lcTranTyp ;
                cCarton    WITH &lcTmpRecv..cCarton ;
                Cuttkt     WITH &lcTmpRecv..PO_CT ;
                Style      WITH &lcTmpRecv..Style ;
                cWareCode  WITH &lcTmpRecv..cWareCode ;
                Dyelot     WITH &lcTmpRecv..Dyelot ;
                LineNo     WITH lnCountLin ;
                TranCd     WITH "2" ;
                Reference  WITH &lcTmpRecv..Reference ;
                Qty&lcSize WITH &lcTmpRecv..nQty ;
                TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ;
                nLineNo    WITH &lcTmpRecv..nLineNo ;
                Flag       WITH "A"

        *-- Update the line counter.
        lnCountLin = lnCountLin + 1
        *-- Update arrauy element of received stock value.
        laData[5] = laData[5] + &lcTmpLine..TotQty
        
        *-- Select the temp. receive file again.
        SELECT (lcTmpRecv)
      ENDSCAN
    ELSE
      SELECT (lcTmpLine)
      SET ORDER TO TAG TmpLine2
      *-- Scan for the accepted lines & have received Qty.
      SELECT (lcTmpRecv)
      SCAN FOR lAccepted .AND. nQty > 0
        *-- Define variable to hold the size to be updated.
        lcSize       = &lcTmpRecv..Size
        *-- Define the key to search for.
        lcRcvKey = &lcTmpRecv..cCarton+&lcTmpRecv..PO_CT+&lcTmpRecv..Style+&lcTmpRecv..Dyelot+STR(&lcTmpRecv..nLineNo,6)
        SELECT (lcTmpLine)
        IF !SEEK(lcRcvKey , lcTmpLine)
          APPEND BLANK
          REPLACE cType      WITH lcTranTyp ;
                  Cuttkt     WITH &lcTmpRecv..PO_CT ;
                  Style      WITH &lcTmpRecv..Style ;
                  cWareCode  WITH &lcTmpRecv..cWareCode ;
                  Dyelot     WITH &lcTmpRecv..Dyelot ;
                  TranCd     WITH "2" ;
                  Reference  WITH &lcTmpRecv..Reference ;
                  Qty&lcSize WITH Qty&lcSize + &lcTmpRecv..nQty ;
                  TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ;
                  nLineNo    WITH &lcTmpRecv..nLineNo ;
                  Flag       WITH "A"
        ELSE
          REPLACE Qty&lcSize WITH Qty&lcSize + &lcTmpRecv..nQty ;
                  TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
        ENDIF
        *-- Update arrauy element of received stock value.
        laData[5] = laData[5] + &lcTmpRecv..nQty
        
        *-- Select the temp. receive file again.
        SELECT (lcTmpRecv)
      ENDSCAN
    ENDIF
    
    *-- Call the print log report if receiving PO.
    IF lcTranTyp = "I"
      *-- Function to execute the print receiving log report.
      =lfCallRep()
    ENDIF
    
    IF !llCSave
      RETURN .F.
    ENDIF
    
    STORE .T. TO llCSave
    lcMPara = lcTranTyp
    *--Call the temp. receiving CT/PO Update function.
    DO lpSavScr IN (gcapphome+'MFRCVTP.FXP')
       
  *-- If Creating Inter location transfer & issuing it or Receive inter location transfer.
  CASE lcTranTyp $ "CR"
    IF lcTranTyp = "C"
      *-- Define the needed variables for the saving procedure of Inter location transfer.
      lcAType     = "N"
      lnShpLoc    = 1
      lcPOType    = 'P/O'
      llRecalTot  = .T.
      lcDropLoc   = IIF(!llWareHous , lcWareCode , laCrSetups[7,2])
      llGenOrNum  = (UPPER(ALLTRIM(laCrSetups[8,2])) ='Y')
      lcSvdPO     = ""
      lnSelWhr    = 0
      lcUnCmPgId  = PADR("STYLE_PO"+lcAType,10)
      lcAProgm    = "POISINT"
      STORE .F. TO llMultiWare , llContinue , llApIstall
      
      *--Uncomplete session variables.
      DECLARE laVariables[6]
      laVariables[1] = 'ldCompDef'
      laVariables[2] = 'lnSelWhr'
      laVariables[3] = 'laScrMode[2]'
      laVariables[4] = 'laScrMode[3]'
      laVariables[5] = 'laScrMode[4]'
      laVariables[6] = 'lcSvdPO'
      
      *-- If the inter location transfer was not saved yet.
      IF !llSav_ILT
        ****************************************
        **** Create Inter Location transfer ****
        ****************************************
        
        *-- Call local function to create the temp. files needed for each program.
        =lfTempFile()
        
        *-- Prepare the array will hold the header data.
        DECLARE laData[71]
        SELECT (lcPOHdr)
        *-- Blank the array that will be used in the save procedure of PO.
        SCATTER FIELDS &lcScFields MEMO TO laData BLANK
        
        laData[2]  = lcSiteLoc
        laData[3]  = "O"
        laData[20] = "DEFDEF"
        *-- Get the Location Address.
        =SEEK(lcTarget,'WAREHOUS')
        laData[18] = WareHous.cDesc
        laData[19] = lcTarget
        laData[21] = WareHous.cAddress1
        laData[22] = WareHous.cAddress2
        laData[23] = WareHous.cAddress3
        laData[24] = WareHous.cAddress4
        laData[25] = WareHous.cAddress5
        *-- Get the default codes.
        laData[4]  = lfGetCodes("CDIVISION")
        laData[10] = lfGetCodes("CTERMCODE")
        laData[11] = lfGetCodes("SHIPVIA")
        laData[67] = lfGetCodes("CPURCODE")
        *-- Get the needed date.
        laData[5]  = gdSysDate
        STORE (gdSysDate + 90) TO laData[6] , laData[7] , ldCompDef
        *-- Get the currency information.
        STORE gcBaseCurr TO laData[26] , laData[27]
        STORE 1 TO laData[28] , laData[29] , lnCurrUnt1 , lnCurrUnt2
        
        *-- Add new line in the temp. PO file.
        SELECT (lcPOHdr)
        APPEND BLANK 
        REPLACE cStyType   WITH lcAType ;
                lMultiWare WITH llMultiWare
        
        *-- Define line counter.
        lnCountLin = 0
        *-- Scan for the accepted lines & have received Qty.
        SELECT (lcTmpRecv)
        SCAN FOR lAccepted .AND. nQty > 0
          *-- Point to the style record in the style file.
          =SEEK(&lcTmpRecv..Style , "Style")
          *-- Define variable to hold the size to be updated.
          lcSize = &lcTmpRecv..Size
          SELECT (lcPOLine)
          LOCATE FOR Style+Dyelot = &lcTmpRecv..Style+&lcTmpRecv..Dyelot
          IF !FOUND()
            *-- Increment the line no. for the new line.
            lnCountLin = lnCountLin + 1
            *-- Add new line in the PO line file.
            APPEND BLANK
            REPLACE cStyType   WITH "N" ;
                    TranCd     WITH "1" ;
                    Lineno     WITH lnCountLin ;
                    Vendor     WITH lcSiteLoc ;
                    Style      WITH &lcTmpRecv..Style ;
                    Dyelot     WITH &lcTmpRecv..Dyelot ;
                    Qty&lcSize WITH Qty&lcSize + &lcTmpRecv..nQty ;
                    TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ;
                    cWareCode  WITH lcTarget ;
                    Reference  WITH &lcTmpRecv..Reference ;
                    cStyGrade  WITH "1" ;
                    Scale      WITH Style.Scale ;
                    Gros_Price WITH IIF(Style.nICost1 <> &lcTmpRecv..nPrice , &lcTmpRecv..nPrice , 0) ;
                    nCost1     WITH &lcTmpRecv..nPrice ;
                    nCost2     WITH IIF(!llMulCurr OR STYLE.cDutyCur = gcBaseCurr,Style.nICost2,0) ;
                    nCost3     WITH IIF(!llMulCurr OR STYLE.cDutyCur = gcBaseCurr,Style.nICost3,0) ;
                    nCost4     WITH IIF(!llMulCurr OR STYLE.cDutyCur = gcBaseCurr,Style.nICost4,0) ;
                    nCost5     WITH IIF(!llMulCurr OR STYLE.cDutyCur = gcBaseCurr,Style.nICost5,0) ;
                    nECost1    WITH nCost1 ;
                    nECost2    WITH nCost2 ;
                    nECost3    WITH nCost3 ;
                    nECost4    WITH nCost4 ;
                    nECost5    WITH nCost5
          ELSE
            REPLACE Qty&lcSize WITH Qty&lcSize + &lcTmpRecv..nQty ;
                    TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
          ENDIF
          
          *-- Select the temp. receive file again.
          SELECT (lcTmpRecv)
        ENDSCAN
        
        STORE .T. TO llCSave
        *--Call the temp. receiving CT/PO Update function.
        DO lpSavScr IN (gcapphome+'POSTYLE.FXP')
        
        *-- Set a flag to know if the inter location transfer was saved or not.
        llSav_ILT =  llCSave
      ELSE
        *-- Prepare the array will hold the header data.
        DECLARE laData[71]
        SELECT (lcPOHdr)
        *-- Fill the array that will be used in the save procedure of PO.
        SCATTER FIELDS &lcScFields MEMO TO laData
      ENDIF
    ENDIF
    
    ***************************************************
    **** Issue or Recveive Inter Location transfer ****
    ***************************************************
    
    *-- Define the variable needed before calling save procedure of 
    *-- issue inter location transfer.
    lcPType    = IIF(lcTranTyp = "C" , "N" , "O")
    lcBrDtTtl  = IIF(lcTranTyp = "C" , 'Issuing Lines','Receiving Lines')
    llDyelot   = llStyDye

    STOR .F.  TO llContinue , llMFCall
    STORE " " TO lcShpCode , lcGLFYear , lcGLPeriod , lcAirWayB , ;
                 lcShpRef , lcGlSession , lcBatch
    STORE 0   TO lnTotStk , lnTotDam , lnTotCan , lnCartons
    STORE {}  TO ldEnterd , ldEta
    
    =CHECKPRD(ldTrDate,'lcGLFYear','lcGLPeriod','PO')
    
    *--Uncomplete session variables.
    DECLARE laVariables[14]
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
    
    *-- Flag to know if open the vendor file.
    llOpenVend = .F.
    IF !USED("APVENDOR")
      llOpenVend = .T.
      *-- Open the vendor file because it is used by the save procedure will be called.
      =gfOpenFile(gcDataDir+'APVENDOR',gcDataDir+'VenCode','SH')
    ENDIF
    
    *-- Prepare temp. file for issuing inter location transfer.
    DO CASE
      CASE lcTranTyp = "C"
        *-- Call local function to create the temp. files needed for each program.
        =lfCreatLin()
        
        *-- Get the original lines.
        SELECT PosLn
        *B602923,1 Reham On 05/23/1999   *** Begin ***
        *B602923,1 Use rush more to optimize the speed.
        =SEEK("N" + laData[1])
        *SCAN FOR Posln.cstytype + Posln.po = "N" + laData[1] .AND. TranCd = "1"
        SCAN FOR cStyType+Po+Style+STR(LineNo,6)+TranCd = "N"+laData[1] .AND. TranCd = "1"
        *B602923,1 Reham On 05/23/1999   *** End   ***
          *-- Get the original line.
          SCATTER MEMVAR
          *-- Add original line in the temp. file.
          SELECT (lcTmpLine)
          APPEND BLANK
          GATHER MEMVAR
          REPLACE nLanPrRat  WITH 1 ;
                  nLanDuRat  WITH 1 ;
                  TotStk     WITH TotQty ;
                  lAutoMode  WITH .T. ;
                  nLan_CST1  WITH nCost1 ;
                  nLan_CST2  WITH nCost2 ;
                  nLan_CST3  WITH nCost3 ;
                  nLan_CST4  WITH nCost4 ;
                  nLan_CST5  WITH nCost5 ;
                  nELanCost1 WITH nCost1 ;
                  nELanCost2 WITH nCost2 ;
                  nELanCost3 WITH nCost3 ;
                  nELanCost4 WITH nCost4 ;
                  nELanCost5 WITH nCost5
          *-- Add line with transaction code = 2.
          SCATTER MEMVAR
          APPEND BLANK
          GATHER MEMVAR
          REPLACE TranCd WITH "2"
        ENDSCAN
        SELECT (lcTmpLine)
      CASE lcTranTyp = "R"
        *-- Prepare the needed variables if calling update procedure of the receiving
        *-- program with type "O" (Receive inter location transfer).
        lnTotStk   = 0
        lcMasterLn = "POSLN"
        lcOthrTrCd = "4"
        lcAProgm   = "POSTREC"
        lcUnCmPgId = "RECIVING_O"
        llOpnBomLn = .F. 
        STORE 1 TO lnRate1 , lnRate2 , lnCurrUnt1 , lnCurrUnt2
        lcTmDyeRel = gfTempName()
        lcTempFile = gfTempName()
        lcTmpCtPk  = gfTempName()
        *-- Check if the SO module is installed or not.
        llSOInstld = (OCCURS('SO',gcCmpModules)<>0)
        *-- Open the cuttiking picking file if the so module is installed.
        llOpenPick = .F.  && Flag to know if opened the cutpick file or not.
        IF llSOInstld 
          IF !USED("CUTPICK")
            llOpenPick = .T.
            =gfOpenFile(gcDataDir+"CutPick","Cutpick","SH")
          ENDIF
        ENDIF
        
        *-- Call local function to create the temp. files needed for each program.
        =lfCreatLin()
        
        *-- Update the temp file will be send to the save procedure of the receiving program.
        SELECT (lcTmpRecv)
        SCAN FOR lAccepted .AND. nQty > 0
          *-- Define variable to hold the size to be updated.
          lcSize = &lcTmpRecv..Size
          *-- Define the key to search for.
          lcKey  = "N" + &lcTmpRecv..PO_CT+&lcTmpRecv..Style+STR(&lcTmpRecv..nLineNo,6)
          SELECT (lcTmpLine)
          SET ORDER TO PosLn
          IF !SEEK(lcKey + "1" , lcTmpLine)
            *-- Get the original line with transaction code "1".
            SELECT PosLn
            =SEEK(lcKey + "1")
            *-- Array hold the open Qty. for the current PO line.
            DEClARE laOpnQty[8]
            laOpnQty = 0
            *-- Save current record pointer.
            lnSvRc   = RECNO()
            *--Get originals.
            SCATTER MEMVAR
            *-- Scan for other transaction codes to get the open qty.
            SCAN REST WHILE cStyType+PO+Style+STR(LineNo,6)=lcKey FOR TranCd <> '1'
              FOR I = 1 TO 8
                lcCnt = STR(I,1)
                laOpnQty[I] = IIF(TranCd="6" , laOpnQty[I]+Qty&lcCnt , MAX(laOpnQty[I]-Qty&lcCnt,0))
              ENDFOR
            ENDSCAN
            *-- Restore record pointer.
            IF BETWEEN(lnSvRc,1,RECCOUNT())
              GOTO lnSvRc
            ENDIF  
            
            *-- Add original line in the temp. file.
            SELECT (lcTmpLine)
            APPEND BLANK
            GATHER MEMVAR
            GATHER FROM laOpnQty FIELDS Qty1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8
            REPLACE TranCd WITH '1' ;
                    TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ;
                    TotStk WITH &lcTmpRecv..nQty ;
                    TotBal WITH TotQty
            
            SCATTER MEMVAR
            *-- Add the currently received qty.
            APPEND BLANK
            GATHER MEMVAR
            REPLACE Dyelot     WITH &lcTmpRecv..Dyelot ;
                    TranCd     WITH "2" ;
                    Reference  WITH &lcTmpRecv..Reference ;
                    Qty1       WITH 0 ;
                    Qty2       WITH 0 ;
                    Qty3       WITH 0 ;
                    Qty4       WITH 0 ;
                    Qty5       WITH 0 ;
                    Qty6       WITH 0 ;
                    Qty7       WITH 0 ;
                    Qty8       WITH 0
            REPLACE Qty&lcSize WITH &lcTmpRecv..nQty ;
                    TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ;
                    TotStk     WITH TotBal ;
                    TotBal     WITH 0
          ELSE
            REPLACE TotStk WITH TotStk + &lcTmpRecv..nQty
            IF SEEK(lcKey + "2" , lcTmpLine)
              REPLACE Qty&lcSize WITH Qty&lcSize + &lcTmpRecv..nQty ;
                      TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
            ENDIF
          ENDIF
          lnTotStk = lnTotStk + &lcTmpRecv..nQty
          *-- Select the temp. receive file again.
          SELECT (lcTmpRecv)
        ENDSCAN
        
        *-- If the system type was point of sale, do not over receive.
        IF lcSysType = "P"
          DECLARE laOpn[9] , laRec[9]
          *-- Scan for the original lines to get the opened Qty.
          SELECT (lcTmpLine)
          SET ORDER TO POSLN
          SCAN FOR TranCd = "1"
            *-- Save current record pointer.
            lnCurRec = RECNO()
            *-- Blank th opened & received Qty. arrays.
            STORE 0 TO laOpn , laRec
            *-- Set flags to false.
            STORE .F. TO llAdjust , llCancel
            *-- Scatter the open qty.
            SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laOpn
            *-- Seek for the received line qty.
            lcKey  = "N" + &lcTmpLine..PO+&lcTmpLine..Style+STR(&lcTmpLine..LineNo,6)+"2"
            IF SEEK(lcKey)
              *-- Scatter the received qty.
              SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laRec
            ENDIF
            FOR lnCnt = 1 TO 8
              *-- If the received qty. over the open qty.
              IF laRec[lnCnt] > laOpn[lnCnt]
                *** You are not allowed to over receive the open qty. ***
                *** There is an over receive in one or more sizes for ***
                *** style {ALLTRIM(lcTmpLine.Style)}. Adjust the over ***
                *** received qty. with the open qty. or delete this   ***
                *** received line? ***
                *** < Adjust > - < Delete > ***
                IF gfModalGen("QRM34153B34013" , "DIALOG" , ALLTRIM(&lcTmpLine..Style)) = 1
                  *-- Set flag to adjust the over received qty.
                  llAdjust = .T.
                ELSE
                  *-- Set flag to delete the current received line.
                  llCancel = .T.
                ENDIF
                EXIT
              ENDIF
            ENDFOR
            DO CASE
              *-- If adjust the received qty.
              CASE llAdjust
                *-- Adjust the over received qty.
                FOR lnCnt = 1 TO 8
                  laRec[lnCnt] = IIF(laRec[lnCnt] > laOpn[lnCnt] , laOpn[lnCnt] , laRec[lnCnt])
                ENDFOR
                laRec[9] = laRec[1]+laRec[2]+laRec[3]+laRec[4]+laRec[5]+laRec[6]+laRec[7]+laRec[8]
                *-- Adjust the line.
                GATHER FROM laRec FIELDS Qty1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TotQty
              *-- If cancel the current received line, delete the original line.
              CASE llCancel
                DELETE
            ENDCASE
            *-- Restore the record pointer in the lines file.
            IF lnCurRec > 0 .AND. lnCurRec <= RECCOUNT()
              GOTO lnCurRec 
            ENDIF
            *-- If cancel the current received line, delete the received line.
            IF llCancel
              DELETE
            ENDIF
          ENDSCAN
          *-- Check if there is still lines to be saved.
          SELECT (lcTmpLine)
          GO TOP
          IF EMPTY(&lcTmpLine..PO)
            *** There is no lines. Cannot save! ***
            *** <  Ok  > ***
            =gfModalGen("QRM34154B00000" , "DIALOG")
            STORE .F. TO llCSave
            RETURN
          ENDIF
        ENDIF
    ENDCASE
    STORE .T. TO llCSave
    *--Call the Issuing inter location transfer Update function.
    DO lpSavScr IN (gcapphome+'POSTREC.FXP') WITH .T.
    
    *-- If the cutting picking file was opened in this session, close it.
    IF lcTranTyp = "R" .AND. llOpenPick
      USE IN CUTPICK
    ENDIF
    *-- If the vendor file was opened in this session, close it.
    IF llOpenVend
      USE IN APVENDOR
    ENDIF
ENDCASE

SELECT (lcBaseFile)

*C128583,1  TMI [Start] Update the ctktrcvh.IMPFILE field
IF ASCAN(laEvntTrig , PADR('SAVBATCH',10)) <> 0
  =gfDoTriger('PORECPO',PADR('SAVBATCH',10))
ENDIF
*C128583,1  TMI [End  ] 

*!*************************************************************
*! Name      : lfGetCodes
*! Developer : Reham Al-Allamy
*! Date      : 04/18/99
*! Purpose   : Function to get codes.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : lcField   -> Field Name.
*!*************************************************************
*! Returns            : lcCurCode -> Code Value.
*!*************************************************************
*! Example   : =lfGetCodes()
*!*************************************************************
*
FUNCTION lfGetCodes
PARAMETERS lcField
PRIVATE lcField

*-- Initialize the "Codes Information" array.
DIMENSION laCodInfo [1,10] , laArray[1]
STORE SPACE(0) TO laCodInfo , laArray , lcCurCode
lnPopup = 1
*-- Fill the "Codes Information" array.
laCodInfo[1,01] = lcField
laCodInfo[1,02] = "laArray"
laCodInfo[1,03] = "lnPopup"
laCodInfo[1,04] = ""
laCodInfo[1,05] = .F.
laCodInfo[1,06] = .F.
laCodInfo[1,07] = "POSHDR"
laCodInfo[1,08] = "POSHDR"
laCodInfo[1,09] = "'N'+laData[1]"
laCodInfo[1,10] = lcField

=gfwCodePop(@laCodInfo, lcField , "D")
lcCurCode = laArray[lnPopup,2]

RETURN lcCurCode

*!*************************************************************
*! Name      : lfTempFile
*! Developer : Reham Al-Allamy
*! Date      : 04/20/99
*! Purpose   : Function to create the needed temp. files.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfTempFile()
*!*************************************************************
*
FUNCTION lfTempFile

DO CASE
  CASE lcTranTyp $ "MI"
    *-- Prepare the file will be send to the temp. receiving program.
    SELECT CTKTRCVL
    SET RELATION TO Style INTO Style
    =AFIELDS(laFStru)
    lnFStru = ALEN(laFStru,1)
    DIMENSION laFStru[lnFStru+4,4]
    laFStru[lnFStru+1,1] = 'TOTSTK'
    laFStru[lnFStru+2,1] = 'TOTDAM'
    laFStru[lnFStru+3,1] = 'TOTCAN'  
    laFStru[lnFStru+4,1] = 'TOTBAL'
    STORE 'N' TO laFStru[lnFStru+1,2],laFStru[lnFStru+2,2],laFStru[lnFStru+3,2],laFStru[lnFStru+4,2]
    STORE  6  TO laFStru[lnFStru+1,3],laFStru[lnFStru+2,3],laFStru[lnFStru+3,3],laFStru[lnFStru+4,3]
    STORE  0  TO laFStru[lnFStru+1,4],laFStru[lnFStru+2,4],laFStru[lnFStru+3,4],laFStru[lnFStru+4,4]
    CREATE DBF (gcWorkDir+lcTmpLine) FROM ARRAY laFStru
    INDEX ON TranCd+cCarton+Cuttkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6) TAG TmpLine1  
    INDEX ON cCarton+Cuttkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6)+TranCd TAG TmpLine2
    INDEX ON Cuttkt+STR(nLineNo,6)+STR(LineNo,6)+TranCd TAG TmpLine3
    IF lcTranTyp = "I"
      =lfCreatLin()
    ENDIF
  CASE lcTranTyp $ "CR"
    IF lcTranTyp = "C"
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
      
      *-Create line file. 
      SELECT POSLN
      =AFIELDS(laStrufile)
      lnFileStru = ALEN(laStrufile,1)
      DIMENSION laStrufile[lnFileStru+1,4]
      laStrufile[lnFileStru+1,1] = 'nSteps'
      laStrufile[lnFileStru+1,2] = 'N'
      laStrufile[lnFileStru+1,3] = 2
      laStrufile[lnFileStru+1,4] = 0
      
      =gfCrtTmp(lcPOline,@laStrufile,'Style+STR(LineNo,6)',lcPOline)
      =gfCrtTmp(lcPOTran,@laStrufile,'Style+STR(LineNo,6)',lcPOTran)
    ENDIF
    *-- Call local function to create temp. lines file.
    =lfCreatLin()
ENDCASE

*!*************************************************************
*! Name      : lfCreatLin
*! Developer : Reham Al-Allamy
*! Date      : 04/26/99
*! Purpose   : Function to create the temp. lines file.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfCreatLin()
*!*************************************************************
*
FUNCTION lfCreatLin

*-- In case of create & issue or receive inter location transfer.
SELECT POSLN
=AFIELDS(laFStru)
lnFStru = ALEN(laFStru,1)
DIMENSION laFStru[lnFStru+13,4]
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
laFStru[lnFStru+12,1]= 'LCOSTMADE'
laFStru[lnFStru+13,1]= 'LAUTOMODE'
STORE 'N' TO laFStru[lnFStru+1,2],laFStru[lnFStru+2,2],;
             laFStru[lnFStru+3,2],laFStru[lnFStru+4,2],;
             laFStru[lnFStru+6,2],laFStru[lnFStru+8,2]
STORE 'C' TO laFStru[lnFStru+5,2],laFStru[lnFStru+10,2]
STORE 'L' TO laFStru[lnFStru+7,2],laFStru[lnFStru+9,2],;
             laFStru[lnFStru+11,2],laFStru[lnFStru+13,2]
laFStru[lnFStru+12,2]= 'L'
STORE  6  TO laFStru[lnFStru+1,3],laFStru[lnFStru+2,3],;
             laFStru[lnFStru+3,3],laFStru[lnFStru+4,3],;
             laFStru[lnFStru+6,3],laFStru[lnFStru+10,3]
STORE  3  TO laFStru[lnFStru+5,3]  
STORE  2  TO laFStru[lnFStru+8,3]
STORE  1  TO laFStru[lnFStru+7,3],laFStru[lnFStru+9,3],;
             laFStru[lnFStru+11,3],laFStru[lnFStru+13,3]
laFStru[lnFStru+12,3]= 1
STORE  0  TO laFStru[lnFStru+1,4],laFStru[lnFStru+2,4],;
             laFStru[lnFStru+3,4],laFStru[lnFStru+4,4],;
             laFStru[lnFStru+5,4],laFStru[lnFStru+6,4],;
             laFStru[lnFStru+7,4],laFStru[lnFStru+8,4],;
             laFStru[lnFStru+9,4],laFStru[lnFStru+10,4],;
             laFStru[lnFStru+11,4],laFStru[lnFStru+13,4]
laFStru[lnFStru+12,4]= 0
DIMENSION laTags[5,2]
laTags[1,1]='TranCd+cCarton+Po+Style+Dyelot+cWareCode+STR(LineNo,6)'
laTags[2,1]='cCarton+PO+Style+Dyelot+cWareCode+STR(LineNo,6)+TranCd'
laTags[3,1]='PO+Style+Dyelot+cWareCode+STR(LineNo,6)+cCarton+TranCd'
laTags[4,1]='cstytype+po+style+STR(lineno,6)+TranCd'
laTags[5,1]='shipno+cstytype+po+style+STR(lineno,6)+trancd'
laTags[1,2]='TmpLine1'
laTags[2,2]='TmpLine2'
laTags[3,2]='TmpLine3'
laTags[4,2]='POSLN'
laTags[5,2]='POSLNSH'

IF lcTranTyp = "I"
  =gfCrtTmp(lcTmpPoLn,@laFStru,@laTags)
  SELECT (lcTmpPoLn)
ELSE
  =gfCrtTmp(lcTmpLine,@laFStru,@laTags)
  SELECT (lcTmpLine)
ENDIF
SET ORDER TO TAG TmpLine1

*!*************************************************************
*! Name      : lfGetEqv
*! Developer : Reham Al-Allamy
*! Date      : 04/26/99
*! Purpose   : Function to get the equevelant cost for cost items.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetEqv()
*!*************************************************************
*
FUNCTION lfGetEqv
PARAMETERS lcUpdCsts,lnPRate1,lnDRate2,lnCurUnt1,lnCurUnt2,;
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
*! Developer : Reham Al-Allamy
*! Date      : 04/26/99
*! Purpose   : Function to get the equevelant cost for cost items.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvEquCost()
*!*************************************************************
*
FUNCTION lfvEquCost
PARAMETERS lcCstNo,lnFrnCost,lnCurRate,lnCurUnt

lnCstType = lcIType&lcCstNo
IF lnCstType $ 'PMD'
  STORE '' TO lcPMethod,lcPUnMeth,lcDMethod,lcDUnMeth
  IF lnCstType='P'
    lcPMethod = gfGetExSin(@lcPUnMeth , POSHDR.cPriceCur)
    lcPMethod = IIF(EMPTY(lcPMethod) , '*' , lcPMethod)
    lcPUnMeth = IIF(EMPTY(lcPUnMeth) , '/' , lcPUnMeth)
    lnEquCost = lnFrnCost &lcPMethod lnCurRate &lcPUnMeth lnCurUnt
  ELSE
    lcDMethod = gfGetExSin(@lcDUnMeth , POSHDR.cDutyCur)
    lcDMethod = IIF(EMPTY(lcDMethod) ,'*' , lcDMethod)
    lcDUnMeth = IIF(EMPTY(lcDUnMeth) ,'/' , lcDUnMeth)
    lnEquCost = lnFrnCost &lcDMethod lnCurRate &lcDUnMeth lnCurUnt
  ENDIF
ELSE
  lnEquCost = lnFrnCost
ENDIF
lnEquCost = ROUND(lnEquCost,2)
RETURN (lnEquCost)

*!*************************************************************
*! Name      : lfCallRep
*! Developer : Reham Al-Allamy
*! Date      : 04/27/99
*! Purpose   : Function to print the receiving log report.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfCallRep()
*!*************************************************************
*
FUNCTION lfCallRep

SELECT (lcTmpLine)
SCAN FOR TranCd = "2"
  =SEEK('P'+&lcTmpLine..Cuttkt,'POSHDR')
  =SEEK('P'+&lcTmpLine..Cuttkt+&lcTmpLine..Style+STR(&lcTmpLine..nLineNo,6)+"1",'POSLN')
  SELECT POSLN
  *-- Add the original line in the file to be printed.
  SCATTER MEMVAR
  SELECT (lcTmpPoLn)
  APPEND BLANK
  GATHER MEMVAR
  REPLACE nLanPrRat  WITH 1 ;
          nLanDuRat  WITH 1 ;
          TotStk     WITH TotQty ;
          lAutoMode  WITH .T. ;
          nLan_CST1  WITH nCost1 ;
          nLan_CST2  WITH nCost2 ;
          nLan_CST3  WITH nCost3 ;
          nLan_CST4  WITH nCost4 ;
          nLan_CST5  WITH nCost5 ;
          nELanCost1 WITH nCost1 ;
          nELanCost2 WITH nCost2 ;
          nELanCost3 WITH nCost3 ;
          nELanCost4 WITH nCost4 ;
          nELanCost5 WITH nCost5
  
  *-- Add the received line in the file to be printed.
  APPEND BLANK
  GATHER MEMVAR
  REPLACE TranCd     WITH "2" ;
          nLanPrRat  WITH 1 ;
          nLanDuRat  WITH 1 ;
          nLan_CST1  WITH nCost1 ;
          nLan_CST2  WITH nCost2 ;
          nLan_CST3  WITH nCost3 ;
          nLan_CST4  WITH nCost4 ;
          nLan_CST5  WITH nCost5 ;
          nELanCost1 WITH nCost1 ;
          nELanCost2 WITH nCost2 ;
          nELanCost3 WITH nCost3 ;
          nELanCost4 WITH nCost4 ;
          nELanCost5 WITH nCost5
  
  *-- collect the received qty.
  SELECT (lcTmpLine)
  SCATTER FIELDS Style,Dyelot,cWareCode,Reference,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,cStyGrade TO laFields
  *-- Add the received qty.
  SELECT (lcTmpPOLn)
  GATHER FROM laFields FIELDS Style,Dyelot,cWareCode,Reference,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,cStyGrade
  SELECT (lcTmpLine)
ENDSCAN

*-- lnSel=(1-> View log , 2-> print log , 3-> post , 4-> cancel)
lnSel = 0
*-- after view/print do the same screen for print/view or post or cance.
DO WHILE lnSel < 3
  *** Do you wish to print/view {reciepts} log before posting all transactions? ***
  *** < Preview Log > - < Print Log > - < Post > - < Cancel > ***
*B803149,1 SSH 26/03/00 *** Begin *** Fix the incorrect spelling for word 'Receipts'
* lnSel = gfModalGen('QRM34075B34014','DIALOG','reciepts')
  lnSel = gfModalGen('QRM34075B34014','DIALOG','receipts')
*B803149,1 SSH 26/03/00 *** End ***
  IF lnSel = 1 .OR. lnSel = 2
    *-- Start all modifications needed to call POSTYREC report.
    
    *-- Save the current alias.
    lnCurAlias  = SELECT(0)
    *-- Define all variabels needed for POSTYREC report .
    llDyelot    = llStyDye
    lcRPTitle   = ''
    llMultiWh   = llWareHous
    llRPCostDt  = .F.
    llRPFrnCur  = .F.
    lcRpSortBy  = 'P'
    lcRPPoType  = 'A'
    lcRPPrice   = 'C'
    lcRpExp     = '.T.'
    R_Width     = ''
    Qcompany    = gcCom_Name
    lcMajTtl    = gfItemMask("HM")
    lcOGWinTitl = 'P/O Receipts Log'
    *-- Save the record pointer of all files (Style, PosHdr, PosLn, ApVendor)
    lnStyPos  = RECNO('STYLE')
    lnPoshPOS = RECNO('POSHDR')
    lnPoslPos = RECNO('POSLN')
    *-- Save the order of POSLN file And then close it .
    lcPosOrd  = ORDER('POSLN')
    USE IN POSLN
    *-- Save the record pointer of TMPLINE file And save it's order.
    lnTmpPos  = RECNO(lcTmpLine)
    lcTmpOrd  = ORDER(lcTmpLine)
    *-- Creat the requierd index the POSTYREC report need for this file:
    *-- [cstytype+po+style+STR(lineno,6)+trancd] then use it again with alias POSLN.
    USE (gcWorkDir+lcTmpPOLn) IN 0 AGAIN ORDER TAG POSLN ALIAS POSLN
    llDoRep = .T.
    
    *-- Run Report.
    DECLARE laWareCodes[1]
    STORE "" TO laWareCodes
    
    IF lnSel=2
      gcDevice     = "PRINTER"
      lcOGPlatForm = "DOS"
      RELEASE WINDOW (lcTmpBrow)
      IF !pSetup(.T.,.F.)
        llDoRep = .F.
        lnSel   = 4
      ENDIF
    ELSE
      gcDevice = "SCREEN"
    ENDIF
    
    IF llDoRep
      FOR lnNo = 1 TO ALEN(laWareCodes,1)
        =gfSetDevice()
        *-- Call the "postyrec" report.
        DO (gcRepHome+'\POSTYREC')
      ENDFOR
    ENDIF
    
    *-- Restore record pointers.
    SELECT STYLE
    IF BETWEEN(lnStyPos,1,RECCOUNT("STYLE"))
      GO lnStyPos
    ENDIF
    
    SELECT POSHDR
    IF BETWEEN(lnPoshPOS,1,RECCOUNT("POSHDR"))
      GO lnPoshPOS
    ENDIF
    
    *-- Close the temp. file in the second alias & open master po lines and
    *-- restore its record pointer & order.
    USE IN POSLN
    USE (gcDatadir+'POSLN') IN 0 SHARED
    SELECT POSLN
    SET ORDER TO TAG &lcPosOrd
    IF BETWEEN(lnPoslPos,1,RECCOUNT("POSLN"))
      GO lnPoslPos
    ENDIF
    
    *-- Restore the record pointer in temp. lines file.
    SELECT(lnCurAlias)
    IF BETWEEN(lnTmpPos,1,RECCOUNT(lnCurAlias))
      GO lnTmpPos
    ENDIF
    *-- Call the browse function to restore the browse displaying.
    =lfwmainBrw()
  ENDIF
ENDDO

*-- If the choice was cancel, return without save.
IF lnSel = 4
  STORE .F. TO llShow,llCSave
ENDIF

*C101704,1 [Start]
*!**************************************************************************
*! Name      : lfPicture  
*! Developer : Sameh Aldesouki 
*! Date      : 01/17/2000
*! Purpose   : Change Picture of PO.
*!**************************************************************************
FUNCTION lfPicture

IF lcTranTyp <> 'M' AND llGenPo
  RETURN "@! XXXXXX" 
ELSE
  RETURN "@! X99999"
ENDIF  
*C101704,1 [End]

*!*************************************************************
*! Name      : lfActPad
*! Developer : Abdou Elgendi [ABD]
*! Date      : 06/19/2000
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
*E500331,1 
FUNCTION lfActPad

DEFINE PAD _INQUIRY OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
SET SKIP OF PAD _INQUIRY OF _MSYSMENU EOF(lcTmpRecv)
ON PAD _INQUIRY OF _MSYSMENU ACTIVATE POPUP _INQURYPOP
DEFINE POPUP _INQURYPOP MARGIN SHADOW
DEFINE BAR 1  OF _INQURYPOP PROMPT '\<Show Style Summary Total'
DEFINE BAR 2  OF _INQURYPOP PROMPT '\<Batch Summary Total'
ON SELECTION POPUP _INQURYPOP DO lpvInquiry

*-- End Of lfActPad.
*!*************************************************************
*! Name      : lpvInquiry
*! Developer : Abdou Elgendi [ABD]
*! Date      : 06/19/2000
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpvInquiry()
*!*************************************************************
*E500331
FUNCTION lpvInquiry

DO CASE
  CASE BAR() = 1    
    *-- Show Style Summary Total Screen.
    llSeltscrn = !llSeltscrn
  	=lfShwStyls()    
  CASE BAR() = 2      && Object Link
    *-- Show Batch Summary Total Screen.
    =lfwBrowBtS()
ENDCASE

*-- End Of lpvInquiry.
*!*************************************************************
*! Name      : lfShwStyls
*! Developer : Abdou Elgendi [ABD]
*! Date      : 06/19/2000
*! Purpose   : Show Style Summary Total screen.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfShwStyls()
*!*************************************************************
*E500331
FUNCTION lfShwStyls

IF llSeltscrn
  SHOW GET ibtkt    DISABLE
  SHOW GET lcCuttkt DISABLE  
  SHOW GET ldDate   DISABLE
  SHOW GET lcDesc   DISABLE
  SHOW GET pbNewCt  DISABLE
  IF cbByCrtn
    *-- Enable carton object & disable CT or PO.
    SHOW GET lcCarton DISABLE
    SHOW GET lcShip    DISABLE
    SHOW GET ibShip    DISABLE
  ENDIF
  HIDE WINDOW (lcRcAll0)
  HIDE WINDOW (lcRcAll1)
  ACTIVATE WINDOW (lcRcAll4)
  SELECT (lcTmpRecv)
  SET RELATION TO STYLE INTO &lcTempRsLn ,;
                  STYLE INTO &lcAgnStyle 
  
  SELECT (lcAgnStyle)
  SET RELATION TO 'S' + Scale INTO SCALE 
   =lfshplins()
ELSE
  HIDE WINDOW (lcRcAll4)
  ACTIVATE WINDOW (lcRcAll0)
  ACTIVATE WINDOW (lcRcAll1)
  SHOW GET ibtkt    ENABLE
  SHOW GET lcCuttkt ENABLE
  SHOW GET ldDate   ENABLE
  SHOW GET lcDesc   ENABLE
  SHOW GET pbNewCt  ENABLE
  IF cbByCrtn
    SHOW GET lcCarton ENABLE     
    SHOW GET lcShip    DISABLE
    SHOW GET ibShip    DISABLE
  ELSE
    SHOW GET lcShip    ENABLE
    SHOW GET ibShip    ENABLE
  ENDIF
  
  SELECT (lcTmpRecv)
  SET RELATION OFF INTO &lcTempRsLn
  SET RELATION OFF INTO &lcAgnStyle
  SELECT (lcAgnStyle)
  SET RELATION OFF INTO SCALE 
ENDIF

SET MARK OF BAR 1 OF _INQURYPOP TO llSeltscrn  

*-- End OF lfShwStyls.
*!*************************************************************
*! Name      : lfshplins
*! Developer : Abdou Elgendi [ABD]
*! Date      : 06/19/2000
*! Purpose   : Refresh Style Summary Total screen.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfshplins()
*!*************************************************************
*E500331
FUNCTION lfshplins

IF llSeltscrn
  lcMajor    = LEFT(&lcTmpRecv..Style,lnstylelen)
  lcNonMjr   = SUBSTR(&lcTmpRecv..Style,lnstylelen+2,lncolorlen)
  lcStylsDsc  = STYLE.Desc1
  =lfRefresh(lcRcAll4)
ENDIF

*--End Of lfshplins.
*!*************************************************************
*! Name      : lfwBrows
*! Developer : Abdou Elgendi [ABD]
*! Date      : 06/19/2000
*! Purpose   : Show Batch Summary Total.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwBrows()
*!*************************************************************
*E500331
FUNCTION lfwBrowBtS
Private lnAlias

lnAlias = SELECT(0)

*-- Show Batch Summary Total Screen.
SELECT (lcTempRsLn)
GO TOP
lcSaveBrow  = lcBrFields
*-- Set the browse fields.
lcBrFields = [STYLE:20:H="Style ",cStyDesc:50:H="Description",]+;
             [nPototqty:10:H="Orginal",nRecTotqty:10:H="Received",]+;
             [nBalTotqty :10:H="Balance"]
lcOld_ttl   = lcFile_ttl
DECLARE laBathsry[1]
laBathsry [1] = style
lcFile_ttl  = "Batch Summary Total"
*-- call the global browse to browse the available name drops for the current account+store.
=ARIABROW("",lcfile_ttl,;
            gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'',;
            'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
    
lcBrFields = lcSaveBrow
lcFile_ttl = lcOld_ttl

SELECT (lcTmpRecv)
GOTO TOP
SELECT(lnAlias)
*-- End Of lfwBrows.
*!*************************************************************
*! Name      : lfAddLns
*! Developer : Abdou Elgendi [ABD]
*! Date      : 06/19/2000
*! Purpose   : Add Lines In temp File for recive lines.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfAddLns()
*!*************************************************************
*E500331
FUNCTION lfAddLns
PARAMETERS llSendUPC
Private lnAlias , lcSizes

IF TYPE('llSendUPC') = "U"
  llSendUPC = .F.
ENDIF
lnAlias = SELECT(0)
SELECT (lcTempRsln)
IF SEEK(&lcTmpRecv..Style)
  =SEEK('Total')
  REPLACE nRecTotqty WITH nRecTotqty + 1         ,;
          nBalTotqty WITH nPoTotqty  - nRecTotqty
  SET SKIP OF PAD _INQUIRY OF _MSYSMENU .F.          
ENDIF
=SEEK(&lcTmpRecv..Style)
lcSizes = IIF(llSendUPC,ALLTRIM(STYLEUPC.SIZE) , &lcTmpRecv..SIZE)
REPLACE cStyDesc         WITH lcStyDesc                        ,;
        nRecQty&lcSizes  WITH nRecQty&lcSizes + 1              ,;
        nRecTotqty       WITH nRecTotqty      + 1              ,;
        nBalaqty&lcSizes WITH nPOqty&lcSizes  - nRecQty&lcSizes,;
        nBalTotqty       WITH nPoTotqty       - nRecTotqty         
IF llSeltscrn
  =lfshplins()
ENDIF
SELECT(lnAlias)

*-- End Of lfAddLns.
*!*************************************************************
*! Name      : lfSelSize
*! Developer : Abdou Elgendi [ABD]
*! Date      : 06/28/2000
*! Purpose   : select style size if style was selected from browse.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfSelSize()
*!*************************************************************
*E500331
FUNCTION lfSelSize

DEFINE WINDOW lcstysize AT  0.000, 0.000 SIZE 4.923,41.000 ;
		TITLE "Select Size" FONT "MS Sans Serif", 8 ;
		FLOAT CLOSE NOMINIMIZE SYSTEM COLOR RGB(,,,192,192,192)
MOVE WINDOW lcstysize CENTER

ACTIVATE WINDOW lcstysize
@ 1.615,15.800 GET lnStySizes ;
	PICTURE "@^" ;
	FROM laStySizes ;
	SIZE 1.538,11.833 ;
	DEFAULT 1 ;
	FONT "MS Sans Serif", 8 ;
	 STYLE "B" ;
	VALID lfvSelSize()
@ 1.923,7.400 SAY "Size :"  ;
	FONT "MS Sans Serif", 8 ;
	 STYLE "BT"
@ 0.385,1.200 TO 4.154,38.800 PEN 1, 8
@ 0.462,1.400 TO 4.231,39.000 PEN 1, 8 COLOR RGB(255,255,255,,,,)
READ CYCLE MODAL
RELEASE WINDOW lcstysize

*-- End OF lfSelSize.
*!*************************************************************
*! Name      : lfvSelSize
*! Developer : Abdou Elgendi [ABD]
*! Date      : 06/28/2000
*! Purpose   : Vaild style size.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvSelSize()
*!*************************************************************
*E500331
FUNCTION lfvSelSize     &&  lnStySizes VALID
CLEAR READ

*- End Of lfvSelSize
*!*************************************************************
*! Name      : lfAdjustPo
*! Developer : Abdou Elgendi [ABD]
*! Date      : 06/28/2000
*! Purpose   : Vaild style size.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfAdjustPo()
*!*************************************************************
*E500331
FUNCTION lfAdjustPo
PRIVATE lnAlias
lnAlias = SELECT(0)

SELECT (lcTempRsln)
IF SEEK('Total')
  REPLACE nRecTotqty WITH nRecTotqty + 1         ,;
          nBalTotqty WITH nPoTotqty  - nRecTotqty
ENDIF

=SEEK(&lcTmpRecv..Style,'STYLE')
lcSizes = &lcTmpRecv..SIZE
IF !SEEK(&lcTmpRecv..Style)
  SELECT (lcTempRsln)
  APPEND BLANK
ENDIF

REPLACE STYLE      WITH &lcTmpRecv..Style ,;
        cStyDesc   WITH STYLE.Desc1       ,;
        nPoTotqty  WITH  0                ,;
        nRecTotqty WITH  nRecTotqty + 1   ,;
        nBalTotqty WITH  nBalTotqty - 1
        
  FOR I = 1 To 8 
   lcCount = STR(I,1) 
   REPLACE nPoqty&lcCount    WITH 0                                              ,;
           nRecQty&lcCount   WITH nRecQty&lcCount  + IIF(lcCount = lcSizes,1,0)  ,;
           nBalaqty&lcCount  WITH nBalaqty&lcCount + IIF(lcCount = lcSizes,-1,0)
  ENDFOR

SET SKIP OF PAD _INQUIRY OF _MSYSMENU .F.

SELECT(lnAlias)
*- End Of lfAdjustPo.


*B604581,1 SSH 06/24/2002 ADD DIVISION IN THE PCS OUT SCREEN.
*!*************************************************************
*! Name      : lfCheckDiv
*! Developer : Ahmed Salah Shalaby [SSH]
*! Date      : 06/28/2002
*! Purpose   : Check style divisoin.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Logic
*!*************************************************************
*! Example            : =lfCheckDiv()
*!*************************************************************
*
FUNCTION lfCheckDiv
PARAMETER lcDiv , lcStyle
RETURN((SEEK(lcStyle,"Style") .AND. ALLTRIM(Style.cDivision) == ALLTRIM(lcDiv)))
*B604581,1 SSH 06/24/2002 ADD DIVISION IN THE PCS OUT SCREEN.

*:**************************************************************************
*:* Name        : lfMClick
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/09/2004
*:* Purpose     : Activate Remove Button if mouse is on it
*:***************************************************************************
*:* Called from : screen
*:***************************************************************************
FUNCTION lfMClick
*B037997,1   TMI 
PRIVATE lnCl,lnRmRw,lnNwRw
lnCl = 75.375
lnRmRw = 5.000
lnNwRw = 1.500
IF MDOWN()
  DO CASE
  CASE BETWEEN(MCOL(),lnCl-1.5,lnCl+1.5) AND BETWEEN(MROW(),lnRmRw-1.5,lnRmRw+1.5)
    _CUROBJ = OBJNUM(pbRemLn)
    =lfvRemLn()
  CASE BETWEEN(MCOL(),lnCl-1.5,lnCl+1.5) AND BETWEEN(MROW(),lnNwRw-1.5,lnNwRw+1.5)
    _CUROBJ = OBJNUM(pbRemLn)
    =lfvNewLn()
  ENDCASE
ENDIF

*-- end of lfMClick.