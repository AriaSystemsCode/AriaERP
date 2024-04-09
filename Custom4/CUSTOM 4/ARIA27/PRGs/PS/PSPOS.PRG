*:****************************************************************************
*: Program file  : PSPOS.prg
*: Program desc. : Point of Sale.
*: For screen    : PSPOS.SPX
*:         System: Point of Sale.
*:         Module: PS
*:      Developer: TAK - Timour Abdalla Khalil 
*:****************************************************************************
*: Calls     : lpShow,lfActBrow,lfwBrows,lfvBrows,lfDspSty,lfTrapKy
*:             lfReadAct,lpTab,lpShiftTab .
*:             lfChkUnComS,lfCrUnSess,lfGetTmpFile,lfUpdUVars
*:             lfvType,lfvMis,lfvCustomer,lfvName,lfvPhone,lfGetInfo
*:             lfClrInfo,lfvCustInfo,lfvCIOk,lfvState,lfOldValue .
*:             lfvProceed,lfvSLogOut,lfvUsrPass,lfvStyle,lfAppndLn
*:             lfvRemove,lfvEdit,lfActPassw,lfvUsr_Pass,lfvUsr_Id .
*:             lfvPrice,lfvDiscnt,lfvDscntP,lfvTotQty,lfvTax
*:             lfvTaxP,lfvllTax .
*:             lfvPayTrm,lfwPayTrm,lfvPaid,lfvMltPay,lpTab2,lpBkTab2
*:             lfPayLines,lfwPyBrow,lfvNewPay,lfvMPatTrm,lfvMPatAmt
*:             lfvMPatRef,lfvRemPay .
*:             lfvInvNo,lfvCCrdit,lfCrdBrow,lfvTrnTyp,lfwSrBrow,lpTab1
*:             lpBkTab1,lfTrapSrKey,lfvCrdtCode,lfvNewCrd,lfRemCrd .
*:             lfUpdSale,lfUpdRent,lfUpdARHst,lfCrtTpFls
*:             lfUpdInvFl,lfUpdRetFl .
*:****************************************************************************
*: Passed Parameters  : ............
*:****************************************************************************
*: Returns            : ............
*:****************************************************************************
*: Modifications :
*B602138,1 TAK 11/02/1998 Added an option to reprint receipt.
*B602138,1                Added new setup for set confirm.
*B602179,1 AMM 11/25/1998 Erase temporary files when quitting the program
*E301077,13 TAK 01/05/99  Redeuce the number of opened files when start running 
*E301077,13               the progrm and open it with object when needed.
*E301116,1 TAK 01/07/1999 Check if tranDate is valid for discount date range. 
*B801994,1 SSH 02/27/1999 Calculate change amount only for cash amount not 
*B801994,1 SSH            for all paid amount.
*E500241,1 SSH 03/05/1999 Give the user the ability to invoice
*E500241,1 SSH            and return merchandise on the point of sale system
*E500241,1 SSH            using the style UPC number from EDI module.
*E500241,1 SSH            By adding a new setting in the setup file.
*E500241,1 SSH            (Style/UPC) which return memory variable(M_STYLEUPC)
*E500241,1 SSH            (S|U) and use this memory variable in the prgs.
*E500241,1 SSH            to scan for either style or upc number.And change
*E500241,1 SSH            some text on the screen(PSPOS3) to print
*E500241,1 SSH            "Style - Color" or "UPC Number" as header.
*E500241,1 SSH            And call the function gfSetup to close all file
*E500241,1 SSH            used in this program.
*B802284,1 SSH 23/05/1999 1-Give the user the ability to scan style or upc
*B802284,1 SSH              on the same session by adding athied choice
*B802284,1 SSH              in the module setup to scan both style and upc
*B802284,1 SSH              if the user chosse to scan style And Upc he/she
*B802284,1 SSH              will be able to switch between style & Upc in the
*B802284,1 SSH              same session.
*B802284,1 SSH            2-Open the cash drawer when user press Proceed Button
*E301245,1 SSE 06/09/1999 Added a new field nHgWtrMark and calcualte its value
*E301245,1                from NETBAL field , if less than NETBAL update with 
*E301245,1                NETBAL else leave it with existing value
*E301264,1 TAK 06/17/1999 1-Make sure that the return lines are validated from invoice.
*E301264,1                2-Dont allow to sale what not in stock.
*E301264,1                3-Prevent the user from selling items with 0 price.
*E301264,1                4-Add an access for cash return (refund).
*E301264,1                5-Default paid amount (refund) to zero in case of retrun.
*E301282,1 TAK 07/04/1999 1-Allow remove or Edit line due to user process access.
*E301282,1                2-Add automatic keyoff.
*E301249,1 IHB 06/22/1999 Make priority field C(3) in CUSTOMER file.
*B603091,1 TAK 08/03/1999 Fix bug of wrong customer balance calculation.
*B603091,1                Fix bug in uncomplete session cycle.
*B603091,1                Make the invoice number the current object in returns.
*B603105,1 TAK 08/11/1999 Take the warehouse from coresponding site id .
*B802525,1 TAK 08/29/1999 Check that the miseleneus returnd invoice does not used in
*B802525,1                preivus return for specific customer.
*B802525,1                Added ability to select size if style selected from browse. 
*C101660,1 WAB 09/16/1999 Sales Tax should be editable and Ability to enter/edit freight 
*B603192,1 TAK 10/05/1999 Added to default tax amount from customer.
*B603192,1                Fixed bug of not displaying states in customer information.
*B802698,1 TAK 10/12/1999 Fixed wrong select size popup display in resolution is not standard.
*B802803,2 MAN 11/21/1999 Credit Memo became part of the POS ... Remove Chk on the RM Module
*B802834,1 KHM 11/30/1999 Fix the bug of invalid subscript out of bound.
*C101715,1 KHM 11/30/1999 Creating a new customized form .
*B603303,1 TAK 03/19/2000 Fixed bug "lcUpcpo variable not found".
*B603531,1 TAK 03/28/2000 Fixed bug Variable not found in KeyOff GL updating.
*B603687,1 ABD 06/28/2000 Fix the Following Bugs :-
*B603687,1 ABD            1) Message splling error in message no 52007 in syddlobj.
*B603687,1 ABD            2) Invoice items filtering error in return transaction.
*B603687,1 ABD            3) disapearing tax amount in return transaction.
*B603687,1 ABD            4) Fix problem in return invoice quantity.
*B603733,1 WMA 07/09/2000 Fix bug "return cost error"
*E301467,1 TAK 14/09/2000 1-Add ability to integrate with Atomic Credit Cards verification.
*E301467,1                2-Add new setups under PS module. 
*B603713,9 NAD 10/22/2000 Increase the price and amount fields width in the browse. 
*E301508,1 WAM 11/16/2000 Send Sales to Back Office
*C102047,1 ABD 11/29/2000 Handle discounts in case multi location get the discounts from stydye.
*C102047,1 ABD            upon add new related file about whole sale or retail sale.
*B604071,1 ABD 01/02/20001 Check if NC module is not installed Get the def. location from 
*B604071,1 ABD             PS setups file.
*C200145,1 ABD 01/08/2001 Add new trigger for custom RSH10 that have 6 chr for
*C200145,1                the UPC number in the Styleupc file.
*B803961,1 ABD 01/10/2001 Add new 2 fields to the temp file to hold the 
*B803961,1 ABD            Credit Card number and the approval number,
*B803961,1 ABD            in case the customer buy with  credit card,
*B803961,1 ABD            And print them in the customize form for Eagle's Eye.
*B803962,1 ABD 01/10/2001 1-Give the user the ability to change the "Credit Card"
*B803962,1 ABD              entry method. If that on each sale level but we will 
*B803962,1 ABD              allow to "Administrators" only to do that.
*B803962,1 ABD            2-Add new setup to ask the user to display the amount tender or not.
*B803962,1 ABD            3-Add New setup to prevent the user to Sale or return with amount
*B803962,1 ABD              less than the total amount for this sales or return.
*B803962,1 ABD            4-Add New setup to return the sale and display the total amount
*B803962,1 ABD              In the Paid amount or not.
*B604185,1 WAB 02/08/2001 1- POS Screen get the wromg price for style in case of UPC     
*B604185,1 WAB            2- the popup to swatch between the UPC and style show one empty 
*B604180,1 ABD 02/11/2001 1- Disable filed payment amount in case do nont leave 
*B604180,1 ABD               Opend amount setups.
*B604180,1 ABD            2- change related filed llCash from 2 cases (Cash or not cash) to 
*B604180,1 ABD               be cash , not cash or Credit card that will give the 
*B604180,1 ABD               system to different Between them.
*B804026,1 ABD 02/15/20001 Fix problem that in case of return with 
*B804026,1 ABD             Miscelleneous account you should not complete the save 
*B804026,1 ABD             In case of paid amount did't equal the total paid amount.
*B604248,1 ABD 02/21/2001  1) Fix Problem that the field Additional discount is showing
*B604248,1 ABD              The default code instead of having it defaulted to "N/A" .
*B604248,1 ABD             2) Fix another problem has to do with the add. retail discount 
*B604248,1 ABD              Which is that when you select a discount from the popup, 
*B604248,1 ABD              The field becomes not editable and you cannot change your selection.
*E301557,1 AHM 02/07/2001 Send the customer information through EDI, Organizational Relationship
*E301557,1                Transaction
*B604253,1 ABD 02/26/2001 1) Fix bug that the program allow the user to select invoice that 
*B604253,1 ABD               Create from SO Or AR.
*B604253,1 ABD            2) Fix bug that the program didn't allow to change the additional 
*B604253,1 ABD               Discount in case of made exchange.
*B604253,1 ABD            3) Don't accept any customer with status not active
*C200153,1 ABD 02/27/2001 Need to apply the discount entered in customer file in Point 
*C200153,1 ABD            of Sale, same as Additional discount is Applied
*B804017,1 ABD 03/13/2001 Fix Problem that the discount perc. didn't accu. the discount 
*B804017,1 ABD            In case We leave the date empty.
*B#604352,1 SSH Alwayes replace the sales with -ve and return with +ve
*B804145,1 ABD 04/29/2001 remove the CbnkCode filed from the array that 
*B804145,1 ABD            Create the Arhist file.
*B604544,1 SSH Direct the out put to module setup port.
*B604544,1 SSH add the tax automatically
*B604522,1 MAN 07/08/2001 Fix Un-Saved Debit record upon saving a misc. sale that has been 
*B604522,1 MAN            switched to permanent customer since partial payment was done.
*B604522,1 MAN            Don't Proceed with saving in cas the user choosed to switch from misc. 
*B604522,1 MAN            to permanent customer, the reason for that: sometimes the permanent 
*B604522,1 MAN            customer has additional discount which changes the transation amounts,
*B604522,1 MAN            and therefore the cashier must this new values first.
*B604522,1 MAN            If the customer has a default rep, the program should stand on the rep
*B604522,1 MAN            object to alert the user with the default rep.
*B604522,1 MAN            In case if Exchange and there is a refund the refund amount was save with
*B604522,1 MAN            wrong sign '-' while it should be '+'
*B604522,1 MAN            Calculate Tax amount when the customer change because the additional 
*B604522,1 MAN            discount changes and therefore taxes should be recalculated
*C200222,1 SSH            Get the customer code from magnitic card.
*C200365,1 SSH 07/15/2002 Check if the customer post the previous transactions to Back Office.
*C200365,1 SSH 07/15/2002 if not do not proceed.
*B606121,1 SSH            Fix some problems in the CP#200222.
*B606415,1 SSH Fix problems in Paid Amount
*E037853,1 HBG 02/16/2004 Change the width of Key field in EDITRANS to 40 char 
*B124698,1 NNA 10/28/2004 Fix 2 bugs that when etering a valid or invalid style the browser open 
*B124698,1 NNA            in the Two Cases , the Exchange check box is disable even user select Return
*B127122,1 MHM 04/12/2005 Remove dist_ctr field from temp INVHDR file while it is now a standard field 
*B128282,1 NNA 07/17/2005 fix bug that sales tax not booking correctly in the return Transaction that when you generate
*B128282,1 NNA            an invoice for account that has link Code <> "DEFDEF" then when you return this invoice
*B128282,1 NNA            the link code is saved as "DEFDEF".
*C127532,1 EIH 07/20/2005 Make point of sale working with default warehouse of Cash Register 
*C127532,1 EIH            For logged user .
*:***************************************************************************
*E500241,1 SSH 03/05/1999 Call the function gfSetup.And open some nessesary
*E500241,1 SSH            files.
IF !gfSetup()
  RETURN
ENDIF

*C200365,1 SSH 15/07/2002 Check if the customer post the previous transactions to Back Office.
*C200365,1 SSH 15/07/2002 if not do not proceed.
IF ASCAN(laEvntTrig , PADR('CHKPOST',10)) <> 0
  PRIVATE llFndPrv
  llFndPrv = gfDoTriger('PSPOS',PADR('CHKPOST',10))
  IF llFndPrv
    RETURN
  ENDIF
ENDIF
*C200365,1 SSH 15/07/2002 Check if the customer post the previous transactions to Back Office.
*C200365,1 SSH 15/07/2002 if not do not proceed.

=gfOpenFile(gcDataDir+'UNCMSESS' ,'TRANS','SH')
=gfOpenFile(gcDataDir+'CODES' ,'CODES','SH')
=gfOpenFile(gcDataDir+'SCALE' ,'SCALE','SH')
*E500241,1 SSH (END).

*--Check if the Point of sale system was setup.
*E500241,1 SSH 03/05/1999 Add the following part to call gfGetMemVar
*E500241,1 SSH            just one time and pass an array as parameter
*E500241,1 SSH            to get all memory variable .
*E301467,1 (Start) integrate with Atomic (added new setups).
*DIMENSION laSetups[11,2]

*B803962,1 ABD Add new  3 setups in the PS setups.  [Begin]
*DIMENSION laSetups[15,2]
DIMENSION laSetups[18,2]
*B803962,1 ABD [End]

*E301467,1 (End).
laSetups[1,1] = 'M_SYSTYPE'
laSetups[2,1] = 'M_STYLEUPC'
laSetups[3,1] = 'M_LINK_GL'
laSetups[4,1] = 'llMulCurr'
laSetups[5,1] = 'llEditExRa'
laSetups[6,1] = 'M_CONFIRM'
laSetups[7,1] = 'M_MSCACCNT'
*E301264,1 Added 3 new setups.
laSetups[8,1] = 'M_ALWRETRN'  && Allow return without invoice.
laSetups[9,1] = 'M_ACPZROPC'
laSetups[10,1]= 'M_ACPZRSTK'
laSetups[11,1] = 'XAGINGTYPE'
*E301264,1 End.
*E301467,1 (Start) integrate with Atomic (added new setups).
laSetups[12,1] = "M_USEATOM"
laSetups[13,1] = "M_ATOMDIR"
laSetups[14,1] = "M_ENTMETH"
laSetups[15,1] = "M_ACTNTYPE"
*E301467,1 End.

*B803962,1 ABD Add new setup to ask the user to display 
*B803962,1 ABD the amount tender or not. [Begin]
laSetups[16,1] = "M_DSAMNTNC"
*-- Add New setup to prevent the user to Sale or return with amount
*-- less than the total amount for this sales or return.
laSetups[17,1] = "M_ALLPAMNT"
*-- Add New setup to return the sale and display the total amount
*-- In the Paid amount or not. [Begin]
laSetups[18,1] = "M_ALLRTAMT"
*B803962,1 ABD [End]

*C101660,1 WAB - GEt SEtting For Display Freight 	
*C101660,1 WAB - START

llDispFrght = gfGetMemVar('M_DSPFRGHT')
IF llDispFrght
  lnFreight = 0				&& Freight amount
  lnRtFrght = 0
  lcCntFrght = 'DISABLE'		&& for disable or enable freight butt.
  llNoFreight = .F.
ENDIF
*C101660,1 WAB - END

gcCurSite = gfGetMemVar('M_CURRSITE')

*B803961,1 ABD - Add 2 Variable to hold the Credit Card number and the approval number.
*B803961,1 ABD - this 2 variable will use in case not multi payment (Specially). [Begin]
STORE '' To lcCrdData , lcAppDesc
*B803961,1 ABD - [End]

=gfGetMemVar(@laSetups,gcAct_Comp)
*lcSysTyp = gfGetMemVar('M_SYSTYPE')
lcSysTyp = laSetups[1,2]
*E500241,1 SSH (END)

*B803962,1 ABD Add new setup to ask the user to display 
*B803962,1 ABD the amount tender or not. [Begin]
llDsAmntNc = laSetups[16,2]
llALLRTAMT = laSetups[18,2]
llDsAmntNc = IIF(EMPTY(llDsAmntNc),.F.,llDsAmntNc)
llALLRTAMT = IIF(EMPTY(llALLRTAMT),.F.,llALLRTAMT)
laSetups[17,2] = IIF(EMPTY(laSetups[17,2]),.F.,laSetups[17,2])
*B803962,1 ABD [End]


IF lcSysTyp = 'M'
  *--The Point of Sale system was not setup, Cannot proceed.
  =gfModalGen('TRM52001B42000','DIALOG')
  RETURN
ENDIF
*E500241,1 SSH 03/05/1999 Declare variable and get memory fields for style/upc
lcStyUpc = 'UPC Number'
*B802284,1 SSH 23/05/1999 Get the setup variable.
*B802284,1 SSH llUPC = 'A' if the user choose to scan style and upc.
*llUPC = (laSetups[2,2] $ "U")
llUpcObj = (laSetups[2,2] = "A")
llStyBrow = (laSetups[2,2] $ "S")
llUPC = (laSetups[2,2] $ "AU")
lcCashSeq = gfGetMemVar('M_CASHSEQ')
lcCashCom = gfGetMemVar('M_CASHCOM')
llTaxSetup = ( gfGetMemVar('M_TAX')='Y' )

*B802284,1 SSH(END)
IF llUPC
  =gfOpenFile(gcDataDir+'StyleUpc' ,'StyleUpc','SH')
ENDIF
*E500241,1 SSH(END)
*--Window and Browse title definition.
lcWindTitl='Point of Sale'
lcBrowTitl='Sales Details'
lcSRDesc  ='Sale'

*--Initialize screen variables.
DECLARE laCodInfo[1,10],laPayTerm[1,2]
STORE 0    TO lnSr,lnCustAmt,lnTender,lnChg,lnPayTerm,lnRecNo,lnInvAmt,lnSCount
STORE 0.00 TO lnTotal,lnPaid,lnBal
STORE ' '  TO lcCustomer,lcName,lcPhone,lcRefer,lcStyle,;
              laPayTerm,laCodInfo,lcOldValue,lcPyStatus
STORE ' ' TO lcInvoiceNo,lcCrMemoNo,lcInvNo,lcGlSession
STORE ' ' TO lcAddr1,lcAddr2,lcAddr3,lcAddr4,lcAddr5,lcAddr6
STORE .F. TO llMis,llMltPay,llExchange,llOpnCinst
*B801994,1 SSH 02/27/1999 Declare the following two variable
*B801994,1 SSH            1- for the amount paid cash
*B801994,1 SSH            2- Temp. to hold the old change.
lnRelpay = 0.00
lnOldChg = 0.00
*B801994,1 SSH(END).
llUpdOver = .F.

*C102047,1 ABD Define variables. [Begin]
lcSRep    = SPACE(3)  && Hold the Sales rep.
lnCommRep = 0         && Hold the comm. for sales rep.
STORE .F. To llOpnSRep,llBrowse ,llDIV_COMM
*B604248,1 ABD - Remak the line & next variable that we didn't need it now.[Begin]
*STORE .T. TO llFirstTme  && Hold .T. if run the first time
*B604248,1 ABD - [End]

*C200153,1 ABD - Apply the discount entered in customer file in Point 
*C200153,1 ABD - of Sale, same as Additional discount is Applied , def. 
*C200153,1 ABD - 2 variables hold the discount% and True if found  [Begin]
STORE 0 To  lnCustDsc
STORE .F. To  llCustDsc
*C200153,1 ABD - [End]

DECLARE laAddDisc[1,2]
lnAddDisc  = 1
lcAddDisc  = ''
llDIV_COMM = gfGetMemVar('M_REP_COMM')='D'
IF llDIV_COMM
  =gfOpenFile(gcDataDir+'REP_DIV','REP_DIV','SH')
ENDIF

*C102047,1 ABD [End]

*--Temp file initialization.
STORE '' TO lcTmpAR,lcInvHdr,lcInvLine,lcPosTmp,lcRetHdr,lcRetLine


*--Style Code structure.
lcStyHdr  = gfItemMask('HI')
lcItmPict = gfItemMask('PI')
lcStyPict = ''
*E500241,1 SSH 03/05/1999  Add the condition that setup is not UPC.
*B802284,1 SSH Remove If condition
*IF !llUPC
lcStyPict = gfItemMask('PI')+'9'
lcUpcPict  ='XXXXXXXXXXXXXXXXXX'
*ENDIF
*B802284,1 SSH(END)
*E500241,1 SSH(END)
lcPhonPict = gfPhoneTem()

*B603303,1 Added.
*B604185,1 WAB (START)- assign "Upc Number" 1st elemnt in the popup as the enitial elment
*lcUPCPop = ''
lcUpcPop ='Upc Number'
*B604185,1 WAB (END)
*B603303,1 End.

*C200145,1 ABD- Add new trigger for custom RSH10 that have 6 chr for
*C200145,1 ABD- The UPC number in the Styleupc file & we will change 
*C200145,1 ABD- The picture of the field & len of seek exepresion. [Begin]
lnUPCnum = 12
*-- This Array will define after call gfSetup function.
IF ASCAN(laEvntTrig , PADR('LLUPCNUM',10)) <> 0
  lnUPCnum  = 6
  lcUpcPict ='XXXXXX'  
ENDIF     
*C200145,1 ABD - [End]

*--Check of needed Installed modules.
llApLink   = (OCCURS('AP',gcCmpModules)<>0)
llRMInstld = (OCCURS('RM',gcCmpModules)<>0)    

*--Default balance color to black.
lcBalColor = 'RGB(0,0,0,192,192,192)'


llPrintRec = .T.
llChgMisln = .F.

ldPostDate = gdSysDate

*--Check GL Needed variabels.
*E500241,1 SSH 03/05/1999 Commented out to use the array instead of gfGetMemVar
*llGLLink   = (ALLTRIM(gfGetMemVar("M_LINK_GL")) = "Y")
llGLLink   = (ALLTRIM(laSetups[3,2]) = "Y")
*llMulCurr  = gfGetMemVar('llMulCurr')
llMulCurr  = laSetups[4,2]
*llEditExRt = gfGetMemVar('llEditExRa')
llEditExRt = laSetups[5,2]
*llConfirm  = (ALLTRIM(gfGetMemVar("M_CONFIRM")) = "Y")
llConfirm  = (ALLTRIM(laSetups[6,2]) = "Y")
lcSetConf = SET('CONFIRM') 
*E500241,1 SSH (END)
*E301264,1 Added 3 new setups.
llAlowRet = (ALLTRIM(laSetups[8,2])="Y")  && Allow return without inv.
llAcpZrPr = (ALLTRIM(laSetups[9,2])="Y")  && Accept items with 0 price.
llAcpZrSk = (ALLTRIM(laSetups[10,2])="Y")  && Accept items with -ve or 0 Stk.
*E301264,1 End.

*--Array hold files and if opend in this program.
*E301077,13 increase no of files that controled inside the program.
*DIME laFlOpn[11,2]
DIME laFlOpn[14,2]
*E301077,13 End.
laFlOpn = .F.

*--This files will be opened with the program in gfsetup and closed when exit.
*--STYLE,STYDYE,WAREHOUS,POSTRAN,DEBIT,CREDIT,CUSTOMER,ARHIST,ORDLINE,INVHDR,INVLINE
*E500241,1 SSH 03/05/1999 Commented out .
*IF !gfSetup()
*  RETURN
*ENDIF
*E500241,1 SSH(END)

*-- Fiscal year and period variables.
lcGLFYear  = SPACE(1)
lcGLPeriod = SPACE(1)
IF llGLLink
  IF !CheckPrd(ldPostDate,"lcGLFYear","lcGLPeriod",'AJ')
    RETURN
  ENDIF
  laFlOpn[1,1]=gfOpenFile(gcDataDir+'GLDIST' ,'GLDISTAC','SH')
  laFlOpn[1,2]='GLDIST'
  lcLinkCode = "DEFDEF"
ENDIF

*--Currency Information.
lcCurrCode = gcBaseCurr && Variable to hold currency code.
lnExRate   = 1          && Variable to hold exchange rate.
lnCurrUnit = 1          && Variable to hold exchange rate.


*--Uncomplte session variables(UNCSS).
DECLARE laVars[25]
lcAProgm = 'PSPOS'
lcProgID = PADR('PNTOSALE',10)
lnSessNo = gnProgCopy
lcSession = gfsequence('cSession')
STORE ''  TO lcAdTrnSeq,lcAcTrnSeq,lcHisSeq,lcGlSess,lcRepBat
STORE .T. TO llUpdGlDif,llUpdMstGL
lnUnCmSeRc= 0
laVars[1]  = 'lnSr'
laVars[2]  = 'lnTotal'
laVars[3]  = 'lnPaid'
laVars[4]  = 'lcRefer'
laVars[5]  = 'lnPayTerm'
laVars[6]  = 'lcInvoiceNo'
laVars[7]  = 'lcCrMemoNo'
laVars[8]  = 'lnTender'
laVars[9]  = 'lnUnCmSeRc'
laVars[10] = 'lcGlSess'
laVars[11] = 'lcAdTrnSeq'
laVars[12] = 'lcRepBat'
laVars[13] = 'lcAcTrnSeq'
laVars[14] = 'llUpdGlDif'
laVars[15] = 'lcHisSeq'
laVars[16] = 'llUpdMstGL'
laVars[17] = 'lcGlSession'
laVars[18] = 'llExchange'
laVars[19] = 'llmis'
laVars[20] = 'lcPhone'
laVars[21] = 'lcName'
laVars[22] = 'lcCustomer'
laVars[23] = 'llUpdOver'
laVars[24] = 'lnSafes'
laVars[25] = 'lnSCount'
llContinue = .F.
llGoAndChk = .T.
llChkUnCom = .T.

*--Read Temp files names.
lcInvHdr = gfTempName()
lcInvLine= gfTempName()
lcRetHdr = gfTempName()
lcRetLine= gfTempName()
lcTmpAR  = gfTempName()


*--Restore the misellenues account code.
*E500241,1 SSH 03/05/1999 Commented out to use the array instead of gfGetMemVar
*lcMisAcct = gfGetMemVar('M_MSCACCNT')
lcMisAcct = laSetups[7,2]
*E500241,1 SSH(END)
IF EMPTY(lcMisAcct) OR !SEEK('M'+lcMisAcct,'CUSTOMER')
  *--Account : xxxxx is not found in the data file.
  = gfModalGen('TRM00001B00000','ALERT','Miscellaneous account')
  RETURN
ENDIF

*--Open Needed program files.
*B802803,2 The Credit Memo became part of the POS ... No need for this check
*IF llRMInstld
  laFlOpn[2,1]=gfOpenFile(gcDataDir+'RETHDR' ,'RETHDR'  ,'SH')
  laFlOpn[2,2] ='RETHDR' 
  laFlOpn[3,1]=gfOpenFile(gcDataDir+'RETLINE','RETLINE' ,'SH')
  laFlOpn[3,2] ='RETLINE' 
*ENDIF
*--Check the current safe bankCode.
laFlOpn[4,1]=gfOpenFile(gcDataDir+'APBANKS','','SH')
laFlOpn[4,2]='APBANKS'
*E301077,13 comment out and open at save process.
*laFlOpn[5,1]=gfOpenFile(gcDataDir+'APCHECKS','Bankcheck','SH')
*laFlOpn[5,2]='APCHECKS'
*laFlOpn[6,1]=gfOpenFile(gcDataDir+"APPAYMNT","","SH")
*laFlOpn[6,2]='APPAYMNT'
*E301077,13 End.

*--Check the safe users and user safes.
GO TOP IN SYUUser
*E301077,13 Open it in variable since we donot need to close it at quitting.
*laFlOpn[7,1]=gfOpenFile(gcSysHome+'SYCINST','','SH')
llOpnCinst=gfOpenFile(gcSysHome+'SYCINST','','SH')
*E301077,13 End.
*--If there is a users defined in the system and login required.
llUsersExt = !EOF('SYUUser') AND SYCINST.lInsLogRq   
*E301077,13 Close this files since if was not needed any more.
IF llOpnCinst AND USED('SYCINST')
  USE IN SYCINST
ENDIF
*E301077,13 End.

DIME laSafes[1]
laSafes = ''
lnCnt   = 1
lnSafes = 1
SELECT APBANKS
SCAN FOR cBnkType ='S' AND (!llUsersExt OR !EMPTY(mSafeUsers) )
  lcSfUserStr = APBANKS.mSafeUsers
  IF ( !llUsersExt OR gcUser_id $ lcSfUserStr )
    DIME laSafes[lnCnt]
    laSafes[lnCnt] = APBANKS.cBnkCode
    lnCnt = lnCnt + 1
  ENDIF
ENDSCAN
IF EMPTY(laSafes[1])
  *--The user :+gcUser_id+ has no rights to use existing cash registers.
  =gfModalGen('QRM52005B42001','DIALOG',ALLTRIM(gcUser_id))
  RETURN
ENDIF
llSelSafe  = .F.
lcCurrSafe = laSafes[1]

IF ALEN(laSafes) <> 1
  *--User has access to multiple safes.
  lnSafes   = 1
  llSelSafe = .T.
ENDIF


*C102047,1 ABD Check if this user have one safe and not close, 
*C102047,1 ABD Or want to make new sale in last time. [Begin]
IF ALEN(laSafes) = 1
  lnAlias = SELECT (0)
  llTrimmed  = .F.
  =gfOpenFile(gcDataDir+'APCHECKS','Bankcheck','SH')
  =SEEK(laSafes[lnSafes])
  IF !EMPTY(dclosdate)
    IF gdSysDate < dclosdate 
       llTrimmed = .T.
     ELSE
       IF gdSysDate = dclosdate .AND. TIME()  < cclos_time
         llTrimmed = .T.
       ENDIF
    ENDIF
  ENDIF
  =gfCloseFile('APCHECKS') 
  SELECT(lnAlias)
  IF llTrimmed 
    *-- Message No.  : 52019
    *-- Message TEXT : You are trying to generate a transaction on an incorrect date . Check your system date.
    *-- Button  No.  : 00000.
    *-- Button  TEXT : OK
    =gfModalGen('TRM52019B00000','DIALOG')
    RETURN
  ENDIF
ENDIF
*C102047,1 ABD [End]

=gfOpenFile(gcDataDir+'SALESREP','SALESREP','SH')

*E301264,1 Read cash refund subprocess acess.
llAlwRfnd = gfUserPriv('PS','PSPOS','CASHREFUND')
*E301264,1 End.
*E301282,1 Start.
llDelAses = gfUserPriv('PS','PSPOS','ALLOWREMOV')
llEdtAses = gfUserPriv('PS','PSPOS','ALLOWEDIT')
*E301282,1 End.

llCMInstld = (OCCURS('CM',gcCmpModules)<>0)    
*--Get the current site warehouse code.
SELECT WAREHOUS
GO TOP
*B603105,1 Start.
*IF lcSysTyp = 'P' AND llCMInstld
IF !EMPTY(gcCurSite)
*B603105,1 End.
  LOCATE FOR cSiteId = gcCurSite
  IF !FOUND()
    GO TOP
  ENDIF
ENDIF
** M.H
LOCATE FOR WAREHOUS.CSITEID = gcCurSite
** M.H
lcStoreCode = WAREHOUS.cWareCode

*B604071,1 ABD Check if NC module is not installed Get the def. location from PS setups file.
*B604071,1 ABD If empty close the screen and don't run the Point of sale program . [Begin]
IF OCCURS('NC',gcCmpModules) = 0 
  lcDefloc = PADR(gfGetMemVar('M_PSDEFLOC'),6)
  IF EMPTY(lcDefloc)
    *-- Message Test  : xxxxx is not found in the data file.
    *-- Message No.   : 00001
    *-- Button text   : Ok
    *-- Button No.    : 00000
    = gfModalGen('TRM00001B00000','ALERT','Default location')
    RETURN
  ELSE
    lcStoreCode = lcDefloc
    = SEEK(lcStoreCode)
  ENDIF  
ENDIF
*B604071,1 ABD [End]

*E301467,1 (Start) integrate with Atomic.
llUseAtom = (ALLTRIM(laSetups[12,2])="Y")  && Use integration with atomic.
IF llUseAtom
  lcAtomDir = ALLTRIM(laSetups[13,2])
  lcEntMeth = ALLTRIM(laSetups[14,2]) 
  lcActType = ALLTRIM(laSetups[15,2]) 
  lcRefer = SPACE(60)
ELSE
  STORE ' ' TO lcAtomDir,lcEntMeth,lcActType
  lcRefer = SPACE(30)
ENDIF
*E301467,1 End.


*--Clear Information.
=lfClrInfo(.T.)

PUSH KEY
ON KEY
ON KEY LABEL ESC llDumy = .T.
ON KEY LABEL ALT+B     llDumy = lfCntEnd('BROW')
ON KEY LABEL Ctrl+END  llDumy = lfCntEnd('END')
ON KEY LABEL Ctrl+HOME llDumy = lfCntEnd('HOME')
ON KEY LABEL Ctrl+PGUP llDumy = .T.
ON KEY LABEL Ctrl+PGDN llDumy = .T.
ON KEY LABEL Ctrl+W    llDumy = .T.
ON KEY LABEL Ctrl+J    llDumy = .T.
ON KEY LABEL Ctrl+Q    llDumy = .T.

*C200222,1 SSH Declare variables
PRIVATE lnStrTim , lnEndTim , lcTmpNam , llCusScan
lcTmpNam = ""
llCusScan = .F.
lnStrTim = 0
*C200222,1 SSH Function to prepare for scan customer or enter manualy.
=lfPrepCust()
*C200222,1 SSH[End]


*C127532,1 EIH 07/20/2005 Call lfDispWhSC() which display screen to enable user select screen.[Begin]
IF ASCAN(laEvntTrig,PADR('DISPWHSC',10))<>0
   =gfDoTriger(lcProgName,PADR('DISPWHSC',10))
ENDIF
*C127532,1 EIH 07/20/2005 [End]

*--Call the Main Screen.
DO (gcScrDir+gcWinAppl+"\PSPOS.SPX")
POP KEY

*--Close program files.
FOR lnI=1 TO 14
  IF laFlOpn[lnI,1]
    IF !EMPTY(laFlOpn[lnI,2]) AND USED(ALLT(laFlOpn[lnI,2]))
      =gfCloseFile(ALLT(laFlOpn[lnI,2]))
      *USE IN (ALLT(laFlOpn[lnI,2]))
    ENDIF
  ENDIF
ENDFOR

SET CONFIRM &lcSetConf
*--Close all Temp files when exit the program.
glQuitting = .T.
*--And Phisical delete for temprorary files.
USE IN (lcPosTmp)
ERASE (gcWorkDir+lcPosTmp+'.DBF')
ERASE (gcWorkDir+lcPosTmp+'.CDX')

IF USED(lcTmpAR)
  USE IN (lcTmpAR)
  ERASE (gcWorkDir+lcTmpAR+'.DBF')
  ERASE (gcWorkDir+lcTmpAR+'.CDX')
ENDIF
IF USED(lcInvHdr)
  USE IN (lcInvHdr)
  ERASE (gcWorkDir+lcInvHdr+'.DBF')
  ERASE (gcWorkDir+lcInvHdr+'.CDX')
ENDIF
IF USED(lcInvLine)
  USE IN (lcInvLine)
  ERASE (gcWorkDir+lcInvLine+'.DBF')
  ERASE (gcWorkDir+lcInvLine+'.CDX')
  *B602179,1 AMM Erase temporary files when quitting the program start
  ERASE (gcWorkDir+lcInvLine+'.FPT')
  *B602179,1 AMM  end
ENDIF
IF USED(lcRetHdr)
  USE IN (lcRetHdr)
  ERASE (gcWorkDir+lcRetHdr+'.DBF')
  ERASE (gcWorkDir+lcRetHdr+'.CDX')
ENDIF
IF USED(lcRetLine)
  USE IN (lcRetLine)
  ERASE (gcWorkDir+lcRetLine+'.DBF')
  ERASE (gcWorkDir+lcRetLine+'.CDX')
ENDIF
RETURN

*B803962,1 ABD - Remove the new menu option. [Begin]
RELEASE PAD _INQUIRY OF _MSYSMENU
*B803962,1 ABD - [End]

*:*************************************************************
*: Name      : lpShow.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Show Screen objects when Mode Changes.
*:*************************************************************
*: Calls     : 
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lpShow()
*:*************************************************************
FUNCTION lpShow

DO CASE
  *-- S E L E C T   M O D E .
  CASE laScrMode[1]
    *--Check uncomplete session (UNCSS).
    IF llGoAndChk AND lfChkUnComS()
      RETURN
    ENDIF

    IF llSelSafe
      SHOW GETS DISABLE ONLY WINDOW PSPOS1
      SHOW GETS DISABLE ONLY WINDOW PSPOS3
      SHOW GET lnSafes ENABLE
      
      _CUROBJ = OBJNUM(lnSafes)
      KEYBOARD '{SPACEBAR}'
      RETURN
    ENDIF

    SHOW GETS ENABLE ONLY WINDOW PSPOS1
    SHOW GET pbCustInfo DISABLE
    SHOW GET lnSafes    DISABLE
    SHOW GETS DISABLE ONLY WINDOW PSPOS3
    SHOW GET pbPay,1 PROMPT '-' DISABLE
    SHOW GET pbProceed,1 PROMPT lcClose ENABLE
    SHOW GET pbLogOut   ENABLE 

  *-- A D D   M O D E .
  CASE laScrMode[4]
    SHOW GETS DISABLE ONLY WINDOW PSPOS1
    SHOW GET pbCustInfo ENABLE
    SHOW GET pbLogOut   ENABLE 
    *C102047,1 ABD  let user ass. sales rep or change the sales rep any time. [End]
    SHOW GET lcSRep    ENABLE
    SHOW GET ibRepBrow ENABLE
    *C102047,1 ABD [End]
    
    IF llMis
      SHOW GET lcName  ENABLE
      SHOW GET lcPhone ENABLE
    ENDIF
    SHOW GETS ENABLE ONLY WINDOW PSPOS3
    
    IF !EOF()
      SHOW GET pbProceed,1 PROMPT lcProc
    ELSE
      *--Check if it is not a miselenus customer excange.
      IF lnSr = 1 AND llExchange   &&AND lcCustomer = lcMisAcct
        SHOW GET pbProceed,1 PROMPT lcCanl DISABLE
      ELSE
        SHOW GET pbProceed,1 PROMPT lcCanl ENABLE     
      ENDIF
      SHOW GET pbRemove  DISABLE
      SHOW GET pbEdit    DISABLE
      SHOW GET lnPaid    DISABLE
      SHOW GET lnPayTerm DISABLE
      SHOW GET pbPay,1  PROMPT PADL(ALLTRIM(laPayTerm[lnPayTerm,1]),18,' ') DISABLE
      SHOW GET lcRefer   DISABLE
      SHOW GET lnTender  DISABLE      
      
      *C102047,1 ABD show the add. discount code disable. [Begin]
      SHOW GET lnAddDisc DISABLE
      *C102047,1 ABD [End]
      
      *C101660,1 WAB - SHOW butt. freight diable at beginning 
      *C101660,1 WAB - START
      IF llDispFrght
        SHOW GET pbFreight DISABLE
      ENDIF
      
      *C101660,1 WAB - END
      
    ENDIF
    lcModDisb = IIF(laPayTerm[lnPayTerm,2]='MMMMMM','ENABLE','DISABLE')
    SHOW GET pbPay,1 PROMPT PADL(ALLTRIM(laPayTerm[lnPayTerm,1]),18,' ') &lcModDisb
    IF lnSr=1
      SHOW GET lcInvNo   DISABLE
      SHOW GET lnInvAmt  DISABLE
    ENDIF
    lcModDisb = IIF(llMis OR lnSr=2,'DISABLE','ENABLE')
    SHOW GET pbCCrdt   &lcModDisb
    SHOW GET lnCustAmt &lcModDisb
    SHOW GET llExchange  DISABLE
    SHOW GET pbKey ENABLE
    *--Check Open uncmsess record.
    IF ! llContinue 
      IF !llUpdOver
        =lfCrUnSess('O',.F.,.F.)
      ENDIF
    ENDIF
    *C102047,1 ABD Disable the button in case return. [begin]
    IF lnSR = 2
      SHOW GET lnAddDisc DISABLE      
    ENDIF
    *C102047,1 ABD [End]
ENDCASE
RETURN


*:*************************************************************
*: Name      : lfActBrow.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Activate the Screen Browse for (Sales Details).
*:*************************************************************
*: Calls     : lfwBrows()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfActBrow()
*:*************************************************************
FUNCTION lfActBrow

lnRecNo  = RECNO()
*E500241,1 SSH 03/05/1999  Add the UPC field in the brows if system 
*E500241,1 SSH             is setup to use UPC number.
*B802284,1 SSH 23/05/1999 Change the browse conditions
*IF !llUPC
IF llStyBrow
  *B603713,9 10/22/2000 NAD (Start) Increase the price and amount fields width in the browse. 
  *lcBrFields = "lcMarker=IIF(lnRecNo=RECNO(),'>>','  '):2:H='  ':W=.F.,"+;
               "lnBrCont=IIF(nSCount=0,'',ALLT(STR(nSCount,3)) ) :3:H='   ':W=.F.,"+;
               "lcBrSty=IIF(Cnt=0 AND TranType='1','Open Balance',Style)  :R :H=lcStyHdr :27 ,"+;
               "Sz1       :R :H='Size  '     :8,"+;
               "lcBrDesc=lfDspSty(.T.) :R :H='Description':30,"+;
               "Gros_Price :R :H='Org.Price'  :9,"+;
               "Price     :R :H='  Price  '  :9,"+;
               "TotQty    :R :H='Quantity'   :8,"+;
               "nAmount   :R :H='  Amount  ' :9,"+;
               "Disc_Amt  :R :H='Discount'  :9,"+;
               "Disc_Pcnt :R :H='Dis. % '   :7,"+;
               "Tax_Amt   :R :H='  Tax '    :8,"+;
               "Tax_Rate  :R :H='Tax % '    :7"
               
  *C102047,1 ABD Add discount % & discount amount to browse. [Begin]
  *- Browse add. discount.
  *lcBrFields = "lcMarker=IIF(lnRecNo=RECNO(),'>>','  '):2:H='  ':W=.F.,"+;
               "lnBrCont=IIF(nSCount=0,'',ALLT(STR(nSCount,3)) ) :3:H='   ':W=.F.,"+;
               "lcBrSty=IIF(Cnt=0 AND TranType='1','Open Balance',Style)  :R :H=lcStyHdr :27 ,"+;
               "Sz1       :R :H='Size  '     :8,"+;
               "lcBrDesc=lfDspSty(.T.) :R :H='Description':30,"+;
               "Gros_Price :R :H='Org.Price' :12,"+;
               "Price     :R :H='  Price  '  :12,"+;
               "TotQty    :R :H='Quantity'   : 8,"+;
               "nAmount   :R :H='  Amount  ' :15,"+;
               "Disc_Amt  :R :H='Discount'   : 9,"+;
               "Disc_Pcnt :R :H='Dis. % '    : 7,"+;
               "Tax_Amt   :R :H='  Tax '     : 8,"+;
               "Tax_Rate  :R :H='Tax % '     : 7"
               
               
  lcBrFields = "lcMarker=IIF(lnRecNo=RECNO(),'>>','  '):2:H='  ':W=.F.,"+;
               "lnBrCont=IIF(nSCount=0,'',ALLT(STR(nSCount,3)) ) :3:H='   ':W=.F.,"+;
               "lcBrSty=IIF(Cnt=0 AND TranType='1','Open Balance',Style)  :R :H=lcStyHdr :27 ,"+;
               "Sz1       :R :H='Size  '     :8,"+;
               "lcBrDesc=lfDspSty(.T.) :R :H='Description':30,"+;
               "Gros_Price :R :H='Org.Price' :12,"+;
               "Price     :R :H='  Price  '  :12,"+;
               "TotQty    :R :H='Quantity'   : 8,"+;
               "nAmount   :R :H='  Amount  ' :15,"+;
               "Disc_Amt  :R :H='Discount'   : 9,"+;
               "Disc_Pcnt :R :H='Dis. % '    : 7,"+;
               "Tax_Amt   :R :H='  Tax '     : 8,"+;
               "Tax_Rate  :R :H='Tax % '     : 7,"+;
               "DisCount  :R :H='Additional Discount' :13,"+;
               "DiscPcnt  :R :H='Additional Dis. %  ': 6"

  *C102047,1 ABD [End]
  *B603713,9 10/22/2000 NAD (End)     
ELSE
  *B603713,9 10/22/2000 NAD (Start) Increase the price and amount fields width in the browse. 
  *lcBrFields = "lcMarker=IIF(lnRecNo=RECNO(),'>>','  '):2:H='  ':W=.F.,"+;
               "lnBrCont=IIF(nSCount=0,'',ALLT(STR(nSCount,3)) ) :3:H='   ':W=.F.,"+;
               "lcBrSty =IIF(lnSr=2 AND TranType='1','Open Balance',Style)  :R :H=lcStyHdr :27 ,"+;
               "Sz1       :R :H='Size  '     :8,"+;
               "lcBrUpc=IIF(!(lnSr=2 AND TranType='1') AND ;
               SEEK(Style+STR(cnt,1),'StyleUpc'),StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3,'')  :R :H='UPC Number' :27 ,"+;
               "lcBrDesc=lfDspSty(.T.) :R :H='Description':30,"+;
               "Gros_Price :R :H='Org.Price'  :9,"+;
               "Price     :R :H='  Price  '  :9,"+;
               "TotQty    :R :H='Quantity'   :8,"+;
               "nAmount   :R :H='  Amount  ' :9,"+;
               "Disc_Amt  :R :H='Discount'  :9,"+;
               "Disc_Pcnt :R :H='Dis. % '   :7,"+;
               "Tax_Amt   :R :H='  Tax '    :8,"+;
               "Tax_Rate  :R :H='Tax % '    :7"
  *C102047,1 ABD Add discount % & discount amount to browse. [Begin]
  *- Browse add. discount.  
  *lcBrFields = "lcMarker=IIF(lnRecNo=RECNO(),'>>','  '):2:H='  ':W=.F.,"+;
               "lnBrCont=IIF(nSCount=0,'',ALLT(STR(nSCount,3)) ) :3:H='   ':W=.F.,"+;
               "lcBrSty =IIF(lnSr=2 AND TranType='1','Open Balance',Style)  :R :H=lcStyHdr :27 ,"+;
               "Sz1       :R :H='Size  '     :8,"+;
               "lcBrUpc=IIF(!(lnSr=2 AND TranType='1') AND ;
               SEEK(Style+STR(cnt,1),'StyleUpc'),StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3,'')  :R :H='UPC Number' :27 ,"+;
               "lcBrDesc=lfDspSty(.T.) :R :H='Description':30,"+;
               "Gros_Price :R :H='Org.Price'  :12,"+;
               "Price     :R :H='  Price  '  :12,"+;
               "TotQty    :R :H='Quantity'   :8,"+;
               "nAmount   :R :H='  Amount  ' :15,"+;
               "Disc_Amt  :R :H='Discount'  :9,"+;
               "Disc_Pcnt :R :H='Dis. % '   :7,"+;
               "Tax_Amt   :R :H='  Tax '    :8,"+;
               "Tax_Rate  :R :H='Tax % '    :7"


  lcBrFields = "lcMarker=IIF(lnRecNo=RECNO(),'>>','  '):2:H='  ':W=.F.,"+;
               "lnBrCont=IIF(nSCount=0,'',ALLT(STR(nSCount,3)) ) :3:H='   ':W=.F.,"+;
               "lcBrSty =IIF(lnSr=2 AND TranType='1','Open Balance',Style)  :R :H=lcStyHdr :27 ,"+;
               "Sz1       :R :H='Size  '     :8,"+;
               "lcBrUpc=IIF(!(lnSr=2 AND TranType='1') AND ;
               SEEK(Style+STR(cnt,1),'StyleUpc'),StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3,'')  :R :H='UPC Number' :27 ,"+;
               "lcBrDesc=lfDspSty(.T.) :R :H='Description':30,"+;
               "Gros_Price :R :H='Org.Price'  :12,"+;
               "Price     :R :H='  Price  '  :12,"+;
               "TotQty    :R :H='Quantity'   :8,"+;
               "nAmount   :R :H='  Amount  ' :15,"+;
               "Disc_Amt  :R :H='Discount'  :9,"+;
               "Disc_Pcnt :R :H='Dis. % '   :7,"+;
               "Tax_Amt   :R :H='  Tax '    :8,"+;
               "Tax_Rate  :R :H='Tax % '    :7,"+;
               "DisCount  :R :H='Additional Discount' :13,"+;
               "DiscPcnt  :R :H='Additional Dis. %  ': 6"

  *C102047,1 ABD [End]
*B603713,9 10/22/2000 NAD (End) 
ENDIF             
*E500241,1 SSH(END)
SELECT (lcPosTmp)
BROWSE FIELDS &lcBrFields;
       WINDOW  PSPOS2;
       WHEN lfwBrows();
       VALID :F lfvBrows() ;
       IN WINDOW PSPOS;
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOMENU;
       NOWAIT;
       NOEDIT;
       SAVE;
       TITLE lcBrowTitl

RETURN

*:*************************************************************
*: Name      : lfwBrows.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : When function of Browse (Sales Details).
*:*************************************************************
*: Example   : =lfwBrows()
*:*************************************************************
FUNCTION lfwBrows
lnRecNo = RECNO()
=lfRefresh()
SHOW WINDOW (lcBrowTitl) REFRESH
RETURN

*!*************************************************************
*! Name      : lfvBrows
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Valid Browse function.
*!*************************************************************
FUNCTION lfvBrows

IF WONTOP() # (lcBrowTitl)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF


*:*************************************************************
*: Name      : lfDspSty.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Display style in browse.
*:*************************************************************
FUNCTION lfDspSty
PARA llShowTran

lcStReturn = Desc1  
IF EMPTY(Style)
  DO CASE
    CASE TranType = '0'
      lcStReturn = 'Credit memo' + IIF(llShowTran,' : '+Tran,'')
    CASE TranType = '1'
      lcStReturn = 'Invoice' + IIF(llShowTran,' : '+Tran,'')
    CASE TranType = '2'
      lcStReturn = 'Debit Adjustment' +IIF(llShowTran,' : '+Tran,'')
    CASE TranType = '3'
      lcStReturn = 'Charge Back'+IIF(llShowTran,' : '+Tran,'')
    CASE TranType = '5'
      lcStReturn = 'Credit Adjustment'+IIF(llShowTran,' : '+Tran,'')
    CASE TranType = '6'
      lcStReturn = 'Credit on Account'+IIF(llShowTran,' : '+Tran,'')
    CASE TranType = '9'
      lcStReturn = 'Payment'+IIF(llShowTran,' : '+Tran,'')
  ENDCASE 
ENDIF
RETURN lcStReturn

*:*************************************************************
*! Name    : lfTrapKy
*! Purpose : Trap key.
*:*************************************************************
FUNCTION lfTrapKy

IF WONTOP() = lcBrowTitl
  glFromBrow = .T.
  ON KEY LABEL TAB     DO lpTab
  ON KEY LABEL BACKTAB DO lpShiftTab
ENDIF  
RETURN


*!*************************************************************
*! Name    : lfReadAct
*! Purpose : Read Activate clear trap.
*!*************************************************************
FUNCTION lfReadAct
*--Clear Trap.
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ENTER
RETURN


*!*************************************************************
*! Name      : lpTab
*! Purpose   : Tab key trapping procedure.
*!*************************************************************
PROCEDURE lptab

IF WONTOP()=lcBrowTitl
  ACTIVATE WINDOW PSPOS3
  *B802284,1 SSH 23/05/1999 Set focus to style/upc popup.
  *B603303,1 Start.
  *IF llStyBrow
  IF llStyBrow OR EMPTY(lcUpcPop)
  *B603303,1 End.
    _CUROBJ=OBJNUM(lcStyle)
  ELSE
    _CUROBJ=OBJNUM(lcUpcPop)
  ENDIF
  *B802284,1 SSH(END)
ELSE
  ACTIVATE WINDOW (lcBrowTitl)
ENDIF  
RETURN


*!*************************************************************
*! Name      : lpShiftTab
*! Purpose   : Shift Tab key trapping procedure.
*!*************************************************************
PROCEDURE lpshifttab

IF WONTOP()=lcBrowTitl
  ACTIVATE WINDOW PSPOS1
  IF laScrMode[1]
    _CUROBJ=OBJNUM(lcPhone)
  ELSE
    _CUROBJ=OBJNUM(pbLogOut)
  ENDIF
ELSE
  ACTIVATE WINDOW (lcBrowTitl)
ENDIF
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
  IF gfUnCompSession(lcProgID, lnSessNo, "point of sale")
    laScrMode[4] = .T.
    llFondSess   = .T.
    llContinue   = .T.
    lcSession    = UNCMSESS.cSession
    IF USED(lcPosTmp)
      USE IN (lcPosTmp)
    ENDIF
    =gfOpenFile(gcWorkDir+lcPosTmp,lcPosTmp,'EX')

    SET ORDER TO

    *C101660,1 WAB - change the filter to 4 and 'F' cause we add type 'F' FOr Freight 
    *C101660,1 WAB - START
    *SET FILTER TO TranType <> '4'
    SET FILTER TO !(TranType $ '4FR')
    *C101660,1 WAB - END

    =lfActBrow()
    lnChg = IIF(lnTender<>0,(lnTender - lnPaid),0)
    lnBal = IIF(SEEK('M'+lcCustomer,'CUSTOMER'),Customer.NetBal,0) 
    lnBal = IIF(!llMis,lnBal,0)
    lnSvPTrm = lnPayTerm
    llSelSafe = .F.
    = gfwCodePop( @laCodInfo, "CTERMCODE" ,'T' )
    =lfwPayTrm()
    lnPayTerm = lnSvPTrm
    = gfwCodePop( @laCodInfo, "CTERMCODE" ,'L' )

    lcCurObj = ALLTRIM(UnCmSess.cCurrObj)
    IF !EMPTY(lcCurObj) AND lcCurObj = 'pbSav'
      llContinue = .T.
      =lfvProceed(.T.)
    ENDIF
    SHOW GETS
  ELSE
    laScrMode[1] = .T.
    =lfGetTmpFile()
  ENDIF
ENDIF
RETURN (llFondSess)


*:*************************************************************
*! Name      : lfCrUnSess
*! Developer : Timour A. K. 
*! Date      : 10/10/97               
*! Purpose   : Create/Update uncomplete session record.
*:*************************************************************
*! Call      : lfUpdUVars()
*:*************************************************************
FUNCTION lfCrUnSess
PARA lcUnSStat,llUpdVr,llpbSav,llExgUVar

lnAlias = SELECT()
SELECT UNCMSESS
IF lcUnSStat='O' AND !llUpdVr AND !llExgUVar
  IF !SEEK('O'+lcProgID+PADR(gcUser_id,10)+lcSession)
    IF !SEEK('I'+lcProgID+PADR(gcUser_id,10)) 
      APPEND BLANK        
      REPLACE Status    WITH 'O',;
              cUTranType WITH lcProgID,;
              cUserId   WITH gcUser_id,;
              cSession  WITH lcSession,;
              cProgram  WITH lcAProgm ,;
              dTranDate WITH ldPostDate,;
              cTranTime WITH TIME()
    ELSE
      REPLACE Status    WITH 'O',;
              cSession  WITH lcSession,;
              dTranDate WITH ldPostDate,;
              cTranTime WITH TIME()
    ENDIF
   =RLOCK('UNCMSESS')
   FLUSH
  ENDIF
  lnUnCmSeRc=RECNO('UNCMSESS')
  =lfUpdUVars()  
ELSE
  IF SEEK('O'+lcProgID+PADR(gcUser_id,10)+lcSession)  
    IF Status <> lcUnSStat
      REPLACE Status WITH lcUnSStat
      FLUSH
    ENDIF
  ENDIF
  lnUnCmSeRc=RECNO('UNCMSESS')
  IF lcUnSStat='O'
    =lfUpdUVars()  
    IF llpbSav
      REPLACE ccurrobj WITH 'pbSav'
      FLUSH
    ELSE
      REPLACE ccurrobj WITH ' '
      FLUSH
    ENDIF
  ENDIF
  IF lcUnSStat='C'
    UNLOCK
    llContinue = .F.
  ENDIF
ENDIF
SELECT(lnAlias)
RETURN

*!*************************************************************
*! Name      : lfGetTmpFile
*! Developer : Timour A. K. 
*! Date      : 10/10/97               
*! Purpose   : To create all the uncompete session temprory files
*!             that is needed by this program.
*!*************************************************************
*! Call      : gfCrtTmp()
*!*************************************************************
FUNCTION lfGetTmpFile

lcPosTmp = gfTempName()
SELECT POSTran
=AFIELDS(laTpFlds)
lnTpFlds = ALEN(laTpFlds,1)
DIME laTpFlds[lnTpFlds+3,4]
laTpFlds[lnTpFlds+1,1] = 'nSteps'
laTpFlds[lnTpFlds+1,2] = 'N'
laTpFlds[lnTpFlds+1,3] = 2
laTpFlds[lnTpFlds+1,4] = 0
laTpFlds[lnTpFlds+2,1] = 'nSCount'
laTpFlds[lnTpFlds+2,2] = 'N'
laTpFlds[lnTpFlds+2,3] = 3
laTpFlds[lnTpFlds+2,4] = 0
laTpFlds[lnTpFlds+3,1] = 'lToExc'
laTpFlds[lnTpFlds+3,2] = 'L'
laTpFlds[lnTpFlds+3,3] = 1
laTpFlds[lnTpFlds+3,4] = 0


*B803961,1 ABD - Add new 2 fileds to the temp file to hold the 
*B803961,1 ABD - Credit Card number and the approval number. [Begin]
IF llUseAtom
  DIME laTpFlds[lnTpFlds+5,4]
  laTpFlds[lnTpFlds+4,1] = 'cApprovNm'
  laTpFlds[lnTpFlds+4,2] = 'C'
  laTpFlds[lnTpFlds+4,3] = 9
  laTpFlds[lnTpFlds+4,4] = 0
  laTpFlds[lnTpFlds+5,1] = 'cCardNumr'
  laTpFlds[lnTpFlds+5,2] = 'C'
  laTpFlds[lnTpFlds+5,3] = 19
  laTpFlds[lnTpFlds+5,4] = 0
ENDIF
*B803961,1 ABD - [End]

*--Create POS Tran temp file.
=gfCrtTmp(lcPosTmp,@laTpFlds,[TranType],lcPosTmp)
SELECT (lcPosTmp)
USE
=gfOpenFile(gcWorkDir+lcPosTmp,lcPosTmp,'EX')
SET ORDER TO
*--Don't display the current payments.

*C101660,1 WAB - change the filter to 4 and 'F' cause we add type 'F' FOr Freight 
*C101660,1 WAB - START
*SET FILTER TO TranType <> '4'
SET FILTER TO !(TranType $ '4FR')
*C101660,1 WAB - END


RETURN


*!*************************************************************
*! Name      : lfUpdUVars
*! Developer : Timour A. K. 
*! Date      : 10/10/97               
*! Purpose   : Function to create a string to be saved in the field
*!             UNCMSESS.mComent and UNCMSESS.mTmpFiles
*!*************************************************************
*! Call      : gfSavSess()
*!*************************************************************
FUNCTION lfUpdUVars

lcFiles = "lcPosTmp," + lcPosTmp+"," + ORDER(lcPosTmp)+";"
IF llpbSav
  lcFiles = lcFiles + "lcTmpAR,"  + lcTmpAR+","  + ORDER(lcTmpAR)+";"
  IF llExgUVar
    lcFiles = lcFiles + "lcInvHdr," + lcInvHdr+"," + ORDER(lcInvHdr) +";" +;
                        "lcInvLine,"+ lcInvLine+","+ ORDER(lcInvLine)+";"
    lcFiles = lcFiles + "lcRetHdr," + lcRetHdr+"," + ORDER(lcRetHdr) +";" +;
                        "lcRetLine,"+ lcRetLine+","+ ORDER(lcRetLine)+";"
  ELSE
    IF lnSr = 1
      lcFiles = lcFiles + "lcInvHdr," + lcInvHdr+"," + ORDER(lcInvHdr) +";" +;
                          "lcInvLine,"+ lcInvLine+","+ ORDER(lcInvLine)+";"
    ELSE
      lcFiles = lcFiles + "lcRetHdr," + lcRetHdr+"," + ORDER(lcRetHdr) +";" +;
                          "lcRetLine,"+ lcRetLine+","+ ORDER(lcRetLine)+";"
    ENDIF
  ENDIF
ELSE
  IF llExgUVar
    lcFiles = lcFiles + "lcTmpAR,"  + lcTmpAR+","  + ORDER(lcTmpAR)+";"
    IF lnSr = 2
      lcFiles = lcFiles + "lcInvHdr," + lcInvHdr+"," + ORDER(lcInvHdr) +";" +;
                          "lcInvLine,"+ lcInvLine+","+ ORDER(lcInvLine)+";"
    ELSE
      lcFiles = lcFiles + "lcRetHdr," + lcRetHdr+"," + ORDER(lcRetHdr) +";" +;
                          "lcRetLine,"+ lcRetLine+","+ ORDER(lcRetLine)+";"
    ENDIF
  ENDIF
ENDIF
= gfSavSess(lcProgID, lcFiles, @laVars,lcSession)
RETURN


*:*************************************************************
*: Name      : lfvType.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Swiches the mode between Sales and Returns.
*:*************************************************************
*: Calls     : lfRefresh()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvType()
*:*************************************************************
FUNCTION lfvType

lcSRDesc = IIF(lnSr=1,'Sale'  ,'Return')

*B802803,2 The Credit Memo became part of the POS ... No need for this check
*IF !llRMInstld AND lnSr = 2
  *--XXX module is not installed, Cannot proceed.
*  =gfModalGen('QRM42083B42001','DIALOG','Return merchandise')
*  lnSr = 1
*  SHOW GET lnSr
*  RETURN 
*ENDIF
*B802803,2 End

*C127532,1 EIH 07/20/2005 Call lfDispWhSC() which display screen to enable user select screen.[Begin]
IF ASCAN(laEvntTrig,PADR('DISPWHSC',10))<>0
   =gfDoTriger(lcProgName,PADR('DISPWHSC',10))
ENDIF
*C127532,1 EIH 07/20/2005 [End]

=lfRefresh()
RETURN


*:*************************************************************
*: Name      : lfvMis.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Valid Miscellaneous Customer Yes/No.
*:*************************************************************
*: Calls     : gfwCodePop(),lfRefresh()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvMis()
*:*************************************************************
FUNCTION lfvMis

IF llMis
  lcCustomer = lcMisAcct
  IF !SEEK('M'+lcCustomer,'CUSTOMER')
    *--Account : xxxxx is not found in the data file.
    lnOption = gfModalGen('QRM00001B00000','ALERT','Miscellaneous customer')
    RETURN .F.
  ENDIF
  = gfwCodePop( @laCodInfo, "CTERMCODE" ,'D' )
  *C102047,1 ABD fill the add. dicount code Popup. [Begin]
  *B604248,1 ABD Fix Problem that the field Additional discount is showing
  *B604248,1 ABD The default code instead of having it defaulted to "N/A" . [Begin]
  *= gfwCodePop( @laCodInfo, "CADDRETDIS" ,'D')

  *C200153,1 ABD - Apply the discount entered in customer file in Point 
  *C200153,1 ABD - of Sale, same as Additional discount is Applied , def. 
  *C200153,1 ABD - 2 variables hold the discount% and True if found  [Begin]
  IF Customer.Expires >= gdSysDate .AND. Customer.Disc # 0 
   lnCustDsc = Customer.Disc
   llCustDsc = .T.
   = lfwAddDisc ()
  ELSE   
    = gfwCodePop( @laCodInfo, "CADDRETDIS" ,'N')
  ENDIF
  *C200153,1 ABD - [End]
  
  *B604248,1 ABD [End]


  *C102047,1 ABD [End]
  laScrMode[1] = .F.
  laScrMode[4] = .T.
  *C102047,1 ABD Get the default sales rep and  default comm. from customer file. [Begin]
  lcSRep    = CUSTOMER.SalesRep
  lnCommRep = CUSTOMER.Comm
  *C102047,1 ABD [End]
  lcBalColor   = 'RGB(0,0,0,192,192,192)'
  SHOW GETS
  =lfRefresh()
  *B604522,1 MAN 07/08/2001 If there is a default rep, go to the rep object to alert the user
  *B604522,1 MAN            Added the IF/ELSE   
  IF !EMPTY(lcSRep)
    _CUROBJ=OBJNUM(lcSRep)
  ELSE
   *B603091,1 Start added.
   IF lnSr=2 AND !llAlowRet
      _CUROBJ=OBJNUM(lcInvNo)
   ELSE
   *B603091,1 End.
     _CUROBJ=OBJNUM(lcStyle)
   ENDIF
  ENDIF 
ENDIF


*:*************************************************************
*: Name      : lfvCustomer.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Validate Customers code.
*:*************************************************************
*: Calls     : CUSBROWM,gfwCodePop(),lfRefresh()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvCustomer()
*:*************************************************************
FUNCTION lfvCustomer


IF EMPTY(lcCustomer)
  lcTmpNam = ""
  RETURN
ENDIF

*C200222,1 SSH If custom setup is scan magnitic card.
IF llCusScan   && 18 Character
  lcTmpNam = lcTmpNam + lcCustomer
  lnStrTim = IIF(SUBSTR(ALLTRIM(lcCustomer), 1,1) = "%",SECONDS(),lnStrTim)
  lnEndTim = IIF(SUBSTR(ALLTRIM(lcTmpNam), LEN(ALLTRIM(lcTmpNam))) = "?",SECONDS(),0)
  IF SUBSTR(ALLTRIM(lcCustomer), LEN(ALLTRIM(lcCustomer))) <> "?"
     lcCustomer = ""
     SHOW GET lcCustomer
    _CUROBJ = OBJNUM(lcCustomer)
  ELSE
    IF (lnEndTim - lnStrTim) > 2
      lcCustomer = ""
      SHOW GET lcCustomer
     _CUROBJ = OBJNUM(lcCustomer)
      WAIT WINDOW "Invalid scan time,Please rescan the card!"
    ELSE
      lnEndTim = 0 
      lnStrTim = 0
      IF LEN(ALLTRIM(lcTmpNam)) = 18
        lcCustomer = SUBSTR(alltrim(lcTmpNam),8,5)
        SHOW GET lcCustomer
        IF SEEK("M"+lcCustomer,"Customer") .AND. CUSTOMER.Status = 'A'
          lcTmpNam = ""
          = lfGetInfo ()
          IF !EOF(lcPosTmp) .AND. laScrMode[4]
            = lfvCustDis ()
          ENDIF  
        ELSE
          IF !EMPTY(CUSTOMER.Status) .AND. CUSTOMER.Status <> 'A'
            =gfModalGen('INM52020B00000','DIALOG','active')
          ELSE
            = gfModalGen('QRM00001B00000','ALERT','Account :'+ALLTRIM(lcCustomer))
          ENDIF
          lcCustomer = SPACE(5)
          _CUROBJ  = OBJNUM(lcCustomer)
        ENDIF
      ELSE
        WAIT WINDOW "Error reading card,Please try again!"
        lcTmpNam = ""
        lcCustomer = ""
        _CUROBJ  = OBJNUM(lcCustomer)
      ENDIF
    ENDIF  
  ENDIF
  =lfStyCond()
ELSE
*C200222,1 SSH [End]
  llBrowse = ('?' $ lcCustomer)    
  IF !llBrowse AND LEN(ALLTRIM(lcCustomer)) < 5
    *-Account code should not be less than 5 characters.
    *-Browse Reenter
    IF gfModalGen('TRM40057B40007','ALERT') = 2
      _CUROBJ  = OBJNUM(lcCustomer)
      RETURN
    ENDIF
    llBrowse = .T.    
  ENDIF
  llNewCust = .F.
  IF llBrowse OR !SEEK('M'+lcCustomer,'CUSTOMER') 
    IF !llBrowse
      *--Account : xxxxx is not found in the data file.
      *--Browse Add Reenter
      lnOption = gfModalGen('QRM00001B00001','ALERT','Account :'+ALLTRIM(lcCustomer))
      DO CASE
        CASE lnOption = 1
          llBrowse = .T.
        CASE lnOption = 2
          =lfvCustInfo(.T.)
          llNewCust = .T.
          llBrowse = .F.
        CASE lnOption = 3
         lcCustomer = SPACE(5)
      ENDCASE
    ENDIF  
    IF llBrowse
      xAccount = lcCustomer
      SELECT CUSTOMER
      SET FILTER TO Type+Account<>'M'+lcMisAcct
      DO CUSBROWM WITH xAccount
      SET FILTER TO
      SELECT (lcPosTmp)
      lcCustomer = xAccount
    ENDIF
  ENDIF
  *B604253,1 ABD - Don't accept any customer with status not active. [Begin]
  IF SEEK('M'+lcCustomer,'CUSTOMER') .AND. CUSTOMER.Status <> 'A'
    *-- Message Text   : Customer status is not active . Can not accept.
    *-- Message Number : 52020.
    *-- Button Message : Ok  
    *-- Button Number  : 00000
    =gfModalGen('INM52020B00000','DIALOG','active')
    _CUROBJ  = OBJNUM(lcCustomer)
    lcCustomer = SPACE(5)  
    RETURN
  ENDIF
  *B604253,1 ABD - [End]
  IF !EMPTY(lcCustomer)
    IF lcCustomer = lcMisAcct
      *B604248,1 ABD Don't get the defult information for the Mis account in case
      *B604248,1 ABD Of return from proceed function. [Begin]
      IF laScrMode[4]
        *--The total amount of sale is not equal to the total paid amount 
        *--for this Miscellaneous Customer ,Please Add/Change this customer
        *--to Not Miscellaneous Customer.
        = gfModalGen('QRM52003B00000','DIALOG')
        lcCustomer = SPACE(5)
        _CUROBJ  = OBJNUM(lcCustomer)
        RETURN
      ELSE
        *B604248,1 ABD - [End]
        llMis = .T.
        =lfvMis()
        *B604248,1 ABD - End for if statement. [Begin]
      ENDIF 
      *B604248,1 ABD -  [End]
    ELSE
      = lfGetInfo ()
      *C200153,4 ABD - Change the dicount if select new customer & Update
      *C200153,4 ABD - The Temp File with the new customer discount . [Begin]
      IF !EOF(lcPosTmp) .AND. laScrMode[4]
        = lfvCustDis ()
      ENDIF  
      *C200153,4 ABD - [End]
      IF llNewCust AND lnSr=2
        SHOW GET lcInvNo  DISABLE
        SHOW GET lnInvAmt DISABLE
      ENDIF
      IF llChgMisln
        REPLACE ALL Account WITH lcCustomer,;
                    StName  WITH lcName,;
                    Phone1  WITH lcPhone
        llChgMisln = .F.
        *B604522,1 MAN 07/08/2001 Don't proceed with saving to allow the user to see the new trans.
        *B604522,1 MAN            amount in case there is additional discount or to confirm the sale
        *B604522,1 MAN            in all cases even if the trans. amount didn't change
        *=lfvProceed()
      ENDIF
    ENDIF
  ELSE
    _CUROBJ = OBJNUM(lcCustomer)
  ENDIF
  RETURN
*C200222,1 SSH [Start]
ENDIF
*C200222,1 SSH [End]


*:*************************************************************
*: Name      : lfvName.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Validate Customers Name.
*:*************************************************************
*: Calls     : 
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvName()
*:*************************************************************
FUNCTION lfvName

*B606121,1 SSH            Fix some problems in the CP#200222.
*IF ! llMis AND !EMPTY(lcName)
IF ! llMis AND !EMPTY(lcName) .AND. !llCusScan
*B606121,1 SSH            Fix some problems in the CP#200222.
  lnAlias = SELECT()
  SELECT CUSTOMER
  SET ORDER TO TAG Customnm
  SET NEAR ON
  IF SEEK('M'+UPPER(ALLTRIM(lcName)) )
    DIMENSION laTmp[1]
    laTmp = ''
    *E500241,1 SSH 03/05/1999 Commented out to change address fields name..
    *lcBrFields = [Account:8:h="Acct#",stName:35:h="Name",StAddr1:30:h='Address',btCity:10:H="City",]+;
                 [Phone1 :P= GFPHONETEM() :14:H="Phone #...",NetBal:11:H="Balance"]
    lcBrFields = [Account:8:h="Acct#",stName:35:h="Name",cAddress1:30:h='Address',cAddress12:30:H="City",]+;
                 [Phone1 :P= GFPHONETEM() :14:H="Phone #...",NetBal:11:H="Balance"]

    *E500241,1 SSH 03/05/1999 Commented out .
    =ARIABROW(['M'+UPPER(ALLTRIM(lcName))],'Customers',gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','Account','laTmp')
    lcCustomer=laTmp[1]
    SHOW GET lcCustomer    
  ENDIF
  SET ORDER TO TAG Customer
  SET NEAR OFF
  SELECT(lnAlias)

  IF !EMPTY(lcCustomer)
    =lfGetInfo()
  ENDIF
ENDIF

*:*************************************************************
*: Name      : lfvPhone.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Validate Customers Name.
*:*************************************************************
*: Calls     : 
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvPhone()
*:*************************************************************
FUNCTION lfvPhone

*B606121,1 SSH            Fix some problems in the CP#200222.
*IF ! llMis AND !EMPTY(SUBSTR(lcPhone,1,1))
IF !llMis AND !EMPTY(SUBSTR(lcPhone,1,1)) .AND. !llCusScan
*B606121,1 SSH            Fix some problems in the CP#200222.
  lnAlias = SELECT()
  SELECT CUSTOMER
  SET ORDER TO TAG Customph
  SET NEAR ON
  IF SEEK('M'+ALLTRIM(lcPhone))
    DIMENSION laTmp[1]
    laTmp = ''
    *E500241,1 SSH 03/05/1999 Commented out to change address fields name..
    *lcBrFields = [Account:8:h="Acct#",stName:35:h="Name",StAddr1:30:h='Address',btCity:10:H="City",]+;
                 [Phone1 :P= GFPHONETEM() :14:H="Phone #...",NetBal:11:H="Balance"]

    lcBrFields = [Account:8:h="Acct#",stName:35:h="Name",cAddress1:30:h='Address',cAddress12:30:H="City",]+;
                 [Phone1 :P= GFPHONETEM() :14:H="Phone #...",NetBal:11:H="Balance"]
    *E500241,1 SSH(END)
    =ARIABROW(['M'+ALLTRIM(lcPhone)],'Customers',gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','Account','laTmp')
    lcCustomer=laTmp[1]
    SHOW GET lcCustomer    
  ENDIF
  SET ORDER TO TAG Customer
  SET NEAR OFF
  SELECT(lnAlias)
  
  IF !EMPTY(lcCustomer)
    =lfGetInfo()
  ENDIF
ENDIF


*:*************************************************************
*: Name      : lfGetInfo.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Get information and variables.
*:*************************************************************
*: Calls     :  gfwCodePop()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfGetInfo()
*:*************************************************************
FUNCTION lfGetInfo

lcName  = Customer.StName
lcPhone = Customer.Phone1
lnBal = IIF(!llMis,Customer.NetBal,0)

*C102047,1 ABD [Begin]
lcSRep     =  Customer.SalesRep
lnCommRep  =  IIF(Customer.Comm=0,SALESREP.Comm,Customer.Comm)
*C102047,1 ABD [End]

*C200153,1 ABD - Apply the discount entered in customer file in Point 
*C200153,1 ABD - of Sale, same as Additional discount is Applied , def. 
*C200153,1 ABD - 2 variables hold the discount% and True if found  [Begin]
IF Customer.Expires >= gdSysDate .AND. Customer.Disc # 0 
  lnCustDsc = Customer.Disc
  llCustDsc = .T.
  = lfwAddDisc ()
  *C200153,4 ABD - change the customer discount [Begin]
ELSE
  lnCustDsc = 0
  llCustDsc = .F.
  *C200153,4 ABD - [End]
ENDIF
*C200153,1 ABD - [End]

IF llMulCurr
  lcCurrCode = IIF(!EMPTY(Customer.cCurrCode),Customer.cCurrCode,gcBaseCurr)
  lnExRate = gfChkRate('lnCurrUnit',lcCurrCode,ldPostDate,llEditExRt,gcAct_comp,.F.)
  IF lcCurrCode = gcBaseCurr OR lnExRate = 0 OR lnCurrUnit = 0
    STORE 1 TO lnExRate , lnCurrUnit
  ENDIF
ENDIF

IF llGLLink
  lcLinkCode = IIF(!EMPTY(Customer.Link_Code),Customer.Link_Code,"DEFDEF")
ENDIF

IF lnBal<> 0 AND !llMis
  lcBalColor = IIF(lnBal>0,"RGB(255,0,0,192,192,192)","RGB(0,128,0,192,192,192)")
  *--This customer has a credit of xxx
  ??CHR(7)
  ??CHR(7)
  =gfModalGen('INM42144B42000','DIALOG',IIF(lnBal>0,'Debit : ','Credit : ')+ALLT(STR(lnBal,10,2)))
ELSE
  lcBalColor = 'RGB(0,0,0,192,192,192)'
ENDIF

IF EMPTY(Customer.cTermCode)
  = gfwCodePop( @laCodInfo, "CTERMCODE" ,'D' )
ELSE
  = gfwCodePop( @laCodInfo, "CTERMCODE" ,'T' )
ENDIF
*C102047,1 ABD Get the fill the popup add. discount code. [Begin]
*B604248,1 ABD Fix Problem that the field Additional discount is showing
*B604248,1 ABD The default code instead of having it defaulted to "N/A" . [Begin]
*= gfwCodePop( @laCodInfo, "CADDRETDIS" ,'D' )
*B604248,1 ABD - Don't get the N/A defualt for additional discount 
*B604248,1 ABD - In case you are in save mode. [Begin]

*C200153,1 ABD - Get the def to N/A in case the selected customer 
*C200153,1 ABD - didn't have any discoun else get the discount in the popup  [Begin]
*IF !laScrMode[4] 
IF !laScrMode[4] .AND. !llCustDsc
  *C200153,1 ABD - [End]
  = gfwCodePop( @laCodInfo, "CADDRETDIS" ,'N' )
ENDIF
*B604248,1 ABD - [End]
*B604248,1 ABD [End]

*C102047,1 ABD [End]

laScrMode[1] = .F.
laScrMode[4] = .T.
SHOW GETS
=lfRefresh()
*B604522,1 MAN The refresh function causes this popup to be enable.           
SHOW GET lnAddDisc DISABLE

*B603091,1 Start added.
IF lnSr=2 AND !llAlowRet
  _CUROBJ=OBJNUM(lcInvNo)
ELSE
*B603091,1 End.
  _CUROBJ=OBJNUM(lcStyle)
ENDIF
RETURN



*:*************************************************************
*: Name      : lfClrInfo.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Clear information and variables.
*:*************************************************************
*: Calls     : 
*:*************************************************************
*: Passed Parameters  : llNoZapFile -> Don't Zap a Temp file
*:                                     when clear.
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfClrInfo(.T.)
*:*************************************************************
FUNCTION lfClrInfo
PARA llNoZapFile

*--Initialize screen variables.
STORE 0.0 TO lnTotal,lnPaid,lnTender,lnChg,lnBal,lnCustAmt
STORE ' ' TO lcCustomer,lcName,lcPhone,lcRefer,lcStyle
*C102047,1 ABD define variables. [Begin]
lcSRep    = ''
lnCommRep = 0
*C102047,1 ABD [End]

*E301467,1 (Start)
lcRefer = IIF(llUseAtom,SPACE(60),SPACE(30))
*E301467,1 (End).
STORE ' ' TO lcInvoiceNo,lcCrMemoNo,lcInvNo
STORE ' ' TO lcAddr1,lcAddr2,lcAddr3,lcAddr4,lcAddr5,lcAddr6
STORE .F. TO llMis,llMltPay
STORE   1 TO lnSr,lnPayTerm
lnInvAmt = 0
lnSCount = 0
llExchange = .F.
lcPyStatus= 'Payment'
*--Default balance color to black.
lcBalColor= 'RGB(0,0,0,192,192,192)'

*C101660,1 WAB - reintial freight amount 
*C101660,1 WAB - START
IF llDispFrght
  STORE 0.0 TO lnFreight ,lnRtFrght
ENDIF
*C101660,1 WAB - change the filter to 4 and 'F' cause we add type 'F' FOr Freight 
*C101660,1 WAB - START


*--Initialize the "Codes Information" array for Payment Terms.
*C102047,1 ABD [Begin]
*- Reinitialize the "Code Information' array for Payment Terms &
*- For Addtional retail discount.
*DECLARE laCodInfo [1,10],laPayTerm[1,2]
DECLARE laCodInfo [2,10],laPayTerm[1,2],laAddDisc[1,2]
*C102047,1 ABD [End]
laCodInfo = SPACE(0)
laCodInfo[1,01] = "CTERMCODE"
laCodInfo[1,02] = "laPayTerm"
laCodInfo[1,03] = "lnPayTerm"
laCodInfo[1,04] = ""
laCodInfo[1,05] = .F.            
laCodInfo[1,06] = .F.            
laCodInfo[1,07] = "CUSTOMER"     
laCodInfo[1,08] = "CUSTOMER"     
laCodInfo[1,09] = "'M'+lcCustomer"     
laCodInfo[1,10] = "cTermCode"
*C102047,1 ABD Fill the array with the add. Discount code. [Begin]
laCodInfo[2,01] = "CADDRETDIS"
laCodInfo[2,02] = "laAddDisc"
laCodInfo[2,03] = "lnAddDisc"
laCodInfo[2,04] = ""
laCodInfo[2,05] = .F.            
laCodInfo[2,06] = .T.            
laCodInfo[2,07] = lcPosTmp
laCodInfo[2,08] = "lcPosTmp"     
laCodInfo[2,09] = ""
laCodInfo[2,10] = "CADDRETDIS"
= gfwCodePop( @laCodInfo, "CADDRETDIS" ,'N' )
*C102047,1 ABD fill the Popup add. discount code . [Begin]
= gfwCodePop( @laCodInfo, "CTERMCODE" ,'N' )
SHOW GET pbPay,1 PROMPT '-' DISABLE

*C200153,1 ABD - 2 variables hold the discount% and True if found  [Begin]
STORE 0 To  lnCustDsc
*C200153,1 ABD [End]

*--Zap a Temp file.
IF ! llNoZapFile
  SELECT (lcPosTmp)
  ZAP
  =lfActBrow()
ENDIF
RETURN


*:*************************************************************
*: Name      : lfvCustInfo.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : View /Edit Customer information.
*:*************************************************************
*: Calls     : 
*:*************************************************************
*: Passed Parameters  : llEdit = .T.  Edit information
*:                             = .F.  Only view information
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvCustInfo()
*:*************************************************************
FUNCTION lfvCustInfo
PARA llEdit

IF !laFlOpn[8,1]
  laFlOpn[8,1]=gfOpenFile(gcSysHome+'SYCINT',gcSysHome+'CCONTCODE','SH')
  laFlOpn[8,2]='SYCINT'
ENDIF
*E301077,13 open on this function instead of with the programs.
IF !laFlOpn[14,1]  
  laFlOpn[14,1]=gfOpenFile(gcDataDir+"SALESREP","SALESREP","SH")
  laFlOpn[14,2]='SALESREP'
ENDIF
*E301077,13 End.

lcContCod = IIF(EMPTY(CUSTOMER.cCont_Code),ALLTRIM(gcContCode),CUSTOMER.cCont_Code)
=SEEK(lcContCod,'SycInt')
lcAddHed1  = SycInt.cPart1Lab

SELECT CUSTOMER
IF !llMis AND SEEK('M'+lcCustomer,'CUSTOMER')
  lcAddr1 = CUSTOMER.cAddress1
  lcAddr2 = CUSTOMER.cAddress2
  lcAddr3 = CUSTOMER.cAddress3
  lcAddr4 = CUSTOMER.cAddress4
  lcAddr5 = CUSTOMER.cAddress5
  lcAddr6 = IIF(EMPTY(CUSTOMER.cAddress6),lcContCod,CUSTOMER.cAddress6)
ENDIF

IF llEdit OR llMis
  llIsCanada = ( UPPER(ALLTRIM(gcContCode))='CANADA' ) 
  lnTaxRate  = 0
  lcTaxRule  = ' 1'
ENDIF
llCICancel = .T.
lcSObjStat = IIF(llEdit OR llMis,'ENABLE','DISABLE')
PUSH KEY
ON KEY
DO (gcScrDir+gcWinAppl+"\PSPOSC.SPX")
POP KEY
IF llCICancel OR llMis
  lcCustomer = IIF(llEdit,SPACE(5),lcCustomer)
ELSE
  DECLARE laCodes[6,10],laClass[1,2],laTerms[1,2],laDivision[1,2],laRegion[1,2],laShipVia[1,2]laSpcInst[1,2]
  STORE '' TO laCodes,laClass,laTerms,laDivision,laRegion,laShipVia,laSpcInst
  STORE 1  TO lnClass,lnTerms,lnDivision,lnRegion,lnShipVia,lnSpcInst

  laCodes[1,1] = 'CLASS'
  laCodes[1,2] = 'laClass'
  laCodes[1,3] = 'lnClass'
  laCodes[2,1] = 'CTERMCODE'
  laCodes[2,2] = 'laTerms'
  laCodes[2,3] = 'lnTerms'
  laCodes[3,1] = 'cDivision'
  laCodes[3,2] = 'laDivision'
  laCodes[3,3] = 'lnDivision'
  laCodes[4,1] = 'REGION'
  laCodes[4,2] = 'laRegion'
  laCodes[4,3] = 'lnRegion'
  laCodes[5,1] = 'SHIPVIA'
  laCodes[5,2] = 'laShipVia'
  laCodes[5,3] = 'lnShipVia'
  laCodes[6,1] = 'SPCINST'
  laCodes[6,2] = 'laSpcInst'
  laCodes[6,3] = 'lnSpcInst'

  *C102047,1 ABD Remark the next line and get the sales rep from the screen. [Begin]
  *GO TOP IN SALESREP
  *lcSRep = SALESREP.RepCode
  *C102047,1 ABD [End]
  
  =gfwCodePop(@laCodes,'CLASS','D')
  =gfwCodePop(@laCodes,'CTERMCODE','D')
  =gfwCodePop(@laCodes,'CDIVISION','N')
  =gfwCodePop(@laCodes,'REGION','D')
  =gfwCodePop(@laCodes,'SHIPVIA','D')
  =gfwCodePop(@laCodes,'SPCINST','D')

  *E301249,1 IHB Change default priority to '  5' insted of '5'
  *--Update customer file.
  SELECT CUSTOMER
  APPEND BLANK
  REPLACE Type       WITH 'M',;
          BillTo     WITH 'M',;
          Account    WITH lcCustomer,;
          Status     WITH 'A',;
          Link_code  WITH 'DEFDEF',;
          Btname     WITH lcName,;
          Stname     WITH lcName,;
          Consol     WITH 'N',;
          Phone1     WITH lcPhone,;
          SalesRep   WITH lcSRep,;
          Priority   WITH '  5',;
          Region     WITH SUBSTR(laRegion[lnRegion,2],1,6),;
          Class      WITH SUBSTR(laClass[lnClass,2],1,6),;
          cTermCode  WITH SUBSTR(laTerms[lnTerms,2],1,6),;
          ShipVia    WITH SUBSTR(laShipVia[lnShipVia,2],1,6),;
          cDivision  WITH SUBSTR(laDivision[lnDivision,2],1,6),;
          Spcinst    WITH SUBSTR(laSpcInst[lnSpcInst,2],1,6),;
          cinsur     WITH 'Y',; 
          PriceLvl   WITH 'A',; 
          Prnt_statm WITH 'Y',; 
          nTaxRate   WITH lnTaxRate,; 
          cTaxRule   WITH lcTaxRule,; 
          cCont_code WITH lcContCod,; 
          cAddress1  WITH lcAddr1,; 
          cAddress2  WITH lcAddr2,; 
          cAddress3  WITH lcAddr3,; 
          cAddress4  WITH lcAddr4,; 
          cAddress5  WITH lcAddr5,; 
          cAddress6  WITH lcAddr6,;                                                  
          cAddress12 WITH lcAddr1,; 
          cAddress22 WITH lcAddr2,; 
          cAddress32 WITH lcAddr3,; 
          cAddress42 WITH lcAddr4,; 
          cAddress52 WITH lcAddr5,; 
          cAddress62 WITH lcAddr6,;                                                  
          cCont_cod2 WITH lcContCod,; 
          cCurrCode  WITH gcBaseCurr,;
          cSlsGlLink WITH 'DEF'
    *-- Call global function to add audit fields info.
    =gfAdd_Info('CUSTOMER')
    *--Call TraceKey global function.
    =gfTraceKey('CUSTOMER','M'+lcCustomer,'A')

  *------------------------------------------------------------
  *E301557,1 Saving a record for the current saved customer in the EDITrans Table (Start)
  IF 'NC' $ gcComp_Mdl AND Customer.Type='M'
    PRIVATE lcCodesTag

    = gfOpenFile(gcDataDir+'EDIAcPrt',gcDataDir+'Accfact','SH')
    = gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PartTrans','SH')
    = gfOpenFile(gcDataDir+'EDITrans',gcDataDir+'Typekey','SH')

    lcCodesTag = ORDER('Codes')
    SET ORDER TO cCode_No IN Codes
    SELECT EDIAcPrt
    SCAN
      IF lInterComp AND SEEK(EDIAcPrt.cPartCode+'816','EDIPD')
        IF SEEK('N'+'CSITEID   '+PADR(EDIAcPrt.cSiteID,6)+SPACE(30)+'LRECCUST','Codes') AND ;
           Codes.cRltd_Vlu = 'T'
          SELECT EDITRANS 
          *B604544,1 SSH [Start] Fix problem variable ladata[2] not found.
          lcCustomer = IIF(TYPE("laData[2]")="C", laData[2], lcCustomer)
          *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
          *IF !SEEK('816'+PADR('M'+lcCustomer,20)+EdiAcPrt.Type+EdiAcPrt.cPartner)
          IF !SEEK('816'+PADR('M'+lcCustomer,40)+EdiAcPrt.Type+EdiAcPrt.cPartner)
          *E037853,1 [End]
          *B604544,1 SSH [End]
            INSERT INTO 'EDITRANS' (cEdiTrnTyp,Key,Type,cPartner,lInterComp) ;
            VALUES                 ('816','M'+Customer.Account,EdiAcPrt.Type,EdiAcPrt.cPartner,EdiAcPrt.lInterComp)
          ENDIF  
          REPLACE cStatus WITH 'N'
          =gfAdd_Info('EDITRANS')
        ENDIF
      ENDIF
    ENDSCAN
    SET ORDER TO (lcCodesTag) IN Codes
  ENDIF
  *E301557,1 (End)
  *------------------------------------------------------------

    *--Update customer history.
    *E301077,13 open on this function instead of with the programs.
    IF !laFlOpn[13,1]  
      laFlOpn[13,1]=gfOpenFile(gcDataDir+"ARCUSHST","ACTHST","SH")
      laFlOpn[13,2]='ARCUSHST'
    ENDIF
    *E301077,13 End.
    SELECT ARCUSHST
    INSERT INTO arCusHst (Account,cFisFYear) VALUES (lcCustomer,STR(VAL(gcCurrYear)-1,4))
    INSERT INTO arCusHst (Account,cFisFYear) VALUES (lcCustomer,gcCurrYear)
    INSERT INTO arCusHst (Account,cFisFYear) VALUES (lcCustomer,STR(VAL(gcCurrYear)+1,4))
    =gfTraceKey('ARCUSHST',lcCustomer,'A')
                  
ENDIF
SHOW GET lcName
SHOW GET lcPhone
*C102047,1 ABD Show Get sales rep field. [Begin]
SHOW GET lcSRep
*C102047,1 ABD [End]
SELECT (lcPosTmp)
RETURN

*:*************************************************************
*: Name      : lfvCIOk.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Validate the customer information entered.
*:*************************************************************
FUNCTION lfvCIOk

IF EMPTY(lcName)
  *--Name field should not be left empty. Please add the name.
  =gfModalGen('TRM40051B00000','ALERT')
  _CUROBJ  = OBJNUM(lcName)
  RETURN
ENDIF
IF .F. AND !llMis
  STORE ' ' TO lcAddrObj
  DO CASE
    CASE EMPTY(lcAddr1)
      lcAddrObj = "lcAddr1"
    CASE EMPTY(lcAddr3)
      lcAddrObj = "lcAddr3"
    CASE EMPTY(lcAddr4)
      lcAddrObj = "lcAddr4"
    CASE EMPTY(lcAddr5)
      lcAddrObj = "lcAddr5"
    CASE EMPTY(lcAddr6)
      lcAddrObj = "lcAddr6"
  ENDCASE
  IF !EMPTY(lcAddrObj)
    *--One or more address fields is empty. Please add all the address fields.
    =gfModalGen('TRM40050B00000','ALERT')
    _CUROBJ  = OBJNUM(&lcAddrObj)
    RETURN
  ENDIF
ENDIF
llCICancel = .F.

*C200153,1 ABD - ass. .F. value if we cancel the screen. [Begin]
llCustDsc = .F.
STORE 0 To  lnCustDsc
*C200153,1 ABD - [End]


CLEAR READ
RETURN


*!*************************************************************
*! Name      : lfvState
*: Developer : Timour A. K.
*: Date      : 02/02/98
*! Purpose   : Validate USA/CANADA State
*!*************************************************************
*! Calls     : AriaBrow
*!*************************************************************
*! Passed Parameters  : Main Account State / Store State
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvState()
*!*************************************************************
FUNCTION lfvState
PARAMETERS lcState
PRIVATE lnAlias,lcKey

IF (SYS(18)='LCADDR4' AND INLIST(UPPER(ALLTRIM(lcContCod)),'USA','CANADA'))
  lnAlias = SELECT()
  SELECT CODES
  SET ORDER TO TAG Idrltfname
  *B603192,1 Start.
  *=SEEK(gcAct_comp+'N'+'STATE')
  *LOCATE REST WHILE ccomp_id+crltfield+cfld_name = gcAct_comp+'N'+'STATE' ;
  *FOR SUBSTR(cCode_No,1,3) = SUBSTR(&lcState,1,3)
  =SEEK('N'+'N'+'STATE')
  LOCATE REST WHILE cdefcode+crltfield+cfld_name = 'N'+'N'+'STATE' ;
                        FOR SUBSTR(cCode_No,1,3) = SUBSTR(&lcState,1,3)
  *B603192,1 End.
  IF !FOUND()
    SELECT CODES
    *B603192,1 Start.
    *=SEEK(gcAct_comp+'N'+'STATE')
    =SEEK('N'+'N'+'STATE')
    *B603192,1 End.
    lcBrSTTitl = PROP(LOOKUP(SYCINT.CPARt4LAB,lcAddr4,sycint.ccont_code,'CCONTCODE'))
    lcBrFields = "cCode_No  :R :H=lcBrSTTitl, "+;
                 "cDiscrep  :R :H='Name'"
    *B603192,1 Start.
    *lcKey = gcAct_comp+'N'+'STATE'
    lcKey = 'N'+'N'+'STATE'
    *B603192,1 End.
    &lcState = IIF(AriaBrow("lcKey",lcBrSTTitl,gnBrHSRow1,.F., gnBrHSRow2, .F., '',;
                   '','cCode_No','laBrowArr'),Codes.cCode_No,&lcState)
  ENDIF

  IF llTaxSetup
    *B603192,1 Start.
    *=SEEK(gcAct_comp+'Y'+'STATE')
    =SEEK('N'+'Y'+'STATE')
    *B603192,1 End.
    LOCATE REST WHILE cdefcode+crltfield+cfld_name = 'N'+'Y'+'STATE' ;
            FOR SUBSTR(cCode_No,1,3) = SUBSTR(&lcState,1,3) .AND. cRltd_Nam='NTAXRATE'
    lnTaxRate = IIF(FOUND(),VAL(codes.cRltd_Vlu),0)
    IF llIsCanada
      =SEEK('N'+'Y'+'STATE')
      LOCATE REST WHILE cdefcode+crltfield+cfld_name = 'N'+'Y'+'STATE' ;
                    FOR SUBSTR(cCode_No,1,3) = SUBSTR(&lcState,1,3) .AND. cRltd_Nam='CTAXRULE'
      lcTaxRule  = IIF(FOUND(),codes.cRltd_Vlu,'1')
    ENDIF
  ENDIF

  SELECT (lnAlias)
ENDIF
RETURN


*:*************************************************************
*: Name      : lfvProceed.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Procced with Update or Cancel or Close.
*:*************************************************************
*: Calls     : lfUpdSale() , lfUpdRetn()
*:             lfCrtTpFls(), lfCrUnSess('O'), lfClrInfo()
*:*************************************************************
*: Passed Parameters  : llFrmUncm = .T. form uncomplete session.
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvProceed()
*:*************************************************************
FUNCTION lfvProceed
PARA llFrmUncm

IF laScrMode[4]
  *B803962,1 ABD - Add New setup to prevent the user to Sale or return with amount
  *B803962,1 ABD - less than the total amount for this sales or return. [Begin]
  *B606415,1 SSH Fix problems in Paid Amount
  *IF laSetups[17,2] .AND. lnTotal <> lnPaid
  IF ((lnSr = 1 .AND. laSetups[17,2]) .OR. (lnSr = 2 .AND. llALLRTAMT)).AND. lnTotal <> lnPaid
  *B606415,1 SSH Fix problems in Paid Amount
    *-- Text Message   :- Paid amount does not equal the total amount , can not proceed.
    *-- MessageNumber  :- 40060.
    *-- Message Button :- OK.
    *-- Button Number  :- 4200.
    =gfModalGen('TRM40060B42000','DIALOG','Paid amount'+'|'+'the total amount ,Can not proceed')
    _CUROBJ  = OBJNUM(lnPaid)
    RETURN
  ENDIF
  *B803962,1 ABD - [End]

  *E301467,1 Start.
  IF llUseAtom AND !llFrmUncm
    SELECT (lcPosTmp)
    lcFltr = SET('FILTER')
    lcFOrd = SET('ORDER')
    SET FILTER TO
    SET ORDER TO 1

    IF !SEEK('4')
      IF lnPaid <> 0
        *--Single Payment
        lcTranCode = laPayTerm[lnPayTerm,2]
             
        IF ! lfvCrCard(lcTranCode,lcRefer,lnPaid)
          SET FILTER TO &lcFltr
          SET ORDER TO &lcFOrd
          RETURN .F.
        ENDIF
      ENDIF
    ELSE
      *--Multiple Payments
      SCAN WHILE TranType='4' FOR nAmount<>0

        IF ! lfvCrCard(TranCode,ALLT(cCrdCard),nAmount)
          SET FILTER TO &lcFltr
          SET ORDER TO &lcFOrd
          RETURN .F.
        ENDIF
      ENDSCAN
    ENDIF     
  ENDIF
  *E301467,1 End.


  *--Open files needed for saving.
  *E301077,13 open on this function instead of with the programs or at the bigining.
  IF !laFlOpn[5,1]
    laFlOpn[5,1]=gfOpenFile(gcDataDir+'APCHECKS','BANKCHECK','SH')
    laFlOpn[5,2]='APCHECKS'
  ENDIF
  IF !laFlOpn[6,1]  
    laFlOpn[6,1]=gfOpenFile(gcDataDir+"APPAYMNT","","SH")
    laFlOpn[6,2]='APPAYMNT'
  ENDIF
  IF !laFlOpn[7,1]  
    laFlOpn[7,1]=gfOpenFile(gcDataDir+"ORDHDR","ORDHDR","SH")
    laFlOpn[7,2]='ORDHDR'
  ENDIF
  IF !laFlOpn[12,1]  
    laFlOpn[12,1]=gfOpenFile(gcDataDir+"REPCOMM","REPCOMM","SH")
    laFlOpn[12,2]='REPCOMM'
  ENDIF
  IF !laFlOpn[13,1]  
    laFlOpn[13,1]=gfOpenFile(gcDataDir+"ARCUSHST","ACTHST","SH")
    laFlOpn[13,2]='ARCUSHST'
  ENDIF
  IF !laFlOpn[14,1]  
    laFlOpn[14,1]=gfOpenFile(gcDataDir+"SALESREP","SALESREP","SH")
    laFlOpn[14,2]='SALESREP'
  ENDIF
  *E301077,13 End.
   
  *--Close the filter to see All transactions.
  SELECT (lcPosTmp)
  SET FILTER TO
  SET ORDER TO 1
  *--You must have a sale or return for update.
  IF ( lnSr=1 AND SEEK('1') ) OR ( lnSr=2 AND SEEK('0') )

    IF ! llFrmUncm
      *--If the customer is miselenues and there is balance.   
      IF llMis AND lnTotal <> lnPaid
        IF !llExchange OR (lnSr=1)
          llChgMisln = .T.
          lcCustomer = SPACE(5)
          llMis = .F.
          *B802525,1 Check that this returnd invoice does not used in preivus return customer.
          SELECT POSTRAN
          lcOLdTag = TAG()
          SET ORDER TO TAG Postrln
          *--check that this returnd invoice does not used in preivus return customer.
          *B804026,1 ABD Fix problem that in case of return with           
          *B804026,1 ABD Miscelleneous account you should not complete the save 
          *B804026,1 ABD In case of paid amount did't equal the total paid amount. [Begin]
          *IF lnSr=2 AND !EMPTY(lcInvNo) AND SEEK(lcInvNo)
          IF lnSr=2 AND !EMPTY(lcInvNo) AND SEEK(lcInvNo) .AND. lnTotal = lnPaid
            *B804026,1 ABD [End]
            lcCustomer=POSTRAN.Account
            SHOW GET lcCustomer
            SET ORDER TO TAG &lcOLdTag
            SELECT (lcPosTmp)
            REPLACE ALL Account WITH lcCustomer
          ELSE
          *B802525,1 End.
          
            *B804026,1 ABD Fix problem that in case of return with           
            *B804026,1 ABD Miscelleneous account you should not complete the save 
            *B804026,1 ABD In case of paid amount did't equal the total paid amount. [Begin]

            *--The total amount of sale is not equal to the total paid amount 
            *--for this Miscellaneous Customer ,Please Add/Change this customer
            *--to Not Miscellaneous Customer.
            *=gfModalGen('TRM52003B42000','DIALOG')
            IF gfModalGen('QRM52003B52000','DIALOG') = 1
              *B804026,1 ABD [End]
              SET ORDER TO TAG &lcOLdTag
              SELECT (lcPosTmp)
              SHOW GETS WINDOW PSPOS1 DISABLE
              SHOW GETS WINDOW PSPOS3 DISABLE
              SHOW GET lcCustomer ENABLE
              _CUROBJ  = OBJNUM(lcCustomer)
              *B604248,1 ABD Disable the sales rep in case you are in save mode [Begin]
              SHOW GET ibRepBrow DISABLE
              SHOW GET lcSRep    DISABLE
              *B604248,1 ABD - [End]
              RETURN
            *B804026,1 ABD Else for if statement. [Begin]
            ELSE
              llMis = .T.
              *:B#604352,1 SSH Restore the customer code.
              lcCustomer = lcMisAcct
              *:B#604352,1 SSH [END]
              _CUROBJ  = OBJNUM(lnPaid)
              RETURN
            ENDIF
            *B804026,1 ABD [End]
          ENDIF
        ELSE
          *--Save ret info.
          llUpdOver = .T.
        ENDIF
      ENDIF

   
      *--Create Temprorary ArHist file, InvHdr and Invline files .OR.
      *--Create Temprorary ArHist file, RetHdr and Retline Files.
      =lfCrtTpFls()

      *--Read POS Transaction sequence.
      lcPOSSeq = gfsequence('CPOSSEQ')
      *--Update all POSTran temp file with new sequence generated. 
      *--And Clear nSteps if Exchange.
      SELECT (lcPosTmp)
      REPLACE ALL cPOSSeq WITH lcPOSSeq,;
                  nSteps  WITH 0

     IF lnSr =1
        *-Read Invoice sequence.
        lcInvoiceNo = gfsequence('INVOICE')
        *--Read GL Session sequence.
        lcGlSession = gfsequence('GLSESSION')
      ELSE
        *-Credit memo sequence.
        lcCrMemoNo = gfsequence('CRMEMO')
      ENDIF

      *--Uncomlete session variables.
      =lfCrUnSess('O',.T.,.T.,(llUpdOver AND lnSr = 1))

      STORE SPACE(0) TO lcAdTrnSeq,lcAcTrnSeq,lcHisSeq,lcGlSess,lcRepBat
      STORE .T. TO llUpdGlDif,llUpdMstGL

    ELSE  
      lcPOSSeq  = cPOSSeq
      lcCustomer = Account
      lcName    = StName
      lcPhone   = Phone1
      =gfwCodePop ( @laCodInfo, "CTERMCODE" ,"L" )
    ENDIF

    *--Create a payment record in lcPosTmp is not multiple payments. 
    *---------------------------------------------------------------
    *--If Single payment/Refunf term the record not in file.
    *--Create Payment record in POSTRAN temp file.
    *--If Multiple payment term the payments record exist in file.
    *---------------------------------------------------------------
    IF !SEEK('4') AND lnPaid <> 0
      =lfCrtSPay()
    ENDIF


    *--Start saving and updating. 
    lcSavSess = lcSession
    IF ! llUpdOver
      llUpdate  = IIF( lnSr = 1 , lfUpdSale() , lfUpdRetn() )
    ELSE
      IF lnSr = 2
        *--Save return information into temp files,
        *--the return update will be later.
        REPLACE ALL lToExc  WITH .T.,;
                    cOWner  WITH lcPOSSeq
      ELSE
        llUpdOver = .F.
        =lfUpdExchg()
      ENDIF
    ENDIF 
    lcSession = lcSavSess


    *--P R I N T I N G.
    *--Check the Existance of optional messege titles.
    IF FILE(gcDataDir+'RCETMSGS.MEM')
      RESTORE FROM (gcDataDir+'RCETMSGS.MEM') ADDI
    ELSE
      lcTtl1 = 'No exchange without receipt, Exchange only   '
      lcTtl2 = 'within 48 hours of date of purchase without  '
      lcTtl3 = 'the removal of swing tag.                    '
      lcTtl4 = '<< Thank You For Coming, See You Soon.! >>   '
      DO (gcScrDir+gcWinAppl+"\PSPOSMG.SPX")
      SAVE ALL LIKE lcTtl* TO (gcDataDir+'RCETMSGS.MEM')
    ENDIF
    *--Print the recept.
    lcRpName = 'PSRECPT'
    lnTtlNo  = 1
    *--Added freight record.
    *lcBzTp   = '01'
    lcBzTp   = '01FR'

    SELECT (lcPosTmp)    
    IF (!llExchange OR lnSr = 1) AND llPrintRec
      SELECT (lcPosTmp)    
      *--print the freight record after style and before payment.
      INDEX ON IIF(trantype $ '10','A',IIF(trantype $ 'FR','B','C')) TAG TempInx ADDI

      *--If receipt will be print calulate total Tax and Total discount.
      STORE 0 TO lnTotTax,lnTotDisc,lnTotGros,lnTDsPerc
      DIME laPrntSPe[7]
      laPrntSPe = ' '
      =lfPrntSPe()

      DO WHILE .T. 
        GO TOP
        *B802834,1 KHM 11/30/99 (Begin) Reinitializing this variable
        *B802834,1              each time you choose to print.
        lnTtlNo  = 1
        *B802834,1 KHM 11/30/99 (End)
        lcConsSet = SET('CONSOLE')
        SET CONSOLE OFF
        lcDevice = gcDevice 
        gcDevice = 'PRINTER'
        lcOGPlatForm = 'DOS'
        lcOutFile = gcOutFile
        _PEJECT   = "NONE"
        DEFINE WINDOW lcTmpWin FROM 1,1 TO 2,2
        ACTIVATE WINDOW lcTmpWin NOSHOW
        
        *E300871,4 If it is a form, restore its settings
		lcRpName   = ''
	    DECLARE laSettings[1,2]
        *B802284,1 SSH 23/05/1999 Give the user the ability to scan style or upc
        *--- Open Cash Drawer
        *--- lcCashSeq == Drawer Seq.
        *--- lcCashCom == Drawer Com.
        lcOldPrn = SET('PRINTER')
        SET PRINTER TO &lcCashCom
        SET DEVICE TO PRINTER
        IF TYPE('EVAL(lcCashSeq)') <> 'U'
          @ 0,0 SAY &lcCashSeq
        ENDIF
        SET PRINTER TO
        SET DEVICE TO SCREEN
        *---  Restore old printer setting
        *B802284,1 SSH(END)
	    IF gfChkForm('PSRCPT', @lcRpName, @laSettings)
	       *-- Restore form settings
	      IF !EMPTY(laSettings[1,1])
	        FOR lnCount = 1 TO ALEN(laSettings,1)
              &laSettings[lnCount, 1] = laSettings[lnCount, 2]
            ENDFOR
          ENDIF
          *:B#604544,1 SSH Direct the out put to module setup port.
          PRIVATE lcOldPort,lcPsPort
          lcPsPort    = ""
          lcOldPort   = gcPrnt_Port
          lcPsPort    = gfGetMemVar('M_PORT')
          gcPrnt_Port = IIF(TYPE("lcPsPort")="C" .AND. !EMPTY(lcPsPort),lcPsPort,gcPrnt_Port)
          *:B#604544,1 SSH Direct the out put to module setup port.  
		  DO gfDispRe WITH gcRepHome+gcWinAppl+"\"+lcRpName
          *:B#604544,1 SSH Direct the out put to module setup port.
          gcPrnt_Port = lcOldPort
          *:B#604544,1 SSH Direct the out put to module setup port.
        ENDIF
	    *E300871,4 end

        *DO gfDispRe WITH gcRepHome+gcWinAppl+"\"+lcRpName
        RELEASE WINDOW lcTmpWin
        gcDevice = lcDevice 
        gcOutFile = lcOutFile
        SET CONSOLE &lcConsSet
        SET PRINT TO

        *B602138,1 Start.
        *--Display Invoice/Credit memo sequence.
        IF lnSr =1
          lcSMsgDsp ='Invoice is saved as '+lcInvoiceNo
        ELSE
          lcSMsgDsp ='Credit Memo is saved as '+lcCrMemoNo
        ENDIF
        *--Do you wish to reprint receipt ?  [Yes/No]
        IF gfModalGen('QRM52006B42002','DIALOG',lcSMsgDsp) = 2
          EXIT
        ENDIF
        *B602138,1 End.
      ENDDO
    
      SELECT (lcPosTmp)
      DELETE TAG TempInx
    ENDIF


    IF ! llUpdOver    
      *--Close uncmsess after finishing saving.
      =lfCrUnSess('C',.F.,.F.)
    ENDIF

    *E301508,1 Send Sales to Back Office if communication module is installed
    IF 'NC' $ gcCmpModules
      *E301508,1 Get Back office site id
      SELECT CODES
      SET ORDER TO TAG Idrltfname
      =SEEK('NYCSITEID')
      LOCATE REST WHILE cdefcode+crltfield+cfld_name = 'NYCSITEID' ;
                  FOR   cRltd_Nam = 'CCMSITETYP' AND cRltd_Vlu= 'B'
      IF FOUND()
        lcSiteId = Codes.cCode_No
        =gfOpenFile(gcDataDir+'EDIACPRT',gcDataDir+'ACCFACT','SH')
        =gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PARTTRANS','SH')
        =gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')
        SELECT EDiAcPrt
        LOCATE FOR cSiteId = lcSiteId
        IF FOUND() 
          *E301508,1 Add record for invoice
          IF SEEK(EdiAcPrt.cPartCode+'810','EdiPd')
            SELECT EdiTrans
            *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
            *IF !SEEK('810'+PADR(lcPosSeq,20)+EdiAcPrt.Type+EdiAcPrt.cPartner)
            IF !SEEK('810'+PADR(lcPosSeq,40)+EdiAcPrt.Type+EdiAcPrt.cPartner)
            *E037853,1 [End]
              INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER,LINTERCOMP) VALUES ;
                                       ('810',lcPosSeq,EdiAcPrt.Type,EdiAcPrt.cPartner,EdiAcPrt.lInterComp)
            ENDIF
            REPLACE cStatus WITH 'N'
            =gfAdd_Info('EDITRANS')
          ENDIF  
          *E301508,1 Add record for product activity
          IF SEEK(EdiAcPrt.cPartCode+'852','EdiPd')
            SELECT EdiTrans
            lcPeriod = DTOS(ldPostDate)+'-'+DTOS(ldPostDate)
            *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
            *IF !SEEK('852'+PADR(lcPeriod,20)+EdiAcPrt.Type+EdiAcPrt.cPartner)
            IF !SEEK('852'+PADR(lcPeriod,40)+EdiAcPrt.Type+EdiAcPrt.cPartner)
            *E037853,1 [End]
              INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER,LINTERCOMP) VALUES ;
                                       ('852',lcPeriod,EdiAcPrt.Type,EdiAcPrt.cPartner,EdiAcPrt.lInterComp)
            ENDIF
            REPLACE cStatus WITH 'N'
            =gfAdd_Info('EDITRANS')
          ENDIF  
        ENDIF
      ENDIF
      SET ORDER TO TAG CODES IN CODES
      SELECT (lcPosTmp)
    ENDIF
    *E301508,1 (End)

  ENDIF
  *--Update was Finished.

  *--Go back and setup to not display the current payments.
  SELECT (lcPosTmp)

  *C101660,1 WAB - change the filter to 4 and 'F' cause we add type 'F' FOr Freight 
  *C101660,1 WAB - START
  *SET FILTER TO TranType <> '4'
  SET FILTER TO !(TranType $ '4FR')
  *C101660,1 WAB - END

  SET ORDER TO


  *--If return with exchange go to sale.
  IF lnSr = 2 AND llExchange
    lnSr = 1
    lnSCount = 0
    =lfCrUnSess('O',.T.,.F.,.T.)
*    *--Check if it is not a miselenus customer excange.
*    IF lcCustomer = lcMisAcct
*      SHOW GET pbProceed DISABLE
*    ENDIF
    
    lcSRDesc = 'Sale'
    =lfRefresh()
    SHOW GETS  
    _CUROBJ = OBJNUM(lcStyle)
  ELSE
    laScrMode[4] = .F.
    laScrMode[1] = .T.
    =lfClrInfo()
    =lfRefresh()
    *C102047,1 ABD Open addtional Discount. [Begin]

    *B604248,1 ABD - Remak the next variable that we didn't need it now.[Begin]
    *llFirstTme = .T.
    *B604248,1 ABD - [End]

    *C102047,1 ABD [End]
    SHOW GETS  
    *_CUROBJ = OBJNUM(lcCustomer)
    _CUROBJ = OBJNUM(llmis)
    SHOW GET pbProceed,1 PROMPT lcClose
  ENDIF
ELSE
  CLEAR READ
  *--Check uncmsess record.
  =lfCrUnSess('I',.F.,.F.)
ENDIF

*B803961,1 ABD - Add 2 Variable to hold the Credit Card number and the approval number.
*B803961,1 ABD - this 2 variable will use in case not multi payment (Specially). [Begin]
STORE '' To lcCrdData , lcAppDesc
*B803961,1 ABD - [End]

RETURN


*:*************************************************************
*: Name      : lfvSLogOut.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Screen Log Out and Validate password.
*:*************************************************************
*: Calls     : Screen PSPOSW.SPX 
*:             lfvUsrPass()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvSLogOut()
*:*************************************************************
FUNCTION lfvSLogOut

IF !llUsersExt
  RETURN
ENDIF  
*--Screen title message.
lcUsrMsg = 'User <'+ ALLTRIM(gcUserName) +'> locked the workstation.'
*--Current user password.
lcCurUsrPas = IIF(SEEK(gcUser_id,'SYUUSER'),ALLTRIM(SYUUSER.cUsr_Pass),'*')

lcUsrPass=SPACE(8)
PUSH KEY
ON KEY
SET CONFIRM OFF
llWrongPass = .T.
DO WHILE llWrongPass
  lcUsrPass=SPACE(8)
  DO (gcScrDir+gcWinAppl+"\PSPOSW.SPX")
  IF ! llWrongPass
    EXIT
  ENDIF
ENDDO
POP KEY
SET CONFIRM ON
RETURN

*:*************************************************************
*: Name      : lfvUsrPass.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Validate user password.
*:*************************************************************
FUNCTION lfvUsrPass

IF (!EMPTY(lcCurUsrPas) AND SYS(2007,ALLTRIM(lcUsrPass))=lcCurUsrPas) OR ;
   ( EMPTY(lcCurUsrPas) AND PADR(lcUsrPass,8)=PADR(lcCurUsrPas,8) )
  llWrongPass = .F.
  CLEAR READ
ELSE
  *--Invalid user password.
  =gfModalGen('TRM42138B42000','DIALOG','user password')
  lcUsrPass=SPACE(8)
  llWrongPass = .T.
  _CUROBJ=OBJNUM(lcUsrPass)
  RETURN
ENDIF

*:*************************************************************
*: Name      : lfvStyle.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Validate Styles Bar Code entry or manual entry.
*:*************************************************************
*: Calls     : lfRefresh(),lfwBrows(),gfStyBrw()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvStyle()
*:*************************************************************
FUNCTION lfvStyle

*B602138,1 Added confirmation by setup in When function of style object.
*lcCnfrm = IIF(llConfirm,'ON','OFF')
*SET CONFIRM &lcCnfrm
*B602138,1 End.
SET CONFIRM ON
IF EMPTY(SUBSTR(lcStyle,1,1))
  lcStyle=' '
  SHOW GET lcStyle
  RETURN
ENDIF

*--If style has space in bar code.
*E500241,1 SSH 03/05/1999 Add the following if condition to valid on style
*E500241,1 SSH            if and only if system was setup to scan style.

*B124698,1 NNA 10/28/2004 (Begin) Stop this Part Because it Make style browser open even you enter
*B124698,1 NNA            a valid or invalid style code that beacuse this code change the style structure
*IF !llUPC
*  IF LEN(ALLT(lcStyle)) < 20 AND LEN(lcStyPict)=20
     *As exple 
     *lcStyle = '98WK1SH/GEOT/H1BK11'  &&bar code style
     *lcStyle = '98WK1SH/GEO /TH1BK11'  && style
*     lcDsnSeg=STRTRAN(SUBSTR(lcStyle,9),'/','')
*     lcStyle=PADR(ALLTRIM(SUBSTR(lcStyle,1,8)+SUBSTR(lcDsnSeg,1,3)+' /'+SUBSTR(lcDsnSeg,4)),20)
*  ENDIF
*ENDIF
*B124698,1 NNA (End)

*E500241,1 SSH(END)
*--Ckeck existance of multiple payments.
*C200153,4 ABD Select the temp file when set filter. [Begin]
SELECT (lcPosTmp)
*C200153,4 ABD [End]
SET FILTER TO TranType = '4'
GO TOP

IF !EOF()
  *--You cannot 'add a new line' since multiple payments was entered,
  *--Please remove the payments first.
  =gfModalGen('INM42135B42000','DIALOG','add a new line')
  lcStyle=' '
  SHOW GET lcStyle
  RETURN
ENDIF

*C101660,1 WAB - change the filter to 4 and 'F' cause we add type 'F' FOr Freight 
*C101660,1 WAB - START
*SET FILTER TO TranType <> '4'
SET FILTER TO !(TranType $ '4FR')
*C101660,1 WAB - END


*E500241,1 SSH 03/05/1999 Add the following part to valid on UPC number if
*E500241,1 SSH            setup to use UPC number
IF llUPC
  lcOldAlias = SELECT(0)
  SELECT STYLEUPC
  SET ORDER TO STYUPCN

  *C200145,1 ABD- Add new trigger for custom RSH10 that have 6 chr for
  *C200145,1 ABD- The UPC number in the Styleupc file & we will change 
  *C200145,1 ABD- The picture of the field & len of seek expration. [Begin]
  *IF !SEEK(PADR(lcStyle,12))
  IF !SEEK(PADR(lcStyle,lnUPCnum))  
    *C200145,1 ABD- [End]
    lcHdEsc1= ON('KEY','ESC')
    ON KEY LABEL ESC
    *E500241,1 Function to browse UPC number.
    = lfUpcBrow()
    ON KEY LABEL ESC &lcHdEsc1
  ELSE
    lcStyle  = STYLEUPC.Style+STYLEUPC.Size
    lcIStyle = STYLEUPC.Style
    *B604185,1 WAB (START) - in case of UPC we must seek i the style file to optain the correct field
    =SEEK(lcIStyle,'STYLE')
    *B604185,1 WAB (END)
  ENDIF
  SET ORDER TO STYLEUPC
  SELECT(lcOldAlias)
  IF EMPTY(SUBSTR(lcStyle,1,1))
    lcStyle=' '
    SHOW GET lcStyle
    RETURN
  ENDIF
ENDIF
*E500241,1 SSH (END)
lcISize  = SUBSTR(lcStyle,LEN(lcItmPict)+1,1)
lcIStyle = SUBSTR(lcStyle,1,LEN(lcItmPict))
*E500241,1 SSH 03/05/1999  Add the condition that setup is not UPC.

*B603687,1 ABD - If you return invoice & setup  allow return without invoice is false we will
*B603687,1 ABD - Browse the styles only from the invoice line else we will browse all 
*B603687,1 ABD - Styles from style file.[Begin]
IF lnSr = 2 .AND. !llAlowRet .AND. !EMPTY(lcInvNo) and !SEEK(lcIStyle+lcInvNo,'INVLINE') ;
  =lfInvlBrow()
  IF EMPTY(lcIStyle)
    lcStyle =' '
    SHOW GET lcStyle
    RETURN
  ELSE
    FOR lnI=1 TO 8
      lcI = STR(lnI,1)
      IF INVLINE.QTY&lcI <> 0
        EXIT
      ENDIF
    ENDFOR   
    lcISize = ALLT(STR(lnI,1))
  ENDIF
  SELECT (lcPosTmp)    
ENDIF
*B603687,1 ABD -  [END]

*E500241,1 SSH 03/05/1999  Add the condition that setup is not UPC.
*IF !SEEK(lcIStyle,'STYLE')
IF !llUPC AND !SEEK(lcIStyle,'STYLE')
  *E500241,1 SSH (END)
  lcHdEsc1= ON('KEY','ESC')
  ON KEY LABEL ESC
  lcIStyle = gfStyBrw('I','','',.F.)
  ON KEY LABEL ESC &lcHdEsc1
  =lfActBrow()
  IF EMPTY(SUBSTR(lcIStyle,1,1))
    lcStyle=' '
    SHOW GET lcStyle
    RETURN
  ELSE
    *B802525,1 Start.
    IF SEEK('S'+STYLE.Scale,'SCALE') AND SCALE.Cnt <> 1
      SELECT SCALE
      SCATTER FIELDS SZ1,SZ2,SZ3,SZ4,SZ5,SZ6,SZ7,SZ8 TO laStySizes
      SELECT (lcPosTmp)

      *B802834,1 KHM 11/30/99 (Begin) Changing the Dimension of the Array.
      *DIME laStySizes[ASCAN(laStySizes,SPACE(5))-1]      
      DIME laStySizes[SCALE.Cnt]
      *B802834,1 KHM 11/30/99 (End)
      
      lnStySizes = 1
      =lfSelSize()
      lcISize = ALLT(STR(lnStySizes,1))
    ENDIF
    *B802525,1 End.
  ENDIF
ENDIF

lcStyle  = lcIStyle+lcISize
*lcISize = IIF(EMPTY(lcISize) OR lcISize='0','1',lcISize)
lcISize = IIF(!EMPTY(lcISize) AND lcISize $ '12345678',lcISize,'1')
lnTCnt  = IIF(SEEK('S'+STYLE.Scale,'SCALE'),SCALE.Cnt,1)

*--Apply style validation.
llErrValid = .F.
DO WHILE .T.
  *--1) Invalid size for this style scale'
  IF lnTCnt < VAL(lcISize)
    =gfModalGen('TRM42138B42000','DIALOG','size for this style scale')
    llErrValid = .T.
    EXIT
  ENDIF

  *--2) Check if this style exist in warehous.
  IF !SEEK(PADR(lcIStyle,19)+lcStoreCode,'STYDYE')
    *-Style: xxx is not assigned to warehouse: xxx.
    =gfModalGen('QRM42025B42001','DIALOG',lcIStyle+'|'+ALLTRIM(lcStoreCode) )
    llErrValid = .T.
    EXIT
  ENDIF

  *--3) E301264,1 Dont allow to sale what not in stock.
  IF lnSr=1 AND !llAcpZrSk
    IF STYDYE.Stk&lcISize <=0
      *--No stock available for sale in style/size : xxx/xxx.
      =gfModalGen('TRM52012B42000','DIALOG',PADR(lcIStyle,19)+'/'+ALLT(SCALE.SZ&lcISize))
      llErrValid = .T.
      EXIT
    ELSE
      SELECT (lcPosTmp)    
      SUM ALL TotQty FOR Style=lcIStyle AND STR(CNT,1)=lcISize TO lnPrvSlQt
      GO TOP
      IF STYDYE.Stk&lcISize - lnPrvSlQt <=0
        *--No stock available for sale in style/size : xxx/xxx.
        =gfModalGen('TRM52012B42000','DIALOG',PADR(lcIStyle,19)+'/'+ALLT(SCALE.SZ&lcISize))
        llErrValid = .T.
        EXIT
      ENDIF
    ENDIF
  ENDIF

  *--4) Prevent the user from selling items with 0 price.
  IF lnSr=1 AND !llAcpZrPr AND STYLE.nSugRetPri=0
    *--The style xxx has price zero ,unable to proceed.
    =gfModalGen('TRM52013B42000','DIALOG',PADR(lcIStyle,19))
    llErrValid = .T.
    EXIT 
  ENDIF
  *E301264,1 End.

  *--5) E301264,1 (Start) Check that return is from invoice and make sure that
        *E301264,1 the returnd style exist in invoice.
  IF lnSr=2 AND !llAlowRet AND EMPTY(lcInvNo)
    *-You have to select the invoice for this return.
    =gfModalGen('TRM52010B42001','DIALOG')
    llErrValid = .T.
    EXIT
  ENDIF


  EXIT
ENDDO 

*--Not valid style.
IF llErrValid
  lcStyle=' '
  SHOW GET lcStyle
  _CUROBJ=OBJNUM(lcStyle)
  RETURN
ENDIF

*C200153,4 ABD - Valid the selected style in case you enter the invoice number
*C200153,4 ABD - And select stle not related to the invoice line. [Begin]
*IF lnSr=2 AND !llAlowRet AND !EMPTY(lcInvNo)
IF lnSr=2 .AND. llAlowRet .AND. !EMPTY(lcInvNo)
  *C200153,4 ABD - [End]
  SELECT INVLINE
  lcCurTag = ORDER()
  SET ORDER TO TAG Invlines
  = SEEK(PADR(lcIStyle,19)+lcInvNo)
  LOCATE REST WHILE Style+Invoice=PADR(lcIStyle,19)+lcInvNo FOR Qty&lcISize # 0 
  llLineExst = FOUND()
  IF !llLineExst
    SET ORDER TO (lcCurTag)
    *-Style: xxx does not exist in invoice # xxx.
     =gfModalGen('TRM52008B42001','DIALOG',lcIStyle+'|'+lcInvNo)
    lcStyle=' '
    SELECT (lcPosTmp)    
    _CUROBJ=OBJNUM(lcStyle)
    RETURN
  ENDIF
  SELECT POSTRAN
  SET ORDER TO TAG Postrln
  llAloRetSt = .F.
  SELECT INVLINE
  SCAN WHILE Style+lcInvNo=PADR(lcIStyle,19)+lcInvNo FOR Qty&lcISize # 0
    IF !SEEK(lcInvNo+PADR(lcIStyle,19)+STR(INVLINE.LINENO,6),'POSTRAN')
      SELECT (lcPosTmp)    
      LOCATE FOR Style=INVLINE.STYLE AND LINENO=INVLINE.LINENO AND CNT = VAL(lcISize)
      IF !FOUND()     
        llAloRetSt = .T.
        EXIT 
      ENDIF
    ENDIF
  ENDSCAN

  SELECT POSTRAN
  SET ORDER TO TAG PostrnA
  SELECT INVLINE
  SET ORDER TO (lcCurTag)
  IF !llAloRetSt
    *--This style already returned.
    =gfModalGen('TRM52016B42000','DIALOG')
    lcStyle=' '
    SELECT (lcPosTmp)    
    _CUROBJ=OBJNUM(lcStyle)
    RETURN
  ENDIF

  lnSOrjPr = INVLINE.Gros_Price
  lnStylDi = INVLINE.Disc_Pcnt
  lnPSLine = INVLINE.LineNo

  *B603687,1 ABD Fix Error Disapearing tax amount in return transaction. [Begin]
  lnStyTRt = 0
  SELECT POSTRAN
  IF SEEK(Invline.Account+lcInvNo,'POSTRAN')
    LOCATE REST FOR Tran + STYLE + STR(lineno,6) = lcInvNo + Invline.Style+STR(Invline.lineno,6)
    IF FOUND() .AND. !EMPTY(tax_rate)
      lnStyTRt = tax_rate
    ENDIF
  ENDIF    
   SELECT (lcPosTmp)
  *B603687,1 ABD [End]
ELSE
  lnSOrjPr = STYLE.nSugRetPri
  STORE 0 TO lnStylDi,lnStyTRt
  *C102047,1 ABD - Get the discount from stydye file not from style file [Begin]
  *IF !EMPTY(STYLE.cDiscCode)
  lcDisType = ''
  IF !EMPTY(STYDYE.cDiscCode)
    *C102047,1 ABD - [End]
    *E301116,1 Start. 
    STORE {} TO ldStrDat,ldEndDat
    *C102047,1 ABD - Add new related filed to now whoch effect will be this code
    *C102047,1 ABD - if retail we will take it else not.     [Begin]
    *DECLARE laTrmRltFd[3,2]
    DECLARE laTrmRltFd[4,2]
    laTrmRltFd[1,1] = 'DISCPCNT  '
    laTrmRltFd[1,2] = 'lnStylDi'
    laTrmRltFd[2,1] = 'START     '
    laTrmRltFd[2,2] = 'ldStrDat'
    laTrmRltFd[3,1] = 'DENDATE   '
    laTrmRltFd[3,2] = 'ldEndDat'

    *C102051,1 ABD - Add new related filed to now which effect will be do this code
    *C102051,1 ABD - if retail we will take it else no. [Begin]
    laTrmRltFd[4,1] = 'CCOSTAFECT'
    laTrmRltFd[4,2] = 'lcDisType'
    *E301116,1 End.
    =gfRltFld(STYDYE.cDiscCode ,@laTrmRltFd,"CDISCCODE")
    *C102047,1 ABD - Add new validation on the discount code up on 
    *C102047,1 ABD - add new option about whole sale or retail sale [begin]
    IF ALLTRIM(lcDisType) $ 'BR'
      *C102047,1 ABD - [End]

      *B804017,1 ABD - Fix Problem that the discount perc. didn't accu. the discount 
      *B804017,1 ABD - In case We leave the date empty. [Begin]
      *IF EMPTY(ldStrDat) OR EMPTY(ldEndDat) OR !BETWEEN(ldPostDate,ldStrDat,ldEndDat)
      *IF (EMPTY(ldStrDat)  .AND. !EMPTY(ldEndDat)) .OR. ;
      *   (!EMPTY(ldStrDat) .AND.  EMPTY(ldEndDat)) .OR. ; 
      *   (!EMPTY(ldStrDat) .AND. !EMPTY(ldEndDat) .AND. !BETWEEN(ldPostDate,ldStrDat,ldEndDat))
      *
      *B804017,1 ABD - [Begin]
      IF (EMPTY(ldStrDat)  .AND. !EMPTY(ldEndDat) .AND. ldEndDat < gdSysDate) .OR. ;
         (!EMPTY(ldStrDat) .AND.  EMPTY(ldEndDat) .AND. ldStrDat > gdSysDate) .OR. ; 
         (!EMPTY(ldStrDat) .AND. !EMPTY(ldEndDat) .AND. !BETWEEN(ldPostDate,ldStrDat,ldEndDat))
         *B804017,4 ABD - [End]
         *B804017,1 ABD - [End]
         *B804017,1 ABD - [End]
        lnStylDi = 0
      ENDIF
      *C102047,1 ABD - Else we will ass. Zero to discound because it is not retail discound.
    ELSE
      lnStylDi = 0
    ENDIF
    *C102047,1 ABD End For If statement. [End]
  ENDIF
  lnPSLine = 0

  *B603192,1 Read default tax from customer.
  lnStyTRt = CUSTOMER.nTaxRate
   
ENDIF
*E301264,1 End.

*C102047,1 ABD Get the sales rep and  comm. from invhdr file & invline file
*C102047,1 ABD in case return from invoice.  [Begin]
IF lnSr=2 .AND. !EMPTY(lcInvNo)
  lnOldAls = SELECT (0)
  lnInvDsc  = INVHDR.DISCPCNT
  lcInvDCd  = ""
  lcInvRep  = ""
  lnInvRpCm = 0
  SELECT POSTRAN
  lcOldTag = TAG()
  SET ORDER TO TAG POSTRNA
  IF SEEK(INVLINE.Account+INVLINE.invoice)
    LOCATE REST FOR Style = INVLINE.Style .AND. LineNo = INVLINE.LineNo
    IF FOUND()
      lcInvDCd  = caddretdis
      lcInvRep  = Rep1 
      lnInvRpCm = Comm1
    ENDIF
  ENDIF
  SET ORDER TO TAG (lcOldTag)
  SELECT(lnOldAls)
ENDIF
*C102047,1 ABD [End]

*--Append new Sales or Return Style record.
*C101660,1 WAB - add paramter to lfappndln()
*C101660,1 WAB - START
*= lfAppndLn(IIF(lnSr=1,'1','0'),'',ldPostDate,'',lcRefer,lcIStyle,lnSOrjPr,lnStylDi,lnStyTRt,IIF(lnSr=1,1,-1),0,VAL(lcISize),.T.,lnPSLine)
*= lfAppndLn(IIF(lnSr=1,'1','0'),'',ldPostDate,'',lcRefer,lcIStyle,lnSOrjPr,lnStylDi,lnStyTRt,IIF(lnSr=1,1,-1),0,VAL(lcISize),.T.,lnPSLine,0)
*B603192,1 Pass default tax amount from customer.

*C102047,1 ABD add the sales rep and  comm. from invhdr file & invline file
*C102047,1 ABD in case return from invoice.  [Begin]
*= lfAppndLn(IIF(lnSr=1,'1','0'),'',ldPostDate,'',lcRefer,lcIStyle,lnSOrjPr,lnStylDi,lnStyTRt,IIF(lnSr=1,1,-1),0,VAL(lcISize),.T.,lnPSLine,IIF(lnStyTRt<>0,((lnStyTRt/100)*(lnSOrjPr-((lnStylDi/100)*lnSOrjPr))),0))
IF lnSr=2 .AND. !EMPTY(lcInvNo)
  = lfAppndLn(IIF(lnSr=1,'1','0'),'',ldPostDate,'',lcRefer,lcIStyle,lnSOrjPr,lnStylDi,lnStyTRt,IIF(lnSr=1,1,-1),0,VAL(lcISize),.T.,lnPSLine,IIF(lnStyTRt<>0,((lnStyTRt/100)*(lnSOrjPr-((lnStylDi/100)*lnSOrjPr))),0),lcInvRep+"*"+STR(lnInvRpCm,6,2)+"*"+PADR(lcInvDCd,6)+"*"+STR(lnInvDsc,6,2))
ELSE
  = lfAppndLn(IIF(lnSr=1,'1','0'),'',ldPostDate,'',lcRefer,lcIStyle,lnSOrjPr,lnStylDi,lnStyTRt,IIF(lnSr=1,1,-1),0,VAL(lcISize),.T.,lnPSLine,IIF(lnStyTRt<>0,((lnStyTRt/100)*(lnSOrjPr-((lnStylDi/100)*lnSOrjPr))),0))  
ENDIF  
*C102047,1 ABD [End]

*C101660,1 WAB - END

SELECT (lcPosTmp)

lnTotal = lnTotal+nAmount

*E301264,1 Start.
*lnPaid  = IIF(!llMltPay,lnTotal,lnPaid)
IF lnSr=1
  lnPaid  = IIF(!llMltPay,lnTotal,lnPaid)
  lnPaid  = IIF(lnPaid<0,0,lnPaid)

  *B803962,1 ABD Add new setup to ask the user to display 
  *B803962,1 ABD the amount tender or not. [Begin]
  IF llDsAmntNc
    lnTender = lnPaid
  ENDIF
  *B803962,1 ABD [End]
ENDIF
*E301264,1 End.


*B803962,1 ABD - Add New setup to return the sale and display the total amount
*B803962,1 ABD - In the Paid amount or set it as it work. [Begin]
IF lnSr=2 .AND. llALLRTAMT .AND. !llExchange
*B604522,1 MAN This new setup 'The amount tender display' should not affect the paid amount
  lnPaid = lnTotal
ENDIF
*B803962,1 ABD - [End]

*--Show Paid,Tendered and Calculate Change amounts.

=lfvPaid()

lnRecNo = RECNO()
SHOW WINDOW (lcBrowTitl) REFRESH
lcStyle=SPACE(20)
SHOW GET lcStyle

*--Since records was added allow remove and modify.
SHOW GET pbRemove  ENABLE
SHOW GET pbEdit    ENABLE

*C101660,1 WAB - case ther are on line enabled the freight butt.
*C101660,1 WAB - START
IF llDispFrght 
  IF !llNoFreight .OR. lnSr = 1
    lcCntFrght = 'ENABLE'
    SHOW GET pbFreight &lcCntFrght
  ENDIF
ENDIF
*C101660,1 WAB - END


*E301264,1 Start.
IF llAlwRfnd OR (lnSr=1 AND (!llExchange OR lnPaid > 0))
   SHOW GET lnPaid    ENABLE
   SHOW GET lnPayTerm ENABLE
ENDIF
*E301264,1 End.
SHOW GET lcRefer   ENABLE
SHOW GET lnTender  ENABLE
SHOW GET pbProceed,1 PROMPT lcProc ENABLE

*C102047,1 ABD Enable the Add. discount code field. [End]
*B604248,1 ABD - Remak the next variable that will didn't need it now & 
*B604248,1 ABD - Disable the add. discount popup in case return with 
*B604248,1 ABD - invoice or made achange . [Begin]
*IF llFirstTme

*B604253,1 ABD - Enable the the additional discount in case we made change. [Begin]
*IF lnSr=2 AND !EMPTY(lcInvNo) .OR. llExchange
IF lnSr=2 AND !EMPTY(lcInvNo)
  *B604253,1 ABD - [End]
  SHOW GET lnAddDisc DISABLE
ELSE
  *C200153,1 ABD - Show get the additional discount disable in case we 
  *C200153,1 ABD - have customer discount. [Begin]

  *C200153,4 ABD - Open the additional discount in case of the customer ]
  *C200153,4 ABD - Have not discount. [Begin]
  *IF !llCustDsc .OR. llExchange
  IF (!llCustDsc .AND. llExchange) .OR. !llCustDsc
    *C200153,4 ABD - [End]
    SHOW GET lnAddDisc ENABLE
  ENDIF
  *C200153,1 ABD - [End]
ENDIF
*B604248,1 ABD - [End]

*C102047,1 ABD [End]
*E301264,1 disable the invoice field. 
IF lnSr=2 AND !EMPTY(lcInvNo)
  SHOW GET lcInvNo DISABLE
ENDIF
*E301264,1 End.

*B604180,1 ABD -Disable filed payment amount in case do not leave 
*B604180,1 ABD -Opend amount setups. [Begin]
*B606415,1 SSH Fix problems in Paid Amount
*IF laSetups[17,2]
IF (lnSr = 1 .AND. laSetups[17,2]) .OR. (lnSr = 2 .AND. llALLRTAMT)
*B606415,1 SSH Fix problems in Paid Amount
  SHOW GET lnPaid DISABLE
ENDIF
*B604180,1 ABD - [End]

*--Check Open uncmsess record.
=lfCrUnSess('O',.T.,.F.)

*:B#604544,1 SSH Calculate the tax.
SELECT (lcPosTmp)
SCAT MEMVAR MEMO
m.Tax_Rate = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
llTax = .T.
=lfvTaxP("")
=lfvEdit("D")
*:B#604544,1 SSH [END]

=lfRefresh()
_CUROBJ=OBJNUM(lcStyle)
RETURN


*:*************************************************************
*: Name      : lfAppndLn.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Append line to Temp POSTran file.
*:*************************************************************
*: Calls     : 
*:*************************************************************
*: Passed Parameters  : lcPTrTp=>Transaction type
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfAppndLn()
*:*************************************************************
FUNCTION lfAppndLn
*C101660,1 WAB - add paramter (ax amount)
*C101660,1 WAB - START
*PARA lcPTrTp,lcPTran,ldPTrDate,lcPTrnCod,lcPRef,lcPStyle,;
     lcPStyPric,lcPStyDist,lcPStyTxRt,lnPQty,lnPAmnt,lnPCnt,;
     llNeedCnt,lnLinNo
PARA lcPTrTp,lcPTran,ldPTrDate,lcPTrnCod,lcPRef,lcPStyle,;
     lcPStyPric,lcPStyDist,lcPStyTxRt,lnPQty,lnPAmnt,lnPCnt,;
     llNeedCnt,lnLinNo,lcPStyTxMT,lcRetFrmDS
*C101660,1 WAB - END

lnAlias = SELECT()
lcPStyDesc=''
IF !EMPTY(lcPStyle)
  =SEEK(lcPStyle,'STYLE')
  lcPStyDesc = IIF(!EMPTY(STYLE.Desc1),STYLE.Desc1,STYLE.Desc)
  lcScale    = STYLE.Scale 
  lcCnt      = STR(lnPCnt,1)
  lcSz1      = IIF(SEEK('S'+lcScale,'SCALE'),SCALE.Sz&lcCnt,'')
ELSE
  lcPStyDesc = lcPRef
  STORE '' TO lcScale,lcSz1
ENDIF
*C102047,1 ABD Cheak on the paramt if def. or not. [Begin]
lcRetFrmDS = IIF(TYPE('lcRetFrmDS')='U',"",lcRetFrmDS)
*C102047,1 ABD [End]

SELECT (lcPosTmp)
APPEND BLANK
*C102047,1 ABD Update the temp file with the sales rep and Add discount. [Begin]
*-- Add The Sales rep and the comm for the sales rep & add. Discount .
*REPLACE TranType  WITH lcPTrTp,;
        TranDate  WITH ldPTrDate,;
        Tran      WITH lcPTran,;
        TranCode  WITH lcPTrnCod,;
        Account   WITH lcCustomer,;
        StName    WITH lcName,;
        Phone1    WITH lcPhone,;
        Style     WITH lcPStyle,;
        Reference WITH lcPRef,;
        Desc1     WITH lcPStyDesc,;
        Scale     WITH lcScale,;
        Cnt       WITH lnPCnt,;
        Sz1       WITH lcSz1,;
        LineNo    WITH lnLinNo,;
        dPostDate WITH ldPostDate,;
        Invoice   WITH IIF(lnSr=2,lcInvNo,'')

REPLACE TranType   WITH lcPTrTp    ,;
        TranDate   WITH ldPTrDate  ,;
        Tran       WITH lcPTran    ,;
        TranCode   WITH lcPTrnCod  ,;
        Account    WITH lcCustomer ,;
        StName     WITH lcName     ,;
        Phone1     WITH lcPhone    ,;
        Style      WITH lcPStyle   ,;
        Reference  WITH lcPRef     ,;
        Desc1      WITH lcPStyDesc ,;
        Scale      WITH lcScale    ,;
        Cnt        WITH lnPCnt     ,;
        Sz1        WITH lcSz1      ,;
        LineNo     WITH lnLinNo    ,;
        dPostDate  WITH ldPostDate ,;
        Invoice    WITH IIF(lnSr=2,lcInvNo,'')
*C102047,1 ABD Get the sales rep and comm.   [Begin]
IF EMPTY(lcRetFrmDS)
  REPLACE CaddRetDis WITH laAddDisc[lnAddDisc,2],;
          REP1       WITH lcSRep

    IF SEEK(Style,'STYLE') .AND. STYLE.Commission
      IF llDIV_COMM
       IF SEEK(lcSRep+Style.cDivision,'REP_DIV')
         REPLACE Comm1  WITH REP_DIV.Comm_Rate
       ELSE
         = SEEK(lcsRep,'SALESREP')
         REPLACE Comm1 WITH SALESREP.Comm
       ENDIF  
      ELSE
        IF !EMPTY(lcCustomer) .AND. SEEK(lcCustomer,'CUSTOMER') .AND. CUSTOMER.SALESREP = lcsRep
          REPLACE Comm1  WITH CUSTOMER.Comm
        ELSE
          REPLACE Comm1  WITH lnCommRep
        ENDIF        
        
      ENDIF  
    ENDIF
ELSE
  *C102047,1 ABD the Paramt. value. 
  *lcRetFrmDS
  *xxx*999.99*xxxxxx*999.99
  *lcInvRep+"*"+STR(lnInvRpCm,6,2)+"*"+lcInvDCd+"*"+STR(lnInvDsc,6,2)

  REPLACE CaddRetDis WITH SUBSTR(lcRetFrmDS,12,6) ,;
          DiscPcnt   WITH VAL(SUBSTR(lcRetFrmDS,19,6)) ,;
          REP1       WITH SUBSTR(lcRetFrmDS,1,3)     ,;
          COMM1      WITH VAL(SUBSTR(lcRetFrmDS,5,6))
  lcSRep = REP1
  IF !EMPTY(CaddRetDis)
    =gfwCodePop(@laCodInfo,'CADDRETDIS','L')
    lnAddDisc = (ASCAN(laAddDisc,PADR(CaddRetDis,6))/2)

    *B604248,1 ABD - Remak the next cheak that we need add. discount enable. [Begin]
    *IF lnAddDisc<>0
    *SHOW GET lnAddDisc DISABLE
    *ENDIF
    *B604248,1 ABD - [End]

  ENDIF
  IF !EMPTY(lcSRep)
    SHOW GET lcSRep DISABLE
    SHOW GET ibRepBrow DISABLE
  ENDIF
ENDIF
*C102047,1 ABD [End]
*E301467,1 (Start)
IF llUseAtom
  REPLACE cCrdCard WITH lcPRef
ENDIF
*E301467,1 (End).

*-- Call global function to add audit fields info.
=gfAdd_Info(lcPosTmp)

=RLOCK()
IF lnPAmnt <> 0
  REPLACE nAmount WITH lnPAmnt
ELSE
*C101660,1 WAB - replace the tax_rate with total tax for all qty
*C101660,1 WAB - START
*  REPLACE Gros_Price WITH lcPStyPric,;
          Disc_Pcnt  WITH lcPStyDist,;
          Tax_Rate   WITH lcPStyTxRt,;
          Disc_Amt   WITH ((Disc_Pcnt/100)*Gros_Price),;
          Price      WITH (Gros_Price-Disc_Amt),;
          Tax_Amt    WITH ((Tax_Rate/100)*Price),;
          TotQty     WITH lnPQty,; 
          nAmount    WITH ((Price * TotQty) + Tax_Amt)

  REPLACE Gros_Price WITH lcPStyPric,;
          Disc_Pcnt  WITH lcPStyDist,;
          Tax_Rate   WITH lcPStyTxRt,;
          Disc_Amt   WITH ((Disc_Pcnt/100)*Gros_Price),;
          Price      WITH (Gros_Price-Disc_Amt),;
          Tax_Amt    WITH lcPStyTxMT*IIF(lnSr=2,-1,1),;
          TotQty     WITH lnPQty,; 
          nAmount    WITH ((Price * TotQty) + Tax_Amt)

  *C102047,1 ABD Get the additional Discount. [Begin]
  lnStylAdDi = 0
  *C200153,1 ABD - add new condition to cheak if this customer have a discount. [Begin]
  *IF !EMPTY(CaddRetDis) 
  IF !EMPTY(CaddRetDis) .OR. llCustDsc
    IF !EMPTY(CaddRetDis)
      *C200153,1 ABD - [End]
      STORE {} TO ldAdStrDat,ldAdEndDat
      DECLARE laTrmRltFd[3,2]
      laTrmRltFd[1,1] = 'DISCPCNT  '
      laTrmRltFd[1,2] = 'lnStylAdDi'
      laTrmRltFd[2,1] = 'START     '
      laTrmRltFd[2,2] = 'ldADStrDat'
      laTrmRltFd[3,1] = 'DENDATE   '
      laTrmRltFd[3,2] = 'ldAdEndDat'

      =gfRltFld(CaddRetDis,@laTrmRltFd,'CADDRETDIS')
      IF EMPTY(ldAdStrDat) OR EMPTY(ldAdEndDat) OR !BETWEEN(ldPostDate,ldAdStrDat,ldAdEndDat)
        lnStylAdDi = 0
      ENDIF
    ENDIF

    *C200153,1 ABD - add cheak if customer have discount , we will get the discount
    *C200153,1 ABD - from the customer file in case is sales else 
    *C200153,1 ABD - get the discount in the invoice. [Begin]
    IF llCustDsc
      IF EMPTY(lcRetFrmDS)
        lnStylAdDi = lnCustDsc
      ELSE 
        lnCustDsc  = DiscPcnt
        = lfwAddDisc()
      ENDIF 
    ENDIF
    *C200153,1 ABD - [End]

    *- Update discount Filed.
    IF EMPTY(lcRetFrmDS)
      REPLACE DiscPcnt   WITH lnStylAdDi
    ENDIF
  
    REPLACE DisCount   WITH ((DiscPcnt/100)*Price)    ,;
            Price      WITH (Price-DisCount)          ,;
            nAmount    WITH ((Price * TotQty) + Tax_Amt)            
  ENDIF
  *C102047,1 ABD [End]
  *C101660,1 WAB - END
ENDIF
IF llNeedCnt
  lnSCount = lnSCount + 1
  REPLACE nSCount WITH lnSCount
ENDIF
UNLOCK
*B803962,1 ABD - Active the new menu option. [Begin]
IF llUseAtom
  SET SKIP OF PAD _INQUIRY OF _MSYSMENU .F.
ENDIF  
*B803962,1 ABD - [End]
SELECT(lnAlias)
RETURN        


*!*************************************************************
*! Name      : lfOldValue
*! Developer : Timour A. K.
*! Date      : 02/02/98
*! Purpose   : Function to store old value of the current filed.
*!*************************************************************
*! Returns   : Return old tran no value
*!*************************************************************
*! Example   : =lfOldValue()
*!*************************************************************
FUNCTION lfOldValue

lcOldValue = EVALUATE(SYS(18))
RETURN


*:*************************************************************
*: Name      : lfvRemove.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Remove Style.
*:*************************************************************
*: Calls     : lfRefresh()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvRemove()
*:*************************************************************
FUNCTION lfvRemove

IF (lnSr=1 AND &lcPosTmp..TranType='1' AND &lcPosTmp..Cnt<>0) OR ;
   (lnSr=2 AND &lcPosTmp..TranType='0')
ELSE
  *-Only return line can be accepted to remove.
  =gfModalGen('TRM42133B42000','DIALOG',IIF(lnSr=1,'sales','return')+'|'+'remove')
  RETURN
ENDIF


*--Ckeck existance of multiple payments.
lnSvRec = RECNO()
SET FILTER TO TranType = '4'
GO TOP
llMltPExst = !EOF()

*C101660,1 WAB - change the filter to 4 and 'F' cause we add type 'F' FOr Freight 
*C101660,1 WAB - START
*SET FILTER TO TranType <> '4'
SET FILTER TO !(TranType $ '4FR')
*C101660,1 WAB - END

GOTO lnSvRec
IF llMltPExst
  *--You cannot 'remove this line' since multiple payments was entered,
  *--Please remove the payments first.
  =gfModalGen('INM42135B42000','DIALOG','remove this line')
  RETURN
ENDIF

*E301282,1 Changed.
*IF llUsersExt
IF llUsersExt AND !llDelAses
*E301282,1 End.
  *--Check Access to Remove.
  IF !lfActPassW(.T.)
    _CUROBJ = OBJNUM(pbRemove)
    RETURN
  ENDIF
ENDIF
  
SELECT (lcPosTmp)
*--Return invoice no.
IF lnSr = 2
  lcTranInv = Tran
ENDIF

*--Remove line.
lnDelAmnt = ABS(nAmount)
=RLOCK()
DELETE
UNLOCK


lnTotal = lnTotal - IIF(lnSr=1,lnDelAmnt,-lnDelAmnt)


*--Check if this is the last invoice line for deletion.
IF lnSr=2
  SET ORDER TO 1
  SEEK '0'
  LOCATE WHILE TranType = '0' FOR Tran = lcTranInv
  IF !FOUND()
    SEEK '1'
    LOCATE WHILE TranType = '1' FOR Tran = lcTranInv
    IF FOUND()
      DELETE
      SEEK '0'
      SUM REST nAmount WHILE TranType = '0' TO lnTotal
    ENDIF
    lcInvNo = SPACE(6)
    lnInvAmt= 0
    SHOW GET lcInvNo  ENABLE
    SHOW GET lnInvAmt ENABLE
  ENDIF    
  SET ORDER TO
  *--Return amount cannot be +ve.
  lnTotal = MIN(lnTotal,0)
ENDIF

*C101660,1 WAB - if there is no line in temp file we must delete frieght line
*C101660,1 WAB - and disable freight butt.
*C101660,1 WAB - START
IF llDispFrght
  SET ORDER TO 1
  SET FILTER TO TranType <> '4'
  COUNT TO lnRecNo
  IF SEEK(IIF(lnSr=1,'F','R')) AND lnRecNo = 1
    DELETE
    SUM REST nAmount WHILE TranType = '0' TO lnTotal
    lcCntFrght = 'DISABLE'
  ENDIF 
  SET FILTER TO !(TranType $ '4FR')
  SET ORDER TO
  IF lnRecNo = 0 
    lcCntFrght = 'DISABLE'
  ENDIF
  SHOW GET pbFreight &lcCntFrght
ENDIF
*C101660,1 WAB - END


*--Recount all
lnSCount = 0
SCAN FOR (lnSr=2 AND TranType='0') OR (lnSr=1 AND TRANTYPE='1' AND CNT<>0 )
  lnSCount = lnSCount + 1
  REPLACE nSCount WITH lnSCount
ENDSCAN
*--Delete last line disable remove.
GO TOP
IF EOF()
  SHOW GET pbRemove  DISABLE
  SHOW GET pbEdit    DISABLE
  SHOW GET lnPaid    DISABLE
  SHOW GET lnPayTerm DISABLE
  SHOW GET pbPay     DISABLE
  SHOW GET lcRefer   DISABLE
  SHOW GET lnTender  DISABLE
  _CUROBJ=OBJNUM(lcStyle)
  *C102047,1 ABD show get the discount field disable. [End]
  SHOW GET lnAddDisc DISABLE
  SHOW GET lcSRep    ENABLE
  SHOW GET ibRepBrow ENABLE

  *B604248,1 ABD - Remak the next variable that we didn't need it now. [Begin]
  *llFirstTme = .T.
  *B604248,1 ABD - [End]
  
  *C102047,1 ABD [End]
ENDIF
IF lnSr=2
  LOCATE FOR TranType = '0'
ELSE
  LOCATE FOR TRANTYPE = '1' AND CNT<>0
ENDIF
IF FOUND()
  SHOW GET pbProceed,1 PROMPT lcProc
ELSE
  *--Check if it is not a miselenus customer excange.
  IF lnSr = 1 AND llExchange  &&AND lcCustomer = lcMisAcct
    SHOW GET pbProceed,1 PROMPT lcCanl DISABLE
  ELSE
    SHOW GET pbProceed,1 PROMPT lcCanl ENABLE     
  ENDIF
  lnTender = 0
ENDIF

*E301264,1 Start.
*lnPaid  = IIF(!llMltPay,lnTotal,lnPaid)
*B803962,1 ABD -  Change the Paid amount in two case Sales or return. [Begin]
*IF lnSr=1
*B803962,1 ABD - [End]
lnPaid  = IIF(!llMltPay,lnTotal,lnPaid)
lnPaid  = IIF(lnPaid<0,0,lnPaid)
*B803962,1 ABD Add new setup to ask the user to display 
*B803962,1 ABD the amount tender or not. [Begin]
IF llDsAmntNc
  lnTender = lnPaid
ENDIF

*-- Add New setup to return the sale and display the totoal amount
*-- In the Paid amount or set it as it work.
IF lnSr=2 .AND. llALLRTAMT
*B604522,1 MAN This new setup 'The amount tender display' should not affect the paid amount
*  lnPaid = lnTotal
ENDIF

*E301264,1 End.

*--Show Paid,Tendered and Calculate Change amounts.

=lfvPaid()
GO TOP
=lfActBrow()
*--Check Open uncmsess record.
=lfCrUnSess('O',.T.,.F.)
=lfRefresh()
RETURN

*:*************************************************************
*: Name      : lfActPassW.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Check password for Remove or Edit.
*:*************************************************************
*: Calls     : PSPOSU.SPX , lfvUsr_ID() , lfvUsr_Pass()
*:*************************************************************
*: Passed Parameters  : llRem -> .T. for remove .F. for Edit.
*:*************************************************************
*: Returns            : .F. if access denied.
*:*************************************************************
*: Example   : =lfActPassW()
*:*************************************************************
FUNCTION lfActPassW
PARA llRem

SET CONFIRM OFF
*--Screen title message.
lcUsrActn = 'Edit access password to '+IIF(llRem,'remove.','modify.')
lcUser_ID = SPACE(10)
STORE SPACE(8) TO lcCurUsrPas,lcUsrPass 
lnUsrCnt = 1

PUSH KEY
ON KEY
llAlowAcss = .F.
DO (gcScrDir+gcWinAppl+"\PSPOSU.SPX")
POP KEY
SET CONFIRM ON
RETURN llAlowAcss

*:*************************************************************
*: Name      : lfvUsr_ID.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Validate user Id for Remove or Edit.
*:*************************************************************
FUNCTION lfvUsr_ID

*E301282,1 Start.
llHaveAscs= .F.
llUserFnd = SEEK(lcUser_ID,'SYUUSER')
IF llUserFnd AND SYUUSER.cUsr_Levl = 'O'
  lcPSUser = gcUser_ID
  gcUser_ID = lcUser_ID
  IF llRem
    llHaveAscs=gfUserPriv('PS','PSPOS','ALLOWREMOV')
  ELSE
    llHaveAscs=gfUserPriv('PS','PSPOS','ALLOWEDIT')
  ENDIF
  gcUser_ID = lcPSUser
ENDIF

IF llUserFnd AND (SYUUSER.cUsr_Levl = 'A' OR llHaveAscs)
*E301282,1 End.
  lcCurUsrPas = ALLTRIM(SYUUSER.cUsr_Pass)
  lnUsrCnt = 1
ELSE
  *--Invalid Administrator User Id.
  =gfModalGen('TRM42138B42000','DIALOG','Administrator User Id')
  IF lnUsrCnt = 3
    CLEAR READ
  ELSE
    lnUsrCnt = lnUsrCnt + 1
    _CUROBJ=OBJNUM(lcUser_ID)
    RETURN
  ENDIF
ENDIF

*:*************************************************************
*: Name      : lfvUsr_Pass.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Validate password for Remove or Edit.
*:*************************************************************
FUNCTION lfvUsr_Pass

IF (!EMPTY(lcCurUsrPas) AND SYS(2007,ALLTRIM(lcUsrPass))=lcCurUsrPas) OR ;
   ( EMPTY(lcCurUsrPas) AND ALLTRIM(lcUsrPass)=lcCurUsrPas )
  llAlowAcss = .T.
  CLEAR READ
ELSE
  llAlowAcss = .F.
  lcUsrPass  = SPACE(8)
  *--Invalid user password.
  =gfModalGen('TRM42138B42000','DIALOG','user password')
  IF lnUsrCnt = 3
    CLEAR READ
  ELSE
    lnUsrCnt = lnUsrCnt + 1
    _CUROBJ=OBJNUM(lcUsrPass)
    RETURN
  ENDIF
ENDIF


*:*************************************************************
*: Name      : lfvEdit.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Modify line information.
*:*************************************************************
*: Calls     : Screen PSPOSL.SPX
*:    Screen valid functions : lfvPrice()
*:                             lfvDiscnt()
*:                             lfvDscntP()
*:                             lfvTotQty()
*:                             lfvTax()
*:                             lfvTaxP()
*:                             lfvllTax()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvEdit()
*:*************************************************************
FUNCTION lfvEdit
PARAMETER lcDisplay

IF (lnSr=1 AND &lcPosTmp..TranType='1' AND &lcPosTmp..Cnt<>0) OR ;
   (lnSr=2 AND &lcPosTmp..TranType='0')
ELSE
  *-Only return line can be accepted to modify.
  =gfModalGen('TRM42133B42000','DIALOG',IIF(lnSr=1,'sales','return')+'|'+'modify')
  RETURN
ENDIF

*--Ckeck existance of multiple payments.
lnSvRec = RECNO()
SET FILTER TO TranType = '4'
GO TOP
llMltPExst = !EOF()

*C101660,1 WAB - change the filter to 4 and 'F' cause we add type 'F' FOr Freight 
*C101660,1 WAB - START
*SET FILTER TO TranType <> '4'
SET FILTER TO !(TranType $ '4FR')
*C101660,1 WAB - END

GOTO lnSvRec
*:B#604544,1 SSH Calculate the tax.
IF TYPE("lcDisplay") <> "C"
*:B#604544,1 SSH Calculate the tax.
  IF llMltPExst
    *--You cannot 'edit this line' since multiple payments was entered,
    *--Please remove the payments first.
    =gfModalGen('INM42135B42000','DIALOG','edit this line')
    RETURN
  ENDIF
  *E301282,1 Changed.
  *IF llUsersExt
  IF llUsersExt AND !llEdtAses
  *E301282,1 End.
    *--Check Access to Modify.
    IF !lfActPassW(.F.)
      _CUROBJ = OBJNUM(pbEdit)
      RETURN
    ENDIF
  ENDIF
  SCATTER MEMVAR
  llTax    = (m.Tax_Amt<>0)
*:B#604544,1 SSH Calculate the tax.
ENDIF
*:B#604544,1 SSH Calculate the tax.
*C101660,1 WAB - case ther are amount in tax field enabled let the field editable
*C101660,1 WAB - START
*lcObjMod = 'ENABLE'
lcObjMod = IIF(llTax ,IIF(lnSr=2 .AND. !EMPTY(lcInvNo),'DISABLE','ENABLE'),'DISABLE') 
*C102047,1 ABD disable discount filed if we have add. Discount. [Begin]
*lcDiscMod = IIF(lnSr=2 .AND. !EMPTY(lcInvNo),'DISABLE','ENABLE')
lcDiscModP = IIF(lnSr=2 .AND. !EMPTY(lcInvNo) .AND. m.Discount <> 0,'DISABLE','ENABLE')
lcDiscMod  = IIF(m.Discount <> 0 ,'DISABLE',lcDiscModP)
*C102047,1 ABD  [End]
*C101660,1 WAB - END

*:B#604544,1 SSH Calculate the tax.
IF TYPE("lcDisplay") <> "C"
*:B#604544,1 SSH Calculate the tax.
  PUSH KEY
  ON KEY
  ON KEY LABEL ESC CLEAR READ
  DO (gcScrDir+gcWinAppl+"\PSPOSL.SPX")
  POP KEY
*:B#604544,1 SSH Calculate the tax.
ENDIF
*:B#604544,1 SSH Calculate the tax.
*C101660,1 WAB - get the round of the amount
*C101660,1 WAB - START
lnTotal = lnTotal + m.nAmount - nAmount

*B606415,1 SSH Fix problems in Paid Amount
IF lnSr=2 .AND. !llExchange
  lnPaid = lnTotal
ENDIF
*B606415,1 SSH Fix problems in Paid Amount

*lnTotal = lnTotal + ROUND(m.nAmount,2) - nAmount
*C101660,1 WAB - END

*E301264,1 Start.
*lnPaid  = IIF(!llMltPay,lnTotal,lnPaid)
IF lnSr=1

  lnPaid  = IIF(!llMltPay,lnTotal,lnPaid)
  lnPaid  = IIF(lnPaid<0,0,lnPaid)

  *B803962,1 ABD Add new setup to ask the user to display 
  *B803962,1 ABD the amount tender or not. [Begin]
  IF llDsAmntNc
    lnTender = lnPaid
  ENDIF
  *B803962,1 ABD [End]
  
ENDIF
*E301264,1 End.


*B803962,1 ABD - Add New setup to return the sale and display the total amount
*B803962,1 ABD - In the Paid amount or set it as it work. [Begin]
IF lnSr=2 .AND. llALLRTAMT
*B604522,1 MAN This new setup 'The amount tender display' should not affect the paid amount
*  lnPaid = lnTotal
ENDIF
*B803962,1 ABD - [End]

*--Show Paid,Tendered and Calculate Change amounts.

=lfvPaid()

=RLOCK()
GATHER MEMVAR
UNLOCK
SHOW WINDOW (lcBrowTitl) REFRESH
*--Check Open uncmsess record.
=lfCrUnSess('O',.T.,.F.)

=lfRefresh()
RETURN


*:*************************************************************
*: Name      : lfvPrice
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Modify selling price. (m.Price)
*:*************************************************************
*: Called from Screen PSPOSL.SPX (Modify sale line)
*:*************************************************************
FUNCTION lfvPrice

*C102047,1 ABD add the add. discount to the orginal price. [Begin]
*m.Disc_Amt  = (m.Gros_Price - m.Price)
m.Disc_Amt  = (m.Gros_Price - m.Price) - m.Discount
*C102047,1 ABD [End]
IF m.Gros_Price<>0
  m.Disc_Pcnt = (m.Disc_Amt / m.Gros_Price)*100
ELSE
  m.Disc_Pcnt = 0
ENDIF
m.nAmount   = (m.Price * m.TotQty)
IF llTax
  m.Tax_Amt = ROUND((m.Tax_Rate/100) * m.nAmount,2)
  m.nAmount = m.nAmount + m.Tax_Amt
ENDIF
SHOW GETS WINDOW PSPOSL ONLY

*:*************************************************************
*: Name      : lfvDiscnt
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Modify Discount amount. (m.Disc_Amt)
*:*************************************************************
*: Called from Screen PSPOSL.SPX (Modify sale line)
*:*************************************************************
FUNCTION lfvDiscnt

m.Price     = (m.Gros_Price - m.Disc_Amt)
IF m.Gros_Price<>0
  m.Disc_Pcnt = (m.Disc_Amt / m.Gros_Price)*100
ELSE
  m.Disc_Pcnt = 0
ENDIF
m.nAmount   = (m.Price * m.TotQty)
IF llTax
  m.Tax_Amt = ROUND((m.Tax_Rate/100) * m.nAmount,2)
  m.nAmount = m.nAmount + m.Tax_Amt
ENDIF
SHOW GETS WINDOW PSPOSL ONLY

*:*************************************************************
*: Name      : lfvDscntP
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Modify Discount percentage. (m.Disc_Pcnt)
*:*************************************************************
*: Called from Screen PSPOSL.SPX (Modify sale line)
*:*************************************************************
FUNCTION lfvDscntP

m.Disc_Amt = (m.Disc_Pcnt/100) * m.Gros_Price
m.Price    = (m.Gros_Price - m.Disc_Amt)
m.nAmount  = (m.Price * m.TotQty)
IF llTax
  m.Tax_Amt = ROUND((m.Tax_Rate/100) * m.nAmount,2)
  m.nAmount = m.nAmount + m.Tax_Amt
ENDIF
SHOW GETS WINDOW PSPOSL ONLY

*:*************************************************************
*: Name      : lfvTotQty
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Modify quantity.  (m.TotQty)
*:*************************************************************
*: Called from Screen PSPOSL.SPX (Modify sale line)
*:*************************************************************
*
FUNCTION lfvTotQty

*B603687,1 ABD - If you return invoice & setup  allow return without invoice is false we will
*B603687,1 ABD - Approve to return quantity less than or equal invoice quantity. [Begin]
IF lnSr=2 .AND. !llAlowRet .AND. m.totqty <> lcOldValue
  lnAlias = SELECT()
  SELECT POSTRAN
  IF SEEK(&lcPosTmp..Account+lcInvNo,'POSTRAN')
    LOCATE REST FOR Tran + STYLE + STR(lineno,6) =lcInvNo + &lcPosTmp..Style+STR(&lcPosTmp..lineno,6)
    IF FOUND()
      IF ABS(m.totqty) > totqty
        *B603687,1 Message No.  : 4400
        *B603687,1 Message TEXT : Return Quantity is greater than invoice quantity. Can not modify.
        *B603687,1 Button  No.  : 00000.
        *B603687,1 Button  TEXT : OK
        =gfModalGen('QRM44000B00000','DIALOG','Return Quantity'+'|'+'invoice quantity'+'|'+'Can not modify.')
        m.totqty = lcOldValue
      ENDIF
    ENDIF
  ENDIF
  SELECT(lnAlias) 
ENDIF
*B603687,1 ABD- [End]
m.nAmount = (m.Price * m.TotQty)
IF llTax
  m.Tax_Amt = ROUND((m.Tax_Rate/100) * m.nAmount,2)
  m.nAmount = m.nAmount + m.Tax_Amt
ENDIF
SHOW GETS WINDOW PSPOSL ONLY

*:*************************************************************
*: Name      : lfvTax
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Modify Tax amount.  (m.Tax_Amt)
*:*************************************************************
*: Called from Screen PSPOSL.SPX (Modify sale line)
*:*************************************************************
FUNCTION lfvTax

m.Tax_Rate = (m.Tax_Amt / (m.Price * m.TotQty) )*100
m.nAmount  = (m.Price * m.TotQty) + m.Tax_Amt
SHOW GETS WINDOW PSPOSL ONLY

*:*************************************************************
*: Name      : lfvTaxP
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Modify Tax percentage.  (m.Tax_Rate)
*:*************************************************************
*: Called from Screen PSPOSL.SPX (Modify sale line)
*:*************************************************************
FUNCTION lfvTaxP
PARAMETER lcFrom
m.Tax_Amt = ROUND((m.Tax_Rate/100) * (m.Price * m.TotQty),2)
m.nAmount = (m.Price * m.TotQty) + m.Tax_Amt
IF TYPE("lcFrom") <> "C"
  SHOW GETS WINDOW PSPOSL ONLY
ENDIF

*:*************************************************************
*: Name      : lfvllTax
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Modify using taxs.
*:*************************************************************
*: Called from Screen PSPOSL.SPX (Modify sale line)
*:*************************************************************
FUNCTION lfvllTax
IF !llTax
 STORE 0 TO m.Tax_Amt,m.Tax_Rate
 m.nAmount = (m.Price * m.TotQty)
 SHOW GET m.nAmount
ENDIF
lcObjMod = IIF(llTax,'ENABLE','DISABLE') 
SHOW GET m.Tax_Amt   &lcObjMod
SHOW GET m.Tax_Rate &lcObjMod
RETURN








*:***********************************************************************
*:  P A Y M E N T S   P A R T   F U N C T I O N S.
*:***********************************************************************

*:*************************************************************
*: Name      : lfvPayTrm.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Validate Payment term.
*:*************************************************************
*: Calls     : lfvMltPay() if Multi payments selected.
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvPayTrm()
*:*************************************************************
FUNCTION lfvPayTrm

*--Multiple payments bar has fixed code 'MMMMMM' used in popup.
IF laPayTerm[lnPayTerm,2]='MMMMMM'
  lcRefer = ' '
  *E301467,1 (Start)
  lcRefer = IIF(llUseAtom,SPACE(60),SPACE(30))
  *E301467,1 (End).
  lnPaid  =  0
  
  SHOW GET pbPay,1  PROMPT PADL(ALLTRIM(laPayTerm[lnPayTerm,1]),18,' ') ENABLE
  SHOW GET lnPaid  DISABLE
  SHOW GET lcRefer DISABLE
  *B803962,1 ABD Add new setup to ask the user to display 
  *B803962,1 ABD the amount tender or not. [Begin]
  IF llDsAmntNc
      lnTender = 0
    SHOW GET lnTender
  ENDIF
  *B803962,1 ABD [End]
  
  *--Multiple payments valid function.
  =lfvMltPay()
ELSE
  *E301264,1 Start.
  IF lnSr=1
    lnPaid = lnTotal
    
    *B803962,1 ABD Add new setup to ask the user to display 
    *B803962,1 ABD the amount tender or not. [Begin]
    IF llDsAmntNc
      lnTender = lnPaid
    ENDIF
    *B803962,1 ABD [End]
    
  ENDIF
  *E301264,1 End.
  SHOW GET pbPay,1  PROMPT PADL(ALLTRIM(laPayTerm[lnPayTerm,1]),18,' ') DISABLE
  
  *B604180,1 ABD -Disable filed payment amount in case do not leave 
  *B604180,1 ABD -Opend amount setups. [Begin]
  *B606415,1 SSH Fix problems in Paid Amount
  *IF laSetups[17,2]
  IF (lnSr = 1 .AND. laSetups[17,2]) .OR. ( lnSr = 2 .AND. llALLRTAMT)
  *B606415,1 SSH Fix problems in Paid Amount
    SHOW GET lnPaid  DISABLE
  ELSE
    SHOW GET lnPaid  ENABLE
  ENDIF
  *B604180,1 ABD - [End]

  SHOW GET lcRefer ENABLE
  *B803962,1 ABD Add new setup to ask the user to display 
  *B803962,1 ABD the amount tender or not. [Begin]
  IF llDsAmntNc
    SHOW GET lnTender
  ENDIF
  *B803962,1 ABD [End]
  
ENDIF
*--Check Open uncmsess record.
=lfCrUnSess('O',.T.,.F.)
*E500241,1 SSH 03/05/1999 Add this part to calculate changed amount Incase
*E500241,1 SSH            of cash payment.

DIME laRelFld[1,2]
laRelFld = ''

*B604180,1 ABD - Change related filed llCash from 2 cases (Cash or not cash) to 
*B604180,1 ABD - be cash , not cash or Credit card that will give the 
*B604180,1 ABD - system to different Between them. [Begin]
*laRelFld[1,1] = 'LLCASH'
*laRelFld[1,2] = 'm.LLCASH'
*m.llCash = .F.
laRelFld[1,1] = 'CASH_NON'
laRelFld[1,2] = 'lcCash_Non'
STORE '' To lcCash_Non
*B604180,1 ABD - [End]


lcTranCode = laPayTerm[lnPayTerm,2]
lnOldChg = lnChg

*B604180,1 ABD - Change related filed llCash from 2 cases (Cash or not cash) to 
*B604180,1 ABD - be cash , not cash or Credit card that will give the 
*B604180,1 ABD - system to different Between them. [Begin]
*IF !llMltPay AND !(gfRltFld(lcTranCode , @laRelFld , 'CTERMCODE') AND m.llCash)
IF !llMltPay AND !(gfRltFld(lcTranCode , @laRelFld , 'CTERMCODE') .AND. ALLTRIM(lcCash_Non) = 'C')
  *B604180,1 ABD - [End]
  lnChg = lnTender
ENDIF
SHOW GET lnChg
lnChg = lnOldChg
lnPaid = IIF(lnTotal<0 AND lnSr=1,0,lnPaid)

*B604180,1 ABD -Disable filed payment amount in case do not leave 
*B604180,1 ABD -Opend amount setups. [Begin]
*B606415,1 SSH Fix problems in Paid Amount
*IF laSetups[17,2]
IF (lnSr = 1 .AND. laSetups[17,2]) .OR. (lnSr = 2 .AND. llALLRTAMT)
*B606415,1 SSH Fix problems in Paid Amount
  SHOW GET lnPaid  DISABLE
ELSE
  SHOW GET lnPaid
ENDIF
*B604180,1 ABD - [End]



*B803962,1 ABD Add new setup to ask the user to display 
*B803962,1 ABD the amount tender or not. [Begin]
IF llDsAmntNc
  lnTender = lnPaid
  SHOW GET lnTender
ENDIF
*B803962,1 ABD [End]

*E500241,1 SSH(END)
RETURN


*:*************************************************************
*: Name      : lfwPayTrm.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : When activate Payment term.
*:*************************************************************
*: Calls     : gfwCodePop() initialize payments array from Codes.
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfwPayTrm()
*:*************************************************************
FUNCTION lfwPayTrm

=gfwCodePop ( @laCodInfo, "CTERMCODE" ,"L" )
IF ASCAN( laPayTerm,'MMMMMM') = 0
  lnPaylen=ALEN(laPayTerm,1)+1
  DECLARE laPayTerm[lnPaylen,2]
  *--Code of Multiple payments is 'MMMMMM' used in this program only.
  laPayTerm[lnPaylen,1]=PADR('Multiple Payments',30)
  laPayTerm[lnPaylen,2]='MMMMMM'
  SHOW GET lnPayTerm
ENDIF
RETURN


*:*************************************************************
*: Name      : lfvPaid.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Valid Payment term amount or Tendered amount.
*:             Show paid , Tendered and Change amounts.
*:*************************************************************
*: Passed Parameters  : ' ' for no validation
*:                      'P' for validate Paid amount
*:                      'T' for validate Tendered amount
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvPaid('')
*:*************************************************************
FUNCTION lfvPaid
PARA lcChkPaid

IF !EMPTY(lcChkPaid)
  llNotVald = .F.
  IF lnTotal >= 0
    IF (lcChkPaid='P' AND lnPaid < 0) OR (lcChkPaid='T' AND lnTender < 0)
      lcMsgDsp  = IIF(lcChkPaid='T','Tendered',lcPyStatus)
      lcPdCkStr = 'negative'
      llNotVald = .T.
    ENDIF
    IF lcChkPaid='P' AND lnPaid > lnTotal
      lcMsgDsp  = lcPyStatus
      lcPdCkStr = 'more than '+ALLTRIM(STR(lnTotal))
      llNotVald = .T.
    ENDIF
  ELSE
    IF (lcChkPaid='P' AND lnPaid > 0) OR (lcChkPaid='T' AND lnTender > 0)
      lcMsgDsp  = IIF(lcChkPaid='T','Tendered',lcPyStatus)
      lcPdCkStr = 'positive'
      llNotVald = .T.
    ENDIF
    IF lcChkPaid='P' AND lnPaid < lnTotal
      lcMsgDsp  = lcPyStatus
      lcPdCkStr = 'less than '+ALLTRIM(STR(lnTotal))
      llNotVald = .T.
    ENDIF
  ENDIF

  *--Not valid paid amount.
  IF llNotVald
    =gfModalGen('TRM42134B42000','DIALOG',lcMsgDsp+'|'+lcPdCkStr)
    IF lcChkPaid='P'
      lnPaid = lcOldValue
      _CUROBJ = OBJNUM(lnPaid)
    ELSE
      lnTender = lcOldValue
      _CUROBJ = OBJNUM(lnTender)
    ENDIF
    RETURN
  ENDIF
ENDIF  

lcPyStatus = IIF(lnPaid >= 0,'Payment','Refund')
lnChg = IIF(lnTender<>0,(lnTender - lnPaid),0)
SHOW GET lnTender

*B604180,1 ABD -Disable filed payment amount in case do not leave 
*B604180,1 ABD -Opend amount setups. [Begin]
*B606415,1 SSH Fix problems in Paid Amount
*IF laSetups[17,2]
IF (lnSr = 1 .AND. laSetups[17,2]) .OR. ( lnSr = 2 .AND. llALLRTAMT)
*B606415,1 SSH Fix problems in Paid Amount
  SHOW GET lnPaid  DISABLE
ELSE
  SHOW GET lnPaid
ENDIF
*B604180,1 ABD - [End]

*B801994,1 SSH 02/27/1999 Add the following part to print change amount
*B801994,1 SSH            only for cash in case of multipayment.
lnOldChg = lnChg
IF llMltPay
  lnChg = lnTender-lnRelpay
ELSE
  DIME laRelFld[1,2]
  laRelFld = ''
  
  *B604180,1 ABD - Change related filed llCash from 2 cases (Cash or not cash) to 
  *B604180,1 ABD - be cash , not cash or Credit card that will give the 
  *B604180,1 ABD - system to different Between them. [Begin]
  *laRelFld[1,1] = 'LLCASH'
  *laRelFld[1,2] = 'm.LLCASH'
  *m.llCash = .F.
  laRelFld[1,1] = 'CASH_NON'
  laRelFld[1,2] = 'lcCash_Non'
  STORE '' To lcCash_Non
  *B604180,1 ABD - [End]

  lcTranCode = laPayTerm[lnPayTerm,2]

  *B604180,1 ABD - Change related filed llCash from 2 cases (Cash or not cash) to 
  *B604180,1 ABD - be cash , not cash or Credit card that will give the 
  *B604180,1 ABD - system to different Between them. [Begin]
  *lnChg = IIF(gfRltFld(lcTranCode , @laRelFld , 'CTERMCODE') AND m.llCash,lnChg,lnTender)
  lnChg = IIF(gfRltFld(lcTranCode , @laRelFld , 'CTERMCODE') .AND. ALLTRIM(lcCash_Non) = 'C',lnChg,lnTender)
  *B604180,1 ABD - [End]

ENDIF
SHOW GET lnChg
*lnChg = lnOldChg
*B801994,1 SSH(END)

*B124698,1 NNA 10/28/2004 (Begin) Enable the Exchange Check box in case of lnpaid<=0 that because
*B124698,1 NNA            in return lnPaid Be less than Zero and this case customer can return and Exchange
*IF lnPaid = 0 AND lnSr = 2 AND !EOF(lcPosTmp)
IF lnPaid <= 0 AND lnSr = 2 AND !EOF(lcPosTmp)
*B124698,1 NNA (End)

  SHOW GET llExchange ENABLE
ELSE
  llExchange = IIF(llExchange AND lnSr = 1,.T.,.F.)
  SHOW GET llExchange DISABLE
ENDIF
*--Check Open uncmsess record.
=lfCrUnSess('O',.T.,.F.)

=lfRefresh('PSPOSR2')
RETURN


*:*************************************************************
*: Name      : lfvMltPay.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Edit multiple payments.
*:*************************************************************
*: Calls     : lfvPaid(),lfActBrow(),lfRefresh()
*:             PSPOSP.SPX  Multiple payments screen.
*:             lpTab2()
*:             lpBkTab2()
*:             lfPayLines()
*:             lfwPyBrow()
*:             lfvNewPay()
*:             lfvMPatTrm()
*:             lfvMPatRef()
*:             lfvMPatAmt()
*:             lfvRemPay()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns   : File was updated with the entered payments
*:             in 'lcPosTmp' file with TranType = '4'.
*:*************************************************************
*: Example   : =lfvMltPay()
*:*************************************************************
FUNCTION lfvMltPay

lcBrowPyLn = 'Payments'
SELECT (lcPosTmp)
lnSavRec=RECNO()

*--Display only Payment records.
SET FILTER TO TranType = '4'
GO TOP
*--Remove button status.
lcRemStat= IIF(!EOF(),'ENABLE','DISABLE')
lnMRecNo =  1
  
*--Create a new payment term temp array without multiple payment bar.
*--Will used in multiple payment screen.
lnPyTmLen = ALEN(laPayTerm,1)
DECLARE laTPayTerm[lnPyTmLen,2]
= ACOPY(laPayTerm,laTPayTerm,1)
DECLARE laTPayTerm[lnPyTmLen-1,2]
lnTPayTerm = 0

PUSH KEY
ON KEY
*--Trapping for Multiple payment screen.
ON KEY LABEL ESC llDumy=.T.
ON KEY LABEL TAB     DO lpTab2
ON KEY LABEL BACKTAB DO lpBkTab2

*--Call multiple payments screen.
DO (gcScrDir+gcWinAppl+"\PSPOSP.SPX")

POP KEY
*--Calculate the total payments amount.
SELECT (lcPosTmp)
DELETE ALL FOR TranType = '4' AND nAmount = 0
GO TOP
IF !EOF()
  *B801994,1 SSH 02/27/1999 Comment the sum command and calculate the cash 
  *B801994,1 SSH            amount into variable (lnRelpay). 
  *B801994,1 SSH            To do this we call the global function
  *B801994,1 SSH            (gfRltFld) to return the related logical field 
  *B801994,1 SSH            wich indicate if cash or not.
  *SUM REST FOR TranType = '4' nAmount TO lnPaid
  DIME laRelFld[1,2]
  laRelFld = ''

  *B604180,1 ABD - Change related filed llCash from 2 cases (Cash or not cash) to 
  *B604180,1 ABD - be cash , not cash or Credit card that will give the 
  *B604180,1 ABD - system to different Between them. [Begin]
  *laRelFld[1,1] = 'LLCASH'
  *laRelFld[1,2] = 'm.LLCASH'
  *m.llCash = .F.
  *SUM FOR TranType = '4' nAmount,IIF(gfRltFld(TranCode , @laRelFld , 'CTERMCODE') AND m.llCash,nAmount,0) TO lnPaid,lnRelpay  
  laRelFld[1,1] = 'CASH_NON'
  laRelFld[1,2] = 'lcCash_Non'
  STORE '' To lcCash_Non
  SUM FOR TranType = '4' nAmount,IIF(gfRltFld(TranCode , @laRelFld , 'CTERMCODE') .AND.ALLTRIM(lcCash_Non) = 'C',nAmount,0) TO lnPaid,lnRelpay
  *B604180,1 ABD - [End]

  *B801994,1 SSH(END)
  llMltPay = .T.
  SHOW GET lnPayTerm DISABLE
  *C102047,1 ABD show get the add discount filed disable. [Begin]
  SHOW GET lnAddDisc DISABLE
  *C102047,1 ABD [End]
ELSE
  lnPaid = 0

  *B803962,1 ABD Add new setup to ask the user to display 
  *B803962,1 ABD the amount tender or not. [Begin]
  IF llDsAmntNc
    lnTender = 0
  ENDIF
  *B803962,1 ABD [End]
  
  llMltPay = .F.
  SHOW GET lnPayTerm ENABLE
ENDIF

*--Show Paid,Tendered and Calculate Change amounts.

=lfvPaid()

*--Set back to not display the current payments.

*C101660,1 WAB - change the filter to 4 and 'F' cause we add type 'F' FOr Freight 
*C101660,1 WAB - START
*SET FILTER TO TranType <> '4'
SET FILTER TO !(TranType $ '4FR')
*C101660,1 WAB - END

GO TOP
=lfActBrow()
IF !EOF()
  GOTO lnSavRec
  SHOW WINDOW (lcBrowTitl) REFRESH
  =lfRefresh()
ENDIF
*--Check Open uncmsess record.
=lfCrUnSess('O',.T.,.F.)

RETURN


*!*************************************************************
*! Name      : lpTab2
*: Developer : Timour A. K.
*: Date      : 02/02/98
*! Purpose   : Tab key trapping for Payments Screen.
*!*************************************************************
PROCEDURE lptab2

IF WONTOP() = lcBrowPyLn
  ACTIVATE WINDOW PSPOSP2
  _CUROBJ=OBJNUM(lnTPayTerm)
ELSE
  IF _CUROBJ = OBJNUM(pbClose) AND !EOF()
    ACTIVATE WINDOW (lcBrowPyLn)
  ELSE
    _CUROBJ = _CUROBJ + 1
  ENDIF
ENDIF  
RETURN

*!*************************************************************
*! Name      : lpBkTab2
*: Developer : Timour A. K.
*: Date      : 02/02/98
*! Purpose   : Shift Tab key trapping for Payments Screen.
*!*************************************************************
PROCEDURE lpBkTab2

IF WONTOP() = lcBrowPyLn
  ACTIVATE WINDOW PSPOSP2
  _CUROBJ = OBJNUM(pbClose)
ELSE
  IF _CUROBJ = OBJNUM(lnTPayTerm) AND !EOF()
    ACTIVATE WINDOW (lcBrowPyLn)
  ELSE
     IF EOF()
       IF _CUROBJ = OBJNUM(pbClose)
         _CUROBJ = OBJNUM(pbAdd)
       ELSE
         _CUROBJ = OBJNUM(pbClose)
       ENDIF
     ELSE
      _CUROBJ = _CUROBJ - 1
     ENDIF
  ENDIF
ENDIF
RETURN

*:*************************************************************
*: Name      : lfPayLines.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Multiple payments browse.
*:*************************************************************
*: Calls     : lfwPyBrow()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfPayLines()
*:*************************************************************
FUNCTION lfPayLines

lnMRecNo = RECNO()
lcPyBrFlds = "lcSrMark=IIF(lnMRecNo=RECNO(),'>>','  '):2:H='  ':W=.F.,"+;
             "Desc1 :R :H='Payment Term' :27,"+;
             "Reference :R :38,nAmount :H='Payment Amount'"
BROWSE FIELDS &lcPyBrFlds;
       WINDOW  PSPOSP1   ;
       WHEN lfwPyBrow()  ;
       IN WINDOW PSPOSP;
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOMENU;
       NOWAIT;
       NOEDIT;
       SAVE;
       TITLE lcBrowPyLn
RETURN

*:*************************************************************
*: Name      : lfwPyBrow.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : When function for multiple payments browse.
*:*************************************************************
*: Calls     : ..........
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfwPyBrow()
*:*************************************************************
FUNCTION lfwPyBrow

lnMRecNo  = RECNO()
lnTPayTerm= (ASCAN(laTPayTerm,TranCode)/2)
lcTRefr   = Reference
lnTAmount = nAmount
lcObjMode = IIF(!EOF(),'ENABLE','DISABLE')
SHOW GET lnTPayTerm &lcObjMode
SHOW GET lcTRefr    &lcObjMode
SHOW GET lnTAmount  &lcObjMode
SHOW WINDOW (lcBrowPyLn) REFRESH
RETURN

*:*************************************************************
*: Name      : lfvNewPay.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Valid new payment.
*:*************************************************************
*: Calls     : lfwPyBrow()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvNewPay()
*:*************************************************************
FUNCTION lfvNewPay

lnTPayTerm = 1
SUM nAmount TO lnToPay
lnToPay = lnTotal - lnToPay
APPEND BLANK
=RLOCK()
REPLACE TranType WITH '4',;
        TranCode WITH laPayTerm[1,2],;
        TranDate WITH ldPostDate,; 
        Account  WITH lcCustomer,;
        StName   WITH lcName,;
        Phone1   WITH lcPhone,;
        Desc1    WITH laTPayTerm[1,1],;
        nAmount  WITH lnToPay,;
        dPostDate WITH ldPostDate
    *-- Call global function to add audit fields info.
    =gfAdd_Info(lcPosTmp)

UNLOCK
_CUROBJ = OBJNUM(lnTPayTerm)
SHOW GET pbRem ENABLE
=lfwPyBrow()
RETURN

*:*************************************************************
*: Name      : lfvMPatTrm.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Valid payment and update file.
*:*************************************************************
*: Calls     : lfwPyBrow()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvMPatTrm()
*:*************************************************************
FUNCTION lfvMPatTrm

=RLOCK()
REPLACE TranCode WITH laTPayTerm[lnTPayTerm,2],;
        Desc1    WITH laTPayTerm[lnTPayTerm,1]
UNLOCK
*E301467,1 Start.
*_CUROBJ = OBJNUM(lcTRefr)
_CUROBJ = OBJNUM(lnTAmount)
*E301467,1 End.
=lfwPyBrow()
RETURN
        
*:*************************************************************
*: Name      : lfvMPatRef.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Valid payment reference and update file.
*:*************************************************************
*: Calls     : lfwPyBrow()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvMPatRef()
*:*************************************************************
FUNCTION lfvMPatRef

=RLOCK()
REPLACE Reference WITH lcTRefr
*E301467,1 (Start)
IF llUseAtom
  *--For non cash payment.
  DIME laRelFld[1,2]
  laRelFld = ''

  *B604180,1 ABD - Change related filed llCash from 2 cases (Cash or not cash) to 
  *B604180,1 ABD - be cash , not cash or Credit card that will give the 
  *B604180,1 ABD - system to different Between them. [Begin]
  *laRelFld[1,1] = 'LLCASH'
  *laRelFld[1,2] = 'm.LLCASH'
  *m.llCash = .F.
  laRelFld[1,1] = 'CASH_NON'
  laRelFld[1,2] = 'lcCash_Non'
  STORE '' To lcCash_Non 
  *B604180,1 ABD - [End]
  
  =gfRltFld(TranCode , @laRelFld , 'CTERMCODE')

  *B604180,1 ABD - Change related filed llCash from 2 cases (Cash or not cash) to 
  *B604180,1 ABD - be cash , not cash or Credit card that will give the 
  *B604180,1 ABD - system to different Between them. [Begin]
  *IF ! m.llCash AND EMPTY(lcTRefr)
  IF ALLTRIM(lcCash_Non) = 'D' .AND. EMPTY(lcTRefr)  
  *B604180,1 ABD - [End]

    =gfModalGen('INM52018B42000','DIALOG')
    _CUROBJ = OBJNUM(lcTRefr)
  ENDIF
  REPLACE cCrdCard WITH lcTRefr
ENDIF
*E301467,1 (End).
UNLOCK
=lfwPyBrow()
RETURN

*:*************************************************************
*: Name      : lfvMPatAmt.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Valid payment amount and update file.
*:*************************************************************
*: Calls     : lfwPyBrow()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvMPatAmt()
*:*************************************************************
FUNCTION lfvMPatAmt

lnSaveRc = RECNO()
SUM nAmount TO lnRestToPy FOR RECNO()<>lnSaveRc
lnRestToPy = lnTotal - lnRestToPy
GOTO lnSaveRc

llNotVald = .F.
IF lnRestToPy >= 0
  IF lnTAmount < 0
    lcPdCkStr = 'negative'
    llNotVald = .T.
  ENDIF
  IF lnTAmount > lnRestToPy
    lcPdCkStr = 'more than '+ALLTRIM(STR(lnRestToPy))
    llNotVald = .T.
  ENDIF
ELSE
  IF lnTAmount > 0
    lcPdCkStr = 'positive'
    llNotVald = .T.
  ENDIF
  IF lnTAmount < lnRestToPy
    lcPdCkStr = 'less than '+ALLTRIM(STR(lnRestToPy))
    llNotVald = .T.
  ENDIF
ENDIF

*--Not valid paid amount.
IF llNotVald
  =gfModalGen('TRM42134B42000','DIALOG',lcPyStatus+'|'+lcPdCkStr)
  lnTAmount = lcOldValue
  _CUROBJ = OBJNUM(lnTAmount)
  RETURN
ENDIF

=RLOCK()
REPLACE nAmount WITH lnTAmount
UNLOCK
=lfwPyBrow()
RETURN


*:*************************************************************
*: Name      : lfvRemPay.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Valid remove payment.
*:*************************************************************
*: Calls     : lfwPyBrow()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvRemPay()
*:*************************************************************
FUNCTION lfvRemPay

DELETE
GO TOP
IF EOF()
  SHOW GET pbAdd      ENABLE
  SHOW GET pbRem      DISABLE
  SHOW GET lnTPayTerm DISABLE
ENDIF
=lfwPyBrow()  
RETURN


*:***********************************************************************
*:  C U S T O M E R   R E T U R N   I N V O I C E  P A R T   F U N C T I O N S.
*:***********************************************************************
*:*************************************************************
*: Name      : lfvInvNo.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Validate Returned Invoice number and select lines.
*:*************************************************************
FUNCTION lfvInvNo

*B604253,1 ABD - Variable hold the order of POSTRAN . [Begin]
PRIVATE lcOldOrder
lcOldOrder= ''
*B604253,1 ABD - [End]

lnAlias = SELECT()
*--If empty or does not changed return from fuction.
IF EMPTY(lcInvNo) OR lcInvNo=lcOldValue
  RETURN
ENDIF

*B604253,4 ABD - we select POSTRAN file and go top in it 
*B604253,4 ABD - & set order to Postrna order [Begin]
SELECT POSTRAN
lcOldOrder = ORDER()
SET ORDER TO POSTRNA
*B604253,4 ABD - [End]
SELECT INVHDR
SET ORDER TO TAG Invhdra
*--Main file.
*--Check existance of transaction in credit file.
*B604253,4 ABD - Do case to Void the invoices that made from AR OR So. [Begin]
*IF !SEEK(lcCustomer+lcInvNo,'INVHDR')
DO CASE
  CASE !SEEK(lcCustomer+lcInvNo,'INVHDR') .AND. !SEEK(lcCustomer+lcInvNo,'POSTRAN')
    *B604253,4 ABD - [End]
    SELECT INVHDR
    *B604253,1 ABD - Fix bug that the program allow the user to select invoice that 
    *B604253,1 ABD - Create from SO Or AR. [Begin]
    *IF SEEK(lcCustomer)
    *B604253,4 ABD - Seek only in invhdr file that the invoice not found in POstran File. [Begin]
    *IF SEEK(lcCustomer) .AND. SEEK(lcCustomer+Invhdr.Invoice,'POSTRAN')
    IF SEEK(lcCustomer)
      *B604253,4 ABD - [End]
      *B604253,1 ABD - [End]
      PRIVATE lcBrFields
      lcbrfields = "Invoice :H='Invoice # ' ,Account :H='Customer',TotalChg :H='Total Invoice Amount'"
      DIMENSION latemp[1]
      latemp = ''

      *B604253,1 ABD - Fix bug that the program allow the user to select invoice that 
      *B604253,1 ABD - Create from SO Or AR . [Begain]
      *=ariabrow([lcCustomer],'Invoices', gnbrfsrow1, gnbrfscol1,;
	  	  	gnbrfsrow2, gnbrfscol2, '','','INVOICE','laTemp')
      *B604253,4 ABD - Set the postran to order Before first seek . [Begin]
      *SELECT POSTRAN
      *lcOldOrder = ORDER()
      *SET ORDER TO POSTRNA
      *GOTO TOP
      *B604253,4 ABD - [End]
      SELECT INVHDR
      = ariabrow([lcCustomer FOR SEEK(lcCustomer+Invhdr.Invoice,'POSTRAN') ],'Invoices', gnbrfsrow1, gnbrfscol1,;
	    		gnbrfsrow2, gnbrfscol2, '','','INVOICE','laTemp')
      *B604253,4 ABD - 	Return to old order out of seek. [Begin]
      *SET ORDER TO &lcOldOrder IN POSTRAN
      *B604253,1 ABD - [End]
      lcInvNo = latemp[1]
    ELSE
      lcInvNo = ' '
      *--This customer has no invoices.  
      =gfModalGen('INM52002B42000','DIALOG','invoice')
      SELECT INVHDR
      SET ORDER TO TAG Invhdr
      SELECT (lnAlias)
      lcInvNo = SPACE(6)
      _CUROBJ = OBJNUM(lcInvNo)
      RETURN
    ENDIF
  *B604253,4 ABD - Close the end if statment & give the user message in case
  *B604253,4 ABD - this invoice didn't made from the PS module. [Begin]
  *ENDIF
  CASE SEEK(lcCustomer+lcInvNo,'INVHDR') .AND. !SEEK(lcCustomer+Invhdr.Invoice,'POSTRAN')
    *-- Message Text   : You can not return this invoice because either it was not
    *-- Message Text   : Created through the Point of Sale screen or it contains a 
    *-- Message Text   : Total zero amount.
    *-- Message Number : 52021.
    *-- Button Message : Ok  
    *-- Button Number  : 00000
    =gfModalGen('INM52021B00000','DIALOG')
    lcInvNo = SPACE(6)
    _CUROBJ = OBJNUM(lcInvNo)
    RETURN
ENDCASE
*-- Return to Old Order. 
SET ORDER TO &lcOldOrder IN POSTRAN
*B604253,4 ABD - [End]


SELECT INVHDR
SET ORDER TO TAG Invhdr
*--Invoice Checking.
SELECT (lnAlias)
IF EMPTY(lcInvNo) OR !SEEK(lcInvNo,'INVLINE')
  *--This xxx number does not exist.
  =gfModalGen('INM42129B42000','DIALOG','invoice'))
  lcInvNo = SPACE(6)
  _CUROBJ = OBJNUM(lcInvNo)
  RETURN
ENDIF

lnInvAmt = IIF(SEEK(lcInvNo,'INVHDR'),INVHDR.TotalChg,0)

*C127532,1 EIH 07/20/2005 Can't return invoice not at default warehouse [Begin]. 
IF INVHDR.Cwarecode <> lcStoreCode
  *--This invoice is not on default warecode
  =gfModalGen('INM00000B00000','','','',"Warehouse of invoice must be "+lcStoreCode )
  lcInvNo = SPACE(6)
  _CUROBJ = OBJNUM(lcInvNo)
  RETURN
ENDIF
*C127532,1 EIH 07/20/2005[End]. 

IF INVHDR.Account <> lcCustomer
  *--This invoice is not sold to this customer.
  =gfModalGen('INM42136B42000','DIALOG')
  lcInvNo = SPACE(6)
  _CUROBJ = OBJNUM(lcInvNo)
  RETURN
ENDIF

*Check that this invoice already return or no.
llAllInvRt = .T.
llIvLinRet = .F.

SELECT POSTRAN
SET ORDER TO TAG Postrln

IF SEEK(lcInvNo)
  *--The invoice returnd, need to check if it all or partialy.
  SELECT INVLINE
  SEEK lcInvNo
  SCAN WHILE Invoice = lcInvNo
    IF SEEK(Invoice+Style+STR(LineNo,6),'POSTRAN')
      *--This line already returnd.
      llIvLinRet = .T.
    ELSE
      *--This lineno is not returnd.
      llAllInvRt = .F.
      EXIT
    ENDIF
  ENDSCAN
ELSE
  *--The all invoice not returnd.
  llAllInvRt = .F.
ENDIF
SELECT (lnAlias)

IF llAllInvRt
  *--This invoice completly returned.
  =gfModalGen('TRM52015B42000','DIALOG')
  lcInvNo = SPACE(6)
  _CUROBJ = OBJNUM(lcInvNo)
  RETURN
ENDIF

llOpenBal = .F.
*--Check the open balance on the invoice.
*:B#604352,1 SSH Set order to debit.
SET ORDER TO Debit IN DEBIT
*:B#604352,1 SSH [END]
IF SEEK(lcCustomer+lcInvNo,'DEBIT')
  lnOpen    = DEBIT.Amount
  lcBaseSml = SET('CURRENCY',1)
  llOpenBal = .T.
ENDIF

*E301264,1 Ask for return invoice line all or no.

*B603687,1 ABD Message splling error in message no 52007 fix in syddlobj. [Begin]
*--Do you wish to return the all invoice? Y/N
*--Do you want to return the all invoice? Y/N
*B603687,1 ABD  [End]

IF gfModalGen('QRM52007B42002') = 2
  IF llOpenBal
    *--This invoice still has XXX as an open amount,
    *--Make sure that you deduct this amount from the return.
    =gfModalGen('INM52009B42000','DIALOG',ALLTRIM(STR(lnOpen,10,2))+' '+lcBaseSml )
  ENDIF
  SHOW GET lcInvNo
  IF llStyBrow
    _CUROBJ=OBJNUM(lcStyle)
  ELSE
    _CUROBJ=OBJNUM(lcUpcPop)
  ENDIF
  lcSRep = INVHDR.REP1
  IF !EMPTY(lcSRep)
    SHOW GET lcSRep    DISABLE
    SHOW GET ibRepBrow DISABLE
  ENDIF
  SELECT POSTRAN
  lcOldTag = TAG()
  SET ORDER TO TAG POSTRNA
  IF SEEK(INVHDR.Account+INVHDR.invoice) AND !EMPTY(CaddRetDis)
    =gfwCodePop(@laCodInfo,'CADDRETDIS','L')
    lnAddDisc = (ASCAN(laAddDisc,PADR(CaddRetDis,6))/2)
    IF lnAddDisc<>0
      SHOW GET lnAddDisc DISABLE
  
      *B604248,1 ABD - Remak the next variable that we didn't need it now. [Begin]
      *llFirstTme = .F.
      *B604248,1 ABD - [End]
      
    ENDIF
  ENDIF
  SET ORDER TO TAG (lcOldTag)
  SELECT(lnAlias)
  RETURN
ELSE
  IF llIvLinRet
    *--Some lines on this invoice already returned it would be excluded.
    =gfModalGen('INM52014B42000','DIALOG')
  ENDIF

  IF llOpenBal
    *--This invoice still has XXX as an open amount,
    *--It will be reduced from the return amount.
    =gfModalGen('INM42137B42000','DIALOG',ALLTRIM(STR(lnOpen,10,2))+' '+lcBaseSml )
  ENDIF
ENDIF
*E301264,1 End.


SELECT INVLINE
SEEK lcInvNo
SCAN WHILE Invoice = lcInvNo
  *C101660,1 WAB - get the tax_rate from the postran table
  *C101660,1 WAB - START
  SELECT POSTRAN
  lcOldTag = TAG()
  SET ORDER TO TAG POSTRNA
  lnTAxRate = 0
  lnTAxAmt  = 0
  *C102047,1 ABD Add New Paramt. to the add line function [Begin]
  lnInvDsc = INVHDR.DISCPCNT
  lcInvDCd = ""
  lcInvRep = ""
  IF SEEK(INVLINE.Account+INVLINE.invoice)
    LOCATE REST FOR Style = INVLINE.Style .AND. LineNo = INVLINE.LineNo
    IF FOUND()
      lcInvDCd = caddretdis
      lcInvRep = Rep1 
      lnTAxRate = Tax_Rate
      lnTAxAmt  = Tax_Amt
      lnInvRpCm = Comm1      
    ENDIF
  ENDIF
  SET ORDER TO TAG (lcOldTag)
  SELECT INVLINE
  *C102047,1 ABD [End]  
  *C101660,1 WAB - END
  
  IF !SEEK(Invoice+Style+STR(LineNo,6),'POSTRAN')
    lnInvCnt = IIF(Qty1#0,1,IIF(Qty2#0,2,IIF(Qty3#0,3,IIF(Qty4#0,4,IIF(Qty5#0,5,IIF(Qty6#0,6,IIF(Qty7#0,7,8)))))))
    *--Append new Sales or Return Style record.
    *C101660,1 WAB - add tax_rate in parameter sending to the lfappndln
    *C101660,1 WAB - START
    *= lfAppndLn('0',Invoice,ldPostDate,'',Desc1,Style,Gros_Price,Disc_pcnt,0,-TotQty,0,lnInvCnt,.T.,Lineno)
    *C102047,1 ABD Add new Paramt. to the add lines function . [Begin]
    *=lfAppndLn('0',Invoice,ldPostDate,'',Desc1,Style,Gros_Price,Disc_pcnt,lnTAxRate,-TotQty,-0,lnInvCnt,.T.,Lineno,lnTAxAmt)
    =lfAppndLn('0',Invoice,ldPostDate,'',Desc1,Style,Gros_Price,Disc_pcnt,lnTAxRate,-TotQty,-0,lnInvCnt,.T.,Lineno,lnTAxAmt,lcInvRep+"*"+STR(lnInvRpCm,6,2)+"*"+PADR(lcInvDCd,6)+"*"+STR(lnInvDsc,6,2))
    *C102047,1 ABD [End]
    *C101660,1 WAB - END
  ENDIF
ENDSCAN



*B803962,1 ABD- Return the payment records. [Begin]
IF lnSR = 2
  *-- Call function to add records in the temp file with the multi payment.
 = lfvRetMPay ()
ENDIF
*B803962,1 ABD- [End]

*C101660,1 WAB - case there are freight add record with type 'F' in return 
*C101660,1 WAB - START
IF llDispFrght
  =SEEK(lcInvNo,'INVHDR')
  IF !EMPTY(INVHDR.Freight)
    llNoFreight = .T.
    SELECT (lcPosTmp)
    SET FILTER TO TranType <> '4'
    APPEND BLANK
    REPLACE TranType  WITH 'R',;
            TranDate  WITH  ldPostDate,;
            Desc1     WITH 'Freight' ,;
            nAmount   WITH (INVHDR.Freight*-1)
    LNFREIGHT = INVHDR.Freight*-1
    lcCntFrght = 'ENABLE'
  ELSE
    llNoFreight = .F.
    lcCntFrght = 'DISABLE'
  ENDIF
  SHOW GET pbFreight &lcCntFrght
ENDIF
*C101660,1 WAB - END

IF llOpenBal
  *C101660,1 WAB - add paramter to lfappndln()
  *C101660,1 WAB - START
  *= lfAppndLn('1',DEBIT.Tran,DEBIT.TranDate,'','Invoice Open Balance','',0,0,0,0,lnOpen,0,.F.,0)
  =lfAppndLn('1',DEBIT.Tran,DEBIT.TranDate,'','Invoice Open Balance','',0,0,0,0,lnOpen,0,.F.,0,0)
  *C101660,1 WAB - END
ENDIF

SELECT POSTRAN
SET ORDER TO TAG Postrna
*--Return with selected styles.
SELECT (lcPosTmp)
SUM ALL nAmount TO lnTotal FOR TranType <> '4'


lnTotal= MIN(lnTotal,0)

*B803962,1 ABD - Add New setup to return the sale and display the total amount
*B803962,1 ABD - In the Paid amount or set it as it work. [Begin]
IF lnSr=2 .AND. llALLRTAMT
*B604522,1 MAN This new setup 'The amount tender display' should not affect the paid amount
  *B606415,1 SSH Fix problems in Paid Amount
  IF !llExchange
    *lnPaid = lnTotal
    lnPaid = lnTotal
  ENDIF
  *B606415,1 SSH Fix problems in Paid Amount
ENDIF
*B803962,1 ABD - [End]

*E301264,1 Start.
*lnPaid = IIF(!llMltPay,lnTotal,lnPaid)
*E301264,1 End.

*--Show Paid,Tendered and Calculate Change amounts.

=lfvPaid()

GO TOP
IF !EOF()
  *--Since records was added allow remove and modify.
  SHOW GET lcInvNo   DISABLE
  SHOW GET lnInvAmt  DISABLE
  SHOW GET pbRemove  ENABLE
  SHOW GET pbEdit    ENABLE
  *E301264,1 Start.
  IF llAlwRfnd OR (lnSr=1 AND (!llExchange OR lnPaid > 0))
    *B803962,1 ABD Show get enable in case not multi paument. [Begin]
    IF !llMltPay
      *B803962,1 ABD [End]

      *B604180,1 ABD -Disable filed payment amount in case do not leave 
      *B604180,1 ABD -Opend amount setups. [Begin]
      *B606415,1 SSH Fix problems in Paid Amount
      *IF laSetups[17,2]
      IF (lnSr = 1 .AND. laSetups[17,2]) .OR. (lnSr = 2 .AND. llALLRTAMT)
      *B606415,1 SSH Fix problems in Paid Amount
        SHOW GET lnPaid  DISABLE
      ELSE
        SHOW GET lnPaid    ENABLE
      ENDIF
      *B604180,1 ABD - [End]

      SHOW GET lnPayTerm ENABLE
      *B803962,1 ABD End For if statment. [Begin]
     ENDIF 
  *B803962,1 ABD [End]
  ENDIF
  *E301264,1 End.
  SHOW GET lcRefer   ENABLE
  SHOW GET lnTender  ENABLE
  IF lnPaid = 0
    SHOW GET llExchange ENABLE
  ENDIF
  SHOW GET pbProceed,1 PROMPT lcProc  
ELSE
  SHOW GET lcInvNo   ENABLE
  SHOW GET lnInvAmt  ENABLE
  SHOW GET pbRemove  DISABLE
  SHOW GET pbEdit    DISABLE
  SHOW GET lnPaid    DISABLE
  SHOW GET lnPayTerm DISABLE
  SHOW GET lcRefer   DISABLE
  SHOW GET lnTender  DISABLE      

  *--Check if it is not a miselenus customer excange.
  IF lnSr = 1 AND llExchange  &&AND lcCustomer = lcMisAcct
    SHOW GET pbProceed,1 PROMPT lcCanl DISABLE
  ELSE
    SHOW GET pbProceed,1 PROMPT lcCanl ENABLE     
  ENDIF
ENDIF

*--Set back to not display the current payments.
SET ORDER TO

*C101660,1 WAB - change the filter to 4 and 'F' cause we add type 'F' FOr Freight 
*C101660,1 WAB - START
*SET FILTER TO TranType <> '4'
SET FILTER TO !(TranType $ '4FR')
*C101660,1 WAB - END

GO TOP
*--Show browse and refresh.
=lfActBrow()
*--Check Open uncmsess record.
=lfCrUnSess('O',.T.,.F.)

*C102047,1 ABD Open addtional Discount. [Begin]

*B604248,1 ABD - Remak the next variable that we didn't need it now. [Begin]
*llFirstTme = .F.
*B604248,1 ABD - [End]

*C102047,1 ABD [End]

=lfRefresh()
SELECT(lnAlias)
RETURN



*:***********************************************************************
*:  C U S T O M E R   C R E D I T S   P A R T   F U N C T I O N S.
*:***********************************************************************

*:*************************************************************
*: Name      : lfvCCrdit.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Select Customer Credit.
*:*************************************************************
*: Calls     : PSPOSR.SPX
*:             lfCrdBrow()
*:             lfvTrnTyp()
*:             lfwSrBrow()
*:             lfTrapSRKy()
*:             lptab1()
*:             lpBkTab1()
*:             lfvCrdtCode()
*:             lfvNewCrd()
*:             lfvRemCrd()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvCCrdit()
*:*************************************************************
FUNCTION lfvCCrdit

*--Ckeck existance of multiple payments.
SET FILTER TO TranType = '4'
GO TOP
IF !EOF()
  *--You cannot 'select a customer credits' since multiple payments was entered,
  *--Please remove the payments first.
  =gfModalGen('INM42135B42000','DIALOG','select a customer credits')

  *C101660,1 WAB - change the filter to 4 and 'F' cause we add type 'F' FOr Freight 
  *C101660,1 WAB - START
  *SET FILTER TO TranType <> '4'
  SET FILTER TO !(TranType $ '4FR')
  *C101660,1 WAB - END

  RETURN
ENDIF

*--Define screen and browse titels.
lcSrWinTll = 'Customer Credits'
lcBrowSRLn = lcSrWinTll+' Details'

*--Initialize Transaction Type and Transaction No. variable.
STORE SPACE(6) TO lcTran
STORE 1 TO lnCrdtType,lnMRecNo
lnAmount = 0

*--Initialize Credit Array.
DECLARE laCrDbArr[4,2]
laCrDbArr = ' '
laCrDbArr[1,1] = 'Credit Memo'
laCrDbArr[1,2] = '0'
laCrDbArr[2,1] = 'Payment'
laCrDbArr[2,2] = '9'
laCrDbArr[3,1] = 'Credit Adjustment'
laCrDbArr[3,2] = '5'
laCrDbArr[4,1] = 'Credit on Account'
laCrDbArr[4,2] = '6'


SELECT (lcPosTmp)

*C101660,1 WAB - change the filter to 4 and 'F' cause we add type 'F' FOr Freight 
*C101660,1 WAB - START
*SET FILTER TO !(TranType $ '14')
SET FILTER TO !(TranType $ '14FR')
*C101660,1 WAB - END


GO TOP
lcRemStat = IIF(EOF(),'DISABLE','ENABLE')

*--Save keys.
PUSH KEY
ON KEY
ON KEY LABEL ESC llDumy=.T.
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowSRLn)

*--Call Screen.
DO (gcScrDir+gcWinAppl+"\PSPOSR.SPX")
POP KEY

*--Exit screen.
*--Return with selected styles.
STORE 0 TO lnTotal,lnCustAmt

*B803962,1 ABD - Add New setup to return the sale and display the totoal amount
*B803962,1 ABD - In the Paid amount or set it as it work. [Begin]
IF lnSr=2 .AND. llALLRTAMT
*B604522,1 MAN This new setup 'The amount tender display' should not affect the paid amount
*  lnPaid = lnTotal
ENDIF
*B803962,1 ABD - [End]

SELECT (lcPosTmp)
SET FILTER TO
GO TOP
SCAN FOR TranType <> '4'
  IF TranType $ '0569'
    lnCustAmt = lnCustAmt + ABS(nAmount)
  ENDIF
  lnTotal = lnTotal + nAmount
ENDSCAN

*--If Multiple payments donot update the paid amount.
*E301264,1 Start.
*lnPaid = lnTotal
IF lnSr=1
  lnPaid = lnTotal
  lnPaid = IIF(lnPaid<0,0,lnPaid)
  
  *B803962,1 ABD Add new setup to ask the user to display 
  *B803962,1 ABD the amount tender or not. [Begin]
  IF llDsAmntNc
    lnTender = lnPaid
  ENDIF
  *B803962,1 ABD [End]
  
ENDIF
*E301264,1 End.

*--Show Paid,Tendered and Calculate Change amounts.
=lfvPaid()

SHOW GET lnCustAmt
  
*--Set back to not display the current payments.

*C101660,1 WAB - change the filter to 4 and 'F' cause we add type 'F' FOr Freight 
*C101660,1 WAB - START
*SET FILTER TO TranType <> '4'
SET FILTER TO !(TranType $ '4FR')
*C101660,1 WAB - END

GO TOP
*--Show browse and refresh.
=lfActBrow()
*--Check Open uncmsess record.
=lfCrUnSess('O',.T.,.F.)
=lfRefresh()
RETURN


*:*************************************************************
*: Name      : lfCrdBrow.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Credit Transaction Browse.
*:*************************************************************
*: Called from : Screen PSPOSR.SPX
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfCrdBrow()
*:*************************************************************
FUNCTION lfCrdBrow

lnMRecNo = RECNO()
*--Case of Invoice or Credit memo.
lcSrBrFlds ="lcSrMark=IIF(lnMRecNo=RECNO(),'>>','  '):2:H='  ':W=.F.,"+;
            "lcBrType=lfDspSty(.F.) :R :H='Credit type' :24,"+;
            "Tran      :R :H='Tran. #'     :9 ,"+;
            "Reference :R :H='Reference'   :37,"+;
            "nAmount   :R :H='   Amount '  :12 "

BROWSE FIELDS &lcSrBrFlds;
       WINDOW  PSPOSR1   ;
       WHEN lfwSrBrow()  ;
       IN WINDOW PSPOSR;
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOMENU;
       NOWAIT;
       NOEDIT;
       SAVE;
       TITLE lcBrowSRLn
RETURN

*:*************************************************************
*: Name      : lfvTrnTyp.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Validate different credit tran types.
*:             and show only the active selected type.
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvTrnTyp()
*:*************************************************************
FUNCTION lfvTrnTyp

_CUROBJ = OBJNUM(lcTran)
RETURN


*:*************************************************************
*: Name      : lfwSrBrow.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : When function for browse.
*:*************************************************************
*: Call      : lfvChkSel()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfwSrBrow()
*:*************************************************************
FUNCTION lfwSrBrow

lnMRecNo   = RECNO()
lnCrdtType = ASCAN(laCrDbArr,TranType)/2
lcTran     = Tran
lnAmount   = nAmount
SHOW GET lnCrdtType DISABLE
SHOW GET lcTran     DISABLE
SHOW GET lnAmount
SHOW WINDOW (lcBrowSRLn) REFRESH
RETURN

*:*************************************************************
*: Name      : lfTrapSRKy.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Trapping for Customer credits screen.
*:*************************************************************
FUNCTION lfTrapSRKy

IF WONTOP() = lcBrowSRLn
  glFromBrow = .T.
  ON KEY LABEL TAB     DO lpTab1
  ON KEY LABEL BACKTAB DO lpBkTab1
ENDIF  
RETURN

*!*************************************************************
*! Name      : lpTab1
*: Developer : Timour A. K.
*: Date      : 02/02/98
*! Purpose   : Tab key trapping for Customer credits screen.
*!*************************************************************
PROCEDURE lptab1
IF WONTOP()=lcBrowSRLn
  ACTIVATE WINDOW PSPOSR2
  _CUROBJ=OBJNUM(pbAdd)
ELSE
  ACTIVATE WINDOW (lcBrowSRLn)
ENDIF  
RETURN

*!*************************************************************
*! Name      : lpBkTab1
*: Developer : Timour A. K.
*: Date      : 02/02/98
*! Purpose   : Shift Tab key trapping for Customer credits screen.
*!*************************************************************
PROCEDURE lpBkTab1

IF WONTOP()=lcBrowSRLn
  ACTIVATE WINDOW PSPOSR2
  _CUROBJ=OBJNUM(pbClose)
ELSE
  ACTIVATE WINDOW (lcBrowSRLn)
ENDIF
RETURN


*:*************************************************************
*: Name      : lfvCrdtCode.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Validate Customer Credit Transaction number.
*:   1- Credit Memo        TranType = '0'
*:   2- Payment            TranType = '9'
*:   3- Credit Adjustment  TranType = '5'
*:   4- Credit on Account  TranType = '6'
*:*************************************************************
*: Call       : lfAppndLn(),lfwSrBrow()
*:*************************************************************
*: Parammeter : ............
*:*************************************************************
*: Returns    : ............
*:*************************************************************
*: Example    : =lfvCrdtCode()
*:*************************************************************
FUNCTION lfvCrdtCode

*--If empty or does not changed return from fuction.
IF EMPTY(lcTran) OR lcTran=lcOldValue
  RETURN
ENDIF

llBrows = (lcTran='?')

IF !llBrows
  *--Check existance of transaction in credit file.
  llFound = SEEK(lcCustomer+lcTran,'CREDIT')
  IF llFound
    SELECT CREDIT
    LOCATE REST WHILE Account+Tran = lcCustomer+lcTran ;
                  FOR TranType = IIF(lnCrdtType=2,'4',laCrDbArr[lnCrdtType,2])
    llFound = FOUND()  
  ENDIF
ELSE
  llFound = .F.
ENDIF

IF ! llFound
  SELECT CREDIT
  IF !SEEK(lcCustomer)
    *--This account has no credits.  
    =gfModalGen('TRM52002B42000','DIALOG','credit')
    SELECT (lcPosTmp)
    _CUROBJ = OBJNUM(lcTran)
    RETURN
  ENDIF

  lcTTYP = IIF(lnCrdtType=2,'4',laCrDbArr[lnCrdtType,2])
  LOCATE REST WHILE Account=lcCustomer FOR TranType = lcTTYP
  IF !FOUND()
    *--This account has no XXXXXX.  
    =gfModalGen('TRM52002B42000','DIALOG',ALLTRIM(laCrDbArr[lnCrdtType,1]))
    SELECT (lcPosTmp)
    _CUROBJ = OBJNUM(lcTran)
    RETURN
  ENDIF

  IF !llBrows 
    *--This xxx number does not exist.
    IF gfModalGen('INM42129B40007','DIALOG',ALLTRIM(laCrDbArr[lnCrdtType,1])) = 2
      SELECT (lcPosTmp)
      _CUROBJ = OBJNUM(lcTran)
      RETURN
    ENDIF
  ENDIF

  PRIVATE lcBrFields
  lcbrfields = "Tran :H='Transcation #',Account :H='Customer',Amount :H='Total Credit Amount'"
  DIMENSION latemp[1]
  latemp = ''
  =ariabrow([lcCustomer FOR TranType = lcTTYP],'Customer Credits', gnbrfsrow1, gnbrfscol1,;
			gnbrfsrow2, gnbrfscol2, '','','TRAN','laTemp')
  lcTran = latemp[1] 
  IF EMPTY(lcTran)
    SELECT (lcPosTmp)
    _CUROBJ = OBJNUM(lcTran)
    RETURN
  ENDIF
ENDIF

SELECT (lcPosTmp)
*--Check existance of the record.
SET ORDER TO 1  
=SEEK(IIF(lnCrdtType=2,'9',CREDIT.TranType))
LOCATE REST WHILE TranType = IIF(lnCrdtType=2,'9',CREDIT.TranType) FOR Tran = CREDIT.Tran
llAldyEntr = FOUND() 
SET ORDER TO  
IF llAldyEntr
  *--This transaction number was allready entered.
  =gfModalGen('INM42130B42000','DIALOG')
  _CUROBJ = OBJNUM(lcTran)
  RETURN
ENDIF
*--Start append the record depend on the transaction entered.
SELECT CREDIT
*C101660,1 WAB - add paramter to lfappndln()
*C101660,1 WAB - START
*= lfAppndLn(IIF(lnCrdtType=2,'9',TranType),Tran,TranDate,cCreditCod,Reference,'',0,0,0,0,Amount,0,.F.,0)
= lfAppndLn(IIF(lnCrdtType=2,'9',TranType),Tran,TranDate,cCreditCod,Reference,'',0,0,0,0,Amount,0,.F.,0,0)
*C101660,1 WAB - END

SELECT (lcPosTmp)
lnMRecNo = RECNO()
=lfwSrBrow()
SHOW GET pbsrKey    ENABLE  
SHOW GET lnCrdtType DISABLE
SHOW GET lcTran     DISABLE
SHOW GET pbRem      ENABLE
RETURN


*:*************************************************************
*: Name      : lfvNewCrd.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : New Customer Credit Transaction number.
*:*************************************************************
FUNCTION lfvNewCrd

lnCrdtType = 1
lcTran     = SPACE(6)
lnAmount   = 0
SHOW GET lnCrdtType ENABLE
SHOW GET lcTran     ENABLE
SHOW GET lnAmount
_CUROBJ = OBJNUM(lnCrdtType)
RETURN

*:*************************************************************
*: Name      : lfvRemCrd.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Remove Customer Credit Transaction number.
*:*************************************************************
FUNCTION lfvRemCrd

DELETE
GO TOP
lnMRecNo = RECNO()
=lfwSrBrow()
IF EOF()
  SHOW GET pbsrKey DISABLE
  SHOW GET pbRem   DISABLE
ENDIF
RETURN


*:*************************************************************
*: Name      : lfCrtSPay.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Create single payment.
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
FUNCTION lfCrtSPay

*--If multiple payment was selected and no payments was entered.
lnPayTerm = IIF(laPayTerm[lnPayTerm,2]='MMMMMM',1,lnPayTerm)
*--Create payment. 
APPEND BLANK
=RLOCK()
REPLACE TranType  WITH '4',;
        TranCode  WITH laPayTerm[lnPayTerm,2],;
        TranDate  WITH ldPostDate,; 
        cPOSSeq   WITH lcPOSSeq,;
        Reference WITH lcRefer,;
        Account   WITH lcCustomer,;
        StName    WITH lcName,;
        Phone1    WITH lcPhone,;
        Desc1     WITH laPayTerm[lnPayTerm,1],;
        dPostDate WITH ldPostDate

*E301467,1 (Start)
IF llUseAtom
  REPLACE cCrdCard  WITH lcRefer
  
  *B803961,1 ABD - Update the  new 2 fileds that add to the temp file to hold the 
  *B803961,1 ABD - Credit Card number and the approval number. [Begin]

    REPLACE cApprovNm WITH lcAppDesc ,;
            cCardNumr WITH lcCrdData
  *B803961,1 ABD - [End]
ENDIF
*E301467,1 (End).


IF llExchange
  REPLACE nAmount WITH IIF(lnPaid<=0,ABS(lnPaid),-ABS(lnPaid))
ELSE
  REPLACE nAmount WITH IIF(lnSr=1 AND lnTotal>=lnPaid,-ABS(lnPaid),ABS(lnPaid))
ENDIF

*B803962,1 ABD - Add New setup to return the sale and display the total amount
*B803962,1 ABD - In the Paid amount or set it as it work. [Begin]
IF lnSr=2 .AND. llALLRTAMT
*B604522,1 MAN This new setup 'The amount tender display' should not affect the paid amount
*  lnPaid = lnTotal
ENDIF
*B803962,1 ABD - [End]

*-- Call global function to add audit fields info.
=gfAdd_Info(lcPosTmp)
UNLOCK
RETURN


*:*************************************************************
*: Name      : lfUpdSale.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Proceed with Update the sales transactions.
*:*************************************************************
*: Calls     : lfUpdARHst(),lfUpdInvFl(),lfPrintLog()
*:             DO gpSaveInv IN ARINV.PRG
*:             DO lfKeyOff  IN ARKEYOF.PRG
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*:
*:-Note that (lnTotal >= 0 and lnPaid < 0 ) AND 
*:-(lnTotal < 0 and lnPaid > 0 ) case is not Accepted from screen.
*:
*:*************************************************************
*: Example   : =lfUpdSale()
*:*************************************************************
FUNCTION lfUpdSale

*--------------------------------------------------------------------------
*-- 1) Create Payment or Debit adjustment(if Refund) record.
*--------------------------------------------------------------------------
SELECT (lcPosTmp)
=SEEK('4')
LOCATE WHILE TranType = '4' FOR nAmount <> 0
llPayExst = FOUND()

IF llGLLink AND llPayExst
  lcTmpGLD = gfTempName()
  SELECT GLDIST
  COPY STRUCTURE TO (gcWorkDir+lcTmpGLD)
  = gfOpenFile(gcWorkDir+lcTmpGLD ,"","EX")
ENDIF


SELECT (lcPosTmp)
SCAN WHILE TranType = '4' FOR nAmount <> 0
  *--If Total amount of sale is -ve create Debit Adjustment due to
  *--customer refund insted of customer Payment.
  llDebtAdj = (lnTotal < 0)
  *--AR file to update.
  lcArfile = IIF(llDebtAdj,'DEBIT','CREDIT')

  *--Create Payment record (Credit) OR
  *--Create Db / Adj record (Debit)
  IF &lcPosTmp..nSteps < 1 
    *-Payment or D/A transaction and Batch sequence. 
    *lcPayTrn= gfsequence('TRAN')
    lcPayTrn = gfsequence(lcArfile, gcAct_Comp, "", "", "TRAN")
    lcBatch  = gfsequence('BATCH') 

    SELECT (lcArFile)
    APPEND BLANK
    REPLACE Account   WITH lcCustomer,;
            Tran      WITH lcPayTrn,; 
            Batch     WITH lcBatch,;
            TranType  WITH IIF(llDebtAdj,'2','4'),;
            TranDate  WITH ldPostDate,;
            Amount    WITH IIF(llDebtAdj,1,-1)*ABS(&lcPosTmp..nAmount),;
            Desc      WITH IIF(llDebtAdj,'REFUND CUSTOMER','PAYMENT-THANK YOU'),;
            Reference WITH &lcPosTmp..Reference,;
            dPostDate WITH ldPostDate,;
            cCurrCode WITH gcBaseCurr,;
            nCurrUnit WITH 1,;
            nExRate   WITH 1
    *-- Call global function to add audit fields info.
    =gfAdd_Info(lcArFile)

    *--Update the Transaction Code with Payment term code selected or
    *--with Debit adjustment code reason if D/A was created. 
    IF llDebtAdj
      *-- Define the arrays used in the transaction popup.
      DECLARE laReson[1] , laCodInfo[1,10]
      laCodInfo = ""
      lnReson   = 1
      *-- Array hold the header's codes info.
      laCodInfo[1,01] = "TRANCODE"
      laCodInfo[1,02] = "laReson"
      laCodInfo[1,03] = "lnReson"
      STORE .F. TO laCodInfo[1,05],laCodInfo[1,06]
      *-- Fill the popups with the default.
      =gfwCodePop(@laCodInfo,"TRANCODE","D")
      REPLACE TranCode WITH laReson[lnReson , 2]
      *--Read the adjustment code reason to get the GL Account.
      lcGLAcc = ' '
      IF !EMPTY(TranCode)
        DECLARE laTrmRltFd[1,2]
        laTrmRltFd[1,1] = 'CADJACCT'
        laTrmRltFd[1,2] = 'lcGLAcc'
        =gfRltFld(TranCode , @laTrmRltFd , "TRANCODE")
      ENDIF
      lcGLAcc = IIF(EMPTY(lcGLAcc) AND SEEK(lcCurrSafe,"APChecks"),APChecks.cChkGLAcc,lcGLAcc)
    ELSE
      REPLACE cCreditCod WITH &lcPosTmp..TranCode
      lcGLAcc = IIF(SEEK(lcCurrSafe,"APChecks"),APChecks.cChkGLAcc,'')
    ENDIF
    *--Update temp POS Tran file with Payment tran # generated. 
    SELECT (lcPosTmp)
    =RLOCK() 
    REPLACE Tran      WITH lcPayTrn,;
            nSteps    WITH 1,;
            cChkGLAcc WITH lcGLAcc
    UNLOCK

    *--Call TraceKey global function.
    lcInsNo = IIF(UPPER(ALLTRIM(lcArFile))='DEBIT',&lcArFile..cInstalNo, "")
    =gfTraceKey(lcArFile,&lcArFile..TranType+&lcArFile..Tran+lcInsNo,'A')

  ENDIF 

  IF &lcPosTmp..nSteps < 2     
    *-- Update the customer file with : open credit, current, total age
    *-- and net balance.
    SELECT CUSTOMER
    =SEEK('M'+lcCustomer)
    =RLOCK()
    *-- Add the equivelant amount to customer file.
    IF llDebtAdj
      IF laSetups[11,2] = "D"
        REPLACE Current WITH Current + &lcArFile..Amount,;
                TotAge  WITH Current + Age30 + Age60 + Age90 + Age120
      ELSE
        REPLACE TerCurrent WITH TerCurrent + &lcArFile..Amount,;
                TotAge     WITH TerCurrent + TerAge30 + TerAge60 + TerAge90 + TerAge120
      ENDIF  
    ELSE
      REPLACE OpenCr WITH OpenCr + &lcArFile..Amount
    ENDIF
    REPLACE NetBal WITH IIF(llDebtAdj, NETBAL + &lcArFile..Amount,NETBAL+(-1 * ABS(&lcArFile..Amount)))
    UNLOCK

    *E301245 function to update nHgWtrMark Field with NETBAL field [Begin.]
    =lfHgWUpdat() 
    *E301245 function to update nHgWtrMark Field with NETBAL field [End.]

    SELECT (lcPosTmp)
    =RLOCK() 
    REPLACE nSteps WITH 2
    UNLOCK

    *--Call TraceKey global function.
    =gfTraceKey('CUSTOMER','M'+lcCustomer,'M')
  ENDIF 

  IF &lcPosTmp..nSteps < 3     
    *--Create Payment <lnPaid>  (KeyOff)
    *--Create Db / Adj <lnPaid> (KeyOff)
    *--Update Temp ARHist file.
    =lfUpdARHst(lcArFile)
    SELECT (lcPosTmp)
    =RLOCK() 
    REPLACE nSteps WITH 3
    UNLOCK
  ENDIF 


  *--Update APpayment file.
  IF &lcPosTmp..nSteps < 4
    =lfUpdApPay()  

    SELECT (lcPosTmp)
    =RLOCK() 
    REPLACE nSteps WITH 4
    UNLOCK
  ENDIF
  
      

  *--Update Tmp GL dist.
  IF llGLLink
    IF llDebtAdj
     
      *-- If Debit adjustment.
      *** CATEGORY KEY FOR "Debit Adjustments"......=> '010'
      DO GLDIST WITH '','010',-(&lcPosTmp..nAmount), ;
         'DA',&lcPosTmp..Tran,ldPostDate,lcGLFYear,lcGLPeriod,'&lcTmpGLD', ;
         &lcPosTmp..cChkGLAcc,lcCurrCode,lnCurrUnit,lnExRate

      *** CATEGORY KEY FOR "Accounts Receivable"....=> '001'
      DO GLDIST WITH lcLinkCode,'001',&lcPosTmp..nAmount, ;
         'DA',&lcPosTmp..Tran,ldPostDate,lcGLFYear,lcGLPeriod,'&lcTmpGLD', ;
         '',lcCurrCode,lnCurrUnit,lnExRate
    ELSE
      *-- If credit adjustment (Payment).
      *** CATEGORY KEY FOR "Cash Receipts"..........=> '002'
      DO GLDIST WITH '','002',ABS(&lcPosTmp..nAmount) ,;
          'CR',&lcPosTmp..Tran,ldPostDate,lcGLFYear,lcGLPeriod,'&lcTmpGLD',;
           &lcPosTmp..cChkGLAcc,lcCurrCode,lnCurrUnit,lnExRate

      *** CATEGORY KEY FOR "Accounts Receivable"....=> '001'
      DO GLDIST WITH lcLinkCode,'001',-ABS(&lcPosTmp..nAmount),;
           'CR',&lcPosTmp..Tran,ldPostDate,lcGLFYear,lcGLPeriod,'&lcTmpGLD',;
           '',lcCurrCode,lnCurrUnit,lnExRate
    ENDIF
  ENDIF
  
ENDSCAN

*--Update Tmp GL dist.
GO TOP
IF llGLLink AND llPayExst
  IF &lcPosTmp..nSteps < 5
    lcGLSeqNo = gfSequence("GLSESSION", gcAct_Comp)
    SELECT (lcTmpGLD)
    REPLACE ALL GLSESSION WITH lcGLSeqNo
    USE IN (lcTmpGLD)
    *--Append to master GL Dist.      
    SELECT GLDIST
    APPEND FROM (gcWorkDir+lcTmpGLD)
  
    SELECT (lcPosTmp)
    =RLOCK() 
    REPLACE nSteps WITH 5
    UNLOCK
  ELSE
    USE IN (lcTmpGLD)
  ENDIF
ENDIF

*--------------------------------------------------------------------------
*-- 2) Update Invoice Header,Line and Debit record.
*--------------------------------------------------------------------------
*-Declare Invoice sequence.
DECLARE laInvoiceNo[1]
laInvoiceNo[1] = lcInvoiceNo


*--Update temp Invoice header and line and return total invoice amount.
lnTotInvLn=lfUpdInvFl()

*--Call the Invoice Update program.
DO gpSaveInv IN (gcapphome+'AR\ARINV.PRG') WITH ;
     lcInvHdr,lcInvLine,'','','','','',lcGlSession,'laInvoiceNo',.T.



*B603091,1 Start, Comment out.
*--Update Temp AR Hist file with Debit.
*IF SEEK(lcCustomer+lcInvoiceNo,'DEBIT')
*  IF &lcInvHdr..nSteps < 99
*    *--Update Temp ARHist file.
*    =lfUpdARHst('DEBIT')
*    IF lnTotal >= 0  AND (lnTotal-lnPaid) <> 0
*      *--If Defferent between Debits(Total Sale) and Credits(Payment) are +ve.
*      *--Create record for invoice in debit file with balance.
*      *--Update Temp ARHist file.
*      SELECT (lcTmpAR)
*      REPLACE cShToOpn   WITH 'N',;
*              nTrnNewAmn WITH (lnTotal-lnPaid)
*    ELSE
*      IF llExchange AND !llmis AND &lcTmpAR..Amount-lnPaid<>0 
*        *--If Defferent between Debits(Total Sale) and Credits(Payment) are +ve.
*        *--Create record for invoice in debit file with balance.
*        *--Update Temp ARHist file.
*        SELECT (lcTmpAR)
*        REPLACE cShToOpn   WITH 'N',;
*                nTrnNewAmn WITH (&lcTmpAR..Amount-lnPaid)
*      ENDIF
*    ENDIF
*    SELECT (lcInvHdr)
*    =RLOCK() 
*    REPLACE nSteps WITH 99
*    UNLOCK
*  ENDIF
*ENDIF
*B603091,1 End.

SELECT (lcPosTmp)
SET ORDER TO

*--------------------------------------------------------------------------
*-- 3) Update CR Memo and other Credits for KeyOff.
*--------------------------------------------------------------------------
*--Scan all other thansactions (credits) and skip invoice lines and
*--paymets records which already have been updated.

*B603091,1 Start, Comment out.
*--Accomulate the credits exists to not exceed the debits.
*lnCkAmnt = 0
*--Flag to delete unneeded credits from POS transaction file.
*llDelRest= .F.
*SCAN FOR !(TranType $ '14') AND !llExchange AND !llmis
*  IF &lcPosTmp..nSteps < 7 
*    IF llDelRest
*      REPLACE TranType WITH 'X'
*      LOOP
*    ENDIF 
*    IF SEEK(Account+Tran,'CREDIT')
*      IF lnTotal >= 0  OR  lnTotal = lnPaid
*      *--Other Credits (KeyOff).
*        *--Update Temp ARHist file.
*        =lfUpdARHst('CREDIT')
*      ELSE
*      *--Other Credits Up To <lnTotInvLn+lnPaid> (KeyOff).
*        lnCkAmnt = lnCkAmnt + nAmount
*        IF ABS(lnCkAmnt) <= lnTotInvLn+ABS(lnPaid)
*          *--Update Temp ARHist file.
*          =lfUpdARHst('CREDIT')
*        ELSE
*          *--Create record for Credit with balance.
*          *--Update Temp ARHist file.
*          =lfUpdARHst('CREDIT')
*          *--Update Temp ARHist file.
*          SELECT (lcTmpAR)
*          REPLACE cShToOpn   WITH 'N',;
*                  nTrnNewAmn WITH -(ABS(lnCkAmnt)-(lnTotInvln+ABS(lnPaid)))
*          SELECT (lcPosTmp)
*          llDelRest= .T.
*        ENDIF 
*      ENDIF
*    ENDIF 
*    SELECT (lcPosTmp)
*    =RLOCK() 
*    REPLACE nSteps WITH 7
*    UNLOCK
*  ENDIF
*ENDSCAN
*--Delete UnNeeded Credits from POS transaction file.
*IF llDelRest
*  SET ORDER TO 1
*  IF SEEK('X')
*    DELETE REST WHILE TranType = 'X'
*  ENDIF
*  SET ORDER TO
*ENDIF 



*--------------------------------------------------------------------------
*-- 4) Update ARHist file Key Off Transactions.
*--------------------------------------------------------------------------
*--The Key off transactions are collected in lcTmpAr file by calling
*--the lfUpdARHst() function.

*B603091,1 Start, Comment out.
*--If exchange key off the previus return also.
*F llExchange AND llmis
* SELECT (lcPosTmp)
*  SCAN FOR TranType='0'
*    *--Update Temp AR Hist file with RETURN.
*    =SEEK(Account+Tran,'CREDIT')
*    *--Update Temp ARHist file.
*    =lfUpdARHst('CREDIT')
*  
*  ENDSCAN
*  *--Check for open balancd record.
*  SELECT (lcPosTmp)
*  LOCATE FOR TranType='1' AND nAmount<>0 AND CNT=0
*  IF FOUND()
*    lnOpnBalanc = nAmount
*    IF lnOpnBalanc <> 0
*      *--If Defferent between Debits(Total Sale) and D/A are +ve.
*      *--Create record for Credit memo in credit file with balance.
*      *--Update Temp ARHist file.
*      SELECT (lcTmpAR)
*      REPLACE cShToOpn   WITH 'N',;
*              nTrnNewAmn WITH -ABS(lnOpnBalanc)
*    ENDIF
*  ENDIF
*ENDIF

*--Calculate the total debit and total credit amounts 
*--to pass it in lfKeyOff.prg
*STORE 0 TO lnTotDbt,lnTotCrd
*SELECT (lcTmpAR)
*SCAN
*  IF Amount > 0
*    lnTotDbt = lnTotDbt + Amount
*  ELSE
*    lnTotCrd = lnTotCrd + Amount
*  ENDIF
*ENDSCAN
*IF lnTotCrd <> 0
*  *--Call Key Off program to Update.
*  llExtAuto = .F.
*  lcAccount = lcCustomer
*  DO lfKeyOff IN (gcapphome+'ARKEYOF.PRG') WITH ;
*           lcAccount,ldPostDate,lnTotDbt,lnTotCrd,lcTmpAR
*ENDIF
*B603091,1 End.


*--------------------------------------------------------------------------
*-- 5) Update POS Transactions file.
*--------------------------------------------------------------------------
lnPOSLine = 0
SELECT (lcPosTmp)

*C101660,1 WAB - Do not update Empty Amount
*C101660,1 WAB - START
*SCAN


SCAN FOR nAmount <> 0
*C101660,1 WAB - END

  IF &lcPosTmp..nSteps < 9 
    SCATTER MEMVAR
    SELECT POSTRAN
    APPEND BLANK
    GATHER MEMVAR
    REPLACE LineNo WITH lfUpdLnNo(),;
            cType  WITH 'S',;
            cWareCode WITH lcStoreCode,;
            cBnkCode  WITH lcCurrSafe
    *--Update the exchange records.
    IF ( TranType='0' OR TranType='1' AND CNT=0 AND EMPTY(SCALE) )
       REPLACE lFlag WITH .T.
    ENDIF 
    *--Update the refound as return type.
    IF llExchange AND POSTRAN.TranType = '4' AND lnPaid < 0
      REPLACE cType WITH 'R'
    ENDIF
    *:B#604352,1 SSH Replace amount with -ve incase of incase of sales or return.

    IF POSTRAN.TranType = '4' .AND. cType = "S" .AND. nAmount > 0

      REPLACE nAmount WITH -nAmount
    ENDIF
    IF POSTRAN.TranType = '4' .AND. cType = "R" .AND. nAmount < 0
      REPLACE nAmount WITH ABS(nAmount)
    ENDIF
    *:B#604352,1 SSH [END].
    SELECT (lcPosTmp)
    =RLOCK() 
    REPLACE nSteps WITH 9
    UNLOCK
    *--Call TraceKey global function.
    =gfTraceKey('POSTRAN',POSTRAN.cPosSeq+STR(POSTRAN.LineNo,6),'A')
  ENDIF
ENDSCAN


*301282,1 Run Automatic keyoff.
lcAccount = lcCustomer
*B603531,1 Start.
lcKCurCode = gcBaseCurr
*DO lfExterAuto WITH "PS",lcAccount,gcBaseCurr,gdSysDate,1,1 IN (gcapphome+'ARKEYOF.PRG')
DO lfExterAuto WITH "PS",lcAccount,lcKCurCode,gdSysDate,1,1 ;
       IN (gcapphome+'ARKEYOF.PRG')
*B603531,1 End.
*E301282,1 End.

RETURN



*:*************************************************************
*: Name      : lfUpdRetn.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Proceed with Update the return transactions.
*:*************************************************************
*: Calls     : lfUpdARHst(),lfUpdRetFl(),lfPrintLog()
*:             DO gpSaveInv IN ARINV.PRG
*:             DO lfKeyOff  IN ARKEYOF.PRG
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*:
*:-Note that lnTotal allwayes -ve and
*:-lnPaid <= 0 other cases is not Accepted from screen.
*:
*:*************************************************************
*: Example   : =lfUpdRetn()
*:*************************************************************
FUNCTION lfUpdRetn

*--------------------------------------------------------------------------
*-- 1) Create Debit adjustment(Refund-> Reverse Payment) record.
*--------------------------------------------------------------------------
SELECT (lcPosTmp)
=SEEK('4')
LOCATE WHILE TranType = '4' FOR nAmount <> 0
llPayExst = FOUND()

IF llGLLink AND llPayExst
  lcTmpGLD = gfTempName()
  SELECT GLDIST
  COPY STRUCTURE TO (gcWorkDir+lcTmpGLD)
  = gfOpenFile(gcWorkDir+lcTmpGLD ,"","EX")
ENDIF

SELECT (lcPosTmp)
SCAN WHILE TranType = '4' FOR nAmount <> 0

  *--Create Db / Adj record (Debit)
  IF &lcPosTmp..nSteps < 1 
    *-D/A transaction and Batch sequence. 
    *lcPayTrn= gfsequence('TRAN')
    lcPayTrn = gfsequence("DEBIT", gcAct_Comp, "", "", "TRAN")
    lcBatch  = gfsequence('BATCH') 

    SELECT DEBIT
    APPEND BLANK
    REPLACE Account   WITH lcCustomer,;
            Tran      WITH lcPayTrn,; 
            Batch     WITH lcBatch,;
            TranType  WITH '2',;
            TranDate  WITH ldPostDate,;
            Amount    WITH ABS(&lcPosTmp..nAmount),;
            Desc      WITH 'REFUND CUSTOMER',;
            Reference WITH &lcPosTmp..Reference,;
            dPostDate WITH ldPostDate,;
            cCurrCode WITH gcBaseCurr,;
            nCurrUnit WITH 1,;
            nExRate   WITH 1
    *-- Call global function to add audit fields info.
    =gfAdd_Info('DEBIT')

    *--Update the Transaction Code with Debit adjustment code reason.
    *-- Define the arrays used in the transaction popup.
    DECLARE laReson[1] , laCodInfo[1,10]
    laCodInfo = ""
    lnReson   = 1
    *-- Array hold the header's codes info.
    laCodInfo[1,01] = "TRANCODE"
    laCodInfo[1,02] = "laReson"
    laCodInfo[1,03] = "lnReson"
    STORE .F. TO laCodInfo[1,05],laCodInfo[1,06]
    *-- Fill the popups with the default.
    =gfwCodePop(@laCodInfo,"TRANCODE","D")
    REPLACE TranCode WITH laReson[lnReson , 2]
    *--Read the adjustment code reason to get the GL Account.
    lcGLAcc = ' '
    IF !EMPTY(TranCode)
      DECLARE laTrmRltFd[1,2]
      laTrmRltFd[1,1] = 'CADJACCT'
      laTrmRltFd[1,2] = 'lcGLAcc'
      =gfRltFld(TranCode , @laTrmRltFd , "TRANCODE")
    ENDIF
    lcGLAcc = IIF(EMPTY(lcGLAcc) AND SEEK(lcCurrSafe,"APChecks"),APChecks.cChkGLAcc,lcGLAcc)
    *--Update temp POS Tran file with Payment tran # generated. 
    SELECT (lcPosTmp)
    =RLOCK() 
    REPLACE Tran      WITH lcPayTrn,;
            nSteps    WITH 1,;
            cChkGLAcc WITH lcGLAcc
    UNLOCK
    *--Call TraceKey global function.
    =gfTraceKey('DEBIT',DEBIT.TranType+DEBIT.Tran+DEBIT.cInstalNo,'A')
  ENDIF 


  IF &lcPosTmp..nSteps < 2     
    *-- Update the customer file with : open credit, current, total age
    *-- and net balance.
    SELECT CUSTOMER
    =SEEK('M'+lcCustomer)
    =RLOCK()
    IF laSetups[11,2] = "D"
      REPLACE Current WITH Current + DEBIT.Amount,;
              TotAge  WITH Current + Age30 + Age60 + Age90 + Age120
    ELSE
      REPLACE TerCurrent WITH TerCurrent + DEBIT.Amount,;
              TotAge     WITH TerCurrent + TerAge30 + TerAge60 + TerAge90 + TerAge120
    ENDIF  
    REPLACE NetBal WITH NETBAL + DEBIT.Amount
    UNLOCK

    *E301245 function to update nHgWtrMark Field with NETBAL field [Begin.]
    =lfHgWUpdat() 
    *E301245 function to update nHgWtrMark Field with NETBAL field [End.]

    *--Call TraceKey global function.
    =gfTraceKey('CUSTOMER','M'+lcCustomer,'M')

    *--Create Db / Adj <lnPaid> (KeyOff)
    *--Update Temp ARHist file.
    =lfUpdARHst('DEBIT')
    SELECT (lcPosTmp)
    =RLOCK() 
    REPLACE nSteps WITH 2
    UNLOCK
  ENDIF 

  *--Update APpayment file.
  IF &lcPosTmp..nSteps < 3
    =lfUpdApPay()  

    SELECT (lcPosTmp)
    =RLOCK() 
    REPLACE nSteps WITH 3
    UNLOCK
  ENDIF

  *--Update Tmp GL dist.
  IF llGLLink
    *** CATEGORY KEY FOR "Debit Adjustments"......=> '010'
    DO GLDIST WITH '','010',-(&lcPosTmp..nAmount), ;
       'DA',&lcPosTmp..Tran,ldPostDate,lcGLFYear,lcGLPeriod,'&lcTmpGLD', ;
       &lcPosTmp..cChkGLAcc,lcCurrCode,lnCurrUnit,lnExRate

    *** CATEGORY KEY FOR "Accounts Receivable"....=> '001'
    DO GLDIST WITH lcLinkCode,'001',&lcPosTmp..nAmount, ;
       'DA',&lcPosTmp..Tran,ldPostDate,lcGLFYear,lcGLPeriod,'&lcTmpGLD', ;
       '',lcCurrCode,lnCurrUnit,lnExRate
  ENDIF


ENDSCAN

*--Update Tmp GL dist.
GO TOP
IF llGLLink AND llPayExst
  IF &lcPosTmp..nSteps < 4
    lcGLSeqNo = gfSequence("GLSESSION", gcAct_Comp)
    SELECT (lcTmpGLD)
    REPLACE ALL GLSESSION WITH lcGLSeqNo
    USE IN (lcTmpGLD)
    *--Append to master GL Dist.      
    SELECT GLDIST
    APPEND FROM (gcWorkDir+lcTmpGLD)
  
    SELECT (lcPosTmp)
    =RLOCK() 
    REPLACE nSteps WITH 4
    UNLOCK
  ELSE
    USE IN (lcTmpGLD)
  ENDIF
ENDIF


*--------------------------------------------------------------------------
*-- 2) Update Return Header,Line and Credit record.
*--------------------------------------------------------------------------

*--Update temp Return header and line.
=lfUpdRetFl()

*--Open files Needed for Update Rrturn program.
IF !laFlOpn[9,1]
  laFlOpn[9,1]=gfOpenFile(gcDataDir+'RALINE','RALINE','SH')
  laFlOpn[9,2]='RALINE'
ENDIF
IF !laFlOpn[10,1]
  laFlOpn[10,1]=gfOpenFile(gcDataDir+'RETAUTH','RETAUTH','SH')
  laFlOpn[10,2]='RETAUTH'
ENDIF
IF !laFlOpn[11,1]
  laFlOpn[11,1]=gfOpenFile(gcDataDir+'ICSTYHST','STYHST','SH')
  laFlOpn[11,2]='ICSTYHST'
ENDIF

*--Call the Return Update program.
*B802803,2 The program became shared
*DO lfSavRet IN (gcapphome+'RM\RMCRMEM.FXP') WITH .T. ,lcRetHdr , lcRetLine
DO lfSavRet IN (gcapphome+'RMCRMEM.FXP') WITH .T. ,lcRetHdr , lcRetLine

*--Balance was changed after return.
IF llExchange
  lnBal = IIF(!llMis,Customer.NetBal,0)
  lcBalColor = IIF(lnBal>0,"RGB(255,0,0,192,192,192)","RGB(0,128,0,192,192,192)")
  
  *C200153,4 ABD - Get the additional discount for selected customer
  *C200153,4 ABD - In case he made sales with anther addititonal discount 
  *C200153,4 ABD - Before that. [Begin]
  IF Customer.Expires >= gdSysDate .AND. Customer.Disc # 0 
    lnCustDsc = Customer.Disc
    llCustDsc = .T.
    = lfwAddDisc ()
  ELSE
    lnCustDsc = 0
    llCustDsc = .F.
    = gfwCodePop( @laCodInfo, "CADDRETDIS" ,'N')  
    = gfwCodePop( @laCodInfo, "CADDRETDIS" ,'L')
    SHOW GET lnAddDisc ENABLE
  ENDIF
  *C200153,4 ABD - [End]
  
ENDIF


*B603091,1 Start, Comment out.
*--Update Temp AR Hist file with RETURN.
*IF SEEK(lcCustomer+lcCrMemoNo,'CREDIT')
*  IF &lcRetHdr..nSteps < 99
*    *--Update Temp ARHist file.
*    =lfUpdARHst('CREDIT')
*    SELECT (lcPosTmp)
*    STORE 0 TO lnOpnBalanc,lnRetTot
*    LOCATE FOR TranType='1' AND nAmount<>0
*    IF FOUND()
*      lnOpnBalanc = nAmount
*      SUM nAmount TO lnRetTot FOR TranType='0'
*      GO TOP
*    ENDIF  
*    IF (lnTotal-lnPaid) <> 0 OR lnOpnBalanc <> 0
*      *--If Defferent between Debits(Total Sale) and D/A are +ve.
*      *--Create record for Credit memo in credit file with balance.
*      *--Update Temp ARHist file.
*      SELECT (lcTmpAR)
*      REPLACE cShToOpn   WITH 'N',;
*              nTrnNewAmn WITH IIF(lnOpnBalanc=0,(lnTotal-lnPaid),(lnRetTot-lnPaid) )
*    ENDIF
*    SELECT (lcRetHdr)
*    =RLOCK() 
*    REPLACE nSteps WITH 99
*    UNLOCK
*  ENDIF
*ENDIF
*B603091,1 End.

*--------------------------------------------------------------------------
*-- 3) Update ARHist file Key Off Transactions.
*--------------------------------------------------------------------------
*--The Key off transactions are collected in lcTmpAr file by calling
*--the lfUpdARHst() function.

*B603091,1 Start, Comment out.
*IF !llExchange OR !llmis
*  IF lnPaid <> 0
*    *--Calculate the total debit and total credit amounts 
*    *--to pass it in lfKeyOff.prg
*    STORE 0 TO lnTotDbt,lnTotCrd
*    SELECT (lcTmpAR)
*    SCAN
*      IF Amount > 0
*        lnTotDbt = lnTotDbt + Amount
*      ELSE
*        lnTotCrd = lnTotCrd + Amount
*      ENDIF
*    ENDSCAN
*    *--Call Key Off program to Update.
*    llExtAuto = .F.
*    lcAccount = lcCustomer
*    DO lfKeyOff IN (gcapphome+'ARKEYOF.PRG') WITH ;
*           lcAccount,(ldPostDate),lnTotDbt,lnTotCrd,lcTmpAR
*  ENDIF
*ENDIF
*B603091,1 End.


*--------------------------------------------------------------------------
*-- 5) Update POS Transactions file.
*--------------------------------------------------------------------------
lnPOSLine = 0
SELECT (lcPosTmp)
*C101660,1 WAB - Do not update Empty Amount
*C101660,1 WAB - START
*SCAN
SCAN FOR nAmount <> 0
*C101660,1 WAB - END

  IF &lcPosTmp..nSteps < 9 
    SCATTER MEMVAR
    SELECT POSTRAN
    APPEND BLANK
    GATHER MEMVAR
    REPLACE LineNo    WITH lfUpdLnNo(),;
            cType     WITH 'R',;
            cWareCode WITH lcStoreCode,;
            cBnkCode  WITH lcCurrSafe,;
            Invoice   WITH IIF(EMPTY(Invoice),lcInvNo,Invoice)
    *:B#604352,1 SSH Replace amount with -ve incase of incase of return.
    IF POSTRAN.TranType = '4' .AND. cType = "R" .AND. nAmount < 0
      REPLACE nAmount WITH ABS(nAmount)
    ENDIF
    *:B#604352,1 SSH [END]
    SELECT (lcPosTmp)
    =RLOCK() 
    REPLACE nSteps WITH 9
    UNLOCK
    *--Call TraceKey global function.
    =gfTraceKey('POSTRAN',POSTRAN.cPosSeq+STR(POSTRAN.LineNo,6),'A')
  ENDIF
ENDSCAN


*301282,1 Run Automatic keyoff.
lcAccount = lcCustomer
*B603531,1 Start.
lcKCurCode = gcBaseCurr
*DO lfExterAuto WITH "PS",lcAccount,gcBaseCurr,gdSysDate,1,1 ;
       IN (gcapphome+'ARKEYOF.PRG')
DO lfExterAuto WITH "PS",lcAccount,lcKCurCode,gdSysDate,1,1 ;
       IN (gcapphome+'ARKEYOF.PRG')
*B603531,1 End.
*E301282,1 End.

RETURN



*:*************************************************************
*: Name      : lfUpdExchg.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Update exchange.
*:*************************************************************
*: Calls     : ..............
*:*************************************************************
*: Passed Parameters  : 
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfUpdExchg()
*:*************************************************************
FUNCTION lfUpdExchg

*--------------------------------------------------------------------------
*-- 1) Create Payment or Debit adjustment(if Refund) record.
*--------------------------------------------------------------------------
SELECT (lcPosTmp)
SET ORDER TO 1
SET FILTER TO

=SEEK('4')
LOCATE WHILE TranType = '4' FOR nAmount <> 0
llPayExst = FOUND()

IF llGLLink AND llPayExst
  lcTmpGLD = gfTempName()
  SELECT GLDIST
  COPY STRUCTURE TO (gcWorkDir+lcTmpGLD)
  = gfOpenFile(gcWorkDir+lcTmpGLD ,"","EX")
ENDIF

SELECT (lcPosTmp)
SCAN WHILE TranType = '4' FOR nAmount <> 0
  *--If Total amount of sale is -ve create Debit Adjustment due to
  *--customer refund insted of customer Payment.
  llDebtAdj = (lnPaid < 0)
  *--AR file to update.
  lcArfile = IIF(llDebtAdj,'DEBIT','CREDIT')

  *--Create Payment record (Credit) OR
  *--Create Db / Adj record (Debit)
  IF &lcPosTmp..nSteps < 1 
    *-Payment or D/A transaction and Batch sequence. 
    lcPayTrn = gfsequence(lcArfile, gcAct_Comp, "", "", "TRAN")
    lcBatch  = gfsequence('BATCH') 

    SELECT (lcArFile)
    APPEND BLANK
    REPLACE Account   WITH lcCustomer,;
            Tran      WITH lcPayTrn,; 
            Batch     WITH lcBatch,;
            TranType  WITH IIF(llDebtAdj,'2','4'),;
            TranDate  WITH ldPostDate,;
            Amount    WITH IIF(llDebtAdj,1,-1)*ABS(&lcPosTmp..nAmount),;
            Desc      WITH IIF(llDebtAdj,'REFUND CUSTOMER','PAYMENT-THANK YOU'),;
            Reference WITH &lcPosTmp..Reference,;
            dPostDate WITH ldPostDate,;
            cCurrCode WITH gcBaseCurr,;
            nCurrUnit WITH 1,;
            nExRate   WITH 1
    *-- Call global function to add audit fields info.
    =gfAdd_Info(lcArFile)

    *--Update the Transaction Code with Payment term code selected or
    *--with Debit adjustment code reason if D/A was created. 
    IF llDebtAdj
      *-- Define the arrays used in the transaction popup.
      DECLARE laReson[1] , laCodInfo[1,10]
      laCodInfo = ""
      lnReson   = 1
      *-- Array hold the header's codes info.
      laCodInfo[1,01] = "TRANCODE"
      laCodInfo[1,02] = "laReson"
      laCodInfo[1,03] = "lnReson"
      STORE .F. TO laCodInfo[1,05],laCodInfo[1,06]
      *-- Fill the popups with the default.
      =gfwCodePop(@laCodInfo,"TRANCODE","D")
      REPLACE TranCode WITH laReson[lnReson , 2]
      *--Read the adjustment code reason to get the GL Account.
      lcGLAcc = ' '
      IF !EMPTY(TranCode)
        DECLARE laTrmRltFd[1,2]
        laTrmRltFd[1,1] = 'CADJACCT'
        laTrmRltFd[1,2] = 'lcGLAcc'
        =gfRltFld(TranCode , @laTrmRltFd , "TRANCODE")
      ENDIF
      lcGLAcc = IIF(EMPTY(lcGLAcc) AND SEEK(lcCurrSafe,"APChecks"),APChecks.cChkGLAcc,lcGLAcc)
    ELSE
      REPLACE cCreditCod WITH &lcPosTmp..TranCode
      lcGLAcc = IIF(SEEK(lcCurrSafe,"APChecks"),APChecks.cChkGLAcc,'')
    ENDIF
    *--Update temp POS Tran file with Payment tran # generated. 
    SELECT (lcPosTmp)
    =RLOCK() 
    REPLACE Tran      WITH lcPayTrn,;
            nSteps    WITH 1,;
            cChkGLAcc WITH lcGLAcc
    UNLOCK

    *--Call TraceKey global function.
    lcInsNo = IIF(UPPER(ALLTRIM(lcArFile))='DEBIT',&lcArFile..cInstalNo, "")
    =gfTraceKey(lcArFile,&lcArFile..TranType+&lcArFile..Tran+lcInsNo,'A')

  ENDIF 

  IF &lcPosTmp..nSteps < 2     
    *-- Update the customer file with : open credit, current, total age
    *-- and net balance.
    SELECT CUSTOMER
    =SEEK('M'+lcCustomer)
    =RLOCK()
    IF llDebtAdj
      IF laSetups[11,2] = "D"
        REPLACE Current WITH Current + &lcArFile..Amount,;
                TotAge  WITH Current + Age30 + Age60 + Age90 + Age120
      ELSE
        REPLACE TerCurrent WITH TerCurrent + &lcArFile..Amount,;
                TotAge     WITH TerCurrent + TerAge30 + TerAge60 + TerAge90 + TerAge120
      ENDIF  
    ELSE
      REPLACE OpenCr WITH OpenCr + &lcArFile..Amount
    ENDIF
    REPLACE NetBal WITH IIF(llDebtAdj, NETBAL + &lcArFile..Amount,NETBAL+(-1 * ABS(&lcArFile..Amount)))
    UNLOCK

    *E301245 function to update nHgWtrMark Field with NETBAL field [Begin.]
    =lfHgWUpdat() 
    *E301245 function to update nHgWtrMark Field with NETBAL field [End.]

    SELECT (lcPosTmp)
    =RLOCK() 
    REPLACE nSteps WITH 2
    UNLOCK

    *--Call TraceKey global function.
    =gfTraceKey('CUSTOMER','M'+lcCustomer,'M')
  ENDIF 

  IF &lcPosTmp..nSteps < 3     
    *--Create Payment <lnPaid>  (KeyOff)
    *--Create Db / Adj <lnPaid> (KeyOff)
    *--Update Temp ARHist file.
    =lfUpdARHst(lcArFile)
    SELECT (lcPosTmp)
    =RLOCK() 
    REPLACE nSteps WITH 3
    UNLOCK
  ENDIF 


  *--Update APpayment file.
  IF &lcPosTmp..nSteps < 4
    =lfUpdApPay()  

    SELECT (lcPosTmp)
    =RLOCK() 
    REPLACE nSteps WITH 4
    UNLOCK
  ENDIF
  
      

  *--Update Tmp GL dist.
  IF llGLLink
    IF llDebtAdj
     
      *-- If Debit adjustment.
      *** CATEGORY KEY FOR "Debit Adjustments"......=> '010'
      DO GLDIST WITH '','010',-(&lcPosTmp..nAmount), ;
         'DA',&lcPosTmp..Tran,ldPostDate,lcGLFYear,lcGLPeriod,'&lcTmpGLD', ;
         &lcPosTmp..cChkGLAcc,lcCurrCode,lnCurrUnit,lnExRate

      *** CATEGORY KEY FOR "Accounts Receivable"....=> '001'
      DO GLDIST WITH lcLinkCode,'001',&lcPosTmp..nAmount, ;
         'DA',&lcPosTmp..Tran,ldPostDate,lcGLFYear,lcGLPeriod,'&lcTmpGLD', ;
         '',lcCurrCode,lnCurrUnit,lnExRate
    ELSE
      *-- If credit adjustment (Payment).
      *** CATEGORY KEY FOR "Cash Receipts"..........=> '002'
      DO GLDIST WITH '','002',ABS(&lcPosTmp..nAmount) ,;
          'CR',&lcPosTmp..Tran,ldPostDate,lcGLFYear,lcGLPeriod,'&lcTmpGLD',;
           &lcPosTmp..cChkGLAcc,lcCurrCode,lnCurrUnit,lnExRate

      *** CATEGORY KEY FOR "Accounts Receivable"....=> '001'
      DO GLDIST WITH lcLinkCode,'001',-ABS(&lcPosTmp..nAmount),;
           'CR',&lcPosTmp..Tran,ldPostDate,lcGLFYear,lcGLPeriod,'&lcTmpGLD',;
           '',lcCurrCode,lnCurrUnit,lnExRate
    ENDIF
  ENDIF
  
ENDSCAN

*--Update Tmp GL dist.
GO TOP
IF llGLLink AND llPayExst
  IF &lcPosTmp..nSteps < 5
    lcGLSeqNo = gfSequence("GLSESSION", gcAct_Comp)
    SELECT (lcTmpGLD)
    REPLACE ALL GLSESSION WITH lcGLSeqNo
    USE IN (lcTmpGLD)
    *--Append to master GL Dist.      
    SELECT GLDIST
    APPEND FROM (gcWorkDir+lcTmpGLD)
  
    SELECT (lcPosTmp)
    =RLOCK() 
    REPLACE nSteps WITH 5
    UNLOCK
  ELSE
    USE IN (lcTmpGLD)
  ENDIF
ENDIF


*--Calculate needed variabels.
SELECT (lcPosTmp)
SET FILTER TO
STORE 0 TO lnRetAmnt,lnOpenRet,lnPayAmnt,lnInvAmnt
SCAN
  IF TranType='0'
    lnRetAmnt = lnRetAmnt + nAmount
  ENDIF
  IF TranType='0' OR (TranType='1' AND EMPTY(Scale))
    lnOpenRet = lnOpenRet + nAmount
  ENDIF
  IF TranType='4'
    lnPayAmnt = lnPayAmnt + nAmount
  ENDIF
  IF TranType='1' AND !EMPTY(Scale)
    lnInvAmnt = lnInvAmnt + nAmount
  ENDIF
ENDSCAN
lnIRDiff  = lnInvAmnt + lnOpenRet
lnOpenBal = lnIRDiff + lnPayAmnt


*--------------------------------------------------------------------------
*-- 2) Update Return Header,Line and Credit record.
*--------------------------------------------------------------------------
SELECT (lcPosTmp)
SET ORDER TO 1
SET FILTER TO lToExc 
*--Update temp Return header and line.
=lfUpdRetFl()
SELECT (lcPosTmp)
SET FILTER TO

*--Open files Needed for Update Rrturn program.
IF !laFlOpn[9,1]
  laFlOpn[9,1]=gfOpenFile(gcDataDir+'RALINE','RALINE','SH')
  laFlOpn[9,2]='RALINE'
ENDIF
IF !laFlOpn[10,1]
  laFlOpn[10,1]=gfOpenFile(gcDataDir+'RETAUTH','RETAUTH','SH')
  laFlOpn[10,2]='RETAUTH'
ENDIF
IF !laFlOpn[11,1]
  laFlOpn[11,1]=gfOpenFile(gcDataDir+'ICSTYHST','STYHST','SH')
  laFlOpn[11,2]='ICSTYHST'
ENDIF

*--Call the Return Update program.
*B802803,2 The program became shared
*DO lfSavRet IN (gcapphome+'RM\RMCRMEM.FXP') WITH .T. ,lcRetHdr , lcRetLine
DO lfSavRet IN (gcapphome+'RMCRMEM.FXP') WITH .T. ,lcRetHdr , lcRetLine
*--Balance was changed after return.
lnBal = IIF(!llMis,Customer.NetBal,0)
lcBalColor = IIF(lnBal>0,"RGB(255,0,0,192,192,192)","RGB(0,128,0,192,192,192)")



*--------------------------------------------------------------------------
*-- 2) Update Invoice Header,Line and Debit record.
*--------------------------------------------------------------------------
*-Declare Invoice sequence.
DECLARE laInvoiceNo[1]
laInvoiceNo[1] = lcInvoiceNo

SELECT (lcPosTmp)
SET ORDER TO 1
SET FILTER TO !lToExc
*--Update temp Invoice header and line and return total invoice amount.
lnTotInvLn=lfUpdInvFl()
SELECT (lcPosTmp)
SET FILTER TO

*--Call the Invoice Update program.
DO gpSaveInv IN (gcapphome+'AR\ARINV.PRG') WITH ;
     lcInvHdr,lcInvLine,'','','','','',lcGlSession,'laInvoiceNo',.T.



*--Update Temp AR Hist file with RETURN.
IF SEEK(lcCustomer+lcCrMemoNo,'CREDIT') AND &lcRetHdr..nSteps < 99
  *--Update Temp ARHist file.
  =lfUpdARHst('CREDIT')

  SELECT (lcRetHdr)
  =RLOCK() 
  REPLACE nSteps WITH 99
  UNLOCK
ENDIF

*--Update Temp AR Hist file with Debit.
IF SEEK(lcCustomer+lcInvoiceNo,'DEBIT') AND &lcInvHdr..nSteps < 99
  *--Update Temp ARHist file.
  =lfUpdARHst('DEBIT')

  SELECT (lcInvHdr)
  =RLOCK() 
  REPLACE nSteps WITH 99
  UNLOCK
ENDIF


*B603091,1 Start, Comment out.
*--Update the open balance.
*IF lnOpenBal <> 0
*  SELECT (lcTmpAR)
*  IF lnOpenBal > 0   && Open balance on invoice.
*    LOCATE FOR TRANTYPE='1'
*  ELSE     && Open balance on Return.
*    LOCATE FOR TRANTYPE='0'
*  ENDIF
*  *--Update Temp ARHist file.
*  REPLACE cShToOpn   WITH 'N',;
*          nTrnNewAmn WITH lnOpenBal
*ENDIF

*--------------------------------------------------------------------------
*-- 4) Update ARHist file Key Off Transactions.
*--------------------------------------------------------------------------

*B603091,1 Start, Comment out.
*--Calculate the total debit and total credit amounts 
*--to pass it in lfKeyOff.prg
*STORE 0 TO lnTotDbt,lnTotCrd
*SELECT (lcTmpAR)
*SCAN
*  IF Amount > 0
*    lnTotDbt = lnTotDbt + Amount
*  ELSE
*    lnTotCrd = lnTotCrd + Amount
*  ENDIF
*ENDSCAN
*IF lnTotCrd <> 0 AND lnTotDbt <> 0
*  *--Call Key Off program to Update.
*  llExtAuto = .F.
*  lcAccount = lcCustomer
*  DO lfKeyOff IN (gcapphome+'ARKEYOF.PRG') WITH ;
*           lcAccount,ldPostDate,lnTotDbt,lnTotCrd,lcTmpAR
*ENDIF
*B603091,1 End.

*--------------------------------------------------------------------------
*-- 5) Update POS Transactions file.
*--------------------------------------------------------------------------
lnPOSLine = 0
SELECT (lcPosTmp)
SCAN
  IF &lcPosTmp..nSteps < 9 
    SCATTER MEMVAR
    SELECT POSTRAN
    *--Update Priveus return.
    IF &lcPosTmp..TranType='0'
      APPEND BLANK
      GATHER MEMVAR
      REPLACE LineNo WITH lfUpdLnNo(),;
              cType  WITH 'R',;
              cWareCode WITH lcStoreCode,;
              cBnkCode  WITH lcCurrSafe,;
              cPOSSeq   WITH ALLT(&lcPosTmp..cOWner),;
              COWNER    WITH ' ' 
      *:B#604352,1 SSH Replace amount with -ve incase of incase of return.
      IF POSTRAN.TranType = '4' .AND. cType = "R" .AND. nAmount < 0
        REPLACE nAmount WITH ABS(nAmount)
      ENDIF
      *:B#604352,1 SSH [END]
      *--Call TraceKey global function.
      =gfTraceKey('POSTRAN',POSTRAN.cPosSeq+STR(POSTRAN.LineNo,6),'A')
    ENDIF
    SELECT POSTRAN
    APPEND BLANK
    GATHER MEMVAR
    REPLACE LineNo WITH lfUpdLnNo(),;
            cType  WITH 'S',;
            cWareCode WITH lcStoreCode,;
            cBnkCode  WITH lcCurrSafe,;
            COWNER    WITH ' ' 
    IF ( TranType='0' OR TranType='1' AND CNT=0 AND EMPTY(SCALE) )
       REPLACE lFlag WITH .T.
    ENDIF 
    *:B#604352,1 SSH Replace amount with -ve incase of incase of sales.
    *B604522,1 MAN Wrong sign update for the refund
*    IF POSTRAN.TranType = '4' .AND. cType = "S" .AND. nAmount > 0
*      REPLACE nAmount WITH -nAmount
*    ENDIF
    *:B#604352,1 SSH [END].
    SELECT (lcPosTmp)
    =RLOCK() 
    REPLACE nSteps WITH 9
    UNLOCK
    *--Call TraceKey global function.
    =gfTraceKey('POSTRAN',POSTRAN.cPosSeq+STR(POSTRAN.LineNo,6),'A')
  ENDIF
ENDSCAN


*301282,1 Run Automatic keyoff.
lcAccount = lcCustomer
*B603531,1 Start.
lcKCurCode = gcBaseCurr
*DO lfExterAuto WITH "PS",lcAccount,gcBaseCurr,gdSysDate,1,1 ;
         IN (gcapphome+'ARKEYOF.PRG')
DO lfExterAuto WITH "PS",lcAccount,lcKCurCode,gdSysDate,1,1 ;
         IN (gcapphome+'ARKEYOF.PRG')
*B603531,1 End.
*E301282,1 End.

RETURN


*:*************************************************************
*: Name      : lfUpdARHst.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Append record in temp ARHist file.
*:*************************************************************
*: Calls     : ..............
*:*************************************************************
*: Passed Parameters  : lcArFile -> AR file (DEBIT or CREDIT).
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfUpdARHst()
*:*************************************************************
FUNCTION lfUpdARHst
PARA lcArFile

lnAlias = SELECT()
*--You have to be on the right record on (Debit/Credit) file.
*--Update temp ARHist file which will be sent to KeyOff program.
SELECT (lcArFile)
IF lcArFile='DEBIT'
  SCATTER FIELDS ACCOUNT,STORE,TRANTYPE,TRANCODE,TRAN,CINSTALNO,;
        TRANDATE,DPOSTDATE,CHGBK_DATE,DESC,REFERENCE,AMOUNT,BATCH,;
        DUEDATE,CFACCODE,DSC_AMT,GLFLAG,MON_FLG,LINK_CODE,CCURRCODE,;
        NCURRUNIT,NEXRATE,CADJACCT,CARGLACC TO laSctFlds
  SELECT (lcTmpAR)
  APPEND BLANK
  GATHER FROM laSctFlds FIELDS ACCOUNT,STORE,TRANTYPE,TRANCODE,TRAN,;
        CINSTALNO,TRANDATE,DPOSTDATE,CHGBK_DATE,DESC,REFERENCE,AMOUNT,BATCH,;
        DUEDATE,CFACCODE,DSC_AMT,GLFLAG,MON_FLG,LINK_CODE,CCURRCODE,;
        NCURRUNIT,NEXRATE,CADJACCT,CARGLACC
  REPLACE cShToOpn WITH 'Y'
ELSE  && CREDIT
  SCATTER FIELDS ACCOUNT,STORE,TRANTYPE,CCREDITCOD,TRAN,TRANDATE,;
        DPOSTDATE,DESC,REFERENCE,AMOUNT,BATCH,HISTORY,CFACCODE,DSC_AMT,;
        GLFLAG,MON_FLG,LINK_CODE,CCURRCODE,NCURRUNIT,NEXRATE,CADJACCT,;
        CARGLACC,CBNKCODE,CCHKACCT  TO laSctFlds
  SELECT (lcTmpAR)
  APPEND BLANK
  GATHER FROM laSctFlds FIELDS ACCOUNT,STORE,TRANTYPE,TRANCODE,TRAN,;
        TRANDATE,DPOSTDATE,DESC,REFERENCE,AMOUNT,BATCH,HISTORY,CFACCODE,;
        DSC_AMT,GLFLAG,MON_FLG,LINK_CODE,CCURRCODE,NCURRUNIT,NEXRATE,;
        CADJACCT,CARGLACC,CBNKCODE,CCHKACCT
  REPLACE cShToOpn WITH 'Y'
ENDIF
SELECT(lnAlias)
RETURN


*:*************************************************************
*: Name      : lfCrtTpFls.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Create temp files for 
*:  (ARHIST and INVHDR,INVLINE or RETHDR,RETLINE).
*:*************************************************************
*: Calls     : =gfCrtTmp()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfCrtTpFls()
*:*************************************************************
FUNCTION lfCrtTpFls
PRIVATE lnAlias
lnAlias = SELECT()

********************************************
*--All keyed-off transactions temporary file.
SELECT ARHIST
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
*B804145,1 ABD - Comment the next line this filed is allready 
*B804145,1 ABD - found in the ARHIST file so we will remove
*B804145,1 ABD - the filed from the Weight of the array. [Begin]
*DIMENSION laFileStru[lnFileStru+7,4]
DIMENSION laFileStru[lnFileStru+5,4]
*B804145,1 ABD [End]
laFileStru[lnFileStru+1,1] = "nStep"
laFileStru[lnFileStru+1,2] = "N"
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = "cYear"
laFileStru[lnFileStru+2,2] = "C"
laFileStru[lnFileStru+2,3] = 4
laFileStru[lnFileStru+2,4] = 0
laFileStru[lnFileStru+3,1] = "cPrd"
laFileStru[lnFileStru+3,2] = "C"
laFileStru[lnFileStru+3,3] = 2
laFileStru[lnFileStru+3,4] = 0

*B804145,1 ABD - Comment the next line this filed is allready 
*B804145,1 ABD - found in the ARHIST file. [Begin]
*laFileStru[lnFileStru+4,1] = "cbnkcode"
*laFileStru[lnFileStru+4,2] = "C"
*laFileStru[lnFileStru+4,3] = 8
*laFileStru[lnFileStru+4,4] = 0
*B804145,1 ABD [End]

*laFileStru[lnFileStru+5,1] = "cchkacct"
*laFileStru[lnFileStru+5,2] = "C"
*laFileStru[lnFileStru+5,3] = 12
*laFileStru[lnFileStru+5,4] = 0
*laFileStru[lnFileStru+6,1] = "cShToOpn"
*laFileStru[lnFileStru+6,2] = "C"
*laFileStru[lnFileStru+6,3] = 1
*laFileStru[lnFileStru+6,4] = 0
*laFileStru[lnFileStru+7,1] = "nTrnNewAmn"
*laFileStru[lnFileStru+7,2] = "N"
*laFileStru[lnFileStru+7,3] = 11
*laFileStru[lnFileStru+7,4] = 2

laFileStru[lnFileStru+4,1] = "cShToOpn"
laFileStru[lnFileStru+4,2] = "C"
laFileStru[lnFileStru+4,3] = 1
laFileStru[lnFileStru+4,4] = 0
laFileStru[lnFileStru+5,1] = "nTrnNewAmn"
laFileStru[lnFileStru+5,2] = "N"
laFileStru[lnFileStru+5,3] = 11
laFileStru[lnFileStru+5,4] = 2
*B804145,1 ABD [End]
*--Index tags array.
DIMENSION laTags[3,2]
laTags[1,1] = 'Account+TranType+Tran+cInstalNo'
laTags[1,2] = lcTmpAR
laTags[2,1] = 'Account+Tran+cInstalNo'
laTags[2,2] = 'Tran'
laTags[3,1] = 'cShToOpn'
laTags[3,2] = 'Show'

*--Call Create temp file.
=gfCrtTmp(lcTmpAR,@laFileStru,@laTags)
SELECT (lcTmpAR)
SET ORDER TO TAG (lcTmpAR)


*****************************
*--Create invoice temp files.
IF lnSr = 1
  *--Create Temprorary invoice header.
  SELECT InvHdr
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  **B127122,1 MHM 04/12/2005 Remove dist_ctr field from temp INVHDR file while it is now a standard field [Begin]
  *DIMENSION laFileStru[lnFileStru+13,4]
  *laFileStru[lnFileStru+1,1] = 'Dist_Ctr'
  *laFileStru[lnFileStru+1,2] = 'C'
  *laFileStru[lnFileStru+1,3] = 8
  *laFileStru[lnFileStru+1,4] = 0
  *laFileStru[lnFileStru+2,1] = 'lUpsIns'
  *laFileStru[lnFileStru+2,2] = 'L'
  *laFileStru[lnFileStru+2,3] = 1
  *laFileStru[lnFileStru+2,4] = 0
  *laFileStru[lnFileStru+3,1] = 'nSteps'
  *laFileStru[lnFileStru+3,2] = 'N'
  *laFileStru[lnFileStru+3,3] = 2
  *laFileStru[lnFileStru+3,4] = 0
  *laFileStru[lnFileStru+4,1] = 'lCompUps'
  *laFileStru[lnFileStru+4,2] = 'L'
  *laFileStru[lnFileStru+4,3] = 1
  *laFileStru[lnFileStru+4,4] = 0
  *laFileStru[lnFileStru+5,1] = 'LastLine'
  *laFileStru[lnFileStru+5,2] = 'N'
  *laFileStru[lnFileStru+5,3] = 6
  *laFileStru[lnFileStru+5,4] = 0
  *laFileStru[lnFileStru+6,1] = 'StName'
  *laFileStru[lnFileStru+6,2] = 'C'
  *laFileStru[lnFileStru+6,3] = 30
  *laFileStru[lnFileStru+6,4] = 0
  *laFileStru[lnFileStru+7,1] = 'cAddress1'
  *laFileStru[lnFileStru+7,2] = 'C'
  *laFileStru[lnFileStru+7,3] = 30
  *laFileStru[lnFileStru+7,4] = 0
  *laFileStru[lnFileStru+8,1] = 'cAddress2'
  *laFileStru[lnFileStru+8,2] = 'C'
  *laFileStru[lnFileStru+8,3] = 30
  *laFileStru[lnFileStru+8,4] = 0
  *laFileStru[lnFileStru+9,1] = 'cAddress3'
  *laFileStru[lnFileStru+9,2] = 'C'
  *laFileStru[lnFileStru+9,3] = 30
  *laFileStru[lnFileStru+9,4] = 0
  *laFileStru[lnFileStru+10,1] = 'cAddress4'
  *laFileStru[lnFileStru+10,2] = 'C'
  *laFileStru[lnFileStru+10,3] = 30
  *laFileStru[lnFileStru+10,4] = 0
  *laFileStru[lnFileStru+11,1] = 'cAddress5'
  *laFileStru[lnFileStru+11,2] = 'C'
  *laFileStru[lnFileStru+11,3] = 30
  *laFileStru[lnFileStru+11,4] = 0
  *laFileStru[lnFileStru+12,1] = 'LKEYOFF'
  *laFileStru[lnFileStru+12,2] = 'L'
  *laFileStru[lnFileStru+12,3] = 0
  *laFileStru[lnFileStru+12,4] = 0
  *laFileStru[lnFileStru+13,1] = 'NTAXDUE'
  *laFileStru[lnFileStru+13,2] = 'N'
  *laFileStru[lnFileStru+13,3] = 13
  *laFileStru[lnFileStru+13,4] = 2

  DIMENSION laFileStru[lnFileStru+12,4]

  laFileStru[lnFileStru+1,1] = 'lUpsIns'
  laFileStru[lnFileStru+1,2] = 'L'
  laFileStru[lnFileStru+1,3] = 1
  laFileStru[lnFileStru+1,4] = 0
  
  laFileStru[lnFileStru+2,1] = 'nSteps'
  laFileStru[lnFileStru+2,2] = 'N'
  laFileStru[lnFileStru+2,3] = 2
  laFileStru[lnFileStru+2,4] = 0
  
  laFileStru[lnFileStru+3,1] = 'lCompUps'
  laFileStru[lnFileStru+3,2] = 'L'
  laFileStru[lnFileStru+3,3] = 1
  laFileStru[lnFileStru+3,4] = 0
  
  laFileStru[lnFileStru+4,1] = 'LastLine'
  laFileStru[lnFileStru+4,2] = 'N'
  laFileStru[lnFileStru+4,3] = 6
  laFileStru[lnFileStru+4,4] = 0
  
  laFileStru[lnFileStru+5,1] = 'StName'
  laFileStru[lnFileStru+5,2] = 'C'
  laFileStru[lnFileStru+5,3] = 30
  laFileStru[lnFileStru+5,4] = 0
  
  laFileStru[lnFileStru+6,1] = 'cAddress1'
  laFileStru[lnFileStru+6,2] = 'C'
  laFileStru[lnFileStru+6,3] = 30
  laFileStru[lnFileStru+6,4] = 0
  
  laFileStru[lnFileStru+7,1] = 'cAddress2'
  laFileStru[lnFileStru+7,2] = 'C'
  laFileStru[lnFileStru+7,3] = 30
  laFileStru[lnFileStru+7,4] = 0
  
  laFileStru[lnFileStru+8,1] = 'cAddress3'
  laFileStru[lnFileStru+8,2] = 'C'
  laFileStru[lnFileStru+8,3] = 30
  laFileStru[lnFileStru+8,4] = 0
  
  laFileStru[lnFileStru+9,1] = 'cAddress4'
  laFileStru[lnFileStru+9,2] = 'C'
  laFileStru[lnFileStru+9,3] = 30
  laFileStru[lnFileStru+9,4] = 0
  
  laFileStru[lnFileStru+10,1] = 'cAddress5'
  laFileStru[lnFileStru+10,2] = 'C'
  laFileStru[lnFileStru+10,3] = 30
  laFileStru[lnFileStru+10,4] = 0
  
  laFileStru[lnFileStru+11,1] = 'LKEYOFF'
  laFileStru[lnFileStru+11,2] = 'L'
  laFileStru[lnFileStru+11,3] = 0
  laFileStru[lnFileStru+11,4] = 0
  
  laFileStru[lnFileStru+12,1] = 'NTAXDUE'
  laFileStru[lnFileStru+12,2] = 'N'
  laFileStru[lnFileStru+12,3] = 13
  laFileStru[lnFileStru+12,4] = 2

  *B127122,1 MHM 04/12/2005 [End]

  *--Call Create temp file.
  =gfCrtTmp(lcInvHdr,@laFileStru)

  *--Create Temprorary invoice line.
  SELECT OrdLine
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+3,4]
  laFileStru[lnFileStru+1,1] = 'LBACKORD'
  laFileStru[lnFileStru+1,2] = 'L'
  laFileStru[lnFileStru+1,3] = 0
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'nSteps'
  laFileStru[lnFileStru+2,2] = 'N'
  laFileStru[lnFileStru+2,3] = 2
  laFileStru[lnFileStru+2,4] = 0
  laFileStru[lnFileStru+3,1] = 'Consol'
  laFileStru[lnFileStru+3,2] = 'C'
  laFileStru[lnFileStru+3,3] = 1
  laFileStru[lnFileStru+3,4] = 0

  *--Call Create temp file.
  =gfCrtTmp(lcInvLine,@laFileStru,'Account+Order+Store+PikTkt+STR(LineNo,6)',lcInvLine)
  SELECT (lcInvLine)
  SET ORDER TO TAG (lcInvLine)

*****************************
*--Create Returns temp files.
ELSE
  SELECT RETHDR
  =AFIELDS(laCrMemHdr)
  lnCrMemHdr = ALEN(laCrMemHdr , 1)
  *-- Add field to know at which step the saving has done.
  DIMENSION laCrMemHdr [lnCrMemHdr+1,4]
  laCrMemHdr[lnCrMemHdr + 1 , 1] = 'NSTEPS'
  laCrMemHdr[lnCrMemHdr + 1 , 2] = 'N'
  laCrMemHdr[lnCrMemHdr + 1 , 3] = 2
  laCrMemHdr[lnCrMemHdr + 1 , 4] = 0
  *-- Call global function to create the temp. header file.
  =gfCrtTmp(lcRetHdr , @laCrMemHdr , [ACCOUNT] , lcRetHdr)

  *-- Temp. name for the lines file.
  SELECT RETLINE
  =AFIELDS(laCrMemLin)
  lnCrMemLin = ALEN(laCrMemLin , 1)
  *-- Add 8 fields for open qty. to the lines file.
  DIMENSION laCrMemLin [lnCrMemLin+11,4]
  FOR lnCount = 1 TO 8
    lcCount = ALLTRIM(STR(lnCount))
    laCrMemLin[lnCrMemLin + lnCount , 1] = 'NOPNQTY&lcCount'
    laCrMemLin[lnCrMemLin + lnCount , 2] = 'N'
    laCrMemLin[lnCrMemLin + lnCount , 3] = 6
    laCrMemLin[lnCrMemLin + lnCount , 4] = 0
  ENDFOR
  *-- Add field for total open qty. to the lines file.
  laCrMemLin[lnCrMemLin + 9 , 1] = 'NTOTOPNQTY'
  laCrMemLin[lnCrMemLin + 9 , 2] = 'N'
  laCrMemLin[lnCrMemLin + 9 , 3] = 6
  laCrMemLin[lnCrMemLin + 9 , 4] = 0
  *-- Add logical field to know if we need to add the valid lines of the 
  *-- selected invoice in the RA line file.
  laCrMemLin[lnCrMemLin + 10 , 1] = 'LINVOICE'
  laCrMemLin[lnCrMemLin + 10 , 2] = 'L'
  laCrMemLin[lnCrMemLin + 10 , 3] = 1
  laCrMemLin[lnCrMemLin + 10 , 4] = 0
  *-- Add field to know at which step the saving has done.
  laCrMemLin[lnCrMemLin + 11 , 1] = 'NSTEPS'
  laCrMemLin[lnCrMemLin + 11 , 2] = 'N'
  laCrMemLin[lnCrMemLin + 11 , 3] = 2
  laCrMemLin[lnCrMemLin + 11 , 4] = 0
  *-- Call global function to create the temp. lines file.
  =gfCrtTmp(lcRetLine , @laCrMemLin , [ACCOUNT+STYLE+CRET_LINNO+CRET_TRNCD] , lcRetLine)
ENDIF

SELECT(lnAlias)
RETURN



*:*************************************************************
*: Name      : lfUpdInvFl.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Append records in Temp Invoice header and line files.
*:*************************************************************
*: Calls     : ..............
*:*************************************************************
*: Passed Parameters  : 
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfUpdInvFl()
*:*************************************************************
FUNCTION lfUpdInvFl

*--Invoice Codes Definition.
DECLARE laCodes[5,10],laTerms[1,2],laSeasons[1,2],laDivision[1,2],;
        laShipVia[1,2],laSpcInst[1,2]
STORE '' TO laCodes,laTerms,laSeasons,laDivision,laShipVia,laSpcInst
STORE 1  TO lnTerms,lnSeason,lnDivision,lnShipVia,lnSpcInst
laCodes[1,1] = 'CTERMCODE'
laCodes[1,2] = 'laTerms'
laCodes[1,3] = 'lnTerms'
laCodes[1,4] = ''
laCodes[1,5] = .F.
laCodes[1,6] = .F.
laCodes[1,10] = 'cTermCode'
= gfwCodePop( @laCodes, "CTERMCODE" ,'D' )  
laCodes[2,1] = 'SHIPVIA'
laCodes[2,2] = 'laShipVia'
laCodes[2,3] = 'lnShipVia'
laCodes[2,4] = ''
laCodes[2,5] = .F.
laCodes[2,6] = .F.
laCodes[2,10] = 'SHIPVIA'
= gfwCodePop( @laCodes, "SHIPVIA" ,'D' )  
laCodes[3,1] = 'SPCINST'
laCodes[3,2] = 'laSpcInst'
laCodes[3,3] = 'lnSpcInst'
laCodes[3,4] = ''
laCodes[3,5] = .F.
laCodes[3,6] = .F.
laCodes[3,10] = 'SPCINST'
= gfwCodePop( @laCodes, "SPCINST" ,'D' )  
laCodes[4,1] = 'SEASON'
laCodes[4,2] = 'laSeasons'
laCodes[4,3] = 'lnSeason'
laCodes[4,4] = ''
laCodes[4,5] = .T.
laCodes[4,6] = .F.
laCodes[4,10] = 'SEASON'
= gfwCodePop( @laCodes, "SEASON" ,'D' )  
laCodes[5,1] = 'CDIVISION'
laCodes[5,2] = 'laDivision'
laCodes[5,3] = 'lnDivision'
laCodes[5,4] = ''
laCodes[5,5] = .F.
laCodes[5,6] = .F.
laCodes[5,10] = 'cDivision'
= gfwCodePop( @laCodes, "CDIVISION" ,'D' )  

*--Read codes from defaults.
lcSeason  = laSeasons[lnSeason,2]
lcDiv     = laDivision[lnDivision,2]
lcShipVia = laShipVia[lnShipVia,2]
lcSpcInst = laSpcInst[lnSpcInst,2]

lcTerms   = IIF(!llMltPay,laPayTerm[lnPayTerm,2],laTerms[lnTerms,2])
DIME laRelFld[1,2]
laRelFld[1,1] = 'NTERDISCR '
laRelFld[1,2] = 'lnTrad_Dis'
lnTrad_Dis = 0
=gfRltFld(lcTerms , @laRelFld , 'CTERMCODE')

*--Initialize Quantity and amounts variables to update the header.
STORE 0 TO lnTotInvLn,lnITotQty,lnAve_Tax,lnAveTxPr,lnDis_amt,lnDis_Per
lnLineNo = 0

*--Start update temp invoice line.
SELECT (lcPosTmp)
SET ORDER TO 1
=SEEK('1')
*ash1
lnShipAmt = 0
SCAN WHILE TranType = '1' FOR EMPTY(Tran)
  lnLineNo = lnLineNo + 1
  IF &lcPosTmp..nSteps < 6 
    SELECT (lcInvLine)
    APPEND BLANK
    REPLACE INVOICE    WITH lcInvoiceNo,;
            INVDATE    WITH &lcPosTmp..TranDate,;    
            ORDER      WITH '',;
            ACCOUNT    WITH &lcPosTmp..Account,;
            STYLE      WITH &lcPosTmp..Style,;
            SCALE      WITH &lcPosTmp..Scale,;
            GROS_PRICE WITH &lcPosTmp..Gros_Price,;
            PRICE      WITH &lcPosTmp..Price     ,;
            DISC_PCNT  WITH &lcPosTmp..Disc_pcnt ,;
            QTY1       WITH IIF(&lcPosTmp..Cnt=1,&lcPosTmp..TotQty,0),;
            QTY2       WITH IIF(&lcPosTmp..Cnt=2,&lcPosTmp..TotQty,0),;
            QTY3       WITH IIF(&lcPosTmp..Cnt=3,&lcPosTmp..TotQty,0),;
            QTY4       WITH IIF(&lcPosTmp..Cnt=4,&lcPosTmp..TotQty,0),;
            QTY5       WITH IIF(&lcPosTmp..Cnt=5,&lcPosTmp..TotQty,0),;
            QTY6       WITH IIF(&lcPosTmp..Cnt=6,&lcPosTmp..TotQty,0),;
            QTY7       WITH IIF(&lcPosTmp..Cnt=7,&lcPosTmp..TotQty,0),;
            QTY8       WITH IIF(&lcPosTmp..Cnt=8,&lcPosTmp..TotQty,0),;
            TOTQTY     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
            DESC1      WITH &lcPosTmp..Desc1,; 
            SEASON     WITH lcSeason,;
            CWARECODE  WITH lcStoreCode

    *C102047,1 ABD Add Comm Per line. [Begin]
    IF !EMPTY(&lcPosTmp..REP1) .AND. SEEK(Style,'STYLE') .AND. STYLE.Commission    
      REPLACE COMM1 WITH &lcPosTmp..Comm1
    ENDIF
    REPLACE PRICE WITH &lcPosTmp..Price + &lcPosTmp..Discount    
    *C102047,1 ABD [End]
    SELECT (lcPosTmp)
    =RLOCK() 
    REPLACE nSteps WITH 6
    UNLOCK
  ENDIF
  lnITotQty = lnITotQty + &lcInvLine..TotQty
  *ash1
  
  lnShipAmt = lnShipAmt + &lcInvLine..Price * &lcInvLine..TotQty 
  SELECT (lcPosTmp)
  *--Accomulation of the amounts for tax ,discount and total. 
  lnTotInvLn = lnTotInvLn + nAmount
  lnAve_Tax  = lnAve_Tax + Tax_amt
  lnAveTxPr  = lnAveTxPr + Tax_rate
  lnDis_amt  = lnDis_amt + Disc_amt
  lnDis_Per  = lnDis_Per + Disc_pcnt
ENDSCAN
*--Point on first invoice record.
=SEEK('1')

*--Start update temp invoice header.
SELECT (lcInvHdr)
GO TOP
IF EOF()
  APPEND BLANK
  REPLACE INVOICE    WITH lcInvoiceNo        ,; 
          INVDATE    WITH &lcPosTmp..TranDate,;    
          SHIPDATE   WITH &lcPosTmp..TranDate,;    
          DUEDATE    WITH &lcPosTmp..TranDate,;    
          ACCOUNT    WITH &lcPosTmp..Account ,;
          PHONE      WITH &lcPosTmp..Phone1  ,;
          SEASON     WITH lcSeason    ,;  
          CDIVISION  WITH lcDiv       ,;  
          cTermCode  WITH lcTerms     ,;  
          SHIPVIA    WITH lcShipVia   ,; 
          SPCINST    WITH lcSpcInst   ,;
          REP1       WITH CUSTOMER.SalesRep,;
          SHIP       WITH lnITotQty   ,;
          SHIPAMT    WITH lnShipAmt   ,;
          LastLine   WITH 0,;
          StName     WITH lcName  ,;
          cAddress1  WITH lcAddr1 ,;
          cAddress2  WITH lcAddr2 ,;
          cAddress3  WITH lcAddr3 ,;
          cAddress4  WITH lcAddr4 ,;
          cAddress5  WITH lcAddr5

*  REPLACE COD        WITH lnITotQty  ,;
          COD_AMT    WITH lnTotInvLn  ,;
          COD_FLAG   WITH 'Y'
  *C102047,1 ABD Fill the temp file with Rep1 , comm & discount .[Begin]
  *REPLACE APPRAMT    WITH lnTotInvLn  ,;
          TAX_RATE   WITH (lnAveTxPr/lnLineNo),;
          TAX_AMT    WITH lnAve_Tax    ,;
          TOTALCHG   WITH lnTotInvLn   ,;
          LINK_CODE  WITH 'DEFDEF'     ,;
          CWARECODE  WITH lcStoreCode  ,;
          DIRECT_INV WITH .T.          ,;
          DSENTDATE  WITH &lcPosTmp..TranDate,;
          CCURRCODE  WITH gcBaseCurr ,;
          NCURRUNIT  WITH 1          ,;
          NEXRATE    WITH 1          ,;
          DPOSTDATE  WITH &lcPosTmp..TranDate
  REPLACE APPRAMT    WITH lnShipAmt               ,;
          TAX_RATE   WITH (lnAveTxPr/lnLineNo)    ,;
          TAX_AMT    WITH lnAve_Tax               ,;
          LINK_CODE  WITH 'DEFDEF'                ,;
          CWARECODE  WITH lcStoreCode             ,;
          DIRECT_INV WITH .T.                     ,;
          DSENTDATE  WITH &lcPosTmp..TranDate     ,;
          CCURRCODE  WITH gcBaseCurr              ,;
          NCURRUNIT  WITH 1                       ,;
          NEXRATE    WITH 1                       ,;
          DPOSTDATE  WITH &lcPosTmp..TranDate     ,;
          Trde_disc  WITH lnTrad_Dis              ,;
          REP1       WITH &lcPosTmp..Rep1         ,;
          COMM1      WITH IIF(EMPTY(&lcPosTmp..Rep1),0,&lcPosTmp..Comm1),;
          DiscPcnt   WITH &lcPosTmp..DiscPcnt      ,;
          Discount   WITH -(lnShipAmt * &lcPosTmp..DiscPcnt/100),;
          TotalChg   WITH lnTotInvLn 
           
  *C102047,1 ABD [End]
  *C101660,1 WAB - Replace freight  in invoice header with lnfeight and also total charge
  *C101660,1 WAB - START
  IF llDispFrght
    lnAlias = SELECT()
    SELECT (lcPosTmp)
    lnRecNo = RECNO()
    SET FILTER TO !(TranType $ '4R')  
    lcOldTag = TAG()
    SET ORDER TO TAG (lcPosTmp)
    IF SEEK('F')				&& there are line for freight 
      lnFreight  = nAmount
    ELSE
      lnFreight  = 0  
    ENDIF
    SET FILTER TO
    SET ORDER TO TAG (lcOldTag)
    SELECT (lnAlias)
    REPLACE Freight    WITH lnFreight ,;
             TOTALCHG   WITH lnTotInvLn + lnFreight
  ENDIF
  *C101660,1 WAB - END


    *-- Call global function to add audit fields info.
    =gfAdd_Info(lcInvHdr)

   *--Donnot put discout in hdr.
   *DISCPCNT   WITH (lnDis_Per/lnLineNo)
   *DISCOUNT   WITH lnDis_amt           

ENDIF

*--Update temp Point of Sale transaction file with invNo.
SELECT (lcPosTmp)
SET ORDER TO
REPLACE ALL Tran    WITH lcInvoiceNo,;
            Balance WITH lnTender FOR TranType = '1'

*B603091,1 Added.
FLUSH

RETURN (lnTotInvLn)
*:*************************************************************
*: Name      : lfUpdRetFl.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Append records in Temp return header and line files.
*:*************************************************************
*: Calls     : ..............
*:*************************************************************
*: Passed Parameters  : 
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfUpdRetFl()
*:*************************************************************
FUNCTION lfUpdRetFl

*--Return Codes Definition.
DECLARE laCodes[3,10],laReasons[1,2],laTerms[1,2],laDivision[1,2]
STORE '' TO laCodes,laReasons,laTerms,laDivision
STORE 1  TO lnReasons,lnTerms,lnDivision
laCodes[1,1] = 'REASON'
laCodes[1,2] = 'laReasons'
laCodes[1,3] = 'lnReasons'
laCodes[1,4] = ''
laCodes[1,5] = .F.
laCodes[1,6] = .F.
laCodes[1,10] = 'REASON'
= gfwCodePop( @laCodes, "REASON" ,'D' )  
laCodes[2,1] = 'CTERMCODE'
laCodes[2,2] = 'laTerms'
laCodes[2,3] = 'lnTerms'
laCodes[2,4] = ''
laCodes[2,5] = .F.
laCodes[2,6] = .F.
laCodes[2,10] = 'cTermCode'
= gfwCodePop( @laCodes, "CTERMCODE" ,'D' )  
laCodes[3,1] = 'CDIVISION'
laCodes[3,2] = 'laDivision'
laCodes[3,3] = 'lnDivision'
laCodes[3,4] = ''
laCodes[3,5] = .F.
laCodes[3,6] = .F.
laCodes[3,10] = 'cDivision'
= gfwCodePop( @laCodes, "CDIVISION" ,'D' )  

*--Read codes from defaults.
lcReason = laReasons[lnReasons,2]
lcDiv    = laDivision[lnDivision,2]
lcTerms  = IIF(!llMltPay,laPayTerm[lnPayTerm,2],laTerms[lnTerms,2])


*--Initialize Quantity and amounts variables to update the header.
STORE 0 TO lnTotRetLn,lnRTotQty,lnAve_Tax,lnGrs_amt,lnDis_amt
lnCommRep = 0
lnLineNo  = 0
*C102047,1 ABD - Define New Variable. [Begin]
lnCommamnt = 0
*C102047,1 ABD [End]

*--Start update temp return line.
SELECT (lcPosTmp)
SET ORDER TO 1
=SEEK('0')
SCAN WHILE TranType = '0'
  lnLineNo = lnLineNo + 1
  *B603733,1 WMA Seek in Stydye file insted of Style file [Begin]
  *=SEEK(&lcPosTmp..Style,'STYLE')
  = SEEK(&lcPosTmp..Style+lcStoreCode,'STYDYE')
  *B603733,1 WMA [End]
  IF &lcPosTmp..nSteps < 6 
    SELECT (lcRetLine)
    APPEND BLANK
    REPLACE CRMEMO     WITH lcCrMemoNo,;
            ACCOUNT    WITH &lcPosTmp..Account,;
            CRDATE     WITH &lcPosTmp..TranDate,;
            STYLE      WITH &lcPosTmp..Style,;
            CSTYGRADE  WITH STYLE.cStyGrade,;
            CRET_LINNO WITH ALLTRIM(STR(lnLineNo,4)),;
            CRET_TRNCD WITH '2',;
            INVOICE    WITH lcInvNo,;
            REASON     WITH lcReason,;
            GROS_PRICE WITH ABS(&lcPosTmp..Gros_Price)-ABS(&lcPosTmp..DISC_AMT),;
            PRICE      WITH ABS(&lcPosTmp..Price),;
            DISC_PCNT  WITH &lcPosTmp..DISCPCNT
    *B603733,1 WMA Get Ave. cost from Stydye file insted of Style file [Begin]
    *REPLACE QTY1       WITH IIF(&lcPosTmp..Cnt=1,ABS(&lcPosTmp..TotQty),0),;
            QTY2       WITH IIF(&lcPosTmp..Cnt=2,ABS(&lcPosTmp..TotQty),0),;
            QTY3       WITH IIF(&lcPosTmp..Cnt=3,ABS(&lcPosTmp..TotQty),0),;
            QTY4       WITH IIF(&lcPosTmp..Cnt=4,ABS(&lcPosTmp..TotQty),0),;
            QTY5       WITH IIF(&lcPosTmp..Cnt=5,ABS(&lcPosTmp..TotQty),0),;
            QTY6       WITH IIF(&lcPosTmp..Cnt=6,ABS(&lcPosTmp..TotQty),0),;
            QTY7       WITH IIF(&lcPosTmp..Cnt=7,ABS(&lcPosTmp..TotQty),0),;
            QTY8       WITH IIF(&lcPosTmp..Cnt=8,ABS(&lcPosTmp..TotQty),0),;
            TOTQTY     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
            AMOUNT     WITH ABS(&lcPosTmp..nAmount),;
            DISC_AMT   WITH ABS(&lcPosTmp..Disc_Amt),;
            TAX_RATE   WITH &lcPosTmp..Tax_Rate,;
            COST       WITH STYLE.Ave_Cost,;
            GL_SALES   WITH 'DEFDEF',;
            GL_COST    WITH 'DEFDEF'
    REPLACE QTY1       WITH IIF(&lcPosTmp..Cnt=1,ABS(&lcPosTmp..TotQty),0),;
            QTY2       WITH IIF(&lcPosTmp..Cnt=2,ABS(&lcPosTmp..TotQty),0),;
            QTY3       WITH IIF(&lcPosTmp..Cnt=3,ABS(&lcPosTmp..TotQty),0),;
            QTY4       WITH IIF(&lcPosTmp..Cnt=4,ABS(&lcPosTmp..TotQty),0),;
            QTY5       WITH IIF(&lcPosTmp..Cnt=5,ABS(&lcPosTmp..TotQty),0),;
            QTY6       WITH IIF(&lcPosTmp..Cnt=6,ABS(&lcPosTmp..TotQty),0),;
            QTY7       WITH IIF(&lcPosTmp..Cnt=7,ABS(&lcPosTmp..TotQty),0),;
            QTY8       WITH IIF(&lcPosTmp..Cnt=8,ABS(&lcPosTmp..TotQty),0),;
            TOTQTY     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
            AMOUNT     WITH ABS(&lcPosTmp..nAmount),;
            DISC_AMT   WITH ABS(&lcPosTmp..DISCOUNT),;
            TAX_RATE   WITH &lcPosTmp..Tax_Rate,;
            COST       WITH STYDYE.Ave_Cost,;
            GL_SALES   WITH 'DEFDEF',;
            GL_COST    WITH 'DEFDEF'
    *B603733,1 WMA [End]
    *-- Call global function to add audit fields info.
    =gfAdd_Info(lcRetLine)

    SELECT (lcPosTmp)
    =RLOCK() 
    REPLACE nSteps WITH 6
    UNLOCK
  ENDIF
  lnRTotQty = lnRTotQty + ABS(&lcRetLine..TotQty)

  SELECT (lcPosTmp)
  *C102047,1 ABD Remark the next line and get the accut. comm. [Begin]
  lnCommRep = Comm1
  lnCommamnt = lnCommamnt + ((ABS(nAmount) * Comm1/100 )* TotQty)
  *C102047,1 ABD [End]  
  *--Accomulation of the amounts for tax ,discount and gross total. 
  lnTotRetLn = lnTotRetLn + ABS(nAmount)
  lnAve_Tax  = lnAve_Tax  + ABS(Tax_amt)
  lnDis_amt  = lnDis_amt  + ABS(Disc_amt)
  lnGrs_amt  = lnGrs_amt  + ABS( Gros_Price * TotQty )
ENDSCAN

*C102047,1 ABD Get the comm. % from the Invoice Header. [Begin]
IF lnSr=2 .AND. !EMPTY(lcInvNo) .AND. lnCommamnt <> 0
  =SEEK(lcInvNo,'INVHDR')
  lnCommRep= INVHDR.Comm1
ENDIF
*C102047,1 ABD [End]

*--Point on first return record.
=SEEK('0')

*--Start update temp return header.
SELECT (lcRetHdr)
GO TOP
IF EOF()
  APPEND BLANK
  *C102047,1 ABD Add New Comm % And New Comm. Amount. [Begin]  
  *REPLACE CRMEMO     WITH lcCrMemoNo,;
          ACCOUNT    WITH &lcPosTmp..Account,;
          CRDATE     WITH &lcPosTmp..TranDate,;
          DPOSTDATE  WITH &lcPosTmp..TranDate,;
          INVOICE    WITH lcInvNo,;
          REASON     WITH lcReason,;
          CDIVISION  WITH lcDiv,;
          cTermCode  WITH lcTerms,;
          SALESREP1  WITH lcSRep,;
          COMMPCNT1  WITH lnCommRep,;
          COMMAMT1   WITH (lnTotRetLn*lnCommRep/100),;
          PIECES     WITH lnRTotQty ,;
          GROSS_AMT  WITH lnGrs_amt ,;
          DISC_AMT   WITH lnDis_amt ,;
          TAX_AMT    WITH lnAve_Tax ,;
          AMOUNT     WITH lnTotRetLn ,;
          TOTCREDIT  WITH lnTotRetLn ,;
          LINK_CODE  WITH 'DEFDEF'   ,;
          CWARECODE  WITH lcStoreCode,;
          CCURRCODE  WITH gcBaseCurr ,;
          NCURRUNIT  WITH 1,;
          NEXRATE    WITH 1
  *B128282,1 NNA 07/17/2005 (Begin) Get Link code from Invhdr file instead of Hard coded As "DEFDEF"
  *REPLACE CRMEMO     WITH lcCrMemoNo         ,;
           ACCOUNT    WITH &lcPosTmp..Account ,;
           CRDATE     WITH &lcPosTmp..TranDate,;
           DPOSTDATE  WITH &lcPosTmp..TranDate,;
           INVOICE    WITH lcInvNo            ,;
           REASON     WITH lcReason           ,;
           CDIVISION  WITH lcDiv              ,;
           cTermCode  WITH lcTerms            ,;
           SALESREP1  WITH lcSRep             ,;
           COMMPCNT1  WITH lnCommRep          ,;
           COMMAMT1   WITH lnCommamnt         ,;
           PIECES     WITH lnRTotQty          ,;
           GROSS_AMT  WITH lnGrs_amt          ,;
           DISC_AMT   WITH lnDis_amt          ,;
           TAX_AMT    WITH lnAve_Tax          ,;
           AMOUNT     WITH lnTotRetLn         ,;
           TOTCREDIT  WITH lnTotRetLn         ,;
           LINK_CODE  WITH 'DEFDEF'           ,;
           CWARECODE  WITH lcStoreCode        ,;
           CCURRCODE  WITH gcBaseCurr         ,;
           NCURRUNIT  WITH 1                  ,;
           NEXRATE    WITH 1

  REPLACE CRMEMO     WITH lcCrMemoNo         ,;
          ACCOUNT    WITH &lcPosTmp..Account ,;
          CRDATE     WITH &lcPosTmp..TranDate,;
          DPOSTDATE  WITH &lcPosTmp..TranDate,;
          INVOICE    WITH lcInvNo            ,;
          REASON     WITH lcReason           ,;
          CDIVISION  WITH lcDiv              ,;
          cTermCode  WITH lcTerms            ,;
          SALESREP1  WITH lcSRep             ,;
          COMMPCNT1  WITH lnCommRep          ,;
          COMMAMT1   WITH lnCommamnt         ,;
          PIECES     WITH lnRTotQty          ,;
          GROSS_AMT  WITH lnGrs_amt          ,;
          DISC_AMT   WITH lnDis_amt          ,;
          TAX_AMT    WITH lnAve_Tax          ,;
          AMOUNT     WITH lnTotRetLn         ,;
          TOTCREDIT  WITH lnTotRetLn         ,;
          LINK_CODE  WITH IIF(!EMPTY(lcInvNo) AND SEEK(lcInvNo,'INVHDR'),INVHDR.LINK_CODE,;
                          IIF(SEEK('M'+&lcPosTmp..Account,'CUSTOMER'),CUSTOMER.LINK_CODE,'DEFDEF')),;
          CWARECODE  WITH lcStoreCode        ,;
          CCURRCODE  WITH gcBaseCurr         ,;
          NCURRUNIT  WITH 1                  ,;
          NEXRATE    WITH 1
  *B128282,1 NNA (End)
  *C102047,1 ABD [End]

  *C101660,1 WAB - replace field other (rethdr table ) with lnfreight and also total credit
  *C101660,1 WAB - and in case of exchange delete the record with type 'F'
  *C101660,1 WAB - START
  IF llDispFrght
    lnAlias = SELECT()
    SELECT (lcPosTmp)
    lnRecNo = RECNO()
    SET FILTER TO !(TranType $ '4F')  
    lcOldTag = TAG()
    SET ORDER TO TAG (lcPosTmp)
    IF SEEK('R')				&& there are line for freight 
      lnRtFrght  = nAmount
    ELSE
      lnRtFrght  = 0  
    ENDIF
    SET FILTER TO
    SET ORDER TO TAG (lcOldTag)
    SELECT (lnAlias)
    REPLACE OTHER      WITH ABS(lnRtFrght) ,;
            TOTCREDIT  WITH lnTotRetLn + ABS(lnRtFrght)
  ENDIF
  *C101660,1 WAB - END
  *C102047,1 ABD add the order no. in case return from invoice. [Begin]
  IF lnSr=2 .AND. !EMPTY(lcInvNo)
    = SEEK (lcInvNo,'INVHDR')
    REPLACE ORDER WITH INVHDR.Order
  ENDIF
  *C102047,1 ABD [End]  
    *-- Call global function to add audit fields info.
    =gfAdd_Info(lcRetHdr)

ENDIF

*--Update temp Point of Sale transaction file with CrMemoNo.
SELECT (lcPosTmp)
SET ORDER TO
REPLACE ALL Tran    WITH lcCrMemoNo,;
            Balance WITH lnTender FOR TranType = '0'
*B603091,1 Added.
FLUSH

RETURN
******

*:*************************************************************
*: Name      : lfPrtTtl.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Print Transaction title. Form 'A' and 'TZ'
*:*************************************************************
*: Calls     : ..............
*:*************************************************************
*: Passed Parameters  : 
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfPrtTtl()
*:*************************************************************
FUNCTION lfPrtTtl

lcRet=''
*--Added freight record.
*IF !(TranType $ '01')
IF !(TranType $ '01FR')
  DO CASE
    CASE lnTtlNo=1
      lcRet  = lcTtl1
    CASE lnTtlNo=2
      lcRet  = lcTtl2
    CASE lnTtlNo=3      
      lcRet  = lcTtl3
    CASE lnTtlNo=4      
      lcRet  = lcTtl4
  ENDCASE
  lnTtlNo =lnTtlNo + 1
ENDIF
RETURN (lcRet)


*:*************************************************************
*: Name      : lfPrntLn
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Print recept details (discout and taxes).
*:*************************************************************
FUNCTION lfPrntLn
lcRetStrng = ''
IF TranType$lcBzTp AND (Disc_pcnt <> 0 OR Tax_Rate <> 0 )
  lcRetStrng = ' Gross Price:'+ALLTRIM(STR(Gros_Price,7,2))
  DO CASE
    CASE Disc_pcnt <> 0 AND Tax_Rate <> 0
      lcRetStrng = lcRetStrng + ' Discount:'+STR(Disc_pcnt,5,2)+'%   Tax Rate :'+STR(TAX_RATE,5,2)+'%'
    CASE Disc_pcnt <> 0 AND Tax_Rate = 0
      lcRetStrng = lcRetStrng + ' Discount:'+STR(Disc_pcnt,5,2)+'%  Dis.Amount:'+ALLTRIM(STR(DISC_AMT,10,2))
    CASE Disc_pcnt =  0 AND Tax_Rate <> 0
      lcRetStrng = lcRetStrng + ' Tax Rate:'+STR(TAX_RATE ,5,2)+'%  Tax Amount:'+ALLTRIM(STR(TAX_AMT,13,2))
  ENDCASE
ENDIF
RETURN lcRetStrng


*:*************************************************************
*: Name      : lfPrntSPe.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Print Transaction title (Form 'CZ').
*:*************************************************************
*: Calls     : ..............
*:*************************************************************
*: Passed Parameters  :	
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfPrntSPe()
*:*************************************************************
FUNCTION lfPrntSPe   

SUM ALL Tax_amt,(TotQty * gros_Price) ;
     TO lnTotTax,lnTotGros
lnTotDisc = lnTotGros - (lnTotal - lnTotTax)
lnTDsPerc = ((lnTotDisc/lnTotGros)*100)

lnArCol = 1
IF lnTotDisc <> 0
  laPrntSPe[lnArCol] = 'TOTAL - DISCOUNT'+SPACE(17)+STR(lnTDsPerc,6,2)+'%'+SPACE(9)+STR(-lnTotDisc,11,2)
  lnArCol = lnArCol + 1
ENDIF 
IF lnTotTax <> 0
  laPrntSPe[lnArCol] = 'TAX'+SPACE(46)+STR(lnTotTax,11,2)
  lnArCol = lnArCol + 1
ENDIF 
laPrntSPe[lnArCol] = ' '+SPACE(50)+'---------'
lnArCol = lnArCol + 1
laPrntSPe[lnArCol] = 'A M O U N T     :'+SPACE(32)+STR(lnTotal,11,2)
lnArCol = lnArCol + 1
laPrntSPe[lnArCol] = 'P A Y M E M T(S):'+PADR(lcRefer,30)+SPACE(2)+STR(lnPaid,11,2)
lnArCol = lnArCol + 1
laPrntSPe[lnArCol] = ' '
lnArCol = lnArCol + 1
laPrntSPe[lnArCol] = '               Thanks for coming'
RETURN

   
*:*************************************************************
*: Name      : lfvSafeId.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Validate Safe Id only if the user use more than safe.
*:*************************************************************
*: Calls     : ..............
*:*************************************************************
*: Passed Parameters  : 
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvSafeId()
*:*************************************************************
FUNCTION lfvSafeId

*--Confirm selection?  The current cash register ID is : XXXX. Y/N
IF gfModalGen('QRM52004B42002','DIALOG',laSafes[lnSafes]) = 1
  *C102047,1 ABD check if the safe didn't close. [Begin]
  *-- Cheak if user close the save , and want to make new sale in last time.
  lnAlias = SELECT (0)
  llTrimmed  = .F.
  =gfOpenFile(gcDataDir+'APCHECKS','Bankcheck','SH')
  =SEEK(laSafes[lnSafes])
  IF !EMPTY(dclosdate)
    IF gdSysDate < dclosdate
      llTrimmed = .T.
    ELSE
      IF gdSysDate = dclosdate .AND. TIME()  < cclos_time
        llTrimmed = .T.
      ENDIF
    ENDIF
  ENDIF
  =gfCloseFile('APCHECKS') 
  SELECT(lnAlias)
  IF !llTrimmed 
    lcCurrSafe = laSafes[lnSafes]
    llSelSafe = .F.
    SHOW GETS
    _CUROBJ=OBJNUM(lnSR)
    *C102047,1 ABD [End]
  ELSE
    *-- Message No.  : 52019
    *-- Message TEXT : You are trying to generate a transaction on an incorrect date . Check your system date.
    *-- Button  No.  : 00000.
    *-- Button  TEXT : \<Resume -- \<Quit
    IF gfModalGen('TRM52019B00008','DIALOG') = 1
      _CUROBJ=_CUROBJ
      KEYBOARD '{SPACEBAR}'
    ELSE
      CLEAR READ 
    ENDIF
  ENDIF  
ENDIF
RETURN

*:*************************************************************
*: Name      : lfUpdApPay.
*: Developer : Timour A. K.
*: Date      : 02/02/98
*: Purpose   : Create payment record in APPAYMENT file.
*:*************************************************************
*: Calls     : ..............
*:*************************************************************
*: Passed Parameters  : 
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfUpdApPay()
*:*************************************************************
FUNCTION lfUpdApPay

SELECT ApPaymnt
APPEND BLANK
=RLOCK()
REPLACE cPayType  WITH "A"               ,;
        cPayMeth  WITH "A"               ,;
        cBnkCode  WITH lcCurrSafe        ,;
        cChkAcct  WITH 'CASH REGISTR'    ,;
        dPayDate  WITH ldPostDate        ,;
        cFisFYear WITH lcGLFYear         ,;
        cFspprdid WITH lcGLPeriod        ,;
        cPayClNo  WITH &lcPosTmp..Account,;
        cPayComp  WITH &lcPosTmp..StName ,;
        nPayAmnt  WITH IIF(lnPaid < 0,1,-1)*ABS(&lcPosTmp..nAmount) ,;
        cCurrCode WITH lcCurrCode,;
        nExRate   WITH lnExRate  ,;
        nCurrUnit WITH lnCurrUnit,;
        cPayRecSt WITH "O"


*-- Call global function to add audit fields info.
=gfAdd_Info("APPAYMNT")
UNLOCK
RETURN



*:*************************************************************
*: Name      : lfUpcBrow. *E500241,1 
*: Developer : Ahmed Salah Shalaby - (SSH).
*: Date      : 03/05/99
*: Purpose   : Function to browse UPC number.
*:*************************************************************
*: Calls     : ..............
*:*************************************************************
*: Passed Parameters  : 
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfUpcBrow()
*:*************************************************************
FUNCTION lfUpcBrow

lcPbrowtitl = 'Style UPC Number'
lcbrfields = "cTemp = cUPCNUM1+cUPCNUM2+cUPCNUM3 :15:H='UPC',"+;
             "STYLE :20:H='Style',"+;
             "Size = IIF(SEEK(STYLE,'STYLE'),IIF(SEEK('S'+STYLE.SCALE,'SCALE'),EVAL('Scale.Sz'+LEFT(Size,1)),''),'')"

DIMENSION latemp[2]
latemp = ''
llReturn=ariabrow([FOR !lUpcPack],lcPbrowtitl, gnbrfsrow1, gnbrfscol1,;
                  gnbrfsrow2, gnbrfscol2,'','',;
                  'STYLE,SIZE','laTemp')
lcStyle = latemp[1]+latemp[2]

*:*************************************************************
*: Name      : lfVPop.  *B802284,1
*: Developer : Ahmed Salah Shalaby - (SSH).
*: Date      : 03/05/99
*: Purpose   : Function Valid popup.
*:*************************************************************
*: Calls     : ..............
*:*************************************************************
*: Passed Parameters  : 
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfVPop()
*:*************************************************************
FUNCTION lfVPop

llUpc = (lcUpcPop ='Upc Number' )
lcStyle = ''
=lfRefresh()
SHOW GET lcstyle
_CUROBJ = OBJNUM(lcStyle)


*:*************************************************************
*: Name      : lfStyCond.  *B802284,1
*: Developer : Ahmed Salah Shalaby - (SSH).
*: Date      : 03/05/99
*: Purpose   : Style When Function.
*:*************************************************************
*: Calls     : ..............
*:*************************************************************
*: Passed Parameters  : 
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfStyCond()
*:*************************************************************
PROCEDURE lfStyCond

lcCnfrm = IIF(llConfirm,'ON','OFF')
SET CONFIRM &lcCnfrm


*!*********************************************************
*! Name      : lfUpdLnNo
*! Developer : TAK
*! Date      : 06/20/1999
*! Purpose   : Increment line no.
*!*********************************************************
FUNCTION lfUpdLnNo

IF LineNo=0
  lnPOSLine = lnPOSLine + 1
  RETURN lnPOSLine
ELSE
  RETURN LineNo
ENDIF


*!*********************************************************
*! Name      : lfAldyRet
*! Developer : TAK
*! Date      : 06/20/1999
*! Purpose   : Check if this style already returnd from this invoice.
*!*********************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*********************************************************
*! Passed Parameters  : lcRStyle,lcRInv
*!*********************************************************
*! Returns            : .T. or .F.
*!*********************************************************
*! Example   : =lfAldyRet()
*!*********************************************************
FUNCTION lfAldyRet
PARA lcRStyle,lcRInv
llAldyRet = .F.
RETURN llAldyRet

*!*********************************************************
*! Name      : lfCntEnd
*! Developer : TAK
*! Date      : 06/09/1999
*! Purpose   : Trapping hot key control.
*!*********************************************************
FUNCTION lfCntEnd
PARA lcLblPara

IF laScrMode[4]
  IF lcLblPara='BROW'
    ACTIVATE WINDOW (lcBrowTitl)
  ELSE
    DO CASE
      CASE lcLblPara='HOME'
        GO TOP IN  (lcPosTmp)  
      CASE lcLblPara='END'
        GO BOTT IN (lcPosTmp)
    ENDCASE
    IF !EOF(lcPosTmp)
      =lfActBrow()
    ENDIF
  ENDIF
ENDIF
RETURN

*!*********************************************************
*! Name      : lfHgWUpdat
*! Developer : Sameh (SSE)
*! Date      : 06/09/1999
*! Purpose   : Update nHgWtrMark field (Customer) with NETBAL, if NETBAL is greater
*!*********************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*********************************************************
*! Passed Parameters  : None
*!*********************************************************
*! Returns            : None
*!*********************************************************
*! Example   : =lfHgWUpdat()
*!*********************************************************
*E301245,1
FUNCTION lfHgWUpdat
=RLOCK()
REPLACE nHgWtrMark WITH IIF(NETBAL>nHgWtrMark,NETBAL,nHgWtrMark)
UNLOCK
*-- End of lfHgWUpdat.


*!*********************************************************
*! Name      : lfSelSize                         *B802525,1
*! Developer : TAK
*! Date      : 08/29/1999
*! Purpose   : select style size if style was selected from browse.
*!*********************************************************
FUNCTION lfSelSize

*B802698,1 Start.
*DEFINE WINDOW lcstysize ;
		AT 20.300, 3.000 ;
		SIZE 2.800,38.200 ;
		FONT "MS Sans Serif", 8 ;
		NOFLOAT NOCLOSE NOMINIMIZE NONE  COLOR RGB(,,,192,192,192)

DEFINE WINDOW lcstysize AT  0.000, 0.000 SIZE 4.923,41.000 ;
		TITLE "Select Size" FONT "MS Sans Serif", 8 ;
		FLOAT CLOSE NOMINIMIZE SYSTEM COLOR RGB(,,,192,192,192)
MOVE WINDOW lcstysize CENTER
*B802698,1 End.

ACTIVATE WINDOW lcstysize

*B802698,1 Start.
*@ 1.100,2.000 SAY "Select Size  :" FONT "MS Sans Serif", 8 STYLE "BT"
*@ 0.700,18.400 GET lnStySizes PICTURE "@^" 	FROM laStySizes ;
	SIZE 1.538,12.000 ;
	DEFAULT 1 ;
	FONT "MS Sans Serif", 8 STYLE "B" ;
	VALID LFVSELSIZE()

@ 1.615,15.800 GET lnStySizes ;
	PICTURE "@^" ;
	FROM laStySizes ;
	SIZE 1.538,11.833 ;
	DEFAULT 1 ;
	FONT "MS Sans Serif", 8 ;
	 STYLE "B" ;
	VALID LFVSELSIZE()
@ 1.923,7.400 SAY "Size :"  ;
	FONT "MS Sans Serif", 8 ;
	 STYLE "BT"
@ 0.385,1.200 TO 4.154,38.800 PEN 1, 8
@ 0.462,1.400 TO 4.231,39.000 PEN 1, 8 COLOR RGB(255,255,255,,,,)
*B802698,1 End.

READ CYCLE MODAL
RELEASE WINDOW lcstysize

*******************
FUNCTION LFVSELSIZE     &&  lnStySizes VALID
CLEAR READ


*C101660,1 WAB - START
*!**************************************************************************
*! Func. Name: lfvFreight()
*! Developer : WAB - Walid A. Wahab
*! Date      : 09/16/99
*! Purpose   : Function to CALL FREIGHT screen and add freight line to the file
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Parameters: NONE
*!**************************************************************************
*! Returns   : None.
*!**************************************************************************
*! Example   : =lfvFreight()
*!**************************************************************************
FUNCTION lfvFreight
Private lnalias,lnRecNo,llAddFrght,lnOldFrght,llCancel
lnAlias = SELECT()
SELECT (lcPosTmp)
lnRecNo = RECNO()
SET FILTER TO 
llAddFrght = .F.		&& there is no freight 
llCancel   = .F.		&& cancel all change
lnOldFrght = 0			&& old freight value
lcOldTag = TAG()
SET ORDER TO TAG (lcPosTmp)
IF SEEK(IIF(lnSr = 1,'F','R')) 		&& there are line for freight 
  lnFreight  = nAmount
  lnOldFrght = lnFreight
  llAddFrght = .T.
ELSE
  lnFreight  = 0  
ENDIF
DO (gcScrDir+gcWinAppl+"\PSFrght.SPX")
IF !llCancel			&& add or change the freight amount
  IF !llAddFrght		&& add
  APPEND BLANK
  REPLACE TranType  WITH IIF(lnSr=1,'F','R'),;
          TranDate  WITH ldPostDate,;
          Account   WITH lcCustomer,;
          StName    WITH lcName,;
          Phone1    WITH lcPhone,;
          Reference WITH IIF(lnSr=1,'','Return ')+'Freight',;
          Desc1     WITH 'Freight',;
          dPostDate WITH ldPostDate
  ENDIF
  *---change or add freight amount
  REPLACE nAmount   WITH lnFreight
  IF lnSr=2
    lnRtFrght = lnFreight
  ENDIF
  lnTotal = lnTotal+nAmount-lnOldFrght
  IF lnSr=1
    lnPaid  = lnTotal
    lnPaid  = IIF(lnPaid<0,0,lnPaid)
    
    *B803962,1 ABD Add new setup to ask the user to display 
    *B803962,1 ABD the amount tender or not. [Begin]
    IF llDsAmntNc
      lnTender = lnPaid
    ENDIF
    *B803962,1 ABD [End]
    
  ENDIF
  
  *B604180,1 ABD -Disable filed payment amount in case do not leave 
  *B604180,1 ABD -Opend amount setups. [Begin]
  *B606415,1 SSH Fix problems in Paid Amount
  
  *IF laSetups[17,2]
  IF (lnSr = 1 .AND. laSetups[17,2]) .OR. (lnSr = 2 .AND. llALLRTAMT)
  *B606415,1 SSH Fix problems in Paid Amount
    SHOW GET lnPaid  DISABLE
  ELSE
    SHOW GET lnPaid ENABLE
  ENDIF
  *B604180,1 ABD - [End]

  *B803962,1 ABD Add new setup to ask the user to display 
  *B803962,1 ABD the amount tender or not. [Begin]
  IF llDsAmntNc
    SHOW GET lnTender
  ENDIF
  *B803962,1 ABD [End]
  
ELSE
  IF lnSr=1
    lnFreight = lnOldFrght  
   ELSE
    lnRtFrght = lnOldFrght
  ENDIF
ENDIF
SET FILTER TO !(TranType $ '4FR')
SET ORDER TO TAG (lcOldTag)
IF BETWEEN(lnRecNO,1,RECCOUNT())
  GO lnREcNO
ENDIF

*B803962,1 ABD - Add New setup to return the sale and display the total amount
*B803962,1 ABD - In the Paid amount or set it as it work. [Begin]
IF lnSr=2 .AND. llALLRTAMT
*B604522,1 MAN This new setup 'The amount tender display' should not affect the paid amount
*  lnPaid = lnTotal
ENDIF
*B803962,1 ABD - [End]

SELECT (lnAlias)
=lfRefresh()
_CUROBJ=OBJNUM(lcStyle)
*C101660,1 WAB - END

*C101660,1 WAB - START
*!**************************************************************************
*! Func. Name: lfVlnFrght()
*! Developer : WAB - Walid A. Wahab
*! Date      : 09/16/99
*! Purpose   : Function to validate freight amount 
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Parameters: NONE
*!**************************************************************************
*! Returns   : None.
*!**************************************************************************
*! Example   : =lfVlnFrght()
*!**************************************************************************
FUNCTION lfVlnFrght
DO CASE 
 CASE lnSr=1 .AND. lnFreight < 0		&& invoice
   WAIT WINDOW 'Freight can not be minus' NOWAIT
 CASE lnSr=2 .AND. lnFreight > 0		&& return
   WAIT WINDOW 'Freight must be less than 0 ' NOWAIT
 OTHERWISE 
  CLEAR READ
ENDCASE
*C101660,1 WAB - END
*!*************************************************************
*! Name      : lfOldValue
*! Developer : Abdou ElGendi
*! Date      : 02/17/2000
*! Purpose   : Function to store old value of the current filed.
*! Reference : B603687,1
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*B603687,1
FUNCTION lfWOldvale
lcOldValue = EVALUATE('m.' + SYS(18))

*-- End Of lfoldvalue
*!*************************************************************
*! Name      : lfInvlBrow
*! Developer : Abdou ElGendi
*! Date      : 02/17/2000
*! Purpose   : Function to store old value of the current filed.
*! Reference : B603687,1
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*B603687,1
FUNCTION lfInvlBrow

lnAlias = SELECT()
SELECT INVLINE
GO TOP
SET RELATION TO STYLE INTO STYLE

lcSveFilt = FILTER()
SET FILTER TO Invoice = lcInvNo
lcSaveBrow  = lcBrFields
lcOld_ttl   = lcFile_ttl
*-- Set the browse fields.
lcBrFields = [Invline.invoice:7:H="Invoice No.",Invline.invdate:12:H="Invoice Date",Style.Style:19:H=lcStyHdr,]+;
             [Style.DESC:13:H="Description",]+;
             [Style.Season:7:H="Se",Style.cdivision:7:H="Division",Style.pricea]+;
             [:6:h="Price",Style.totWip:7:h="WIP",Style.totstk:7:h="Stock",]+;
             [Style.totord:7:h="Orders",OTS=(Style.TOTWIP+Style.TOTSTK-Style.TOTORD):7:H="O.T.S.",]+;
             [Style.Fabric:9:h="Fabric"]
DECLARE laTemp[1]
latemp   = ''
lcFile_ttl  = lcStyHdr
*-- call the global browse to browse the available name drops for the current account+store.
= ariabrow('',lcFile_ttl, gnBrFsRow1, gnBrFsCol1,;
              gnBrFsRow2, gnBrFsCol2,'','',;
              'STYLE','laTemp')
lcIStyle = latemp
lcBrFields = lcSaveBrow
lcFile_ttl = lcOld_ttl 
SELECT INVLINE
SET FILTER TO &lcSveFilt
SET RELATION TO
SELECT(lnAlias)

*-- END OF lfInvlBrow.



*!*************************************************************
*! Name      : lfvCrCard
*! Developer : TAK
*! Date      : 09/17/2000
*! Purpose   : Function to validate a credit card if PS was setup 
*!             integrated with Atomic.
*! Reference : E301467,1
*!*************************************************************
FUNCTION lfvCrCard
PARA lcTmCode,lcTmRefer,lnTmAmount

***=lfvMPatRef()

*--For non cash payment.
DIME laRelFld[1,2]
laRelFld = ''

*B604180,1 ABD - Change related filed llCash from 2 cases (Cash or not cash) to 
*B604180,1 ABD - be cash , not cash or Credit card that will give the 
*B604180,1 ABD - system to different Between them. [Begin]
*laRelFld[1,1] = 'LLCASH'
*laRelFld[1,2] = 'm.LLCASH'
*m.llCash = .F.
laRelFld[1,1] = 'CASH_NON'
laRelFld[1,2] = 'lcCash_Non'
STORE '' To lcCash_Non 
*B604180,1 ABD - [End]

=gfRltFld(lcTmCode , @laRelFld , 'CTERMCODE')

llAuthzd = .T.

*B604180,1 ABD - Change related filed llCash from 2 cases (Cash or not cash) to 
*B604180,1 ABD - be cash , not cash or Credit card that will give the 
*B604180,1 ABD - system to different Between them. [Begin]
*IF ! m.llCash
IF ALLTRIM(lcCash_Non) = 'D'
  *B604180,1 ABD - [End]

  IF EMPTY(lcTmRefer) 
    =gfModalGen('INM52018B42000','DIALOG')
    _CUROBJ = OBJNUM(lcRefer)
    llAuthzd = .F.
  ELSE
    llAuthzd = lfvAutorize()
  ENDIF
ENDIF
RETURN llAuthzd



*!*************************************************************
*! Name      : lfvAutorize
*! Developer : TAK
*! Date      : 09/17/2000
*! Purpose   : Do an Authorization when integrated with Atomic.
*! Reference : E301467,1
*!*************************************************************
FUNCTION lfvAutorize

WAIT WINDOW 'Processing....' NOWAIT

lcAuthSen = SUBSTR(lcSession,3,4)
lcAuthFil = "AUTH"+lcAuthSen

lcStorPath = FULLPATH('')
*--Load Authorizer
SET DEFA TO (lcAtomDir)
RUN /N Atom32.exe


*--Erase OUT file if exist,
IF FILE(lcAtomDir+lcAuthFil+".OUT")
  ERASE lcAtomDir+lcAuthFil+".OUT"
ENDIF


*--Build the authorizer string.
*B803961,1 ABD - Fix bug that in case of return don't return -ve value 
*B803961,1 ABD - to the aut. program. [Begin]
*lcString = lcAuthSen+','+lcActType+','+IIF(lnSr=1,"5","0")+','+lcEntMeth+','+;
           ALLTRIM(lcTmRefer)+','+ALLTRIM(STR(lnTmAmount*100))+',,,,'+lcSession+',,,'
lcString = lcAuthSen+','+lcActType+','+IIF(lnSr=1,"5","0")+','+lcEntMeth+','+;
           ALLTRIM(lcTmRefer)+','+ALLTRIM(STR(ABS(lnTmAmount)*100))+',,,,'+lcSession+',,,'
*B803961,1 ABD - [End]


*--Create and Fill the IN File.
lcINAFile = lcAtomDir+lcAuthFil+".IN"
lnHandle = FCREATE(lcINAFile,0)
=FSEEK(lnHandle,0,2)
=FPUTS(lnHandle,lcString)
=FCLOSE(lnHandle)



lnEsc = 0
DO WHILE lnEsc <> 32 AND ! FILE(lcAtomDir+lcAuthFil+".OUT") 
  WAIT WINDOW 'Processing....' NOWAIT

  IF FILE(lcAtomDir+lcAuthFil+".OUT") 
    EXIT
  ENDIF  
  lnEsc=INKEY(0.3)
ENDDO


*--Unload Authorizer
RUN /N StopAuth.exe


*--Read Result...
IF FILE(lcAtomDir+lcAuthFil+".OUT") 

  *--Read Result to lcOutStrg ,
  lcOTFile = lcAtomDir+lcAuthFil+".OUT"
  lnHandle = FOPEN(lcOTFile,0)
  lnHndSz  = FSEEK(lnHandle,0,2)
  =FSEEK(lnHandle,0,0)
  lcOutStrg= FREAD(lnHandle,lnHndSz)
  =FCLOSE(lnHandle)

  *B803961,1 ABD - Fix bug that when the file didn't return any parameter it 
  *B803961,1 ABD - Will release all Comma  ',' in the out file.
  *lcRetCode = IIF(SUBSTR(lcOutStrg,1,1)="0",'APPROVED',IIF(SUBSTR(lcOutStrg,1,1)="2",'HOST DECLINE','LOCAL ERROR'))
  *lcAzStat  = IIF(SUBSTR(lcOutStrg,1,1)="0",'A',IIF(SUBSTR(lcOutStrg,1,1)="2",'D','E'))
  *lcOutStrg = STRTRAN(lcOutStrg,substr(lcOutStrg,1,atc(',',lcOutStrg)))
  *lcAppDesc = IIF(lcAzStat='A',' # ' + SUBSTR(lcOutStrg,1,ATC(',',lcOutStrg)-1),"")
  *lcOutStrg = STRTRAN(lcOutStrg,SUBSTR(lcOutStrg,1,ATC(',',lcOutStrg)))
  *lcRespons = substr(lcOutStrg,1,atc(',',lcOutStrg)-1)
  *lcOutStrg = STRTRAN(lcOutStrg,SUBSTR(lcOutStrg,1,ATC(',',lcOutStrg)))
  *lcRspDate = substr(lcOutStrg,1,atc(',',lcOutStrg)-1)
  *lcOutStrg = STRTRAN(lcOutStrg,SUBSTR(lcOutStrg,1,ATC(',',lcOutStrg)))
  *lcRspTime = substr(lcOutStrg,1,atc(',',lcOutStrg)-1)

  *B803961,1 ABD - Fix bug that when the file didn't return any parameter it 
  *B803961,1 ABD - Will release all Comma  ',' in the out file. [Begin]
  DIMENSION laAuthData [13]
  STORE '' TO laAuthData
  I = 1
  FOR I = 1 TO 13
    laAuthData[I] = LEFT(lcOutStrg,atc(',',lcOutStrg)-1)
    lcOutStrg     = SUBSTR(lcOutStrg,atc(',',lcOutStrg)+1,LEN(lcOutStrg))
  ENDFOR
  lcRetCode = IIF(laAuthData[1] = "0",'APPROVED',IIF(laAuthData[1] = "2",'HOST DECLINE','LOCAL ERROR'))
  lcAzStat  = IIF(laAuthData[1] = "0",'A',IIF(laAuthData[1] = "2",'D','E'))
  lcAppDesc = IIF(lcAzStat='A',' # '+laAuthData[2],"")  
  lcRespons = laAuthData[3]
  lcRspDate = laAuthData[4]
  lcRspTime = laAuthData[5]
  *B803961,1 ABD -[End]  
  

  *B803961,1 ABD - Update the  new 2 fileds that add to the temp file to hold the 
  *B803961,1 ABD - Credit Card number and the approval number. [Begin]
  *-- Save the approve number in the tempfile to print in the report.
  IF lcAzStat='A'
    
    lcCrdData  = laAuthData[8]
    lnCrdLen   = LEN(lcCrdData)
    lnVariable = LEN(lcCrdData) - 4
    *lcCrdData = IIF( LEN(lcCrdData) <= 4,lcCrdData,STRTRAN(lcCrdData,SUBSTR(lcCrdData,1,lnVariable),REPLICATE('*' , lnVariable)) )
    lcCrdData  = IIF( LEN(lcCrdData) <= 4,lcCrdData,REPLICATE('*',lnCrdLen-4)+SUBSTR(lcCrdData,lnCrdLen-3,lnCrdLen))
    SELECT (lcPostmp)
    IF TranType = '4'
      REPLACE cApprovNm WITH lcAppDesc ,;
              cCardNumr WITH lcCrdData
    ENDIF
  ENDIF
  *B803961,1 ABD - [End]


  *--Output message.
  lcRsDtTim = IIF(!EMPTY(lcRspDate+lcRspTime),'DatE/Time:'+lcRspDate+'/'+lcRspTime,"")
  =gfModalGen('INM52017B42000','DIALOG',lcRetCode+'|'+lcAppDesc+'|'+lcRespons+'|'+lcRsDtTim)

  ERASE lcAtomDir+lcAuthFil+".OUT"
ENDIF

SET DEFA TO (lcStorPath)
RETURN (lcAzStat='A')
*:*************************************************************
*: Name      : lfvSRep.
*: Developer : Abdou ElGendi.
*: Date      : 12/12/2000
*: Purpose   : Valid the sales rep.
*:*************************************************************
*: Calls     : 
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvSRep()
*:*************************************************************
*
FUNCTION lfvSRep
PRIVATE lnAlias

lnAlias = SELECT (0)

IF llBrowse .OR. (!EMPTY(lcSRep) AND !SEEK(lcSRep,'SalesRep'))
  SELECT SALESREP
  DO REPCHK WITH lcSRep
  llBrowse = .F.
ENDIF
lnCommRep  =  SALESREP.Comm

SELECT (lcPosTmp)
IF !EOF()
  GOTO TOP
  SCAN REST WHILE !EOF()
    REPLACE REP1   WITH lcsRep
    IF SEEK(Style,'STYLE') .AND. STYLE.Commission
      IF llDIV_COMM
       *-- Get the comm from rep_Div file.
       IF SEEK(lcsRep+STYLE.cDivision,'REP_DIV')
         REPLACE Comm1  WITH REP_DIV.Comm_Rate
       ELSE
         REPLACE Comm1  WITH lnCommRep
       ENDIF  
      ELSE
        IF !EMPTY(lcCustomer) .AND. SEEK(lcCustomer,'CUSTOMER') .AND. CUSTOMER.SALESREP = lcsRep
          *-- Get the comm from the.
          REPLACE Comm1  WITH CUSTOMER.Comm
        ELSE
          REPLACE Comm1  WITH lnCommRep
        ENDIF        
      ENDIF  
    ENDIF
  ENDSCAN
ENDIF

SELECT(lnAlias)
*B604522,1 MAN After the Sales Rep, Either go to the style or the invoice
IF lnSr=2 AND !llAlowRet
   _CUROBJ=OBJNUM(lcInvNo)
ELSE
   _CUROBJ=OBJNUM(lcStyle)
ENDIF

*-- END Of lfvRep
*:*************************************************************
*: Name      : lfvAddDisc.
*: Developer : Abdou ElGendi.
*: Date      : 12/12/2000
*: Purpose   : Valid the Additional discound Code.
*:*************************************************************
*: Calls     : 
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvAddDisc()
*:*************************************************************
*
FUNCTION lfvAddDisc
PRIVATE lnStylAdDi , ldADStrDat ,ldAdEndDat

SELECT (lcPosTmp)
IF !EOF()
  *-- Get the additional Discount.
  lcAddDisc = laAddDisc[lnAddDisc,2]
  lnStylAdDi = 0
  IF !EMPTY(lcAddDisc)
    STORE {} TO ldAdStrDat,ldAdEndDat
    DECLARE laTrmRltFd[3,2]
    laTrmRltFd[1,1] = 'DISCPCNT  '
    laTrmRltFd[1,2] = 'lnStylAdDi'
    laTrmRltFd[2,1] = 'START     '
    laTrmRltFd[2,2] = 'ldADStrDat'
    laTrmRltFd[3,1] = 'DENDATE   '
    laTrmRltFd[3,2] = 'ldAdEndDat'

    =gfRltFld(lcAddDisc,@laTrmRltFd,'CADDRETDIS')    
    IF EMPTY(ldAdStrDat) OR EMPTY(ldAdEndDat) OR !BETWEEN(ldPostDate,ldAdStrDat,ldAdEndDat)
      STORE 0 TO lnStylAdDi
    ENDIF
  ENDIF
  STORE 0 TO lnTotal
  lcAddDisc  = laAddDisc[lnAddDisc,2]
  GOTO TOP
  *B604248,1 ABD - Fix another problem has to do with the add. retail discount 
  *B604248,1 ABD - Which is that when you select a discount from the popup, 
  *B604248,1 ABD - The field becomes not editable and you cannot change your selection.
  *B604248,1 ABD - So , We will scan for transaction sales or return only 
  *B604248,1 ABD - And amount <> 0 . [Begin]
  *SCAN

  *B604253,1 ABD - Fix bug that the program didn't allow to change the additional 
  *B604253,1 ABD - Discount in case of made exchange. [Begin]
  *SCAN FOR TranType <> '4' .AND. nAmount<>0
  lcScanExp  = ".T."
  IF lnSr= 1 .AND. !EMPTY(lcInvNo) .AND. llExchange
    lcScanExp = " TranType = '1' "
  ENDIF
  SCAN FOR TranType <> '4' .AND.  &lcScanExp
  *B604253,1 ABD - [END]
  
    IF TranType='1' .OR. TranType='0'
      *B604248,1 ABD - [End]
      lnNewPrice = Gros_Price-Disc_Amt
      *- Update discount Filed.
    *B604522,1 MAN  Update the taxes
      *REPLACE DiscPcnt    WITH lnStylAdDi                  ,;
              DisCount    WITH ((DiscPcnt/100)*lnNewPrice) ,;
              Price       WITH (lnNewPrice - DisCount)     ,;
              nAmount     WITH ((Price * TotQty) + Tax_Amt),;
              CaddRetDis  WITH lcAddDisc
     REPLACE DiscPcnt    WITH lnStylAdDi                  ,;
              DisCount    WITH ((DiscPcnt/100)*lnNewPrice) ,;
              Price       WITH (lnNewPrice - DisCount)     ,;
              Tax_Amt     WITH ((Tax_Rate/100)*Price),;
              nAmount     WITH ((Price * TotQty) + Tax_Amt),;
              CaddRetDis  WITH lcAddDisc
        
      *B604248,1 ABD - End for if statement. [Begin]
    ENDIF
    *B604248,1 ABD - [End]
    lnTotal = lnTotal+nAmount
  ENDSCAN
ENDIF

*B604248,1 ABD - Add Freight to the total amount if use Freight. [Begin]
IF llDispFrght
  lnTotal = lnTotal + lnFreight				&& Freight amount
ENDIF
*B604248,1 ABD - [End]

IF lnSr=1

  lnPaid  = IIF(!llMltPay,lnTotal,lnPaid)
  lnPaid  = IIF(lnPaid<0,0,lnPaid)

  *B803962,1 ABD Add new setup to ask the user to display 
  *B803962,1 ABD the amount tender or not. [Begin]
  IF llDsAmntNc
    lnTender = lnPaid
  ENDIF
  *B803962,1 ABD [End]
ENDIF

*B803962,1 ABD - Add New setup to return the sale and display the total amount
*B803962,1 ABD - In the Paid amount or set it as it work. [Begin]
IF lnSr=2 .AND. llALLRTAMT
*B604522,1 MAN This new setup 'The amount tender display' should not affect the paid amount
*  lnPaid = lnTotal
ENDIF
*B803962,1 ABD - [End]

*B604248,1 ABD - Browse on top of file. [Begin]
SELECT (lcPosTmp)
GOTO TOP
*B604248,1 ABD - [END]
*--Show browse and refresh.
=lfActBrow()
=lfRefresh()

  
*B604248,1 ABD - Remak the next variable that we didn't need it now. [Begin]
*llFirstTme = .F.
*B604248,1 ABD - [End]

*B604180,1 ABD -Disable filed payment amount in case do not leave 
*B604180,1 ABD -Opend amount setups. [Begin]
*B606415,1 SSH Fix problems in Paid Amount

*IF laSetups[17,2]
IF (lnSr = 1 .AND. laSetups[17,2]) .OR. (lnSr = 2 .AND. llALLRTAMT)
*B606415,1 SSH Fix problems in Paid Amount
  SHOW GET lnPaid  DISABLE
ELSE
  SHOW GET lnPaid
ENDIF
*B604180,1 ABD - [End]

*B803962,1 ABD Add new setup to ask the user to display 
*B803962,1 ABD the amount tender or not. [Begin]
IF llDsAmntNc
  SHOW GET lnTender
ENDIF
*B803962,1 ABD [End]

*B604248,1 ABD - Remak the next line that we need it enable now. [Begin]
*SHOW GET lnAddDisc DISABLE 
*B604248,1 ABD - [End]

*-- End Of lfvAddDisc
*!*************************************************************
*! Name      : lfActPad
*! Developer : Abdou Elgendi [ABD]
*! Date      : 01/10/2001 
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
*B803962,1 
FUNCTION lfActPad

IF llUseAtom
  DEFINE PAD _INQUIRY OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
  SET SKIP OF PAD _INQUIRY OF _MSYSMENU EOF(lcPostmp)
  ON PAD _INQUIRY OF _MSYSMENU ACTIVATE POPUP _INQURYPOP
  DEFINE POPUP _INQURYPOP MARGIN SHADOW
  DEFINE BAR 1  OF _INQURYPOP PROMPT '\<Manually entered card'
  DEFINE BAR 2  OF _INQURYPOP PROMPT '\<Card reader track 1'
  DEFINE BAR 3  OF _INQURYPOP PROMPT 'Card \<reader track 2'
  DEFINE BAR 4  OF _INQURYPOP PROMPT 'C\<heck (MICR) reader'
  ON SELECTION POPUP _INQURYPOP DO lpvInquiry
  
  *- Set mark to defa method.
  SET MARK OF BAR EVAL(lcEntMeth) + 1 OF _INQURYPOP TO .T.
ENDIF

*-- End Of lfActPad.
*!*************************************************************
*! Name      : lpvInquiry
*! Developer : Abdou Elgendi [ABD]
*! Date      : 01/10/2001
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
*B803962,1 
FUNCTION lpvInquiry

lnBarNum = BAR()
IF llUsersExt .AND. !llEdtAses
  IF lfActPassW(.F.)
    lnOldBar = EVAL(lcEntMeth)
    lcEntMeth = STR(BAR() - 1,1)
    SET MARK OF BAR lnOldBar + 1 OF _INQURYPOP TO .F.
    SET MARK OF BAR lnBarNum OF _INQURYPOP TO .T.
  ENDIF
ELSE
  lnOldBar = EVAL(lcEntMeth)
  lcEntMeth = STR(BAR() - 1,1)
  SET MARK OF BAR lnOldBar + 1 OF _INQURYPOP TO .F.
  SET MARK OF BAR lnBarNum OF _INQURYPOP TO .T.
ENDIF

*-- End Of lpvInquiry.
*:*************************************************************
*: Name      : lfvRetMPay.
*! Developer : Abdou Elgendi [ABD]
*! Date      : 01/16/2001
*: Purpose   : Add Return payment.
*:*************************************************************
*: Calls     :None.
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvRetMPay()
*:*************************************************************
*B803962,1 
FUNCTION lfvRetMPay
PRIVATE lnAlias

lnAlias = SELECT(0)
=SEEK (lcInvNo,'INVLINE')
lcSelAcout = INVLINE.Account

SELECT POSTRAN
lcOLdTag = TAG()
SET ORDER TO TAG PostrnA
= SEEK(lcSelAcout +lcInvNo)
lcProsseq  = cPosseq
lcAdddsPop = ALLTRIM(caddretdis)

*C200153,4 ABD - Get the Additional discount per. in case of return from invoice. [Begin]
IF lnSR = 2 .AND. ALLTRIM(caddretdis) = 'CCCCC'
  lnCustDsc = discpcnt
ENDIF
*C200153,4 ABD - [End]

*-- Set order to cposseq+STR(lineno,6) To speed up the count.
SET ORDER TO POSTRAN
*-- Count The number of recored.
COUNT For cPosseq = lcProsseq  .AND.  TRANTYPE = '4' TO lncount

IF lncount > 1 
  SELECT POSTRAN
  GOTO TOP
  SET ORDER TO TAG POSTRAN
  = SEEK(lcProsseq)
  SCAN FOR  cPosseq = lcProsseq .AND. TranType = '4'
    SCATTER MEMVAR MEMO
    SELECT (lcPostmp)
    APPEND BLANK
    GATHER MEMVAR MEMO
    SELECT POSTRAN  
  ENDSCAN
  llMltPay = .T.
ELSE
  IF lncount = 1
    SELECT POSTRAN
    GOTO TOP
    SET ORDER TO TAG POSTRAN
    = SEEK(lcProsseq)
    IF !EOF()
      SKIP
      lcCodePop  = Trancode
    ENDIF  
  ENDIF
ENDIF

SELECT POSTRAN
SET ORDER TO TAG &lcOLdTag
SELECT(lnAlias)

*-- To activate Payment term.
=lfwPayTrm()

*C200153,1 ABD -  get the default additional code if return from customer discount. [Begin]

*C200153,4 ABD - In case we didn't made change or customer have not any
*C200153,4 ABD - Discount get the default discount. [Begin]
IF !llCustDsc .AND. !llExchange
  *C200153,4 ABD - [End]
  = gfwCodePop( @laCodInfo, "CADDRETDIS" ,'N')  
  = gfwCodePop( @laCodInfo, "CADDRETDIS" ,'L')
*C200153,4 ABD - End for if statement. [Begin]
ENDIF  
*C200153,4 ABD - [End]

*C200153,1 ABD - [End]

*B604253,4 ABD - Fix Bug Variable 'lcCodePop' Not Found , we Will not 
*B604253,4 ABD - get the type of payment in case we haven't any payment type. [Begin] 
IF lncount <> 0
  *B604253,4 ABD - [End]
  IF llMltPay
    lnPayTerm = ASCAN('laPayTerm','MMMMMM')
    lnPayTerm = IIF(lnPayTerm =0,1,ASUBSCRIPT(laPayTerm,lnPayTerm ,1))
    SHOW GET lnPayTerm DISABLE
    SHOW GET pbPay,1 PROMPT PADL(ALLTRIM(laPayTerm[lnPayTerm,1]),18,' ') ENABLE
  ELSE
    lnPayTerm = ASCAN('laPayTerm',lcCodePop )
    lnPayTerm = IIF(lnPayTerm =0,1,ASUBSCRIPT(laPayTerm,lnPayTerm ,1))
    SHOW GET pbPay,1 PROMPT PADL(ALLTRIM(laPayTerm[lnPayTerm,1]),18,' ') DISABLE
  ENDIF
  *B604253,4 ABD - End For IF Statement. [Begin]
ENDIF
*B604253,4 ABD - [End]

=gfwCodePop(@laCodInfo,'CADDRETDIS','L')

*C200153,4 ABD - Get the old customer discount from the return lines.
*C200153,4 ABD - And change the popup to show the old discount per. [Begin]
IF lcAdddsPop = 'CCCCC'
  llCustDsc = .F.
  = lfwAddDisc ()
ELSE
  *C200153,4 ABD - [End]
  lnAddDisc = ASCAN('laAddDisc',lcAdddsPop)
  lnAddDisc  = IIF(lnAddDisc =0,1,ASUBSCRIPT(laAddDisc,lnAddDisc,1))
  *C200153,4 ABD - End for if statement. [Begin]
ENDIF
*C200153,4 ABD - [End]
SHOW GET lnAddDisc 

*-- End Of lfvRetMPay.
*:*************************************************************
*: Name      : lfwAddDisc.
*: Developer : Abdou Elgendi [ABD]
*: Date      : 02/27/2001
*: Purpose   : Dont activate Add. discount in case 
*:           : customer have discount
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example   : = lfwAddDisc ()
*:*************************************************************
*C200153
FUNCTION lfwAddDisc

= gfwCodePop( @laCodInfo, "CADDRETDIS" ,'L')  
IF ASCAN( laAddDisc,'CCCCCC') = 0
  lnAddDisc = ALEN(laAddDisc,1)+1
  DECLARE laAddDisc[lnAddDisc ,2]
  *--Code of Customer Discount is 'CCCCCC' used in this program only.
  laAddDisc[lnAddDisc ,1]=PADR('Customer Disc. '+ALLTRIM(STR(lnCustDsc,5,2)),30)
  laAddDisc[lnAddDisc ,2]='CCCCCC'
  SHOW GET lnAddDisc DISABLE
ELSE
  lnAddDisc = ASCAN(laAddDisc,'CCCCCC') / 2
  laAddDisc[lnAddDisc ,1]=PADR('Customer Disc. '+ALLTRIM(STR(lnCustDsc,5,2)),30)
  SHOW GET lnAddDisc DISABLE  
ENDIF

RETURN

*-- End OF lfwAddDisc.
*:*************************************************************
*: Name      : lfvCustDis.
*: Developer : Abdou ElGendi.
*: Date      : 12/12/2000
*: Purpose   : Valid the customer discount code & update temp File.
*:*************************************************************
*: Calls     : 
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvCustDis()
*:*************************************************************
*:C200153,4 
FUNCTION lfvCustDis
PRIVATE lnStylAdDi
STORE lnCustDsc TO lnStylAdDi

*B604522,1 MAN Keep the old amount, When changed later, the program will defaul the paid amnt.
STORE lnTotal TO lnOldTotal

SELECT (lcPosTmp)
GOTO TOP
IF !EOF()
  STORE 0 TO lnTotal
  lcScanExp  = ".T."
  IF lnSr= 1 .AND. !EMPTY(lcInvNo) .AND. llExchange
    lcScanExp = " TranType = '1' "
  ENDIF
  SCAN FOR TranType <> '4' .AND.  &lcScanExp
  
    IF TranType='1' .OR. TranType='0'
      lnNewPrice = Gros_Price - Disc_Amt
     
      *- Update discount Filed.
      *B604522,1 MAN  Recalculate Tax Amounmt
*      REPLACE DiscPcnt    WITH lnStylAdDi                  ,;
              DisCount    WITH ((DiscPcnt/100)*lnNewPrice) ,;
              Price       WITH (lnNewPrice - DisCount)     ,;
              nAmount     WITH ((Price * TotQty) + Tax_Amt),;
              CaddRetDis  WITH lcAddDisc
      REPLACE DiscPcnt    WITH lnStylAdDi                  ,;
              DisCount    WITH ((DiscPcnt/100)*lnNewPrice) ,;
              Price       WITH (lnNewPrice - DisCount)     ,;
              Tax_Amt     WITH ((Tax_Rate/100)*Price),;
              nAmount     WITH ((Price * TotQty) + Tax_Amt),;
              CaddRetDis  WITH lcAddDisc

    ENDIF
    lnTotal = lnTotal+nAmount
  ENDSCAN
ENDIF

IF llDispFrght
  lnTotal = lnTotal + lnFreight				&& Freight amount
ENDIF

IF lnSr=1
*B604522,1 MAN Default the paid amount only if the total has been changed
*  lnPaid  = IIF(!llMltPay,lnTotal,lnPaid)
  lnPaid  = IIF(!llMltPay AND lnTotal <> lnOldTotal,lnTotal,lnPaid)
  lnPaid  = IIF(lnPaid<0,0,lnPaid)
  IF llDsAmntNc
    lnTender = lnPaid
  ENDIF
ENDIF

IF lnSr=2 .AND. llALLRTAMT
*B604522,1 MAN This new setup 'The amount tender display' should not affect the paid amount
*  lnPaid = lnTotal
ENDIF

SELECT (lcPosTmp)
GOTO TOP
*-- Refresh the additional discount popup .
*B604522,1 MAN Added If condition
IF !laScrMode[4] .AND. !llCustDsc
  = gfwCodePop( @laCodInfo, "CADDRETDIS" ,'N')
ENDIF  
*--Show browse and refresh.
=lfActBrow()
=lfRefresh()

*B606415,1 SSH Fix problems in Paid Amount
*IF laSetups[17,2]
IF (lnSr = 1 .AND. laSetups[17,2]) .OR. (lnSr = 2 .AND. llALLRTAMT)
*B606415,1 SSH Fix problems in Paid Amount
  SHOW GET lnPaid  DISABLE
ELSE
  SHOW GET lnPaid
ENDIF

IF llDsAmntNc
  SHOW GET lnTender
ENDIF

*-- End Of lfvCustDis.
*:*************************************************************


*:*************************************************************
*: Name      : lfPrepCust.  *C200222,1
*: Developer : Ahmed Salah Shalaby - (SSH).
*: Date      : 03/05/99
*: Purpose   : Functio to prepare for scaning Customer.
*:*************************************************************
*: Calls     : ..............
*:*************************************************************
*: Passed Parameters  : 
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfPrepCust()
*:*************************************************************
*:
*C200222,1 SSH Get the customer code from magnitic card.
FUNCTION lfPrepCust
PRIVATE lcScnManu

lcScnManu = gfGetMemVar('M_SCANMANU')
llCusScan = TYPE("lcScnManu") = "C" .AND. !EMPTY(ALLTRIM(lcScnManu)) .AND. ALLTRIM(lcScnManu) = "S"


*:*************************************************************
*: Name      : lfWCust.  *C200222,1
*: Developer : Ahmed Salah Shalaby - (SSH).
*: Date      : 03/05/99
*: Purpose   : Customer When Function.
*:*************************************************************
*: Calls     : ..............
*:*************************************************************
*: Passed Parameters  : 
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfWCust()
*:*************************************************************
*:
FUNCTION lfWCust

IF llCusScan
  SET CONFIRM OFF
ENDIF