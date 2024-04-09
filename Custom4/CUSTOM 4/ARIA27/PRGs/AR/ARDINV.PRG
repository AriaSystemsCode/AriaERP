**************************************************************************
*: Program file  : ArDInv.PRG
*: Program desc. : Direct Invoice
*:
*:         System: Aria Apparel System
*:      Developer: WAM - Wael Aly Mohamed
*:************************************************************************
*: Calls : 
*:         Functions  : 
*:*************************************************************
*: Passed Parameters  : lcInvoiceNo : Invoice#
*:*************************************************************
*: Modifications      :
*E300889,1 TAK 06/23/1998 Added to control the location editing if the
*E300889,1                system was setup to Point of Sale.
*E300834,1 WAM 08/12/1998 - Update stock for Inventory styles only
*E300834,1                - Get Style price discount according to style 
*E300834,1                  discount code.
*E300834,1                - Get style price according to ordered quantity
*E300834,1                  when the customer use price level at quantity.
*B602278,1 WAM 11/26/1998 Elimenate price roundation
*B602295,1 WAM 11/30/1998 Void invoices have Voided Credit Memo
*B602322,1 TAK 12/03/1998 Pass GL Session sequence as a parameter to 
*B602322,1                the GFSTYCRL function to make sure that the record
*B602322,1                in journal with the same session no.
*E301090,1 WAM 12/10/1998 Do not delete invoice lines while void invoice.
*B602325,1 WAM 12/10/1998 Accept empty sales rep.
*B602446,1 WAM 01/11/1999 Elimenate Ship amount roundation
*B602465,1 WAM 01/17/1999 Store alternative ship address
*B801942,1 WAM 02/09/1999 Add the ability to cancel session defaults
*E301142,1 WAM 02/22/1999 Select alternative price level
*E301077,1 WAM 02/18/1999 Inhance openning files to speed up transaction
*B802050,1 WAM 03/16/1999 Refresh tool bar buttons after call other options
*E301176,1 HDM 03/22/1999 Prevent programs from displaying notepad icon
*                           as it's now controlled globally
*B602681,1 WAM 03/18/1999 Seek the session record in the uncompeted sessions 
*B602681,1                file after return from other called screen.
*B602636,1 WAM 03/24/1999 Defualt pack quantity when selecting style pack
*B801882,1 WAM 03/31/1999 Recompute USA charges when needed.
*B602737,1 MAN 04/04/1999 Total Charges is saved with 0 values sometimes
*:B602753,1 HDM 04/07/1999 Stop Calling NotePad Program In lpSavScr as the global save
*:                         Will Call it
*:E301200,1 AHM 04/13/1999 Add invoice line No in calling gfStyCrl function
*:E301200,1                to update the added field (lineNo) in StyInvJl file
*:E301192   AHM 04/19/1999 Update reference field in styinvjl with descriptive text
*:E301204,1 MAB Update Customer History file for Void invoice 
*:              instead of delete its records from Debit file.
*E301217,1 AMM 05/09/1999 Calculate due date of the invoice due to the new 
*E301217,1 AMM            added field that define EOM day.
*B602894,1 MAB 05/17/1999 Void invoice credit record type should be I not 5.
*B602905,1 MAB 05/17/1999 Default No. of cartons must be 1 at least.
*E301245,1 SSE 06/09/1999 Added a new field nHgWtrMark and calcualte its value
*                         from NETBAL field , if less than NETBAL update with 
*                         NETBAL else leave it with existing value
*B802176,1 TAK 06/21/1999 Fixed wrong second sales rep commision for store.
*B603064,1 MAN 07/08/1999 Clear taxes after every invoice.
*B802241,1 AHM 08/02/1999 Make the shipping address is the store dist. center add. 
*B802241,1                if there is dist. center , else it's store address
*B602937,1 AHM 08/05/1999 Update Audit fields in GLDist File
*B602893,1 AHM 08/11/1999 Fixing the bug of variable lcTmpAr not found
*B603134,1 AHM 08/29/1999 Fixing the bug of not printing Invoice
*B802575,1 RENEE 09/02/99 Fix Alias "ARHIST" not found when voiding an invoice
*B802653,1 AHM 09/23/1999 Fix the bug of rounding the calculated total charge 
*B603219,1 AHM 11/01/1999 Fix the bug of wrong update TotAge in customer file 
*B603219,1                in case of voiding the invoice
*B802779,1 WAM 11/09/1999 When void invoice we should not send 810
*B603181,1 AHM 11/10/1999 Fix the bug of updating Tran field in ARHist file
*B603181,1                 with new sequnce No. instead of Invoice No.
*B603293,1 AHM 11/23/1999 Fix the bug of rounding the calculated total charge 
*B603293,1                if the discount is defaulted from customer screen
*B603305,1 SSH 12/23/1999 Fix the bug of duplicate the appramt in case of 
*B603305,1                voide invoice and rebulid order.
*B802859,1 SSH 12/14/1999 Fix the bug of automatically bounces back to the
*B802859,1 SSH            original Invoice.
*B603341,1 SSH 12/15/1999 Fix the bug of neglect extra charge amount.
*B603308,1 WAB 12/30/1999 void the invoice with the price from ordline
*B603401,1 WAM 01/25/2000 Fix Error while browse account
*B603411,1 NAD 01/30/2000 Fix the bug of long delay after entering style/color 
*B603411,1                When doing direct invoice.
*B603395,1 SAM 08/02/2000 Refresh sales rep. header commission  
*B603449,1 ABD 14/02/2000 Fix Bug that invoice program accetps Negative Quantity.
*B603119,1 NAD 03/12/2000 Fix Bug Alias not found in Uncomplete Session  
*B801953,1 NAD 03/19/2000 FiX Bug in the cartons screen of direct invoice
*B802853,1 NAD 03/19/2000 Fix Bug Round tax amount in the invoice
*B603402,1 NAD 03/20/2000 Fix Bug Phone Number doesn't updated correctly. 
*B602760,1 NAD 03/20/2000 Fix Bug Alias not found
*B603363,1 NAD 04/09/2000 Fix Bug subscript out of bounds while browsing dylots
*E301386,1 NAD 04/12/2000 Add the edit mode for the direct invoice for ship date,
*E301386,1                Due Date and Departmant
*C101796,1 NAD 04/17/2000 Custom Process for GMA to Edit more fields ine the invoice    
*B603638,1 WAM 05/14/2000 Fix bug variable "LAENGSTYTAX" not found
*B603641,1 WAM 05/14/2000 Print invoice after enter note pad
*C200195,1 BWA 06/13/2001 Custom trigger to enter any style regardless of season.
*B803350,1 NAD 06/22/2000 1)Fix bug message "you are editing the same record in another session"     
*B803350,1                2)Update the store field in the INVLINE,UPSBOX, REPCOMM
*B803350,1                DEBIT and ARHIST for GMA only.                   
*E301429,1 NAD 06/25/2000 Adjust the fields width in the ARAging Screen
*B803397,1 NAD 07/13/2000 Fix bug Edit Installments error Alias not found. 
*B803369,1 NAD 07/26/2000 Open the uncomplete session file before calling gpSaveInv.
*B603760,1 NAD 07/30/2000 1-Fix bug style X records are missing while voiding conslidated invoice. 
*B603760,1                2-Add the cWarecode field to the invline file.   
*B603760,1                3-If ship amount equal zero, don't make GL transaction. 
*E301466,1 AMH 09/07/2000 Switching Potential customer to Active when creating new invoice.
*B803642,1 NAD 09/10/2000 Not to check for the posting date if the system was not linked to GL.
*B603727,1 NAD 09/17/2000 1-After changing the store new added styles doesn't appear in the browse. 
*B603727,1                2-Saving the invoice with a wrong store that dosn't exists in the customer file.
*B603727,1                3-Add a when function for the store field in the ardinv1.scx
*B603727,1                4-Disable the size screen fields after message zero quantities this line will be deleted. 
*B603713,5 NAD 09/27/2000 Increase the Gross price,Net price and the fields in the charges tab to accept
*B603713,5                large currencies like Italian Lira in the ARDInv Screen set. 
*B803571,1 NAD 10/25/2000 Not to allow users to add negative values in the charges fields.
*B603989,1 NAD 10/26/2000 COD amount shows different values while moving through the charges fields. 
*B803747,1 NAD 10/26/2000 Uncompleted session message displays after cancelling the invoice.
*B603966,1 NAD 11/22/2000 Problem with the salesrep commission worksheet. 
*C102052,1 ABD 11/29/2000 Handle discounts in case multi location get the discounts from stydye.
*C102052,1 ABD            upon addING new related field about whole sale or retail sale.
*B604116,1 AMH 01/25/2001 Fix bug of Total header doesn't equal the total of the lines.
*C102212,1 ADEL 03/14/2001 Add HST tax to setup and add it on the Charge folder.
*C102239,1 ADEL 04/17/2001 Add custom Carton information screen for BEL05.
*B604390,1 NAD  04/30/2001 Wrong transactoin date in the credit file while voiding invoices 
*B804106,1 NAD  04/30/2001 Style browse is empty when Clicking the browse button 
*C102314,1 BWA 06/10/2001 Add a triger functions for the custome charges screen.
*B804430,1 NAD 09/25/2001 Error "Duplicate fieldnames" while voiding invoce           
*B604924,1 SSE 11/01/2001 Fix bug of not updating the Pik Qty in OrdLine , Alo in Style and StyDye
*B604924,1                When voiding the invoice.
*B804466,1 ADEL 10/10/01 Fix the bug of preventing from making invoices only when linked to GL.
*C102424,1 ADEL 10/15/01 Add custom Carton information screen for OGIO.
*B605166,1 NAD 11/27/2001 Fix bug of resending the triggers for Season check.
*B603998,1 SSE 11/21/2001 Fix bug of not reseting the rep commission if user changed division.
*B603998,1                Also add a field called nvHstAmt for holding the nHstAmt of voided invoice.
*B605532,1 NAD 02/17/2002 Restore the dyelot after voiding the invoice
*B605617,1 NAD 03/18/2002 File Piktkt Not found 
*C102574,1 HBG 04/15/2002 Modification in direct invoice screen for selecting Packs
*C200352,1 HBG 04/15/2002 IF Trigger to GMA update 'SHDAEXPR' File of UPS system 
*C200340,1 ABD 05/23/2002 Add new Custom trigger for customer stuncroft.
*B606120,1 NAD            Fix For C200340
*B606206,1 TMI 07/03/2002 Make sales reps be assigned to customers at store level
*B606333,1 NAD 08/01/2002 Numeric overflow in case of costing method FIFO/LIFO 
*B606106,1 SSE 07/16/2002 Fix bugs of weight doesn't show less than one (ex. 0.54) 
*B606106,1                All modifications in screen set.
*B604091,1 SSE 09/05/2002 Fix bug Update commission based on Salesrep commission base setup.
*C102676,1 SSE 09/03/2002 Custom modifications add 3rd Rep and comm for OrdHdr file.
*B606392,4 SSE 10/27/2002 Fix bug of not locking the order which is selected for invoice.
*B606702,1 SSE 01/15/2002 Fix bug of numeric overflow when voiding factored invoice.
*B607038,1 ABD 07/30/2003 Wrong Value display on the aging screen, the program 
*B607038,1 ABD            Doesn't  use the seting Age based on Terms or days, and also 
*B607038,1 ABD            add new seting in ar to detrmined the Credit Limit Aged Days.
*B604418,1 NNA 12/30/2003 Fix bug of not refreshing the G.S.T Tax,HST Tax,P.S.T Amount And Freight
*B604418,1 NNA 			  When the Ship_Amount Become Zero
*B120822,1 MHM 01/05/2004 Fix Bug of the size headers for the style are not displayed if go to 
*B120822,1 MHM            inventory adjustment and back again to Invoice screen. 
*B121026,1 MHM 12/30/2003 Fix Bug of accept saving with empty Invoice Date. 
*B604425,1 NNA 12/31/2003 Add the H.S.T Tax to the C.O.D amount.
*B604425,1 NNA            Recalculate the C.O.D amount when The C.O.D  Charge Become .T. Or .F.
*B037410,1 MHM 01/15/2004 Fix Bug of the Qtys for the style are not displayed correctlly if go to 
*B037410,1 MHM            inventory adjustment and back again to Invoice screen. 
*B037436,1 MHM 01/20/2004 Fix bug of goto infinit loop if you open Direct Invoice.
*E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char
*C037816,1 MHM 04/06/2004 Custom Bin Location For David Luke
*B122051,1 NNA 03/14/2004 Fix bug of Can't view UPS shipping info on invoices , when we click 
*B122051,1 NNA            "Cartons..." button on Invoice charge screen, we get the error message like 
*B122051,1 NNA            (Variable 'lcUpsTmp not Found)
*C037814,1 TMI 04/08/2004 Update total charges field with the Additional charges custom field for REL01
*B123016,1 NNA 05/30/2004 Fix bug that when you want now to void an invoice that made in a 
*B123016,1 NNA            Previous locked period the void invoice option doesn't work
*B038154,1 TMI 07/27/2004 Fix bug that when open invadj screen the scale desc is lost
*B123626,1 NNA 11/01/2004 Fix bug that if you have 3 Consolidated invoices and you voided the First
*B123626,1 NNA            then you voided the third, you'll see that system make the Second invoice
*B123626,1 NNA            voided in the Consinvh.dbf but it's Status in the InvHdr.dbf still Completed
*B123626,1 NNA            So this bug effect on the Rebalance Program
*B126004,1 ASH 01/31/2005 Fix bug of displaying account summary screen in all cases.
*B126828,1 EIH 03/15/2005 Fix bug when select styles from same division on invoice transaction.
*B039106,1 NNA 04/13/2005 Fix bug that duplicate qty at Order line after Voiding a Consolidated Invoice.
*B127347,1 TMI 05/03/2005 Remove the invoice refrence from POCRTNMF file if the invoice is voided
*C123851,1 TMI 06/07/2005 Update the ORDCHG file when the invoice is voided
*B128959,1 NNA 09/04/2005 Fix bug that Style with Inv.Style Not Checked not invoiced if onhand = 0
*B131175,1 MMR 03/28/2006 Fix bug of make sales disable if edit comm per line setup=yes.
*E131400,1 NNA 04/12/2006 get the default warehous as saved in the file that if there is no default warehouse assigned 
*E131400,1 NNA            to the invoice screen
*B132491,1 TMI 07/17/2006 relocate the pointer to the correct record in the invhdr file
*B608279,1 NNA 09/23/2007 Update the audit trail information fields at piktkt file when void an invoice and change the pick
*B608279,1 NNA            ticket status to open
*E302472,1 MHM 11/18/2007  Multiple Taxes for Canada on Invoice [T20060709.0008]
*:******************************************************************************

PARAMETERS lcInvoiceNo
EXTERNAL ARRAY laData,laKeyField

*B603119,1 NAD (Start) Declare laVars[20] instead of laVars[19] to hold llActCrdt
*DECLARE lafolders[3,2],laAddress[6,3],laQtyPaste[1,8],lafoldwinds[3,2],;
        laKeyField[1,4],laSetups[20,2],laVars[19],laBrowArr[1]
        
DECLARE lafolders[3,2],laAddress[6,3],laQtyPaste[1,8],lafoldwinds[3,2],;
        laKeyField[1,4],laSetups[20,2],laVars[20],laBrowArr[1]
*B603119,1 NAD (End)
*C102212,1 (Begin) Increase array setup by 1. for HST tax.
DECLARE laSetups[21,2]
*C102212,1 (End)

*E301217,1 AMM declare laTrltFld [6,2] to add a new related field of EOM day
*DECLARE laCodes[6,10],laTerms[1,2],laSeasons[1,2],laDivision[1,2],;
        laShipVia[1,2],laSpcInst[1,2],laTRltFld[5,2],laDRltFld[2,2],;
        laVRltFld[1,2],laWareHouses[1,2]
DECLARE laCodes[6,10],laTerms[1,2],laSeasons[1,2],laDivision[1,2],;
        laShipVia[1,2],laSpcInst[1,2],laTRltFld[6,2],laDRltFld[2,2],;
        laVRltFld[1,2],laWareHouses[1,2]
*E301217,1 AMM end

*B603638,1 Fix bug variable "LAENGSTYTAX" not found
DECLARE laEngStyTax[1,2]
STORE 0 TO lnTaxRate  && Tax Rate at Style level  
STORE '' TO laEngStyTax
*B603638,1 (End)

*E300834,1 Declare discount code related fields array
DECLARE laDisRltFld[1,2]
*E300834,1 Initialize discount percent
STORE 0   TO lnDisc_Pcnt
*B603449,1 ABD add the initialization of lcOldValue
STORE '' TO laCodes,laTerms,laShipVia,laSpcInst,laSeasons,laDivision,;
            laVars,lcGlSession,laBrowArr,laWareHouses,lcTaxName,lcOldValue
*B603727,1 NAD 09/17/2000 (Start) Variable to hold the old store.
lcOldStore=SPACE(8)
*B603727,1 (End)            
laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] = .T.
laKeyField[1,3] = 'INVHDR'
laKeyField[1,4] = 1
STORE .F. TO laSetups,llRebuild
lcDelMesag    = 'void'
laDefProc[7]  = .F.     && Void procedure(lpDelScr)
laDefProc[9]  = .F.     && Save procedure(lpSavScr)
laDefProc[10] = .F.     && close procedure(lpClsScr)

STORE 0    TO lnMarker,lnTopMark,lnBotMark,lnMerchTax,lnChrgtax,laQtyPaste
STORE ' '  TO lcInvBrow ,lcBrTtl2,lcBrowseTl,lcInvStat,lcGlYear,lcGlPeriod,;
              lcSysYear,lcSysPeriod,lcIDefSes,lcIDefDiv,lcPriceLvl,lcWinCh1,;
              lcWinCh2,lcWinCh3,lcWinCh30,lcWinCh31,lcWinCh32,lcWinCh33,;
              lcWinCh34,lcWinCh35,lcWinCh4,lcWinCh5,lcWinCh6,llAlowNew,lcIDefWare,lcCurrInv
              
STORE {} TO ldDefInvDate,ldDefPstDate
STORE 0 TO lnCartons,lnLines ,lnSouComm1,lnSouComm2,lnActFolder,lnStyLngth

*lnCartons = 1
STORE 1 TO lnShipAddr,lnPstRule,lnWareHouse
STORE '' TO lcLastStyle,lcFiles
STORE '' TO  lcDShpName,lcDShpAdd1,lcDShpAdd2,lcDShpAdd3,lcDShpAdd4,lcDShpAdd5,;
             lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,;
             lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5,;
             lcDBllName,lcDBllAdd1,lcDBllAdd2,lcDBllAdd3,lcDBllAdd4,lcDBllAdd5,;
             lcFolder,lcTaxTitle,lcTaxBreak,lcSession,lcUpsType
STORE .F. TO llBrowse,llZoom,lcIDefPrnt,cbSetAsDef,llIsCanada,llOpnCrdt,;
             llIsEngland,llMulCurr,llEditExRt,llupsinsur,llCod,llNewLine,;
             llContinue,llSysDate,llInstTerm,llActCrdt
STORE .T. TO llReBrowse,llAddLine,llReMerTax
*B803571,1 NAD 10/25/2000 (Start) Variable to hold the old Charges used in the when function of the field. 
PRIVATE lnOldChrg
STORE 0 TO lnOldChrg
*B803571,1 NAD 10/25/2000 (End)
*B602893 (Start)
*STORE ""  TO lcInvLine,lcInvHdr,lcGlDist,lcPackHdr,lcUpsBox,lcEngChrg,;
             lcVScFields,lcScFields,lcWinChrg,lcStyHdr,;
             lcInstHdr,lcInstLin,lcEmptySty,lcAppCrdt,lcUpsTrack
STORE ""  TO lcInvLine,lcInvHdr,lcGlDist,lcPackHdr,lcUpsBox,lcEngChrg,;
             lcVScFields,lcScFields,lcWinChrg,lcStyHdr,;
             lcInstHdr,lcInstLin,lcEmptySty,lcAppCrdt,lcUpsTrack,lcTmpAR
*B602893 (End)

STORE ''  TO lcTEOM,lcTCod,lcBaseFild,lcBaseTit,lcScrMode
*E301217,1 AMM Initialize
STORE 20 TO lnEomDay   && End of month day default value
*E301217,1 AMM end
STORE 0   TO lnTDaysDue,lnTaxDue
lcStatColor = gcObjColor
lnSessNo = gnProgCopy
STORE 1   TO lnTerms,lnShipVia,lnSpcInst,lnSeason,lnDivision,lnCanReason
STORE 0   TO lnLastMode
STORE .T. TO llGoAndChk,llChkUnCom

*E301204,1 lnUnCmSeRc IS numeric variable [Begin]
*STORE ''  TO lcAdTrnSeq,lcAcTrnSeq,lcHisSeq,lcGlSess,lcRepBat,lnUnCmSeRc
STORE ''  TO lcAdTrnSeq,lcAcTrnSeq,lcHisSeq,lcGlSess,lcRepBat
lnUnCmSeRc = 0
*E301204,1 lnUnCmSeRc IS numeric variable [End  ]

*B607038,1 ABD - Define new variable to be based on the seting. [Begin]
PRIVATE lcCrLm_Day 
STORE '' TO lcCrLm_Day , lcCrLm_Exp
*B607038,1 ABD - [End]

STORE .T. TO llUpdGlDif,llUpdMstGL
*-- initialize all the global variables and opens the files needed by the program. 
IF !gfSetup()
  RETURN
ENDIF
*B802859,1 SSH Set filter to the selected account when call from customer screen.
IF TYPE('lcInvoiceNo') = 'C' .AND. SEEK(lcInvoiceNo,'INVHDR')
  SELECT INVHDR
  lnOldOrd = ORDER()
  SET ORDER TO Invhdra
  PRIVATE lcTempFlt
  lcTempFlt = INVHDR.Account
  SET FILTER TO Account+Invoice = lcTempFlt
  SET ORDER TO &lnOldOrd
  GOTO TOP
ENDIF
*B802859,1 SSH (END)

lcProgID  ='DINVOICE'
llReBrowse = .T.
lcInvBrow = 'Invoice Lines'
lcBrTtlZ  = 'Zoom Invoice Lines'
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
lafolders[3,1] = 'Charges'
lafolders[3,2] = 3
lnnofld = 3

lcwfoldchng = '=lfActFolder()'  && function to control shows after change the folder
lnfoldercend = 102.000
lnfolderrend = 2.000
STORE 'DISABLE' TO lcpbNoteStat,lcpbObjStat,lcPbSendStat
*B802050,1 Force calling lpshow function
llNoShow = .F.
*B802050,1 (End)

IF !WEXIST(gcBaseWind)
  STORE 0 TO lnLastMode
  lcEmptySty = PADR(STRTRAN(GFITEMMASK("PI"),'X',' '),19)
  lcStyHdr   = gfItemMask("HI")
  lnStyLngth = LEN(lcStyHdr) 
  lcScFields = 'INVOICE,ACCOUNT,ORDER,CUSTPO,STORE,SHIPDATE,DUEDATE,APPROVAL,'+;
               'cTermCode,SHIPVIA,SPCINST,SEASON,cDivision,DEPT,NOTE1,NOTE2,REP1,'+;
               'COMM1,REP2,COMM2,CWARECODE,CCURRCODE,NEXRATE,CFACCODE,CARTONS,'+;
               'WEIGHT,CODTAG,COD_AMT,TRDE_DISC,PIKTKT,SHIPAMT,DISCPCNT,'+;
               'DISCOUNT,FREIGHT,INSUR,COD,TAX_AMT,TOTALCHG,NCHARGES,'+;
               'NCURRUNIT,INVDATE,SHIP,nPstAmt,cTaxRule,nPstRate,TAX_RATE,'+;
               'BOL_NO,APPRAMT,DPOSTDATE,CCODTRCKNO'
  lcVScFields= 'INVOICE,ACCOUNT,ORDER,CUSTPO,STORE,SHIPDATE,DUEDATE,APPROVAL,'+;
               'cTermCode,SHIPVIA,SPCINST,SEASON,cDivision,DEPT,NOTE1,NOTE2,REP1,'+;
               'COMM1,REP2,COMM2,CWARECODE,CCURRCODE,NEXRATE,CFACCODE,CARTONS,'+;
               'WEIGHT,CODTAG,VCOD_AMT,TRDE_DISC,PIKTKT,VSHIPAMT,DISCPCNT,'+;
               'VDISCOUNT,VFREIGHT,VINSUR,VCOD,VTAX_AMT,VTOTALCHG,NvCHARGES,'+;
               'NCURRUNIT,INVDATE,VSHIP,nVPstAmt,cTaxRule,nPstRate,TAX_RATE,'+;
               'BOL_NO,APPRAMT,DPOSTDATE,CCODTRCKNO'
  *C102212,1 (Begin) Add HST Tax fields (nPstRate,nHstRate) to fields.
  lcScFields  = lcScFields  + ',nHstRate,nHstAmt'
  
  
 *B603998,1 Add the voided HST Tax field instead of the HST tax. [Begin]
  *lcVScFields = lcVScFields + ',nHstRate,nHstAmt'
  lcVScFields = lcVScFields + ',nHstRate,nvHstAmt'
  *B603998,1 Add the voided HST Tax field instead of the HST tax. [End]
  *C102212,1 (End)
               
  SELECT InvHdr
  SCATTER FIELDS &lcScFields TO laData BLANK
  lcSession   = gfsequence('CSESSION')
  lcGlSession = gfsequence('GLSESSION')
  llAlowNew = .T.
  laSetups[1,1]  = 'M_PACK'         &&  Use style packs / SKU    
  laSetups[2,1]  = 'M_STY_COM'      &&  Commision at Style level   
  laSetups[3,1]  = 'M_LN_NOTE'      &&  Add Note to the invoice line
  laSetups[4,1]  = 'M_LINK_GL'      &&  Check for Gl link
  laSetups[5,1]  = 'M_WareHouse'    &&  use maltiple locations Y Or N  Ic Setup 
  laSetups[6,1]  = 'M_GenOrNum'     &&  Enter order # Manually Y Or N
  laSetups[7,1]  = 'M_COST_METH'    &&  Cost Method
  laSetups[8,1]  = 'M_DYELOT'       &&  Use Dylot Y Or N      
  laSetups[9,1]  = 'M_TAX'          &&  use Taxes Y or N
  laSetups[10,1] = 'M_TAX_RATE'     &&  Tax Rate
  laSetups[11,1] = 'M_TAX_METH'     &&  Tax Method
  laSetups[12,1] = 'M_UPC_USE'      &&  Use U.P.C. Numbers        
  laSetups[13,1] = 'M_DIV_LINK'     &&  GL link codes at Division level
  laSetups[14,1] = 'M_REP_COMM'     &&  Salesrep commission base 
  laSetups[15,1] = 'M_UPSBOX'       &&  Track Boxes for Ups Manifacuring   
  laSetups[16,1] = 'XAGINGTYPE'     &&  Aging AR by Date\Terms 
  laSetups[17,1] = 'XPOSTFINV'      &&  Post Factored invoice to customer
  laSetups[18,1] = 'M_CRDT_LMT'     &&  Warn above Credit limit 
  laSetups[19,1] = 'M_EDTPRICE'     &&  Edit style price Y/N 
  laSetups[20,1] = 'M_TAX_DESC'     &&  Tax Name      
  *C102212,1 (Begin) Get HST Tax.
  laSetups[21,1] = 'M_HST_RATE' && HST Tax Rate
  *C102212,1 (End)
  
  *C200195,1 Customer Trigger to add a new SO setup. [Begin] 
  IF ASCAN(laEvntTrig , PADR('STYLEVALID',10)) <> 0
    DECLARE laSetups[22,2]
    laSetups[22,1] = 'M_SEASNCHK'     && Check for Season Validation on every style entered on Order Line.
  ENDIF   
  *C200195,1 Customer Trigger to add a new SO setup. [End] 
  
  =gfGetMemVar(@laSetups,gcAct_Comp)
  
  *B607038,1 ABD - get the Credit Limit Aged Days  seting . [Begin]
  lcCrLm_Day = gfGetMemVar('M_CRLM_DAY',gcAct_Comp)    && Credit Limit Aged Days.
  *-- Function to get the Credit Limit Expration.
  = lfGetAgeBy ()
  *B607038,1 ABD - [End]

 
  lcTaxName  = IIF(EMPTY(laSetups[20,2]),'G.S.T. Tax',laSetups[20,2])
  llIsCanada = IIF(UPPER(ALLTRIM(gcContCode))='CANADA',.T.,.F.)

  IF UPPER(ALLTRIM(gcContCode))='ENG'   && in case of England
    *-- Arrat to hold Tax Rate at Style level 
    DECLARE laEngStyTax[1,2]    
    laEngStyTax[1,1] = 'NTAXRATE'
    laEngStyTax[1,2] = 'lnTaxRate'  && Tax Rate at Style level  

    STORE "" TO lcTaxTitle,lcTaxBreak
    llIsEngland = .T.
    *-- variable to hold the temp Name for the english charges file
    lcEngChrg = gfTempName() 
    *-- in case of england Track Boxes for Ups Manifacuring must be No
    laSetups[15,2]='N'
  ELSE
    llIsEngland = .F.
  ENDIF
  llMulCurr   = gfGetMemVar('llMulCurr',gcAct_Comp)   && Use Multi Currency       
  llEditExRt  = gfGetMemVar('LLEDITEXRA',gcAct_Comp)  && Change exch. rates       

  lcBaseFild = "INVOICE :H='Invoice',INVDATE :H='Date',ACCOUNT :H='Account',"+;
               "STORE :H='Store',ORDER :H='Order',CUSTOMER.STNAME :H='Name':20 ,"+;
               "SHIP :H='Pieces',TOTALCHG :H='Amount',"+;
               IIF(laSetups[5,2]='Y',"cWareCode :H='Warehouse',",'')+;
               IIF(llMulCurr,"cCurrCode :H='Currency',NexRate :H='Curr.Rate',",'')+;
               "REP1 :H='Rep1', COMM1 :H='Comm',REP2 :H='Rep2', COMM2 :H='Comm',"+;
               IIF(gfIsEdtble('CTERMCODE'),"cTermCode :H='Terms'",;
               "lcDesc= gfCodDes(cTermCode,'CTERMCODE') :H='Terms'")
  lcBaseTit  = 'Invoices'

  lcWinCh1    = gfTempName()
  lcWinCh2    = gfTempName()
  lcWinCh3    = gfTempName()
  lcWinCh30   = gfTempName()
  lcWinCh31   = gfTempName()
  lcWinCh32   = gfTempName()
  lcWinCh33   = gfTempName()
  lcWinCh34   = gfTempName()
  lcWinCh35   = gfTempName()
  lcWinCh4    = gfTempName()
  lcWinCh5    = gfTempName()
  lcWinCh6    = gfTempName()
  lcfolder    = gfTempName()      && Folder Window Name
  lcfoldprnt  = gcBaseWind        && window parent name for the folder
  lnactfolder = 1                 && active folder

  lcWinChrg = IIF(llIsEngland,lcWinCh4,IIF(llIsCanada,lcWinCh5,lcWinCh6))
  lafoldwinds[1,1] = lafolders[1,1]
  lafoldwinds[1,2] = lcWinCh2
  lafoldwinds[2,1] = lafolders[2,1]
  lafoldwinds[2,2] = lcWinCh3
  lafoldwinds[3,1] = lafolders[3,1]
  lafoldwinds[3,2] = lcWinChrg
  *-- get temp names for the files
  lcInvLine = gfTempName()
  lcInvHdr  = gfTempName()
  lcGlDist  = gfTempName()
  lcPackHdr = gfTempName()
  lcInstHdr = gfTempName()
  lcInstLin = gfTempName()
  lcAppCrdt = gfTempName()
  *C102574,1 HBG 04/15/2002 Trigger to GMA [Begin]
  IF ASCAN(laEvntTrig , PADR('INTDVAR',10)) <> 0
    =gfDoTriger('ARDINV',PADR('INTDVAR',10)) 
  ENDIF  
  *C102574,1 [End]
  *C037816,1 MHM 04/06/2004 Define New Variables[Start]
  *IF ASCAN(laEvntTrig , PADR('DLINTVAR',10)) <> 0
  *  =gfDoTriger('ARDINV',PADR('DLINTVAR',10)) 
  *ENDIF  
  *C037816,1 MHM [End]

  *E301204,1 Define name for temp. file using in key off [Begin]
  lcTmpAR   = gfTempName()
  *E301204,1 Define name for temp. file using in key off [End  ]

  lcUpsBox  = IIF(laSetups[15,2]='Y',gfTempName(),'')
  SELECT InvLine
  SCATTER MEMVAR BLANK MEMO
  llNoShow = .F.
  IF TYPE('lcInvoiceNo') = 'C' .AND. SEEK(lcInvoiceNo,'INVHDR')
    SELECT INVHDR
    STORE .F. TO laScrMode
    STORE .T. TO laScrMode[2]
    *E301077,51 Inhance openning files to speed up transaction
    *SCATTER FIELDS &lcScFields TO laData
  ENDIF
  *-- variable used to update the uncomplete session file
  laVars[1]  = 'ldDefInvDate'
  laVars[2]  = 'lcIDefSes'  
  laVars[3]  = 'lcIDefDiv'
  laVars[4]  = 'lcIDefWare'
  laVars[5]  = 'llRebuild'
  laVars[6]  = 'lcGlSession'
  laVars[7]  = 'ldDefPstDate'  
  laVars[8]  = 'llupsinsur'
  laVars[9]  = 'lcScrMode'
  laVars[10] = 'lcCurrInv'
  laVars[11] = 'lnUnCmSeRc'    && Used by Key off program
  laVars[12] = 'lcAdTrnSeq'    && Used by Key off program
  laVars[13] = 'lcAcTrnSeq'    && Used by Key off program
  laVars[14] = 'lcHisSeq'      && Used by Key off program
  laVars[15] = 'lcGlSess'      && Used by Key off program
  laVars[16] = 'lcRepBat'      && Used by Key off program
  laVars[17] = 'llUpdGlDif'    && Used by Key off program
  laVars[18] = 'llUpdMstGL'    && Used by Key off program
  laVars[19] = 'lnShipAddr'
  *B603119,1 NAD (Start) Store  llActCrdt For the uncomplete Session
  laVars[20] = 'llActCrdt'
  *B603119,1 NAD (End)
  * --Payment Terms Code
  laCodes[1,1] = 'CTERMCODE'
  laCodes[1,2] = 'laTerms'
  laCodes[1,3] = 'lnTerms'
  laCodes[1,4] = ''
  laCodes[1,5] = .F.
  laCodes[1,6] = .F.
  laCodes[1,10] = 'cTermCode'
  *-- Payment Terms Related Fields
  laTRltFld[1,1] = 'NTERDISCR'   
  laTRltFld[1,2] = 'laData[29]'  && Trade Discount
  laTRltFld[2,1] = 'EOM'
  laTRltFld[2,2] = 'lcTEOM'      && End of Month Y/N
  laTRltFld[3,1] = 'NTERDUED'
  laTRltFld[3,2] = 'lnTDaysDue'  && Net Due Days
  laTRltFld[4,1] = 'CODYN'       
  laTRltFld[4,2] = 'lcTCod'      && Cash on delivery Y/N
  laTRltFld[5,1] = 'LINSTALLM'
  laTRltFld[5,2] = 'llInstTerm'  && Use Instalment 
  *E301217,1 AMM Add new element to hold the EOM day
  laTRltFld[6,1] = 'EOMDAY'
  laTRltFld[6,2] = 'lnEomDay'    && End Of Month Day
  *E301217,1 AMM end 
  *-- Ship via  Codes
  laCodes[2,1] = 'SHIPVIA'
  laCodes[2,2] = 'laShipVia'
  laCodes[2,3] = 'lnShipVia'
  laCodes[2,4] = ''
  laCodes[2,5] = .F.
  laCodes[2,6] = .F.
  laCodes[2,10] = 'SHIPVIA'
  *--Special instruction Codes
  laCodes[3,1] = 'SPCINST'
  laCodes[3,2] = 'laSpcInst'
  laCodes[3,3] = 'lnSpcInst'
  laCodes[3,4] = ''
  laCodes[3,5] = .F.
  laCodes[3,6] = .F.
  laCodes[3,10] = 'SPCINST'
  *-- Season codes
  laCodes[4,1] = 'SEASON'
  laCodes[4,2] = 'laSeasons'
  laCodes[4,3] = 'lnSeason'
  laCodes[4,4] = ''
  laCodes[4,5] = .T.
  laCodes[4,6] = .F.
  laCodes[4,10] = 'SEASON'
  *-- Devision  codes
  laCodes[5,1] = 'CDIVISION'
  laCodes[5,2] = 'laDivision'
  laCodes[5,3] = 'lnDivision'
  laCodes[5,4] = ''
  laCodes[5,5] = .F.
  laCodes[5,6] = .F.
  laCodes[5,10] = 'cDivision'
  *-- Devision Related Fields 
  laDRltFld[1,1] = 'LINK_CODE'
  laDRltFld[2,1] = 'CSLSGLLINK'

  *E300834,1 Declare discount code related fields array
  laDisRltFld[1,1] = 'DISCPCNT'
  laDisRltFld[1,2] = 'lnDisc_Pcnt'

  laVRltFld[1,1] = 'CUPS'
  laVRltFld[1,2] = 'lcUpsType'
  *-- order cancelation reason codes
  laCodes[6,1] = 'CCANCRESON'
  laCodes[6,2] = 'laCanReason'
  laCodes[6,3] = 'lnCanReason'
  laCodes[6,4] = ''
  laCodes[6,5] = .F.
  laCodes[6,6] = .F.
  laCodes[6,10] = 'CCANCRESON'
ELSE
  DO CASE
    CASE laScrMode[1] 
      STORE 'DISABLE' TO lcpbNoteStat,lcpbObjStat,lcPbSendStat
    CASE laScrMode[2]
      STORE 'ENABLE' TO lcpbNoteStat,lcpbObjStat,lcPbSendStat    
    CASE laScrMode[4]
      STORE 'DISABLE' TO lcpbNoteStat,lcpbObjStat,lcPbSendStat
  ENDCASE  
  SELECT IIF(laScrMode[2],'InvLine',lcInvLine)
  SCATTER MEMVAR BLANK MEMO
ENDIF
llUpsBox  = laSetups[15,2]='Y' .AND. gfOpenFile(gcDataDir+'UPSBOX',gcDataDir+'UPSBOX','SH')
llRep_div = laSetups[14,2]='D' .AND. gfOpenFile(gcDataDir+'REP_DIV',gcDataDir+'REP_DIV','SH')




*E301077,51 Inhance openning files to speed up transaction
*llInvChrg = llIsEngland .AND. gfOpenFile(gcDataDir+'InvChrg',gcDataDir+'InvChrg','SH')
*llSpck_HDr= laSetups[1,2] ='Y' .AND. gfOpenFile(gcDataDir+'SPck_Hdr',gcDataDir+'SPck_Hdr','SH')
*llSPck_Lin= laSetups[1,2] ='Y' .AND. gfOpenFile(gcDataDir+'SPck_Lin',gcDataDir+'SPck_Lin','SH')
*llGLDIST  = laSetups[4,2]='Y' .AND. gfOpenFile(gcDataDir+'GLDIST', gcDataDir+'GLDISTNO', 'SH')
*llSycCurr = llMulCurr .AND. gfOpenFile(gcSysHome+'SycCurr',gcSysHome+'cCurrCode','SH')
*E301077,51 (End)

*E301077,51 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
*E301077,51 Inhance openning files to speed up transaction

SELECT LEFT(cDesc,20),cWareCode FROM WAREHOUS INTO ARRAY laWareHouses
*E300889,1 (Start) Initialize variable holds the system type setup.
llPOSSetup = ('CM' $ gcComp_Mdl) AND (gfGetMemVar('M_SYSTYPE') = 'P')
*-- if the communication module was installed and system type = Point of sale
IF llPOSSetup
  *-- get the ware house for the current Point of sale
  SELECT WAREHOUS
  LOCATE FOR cSiteId = gcCurSite
  lcPOSWrHs = WAREHOUS.cWareCode
  lnPOSWeHs = ASUBSCRIPT(laWareHouses,ASCAN(laWareHouses,lcPOSWrHs),1)
  lnWareHouse = lnPOSWeHs
ENDIF
*E300889,1 (End).

*E301077,51 Inhance openning files to speed up transaction
=gfCloseFile('WAREHOUS')
*E301077,51 Inhance openning files to speed up transaction

=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

lcKeyBmp  = gcBmpHome + "ExtKey.BMP"
lcNew     = gcBmpHome + "new1.bmp"
lcNotes   = gcBmpHome + "NOTES1.BMP"
lcPacks   = gcBmpHome + "packs.bmp"
lcRemove  = gcBmpHome + "rem1.bmp"
lcClose   = gcBmpHome + "Close2.bmp"

*E301176,1 HDM 03/22/1999[Start] Prevent programs from displaying notepad icon
*                           as it's now controlled globally
*DECLARE laPanelObj[3,3]
DECLARE laPanelObj[2,3]

STORE '' TO laPanelObj
*laPanelObj[1,1] = 'pbInvNote'
*laPanelObj[1,2] = gcBmpHome+'NOTES2.BMP'
*laPanelObj[1,3] = [VALID lfvInvNote() MESSAGE 'Invoice Notes' &lcpbNoteStat ]
laPanelObj[1,1] = 'pbObjLink'
laPanelObj[1,2] = gcBmpHome+"Relate.bmp"
laPanelObj[1,3] = [VALID lfvObjLink() MESSAGE 'Object Link' &lcpbObjStat ]
laPanelObj[2,1] = 'pbSend'
laPanelObj[2,2] = gcBmpHome+"send2.bmp"
laPanelObj[2,3] = [VALID lfvSend() MESSAGE 'Send EDI Invoice File' &lcPbSendStat]
*E301176,1 HDM 03/22/1999[End]

DEFINE BAR 10 OF P03PU03 PROMPT '\<Void' SKIP FOR !(laScrMode[2] .AND. INVHDR.STATUS<>'V')

*E301077,51 Inhance openning files to speed up transaction
*SELECT STYLE
*SET RELATION TO 'S'+SCALE INTO SCALE
*E301077,51 (End)

*B603119,1 (Start) Variable to know if the files used for the uncomplete
*B603119,1             Was opened or not
llOpnFiles=.T.
*B603119,1 (End)

SELECT INVHDR
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER

*E301386,1 (Start) Variable to hold the name of the field that can be edited by the user
*E301386,1   or any screen field to enable it
lcEdtFld='SHIPDATE,DUEDATE,DEPT,ibDepart,NOTE1,NOTE2,lnSpcinst,SPCINST'
*E301386,1 (End)

*C101796,1  (Start)  Custom Process for GMA to Edit more fields ine the invoice   
=gfDoTriger('ARDINV','EDTINV')
*C101796,1   (End)

DO (gcScrDir+gcWinAppl+"\ARDINV.SPX")
=lfClearKey()
DEFINE BAR 10 OF P03PU03 PROMPT '\<Delete' SKIP FOR .T.
IF WEXIST(lcBrTtlZ)
  HIDE WINDOW (lcBrTtlZ)
  RELEASE WINDOW (lcBrTtlZ)
ENDIF
IF WEXIST(lcInvBrow)
  HIDE WINDOW (lcInvBrow)
  RELEASE WINDOW (lcInvBrow)
ENDIF
IF glQuitting
  IF USED(lcInvLine)
    USE IN (lcInvLine)
  ENDIF
  ERASE (gcWorkDir+lcInvLine+".DBF")
  ERASE (gcWorkDir+lcInvLine+".CDX")
  ERASE (gcWorkDir+lcInvLine+".FPT")
  IF USED(lcInvHdr)
    USE IN (lcInvHdr)
  ENDIF
  ERASE (gcWorkDir+lcInvHdr+".DBF")
  ERASE (gcWorkDir+lcInvHdr+".CDX")
  IF USED(lcGlDist)
    USE IN (lcGlDist)
  ENDIF
  ERASE (gcWorkDir+lcGlDist+".DBF")
  IF USED(lcInstHdr)
    USE IN (lcInstHdr)
  ENDIF
  ERASE (gcWorkDir+lcInstHdr+".DBF")
  ERASE (gcWorkDir+lcInstHdr+".CDX")
  IF USED(lcInstLin)
    USE IN (lcInstLin)
  ENDIF
  ERASE (gcWorkDir+lcInstLin+".DBF")
  ERASE (gcWorkDir+lcInstLin+".CDX")
  IF USED(lcAppCrdt)
    USE IN (lcAppCrdt)
  ENDIF
  ERASE (gcWorkDir+lcAppCrdt+".DBF")
  ERASE (gcWorkDir+lcAppCrdt+".CDX")
  IF USED(lcUpsBox)
    USE IN (lcUpsBox)
  ENDIF
  ERASE (gcWorkDir+lcUpsBox+".DBF")
  ERASE (gcWorkDir+lcUpsBox+".CDX")

  *E301204,1 Close key off File [Begin]
  IF USED(lcTmpAR)
    USE IN (lcTmpAR)
    ERASE (gcWorkDir+lcTmpAR+'.DBF')
    ERASE (gcWorkDir+lcTmpAR+'.CDX')
  ENDIF
  *E301204,1 Close key off File [End  ]

ENDIF


SELECT INVHDR
SET RELATION TO
SELECT INVLINE
SET RELATION TO

*E301077,51 Inhance openning files to speed up transaction
*SELECT STYLE
*SET RELATION TO
*IF llSpck_HDr
*  USE IN SPck_Hdr
*ENDIF
*IF llSPck_Lin
*  USE IN SPck_Lin
*ENDIF
*IF llGLDIST
*  USE IN GLDIST
*ENDIF
*IF llSycCurr
*  USE IN SycCurr
*ENDIF
*IF llInvChrg 
*  USE IN InvChrg
*ENDIF
*E301077,51 (End)

IF llUpsBox
  USE IN UPSBOX
ENDIF
IF llRep_div
  USE IN REP_DIV
ENDIF
RELEASE PAD _INQUIRY OF _MSYSMENU
  
*!*************************************************************
*! Name      : lfActFolder
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Change folders
*!*************************************************************
*! Calls     : None.
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
    SHOW GETS WINDOW (lcWinCh30) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh32) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh34) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh35) DISABLE ONLY
    SHOW GETS WINDOW (lcWinChrg) DISABLE ONLY
    lcStatus = IIF(laScrMode[4],'ENABLE','DISABLE')
    SHOW GETS WINDOW (lcWinCh2) &lcStatus ONLY
    
    lcLines = IIF(laScrMode[4] .AND. EOF(lcInvLine),'ENABLE','DISABLE')
    IF lnShipAddr = 1
      SHOW GET lcShipName DISABLE
      SHOW GET lcShipAdd1 DISABLE
      SHOW GET lcShipAdd2 DISABLE
      SHOW GET lcShipAdd3 DISABLE
      SHOW GET lcShipAdd4 DISABLE
      SHOW GET lcShipAdd5 DISABLE
    ENDIF
    SHOW GET lnSeason   &lcLines
    SHOW GET lnDivision &lcLines
    SHOW GET lnWareHouse &lcLines
    SHOW GET ibCurrency &lcLines
    SHOW GET laData[22] &lcLines
    *-- control the status of the currency in the screen due to edit currency or not
    IF !llEditExRt OR laData[22]=gcBaseCurr
      SHOW GET laData[23] DISABLE
    ELSE
      SHOW GET laData[23] &lcLines
    ENDIF
    lcSalesStat = IIF(laScrMode[4] AND laSetups[2,2]<>'Y','ENABLE','DISABLE')
    *B131175,1 MMR 03/28/2006 Fix bug of make sales disable if edit comm per line setup=yes.[Start]
    *SHOW GET ibSaleRep1 &lcSalesStat
    *SHOW GET laData[17] &lcSalesStat  && sales rep1
    *B131175,1 MMR.[END]
    SHOW GET laData[18] &lcSalesStat  && sales rep1 Percent
    *B131175,1 MMR 03/28/2006 Fix bug of make sales disable if edit comm per line setup=yes.[Start]
    *SHOW GET ibSaleRep2 &lcSalesStat  
    *SHOW GET laData[19] &lcSalesStat  && sales rep2
    *B131175,1 MMR.[END]
    SHOW GET laData[20] &lcSalesStat  && sales rep2 Percent
    *E301386,1 (Start) Add if condition to handle the edit mode
    IF  laScrMode[3]      
      SHOW GET laData[5] DISABLE
      SHOW GET ibStore DISABLE 
      =lfEdtFld(lcEdtFld) 
     
      SHOW GETS WINDOW (lcWinChrg) DISABLE ONLY   
    ENDIF
   *E301386,1 (End) 
    
  CASE lnActFolder = 2
    
    
    SHOW GETS WINDOW (lcWinCh2)  DISABLE ONLY
    SHOW GETS WINDOW (lcWinChrg) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh30) ENABLE  ONLY
    *E301386,1 (Start) add the edit mode to the if condition
    *SELECT IIF(laScrMode[2],'InvLine',lcInvLine)
    SELECT IIF(laScrMode[2] OR laScrMode[3],'InvLine',lcInvLine)
    *E301386,1 (End) 
    =IIF(llReBrowse,lfBrowDet(),.T.)
    IF !llZoom
      lcStatus = IIF(laScrMode[4] .AND. !EMPTY(Style),'ENABLE','DISABLE')
      SHOW GETS WINDOW (lcWinCh32) &lcStatus ONLY
      SHOW GETS WINDOW (lcWinCh34) &lcStatus ONLY
      SHOW GETS WINDOW (lcWinCh35) &lcStatus ONLY
      IF laScrMode[4]
        SHOW GET pbNew   ENABLE 
        SHOW GET pbPacks ENABLE
      ELSE
        SHOW GET pbNew   DISABLE
        SHOW GET pbPacks DISABLE
      ENDIF
      IF !EMPTY(Style)
        SHOW GET pbLineNote ENABLE
      ELSE
        SHOW GET pbLineNote DISABLE
      ENDIF
      IF laScrMode[4] .AND. laSetups[2,2]='Y' AND !EMPTY(laData[17])
        SHOW GET m.Comm1 ENABLE
      ELSE
        SHOW GET m.Comm1 DISABLE
      ENDIF
      IF laScrMode[4] .AND. laSetups[2,2]='Y' AND !EMPTY(laData[19])
        SHOW GET m.Comm2 ENABLE
      ELSE
        SHOW GET m.Comm2 DISABLE
      ENDIF
      _CUROBJ = IIF(EMPTY(STYLE),OBJNUM(pbNew),OBJNUM(ibInv200E))
    ENDIF
    
  CASE lnActFolder = 3
    =gfRltFld(laData[10],@laVRltFld,'SHIPVIA') 
    =IIF(laScrMode[4] AND !llIsEngland AND &lcInvHDr..lCompUps ,lfvUpsChrg(),.T.)
    =IIF(laScrMode[4] AND llIsEngland AND llReMerTax,lfMechTax(),.T.)
    lcUpsTrack = IIF(SUBSTR(lcUpsType,1,5)='USUPS','UPS COD Tracking#','COD Tracking#')
    lcLStatus  = IIF(laScrMode[4] .AND. laData[31]>0,'ENABLE','DISABLE')
    lcStatus   = IIF(laScrMode[4],'ENABLE','DISABLE')
    IF SUBSTR(lcUpsType,1,5)='USUPS'
      SHOW GET llUpsInsur,1 PROMPT 'UPS Insur.'
    ELSE
      SHOW GET llUpsInsur,1 PROMPT 'Insurance '
    ENDIF
    SHOW GETS WINDOW (lcWinChrg) &lcStatus ONLY
    SHOW GETS WINDOW (lcWinCh2)  DISABLE   ONLY
    SHOW GETS WINDOW (lcWinCh30) DISABLE   ONLY
    SHOW GETS WINDOW (lcWinCh32) DISABLE   ONLY
    SHOW GETS WINDOW (lcWinCh34) DISABLE   ONLY
    SHOW GETS WINDOW (lcWinCh35) DISABLE   ONLY
    SHOW GET laData[25] &lcLStatus
    SHOW GET laData[26] &lcLStatus
    SHOW GET laData[34] &lcLStatus
    SHOW GET laData[35] &lcLStatus
    SHOW GET laData[36] &lcLStatus
    SHOW GET laData[28] &lcLStatus
    SHOW GET laData[45] &lcLStatus
    SHOW GET laData[46] &lcLStatus
    *C102212,1 (Begin) Show get HST tax fileds if Use Tax 'Y' and Use HST Tax 'Y'.
    SHOW GET laData[51] &lcLStatus
    SHOW GET laData[52] &lcLStatus
    *C102212,1 (End)
    *B803397,1 (Start) Add the edit mode to the if condition.
    *IF laScrMode[2] .AND. llIsEngland
    IF (laScrMode[2] OR laScrMode[3]) .AND. llIsEngland
      SHOW GET pbCharges ENABLE
    ENDIF
    *B803397,1 (End)
    *B801953,1 NAD (Start)  If UPS type not equal USUPS Disable the UPSBOX Push buttom
    *IF !laScrMode[1] .AND. !llIsEngland .AND. laData[25] > 1
    IF !laScrMode[1] .AND. !llIsEngland .AND. laData[25] > 1 AND SUBSTR(lcUpsType,1,5) = "USUPS"
    *B801953,1 NAD (END)
      SHOW GET pbUpsBox ENABLE
    ELSE
      SHOW GET pbUpsBox DISABLE
    ENDIF
    *C102239,1 (Begin) Always enable Cartns button for BEL05.
    IF !laScrMode[1] .AND. !llIsEngland .AND. !EMPTY(InvHdr.PikTkt) AND ASCAN(laEvntTrig , PADR('ENBLCAR',10)) <> 0
       =gfDoTriger('ARDINV',PADR('ENBLCAR',10))
    ENDIF  
    *C102239,1 (End)
    IF laSetups[9,2] <> 'Y'
      SHOW GET lnPstRule  DISABLE
      SHOW GET laData[45] DISABLE
      SHOW GET laData[46] DISABLE
      *C102212,1 (Begin) Show get HST tax fileds if Use Tax 'Y'.
      SHOW GET laData[51] DISABLE
      *C102212,1 (End)
    ENDIF

    *E302472,1 MHM 11/18/2007  Multiple Taxes for Canada on Invoice [Start]
    IF llIsCanada AND LASCRMODE[4]
      llRpHSTTax = .F.
      =lfGetHST()
      IF !(laData[51] <> 0 AND laData[45] = 0) 
        IF llRpHSTTax
          laData[51] = laData[45]
          laData[52] = laData[43]
          laData[37] = 0
          laData[43] = 0
          laData[45] = 0
          laData[46] = 0
        
          SHOW GET laData[37] DISABLE
          SHOW GET laData[43] DISABLE
          SHOW GET laData[45] DISABLE
          SHOW GET laData[46] DISABLE
          SHOW GET laData[51] ENABLE
          SHOW GET laData[52] DISABLE
        ELSE
          laData[52] = 0
          laData[51] = 0
          SHOW GET laData[37] ENABLE
          SHOW GET laData[43] ENABLE
          SHOW GET laData[52] DISABLE
          SHOW GET laData[51] DISABLE
        ENDIF
      ELSE
        SHOW GET laData[37] DISABLE
        SHOW GET laData[43] DISABLE
        SHOW GET laData[45] DISABLE
        SHOW GET laData[46] DISABLE
        SHOW GET laData[51] ENABLE
        SHOW GET laData[52] DISABLE
      ENDIF
      
    ENDIF
    *E302472,1 [End
    
    *B602760,1 NAD (Start) Control object enable and disable in the screen
    *IF laScrMode[4] AND laSetups[9,2]='Y' AND laData[46]=0 AND ;
    *   IIF(UPPER(ALLTRIM(gcContCode))='USA',lnTaxDue>0,laData[31]>0)
    * SHOW GET laData[37] ENABLE
    *ELSE
    *  SHOW GET laData[37] DISABLE
    *ENDIF
    *IF laScrMode[4] AND laData[31]>0 AND laSetups[9,2]='Y' AND laData[45]=0
    * SHOW GET laData[43] ENABLE
    *ELSE
    *  SHOW GET laData[43] DISABLE
    *ENDIF
    *IF laScrMode[4] .AND. laData[31]>0 .AND. laData[32] = 0
    *  SHOW GET laData[33] ENABLE
    *ELSE
    *  SHOW GET laData[33] DISABLE
    *ENDIF
    *B602760,1 NAD (End)
    llOpnCrdt = .F.
    IF laScrMode[4] AND llCod AND laData[38] > 0 
      SHOW GET laData[27] ENABLE
      SHOW GET laData[28] ENABLE
      llOpnCrdt = llActCrdt
    ELSE
      STORE SPACE(6) TO laData[27]
      SHOW GET laData[27] DISABLE
      SHOW GET laData[28] DISABLE
    ENDIF
        
    IF llOpnCrdt
      SHOW GET pbCredits ENABLE
    ELSE
      SHOW GET pbCredits DISABLE
    ENDI
    IF !laScrMode[1] AND laData[38] > 0 AND llInstTerm
      SHOW GET pbInvInst ENABLE
    ELSE
      SHOW GET pbInvInst DISABLE
    ENDIF
    *E301386,1 (Start) Add if condition to handle the edit mode
    IF  laScrMode[3]      
      SHOW GET laData[5] DISABLE
      SHOW GET ibStore DISABLE 
      =lfEdtFld(lcEdtFld) 
       SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY    
    ENDIF
    *E301386,1 (End)
    =lfRefresh(lcWinChrg)
ENDCASE
RETURN

*!*************************************************************
*! Name      : lfBrowDet
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Browse Invoice lines
*!*************************************************************
*! Calls     : lfBrowDet
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfBrowDet()
*!*************************************************************
FUNCTION lfBrowDet
PRIVATE lcFields

lcSelect = SELECT()
*E301386,1 (Start) Add the edit mode to the if condition
*SELECT IIF(laScrMode[2],'INVLINE',lcInvLine)
*=SEEK(IIF(laScrMode[2],laData[1],laData[2]+laData[3]+laData[5]+laData[30]))

SELECT IIF(laScrMode[2] OR laScrMode[3],'INVLINE',lcInvLine)
=SEEK(IIF(laScrMode[2] OR laScrMode[3] ,laData[1],laData[2]+laData[3]+laData[5]+laData[30]))
*E301386,1 (End) 

lnMarker = RECNO()
lcFields = "cMarker=IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,Style :H=lcStyHdr :R,"
lcFields = lcFields + IIF(laSetups[8,2]='Y',"Dyelot :R,",'')
*B602278,1 Elimenate price roundation
*B602446,1 Elimenate Ship amount roundation
*lcFields = lcFields +;
           "Group :H='Group' :R,PrePak :H='Prepack' :R,Gros_Price :H='Gross Price' :R,"+;
           "Disc_Pcnt :H='Disc. Pcnt.' :R,Price :H='Net Price' :R,TotQty :H='Quantity' :R,"+;
           "lnAmount=PRICE*TOTQTY :12:H='Ship Amount':R"
*lcFields = lcFields +;
           "Group :H='Group' :R,PrePak :H='Prepack' :R,Gros_Price :H='Gross Price' :R,"+;
           "Disc_Pcnt :H='Disc. Pcnt.' :R,Price :H='Net Price' :R,TotQty :H='Quantity' :R,"+;
           "lnAmount=(100-Disc_Pcnt)*Gros_Price*TOTQTY/100 :P='9999999999.99':H='Ship Amount':R"
lcFields = lcFields +;
           "Group :H='Group' :R,PrePak :H='Prepack' :R,Gros_Price :H='Gross Price' :R,"+;
           "Disc_Pcnt :H='Disc. Pcnt.' :R,Price :H='Net Price' :R,TotQty :H='Quantity' :R,"+;
           "lnAmount=PRICE*TOTQTY :12:H='Ship Amount':R"
*B602446,1 (End)
*B602278,1 (End)

IF WEXIST(lcBrTtlZ)
  HIDE WINDOW (lcBrTtlZ)
  RELEASE WINDOW (lcBrTtlZ)
ENDIF
IF WEXIST(lcInvBrow)
  HIDE WINDOW (lcInvBrow)
  RELEASE WINDOW (lcInvBrow)
ENDIF
IF llZoom
  HIDE WINDOW (lcWinCh32),(lcWinCh34),(lcWinCh35) SAME
  SHOW GETS WINDOW (lcWinCh32) DISABLE ONLY
  SHOW GETS WINDOW (lcWinCh34) DISABLE ONLY
  SHOW GETS WINDOW (lcWinCh35) DISABLE ONLY
 *E301386,1 (Start) in case of edit mode the key is invoice number
 *BROWSE FIELDS &lcFields ;
         WINDOW (lcWinCh33)  ;
         IN WINDOW (lcWinCh3);
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
	     NOCLEAR           ;
	     KEY IIF(laScrMode[2],laData[1],laData[2]+laData[3]+laData[5]+laData[30])  ;
         WHEN lfwBrow()    ;
         TITLE lcBrTtlZ 
  *B603727,1 NAD 09/17/2000 Remove the key from the browse (in add mode) to give the user
  *B603727,1                the ability to change the store.       
  *BROWSE FIELDS &lcFields ;
         WINDOW (lcWinCh33)  ;
         IN WINDOW (lcWinCh3);
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
	     NOCLEAR           ;
	     KEY IIF(laScrMode[2] OR laScrMode[3],laData[1],laData[2]+laData[3]+laData[5]+laData[30])  ;
         WHEN lfwBrow()    ;
         TITLE lcBrTtlZ
                  
  BROWSE FIELDS &lcFields ;
         WINDOW (lcWinCh33)  ;
         IN WINDOW (lcWinCh3);
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
	     NOCLEAR           ;
	     KEY IIF(laScrMode[2] OR laScrMode[3],laData[1],'')  ;
	     WHEN lfwBrow()    ;
         TITLE lcBrTtlZ 
  
  *B603727,1 NAD (End)              
 
 
 *E301386,1 (End)     
ELSE
  SHOW WINDOW (lcWinCh32),(lcWinCh34),(lcWinCh35) TOP
  *E301386,1 (Start) in case of edit mode the key is invoice number
  *BROWSE FIELDS &lcFields ;
         WINDOW (lcWinCh31)  ;
         IN WINDOW (lcWinCh3);
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
	     NOCLEAR           ;
  	     KEY IIF(laScrMode[2],laData[1],laData[2]+laData[3]+laData[5]+laData[30])  ;
         WHEN lfwBrow()    ;
         TITLE lcInvBrow
  *B603727,1 NAD 09/17/2000 Remove the key from the browse (in add mode) to give the user
  *B603727,1                the ability to change the store.             
  *BROWSE FIELDS &lcFields ;
         WINDOW (lcWinCh31)  ;
         IN WINDOW (lcWinCh3);
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
	     NOCLEAR           ;
  	     KEY IIF(laScrMode[2] OR laScrMode[3],laData[1],laData[2]+laData[3]+laData[5]+laData[30])  ;
         WHEN lfwBrow()    ;
         TITLE lcInvBrow   
  *E301386,1 (End)    
  
  BROWSE FIELDS &lcFields ;
         WINDOW (lcWinCh31)  ;
         IN WINDOW (lcWinCh3);
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
	     NOCLEAR           ;
  	     KEY IIF(laScrMode[2] OR laScrMode[3],laData[1],'')  ;
  	     WHEN lfwBrow()    ;
         TITLE lcInvBrow   
*B603727,1 NAD (End)    
ENDIF
*C102574,1 HBG 04/15/2002 IF Trigger to GMA create And user choose to Show lines by Pack[Begin]
IF ASCAN(laEvntTrig , PADR('BROWFLD',10)) <> 0
  =gfDoTriger('ARDINV',PADR('BROWFLD',10))
ENDIF
*C102574,1 [End]

=lfwBrow()
llReBrowse = .F.
SELECT (lcSelect)

*!*************************************************************
*! Name      : lfwBrow
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
*! Example   :  =lfwBrow()
*!*************************************************************
FUNCTION lfwBrow

*B038154,1  TMI [Start] Always check the relation into style then into scale file exist
IF !'INTO STYLE' $ SET('RELATION')
  SET RELATION TO STYLE INTO STYLE
  SELECT STYLE
  IF !'INTO SCALE' $ SET('RELATION')
    SET RELATION TO 'S'+SCALE INTO SCALE
  ENDIF
  SELECT IIF(laScrMode[2],('InvLine'),(lcInvLine))
ENDIF
*B038154,1  TMI [End  ] 

lnMarker = IIF(laScrMode[2],RECNO('InvLine'),RECNO(lcInvLine))
IF llZoom
  SHOW WINDOW (lcBrTtlZ) REFRESH SAME
ELSE  
  SHOW WINDOW (lcInvBrow) REFRESH SAME  
  SCATTER MEMVAR MEMO
  SHOW GETS WINDOW (lcWinCh32) ONLY
  SHOW GETS WINDOW (lcWinCh34) ONLY
  SHOW GETS WINDOW (lcWinCh35) ONLY
  =lfRefresh(lcWinCh32)
  =lfRefresh(lcWinCh34)
  IF laScrMode[4] .AND. laSetups[2,2]='Y' AND !EMPTY(laData[17])
    SHOW GET m.Comm1 ENABLE
  ELSE
    SHOW GET m.Comm1 DISABLE
  ENDIF
  IF laScrMode[4] .AND. laSetups[2,2]='Y' AND !EMPTY(laData[19])
    SHOW GET m.Comm2 ENABLE
  ELSE
    SHOW GET m.Comm2 DISABLE
  ENDIF
  
  *B602760,1 NAD (Start) Disable Style Prepack code and quantity if style scale does not have prepacks
  IF laScrMode[4]
    =lfPpStat()
  ENDIF
  *B602760,1 NAD (Start) Control object enable and disable in the screen
  *IF laScrMode[4] .AND. !EMPTY(m.Prepak)
  *  SHOW GET m.PPQty ENABLE
  *ELSE
  *  SHOW GET m.PPQty DISABLE
  *ENDIF
  *B602760,1 NAD (End)
  *C102574,1 HBG 04/15/2002 If GMA Trigger and the line is from Pack, and
  *C102574,1                this pack is Range Disable the Qty fields [Begin]
  IF ASCAN(laEvntTrig , PADR('SHOWGET',10)) <> 0
    =gfDoTriger('ARDINV',PADR('SHOWGET',10))
    =gfDoTriger('ARDINV',PADR('DISBLFlD',10))
  ENDIF
  *C102574,1 [End]

ENDIF

*!*************************************************************
*! Name      : lfDARDINV
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : READ Deactivate function of screen ARDINV
*!*************************************************************
*! Calls     : lpTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfDARDINV()
*!*************************************************************
FUNCTION lfDARDINV

IF laScrMode[4] AND WLAST()=(lcWinCh34) AND !lfChkTotQty()
  ACTIVATE WINDOW (lcWinCh34)
  _CUROBJ = OBJNUM(m.Qty1)
  RETURN .F.
ENDIF

SET SKIP OF PAD _INQUIRY OF _MSYSMENU laScrMode[1]
IF WONTOP()=lcInvBrow .OR. WONTOP()=lcBrTtlZ
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  glFromBrow = .T.
  ON KEY LABEL TAB DO lpTab WITH ;
  IIF(WONTOP()=lcInvBrow,lcWinCh32,'gwcContrl1'),;
  IIF(WONTOP()=lcInvBrow,'m.Style','pbTop')
  ON KEY LABEL BACKTAB DO lpBackTab WITH lcFolder,'ibFolder[2]'
ELSE
  glFromBrow = .F.
ENDIF

IF laScrMode[4]  .AND. llNewLine .AND. ;
  !(WONTOP()=(lcWinCh32) OR WONTOP()=(lcWinCh34) OR WONTOP()=(lcWinCh35))
  SELECT (lcInvLine)
  SCATTER MEMVAR MEMO
  STORE .F.        TO llAddLine,llNewLine
  STORE lcEmptySty TO lcLastStyle
  
  IF EMPTY(m.Style)
    SHOW GETS WINDOW (lcWinCh32) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh34) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh35) DISABLE ONLY
    SHOW GET pbNew     ENABLE
    SHOW GET ibInv200E ENABLE
    SHOW GET pbPacks ENABLE
  ELSE
    SHOW GETS WINDOW (lcWinCh32) ENABLE ONLY
    SHOW GETS WINDOW (lcWinCh34) ENABLE ONLY
    SHOW GETS WINDOW (lcWinCh35) ENABLE ONLY
  ENDIF
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
*! Example   :  DO lpTab WITH 'ARDINV0', 'pbClose'
*!*************************************************************
PROCEDURE lpTab
PARAMETERS lcWindName, lcObjName

ON KEY LABEL TAB 
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)

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
*! Example   :  DO lpBackTab WITH 'ARDINV0', 'pbClose'
*!*************************************************************
PROCEDURE lpBackTab
PARAMETERS lcWindName, lcObjName

ON KEY LABEL BACKTAB
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)

*!*************************************************************
*! Name      : lfvInvNote
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : iNVOICE notepad
*!*************************************************************
*! Calls     : NOTEPAD
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvInvNote()
*!*************************************************************
FUNCTION lfvInvNote
=NOTEPAD('C',laData[1])

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
DO GetObj WITH 'I',laData[1]
SELECT (lnAlais)

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
PRIVATE laCanReason,lnCanReason

DECLARE laCanReason[1,2]
STORE '' TO laCanReason
STORE 1  TO lnCanReason
=gfwCodePop(@laCodes,'CCANCRESON','L')
DO (gcScrDir+"ARORDER.SPX") WITH 'X','lnCanReason'
RETURN(ALLTRIM(laCanReason[lnCanReason,2]))

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
DO (gcScrDir+"ARORDER.SPX") WITH 'O','lcOrderNo'
RETURN(lcOrderNo)

*!*************************************************************
*! Name      : lpDelScr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Void Invoice
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvVoid()
*!*************************************************************
FUNCTION lpDelScr

*B606392,4 Check if invoice is voided. [Begin]
IF InvHdr.Status = 'V'
  *-- Message : 40004
  *-- Invoice#  has been Voided.
  *-- Button : 00000 
  *-- Ok  
  =gfModalGen('INM40183B00000','ALERT',laData[1])
  RETURN
ENDIF  
*B606392,4 Check if invoice is voided. [End]

PRIVATE lcInvYear,lcInvPeriod,lcInvLines

*E301077,51 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'icStyHst', gcDataDir+'Styhst','SH')
=gfOpenFile(gcDataDir+'arCusHst', gcDataDir+'Acthst','SH')
=gfOpenFile(gcDataDir+'DEBIT', gcDataDir+'DEBIT','SH')
=gfOpenFile(gcDataDir+'REPCOMM', gcDataDir+'REPCOMM','SH')
IF InvHdr.CONSOL = 'Y' 
  =gfOpenFile(gcDataDir+'CONSINVH', gcDataDir+'CONSINVH','SH')
  =gfOpenFile(gcDataDir+'CONSINVL', gcDataDir+'CONSINVL','SH')
ENDIF
*E301077,51 (End)

*B802575,1 open ARHIST file
=gfOpenFile(gcDataDir+'ARHIST', gcDataDir+'ARHISTT','SH')
*B802575,1 end

*E301204,1 Open History and Credit files [Begin]
IF EMPTY(laData[24]) OR laSetups[17,2]='Y'
*-- if there is no factor or post factored invoice to customer = Y
  *B802575,1 Move opening ARHIST above the IF condition
  *=gfOpenFile(gcDataDir+'ARHIST', gcDataDir+'ARHISTT','SH')
  *B802575,1 end
  =gfOpenFile(gcDataDir+'CREDIT', gcDataDir+'CREDIT','SH')
ENDIF
*E301204,1 Open History and Credit files [End  ]

*B606702,1 Make program checks for payment anyway. [Begin]
*IF laSetups[17,2]='Y' AND EMPTY(laData[24])
*B606702,1 Make program checks for payment anyway. [End] 

  *E301204,1 Now open History file is in the previous separate code [Begin]
  *E301077,51 Inhance openning files to speed up transaction
  *=gfOpenFile(gcDataDir+'ARHIST', gcDataDir+'ARHISTT','SH')
  *E301204,1 Now open History file is in the previous separate code [End  ]

  SELECT ARHIST
  =SEEK(laData[2]+laData[1])
  *--  Check if the invoice was paid or not   
  LOCATE REST WHILE Account+Tran+cInstalNo = laData[2]+laData[1] FOR TranType='1'
  llPaid = FOUND()
  *E301077,51 Inhance openning files to speed up transaction
  
  *E301204,1 Now close History file is in the end of function [Begin]
  *=gfCloseFile('ARHIST')
  *E301204,1 Now close History file is in the end of function [End  ]

  *E301077,51 (End)
  SELECT INVHDR
  IF llPaid
    *E300408,1 Message : 40130
    *E300408,1 This invoice has already been paid. Cannot void.
    *E300408,1 Button : 00000 
    *E300408,1 Ok
    =gfModalGen('TRM40130B00000','ALERT','')    
    RETURN
  ENDIF  

*B606702,1 Make program checks for payment anyway. [Begin]
*ENDIF
*B606702,1 Make program checks for payment anyway. [End]

IF 'RM' $ gcCmpModules
*-- if the return mechendise module installed 
  llRetLine = gfOpenFile(gcDataDir+'RETLINE',gcDataDir+'RETLINEI','SH')

  *B602295,1 Void invoices have Voided Credit Memo
  *llRetInv = SEEK(laData[2]+laData[1],'RETLINE')
  llRetHdr  = gfOpenFile(gcDataDir+'RETHDR',gcDataDir+'RETHDR','SH')
  *-- seek for the invoice  in the return merchandise files 
  llRetInv  = SEEK(laData[2]+laData[1],'RETLINE') AND ;
              SEEK(RETLINE.CrMemo,'RETHDR') AND RETHDR.Status <> 'V'
  IF llRetHdr
    USE IN RETHDR
  ENDIF
  *B602295,1 (End)
  
  IF llRetLine
    USE IN RETLINE
  ENDIF
  SELECT INVHDR
  IF llRetInv
    *E300408,1 Message : 40134
    *E300408,1 Return merchandise invoice. Cannot void
    *E300408,1 Button : 00000 
    *E300408,1 Ok
    =gfModalGen('TRM40134B00000','ALERT','')
    RETURN
  ENDIF
ENDIF


*B802236 (Start)
PRIVATE lcVDateWin,ldVDate,ldOldDate,llVoid
lcVDateWin = gfTempName()
STORE gdSysDate TO ldVDate,ldOldDate
llVoid = .F.
*-- get the void date from the user
DO (gcScrDir+gcWinAppl+"\ARVDATE.SPR")
IF !llVoid
  RETURN
ENDIF  
*B802236 (End)

*B802236 (Start)
*IF !llSysDate
*  IF CHECKPRD(gdSysDate,'lcSysYear','lcSysPeriod','VI1')
*    llSysDate = .T.
*  ELSE
*    RETURN
*  ENDIF
*ENDIF
*IF laSetups[4,2]='Y' .AND. INVHDR.dPostDate > gdSysDate
*  *E300408,1 Message : 40086
*  *E300408,1 invoice date cannot be greater than posting date
*  *E300408,1 Button : 00000 
*  *E300408,1 Ok
*  =gfModalGen('TRM40086B00000','ALERT','')
*  RETURN
*ENDIF
*-- check if the void data is valid or not
IF !CHECKPRD(ldVDate,'lcSysYear','lcSysPeriod','VI1')
  RETURN
ENDIF

*B802236 (End)

STORE '' TO lcInvYear,lcInvPeriod

*B123016,1 NNA 05/30/2004 (Begin) Check the Void date not the invoice date to see if this period locked or not
*IF !CHECKPRD(INVHDR.dPostDate,'lcInvYear','lcInvPeriod','VI2')
IF !CHECKPRD(ldVDate,'lcInvYear','lcInvPeriod','VI2')
*B123016,1 NNA (End)

  RETURN
ENDIF
IF !SEEK(InvHdr.Invoice,'InvLine')
  *E300408,1 Message : 40002
  *E300408,1 invoice lines are not found
  *E300408,1 Button : 00000 
  *E300408,1 Ok
  =gfModalGen('INM40002B00000','ALERT','')
  RETURN
ENDIF
IF InvHdr.CONSOL = 'Y' .AND. (!SEEK(InvHdr.Invoice,'CONSINVH') OR ;
   !SEEK(InvHdr.Invoice,'CONSINVL') )
  *E300511,1 Message : 40002
  *E300408,1 invoice lines are not found
  *E300408,1 Button : 00000 
  *E300408,1 Ok
  =gfModalGen('INM40002B00000','ALERT','Consolidated')
  RETURN
ENDIF
IF !llContinue
  SELECT (lcInvLine)
  DELETE ALL
  SELECT (lcInvHdr)
  DELETE ALL
  llRebuild = .T.
  *E300408,1 Message : 40003
  *E300408,1 This is a direct invoice. Would you like to rebuild the order
  *E300408,1 Button : 00001 
  *E300408,1 Yes No
  IF InvHdr.Direct_Inv .AND. gfModalGen('INM40003B40000','ALERT') = 2
    llRebuild = .F.
  ENDIF
  SELECT 'UNCMSESS'
  IF SEEK('I')
    BLANK
  ELSE
    APPEND BLANK
  ENDIF
  *B802236 (Start)
  *REPLACE Status     WITH 'O'        ,;
          cUTranType WITH lcProgID   ,;
          cUserId    WITH gcUser_id  ,;
          cSession   WITH lcSession  ,;
          cCurrObj   WITH 'PBDLT'    ,;
          cProgram   WITH 'ARDINV'   ,;
          cCurrScr   WITH 'ARDINV'   ,;
          dTranDate  WITH gdSysDate  ,;
          cTranTime  WITH TIME()
  REPLACE Status     WITH 'O'        ,;
          cUTranType WITH lcProgID   ,;
          cUserId    WITH gcUser_id  ,;
          cSession   WITH lcSession  ,;
          cCurrObj   WITH 'PBDLT'    ,;
          cProgram   WITH 'ARDINV'   ,;
          cCurrScr   WITH 'ARDINV'   ,;
          dTranDate  WITH ldVDate    ,;
          cTranTime  WITH TIME()
  *B802236 (End)
  =RLOCK()
  lcCurrInv = laData[1]

  *E301204,1 Create temporary History file which used key off process [Begin]
  =lfCreatHst()
  *E301204,1 Create temporary History file which used key off process [End  ]
  
  lcFiles = 'lcInvHdr,'+lcInvHdr+',;lcInvLine,'+lcInvLine+',InvLine;'
  *E301204,1 Add Temporary history file to uncomplete session files [Begin]
  lcFiles = lcFiles + "lcTmpAR,"  + lcTmpAR+","  + ORDER(lcTmpAR)+";"
  *E301204,1 Add Temporary history file to uncomplete session files [End  ]

  =gfSavSess(lcProgID, lcFiles, @laVars,lcSession)
ENDIF
DECLARE laGlArray[2,13]
STORE '' TO laGlArray
IF laSetups[4,2]='Y'
  =gfOpenFile(gcDataDir+'GLDIST', gcDataDir+'GLDISTNO', 'SH')

  SELECT GLDIST
  COPY STRUCTURE TO (gcWorkDir+lcGlDist)
  =gfOpenFile(gcWorkDir+lcGlDist, '', 'EX')
  STORE 'VI'        TO laGlArray[1,4],laGlArray[2,4]
  STORE laData[1]   TO laGlArray[1,5],laGlArray[2,5]   && Invoice no
  *B802236 (Start)
  *STORE gdSysDate   TO laGlArray[1,6],laGlArray[2,6]  
  STORE ldVDate     TO laGlArray[1,6],laGlArray[2,6]   && Void Date
  *B802236 (End)
  STORE lcSysYear   TO laGlArray[1,7],laGlArray[2,7]   && Year
  STORE lcSysPeriod TO laGlArray[1,8],laGlArray[2,8]   && Period
  STORE lcGlDist    TO laGlArray[1,9],laGlArray[2,9]
  STORE ''          TO laGlArray[1,11],laGlArray[2,11]
  STORE ''          TO laGlArray[1,12],laGlArray[2,12]
  STORE ''          TO laGlArray[1,13],laGlArray[2,13]
ENDIF
SET ORDER TO TAG DRTRAN IN DEBIT
*E300408,1 Get invoice currency unit sign and exchange rate sign
lcUntSin = ''
lcExRSin = gfGetExSin(@lcUntSin, INVHDR.cCurrCode)

lcInvHFile = IIF(InvHdr.CONSOL='Y','CONSINVH','INVHDR')

*B039106,1 NNA 04/13/2005 (Begin) Set order to tag (InvLine) that has key (Account+Order+Store+Piktkt+STR(LineNo,6))
*B039106,1 NNA            instead of Tag (InvLines) that has key (Account+Order+Store+Piktkt+Style)
SET ORDER TO TAG InvLine IN (lcInvLine)
*B039106,1 NNA (End)

SELECT (lcInvHFile)
SCAN REST WHILE Invoice = laData[1]
  IF !SEEK(Account+Order+Store+PikTKt,lcInvHdr)
    INSERT INTO (lcInvHdr) (Account,Order,Store,PikTKt) VALUES ;
    (&lcInvHFile..Account,&lcInvHFile..Order,&lcInvHFile..Store,&lcInvHFile..PikTkt)
  ENDIF
  IF SEEK('O'+Order,'OrdHdr')
    WAIT 'Rebuilding order# '+Order+' ...' WINDOW NOWAIT
    *B603308,1 WAB - variable to hold the diferent between the price in ordline 
    *B603308,1 WAB - and the price in invline
    lnDifValue = 0
    *B603308,1 WAB - END
    IF &lcInvHdr..nSteps < 1
      *-- the invoice line file
      lcInvLines = IIF(InvHdr.CONSOL='Y','CONSINVL','INVLINE')
      SELECT (lcInvLines)
      =SEEK(InvHdr.Invoice+IIF(InvHdr.CONSOL='Y',&lcInvHFile..Store,''))
      SCAN REST WHILE Invoice+IIF(InvHdr.CONSOL='Y',Store,'') = ;
           &lcInvHFile..Invoice+IIF(InvHdr.CONSOL='Y',&lcInvHFile..Store,'') ;
           FOR Order = &lcInvHFile..Order
        IF !SEEK(Account+Order+Store+SPACE(6)+STR(LineNo,6),lcInvLine)
          =SEEK('O'+Order+STR(LineNo,6),'ORDLINE')
          SELECT ORDLINE
          =RLOCK()
          *-- update the order line quantity
          REPLACE QTY1   WITH QTY1 + &lcInvLines..QTY1 ,;
                  QTY2   WITH QTY2 + &lcInvLines..QTY2 ,;
                  QTY3   WITH QTY3 + &lcInvLines..QTY3 ,;
                  QTY4   WITH QTY4 + &lcInvLines..QTY4 ,;
                  QTY5   WITH QTY5 + &lcInvLines..QTY5 ,;
                  QTY6   WITH QTY6 + &lcInvLines..QTY6 ,;
                  QTY7   WITH QTY7 + &lcInvLines..QTY7 ,;
                  QTY8   WITH QTY8 + &lcInvLines..QTY8 ,;
                  TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
          UNLOCK
          *B603308,1 WAB - get the diferent between the price in ordline 
          *B603308,1 WAB - and the price in invline
          lnDifValue = lnDifValue + ( (&lcInvLines..TotQty * &lcInvLines..Price) - ;
                                      (&lcInvLines..TotQty * Price) )
          *B603308,1 WAB - END
          =gfTraceKey('ORDLINE','O'+ORDLINE.Order+STR(ORDLINE.LineNo,6),'M')
          INSERT INTO (lcInvLine) (Account,Order,Store,LineNo) VALUES ;
          (&lcInvLines..Account,&lcInvLines..Order,&lcInvLines..Store,&lcInvLines..LineNo)
        ENDIF
      ENDSCAN
      SELECT (lcInvHdr)
      =RLOCK()
      REPLACE nSteps WITH 1
      UNLOCK
    ENDIF  
    IF &lcInvHdr..nSteps < 2
      SELECT ORDHDR
      =RLOCK()
      IF llRebuild
        *B603305,1 SSH 12/23/1999 Fix the bug of duplicate the appramt in case of 
        *B603305,1                voide invoice and rebulid order.
        *REPLACE STATUS     WITH 'O'  ,;
                SHIP       WITH SHIP    - &lcInvHFile..SHIP,;
                SHIPAMT    WITH SHIPAMT - &lcInvHFile..SHIPAMT,;
                OPEN       WITH OPEN    + &lcInvHFile..SHIP,;
                OPENAMT    WITH OPENAMT + &lcInvHFile..SHIPAMT,;
                APPRAMT    WITH APPRAMT + ROUND(&lcInvHFile..SHIPAMT,0)
        *B603308,1 WAB - add the shipment amount + the diferent between the invline price and the order line price 
        *B603308,1 WAB - the the open amount whith the 
        *REPLACE STATUS     WITH 'O'  ,;
                SHIP       WITH SHIP    - &lcInvHFile..SHIP,;
                SHIPAMT    WITH SHIPAMT - &lcInvHFile..SHIPAMT,;
                OPEN       WITH OPEN    + &lcInvHFile..SHIP,;
                OPENAMT    WITH OPENAMT + &lcInvHFile..SHIPAMT,;
                APPRAMT    WITH IIF((InvHdr.Consol='Y' OR (&lcInvHFile..Store <> Store)),APPRAMT+ROUND(&lcInvHFile..SHIPAMT,0),;
                                                      ROUND(&lcInvHFile..SHIPAMT,0))
        
        *-- update the order header
        *-- open amount =open amount + shipped amount- difference between invine price 
        *-- and order line price
        REPLACE STATUS     WITH 'O'  ,;
                SHIP       WITH SHIP    - &lcInvHFile..SHIP,;
                SHIPAMT    WITH SHIPAMT - &lcInvHFile..SHIPAMT ,;
                OPEN       WITH OPEN    + &lcInvHFile..SHIP,;
                OPENAMT    WITH OPENAMT + &lcInvHFile..SHIPAMT - lnDifValue,;
                APPRAMT    WITH IIF((InvHdr.Consol='Y' OR (&lcInvHFile..Store <> Store)),APPRAMT+ROUND(&lcInvHFile..SHIPAMT,0),;
                                                      ROUND(&lcInvHFile..SHIPAMT,0))
       *C102574,1 HBG 04/15/2002 Add temp file to be used for GMA [Begin]
       IF ASCAN(laEvntTrig , PADR('UPORDSH',10)) <> 0
         =gfDoTriger('ARDINV',PADR('UPORDSH',10))
       ENDIF
       *C102574,1 [End]

       *B603308,1 WAB - END
       *B603305,1 SSH (END)
      ELSE
        *B802236 (Start)
        *-- don't re build the order so cancel it 
        *REPLACE CANCEL     WITH SHIP       ,;
                CANCELAMT  WITH SHIPAMT    ,;
                SHIPAMT    WITH 0          ,;
                SHIP       WITH 0          ,;
                CANCELLED  WITH gdSysDate  ,;
                CCANCRESON WITH lfCanReason() ,;
                STATUS     WITH 'X'
        REPLACE CANCEL     WITH SHIP       ,;
                CANCELAMT  WITH SHIPAMT    ,;
                SHIPAMT    WITH 0          ,;
                SHIP       WITH 0          ,;
                CANCELLED  WITH ldVDate    ,;
                CCANCRESON WITH lfCanReason() ,;
                STATUS     WITH 'X'
        *B802236 (End)
      ENDIF
      UNLOCK
      =gfTraceKey('ORDHDR','O'+ORDHDR.Order,'M')
      SELECT (lcInvHdr)
      =RLOCK()
      REPLACE nSteps WITH 2
      UNLOCK
    ENDIF
  ENDIF

  *E300408,1 Update sales commission
  IF (!EMPTY(&lcInvHFile..Rep1) .AND. &lcInvHFile..CommAmt1 > 0) .AND. ;
    SEEK(&lcInvHFile..Rep1,'SALESREP')
    WAIT 'Charging back salesrep commissions ...' WINDOW NOWAIT
    *-- Calculate the equivlant amount
    lnEqvAmnt = -1* &lcInvHFile..CommAmt1 &lcExRSin InvHdr.nExRate ;
                    &lcUntSin InvHdr.nCurrUnit
    IF &lcInvHdr..nSteps < 3
      SELECT REPCOMM
      APPEND BLANK
      *B802236 (Start)
      *REPLACE STATUS     WITH 'O',;
              REPCODE    WITH &lcInvHFile..REP1   ,;
              ORDER      WITH &lcInvHFile..Order  ,;
              TRAN       WITH &lcInvHFile..Invoice,;
              DATE       WITH gdSysDate,;
              TRANTYPE   WITH '6',;
              DESC       WITH 'CHARGE BACK - VOID',;
              CUSTPO     WITH &lcInvHFile..CustPo  ,;
              ACCOUNT    WITH &lcInvHFile..Account ,;
              STORE      WITH &lcInvHFile..Store   ,;
              AMOUNT     WITH lnEqvAmnt      ,;
              BALANCE    WITH SALESREP.BALANCE + lnEqvAmnt,;
              nForAmnt   WITH &lcInvHFile..COMMAMT1 ,;
              cCurrCode  WITH InvHdr.cCurrCode,;
              nCurrUnit  WITH InvHdr.nCurrUnit,;
              nExRate    WITH InvHdr.nExRate  ,;
              dAdd_Date  WITH gdSysDate,;
              cAdd_Time  WITH TIME(),;
              cAdd_User  WITH gcUser_id
     *B603966,1 NAD 11/22/2000 Multiply the commission amount with -1 before updating the nForAmnt field .
     *REPLACE STATUS     WITH 'O',;
              REPCODE    WITH &lcInvHFile..REP1   ,;
              ORDER      WITH &lcInvHFile..Order  ,;
              TRAN       WITH &lcInvHFile..Invoice,;
              DATE       WITH ldVdate,;
              TRANTYPE   WITH '6',;
              DESC       WITH 'CHARGE BACK - VOID',;
              CUSTPO     WITH &lcInvHFile..CustPo  ,;
              ACCOUNT    WITH &lcInvHFile..Account ,;
              STORE      WITH &lcInvHFile..Store   ,;
              AMOUNT     WITH lnEqvAmnt      ,;
              BALANCE    WITH SALESREP.BALANCE + lnEqvAmnt,;
              nForAmnt   WITH &lcInvHFile..COMMAMT1 ,;
              cCurrCode  WITH InvHdr.cCurrCode,;
              nCurrUnit  WITH InvHdr.nCurrUnit,;
              nExRate    WITH InvHdr.nExRate  ,;
              dAdd_Date  WITH gdSysDate,;
              cAdd_Time  WITH TIME(),;
              cAdd_User  WITH gcUser_id
     
     
     REPLACE STATUS     WITH 'O',;
              REPCODE    WITH &lcInvHFile..REP1   ,;
              ORDER      WITH &lcInvHFile..Order  ,;
              TRAN       WITH &lcInvHFile..Invoice,;
              DATE       WITH ldVdate,;
              TRANTYPE   WITH '6',;
              DESC       WITH 'CHARGE BACK - VOID',;
              CUSTPO     WITH &lcInvHFile..CustPo  ,;
              ACCOUNT    WITH &lcInvHFile..Account ,;
              STORE      WITH &lcInvHFile..Store   ,;
              AMOUNT     WITH lnEqvAmnt      ,;
              BALANCE    WITH SALESREP.BALANCE + lnEqvAmnt,;
              nForAmnt   WITH -1 * &lcInvHFile..COMMAMT1  ,;
              cCurrCode  WITH InvHdr.cCurrCode,;
              nCurrUnit  WITH InvHdr.nCurrUnit,;
              nExRate    WITH InvHdr.nExRate  ,;
              dAdd_Date  WITH gdSysDate,;
              cAdd_Time  WITH TIME(),;
              cAdd_User  WITH gcUser_id
     *B603966,1 NAD 11/22/2000 (End)
     *B802236 (End)
     
     
      =gfTraceKey('REPCOMM',REPCOMM.REPCODE+DTOS(REPCOMM.DATE)+REPCOMM.TRAN+REPCOMM.TRANTYPE,'A')
      SELECT (lcInvHdr)
      =RLOCK()
      REPLACE nSteps WITH 3
      UNLOCK
    ENDIF
    IF &lcInvHdr..nSteps < 4
      SELECT SALESREP
      =RLOCK()
      REPLACE CURRENT  WITH CURRENT + lnEqvAmnt,;
              BALANCE  WITH CURRENT + AGE30+AGE60+AGE90+AGE120
      UNLOCK
      =gfTraceKey('SALESREP',SALESREP.REPCODE,'M')
      SELECT (lcInvHdr)
      =RLOCK()
      REPLACE nSteps WITH 4
      UNLOCK
    ENDIF
  ENDIF
  IF (!EMPTY(&lcInvHFile..Rep2) .AND. &lcInvHFile..CommAmt2 > 0) .AND. ;
     SEEK(&lcInvHFile..Rep2,'SALESREP')
    WAIT 'Charging back salesrep commissions ...' WINDOW NOWAIT
    lnEqvAmnt = -1 * &lcInvHFile..CommAmt2 &lcExRSin InvHdr.nExRate ;
                   &lcUntSin InvHdr.nCurrUnit
    IF &lcInvHdr..nSteps < 5
      SELECT REPCOMM
      APPEND BLANK
      =RLOCK()
      *B802236 (Start)
      *REPLACE STATUS    WITH 'O'           ,;
              REPCODE    WITH &lcInvHFile..REP2   ,;
              ORDER      WITH &lcInvHFile..Order  ,;
              TRAN       WITH &lcInvHFile..Invoice,;
              DATE       WITH gdSysDate     ,;
              TRANTYPE   WITH '6'           ,;
              DESC       WITH 'CHARGE BACK - VOID',;
              CUSTPO     WITH &lcInvHFile..CustPo ,;
              ACCOUNT    WITH &lcInvHFile..Account,;
              STORE      WITH &lcInvHFile..Store  ,;
              AMOUNT     WITH lnEqvAmnt     ,;
              BALANCE    WITH SALESREP.BALANCE + lnEqvAmnt,;
              nForAmnt   WITH &lcInvHFile..COMMAMT2 ,;
              cCurrCode  WITH InvHdr.cCurrCode,;
              nCurrUnit  WITH InvHdr.nCurrUnit,;
              nExRate    WITH InvHdr.nExRate  ,;
              dAdd_Date  WITH gdSysDate  ,;
              cAdd_Time  WITH TIME()  ,;
              cAdd_User  WITH gcUser_id
      *B603966,1 NAD 11/22/2000 Multiply the commission amount with -1 before updating the nForAmnt field .
      *REPLACE STATUS     WITH 'O'           ,;
              REPCODE    WITH &lcInvHFile..REP2   ,;
              ORDER      WITH &lcInvHFile..Order  ,;
              TRAN       WITH &lcInvHFile..Invoice,;
              DATE       WITH ldVDate     ,;
              TRANTYPE   WITH '6'           ,;
              DESC       WITH 'CHARGE BACK - VOID',;
              CUSTPO     WITH &lcInvHFile..CustPo ,;
              ACCOUNT    WITH &lcInvHFile..Account,;
              STORE      WITH &lcInvHFile..Store  ,;
              AMOUNT     WITH lnEqvAmnt     ,;
              BALANCE    WITH SALESREP.BALANCE + lnEqvAmnt,;
              nForAmnt   WITH &lcInvHFile..COMMAMT2 ,;
              cCurrCode  WITH InvHdr.cCurrCode,;
              nCurrUnit  WITH InvHdr.nCurrUnit,;
              nExRate    WITH InvHdr.nExRate  ,;
              dAdd_Date  WITH gdSysDate  ,;
              cAdd_Time  WITH TIME()  ,;
              cAdd_User  WITH gcUser_id
              
      REPLACE STATUS     WITH 'O'           ,;
              REPCODE    WITH &lcInvHFile..REP2   ,;
              ORDER      WITH &lcInvHFile..Order  ,;
              TRAN       WITH &lcInvHFile..Invoice,;
              DATE       WITH ldVDate     ,;
              TRANTYPE   WITH '6'           ,;
              DESC       WITH 'CHARGE BACK - VOID',;
              CUSTPO     WITH &lcInvHFile..CustPo ,;
              ACCOUNT    WITH &lcInvHFile..Account,;
              STORE      WITH &lcInvHFile..Store  ,;
              AMOUNT     WITH lnEqvAmnt     ,;
              BALANCE    WITH SALESREP.BALANCE + lnEqvAmnt,;
              nForAmnt   WITH -1 *  &lcInvHFile..COMMAMT2 ,;
              cCurrCode  WITH InvHdr.cCurrCode,;
              nCurrUnit  WITH InvHdr.nCurrUnit,;
              nExRate    WITH InvHdr.nExRate  ,;
              dAdd_Date  WITH gdSysDate  ,;
              cAdd_Time  WITH TIME()  ,;
              cAdd_User  WITH gcUser_id
      *B603966,1 NAD 11/22/2000 (End)
      *B802236 (End)
      UNLOCK
      =gfTraceKey('REPCOMM',REPCOMM.REPCODE+DTOS(REPCOMM.DATE)+REPCOMM.TRAN+REPCOMM.TRANTYPE,'A')
      SELECT (lcInvHdr)
      =RLOCK()
      REPLACE nSteps WITH 5
      UNLOCK
    ENDIF
    IF &lcInvHdr..nSteps < 6
      SELECT SALESREP
      =RLOCK()
      REPLACE CURRENT  WITH CURRENT + lnEqvAmnt,;
              BALANCE  WITH CURRENT + AGE30+AGE60+AGE90+AGE120
      UNLOCK
      =gfTraceKey('SALESREP',SALESREP.REPCODE,'M')
      SELECT (lcInvHdr)
      =RLOCK()
      REPLACE nSteps WITH 6
      UNLOCK
    ENDIF
  ENDIF
    
  *C102676,1 Custom process for A.S.T. sportwear. [Begin]
  IF ASCAN(laEvntTrig,PADR('VOIDREP3',10))<>0
    =gfDoTriger('ARDINV',PADR('VOIDREP3',10))
  ENDIF  
  *C102676,1 Custom process for A.S.T. sportwear. [End]

ENDSCAN

=SEEK(laData[1],'InvHdr')
=SEEK(laData[1],'InvLine')
WAIT 'Updating the style Cut & Sold ...' WINDOW NOWAIT
IF !SEEK(InvHdr.Account+InvHdr.Order+InvHdr.Store+InvHdr.PikTKt,lcInvHdr)
  INSERT INTO (lcInvHdr) (Account,Order,Store,PikTKt) VALUES ;
  (InvHdr.Account,InvHdr.Order,InvHdr.Store,InvHdr.PikTkt)
ENDIF
lnCOGSAmt = 0
SELECT INVLINE
=SEEK(InvHdr.Invoice)
SCAN REST WHILE INVOICE = InvHdr.Invoice
  IF !SEEK(Style,'Style')
    LOOP
  ENDIF
  IF !SEEK(Account+Order+Store+SPACE(6)+STR(LineNo,6),lcInvLine)
    INSERT INTO (lcInvLine) (Account,Order,Store,LineNo) VALUES ;
    (InvLine.Account,InvLine.Order,InvLine.Store,InvLine.LineNo)
  ENDIF
 
  =SEEK('O'+InvLine.Order+STR(LineNo,6),'ORDLINE')
  
  *E300834,1 Update style dyelot Shipped Quantity
  *B603760,1 NAD 07/30/2000 (Start) We Added the cWareCode field to The invline 
  *B603760,1                        so we must check if it is empty or not.                         
  *B603760,1                        if it was not empty then use it instead of ordline.cWareCode. 
  *IF laSetups[8,2]='Y' .AND. Style.cDye_Flg = 'Y' .AND. ;   
     SEEK(InvLine.Style+OrdLine.cWareCode+InvLine.Dyelot,'StyDye')
  IF laSetups[8,2]='Y' .AND. Style.cDye_Flg = 'Y' .AND. ;   
    SEEK(InvLine.Style+IIF(EMPTY(Invline.cWareCode),OrdLine.cWareCode,InvLine.cWareCode)+InvLine.Dyelot,'StyDye')
  *B603760,1  (End) 
      
    *E300834,1 Update Dyelot Ordered Quantity
    IF llRebuild .AND. &lcInvLine..nSteps < 1
      SELECT StyDye 
      =RLOCK()
      REPLACE ORD1   WITH ORD1 + InvLine.QTY1,;
              ORD2   WITH ORD2 + InvLine.QTY2,;
              ORD3   WITH ORD3 + InvLine.QTY3,;
              ORD4   WITH ORD4 + InvLine.QTY4,;
              ORD5   WITH ORD5 + InvLine.QTY5,;
              ORD6   WITH ORD6 + InvLine.QTY6,;
              ORD7   WITH ORD7 + InvLine.QTY7,;
              ORD8   WITH ORD8 + InvLine.QTY8,;
              TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
      UNLOCK
      SELECT (lcInvLine)
      =RLOCK()
      REPLACE nSteps WITH 1
      UNLOCK
    ENDIF
    IF &lcInvLine..nSteps < 2
      SELECT StyDye
      =RLOCK()
      REPLACE SHP1   WITH SHP1 - InvLine.QTY1 ,;
              SHP2   WITH SHP2 - InvLine.QTY2 ,;
              SHP3   WITH SHP3 - InvLine.QTY3 ,;
              SHP4   WITH SHP4 - InvLine.QTY4 ,;
              SHP5   WITH SHP5 - InvLine.QTY5 ,;
              SHP6   WITH SHP6 - InvLine.QTY6 ,;
              SHP7   WITH SHP7 - InvLine.QTY7 ,;
              SHP8   WITH SHP8 - InvLine.QTY8 ,;
              TOTSHP WITH SHP1+SHP2+SHP3+SHP4+SHP5+SHP6+SHP7+SHP8
      UNLOCK
      SELECT (lcInvLine)
      =RLOCK()
      REPLACE nSteps WITH 2
      UNLOCK
    ENDIF
    =gfTraceKey('STYDYE',StyDye.STYLE+StyDye.CWARECODE+StyDye.DYELOT,'M')
  ENDIF
  *E300834,1 Update style dyelot Shipped Quantity
  *B603760,1 NAD 07/30/2000 (Start) We Added the cWareCode field to The invline 
  *B603760,1                        so we must check if it is empty or not.                         
  *B603760,1                        if it was not empty then use it instead of ordline.cWareCode. 
  *IF SEEK(InvLine.Style+OrdLine.cWareCode+SPACE(10),'StyDye')
  IF SEEK(InvLine.Style+IIF(EMPTY(Invline.cWareCode),OrdLine.cWareCode,InvLine.cWareCode)+SPACE(10),'StyDye')
  *B603760,1 (End)
    *E300834,1 Update Warehouse Ordered Quantity
    IF llRebuild .AND. &lcInvLine..nSteps < 3
      SELECT StyDye
      =RLOCK()
      REPLACE ORD1   WITH ORD1 + InvLine.QTY1,;
              ORD2   WITH ORD2 + InvLine.QTY2,;
              ORD3   WITH ORD3 + InvLine.QTY3,;
              ORD4   WITH ORD4 + InvLine.QTY4,;
              ORD5   WITH ORD5 + InvLine.QTY5,;
              ORD6   WITH ORD6 + InvLine.QTY6,;
              ORD7   WITH ORD7 + InvLine.QTY7,;
              ORD8   WITH ORD8 + InvLine.QTY8,;
              TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
      UNLOCK
      SELECT (lcInvLine)
      =RLOCK()
      REPLACE nSteps WITH 3
      UNLOCK
    ENDIF
    IF &lcInvLine..nSteps < 4
      *E300834,1 Update warehouse Shipped Quantity
      SELECT StyDye
      =RLOCK()
      REPLACE SHP1   WITH SHP1 - InvLine.QTY1 ,;
              SHP2   WITH SHP2 - InvLine.QTY2 ,;
              SHP3   WITH SHP3 - InvLine.QTY3 ,;
              SHP4   WITH SHP4 - InvLine.QTY4 ,;
              SHP5   WITH SHP5 - InvLine.QTY5 ,;
              SHP6   WITH SHP6 - InvLine.QTY6 ,;
              SHP7   WITH SHP7 - InvLine.QTY7 ,;
              SHP8   WITH SHP8 - InvLine.QTY8 ,;
              TOTSHP WITH SHP1+SHP2+SHP3+SHP4+SHP5+SHP6+SHP7+SHP8
      UNLOCK
      SELECT (lcInvLine)
      =RLOCK()
      REPLACE nSteps WITH 4
      UNLOCK
    ENDIF
    =gfTraceKey('STYDYE',STYDYE.STYLE+STYDYE.CWARECODE+STYDYE.DYELOT,'M')
  ENDIF

  SELECT STYLE
  =SEEK(INVLINE.STyle)
  =RLOCK()
  IF llRebuild .AND. &lcInvLine..nSteps < 5
    REPLACE ORD1   WITH ORD1 + InvLine.QTY1 ,;
            ORD2   WITH ORD2 + InvLine.QTY2 ,;
            ORD3   WITH ORD3 + InvLine.QTY3 ,;
            ORD4   WITH ORD4 + InvLine.QTY4 ,;
            ORD5   WITH ORD5 + InvLine.QTY5 ,;
            ORD6   WITH ORD6 + InvLine.QTY6 ,;
            ORD7   WITH ORD7 + InvLine.QTY7 ,;
            ORD8   WITH ORD8 + InvLine.QTY8 ,;
            TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
    SELECT (lcInvLine)
    =RLOCK()
    REPLACE nSteps WITH 5
    UNLOCK
    SELECT Style
  ENDIF
  *C037816,1 MHM 04/06/2004 Void Invoice[Start]
  *IF ASCAN(laEvntTrig , PADR('DLVODINV',10)) <> 0
  *  =gfDoTriger('ARDINV',PADR('DLVODINV',10)) 
  *ENDIF  
  *C037816,1 MHM [End]

  IF &lcInvLine..nSteps < 6
    *E300834,1 Update style Shipped Quantity
    REPLACE SHP1   WITH SHP1 - InvLine.QTY1 ,;
            SHP2   WITH SHP2 - InvLine.QTY2 ,;
            SHP3   WITH SHP3 - InvLine.QTY3 ,;
            SHP4   WITH SHP4 - InvLine.QTY4 ,;
            SHP5   WITH SHP5 - InvLine.QTY5 ,;
            SHP6   WITH SHP6 - InvLine.QTY6 ,;
            SHP7   WITH SHP7 - InvLine.QTY7 ,;
            SHP8   WITH SHP8 - InvLine.QTY8 ,;
            TOTSHP WITH SHP1+SHP2+SHP3+SHP4+SHP5+SHP6+SHP7+SHP8
    SELECT (lcInvLine)
    =RLOCK()
    REPLACE nSteps WITH 6
    UNLOCK
  ENDIF
  SELECT Style
  UNLOCK
  =gfTraceKey('STYLE',STYLE.STYLE,'M')

  lnCOGSAmt = lnCOGSAmt + InvLine.TotQty*InvLine.Cost
  IF laSetups[4,2]='Y'
    *E300408,1 Sales Revenus <DEBIT> Category key '003'
    
    *B802236 (Start)
    *DO GLDIST WITH InvLine.GL_Sales,'003',(InvLine.TotQty *InvLine.Price),'VI',InvLine.Invoice,;
                   (gdSysDate),lcSysYear,lcSysPeriod,lcGlDist,InvLine.cSalesAcnt,;
                   InvHDr.cCurrCode,InvHDr.nCurrUnit,InvHDr.nExRate
    **E300408,1 Discount <CREDIT> Category key '005'
    *DO GLDIST WITH InvLine.GL_Sales,'005',-(InvLine.TotQty*InvLine.Price*ABS(InvHDr.Discount)/InvHDr.ShipAmt),;
                   'VI',InvLine.Invoice,(gdSysDate),lcSysYear,lcSysPeriod,;
                   lcGlDist,InvLine.cDiscAcnt,InvHDr.cCurrCode,InvHDr.nCurrUnit,;
                   InvHDr.nExRate

    DO GLDIST WITH InvLine.GL_Sales,'003',(InvLine.TotQty *InvLine.Price),'VI',InvLine.Invoice,;
                   ldVDate,lcSysYear,lcSysPeriod,lcGlDist,InvLine.cSalesAcnt,;
                   InvHDr.cCurrCode,InvHDr.nCurrUnit,InvHDr.nExRate
    
    *B603760,1 NAD 08/01/2000 If ship amount equal zero, don't make GL transaction.
    IF InvHdr.ShipAmt <> 0
    *B603760,1 (End) 
      DO GLDIST WITH InvLine.GL_Sales,'005',-(InvLine.TotQty*InvLine.Price*ABS(InvHDr.Discount)/InvHDr.ShipAmt),;
                     'VI',InvLine.Invoice,ldVDate,lcSysYear,lcSysPeriod,;
                     lcGlDist,InvLine.cDiscAcnt,InvHDr.cCurrCode,InvHDr.nCurrUnit,;
                     InvHDr.nExRate
    
    *B603760,1 NAD 08/01/2000 Aded the ENDIF.
    ENDIF 
    *B603760,1 (End) 
    *B802236 (End)
  ENDIF
  SELECT INVLINE
  =SEEK('O'+INVLINE.Order+STR(LINENO,6),'ORDLINE')
  IF !EMPTY(Ordline.AltStyle) .AND. (Ordline.AltStyle <> Invline.Style)
    *DO lpSwchSty WITH Ordline.Style,Ordline.AltStyle
  ENDIF
  SELECT icStyHst
  IF SEEK(INVLINE.Style+lcInvYear) .AND. &lcInvLine..nSteps < 7
    lnShipAmt = ROUND(InvLine.TotQty*InvLine.Price &lcExRSin InvHdr.nExRate ;
                    &lcUntSin InvHdr.nCurrUnit, 2)
    lnDiscount = ROUND(InvLine.TotQty*InvLine.Price*laData[32]/100 &lcExRSin InvHdr.nExRate ;
                    &lcUntSin InvHdr.nCurrUnit, 2)
    =RLOCK()
    REPLACE nSlsQty&lcInvPeriod  WITH nSlsQty&lcInvPeriod  - InvLine.TotQty ,;
            nSlsQty              WITH nSlsQty              - InvLine.TotQty ,;
            nSlsAmt&lcInvPeriod  WITH nSlsAmt&lcInvPeriod  - lnShipAmt  ,;
            nSlsAmt              WITH nSlsAmt              - lnShipAmt  ,;
            nDisAmt&lcInvPeriod  WITH nDisAmt&lcInvPeriod  - lnDiscount ,;
            nDisAmt              WITH nDisAmt              - lnDiscount ,;
            nCOGSAmt&lcInvPeriod WITH nCOGSAmt&lcInvPeriod - InvLine.TotQty*InvLine.Cost  ,;
            nCOGSAmt             WITH nCOGSAmt             - InvLine.TotQty*InvLine.Cost  
    UNLOCK
    =gfTraceKey('icStyHst',INVLINE.Style+lcInvYear,'M')
    SELECT (lcInvLine)
    =RLOCK()
    REPLACE nSteps WITH 7
    UNLOCK
  ENDIF

  SELECT INVLINE
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laInvQty
  IF laSetups[4,2]='Y'
    *E300408,1 Cost of Goods Sold <CREDIT> Category key '008'
    laGlArray[1,1]  = GL_Sales
    laGlArray[1,2]  = '008'
    laGlArray[1,3]  = -1
    laGlArray[1,10] = cCOGSAcnt

    *E300408,1 Inventory Control <DEBIT> Category key '006'
    laGlArray[2,1]  = GL_COST
    laGlArray[2,2]  = '006'
    laGlArray[2,3]  = 1
    laGlArray[2,10] = cICAcnt
  ENDIF
  
  *B602322,1 Pass GL Session sequence to this function to make sure that the record
  *=gfStyCrl('4',Style,OrdLine.cWareCode,Dyelot,gdSysDate,laData[1],;
            @laInvQty,Cost,'',.F.,'',8,lcInvLine,'nSteps',@laGlArray)

  *E301200,1 (Start)
  *=gfStyCrl('4',Style,OrdLine.cWareCode,Dyelot,gdSysDate,laData[1],;
            @laInvQty,Cost,'',lcGlSession,'',8,lcInvLine,'nSteps',@laGlArray)

  *E301192 (Start)
  *=gfStyCrl('4',Style,OrdLine.cWareCode,Dyelot,gdSysDate,laData[1],;
            @laInvQty,Cost,'',lcGlSession,'',8,lcInvLine,'nSteps',@laGlArray,InvLine.LineNo)

  PRIVATE lcRefer
  IF InvHdr.DIRECT_INV
    lcRefer = 'CUST# '+ Customer.Account + "-" + Customer.BTName
  ELSE
    lcRefer = 'CUST# '+ Customer.Account + " Sales order " + InvHdr.Order
  ENDIF
  *B802236 (Start)
  *=gfStyCrl('4',Style,OrdLine.cWareCode,Dyelot,gdSysDate,laData[1],;
            @laInvQty,Cost,lcRefer,lcGlSession,'',8,lcInvLine,'nSteps',@laGlArray,InvLine.LineNo)
  
  *B603760,1 NAD 07/30/2000 (Start) We Added the cWareCode field to The invline 
  *B603760,1                        so we must check if it is empty or not.                         
  *B603760,1                        if it was not empty then pass it to the gfStycrl 
  *B603760,1                        instead of ordline.cWareCode. 
  *=gfStyCrl('4',Style,OrdLine.cWareCode,Dyelot,ldVDate,laData[1],;
            @laInvQty,Cost,lcRefer,lcGlSession,'',8,lcInvLine,'nSteps',@laGlArray,InvLine.LineNo)
  =gfStyCrl('4',Style,IIF(EMPTY(Invline.cWareCode),OrdLine.cWareCode,InvLine.cWareCode),Dyelot,ldVDate,laData[1],;
            @laInvQty,Cost,lcRefer,lcGlSession,'',8,lcInvLine,'nSteps',@laGlArray,InvLine.LineNo)
  *B603760,1 (End)  
  *B802236 (End)

  *E301192 (End)
  *E301200,1 (End)          
  
  *!B602322,1 end.
  *--mhm123853
  
  *C037816,1 MHM 04/06/2004 Void Invoice[Start]
  IF ASCAN(laEvntTrig , PADR('DLVODINV',10)) <> 0
    =gfDoTriger('ARDINV',PADR('DLVODINV',10)) 
  ENDIF  
  *C037816,1 MHM [End]
  
  *--mhm123853

  *E301090,1 Keep invoice lines
  *SELECT INVLINE
  *=gfTraceKey('INVLINE',INVOICE+STR(LINENO,6),'D')
  *DELETE
  *E301090,1 (End)
ENDSCAN

*B605617,1 NAD File PickTkt not Found (Began)
IF 'AL' $ gcCmpModules
*B605617,1 NAD (End)

  *B604924,1 Update the alo. qty in Style , StyDye and Pik qty in Ordline. [Begin]
  PRIVATE lcCurFile , lcInvFile
  lcCurFile = ALIAS()
  =SEEK(laData[1],'InvHdr')
  lcInvFile = IIF(InvHdr.Consol='Y','ConsInvH','InvHdr')
  SELECT (lcInvFile)
  =SEEK(laData[1],lcInvFile)
  SCAN REST WHILE Invoice = laData[1]
    IF SEEK('O'+Order,'ORDLINE')
      PRIVATE llPikLine , llPack_Lin , llPikTkt          
          
      llPikTkt = gfOpenFile(gcDataDir+'PikTkt',gcDataDir+'OrdPik','SH')
      llPack_Lin = gfOpenFile(gcDataDir+'Pack_Lin',gcDataDir+'Pack_Lin','SH')
      llPikLine = gfOpenFile(gcDataDir+'PikLine',gcDataDir+'PikLine','SH')

      IF SEEK(OrdLine.Order+&lcInvFile..PikTkt,'PikTkt')
        SELECT PikTkt
        =RLOCK()
        REPLACE Status WITH "O"

        *B608279,1 NNA 09/23/2007 (Begin) add the audit trail information when void an invoice
        =gfAdd_Info('PIKTKT')  
        *B608279,1 NNA (End)
        UNLOCK
      ENDIF          
      
      IF SEEK(&lcInvFile..PikTkt+OrdLine.Order,'PikLine')
        SELECT PikLine
        SCAN REST WHILE PikTkt + Order = &lcInvFile..PikTkt+OrdLine.Order
          IF SEEK('O'+Order+STR(LineNo,6),'OrdLine')
            SELECT OrdLine
            =RLOCK()
            REPLACE Pik1 WITH Pik1 + PikLine.Pik1 ,;
                    Pik2 WITH Pik2 + PikLine.Pik2 ,;
                    Pik3 WITH Pik3 + PikLine.Pik3 ,;
                    Pik4 WITH Pik4 + PikLine.Pik4 ,;
                    Pik5 WITH Pik5 + PikLine.Pik5 ,;
                    Pik6 WITH Pik6 + PikLine.Pik6 ,;
                    Pik7 WITH Pik7 + PikLine.Pik7 ,;
                    Pik8 WITH Pik8 + PikLine.Pik8 ,;
                    TotPik WITH TotPik + PikLine.TotPik ,;
                    PikTkt WITH PikLine.PikTkt ,;
                    PikDate WITH PikLine.PikDate ,;
                    Picked WITH .T.
         
            *B605532,1 NAD (Start)
               REPLACE Dyelot WITH  PikLine.Dyelot          
            *B605532,1 NAD (End)
          
            IF SEEK(OrdLine.PikTkt,'Pack_Lin')             
              SELECT Pack_Lin
              LOCATE REST WHILE Pack_No = OrdLine.PikTkt FOR OrdLine.LineNo = nOrdLineNo
              IF FOUND()
                SELECT OrdLine
                REPLACE nPck1 WITH nPck1 + Pack_Lin.Qty1 ,;
                        nPck2 WITH nPck2 + Pack_Lin.Qty2 ,;
                        nPck3 WITH nPck3 + Pack_Lin.Qty3 ,;
                        nPck4 WITH nPck4 + Pack_Lin.Qty4 ,;
                        nPck5 WITH nPck5 + Pack_Lin.Qty5 ,;
                        nPck6 WITH nPck6 + Pack_Lin.Qty6 ,;
                        nPck7 WITH nPck7 + Pack_Lin.Qty7 ,;
                        nPck8 WITH nPck8 + Pack_Lin.Qty8
              ENDIF                      
            ENDIF
            UNLOCK

            IF laSetups[8,2]='Y' AND SEEK(Style,'Style') AND Style.cDye_Flg = 'Y' AND ;   
              SEEK(Style+cWareCode+Dyelot,'StyDye')          
              SELECT StyDye
              =RLOCK() 
              REPLACE Alo1 WITH Alo1 + OrdLine.Pik1 ,;
                      Alo2 WITH Alo2 + OrdLine.Pik2 ,;
                      Alo3 WITH Alo3 + OrdLine.Pik3 ,;
                      Alo4 WITH Alo4 + OrdLine.Pik4 ,;
                      Alo5 WITH Alo5 + OrdLine.Pik5 ,;
                      Alo6 WITH Alo6 + OrdLine.Pik6 ,;
                      Alo7 WITH Alo7 + OrdLine.Pik7 ,;
                      Alo8 WITH Alo8 + OrdLine.Pik8 ,;
                      TotAlo WITH TotAlo + OrdLine.TotPik
              UNLOCK        
            ENDIF  

            IF SEEK(Style+cWareCode+SPACE(10),'StyDye')
              SELECT StyDye
              =RLOCK() 
              REPLACE Alo1 WITH Alo1 + OrdLine.Pik1 ,;
                      Alo2 WITH Alo2 + OrdLine.Pik2 ,;
                      Alo3 WITH Alo3 + OrdLine.Pik3 ,;
                      Alo4 WITH Alo4 + OrdLine.Pik4 ,;
                      Alo5 WITH Alo5 + OrdLine.Pik5 ,;
                      Alo6 WITH Alo6 + OrdLine.Pik6 ,;
                      Alo7 WITH Alo7 + OrdLine.Pik7 ,;
                      Alo8 WITH Alo8 + OrdLine.Pik8 ,;
                      TotAlo WITH TotAlo + OrdLine.TotPik
              UNLOCK                
            ENDIF
        
            IF SEEK(Style,'Style')
              SELECT Style
              =RLOCK() 
              REPLACE Alo1 WITH Alo1 + OrdLine.Pik1 ,;
                      Alo2 WITH Alo2 + OrdLine.Pik2 ,;
                      Alo3 WITH Alo3 + OrdLine.Pik3 ,;
                      Alo4 WITH Alo4 + OrdLine.Pik4 ,;
                      Alo5 WITH Alo5 + OrdLine.Pik5 ,;
                      Alo6 WITH Alo6 + OrdLine.Pik6 ,;
                      Alo7 WITH Alo7 + OrdLine.Pik7 ,;
                      Alo8 WITH Alo8 + OrdLine.Pik8 ,;
                      TotAlo WITH TotAlo + OrdLine.TotPik
              UNLOCK                
            ENDIF
          ENDIF
          SELECT PikLine
          DELETE
        ENDSCAN  
      ENDIF

      IF llPikTkt
        USE IN PikTkt
      ENDIF
      IF llPack_Lin
        USE IN Pack_Lin
      ENDIF
      IF llPikLine
        USE IN PikLine
      ENDIF  
    ENDIF
  ENDSCAN  
  =SEEK(laData[1],'InvHdr')
  =SEEK(laData[1],lcInvFile)
  SELECT (lcCurFile)
 *B604924,1 Update the alo. qty in Style , StyDye and Pik qty in Ordline. [End]
*B605617,1 NAD File PickTkt not Found (Began)
ENDIF 
*B605617,1 NAD (End)

*E300408,1 Delete consolidated invoice lines
*E301090,1 Keep invoice lines
*IF InvHdr.CONSOL='Y'
*  SELECT CONSINVL
*  =SEEK(InvHdr.Invoice)
*  DELETE REST WHILE INVOICE=InvHdr.Invoice AND ;
*  gfTraceKey('CONSINVL',CONSINVL.INVOICE+CONSINVL.STORE+CONSINVL.ORDER+;
*              CONSINVL.STYLE+STR(CONSINVL.LINENO,6),'D')
*ENDIF
*E301090,1 (End)
*-- update the customer history file
SELECT arCusHst
IF SEEK(laData[2]+lcInvYear) .AND. &lcInvHdr..nSteps < 7
  lnShipAmt = ROUND(InvHdr.SHIPAMT &lcExRSin InvHdr.nExRate ;
                  &lcUntSin InvHdr.nCurrUnit, 2)
  lnDiscount = ROUND(ABS(InvHdr.DISCOUNT) &lcExRSin InvHdr.nExRate ;
                  &lcUntSin InvHdr.nCurrUnit, 2)
  =RLOCK()
  REPLACE nSlsQty&lcInvPeriod  WITH nSlsQty&lcInvPeriod  - InvHdr.SHIP ,;
          nSlsQty              WITH nSlsQty              - InvHdr.SHIP ,;
          nSlsAmt&lcInvPeriod  WITH nSlsAmt&lcInvPeriod  - lnShipAmt   ,;
          nSlsAmt              WITH nSlsAmt              - lnShipAmt   ,;
          nDisAmt&lcInvPeriod  WITH nDisAmt&lcInvPeriod  - lnDiscount  ,;
          nDisAmt              WITH nDisAmt              - lnDiscount  ,;
          nCOGSAmt&lcInvPeriod WITH nCOGSAmt&lcInvPeriod - lnCOGSAmt   ,;
          nCOGSAmt             WITH nCOGSAmt             - lnCOGSAmt
  UNLOCK
  =gfTraceKey('arCusHst',laData[2]+lcInvYear,'M')
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE nSteps WITH 7
  UNLOCK
ENDIF
SELECT INVHDR
IF laSetups[4,2]='Y'
  *E300408,1 Freight income <DEBIT> Category key '004'
  IF llIsEngland
    *E301077,51 Inhance openning files to speed up transaction
    =gfOpenFile(gcDataDir+'InvChrg',gcDataDir+'InvChrg','SH')
    *E301077,51 (End)
    IF SEEK(InvHdr.Invoice,'INVCHRG')
      SELECT INVCHRG
      SCAN REST WHILE Invoice= InvHdr.Invoice
        *B802236 (Start)
        *DO GLDIST WITH InvHdr.LINK_CODE,'004',INVCHRG.nChrgAmnt,'VI',;
                  InvHdr.Invoice,(gdSysDate),lcSysYear,lcSysPeriod,lcGlDist,;
                  INVCHRG.cFrgtAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,InvHdr.nExRate
        DO GLDIST WITH InvHdr.LINK_CODE,'004',INVCHRG.nChrgAmnt,'VI',;
                  InvHdr.Invoice,ldVDate,lcSysYear,lcSysPeriod,lcGlDist,;
                  INVCHRG.cFrgtAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,InvHdr.nExRate
        *B802236 (End)
      ENDSCAN
    ENDIF
    *E301077,51 Inhance openning files to speed up transaction
    =gfCloseFile('InvChrg')
    *E301077,51 (End)
  ELSE
    *B802236 (Start)
    *DO GLDIST WITH InvHdr.LINK_CODE,'004',(InvHdr.Freight+InvHdr.Insur+InvHdr.Cod),;
              'VI',InvHdr.Invoice,(gdSysDate),lcSysYear,lcSysPeriod,lcGlDist,;
              InvHdr.cFrgtAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,InvHdr.nExRate
    DO GLDIST WITH InvHdr.LINK_CODE,'004',(InvHdr.Freight+InvHdr.Insur+InvHdr.Cod),;
              'VI',InvHdr.Invoice,ldVDate,lcSysYear,lcSysPeriod,lcGlDist,;
              InvHdr.cFrgtAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,InvHdr.nExRate
    *B802236 (Start)
  ENDIF                 
  IF laSetups[9,2]='Y'
    *E300408,1 Tax Income <DEBIT> Category key '014'
    *B802236 (Start)
    *DO GLDIST WITH InvHdr.LINK_CODE,'014',InvHdr.TAX_AMT+InvHdr.nPstAmt,;
                   'VI',InvHdr.INVOICE,(gdSysDate),lcSysYear,lcSysPeriod,;
                   lcGlDist,InvHdr.cTaxAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,;
                    InvHdr.nExRate
    *C102212,1 (Begin) Update HST Tax.                
    *DO GLDIST WITH InvHdr.LINK_CODE,'014',InvHdr.TAX_AMT+InvHdr.nPstAmt,;
                   'VI',InvHdr.INVOICE,ldVDate,lcSysYear,lcSysPeriod,;
                   lcGlDist,InvHdr.cTaxAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,;
                    InvHdr.nExRate


    *E302472,1 MHM 11/18/2007  Multiple Taxes for Canada on Invoice [T20060709.0008] [Start]
    *--Handle updating GL 
    *DO GLDIST WITH InvHdr.LINK_CODE,'014',InvHdr.TAX_AMT+InvHdr.nPstAmt+InvHdr.nHstAmt,;
                   'VI',InvHdr.INVOICE,ldVDate,lcSysYear,lcSysPeriod,;
                   lcGlDist,InvHdr.cTaxAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,;
                    InvHdr.nExRate
  
    IF llIsCanada
      llRpHSTTax = .F.
      =lfGetHST()
      IF !llRpHSTTax
        DO GLDIST WITH InvHdr.LINK_CODE,'014',InvHdr.TAX_AMT,;
                     'VI',InvHdr.INVOICE,ldVDate,lcSysYear,lcSysPeriod,;
                     lcGlDist,InvHdr.cTaxAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,;
                      InvHdr.nExRate

        DO GLDIST WITH InvHdr.LINK_CODE,'027',InvHdr.nPstAmt,;
                     'VI',InvHdr.INVOICE,ldVDate,lcSysYear,lcSysPeriod,;
                     lcGlDist,InvHdr.cTaxAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,;
                      InvHdr.nExRate
      ELSE
        DO GLDIST WITH InvHdr.LINK_CODE,'028',InvHdr.nHstAmt,;
                     'VI',InvHdr.INVOICE,ldVDate,lcSysYear,lcSysPeriod,;
                     lcGlDist,InvHdr.cTaxAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,;
                      InvHdr.nExRate
      ENDIF                
    ELSE
      DO GLDIST WITH InvHdr.LINK_CODE,'014',InvHdr.TAX_AMT+InvHdr.nPstAmt+InvHdr.nHstAmt,;
                     'VI',InvHdr.INVOICE,ldVDate,lcSysYear,lcSysPeriod,;
                     lcGlDist,InvHdr.cTaxAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,;
                      InvHdr.nExRate
    ENDIF
   *                    
  *E302472,1 MHM [End]

    *C102212,1 (End)
    *B802236 (End)
  ENDIF
  *E300408,1 Account receivable <CREDIT> Category key '001'
  *B802236 (Start)
  *DO GLDIST WITH InvHdr.LINK_CODE,'001',-(InvHdr.TOTALCHG),'VI', ;
                 InvHdr.INVOICE,(gdSysDate),lcSysYear,lcSysPeriod,lcGlDist,;
                 InvHdr.cArAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,;
                 InvHdr.nExRate  
  DO GLDIST WITH InvHdr.LINK_CODE,'001',-(InvHdr.TOTALCHG),'VI', ;
                 InvHdr.INVOICE,ldVDate,lcSysYear,lcSysPeriod,lcGlDist,;
                 InvHdr.cArAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,;
                 InvHdr.nExRate  
  *B802236 (End)
ENDIF
SELECT INVHDR
=RLOCK()
*B802236 (Start)
*REPLACE STATUS     WITH 'V',;
        CUSTPO     WITH '*VOID*',;
        VDATE      WITH gdSysDate
REPLACE STATUS     WITH 'V',;
        CUSTPO     WITH '*VOID*',;
        VDATE      WITH ldVDate
*B802236 (End)        
REPLACE VCOMMAMT1  WITH IIF(VCOMMAMT1=0,COMMAMT1,VCOMMAMT1),;
        VCOMMAMT2  WITH IIF(VCOMMAMT2=0,COMMAMT2,VCOMMAMT2),;
        VSHIP      WITH IIF(VSHIP=0,SHIP,VSHIP),;
        VSHIPAMT   WITH IIF(VSHIPAMT=0,SHIPAMT,VSHIPAMT),;
        VCOD       WITH IIF(VCOD=0,COD,VCOD),;
        VCOD_AMT   WITH IIF(VCOD_AMT=0,COD_AMT,VCOD_AMT),;
        VINSUR     WITH IIF(VINSUR=0,INSUR,VINSUR),;
        VFREIGHT   WITH IIF(VFREIGHT=0,FREIGHT,VFREIGHT),;
        VDISCOUNT  WITH IIF(VDISCOUNT=0,DISCOUNT,VDISCOUNT),;
        VTOTALCHG  WITH IIF(VTOTALCHG=0,TOTALCHG,VTOTALCHG),;
        COMMAMT1   WITH 0,;
        COMMAMT2   WITH 0,;
        SHIP       WITH 0,;
        SHIPAMT    WITH 0,;
        COD        WITH 0,;
        COD_AMT    WITH 0,;
        INSUR      WITH 0,;
        FREIGHT    WITH 0,;
        DISCOUNT   WITH 0,;
        TOTALCHG   WITH 0 ,;
        nVPstAmt   WITH IIF(nVPstAmt=0,nPstAmt,nVPstAmt)  ,;
        nPstAmt    WITH 0        ,;
        VTAX_AMT   WITH IIF(VTAX_AMT=0,TAX_AMT,VTAX_AMT)  ,;
        TAX_AMT    WITH 0        ,;
        nvCharges  WITH IIF(nvCharges=0,nCharges,nvCharges) ,;
        nCharges   WITH 0

*C102676,1 Custom process for A.S.T. sportwear. [Begin]
IF ASCAN(laEvntTrig,PADR('VOIDCOM3',10))<>0
  =gfDoTriger('ARDINV',PADR('VOIDCOM3',10))
ENDIF  
*C102676,1 Custom process for A.S.T. sportwear. [End]

*B603998,1 Update the voided HST Tax field. [Begin]
REPLACE nVHstAmt   WITH IIF(nVHstAmt=0,nHstAmt,nVHstAmt)  ,;
        nHstAmt    WITH 0
*B603998,1 Update the voided HST Tax field. [End]


UNLOCK
=gfTraceKey('INVHDR',laData[1],'M')

*E300408,1 Update consolidated invoice header
IF InvHdr.CONSOL='Y'
  SELECT CONSINVH
  =SEEK(InvHdr.Invoice)
  SCAN REST WHILE INVOICE = InvHdr.Invoice
   
   *B123626,1 NNA 11/01/2004 (Begin) Stop command [Rest] Because it is doing as [Replace All]
   * REPLACE REST STATUS WITH 'V', CUSTPO WITH '*VOID*' 
    REPLACE STATUS WITH 'V' , CUSTPO WITH '*VOID*' 
   *B123626,1 NNA (End)
   
    =gfTraceKey('CONSINVH',INVOICE+STORE+ORDER,'M')
  ENDSCAN
ENDIF
*E300408,1 Update general ledger distribution file
IF laSetups[4,2]='Y' AND !SEEK(laData[1]+'VI'+lcGlSession,'GLDIST')
  WAIT 'Updating the general ledger distribution file ...' WINDOW NOWAIT
  SELECT (lcGlDist)

  *B602937 (Start)
  *REPLACE ALL GLSESSION WITH lcGlSession
  SCAN
    REPLACE GLSESSION WITH lcGlSession
    =gfAdd_Info(lcGlDist)  
  ENDSCAN
  *B602937 (End)
  
  USE
  SELECT GLDIST
  APPEND FROM (gcWorkDir+lcGlDist)
  =gfTraceKey('GLDIST',laData[1]+'VI'+lcGlSession,'A')
ENDIF

*E300408,1 Update customer A/R
IF EMPTY(InvHdr.cFacCode) .OR. laSetups[17,2]='Y'

  SELECT DEBIT
  =SEEK('1'+laData[1])

  *E301204,1 Add debit records and create credit records [Begin]
  *E301204,1 then call key off program.
  *DELETE REST WHILE TRANTYPE+TRAN+CINSTALNO='1'+laData[1] AND ;
  *gfTraceKey('DEBIT','1'+laData[1]+DEBIT.CINSTALNO,'D')
  *
  lnCreAmt = 0 
  
  *-- Scan debit records.
  SCAN REST WHILE TRANTYPE+TRAN+CINSTALNO='1'+laData[1]
    SCATTER MEMVAR MEMO
    m.cShToOpn  = 'Y'
    
    INSERT INTO (lcTmpAr) FROM MEMVAR
    lnCreAmt = lnCreAmt - m.Amount
  ENDSCAN

  *-- Add Credit Data to master credit file[Begin]
  m.Batch     = gfsequence('BATCH')
  
  *B603181 (Start)
  *m.Tran      = gfsequence('CREDIT', gcAct_Comp, "", "", "TRAN")
  m.Tran = InvHdr.Invoice
  *B603181 (End)

  *B602894,1 Transaction type should be I not 5. [Begin]
  *m.TranType  = '5' 
  m.TranType  = 'I' 
  *B602894,1 Transaction type should be I not 5. [End  ]

  m.Desc      = 'CREDIT ADJ./VOID INV'
  m.Reference = 'Void Invoice'
  m.Amount    = lnCreAmt
  m.cInstalNo = ''
  *B604390,1 NAD  (Start) 04/30/2001 Update the transaction date with the void Date instead of the invoice date
  m.TranDate=ldVDate
  *B604390,1 NAD  (End)
  SELECT CREDIT
  =RLOCK()
  INSERT INTO ('CREDIT') FROM MEMVAR
  =gfAdd_Info('CREDIT')

  *B602894,1 Transaction type should be I not 5. [Begin]
  *=gfTraceKey('CREDIT','5'+Tran,'A')
  =gfTraceKey('CREDIT','I'+Tran,'A')
  *B602894,1 Transaction type should be I not 5. [End  ]

  UNLOCK
  *-- Add Credit Data to master credit file[End  ]

  *-- Add Credit Data to temporary history file[Begin]
  =RLOCK(lcTmpAr)
  INSERT INTO (lcTmpAr) FROM MEMVAR
  UNLOCK  
  *-- Add Credit Data to temporary history file[End  ]
  
  *-- Call key off program.
  *B802236 (Start)
  *DO lfKeyOff IN (gcapphome+'ARKEYOF.PRG') WITH ;
           INVHDR.ACCOUNT,gdSysDate,ABS(lnCreAmt),lnCreAmt,lcTmpAR
  DO lfKeyOff IN (gcapphome+'ARKEYOF.PRG') WITH ;
           INVHDR.ACCOUNT,ldVDate,ABS(lnCreAmt),lnCreAmt,lcTmpAR
  *B802236 (End)  
  *E301204,1 Add debit records and create credit records [End  ]

  SELECT CUSTOMER
  IF SEEK('M'+laData[2]) .AND. &lcInvHdr..nSteps < 8
    lnEqvAmnt = ABS(InvHdr.VTotalChg) &lcExRSin INVHDR.nExRate;
                &lcUntSin InvHdr.nCurrUnit * -1
    **B603219,1 AHM Start           
    *=RLOCK()
    *REPLACE CURRENT WITH CURRENT + lnEqvAmnt,;
            TOTAGE  WITH CURRENT + AGE30 + AGE60 + AGE90 + AGE120,;
            NETBAL  WITH NETBAL  + lnEqvAmnt
    *UNLOCK
    **B603219,1 AHM End
    
    *E301245,1 function to update nHgWtrMark Field with NETBAL field [Begin.]
    =lfHgWUpdat() 
    *E301245,1 function to update nHgWtrMark Field with NETBAL field [End.]

    =gfTraceKey('CUSTOMER','M'+laData[2],'M')
    SELECT (lcInvHdr)
    =RLOCK()
    REPLACE nSteps WITH 8
    UNLOCK
  ENDIF

ENDIF

*B802779,1 When void invoice we should not send 810
IF 'EB' $ gcCmpModules 
  *B132491,1  TMI [Start] relocate the pointer to the correct record in the invhdr file
  =SEEK(laData[1],'INVHDR')
  *B132491,1  TMI [End  ] 
  llEdiTrans = gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')
  SELECT EDITRANS
  *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
  *IF SEEK('810'+PADR(InvHdr.Invoice,20)+'A'+InvHdr.Account)
  IF SEEK('810'+PADR(InvHdr.Invoice,40)+'A'+InvHdr.Account)
  *E037853,1 [End]
    DELETE
  ENDIF
  *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
  *IF SEEK('810'+PADR(InvHdr.Invoice,20)+'F'+InvHdr.cFacCode)
  IF SEEK('810'+PADR(InvHdr.Invoice,40)+'F'+InvHdr.cFacCode)
  *E037853,1 [End]

    DELETE
  ENDIF
  IF llEdiTrans
    USE IN 'EDITRANS'
  ENDIF
ENDIF
*B802779,1 (End)

SELECT unCmSess
REPLACE STATUS WITH 'C'
llContinue = .F.
UNLOCK
*E301077,51 Inhance openning files to speed up transaction
=gfCloseFile('icStyHst')
=gfCloseFile('arCusHst')
=gfCloseFile('DEBIT')
=gfCloseFile('REPCOMM')
IF InvHdr.CONSOL = 'Y' 
  =gfCloseFile('CONSINVH')
  =gfCloseFile('CONSINVL')
ENDIF
*E301077,51 (End)
SELECT INVHDR
WAIT CLEAR
*E300408,1 Message : 40004
*E300408,1 Invoice#  has been Voided.
*E300408,1 Button : 00000 
*E300408,1 Ok
=gfModalGen('INM40004B00000','ALERT',laData[1])

*E301204,1 Close credit and history files [Begin]
IF EMPTY(laData[24]) OR laSetups[17,2]='Y'
  =gfCloseFile('ARHIST')
  =gfCloseFile('CREDIT')
ENDIF
*E301204,1 Close credit and history files [End  ]
*C200352,1 HBG 04/15/2002 IF Trigger to GMA update 'SHDAEXPR' File of UPS system [Begin] 
IF ASCAN(laEvntTrig , PADR('VOIDUPS',10)) <> 0 
  =gfDoTriger('ARDINV',PADR('VOIDUPS',10))
ENDIF
*C200352,1 [End]

*B127347,1  TMI [Start] Remove the invoice refrence from POCRTNMF file if the invoice is voided
IF ASCAN(laEvntTrig , PADR('VOIDCRT',10)) <> 0   
  =gfDoTriger('ARDINV',PADR('VOIDCRT',10))
ENDIF
*B127347,1  TMI [End  ] 
*C123851,1  TMI [Start] Update the file ORDCHG for DCC 
IF ASCAN(laEvntTrig , PADR('VDORDCHG',10)) <> 0   
  =gfDoTriger('ARDINV',PADR('VDORDCHG',10))
ENDIF
*C123851,1  TMI [End  ] 

RETURN

*!*************************************************************
*! Name      : lfvSend
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Send Invoice EDI File
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvSend()
*!*************************************************************
FUNCTION lfvSend

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
lcIDefSes = ALLTRIM(laSeasons[lnSeason,2])
lcIDefDiv = ALLTRIM(laDivision[lnDivision,2])
IF EMPTY(ldDefInvDate)
  *E300408,1 Message : 40032
  *E300408,1 Must enter a default invoice date.
  *E300408,1 Button : 00000
  *E300408,1 Ok
  =gfModalGen('INM40032B00000','ALERT','invoice')
  _CUROBJ = OBJNUM(ldDefInvDate)
  RETURN
ENDIF
*-- if the gl module is installed then a default posting date must be entered
IF laSetups[4,2] = 'Y' .AND. EMPTY(ldDefPstDate)
  *E300408,1 Message : 40032
  *E300408,1 Must enter a default posting date.
  *E300408,1 Button : 00000
  *E300408,1 Ok
  =gfModalGen('INM40032B00000','ALERT','posting')
  _CUROBJ = OBJNUM(ldDefPstDate) 
  RETURN
ENDIF
IF laSetups[4,2] = 'Y' .AND. ldDefInvDate > ldDefPstDate
  *E300408,1 Message : 40086
  *E300408,1 invoice date cannot be greater than posting date
  *E300408,1 Button : 00000 
  *E300408,1 Ok
  =gfModalGen('TRM40086B00000','ALERT','')
  _CUROBJ = OBJNUM(ldDefPstDate) 
  RETURN
ENDIF

*B804466,1 (Begin) Fix the bug of preventing from making invoices only when linked to GL.
*IF laSetups[4,2]='Y' AND !CHECKPRD(ldDefPstDate,'lcGlYear','lcGlPeriod','IN')
IF !CHECKPRD(IIF(laSetups[4,2]='Y',ldDefPstDate,ldDefInvDate),'lcGlYear','lcGlPeriod','IN')
*B804466,1 (End)

  *B801942,1 Return to posting date
  *llSessDef = .F.
  _CUROBJ = OBJNUM(ldDefPstDate) 
  RETURN
  *B801942,1 (End)
ELSE
  lcIDefWare = laWareHouses[lnWareHouse,2]
  *-- save the new defaults
  IF cbSetAsDef
    SAVE ALL LIKE lcIDef* TO ("INVDEF.MEM")
  ENDIF
ENDIF
CLEAR READ

*!*************************************************************
*! Name      : lfvInvDate
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Invoice date
*!*************************************************************
*! Calls     : CODECHK,lfvTerms,lfUpdate
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvInvDate()
*!*************************************************************
FUNCTION lfvInvDate
*B121026,1 MHM 12/30/2003 Fix Bug of accept saving with empty Invoice Date. [Start]
IF EMPTY(laData[41])
  *-You cannot leave the entered date empty.
  =gfModalGen('INM40178B34000','DIALOG','Invoice')
  _CUROBJ = OBJNUM(laData[41])
  laData[41] = &lcInvHdr..InvDate
  RETURN
ENDIF  
*B121026,1 MHM 12/30/2003 Fix Bug of accept saving with empty Invoice Date.[End] 

IF &lcInvHdr..InvDate <> laData[41]
  IF laData[41] > laData[49]
    *E300408,1 Message : 40086
    *E300408,1 invoice date cannot be greater than posting date
    *E300408,1 Button : 00000 
    *E300408,1 Ok
    =gfModalGen('TRM40086B00000','ALERT','')
    STORE &lcInvHdr..InvDate TO laData[41]
    RETURN
  ENDIF
  =lfvTerms() .AND. lfUpdate()
ENDIF

*!*************************************************************
*! Name      : lfvPostDate
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Posting date
*!*************************************************************
*! Calls     : CODECHK,lfvTerms,lfUpdate
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvPostDate()
*!*************************************************************
FUNCTION lfvPostDate

IF &lcInvHdr..dPostDate <> laData[49]
  *E300408,1 Message : 40086
  *E300408,1 invoice date cannot be greater than posting date
  *E300408,1 Button : 00000 
  *E300408,1 Ok
  *-- ladata[41]= invoice date
  IF (laData[41] > laData[49] .AND. gfModalGen('TRM40086B00000','ALERT','')=1) .OR. ;
    !CHECKPRD(laData[49],'lcGlYear','lcGlPeriod','IN')
    STORE &lcInvHdr..dPostDate TO laData[49]
    RETURN
  ENDIF  
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE dPostDate WITH laData[49]
  UNLOCK
  llCUpdate = .T.
ENDIF

*!*************************************************************
*! Name      : lpClsScr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Cancel new Invoice
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

IF laSetups[4,2]='Y' .AND. USED(lcGlDist)
  USE IN (lcGlDist)
ENDIF
*E301386,1 (Start) Add the folowing line
IF !laScrMode[3]  
*E301386,1  (End)
  SELECT unCmSess  
  *B803747,1 NAD 10/26/2000 (Start) Go to the correct record before updating.
  IF SEEK('O'+PADR('DINVOICE',10)+PADR(gcUser_id,10)+lcSession)
  *B803747,1 NAD (End)
    REPLACE STATUS WITH 'I'
    llContinue = .F.
    UNLOCK
  *B803747,1 NAD 10/26/2000 (Start) Added the end if.
  ENDIF
  *B803747,1 NAD 10/26/2000 End
   
*E301386,1 (Start)
ENDIF           
*E301386,1  (End) 

SELECT InvHdr
*E301386,1 (Start)
*SCATTER FIELDS &lcScFields TO laData BLANK
IF laScrMode[4]
  SCATTER FIELDS &lcScFields TO laData BLANK
ELSE
  SCATTER FIELDS &lcScFields TO laData 
ENDIF
*E301386,1  (End) 
*!*************************************************************
*! Name      : lpSavScr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Save new Invoice
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
PRIVATE llSaveInv

*B606206,1 TMI [Start] If Sales rep not entred for any reason , Deny Saving ( FOR ENG07 )
IF ASCAN(laEvntTrig,PADR('ARSAVEOK',10))<>0 AND ;
  !gfDoTriger('ARDINV',PADR('ARSAVEOK',10))
  RETURN .F.
ENDIF
*B606206,1 TMI [End  ] If Sales rep not entred for any reason , Deny Saving ( FOR ENG07 )

*C037816,1 MHM 04/06/2004 [Start] Save to custom Bin location files. 
IF ASCAN(laEvntTrig,PADR('ARCKSVBN',10))<>0 AND ;
  !gfDoTriger('ARDINV',PADR('ARCKSVBN',10))
  RETURN .F.
ENDIF
*C037816,1 MHM 04/06/2004 [End]

*B602737,1 MAN 04/04/1999 Total Charges is saved with 0 values sometimes
*B602737,1 Recompute Charges before Saving
PRIVATE lcCInvOrd,lnCAlSel  
*E301386,1 (Start) Save the edited fields only
*C102574,1 HBG 04/15/2002 IF Trigger to GMA Validate the selected packs[Begin]
IF ASCAN(laEvntTrig , PADR('UPDTPRCD',10)) <> 0
  =gfDoTriger('ARDINV',PADR('UPDTPRCD',10))
ENDIF
*C102574,1 [End]

IF laScrMode[3] 
  *C200352,1 HBG 04/15/2002 IF Trigger to GMA update 'SHDAEXPR' File of UPS system [Begin] 
  IF ASCAN(laEvntTrig , PADR('UPDCRTAV',10)) <> 0 
    =gfDoTriger('ARDINV',PADR('UPDCRTAV',10))
  ENDIF
  *C200352,1 [End]

  SELECT INVHDR
  =lfEdtInv(lcEdtFld)
  =gfOpenFile(gcDataDir+'DEBIT',gcDataDir+'DEBIT','SH')
  SELECT DEBIT
  IF SEEK(laData[2]+laData[1]+SPACE[3]+DTOS(laData[41]))
    =RLOCK()
    REPLACE DUEDATE WITH laData[7] 
    *B803350,1 (Start)  Edit the Store field in The debit file for GMA
    =gfDoTriger('ARDINV','EDTSTORE')
    *B803350,1 (End) 
    UNLOCK
  ELSE
    =gfOpenFile(gcDataDir+'ARHIST',gcDataDir+'ARHISTT','SH')
    SELECT ARHIST
    IF SEEK(laData[2]+laData[1]+SPACE[3])
      =RLOCK()
      REPLACE DUEDATE WITH laData[7]
      *B803350,1 (Start)  Edit the Store field in The ARHIST  file for GMA
      =gfDoTriger('ARDINV','EDTSTORE')
      *B803350,1 (End)
      UNLOCK
    ENDIF  
  ENDIF
  
  *B803350,1 (Start) 1-Save the store field in the INVLINE,UPSBOX, REPCOMM for GMA.
  *B803350,1         2-Select the invhdr because the global function 'gfCPSave' will update        
  *B803350,1         the fields  lLok_Stat, cLok_User, dLok_Date, cLok_Time .
  =gfDoTriger('ARDINV','SAVSTORE')
  SELECT INVHDR
  *B803350,1  (End)   
  RETURN
ENDIF
*E301386,1 (End)

lnCAlSel = SELECT(0)
SELECT (lcInvHdr)
IF !llIsEngland .AND. lCompUps
    lcCInvOrd = ORDER()
  *B604425,1 NNA 12/31/2003 Return The HST Rate And The Hst Amnt. With Their Values If Zero Or other [Begin]
  *DO gpUpsChrg IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
      laData[2],laData[5],laData[3],ladata[30],laData[21],laData[31],llCod,;
      llupsinsur,laData[32],laData[46],laData[44],laData[45],lcUpsBox,'laData[10]',;
      'laData[28]','laData[34]','laData[35]','laData[36]','laData[25]','laData[26]',;
      'laData[37]','laData[43]',IIF(UPPER(ALLTRIM(gcContCode))='USA',;
      lnTaxDue,laData[31]),"A"
  DO gpUpsChrg IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
     laData[2],laData[5],laData[3],ladata[30],laData[21],laData[31],llCod,;
     llupsinsur,laData[32],laData[46],laData[44],laData[45],lcUpsBox,'laData[10]',;
     'laData[28]','laData[34]','laData[35]','laData[36]','laData[25]','laData[26]',;
     'laData[37]','laData[43]',IIF(UPPER(ALLTRIM(gcContCode))='USA',;
     lnTaxDue,laData[31]),"A",laData[51],'laData[52]'
  *B604425,1 NNA [End]
  SELECT (lcInvHdr)
  SET ORDER TO TAG (lcCInvOrd)  
ENDIF  

*E302472,1 MHM 11/18/2007  Multiple Taxes for Canada on Invoice [T20060709.0008] [Start]
IF llIsCanada AND LASCRMODE[4]
  llRpHSTTax = .F.
  =lfGetHST()
  IF !(laData[51] <> 0 AND laData[45] = 0) 
    IF llRpHSTTax
      laData[52] = laData[43]
      laData[51] = laData[45]
      laData[37] = 0
      laData[43] = 0
      laData[45] = 0
      laData[46] = 0
    ELSE
      laData[52] = 0
    ENDIF
  ENDIF
ENDIF
*E302472,1 MHM [End]

*B802653,1 (Start)
*laData[38] = laData[31]+laData[34]+laData[35]+laData[36] - ;
              ABS(laData[31]*laData[32]/100)+laData[37]+laData[43]
*laData[33] = -1*laData[31]*laData[32]/100

*B603341,1 SSH 12/15/1999 Fix the bug of neglect extra charge amount.
*laData[38] = laData[31]+laData[34]+laData[35]+laData[36] - ;
              ROUND(ABS(laData[31]*laData[32]/100),2)+laData[37]+laData[43]
*-- Balance to pay
laData[38] = laData[31]+laData[34]+laData[35]+laData[36] - ;
              ROUND(ABS(laData[31]*laData[32]/100),2)+laData[37]+laData[43]+laData[39]
*C102212,1 (Begin) Add HST Tax.
laData[38] = laData[38] + laData[52]
*C102212,1 (End)

*B603341,1 SSH (End)
*-- Merch discount
laData[33] = ROUND((-1*laData[31]*laData[32]/100),2)
*B802653,1 (End)

SELECT (lcInvHdr)

*B602905,1 Default No. of cartons must be 1 at least [Begin]
laData[25] = IIF(laData[25]=0,1,laData[25])
*B602905,1 Default No. of cartons must be 1 at least [End  ]

*C037814,1  TMI [Start] Update total charges field with the Additional charges custom field for REL01
IF ASCAN(laEvntTrig , PADR('ADCHRG',10)) <> 0
  =gfDoTriger('ARDINV',PADR('ADCHRG',10)) 
ENDIF  
*C037814,1  TMI [End  ] 
=RLOCK()
REPLACE Ship     WITH laData[42] ,;
        ShipAmt  WITH laData[31] ,;
        Discount WITH laData[33] ,;
        Freight  WITH laData[34] ,;
        Insur    WITH laData[35] ,;
        Cod      WITH laData[36] ,;
        Cod_Amt  WITH laData[28] ,; 
        Weight   WITH laData[26] ,;
        Tax_Amt  WITH laData[37] ,;
        nPstAmt  WITH laData[43] ,;
        Cartons  WITH laData[25] ,;
        TotalChg WITH laData[38] ,;
        Cod_Flag WITH IIF(llCod,'Y','N') ,;
        lKeyOff  WITH IIF(llCod,lKeyOff,.F.) ,;
        lCompUps WITH .F.

*B603998,1 Update the HST Tax amount. [Begin]
REPLACE nHstAmt WITH laData[52]
*B603998,1 Update the HST Tax amount. [End]

*E302472,1 MHM 11/18/2007  Multiple Taxes for Canada on Invoice [T20060709.0008] [Start]
IF llIsCanada AND LASCRMODE[4]
  llRpHSTTax = .F.
  =lfGetHST()
    IF llRpHSTTax
      REPLACE nHstRate WITH laData[51],;
              Tax_Rate WITH 0 ,;
              nPstRate WITH 0 
    ELSE
      REPLACE nHstRate WITH 0
              
    ENDIF
ENDIF

*E302472,1 MHM [End]



SELECT (lnCAlSel)
*B602737,1 END
*-- Calculate merchendise tax
=IIF(llIsEngland AND llReMerTax ,lfMechTax(),.T.)
llSaveInv = .T.
*-- check if the invoice data is valid or not
DO gfChkSavInv IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
laData[2],laData[3],laData[5],laData[30],lcInvHdr,IIF(USED(lcInstHdr),lcInstHdr,''),;
IIF(USED(lcAppCrdt),lcAppCrdt,''),IIF(USED(lcUpsBox),lcUpsBox,''),'llSaveInv'
IF !llSaveInv
*-- cannot save the invoice
  llCSave = .F.
  SELECT (IIF(lnActFolder=2,lcInvLine,lcInvHdr))
  RETURN
ENDIF
*--start saving
SELECT UNCMSESS
REPLACE cCurrObj WITH 'pbSav'
*B603119,1 (Start) nad Close the file 
USE IN  UNCMSESS
*B603119,1 (End)  nad
*B803369,1  (Start) Open the uncomplete session file.
=gfOpenFile(gcDataDir+'UNCMSESS','TRANS','SH')
=SEEK ('O'+PADR('DINVOICE',10)+PADR(gcUser_id,10)+lcSession,'UNCMSESS')
*B803369,1  (End)
DECLARE laInv[1]
STORE '' TO laInv
DO gpSaveInv IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
lcInvHdr,lcInvLine,IIF(USED(lcUpsBox),lcUpsBox,''),IIF(USED(lcEngChrg),lcEngChrg,''),;
IIF(USED(lcInstHdr),lcInstHdr,''),IIF(USED(lcInstLin),lcInstLin,''),'',lcGlSession,'laInv'
*B803369,1 (Start) Open the uncomplete session file before gpSaveInv.
*B603119,1 (Start) nad
*=gfOpenFile(gcDataDir+'UNCMSESS','TRANS','SH')
*B603119,1 (End)
*B803369,1 (End)
SELECT unCmSess
*B603119,1 (Start) nad
IF SEEK('O'+PADR('DINVOICE',10)+PADR(gcUser_id,10)+lcSession)
*B603119,1 (End) nad
  REPLACE STATUS WITH 'C'
  llContinue = .F.
  UNLOCK
*B603119,1 (Start) nad
ENDIF
*B603119,1 (End) nad
*E300408,1 Message : 40005
*E300408,1 Invoice has been saves as.
*E300408,1 Button : 00000 
*E300408,1 Ok
=gfModalGen('INM40005B00000','ALERT',laInv[1])
*:B602753,1 HDM 04/07/1999 [Start] Stop Calling NotePad Program In lpSavScr as the global save
*:                         Will Call it

*=IIF(gfModalGen("QRM00271B00006") = 1, NotePad('C',laInv[1]), .T.)
*:B602753,1 HDM 04/07/1999 [End] Stop Calling NotePad Program In lpSavScr as the global save
*:                         Will Call it

*B603134 (Start)
*-- invoice number
laData[1] = laInv[1]
*B603134 (End)
SELECT INVHDR
*C102574,1 HBG 04/15/2002 IF Trigger to GMA update order lines created from this invoice 
*C102574,1                and if there is price variance,Make credit or debit Adj[Begin]
IF ASCAN(laEvntTrig , PADR('UPDTORDD',10)) <> 0 
 *-- Update Ordline new fields
 =gfDoTriger('ARDINV',PADR('UPDTORDD',10))
ENDIF
*C102574,1 [End]

*C037816,1 MHM 04/06/2004 Save Invoice [Start]
IF ASCAN(laEvntTrig , PADR('DLSAVINV',10)) <> 0 
 =gfDoTriger('ARDINV',PADR('DLSAVINV',10))
ENDIF
*C037816,1 MHM [End]

*B603641,1 Print invoice after enter note pad
*IF lcIDefPrnt
*  *man
*  *_CUROBJ = OBJNUM(pbcpPrint)
*  *KEYBOARD "{ENTER}"
*  =gfCPPrint()
*ENDIF
*B603641,1 (End)

RETURN
*-- end of lpsavscr.

*!*************************************************************
*! Name      : lfGetInfo
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Restore invoice information
*!*************************************************************
*! Calls     : gfGetAdr,lfBrowDet
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfGetInfo()
*!*************************************************************
FUNCTION lfGetInfo
PRIVATE lcHdrFile

lcHdrFile = IIF(laScrMode[2],'INVHDR',lcInvHdr)
=SEEK(IIF(laScrMode[2],laData[1],laData[2]+laData[3]+laData[5]+laData[30]),lcHdrFile)
=SEEK(IIF(EMPTY(laData[5]),'M'+laData[2],'S'+laData[2]+laData[5]),'Customer')

*B802241 (Start)
PRIVATE lnCusRecNo,lcDistCtr
lnCusRecNo = RECNO('Customer')
*-- get the store which is defined as distribution center for the customer
IF !EMPTY(Customer.Dist_Ctr)
  lcDistCtr = Customer.Dist_Ctr
  llNoThing = SEEK('S'+laData[2]+lcDistCtr,'Customer')
ENDIF
*B802241 (End)

=SEEK('O'+laData[3],'OrdHdr')

*B603119,1 (Start)  Commented out
*=IIF(laScrMode[4],gfRltFld(laData[9],@laTRltFld,'CTERMCODE'),.T.)

*B803397,1 (Start) Check if the instalment is yes or no in the term code 
DECLARE laInsRltd[1,2]
laInsRltd[1,1]='LINSTALLM'
laInsRltd[1,2] = 'llInstTerm'
*=IIF(!laScrMode[4],gfRltFld(laData[9],@laInsRltd,'CTERMCODE'),.T.)
=gfRltFld(laData[9],@laInsRltd,'CTERMCODE')
*B803397,1 (End)

*B603119,1 (End)
*-- get the ship address or the alternate ship address
lnShipAddr = IIF(laScrMode[4],lnShipAddr,IIF(OrdHdr.Alt_ShpTo,2,1))
STORE '' TO  lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,;
             lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5
lcBillName = Customer.BtName
=gfGetAdr('Customer','','','',1,'2')
*-- get the bill address
FOR lnCount = 1 TO ALEN(laAddress,1)
  lcCount = STR(laAddress[lnCount,1],1)
  lcBillAdd&lcCount = lcBillAdd&lcCount + IIF(EMPTY(lcBillAdd&lcCount),'',',')+;
  SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
ENDFOR  
IF lnShipAddr = 2
*-- Alternate ship address
  lcShipName = IIF(laScrMode[2],OrdHdr.StName,&lcInvHdr..StName)
  lcShipAdd1 = IIF(laScrMode[2],OrdHdr.cAddress1,&lcInvHdr..cAddress1)
  lcShipAdd2 = IIF(laScrMode[2],OrdHdr.cAddress2,&lcInvHdr..cAddress2)
  lcShipAdd3 = IIF(laScrMode[2],OrdHdr.cAddress3,&lcInvHdr..cAddress2)
  lcShipAdd4 = IIF(laScrMode[2],OrdHdr.cAddress4,&lcInvHdr..cAddress4)
  lcShipAdd5 = IIF(laScrMode[2],OrdHdr.cAddress5,&lcInvHdr..cAddress5)
ELSE
  lcShipName =  IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba)
  =gfGetAdr('CUSTOMER','','','',1,'')
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcShipAdd&lcCount = lcShipAdd&lcCount + IIF(EMPTY(lcShipAdd&lcCount),'',',')+;
    SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
  ENDFOR  
ENDIF  

*B802241 (Start)
IF RECCOUNT('Customer') >= lnCusRecNo
  GO lnCusRecNo IN Customer
ENDIF
*B802241 (End)  

SELECT (lcHdrFile)
*-- Restore invoice status
DO CASE
  CASE &lcHdrFile..STATUS = 'V'
    lcInvStat = 'Void'    
    lcStatColor = "RGB(255,255,255,255,0,0)"
    SCATTER FIELDS &lcVScFields TO laData
  CASE &lcHdrFile..STATUS = 'O'
    lcInvStat = 'Open'
    lcStatColor = "RGB(0,0,0,255,255,0)"
  CASE &lcHdrFile..STATUS = 'C'
    lcInvStat = 'Complete'
    lcStatColor = "RGB(255,255,255,0,128,0)"
  OTHERWISE
    lcInvStat = ''
    lcStatColor = gcObjColor
ENDCASE
*-- Restore cash on delivery
llCod = &lcHdrFile..Cod_Flag='Y'
lnPstRule = VAL(laData[44])
=gfwCodePop(@laCodes,'CTERMCODE','T')
=gfwCodePop(@laCodes,'SHIPVIA','T')
=gfwCodePop(@laCodes,'SPCINST','T')
=gfwCodePop(@laCodes,'SEASON','T')
=gfwCodePop(@laCodes,'CDIVISION','T')
lnWareHouse = ASCAN(laWareHouses,laData[21])
lnWareHouse = IIF(lnWareHouse=0,1,ASUBSCRIPT(laWareHouses,lnWareHouse,1))
*E300889,1 (Start) Read POS warehous insted.
lnWareHouse = IIF(llPOSSetup,lnPOSWeHs,lnWareHouse)
*E300889,1 (End).
llReBrowse = .T.
=lfActFolder()

*!*************************************************************
*! Name      : lfvLineNote
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Invoice line notepad
*!*************************************************************
*! Calls     : INV200LN.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvLineNote()
*!*************************************************************
FUNCTION lfvLineNote
*E301386,1 (Start)  Change the Variable to Disable the notes screen in the edit mode
laScrMode[2]=IIF(laScrMode[3],.T.,laScrMode[2])
*E301386,1 (End)

DO (gcScrDir+"ARLNOTES.SPX")

*E301386,1 (Start)  Restore the screen mode
laScrMode[2]=IIF(laScrMode[3],.F.,laScrMode[2])
*E301386,1 (End)
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
SET SKIP OF PAD _INQUIRY OF _MSYSMENU laScrMode[1]
ON PAD _INQUIRY OF _msysmenu ACTIVATE POPUP _INQURYPOP

DEFINE POPUP _INQURYPOP MARGIN SHADOW
DEFINE BAR 1  OF _INQURYPOP PROMPT 'In\<voice Notes'         SKIP FOR !laScrMode[2]
DEFINE BAR 2  OF _INQURYPOP PROMPT '\<Object Link'           SKIP FOR !laScrMode[2]
DEFINE BAR 3  OF _INQURYPOP PROMPT 'Send \<EDI Invoice File' SKIP FOR !laScrMode[2]
DEFINE BAR 4  OF _INQURYPOP PROMPT 'Customer \<Inquire'   SKIP FOR EMPTY(laData[2])
DEFINE BAR 5  OF _INQURYPOP PROMPT 'Customer \<Notepad'   SKIP FOR EMPTY(laData[2])
DEFINE BAR 6  OF _INQURYPOP PROMPT '\<Aging Summary'      SKIP FOR EMPTY(laData[2])
DEFINE BAR 7  OF _INQURYPOP PROMPT '\<Style Inquire'      SKIP FOR (lnactfolder<>2) .OR. EMPTY(m.Style)
DEFINE BAR 8  OF _INQURYPOP PROMPT '\<Line Notepad'       SKIP FOR (lnactfolder<>2) .OR. laSetups[3,2]<>'Y' .OR. EMPTY(m.Style)
DEFINE BAR 9  OF _INQURYPOP PROMPT '\<Copy Line'          SKIP FOR (lnactfolder<>2) .OR. laScrMode[2] .OR. EMPTY(m.Style) .OR. llZoom
DEFINE BAR 10 OF _INQURYPOP PROMPT '\<Paste Line'         SKIP FOR (lnactfolder<>2) .OR. laScrMode[2] .OR. EMPTY(m.Style) .OR. llZoom
DEFINE BAR 11 OF _INQURYPOP PROMPT '\<Zoom Invoice Lines' SKIP FOR (lnactfolder<>2)
*B603395,1 [Start] new bar if calculate Sales Rep. commesion for lines and add mode
DEFINE BAR 12 OF _INQURYPOP PROMPT '\<Refresh Invoice Sales Rep. Commission' SKIP FOR laSetups[2,2] <> 'Y' OR !laScrMode[4]
*B603395,1 [End]
*C102574,1 HBG 04/15/2002 IF Trigger to GMA add bar to option menu to allow show lines by pack[Begin]
*B037436,1 MHM 01/20/2004 Fix bug of goto infinit loop if you open Direct Invoice[Start] 
*IF ASCAN(laEvntTrig , PADR('DIFBARD',10)) <> 0
*  =gfDoTriger('ARDINV',PADR('DIFBARD',10))
*ENDIF  
IF ASCAN(laEvntTrig , PADR('VEWBARD',10)) <> 0
  =gfDoTriger('ARDINV',PADR('VEWBARD',10))
ENDIF  
*B037436,1 MHM 01/20/2004 [End] 
*C102574,1 [End]

*C102314,1 BWA 06/10/2001 Add a triger functions for the custome charges screen.[START]
IF ASCAN(laEvntTrig , PADR('FOLDERINV',10)) <> 0
  =gfDoTriger('ARDINV',PADR('FOLDERINV',10))
ENDIF
*C102314,1 BWA 06/10/2001 [END]
*C037814,3  TMI [Start] Add trigger to open MoreCharges screen for REL01
IF ASCAN(laEvntTrig , PADR('MORCHG',10)) <> 0
  =gfDoTriger('ARDINV',PADR('MORCHG',10))
ENDIF
*C037814,3  TMI [End  ] 
*C037816,1 MHM 04/06/2004 Void Invoice[Start]
IF ASCAN(laEvntTrig , PADR('ARDFNMNU',10)) <> 0
  =gfDoTriger('ARDINV',PADR('ARDFNMNU',10)) 
ENDIF  
*C037816,1 MHM [End]

SET MARK OF BAR 11 OF _INQURYPOP TO llZoom
ON SELECTION POPUP _INQURYPOP DO lpvInquiry

*!*************************************************************
*! Name      : lpvInquiry
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : None
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
  CASE BAR() = 1      && Invoice Notes
  	=lfvInvNote()
  CASE BAR() = 2      && Object Link
    =lfvObjLink()
  CASE BAR() = 3      && Send EDI Invoice File
    =lfvSend()
  CASE BAR() = 4      && Customer Inquire
  	lcParameter = "'"+laData[2]+"'"+IIF(EMPTY(laData[5]),'',",'"+laData[5]+"'")
    DO gpDoProg WITH 'AWRARCUST',.F.,'AR',lcParameter
  CASE BAR() = 5      && Customer Notepad
    =NOTEPAD('A',laData[2])  
  CASE BAR() = 6      && Customer Credit
    =SEEK('M'+ladata[2],'Customer')
    DO (gcScrDir+gcWinAppl+"\ARAGING.SPX") WITH laData[8]
  CASE BAR() = 7      && Style Inquire
  	lcParameter = "'"+m.Style+"',''"
    DO gpDoProg WITH 'AWRICSTYLE',.F.,'IC',lcParameter
  CASE BAR() = 8      &&  Invoice Line Notepad
    =lfvLineNote()
  CASE BAR() = 9      &&  Copy Invoice Line
   =lfvCopyQty()
  CASE BAR() = 10      &&  Paste Invoice LIne
   =lfvPasteQty()
  CASE BAR() = 11     &&   Zoom Invoice Browse
    llZoom = !llZoom
    =lfvBrowZoom()
    SET MARK OF BAR 11 OF _INQURYPOP TO llZoom
  *B603395,1 [Start] 
  CASE BAR() = 12     && Refresh sales rep. header commession befor saving
    =lfRefComm()  
  *B603395,1 [End] 

  *C102314,1 BWA 06/10/2001 Add a triger functions for the custome charges screen.[START]
  *--Show the charge screen for the selected Invoice.
  CASE BAR() = 13
    *C102574,1 HBG 04/15/2002 IF Trigger to GMA validate bar in option menu of allowing
    *C102574,1                show lines by pack[Begin]
    IF ASCAN(laEvntTrig , PADR('VLDBARD',10)) <> 0
      =gfDoTriger('ARDINV',PADR('VLDBARD',10))
    ENDIF
    *C102574,1 [End]

    IF ASCAN(laEvntTrig , PADR('DINVCHRG',10)) <> 0
      =gfDoTriger('ARDINV',PADR('DINVCHRG',10))
    ENDIF
    *C102314,1 BWA 06/10/2001 [END]
  *C102574,1 HBG 04/15/2002 IF Trigger to GMA validate bar in option menu of allowing
  *C102574,1                show lines by pack[Begin]
  CASE BAR() = 14
    IF ASCAN(laEvntTrig , PADR('VLDSUMD',10)) <> 0
      =gfDoTriger('ARDINV',PADR('VLDSUMD',10))
    ENDIF
    *C102574,1 [End]

ENDCASE

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
  =lfBrowDet()
ELSE
  llReBrowse=.T.
  =lfActFolder()
ENDIF

*!*************************************************************
*! Name      : lfvNew
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : New Invoice line
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

STORE .T. TO llNewline,llAddLine
SCATTER MEMVAR BLANK MEMO
m.Style = lcLastStyle
SHOW GETS WINDOW (lcWinCh32) DISABLE ONLY
SHOW GETS WINDOW (lcWinCh34) DISABLE ONLY
SHOW GETS WINDOW (lcWinCh35) DISABLE ONLY
=lfRefresh(lcWinCh32)
=lfRefresh(lcWinCh34)
SHOW GET ibStyle ENABLE
SHOW GET m.Style ENABLE
_CUROBJ = OBJNUM(m.Style)

*!*************************************************************
*! Name      : lfvRemove
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Remove Invoice line
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvRemove()
*!*************************************************************
FUNCTION lfvRemove

*C102574,1 HBG 08/05/2002 If trigger of GMA remove the all pack when removing line in it[Begin]
IF ASCAN(laEvntTrig , PADR('REMPACK',10)) <> 0
  =gfDoTriger('ARDINV',PADR('REMPACK',10))
  RETURN
ENDIF
*C102574,1 [End]

*E300408,1 Message : 40011
*E300408,1 Are you sure you want to remove this xxx?
*E300408,1 Button : 40000
*E300408,1 Yes No
IF gfModalGen('QRM40011B40000','ALERT','invoice line')= 1
  *--decrese the quantity per carton with the style quantity
  lnCartons  = lnCartons  - IIF(Style.Qty_Ctn>0,m.TotQty/Style.Qty_Ctn,0)
  laData[25] = CEILING(lnCartons)

  *B602905,1 Default No. of cartons must be 1 at least [Begin]
  laData[25] = IIF(laData[25]=0,1,laData[25])
  *B602905,1 Default No. of cartons must be 1 at least [End  ]
  *-- decrease the total wieght with style wieght
  laData[26] = laData[26] - m.TotQty*Style.nStyWeight
  *-- decrease the shipped amount 
  laData[31] = laData[31] - m.TotQty*m.Price
  *B604418,1 NNA 12/30/2003 if Statment to refresh the next fields when ship amount become zero [Begin]  
  IF laData[31] = 0
     laData[34] = 0		&& The Fright Field
	 laData[37] = 0		&& The G.S.T Field
	 laData[43] = 0		&& The P.S.T Amount Field
	 laData[52] = 0		&& The H.S.T Tax Field
	 laData[35] = 0     && The U.P.S Insur. Field
	 laData[36] = 0     && The C.O.D Charge Field
	 laData[28] = 0     && The C.O.D Amount Field 
  ENDIF						
  *B604418,1 NNA 12/30/2003 [End]
  
  *B603293,1 (Start)
  *laData[33] = -laData[31]*laData[32]/100
  *-- merchandise discount
  laData[33] = -ROUND(laData[31]*laData[32]/100,2)
  *B603293,1 (End)
  *-- approval amount =Shipped amount
  laData[48] = laData[31]
  *-- decrease shipped quantyties
  laData[42] = laData[42] - m.TotQty
  *-- if this style was taxable then decrease the tax amount
  lnTaxDue   = IIF(lTaxable,lnTaxDue-m.TotQty*m.Price,lnTaxDue)
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE CARTONS  WITH laData[25] ,;
          WEIGHT   WITH laData[26] ,;
          SHIPAMT  WITH laData[31] ,;
          Discount WITH laData[33] ,;
          SHIP     WITH laData[42] ,;
          APPRAMT  WITH laData[48] ,;
          nTaxDue  WITH lnTaxDue   ,;
          lCompUps WITH .T.    
  UNLOCK
  SELECT (lcInvLine)
  
  IF laSetups[9,2]='Y' .AND. llIsEngland .AND. Style.nTaxBreak <> 0
    lnQuantity = 0
    FOR lnCount = Style.nTaxBreak TO 8
    *-- Sum the quantity for only the sizes that is Greater than the tax break weight
      lnQuantity = lnQuantity+EVAL('m.Qty'+STR(lnCount,1))
    ENDFOR
    *-- Decrease Merchandise tax
    lnMerchTax = lnMerchTax - lnQuantity * m.Price * nTaxRate/100
  ENDIF
  SELECT (lcInvLine)
  DELETE
  GO TOP
  *-- change  the status  for the screen fields
  lcDispDet = IIF(EOF(),'DISABLE','')
  SCATTER MEMVAR
  SHOW WINDOW (lcInvBrow) REFRESH SAME
  SHOW GETS WINDOW (lcWinCh32) &lcDispDet ONLY
  SHOW GETS WINDOW (lcWinCh34) &lcDispDet ONLY
  SHOW GETS WINDOW (lcWinCh35) &lcDispDet ONLY
  =lfRefresh(lcWinCh32)
  =lfRefresh(lcWinCh34)
  IF EOF()
    SHOW GET pbNew ENABLE 
    SHOW GET pbPacks ENABLE
  ENDIF
  STORE .T. TO llReMerTax,llCUpdate
  *C037816,1 MHM 04/06/2004 Remove Lines from invoice[Start]
  *--mhm123853 
  *IF ASCAN(laEvntTrig , PADR('DLREMLIN',10)) <> 0
  *  =gfDoTriger('ARDINV',PADR('DLREMLIN',10))
  *ENDIF
  *C037816,1 MHM [End]
  
ENDIF

*!*************************************************************
*! Name      : lfvPacks
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Calls packs screen
*!*************************************************************
*! Calls     : lfAccPacks,gfModalGen,arPacks.SPX
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvPacks()
*!*************************************************************
FUNCTION lfvPacks
PRIVATE lnAlias 

lnAlias = SELECT()
*E301077,51 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'SPck_Hdr',gcDataDir+'SPck_Hdr','SH')
=gfOpenFile(gcDataDir+'SPCK_LIN',gcDataDir+'SPCK_LIN','SH')
*C102574,1 HBG 04/15/2002 IF Trigger to GMA Open Pack header and pack line by the new index[Begin]
IF ASCAN(laEvntTrig , PADR('OPENFILE',10)) <> 0
  =gfDoTriger('ARDINV',PADR('OPENFILE',10))
ENDIF
*C102574,1 [End]

*E301077,51 (End)

SELECT SPck_Lin
SET RELATION TO Style INTO Style
SET RELATION TO Style+laData[21]+SPACE(10) INTO StyDye ADDITIVE
IF !lfAccPacks()
  *E300408,1 Message : 40034
  *E300408,1 There are neither generic packs nor packs for account xxx.
  *E300408,1 Button : 00000
  *E300408,1 Ok
  =gfModalGen('INM40034B00000','ALERT',laData[2])
  SELECT (lnAlias)
  RETURN
ENDIF
lcAccount = laData[2]
SELECT (lcPackHdr)
SET RELATION TO Type+Account+Pack_Id INTO SPck_Lin
*C102574,1 HBG 04/15/2002 IF Trigger to GMA set the releation to the new key[Begin]
IF ASCAN(laEvntTrig , PADR('SETRELTM',10)) <> 0
  =gfDoTriger('ARDINV',PADR('SETRELTM',10))
ENDIF
*C102574,1 [End]

DO (gcScrDir+"arPacks.SPX")
SELECT (lcPackHdr)
USE IN (lcPackHdr)
ERASE (gcWorkDir+lcPackHdr+".DBF")
ERASE (gcWorkDir+lcPackHdr+".CDX")

*E301077,51 Inhance openning files to speed up transaction
=gfCloseFile('SPck_Hdr')
=gfCloseFile('SPck_Lin')
*E301077,51 (End)

IF SEEK(laData[2]+laData[3]+laData[5]+laData[30],lcInvline)
  SHOW GETS WINDOW (lcWinCh32) ENABLE ONLY
  SHOW GETS WINDOW (lcWinCh34) ENABLE ONLY
  SHOW GETS WINDOW (lcWinCh35) ENABLE ONLY
  _CUROBJ = OBJNUM(ibInv200E)
ENDIF
SELECT (lnAlias)

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

*C102574,1 HBG 04/15/2002 IF Trigger to GMA creat the index with the new key[Begin]
IF ASCAN(laEvntTrig , PADR('CRTFILE',10)) <> 0
  =gfDoTriger('ARDINV',PADR('CRTFILE',10))
ENDIF
*C102574,1 [End]

lnPacks = 0
SELECT SPck_Hdr
SET RELATION TO Type+Account+Pack_Id INTO SPck_Lin
*C102574,1 HBG 04/15/2002 IF Trigger to GMA set the releation to the new key[Begin]
IF ASCAN(laEvntTrig , PADR('SETRELMS',10)) <> 0
  =gfDoTriger('ARDINV',PADR('SETRELMS',10))
ENDIF
*C102574,1 [End]

IF SEEK('P'+laData[2],'SPck_Hdr')
  SELECT SPck_Hdr
  
  *C102574,1 HBG 04/15/2002 Optimize the collection of packs.[Back]
  lcScnExpr = "Type+Account ='P'+laData[2]"
  IF ASCAN(laEvntTrig , PADR('SCANPCKA',10)) <> 0
    =gfDoTriger('ARDINV',PADR('SCANPCKA',10))
  ENDIF
  *SCAN REST WHILE Type+Account='P'+laData[2] ;
       FOR IIF(ALLTRIM(laData[12])='*',.T.,Season=laData[12]) .AND. cDivision=laData[13]
  SCAN REST WHILE &lcScnExpr  ;
       FOR IIF(ALLTRIM(laData[12])='*',.T.,Season=laData[12]) .AND. cDivision=laData[13]
  *C102574,1 [End]
    SELECT SPck_Lin
    SUM REST WHILE Type+Account+Pack_Id='P'+laData[2]+Spck_Hdr.Pack_Id ;
                   StyDye.TotStk,StyDye.TotOrd,StyDye.TotWip,Spck_Lin.TotQty ;
                   TO lnPackStk, lnPackOrd, lnPackWip, lnPackQty
    INSERT INTO (lcPackHdr) ;
    (Type,Account,Pack_Id,Desc,Season,cDivision,TotWip,TotStk,TotOrd,TotQty);
     VALUES ('P',SPck_Hdr.Account,SPck_Hdr.Pack_Id,SPck_Hdr.Desc,SPck_Hdr.Season,;
            SPck_Hdr.cDivision,lnPackWip,lnPackStk,lnPackOrd,lnPackQty)
    *C102574,1 HBG 04/15/2002 IF Trigger to GMA Change the key to the new one[Begin]
    IF ASCAN(laEvntTrig , PADR('UPDFILEA',10)) <> 0
      =gfDoTriger('ARDINV',PADR('UPDFILEA',10))
    ENDIF
    *C102574,1 [End]

    lnPacks = lnPacks + 1
  ENDSCAN
ENDIF
IF SEEK('P*****','SPck_Hdr')
  SELECT SPck_Hdr
  *C102574,1 HBG 04/15/2002 Optimize the collection of packs.[Back]
  lcScnExpr = "Type+Account ='P*****'"
  IF ASCAN(laEvntTrig , PADR('SCANPCKG',10)) <> 0
    =gfDoTriger('ARDINV',PADR('SCANPCKG',10))
  ENDIF
  *SCAN REST WHILE Type+Account='P*****' ;
       FOR IIF(ALLTRIM(laData[12])='*',.T.,Season=laData[12]) .AND. cDivision=laData[13]
  SCAN REST WHILE &lcScnExpr  ;
       FOR IIF(ALLTRIM(laData[12])='*',.T.,Season=laData[12]) .AND. cDivision=laData[13]
  *C102574,1 [End]
    SELECT SPck_Lin
    SUM REST WHILE Type+Account+Pack_Id='P*****'+Spck_Hdr.Pack_Id ;
                   StyDye.TotStk,StyDye.TotOrd,StyDye.TotWip,Spck_Lin.TotQty ;
                   TO lnPackStk, lnPackOrd, lnPackWip, lnPackQty
    INSERT INTO (lcPackHdr) ;
    (Type,Account,Pack_Id,Desc,Season,cDivision,TotWip,TotStk,TotOrd,TotQty);
     VALUES ('P',SPck_Hdr.Account,SPck_Hdr.Pack_Id,SPck_Hdr.Desc,SPck_Hdr.Season,;
            SPck_Hdr.cDivision,lnPackStk, lnPackOrd, lnPackWip, lnPackQty)
    *C102574,1 HBG 04/15/2002 IF Trigger to GMA Change the key to the new one[Begin]
    IF ASCAN(laEvntTrig , PADR('UPDFILEG',10)) <> 0
      =gfDoTriger('ARDINV',PADR('UPDFILEG',10))
    ENDIF
    *C102574,1 [End]

    lnPacks = lnPacks + 1
  ENDSCAN
ENDIF
SELECT SPck_Hdr
SET RELATION OFF INTO SPck_Lin
GO TOP IN (lcPackHdr)
RETURN(lnPacks>0)

*!*************************************************************
*! Name      : lfwAccPacks
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Browse Account and generic packs
*!*************************************************************
*! Calls     : gfCodDes,lfSAccPack,lfPackDet
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfwAccPacks()
*!*************************************************************
FUNCTION lfwAccPacks

SELECT (lcPackHdr)
lnTopMark = RECNO()
*C102574,1 HBG 04/15/2002 IF Trigger to GMA BROWSE FIELDS will be 
*C102574,1                changed to add the new fields of the key[Begin]
*BROWSE FIELDS ;
       cMarker  =IIF(RECNO()=lnTopMark,'>',' '):H=' ':R:1:W=.F.,;
       Account  :H='Account' :R,;
       Pack_Id  :H='Pack Id' :R ,;
       Desc     :H='Description':R ,;
       PackQty  :H='Pack' ,;
       TotWip   :H='Wip':R,;
       TotStk   :H='Stock':R,;
       TotOrd   :H='Ordered' :R,;
       TotQty   :H='Qty.' :R,;
       lcSesDesc=gfCodDes(Season,'SEASON') :H="Season" :R :20 ,;
       lcDivDesc=gfCodDes(cDivision,'CDIVISION') :H="Division" :20 :R;
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

lcFields = [cMarker  =IIF(RECNO()=lnTopMark,'>',' '):H=' ':R:1:W=.F.,]+;
           [Account  :H='Account' :R,]+;
           [Pack_Id  :H='Pack Id' :R,]+;
           [Desc     :H='Description':R,]+;
           [PackQty  :H='Pack' ,]+;
           [TotWip   :H='Wip':R,]+;
           [TotStk   :H='Stock':R,]+;
           [TotOrd   :H='Ordered' :R,]+;
           [TotQty   :H='Qty.' :R,]+;
           [lcSesDesc=gfCodDes(Season,'SEASON') :H="Season" :R :20 ,]+;
           [lcDivDesc=gfCodDes(cDivision,'CDIVISION') :H="Division" :20 :R]
IF ASCAN(laEvntTrig , PADR('BROWPKFL',10)) <> 0
  =gfDoTriger('ARDINV',PADR('BROWPKFL',10))
ENDIF

BROWSE FIELDS &lcFields;
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
*C102574,1 [End]

SELECT Spck_Lin
lnBotMark = RECNO()
BROWSE FIELDS ;
       cMarker  =IIF(RECNO()=lnBotMark,'>',' '):H=' ':R:1:W=.F.,;
       Style :H = lcStyHdr :R ,;
       StyDye.TotWip :H= 'WIP' :R ,;
       StyDye.TotStk :H= 'Stock' :R ,;
       StyDye.TotOrd :H= 'Ordered' :R ,;
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

*!*************************************************************
*! Name      : lfPackDet
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Show pack details
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
lnBotMark = RECNO('Spck_Lin')
SHOW WINDOW (lcBrTtl2) REFRESH SAME

*!*************************************************************
*! Name      : lfSAccPack
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Show Account and generic packs
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

*!*************************************************************
*! Name      : lfReadAct
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : READ Activate function of invoice screen
*!*************************************************************
*! Calls     : gfStopBrow,lfClearKey.
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
IF WONTOP() <> lcWinCh1 .AND. WONTOP() <> lcFolder .AND. WONTOP() <> 'GWCCONTRL1' .AND. ;
   laScrMode[4] .AND. EMPTY(laData[2])
  _CUROBJ = OBJNUM(pbCls)
  RETURN
ENDIF
*B120822,1 MHM 01/05/2003 seek to the correct scale[Start]
IF !EMPTY(m.Style) 
  =SEEK('S'+m.Scale,'SCALE')
  *B037410,1 MHM 01/15/2004 seek to the correct Style and StyDye[Start]
  =SEEK(m.Style,'STYLE')
  =SEEK(m.Style+m.cWareCode,'STYDYE')
  *B037410,1 MHM 01/15/2004 [End]
ENDIF
*B120822,1 MHM 01/05/2003 seek to the correct scale[End]
=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

*!*************************************************************
*! Name      : lfDAccPack
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Deactivate function for packs screen
*!*************************************************************
*! Calls     : lpTab,lpBackTab
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
*! Calls     : gfModalGen,lfValidPack,gpAdStyWar
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvPckApply()
*!*************************************************************
FUNCTION lfvPckApply
PARAMETERS llStores,lcStore

SELECT (lcPackHdr)
LOCATE FOR PackQty <> 0
IF !FOUND()
  *E300408,1 Message : 40035
  *E300408,1 No packs selected
  *E300408,1 Button : 00000
  *E300408,1 Ok
  =gfModalGen('INM40035B00000','ALERT')
  GO TOP
  RETURN
ENDIF
STORE .T. TO llReMerTax,llCUpdate
SCAN FOR PackQty <> 0
  *C102574,1 HBG 04/15/2002 IF Trigger to GMA get # of packs and pack price to calculate the ship amount[Begin]
  IF ASCAN(laEvntTrig , PADR('SETDVAR1',10)) <> 0
    =gfDoTriger('ARDINV',PADR('SETDVAR1',10))
  ENDIF
  *C102574,1 [End]

  IF !lfValidPack()
    LOOP
  ENDIF
  SELECT Spck_Lin
  *C102574,1 HBG 04/15/2002 IF Trigger to GMA Scan Exprision will be 
  *C102574,1                changed to add the new fields of the key[Begin]
  lcScnExpr = 'Type+Account+Pack_Id=&lcPackHdr..Type+&lcPackHdr..Account+&lcPackHdr..Pack_Id'
  IF ASCAN(laEvntTrig , PADR('SCANEXP',10)) <> 0
    =gfDoTriger('ARDINV',PADR('SCANEXP',10))
  ENDIF  
  *SCAN REST WHILE Type+Account+Pack_Id=&lcPackHdr..Type+&lcPackHdr..Account+&lcPackHdr..Pack_Id
  SCAN REST WHILE &lcScnExpr
  *C102574,1 [End]

    WAIT 'Adding '+ALLTRIM(Pack_ID)+'/'+ALLTRIM(Style) WINDOW NOWAIT
    IF laSetups[5,2]='Y' .AND. !SEEK(Style+&lcInvHDr..cWareCode+SPACE(10),'StyDye')
      *-- Add a new record to the StyDye file.
      DO gpAdStyWar WITH Style,SPACE(10),&lcInvHdr..cWareCode
    ENDIF
    *E300834,1 Get style price according to ordered quantity
    *lnPrice = IIF(!llMulCurr,Style.Price&lcPriceLvl,;
                  gfStyPrice(Style,lcPriceLvl,&lcInvHdr..cCurrCode))
    
    *E301142,1 Select alternative price level
    *lcPriceCatg = IIF(lcPriceLvl='Q',IIF(&lcPackHdr..PackQty*TotQty >= Style.nAtQtyC,'C',;
    *              IIF(&lcPackHdr..PackQty*TotQty>=Style.nAtQtyB,'B','A')),lcPriceLvl)
    *m.Gros_Price= IIF(!llMulCurr OR laData[22]=gcBaseCurr,Style.Price&lcPriceCatg,;
    *                  gfStyPrice(Style,lcPriceCatg,laData[22]))
    m.Gros_Price = lfGetprice(Spck_Lin.Style,lcPriceLvl,&lcPackHdr..PackQty*Spck_Lin.TotQty)
    *C102574,1 HBG 04/15/2002 IF Trigger to GMA GEt the price from Spck_Lin[Begin]
    IF ASCAN(laEvntTrig , PADR('GETPRC',10)) <> 0
      =gfDoTriger('ARDINV',PADR('GETPRC',10))
     ENDIF
    *C102574,1 [End]

    *E301142,1 (End)
    
    *C102052,1 ABD - Add new validation on the discount code up on 
    *C102052,1 ABD - add new option about whole sale or retail sale [begin]
    *-- get the cDiscCode From stydye in every case
    lcDiscCode = IIF(SEEK(Style+&lcInvHDr..cWareCode+SPACE(10),'StyDye'),StyDye.cDiscCode,'')
    IF !EMPTY(ALLTRIM(lcDiscCode))
      *-- Get the discount related field to know which 
      *-- type whole Sale Or Retail sale Or Both.
      DECLARE laDisType[1,2] , laStartDte[1,2] , laEndDate[1,2]
      STORE '' To lcDisType , ldStartDte ,ldEndDate
      *-- Array to get the Discount affect for DecCode.
      laDisType[1,1]  = 'CCOSTAFECT'
      laDisType[1,2]  = 'lcDisType'
      *-- Array to get the start date For DescCode.
      lastartDte[1,1] = 'START'
      lastartDte[1,2] = 'ldStartDte'
      *-- Array to get the end date For DescCode.
      laEndDate[1,1]  = 'DENDATE'
      laEndDate[1,2]  = 'ldEndDate'
      = gfRltFld(lcDiscCode , @laDisType,  'CDISCCODE')
      = gfRltFld(lcDiscCode , @laStartDte, 'CDISCCODE')
      = gfRltFld(lcDiscCode , @laEndDate,  'CDISCCODE')
      STORE 0 To m.Disc_Pcnt  , lnDisc_Pcnt
      IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(laData[41],ldStartDte,ldEndDate)
        *C102052,1 ABD - [End]
        lnDisc_Pcnt = m.Disc_Pcnt
        *E300834,1 Get Style discount percent and Net Price        
        *C102052,1 ABD - remark next line and get cDiscCode From stydye file. [Begin]
        *=IIF(EMPTY(Style.CDISCCODE),.T.,gfRltFld(Style.CDISCCODE,@laDisRltFld,'CDISCCODE'))
        =gfRltFld(lcDiscCode,@laDisRltFld,'CDISCCODE')
        *E300834,1 Calculate net price
        m.Disc_Pcnt = lnDisc_Pcnt
        m.Price     = m.Gros_Price*(100-m.Disc_Pcnt)/100
        *E300834,1 (End)
        *C102052,1 ABD - [End]
        m.Disc_Pcnt = lnDisc_Pcnt
        *C102052,1 ABD - end if for if statement. [Begin]
      ENDIF
    ENDIF
    m.Price     = m.Gros_Price*(100-m.Disc_Pcnt)/100          
    *C102052,1 ABD - [End]
    lnLines = lnLines + 1
    SELECT (lcInvLine)
    *-- Add new line for the invline temp file
    APPEND BLANK
    =RLOCK()
    REPLACE InvDate WITH laData[41],;
            Account WITH laData[2] ,;
            Store   WITH laData[5] ,;
            LineNo  WITH lnLines     ,;
            Style   WITH Spck_Lin.Style ,;
            Desc1   WITH Style.Desc1 ,;
            Season  WITH Style.Season ,;
            Scale   WITH Style.Scale ,;
            Gros_Price WITH m.Gros_Price ,;
            Price   WITH m.Price ,;
            Disc_Pcnt WITH m.Disc_Pcnt ,;
            cWareCode WITH laData[21] ,;
            Qty1    WITH Spck_Lin.Qty1*&lcPackHdr..PackQty,;
            Qty2    WITH Spck_Lin.Qty2*&lcPackHdr..PackQty,;
            Qty3    WITH Spck_Lin.Qty3*&lcPackHdr..PackQty,;
            Qty4    WITH Spck_Lin.Qty4*&lcPackHdr..PackQty,;
            Qty5    WITH Spck_Lin.Qty5*&lcPackHdr..PackQty,;
            Qty6    WITH Spck_Lin.Qty6*&lcPackHdr..PackQty,;
            Qty7    WITH Spck_Lin.Qty7*&lcPackHdr..PackQty,;
            Qty8    WITH Spck_Lin.Qty8*&lcPackHdr..PackQty,;
            TotQty  WITH Spck_Lin.TotQty*&lcPackHdr..PackQty ,;
            Comm1   WITH laData[18] ,;
            Comm2   WITH laData[20] ,;
            lTaxable WITH STYLE.lTaxable ,;
            Pack_Id WITH &lcPackHdr..Pack_Id
    *C102574,1 HBG 04/15/2002 IF Trigger to GMA update he new fields in Invline[Begin]
    IF ASCAN(laEvntTrig , PADR('UPDINV',10)) <> 0
      =gfDoTriger('ARDINV',PADR('UPDINV',10))
    ENDIF          
    *C102574,1 [End]
            
    IF INLIST(laSetups[7,2],'F','L') AND SEEK(Spck_Lin.Style+laData[21]+SPACE(10),'StyDye')
      *-- if the cost method is FIFO or LIFO then replasce only with the On Hand Quantitys 
      REPLACE Qty1 WITH MIN(Qty1,StyDye.Stk1) ,;
              Qty2 WITH MIN(Qty2,StyDye.Stk2) ,;
              Qty3 WITH MIN(Qty3,StyDye.Stk3) ,;
              Qty4 WITH MIN(Qty4,StyDye.Stk4) ,;
              Qty5 WITH MIN(Qty5,StyDye.Stk5) ,;
              Qty6 WITH MIN(Qty6,StyDye.Stk6) ,;
              Qty7 WITH MIN(Qty7,StyDye.Stk7) ,;
              Qty8 WITH MIN(Qty8,StyDye.Stk8) ,;
              TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
    ENDIF
    UNLOCK
    *-- add the quantity per carton for that style
    lnCartons  = lnCartons  + IIF(Style.Qty_Ctn>0,TotQty/Style.Qty_Ctn,0)
    *-- add the shipped amount 
    laData[31] = laData[31] + TotQty*Price
    *C102574,1 HBG 04/15/2002 IF Trigger to GMA calculate the ship amount by using # of packs and price of pack[Begin]
    IF ASCAN(laEvntTrig , PADR('UPDSHP',10)) <> 0
      =gfDoTriger('ARDINV',PADR('UPDSHP',10))
    ENDIF
    *C102574,1 [End]

    *-- add the shipped Quantity
    laData[42] = laData[42] + TotQty
    *-- add the total weight
    laData[26] = laData[26] + TotQty*Style.nStyWeight
    *-- add the tax due for taxable styles only
    lnTaxDue   = lnTaxDue + IIF(lTaxable,TotQty*Price,0)
    IF laSetups[9,2]='Y' .AND. llIsEngland .AND. Style.nTaxBreak <> 0
    *-- Sum the quantity for only the sizes that is Greater than the tax break weight 
      lnQuantity = 0
      FOR lnCount = Style.nTaxBreak TO 8
        lnQuantity = lnQuantity+EVAL('Qty'+STR(lnCount,1))
      ENDFOR
      lnMerchTax = lnMerchTax + lnQuantity * Price * nTaxRate/100
    ENDIF
  ENDSCAN
  *-- Approved amount = Shipped Amount
  laData[48] = laData[31]    

  *-- Total cartons
  laData[25] = CEILING(lnCartons)

  *B602905,1 Default No. of cartons must be 1 at least [Begin]
  laData[25] = IIF(laData[25]=0,1,laData[25])
  *B602905,1 Default No. of cartons must be 1 at least [End  ]

  *B603293,1 (Start)
  *laData[33] = -laData[31]*laData[32]/100
  laData[33] = -ROUND(laData[31]*laData[32]/100,2)
  *B603293,1 (End)
  
ENDSCAN
*C102574,1 HBG 04/15/2002 IF Not Trigger to GMA calculate the ship amount in the regular way[Begin]
IF ASCAN(laEvntTrig , PADR('GETAMNT',10)) <> 0
  =gfDoTriger('ARDINV',PADR('GETAMNT',10)) 
ENDIF      
*C102574,1 [End]

SELECT (lcInvHdr)  
=RLOCK()
REPLACE CARTONS  WITH laData[25] ,;
        WEIGHT   WITH laData[26] ,;
        SHIPAMT  WITH laData[31] ,;
        Discount WITH laData[33] ,;
        SHIP     WITH laData[42] ,;
        APPRAMT  WITH laData[48] ,;
        nTaxDue  WITH lnTaxDue   ,;
        lCompUps WITH .T.
UNLOCK
SELECT (lcInvLine)
WAIT CLEAR
CLEAR READ

*!*************************************************************
*! Name      : lfValidPack
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Validate packs before add to the order lines
*!*************************************************************
*! Calls     : gfModalGen,gfStyPrice
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
*C102574,1 HBG 04/15/2002 IF Trigger to GMA Scan exprision will 
*C102574,1                be changed to add the new fields of the key[Begin]
lcScnExpr = 'Type+Account+Pack_Id=&lcPackHdr..Type+&lcPackHdr..Account+&lcPackHdr..Pack_Id'
IF ASCAN(laEvntTrig , PADR('SCANEXP',10)) <> 0
 =gfDoTriger('ARDINV',PADR('SCANEXP',10))
ENDIF  
*SCAN REST WHILE Type+Account+Pack_Id=&lcPackHdr..Type+&lcPackHdr..Account+&lcPackHdr..Pack_Id
SCAN REST WHILE &lcScnExpr
*C102574,1 [End]

  IF !SEEK('S'+Style.Scale,'Scale')
    *E300408,1 Message : 40029
    *E300408,1 Pack xxx contains one or more xxx. xxx
    *E300408,1 Button : 00000
    *E300408,1 Ok
    =gfModalGen('INM40029B00000','ALERT',&lcPackHdr..Pack_Id+'|'+'style with non existing scale'+'|'+'Pack will be ignored.')
    llValidPack = .F.
    EXIT
  ENDIF  
  IF Style.Status = 'X'
    *E300408,1 Message : 40029
    *E300408,1 Pack xxx contains one or more xxx. xxx
    *E300408,1 Button : 00000
    *E300408,1 Ok
    =gfModalGen('INM40029B00000','ALERT',&lcPackHdr..Pack_Id+'|'+'canceled style'+'|'+'Pack will be ignored.')
    llValidPack = .F.
    EXIT
  ENDIF
  IF !llOnHoldAll .AND. Style.Status = 'H'
    *E300408,1 Message : 40030
    *E300408,1 Pack xxx contains one or more on-hold style.
    *E300408,1 Button : 40003
    *E300408,1 Proceed Cancel
    IF gfModalGen('QRM40030B40003','ALERT',&lcPackHdr..Pack_Id)=1
      llOnHoldAll = .T.
    ELSE
      llValidPack= .F.
      EXIT
    ENDIF
  ENDIF
  IF laSetups[5,2]='Y' .AND. !llAssignAll .AND. ;
     !SEEK(Style+&lcInvHdr..cWareCode+SPACE(10),'StyDye')
    *E300408,1 Message : 40031
    *E300408,1 Pack xxx contains one or more style that is not assign
    *E300408,1 to warehouse xxx. Assign all styles to this warehouse?
    *E300408,1 Button : 40000
    *E300408,1 Yes No
    IF gfModalGen('QRM40031B40000','ALERT',&lcPackHdr..Pack_Id+'|'+&lcInvHdr..cWareCode)=1
      llAssignAll = .T.
    ELSE
      llValidPack = .F.
      EXIT
    ENDIF
  ENDIF 
  *E300834,1 Get style price according to ordered quantity
  *lnPrice = IIF(!llMulCurr,Style.Price&lcPriceLvl,;
                gfStyPrice(Style,lcPriceLvl,&lcInvHdr..cCurrCode))
  
  *E301142,1 Select alternative price level if zero price
  *lcPriceCatg = IIF(lcPriceLvl='Q',IIF(&lcPackHdr..PackQty*TotQty >= Style.nAtQtyC,'C',;
  *              IIF(&lcPackHdr..PackQty*TotQty>=Style.nAtQtyB,'B','A')),lcPriceLvl)
  *lnPrice     = IIF(!llMulCurr OR laData[22]=gcBaseCurr,Style.Price&lcPriceCatg,;
  *                   gfStyPrice(Style,lcPriceCatg,laData[22]))
  **E300834,1 (End)
  *IF lnPrice < 0
  *  llValidPack = .F.
  *  EXIT
  *ENDIF
  *E301142,1 (End)
ENDSCAN
=SEEK(&lcPackHdr..Type+&lcPackHdr..Account+&lcPackHdr..Pack_Id)
*C102574,1 HBG 04/15/2002 IF Trigger to GMA Seek exprision will 
*C102574,1                be changed to add the new fields of the key[Begin]
IF ASCAN(laEvntTrig , PADR('SEEK',10)) <> 0
  =gfDoTriger('ARDINV',PADR('SEEK',10))
ENDIF  
*C102574,1 [End]

RETURN(llValidPack)

*!*************************************************************
*! Name      : FUNCTION lfvStyle
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate entered style
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

IF MDOWN() .AND. !llBrowse
  RETURN
ENDIF
 
IF llBrowse .OR. ;
   ( (!llAddLine .OR. m.Style <> lcEmptySty) .AND. !SEEK(m.Style,'Style'))
  *B804106,1 NAD  04/30/2001 (Start) Style browse is empty when Clicking the browse button 
  *m.Style = IIF(llBrowse,'?',m.Style)
   m.Style = IIF(llBrowse,'',m.Style)
  *B804106,1 NAD  04/30/2001 (END)
  m.Style = PADR(gfStyBrw("I" , m.Style , "" , .F.),19)
  IF EMPTY(m.Style)
    STORE IIF(llAddLine,lcEmptySty,&lcInvLine..Style) TO m.Style
    _CUROBJ = OBJNUM(m.Style)
  ENDIF
  llBrowse = .F.
ENDIF

IF !llAddLine .AND. m.Style=&lcInvLine..Style
  RETURN
ENDIF
IF m.Style=lcEmptySty
  STORE m.Style TO lcLastStyle
  SCATTER MEMVAR MEMO
  STORE .F. TO llAddLine,llNewLine
  IF EMPTY(m.Style)
    SHOW GETS WINDOW (lcWinCh32) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh34) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh35) DISABLE ONLY
    SHOW GET pbNew     ENABLE
    SHOW GET ibInv200E ENABLE
    SHOW GET pbPacks   ENABLE
    _CUROBJ = OBJNUM(pbNew)
  ELSE
    SHOW GETS WINDOW (lcWinCh32) ENABLE ONLY
    SHOW GETS WINDOW (lcWinCh34) ENABLE ONLY
    SHOW GETS WINDOW (lcWinCh35) ENABLE ONLY
    _CUROBJ = OBJNUM(ibInv200E)
  ENDIF
  RETURN
ENDIF
SELECT (lcInvLine) 
SET RELATION OFF INTO STYLE
IF SEEK(m.Style,'Style')
  *E300408,1 Message : 40006
  *E300408,1 Style scale not found in the scale file. Cannot accept.
  *E300408,1 Button : 00000 
  *E300408,1 Ok

  *E300408,1 Message : 40007
  *E300408,1 This is a canceled style. Not allowed to invoice.
  *E300408,1 Button : 00000 
  *E300408,1 Ok

  *E300408,1 Message : 40008
  *E300408,1 This style/color is on hold. 
  *E300408,1 Button : 40001
  *E300408,1 Accept Reenter

  *E300408,1 Message : 40009
  *E300408,1 Styles restricted to XXX!
  *E300408,1 Button : 00000
  *E300408,1 Ok

  *E300408,1 Message : 40010
  *E300408,1 Style XXX date is 
  *E300408,1 Button : 40001
  *E300408,1 Accept Reenter

  *E300408,1 Message : 40085
  *E300408,1 No dyelots found for style xxxx in warehouse xxxxx
  *E300408,1 Button : 00000
  *E300408,1 Ok

  *C200195,1 Add the customer trigger to ignore checking for season. [Begin]
  *IF (!SEEK('S'+Style.Scale,'Scale') .AND. ;
  *    gfModalGen('TRM40006B00000','ALERT')=1) ;
  * .OR. (Style.Status='X' .AND. ;
  *      gfModalGen('TRM40007B00000','ALERT')=1) ;
  * .OR. (laSetups[8,2]='Y' .AND. Style.cDye_Flg='Y' .AND. !lfStyDye() .AND.;
  *      gfModalGen('TRM40085B00000','ALERT',lcStyHdr+': '+m.Style+IIF(laSetups[5,2]='Y',' in warehouse '+laData[21],''))=1) ;
  * .OR. (Style.cDivision <> laData[13] .AND. ;
  *      gfModalGen('TRM40009B00000','ALERT','division '+ALLTRIM(laDivision[lnDivision,1]))=1) ;        
  * .OR. (ALLTRIM(laData[12])<>'*' AND TRIM(Style.Season)<>'Y' AND Style.Season<>laData[12] .AND. ;
  *      gfModalGen('TRM40009B00000','ALERT','season '+ALLTRIM(laSeasons[lnSeason,1]))=1) ;
  * .OR. (Style.Status='H' .AND. ;
  *      gfModalGen('QRM40008B40001','ALERT')=2) ;
  * .OR. (!EMPTY(Style.Start) .AND. Style.Start > laData[41] .AND. ;
  *      gfModalGen('QRM40010B40001','ALERT','start|'+DTOC(Style.Start))=2) ;
  * .OR. (!EMPTY(Style.SoldOut) .AND. Style.SoldOut < laData[41] .AND. ;
  *      gfModalGen('QRM40010B40001','ALERT','sold out|'+DTOC(Style.SoldOut))=2)
  
  *B126828,1 EIH 03/15/2005 Fix bug when select styles from same division on invoice transaction [Begin].
  *IF (!SEEK('S'+Style.Scale,'Scale') .AND. ;
      gfModalGen('TRM40006B00000','ALERT')=1) ;
   .OR. (Style.Status='X' .AND. ;
        gfModalGen('TRM40007B00000','ALERT')=1) ;
   .OR. (laSetups[8,2]='Y' .AND. Style.cDye_Flg='Y' .AND. !lfStyDye() .AND.;
        gfModalGen('TRM40085B00000','ALERT',lcStyHdr+': '+m.Style+IIF(laSetups[5,2]='Y',' in warehouse '+laData[21],''))=1) ;
   .OR. (Style.cDivision <> laData[13] .AND. ;
        gfModalGen('TRM40009B00000','ALERT','division '+ALLTRIM(laDivision[lnDivision,1]))=1) ;        
   .OR. IIF(ASCAN(laEvntTrig , PADR('STYLEVALID',10)) <> 0 AND laSetups[22,2] = .F.,.F.,(ALLTRIM(laData[12])<>'*' AND TRIM(Style.Season)<>'Y' AND Style.Season<>laData[12] .AND. ;
        gfModalGen('TRM40009B00000','ALERT','season '+ALLTRIM(laSeasons[lnSeason,1]))=1)) ;  
   .OR. (Style.Status='H' .AND. ;
        gfModalGen('QRM40008B40001','ALERT')=2) ;
   .OR. (!EMPTY(Style.Start) .AND. Style.Start > laData[41] .AND. ;
        gfModalGen('QRM40010B40001','ALERT','start|'+DTOC(Style.Start))=2) ;
   .OR. (!EMPTY(Style.SoldOut) .AND. Style.SoldOut < laData[41] .AND. ;
        gfModalGen('QRM40010B40001','ALERT','sold out|'+DTOC(Style.SoldOut))=2)
  
  IF (!SEEK('S'+Style.Scale,'Scale') .AND. ;
      gfModalGen('TRM40006B00000','ALERT')=1) ;
   .OR. (Style.Status='X' .AND. ;
        gfModalGen('TRM40007B00000','ALERT')=1) ;
   .OR. (laSetups[8,2]='Y' .AND. Style.cDye_Flg='Y' .AND. !lfStyDye() .AND.;
        gfModalGen('TRM40085B00000','ALERT',lcStyHdr+': '+m.Style+IIF(laSetups[5,2]='Y',' in warehouse '+laData[21],''))=1) ;
   .OR. (!(ALLTRIM(Style.cDivision) == ALLTRIM(laData[13])) .AND. ;
        gfModalGen('TRM40009B00000','ALERT','division '+ALLTRIM(laDivision[lnDivision,1]))=1) ;        
   .OR. IIF(ASCAN(laEvntTrig , PADR('STYLEVALID',10)) <> 0 AND laSetups[22,2] = .F.,.F.,(ALLTRIM(laData[12])<>'*' AND TRIM(Style.Season)<>'Y' AND Style.Season<>laData[12] .AND. ;
        gfModalGen('TRM40009B00000','ALERT','season '+ALLTRIM(laSeasons[lnSeason,1]))=1)) ;  
   .OR. (Style.Status='H' .AND. ;
        gfModalGen('QRM40008B40001','ALERT')=2) ;
   .OR. (!EMPTY(Style.Start) .AND. Style.Start > laData[41] .AND. ;
        gfModalGen('QRM40010B40001','ALERT','start|'+DTOC(Style.Start))=2) ;
   .OR. (!EMPTY(Style.SoldOut) .AND. Style.SoldOut < laData[41] .AND. ;
        gfModalGen('QRM40010B40001','ALERT','sold out|'+DTOC(Style.SoldOut))=2)
  *B126828,1 EIH 03/15/2005  [End].      
  
  *C200195,1 Add the customer trigger to ignore checking for season. [End]

    STORE IIF(llAddLine,lcEmptySty,&lcInvLine..Style) TO m.Style

    SET RELATION TO STYLE INTO STYLE ADDITIVE
    _CUROBJ = OBJNUM(m.Style)
    RETURN
  ENDIF
  *E300834,1 Get style price according to ordered quantity
  *m.Gros_Price = IIF(!llMulCurr .OR. laData[22]=gcBaseCurr,Style.Price&lcPriceLvl,;
                gfStyPrice(m.Style,lcPriceLvl,laData[22]))
  
  *E301142,1 Select alternative price level if zero price
  *lcPriceCatg  = IIF(lcPriceLvl='Q',IIF(m.TotQty >= Style.nAtQtyC,'C',;
  *               IIF(m.TotQty>=Style.nAtQtyB,'B','A')),lcPriceLvl)
  *m.Gros_Price = IIF(!llMulCurr OR laData[22]=gcBaseCurr,Style.Price&lcPriceCatg,;
  *                   gfStyPrice(m.Style,lcPriceCatg,laData[22]))
  m.Gros_Price = lfGetprice(m.Style,lcPriceLvl,m.TotQty)
  *E301142,1 (End)
  *E300834,1 (End)
  
  SELECT (lcInvLine)
  IF m.Gros_Price < 0
    STORE IIF(llAddLine,lcEmptySty,&lcInvLine..Style) TO m.Style
    SET RELATION TO STYLE INTO STYLE ADDITIVE
    _CUROBJ = OBJNUM(m.Style)
    RETURN
  ENDIF
  IF laSetups[5,2]='Y' .AND. !SEEK(m.Style+laData[21]+SPACE(10),'StyDye')
    *E300408,1 Message : 40012
    *E300408,1 Style xxx is not assigned to warehouse xxx
    *E300408,1 Button : 40002
    *E300408,1 Add Reenter
    IF gfModalGen('QRM40012B40002','ALERT',TRIM(m.Style)+'|'+TRIM(laData[21]))=1
       *-- Add a new record to the StyDye file.
       DO gpAdStyWar WITH m.Style,SPACE(10),laData[21]
    ELSE
      STORE IIF(llAddLine,lcEmptySty,&lcInvLine..Style) TO m.Style
      SET RELATION TO STYLE INTO STYLE ADDITIVE
      _CUROBJ = OBJNUM(m.Style)
      RETURN
    ENDIF
  ENDIF
  lnTaxRate = 0
  SET ORDER TO TAG InvLines
  IF SEEK(laData[2]+laData[3]+laData[5]+laData[30]+m.Style)
    *E300408,1 Message : 40013
    *E300408,1 Warning! This style had been entered on this invoice
    *E300408,1 Button : 00000
    *E300408,1 Ok
    =gfModalGen('INM40013B00000','ALERT')
  ENDIF
  IF llAddLine
    lnLines   = lnLines + 1
    m.LineNo  = lnLines
    m.Account = laData[2]
    m.Store   = laData[5]
    m.InvDate = laData[41]
    m.cWareCode = laData[21]
    INSERT INTO (lcInvLine) FROM MEMVAR
  ELSE
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      IF lnCount > Scale.cnt
        m.Qty&lcCount = 0
      ENDIF
    ENDFOR
    m.TotQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
    *-- if the style quantity not divisible by Prepack
    IF !EMPTY(m.Prepak) AND SEEK('P'+m.Scale+m.Prepak,'Scale') AND ;
       MOD(m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,Scale.PPTot) <> 0 
      m.Prepak=''
      m.PpQty = 0 
    ENDIF
  ENDIF
  SELECT STYLE
  SKIP
  lcLastStyle = Style 
  =SEEK(m.Style) 
  SELECT (lcInvLine)
  SET ORDER TO TAG InvLine
  =SEEK(laData[2]+laData[3]+laData[5]+laData[30]+STR(m.LineNo,6))

  *C102052,1 ABD - Add new validation on the discount code up on 
  *C102052,1 ABD - add new option about whole sale or retail sale [begin]
  *-- get the cDiscCode From stydye in every case
  lcDiscCode = IIF(SEEK(Style+&lcInvHDr..cWareCode+SPACE(10),'StyDye'),StyDye.cDiscCode,'')
  IF !EMPTY(ALLTRIM(lcDiscCode))
    *-- Get the discount related field to now which 
    *-- type whole Sale Or Retail sale Or Both.
    DECLARE laDisType[1,2] , lastartDte[1,2] , laEndDate[1,2]
    STORE '' To lcDisType , ldStartDte ,ldEndDate
    *-- Array to get the Discount affect for DecCode.
    laDisType[1,1]  = 'CCOSTAFECT'
    laDisType[1,2]  = 'lcDisType'
    *-- Array to get the start date For DescCode.
    lastartDte[1,1] = 'START'
    lastartDte[1,2] = 'ldStartDte'
    *-- Array to get the end date For DescCode.
    laEndDate[1,1]  = 'DENDATE'
    laEndDate[1,2]  = 'ldEndDate'
    = gfRltFld(lcDiscCode, @laDisType, 'CDISCCODE')
    = gfRltFld(lcDiscCode, @laStartDte,'CDISCCODE')
    = gfRltFld(lcDiscCode, @laEndDate, 'CDISCCODE')
    STORE 0 To m.Disc_Pcnt , lnDisc_Pcnt
    IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(laData[41],ldStartDte,ldEndDate)
      *C102052,1 ABD - [End]
      *E300834,1 Get Style discount percent and Net Price        
      lnDisc_Pcnt = m.Disc_Pcnt
      *E300834,1 Get Style discount percent and Net Price        
      *C102052,1 ABD - remark next line and get cDiscCode From stydye file. [Begin]
      *=IIF(EMPTY(Style.CDISCCODE),.T.,gfRltFld(Style.CDISCCODE,@laDisRltFld,'CDISCCODE'))
      =gfRltFld(lcDiscCode,@laDisRltFld,'CDISCCODE')
      *E300834,1 Calculate net price
      m.Disc_Pcnt = lnDisc_Pcnt
      m.Price     = m.Gros_Price*(100-m.Disc_Pcnt)/100
      *E300834,1 (End)
      *C102052,1 ABD - [End]
      m.Disc_Pcnt = lnDisc_Pcnt
      *C102052,1 ABD - end if for if statement. [Begin]
    ENDIF
  ENDIF  
  m.Price     = m.Gros_Price*(100-m.Disc_Pcnt)/100  
  *C102052,1 ABD - [End]
  *B604116,1 AMH Don't run lfChkTotQty() for TotQty = 0 for not deleting [Start]
  *B604116,1     the current record in lcInvLine.
  *=IIF(llAddLine,.T.,lfvNPrice() AND lfChkTotQty())
  =IIF(llAddLine .OR. TotQty = 0,.T.,lfvNPrice() AND lfChkTotQty())
  *B604116,1 AMH [End]
  IF llIsEngland .AND. !Customer.lVatExem  .AND. Style.nTaxBreak <> 0
  *-- get the england style tax related field if it is not smaller the the tax Break
    =gfRltFld(Style.cTaxCode,@laEngStyTax,'CTAXCODE')  
  ENDIF
  llAddLine  = .F.
  llCUpdate  = .T.
  m.Desc1    = Style.Desc1
  m.Scale    = Style.Scale
  m.PrePak   = Style.PrePak
  m.Comm1    = IIF(Style.commission,laData[18],0)
  m.Comm2    = IIF(Style.commission,laData[20],0)
  m.lTaxAble = STYLE.lTaxAble
  m.Season   = Style.Season
  m.nTaxRate = lnTaxRate
  =RLOCK()
  GATHER MEMVAR MEMO
  UNLOCK
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  SHOW WINDOW (lcInvBrow) REFRESH SAME
  SHOW GETS WINDOW (lcWinCh32) ENABLE ONLY
  SHOW GETS WINDOW (lcWinCh34) ENABLE ONLY
  SHOW GETS WINDOW (lcWinCh35) ENABLE ONLY
  IF laSetups[2,2] <> 'Y' OR EMPTY(laData[17])
    SHOW GET m.Comm1 DISABLE
  ENDIF
  IF laSetups[2,2] <> 'Y' OR EMPTY(laData[19])
    SHOW GET m.Comm2 DISABLE
  ENDIF
  *C102574,1 HBG 04/15/2002 If Trigger to GMA Disaple Pack Field[Begin]
  IF ASCAN(laEvntTrig , PADR('DISBLFlD',10)) <> 0
    =gfDoTriger('ARDINV',PADR('DISBLFlD',10))
  ENDIF  
  *C102574,1 [End]
  
  =lfRefresh(lcWinCh32)
  =lfRefresh(lcWinCh34)
  *B602760,1 NAD (Start) Disable Style Prepack code and quantity 
  *B602760,1             if style scale does not have prepacks
  =lfPpStat()   
  *B602760,1 NAD (End)

ENDIF  
  
*-- end of lfvStyle.

*!*************************************************************
*! Name      : FUNCTION lfStyDye
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Check if invoice style has dyelots in the selected warehouse
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfStyDye()
*!*************************************************************
FUNCTION lfStyDye
PRIVATE laDyelots

*B603411,1 NAD 01/30/2000   [Start] Fix the bug of long delay after entering style/color

*SELECT * FROM STYDYE WHERE style+cwarecode+dyelot+STR(RECNO(),7) = ;
*m.Style+laData[21] .AND. !EMPTY(Dyelot) INTO ARRAY laDyelots
*RETURN(_TALLY > 0)

IF SEEK (m.style+laData[21]+SPACE(10),'StyDye')
  SKIP IN StyDye
  RETURN (STYDYE.Style+STYDYE.cwarecode= m.Style+laData[21] AND !EMPTY(STYDYE.Dyelot) )
ELSE 
  RETURN .F.
ENDIF
*B603411,1 NAD 01/30/2000   [End]
*!*************************************************************
*! Name      : FUNCTION lfvDyelot
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate entered dyelot
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvDyelot()
*!*************************************************************
FUNCTION lfvDyelot
PRIVATE lcDyelot

lcDyelot = m.Dyelot
*B603363,1 (Start) Select the stydye file before calling SDYEBROW function to prevent
*B603363,1         Bug subscript out of bounds  
*IF (llBrowse .OR. EMPTY(lcDyelot) .OR. !SEEK(m.Style+laData[21]+lcDyelot,'StyDye')) .AND. ;
*  !SDYEBROW(m.Style,@lcDyelot,.T.,IIF(laSetups[5,2]='Y',laData[21],''))
*  lcDyelot = Dyelot
*ENDIF
IF (llBrowse .OR. EMPTY(lcDyelot) .OR. !SEEK(m.Style+laData[21]+lcDyelot,'StyDye'))
  lcAlias=SELECT()
  SELECT STYDYE
  IF !SDYEBROW(m.Style,@lcDyelot,.T.,IIF(laSetups[5,2]='Y',laData[21],''))
    lcDyelot = Dyelot
  ENDIF  
  SELECT (lcAlias)
ENDIF
*B603363,1 (End)
m.Dyelot = lcDyelot
llBrowse = .F.
IF Dyelot <> m.Dyelot
  llCUpdate = .T.
  =RLOCK()
  REPLACE Dyelot WITH m.Dyelot
  UNLOCK
  SHOW WINDOW (lcInvBrow) REFRESH SAME
ENDIF
IF EMPTY(m.Dyelot)
  _CUROBJ = _CUROBJ
ENDIF
RETURN

*!*************************************************************
*! Name      : FUNCTION lfvStyGrp
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style/color group
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

IF Group <> m.Group
  llCUpdate = .T.
  =RLOCK()
  REPLACE Group WITH m.Group
  UNLOCK
  SHOW WINDOW (lcInvBrow) REFRESH SAME
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
PRIVATE lcBrFields,lcFile_Ttl

IF llBrowse .OR. (!EMPTY(m.Prepak) .AND. !SEEK('P'+m.Scale+m.Prepak,'Scale'))
  =gfPrePBrow(Scale,@m.Prepak)
ENDIF
llBrowse = .F.
=SEEK('S'+m.Scale,'Scale')
IF Prepak <> m.Prepak
  llCUpdate = .T.
  =RLOCK()
  REPLACE Prepak WITH m.Prepak
  UNLOCK
  SHOW GETS WINDOW (lcWinCh32) ONLY
  SHOW GETS WINDOW (lcWinCh34) ONLY
  SHOW GETS WINDOW (lcWinCh35) ONLY
ENDIF
IF EMPTY(Prepak)
  =RLOCK()
  REPLACE PPQty WITH 0
  UNLOCK
  m.PPQTY = 0
  *B602760,1 NAD (Start) Control object enable and disable in the screen
  *SHOW GET m.PPQty DISABLE
ENDIF  
*ELSE
*  SHOW GET m.PPQty ENABLE
*ENDIF
*B602760,1 NAD (End)
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

IF m.PPQty = PPQty
  RETURN
ENDIF
IF m.PPQty <> 0
  =SEEK('P'+m.Scale+m.Prepak,'Scale')
  m.Qty1   = m.PPQty*Scale.PP1
  m.Qty2   = m.PPQty*Scale.PP2
  m.Qty3   = m.PPQty*Scale.PP3
  m.Qty4   = m.PPQty*Scale.PP4
  m.Qty5   = m.PPQty*Scale.PP5
  m.Qty6   = m.PPQty*Scale.PP6
  m.Qty7   = m.PPQty*Scale.PP7
  m.Qty8   = m.PPQty*Scale.PP8
  m.TotQty = m.PPQty*Scale.PPTot
  SHOW GET m.TotQty DISABLE
  =SEEK('S'+m.Scale,'Scale')
  IF lfChkTotQty()
    SHOW GETS WINDOW (lcWinCh32) ONLY
    SHOW GETS WINDOW (lcWinCh34) ONLY
    SHOW GETS WINDOW (lcWinCh35) ONLY
    =lfRefresh(lcWinCh32)
    =lfRefresh(lcWinCh34)
  ELSE
    SCATTER MEMVAR FIELDS PPQty,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty
  ENDIF
ENDIF
IF m.PPQty = 0
  SHOW GET m.TotQty ENABLE
ENDIF

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

IF EMPTY(m.Prepak) AND Scale.Cnt > 1
  RETURN
ENDIF
IF !EMPTY(m.Prepak) .AND. SEEK('P'+m.Scale+m.Prepak,'Scale')
  *E300408,1 Message : 40014
  *E300408,1 Total quantity is not evenly divisible by the prepak quantity xxx
  *E300408,1 Button : 00000 
  *E300408,1 Ok
  IF MOD(m.TotQty,Scale.PPTot) <> 0 .AND. ;
    gfModalGen('INM40014B00000','ALERT',STR(Scale.PPTot,4))=1
    _CUROBJ = _CUROBJ
    =SEEK('S'+m.Scale,'Scale')
    RETURN
  ENDIF
  m.Qty1 = IIF(Scale.PPTot>0,m.TotQty*Scale.PP1/Scale.PPTot,0)
  m.Qty2 = IIF(Scale.PPTot>0,m.TotQty*Scale.PP2/Scale.PPTot,0)
  m.Qty3 = IIF(Scale.PPTot>0,m.TotQty*Scale.PP3/Scale.PPTot,0)
  m.Qty4 = IIF(Scale.PPTot>0,m.TotQty*Scale.PP4/Scale.PPTot,0)
  m.Qty5 = IIF(Scale.PPTot>0,m.TotQty*Scale.PP5/Scale.PPTot,0)
  m.Qty6 = IIF(Scale.PPTot>0,m.TotQty*Scale.PP6/Scale.PPTot,0)
  m.Qty7 = IIF(Scale.PPTot>0,m.TotQty*Scale.PP7/Scale.PPTot,0)
  m.Qty8 = IIF(Scale.PPTot>0,m.TotQty*Scale.PP8/Scale.PPTot,0)
ENDIF
=SEEK('S'+m.Scale,'Scale')
IF Scale.Cnt = 1
  m.Qty1 = m.TotQty
ENDIF
IF m.Qty1=Qty1 AND m.Qty2=Qty2 AND m.Qty3=Qty3 AND m.Qty4=Qty4 AND ;
   m.Qty5=Qty5 AND m.Qty6=Qty6 AND m.Qty7=Qty7 AND m.Qty8=Qty8
  RETURN
ENDIF
*-- Check total pieces againest total quantity entered
IF lfChkTotQty()
  SHOW GETS WINDOW (lcWinCh32) ONLY
  SHOW GETS WINDOW (lcWinCh34) ONLY
  =lfRefresh(lcWinCh34)
ELSE
  SCATTER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty
ENDIF
*!*************************************************************
*! Name      : FUNCTION lfvSizeQty
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style size quantity
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvSizeQty()
*!*************************************************************
FUNCTION lfvSizeQty
PARAMETERS lcQtyNo


*B603449,1 ABD If statement to cheak if user inter Negative values. [ Begin ]
IF m.Qty&lcQtyNo < 0 
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Buttfon  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.Qty&lcQtyNo = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
*B603449,1 ABD [ End ]

=lfRefresh(lcWinCh34)
*-- End OF lfvSizeQty.
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

*E300408,1 Message : 40015
*E300408,1 Quantity out of balance! xxx pieces ok
*E300408,1 Button : 40000 
*E300408,1 Yes NO
IF m.TotQty <> m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8 AND ;
  gfModalGen('QRM40015B40000','ALERT',STR(m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,5))=2
  RETURN(.F.)
ENDIF

*IF !EMPTY(m.Prepak)
*  llPrepak= SEEK('P'+m.Scale+m.Prepak,'Scale') AND ;
*   MOD(m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,Scale.PPTot) <> 0
*   =SEEK('S'+m.Scale,'Scale')
*  IF llPrepak 
*    *E300408,1 Message : 40014
*    *E300408,1 Total quantity is not evenly divisible by the prepak quantity xxx
*    *E300408,1 Button : 00000 
*    *E300408,1 Ok
*    =gfModalGen('TRM40014B00000','ALERT',STR(Scale.PPTot,4))
*    RETURN(.F.)
*  ENDIF 
*ENDIF
IF Qty1 <> m.Qty1 OR Qty2 <> m.Qty2 OR Qty3 <> m.Qty3 OR Qty4 <> m.Qty4 OR ;
   Qty5 <> m.Qty5 OR Qty6 <> m.Qty6 OR Qty7 <> m.Qty7 OR Qty8 <> m.Qty8
  
  *B128959,1 NNA 09/04/2005 (Start) Check also if Inv.Style Checked or not to Generate the following Massage
  *IF INLIST(laSetups[7,2],'F','L')
  IF INLIST(laSetups[7,2],'F','L') AND STYLE.LINVSTY
  *B128959,1 NNA (End)
  *-- if the cost method is FIFO or LIFO then replace only with the On Hand Quantitys 
    FOR lnCount = 1 TO Scale.Cnt
      IF EVAL('m.Qty'+STR(lnCount,1)) > EVAL('StyDye.Stk'+STR(lnCount,1))
        *E300408,1 Message : 40137
        *E300408,1 Shipped Quantity For Size XXXXX Cannot Exceed On Hand
        *E300408,1 Quantity (99999).
        *E300408,1 Button : 00000 
        *E300408,1 OK
        =gfModalGen('TRM40137B00000','ALERT',ALLTRIM(EVAL('Scale.Sz'+STR(lnCount,1)))+'|('+ALLTRIM(STR(EVAL('StyDye.Stk'+STR(lnCount,1)),5))+')')
        STORE MAX(EVAL('StyDye.Stk'+STR(lnCount,1)),0) TO ('m.Qty'+STR(lnCount,1))
        RETURN(.F.)
      ENDIF
    ENDFOR
  ENDIF
  m.TotQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
  *E300834,1 Get style price according to ordered quantity
  IF lcPriceLvl='Q'
    *E301142,1 Select alternative price level if zero price
    *lcPriceCatg  = IIF(m.TotQty >= Style.nAtQtyC,'C',IIF(m.TotQty >= Style.nAtQtyB,'B','A'))
    *m.Gros_Price = IIF(!llMulCurr OR laData[22]=gcBaseCurr,Style.Price&lcPriceCatg,;
    *                   gfStyPrice(m.Style,lcPriceCatg,laData[22]))
    *--Ask the user for the Prices
    m.Gros_Price = lfGetprice(m.Style,lcPriceLvl,m.TotQty)
    *E301142,1 (End)
    m.Price = m.Gros_Price*(100-m.Disc_Pcnt)/100
  ENDIF
  *E300834,1 (End)
  SELECT (lcInvLine)
  lnCartons  = lnCartons  - IIF(Style.Qty_Ctn>0,(TotQty-m.TotQty)/Style.Qty_Ctn,0)
  laData[25] = CEILING(lnCartons)

  *B602905,1 Default No. of cartons must be 1 at least [Begin]
  laData[25] = IIF(laData[25]=0,1,laData[25])
  *B602905,1 Default No. of cartons must be 1 at least [End  ]
  *-- Total Wieght 
  laData[26] = laData[26] - (TotQty-m.TotQty)*Style.nStyWeight
  *-- Shipped Amount
  laData[31] = laData[31] - TotQty*Price+m.TotQty*m.Price
  *-- Shippd Quantity
  laData[42] = laData[42] - TotQty+m.TotQty
  *-- Approval Amount
  laData[48] = laData[31]
  
  *B603293,1 (Start)
  *laData[33] = -laData[31]*laData[32]/100
  laData[33] = -ROUND(laData[31]*laData[32]/100,2)
  *B603293,1 (End)
  
  lnTaxDue   = lnTaxDue + IIF(lTaxable,m.TotQty*m.Price-TotQty*Price,0)
  SELECT (lcInvHdr)  
  =RLOCK()
  REPLACE CARTONS  WITH laData[25] ,;
          WEIGHT   WITH laData[26] ,;
          SHIPAMT  WITH laData[31] ,;
          SHIP     WITH laData[42] ,;
          Discount WITH laData[33] ,;
          APPRAMT  WITH laData[48] ,;
          nTaxDue  WITH lnTaxDue   ,;
          lCompUps WITH .T.
  UNLOCK
  SELECT (lcInvLine)
  IF laSetups[9,2]='Y' .AND. llIsEngland
    FOR lnCount = 1 TO Scale.Cnt
      IF BETWEEN(lnCount,Style.nTaxBreak,8)
        lcCount = STR(lnCount,1)
        lnMerchTax = lnMerchTax + (m.Qty&lcCount*m.Price-Qty&lcCount*Price)*nTaxRate/100
      ENDIF
    ENDFOR  
  ENDIF
  =RLOCK()
  GATHER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,;
                       Gros_Price,Price,PPQty

  *C102676,1 Custom process for A.S.T. sportwear. [Begin]
  IF ASCAN(laEvntTrig,PADR('UPDAMT3',10))<>0
    =gfDoTriger('ARDINV',PADR('UPDAMT3',10))
  ENDIF  
  *C102676,1 Custom process for A.S.T. sportwear. [End]

  UNLOCK
  STORE .T. TO llReMerTax,llCUpdate
ENDIF
IF TotQty = 0
  *E300408,1 Message : 40017
  *E300408,1 Total Pieces 0! This line will be deleted
  *E300408,1 Button : 00000 
  *E300408,1 OK
  =gfModalGen('INM40017B00000','ALERT')
  BLANK
  DELETE
  IF !llNewLine
    GO TOP
    lcDispDet = IIF(EOF(),'DISABLE','')
    SCATTER MEMVAR
    SHOW GETS WINDOW (lcWinCh32) &lcDispDet ONLY
    SHOW GETS WINDOW (lcWinCh34) &lcDispDet ONLY
    SHOW GETS WINDOW (lcWinCh35) &lcDispDet ONLY
    =lfRefresh(lcWinCh32)
    =lfRefresh(lcWinCh34)
    IF EOF()
      SHOW GET pbNew ENABLE 
      SHOW GET pbPacks ENABLE
    ENDIF
  ENDIF
  *B603727,1 NAD 09/17/2000 (Start) Activate the browse window to decactivate the window of the 
  *B603727,1                of the sizes to call lfDARDINV() which will disable the size fields.
  ACTIVATE WINDOW (lcInvBrow)
  *B603727,1 (End)
  SHOW WINDOW (lcInvBrow) REFRESH SAME
ENDIF
*C037816,1 MHM 04/06/2004 Get Valid Bin[Start]
IF ASCAN(laEvntTrig , PADR('DLVLDBIN',10)) <> 0
  =gfDoTriger('ARDINV',PADR('DLVLDBIN',10)) 
ENDIF  
*C037816,1 [End]

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
IF Desc1 <> m.Desc1
  llCUpdate = .T.
  =RLOCK()
  REPLACE Desc1 WITH m.Desc1
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
PARAMETERS lnSalesRep
PRIVATE lcSalesRep

lcSalesRep = STR(lnSalesRep,1)
IF Comm&lcSalesRep <> m.Comm&lcSalesRep
  llCUpdate = .T.
  =RLOCK()
  REPLACE Comm&lcSalesRep WITH m.Comm&lcSalesRep
  UNLOCK
ENDIF
*!*************************************************************
*! Name      : FUNCTION lfwPrice
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : When function for Style price
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfwPrice()
*!*************************************************************
FUNCTION lfwPrice

IF !(laSetups[19,2] $ 'DB')
  RETURN .F.
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

*B603449,1 ABD If statement to cheak if user inter Negative values. [ Begin ]
IF m.Gros_Price < 0 
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.Gros_Price = lcOldValue
  _CUROBJ      = _CUROBJ
  RETURN
ENDIF
*B603449,1 ABD [ End ]


m.Price = ROUND(m.Gros_Price*(100-m.Disc_Pcnt)/100,2)
=lfvNPrice()

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

m.Price = ROUND(m.Gros_Price*(100-m.Disc_Pcnt)/100,2)
=lfvNPrice(.T.)

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
PARAMETERS llPcnt

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

IF m.Price <> Price
  *--recalculate merchandise tax
  STORE .T. TO llReMerTax,llCUpdate
  SELECT (lcInvLine)
  *-- Shipped
  laData[31] = laData[31] + TotQty*(m.Price - Price)
  *-- approval amount
  laData[48] = laData[31]
  
  *B603293,1 (Start)
  *laData[33] = -laData[31]*laData[32]/100
  *-- Merchendise discount
  laData[33] = -ROUND(laData[31]*laData[32]/100,2)
  *B603293,1 (End)
  lnTaxDue   = lnTaxDue + IIF(lTaxable,TotQty*m.Price-TotQty*Price,0)
  SELECT (lcInvHdr)  
  =RLOCK()
  REPLACE SHIPAMT  WITH laData[31] ,;
          APPRAMT  WITH laData[48] ,;
          Discount WITH laData[33] ,;
          nTaxDue  WITH lnTaxDue   ,;
          lCompUps WITH .T.
  UNLOCK
  SELECT (lcInvLine)  
  IF laSetups[9,2]='Y' .AND. llIsEngland .AND. Style.nTaxBreak <> 0
    lnQuantity = 0
    *-- Sum the quantity for only the sizes that is Greater than the tax break weight
    FOR lnCount = Style.nTaxBreak TO 8
      lnQuantity = lnQuantity+EVAL('m.Qty'+STR(lnCount,1))
    ENDFOR
    lnMerchTax = lnMerchTax + lnQuantity * (m.Price-Price) * nTaxRate/100
  ENDIF
  m.Disc_Pcnt  = IIF(llPcnt,m.Disc_Pcnt,IIF(m.Gros_Price=0,0,MAX(100-m.Price*100/m.Gros_Price,0)))
  m.Gros_Price = IIF(m.Disc_Pcnt=0,m.Price,m.Gros_Price)
  =RLOCK()
  REPLACE Price      WITH m.Price ,;
          Gros_Price WITH m.Gros_Price ,;
          Disc_Pcnt  WITH m.Disc_Pcnt
  UNLOCK
  SHOW GET m.Gros_Price
  SHOW GET m.Disc_Pcnt
  SHOW GET m.Price
  SHOW WINDOW (lcInvBrow) REFRESH SAME
ENDIF

*C102676,1 Custom process for A.S.T. sportwear. [Begin]
IF ASCAN(laEvntTrig,PADR('UPDAMT3',10))<>0
  =gfDoTriger('ARDINV',PADR('UPDAMT3',10))
ENDIF  
*C102676,1 Custom process for A.S.T. sportwear. [End]

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
PRIVATE lnAlias,lcStyClr

IF llBrowse .OR. !EMPTY(m.Pack_Id)
  lnAlias = SELECT()
  *E301077,51 Inhance openning files to speed up transaction
  =gfOpenFile(gcDataDir+'SPck_Hdr',gcDataDir+'SPck_Hdr','SH')
  =gfOpenFile(gcDataDir+'SPck_Lin',gcDataDir+'SPCKLNST','SH')
  *E301077,51 (End)
  
  IF !SEEK('P'+m.Style+laData[2],'SPck_Lin') .AND. ;
     !SEEK('P'+m.Style+'*****','SPck_Lin')
    *E300408,1 Message : 40039
    *E300408,1 No packs found for style
    *E300408,1 Button : 00000 
    *E300408,1 Ok
    =gfModalGen('TRM40039B00000','ALERT',ALLTRIM(m.Style))
    STORE SPACE(16) TO m.Pack_Id
  ELSE
    IF !SEEK('P'+m.Style+laData[2]+m.Pack_Id,'SPck_Lin') .AND. ;
       !SEEK('P'+m.Style+'*****'+m.Pack_Id,'SPck_Lin'))
      lcStyClr = m.Style
      SET RELATION TO Type+Account+Pack_Id INTO SPck_Hdr
      *C102574,1 HBG 04/15/2002 IF Trigger to GMA set the releation to the new key[Begin]
      IF ASCAN(laEvntTrig , PADR('SETREL',10)) <> 0
        =gfDoTriger('ARDINV',PADR('SETREL',10))
      ENDIF
      *C102574,1 [End]

      lcBrFields = [Account :H="Account",Pack_Id :H="Pack Id",SPck_Hdr.Desc :H='Description',TotQty :H='Quantity']
      m.Pack_Id = IIF(ARIABROW("'P'+lcStyClr"+" FOR ACCOUNT=laData[2] OR ACCOUNT='*****'","Style packs",;
                  gnBrFSRow1, gnBrFSCol1,gnBrFSRow2, gnBrFSCol2,'','','Pack_Id','laBrowArr'),SPck_Lin.Pack_Id,&lcInvLine..Pack_Id)
      SET RELATION OFF INTO SPck_Hdr
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
    =lfChkTotQty()
    SHOW GETS WINDOW (lcWinCh32) ONLY
    SHOW GETS WINDOW (lcWinCh34) ONLY
    =lfRefresh(lcWinCh34)
    SHOW WINDOW (lcInvBrow) REFRESH SAME
  ENDIF
  *B602636,1 (End)
  llBrowse = .F.
  SELECT (lnAlias)
ENDIF
IF &lcInvLine..Pack_Id <> m.Pack_Id
  llCUpdate = .T.
  =RLOCK()
  REPLACE Pack_Id WITH m.Pack_Id 
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

IF laQtyPaste[1]+laQtyPaste[2]+laQtyPaste[3]+laQtyPaste[4]+;
   laQtyPaste[5]+laQtyPaste[6]+laQtyPaste[7]+laQtyPaste[8]=0
  *E300408,1 Message : 40016
  *E300408,1 No line quantities copied to paste!
  *E300408,1 Button : 00000 
  *E300408,1 ok
  =gfModalGen('INM40016B00000','ALERT')
  RETURN
ENDIF
SELECT(lcInvLine)
STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty
FOR lnCount = 1 TO Scale.Cnt
  lcCount = STR(lnCount,1)
  m.Qty&lcCount = laQtyPaste[lnCount]
  m.TotQty = m.TotQty + m.Qty&lcCount
ENDFOR
IF !lfChkTotQty()
  SCATTER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty
ENDIF
SHOW GETS WINDOW (lcWinCh32) ONLY
SHOW GETS WINDOW (lcWinCh34) ONLY
SHOW WINDOW (lcInvBrow) REFRESH SAME
_CUROBJ=OBJNUM(m.Qty1)

*!*************************************************************
*! Name      : FUNCTION lfvGstTax
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Compute GST Tax Percent and amount
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: Percent 
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvGstTax(.T.)
*!*************************************************************
FUNCTION lfvGstTax
PARAMETERS llPercent

*B803571,1 NAD 10/25/2000 (Start) Not to allow charges with negative values.
IF lfNegChrg()
*B803571,1 NAD  (End)

  IF !UPDATE()
    RETURN
  ENDIF  

  lnTaxAmnt = IIF(UPPER(ALLTRIM(gcContCode))='USA',lnTaxDue,laData[31])
  *B802853,1 NAD (Start)  Round tax amount in the invoice
  *laData[37] = IIF(llPercent,laData[46]*(IIF(laSetups[11,2]='M',lnTaxAmnt,;
                   lnTaxAmnt+laData[34]+laData[35]+laData[36])-;
                   lnTaxAmnt*laData[32]/100)/100,laData[37])
  laData[37] = ROUND(IIF(llPercent,laData[46]*(IIF(laSetups[11,2]='M',lnTaxAmnt,;
                   lnTaxAmnt+laData[34]+laData[35]+laData[36])-;
                   lnTaxAmnt*laData[32]/100)/100,laData[37]),2)    
  *B802853,1 NAD (End)                         
  laData[46] = IIF(!llPercent,laData[37]*100/;
                  (IIF(laSetups[11,2]='M',lnTaxAmnt,lnTaxAmnt+laData[34]+;
                  laData[35]+laData[36])-ABS(lnTaxAmnt*laData[32]/100)),laData[46])
  laData[43] = IIF(llIsCanada .AND. VAL(laData[44])=2,;
               (laData[31]+laData[34]+laData[35]+laData[36]+laData[37]-;
               laData[31]*laData[32]/100)*laData[45]/100,laData[43])
  IF llCod
    laData[28]= laData[28]-&lcInvHdr..Tax_Amt-&lcInvHdr..nPstAmt+laData[37]+laData[43]
    IF laSetups[15,2]='Y' .AND. SUBSTR(lcUpsType,1,5)='USUPS' .AND. laData[25]=1 ;
      .AND. SEEK(laData[3]+laData[5]+laData[30],lcUpsBox)
      SELECT (lcUpsBox)
      =RLOCK()
      REPLACE TotalCod WITH laData[28]
      UNLOCK
    ENDIF
  ENDIF
  llCUpdate = .T.
  laData[38] = laData[38]-&lcInvHdr..Tax_Amt-&lcInvHdr..nPstAmt+laData[37]+laData[43]
  *C102212,1 (Begin) Add HST Tax.
  laData[38] = laData[38] - &lcInvHdr..nHstAmt + laData[52]
  *C102212,1 (End)
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE Cod_Amt  WITH laData[28] ,; 
          Tax_Rate WITH laData[46] ,;
          Tax_Amt  WITH laData[37] ,;
          nPstAmt  WITH laData[43] ,;
          TotalChg WITH laData[38]
  UNLOCK

  *B802653,1 (Start)
  laData[46] = &lcInvHdr..Tax_Rate
  laData[37] = &lcInvHdr..Tax_Amt
  laData[43] = &lcInvHdr..nPstAmt
  laData[28] = &lcInvHdr..Cod_Amt
  laData[38] = &lcInvHdr..TotalChg
  *B802653,1 (End)

  SHOW GETS WINDOW (lcWinChrg) ONLY
  *B602760,1 NAD (Start) Control object enable and disable in the screen
  *IF laData[46] = 0  AND lnTaxAmnt > 0
  *  SHOW GET laData[37] ENABLE
  *ELSE
  *  SHOW GET laData[37] DISABLE
  *ENDIF
  *B602760,1 NAD (End) 
  =lfRefresh(lcWinChrg)
*B803571,1 NAD 10/25/2000 (Start) Added the end if. 
ENDIF
*B803571,1 NAD 10/25/2000 (End) 
*!*************************************************************
*! Name      : FUNCTION lfvPstTax
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Compute GST Pst Percent and amount
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: Percent 
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvPstTax(.T.)
*!*************************************************************
FUNCTION lfvPstTax
PARAMETERS llPercent

*B803571,1 NAD 10/25/2000 (Start) Not to allow charges with negative values. 
IF lfNegChrg()
*B803571,1 NAD 10/25/2000 (End) 
  IF !UPDATE()
    RETURN
  ENDIF
  laData[44] = STR(lnPstRule,2)
  laData[43] = IIF(llPercent,(laData[31]+laData[34]+laData[35]+laData[36]-;
               laData[31]*laData[32]/100+IIF(VAL(laData[44])=1,0,laData[37]));
               *laData[45]/100,laData[43])
  laData[45] = IIF(!llPercent,laData[43]*100/(laData[31]+laData[34]+;
               laData[35]+laData[36]-laData[31]*laData[32]/100+;
               IIF(VAL(laData[44])=1,0,laData[37])),laData[45])
               
  IF llCod
    laData[28]= laData[28]-&lcInvHdr..nPstAmt+laData[43]
    IF laSetups[15,2]='Y' .AND. SUBSTR(lcUpsType,1,5)='USUPS' .AND. laData[25]=1 ;
      .AND. SEEK(laData[3]+laData[5]+laData[30],lcUpsBox)
      SELECT (lcUpsBox)
      =RLOCK()
      REPLACE TotalCod WITH laData[28]
      UNLOCK
    ENDIF
  ENDIF
  llCUpdate = .T.
  laData[38] = laData[38] - &lcInvHdr..nPstAmt+laData[43]
  *C102212,1 (Begin) Calculate HST Tax and add it to the total charge.
  laData[52] = (laData[31]+laData[34]+laData[35]+laData[36]-;
                ABS(laData[31]*laData[32]/100)+;
                IIF(VAL(laData[44])=1,0,laData[37]))*laData[51]/100
  laData[38] = laData[31]+laData[34]+laData[35]+laData[36] - ;
               ROUND(ABS(laData[31]*laData[32]/100),2)+laData[37]+laData[43]+laData[52]
  *C102212,1 (End)
  *B603293,1 (End)             
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE Cod_Amt  WITH laData[28] ,; 
          cTaxRule WITH laData[44] ,; 
          nPstRate WITH laData[45] ,;
          nPstAmt  WITH laData[43] ,;
          TotalChg WITH laData[38]
  UNLOCK
  SHOW GETS WINDOW (lcWinChrg) ONLY
  *B602760,1 NAD (Start) Control object enable and disable in the screen
  *IF laData[45] = 0
  *  SHOW GET laData[43] ENABLE
  *ELSE
  *  SHOW GET laData[43] DISABLE
  *ENDIF
  *B602760,1 NAD (End) 
  =lfRefresh(lcWinChrg)
*B803571,1 NAD 10/25/2000 (Start)  Not to allow charges with negative values.
ENDIF
*B803571,1 NAD  (End)
*!*************************************************************
*! Name      : lfvTerms
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid Invoice terms code
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

*E301386,1 (Start) Add the edit mode to the if condition
*IF laData[9] <> ALLTRIM(laTerms[lnTerms,2]) .OR. &lcInvHdr..SHIPDATE <> laData[6]
IF laScrMode[3] OR laData[9] <> ALLTRIM(laTerms[lnTerms,2]) .OR. &lcInvHdr..SHIPDATE <> laData[6]
*E301386,1 (End)
  laData[9] = ALLTRIM(laTerms[lnTerms,2])  && Get the new term code
  *B803397,1 (Start) Store the trade discount amount because gfRltFld will overwrite with
  *B803397,1         the default value 
  IF laScrMode[3]
    lnOTrdDis=laData[29]
  ENDIF
  *B803397,1 (End) 
  =gfRltFld(laData[9],@laTRltFld,'CTERMCODE')
  *B803397,1 (Start) Restore the trade discount amount
  IF laScrMode[3]
    laData[29]=lnOTrdDis
  ENDIF
  *B803397,1 (End)
  lcTEOM = ALLTRIM(lcTEOM)
  *E301217,1 AMM If no value entered, default by 20
  *Ren
  *lnEOMDay = IIF(lnEOMDay = 0,20,lnEOMDay-1)
  lnEOMDay = IIF(TYPE('lnEOMDay') <> 'N' .OR. lnEOMDay = 0,20,lnEOMDay-1)
  *E301217,1 AMM end
  lcTCod = ALLTRIM(lcTCod)
  llCod  = (lcTCod='Y')
  IF llIsEngland
    *-- get the new due date
    laData[7] = IIF(lcTEOM <> 'Y',laData[6]+lnTDaysDue,;
                    CTOD('01'+SUBSTR(DTOC(GOMONTH(laData[6],1)),3))-1+lnTDaysDue)
   ELSE 
     *-- EOM 	End of Month 		m/10/yy
     *-- if  day of invoice > End of Month date 		
     *-- m/10/yy + 2 monthes+ net due date	
     *-- if day of invoice < End of Month date 		
     *-- m/10/yy + 1 monthes+ net due date                  
     *E301217,1 AMM Calculate due to the end of month day variable which is 
     *E301217,1 AMM related field to the payment terms code. 
     *laData[7] = IIF(lcTEOM <> 'Y',laData[6] + lnTDaysDue,;
                GOMONTH(CTOD(SUBSTR(DTOC(laData[6]),1,3) +'10'+;
                SUBSTR(DTOC(laData[6]),6,5)),IIF(DAY(laData[6])>20,2,1))+lnTDaysDue)
     laData[7] = IIF(lcTEOM <> 'Y',laData[6] + lnTDaysDue,;
                GOMONTH(CTOD(SUBSTR(DTOC(laData[6]),1,3) +'10'+;
                SUBSTR(DTOC(laData[6]),6,5)),IIF(DAY(laData[6])>lnEOMDay,2,1))+lnTDaysDue)
     *E301217,1 AMM end
  ENDIF
  SHOW GET laData[7]
  *E301386,1 (Start) update the temp file in case of add mode only
  *SELECT (lcInvHdr)
  *  =RLOCK()
  * REPLACE lCompUps WITH .T.
  * UNLOCK
  *STORE .T. TO llReMerTax
  IF !laScrMode[3]  
    SELECT (lcInvHdr)
    =RLOCK()
    REPLACE lCompUps WITH .T.
    UNLOCK
    STORE .T. TO llReMerTax
  ENDIF
  *E301386,1 (End)
ENDIF

*!*************************************************************
*! Name      : lfvShipVia
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid Invoice shipvia code
*!*************************************************************
*! Calls     : CODECHK
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvShipVia()
*!*************************************************************
FUNCTION lfvShipVia

IF laData[10] <> ALLTRIM(laShipVia[lnShipVia,2])
  laData[10] = ALLTRIM(laShipVia[lnShipVia,2])
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE lCompUps WITH .T.
  UNLOCK
ENDIF

*!*************************************************************
*! Name      : lfvSpcInst
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid Invoice special instruction code
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

IF laData[11] <> ALLTRIM(laSpcInst[lnSpcInst,2])
  laData[11] = ALLTRIM(laSpcInst[lnSpcInst,2])
ENDIF

*!*************************************************************
*! Name      : lfvSeason
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid Invoice season code
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

IF laData[12] <> SUBSTR(laSeasons[lnSeason,2],1,6)
  laData[12] = SUBSTR(laSeasons[lnSeason,2],1,6)
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

IF laData[13] <> SUBSTR(laDivision[lnDivision,2],1,6)
  laData[13] = SUBSTR(laDivision[lnDivision,2],1,6)
  IF laSetups[14,2]='D'
  
    *B606206,1 TMI [Start] 
    **C200340,1 ABD Add new Custom trigger for customer stuncroft. [Begin]
    *IF ASCAN(laEvntTrig , PADR('ASLSREP1',10)) <> 0
    *  =gfDoTriger('ARDINV',PADR('ASLSREP1',10))      
    *ENDIF  
    *IF ASCAN(laEvntTrig , PADR('ASLSREP2',10)) <> 0
    *  =gfDoTriger('ARDINV',PADR('ASLSREP2',10))
    *  RETURN
    *ENDIF
    **C200340,1 ABD [End]    
    *B606206,1 TMI [End  ] 

    *B603998,1 Correct restoring sales rep commission [Begin]
    *IF SEEK(laData[17]+laData[13],'REP_DIV')
    *  STORE REP_DIV.Comm_Rate TO laData[18],lnSouComm1
    *ENDIF
    *IF SEEK(laData[19]+laData[13],'REP_DIV')
    *  STORE REP_DIV.Comm_Rate TO laData[20],lnSouComm2
    *ENDIF        
    STORE IIF(SEEK(laData[17]+laData[13],'REP_DIV'),REP_DIV.Comm_Rate,Customer.Comm) ;
          TO laData[18],lnSouComm1
    STORE IIF(SEEK(laData[19]+laData[13],'REP_DIV'),REP_DIV.Comm_Rate,Customer.Comm2) ;
          TO laData[20],lnSouComm2
    *B603998,1 Correct restoring sales rep commission [End]

    
    *B606206,1 TMI [Start] Load sales rep for the selected customer/store
    IF ASCAN(laEvntTrig , PADR('VACC',10)) <> 0
      =gfDoTriger('ARDINV',PADR('EMPT17',10))
      =gfDoTriger('ARDINV',PADR('EMPT19',10))
      =gfDoTriger('ARDINV',PADR('VACC',10))
    ENDIF
    *B606206,1 TMI [End  ] Load sales rep for the selected customer/store 
     
    *-- Recalculate invoice commission
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

*E301386,1 (Start) Validate the if condition in the add mode only
*IF !llBrowse .AND. laData[14] = &lcInvHdr..Dept
*  RETURN
*ENDIF
IF !llBrowse .AND. laData[14] = IIF(laScrMode[3],InvHdr.Dept,&lcInvHdr..Dept)
  RETURN
ENDIF
*E301386,1 (End)

*E301077,51 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'CustDept',gcDataDir+'CustDept','SH')
*E301077,51 (End)

IF llBrowse .OR. (!EMPTY(laData[14]) .AND. !SEEK(laData[2]+laData[14],'CustDept'))
  IF !SEEK(laData[2],'CustDept')
    IF llBrowse
      *E300408,1 Message : 40018
      *E300408,1 No departments assigend to account xxx
      *E300408,1 Button : 00000
      *E300408,1 ok
      =gfModalGen('INM40018B00000','ALERT',laData[2])
      STORE SPACE(5) TO laData[14]
    ENDIF
    llBrowse = .F.
    RETURN
  ENDIF
  SELECT CustDept
  lcBrFields = [Dept:H="Department",cDeptDesc :H="Description",Rep1,Comm1,Rep2,Comm2,cTermCode]
  laData[14] = IIF(ARIABROW('laData[2]',"Departments",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Dept','laBrowArr'),;
                  CustDept.Dept,laData[14])
ENDIF
llBrowse = .F.
*E301386,1 (Start) Not to overwrite the customer Defaults by the department defaults 
IF !laScrMode[3]
*E301386,1 (end)

  IF laData[14] <> &lcInvHdr..Dept  
    IF SEEK(laData[2]+laData[14],'CustDept')
      *E300408,1 Message : 40019
      *E300408,1 Customer defaults will be overwritten by the department defaults.
      *E300408,1 Button : 00000
      *E300408,1 ok
      =gfModalGen('INM40019B00000','ALERT')
      STORE CustDept.Rep1  TO laData[17]   && sales rep at department level   
      STORE CustDept.Rep2  TO laData[19]
      =lfvRep1() .AND. lfvRep2()
      SELECT (lcInvHdr)
      =RLOCK()
      REPLACE Rep1  WITH CustDept.Rep1 ,;
              Rep2  WITH CustDept.Rep1 ,;
              cTermCode WITH CustDept.cTermCode
      UNLOCK
      =gfwCodePop(@laCodes,'CTERMCODE','T')
      STORE CustDept.Comm1 TO laData[18],lnSouComm1  && commission at division level
      STORE CustDept.Comm2 TO laData[20],lnSouComm2
      SHOW GET laData[17]
      SHOW GET laData[18]
      SHOW GET laData[19]
      SHOW GET laData[20]
      *-- recalculate terms
      =lfvTerms()
      *-- Recalculate commission 
      =lfvCommision()
    ENDIF
  ENDIF
*E301386,1 (Start)
ENDIF
*E301386,1 (End) add the end if 
*E301077,51 Inhance openning files to speed up transaction
=gfCloseFile('CustDept')
*E301077,51 (End)

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
PRIVATE lnAlias
lnAlias = SELECT()
IF llBrowse OR (!EMPTY(laData[24]) .AND. !SEEK(laData[24],'SycFact'))
  lcBrFields = [cFacCode:H='Factor',cFacComp:H='Name',cFacCont:H='Contact',cPhoneNo :P= gfPhoneTem() :H='Phone']
  SELECT SycFact
  laData[24] = IIF(ARIABROW('',"Factors",gnBrFSRow1, gnBrFSCol1,;
                  gnBrFSRow2, gnBrFSCol2,'','','cFacCode','laBrowArr'),;
                  SycFact.cFacCode,SPACE(6))
ENDIF
=lfRefresh(lcWinChrg)
SELECT (lnAlias)
llBrowse = .F.

*!*************************************************************
*! Name      : lfvCurrency
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid function for currency fields
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcCurrVar : Variable holds currency
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCurrency()
*!*************************************************************
FUNCTION lfvCurrency
PRIVATE lcCurrency

IF llBrowse .OR.  laData[22] <> &lcInvHdr..cCurrCode
  *E301077,51 Inhance openning files to speed up transaction
  =gfOpenFile(gcSysHome+'SycCurr',gcSysHome+'cCurrCode','SH')
  *E301077,51 (End)

  lcCurrency = laData[22]
  IF !gfCurrBrow(@lcCurrency)
    lcCurrency = &lcInvHdr..cCurrCode
  ENDIF
  laData[22] = lcCurrency
  IF laData[22] <> &lcInvHdr..cCurrCode
    IF laData[22] = gcBaseCurr
      STORE 1 TO laData[40], laData[23]  &&,currency unit ,currency rate
    ELSE
      laData[23] = gfChkRate('laData[40]',laData[22],laData[41],.T.,gcAct_Comp,.F.,.T.)
      IF laData[23] = 0
        *E300511,1 Message : 00262
        *E300511,1 A valid xxx to xxx exchange rate could not be found on xxx.
        *E300511,1 Button : 00000
        *E300511,1 Ok
        =gfModalGen('INM00262B00000','ALERT',ALLTRIM(laData[22])+'|'+ALLTRIM(gcBaseCurr)+'|'+DTOC(laData[41]))
        laData[22] = &lcInvHdr..cCurrCode
        laData[23] = &lcInvHdr..nExRate
      ENDIF  
    ENDIF
  ENDIF  
  IF !llEditExRt OR laData[22]=gcBaseCurr
    SHOW GET laData[23] DISABLE
  ELSE
    SHOW GET laData[23] ENABLE
  ENDIF
  *E301077,51 Inhance openning files to speed up transaction
  =gfCloseFile('SycCurr')
  *E301077,51 (End)
  
  *E301077,51 Inhance openning files to speed up transaction
  *SELECT CREDIT
  *=SEEK(laData[2])
  *LOCATE REST WHILE Account+Tran+DTOS(TranDate) = laData[2] FOR ;
  *cCurrCode = laData[22]
  *llActCrdt = FOUND()
  DO gfAcOpCrdt IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
                   laData[2],laData[22],'llActCrdt'
  *E301077,51 (End)

  SELECT (lcInvHdr)  
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

IF laData[23] <=0
  *E300408,1 Message : 40037
  *E300408,1 The exchange rate must be greater than zero
  *E300408,1 Button : 00000 
  *E300408,1 Ok
  =gfModalGen('INM40037B00000','ALERT')
  laData[23] = &lcInvHdr..nExRate
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

*B602325,1 Accept empty sales rep.
*IF llBrowse .OR. !SEEK(laData[17],'SalesRep')
IF llBrowse .OR. (!EMPTY(laData[17]) AND !SEEK(laData[17],'SalesRep'))
*B602325,1 (End)w

  laData[17] = IIF(llBrowse,SPACE(3),laData[17])
  lcRep = laData[17]
  DO REPCHK WITH lcRep
  laData[17] = lcRep
ENDIF  

*B604091,1 Update commission based on Salesrep commission base setup. [Begin]
IF &lcInvHdr..Rep1 <> laData[17]
  IF laSetups[14,2]='D'
    IF SEEK(laData[17]+laData[13],'REP_DIV')
      STORE REP_DIV.Comm_Rate TO laData[18],lnSouComm1
    ELSE
      STORE IIF(EMPTY(laData[17]),0,SalesRep.Comm) TO laData[18],lnSouComm1
    ENDIF
  ELSE  
    STORE IIF(EMPTY(laData[17]),0,IIF(Customer.salesrep # laData[17],SalesRep.Comm,CUSTOMER.Comm)) TO laData[18],lnSouComm1
  ENDIF
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE Rep1  WITH laData[17] ,;
          Comm1 WITH laData[18]
  UNLOCK
  llCUpdate = .T.
  SHOW GET laData[18]  
  =lfRefresh(lcWinCh2)
ENDIF
*B604091,1 Update commission based on Salesrep commission base setup. [End]

*C200340,1 ABD Add new Custom trigger for customer stuncroft. [Begin]
IF ASCAN(laEvntTrig , PADR('ASLSREP1',10)) <> 0
  =gfDoTriger('ARDINV',PADR('ASLSREP1',10))
ENDIF
*C200340,1 ABD [End]

llBrowse = .F.

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
IF llBrowse .OR. (!EMPTY(laData[19]) .AND. !SEEK(laData[19],'SalesRep'))
  laData[19] = IIF(llBrowse,SPACE(3),laData[19])
  lcRep = laData[19]
  DO REPCHK WITH lcRep
  laData[19] = lcRep
ENDIF  

*B604091,1 Update commission based on Salesrep commission base setup. [Begin]
IF &lcInvHdr..Rep2 <> laData[19]
  IF laSetups[14,2]='D'
    IF SEEK(laData[19]+laData[13],'REP_DIV')
      STORE REP_DIV.Comm_Rate TO laData[20],lnSouComm2
    ELSE
      STORE IIF(EMPTY(laData[19]),0,SalesRep.Comm) TO laData[20] , lnSouComm2
    ENDIF
  ELSE
    STORE IIF(EMPTY(laData[19]),0,IIF(Customer.Rep2 # laData[19],SalesRep.Comm,CUSTOMER.Comm2)) TO laData[20],lnSouComm2
  ENDIF
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE Rep1  WITH laData[19] ,;
          Comm1 WITH laData[20]
  UNLOCK
  llCUpdate = .T.
  SHOW GET laData[20]  
  =lfRefresh(lcWinCh2)
ENDIF
*B604091,1 Update commission based on Salesrep commission base setup. [End]

*C200340,1 ABD Add new Custom trigger for customer stuncroft. [Begin]
IF ASCAN(laEvntTrig , PADR('ASLSREP2',10)) <> 0
  =gfDoTriger('ARDINV',PADR('ASLSREP2',10))
ENDIF
*C200340,1 ABD [End]

llBrowse = .F.

*!*************************************************************
*! Name      : lfUpdate
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Update Invoice header other information
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
  *E301386,1 (Start)
  IF laScrMode[4]
  *E301386,1 (End)
    SELECT (lcInvHdr)
    =RLOCK()
    GATHER FROM laData FIELDS &lcScFields
    UNLOCK
  *E301386,1 (Start)
  ENDIF
  *E301386,1 (End)
  =lfRefresh(lcWinCh2)
  llCUpdate = .T.
ENDIF
*!*************************************************************
*! Name      : lfvWareCode
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Invoice warehouse code
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
laData[21] = laWareHouses[lnWareHouse,2]

*!*************************************************************
*! Name      : lfvTrdeDisc
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Invoice trade discount
*!*************************************************************
*! Calls     : lfMechTax,lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvTrdeDisc()
*!*************************************************************
FUNCTION lfvTrdeDisc

SELECT (lcInvHdr)
=RLOCK()
REPLACE Trde_Disc WITH laData[29]
UNLOCK
*-- Compute England merchandise tax
=IIF(llIsEngland,lfMechTax(),.T.)
SHOW GETS WINDOW (lcWinChrg) ONLY
=lfRefresh(lcWinChrg)

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

IF !UPDATE()
  RETURN
ENDIF

laData[33] = IIF(llPercent,-1*laData[31]*laData[32]/100,-1*ABS(laData[33]))
*B802653,1 (Start)
laData[33] = ROUND(laData[33],2)
*B802653,1 (End)

laData[32] = IIF(!llPercent,100*ABS(laData[33])/laData[31],laData[32])
IF laSetups[9,2] = 'Y' 
  lnTaxAmnt = IIF(UPPER(ALLTRIM(gcContCode))='USA',lnTaxDue,laData[31])
  *B802853,1 NAD (Start) Round tax amount in the invoice   
  *laData[37] = laData[46]*(IIF(laSetups[11,2]='M',lnTaxAmnt,;
               lnTaxAmnt+laData[34]+laData[35]+laData[36])-;
               lnTaxAmnt*laData[32]/100 )/100
               
  laData[37] = ROUND(laData[46]*(IIF(laSetups[11,2]='M',lnTaxAmnt,;
               lnTaxAmnt+laData[34]+laData[35]+laData[36])-;
               lnTaxAmnt*laData[32]/100 )/100,2)
  *B802853,1 NAD (End)
  IF llIsCanada
    laData[43] = (laData[31]+laData[34]+laData[35]+laData[36]-;
                  ABS(laData[31]*laData[32]/100)+;
                  IIF(VAL(laData[44])=1,0,laData[37]))*laData[45]/100
    *C102212,1 (Begin) Calculate HST Tax from amount after deducting discount.
    laData[52] = (laData[31]+laData[34]+laData[35]+laData[36]-;
                  ABS(laData[31]*laData[32]/100)+;
                  IIF(VAL(laData[44])=1,0,laData[37]))*laData[51]/100
    *C102212,1 (End)
  ENDIF
ENDIF
*-- Balance to pay
laData[38] = laData[31]+laData[34]+laData[35]+laData[36]+laData[33]+;
             laData[37]+laData[43]
*C102212,1 (Begin) Add HST Tax.
laData[38] = laData[38] + laData[52]
*C102212,1 (End)
IF llCod
*-- Cash on delivery amount
  *B603989,1 NAD 10/26/2000 Add the Frieght , COD charges to the cod amount (Begin)
   IF EMPTY(lcUpsType) OR lcUPSType='OTHER'

     *B604425,1 NNA 12/31/2003 (Start) Add the H.S.T Tax to the cod amount.
     *laData[28] = laData[31]+laData[33]+laData[37]+laData[43]
     laData[28] = laData[31]+laData[33]+laData[37]+laData[43]+laData[52]
     *B604425,1 NNA 12/31/2003 (End)

  ELSE
  *B603989,1 NAD 10/26/2000 (End)

    *B604425,1 NNA 12/31/2003 Add the H.S.T Tax to the c.o.d amount (Begin)
    *laData[28] = laData[31]+laData[34]+laData[35]+laData[36]+laData[33]+;
                  laData[37]+laData[43]
    laData[28] = laData[31]+laData[34]+laData[35]+laData[36]+laData[33]+;
                 laData[37]+laData[43]+laData[52]
    *B604425,1 NNA 12/31/2003 (End)               
               
  *B603989,1 NAD 10/26/2000 (Start) Added the end if. 
  ENDIF
  *B603989,1 NAD 10/26/2000 (End) 
ENDIF
llCUpdate = .T.
SELECT (lcInvHdr)
=RLOCK()
REPLACE DiscPcnt WITH laData[32] ,;
        Discount WITH laData[33] ,;
        Tax_Amt  WITH laData[37] ,;
        nPstAmt  WITH laData[43] ,;
        Cod_Amt  WITH laData[28] ,;
        TotalChg WITH laData[38]
*C102212,1 (Begin) Update HST tax Amount.
REPLACE nHstAmt  WITH laData[52]
*C102212,1 (End) 
UNLOCK
lnOldComm1 = laData[18]
lnOldComm2 = laData[20]
=lfvCommision()
*E300408,1 Message : 40038
*E300408,1 Would you like to overwrite invoice lines commissions with new commissions?
*E300408,1 Button : 40000
*E300408,1 Yes No
IF (lnOldComm1<>laData[18] .OR. lnOldComm2<>laData[20]) .AND. ; 
   SEEK(laData[2]+laData[3]+laData[5]+laData[30],lcInvLine) .AND. ;
   (laSetups[2,2] <> 'Y' .OR. gfModalGen('QRM40038B40000','ALERT')=1 )
  SELECT (lcInvLine) 
  SCAN
    =RLOCK()
    REPLACE Comm1 WITH laData[18], Comm2 WITH laData[20]
    UNLOCK
  ENDSCAN
  GO TOP 
  SELECT (lcInvHdr)
ENDIF
SHOW GETS WINDOW (lcWinChrg) ONLY
=lfRefresh(lcWinChrg)

*B602760,1 NAD (Start) Control object enable and disable in the screen
*IF laData[32] = 0
*  SHOW GET laData[33] ENABLE
*ELSE
*  SHOW GET laData[33] DISABLE
*ENDIF
*B602760,1 NAD (End)
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

*B606206,1 TMI [Start] Ignote "lfvCommision" function for ENG07
IF ASCAN(laEvntTrig , PADR('VACC',10)) <> 0
  RETURN
ENDIF
*B606206,1 TMI [End  ] Ignote "lfvCommision" function for ENG07

IF SEEK(laData[17],'SalesRep')
  
  *B604091,1 SSE Sort the reduce commesion of sales rep. [Begin]
  *DO CASE
  *  CASE laData[32] >= SalesRep.Discnt4
  *    laData[18] = lnSouComm1 * (1-SalesRep.Redcom4/100)
  *  CASE laData[32] >= SalesRep.Discnt3
  *    laData[18] = lnSouComm1 * (1-SalesRep.Redcom3/100)
  *  CASE laData[32] >= SalesRep.Discnt2
  *    laData[18] = lnSouComm1 * (1-SalesRep.Redcom2/100)
  *  CASE laData[32] >= SalesRep.Discnt1
  *    laData[18] = lnSouComm1 * (1-SalesRep.Redcom1/100)
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
    CASE laData[32] >= laDiscnt[4,1]
      laData[18] = lnSouComm1 * (1-laDiscnt[4,2]/100)
    CASE laData[32] >= laDiscnt[3,1]
      laData[18] = lnSouComm1 * (1-laDiscnt[3,2]/100)
    CASE laData[32] >= laDiscnt[2,1]
      laData[18] = lnSouComm1 * (1-laDiscnt[2,2]/100)
    CASE laData[32] >= laDiscnt[1,1]
      laData[18] = lnSouComm1 * (1-laDiscnt[1,2]/100)
  ENDCASE
  *B604091,1 SSE Sort the reduce commesion of sales rep. [End]
  
  IF laData[18] <> &lcInvHdr..Comm1
    *E300408,1 Message : 40023
    *E300408,1 xxx commission decreased to xxx%
    *E300408,1 Button : 00000
    *E300408,1 Ok
    =gfModalGen('INM40023B00000','ALERT',laData[17]+'|'+STR(laData[18],5,2))
  ENDIF
ENDIF
IF SEEK(laData[19],'SalesRep')
  
  *B604091,1 SSE Sort the reduce commesion of sales rep. [Begin]
  *DO CASE
  *  CASE laData[32] >= SalesRep.Discnt4
  *    laData[20] = lnSouComm2 * (1-SalesRep.Redcom4/100)
  *  CASE laData[32] >= SalesRep.Discnt3
  *    laData[20] = lnSouComm2 * (1-SalesRep.Redcom3/100)
  *  CASE laData[32] >= SalesRep.Discnt2
  *    laData[20] = lnSouComm2 * (1-SalesRep.Redcom2/100)
  *  CASE laData[32] >= SalesRep.Discnt1
  *    laData[20] = lnSouComm2 * (1-SalesRep.Redcom1/100)
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
    CASE laData[32] >= laDiscnt[4,1]
      laData[20] = lnSouComm2 * (1-laDiscnt[4,2]/100)
    CASE laData[32] >= laDiscnt[3,1]
      laData[20] = lnSouComm2 * (1-laDiscnt[3,2]/100)
    CASE laData[32] >= laDiscnt[2,1]
      laData[20] = lnSouComm2 * (1-laDiscnt[2,2]/100)
    CASE laData[32] >= laDiscnt[1,1]
      laData[20] = lnSouComm2 * (1-laDiscnt[1,2]/100)
  ENDCASE
  *B604091,1 SSE Sort the reduce commesion of sales rep. [End]

  IF laData[20] <> &lcInvHdr..Comm2
    *E300408,1 Message : 40023
    *E300408,1 xxx commission decreased to xxx%
    *E300408,1 Button : 00000
    *E300408,1 Ok
    =gfModalGen('INM40023B00000','ALERT',laData[19]+'|'+STR(laData[20],5,2))
  ENDIF  
ENDIF

SELECT (lcInvHdr)
=RLOCK()
REPLACE Comm1 WITH laData[18] ,;
        Comm2 WITH laData[20]
UNLOCK
SHOW GET laData[18]
SHOW GET laData[20]

*!*************************************************************
*! Name      : lfvInvoice
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Invoice number
*!*************************************************************
*! Calls     : lfInvBrow,gfSeekRec
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvInvoice()
*!*************************************************************
FUNCTION lfvInvoice
PRIVATE lcInvoice

IF llBrowse .OR. (!EMPTY(laData[1]) .AND. !SEEK(laData[1],'INVHDR'))
  lcInvoice = laData[1]
  =lfInvBrow(@lcInvoice,laData[2],laData[3],laData[4],laData[5])
  laData[1] = lcInvoice
  llBrowse = .F.
ENDIF
IF !EMPTY(laData[1])
 *-- Seek for a record with the main key fields values in the base file of the
 *-- Program 
  =gfSeekRec()
  *C102574,1 HBG 04/15/2002 IF Trigger to GMA Open Pack header by the new index[Begin]
  IF ASCAN(laEvntTrig , PADR('OPENFILE',10)) <> 0
    =gfDoTriger('ARDINV',PADR('OPENFILE',10))
  ENDIF  
  *C102574,1 [End]
  
ENDIF  

*!*************************************************************
*! Name      : lfInvBrow
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Browse Invoices
*!*************************************************************
*! Calls     : AriaBrow,gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfInvBrow()
*!*************************************************************
FUNCTION lfInvBrow
PARAMETERS lcInvoice,lcAccount,lcInvOrd,lcCustPo,lcStore
PRIVATE lnAlias,lnCurrTag

GO TOP IN 'INVHDR'
IF EOF('INVHDR')
  *E300408,1 Message : 40036
  *E300408,1 The invoice file is empty.
  *E300408,1 Button : 00000
  *E300408,1 Ok
  =gfModalGen('INM40036B00000','ALERT')
  STORE SPACE(6) TO lcInvoice
  RETURN
ENDIF  
lnAlias = SELECT()
SELECT INVHDR
GO TOP
lnCurrTag = TAG()
SET ORDER TO TAG INVHDR
IF !EMPTY(lcAccount)
  SET ORDER TO TAG INVHDRA
  =SEEK(lcAccount)
ENDIF
LOCATE REST WHILE Account=IIF(EMPTY(lcAccount),'',lcAccount) ;
             FOR  ORDER  =IIF(EMPTY(lcInvOrd),"",lcInvOrd) .AND. ;
                  CUSTPO =IIF(EMPTY(lcCustpo),"",lcCustpo) .AND. ;
                  STORE  =IIF(EMPTY(lcStore),"",lcStore)
IF !FOUND()
  *E300408,1 Message : 40084
  *E300408,1 No invoices found
  *E300408,1 Button : 00000 
  *E300408,1 Ok
  =gfModalGen('INM40084B00000','ALERT',IIF(!EMPTY(lcAccount),IIF(EMPTY(lcStore),'for account: '+lcAccount,'for account/store: '+lcAccount+'/'+lcStore),''))
  STORE SPACE(6) TO lcInvoice
  SET ORDER TO TAG lnCurrTag
  SELECT (lnAlias)
  RETURN
ENDIF
GO TOP
lcInvoice = ;
IIF(AriaBrow(IIF(!EMPTY(lcAccount),[lcAccount],'')+;
             ' FOR (ORDER=IIF(EMPTY(lcInvOrd),"",lcInvOrd)) .AND. ;
                   (CUSTPO=IIF(EMPTY(lcCustpo),"",lcCustpo)) .AND. ;
                   (STORE=IIF(EMPTY(lcStore),"",lcStore))',;
lcFile_Ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Invoice','laBrowArr'),InvHdr.Invoice,SPACE(6)) 
SET ORDER TO TAG lnCurrTag
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvOrder
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Invoice order
*!*************************************************************
*! Calls     : ORDBROWO,ORDBROWA
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvOrder()
*!*************************************************************
FUNCTION lfvOrder
PRIVATE XOrder,XAccount,XSTORE

*-- get the ivoice of that order
IF llBrowse .OR. (!EMPTY(laData[3]) .AND. !SEEK('O'+laData[3],'OrdHdr'))
  XAccount = laData[2]
  XORDER   = laData[3]
  XSTORE   = laData[5]
  llBrowse = .T.
  laData[3] = IIF(IIF(EMPTY(XAccount),ORDBROWO(@XORDER, .T., 'O'),ORDBROWA(XAccount,.T.,'O')),XORDER,'')
ENDIF
llBrowse = .F.
IF !EMPTY(laData[3])
  laData[2] = OrdHdr.Account
  llBrowse = .T.
  _CUROBJ = OBJNUM(laData[1])
  KEYBOARD "{ENTER}"
ENDIF
  
*!*************************************************************
*! Name      : lfvAccount
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Invoice Account
*!*************************************************************
*! Calls     : CUSBROWM,gfGetAdr,gfChkRate,gfModalGen,gfActWind,lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvAccount()
*!*************************************************************
FUNCTION lfvAccount
PRIVATE xAccount

*B603401,1 Fix Error while browse account
IF !(llBrowse .OR. (!EMPTY(laData[2]) AND LASTKEY()=13))
  RETURN
ENDIF
*B603401,1 (End)
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
    STORE SPACE(8) TO laData[5]
    SHOW GET ibStore   DISABLE
    SHOW GET laData[5] DISABLE
  ELSE
    SHOW GET ibStore   ENABLE
    SHOW GET laData[5] ENABLE
  ENDIF
ENDIF
IF laScrMode[4] .AND. !EMPTY(laData[2])
  *B606206,1 TMI [Start] 
  **B606120,1 NAD
  *IF ASCAN(laEvntTrig , PADR('VACC',10)) <> 0
  *  IF gfDoTriger('ARDINV',PADR('VACC',10))
  *    RETURN   
  *   ENDIF   
  *ENDIF
  **B606120,1 NAD
  *B606206,1 TMI [End  ] 
  IF Customer.Status <> 'A'
    *E301466,1 AMH Allow Creat invoice for Potential Customer [Start]
    *E300408,1 Message : 40022
    *E300408,1 This a non-active account. Invoice is not allowed!
    *E300408,1 Button : 00000
    *E300408,1 Ok
    *=gfModalGen('INM40022B00000','ALERT','account')
    IF Customer.Status <> 'P'
      *E301466,1 Message : 40022
      *E301466,1 This a ð account. Invoice is not allowed!
      *E301466,1 Button : 00000
      *E301466,1 Ok
      lcMessage = IIF(Customer.Status = 'H','Hold','Canceled') + ' account'
      =gfModalGen('INM40022B00000','ALERT',lcMessage)
      _CUROBJ = _CUROBJ
      RETURN
    ELSE
      *E301466,1 Message : 40173
      *E301466,1 Tetxt   : The ð you have selected is Potential.
      *E301466,1 Tetxt   : Would you like to continue and make it Active ?
      *E301466,1 Button  : 40000
      *E301466,1         : Yes ; No
      IF gfModalGen("QRM40173B40000","DIALOG",'account')=2
        STORE &lcInvHdr..Account TO laData[2]
        _CUROBJ = _CUROBJ
        RETURN
      ELSE
        lnAlias = SELECT(0)
        IF SEEK('M'+laData[2],'Customer') .AND. Customer.Status = 'P' 
          SELECT Customer
          IF gfObj_Lock(.T.)
            REPLACE Customer.Status WITH "A"
            =gfObj_Lock(.F.)
          ELSE
            STORE &lcInvHdr..Account TO laData[2]
            _CUROBJ = _CUROBJ
            RETURN
          ENDIF
        ENDIF  
        SELECT (lnAlias)
      ENDIF
    ENDIF
    *E301466,1 AMH Allow Creat invoice for Potential Customer [End  ]
  ENDIF  
  lcPriceLvl = IIF(!EMPTY(Customer.PriceLvl),Customer.PriceLvl,'A')
  *-- Customer Bill To name
  STORE Customer.BtName TO lcBillName,lcDBllName
  =gfGetAdr('Customer','','','',1,'2')
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcDBllAdd&lcCount = lcDBllAdd&lcCount + IIF(EMPTY(lcDBllAdd&lcCount),'',',')+;
      SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
    lcBillAdd&lcCount = lcDBllAdd&lcCount
  ENDFOR  
  *-- Dba =Do business as 
  *-- get the shipname  
  STORE IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba) TO lcDShpName,lcShipName
  =gfGetAdr('CUSTOMER','','','',1,'')
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcDShpAdd&lcCount = lcDShpAdd&lcCount + IIF(EMPTY(lcDShpAdd&lcCount),'',',')+;
    SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
    lcShipAdd&lcCount = lcDShpAdd&lcCount
  ENDFOR 
  llupsinsur = (Customer.cInsur='Y')
  *-- get the tax rate from the customer in case of canada
  STORE IIF(laSetups[9,2] <> 'Y',0,;
            IIF(llIsCanada,laSetups[10,2],Customer.nTaxRate)) TO laData[46]
  *-- PST Rate for canada only
  STORE IIF(laSetups[9,2]='Y' .AND. llIsCanada,Customer.nTaxRate,0) TO laData[45]
  *C102212,1 (Begin) Update HST Tax wit its default.
  STORE IIF(laSetups[9,2]='Y' .AND. llIsCanada,laSetups[21,2],0) TO laData[51]
  *C102212,1 (End)
  *-- tax Rule
  STORE IIF(laSetups[9,2]='Y' .AND. llIsCanada,Customer.cTaxRule,SPACE(2)) TO laData[44]
  STORE IIF(EMPTY(laData[44]),' 1',laData[44]) TO laData[44]
  STORE IIF(llIsCanada,VAL(laData[44]),1) TO lnPstRule
  *--factor code
  STORE Customer.cFacCode TO laData[24]  
  *-- the default codes
  STORE IIF(EMPTY(Customer.ShipVia),lfGetDefCode('SHIPVIA'),;
            Customer.ShipVia) TO laData[10]
  STORE IIF(EMPTY(Customer.cTermCode),lfGetDefCode('CTERMCODE'),;
            Customer.cTermCode)   TO laData[09]
  STORE IIF(EMPTY(Customer.SpcInst),lfGetDefCode('SPCINST'),;
            Customer.SpcInst) TO laData[11]
  STORE Customer.Disc      TO laData[32]
  STORE Customer.SalesRep  TO laData[17]
  STORE Customer.Rep2      TO laData[19]
  STORE Customer.Note      TO laData[15]
  STORE Customer.cCurrCode TO laData[22]
  STORE ldDefInvDate       TO laData[6],laData[41]
  STORE ldDefPstDate       TO laData[49]
  STORE lcIDefDiv          TO laData[13]
  STORE lcIDefSes          TO laData[12]
  STORE lcIDefWare         TO laData[21]
  *B606206,1 TMI [Start] 
  **B606120,1 NAD
  *IF ASCAN(laEvntTrig , PADR('ASLSREP1',10)) <> 0
  *  =gfDoTriger('ARDINV',PADR('EMPT17',10)) 
  *  =gfDoTriger('ARDINV',PADR('EMPT19',10))  
  *ENDIF
  **B606120,1 NAD
  *B606206,1 TMI [End  ] 

  *B606206,1 TMI [Start] 
  IF ASCAN(laEvntTrig , PADR('ASLSREP1',10)) = 0
  *B606206,1 TMI [End  ] 
    =lfvRep1() .AND. lfvRep2()
  *B606206,1 TMI [Start]   
  ENDIF
  *B606206,1 TMI [End  ] 

  *-- sales rep commission on devisio or on customer
  *B604091,1 Update commission based on Salesrep commission base setup. [Begin] 
  *STORE IIF(laSetups[14,2]='D' .AND. SEEK(laData[17]+laData[13],'REP_DIV'),;
  *          REP_DIV.Comm_Rate,Customer.Comm) TO laData[18],lnSouComm1
  *STORE IIF(laSetups[14,2]='D' .AND. SEEK(laData[19]+laData[13],'REP_DIV'),;
  *          REP_DIV.Comm_Rate,Customer.Comm2) TO laData[20],lnSouComm2            
  IF laSetups[14,2]='D'
    IF SEEK(laData[17]+laData[13],'REP_DIV')
      STORE REP_DIV.Comm_Rate TO laData[18],lnSouComm1
    ELSE
      STORE IIF(EMPTY(laData[17]),0,SalesRep.Comm) TO laData[18],lnSouComm1
    ENDIF

    IF SEEK(laData[19]+laData[13],'REP_DIV')
      STORE REP_DIV.Comm_Rate TO laData[20],lnSouComm2
    ELSE
      STORE IIF(EMPTY(laData[19]),0,SalesRep.Comm) TO laData[20],lnSouComm2
    ENDIF
  ELSE
    STORE IIF(EMPTY(laData[17]),0,IIF(Customer.salesrep # laData[17],SalesRep.Comm,CUSTOMER.Comm)) TO laData[18],lnSouComm1
    STORE IIF(EMPTY(laData[19]),0,IIF(Customer.Rep2 # laData[19],SalesRep.Comm,CUSTOMER.Comm2)) TO laData[20],lnSouComm2
  ENDIF
  *B604091,1 Update commission based on Salesrep commission base setup. [End] 

  *B606206,1 TMI [Start] 
  **C200340,1 ABD Add new Custom trigger for customer stuncroft. [Begin]
  *IF ASCAN(laEvntTrig , PADR('ASLSREP1',10)) <> 0
  *  =gfDoTriger('ARDINV',PADR('EMPT17',10))  
  *  =gfDoTriger('ARDINV',PADR('ASLSREP1',10))
  * 
  *ENDIF
  *
  *IF ASCAN(laEvntTrig , PADR('ASLSREP2',10)) <> 0
  *  =gfDoTriger('ARDINV',PADR('EMPT19',10))  
  *  =gfDoTriger('ARDINV',PADR('ASLSREP2',10))
  *  
  *ENDIF
  **C200340,1 ABD [End]
  *B606206,1 TMI [End  ]         
  
  *B606206,1 TMI [Start] 
  IF ASCAN(laEvntTrig , PADR('VACC',10)) <> 0
    =gfDoTriger('ARDINV',PADR('EMPT17',10))
    =gfDoTriger('ARDINV',PADR('EMPT19',10))
    IF gfDoTriger('ARDINV',PADR('VACC',10))
      RETURN   
     ENDIF   
  ENDIF
  *B606206,1 TMI [End  ] 
  
  =gfRltFld(laData[9],@laTRltFld,'CTERMCODE')
  lcTEOM = ALLTRIM(lcTEOM)
  *E301217,1 AMM If no value entered, default by 20
  *Ren
  *lnEOMDay = IIF(lnEOMDay = 0,20,lnEOMDay-1)
  lnEOMDay = IIF(TYPE('lnEOMDay') <> 'N' .OR. lnEOMDay = 0,20,lnEOMDay-1)
  *E301217,1 AMM end
  lcTCod = ALLTRIM(lcTCod)
  llCod  = (lcTCod='Y')
  IF llIsEngland
  *-- due date 
    laData[7] = IIF(lcTEOM <> 'Y',laData[41]+lnTDaysDue,;
                    CTOD('01'+SUBSTR(DTOC(GOMONTH(laData[41],1)),3))-1+lnTDaysDue)
   ELSE  
   
     *-- EOM 	End of Month 		m/10/yy
     *-- if  day of invoice > End of Month date 		
     *-- m/10/yy + 2 monthes+ net due date	
     *-- if day of invoice < End of Month date 		
     *-- m/10/yy + 1 monthes+ net due date                 
    *E301217,1 AMM Calculate due to the end of month day variable which is 
    *E301217,1 AMM related field to the payment terms code. 
    *laData[7] = IIF(lcTEOM <> 'Y',laData[41] + lnTDaysDue,;
                GOMONTH(CTOD(SUBSTR(DTOC(laData[41]),1,3) +'10'+;
                SUBSTR(DTOC(laData[41]),6,5)),IIF(DAY(laData[41])>20,2,1))+lnTDaysDue)
    laData[7] = IIF(lcTEOM <> 'Y',laData[41] + lnTDaysDue,;
                GOMONTH(CTOD(SUBSTR(DTOC(laData[41]),1,3) +'10'+;
                SUBSTR(DTOC(laData[41]),6,5)),IIF(DAY(laData[41])>lnEOMDay,2,1))+lnTDaysDue)
    *E301217,1 AMM end
  ENDIF
  *--  currency 
  laData[22] = IIF(EMPTY(laData[22]),gcBaseCurr,laData[22])
  IF laData[22] = gcBaseCurr
    STORE 1 TO laData[23], laData[40]
  ELSE
   *-- Exchange Rate
    laData[23] = gfChkRate('laData[40]', laData[22], laData[41] ,;
                              .T., .F., .F., llEditExRt)
    IF laData[23] = 0
      IF llEditExRt
        *E300511,1 Message : 00262
        *E300511,1 A valid xxx to xxx exchange rate could not be found on xxx.
        *E300511,1 Button : 00000
        *E300511,1 Ok
        =gfModalGen('INM00262B00000','ALERT',ALLTRIM(laData[22])+'|'+ALLTRIM(gcBaseCurr)+'|'+DTOC(laData[41]))
      ELSE
        laData[22] = gcBaseCurr
        STORE 1 TO laData[23], laData[40]
      ENDIF
    ENDIF
  ENDIF
  *E301077,51 Inhance openning files to speed up transaction
  *SELECT CREDIT
  *=SEEK(laData[2])
  *LOCATE REST WHILE Account+Tran+DTOS(TranDate) = laData[2] FOR ;
  *cCurrCode = laData[22]
  *llActCrdt = FOUND()
  DO gfAcOpCrdt IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
                   laData[2],laData[22],'llActCrdt'
  *E301077,51 (End)
    
  SELECT (lcInvHdr)
  APPEND BLANK
  =RLOCK() 
  GATHER FROM laData FIELDS &lcScFields
 
 *B603119,1 NAD (Start) Invoice Status ='' not 'C' 
 *B603402,1 NAD (Start) Phone Number doesn't update .                                                    
 *REPLACE Status     WITH '' ,;
          DIRECT_INV WITH .T. ,;
          Cod_Flag   WITH IIF(llCod,'Y','N')
 *REPLACE Status     WITH 'C' ,;
          DIRECT_INV WITH .T. ,;
          Cod_Flag   WITH IIF(llCod,'Y','N') 
  *B603402,1 NAD(End)

  REPLACE Status     WITH '' ,;
          DIRECT_INV WITH .T. ,;
          Cod_Flag   WITH IIF(llCod,'Y','N'),;
          Phone      WITH Customer.Phone1          
  *B603119,1 NAD (End)  
      
  UNLOCK
  SHOW GET ibAccount  DISABLE
  SHOW GET laData[2]  DISABLE
  SHOW GET laData[4]  ENABLE
  SHOW GET ibStore    ENABLE
  SHOW GET laData[5]  ENABLE
  =lfRefresh(lcWinCh2)
  =gfwCodePop(@laCodes,'CTERMCODE','T')
  =gfwCodePop(@laCodes,'SHIPVIA','T')
  =gfwCodePop(@laCodes,'SPCINST','T')
  =gfwCodePop(@laCodes,'SEASON','T')
  =gfwCodePop(@laCodes,'CDIVISION','T')

  *B606120,1 NAD
  *=lfvCommision()
  IF ASCAN(laEvntTrig , PADR('ASLSREP1',10)) = 0
    =lfvCommision() 
  ENDIF  
  *B606120,1 NAD
  SELECT 'UNCMSESS'
  IF SEEK('I') && if found repalce it 
    BLANK
  ELSE
    APPEND BLANK
  ENDIF
  REPLACE Status     WITH 'O'       ,;
          cUTranType WITH lcProgID  ,;
          cUserId    WITH gcUser_id ,;
          cSession   WITH lcSession ,;
          cProgram   WITH 'ARDINV'  ,;
          cCurrScr   WITH 'ARDINV'  ,;
          dTranDate  WITH gdSysDate ,;
          cTranTime  WITH TIME()
  =RLOCK()
  *E301077,51 Inhance openning files to speed up transaction
  *lcFiles = 'lcInvHdr,' +lcInvHdr +','+lcInvHdr+';'+;
            'lcInvLine,'+lcInvLine+',InvLine;'     +;
            'lcInstHdr,' +lcInstHdr+','+lcInstHdr+';'+;
            'lcInstLin,' +lcInstLin+','+lcInstLin+';'+;
            'lcAppCrdt,' +lcAppCrdt+','+lcAppCrdt+';'+;
            IIF(llIsEngland,'lcEngChrg,'+lcEngChrg+','+lcEngChrg+';','')+;
            IIF(laSetups[15,2]='Y','lcUpsBox,'+lcUpsBox+','+lcUpsBox+';','')
            
  *B603119,1 NAD (Start) Add the ups box temp file to the uncomplete session if it was used  
  *lcFiles = 'lcInvHdr,' +lcInvHdr +','+lcInvHdr+';'+;
            'lcInvLine,'+lcInvLine+',InvLine;'
  lcFiles = 'lcInvHdr,' +lcInvHdr +','+lcInvHdr+';'+;
            'lcInvLine,'+lcInvLine+',InvLine;'+IIF(USED(lcupsbox),'lcUpsBox,'+lcUpsBox+','+ lcUpsBox+';','')
  *B603119,1 NAD (End)  
  *E301077,51 (End)
  =gfSavSess(lcProgID, lcFiles, @laVars,lcSession)
  llCUpDate = .T.
  =lfActFolder()
  
  *B607038,1 ABD - Remark the Next line and Valid upon the seting fields. [Begin]
  *IF (Customer.Age60 <> 0 .OR. Customer.Age90 <> 0;
  *  .OR. Customer.Age120 <> 0 ) .AND. laSetups[18,2]='Y'
  *B126004,1 ASH 01/31/2005 (Begin) Fix bug of displaying account summary screen in all cases.
  *IF laSetups[18,2]='Y' .AND. &lcCrLm_Exp
  IF laSetups[18,2]='Y' 
    IF &lcCrLm_Exp
  *B126004,1 ASH 01/31/2005 (End)
    *B607038,1 ABD - [End]
      ?? CHR(7)
      ?? CHR(7)
      ?? CHR(7)
      DO (gcScrDir+gcWinAppl+"\ARAGING.SPX") WITH laData[8]
    ENDIF
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvCustPo
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Customer PO#
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvCustPo()
*!*************************************************************
FUNCTION lfvCustPo
PRIVATE llCustPo,lnAlias

IF laScrMode[4]
  IF &lcInvHdr..CustPo <> laData[4]
    IF !EMPTY(laData[4]))
      SET ORDER TO TAG OrdCust IN OrdHdr
      llCustPo =SEEK(laData[2]+UPPER(laData[4])+'O','OrdHdr')
      SET ORDER TO TAG OrdHdr IN OrdHdr
      *E300408,1 Message : 40021
      *E300408,1 Cust. PO# xxx has been entered before.
      *E300408,1 Button : 40004
      *E300408,1 Proceed;Reenter
      IF llCustPo .AND. gfModalGen('QRM40021B40004','ALERT',ALLTRIM(laData[4]))=2
        _CUROBJ = _CUROBJ
        RETURN
      ENDIF
    ENDIF
    lnAlias = SELECT()
    SELECT (lcInvHdr)
    =RLOCK()
    REPLACE CustPo WITH laData[4]
    UNLOCK
    SELECT (lnAlias)
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvStore
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Invoice store
*!*************************************************************
*! Calls     : CUSBROW,gfGetAdr,gfChkRate,gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvStore()
*!*************************************************************
FUNCTION lfvStore
PRIVATE xStore

IF llBrowse .OR. (!EMPTY(laData[5]) .AND. ;
                  !SEEK('S'+laData[2]+laData[5],'CUSTOMER'))
  xStore = laData[5]
  SELECT CUSTOMER
  *B603727,1 NAD 09/17/2000 (Start) check if the user selects a store or selects cancel. 
  *=CUSBROWS(laData[2],.T.)
  *laData[5] = xStore
  *IF EMPTY(laData[5])
  *  llBrowse = .F.
  *  RETURN
  *ENDIF
  IF !CUSBROWS(laData[2],.T.)
    llBrowse = .F.
    IF laScrMode[1]
      STORE lcOldStore TO laData[5]
    ELSE
      SELECT (lcInvHdr)
      STORE Store TO laData[5]
    ENDIF
    RETURN
  ENDIF
  laData[5] = xStore
  *B603727,1 NAD (End)
ENDIF  
llBrowse = .F.
IF !EMPTY(laData[5]) .AND. Customer.Status <> 'A' 
  *E301466,1 AMH Allow Creat invoice for Potential Customer [Start]
  *E300408,1 Message : 40022
  *E300408,1 This a non-active account. Invoice is not allowed!
  *E300408,1 Button : 00000
  *E300408,1 Ok
  *=gfModalGen('INM40022B00000','ALERT','store')
  IF Customer.Status <> 'P'
    *E301466,1 Message : 40022
    *E301466,1 This a ð account. Invoice is not allowed!
    *E301466,1 Button : 00000
    *E301466,1 Ok
    lcMessage = IIF(Customer.Status = 'H','Hold','Canceled') + ' store'
    =gfModalGen('INM40022B00000','ALERT',lcMessage)
    STORE &lcInvHdr..Store TO laData[5]
    RETURN
  ELSE
    *E301466,1 Message : 40173
    *E301466,1 Tetxt   : You are about to save the invoice while the ð is Potential.
    *E301466,1 Tetxt   : Continuing with change to Active ?
    *E301466,1 Button  : 40000
    *E301466,1         : Yes ; No
    IF gfModalGen("QRM40173B40000","DIALOG",'store')=2
      STORE &lcInvHdr..Store TO laData[5]
      _CUROBJ = _CUROBJ
      RETURN
    ELSE
      lnAlias = SELECT(0)
      IF SEEK('S'+laData[2]+laData[5],'Customer') .AND. Customer.Status = 'P' 
        SELECT Customer
        IF gfObj_Lock(.T.)
          REPLACE Customer.Status WITH "A"
          =gfObj_Lock(.F.)
        ELSE
          STORE &lcInvHdr..Store TO laData[5]
          _CUROBJ = _CUROBJ
          RETURN
        ENDIF
      ENDIF  
      SELECT (lnAlias)
    ENDIF
  ENDIF
  *E301466,1 AMH Allow Creat invoice for Potential Customer [End  ]
ENDIF
IF laScrMode[1]
  IF !EMPTY(laData[5]) .OR. !EMPTY(laData[4])
    llBrowse = .T.
    _CUROBJ = OBJNUM(laData[1])
    KEYBOARD "{ENTER}"
  ENDIF  
  RETURN
ENDIF
*C101796,1 (Start)   Not to Overwrite invoice header with customer defaults in edit mode  
IF laScrMode[4]  
  *C101796,1 (End)
  IF laData[5] <> &lcInvHdr..Store
    *E300408,1 Message : 40024
    *E300408,1 Overwrite invoice header with customer defaults?
    *E300408,1 Button : 40000
    *E300408,1 Yes;No
    IF EMPTY(laData[5]) .AND. ;
      gfModalGen('QRM40024B40000','ALERT','customer')=1
      =SEEK('M'+laData[2],'CUSTOMER')
      *-- Overwrite invoice header with customer defaults
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
    
      STORE Customer.SalesRep  TO laData[17]
      STORE Customer.Rep2      TO laData[19] 
      *B606206,1 TMI [Start] 
      IF ASCAN(laEvntTrig , PADR('VACC',10) ) = 0
      *B606206,1 TMI [End  ] 
        =lfvRep1() .AND. lfvRep2()
      *B606206,1 TMI [Start] 
      ENDIF
      *B606206,1 TMI [End  ] 
      *-- sales rep commission on customer or on devision
      STORE IIF(laSetups[14,2]='D' .AND. SEEK(laData[17]+laData[13],'REP_DIV'),;
            REP_DIV.Comm_Rate,Customer.Comm) TO laData[18],lnSouComm1
      *B802176,1 Start.
      *STORE IIF(laSetups[14,2]='D' .AND. SEEK(laData[19]+laData[13],'REP_DIV'),;
            REP_DIV.Comm_Rate,Customer.Comm) TO laData[20],lnSouComm2
      STORE IIF(laSetups[14,2]='D' .AND. SEEK(laData[19]+laData[13],'REP_DIV'),;
            REP_DIV.Comm_Rate,Customer.Comm2) TO laData[20],lnSouComm2
      *B802176,1 End.

      *B606206,1 TMI [Start] Load sales reps for the selected customer/store
      IF ASCAN(laEvntTrig , PADR('VACC',10)) <> 0
        =gfDoTriger('ARDINV',PADR('EMPT17',10))
        =gfDoTriger('ARDINV',PADR('EMPT19',10))
        IF gfDoTriger('ARDINV',PADR('VACC',10))
          RETURN   
         ENDIF   
      ENDIF
      *B606206,1 TMI [End  ] Load sales reps for the selected customer/store

      STORE IIF(laSetups[9,2] <> 'Y',0,;
                IIF(llIsCanada,laSetups[10,2],Customer.nTaxRate)) TO laData[46]
      STORE IIF(laSetups[9,2]='Y' .AND. llIsCanada,Customer.nTaxRate,0) TO laData[45]
      STORE IIF(laSetups[9,2]='Y' .AND. llIsCanada,Customer.cTaxRule,SPACE(2)) TO laData[44]
      STORE IIF(EMPTY(laData[44]),' 1',laData[44]) TO laData[44]
      STORE IIF(llIsCanada,VAL(laData[44]),1) TO lnPstRule
      STORE .T. TO llReMerTax
      SELECT (lcInvHdr)
      =RLOCK()
      *B603402,1 NAD (Start) Phone Number doesn't update .       
      *REPLACE Rep1  WITH laData[17] ,;
              Comm1 WITH laData[18] ,;
              Rep2  WITH laData[19] ,;            
              Comm2 WITH laData[20] ,;
              lCompUps WITH .T.
            
      REPLACE Rep1  WITH laData[17] ,;
              Comm1 WITH laData[18] ,;
              Rep2  WITH laData[19] ,;            
              Comm2 WITH laData[20] ,;
              lCompUps WITH .T.,;       
              Phone WITH Customer.Phone1
      *B603402,1 NAD (End)   
      UNLOCK
      lnShipAddr = 1
      =lfRefresh(lcWinCh2)
      SHOW GETS WINDOW (lcWinCh2) ONLY
      =lfvCommision()
    ENDIF  
  
    *E300408,1 Message : 40024
    *E300408,1 Overwrite invoice header with xxx defaults?
    *E300408,1 Button : 40000
    *E300408,1 Yes;No
    IF !EMPTY(laData[5]) .AND. ;
      gfModalGen('QRM40024B40000','ALERT','store')=1
      =SEEK('S'+laData[2]+laData[5],'CUSTOMER')
      STORE '' TO lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5,lcBillName,;
                  lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,lcShipName
      lcBilLName = Customer.BtName
      =gfGetAdr('Customer','','','',1,'2')
      FOR lnCount = 1 TO ALEN(laAddress,1)
        lcCount = STR(laAddress[lnCount,1],1)
        lcBillAdd&lcCount = lcBillAdd&lcCount + IIF(EMPTY(lcBillAdd&lcCount),'',',')+;
        SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
      ENDFOR  
    
      *B802241 (Start)
      *lcShipName = IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba)
      *=gfGetAdr('CUSTOMER','','','',1,'')
      *FOR lnCount = 1 TO ALEN(laAddress,1)
      *  lcCount = STR(laAddress[lnCount,1],1)
      *  lcShipAdd&lcCount = lcShipAdd&lcCount + IIF(EMPTY(lcShipAdd&lcCount),'',',')+;
      *  SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
      *ENDFOR  
      *B802241 (End)
    
      STORE Customer.SalesRep  TO laData[17]
      STORE Customer.Rep2      TO laData[19] 

      *B606206,1 TMI [Start] Call this in case other than ENG07,since it calls ASLSREP1,and it is not needed here
      IF ASCAN(laEvntTrig , PADR('VACC',10)) = 0
      *B606206,1 TMI [End  ]       
        =lfvRep1() .AND. lfvRep2()        
      *B606206,1 TMI [Start] 
      ENDIF
      *B606206,1 TMI [End  ] 

      *C200340,1 ABD - Call this 2 funtion once at a time not 2 times.  [Begin]
      *=lfvRep1() .AND. lfvRep2()
      *C200340,1 ABD -  [End]
      
      
      STORE IIF(laSetups[14,2]='D' .AND. SEEK(laData[17]+laData[13],'REP_DIV'),;
            REP_DIV.Comm_Rate,Customer.Comm) TO laData[18],lnSouComm1
      *B802176,1 Start.
      *STORE IIF(laSetups[14,2]='D' .AND. SEEK(laData[19]+laData[13],'REP_DIV'),;
            REP_DIV.Comm_Rate,Customer.Comm2) TO laData[20],lnSouComm2
      STORE IIF(laSetups[14,2]='D' .AND. SEEK(laData[19]+laData[13],'REP_DIV'),;
            REP_DIV.Comm_Rate,Customer.Comm2) TO laData[20],lnSouComm2
      *B802176,1 End.
      
      *B606206,1 TMI [Start] Load sales reps for the selected customer/store
      IF ASCAN(laEvntTrig , PADR('VACC',10)) <> 0
        =gfDoTriger('ARDINV',PADR('EMPT17',10))
        =gfDoTriger('ARDINV',PADR('EMPT19',10))
        IF gfDoTriger('ARDINV',PADR('VACC',10))
          RETURN   
         ENDIF   
      ENDIF
      *B606206,1 TMI [End  ] Load sales reps for the selected customer/store
    
      STORE IIF(laSetups[9,2] <> 'Y',0,;
                IIF(llIsCanada,laSetups[10,2],Customer.nTaxRate)) TO laData[46]
      STORE IIF(laSetups[9,2]='Y' .AND. llIsCanada,Customer.nTaxRate,0) TO laData[45]
      STORE IIF(laSetups[9,2]='Y' .AND. llIsCanada,Customer.cTaxRule,SPACE(2)) TO laData[44]
      STORE IIF(EMPTY(laData[44]),' 1',laData[44]) TO laData[44]
      STORE IIF(llIsCanada,VAL(laData[44]),1) TO lnPstRule
      STORE .T. TO llReMerTax
      SELECT (lcInvHdr)
      =RLOCK()
      *B603402,1 NAD (Start) Phone Number doesn't update .      
      *REPLACE Rep1  WITH laData[17] ,;
              Comm1 WITH laData[18] ,;
              Rep2  WITH laData[19] ,;            
              Comm2 WITH laData[20] ,;
              lCompUps WITH .T.
            
       REPLACE Rep1  WITH laData[17] ,;
              Comm1 WITH laData[18] ,;
              Rep2  WITH laData[19] ,;            
              Comm2 WITH laData[20] ,;
              lCompUps WITH .T.,;       
              Phone WITH Customer.Phone1
      *B603402,1 NAD (End)             
            
            
      UNLOCK
      lnShipAddr = 1
      =lfRefresh(lcWinCh2)
      SHOW GETS WINDOW (lcWinCh2) ONLY
      =lfvCommision()
    ENDIF
    *B802241 (Start)
    IF !EMPTY(laData[5])
      PRIVATE lcDistCntr,lnCusRecNo
      STORE '' TO lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,lcShipName
      =SEEK('S'+laData[2]+laData[5],'CUSTOMER')
      lnCusRecNo = RECNO('Customer')    
      IF !EMPTY(Customer.Dist_Ctr)
        lcDistCntr = Customer.Dist_Ctr
        =SEEK('S'+laData[2]+lcDistCntr,'CUSTOMER')
      ENDIF
      lcShipName = IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba)
      =gfGetAdr('CUSTOMER','','','',1,'')
      FOR lnCount = 1 TO ALEN(laAddress,1)
        lcCount = STR(laAddress[lnCount,1],1)
        lcShipAdd&lcCount = lcShipAdd&lcCount + IIF(EMPTY(lcShipAdd&lcCount),'',',')+;
        SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
      ENDFOR
      =lfRefresh(lcWinCh2)
      SHOW GETS WINDOW (lcWinCh2) ONLY
      IF RECCOUNT('Customer') >= lnCusRecNo
        GO lnCusRecNo IN Customer
      ENDIF
    ENDIF
    *B802241 (End)

    SELECT (lcInvHdr)
    =RLOCK()
    REPLACE Store WITH laData[5]
    UNLOCK
    *B603727,1 NAD (Start) if the user changes the store change it in the lines too.
    PRIVATE lcOrder
    IF SEEK(laData[2]+laData[3]+lcOldStore,lcInvLine)
      WAIT 'Update invoice lines...' WINDOW NOWAIT
      SELECT (lcInvLine)
      lnCurrRec =RECNO(lcInvLine)
      lcOrder=ORDER()
      SET ORDER TO
      SCAN REST WHILE Account+Order+Store+Piktkt = laData[2]
        =RLOCK()
        REPLACE Store WITH laData[5] 
        UNLOCK      
      ENDSCAN
      SET ORDER TO &lcOrder
      IF BETWEEN(lnCurrRec,1,RECCOUNT(lcInvLine))
        GOTO lnCurrRec
      ENDIF
      WAIT CLEAR 
    ENDIF
  *B603727,1 NAD (End)   
  ENDIF
*C101796,1 (Start)  Custom procces for GMA to get the ship address in edit mode 
ELSE
 =gfDoTriger('ARDINV','STOREADD')
ENDIF
*C101796,1 (End)

*!*************************************************************
*! Name      : lfvShipAddr
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate shipping address
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvShipAddr()
*!*************************************************************
FUNCTION lfvShipAddr

IF lnShipAddr = 1
  IF EMPTY(laData[5])
    lcShipName = lcDShpName
    lcShipAdd1 = lcDShpAdd1
    lcShipAdd2 = lcDShpAdd2
    lcShipAdd3 = lcDShpAdd3
    lcShipAdd4 = lcDShpAdd4
    lcShipAdd5 = lcDShpAdd5
  ELSE
    =SEEK('S'+laData[2]+laData[5],'Customer')
    STORE '' TO lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5
    lcShipName = IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba)
    =gfGetAdr('CUSTOMER','','','',1,'')
    FOR lnCount = 1 TO ALEN(laAddress,1)
      lcCount = STR(laAddress[lnCount,1],1)
      lcShipAdd&lcCount = lcShipAdd&lcCount + IIF(EMPTY(lcShipAdd&lcCount),'',',')+;
      SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
    ENDFOR  
  ENDIF
ENDIF
lcStatus = IIF(lnShipAddr=1,'DISABLE','ENABLE')
SHOW GET lcShipName &lcStatus
SHOW GET lcShipAdd1 &lcStatus
SHOW GET lcShipAdd2 &lcStatus
SHOW GET lcShipAdd3 &lcStatus
SHOW GET lcShipAdd4 &lcStatus
SHOW GET lcShipAdd5 &lcStatus

*B602465,1 Store alternative ship address
=gfSavSess(lcProgID, lcFiles, @laVars,lcSession)

*!*************************************************************
*! Name      : lfvAShpAdr
*! Developer : Wael Aly Mohamed
*! Date      : 01/17/1999
*! Purpose   : Store Alternative shipping address (B602465,1)
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvAShpAdr()
*!*************************************************************
FUNCTION lfvAShpAdr

SELECT (lcInvHdr)
=RLOCK()
REPLACE StName    WITH lcShipName ,;
        cAddress1 WITH lcShipAdd1 ,;
        cAddress2 WITH lcShipAdd2 ,;
        cAddress3 WITH lcShipAdd3 ,;
        cAddress4 WITH lcShipAdd4 ,;
        cAddress5 WITH lcShipAdd5
UNLOCK

*!*************************************************************
*! Name      : lfvCharges
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : England Charges Screen
*!*************************************************************
*! Calls     : ARChrg.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvCharges()
*!*************************************************************
FUNCTION lfvCharges
PRIVATE lnAlias

lnAlias = SELECT()
*E301077,51 Inhance openning files to speed up transaction
IF laScrMode[4] AND !USED(lcEngChrg)
  =gfOpenFile(gcDataDir+'InvChrg',gcDataDir+'InvChrg','SH')
  SELECT InvChrg
  =AFIELDS(laFileStru)
  =gfCloseFile('InvChrg')
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]
  laFileStru[lnFileStru+1,1] = 'Order'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 6
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'PikTkt'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 6
  laFileStru[lnFileStru+2,4] = 0
  *--  Create temp file for the english Charges
  =gfCrtTmp(lcEngChrg,@laFileStru,[Order+cStore+PikTkt+cchrgcode],lcEngChrg)
ENDIF
*E301077,51 (End)

DO gpCharges IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
IIF(laScrMode[4],lcEngChrg,'InvChrg'),laData[1],laData[2],laData[3],laData[5],;
laData[30],laData[29],'laData[39]','lnChrgtax'
IF laScrMode[4]
  =lfMechTax()
ENDIF
*E301077,51 Inhance openning files to speed up transaction
=IIF(laScrMode[4],lfSavTmpFiles(),.T.)
*E301077,51 (End)
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvUpsBox
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Invoice Cartons Details (UPS Box)
*!*************************************************************
*! Calls     : ARUps.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvUpsBox()
*!*************************************************************
FUNCTION lfvUpsBox
PRIVATE lnAlias
lnAlias = SELECT()

*C102239,1 (Begin) Call custom  cartons screen for BEL05.
IF !EMPTY(InvHdr.PikTkt) AND ASCAN(laEvntTrig , PADR('CARBOX',10)) <> 0
  =gfDoTriger('ARDINV',PADR('CARBOX',10))
  RETURN
ENDIF  
*C102239,1 (End)
*C102424,1 (Begin) Call custom  cartons screen for BEL05.
*B122051,1 NNA 03/14/2004 (Begin) Stop the trigger that Show the error and dealing with the standard screen
*IF !EMPTY(InvHdr.PikTkt) AND ASCAN(laEvntTrig , PADR('OGIBOX',10)) <> 0
*  =gfDoTriger('ARDINV',PADR('OGIBOX',10))
*  RETURN
*ENDIF  
*B122051,1 NNA (End)
*C102424,1 (End)

DO gpUpsBox IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
IIF(laScrMode[4],lcUpsBox,'UpsBox'),laData[1],laData[2],laData[3],laData[5],;
laData[30],llupsinsur,llCod,laData[31],laData[10],laData[21],'laData[25]',;
'laData[26]','laData[34]','laData[35]','laData[36]','laData[28]'
IF laScrMode[4]
  SELECT (lcInvHdr)
  =RLOCK()
  GATHER FROM laData FIELDS &lcScFields
  UNLOCK
  SHOW GETS WINDOW (lcWinChrg) ONLY
  =lfRefresh(lcWinChrg)
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvFreight
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Validate Freight,Cod and Insurance
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvFreight()
*!*************************************************************
FUNCTION lfvFreight

*B803571,1 NAD 10/25/2000 (Start) Not to allow charges with negative values.
IF lfNegChrg()
*B803571,1 NAD 10/25/2000 (End)
  IF laSetups[9,2]='Y'
    lnTaxAmnt = IIF(UPPER(ALLTRIM(gcContCode))='USA',lnTaxDue,laData[31])
    *B802853,1 NAD (Start) Round tax amount in the invoice 
    *laData[37] = laData[46]*(IIF(laSetups[11,2]='M',lnTaxAmnt,lnTaxAmnt+laData[36]+;
                 laData[35]+laData[34])-lnTaxAmnt*laData[32]/100)/100
               
    laData[37] = ROUND(laData[46]*(IIF(laSetups[11,2]='M',lnTaxAmnt,lnTaxAmnt+laData[36]+;
               laData[35]+laData[34])-lnTaxAmnt*laData[32]/100)/100,2)         
    *B802853,1 NAD (End)         
    laData[43] = IIF(llIsCanada,(laData[31]+laData[36]+laData[35]+laData[34]-;
                     laData[31]*laData[32]/100+;
                IIF(VAL(laData[44])=1,0,laData[37]))*laData[45]/100,0)
    *C102212,1 (Begin) Calculate HST Tax from amount after deducting discount.
    laData[52] = IIF(llIsCanada,(laData[31]+laData[36]+laData[35]+laData[34]-;
                     laData[31]*laData[32]/100+;
                 IIF(VAL(laData[44])=1,0,laData[37]))*laData[51]/100,0)
    *C102212,1 (End)
  ENDIF
  IF llCod
    *-- calculate Cash on delivery amount
    *B603989,1 NAD 10/26/2000 (Start)  If the UPS Type is other don't add the Frieght, COD charges to the COD amount.
    IF EMPTY(lcUPSType) OR lcUPSType='OTHER'

      *B604425,1 NNA 12/31/2003 Add the H.S.T Tax to the cod amount (Begin)
      *laData[28] = laData[31]+laData[33]+laData[37]+laData[43]
      laData[28] = laData[31]+laData[33]+laData[37]+laData[43]+laData[35]+laData[52]
      *B604425,1 NNA 12/31/2003 (End)      

    ELSE
    *B603989,1 NAD 10/26/2000 (End)
      laData[28] = laData[31]+laData[34]+laData[35]+laData[36]-;
                   laData[31]*laData[32]/100+laData[37]+laData[43]
                   
    *B603989,1 NAD 10/26/2000 (Start) added the end if.
    ENDIF
    *B603989,1 NAD 10/26/2000 (End)
    
    IF laSetups[15,2]='Y' .AND. SUBSTR(lcUpsType,1,5)='USUPS' .AND. laData[25]=1 ;
      .AND. SEEK(laData[3]+laData[5]+laData[30],lcUpsBox)
      SELECT (lcUpsBox)
      =RLOCK()
      REPLACE TotalCod WITH laData[28]
      UNLOCK
    ENDIF
  ENDIF

  *B603293,1 (Start)
  *laData[38] = laData[31]+laData[34]+laData[35]+laData[36]-;
               laData[31]*laData[32]/100+laData[37]+laData[43]
  laData[38] = laData[31]+laData[34]+laData[35]+laData[36]-;
               ROUND(laData[31]*laData[32]/100,2)+laData[37]+laData[43]
  *C102212,1 (Begin) Add HST Tax.
  laData[38] = laData[38] + laData[52]
  *C102212,1 (End)
  *B603293,1 (End)

  *C037814,3  TMI [Start] Update "Balanc To Pay" by the additional charge amnt for REL01
  IF ASCAN(laEvntTrig , PADR('ADCHRG',10)) <> 0
    =gfDoTriger('ARDINV',PADR('ADCHRG',10)) 
  ENDIF  
  *C037814,3  TMI [End  ] 

  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE Freight  WITH laData[34] ,;
          Insur    WITH laData[35] ,;
          Cod      WITH laData[36] ,;
          Cod_Amt  WITH laData[28] ,; 
          Tax_Amt  WITH laData[37] ,;
          nPstAmt  WITH laData[43] ,;
          TotalChg WITH laData[38]
  *C102212,1 (Begin) Update HST Tax amount.
  REPLACE nHstAmt   WITH laData[52]
  *C102212,1 (End)
  UNLOCK
  SHOW GETS WINDOW (lcWinChrg) ONLY
  =lfRefresh(lcWinChrg)
  RETURN
*B803571,1 NAD 10/25/2000 (Start) Added the end if.
ENDIF
*B803571,1 NAD 10/25/2000 (End)
*!*************************************************************
*! Name      : lfvCodAmnt
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Validate COD amount
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvCodAmnt()
*!*************************************************************
FUNCTION lfvCodAmnt
*B803571,1 NAD 10/25/2000 (Start) Not to allow charges with negative values.
IF lfNegChrg()  
*B803571,1 NAD 10/25/2000 (End)
  IF laSetups[15,2]='Y' .AND. SUBSTR(lcUpsType,1,5)='USUPS' .AND. laData[25]=1 ;
    .AND. SEEK(laData[3]+laData[5]+laData[30],lcUpsBox)
    SELECT (lcUpsBox)
    =RLOCK()
    REPLACE &lcUpsBox..TotalCod WITH laData[28]
    UNLOCK
  ENDIF
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE Cod_Amt  WITH laData[28]
  UNLOCK
  SHOW GETS WINDOW (lcWinChrg) ONLY
  =lfRefresh(lcWinChrg)
  RETURN
*B803571,1 NAD 10/25/2000 (Start) Added the end if
ENDIF
*B803571,1 NAD 10/25/2000 (End)
*!*************************************************************
*! Name      : lfvUpsChrg
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/19996
*! Purpose   : Caculate invoice charges
*!*************************************************************
*! Calls     : gpUpsChrg
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvUpsChrg()
*!*************************************************************
FUNCTION lfvUpsChrg

*C101796,1  (Start)   Not to Overwrite invoice header with customer defaults in edit mode  
IF !laScrMode[4]
  RETURN
ENDIF
*C101796,1  (End)
*B801882,1 Send charge type to be recomputed
*DO gpUpsChrg IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
   laData[2],laData[5],laData[3],ladata[30],laData[21],laData[31],llCod,;
   llupsinsur,laData[32],laData[46],laData[44],laData[45],lcUpsBox,'laData[10]',;
   'laData[28]','laData[34]','laData[35]','laData[36]','laData[25]','laData[26]',;
   'laData[37]','laData[43]','laData[38]',IIF(UPPER(ALLTRIM(gcContCode))='USA',lnTaxDue,laData[31])

lcChrgType = IIF(SYS(18)='LLUPSINSUR','I',IIF(SYS(18)='LLCOD','C','A'))
*B604425,1 NNA 12/31/2003 Return The HST Rate And The Hst Amnt. With Their Values If Zero Or other [Begin]
*DO gpUpsChrg IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
   laData[2],laData[5],laData[3],ladata[30],laData[21],laData[31],llCod,;
   llupsinsur,laData[32],laData[46],laData[44],laData[45],lcUpsBox,'laData[10]',;
   'laData[28]','laData[34]','laData[35]','laData[36]','laData[25]','laData[26]',;
   'laData[37]','laData[43]',IIF(UPPER(ALLTRIM(gcContCode))='USA',;
   lnTaxDue,laData[31]),lcChrgType

DO gpUpsChrg IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
   laData[2],laData[5],laData[3],ladata[30],laData[21],laData[31],llCod,;
   llupsinsur,laData[32],laData[46],laData[44],laData[45],lcUpsBox,'laData[10]',;
   'laData[28]','laData[34]','laData[35]','laData[36]','laData[25]','laData[26]',;
   'laData[37]','laData[43]',IIF(UPPER(ALLTRIM(gcContCode))='USA',;
   lnTaxDue,laData[31]),lcChrgType,laData[51],'laData[52]'
*B604425,1 NNA [End]

*B603293,1 (Start)
*laData[38] = laData[31]+laData[34]+laData[35]+laData[36] - ;
             ABS(laData[31]*laData[32]/100)+laData[37]+laData[43]
*C102212,1 (Begin) Calculate HST Tax and add it to the total charge.
*laData[38] = laData[31]+laData[34]+laData[35]+laData[36] - ;
             ROUND(ABS(laData[31]*laData[32]/100),2)+laData[37]+laData[43]
*laData[52] = ROUND((laData[51]*laData[31])/100,2)
*B604418,1 NNA 12/30/2003 IIF Statement to make The H.S.T Tax Field Equal Zero ;
             when ship amount become zero [Begin]  
*laData[52] = (laData[31]+laData[34]+laData[35]+laData[36]-;
              ABS(laData[31]*laData[32]/100)+;
              IIF(VAL(laData[44])=1,0,laData[37]))*laData[51]/100
laData[52] = IIF(laData[31]>0,(laData[31]+laData[34]+laData[35]+laData[36]-;
             ABS(laData[31]*laData[32]/100)+;
             IIF(VAL(laData[44])=1,0,laData[37]))*laData[51]/100,0)
*B604418,1 NNA 12/30/2003  (End)
*B604425,1 NNA 12/31/2003 Recalculate the C.O.D amount when The C.O.D  charge become .T. Or .F. (Begin)
laData[28] = IIF(llCod,laData[31]+laData[34]+laData[35]+laData[36] - ;
             ROUND(ABS(laData[31]*laData[32]/100),2)+laData[37]+laData[43]+laData[52],0)
*B604425,1 NNA 12/31/2003   (End)            

*E302472,1 MHM 11/18/2007  Multiple Taxes for Canada on Invoice [T20060709.0008] [Start]
IF llIsCanada AND LASCRMODE[4]
  llRpHSTTax = .F.
  =lfGetHST()
  IF !(laData[51] <> 0 AND laData[45] = 0)
    IF llRpHSTTax
      laData[52] = laData[43]
      laData[51] = laData[45]
      laData[37] = 0
      laData[43] = 0
      laData[45] = 0
      laData[46] = 0
    ELSE
      laData[52] = 0
    ENDIF
  ENDIF
ENDIF
*E302472,1 MHM  [End]

laData[38] = laData[31]+laData[34]+laData[35]+laData[36] - ;
             ROUND(ABS(laData[31]*laData[32]/100),2)+laData[37]+laData[43]+laData[52]

*C037814,1  TMI [Start] Update the Charge to pay field for Rel01
IF ASCAN(laEvntTrig , PADR('ADCHRG',10)) <> 0
  =gfDoTriger('ARDINV',PADR('ADCHRG',10)) 
ENDIF  
*C037814,1  TMI [End  ]              
             
*C102212,1 (End)
*B603293,1 (End)             
             
*B801882,1 (End)
*-- update the changes to the uncomplete session file
=gfSavSess(lcProgID, lcFiles, @laVars,lcSession)

*B603293,1 (Start)
*laData[33] = -1*laData[31]*laData[32]/100
*-- merchendise discount
laData[33] = -1 * ROUND(laData[31]*laData[32]/100,2)
*B603293,1 (End)

SELECT (lcInvHdr)

*B602905,1 Default No. of cartons must be 1 at least [Begin]
laData[25] = IIF(laData[25]=0,1,laData[25])
*B602905,1 Default No. of cartons must be 1 at least [End  ]

=RLOCK()
REPLACE Ship     WITH laData[42] ,;
        ShipAmt  WITH laData[31] ,;
        Discount WITH laData[33] ,;
        Freight  WITH laData[34] ,;
        Insur    WITH laData[35] ,;
        Cod      WITH laData[36] ,;
        Cod_Amt  WITH laData[28] ,; 
        Weight   WITH laData[26] ,;
        Tax_Amt  WITH laData[37] ,;
        nPstAmt  WITH laData[43] ,;
        Cartons  WITH laData[25] ,;
        TotalChg WITH laData[38] ,;
        Cod_Flag WITH IIF(llCod,'Y','N') ,;
        lKeyOff  WITH IIF(llCod,lKeyOff,.F.) ,;
        lCompUps WITH .F.
*C102212,1 (Begin) Update HST Tax amount.
REPLACE nHstAmt  WITH laData[52]
*C102212,1 (End)

UNLOCK
IF llCod
  SHOW GET laData[27] ENABLE
  SHOW GET laData[28] ENABLE
ELSE
  IF USED(lcAppCrdt) AND SEEK(laData[2],lcAppCrdt)
    SELECT (lcAppCrdt)
    DELETE ALL
  ENDIF
  STORE SPACE(6) TO laData[27]
  SHOW GET laData[27] DISABLE
  SHOW GET laData[28] DISABLE
ENDIF

IF llCod AND laData[38] > 0 AND llActCrdt
  llOpnCrdt = .T.
  SHOW GET pbCredits ENABLE
ELSE
  llOpnCrdt = .F.
  SHOW GET pbCredits DISABLE
ENDI

*B801953,1 NAD (Start)  If UPS type not equal USUPS Disable the UPSBOX Push buttom
*IF laSetups[15,2] <> 'Y' .OR. laData[25]=1   
IF laSetups[15,2] <> 'Y' .OR. laData[25]=1 OR SUBSTR(lcUpsType,1,5) <> "USUPS"
*B801953,1 NAD (End)
  SHOW GET pbUpsBox DISABLE
ELSE
  SHOW GET pbUpsBox ENABLE
ENDIF
SHOW GETS WINDOW (lcWinChrg) ONLY
=lfRefresh(lcWinChrg)

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

*B603641,1 Print invoice after enter note pad


IF laScrMode[1] AND lnLastMode = 4 AND lcIDefPrnt
  GO TOP IN (lcInvHdr)
  IF !EMPTY(&lcInvHdr..Invoice) AND SEEK(&lcInvHdr..Invoice,'INVHDR')
    SELECT INVHDR
    SCATTER FIELDS &lcScFields TO laData
    =gfCPPrint()
  ENDIF
ENDIF
*B603641,1 (End)

laCtrStat[7] = 'DISABLE'
lcBrFields   = lcBaseFild
lcFile_Ttl   = lcBaseTit

*E301077,51 Inhance openning files to speed up transaction
*B802859,1 SSH 12/14/1999 Seek Only at the first time you call this program.
*B802859,1 SSH            to prevent automatically bounces back to the
*B802859,1 SSH            original Invoice in case of call this program from 
*B802859,1 SSH            Customer > option >Invoice.
*IF TYPE('lcInvoiceNo') = 'C' .AND. SEEK(lcInvoiceNo,'INVHDR')
IF TYPE('lcInvoiceNo') = 'C' .AND. EMPTY(laData[1]) AND SEEK(lcInvoiceNo,'INVHDR')
*B802859,1 SSH(END)
  SELECT INVHDR
  *B802859,1 SSH 12/14/1999  Commented Out 
  *SET FILTER TO 
  *B802859,1 SSH(END)
  SCATTER FIELDS &lcScFields TO laData
ENDIF


*B603119,1 NAD (Start) Do not open the files here and open it in lfChkUnComS
IF (laScrMode[2] AND lnLastMode <> 2) OR (laScrMode[4] AND lnLastMode <> 4)
* =gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH') 
* =gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
* =gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH') 
* =gfOpenFile(gcDataDir+'SALESREP',gcDataDir+'SALESREP','SH')
* =gfOpenFile(gcSysHome+'SYCFACT',gcSysHome+'CFACCODE','SH')
  SELECT STYLE
  SET RELATION TO 'S'+SCALE INTO SCALE
ENDIF
*B603119,1 NAD (END)

*E301077,51 (End)
DO CASE
  CASE laScrMode[1] .AND. lnLastMode <> 1
  *-- check for uncomplete session
    IF llGoAndChk AND lfChkUnComS()
      RETURN
    ENDIF
    lnLastMode = 1
    *-- ReInitialize variables
    =lfReStart()
    SHOW GET pbInvNote DISABLE
    SHOW GET pbObjLink DISABLE
    SHOW GET pbSend    DISABLE
    *C102574,1 HBG 04/15/2002 IF Trigger to GMA reset bars in option menu [Begin]
    *B037436,1 MHM 01/20/2004 Fix bug of goto infinit loop if you open Direct Invoice[Start] 
    *IF ASCAN(laEvntTrig , PADR('DIFBARD',10)) <> 0
    *  =gfDoTriger('ARDINV',PADR('DIFBARD',10))
    *ENDIF  
    IF ASCAN(laEvntTrig , PADR('VEWBARD',10)) <> 0
      =gfDoTriger('ARDINV',PADR('VEWBARD',10))
    ENDIF  
    *B037436,1 MHM 01/20/2004 [End] 
    *C102574,1[End]
    
  CASE laScrMode[2]
    llGoAndChk = .T.  && for the uncomplete session
    lnLastMode = 2
    lcScrMode = 'V'   && Void 
    *E301386,1 (Start) Commented out 
    * SHOW GET pbEdt   DISABLE
    *E301386,1 (End)
    SHOW GET pbInvNote ENABLE
    SHOW GET pbObjLink ENABLE
    SHOW GET pbSend    ENABLE
    IF InvHdr.Status='V'
      SHOW GET pbDlt DISABLE
      *E301386,1 (Start) Disable edit button for void invoices
      SHOW GET pbEdt DISABLE
      *E301386,1 (End)
    ELSE
      SHOW GET pbDlt ENABLE
    ENDIF
    SELECT InvLine
    SET RELATION TO STYLE+laData[21]+DYELOT INTO STYDYE
    SET RELATION TO STYLE INTO STYLE ADDITIVE
    
    STORE 'INVHDR' TO laCodes[1,7],laCodes[1,8],laCodes[2,7],laCodes[2,8],;
                      laCodes[3,7],laCodes[3,8],laCodes[4,7],laCodes[4,8],;
                      laCodes[5,7],laCodes[5,8]
    STORE 'laData[1]' TO laCodes[1,9],laCodes[2,9],laCodes[3,9],laCodes[4,9],;
                         laCodes[5,9]
    *-- Restore invoice information
    =lfGetInfo()
  CASE laScrMode[4] .AND. lnLastMode <> 4
   
    lnLastMode = 4
    llGoAndChk = .T.
    IF EMPTY(ldDefInvDate)
    *-- Restore invoice defaults
      STORE '' TO lcIDefSes,lcIDefDiv
      STORE 0 TO lnSeason,lnDivision,lnWareHouse

      *E131400,1 NNA 04/12/2006 (Begin) Open Warehous file to get the default warehous as saved at ldefware field then
      *E131400,1 NNA             close file again
      lnOldAlias = SELECT(0)
      IF !USED('WAREHOUS')
        =gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
      ENDIF
      SELECT WAREHOUS
      LOCATE FOR ldefware
      IF FOUND()
        lnWareHouse = ASCAN(laWareHouses,WAREHOUS.CWARECODE)
        lnWareHouse = IIF(lnWareHouse=0,1,ASUBSCRIPT(laWareHouses,lnWareHouse,1))
      ENDIF  
      =gfCloseFile('WAREHOUS')
      SELECT(lnOldAlias)
      *E131400,1 NNA (End)
      
      IF FILE("INVDEF.MEM")
        RESTORE ADDITIVE FROM ("INVDEF.MEM")
        STORE 1 TO lnSeason,lnDivision
        =gfwCodePop(@laCodes,'CDIVISION','L')
        =gfwCodePop(@laCodes,'SEASON','L')
        lnSeason    = ASCAN(laSeasons,lcIDefSes)
        lnDivision  = ASCAN(laDivision,lcIDefDiv)
        lnWareHouse = ASCAN(laWareHouses,lcIDefWare)
        lnSeason    = IIF(lnSeason=0,1,ASUBSCRIPT(laSeasons,lnSeason,1))
        lnDivision  = IIF(lnDivision=0,1,ASUBSCRIPT(laDivision,lnDivision,1))
        lnWareHouse = IIF(lnWareHouse=0,1,ASUBSCRIPT(laWareHouses,lnWareHouse,1))
      ENDIF
      *E300889,1 (Start) Read POS warehous insted.
      lnWareHouse = IIF(llPOSSetup,lnPOSWeHs,lnWareHouse)
      *E300889,1 (End).
      IF lnSeason = 0
        lnSeason = 1
        =gfwCodePop(@laCodes,'SEASON','D')
      ENDIF
      IF lnDivision = 0
        lnDivision = 1
        =gfwCodePop(@laCodes,'CDIVISION','D')
      ENDIF
      lnWareHouse = MAX(lnWareHouse,1)
      *E300889,1 (Start) Read POS warehous insted.
      lnWareHouse = IIF(llPOSSetup,lnPOSWeHs,lnWareHouse)
      *E300889,1 (End).

      STORE gdSysDate TO ldDefInvDate,ldDefPstDate
      llSessDef = .T.
      *-- the session default screen
      DO (gcScrDir+gcWinAppl+"\ARInvDef.SPX")
      IF !llSessDef
        STORE {}  TO ldDefInvDate,ldDefPstDate
        STORE .F. TO laScrMode
        STORE .T. TO laScrMode[1]
        SHOW GETS
        RETURN
      ENDIF
      SHOW GET lnWareHouse DISABLE
    ENDIF
    *E301077,51 Inhance openning files to speed up transaction
    SELECT (lcInvLine)
    SET RELATION TO STYLE+laData[21]+DYELOT INTO STYDYE
    SET RELATION TO STYLE INTO STYLE ADDITIVE
    *E301077,51 (End)
    
    STORE lcInvHdr TO laCodes[1,7],laCodes[1,8],laCodes[2,7],laCodes[2,8],;
                      laCodes[3,7],laCodes[3,8],laCodes[4,7],laCodes[4,8],;
                      laCodes[5,7],laCodes[5,8]
    STORE 'laData[2]+laData[3]+laData[5]+laData[30]' TO ;
          laCodes[1,9],laCodes[2,9],laCodes[3,9],laCodes[4,9],laCodes[5,9]
    IF llContinue
      RETURN
    ENDIF
    =lfReStart()
    lcScrMode = 'N'
    =lfRefresh(lcWinCh2)    
    SHOW GETS WINDOW (lcWinCh32) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh34) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh35) DISABLE ONLY
    SHOW GETS WINDOW (lcWinChrg) DISABLE ONLY
    SHOW GET pbInvNote DISABLE
    SHOW GET pbObjLink DISABLE
    SHOW GET pbSend    DISABLE
    SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
    SHOW GET ibAccount ENABLE
    SHOW GET laData[2] ENABLE
    _CUROBJ = OBJNUM(laData[2])    
    *C102574,1 HBG 04/15/2002 IF Trigger to GMA reset bars in option menu [Begin]
    *B037436,1 MHM 01/20/2004 Fix bug of goto infinit loop if you open Direct Invoice[Start] 
    *IF ASCAN(laEvntTrig , PADR('DIFBARD',10)) <> 0
    *  =gfDoTriger('ARDINV',PADR('DIFBARD',10))
    *ENDIF  
    IF ASCAN(laEvntTrig , PADR('VEWBARD',10)) <> 0
      =gfDoTriger('ARDINV',PADR('VEWBARD',10))
    ENDIF  
    *B037436,1 MHM 01/20/2004 [End] 
    *C102574,1[End]

  *E301386,1 (Start) Add the edit mode for the lpshow function
  CASE  laScrMode[3].AND. lnLastMode <> 3
    lnLastMode = 3
    SHOW GETS WINDOW (lcWinCh32) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh34) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh35) DISABLE ONLY     
    SHOW GETS WINDOW (lcWinCh30) DISABLE ONLY
    SHOW GETS WINDOW (lcWinChrg) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh2)  DISABLE ONLY
    SHOW GET laData[5] DISABLE
    SHOW GET ibStore DISABLE
    DO CASE
      CASE lnActFolder = 1
        =lfEdtFld(lcEdtFld)  
         SHOW GETS WINDOW (lcWinChrg) DISABLE ONLY
      CASE lnActFolder = 2
        SHOW GET pbLineNote ENABLE
      CASE lnActFolder = 3
       IF !llIsEngland .AND. laData[25] > 1 AND lcUpsType = "USUPS"
         SHOW GET pbUpsBox ENABLE
       ELSE
         SHOW GET pbUpsBox DISABLE
       ENDIF
       IF llIsEngland
         SHOW GET pbCharges ENABLE
       ENDIF
       IF laData[38] > 0 AND llInstTerm
         SHOW GET pbInvInst ENABLE
       ELSE
         SHOW GET pbInvInst DISABLE
       ENDIF
       =lfEdtFld(lcEdtFld)
       SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY 
       ENDCASE
    *E301386,1 (End)
      *C102574,1 HBG 04/15/2002 IF Trigger to GMA reset bars in option menu [Begin]
      *B037436,1 MHM 01/20/2004 Fix bug of goto infinit loop if you open Direct Invoice[Start] 
      *IF ASCAN(laEvntTrig , PADR('DIFBARD',10)) <> 0
      *  =gfDoTriger('ARDINV',PADR('DIFBARD',10))
      *ENDIF  
      IF ASCAN(laEvntTrig , PADR('VEWBARD',10)) <> 0
        =gfDoTriger('ARDINV',PADR('VEWBARD',10))
      ENDIF  
      *B037436,1 MHM 01/20/2004 [End] 
      *C102574,1[End]
     
ENDCASE  
*B602681,1 Seek the session record in the uncompeted sessions file after 
*B602681,1 return from other called screen.
=IIF(laScrMode[4],SEEK('O'+PADR(lcProgID,10)+gcUser_id+lcSession,'UNCMSESS'),.T.)
*B602681,1 (End)

*!*************************************************************
*! Name      : lfMechTax
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/19996
*! Purpose   : Compute England merchandise tax
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfMechTax()
*!*************************************************************
FUNCTION lfMechTax

=SEEK(IIF(EMPTY(laData[5]),'M'+laData[2],'S'+laData[2]+laData[5]),'CUSTOMER')
lnAlias = SELECT()
lnChrgtax  = 0
IF USED(lcEngChrg)
  SELECT (lcEngChrg)
  =SEEK(laData[3]+laData[5]+laData[30])
  SUM REST nChrgAmnt*(1-laData[29]/100)*nTaxRate/100 TO lnChrgtax  ;
  WHILE Order+Store+PikTkt+cchrgcode = laData[3]+laData[5]+laData[30]
ENDIF

*B603293,1 (Start)
*laData[33] = -1*laData[32]*laData[31]/100
laData[33] = -1 * ROUND(laData[32]*laData[31]/100,2)
*B603293,1 (End)
*B802853,1 NAD (Start) Round TAX Amount
*laData[37] = lnChrgtax + IIF(Customer.lVatExem,0,;
             lnMerchTax * (100-laData[29])/100 * (100-laData[32])/100)
             
laData[37] = ROUND(lnChrgtax + IIF(Customer.lVatExem,0,;
             lnMerchTax * (100-laData[29])/100 * (100-laData[32])/100),2)            
*B802853,1 NAD (End)             
laData[28] = IIF(llCod,laData[31]+laData[39]+laData[37]+laData[33],0)
laData[38] = laData[31]+laData[39]+laData[37]+laData[33]
SELECT (lcInvHdr)

*B602905,1 Default No. of cartons must be 1 at least [Begin]
laData[25] = IIF(laData[25]=0,1,laData[25])
*B602905,1 Default No. of cartons must be 1 at least [End  ]

=RLOCK()
REPLACE Ship      WITH laData[42] ,;
        ShipAmt   WITH laData[31] ,;
        DiscPcnt  WITH laData[32] ,;
        Discount  WITH laData[33] ,;
        Trde_Disc WITH laData[29] ,;
        Cod_Amt   WITH laData[28] ,; 
        Weight    WITH laData[26] ,;
        Tax_Amt   WITH laData[37] ,;
        Cartons   WITH laData[25] ,;
        TotalChg  WITH laData[38] ,;
        nCharges  WITH laData[39] ,;
        Cod_Flag  WITH IIF(llCod,'Y','N') ,;
        lKeyOff  WITH IIF(llCod,lKeyOff,.F.)
UNLOCK
STORE .T. TO llCUpdate
STORE .F. TO llReMerTax
SELECT (lnAlias)
=lfActFolder()

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

=gfwCodePop(@laCodes,'CTERMCODE','N')
=gfwCodePop(@laCodes,'SHIPVIA','N')
=gfwCodePop(@laCodes,'SPCINST','N')
=gfwCodePop(@laCodes,'SEASON','N')
=gfwCodePop(@laCodes,'CDIVISION','N')
STORE ''  TO lcInvStat,lcFiles,lcScrMode,lcCurrInv,lcUpsType
STORE 0 TO lnCartons,lnLines ,lnSouComm1,lnSouComm2,laQtyPaste,lnTaxDue
STORE 1 TO lnShipAddr,lnPstRule
STORE lcEmptySty TO lcLastStyle
STORE '' TO  lcDShpName,lcDShpAdd1,lcDShpAdd2,lcDShpAdd3,lcDShpAdd4,lcDShpAdd5,;
             lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,;
             lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5,;
             lcDBllName,lcDBllAdd1,lcDBllAdd2,lcDBllAdd3,lcDBllAdd4,lcDBllAdd5
STORE .F. TO llZoom,llupsinsur,llCod
STORE .T. TO llReBrowse,llAddLine,llNewLine
lcStatColor = gcObjColor
lcCrdtColor = gcObjColor
lcUpsTrack = 'COD Tracking#'
lcSize1 = 'Size1'
lcSize2 = 'Size2'
lcSize3 = 'Size3'
lcSize4 = 'Size4'
lcSize5 = 'Size5'
lcSize6 = 'Size6'
lcSize7 = 'Size7'
lcSize8 = 'Size8'
IF lnactfolder <> 1
  lnlastfold = lnactfolder
  lnactfolder=1
  =lfchngfolder(lnactfolder)
ENDIF
*E301077,51 Inhance openning files to speed up transaction
IF USED(lcAppCrdt)
  SELECT (lcAppCrdt)
  DELETE ALL
ENDIF
IF USED(lcInstHdr)
  SELECT (lcInstHdr)
  DELETE ALL
ENDIF
IF USED(lcInstLin)
  SELECT (lcInstLin)
  DELETE ALL
ENDIF
IF USED(lcEngChrg)
  SELECT (lcEngChrg)
  DELETE ALL
ENDIF
*E301077,51 (End)

SELECT INVHDR
SCATTER FIELDS &lcScFields TO laData BLANK
*B603064,1 Clear taxes after every invoice
lnMerchTax = 0
*B603064,1 end

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

*B603119,1 NAD (Start)  open the files 
IF llOpnFiles=.T.
  =gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH') 
  =gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
  =gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH') 
  =gfOpenFile(gcDataDir+'SALESREP',gcDataDir+'SALESREP','SH')
  =gfOpenFile(gcSysHome+'SYCFACT',gcSysHome+'CFACCODE','SH')
  llOpnFiles=.F.
ENDIF 
*B603119,1 NAD (END)


IF (llFrmSetup AND llChkUnCom) OR !llFrmSetup
  llGoAndChk = IIF(llFrmSetup, .F., llGoAndChk)
  llChkUnCom = .F.
  
  IF gfUnCompSession(lcProgID, lnSessNo, "Direct Invoice")
    STORE .F. TO laScrMode
    DO CASE
      CASE lcScrMode = 'V'       && case Void 
        STORE .T. TO laScrMode[2]
      CASE lcScrMode = 'N'
        STORE .T. TO laScrMode[4]  && case New
    ENDCASE
   
    SELECT IIF(lcScrMode='V','INVHDR',lcInvHdr)
    IF lcScrMode='V'
      =SEEK(lcCurrInv)
    ELSE
      GO TOP
    ENDIF
    SCATTER FIELDS &lcScFields TO laData
    SELECT IIF(lcScrMode='V','INVLINE',lcInvLine)
    SET ORDER TO TAG INVLINE
    SET RELATION TO STYLE+laData[21]+DYELOT INTO STYDYE
    SET RELATION TO STYLE INTO STYLE ADDITIVE
    
    IF lcScrMode='V'
      =SEEK(lcCurrInv)
    ELSE
      GO TOP
    ENDIF
    SCATTER MEMVAR MEMO
    STORE .T. TO llContinue,llCUpDate
    STORE IIF(lcScrMode='V','INVHDR',lcInvHdr) TO ;
          laCodes[1,7],laCodes[1,8],laCodes[2,7],laCodes[2,8],laCodes[3,7],;
          laCodes[3,8],laCodes[4,7],laCodes[4,8],laCodes[5,7],laCodes[5,8]
    STORE IIF(lcScrMode='V','laData[1]','laData[2]+laData[3]+laData[5]+laData[30]') TO ;
          laCodes[1,9],laCodes[2,9],laCodes[3,9],laCodes[4,9],laCodes[5,9]
    lcSession = UNCMSESS.cSession

    *B603119,1 NAD (Start) Restore the last uncomplete Session Temp Files
    lcFiles = ALLTRIM(UnCmSess.Mtmpfiles)
    *B603119,1 NAD (End)

    =lfGetInfo()
    lcObject=ALLTRIM(UnCmSess.cCurrObj)
    IF !EMPTY(lcObject)
      IF llFrmSetup AND (UPPER(lcObject)="PBSAV" .OR. UPPER(lcObject)="PBDLT")
        =IIF(UPPER(lcObject)="PBSAV",lpSavScr(),lpDelScr())
        =lfCratTemp()
        laScrMode    = .F.
        laScrMode[1] = .T.
      ELSE
        _CUROBJ = OBJNUM(&lcObject)
        KEYBOARD "{ENTER}" PLAIN CLEAR
      ENDIF
      SHOW GETS
    ENDIF
  ELSE
    =lfCratTemp()
  ENDIF
ENDIF

=lfBrowDet()

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

SELECT INVHDR
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+12,4]
laFileStru[lnFileStru+1,1] = 'nSteps'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'lUpsIns'
laFileStru[lnFileStru+2,2] = 'L'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0
laFileStru[lnFileStru+3,1] = 'lCompUps'
laFileStru[lnFileStru+3,2] = 'L'
laFileStru[lnFileStru+3,3] = 1
laFileStru[lnFileStru+3,4] = 0
laFileStru[lnFileStru+4,1] = 'LastLine'
laFileStru[lnFileStru+4,2] = 'N'
laFileStru[lnFileStru+4,3] = 6
laFileStru[lnFileStru+4,4] = 0
laFileStru[lnFileStru+5,1] = 'STNAME'
laFileStru[lnFileStru+5,2] = 'C'
laFileStru[lnFileStru+5,3] = 30
laFileStru[lnFileStru+5,4] = 0
laFileStru[lnFileStru+6,1] = 'CADDRESS1'
laFileStru[lnFileStru+6,2] = 'C'
laFileStru[lnFileStru+6,3] = 30
laFileStru[lnFileStru+6,4] = 0
laFileStru[lnFileStru+7,1] = 'CADDRESS2'
laFileStru[lnFileStru+7,2] = 'C'
laFileStru[lnFileStru+7,3] = 30
laFileStru[lnFileStru+7,4] = 0
laFileStru[lnFileStru+8,1] = 'CADDRESS3'
laFileStru[lnFileStru+8,2] = 'C'
laFileStru[lnFileStru+8,3] = 30
laFileStru[lnFileStru+8,4] = 0
laFileStru[lnFileStru+9,1] = 'CADDRESS4'
laFileStru[lnFileStru+9,2] = 'C'
laFileStru[lnFileStru+9,3] = 30
laFileStru[lnFileStru+9,4] = 0
laFileStru[lnFileStru+10,1] = 'CADDRESS5'
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
=gfCrtTmp(lcInvHdr,@laFileStru,[ACCOUNT+ORDER+STORE+PIKTKT],lcInvHdr)

SELECT ORDLINE
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+5,4]
laFileStru[lnFileStru+1,1] = 'nTaxRate'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 10
laFileStru[lnFileStru+1,4] = 2
laFileStru[lnFileStru+2,1] = 'nSteps'
laFileStru[lnFileStru+2,2] = 'N'
*B606333,1 NAD 08/01/2002 Numeric overflow in case of costing method FIFO/LIFO (Start)
*laFileStru[lnFileStru+2,3] = 2
laFileStru[lnFileStru+2,3] = 6
*B606333,1 NAD 08/01/2002 Numeric overflow in case of costing method FIFO/LIFO (END)
laFileStru[lnFileStru+2,4] = 0
laFileStru[lnFileStru+3,1] = 'LBACKORD'
laFileStru[lnFileStru+3,2] = 'L'
laFileStru[lnFileStru+3,3] = 0
laFileStru[lnFileStru+3,4] = 0
laFileStru[lnFileStru+4,1] = 'CONSOL'
laFileStru[lnFileStru+4,2] = 'C'
laFileStru[lnFileStru+4,3] = 1
laFileStru[lnFileStru+4,4] = 0
laFileStru[lnFileStru+5,1] = 'LTAXABLE'
laFileStru[lnFileStru+5,2] = 'L'
laFileStru[lnFileStru+5,3] = 0
laFileStru[lnFileStru+5,4] = 0
*C102574,1 HBG 04/15/2002 If GMA Trigger add range field to temp INVLINE [Begin]
IF ASCAN(laEvntTrig , PADR('ADDFLD',10)) <> 0
  =gfDoTriger('ARDINV',PADR('ADDFLD',10))
ENDIF
*C102574,1 [End]
*C037816,1 MHM 04/06/2004 Define New Variables[Start]
IF ASCAN(laEvntTrig , PADR('DLINTVAR',10)) <> 0
  =gfDoTriger('ARDINV',PADR('DLINTVAR',10)) 
ENDIF  
*C037816,1 MHM [End]

DECLARE laIndex[2,2]
laIndex[1,1] = 'Account+Order+Store+PikTkt+STYLE'
laIndex[1,2] = 'InvLines'
laIndex[2,1] = 'Account+Order+Store+PikTkt+STR(LineNo,6)'
laIndex[2,2] = 'InvLine'
*C102574,1 HBG 04/15/2002 If GMA Trigger add range field to temp INVLINE [Begin]
IF ASCAN(laEvntTrig , PADR('ADDIINDX',10)) <> 0
  =gfDoTriger('ARDINV',PADR('ADDIINDX',10))
ENDIF
*C102574,1 [End]

=gfCrtTmp(lcInvLine,@laFileStru,@laIndex)

SELECT (lcInvLine)
SCATTER MEMVAR MEMO

*E301077,51 Inhance openning files to speed up transaction
*SET RELATION TO STYLE+laData[21]+DYELOT INTO STYDYE
*SET RELATION TO STYLE INTO STYLE ADDITIVE
*IF llIsEngland
*  SELECT InvChrg
*  =AFIELDS(laFileStru)
*  lnFileStru = ALEN(laFileStru,1)
*  DIMENSION laFileStru[lnFileStru+2,4]
*  laFileStru[lnFileStru+1,1] = 'Order'
*  laFileStru[lnFileStru+1,2] = 'C'
*  laFileStru[lnFileStru+1,3] = 6
*  laFileStru[lnFileStru+1,4] = 0
*  laFileStru[lnFileStru+2,1] = 'PikTkt'
*  laFileStru[lnFileStru+2,2] = 'C'
*  laFileStru[lnFileStru+2,3] = 6
*  laFileStru[lnFileStru+2,4] = 0
*  =gfCrtTmp(lcEngChrg,@laFileStru,[Order+cStore+PikTkt+cchrgcode],lcEngChrg)
*ENDIF
*=gfCrtTmp(lcInstHdr,[(Order C(6),Store C(8),PikTkt C(6),;
          cInstmType C(1), nInstmFreq N(3),nInstIAmnt N(11,2),dInstmStDt D,;
          nNoInstm N(3), cInstmRef C(30),nInstIPcnt N(6,2),nInstmAmnt N(10,2))],;
          [Order+Store+PikTkt],lcInstHdr)
*=gfCrtTmp(lcInstLin,[(Order C(6),Store C(8),PikTkt C(6),;
            cInstalNo C(3),nInstmAmnt N(10,2),DueDate D,nInstmPcnt N(6,2),cInstmNote C(30))],;
            [Order+Store+PikTkt+cInstalNo],lcInstLin)
*E301077,51 (End)
IF laSetups[15,2]='Y'
  SELECT UPSBOX
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]
  laFileStru[lnFileStru+1,1] = 'Order'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 6
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'PikTkt'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 6
  laFileStru[lnFileStru+2,4] = 0
  =gfCrtTmp(lcUpsBox,@laFileStru,[Order+Store+PikTkt+STR(CARTONS,5)],lcUpsBox)
ENDIF

*!*************************************************************
*! Name      : lfvInvInst
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Invoice Installments
*!*************************************************************
*! Calls     : gpInvInstm
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvInvInst()
*!*************************************************************
FUNCTION lfvInvInst
PRIVATE lnAlias
lnAlias = SELECT()

*E301077,51 Inhance openning files to speed up transaction
IF laScrMode[4] AND !USED(lcInstHdr)
  *-- create the instalment header temp file 
  =gfCrtTmp(lcInstHdr,[(Order C(6),Store C(8),PikTkt C(6),;
            cInstmType C(1), nInstmFreq N(3),nInstIAmnt N(11,2),dInstmStDt D,;
            nNoInstm N(3), cInstmRef C(30),nInstIPcnt N(6,2),nInstmAmnt N(10,2))],;
            [Order+Store+PikTkt],lcInstHdr)
  *-- create the instalment line temp file            
  =gfCrtTmp(lcInstLin,[(Order C(6),Store C(8),PikTkt C(6),;
              cInstalNo C(3),nInstmAmnt N(10,2),DueDate D,nInstmPcnt N(6,2),cInstmNote C(30))],;
              [Order+Store+PikTkt+cInstalNo],lcInstLin)
ENDIF
*E301077,51 (End)

*803397,1 (Start) Pass the instalment files to the function in the edit mode too.
*DO gpInvInstm IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
   laData[1],laData[3],laData[5],laData[30],laData[38],laData[41],;
   IIF(laScrMode[2],'ARINSTMH',lcInstHdr),IIF(laScrMode[2],'ARINSTMD',lcInstLin)
DO gpInvInstm IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
   laData[1],laData[3],laData[5],laData[30],laData[38],laData[41],;
   IIF(laScrMode[2] OR laScrMode[3] ,'ARINSTMH',lcInstHdr),IIF(laScrMode[2] OR laScrMode[3],'ARINSTMD',lcInstLin)
*803397,1  (End)
*E301077,51 Inhance openning files to speed up transaction
=IIF(laScrMode[4],lfSavTmpFiles(),.T.)
*E301077,51 (End)
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvCredits
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Customer Credits
*!*************************************************************
*! Calls     : gpOpnCredits
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCredits()
*!*************************************************************
FUNCTION lfvCredits
PRIVATE lnApplied,llKeyOff,lnKeyOffAmnt
*-- key off amount =initial instalment amount or balance to pay                                         
lnKeyOffAmnt = IIF(USED(lcInstHdr) AND SEEK(laData[3]+laData[5]+laData[30],lcInstHdr),;
               &lcInstHdr..nInstIAmnt,laData[38])
IF lnKeyOffAmnt = 0
  RETURN
ENDIF

*E301077,51 Inhance openning files to speed up transaction
IF !USED(lcAppCrdt)
  =gfOpenFile(gcDataDir+'ARHIST', gcDataDir+'ARHIST','SH')
  SELECT ARHIST
  =AFIELDS(laFileStru)
  =gfCloseFile('ARHIST')
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+6,4]
  laFileStru[lnFileStru+1,1] = 'Order'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 6
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'PikTkt'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 6
  laFileStru[lnFileStru+2,4] = 0
  laFileStru[lnFileStru+3,1] = 'cStore'
  laFileStru[lnFileStru+3,2] = 'C'
  laFileStru[lnFileStru+3,3] = 8
  laFileStru[lnFileStru+3,4] = 0
  laFileStru[lnFileStru+4,1] = 'nTrnNewAmn'
  laFileStru[lnFileStru+4,2] = 'N'
  laFileStru[lnFileStru+4,3] = 11
  laFileStru[lnFileStru+4,4] = 2
  laFileStru[lnFileStru+5,1] = 'cShToOpn'
  laFileStru[lnFileStru+5,2] = 'C'
  laFileStru[lnFileStru+5,3] = 1
  laFileStru[lnFileStru+5,4] = 0
  laFileStru[lnFileStru+6,1] = 'nStep'
  laFileStru[lnFileStru+6,2] = 'N'
  laFileStru[lnFileStru+6,3] = 2
  laFileStru[lnFileStru+6,4] = 0
  =gfCrtTmp(lcAppCrdt,@laFileStru,[Account+Order+cStore+PikTkt+Tran],lcAppCrdt)
ENDIF
*E301077,51 (End)

llKeyOff = &lcInvHdr..lKeyOff
*-- Apply Invoice to customer credits
DO gpOpnCredits IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
laData[2],laData[3],laData[5],laData[30],lnKeyOffAmnt,'llKeyOff',lcAppCrdt,laData[22]
SELECT (lcAppCrdt)
*-- Seek amount + order + store + pick ticket 
=SEEK(laData[2]+laData[3]+laData[5]+laData[30])
SUM REST Amount TO lnApplied WHILE ;
Account+Order+cStore+PikTkt=laData[2]+laData[3]+laData[5]+laData[30]
*-- cash on delivery amount
laData[28] = lnKeyOffAmnt - ABS(lnApplied)

*E301077,51 Inhance openning files to speed up transaction
=IIF(laScrMode[4],lfSavTmpFiles(),.T.)
*E301077,51 (End)

SELECT (lcInvHdr)
=RLOCK()
REPLACE COD_AMT WITH laData[28] ,;
        lKeyOff WITH llKeyOff
UNLOCK
SHOW GET laData[28]
=lfRefresh(lcWinChrg)

*!*************************************************************
*! Name      : lfSavTmpFiles
*! Developer : WAM
*! Date      : 02/27/1999
*! Purpose   : Save temporary files in the uncmsess file
*!*************************************************************
*! Calls     : gfSavSess
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfSavTmpFiles()
*!*************************************************************
FUNCTION lfSavTmpFiles
PRIVATE lcFiles,lnAlias

lnAlias = SELECT()
lcFiles = 'lcInvHdr,' +lcInvHdr +','+lcInvHdr+';'+;
          'lcInvLine,'+lcInvLine+',InvLine;'
IF USED(lcInstHdr) AND SEEK(laData[3]+laData[5]+laData[30],lcInstHdr)
  lcFiles = lcFiles + 'lcInstHdr,' +lcInstHdr+','+lcInstHdr+';'+;
                      'lcInstLin,' +lcInstLin+','+lcInstLin+';'
ENDIF
IF USED(lcAppCrdt) AND SEEK(laData[2]+laData[3]+laData[5]+laData[30],lcAppCrdt)
  lcFiles = lcFiles + 'lcAppCrdt,' +lcAppCrdt+','+lcAppCrdt+';'
ENDIF
IF llIsEngland AND USED(lcEngChrg) AND SEEK(laData[3]+laData[5]+laData[30],lcEngChrg)
  lcFiles = lcFiles + 'lcEngChrg,'+lcEngChrg+','+lcEngChrg+';'
ENDIF
IF laSetups[15,2]='Y' AND USED(lcUpsBox) AND SEEK(laData[3]+laData[5]+laData[30],lcUpsBox)
  lcFiles = lcFiles + 'lcUpsBox,'+lcUpsBox+','+lcUpsBox+';'
ENDIF
=gfSavSess(lcProgID, lcFiles, @laVars,lcSession)
SELECT (lnAlias)

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
lnPrice = IIF(!llMulCurr OR laData[22]=gcBaseCurr,Style.Price&lcLevel,;
                      gfStyPrice(lcStyle,lcLevel,laData[22]))
*-- if price= 0 then get the price from the user                      
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
*! Example            :  =lfCheckPri(m.Style,'A')
*!*************************************************************
FUNCTION lfCheckPri
PARAMETERS lcStyle,lcLevel
PRIVATE lcTitle,lcPrompt,lcPrompt1,lcPrompt2,lcPrompt3,lnPrice,lnprice1,;
        lnPrice2,lnPrice3

=SEEK(lcStyle,'Style')
lcTitle  = PROPER(lcStyHdr)+': '+lcStyle
lnPrice1 = IIF(!llMulCurr OR laData[22]=gcBaseCurr,Style.PriceA,;
                       gfStyPrice(lcStyle,'A',laData[22]))
lnPrice2 = IIF(!llMulCurr OR laData[22]=gcBaseCurr,Style.PriceB,;
                       gfStyPrice(lcStyle,'B',laData[22]))
lnPrice3 = IIF(!llMulCurr OR laData[22]=gcBaseCurr,Style.PriceC,;
                       gfStyPrice(lcStyle,'C',laData[22]))
lnPrice   = IIF(lnPrice1 >0,1,IIF(lnPrice2 >0,2,AT(lcLevel,'ABC')))
lcPrompt  = "Price level '"+lcLevel+"' is zero. Proceed with"
lcPrompt1 = 'Price level A, '+ALLTRIM(STR(lnPrice1,12,2))
lcPrompt2 = 'Price level B, '+ALLTRIM(STR(lnPrice2,12,2))
lcPrompt3 = 'Price level C, '+ALLTRIM(STR(lnPrice3,12,2))

DO (gcScrDir+"SOSTYPRI.SPX")
RETURN(EVAL('lnPrice'+STR(lnPrice,1)))
             
*!*************************************************************
*! Name      : lfCreatHst
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 04/22/1999
*! Purpose   : Create Temporary file.
*!*************************************************************
*! Calls     : gfCrtTmp
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  =lfCreatHst()
*!*************************************************************
*
FUNCTION lfCreatHst
SELECT ARHIST
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
*B804430,1 NAD 09/25/2001 Error (Start) "Duplicate fieldnames" while voiding invoce
*DIMENSION laFileStru[lnFileStru+7,4]
DIMENSION laFileStru[lnFileStru+5,4]
*B804430,1 NAD 09/25/2001 Error (End) 
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
*B804430,1 NAD 09/25/2001 Error (Start) "Duplicate fieldnames" while voiding invoce
*laFileStru[lnFileStru+4,1] = "cbnkcode"
*laFileStru[lnFileStru+4,2] = "C"
*laFileStru[lnFileStru+4,3] = 8
*laFileStru[lnFileStru+4,4] = 0
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
*B804430,1 NAD 09/25/2001 (END)
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
*-- end of lfCreatHst.


*!**************************************************************************
*! Name      : lfHgWUpdat
*! Developer : Sameh (SSE)
*! Date      : 06/09/1999
*! Purpose   : Update nHgWtrMark field (Customer) with NETBAL, if NETBAL is greater
*!*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************************
*! Passed Parameters  : None
*!*************************************************************************
*! Returns            : None
*!*************************************************************************
*! Example   : =lfHgWUpdat()
*!*************************************************************************
*E301245,1
FUNCTION lfHgWUpdat
=RLOCK()
REPLACE nHgWtrMark WITH IIF(NETBAL>nHgWtrMark,NETBAL,nHgWtrMark)
UNLOCK
*-- End of lfHgWUpdat.

*!*************************************************************
*
FUNCTION lfvOk
*B803642,1 09/10/2000 NAD (Start) Not to check for the posting date if the system was not 
*B803642,1                        linked to GL .
*IF ldVDate < InvHdr.dPostDate
IF laSetups[4,2]='Y' AND ldVDate < InvHdr.dPostDate
*B803642,1   NAD (End)
  llVoid = .F.
  =gfModalGen("INM40163B00000" , "DIALOG")
  ldVDate   = ldOldDate
  _CurObj   = OBJNUM(ldVDate)
ELSE
  llVoid = .T.
  CLEAR READ
ENDIF
  
*!*************************************************************
*
FUNCTION lfvCan

llVoid = .F.

*!*************************************************************
*
FUNCTION lfwVDate

ldOldDate = ldVDate

*B603395,1 [Start] added function
*!*************************************************************
*! Name      : lfRefComm
*! Developer : Sameh Al-Desouki
*! Date      : 06/02/2000
*! Purpose   : Refresh Sales Rep. header Commession For invoice
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfRefComm()
*!*************************************************************
FUNCTION lfRefComm
PRIVATE lnComm1 ,lnComm2 ,lnNetShipAmnt,lnNetAmnt, lnAlias

STORE 0 TO lnComm1 ,lnComm2
*-- Restore  current alias
lnAlias = SELECT(0)

WAIT 'Computing Sales Rep. commission...' WINDOW NOWAIT
*-- seek to Account+Order+Store+PikTkt in Invoice Header [Start]
IF SEEK(laData[2]+laData[3]+laData[5]+laData[30] ,lcInvHdr)
  = SEEK(laData[2]+laData[3]+laData[5]+laData[30] ,lcInvLine)
  SELECT (lcInvLine)
  *-- Accumulate sales Rep. commission
  SCAN REST WHILE Account+Order+Store+PikTkt = ;
                  &lcInvHdr..Account+&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt
    lnNetAmnt = &lcInvLine..TotQty*&lcInvLine..Price*(1-&lcInvHdr..DiscPcnt/100)*(1-&lcInvHdr..Trde_Disc/100)
    lnComm1   = lnComm1 + (lnNetAmnt * &lcInvLine..Comm1 / 100)
    lnComm2   = lnComm2 + (lnNetAmnt * &lcInvLine..Comm2 / 100)
  ENDSCAN
  *-- calculate net shipment amount
  lnNetShipAmnt = &lcInvHdr..ShipAmt*(1-&lcInvHdr..DiscPcnt/100)*(1-&lcInvHdr..Trde_Disc/100)
  *-- for lnNetShipAmnt <> 0 update header commission [Start]
  IF lnNetShipAmnt <> 0
    *-- header commission for Sales Rep 1 
    laData[18] = ROUND( lnComm1/lnNetShipAmnt*100 , 2)
    *-- header commission for Sales Rep 2 
    laData[20] = ROUND( lnComm2/lnNetShipAmnt*100 , 2)
  ENDIF && for lnNetShipAmnt <> 0 update header commission [End]
ENDIF && seek to Account+Order+Store+PikTkt in Invoice Header [End]

SHOW GET laData[18] DISABLE && header commission for Sales Rep 1 
SHOW GET laData[20] DISABLE && header commission for Sales Rep 2 
SELECT (lnAlias)
WAIT CLEAR
*B603395,1 [End]
*!*************************************************************
*! Name      : lfOldValue
*! Developer : Abdou ElGendi
*! Date      : 02/17/2000
*! Purpose   : Function to store old value of the current filed.
*! Reference : *B603449,1
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



*:*************************************************************
*! Name      : lfPpStat
*! Developer : Nader Anis (NAD)
*! Date      : 03/28/2000
*! Purpose   : Disable Style Prepack code and quantity if style scale does not have prepacks
*! Reference : B602760,1
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************

FUNCTION lfPpStat

IF SEEK('P'+Scale,'SCALE')
  SHOW GET m.Prepak ENABLE
  SHOW GET m.PpQty  ENABLE
  SHOW GET ibPrePak ENABLE
ELSE
  SHOW GET m.PrePak DISABLE
  SHOW GET m.PpQty  DISABLE
  SHOW GET ibPrePak DISABLE
ENDIF   

*--End of lfPpStat


*:*************************************************************
*! Name      : lfEdtFld
*! Developer : Nader Anis (NAD)
*! Date      : 03/28/2000
*! Purpose   : To handle the status of the screen fields in the edit mode
*!             in the invoice header  
*! Reference : E301386,1
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
FUNCTION lfEdtFld
PARAMETERS lcFlds

lcFlds=lcFlds+','

FOR lnloop = 1 TO OCCURS(',',lcFlds)
  lcFld=SUBSTR(lcFlds,1,AT(',',lcFlds)-1)
  lnItem=AT(lcFld,lcScFields)
  IF lnItem >0 
    lnItem=OCCURS(',',LEFT(lcScFields,LnItem))+1   
    lcScFld='laData['+ STR(lnItem,2) +']' 
    SHOW GET &lcScFld ENABLE
  ELSE
    SHOW GET &lcFld ENABLE
  ENDIF   
  lcFlds=SUBSTR(lcflds,LEN(lcFld)+2)
ENDFOR


*B803350,1 (Start) Not to edit the store field in case of conslidated invoice. (GMA only) 
=gfDoTriger('ARDINV','STOREDIS')
*B803350,1  (End)

*:*************************************************************
*! Name      : lfEdtInv
*! Developer : Nader Anis (NAD)
*! Date      : 03/28/2000
*! Purpose   : Save function for the edit mode of the invoice
*! Reference : E301386,1
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************

FUNCTION lfEdtInv
PARAMETERS lcSavFlds

lcSavFlds=lcSavFlds+',' 
=RLOCK()
FOR lnloop = 1 TO OCCURS(',',lcSavFlds)
  lcFld=SUBSTR(lcSavFlds,1,AT(',',lcSavFlds)-1)
  lnItem=AT(lcFld,lcScFields)
  IF lnItem >0 
    lnitem=OCCURS(',',LEFT(lcScFields,Lnitem))+1   
    lcScFld='laData['+ Str(lnItem,2) +']'
    REPLACE &lcFld WITH  &lcScFld
  ENDIF   
  lcSavFlds=SUBSTR(lcSavFlds,len(lcFld)+2)
ENDFOR
UNLOCK

*:*************************************************************
*! Name      : lfwStore
*! Developer : Nader Anis (NAD)
*! Date      : 09/17/2000
*! Purpose   : To save the old store for later use.
*! Reference : B603727,1 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************

FUNCTION lfwStore
lcOldStore=laData[5]


*:********************************************************************************
*! Name      : lfwCharges
*! Developer : Nader Anis (NAD)
*! Date      : 10/24/2000
*! Purpose   : When function for the charges screen fields. 
*! Reference : B803571,1 
*!*********************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : 
*!*************************************************************
FUNCTION lfwCharges

lnOldChrg=EVAL(SYS(18))

RETURN .T.


*:********************************************************************************
*! Name      : lfNegChrg
*! Developer : Nader Anis (NAD)
*! Date      : 10/24/2000
*! Purpose   : When function for the charges screen fields. 
*! Reference : B803571,1 
*!*********************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : 
*!*************************************************************
FUNCTION lfNegChrg
PRIVATE lcObject,llReturn
llReturn=.T.
lcObject=SYS(18)
*C037814,3  TMI [Start] 
IF TYPE('&lcObject') = 'N'
  *C037814,3  TMI [End  ] 
  IF &lcObject <0 
    &lcObject=lnOldChrg
    * Message : 42000
    * Negative values are not allowed.
    * Buttfon  : 40011
    * Ok
    = gfModalGen('TRM42000B40011','DIALOG')
    _CUROBJ = _CUROBJ
    llReturn=.F.
  ENDIF
*C037814,3  TMI [Start] 
ENDIF
*C037814,3  TMI [End  ]   

RETURN llReturn
*!*************************************************************
*! Name      : FUNCTION lfvHstTax
*! Developer : Adel Mohammed El Gazzar
*! Date      : 03/19/2001
*! Purpose   : Compute HST Tax Rate and Amount
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  llRate : .T. : HST Tax Rate Percent
*!                                .F. : HST Tax Rate Amount
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvHstTax(.T.)
*!*************************************************************
*C102212,1
FUNCTION lfvHstTax
PARAMETERS llPercent

IF lfNegChrg()
  IF !UPDATE()
    RETURN
  ENDIF
  *--Tax Rule.
  laData[44] = STR(lnPstRule,2)
  *--Hst Amount
  laData[52] = IIF(llPercent,(laData[31]+laData[34]+laData[35]+laData[36]-;
               laData[31]*laData[32]/100+IIF(VAL(laData[44])=1,0,laData[37]));
               *laData[51]/100,laData[52])
  laData[51] = IIF(!llPercent,laData[52]*100/(laData[31]+laData[34]+;
               laData[35]+laData[36]-laData[31]*laData[32]/100+;
               IIF(VAL(laData[44])=1,0,laData[37])),laData[51])
  IF llCod
    laData[28]= laData[28]-&lcInvHdr..nHstAmt+laData[52]
    IF laSetups[15,2]='Y' .AND. SUBSTR(lcUpsType,1,5)='USUPS' .AND. laData[25]=1 ;
      .AND. SEEK(laData[3]+laData[5]+laData[30],lcUpsBox)
      SELECT (lcUpsBox)
      =RLOCK()
      REPLACE TotalCod WITH laData[28]
      UNLOCK
    ENDIF
  ENDIF
  llCUpdate = .T.
  laData[38] = laData[38] - &lcInvHdr..nHstAmt+laData[52]
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE Cod_Amt  WITH laData[28] ,; 
          cTaxRule WITH laData[44] ,; 
          nHstRate WITH laData[51] ,;
          nHstAmt  WITH laData[52] ,;
          TotalChg WITH laData[38]
  
  UNLOCK
  SHOW GETS WINDOW (lcWinChrg) ONLY
  =lfRefresh(lcWinChrg)
ENDIF

*!**************************************************************************
*! Name      : lfUpdComm
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/01/2002
*! Purpose   : Update commission.
*!**************************************************************************
*! Reference : B604091,1
*!**************************************************************************
*! Called From : Direct Invoice screen. 
*!**************************************************************************
*! Example   : =lfUpdComm('1') OR =lfUpdComm('2')
*!**************************************************************************
*
FUNCTION lfUpdComm
PARAMETER lcRepNo
IF lcRepNo = '1'
  IF laData[18] <> &lcInvHdr..Comm1
    lnSouComm1 = laData[18]
  ENDIF  
ELSE
  IF laData[20] <> &lcInvHdr..Comm2
    lnSouComm2 = laData[20]
  ENDIF  
ENDIF
*-- End of lfUpdComm
*B607038,1 ABD - [Begin]
*:*************************************************************
*: Name      : lfGetAgeBy
*: Developer : Abdou ElGendi (ABD)
*: Date      : 01/20/2003
*: Purpose   : Get 
*: Reference : 607038
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*
FUNCTION lfGetAgeBy

*-- Case the seting screen not open after instual this seting.
IF EMPTY(lcCrLm_Day)
  *-- The Defult Setting.
  *-- Aging ar by date.
  IF laSetups[16,2] = 'D'
    lcCrLm_Exp = [Customer.Age60 <> 0 .OR. Customer.Age90 <> 0 .OR. Customer.Age120 <> 0]
  ELSE
    *-- Aging ar by terms.
    lcCrLm_Exp = [Customer.TerAge60 <> 0 .OR. Customer.TerAge90 <> 0 .OR. Customer.TerAge120 <> 0]
  ENDIF
ELSE
  DO CASE
    *-- Case 30
    CASE lcCrLm_Day = '30'
      *-- Aging ar by date.
      IF laSetups[16,2] = 'D'
        lcCrLm_Exp = [Customer.Age30 <> 0 .OR. Customer.Age60 <> 0 .OR. Customer.Age90 <> 0 .OR. Customer.Age120 <> 0]
      ELSE
        *-- Aging ar by terms.
        lcCrLm_Exp = [Customer.TerAge30 <> 0 .OR. Customer.TerAge60 <> 0 .OR. Customer.TerAge90 <> 0 .OR. Customer.TerAge120 <> 0]
      ENDIF

    *-- Case 60
    CASE lcCrLm_Day = '60'
      *-- Aging ar by date.
      IF laSetups[16,2] = 'D'
        lcCrLm_Exp = [Customer.Age60 <> 0 .OR. Customer.Age90 <> 0 .OR. Customer.Age120 <> 0]
      ELSE
        *-- Aging ar by terms.
        lcCrLm_Exp = [Customer.TerAge60 <> 0 .OR. Customer.TerAge90 <> 0 .OR. Customer.TerAge120 <> 0]
      ENDIF

    *-- Case 90
    CASE lcCrLm_Day = '90'
      *-- Aging ar by date.
      IF laSetups[16,2] = 'D'
        lcCrLm_Exp = [Customer.Age90 <> 0 .OR. Customer.Age120 <> 0]
      ELSE
        *-- Aging ar by terms.
        lcCrLm_Exp = [Customer.TerAge90 <> 0 .OR. Customer.TerAge120 <> 0]
      ENDIF

    *-- Case 120
    CASE lcCrLm_Day = '120'
      *-- Aging ar by date.
      IF laSetups[16,2] = 'D'
        lcCrLm_Exp = [Customer.Age120 <> 0]
      ELSE
        *-- Aging ar by terms.
        lcCrLm_Exp = [Customer.TerAge120 <> 0]
      ENDIF
  
  ENDCASE
ENDIF
*-- End OF lfGetAgeBy
*B607038,1 ABD - [End]

*!*************************************************************
*! Name      : lfvDueDate
*! Developer : Mohamed Shokry(MHM)
*! Date      : 01/11/2004
*! Purpose   : Validate Invoice Due date
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDueDate()
*!*************************************************************
FUNCTION lfvDueDate
*B121026,1 MHM 12/30/2003 Fix Bug of accept saving with empty Invoice Due Date. [Start]
IF EMPTY(laData[7])
  *-You cannot leave the entered date empty.
  =gfModalGen('INM40178B34000','DIALOG','Invoice Due')
  _CUROBJ = OBJNUM(laData[7])
  laData[7] = &lcInvHdr..DueDate
  RETURN
ENDIF  
*B121026,1 MHM .[End] 
*!*************************************************************
*! Name        : lfGetHST
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 11/18/2007
*! Purpose     : To get the HST from Codes file
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Refer to    : 
*!*************************************************************
*! Example     : 
*!*************************************************************
FUNCTION lfGetHST
lcAlias = ALIAS()

SELECT CODES
lcOldOrd = ORDER()
SET ORDER TO Codes
llRpHSTTax = .F.
IF SEEK("N"+ LEFT(Customer.cAddress4,6)+"YSTATE")
  SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = "N"+ LEFT(Customer.cAddress4,6)+"YSTATE"
    IF crltd_nam = "LHSTTAX   "
       llRpHSTTax = IIF(ALLTRIM(crltd_vlu) = "T",.T.,.F.)
       EXIT
    ENDIF
  ENDSCAN  
ENDIF

SELECT CODES
SET ORDER TO &lcOldOrd 

SELECT (lcAlias)

*:*************************************************************
