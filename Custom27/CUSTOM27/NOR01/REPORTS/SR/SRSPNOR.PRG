*:***************************************************************************
*: Program file  : SRSPNOR.PRG
*: Program desc. : CUSTOMEZED SHIPMENTS BY SALESREP FOR NORTH BAY APPAREL
*! Date          : 12/2/2004
*: System        : Aria Advantage Series.
*: Module        : SALES REPRESENTATIVES (SR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C124669
*: 
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO SRSPNOR
*:***************************************************************************
*:Modifications :
*:B126350,1 BWA 02/17/2005 Fix some bugs in the report.
*:B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
*:B126350,2 MMR            and have returns and make modification to include credits in the 
*:B126350,2 MMR            harge back also rase the lines in the first page.  
*:B126350,3 MMR 10/19/2005 1)Fix  bug of not SHowing chargeback if recap=yes and i have 2 divisions
*:B126350,3 MMR            2)Fixing bug of showing credit adjustments if the user enters wrong
*:B126350,3 MMR              division in the reference field.   
*:B126350,3 MMR            3)Fixing bug of repeating chargebacks for every salesrep. 
*:B126350,3 MMR            4)Fixing bug of including chargesbacks for Kmart and Wallmart in 
*:B126350,3 MMR              the total rest for account. 
*:B126350,3 MMR            5)Fix  bug of not SHowing right amounts for divisions 
*:B126350,3 MMR              in the Cover page if i have more than SALESREP   
*:B126350,3 MMR            6)Fix  bug of not SHowing Total and chargeback lines per account if
*:B126350,3 MMR              we 2 salesrep and there is one account only for the 1rst salesrep.
*:B126350,3 MMR            7)Fix  bug of not SHowing Met Sales if i choose specific Style then
*:B126350,3 MMR              preivew then Tag non and preivew again.     
*:B126350,3 MMR            8)Fix  bug of not deducting chargebacks from the special reacp for 
*:B126350,3 MMR              Kmart and Wallmart. 
*:B126350,3 MMR            9)Fix  bug of not showing the 2nd SalesRep data if exist 
*:B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
*:B126350,4 MMR            2)Fix bug of showing rest of account with wrong values. 
*:***************************************************************************
*-- llPrint         variable that is used to prevent printing if there is not
*--                 any record matches the report criteria
*-- laDvision       Array hold the divisions of the cover page. 
*-- laAllDvi        Array hold the total divisions for each salesrep. 
*-- laInvoice       Array hold the invoices that get there totalchg.
*-- laAccRepDv      Array hold the account , repcode and division for charge back.
PRIVATE llPrint , laDvision , laAllDvi , lnRtrnChBk , laRstAccCb
*B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices and
*B126350,2                have returns.
lcOldRpExp=''
lcRPCredit=''
*:B126350,2 MMR.[End]
*B126350,3 MMR 10/19/2005 3)Fixing bug of repeating chargebacks for every salesrep. 
lcSlsRep=''
*B126350,3 MMR.[End]
DIMENSION laDvision[1,4] , laInvoice[1] , laAllDvi[1,4] , laAccRepDv[1] , laRstAccCb[1,2]
STORE '' TO laDvision , laInvoice , laAllDvi , laAccRepDv , laRstAccCb

STORE 0 TO lnStyLnGl , lnStyPosGl , lnRtrnChBk
=lfChkStrct() && Get the length of the style , color and scale.

PRIVATE ldDatFrom , ldDatTo , lcFrom , lcTo
STORE SPACE(0) TO ldDatFrom , ldDatTo , lcFrom , lcTo
lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVHDR.INVDATE'),1)
IF !EMPTY(laOGFxFlt[lnDatePos,6])
  ldDatFrom = LEFT(laOGFxFlt[lnDatePos,6] , ATC( "|" , laOGFxFlt[lnDatePos,6])-1)
  ldDatTo   = SUBSTR(laOGFxFlt[lnDatePos,6] , ATC( "|" , laOGFxFlt[lnDatePos,6]) + 1)

  lcFrom    = substr(lddatfrom,7) + left(lddatfrom,2) +substr(lddatfrom,4,2) 
  lcTo      = substr(ldDatTo,7) + left(ldDatTo,2) +substr(ldDatTo,4,2) 
ENDIF

*--Let the type for customers by 'M'.
IF 'CUSTOMER.TYPE+CUSTOMER.ACCOUNT+CUSTOMER.STORE'$ lcRpExp
  lcRpExp = STRTRAN(lcRpExp,'CUSTOMER.TYPE+CUSTOMER.ACCOUNT+CUSTOMER.STORE','"M"+CUSTOMER.ACCOUNT+CUSTOMER.STORE')
ENDIF

llprint     = .F.

*-- R_WIDTH  Variable that hold the report width
*-- R_TITLE  Variable that hold the report title
*-- XREPORT  variable that hold the report name
*-- XTITLE   variable that hold the report salesrep name

R_WIDTH     = 'W'
R_TITLE     = 'SHIPMENTS BY SALESREP'
XREPORT     = 'SRSPNOR'
XTITLE      = lcRpOpMsg

*-- llStyCom   variable that hold the vlaue of "Edit comm. at line level".
llStyCom  = gfGetMemVar('M_STY_COM') = 'Y'

*-- XFILTER  variable of the option grid.
XSL  = lcRpSales
XREP = "D"       &&The user will print the detail only.

*-- THE MAIN PROGRAM 

lcCent = SET('CENT')                &&--Hold the current Centery setting.
SET CENT ON
llCustomer = .F.                    &&--Hold .T. if the user select specific Customer(s).
lcMainExp  = ''                     &&--Hold the Filter of the INVHDR.
lcWorkFile = ''                     &&--Hold the Temp. Cursor that hold the selected style/customer or hold 'INVHDR'.
lcCustFile = ''                     &&--Hold the temp cusror that hold the selectd customer(s).
lcStylFile = ''                     &&--Hold the temp cusror that hold the selectd Style(s).

*------------------> if the user didn't selelect any customer or Style
lcDivFltr  = ''                    &&--Hold the Division filter
lcSeasFltr = ''                    &&--Hold the season Filter
lcDatFltr  = ''                    &&--Hold the invdate filter

*--Call Function to collect all the selected filters.
=lfGetFltrs()

*--Save the Orginal the old name.
lcHoldName = lcInvltemp
SELECT RETHDR
SET ORDER TO RETHDRA
*--Function to collect the data.
DO lpCollData
SET CENT &lcCent


*-- PRINT THE REPORT FROM THE WORKFILE
IF llPrint
  *--Section to marge the styles.
  =lfMargData()

  *--Section collect the date of the divisions.
  =lfDvsion()

  SET DEVICE TO PRINT
  DO lpPrint
  DO ENDREPORT
  SET DEVICE TO SCREEN
ENDIF

*--Restore the old name.
lcInvltemp = lcHoldName

IF USED(lcInvltemp)
  SELECT (lcInvltemp)
  USE
ENDIF
  
IF USED(lcStyltmp)
  SELECT (lcStyltmp)
  USE
ENDIF

*-- ERASE THE lcWorkFile
ERASE (gcWorkDir+lcInvltemp+'.DBF')
ERASE (gcWorkDir+lcInvltemp+'.CDX')
ERASE (gcWorkDir+lcInvltemp+'.FPT')

ERASE (gcWorkDir+lcStyltmp+'.DBF')
ERASE (gcWorkDir+lcStyltmp+'.CDX')
ERASE (gcWorkDir+lcStyltmp+'.FPT')

               *--Section functions of the report.--*
*!*************************************************************
*! Name      : lpCollData
*! Developer : Bassem Rafaat ERNEST(BWA)
*! Date      : 10/20/2004
*! Purpose   : Print the report.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpCollData
*!*************************************************************
PROCEDURE lpCollData

SELE INVLINE

IF !EMPTY(XSL)
  lcRpExp = lcRpExp  + " .AND. (INVHDR.REP1 = '" + XSL + "' .OR. INVHDR.REP2 = '" + XSL + "')"
ENDIF                     
lcRpExp = lcRpExp + ".AND. INVHDR.STATUS <> 'V' "

*--Optimize the locate function
lcSeekExp = lcMainExp + IIF(!EMPTY(lcDatFltr)," .AND. ","") + lcDatFltr 
SELECT INVHDR
LOCATE FOR Invoice = '' AND &lcSeekExp
*B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices and
*B126350,2 MMR               have returns.
*IF EOF()
  *-TEXT (There are no records to display)
 * =gfModalGen('TRM00052B00000','DIALOG')
 * RETURN
*ELSE
*B126350,2 MMR.[End]
  *--Create the lcInvltemp file and add new fields.
  SELECT INVLINE
  =AFIELDS(laFileStru)
  
  =lfAddField("laFileStru", "REPCODE" , "C",3 ,0)
  =lfAddField("laFileStru", "cdivision" , "C",6 ,0)
  *B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
  *B126350,2 MMR            and have returns and make modification to include credits in the charge back
  =lfAddField("laFileStru", "nReturns" , "N",14 ,2)
  =lfAddField("laFileStru", "nCredit" , "N",14 ,2)
  *B126350,2 MMR.[End]
  Create Table (gcWorkDir + lcInvltemp) FROM ARRAY laFileStru
  INDEX ON CDIVISION TAG CDIVISION OF (gcWorkDir + lcInvltemp +'.CDX') ADDITIVE
  INDEX ON REPCODE + CDIVISION + ACCOUNT + INVOICE TAG (lcInvltemp) OF (gcWorkDir + lcInvltemp +'.CDX') ADDITIVE
  *B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
  *B126350,2 MMR            and have returns and make modification to include credits in the charge back
  INDEX ON  CDIVISION + ACCOUNT+REPCODE  TAG (lcInvltemp2) OF (gcWorkDir + lcInvltemp +'.CDX') ADDITIVE
  *B126350,2 MMR.[End]
  llprint = .T.

  *--- i will collect the data in 3 diffrente ways , depend on the user selections
  *--- 1- if the user select specific Customer(s) , i will scan the Temp cursor that hold all the 
  *---    Selected customer(s) , then seek in the invhdr to collect all the invoices for the specific customer
  *--- 2- if the user select specific Style(s) , i will scan the Temp cursor that hold all the 
  *---    Selected Style(s) ,then seek in the invLine to collect all the invoices for the specific Style
  *--- 3- if the user didn't select specific Style(s) or customer(s), i will scan the Invhdr then collect
  *---    the data from the invline 
  
  SELECT INVHDR
  lcOrder = ORDER()
  IF llCustomer
    SET ORDER TO TAG INVHDRA
  ENDIF
  SELECT INVLINE
  SET ORDER TO TAG IIF(llcustomer OR lcWorkFile = 'INVHDR','INVLINE','INVLINES')
  SELECT (lcWorkFile)
  IF llCustomer
    *--if the user select specific Customer
    SCAN
      WAIT 'Selecting records for report , Account # '+Account       WINDOW NOWAIT
      IF SEEK(Account,'INVHDR') 
        SELECT INVHDR
        lcSeekExp = lcMainExp + IIF(!EMPTY(lcDatFltr)," .AND. ","") + lcDatFltr 
        SCAN REST WHILE account + invoice = &lcWorkFile..Account FOR &lcSeekExp
          WAIT 'Selecting records for report , Invoice # '+INVOICE       WINDOW NOWAIT
          IF SEEK(INVOICE,'INVLINE')
            M.cdivision = INVHDR.CDIVISION
            SELECT INVLINE
            lcLineExp = IIF(EMPTY(lcSeasFltr),".T.",lcSeasFltr)
            SCAN REST WHILE invoice+STR(lineno,6) = INVHDR.INVOICE FOR &lcLineExp
              WAIT 'Selecting records for report , Style # '+Style       WINDOW NOWAIT
              lcChkExp = IIF(EMPTY(lcStylFile),".T.","SEEK(STYLE.cStyMajor,lcStylFile)")
              IF SEEK(INVLINE.Style,'STYLE') AND &lcChkExp
                =lfGetData()
              ENDIF
            ENDSCAN 
          ENDIF
        ENDSCAN
      ENDIF
    ENDSCAN
  ELSE
    IF lcWorkFile = 'INVHDR'
      *--if the user didn't select any Style(s) or  Customer(s) .

      lcSeekExp = lcMainExp + IIF(!EMPTY(lcDatFltr)," .AND. ","") + lcDatFltr 
      SCAN FOR Invoice = '' AND &lcSeekExp 
        WAIT 'Selecting records for report , Invoice # '+Invoice    WINDOW NOWAIT
        IF SEEK(INVOICE,'INVLINE')
          M.cdivision = &lcWorkFile..CDIVISION
          SELECT INVLINE
          lcLineExp = IIF(EMPTY(lcSeasFltr),".T.",lcSeasFltr)
          SCAN REST WHILE invoice+STR(lineno,6) = INVHDR.INVOICE FOR &lcLineExp
            =lfGetData()
          ENDSCAN
        ENDIF
      ENDSCAN
    ELSE
      *--if the user select specific Style(s).
      SCAN
        lcStyle = &lcWorkFile..cStyMajor
        IF SEEK(ALLTRIM(lcStyle),'INVLINE') AND SEEK(ALLTRIM(lcStyle),'STYLE') 
          SELECT INVLINE
          lcSeekExp = lcSeasFltr + IIF(!EMPTY(lcDatFltr) AND !EMPTY(lcSeasFltr)," .AND. ","") + lcDatFltr 
          lcSeekExp = IIF(EMPTY(lcSeekExp),".T.",lcSeekExp)
          SCAN REST WHILE style+invoice+STR(lineno,6) = ALLTRIM(lcStyle) FOR &lcSeekExp
            WAIT 'Selecting records for report , Invoice # '+Invoice    WINDOW NOWAIT
            IF SEEK(INVLINE.INVOICE,'INVHDR') AND &lcMainExp
              M.cdivision = INVHDR.CDIVISION
              =lfGetData()
            ENDIF
          ENDSCAN
        ENDIF
      ENDSCAN
    ENDIF
  ENDIF
  SELECT INVHDR
  SET ORDER TO &lcOrder
  SELECT INVLINE    
  SET ORDER TO INVLINES
*B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
*B126350,2 MMR            and have returns and make modification to include credits in the charge back
lnStylPos   = lfItmPos('STYLE.CSTYMAJOR')
IF lnStylPos <> 0 
  DIME laStyFlt[1,7]
  =ACOPY(laOGFxFlt,laStyFlt,((lnStylPos-1)*7)+1,7)
  =ACOPY(laBrFldFlt,laOldBr)
  laBrFldFlt[1,1] = "laStyFlt:1"
  lcRpExpSty = gfGenFlt('laStyFlt',.T.,.T.)
  lcRPExp =strtran(lcRPExp,lcRpExpSty,".T.")
ENDIF
lnSesPos  = lfItmPos('INVHDR.SEASON')
IF lnSesPos<> 0 
  DIME laSesFlt[1,7]
  =ACOPY(laOGFxFlt,laSesFlt,((lnSesPos-1)*7)+1,7)
  lcRpExpSes = gfGenFlt('laSesFlt',.T.,.T.)
  lcRPExp =strtran(lcRPExp,lcRpExpSes,".T.")
ENDIF
lcOldRpExp = lcrpexp
lnDivPos  = lfItmPos('INVHDR.CDIVISION')
IF lnDivPos<> 0 
  DIME laDivFlt[1,7]
  =ACOPY(laOGFxFlt,laDivFlt,((lnDivPos-1)*7)+1,7)
  lcRpExpDiv = gfGenFlt('laDivFlt',.T.,.T.)
  lcRPExp =strtran(lcRPExp,lcRpExpDiv,".T.")
ENDIF
IF !EMPTY(XSL)
lcRPExp =strtran(lcRPExp," .AND. (INVHDR.REP1 = '" + XSL + "' .OR. INVHDR.REP2 = '" + XSL + "')"," .AND. (REPCOMM.REPCODE = '" + XSL + "')")
ENDIF
lcRPCredit =strtran(lcRPExp,".AND. INVHDR.STATUS <> 'V' ",'')
lcRPCredit =strtran(lcRPExp,'INVHDR.INVDATE','REPCOMM.DATE')
lcRPCredit=strtran(lcRPCredit,'"M"+CUSTOMER.ACCOUNT+CUSTOMER.STORE','"M"+REPCOMM.ACCOUNT')
STORE 0 to lnTotCAmnt,lntotRAmnt,lcOldOrd
*B126350,3 MMR 10/19/2005 2)Fixing bug of showing credit adjustments if the user enters wrong
*B126350,3 MMR              division in the reference field.[Start]   
STORE SPACE(0) TO lcAliass,lcOrd
*B126350,3 MMR.[End]
SELECT REPCOMM
SCAN FOR &lcRPCredit
  SELECT CREDIT
  IF SEEK ('5'+REPCOMM.TRAN,'CREDIT') 
    LOCATE FOR IIF(!EMPTY(laOGFxFlt[2,6]),SUBSTR(CREDIT.REFERENCE,1,6) $ laOGFxFlt[2,6],.T.) AND TRANTYPE='5' AND TRAN=REPCOMM.TRAN
    IF FOUND()
      *B126350,3 MMR 10/19/2005 2)Fixing bug of showing credit adjustments if the user enters wrong
      *B126350,3 MMR              division in the reference field.[Start]   
      lcAliass=ALIAS()
      SELECT CODES
      lcOrd=ORDER()
      SET ORDER TO Ccode_no
      IF SEEK ('N'+'CDIVISION '+SUBSTR(CREDIT.REFERENCE,1,6))
      *B126350,3 MMR.[End]
        SELECT (lcInvltemp)
        lcOldOrd=ORDER()
        SET ORDER TO lcInvltemp2
        IF EMPTY(lcRpExpSty) AND EMPTY(lcRpExpSes)
          IF SEEK (SUBSTR(CREDIT.REFERENCE,1,6)+REPCOMM.ACCOUNT+REPCOMM.REPCODE)
              REPLACE nCredit WITH nCredit+CREDIT.Amount
          ELSE
            APPEND BLANK
            REPLACE ACCOUNT   WITH REPCOMM.ACCOUNT        ,;
                    CDIVISION WITH CREDIT.REFERENCE       ,; 
                    nCredit   WITH nCredit+CREDIT.Amount ,;
                    REPCODE   WITH REPCOMM.REPCODE 
          ENDIF
        ELSE
          IF SEEK (SUBSTR(CREDIT.REFERENCE,1,6)+REPCOMM.ACCOUNT+REPCOMM.REPCODE)
              REPLACE nCredit WITH nCredit+CREDIT.Amount
          ENDIF    
        ENDIF
      *B126350,3 MMR 10/19/2005 2)Fixing bug of showing credit adjustments if the user enters wrong
      *B126350,3 MMR              division in the reference field.[Start]   
      ENDIF 
      SELECT CODES
      SET ORDER TO (lcOrd)
      SELECT(lcAliass)
      *B126350,3 MMR.[End]
    ENDIF 
 ENDIF 
ENDSCAN
IF !EMPTY(XSL)
lcOldRpExp =strtran(lcOldRpExp," .AND. (INVHDR.REP1 = '" + XSL + "' .OR. INVHDR.REP2 = '" + XSL + "')"," .AND. (RETHDR.salesrep1 = '" + XSL + "' .OR. RETHDR.salesrep2 = '" + XSL + "')")
ENDIF
lcOldRpExp =strtran(lcOldRpExp,".AND. INVHDR.STATUS <> 'V' ",".AND. RETHDR.STATUS <> 'V' ")
lcOldRpExp =strtran(lcOldRpExp,'INVHDR.INVDATE','RETHDR.CRDATE')
lcOldRpExp=strtran(lcOldRpExp,'"M"+CUSTOMER.ACCOUNT+CUSTOMER.STORE','"M"+RETHDR.ACCOUNT')
lcOldRpExp =strtran(lcOldRpExp,'INVHDR.CDIVISION','RETHDR.CDIVISION')
SELECT RETHDR
SCAN FOR &lcOldRpExp
  SELECT (lcInvltemp)
  lcOldOrd=ORDER()
  SET ORDER TO lcInvltemp2
  IF !EMPTY(RETHDR.salesrep1)
    IF EMPTY(lcRpExpSty) AND EMPTY(lcRpExpSes)
      IF SEEK (RETHDR.CDIVISION+RETHDR.ACCOUNT+RETHDR.salesrep1)
            REPLACE nReturns WITH nReturns+RETHDR.Amount
      ELSE
        APPEND BLANK
        REPLACE ACCOUNT   WITH RETHDR.ACCOUNT         ,;
                CDIVISION WITH RETHDR.CDIVISION       ,; 
                nReturns  WITH nReturns+RETHDR.AMOUNT	,;  
                REPCODE   WITH RETHDR.salesrep1        
      ENDIF
    ELSE
      IF SEEK (RETHDR.CDIVISION+RETHDR.ACCOUNT+RETHDR.salesrep1)
            REPLACE nReturns WITH nReturns+RETHDR.Amount
      ENDIF      
    ENDIF  
  ENDIF  
  IF !EMPTY(RETHDR.salesrep2)
    IF EMPTY(lcRpExpSty) AND EMPTY(lcRpExpSes)
      IF SEEK (RETHDR.CDIVISION+RETHDR.ACCOUNT+RETHDR.salesrep2)
            REPLACE nReturns WITH nReturns+RETHDR.Amount
      ELSE
        APPEND BLANK
        REPLACE ACCOUNT   WITH RETHDR.ACCOUNT         ,;
                CDIVISION WITH RETHDR.CDIVISION       ,; 
                nReturns  WITH nReturns+RETHDR.AMOUNT	,;  
                REPCODE   WITH RETHDR.salesrep2          
    ENDIF
   ELSE
      IF SEEK (RETHDR.CDIVISION+RETHDR.ACCOUNT+RETHDR.salesrep2)
            REPLACE nReturns WITH nReturns+RETHDR.Amount
      ENDIF      
   ENDIF 
  ENDIF  
ENDSCAN 
*B126350,2 MMR.[End]
 SELECT (lcInvltemp)
  *B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
  *B126350,2 MMR            and have returns and make modification to include credits in the charge back
  GO TOP
  *B126350,2 MMR.[End]
  IF EOF()
    *-TEXT (There are no records to display)
    =gfModalGen('TRM00052B00000','DIALOG')
    llprint = .F.
    RETURN
  ENDIF
  SET RELATION TO INVOICE INTO INVHDR
  SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE ) ;
                      INTO CUSTOMER ADDITIVE
*B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices and
*B126350,2 MMR               have returns.
*ENDIF
*B126350,2 MMR.[End]
*--End of lpCollData.
*!*************************************************************
*! Name      : lpPrint
*! Developer : Bassem Rafaat ERNEST(BWA)
*! Date      : 10/20/2004
*! Purpose   : Print the report
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpPrint
*!*************************************************************
PROCEDURE lpPrint

ROW = 99
STORE '' TO XREPNAME , XSAVFLD , lnCalChBk , lcHldOldDv
STORE 0 TO PAGENO , XSUBPCS , XSUBEXT , lnSubComm1 , lnSubComm2 , lnCalChBk

SELECT (lcInvltemp)
LOCATE
PRIVATE llDivPrn
llDivPrn = .F.
=lfPrtCover()
llDivPrn = .T.

PRIVATE lcChngDiv , lcChngAcc , lnExtOnly , lnTotDvAmt , lnTotChrgB , lnNetSales , lnRetOnly,lcChngRep
PRIVATE lnWalmrtRp , lnWalNonRp , lnKmartVal , lnOthrAcnt

STORE ''  TO lcChngDiv , lcChngAcc,lcChngRep
STORE 0   TO lnExtOnly , lnTotDvAmt , lnTotChrgB , lnNetSales , lnRetOnly
STORE 0   TO lnWalmrtRp , lnWalNonRp , lnKmartVal , lnOthrAcnt
*B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
*B126350,2 MMR            and have returns and make modification to include credits in the charge back
STORE 0 TO lnReturn,lnCredit
*B126350,2 MMR.[End]
*--Refresh the array hold the divisions.
=lfRfrshDiv(.T.)
GO TOP
XSAVFLD  = REPCODE
=SEEK(XSAVFLD,'SALESREP')
XREPNAME = SALESREP.NAME

SCAN FOR REPCODE + CDIVISION + ACCOUNT + INVOICE = ''
  =lpPrtRpHdr()

  SELECT (lcInvltemp)
  IF REPCODE # XSAVFLD
    STORE '' TO laInvoice
    *B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
    *B126350,2 MMR            and have returns and make modification to include credits in the charge back
    *IF (lcChngDiv # cDivision OR lcChngAcc # Account) AND lnExtOnly > 0 AND lcRpRecap = "N"
    *B126350,3 MMR 10/19/2005 6)Fix  bug of not SHowing Total and chargeback lines per account if
    *B126350,3 MMR              we 2 salesrep and there is one account only for the 1rst salesrep.                            
    *IF (lcChngDiv # cDivision OR lcChngAcc # Account) AND (lnExtOnly+lnReturn+(-1*lnCredit)) > 0 AND lcRpRecap = "N"
    IF (lcChngDiv # cDivision OR lcChngAcc # Account OR lcChngRep # REPCODE) AND (lnExtOnly+lnReturn+(-1*lnCredit)) > 0 AND lcRpRecap = "N"
    *B126350,3 MMR.[End]
    *B126350,2 MMR.[End]
      IF (lcChngDiv # cDivision) AND (lcChngAcc == Account)
        lnExtOnly = IIF( lcRpDeciml = "Y" ,  PADL(ALLTRIM(STR(lnExtOnly,10,2)),10," ") , PADL(ALLTRIM(STR(lnExtOnly,10)),10," ") )
        lcChngDiv = cDivision
      ELSE
        IF (lcChngAcc # Account) AND (lcChngDiv == cDivision)
          lnExtOnly = IIF( lcRpDeciml = "Y" ,  PADL(ALLTRIM(STR(lnExtOnly,10,2)),10," ") , PADL(ALLTRIM(STR(lnExtOnly,10)),10," ") )
          *@ ROW,50  SAY " Total For Account >>  " + lcChngAcc + lnExtOnly
          *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
          *@ ROW,50  SAY " Total For Account   >>  " + lcChngAcc + IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(lnExtOnly) , '999,999.99') , TRANSFORM(VAL(lnExtOnly) , '99,999,999'))
          @ ROW,50  SAY " Total For Account   >>  " + lcChngAcc + IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(lnExtOnly) , '99,999,999.99') , TRANSFORM(VAL(lnExtOnly) , '99,999,999'))
          *B126350,4 MMR.[End]
		  *B126350,1 BWA 02/17/2005 Fix some bugs in the report.[START]
          ROW = ROW + 1
          *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
          *lnPerAcc = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lfchrgback(lcChngAcc) , '999,999.99') , TRANSFORM(lfchrgback(lcChngAcc) , '99,999,999') )
          lnPerAcc = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lfchrgback(lcChngAcc) , '99,999,999.99') , TRANSFORM(lfchrgback(lcChngAcc) , '99,999,999') )
          *B126350,4 MMR.[End]
          @ ROW,50  SAY " Charge Back Account >>  " + lcChngAcc + lnPerAcc
          *B126350,1 BWA 02/17/2005.[END]
          
          lcChngAcc = Account
        ELSE
          lnExtOnly = IIF( lcRpDeciml = "Y" ,  PADL(ALLTRIM(STR(lnExtOnly,10,2)),10," ") , PADL(ALLTRIM(STR(lnExtOnly,10)),10," ") )
          *B126350,3 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
          *@ ROW,50  SAY " Total For Account   >>  " + lcChngAcc + IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(lnExtOnly) , '999,999.99') , TRANSFORM(VAL(lnExtOnly) , '99,999,999'))
          @ ROW,50  SAY " Total For Account   >>  " + lcChngAcc + IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(lnExtOnly) , '99,999,999.99') , TRANSFORM(VAL(lnExtOnly) , '99,999,999'))
   		  *B126350,4 MMR.[End]
   		  *B126350,1 BWA 02/17/2005 Fix some bugs in the report.[START]
          ROW = ROW + 1
          *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
          *lnPerAcc = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lfchrgback(lcChngAcc) , '999,999.99') , TRANSFORM(lfchrgback(lcChngAcc) , '99,999,999') )
          lnPerAcc = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lfchrgback(lcChngAcc) , '99,999,999.99') , TRANSFORM(lfchrgback(lcChngAcc) , '99,999,999') )
          *B126350,4 MMR.[End]
          @ ROW,50  SAY " Charge Back Account >>  " + lcChngAcc + lnPerAcc
          *B126350,1 BWA 02/17/2005.[END]
          
          lcChngDiv = cDivision
          lcChngAcc = Account
          *B126350,3 MMR 10/19/2005 6)Fix  bug of not SHowing Total and chargeback lines per account if
          *B126350,3 MMR              we 2 salesrep and there is one account only for the 1rst salesrep.                            
          lcChngRep = REPCODE
          *B126350,3 MMR.[End]
        ENDIF
      ENDIF
      ROW = ROW + 1
    ENDIF
    lnExtOnly = 0
    *B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
    *B126350,2 MMR            and have returns and make modification to include credits in the charge back
    lnReturn = 0
    lnCredit = 0
    *B126350,2 MMR.[End]  
    *B126350,1 BWA 02/17/2005 Fix some bugs in the report.[START]
    lnRetOnly = 0
    *B126350,1 BWA 02/17/2005.[END]

	*--Last division in the salesrep divisions.
	IF lcRpRecap = "N"
      =lfprnDvson()
      ROW = 99
      lcRpRecap = "Y"                 && I do that to make the header of the division = All in case the total page
      =lpPrtRpHdr()
      lcRpRecap = "N"
    ENDIF

	*--Section print the divisions for the salesrep.
    STORE 0 TO lnTotDvAmt , lnTotChrgB , lnNetSales
	ROW  = ROW + 1
	IF ALEN(laAllDvi,1) > 0
      FOR lnItem = 1 TO ALEN(laAllDvi,1)
        *B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
        *B126350,2 MMR            and have returns and make modification to include credits in the charge back
        *IF VAL(laAllDvi[lnItem,2]) > 0
        IF VAL(laAllDvi[lnItem,2])+VAL(laAllDvi[lnItem,3]) > 0
        *B126350,2 MMR.[End]
          lcCheck = IIF( ROW > 55 , lpPrtRpHdr() , "" )

          ROW  = ROW + 1
          *@ ROW,5 SAY "Division :" + laAllDvi[lnItem,1] + SPACE(5) + "Amount  :" + SPACE(5) + PADL(laAllDvi[lnItem,2] , 10 , " ")
          *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
          *@ ROW,5 SAY "Division :" + laAllDvi[lnItem,1] + SPACE(5) + "Amount  :" + SPACE(5) + IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(laAllDvi[lnItem,2]) , '999,999.99') , TRANSFORM(VAL(laAllDvi[lnItem,2]) , '99,999,999'))
          @ ROW,5 SAY "Division :" + laAllDvi[lnItem,1] + SPACE(5) + "Amount  :" + SPACE(5) + IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(laAllDvi[lnItem,2]) , '99,999,999.99') , TRANSFORM(VAL(laAllDvi[lnItem,2]) , '99,999,999'))
          *B126350,4 MMR.[End]
          lnTotDvAmt = lnTotDvAmt + VAL(laAllDvi[lnItem,2])
          ROW  = ROW + 1
          *@ ROW,5 SAY SPACE(16) + "CHARGE BACKS :" + SPACE(5) + IIF(VAL(laAllDvi[lnItem,3]) > 0 , PADL(laAllDvi[lnItem,3] , 10 , " ") , "")
          *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
          *@ ROW,5 SAY SPACE(16) + "CHARGE BACKS :" + SPACE(5) + IIF(VAL(laAllDvi[lnItem,3]) > 0 , IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(laAllDvi[lnItem,3]) , '999,999.99') , TRANSFORM(VAL(laAllDvi[lnItem,3]) , '99,999,999')) , "")
          @ ROW,5 SAY SPACE(16) + "CHARGE BACKS :" + SPACE(5) + IIF(VAL(laAllDvi[lnItem,3]) > 0 , IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(laAllDvi[lnItem,3]) , '99,999,999.99') , TRANSFORM(VAL(laAllDvi[lnItem,3]) , '99,999,999')) , "")
          *B126350,4 MMR.[End]
          lnTotChrgB = lnTotChrgB + VAL(laAllDvi[lnItem,3])

          ROW  = ROW + 1
          lcNet = ALLTRIM(STR(VAL(laAllDvi[lnItem,2]) - VAL(laAllDvi[lnItem,3]),10,2))
          *@ ROW,5 SAY SPACE(16) + "NET SALES    :" + SPACE(5) + IIF(VAL(laAllDvi[lnItem,2]) # 0 , PADL(lcNet , 10 , " ") , "")
          *B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
          *B126350,2 MMR            and have returns and make modification to include credits in the charge back
          *@ ROW,5 SAY SPACE(16) + "NET SALES    :" + SPACE(5) + IIF(VAL(laAllDvi[lnItem,2]) # 0 , IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(lcNet) , '999,999.99') , TRANSFORM(VAL(lcNet) , '99,999,999')) , "")
          *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
          *@ ROW,5 SAY SPACE(16) + "NET SALES    :" + SPACE(5) +  IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(lcNet) , '999,999.99') , TRANSFORM(VAL(lcNet) , '99,999,999'))
          @ ROW,5 SAY SPACE(16) + "NET SALES    :" + SPACE(5) +  IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(lcNet) , '99,999,999.99') , TRANSFORM(VAL(lcNet) , '99,999,999'))
          *B126350,4 MMR.[End]
          *B126350,2 MMR.[End]
          lnNetSales = lnNetSales + VAL(lcNet)
          ROW  = ROW + 2
        ENDIF
      ENDFOR

      lcCheck = IIF( ROW > 55 , lpPrtRpHdr() , "" )
      *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
      *lnTotDvAmt = IIF( lcRpDeciml = "Y" ,   TRANSFORM(lnTotDvAmt , '999,999.99') , TRANSFORM(lnTotDvAmt , '99,999,999') )
      lnTotDvAmt = IIF( lcRpDeciml = "Y" ,   TRANSFORM(lnTotDvAmt , '99,999,999.99') , TRANSFORM(lnTotDvAmt , '99,999,999') )
      *B126350,4 MMR.[End]
      @ ROW,0 SAY "Grand Total:"  + SPACE(23) + "Divisions Amount >> "  + lnTotDvAmt

      ROW  = ROW + 1
      *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
      *@ ROW,5 SAY SPACE(30) + "CHARGE BACKS :" + SPACE(6) + IIF(lnTotChrgB > 0 , IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnTotChrgB , '999,999.99') , TRANSFORM(lnTotChrgB , '99,999,999')) , "")
      @ ROW,5 SAY SPACE(30) + "CHARGE BACKS :" + SPACE(6) + IIF(lnTotChrgB > 0 , IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnTotChrgB , '99,999,999.99') , TRANSFORM(lnTotChrgB , '99,999,999')) , "")
      *B126350,4 MMR.[End]
      ROW  = ROW + 1
      *B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
      *B126350,2 MMR            and have returns and make modification to include credits in the charge back
      *@ ROW,5 SAY SPACE(30) + "NET SALES    :" + SPACE(6) + IIF(lnNetSales > 0 , IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnNetSales , '999,999.99') , TRANSFORM(lnNetSales , '99,999,999') ) , "")
      *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
      *@ ROW,5 SAY SPACE(30) + "NET SALES    :" + SPACE(6) + IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnNetSales , '999,999.99') , TRANSFORM(lnNetSales , '99,999,999') )
      @ ROW,5 SAY SPACE(30) + "NET SALES    :" + SPACE(6) + IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnNetSales , '99,999,999.99') , TRANSFORM(lnNetSales , '99,999,999') )
      *B126350,4 MMR.[End]
      *B126350,2 MMR.[End]
      ROW  = ROW + 2

	  lcCheck = IIF( ROW > 55 , lpPrtRpHdr() , "" )
      *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
      *lnWalmrtRp = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnWalmrtRp , '999,999.99') , TRANSFORM(lnWalmrtRp , '99,999,999') )
      lnWalmrtRp = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnWalmrtRp , '99,999,999.99') , TRANSFORM(lnWalmrtRp , '99,999,999') )
      *B126350,4 MMR.[End]
      @ ROW,0 SAY "Total for WALMART:"  + SPACE(17) + "Replenishment >>    "  + lnWalmrtRp
      ROW  = ROW + 1
      *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
      *lnWalNonRp = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnWalNonRp , '999,999.99') , TRANSFORM(lnWalNonRp , '99,999,999') )
      lnWalNonRp = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnWalNonRp , '99,999,999.99') , TRANSFORM(lnWalNonRp , '99,999,999') )
	  *B126350,4 MMR.[End]
	  @ ROW,0 SAY SPACE(31) + "Non Replenishment >>    "  + lnWalNonRp
      ROW  = ROW + 2

      lcCheck = IIF( ROW > 55 , lpPrtRpHdr() , "" )
      *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
      *lnKmartVal = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnKmartVal , '999,999.99') , TRANSFORM(lnKmartVal , '99,999,999') )
      lnKmartVal = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnKmartVal , '99,999,999.99') , TRANSFORM(lnKmartVal , '99,999,999') )
      *B126350,4 MMR.[End]
      @ ROW,0 SAY "Total for KMART:" + SPACE(16) + lnKmartVal
      ROW  = ROW + 1

      lnOthrAcnt = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnOthrAcnt , '99,999,999.99') , TRANSFORM(lnOthrAcnt , '99,999,999') )
	  @ ROW,0 SAY "Total for the rest of Accounts: " + lnOthrAcnt
      ROW  = ROW + 2

	  @ ROW,20 SAY REPLICATE('=',60)
      ROW  = ROW + 1

      =lfRfrshDiv(.T.)
      STORE 0 TO lnWalmrtRp , lnWalNonRp , lnKmartVal , lnOthrAcnt
    ENDIF

    STORE 0 TO lnSubComm1 , lnSubComm2 , XSUBEXT , XSUBPCS , lnCalChBk
    lcHldOldDv = CDIVISION
    XSAVFLD  = REPCODE
    SELECT SALESREP
    SEEK XSAVFLD
    XREPNAME = IIF( FOUND() , NAME , '' )
	
    SELECT (lcInvltemp)
      ROW = 99
      =lpPrtRpHdr()
  ENDIF
  SELECT (lcInvltemp)
  *--Section to print totals if the account or the division changed.
  *B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
  *B126350,2 MMR            and have returns and make modification to include credits in the charge back
  *IF (lcChngDiv # cDivision OR lcChngAcc # Account) AND lnExtOnly > 0 AND lcRpRecap = "N"
  *B126350,3 MMR 10/19/2005 6)Fix  bug of not SHowing Total and chargeback lines per account if
  *B126350,3 MMR              we 2 salesrep and there is one account only for the 1rst salesrep.                            
  *IF (lcChngDiv # cDivision OR lcChngAcc # Account) AND (lnExtOnly+lnReturn+(-1*lnCredit)) > 0 AND lcRpRecap = "N"
  IF (lcChngDiv # cDivision OR lcChngAcc # Account OR lcChngRep # REPCODE) AND (lnExtOnly+lnReturn+(-1*lnCredit)) > 0 AND lcRpRecap = "N"
  *B126350,3 MMR.[End]
  *B126350,2 MMR.[End]
    IF (lcChngDiv # cDivision) AND (lcChngAcc == Account)
      lnExtOnly = IIF( lcRpDeciml = "Y" ,  PADL(ALLTRIM(STR(lnExtOnly,10,2)),10," ") , PADL(ALLTRIM(STR(lnExtOnly,10)),10," ") )

      *B126350,1 BWA 02/17/2005 Fix some bugs in the report.[START]
      *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
      *@ ROW,50  SAY " Total For Account   >>  " + lcChngAcc + IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(lnExtOnly) , '999,999.99') , TRANSFORM(VAL(lnExtOnly) , '99,999,999'))
      @ ROW,50  SAY " Total For Account   >>  " + lcChngAcc + IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(lnExtOnly) , '99,999,999.99') , TRANSFORM(VAL(lnExtOnly) , '99,999,999'))
      *B126350,4 MMR.[End]
      ROW = ROW + 1
      *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
      *lnPerAcc = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lfchrgback(lcChngAcc) , '999,999.99') , TRANSFORM(lfchrgback(lcChngAcc) , '99,999,999') )
      lnPerAcc = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lfchrgback(lcChngAcc) , '99,999,999.99') , TRANSFORM(lfchrgback(lcChngAcc) , '99,999,999') )
      *B126350,4 MMR[End]
      @ ROW,50  SAY " Charge Back Account >>  " + lcChngAcc + lnPerAcc
      *B126350,1 BWA 02/17/2005.[END]

    ELSE
      IF (lcChngAcc # Account) AND (lcChngDiv == cDivision)
        lnExtOnly = IIF( lcRpDeciml = "Y" ,  PADL(ALLTRIM(STR(lnExtOnly,10,2)),10," ") , PADL(ALLTRIM(STR(lnExtOnly,10)),10," ") )
        *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
        *@ ROW,50  SAY " Total For Account   >>  " + lcChngAcc + IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(lnExtOnly) , '999,999.99') , TRANSFORM(VAL(lnExtOnly) , '99,999,999'))
        @ ROW,50  SAY " Total For Account   >>  " + lcChngAcc + IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(lnExtOnly) , '99,999,999.99') , TRANSFORM(VAL(lnExtOnly) , '99,999,999'))
        *B126350,4 MMR.[End]
        *B126350,1 BWA 02/17/2005 Fix some bugs in the report.[START]
        ROW = ROW + 1
        *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
        *lnPerAcc = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lfchrgback(lcChngAcc) , '999,999.99') , TRANSFORM(lfchrgback(lcChngAcc) , '99,999,999') )
        lnPerAcc = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lfchrgback(lcChngAcc) , '99,999,999.99') , TRANSFORM(lfchrgback(lcChngAcc) , '99,999,999') )
        *B126350,4 MMR.[End]
        @ ROW,50  SAY " Charge Back Account >>  " + lcChngAcc + lnPerAcc
        *B126350,1 BWA 02/17/2005.[END]

        lcChngAcc = Account
      ELSE
        lnExtOnly = IIF( lcRpDeciml = "Y" ,  PADL(ALLTRIM(STR(lnExtOnly,10,2)),10," ") , PADL(ALLTRIM(STR(lnExtOnly,10)),10," ") )      
        *@ ROW,50  SAY " Total For Account >>  " + lcChngAcc + lnExtOnly
        *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
        *@ ROW,50  SAY " Total For Account   >>  " + lcChngAcc + IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(lnExtOnly) , '999,999.99') , TRANSFORM(VAL(lnExtOnly) , '99,999,999'))
        @ ROW,50  SAY " Total For Account   >>  " + lcChngAcc + IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(lnExtOnly) , '99,999,999.99') , TRANSFORM(VAL(lnExtOnly) , '99,999,999'))
  		*B126350,4 MMR.[End]
  		
  		*B126350,1 BWA 02/17/2005 Fix some bugs in the report.[START]
        ROW = ROW + 1
        *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
        *lnPerAcc = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lfchrgback(lcChngAcc) , '999,999.99') , TRANSFORM(lfchrgback(lcChngAcc) , '99,999,999') )
        lnPerAcc = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lfchrgback(lcChngAcc) , '99,999,999.99') , TRANSFORM(lfchrgback(lcChngAcc) , '99,999,999') )
        *B126350,4 MMR.[End]
        @ ROW,50  SAY " Charge Back Account >>  " + lcChngAcc + lnPerAcc
        *B126350,1 BWA 02/17/2005.[END]
        
        lcChngAcc = Account
      ENDIF
    ENDIF  
    ROW = ROW + 2
    lnExtOnly = neqvamnt
    *B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
    *B126350,2 MMR            and have returns and make modification to include credits in the charge back
    lnReturn  =  &lcInvlTemp..nReturns
    lnCredit  =  &lcInvlTemp..nCredit 
    *B126350,2 MMR.[End]
  ELSE
    lnExtOnly = lnExtOnly + neqvamnt
    *B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
    *B126350,2 MMR            and have returns and make modification to include credits in the charge back
    lnReturn  = lnReturn + &lcInvlTemp..nReturns
    lnCredit  = lnCredit + &lcInvlTemp..nCredit 
    *B126350,2 MMR.[End]
    lcChngAcc = Account
  ENDIF
  *B126350,3 MMR 10/19/2005 Fix  bug of not SHowing chargeback if recap=yes and i have 2 divisions
  *lnCalChBk = lnCalChBk + 1
  IF lcChngDiv = cDivision
    lnCalChBk = lnCalChBk + 1
  ELSE
    lnCalChBk =1
  ENDIF  
  *B126350,3 MMR.[End]
  IF lcChngDiv # cDivision AND !EMPTY(lcChngDiv) AND lcRpRecap = "N"
    lnCalChBk = 1
    =lfprnDvson()
    ROW = 99
    =lpPrtRpHdr()
  ENDIF
  lcChngDiv = cDivision
  *B126350,3 MMR 10/19/2005 6)Fix  bug of not SHowing Total and chargeback lines per account if
  *B126350,3 MMR              we 2 salesrep and there is one account only for the 1rst salesrep.                            
  lcChngRep = REPCODE 
  *B126350,3 MMR.[End]
  *--Fill the division array.
    =lfDivFill(cDivision,neqvamnt,INVHDR.TOTALCHG)

  *B126350,3 MMR 10/19/2005 8)Fix  bug of not deducting chargebacks from the special reacp for 
  *B126350,3 MMR              Kmart and Wallmart.[Start] 
  lnWalmrtRp = lnWalmrtRp + IIF(&lcInvltemp..Account = "W149A" , IIF(LEFT(&lcInvltemp..Style,3) == "302" OR LEFT(&lcInvltemp..Style,3) == "303" , (Price * TotQty) , 0 ) , 0)
  lnWalNonRp = lnWalNonRp + IIF(&lcInvltemp..Account = "W149A" , IIF(LEFT(&lcInvltemp..Style,3) == "302" OR LEFT(&lcInvltemp..Style,3) == "303" , 0 , (Price * TotQty) ) , 0)
  
  *lnKmartVal = lnKmartVal + IIF(&lcInvltemp..Account = "K550A" , (Price * TotQty) , 0)
  lnKmartVal = lnKmartVal + IIF(&lcInvltemp..Account = "K550A" , (Price * TotQty) - lfchrgback("lcKmart") , 0)
  *B126350,3 MMR.[End]

  *B126350,1 BWA 02/17/2005 Fix some bugs in the report.[START]
  *lnOthrAcnt = lnOthrAcnt + IIF(&lcInvltemp..Account $ ["K550A" , "W149A"] , 0 , (Price * TotQty))
  *B126350,4 MMR 11/20/2005 2)Fix bug of showing rest of account with wrong values.[Start] 
  *lnOthrAcnt = lnOthrAcnt + IIF(&lcInvltemp..Account $ ["K550A" , "W149A"] , 0 , (Price * TotQty) - lfchrgback("AllAcc"))
  lnOthrAcnt = lnOthrAcnt + IIF(&lcInvltemp..Account $ ["K550A" , "W149A"] , 0 , (neqvamnt) - lfchrgback("AllAcc"))
  *B126350,4 MMR.[End]
  *B126350,1 BWA 02/17/2005.[END]

  IF llStyCom
    lnStyAmt = Price * TotQty
    lnStyNet = (lnStyAmt-( InvHdr.DiscPcnt * lnStyAmt / 100 )) ;
               * ( 1-InvHdr.Trde_Disc / 100 )
  ENDIF
  IF XREP = 'D' AND lcRpRecap = "N"
    @ ROW,0  SAY INVOICE
    @ ROW,07 SAY INVDATE
    @ ROW,16 SAY ACCOUNT
    @ ROW,22 SAY LEFT(CUSTOMER.STNAME,7)
    @ ROW,30 SAY SUBSTR(STYLE , lnStyPosGl , lnStyLnGl)
    @ ROW,50 SAY TOTQTY  PICTURE '999999'
    IF lcRpDeciml = "Y"
      @ ROW,57 SAY PRICE   PICTURE '9999.99'
      @ ROW,64 SAY TOTQTY*PRICE PICTURE '9999999.99'
    ELSE
      @ ROW,57 SAY PRICE   PICTURE '9999999'
      @ ROW,64 SAY TOTQTY*PRICE PICTURE '9999999999'
    ENDIF  
    @ ROW,75 SAY ORDER
    @ ROW,82 SAY LEFT(INVHDR.CUSTPO,10)
    IF llStyCom
      IF lcRpDeciml = "Y"
        @ Row,93 SAY InvHdr.Rep1             PICTURE '!!!'
        @ Row,97 SAY Comm1                   PICTURE '99.99'
        @ Row,103 SAY lnStyNet * Comm1 / 100 PICTURE '999999.99'
        @ Row,113 SAY InvHdr.Rep2            PICTURE '!!!'
        @ Row,117 SAY Comm2                  PICTURE '99.99'
        @ Row,123 SAY lnStyNet * Comm2 / 100 PICTURE '999999.99'
      ELSE
        @ Row,93 SAY InvHdr.Rep1             PICTURE '!!!'
        @ Row,97 SAY Comm1                   PICTURE '99999'
        @ Row,103 SAY lnStyNet * Comm1 / 100 PICTURE '999999999'
        @ Row,113 SAY InvHdr.Rep2            PICTURE '!!!'
        @ Row,117 SAY Comm2                  PICTURE '99999'
        @ Row,123 SAY lnStyNet * Comm2 / 100 PICTURE '999999999'
      ENDIF  
    ENDIF
    ROW = ROW + 1
  ENDIF

  IF llStyCom
    lnSubComm1 = lnSubComm1 + (lnStyNet * Comm1 / 100)
    lnSubComm2 = lnSubComm2 + (lnStyNet * Comm2 / 100)
  ENDIF
  XSUBPCS = XSUBPCS + TOTQTY
  XSUBEXT = XSUBEXT + (TOTQTY * PRICE )
ENDSCAN
*--Section to print the last total for the account or division changed.
IF lcRpRecap = "N"
  lnExtOnly = IIF( lcRpDeciml = "Y" ,  PADL(ALLTRIM(STR(lnExtOnly,10,2)),10," ") , PADL(ALLTRIM(STR(lnExtOnly,10)),10," ") )
  *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
  *@ ROW,50  SAY " Total For Account   >>  " + lcChngAcc + IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(lnExtOnly) , '999,999.99') , TRANSFORM(VAL(lnExtOnly) , '99,999,999'))
  @ ROW,50  SAY " Total For Account   >>  " + lcChngAcc + IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(lnExtOnly) , '99,999,999.99') , TRANSFORM(VAL(lnExtOnly) , '99,999,999'))
  *B126350,4 MMR.[End]
  *B126350,1 BWA 02/17/2005 Fix some bugs in the report.[START]
  ROW = ROW + 1
  *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
  *lnPerAcc = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lfchrgback(lcChngAcc) , '999,999.99') , TRANSFORM(lfchrgback(lcChngAcc) , '99,999,999') )
  lnPerAcc = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lfchrgback(lcChngAcc) , '99,999,999.99') , TRANSFORM(lfchrgback(lcChngAcc) , '99,999,999') )
  *B126350,4 MMR.[End]
  @ ROW,50  SAY " Charge Back Account >>  " + lcChngAcc + lnPerAcc
  *B126350,1 BWA 02/17/2005.[END]
  
  ROW = ROW + 1
ENDIF

*--Last division in the salesrep divisions.
IF lcRpRecap = "N"
  =lfprnDvson()
  ROW = 99
  lcRpRecap = "Y"                 && I do that to make the header of the division = All in case the total page
  =lpPrtRpHdr()
  lcRpRecap = "N"
ENDIF

*--Section print the divisions for the salesrep.
STORE 0 TO lnTotDvAmt , lnTotChrgB , lnNetSales
ROW  = ROW + 1
IF ALEN(laAllDvi,1) > 0
  FOR lnItem = 1 TO ALEN(laAllDvi,1)
    *B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
    *B126350,2 MMR            and have returns and make modification to include credits in the charge back
    *IF VAL(laAllDvi[lnItem,2]) > 0
    IF VAL(laAllDvi[lnItem,2])+VAL(laAllDvi[lnItem,3]) > 0
    *B126350,2 MMR.[End]
      lcCheck = IIF( ROW > 55 , lpPrtRpHdr() , "" )
      ROW  = ROW + 1
      *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
      *@ ROW,5 SAY "Division :" + laAllDvi[lnItem,1] + SPACE(5) + "Amount  :" + SPACE(5) + IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(laAllDvi[lnItem,2]) , '999,999.99') , TRANSFORM(VAL(laAllDvi[lnItem,2]) , '99,999,999'))
      @ ROW,5 SAY "Division :" + laAllDvi[lnItem,1] + SPACE(5) + "Amount  :" + SPACE(5) + IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(laAllDvi[lnItem,2]) , '99,999,999.99') , TRANSFORM(VAL(laAllDvi[lnItem,2]) , '99,999,999'))
      *B126350,4 MMR.[End]
      lnTotDvAmt = lnTotDvAmt + VAL(laAllDvi[lnItem,2])

      ROW  = ROW + 1
      *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
      *@ ROW,5 SAY SPACE(16) + "CHARGE BACKS :" + SPACE(5) + IIF(VAL(laAllDvi[lnItem,3]) > 0 , IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(laAllDvi[lnItem,3]) , '999,999.99') , TRANSFORM(VAL(laAllDvi[lnItem,3]) , '99,999,999')) , "")
      @ ROW,5 SAY SPACE(16) + "CHARGE BACKS :" + SPACE(5) + IIF(VAL(laAllDvi[lnItem,3]) > 0 , IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(laAllDvi[lnItem,3]) , '99,999,999.99') , TRANSFORM(VAL(laAllDvi[lnItem,3]) , '99,999,999')) , "")
      *B126350,4 MMR.[End]
     
      lnTotChrgB = lnTotChrgB + VAL(laAllDvi[lnItem,3])

      ROW  = ROW + 1
      lcNet = ALLTRIM(STR(VAL(laAllDvi[lnItem,2]) - VAL(laAllDvi[lnItem,3]),10,2))
      *B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
      *B126350,2 MMR            and have returns and make modification to include credits in the charge back
      *@ ROW,5 SAY SPACE(16) + "NET SALES    :" + SPACE(5) + IIF(VAL(laAllDvi[lnItem,2]) # 0 , IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(lcNet) , '999,999.99') , TRANSFORM(VAL(lcNet) , '99,999,999')) , "")
      *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
      *@ ROW,5 SAY SPACE(16) + "NET SALES    :" + SPACE(5) + IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(lcNet) , '999,999.99') , TRANSFORM(VAL(lcNet) , '99,999,999')) 
      @ ROW,5 SAY SPACE(16) + "NET SALES    :" + SPACE(5) + IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(lcNet) , '99,999,999.99') , TRANSFORM(VAL(lcNet) , '99,999,999')) 
      *B126350,4 MMR.[End]
      *B126350,2 MMR.[End]
      lnNetSales = lnNetSales + VAL(lcNet)
      ROW  = ROW + 2
    ENDIF
  ENDFOR
  lcCheck = IIF( ROW > 55 , lpPrtRpHdr() , "" )
  *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
  *lnTotDvAmt = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnTotDvAmt , '999,999.99') , TRANSFORM(lnTotDvAmt , '99,999,999') )
  lnTotDvAmt = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnTotDvAmt , '99,999,999.99') , TRANSFORM(lnTotDvAmt , '99,999,999') )
  *B126350,4 MMR.[End]
  @ ROW,0 SAY "Grand Total:"  + SPACE(23) + "Divisions Amount >> "  + lnTotDvAmt

  ROW  = ROW + 1
  *B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
  *B126350,2 MMR            and have returns and make modification to include credits in the charge back
  IF lnTotChrgB = 0
    STORE 0 to lnRec,lnTotCR,lnTotRT
    lcAli=''
    lcAli=ALIAS()
    SELECT (lcInvltemp)
    lnRec=RECNO()
    *B126350,3 MMR 10/19/2005 3)Fixing bug of repeating chargebacks for every salesrep. 
    *SUM nCredit  TO lnTotCR
    SUM nCredit  TO lnTotCR FOR(REPCODE==lcSlsRep)
    *SUM nReturns TO lnTotRT
    SUM nReturns TO lnTotRT FOR(REPCODE==lcSlsRep)
    lcSlsRep=''
    *B126350,3 MMR.[End]
    lnTotCR= -1*lnTotCR
    lnTotChrgB=lnTotCR+lnTotRT
    IF lnRec<>RECNO()
      GOTO lnRec
    ENDIF
    SELECT (lcAli)
  ENDIF  
  *B126350,2 MMR.[End]
  *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
  *@ ROW,5 SAY SPACE(30) + "CHARGE BACKS :" + SPACE(6) + IIF(lnTotChrgB > 0 , IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnTotChrgB , '999,999.99') , TRANSFORM(lnTotChrgB , '99,999,999') ) , "")
  @ ROW,5 SAY SPACE(30) + "CHARGE BACKS :" + SPACE(6) + IIF(lnTotChrgB > 0 , IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnTotChrgB , '99,999,999.99') , TRANSFORM(lnTotChrgB , '99,999,999') ) , "")
  *B126350,4 MMR.[End]
  ROW  = ROW + 1
  *B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
  *B126350,2 MMR            and have returns and make modification to include credits in the charge back
  *@ ROW,5 SAY SPACE(30) + "NET SALES    :" + SPACE(6) + IIF(lnNetSales > 0 , IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnNetSales , '999,999.99') , TRANSFORM(lnNetSales , '99,999,999') ) , "")
  *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
  *@ ROW,5 SAY SPACE(30) + "NET SALES    :" + SPACE(6) + IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnNetSales , '999,999.99') , TRANSFORM(lnNetSales , '99,999,999') ) 
  @ ROW,5 SAY SPACE(30) + "NET SALES    :" + SPACE(6) + IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnNetSales , '99,999,999.99') , TRANSFORM(lnNetSales , '99,999,999') ) 
  *B126350,4 MMR.[End]
  *B126350,2 MMR.[End]
  ROW  = ROW + 2

  lcCheck = IIF( ROW > 55 , lpPrtRpHdr() , "" )
  *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
  *lnWalmrtRp = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnWalmrtRp , '999,999.99') , TRANSFORM(lnWalmrtRp , '99,999,999') )
  lnWalmrtRp = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnWalmrtRp , '99,999,999.99') , TRANSFORM(lnWalmrtRp , '99,999,999') )
  *B126350,4 MMR.[End]
  @ ROW,0 SAY "Total for WALMART:"  + SPACE(17) + "Replenishment >>    "  + lnWalmrtRp
  ROW  = ROW + 1
  *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
  *lnWalNonRp = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnWalNonRp , '999,999.99') , TRANSFORM(lnWalNonRp , '99,999,999') )
  lnWalNonRp = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnWalNonRp , '99,999,999.99') , TRANSFORM(lnWalNonRp , '99,999,999') )
  *B126350,4 MMR.[End]
  @ ROW,0 SAY SPACE(31) + "Non Replenishment >>    "  + lnWalNonRp
  ROW  = ROW + 2

  lcCheck = IIF( ROW > 55 , lpPrtRpHdr() , "" )
  *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
  *lnKmartVal = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnKmartVal , '999,999.99') , TRANSFORM(lnKmartVal , '99,999,999') )
  lnKmartVal = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnKmartVal , '99,999,999.99') , TRANSFORM(lnKmartVal , '99,999,999') )
  *B126350,4 MMR.[End]
  @ ROW,0 SAY "Total for KMART:" + SPACE(16) + lnKmartVal
  ROW  = ROW + 1

  lnOthrAcnt = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnOthrAcnt , '99,999,999.99') , TRANSFORM(lnOthrAcnt , '99,999,999') )
  
  @ ROW,0 SAY "Total for the rest of Accounts: " + lnOthrAcnt
  ROW  = ROW + 2
  =lfRstAcBak()                    && Print the charge back of the rest of the account.
  IF ALEN(laRstAccCb,1) > 0 AND !EMPTY(laRstAccCb[1,1])
    FOR lnChrBk = 1 TO ALEN(laRstAccCb,1)
       IF laRstAccCb[lnChrBk,2] >0
       lcCheck = IIF( ROW > 55 , lpPrtRpHdr() , "" )
      *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
      *@ ROW,0 SAY "Charge Back for Account: " + laRstAccCb[lnChrBk,1] + " >> " +;
                  IIF( lcRpDeciml = "Y" ,  TRANSFORM(laRstAccCb[lnChrBk,2] , '999,999.99') , TRANSFORM(laRstAccCb[lnChrBk,2] , '99,999,999'))
      @ ROW,0 SAY "Charge Back for Account: " + laRstAccCb[lnChrBk,1] + " >> " +;
                  IIF( lcRpDeciml = "Y" ,  TRANSFORM(laRstAccCb[lnChrBk,2] , '99,999,999.99') , TRANSFORM(laRstAccCb[lnChrBk,2] , '99,999,999'))
      *B126350,4 MMR.[End]
      
      ROW  = ROW + 1
      ENDIF
    ENDFOR
    ROW  = ROW + 1
  ENDIF
  
  @ ROW,20 SAY REPLICATE('=',60)
  ROW  = ROW + 1

  =lfRfrshDiv(.T.)
  STORE 0 TO lnWalmrtRp , lnWalNonRp, lnKmartVal , lnOthrAcnt
ENDIF

*--End of lpPrint.
*!*************************************************************
*! Name      : lfvSalesr
*! Developer : Bassem Rafaat ERNEST(BWA)
*! Date      : 10/20/2004
*! Purpose   : Validate Primary Sales Rep. in SALESREP file.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfBrows
*!*************************************************************
*! Called from : Option Grid [Sales representative Object]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvSalesr()
*!*************************************************************
FUNCTION lfvSalesr
PRIVATE lcVar , lcObj , laTemp

lcSaleF   = VARREAD()
lcSales   = EVALUATE(lcSaleF) 
lnSelFile =  SELECT(0)
SELECT SALESREP
lcCusTag  = ORDER('SALESREP')
SET ORDER TO TAG SALESREP IN SALESREP
IF !EMPTY(lcSales) .AND. ('?' $ lcSales OR !SEEK(lcSales , 'SALESREP'))
  DIMENSION laTemp[1]
  laTemp = ''    
  lcBrFields  = "REPCODE   :R :H= 'Code' , "   +;
                "NAME      :R :H= 'Name' ,"    +;
                "cAddress6 :R :H= 'Country' ," +;
                "PHONE     :R :H= 'Phone' ,"   +;
                "BALANCE   :R :H= 'Balance' "
  lcFile_Ttl  = "Sales Representative ..."
  = gfBrows('','REPCODE','laTemp')
  IF !EMPTY(laTemp[1])
    lcSales   = laTemp[1]
  ELSE   
    lcSales   = ""
  ENDIF    
ENDIF    
&lcSaleF = lcSales 
SET ORDER TO lcCusTag
SELECT (lnSelFile)

*--End of lfvSalesr.
*!*************************************************************
*! Name      : lfvAcc
*! Developer : Bassem Rafaat ERNEST(BWA)
*! Date      : 10/20/2004
*! Purpose   : Validate Account
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvAcc()
*!*************************************************************
FUNCTION lfvAcc
PRIVATE lcCusFld,lcCustom,lnSelFile,lcCusTag

lcCusFld  = VARREAD()
lcCustom  = EVAL(lcCusFld)
lnSelFile =  SELECT(0)
SELECT CUSTOMER
lcCusTag  = ORDER('CUSTOMER')
SET ORDER TO TAG CUSTOMER IN CUSTOMER
IF !EMPTY(lcCustom) .AND. ( '?' $ lcCustom .OR. !SEEK((IIF( EMPTY(STORE) , 'M'+lcCustom , 'S'+lcCustom+STORE )) , 'CUSTOMER'))
  =CUSBROWM (@lcCustom)
ENDIF
&lcCusFld = lcCustom
SET ORDER TO lcCusTag
SELECT (lnSelFile)

*--End of lfvAcc.
*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Bassem Rafaat ERNEST(BWA)
*! Date      : 10/20/2004
*! Purpose   : To set relation on or off when running the in 
*!           : range function in the option grid.
*!*************************************************************
*! Called from : SRSPNOR.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfsrvSty()
*!*************************************************************
FUNCTION lfsrvSty
PARAMETERS lcParm

IF lcParm = 'S'  && Set code
  SET ORDER TO TAG CSTYLE IN STYLE
ELSE
  SET ORDER TO TAG STYLE  IN STYLE
ENDIF

*--End of lfsrvSty.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Bassem Rafaat ERNEST(BWA)
*! Date      : 10/20/2004
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

lnDatapos = ASCAN(laOGFxFlt,'INVHDR.INVDATE')
IF lnDatapos > 0
  lnDatapos = ASUBSCRIPT(laOGFxFlt,lnDatapos,1)
  IF EMPTY(laOGFxFlt[lnDatapos,6])
    laOGFxFlt[lnDataPos,6] = DTOC(gdSysDate) + '|' + DTOC(gdSysDate)
  ENDIF
ENDIF

*--End of lfwRepWhen.
*!*************************************************************
*! Name      : lpPrtRpHdr
*! Developer : Bassem Rafaat ERNEST(BWA)
*! Date      : 10/20/2004
*! Purpose   : Print The Heder Of The Report.
*!*************************************************************
*! Called from : lpPrint
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lpPrtRpHdr()
*!*************************************************************
FUNCTION lpPrtRpHdr

IF ROW > 55
  lcDivInfo = PADR('Salesrep:' + IIF(!EMPTY(lcRpSales) , lcRpSales , XSAVFLD ) + SPACE(8) + IIF(EMPTY(XSAVFLD) AND EMPTY(lcRpSales) , SPACE(24), SALESREP.NAME) , 55 , " ")

  IF EMPTY(XSAVFLD)
    SELECT (lcInvltemp)
    XSAVFLD = REPCODE
    SELECT SALESREP
    SEEK XSAVFLD
    SELECT (lcInvltemp)
  ENDIF

  lcHldOldDv = CDIVISION
  PAGENO = PAGENO + 1

  XRPTNAME = lcDivInfo + lcRpOpMsg

  DO RPT_HDR WITH XREPORT,XRPTNAME,R_WIDTH

  ROW = 6
   *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....2....+....3..
       @ ROW,0 SAY 'INVOIC INVDATE  ACCT# NAME... STYLE                PIECE   PRICE       EXT ORDER  CUST P/O#  '+;
                IIF( llStyCom, 'REP1 COMM  COMM.AMT REP2 COMM  COMM.AMT ', '' )

  ROW = ROW + 1
  IF XREP='D'
    @ ROW,00 SAY REPLICATE('=',132)
    ROW = ROW + 1
  ENDIF
ENDIF

*-- End OF lpPrtRpHdr.  
*!*************************************************************
*! Name      : lfGetFltrs
*! Developer : Bassem Rafaat ERNEST(BWA)
*! Date      : 10/20/2004
*! Purpose   : collect all the selected filters 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetFltrs()
*!*************************************************************
FUNCTION lfGetFltrs

lnAccPos   = lfItmPos('CUSTOMER.ACCOUNT')
lnStyPos   = lfItmPos('STYLE.CSTYMAJOR')
lnDivPos   = lfItmPos('INVHDR.CDIVISION')
lnSeaSPos  = lfItmPos('INVHDR.SEASON')
lnDatePos  = lfItmPos('INVHDR.INVDATE')

IF !EMPTY(XSL)
  lcMainExp = "(INVHDR.REP1 = '" + XSL + "' .OR. INVHDR.REP2 = '" + XSL + "')"
ENDIF                     
lcMainExp  = lcMainExp + IIF(!EMPTY(lcMainExp)," .AND. ","")+" INVHDR.STATUS <> 'V' "

llCustomer = USED(laOgFxFlt[lnAccPos,6]) AND RECCOUNT(laOgFxFlt[lnAccPos,6]) > 0
lcCustFile = IIF(llCustomer,laOgFxFlt[lnAccPos,6],"")
*B126350,3 MMR 10/19/2005 7)Fix  bug of not SHowing Met Sales if i choose specific Style then
*B126350,3 MMR              preivew then Tag non and preivew again.[Start]                                
*lcStylFile = IIF(USED(laOgFxFlt[lnStyPos,6]),laOgFxFlt[lnStyPos,6],"")
IF USED(laOgFxFlt[lnStyPos,6]) AND RECCOUNT(laOgFxFlt[lnStyPos,6])>0
   lcStylFile=laOgFxFlt[lnStyPos,6]
ELSE
  lcStylFile=""
ENDIF    
*B126350,3 MMR.[End]

lcWorkFile = IIF(llCustomer,lcCustFile,IIF(!EMPTY(lcStylFile),lcStylFile,'INVHDR'))
 
lcDivFltr  = IIF(!EMPTY(laOgFxFlt[lnDivPos,6]),lfMakeExpr(lnDivPos),"")

lcMainExp = lcMainExp + IIF(!EMPTY(lcDivFltr)," .AND. ","")+lcDivFltr

lcSeasFltr = IIF(!EMPTY(laOgFxFlt[lnSeaSPos,6]),lfMakeExpr(lnSeaSPos),"")
lcSeasFltr = STRTRAN(lcSeasFltr,'INVHDR.','')

lcDatFltr  = IIF(!EMPTY(laOgFxFlt[lnDatePos,6]),lfMakeExpr(lnDatePos),"")
lcDatFltr  = STRTRAN(lcDatFltr,'INVHDR.','')

*--End of lfGetFltrs.
*!*************************************************************
*! Name      : lfItmPos
*! Developer : Bassem Rafaat ERNEST(BWA)
*! Date      : 10/20/2004
*! Purpose   : get the item position in laOgFxFlter
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfItmPos()
*!*************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*--End of lfItmPos.
*!*************************************************************
*! Name      : lfMakeExpr
*! Developer : Bassem Rafaat ERNEST(BWA)
*! Date      : 10/20/2004
*! Purpose   : creat an expersion for specific filter
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMakeExpr()
*!*************************************************************
FUNCTION lfMakeExpr
PARAMETERS lnFxFltItm
PRIVATE lcItmExpr

lcItmExpr = ''
*-- if operator is like
IF 'LIKE' $ UPPER(ALLTRIM(laOGFxFlt[lnFxFltItm,5]))
  lcItmExpr = ALLTRIM(laOGFxFlt[lnFxFltItm,1]) + ' = "' +  laOGFxFlt[lnFxFltItm,6] + '"'

ELSE  && operator is either Between or InList
  *-- if operator is between.
  IF 'BETWEEN' $ UPPER(ALLTRIM(laOGFxFlt[lnFxFltItm,5]))
    *-- if Type is Date
    PRIVATE lcFrom,lcTo
    lcFrom = SUBSTR(laOGFxFlt[lnFxFltItm,6],1,ATC('|',laOGFxFlt[lnFxFltItm,6]) - 1)
    lcTo   = SUBSTR(laOGFxFlt[lnFxFltItm,6],ATC('|',laOGFxFlt[lnFxFltItm,6]) + 1)

    IF laOGFxFlt[lnFxFltItm,3] = 'D'
      lcFrom = CTOD(lcFrom)
      lcTo   = CTOD(lcTo)

      IF !EMPTY(lcFrom)
        lcFrom = DTOS(lcFrom)
        lcTo   = DTOS(lcTo)
        lcItmExpr = 'BETWEEN( DTOS(' + ALLTRIM(laOGFxFlt[lnFxFltItm,1]) + ') , "' + ;
                                 lcFrom + '" , "' + lcTo + '")'
      ENDIF  
    ENDIF
  ELSE

    *-- if in range case.
    IF laOGFxFlt[lnFxFltItm,7] = 'R'
      lcItmExpr = 'SEEK(' + ALLTRIM(laOGFxFlt[lnFxFltItm,1]) + ', "' +;
                            ALLTRIM(laOGFxFlt[lnFxFltItm,6]) + '")'
    ELSE
      lcItmExpr = ALLTRIM(laOGFxFlt[lnFxFltItm,1]) + ' $ "' + laOGFxFlt[lnFxFltItm,6] + '"'
    ENDIF                           
  ENDIF

ENDIF
RETURN IIF(EMPTY(lcItmExpr),lcItmExpr,'(' + lcItmExpr + ')')

*--End of lfMakeExpr.
*!*************************************************************
*! Name      : lfGetData
*! Developer : Bassem Rafaat ERNEST(BWA)
*! Date      : 10/20/2004
*! Purpose   : creat a record to the temp file 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetData()
*!*************************************************************
FUNCTION lfGetData

SCATTER MEMVAR MEMO
WAIT 'Selecting records for report , Invoice# '+m.Invoice       WINDOW NOWAIT
IF EMPTY(XSL)
  IF EMPTY(INVHDR.Rep1)  .AND. EMPTY(INVHDR.Rep2)
    m.FLAG = ''
    m.REPCODE = SPACE(0)
    INSERT INTO (lcInvltemp) FROM MEMVAR 
  ELSE
    IF !EMPTY(INVHDR.Rep1)
      m.FLAG = '1'
      M.REPCODE = INVHDR.Rep1
      INSERT INTO (lcInvltemp) FROM MEMVAR 
    ENDIF
    IF !EMPTY(INVHDR.Rep2) 
      m.FLAG = '2'
      M.REPCODE = INVHDR.Rep2
      INSERT INTO (lcInvltemp) FROM MEMVAR 
    ENDIF
  ENDIF
ELSE
  m.FLAG = IIF(INVHDR.REP1 = XSL ,'1','2')
  M.REPCODE = IIF(INVHDR.REP1 = XSL,INVHDR.Rep1,INVHDR.Rep2)
  INSERT INTO (lcInvltemp) FROM MEMVAR 
ENDIF

*--End of lfGetData.
*!*************************************************************
*! Name      : lfAddField
*! Developer : Bassem Rafaat ERNEST(BWA)
*! Date      : 10/20/2004
*! Purpose   : Add fields to the array of file structure.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Passed Parameters : lcFldName -- Field Name
*!                   : lcFldType -- Field Type (C;N;L....M)
*!                   : lnFldLen  -- Field Length
*!                   : lnFldDec  -- Field Decimal
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfAddField()
*!*************************************************************
FUNCTION lfAddField
PARAMETERS lcStruArry , lcFldName , lcFldType , lnFldLen , lnFldDec

lnFldPos  = ALEN(&lcStruArry,1) + IIF(TYPE('&lcStruArry') = 'L', 0 , 1 )
DIMENSION &lcStruArry[lnFldPos , 4]
&lcStruArry[lnFldPos , 1]	= lcFldName
&lcStruArry[lnFldPos , 2]	= lcFldType
&lcStruArry[lnFldPos , 3]	= lnFldLen
&lcStruArry[lnFldPos , 4]	= lnFldDec

*--End of lfAddField.

*!*************************************************************
*! Name      : lfPrtCover
*! Developer : Bassem Rafaat ERNEST(BWA)
*! Date      : 10/20/2004
*! Purpose   : Print the Cover page hold the divisions Info.
*!*************************************************************
*! Called from : lpPrint
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfPrtCover()
*!*************************************************************
FUNCTION lfPrtCover
*B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
*B126350,2 MMR            and have returns and make modification to include credits in the charge back
*PRIVATE lnTotDiv
PRIVATE lnTotDiv,lcAlis,lcdiv
lnRecno  = 0
lnTotRetD= 0 
*B126350,2 MMR.[End]
lnTotDiv = 0
*XRPTNAME = SPACE(55) + lcRpOpMsg
XRPTNAME = "SALESREP :" + IIF(!EMPTY(lcRpSales) , ALLTRIM(lcRpSales) + SPACE(8) + SALESREP.NAME + SPACE(10) ,  "ALL" + SPACE(42))  + lcRpOpMsg

PAGENO = PAGENO + 1
DO RPT_HDR WITH XREPORT,XRPTNAME,R_WIDTH
ROW = 6

@ ROW,0 SAY SPACE(9) + 'Divisions' + SPACE(20) + 'Amounts'

ROW = ROW + 1
@ ROW,00 SAY REPLICATE('=',132)
ROW = ROW + 1

FOR lnItem = 1 TO ALEN(laDvision,1)
*B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
*B126350,2 MMR            and have returns and make modification to include credits in the charge back
lcAlis=ALIAS()
SELECT (lcInvltemp)
lnRecno=RECNO()
SUM nCredit FOR (cdivision==laDvision[lnItem,1])  TO lnTotRetD
laDvision[lnItem,2]=laDvision[lnItem,2]+lnTotRetD
IF lnRecno<>RECNO()
  GOTO lnRecno
ENDIF  
SELECT (lcAlis)
*B126350,2 MMR.[End]
  *@ ROW,0 SAY PADL(laDvision[lnItem,1] , 15 , " ") + SPACE(20) + PADL(STR(laDvision[lnItem,2]) , 10 , " ")
  *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
  *@ ROW,0 SAY PADL(laDvision[lnItem,1] , 15 , " ") + SPACE(20) + IIF( lcRpDeciml = "Y" ,  TRANSFORM(laDvision[lnItem,2] , '999,999.99') , TRANSFORM(laDvision[lnItem,2] , '99,999,999'))
  @ ROW,0 SAY PADL(laDvision[lnItem,1] , 15 , " ") + SPACE(20) + IIF( lcRpDeciml = "Y" ,  TRANSFORM(laDvision[lnItem,2] , '99,999,999.99') , TRANSFORM(laDvision[lnItem,2] , '99,999,999'))
  *B126350,4 MMR.[End]
  lnTotDiv = lnTotDiv + laDvision[lnItem,2]
  ROW = ROW + 1
  IF ROW > 50
    =lfPrtCover()
  ENDIF
ENDFOR
*B126350,2 MMR 08/11/2005 leave 2 lines after the last printed record.
IF ROW <=50
  ROW = ROW + 3

  @ ROW,00 SAY REPLICATE('=',132)
  ROW = ROW + 1
  *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
  *lnTotDiv = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnTotDiv , '999,999.99') , TRANSFORM(lnTotDiv , '99,999,999') )
  lnTotDiv = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnTotDiv , '99,999,999.99') , TRANSFORM(lnTotDiv , '99,999,999') )
  *B126350,4 MMR.[End]
  *B126350,2 MMR 08/11/2005 leave 2 lines after the last printed record.
  *@ ROW,0 SAY 'Grand Total >>'  + SPACE(24) + lnTotDiv
  @ ROW,0 SAY 'Grand Total >>'  + SPACE(21) + lnTotDiv
  *B126350,2 MMR.[End]
  ROW = ROW + 1
  @ ROW,00 SAY REPLICATE('=',132)
  ROW = ROW + 1
ELSE
*B126350,2 MMR .[End]
ROW = 52
@ ROW,00 SAY REPLICATE('=',132)
ROW = ROW + 1

*lnTotDiv = IIF( lcRpDeciml = "Y" ,  PADL(ALLTRIM(STR(lnTotDiv,10,2)),10," ") , PADL(ALLTRIM(STR(lnTotDiv,10)),10," ") )
*B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
*lnTotDiv = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnTotDiv , '999,999.99') , TRANSFORM(lnTotDiv , '99,999,999') )
lnTotDiv = IIF( lcRpDeciml = "Y" ,  TRANSFORM(lnTotDiv , '99,999,999.99') , TRANSFORM(lnTotDiv , '99,999,999') )
*B126350,4 MMR.[End]
@ ROW,0 SAY 'Grand Total >>'  + SPACE(24) + lnTotDiv
ROW = ROW + 1

@ ROW,00 SAY REPLICATE('=',132)
ROW = ROW + 1
*B126350,2 MMR 08/11/2005 leave 2 lines after the last printed record.
ENDIF
*B126350,2 MMR.[End]
ROW = 99

*-- End OF FUNCTION.
*!*************************************************************
*! Name      : lfDvsion
*! Developer : Bassem Rafaat ERNEST(BWA)
*! Date      : 10/20/2004
*! Purpose   : Function to collect the date of the divisions.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfDvsion()
*!*************************************************************
FUNCTION lfDvsion
PRIVATE lcTag , lnContDiv , lcdivision , lnColDvVal , llUpdatVal

STORE '' TO lcdivision
STORE 0 TO lnColDvVal
llUpdatVal = .F.

SELECT (lcInvltemp)
SELECT DISTINCT cDivision , 0 , "" , "" FROM (lcInvltemp) INTO ARRAY laDvision

lcTag = ORDER()
SET ORDER TO TAG CDIVISION

SCAN
  IF lcdivision # cDivision
    lnDvPos = ASUBSCRIPT(laDvision , ASCAN(laDvision , lcdivision) , 1 )
    IF lnDvPos > 0 AND lnColDvVal > 0
      lnColDvVal = IIF( lcRpDeciml = "Y" ,  lnColDvVal , ROUND(lnColDvVal,0) )
      laDvision[lnDvPos,2] = lnColDvVal
    ENDIF
    lcdivision = cDivision
    lnColDvVal = neqvamnt
  ELSE
    lnColDvVal = lnColDvVal + neqvamnt
  ENDIF

ENDSCAN

IF !llUpdatVal
  lnDvPos = ASUBSCRIPT(laDvision , ASCAN(laDvision , lcdivision) , 1 )
  IF lnDvPos > 0 AND lnColDvVal > 0
    lnColDvVal = IIF( lcRpDeciml = "Y" ,  lnColDvVal , ROUND(lnColDvVal,0) )
    laDvision[lnDvPos,2] = lnColDvVal
  ENDIF
ENDIF

SELECT (lcInvltemp)
SET ORDER TO TAG &lcTag
LOCATE

*B126350,1 BWA 02/17/2005 Fix some bugs in the report.[START]
=lfCutCharg()
*B126350,1 BWA 02/17/2005.[END]

*--Update the laAllDvi with the values in laDvision.
DIMENSION laAllDvi[ALEN(laDvision,1),4]
=ACOPY(laDvision , laAllDvi)

*--End of lfDvsion.
*!*************************************************************
*! Name      : lfRfrshDiv
*! Developer : Bassem Rafaat ERNEST(BWA)
*! Date      : 10/20/2004
*! Purpose   : Function to refresh the division array.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfRfrshDiv()
*!*************************************************************
FUNCTION lfRfrshDiv
PARAMETERS llParam

IF ALEN(laDvision,1) > 0
  FOR lnItem = 1 TO ALEN(laDvision,1)
    STORE SPACE(0) TO laDvision[lnItem,2] , laDvision[lnItem,3] , laDvision[lnItem,4]
  ENDFOR
ENDIF

IF ALEN(laAllDvi,1) > 0 AND llParam
  FOR lnItem = 1 TO ALEN(laAllDvi,1)
    STORE SPACE(0) TO laAllDvi[lnItem,2] , laAllDvi[lnItem,3] , laAllDvi[lnItem,4]
  ENDFOR
ENDIF

*--End of lfRfrshDiv.
*!*************************************************************
*! Name      : lfsrvAcc
*! Developer : Bassem Rafaat ERNEST(BWA)
*! Date      : 10/20/2004
*! Purpose   : 
*!*************************************************************
*! Called from : SRSPNOR.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfsrvAcc()
*!*************************************************************
FUNCTION lfsrvAcc
PARAMETERS lcParm

IF lcParm = 'S'  && Set code
  GOTO TOP IN CUSTOMER  
ENDIF

*--End of lfsrvAcc.
*!*************************************************************
*! Name      : lfChkStrct
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/18/2004
*! Purpose   : Get the Style and Color Length.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfChkStrct()
*!*************************************************************
FUNCTION lfChkStrct

*--THE STYLE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lnStyLnGl  = LEN(laItemSeg[lnCount,3])
    lnStyPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--End of lfChkStrct.
*!*************************************************************
*! Name      : lfprnDvson
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/18/2004
*! Purpose   : To print the division.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfprnDvson()
*!*************************************************************
FUNCTION lfprnDvson
*--Section print the divisions for the salesrep.
STORE 0 TO lnTotDvAmt , lnTotChrgB , lnNetSales
ROW  = ROW + 1
IF ALEN(laDvision,1) > 0
  FOR lnItem = 1 TO ALEN(laDvision,1)
    *B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
    *B126350,2 MMR            and have returns and make modification to include credits in the charge back
    *IF VAL(laDvision[lnItem,2]) > 0
    IF VAL(laDvision[lnItem,2])+VAL(laDvision[lnItem,3]) > 0 
    *B126350,2 MMR. [End]
      lcCheck = IIF( ROW > 55 , lpPrtRpHdr() , "" )
      ROW  = ROW + 1
     *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
      *@ ROW,5 SAY "Division :" + laDvision[lnItem,1] + SPACE(5) + "Amount  :" + SPACE(5) + IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(laDvision[lnItem,2]) , '999,999.99') , TRANSFORM(VAL(laDvision[lnItem,2]) , '99,999,999'))
      @ ROW,5 SAY "Division :" + laDvision[lnItem,1] + SPACE(5) + "Amount  :" + SPACE(5) + IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(laDvision[lnItem,2]) , '99,999,999.99') , TRANSFORM(VAL(laDvision[lnItem,2]) , '99,999,999'))
      *B126350,4 MMR.[End]
      lnTotDvAmt = lnTotDvAmt + VAL(laDvision[lnItem,2])

      ROW  = ROW + 1
      *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
      *@ ROW,5 SAY SPACE(16) + "CHARGE BACKS :" + SPACE(5) + IIF(VAL(laDvision[lnItem,3]) > 0 , IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(laDvision[lnItem,3]) , '999,999.99') , TRANSFORM(VAL(laDvision[lnItem,3]) , '99,999,999')) , "")
      @ ROW,5 SAY SPACE(16) + "CHARGE BACKS :" + SPACE(5) + IIF(VAL(laDvision[lnItem,3]) > 0 , IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(laDvision[lnItem,3]) , '99,999,999.99') , TRANSFORM(VAL(laDvision[lnItem,3]) , '99,999,999')) , "")
      *B126350,4 MMR.[End]
      lnTotChrgB = lnTotChrgB + VAL(laDvision[lnItem,3])

      ROW  = ROW + 1
      lcNet = ALLTRIM(STR(VAL(laDvision[lnItem,2]) - VAL(laDvision[lnItem,3]),10,2))
      *B126350,4 MMR 11/20/2005 1)Fix  bug of showing the amounts as *** 
      *@ ROW,5 SAY SPACE(16) + "NET SALES    :" + SPACE(5) + IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(lcNet) , '999,999.99') , TRANSFORM(VAL(lcNet) , '99,999,999')) 
      @ ROW,5 SAY SPACE(16) + "NET SALES    :" + SPACE(5) + IIF( lcRpDeciml = "Y" ,  TRANSFORM(VAL(lcNet) , '99,999,999.99') , TRANSFORM(VAL(lcNet) , '99,999,999')) 
      *B126350,4 MMR.[End]
      lnNetSales = lnNetSales + VAL(lcNet)
       
      ROW  = ROW + 2
    ENDIF
  ENDFOR

  =lfRfrshDiv(.F.)
ENDIF

*--End of lfprnDvson.
*!*************************************************************
*! Name      : RPT_HDR
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/18/2004
*! Purpose   : PRINTS THE HEADER (WIDE PAPER,NARROW PAPER).
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : 
*!*************************************************************
*! Passed Parameters  : 1) PROGRAM
*!                    : 2) TITLE    ADDITIONAL TITLE (XRPTNAME)
*!                    : 3) TYPE     'N' -> NARROW   'W'-> WIDE    'XW'-> XTRA WIDE
*!                    : NOTE: THE ACTUAL REPORT CAN START ON LINE 5
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : DO RPT_HDR
*!*************************************************************
PROCEDURE RPT_HDR
PARAMETER XPROG,XRPTNAME,XTYPE

PRIVATE ALL LIKE X*
R_TITLE = TRIM(R_TITLE)

X1 = ((130 - (LEN(TRIM(QCOMPANY))))/2)
X2 = ((130 - (LEN( R_TITLE  )))/2)

@ 01,000 SAY XPROG
@ 01,X1  SAY QCOMPANY
@ 01,120 SAY DATE()
@ 01,129 SAY '~'   &&TAK 06/05/94
@ 02,000 SAY TIME()
@ 02,X2  SAY R_TITLE
@ 02,120 SAY 'PAGE#'
@ 02,126 SAY STR(PAGENO,4)
@ 03,000 SAY ALLTRIM(XRPTNAME)
@ 04,000 SAY PADR(IIF(llDivPrn , 'Division:' + IIF(lcRpRecap = "Y" , "All" , lcHldOldDv) + SPACE(5) + IIF(lcRpRecap = "Y" , "" , gfCodDes(lcHldOldDv , 'CDIVISION')) , ""),55," ") + "Period : " + ldDatFrom + "  To  " + ldDatTo
@ 05,000 SAY REPLICATE('*',132)

RETURN

*--End of RPT_HDR.
*!*************************************************************
*! Name      : lfMargData
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/18/2004
*! Purpose   : To marge the collected data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfMargData()
*!*************************************************************
FUNCTION lfMargData
*B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
*B126350,2 MMR            and have returns and make modification to include credits in the charge back
*PRIVATE lcRepCode , lcDivision , lcStyle , lcOrder , lcInvoice
*STORE SPACE(0) TO lcRepCode , lcDivision , lcStyle , lcInvoice
PRIVATE lcRepCode , lcDivision , lcStyle , lcOrder , lcInvoice , lcAcount
STORE SPACE(0) TO lcRepCode , lcDivision , lcStyle , lcInvoice , lcAcount
*B126350,2 MMR.[End]
SELECT (lcInvltemp)
lcOrder = ORDER()
=AFIELDS(laFileStru)

Create Table (gcWorkDir + lcStyltmp) FROM ARRAY laFileStru
INDEX ON REPCODE + CDIVISION + SUBSTR(STYLE , lnStyPosGl , lnStyLnGl) TAG StyleVal OF (gcWorkDir + lcStyltmp +'.CDX') ADDITIVE
INDEX ON CDIVISION TAG CDIVISION OF (gcWorkDir + lcStyltmp +'.CDX') ADDITIVE
INDEX ON REPCODE + CDIVISION + ACCOUNT + INVOICE TAG (lcInvltemp) OF (gcWorkDir + lcStyltmp +'.CDX') ADDITIVE

SELECT (lcInvltemp)
SCAN
  SCATTER MEMVAR MEMO
  *B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
  *B126350,2 MMR            and have returns and make modification to include credits in the charge back
  *IF (lcInvoice # M.INVOICE) OR (lcRepCode # M.REPCODE) OR (lcDivision # M.CDIVISION) OR (lcStyle # SUBSTR(M.STYLE , lnStyPosGl , lnStyLnGl))
  IF (lcInvoice # M.INVOICE) OR (lcRepCode # M.REPCODE) OR (lcDivision # M.CDIVISION) OR (lcStyle # SUBSTR(M.STYLE , lnStyPosGl , lnStyLnGl)) OR lcAcount # M.ACCOUNT
  *B126350,2 MMR.[End]
    SELECT (lcStyltmp)
    APPEND BLANK
    GATHER MEMVAR MEMO
  ELSE
    REPLACE &lcStyltmp..TOTQTY   WITH &lcStyltmp..TOTQTY   + M.TOTQTY   ,;
		    &lcStyltmp..neqvamnt WITH &lcStyltmp..neqvamnt + M.neqvamnt
  ENDIF
  lcRepCode  = M.REPCODE
  lcDivision = M.CDIVISION
  lcStyle    = SUBSTR(M.STYLE , lnStyPosGl , lnStyLnGl)
  lcInvoice  = M.INVOICE
  *B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
  *B126350,2 MMR            and have returns and make modification to include credits in the charge back
  lcAcount   = M.ACCOUNT
  *B126350,2 MMR.[End]
ENDSCAN
*--NNA  (START)  && Disconnect the relation Between (lcInvltemp) with InvHdr and Customer
SELECT (lcInvltemp)
SET RELATION TO
*--NNA (END)

lcInvltemp = lcStyltmp
SELECT (lcInvltemp)
SET ORDER TO TAG &lcOrder

*--NNA (START)  set Relation Between the new (lcInvltemp) with InvHdr and Customer
SET RELATION TO INVOICE INTO INVHDR
SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE ) INTO CUSTOMER ADDITIVE
*--NNA (END)

*--End of lfMargData.
*!*************************************************************
*! Name      : lfDivFill
*! Developer : Bassem Rafaat ERNEST(BWA)
*! Date      : 10/20/2004
*! Purpose   : Function to fill values for the divisions.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfDivFill()
*!*************************************************************
FUNCTION lfDivFill
PARAMETERS lcDivPar1 , lnValPar2 , lcInvPar4
PRIVATE lcSeekKey , lnRetrnVal , lcAlasRet
STORE SPACE(0) TO lcSeekKey , lcAlasRet
*B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
*B126350,2 MMR            and have returns and make modification to include credits in the charge back
*STORE 0 TO lnRetrnVal
STORE 0 TO lnRetrnVal,lnTotR,lnR
*B126350,2 MMR.[End]
lcAlasRet = SELECT(0)

IF lnCalChBk = 1
  SELECT RETHDR
  *B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
  *B126350,2 MMR            and have returns and make modification to include credits in the charge back
  *SCAN
  SCAN FOR &lcOldRpExp
  *B126350,2 MMR.[End]
    *B126350,3 MMR 10/19/2005 5)Fix  bug of not SHowing right amounts for divisions 
    *B126350,3 MMR              in the Cover page if i have more than SALESREP   
    IF !EMPTY(RETHDR.salesrep1) 
    *B126350,3 MMR.[END]    
      IF (&lcInvltemp..REPCODE == RETHDR.SALESREP1) AND (RETHDR.CDIVISION == lcDivPar1) ;
         AND BETWEEN(DTOS(RETHDR.CRDATE),lcFrom,lcTo) AND STATUS <> 'V'
        lnRetrnVal = lnRetrnVal + RETHDR.AMOUNT
      ENDIF
    *B126350,3 MMR 10/19/2005 5)Fix  bug of not SHowing right amounts for divisions 
    *B126350,3 MMR              in the Cover page if i have more than SALESREP   
    ELSE
      IF  !EMPTY(RETHDR.salesrep2)
        IF (&lcInvltemp..REPCODE == RETHDR.SALESREP2 ; 
           AND (RETHDR.CDIVISION == lcDivPar1) ;
           AND BETWEEN(DTOS(RETHDR.CRDATE),lcFrom,lcTo) AND STATUS <> 'V'
          lnRetrnVal = lnRetrnVal + RETHDR.AMOUNT
        ENDIF
      ENDIF
    ENDIF  
    *B126350,3 MMR.[End]
  ENDSCAN
*B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
*B126350,2 MMR            and have returns and make modification to include credits in the charge back
lcAlss=ALIAS()
SELECT (lcInvltemp)
lnR=RECNO()
*B126350,3 MMR 10/19/2005 3)Fixing bug of repeating chargebacks for every salesrep. 
lcSlsRep=REPCODE
*SUM nCredit FOR (cdivision==lcDivPar1)  TO lnTotR
SUM nCredit FOR (cdivision==lcDivPar1) AND (REPCODE==lcSlsRep)  TO lnTotR
*B126350,3 MMR.[End]
lnTotR= -1*lnTotR
lnRetrnVal=lnRetrnVal+lnTotR
IF lnR<>RECNO()
   GOTO lnR
ENDIF
SELECT (lcAlss)
*B126350,2 MMR.[End]
ENDIF
lnDvPos = ASUBSCRIPT(laDvision , ASCAN(laDvision , lcDivPar1) , 1 )
IF lnDvPos > 0
  IF lcRpDeciml = "Y" 
    laDvision[lnDvPos,2] = ALLTRIM(STR(VAL(laDvision[lnDvPos,2]) + lnValPar2 , 10 , 2))
    laDvision[lnDvPos,3] = IIF(lnCalChBk = 1 , ALLTRIM(STR(VAL(laDvision[lnDvPos,3]) + lnRetrnVal, 10 , 2)) ,;
                                               ALLTRIM(STR(VAL(laDvision[lnDvPos,3]), 10 , 2)))
    
    laAllDvi[lnDvPos,2] = ALLTRIM(STR(VAL(laAllDvi[lnDvPos,2]) + lnValPar2 , 10 , 2))
    laAllDvi[lnDvPos,3] = IIF(lnCalChBk = 1 , ALLTRIM(STR(VAL(laAllDvi[lnDvPos,3]) + lnRetrnVal, 10 , 2)) ,;
											  ALLTRIM(STR(VAL(laAllDvi[lnDvPos,3]), 10 , 2)))
  ELSE
    laDvision[lnDvPos,2] = ALLTRIM(STR(VAL(laDvision[lnDvPos,2]) + lnValPar2 , 10 ))
    laDvision[lnDvPos,3] = IIF(lnCalChBk = 1 , ALLTRIM(STR(VAL(laDvision[lnDvPos,3]) + lnRetrnVal, 10 )) ,;
											   ALLTRIM(STR(VAL(laDvision[lnDvPos,3]), 10 )))
    
    laAllDvi[lnDvPos,2] = ALLTRIM(STR(VAL(laAllDvi[lnDvPos,2]) + lnValPar2 , 10 ))
    laAllDvi[lnDvPos,3] = IIF(lnCalChBk = 1 , ALLTRIM(STR(VAL(laAllDvi[lnDvPos,3]) + lnRetrnVal, 10 )) ,;
											  ALLTRIM(STR(VAL(laAllDvi[lnDvPos,3]), 10 )))
  ENDIF
  
  IF ASCAN(laInvoice , EVAL(lcInvltemp+'.INVOICE')) = 0
    IF lcRpDeciml = "Y" 
      laDvision[lnDvPos,4] = ALLTRIM(STR(VAL(laDvision[lnDvPos,4]) + lcInvPar4 , 10 , 2))
      
      laAllDvi[lnDvPos,4] = ALLTRIM(STR(VAL(laAllDvi[lnDvPos,4]) + lcInvPar4 , 10 , 2))
    ELSE
      laDvision[lnDvPos,4] = ALLTRIM(STR(VAL(laDvision[lnDvPos,4]) + lcInvPar4 , 10 ))
      
      laAllDvi[lnDvPos,4] = ALLTRIM(STR(VAL(laAllDvi[lnDvPos,4]) + lcInvPar4 , 10 ))
    ENDIF

      IF EMPTY(laInvoice[1])
        laInvoice[1] = EVAL(lcInvltemp+'.INVOICE')
      ELSE
        DIMENSION laInvoice[ALEN(laInvoice,1) + 1]
        =AINS(laInvoice, ALEN(laInvoice,1))
        laInvoice[ALEN(laInvoice,1)] = EVAL(lcInvltemp+'.INVOICE')
      ENDIF
  ENDIF
 
ENDIF

SELECT(lcAlasRet)

*--ENd of lfDivFill.
*B126350,1 BWA 02/17/2005 Fix some bugs in the report.[START]
*!*************************************************************
*! Name      : lfCutCharg
*! Developer : Bassem Rafaat ERNEST(BWA)
*! Date      : 10/20/2004
*! Purpose   : Function to get the value of the divisions.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfCutCharg()
*!*************************************************************
FUNCTION lfCutCharg
PRIVATE lnRetrnVal , lcAlasRet , lcRepCode , lcDivision
STORE SPACE(0) TO lcAlasRet , lcRepCode , lcDivision

lcAlasRet = SELECT(0)

SELECT (lcInvltemp)
SCAN
  IF ((lcRepCode # EVAL(lcInvltemp+'.REPCODE') OR lcDivision # EVAL(lcInvltemp+'.cdivision')) ;
      AND !EMPTY(EVAL(lcInvltemp+'.REPCODE')))
     
    lcRepCode  = EVAL(lcInvltemp+'.REPCODE')
	lcDivision = EVAL(lcInvltemp+'.cdivision')
	lndiv = ASUBSCRIPT(laDvision,ASCAN(laDvision,lcDivision),1)

    lnRetrnVal = 0
    SELECT RETHDR
    *B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
    *B126350,2 MMR            and have returns and make modification to include credits in the charge back
    *SCAN
    SCAN FOR &lcOldRpExp
    *B126350,2 MMR.[End]
      *B126350,3 MMR 10/19/2005 5)Fix  bug of not SHowing right amounts for divisions 
      *B126350,3 MMR              in the Cover page if i have more than SALESREP   
      IF !EMPTY(RETHDR.salesrep1) 
        *IF (IIF(EMPTY(XSL),.T.,&lcInvltemp..REPCODE == RETHDR.SALESREP1)) AND (RETHDR.CDIVISION == laDvision[lndiv,1]) ;
           AND BETWEEN(DTOS(RETHDR.CRDATE),lcFrom,lcTo) AND STATUS <> 'V'
        IF (&lcInvltemp..REPCODE == RETHDR.SALESREP1) AND (RETHDR.CDIVISION == laDvision[lndiv,1]) ;
           AND BETWEEN(DTOS(RETHDR.CRDATE),lcFrom,lcTo) AND STATUS <> 'V'
      *B126350,3 MMR.[End]   
          lnRetrnVal = lnRetrnVal + RETHDR.AMOUNT
        ENDIF
      *B126350,3 MMR 10/19/2005 5)Fix  bug of not SHowing right amounts for divisions 
      *B126350,3 MMR              in the Cover page if i have more than SALESREP  
      ELSE 
        IF !EMPTY(RETHDR.salesrep2) 
          IF (&lcInvltemp..REPCODE == RETHDR.SALESREP2) AND (RETHDR.CDIVISION == laDvision[lndiv,1]) ;
             AND BETWEEN(DTOS(RETHDR.CRDATE),lcFrom,lcTo) AND STATUS <> 'V'
            lnRetrnVal = lnRetrnVal + RETHDR.AMOUNT
          ENDIF
        ENDIF
      ENDIF  
      *B126350,3 MMR.[End]   
    ENDSCAN
    laDvision[lndiv,2] = laDvision[lndiv,2] - lnRetrnVal
  ENDIF

  lcRepCode  = EVAL(lcInvltemp+'.REPCODE')
  lcDivision = EVAL(lcInvltemp+'.cdivision')

ENDSCAN

SELECT(lcAlasRet)

*--ENd of lfCutCharg.
*!*************************************************************
*! Name      : lfchrgback
*! Developer : Bassem Rafaat ERNEST(BWA)
*! Date      : 10/20/2004
*! Purpose   : Function to get the charge back for the accounts.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfchrgback()
*!*************************************************************
FUNCTION lfchrgback
PARAMETERS lcFromWher
PRIVATE lcAlasORg , lnElements
*B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
*B126350,2 MMR            and have returns and make modification to include credits in the charge back
*STORE SPACE(0) TO lcAlasORg , lnElements
STORE SPACE(0) TO lcAlasORg , lnElements,lcAlis,lcOrd
lnRecno = 0
*B126350,2 MMR.[End]
lnRtrnChBk = 0
lcAlasORg = SELECT(0)
IF lcFromWher == "AllAcc"
  *lcthreeFld = ALLTRIM(&lcInvltemp..REPCODE) + ALLTRIM(&lcInvltemp..cdivision) + ALLTRIM(&lcInvltemp..ACCOUNT)
  lcthreeFld = ALLTRIM(&lcInvltemp..REPCODE) + ALLTRIM(&lcInvltemp..cdivision) 
  IF ASCAN(laAccRepDv,lcthreeFld) = 0
    lnElements = ALEN(laAccRepDv,1) + IIF(EMPTY(laAccRepDv[1]) , 0 , 1 )
    DIMENSION laAccRepDv[lnElements]
    =AINS(laAccRepDv, lnElements )
    laAccRepDv[lnElements] = lcthreeFld
*B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
*B126350,2 MMR            and have returns and make modification to include credits in the charge back
lcAlis=ALIAS()
*B126350,3 MMR 10/19/2005 2)Fixing bug of showing credit adjustments if the user enters wrong
*B126350,3 MMR              division in the reference field.[Start]   
SELECT CODES
lcOrd=ORDER()
SET ORDER TO Ccode_no
*B126350,3 MMR.[End]
SELECT REPCOMM
SCAN 
  SELECT CREDIT
  IF SEEK ('5'+REPCOMM.TRAN,'CREDIT')  
      *B126350,3 MMR 10/19/2005 2)Fixing bug of showing credit adjustments if the user enters wrong
      *B126350,3 MMR              division in the reference field.   
      *B126350,3 MMR            4)Fixing bug of including chargesbacks for Kmart and Wallmart in 
      *B126350,3 MMR              the total rest for account.[Start] 
      *IF (&lcInvltemp..REPCODE == REPCOMM.REPCODE) AND (SUBSTR(CREDIT.REFERENCE,1,6)== &lcInvltemp..cdivision) ;
          AND BETWEEN(DTOS(REPCOMM.DATE),lcFrom,lcTo) 
      IF (&lcInvltemp..REPCODE == REPCOMM.REPCODE) AND (SUBSTR(CREDIT.REFERENCE,1,6)== &lcInvltemp..cdivision) ;
         AND BETWEEN(DTOS(REPCOMM.DATE),lcFrom,lcTo) AND SEEK ('N'+'CDIVISION '+SUBSTR(CREDIT.REFERENCE,1,6),'CODES');
         AND (CREDIT.Account #"K550A") AND (CREDIT.Account#"W149A")  
      *B126350,3 MMR.[End]   
          lnRtrnChBk = lnRtrnChBk + CREDIT.AMOUNT
      ENDIF
  ENDIF    
ENDSCAN
lnRtrnChBk= -1*lnRtrnChBk
*B126350,3 MMR 10/19/2005 2)Fixing bug of showing credit adjustments if the user enters wrong
*B126350,3 MMR              division in the reference field.[Start]   
SELECT CODES
SET ORDER TO (lcOrd)
*B126350,3 MMR.[End]   
SELECT (lcAlis)
*B126350,2 MMR.[End]
    SELECT RETHDR
    SCAN
      *B126350,3 MMR 10/19/2005 4)Fixing bug of including chargesbacks for Kmart and Wallmart in 
      *B126350,3 MMR              the total rest for account. 
      *B126350,3 MMR            9)Fix  bug of not showing the 2nd SalesRep data if exist 
      *IF (&lcInvltemp..REPCODE == RETHDR.SALESREP1) AND (RETHDR.CDIVISION == &lcInvltemp..cdivision) ;
          AND BETWEEN(DTOS(RETHDR.CRDATE),lcFrom,lcTo) AND STATUS <> 'V' 
      IF !EMPTY(RETHDR.salesrep1) 
        IF (&lcInvltemp..REPCODE == RETHDR.SALESREP1) AND (RETHDR.CDIVISION == &lcInvltemp..cdivision) ;
            AND BETWEEN(DTOS(RETHDR.CRDATE),lcFrom,lcTo) AND STATUS <> 'V';
            AND (RETHDR.Account #"K550A") AND (RETHDR.Account#"W149A") 
            lnRtrnChBk = lnRtrnChBk + RETHDR.AMOUNT
        ENDIF
      ENDIF
      IF !EMPTY(RETHDR.salesrep2) 
        IF (&lcInvltemp..REPCODE == RETHDR.SALESREP2) AND (RETHDR.CDIVISION == &lcInvltemp..cdivision) ;
            AND BETWEEN(DTOS(RETHDR.CRDATE),lcFrom,lcTo) AND STATUS <> 'V';
            AND (RETHDR.Account #"K550A") AND (RETHDR.Account#"W149A") 
            lnRtrnChBk = lnRtrnChBk + RETHDR.AMOUNT
        ENDIF
      ENDIF
     *B126350,3 MMR.[End]    
    ENDSCAN
  ENDIF
ELSE
  *B126350,3 MMR 10/19/2005 8)Fix  bug of not deducting chargebacks from the special reacp for 
  *B126350,3 MMR              Kmart and Wallmart.[Start] 
  IF lcFromWher # "lcKmart"
  *B126350,3 MMR.[END]
    SELECT (lcInvltemp)
    SKIP -1
    *B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
    *B126350,2 MMR            and have returns and make modification to include credits in the charge back
    lnRecno=RECNO()
    lcdiv=cdivision
    *B126350,3 MMR 10/19/2005 3)Fixing bug of repeating chargebacks for every salesrep. 
    lcSlsRep=REPCODE
    *SUM nCredit FOR (cdivision==lcdiv) AND (ACCOUNT==lcFromWher) TO lnRtrnChBk 
    SUM nCredit FOR (cdivision==lcdiv) AND (ACCOUNT==lcFromWher) AND (REPCODE==lcSlsRep) TO lnRtrnChBk 
    *B126350,3 MMR.[End]
    lnRtrnChBk= -1*lnRtrnChBk
    IF lnRecno<>RECNO()
      GOTO lnRecno
    ENDIF
    *B126350,2 MMR.[End]
    SELECT RETHDR
    =SEEK(lcFromWher)
    SCAN REST WHILE ACCOUNT = lcFromWher
      *B126350,3 MMR            9)Fix  bug of not showing the 2nd SalesRep data if exist 
      IF !EMPTY(RETHDR.salesrep1) 
      *B126350,3 MMR.[End]
        IF (SALESREP1 == &lcInvltemp..REPCODE ) AND (CDIVISION == &lcInvltemp..cdivision) ;
            AND BETWEEN(DTOS(RETHDR.CRDATE),lcFrom,lcTo) AND STATUS <> 'V'
          lnRtrnChBk = lnRtrnChBk + RETHDR.AMOUNT
        ENDIF
      *B126350,3 MMR            9)Fix  bug of not showing the 2nd SalesRep data if exist 
      ENDIF
      IF !EMPTY(RETHDR.salesrep2) 
        IF (SALESREP2 == &lcInvltemp..REPCODE ) AND (CDIVISION == &lcInvltemp..cdivision) ;
            AND BETWEEN(DTOS(RETHDR.CRDATE),lcFrom,lcTo) AND STATUS <> 'V'
          lnRtrnChBk = lnRtrnChBk + RETHDR.AMOUNT
        ENDIF
      ENDIF
      *B126350,3 MMR.[End]  
    ENDSCAN
    SELECT (lcInvltemp)
    SKIP
  *B126350,3 MMR 10/19/2005 8)Fix  bug of not deducting chargebacks from the special reacp for 
  *B126350,3 MMR              Kmart and Wallmart.[Start] 
  ELSE
    SELECT (lcInvltemp)
    lnRecno=RECNO()
    lcdiv=cdivision
    lcSlsRep=REPCODE
    SUM nCredit FOR (ACCOUNT=="K550A") AND (cdivision==lcdiv) AND (REPCODE==lcSlsRep) TO lnRtrnChBk 
    lnRtrnChBk= -1*lnRtrnChBk
    IF lnRecno<>RECNO()
      GOTO lnRecno
    ENDIF
    SELECT RETHDR
    =SEEK("K550A")
    SCAN REST WHILE ACCOUNT == "K550A"
      *B126350,3 MMR            9)Fix  bug of not showing the 2nd SalesRep data if exist 
      IF !EMPTY(RETHDR.salesrep1) 
      *B126350,3 MMR.[End]
        IF (SALESREP1 == &lcInvltemp..REPCODE ) AND (RETHDR.CDIVISION == &lcInvltemp..cdivision);
            AND BETWEEN(DTOS(RETHDR.CRDATE),lcFrom,lcTo) AND STATUS <> 'V'
          lnRtrnChBk = lnRtrnChBk + RETHDR.AMOUNT
        ENDIF
      *B126350,3 MMR            9)Fix  bug of not showing the 2nd SalesRep data if exist 
      ENDIF
      IF !EMPTY(RETHDR.salesrep2) 
        IF (SALESREP2 == &lcInvltemp..REPCODE ) AND (RETHDR.CDIVISION == &lcInvltemp..cdivision);
            AND BETWEEN(DTOS(RETHDR.CRDATE),lcFrom,lcTo) AND STATUS <> 'V'
          lnRtrnChBk = lnRtrnChBk + RETHDR.AMOUNT
        ENDIF
      ENDIF
      *B126350,3 MMR.[END]
    ENDSCAN
    SELECT (lcInvltemp)
  ENDIF
  *B126350,3 MMR.[End]  
ENDIF
SELECT(lcAlasORg)
RETURN lnRtrnChBk

*--ENd of lfchrgback.
*!*************************************************************
*! Name      : lfRstAcBak
*! Developer : Bassem Rafaat ERNEST(BWA)
*! Date      : 10/20/2004
*! Purpose   : Function to Print the charge back of the rest of the account.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfRstAcBak()
*!*************************************************************
FUNCTION lfRstAcBak
PRIVATE lcAlias , lcKey , laAccPrnt
DIMENSION laAccPrnt[1]
STORE '' TO laAccPrnt

STORE (0) TO lcAlias , lcKey
lcAlias = SELECT(0)
lcKey = EVAL(KEY())

SELECT (lcInvltemp)
SELECT DISTINCT ACCOUNT FROM (lcInvltemp) INTO ARRAY laAccPrnt

SELECT RETHDR
SELECT DISTINCT ACCOUNT , 0 FROM RETHDR INTO ARRAY laRstAccCb WHERE BETWEEN(DTOS(RETHDR.CRDATE),lcFrom,lcTo)
SCAN
  IF !EMPTY(XSL) AND salesrep1<> XSL
    LOOP
  ENDIF
  IF BETWEEN(DTOS(RETHDR.CRDATE),lcFrom,lcTo) AND STATUS <> 'V'
    IF ASCAN( laAccPrnt , RETHDR.ACCOUNT) = 0 AND IIF(!EMPTY(laOGFxFlt[2,6]),RETHDR.cdivision $ laOGFxFlt[2,6],.T.)
    
      lnItmPos = ASUBSCRIPT(laRstAccCb,ASCAN(laRstAccCb,RETHDR.ACCOUNT),1)    
      laRstAccCb[lnItmPos,2] = laRstAccCb[lnItmPos,2] + RETHDR.AMOUNT
    ENDIF
  ENDIF
ENDSCAN

*B126350,2 MMR 08/11/2005 Fix main bug for not including accounts if they havent invoices
*B126350,2 MMR            and have returns and make modification to include credits in the charge back
SELECT REPCOMM
SCAN
  IF !EMPTY(XSL) AND REPCODE<> XSL
    LOOP
  ENDIF
  SELECT CREDIT
  IF SEEK ('5'+REPCOMM.TRAN,'CREDIT') 
    LOCATE FOR IIF(!EMPTY(laOGFxFlt[2,6]),SUBSTR(CREDIT.REFERENCE,1,6) $ laOGFxFlt[2,6],.T.) AND TRANTYPE='5' AND TRAN=REPCOMM.TRAN
    IF FOUND()
      *B126350,3 MMR 10/19/2005 2)Fixing bug of showing credit adjustments if the user enters wrong
      *B126350,3 MMR              division in the reference field.[Start]   
      lcAliass=ALIAS()
      SELECT CODES
      lcOrd=ORDER()
      SET ORDER TO Ccode_no
      IF SEEK ('N'+'CDIVISION '+SUBSTR(CREDIT.REFERENCE,1,6)) 
      *B126350,3 MMR.[End]
        IF BETWEEN(DTOS(REPCOMM.DATE),lcFrom,lcTo)
          IF ASCAN( laAccPrnt , REPCOMM.ACCOUNT) = 0 AND IIF(!EMPTY(laOGFxFlt[2,6]),SUBSTR(CREDIT.REFERENCE,1,6) $ laOGFxFlt[2,6],.T.)
            lnItmPos = ASCAN(laRstAccCb,REPCOMM.ACCOUNT)
            IF lnItmPos>0 
              lnItmPos = ASUBSCRIPT(laRstAccCb,lnItmPos,1)    
              laRstAccCb[lnItmPos,2] = laRstAccCb[lnItmPos,2] + (-1*CREDIT.AMOUNT)
            ELSE
              lnDim = ALEN(laRstAccCb,1) + 1
              DIMENSION laRstAccCb[lnDim,2]
              laRstAccCb[lnDim,1] = REPCOMM.ACCOUNT
              laRstAccCb[lnDim,2] = (-1*CREDIT.AMOUNT)
            ENDIF
          ENDIF
        ENDIF  
      *B126350,3 MMR 10/19/2005 2)Fixing bug of showing credit adjustments if the user enters wrong
      *B126350,3 MMR              division in the reference field.[Start]   
      ENDIF 
      SELECT CODES
      SET ORDER TO (lcOrd)
      SELECT(lcAliass)
      *B126350,3 MMR.[End]
   ENDIF  
 ENDIF     
ENDSCAN
*B126350,2 MMR.[End]

SELECT(lcAlias)
=SEEK(lcKey)
  
*--ENd of lfRstAcBak.
*B126350,1 BWA 02/17/2005.[END]

