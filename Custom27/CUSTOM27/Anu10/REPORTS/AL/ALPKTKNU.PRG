*:******************************************************************************************
*:Program file : alpktknu.PRG
*:Program desc. : Pick Ticket Form for A'nue
*:System : Sales order allocation
*:Developer : Ahmed Abdel Naby- (AAN) Due to C#102022
*:Date : 06/11/2000
*:******************************************************************************************
*:Calls: 
*:		Functions: 
*:******************************************************************************************
*:Passed Parameters:
*:******************************************************************************************
*:Example : Do alpktknu
*:******************************************************************************************
*: Modifications :
*: B605947,1 BWA 12/05/2002 Fix the alignment of several fields in the report.
*: C037869,1 NNA 03/14/2004 Add new Additional Size To the form to become (P - S - M - L - L1 - L2)
***************************************************************************
SELECT (lcTmpOrdL)
LOCATE
IF EOF()
  llNoRec = .T.
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
ELSE
  = (TYPE("lnMajSeg") = "C") AND lfEvalSegs()
  llNoRec = .F.
  DO lpPrintRep
ENDIF
*-- end of main report code.

*!*******************************************************************************************
*! Name : lpPrintRep
*! Developer : Ahmed Abdel Naby (AAN)
*! Date : 06/11/2000
*! Purpose : 
*!*******************************************************************************************
*! Calls :
*!*******************************************************************************************
*! Passed Parameters :
*!*******************************************************************************************
*! Return : 
*!*******************************************************************************************
*! Example : lpPrintRep
*!*******************************************************************************************

PROCEDURE lpPrintRep
*-- Variables declaration section [Starts
STORE 0.00 TO XORDTOT, XVALUE , SVALUE
*-- Variables declaration section [Ends

lcCustPoVl  = ''

*-- MAIN LOOP
llEndPT = .F.
NEWDOC  = .T.

lnNotLine = 1
SET DEVICE TO PRINT

lcStatuBar = SET("STATUS BAR")
lcMessIs  = SET("MESSAGE",1)

SET STATUS BAR ON
SET MESSAGE TO 'Printing - Press << Space Bar >> to abort'

DECLARE  laBtAddr[3,1] , laStAddr[3,1]
STORE "" TO laBtAddr , laStAddr

llSkipVe = .F.

lnInvNo = 22  &&new
SELECT (lcTmpOrdL)
LOCATE
SCAN REST WHILE INKEY()<> 32
  SELECT (lcTmpOrdL)
  *-- GET ORDERHDR & LINE ITEMS
  IF NEWDOC
    STORE 0 TO lnTotPik,lnTotAmt,lnSubPik,lnSubAmt  
    WAIT WINDOW "Printing Pick Ticket : " + PIKTKT NOWAIT
    XPIKTKT  = PIKTKT
    XORDER   = ORDER
    XDATE    = PIKDATE
    XSTORE    = &lcTmpOrdL..STORE

    IF llMultWare
  	  lcWDesc = 'WARHSE: '+ WareHous.cWareCode+''+SUBSTR(WareHous.cDesc,1,18)
  	ELSE
  	  lcWDesc = SPACE(1)
  	ENDIF

    SELECT ORDHDR
    XMULTI   = MULTI
    XACCOUNT = ACCOUNT

    lcCustPoVl = IIF(ORDHDR.MultiPO,&lcTmpOrdL..CustPO,ORDHDR.CustPO)

    STORE 0.00 TO XORDTOT, XVALUE , SVALUE

    SELECT ORDLINE
    SET ORDER TO ordlinst
    = SEEK("O" + XORDER+XSTORE)

    llOrdNot = .T.
    PRTERR = 0
    STORE 0.00 TO XPIECES,XSUBTOTAL

    XBTNAME  = CUSTOMER.BTNAME

    = gfGetAdr('CUSTOMER' , '' , '' , '' , @laBtAddr , '2')
    IF LEN(TRIM(laBtAddr[2])) =0
       laBtAddr[2] = laBtAddr[3]
       laBtAddr[3] = ''
    ENDIF

    STORE "" TO XSTNAME,XSTADDR1,XSTADDR2,XSTADDR3,XSTADDR4
    XPIECES = 0

  ENDIF
  *-- END PICK TKT/ORDER SELECTION.
  
  *-- SHIP-TO ADDRESS FOR THIS STORE
  *- Initialize the alt address if ther is any.
  SELECT OrdHdr
  
  IF Alt_ShpTo
  	XSTNAME  = OrdHdr.STName   
    XSTADDR1 = OrdHdr.cAddress1
    XSTADDR2 = OrdHdr.cAddress2
    XSTADDR3 = TRIM(OrdHdr.cAddress3) + ' ' + TRIM(OrdHdr.cAddress4) + ' ' + OrdHdr.cAddress5
    
    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ELSE
    SELECT Customer
    lnRecNo = RECNO()       && Variable to hold the record number.
    XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
    = gfGetAdr('CUSTOMER' , '' , '' , '' , @laStAddr)
    XSTADDR1 = laStAddr[1]
    XSTADDR2 = laStAddr[2]
    XSTADDR3 = laStAddr[3]

    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ENDIF
  SELECT ORDHDR
  *-- START PRINTING
  IF lnInvNo = 22
    *B605947,1 BWA 12/05/2002 Fix the alignment of several fields in the report.[START]
    *lnHdrRow = 05                   && Start header row.
    *lnLinRow = 20                   && Start lines printing row.
    *lnBotRow = 39                   && Start footer row.
    **MaxRow   = lnBotRow - 3         && Max row number for lines printing.    
    *MaxRow   = lnBotRow - 11         && Max row number for lines printing.    
    *lnInvNo  = 0

    lnHdrRow = 03                   && Start header row.
    lnLinRow = 18                   && Start lines printing row.
    lnBotRow = 38                   && Start footer row.
    MaxRow   = lnBotRow - 11        && Max row number for lines printing.    
    lnInvNo  = 0
    *B605947,1 BWA 12/05/2002.[END]

  ELSE

    *B605947,1 BWA 12/05/2002 Fix the alignment of several fields in the report.[START]
    *lnHdrRow = lnBotRow + 17        && Start header row.
    *lnLinRow = lnHdrRow + 15        && Start lines printing row.
    *lnBotRow = lnLinRow + 19        && Start footer row.
    *MaxRow   = lnBotRow - 11        && Max row number for lines printing.

    lnHdrRow = lnBotRow + 16        && Start header row.
    lnLinRow = lnHdrRow + 15        && Start lines printing row.
    lnBotRow = lnLinRow + 20        && Start footer row.
    MaxRow   = lnBotRow - 11        && Max row number for lines printing.
    *B605947,1 BWA 12/05/2002.[END]

    lnInvNo  = lnInvNo  + 01
  ENDIF
  
  DO lpHrdLabls

  *-- LINE LOOP  
  SELECT (lcTmpOrdL)
  NEWDOC  = .T.

  lnLineNo = 0
  DO lpAplyLins	  

  IF NewDoc
    IF llSkipVe
      llSkipVe = .F.
      SELECT (lcTmpOrdL)
      SKIP -1
    ENDIF
    lnNotLine = 1
    llEndPT = .T.
    DO lpNewFoter  
  ELSE
    llEndPT = .F.
    @ lnLinRow,12 SAY 'C O N T I N U E D ...'
    *-- Print subtotal for each page. [Start]
    @ lnLinRow,50 SAY 'S U B T O T A L : '

    *B605947,1 BWA 12/05/2002 Fix the alignment of several fields in the report.[START]
    *@ lnLinRow,86 SAY lnSubPik PICTURE '99999'  
    *@ lnLinRow,95 SAY lnSubAmt PICTURE '999,999.99'  

    @ lnLinRow,71 SAY lnSubPik PICTURE '99999'
    @ lnLinRow,86 SAY lnSubAmt PICTURE '999,999.99'
    *B605947,1 BWA 12/05/2002.[END]

    STORE 0 TO lnSubPik,lnSubAmt
    *-- Print subtotal for each page. [End]
    =lfPrntFot()
  lnLinRow = lnLinRow + 1
  NEWDOC = .T.
  LOOP  
 ENDIF
ENDSCAN
WAIT CLEAR

SET MESSAGE TO lcMessIs
SET STATUS BAR &lcStatuBar

SET DEVICE TO SCREEN
RETURN
*-- 

*!*************************************************************
*! Name : lpHrdLabls.
*! Auth : Ahmed Abdel Naby (AAN)
*! Date : 06/11/2000
*!*************************************************************
*! Synopsis : Print the Header labels.
*!*************************************************************
*! Called from : 
*!         Program : alpktkog
*!*************************************************************
PROCEDURE lpHrdLabls

@ lnHdrRow -1,23 SAY XACCOUNT  
  
lnHdrRow = lnHdrRow + 3

@ lnHdrRow -1,15 SAY XBTNAME  
@ lnHdrRow -1,68 SAY XSTNAME
  
lnHdrRow = lnHdrRow + 1
  
@ lnHdrRow -1,15 SAY laBtAddr[1]  
@ lnHdrRow -1,68 SAY XSTADDR1  
  
lnHdrRow = lnHdrRow + 1
  
@ lnHdrRow -1,15 SAY laBtAddr[2]  
@ lnHdrRow -1,68 SAY XSTADDR2  

lnHdrRow = lnHdrRow + 1

@ lnHdrRow -1,15 SAY laBtAddr[3]  
@ lnHdrRow -1,68 SAY XSTADDR3  

lnHdrRow = lnHdrRow + 3

*B605947,1 BWA 12/05/2002 Fix the alignment of several fields in the report.[START]
*@ lnHdrRow -1, 00 SAY XORDER
*@ lnHdrRow -1, 10 SAY XPIKTKT
*@ lnHdrRow -1, 18 SAY XDATE
*@ lnHdrRow -1, 31 SAY (lcCustPoVl
*@ lnHdrRow -1, 49 SAY ORDHDR.DEPT
*@ lnHdrRow -1, 56 SAY OrdHdr.ShipVia  
*@ lnHdrRow -1, 63 SAY OrdHdr.Rep1+' '+OrdHdr.Rep2
*@ lnHdrRow -1, 78 SAY SUBSTR(gfCodDes(ORDHDR.CTERMCODE,'CTERMCODE'),1,11)
*@ lnHdrRow -1, 91 SAY ORDHDR.START
*@ lnHdrRow -1, 101 SAY ORDHDR.COMPLETE  
*@ lnHdrRow -1, 112 SAY gfCodDes(ORDHDR.Season,'SEASON')
*lnHdrRow = lnHdrRow + 2
*@ lnHdrRow -1, 02 SAY OrdHdr.Note1  
*lnHdrRow = lnHdrRow + 1
*@ lnHdrRow -1, 02 SAY OrdHdr.Note2  
*lnHdrRow = lnHdrRow + 1
*@ lnHdrRow -1, 45 SAY "     4      6      8     10      12"

@ lnHdrRow -1, 02 SAY XORDER
@ lnHdrRow -1, 10 SAY XPIKTKT  
@ lnHdrRow -1, 17 SAY XDATE
@ lnHdrRow -1, 28 SAY LEFT(lcCustPoVl,14)
@ lnHdrRow -1, 43 SAY ORDHDR.DEPT
@ lnHdrRow -1, 48 SAY OrdHdr.ShipVia
@ lnHdrRow -1, 55 SAY OrdHdr.Rep1 + ' ' + OrdHdr.Rep2
@ lnHdrRow -1, 67 SAY SUBSTR(gfCodDes(ORDHDR.CTERMCODE,'CTERMCODE'),1,11)
@ lnHdrRow -1, 78 SAY ORDHDR.START
@ lnHdrRow -1, 87 SAY ORDHDR.COMPLETE  
@ lnHdrRow -1, 96 SAY gfCodDes(ORDHDR.Season,'SEASON')

lnHdrRow = lnHdrRow + 2
@ lnHdrRow -1, 02 SAY OrdHdr.Note1  
lnHdrRow = lnHdrRow + 1
@ lnHdrRow -1, 02 SAY OrdHdr.Note2  
lnHdrRow = lnHdrRow + 1
 *C037869,1 NNA 03/14/2004 (Begin) Change the Position of the sizes to add a new size
 *@ lnHdrRow -1, 41 SAY "    4    6    8    10     12"
 @ lnHdrRow -1, 41 SAY "  4   6    8    10   12   14"
 * C037869,1 NNA (End)
*B605947,1 BWA 12/05/2002.[END]
*-- [1] LINE ITEM PRINT LOOP
XSTORE   = STORE
XSCALE   =  ' '
  
*-- PRINT SCALE
lnHdrRow = lnHdrRow + 1

*--End of lpHrdLabls.
*!*************************************************************
*! Name : lpNewFoter.
*! Auth : Ahmed Abdel Naby (AAN)
*! Date : 06/11/2000
*!*************************************************************
*! Synopsis : Print the form footer at the end of a spacific 
*!            pick ticket. 
*!*************************************************************
*! Called from : 
*!         Program : 
*!*************************************************************
PROCEDURE lpNewFoter
lnLinRow = lnLinRow + 1
@ lnLinRow -1,02 SAY lcRpMsg1          
lnLinRow = lnLinRow + 1
@ lnLinRow -1,02 SAY lcRpMsg2          
lnLinRow = lnLinRow + 1
@ lnLinRow -1,02 SAY lcRpMsg3          
lnLinRow = lnLinRow + 1
@ lnLinRow -1,50 SAY "T O T A L"   

*B605947,1 BWA 12/05/2002 Fix the alignment of several fields in the report.[START]
*@ lnLinRow -1,86 SAY lnTotPik PICTURE '99999'  
*@ lnLinRow -1,95 SAY lnTotAmt PICTURE '999,999.99'  

@ lnLinRow -1,71 SAY lnTotPik PICTURE '99999'
@ lnLinRow -1,86 SAY lnTotAmt PICTURE '999,999.99'
*B605947,1 BWA 12/05/2002.[END]

=lfPrntFot()
*-- End of lpNewFoter.

*:***************************************************************************
*: Name        : lfEvalSegs
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 06/11/2000
*: Purpose     : Evaluate NonMajor Type and variables.
*:***************************************************************************
*: Called from : [Option Grid] lcDummy variable.
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfEvalSegs()
*:***************************************************************************
*
FUNCTION lfEvalSegs
PARAMETER lcReturn
STORE 0  TO  lnMajSeg,lnNonMajSt,lnMajorLen,lnFreeLen,lnColorLen
STORE "" TO lcMajPict,lcFree_Clr,lcNonMajPi,lcNonMajTl,lcColorTlt

lnMajSeg    = gfItemMask('SM')  && No. of major segments.
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcMajPict  = gfItemMask("PM")
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
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
lnMajorLen = LEN(lcMajPict)
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen

IF EMPTY (lcNonMajTl)
  lcColorTlt = 'Color'
ELSE 
  lcColorTlt = ALLTRIM(lcNonMajTl)
ENDIF
*-- end of lfEvalSegs.

*!*******************************************************************************************
*! Name : lpAplyLins
*! Developer : Ahmed Abdel Naby (AAN)
*! Date : 06/11/2000
*! Purpose : Print PikTkt data
*!*******************************************************************************************
*! Calls :
*!*******************************************************************************************
*! Passed Parameters :
*!*******************************************************************************************
*! Return : 
*!*******************************************************************************************
*! Example : lpAplyLins
*!*******************************************************************************************

PROCEDURE lpAplyLins

lnLineNo = lnLineNo + 1

lcOrdHKey = OrdHdr.cOrdType + OrdHdr.Order

*-- Scan lcTmpOrdL.
SCAN REST WHILE PikTkt + Order + cGrupDetal + STR(LineNo , 6) = ;
                XPIKTKT + XORDER FOR  LINENO <> 0
  
  IF SEEK(&lcTmpOrdL..Style,"Style")
    XSTYDESC   = Style.DESC
    lcStyLocat = Style.Location
    lcScale    = Style.Scale
  ELSE
    XSTYDESC   = ''
    lcStyLocat = ''
    lcScale    = ''
  ENDIF

  *B605947,1 BWA 12/05/2002 Fix the alignment of several fields in the report.[START]
  *@ lnLinRow -1,00 SAY ALLTRIM(STR(LINENO,4))  
  *@ lnLinRow -1,05 SAY SUBSTR(Style,1,7)  

  @ lnLinRow -1,01 SAY ALLTRIM(STR(LINENO,4))  
  @ lnLinRow -1,07 SAY SUBSTR(Style,1,7)  
  *B605947,1 BWA 12/05/2002.[END]

  @ lnLinRow -1,15 SAY SUBSTR(Style,lnNonMajSt,3)

  @ lnLinRow -1,19 SAY XSTYDESC  &&@ lnLinRow,17 SAY XSTYDESC

  *B605947,1 BWA 12/05/2002 Fix the alignment of several fields in the report.[START]
  *@ lnLinRow -1,47 SAY IIF(pik1 = 0,SPACE(4),pik1)  PICTURE '9999'  
  *@ lnLinRow -1,54 SAY IIF(pik2 = 0 ,SPACE(4),pik2) PICTURE '9999'  
  *@ lnLinRow -1,61 SAY IIF(pik3 = 0,SPACE(4),pik3) PICTURE '9999'  
  *@ lnLinRow -1,67 SAY IIF(pik4 =0,SPACE(4),pik4)  PICTURE '9999'  
  *@ lnLinRow -1,75 SAY IIF(pik5 + pik6 + pik7 + pik8 = 0,SPACE(5),pik5 + pik6 + pik7 + pik8)  PICTURE '99999'

  *C037869,1 NNA 03/14/2004 (Begin) Add new Additional Size To the form to become (P - S - M - L - L1 - L2)
  *@ lnLinRow -1,41 SAY IIF(pik1 = 0 , SPACE(4) , pik1 )  PICTURE '9999'  
  *@ lnLinRow -1,48 SAY IIF(pik2 = 0 , SPACE(4) , pik2 )  PICTURE '9999'  
  *@ lnLinRow -1,54 SAY IIF(pik3 = 0 , SPACE(4) , pik3 )  PICTURE '9999'  
  *@ lnLinRow -1,60 SAY IIF(pik4 = 0 , SPACE(4) , pik4 )  PICTURE '9999'  
  *@ lnLinRow -1,66 SAY IIF(pik5 + pik6 + pik7 + pik8 = 0 , SPACE(5) , pik5 + pik6 + pik7 + pik8)  PICTURE '99999'
  *B605947,1 BWA 12/05/2002.[END]
  @ lnLinRow -1,41 SAY IIF(pik1 = 0 , SPACE(4) , pik1 )  PICTURE '9999'  
  @ lnLinRow -1,46 SAY IIF(pik2 = 0 , SPACE(4) , pik2 )  PICTURE '9999'  
  @ lnLinRow -1,51 SAY IIF(pik3 = 0 , SPACE(4) , pik3 )  PICTURE '9999'  
  @ lnLinRow -1,56 SAY IIF(pik4 = 0 , SPACE(4) , pik4 )  PICTURE '9999'  
  @ lnLinRow -1,61 SAY IIF(pik5 = 0 , SPACE(4) , pik5 )  PICTURE '9999'    
  @ lnLinRow -1,66 SAY IIF(pik6 + pik7 + pik8 = 0 , SPACE(5) , pik6 + pik7 + pik8)  PICTURE '99999'
  *C037869,1 NNA (End)

  IF TOTPIK <> 0
    *B605947,1 BWA 12/05/2002 Fix the alignment of several fields in the report.[START]
    *@ lnLinRow -1,81 SAY TOTPIK   PICTURE '99999'
    @ lnLinRow -1,71 SAY TOTPIK   PICTURE '99999'
    *B605947,1 BWA 12/05/2002.[END]
  ENDIF

  XAMOUNT = PRICE*TOTPIK
  *B605947,1 BWA 12/05/2002 Fix the alignment of several fields in the report.[START]
  *IF llRpStyPrc
  *  @ lnLinRow -1,88 SAY PRICE        PICTURE '9999.99'
  *ENDIF
  *@ lnLinRow -1,95 SAY XAMOUNT      PICTURE '999,999.99'

  IF llRpStyPrc
    @ lnLinRow -1,77 SAY PRICE        PICTURE '9999.99'
  ENDIF
  @ lnLinRow -1,86 SAY XAMOUNT      PICTURE '999,999.99'
  *B605947,1 BWA 12/05/2002.[END]

  lnTotPik = lnTotPik + TOTPIK
  lnTotAmt = lnTotAmt + XAMOUNT
  lnSubPik = lnSubPik + TOTPIK
  lnSubAmt = lnSubAmt + XAMOUNT
  lnLinRow = lnLinRow + 1
    
  DO CASE
    CASE EOF()
      NEWDOC=.T.
      EXIT
    CASE totpik<=0
      SKIP
      LOOP
    CASE lnLinRow >= MaxRow
      NEWDOC=.F.
      EXIT
  ENDCASE    

ENDSCAN
llSkipVe = .T.

*-- END PRINT LINE LOOP
=SEEK(lcOrdHKey,"ORDHDR")
*-- end of lpAplyLins.


*!*************************************************************
*! Name      : lfPrntFot
*! Auth      : Ahmed Abdel Naby (AAN)
*! Date      : 06/11/2000
*! Purpose   : Print report footer.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
FUNCTION lfPrntFot

@ lnBotRow-1,35 SAY 'UPS# FL 378-419'
@ lnBotRow-1,100 SAY 'UPS# FL 378-419'

@ lnBotRow+2,10 SAY XACCOUNT 
@ lnBotRow+2,77 SAY XACCOUNT 

@ lnBotRow+3 , 10 SAY XSTNAME
@ lnBotRow+3 , 77 SAY XSTNAME

@ lnBotRow+4 , 10 SAY XSTADDR1
@ lnBotRow+4 , 77 SAY XSTADDR1

@ lnBotRow+5 , 10 SAY XSTADDR2
@ lnBotRow+5 , 77 SAY XSTADDR2

@ lnBotRow+6 , 10 SAY XSTADDR3
@ lnBotRow+6 , 77 SAY XSTADDR3

@ lnBotRow+8 , 23 SAY lcCustPoVl
@ lnBotRow+8 , 39 SAY ORDHDR.DEPT
@ lnBotRow+8 , 47 SAY OrdHdr.ShipVia

*B605947,1 BWA 12/05/2002 Fix the alignment of several fields in the report.[START]
*@ lnBotRow+8 , 89 SAY lcCustPoVl
*@ lnBotRow+8 , 108 SAY ORDHDR.DEPT
*@ lnBotRow+8 , 115 SAY OrdHdr.ShipVia

@ lnBotRow+8 , 76 SAY lcCustPoVl
@ lnBotRow+8 , 93 SAY ORDHDR.DEPT
@ lnBotRow+8 , 100 SAY OrdHdr.ShipVia
*B605947,1 BWA 12/05/2002.[END]

*--End of lfPrntFot.