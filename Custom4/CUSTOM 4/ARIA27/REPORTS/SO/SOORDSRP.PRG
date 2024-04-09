*:***************************************************************************
*: Program file  : SOORDSRP.PRG
*: Program desc. : ORDER STATUS REPORT.
*: Date          : 12/29/2002
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER (SO)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tester        : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: N000494
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO SOORDSRP
*:***************************************************************************
*: Modifications :
*:B121684,1 NNA 02/16/2004 Fix bug that the report is so slowly to run 
*:B608294,1 NNA 27/09/2007 (T20070824.0041) Fix bug that report display all orders type
*:B608294,1 NNA            include Contracts and EDI Temporary orders 
***************************************************************************

*--The Style length.
STORE 0 TO lnLenth
lnLnthSty = LEN(gfItemMask('PM'))

*--Section initial the wanted variables.
STORE SPACE(0) TO lcDatPos , lcEntPos , lcComPos , lcStrPos , lcStyPos

*--Case get the value of the Special Instructions.
lnClassPo = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.SPCINST'),1)
lcCodeinst = laOGFxFlt[lnClassPo,6]

*--Case get the value of the Order(S).
lnOrdrPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.ORDER'),1)
IF lnOrdrPos > 0
  lnDatPos = AT('INLIST(ORDHDR.CORDTYPE+ORDHDR.ORDER',LCRPEXP)

  IF lnDatPos > 0
    lnPos1     = AT('AND' , SUBSTR(LCRPEXP,lnDatPos))
    IF lnPos1 > 0
      lcDatPos = SUBSTR(lcRpExp ,lnDatPos , lnPos1-1)
    ELSE
      lcDatPos = SUBSTR(lcRpExp ,lnDatPos)
    ENDIF
  ENDIF
ENDIF

*--Case get the value of the Entered date.
lnEntrPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.ENTERED'),1)
IF lnEntrPos > 0
  lnEntPos = AT('BETWEEN(DTOS(ORDHDR.ENTERED',LCRPEXP)

  IF lnEntPos > 0
    lnPos1     = AT('AND' , SUBSTR(LCRPEXP,lnEntPos))
    IF lnPos1 > 0
      lcEntPos = SUBSTR(lcRpExp ,lnEntPos , lnPos1-1)
    ELSE
      lcEntPos = SUBSTR(lcRpExp ,lnEntPos)
    ENDIF
  ENDIF
ENDIF

*--Case get the value of the Complete date.
lnCompPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.COMPLETE'),1)
IF lnCompPos > 0
  lnComPos = AT('BETWEEN(DTOS(ORDHDR.COMPLETE',LCRPEXP)

  IF lnComPos > 0
    lnPos1     = AT('AND' , SUBSTR(LCRPEXP,lnComPos))
    IF lnPos1 > 0
      lcComPos = SUBSTR(lcRpExp ,lnComPos , lnPos1-1)
    ELSE
      lcComPos = SUBSTR(lcRpExp ,lnComPos)
    ENDIF
  ENDIF
ENDIF

*--Case get the value of the Start date.
lnStrtPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.START'),1)
IF lnStrtPos > 0
  lnStrPos = AT('BETWEEN(DTOS(ORDHDR.START',LCRPEXP)

  IF lnStrPos > 0
    lnPos1     = AT('AND' , SUBSTR(LCRPEXP,lnStrPos))
    IF lnPos1 > 0
      lcStrPos = SUBSTR(lcRpExp ,lnStrPos , lnPos1-1)
    ELSE
      lcStrPos = SUBSTR(lcRpExp ,lnStrPos)
    ENDIF
  ENDIF
ENDIF

*--Case get the value of the Style(s).
lnStylPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.STYLE'),1)
IF lnStylPos > 0
  lnStyPos = AT('INLIST(STYLE.STYLE',LCRPEXP)

  IF lnStyPos > 0
    lnPos1     = AT('AND' , SUBSTR(LCRPEXP,lnStyPos))
    IF lnPos1 > 0
      lcStyPos = SUBSTR(lcRpExp ,lnStyPos , lnPos1-1)
    ELSE
      lcStyPos = SUBSTR(lcRpExp ,lnStyPos)
    ENDIF
  ENDIF
ENDIF

*--Section get the value of the lcFilter.

*B608294,1 NNA 27/09/2007 (Begin) Exclude Contracts and EDI Temporary orders 
*lcFilter = "WHERE ORDLINE.ORDER = ORDHDR.ORDER"
lcFilter = "WHERE ORDLINE.ORDER = ORDHDR.ORDER AND ORDHDR.CORDTYPE='O'"
*B608294,1 NNA (End)

IF !EMPTY(lcEntPos)
  lcFilter = lcFilter + ' .AND. ' +  lcEntPos
ENDIF

IF !EMPTY(lcComPos)
  lcFilter = lcFilter + ' .AND. ' +  lcComPos
ENDIF

IF !EMPTY(lcStrPos)
  lcFilter = lcFilter + ' .AND. ' +  lcStrPos
ENDIF

IF !EMPTY(lcStyPos)
  lcStyPos = STRTRAN(lcStyPos , "STYLE.STYLE" , "LEFT(ORDLINE.STYLE,lnLnthSty)" )
  lcFilter = lcFilter + ' .AND. ' +  lcStyPos
ENDIF

*--Section get the value of the lcFilter1.
lcFilter1  = SPACE(0)
IF !EMPTY(lcAccount)
  lcFilter1 = lcFilter1 + ' .AND. ' +  ' ORDHDR.ACCOUNT = lcAccount'
ENDIF

IF !EMPTY(lcCodeinst)
  lcFilter1 = lcFilter1 + ' .AND. ' + ' ORDHDR.SPCINST = lcCodeinst'
ENDIF

IF !EMPTY(lcDatPos)
  lcFilter1 = lcFilter1 + ' .AND. ' + lcDatPos
ENDIF
*-- Create the temp file and put data in it.
*B121684,1 NNA 02/16/2004 (Begin) Get the (ordline.totqty) and (ordline.price) in the temp file;
						  and correct the (Group & Order) Section to group by (LEFT(ORDLINE.STYLE,lnLnthSty))
*SELECT DISTINCT ORDHDR.ORDER,ORDHDR.ACCOUNT,ORDHDR.CUSTPO,ORDHDR.START                ,;
                ORDHDR.COMPLETE,ORDHDR.ENTERED,LEFT(ORDLINE.STYLE,lnLnthSty) AS STYLE  ;
       FROM ORDHDR,ORDLINE                                                             ;
       &lcFilter.&lcFilter1                                                            ;
       GROUP BY ORDHDR.COMPLETE,ORDHDR.ORDER,STYLE                                     ;
       ORDER BY ORDHDR.COMPLETE,ORDHDR.ORDER,STYLE                                     ;
       INTO DBF &gcWorkDir.&LstTemp

SELECT Distinct ORDHDR.ORDER, ORDHDR.ACCOUNT, ORDHDR.CUSTPO, ORDHDR.START, ORDHDR.COMPLETE ,;
				ORDHDR.ENTERED, LEFT(ORDLINE.STYLE,lnLnthSty) AS STYLE					   ,;
				SUM(ORDLINE.TOTQTY) AS TOTQTY, ORDLINE.PRICE							    ;
       FROM ORDHDR,ORDLINE				                                              		;
       			&lcFilter.&lcFilter1 														;
       				AND	Ordhdr.status $'OH' 												;
       				AND ORDLINE.TOTQTY>0													;
       GROUP BY ORDHDR.COMPLETE,ORDHDR.ORDER,7 			                             		;
       ORDER BY ORDHDR.COMPLETE,ORDHDR.ORDER,7												;
       INTO DBF &gcWorkDir.&LstTemp

	INDEX ON (ORDER+STYLE) TAG ORDSTY
	SET ORDER TO ORDSTY
	SET RELATION TO order INTO Invline ADDITIVEM
	SELECT INVLINE
	SET ORDER TO TAG INVLINEO
	SET RELATION TO INVOICE INTO INVHDR ADDITIVE
	SELECT (LSTTEMP)
*B121684,1 NNA (End)
*-- If there were nothing selected from the SQL then return.
IF RECCOUNT() = 0 .OR. EOF()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF

SET DEVICE TO PRINT
DO lpPrint
DO ENDREPORT
SET DEVICE TO SCREEN
WAIT CLEAR

*--Call the function of deleting the temprory files.
=lfBasToClr(LstTemp , 'F')
=lfBasToClr(lcCursName , 'F')

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lpPrint
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/29/2002
*! Purpose   : Print report.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpPrint
*!*************************************************************
PROCEDURE lpPrint

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+...
*                                                     Date                                                                        
*       Order    Custmer                              Required Units   Units   Units   Recvd   Recvd   Units   Units   Open           
*Order# Date     Style                     Cust. P/O# Cancel   Ordered Issued  Cut     1rst.   2nds.   Shipped Balance Balance         
*================================================================================================================================
*123456 99/99/99 1234567890123456789012345 1234567890 99/99/99 1234567 1234567 1234567 1234567 1234567 1234567 1234567 123456.99
*                1234567 12345678901234567890         99/99/99                                                                     
*                                                              ------- ------- ------- ------- ------- ------- ------- ---------
*                                        ***** Weekly Total:   1234567 1234567 1234567 1234567 1234567 1234567 1234567 123456.99
*                                                              ------- ------- ------- ------- ------- ------- ------- ---------
*                                                              ------- ------- ------- ------- ------- ------- ------- ---------
*                                        ***** Monthly Total:  1234567 1234567 1234567 1234567 1234567 1234567 1234567 123456.99
*                                                              ------- ------- ------- ------- ------- ------- ------- ---------
*                                                              ******* ******* ******* ******* ******* ******* ******* *********
*                                        ***** Total Overall:  1234567 1234567 1234567 1234567 1234567 1234567 1234567 123456.99
*                                                              ******* ******* ******* ******* ******* ******* ******* *********
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+...

*-- Declare the necessary printing vars.
R_Title = 'ORDER STATUS REPORT'
R_Width = 'W'
lnRow   = 99
PageNo  = 0

*--As Ver2.6 Store Complete date. 
STORE COMPLETE TO ldWekTot ,ldMonTot

DIMENSION laWek (8) ,laMon (8) ,laGrn (8)
STORE 0 TO laWek,laMon,laGrn

*-- Collect cut tickets information from the CutPick file.
SELECT DIST CutPick.Order,cTktNo,CutPick.TotQty,LEFT(CutPick.Style,lnLnthSty) AS STYLE ;
       FROM CutPick,(lstTemp)                                                          ;
       WHERE CutPick.Order = &lstTemp..Order                                           ;
       INTO DBF &gcWorkDir.&lcCursName

INDEX ON ORDER + CTKTNO TAG (lcCursName) OF (gcWorkDir + lcCursName)

SELECT (LstTemp)
LOCATE

*-- Print LIST details
DO lpDetails

*-- Print the grand total after week total and month total.
DO lpWekSub

*lnRow = lnRow + 1
DO lpMonSub

lnRow = lnRow + 1
DO lpgrnd 

*--End of lpPrint.
*!*************************************************************
*! Name      : lpDetails
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/29/2002
*! Purpose   : check the time between START (ldWekTot and ldMonTot)
*!            and Print the details .
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpDetails
*!*************************************************************
PROCEDURE lpDetails

ldBrkDate = lfGetNxtBrk(Complete)

SCAN
  WAIT WINDOW 'Selecting Records For Order...' + ORDER NOWAIT
  STORE 0 TO lnOrdTot ,lnIssTot ,lnCutTot ,ln1stTot ,;
             ln2ndTot ,lnShpTot ,lnUbaTot ,lnBalTot

  IF Complete > ldBrkDate
    DO lpWekSub
    ldBrkDate  = lfGetNxtBrk(Complete)
    llWekPrntd = .T.
  ELSE
    llWekPrntd = .F.
  ENDIF

 IF MONTH(COMPLETE) <> MONTH(ldMonTot)
    IF !llWekPrntd
      DO lpWekSub
      ldBrkDate = lfGetNxtBrk(Complete)
      *lnRow = lnRow + 1
    ENDIF
    DO lpMonSub
  ENDIF

  IF lnRow >= 51
    DO lpHdrPrnt WITH lnRow
  ENDIF

  SELECT (LSTTEMP)
  lnRow = lnRow + 1
  @ lnRow ,001 SAY ORDER
  @ lnRow ,008 SAY ENTERED
  @ lnRow ,017 SAY IIF(SEEK('M'+ACCOUNT,'CUSTOMER')            ,;
                       SUBSTR(CUSTOMER->BTNAME,1,25),SPACE(25))
  @ lnRow ,043 SAY LEFT(CUSTPO,10)
  @ lnRow ,054 SAY START

  DO lpOrder
  @ lnRow ,063 SAY lnOrdTot PICTURE '9999999'

  DO lpIssud

  @ lnRow ,070 SAY lnIssTot PICTURE '9999999'
  @ lnRow ,078 SAY lnCutTot PICTURE '9999999'
  @ lnRow ,086 SAY ln1stTot PICTURE '9999999'
  @ lnRow ,094 SAY ln2ndTot PICTURE '9999999'
  @ lnRow ,103 SAY lnShpTot PICTURE '9999999'
  @ lnRow ,112 SAY lnUbaTot PICTURE '9999999'
  @ lnRow ,122 SAY lnBalTot PICTURE '999999.99'

  lnRow =lnRow +1

  SELECT (LstTemp)
  IF SEEK(STYLE,'STYLE')
    @ lnRow ,017 SAY LEFT(STYLE,lnLnthSty)
    @ lnRow ,030 SAY STYLE.DESC
  ENDIF
  @ lnRow ,054 SAY COMPLETE
  lnRow =lnRow +1

  laWek[1] = laWek[1] + lnOrdTot
  laWek[2] = laWek[2] + lnIssTot
  laWek[3] = laWek[3] + lnCutTot
  laWek[4] = laWek[4] + ln1stTot
  laWek[5] = laWek[5] + ln2ndTot
  laWek[6] = laWek[6] + lnShpTot
  laWek[7] = laWek[7] + lnUbaTot
  laWek[8] = laWek[8] + lnBalTot

ENDSCAN

*--End of lpDetails.
*!*************************************************************
*! Name      : lpWekSub
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/29/2002
*! Purpose   : print laWek and increment laMon.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpWekSub
*!*************************************************************
PROCEDURE lpWekSub

*--PRINT SUB TOTAL OF WEEK
@ lnRow,000 SAY '                                                               ------- ------- ------- ------- ------- -------  ------- -----------'

lnRow = lnRow + 1

@ lnRow ,041 SAY '***** Weekly Total:'
@ lnRow ,063 SAY laWek[1] PICTURE '9999999'
@ lnRow ,070 SAY laWek[2] PICTURE '9999999'
@ lnRow ,078 SAY laWek[3] PICTURE '9999999'
@ lnRow ,086 SAY laWek[4] PICTURE '9999999'
@ lnRow ,094 SAY laWek[5] PICTURE '9999999'
@ lnRow ,103 SAY laWek[6] PICTURE '9999999'
@ lnRow ,112 SAY laWek[7] PICTURE '9999999'
@ lnRow ,120 SAY laWek[8] PICTURE '99999999.99'

lnRow = lnRow + 1

@ lnRow,000 SAY '                                                               ------- ------- ------- ------- ------- -------  ------- -----------'

*--MOVE laWek TO laMon

laMon[1] = laMon[1] + laWek[1]
laMon[2] = laMon[2] + laWek[2]
laMon[3] = laMon[3] + laWek[3]
laMon[4] = laMon[4] + laWek[4]
laMon[5] = laMon[5] + laWek[5]
laMon[6] = laMon[6] + laWek[6]
laMon[7] = laMon[7] + laWek[7]
laMon[8] = laMon[8] + laWek[8]

ldWekTot = COMPLETE
STORE 0 TO laWek

*--End of lpWekSub.
*!*************************************************************
*! Name      : lpMonSub
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/29/2002
*! Purpose   : print laMon and increment laGrn.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpMonSub
*!*************************************************************
PROCEDURE lpMonSub
lnRow = lnRow + 1
@ lnRow,000 SAY '                                                               ------- ------- ------- ------- ------- -------  ------- -----------'

lnRow = lnRow + 1

@ lnRow ,041 SAY '***** Monthly Total:'
@ lnRow ,063 SAY laMon[1] PICTURE '9999999'
@ lnRow ,070 SAY laMon[2] PICTURE '9999999'
@ lnRow ,078 SAY laMon[3] PICTURE '9999999'
@ lnRow ,086 SAY laMon[4] PICTURE '9999999'
@ lnRow ,094 SAY laMon[5] PICTURE '9999999'
@ lnRow ,103 SAY laMon[6] PICTURE '9999999'
@ lnRow ,112 SAY laMon[7] PICTURE '9999999'
@ lnRow ,120 SAY laMon[8] PICTURE '99999999.99'

lnRow = lnRow + 1

@ lnRow,000 SAY '                                                               ------- ------- ------- ------- ------- -------  ------- -----------'

*--MOVE laMon TO laGrn

laGrn[1] = laGrn[1] + laMon[1]
laGrn[2] = laGrn[2] + laMon[2]
laGrn[3] = laGrn[3] + laMon[3]
laGrn[4] = laGrn[4] + laMon[4]
laGrn[5] = laGrn[5] + laMon[5]
laGrn[6] = laGrn[6] + laMon[6]
laGrn[7] = laGrn[7] + laMon[7]
laGrn[8] = laGrn[8] + laMon[8]

ldMonTot = COMPLETE
STORE 0 TO laMon

*--End of lpMonSub.
*!*************************************************************
*! Name      : lpgrnd
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/29/2002
*! Purpose   : Calculate garand subtotal and print it. 
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpgrnd
*!*************************************************************
PROCEDURE lpgrnd

@ lnRow,000 SAY '                                                               ******* ******* ******* ******* ******* *******  ******* ***********'

lnRow = lnRow + 1

@ lnRow ,041 SAY '***** Total Overall:'
@ lnRow ,063 SAY laGrn[1] PICTURE '9999999'
@ lnRow ,070 SAY laGrn[2] PICTURE '9999999'
@ lnRow ,078 SAY laGrn[3] PICTURE '9999999'
@ lnRow ,086 SAY laGrn[4] PICTURE '9999999'
@ lnRow ,094 SAY laGrn[5] PICTURE '9999999'
@ lnRow ,103 SAY laGrn[6] PICTURE '9999999'
@ lnRow ,112 SAY laGrn[7] PICTURE '9999999'
@ lnRow ,120 SAY laGrn[8] PICTURE '99999999.99'

lnRow = lnRow + 1

@ lnRow,000 SAY '                                                               ******* ******* ******* ******* ******* *******  ******* ***********'

*--End of lpgrnd.
*!*************************************************************
*! Name      : lpHdrPrnt
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/29/2002
*! Purpose   : Print the header of the report.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpHdrPrnt
*!*************************************************************
PROCEDURE lpHdrPrnt
PARAMETER lnRow

PageNo  = PageNo + 1
DO RPT_HDR WITH 'SOORDSRP','' ,R_Width
lnRow = 5

@lnRow ,001 SAY '                                                     Date'
lnRow = lnRow + 1
@ lnROW,001 SAY '       Order    Custmer                              Required Units   Units   Units   Recvd   Recvd   Units    Units       Open '
lnRow = lnRow + 1
@ lnRow,001 SAY 'Order# Date     Style                     Cust. P/O# Cancel   Ordered Issued  Cut     1rst.   2nds.   Shipped  Balance     Balance'         
lnRow = lnRow + 1
@ lnRow,001 SAY REPLICATE ('=',130)

*--End of lpHdrPrnt.
*!*************************************************************
*! Name      : lpIssud
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/29/2002
*! Purpose   : calculate the units (issued,cut,1strecvd,2strecvd)
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpIssud
*!*************************************************************
PROCEDURE lpIssud

*-- Only for manufactued styles
IF (SEEK(&LstTemp..Style,'Style') .AND. !Style.Make) OR OCCURS('MF',gcCmpModules)=0
  RETURN
ENDIF

*-- Get budget, Actualized, Received and Damaged quantities for all cut tickets
*-- computed from the processed order.
SELECT (lcCursName)
SEEK &LstTemp..Order
PRIVATE lcOrder
lcOrder = SPACE(0)

SCAN WHILE Order = &LstTemp..Order
  IF SEEK(cTktNo,'CutTktH') .AND. LEFT(CutTktH.Style,lnLnthSty) = &LstTemp..Style
    lnIssTot = lnIssTot + IIF(CutTktH.Status $ 'OH' , TotQty ,0 )
    lnCutTot = lnCutTot + IIF(CutTktH.Status $ 'AC' , TotQty ,0 )
    IF lcOrder # ORDER
      ln1stTot = ln1stTot + IIF(CutTktH.Status $ 'OHACS',CutTktH.PCS_REC,0)
      ln2ndTot = ln2ndTot + IIF(CutTktH.Status $ 'OHACS',CutTktH.PCS_DAM,0)
      lcOrder = ORDER
    ENDIF
  ENDIF
ENDSCAN

*--End of lpIssud.
*!*************************************************************
*! Name      : lpOrder
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/29/2002
*! Purpose   : calculate the order units And any special notes.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpOrder
*!*************************************************************
PROCEDURE lpOrder
*B121684,1 NNA 02/16/2004 (Begin) get the (lnUbaTot & lnBalTot) from the temp file Directly
*SELECT ORDLINE
*SET ORDER TO TAG ORDLINES
*SET RELATION TO CORDTYPE+ORDER INTO ORDHDR
*IF SEEK(&LstTemp..STYLE)
  *SCAN WHILE LEFT(STYLE,lnLnthSty) + DTOS(COMPLETE) + CORDTYPE  + ORDER + STORE + STR(LINENO,6) = &LstTemp..STYLE ;
       FOR ORDER = &LstTemp..ORDER .AND. ORDHDR.STATUS $ 'OH'
  SELECT (LstTemp)
*B121684,1 NNA (End)  
    lnUbaTot = lnUbaTot + TOTQTY
    lnBalTot = lnBalTot + (TOTQTY * PRICE)
*B121684,1 NNA 02/16/2004 (Begin)
  *ENDSCAN
*ENDIF
*B121684,1 NNA (End)

SELECT INVLINE

*B121684,1 NNA 02/16/2004 (Begin) change the seek item to (order) instead of (Style)
*SET ORDER TO TAG INVLINES
*SET ORDER TO TAG INVLINEO
*SET RELATION TO INVOICE INTO INVHDR ADDITIVE
*IF SEEK(&LstTemp..STYLE)
  *SCAN WHILE LEFT(STYLE,lnLnthSty) + INVOICE  + STR(LINENO,6) = &LstTemp..STYLE ;
       FOR ORDER = &LstTemp..ORDER .AND. INVHDR.STATUS <> 'V'
IF SEEK(&LstTemp..ORDER)
  SCAN WHILE order+STR(lineno,6)+invoice = &LstTemp..ORDER ;
       FOR LEFT(STYLE,lnLnthSty)= &LstTemp..STYLE .AND. INVHDR.STATUS <> 'V'
*B121684,1 NNA (End)

    lnShpTot = lnShpTot + TOTQTY
  ENDSCAN
ENDIF
lnOrdTot = lnUbaTot + lnShpTot

*--End of lpOrder.
*!*************************************************************
*! Name      : lfGetNxtBrk
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/29/2002
*! Purpose   : To get the next friday date.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lfGetNxtBrk
*!*************************************************************
FUNCTION lfGetNxtBrk
PARAMETERS ldDate
PRIVATE ldDate, ldRet
IF EMPTY(ldDate)
  RETURN {}
ENDIF
ldRet = ldDate
DO WHILE UPPER(CDOW(ldRet)) <> "FRIDAY"
  ldRet = ldRet + 1
ENDDO

RETURN ldRet

*--End of lfGetNxtBrk.
*!*************************************************************
*! Name      : lfAcctVld
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/29/2002
*! Purpose   : Validate function for the Account.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfAcctVld()
*!*************************************************************
FUNCTION lfAcctVld

SELECT CUSTOMER
LOCATE
lcFrstone = CUSTOMER.ACCOUNT

IF !EMPTY(lcAccount) .AND. !SEEK('M'+lcAccount)
  SAVE SCREEN TO lsAcctScr
  lcAccount = lcFrstone
  DO CusBrowm WITH lcAccount
  LOCATE
  RESTORE SCREEN FROM lsAcctScr
ENDIF

*--End of lfAcctVld.
*!*************************************************************
*! Name      : lfsrOrder
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/29/2002
*! Purpose   : Rise change order flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrOrder()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrOrder
PARAMETERS lcParm

IF lcParm = "S"
  SELECT ORDHDR
  LOCATE
ENDIF

*--End of lfsrOrder.
*!*************************************************************
*! Name      : lfsrStyle
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/29/2002
*! Purpose   : Set and Rest functions for style filter.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrStyle()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfsrStyle
PARAMETERS lcParm

IF lcParm = 'S'  && Set code
  *-- open this file in another alias to set order to Style Major 
  *-- unique index.
  USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
  SELECT STYLE
  SET ORDER TO TAG Cstyle
  SET RELATION TO STYLE.STYLE INTO STYLE_X
  GO TOP IN STYLE

ELSE  && Reset code

  USE IN STYLE_X
  SELECT STYLE
  SET ORDER TO TAG STYLE

ENDIF

*--End of lfsrStyle.
*!*************************************************************
*! Name      : lfBasToClr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/29/2002
*! Purpose   : Deleting temp. files.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : 1) lcFilName : hold the file name or array hold more than one file
*!                   : 2) lcTypFun  : 'F' for one file
*!                   :              : 'A' for array hold more than one file.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfBasToClr(CUTTTEMP , 'F')     >> one file.
*!             : =lfBasToClr(@laFileName , 'A')  >> more than one file.
*!*************************************************************
FUNCTION lfBasToClr
PARAMETERS lcFilName , lcTypFun

IF lcTypFun = "F"
  IF USED(lcFilName)
    SELECT (lcFilName)
    USE
  ENDIF
ELSE
  FOR lnLop = 1 TO ALEN(lcFilName,1)
    IF USED(lcfilname[lnLop])
      SELECT (lcfilname[lnLop])
      USE
    ENDIF
  ENDFOR
ENDIF

*--End of lfBasToClr.
*!*************************************************************
*! Name      : lfStySum
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/29/2002
*! Purpose   : sum a specific field for the style browse.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
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

*--End of lfStySum.