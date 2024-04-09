*:************************************************************************
*: Program file  : SOINT200.Prg          (C101664)
*: Program desc. : CUSTOM FOR INT20
*: System        : Aria Advantage Series VER. 2.7
*: Module        : SO
*: Developer     : Mohamed Shokry
*: Date          : 08/08/2000
*:************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : 
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO SOINT20
*:************************************************************************
*: Modifications:
*:************************************************************************
*B804039,1 AAN 03/21/2001 Many changes have been done in this report . Please 
*B804039,1                refer to the specs.  
**************************************************************************
STORE 0 TO lnCurAlis
lcTime = TIME()
lcNewExp = ' '
IF "REP1" $ lcRpExp
  =lfAddFxOr("ORDHDR.REP1","ORDHDR.REP2")
ELSE
  lcNewExp = lcRpExp
ENDIF
*-- To Get Open Order Only
lcNewExp = lcNewExp +'.AND. ORDHDR.STATUS$"OC"'
SELECT OrdHdr
LOCATE FOR &lcNewExp
IF EOF()
  = gfModalGen("TRM00052B00000","DIALOG",'')  && 'There are no records to display' 
  RETURN
ENDIF
*------ Collecting data  [BEGIN] --------
lcOrdhTmp  = gfTEMPNAME()
lcRevTemp = gfTEMPNAME()
lcOrdhTmp1 = gfTEMPNAME()
lcOrdhTmp2 = gfTEMPNAME()
lcGrpCurr = cCurrCode
llSameGrp = .T.

DO lpCreaTemp
DO lpColect

*--RELATIONS SECTION 
SELECT ORDHDR
SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO CUSTOMER
SET RELATION TO CordType + order INTO ORDLINE ADDITIVE
SET RELATION TO Order INTO INVLINE ADDITIVE
*----
SELECT (lcOrdhTmp)
*B804039 AAN Check if there are data to print or not[Begin]
*SET RELATION TO CordType + order INTO ORDHDR ADDITIVE
*SELECT (lcOrdhTmp)
*!-- Printing Section
*SET ORDER TO TAG &lcOrdhTmp2
*DO gfDispRe WITH EVALUATE('lcRpForm')
LOCATE
IF EOF()
  WAIT WINDOW "No records to display"
ELSE
  SET RELATION TO CordType + order INTO ORDHDR ADDITIVE
  SELECT (lcOrdhTmp)
  *!-- Printing Section
  SET ORDER TO TAG &lcOrdhTmp2
  DO gfDispRe WITH EVALUATE('lcRpForm')
ENDIF
*B804039 AAN Check if there are data to print or not[End]
*!**************************************************************************
*! Name      : lfSeTOrdr
*! Developer : WAB - WALID A. WAHAB
*! Date      : 06/14/1999
*! Purpose   : Set Relation,Reset Relation, in range browse screen.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid && Report DYE DEMOND REPORT
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =fSeTOrdr()
*!**************************************************************************
*! Note      : symbol is [S,Set- R,ReSet]
*!**************************************************************************
FUNCTION lfSeTOrdr 
PARAMETERS OpGrdParm
PRIVATE lnOrder
lnOrder = SET('ORDER')
DO CASE
  CASE OpGrdParm = 'S'
    SELECT ORDHDR
    SET ORDER TO TAG ORDHDR
    lcRelation = [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)]
    SET ORDER TO Customer IN Customer
    SET RELATION TO &lcRelation INTO CUSTOMER && To customer file.
    GO TOP
  CASE OpGrdParm = 'R'
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER && To customer file.
    SET ORDER TO &lnOrder
ENDCASE

*!**************************************************************************
*! Name      : lfSetAcc
*! Developer : WAB - WALID A. WAHAB
*! Date      : 06/14/1999
*! Purpose   : Set Relation,Reset Relation, in range browse screen.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid  && Report DYE DEMOND REPORT
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSetAcc()
*!**************************************************************************
*! Note      :  symbol is [S,Set- R,ReSet]
*!**************************************************************************
FUNCTION lfSetAcc
PARAMETERS OpGrdParm
DO CASE
  CASE OpGrdParm = 'S'  && Set code
    SELECT CUSTOMER
    SET ORDER TO TAG CUSTOMER
    GO TOP IN CUSTOMER
  CASE OpGrdParm = 'R'  && Reset code
    SELECT CUSTOMER
    SET ORDER TO 
ENDCASE
*!**************************************************************************
*! Name      : lfSeTSRep 
*! Developer : WAB - WALID A. WAHAB
*! Date      : 06/14/1999
*! Purpose   : Go top in the style IN RANGE
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSeTSRep()
*!**************************************************************************
FUNCTION lfSeTSRep 
PARAMETERS OpGrdParm
DO CASE
  CASE OpGrdParm = 'S'
   SELECT SALESREP
   SET ORDER TO TAG  SALESREP
   GO TOP
  CASE OpGrdParm = 'R'
    SELECT SALESREP 
    SET ORDER TO 
ENDCASE
*!**************************************************************************
*! Name      : lfDydWhen 
*! Developer : WAB - WALID A. WAHAB
*! Date      : 06/10/1999
*! Purpose   : set Last Order Start Ship Date = today()+dye 
*!             visibility window
*!**************************************************************************
*! Calls     : gfGetmemVar()
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfDydWhen()
*!**************************************************************************
FUNCTION lfDydWhen
PRIVATE lnarrPos
lnarrpos = INT(ASCAN(laOGFxFlt,'ORDHDR.START') / 7 + 0.9)
IF EMPTY(laOGFxFlt[lnarrpos,6])
  laOGFxFlt[lnarrpos,6] = date()+gfGetmemVar('M_DOVW','gcAct_Comp')
ENDIF
*!**************************************************************************
*! Name      : lfGetPos  
*! Developer : WAB - WALID A. WAHAB
*! Date      : 06/10/1999
*! Purpose   : Get Starting Position & lenth Of Color
*!**************************************************************************
*! Calls     : ()
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfGetPos()
*!**************************************************************************
FUNCTION lfGetPos
PRIVATE lnClrPos
*-- laMajSeg array holds the style code segments data
*-- laMajSeg[x,1] Holds segment type
*-- laMajSeg[x,2] Holds segment title
*-- laMajSeg[x,3] Holds segment picture
*-- laMajSeg[x,4] Holds segment Starting position
*-- laMajSeg[x,5] Holds segment description
*-- laMajSeg[x,6] Holds segment separator
*-- laMajSeg[x,7] Holds (.T./.F.) segment end major. 
 DIMENSION laMajSeg[1,1]
 = gfItemMask(@laMajSeg)
 lnClrPos = int(ASCAN(laMajSeg,'C')/7+0.9)
 lnStrtPos = laMajSeg[lnClrPos,4] 
 lnFldLngth= LEN(laMajSeg[lnClrPos,3])
*!**************************************************************************
*! Name      : lfAddFxOr
*! Developer : WAB - WALID A. WAHAB
*! Date      : 06/10/1999
*! Purpose   : Create expretion Filter  
*!           : Devide laOgFlt into 2 array and add (.or.) inside 2nd array
*!**************************************************************************
*! Calls     : gfGenFlt()
*!**************************************************************************
*! Called from : SOINT20.prg
*!**************************************************************************
*! Passed Parameters  : Exist Field / New Field
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfAddFxOr()
*!**************************************************************************
FUNCTION lfAddFxOr
PARAMETER lcExstFld,lcNewFld

*Add new lines
*-- We will extract the filter part concerning with REP1 and add REP2 to the filter expression
IF RAT('AND' , LEFT(lcRpExp,AT('ORDHDR.REP1',lcRpExp) )) # 0
  *-- Get the expression part before the rep1 expression
  lcTempV = SUBSTR(lcRpExp,RAT('AND' , LEFT(lcRpExp,AT('ORDHDR.REP1',lcRpExp) ))+4   )
ELSE
  lcTempV = lcRpExp
ENDIF
IF ATC(')AND',lcTempV) # 0 .OR. ATC(') AND',lcTempV) # 0
  *-- Get the exact REP1 expression
  lcTempV = LEFT(lcTempV,ATC('AND',lcTempV)-1 )
ENDIF
*-- Add an expression pretty much like REP1 but of REP2
lcExpr  = '(' + lcTempV + '.OR.' + STRTRAN(lcTempV,'ORDHDR.REP1','ORDHDR.REP2') + ')'
*-- Form the new expression
lcNewExp = STRTRAN(lcRpExp,lcTempV,lcExpr)

RETURN
*:B802816,1 RAMY (end)
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 04/12/2000
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
*B603547,1
FUNCTION lfwRepWhen

*:B803305,1 ABD  variable that get the position of RepCode.[begin]
lnOrdPos = lfItmPos('ORDHDR.REP1')
*:B803305,1 ABD  [End]

*-- end of lfwRepWhen.
*!*************************************************************
*! Name      : lfItmPos
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 04/12/2000
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*!B603547,1 
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- End Of lfItmPos.

*!*************************************************************
*! Name      : lpColect
*! Developer : Mohamed Shokry
*! Date      : 08/10/2000
*! Purpose   : PROCEDURE to collect the correct data.
*!*************************************************************
*! Called from : None.
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : Do lpColect()
*!*************************************************************
PROCEDURE lpColect

*B804039 AAN Add Var. (lnRecNo) [Begin]
*PRIVATE lnPrice , lnAmnt ,lcAccount
PRIVATE lnPrice , lnAmnt ,lcAccount , lnRecNo
lnPrice = 0
*B804039 AAN Add Var. (lnRecNo) [End]

*-- Relation to collect data
STORE '' TO lcAac

SELECT (lcOrdhTmp)
SET ORDER TO TAG &lcOrdhTmp

SELECT ORDHDR
SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO CUSTOMER
SET RELATION TO CordType + order INTO ORDLINE ADDITIVE
SET RELATION TO Order INTO INVLINE ADDITIVE
SELECT ORDHDR

*B804039 AAN Change the Exp. [Begin]
*GOTO TOP
LOCATE 
*B804039 AAN Change the Exp. [End]
*--SCAN WHILE cordtype = "O" FOR &lcNewExp

SCAN FOR &lcNewExp
  *--TO GET SEPARATOR RECORD IN FRX.[BEGIN]  
  SCATTER MEMVAR MEMO
  *B804039,1 AAN Get the customer address [Begin].
  m.cCustnam = Customer.BtName
  *B804039,1 AAN Get the customer address [End]. 
   
  *B804039 AAN Remove the following lines [Begin]
  **IF LCAAC <> ACCOUNT
    **SELECT (lcOrdhTmp)
    **INSERT INTO (lcOrdhTmp) FROM MEMVAR
    ***REPLACE &lcOrdhTmp..cGroup WITH "E" 
    **SELECT ORDHDR
  **ENDIF
  *--TO GET SEPARATOR RECORD IN FRX[END].  
  *-- to get group detal
  *m.cGroup = "D"
  *B804039 AAN Remove the following lines [End]
  
  SELECT ORDLINE
  
  *B804039 AAN Remove the following lines [Begin]
  *lnPrice = Price
  *B804039 AAN Remove the following lines [End]

  *B804039 AAN Collecting data from OrdLine file [Begin]
  lnRecNo = RECNO()

  SCAN REST WHILE cOrdType+Order = Ordhdr.cOrdType+Ordhdr.Order
    m.open = 0
    IF lcRpShip = "B"
      m.Open = TotBook
      lnPrice = Price
    ELSE
      SELECT INVLINE
      SET ORDER TO TAG InvLineo
      IF SEEK(OrdLine.Order+STR(OrdLine.LineNo,6))
        m.Open = TotQty
        lnPrice = Price
      ENDIF
    ENDIF
    * Avoid this style is the qty is equal to zero  
    IF m.Open=0
      LOOP
    ENDIF
    *B804039 AAN Collecting data from OrdLine file [End]

    *B804039 AAN Change seek Exp. [Begin]
    **IF !SEEK(m.ACCOUNT+SEASON + STYLE +STR(YEAR(Complete)) + "D", lcOrdhTmp)
    IF !SEEK(m.ACCOUNT+Season + SUBSTR(STYLE,1,12), lcOrdhTmp)
    *B804039 AAN Change seek Exp. [End]
      INSERT INTO (lcOrdhTmp) FROM MEMVAR
      *B804039 AAN Add Season filed to replace stat. [Begin]
      *REPLACE &lcOrdhTmp..Style WITH Style ;
              &lcOrdhTmp..Price WITH Price

      REPLACE &lcOrdhTmp..Style WITH Style ;
              &lcOrdhTmp..Price WITH lnPrice ;
              &lcOrdhTmp..Season WITH Season
      *B804039 AAN Add Season filed to replace stat. [End]
      
     ENDIF
     *B804039 AAN Remove the following lines [Begin]
    **ENDSCAN
    **GOTO lnRecNo
    *B804039 AAN Remove the following lines [End]
    *B804039 AAN Remove the following lines [Begin]
    *-- TO GET INVOICED ORDER
    *IF Status = "C"
      *SELECT INVLINE
      *SCAN REST WHILE order = Ordhdr.Order
        *m.Open = 0
        *m.Open = m.Open +TotQty
      *ENDSCAN
    *ENDIF  
    *B804039 AAN Remove the following lines [End]
    lnAmnt = m.Open*lnPrice
    SELECT ORDHDR
    *B804039 AAN Update Temp file with the qty & Amount [Begin]
    lcMonCom = MONTH(COMPLETE)
    =lfMonth(LEFT(CMONTH(COMPLETE),3)+"_QTY",LEFT(CMONTH(COMPLETE),3)+"_Amnt")
    *B804039 AAN Update Temp file with the qty & Amount [End]
    *B804039 AAN finishing the scan [Begin]
    SELECT ORDLINE
   ENDSCAN
   *B804039 AAN finishing the scan [End]
   LCAAC = ACCOUNT
ENDSCAN
SELECT ORDHDR
SET RELATION TO

*!*****************************************************************************
*! Name      : lpCreaTemp
*! Developer : Mohamed Shokry
*! Date      : 25/05/2000
*! Purpose   : Function  To Create Temp File for Collecting Data
*!*****************************************************************************
*! Example   : =lpCreaTemp()
*!*****************************************************************************
*!* 
FUNCTION lpCreaTemp
PRIVATE lnRows

IF USED(lcOrdhTmp)
  USE IN (lcOrdhTmp)
ENDIF
IF !USED(lcOrdhTmp)
  DIMENSION laTempStru[1,4]
   SELECT ORDHDR
    lnRows=AFIELDS(laTempStru)
    *B804039,1 AAN Add row to the array [Begin].
    *DIMENSION laTempStru[lnRows + 27 , 4]
    DIMENSION laTempStru[lnRows + 28 , 4]
    *B804039,1 AAN Add row to the array [End].

  laTempStru[lnRows + 1 ,1] = "Jan_Qty"
  laTempStru[lnRows + 1 ,2] = "N"
  laTempStru[lnRows + 1 ,3] = 7
  laTempStru[lnRows + 1 ,4] = 0

  laTempStru[lnRows + 2 ,1] = "Feb_Qty"
  laTempStru[lnRows + 2 ,2] = "N"
  laTempStru[lnRows + 2 ,3] = 7
  laTempStru[lnRows + 2 ,4] = 0

  laTempStru[lnRows + 3 ,1] = "Mar_Qty"
  laTempStru[lnRows + 3 ,2] = "N"
  laTempStru[lnRows + 3 ,3] = 7
  laTempStru[lnRows + 3 ,4] = 0

  laTempStru[lnRows + 4 ,1] = "Apr_Qty"
  laTempStru[lnRows + 4,2] = "N"
  laTempStru[lnRows + 4,3] = 7
  laTempStru[lnRows + 4,4] = 0

  laTempStru[lnRows + 5 ,1] = "May_Qty"
  laTempStru[lnRows + 5 ,2] = "N"
  laTempStru[lnRows + 5 ,3] = 7
  laTempStru[lnRows + 5 ,4] = 0

  laTempStru[lnRows + 6 ,1] = "Jun_Qty"
  laTempStru[lnRows + 6 ,2] = "N"
  laTempStru[lnRows + 6 ,3] = 7
  laTempStru[lnRows + 6 ,4] = 0

  laTempStru[lnRows + 7 ,1] = "Jul_Qty"
  laTempStru[lnRows + 7 ,2] = "N"
  laTempStru[lnRows + 7 ,3] = 7
  laTempStru[lnRows + 7 ,4] = 0

  laTempStru[lnRows + 8 ,1] = "Aug_Qty"
  laTempStru[lnRows + 8 ,2] = "N"
  laTempStru[lnRows + 8 ,3] = 7
  laTempStru[lnRows + 8 ,4] = 0

  laTempStru[lnRows + 9 ,1] = "Sep_Qty"
  laTempStru[lnRows + 9 ,2] = "N"
  laTempStru[lnRows + 9 ,3] = 7
  laTempStru[lnRows + 9 ,4] = 0

  laTempStru[lnRows + 10 ,1] = "Oct_Qty"
  laTempStru[lnRows + 10 ,2] = "N"
  laTempStru[lnRows + 10 ,3] = 7
  laTempStru[lnRows + 10 ,4] = 0

  laTempStru[lnRows + 11 ,1] = "Nov_Qty"
  laTempStru[lnRows + 11 ,2] = "N"
  laTempStru[lnRows + 11 ,3] = 7
  laTempStru[lnRows + 11 ,4] = 0

  laTempStru[lnRows + 12 ,1] = "Dec_Qty"
  laTempStru[lnRows + 12 ,2] = "N"
  laTempStru[lnRows + 12 ,3] = 7
  laTempStru[lnRows + 12 ,4] = 0

  laTempStru[lnRows + 13 ,1] = "STYLE"
  laTempStru[lnRows + 13 ,2] = "C"
  laTempStru[lnRows + 13 ,3] = 19
  laTempStru[lnRows + 13 ,4] = 0

  laTempStru[lnRows + 14 ,1] = "PRICE"
  laTempStru[lnRows + 14 ,2] = "N"
  laTempStru[lnRows + 14 ,3] = 7
  laTempStru[lnRows + 14 ,4] = 0

  laTempStru[lnRows + 15 ,1] = "Jan_Amnt"
  laTempStru[lnRows + 15 ,2] = "N"
  laTempStru[lnRows + 15 ,3] = 7
  laTempStru[lnRows + 15 ,4] = 0

  laTempStru[lnRows + 16 ,1] = "Feb_Amnt"
  laTempStru[lnRows + 16 ,2] = "N"
  laTempStru[lnRows + 16 ,3] = 7
  laTempStru[lnRows + 16 ,4] = 0

  laTempStru[lnRows + 17 ,1] = "Mar_Amnt"
  laTempStru[lnRows + 17 ,2] = "N"
  laTempStru[lnRows + 17 ,3] = 7
  laTempStru[lnRows + 17 ,4] = 0

  laTempStru[lnRows + 18 ,1] = "Apr_Amnt"
  laTempStru[lnRows + 18 ,2] = "N"
  laTempStru[lnRows + 18 ,3] = 7
  laTempStru[lnRows + 18 ,4] = 0

  laTempStru[lnRows + 19 ,1] = "May_Amnt"
  laTempStru[lnRows + 19 ,2] = "N"
  laTempStru[lnRows + 19 ,3] = 7
  laTempStru[lnRows + 19 ,4] = 0

  laTempStru[lnRows + 20 ,1] = "Jun_Amnt"
  laTempStru[lnRows + 20 ,2] = "N"
  laTempStru[lnRows + 20 ,3] = 7
  laTempStru[lnRows + 20 ,4] = 0

  laTempStru[lnRows + 21 ,1] = "Jul_Amnt"
  laTempStru[lnRows + 21 ,2] = "N"
  laTempStru[lnRows + 21 ,3] = 7
  laTempStru[lnRows + 21 ,4] = 0

  laTempStru[lnRows + 22 ,1] = "Aug_Amnt"
  laTempStru[lnRows + 22 ,2] = "N"
  laTempStru[lnRows + 22 ,3] = 7
  laTempStru[lnRows + 22 ,4] = 0

  laTempStru[lnRows + 23 ,1] = "Sep_Amnt"
  laTempStru[lnRows + 23 ,2] = "N"
  laTempStru[lnRows + 23 ,3] = 7
  laTempStru[lnRows + 23 ,4] = 0

  laTempStru[lnRows + 24 ,1] = "Oct_Amnt"
  laTempStru[lnRows + 24 ,2] = "N"
  laTempStru[lnRows + 24 ,3] = 7
  laTempStru[lnRows + 24 ,4] = 0

  laTempStru[lnRows + 25 ,1] = "Nov_Amnt"
  laTempStru[lnRows + 25 ,2] = "N"
  laTempStru[lnRows + 25 ,3] = 7
  laTempStru[lnRows + 25 ,4] = 0

  laTempStru[lnRows + 26 ,1] = "Dec_Amnt"
  laTempStru[lnRows + 26 ,2] = "N"
  laTempStru[lnRows + 26 ,3] = 7
  laTempStru[lnRows + 26 ,4] = 0

  laTempStru[lnRows + 27 ,1] = "cGroup"
  laTempStru[lnRows + 27 ,2] = "C"
  laTempStru[lnRows + 27 ,3] = 1
  laTempStru[lnRows + 27 ,4] = 0

  *B804039,1 AAN Add row to the array [Begin].
  laTempStru[lnRows + 28 ,1] = "cCustNam"
  laTempStru[lnRows + 28 ,2] = "C"
  laTempStru[lnRows + 28 ,3] = 30
  laTempStru[lnRows + 28 ,4] = 0
  *B804039,1 AAN Add row to the array [End].

  CREATE CURSOR (lcOrdhTmp) FROM ARRAY laTempStru
  ZAP
  *-- Index On Temp File  

  *B804039,1 AAN 03/21/2001  Change the index key [Begin] 
  *INDEX ON ACCOUNT+SEASON + STYLE +STR(YEAR(Complete)) + cGroup TAG &lcOrdhTmp 
  INDEX ON ACCOUNT+SEASON + SUBSTR(STYLE,1,12) +STR(YEAR(Complete)) + cGroup TAG &lcOrdhTmp 
  *B804039,1 AAN 03/21/2001  Change the index key [End] 
  INDEX ON ACCOUNT+STR(YEAR(Complete)) +cGroup TAG &lcOrdhTmp1 
  INDEX ON ACCOUNT+cGroup TAG &lcOrdhTmp2 

ENDIF

*!*****************************************************************************
*! Name      : lfMonth
*! Developer : Ahmed Abdel Naby
*! Date      : 15/04/2001
*! Purpose   : Function to update the temp file with the quantities and amounts 
*!*****************************************************************************
*! Example   : =lfMonth()
*!*****************************************************************************
*!B804039,1 AAN 03/21/2001 

FUNCTION lfMonth
PARAMETERS lcMonth,lnAmt
SELECT (lcOrdhTmp)
REPLACE &lcOrdhTmp..&lcMonth WITH &lcOrdhTmp..&lcMonth + m.Open ;
        &lcOrdhTmp..&lnAmt WITH lnAmnt +&lcOrdhTmp..&lnAmt

