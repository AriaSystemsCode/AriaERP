*:***************************************************************************
*: Program file  : MAWEA700.PRG
*: Program desc. : Material CUSTOMER REQUIREMENT
*! Date          : 04/18/1999
*: System        : Aria Advantage Series.
*: Module        : MATERIALS (M)
*: Developer     : BASSEM RAFAAT (BWA)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: REF.          : *c101499,1
*:***************************************************************************
*: Example       : DO MAWEA700
*:***************************************************************************
*:B125279,1 BWA 12/02/2004 Fix Bug to collecting order values from open orders only.
*:***************************************************************************

*-- llPrint variable that is used to prevent printing if there is not
*--           any record matches the report criteria
PRIVATE llPrint
llPrint    = .F.

*-- R_WIDTH  Variable that hold the report width
*-- R_TITLE  Variable that hold the report title
*-- XREPORT  variable that hold the report name

R_WIDTH    = 'N'
R_TITLE    = 'MATERIAL REQUIREMENTS REPORT'
XREPORT    = 'MAWEA700'

*-- lcTmpFab  Variable that hold the temporary file name
lcTmpFab   = gfTempName()

*-- Function to check if the location is empty or not
*-- lcRpWarh  variable of the option grid
IF EMPTY(lcRpWarh)
  =gfModalGen('TRM00250B00000','DIALOG', 'location ') 
  _CUROBJ = OBJNUM(LCRPWARH)
  RETURN
ELSE
  lcWareHCod = lcRpWarh
ENDIF

*-- THE MAIN PROGRAM   
DO lpCollData

*-- PRINT THE REPORT FROM THE WORKFILE
IF llPrint
  SET DEVICE TO PRINT
  DO  lpPrint
  DO  ENDREPORT
  SET DEVICE TO SCREEN
ENDIF

IF USED(lcTmpFab)
  USE IN (lcTmpFab)
ENDIF

*-- ERASE THE lcWorkFile
ERASE (gcWorkDir+lcTmpFab+'.DBF')  
ERASE (gcWorkDir+lcTmpFab+'.CDX')  

*!*************************************************************
*! Name        : lpCollData
*! Developer   : BASSEM RAFAAT 
*! Date        : 04/18/1999
*! Purpose     : Print the report
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

SELECT FabDye
SET RELATION TO 
SET KEY TO lcWareHCod
GOTO TOP

IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF

CREATE CURSOR (lcTmpFab);
   (cFabric C(7), CColor C(6), FabDesc C(20), nOnHand N(12,2),nReqQty N(12,3)) 

SELECT FabDye
SET RELATION TO Fabric+Color INTO Fabric ADDITIVE

SCAN FOR &lcRpexp
  SCATTER MEMVAR
  INSERT INTO (lcTmpFab) (cFabric,cColor,FabDesc,nOnHand);
                  VALUES (Fabric,Color,Fabric.Desc,OnHand)
ENDSCAN

SELECT (lcTmpFab)
INDEX ON cFabric+cColor TAG (lcTmpFab) OF (lcTmpFab)
GOTO TOP
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ELSE
  WAIT 'Selecting Records...' WINDOW NOWAIT
  llPrint = .T. 
ENDIF

SELECT CTktBom
SET ORDER TO TAG CTKTYP
SET KEY TO "I"
SET RELATION TO 'P'+CutTkt INTO PoSHdr,LEFT(Item,7)+IClr INTO (lcTmpFab) ADDITIVE
SCAN FOR cWareCode = lcWareHCod .AND. !EOF('PoSHdr') .AND. PoSHdr.Status = 'O' ;
        .AND. !EOF(lcTmpFab) 
    SELECT (lcTmpFab)
    WAIT WINDOW 'Selecting Fabric/Color : '+cFabric+'/'+cColor NOWAIT
    REPLACE nReqQty WITH nReqQty + ( cTktBom.Req_Qty - cTktBom.Used_Qty )
ENDSCAN

*!*************************************************************
*! Name        : lpPrint
*! Developer   : BASSEM RAFAAT 
*! Date        : 04/18/1999
*! Purpose     : Print the report
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

*-- Initializing these variables to hold the subtotals and grand totals.
STORE 0 TO lnOnHandSb,lnOnOrdrSb,lnRequirSb,lnRemainSb,;
           lnOnHandGd,lnOnOrdrGd,lnRequirGd,lnRemainGd

ROW        = 99
PageNo     = 0
SELECT (lcTmpFab)
GOTO TOP

DO WHILE !EOF() 

  =IIF(Row >=55,lfPrnHdr(),.F.)
  lcFabric = cFabric
  SCAN WHILE cFabric = lcFabric 
    =IIF(Row >=55,lfPrnHdr(),.F.)
    @ Row,00 SAY cFabric
    @ Row,08 SAY cColor
    @ Row,15 SAY FabDesc
    @ Row,36 SAY nOnHand PICTURE "9999999"

    *-- lcGetOnOrd function is used to get the on order qty from PofLn file.
    lnOnOrder = lfGetOnOrd(cFabric+cColor)
    @ Row,45 SAY lnOnOrder PICTURE "999999.999"
    
    *-- Required Quantity
    @ Row,57 SAY nReqQty PICTURE "9999999.999"

    *-- Remaining Quantity
    
    lnRemainQty = nOnHand+lnOnOrder - nReqQty
    
    @ Row,69 SAY lnRemainQty PICTURE "9999999.999"

    *-- Calculating the sub totals.
    lnOnHandSb = lnOnHandSb + nOnHand
    lnOnOrdrSb = lnOnOrdrSb + lnOnOrder
    lnRequirSb = lnRequirSb + nReqQty
    lnRemainSb = lnRemainSb + lnRemainQty
       
    *-- Calculating the grand totals.
    lnOnHandGd = lnOnHandGd + nOnHand
    lnOnOrdrGd = lnOnOrdrGd + lnOnOrder
    lnRequirGd = lnRequirGd + nReqQty
    lnRemainGd = lnRemainGd + lnRemainQty
    Row = Row + 1
  ENDSCAN
  
  *-- Printing the sub totals.
  @ Row,00 SAY REPLICATE('-',80)
  Row = Row + 1  
  =IIF(Row >=58,lfPrnHdr(),.F.)
  =lfPrnSbGd('S')
  Row = Row + 1  
  @ Row,00 SAY REPLICATE('-',80)
  Row = Row + 1  
ENDDO

*-- Printing the grand totals.
=lfPrnSbGd('G')

*!*************************************************************
*! Name         : lfPrnHdr
*! Developer    : BASSEM RAFAAT 
*! Date         : 04/18/1999
*! Purpose      : To print the report header
*!*************************************************************
*! Called from  : Option Grid
*!*************************************************************
*! Calls        : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return       : None
*!*************************************************************
*! Example      :  lfPrnHdr()
*!*************************************************************
FUNCTION lfPrnHdr

PAGENO = PAGENO + 1

*-- To print the report header
DO Rpt_Hdr WITH XREPORT,'',R_WIDTH

Row = 5
@ Row,00 SAY 'Location  : ' + lcWareHCod + SPACE(3) + WareHous.cDesc
Row = Row + 2
@ Row,00 SAY 'Item    Color  Description           OnHand     OnOrder     Required   Remaining'
ROW = ROW + 1
@ Row,00 SAY REPLICATE('-',80)
ROW = ROW + 1

*!*************************************************************
*! Name        : lfGetOnOrd
*! Developer   : BASSEM RAFAAT 
*! Date        : 04/18/1999
*! Purpose     : To get the on order qty.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     :  lfGetOnOrd()
*!*************************************************************
FUNCTION lfGetOnOrd
PARAMETERS lcKey

PRIVATE lnOrdQty,lnAlias

lnAlias = SELECT(0)
lnOrdQty = 0

=SEEK(lcKey,"FABRIC")

IF SEEK(lcKey,'PoFLn')
  SELECT PoFLn
  SCAN REST WHILE Fabric+Color = lcKey

   *B125279,1 BWA 12/02/2004 Fix Bug to collecting order values from open orders only.[START]
   *IF SEEK(Cmattype+Pomat,'PoFHdr') AND PoFHdr.cWareCode = lcWareHCod
   IF SEEK(Cmattype+Pomat,'PoFHdr') AND PoFHdr.cWareCode = lcWareHCod AND PoFHdr.Status = "O"
   *B125279,1 BWA 12/02/2004.[END]

     IF TranCd = '1'
       lnOrdQty = lnOrdQty + (NfabTotQty * FABRIC.Conv)
     ELSE
       lnOrdQty = lnOrdQty - (NfabTotQty * FABRIC.Conv)
     ENDIF
   ENDIF       
  ENDSCAN
ENDIF
SELECT(lnAlias)
RETURN(IIF(lnOrdQty<0,0,lnOrdQty))

*!*************************************************************
*! Name        : lfPrnSbGd
*! Developer   : BASSEM RAFAAT 
*! Date        : 04/18/1999
*! Purpose     : To Print the subtotals and grand totals.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     :  lfPrnSbGd()
*!*************************************************************
FUNCTION lfPrnSbGd
PARAMETERS lcType

IF lcType = "S"
  @ Row,01 SAY "SubTotal   :"
  @ Row,34 SAY lnOnHandSb PICTURE "999999999"
  @ Row,44 SAY lnOnOrdrSb PICTURE "9999999.999"
  @ Row,56 SAY lnRequirSb PICTURE "99999999.999"
  @ Row,68 SAY lnRemainSb PICTURE "99999999.999"

  STORE 0 TO lnOnHandSb,lnOnOrdrSb,lnRequirSb,lnRemainSb
ELSE
  @ Row,01 SAY "Grand Total:"
  @ Row,33 SAY lnOnHandGd PICTURE "9999999999"
  @ Row,43 SAY lnOnOrdrGd PICTURE "99999999.999"  
  @ Row,56 SAY lnRequirGd PICTURE "99999999.999"
  @ Row,68 SAY lnRemainGd PICTURE "99999999.999"

ENDIF

*!*************************************************************
*! Name      : lfvFab
*! Developer : BASSEM RAFAAT 
*! Date      : 04/18/1999
*! Purpose   : VALIDATE THE FABRIC
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFab()
*!*************************************************************

FUNCTION lfvFab

PRIVATE lcFabld,lcFabrc,lnSelFile,lcFaTag 

lcFabld   = VARREAD()
lcFabrc  = EVAL(lcFabld)

lnSelFile = SELECT(0)

SELECT FABRIC
lcFaTag   = ORDER('FABRIC')

SET ORDER TO TAG FABRIC IN FABRIC

IF !EMPTY(lcFabrc) .AND. ('?' $ lcFabrc .OR. !SEEK(lcFabrc , 'FABRIC'))
  =FABROW (@lcFabrc,'*')
ENDIF
 
&lcFabld = lcFabrc

SET ORDER TO lcFaTag

SELECT (lnSelFile)
*!*************************************************************
*! Name      : lfvWare
*! Developer : BASSEM RAFAAT 
*! Date      : 03/18/1999
*! Purpose   : Validate the warehous
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvWare()
*!*************************************************************

FUNCTION lfvWare
PRIVATE lcWarFld,lcWarhous,lnSelFile,lcWareTag

lcWarFld   = VARREAD()
lcWareCode = EVAL(lcWarFld)

lnSelFile = SELECT(0)

SELECT WAREHOUS

lcWareTag = ORDER('WAREHOUS') 

SET ORDER TO TAG WAREHOUS IN WAREHOUS

IF !EMPTY(lcWareCode) .AND. ('?' $ lcWareCode .OR. !SEEK(lcWareCode , 'WAREHOUS'))
  lcWareCode =gfBrowWare(.T.)  
ENDIF

&lcWarFld = lcWareCode

SET ORDER TO lcWareTag

SELECT (lnSelFile)
*!*************************************************************