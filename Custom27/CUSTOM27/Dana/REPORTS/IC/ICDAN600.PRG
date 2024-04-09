*:***************************************************************************
*: Program file  : ICDAN600.PRG
*: Program desc. : Customized Inventory Detail Report.
*! Date          : 05/25/2000
*: System        : Aria Advantage Series.
*: Module        : INVENTORY CONTROL (IC)
*: Developer     : NADER ANIS METRY (NAD)
*: Tracking Job Number: C101041
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICDAN600
*:***************************************************************************
*: Modifications :
*****************************************************************************
*: B804297,1 BWA 07/04/2001 Fix the bug of the report doesn't calculate the stock on hand properly.
*: B804533,1 BWA 18/11/2001 Fix the bug of wrong calculating the YTD field. [Fix in SYDREPRT by changing the index of the RETLINE.DBF]
*****************************************************************************

lcDate    = ALLTRIM(STR(YEAR(IIF(EMPTY(ldDate),DATE(),ldDate))))
ldBdate   = {01/01/&lcDate}
lnNMLen=Len(lcNonMajPi)
lnMjLen=Len(gfItemMAsk("PM"))
*-- SET RELATIONS
SELECT STYLE
SET RELATION TO Style INTO INVLINE 
SET RELATION TO Style INTO RETLINE ADDITIVE

SELECT RETLINE
SET RELATION TO CRMEMO INTO RETHDR 

SELECT STYLE
SET RELATION TO 'S'+SCALE   INTO SCALE ADDITIVE
SET RELATION TO cStyGroup   INTO DANACONV ADDITIVE

SELECT INVLINE
SET RELATION TO Invoice     INTO INVHDR

SELECT STYLE
SET RELATION TO STYLE INTO STYINVJL ADDITIVE 
SET RELATION TO STYLE INTO ORDLINE ADDITIVE

SELECT ORDLINE
SET RELATION TO ORDER INTO ORDHDR ADDITIVE


PAGENO   = 0
ROW      = 99
lnTotQoh = 0
lnTotYtd = 0
DIMENSION laTotShip(9)
laTotShip = 0.00

*- REPORT PRINTING
SELECT STYLE

LOCATE FOR &lcRpExp

IF !FOUND()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF


DIMENSION laTotStk(9),laTotOrd(9)

SET DEVICE TO PRINT
 
SCAN FOR &lcrpExp
 
  IF ROW >=37   
    PAGENO = PAGENO +1
    @00,00 SAY "Date :"
    @00,07 SAY DATE()
    @00,57 SAY " DANA UNDIES - INVENTORY DETAIL REPORT"
    @00,140 SAY "Page :"
    @00,146 SAY PAGENO PICTURE '9999'
     
    IF EMPTY(lcRpTitle)
      @01,62 SAY "AS OF DATE: "
      @01,74 SAY ldDate
    ELSE
      @01,65 SAY lcRpTitle
      @02,62 SAY "AS OF DATE: "
      @02,74 SAY ldDate
    ENDIF  
    lnAddRow = IIF(EMPTY(lcRpTitle),0,1)
    @02+lnAddRow,00 SAY "ITEM #"
    @02+lnAddRow,14 SAY "COLOR"
    @02+lnAddRow,22 SAY "DESCRIPTION"
    @02+lnAddRow,67 SAY "-- SIZES --"
    @02+lnAddRow,126 SAY "CONVERT"
    @02+lnAddRow,140 SAY "PRODUCTION"
    @03+lnAddRow,00 SAY "GROUP"
    @03+lnAddRow,14 SAY "GROUP DESC."
    @03+lnAddRow,112 SAY "TOTAL"
    @03+lnAddRow,126 SAY "FACTORS"    
    @03+lnAddRow,145 SAY "TOTAL"    
    @04+lnAddRow,00 SAY REPLICATE('-',150)            
    ROW = IIF(EMPTY(lcRpTitle),5,6)
    
  ENDIF
 

  
  @ROW,00 SAY LEFT(Style,lnMjLen)
  @ROW,14 SAY SUBSTR(Style,lnMjLen+2,lnNMlen)  
  @ROW,22 SAY Desc      
  @ROW,44 SAY SCALE.SZ1
  @ROW,52 SAY SCALE.SZ2
  @ROW,60 SAY SCALE.SZ3
  @ROW,68 SAY SCALE.SZ4
  @ROW,76 SAY SCALE.SZ5
  @ROW,84 SAY SCALE.SZ6
  @ROW,92 SAY SCALE.SZ7
  @ROW,100 SAY SCALE.SZ8
 
  ROW = ROW +1
  @ROW,00 SAY CSTYGROUP    
  @ROW,14 SAY gfCodDes(STYLE.CSTYGROUP , 'CSTYGROUP')    && Get Style group from codes file. 

  ROW = ROW +1
  @ROW,31 SAY "QOH"  
  lnCol = 43
  
  
  STORE 0  TO laTotStk 
  IF !EMPTY(ldDate) 
    =lfGetStk()
  ELSE
    FOR lnCount = 1 TO SCALE.CNT
      STORE EVAL('STK'+STR(lnCount,1)) TO laTotStk(lnCount)
    ENDFOR
    laTotStk(9) = TotStk
  ENDIF
  
  
  FOR lnCount = 1 TO SCALE.CNT
    @ ROW,lnCol SAY laTotStk(lnCount) PICTURE '999999'
    lnCol = lnCol + 8
  ENDFOR
  
  @ROW,110 SAY laTotStk(9) PICTURE '9999999'
 

  @ROW,120 SAY "/"
  @ROW,122 SAY DANACONV.nConv1
  @ROW,130 SAY "*"
  @ROW,132 SAY DANACONV.nConv2
  IF DANACONV.nConv1 <> 0    
    lnTotQoh = lnTotQoh +(laTotStk(9)/DANACONV.nConv1)*DANACONV.nConv2         
    @ROW,139 SAY ROUND(laTotStk(9)/DANACONV.nConv1*DANACONV.nConv2,2)  PICTURE '99999999.99'   
  ELSE
    @ROW,145 SAY "N/A"  
  ENDIF  

  ROW = ROW +1
  @ROW,31 SAY "ONORDER"  
  lnCol = 43
 
  STORE 0 TO laTotOrd
  IF !EMPTY(ldDate)
    =lfGetOrd()
  ELSE
    FOR lnCount = 1 TO SCALE.CNT
      STORE EVAL('ORD'+STR(lnCount,1)) TO laTotOrd(lnCount)
    ENDFOR
    laTotOrd(9) = TotOrd
  ENDIF
  
  
  FOR lnCount = 1 TO SCALE.CNT   
    @ ROW,lnCol SAY laTotOrd(lnCount) PICTURE '999999'
    lnCol = lnCol + 8
  ENDFOR
  
  @ROW,110 SAY laTotOrd(9) PICTURE '9999999' 
  
  ROW = ROW +1  
  @ROW,31 SAY "YTD"
  laTotShip = 0
  SELECT INVLINE
  
  SCAN REST WHILE STYLE = STYLE.STYLE ;
            FOR   INVHDR.STATUS <> 'V' .AND. ;
            BETWEEN(INVHDR.INVDATE,ldBDate,IIF(EMPTY(ldDate),DATE(),ldDate))
              
    FOR X = 1 TO 8
      Z = STR(X,1)
      laTotShip(X) = laTotShip(X) + QTY&Z 
    ENDFOR 
      
    laTotShip(9) = laTotShip(9) + TOTQTY
  ENDSCAN  

  SELECT RETLINE
  
  SCAN REST WHILE STYLE = STYLE.STYLE ;
            FOR RETHDR.STATUS <> 'V' .AND. ;
            BETWEEN(RETHDR.CRDATE,ldBDate,IIF(EMPTY(ldDate),DATE(),ldDate))
            
    FOR X = 1 TO 8
      Z = STR(X,1)
      laTotShip(X) = laTotShip(X) - QTY&Z       
    ENDFOR   
    
    laTotShip(9) = laTotShip(9) - TOTQTY    
  ENDSCAN  
  
  lnCol = 43
  FOR lnCount = 1 TO SCALE.CNT
    @ ROW,lnCol SAY laTotShip(lnCount) PICTURE '999999'
    lnCol = lnCol + 8
  ENDFOR
  @ROW,110 SAY laTotShip(9)  PICTURE '9999999'
  IF DANACONV.nConv1 <> 0
    lnTotYtd = lnTotYtd +(laTotShip(9)/DANACONV.nConv1)*DANACONV.nConv2
      
    @ROW,139 SAY ROUND(laTotShip(9)/DANACONV.nConv1*DANACONV.nConv2,2)  PICTURE '99999999.99'  
   
  ELSE
    @ROW,145 SAY "N/A"
  ENDIF  
  
  ROW = ROW +1
  @ROW,43 SAY REPLICATE('=',75)    
  ROW = ROW +1
  @ROW,31 SAY "AVAIL."
  lnCol = 43
  FOR lnCount = 1 TO SCALE.CNT    
    @ ROW,lnCol SAY laTotStk(lnCount) - laTotOrd(lnCount) PICTURE '999999'
    lnCol = lnCol + 8
  ENDFOR
  @ROW,110 SAY laTotStk(9)-laTotOrd(9) PICTURE '9999999'  
  ROW = ROW +1
  @ROW,00 SAY REPLICATE('-',150)              

  ROW = ROW +1  
  SELECT STYLE
  
ENDSCAN
ROW = ROW+2
IF ROW >= 40
  PAGENO = PAGENO +1
  @00,00 SAY "Date :"
  @00,07 SAY DATE()
  @00,57 SAY " DANA UNDIES - INVENTORY DETAIL REPORT"
  @00,140 SAY "Page :"
  @00,146 SAY PAGENO PICTURE '9999'
  @01,65 SAY lcRpTitle
  ROW = 2    
ENDIF
*----SAYING GRAND TOTAL
@ROW,00 SAY REPLICATE('=',150)    
ROW = ROW+1
@ROW,00 SAY "GRAND TOTAL"
@ROW,30 SAY "Total Quantity on Hand:"
@ROW,139 SAY ROUND(lnTotQoh,2)  PICTURE '99999999.99'  
ROW = ROW+1
@ROW,30 SAY "Total Quantity YTD: "
@ROW,139 SAY ROUND(lnTotYtd,2)  PICTURE '99999999.99'  
ROW = ROW+1  
@ROW,00 SAY REPLICATE('=',150)      
DO ENDREPORT
SET DEVICE TO SCREEN
*!*************************************************************
*! Name      : lfGetStk
*! Developer : Nader Anis (Nad)
*! Date      : 05/25/2000
*! Purpose   :
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetStk()
*!*************************************************************

FUNCTION lfGetStk
SELECT STYINVJL

*B804297,1 BWA 07/04/2001 Fix the bug of the report doesn't calculate the stock on hand properly.[START]
*SUM REST IIF(cIrType='R' OR CTRTYPE = "1",NSTK1,IIF(cTrType = "2",-NSTK1,-ABS(NSTK1))), IIF(cIrType='R' OR CTRTYPE = "1",NSTK2,IIF(cTrType = "2",-NSTK2,-ABS(NSTK2))),;
         IIF(cIrType='R' OR CTRTYPE = "1",NSTK3,IIF(cTrType = "2",-NSTK3,-ABS(NSTK3))), IIF(cIrType='R' OR CTRTYPE = "1",NSTK4,IIF(cTrType = "2",-NSTK4,-ABS(NSTK4))),;
         IIF(cIrType='R' OR CTRTYPE = "1",NSTK5,IIF(cTrType = "2",-NSTK5,-ABS(NSTK5))), IIF(cIrType='R' OR CTRTYPE = "1",NSTK6,IIF(cTrType = "2",-NSTK6,-ABS(NSTK6))),;
         IIF(cIrType='R' OR CTRTYPE = "1",NSTK7,IIF(cTrType = "2",-NSTK7,-ABS(NSTK7))), IIF(cIrType='R' OR CTRTYPE = "1",NSTK8,IIF(cTrType = "2",-NSTK8,-ABS(NSTK8))),;
         IIF(cIrType='R' OR CTRTYPE = "1",NTOTSTK,IIF(cTrType = "2",-NTOTSTK,-ABS(NTOTSTK)));
    TO ARRAY laTotStk ;
    WHILE STYLE  = STYLE.Style ;
    FOR DtrDate <= ldDate

SUM REST NSTK1,NSTK2,NSTK3,NSTK4,NSTK5,NSTK6,NSTK7,NSTK8,NTOTSTK;
    TO ARRAY laTotStk ;
    WHILE STYLE  = STYLE.Style ;
    FOR DtrDate <= ldDate
*B804297,1 BWA 07/04/2001 [END]

SELECT STYLE
*!*************************************************************
*! Name      : lfGetOrd
*! Developer : Nader Anis (Nad)
*! Date      : 05/25/2000
*! Purpose   :
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetOrd()
*!*************************************************************

FUNCTION lfGetOrd
SELECT ORDLINE
SUM REST Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty;
    TO ARRAY laTotOrd;
    WHILE Style = STYLE.Style ;
    FOR ORDHDR.Entered <= ldDate

SELECT STYLE

*!**************************************************************************
*! Name      : lfsrvSty
*! Developer : Nader Anis (NAD)
*! Date      : 05/25/2000
*! Purpose   : Rise change style flag, in range browse screen.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Parameters  : lcParm
*!**************************************************************************
*! Example     : =lfsrvSty()
*!**************************************************************************
*! Note        : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!**************************************************************************
* 
FUNCTION lfSRVSty
PARAMETERS lcParm



DO CASE
  CASE lcParm = 'S'  && Set code
  
    SET ORDER TO TAG cStyle IN STYLE
    GO TOP IN STYLE
  CASE lcParm = 'R'  && Reset code
    SET ORDER TO TAG style  IN STYLE   && restore old Index    
ENDCASE   
  



*!*************************************************************
*! Name      : lfvFabric
*! Developer : Nader Anis(NAD)
*! Date      : 05/25/2000
*! Purpose   : validate fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFabric()
*!*************************************************************

FUNCTION lfvFabric

lcFabObj = VARREAD()

lcFab    = &lcFabObj

llUseByMe = .F.

IF !USED('FABRIC')
  llUseByMe = .T.
  USE (gcDataDir+'FABRIC') IN 0 SHARE
ENDIF
  
lcTag = ORDER('FABRIC')

SET ORDER TO FABRIC IN FABRIC

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(lcFab,'FABRIC') 
    &lcFabObj = FABRIC.Fabric
  ELSE
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF
ELSE
  &lcFabObj = ''
ENDIF

SET ORDER TO FABRIC IN FABRIC

IF llUseByMe
  USE IN FABRIC
ENDIF  
*!*************************************************************
*! Name      : lfNonMaj
*! Developer : Nader Anis (Nad)
*! Date      : 05/25/2000
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************

FUNCTION lfNonMaj

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)

  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)

  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
  
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)

    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])

    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))

  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')

    IF laMajSeg[lnI,1] = 'C'

      lnClrPo    = laMajSeg[lnI,4]

      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
  
      lcNonMajPi = laMajSeg[lnI,3]

      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
  
      EXIT
  
    ELSE
      
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.
      
    ENDIF

  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''
*!*************************************************************
*! Name      : lfwDate
*! Developer : Nader Anis (Nad)
*! Date      : 05/25/2000
*! Purpose   :
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwDate()
*!*************************************************************

FUNCTION lfwDate
ldOldate= ldDate

*!*************************************************************
*! Name      : lfvDate
*! Developer : Nader Anis (Nad)
*! Date      : 05/25/2000
*! Purpose   :
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDate()
*!*************************************************************
 
FUNCTION lfvDate
IF !ldDate <= date()
 ldDate=ldOldate
 =gfModalGen('TRM40135B00000','DIALOG')
 RETURN
ENDIF










    
 