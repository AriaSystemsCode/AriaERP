*!*****************************************************************************************
*! Name      : ALPKTAU.PRG
*! Developer : Hassan Ibrahim Ali [HIA]
*! Date      : 02/16/2005 
*! Purpose   : Picking Ticket Form AU IN ARIA4
*! Entry no. : N000482 - Picking Ticket Form AU.
*!*****************************************************************************************
*! Modification
*!*****************************************************************************************

IF USED("PkBarCode")
  SELECT PkBarCode
  DELETE ALL 
  GO TOP 
ELSE 
  SELECT 0
  CREATE CURSOR PkBarCode (piktkt c(6), cBarCode G)
  SELECT PkBarCode
  INDEX on piktkt  TAG piktkt 
  	
  SELECT(lcPiktktTemp)
  SET RELATION TO piktkt INTO PkBarCode ADDITIVE 
ENDIF 

SELECT(lcPiktktTemp)
SCAN
 SELECT PkBarCode
 APPEND BLANK 
 replace Piktkt WITH  &lcPiktktTemp..Piktkt
 =lfGetBarCode(&lcPiktktTemp..Piktkt)
ENDSCAN 
SELECT (lcTmpOrdL)
LOCATE 


*!**************************************************************************
*! Name      : lfGetBarCode
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : Get Barcode for CustPO
*!**************************************************************************
FUNCTION lfGetBarCode
PARAMETERS  tcCUPC
SELECT PkBarCode
lcF_name = UPPER('CBARCODE')
WITH loogScroll && your form name
  IF TYPE('loogScroll.'+ lcF_name) <> 'O'
    .ADDOBJECT(lcF_name,"OLEBoundControl")
  ENDIF
  .&lcF_name..CONTROLSOURCE = lcF_name
  .&lcF_name..WIDTH         = 100
  *! C201423,1 SAB 01-02-2012 Fix bug in Barcode and GiftWrap [Start]
  *.&lcF_name..HEIGHT        = 200
  .&lcF_name..HEIGHT        = 300
  *! C201423,1 SAB 01-02-2012 Fix bug in Barcode and GiftWrap [End]
  SELECT PkBarCode
   APPEND GENERAL &lcF_name. CLASS ("IDAuto.BarCode")
  .&lcF_name..REFRESH
  *-SABER-------------------------------------------------------------------[Start]
  *.&lcF_name..OBJECT.DataToEncode = tcCUPC && EVALUATE(SYCUPCDT.F_VALUE) The CUPC #
  *! C201423,1 SAB 01-02-2012 Fix bug in Barcode and GiftWrap [Start]
  *.&lcF_name..OBJECT.DataToEncode = "9859"+tcCUPC && EVALUATE(SYCUPCDT.F_VALUE) The CUPC #
  .&lcF_name..OBJECT.DataToEncode = PADR(tcCUPC, 6)
  *! C201423,1 SAB 01-02-2012 Fix bug in Barcode and GiftWrap [End]
  *-SABER-------------------------------------------------------------------[End]  
  *! C201423,1 SAB 01-02-2012 Fix bug in Barcode and GiftWrap [Start]
  *.&lcF_name..OBJECT.Barheight    = 2
  .&lcF_name..OBJECT.Barheight    = 3
  *! C201423,1 SAB 01-02-2012 Fix bug in Barcode and GiftWrap [End]
  *-SABER 12-20-2011 --------------- [Start]
  *.&lcF_name..OBJECT.SymbologyID  = 13
  .&lcF_name..OBJECT.SymbologyID  = 16
  *-SABER 12-20-2011 --------------- [End]
  .&lcF_name..OBJECT.showtext     = 0
  .&lcF_name..OBJECT.NarrowBarWidth = 0.04
  .&lcF_name..OBJECT.Wide2NarrowRatio = 3
  *adding Orientation to the barcode
  * Values are 0,90,180
  .&lcF_name..OBJECT.Orientation = 0
  .&lcF_name..OBJECT.TopMarginCm = 0
  .&lcF_name..OBJECT.LeftMarginCm = 0.1
  .&lcF_name..OBJECT.BACKCOLOR = RGB(255,255,255)
  .&lcF_name..OBJECT.FORECOLOR = RGB(0,0,0)
ENDWITH

*!**************************************************************************