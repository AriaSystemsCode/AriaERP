*:***************************************************************************
*: Program file  : ALPKTKBL.PRG
*: Program desc. : SALES ORDER ALLOCATION PICKING TICKET FORM 'BL' (Print Bin Location/Size).
*! Date          : 10/14/2018
*: System        : ARIA4XP 
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : Mariam Mazhar (MMT)
*: Tracking Job Number: E304075{P20171201.0003}
*:***************************************************************************
*: Calls :
*:    Procedures : 
*:    Functions  : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPKTKB
*:***************************************************************************
IF llOgFltCh
  SELECT (lcTmpOrdL) 
  lcKeyFld = KEY()
  lcNamOrder = ORDER()
  lcRelation = SET("Relation")
  SELECT *,SPACE(10) AS Bin1,SPACE(10) as Bin2 ,SPACE(10) as Bin3,SPACE(10) AS Bin4 ,;
         SPACE(10) AS Bin5,SPACE(10) AS Bin6,SPACE(10) As  Bin7 ,SPACE(10) AS Bin8 FROM (lcTmpOrdL) INTO CURSOR 'TMPCUR' READWRITE 

  SELECT 'TMPCUR' 
  INDEX ON &lcKeyFld. TAG 'TMPCUR' 
  SET RELATION TO &lcRelation. 
  LOCATE
  lcTmpOrdL = 'TMPCUR' 

  SELECT (lcTmpOrdL)
  LOCATE 
  SCAN 
    lcBin1 = lfGetStyleSizeBin(iif(!empty(&lcTmpOrdL..altstyle),&lcTmpOrdL..altstyle,&lcTmpOrdL..Style),EVAL(lcPiktktTemp+'.cWareCode'),1)
    lcBin2 = lfGetStyleSizeBin(iif(!empty(&lcTmpOrdL..altstyle),&lcTmpOrdL..altstyle,&lcTmpOrdL..Style),EVAL(lcPiktktTemp+'.cWareCode'),2)
    lcBin3 = lfGetStyleSizeBin(iif(!empty(&lcTmpOrdL..altstyle),&lcTmpOrdL..altstyle,&lcTmpOrdL..Style),EVAL(lcPiktktTemp+'.cWareCode'),3)
    lcBin4 = lfGetStyleSizeBin(iif(!empty(&lcTmpOrdL..altstyle),&lcTmpOrdL..altstyle,&lcTmpOrdL..Style),EVAL(lcPiktktTemp+'.cWareCode'),4)
    lcBin5 = lfGetStyleSizeBin(iif(!empty(&lcTmpOrdL..altstyle),&lcTmpOrdL..altstyle,&lcTmpOrdL..Style),EVAL(lcPiktktTemp+'.cWareCode'),5)
    lcBin6 = lfGetStyleSizeBin(iif(!empty(&lcTmpOrdL..altstyle),&lcTmpOrdL..altstyle,&lcTmpOrdL..Style),EVAL(lcPiktktTemp+'.cWareCode'),6)
    lcBin7 = lfGetStyleSizeBin(iif(!empty(&lcTmpOrdL..altstyle),&lcTmpOrdL..altstyle,&lcTmpOrdL..Style),EVAL(lcPiktktTemp+'.cWareCode'),7)            
    lcBin8 = lfGetStyleSizeBin(iif(!empty(&lcTmpOrdL..altstyle),&lcTmpOrdL..altstyle,&lcTmpOrdL..Style),EVAL(lcPiktktTemp+'.cWareCode'),8)            
    REPLACE Bin1 WITH lcBin1 ,;
  		  Bin2 WITH lcBin2 ,;
  		  Bin3 WITH lcBin3 ,;
  		  Bin4 WITH lcBin4 ,;
  		  Bin5 WITH lcBin5 ,;
  		  Bin6 WITH lcBin6 ,;
  		  Bin7 WITH lcBin7 ,;
  		  Bin8 WITH lcBin8
  ENDSCAN
  SELECT (lcTmpOrdL)
  LOCATE 
ENDIF
*!*************************************************************
*! Name      : lfGetStyleSizeBin
*! Developer : Mariam Mazhar[MMT]
*! Date      : 10/14/2018
*! Purpose   : Get Style/location/Size Bin
*!*************************************************************
FUNCTION lfGetStyleSizeBin
LPARAMETERS lcStyle,LcWareHouse,lnSizeNo
lnCrAlis = SELECT(0)
lcRetBin = ''
IF !USED('WHSLOC') 
  =gfOpenTable('WHSLOC','WHSLOCST')
ELSE
  SELECT WHSLOC
  =gfSetOrder('WHSLOCST') && STYLE+COLOR+CWARECODE+CLOCATION
ENDIF
IF gfSeek(lcStyle+SPACE(6)+LcWareHouse,'WHSLOC')
  SELECT WHSLOC
  LOCATE REST WHILE STYLE+COLOR+CWARECODE+CLOCATION =lcStyle+SPACE(6)+LcWareHouse FOR Size = ALLTRIM(STR(lnSizeNo))
  IF FOUND()
    lcRetBin = WHSLOC.CLOCATION 
  ELSE
    =gfSeek(lcStyle+SPACE(6)+LcWareHouse,'WHSLOC')
    LOCATE REST WHILE STYLE+COLOR+CWARECODE+CLOCATION =lcStyle+SPACE(6)+LcWareHouse FOR EMPTY(Size)
    IF FOUND()
      lcRetBin = WHSLOC.CLOCATION 
    ENDIF
  ENDIF
ENDIF
SELECT(lnCrAlis)
RETURN lcRetBin


