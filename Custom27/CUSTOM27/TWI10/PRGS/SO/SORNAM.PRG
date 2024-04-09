*!********************************************************************
*: Program file  : SORNAM.PRG (REFERE TO C102117)
*: Program desc. : Program to Rename WareHouse for Open Order and Assign Styles
*: System        : ARIA 2.7
*: Module        : SO
*: Developer     : MOHAMED SHOKRY (MHM)
*:***************************************************************************
*: Calls : 
*:    Procedures : 
*:    Functions  : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SORNAM
*:***************************************************************************
*: Modifications:
*:***************************************************************************
*** You Will Now Rename Warehouse for Open order and Assign Styles ***
*** <  Ok  > , < Cancel> ***
*lnChoice =gfModalGen('QRM00000B42002','F',' ',' ','You will now rename the warehouse for open order and assign styles')   

lnChoice =gfModalGen('QRM00000B42002','F',' ',' ','Will you rename the warehouse for the open orders and assign styles now.')   
* Will you rename the warehouse for the open orders and assign styles now.
IF lnChoice = 1 
  PRIVATE lcNewWH , lcOldWH
  lcNewWH  = "IMPAC"
  lcOldWH  = "PHI" 
  *--Open Needed Files [BEGIN]
  = gfOpenFile(gcdatadir+'ORDHDR',gcdatadir+'ORDHDR','SH')
  = gfOpenFile(gcdatadir+'STYLE',gcdatadir+'STYLE','SH')
  = gfOpenFile(gcdatadir+'OrdLine',gcdatadir+'OrdLine','SH')
  = gfOpenFile(gcdatadir+'StyDye',gcdatadir+'StyDye','SH')
  = gfOpenFile(gcdatadir+'WareHous',gcdatadir+'WareHous','SH')
  
  llWareHus =IIF(gfGetMemVar('M_WAREHOUS') ='Y' ,.T.,.F.) 
  DIMENSION laSeason[6,1]
  laSeason[1,1] = PADR('17',6,' ')    && HOLIDAY 2000   Season
  laSeason[2,1] = PADR('18',6,' ')    && SPRING 2001    Season
  laSeason[3,1] = PADR('19',6,' ')    && FALL 2001      Season
  laSeason[4,1] = PADR('20',6,' ')    && HOLIDAY 2001   Season
  laSeason[5,1] = PADR('21',6,' ')    && SPRING 2002    Season
  laSeason[6,1] = PADR('22',6,' ')    && FALL 2002      Season
  *--Open Needed Files [END]

  SELECT ORDHDR
  SET RELATION TO 'O' + ORDER INTO OrdLine

  SELECT STYLE
  SCAN FOR ASCAN(laSeason , STYLE.SEASON) >0
    WAIT WINDOW "Updating warehouse "     NOWAIT
    IF !SEEK(Style + lcNewWH ,'StyDye')
      DO gpAdStyWar WITH Style,SPACE(10), lcNewWH
      *--UPATE ORDERS QUANTITY
    ENDIF
  ENDSCAN

  SELECT ORDHDR
  SCAN 
    IF STATUS = 'O' AND cWareCode = lcOldWH
      WAIT WINDOW "Checking existing orders "     NOWAIT
      SELECT OrdLine
      =SEEK(STYLE,'STYLE')
      SCAN REST WHILE ORDHDR.cOrdType + ORDHDR.Order = "O" + Order ; 
                FOR ASCAN(laSeason , STYLE.SEASON) >0 
                
        REPLACE OrdHdr.cWareCode WITH  lcNewWH
        WAIT WINDOW "Check the open order " + Order      NOWAIT
        lcOldWar = cWareCode
        REPLACE cWareCode     WITH lcNewWH
        IF SEEK(Style + cWareCode ,'StyDye')
           REPLACE StyDye.ord1   WITH OrdLine.qty1   + StyDye.ord1 ,;
                   StyDye.ord2   WITH OrdLine.qty2   + StyDye.ord2,;
                   StyDye.ord3   WITH OrdLine.qty3   + StyDye.ord3,;
                   StyDye.ord4   WITH OrdLine.qty4   + StyDye.ord4,;
                   StyDye.ord5   WITH OrdLine.qty5   + StyDye.ord5,;
                   StyDye.ord6   WITH OrdLine.qty6   + StyDye.ord6,;
                   StyDye.ord7   WITH OrdLine.qty7   + StyDye.ord7,;
                   StyDye.ord8   WITH OrdLine.qty8   + StyDye.ord1,;
                   StyDye.TOTord WITH OrdLine.TOTqty + StyDye.TOTord 
        ENDIF
       
        IF SEEK(Style + lcOldWar ,'StyDye')
           REPLACE StyDye.ord1   WITH StyDye.ord1   - OrdLine.qty1 ,;
                   StyDye.ord2   WITH StyDye.ord2   - OrdLine.qty2 ,;
                   StyDye.ord3   WITH StyDye.ord3   - OrdLine.qty3 ,;
                   StyDye.ord4   WITH StyDye.ord4   - OrdLine.qty4 ,;
                   StyDye.ord5   WITH StyDye.ord5   - OrdLine.qty5 ,;
                   StyDye.ord6   WITH StyDye.ord6   - OrdLine.qty6 ,;
                   StyDye.ord7   WITH StyDye.ord7   - OrdLine.qty7 ,;
                   StyDye.ord8   WITH StyDye.ord8   - OrdLine.qty8 ,;
                   StyDye.TOTord WITH StyDye.TOTord - OrdLine.TOTqty 
       ENDIF            
      ENDSCAN
    ELSE
      WAIT WINDOW "Checking existing orders "     NOWAIT
    ENDIF 
  ENDSCAN

  
  =gfModalGen('QRM00000B38018','F',' ',' ','Renaming of warehouse has been successfully completed.')   
ELSE
  =gfModalGen('QRM00000B38018','F',' ',' ','Renaming of warehouse has been cancelled.')   
ENDIF
*-- END SORNAM
