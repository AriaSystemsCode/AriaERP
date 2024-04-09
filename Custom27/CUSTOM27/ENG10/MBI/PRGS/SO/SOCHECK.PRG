*:***************************************************************************
*: Program file     : SOREPAIR.PRG
*: Program desc.    : Fix program to check & repair the SO status and Qty.
*: Date             : 01/04/2005
*: System           : Aria Advantage Series.
*: Module           : SALES ORDER (SO)
*: Developer        : NADER NABIL (NNA)
*: Tracking Job NO# : B125455,1
*:***************************************************************************
*: Calls            : 
*:***************************************************************************
*: Passed Parameters: None
*:***************************************************************************
*: Example          : DO SOREPAIR
*:***************************************************************************
PRIVATE lnRecCount , lnRecNo , lnordShipQ , lnOrdShipA , lnordCnclQ , lnordCnclA ,;
        lnordopenQ , lnordopenA , lcInvNo , lcExpr , lcTmpFile,lcFiles,lcIndex
        
STORE 0 TO lnRecCount , lnRecNo , lnordShipQ , lnOrdShipA , lnordCnclQ , ;
        lnordCnclA , lnordopenQ , lnordopenA
STORE '' TO lcInvNo , lcExpr , lcTmpFile , lcFiles , lcIndex
lcTmpFile  = gfTempName()

*--Get the needed files and indexes into arrays to open or close them.
DIMENSION laFiles[8],laIndex[8]
STORE '' TO laFiles,laIndex
lcFiles ='INVHDR,INVLINE,ORDHDR,ORDLINE,CONSINVH,CONSINVL,ORDCANLN,PIKTKT'
lcIndex ='INVHDR,INVLINEO,ORDHDR,ORDLINE,CONSINVH,CINVLINE,ORDCANLN,ORDPIK'
=gfSubStr(lcFiles,@laFiles,',')
=gfSubStr(lcIndex,@laIndex,',')

CREATE TABLE (gcWorkDir+lcTmpFile) (Invoice C(6),STORE C(8),ORDER C(6),LINENO N(6))
INDEX ON Invoice+STORE+ORDER+STR(LINENO,6) TAG (lcTmpFile)  OF (lcTmpFile)
lcExpr = gfOpGrid('SOCHECK' , .T.)
IF EMPTY(lcExpr)
  RETURN
ENDIF
= lfOpnFiles()

SELECT ORDHDR
IF lcExpr=".T."
  COUNT TO lnRecCount FOR STATUS $ "OC"
  lcExpr = 'STATUS $ "OC"'
ELSE
  COUNT TO lnRecCount FOR &lcExpr
ENDIF
SET RELATION TO
SET RELATION TO Ordhdr.cordtype+ Ordhdr.order INTO Ordcanln ADDITIVE
SET RELATION TO Ordhdr.cordtype+ Ordhdr.order INTO OrdLine ADDITIVE
SET RELATION TO Ordhdr.order INTO Invline  ADDITIVE
SET RELATION TO Ordhdr.order INTO Consinvl ADDITIVE
SELECT INVLINE
SET RELATION TO
SET RELATION TO Invline.invoice INTO Invhdr ADDITIVE
SELECT CONSINVL
SET RELATION TO
SET RELATION TO Consinvl.invoice+Consinvl.store+ Consinvl.order INTO Consinvh ADDITIVE
SELECT ORDHDR
SCAN FOR &lcExpr
  lnRecNo = lnRecNo + 1
  =gfThermo(lnRecCount , lnRecNo , '' ,"Checking order : " + ORDHDR.ORDER)
  *--Get the Cancelled Qty.
  IF SEEK(Ordhdr.cordtype+ Ordhdr.order ,'ORDCANLN') 
    SELECT ORDCANLN
    SCAN REST WHILE cordtype + order = Ordhdr.cordtype + Ordhdr.order
      lnordCnclQ = lnordCnclQ + ORDCANLN.TOTQTY
      lnordCnclA = lnordCnclA + (ORDCANLN.TOTQTY * ORDCANLN.PRICE)
    ENDSCAN
  ENDIF
  *-- Get Shipped Qty. from the order
  IF SEEK(Ordhdr.order , 'INVLINE')
    SELECT INVLINE
    SCAN REST WHILE ORDER = Ordhdr.order FOR INVHDR.STATUS<>'V'
      IF INVHDR.CONSOL<>'Y'
        IF !SEEK(INVOICE+STORE+ORDER+STR(LINENO,6),lcTmpFile)
          INSERT INTO &lcTmpFile (INVOICE,LINENO) VALUES (INVLINE.INVOICE,INVLINE.LINENO)
          lnordShipQ = lnordShipQ + TOTQTY
          lnOrdShipA = lnOrdShipA + (TOTQTY*Price)
        ENDIF
      ELSE
        IF SEEK(Ordhdr.order , 'CONSINVL')
          SELECT CONSINVL
          SCAN REST WHILE ORDER = Ordhdr.order FOR CONSINVH.STATUS<>'V'
            IF !SEEK(INVOICE+STORE+ORDER+STR(LINENO,6),lcTmpFile)
              INSERT INTO &lcTmpFile (INVOICE,STORE,ORDER,LINENO) VALUES (CONSINVL.INVOICE,;
                      CONSINVL.STORE,CONSINVL.ORDER,CONSINVL.LINENO)
              lnordShipQ = lnordShipQ + TotQty
              lnOrdShipA = lnOrdShipA + (TotQty*Price)
            ENDIF
          ENDSCAN
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
  IF SEEK(Ordhdr.order , 'CONSINVL')
    SELECT CONSINVL
    SCAN REST WHILE ORDER = Ordhdr.order FOR CONSINVH.STATUS<>'V'
      IF !SEEK(INVOICE+STORE+ORDER+STR(LINENO,6),lcTmpFile)
        INSERT INTO &lcTmpFile (INVOICE,STORE,ORDER,LINENO) VALUES (CONSINVL.INVOICE,;
                    CONSINVL.STORE,CONSINVL.ORDER,CONSINVL.LINENO)
        lnordShipQ = lnordShipQ + TotQty
        lnOrdShipA = lnOrdShipA + (TotQty*Price)
      ENDIF
    ENDSCAN
  ENDIF
  SELECT ORDHDR
  REPLACE Cancel    WITH lnordCnclQ 			,;
          CancelAmt WITH lnordCnclA				,;
          Ship      WITH lnordShipQ             ,;
          ShipAmt   WITH lnordShipA

  *--Get the Opened Qty from the Order.
  IF SEEK(Ordhdr.cordtype + Ordhdr.order , 'OrdLine')
    SELECT ORDLINE
    SCAN REST WHILE cOrdType + Order = Ordhdr.cordtype + Ordhdr.order FOR TOTQTY > 0
      lnordopenQ = lnordopenQ + TOTQTY
      lnordopenA = lnordopenA + (TOTQTY*PRICE)
      *ash1
      IF !EMPTY(PIKTKT) AND SEEK(Order+PikTkt,'PIKTKT')
        IF PIKTKT.STATUS $ 'XC'
          FOR I = 1 TO 8
            LCI = ALLTRIM(STR(I))
            REPLACE PIK&LCI WITH 0
          ENDFOR
          REPLACE TOTPIK WITH 0 , PIKTKT WITH '',PIKDATE WITH {}, PICKED WITH .F.
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
  SELECT ORDHDR
  REPLACE OPEN WITH lnordopenQ ,OPENAMT WITH lnordopenA
  IF OPEN = 0 AND STATUS $ 'OC' AND BOOK+SHIP+CANCEL=0
    REPLACE STATUS WITH 'X'
  ENDIF
  IF OPEN = 0 AND STATUS='O' AND BOOK > (SHIP+CANCEL)
    REPLACE OPEN WITH (BOOK-SHIP-CANCEL)
  ENDIF
  IF OPEN = 0 AND STATUS='C' AND BOOK > SHIP+CANCEL
    REPLACE STATUS WITH 'O',OPEN WITH (BOOK-SHIP-CANCEL)
  ENDIF
  IF OPEN = 0 AND STATUS='O' AND BOOK = SHIP+CANCEL 
    REPLACE STATUS WITH 'C'
  ENDIF
  IF OPEN = 0 AND STATUS='O' AND BOOK < SHIP+CANCEL 
    REPLACE STATUS WITH 'C'
  ENDIF
  IF OPEN > 0 AND STATUS='C' AND BOOK > SHIP+CANCEL 
    REPLACE STATUS WITH 'O'
  ENDIF
  IF OPEN > 0 AND STATUS='C' AND BOOK=SHIP+CANCEL
    REPLACE OPEN WITH 0 , OPENAMT WITH 0
  ENDIF
  IF OPEN > 0 AND STATUS='O' AND BOOK = SHIP+CANCEL
    REPLACE STATUS WITH 'C',OPEN WITH 0 , OPENAMT WITH 0
  ENDIF

  *-- if the Order is Completed then Zeroize the ordline Record for this order
  IF OrdHdr.Status = 'C' AND BOOK=SHIP+CANCEL AND ;
    SEEK(Ordhdr.cordtype + Ordhdr.order , 'OrdLine')
    SELECT ORDLINE
    SCAN REST WHILE cOrdType + Order = Ordhdr.cordtype + Ordhdr.order FOR TOTQTY > 0
      FOR I = 1 TO 8
        LCI = ALLTRIM(STR(I))
        REPLACE QTY&LCI WITH 0
        REPLACE PIK&LCI WITH 0
      ENDFOR
      REPLACE TOTQTY WITH 0 ,TOTPIK WITH 0 , PIKTKT WITH '',PIKDATE WITH {}, PICKED WITH .F.
    ENDSCAN
  ENDIF

  STORE 0 TO lnordShipQ , lnOrdShipA , lnordCnclQ , lnordCnclA , lnordopenQ , lnordopenA  
ENDSCAN
=gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'The SO files have checked and repaired successfully')
*--Use in the opened files.
FOR I = 1 TO ALEN(laFiles,1)
  IF USED(laFiles[I])
    USE IN &laFiles[I]
  ENDIF
ENDFOR
RETURN
*!*************************************************************
*! Name      : lfOpnFiles
*! Developer : Nader Nabil (NNA)
*! Date      : 01/04/2005
*! Purpose   : Open needed files if not opened before
*!*************************************************************
*!B125455,1
FUNCTION lfOpnFiles
FOR I = 1 TO ALEN(laFiles,1)
  IF !USED(laFiles[I])
    = gfOpenFile(gcDataDir+laFiles[I],gcDataDir+laIndex[I],'SH') 
  ENDIF
ENDFOR
*--End of function lfOpnFiles.