*:**************************************************************************
*: Program file  : SoFxDel.PRG C121665
*: Program desc. : Fix Program For Inf
*: System        : Aria 2.7
*: Developer     : MOHAMED SHOKRY (MHM)
*: Date          : 02/12/2004   
*:**************************************************************************
lnChoice =gfModalGen('QRM00000B42002','F',' ',' ','Start Cancelling Orders?')
IF lnChoice = 2
  RETURN
ENDIF

*-- Add this line to fix the bug "Variable llFromEDI not found" [Begin]
llFromEDI = IIF(TYPE('llFromEDI') = 'L' , llFromEDI , .F.)
*-- Add this line to fix the bug "Variable llFromEDI not found" [End]

PRIVATE lnTktCnt
STORE ' ' TO lcGlYear,lcGlPeriod

DECLARE laCodes[7,10],laCanReason[1,2]
STORE '' TO laCodes,laCanReason

  laCodes[1,1] = 'CTERMCODE'
  laCodes[1,2] = 'laTerms'
  laCodes[1,3] = 'lnTerms'
  laCodes[1,4] = ''
  laCodes[1,5] = .F.
  laCodes[1,6] = .F.
  laCodes[1,10] = 'cTermCode'

  laCodes[2,1] = 'SHIPVIA'
  laCodes[2,2] = 'laShipVia'
  laCodes[2,3] = 'lnShipVia'
  laCodes[2,4] = ''
  laCodes[2,5] = .F.
  laCodes[2,6] = .F.
  laCodes[2,10] = 'SHIPVIA'

  laCodes[3,1] = 'SPCINST'
  laCodes[3,2] = 'laSpcInst'
  laCodes[3,3] = 'lnSpcInst'
  laCodes[3,4] = ''
  laCodes[3,5] = .F.
  laCodes[3,6] = .F.
  laCodes[3,10] = 'SPCINST'

  laCodes[4,1] = 'SEASON'
  laCodes[4,2] = 'laSeasons'
  laCodes[4,3] = 'lnSeason'
  laCodes[4,4] = ''
  laCodes[4,5] = .T.
  laCodes[4,6] = .F.
  laCodes[4,10] = 'SEASON'

  laCodes[5,1] = 'CDIVISION'
  laCodes[5,2] = 'laDivision'
  laCodes[5,3] = 'lnDivision'
  laCodes[5,4] = ''
  laCodes[5,5] = .F.
  laCodes[5,6] = .F.
  laCodes[5,10] = 'cDivision'

  laCodes[6,1] = 'CCANCRESON'
  laCodes[6,2] = 'laCanReason'
  laCodes[6,3] = 'lnCanReason'
  laCodes[6,4] = ''
  laCodes[6,5] = .F.
  laCodes[6,6] = .F.
  laCodes[6,10] = 'CCANCRESON'

  laCodes[7,1] = 'CORDERCAT'
  laCodes[7,2] = 'laOCtgCd'
  laCodes[7,3] = 'lnOCtgCd'
  laCodes[7,4] = ''
  laCodes[7,5] = .F.
  laCodes[7,6] = .F.
  laCodes[7,10] = 'CORDERCAT'


*--Open All Needed file
*-- Open the order cancellation table if we have shipped qty.
=gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH')
=gfOpenFile(gcDataDir+'ORDLINE',gcDataDir+'ORDLINE','SH')
=gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')

=gfOpenFile(gcDataDir+'ORDCANLN',gcDataDir+'ORDCANLN','SH') 
IF 'AL' $ gcCmpModules
  =gfOpenFile(gcDataDir+'PIKTKT',gcDataDir+'ORDPIK','SH')
  =gfOpenFile(gcDataDir+'PIKLINE',gcDataDir+'PIKLINE','SH') 
  SET ORDER TO TAG 'PIKTKT' IN 'PIKTKT'
ENDIF

IF 'PO' $ gcCmpModules
  =gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
  =gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLN','SH')
ENDIF
IF 'MF' $ gcCmpModules
  =gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')
  =gfOpenFile(gcDataDir+'CUTTKTL',gcDataDir+'CUTLIN','SH')
  =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
ENDIF
=gfOpenFile(gcDataDir+'icStyHst',gcDataDir+'Styhst','SH') 
=gfOpenFile(gcDataDir+'arCusHst',gcDataDir+'Acthst','SH')

IF !USED('STYLE')
  =gfOpenFile(gcDataDir+'STYLE','STYLE','SH')
ENDIF
IF !USED('STYDYE')
  =gfOpenFile(gcDataDir+'STYDYE','STYDYE','SH')
ENDIF
IF !USED('SCALE')
  =gfOpenFile(gcDataDir+'SCALE','SCALE','SH')
ENDIF
*-- End Open Needed Files
SELECT ORDHDR

SCAN FOR  ORDHDR.Entered < {12/01/2003}
  *-- Uncancel order
  IF OrdHdr.Status<>'X'
    lcOrdType = cOrdType
    IF lcOrdType='O' .AND. !CHECKPRD(OrdHdr.Entered,'lcGlYear','lcGlPeriod ','',.T.)
    ENDIF
    SET ORDER TO TAG ORDLINE IN ORDLINE
    SET ORDER TO TAG ORDHDR  IN ORDHDR  

    IF ORDHDR.STATUS='C'
      LOOP
    ENDIF      
    IF ORDHDR.STATUS='B'
      SELECT ORDHDR
      =RLOCK()
      REPLACE STATUS     WITH 'X' ,;
              cCancReson WITH gfCodDes(cCancReson, 'CCANCRESON'),;
              Cancelled  WITH gdSysDate ,;
              Flag       WITH SPACE(1)
      UNLOCK
      LOOP
    ENDIF
    lnTktCnt = 0
    DIMENSION laTickets[1,2]

    m.cCancReson = gfCodDes(cCancReson, 'CCANCRESON')
    REPLACE cCancReson WITH m.cCancReson
    
    WAIT 'Cancelling and updating Order ' + OrdHdr.Order WINDOW NOWAIT 
    SELECT OrdLine
    STORE 0 TO lnOpen,lnOpenAmt,lnBook,lnBookAmt

    SCAN FOR ORDER = ORDHDR.ORDER
      *-- Decrease warehouse ordered quantity with order open quantity
      IF lcOrdType='O' .AND. SEEK(OrdLine.Style+OrdLine.cWareCode+SPACE(10),'StyDye')
        SELECT StyDye
        =RLOCK()
        REPLACE Ord1   WITH MAX(Ord1-OrdLine.Qty1,0) ,;
                Ord2   WITH MAX(Ord2-OrdLine.Qty2,0) ,;
                Ord3   WITH MAX(Ord3-OrdLine.Qty3,0) ,;
                Ord4   WITH MAX(Ord4-OrdLine.Qty4,0) ,;
                Ord5   WITH MAX(Ord5-OrdLine.Qty5,0) ,;
                Ord6   WITH MAX(Ord6-OrdLine.Qty6,0) ,;
                Ord7   WITH MAX(Ord7-OrdLine.Qty7,0) ,;
                Ord8   WITH MAX(Ord8-OrdLine.Qty8,0) ,;
                TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
        UNLOCK
        SELECT OrdLine
        *-- Decrease warehouse allocated quantity with order picked quantity
        IF OrdLine.PICKED 
          SELECT StyDye
          =RLOCK()
          REPLACE Alo1   WITH MAX(Alo1-OrdLine.Pik1,0) ,;
                  Alo2   WITH MAX(Alo2-OrdLine.Pik2,0) ,;
                  Alo3   WITH MAX(Alo3-OrdLine.Pik3,0) ,;
                  Alo4   WITH MAX(Alo4-OrdLine.Pik4,0) ,;
                  Alo5   WITH MAX(Alo5-OrdLine.Pik5,0) ,;
                  Alo6   WITH MAX(Alo6-OrdLine.Pik6,0) ,;
                  Alo7   WITH MAX(Alo7-OrdLine.Pik7,0) ,;
                  Alo8   WITH MAX(Alo8-OrdLine.Pik8,0) ,;
                  TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
          UNLOCK
          SELECT OrdLine
        ENDIF
      ENDIF
      *-- Decrease style ordered quantity with order open quantity
      IF lcOrdType='O' .AND. SEEK(OrdLine.STYLE,'Style')
        SELECT Style
        =RLOCK()
        REPLACE Ord1   WITH MAX(Ord1-OrdLine.Qty1,0) ,;
                Ord2   WITH MAX(Ord2-OrdLine.Qty2,0) ,;
                Ord3   WITH MAX(Ord3-OrdLine.Qty3,0) ,;
                Ord4   WITH MAX(Ord4-OrdLine.Qty4,0) ,;
                Ord5   WITH MAX(Ord5-OrdLine.Qty5,0) ,;
                Ord6   WITH MAX(Ord6-OrdLine.Qty6,0) ,;
                Ord7   WITH MAX(Ord7-OrdLine.Qty7,0) ,;
                Ord8   WITH MAX(Ord8-OrdLine.Qty8,0) ,;
                TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
        UNLOCK
        SELECT OrdLine
        *-- Decrease style allocated quantity with order picked quantity
        IF OrdLine.PICKED 
          SELECT Style
          =RLOCK()
          REPLACE Alo1   WITH MAX(Alo1-OrdLine.Pik1,0) ,;
                  Alo2   WITH MAX(Alo2-OrdLine.Pik2,0) ,;
                  Alo3   WITH MAX(Alo3-OrdLine.Pik3,0) ,;
                  Alo4   WITH MAX(Alo4-OrdLine.Pik4,0) ,;
                  Alo5   WITH MAX(Alo5-OrdLine.Pik5,0) ,;
                  Alo6   WITH MAX(Alo6-OrdLine.Pik6,0) ,;
                  Alo7   WITH MAX(Alo7-OrdLine.Pik7,0) ,;
                  Alo8   WITH MAX(Alo8-OrdLine.Pik8,0) ,;
                  TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
          UNLOCK
          SELECT OrdLine
        ENDIF
      ENDIF
      IF lcOrdType='O'  AND OrdLine.PICKED AND !EMPTY(OrdLine.Dyelot) AND ;
        SEEK(OrdLine.Style+OrdLine.cWareCode+OrdLine.Dyelot,'StyDye')
        SELECT StyDye
        =RLOCK()
        REPLACE Alo1   WITH MAX(Alo1-OrdLine.Pik1,0) ,;
                Alo2   WITH MAX(Alo2-OrdLine.Pik2,0) ,;
                Alo3   WITH MAX(Alo3-OrdLine.Pik3,0) ,;
                Alo4   WITH MAX(Alo4-OrdLine.Pik4,0) ,;
                Alo5   WITH MAX(Alo5-OrdLine.Pik5,0) ,;
                Alo6   WITH MAX(Alo6-OrdLine.Pik6,0) ,;
                Alo7   WITH MAX(Alo7-OrdLine.Pik7,0) ,;
                Alo8   WITH MAX(Alo8-OrdLine.Pik8,0) ,;
                TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
        UNLOCK
        SELECT OrdLine
      ENDIF

      *-- Decrease ordered quantity & amount in the style history with order open quantity 
      IF lcOrdType='O'  .AND. SEEK(Style+lcGlYear,'icStyHst')

        lnOrdAmt = OrdLine.TotQty*OrdLine.Price 
        
        SELECT icStyHst
        =RLOCK()
        REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod - OrdLine.TotQty ,;
                nOrdQty            WITH nOrdQty            - OrdLine.TotQty ,;
                nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod - lnOrdAmt ,;
                nOrdAmt            WITH nOrdAmt            - lnOrdAmt
        UNLOCK  
        SELECT OrdLine
      ENDIF
      *-- Release pick ticket and zero out order picked quantity
      IF OrdLine.Picked
        IF SEEK(OrdLine.PikTkt,'PIKTKT')
          SELECT PIKTKT
          =RLOCK()
          REPLACE STATUS WITH 'X'
          UNLOCK
        ENDIF
        SELECT ORDLINE
        IF !SEEK(PikTkt+Order+STR(LineNo,6),'PIKLINE')      
          SCATTER TO laPikLine
          INSERT INTO PIKLINE FROM ARRAY laPikLine
        ENDIF
        SELECT ORDLINE
        =RLOCK()
        REPLACE PIK1   WITH 0 ,;
                PIK2   WITH 0 ,;
                PIK3   WITH 0 ,;
                PIK4   WITH 0 ,;
                PIK5   WITH 0 ,;
                PIK6   WITH 0 ,;
                PIK7   WITH 0 ,;
                PIK8   WITH 0 ,;
                TOTPIK WITH 0 ,;
                PICKED WITH .F. ,;
                PIKTKT WITH SPACE(6) ,;
                PIKDATE WITH {}
        IF SEEK(AltStyle,'STYLE') .AND. SEEK('S'+STYLE.Scale,'SCALE')
          REPLACE Style     WITH AltStyle,;
                  AltStyle  WITH SPACE(19),;
                  Scale     WITH SCALE.Scale,;
                  cWareCode WITH OrdHdr.cWareCode
          REPLACE Qty1  WITH IIF(Scale.Cnt>=1,Qty1,0)  ,;
                  Book1 WITH IIF(Scale.Cnt>=1,Book1,0) ,;
                  Qty2  WITH IIF(Scale.Cnt>=2,Qty2,0)  ,;
                  Book2 WITH IIF(Scale.Cnt>=2,Book2,0) ,;
                  Qty3  WITH IIF(Scale.Cnt>=3,Qty3,0)  ,;
                  Book3 WITH IIF(Scale.Cnt>=3,Book3,0) ,;
                  Qty4  WITH IIF(Scale.Cnt>=4,Qty4,0)  ,;
                  Book4 WITH IIF(Scale.Cnt>=4,Book4,0) ,;
                  Qty5  WITH IIF(Scale.Cnt>=5,Qty5,0)  ,;
                  Book5 WITH IIF(Scale.Cnt>=5,Book5,0) ,;
                  Qty6  WITH IIF(Scale.Cnt>=6,Qty6,0)  ,;
                  Book6 WITH IIF(Scale.Cnt>=6,Book6,0) ,;
                  Qty7  WITH IIF(Scale.Cnt>=7,Qty7,0)  ,;
                  Book7 WITH IIF(Scale.Cnt>=7,Book7,0) ,;
                  Qty8  WITH IIF(Scale.Cnt>=8,Qty8,0)  ,;
                  Book8 WITH IIF(Scale.Cnt>=8,Book8,0) ,;
                  TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
                  TotBook WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8
        ENDIF
        UNLOCK
      ENDIF
      *-- Release order allocated quantity to any cutting ticket order style purchae order
      IF ORDHDR.TotCut > 0
        SELECT CutPick
        DO WHILE SEEK(IIF(Style.Make,'1','2')+OrdLine.Order+STR(OrdLine.LineNo,6))
          SELECT IIF(CutPick.TranCd='1','CutTktH','PosHdr')
          =SEEK(IIF(CutPick.TranCd='1','','P')+CutPick.CTKTNO)
          =RLOCK()
          REPLACE TotOrd WITH MAX(TotOrd-CUTPICK.TotQty,0)
          UNLOCK
          SELECT IIF(CutPick.TranCd='1','CutTktL','PosLn')
          =SEEK(IIF(CutPick.TranCd='2','P','')+;
                    CutPick.CTKTNO+CutPick.Style+CutPick.cTktLineNo+'1')
          =RLOCK()
          REPLACE ORD1   WITH MAX(ORD1-CUTPICK.Qty1,0) ,;
                  ORD2   WITH MAX(ORD2-CUTPICK.Qty2,0) ,;
                  ORD3   WITH MAX(ORD3-CUTPICK.Qty3,0) ,;
                  ORD4   WITH MAX(ORD4-CUTPICK.Qty4,0) ,;
                  ORD5   WITH MAX(ORD5-CUTPICK.Qty5,0) ,;
                  ORD6   WITH MAX(ORD6-CUTPICK.Qty6,0) ,;
                  ORD7   WITH MAX(ORD7-CUTPICK.Qty7,0) ,;
                  ORD8   WITH MAX(ORD8-CUTPICK.Qty8,0) ,;
                  TOTORD WITH MAX(TOTORD-CUTPICK.TotQty,0)
          UNLOCK 
          lnTicket=ASCAN(laTickets,CutPick.CTKTNO)
          IF lnTicket <> 0
            llUpdate = (laTickets[lnTicket+1]='Y')
          ENDIF
          *-- Update hold cutting tickets order purchase orders budget quantity
          IF llUpdate
            SELECT IIF(CutPick.TranCd='1','CutTktH','PosHdr')
            =RLOCK()
            IF CutPick.TranCd = '1'
              REPLACE Pcs_Bud WITH Pcs_Bud - CUTPICK.TotQty ,;
                      Pcs_Opn WITH Pcs_Opn - CUTPICK.TotQty
            ELSE
              REPLACE nStyOrder WITH nStyOrder - CUTPICK.TotQty ,;
                      Open      WITH Open  - CUTPICK.TotQty
            ENDIF
            UNLOCK
            SELECT IIF(CutPick.TranCd='1','CutTktL','PosLn')
            =RLOCK()
            REPLACE Qty1   WITH Qty1-CUTPICK.Qty1 ,;
                    Qty2   WITH Qty2-CUTPICK.Qty2 ,;
                    Qty3   WITH Qty3-CUTPICK.Qty3 ,;
                    Qty4   WITH Qty4-CUTPICK.Qty4 ,;
                    Qty5   WITH Qty5-CUTPICK.Qty5 ,;
                    Qty6   WITH Qty6-CUTPICK.Qty6 ,;
                    Qty7   WITH Qty7-CUTPICK.Qty7 ,;
                    Qty8   WITH Qty8-CUTPICK.Qty8 ,;
                    TotQty WITH TotQty - CUTPICK.TotQty
            UNLOCK
          ENDIF
          SELECT CUTPICK
          DELETE
        ENDDO
      ENDIF

      IF llUpdate
        IF lcOrdType='O' .AND. SEEK(OrdLine.Style+OrdLine.cWareCode+SPACE(10),'StyDye')
          SELECT StyDye
          =RLOCK()
          REPLACE WIP1   WITH MAX(WIP1-OrdLine.Qty1,0) ,;
                  WIP2   WITH MAX(WIP2-OrdLine.Qty2,0) ,;
                  WIP3   WITH MAX(WIP3-OrdLine.Qty3,0) ,;
                  WIP4   WITH MAX(WIP4-OrdLine.Qty4,0) ,;
                  WIP5   WITH MAX(WIP5-OrdLine.Qty5,0) ,;
                  WIP6   WITH MAX(WIP6-OrdLine.Qty6,0) ,;
                  WIP7   WITH MAX(WIP7-OrdLine.Qty7,0) ,;
                  WIP8   WITH MAX(WIP8-OrdLine.Qty8,0) ,;
                  TotWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
 
          REPLACE NWO1   WITH MAX(NWO1-OrdLine.Qty1,0) ,;
                  NWO2   WITH MAX(NWO2-OrdLine.Qty2,0) ,;
                  NWO3   WITH MAX(NWO3-OrdLine.Qty3,0) ,;
                  NWO4   WITH MAX(NWO4-OrdLine.Qty4,0) ,;
                  NWO5   WITH MAX(NWO5-OrdLine.Qty5,0) ,;
                  NWO6   WITH MAX(NWO6-OrdLine.Qty6,0) ,;
                  NWO7   WITH MAX(NWO7-OrdLine.Qty7,0) ,;
                  NWO8   WITH MAX(NWO8-OrdLine.Qty8,0) ,;
                  NTotWO WITH NWO1+NWO2+NWO3+NWO4+NWO5+NWO6+NWO7+NWO8
  
          UNLOCK
        ENDIF
        *-- Decrease style ordered quantity with order open quantity
        IF lcOrdType='O' .AND. SEEK(OrdLine.STYLE,'Style')
          SELECT Style
          =RLOCK()
          REPLACE WIP1   WITH MAX(WIP1-OrdLine.Qty1,0) ,;
                  WIP2   WITH MAX(WIP2-OrdLine.Qty2,0) ,;
                  WIP3   WITH MAX(WIP3-OrdLine.Qty3,0) ,;
                  WIP4   WITH MAX(WIP4-OrdLine.Qty4,0) ,;
                  WIP5   WITH MAX(WIP5-OrdLine.Qty5,0) ,;
                  WIP6   WITH MAX(WIP6-OrdLine.Qty6,0) ,;
                  WIP7   WITH MAX(WIP7-OrdLine.Qty7,0) ,;
                  WIP8   WITH MAX(WIP8-OrdLine.Qty8,0) ,;
                  TotWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
  
          REPLACE NWO1   WITH MAX(NWO1-OrdLine.Qty1,0) ,;
                  NWO2   WITH MAX(NWO2-OrdLine.Qty2,0) ,;
                  NWO3   WITH MAX(NWO3-OrdLine.Qty3,0) ,;
                  NWO4   WITH MAX(NWO4-OrdLine.Qty4,0) ,;
                  NWO5   WITH MAX(NWO5-OrdLine.Qty5,0) ,;
                  NWO6   WITH MAX(NWO6-OrdLine.Qty6,0) ,;
                  NWO7   WITH MAX(NWO7-OrdLine.Qty7,0) ,;
                  NWO8   WITH MAX(NWO8-OrdLine.Qty8,0) ,;
                  NTotWO WITH NWO1+NWO2+NWO3+NWO4+NWO5+NWO6+NWO7+NWO8
  
          UNLOCK
        ENDIF
      ENDIF
 
      SELECT ORDLINE
      *-- Release any cutting ticket order.
      =RLOCK()
      REPLACE Cut1   WITH 0 ,;
              Cut2   WITH 0 ,;
              Cut3   WITH 0 ,;
              Cut4   WITH 0 ,;
              Cut5   WITH 0 ,;
              Cut6   WITH 0 ,;
              Cut7   WITH 0 ,;
              Cut8   WITH 0 ,;
              TotCut WITH 0
      UNLOCK
      lnOpen    = lnOpen    + OrdLine.TotQty
      lnOpenAmt = lnOpenAmt + OrdLine.TotQty*OrdLine.Price
      lnBook    = lnBook    + OrdLine.TotBook
      lnBookAmt = lnBookAmt + OrdLine.TotBook*OrdLine.Price

      IF ORDHDR.Ship > 0 .AND. ORDLINE.TOTQTY > 0
          INSERT INTO ('ORDCANLN') ;
            (cOrdType,Order,LineNo,Cancelled,cCancReson,Qty1,Qty2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY, ;
             ACCOUNT,STYLE,STORE,DYELOT,Price) ;
            VALUES ;
            (lcOrdType,OrdHdr.Order,OrdLine.LineNo,gdSysDate,OrdHdr.cCancReson,OrdLine.Qty1,OrdLine.Qty2,;
            OrdLine.Qty3,OrdLine.Qty4,OrdLine.Qty5,OrdLine.Qty6,OrdLine.Qty7,OrdLine.Qty8,;
            OrdLine.TOTQty,ORDLINE.ACCOUNT,ORDLINE.STYLE,ORDLINE.STORE,ORDLINE.DYELOT,OrdLine.Price)
 
        REPLACE QTY1   WITH 0 ,;
                QTY2   WITH 0 ,;
                QTY3   WITH 0 ,;
                QTY4   WITH 0 ,;
                QTY5   WITH 0 ,;
                QTY6   WITH 0 ,;
                QTY7   WITH 0 ,;
                QTY8   WITH 0 ,;
                TOTQTY WITH 0 
      ENDIF
    
    ENDSCAN
    *-- llOrdCan ----------> check if ORDCANLN is Open Or Not
    llOrdCan  = !USED('ORDCANLN') .AND. gfOpenFile(gcDataDir+'ORDCANLN',gcDataDir+'ORDCANLN','SH')
    IF SEEK(lcOrdType+OrdHdr.Order,'ORDCANLN')
      SELECT ORDCANLN
      SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = lcOrdType+OrdHdr.Order
        IF SEEK(cOrdType+Order+STR(LineNo,6),'OrdLine')
          LOOP
        ENDIF
        lnBook    = lnBook    + ORDCANLN.TotQty
        lnBookAmt = lnBookAmt + ORDCANLN.TotQty*ORDCANLN.Price
      ENDSCAN  
    ENDIF  
    IF llOrdCan  
      =gfCloseFile('ORDCANLN')
    ENDIF  
    *-- Decrease number of bulk orders for this account
    IF OrdHdr.BULK='Y'  .AND. SEEK('M'+ORDHDR.ACCOUNT,'Customer')
      SELECT CUSTOMER
      =RLOCK()
      REPLACE nBulk WITH nBulk - 1
      UNLOCK
      SELECT OrdHdr
    ENDIF
    *-- Decrease orders quantity and amount in the customerr history file
    IF lcOrdType='O' 
      =SEEK(OrdHdr.ACCOUNT+lcGlYear,'arCusHst')
      lnOrdAmt = OrdHdr.OpenAmt 
      
      SELECT arCusHst
      =RLOCK()
      REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod - OrdHdr.Open ,;
              nOrdQty            WITH nOrdQty            - OrdHdr.Open ,;
              nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod - lnOrdAmt    ,;
              nOrdAmt            WITH nOrdAmt            - lnOrdAmt
      UNLOCK
      SELECT OrdHdr
    ENDIF
   
    *-- Update order open, Cancel and book quantity and amount and order status
    SELECT ORDHDR
    =RLOCK()
  
    REPLACE STATUS     WITH IIF(Ship > 0,'C','X') ,;
            Cancelled  WITH gdSysDate ,;
            Cancel     WITH IIF(OrdHdr.Bulk='Y',Cancel ,Cancel+lnOpen),;
            CancelAmt  WITH IIF(OrdHdr.Bulk='Y',CancelAmt ,CancelAmt+lnOpenAmt),;
            Open       WITH 0,;
            OpenAmt    WITH 0,;
            TotCut     WITH 0,;
            Book       WITH IIF(OrdHdr.Bulk='Y',Ship + Cancel,lnBook)    ,;
            BookAmt    WITH IIF(OrdHdr.Bulk='Y',ShipAmt + CancelAmt,lnBookAmt) ,;
            Flag       WITH SPACE(1)
    UNLOCK  
    SELECT OrdHdr
  ENDIF
ENDSCAN

WAIT CLEAR
=gfModalGen('INM00000B40011','DIALOG','','','Orders have been successfully Cancelled.')

IF USED('ORDHDR')
  USE IN ORDHDR
ENDIF
IF USED('ORDLINE')
  USE IN ORDLINE
ENDIF
IF USED('CODES')
  USE IN CODES
ENDIF
IF USED('ORDCANLN')
  USE IN ORDCANLN
ENDIF
IF USED('PIKTKT')
  USE IN PIKTKT
ENDIF
IF USED('PIKLINE')
  USE IN PIKLINE
ENDIF
IF USED('POSHDR')
  USE IN POSHDR
ENDIF
IF USED('POSLN')
  USE IN POSLN
ENDIF
IF USED('CUTPICK')
  USE IN CUTPICK
ENDIF
IF USED('CUTTKTL')
  USE IN CUTTKTL
ENDIF
IF USED('CUTTKTH')
  USE IN CUTTKTH
ENDIF
IF USED('Styhst')
  USE IN Styhst
ENDIF
IF USED('Acthst')
  USE IN Acthst
ENDIF
IF USED('STYLE')
  USE IN STYLE
ENDIF
IF USED('STYDYE')
  USE IN STYDYE
ENDIF

IF USED('SCALE')
  USE IN SCALE
ENDIF

IF USED('GLACCHAR')
  USE IN GLACCHAR
ENDIF



