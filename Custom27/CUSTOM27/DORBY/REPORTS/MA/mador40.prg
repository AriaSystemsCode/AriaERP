************************************************************************
*: Program file   : MADOR40.PRG (C# 101509)
*: Program desc.  : Material Inventory report for Dorby.
*:         System : Aria Apparel System
*:      Developer : AHMED SALAH SHALABY _ (SSH)
*:      Module    : MATERIAL (MA)
*:************************************************************************
*: Calls :PROCEDURES :
*:        FUNCTIONS  : 
************************************************************************
*: Passed Parameters  : None
************************************************************************
*:Modifications :
*:B#802735,1 SSH  28/10/99 1- Filter on shipper code (Make it as PopUp)
*:B#802735,1 SSH           2- Negative Signe for Style Po Yards
*:B#802735,1 SSH           3- SubStr from contents of 35
*:B802909,1 SHA 12/23/99 Aligned the header of the report. 
*:B803059,1 HDM 02/22/2000 read the style information from the BOMLine file
*:                         instead of the ctktbom file
*:B803171,1 SHA 03/29/2000 Some modifications in the report.
*:C101760,1 SSH Add new option grid criteria,and filter all transaction on the season field.
*:B603624,1 SSH Fix the bug of not print MAt PO's.
*C101894,1 SHA 06/02/2000 Changed the way of calculating the current balance.
***********************************************************************

*--- lfChkFlt :: Function To Check If Location Or Color Or Type Arrays Changed
llChanged = .F.
IF llOgFltCh OR lfChkFlt()
  DIME laRpOL[1],laRpOC[1],laRpOT[1],laRpOPAT[1]
  STORE '' TO laRpOL,laRpOC,laRpOT
  =ACOPY(laRpTarget,laRpOL)
  =ACOPY(laRpTColor,laRpOC)
  =ACOPY(laRpTType,laRpOT)
  =ACOPY(laRpPatTar,laRpOPAT)
  *:C101760,1 SSH  [begin]
  lDate1 = {}
  lDate2 = {}
  lnDatePo = lfItmPos('FINVTADJ.DATE','F')
  IF !EMPTY(laOgFXFlt[lnDatePo,6])
    lDate1 = CTOD(SUBSTR(laOgFXFlt[lnDatePo,6],1,10))
    lDate2 = CTOD(SUBSTR(laOgFXFlt[lnDatePo,6],12))
  ENDIF
  *:C101760,1 SSH  [end]
  llMWHous = (gfGetMemVar('M_WareHouse') = 'Y')
  lcClrStr = ''
  lcTypStr = ''
  lcLocStr = ''
  lcFabFlt = '.T.'
  =lfvClrTyp()
  =lfFabDRec()
  IF !lfColTran()
    SET DEVI TO SCREE
    RETURN
  ENDIF
ENDIF
lnTotLst = 0
STORE 0 TO lnTotLst,lnDolrFg,lnYardRed,lnDolrRed
PAGENO   = 0
ROW      = 0
R_TITLE  = [Material Inventory Report]+IIF(lcRpFormat = 'D','  [Detail Form]',;
                                                            '  [Summary Form]')
XTITLE   = lcRpTit
SELECT (lcRpTmp)
lcPNA = '.T.'
DO CASE
  CASE  lcRpPNA  = 'N'
    lcPNA = 'TYards < 0'
  CASE  lcRpPNA  = 'P'
    lcPNA = 'TYards > 0'
ENDCASE
SET FILTER TO &lcPNA
GOTO TOP
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVI TO SCREE
  RETURN
ENDIF
SET DEVICE TO PRINT
=lfPrnHdr()
=lfPrnBody()
DO ENDREPORT
SET DEVI TO SCREE
RETURN

*!*************************************************************
*! Name      : lfPrnBody
*! Developer : AHMED SALAH SHALABY _ (SSH)
*! Date      : 07/30/1999
*! Purpose   : Function to Print body.
*!*************************************************************
*! Calls     : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return    : None
*!*************************************************************
*
FUNCTION lfPrnBody
SELECT (lcRpTmp)
GOTO TOP
WAIT WINDOW 'Printing Report press <SPACE BAR> to abort ' NOWAIT
SCAN WHILE INKEY() <> 32
  lcFabric = Fabric
  lcColor  = Color
  lcWare   = cWareCode
  @ ROW,001 SAY Vendor
  @ ROW,010 SAY PATTERN
  @ ROW,021 SAY COLOR
  @ ROW,028 SAY WIDTH
  *:B#802735,1 SSH 31/10/99 SubStr from contents of 35 and alignment the report.
  
  *@ ROW,035 SAY Content
  @ ROW,035 SAY SUBSTR(Content,1,35)
  
  *@ ROW,096 SAY cWareCode
  @ ROW,071 SAY cWareCode
  
  *@ ROW,107 SAY TYards  PICTURE "999999"
  
  *C101894,1 SHA(Begin)Changed to include the balance of the transactions that are not 
  *C101894,1           included in the report.
  *@ ROW,77 SAY TYards  PICTURE "999999"
  @ ROW,77 SAY TYards + TDiff PICTURE "999999"

  *lnTotLst = lnTotLst + IIF(TYards>0,TYards,0)
  lnTotLst = lnTotLst + IIF(TYards + TDiff>0,TYards + TDiff,0)
  *C101894,1 SHA(End)
   
  *@ ROW,114 SAY SPACE(08)
  @ ROW,84 SAY SPACE(08)
  
  *@ ROW,123 SAY IIF(SEEK(FABRIC+COLOR,'FABRIC'),FABRIC.nFAve_Cost,0)
  @ ROW,93 SAY IIF(SEEK(FABRIC+COLOR,'FABRIC'),FABRIC.nFAve_Cost,0)

  lnDolrFg = lnDolrFg + (IIF(TYards>0,TYards,0) * FABRIC.nFAve_Cost)
  *@ ROW,134 SAY 'Current Balance'
  @ ROW,106	 SAY 'Current Balance'
  
  *@ ROW,150 SAY ''
  @ ROW,123 SAY ''
  
  
  SELECT DFABRIC
  =SEEK(&lcRpLTmp..Type+&lcRpLTmp..Tran+lcFabric+lcColor+lcWare)
  *@ ROW,165 SAY SUBSTR(DFABRIC.Customer,1,22)
  *@ ROW,130 SAY SUBSTR(DFABRIC.Customer,1,22)

  IF lcRpFormat = 'D'
    SELECT (lcRpTmp)
    *C101894,1 SHA(Begin)Added to print the new generated transaction.
    IF TDiff <> 0
      ROW = ROW + 1
      @ ROW,77 SAY TDiff PICTURE "999999"
      @ ROW,106 SAY 'Balance Transfer'
    ENDIF
    *C101894,1 SHA(End)

    
    ROW = ROW + 1
    @ ROW,001 SAY IIF(SEEK(VENDOR,'APVENDOR'),APVENDOR.cVenComp,'')
    IF ROW > 63
        =lfPrnHdr()
      ENDIF
  ENDIF 
  SELECT (lcRpLTmp)
  IF SEEK(lcFabric+lcColor+lcWare)
    SCAN REST WHILE Fabric+ Color+ cWareCode = ;
                    lcFabric+lcColor+lcWare
      IF lcRpFormat = 'D'
        DO CASE
          CASE TYPE = 'A'
            =SEEK(lcWare+lcFabric+lcColor+Tran,'DFINVADJ')
            lcLoc = DFINVADJ.cReason
          CASE TYPE $ 'PT'
            lnOld = SELECT(0)
            SELECT FINVTADJ
            =SEEK(lcFabric+lcColor,'FINVTADJ')
            LOCATE REST FOR FINVTADJ.cTrn_Seq = &lcRpLTmp..Tran
            lcLoc = FINVTADJ.cReason
            SELECT(lnOld)
          CASE TYPE = 'M'
            =SEEK(&lcRpLTmp..Type+&lcRpLTmp..Tran+lcFabric+lcColor+lcWare,'DFABRIC')
            lcLoc = DFABRIC.Comments
          CASE TYPE = 'S'
            =SEEK('P'+Tran,'POSHDR')
            lcLoc = IIF(PosHdr.Lreqed,'Yes','No')
            *B803171,1 SHA(Begin)Added to pull up the customer # from 
            *B803171              POSHDR file.
            lcBuyer = SUBSTR(POSHDR.Buyer,1,22)
            *B803171,1 SHA(End)
        ENDCASE
        *@ ROW,099-(LEN(ALLTRIM(lcLoc))) SAY ALLTRIM(lcLoc)
        *B803171,1 SHA(Begin)Changed the positions
        *@ ROW,074-(LEN(ALLTRIM(lcLoc))) SAY ALLTRIM(lcLoc)
        @ ROW,074-(LEN(ALLTRIM(lcLoc)) + 5 ) SAY ALLTRIM(lcLoc)
        *B803171,1 SHA(End)
        *:B#802735,1 SSH 28/10/99  Negative Signe for Style Po Yards
        *lnPull = IIF(Added<>0,Added - Pulled,IIF(TYPE$'PT',-Pulled,Pulled))
        lnPull = IIF(Added<>0,Added - Pulled,IIF(TYPE$'PTS',-Pulled,Pulled))
        *:B#802735,1 SSH(END).
        *@ ROW,107 SAY lnPull   PICTURE "999999"
        @ ROW,77 SAY lnPull   PICTURE "999999"
      ENDIF
      lnYardRed = lnYardRed + Pulled
      lnDolrRed  = lnDolrRed + (Pulled * FABRIC.nFAve_Cost)
      IF lcRpFormat = 'D'
        *@ ROW,117 SAY ShpDate
        @ ROW,87 SAY ShpDate
        
        *@ ROW,127 SAY Tran
        @ ROW,97 SAY Tran
        
        IF TYPE = 'S'
          *@ ROW,132 SAY Unit PICTURE "99999999"
          @ ROW,102 SAY Unit PICTURE "99999999"
        ENDIF
        *@ ROW,144 SAY Style
        @ ROW,114 SAY Style

        SELECT DFABRIC
        =SEEK(&lcRpLTmp..Type+&lcRpLTmp..Tran+lcFabric+lcColor+lcWare)
        *@ ROW,165 SAY SUBSTR(DFABRIC.Customer,1,22)
        *B803171,1 SHA(Begin)Changed to pick up the customer# from 
        *B803171,1           POSHDR file instead of DFABRIC file in case 
        *B803171,1           of transactions with type "S"
        *@ ROW,135 SAY SUBSTR(DFABRIC.Customer,1,22)
        @ ROW,135 SAY IIF(&lcRpLTmp..Type = "S",lcBuyer,SUBSTR(DFABRIC.Customer,1,22))
        *B803171,1 SHA(End)
        *:B#802735,1 SSH (END)        
        ROW = ROW + 1
        IF ROW > 63
         =lfPrnHdr()
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
  ROW = ROW + 1
  IF ROW > 63
   =lfPrnHdr()
  ENDIF
ENDSCAN
ROW = ROW + 3
IF ROW > 63
   =lfPrnHdr()
ENDIF
@ ROW , 080 SAY 'Total Listing    : '
@ ROW , 105 SAY lnTotLst   PICTURE "99999999"
@ ROW , 118 SAY lnDolrFg      PICTURE "9999999999999.999"
ROW = ROW + 2
IF ROW > 63
   =lfPrnHdr()
ENDIF
@ ROW , 080 SAY 'Yard Reduction   : '  
@ ROW , 105 SAY lnYardRed  PICTURE "99999999"
ROW = ROW + 1
IF ROW > 63
   =lfPrnHdr()
ENDIF
@ ROW , 080 SAY 'Dollar Reduction : '
@ ROW , 101 SAY lnDolrRed   PICTURE "99999999.999"


*!*************************************************************
*! Name      : lfColTran
*! Developer : AHMED SALAH SHALABY _ (SSH)
*! Date      : 07/30/1999
*! Purpose   : Function to Colect Trans.
*!*************************************************************
*! Calls     : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return    : None
*!*************************************************************
*
FUNCTION lfColTran

SELECT (lcRpTmp)
GOTO TOP
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN(.F.)
ENDIF
CREATE TABLE &gcWorkDir.&lcRpLTmp (TYPE C(1),FABRIC C(07)  , COLOR C(06) ,;
                                   Cwarecode C(6), Pulled N(10),Added N(10),;
                                   ShpDate D,Tran C(6),UNIT N(10),;
                                   Style C(19), CUSTOMER C(30))
*:C101760,1 SSH  Begin
*INDEX ON FABRIC+COLOR+cWareCode TAG (lcRpLTmp)
INDEX ON FABRIC+COLOR+cWareCode+DTOS(ShpDate) TAG (lcRpLTmp)
*:C101760,1 SSH  End
SELECT (lcRpTmp)
GOTO TOP
lntotYard = 0
WAIT WINDOW 'Colecting Trans. press <SPACE BAR> to abort ' NOWAIT

SCAN WHILE INKEY() <> 32
*--- Start Colecting Inventory Adj. Trans. From DfInvAdj
  SCAT MEMVAR MEMO
  WAIT WINDOW 'Selecting Trans. For Item /Color : '+m.Fabric +'/'+m.Color NOWAIT
  lntotYard = 0
  *C101894,1 SHA(Begin)Added to initialize the variable that will hold the balance diff.
  lnDiff = 0 
  *C101894,1 SHA(End)
  m.Pulled  = 0
  SELECT DFINVADJ
  IF llMWHous
    SET ORDER TO DFINVADJ
    =SEEK(m.cWareCode+m.Fabric+m.Color)
    lcKey = 'cfromware+fabric+color+ctrn_seq'
    lcVal = m.cWareCode+m.Fabric+m.Color
  ELSE
    SET ORDER TO FINVTADJ
    =SEEK(m.Fabric+m.Color)
    lcKey = 'fabric+color'
    lcVal = m.Fabric+m.Color
  ENDIF
  *:C101760,1 SSH [Begin] Add date to the filter
  *SCAN REST WHILE &lcKey = lcVal FOR TYPE = 'A'
  
  *C101894,1 SHA(Begin)Changed the way of collecting the inventory adj. transctions.
  *SCAN REST WHILE &lcKey = lcVal FOR TYPE = 'A' .AND. ;
                                     IIF(lDate2={} .AND. lDate1 <> {} ,Date>=lDate1,;
                                     IIF(lDate2={} .AND. lDate1={},.T.,BETWEEN(Date,lDate1,lDate2)))
  *:C101760,1 SSH [END]
  
  SCAN REST WHILE &lcKey = lcVal FOR TYPE = 'A' 
  
    m.Added     = IIF(DFINVADJ.NmTotAdj > 0 ,DFINVADJ.NmTotAdj ,0)
    m.Pulled    = IIF(DFINVADJ.NmTotAdj < 0 ,DFINVADJ.NmTotAdj ,0)
    IF IIF(lDate2={} .AND. lDate1 <> {} ,Date>=lDate1,;
       IIF(lDate2={} .AND. lDate1={},.T.,BETWEEN(Date,lDate1,lDate2)))
      m.TYPE      = 'A'
      *m.Added     = IIF(DFINVADJ.NmTotAdj > 0 ,DFINVADJ.NmTotAdj ,0)
      *m.Pulled    = IIF(DFINVADJ.NmTotAdj < 0 ,DFINVADJ.NmTotAdj ,0)
      lntotYard   = lntotYard +(m.Added-m.Pulled)
      m.ShpDate   = Date
      m.Tran      = cTrn_Seq
      m.UNIT      = 0
      m.Style     = ''
      INSERT INTO (lcRpLTmp) FROM MEMVAR
    ELSE
      lnDiff      = lnDiff + (m.Added-m.Pulled)
    ENDIF
  ENDSCAN
  *C101894,1 SHA(End)
  
*--- Start Colecting Transfere and Physical Trans. From FInvTAdj
  SELECT FINVTADJ
  =SEEK(m.FABRIC+m.Color)
  *:C101760,1 SSH [Begin] Add date to the filter
  *SCAN REST WHILE Fabric + Color = m.FABRIC+m.Color FOR Type $ 'PT';
            AND IIF(llMWHous,((cFromWare = m.cWareCode) OR ;
                              (cToWare   = m.cWareCode)),.T.)

  *C101894,1 SHA(Begin)Changed the way of calculating the transfer and physical trans.
  *SCAN REST WHILE Fabric + Color = m.FABRIC+m.Color FOR Type $ 'PT';
                                                      .AND. IIF(llMWHous,((cFromWare = m.cWareCode) OR ;
                                                             (cToWare   = m.cWareCode)),.T.)  ;
                                                     .AND.  IIF(lDate2={} .AND. lDate1 <> {} ,Date>=lDate1,;
                                                            IIF(lDate2={} .AND. lDate1={},.T.,BETWEEN(Date,lDate1,lDate2)))
  *:C101760,1 SSH [End]
  SCAN REST WHILE Fabric + Color = m.FABRIC+m.Color FOR Type $ 'PT';
              .AND. IIF(llMWHous,((cFromWare = m.cWareCode) OR ;
                    (cToWare   = m.cWareCode)),.T.)  

    DO CASE
      *--- Physical Inv.
      CASE Type = 'P' AND IIF(llMWHous,(cFromWare = m.cWareCode),.T.)
        m.Added   = IIF((FINVTADJ.OldQty - FINVTADJ.NmTotAdj)>0, (FINVTADJ.OldQty - FINVTADJ.NmTotAdj),0)
        m.Pulled  = IIF((FINVTADJ.OldQty - FINVTADJ.NmTotAdj)<0, (FINVTADJ.OldQty - FINVTADJ.NmTotAdj),0)

        IF IIF(lDate2={} .AND. lDate1 <> {} ,Date>=lDate1,;
          IIF(lDate2={} .AND. lDate1={},.T.,BETWEEN(Date,lDate1,lDate2)))
        
          m.TYPE    = 'P'
          *m.Added   = IIF((FINVTADJ.OldQty - FINVTADJ.NmTotAdj)>0, (FINVTADJ.OldQty - FINVTADJ.NmTotAdj),0)
          *m.Pulled  = IIF((FINVTADJ.OldQty - FINVTADJ.NmTotAdj)<0, (FINVTADJ.OldQty - FINVTADJ.NmTotAdj),0)
          lntotYard = lntotYard + (m.Added-m.Pulled)
          m.ShpDate = Date
          m.Tran    = cTrn_Seq
          m.UNIT    = 0
          m.Style   = ''
          INSERT INTO (lcRpLTmp) FROM MEMVAR
        ELSE
          lnDiff    = lnDiff + (m.Added-m.Pulled)
        ENDIF
      *--- Transfere Inv.
      CASE Type  = 'T'
        m.TYPE   = 'T'
        m.Added  =  0
        m.Pulled =  0
        IF m.cWareCode = FINVTADJ.cFromware
          m.Added  =  0
          m.Pulled =  FINVTADJ.NmTotAdj
        ELSE
          IF m.cWareCode = FINVTADJ.cToware
            m.Added  =  FINVTADJ.NmTotAdj
            m.Pulled =  0
          ENDIF
        ENDIF
        IF IIF(lDate2={} .AND. lDate1 <> {} ,Date>=lDate1,;
          IIF(lDate2={} .AND. lDate1={},.T.,BETWEEN(Date,lDate1,lDate2)))

          lntotYard   = lntotYard +(m.Added-m.Pulled)
          m.ShpDate = Date
          m.Tran    = cTrn_Seq
          m.UNIT    = 0
          m.Style   = ''
          INSERT INTO (lcRpLTmp) FROM MEMVAR
        ELSE
          lnDiff    = lnDiff + (m.Added-m.Pulled)
        ENDIF
    ENDCASE
  ENDSCAN
  *C101894,1 SHA(End)
  
  *--- Start Colecting Trans. From (POSHDR & CtktBom)
  m.TYPE    = ''
  m.Yards   = 0
  m.ShpDate = {}
  m.Tran    = ''
  m.UNIT    = 0
  m.Style   = ''
  m.Pulled  = 0
  SELECT CtktBom
  SET ORDER TO Ctktyp  &&--- cimtyp+cuttkt+item+iclr+mfgcode+dyelot
  llFound = .F.
  IF SEEK('I')
    SCAN REST FOR cimtyp+item+iclr = 'I'+PADR(m.FABRIC,19)+m.Color;
              AND IIF(llMWHous,cwarecode = m.cWareCode,.T.);
              AND CTKTBOM.cCatgTyp $ 'FT'
      m.Added   = 0
      m.Pulled  = 0
      =SEEK('P'+CtktBom.CutTkt,'POSHDR')
      *:C101760,1 SSH [Begin] Add Season to the filter
      *IF !EMPTY(lcRpSeason) AND POSHDR.CSEASON = lcRpSeason
      IF !EMPTY(lcRpSeason) AND POSHDR.CSEASON <> lcRpSeason
        *C101894,1 SHA(Begin)Added to calculate the balance of the transctions that are not 
        *C101894,1           included in the report.
        IF POSHDR.STATUS = 'O'
          m.Added   = 0
          m.Pulled  = m.Pulled + CtktBom.Req_Qty
          lnDiff = lnDiff +(m.Added - m.Pulled)
        ENDIF  
        *C101894,1 SHA(End)
        LOOP
      ENDIF
      *:C101760,1 SSH  [End]
      =SEEK('P'+CtktBom.CutTkt,'POSLN')
      m.TYPE    = 'S'
      m.ShpDate = Date
      m.Tran    = CtktBom.CutTkt
      *B803059,1 [Start] get the style and style qty from BomLine
      SELECT BomLine
      IF SEEK("I1"+CtktBom.CutTkt)
        SELECT BOMLINE
        LOCATE REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode = "I1"+CtktBom.CutTkt FOR item+iclr = PADR(m.FABRIC,19)+m.Color
      
        *m.UNIT    = POSHDR.Open+POSHDR.Receive
        *m.Style   = POSLN.Style
        IF FOUND()
          m.UNIT  = STYQTY
          m.Style = STYLE
        ELSE
          m.UNIT  = 0
          m.Style = ''
        ENDIF
      ENDIF

      SELECT CtktBom
      *B803059,1 [End]
      llFound   = .T.
      IF POSHDR.STATUS = 'O'
        m.Added   = 0
        m.Pulled  = m.Pulled + CtktBom.Req_Qty
        lntotYard = lntotYard +(m.Added - m.Pulled)
      ENDIF  
      IF llFound
        INSERT INTO (lcRpLTmp) FROM MEMVAR
      ENDIF
    ENDSCAN
  ENDIF
*--- Start Colecting Trans. From POFHDR
  SELECT POFLN
  lcOldOrd = ORDER()
  SET ORDER TO
  SELECT DIST cmattype,POMAT FROM POFLN WHERE fabric+color+cmattype+pomat+trancd=;
                                              m.Fabric+m.Color;
                                        AND   cWareCode = m.cWareCode INTO DBF &gcWorkDir.&lcRpPofLn ;
                                        ORDER BY cmattype,POMAT
  SELECT POFLN
  SET ORDER TO &lcOldOrd
  IF _TALLY <> 0
    SELECT (lcRpPofLn)
    GOTO TOP
    SCAN
      *:C101760,1 SSH  [Begin] Add Season
      *IF SEEK(cmattype+POMAT,'POFHDR')
      *:B603624,1 SSH Fix the bug of not print MAt PO's.[Start]
      *IF SEEK(cmattype+POMAT,'POFHDR') .AND. !EMPTY(lcRpSeason) .AND. POFHDR.CSEASON = lcRpSeason

      *C101894,1 SHA(Begin)Changed the way of Collecting Trans. From POFHDR
      *IF SEEK(cmattype+POMAT,'POFHDR') .AND. IIF(!EMPTY(lcRpSeason),POFHDR.CSEASON = lcRpSeason,.T.)
      IF SEEK(cmattype+POMAT,'POFHDR') 
     
      *:B603624,1 SSH Fix the bug of not print MAt PO's. [End]
      *:C101760,1 SSH  [End]
        SELECT POFLN
        =SEEK('P'+PoFHdr.pomat+m.fabric+m.color)
        m.TYPE    = 'M'
        m.ShpDate = {}
        llFound   = .F.
        m.Tran    = ''
        m.UNIT    = 0
        m.Added   = 0
        m.Style   = ''
        DO CASE
          CASE POFHDR.Status = 'O'
            SELECT POFLN
            =SEEK('P'+PoFHdr.pomat+m.fabric+m.color+'1')
            SCAN REST WHILE cmattype+pomat+fabric+color+trancd = ;
                            'P'+PoFHdr.pomat+m.fabric+m.color+'1'
                
              WAIT WINDOW 'Selecting material PO. Trans. For Item /Color : '+m.Fabric +'/'+m.Color NOWAIT
              m.Added   = m.Added + POFLN.nFabTotQty
              m.Pulled  = 0
              IF !EMPTY(lcRpSeason) AND POFHDR.CSEASON <> lcRpSeason
                lnDiff = lnDiff +(m.Added - m.Pulled)
              ELSE
                lntotYard = lntotYard +(m.Added - m.Pulled)
                m.ShpDate = POFHDR.ENTERED
                llFound   = .T.
                m.Tran    = PoFHdr.pomat
              ENDIF  
            ENDSCAN
          CASE POFHDR.Status = 'C'
            SELECT POFLN
            =SEEK('P'+PoFHdr.pomat+m.fabric+m.color)
            SCAN REST WHILE cmattype+pomat+fabric+color+trancd = ;
                            'P'+PoFHdr.pomat+m.fabric+m.color FOR cFabGrade = '1';
                                                              AND Trancd = '2'
              WAIT WINDOW 'Selecting material PO. Trans. For Item /Color : '+m.Fabric +'/'+m.Color NOWAIT
              m.Added   = m.Added + POFLN.nFabTotQty
              m.Pulled  = 0
              IF !EMPTY(lcRpSeason) AND POFHDR.CSEASON <> lcRpSeason
                lnDiff = lnDiff +(m.Added - m.Pulled)
              ELSE
                lntotYard   = lntotYard +(m.Added - m.Pulled)
                m.ShpDate = POFHDR.ENTERED
                llFound = .T.
                m.Tran    = PoFHdr.pomat
              ENDIF
            ENDSCAN
          CASE POFHDR.STATUS = 'X'
            SELECT POFLN
            =SEEK('P'+PoFHdr.pomat+m.fabric+m.color+'1')
            SCAN REST WHILE cmattype+pomat+fabric+color+trancd = ;
                            'P'+PoFHdr.pomat+m.fabric+m.color+'1'
              WAIT WINDOW 'Selecting material PO. Trans. For Item /Color : '+m.Fabric +'/'+m.Color NOWAIT
              m.Added   = 0
              m.Pulled  = 0
              m.ShpDate = POFHDR.ENTERED
              llFound = .T.
              m.Tran    = PoFHdr.pomat
            ENDSCAN
        ENDCASE
        IF llFound
          INSERT INTO (lcRpLTmp) FROM MEMVAR
        ENDIF
      ENDIF
      *C101894,1 SHA(End)
    ENDSCAN
  ENDIF
  SELECT (lcRpTmp)
  REPLACE TYards WITH lntotYard
  *C101894,1 SHA(Begin)Added to update the balance diff.
  REPLACE TDiff WITH lnDiff
  *C101894,1 SHA(End)
ENDSCAN
WAIT CLEAR

*!*************************************************************
*! Name      : lfPrnHdr
*! Developer : AHMED SALAH SHALABY _ (SSH)
*! Date      : 07/30/1999
*! Purpose   : Function to print header.
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfPrnHdr

PAGENO = PAGENO + 1
*B802909,1 SHA(Begin)
*DO RPT_HDR WITH 'MADOR40',XTITLE,R_WIDTH
@ 2,0 SAY "MADOR40"
@ 2,7 SAY PADC(ALLTRIM(gcCom_Name),142)
@ 2,148 SAY DATE()

@ 3,0 SAY TIME()
@ 3,8 SAY PADC(ALLT(XTITLE),141)
@ 3,147 SAY "PAGE# " + STR(PAGENO,4)

*---------10--------20--------30--------40--------50--------60--------70--------80--------90--------100-------110-------120-------130-------140-------150-------160-------170-------180---
*||||5||||0||||5||||0||||5||||0||||5||||0||||5||||0||||5||||0||||5||||0||||5||||0||||5||||0||||5||||0||||5||||0||||5||||0||||5||||0||||5||||0||||5||||0||||5||||0||||5||||0||||5||||0||||5
@ 5,1 SAY "VENDOR   PATTERN    COLOR  WIDTH  CONTENTS                            LOC     YARDS   SHP  DATE TRANS#   UNITS   STYLE                CUSTOMER              "
@ 6,0 SAY REPL('*',157)  &&@ 6,0 SAY REPL('*',187)
*B802909,1 SHA(End)
ROW = 7

*!*************************************************************
*! Name      : lfFabDRec
*! Developer : AHMED SALAH SHALABY _ (SSH)
*! Date      : 07/30/1999
*! Purpose   : Function to colcet data form fabric and fabdye.
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfFabDRec

SELECT FABDYE
SET ORDER TO FABRIC IN FABRIC
SET RELATION TO FABRIC+COLOR INTO FABRIC ADDIT
lcRpPatt = ''
FOR lnInd = 1 TO ALEN(laRpPatTar)
  lcRpPatt = IIF(!EMPTY(laRpPatTar[lnInd]),PADR(laRpPatTar[lnInd],10)+'~~'+lcRpPatt,'')
ENDFOR
*---Pattern
IF !EMPTY(lcRpPatt)
  lcFabFlt = lcFabFlt + ' AND FABRIC.PATTERN $ lcRpPatt'
ENDIF
*---Locatin
IF llMWHous AND !EMPTY(lcLocStr)
  lcFabFlt = lcFabFlt + ' AND FABDYE.cWareCode $ lcLocStr'
ENDIF
*--- Color
IF !EMPTY(lcClrStr)
  lcFabFlt = lcFabFlt + ' AND COLOR $ lcClrStr'
ENDIF
*--- Type
IF !EMPTY(lcTypStr)
  lcFabFlt = lcFabFlt + ' AND FABRIC.Item_Type $ lcTypStr'
ENDIF
*---Vendor
IF !EMPTY(lcRpVen)
  lcFabFlt = lcFabFlt + ' AND FABRIC.Vendor = lcRpVen'
ENDIF
*--- Reason
IF !EMPTY(lcRpSeason)
    *:C101760,1 SSH [Begin] Add Season to the filter
    *lcFabFlt = lcFabFlt + ' AND FABRIC.SEASON = lcRpSeason'
    lcFabFlt = lcFabFlt + ' AND FABRIC.CSEASON = lcRpSeason'
    *:C101760,1 SSH [End] Add Season to the filter
ENDIF
*C101894,1 SHA(Begin)Added a new field to keep the balance diff.
*CREATE TABLE &gcWorkDir.&lcRpTmp (FABRIC C(07) , COLOR C(06) , Vendor C(8),;
                                  Pattern C(10),WIDTH C(6) , Cwarecode C(6),Content C(60),;
                                  TYards N(8))
CREATE TABLE &gcWorkDir.&lcRpTmp (FABRIC C(07) , COLOR C(06) , Vendor C(8),;
                                  Pattern C(10),WIDTH C(6) , Cwarecode C(6),Content C(60),;
                                  TYards N(8), TDiff N(8))
*C101894,1 SHA(End)
IF lcRpSort = 'L'
  INDEX ON cWareCode+VENDOR+FABRIC+COLOR TAG (lcRpTmp)
ELSE
  INDEX ON VENDOR+FABRIC+COLOR+cWareCode TAG (lcRpTmp)
ENDIF
SELECT FABDYE
SET ORDER TO FABDYE
WAIT WINDOW 'Colecting Item press <SPACE BAR> to abort ' NOWAIT

SCAN FOR &lcFabFlt  AND INKEY() <> 32
  SCAT MEMVAR MEMO
  IF !EMPTY(lcRpUPS) AND SEEK(m.cWareCode,'WareHous') ;
                     AND ALLTRIM(WareHous.UPS) <> ALLTRIM(lcRpUPS)
    LOOP
  ENDIF
  WAIT WINDOW 'Selecting Item /Color : '+m.Fabric +'/'+m.Color NOWAIT
  M.VENDOR  = FABRIC.VENDOR
  M.WIDTH   = FABRIC.WIDTH
  M.Pattern = FABRIC.Pattern
  M.Content = FABRIC.Content
  INSERT INTO (lcRpTmp) FROM MEMVAR
ENDSCAN
WAIT CLEAR

*!*************************************************************
*! Name      : lfvClrTyp
*! Developer : AHMED SALAH SHALABY _ (SSH)
*! Date      : 07/30/1999
*! Purpose   : Function To prepare color and item type string.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvClrTyp

FOR lnInd = 1 TO ALEN(laRpTColor)
  lcClrStr = lcClrStr + PADR(laRpTColor[lnInd],6) + ' | '
ENDFOR
lcClrStr = IIF(ALLTRIM(lcClrStr) = '|','',lcClrStr)
FOR lnInd = 1 TO ALEN(laRpTType)
  lcTypStr = lcTypStr + PADR(laRpTType[lnInd],6) + ' | '
ENDFOR
lcTypStr = IIF(ALLTRIM(lcTypStr) = '|','',lcTypStr)
FOR lnInd = 1 TO ALEN(laRpTarget)
  lcLocStr = lcLocStr + PADR(laRpTarget[lnInd],8) + ' | '
ENDFOR
lcLocStr = IIF(ALLTRIM(lcLocStr) = '|','',lcLocStr)

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : AHMED SALAH SHALABY _ (SSH)
*! Date      : 07/30/1999
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfwOldVal

laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value


*!*************************************************************
*! Name      : lfvLoc
*! Developer : AHMED SALAH SHALABY _ (SSH)
*! Date      : 07/30/1999
*! Purpose   : Function to valid location mover.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Example   : = lfvLoc()
*!*************************************************************
FUNCTION lfvLoc

= gfMover(@laRpSource,@laRpTarget,'Material Location',.T.,'')

*!*************************************************************
*! Name      : lfFillLoc
*! Developer : AHMED SALAH SHALABY _ (SSH)
*! Date      : 07/30/1999
*! Purpose   : Vaildate function to fill location.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfFillLoc()
*!*************************************************************
FUNCTION lfFillLoc

DIME laRpSource[1,1]
DIME laRpTarget[1,1]
STORE '' TO laRpSource,laRpTarget
SELECT WareHous
SELECT DISTINCT cWareCode FROM WareHous WHERE lMatInv INTO ARRAY laRpSource

*!*************************************************************
*! Name      : lfvFabric
*! Developer : AHMED SALAH SHALABY _ (SSH)
*! Date      : 07/30/1999
*! Purpose   : Vaildate the fabric code.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvFabric()
*!*************************************************************
FUNCTION lfvFabric
PRIVATE lcObjNam , lcObjVal

lcObjNam = SYS(18)
lcObjVal = EVALUATE(SYS(18))

SELECT FABRIC
lcOldOrd = ORDER()
SET ORDER TO TAG PATTERN IN FABRIC

IF !EMPTY(lcObjVal) .AND. ('?' $ lcObjVal .OR. !SEEK(lcObjVal,'FABRIC'))
  =FABROW (@lcObjVal ,'*')
ENDIF 
&lcObjNam    = IIF(!EMPTY(lcObjVal),FABRIC.PATTERN,'')
SELECT FABRIC
SET ORDER TO &lcOldOrd

*!*************************************************************
*! Name      : lfFillCode
*! Developer : AHMED SALAH SHALABY _ (SSH)
*! Date      : 07/30/1999
*! Purpose   : Vaildate function to fill codes (Color , Type).
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfFillCode()
*!*************************************************************
FUNCTION lfFillCode

*--- Color Array
DIME laRpSColor[1,1]
DIME laRpTColor[1,1]
*--- Item Type Array
DIME laRpSType[1,1]
DIME laRpTType[1,1]
STORE '' TO laRpSType,laRpTType,laRpSColor,laRpTColor
SELECT CODES
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'ITEM_TYPE ' AND crltField = 'N';
                                        INTO ARRAY laRpSType
GOTO TOP
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'COLOR     ' AND crltField = 'N';
                                        INTO ARRAY laRpSColor


*!*************************************************************
*! Name      : lfvColor
*! Developer : AHMED SALAH SHALABY _ (SSH)
*! Date      : 07/30/1999
*! Purpose   : Function to valid color mover.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Example   : = lfvColor()
*!*************************************************************
FUNCTION lfvColor

= gfMover(@laRpSColor,@laRpTColor,'Color Code',.T.,'')

*!*************************************************************
*! Name      : lfvType
*! Developer : AHMED SALAH SHALABY _ (SSH)
*! Date      : 07/30/1999
*! Purpose   : Function to valid type mover.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Example   : = lfvType()
*!*************************************************************
FUNCTION lfvType

= gfMover(@laRpSType,@laRpTType,'Item Type',.T.,'')


*!*************************************************************
*! Name      : lfvVendor
*! Developer : AHMED SALAH SHALABY _ (SSH)
*! Date      : 07/30/1999
*! Purpose   : Vaildate the vendor code.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvVendor()
*!*************************************************************
FUNCTION lfvVendor
PRIVATE lcObjNam , lcObjVal

lcObjNam = SYS(18)
lcObjVal = EVALUATE(SYS(18))

SELECT APVENDOR
lcOldOrder = ORDER()
SET ORDER TO TAG VenCode 
SET FILTER TO 'M' $ cVenSupTyp
IF !EMPTY(lcObjVal) .AND. ;
   ('?' $ lcObjVal .OR. !SEEK(lcObjVal , 'APVENDOR'))
   = gfApVnBrow(@lcObjVal)
   &lcObjNam = lcObjVal
ENDIF
SELECT APVENDOR
SET ORDER TO &lcOldOrder


*!*************************************************************
*! Name      : lfwOGrid
*! Developer : AHMED SALAH SHALABY _ (SSH)
*! Date      : 07/30/1999
*! Purpose   : Option grid when function.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfwOGrid()
*!*************************************************************
FUNCTION lfwOGrid

*:C101760,1 SSH   [begin] Initialize date range
lnDatapos = ASCAN(laOGFxFlt,'FINVTADJ.DATE')
IF lnDatapos > 0
  lnDatapos = ASUBSCRIPT(laOGFxFlt,lnDatapos,1)
  lcStr = ALLTRIM(DTOC(gdSysDate)) + '|' + ALLTRIM(DTOC({}))     
  IF EMPTY(laOGFxFlt[lnDatapos,6])
    laOGFxFlt[lnDatapos,6] = lcStr
  ENDIF
ENDIF
*:C101760,1 SSH   [end]

R_WIDTH  = 'XW'
*:B#802735,1 SSH  28/10/99 Filter on shipper code (Make it as PopUp)
lcRpUPS  = laSortVal[1]
=lfogShowGet('lcRpUPS')
*:B#802735,1 SSH(END)
*!*************************************************************
*! Name      : lfClearRep
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 05/08/1998
*! Purpose   : Function to Clear temp file.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClearRep()
*!*************************************************************
FUNCTION lfClearRep


*--- Global variable to indicate if the selection criteria has been changed or not.
llOgFltCh = .T.
*---Erase the temp file
USE IN IIF(USED(lcRpTmp),lcRpTmp,0)
ERASE &gcWorkDir.&lcRpTmp+'.DBF'
ERASE &gcWorkDir.&lcRpTmp+'.CDX'

USE IN IIF(USED(lcRpLTmp),lcRpLTmp,0)
ERASE &gcWorkDir.&lcRpLTmp+'.DBF'
ERASE &gcWorkDir.&lcRpLTmp+'.CDX'

*!*************************************************************
*! Name      : lfChkFlt
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 05/08/1998
*! Purpose   : Function to CHECK IF FILTER CHANGED.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  TRUE/FAULSE
*!*************************************************************
*! Example            :  =lfChkFlt()
*!*************************************************************
FUNCTION lfChkFlt

lcOldEx = SET('EXACT')
SET EXACT ON

FOR lnIndex = 1 TO ALEN(laRpTarget)
  IF ASCAN(laRpOL,laRpTarget[lnIndex]) = 0
    llChanged = .T.
    EXIT
  ENDIF
ENDFOR

FOR lnIndex = 1 TO ALEN(laRpTColor)
  IF ASCAN(laRpOC,laRpTColor[lnIndex]) = 0
    llChanged = .T.
    EXIT
  ENDIF
ENDFOR

FOR lnIndex = 1 TO ALEN(laRpTType)
  IF ASCAN(laRpOT,laRpTType[lnIndex]) = 0
    llChanged = .T.
    EXIT
  ENDIF
ENDFOR

FOR lnIndex = 1 TO ALEN(laRpPatTar)
  IF ASCAN(laRpOPAT,laRpPatTar[lnIndex]) = 0
    llChanged = .T.
    EXIT
  ENDIF
ENDFOR

SET EXACT &lcOldEx
RETURN llChanged


*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 
*! Purpose   : 
*!           : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvBins()
*!*************************************************************
FUNCTION lfvPatt

= gfMover(@laRpPatSou,@laRpPatTar,'Select Pattern',.T.,'')

*!*************************************************************
*! Name      : lfFillPat
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 
*! Purpose   : 
*!           : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : = lfFillPat()
*!*************************************************************
FUNCTION lfFillPat

DIME laRpPatSou[1,1]
DIME laRpPatTar[1,1]
SELECT FABRIC
SELECT DISTINCT Pattern FROM FABRIC WHERE !EMPTY(Pattern) INTO ARRAY laRpPatSou


*!*************************************************************
*! Name      : lfFill     *:B#802735
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 28/10/99
*! Purpose   : Fill Shipper Code Arrayes
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : = lfFill()
*!*************************************************************
*:B#802735,1 SSH  28/10/99 Filter on shipper code (Make it as PopUp)
FUNCTION lfFill

DIMENSION  laSortDesc[1,1]
SELECT WareHous
SELECT DIST UPS FROM WareHous WHERE !EMPTY(UPS) INTO ARRAY laSortDesc
DIME laSortDesc[ALEN(laSortDesc)+1,1]
=AINS(laSortDesc,1)
laSortDesc[1] = 'ALL'

DIMENSION  laSortVal[ALEN(laSortDesc),1]
=ACOPY(laSortDesc,laSortVal)
laSortVal[1] = ''



*:----------------------------------------------------------------------------
*: Program file        : lfItmPos.
*: Program description : Function to get item positoin in laogfxflt/laogvrflt.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101760  FOR DORBY
*:----------------------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------------------
*: Example             : =lfItmPos()
*:----------------------------------------------------------------------------
*!*:C101760,1
FUNCTION lfItmPos
PARAMETERS lcItmInFlt,lcArrType
PRIVATE lnItmPos
IF lcArrType = 'F'
  lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
  IF lnItmPos > 0
    lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
 ENDIF
ELSE
  lnItmPos = ASCAN(laOgVrFlt,lcItmInFlt)
  IF lnItmPos > 0
    lnItmPos = ASUBSCRIPT(laOgVrFlt,lnItmPos,1)
 ENDIF
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.
