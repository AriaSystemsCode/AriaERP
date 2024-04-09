*:************************************************************************
*: Program file  : SOCAN.Prg
*: Program desc. : Cancellation report.
*: System        : Aria 4 XP
*: Module        : SO, PO, MF
*: Developer     : Wael M. Abo-Shawareb (WSH)
*: Date          : 05/23/2006
*: REF           : 037536,1
*:************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfGrCodes(), lfwOGWhen(), lfNonMaj(), lfsrvSty()
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO SOCAN
*!***************************************************************************
*! Modification:
*! B608519,1 WAM 04/17/2008 check cacel reason code in ORDHDR for canceled orders
*! B608562,1 WAM 05/20/2008 Filter orders to selected ones [T20080514.0001]
*:************************************************************************

lcStyTitle = gfItemMask('HI')             && Style title
llDyelot   = (gfGetMemvar('M_DYELOT', gcAct_Comp) = 'Y')
lcRpForm   = 'SOCAN'
lcTime     = TIME()                       && Variable to hold the Time
lnMajLen   = LEN(gfItemMask("PM"))

IF loOgScroll.llOGFltCh
  =lfGetFilters()
  
  LOCAL lcCond
  lcCond      = lfCheckFilter(1, 'ORDCANLN.CANCELLED')
  llCheckDate = !EMPTY(lcCond)
  IF llCheckDate
    LOCAL lnSpPos
    lnSpPos = ATC('|', lcCond)
    lcDate1 = DTOS(CTOD(IIF(lnSpPos > 0, SUBSTR(lcCond, 1, lnSpPos - 1), ALLTRIM(lcCond))))
    lcDate2 = DTOS(CTOD(IIF(lnSpPos > 0, SUBSTR(lcCond, lnSpPos + 1), "")))
  ENDIF
  lcRpExp = ".T."
  
  DO CASE
    *-- Sales Order Cancellation report
    CASE lcRepTyp = 'O'
      IF !USED(lcStyFil)
        = gfOpenFile(gcDataDir + 'STYLE', gcDataDir + 'STYLE', 'SH', @lcStyFil, .T.)
      ENDIF
      
      *-- Groups of the .FRX
      lcSecGr = 'CORDTYPE+ACCOUNT'
      lcTrdGr = 'CORDTYPE+ACCOUNT+ORDER'
      lcFthGr = 'CORDTYPE+ACCOUNT+ORDER+STORE'
      
      *-- Add date filter
      IF llCheckDate
        lcRpExp = "BETWEEN(DTOS(ORDCANLN.CANCELLED), '" + lcDate1 + "', '" + lcDate2 + "')"
      ENDIF
      
      *-- Add the filter of status
      IF lcStatus <> "L"
        lcRpExp = '(' + lcRpExp + ") .AND. (ORDHDR.Status = '" + lcStatus +  "')"
      ENDIF
      
      *-- Print Order or contract or both as the user choose
      IF lcRpType = "L"
        lcRpExp = '(' + lcRpExp + ") .AND. (ORDHDR.CORDTYPE $ 'OC' )"
      ELSE
        lcRpExp = '(' + lcRpExp + ") .AND. (ORDHDR.CORDTYPE = [" + lcRpType +  "])"
      ENDIF
      IF !EMPTY(lcRpEdiFlt)
        lcRpExp = lcRpExp + lcRpEdiFlt
      ENDIF
      
      *-- Built the temporary file
      =lfBuildST()
      
    *-- Purchase order cancellation report
    CASE lcRepTyp = 'P'
      *-- Groups of the .FRX
      lcSecGr = 'CORDTYPE+VENDOR'
      lcTrdGr = 'CORDTYPE+VENDOR+ORDER'
      lcFthGr = 'CORDTYPE+VENDOR+ORDER+CWARECODE'
      
      *-- Add the filter of status
      IF lcStatus <> "L"
        lcRpExp = "(POSHDR.Status = '" + lcStatus +  "')"
      ENDIF
      
      *-- Add date filter
      IF !EMPTY(lcCond)
        lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp), "", " AND ") + "IIF(POSHDR.Status = 'X', BETWEEN(DTOS(POSHDR.POCANCEL), '" + lcDate1 + "', '" + lcDate2 + "'), .T.)"
      ENDIF
      
      *-- Build the temporary file
      = lfBuildPT()
      
    *-- Cutting ticket cancellation report
    CASE lcRepTyp = 'C'
      *-- Groups of the .FRX
      lcSecGr = 'CORDTYPE'
      lcTrdGr = 'CORDTYPE+ORDER'
      lcFthGr = 'CORDTYPE+ORDER+CWARECODE'
      
      *-- Add the filter of status
      IF lcStatus <> "L"
        lcRpExp = "(POSHDR.Status = '" + lcStatus +  "')"
      ENDIF
      
      *-- Add date filter
      IF !EMPTY(lcCond)
        lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp), "", " AND ") + "IIF(POSHDR.Status = 'X', BETWEEN(DTOS(POSHDR.POCANCEL), '" + lcDate1 + "', '" + lcDate2 + "'), .T.)"
      ENDIF
      
      *-- Built the temporary file
      = lfBuildCT()
  ENDCASE
  
  lcEdTime   = TIME()
  lnInterval = lfCollTime(lcTime, lcEdTime)
  
  WAIT WINDOW "Selected " + ALLTRIM(STR(RECCOUNT(lcMainF))) + " Records in " + ALLTRIM(STR(lnInterval,6,2)) + " Seconds..." NOWAIT
ENDIF

SELECT (lcMainF)
LOCATE

*-- Display the report
DO gfDispRe WITH EVALUATE('lcRpForm')

SELECT STYLE
=gfSetOrder("CSTYLE")

RETURN

*!*************************************************************
*! Name      : lfBuildST
*! Developer : Wael M. abo-Shawareb (WSH)
*! Date      : 05/23/2006
*! Purpose   : To build the temporary file in case of sales order 
*!             cancellation report.
*!*************************************************************
FUNCTION lfBuildST

SELECT STYLE
=gfSetOrder("STYLE")

*-- Set necessary relations to get the required filter
SELECT ORDLINE
SET RELATION TO STYLE INTO STYLE ADDITIVE

SELECT ORDCANLN
SET RELATION TO CORDTYPE+ORDER+STR(LINENO,6) INTO ORDLINE ADDITIVE
SET RELATION TO CORDTYPE+ORDER INTO ORDHDR ADDITIVE
SET RELATION TO IIF(EMPTY(STORE),'M','S')+ACCOUNT+STORE INTO CUSTOMER ADDITIVE
SET RELATION TO STYLE INTO (lcStyFil) ADDITIVE

*-- Create the temporary file from the ORDCANLN table and add some necessary fields to it.
=lfCreateTemp()

PRIVATE lcNewIndex
lcNewIndex = SUBSTR(lcMainF,1,7)+'_1'

=gfOpenFile(gcWorkDir+lcMainF, '', 'EX')

*-- Fill the temporary file
SELECT ORDCANLN
IF !EMPTY(lcOrdFltr) AND USED(lcOrdFltr)
  lcOrdType = IIF(lcRpType = 'L', 'O', lcRpType)
  SELECT (lcOrdFltr)
  SCAN
    SELECT ORDCANLN
    =SEEK(lcOrdType + &lcOrdFltr..Order)
    SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = lcOrdType + &lcOrdFltr..Order ;
              FOR   &lcRpExp .AND. !EOF('ORDHDR')
      *B608519,1 WAM 04/17/2008 check cacel reason code in ORDHDR for canceled orders
      *=lfInsSOLine()
      =lfInsSOLine('L')
      *B608519,1 WAM 04/17/2008 (End)
    ENDSCAN
    
    SELECT ORDCANLN
    IF lcOrdType = 'L' AND SEEK("C" + &lcOrdFltr..Order)
      SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'C'  + &lcOrdFltr..Order ;
                FOR   &lcRpExp .AND. !EOF('ORDHDR')
        *B608519,1 WAM 04/17/2008 check cacel reason code in ORDHDR for canceled orders
        *=lfInsSOLine()
        =lfInsSOLine('L')
        *B608519,1 WAM 04/17/2008 (End)
      ENDSCAN
    ENDIF
  ENDSCAN
ELSE
  SELECT ORDCANLN
  SCAN FOR &lcRpExp .AND. !EOF('ORDHDR')
    *B608519,1 WAM 04/17/2008 check cacel reason code in ORDHDR for canceled orders
    *=lfInsSOLine()
    =lfInsSOLine('L')
    *B608519,1 WAM 04/17/2008 (End)
  ENDSCAN
ENDIF

SELECT ORDCANLN
SET RELATION TO

*-- Get missing lines.
IF lcStatus = "X" OR lcStatus = "L"
  =lfGetMisLn()
ENDIF

SELECT (lcMainF)
SET RELATION TO IIF(EMPTY(STORE),'M','S')+ACCOUNT+STORE INTO CUSTOMER ADDITIVE
SET RELATION TO CORDTYPE+ORDER INTO ORDHDR ADDITIVE
SET RELATION TO STYLE INTO STYLE ADDITIVE

*!************************************************************************************
*! Name      : lfGetMisLn
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/23/2006
*! Purpose   : To get the cancelled order line(s) that hasn't been sent to ORDCANLN
*!************************************************************************************
*! Example   : =lfGetMisLn()
*!************************************************************************************
FUNCTION lfGetMisLn

SET ORDER TO (lcNewIndex) IN (lcMainF)

*--Make a new filter
lcNewFlter = lcRpExp

*-- Clear (CANCELLED,ORDER,CCANCRESON) From filter to apply it on OrdHdr.
lcNewFlter = STRTRAN(lcNewFlter,'ORDCANLN.','ORDHDR.')

*--If the user Status = 'All' scan Canceleld order(s) only.
IF AT('ORDHDR.Status',lcNewFlter) > 0
  lcNewFlter = STRTRAN(lcNewFlter, "ORDHDR.Status = 'L'", "ORDHDR.Status = 'X'")
ELSE
  lcNewFlter = '(' +lcNewFlter+ ") .AND. (ORDHDR.Status = 'X')"
ENDIF

SELECT ORDLINE
SET RELATION TO CORDTYPE+ORDER INTO ORDHDR ADDITIVE
SET RELATION TO IIF(EMPTY(STORE),'M','S')+ACCOUNT+STORE INTO CUSTOMER ADDITIVE

*B608562,1 WAM 05/20/2008 Filter orders to selected ones [T20080514.0001]
IF !EMPTY(lcOrdFltr) AND USED(lcOrdFltr)
  lcOrdType = IIF(lcRpType = 'L', 'O', lcRpType)
  SELECT (lcOrdFltr)
  SCAN
    SELECT ORDLINE
    =SEEK(lcOrdType + &lcOrdFltr..Order)
    SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = lcOrdType + &lcOrdFltr..Order ;
              FOR   &lcNewFlter .AND. !EOF('ORDHDR')
      IF !SEEK(cordtype+order+STR(lineno,6),lcMainF)
        =lfInsSOLine('H')
      ENDIF  
    ENDSCAN
  ENDSCAN
ELSE
*B608562,1 WAM 05/20/2008 (End)

*--Now ORDLINE has relations to : OrdHdr,Customer,Style
SCAN FOR &lcNewFlter AND !EOF('ORDHDR')
  IF !SEEK(cordtype+order+STR(lineno,6),lcMainF)
    *B608519,1 WAM 04/17/2008 check cacel reason code in ORDHDR for canceled orders
    *=lfInsSOLine()
    =lfInsSOLine('H')
    *B608519,1 WAM 04/17/2008 (End)
  ENDIF
ENDSCAN
*B608562,1 WAM 05/20/2008 Filter orders to selected ones [T20080514.0001]
ENDIF
*B608562,1 WAM 05/20/2008 (End)

SET ORDER TO (lcMainF) IN (lcMainF)

*!*************************************************************
*! Name      : lfInsSOLine
*! Developer : Wael M. abo-Shawareb (WSH)
*! Date      : 05/23/2006
*! Purpose   : Insert a new line in the temp file
*!*************************************************************
FUNCTION lfInsSOLine

*B608519,1 WAM 04/17/2008 check cacel reason code in ORDHDR for canceled orders
LPARAMETERS lcFromAlias
*B608519,1 WAM 04/17/2008 (End)

IF !EMPTY(lcSesFltr) AND !SEEK(ORDHDR.SEASON, lcSesFltr)
  RETURN
ENDIF
IF !EMPTY(lcDivFltr) AND !SEEK(ORDHDR.CDIVISION, lcDivFltr)
  RETURN
ENDIF
*B608519,1 WAM 04/17/2008 check cacel reason code in ORDHDR for canceled orders
*IF !EMPTY(lcResFltr) AND !SEEK(ORDCANLN.CCANCRESON, lcResFltr)
IF !EMPTY(lcResFltr) 
  IF lcFromAlias ='L' AND !SEEK(ORDCANLN.CCANCRESON, lcResFltr)
    RETURN
  ENDIF  
  IF lcFromAlias ='H' AND !SEEK(ORDHDR.CCANCRESON, lcResFltr)
    RETURN
  ENDIF  
ENDIF
*B608519,1 WAM 04/17/2008 (End)
IF !EMPTY(lcActFltr) AND !SEEK(CUSTOMER.ACCOUNT, lcActFltr)
  RETURN
ENDIF
IF !EMPTY(lcStyFltr) AND !SEEK(&lcStyFil..cStyMajor, lcStyFltr)
  RETURN
ENDIF
IF !EMPTY(lcGrpFltr) AND !SEEK(&lcStyFil..cStyGroup, lcGrpFltr)
  RETURN
ENDIF
IF lnClrPo <> 0 AND !EMPTY(lcClrFltr) AND !SEEK(SUBSTR(&lcStyFil..Style,lnClrPo,lnColorLen), lcClrFltr)
  RETURN
ENDIF

=gfSeek('S' + &lcStyFil..Scale, "Scale")

SCATTER MEMVAR
m.Name        = CUSTOMER.btName
m.CDIVISION   = ORDHDR.CDIVISION
m.SEASON      = ORDHDR.SEASON
m.Scale       = Scale.Scale
m.Sz1         = Scale.Sz1
m.Sz2         = Scale.Sz2
m.Sz3         = Scale.Sz3
m.Sz4         = Scale.Sz4
m.Sz5         = Scale.Sz5
m.Sz6         = Scale.Sz6
m.Sz7         = Scale.Sz7
m.Sz8         = Scale.Sz8

*B608519,1 WAM 04/17/2008 get cacel reason code and cancel date from ORDHDR for canceled orders
IF lcFromAlias = 'H'
  m.cancelled = ORDHDR.cancelled
  m.cCancReson = ORDHDR.cCancReson
ENDIF
*B608519,1 WAM 04/17/2008 (End)

INSERT INTO (lcMainF) FROM MEMVAR

RETURN

*!*************************************************************
*! Name      : lfBuildCT
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/23/2006
*! Purpose   : To build the temporary file in case of Cut ticket
*!             cancellation report.
*!*************************************************************
FUNCTION lfBuildCT

SELECT STYLE
=gfSetOrder("Style")

*--Create tempFile
=lfCreateTemp()

PRIVATE lcNewIndex
lcNewIndex = SUBSTR(lcMainF,1,7)+'_2'

SELECT (lcMainF)
SET ORDER TO (lcNewIndex)

*-- Fill the temporary file
IF !EMPTY(lcTktFltr) AND USED(lcTktFltr)
  *-- If user select POs from OG ...
  
  SELECT POSLN
  =gfSetOrder("POSLN")
  
  SELECT (lcTktFltr)
  SCAN
    WAIT WINDOW "Cutting Ticket #: " + PO NOWAIT
    
    SELECT POSHDR
    =gfSEEK('PU' + &lcTktFltr..PO)
    
    IF !(&lcRpExp.)
      LOOP
    ENDIF
    
    SELECT POSLN
    =gfSEEK(POSHDR.cBusDocu+POSHDR.cStyType+POSHDR.PO)
    
    SCAN REST WHILE cBusDocu+cStyType+PO = POSHDR.cBusDocu+POSHDR.cStyType+POSHDR.PO ;
              FOR   IIF(POSHDR.Status = 'X', .T., TranCD = '5' AND IIF(llCheckDate, BETWEEN(DTOS(Date), lcDate1, lcDate2), .T.))
      =gfSeek(Style, "Style")
      =lfInsCTLine()
    ENDSCAN
  ENDSCAN
ELSE
  IF !EMPTY(lcStyFltr) AND USED(lcStyFltr)
    *-- If user select Styles from OG ...
    
    SELECT POSLN
    =gfSetOrder("POSLNS")
    
    SELECT (lcStyFltr)
    SCAN
      lcStyCode = SUBSTR(EVALUATE(lcStyFltr + '.cStyMajor'), 1, lnMajLen)
      SELECT POSLN
      =gfSeek('0001' + lcStyCode)
      SCAN REST WHILE CINVTYPE+STYLE+CBUSDOCU+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = '0001' + lcStyCode ;
                FOR   cBusDocu+cStyType = 'PU'
        SELECT POSHDR
        =gfSEEK(POSLN.cBusDocu+POSLN.cStyType+POSLN.PO)
        
        WAIT WINDOW "Cutting Ticket #: " + PO NOWAIT
        
        IF POSHDR.Status <> 'X' AND (POSLN.TranCD <> '5' OR (llCheckDate AND !BETWEEN(DTOS(Date), lcDate1, lcDate2)))
          LOOP
        ENDIF
        
        IF !(&lcRpExp.)
          LOOP
        ENDIF
        
        IF !(&lcRpExp.)
          LOOP
        ENDIF
        
        =gfSeek(POSLN.Style, "Style")
        =lfInsCTLine()
      ENDSCAN
    ENDSCAN
  ELSE
    *-- If user don't select POs or Styles...
    
    SELECT POSLN
    =gfSetOrder("POSLN")
    
    SELECT POSHDR
    llExitLoop = !gfSeek('PU', .F., .T.)
    DO WHILE !llExitLoop
      IF !(&lcRpExp.)
        SELECT POSHDR
        llExitLoop = !gfGoNext() OR cBusDocu+cStyType <> "PU"
        LOOP
      ENDIF
      
      SELECT POSLN
      =gfSeek(POSHDR.cBusDocu+POSHDR.cStyType+POSHDR.PO)
      SCAN REST WHILE cBusDocu+cStyType+PO = POSHDR.cBusDocu+POSHDR.cStyType+POSHDR.PO ;
                FOR   IIF(POSHDR.Status = 'X', .T., TranCD = '5' AND IIF(llCheckDate, BETWEEN(DTOS(Date), lcDate1, lcDate2), .T.))
        
        WAIT WINDOW "Cutting Ticket #: " + PO NOWAIT
        
        =gfSeek(Style, "STYLE")
        =lfInsCTLine()
      ENDSCAN
      
      SELECT POSHDR
      llExitLoop = !gfGoNext() OR cBusDocu+cStyType <> "PU"
    ENDDO
  ENDIF
ENDIF

RETURN

*!*************************************************************
*! Name      : lfInsCTLine
*! Developer : Wael M. abo-Shawareb (WSH)
*! Date      : 05/23/2006
*! Purpose   : Insert a new line in the temp file for Cut Tickets
*!*************************************************************
FUNCTION lfInsCTLine

IF !EMPTY(POSHDR.SEASON) AND !EMPTY(lcSesFltr) AND !SEEK(POSHDR.SEASON, lcSesFltr)
  RETURN
ENDIF
IF !EMPTY(POSHDR.CDIVISION) AND !EMPTY(lcDivFltr) AND !SEEK(POSHDR.CDIVISION, lcDivFltr)
  RETURN
ENDIF
IF !EMPTY(lcStyFltr) AND !SEEK(STYLE.cStyMajor, lcStyFltr)
  RETURN
ENDIF
IF !EMPTY(lcGrpFltr) AND !SEEK(STYLE.cStyGroup, lcGrpFltr)
  RETURN
ENDIF
IF lnClrPo <> 0 AND !EMPTY(lcClrFltr) AND !SEEK(SUBSTR(STYLE.Style,lnClrPo,lnColorLen), lcClrFltr)
  RETURN
ENDIF

=gfSeek('S' + Style.Scale, "Scale")

SELECT POSLN
SCATTER MEMVAR

m.Cancelled   = IIF(POSHDR.Status = 'X', POSHDR.POCancel, POSLN.Date)
m.lMultiWare  = POSHDR.lMultiWare
m.CDIVISION   = POSHDR.CDIVISION
m.SEASON      = POSHDR.SEASON
m.cTktType    = POSHDR.cTktType
m.Status      = POSHDR.Status
m.Order       = m.PO
m.cOrdType    = m.cStyType
m.Scale       = Scale.Scale
m.Sz1         = Scale.Sz1
m.Sz2         = Scale.Sz2
m.Sz3         = Scale.Sz3
m.Sz4         = Scale.Sz4
m.Sz5         = Scale.Sz5
m.Sz6         = Scale.Sz6
m.Sz7         = Scale.Sz7
m.Sz8         = Scale.Sz8
INSERT INTO (lcMainF) FROM MEMVAR

RETURN

*!*************************************************************
*! Name      : lfBuildPT
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/23/2006
*! Purpose   : To build the temporary file in case of Purchase order
*!             cancellation report.
*!*************************************************************
FUNCTION lfBuildPT

SELECT STYLE
=gfSetOrder("Style")

*--Create tempFile
=lfCreateTemp()

PRIVATE lcNewIndex
lcNewIndex = SUBSTR(lcMainF,1,7)+'_2'

SELECT (lcMainF)
SET ORDER TO (lcNewIndex)

*-- Fill the temporary file
IF !EMPTY(lcPOFltr) AND USED(lcPOFltr)
  *-- If user select POs from OG ...
  
  SELECT POSLN
  =gfSetOrder("POSLN")
  
  lcPOType = IIF(lcRpTyp = 'L', 'P', lcRpTyp)
  SELECT (lcPOFltr)
  SCAN
    WAIT WINDOW IIF(lcPOType = 'C', "Contract #: ", "Purchase Order #: ") + PO NOWAIT
    
    SELECT POSHDR
    =gfSEEK(lcPOType + lcPOType + &lcPOFltr..PO)
    
    IF !(&lcRpExp.)
      LOOP
    ENDIF
    
    SELECT POSLN
    =gfSEEK(POSHDR.cBusDocu+POSHDR.cStyType+POSHDR.PO)
    
    SCAN REST WHILE cBusDocu+cStyType+PO = POSHDR.cBusDocu+POSHDR.cStyType+POSHDR.PO ;
              FOR   IIF(POSHDR.Status = 'X', .T., TranCD = '5' AND IIF(llCheckDate, BETWEEN(DTOS(Date), lcDate1, lcDate2), .T.))
      =gfSeek(Style, "Style")
      =lfInsPOLine()
    ENDSCAN
    
    IF lcPOType = 'L'
      SELECT POSHDR
      IF gfSEEK('CC' + &lcPOFltr..PO)
        
        IF !(&lcRpExp.)
          LOOP
        ENDIF
        
        SELECT POSLN
        =gfSEEK(POSHDR.cBusDocu+POSHDR.cStyType+POSHDR.PO)
        
        SCAN REST WHILE cBusDocu+cStyType+PO = POSHDR.cBusDocu+POSHDR.cStyType+POSHDR.PO ;
                  FOR   IIF(POSHDR.Status = 'X', .T., TranCD = '5' AND IIF(llCheckDate, BETWEEN(DTOS(Date), lcDate1, lcDate2), .T.))
          =gfSeek(Style, "Style")
          =lfInsPOLine()
        ENDSCAN
      ENDIF
    ENDIF
  ENDSCAN
ELSE
  IF !EMPTY(lcStyFltr) AND USED(lcStyFltr)
    *-- If user select Styles from OG ...
    
    SELECT POSLN
    =gfSetOrder("POSLNS")
    
    SELECT (lcStyFltr)
    SCAN
      lcStyCode = SUBSTR(EVALUATE(lcStyFltr + '.cStyMajor'), 1, lnMajLen)
      SELECT POSLN
      =gfSeek('0001' + lcStyCode)
      SCAN REST WHILE CINVTYPE+STYLE+CBUSDOCU+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = '0001' + lcStyCode ;
                FOR   cBusDocu $ IIF(lcRpTyp = 'L', "PC", lcRpTyp) AND cStyType $ IIF(lcRpTyp = 'L', "PC", lcRpTyp)
        SELECT POSHDR
        =gfSEEK(POSLN.cBusDocu+POSLN.cStyType+POSLN.PO)
        
        WAIT WINDOW IIF(cStyType = 'C', "Contract #: ", "Purchase Order #: ") + PO NOWAIT
        
        IF POSHDR.Status <> 'X' AND (POSLN.TranCD <> '5' OR (llCheckDate AND !BETWEEN(DTOS(Date), lcDate1, lcDate2)))
          LOOP
        ENDIF
        
        IF !(&lcRpExp.)
          LOOP
        ENDIF
        
        =gfSeek(POSLN.Style, "Style")
        =lfInsPOLine()
      ENDSCAN
    ENDSCAN
  ELSE
    *-- If user don't select POs or Styles...
    
    SELECT POSLN
    =gfSetOrder("POSLN")
    
    SELECT POSHDR
    lcPOType   = IIF(lcRpTyp = 'L', 'P', lcRpTyp)
    llExitLoop = !gfSeek(IIF(lcPOType = 'L', "PP", lcPOType + lcPOType), .F., .T.)
    DO WHILE !llExitLoop
      IF !(&lcRpExp.)
        SELECT POSHDR
        llExitLoop = !gfGoNext() OR cBusDocu+cStyType <> IIF(lcPOType = 'L', "PP", lcPOType + lcPOType)
        LOOP
      ENDIF
      
      SELECT POSLN
      =gfSeek(POSHDR.cBusDocu+POSHDR.cStyType+POSHDR.PO)
      SCAN REST WHILE cBusDocu+cStyType+PO = POSHDR.cBusDocu+POSHDR.cStyType+POSHDR.PO ;
                FOR   IIF(POSHDR.Status = 'X', .T., TranCD = '5' AND IIF(llCheckDate, BETWEEN(DTOS(Date), lcDate1, lcDate2), .T.))
        
        WAIT WINDOW IIF(cStyType = 'C', "Contract #: ", "Purchase Order #: ") + PO NOWAIT
        
        =gfSeek(Style, "STYLE")
        =lfInsPOLine()
      ENDSCAN
      
      SELECT POSHDR
      llExitLoop = !gfGoNext() OR cBusDocu+cStyType <> IIF(lcPOType = 'L', "PP", lcPOType + lcPOType)
    ENDDO
    
    IF lcRpTyp = 'L'
      llExitLoop = !gfSeek("CC", .F., .T.)
      DO WHILE !llExitLoop
        IF !(&lcRpExp.)
          SELECT POSHDR
          llExitLoop = !gfGoNext() OR cBusDocu+cStyType <> "CC"
          LOOP
        ENDIF
        
        SELECT POSLN
        =gfSeek(POSHDR.cBusDocu+POSHDR.cStyType+POSHDR.PO)
        SCAN REST WHILE cBusDocu+cStyType+PO = POSHDR.cBusDocu+POSHDR.cStyType+POSHDR.PO ;
                  FOR   IIF(POSHDR.Status = 'X', .T., TranCD = '5' AND IIF(llCheckDate, BETWEEN(DTOS(Date), lcDate1, lcDate2), .T.))
          
          WAIT WINDOW "Contract #: " + PO NOWAIT
          
          =gfSeek(Style, "STYLE")
          =lfInsPOLine()
        ENDSCAN
        
        SELECT POSHDR
        llExitLoop = !gfGoNext() OR cBusDocu+cStyType <> "CC"
      ENDDO
    ENDIF
  ENDIF
ENDIF

RETURN

*!*************************************************************
*! Name      : lfInsPOLine
*! Developer : Wael M. abo-Shawareb (WSH)
*! Date      : 05/23/2006
*! Purpose   : Insert a new line in the temp file for POs
*!*************************************************************
FUNCTION lfInsPOLine

IF !EMPTY(POSHDR.SEASON) AND !EMPTY(lcSesFltr) AND !SEEK(POSHDR.SEASON, lcSesFltr)
  RETURN
ENDIF
IF !EMPTY(POSHDR.CDIVISION) AND !EMPTY(lcDivFltr) AND !SEEK(POSHDR.CDIVISION, lcDivFltr)
  RETURN
ENDIF
IF !EMPTY(lcVndFltr) AND !SEEK(POSLN.Vendor, lcVndFltr)
  RETURN
ENDIF
IF !EMPTY(lcStyFltr) AND !SEEK(STYLE.cStyMajor, lcStyFltr)
  RETURN
ENDIF
IF !EMPTY(lcGrpFltr) AND !SEEK(STYLE.cStyGroup, lcGrpFltr)
  RETURN
ENDIF
IF lnClrPo <> 0 AND !EMPTY(lcClrFltr) AND !SEEK(SUBSTR(STYLE.Style,lnClrPo,lnColorLen), lcClrFltr)
  RETURN
ENDIF

=gfSeek(POSHDR.Vendor, "APVENDOR")
=gfSeek('S' + Style.Scale, "Scale")

SELECT POSLN
SCATTER MEMVAR

m.Name        = APVendor.cVenComp
m.Cancelled   = IIF(POSHDR.Status = 'X', POSHDR.POCancel, POSLN.Date)
m.lMultiWare  = POSHDR.lMultiWare
m.CDIVISION   = POSHDR.CDIVISION
m.SEASON      = POSHDR.SEASON
m.Status      = POSHDR.Status
m.cOrdType    = m.cStyType
m.Order       = m.PO
m.Scale       = Scale.Scale
m.Sz1         = Scale.Sz1
m.Sz2         = Scale.Sz2
m.Sz3         = Scale.Sz3
m.Sz4         = Scale.Sz4
m.Sz5         = Scale.Sz5
m.Sz6         = Scale.Sz6
m.Sz7         = Scale.Sz7
m.Sz8         = Scale.Sz8
INSERT INTO (lcMainF) FROM MEMVAR

RETURN

*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/23/2006
*! Purpose   : The when function of the option grid
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOGWhen()
*!*************************************************************
FUNCTION lfwOGWhen

DO CASE
  CASE oAriaApplication.ActiveModuleID = 'MF'
    =gfOpenTable('POSHDR', 'POSHDR', 'POSHDR', SET("Datasession"))
    =gfOpenTable('POSLN', 'POSLNS', 'POSLN', SET("Datasession"))
    lcRepTyp = 'C'
  CASE oAriaApplication.ActiveModuleID = 'PO'
    =gfOpenTable('POSHDR', 'POSHDR', 'POSHDR', SET("Datasession"))
    =gfOpenTable('POSLN', 'POSLNS', 'POSLN', SET("Datasession"))
    lcRepTyp = 'P'
  CASE oAriaApplication.ActiveModuleID = 'SO'
    lcRepTyp = 'O'
ENDCASE

*!*************************************************************
*! Name      : lfGrCodes
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/23/2006
*! Purpose   : To get some codes descriptions
*!*************************************************************
FUNCTION lfGrCodes
LPARAMETERS lcCodeType

LOCAL lcRetVal
lcRetVal = ''

DO CASE
  CASE lcCodeType = 'WareHouse'
    lcRetVal = IIF(lcRepTyp = 'O', IIF(ORDHDR.MULTI='Y',STORE,'******'), cWareCode)
    
  CASE lcCodeType = 'Division'
    lcRetVal = gfCodDes(cDivision, 'CDIVISION')
    
  CASE lcCodeType = 'Season'
    lcRetVal = IIF(lcRepTyp = 'P', '', gfCodDes(Season, 'SEASON '))
ENDCASE

*-- Get Status
IF lcCodeType = 'Status'
  DO CASE
    CASE lcRepTyp = 'O'
      DO CASE
        CASE ORDHDR.STATUS = 'O'
          lcRetVal = 'Open'
        CASE ORDHDR.STATUS = 'H'
          lcRetVal = 'Hold'
        CASE ORDHDR.STATUS = 'C'
          lcRetVal = 'Complete'
        CASE ORDHDR.STATUS = 'B'
          lcRetVal = 'Bid'
        CASE ORDHDR.STATUS = 'X'
          lcRetVal = 'Cancel'
      ENDCASE
      
    CASE lcRepTyp = 'P'
      DO CASE
        CASE STATUS = 'O'
          lcRetVal = 'Open'
        CASE STATUS = 'C'
          lcRetVal = 'Complete'
        CASE STATUS = 'X'
          lcRetVal = 'Cancel'
        CASE STATUS = 'S'
          lcRetVal = 'Closed'
        CASE STATUS = 'H'
          lcRetVal = 'Hold'
      ENDCASE
      
    CASE lcRepTyp = 'C'
      DO CASE
        CASE STATUS = 'O'
          lcRetVal = 'Open'
        CASE STATUS = 'H'
          lcRetVal = 'Hold'
        CASE STATUS = 'X'
          lcRetVal = 'Cancel'
        CASE STATUS = 'S'
          lcRetVal = 'Closed'
        CASE STATUS = 'C'
          lcRetVal = 'Complete'
        CASE STATUS = 'A'
          lcRetVal = 'Actual'
      ENDCASE
  ENDCASE
ENDIF

RETURN lcRetVal

*!*************************************************************
*! Name      : lfNonMaj
*! Developer : Ahmed Mohamed Mohamed  (AMM)
*! Date      : 08/27/1998
*! Purpose   : To get the style major segement title
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

llCodeF = .F.
lnClrPo = 0
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.

* Array to collect data about all segments in the style code structure
DIMENSION laMajSeg[1,1]

= gfItemMask(@laMajSeg)

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)

  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)

  IF laMajSeg[lnI,1] = 'F' .AND. !llCodeF
  
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)

    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])

    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
    lnFreeLen = LEN(lcNonMajPi)
  ENDIF

  *-- If you Find Color Type or Find previous Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')

    IF laMajSeg[lnI,1] = 'C'
      
      *-- Color position
      lnClrPo    = laMajSeg[lnI,4]
      
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      *-- Picture
      lcNonMajPi = laMajSeg[lnI,3]
      *-- NonMajor title
      lcColorTt = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      lnColorLen = LEN(lcNonMajPi)
      EXIT
    ELSE
      llCodeF = .T.
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

*-- Compute Free/Color Items in Style Structure. [End]

* get the style major segement title
lcMajTtl =gfItemMask("HM")

lcMajPic = "@! " + gfItemMask("PM")
RETURN lnClrPo

*!*************************************************************
*! Name      : lfStatus
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/27/98
*! Purpose   : To Fill the arrays of status due to the type of report 
*!             (SO, PO or CT)
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfStatus()
*!*************************************************************
FUNCTION lfStatus

DO CASE
  CASE oAriaApplication.ActiveModuleID = 'SO'
    DIMENSION laStatT[6,1]
    DIMENSION laStatV[6,1]
    STORE SPACE(0) TO laStatT,laStatV
    
    laStatT[1] = 'All'
    laStatV[1] = 'L'
    laStatT[2] = 'Open'
    laStatV[2] = 'O'
    laStatT[3] = 'Hold'
    laStatV[3] = 'H'
    laStatT[4] = 'Bid'
    laStatV[4] = 'B'
    laStatT[5] = 'Complete'
    laStatV[5] = 'C'
    laStatT[6] = 'Cancel'
    laStatV[6] = 'X'
    
  CASE oAriaApplication.ActiveModuleID = 'PO'
    DIMENSION laStatT[6,1]
    DIMENSION laStatV[6,1]
    STORE SPACE(0) TO laStatT,laStatV
    laStatT[1] = 'All'
    laStatV[1] = 'L'
    laStatT[2] = 'Open'
    laStatV[2] = 'O'
    laStatT[3] = 'Hold'
    laStatV[3] = 'H'
    laStatT[4] = 'Closed'
    laStatV[4] = 'S'
    laStatT[5] = 'Complete'
    laStatV[5] = 'C'
    laStatT[6] = 'Cancel'
    laStatV[6] = 'X'

  CASE oAriaApplication.ActiveModuleID = 'MF'
    DIMENSION laStatT[7,1]
    DIMENSION laStatV[7,1]
    STORE SPACE(0) TO laStatT,laStatV
    laStatT[1] = 'All'
    laStatV[1] = 'L'
    laStatT[2] = 'Open'
    laStatV[2] = 'O'
    laStatT[3] = 'Hold'
    laStatV[3] = 'H'
    laStatT[4] = 'Closed'
    laStatV[4] = 'S'
    laStatT[5] = 'Complete'
    laStatV[5] = 'C'
    laStatT[6] = 'Actual'
    laStatV[6] = 'A'
    laStatT[7] = 'Cancel'
    laStatV[7] = 'X'   
ENDCASE
*-- End of lfStatus.

*!*************************************************************
*! Name      : lfGetTit
*! Developer : Wael M. abo-Shawareb (WSH)
*! Date      : 05/23/2006
*! Purpose   : Get title to be displayed for the C/T
*!*************************************************************
FUNCTION lfGetTit
PRIVATE lcTit

lcTit = ALLTRIM(gfGetMemvar('M_PRDLNLBL', gcAct_Comp))
lcTit = IIF(RIGHT(lcTit,1) = '#', lcTit, ALLTRIM(lcTit) + ' #')

RETURN lcTit
*-- End of lfGetTit.

*!*************************************************************
*! Name      : lfsrOrder
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/23/2006
*! Purpose   : Rise change order flag, in range browse screen.
*!*************************************************************
FUNCTION lfsrOrder
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'  && Set code
    SELECT ORDHDR
    SET RELATION TO IIF(EMPTY(STORE), "M", "S") + ACCOUNT + STORE INTO CUSTOMER ADDITIVE
    LOCATE
  CASE lcParm = 'R'  && Reset code
    SELECT ORDHDR
    SET RELATION TO
ENDCASE
*-- End of lfsrOrder.

*!*************************************************************
*! Name      : lfsrvTrans
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/27/98
*! Purpose   : To set relation on or off when running the in range function 
*!             in the option grid.
*!*************************************************************
*! Called from : Option grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfsrvTrans()
*!*************************************************************
FUNCTION lfsrvTrans
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    SET ORDER TO VENCODE IN APVENDOR
    SELECT POSHDR
    SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE
  CASE lcParm = 'R'  && Reset code
    SELECT POSHDR
    SET RELATION TO
ENDCASE
*-- End of lfsrvTrans.

*!**************************************************************************
*! Name      : lfvPrint
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/23/2006
*! Purpose   : Valid function of Print (Both,Orders,Contract)
*!**************************************************************************
*! Example   : =lfvPrint()
*!**************************************************************************
FUNCTION lfvPrint

lcRpPrint = IIF(lcRpType = 'L', "'O','C'", "'" + lcRpType + "'")
*-- End of lfvPrint.

*!**************************************************************************
*! Name      : lfvEdiOrd
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/23/2006
*! Purpose   : to validate (Print Orders/Edi Orders) popup in OG 
*!**************************************************************************
*! Example   : =lfvEdiOrd()
*!**************************************************************************
FUNCTION lfvEdiOrd

lcRpEdiFlt = ""
IF lcRpEdiPrn <> "B"
  lcRpEdiFlt = " .AND. (" + IIF(lcRpEdiPrn = "O" , "!OrdHdr.lEdiOrder" ,;
                             "OrdHdr.lEdiOrder") + ")"
ENDIF
llClearOrd = .T.
*-- end of lfvEdiOrd.

*************************************************************
*! Name      : lfGetFilters
*! Developer : Wael M. Abo-Shaweareb (WSH)
*! Date      : 05/23/2006
*! Purpose   : Get Optiongrid Filter Cursors
*!*************************************************************
FUNCTION lfGetFilters

LOCAL lnAlias, lcCurName, llFound, lcCond, lnI
lnAlias = SELECT(0)

*-- Style Filter
lcCurName = lfCheckFilter(1, 'STYLE.CSTYMAJOR')
llFound   = !EMPTY(lcCurName) AND USED(lcCurName) AND RECCOUNT(lcCurName) > 0
IF llFound
  lcStyFltr = lcCurName
  SELECT (lcStyFltr)
  INDEX ON cStyMajor TAG (lcStyFltr)
ELSE
  IF TYPE("lcStyFltr") = "C" AND USED(lcStyFltr)
    USE IN (lcStyFltr)
  ENDIF
  lcStyFltr = ''
ENDIF

*-- Order Filter
lcCurName = lfCheckFilter(1, 'ORDCANLN.ORDER')
llFound   = !EMPTY(lcCurName) AND USED(lcCurName) AND RECCOUNT(lcCurName) > 0
IF llFound
  lcOrdFltr = lcCurName
  SELECT (lcOrdFltr)
  INDEX ON Order TAG (lcOrdFltr)
ELSE
  IF TYPE("lcOrdFltr") = "C" AND USED(lcOrdFltr)
    USE IN (lcOrdFltr)
  ENDIF
  lcOrdFltr = ''
ENDIF

*-- Account Filter
lcCurName = lfCheckFilter(1, 'CUSTOMER.ACCOUNT')
llFound   = !EMPTY(lcCurName) AND USED(lcCurName) AND RECCOUNT(lcCurName) > 0
IF llFound
  lcActFltr = lcCurName
  SELECT (lcActFltr)
  INDEX ON Account TAG (lcActFltr)
ELSE
  IF TYPE("lcActFltr") = "C" AND USED(lcActFltr)
    USE IN (lcActFltr)
  ENDIF
  lcActFltr = ''
ENDIF

*-- PO Filter
lcCurName = lfCheckFilter(1, 'POSHDR.PO')
llFound   = !EMPTY(lcCurName) AND USED(lcCurName) AND RECCOUNT(lcCurName) > 0
IF llFound
  lcPOFltr = lcCurName
  SELECT (lcPOFltr)
  INDEX ON PO TAG (lcPOFltr)
ELSE
  IF TYPE("lcPOFltr") = "C" AND USED(lcPOFltr)
    USE IN (lcPOFltr)
  ENDIF
  lcPOFltr = ''
ENDIF

*-- CutTkt Filter
lcCurName = lfCheckFilter(1, 'CUTTKTH.CUTTKT')
llFound   = !EMPTY(lcCurName) AND USED(lcCurName) AND RECCOUNT(lcCurName) > 0
IF llFound
  lcTktFltr = lcCurName
  SELECT (lcTktFltr)
  INDEX ON PO TAG (lcTktFltr)
ELSE
  IF TYPE("lcTktFltr") = "C" AND USED(lcTktFltr)
    USE IN (lcTktFltr)
  ENDIF
  lcTktFltr = ''
ENDIF

*-- PO Vendor Filter
lcCurName = lfCheckFilter(1, 'POSHDR.VENDOR')
llFound   = !EMPTY(lcCurName) AND USED(lcCurName) AND RECCOUNT(lcCurName) > 0
IF llFound
  lcVndFltr = lcCurName
  SELECT (lcVndFltr)
  INDEX ON cVendCode TAG (lcVndFltr)
ELSE
  IF TYPE("lcVndFltr") = "C" AND USED(lcVndFltr)
    USE IN (lcVndFltr)
  ENDIF
  lcVndFltr = ''
ENDIF

*-- Color Filter
IF lnClrPo <> 0
  lcCond = lfCheckFilter(1, 'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)')
  IF !EMPTY(lcCond)
    lcClrFltr = loOgScroll.gfTempName()
    CREATE CURSOR (lcClrFltr) (Color C(6))
    DIMENSION laValues[1]
    =gfSubStr(lcCond, @laValues, '|')
    SELECT (lcClrFltr)
    INDEX ON Color TAG (lcClrFltr)
    FOR lnI = 1 TO ALEN(laValues,1)
      APPEND BLANK
      REPLACE Color WITH laValues[lnI]
    ENDFOR
  ELSE
    IF TYPE("lcClrFltr") = "C" AND USED(lcClrFltr)
      USE IN (lcClrFltr)
    ENDIF
    lcClrFltr = ''
  ENDIF
ENDIF

*-- Reason Filter
lcCond = lfCheckFilter(3, 'ORDCANLN.CCANCRESON')
IF !EMPTY(lcCond)
  lcResFltr = loOgScroll.gfTempName()
  CREATE CURSOR (lcResFltr) (Reason C(6))
  DIMENSION laValues[1]
  =gfSubStr(lcCond, @laValues, '|')
  SELECT (lcResFltr)
  INDEX ON Reason TAG (lcResFltr)
  FOR lnI = 1 TO ALEN(laValues,1)
    APPEND BLANK
    REPLACE Reason WITH laValues[lnI]
  ENDFOR
ELSE
  IF TYPE("lcResFltr") = "C" AND USED(lcResFltr)
    USE IN (lcResFltr)
  ENDIF
  lcResFltr = ''
ENDIF

*-- Groups Filter
lcCond = lfCheckFilter(3, 'STYLE.CSTYGROUP')
IF !EMPTY(lcCond)
  lcGrpFltr = loOgScroll.gfTempName()
  CREATE CURSOR (lcGrpFltr) (cStyGroup C(6))
  DIMENSION laValues[1]
  =gfSubStr(lcCond, @laValues, '|')
  SELECT (lcGrpFltr)
  INDEX ON cStyGroup TAG (lcGrpFltr)
  FOR lnI = 1 TO ALEN(laValues,1)
    APPEND BLANK
    REPLACE cStyGroup WITH laValues[lnI]
  ENDFOR
ELSE
  IF TYPE("lcGrpFltr") = "C" AND USED(lcGrpFltr)
    USE IN (lcGrpFltr)
  ENDIF
  lcGrpFltr = ''
ENDIF

*-- Season Filter
lcCond = lfCheckFilter(3, 'ORDHDR.SEASON')
IF !EMPTY(lcCond)
  lcSesFltr = loOgScroll.gfTempName()
  CREATE CURSOR (lcSesFltr) (Season  C(6))
  DIMENSION laValues[1]
  =gfSubStr(lcCond, @laValues, '|')
  SELECT (lcSesFltr)
  INDEX ON SEASON TAG (lcSesFltr)
  FOR lnI = 1 TO ALEN(laValues,1)
    APPEND BLANK
    REPLACE SEASON WITH laValues[lnI]
  ENDFOR
ELSE
  IF TYPE("lcSesFltr") = "C" AND USED(lcSesFltr)
    USE IN (lcSesFltr)
  ENDIF
  lcSesFltr = ''
ENDIF

*-- Division Filter
lcCond = lfCheckFilter(3, 'ORDHDR.CDIVISION')
IF !EMPTY(lcCond)
  lcDivFltr = loOgScroll.gfTempName()
  CREATE CURSOR (lcDivFltr) (CDIVISION C(6))
  DIMENSION laValues[1]
  =gfSubStr(lcCond, @laValues, '|')
  SELECT (lcDivFltr)
  INDEX ON CDIVISION TAG (lcDivFltr)
  FOR lnI = 1 TO ALEN(laValues,1)
    APPEND BLANK
    REPLACE CDIVISION WITH laValues[lnI]
  ENDFOR
ELSE
  IF TYPE("lcDivFltr") = "C" AND USED(lcDivFltr)
    USE IN (lcDivFltr)
  ENDIF
  lcDivFltr = ''
ENDIF

SELECT (lnAlias)
RETURN

*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter

LOCAL lcReturn, lnPOS   
DO CASE
  CASE lnArrayType = 1
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  CASE lnArrayType = 3  
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  OTHERWISE
    lcReturn = ""
ENDCASE

RETURN lcReturn

*************************************************************
*! Name      : lfCreateTemp
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Create temp file and add needed fields
*!*************************************************************
FUNCTION lfCreateTemp

*-- Create the temporary file from the ORDCANLN table and add some necessary fields to it.
SELECT ORDCANLN
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+17,18]
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'VENDOR'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 8
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'CWARECODE'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 6
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Scale'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 3
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Sz1'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Sz2'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Sz3'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Sz4'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Sz5'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Sz6'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Sz7'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Sz8'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Name'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 30
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'CDIVISION'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 6
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'SEASON'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 6
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Status'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 1
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'lMultiWare'
laFileStru[lnFileStru,2] = 'L'
laFileStru[lnFileStru,3] = 1
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cTktType'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 6
laFileStru[lnFileStru,4] = 0

FOR lnI = 7 TO 16
  FOR lnJ = 1 TO lnFileStru
    STORE '' TO laFileStru[lnJ,lnI]
  ENDFOR
ENDFOR
FOR lnI = 1 TO lnFileStru
  STORE .T. TO laFileStru[lnI,5], laFileStru[lnI,6]
  STORE 0 TO laFileStru[lnI,17], laFileStru[lnI,18]
ENDFOR

=gfCrtTmp(lcMainF, @laFileStru, "CORDTYPE+ACCOUNT+ORDER+STORE+STYLE", lcMainF)

SELECT (lcMainF)

*---Create new index.
lcNewIndex1 = SUBSTR(lcMainF,1,7)+'_1'
lcNewIndex2 = SUBSTR(lcMainF,1,7)+'_2'
INDEX ON CORDTYPE+ORDER+STR(LINENO,6) TAG (lcNewIndex1)
INDEX ON CORDTYPE+VENDOR+ORDER+CWARECODE+STYLE TAG (lcNewIndex2)
SET ORDER TO (lcMainF)

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/09/1998
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*!
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- end of lfCollTime.
