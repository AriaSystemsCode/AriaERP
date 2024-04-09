*!*************************************************************
*! Name      : lfSavScr
*! Developer : Tarek mohamed Ibrahim - (TMI)
*! Date      : 05/30/2002
*! Purpose   : Save new or modified order
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfSavScr()
*!*************************************************************
*Modifications
*:B606634,1 TMI 01/20/2003 Enable to add order # manually
*:E126684,1 TMI 03/01/2005 Round the book Amt.
*:N125956,1 TMI 03/22/2005 Update the lContract field in lcOrdline temp file in Add mode
*:E126324,1 MHM 03/27/2005 handle printing from material sales order screen
*!*************************************************************

FUNCTION lfSavScr

*-- lnLineCount:- Variable hold number of lines in order line.

PRIVATE lnLineCount,lcBulkYear,lcBulkPrd,lcAlias

STORE ' ' TO lcBulkYear,lcBulkPrd

lnMessage = 0
IF SEEK('M'+laData[2],'Customer') .AND. Customer.Status = 'P' 
  IF laData[5] <> 'B'
    lcMessage = ALLTRIM(laOrdStatus[lnOrdStatus])+' while the customer is Potential. If you keep the order as '+;
                ALLTRIM(laOrdStatus[lnOrdStatus])
    lnMessage = gfModalGen("QRM32085B32008","DIALOG",lcMessage)
    IF lnMessage =1  
      =SEEK('M'+laData[2],'Customer')
      SELECT Customer
      IF gfObj_Lock(.T.)
        REPLACE Customer.Status WITH "A"
        =gfObj_Lock(.F.)
      ELSE
        =gfModalGen("INM32087B00000","DIALOG",'account')
        laData[5] = 'B'
      ENDIF
    ELSE
      laData[5] = 'B'
    ENDIF
  ENDIF  
ENDIF

SELECT(lcOrdLine)
LOCATE FOR nSellConv <=0
IF FOUND()
  *-- Sell Conversion factor cannot be zero
  =gfModalGen('TRM36014B36000','ALERT','Conversion factor')
   llcSave = .F.
   RETURN
ENDIF

SELECT(lcOrdLine)
GOTO TOP
SCAN
  IF !EMPTY(STORE) .AND. SEEK('S'+Account+Store,'Customer') .AND. Customer.Status = 'P'
    IF laData[5] <> 'B'
      lcMessage = ALLTRIM(laOrdStatus[lnOrdStatus])+' while some stores are Potential. If you keep the order as '+;
                  ALLTRIM(laOrdStatus[lnOrdStatus])+', the Potential Stores will be changed to Active. Otherwise, Keep the Potential Stores'
      IF lnMessage = 0
        lnMessage = gfModalGen("QRM32086B32009","DIALOG",lcMessage)
      ENDIF
      IF lnMessage =1
        =SEEK('S'+Account+Store,'Customer')
        SELECT Customer
        IF gfObj_Lock(.T.)
          REPLACE Customer.Status WITH "A"
          =gfObj_Lock(.F.)
        ELSE
          =gfModalGen("INM32087B00000","DIALOG",'store')
          laData[5] = 'B'
        ENDIF
      ELSE
        laData[5] = 'B'
      ENDIF
    ENDIF  
  ENDIF
ENDSCAN 
IF !SEEK(lcOrdType,lcOrdLine) .OR. (laData[41] = 0  .AND. laData[37] = 0)
  *E300420,1 Message : 32044
  *E300420,1 No lines entered! No updates performed.
  *E300420,1 Button : 00000 
  *E300420,1 Ok
  =gfModalGen('TRM32044B00000','DIALOG')
  llcSave = .F.
  SELECT (IIF(lnActFolder=2,lcOrdLine,lcOrdHdr))
  RETURN
ENDIF

*-- don't update in case the exchange rate less than or equal  zero.
IF laData[34] <=0
  *E300420,1 Message : 32028
  *E300420,1 The exchange rate must be greater than zero
  *E300420,1 Button : 00000 
  *E300420,1 Ok
  =gfModalGen('TRM32028B00000','ALERT')
  llcSave = .F.
  SELECT (IIF(lnActFolder=2,lcOrdLine,lcOrdHdr))
  RETURN
ENDIF
IF lcOrdType='O' .AND. laData[5]<>'B'
  IF !CHECKPRD(laData[8],'lcGlYear','lcGlPeriod','',.T.)
    STORE SPACE(4) TO lcGlYear
    STORE SPACE(2) TO lcGlPeriod
  ENDIF
  IF laScrMode[4] .AND. SEEK(lcOrdType+&lcOrdHdr..cFromOrder,'MASOHDR')
    =CHECKPRD(MASOHDR.Entered,'lcBulkYear','lcBulkPrd','',.T.)
  ENDIF
ENDIF
SET ORDER TO TAG 'MASOHDR' IN 'MASOHDR'
SET ORDER TO TAG 'MASOLIN' IN 'MASOLIN'
*- get the oreder number in case of manual or sequence order.
*B606634 TMI [Start] If the SO setup "Generate order # Manually" is 'Yes' for styles 
*                    sales orders then generate it manually also for materials sales orders
*laData[1] = IIF((llContinue AND !EMPTY(laData[1])) OR laScrMode[3],laData[1],;
            gfSequence('CMORDER','','',laData[15]))
laData[1] = IIF((llContinue AND !EMPTY(laData[1])) OR laScrMode[3],laData[1],;
             IIF(laSetups[6,2]='Y',lfGtOrder(),gfSequence('CMORDER','','',laData[15])) )
*B606634 TMI [End  ] 
STORE laData[1] TO lcCurrOrd

=gfSavSess('SOMAT', lcFiles, @laVariables,lcSession)
SELECT UnCmSess
=SEEK('O'+PADR('SOMAT',10)+gcUser_id+lcSession)
REPLACE cCurrObj WITH 'pbSav'
SELECT (lcOrdHdr)
lcEdiOrd = CMORDER
=RLOCK()
REPLACE CMORDER WITH laData[1]
UNLOCK
SHOW GET laData[1] DISABLE
=gfOpenFile(gcDataDir+'MORDCNLN',gcDataDir+'MORDCNLN','SH') 
SELECT MASOLIN
SET RELATION TO
*N000385,4 TMI [Start] If fabric/color is not assigned to a WareHouse then add it
SELECT (lcOrdLine)
SCAN
  IF !EMPTY(m.Fabric) AND !EMPTY(m.Color) .AND. laSetups[5,2]='Y' .AND. !SEEK(m.FABRIC+m.COLOR+laData[31]+SPACE(10),'FABDYE')
    *E300725,1 Message : 38029
    *E300725,1 Item/Color xxxxx/xxxx is not assigned to warehouse xxxx
    *E300725,1 Button : 38001
    *E300725,1 Add Cancel
    DO gpAdFabWar WITH SUBSTR(m.FABRIC,1,7),m.COLOR,SPACE(10),laData[31]
    *N000385,4 TMI [Start] Add link code and sell link code to fabdye from fabric
    lcAlias = ALIAS()
    SELECT FABDYE
    IF !EMPTY(FABRIC.LINK_CODE)
       REPLACE FABDYE.GL_LINK WITH FABRIC.LINK_CODE
    ENDIF
    IF !EMPTY(FABRIC.CSLSGLLINK)
      REPLACE FABDYE.CSLSGLLINK WITH FABRIC.CSLSGLLINK
    ENDIF
    SELECT (lcAlias)
    *N000385,4 TMI [End  ] Add link code and sell link code to fabdye from fabric      
  ENDIF
  REPLACE &lcOrdLine..cwarecode WITH laData[31],;
          &lcOrdLine..Flag      WITH 'M'
ENDSCAN  
*N000385,4 TMI [End  ]  If fabric/color is not assigned to a WareHouse then add it

SELECT (lcOrdLine)
SET DELETE OFF
SET ORDER TO TAG 0
GO TOP
WAIT 'Updating order lines...' WINDOW NOWAIT
lcUntSin = ''
lcExRSin = gfGetExSin(@lcUntSin, laData[33])
lnLineCount=IIF(SEEK(lcOrdType+laData[1],'MASOHDR'),MASOHDR.LastLine,0)
SCAN FOR IIF(MASOHDR.Status='B',.T.,Flag='M' .OR. (Flag='N' .AND. !DELETED() .AND. FABQTY > 0))

  REPLACE CMORDER WITH laData[1]
  
  *-- Update Warehouse Ordered Quantity
  IF lcOrdType='O' .AND. laData[5]<>'B' .AND. nSteps < 2 .AND. ;
     SEEK(Fabric+Color+&lcOrdLine..cWareCode+SPACE(10),'FabDye')
    =SEEK(lcOrdType+CMORDER+STR(LineNo,6),'MASOLIN')
    llFabChgd = (&lcOrdLine..Fabric+&lcOrdLine..Color <> MASOLIN.Fabric+MASOLIN.Color)
    IF llFabChgd
      =lfOrdQUpd('FabDYE')
    ELSE
      *-- For modified order line : Substract order original quantity and add order new quantity
      *-- For deleted lines       : Substract order original quantity
      *-- For orders that was bid : Add order quantity 
      SELECT FabDye                           
      =RLOCK('FabDye')
      REPLACE nSellOrder WITH nSellOrder - IIF(MASOHDR.Status='B',0,MASOLIN.FABQTY*MASOLIN.nSellConv)+;
                                       IIF(!DELETED(lcOrdLine),&lcOrdLine..FABQTY*&lcOrdLine..nSellConv,0)
      UNLOCK IN 'FabDye'
    ENDIF
    SELECT (lcOrdLine)
    =RLOCK()    
    REPLACE nSteps WITH 2
    UNLOCK
  ENDIF

  *-- Update Style Ordered Quantity 
  IF lcOrdType='O' AND laData[5]<>'B' AND nSteps < 4 AND SEEK(Fabric+Color,'Fabric')
    =SEEK(lcOrdType+CMORDER+STR(LineNo,6),'MASOLIN')
    *--If style was changed in the same line, don't update by subtract old
    *--and add new because it is a different style, you should subtract
    *--from old style and add to new style.
    llFabChgd = (&lcOrdLine..Fabric+&lcOrdLine..Color <> MASOLIN.Fabric+MASOLIN.Color)
    IF llFabChgd
      =lfOrdQUpd('Fabric')
    ELSE
      *-- For modified order line : Substract order original quantity and add order new quantity
      *-- For deleted lines       : Substract order original quantity
      *-- For orders that was bid : Add order quantity 
      SELECT Fabric
      =RLOCK('Fabric')
      REPLACE  nSellOrder  WITH nSellOrder - IIF(MASOHDR.Status='B',0,MASOLIN.FABQTY*MASOLIN.nSellConv)+;
                                         IIF(!DELETED(lcOrdLine),&lcOrdLine..FABQTY*&lcOrdLine..nSellConv,0)
      UNLOCK IN 'Fabric'
    ENDIF  
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 4
    UNLOCK
  ENDIF

  =SEEK(lcOrdType+CMORDER+STR(LineNo,6),'MASOLIN')
  IF laScrMode[3] AND &lcOrdLine..nSteps < 8 AND SEEK(lcOrdType+CMORDER+STR(LineNo,6),lcOrdCanLn)
    SELECT (lcOrdCanLn)
    SCATTER TO laCanRec
    INSERT INTO MORDCNLN FROM ARRAY laCanRec
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 8
    UNLOCK
  ENDIF
  SELECT (lcOrdLine)
  SCATTER MEMVAR MEMO
  m.Start    = laData[9]
  m.Complete = laData[10]
  m.Flag     = SPACE(1)
  DO CASE
    CASE !DELETED() .AND. !SEEK(lcOrdType+laData[1]+STR(LineNo,6),'MASOLIN')
      lnLineCount=lnLineCount+1
      m.LineNo = lnLineCount
      INSERT INTO MASOLIN FROM MEMVAR
      IF SEEK(lcOrdType+laData[1],'MASOHDR')
        SELECT MASOHDR
        REPLACE LastLine WITH lnLineCount
      ENDIF
    CASE (!DELETED() .AND. SEEK(lcOrdType+CMORDER+STR(LineNo,6),'MASOLIN'))
      SELECT MASOLIN
      GATHER MEMVAR MEMO
    CASE DELETED() .AND. SEEK(lcOrdType+CMORDER+STR(LineNo,6),'MASOLIN')
      SELECT MASOLIN
      lcOrder  = CMORDER
      lclineNo = STR(LineNo,6)
      lcFabric  = Fabric
      lnTotQty = 0
      lnPrice  = 0
      lnOldPrc = NSELLPRICE
      lnOldQty = FABQTY
      DELETE
  ENDCASE
  
  SELECT (lcOrdLine)
  =RLOCK()
  REPLACE CMORDER  WITH laDAta[1] ,;
          LineNo   WITH m.LineNo
  UNLOCK
ENDSCAN

SELECT MASOLIN
IF SEEK(lcOrdType+laData[1])
  IF laData[7]
    REPLACE REST Start WITH laData[9] , Complete WITH laData[10] ;
                 WHILE cOrdType+CMORDER=lcOrdType+laData[1]
  ELSE
    REPLACE REST Start WITH laData[9] ,;
                 Complete WITH laData[10], ;
                 CustPo WITH laData[4] ;
            WHILE cOrdType+CMORDER=lcOrdType+laData[1]
  ENDIF
ENDIF
=gfCloseFile('MORDCNLN')
SET DELETE ON
WAIT 'Updating order header...' WINDOW NOWAIT
=gfOpenFile(gcDataDir+'arCusHst',gcDataDir+'Acthst','SH')
SELECT MASOHDR
IF !SEEK(lcOrdType+laData[1])
  laData[36] = ROUND(laData[36],2)
  laData[38] = ROUND(laData[38],2)
  laData[40] = ROUND(laData[40],2)
  laData[42] = ROUND(laData[42],2)
  INSERT INTO MASOHDR FROM ARRAY laData
  FLUSH
ENDIF

PRIVATE lnHdrRecNo
lnHdrRecNo = RECNO()
*-- Update orders Quantity & Amount in the Customer History file
IF lcOrdType='O' .AND. laData[5]<>'B' .AND. SEEK(laData[2]+lcGlYear,'arCusHst') .AND. &lcOrdHdr..nSteps < 1
  lnOrdAmt = laData[42] &lcExRSin laData[34] &lcUntSin laData[50] - ;
  IIF(laScrMode[4] OR MASOHDR.Status='B',0,MASOHDR.FabOpenAmt &lcExRSin MASOHDR.nExRate &lcUntSin MASOHDR.nCurrUnit)
  SELECT arCusHst
  =RLOCK()
  REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod -;
            IIF(MASOHDR.Status='B',0,MASOHDR.FabOpen) + laData[41],;
          nOrdQty WITH nOrdQty -;
            IIF(MASOHDR.Status='B',0,MASOHDR.FabOpen) + laData[41],;
          nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + lnOrdAmt ,;
          nOrdAmt WITH nOrdAmt + lnOrdAmt
  UNLOCK
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE nSteps WITH 1
  UNLOCK
ENDIF
=gfCloseFile('arCusHst')
SELECT MASOHDR
=lfFxPointr(lcOrdType,laData[1])
=RLOCK()
laData[5] = IIF(laData[41] = 0  ,IIF(laData[37]>0,'C','X'),laData[5])
laData[36] = ROUND(laData[36],2)
laData[38] = ROUND(laData[38],2)
laData[40] = ROUND(laData[40],2)
laData[42] = ROUND(laData[42],2)
GATHER FROM laData FIELDS &lcScFields
REPLACE cOrdType   WITH lcOrdType ,;
        LastLine   WITH lnLineCount  ,;
        cFromOrder WITH &lcOrdHdr..cFromOrder ,;
        StName     WITH IIF(laData[51],lcShipName,SPACE(30)) ,;
        cAddress1  WITH IIF(laData[51],lcShipAdd1,SPACE(30)) ,;
        cAddress2  WITH IIF(laData[51],lcShipAdd2,SPACE(30)) ,;
        cAddress3  WITH IIF(laData[51],lcShipAdd3,SPACE(30)) ,;
        cAddress4  WITH IIF(laData[51],lcShipAdd4,SPACE(30)) ,;
        cAddress5  WITH IIF(laData[51],lcShipAdd5,SPACE(30))
UNLOCK
*-- If the function was not called from EDI
SELECT unCmSess
=SEEK('O'+PADR('SOMAT',10)+gcUser_id+lcSession)
REPLACE STATUS WITH 'C'
UNLOCK
llContinue = .F.
UNLOCK 

*:E126324,1 MHM 03/27/2005 Update array [Start]
IF laScrMode[4]
  IF ALEN(laNewMpos,1) = 1 AND EMPTY(laNewMpos)
    laNewMpos[1] = lcOrdType+laData[1]
  ELSE
    lnNewOne = ALEN(laNewMpos,1) +1
    DIMENSION  laNewMpos[lnNewOne]
    laNewMpos[lnNewOne] = lcOrdType+laData[1]
  ENDIF
ENDIF  
*:E126324,1 MHM [End]

WAIT CLEAR 
IF laScrMode[4]
  *E300420,1 Message : 32045
  *E300420,1 Order has been saved as xxxxxx
  *E300420,1 Button : 00000 
  *E300420,1 Ok
  =gfModalGen('INM32045B00000','DIALOG',IIF(lcOrdType='C','Contract','Order')+'|'+laData[1])
ENDIF
SET ORDER TO TAG 'MASOLIN' IN (lcOrdLine)
PRIVATE llChanged
llChanged = .F.
IF llChanged
  llWebOrdUp = .T.
ENDIF  

*N125956,1  TMI [Start] Update the lContract field in lcOrdline temp file in Add mode
IF laScrMode[4]
  SELECT &lcOrdLine
  LOCATE
  m.lContract = .F.
  REPLACE LCONTRACT WITH m.lContract ALL
ENDIF
*N125956,1  TMI [End  ] 

SELECT MASOHDR
RETURN

*!*************************************************************
*! Name      : lfDelScr
*! Developer : TMI - Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Cancel/Uncancel order
*!*************************************************************
*! Calls     : gfModalGen,lfGetInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfDelScr()
*!*************************************************************
*!Modifications
*!*************************************************************
FUNCTION lfDelScr
PRIVATE llDisSdMsg
llDisSdMsg = .F.
PRIVATE lnTktCnt
IF lcOrdType='O' .AND. !CHECKPRD(MASOHDR.Entered,'lcGlYear','lcGlPeriod ','',.T.)
  RETURN
ENDIF
lcUntSin = ''
lcExRSin = gfGetExSin(@lcUntSin, laData[33])
*-- Open the order cancellation table if we have shipped qty.
IF MASOHDR.FabSHIP > 0
  =gfOpenFile(gcDataDir+'MORDCNLN',gcDataDir+'MORDCNLN','SH') 
ENDIF
*-- Uncancel order
IF MASOHDR.Status='X'
  IF lcOrdType='O' .AND. !SEEK(lcOrdType+laData[1],'MASOLIN')
    *E300420,1 Message : 32000
    *E300420,1 The lines for this order are missing! Cannot update cut & sold.
    *E300420,1 Button : 00000 
    *E300420,1 Ok
    =gfModalGen('TRM32000B00000','ALERT')
    RETURN
  ENDIF
  SET ORDER TO TAG MASOLIN IN MASOLIN
  SET ORDER TO TAG MASOHDR  IN MASOHDR  
  *E300420,1 Message : 32002
  *E300420,1 Order is canceled. Uncancel?
  *E300420,1 Button : 32000
  *E300420,1 Yes/No
  IF gfModalGen('QRM32002B32000','ALERT','Order') =2
    RETURN
  ENDIF
  IF MASOHDR.FabCancel = 0
    SELECT MASOHDR
    =RLOCK()
    REPLACE Status     WITH 'B' ,;
            cCancReson WITH SPACE(6) ,;
            Cancelled  WITH {} ,;
            Flag       WITH SPACE(1)
    UNLOCK
    SCATTER FIELDS &lcScFields TO laData
    =lfGetInfo()           
    RETURN
  ENDIF  
  SELECT *, 00 AS nSteps FROM MASOHDR ;
   WHERE cordtype+CMORDER=lcOrdType+laData[1];
   INTO DBF (gcWorkDir+lcOrdHdr)

  SELECT *, 00 AS nSteps FROM MASOLIN ;
   WHERE cordtype+CMORDER+STR(lineno,6)=lcOrdType+laData[1];
   INTO DBF (gcWorkDir+lcOrdLine)
  SELECT 'UNCMSESS'
  IF SEEK('I')
    BLANK
  ELSE
    APPEND BLANK
  ENDIF
  REPLACE Status     WITH 'O'       ,;
          cUTranType WITH 'SOMAT'   ,;
          cUserId    WITH gcUser_id ,;
          cSession   WITH lcSession ,;
          cProgram   WITH 'SOMAT'   ,;
          cCurrScr   WITH 'SOMAT'   ,;
          cCurrObj   WITH 'PBDLT'   ,;
          dTranDate  WITH gdSysDate ,;
          cTranTime  WITH TIME()
  =RLOCK()
  lcCurrOrd = laData[1]
  lcFiles = 'lcOrdHdr,'+lcOrdHdr+','+lcOrdHdr+';'+;
            'lcOrdLine,'+lcOrdLine+',lcOrdLine;'
  =gfSavSess('SOMAT', lcFiles, @laVariables,lcSession)
  STORE 0 TO lnNewOpen,lnNewOAmt
  SELECT (lcOrdLine)
  SCAN
    *-- Increase warehouse ordered quantity
    IF lcOrdType='O' .AND. nSteps < 1 .AND. SEEK(Fabric+Color+&lcOrdLine..cWareCode+SPACE(10),'FabDye')
      SELECT FabDye
      =RLOCK()
      REPLACE nSellOrder   WITH nSellOrder + &lcOrdLine..FABQTY*&lcOrdLine..nSellConv
      UNLOCK
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE nSteps WITH 1
      UNLOCK
    ENDIF
    *-- Increase style ordered quantity
    IF lcOrdType='O' .AND. nSteps < 2 .AND. SEEK(Fabric+Color,'Fabric')
      SELECT Fabric
      =RLOCK()
      REPLACE nSellOrder WITH nSellOrder + &lcOrdLine..FABQTY*&lcOrdLine..nSellConv
      UNLOCK
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE nSteps WITH 2
      UNLOCK
    ENDIF
    lnNewOpen = lnNewOpen+ FABQTY
    lnNewOAmt = lnNewOAmt+ FABQTY*NSELLPRICE
  ENDSCAN
  *-- Update order header cancel and open quantity and amount
  IF &lcOrdHdr..nSteps < 2
    SELECT MASOHDR
    =RLOCK()
    REPLACE Status     WITH 'O'      ,;
            cCancReson WITH SPACE(6) ,;
            Cancelled  WITH {} ,;
            FabCancel  WITH FabCancel-lnNewOpen,;
            FabCanAmt  WITH FabCanAmt-lnNewOAmt,;
            FabOpen    WITH lnNewOpen ,;
            FabOpenAmt WITH lnNewOAmt ,;
            Flag       WITH SPACE(1)
    UNLOCK
    IF laData[23]='Y'
      =RLOCK()
      REPLACE FabBOOK    WITH FabShip + FabCancel + lnNewOpen,;
              FabBOOKAMT WITH ShipAmt + FabCanAmt + lnNewOAmt
      UNLOCK    
    ENDIF
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 2
    UNLOCK
  ENDIF
  *-- Update ordered quantity and amount in the customer history file 
  IF lcOrdType='O' .AND. &lcOrdHdr..nSteps < 3 
    =gfOpenFile(gcDataDir+'arCusHst',gcDataDir+'Acthst','SH')
    =SEEK(laData[2]+lcGlYear,'arCusHst')
    lnOrdAmt = lnNewOAmt &lcExRSin laData[34] &lcUntSin laData[50]
    SELECT arCusHst
    =RLOCK()
    REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod + lnNewOpen,;
            nOrdQty            WITH nOrdQty            + lnNewOpen,;
            nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + lnOrdAmt ,;
            nOrdAmt            WITH nOrdAmt            + lnOrdAmt
    UNLOCK
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 3
    UNLOCK
  ENDIF
ELSE
  *-- Cancel order
  *-- Do not cancel shipped complete order
  IF MASOHDR.STATUS='C'
    *E300420,1 Message : 32003
    *E300420,1 Order has been shipped complete! Cannot cancel.
    *E300420,1 Button : 00000 
    *E300420,1 Ok
    =gfModalGen('TRM32003B00000','ALERT')
    REPLACE MASOHDR.LLOK_STAT WITH .F.,;
            MASOHDR.CLOK_USER WITH '',;
            MASOHDR.DLOK_DATE WITH {},;
            MASOHDR.CLOK_TIME WITH ''
    RETURN
  ENDIF
  *-- Do not cancel order with no lines
  IF !(lcOrdType='T' AND MASOHDR.Mon_Flg='L') AND !SEEK(lcOrdType+laData[1],'MASOLIN')
    *E300420,1 Message : 32000
    *E300420,1 The lines for this order are missing! Cannot update cut & sold.
    *E300420,1 Button : 00000 
    *E300420,1 Ok
    =gfModalGen('TRM32000B00000','ALERT')
    RETURN
  ENDIF
  SET ORDER TO TAG MASOLIN IN MASOLIN
  SET ORDER TO TAG MASOHDR  IN MASOHDR  
  *E300420,1 Message : 32005
  *E300420,1 Cancel all open items on this order?
  *E300420,1 Button : 320000
  *E300420,1 Yes/No
  IF gfModalGen('QRM32005B32000','ALERT','order') = 2
    RETURN
  ENDIF
  IF MASOHDR.STATUS='B'
    SELECT MASOHDR
    =RLOCK()
    REPLACE STATUS     WITH 'X' ,;
            cCancReson WITH lfCanReason() ,;
            Cancelled  WITH gdSysDate ,;
            Flag       WITH SPACE(1)
    UNLOCK
    SCATTER FIELDS &lcScFields TO laData
    =lfGetInfo()
    RETURN
  ENDIF
  lnTktCnt = 0
  DIMENSION laTickets[1,2]
  SELECT *, 00 AS nSteps FROM MASOHDR ;
    WHERE cordtype+CMORDER=lcOrdType+laData[1];
    INTO DBF (gcWorkDir+lcOrdHdr)
  m.cCancReson = lfCanReason()
  REPLACE cCancReson WITH m.cCancReson

  SELECT *, 00 AS nSteps FROM MASOLIN ;
    WHERE cordtype+CMORDER+STR(lineno,6)=lcOrdType+laData[1] ;
    INTO DBF (gcWorkDir+lcOrdLine)
  SELECT 'UNCMSESS'
  IF SEEK('I')
    BLANK
  ELSE
    APPEND BLANK
  ENDIF
  REPLACE Status     WITH 'O'       ,;
          cUTranType WITH 'SOMAT'   ,;
          cUserId    WITH gcUser_id ,;
          cSession   WITH lcSession ,;
          cProgram   WITH 'SOMAT'   ,;
          cCurrScr   WITH 'SOMAT'   ,;
          cCurrObj   WITH 'PBDLT'   ,;
          dTranDate  WITH gdSysDate ,;
          cTranTime  WITH TIME()
  =RLOCK()
  lcCurrOrd = laData[1]
  lcFiles = 'lcOrdHdr,'+lcOrdHdr+','+lcOrdHdr+';'+;
            'lcOrdLine,'+lcOrdLine+',lcOrdLine;'
  =gfSavSess('SOMAT', lcFiles, @laVariables,lcSession)
  SELECT (lcOrdLine)
  STORE 0 TO lnOpen,lnOpenAmt,lnBook,lnBookAmt
  SCAN
    *-- Decrease warehouse ordered quantity with order open quantity
    IF lcOrdType='O' .AND. SEEK(&lcOrdLine..Fabric+&lcOrdLine..Color+&lcOrdLine..cWareCode+SPACE(10),'FabDye')
      IF &lcOrdLine..nSteps < 1
        SELECT FabDye
        =RLOCK()
        REPLACE nSellOrder WITH MAX(nSellOrder-&lcOrdLine..FABQTY*&lcOrdLine..nSellConv,0)
        UNLOCK
        SELECT (lcOrdLine)
        =RLOCK()
        REPLACE nSteps WITH 1
        UNLOCK
      ENDIF
    ENDIF
    *-- Decrease style ordered quantity with order open quantity
    IF lcOrdType='O' .AND. SEEK(&lcOrdLine..Fabric+&lcOrdLine..Color,'Fabric')
      IF &lcOrdLine..nSteps < 3
        SELECT Fabric
        =RLOCK()
        REPLACE nSellOrder WITH MAX(nSellOrder-&lcOrdLine..FABQTY*&lcOrdLine..nSellConv,0)
        UNLOCK
        SELECT (lcOrdLine)
        =RLOCK()
        REPLACE nSteps WITH 3
        UNLOCK
      ENDIF
    ENDIF
    =SEEK(lcOrdType+&lcOrdLine..CMORDER+STR(&lcOrdLine..LINENO,6),'MASOLIN')
    UNLOCK
    lnOpen    = lnOpen    + MASOLIN.FABQTY
    lnOpenAmt = lnOpenAmt + MASOLIN.FABQTY*MASOLIN.NSELLPRICE
    lnBook    = lnBook    + MASOLIN.FabBook
    *E126684,1  TMI [Start] round the book amt up for each line
    *lnBookAmt = lnBookAmt + MASOLIN.FabBook*MASOLIN.NSELLPRICE
    lnBookAmt = lnBookAmt + CEILING(MASOLIN.FabBook*MASOLIN.NSELLPRICE*100)/100
    *E126684,1  TMI [End  ] 
    IF MASOHDR.FABSHIP > 0 .AND. MASOLIN.FabQTY > 0
      IF !SEEK(lcOrdType+CMORDER+STR(LineNo,6),'MORDCNLN')
        INSERT INTO ('MORDCNLN') ;
          (cOrdType,CMORDER,LineNo,Cancelled,cCancReson,FabQty,ACCOUNT,Fabric,Color,STORE,DYELOT,NSELLPRICE) ;
          VALUES ;
          (lcOrdType,laData[1],MASOLIN.LineNo,gdSysDate,&lcOrdHdr..cCancReson,MASOLIN.FabQty,;
           MASOLIN.ACCOUNT,MASOLIN.Fabric,MASOLIN.Color,MASOLIN.STORE,MASOLIN.DYELOT,MASOLIN.NSELLPRICE)
      ENDIF
      REPLACE MASOLIN.FabQTY WITH 0
    ENDIF
  ENDSCAN
  *-- llOrdCan ----------> check if MORDCNLN is Open Or Not
  llOrdCan  = !USED('MORDCNLN') .AND. gfOpenFile(gcDataDir+'MORDCNLN',gcDataDir+'MORDCNLN','SH')
  IF SEEK(lcOrdType+laData[1],'MORDCNLN')
    SELECT MORDCNLN
    SCAN REST WHILE cOrdType+CMORDER+STR(LineNo,6) = lcOrdType+laData[1]
      IF SEEK(cOrdType+CMORDER+STR(LineNo,6),'MASOLIN')
        LOOP
      ENDIF
      lnBook    = lnBook    + MORDCNLN.FabQty
      *E126684,1  TMI [Start] Round the book amt up
      *lnBookAmt = lnBookAmt + MORDCNLN.FabQty*MORDCNLN.NSELLPRICE
      lnBookAmt = lnBookAmt + CEILING(MORDCNLN.FabQty*MORDCNLN.NSELLPRICE*100)/100
      *E126684,1  TMI [End  ] 
    ENDSCAN  
  ENDIF  
  IF llOrdCan  
    =gfCloseFile('MORDCNLN')
  ENDIF  
  *-- Decrease orders quantity and amount in the customerr history file
  IF lcOrdType='O' .AND. &lcOrdHdr..nSteps < 2 
    =gfOpenFile(gcDataDir+'arCusHst',gcDataDir+'Acthst','SH')
    =SEEK(laData[2]+lcGlYear,'arCusHst')
    lnOrdAmt = MASOHDR.FabOpenAmt &lcExRSin laData[34] &lcUntSin laData[50]
    SELECT arCusHst
    =RLOCK()
    REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod - MASOHDR.FabOpen ,;
            nOrdQty            WITH nOrdQty            - MASOHDR.FabOpen ,;
            nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod - lnOrdAmt    ,;
            nOrdAmt            WITH nOrdAmt            - lnOrdAmt
    UNLOCK
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 2
    UNLOCK
  ENDIF
  *-- Update order open, Cancel and book quantity and amount and order status
  IF &lcOrdHdr..nSteps < 3
    SELECT MASOHDR
    =RLOCK()
    REPLACE STATUS     WITH IIF(FABSHIP > 0,'C','X') ,;
            cCancReson WITH &lcOrdHdr..cCancReson ,;
            Cancelled  WITH gdSysDate ,;
            FabCancel  WITH IIF(laData[23]='Y',FabCancel ,FabCancel+lnOpen),;
            FabCanAmt  WITH IIF(laData[23]='Y',FabCanAmt ,FabCanAmt+lnOpenAmt),;
            FabOpen    WITH 0,;
            FabOpenAmt WITH 0,;
            FabBook    WITH IIF(laData[23]='Y',FabShip + FabCancel,lnBook)    ,;
            FabBookAmt WITH IIF(laData[23]='Y',FabShipAmt + FabCanAmt,lnBookAmt) ,;
            Flag       WITH SPACE(1)
    UNLOCK
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 3
    UNLOCK
  ENDIF
ENDIF
=gfCloseFile('arCusHst')
*-- If the function was not called from EDI
SELECT unCmSess
=SEEK('O'+PADR('SOMAT',10)+gcUser_id+lcSession)
REPLACE STATUS WITH 'C'
UNLOCK

llContinue = .F.
WAIT CLEAR  
SELECT MASOHDR
SCATTER FIELDS &lcScFields TO laData
=lfGetInfo()
=gfCloseFile('MORDCNLN')
STORE .T. TO laScrMode[1]
SHOW GETS
RETURN

*!*************************************************************
*! Name      : lfGet_Info
*! Developer : TMI - Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Restore invoice information
*!*************************************************************
*! Calls     : gfGetAdr
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfGet_Info()
*!*************************************************************
FUNCTION lfGet_Info
PRIVATE llMove
llMove = .F.
=SEEK('M'+laData[2],'Customer')
llConsAcc  = Customer.CONSOL='Y'
lcPriceLvl = IIF(!EMPTY(Customer.PriceLvl),Customer.PriceLvl,'A')
=SEEK(IIF(EMPTY(laData[3]),'M'+laData[2],'S'+laData[2]+laData[3]),'Customer')
lnShipAddr = IIF(laData[51],2,1)
STORE '' TO  lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,;
             lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5
llMultiSt = (laData[6]='Y')  && cheak if order is multi store.

IF llMultiSt
  STORE 'At Store Level' TO lcBillName
ELSE
  lcBillName = Customer.BtName
  =gfGetAdr('Customer','','','',1,'2')
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcBillAdd&lcCount = lcBillAdd&lcCount + IIF(EMPTY(lcBillAdd&lcCount),'',',')+;
      SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
  ENDFOR
ENDIF
IF lnShipAddr = 2
  lcShipName = IIF(laScrMode[2],MASOHDR.StName,&lcOrdHdr..StName)
  lcShipAdd1 = IIF(laScrMode[2],MASOHDR.cAddress1,&lcOrdHdr..cAddress1)
  lcShipAdd2 = IIF(laScrMode[2],MASOHDR.cAddress2,&lcOrdHdr..cAddress2)
  lcShipAdd3 = IIF(laScrMode[2],MASOHDR.cAddress3,&lcOrdHdr..cAddress3)
  lcShipAdd4 = IIF(laScrMode[2],MASOHDR.cAddress4,&lcOrdHdr..cAddress4)
  lcShipAdd5 = IIF(laScrMode[2],MASOHDR.cAddress5,&lcOrdHdr..cAddress5)
  IF laScrMode[2]
    SHOW GET lcShipName DISABLE
    SHOW GET lcShipAdd1 DISABLE
    SHOW GET lcShipAdd2 DISABLE
    SHOW GET lcShipAdd3 DISABLE
    SHOW GET lcShipAdd4 DISABLE
    SHOW GET lcShipAdd5 DISABLE
  ELSE
    SHOW GET lcShipName ENABLE
    SHOW GET lcShipAdd1 ENABLE
    SHOW GET lcShipAdd2 ENABLE
    SHOW GET lcShipAdd3 ENABLE
    SHOW GET lcShipAdd4 ENABLE
    SHOW GET lcShipAdd5 ENABLE
  ENDIF
ELSE
  IF llMultiSt
    STORE 'At Store Level' TO lcShipName
  ELSE
    IF !EMPTY(Customer.Dist_Ctr)
      llMove = .T.
      lcESeek = Customer.Store
      =SEEK("S"+Customer.Account+Customer.Dist_ctr,"Customer")
    ENDIF
    lcShipName =  IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba)
    =gfGetAdr('CUSTOMER','','','',1,'')
    FOR lnCount = 1 TO ALEN(laAddress,1)
      lcCount = STR(laAddress[lnCount,1],1)
      lcShipAdd&lcCount = lcShipAdd&lcCount + IIF(EMPTY(lcShipAdd&lcCount),'',',')+;
      SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
    ENDFOR
    IF llMove
      llMove = .F.
      =SEEK("S"+Customer.Account+lcESeek,"Customer")
    ENDIF
  ENDIF
  SHOW GET lcShipName DISABLE
  SHOW GET lcShipAdd1 DISABLE
  SHOW GET lcShipAdd2 DISABLE
  SHOW GET lcShipAdd3 DISABLE
  SHOW GET lcShipAdd4 DISABLE
  SHOW GET lcShipAdd5 DISABLE
ENDIF  
lcDelMesag = 'cancel'
DO CASE
  CASE laData[5] = 'B'
    lnOrdStatus = 1
    SHOW GET pbDlt,1 PROMPT lcCancel
  CASE laData[5] = 'O'
    lnOrdStatus = 2
    SHOW GET pbDlt,1 PROMPT lcCancel
  CASE laData[5] = 'H'
    lnOrdStatus = 3
    SHOW GET pbDlt,1 PROMPT lcCancel
  CASE laData[5] = 'X'
    lnOrdStatus = 4
    SHOW GET pbDlt,1 PROMPT lcUnCancel
    lcDelMesag    = 'uncancel'
  CASE laData[5] = 'C'
    lnOrdStatus = 5
    SHOW GET pbDlt,1 PROMPT lcCancel
ENDCASE
llInsur   = (laData[22]='Y')
llBulk    = (laData[23]='Y')
llReorder = (laData[24]='Y')
=gfwCodePop(@laCodes,'CTERMCODE','T')
=gfwCodePop(@laCodes,'SHIPVIA','T')
IF llMultiSt
  =lfChMSHV()
ENDIF
=gfwCodePop(@laCodes,'SPCINST','T')
=gfwCodePop(@laCodes,'SEASON','T')
=gfwCodePop(@laCodes,'CDIVISION','T')
=gfwCodePop(@laCodes,'CORDERCAT','T')
lnWareHouse = ASCAN(laWareHouses,laData[31])
lnWareHouse = IIF(lnWareHouse=0,1,ASUBSCRIPT(laWareHouses,lnWareHouse,1))
SHOW GET lnOrdStatus
SHOW GET lnWareHouse
SHOW GET lnShipAddr
SHOW GET llMultiSt
llReBrowse = .T.


*!*************************************************************
*! Name      : lfOrdQUpd                         
*! Developer : TMI - Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Update Fabric orderd quantity in case of order
*!             line style was changed.
*!*************************************************************
*! Calls     : lfSavScr
*!*************************************************************
*! Parameters: lcFlToUpd-> Alias to update (FABRIC or FABDYE).
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfOrdQUpd
PARA lcFlToUpd

*--Update New Style.
SELECT (lcFlToUpd)
=RLOCK()
REPLACE nSellOrder WITH nSellOrder  + IIF(MASOHDR.Status='B',0,&lcOrdLine..FabQty*&lcOrdLine..NSELLCONV)
UNLOCK

*--Update Old Style.
=SEEK(MASOLIN.Fabric+MASOLIN.Color+IIF(lcFlToUpd='Fabric','',MASOLIN.cWareCode+SPACE(10)),lcFlToUpd)
=RLOCK()
REPLACE nSellOrder WITH nSellOrder - IIF(MASOHDR.Status='B',0,MASOLIN.FabQty*MASOLIN.NSELLCONV)
UNLOCK
RETURN

*******************Function to correct MASOHDR pointer.
FUNCTION lfFxPointr
PARAMETER lcFxTyp,lcFOrd

PRIVATE lnFAls , ll2Return , lcOldOrd
lnFAls = SELECT(0)
SELECT MASOHDR
lcOldOrd = ORDER()
SET ORDER TO
LOCATE FOR cOrdType+CMORDER = lcFxTyp+lcFOrd
IF !FOUND() .OR. MASOHDR.CMORDER <> lcFOrd
  IF BETWEEN(lnHdrRecNo, 1, RECCOUNT())
    GO lnHdrRecNo
    ll2Return = (MASOHDR.CMORDER = lcFOrd)
  ELSE
    WAIT WINDOW "Unable to locate order# "+lcFOrd
    ll2Return = .F.
  ENDIF
ELSE
  ll2Return = (MASOHDR.CMORDER = lcFOrd)
ENDIF
SET ORDER TO &lcOldOrd
SELECT(lnFAls)
RETURN(ll2Return)






