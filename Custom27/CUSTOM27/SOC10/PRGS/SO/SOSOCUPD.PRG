*!**********************************************************************************************
*! Program file  : SoSocUpD.PRG
*! Program desc. : Custom Sales Orders Saving, Deleting, lfGet_Info
*! Screen        : ArVInvGM.SPR
*! System        : Aria Apparel System
*! Module        : Sales Order (SO)
*! Developer     : Sameh Saiid Ezzat 
*! Date          : 02/05/2001
*! Reference     : C102118
*!**********************************************************************************************
*! N.B. This custom Program is a typical copy from Standard Sales Order SoUpdate.Prg
*!      with no modifications in the Standard Code.
*!**********************************************************************************************
*


*!*************************************************************
*! Name      : lfSavScr
*! Developer : WAM
*! Date      : 07/01/1996
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
*
FUNCTION lfSavScr
*-- Add this line to add a new parameter to know if the function was
*-- called from EDI - from EDI transaction 860
*-- (Purchase Order Modify) -. [Begin]
PARAMETERS llFromEDI

*-- llFromEDI  :- Variable hold true or false in case call save from EDI module.
*-- lnCanBulk  :- Variable hold cancel qty from Depleted bulk orders Quantity.
*-- lnCancel   :- Variable hold cancel qty from Depleted bulk orders Quantity.
*-- lnCancelAmt:- Variable hold cancel amount from Depleted bulk orders Quantity.
*-- lnLineCount:- Variable hold number of lines in order line.
*-- lcHdrAlo   :- Variable hold CutTktH Or PosHdr File if we have alocated qty.
*-- lcDetAlo   :- Variable hold CutTktL Or PosLn File if we have alocated qty.

llFromEDI = IIF(TYPE('llFromEDI') = 'L' , llFromEDI , .F.)

PRIVATE lnCanBulk,lnCancel,lnCancelAmt,lnLineCount,lcHdrAlo,lcDetAlo,;
        lcBulkYear,lcBulkPrd

*C102118,1 Before saving we need to Change all JY Group to 2 lines. [Begin]
DO lpAdjLines
*C102118,1 Before saving we need to Change all JY Group to 2 lines. [End]

STORE ' ' TO lcBulkYear,lcBulkPrd

lnMessage = 0
IF SEEK('M'+laData[2],'Customer') .AND. Customer.Status = 'P' 
  IF laData[5] <> 'B'
    lcMessage = ALLTRIM(laOrdStatus[lnOrdStatus])+' while the customer is Potential. If you keep the order as '+;
                ALLTRIM(laOrdStatus[lnOrdStatus])
    lnMessage = gfModalGen("QRM32085B32008","DIALOG",lcMessage)
    *-- If user selects Keep Order, program will switch the customer status to Active. 
    *-- If user selects Keep Customer, program will switch the Order status to BID. 
    
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
GOTO TOP
SCAN
  IF !EMPTY(STORE) .AND. SEEK('S'+Account+Store,'Customer') ;
                   .AND. Customer.Status = 'P'
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
  *-- Message : 32044
  *-- No lines entered! No updates performed.
  *-- Button : 00000 
  *-- Ok
  =gfModalGen('TRM32044B00000','DIALOG')
  llcSave = .F.
  SELECT (IIF(lnActFolder=2,lcOrdLine,lcOrdHdr))
  RETURN
ENDIF

*-- don't update in case the exchange rate less than or equal  zero.
IF laData[34] <=0
  *-- Message : 32028
  *-- The exchange rate must be greater than zero
  *-- Button : 00000 
  *-- Ok
  =gfModalGen('TRM32028B00000','ALERT')
  llcSave = .F.
  SELECT (IIF(lnActFolder=2,lcOrdLine,lcOrdHdr))
  RETURN
ENDIF
IF lcOrdType='O' .AND. laData[5]<>'B' .AND. !CHECKPRD(laData[8],'lcGlYear','lcGlPeriod','',.T.)
  STORE SPACE(4) TO lcGlYear
  STORE SPACE(2) TO lcGlPeriod
ENDIF
IF lcOrdType='O' .AND. laData[5]<>'B' .AND. laScrMode[4] .AND. SEEK(lcOrdType+&lcOrdHdr..cFromOrder,'OrdHdr')
  =CHECKPRD(OrdHdr.Entered,'lcBulkYear','lcBulkPrd','',.T.)
ENDIF
SET ORDER TO TAG 'ORDHDR' IN 'ORDHDR'
SET ORDER TO TAG 'ORDLINE' IN 'ORDLINE'

*- get the oreder number in case of manual or sequence order.
laData[1] = IIF((llContinue AND !EMPTY(laData[1])) OR laScrMode[3],laData[1],;
            IIF(laSetups[6,2]='Y',lfGtOrder(),gfSequence('ORDER','','',laData[15])))
lcCurrOrd = laData[1]

IF llBomVarnt .AND. laScrMode[4] 
  *-- Commit this line cause we must replace lines with orderno only
  *-- for record that have orline with the same orline in lcOrdline in case
  *-- last record in lcOrdLine Without qty (empty line)
  SELECT (lcOrdLine)
  lnRecNo = RECNO()
  GO TOP
  SCAN
    SELECT (lcT_BomVar)
    REPLACE ALL cCost_Id WITH laData[1] ;
    FOR cIdType+cCost_ID+STR(LineNo,6)="SO"+ &lcOrdLine..Order+STR(&lcOrdLine..LineNo,6)
  ENDSCAN
  IF BETWEEN(lnRecNo, 1, RECCOUNT())
    GO lnRecNo
  ENDIF
  *-- Delete any record without order no and pack The temp file 
  SELECT (lcT_BomVar)
  DELETE FOR EMPTY(cCost_Id)
  PACK
ENDIF

*-- Add this line to add the capability to call this function from
*-- the EDI - from EDIProcessPO Class - [Begin]

*-- If the function was not called from EDI
IF !llFromEDI
   
  *C102118,1 We don't use Uncomplete session in Custom Programs. [Begin] 
  *=gfSavSess('SOORD', lcFiles, @laVariables,lcSession)
  *SELECT UnCmSess
  *=SEEK('O'+PADR('SOORD',10)+gcUser_id+lcSession)
  *REPLACE cCurrObj WITH 'pbSav'
  *C102118,1 We don't use Uncomplete session in Custom Programs. [End] 
  
ENDIF    && End of IF !llFromEDI

SELECT (lcOrdHdr)
lcEdiOrd = Order
=RLOCK()
REPLACE Order WITH laData[1]
UNLOCK
SHOW GET laData[1] DISABLE
*-- update the allocated quantity in case we come in edit mode.
IF lcOrdType='O' .AND. laData[5]<>'B' .AND. laScrMode[3] .AND. USED(lcAlocated)
  SET ORDER TO TAG 'CUTPKORD' IN (lcAlocated)
  SELECT (lcAlocated)
  GO TOP
  IF !EOF()
    WAIT 'Updating alocated quantity...' WINDOW NOWAIT
    IF 'PO' $ gcCmpModules  && if we have style purchase order.
      =gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
      =gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLN','SH')
    ENDIF
    IF 'MF' $ gcCmpModules && if we have munf. module.
      =gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')
      =gfOpenFile(gcDataDir+'CUTTKTL',gcDataDir+'CUTLIN','SH')
    ENDIF
    =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTPKORD','SH')
    SELECT (lcAlocated)
    DO WHILE !EOF()
      lcKey = TRANCD+CTKTNO+CTKTLINENO+ORDER+STYLE+CORDLINE
      SCAN REST WHILE TRANCD+CTKTNO+CTKTLINENO+ORDER+STYLE+CORDLINE = lcKey
        lcHdrAlo = IIF(TranCd='1','CutTktH','PosHdr')
        lcDetAlo = IIF(TranCd='1','CutTktL','PosLn')
        =SEEK(lcKey,'CUTPICK')
        SCATTER MEMVAR
        SELECT (lcHdrAlo)
        =SEEK(IIF(m.TranCd='1',m.CTKTNO,'P'+m.CTKTNO))
        =RLOCK()
        IF &lcAlocated..nSteps < 1
          REPLACE TotOrd WITH TotOrd - CUTPICK.TotQty + m.TotQty
          SELECT (lcAlocated)
          =RLOCK()
          REPLACE nSteps WITH 1
          UNLOCK
          SELECT (lcHdrAlo)
        ENDIF
        IF !EMPTY(&lcAlocated..cUpdSizes) .AND. &lcAlocated..nSteps < 2
          IF m.TranCd = '1'
            REPLACE Pcs_Bud WITH Pcs_Bud - CUTPICK.TotQty + m.TotQty ,;
                    Pcs_Opn WITH Pcs_Opn - CUTPICK.TotQty + m.TotQty
          ELSE
            REPLACE nStyOrder WITH nStyOrder - CUTPICK.TotQty + m.TotQty ,;
                    Open      WITH Open  - CUTPICK.TotQty + m.TotQty
          ENDIF
          SELECT (lcAlocated)
          =RLOCK()
          REPLACE nSteps WITH 2
          UNLOCK
          SELECT (lcHdrAlo)
        ENDIF
        UNLOCK
        SELECT (lcDetAlo)
        =SEEK(IIF(m.TranCd='2','P','')+m.CTKTNO+m.Style+m.cTktLineNo+'1')
        =RLOCK()
        IF &lcAlocated..nSteps < 3
          REPLACE ORD1   WITH ORD1   - CUTPICK.Qty1   + m.Qty1 ,;
                  ORD2   WITH ORD2   - CUTPICK.Qty2   + m.Qty2 ,;
                  ORD3   WITH ORD3   - CUTPICK.Qty3   + m.Qty3 ,;
                  ORD4   WITH ORD4   - CUTPICK.Qty4   + m.Qty4 ,;
                  ORD5   WITH ORD5   - CUTPICK.Qty5   + m.Qty5 ,;
                  ORD6   WITH ORD6   - CUTPICK.Qty6   + m.Qty6 ,;
                  ORD7   WITH ORD7   - CUTPICK.Qty7   + m.Qty7 ,;
                  ORD8   WITH ORD8   - CUTPICK.Qty8   + m.Qty8 ,;
                  TOTORD WITH TOTORD - CUTPICK.TotQty + m.TotQty
          SELECT (lcAlocated)
          =RLOCK()
          REPLACE nSteps WITH 3
          UNLOCK
          SELECT (lcDetAlo)
        ENDIF
        IF !EMPTY(&lcAlocated..cUpdSizes) .AND. &lcAlocated..nSteps < 4
          REPLACE Qty1   WITH IIF('1' $ &lcAlocated..cUpdSizes,Qty1-CUTPICK.Qty1+m.Qty1,Qty1) ,;
                  Qty2   WITH IIF('2' $ &lcAlocated..cUpdSizes,Qty2-CUTPICK.Qty2+m.Qty2,Qty2) ,;
                  Qty3   WITH IIF('3' $ &lcAlocated..cUpdSizes,Qty3-CUTPICK.Qty3+m.Qty3,Qty3) ,;
                  Qty4   WITH IIF('4' $ &lcAlocated..cUpdSizes,Qty4-CUTPICK.Qty4+m.Qty4,Qty4) ,;
                  Qty5   WITH IIF('5' $ &lcAlocated..cUpdSizes,Qty5-CUTPICK.Qty5+m.Qty5,Qty5) ,;
                  Qty6   WITH IIF('6' $ &lcAlocated..cUpdSizes,Qty6-CUTPICK.Qty6+m.Qty6,Qty6) ,;
                  Qty7   WITH IIF('7' $ &lcAlocated..cUpdSizes,Qty7-CUTPICK.Qty7+m.Qty7,Qty7) ,;
                  Qty8   WITH IIF('8' $ &lcAlocated..cUpdSizes,Qty8-CUTPICK.Qty8+m.Qty8,Qty8) ,;
                  TotQty WITH TotQty - CUTPICK.TotQty + m.TotQty
          SELECT (lcAlocated)
          =RLOCK()
          REPLACE nSteps WITH 4
          UNLOCK
          SELECT (lcDetAlo)
        ENDIF
        UNLOCK
        SELECT CUTPICK
        =RLOCK()
        IF m.TOTQTY = 0
          DELETE
        ELSE
          =RLOCK()
          GATHER MEMVAR
          UNLOCK
        ENDIF
        UNLOCK
      ENDSCAN
    ENDDO
    =IIF('PO' $ gcCmpModules,gfCloseFile('POSHDR')  AND gfCloseFile('POSLN'),.T.)
    =IIF('MF' $ gcCmpModules,gfCloseFile('CUTTKTH') AND gfCloseFile('CUTTKTL'),.T.)
    =gfCloseFile('CUTPICK')
  ENDIF
ENDIF
=gfOpenFile(gcDataDir+'icStyHst',gcDataDir+'Styhst','SH') 
=IIF('AL' $ gcCmpModules,gfOpenFile(gcDataDir+'PikTkt',gcDataDir+'PikTkt','SH'),.T.)
=gfOpenFile(gcDataDir+'ORDCANLN',gcDataDir+'ORDCANLN','SH') 
IF lcOrdType='O' AND laScrMode[4] AND !EMPTY(&lcOrdHdr..cFromOrder) AND ;
  ('PO' $ gcCmpModules .OR. 'MF' $ gcCmpModules)
  =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
ENDIF

IF lcOrdType='O' .AND. laData[5]<>'B' .AND. laScrMode[3]
  SELECT (lcOrdLine)
  SCAN
    IF Picked .AND. SEEK(PikTkt,'PikTkt')
      SELECT PikTkt
      SET ORDER TO TAG Ordpik
      SCAN FOR Order+Piktkt = EVAL(lcOrdHdr+'.Order')
        =RLOCK() 
        REPLACE CustPo WITH laData[4]
        UNLOCK
      ENDSCAN  
      SET ORDER TO TAG Piktkt
    ENDIF
  ENDSCAN
ENDIF

SELECT ORDLINE
SET RELATION TO
SELECT (lcOrdLine)
SET DELETE OFF
SET ORDER TO TAG 0
GO TOP
WAIT 'Updating order lines...' WINDOW NOWAIT
lcUntSin = ''
lcExRSin = gfGetExSin(@lcUntSin, laData[33])

STORE 0 TO lnCancel,lnCancelAmt,lnCanBulk
lnLineCount=IIF(SEEK(lcOrdType+laData[1],'ORDHDR'),ORDHDR.LastLine,0)
SCAN FOR IIF(OrdHdr.Status='B',.T.,Flag='M' .OR. (Flag='N' .AND. !DELETED() .AND. TotQty > 0))

  *-- Decrease Depleted bulk orders from warehouse Ordered Quantity 
  IF lcOrdType='O' .AND. laData[5] <> 'B' .AND. !DELETED() .AND. ;
     (laScrMode[4] .OR. OrdHdr.Status='B') .AND. nSteps < 1 .AND. ;
    SEEK(lcOrdType+cFromOrder+STR(BulkLineNo,6),'OrdLine') .AND.;
    SEEK(OrdLine.Style+OrdLine.cWareCode+SPACE(10),'StyDye')
    SELECT StyDye
    =RLOCK('StyDye')
    REPLACE Ord1 WITH Ord1 - MIN(OrdLine.Qty1,&lcOrdLine..Qty1),;
            Ord2 WITH Ord2 - MIN(OrdLine.Qty2,&lcOrdLine..Qty2),;
            Ord3 WITH Ord3 - MIN(OrdLine.Qty3,&lcOrdLine..Qty3),;
            Ord4 WITH Ord4 - MIN(OrdLine.Qty4,&lcOrdLine..Qty4),;
            Ord5 WITH Ord5 - MIN(OrdLine.Qty5,&lcOrdLine..Qty5),;
            Ord6 WITH Ord6 - MIN(OrdLine.Qty6,&lcOrdLine..Qty6),;
            Ord7 WITH Ord7 - MIN(OrdLine.Qty7,&lcOrdLine..Qty7),;
            Ord8 WITH Ord8 - MIN(OrdLine.Qty8,&lcOrdLine..Qty8),;
            TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
    UNLOCK IN 'StyDye'
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 1
    UNLOCK
  ENDIF

  *-- Update Warehouse Ordered Quantity 
  IF lcOrdType='O' .AND. laData[5]<>'B' .AND. nSteps < 2 .AND. ;
     SEEK(Style+laData[31]+SPACE(10),'StyDye')
    =SEEK(lcOrdType+Order+STR(LineNo,6),'OrdLine')

    *--If style was changed in the same line, don't update by subtract old
    *--and add new because it is a different style, you should subtract
    *--from old style and add to new style.
    llStyChgd = (&lcOrdLine..Style<>OrdLine.Style)
    IF llStyChgd
      =lfOrdQUpd('STYDYE')    
    ELSE
      
      *-- For modified order line : Substract order original quantity and add order new quantity
      *-- For deleted lines       : Substract order original quantity
      *-- For orders that was bid : Add order quantity 
      SELECT StyDye                           
      =RLOCK('StyDye')
      REPLACE Ord1   WITH Ord1   - IIF(OrdHdr.Status='B',0,OrdLine.Qty1)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty1,0),;
              Ord2   WITH Ord2   - IIF(OrdHdr.Status='B',0,OrdLine.Qty2)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty2,0),;
              Ord3   WITH Ord3   - IIF(OrdHdr.Status='B',0,OrdLine.Qty3)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty3,0),;
              Ord4   WITH Ord4   - IIF(OrdHdr.Status='B',0,OrdLine.Qty4)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty4,0),;
              Ord5   WITH Ord5   - IIF(OrdHdr.Status='B',0,OrdLine.Qty5)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty5,0),;
              Ord6   WITH Ord6   - IIF(OrdHdr.Status='B',0,OrdLine.Qty6)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty6,0),;
              Ord7   WITH Ord7   - IIF(OrdHdr.Status='B',0,OrdLine.Qty7)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty7,0),;
              Ord8   WITH Ord8   - IIF(OrdHdr.Status='B',0,OrdLine.Qty8)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty8,0),;
              TotOrd WITH TotOrd - IIF(OrdHdr.Status='B',0,OrdLine.TotQty)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..TotQty,0)
      UNLOCK IN 'StyDye'
    ENDIF
    SELECT (lcOrdLine)
    =RLOCK()    
    REPLACE nSteps WITH 2
    UNLOCK
  ENDIF

  *-- Decrease Depleted bulk orders from Style Ordered Quantity 
  IF lcOrdType='O' .AND. laData[5]<>'B' .AND. !DELETED() .AND. ;
     (laScrMode[4] .OR. OrdHdr.Status='B') .AND. nSteps < 3 .AND.;
     SEEK(lcOrdType+cFromOrder+STR(BulkLineNo,6),'ORDLINE') .AND. ;
     SEEK(OrdLine.Style,'Style')
    SELECT Style 
    =RLOCK('Style')
    REPLACE Ord1   WITH Ord1 - MIN(OrdLine.Qty1,&lcOrdLine..Qty1),;
            Ord2   WITH Ord2 - MIN(OrdLine.Qty2,&lcOrdLine..Qty2),;
            Ord3   WITH Ord3 - MIN(OrdLine.Qty3,&lcOrdLine..Qty3),;
            Ord4   WITH Ord4 - MIN(OrdLine.Qty4,&lcOrdLine..Qty4),;
            Ord5   WITH Ord5 - MIN(OrdLine.Qty5,&lcOrdLine..Qty5),;
            Ord6   WITH Ord6 - MIN(OrdLine.Qty6,&lcOrdLine..Qty6),;
            Ord7   WITH Ord7 - MIN(OrdLine.Qty7,&lcOrdLine..Qty7),;
            Ord8   WITH Ord8 - MIN(OrdLine.Qty8,&lcOrdLine..Qty8),;
            TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
    UNLOCK IN 'Style'
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 3
    UNLOCK
  ENDIF

  *-- Update Style Ordered Quantity 
  IF lcOrdType='O' AND laData[5]<>'B' AND nSteps < 4 AND SEEK(Style,'Style')
    =SEEK(lcOrdType+Order+STR(LineNo,6),'OrdLine')

    *--If style was changed in the same line, don't update by subtract old
    *--and add new because it is a different style, you should subtract
    *--from old style and add to new style.
    llStyChgd = (&lcOrdLine..Style<>OrdLine.Style)
    IF llStyChgd
      =lfOrdQUpd('STYLE')
    ELSE

      *-- For modified order line : Substract order original quantity and add order new quantity
      *-- For deleted lines       : Substract order original quantity
      *-- For orders that was bid : Add order quantity 
      SELECT Style
      =RLOCK('Style')
      REPLACE  Ord1   WITH Ord1   - IIF(OrdHdr.Status='B',0,OrdLine.Qty1)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty1,0),;
               Ord2   WITH Ord2   - IIF(OrdHdr.Status='B',0,OrdLine.Qty2)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty2,0),;
               Ord3   WITH Ord3   - IIF(OrdHdr.Status='B',0,OrdLine.Qty3)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty3,0),;
               Ord4   WITH Ord4   - IIF(OrdHdr.Status='B',0,OrdLine.Qty4)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty4,0),;
               Ord5   WITH Ord5   - IIF(OrdHdr.Status='B',0,OrdLine.Qty5)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty5,0),;
               Ord6   WITH Ord6   - IIF(OrdHdr.Status='B',0,OrdLine.Qty6)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty6,0),;
               Ord7   WITH Ord7   - IIF(OrdHdr.Status='B',0,OrdLine.Qty7)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty7,0),;
               Ord8   WITH Ord8   - IIF(OrdHdr.Status='B',0,OrdLine.Qty8)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty8,0),;
               TotOrd WITH TotOrd - IIF(OrdHdr.Status='B',0,OrdLine.TotQty)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..TotQty,0)
      UNLOCK IN 'Style'
    ENDIF
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 4
    UNLOCK
  ENDIF

  *-- Cancel Depleted bulk orders Quantity 
  IF lcOrdType='O' .AND. laData[5] <> 'B' .AND. ;
     (laScrMode[4] .OR. OrdHdr.Status='B') .AND. !DELETED() .AND. ;
     SEEK(lcOrdType+cFromOrder+STR(BulkLineNo,6),'OrdLine')
    lnCanBulk = MIN(OrdLine.Qty1,Qty1)+MIN(OrdLine.Qty2,Qty2)+;
                MIN(OrdLine.Qty3,Qty3)+MIN(OrdLine.Qty4,Qty4)+;
                MIN(OrdLine.Qty5,Qty5)+MIN(OrdLine.Qty6,Qty6)+;
                MIN(OrdLine.Qty7,Qty7)+MIN(OrdLine.Qty8,Qty8)
    lnCancel    = lnCancel    + lnCanBulk
    lnCancelAmt = lnCancelAmt + lnCanBulk*OrdLine.Price
    lnOpmAmnt = lnCanBulk*OrdLine.Price &lcExRSin laData[34] &lcUntSin laData[50]

    *-- Decrease Depleted bulk orders Quantity & Amount in the Style History file
    IF SEEK(Style+lcBulkYear,'icStyHst') .AND. &lcOrdLine..nSteps < 5
      lnOrdAmt = lnOpmAmnt &lcExRSin laData[34] &lcUntSin laData[50]
      SELECT icStyHst
      =RLOCK()
      REPLACE nOrdQty&lcBulkPrd WITH nOrdQty&lcBulkPrd - lnCanBulk ,;
              nOrdQty           WITH nOrdQty           - lnCanBulk ,;
              nOrdAmt&lcBulkPrd WITH nOrdAmt&lcBulkPrd - lnOrdAmt  ,;
              nOrdAmt           WITH nOrdAmt           - lnOrdAmt
      UNLOCK
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE nSteps WITH 5
      UNLOCK
    ENDIF
    
    *-- Update Bulk order line open and book quantity
    IF nSteps < 6
      =RLOCK('ORDLINE')
      SELECT ORDLINE
      =RLOCK()
      REPLACE Qty1   WITH MAX(Qty1-&lcOrdLine..Qty1,0),;
              Qty2   WITH MAX(Qty2-&lcOrdLine..Qty2,0),;
              Qty3   WITH MAX(Qty3-&lcOrdLine..Qty3,0),;
              Qty4   WITH MAX(Qty4-&lcOrdLine..Qty4,0),;
              Qty5   WITH MAX(Qty5-&lcOrdLine..Qty5,0),;
              Qty6   WITH MAX(Qty6-&lcOrdLine..Qty6,0),;
              Qty7   WITH MAX(Qty7-&lcOrdLine..Qty7,0),;
              Qty8   WITH MAX(Qty8-&lcOrdLine..Qty8,0),;
              Totqty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
      REPLACE Book1   WITH MAX(Book1-&lcOrdLine..Book1,0),;
              Book2   WITH MAX(Book2-&lcOrdLine..Book2,0),;
              Book3   WITH MAX(Book3-&lcOrdLine..Book3,0),;
              Book4   WITH MAX(Book4-&lcOrdLine..Book4,0),;
              Book5   WITH MAX(Book5-&lcOrdLine..Book5,0),;
              Book6   WITH MAX(Book6-&lcOrdLine..Book6,0),;
              Book7   WITH MAX(Book7-&lcOrdLine..Book7,0),;
              Book8   WITH MAX(Book8-&lcOrdLine..Book8,0),;
              TotBook WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8
      UNLOCK IN 'ORDLINE'
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE nSteps WITH 6
      UNLOCK
    ENDIF
  ENDIF
  IF lcOrdType='O' .AND. laData[5]<>'B' .AND. laScrMode[3] .AND. ;
     Picked .AND. SEEK(PikTkt,'PikTkt')
    SELECT PikTkt
    =RLOCK() 
    REPLACE Store  WITH &lcOrdLine..Store ,;
            CustPo WITH &lcOrdLine..CustPo
    UNLOCK
    SELECT (lcOrdLine)
  ENDIF
  =SEEK(lcOrdType+Order+STR(LineNo,6),'OrdLine')

  *-- Update orders Quantity & Amount in the Style History file
  IF lcOrdType='O' .AND. laData[5]<>'B' .AND. SEEK(Style+lcGlYear,'icStyHst') .AND. ;
     &lcOrdLine..nSteps < 7
    lnOrdAmt = ;
    IIF(DELETED(lcOrdLine),0,&lcOrdLine..TotQty*&lcOrdLine..Price &lcExRSin laData[34] &lcUntSin laData[50]) - ;
    IIF(laScrMode[4] OR OrdHdr.Status='B',0,OrdLine.TotQty*OrdLine.Price &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit)
    SELECT icStyHst
    =RLOCK()
    REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod - ;
            IIF(OrdHdr.Status='B',0,OrdLine.TotQty) + ;
            IIF(DELETED(lcOrdLine),0,&lcOrdLine..TotQty) ,;
            nOrdQty WITH nOrdQty- IIF(OrdHdr.Status='B',0,OrdLine.TotQty) + ;
            IIF(DELETED(lcOrdLine),0,&lcOrdLine..TotQty) ,;
            nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + lnOrdAmt ,;
            nOrdAmt WITH nOrdAmt + lnOrdAmt
    UNLOCK
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 7
    UNLOCK
  ENDIF
  IF laScrMode[3] AND &lcOrdLine..nSteps < 8 AND SEEK(lcOrdType+Order+STR(LineNo,6),lcOrdCanLn)
    SELECT (lcOrdCanLn)
    SCATTER TO laCanRec
    INSERT INTO ORDCANLN FROM ARRAY laCanRec
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 8
    UNLOCK
  ENDIF

  *-- Check if any C/Ts have been assigned to the bulk order.
  *-- If any, Release allocation from the bulk order and allocate them
  *-- to the newly received distribution order.
  SELECT (lcOrdLine)
  IF lcOrdType='O' AND laScrMode[4] AND 'MF' $ gcCmpModules AND ;
     !EMPTY(&lcOrdHdr..cFromOrder) .AND. &lcOrdLine..nSteps < 9
    SCATTER FIELDS ;
            Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laDistQty
    SELECT CUTPICK
    =SEEK('1'+&lcOrdHdr..cFromOrder+STR(&lcOrdLine..BulkLineNo,6))
    SCAN REST ;
         WHILE TRANCD+ORDER+CORDLINE='1'+&lcOrdHdr..cFromOrder+STR(&lcOrdLine..BulkLineNo,6) ;
         AND laDistQty[1]+laDistQty[2]+laDistQty[3]+laDistQty[4]+;
             laDistQty[5]+laDistQty[6]+laDistQty[7]+laDistQty[8] >0
      m.CtktLineNo = CtktLineNo
      m.CtktNo = CtktNo
      m.Cut1 = Qty1
      m.Cut2 = Qty2
      m.Cut3 = Qty3
      m.Cut4 = Qty4
      m.Cut5 = Qty5
      m.Cut6 = Qty6
      m.Cut7 = Qty7
      m.Cut8 = Qty8
      =RLOCK()            
      REPLACE Qty1   WITH MAX(Qty1-laDistQty[1],0) ,;
              Qty2   WITH MAX(Qty2-laDistQty[2],0) ,;
              Qty3   WITH MAX(Qty3-laDistQty[3],0) ,;
              Qty4   WITH MAX(Qty4-laDistQty[4],0) ,;
              Qty5   WITH MAX(Qty5-laDistQty[5],0) ,;
              Qty6   WITH MAX(Qty6-laDistQty[6],0) ,;
              Qty7   WITH MAX(Qty7-laDistQty[7],0) ,;
              Qty8   WITH MAX(Qty8-laDistQty[8],0) ,;
              TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
      UNLOCK 
      IF TotQty = 0
        DELETE
      ENDIF
      m.Cut1 = MIN(m.Cut1,laDistQty[1])
      m.Cut2 = MIN(m.Cut2,laDistQty[2])
      m.Cut3 = MIN(m.Cut3,laDistQty[3])
      m.Cut4 = MIN(m.Cut4,laDistQty[4])
      m.Cut5 = MIN(m.Cut5,laDistQty[5])
      m.Cut6 = MIN(m.Cut6,laDistQty[6])
      m.Cut7 = MIN(m.Cut7,laDistQty[7])
      m.Cut8 = MIN(m.Cut8,laDistQty[8])
      m.TotCut = m.Cut1+m.Cut2+m.Cut3+m.Cut4+m.Cut5+m.Cut6+m.Cut7+m.Cut8
      lnRecNo  = RECNO('CUTPICK')
      SELECT CUTPICK
      SET ORDER TO TAG CUTPKORD
      IF !SEEK('1'+m.CtktNo+m.cTktLineNo+laData[1]+&lcOrdLine..STYLE+STR(&lcOrdLine..LINENO,6))
        INSERT INTO 'CUTPICK' ;
        (CTKTNO,CTKTLINTNO,TRANCD,ORDER,CORDLINE,STYLE) VALUES ;
        (m.CTKTNO,m.cTktLineNo,'1',laData[1],STR(&lcOrdLine..LineNo,6),&lcOrdLine..STYLE)
      ENDIF   
      REPLACE Qty1   WITH Qty1   + m.Cut1 ,;
              Qty2   WITH Qty2   + m.Cut2 ,;
              Qty3   WITH Qty3   + m.Cut3 ,;
              Qty4   WITH Qty4   + m.Cut4 ,;
              Qty5   WITH Qty5   + m.Cut5 ,;
              Qty6   WITH Qty6   + m.Cut6 ,;
              Qty7   WITH Qty7   + m.Cut7 ,;
              Qty8   WITH Qty8   + m.Cut8 ,;
              TotQty WITH TotQty + m.TotCut
      SET ORDER TO TAG CUTORD
      GOTO lnRecNo
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE Cut1   WITH Cut1+m.Cut1 ,;
              Cut2   WITH Cut2+m.Cut2 ,;
              Cut3   WITH Cut3+m.Cut3 ,;
              Cut4   WITH Cut4+m.Cut4 ,;
              Cut5   WITH Cut5+m.Cut5 ,;
              Cut6   WITH Cut6+m.Cut6 ,;
              Cut7   WITH Cut7+m.Cut7 ,;
              Cut8   WITH Cut8+m.Cut8 ,;
              TotCut WITH Cut1+Cut2+Cut3+Cut4+Cut5+Cut6+Cut7+Cut8
      UNLOCK
      SELECT (lcOrdHdr)
      =RLOCK()
      REPLACE TotCut WITH TotCut + m.TotCut
      UNLOCK
      IF SEEK('O'+&lcOrdHdr..cFromOrder+STR(&lcOrdLine..BulkLineNo,6),'ORDLINE')
        SELECT ORDLINE
        =RLOCK()
        REPLACE Cut1   WITH MAX(Cut1-m.Cut1,0) ,;
                Cut2   WITH MAX(Cut2-m.Cut2,0) ,;
                Cut3   WITH MAX(Cut3-m.Cut3,0) ,;
                Cut4   WITH MAX(Cut4-m.Cut4,0) ,;
                Cut5   WITH MAX(Cut5-m.Cut5,0) ,;
                Cut6   WITH MAX(Cut6-m.Cut6,0) ,;
                Cut7   WITH MAX(Cut7-m.Cut7,0) ,;
                Cut8   WITH MAX(Cut8-m.Cut8,0) ,;
                TotCut WITH Cut1+Cut2+Cut3+Cut4+Cut5+Cut6+Cut7+Cut8
        UNLOCK
        IF SEEK('O'+&lcOrdHdr..cFromOrder,'ORDHDR')
          SELECT ORDHDR
          =RLOCK()
          REPLACE TotCut WITH MAX(TotCut - m.TotCut,0)
          UNLOCK
        ENDIF
      ENDIF
      laDistQty[1] = laDistQty[1] - m.Cut1
      laDistQty[2] = laDistQty[2] - m.Cut2
      laDistQty[3] = laDistQty[3] - m.Cut3
      laDistQty[4] = laDistQty[4] - m.Cut4
      laDistQty[5] = laDistQty[5] - m.Cut5
      laDistQty[6] = laDistQty[6] - m.Cut6
      laDistQty[7] = laDistQty[7] - m.Cut7
      laDistQty[8] = laDistQty[8] - m.Cut8
    ENDSCAN
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 9
    UNLOCK
  ENDIF
  *-- Check if any POs have been assigned to the bulk order.
  *-- If any, Release allocation from the bulk order and allocate them
  *-- to the newly received distribution order.
  SELECT (lcOrdLine)
  IF lcOrdType='O' AND laScrMode[4] AND 'PO' $ gcCmpModules .AND. ;
     !EMPTY(&lcOrdHdr..cFromOrder) AND &lcOrdLine..nSteps < 10
    SCATTER FIELDS ;
            Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laDistQty
    SELECT CUTPICK
    =SEEK('2'+&lcOrdHdr..cFromOrder+STR(&lcOrdLine..BulkLineNo,6))
    SCAN REST ;
         WHILE TRANCD+ORDER+CORDLINE='2'+&lcOrdHdr..cFromOrder+STR(&lcOrdLine..BulkLineNo,6) ;
         AND laDistQty[1]+laDistQty[2]+laDistQty[3]+laDistQty[4]+;
             laDistQty[5]+laDistQty[6]+laDistQty[7]+laDistQty[8] >0
      m.CtktLineNo = CtktLineNo
      m.CtktNo = CtktNo
      m.Cut1 = Qty1
      m.Cut2 = Qty2
      m.Cut3 = Qty3
      m.Cut4 = Qty4
      m.Cut5 = Qty5
      m.Cut6 = Qty6
      m.Cut7 = Qty7
      m.Cut8 = Qty8
      =RLOCK()            
      REPLACE Qty1   WITH MAX(Qty1-laDistQty[1],0) ,;
              Qty2   WITH MAX(Qty2-laDistQty[2],0) ,;
              Qty3   WITH MAX(Qty3-laDistQty[3],0) ,;
              Qty4   WITH MAX(Qty4-laDistQty[4],0) ,;
              Qty5   WITH MAX(Qty5-laDistQty[5],0) ,;
              Qty6   WITH MAX(Qty6-laDistQty[6],0) ,;
              Qty7   WITH MAX(Qty7-laDistQty[7],0) ,;
              Qty8   WITH MAX(Qty8-laDistQty[8],0) ,;
              TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
      UNLOCK 
      IF TotQty = 0
        DELETE
      ENDIF
      m.Cut1 = MIN(m.Cut1,laDistQty[1])
      m.Cut2 = MIN(m.Cut2,laDistQty[2])
      m.Cut3 = MIN(m.Cut3,laDistQty[3])
      m.Cut4 = MIN(m.Cut4,laDistQty[4])
      m.Cut5 = MIN(m.Cut5,laDistQty[5])
      m.Cut6 = MIN(m.Cut6,laDistQty[6])
      m.Cut7 = MIN(m.Cut7,laDistQty[7])
      m.Cut8 = MIN(m.Cut8,laDistQty[8])
      m.TotCut = m.Cut1+m.Cut2+m.Cut3+m.Cut4+m.Cut5+m.Cut6+m.Cut7+m.Cut8
      lnRecNo  = RECNO('CUTPICK')
      SELECT CUTPICK
      SET ORDER TO TAG CUTPKORD

      IF !SEEK('2'+m.CtktNo+m.cTktLineNo+laData[1]+&lcOrdLine..STYLE+STR(&lcOrdLine..LINENO,6))
        INSERT INTO 'CUTPICK' ;
        (CTKTNO,CTKTLINENO,TRANCD,ORDER,CORDLINE,STYLE) VALUES ;
        (m.CTKTNO,m.cTktLineNo,'2',laData[1],STR(&lcOrdLine..LineNo,6),&lcOrdLine..STYLE)
      ENDIF
      REPLACE Qty1   WITH Qty1   + m.Cut1 ,;
              Qty2   WITH Qty2   + m.Cut2 ,;
              Qty3   WITH Qty3   + m.Cut3 ,;
              Qty4   WITH Qty4   + m.Cut4 ,;
              Qty5   WITH Qty5   + m.Cut5 ,;
              Qty6   WITH Qty6   + m.Cut6 ,;
              Qty7   WITH Qty7   + m.Cut7 ,;
              Qty8   WITH Qty8   + m.Cut8 ,;
              TotQty WITH TotQty + m.TotCut
      SET ORDER TO TAG CUTORD
      GOTO lnRecNo
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE Cut1   WITH Cut1+m.Cut1 ,;
              Cut2   WITH Cut2+m.Cut2 ,;
              Cut3   WITH Cut3+m.Cut3 ,;
              Cut4   WITH Cut4+m.Cut4 ,;
              Cut5   WITH Cut5+m.Cut5 ,;
              Cut6   WITH Cut6+m.Cut6 ,;
              Cut7   WITH Cut7+m.Cut7 ,;
              Cut8   WITH Cut8+m.Cut8 ,;
              TotCut WITH Cut1+Cut2+Cut3+Cut4+Cut5+Cut6+Cut7+Cut8
      UNLOCK
      SELECT (lcOrdHdr)
      =RLOCK()
      REPLACE TotCut WITH TotCut + m.TotCut
      UNLOCK
      IF SEEK('O'+&lcOrdHdr..cFromOrder+STR(&lcOrdLine..BulkLineNo,6),'ORDLINE')
        SELECT ORDLINE
        =RLOCK()
        REPLACE Cut1   WITH MAX(Cut1-m.Cut1,0) ,;
                Cut2   WITH MAX(Cut2-m.Cut2,0) ,;
                Cut3   WITH MAX(Cut3-m.Cut3,0) ,;
                Cut4   WITH MAX(Cut4-m.Cut4,0) ,;
                Cut5   WITH MAX(Cut5-m.Cut5,0) ,;
                Cut6   WITH MAX(Cut6-m.Cut6,0) ,;
                Cut7   WITH MAX(Cut7-m.Cut7,0) ,;
                Cut8   WITH MAX(Cut8-m.Cut8,0) ,;
                TotCut WITH Cut1+Cut2+Cut3+Cut4+Cut5+Cut6+Cut7+Cut8
        UNLOCK
        IF SEEK('O'+&lcOrdHdr..cFromOrder,'ORDHDR')
          SELECT ORDHDR
          =RLOCK()
          REPLACE TotCut WITH MAX(TotCut - m.TotCut,0)
          UNLOCK
        ENDIF
      ENDIF
      laDistQty[1] = laDistQty[1] - m.Cut1
      laDistQty[2] = laDistQty[2] - m.Cut2
      laDistQty[3] = laDistQty[3] - m.Cut3
      laDistQty[4] = laDistQty[4] - m.Cut4
      laDistQty[5] = laDistQty[5] - m.Cut5
      laDistQty[6] = laDistQty[6] - m.Cut6
      laDistQty[7] = laDistQty[7] - m.Cut7
      laDistQty[8] = laDistQty[8] - m.Cut8
    ENDSCAN
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 10
    UNLOCK
  ENDIF
   
  SCATTER MEMVAR MEMO

  m.Start    = laData[9]
  m.Complete = IIF(!llCDPerL .OR. EMPTY(m.Complete),laData[10],m.Complete )
  m.Flag     = SPACE(1)
  m.Order    = laData[1]

  DO CASE
    CASE !llFromEDI AND !DELETED() .AND. !SEEK(lcOrdType+laData[1]+STR(LineNo,6),'OrdLine')
      lnLineCount=lnLineCount+1
      m.LineNo = lnLineCount
      INSERT INTO ORDLINE FROM MEMVAR
      IF SEEK(lcOrdType+laData[1],'ORDHDR')
        SELECT ORDHDR
        REPLACE LastLine WITH lnLineCount
      ENDIF
    CASE (llFromEDI .AND. SEEK('T'+Order+STR(LineNo,6),'OrdLine') ) .OR. ;
         (!llFromEDI AND !DELETED() .AND. SEEK(lcOrdType+Order+STR(LineNo,6),'OrdLine'))
      m.cOrdType = IIF(llFromEDI,'O',m.cOrdType)
      lnLineCount = IIF(llFromEDI,m.LineNo,lnLineCount)
      SELECT ORDLINE
      
      IF llMFDsPrc .OR. llPODsPrc
        lnOldPrc = Price 
        lnOldQty = TotQty
        GATHER MEMVAR MEMO
        lnOldQty = IIF(llUpdAlo,TotQty,lnOldQty)
        =lfUpdtPoCT(Order,Style,STR(LineNo,6),TotQty,Price,lnOldPrc,lnOldQty,.F.)
      ELSE
        GATHER MEMVAR MEMO
      ENDIF
      
    CASE DELETED() .AND. SEEK(lcOrdType+Order+STR(LineNo,6),'OrdLine')
      SELECT ORDLINE

      lcOrder  = Order
      lclineNo = STR(LineNo,6)
      lcStyle  = Style
      lnTotQty = 0
      lnPrice  = 0
      lnOldPrc = Price
      lnOldQty = TotQty
      DELETE
      IF llMFDsPrc .OR. llPODsPrc
        =lfUpdtPoCT(lcOrder,lcStyle,lclineNo,lnTotQty,lnPrice,lnOldPrc,lnOldQty,.T.)      
      ENDIF
  ENDCASE
  
  SELECT (lcOrdLine)
  =RLOCK()
  REPLACE Order  WITH laDAta[1] ,;
          LineNo WITH m.LineNo
  UNLOCK
ENDSCAN

  SELECT ORDLINE
  IF SEEK(lcOrdType+laData[1])
    REPLACE REST Start WITH laData[9] , Complete WITH IIF(llCDPerL,Complete,laData[10]) ;
           WHILE cOrdType+Order=lcOrdType+laData[1]
    
  ENDIF

IF llBomVarnt
  =gfTmp2Mast('BOMVAR' , lcT_BomVar , 'Update the style positions for the order lines...')
ENDIF

=gfCloseFile('icStyHst')
=gfCloseFile('ORDCANLN')
=IIF('AL' $ gcCmpModules,gfCloseFile('PikTkt'),.T.)
IF lcOrdType='O' AND laScrMode[4] AND !EMPTY(&lcOrdHdr..cFromOrder) AND ;
  ('PO' $ gcCmpModules .OR. 'MF' $ gcCmpModules)
  =gfCloseFile('CUTPICK')
ENDIF

SET DELETE ON
WAIT 'Updating order header...' WINDOW NOWAIT
=gfOpenFile(gcDataDir+'arCusHst',gcDataDir+'Acthst','SH')

SELECT ORDHDR
IF !llFromEDI .AND. !SEEK(lcOrdType+laData[1])
  INSERT INTO ORDHDR (cOrdType,Order) VALUES (lcOrdType,laData[1])
ENDIF
IF llFromEDI .AND. SEEK('T'+lcEdiOrd)
  REPLACE cOrdType WITH 'O' ,;
          Order    WITH laData[1]
ENDIF

*-- Update orders Quantity & Amount in the Customer History file
IF lcOrdType='O' .AND. laData[5]<>'B' .AND. SEEK(laData[2]+lcGlYear,'arCusHst') .AND. ;
   &lcOrdHdr..nSteps < 1
  lnOrdAmt = laData[42] &lcExRSin laData[34] &lcUntSin laData[50] - ;
  IIF(laScrMode[4] OR OrdHdr.Status='B',0,OrdHdr.OpenAmt &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit)
  SELECT arCusHst
  =RLOCK()
  REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod -;
            IIF(OrdHdr.Status='B',0,OrdHdr.Open) + laData[41],;
          nOrdQty WITH nOrdQty -;
            IIF(OrdHdr.Status='B',0,OrdHdr.Open) + laData[41],;
          nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + lnOrdAmt ,;
          nOrdAmt WITH nOrdAmt + lnOrdAmt
  UNLOCK
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE nSteps WITH 1
  UNLOCK
ENDIF

IF lcOrdType='O' .AND. laData[5]<>'B' .AND. laScrMode[4] .AND. ;
   SEEK(lcOrdType+&lcOrdHdr..cFromOrder,'OrdHdr')
  *-- Add bulk orders approve Amount in the header of new order from bulk order .
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE APPRAMT  WITH MIN(&lcOrdHdr..OPENAMT,ORDHDR.APPRAMT)
  UNLOCK

  *-- Decrease bulk orders approve Amount in the header of bulk order.
  SELECT ORDHDR
  =RLOCK()
  REPLACE APPRAMT  WITH MAX(0,APPRAMT - &lcOrdHdr..OPENAMT)
  UNLOCK
ENDIF

IF lcOrdType='O' .AND. laData[5]<>'B' .AND. (laScrMode[4] .OR. OrdHdr.Status='B') .AND. ;
   SEEK(lcOrdType+&lcOrdHdr..cFromOrder,'OrdHdr')

  *-- Decrease bulk orders depleted Quantity & Amount in the Customer History file
  IF &lcOrdHdr..nSteps < 2 .AND. SEEK(laData[2]+lcBulkYear,'arCusHst')
    lnOrdAmt = lnCancelAmt &lcExRSin laData[34] &lcUntSin laData[50]
    SELECT arCusHst
    =RLOCK()
    REPLACE nOrdQty&lcBulkPrd WITH nOrdQty&lcBulkPrd - lnCancel ,;
            nOrdQty           WITH nOrdQty           - lnCancel ,;
            nOrdAmt&lcBulkPrd WITH nOrdAmt&lcBulkPrd - lnOrdAmt ,;
            nOrdAmt           WITH nOrdAmt           - lnOrdAmt
    UNLOCK
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 2
    UNLOCK
  ENDIF
  IF &lcOrdHdr..nSteps < 3
    SELECT ORDHDR
    =RLOCK('ORDHDR')

    IF BULK = 'Y'
      REPLACE Book      WITH Book      - lnCancel   ,;
              BookAmt   WITH BookAmt   - lnCancelAmt,;
              Open      WITH Book      - Cancel     ,;
              Openamt   WITH Bookamt   - Cancelamt  ,;
              Status    WITH IIF(OrdHdr.Open = 0 ,'X',OrdHdr.Status)
   
    ELSE
      REPLACE Cancel    WITH Cancel    + lnCancel   ,;
              Cancelamt WITH Cancelamt + lnCancelAmt,;
              Open      WITH Book      - Cancel     ,;
              Openamt   WITH Bookamt   - Cancelamt  ,;
              Status    WITH IIF(OrdHdr.Open = 0 ,'X',OrdHdr.Status)
    ENDIF
    UNLOCK IN 'ORDHDR'
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 3
    UNLOCK
  ENDIF  
  IF OrdHdr.Status='X' .AND. &lcOrdHdr..nSteps < 4 .AND. ;
    SEEK('M'+OrdHdr.Account,'Customer')
    SELECT Customer
    =RLOCK('Customer')
    REPLACE nBulk WITH nBulk - 1
    UNLOCK IN 'Customer'
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 4
    UNLOCK
  ENDIF
ENDIF
=gfCloseFile('arCusHst')
IF lcOrdType='O' AND laData[5]<>'B' AND (laScrMode[4] OR OrdHdr.Status='B') AND ;
   &lcOrdHdr..Bulk='Y' AND &lcOrdHdr..nSteps < 5 AND SEEK('M'+laData[2],'Customer')
  SELECT Customer
  =RLOCK()
  REPLACE nBulk WITH nBulk + 1
  UNLOCK
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE nSteps WITH 5
  UNLOCK
ENDIF
SELECT ORDHDR
=SEEK(lcOrdType+laData[1])
=RLOCK()
laData[5] = IIF(laData[41] = 0  ,IIF(laData[37]>0,'C','X'),laData[5])
GATHER FROM laData FIELDS &lcScFields

REPLACE cOrdType   WITH lcOrdType ,;
        LastLine   WITH lnLineCount  ,;
        cFromOrder WITH &lcOrdHdr..cFromOrder ,;
        StName     WITH IIF(laData[51],lcShipName,SPACE(30)) ,;
        cAddress1  WITH IIF(laData[51],lcShipAdd1,SPACE(30)) ,;
        cAddress2  WITH IIF(laData[51],lcShipAdd2,SPACE(30)) ,;
        cAddress3  WITH IIF(laData[51],lcShipAdd3,SPACE(30)) ,;
        cAddress4  WITH IIF(laData[51],lcShipAdd4,SPACE(30)) ,;
        cAddress5  WITH IIF(laData[51],lcShipAdd5,SPACE(30)) ,;
        TotCut     WITH &lcOrdHdr..TotCut                    ,;
        APPRAMT    WITH &lcOrdHdr..APPRAMT 

UNLOCK

*-- If the function was not called from EDI
IF !llFromEDI
  SELECT unCmSess
  =SEEK('O'+PADR('SOORD',10)+gcUser_id+lcSession)
  REPLACE STATUS WITH 'C'
  UNLOCK
  llContinue = .F.
  UNLOCK 
ENDIF    && End of IF !llFromEDI

WAIT CLEAR 
IF laScrMode[4] AND !llFromEDI
  *-- Message : 32045
  *-- Order has been saved as xxxxxx
  *-- Button : 00000 
  *-- Ok
  =gfModalGen('INM32045B00000','DIALOG',IIF(lcOrdType='C','Contract','Order')+'|'+laData[1])
ENDIF
SET ORDER TO TAG 'ORDLINE' IN (lcOrdLine)

IF !llFromEDI AND lcOrdType='O' AND 'EB' $ gcCmpModules
  =gfOpenFile(gcDataDir+'EDIACPRT',gcDataDir+'ACCFACT','SH')
  =gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PARTTRANS','SH')
  =gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')

  IF SEEK('A'+laData[2],'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'855','EDIPD')
    SELECT EDITRANS
    IF !SEEK('855'+PADR(laData[1],20)+'A'+laData[2])
      INSERT INTO 'EDITRANS' (cEdiTrnTyp,Key,Type,cPartner) VALUES ;
      ('855',laData[1],'A',laData[2])
    ENDIF
    REPLACE cStatus WITH 'N'
    =gfAdd_Info('EDITRANS')
  ENDIF 
  =gfCloseFile('EDIACPRT')
  =gfCloseFile('EDIPD')
  =gfCloseFile('EDITRANS')
ENDIF

*C102118,1 Update Soccer Master File. [Begin]
DO lpUpDatSoc
*C102118,1 Update Soccer Master File. [End]

SELECT ORDHDR
RETURN
*-- End of lfSavScr.

*!*************************************************************
*! Name      : lfDelScr
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
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
*
FUNCTION lfDelScr

*-- Add this line to add a new parameter to know if the function was
*-- Called from EDI - from EDI transaction 860
*-- (Purchase Order Modify) -. [Begin]
PARAMETERS llFromEDI

llFromEDI = IIF(TYPE('llFromEDI') = 'L' , llFromEDI , .F.)

PRIVATE lnTktCnt

*C102118,1 Before deleting we need to Change all JY Group to 2 lines. [Begin]
DO lpAdjLines
*C102118,1 Before deleting we need to Change all JY Group to 2 lines. [End]

IF lcOrdType='O' .AND. !CHECKPRD(OrdHdr.Entered,'lcGlYear','lcGlPeriod ','',.T.)
  RETURN
ENDIF
lcUntSin = ''
lcExRSin = gfGetExSin(@lcUntSin, laData[33])

*-- Open the order cancellation table if we have shipped qty.
IF ORDHDR.SHIP > 0
  =gfOpenFile(gcDataDir+'ORDCANLN',gcDataDir+'ORDCANLN','SH') 
ENDIF
*ahmed

*-- Uncancel order
IF OrdHdr.Status='X'
  IF lcOrdType='O' .AND. !SEEK(lcOrdType+laData[1],'ORDLINE')
    *-- Message : 32000
    *-- The lines for this order are missing! Cannot update cut & sold.
    *-- Button : 00000 
    *-- Ok
    =gfModalGen('TRM32000B00000','ALERT')
    RETURN
  ENDIF
  
  SET ORDER TO TAG ORDLINE IN ORDLINE
  SET ORDER TO TAG ORDHDR  IN ORDHDR  
  IF !llContinue
    IF OrdHdr.Cancel = 0 AND OrdHdr.Bulk <> 'Y'
      SELECT ORDHDR
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
    SELECT *, 00 AS nSteps FROM ORDHDR WHERE cordtype+order=lcOrdType+laData[1];
    INTO DBF (gcWorkDir+lcOrdHdr)
    SELECT *, 00 AS nSteps FROM ORDLINE WHERE cordtype+order+STR(lineno,6)=;
    lcOrdType+laData[1] INTO DBF (gcWorkDir+lcOrdLine)
    
    SELECT 'UNCMSESS'
    IF SEEK('I')
      BLANK
    ELSE
      APPEND BLANK
    ENDIF
    REPLACE Status     WITH 'O'       ,;
            cUTranType WITH 'SOORD'   ,;
            cUserId    WITH gcUser_id ,;
            cSession   WITH lcSession ,;
            cProgram   WITH 'SOORD'   ,;
            cCurrScr   WITH 'SOORD'   ,;
            cCurrObj   WITH 'PBDLT'   ,;
            dTranDate  WITH gdSysDate ,;
            cTranTime  WITH TIME()
    =RLOCK()
    lcCurrOrd = laData[1]

    *C102118,1 We don't use Uncomplete session in Custom Programs. [Begin] 
    *lcFiles = 'lcOrdHdr,'+lcOrdHdr+','+lcOrdHdr+';'+;
    *          'lcOrdLine,'+lcOrdLine+',lcOrdLine;'
    *=gfSavSess('SOORD', lcFiles, @laVariables,lcSession)
    *C102118,1 We don't use Uncomplete session in Custom Programs. [End] 
    
  ENDIF
  IF lcOrdType='O'
    WAIT 'Uncancelling and updating Cut & Sold...' WINDOW NOWAIT
  ENDIF
  STORE 0 TO lnNewOpen,lnNewOAmt
  =gfOpenFile(gcDataDir+'icStyHst',gcDataDir+'Styhst','SH') 
  SELECT (lcOrdLine)
  SCAN
    *-- Increase warehouse ordered quantity
    IF lcOrdType='O' .AND. nSteps < 1 .AND. SEEK(Style+laData[31]+SPACE(10),'StyDye')
      SELECT StyDye
      =RLOCK()
      REPLACE ORD1   WITH ORD1 + &lcOrdLine..QTY1 ,;
              ORD2   WITH ORD2 + &lcOrdLine..QTY2 ,;
              ORD3   WITH ORD3 + &lcOrdLine..QTY3 ,;
              ORD4   WITH ORD4 + &lcOrdLine..QTY4 ,;
              ORD5   WITH ORD5 + &lcOrdLine..QTY5 ,;
              ORD6   WITH ORD6 + &lcOrdLine..QTY6 ,;
              ORD7   WITH ORD7 + &lcOrdLine..QTY7 ,;
              ORD8   WITH ORD8 + &lcOrdLine..QTY8 ,;
              TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
      UNLOCK
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE nSteps WITH 1
      UNLOCK
    ENDIF
    *-- Increase style ordered quantity
    IF lcOrdType='O' .AND. nSteps < 2 .AND. SEEK(STYLE,'Style')
      SELECT STYLE
      =RLOCK()
      REPLACE ORD1   WITH ORD1 + &lcOrdLine..QTY1 ,;
              ORD2   WITH ORD2 + &lcOrdLine..QTY2 ,;
              ORD3   WITH ORD3 + &lcOrdLine..QTY3 ,;
              ORD4   WITH ORD4 + &lcOrdLine..QTY4 ,;
              ORD5   WITH ORD5 + &lcOrdLine..QTY5 ,;
              ORD6   WITH ORD6 + &lcOrdLine..QTY6 ,;
              ORD7   WITH ORD7 + &lcOrdLine..QTY7 ,;
              ORD8   WITH ORD8 + &lcOrdLine..QTY8 ,;
              TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
      UNLOCK
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE nSteps WITH 2
      UNLOCK
    ENDIF
    *-- Increase ordered quantity and amount in the style history file
    IF lcOrdType='O' .AND. nSteps < 3 .AND. SEEK(Style+lcGlYear,'icStyHst')
      lnOrdAmt = &lcOrdLine..TotQty*&lcOrdLine..Price &lcExRSin laData[34] &lcUntSin laData[50]
      SELECT icStyHst
      =RLOCK()
      REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod + &lcOrdLine..TotQty ,;
              nOrdQty            WITH nOrdQty            + &lcOrdLine..TotQty ,;
              nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + lnOrdAmt ,;
              nOrdAmt            WITH nOrdAmt            + lnOrdAmt
      UNLOCK
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE nSteps WITH 3
      UNLOCK
    ENDIF
    lnNewOpen = lnNewOpen+ TotQty
    lnNewOAmt = lnNewOAmt+ TotQty*Price
  ENDSCAN
  =gfCloseFile('icStyHst')
  *-- Increase number of bulk orders for this account
  IF laData[23]='Y' .AND. &lcOrdHdr..nSteps < 1 .AND. SEEK('M'+laData[2],'Customer')
    SELECT Customer
    =RLOCK()
    REPLACE nBulk WITH nBulk + 1
    UNLOCK
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 1
    UNLOCK
  ENDIF
  *-- Update order header cancel and open quantity and amount
  IF &lcOrdHdr..nSteps < 2
    SELECT ORDHDR
    =RLOCK()
    REPLACE Status     WITH 'O'      ,;
            cCancReson WITH SPACE(6) ,;
            Cancelled  WITH {} ,;
            Cancel     WITH IIF(BULK='Y',Cancel,Cancel-lnNewOpen) ,;
            CancelAmt  WITH IIF(BULK='Y',CancelAmt,CancelAmt-lnNewOAmt) ,;
            Open       WITH lnNewOpen ,;
            OpenAmt    WITH lnNewOAmt ,;
            Flag       WITH SPACE(1)
    UNLOCK

    IF laData[23]='Y'
      =RLOCK()
      REPLACE BOOK    WITH  Ship + Cancel   + lnNewOpen ,;
              BOOKAMT WITH  ShipAmt + CancelAmt+ lnNewOAmt 
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
    REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod + lnNewOpen ,;
            nOrdQty            WITH nOrdQty            + lnNewOpen ,;
            nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + lnOrdAmt  ,;
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
  IF ORDHDR.STATUS='C'
    *-- Message : 32003
    *-- Order has been shipped complete! Cannot cancel.
    *-- Button : 00000 
    *-- Ok
    =gfModalGen('TRM32003B00000','ALERT')
    RETURN
  ENDIF
  *-- Do not cancel order with no lines
  IF !(lcOrdType='T' AND ORDHDR.Mon_Flg='L') AND !SEEK(lcOrdType+laData[1],'ORDLINE')
    *-- Message : 32000
    *-- The lines for this order are missing! Cannot update cut & sold.
    *-- Button : 00000 
    *-- Ok
    =gfModalGen('TRM32000B00000','ALERT')
    RETURN
  ENDIF
  SET ORDER TO TAG ORDLINE IN ORDLINE
  SET ORDER TO TAG ORDHDR  IN ORDHDR  
  IF !llContinue
    IF 'AL' $ gcCmpModules
      =gfOpenFile(gcDataDir+'PIKTKT',gcDataDir+'ORDPIK','SH')
       
      *-- Do not cancel picked orders 
      IF SEEK(OrdHdr.Order,'PikTkt')
        SELECT PikTkt
        LOCATE REST WHILE Order = OrdHdr.Order FOR Status $ 'OHP'
        IF FOUND()
          *-- Message : 32004
          *-- Some order lines hve been picked.
          *-- Button : 00000 
          *-- Ok
          =gfModalGen('INM32004B00000','ALERT')
        ENDIF
      ENDIF
      =gfOpenFile(gcDataDir+'PIKLINE',gcDataDir+'PIKLINE','SH') 
      SET ORDER TO TAG 'PIKTKT' IN 'PIKTKT'
    ENDIF

    IF ORDHDR.TotCut > 0
      *-- Message : 32054
      *-- This order has quantity allocated.
      *-- Canceling this order will release this allocation.
      *-- Would you like to continue ? 
      *-- Button : 32000
      *-- Yes  No
      IF gfModalGen('QRM32054B32000','ALERT')=2
        RETURN
      ENDIF
      IF 'PO' $ gcCmpModules
        =gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
        =gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLN','SH')
      ENDIF
      IF 'MF' $ gcCmpModules
        =gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')
        =gfOpenFile(gcDataDir+'CUTTKTL',gcDataDir+'CUTLIN','SH')
      ENDIF
      =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
    ENDIF
    
    *-- Message : 32005
    *-- Cancel all open items on this order?
    *-- Button : 320000
    *-- Yes/No
    IF gfModalGen('QRM32005B32000','ALERT',IIF(lcOrdType='C','contract','order')) = 2
      RETURN
    ENDIF
    IF ORDHDR.STATUS='B'
      SELECT ORDHDR
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

    SELECT *, 00 AS nSteps FROM ORDHDR WHERE cordtype+order=lcOrdType+laData[1];
    INTO DBF (gcWorkDir+lcOrdHdr)
    m.cCancReson = lfCanReason()
    REPLACE cCancReson WITH m.cCancReson
    SELECT *, 00 AS nSteps FROM ORDLINE WHERE cordtype+order+STR(lineno,6)=;
    lcOrdType+laData[1] INTO DBF (gcWorkDir+lcOrdLine)
    
    SELECT 'UNCMSESS'
    IF SEEK('I')
      BLANK
    ELSE
      APPEND BLANK
    ENDIF
    REPLACE Status     WITH 'O'       ,;
            cUTranType WITH 'SOORD'   ,;
            cUserId    WITH gcUser_id ,;
            cSession   WITH lcSession ,;
            cProgram   WITH 'SOORD'   ,;
            cCurrScr   WITH 'SOORD'   ,;
            cCurrObj   WITH 'PBDLT'   ,;
            dTranDate  WITH gdSysDate ,;
            cTranTime  WITH TIME()
    =RLOCK()
    lcCurrOrd = laData[1]

    *C102118,1 We don't use Uncomplete session in Custom Programs. [Begin] 
    *lcFiles = 'lcOrdHdr,'+lcOrdHdr+','+lcOrdHdr+';'+;
    *          'lcOrdLine,'+lcOrdLine+',lcOrdLine;'
    *=gfSavSess('SOORD', lcFiles, @laVariables,lcSession)
    *C102118,1 We don't use Uncomplete session in Custom Programs. [End] 
    
  ENDIF
  WAIT 'Cancelling and updating Cut & Sold...' WINDOW NOWAIT 
  =gfOpenFile(gcDataDir+'icStyHst',gcDataDir+'Styhst','SH') 
  SELECT (lcOrdLine)
  STORE 0 TO lnOpen,lnOpenAmt,lnBook,lnBookAmt
  SCAN
    *-- Decrease warehouse ordered quantity with order open quantity
    IF lcOrdType='O' .AND. SEEK(&lcOrdLine..Style+&lcOrdLine..cWareCode+SPACE(10),'StyDye')
      IF &lcOrdLine..nSteps < 1
        SELECT StyDye
        =RLOCK()
        REPLACE Ord1   WITH MAX(Ord1-&lcOrdLine..Qty1,0) ,;
                Ord2   WITH MAX(Ord2-&lcOrdLine..Qty2,0) ,;
                Ord3   WITH MAX(Ord3-&lcOrdLine..Qty3,0) ,;
                Ord4   WITH MAX(Ord4-&lcOrdLine..Qty4,0) ,;
                Ord5   WITH MAX(Ord5-&lcOrdLine..Qty5,0) ,;
                Ord6   WITH MAX(Ord6-&lcOrdLine..Qty6,0) ,;
                Ord7   WITH MAX(Ord7-&lcOrdLine..Qty7,0) ,;
                Ord8   WITH MAX(Ord8-&lcOrdLine..Qty8,0) ,;
                TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
        UNLOCK
        SELECT (lcOrdLine)
        =RLOCK()
        REPLACE nSteps WITH 1
        UNLOCK
      ENDIF
      *-- Decrease warehouse allocated quantity with order picked quantity
      IF &lcOrdLine..PICKED .AND. &lcOrdLine..nSteps < 2
        SELECT StyDye
        =RLOCK()
        REPLACE Alo1   WITH MAX(Alo1-&lcOrdLine..Pik1,0) ,;
                Alo2   WITH MAX(Alo2-&lcOrdLine..Pik2,0) ,;
                Alo3   WITH MAX(Alo3-&lcOrdLine..Pik3,0) ,;
                Alo4   WITH MAX(Alo4-&lcOrdLine..Pik4,0) ,;
                Alo5   WITH MAX(Alo5-&lcOrdLine..Pik5,0) ,;
                Alo6   WITH MAX(Alo6-&lcOrdLine..Pik6,0) ,;
                Alo7   WITH MAX(Alo7-&lcOrdLine..Pik7,0) ,;
                Alo8   WITH MAX(Alo8-&lcOrdLine..Pik8,0) ,;
                TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
        UNLOCK
        SELECT (lcOrdLine)
        =RLOCK()
        REPLACE nSteps WITH 2
        UNLOCK
      ENDIF
    ENDIF
    *-- Decrease style ordered quantity with order open quantity
    IF lcOrdType='O' .AND. SEEK(&lcOrdLine..STYLE,'Style')
      IF &lcOrdLine..nSteps < 3
        SELECT Style
        =RLOCK()
        REPLACE Ord1   WITH MAX(Ord1-&lcOrdLine..Qty1,0) ,;
                Ord2   WITH MAX(Ord2-&lcOrdLine..Qty2,0) ,;
                Ord3   WITH MAX(Ord3-&lcOrdLine..Qty3,0) ,;
                Ord4   WITH MAX(Ord4-&lcOrdLine..Qty4,0) ,;
                Ord5   WITH MAX(Ord5-&lcOrdLine..Qty5,0) ,;
                Ord6   WITH MAX(Ord6-&lcOrdLine..Qty6,0) ,;
                Ord7   WITH MAX(Ord7-&lcOrdLine..Qty7,0) ,;
                Ord8   WITH MAX(Ord8-&lcOrdLine..Qty8,0) ,;
                TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
        UNLOCK
        SELECT (lcOrdLine)
        =RLOCK()
        REPLACE nSteps WITH 3
        UNLOCK
      ENDIF
      *-- Decrease style allocated quantity with order picked quantity
      IF &lcOrdLine..PICKED .AND. &lcOrdLine..nSteps < 4
        SELECT Style
        =RLOCK()
        REPLACE Alo1   WITH MAX(Alo1-&lcOrdLine..Pik1,0) ,;
                Alo2   WITH MAX(Alo2-&lcOrdLine..Pik2,0) ,;
                Alo3   WITH MAX(Alo3-&lcOrdLine..Pik3,0) ,;
                Alo4   WITH MAX(Alo4-&lcOrdLine..Pik4,0) ,;
                Alo5   WITH MAX(Alo5-&lcOrdLine..Pik5,0) ,;
                Alo6   WITH MAX(Alo6-&lcOrdLine..Pik6,0) ,;
                Alo7   WITH MAX(Alo7-&lcOrdLine..Pik7,0) ,;
                Alo8   WITH MAX(Alo8-&lcOrdLine..Pik8,0) ,;
                TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
        UNLOCK
        SELECT (lcOrdLine)
        =RLOCK()
        REPLACE nSteps WITH 4
        UNLOCK
      ENDIF
    ENDIF
    IF lcOrdType='O' AND &lcOrdLine..nSteps < 5 AND &lcOrdLine..PICKED AND !EMPTY(&lcOrdLine..Dyelot) AND ;
       SEEK(&lcOrdLine..Style+&lcOrdLine..cWareCode+&lcOrdLine..Dyelot,'StyDye')
      SELECT StyDye
      =RLOCK()
      REPLACE Alo1   WITH MAX(Alo1-&lcOrdLine..Pik1,0) ,;
              Alo2   WITH MAX(Alo2-&lcOrdLine..Pik2,0) ,;
              Alo3   WITH MAX(Alo3-&lcOrdLine..Pik3,0) ,;
              Alo4   WITH MAX(Alo4-&lcOrdLine..Pik4,0) ,;
              Alo5   WITH MAX(Alo5-&lcOrdLine..Pik5,0) ,;
              Alo6   WITH MAX(Alo6-&lcOrdLine..Pik6,0) ,;
              Alo7   WITH MAX(Alo7-&lcOrdLine..Pik7,0) ,;
              Alo8   WITH MAX(Alo8-&lcOrdLine..Pik8,0) ,;
              TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
      UNLOCK
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE nSteps WITH 5
      UNLOCK
    ENDIF

    *-- Decrease ordered quantity & amount in the style history with order open quantity 
    IF lcOrdType='O' .AND. &lcOrdLine..nSteps < 6 .AND. SEEK(Style+lcGlYear,'icStyHst')
      lnOrdAmt = &lcOrdLine..TotQty*&lcOrdLine..Price &lcExRSin laData[34] &lcUntSin laData[50]
      SELECT icStyHst
      =RLOCK()
      REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod - &lcOrdLine..TotQty ,;
              nOrdQty            WITH nOrdQty            - &lcOrdLine..TotQty ,;
              nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod - lnOrdAmt ,;
              nOrdAmt            WITH nOrdAmt            - lnOrdAmt
      UNLOCK
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE nSteps WITH 6
      UNLOCK
    ENDIF
    *-- Release pick ticket and zero out order picked quantity
    =SEEK(lcOrdType+&lcOrdLine..ORDER+STR(&lcOrdLine..LINENO,6),'ORDLINE')
    IF &lcOrdLine..Picked
      IF SEEK(&lcOrdLine..PikTkt,'PIKTKT')
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
      DO WHILE SEEK(IIF(Style.Make,'1','2')+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6))
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
        *-- Message : 32006
        *-- Cuttkt# xxxxxx status is HOLD. Do you wish to update the generated 
        *-- cuttkt quantities accordingly?
        *-- Button : 32000 
        *-- Yes/No

        *-- Message : 32007
        *-- Purchase order# xxxxxx status is HOLD. Do you wish to update the generated 
        *-- Purchase order quantities accordingly?
        *-- Button : 32000 
        *-- Yes/No
  
        *-- Message : 32008
        *-- Do you wish to update Purchase order# 999999 quantities accordingly?
        *-- Button : 32000 
        *-- Yes/No
        lnTicket=ASCAN(laTickets,CutPick.CTKTNO)
        IF lnTicket = 0
          *llUpdate= (CutPick.TranCd='1' .AND. CUTTKTH.Status='H' .AND. ;
            gfModalGen('QRM32006B32000','ALERT',CutPick.CTKTNO)=1) .OR. ;
            (CutPick.TranCd='2' .AND. ( ( POSHDR.Status='H' .AND. ;
             gfModalGen('QRM32007B32000','ALERT',CutPick.CTKTNO)=1) .OR. ;
            ( POSHDR.Status $ 'OA' .AND. !laSetups[13,2] .AND. ;
              gfModalGen('QRM32008B32000','ALERT',CutPick.CTKTNO)=1) ))
          llUpdate= (CutPick.TranCd='1' .AND. CUTTKTH.Status='H' .AND. ;
            gfModalGen('QRM32006B32000','ALERT',CutPick.CTKTNO)=1) .OR. ;
                    (CutPick.TranCd='2' .AND. POSHDR.Status='H'  .AND. ;
            gfModalGen('QRM32007B32000','ALERT',CutPick.CTKTNO)=1)

          lnTktCnt=lnTktCnt+1
          DIMENSION laTickets[lnTktCnt,2]
          laTickets[lnTktCnt,1]=CutPick.CTKTNO
          laTickets[lnTktCnt,2]=IIF(llUpdate,'Y','N')
        ELSE
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
      IF !SEEK(lcOrdType+Order+STR(LineNo,6),'ORDCANLN')
        INSERT INTO ('ORDCANLN') ;
          (cOrdType,Order,LineNo,Cancelled,cCancReson,Qty1,Qty2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY, ;
           ACCOUNT,STYLE,STORE,DYELOT) ;
          VALUES ;
          (lcOrdType,laData[1],OrdLine.LineNo,gdSysDate,&lcOrdHdr..cCancReson,OrdLine.Qty1,OrdLine.Qty2,;
          OrdLine.Qty3,OrdLine.Qty4,OrdLine.Qty5,OrdLine.Qty6,OrdLine.Qty7,OrdLine.Qty8,;
          OrdLine.TOTQty,ORDLINE.ACCOUNT,ORDLINE.STYLE,ORDLINE.STORE,ORDLINE.DYELOT)
      ENDIF
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
    *ahmed end
    
  ENDSCAN
  =gfCloseFile('icStyHst')
  *-- Decrease number of bulk orders for this account
  IF laData[23]='Y' .AND. &lcOrdHdr..nSteps < 1 .AND. SEEK('M'+laData[2],'Customer')
    SELECT CUSTOMER
    =RLOCK()
    REPLACE nBulk WITH nBulk - 1
    UNLOCK
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 1
    UNLOCK
  ENDIF
  *-- Decrease orders quantity and amount in the customerr history file
  IF lcOrdType='O' .AND. &lcOrdHdr..nSteps < 2 
    =gfOpenFile(gcDataDir+'arCusHst',gcDataDir+'Acthst','SH')
    =SEEK(laData[2]+lcGlYear,'arCusHst')
    lnOrdAmt = OrdHdr.OpenAmt &lcExRSin laData[34] &lcUntSin laData[50]
    SELECT arCusHst
    =RLOCK()
    REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod - OrdHdr.Open ,;
            nOrdQty            WITH nOrdQty            - OrdHdr.Open ,;
            nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod - lnOrdAmt    ,;
            nOrdAmt            WITH nOrdAmt            - lnOrdAmt
    UNLOCK
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 2
    UNLOCK
  ENDIF
  =IIF('AL' $ gcCmpModules,gfCloseFile('PIKLINE') AND gfCloseFile('PIKTKT'),.T.)
  IF ORDHDR.TotCut > 0
    =IIF('PO' $ gcCmpModules,gfCloseFile('POSHDR')  AND gfCloseFile('POSLN'),.T.)
    =IIF('MF' $ gcCmpModules,gfCloseFile('CUTTKTH') AND gfCloseFile('CUTTKTL'),.T.)
    =gfCloseFile('CUTPICK')
  ENDIF
  
  *-- Update order open, Cancel and book quantity and amount and order status
  IF &lcOrdHdr..nSteps < 3
    SELECT ORDHDR
    =RLOCK()

    REPLACE STATUS     WITH IIF(Ship > 0,'C','X') ,;
            cCancReson WITH &lcOrdHdr..cCancReson ,;
            Cancelled  WITH gdSysDate ,;
            Cancel     WITH IIF(laData[23]='Y',Cancel ,Cancel+lnOpen),;
            CancelAmt  WITH IIF(laData[23]='Y',CancelAmt ,CancelAmt+lnOpenAmt),;
            Open       WITH 0,;
            OpenAmt    WITH 0,;
            TotCut     WITH 0,;
            Book       WITH IIF(laData[23]='Y',Ship + Cancel,lnBook)    ,;
            BookAmt    WITH IIF(laData[23]='Y',ShipAmt + CancelAmt,lnBookAmt) ,;
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
IF !llFromEDI
  SELECT unCmSess
  =SEEK('O'+PADR('SOORD',10)+gcUser_id+lcSession)
  REPLACE STATUS WITH 'C'
  UNLOCK
ENDIF    && End of IF !llFromEDI

llContinue = .F.

WAIT CLEAR  
SELECT ORDHDR
SCATTER FIELDS &lcScFields TO laData
=lfGetInfo()
=gfCloseFile('ORDCANLN')

STORE .T. TO laScrMode[1]
SHOW GETS
RETURN
*-- End of lfDelScr.

*!*************************************************************
*! Name      : lfGet_Info
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
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
*
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

*C102118,5 Always display main address in Bill to address (as A26) [Begin]
*  STORE 'At Store Level' TO lcBillName
*ELSE
*C102118,5 Always display main address in Bill to address (as A26) [End]

  lcBillName = Customer.BtName
  =gfGetAdr('Customer','','','',1,'2')
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcBillAdd&lcCount = lcBillAdd&lcCount + IIF(EMPTY(lcBillAdd&lcCount),'',',')+;
      SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
  ENDFOR
ENDIF
IF lnShipAddr = 2
  lcShipName = IIF(laScrMode[2],OrdHdr.StName,&lcOrdHdr..StName)
  lcShipAdd1 = IIF(laScrMode[2],OrdHdr.cAddress1,&lcOrdHdr..cAddress1)
  lcShipAdd2 = IIF(laScrMode[2],OrdHdr.cAddress2,&lcOrdHdr..cAddress2)
  lcShipAdd3 = IIF(laScrMode[2],OrdHdr.cAddress3,&lcOrdHdr..cAddress3)
  lcShipAdd4 = IIF(laScrMode[2],OrdHdr.cAddress4,&lcOrdHdr..cAddress4)
  lcShipAdd5 = IIF(laScrMode[2],OrdHdr.cAddress5,&lcOrdHdr..cAddress5)
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
*-- End of lfGet_Info.

*!*************************************************************
*! Name      : lfOrdQUpd
*! Developer : Timour A. K.
*! Date      : 05/30/1999
*! Purpose   : Update style orderd quantity in case of order
*!             line style was changed.
*!*************************************************************
*! Calls     : lfSavScr
*!*************************************************************
*! Parameters: lcFlToUpd-> Alias to update (STYLE or STYDYE).
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*
FUNCTION lfOrdQUpd
PARA lcFlToUpd

*--Update New Style.
SELECT (lcFlToUpd)
=RLOCK()
REPLACE Ord1   WITH Ord1   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty1),;
        Ord2   WITH Ord2   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty2),;
        Ord3   WITH Ord3   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty3),;
        Ord4   WITH Ord4   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty4),;
        Ord5   WITH Ord5   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty5),;
        Ord6   WITH Ord6   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty6),;
        Ord7   WITH Ord7   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty7),;
        Ord8   WITH Ord8   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty8),;
        TotOrd WITH TotOrd + IIF(OrdHdr.Status='B',0,&lcOrdLine..TotQty)
UNLOCK

*--Update Old Style.
=SEEK(OrdLine.Style+IIF(lcFlToUpd='STYLE','',laData[31]+SPACE(10)),lcFlToUpd)

=RLOCK()
REPLACE Ord1   WITH Ord1   - IIF(OrdHdr.Status='B',0,OrdLine.Qty1),;
        Ord2   WITH Ord2   - IIF(OrdHdr.Status='B',0,OrdLine.Qty2),;
        Ord3   WITH Ord3   - IIF(OrdHdr.Status='B',0,OrdLine.Qty3),;
        Ord4   WITH Ord4   - IIF(OrdHdr.Status='B',0,OrdLine.Qty4),;
        Ord5   WITH Ord5   - IIF(OrdHdr.Status='B',0,OrdLine.Qty5),;
        Ord6   WITH Ord6   - IIF(OrdHdr.Status='B',0,OrdLine.Qty6),;
        Ord7   WITH Ord7   - IIF(OrdHdr.Status='B',0,OrdLine.Qty7),;
        Ord8   WITH Ord8   - IIF(OrdHdr.Status='B',0,OrdLine.Qty8),;
        TotOrd WITH TotOrd - IIF(OrdHdr.Status='B',0,OrdLine.TotQty)
UNLOCK
RETURN
*-- End of lfOrdQUpd.

*!*************************************************************
*! Name      : lfStyPos
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1999
*! Purpose   : Function to check if there is style position for 
*!           : any order line to be sure it has been filled.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfStyPos()
*!*************************************************************
*
FUNCTION lfStyPos
PRIVATE lcCurAlias

*-- Save the current alias.
lcCurAlias = ALIAS()

*-- Flag to know if valid to save or not.
llSvBomVar = .T.

*-- Open the style positions file to Check if there is positions for the selected style.
=gfOpenFile(gcDataDir+'IcStyPos',gcDataDir+'IcStyPos','SH')

*-- Scan all the order lines to check if there is style position entered for the lines 
*-- that have to have records in the BomVar file.
SELECT (lcOrdLine)
SCAN FOR !DELETED()
  *-- Check if there is positions for the style in the current order line.
  IF SEEK(&lcOrdLine..Style , "IcStyPos")
    *-- Check if there is style position entered for the current line.
    IF SEEK("SO" + laData[1] + STR(&lcOrdLine..LineNo,6) , lcT_BomVar)
      *-- Check that at least one of the name drop or the design code has entered for 
      *-- all the style positions of the current order line.
      SELECT (lcT_BomVar)
      SCAN REST WHILE cidtype+ccost_id+STR(lineno,6) = "SO"+laData[1] + STR(&lcOrdLine..LineNo,6)
        IF EMPTY(&lcT_BomVar..cNDrpId) .AND. EMPTY(&lcT_BomVar..cDsgnCode)
          *** You cannot leave both name-drop and design code empty for style: {&lcOrdLine..Style}. ***
          *** <  OK  > ***
          =gfModalGen("TRM32071B00000" , "DIALOG" , &lcOrdLine..Style)
          llSvBomVar = .F.
        ENDIF
      ENDSCAN
      SELECT (lcOrdLine)
    ELSE
      *** You have to enter style position for Style: {&lcOrdLine..Style}. ***
      *** <  OK  > ***
      =gfModalGen("TRM32070B00000" , "DIALOG" , &lcOrdLine..Style)
      llSvBomVar = .F.
    ENDIF
  ENDIF
ENDSCAN

*-- Restore the previous alias.
SELECT (lcCurAlias)

*-- Return with the returned flag.
RETURN llSvBomVar
*-- End of lfStyPos.

*!*************************************************************
*! Name      : lfUpdtPoCT
*! Developer : WAB - WALID A. WAHAB
*! Date      : 09/26/1999
*! Purpose   : update selling price in po/ct lines
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcOrder	---> sales order no
*!             LcStyle  ---> Style COde
*!             lcLineNo ---> sales order line no
*!             lnTotQty ---> new qty
*!             lnPrice  ---> new price
*!             lnOldPrc ---> old price 
*!             lnOldQty ---> old qty
*!			   llRecDel ---> record is deleted (remove line)
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfUpdtPoCT()
*!*************************************************************
*
FUNCTION lfUpdtPoCT
PARAMETER  lcOrder,LcStyle,lcLineNo,lnQty,lnPrice,lnOldPrc,lnOldQty,llRecDel

=gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
=gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLN','SH')
=gfOpenFile(gcDataDir+'CUTTKTL',gcDataDir+'CUTTKTLS','SH')
SELECT CUTPICK
IF SEEK('2'+lcOrder+lcLineNo)
  *-- there are a po generated from curr. sales order
  SCAN REST WHILE TranCd+Order+Cordline = '2'+LcOrder+lcLineNo
    lnGenQty   = TotQty					&& generated qty
    lnGenPerc  = TotQty/lnOldQty  		&& genereted percent
    lnCtktNo   = CtktNo					&& PO number
    lnTotQty   = INT(lnQty*lnGenPerc)     && new generated qty 
    IF SEEK('P'+lnCtktNo+lcStyle,'POSLN')	
      *-- new selling price = --> curr selling price * total qty
      *--                     --> - oldprice * old qty generated 
      *--					  --> + newprice * new generated qty
      *-- 					  --> / total qty - old qty generated + new generated qty
      IF llRecDel
        lnNewPrc = ((POSLN.nSelPrice*POSLN.TotQty)-(lnOldPrc*lnGenQty));
                   /(POSLN.TotQty-lngenqty)
      ELSE
        lnNewPrc = ((POSLN.nSelPrice*POSLN.TotQty)-(lnOldPrc*lnGenQty)+;
                    (lnPrice*lnTotQty)) / (POSLN.TotQty-lngenqty+lnTotQty)
      ENDIF
      lnNewPrc = IIF(BETWEEN(lnNewPrc,-999999999.99,999999999.99),lnNewPrc,0)
      =SEEK(STYLE,'STYLE')
      lnTotCost= nEcost1+nEcost2+nEcost3+nEcost4+nEcost5
      lnRotSub    = IIF(llStyMark,lnTotCost,lnNewPrc)
      lnGrosMrgn = IIF(lnRotSub=0,0,((lnNewPrc - lnTotCost)/lnRotSub)*100)
      REPLACE POSLN.nSelPrice  WITH lnNewPrc ,;
              POSLN.nGrosMrgn  WITH lnGrosMrgn
    ENDIF
  ENDSCAN  
ENDIF
SELECT CUTPICK
IF SEEK('1'+lcOrder+lcLineNo)
  *-- there are a CT generated from curr. sales order
  SCAN REST WHILE TranCd+Order+Cordline = '1'+LcOrder+lcLineNo
    lnGenQty   = TotQty					&& generated qty
    lnGenPerc  = TotQty/lnOldQty  		&& genereted percentg
    lnCtktNo   = CtktNo					&& PO number
    lnTotQty   = INT(lnQty*lnGenPerc)   && new generated qty 
    IF SEEK(lcStyle+lnCtktNo+'1','CUTTKTL')
      *-- new selling price = --> curr selling price * total qty
      *--                     --> - oldprice * old qty generated 
      *--					  --> + newprice * new generated qty
      *-- 					  --> / total qty - old qty generated + new generated qty
      IF llRecDel
        lnNewPrc = ((CUTTKTL.nSelPrice*CUTTKTL.TotQty)-(lnOldPrc*lnGenQty));
                   /(CUTTKTL.TotQty-lngenqty)
      ELSE
        lnNewPrc = ((CUTTKTL.nSelPrice*CUTTKTL.TotQty)-(lnOldPrc*lnGenQty)+;
                   (lnPrice*lnTotQty)) / (CUTTKTL.TotQty-lngenqty+lnTotQty)
      ENDIF
      lnNewPrc = IIF(BETWEEN(lnNewPrc,-999999999.99,999999999.99),lnNewPrc,0)
      =SEEK(STYLE,'STYLE')
      lnRotSub   = IIF(llStyMark,Style.TotCost,lnNewPrc)
      lnGrosMrgn = IIF(lnRotSub=0,0,((lnNewPrc - Style.TotCost)/lnRotSub)*100)
      REPLACE CUTTKTL.nSelPrice  WITH lnNewPrc ,;
              CUTTKTL.nGrosMrgn  WITH lnGrosMrgn
    ENDIF
  ENDSCAN
ENDIF
*-- End of lfUpdtPoCT.

*!*******************************************************************************
*! Name      : lpUpDatSoc
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : To update Soccer Master File.
*!*******************************************************************************
*! Reference : C102118,1
*!*******************************************************************************
*! Example   : DO lpUpDatSoc
*!*******************************************************************************
*
PROCEDURE lpUpDatSoc
PRIVATE lcDelete, lcCurStore

SELECT (lcOrdLine)
SET ORDER TO TAG OrdLinSt
LOCATE
lcCurStore = Store
lcDelete = SET('DELETE')
SET DELETE OFF

*SCAN REST WHILE cOrdType+Order = lcOrdType + laData[1]
SCAN
  SELECT (lcSocTemp)
  IF !SEEK(&lcOrdLine..Store)
    APPEND BLANK
    REPLACE Div   WITH SUBSTR(&lcOrdLine..Store,1,4) ,;
            Team  WITH SUBSTR(&lcOrdLine..Store,5,8)
  ENDIF

  REPLACE cOrdType WITH lcOrdType, Order WITH laData[1], ;
          OStation WITH IIF(SEEK(lcOrdType+laData[1],'Soccer'),Soccer.OStation,'N') ,;
          TStation WITH IIF(laScrMode[3],&lcSocTemp..TStation ,'K')
  
  *Commented out
  *IF DELETED(lcOrdLine) AND &lcOrdLine..cStyGroup = 'JY'
  *  PRIVATE lcOldJersy
  *  lcOldJersy = &lcOrdLine..Style
  *  SELECT (lcSocTemp)  
  *  
  *  *REPLACE YouthPkg WITH 0, AdultPkg WITH 0
  *  IF &lcOrdLine..Book1 + &lcOrdLine..Book2 + &lcOrdLine..Book3 > 0
  *    REPLACE YouthPkg WITH 0
  *  ENDIF  
  *  IF &lcOrdLine..Book4 + &lcOrdLine..Book5 + &lcOrdLine..Book6 + &lcOrdLine..Book7 + &lcOrdLine..Book8> 0
  *    REPLACE AdultPkg WITH 0
  *  ENDIF  
  *  IF !EOF(lcOrdLine)
  *    SKIP 1
  *    IF DELETED(lcOrdLine) AND &lcOrdLine..cStyGroup = 'JY' AND &lcOrdLine..Style == lcOldJersy
  *      IF &lcOrdLine..Book4 > 0 OR &lcOrdLine..Book5 > 0 OR &lcOrdLine..Book6 > 0 ; 
  *        OR &lcOrdLine..Book7 > 0 OR &lcOrdLine..Book8 > 0
  *        REPLACE AdultPkg WITH 0
  *      ENDIF
  *    ENDIF
  *    SKIP -1
  *  ENDIF
  *ENDIF  
  *IF !(Store == lcCurStore)
  *  lcCurStore = Store
  *ENDIF
ENDSCAN
  
SELECT (lcSocTemp)
SCAN
  SCATTER MEMVAR MEMO
  SELECT Soccer
  IF !SEEK(lcOrdType + &lcSocTemp..Order + &lcSocTemp..Div + &lcSocTemp..Team)
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDIF  
  REPLACE YouthPkg WITH m.YouthPkg ,;
          AdultPkg WITH m.AdultPkg ,;
          OStation WITH IIF(laScrMode[3],m.OStation,'N') ,;
          TStation WITH IIF(laScrMode[3],m.TStation,'K')
  REPLACE Div       WITH m.Div      ,;
          Team      WITH m.Team     ,;
          Sponsor   WITH m.Sponsor ,;
          InkColor  WITH m.InkColor ,;
          SpName    WITH m.SpName   ,;
          PrintInst WITH m.PrintInst ,;
          TeamsNo   WITH m.TeamsNo   ,;
          InkColorD WITH m.InkColorD ,;
          LogoName  WITH m.LogoName ,;
          Logo      WITH m.Logo     ,;
          SpInst    WITH m.SpInst

ENDSCAN
SET DELETE &lcDelete
*-- End of lpUpDatSoc.

*!*******************************************************************************
*! Name      : lpAdjLines
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Valid function for Proceed button in detail screen.
*!*******************************************************************************
*! Reference : C102118,1
*!*******************************************************************************
*! Example   : DO lpAdjLines
*!*******************************************************************************
*
PROCEDURE lpAdjLines
PRIVATE lcSetDelet , llDeleted , lcAlias , lcCurrTag , lnOrdLinNo , lnOrderLns , ;
        lcOrdLin3 , lcOrdLin4 , lcOrdLinTg , lnI , lcI

lcAlias = ALIAS()

*Added Recently
*DO lpProceed
*Added Recently

lcSetDelet = SET('DELETE')        && Save Delete Settings
SET DELETE OFF

*Added Recently
DO lpProceed
*Added Recently

IF RECCOUNT(lcOrdCanLn) > 0
  IF USED(lcOrdCanLn)
    USE IN (lcOrdCanLn)
  ENDIF
  =gfOpenFile(gcWorkDir+lcOrdCanLn,gcWorkDir+lcOrdCanLn, 'EX')
  SELECT (lcOrdCanLn)
  SCAN FOR TotQty =0
    DELETE
  ENDSCAN
  PACK
ENDIF

*-- If Edit mode.
*-- we need to get the last order line number
IF laScrMode[3] AND SEEK(lcOrdType+laData[1],(lcOrdLine))
  SELECT (lcOrdLine)
  
  lcOrdLin3 = gfTempName()
  lcOrdLin4 = gfTempName()
  COPY STRUCTURE TO (gcWorkDir+lcOrdLin3) WITH CDX
  COPY STRUCTURE TO (gcWorkDir+lcOrdLin4) WITH CDX
  =gfOpenFile(gcWorkDir+lcOrdLin3,gcWorkDir+'OrdLine', 'EX')
  =gfOpenFile(gcWorkDir+lcOrdLin4,gcWorkDir+'OrdLine', 'EX')

  SELECT (lcOrdLine)  
  SET ORDER TO TAG OrdLine DESCENDING
  LOCATE
  lnOrderLns = LineNo + 1
  SET ORDER TO TAG OrdLine ASCENDING
  LOCATE
  LOCATE FOR lRemoveLin
  
  IF FOUND()
    SELECT OrdLine
    lcOrdLinTg = TAG()
    SET ORDER TO TAG OrdLine 
    
    SELECT (lcOrdLine)
    SCAN FOR lRemoveLin
      IF SEEK(&lcOrdLine..cOrdType + &lcOrdLine..Order + STR(&lcOrdLine..LineNo,6),'OrdLine')
        SELECT OrdLine
        SCATTER MEMVAR MEMO
        m.cStyGroup = Style.cStyGroup
        m.Flag = "M"
        STORE 0 TO m.AdultPr , m.YouthPr , m.Adult , m.Youth
        FOR lnI = 1 TO 8
          lcI = ALLTRIM(STR(lnI,1))
          m.Size&lcI = OrdLine.Qty&lcI
        ENDFOR
        m.TotSize = OrdLine.TotQty
        INSERT INTO (lcOrdLin3) FROM MEMVAR
        
        SELECT (lcOrdLine)
        REPLACE LineNo WITH lnOrderLns
        lnOrderLns = lnOrderLns + 1
      ENDIF
    ENDSCAN

    IF RECCOUNT(lcOrdLin3) > 0
      SELECT (lcOrdLin3)
      SCAN 
        SCATTER MEMVAR MEMO
        SELECT (lcOrdLine)
        APPEND BLANK
        GATHER MEMVAR MEMO
        DELETE
      ENDSCAN
      
      SELECT (lcOrdLin3)
      ZAP
    ENDIF       

    SELECT OrdLine
    SET ORDER TO TAG (lcOrdLinTg)  
  ENDIF

ENDIF
*-- Endif of Edit mode.

SELECT (lcOrdLine)
LOCATE
lcCurrTag  = TAG()

*-- If Edit mode.
IF laScrMode[3]
  SCAN FOR cStyGroup = 'JY'
    llDeleted = IIF(DELETED(),.T.,.F.)
    SCATTER MEMVAR MEMO
    *-- Check if this line is changed to Jersey , we need to add them at end of order 
    *-- line number so as the 2 lines will not overlap any existing lines in Ordline file
    *-- so if the lines number is found and the style not equal old style , this means that
    *-- that style is changed in edit mode
    IF SEEK(lcOrdType+laData[1]+STR(LineNo,6),'OrdLine') AND Flag = "M" AND Style <> OrdLine.Style AND Style.cStyGroup <> "JY"
      IF !llDeleted
        m.LineNo = lnOrderLns
        INSERT INTO (lcOrdLin4) FROM MEMVAR
        lnOrderLns = lnOrderLns + 2
      ENDIF  
      REPLACE Account WITH 'zzzzz'
      
      SELECT OrdLine
      SCATTER MEMVAR MEMO
      INSERT INTO (lcOrdLin3) FROM MEMVAR
      SELECT (lcOrdLin3)
      REPLACE FLAG WITH "M"
      DELETE
      LOOP
    ENDIF
    
    INSERT INTO (lcOrdLin2) FROM MEMVAR
    REPLACE Qty4 WITH 0, Qty5 WITH 0, Qty6 WITH 0, Qty7 WITH 0, Qty8 WITH 0, ;
            TotQty WITH m.Youth, Price WITH m.YouthPr, Gros_Price WITH m.YouthPr;
            Book4 WITH 0, Book5 WITH 0, Book6 WITH 0, Book7 WITH 0, Book8 WITH 0, ;
            TotBook WITH m.Book1 + m.Book2 + m.Book3, ;
            Pik4 WITH 0, Pik5 WITH 0, Pik6 WITH 0, Pik7 WITH 0, Pik8 WITH 0, ;
            TotPik WITH m.Pik1 + m.Pik2 + m.Pik3, ;
            Cut4 WITH 0, Cut5 WITH 0, Cut6 WITH 0, Cut7 WITH 0, Cut8 WITH 0, ;
            TotCut WITH m.Cut1 + m.Cut2 + m.Cut3, ;
            PoAlo4 WITH 0, PoAlo5 WITH 0, PoAlo6 WITH 0, PoAlo7 WITH 0, PoAlo8 WITH 0, ;
            Tot_PoAlo WITH m.PoAlo1 + m.PoAlo2 + m.PoAlo3, ;
            nPck4 WITH 0, nPck5 WITH 0, nPck6 WITH 0, nPck7 WITH 0, nPck8 WITH 0
    IF SEEK(&lcOrdLine..cOrdType+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6),(lcOrdLin2))
      SELECT (lcOrdLin2)
      REPLACE Qty1 WITH 0, Qty2 WITH 0, Qty3 WITH 0, TotQty WITH m.Adult, ;
              Price WITH m.AdultPr, Gros_Price WITH m.AdultPr, LineNo WITH m.LineNo + 1, ;
              Book1 WITH 0, Book2 WITH 0, Book3 WITH 0, ;
              TotBook WITH m.Book4 + m.Book5 + m.Book6 + m.Book7 + m.Book8, ;
              Pik1 WITH 0, Pik2 WITH 0, Pik3 WITH 0, ;
              TotPik WITH m.Pik4 + m.Pik5 + m.Pik6 + m.Pik7 + m.Pik8, ;
              Cut1 WITH 0, Cut2 WITH 0, Cut3 WITH 0, ;
              TotCut WITH m.Cut4 + m.Cut5 + m.Cut6 + m.Cut7 + m.Cut8, ;
              PoAlo1 WITH 0, PoAlo2 WITH 0, PoAlo3 WITH 0, ;
              Tot_PoAlo WITH m.PoAlo4 + m.PoAlo5 + m.PoAlo6 + m.PoAlo7 + m.PoAlo8, ;
              nPck1 WITH 0, nPck2 WITH 0, nPck3 WITH 0

      IF llDeleted
        DELETE
      ENDIF
    ENDIF    
  ENDSCAN

  SELECT (lcOrdLin4)
  LOCATE
  IF !EOF()
    SCAN
      llDeleted = IIF(DELETED(),.T.,.F.)
      SCATTER MEMVAR MEMO
      INSERT INTO (lcOrdLin3) FROM MEMVAR
      REPLACE Qty4 WITH 0, Qty5 WITH 0, Qty6 WITH 0, Qty7 WITH 0, Qty8 WITH 0, ;
              TotQty WITH m.Youth, Price WITH m.YouthPr, Gros_Price WITH m.YouthPr;
              Book4 WITH 0, Book5 WITH 0, Book6 WITH 0, Book7 WITH 0, Book8 WITH 0, ;
              TotBook WITH m.Book1 + m.Book2 + m.Book3, ;
              Pik4 WITH 0, Pik5 WITH 0, Pik6 WITH 0, Pik7 WITH 0, Pik8 WITH 0, ;
              TotPik WITH m.Pik1 + m.Pik2 + m.Pik3, ;
              Cut4 WITH 0, Cut5 WITH 0, Cut6 WITH 0, Cut7 WITH 0, Cut8 WITH 0, ;
              TotCut WITH m.Cut1 + m.Cut2 + m.Cut3, ;
              PoAlo4 WITH 0, PoAlo5 WITH 0, PoAlo6 WITH 0, PoAlo7 WITH 0, PoAlo8 WITH 0, ;
              Tot_PoAlo WITH m.PoAlo1 + m.PoAlo2 + m.PoAlo3, ;
              nPck4 WITH 0, nPck5 WITH 0, nPck6 WITH 0, nPck7 WITH 0, nPck8 WITH 0
      IF SEEK(&lcOrdLin4..cOrdType+&lcOrdLin4..Order+STR(&lcOrdLin4..LineNo,6),(lcOrdLin3))
        SELECT (lcOrdLin3)
        REPLACE Qty1 WITH 0, Qty2 WITH 0, Qty3 WITH 0, TotQty WITH m.Adult, ;
                Price WITH m.AdultPr, Gros_Price WITH m.AdultPr, LineNo WITH m.LineNo + 1, ;
                Book1 WITH 0, Book2 WITH 0, Book3 WITH 0, ;
                TotBook WITH m.Book4 + m.Book5 + m.Book6 + m.Book7 + m.Book8, ;
                Pik1 WITH 0, Pik2 WITH 0, Pik3 WITH 0, ;
                TotPik WITH m.Pik4 + m.Pik5 + m.Pik6 + m.Pik7 + m.Pik8, ;
                Cut1 WITH 0, Cut2 WITH 0, Cut3 WITH 0, ;
                TotCut WITH m.Cut4 + m.Cut5 + m.Cut6 + m.Cut7 + m.Cut8, ;
                PoAlo1 WITH 0, PoAlo2 WITH 0, PoAlo3 WITH 0, ;
                Tot_PoAlo WITH m.PoAlo4 + m.PoAlo5 + m.PoAlo6 + m.PoAlo7 + m.PoAlo8, ;
                nPck1 WITH 0, nPck2 WITH 0, nPck3 WITH 0

        IF llDeleted
          DELETE
        ENDIF
      ENDIF      
    ENDSCAN

    SELECT (lcOrdLin4)
    LOCATE
    SCAN  
      SCATTER MEMVAR MEMO
      INSERT INTO (lcOrdLin3) FROM MEMVAR
      IF DELETED()
        SELECT (lcOrdLin3)
        DELETE
      ENDIF
    ENDSCAN

  ENDIF

ELSE    && Add mode
  SCAN FOR cStyGroup = 'JY'
    llDeleted = IIF(DELETED(),.T.,.F.)
    SCATTER MEMVAR MEMO
    INSERT INTO (lcOrdLin2) FROM MEMVAR
    REPLACE Qty4 WITH 0, Qty5 WITH 0, Qty6 WITH 0, Qty7 WITH 0, Qty8 WITH 0, ;
            TotQty WITH m.Youth, Price WITH m.YouthPr, Gros_Price WITH m.YouthPr;
            Book4 WITH 0, Book5 WITH 0, Book6 WITH 0, Book7 WITH 0, Book8 WITH 0, ;
            TotBook WITH m.Book1 + m.Book2 + m.Book3, ;
            Pik4 WITH 0, Pik5 WITH 0, Pik6 WITH 0, Pik7 WITH 0, Pik8 WITH 0, ;
            TotPik WITH m.Pik1 + m.Pik2 + m.Pik3, ;
            Cut4 WITH 0, Cut5 WITH 0, Cut6 WITH 0, Cut7 WITH 0, Cut8 WITH 0, ;
            TotCut WITH m.Cut1 + m.Cut2 + m.Cut3, ;
            PoAlo4 WITH 0, PoAlo5 WITH 0, PoAlo6 WITH 0, PoAlo7 WITH 0, PoAlo8 WITH 0, ;
            Tot_PoAlo WITH m.PoAlo1 + m.PoAlo2 + m.PoAlo3, ;
            nPck4 WITH 0, nPck5 WITH 0, nPck6 WITH 0, nPck7 WITH 0, nPck8 WITH 0
    IF SEEK(&lcOrdLine..cOrdType+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6),(lcOrdLin2))
      SELECT (lcOrdLin2)
      REPLACE Qty1 WITH 0, Qty2 WITH 0, Qty3 WITH 0, TotQty WITH m.Adult, ;
              Price WITH m.AdultPr, Gros_Price WITH m.AdultPr, LineNo WITH m.LineNo + 1, ;
              Book1 WITH 0, Book2 WITH 0, Book3 WITH 0, ;
              TotBook WITH m.Book4 + m.Book5 + m.Book6 + m.Book7 + m.Book8, ;
              Pik1 WITH 0, Pik2 WITH 0, Pik3 WITH 0, ;
              TotPik WITH m.Pik4 + m.Pik5 + m.Pik6 + m.Pik7 + m.Pik8, ;
              Cut1 WITH 0, Cut2 WITH 0, Cut3 WITH 0, ;
              TotCut WITH m.Cut4 + m.Cut5 + m.Cut6 + m.Cut7 + m.Cut8, ;
              PoAlo1 WITH 0, PoAlo2 WITH 0, PoAlo3 WITH 0, ;
              Tot_PoAlo WITH m.PoAlo4 + m.PoAlo5 + m.PoAlo6 + m.PoAlo7 + m.PoAlo8, ;
              nPck1 WITH 0, nPck2 WITH 0, nPck3 WITH 0

      IF llDeleted
        DELETE
      ENDIF
    ENDIF  
  ENDSCAN
ENDIF  
*-- Endif of Edit mode.

IF RECCOUNT(lcOrdLine) > 0
  SELECT (lcOrdLine)
  LOCATE
  SCAN  
    IF Account <> 'zzzzz'
      SCATTER MEMVAR MEMO
      INSERT INTO (lcOrdLin2) FROM MEMVAR
      IF DELETED()
        SELECT (lcOrdLin2)
        DELETE
      ENDIF
    ENDIF  
  ENDSCAN

  IF laScrMode[3]
    SELECT (lcOrdLin3)
    LOCATE
    SCAN  
      SCATTER MEMVAR MEMO
      INSERT INTO (lcOrdLin2) FROM MEMVAR
      IF DELETED()
        SELECT (lcOrdLin2)
        DELETE
      ENDIF
    ENDSCAN
  ENDIF  

  SELECT (lcOrdLine)
  ZAP
ENDIF

SELECT (lcOrdLin2)
LOCATE
SET ORDER TO TAG OrdLine
lnOrdLinNo = 0

SCAN
  SCATTER MEMVAR MEMO
  *lnOrdLinNo = lnOrdLinNo + 1  
  *m.LineNo = lnOrdLinNo    
  IF m.cStyGroup = "JY"
    INSERT INTO (lcOrdLine) FROM MEMVAR
    IF m.TotQty = 0 OR DELETED()
      SELECT (lcOrdLine)
      DELETE        
    ENDIF
  ELSE
    INSERT INTO (lcOrdLine) FROM MEMVAR
    IF DELETED()
      SELECT (lcOrdLine)
      DELETE
    ENDIF
  ENDIF  
ENDSCAN

IF laScrMode[3]
  IF USED(lcOrdLin3)
    USE IN (lcOrdLin3)
    ERASE (gcWorkDir+lcOrdLin3+".DBF")
    ERASE (gcWorkDir+lcOrdLin3+".CDX")
    ERASE (gcWorkDir+lcOrdLin3+".FPT")     
  ENDIF
  
  IF USED(lcOrdLin4)
    USE IN (lcOrdLin4)
    ERASE (gcWorkDir+lcOrdLin4+".DBF")
    ERASE (gcWorkDir+lcOrdLin4+".CDX")
    ERASE (gcWorkDir+lcOrdLin4+".FPT")         
  ENDIF
ENDIF

SELECT (lcOrdLine)
LOCATE
SET DELETE &lcSetDelet
=lfBrowDet(.T.,'O')  
SELECT (lcAlias)
*-- End of lpAdjLines.

*!*******************************************************************************
*! Name      : lpProceed
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Valid function for Proceed button in detail screen.
*!*******************************************************************************
*! Reference : C102118,1
*!*******************************************************************************
*! Example   : DO lpProceed
*!*******************************************************************************
*
PROCEDURE lpProceed
PRIVATE lcOldStore, llJersyAdd 
llJersyAdd = .F.

lcOldStore = ''
SELECT (lcOrdLine)
LOCATE

*Added Recently 
*SCAN
*  SELECT (lcSocTemp)
*  IF &lcOrdLine..Store <> lcOldDiv+lcOldTeam
*    IF SEEK(&lcOrdLine..Store)
*      REPLACE YouthPkg WITH &lcOrdLine..Youth , AdultPkg WITH &lcOrdLine..Adult
*      IF DELETED(lcOrdLine)
*        REPLACE YouthPkg WITH 0 , AdultPkg WITH 0
*      ENDIF
*    ENDIF
*    lcOldDiv  = SUBSTR(&lcOrdLine..Store,1,4)
*    lcOldTeam = SUBSTR(&lcOrdLine..Store,5,8)
*  ELSE
*    IF SEEK(&lcOrdLine..Store) AND (&lcOrdLine..Youth <> 0 OR &lcOrdLine..Adult <> 0)
*      REPLACE YouthPkg WITH &lcOrdLine..Youth , AdultPkg WITH &lcOrdLine..Adult
*    ENDIF
*  ENDIF
*ENDSCAN

IF laScrMode[3]
  SCAN FOR cStyGroup = "JY"
    SELECT (lcSocTemp)
    IF !(&lcOrdLine..Store == lcOldStore)
      IF SEEK(&lcOrdLine..Store)
        REPLACE YouthPkg WITH &lcOrdLine..Youth , AdultPkg WITH &lcOrdLine..Adult
        IF DELETED(lcOrdLine)
          REPLACE YouthPkg WITH 0 , AdultPkg WITH 0
        ENDIF
      ENDIF
      lcOldStore = &lcOrdLine..Store
    ENDIF
  ENDSCAN
ENDIF

lcOldStore = ""
IF laScrMode[4]
  SCAN FOR cStyGroup = "JY" AND !DELETED()
    SELECT (lcSocTemp)
    IF !(&lcOrdLine..Store == lcOldStore)
      IF SEEK(&lcOrdLine..Store)
        REPLACE YouthPkg WITH &lcOrdLine..Youth , AdultPkg WITH &lcOrdLine..Adult
      ENDIF
      lcOldStore = &lcOrdLine..Store
    ENDIF
  ENDSCAN
ENDIF  
*Added Recently 

*-- End of lpProceed.

