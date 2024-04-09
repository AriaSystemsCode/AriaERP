*:***************************************************************************
*: Program file  : MAMFHDR.PRG
*: Program desc. : Material Manufacturing Orders Report
*: Date          : 07/15/1999
*: System        : Aria Advantage Series.
*: Module        : MATERIAL MANUFACTURING ORDER FORM (MF)
*: Developer     : BASSEM RAFAAT (BWA)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : 
*:***************************************************************************
*: Notes         : Enh. # 301236
*:***************************************************************************
*: Example       : DO MAMFHDR
*:***************************************************************************


*--  Section Declare variables
llMultiWare  = ALLTRIM(UPPER(gfGetMemVar('M_WareHouse'))) = 'Y'
llUseDyelots = ALLTRIM(UPPER(gfGetMemVar('M_DYELOT')))    = 'Y'
lcTempCurs   = gfTempName()
llGlLink     = ALLTRIM(UPPER(gfGetMemVar('M_Link_GL')))   = 'Y'
lcTSLbl1     = ALLTRIM(UPPER(gfGetMemVar('M_cTSLbl1'))) 
lcTSLbl2     = ALLTRIM(UPPER(gfGetMemVar('M_cTSLbl2'))) 
lcTSLbl3     = ALLTRIM(UPPER(gfGetMemVar('M_cTSLbl3'))) 
lcTSLbl4     = ALLTRIM(UPPER(gfGetMemVar('M_cTSLbl4'))) 
lcOptTitle   = lcOpTtl                                              && Variable hold the Optional Title
lcShowCost   = llRpCoDe                                             && Variable hold the Costing Details
lcSortOpt    = lcRpStOrd                                            && Variable hold the Choice of the Sort orders by
lcLinesOpt   = lcRpOrdLn                                            && Variable hold the Choice of the Sort orders <Lines> by
llIncLines  = lcRpOrLn = 'Y'
STORE '' TO lcOldVal,  lcStatStr , lcHdrStr, lcMainGrp, lcSubTotStr ,lcMFGOrdNo , lcStrng , lcdescrep , lcdesLines
STORE 0.000 TO lnBudget,  lnReceived,lnDamaged, lnCanceled, lnOpen,;
               lnTotBud , lnTotRec , lnTotDam,  lnTotCan,   lnTotOpen,;
               lnSubEst1, lnSubEst2, lnSubEst3, lnSubEst4,;
               lnSubLan1, lnSubLan2, lnSubLan3, lnSubLan4,; 
               lnSubAct1, lnSubAct2, lnSubAct3, lnSubAct4,;
               lnTotEst1, lnTotEst2, lnTotEst3, lnTotEst4,;
               lnTotLan1, lnTotLan2, lnTotLan3, lnTotLan4,; 
               lnTotAct1, lnTotAct2, lnTotAct3, lnTotAct4,;
               lnSubEst , lnSubLan , lnSubAct,;
               lnTotEst , lnTotLan , lnTotAct
               
*--End Section Declare variables

*--Section to get to the variable  <llIncLines> its value 

*--End

STORE lcShowCost = 'Y' TO llShowCost, llHdrCost   && if lcShowCost = 'Y' 
*llNtSumRep       = lcSumRep   = 'N'
llNtSumRep       = llRPSumR   = 'N'
*llPrntNote       = lcPrntNote = 'Y' 
llPrntNote       = llRpNote = 'Y' 
IF lfGetData()
  lcLinesStr = IIF(!llHdrRep .AND. llNtSumRep,;
                   IIF(llRecvLog,[],;
                   [IIF(TranCd $ '1234', lcdesLines, SPACE(13)) +])+;
                   [DTOC(drecvdate) + ' ' + ]+;
                   [cFabric + ' ' + Color + ' ' + IIF(llUseDyelots, Dyelot +  ' ', '') + pattern + ' ' + IIF(llMultiWare, cRecWare  + SPACE(4),'') +  STR(NMFGTOTQTY,11,3)],[''])

  lcStrng   =      [IIF(llPrntNote .AND.]+;
                   [SEEK('O'+cMFGOrdNo, 'NOTEPAD') .AND.]+;
                   [!EMPTY(NOTEPAD.mNotes),]+; 
                   ['Notes : ' + CHR(13) + ALLTRIM(NOTEPAD.mNotes) + ]+;
                   [IIF(! CHR(13) $ RIGHT(NOTEPAD.mNotes,2), CHR(13),'') , '')] +;
                 IIF(llHdrRep .AND. llIncLines .AND. llNtSumRep,;
                   [ + SPACE(29) + IIF(TranCd $ '1234', lcdesLines, SPACE(13)) +]+;
                   [DTOC(drecvdate) + ' ' + ]+ ;
                   [cFabric + ' ' + Color + ' ' + IIF(llUseDyelots, Dyelot +  ' ', '') + pattern + ' ' + IIF(llMultiWare, cRecWare  + SPACE(4),'') +  STR(NMFGTOTQTY,11,3)], '')

  R_TITLE = 'MATERIAL MANUFACTURING ORDERS REPORT'
  R_WIDTH = 'W'
  PageNo  = 0
  lcHdrTtl     = 'Comp% Item         Budget    Received     Damaged   Cancelled        Open ' + ;
                 IIF(llMultiWare, 'Warehouse', '') 
*--BAS                 
  *lcLineTtl    = IIF(llRecvLog, '', SPACE(13)) + 'Received Item    Color  ' + IIF(llUseDyelots, 'Dyelot     ', '') + ;
                  'Pattern    ' + ;
                 IIF(llMultiWare, 'Warehouse    Quantity', '   Quantity') 
                 
  lcLineTtl    = IIF(llRecvLog, '', '') + 'Received Item    Color  ' + IIF(llUseDyelots, 'Dyelot     ', '') + ;
                  'Pattern    ' + ;
                 IIF(llMultiWare, 'Warehouse    Quantity', '   Quantity') 
*--BAS                 

  llSubTotal   = !EMPTY(lcSubTotStr)
  llSubCost    = llSubTotal .AND. llShowCost
  IF llHdrRep .OR. lcRPline = 'A'
    STORE .T. TO llTotBud, llTotRec, llTotDam, llTotCan, llTotOpen
  ELSE
    llTotBud    = '1' $ lcRPline
    llTotRec    = '2' $ lcRPline
    llTotDam    = '3' $ lcRPline
    llTotCan    = '4' $ lcRPline
    llTotOpen   = llTotBud .AND. llTotRec .AND. llTotDam .AND. llTotCan
  ENDIF  
  lnSetDecimals = SET('DECIMAL')
  lcSetFixed    = SET('FIXED')
  SET DECIMALS TO 3
  SET FIXED       ON
  DO gfDispRe WITH EVAL('lcRpForm')
  SET DECIMALS TO lnSetDecimals
  SET FIXED       &lcSetFixed
ENDIF
  
*!*************************************************************
*! Name      : lfSetCst
*! Developer : BASSEM RAFAAT
*! Date      : 07/15/1999
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfSetCst()
*!*************************************************************
FUNCTION lfSetCst
IF llHdrRep 
  IF llIncLines
    IF lcMFGOrdNo <> cMFGOrdNo
      lcMFGOrdNo = cMFGOrdNo
*      llPrntNote = lcPrntNote = 'Y'
      llPrntNote = llRpNote = 'Y'
      IF lcShowCost = 'Y'
        llHdrCost  = .T.
        CALCULATE NEXT 1 SUM(lnBudget + nmmgBudget), SUM(lnReceived + Received),;
                         SUM(lnDamaged + Damaged), SUM(lnCanceled + Canceled),;
                         SUM(lnOpen   + nmmfgOpen),;
                         SUM(lnSubEst1 + nEst_Cost1),;
                         SUM(lnSubEst2 + nEst_Cost2),;
                         SUM(lnSubEst3 + nEst_Cost3),;
                         SUM(lnSubEst4 + nEst_Cost4),;
                         SUM(lnSubLan1 + nLan_Cost1),;
                         SUM(lnSubLan2 + nLan_Cost2),;
                         SUM(lnSubLan3 + nLan_Cost3),;
                         SUM(lnSubLan4 + nLan_Cost4),;                 
                         SUM(lnSubAct1 + nAct_Cost1),;                 
                         SUM(lnSubAct2 + nAct_Cost2),;                 
                         SUM(lnSubAct3 + nAct_Cost3),;                 
                         SUM(lnSubAct4 + nAct_Cost4);                                  
                      TO lnBudget, lnReceived, lnDamaged, lnCanceled, lnOpen,;
                         lnSubEst1,;
                         lnSubEst2,;
                         lnSubEst3,;
                         lnSubEst4,;
                         lnSubLan1,;
                         lnSubLan2,;
                         lnSubLan3,;
                         lnSubLan4,;
                         lnSubAct1,;
                         lnSubAct2,;
                         lnSubAct3,;
                         lnSubAct4
      ELSE
        CALCULATE NEXT 1 SUM(lnBudget + nmmgBudget), SUM(lnReceived + Received),;
                         SUM(lnDamaged + Damaged), SUM(lnCanceled + Canceled),;
                         SUM(lnOpen   + nmmfgOpen);
                     TO lnBudget, lnReceived, lnDamaged, lnCanceled, lnOpen
      ENDIF                    
    ENDIF  
  ELSE
*    llPrntNote = lcPrntNote = 'Y'
    llPrntNote = llRpNote = 'Y'
    IF lcShowCost = 'Y'
        llHdrCost  = .T.
        CALCULATE NEXT 1 SUM(lnBudget + nmmgBudget), SUM(lnReceived + Received),;
                         SUM(lnDamaged + Damaged), SUM(lnCanceled + Canceled),;
                         SUM(lnOpen   + nmmfgOpen),;
                         SUM(lnSubEst1 + nEst_Cost1),;
                         SUM(lnSubEst2 + nEst_Cost2),;
                         SUM(lnSubEst3 + nEst_Cost3),;
                         SUM(lnSubEst4 + nEst_Cost4),;
                         SUM(lnSubLan1 + nLan_Cost1),;
                         SUM(lnSubLan2 + nLan_Cost2),;
                         SUM(lnSubLan3 + nLan_Cost3),;
                         SUM(lnSubLan4 + nLan_Cost4),;                 
                         SUM(lnSubAct1 + nAct_Cost1),;                 
                         SUM(lnSubAct2 + nAct_Cost2),;                 
                         SUM(lnSubAct3 + nAct_Cost3),;                 
                         SUM(lnSubAct4 + nAct_Cost4);                                  
                      TO lnBudget, lnReceived, lnDamaged, lnCanceled, lnOpen,;
                         lnSubEst1,;
                         lnSubEst2,;
                         lnSubEst3,;
                         lnSubEst4,;
                         lnSubLan1,;
                         lnSubLan2,;
                         lnSubLan3,;
                         lnSubLan4,;
                         lnSubAct1,;
                         lnSubAct2,;
                         lnSubAct3,;
                         lnSubAct4
      ELSE
        CALCULATE NEXT 1 SUM(lnBudget + nmmgBudget), SUM(lnReceived + Received),;
                         SUM(lnDamaged + Damaged), SUM(lnCanceled + Canceled),;
                         SUM(lnOpen   + nmmfgOpen);
                     TO lnBudget, lnReceived, lnDamaged, lnCanceled, lnOpen
      ENDIF                    
  
  ENDIF
ELSE
*  llPrntNote = lcPrntNote = 'Y'
  llPrntNote = llRpNote = 'Y'
  IF lcShowCost = 'Y'
*    llHdrCost  = lcSumRep = 'N'
    llHdrCost  = llRPSumR = 'N'
    
    CALCULATE NEXT 1 SUM(lnBudget + IIF(TranCd = '1', NMFGTOTQTY, 0)),;
                     SUM(lnReceived + IIF(TranCd = '2', NMFGTOTQTY, 0)),;
                     SUM(lnDamaged + IIF(TranCd = '3', NMFGTOTQTY, 0)),;
                     SUM(lnCanceled + IIF(TranCd = '4', NMFGTOTQTY, 0)),;
                     SUM(lnSubEst1 + nEst_Cost1),;
                     SUM(lnSubEst2 + nEst_Cost2),;
                     SUM(lnSubEst3 + nEst_Cost3),;
                     SUM(lnSubEst4 + nEst_Cost4),;
                     SUM(lnSubLan1 + nLan_Cost1),;
                     SUM(lnSubLan2 + nLan_Cost2),;
                     SUM(lnSubLan3 + nLan_Cost3),;
                     SUM(lnSubLan4 + nLan_Cost4),;                 
                     SUM(lnSubAct1 + nAct_Cost1),;                 
                     SUM(lnSubAct2 + nAct_Cost2),;                 
                     SUM(lnSubAct3 + nAct_Cost3),;                 
                     SUM(lnSubAct4 + nAct_Cost4);                                  
                  TO lnBudget, lnReceived, lnDamaged, lnCanceled,;
                     lnSubEst1,;
                     lnSubEst2,;
                     lnSubEst3,;
                     lnSubEst4,;
                     lnSubLan1,;
                     lnSubLan2,;
                     lnSubLan3,;
                     lnSubLan4,;
                     lnSubAct1,;
                     lnSubAct2,;
                     lnSubAct3,;
                     lnSubAct4
  ELSE
    CALCULATE NEXT 1 SUM(lnBudget + IIF(TranCd = '1', NMFGTOTQTY, 0)),;
                     SUM(lnReceived + IIF(TranCd = '2', NMFGTOTQTY, 0)),;
                     SUM(lnDamaged + IIF(TranCd = '3', NMFGTOTQTY, 0)),;
                     SUM(lnCanceled + IIF(TranCd = '4', NMFGTOTQTY, 0));
                TO lnBudget, lnReceived, lnDamaged, lnCanceled
  ENDIF                    
  lnOpen = MAX(lnBudget - lnReceived - lnDamaged - lnCanceled, 0)
ENDIF
  
*!*************************************************************
*! Name      : lfResetCst
*! Developer : BASSEM RAFAAT
*! Date      : 07/15/1999
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfResetCst()
*!*************************************************************
FUNCTION lfResetCst
STORE .F. TO llHdrCost, llPrntNote

*!*************************************************************
*! Name      : lfResetTot
*! Developer : BASSEM RAFAAT
*! Date      : 07/15/1999
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfResetTot()
*!*************************************************************
FUNCTION lfResetTot
lnTotBud  = lnTotBud  + lnBudget 
lnTotRec  = lnTotRec  + lnReceived
lnTotDam  = lnTotDam  + lnDamaged
lnTotCan  = lnTotCan  + lnCanceled
lnTotOpen = lnTotOpen + lnOpen
lnTotEst1 = lnTotEst1 + lnSubEst1    
lnTotEst2 = lnTotEst2 + lnSubEst2
lnTotEst3 = lnTotEst3 + lnSubEst3
lnTotEst4 = lnTotEst4 + lnSubEst4        
lnTotLan1 = lnTotLan1 + lnSubLan1
lnTotLan2 = lnTotLan2 + lnSubLan2
lnTotLan3 = lnTotLan3 + lnSubLan3
lnTotLan4 = lnTotLan4 + lnSubLan4     
lnTotAct1 = lnTotAct1 + lnSubAct1
lnTotAct2 = lnTotAct2 + lnSubAct2
lnTotAct3 = lnTotAct3 + lnSubAct3
lnTotAct4 = lnTotAct4 + lnSubAct4
STORE 0.000 TO lnBudget,  lnReceived,lnDamaged, lnCanceled, lnOpen,;
               lnSubEst1, lnSubEst2, lnSubEst3, lnSubEst4,;
               lnSubLan1, lnSubLan2, lnSubLan3, lnSubLan4,; 
               lnSubAct1, lnSubAct2, lnSubAct3, lnSubAct4 
RETURN ''               

*!*************************************************************
*! Name      : lfSubEst
*! Developer : BASSEM RAFAAT
*! Date      : 07/15/1999
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfSubEst()
*!*************************************************************
FUNCTION lfSubEst
lnSubEst = lnSubEst1 + lnSubEst2 + lnSubEst3 + lnSubEst4
lnSubLan = lnSubLan1 + lnSubLan2 + lnSubLan3 + lnSubLan4
lnSubAct = lnSubAct1 + lnSubAct2 + lnSubAct3 + lnSubAct4
RETURN lnSubEst


*!*************************************************************
*! Name      : lfTotEst
*! Developer : BASSEM RAFAAT
*! Date      : 07/15/1999
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfTotEst()
*!*************************************************************
FUNCTION lfTotEst
lnTotEst = lnTotEst1 + lnTotEst2 + lnTotEst3 + lnTotEst4
lnTotLan = lnTotLan1 + lnTotLan2 + lnTotLan3 + lnTotLan4
lnTotAct = lnTotAct1 + lnTotAct2 + lnTotAct3 + lnTotAct4
RETURN lnTotEst

*!*************************************************************
*! Name      : lfGetData
*! Developer : BASSEM RAFAAT
*! Date      : 07/15/1999
*! Purpose   : Collects data according to the grid selection
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  IF lfGetData()
*!*************************************************************
FUNCTION lfGetData

WAIT 'Selecting records for report ...' WINDOW NOWAIT

lcFltrStr = lcRpExp                       && Variable hold the filter from the grid

DO CASE                                   && we modify to the filter in the Header without include lines , Detail form ,Receving form
**--- User select All from the order status    
  CASE lcRpOrdSt = 'L'
    lcdescrep    = " "
    lcFltrStr    = lcFltrStr + [ AND  MMFGORDH.STATUS = '' ]

**--- User select open from the order status
  CASE lcRpOrdSt = 'O'
    lcdescrep    = "Open"
    lcFltrStr    = lcFltrStr + [ AND  MMFGORDH.STATUS = 'O' ]
    
**--- User select Complete from the order status
  CASE lcRpOrdSt = 'C'
    lcdescrep    = "Complete"
    lcFltrStr    = lcFltrStr + [ AND  MMFGORDH.STATUS = 'C' ]
    
**--- User select on Hold from the order status
  CASE lcRpOrdSt = 'H'
    lcdescrep    = "on Hold"
    lcFltrStr    = lcFltrStr + [ AND  MMFGORDH.STATUS = 'H' ]
    
**--- User select Closed from the order status
  CASE lcRpOrdSt = 'S'
    lcdescrep    = "Closed"
    lcFltrStr    = lcFltrStr + [ AND  MMFGORDH.STATUS = 'S' ]
    
**--- User select Cancelled from the order status
  CASE lcRpOrdSt = 'X'
    lcdescrep    = "Cancelled"
    lcFltrStr    = lcFltrStr + [ AND  MMFGORDH.STATUS = 'X' ]
    
**--- User select Actualized from the order status    
  CASE lcRpOrdSt = 'A'
    lcdescrep    = "Actualized"
    lcFltrStr    = lcFltrStr + [ AND  MMFGORDH.STATUS = 'A' ]
    
    
ENDCASE

IF lcRpOrLn = 'Y'                  && If the user change include order lines to YES
  DO CASE                          && we modify to the filter in the Header with include lines 
  **--- User select All from the Include lines
    CASE lcRpLine  = 'A'                                         && All
      lcFltrStr    = lcFltrStr + [ AND  MMFGORDD.TRANCD = '' ]
      lcdesLines   = " "
      
  **--- User select open from the order status
    CASE lcRpLine  = '1'                                         && Budget
      lcFltrStr    = lcFltrStr + [ AND  MMFGORDD.TRANCD = '1' ]
      lcdesLines   = "Budget Lines"
    
  **--- User select Complete from the order status
    CASE lcRpLine  = '2'                                          && Received
      lcFltrStr    = lcFltrStr + [ AND  MMFGORDD.TRANCD = '2' ]
      lcdesLines   = "Received"
    
  **--- User select on Hold from the order status
    CASE lcRpLine  = '3'                                          && Damaged
      lcFltrStr    = lcFltrStr + [ AND  MMFGORDD.TRANCD = '3' ]
      lcdesLines   = "Damaged"
    
  **--- User select Closed from the order status
    CASE lcRpLine  = '4'                                          && Cancelled
      lcFltrStr    = lcFltrStr + [ AND  MMFGORDD.TRANCD = '4' ]
      lcdesLines   = "Cancelled"
    
  ENDCASE
ENDIF  

IF llHdrRep 
*-bas
*  lcHdrStr  = [cMFGOrdNo+' '+PADR(IIF(lcRpOrdSt $ 'OCHSX', lcdescrep, ''),11)+]+;
              [DTOC(Entered)+' '+DTOC(Complete)+' '+]+;
              [IIF(nMmgBudget>0, STR((nMmgBudget-nMmfgOpen)/nMmgBudget*100,5,2),'N/A  ')+' '+]+;
              [cfabric+' '+PADL(nMmgBudget,11)+' '+PADL(Received,11)+' '+]+; 
              [PADL(Damaged,11)+' '+PADL(Canceled,11)+' '+PADL(nMmfgOpen,11)+' '+]+;
              IIF(llMultiWare,[cDefWare],[]) 
*--bas              
*  lcHdrStr  = [cMFGOrdNo+' '+PADR(IIF(lcRpOrdSt $ 'OCHSX', lcdescrep, lfStatus() ),11)+]+;
              [DTOC(Entered)+' '+DTOC(Complete)+' '+]+;
              [IIF(nMmgBudget>0, STR((nMmgBudget-nMmfgOpen)/nMmgBudget*100,5,2),'N/A  ')+' '+]+;
              [cfabric+' '+PADL(nMmgBudget,11)+' '+PADL(Received,11)+' '+]+; 
              [PADL(Damaged,11)+' '+PADL(Canceled,11)+' '+PADL(nMmfgOpen,11)+' '+]+;
              IIF(llMultiWare,[cDefWare],[])               

  lcHdrStr  = [cMFGOrdNo+' '+PADR(IIF(lcRpOrdSt $ 'AOCHSX', lcdescrep, lfStatus() ),11)+]+;
              [DTOC(Entered)+' '+DTOC(Complete)+' '+]+;
              [IIF(nMmgBudget>0, STR((nMmgBudget-nMmfgOpen)/nMmgBudget*100,5,2),'N/A  ')+' '+]+;
              [cfabric+' '+PADL(nMmgBudget,11)+' '+PADL(Received,11)+' '+]+; 
              [PADL(Damaged,11)+' '+PADL(Canceled,11)+' '+PADL(nMmfgOpen,11)+' '+]+;
              IIF(llMultiWare,[cDefWare],[])               
              
*-bas              
  IF llIncLines 
    DO CASE
      CASE lcSortOpt = 'O'
        lcSortStr   = IIF(lcLinesOpt = 'C',;
                          'MMFGORDH.cMFGOrdNo, Color,'+;
                          'Dyelot, TranCd, dRecvDate',;
                          'MMFGORDH.cMFGOrdNo, dRecvDate, TranCd,'+;
                          'Color,Dyelot')
        lcMainGrp   = ['']
        lcSubTotStr = ''
    
      CASE lcSortOpt = 'I'
        lcSortStr   = IIF(lcLinesOpt = 'C',;
                          'MMFGORDH.cFabric, MMFGORDH.cMFGOrdNo, Color,'+;
                          'Dyelot,TranCd, dRecvDate',;
                          'MMFGORDH.cFabric, MMFGORDH.cMFGOrdNo, dRecvDate,'+;
                          'TranCd, Color, Dyelot')
        lcMainGrp   = 'cFabric'
        lcSubTotStr = ['Item ' + cFabric + ' SubTotal : ']

      CASE lcSortOpt = 'C'
        lcSortStr   = IIF(lcLinesOpt = 'C',;
                          'Complete, MMFGORDH.cMFGOrdNo,'+;
                          'Color, Dyelot, TranCd, dRecvDate',;
                          'Complete, MMFGORDH.cMFGOrdNo,'+;
                          'dRecvDate, TranCd, Color, Dyelot')
        lcMainGrp   = 'DTOC(Complete)'
        lcSubTotStr = ['Complete Date ' + DTOC(Complete) + ' SubTotal : ']
    ENDCASE  
    lcFltrStr = [ .AND. ] + lcFltrStr               && Add <and> which is ignord when we use the fixed filter
    SELECT MMFGORDH.*, Color , Pattern , Dyelot,;
           MMFGORDD.cWareCode AS cRecWare,;
           TranCd, dRecvDate, nMfgTotQty,;
           MMFGORDH.cWareCode AS cDefWare;
      FROM MMFGORDH, MMFGORDD;
      INTO CURSOR (lcTempCurs);
     WHERE MMFGORDH.cMFGOrdNo = MMFGORDD.cMFGOrdNo &lcFltrStr ;
     ORDER BY &lcSortStr
  ELSE
    IF !EMPTY(lcFltrStr)     
      lcFltrStr = 'WHERE ' + lcFltrStr
    ENDIF  
    DO CASE
      CASE lcSortOpt = 'O'
        lcSortStr   = 'MMFGORDH.cMFGOrdNo'
        lcMainGrp   = ['']
        lcSubTotStr = ''
    
      CASE lcSortOpt = 'I'
        lcSortStr   = 'MMFGORDH.cFabric, MMFGORDH.cMFGOrdNo'
        lcMainGrp   = 'cFabric'
        lcSubTotStr = ['Item ' + cFabric + ' SubTotal : ']
                
      CASE lcSortOpt = 'C'
        lcSortStr = 'Complete, MMFGORDH.cMFGOrdNo'
        lcMainGrp = 'DTOC(Complete)'
        lcSubTotStr = ['Complete Date ' + DTOC(Complete) + ' SubTotal : ']
    ENDCASE  
    
    SELECT *, cWareCode AS cDefWare;
      FROM MMFGORDH &lcFltrStr;
      INTO CURSOR (lcTempCurs) ;
     ORDER BY &lcSortStr
  ENDIF     
ELSE
  lcFltrStr = [ .AND. ] + lcFltrStr               && Add <and> which is ignord when we use the fixed filter
  IF llRecvLog
    lcFltrStr = lcFltrStr + [ AND + MMFGORDD.TRANCD = '2' ] && FOR RECEIVING ONLY
  ENDIF
  DO CASE
    CASE lcSortOpt = 'O'
      lcSortStr    = 'MMFGORDH.cMFGOrdNo, TranCd, MMFGORDH.cFabric, Color, Dyelot, TranCd, dRecvDate'
      lcMainGrp    = 'cMFGOrdNo'
      lcSubTotStr  = ['Order : ' + cMFGOrdNo + ' Subtotal : ']

    CASE lcSortOpt = 'I'
      lcSortStr    = 'MMFGORDH.cFabric, Color, Dyelot,TranCd, MMFGORDH.cMFGOrdNo, dRecvDate'
      lcMainGrp    = 'cFabric + Color'
      lcSubTotStr  = ['Item\Color ' + ALLTRIM(cFabric) + '\' + ALLTRIM(Color) + ' SubTotal : ']

    CASE lcSortOpt = 'C'
      lcSortStr    = 'Complete, MMFGORDH.cMFGOrdNo, MMFGORDH.cFabric, Color, Dyelot, TranCd, dRecvDate'
      lcMainGrp    = 'DTOC(Complete)'
      lcSubTotStr  = ['Complete Date ' + DTOC(Complete) + ' SubTotal : ']

    CASE lcSortOpt = 'R'
      lcSortStr    = 'MMFGORDH.cMFGOrdNo, dRecvDate, TranCd, MMFGORDH.cFabric, Color, Dyelot'
      lcMainGrp    = 'cMFGOrdNo'
      lcSubTotStr  = ['Order ' + cMFGOrdNo + ' SubTotal : ']

    CASE lcSortOpt = 'V'
      lcSortStr    = 'dRecvDate, TranCd, MMFGORDH.cMFGOrdNo, MMFGORDH.cFabric, Color, Dyelot'
      lcMainGrp    = 'dRecvDate'
      lcSubTotStr  = ['Receiving Date ' + DTOC(dRecvDate) + ' SubTotal : ']      
      
  ENDCASE  

  SELECT MMFGORDH.cMFGORdNo, MMFGORDH.cFabric,;
         Color , Pattern  , Dyelot   , ;
         MMFGORDD.cWareCode AS cRecWare,;
         TranCd, dRecvDate, NMFGTOTQTY,;
         Status, Entered, Complete,;
         MMFGORDD.nCost1 * NMFGTOTQTY AS nEst_Cost1, MMFGORDD.nCost2  * NMFGTOTQTY AS nEst_Cost2,;
         MMFGORDD.nCost3 * NMFGTOTQTY AS nEst_Cost3, MMFGORDD.nCost4  * NMFGTOTQTY AS nEst_Cost4,;
         MMFGORDD.nLan_Cost1 * NMFGTOTQTY AS nLan_Cost1,;
         MMFGORDD.nLan_Cost2 * NMFGTOTQTY AS nLan_Cost2,;
         MMFGORDD.nLan_Cost3 * NMFGTOTQTY AS nLan_Cost3,;
         MMFGORDD.nLan_Cost4 * NMFGTOTQTY AS nLan_Cost4,;
         MMFGORDD.nAct_Cost1 * NMFGTOTQTY AS nAct_Cost1,;
         MMFGORDD.nAct_Cost2 * NMFGTOTQTY AS nAct_Cost2,;
         MMFGORDD.nAct_Cost3 * NMFGTOTQTY AS nAct_Cost3,;
         MMFGORDD.nAct_Cost4 * NMFGTOTQTY AS nAct_Cost4,;
         Link_Code; 
    FROM MMFGORDH, MMFGORDD;
    INTO CURSOR (lcTempCurs);
   WHERE MMFGORDH.cMFGOrdNo = MMFGORDD.cMFGOrdNo &lcFltrStr;
   ORDER BY &lcSortStr
ENDIF
IF EOF()
  WAIT CLEAR 
  *-- Message <No records to display.>
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN .F.
ENDIF
*!*************************************************************
*! Name      : lfvWare
*! Developer : BASSEM RAFAAT
*! Date      : 07/15/1999
*! Purpose   : Validate the warehous
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvWare()
*!*************************************************************

FUNCTION lfvWare
PRIVATE lcWarFld,lcWarhous,lnSelFile,lcWareTag

lcWarFld   = VARREAD()
lcWareCode = EVAL(lcWarFld)
lnSelFile = SELECT(0)

SELECT WAREHOUS
lcWareTag = ORDER('WAREHOUS') 
SET ORDER TO TAG WAREHOUS IN WAREHOUS

IF !EMPTY(lcWareCode) .AND. ('?' $ lcWareCode .OR. !SEEK(lcWareCode , 'WAREHOUS'))
  lcWareCode =gfBrowWare(.T.)  
ENDIF

&lcWarFld = lcWareCode
SET ORDER TO lcWareTag
SELECT (lnSelFile)
*!*************************************************************
*! Name      : lfvOrd
*! Developer : BASSEM RAFAAT
*! Date      : 07/15/1999
*! Purpose   : Validate the piktkt
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOrd()
*!*************************************************************

FUNCTION lfvOrd
PRIVATE lcOrdFld,lcOrder,lnSelFile,lcOrdTag
	
lcOrdFld  = VARREAD()
lcOrder   = EVAL(lcOrdFld)
lnSelFile =  SELECT(0)

SELECT MMFGORDH
lcOrdTag  = ORDER('MMFGORDH')
SET ORDER TO TAG Mmfgordh IN MMFGORDH

IF !EMPTY(lcOrder) .AND. (!SEEK(lcOrder , 'MMFGORDH'))
  DIMENSION laTemp[1]
  laTemp = ''     
  lcBrFields = "CMFGORDNO :R :H= 'Order #' , CFABRIC  :R :H= 'Fabric' , ;
                CWARECODE  :R :H= 'Warehouse' , STATUS   :R :H= 'S'  ,  ;
                ENTERED  :R :H= 'Entered'  ,COMPLETE  :R :H= 'Complete' "
  
   = gfBrows('','MMFGORDH','laTemp')
  IF !EMPTY(laTemp[1])
    lcOrder = laTemp[1]
  ELSE 
    lcOrder = ''
  ENDIF
ENDIF

&lcOrdFld = lcOrder
SET ORDER TO lcOrdTag
SELECT (lnSelFile)
*!*************************************************************
*! Name      : lfvFab
*! Developer : BASSEM RAFAAT
*! Date      : 07/15/1999
*! Purpose   : Validate the fabric item range
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFab()
*!*************************************************************

FUNCTION lfvFab
PRIVATE lcFabFld,lcFabric,lnSelFile,lcFabTag

lcFabFld  = VARREAD()
lcFabric  = EVAL(lcFabFld)

lnSelFile = SELECT(0)
SELECT FABRIC
lcFabTag  = ORDER('FABRIC')
SET ORDER TO TAG FABRIC IN FABRIC
IF !EMPTY(lcFabric)  .AND. ('?' $  lcFabric .OR. !SEEK(lcFabric , 'FABRIC')) 
  =FABROW (@lcFabric,'*')
ENDIF

&lcFabFld  = lcFabric
SET ORDER TO lcFabTag
SELECT (lnSelFile)
*!*************************************************************
*! Name      : lfSRVFab
*! Developer : BASSEM RAFAAT
*! Date      : 07/15/1999
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVFab()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVFab
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to primary fabric
    *-- unique index.
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
    *llChFabric = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
ENDCASE
*-- end of lfSRVFab.

*!*************************************************************
*! Name      : lfFabSum
*! Developer : BASSEM RAFAAT
*! Date      : 07/15/1999
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
lnTotcomp = 0
lnFabRec = IIF(RECNO('FABRIC') <= RECCOUNT('FABRIC'),RECNO('FABRIC'),1)

SELECT Fabric_X
SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
SELECT Fabric
GO lnFabRec
RETURN INT(lnTotcomp)
*-- end of lfFabSum.

*!*************************************************************
*! Name      : lfClrRead
*! Developer : BASSEM RAFAAT
*! Date      : 07/15/1999
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : =lfClrRead()
*!*************************************************************

FUNCTION lfClrRead
CLEAR READ

*!*************************************************************
*! Name      : lfPrepair
*! Developer : BASSEM RAFAAT
*! Date      : 07/15/1999
*! Purpose   : Validate Selecting the menu bar 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPrepair()
*! Notes       : lcOGRepID >> Variable in [GFopgrid.prg] hold the 
*!             : name of the prgs in the SYREPUVR , and we use it 
*!             : to know the name of the prg .
*!*************************************************************

FUNCTION lfPrepair
DO CASE

  CASE lcOGRepID = 'MAMFHDR'    
    llHdrRep     = .T.
    llRecvLog    = .F.    
    lcMainTtl    = 'Material Manufacturing Orders Form'
    llNtSumRep   = .F.
    
  CASE lcOGRepID = 'MAMFDTL'
    llHdrRep     = .F.
    llRecvLog    = .F.    
    lcMainTtl    = 'Material Manufacturing Order Lines Report'

  CASE lcOGRepID = 'MAMFREC'
    llHdrRep     = .F. 
    llRecvLog    = .T.
    lcMainTtl    = 'Material Manufacturing Receipts Log'

ENDCASE

*!*************************************************************
*! Name      : lfStatus
*! Developer : BASSEM RAFAAT
*! Date      : 10/13/1999
*! Purpose   : Change the character of status field to words
*!*************************************************************
*! Called from : lfGetData()
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : lcX
*!*************************************************************
FUNCTION lfStatus

DO CASE
**--- MMFGORDH.STATUS = 'O'
*  CASE MMFGORDH.STATUS = 'O'
  CASE &lcTempCurs..STATUS = 'O'
    lcX    = "Open"
    
**--- MMFGORDH.STATUS = 'C'
*  CASE MMFGORDH.STATUS = 'C'
  CASE &lcTempCurs..STATUS = 'C'
    lcX    = "Complete"
    
**--- MMFGORDH.STATUS = 'H'
*  CASE MMFGORDH.STATUS = 'H'
  CASE &lcTempCurs..STATUS = 'H'
    lcX    = "on Hold"
    
**--- MMFGORDH.STATUS = 'S'
*  CASE MMFGORDH.STATUS = 'S'
  CASE &lcTempCurs..STATUS = 'S'
    lcX    = "Closed"
    
**--- MMFGORDH.STATUS = 'X'
*  CASE MMFGORDH.STATUS = 'X'
  CASE &lcTempCurs..STATUS = 'X'
    lcX    = "Cancelled"
    
*--bas
**--- MMFGORDH.STATUS = 'A'
*  CASE MMFGORDH.STATUS = 'A'
  CASE &lcTempCurs..STATUS = 'A'
    lcX    = "Actualized"
    
ENDCASE
    
RETURN lcX