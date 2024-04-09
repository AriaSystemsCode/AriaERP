*:***********************************************************************
*:  Program file : POCNFCU.PRG
*:  Program desc.: Confirm Cut Qty 
*:         System: Aria 4XP
*:      Developer: Mariam Mazhar
*:           Date: 02/25/2010
*:      Reference: C201219[T20091217.0005]
*:************************************************************************
*: Modifications:
*! C201219,2 MMT 03/15/2010 Fix bug of wrong raised qty[T20091217.0005]
*:************************************************************************
DO FORM (oAriaApplication.ScreenHome+'PO\POCNFCUT.SCX')

*!*************************************************************
*! Name      : lfChangeMode
*: Developer : Mariam Mazhar
*: Date      : 02/25/2010
*! Purpose   : Change Mode
*!*************************************************************
FUNCTION lfChangeMode
  PARAMETERS loFormSet

  loFormSet.ariaform1.TxtStatus.ENABLED = .F.
  DO CASE
    CASE loFormSet.ActiveMode = 'S'
      loFormSet.ariaform1.kbStyle.ENABLED = .F.
      loFormSet.ariaform1.kbStyle.VALUE = ''
      loFormSet.ariaform1.knPoNo.ENABLED = .T.
      loFormSet.ariaform1.knPoNo.KeyTextBox.VALUE =''
      loFormSet.ariaform1.TxtStatus.VALUE =  ''
      IF !EMPTY(loFormSet.lcposln) AND USED(loFormSet.lcposln) AND RECCOUNT()>0
        SELECT (loFormSet.lcposln)
        ZAP
      ENDIF
      
      IF !EMPTY(loFormSet.lcPONum)
        loFormSet.ariaForm1.knPoNo.keytextbox.Value=loFormSet.lcPONum 
        loFormSet.ariaForm1.knPoNo.keytextbox.Valid()
        loFormSet.ariaform1.kbStyle.ENABLED = .T.
      ENDIF
        
    CASE loFormSet.ActiveMode = 'V'
      loFormSet.ariaform1.knPoNo.ENABLED = .F.
      loFormSet.ariaform1.kbStyle.ENABLED = .F.

    CASE loFormSet.ActiveMode = 'E'
      loFormSet.ariaform1.knPoNo.ENABLED = .F.
      loFormSet.ariaform1.kbStyle.ENABLED = .F.
      loFormSet.ariaform1.cntOrdered.ENABLED = .F.
      loFormSet.ariaform1.cntCut.ENABLED = .T.
      loFormSet.ariaform1.cntRec.ENABLED = .F.
      loFormSet.ariaform1.cntCut.txtTotQty.ENABLED = .F.
      loFormSet.ariaform1.cntCut.mdisableunused ()

      loMainWorkOrder = loFormSet.ariaform1.mainworkorder1
      lcTmpPoHdr = loMainWorkOrder.cposhdr

      SELECT (lcTmpPoHdr)
      DELETE ALL
      SELECT (loMainWorkOrder.cMPosHdr)
      DELETE ALL
      SELECT POSHDR
      SCATTER MEMO MEMVAR
      INSERT INTO (lcTmpPoHdr) FROM MEMVAR
      INSERT INTO (loMainWorkOrder.cMPosHdr) FROM MEMVAR
      =TABLEUPDATE(.T.,.T.,loMainWorkOrder.cMPosHdr)
      lcPoNo = PADR(loFormSet.ariaform1.knPoNo.KeyTextBox.VALUE,6)
      lfGetOrgPOLines(lcPoNo,loFormSet)

  ENDCASE
*!*************************************************************
*! Name      : lfvPo
*! Developer : Mariam Mazhar (MMT)
*! Date      : 02/25/2010
*! Purpose   : Validate PO Number
*!*************************************************************
FUNCTION lfvPo
  PARAMETERS loFormSet
  LOCAL lcPoNo
  lcPoNo = PADR(loFormSet.ariaform1.knPoNo.KeyTextBox.VALUE,6)

  SELECT POSHDR
  IF (EMPTY(lcPoNo) AND loFormSet.ariaform1.knPoNo.selectedfrombrowse) OR ;
      ('?' $ ALLTRIM(lcPoNo)) OR;
      (!EMPTY(lcPoNo) AND !gfSEEK('PP'+lcPoNo)) OR ;
      loFormSet.ariaform1.knPoNo.selectedfrombrowse

    lcBrFields = [Po :R :H= 'P/O#' , STATUS :R :H= 'S', Vendor :R :H= 'Vendor',] +;
      [Entered :R :H= 'Entered' , Complete :R :H= 'Complete', ] +;
      [nStyorder :R :H= 'Tot. Qty.' , POtotal :R :H= 'Amount',                    Receive  :R :H= 'Receive',Open :R :H= 'Open']

    IF !loFormSet.ariaform1.knPoNo.selectedfrombrowse
      loFormSet.ariaform1.knPoNo.selectedfrombrowse = .T.
    ENDIF

    IF loFormSet.ariaform1.knPoNo.selectedfrombrowse
      lcIndexSel = "POSHDR"
      lcFilter = "'PP' FOR  cbusdocu+Cstytype = 'PP' AND POSHDR.STATUS In ('O','A','C')"
      lcPoNo = IIF(ARIABROW(lcFilter,"PO",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, ;
        gnBrFSCol2,'','','POSHDR','laBrowArr', .F.,'',.F.,'POSHDR','','',lcIndexSel),POSHDR.PO,SPACE(6))


      loFormSet.ariaform1.knPoNo.KeyTextBox.VALUE = lcPoNo
      IF !EMPTY(lcPoNo)
        =lfGetPOLines(lcPoNo)
        loFormSet.ariaform1.TxtStatus.VALUE = IIF(POSHDR.STATUS = 'O','Open',IIF(POSHDR.STATUS = 'A',;
          'Actualized',IIF(POSHDR.STATUS = 'C','Completed',;
          IIF(POSHDR.STATUS = 'S','Closed','Cancelled'))))
      ENDIF
      loFormSet.ariaform1.knPoNo.selectedfrombrowse = .F.
      RETURN .T.
    ENDIF
  ELSE
    IF gfSEEK('PP'+lcPoNo)
      IF !INLIST(POSHDR.STATUS,'O','A','C')
        =gfModalGen('INM00000B00000',.F.,.F.,.F.,"PO:"+lcPoNo+" is "+IIF(POSHDR.STATUS = 'X','Cancelled',IIF(POSHDR.STATUS = 'H','Hold',IIF(POSHDR.STATUS = 'S','Closed','Bid'))))
        loFormSet.ariaform1.knPoNo.KeyTextBox.VALUE = ''
        RETURN
      ENDIF
      loFormSet.ariaform1.knPoNo.selectedfrombrowse = .F.
      IF !EMPTY(lcPoNo)
        =lfGetPOLines(lcPoNo)
        loFormSet.ariaform1.TxtStatus.VALUE = IIF(POSHDR.STATUS = 'O','Open',IIF(POSHDR.STATUS = 'A',;
          'Actualized',IIF(POSHDR.STATUS = 'C','Completed',;
          IIF(POSHDR.STATUS = 'S','Closed','Cancelled'))))
      ENDIF
      RETURN .T.
    ENDIF
  ENDIF

ENDFUNC

*!*************************************************************
*! Name      : lfInitForm
*: Developer : Mariam Mazhar
*: Date      : 02/25/2010
*! Purpose   : Init Form
*!*************************************************************
FUNCTION lfInitForm
  PARAMETERS loFormSet

  =gfOpenTable('SCALE','SCALE')
  =gfOpenTable('STYLE','STYLE')
  =gfOpenTable('STYDYE','STYDYE')
  =gfOpenTable('POSHDR','POSHDR')
  =gfOpenTable('POSln','POSln')
  =gfOpenTable('POCUT','POCUT')
  =gfOpenTable('ORDLINE','ORDLINE')
  =gfOpenTable('APVENDOR','VENCODE')
  =gfOpenTable('ORDHDR','ORDHDR')
  =gfOpenTable('CUSTOMER','CUSTOMER')

  loFormSet.lcposln = gfTempName()
  SELECT POSLN
  DIMENSION LAPOSLNSTRUCT[1,18]
  LAPOSLNSTRUCT = ''
  lnFldCnt = AFIELDS(LAPOSLNSTRUCT)
  DIMENSION LAPOSLNSTRUCT[lnFldCnt + 18,18]

  *!*  LAPOSLNSTRUCT[1,1] = 'PO'
  *!*  LAPOSLNSTRUCT[1,2] = 'C'
  *!*  LAPOSLNSTRUCT[1,3] = 6
  *!*  LAPOSLNSTRUCT[1,4] = 0

  *!*  LAPOSLNSTRUCT[2,1] = 'CSTYTYPE'
  *!*  LAPOSLNSTRUCT[2,2] = 'C'
  *!*  LAPOSLNSTRUCT[2,3] = 1
  *!*  LAPOSLNSTRUCT[2,4] = 0

  *!*  LAPOSLNSTRUCT[3,1] = 'CBUSDOCU'
  *!*  LAPOSLNSTRUCT[3,2] = 'C'
  *!*  LAPOSLNSTRUCT[3,3] = 1
  *!*  LAPOSLNSTRUCT[3,4] = 0

  *!*  LAPOSLNSTRUCT[4,1] = 'STYLE'
  *!*  LAPOSLNSTRUCT[4,2] = 'C'
  *!*  LAPOSLNSTRUCT[4,3] = 19
  *!*  LAPOSLNSTRUCT[4,4] = 0

  *!*  LAPOSLNSTRUCT[5,1] = 'LINENO'
  *!*  LAPOSLNSTRUCT[5,2] = 'N'
  *!*  LAPOSLNSTRUCT[5,3] = 6
  *!*  LAPOSLNSTRUCT[5,4] = 0

  LAPOSLNSTRUCT[lnFldCnt +1,1] = 'TOTREC'
  LAPOSLNSTRUCT[lnFldCnt +1,2] = 'N'
  LAPOSLNSTRUCT[lnFldCnt +1,3] = 11
  LAPOSLNSTRUCT[lnFldCnt +1,4] = 0

  *!*  LAPOSLNSTRUCT[7,1] = 'TOTQTY'
  *!*  LAPOSLNSTRUCT[7,2] = 'N'
  *!*  LAPOSLNSTRUCT[7,3] = 11
  *!*  LAPOSLNSTRUCT[7,4] = 0

  LAPOSLNSTRUCT[lnFldCnt +2,1] = 'TOTCUT'
  LAPOSLNSTRUCT[lnFldCnt +2,2] = 'N'
  LAPOSLNSTRUCT[lnFldCnt +2,3] = 11
  LAPOSLNSTRUCT[lnFldCnt +2,4] = 0


  lnCnt = lnFldCnt +3
  FOR lnI = 1 TO 8
    *!*    LAPOSLNSTRUCT[lnCnt ,1] = 'QTY'+STR(lnI,1)
    *!*    LAPOSLNSTRUCT[lnCnt ,2] = 'N'
    *!*    LAPOSLNSTRUCT[lnCnt ,3] = 11
    *!*    LAPOSLNSTRUCT[lnCnt ,4] = 0
    *!*    lnCnt = lnCnt +  1
    LAPOSLNSTRUCT[lnCnt ,1] = 'REC'+STR(lnI,1)
    LAPOSLNSTRUCT[lnCnt ,2] = 'N'
    LAPOSLNSTRUCT[lnCnt ,3] = 11
    LAPOSLNSTRUCT[lnCnt ,4] = 0
    lnCnt = lnCnt +  1
    LAPOSLNSTRUCT[lnCnt ,1] = 'CUT'+STR(lnI,1)
    LAPOSLNSTRUCT[lnCnt ,2] = 'N'
    LAPOSLNSTRUCT[lnCnt ,3] = 11
    LAPOSLNSTRUCT[lnCnt ,4] = 0
    lnCnt = lnCnt +  1
  ENDFOR

  FOR lnCount = 1 TO 18
    STORE '' TO LAPOSLNSTRUCT[lnFldCnt +lnCount,7],LAPOSLNSTRUCT[lnFldCnt +lnCount,8],LAPOSLNSTRUCT[lnFldCnt +lnCount,9],;
      LAPOSLNSTRUCT[lnFldCnt +lnCount,10],LAPOSLNSTRUCT[lnFldCnt +lnCount,11],LAPOSLNSTRUCT[lnFldCnt +lnCount,12],;
      LAPOSLNSTRUCT[lnFldCnt +lnCount,13],LAPOSLNSTRUCT[lnFldCnt +lnCount,14],LAPOSLNSTRUCT[lnFldCnt +lnCount,15],;
      LAPOSLNSTRUCT[lnFldCnt +lnCount,16]
    STORE 0 TO  LAPOSLNSTRUCT[lnFldCnt +lnCount,17],LAPOSLNSTRUCT[lnFldCnt +lnCount,18]
  ENDFOR
  =gfCrtTmp(loFormSet.lcposln,@LAPOSLNSTRUCT,'CBUSDOCU+CSTYTYPE+PO+STYLE+STR(LINENO,6)',loFormSet.lcposln)

*!*************************************************************
*! Name      : lfGetPOLines
*: Developer : Mariam Mazhar
*: Date      : 02/25/2010
*! Purpose   : Get PO Lines
*!*************************************************************
FUNCTION lfGetPOLines
  PARAMETERS lcPONum
  IF RECCOUNT(loFormSet.lcposln) > 0
    SELECT (loFormSet.lcposln)
    DELETE ALL
  ENDIF
  SELECT POSLN
  =gfSEEK('PP'+lcPONum)
  SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD = 'PP'+lcPONum+'0001' FOR TRANCD $ '124'
    SCATTER MEMO MEMVAR
    SELECT (loFormSet.lcposln)
    IF POSLN.TRANCD = '1' AND !SEEK(POSLN.CBUSDOCU+POSLN.CSTYTYPE+POSLN.PO+POSLN.STYLE+STR(POSLN.LINENO,6),loFormSet.lcposln)
      SELECT (loFormSet.lcposln)
      APPEND BLANK
      GATHER  MEMO MEMVAR
      *!*      REPLACE STYLE WITH POSLN.STYLE,;
      *!*              CSTYTYPE WITH POSLN.CSTYTYPE,;
      *!*              CBUSDOCU WITH POSLN.CBUSDOCU ,;
      *!*              PO WITH POSLN.PO,;
      *!*              LINENO WITH POSLN.LINENO
      *!*      DO CASE
      *!*        CASE POSLN.TRANCD = '1'
      FOR lnA = 1 TO 8
        lcA = STR(lnA,1)
        REPLACE Qty&lcA. WITH POSLN.Qty&lcA.,;
          REC&lcA. WITH 0,;
          CUT&lcA. WITH 0
      ENDFOR
      REPLACE TOTQTY WITH POSLN.TOTQTY ,;
        TOTREC WITH 0,;
        TOTCUT WITH 0
      *!*        CASE POSLN.TRANCD $ '24'
      *!*          FOR lnA = 1 TO 8
      *!*            lcA = STR(lnA,1)
      *!*            REPLACE REC&lcA. WITH REC&lcA.+POSLN.Qty&lcA.
      *!*          ENDFOR
      *!*          REPLACE TOTREC WITH TOTREC +POSLN.TOTQTY
      *!*      ENDCASE
    ELSE
      *!*      DO CASE
      *!*        CASE  POSLN.TRANCD = '1'
      *!*          FOR lnA = 1 TO 8
      *!*            lcA = STR(lnA,1)
      *!*            REPLACE Qty&lcA. WITH Qty&lcA.+POSLN.Qty&lcA.
      *!*          ENDFOR
      *!*          REPLACE TOTQTY WITH TOTQTY+POSLN.TOTQTY
      IF POSLN.TRANCD $ '24'
        FOR lnA = 1 TO 8
          lcA = STR(lnA,1)
          REPLACE REC&lcA. WITH REC&lcA.+POSLN.Qty&lcA.
        ENDFOR
        REPLACE TOTREC WITH TOTREC +POSLN.TOTQTY
      ENDIF
      *!*      ENDCASE
    ENDIF
    IF POSLN.TRANCD = '1' AND gfSEEK(POSLN.PO+POSLN.STYLE+STR(POSLN.LINENO,6),'POCUT')
      FOR lnA = 1 TO 8
        lcA = STR(lnA,1)
        REPLACE CUT&lcA. WITH CUT&lcA.+POCUT.CUT&lcA.
        *! C201219,2 MMT 03/15/2010 Fix bug of wrong raised qty[Start]
        REPLACE Qty&lcA. WITH POCUT.NWO&lcA.
        *! C201219,2 MMT 03/15/2010 Fix bug of wrong raised qty[End]
      ENDFOR
      *! C201219,2 MMT 03/15/2010 Fix bug of wrong raised qty[Start]
      REPLACE totQty WITH POCUT.NTOTWO    
      *! C201219,2 MMT 03/15/2010 Fix bug of wrong raised qty[End]
      REPLACE TOTCUT WITH POCUT.TOTCUT
    ENDIF
  ENDSCAN

*!*************************************************************
*! Name      : lfVStyle
*: Developer : Mariam Mazhar
*: Date      : 02/25/2010
*! Purpose   : Validate Style
*!*************************************************************
FUNCTION lfVStyle
  LPARAMETERS lcValueSty ,llStyBrowse,loFormSet
  SELECT(loFormSet.lcposln)
  LOCATE 
  lcPONum = PADR(loFormSet.ariaform1.knPoNo.KeyTextBox.VALUE,6)
  lcStyle = ''
  lcValueSty = IIF(!EMPTY(lcValueSty),lcValueSty,'')
  IF llStyBrowse OR ('?' $ lcValueSty) OR (!SEEK("PP"+lcPONum +lcValueSty, loFormSet.lcposln))
    DIMENSION laBrowArr[3]
    STORE '' TO laBrowArr

    lcBrFields = "PO:R :H='PO', Style :R :H='Style' :25,LINENO :R :H='Line No.',"+;
                 "lcShpTTl=IIF(!EMPTY(Account),Account,cWareCode) :H='ShipTo',"+;
                 "Qty1:R :H='Qty1',Qty2:R :H='Qty2',Qty3:R :H='Qty3',Qty4:R :H='Qty4',Qty5:R :H='Qty5',Qty6:R :H='Qty6',"+;
                 "Qty7:R :H='Qty7',Qty8:R :H='Qty8',"+;
                 "TotQty :R :H='Quantity':P='9999999',"
    lcBrFields = lcBrFields +"nTotCost =nICost1+nICost2+nICost3+nICost4+nICost5+nICost6+nICost7 :R :H='Total Cost':P='9999999.999',"
    lcBrFields = lcBrFields + "Reference :R"
    llBrRes  =ARIABROW('',"Style",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, ;
      gnBrFSCol2,'','',"PO,Style,LINENO",'laBrowArr', .F.,'',.F.,loFormSet.lcposln,'','',loFormSet.lcposln)
    IF llBrRes
      loFormSet.ariaform1.kbStyle.VALUE= laBrowArr[2]
      loFormSet.lcStyle = laBrowArr[2]
      loFormSet.lnLineNo = laBrowArr[3]
      loFormSet.ariaform1.kbStyle.ENABLED = .F.
      loFormSet.ariaform1.knPoNo.ENABLED = .F.
    ELSE
      loFormSet.ariaform1.kbStyle.VALUE= ''
      loFormSet.lcStyle = ''
      loFormSet.lnLineNo = 0
      RETURN 0
    ENDIF
    lfAddCntrlSrce(loFormSet)
    loFormSet.ChangeMode ('V')
    RETURN .T.
  ELSE
    IF SEEK("PP"+lcPONum +lcValueSty, loFormSet.lcposln)
      loFormSet.lcStyle = lcValueSty
      loFormSet.ariaform1.kbStyle.VALUE= lcValueSty
      loFormSet.lnLineNo = EVALUATE(loFormSet.lcposln+'.LINENO')
      loFormSet.ariaform1.kbStyle.ENABLED = .F.
      loFormSet.ariaform1.knPoNo.ENABLED = .F.
      lfAddCntrlSrce(loFormSet)
      loFormSet.ChangeMode ('V')
      RETURN .T.
    ENDIF
  ENDIF

*!*************************************************************
*! Name      : lfAddCntrlSrce
*: Developer : Mariam Mazhar
*: Date      : 02/25/2010
*! Purpose   : Add Control Source
*!*************************************************************
FUNCTION lfAddCntrlSrce
  LPARAMETERS loFormSet
  WITH loFormSet.ariaform1.cntOrdered
    .SCALE  = EVALUATE(loFormSet.lcposln+'.Scale')
    .txtQty1.CONTROLSOURCE = loFormSet.lcposln+'.Qty1'
    .txtQty2.CONTROLSOURCE = loFormSet.lcposln+'.Qty2'
    .txtQty3.CONTROLSOURCE = loFormSet.lcposln+'.Qty3'
    .txtQty4.CONTROLSOURCE = loFormSet.lcposln+'.Qty4'
    .txtQty5.CONTROLSOURCE = loFormSet.lcposln+'.Qty5'
    .txtQty6.CONTROLSOURCE = loFormSet.lcposln+'.Qty6'
    .txtQty7.CONTROLSOURCE = loFormSet.lcposln+'.Qty7'
    .txtQty8.CONTROLSOURCE = loFormSet.lcposln+'.Qty8'
    .txtTotQty.CONTROLSOURCE = loFormSet.lcposln+'.TotQty'
    .REFRESH
  ENDWITH
  WITH loFormSet.ariaform1.cntRec
    .SCALE  = EVALUATE(loFormSet.lcposln+'.Scale')
    .txtQty1.CONTROLSOURCE = loFormSet.lcposln+'.REC1'
    .txtQty2.CONTROLSOURCE = loFormSet.lcposln+'.REC2'
    .txtQty3.CONTROLSOURCE = loFormSet.lcposln+'.REC3'
    .txtQty4.CONTROLSOURCE = loFormSet.lcposln+'.REC4'
    .txtQty5.CONTROLSOURCE = loFormSet.lcposln+'.REC5'
    .txtQty6.CONTROLSOURCE = loFormSet.lcposln+'.REC6'
    .txtQty7.CONTROLSOURCE = loFormSet.lcposln+'.REC7'
    .txtQty8.CONTROLSOURCE = loFormSet.lcposln+'.REC8'
    .txtTotQty.CONTROLSOURCE = loFormSet.lcposln+'.TOTREC'
    .REFRESH
  ENDWITH
  WITH loFormSet.ariaform1.cntCut
    .SCALE  = EVALUATE(loFormSet.lcposln+'.Scale')
    .txtQty1.CONTROLSOURCE = loFormSet.lcposln+'.CUT1'
    .txtQty2.CONTROLSOURCE = loFormSet.lcposln+'.CUT2'
    .txtQty3.CONTROLSOURCE = loFormSet.lcposln+'.CUT3'
    .txtQty4.CONTROLSOURCE = loFormSet.lcposln+'.CUT4'
    .txtQty5.CONTROLSOURCE = loFormSet.lcposln+'.CUT5'
    .txtQty6.CONTROLSOURCE = loFormSet.lcposln+'.CUT6'
    .txtQty7.CONTROLSOURCE = loFormSet.lcposln+'.CUT7'
    .txtQty8.CONTROLSOURCE = loFormSet.lcposln+'.CUT8'
    .txtTotQty.CONTROLSOURCE = loFormSet.lcposln+'.TOTCUT'
    .REFRESH
  ENDWITH


*!*************************************************************
*! Name      : lfGetOrgPOLines
*: Developer : Mariam Mazhar
*: Date      : 02/25/2010
*! Purpose   : Get Original Lines
*!*************************************************************
FUNCTION lfGetOrgPOLines
  LPARAMETERS lcPOKey,loFormSet
  LOCAL lnCntr, lcCntr, lcMastPoLn, lcTmpPoLn, lcTmpPoTrn
  *-- To get the lines of the selected PO
  loMainWorkOrder = loFormSet.ariaform1.mainworkorder1
  IF !loMainWorkOrder.mtranlines(loFormSet.cBusDocumnt,loFormSet.cWorkOrderType,lcPOKey)
    RETURN
  ENDIF



  lcMastPoLn = loMainWorkOrder.cPosLn
  lcTmpPoLn  = loMainWorkOrder.cPoLine
  lcTmpPoTrn = loMainWorkOrder.cPOTran
  lcPOKey    = loFormSet.cBusDocumnt+loFormSet.cWorkOrderType+lcPOKey
  *--Fill Temp line files with details.
  SELECT (lcTmpPoTrn)
  ZAP
  SELECT (lcTmpPoLn)
  ZAP
  IF SEEK(lcPOKey,lcMastPoLn)
    SELECT (lcMastPoLn)
    SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+STYLE = lcPOKey FOR TRANCD$'12345'
      SCATTER TO MEMVAR
      DO CASE
        CASE TRANCD='1'
          SELECT (lcTmpPoLn)
        CASE TRANCD $ '2345'
          SELECT (lcTmpPoTrn)
      ENDCASE
      APPEND BLANK
      GATHER FROM MEMVAR
      IF !loMainWorkOrder.lMultiWarehous
        REPLACE cWareCode WITH IIF(!EMPTY(Account),loMainWorkOrder.lcdroploc ,cWareCode)
      ENDIF
      *--Show the account in warehouse field if line ship to customer.
      IF loMainWorkOrder.lMultiWarehous AND !EMPTY(Account)
        REPLACE cWareCode WITH loMainWorkOrder.lcdroploc
      ENDIF

      IF !EMPTY(&lcMastPoLn..ShipNo)
        IF loMainWorkOrder.mGetShipment(EVALUATE(lcMastPoLn+'.cBusdocu'),;
            EVALUATE(lcMastPoLn+'.cStyType'),;
            EVALUATE(lcMastPoLn+'.ShipNo'))
          LOCATE
          ldEtaDate = ETA
          SELECT (lcTmpPoTrn)
          REPLACE DETA WITH ldEtaDate
        ENDIF
      ENDIF
      *-- Add the records for the summary options
      SELECT (lcTmpPoTrn)
      DO CASE
        CASE &lcMastPoLn..TRANCD = '1'
          IF !SEEK(&lcMastPoLn..STYLE+STR(&lcMastPoLn..LINENO,6)+'1')
            APPEND BLANK
            REPLACE CBUSDOCU  WITH &lcMastPoLn..CBUSDOCU,;
              CSTYTYPE  WITH &lcMastPoLn..CSTYTYPE,;
              PO        WITH &lcMastPoLn..PO,;
              STYLE     WITH &lcMastPoLn..STYLE,;
              LINENO    WITH &lcMastPoLn..LINENO,;
              Dyelot    WITH &lcMastPoLn..Dyelot,;
              TRANCD    WITH '9',;
              cWareCode WITH &lcMastPoLn..cWareCode,;
              GROUP     WITH '1',;
              REFERENCE WITH 'Open'
            FOR lnCntr = 1 TO 8
              lcCntr = STR(lnCntr,1)
              REPLACE Qty&lcCntr WITH Qty&lcCntr + &lcMastPoLn..Qty&lcCntr,;
                TOTQTY     WITH TOTQTY     + &lcMastPoLn..Qty&lcCntr
            ENDFOR
          ENDIF
        CASE &lcMastPoLn..TRANCD $ '245'
          lcGroup = IIF(TRANCD='4',IIF(cStyGrade='2','4','6'),TRANCD)
          IF !SEEK(&lcMastPoLn..STYLE+STR(&lcMastPoLn..LINENO,6)+lcGroup)
            APPEND BLANK
            REPLACE CBUSDOCU  WITH &lcMastPoLn..CBUSDOCU,;
              CSTYTYPE  WITH &lcMastPoLn..CSTYTYPE,;
              PO        WITH &lcMastPoLn..PO,;
              STYLE     WITH &lcMastPoLn..STYLE,;
              cRetSty   WITH &lcMastPoLn..cRetSty,;
              LINENO    WITH &lcMastPoLn..LINENO,;
              Dyelot    WITH &lcMastPoLn..Dyelot,;
              cWareCode WITH &lcMastPoLn..cWareCode,;
              TRANCD    WITH '9',;
              GROUP     WITH lcGroup,;
              REFERENCE WITH IIF(lcGroup='2','Received',IIF(lcGroup='4','2nd Quality',IIF(lcGroup='6','Damaged','Cancelled')))
          ENDIF

          FOR lnCntr = 1 TO 8
            lcCntr = STR(lnCntr,1)
            REPLACE Qty&lcCntr WITH Qty&lcCntr + &lcMastPoLn..Qty&lcCntr,;
              TOTQTY     WITH TOTQTY     + &lcMastPoLn..Qty&lcCntr
          ENDFOR
          IF SEEK(&lcMastPoLn..STYLE+STR(&lcMastPoLn..LINENO,6)+'1')
            FOR lnCntr = 1 TO 8
              lcCntr = STR(lnCntr,1)
              REPLACE Qty&lcCntr WITH MAX(Qty&lcCntr - &lcMastPoLn..Qty&lcCntr,0),;
                TOTQTY     WITH MAX(TOTQTY - &lcMastPoLn..Qty&lcCntr,0)
            ENDFOR
          ENDIF
        CASE &lcMastPoLn..TRANCD $ '3'
          IF !SEEK(&lcMastPoLn..STYLE+STR(&lcMastPoLn..LINENO,6)+'3')
            APPEND BLANK
            REPLACE CBUSDOCU  WITH &lcMastPoLn..CBUSDOCU,;
              CSTYTYPE  WITH &lcMastPoLn..CSTYTYPE,;
              PO        WITH &lcMastPoLn..PO,;
              STYLE     WITH &lcMastPoLn..STYLE,;
              LINENO    WITH &lcMastPoLn..LINENO,;
              Dyelot    WITH &lcMastPoLn..Dyelot,;
              cWareCode WITH &lcMastPoLn..cWareCode,;
              TRANCD    WITH '9',;
              GROUP     WITH '3',;
              REFERENCE WITH 'Shipment'
          ENDIF
          FOR lnCntr = 1 TO 8
            lcCntr = STR(lnCntr,1)
            REPLACE Qty&lcCntr WITH Qty&lcCntr + &lcMastPoLn..Qty&lcCntr,;
              TOTQTY     WITH TOTQTY     + &lcMastPoLn..Qty&lcCntr
          ENDFOR
        CASE &lcMastPoLn..TRANCD $ '6'
          IF !SEEK(&lcMastPoLn..STYLE+STR(&lcMastPoLn..LINENO,6)+'4')
            APPEND BLANK
            REPLACE CBUSDOCU  WITH &lcMastPoLn..CBUSDOCU,;
              CSTYTYPE  WITH &lcMastPoLn..CSTYTYPE,;
              PO        WITH &lcMastPoLn..PO,;
              STYLE     WITH &lcMastPoLn..STYLE,;
              LINENO    WITH &lcMastPoLn..LINENO,;
              Dyelot    WITH &lcMastPoLn..Dyelot,;
              cWareCode WITH &lcMastPoLn..cWareCode,;
              TRANCD    WITH '9',;
              GROUP     WITH '4',;
              REFERENCE WITH 'Issued'
          ENDIF
          FOR lnCntr = 1 TO 8
            lcCntr = STR(lnCntr,1)
            REPLACE Qty&lcCntr WITH Qty&lcCntr + &lcMastPoLn..Qty&lcCntr,;
              TOTQTY     WITH TOTQTY     + &lcMastPoLn..Qty&lcCntr
          ENDFOR
        CASE &lcMastPoLn..TRANCD $ '7'
          IF !SEEK(&lcMastPoLn..STYLE+STR(&lcMastPoLn..LINENO,6)+'5')
            APPEND BLANK
            REPLACE CBUSDOCU  WITH &lcMastPoLn..CBUSDOCU,;
              CSTYTYPE  WITH &lcMastPoLn..CSTYTYPE,;
              PO        WITH &lcMastPoLn..PO,;
              STYLE     WITH &lcMastPoLn..STYLE,;
              LINENO    WITH &lcMastPoLn..LINENO,;
              Dyelot    WITH &lcMastPoLn..Dyelot,;
              cWareCode WITH &lcMastPoLn..cWareCode,;
              TRANCD    WITH '9',;
              GROUP     WITH '5',;
              REFERENCE WITH 'Planned Shipment'
          ENDIF
          FOR lnCntr = 1 TO 8
            lcCntr = STR(lnCntr,1)
            REPLACE Qty&lcCntr WITH Qty&lcCntr + &lcMastPoLn..Qty&lcCntr,;
              TOTQTY     WITH TOTQTY     + &lcMastPoLn..Qty&lcCntr
          ENDFOR

      ENDCASE
      SELECT (lcMastPoLn)
    ENDSCAN
    SELECT (lcTmpPoLn)
    LOCATE
  ENDIF
  =SEEK(lcPOKey,lcMastPoLn)

  *-- To get the related records from the CutPick file in case of PO from SO
  IF USED(loMainWorkOrder.cTmpCutPik)
    SELECT (loMainWorkOrder.cTmpCutPik)
    ZAP
  ENDIF
  LOCAL lcTmpCutPk, llCutPick
  llCutPick = loMainWorkOrder.mpocutpick(IIF(&lcMastPoLn..CSTYTYPE='P','2','1'),&lcMastPoLn..PO)
  lcTmpCutPk =loMainWorkOrder.cCutPick
  loFormSet.llCutPick =llCutPick
  IF llCutPick AND "SO" $ oAriaApplication.CompanyInstalledModules AND;
      &lcMastPoLn..CSTYTYPE $ "PU" AND SEEK(lcPOKey,lcMastPoLn)

    IF !USED(loMainWorkOrder.cTmpCutPik)
      *-Create Temp Cut Pick file.
      SELECT (loMainWorkOrder.cCutPick)
      =AFIELDS(laFStru)
      lnFStru = ALEN(laFStru,1)
      =gfCrtTmp(loMainWorkOrder.cTmpCutPik,@laFStru,'TranCd+cTktNo+cTktLineNo+Order+Style+cOrdLine','Cutpkord')
      SELECT (loMainWorkOrder.cTmpCutPik)
      SET ORDER TO TAG Cutpkord
    ENDIF

    SELECT (loMainWorkOrder.cCutPick)
    SET ORDER TO Cutpkord
    *--Allocated orders lines , Use TranCD='8' for this lines.
    SCAN
      SELECT (lcTmpPoTrn)
      APPEND BLANK
      REPLACE CBUSDOCU WITH &lcMastPoLn..CBUSDOCU,;
        CSTYTYPE WITH &lcMastPoLn..CSTYTYPE,;
        PO       WITH &lcMastPoLn..PO,;
        TRANCD   WITH '8',;
        LINENO   WITH INT(VAL(&lcTmpCutPk..cTktLineno)),;
        Vendor   WITH &lcTmpCutPk..ORDER,;
        STYLE    WITH &lcTmpCutPk..STYLE,;
        GROUP    WITH '8'
      IF gfSEEK('O'+&lcTmpCutPk..ORDER+STR(INT(VAL(&lcTmpCutPk..cOrdLine)),6),'ORDLINE')
        SELECT ORDLINE
        SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TOTQTY,Account,START,COMPLETE TO laQtyar
        SELECT (lcTmpPoTrn)
        GATHER FROM laQtyar FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TOTQTY,Account,START,COMPLETE
        GATHER FROM laQtyar FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
        =gfSEEK(IIF(EMPTY(ORDLINE.STORE),'M'+ORDLINE.Account,'S'+ORDLINE.Account+ORDLINE.STORE),'CUSTOMER')
        REPLACE cAccName  WITH CUSTOMER.STName
      ENDIF

    ENDSCAN

    lcCtPkKey = '2'+ALLTRIM(loFormSet.ariaform1.knPoNo.KeyTextBox.VALUE)+STR(loFormSet.lnLineNo,6)
    lcCutPick  = loMainWorkOrder.cCutPick
    lcTmpCutPk = loMainWorkOrder.cTmpCutPik
    SELECT (lcCutPick)
    SET ORDER TO Cutpkord
    IF SEEK(lcCtPkKey,lcCutPick)
      SCAN REST WHILE TRANCD+cTktNo+cTktLineno+ORDER+STYLE+cOrdLine = lcCtPkKey
        SCATTER MEMVAR
        IF !SEEK(lcCtPkKey+&lcCutPick..ORDER+&lcCutPick..STYLE+&lcCutPick..cOrdLine,lcTmpCutPk )
          INSERT INTO (lcTmpCutPk) FROM MEMVAR
        ENDIF
      ENDSCAN
    ENDIF


  ENDIF
  GO TOP IN (lcTmpPoTrn)


*!*************************************************************
*! Name      : lfvSzQty
*: Developer : Mariam Mazhar
*: Date      : 02/25/2010
*! Purpose   : Validate Size QTY
*!*************************************************************
FUNCTION lfvSzQty
  PARAMETERS lnSize, llChkBySiz,loFormSet
  lcScrPoLn = loFormSet.lcposln
  PRIVATE lcMastPoLn, lcPoNo, lcTmpCutPk, lcPoLine, lcPoStatus
  LOCAL laOldECst, laNewECst, laOldFCst, laNewFCst, lnCnt, lnI
  loMainWorkOrder = loFormSet.ariaform1.mainworkorder1
  DIMENSION laOldECst[7], laNewECst[7], laOldFCst[7], laNewFCst[7]
  STORE 0 TO laOldECst, laNewECst, laOldFCst, laNewFCst
  lcSz = STR(lnSize,1)
  *-- Get the multi currency set up
  llMultiCur = loMainWorkOrder.lMultiCurrency
  *-- Master POSLN
  lcMastPoLn  = loMainWorkOrder.cPosLn
  *-- Cutpick file
  lcTmpCutPk  = loMainWorkOrder.cCutPick
  *-- PO No. & Status
  lcPoNo      = PADR(loFormSet.ariaform1.knPoNo.KeyTextBox.VALUE,6)
  lcPoStatus  = POSHDR.STATUS
  *-- Temporary POSLN
  lcPoLine    = loMainWorkOrder.cPoLine
  *-- Get the price currency, price rate and duty rate
  lcPriceCurr = POSHDR.CPRICECUR
  lnPricRate  = POSHDR.NPRICERAT
  lcDutyCurr  = POSHDR.CDUTYCUR
  =SEEK(loFormSet.lcStyle+STR(loFormSet.lnLineNo,6),lcPoLine)
  IF EOF(lcPoLine)
    RETURN
  ENDIF
  IF llChkBySiz
    SELECT(lcPoLine)
    m.Qty&lcSz = loFormSet.ariaform1.cntCut.txtqty&lcSz..VALUE

    *-Cannot accept -ve quantity.
    IF loFormSet.ariaform1.cntCut.txtqty&lcSz..VALUE < 0
      =gfModalGen('TRM34051B34000','DIALOG')
      loFormSet.ariaform1.cntCut.txtqty&lcSz..VALUE = &lcPoLine..Qty&lcSz
      RETURN .F.
    ENDIF

    =gfSEEK('S'+&lcPoLine..SCALE,'Scale')
    lnTotQty = 0
    FOR lnCnt = 1 TO SCALE.CNT
      lcCnt = STR(lnCnt,1)
      lnTotQty = lnTotQty + loFormSet.ariaform1.cntCut.txtqty&lcCnt..VALUE
    ENDFOR

  ENDIF
  SELECT(loMainWorkOrder.cposhdr)
  REPLACE nStyOrder WITH nStyOrder - &lcPoLine..Qty&lcSz + m.Qty&lcSz
  IF &lcScrPoLn..REC&lcSz = 0
    REPLACE OPEN WITH MAX(OPEN - &lcPoLine..Qty&lcSz + m.Qty&lcSz  ,0)
  ELSE
*    IF &lcScrPoLn..REC&lcSz <= m.Qty&lcSz
      IF &lcScrPoLn..REC&lcSz <= &lcPoLine..Qty&lcSz 
        REPLACE OPEN WITH  MAX(OPEN- &lcPoLine..Qty&lcSz + m.Qty&lcSz,0)
      ELSE
        REPLACE OPEN WITH  MAX(OPEN- &lcScrPoLn..REC&lcSz + m.Qty&lcSz,0)
      ENDIF   
*!*      ELSE
*!*        IF &lcScrPoLn..REC&lcSz <= &lcPoLine..Qty&lcSz 
*!*          REPLACE OPEN WITH  OPEN- &lcPoLine..Qty&lcSz + m.Qty&lcSz
*!*        ELSE
*!*          REPLACE OPEN WITH  OPEN- &lcScrPoLn..REC&lcSz + m.Qty&lcSz
*!*        ENDIF    
*!*      ENDIF                         
  ENDIF                          
  *-- Calculate the estimated costs in the summary folder
  *-- Pass the old estimated cost, new estimated cost
  *-- old esitmted foreign cost, new estimated foreign cost as an arrays by reference
  FOR lnI = 1 TO 7
    lcI = STR(lnI,1)
    STORE &lcPoLine..nICost&lcI TO laOldECst[lnI], laNewECst[lnI]
    STORE &lcPoLine..nFCost&lcI TO laOldFCst[lnI], laNewFCst[lnI]
  ENDFOR
  loMainWorkOrder.mCalEstCst(m.Qty&lcSz,&lcPoLine..Qty&lcSz,@laOldECst,@laNewECst,;
    @laOldFCst,@laNewFCst,llMultiCur,.T.,lcPriceCurr,lnPricRate,;
    lcDutyCurr)

  SELECT (lcPoLine)
  REPLACE Qty&lcSz WITH m.Qty&lcSz ,;
          TOTQTY   WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8


*!*************************************************************
*! Name      : lfSavePoCut
*: Developer : Mariam Mazhar
*: Date      : 02/25/2010
*! Purpose   : Saving
*!*************************************************************
FUNCTION lfSavePoCut
PARAMETERS loFormSet
lcPoNo = PADR(loFormSet.ariaform1.knPoNo.KeyTextBox.VALUE,6)
loMainWorkOrder = loFormSet.ariaform1.mainworkorder1
lcTmpPoLn  = loMainWorkOrder.cPoLine

DO CASE
  * PO HAS STATUS 'O' AT TIME OF ENTERING CUT QUANTITIES. 
  * NO CUT QUANTITIES HAVE PREVIOUSLY BEEN ENTERED MEANING THAT NO POCUT RECORDS EXIST FOR THE PO 
  * AND NO PO RECEIPTS HAVE BEEN ENTERED I.E. NO POSLN.TRANCD = 2 RECORDS EXIST
  CASE  !gfSEEK(lcPoNo +loFormSet.lcStyle+STR(loFormSet.lnLineNo,6),'POCUT')
*     AND EVALUATE(loFormSet.lcposln+'.TOTREC') = 0
    m.PO = lcPoNo
    m.Style = loFormSet.lcStyle
    m.LINENO = loFormSet.lnLineNo
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      m.NWO&lcI. = EVALUATE(loFormSet.lcposln+'.Qty'+lcI)
      m.CUT&lcI. = EVALUATE(loFormSet.lcposln+'.Cut'+lcI)
    ENDFOR
    m.NTOTWO =  EVALUATE(loFormSet.lcposln+'.totQty')
    m.TOTCUT =  EVALUATE(loFormSet.lcposln+'.totCut')
    SELECT 'POCUT'
    =gfAppend('POCUT',.T.)
    =gfAdd_Info('POCUT')
    =gfReplace('')
    =gfTableUpdate()
*      REPLACE STATUS WITH 'A' IN  SELECT(loMainWorkOrder.cposhdr)
    
 CASE POSHDR.STATUS $ 'ACO' AND gfSEEK(lcPoNo +loFormSet.lcStyle+STR(loFormSet.lnLineNo,6),'POCUT')
*  AND  EVALUATE(loFormSet.lcposln+'.TOTREC') = 0
    SELECT POCUT
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      REPLACE CUT&lcI. WITH  EVALUATE(loFormSet.lcposln+'.Cut'+lcI)
    ENDFOR
    REPLACE TOTCUT WITH  EVALUATE(loFormSet.lcposln+'.totCut')
    =gfAdd_Info('POCUT')
    =gfReplace('')
    =gfTableUpdate()        
    
  *PO HAS STATUS 'A'.  CUT QUANTITIES (I.E. POCUT RECORDS ALREADY EXIST).  PO RECEIPTS HAVE BEEN ENTERED
*!*    CASE POSHDR.STATUS = 'A' AND gfSEEK(lcPoNo +loFormSet.lcStyle+STR(loFormSet.lnLineNo,6),'POCUT') AND ;
*!*          EVALUATE(loFormSet.lcposln+'.TOTREC') > 0
*!*        SELECT POCUT
*!*        FOR lnI = 1 TO 8
*!*          lcI = STR(lnI,1)
*!*          REPLACE CUT&lcI. WITH  EVALUATE(loFormSet.lcposln+'.Cut'+lcI)
*!*        ENDFOR
*!*        REPLACE TOTCUT WITH  EVALUATE(loFormSet.lcposln+'.totCut')
*!*        =gfAdd_Info('POCUT')
*!*        =gfReplace('')
*!*        =gfTableUpdate()        

*!*        
*!*    CASE POSHDR.STATUS = 'C' AND gfSEEK(lcPoNo +loFormSet.lcStyle+STR(loFormSet.lnLineNo,6),'POCUT') AND ;
*!*          EVALUATE(loFormSet.lcposln+'.TOTREC') > 0 AND POSHDR.OPEN = 0
*!*          
*!*  * PO HAS STATUS 'C'. POCUT RECORDS ALREADY EXIST. PO RECEIPTS HAVE BEEN ENTERED AND THE PO OPEN QUANTITY = ZERO   

*!*        SELECT POCUT
*!*        FOR lnI = 1 TO 8
*!*          lcI = STR(lnI,1)
*!*          REPLACE CUT&lcI. WITH  EVALUATE(loFormSet.lcposln+'.Cut'+lcI)
*!*        ENDFOR
*!*        REPLACE TOTCUT WITH  EVALUATE(loFormSet.lcposln+'.totCut')
*!*        =gfAdd_Info('POCUT')
*!*        =gfReplace('')
*!*        =gfTableUpdate()        
*!*        REPLACE STATUS WITH IIF(OPEN = 0,'C','A') IN  SELECT(loMainWorkOrder.cposhdr)      
ENDCASE
REPLACE STATUS WITH IIF(OPEN = 0,'C','A') IN  SELECT(loMainWorkOrder.cposhdr)      
SELECT POSLN
IF SEEK(loFormSet.lcStyle+STR(loFormSet.lnLineNo,6),lcTmpPoLn)
  SELECT(lcTmpPoLn)
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    REPLACE Qty&lcI. WITH  EVALUATE(loFormSet.lcposln+'.Cut'+lcI)
  ENDFOR
  REPLACE TOTQTY WITH EVALUATE(loFormSet.lcposln+'.TotCut')
ENDIF
lnallQty = 0
IF loFormSet.llCutPick AND "SO" $ oAriaApplication.CompanyInstalledModules
  FOR lnD = 1  TO 8
    lnCutPikTot = 0
    lcD = STR(lnD,1)
    lnOldQty = EVALUATE(loFormSet.lcposln+'.Qty'+lcD)
    lnNewQty = EVALUATE(loFormSet.lcposln+'.Cut'+lcD)
    SELECT (loMainWorkOrder.cTmpCutPik)
    =SEEK('2' + lcPoNo + STR(loFormSet.lnLineNo,6))
    SUM Qty&lcD. REST WHILE TRANCD+cTktNo+cTktLineno+ORDER+STYLE+cOrdLine = '2' + lcPoNo + STR(loFormSet.lnLineNo,6) TO lnCutPikTot
    LOCATE
    lnRemQty = lnNewQty
    IF lnNewQty < lnCutPikTot
      =SEEK('2' + lcPoNo + STR(loFormSet.lnLineNo,6))
      SCAN REST WHILE TRANCD+cTktNo+cTktLineno+ORDER+STYLE+cOrdLine = '2' + lcPoNo + STR(loFormSet.lnLineNo,6)
        lnPerCent = Qty&lcD./lnCutPikTot
        lnNewTotQty = ROUND(lnPerCent * lnNewQty,0)
        lnNewTotQty = MAX(MIN(lnRemQty ,lnNewTotQty),0)
        lnRemQty  = lnRemQty - lnNewTotQty
        REPLACE Qty&lcD. WITH lnNewTotQty
        REPLACE TOTQTY WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
      ENDSCAN
      IF lnRemQty > 0
        lnCutPikTot = 0
        =SEEK('2' + lcPoNo + STR(loFormSet.lnLineNo,6))
        SUM Qty&lcD. REST WHILE TRANCD+cTktNo+cTktLineno+ORDER+STYLE+cOrdLine = '2' + lcPoNo + STR(loFormSet.lnLineNo,6) TO lnCutPikTot
        =SEEK('2' + lcPoNo + STR(loFormSet.lnLineNo,6))
        SCAN REST WHILE TRANCD+cTktNo+cTktLineno+ORDER+STYLE+cOrdLine = '2' + lcPoNo + STR(loFormSet.lnLineNo,6)
          lnPerCent = Qty&lcD./lnCutPikTot
          lnQtyToAdd = CEILING((Qty&lcD./lnCutPikTot)*lnRemQty)
          lnRemQty = lnRemQty - lnQtyToAdd
          REPLACE Qty&lcD. WITH Qty&lcD. + MAX(lnQtyToAdd ,0)
          REPLACE TOTQTY WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
          IF lnRemQty <= 0
            EXIT
          ENDIF
        ENDSCAN
      ENDIF
    ENDIF
  ENDFOR
  lcTmpPoHdr = loMainWorkOrder.cposhdr
  IF SEEK('2'+lcPoNo+STR(loFormSet.lnLineNo,6),loMainWorkOrder.cCutPick)
    lcCutPikFl = loMainWorkOrder.cCutPick
    lcCutPkKey = '2'+lcPoNo+STR(loFormSet.lnLineNo,6)
    SELECT (lcCutPikFl)
    SCAN REST WHILE TRANCD+cTktNo+cTktLineno+ORDER+STYLE+cOrdLine = lcCutPkKey
      =gfSEEK('O'+&lcCutPikFl..ORDER,'ORDHDR')
      SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TOTQTY TO laOldOrd
      LOCAL laNewOrd
      DIMENSION laNewOrd[9]
      laNewOrd = 0
      IF USED(loMainWorkOrder.cTmpCutPik) AND SEEK(lcCutPkKey+&lcCutPikFl..ORDER+&lcCutPikFl..STYLE+&lcCutPikFl..cOrdLine,loMainWorkOrder.cTmpCutPik)
        SELECT(loMainWorkOrder.cTmpCutPik)
        FOR lnForSz = 1 TO 8
          lcForSz = STR(lnForSz,1)
          laNewOrd[lnForSz] = Qty&lcForSz
        ENDFOR
        laNewOrd[9] = TOTQTY
      ELSE
        =ACOPY(laOldOrd,laNewOrd)
      ENDIF
      SELECT (lcCutPikFl)
      GATHER FROM laNewOrd FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TOTQTY
      =gfSEEK('O'+&lcCutPikFl..ORDER,'ORDHDR')
      =gfSEEK('O'+&lcCutPikFl..ORDER+STR(INT(VAL(&lcCutPikFl..cOrdLine)),6),'ORDLINE')
      FOR lnSizeNo = 1 TO 8
        lcSz = STR(lnSizeNo,1)
        REPLACE ORDHDR.TOTCUT       WITH ORDHDR.TOTCUT  - laOldOrd[lnSizeNo] + laNewOrd[lnSizeNo],;
                ORDLINE.TOTCUT      WITH ORDLINE.TOTCUT - laOldOrd[lnSizeNo] + laNewOrd[lnSizeNo],;
                ORDLINE.CUT&lcSz    WITH ORDLINE.CUT&lcSz - laOldOrd[lnSizeNo] + laNewOrd[lnSizeNo],;
                &lcTmpPoHdr..TotOrd WITH &lcTmpPoHdr..TotOrd - laOldOrd[lnSizeNo] + laNewOrd[lnSizeNo]
                
         REPLACE &lcTmpPoLn..ORD&lcSz WITH &lcTmpPoLn..ORD&lcSz- laOldOrd[lnSizeNo] + laNewOrd[lnSizeNo],;
                  &lcTmpPoLn..TOTORD WITH &lcTmpPoLn..ORD1+&lcTmpPoLn..ORD2+&lcTmpPoLn..ORD3+&lcTmpPoLn..ORD4+;
                                         &lcTmpPoLn..ORD5+&lcTmpPoLn..ORD6+&lcTmpPoLn..ORD7+&lcTmpPoLn..ORD8
          
      ENDFOR
    ENDSCAN
    IF SEEK ('2'+lcPoNo+STR(loFormSet.lnLineNo,6))
      DELETE REST WHILE TRANCD+cTktNo+cTktLineno+ORDER+STYLE+cOrdLine = lcCutPkKey;
        FOR TOTQTY = 0
    ENDIF
    SELECT(loMainWorkOrder.cCutPick)
    IF loMainWorkOrder.mupdatesql(loMainWorkOrder.oCutPickCon,loMainWorkOrder.cCutPick,;
                       'TranCd,cTktNo,cTktLineNo,Order,Style,cOrdLine','CUTPICK','CUTPKORD')
      =TABLEUPDATE(.T.,.T.)
    ELSE
      =TABLEREVERT(.T.)
    ENDIF          
  ENDIF
  
ENDIF

lcPrjValue = loMainWorkOrder.lcgenproj
loMainWorkOrder.lcgenproj = ''
loMainWorkOrder.mSavePO (.F.,.F.)
SELECT ORDHDR
=gfTableUpdate()
SELECT ORDLINE
=gfTableUpdate()
loMainWorkOrder.lcgenproj = lcPrjValue
