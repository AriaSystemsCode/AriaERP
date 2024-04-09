*:**********************************************************************************************
*: Program file  : posplit.PRG 
*: Program desc. : Po Split program for GPS
*: System        : Aria 4 XP
*: Module        : PO
*: Developer     : Mariam Mazhar[MMT]
*: Date          : 11/03/2008 
*: Refer to      : [T20061128.0005] C200963
*:**********************************************************************************************
*: Modifications:
*: B608686,1 MMT 09/11/2008 Fix bug of not displaying lines of the last and first PO in file[T20061128.0005]
*: B608686,2 MMT 09/11/2008 Fix bug of not locate in PO browse [T20080924.0006]
*:**********************************************************************************************
DO FORM (oAriaApplication.ScreenHome+"\PO\posplit.SCX") 
RETURN
*!*************************************************************
*! Name      : lfChngMode
*! Developer : Mariam Mazhar (MMT)
*! Date      : 11/03/2008
*! Purpose   : Change Mode Function
*!*************************************************************
FUNCTION lfChngMode
PARAMETERS loFormSet

DO CASE 

CASE  loFormSet.ActiveMode = 'E'
  
  WITH loFormSet.ariaform1
    .lblDots.Visible = .F.
    .knPoNo.Enabled = .F. 
    .kbnEwSup.Enabled = .T. 
    .cntSpQty.Enabled= .F. 
    .cntQtyBal.Enabled= .F. 
    .kbnEwSup.KeyTextBox.SetFOcus()
    .lblSclDesc.Visible = .T.
    .chkSpltAll.Enabled = .T.
  ENDWITH  
  
CASE  loFormSet.ActiveMode = 'S'
  
  WITH loFormSet.ariaform1
    .lblDots.Visible = .F.
    .lblSclDesc.Visible = .F.
    .knPoNo.Enabled = .T. 
    .knPoNo.KeyTextBox.Value = ''
    .kbnEwSup.Enabled = .F. 
    .kbnEwSup.KeyTextBox.Value = ''
    .cntSpQty.Enabled= .F. 
    .cntQtyBal.Enabled= .F. 
    .chkSpltAll.Enabled = .F.
  ENDWITH 
  
  IF USED(loFormSet.lcTmpPoHdr) 
    SELECT(loFormSet.lcTmpPoHdr)
    DELETE all
  ENDIF 
  
  IF USED(loFormSet.lcTmpPoLns ) 
    SELECT(loFormSet.lcTmpPoLns )
    DELETE all
  ENDIF 
    
CASE  loFormSet.ActiveMode = 'V'
  WITH loFormSet.ariaform1
    .lblDots.Visible = .T.
    .lblSclDesc.Visible = .T.
    .knPoNo.Enabled = .F. 
    .kbnEwSup.Enabled = .F. 
    .cntSpQty.Enabled= .F. 
    .cntQtyBal.Enabled= .F. 
    .chkSpltAll.Enabled = .F.
  ENDWITH   
  
  lfGetPOData(loFormSet)
  lfAddCntrlSrc(loFormSet)
  

ENDCASE 
loFormSet.ariaform1.grdStydet.readonly = .T.
loFormSet.ariaform1.grdStydet.afterrowcolchange()
*!*************************************************************
*! Name      : lfvPo
*! Developer : Mariam Mazhar (MMT)
*! Date      : 11/03/2008
*! Purpose   : Validate PO Number
*!*************************************************************
FUNCTION lfvPo
PARAMETERS loFormSet
LOCAL lcPoNo
lcPoNo = PADR(loFormSet.ariaform1.knPoNo.keytextbox.Value,6)

SELECT POSHDR
IF (EMPTY(lcPoNo) AND loFormSet.ariaform1.knPoNo.selectedfrombrowse) OR ;
  (AT('?',ALLTRIM(lcPoNo),1) > 0) OR;
  (!EMPTY(lcPoNo) AND !gfSEEK('PP'+lcPoNo)) OR ;
  loFormSet.ariaform1.knPoNo.selectedfrombrowse
  
  lcBrFields = [Po :R :H= 'P/O#' , STATUS :R :H= 'S', Vendor :R :H= 'Vendor',] +;
                   [Entered :R :H= 'Entered' , Complete :R :H= 'Complete', ] +;
                   [nStyorder :R :H= 'Tot. Qty.' , POtotal :R :H= 'Amount', ;
                   Receive  :R :H= 'Receive',Open :R :H= 'Open']
                             
  IF !loFormSet.ariaform1.knPoNo.selectedfrombrowse
    loFormSet.ariaform1.knPoNo.selectedfrombrowse = .T.
  ENDIF 

  IF loFormSet.ariaform1.knPoNo.selectedfrombrowse
    lcIndexSel = "POSHDR"   
    
    *: B608686,2 MMT 09/11/2008 Fix bug of not locate in PO browse [Start]
    *lcFilter = " FOR  cbusdocu+Cstytype = 'PP' AND  (POSHDR.STATUS = 'H') AND POSHDR.[Open] > 0 AND POSHDR.PARENTPO = ''"
    lcFilter = "'PP' FOR  cbusdocu+Cstytype = 'PP' AND  (POSHDR.STATUS = 'H') AND POSHDR.[Open] > 0 AND POSHDR.PARENTPO = ''"
    *: B608686,2 MMT 09/11/2008 Fix bug of not locate in PO browse [End]
    
   
    
    lcPoNo = IIF(ARIABROW(lcFilter,"PO",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, ;
                         gnBrFSCol2,'','','POSHDR','laBrowArr', .F.,'',.F.,'POSHDR','','',lcIndexSel),POSHDR.PO,SPACE(6))


    loFormSet.ariaform1.knPoNo.keytextbox.Value = lcPoNo
    loFormSet.ariaform1.knPoNo.selectedfrombrowse = .F.
    RETURN .T.
  ENDIF  
ELSE
  IF gfSEEK('PP'+lcPoNo)
     loFormSet.ariaform1.knPoNo.selectedfrombrowse = .F.
    RETURN .T.        
  ENDIF  
ENDIF 
ENDFUNC  
*!*************************************************************
*! Name      : lfGetPOData
*! Developer : Mariam Mazhar (MMT)
*! Date      : 11/03/2008
*! Purpose   : get PO data
*!*************************************************************
FUNCTION   lfGetPOData
PARAMETERS loFormSet

IF USED(loFormSet.lcTmpPoHdr) 
  SELECT(loFormSet.lcTmpPoHdr)
  DELETE all
ENDIF 
  
IF USED(loFormSet.lcTmpPoLns ) 
  SELECT(loFormSet.lcTmpPoLns )
  DELETE all 
ENDIF 

*: B608686,1 MMT 09/11/2008 Fix bug of not displaying lines of the last and first PO in file[Start]
*lcPoNO = loFormSet.ariaform1.knPoNo.keytextbox.Value
lcPoNO = ALLTRIM(loFormSet.ariaform1.knPoNo.keytextbox.Value)
*: B608686,1 MMT 09/11/2008 Fix bug of not displaying lines of the last and first PO in file[End]

lcTmpPoHdr = loFormSet.lcTmpPoHdr
lcTmpPoLns = loFormSet.lcTmpPoLns

IF loFormSet.ActiveMode = 'V' AND !EMPTY(lcPoNO )
  STORE 0 TO SpltQty1 , SpltQty2 ,SpltQty3,SpltQty4,SpltQty5,SpltQty6,SpltQty7,SpltQty8,SpltTQty
  SELECT POSHDR
  IF gfSEEK('PP'+lcPoNO)
    SCATTER MEMVAR MEMO
    SELECT (lcTmpPoHdr)
    APPEND BLANK
    GATHER MEMVAR MEMO
    loFormSet.ariaform1.txtOrgSupp.Value = &lcTmpPoHdr..vendor
    SELECT POSLN
    =gfSEEK('PP'+lcPoNO)
    SCAN REST WHILE cbusdocu+cstytype+po+style+STR(lineno,6)+trancd = 'PP'+lcPoNO FOR trancd = '1'
      SCATTER MEMVAR MEMO 
      SELECT (lcTmpPoLns)
      APPEND BLANK
      GATHER MEMVAR MEMO
      SELECT POSLN
    ENDSCAN
    SELECT POSLN
    =gfSEEK('PP'+lcPoNO)
    SCAN REST WHILE cbusdocu+cstytype+po+style+STR(lineno,6)+trancd = 'PP'+lcPoNO  FOR trancd = '5'    
      SELECT (lcTmpPoLns)
      =SEEK('PP'+lcPoNO+POSLN.style)
      FOR I = 1 TO 8
        LCI =ALLTRIM(STR(I))
        REPLACE &lcTmpPoLns..QTY&LCI  WITH  &lcTmpPoLns..QTY&LCI - POSLN.QTY&LCI
      ENDFOR
      REPLACE &lcTmpPoLns..TotQTY  WITH  &lcTmpPoLns..TotQTY  - POSLN.TotQTY
    ENDSCAN
  ENDIF 
  SELECT (lcTmpPoLns)
  LOCATE
ENDIF 

*!*************************************************************
*! Name      : lfInit
*! Developer : Mariam Mazhar (MMT)
*! Date      : 11/03/2008
*! Purpose   : Init Function of screen
*!*************************************************************
FUNCTION lfInit
PARAMETERS loFormSet

loFormSet.lcTmpPoHdr = gfTempName()
loFormSet.lcTmpPoLns = gfTempName()

SET MULTILOCKS ON
=gfOpenTable(oAriaApplication.DataDir+'Scale',oAriaApplication.DataDir+'Scale','SH')
=gfOpenTable(oAriaApplication.DataDir+'Style',oAriaApplication.DataDir+'Style','SH')
=gfOpenTable(oAriaApplication.DataDir+'POSLN',oAriaApplication.DataDir+'POSLN','SH')
=gfOpenTable(oAriaApplication.DataDir+'POSHDR',oAriaApplication.DataDir+'POSHDR','SH')
=gfOpenTable(oAriaApplication.DataDir+'APVENDOR',oAriaApplication.DataDir+'VENCODE','SH')

lfCreateTemp(loFormSet)


WITH loFormSet
  .cbrowsetabledbengine   = "SQL"
  .nWorkArea        = 'POSHDR'
  .DataEnvironment.InitialSelectedAlias = 'POSHDR'
  .cBrowseFileName        = "POSHDR"
  .cBrowseIndexExpression = "CBUSDOCU+CSTYTYPE+PO"
  .cBrowseIndexFields     = "CBUSDOCU,CSTYTYPE,PO"
  .cBrowseIndexName       = "POSHDR"
  .cBrowseAliasName       = "POSHDR"
  .cBrowseTableName       = "POSHDR"
  .cBrowseFilter          = "CBUSDOCU+CSTYTYPE = 'PP'  AND  (POSHDR.STATUS = 'H') AND POSHDR.[Open] > 0 AND POSHDR.PARENTPO = ''"
  .BrowseTitle           ="PO"
ENDWITH 
*!*************************************************************
*! Name      : lfCreateTemp
*! Developer : Mariam Mazhar (MMT)
*! Date      : 11/03/2008
*! Purpose   : Create Temp Files
*!*************************************************************
FUNCTION lfCreateTemp
PARAMETERS loFormSet

SELECT POSHDR
DIMENSION laFileStru[1,18]
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,18]
laFileStru[lnFileStru+1,1] = 'NewVend'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 8
laFileStru[lnFileStru+1,4] = 0

STORE '' TO laFileStru[lnFileStru+1,7],laFileStru[lnFileStru+1,8],laFileStru[lnFileStru+1,9],;
      laFileStru[lnFileStru+1,10],laFileStru[lnFileStru+1,11],laFileStru[lnFileStru+1,12],;
        laFileStru[lnFileStru+1,13],laFileStru[lnFileStru+1,14],laFileStru[lnFileStru+1,15],;
        laFileStru[lnFileStru+1,16]
STORE 0 TO  laFileStru[lnFileStru+1,17],laFileStru[lnFileStru+1,18]

=gfCrtTmp(loFormSet.lcTmpPoHdr,@laFileStru,"cbusdocu+Cstytype+po",loFormSet.lcTmpPoHdr,.T.)


SELECT POSLN
DIMENSION laFileStru[1,18]
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+10,18]
FOR I = 1 TO 8
  LCI =ALLTRIM(STR(I))
  laFileStru[lnFileStru+I,1] = 'SpltQty&LCI'
  laFileStru[lnFileStru+I,2] = 'N'
  laFileStru[lnFileStru+I,3] = 6
  laFileStru[lnFileStru+I,4] = 0
  
  STORE '' TO laFileStru[lnFileStru+I,7],laFileStru[lnFileStru+I,8],laFileStru[lnFileStru+I,9],;
    laFileStru[lnFileStru+I,10],laFileStru[lnFileStru+I,11],laFileStru[lnFileStru+I,12],;
    laFileStru[lnFileStru+I,13],laFileStru[lnFileStru+I,14],laFileStru[lnFileStru+I,15],;
    laFileStru[lnFileStru+I,16]
  STORE 0 TO  laFileStru[lnFileStru+I,17],laFileStru[lnFileStru+I,18]

ENDFOR

laFileStru[lnFileStru+9,1] = 'SpltTQty'
laFileStru[lnFileStru+9,2] = 'N'
laFileStru[lnFileStru+9,3] = 7
laFileStru[lnFileStru+9,4] = 0

STORE '' TO laFileStru[lnFileStru+9,7],laFileStru[lnFileStru+9,8],laFileStru[lnFileStru+9,9],;
    laFileStru[lnFileStru+9,10],laFileStru[lnFileStru+9,11],laFileStru[lnFileStru+9,12],;
    laFileStru[lnFileStru+9,13],laFileStru[lnFileStru+9,14],laFileStru[lnFileStru+9,15],;
    laFileStru[lnFileStru+9,16]
STORE 0 TO  laFileStru[lnFileStru+9,17],laFileStru[lnFileStru+9,18]


laFileStru[lnFileStru+10,1] = 'llSplt'
laFileStru[lnFileStru+10,2] = 'L'
laFileStru[lnFileStru+10,3] = 1
laFileStru[lnFileStru+10,4] = 0

STORE '' TO laFileStru[lnFileStru+10,7],laFileStru[lnFileStru+10,8],laFileStru[lnFileStru+10,9],;
    laFileStru[lnFileStru+10,10],laFileStru[lnFileStru+10,11],laFileStru[lnFileStru+10,12],;
    laFileStru[lnFileStru+10,13],laFileStru[lnFileStru+10,14],laFileStru[lnFileStru+10,15],;
    laFileStru[lnFileStru+10,16]
STORE 0 TO  laFileStru[lnFileStru+10,17],laFileStru[lnFileStru+10,18]


=gfCrtTmp(loFormSet.lcTmpPoLns,@laFileStru,"cbusdocu+Cstytype+po+style+STR(lineno,6)+trancd",loFormSet.lcTmpPoLns,.T.)
lfAddCntrlSrc(loFormSet)

*!*************************************************************
*! Name      : lfCreateTemp
*! Developer : Mariam Mazhar (MMT)
*! Date      : 11/03/2008
*! Purpose   : add Control source
*!*************************************************************
FUNCTION lfAddCntrlSrc
PARAMETERS loFormSet

WITH loFormSet.ariaform1.grdStydet
  .RecordSource = ''
  .ColumnCount = 10
  .RecordSource = loFormSet.lcTmpPoLns
  .readonly = .T.

  .Column1.controlSource = 'Style'
  .Column1.Width  = 130
  .Column1.Header1.Caption = 'Style   - Color'

  .Column2.controlSource = 'QTY1'
  .Column2.Width  = 52
  .Column2.InputMask  = '99999'
  .Column2.FormAt= '999999'
  .Column2.Header1.Caption = 'Qty1'

  .Column3.controlSource = 'QTY2'
  .Column3.Width  = 52
  .Column3.InputMask  = '99999'
  .Column3.FormAt= '999999'
  .Column3.Header1.Caption = 'Qty2'

  .Column4.controlSource = 'QTY3'
  .Column4.Width  = 52
  .Column4.InputMask  = '99999'
  .Column4.FormAt= '999999'
  .Column4.Header1.Caption = 'Qty3'

  .Column5.controlSource = 'QTY4'
  .Column5.Width  = 52
  .Column5.FormAt= '999999'
  .Column5.InputMask  = '99999'
  .Column5.Header1.Caption = 'Qty4'

  .Column6.controlSource = 'QTY5'
  .Column6.Width  = 52
  .Column6.InputMask  = '99999'
  .Column6.FormAt= '999999'
  .Column6.Header1.Caption = 'Qty5'

  .Column7.controlSource = 'QTY6'
  .Column7.Width  = 52
  .Column7.InputMask  = '99999'
  .Column7.FormAt= '999999'
  .Column7.Header1.Caption = 'Qty6'

  .Column8.controlSource = 'QTY7'
  .Column8.Width  = 52
  .Column8.InputMask  = '99999'
  .Column8.FormAt= '999999'
  .Column8.Header1.Caption = 'Qty7'

  .Column9.controlSource = 'QTY8'
  .Column9.Width  = 52
  .Column9.InputMask  = '99999'
  .Column9.FormAt= '999999'
  .Column9.Header1.Caption = 'Qty8'

  .Column10.controlSource = 'TOTQTY'
  .Column10.Width  = 75
  .Column10.InputMask  = '999999'
  .Column10.FormAt= '999999'
  .Column10.Header1.Caption = 'TotQty'

  .refresh 

ENDWITH 
WITH loFormSet.ariaform1.cntQtyBal
  .txtQty1.ControlSource    = loFormSet.lcTmpPoLns + '.Qty1'
  .txtQty2.ControlSource    = loFormSet.lcTmpPoLns + '.Qty2'
  .txtQty3.ControlSource    = loFormSet.lcTmpPoLns + '.Qty3'
  .txtQty4.ControlSource    = loFormSet.lcTmpPoLns + '.Qty4'
  .txtQty5.ControlSource    = loFormSet.lcTmpPoLns + '.Qty5'
  .txtQty6.ControlSource    = loFormSet.lcTmpPoLns + '.Qty6'
  .txtQty7.ControlSource    = loFormSet.lcTmpPoLns + '.Qty7'
  .txtQty8.ControlSource    = loFormSet.lcTmpPoLns + '.Qty8'
  .txtTotQty.ControlSource  = loFormSet.lcTmpPoLns + '.TotQty'
ENDWITH   


WITH loFormSet.ariaform1.cntSpQty
  .txtQty1.ControlSource    = loFormSet.lcTmpPoLns + '.SpltQty1'
  .txtQty2.ControlSource    = loFormSet.lcTmpPoLns + '.SpltQty2'
  .txtQty3.ControlSource    = loFormSet.lcTmpPoLns + '.SpltQty3'
  .txtQty4.ControlSource    = loFormSet.lcTmpPoLns + '.SpltQty4'
  .txtQty5.ControlSource    = loFormSet.lcTmpPoLns + '.SpltQty5'
  .txtQty6.ControlSource    = loFormSet.lcTmpPoLns + '.SpltQty6'
  .txtQty7.ControlSource    = loFormSet.lcTmpPoLns + '.SpltQty7'
  .txtQty8.ControlSource    = loFormSet.lcTmpPoLns + '.SpltQty8'
  .txtTotQty.ControlSource  = loFormSet.lcTmpPoLns + '.SpltTQty'
ENDWITH   

loFormSet.ariaform1.chkSpltAll.ControlSource =  loFormSet.lcTmpPoLns +'.llSplt'


*!*************************************************************
*! Name      : lfvVendor 
*! Developer : Mariam Mazhar (MMT)
*! Date      : 11/03/2008
*! Purpose   : Validate vendor code
*!*************************************************************
FUNCTION lfvVendor 
PARAMETERS loFormSet
LOCAL lcPoNo
lcVenNo = PADR(loFormSet.ariaform1.kbNEwSup.keytextbox.Value,8)

SELECT APVENDOR
IF (EMPTY(lcVenNo) AND loFormSet.ariaform1.kbNEwSup.selectedfrombrowse) OR ;
  (AT('?',ALLTRIM(lcVenNo),1) > 0) OR;
  (!EMPTY(lcVenNo) AND !gfSEEK(lcVenNo)) OR ;
  loFormSet.ariaform1.kbNEwSup.selectedfrombrowse
                           
  IF !loFormSet.ariaform1.kbNEwSup.selectedfrombrowse
    loFormSet.ariaform1.kbNEwSup.selectedfrombrowse = .T.
  ENDIF 

  IF loFormSet.ariaform1.kbNEwSup.selectedfrombrowse
  
    =gfApVnBrow(@lcVenNo,.F.,'S')

    loFormSet.ariaform1.kbNEwSup.keytextbox.Value = lcVenNo
    loFormSet.ariaform1.kbNEwSup.selectedfrombrowse = .F.
    
    RETURN .T.
  ENDIF  
ELSE
  IF gfSEEK(lcVenNo)
     loFormSet.ariaform1.kbNEwSup.selectedfrombrowse = .F.
    RETURN .T.        
  ENDIF  
ENDIF 
*!*************************************************************
*! Name      : lfAftrRwColChng
*! Developer : Mariam Mazhar (MMT)
*! Date      : 11/03/2008
*! Purpose   : after row col. chnaged function of the grid
*!*************************************************************
FUNCTION lfAftrRwColChng
PARAMETERS loFormSet
lcTempL = loFormSet.lcTmpPoLns
gfSeek(&lcTempL..Style ,'Style')
gfSeek('S' + Style.Scale ,'Scale')
loFormSet.ariaform1.KbStyle.Value = &lcTempL..Style
loFormSet.ariaform1.cntSpQty.Scale = Style.Scale
loFormSet.ariaform1.cntQtyBal.Scale = Style.Scale
loFormSet.ariaform1.lblSclDesc.Caption = SUBSTR(Scale.cscl_desc,1,7)

IF loFormSet.ActiveMode = 'E'
  IF EVALUATE(loFormSet.lcTmpPoLns+'.TotQty') > 0
    loFormSet.ariaform1.chkSpltAll.Enabled = .T.
    loFormSet.ariaform1.cntSpQty.Enabled= .T. 
    loFormSet.ariaform1.cntSpQty.mdisableunused
  ELSE
    loFormSet.ariaform1.cntSpQty.Enabled= .F. 
    loFormSet.ariaform1.chkSpltAll.Enabled = .F.
  ENDIF   
  loFormSet.ariaform1.cntSpQty.TXTtotqty.Enabled = .F.
  loFormSet.ariaform1.cntQtyBal.Enabled= .F. 
ENDIF   
loFormSet.ariaform1.chkSpltAll.Value = &lcTempL..llSplt
loFormSet.ariaform1.cntQtyBal.refresh 
loFormSet.ariaform1.cntSpQty.refresh  
loFormSet.ariaform1.cntSpQty.refresh
*!*************************************************************
*! Name      : lfvSplQty
*! Developer : Mariam Mazhar (MMT)
*! Date      : 11/03/2008
*! Purpose   : Validate Split qty
*!*************************************************************
FUNCTION lfvSplQty
PARAMETERS loFormSet,lnTxtNo
lcTxtNo = STR(lnTxtNo,1)
lcTmpPoLns = loFormSet.lcTmpPoLns
lcAlias  = ALIAS()   && Save the current alias.

SELECT (lcTmpPoLns)
IF &lcTmpPoLns..Qty&lcTxtNo < loFormSet.ariaform1.cntSpQty.txtQty&lcTxtNo..Value
  =gfModalGen('TRM44069B40011','DIALOG',.F.,.F.,' Split Qty. can not be greater than balance Qty.')
  
  WITH loFormSet.ariaform1.cntSpQty
    .txtQty&lcTxtNo..Value = 0
    lnSTotQty = .txtQty1.Value + ;
                .txtQty2.Value + ;
                .txtQty3.Value + ;
                .txtQty4.Value + ;
                .txtQty5.Value + ;
                .txtQty6.Value + ;
                .txtQty7.Value + ;
                .txtQty8.Value 
  
     REPLACE &lcTmpPoLns..SpltQty&lcTxtNo WITH 0  , ;
          &lcTmpPoLns..SpltTQty WITH lnSTotQty  
          
    .refresh          
  ENDWITH 
  
  RETURN .F. 
ENDIF

WITH loFormSet.ariaform1.cntSpQty
  lnSTotQty = .txtQty1.Value + ;
              .txtQty2.Value + ;
              .txtQty3.Value + ;
              .txtQty4.Value + ;
              .txtQty5.Value + ;
              .txtQty6.Value + ;
              .txtQty7.Value + ;
              .txtQty8.Value 

  REPLACE &lcTmpPoLns..SpltQty&lcTxtNo WITH .txtQty&lcTxtNo..Value , ;
        &lcTmpPoLns..SpltTQty WITH lnSTotQty
        
  .refresh         
ENDWITH 
        
RETURN .T.
SELECT (lcAlias)  

*!*************************************************************
*! Name      : lfSavScr
*! Developer : Mariam Mazhar (MMT)
*! Date      : 11/03/2008
*! Purpose   : Saving function
*!*************************************************************
FUNCTION lfSavScr
PARAMETERS loFormSet

PRIVATE lnAlias ,lnRecno
DIMENSION laPoLnsQty[9]
STORE .F. TO llContinue 
STORE 0 TO laPoLnsQty
STORE 0 TO lnTotSplit 

lcTmpPoHdr = loFormSet.lcTmpPoHdr
lcTmpPoLns = loFormSet.lcTmpPoLns 

SELECT (lcTmpPoHdr)
lcNewPO = gfSequence('PO','','',&lcTmpPoHdr..CDIVISION,'','P','POSHDR','POSHDR')
lcGlSession = gfsequence('GLSESSION')        

STORE 0 TO lnTCost1,lnTCost2,lnTCost3,lnTCost4,lnTCost5,lnTCost6,lnTCost7,;
           lnTFCost1,lnTFCost2,lnTFCost3,lnTFCost4,lnTFCost5,lnTFCost6,lnTFCost7
           

SELECT (lcTmpPoLns)
LOCATE
lcPoNo = PADR(loFormSet.ariaform1.knPoNo.keytextbox.Value,6)
lcNewVend = PADR(loFormSet.ariaform1.kbNEwSup.keytextbox.Value,8)


IF EMPTY(lcNewVend)
  =gfModalGen('INM00000B00000','','',''," Can't generate PO with empty Supplier ")
  RETURN .F.
ENDIF


SELECT POSLN
=gfSEEK('PP'+lcPoNo) 
SCAN REST WHILE cbusdocu+Cstytype+po+style+STR(lineno,6)+trancd = 'PP'+lcPoNo FOR trancd = '1'
  SCATTER MEMVAR MEMO
  lnRecno = RECNO()
  
  IF &lcTmpPoLns..SpltTQty > 0
    llContinue = .T.
    lnTotSplit = lnTotSplit + &lcTmpPoLns..SpltTQty
    
    lnTCost1 = lnTCost1 + ( &lcTmpPoLns..SpltTQty  *  &lcTmpPoLns..NFCOST1 )
    lnTCost2 = lnTCost2 + ( &lcTmpPoLns..SpltTQty  *  &lcTmpPoLns..NFCOST2 )
    lnTCost3 = lnTCost3 + ( &lcTmpPoLns..SpltTQty  *  &lcTmpPoLns..NFCOST3 )
    lnTCost4 = lnTCost4 + ( &lcTmpPoLns..SpltTQty  *  &lcTmpPoLns..NFCOST4 )
    lnTCost5 = lnTCost5 + ( &lcTmpPoLns..SpltTQty  *  &lcTmpPoLns..NFCOST5 )    
    lnTCost6 = lnTCost6 + ( &lcTmpPoLns..SpltTQty  *  &lcTmpPoLns..NFCOST6 )    
    lnTCost7 = lnTCost7 + ( &lcTmpPoLns..SpltTQty  *  &lcTmpPoLns..NFCOST7 )    
    
    lnTFCost1= lnTFCost1+ ( &lcTmpPoLns..SpltTQty  *  &lcTmpPoLns..NICOST1 )    
    lnTFCost2= lnTFCost2+ ( &lcTmpPoLns..SpltTQty  *  &lcTmpPoLns..NICOST2 )    
    lnTFCost3= lnTFCost3+ ( &lcTmpPoLns..SpltTQty  *  &lcTmpPoLns..NICOST3 )    
    lnTFCost4= lnTFCost4+ ( &lcTmpPoLns..SpltTQty  *  &lcTmpPoLns..NICOST4 )        
    lnTFCost5= lnTFCost5+ ( &lcTmpPoLns..SpltTQty  *  &lcTmpPoLns..NICOST5 )    
    lnTFCost6= lnTFCost6+ ( &lcTmpPoLns..SpltTQty  *  &lcTmpPoLns..NICOST6 )        
    lnTFCost7= lnTFCost7+ ( &lcTmpPoLns..SpltTQty  *  &lcTmpPoLns..NICOST7 )    
    
    gfAppend('POSLN',.T.)
    FOR I = 1 TO 8
      LCI =ALLTRIM(STR(I))
      gfREPLACE("QTY&LCI WITH &lcTmpPoLns..SpltQty&LCI")
    ENDFOR
    gfREPLACE("TotQTY   WITH  &lcTmpPoLns..SpltTQty ,"+;
              "trancd    WITH  '5'")
              
    gfREPLACE(" cRSession WITH lcGlSession")          

    =gfAdd_Info('POSLN')
    =gfREPLACE('')


             
    **APPEND FOR NEW PO
    gfAppend('POSLN',.T.)
    FOR I = 1 TO 8
      LCI =ALLTRIM(STR(I))
      gfREPLACE("QTY&LCI WITH &lcTmpPoLns..SpltQty&LCI")
    ENDFOR
    
    gfREPLACE("TotQTY   WITH  &lcTmpPoLns..SpltTQty , ;
               VENDOR    WITH  lcNewVend            , ;
               po        WITH  lcNewPO               ")
               
               
               
    =gfAdd_Info('POSLN')
    =gfREPLACE('')
           
           
  ENDIF
  GOTO lnRecno
  SELECT (lcTmpPoLns)
  SKIP
  SELECT POSLN
ENDSCAN     &&END OF lcTmpPoLNS

IF !llContinue 
  =gfModalGen('INM00000B00000','','',''," Can't generate PO with empty split Qty ")
  STORE .F. TO llShow,llCSave
  RETURN .F.
ELSE
  STORE .T. TO llCSave,llCUpdate,llUpdated,llShow
  SELECT (lcTmpPoHdr)
  SCATTER MEMVAR MEMO
  SELECT POSHDR
  gfAppend('POSHDR',.T.)
  

  gfREPLACE("OPEN      WITH  lnTotSplit        ,;
            nstyorder WITH  lnTotSplit        ,;
            CANCEL    WITH  0                 ,;
            RECEIVE   WITH  0                 ")
          
  gfREPLACE("VENDOR    WITH  lcNewVend         ,;
             po        WITH  lcNewPO           ,;    
             parentpo  WITH  lcPoNo")
          
  gfREPLACE("Nicost1      WITH  lnTCost1       ,;
            Nicost2      WITH  lnTCost2       ,;
            Nicost3      WITH  lnTCost3       ,;          
            Nicost4      WITH  lnTCost4       ")                    
          
  gfREPLACE("Nicost5      WITH  lnTCost5       ,;                              
             Nfcost1      WITH  lnTFCost1      ,;                              
             Nfcost2      WITH  lnTFCost2      ,;                              
             Nfcost3      WITH  lnTFCost3      ")                              
          
  gfREPLACE("Nfcost4      WITH  lnTFCost4      ,;                              
             Nfcost5      WITH  lnTFCost5      ,;
             Nfcost6      WITH  lnTFCost6      ,;                              
             Nfcost7      WITH  lnTFCost7")
          
  gfREPLACE("pototal      WITH  lnTCost1+lnTCost2+lnTCost3+lnTCost4+lnTCost5+lnTCost6+lnTCost7")                  

  =gfAdd_Info('POSHDR')
  gfREPLACE('')
          
  
  LOCATE
  =gfSEEK('PP'+lcPoNo) 
   gfREPLACE("OPEN    WITH  OPEN - lnTotSplit  ,; 
              CANCEL  WITH  CANCEL + lnTotSplit ")
  
  IF POSHDR.OPEN = 0
    gfREPLACE("status   WITH  'X' ")
  ENDIF
  
 
  
  lfSaveFile()
  *-- Give the numbers of the generated PO
  =gfModalGen('INM00000B00000','','',''," Generated new p/o#  "+ ALLTRIM(lcNewPO))
  =lfGenCstSH()
  lcNewPO   = ''
  return .T.
ENDIF
*!*************************************************************
*! Name      : lfvGenCstSH
*! Developer : Mariam Mazhar[MMT]
*! Date      : 11/03/2008
*! Purpose   : Function to generate the cost sheet.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfGenCstSH

lcCrtCSH   = gfGetMemVar('M_CRTCSTSH')

IF EMPTY(lcCrtCSH)
  lcCrtCSH = 'L'
ENDIF
  
lcAType = 'P'  
IF lcCrtCSH = 'I' 
  *--Would you like to create the purchase order cost sheet ?
  IF gfModalGen('QRM34064B34001','DIALOG',IIF(lcAType='D','Dye Order','PO')) = 2
     RETURN
  ENDIF  
ENDIF
lcParameter = "'" + lcNewPO + "'"+IIF(lcCrtCSH ="T",",.T.",',.F.')
oAriaApplication.DoProgram("AWRPOCSSH",lcParameter,.F.,'PO')
*************************************************************
*! Name      : lfSaveFile
*! Developer : Mariam Mazhar
*! Date      : 11/03/2008
*! Purpose   : function to save files
*!*************************************************************
FUNCTION lfSaveFile
*-- Call default save
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
   RETURN .F.
ENDIF
lnUpdated = 0
lnAryLen = ALEN(oAriaApplication.laRemoteTable)
FOR lnCounter=1 TO lnAryLen
  IF oAriaApplication.laRemoteTable[lnCounter].lnDataSession == SET("Datasession")
    IF !oAriaApplication.laRemoteTable[lnCounter].TableUpdate(lcTranCode)
      lnUpdated=lnCounter
      exit
    ENDIF
  ENDIF
NEXT
IF lnUpdated>0
  oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  RETURN
ELSE
  oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
ENDIF
*************************************************************
*! Name      : lfSpltAll
*! Developer : Mariam Mazhar
*! Date      : 11/03/2008
*! Purpose   : function to Splt all
*!*************************************************************
FUNCTION lfSpltAll
PARAMETERS   loFormSet

lcTmpPoLns = loFormSet.lcTmpPoLns
lnSTotQty = 0
IF loFormSet.ariaform1.chkSpltAll.Value
  FOR lnI = 1 TO 8 
    lcI = STR(lnI,1)
    REPLACE &lcTmpPoLns..SpltQty&lcI  WITH &lcTmpPoLns..Qty&lcI
    lnSTotQty = lnSTotQty  + &lcTmpPoLns..Qty&lcI
  ENDFOR 
  REPLACE &lcTmpPoLns..SpltTQty WITH lnSTotQty
ELSE
  FOR lnI = 1 TO 8 
    lcI = STR(lnI,1)
    REPLACE &lcTmpPoLns..SpltQty&lcI  WITH 0
  ENDFOR 
  REPLACE &lcTmpPoLns..SpltTQty WITH 0
ENDIF 
REPLACE &lcTmpPoLns..llSplt WITH loFormSet.ariaform1.chkSpltAll.Value
loFormSet.ariaform1.cntSpQty.refresh
