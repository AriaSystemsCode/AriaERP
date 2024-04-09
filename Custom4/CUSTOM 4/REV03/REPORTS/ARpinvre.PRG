*:***************************************************************************
*: Program file  : ARPINVRE.PRG (Custom Invoice for REVUE)
*: Program desc. : Invoice Form
*: Date          : 08/14/2008
*: System        : Aria 4XP.
*: Module        : Account Receivable (AR)
*: Developer     : Mariam Mazhar (MMT)
*: Ticket No     : T20080422.0041
*: Tracking Job  : C201043
*:***************************************************************************
loOGScroll.cCRPapersize = 'LETTER'
lcRpPrSt = IIF(lcRpPrSt ='N',SPACE(1),lcRpPrSt)
=gfOpenTable('INVHDR','INVHDR','SH','INVHDR_A')
=gfOpenTable('INVLINE','INVLINE','SH','INVLINE_A')
=gfOpenTable('STYLE','STYLE','SH','STYLE_A')
=gfOpenTable('SCALE','SCALE','SH','SCALE_A')
=gfOpenTable('NOTEPAD','NOTEPAD','SH','NOTEPAD_A')
=gfOpenTable('CUSTOMER','CUSTOMER','SH','CUSTOMER_A')
=gfOpenTable('SPCK_lin','SPCKLINS','SH','SPCK_lin_A')
=gfOpenTable('ORDHDR','ORDHDR','SH','ORDHDR_A')
=gfOpenTable('CONSINVL','CONSINVL','SH','CONSINVL_A')
=gfOpenTable('SkuTmpl','SKUTMPL','SH','SkuTmpl_A')

lcRpExp = STRTRAN(lcRpExp,'INVHDR','INVHDR_A')
lcRpExp =  STRTRAN(lcRpExp,'INVLINE','INVLINE_A')


PRIVATE lcTaxRefr , lcOldStyle , lnStyleLen
lcOldStyle = ''
lnStyleLen = LEN(ALLTRIM(gfItemMask('PM')))
lcTaxRefr = gfGetMemVar('M_TAX_REFE',oAriaApplication.ActiveCompanyID)
SELECT InvHdr_A
LOCATE FOR &lcRpExp
IF !FOUND()
  *-- Text : 'No Record Selected for the report..!'
*  =gfModalGen('TRM00052B00000','DIALOG')
  lcRpPrSt = IIF(lcRpPrSt =SPACE(1),'N',lcRpPrSt)
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)

SELECT INVLINE_A
=AFIELDS(laFileStru)
=gfCrtTmp(lcStylIndx,@laFileStru,"Invoice + Style",lcStylIndx ,.F.)


SELECT INVHDR_A
=AFIELDS(laFileStru)
=gfCrtTmp(lcInvHDr,@laFileStru,"Invoice",'lcInvHDr' ,.F.)


DIMENSION laFootStr[5,4]
laFootStr[1,1] = 'Invoice'
laFootStr[1,2] = "C"
laFootStr[1,3] = 6
laFootStr[1,4] = 0

laFootStr[2,1] = 'cLine1'
laFootStr[2,2] = "C"
laFootStr[2,3] = 40
laFootStr[2,4] = 0

laFootStr[3,1] = 'cLine2'
laFootStr[3,2] = "C"
laFootStr[3,3] = 40
laFootStr[3,4] = 0

laFootStr[4,1] = 'cLine3'
laFootStr[4,2] = "C"
laFootStr[4,3] = 40
laFootStr[4,4] = 0

laFootStr[5,1] = 'cLine4'
laFootStr[5,2] = "C"
laFootStr[5,3] = 40
laFootStr[5,4] = 0

=gfCrtTmp(lcInvFoter,@laFootStr,"Invoice",lcInvFoter,.F.)

PRIVATE lnConsLine , lcDateCent , lcInvLnTag , lnLinesNo , lcLinesNo
lcDateCent = SET('CENTURY')
SET CENTURY ON

lnConsLine = 0
SELECT INVHDR_A
SCAN FOR &lcASExp
  SCATTER MEMO MEMVAR 
  INSERT INTO (lcInvHDr) FROM MEMVAR 
  INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR_A.INVOICE)
  *--lcInvcee    >> Variable hold the invoice No.
  lcInvcee = INVHDR_A.Invoice
  SELECT (lcInvFoter)
  lnLinesNo = 1
  APPEND BLANK
  REPLACE Invoice WITH lcInvcee

  IF llTAX AND lcTaxMeth = 'M' AND INVHDR_A.Tax_Amt # 0
    IF !EMPTY(lcTaxRefr)
      lcLinesNo = ALLTRIM(STR(lnLinesNo,1))
      REPLACE cLine&lcLinesNo WITH lcTaxRefr
      lnLinesNo = lnLinesNo + 1
    ENDIF  
    lcLinesNo = ALLTRIM(STR(lnLinesNo,1))
    REPLACE cLine&lcLinesNo WITH lcTaxDesc + ' :' + ALLTRIM(STR(INVHDR_A.TAX_RATE,5,2)) + '  ' + ;
                                 TRANSFORM(INVHDR_A.Tax_Amt,'9999999.99')
    lnLinesNo = lnLinesNo + 1
  ENDIF

  IF INVHDR_A.FREIGHT + INVHDR_A.INSUR + INVHDR_A.COD <> 0
    lcLinesNo = ALLTRIM(STR(lnLinesNo,1))
    REPLACE cLine&lcLinesNo WITH 'TOTAL - FREIGHT ' + TRANSFORM(INVHDR_A.FREIGHT+INVHDR_A.INSUR+INVHDR_A.COD,'9999999.99')
    lnLinesNo = lnLinesNo + 1
  ENDIF

  IF INVHDR_A.DISCOUNT<>0
    lcLinesNo = ALLTRIM(STR(lnLinesNo,1))
    REPLACE cLine&lcLinesNo WITH 'DISCOUNT' + SPACE(5) + TRANSFORM(INVHDR_A.Discount,'99999999.99')
  ENDIF
  
  lnConsLine = 0    
  IF INVHDR_A.CONSOL = 'Y'
    SELECT CONSINVL_A
    gfSeek(INVHDR_A.INVOICE,'CONSINVL_A')
    SCAN REST WHILE Invoice+Store+Order+Style+STR(lineno,6) = lcInvcee
      WAIT WINDOW 'Selecting Records For The Report ...' + lcInvcee NOWAIT

      SCATTER MEMVAR MEMO
      lnConsLine = lnConsLine + 1
      m.LineNo = lnConsLine
      m.Desc1 = IIF(gfSEEK(m.Style+m.Invoice,'InvLine_A','InvLines'),InvLine_A.Desc1,'')
      SELECT(lcStylIndx)
      APPEND BLANK
      GATHER MEMVAR MEMO      
    ENDSCAN
  ELSE
    SELECT INVLINE_A
    =gfSEEK(lcInvcee)
    SCAN REST WHILE Invoice+STR(lineno,6) = lcInvcee
      WAIT WINDOW 'Selecting Records For The Report ...' + lcInvcee NOWAIT
      SCATTER MEMVAR MEMO
      SELECT(lcStylIndx)
      APPEND BLANK
      GATHER MEMVAR MEMO
    ENDSCAN
  ENDIF  
ENDSCAN

SET CENTURY &lcDateCent



SELECT (lcStylIndx)
SET RELATION TO IIF(!EMPTY(ALTSTYLE) , ALTSTYLE ,Style) INTO STYLE_A
SET RELATION TO "S" + Account + Style INTO SPCK_LIN_A ADDITIVE

SELECT STYLE_A
SET RELATION TO 'S' + Scale INTO SCALE_A

SELECT (lcInvHDr)
IF llPrntInst .OR. llRpInvNot 
  IF !llIsAparel
    SET RELATION TO '' INTO (lcTmpDbt)
    SELECT (lcTmpDbt)
    SET RELATION TO IIF(CFILE_NUM = '1', &lcInvHDr..Invoice, '*') INTO (lcStylIndx) ADDITIVE
  ELSE
    SET RELATION TO &lcInvHDr..Invoice INTO (lcStylIndx) ADDITIVE
  ENDIF   
ELSE
  SET RELATION TO &lcInvHDr..Invoice INTO (lcStylIndx) ADDITIVE
ENDIF

SELECT (lcInvHDr)
SET RELATION TO IIF(EMPTY(Store) OR Store = "********",IIF (EMPTY(dist_ctr),'M' + Account,'S' + Account + dist_ctr),'S' + Account + Store) INTO CUSTOMER_A ADDITIVE
SET RELATION TO INVOICE INTO (lcInvFoter) ADDITIVE
SET RELATION TO 'O' + order INTO Ordhdr_A ADDITIVE
IF (llPrntInst .OR. llRpInvNot)  AND !llIsAparel
  
  SET SKIP TO (lcTmpDbt) , (lcStylIndx)
ELSE
  SET SKIP TO (lcStylIndx)
ENDIF


SELECT (lcInvHDr)
DO gfDispRe WITH EVAL('lcFormName')
lcRpPrSt = IIF(lcRpPrSt =SPACE(1),'N',lcRpPrSt)
llarpinv = .F.
=gfCloseTable('INVHDR_A')
=gfCloseTable('INVLINE_A')
=gfCloseTable('STYLE_A')
=gfCloseTable('SCALE_A')
=gfCloseTable('NOTEPAD_A')
=gfCloseTable('CUSTOMER_A')
=gfCloseTable('SPCK_lin_A')
=gfCloseTable('ORDHDR_A')
=gfCloseTable('CONSINVL_A')
=gfCloseTable('SkuTmpl_A')
*!**************************************************************************
*! Name      : lfNewStye
*! Developer : Mariam Mazhar(MMT)
*! Date      : 08/14/2008
*! Purpose   : assigne empty to the old style to reprint the style again at 2 page.
*!**************************************************************************
FUNCTION lfNewStye

PARAMETER lcDummy
lcDummy = ''

lcOldStyle  = ''

RETURN lcDummy
*!**************************************************************************
*! Name      : lfOldStyle
*! Developer : Mariam Mazhar(MMT)
*! Date      : 08/14/2008
*! Purpose   : To check if style is changed.
*!**************************************************************************
*
FUNCTION lfOldStyle
PARAMETER lcDummy
lcDummy = ''
IF !(lcOldStyle == &lcStylIndx..Invoice + SUBSTR(&lcStylIndx..Style,1,lnStyleLen))
  lcOldStyle = &lcStylIndx..Invoice + SUBSTR(&lcStylIndx..Style,1,lnStyleLen)
ENDIF
RETURN lcDummy
*-- End of lfOldStyle.
*!**************************************************************************
*! Name      : lfRevPrSku
*! Developer : Mariam Mazhar(MMT)
*! Date      : 08/14/2008
*! Purpose   : Print the style Skus for a specific account.
*!**************************************************************************
*
FUNCTION lfRevPrSku
PARAMETER llDummy
llDummy = .F.
lcStrToPrn = ''
IF !gfSEEK('S'+&lcStylIndx..Account+&lcStylIndx..Style,'Spck_Lin_A')
  RETURN llDummy
ENDIF
llDummy = .T.
PRIVATE lcAlias
lcAlias = ALIAS()
SELECT Spck_Lin_A
IF EMPTY(Sku)
  lnI = 1
  =gfSEEK('S'+Style_A.Scale,'Scale_A')
  lcSkuTmpl=IIF(!EMPTY(Customer_A.SkuTmpl),Customer_A.SkuTmpl,'DEF')
  IF gfSEEK('S'+lcSkuTmpl,'SkuTmpl_A')
    lnDime1 = SkuTmpl_A.Len1+SkuTmpl_A.Len2+SkuTmpl_A.Len3
    lnDime2 = SkuTmpl_A.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF 

  DIME laSku[8]
  laSku = SPACE(16)
   SCAN WHILE Type+Account+Style = 'S'+&lcStylIndx..Account+&lcStylIndx..Style AND lnI < 9
    FOR lnX=1 TO 8
      Z=STR(lnX,1)
      IF QTY&Z > 0
        laSku(lnX)=SUBSTR(Pack_Id,lnDime1+1,lnDime2)
        EXIT
      ENDIF
    ENDFOR
    lnI = lnI + 1
  ENDSCAN

  lnI = 1
  = gfSEEK('S'+&lcStylIndx..Account+&lcStylIndx..Style,'Spck_Lin_A')
  DO WHILE Type+Account+Style = 'S'+&lcStylIndx..Account+&lcStylIndx..Style AND lnI < 9
    lcStrToPrn = lcStrToPrn + 'SKU N#' + SUBSTR(Pack_Id,1,lnDime1) + ' '
    DO WHILE Type+Account+Style = ;
             'S'+&lcStylIndx..Account+&lcStylIndx..Style AND !EOF()
      lcI = STR(lnI,1)
      lcStrToPrn = lcStrToPrn + Scale_A.Sz&lcI+':'+laSku(lnI) + ' '
      lnI = lnI + 1
      SKIP
      IF lnI = 5 .OR. lnI = 9
        EXIT
      ENDIF
    ENDDO
    lcStrToPrn = lcStrToPrn + CHR(13)
  ENDDO  
ELSE
  lcStrToPrn = Sku + 'CUSTOMER SKU #'
ENDIF
IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF
RETURN llDummy
*-- End of lfRevPrSku.
*!**************************************************************************
*! Name      : lfRevSclSz
*! Developer : Mariam Mazhar(MMT)
*! Date      : 08/14/2008
*! Purpose   : To get the scale and size.
*!**************************************************************************
*
FUNCTION lfRevSclSz
PARAMETER lcDummy
lcDummy = ''
PRIVATE lcAlias  , lcX , lnCountSc , lcKey , lnSclCnt , lcScaleSz
lcOldInv   = &lcInvHDr..INVOICE
lcAlias = ALIAS()
SELECT (lcStylIndx)
lcKey = Invoice + Style
=SEEK(&lcInvHDr..Invoice)
lnSclCnt = 0
STORE SPACE(0) TO laScale
SCAN WHILE Invoice = &lcInvHDr..Invoice  AND lnSclCnt < lcScalCont + 1
  IF ASCAN(laScale,&lcStylIndx..SCALE) = 0 AND gfSEEK('S'+&lcStylIndx..SCALE,'SCALE_A')
    lnSclCnt = lnSclCnt + 1
    laScale[lnSclCnt,1] = SCALE_A.Scale
    FOR lnCountSc = 1 TO 8 
      lcX = STR(lnCountSc,1)
      laScale[lnSclCnt,lnCountSc+1] = SCALE_A.SZ&lcX
    ENDFOR
  ENDIF
ENDSCAN    
=SEEK(lcKey,lcStylIndx)
SELECT (lcAlias)
RETURN lcDummy
*-- End of lfRevSclSz.
*!*************************************************************
*! Name      : lfPrtNotes_A
*! Developer : Mariam Mazhar(MMT)
*! Date      : 08/14/2008
*! Returns   : Printed Notes
*!*************************************************************
*! Called from : ARPINVA,ARPINVZ .FRX (Notes Expression)
*!*************************************************************
FUNCTION lfPrtNotes_A
PRIVATE lcReturn

DO CASE
  CASE llRpInvNot .AND. EVAL(lcTmpDbt+'.cfile_num')='2' .AND.;
       !EMPTY(ALLTRIM(NOTEPAD_A.mNotes)) .AND. LEFT(ALLTRIM(STRTRAN(NOTEPAD_A.mNotes,CHR(13)+CHR(10),' ')),1) <> '*' ;
       .AND. gfSEEK('C' + &lcInvHDr..Invoice , 'NOTEPAD_A')

    FOR lnLoop = 1 TO MEMLINES(NOTEPAD.mNotes)
      IF MLINE(NOTEPAD_A.mNotes , lnLoop) = CHR(13)
        lcNotes    = ALLTRIM(NOTEPAD_A.mNotes)
      ENDIF
    ENDFOR
    lcNotesTtl = 'Invoice Notes'
    lcNotes    = ALLTRIM(NOTEPAD_A.mNotes)
  CASE llRpInvLNt .AND. !EMPTY(&lcStylIndx..note_mem) .AND. LEFT(ALLTRIM(STRTRAN(&lcStylIndx..note_mem,CHR(13)+CHR(10),' ')),1)<>'*'
    lcNotesTtl = 'Line Notes'
    lcNotes    = ALLTRIM(&lcStylIndx..note_mem)

  OTHERWISE
    STORE '' TO lcNotesTtl, lcNotes
ENDCASE

RETURN !EMPTY(lcNotesTtl)

*!*************************************************************
*! Name      : lfSolSpAdr_A
*! Developer : Mariam Mazhar(MMT)
*! Date      : 08/14/2008
*! Purpose   : Function to Get the Sold to Address & Ship to Address
*!             & the Description of the Ship Via , Terms
*!*************************************************************
*! Called from : ARPINVA.FRX
*!*************************************************************
*! Calls       : gfRltFld() , gfCodDes() , gfGetAdr() , lfAdrShift()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*
FUNCTION lfSolSpAdr_A

PRIVATE lnInvHdRec , lnInvLnRec , lnPakLnRec ,lnLineRec
lnInvHdRec = IIF(EOF(lcInvHDr) , 0 , RECNO(lcInvHDr))
lnInvLnRec = IIF(EOF(lcStylIndx) , 0 , RECNO(lcStylIndx))
lnPakLnRec = IIF(EOF('SPCK_LIN_A') , 0 , RECNO('SPCK_LIN_A'))

IF USED(lcTmpDbt)
  lnTmpDbt = IIF(EOF(lcTmpDbt) , 0 , RECNO(lcTmpDbt))
ELSE
  lnTmpDbt   = 0
ENDIF
lnLineRec = IIF(EOF(lcStylIndx) , 0 , RECNO(lcStylIndx))
lnHrRc    = IIF(EOF(lcInvHDr) , 0 , RECNO(lcInvHDr))

COUNT TO lnLines WHILE &lcStylIndx..INVOICE = &lcInvHDr..INVOICE
IF lnInvLnRec > 0
  GO (lnLineRec) IN (lcStylIndx)
ENDIF
IF lnHrRc > 0
  GO (lnHrRc) IN (lcInvHDr)
ENDIF


DECLARE laFactor[5,1]
STORE '' TO laFactor,lcFacName   


*-- Fill laFactor with factor address
IF !EMPTY(&lcInvHDr..CFACCODE)
  =SEEK(&lcInvHDr..CFACCODE,'SYCFACT')
    lcFacName   = SYCFACT.cfaccomp
    laFactor[1] = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
    laFactor[2] = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
    laFactor[3] = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
    laFactor[4] = gfGetAdr('SYCFACT' , '' , '' , '' , 4)
    laFactor[5] = gfGetAdr('SYCFACT' , '' , '' , '' , 5)
    =lfAdrShift('laFactor')
ENDIF

llEndGroup = .F.
=gfRltFld(&lcInvHDr..cDivision , @laDivLName , 'CDIVISION')
lcShipVia = gfCodDes(&lcInvHDr..ShipVia , 'SHIPVIA')
lcTerms = gfCodDes(&lcInvHDr..cTermCode , 'CTERMCODE')

lcSolTName = CUSTOMER_A.BTName

laSoldTo[1] = gfGetAdr('CUSTOMER_A' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER_A' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER_A' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER_A' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER_A' , '' , '' , '' , 5 , '2')

=lfAdrShift('laSoldTo')

*IF ORDHDR.Alt_ShpTo is .T.
SELECT (lcInvHDr)
IF BETWEEN(RECNO(), 1, RECCOUNT())
  GOTO RECNO()
ENDIF
SELECT CUSTOMER_A

IF ORDHDR_A.Alt_ShpTo
  lcShpTName  = ORDHDR_A.STName
  laShipTo[1] = ORDHDR_A.cAddress1
  laShipTo[2] = ORDHDR_A.cAddress2
  laShipTo[3] = ORDHDR_A.cAddress3
  laShipTo[4] = ORDHDR_A.cAddress4
  laShipTo[5] = ORDHDR_A.cAddress5
ELSE    && Else

  lnCUSRec = 0
  IF !EMPTY(CUSTOMER_A.Store) AND !EMPTY(CUSTOMER_A.Dist_ctr) AND !ORDHDR_A.lStrDirct
    lnCUSRec = IIF(!EOF('CUSTOMER_A'),RECNO('CUSTOMER_A'),0)
    =gfSEEK('S'+CUSTOMER_A.Account+CUSTOMER_A.Dist_ctr)
    lcDCCode    = CUSTOMER_A.STORE
  ELSE
    lcDCCode = ''
  ENDIF

  lcShpTName  = IIF(&lcInvHDr..STORE = "********" , "At Store Level " ,;
                IIF( EMPTY(CUSTOMER_A.DBA) , CUSTOMER_A.STNAME , CUSTOMER_A.DBA))

  laShipTo[1] = IIF(&lcInvHDr..STORE = "********" , "" , gfGetAdr('CUSTOMER_A' , '' , '' , '' , 1))
  laShipTo[2] = IIF(&lcInvHDr..STORE = "********" , "" , gfGetAdr('CUSTOMER_A' , '' , '' , '' , 2))
  laShipTo[3] = IIF(&lcInvHDr..STORE = "********" , "" , gfGetAdr('CUSTOMER_A' , '' , '' , '' , 3))
  laShipTo[4] = IIF(&lcInvHDr..STORE = "********" , "" , gfGetAdr('CUSTOMER_A' , '' , '' , '' , 4))
  laShipTo[5] = IIF(&lcInvHDr..STORE = "********" , "" , gfGetAdr('CUSTOMER_A' , '' , '' , '' , 5))

  IF lnCUSRec <> 0
    GOTO lnCUSRec IN CUSTOMER_A
  ENDIF
ENDIF    && End of IF

=lfAdrShift('laShipTo')

SELECT (lcInvHDr)

IF lnTmpDbt <> 0
  GO lnTmpDbt IN (lcTmpDbt)
ENDIF

*-- Restore the old record pointer in INVLINE
IF lnInvLnRec = 0
  GO BOTTOM IN (lcStylIndx)
  IF !EOF(lcStylIndx)
    SKIP IN (lcStylIndx)
  ENDIF
ELSE
  GO lnInvLnRec IN (lcStylIndx)
ENDIF

*-- Restore the old record pointer in SPCK_LIN
IF lnPakLnRec = 0
  GO BOTTOM IN SPCK_LIN_A
  IF !EOF('SPCK_LIN_A')
    SKIP IN SPCK_LIN_A
  ENDIF
ELSE
  GO lnPakLnRec IN SPCK_LIN_A
ENDIF
RETURN ''