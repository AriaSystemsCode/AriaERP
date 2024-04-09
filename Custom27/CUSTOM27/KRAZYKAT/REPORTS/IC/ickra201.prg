****************************************************************************
*: Program file      : ICKRA201.PRG        (C#101645)
*: Program desc.     : Print style and all sales order allocated for it .
*:                   : (For Krazy Kat).
*: System            : Aria Apparel System (A27)
*: Module            : Inventory Control   (IC)
*: Developer         : ABDOU ELGENDI       (ABD)
*: Date              : 09/28/1999
*:**************************************************************************
*: Calls   FUNCTIONS : lfPrnHdr , lfVDate ,lfWoldVal , lfMyValid 
*:                   : ......
*:         PROCEDURE : lpTitle  , lpSayDetail
*:                   : ......
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*: Modifications     : ........
*:**************************************************************************
*
*--  Variable Declaration.
lnMaxRow  = 65
lnRow     = 6
R_WIDTH   = 'W'
R_TITLE   = [Minimum Requirements]
PAGENO    = 0
lcAccount =SPACE(6)
*-- END OF  Variable Declaration.

*-- Get the style major and color [Begin.]
STORE 0 TO lnColorLen,lnNonMajSt
lcMajPict  = gfItemMask("PM")
lnMajorLen = LEN(lcMajPict)
lcMajorTit = gfItemMask('HM')
lcNonMajTt = gfItemMask('HN')

*--Get the color & Get the No. of major segments.
lnMajSeg = gfItemMask('SM')
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
*-- lnMajor Var. is declared in main prg (ARPINV) 
*-- Get the Non Major elements. [Begin.]
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  IF laMajSeg[lnI,1] = 'C'
    lnNonMajSt = laMajSeg[lnI,4]
    lnColorLen = LEN(laMajSeg[lnI,3])
    EXIT
  ENDIF
ENDFOR 

*-- valid function to validate dates.
IF !lfMyValid()
  RETURN
ENDIF

*-- Get the Non Major elements. [End.]
*--- If user change filter criteria then you must collect data again [Begin]
IF llOGFltCh 
  *-- lcCursor : TABLE to hold data from (lcPSHdrTmp, OrdLin and OrdHdr)
  CREATE DBF (lcCursor) (PO          C(6)  ,;
                         VENDOR      C(8)  ,;
                         STYLE       C(19) ,;
                         TOTQTY      N(7,0),;
                         OLTOTQTY    N(6,0),;
                         CONTACT     C(20) ,;
                         AVAILABLE   D(8)  ,;
                         COMPLETE    D(8)  ,;
                         INSURANCE   C(18) ,;
                         ACCOUNT     C(05))

  INDEX ON STYLE+CONTACT TAG STYCLR OF (gcWorkDir+lcCursor)
  *-- lcSelctTmp : TABLE to hold data from (PoSHdr , PoSLn , OrdHdr , ORDLINE)
  CREATE DBF (lcSelctTmp)( OLTOTQTY    N(6,0),;
                           COMPLETE    D(8)  ,;  
                           ACCOUNT     C(05),;
                           CSTYLE      C(19) )
  INDEX ON CSTYLE+ACCOUNT+DTOS(COMPLETE) TAG (lcSelctTmp)
   
  SELECT ORDLINE
  SET ORDER TO ORDLINES
  SET RELATION TO 'O'+Order INTO ORDHDR , 'M'+ OrdHdr.ACCOUNT INTO CUSTOMER
  SELECT POSHDR
  
  *-- lcSelctTmp :Temprary file to hold all PO's with open or hold status
  SCAN FOR Status $ 'OH'
    SELECT PoSLn
    = SEEK(PoSHdr.cstytype+PoSHdr.PO,'PoSLn')
    SCAN WHILE cstytype+PO+STYLE+STR(LINENO,6)+TRANCD+STR(RECNO(),7) = PoSHdr.cstytype+PoSHdr.PO
      lcStyle   = PoSLn.Style
      lcContact = UPPER(PoSHdr.Contact)
      WAIT WINDOW "Selecting Style ...." + lcStyle NOWAIT
      *-- To check if this Style/Contact has been selected before then loop.
      IF ALLTRIM(lcContact) == 'KK' AND SEEK(lcStyle+'KK',lcCursor)
        LOOP     
      ENDIF
      SELECT ORDLINE
      =SEEK(LCSTYLE)

      IF !EMPTY(lcContact) .AND. (ALLTRIM(lcContact) = 'KK' .OR. ;
       SEEK(UPPER('M'+SUBSTR(lcContact,1,5)),'CUSTOMER') ).AND. !SEEK(lcStyle+UPPER(SUBSTR(lcContact,1,5)),lcSelctTmp) 
        SCAN WHILE STYLE = LCSTYLE FOR OrdLine.TotQty#0 .AND. OrdHdr.STATUS $ 'HO' .AND. OrdHdr.Complete <= ldDate5 ;
          .AND. IIF(ALLTRIM(lcContact) = 'KK',UPPER(CUSTOMER.DunsRtg) = 'KK',OrdHdr.ACCOUNT = UPPER(SUBSTR(lcContact,1,5)))
          lcAccnt = ORDHDR.ACCOUNT        
          IF SEEK(lcStyle+ORDHDR.ACCOUNT+DTOS(ORDHDR.COMPLETE),lcSelctTmp)
            SELECT (lcSelctTmp)
            REPLACE OLTOTQTY WITH OLTOTQTY+OrdLine.TotQty
          ELSE
            INSERT INTO (lcSelctTmp) (OLTOTQTY, COMPLETE, ACCOUNT,CSTYLE) ;
             VALUES (OrdLine.TotQty , OrdHdr.Complete, lcAccnt,lcStyle)
          ENDIF
        ENDSCAN
      ENDIF

      IF SEEK(lcStyle,lcSelctTmp)
        SELECT(lcSelctTmp)
        SCAN WHILE CSTYLE = lcStyle FOR IIF(ALLTRIM(lcContact) = 'KK',SEEK('M'+ACCOUNT,'CUSTOMER') .AND. UPPER(CUSTOMER.DunsRtg) = 'KK',ACCOUNT=UPPER(SUBSTR(lcContact,1,5)))
          SCATTER MEMVAR
          m.VENDOR    = PoSHdr.VENDOR    
          m.CONTACT   = UPPER(PoSHdr.CONTACT)
          m.AVAILABLE = PoSHdr.AVAILABLE
          m.INSURANCE = PoSHdr.INSURANCE
          m.STYLE     = lcStyle         
          m.TOTQTY    = POSLN.TOTQTY    
          m.PO        = PoSHdr.PO
          SELECT(lcCursor)
          INSERT INTO (lcCursor) FROM MEMVAR
        ENDSCAN
      ENDIF
    ENDSCAN 
  ENDSCAN
  SELECT ORDLINE
  SET RELATION TO
ENDIF   &&-- END IF FOR THE FLAG llOGFltCh


SELECT(lcCursor)
GOTO TOP
IF EOF()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG') 
  SET DEVICE TO SCREEN
  RETURN
 ENDIF
INDEX ON VENDOR+STYLE+PO TAG (lcCursor) OF (gcWorkDir+lcCursor)
SET DEVICE TO PRINT

*-- To print in condense form
IF gcDevice='P'
  @ PROW(),PCOL() SAY [&l1O&l14]
ENDIF

*-- Procedure Print report title
=lpTitle()

*-- Procedure Print report detail
=lpSayDetail()


DO ENDREPORT  && END THE REPORT OR DISPLAY ON SCREEN
*-- Return printing to default
IF gcDevice='P'
  @ PROW(),PCOL() SAY [&l0O]
ENDIF
SET DEVICE TO SCREEN
RETURN

*!**************************************************************************
*! Name      : lpTitle
*: Developer : ABDOU ELGENDI       (ABD)
*: Date      : 09/28/1999
*! Purpose   : Print report title
*!**************************************************************************
*! Calls     : None 
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   : = lpTitle()
*!**************************************************************************
*
PROCEDURE lpTitle
PAGENO = PAGENO+1

*-- Function To Print header.
=lfPrnHdr()
@ 04,001 SAY "VENDOR"
@ 04,011 SAY "PO#"
@ 04,020 SAY lcMajorTit
@ 04,033 SAY lcNonMajTt
@ 04,040 SAY "P/O QTY"
@ 04,055 SAY ldDate1
@ 04,064 SAY ldDate2
@ 04,073 SAY ldDate3
@ 04,082 SAY ldDate4
@ 04,091 SAY ldDate5
@ 04,107 SAY "ACNT.#"
@ 04,119 SAY "ACCOUNT NAME"
@ 04,155 SAY "SOLD DATE"
@ 04,165 SAY "SPECIAL INST."
@ 04,185 SAY "REMARKS"
@ 05,000 SAY REPLICATE('=',193)
lnRow = 6

*-- END OF lpTitle.

*!**************************************************************************
*! Name      : lpSayDetail
*: Developer : ABDOU ELGENDI       (ABD)
*: Date      : 09/28/1999
*! Purpose   : Print report detail
*!**************************************************************************
*! Calls     : None 
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   : = lpSayDetail()
*!**************************************************************************
*
PROCEDURE lpSayDetail

SELECT(lcCursor)

lcVendor = SPACE(08)
lcSty    = SPACE(19)
lcPO     = SPACE(06)

SCAN
  IF VENDOR+STYLE+PO = lcVendor+lcSty+lcPO
    LOOP
  ENDIF
  lcVendor = VENDOR
  lcSty    = STYLE
  lcPO     = PO
  lcSeekno = lcVendor+lcSty+lcPO
  
  STORE 0 TO lnTotQty1 ,lnTotQty2,lnTotQty3,lnTotQty4,lnTotQty5
  IF lnRow>=lnMaxRow
   =lpTitle()
   
  ENDIF
  @lnRow,001 SAY VENDOR
  @lnRow,011 SAY PO
  @lnRow,020 SAY  SUBSTR(STYLE,1,lnMajorLen)
  @lnRow,033 SAY SUBSTR(STYLE,lnMajorLen+2,lnColorLen)
  @lnRow,040 SAY TOTQTY


  SCAN FOR VENDOR+STYLE+PO = lcVendor+lcSty+lcPO
    DO CASE
      CASE Complete <= ldDate1
        lnTotQty1 = lnTotQty1 + OLTOTQTY
      CASE Complete > ldDate1 AND Complete <=ldDate2
        lnTotQty2 = lnTotQty2 + OLTOTQTY
      CASE Complete > ldDate2 AND Complete <=ldDate3
        lnTotQty3 = lnTotQty3 + OLTOTQTY
      CASE Complete > ldDate3 AND Complete <=ldDate4
        lnTotQty4 = lnTotQty4 + OLTOTQTY
      CASE Complete > ldDate4 
        lnTotQty5 = lnTotQty5 + OLTOTQTY
    ENDCASE
  ENDSCAN
  = SEEK(lcSeekno)
  @lnRow,050 SAY IIF(lnTotQty1 = 0,SPACE(08),lnTotQty1)
  @lnRow,060 SAY IIF(lnTotQty2 = 0,SPACE(08),lnTotQty2)
  @lnRow,070 SAY IIF(lnTotQty3 = 0,SPACE(08),lnTotQty3)
  @lnRow,080 SAY IIF(lnTotQty4 = 0,SPACE(08),lnTotQty4)
  @lnRow,090 SAY IIF(lnTotQty5 = 0,SPACE(08),lnTotQty5)

  lcTempAcc = IIF(CONTACT = 'KK',Account,CONTACT)
  @lnRow,107 SAY SUBSTR(lcTempAcc,1,5)
  lcAccName = IIF(SEEK('M'+SUBSTR(lcTempAcc,1,5),'CUSTOMER'),CUSTOMER.BTNAME,SPACE(30))
  
  @lnRow,119 SAY lcAccName
  @lnRow,155 SAY AVAILABLE
  @lnRow,165 SAY INSURANCE
  lnRow = lnRow+2
ENDSCAN

*-- END OF lpSayDetail. 

*!*****************************************************************************
*! Name      : lfPrnHdr
*: Developer : ABDOU ELGENDI       (ABD)
*: Date      : 09/28/1999
*! Purpose   : Print header
*!*****************************************************************************
*! Calls     : None 
*!*****************************************************************************
*! Returns   : None
*!*****************************************************************************
*! Example   : = lfPrnHdr()
*!*****************************************************************************
*
FUNCTION lfPrnHdr

R_TITLE  = TRIM(R_TITLE)
X1 = ((172 - (LEN(TRIM(gcCom_NAME))))/2)
X2 = ((172 - (LEN(R_TITLE)))/2)
@ 01,000 SAY "ICKRA201"
@ 01,X1  SAY gcCom_NAME
@ 01,181 SAY gdSysDate
@ 01,189 SAY '~'
@ 02,000 SAY TIME()
@ 02,X2  SAY R_TITLE
@ 02,181 SAY 'PAGE#'
@ 02,187 SAY STR(PAGENO,4)
@ 03,00  SAY REPLICATE('=',193)
*-- END OF lfPrnHdr.

*!**************************************************************************
*! Name      : lfVDate
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 09/23/1999 
*! Purpose   : Validate Date
*!**************************************************************************
*! Calls     : None 
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   : = lfVDate()
*!**************************************************************************
*
FUNCTION lfVDate
PARAMETER lnNumber,llCallFr

PRIVATE lcNumber
IF MDOWN()
  RETURN
ENDIF

lcNumber = STR(lnNumber,1)
IF EMPTY(ldDate&lcNumber)
  *-- Text Message :Date No.1,2,3,4,5  cannt be empty.
  = gfModalGen('QRM04074B00000','DIALOG','Date No.'+lcNumber)
  RETURN !llCallFr
ELSE   
  IF lnNumber # 1 .AND. EVAL('ldDate'+STR(lnNumber,1)) <= EVAL('ldDate'+STR(lnNumber-1,1))
    = gfModalGen('QRM04072B00000','DIALOG','Date No.'+STR(lnNumber,1)+"|"+'Date No.'+;
                  STR(lnNumber-1,1))
    RETURN !llCallFr
  ENDIF  
  IF lnNumber # 5 .AND. !llCallFr .AND. !EMPTY(EVAL('ldDate'+STR(lnNumber+1,1))) ;
              .AND. EVAL('ldDate'+STR(lnNumber,1)) >= EVAL('ldDate'+STR(lnNumber+1,1))
    = gfModalGen('QRM04072B00000','DIALOG','Date No.'+STR(lnNumber+1,1)+"|"+'Date No.'+;
                 STR(lnNumber,1))
    RETURN !llCallFr
  ENDIF
  IF !llCallFr .AND. !(laOldVal == ldDate&lcNumber)
    llOgFltCh = .T.
  ENDIF
ENDIF   
   
*-- END OF lfVDate.

*!*************************************************************
*! Name      : lfWoldVal
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/15/1999 
*! Purpose   : To return the old value.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfWoldVal()
*!*************************************************************
FUNCTION lfWoldVal

PARAMETERS lcDateNo
laOldVal = ldDate&lcDateNo

*-- END OF lfWoldVal
*!**************************************************************************
*! Name      : lfMyValid
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 09/29/1999 
*! Purpose   : Validate Date in the privew mode
*!**************************************************************************
*! Calls     : =lfVDate()
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   : = lfMyValid()
*!**************************************************************************
*
FUNCTION lfMyValid

lnCount = 0
FOR lnCount = 1 TO 5
  IF !lfVDate(lnCount,.T.)
    lcCount = STR(lnCount,1)
    _CUROBJ = OBJNUM(ldDate&lcCount)
    RETURN .F.
  ENDIF
ENDFOR
*-- END OF lfMyValid.
*!**************************************************************************
