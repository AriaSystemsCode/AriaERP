*:************************************************************************
*: PROGRAM   : ALPKTKT.PRG 
*: Tracking #: E302596
*: DESC.     : PICK TICKET FORM T (Conversion of ALPKTKT.PRG from a27)
*: System    : Aria4xp
*: Module    : SALES ORDER ALLOCATION (AL)
*: Developer : TMI - TAREK MOHAMMED IBRAHIM
*: DATE      : 04/29/2009
*: Idea      : I 've took the alpktkt.prg of aria27 as it is, I an output is sent to a file 
*              after finish I create a temp file and append from this text file
*              a field for Page# is added and the frx is grouped by Pag#
*:************************************************************************
*:Modifications:
*:************************************************************************
*
*-- Set the memory variables.
*--llRpPrtClr :- print Color Desc.
*-- Call this function once per OG session.
= (TYPE("lnMajSeg") = "C") AND lfEvalSegs()

STORE '' To lcStrToPrn,lcSkuSize,lcSclStrn,lcStr

STORE .F. TO llNoRec                && Variable used to run the endreport function in the main program.

lnPrvfil   = SELECT(0)                        && Restore Current Alias
lcScale    = ''                               && style scale 
lcSetOrdH  = ''                               && current order of OrdHdr 
lcSetOrdL  = ''                               && current order of OrdLin
lnLength   = 07
lnLen      = 0
ROW        = 0
llScale    = .T.  && Flag to print the size scales at the first page only.
llEndPT    = .F.
llNewDoc   = .T.  && Flag to print the new page.
lnNotLine  = 1
*--- Flag to loop if Row>=47 when printing size scales
llRet = .F.

STORE SPACE(1) TO HLINE1
HLine2  = TRIM(laCompAdd[1])           && variable hold the addressess of the company.
HLine3  = TRIM(laCompAdd[2])
HLine4  = TRIM(laCompAdd[3])
IF LEN(TRIM(HLine3)) = 0
  HLine3 = HLine4
  HLine4 = SPACE(1)
ENDIF

*- Initial variable hold the length og the sku in spck_hdr file.[START]
*--Variable hold the length of the sku in the spck_hdr.
STORE 0 TO lnLenDm1

*-- Open The Req. Files [Begin]
 = gfOpenFile(gcDataDir+'Customer','Customer','SH')
 = gfOpenFile(gcDataDir+'SkuTmpl','SkuTmpl','SH')
 = gfOpenFile(gcDataDir+'Spck_Lin','Spcklins','SH')
 = gfOpenFile(gcDataDir+'WhsLoc','WhsLocSt','SH')
 = gfOpenFile(gcDataDir+'Scale','Scale','SH')
 = gfOpenFile(gcDataDir+'PIKTKT','PIKTKT','SH')
*-- Open The Req. Files [End  ]

SELECT (lcTmpOrdL)
DELETE ALL FOR  LINENO = 0
LOCATE
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  *-- Do EndReport Or Don't.
  llNoRec = .T.

  SET DEVICE TO SCREEN

  RETURN
ENDIF

lcPiktktNo = gfTempName()
CREATE CURSOR &lcPiktktNo (PIKTKT C(6))
INDEX ON PIKTKT TAG PIKTKT

*-- MAIN LOOP.
SELECT (lcTmpOrdL)

lcDevice = SET("Device")
lcCent = SET("Century")
SET CENTURY OFF	

*- output to file that will used to get the data 
lcTempOut = gfTempName()
SET DEVICE TO FILE (oAriaApplication.WorkDir+lcTempOut+'.TXT')

DO WHILE !EOF(lcTmpOrdL)

  *-- GET ORDERHDR & LINE ITEMS
  IF llNewDoc
    SELECT (lcTmpOrdL)
    lcPikTkt = PIKTKT
    lcOrder  = ORDER
    =SEEK(lcPikTkt,'PIKTKT')
    ldDate   = PIKTKT.DATE
    lcStore  = PIKTKT.STORE
    WAIT WINDOW 'Printing Pick Ticket ...' + PIKTKT NOWAIT
    *- get the selected piktkts
    IF !SEEK(lcPikTkt,lcPiktktNo) 
      SELECT &lcPiktktNo
      APPEND BLANK
      REPLACE PIKTKT WITH lcPikTkt
    ENDIF
    SELECT ORDHDR
    
    *- Get the correct order in ORDHDR [Begin]
    =SEEK('O'+lcOrder,'ORDHDR')
    
    lcSetOrdH = ORDER()
    SET ORDER TO ORDHDR
    =SEEK ('O'+lcOrder)
    lcAccount = ACCOUNT    
    STORE 0.00 TO XORDTOT, lnXValue , SVALUE

    SELECT ORDLINE
    lcSetOrdL = ORDER()
    SET ORDER TO TAG Ordlinst IN ORDLINE
    =SEEK ('O'+lcOrder+lcStore)

    IF ORDHDR.MultiPO 
      lcCustPO = CustPO
    ENDIF 

    IF PIKTKT.PIKTKT <> lcPikTkt    
      LOCATE REST FOR PIKTKT = lcPikTkt ;
             WHILE cOrdtype+Order+Store+Style+STR(lineno,6) = lcOrder+lcStore
    ENDIF

    IF ORDER+STORE <> lcOrder+lcStore 
      SELECT (lcTmpOrdL)
      SKIP
      LOOP
    ENDIF

    SELECT (lcTmpOrdL)
    *-- Function in alpktkt.prg that collect address and all needed variables.
    =lfSolSpAdr()
    
    lcBTName  = lcSolTName 
    lcBTAddr1 = laSoldTo[1]
    lcBTAddr2 = laSoldTo[2]
    lcBTAddr3 = laSoldTo[3] 
    lcTerm_Data = lcTerms
    lcShp_Data  = lcShipVia
    XSPCI_DATA  = lcSpcInst
    XSEAS_DATA  = lcSeason
    lcDIV_DATA  = lcDivLName

    lcSTName  = lcShpTName
    lcSTAddr1 = laShipTo[1]
    lcSTAddr2 = laShipTo[2]
    lcSTAddr3 = laShipTo[3]
    lnPieces = 0
    *-- GET THE SIZE SCALES
    *-- once outside the loop not every time in the loop because it will
    *-- be the same.
    SELECT (lcTmpOrdL)
    lnRecNo = RECNO() 
    STORE 0 TO lnRec,lnSca
    I = 1
    DO WHILE ORDER = lcOrder
      *- DONT USE No. of scales exceeds 9.
      IF I > 9 
        EXIT
      ENDIF
      X = STR(I,1)
      lcScale&X = SCALE
      LOCATE REST FOR SCALE <> lcScale&X
      lnSca = lnSca +1
      I = I +1
    ENDDO
    GOTO lnRecNo
    HLINE1   = IIF(EMPTY(HLINE1) , lcCompName , HLINE1)
  ENDIF
  *-- START PRINTING

  DO lpHrdLabls   && Print the Header labels.
  IF llRet
    LOOP
  ENDIF 
  *-- LINE LOOP
  SELECT (lcTmpOrdL) 
  llNewDoc  = .T.
  XTOTQTY   = 0

  SCAN WHILE PikTkt = lcPikTkt

    *-- Modified to get the Style/Color location.     
    SELECT STYLE
    IF SEEK(&lcTmpOrdL..Style)
      lcStyDesc  = DESC   
      lcStyLocat = Location
      lcScale    = Scale
    ELSE
      STORE '' TO lcStyDesc,lcStyLocat,lcScale
    ENDIF
    lcClrDesc = gfCodDes(SUBSTR(Style.Style,lnNonMajSt,lnColorLen) ,'COLOR')
    SELECT (lcTmpOrdL)
    @ ROW,4 SAY SUBSTR(Style.Style,1,lnMajorLen)
    @ ROW,lnMajorLen+4 SAY SUBSTR(Style.Style,lnNonMajSt,lnColorLen)
    @ ROW,23 SAY LEFT(lcScale,1)
    @ ROW,25 SAY PIK1   PICTURE '@Z 9999'
    @ ROW,29 SAY PIK2   PICTURE '@Z 9999'
    @ ROW,34 SAY PIK3   PICTURE '@Z 9999'
    @ ROW,38 SAY PIK4   PICTURE '@Z 9999'
    @ ROW,43 SAY PIK5   PICTURE '@Z 9999'
    @ ROW,47 SAY PIK6   PICTURE '@Z 9999'
    @ ROW,52 SAY PIK7   PICTURE '@Z 9999'
    @ ROW,56 SAY PIK8   PICTURE '@Z 9999'
    @ ROW,62 SAY TOTPIK PICTURE '99999'
    @ ROW,69 SAY IIF(llRpStyPrc,PRICE,'') PICTURE '9999.99'
    @ ROW,78 SAY TOTPIK PICTURE '9999' 
    @ ROW,83 SAY IIF(SEEK(Style,'Style'),IIF(Style.Qty_Ctn <> 0, ;
                     CEILING(TotPik/Style.Qty_Ctn),0),0) PICTURE '9999'                 
    ROW = ROW + 1

    IF !llRpSkuBck 
      @ ROW,04 SAY 'STYLE DESC.:'+lcStyDesc
      @ ROW,36 SAY IIF(llRpPrtClr,'CLR DESC.: ' +ALLTRIM(SUBSTR(lcClrDesc,1,25)), '')
      IF llRpStyLoc
        =lfGetLoc() 
        IF !EMPTY(lcStr)
          @ ROW,63 SAY 'BINS    :'+ SUBSTR(lcStr,1,10)
          Row = Row + 1
          IF LEN(lcStr) > 18
            =lfContuLoc()
          ENDIF  
        ELSE
          Row = Row + 1  
        ENDIF
      ENDIF  
    ELSE
      IF SEEK('P'+PIKTKT.Account+&lcTmpOrdL..Style,'SPCK_LIN')
        @ ROW,4 SAY 'PACK ID#:'+Spck_Lin.Pack_Id
        @ ROW,29 SAY 'STYLE DESC.:'+lcStyDesc
        @ ROW,62 SAY IIF(llRpPrtClr,'CLR DESC.:' + SUBSTR(lcClrDesc,1,25), '')
        Row = Row + 1
      ELSE
        *--- Procedure to print the Style/Color Sku no.      
        DO lpPrtSku  && Print the ,main style/color Skus for a specific account.
        IF !llRpSkuSiz
          @ ROW,03 SAY lcStrToPrn+' '+'STYLE DESC.:'+lcStyDesc+' '+IIF(llRpPrtClr,'CLR DESC.:'+SUBSTR(lcClrDesc,1,25), '')
          Row = Row + 1
          IF llRpStyLoc
            =lfGetLoc()
            IF !EMPTY(lcStr)
              @ ROW,03 SAY 'BINS    :'+ SUBSTR(lcStr,1,85)
              Row = Row + 1
              IF LEN(lcStr) > 85
                =lfContuLoc() && Print the ,main style/color Skus for a specific account. 
              ENDIF  
            ELSE
              Row = Row + 1  
            ENDIF
          ENDIF  
          Row = Row + 1
        ELSE
          @ ROW,03 SAY 'STYLE DESC.:'+lcStyDesc
          @ ROW,36 SAY IIF(llRpPrtClr,'CLR DESC.:'+ ALLTRIM(SUBSTR(lcClrDesc,1,25)), '')
          IF llRpStyLoc
            =lfGetLoc()
            IF !EMPTY(lcStr)
              @ ROW,63 SAY 'BINS    :'+ SUBSTR(lcStr,1,10)
              Row = Row + 1
              IF LEN(lcStr) > 18
                =lfContuLoc() && Print the ,main style/color Skus for a specific account.
              ENDIF  
            ELSE
              Row = Row + 1  
            ENDIF
          ELSE
            Row = Row + 1  
          ENDIF
          =lfPrnStrn() &&  Print the ,main style/color Skus for a specific account.
          lcSkuSize=lcStrToPrn+' '+lcSkuSize
          @ ROW,03 SAY SUBSTR(lcSkuSize,1,86)
          Row = Row + 1  
        ENDIF
      ENDIF
    ENDIF  
    Row = Row + 1
    SELECT (lcTmpOrdL)
    XTOTQTY    = XTOTQTY+ TOTQTY
    lnXValue   = lnXValue + TOTPIK * PRICE
    lnPieces   = lnPieces + TOTPIK
    IF ROW > 47
      llNewDoc = .F.
      EXIT
    ENDIF
  ENDSCAN
  
  *- Check if not end of file and its the second page of 
  *- pick ticket then do not print the style again in the second page.
  IF !EOF() AND !llNewDoc
    SKIP
    llScale = .T.
  ENDIF

  IF EOF() .OR. lcOrder # Order
    llNewDoc = .T.
  ENDIF  

  *-- END PRINT LINE LOOP
  IF llNewDoc
    *-- To print the notepad.
    IF llRpOrdNot
      SELECT NotePad
      IF SEEK('B' + lcOrder)
        lnOldMemW = SET("MEMOWIDTH")
        SET MEMOWIDTH TO 75
        lnMemLins = MEMLINES(NOTEPAD.MNOTES)
        @ Row,03 SAY '* -- N O T E S -- *' 
        Row = Row + 1
        DO WHILE lnNotLine <= lnMemLins
          IF Row >= 53
            DO lpOldFoter && Print the form footer at in the middle of a spacific pick ticket. 
            DO lpHrdLabls && Print the Header labels.
          ENDIF
          IF SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),1,1) <> '*'
            @ ROW,03 SAY MLINE(MNOTES,lnNotLine)
            ROW = ROW + 1
          ENDIF
          lnNotLine = lnNotLine + 1
        ENDDO
        SET MEMOWIDTH TO lnOldMemW
      ENDIF
    ENDIF  
    lnNotLine = 1
    llEndPT = .T.
    DO lpNewFoter && Print the form footer at the end of a spacific pick ticket.          
  ELSE
    llEndPT = .F.
    DO lpOldFoter && Print the form footer at in the middle of a spacific pick ticket.   
    LOOP
  ENDIF
  
  llScale = .T.
  SELECT (lcTmpOrdL)
ENDDO
WAIT CLEAR

SET DEVICE TO SCREEN

SELECT ORDHDR
SET ORDER TO (lcSetOrdH)   
SELECT ORDLINE
SET ORDER TO (lcSetOrdL) 
SELECT (lnPrvfil) 

SET DEVICE TO &lcDevice
SET CENTURY &lcCent
*- Update the prtflag file in piktkt file
IF SET("Device")='PRINT'
  SELECT &lcPiktktNo
  SCAN
    =SEEK(&lcPiktktNo..PIKTKT,'PIKTKT')
    SELECT PIKTKT
    REPLACE PRTFLAG WITH 'P'
  ENDSCAN
ENDIF
CREATE TABLE (oAriaApplication.WorkDir+lcTempOut) (PAGE N(3),DATALINE C(100))
APPEND FROM (oAriaApplication.WorkDir+lcTempOut+'.txt') SDF
LOCATE

*- update the page field
lnPage = 1
SELECT &lcTempOut
LOCATE
SELECT &lcPiktktNo
SCAN
  SELECT &lcTempOut
  LOCATE
  LOCATE FOR  SUBSTR(DATALINE,67,6) = &lcPiktktNo..PIKTKT
  DO WHILE FOUND()
    SKIP - 5
    REPLACE PAGE WITH lnPage REST WHILE !CHR(12) $ DATALINE
    REPLACE PAGE WITH lnPage 
    lnPage = lnPage + 1
    CONTINUE
  ENDDO
ENDSCAN

SELECT &lcTempOut
LOCATE
REPLACE ALL DATALINE WITH CHRTRAN(DATALINE, CHR(12),'')
LOCATE
INDEX ON PAGE TAG PAGE
LOCATE

DO gfDispRe WITH EVAL('lcFormName')

*- To prevent the standard to re-run
SELECT &lcTmpOrdL
llAlpktk = .F.

*-- End Of Printing Form.
*:***************************************************************************
*: Name        : lpHrdLabls
*: Developer   : ABDOU ELGENDI - (ABD) 
*: Date        : 04/11/2000
*: Purpose     : Print the Header labels.
*:***************************************************************************
*: Called from : None.
*:***************************************************************************
*: Calls       : None.
*:***************************************************************************
*: Return      : None.
*:***************************************************************************
*: Example     : Do lpHrdLabls
*:***************************************************************************
PROCEDURE lpHrdLabls

@ 01,004 SAY IIF(llPrntComp , HLINE1 , '' )
@ 02,004 SAY IIF(llPrntComp , HLINE2 , '' )
@ 03,004 SAY IIF(llPrntComp , HLINE3 , '' )
@ 04,004 SAY IIF(llPrntComp , HLINE4 , '' )

@ 06,64 SAY SUBSTR(ALLTRIM(lcDIV_DATA),1,14)
@ 06,77 SAY lcPikTkt
@ 08,63 SAY SUBSTR(ORDHDR.APPROVAL,1,9)
@ 09,77 SAY ldDate

@ 11,08 SAY lcBTName
@ 11,51 SAY lcSTName
@ 12,08 SAY lcBTAddr1
@ 12,51 SAY lcSTAddr1
@ 13,08 SAY lcBTAddr2
@ 13,51 SAY lcSTAddr2
@ 14,08 SAY lcBTAddr3
@ 14,51 SAY lcSTAddr3
@ 19,04 SAY lcAccount
@ 19,11 SAY lcOrder

@ 19,17 SAY ORDHDR.REP1
@ 19,23 SAY ORDHDR.ENTERED
@ 19,32 SAY ORDHDR.START
@ 19,40 SAY ORDHDR.COMPLETE

@ 19,49 SAY SUBSTR(lcTerm_Data,1,14)
@ 19,63 SAY lcStore
@ 19,72 SAY IIF(ORDHDR.MultiPO,LEFT(lcCustPO,13),LEFT(OrdHdr.CUSTPO,13))  PICTURE '##########'
@ 19,83 SAY ORDHDR.DEPT 

ROW = 21
IF llScale
  I   = 1
  ROW = 21
  IF lnSca > 2
    lnSca = 2
  ENDIF
  FOR I = 1 TO lnSca
    Z = STR(I,1)
    IF ROW >=47
      llRet =.T.
      llNewDoc  = .F.
      llEndPT = .F.
      DO lpOldFoter && Print the form footer at in the middle of a spacific pick ticket. 
      EXIT
    ENDIF
    IF !EMPTY(lcScale&Z)
      @ ROW,23 SAY LEFT(lcScale&Z,1)
      
      lcScale = GETSCALE(lcScale&Z,SPACE(1))
      @ ROW,25 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,4),4,' ')
      @ ROW,29 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,4),4,' ')
      @ ROW,34 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,4),4,' ')
      @ ROW,38 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,4),4,' ')
      @ ROW,43 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,4),4,' ')
      @ ROW,47 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,4),4,' ')
      @ ROW,52 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,4),4,' ')
      @ ROW,56 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,4),4,' ')
      ROW = ROW+1
    ENDIF
  ENDFOR  
  lcSclStrn=lcScale
  llScale =.F.
  ROW=ROW+1
ELSE
  ROW = 23
ENDIF 
lcSclStrn=lcScale

RETURN
*-- END OF lpHrdLabls.

*:***************************************************************************
*: Name        : lpOldFoter
*: Developer   : ABDOU ELGENDI - (ABD) 
*: Date        : 04/11/2000
*: Purpose     : Print the form footer at in the middle of a spacific pickticket. 
*:***************************************************************************
*: Called from : None.
*:***************************************************************************
*: Calls       : None.
*:***************************************************************************
*: Return      : None.
*:***************************************************************************
*: Example     : Do lpOldFoter
*:***************************************************************************
*
PROCEDURE lpOldFoter

@ 57,15 SAY '*** CONTINUED NEXT PAGE ***'
ROW = ROW + 1

*-- End Of lpOldFoter.

*:***************************************************************************
*: Name        : lpNewFoter
*: Developer   : ABDOU ELGENDI - (ABD) 
*: Date        : 04/11/2000
*: Purpose     : Print the form footer at the end of a spacific pickticket. 
*:***************************************************************************
*: Called from : None.
*:***************************************************************************
*: Calls       : None.
*:***************************************************************************
*: Return      : None.
*:***************************************************************************
*: Example     : Do lpNewFoter
*:***************************************************************************
*
PROCEDURE lpNewFoter
*-- Not to print the total amount of the piktkt if the 
*-- user does not want to print the style prices.
@ 53,003 SAY lcRpMsg1
@ 54,003 SAY lcRpMsg2
@ 55,003 SAY lcRpMsg3

=SEEK('O'+lcOrder,'ORDHDR')

lnCol = 14
IF SUBSTR(ALLTRIM(OrdHdr.Note1),1, 1) # '*'
  @ 59,lnCol SAY OrdHdr.Note1
  lnCol = 45
ENDIF
@ 59,lnCol SAY IIF(SUBSTR(ALLTRIM(OrdHdr.Note2),1, 1) # '*', SUBSTR(OrdHdr.Note2,1,25), '')
@ 59,079 SAY lnPieces PICTURE '999999' 
@ 63,063 SAY SUBSTR(lcShp_Data,1,11) 
@ 63,079 SAY IIF(llRpStyPrc ,lnXValue,'') PICTURE '99999.99'
lnXValue = 0.00
ROW = ROW + 1

*-- End Of lpNewFoter.
*:*************************************************************************
*: Name        : lpPrtSku
*: Developer   : ABDOU ELGENDI - (ABD) 
*: Date        : 04/11/2000
*: Purpose     : Print the ,main style/color Skus for a specific account.
*:***************************************************************************
*: Called from : None.
*:***************************************************************************
*: Calls       : None.
*:***************************************************************************
*: Return      : None.
*:***************************************************************************
*: Example     : Do lpPrtSku
*:***************************************************************************
*
PROCEDURE lpPrtSku

IF !SEEK('S'+PIKTKT.Account+&lcTmpOrdL..Style,'Spck_Lin')
  *--- If we print 2 p/t and the style of the second p/t have no
  *--- Sku, leave it empty and do not print the sku of the style
  *--- In the first p/t.
  lcStrToPrn = " "
  RETURN
ENDIF

SELECT Spck_Lin
IF EMPTY(Sku)
  =SEEK('S'+Style.Scale,'Scale')
  =SEEK('M'+PIKTKT.Account,'Customer')

  PRIVATE lcAlasSpkH , lcOrdrSpkH
  lcAlasSpkH = SELECT(0)
  SELECT SPCK_HDR
  lcOrdrSpkH = ORDER('Spck_Hdr')
  SET ORDER TO TAG Sku_style IN SPCK_HDR
  IF SEEK('S'+PIKTKT.Account+&lcTmpOrdL..Style,'Spck_Hdr')
    lnLenDm1 = LEN(ALLTRIM(Spck_Hdr.pack_id))
  ELSE
    lcStrToPrn = " "
    RETURN
  ENDIF
  SET ORDER TO TAG &lcOrdrSpkH
  SELECT(lcAlasSpkH)

  = SEEK('S'+PIKTKT->Account+&lcTmpOrdL..Style,'Spck_Lin')
  lcStrToPrn = 'SKU#: ' + SUBSTR(Pack_Id,1,lnLenDm1)
ELSE
  @ ROW,04 SAY SUBSTR(Sku,1,8)
  @ ROW,20 SAY 'CUSTOMER SKU #'
ENDIF
RETURN

*-- End Of lpPrtSku.
*:***************************************************************************
*: Name        : lfPrnStrn
*: Developer   : ABDOU ELGENDI - (ABD) 
*: Date        : 04/11/2000
*: Purpose     : Print the ,main style/color Skus for a specific account.
*:***************************************************************************
*: Called from : None.
*:***************************************************************************
*: Calls       : None.
*:***************************************************************************
*: Return      : None.
*:***************************************************************************
*: Example     : Do lfPrnStrn
*:***************************************************************************
*
FUNCTION lfPrnStrn
lcAlias=ALIAS()
lcSkuSize =' '
lcKey     =' '

IF SEEK('S'+PIKTKT.Account+&lcTmpOrdL..Style,'Spck_Lin')
  lcKey='S'+PIKTKT.Account+&lcTmpOrdL..Style
  lnSep=1
  Q=1
  W=STR(Q,1)
  X=1
  Z=STR(X,1) 
  SELECT Spck_Lin

  SCAN REST WHILE 'S'+PIKTKT.Account+&lcTmpOrdL..Style = lcKey
    IF &lcTmpOrdL..Qty&Z > 0

      PRIVATE lcAlasSpkh , lcRecNoH
      lcAlasSpkh = SELECT(0)
      SELECT SPCK_LIN
      =SEEK(lckey)
      lcRecNoH = RECNO()
      SCAN REST WHILE TYPE + ACCOUNT + STYLE + PACK_ID = lcKey
        IF SPCK_LIN.QTY&Z = 1
          lcSkuSize = lcSkuSize+'S'+W+':' + SUBSTR(ALLTRIM(Pack_Id),lnLenDm1+1) + ' '
          GOTO lcRecNoH IN SPCK_LIN
          EXIT
        ENDIF
      ENDSCAN

    ENDIF
    lnSep=lnSep+6
    X=X+1
    Z=STR(X,1) 
    Q=Q+1
    W=STR(Q,1)
    IF Z='9'
      EXIT
    ENDIF  
  ENDSCAN
  lcSkuSize=ALLTRIM(lcSkuSize)
ENDIF
SELECT (lcAlias)
RETURN
*-- End Of lfPrnStrn.

*:***************************************************************************
*: Name        : lfGetLoc
*: Developer   : ABDOU ELGENDI - (ABD) 
*: Date        : 04/11/2000
*: Purpose     : Print the ,main style/color Skus for a specific account.
*:***************************************************************************
*: Called from : None.
*:***************************************************************************
*: Calls       : None.
*:***************************************************************************
*: Return      : None.
*:***************************************************************************
*: Example     : =lfGetLoc()
*:***************************************************************************
*
FUNCTION lfGetLoc
lcAlias=ALIAS()
lcStr=" "
SELECT WhsLoc
IF SEEK(&lcTmpOrdL..Style)
  SCAN REST WHILE Style = &lcTmpOrdL..Style
    lcStr = lcStr +" "+cLocation
  ENDSCAN
  lcStr=ALLTRIM(lcStr)
  lnLen=LEN(lcStr)
ENDIF
SELECT (lcAlias)

*-- End Of lfGetLoc.
*:***************************************************************************
*: Name        : lfContuLoc
*: Developer   : ABDOU ELGENDI - (ABD) 
*: Date        : 04/11/2000
*: Purpose     : Print the ,main style/color Skus for a specific account.
*:***************************************************************************
*: Called from : None.
*:***************************************************************************
*: Calls       : None.
*:***************************************************************************
*: Return      : None.
*:***************************************************************************
*: Example     : =lfContuLoc()
*:***************************************************************************
*
FUNCTION lfContuLoc

FOR I = 12 TO lnLen  
   @ ROW ,04 SAY SUBSTR(lcStr,I,86) 
   I=I+86
   Row = Row + 1
ENDFOR

*-- End OF lfContuLoc.
*:***************************************************************************
*: Name        : lfEvalSegs
*: Developer   : ABDOU ELGENDI - (ABD) 
*: Date        : 04/11/2000
*: Purpose     : Evaluate NonMajor Type and variables.
*:***************************************************************************
*: Called from : [Option Grid] lcDummy variable.
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfEvalSegs()
*:***************************************************************************
FUNCTION lfEvalSegs

STORE 0  TO  lnMajSeg,lnNonMajSt,lnMajorLen,lnFreeLen,lnColorLen
STORE "" TO lcMajPict,lcFree_Clr,lcNonMajPi,lcNonMajTl,lcColorTlt

lnMajSeg    = gfItemMask('SM')  && No. of major segments.
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcMajPict  = gfItemMask("PM")
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF                     
  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
lnMajorLen = LEN(lcMajPict)
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen

IF EMPTY (lcNonMajTl)
  lcColorTlt = 'Color'
ELSE 
  lcColorTlt = ALLTRIM(lcNonMajTl)
ENDIF

*RETURN ''

*-- End of lfEvalSegs.
*:***************************************************************************
*: Name        : lfClearRep
*: Developer   : ABDOU ELGENDI - (ABD) 
*: Date        : 04/11/2000
*: Purpose     : Close all open files in this form.
*:***************************************************************************
*: Called from : [Option Grid] lcDummy variable.
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfClearRep()
*:***************************************************************************
*
FUNCTION lfClearRep

*-- Close Open Files [Begin]
IF (ASCAN(laSelFile,'CUSTOMER') = 0) AND USED('CUSTOMER')
  USE IN CUSTOMER
ENDIF

IF (ASCAN(laSelFile,'SKUTMPL') = 0) AND USED('SKUTMPL')
  USE IN SKUTMPL
ENDIF

IF (ASCAN(laSelFile,'SPCK_LIN') = 0) AND USED('SPCK_LIN')
  USE IN SPCK_LIN
ENDIF

IF (ASCAN(laSelFile,'WHSLOCST') = 0) AND USED('WHSLOCST')
  USE IN WHSLOCST
ENDIF

IF (ASCAN(laSelFile,'SCALE') = 0) AND USED('SCALE')
  USE IN SCALE
ENDIF

IF (ASCAN(laSelFile,'PIKTKT') = 0) AND USED('PIKTKT')
  USE IN PIKTKT
ENDIF

*-- Close Open Files .[END]
*-- End Of lfClearRep.
*:***************************************************************************
*: Name        : lfvskuSz.
*: Developer   : ABDOU ELGENDI - (ABD) 
*: Date        : 04/11/2000
*: Purpose     : Show or not shoew the sku by size.
*:***************************************************************************
*: Called from : [Option Grid] Print Color Desc
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfvskuSz()
*:***************************************************************************
*
FUNCTION lfvskuSz
PARAMETER lcParam

CLEAR READ
lcParam = .T.
RETURN lcParam
*--End Of lfvskuSz.
*:***************************************************************************