*:************************************************************************
*: PROGRAM   : ALPKTKRE.PRG (C037402)
*: DESC.     : Customized Sales Order Allocation Picking Ticket Form For REG10
*: System    : Aria Apparel System    (A27)
*: Module    : SALES ORDER ALLOCATION (AL)
*: Developer : Nader Nabil        - (NNA)
*: DATE      : 01/12/2004
*:************************************************************************
*: Calls : 
*:         FUNCTION  : lfPrnStrn , lfGetLoc , lfContuLoc , lfEvalSegs.
*:         --------------------------------------------------------------
*:         PROCEDURE : lpOldFoter , lpNewFoter , lpPrtSku.
*:************************************************************************
*:Modifications:
*B130379,1 EIH 02/15/2006 Fix bug of printing only 2 size scale in picking ticket form 
*:************************************************************************
*
*-- Set the memory variables.
*--llRpPrtClr :- print Color Desc.
*-- Call this function once per OG session.
= (TYPE("lnMajSeg") = "C") AND lfEvalSegs()

STORE '' To lcStrToPrn,lcSkuSize,lcSclStrn,lcStr

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

*-- MAIN LOOP.
SELECT (lcTmpOrdL)

SET DEVICE TO PRINT

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
    SELECT ORDHDR
 
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
    lcDIV_DATA  = gfCodDes(ORDHDR.CDIVISION,'CDIVISION')
    

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
    @ ROW,1 SAY SUBSTR(Style.Style,1,lnMajorLen)
    @ ROW,lnMajorLen+1 SAY SUBSTR(Style.Style,lnNonMajSt,lnColorLen)
    *ash1
    @ ROW,20 SAY LEFT(lcScale,1)
    @ ROW,21 SAY PIK1   PICTURE '@Z 9999'
    @ ROW,25 SAY PIK2   PICTURE '@Z 9999'
    @ ROW,29 SAY PIK3   PICTURE '@Z 9999'
    @ ROW,33 SAY PIK4   PICTURE '@Z 9999'
    @ ROW,38 SAY PIK5   PICTURE '@Z 9999'
    @ ROW,42 SAY PIK6   PICTURE '@Z 9999'
    @ ROW,47 SAY PIK7   PICTURE '@Z 9999'
    @ ROW,51 SAY PIK8   PICTURE '@Z 9999'
    @ ROW,55 SAY TOTPIK PICTURE '99999'
    *ash1
    @ ROW,63 SAY IIF(llRpStyPrc,PRICE,'') PICTURE '9999.99'
    @ ROW,71 SAY TOTPIK PICTURE '9999' 
    @ ROW,76 SAY IIF(SEEK(Style,'Style'),IIF(Style.Qty_Ctn <> 0, ;
                     CEILING(TotPik/Style.Qty_Ctn),0),0) PICTURE '9999'                 
    ROW = ROW + 1

    IF !llRpSkuBck 
      @ ROW,01 SAY 'STYLE DESC.:'+lcStyDesc
      @ ROW,33 SAY IIF(llRpPrtClr,'CLR DESC.: ' +ALLTRIM(SUBSTR(lcClrDesc,1,25)), '')
      IF llRpStyLoc
        =lfGetLoc() 
        IF !EMPTY(lcStr)
          @ ROW,60 SAY 'BINS    :'+ SUBSTR(lcStr,1,10)
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
        @ ROW,1 SAY 'PACK ID#:'+Spck_Lin.Pack_Id
        @ ROW,25 SAY 'STYLE DESC.:'+lcStyDesc
        @ ROW,59 SAY IIF(llRpPrtClr,'CLR DESC.:' + SUBSTR(lcClrDesc,1,10), '')
        Row = Row + 1
      ELSE
        *--- Procedure to print the Style/Color Sku no.      
        DO lpPrtSku  && Print the ,main style/color Skus for a specific account.
        IF !llRpSkuSiz
          @ ROW,00 SAY SUBSTR(lcStrToPrn,1,IIF(LEN(lcStrToPrn)>=40,40,LEN(lcStrToPrn)))
          @ ROW,IIF(LEN(lcStrToPrn)>=40,41,LEN(lcStrToPrn)+2) SAY 'STYLE DESC.:'+lcStyDesc
          lcCol=IIF(LEN(lcStrToPrn)>=40,73,LEN(lcStrToPrn)+34)
          @ ROW,lcCol SAY IIF(llRpPrtClr,'CLR DESC.:' + SUBSTR(lcClrDesc,1,76-lcCol), '')
          Row = Row + 1
          IF llRpStyLoc
            =lfGetLoc()
            IF !EMPTY(lcStr)
              @ ROW,00 SAY 'BINS    :'+ SUBSTR(lcStr,1,85)
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
          @ ROW,00 SAY 'STYLE DESC.:'+lcStyDesc
          @ ROW,33 SAY IIF(llRpPrtClr,'CLR DESC.:'+ ALLTRIM(SUBSTR(lcClrDesc,1,25)), '')
          IF llRpStyLoc
            =lfGetLoc()
            IF !EMPTY(lcStr)
              @ ROW,60 SAY 'BINS    :'+ SUBSTR(lcStr,1,10)
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
          @ ROW,00 SAY SUBSTR(lcSkuSize,1,86)
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
        @ Row,00 SAY '* -- N O T E S -- *' 
        Row = Row + 1
        DO WHILE lnNotLine <= lnMemLins
          IF Row >= 53
            DO lpOldFoter && Print the form footer at in the middle of a spacific pick ticket. 
            DO lpHrdLabls && Print the Header labels.
          ENDIF
          IF SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),1,1) <> '*'
            @ ROW,00 SAY MLINE(MNOTES,lnNotLine)
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

*-- End Of Printing Form.
*:***************************************************************************
*: Name        : lpHrdLabls
*: Developer   : Nader Nabil   - (NNA)
*: Date        : 01/12/2004
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

@ 00,001 SAY IIF(llPrntComp , HLINE1 , '' )
@ 01,001 SAY IIF(llPrntComp , HLINE2 , '' )
@ 02,001 SAY IIF(llPrntComp , HLINE3 , '' )
@ 03,001 SAY IIF(llPrntComp , HLINE4 , '' )

@ 05,61 SAY SUBSTR(ALLTRIM(lcDIV_DATA),1,7)
@ 05,75 SAY lcPikTkt
@ 07,60 SAY SUBSTR(ORDHDR.APPROVAL,1,9)
@ 07,73 SAY ldDate
@ 10,05 SAY lcBTName
@ 10,48 SAY lcSTName
@ 11,05 SAY lcBTAddr1
@ 11,48 SAY lcSTAddr1
@ 12,05 SAY lcBTAddr2
@ 12,48 SAY lcSTAddr2
@ 13,05 SAY lcBTAddr3
@ 13,48 SAY lcSTAddr3
@ 18,01 SAY lcAccount
@ 18,07 SAY lcOrder
*ash1
@ 18,14 SAY ORDHDR.REP1
*Nader
@ 18,17 SAY ORDHDR.REP2
*ash1
@ 18,21 SAY ORDHDR.ENTERED
@ 18,30 SAY ORDHDR.START
@ 18,38 SAY ORDHDR.COMPLETE
@ 18,47 SAY SUBSTR(lcTerm_Data,1,13)
@ 18,61 SAY lcStore
@ 18,69 SAY IIF(ORDHDR.MultiPO,LEFT(lcCustPO,13),LEFT(OrdHdr.CUSTPO,13))  PICTURE '##########'
@ 18,79 SAY ORDHDR.DEPT 
*Nader 
ROW = 20
IF llScale
  I   = 1
  ROW = 20
  
  *B130379,1 EIH 02/15/2006 Fix bug of printing only 2 size scale in picking ticket form. [Begin] 
  *IF lnSca > 2
  *  lnSca = 2
  *ENDIF
  *B130379,1 EIH 02/15/2006 [End] 
  
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
      *ash1
      @ ROW,20 SAY LEFT(lcScale&Z,1)
      
      lcScale = GETSCALE(lcScale&Z,SPACE(1))
      @ ROW,21 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,4),4,' ')
      @ ROW,25 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,4),4,' ')
      @ ROW,30 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,4),4,' ')
      @ ROW,34 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,4),4,' ')
      @ ROW,37 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,4),4,' ')
      @ ROW,41 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,4),4,' ')
      @ ROW,46 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,4),4,' ')
      @ ROW,51 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,4),4,' ')
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
*: Developer   : Nader Nabil   - (NNA)
*: Date        : 01/12/2004
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

@ 57,12 SAY '*** CONTINUED NEXT PAGE ***'
ROW = ROW + 1

*-- End Of lpOldFoter.
*:***************************************************************************
*: Name        : lpNewFoter
*: Developer   : Nader Nabil   - (NNA)
*: Date        : 01/12/2004
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
@ 53,000 SAY lcRpMsg1
@ 54,000 SAY lcRpMsg2
@ 55,000 SAY lcRpMsg3

=SEEK('O'+lcOrder,'ORDHDR')

lnCol = 11
IF SUBSTR(ALLTRIM(OrdHdr.Note1),1, 1) # '*'
  @ 58,lnCol SAY OrdHdr.Note1
  lnCol = 42
ENDIF
@ 58,lnCol SAY IIF(SUBSTR(ALLTRIM(OrdHdr.Note2),1, 1) # '*', SUBSTR(OrdHdr.Note2,1,25), '')
@ 58,076 SAY lnPieces PICTURE '999999' 
@ 62,060 SAY SUBSTR(lcShp_Data,1,11) 
@ 62,076 SAY IIF(llRpStyPrc ,lnXValue,'') PICTURE '99999.99'
lnXValue = 0.00
ROW = ROW + 1

*-- End Of lpNewFoter.
*:*************************************************************************
*: Name        : lpPrtSku
*: Developer   : Nader Nabil   - (NNA)
*: Date        : 01/12/2004
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
    lnDime1 = LEN(ALLTRIM(Spck_Hdr.pack_id))
  	lnLenDm1 = lnDime1
  ELSE
    lcStrToPrn = " "
    RETURN
  ENDIF
  SET ORDER TO TAG &lcOrdrSpkH
  SELECT(lcAlasSpkH)

  = SEEK('S'+PIKTKT->Account+&lcTmpOrdL..Style,'Spck_Lin')
  lcStrToPrn = 'SKU#: ' + SUBSTR(Pack_Id,1,lnDime1)
  lcStrToPrn = IIF(LEN(lcStrToPrn)>65,SUBSTR(lcStrToPrn,1,65),ALLTRIM(lcStrToPrn))
  lnLength   = LEN(lcStrToPrn) + 2
  lnLocCol   = LEN(lcStrToPrn) + 5
ELSE
  @ ROW,01 SAY SUBSTR(Sku,1,8)
  lnLocCol = 35
  @ ROW,17 SAY 'CUSTOMER SKU #'
ENDIF
RETURN

*-- End Of lpPrtSku.
*:***************************************************************************
*: Name        : lfPrnStrn
*: Developer   : Nader Nabil   - (NNA)
*: Date        : 01/12/2004
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
*: Developer   : Nader Nabil   - (NNA)
*: Date        : 01/12/2004
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
*: Developer   : Nader Nabil   - (NNA)
*: Date        : 01/12/2004
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
   @ ROW ,00 SAY SUBSTR(lcStr,I,86) 
   I=I+86
   Row = Row + 1
ENDFOR

*-- End OF lfContuLoc.
*:***************************************************************************
*: Name        : lfEvalSegs
*: Developer   : Nader Nabil   - (NNA)
*: Date        : 01/12/2004
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
*
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

*-- End of lfEvalSegs.
*:***************************************************************************
*: Name        : lfClearRep
*: Developer   : Nader Nabil   - (NNA)
*: Date        : 01/12/2004
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
*: Developer   : Nader Nabil   - (NNA)
*: Date        : 01/12/2004
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
