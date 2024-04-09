*:***************************************************************************
*: Program file  : ARPINVTH.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR NAT NAST COMPANY.
*: Date          : 05/05/2003
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Khalid Mohi El-Din Mohamed (KHM)
*: Tracking Job Number: C102832
*:***************************************************************************
*: Example : DO ARPINVTH
*:***************************************************************************
*: Modifications :
*:C121795,1 BWA 04/07/2004 Change the position of the color in the previewing.
*:C125991,1 BWA 02/21/2004 Change the position of the Sold To box in FRX
***************************************************************************

*C121795,1 BWA 04/07/2004 Variables hold the postion color and it's length.[START]
*--The Color Length.
STORE "" TO lnClrLnCd , lnClrPosCd
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnCd  = LEN(laItemSeg[lnCount,3])
    lnClrPosCd = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
*C121795,1 BWA 04/07/2004.[END]


llUseArh = .F.
IF !USED('ARHIST')
  =gfOpenFile(gcDataDir+"ARHIST",gcDataDir+"ARHISTT",'SH')
  llUseArh = .T.
ENDIF  

SELECT ARHIST
IF !llUseArh
  SET ORDER TO TAG ARHISTT
ENDIF

=lfCreatTmp()
=lfCollData()  && Collect the data.

*C121795,1 BWA 04/07/2004 Function handling the color changes postion.[START]
=lfChngStyl()  &&Collect the invoice lines and change the postion style.
*C121795,1 BWA 04/07/2004.[END]

SELECT INVHDR
SET RELATION TO ACCOUNT + INVOICE INTO (lcCrdtInv) ADDITIVE

IF llUseArh
  USE IN ARHIST
ENDIF
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 05/05/2003
*! Purpose   : Create Temp. file that hold the Credit data.
*!*************************************************************
*! Example            : =lfCreatTmp()
*!*************************************************************
FUNCTION lfCreatTmp
PRIVATE lcAlias

lcAlias = ALIAS()

CREATE CURSOR (lcCrdtInv) (Invoice C(6) , Account C(5) , TotCrd N(13,2))
INDEX ON ACCOUNT + Invoice TAG Invoice OF (gcWorkDir + lcCrdtInv)

SELECT(lcAlias)

*!*************************************************************
*! Name      : lfCollData
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 05/05/2003
*! Purpose   : To collect the cash recipt
*!*************************************************************
*! Example            : =lfCollData()
*!*************************************************************
FUNCTION lfCollData
PRIVATE lcAlias , lcRcNoHdr

lcAlias = ALIAS()
SELECT INVHDR
lcRcNoHdr = EVAL(KEY())

SCAN FOR &lcRpExp
  SELECT ARHIST
  IF SEEK(INVHDR.ACCOUNT + INVHDR.INVOICE , 'ARHIST')
    SUM REST AMOUNT WHILE ACCOUNT + TRAN + CINSTALNO = INVHDR.ACCOUNT + INVHDR.INVOICE FOR TranType='1' TO lnCrdAmt
    
    IF lnCrdAmt # 0
      INSERT INTO (lcCrdtInv) (Invoice, Account, TotCrd) VALUES;
                              (INVHDR.INVOICE, INVHDR.ACCOUNT, lnCrdAmt)
    ENDIF
  ENDIF
ENDSCAN

=SEEK(lcRcNoHdr)
SELECT(lcAlias)

*--End of lfCollData.
*C121795,1 BWA 04/07/2004 Function handling the color changes postion.[START]
*!*************************************************************
*! Name        : lfChngStyl
*! Developer   : BASSEM RAAFAT ERNEST(BWA)
*! Date        : 04/07/2004
*! Purpose     : To handle changing the color postion.
*!*************************************************************
*! Called from : ARPINTH.PRG
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Refer to    : C121795
*!*************************************************************
*! Example     : lfChngStyl
*!*************************************************************
FUNCTION lfChngStyl
PRIVATE lcAlias , lnCUSRec , lnRecInvH , lcOldSkip 

lcAlias = ALIAS()
lnCUSRec = RECNO("INVLINE")
lnRecInvH = RECNO("INVHDR")

SELECT INVHDR
lcOldSkip = SET('SKIP')
SET SKIP TO

CREATE CURSOR (lcInvTemp) (Invoice C(6) , LinenoNn N(6) , StylColor C(19) , ClrDescrp C(8) , ALTSTYLE C(19))
INDEX ON Invoice + STR(linenoNn,6) TAG Invoice OF (gcWorkDir + lcInvTemp)

SELECT INVHDR
SCAN FOR &lcRpExp
  SELECT INVLINE
  IF(SEEK(INVHDR.INVOICE , 'INVLINE'))
    SCAN REST WHILE INVLINE.INVOICE + STR(INVLINE.LINENO,6) = INVHDR.INVOICE
	  WAIT WINDOW 'Selecting Records For The Report ...' +  INVLINE.INVOICE NOWAIT
      INSERT INTO (lcInvTemp) (Invoice , LinenoNn , StylColor , ClrDescrp , ALTSTYLE) VALUES;
                              (INVLINE.INVOICE, INVLINE.lineno , LEFT(INVLINE.STYLE,lnMajor) + "-" +;
                               SUBSTR(INVLINE.STYLE,16,3) + " " +; 
                               SUBSTR(INVLINE.STYLE,lnClrPosCd,lnClrLnCd) ,;
                               LEFT(gfCodDes(SUBSTR(INVLINE.STYLE,lnClrPosCd,lnClrLnCd),'COLOR'),8) ,;
                               IIF(!EMPTY(INVLINE.ALTSTYLE) , LEFT(INVLINE.ALTSTYLE,lnMajor) + "-" +;
                               SUBSTR(INVLINE.ALTSTYLE,16,3) + " " +; 
                               SUBSTR(INVLINE.ALTSTYLE,lnClrPosCd,lnClrLnCd) , SPACE(0)) )
    ENDSCAN
  ENDIF  
  SELECT INVHDR
ENDSCAN

SELECT INVHDR
SET SKIP TO &lcOldSkip
IF lnRecInvH <> 0 
  GOTO lnRecInvH IN INVHDR
ENDIF

IF lnCUSRec <> 0 
  GOTO lnCUSRec IN INVLINE
ENDIF
WAIT CLEAR
SELECT(lcAlias)

SELECT INVLINE
SET RELATION TO INVOICE + STR(lineno,6) INTO (lcInvTemp) ADDITIVE

*--End of lfChngStyl.
*C121795,1 BWA 04/07/2004.[END]