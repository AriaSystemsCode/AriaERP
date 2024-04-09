*!**************************************************************************
*! Name      : EFMAIN.PRG
*! Developer : BASSEM RAFAAT
*! Date      : 03/20/2000
*! Purpose   : EILEEN FISHER Custom Process Program.
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
*B603656,1 BWA 05/25/2000 Modifications on the calculations for the custom process
*B603656,1                for the customized sales j. for (Fresh Perduce).
*C102390,1 TMI 07/30/2001 Change the default value of the dyelot check box to be checked by default
*!**************************************************************************

PARAMETER LCEVNTFUN,LCFUNPARS

LCFUNPARS  = IIF(TYPE('lcFunPars') = 'C',LCFUNPARS,'')
LCFUNTORUN = 'lf'+ALLT(LCEVNTFUN)+'('+LCFUNPARS+')'

*--Run the function.
LLRETVALUE = EVAL(LCFUNTORUN)

RETURN LLRETVALUE


*!*************************************************************
*! Name      : lfGetPrice
*! Developer : BASSEM RAFAAT
*! Date      : 03/20/2000
*! Purpose   : EILEEN FISHER Custom calculate for gross and net amount.
*!           : from GROSS PROFIT REPORT.
*!*************************************************************
*! Called from  : EFMAIN.PRG
*! Program name : ARGROSSP.PRG
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lfGetPrice
*!*************************************************************
*C101712,1 BWA 03/20/2000 A custom process to calculate the gross and the net amount.FUNCTION lfGetPrice
*!*************************************************************

FUNCTION LFGETPRICE

*B603656,1 BWA 05/25/2000 Modifications on the calculations for the custom process
*B603656,1                for the customized sales j. for (Fresh Perduce).
*lnGross = lnQty*IIF(llCallGfam,gfAmntDisp(Gros_Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Gros_Price)
*lnNet   = lnQty*IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price)*(1 - (DiscPcnt/100)) * (1 - (Trde_Disc/100))

PRIVATE LCDISCUNTA,LCTRADEDIS,LCTOTDISNT
STORE 0 TO LCDISCUNTA,LCTRADEDIS,LCTOTDISNT

LNGROSS    = LNQTY*IIF(LLCALLGFAM,GFAMNTDISP(STYLE.PRICEA, LCRPCURR , LDRPEXDATE , LCRPTMPNAM),STYLE.PRICEA)
LNMERCH    = LNQTY*IIF(LLCALLGFAM,GFAMNTDISP(PRICE, LCRPCURR , LDRPEXDATE , LCRPTMPNAM),PRICE)
LCDISCUNTA = (LNGROSS - LNMERCH) + LNMERCH*INVHDR.DISCPCNT/100
LCTRADEDIS = (LNGROSS-LCDISCUNTA)*INVHDR.TRDE_DISC/100
LCTOTDISNT = LCDISCUNTA + LCTRADEDIS
LNNET      = LNGROSS - LCTOTDISNT
*B603656,1 [END]
*-- End function lfGetPrice --*

*B603656,1 Make relations to the style and invhdr to get the sales price
*B603656,1 and the trad discount value.
*!*************************************************************
*! Name      : lfSalesRel
*! Developer : BASSEM RAFAAT
*! Date      : 05/24/2000
*! Purpose   : EILEEN FISHER Custom calculate for gross and net amount.
*!           : for GROSS PROFIT REPORT.
*!*************************************************************
*! Called from  : EFMAIN.PRG
*! Program name : ARGROSSP.PRG
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lfSalesRel
*!*************************************************************
*B603656,1 BWA 05/25/2000 Modifications on the calculations for the custom process
*B603656,1                for the customized gross profit for (Fresh Perduce).
*!*************************************************************
FUNCTION LFSALESREL

SELECT &INVLTEMP
SET RELATION TO STYLE INTO STYLE ADDITIVE
SET RELATION TO INVOICE INTO INVHDR ADDITIVE
LLRETUV = .T.
*B603656,1 [END]
*-- End function lfSalesRel --*

*!*************************************************************
*! Name      : lfGetJoPrc
*! Developer : BASSEM RAFAAT
*! Date      : 03/20/2000
*! Purpose   : EILEEN FISHER Custom calculate for gross and net amount.
*!           : from the Sales Journal Report.
*!*************************************************************
*! Called from  : EFMAIN.PRG
*! Program name : ARSJOUR.PRG
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lfGetJoPrc
*!*************************************************************
*C101713,1 A custom process to calculate the shipped amount and the Mrech. Discount.[START]
*!*************************************************************
FUNCTION LFGETJOPRC
PRIVATE LCALIAS,LNSHIPAMT,LNLINESDISC
LCALIAS = SELECT(0)

*B603656,1 BWA 05/25/2000 Open style file [START]
IF !USED(LCSTYFILE)
  = GFOPENFILE(GCDATADIR+'Style' ,GCDATADIR+'Style', 'SH', @LCSTYFILE, .T.)
ENDIF
*B603656,1 [END]

IF !USED(LCINVLINE)
  = GFOPENFILE(GCDATADIR+'Invline' ,GCDATADIR+'Invline', 'SH', @LCINVLINE, .T.)
ENDIF
SELECT (LCINVLINE)

*B603656,1 BWA 05/25/2000 Set relation into style file [START]
SET RELATION TO STYLE INTO (LCSTYFILE) ADDITIVE
*B603656,1 [END]

=SEEK(INVHDR.INVOICE)

*B603656,1 BWA 05/25/2000 Modifications on the calculations.[START]
*SUM REST TotQty * Gros_Price,TotQty*(Gros_Price-Price) TO m.ShipAmt,lnLinesDisc WHILE invoice+STR(lineno,6) = InvHdr.Invoice
*m.Discount = m.Discount - lnLinesDisc

SUM REST TOTQTY * &LCSTYFILE..PRICEA ,TOTQTY*(&LCSTYFILE..PRICEA-PRICE) TO LNSHIPAMT,LNLINESDISC ;
  WHILE INVOICE+STR(LINENO,6) = INVHDR.INVOICE
IF INVHDR.STATUS = 'V'
  M.VSHIPAMT  = LNSHIPAMT
  M.VDISCOUNT = M.VDISCOUNT - LNLINESDISC
ELSE
  M.SHIPAMT = LNSHIPAMT
  M.DISCOUNT = M.DISCOUNT - LNLINESDISC
ENDIF
SET RELATION OFF INTO (LCSTYFILE)
*B603656,1 [END]

SELECT (LCALIAS)
*-- End function lfGetJoPrc --*

*:**************************************************************************
*:* Name        : lfInitDye
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/30/2001
*:* Purpose     : Initialize Dyelot flag  with 'Y' Only in Add mode for style
*:*             : not copied from other styles
*:***************************************************************************
*:* Called from : ICSTYLE  // as a trigger
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfInitDye()
*:***************************************************************************
*:* ENTRY       : C102390,1 TMI
*:***************************************************************************
FUNCTION LFINITDYE

IF LASCRMODE[4] AND EMPTY(LCFRMSTYLE)
  LADATA[23] = 'Y'
ENDIF

*-- end of lfInitDye.



*:**************************************************************************
*:* Name        : lfAddCrtNo
*:* Developer   : AEH - Amgad EL Halawany
*:* Date        : 05/30/2005
*:* Purpose     : Add Carton Number to File
*:*             :
*:***************************************************************************
*:* Called from : ALPLIST // as a trigger
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :
*:***************************************************************************
*:* ENTRY       :C125077
*:***************************************************************************
FUNCTION LFADDCRTNO

IF !USED("CRTINFO")
  =GFOPENFILE(GCDATADIR+'CRTINFO',GCDATADIR+'CRTINFOP','SH')
ENDIF
IF !USED("pack_lin")
  =GFOPENFILE(GCDATADIR+'pack_lin',GCDATADIR+'packstyle','SH')
ENDIF

IF LCPROGNAME = 'ALPLIST'
  IF LASCRMODE[3]
    SELECT PACK_LIN
    =SEEK(LADATA[1],'PACK_LIN')
    SCAN REST WHILE PACK_NO = LADATA[1]
      LCCURINDEX = LADATA[1]+LADATA[4]+ALLTRIM(STR(PACK_LIN.NO_CART,1,4))
      IF !SEEK(LCCURINDEX,'CRTINFO','CRTINFOP')
        LCTEMP = GFSEQUENCE('CcartSeq')
        INSERT INTO CRTINFO (PACK_NO,ACCOUNT,NCARTNO,CCARTSEQ,CADD_USER,DADD_DATE,CADD_TIME) VALUES;
          (LADATA[1],LADATA[4],PACK_LIN.NO_CART,LCTEMP,GCUSER_ID,GDSYSDATE,GFGETTIME())
      ENDIF
    ENDSCAN

    SELECT CRTINFO

    IF SEEK(LADATA[1]+LADATA[4],'CRTINFO','CRTINFOP')
      SCAN REST WHILE PACK_NO = LADATA[1]
        LCCURINDEX = LADATA[1]+PADL(ALLTRIM(STR(NCARTNO)),4,' ')
        =SEEK(LCCURINDEX,'PACK_LIN','packstyle')
        IF !SEEK(LCCURINDEX,'PACK_LIN','packstyle')
          DELETE
        ENDIF
      ENDSCAN
    ENDIF
  ENDIF
  IF LASCRMODE[4]
    SELECT CRTINFO
    FOR I = 1 TO PACK_HDR.TOT_CART
      LCTEMP = GFSEQUENCE('CcartSeq')
      INSERT INTO CRTINFO (PACK_NO,ACCOUNT,NCARTNO,CCARTSEQ,CADD_USER,DADD_DATE,CADD_TIME) VALUES;
        (LADATA[1],LADATA[4],I,LCTEMP,GCUSER_ID,GDSYSDATE,GFGETTIME())
    ENDFOR
  ENDIF
ENDIF

IF LCPROGNAME = 'ALAUTP'
  IF LASCRMODE[3]
    SELECT PACK_LIN
    =SEEK(M.PACK_NO,'PACK_LIN')
    SCAN REST WHILE PACK_NO = M.PACK_NO
      LCCURINDEX = M.PACK_NO+M.ACCOUNT+ALLTRIM(STR(PACK_LIN.NO_CART,1,4))
      IF !SEEK(LCCURINDEX,'CRTINFO','CRTINFOP')
        LCTEMP = GFSEQUENCE('CcartSeq')
        INSERT INTO CRTINFO (PACK_NO,ACCOUNT,NCARTNO,CCARTSEQ,CADD_USER,DADD_DATE,CADD_TIME) VALUES;
          (M.PACK_NO,M.ACCOUNT,PACK_LIN.NO_CART,LCTEMP,GCUSER_ID,GDSYSDATE,GFGETTIME())
      ENDIF
    ENDSCAN

    SELECT CRTINFO
    IF SEEK(M.PACK_NO+M.ACCOUNT,'CRTINFO','CRTINFOP')
      SCAN REST WHILE PACK_NO = M.PACK_NO
        LCCURINDEX = M.PACK_NO+PADL(ALLTRIM(STR(NCARTNO)),4,' ')
        IF !SEEK(LCCURINDEX,'PACK_LIN','packstyle')
          DELETE
        ENDIF
      ENDSCAN
    ENDIF
  ENDIF
  IF LASCRMODE[4]
    SELECT CRTINFO
    FOR I = 1 TO M.NO_CART
      LCTEMP = GFSEQUENCE('CcartSeq')
      INSERT INTO CRTINFO (PACK_NO,ACCOUNT,NCARTNO,CCARTSEQ,CADD_USER,DADD_DATE,CADD_TIME) VALUES;
        (M.PACK_NO,M.ACCOUNT,I,LCTEMP,GCUSER_ID,GDSYSDATE,GFGETTIME())
    ENDFOR
  ENDIF
ENDIF
IF LCPROGNAME = 'ALPKSCN'
  IF LASCRMODE[4]
    SELECT CRTINFO
    FOR I = 1 TO PACK_HDR.TOT_CART
      LCTEMP = GFSEQUENCE('CcartSeq')
      INSERT INTO CRTINFO (PACK_NO,ACCOUNT,NCARTNO,CCARTSEQ,CADD_USER,DADD_DATE,CADD_TIME) VALUES;
        (LCPACKNO,LCACCOUNT,I,LCTEMP,GCUSER_ID,GDSYSDATE,GFGETTIME())
    ENDFOR
  ENDIF
ENDIF

*-- end of lfAddCrtNo.
