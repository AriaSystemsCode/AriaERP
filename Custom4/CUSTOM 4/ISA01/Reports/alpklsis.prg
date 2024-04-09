****************************************************************************
*: Program file  : ALPKLSIS.PRG --- ALPKLSIS.FRX
*: Program desc. : Custom Print Packing List for Isaac Morise
*: System        : Aria Apparel System (Aria4XP).
*: Module        : Sales Order Allocation  (AL)
*: Developer     : Hassan Ibrahim - (HIA) Due to issue #200751,200752 [T20061228.0005 -Custom Packing List.doc]
*: Date          : 02/25/2007
*:**************************************************************************
*: Calls : FUNCTIONS  :  lfModDbf
*:**************************************************************************
*: Passed Parameters  : None
*:**************************************************************************

=lfModDbf()

=lfModPDt()
=lfStyMasks()

=lfModWar()

SELECT(lcPacktmp)
SET RELATION TO

SET RELATION TO IIF(EMPTY(STORE),'M','S') + Account + STORE INTO &lcCustomer ,;
  "O" + ORDER + STORE     INTO &lcOrdLnTmp,;
  "B" + ORDER             INTO &lcNotePad  ,;
  PACK_NO                 INTO (lcPakLnTmp) ,;
  "O" + ORDER             INTO &lcOrdHdr  ,;
  invoice                 INTO &lcInvLnTmp ADDITIVE

*!**************************************************************************
*-- Functions and Procedures :
*!**************************************************************************
*! Name      : lfModDbf()
*: Developer : Hassan Ibrahim - (HIA) Due to issue #200751,200752
*! Date      : 02/25/2007
*! Purpose   : To update the main temp file with the needed fields in;
*!           : The custom report
*!**************************************************************************
*! Called from : ALPKLSIS.PRG
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfModDbf()
*!**************************************************************************
FUNCTION lfModDbf

SELECT(lcPacktmp)
SET RELATION TO

SELECT(lcPacktmp)


DELETE TAG ALL
IF TYPE('DPRNDT')<> 'D'
  ALTER TABLE (lcPacktmp) ADD COLUMN "dPrnDt" D(8)
ENDIF
IF TYPE('NVOLUME')<> 'N'
  ALTER TABLE (lcPacktmp) ADD COLUMN "NVOLUME" N(8,2)
ENDIF
IF TYPE('DIST_CTR')<> 'C'
  ALTER TABLE (lcPacktmp) ADD COLUMN "DIST_CTR" C(15)
ENDIF
IF TYPE('SPCINST')<> 'C'
  ALTER TABLE (lcPacktmp) ADD COLUMN "SPCINST" C(250)
ENDIF

SELECT(lcPakLnTmp)
DELETE TAG ALL
*
*!*	  IF TYPE('CIQANO')<> 'C'
*!*	    ALTER TABLE (lcPakLnTmp) ADD COLUMN "CIQANO" C(15)
*!*	  ENDIF
IF TYPE('CSKU')<> 'C'
  ALTER TABLE (lcPakLnTmp) ADD COLUMN "CSKU" C(16)
ENDIF
IF TYPE('DESC1')<> 'C'
  ALTER TABLE (lcPakLnTmp) ADD COLUMN "DESC1" C(16)
ENDIF


INDEX ON PACK_NO+STR(no_cart,4)+STYLE TAG 'PACKCRTN'
INDEX ON PACK_NO+STYLE+STR(nOrdLineNO,6) TAG (lcPakLnTmp) ADDITIVE


IF lcInvLnTmp=LCTEMPINVLINE
  lcInvLnTmp = LOOGSCROLL.GFTEMPNAME()
  SELECT * FROM &LCTEMPINVLINE WHERE .T. INTO CURSOR &lcInvLnTmp READWRITE
  SELECT(lcInvLnTmp)
  = CURSORSETPROP("Buffering", 3, lcInvLnTmp)
ENDIF
SELECT(lcInvLnTmp)
DELETE TAG ALL
*!*	  IF TYPE('CIQANO')<> 'C'
*!*	    ALTER TABLE (lcInvLnTmp) ADD COLUMN "CIQANO" C(15)
*!*	  ENDIF
IF TYPE('CSKU')<> 'C'
  ALTER TABLE (lcInvLnTmp) ADD COLUMN "CSKU" C(16)
ENDIF
IF TYPE('DESC1')<> 'C'
  ALTER TABLE (lcInvLnTmp) ADD COLUMN "DESC1" C(16)
ENDIF
IF TYPE('NO_CART')<> 'N'
  ALTER TABLE (lcInvLnTmp) ADD COLUMN "NO_CART" N(4)
ENDIF
INDEX ON invoice+STR(LINENO,6) TAG (lcInvLnTmp)

IF lcOrdLnTmp=LCTEMPORDLINE
  lcOrdLnTmp = LOOGSCROLL.GFTEMPNAME()
  SELECT * FROM &LCTEMPORDLINE WHERE .T. INTO CURSOR &lcOrdLnTmp READWRITE
  SELECT(lcOrdLnTmp)
  = CURSORSETPROP("Buffering", 3, lcInvLnTmp)
ENDIF
SELECT(lcOrdLnTmp)
DELETE TAG ALL
IF TYPE('CSKU')<> 'C'
  ALTER TABLE (lcOrdLnTmp) ADD COLUMN "CSKU" C(16)
ENDIF
IF TYPE('NO_CART')<> 'N'
  ALTER TABLE (lcOrdLnTmp) ADD COLUMN "NO_CART" N(4)
ENDIF
INDEX ON  CORDTYPE+ORDER+STR(LINENO,6) TAG (lcOrdLnTmp)

*MMT
INDEX ON  CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6) TAG (lcOrdLnIndTmp)
SET ORDER TO (lcOrdLnIndTmp)
*MMT

SELECT(lcPacktmp)


ENDFUNC
*!**************************************************************************
*! Name      : lfModPDt()
*: Developer : Hassan Ibrahim - (HIA) Due to issue #200751,200752
*! Date      : 02/25/2007
*! Purpose   : To update the header temp file with the distrb_no values from ordhdr cursor.
*!**************************************************************************
*! Called from : ALPKLSIS.PRG
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfModPDt()
*!**************************************************************************
FUNCTION lfModPDt

SELECT(lcPacktmp)

SET RELATION OFF INTO (lcPakLnTmp)
SET RELATION OFF INTO (lcOrdLnTmp)

SCAN

  STORE 0 TO LNVOLUME
  =lfModIQA()
  =lfModVOL()
  
  SELECT(lcPacktmp)
  REPLACE NVOLUME WITH LNVOLUME

  IF SEEK(&lcPacktmp..PACK_NO,lcTempPikTkt,'PikTkt')
    REPLACE dPrnDt WITH &lcTempPikTkt..DATE
  ENDIF


  *MMT
  *SELECT (lcOrdHdr)
  =SEEK("O" + ORDER,lcOrdHdr)
  *MMT
  REPLACE spcinst WITH ALLTRIM(gfCodDes(EVAL(lcOrdHdr+'.spcinst'),'SPCINST   ')) IN (lcPacktmp)
  SELECT (lcCustomer)
  REPLACE DIST_CTR WITH ALLTRIM(&lcCustomer..DIST_CTR) IN (lcPacktmp)
  SELECT(lcPacktmp)

ENDSCAN

SELECT(lcPacktmp)
SET RELATION TO PACK_NO             INTO (lcPakLnTmp)  ADDITIVE
SET RELATION TO "O" + ORDER + STORE INTO &lcOrdLnTmp   ADDITIVE

ENDFUNC
*!**************************************************************************
*! Name      : lfModIQA()
*: Developer : Hassan Ibrahim - (HIA) Due to issue #200751,200752
*! Date      : 02/25/2007
*! Purpose   : To update the detail temp file with the IQA values from style cursor.
*!**************************************************************************
*! Called from : ALPKLSIS.PRG
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfModIQA()
*!**************************************************************************
FUNCTION lfModIQA

lcHdrFile = lcPacktmp
lcDetailFile = lcPakLnTmp

IF &lcPacktmp..nRprtTyp=1

  lcHdrFile = lcInvLnTmp
  lcDetailFile = lcInvLnTmp

  SELECT(lcDetailFile)
  SCAN FOR invoice+STR(LINENO,6) = &lcPacktmp..invoice

    lcStyle = &lcDetailFile..STYLE
    =SEEK(lcStyle,lcSTYLEFile)
    =lfModCrt()
    =SEEK('O'+&lcHdrFile..ORDER+&lcHdrFile..STORE+&lcSTYLEFile..STYLE,'ORDLINE','ORDLINST')
    REPLACE Desc1  WITH ALLTRIM(ORDLINE.Desc1) IN (lcDetailFile)
    =lfModSKU()

  ENDSCAN
ELSE
  IF &lcPacktmp..nRprtTyp=2

    SELECT(lcPakLnTmp)
    SCAN FOR PACK_NO = &lcPacktmp..PACK_NO

      lcStyle = IIF(&lcPacktmp..nRprtTyp=1,&lcInvLnTmp..STYLE,IIF(&lcPacktmp..nRprtTyp=2,&lcPakLnTmp..STYLE,&lcOrdLnTmp..STYLE))
      =SEEK(lcStyle,lcSTYLEFile)
      =lfModCrt()
      =SEEK('O'+&lcPacktmp..ORDER+&lcPacktmp..STORE+&lcSTYLEFile..STYLE,'ORDLINE','ORDLINST')
      REPLACE Desc1  WITH ALLTRIM(ORDLINE.Desc1)
      =lfModSKU()

    ENDSCAN
  ELSE

    lcHdrFile = lcOrdLnTmp
    lcDetailFile = lcOrdLnTmp

    SELECT(lcDetailFile)
    SCAN FOR "O" + ORDER + STORE = "O"+&lcPacktmp..ORDER+&lcPacktmp..STORE

      lcStyle = &lcDetailFile..STYLE
      =SEEK(lcStyle,lcSTYLEFile)
      =lfModCrt()
      =lfModSKU()

    ENDSCAN
  ENDIF
ENDIF
ENDFUNC
*!**************************************************************************
*! Name      : lfModSKU()
*: Developer : Hassan Ibrahim - (HIA) Due to issue #200751,200752
*! Date      : 02/25/2007
*! Purpose   : To update the detail temp file with the csku values from spck_lin cursor.
*!**************************************************************************
*! Called from : ALPKLSIS.PRG
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfModSKU()
*!**************************************************************************
FUNCTION lfModSKU

IF SEEK( 'S' + &lcHdrFile..Account + &lcSTYLEFile..STYLE ,lcTempSpck_Lin)

  SELECT(lcTempSpck_Lin)
  SCAN REST WHILE TYPE+Account+STYLE+DYELOT+PACK_ID  = 'S' + &lcHdrFile..Account + &lcSTYLEFile..STYLE  FOR &lcTempSpck_Lin..QTY1 => 0
    REPLACE csku WITH &lcTempSpck_Lin..PACK_ID IN (lcDetailFile)
  ENDSCAN
ELSE
  REPLACE csku WITH  "" IN (lcDetailFile)
ENDIF

SELECT(lcDetailFile)
ENDFUNC
*!**************************************************************************
*! Name      : lfModCrt()
*: Developer : Hassan Ibrahim - (HIA) Due to issue #200751,200752
*! Date      : 02/25/2007
*! Purpose   : To update the header temp file with the nvolume values from ordhdr cursor.
*!**************************************************************************
*! Called from : ALPKLSIS.PRG
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfModCrt()
*!**************************************************************************
FUNCTION lfModCrt

*!*	  MVOLUME = 0
*!*	  Store 0 To lnLength,lnWidth,lnHeight
*!*	If !Empty(&lcSTYLEFile..cCrtnVlTE)
*!*	  Declare laVRltFld[3,2]
*!*	  laVRltFld[1,1] = 'NCRTLENGTH'
*!*	  laVRltFld[1,2] = 'lnLength'
*!*	  laVRltFld[2,1] = 'NCRTWIDTH'
*!*	  laVRltFld[2,2] = 'lnWidth'
*!*	  laVRltFld[3,1] = 'NCRTHEIGHT'
*!*	  laVRltFld[3,2] = 'lnHeight'

*!*	  oRelatedFlds = Createobject('GetRelatedFields')
*!*	  =oRelatedFlds.Do(Alltrim(&lcSTYLEFile..cCrtnVlTE),@laVRltFld,'CCRTNVLTE')
*!*	  MVOLUME  = (lnLength * lnWidth * lnHeight)
*!*	Endif

*LNVOLUME = LNVOLUME + (MVOLUME*&lcPakLnTmp..no_cart)
SELECT PACK_LIN
cSave_PACK_LIN_Order = ''
cSave_PACK_LIN_Order = ORDER()
SET ORDER TO PACKSTYLE   && PACK_NO+STR(NO_CART,4)+STYLE+DYELOT
=SEEK(&lcPacktmp..PACK_NO)
lno_cart = 0
SCAN REST WHILE PACK_NO+STR(no_cart,4)+STYLE+DYELOT = &lcPacktmp..PACK_NO FOR PACK_LIN.STYLE = &lcSTYLEFile..STYLE
  *!*	    If  MVOLUME > 0
  *!*	      LNVOLUME = LNVOLUME + (MVOLUME)
  *!*	    Endif
  *lno_cart = no_cart &&lno_cart + 1
  lno_cart = lno_cart + 1
ENDSCAN
REPLACE no_cart WITH  lno_cart IN (lcDetailFile)
IF !EMPTY(cSave_PACK_LIN_Order)
  SET ORDER TO (cSave_PACK_LIN_Order)
ENDIF



SELECT (lcDetailFile)
ENDFUNC
*!**************************************************************************
*! Name      : lfModWar()
*: Developer : Hassan Ibrahim - (HIA) Due to issue #200751,200752
*! Date      : 02/25/2007
*! Purpose   : Get the WareHouse information.
*!**************************************************************************
*! Called from : ALPKLSIS.PRG
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfModWar()
*!**************************************************************************
FUNCTION lfModWar

IF !llPrntComp && if warehouse information not picked from the ALPKLS program
  llPrntComp = .T.
ENDIF

ENDFUNC
*!**************************************************************************
*! Name      : lfHeadVarFn
*! Developer : Mariam Mazhar (MMT)
*! Date      : 03/13/2007
*! Purpose   : Get address data
*!*************************************************************
FUNCTION lfHeadVarFn
DIMENSION LASHIPTO[5, 1]
LASHIPTO = ''
DIMENSION LASOLDTO[6, 1]
LASOLDTO = ''
LCSOLTNAME = &lcCustomer..BTNAME
LASOLDTO[1] = GFGETADR(lcCustomer, '', '', '', 1, '2')
LASOLDTO[2] = GFGETADR(lcCustomer, '', '', '', 2, '2')
LASOLDTO[3] = GFGETADR(lcCustomer, '', '', '', 3, '2')
LASOLDTO[4] = GFGETADR(lcCustomer, '', '', '', 4, '2')
LASOLDTO[5] = GFGETADR(lcCustomer, '', '', '', 5, '2')
LASOLDTO[6] = GFGETADR(lcCustomer, '', '', '', 6, '2')
= LFADRSHIFT('laSoldTo')
PRIVATE LCDISTCNTR, LCALASCUST
LCALASCUST = SELECT(0)
SELECT (lcCustomer)
IF &lcOrdHdr..ALT_SHPTO
  DIMENSION LASHIPTO[6, 1]
  LASHIPTO = ''
  LASHIPTO[1] = &lcOrdHdr..STNAME
  LASHIPTO[2] = &lcOrdHdr..CADDRESS1
  LASHIPTO[3] = &lcOrdHdr..CADDRESS2
  LASHIPTO[4] = &lcOrdHdr..CADDRESS3
  LASHIPTO[5] = &lcOrdHdr..CADDRESS4
  LASHIPTO[6] = &lcOrdHdr..CADDRESS5
  = LFADRSHIFT('laShipTo')
ELSE
  LNCUSREC = 0
  IF !EMPTY(&lcCustomer..STORE) AND !EMPTY(&lcCustomer..DIST_CTR)
    LNCUSREC = IIF( .NOT. EOF(lcCustomer), RECNO(lcCustomer), 0)
    =SEEK('S'+&lcCustomer..Account+&lcCustomer..DIST_CTR)
  ENDIF
  = GFGETADR(lcCustomer, '', '', '', @LASHIPTO)
  = LFADRSHIFT('laShipTo')
  DIMENSION LASHIPTO[6, 1]
  = AINS(LASHIPTO, 1)
  LASHIPTO[1,1] = &lcCustomer..STNAME
  = LFADRSHIFT('laShipTo')
  IF BETWEEN(LNCUSREC, 1, RECCOUNT(lcCustomer))
    GOTO LNCUSREC IN &lcCustomer
  ENDIF
ENDIF
SELECT (LCALASCUST)
DIMENSION LACOMPADD[6, 1]
LACOMPADD = ''
STORE '' TO LCCOMPNAME, LCCOMPPHON
PRIVATE LNSLCT
LNSLCT = SELECT(0)
SELECT (LCWAREHOUS)
SEEK &lcPacktmp..CWARECODE
IF llPrntComp
  DIMENSION LACOMPADD[5, 1]
  = GFGETADR(LCWAREHOUS, '', '', '', @LACOMPADD)
  LCCOMPNAME = &LCWAREHOUS..CDESC
  LCCOMPPHON = &LCWAREHOUS..CPHONE
  DIMENSION LACOMPADD[6, 1]
  LACOMPADD[6, 1] = 'Phone# : '+TRANSFORM(LCCOMPPHON, '@R '+LCPHONPICT)
  = LFADRSHIFT('laCompAdd')
ENDIF
SELECT (LNSLCT)
STORE "" TO LCSCALE, LCPACKNO
RETURN ""
ENDFUNC
*!**************************************************************************
*! Name      : lfModVol()
*: Developer : Hassan Ibrahim - (HIA) Due to issue #200751,200752
*! Date      : 02/25/2007
*! Purpose   : To update the header temp file with the nvolume values from Style and Pack_LIN cursor.
*!**************************************************************************
*! Called from : ALPKLSIS.PRG
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfModVol()
*!**************************************************************************
FUNCTION lfModVOL
SELECT PACK_LIN
cSave_PACK_LIN_Order = ''
cSave_PACK_LIN_Order = ORDER()
SET ORDER TO PACKSTYLE   && PACK_NO+STR(NO_CART,4)+STYLE+DYELOT
=SEEK(&lcPacktmp..PACK_NO)

SCAN REST WHILE PACK_NO+STR(no_cart,4)+STYLE+DYELOT = &lcPacktmp..PACK_NO 

  =SEEK(PACK_LIN.style,lcSTYLEFile)
  MVOLUME = 0
  STORE 0 TO lnLength,lnWidth,lnHeight
  IF !EMPTY(&lcSTYLEFile..cCrtnVlTE)
    DECLARE laVRltFld[3,2]
    laVRltFld[1,1] = 'NCRTLENGTH'
    laVRltFld[1,2] = 'lnLength'
    laVRltFld[2,1] = 'NCRTWIDTH'
    laVRltFld[2,2] = 'lnWidth'
    laVRltFld[3,1] = 'NCRTHEIGHT'
    laVRltFld[3,2] = 'lnHeight'

    oRelatedFlds = CREATEOBJECT('GetRelatedFields')
    =oRelatedFlds.DO(ALLTRIM(&lcSTYLEFile..cCrtnVlTE),@laVRltFld,'CCRTNVLTE')
    MVOLUME  = (lnLength * lnWidth * lnHeight)
  ENDIF


  IF  MVOLUME > 0
    LNVOLUME = LNVOLUME + (MVOLUME)
  ENDIF

ENDSCAN

IF !EMPTY(cSave_PACK_LIN_Order)
  SET ORDER TO (cSave_PACK_LIN_Order)
ENDIF


ENDFUNC
*!**************************************************************************
