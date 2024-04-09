*:***********************************************************************************************************
*: Program file  : FixCrtSeq
*: Program desc. : Fix program to Update EDICRTSQ File For (B039978)
*:        System : Aria Advantage Series.
*:        Module : (EDI).
*:     Developer : Waleed Hamed (WLD)
*:          Date : 11/24/2005
*:***********************************************************************************************************

_SCREEN.CAPTION ='Carton Sequence Fix '
_SCREEN.WINDOWSTATE = 2
SET DELETE ON
CLOSE DATA


= MESSAGEBOX("Please select the System Files Directory.",64,'Carton Sequence Fix ')
lcSysDir = GETDIR()
IF !FILE(lcSysDir+'SYDFILES.DBF')
  =MESSAGEBOX("This is not the System Files Directory. Cannot proceed.",16,'Carton Sequence Fix ')
  RETURN
ENDIF

USE lcSysDir+"SYCCOMP" SHARE IN 0
SELECT  SYCCOMP
SCAN
  lcDataDir = ALLT(ccom_Ddir)

  USE lcDataDir+"ASN_SHIP" SHARE  IN  0  ORDER ASN_SHIP
  USE lcDataDir+"PACK_HDR" SHARE IN 0 ORDER PACK_HDR
  USE lcDataDir+"EDICRTSQ" SHARE IN 0  ORDER PCKCRTSQ
  USE lcDataDir+"EDIACPRT" SHARE IN 0  ORDER  ACCfACT

  SELECT ASN_SHIP
  SET RELATION TO pack_no INTO PACK_HDR ADDITIVE
  lcTemp       = SYS(2015)
  lcTempQry    = SYS(2015)
  lcMaxTemp    = SYS(2015)
  lcMaxUCC9ASN = SYS(2015)

  SELECT ASN_SHIP.Cart_NO , ASN_SHIP.pack_no , ASN_SHIP.ucc9 , PACK_HDR.account ,PACK_HDR.ship_date;
    FROM ASN_SHIP, PACK_HDR;
    WHERE PACK_HDR.pack_no = ASN_SHIP.pack_no;
    INTO CURSOR (lcTemp)
  *--
  *- Get max ucc9 from ASN_Ship
  SELECT DISTINCT PACK_HDR.account, MAX(ASN_SHIP.ucc9);
    FROM ASN_SHIP,PACK_HDR;
    Where ASN_SHIP.pack_no = PACK_HDR.pack_no;
    GROUP BY PACK_HDR.account INTO CURSOR (lcMaxUCC9ASN)
  *-- Put the Max UCC9 in EDIACPRT
  SELECT (lcMaxUCC9ASN)
  SCAN
    IF SEEK('A'+account,'EDIACPRT')
      REPLACE EDIACPRT.ucc9 WITH &lcMaxUCC9ASN..MAX_UCC9
    ENDIF
  ENDSCAN
  *******
  *  SELECT MAX(ASN_SHIP.ucc9),PACK_HDR.ACCOUNT
  *    FROM ASN_SHIP,PACK_HDR
  *    WHERE PACK_HDR.pack_no = ASN_SHIP.pack_no
  *    GROUP BY PACK_HDR.ACCOUNT
  *    INTO CURSOR(lcMaxTemp)

  *  SELECT (lcMaxTemp)
  *  SCAN
  *  SELECT MAX(ASN_SHIP.ucc9) FROM ASN_SHIP,PACK_HDR WHERE PACK_HDR.pack_no = ASN_SHIP.pack_no AND PACK_HDR.ACCOUNT = EDIACPRT.CPARTNER GROUP BY PACK_HDR.ACCOUNT  INTO ARRAY LAMAX
  *  REPLACE EDIACPRT.ucc9 WITH MAX(LAMAX,EDIACPRT.ucc9)
  * ENDSCAN
  *--
  SELECT (lcTemp)
  SCAN
    SCATTER MEMVAR MEMO
    IF EMPTY(m.ucc9)
      IF SEEK('A'+account,'EDIACPRT')
        m.ucc9= PADL(ALLTRIM(STR(VAL(EDIACPRT.ucc9)+1)),9,'0')
        REPLACE EDIACPRT.ucc9 WITH ALLTRIM(STR(VAL(EDIACPRT.ucc9)+1)) IN EDIACPRT
      ENDIF
    ENDIF
    IF !SEEK(m.pack_no+STR(m.Cart_NO,6),'EDICRTSQ')
      WAIT WINDOW 'Company Path : ' +lcDataDir + '. Update Cartons information for Packing slip # ' + m.pack_no NOWAIT
      INSERT INTO EDICRTSQ FROM MEMVAR

    ENDIF
  ENDSCAN

  SELECT DISTINCT EDICRTSQ.account, MAX(EDICRTSQ.ucc9);
    FROM EDICRTSQ;
    GROUP BY EDICRTSQ.account INTO CURSOR (lcTempQry)

  SELECT (lcTempQry)
  SCAN
    IF SEEK('A'+account,'EDIACPRT')
      REPLACE EDIACPRT.ucc9 WITH &lcTempQry..MAX_UCC9
    ENDIF
  ENDSCAN

  IF USED('ASN_SHIP')
    USE IN  ASN_SHIP
  ENDIF
  IF USED('PACK_HDR')
    USE IN  PACK_HDR
  ENDIF
  IF USED('EDICRTSQ')
    USE IN  EDICRTSQ
  ENDIF
  IF USED('EDIACPRT')
    USE IN EDIACPRT
  ENDIF

  IF USED(lcTemp)
    USE IN  (lcTemp)
  ENDIF
  IF USED(lcTempQry)
    USE IN  (lcTempQry)
  ENDIF

ENDSCAN
=MESSAGEBOX('Fix Done',64,'Carton Sequence Fix ')

CLOSE DATA
