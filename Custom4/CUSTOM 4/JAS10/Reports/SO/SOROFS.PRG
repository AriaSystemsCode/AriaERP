*:***************************************************************************
*: Program file  : SOROFS
*: Program desc. : Receive Orders From Seta
*: Module        : Sales Order (SO)
*: Developer     : HIA(HMA)
*: Tracking Job Number: C201469
*: Date : 04/02/2012
*:***************************************************************************
*: Calls :
*:    Programs   : ....
*:    Screens    : ....
*:    Global Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SOROFS
*:***************************************************************************
*: Modification:
*! E303163,1 HIA Validate Custpo, address format [T20120228.0054]
*! C201516,1 MMT 09/10/2012 Add location option to custom receiving program from SETA[T20120724.0038]
*:**************************************************************************
*----------------------- Report Code Begin -----------------------------
*SET STEP ON

*lcRpOutNam = ""
*lcRpAcct   = ""
*- Previewing The OG
*lcExpr = gfOpGrid('SOROFS',.T.)

*- Just closing the OG
*IF EMPTY(&lcExpr)
*  RETURN
*ENDIF

*IF EMPTY(lcRpOutNam) OR !FILE(lcRpOutNam)
*  gfModalGen('INM00000B00000',.F.,.F.,.F.,'No file was selected.')
*  RETURN
*ENDIF
LCRPFLPATH = LCRPOUTNAM
*--Open Needed files
=LFOPENFLS()

*- Create the needed temp files for the current session
*lcTmImprt  = gfTempName()
LCTMPIMPRT = GFTEMPNAME()
LCLOGFILE  = GFTEMPNAME()
LCORDHDR   = GFTEMPNAME()
LCORDLINE  = GFTEMPNAME()
LCT_BOMVAR = GFTEMPNAME()
=LFCRATTEMP()

*- Get the data from the temp file
SELECT &LCTMPIMPRT
ZAP
APPEND FROM (LCRPFLPATH) CSV
***
*RENAME (lcRpFlPath)  TO   ADDBS(JUSTPATH(lcRpFlPath))+"old_"+JUSTSTEM(lcRpFlPath)+"."+JUSTEXT(lcRpFlPath)

***
LOCATE
IF ALLTRIM(UPPER(&LCTMPIMPRT..BATCHNO)) = "BATCHNO"
  DELETE
ENDIF

LCREJORDERS = ""
OPROSS = CREATEOBJECT('ariaprogressbar')
OPROSS.LBLFIRSTLABEL.CAPTION = "Receive Seta Orders ... "
OPROSS.TOTALPROGRESS         = RECCOUNT()
OPROSS.AUTOCENTER            = .T.


LCERRSTR = ''
LCCR = CHR(13)
=LFVERIFY()

SELECT &LCTMPIMPRT
*!* ZAP
*!*	SELECT (lcTmImprt)
*!*	SCAN
*!*	  SCATTER MEMVAR MEMO

*!*	  SELECT &lcTmpImprt
*!*	  APPEND BLANK
*!*	  GATHER MEMVAR MEMO
*!*	ENDSCAN


*--lnOrdCount : to hold the numbers of orders that have been generated
*--laOrders   : array to hold the orders numbers.
DIMENSION LAORDERS[1]
STORE '' TO LAORDERS
LNORDCOUNT = 0

DIMENSION LACANCELORDERS[1]
STORE '' TO LACANCELORDERS
LNCANCELORDERSCOUNT = 0


LLTRDDISCL = GFGETMEMVAR('M_TRDDISCL')
LLTRDDISCL = IIF(EMPTY(LLTRDDISCL),.F.,LLTRDDISCL)

SELECT WAREHOUS
LOCATE FOR LDEFWARE
IF !FOUND()
  LOCATE
ENDIF

SELECT &LCTMPIMPRT
LOCATE
NEWCUSTPO    = .T.
*lcOldCustPo = ""
LNSHIPHANDL  = 0
LNTAXAMT     = 0
OPROSS.SHOW()

SCAN
  IF &LCTMPIMPRT..REJ = .F. AND &LCTMPIMPRT..RECORDTYPE <> "C"

    IF NEWCUSTPO
      NEWCUSTPO = .F.

      SELECT (LCORDHDR)
      SCATTER MEMVAR MEMO BLANK

      m.LINENO = 0

      SELECT &LCTMPIMPRT
      m.CUSTPO    = &LCTMPIMPRT..ORDERNO
      m.START     = LFGETDT(&LCTMPIMPRT..ORDERDATE)
      m.COMPLETE  = LFGETDT(&LCTMPIMPRT..ORDERDATE)
      m.ENTERED   = LFGETDT(&LCTMPIMPRT..ORDERDATE)
      *! C201516,1 MMT 09/10/2012 Add location option to custom receiving program from SETA[Start]
      *m.CWARECODE = WAREHOUS.CWARECODE
      m.CWARECODE =  lcRpOrdWrh
      *! C201516,1 MMT 09/10/2012 Add location option to custom receiving program from SETA[End]
      m.ACCOUNT   = LCRPACCT
      m.INT_VEND  = &LCTMPIMPRT..VENDORID

      =SEEK('M'+LCRPACCT,'CUSTOMER')
      m.SHIPVIA   = CUSTOMER.SHIPVIA

      m.NCURRUNIT = 1
      m.NEXRATE   = 1
      m.CCURRCODE = CUSTOMER.CCURRCODE
      LCCURRCODE = IIF(EMPTY(m.CCURRCODE),OARIAAPPLICATION.BASECURRENCY,m.CCURRCODE)
      STORE 1 TO LNEXRATE, LNCURRUNIT
      IF LCCURRCODE <> OARIAAPPLICATION.BASECURRENCY
        DECLARE LASETUPS[1,2]
        LASETUPS[1,1] = 'LLEDITEXRA'    && Edit Exchange Rate
        =GFGETMEMVAR(@LASETUPS,OARIAAPPLICATION.ACTIVECOMPANYID)
        LLEDITEXRT   = LASETUPS[1,2]
        LNEXRATE = GFCHKRATE('lnCurrUnit',LCCURRCODE,m.ENTERED,.T.,.F.,.F.,LLEDITEXRT)
        IF LNEXRATE = 0
          IF LLEDITEXRT

            =GFMODALGEN('INM04156B00000','ALERT',ALLTRIM(LCCURRCODE)+'|'+ALLTRIM(OARIAAPPLICATION.BASECURRENCY)+'|'+DTOC(OARIAAPPLICATION.SYSTEMDATE))
          ELSE
            LCCURRCODE = OARIAAPPLICATION.BASECURRENCY
            STORE 1 TO LNEXRATE, LNCURRUNIT
          ENDIF
        ENDIF
      ENDIF
      m.CCURRCODE = LCCURRCODE
      m.NEXRATE   = LNEXRATE
      m.NCURRUNIT = LNCURRUNIT

      m.CORDTYPE  = 'O'
      m.STATUS    = 'O'
      m.FLAG      = 'N'
      m.BULK      = 'N'
      m.DIRECTINV = .F.
      m.LCONTRACT = .F.
      m.CREORDER  = 'N'
      m.MULTI     = 'N'
      m.CINSUR    = 'Y'
      m.BUYER     = CUSTOMER.BUYER
      m.PHONE     = CUSTOMER.PHONE1
      m.DISC      = CUSTOMER.DISC
      m.REP1      = CUSTOMER.SALESREP
      m.COMM1     = CUSTOMER.COMM
      m.REP2      = CUSTOMER.REP2
      m.COMM2     = CUSTOMER.COMM2
      m.PRIORITY  = CUSTOMER.PRIORITY
      *! E303163,1 HIA Validate Custpo, address format [T20120228.0054][Begin]
      *m.CADDRESS1 = &lcTmpImprt..shiptoname
      *m.CADDRESS2 = &lcTmpImprt..shiptoadd1
      *m.CADDRESS3 = &lcTmpImprt..shiptoadd2
      *m.CADDRESS4 = &lcTmpImprt..shiptocity
      *m.CADDRESS5 = &lcTmpImprt..shiptozip

      m.ALT_SHPTO = .T.
      m.STNAME    = &LCTMPIMPRT..SHIPTONAME
      m.CADDRESS1 = &LCTMPIMPRT..SHIPTOADD1
      m.CADDRESS2 = &LCTMPIMPRT..SHIPTOADD2
      m.CADDRESS3 = SUBSTR(ALLTRIM(&LCTMPIMPRT..SHIPTOCITY)+', '+ALLTRIM(&LCTMPIMPRT..SHIPTOSTAT)+', '+ALLTRIM(&LCTMPIMPRT..SHIPTOZIP),1,30)
      m.CADDRESS4 = ''
      m.CADDRESS5 = ''

      *! E303163,1 HIA Validate Custpo, address format [T20120228.0054][End]
      m.LINK_CODE = IIF(EMPTY(CUSTOMER.LINK_CODE),'DEFDEF',CUSTOMER.LINK_CODE)
      m.GL_SALES  = IIF(EMPTY(CUSTOMER.CSLSGLLINK),'DEF',CUSTOMER.CSLSGLLINK)
      m.CTERMCODE = CUSTOMER.CTERMCODE
      m.SPCINST   = CUSTOMER.SPCINST

    ENDIF

    =SEEK(&LCTMPIMPRT..STYLE,'STYLE')
    LCSZ = ALLTRIM(&LCTMPIMPRT..SIZE)
    && as per Alaa, customer use styles only, no packs.
    *IF !(SEEK(STYLE.STYLE,lcOrdLine) AND &lcOrdLine..Pack_ID = &lcTmpImprt..Pack_ID )
    IF !(SEEK(STYLE.STYLE,LCORDLINE))
      m.LINENO    = m.LINENO + 1
      m.LASTLINE  = m.LINENO
      m.STYLE     = STYLE.STYLE
      m.DESC1     = STYLE.DESC1
      m.SCALE     = STYLE.SCALE
      m.SEASON    = STYLE.SEASON
      m.CDIVISION = STYLE.CDIVISION

      *m.Pack_ID   = &lcTmpImprt..Pack_ID
      *IF !EMPTY(&lcTmpImprt..Pack_ID)
      *  m.note_mem  = "ORDER SEQ.="+&lctmpOrders..orderseq+"|"
      *  m.note_mem  = m.note_mem       + "ITEM SEQ.="+&lctmpOrders..ITEMSEQ+"|"
      *  m.note_mem  = m.note_mem       + "ITEM NUMBER="+&lctmpOrders..ITEMNO+"|"
      *  m.note_mem  = m.note_mem       + "SETA ITEM SUB="+&lctmpOrders..ITEMSub+"|"
      *ENDIF

      *-if style.cdefware differ for cwarecode then add this style to the new warehouse
      IF m.CWARECODE <> STYLE.CDEFWARE  AND !SEEK(STYLE.STYLE+m.CWARECODE,'STYDYE')
        INSERT INTO STYDYE (STYLE,CWARECODE,DESC,GL_LINK,AVE_COST) ;
          VALUES (STYLE.STYLE,m.CWARECODE,STYLE.DESC,STYLE.LINK_CODE,STYLE.AVE_COST)
        =LFADDUSRDT('STYDYE')
      ENDIF

      WAIT WINDOW NOWAIT 'Saving the Seta order # '+ m.CUSTPO + ",Style "+ ALLTRIM(STYLE.STYLE)
      *- updating the lcordline temp file
      SELECT &LCORDLINE
      APPEND BLANK
      GATHER MEMVAR    &&the fields that would be filled using this GATHER command are the following
      && ACCOUNT,COMM1,COMM2,COMPLETE,CORDTYPE,CUSTPO,CWARECODE,FLAG,GL_SALES,ORDER,SEASON,START,STORE

      *- update audit data
      =LFADDUSRDT(LCORDLINE)


    ENDIF

    SELECT &LCORDLINE
    LNNEWQTY = VAL(ALLTRIM(&LCTMPIMPRT..ITEMQTY))
    *IF !EMPTY(ALLTRIM(lcSz))

    REPLACE QTY&LCSZ  WITH QTY&LCSZ + LNNEWQTY ;
      TOTQTY    WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8 ;
      BOOK&LCSZ WITH QTY&LCSZ ;
      TOTBOOK   WITH BOOK1+BOOK2+BOOK3+BOOK4+BOOK5+BOOK6+BOOK7+BOOK8

    m.NOTE_MEM  = ALLTRIM(&LCORDLINE..NOTE_MEM)+IIF(!EMPTY(ALLTRIM(&LCORDLINE..NOTE_MEM)),CHR(13),"") +"ORDER SEQ.,"   +LCSZ+" ="+&LCTMPIMPRT..ORDERSEQ+"|"
    m.NOTE_MEM  = m.NOTE_MEM       + "ITEM SEQ.,"    +LCSZ+" ="+&LCTMPIMPRT..ITEMSEQ+"|"
    m.NOTE_MEM  = m.NOTE_MEM       + "ITEM NUMBER,"  +LCSZ+" ="+&LCTMPIMPRT..ITEMNO+"|"
    m.NOTE_MEM  = m.NOTE_MEM       + "SETA ITEM SUB,"+LCSZ+" ="+&LCTMPIMPRT..ITEMSUB+"|"
    REPLACE NOTE_MEM WITH m.NOTE_MEM

    *ELSE
    *  REPLACE QTY1  WITH QTY1 + (lnNewQty * &lcTmpImprt..QTY1) ,;
    *    QTY2  WITH QTY2 + (lnNewQty * &lcTmpImprt..QTY2) ,;
    *    QTY3  WITH QTY3 + (lnNewQty * &lcTmpImprt..QTY3) ,;
    *    QTY4  WITH QTY4 + (lnNewQty * &lcTmpImprt..QTY4) ,;
    *    QTY5  WITH QTY5 + (lnNewQty * &lcTmpImprt..QTY5) ,;
    *    QTY6  WITH QTY6 + (lnNewQty * &lcTmpImprt..QTY6) ,;
    *    QTY7  WITH QTY7 + (lnNewQty * &lcTmpImprt..QTY7) ,;
    *    QTY8  WITH QTY8 + (lnNewQty * &lcTmpImprt..QTY8) ,;
    *    TOTQTY    WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8 ,;
    *    BOOK&lcSz WITH QTY&lcSz ,;
    *    TOTBOOK   WITH BOOK1+BOOK2+BOOK3+BOOK4+BOOK5+BOOK6+BOOK7+BOOK8
    *ENDIF



    LCLEVEL = LFGETLVL()

    LCGROS_PRICE = GFGETPRICE(&LCORDLINE..STYLE ,LCLEVEL ,&LCORDLINE..TOTQTY,LCCURRCODE)
    REPLACE PRICE  WITH LCGROS_PRICE ;
      GROS_PRICE WITH LCGROS_PRICE ;
      TRDE_DISC  WITH IIF(LLTRDDISCL,STYLE.TRD_DISC&LCLEVEL.,0)

    *SELECT &lcTmpImprt
    IF !EMPTY(&LCTMPIMPRT..SHIPHANDL)
      LNSHIPHANDL = LNSHIPHANDL + VAL(ALLTRIM(&LCTMPIMPRT..SHIPHANDL))
    ENDIF
    IF !EMPTY(&LCTMPIMPRT..TAXAMT)
      LNTAXAMT    = LNTAXAMT    + VAL(ALLTRIM(&LCTMPIMPRT..TAXAMT))
    ENDIF

    SELECT (LCORDLINE)
    IF &LCTMPIMPRT..LASTLINE
      SELECT (LCORDLINE)
      LOCATE
      SCAN
        m.BOOK      = m.BOOK + TOTBOOK
        m.BOOKAMT   = m.BOOKAMT + (PRICE * TOTQTY)
        m.OPENAMT   = m.BOOKAMT
        m.CANCELAMT = 0
        m.OPEN      = m.BOOK
      ENDSCAN

      IF (m.BOOK > 0 OR m.OPEN > 0)

        *- select if styles are related to multiple seasons
        DIMENSION LASEASON[1]
        LASEASON = ' '
        SELECT DISTINCT SEASON FROM (LCORDLINE) INTO ARRAY LASEASON
        m.SEASON = LASEASON[1]
        IF ALEN(LASEASON) > 1
          SELECT (LCORDLINE)
          LOCATE
          REPLACE SEASON WITH '*' ALL
          LOCATE

          m.SEASON = '*'

        ENDIF

        INSERT INTO (LCORDHDR) FROM MEMVAR
        =LFADDUSRDT(LCORDHDR)
        SELECT (LCORDHDR)
        LOCATE

        SELECT (LCORDLINE)
        SET ORDER TO ORDLINE
        LOCATE
        =LFSAVEDATA()
        LNSHIPHANDL = 0
        LNTAXAMT    = 0
      ENDIF
      SET ORDER TO ORDLINES IN (LCORDLINE)
    ENDIF
  ELSE
    *Cancel orders
    IF &LCTMPIMPRT..RECORDTYPE = "C" AND &LCTMPIMPRT..LASTLINE
      *SET ORDER TO ORDCUST   && ACCOUNT+UPPER(CUSTPO)+CORDTYPE+ORDER
      IF SEEK(LCRPACCT+&LCTMPIMPRT..ORDERNO,"ordhdr","ORDCUST") AND ORDHDR.STATUS <> "X"
        IF GFMODALGEN('QRM00274B32005','ALERT',"Order:"+ALLTRIM(ORDHDR.ORDER)+" Customer PO:"+ALLTRIM(ORDHDR.CUSTPO)+", will be canceled") = 1
          *IF gfModalGen('QRM00000B32005','ALERT',"Order:"+ALLTRIM(ordhdr.Order)+" Customer PO:"+ALLTRIM(Ordhdr.CustPO)+", will be canceled") =1
          LCCANCELORDER = ORDHDR.ORDER
          =LFDELORDER()
          IF !EMPTY(LACANCELORDERS(ALEN(LACANCELORDERS)) )
            DIME LACANCELORDERS(ALEN(LACANCELORDERS)+1)
          ENDIF
          LACANCELORDERS(ALEN(LACANCELORDERS)) = LCCANCELORDER
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  OPROSS.CURRENTPROGRESS(RECNO())

  SELECT &LCTMPIMPRT
  IF &LCTMPIMPRT..LASTLINE
    NEWCUSTPO = .T.
  ENDIF
ENDSCAN

OPROSS.HIDE()

LCCANCELORDERSSTR = ""
LNCANCELORDERSCOUNT = ALEN(LACANCELORDERS)
DO CASE
CASE LNCANCELORDERSCOUNT = 1 AND !EMPTY(LACANCELORDERS[1])
  LCCANCELORDERSSTR = 'Order '+LACANCELORDERS[1]+' is Deleted '
  LCCANCELORDERSSTR = CHR(13)+CHR(10)+LCCANCELORDERSSTR

CASE LNCANCELORDERSCOUNT > 1
  LCCANCELORDERSSTR = 'Order '
  FOR I = 1 TO ALEN(LACANCELORDERS,1)
    LCCANCELORDERSSTR = LCCANCELORDERSSTR +LACANCELORDERS[1]+', '
  ENDFOR
  LCCANCELORDERSSTR = SUBSTR(LCCANCELORDERSSTR,1,LEN(LCCANCELORDERSSTR)-2) +' are Deleted '
  LCCANCELORDERSSTR = CHR(13)+CHR(10)+LCCANCELORDERSSTR

OTHERWISE
  LCCANCELORDERSSTR = ""
ENDCASE

*MT
=ASORT(LAORDERS)
*MT
DO CASE
CASE LNORDCOUNT=1
  =GFMODALGEN('INM00000B00000',.F.,.F.,.F.,'Order is saved as ' + LAORDERS[1]+LCCANCELORDERSSTR)
CASE LNORDCOUNT > 1
  =GFMODALGEN('INM00000B00000',.F.,.F.,.F.,'Orders from ' + LAORDERS[1] + ' to ' + LAORDERS[lnOrdCount]+ ' have been generated successfully' +LCCANCELORDERSSTR)
CASE LNORDCOUNT = 0
  =GFMODALGEN('INM00000B00000',.F.,.F.,.F.,'No orders saved.'+LCCANCELORDERSSTR)
ENDCASE

*- Display errors if found.
IF !EMPTY(LCERRSTR)
  =LFDISPLAYERRORS()
ENDIF

*- clear
=LFCLEAR()
RETURN

*!**************************************************************************
*!* Name        : lfOpenFls
*!* Developer   : TMI - TAREK MOHAMMED IBRAHIM
*!* Date        : 04/11/2011
*!* Purpose     : Open needed files
*!***************************************************************************
FUNCTION LFOPENFLS
=GFOPENTABLE(OARIAAPPLICATION.DATADIR+'BomVar',OARIAAPPLICATION.DATADIR+'BomVar','SH')
=GFOPENTABLE(OARIAAPPLICATION.DATADIR+'STYLE','STYLE','SH')
=GFOPENTABLE(OARIAAPPLICATION.DATADIR+'SCALE','SCALE','SH')
=GFOPENTABLE(OARIAAPPLICATION.DATADIR+'WAREHOUS','WAREHOUS','SH')
=GFOPENTABLE(OARIAAPPLICATION.DATADIR+'CUSTOMER','CUSTOMER','SH')
=GFOPENTABLE(OARIAAPPLICATION.DATADIR+'SALESREP','SALESREP','SH')
=GFOPENTABLE(OARIAAPPLICATION.DATADIR+'ORDHDR','ORDHDR','SH')
=GFOPENTABLE(OARIAAPPLICATION.DATADIR+'ORDLINE','ORDLINE','SH')
=GFOPENTABLE(OARIAAPPLICATION.DATADIR+'UnCmSess',OARIAAPPLICATION.DATADIR+'TRANS','SH')
=GFOPENTABLE(OARIAAPPLICATION.DATADIR+'STYDYE',OARIAAPPLICATION.DATADIR+'STYDYE','SH')
=GFOPENTABLE(OARIAAPPLICATION.DATADIR+'NOTEPAD','NOTEPAD','SH')

=GFOPENTABLE(OARIAAPPLICATION.SYSPATH+'SycCurr','cCurrCode','SH')
=GFOPENTABLE(OARIAAPPLICATION.SYSPATH+'SYCEXCH','CURRENCY','SH')
=GFOPENTABLE(OARIAAPPLICATION.SYSPATH+'SYCINT','CCONTCODE','SH')

=GFOPENTABLE(OARIAAPPLICATION.SYSPATH+'SPCK_HDR','SPCK_HDR','SH')
=GFOPENTABLE(OARIAAPPLICATION.SYSPATH+'SPCK_LIN','Spck_Lin','SH')
=GFOPENTABLE(OARIAAPPLICATION.SYSPATH+'ORDERCHG','ORDERCHG','SH')

SET ORDER TO TAG CURRENCY DESC IN 'SYCEXCH'
*- End of FUNCTION lfOpenFls

*!**************************************************************************
*!* Name        : lfCratTemp
*!* Developer   : TMI - TAREK MOHAMMED IBRAHIM
*!* Date        : 04/11/2011
*!* Purpose   : Create temp files
*!***************************************************************************
FUNCTION LFCRATTEMP
PRIVATE LAFILESTRU,LNFILESTRU,LAINDEX,LNALIAS
LNALIAS = SELECT()
SELECT ORDHDR
=AFIELDS(LAFILESTRU)
LNFILESTRU = ALEN(LAFILESTRU,1)
DIMENSION LAFILESTRU[lnFileStru+2,18]
LAFILESTRU[lnFileStru+1,1] = 'nSteps'
LAFILESTRU[lnFileStru+1,2] = 'N'
LAFILESTRU[lnFileStru+1,3] = 2
LAFILESTRU[lnFileStru+1,4] = 0

LAFILESTRU[lnFileStru+2,1] = 'LERRORS'
LAFILESTRU[lnFileStru+2,2] = 'L'
LAFILESTRU[lnFileStru+2,3] = 1
LAFILESTRU[lnFileStru+2,4] = 0

FOR LNLOOP = 1 TO  2
  STORE ' ' TO  LAFILESTRU[lnFileStru +lnLoop,7],LAFILESTRU[lnFileStru +lnLoop,8],;
    LAFILESTRU[lnFileStru +lnLoop,9],LAFILESTRU[lnFileStru +lnLoop,10],;
    LAFILESTRU[lnFileStru +lnLoop,11],LAFILESTRU[lnFileStru +lnLoop,12],;
    LAFILESTRU[lnFileStru +lnLoop,13],LAFILESTRU[lnFileStru +lnLoop,14],;
    LAFILESTRU[lnFileStru +lnLoop,15],LAFILESTRU[lnFileStru +lnLoop,16]
  STORE 0 TO    LAFILESTRU[lnFileStru +lnLoop,17] ,LAFILESTRU[lnFileStru +lnLoop,18]
ENDFOR

DECLARE LAINDEX[2,2]
LAINDEX[1,1] = 'ACCOUNT+CUSTPO'
LAINDEX[1,2] = 'CUSTPO'
LAINDEX[2,1] = 'cOrdType+ORDER'
LAINDEX[2,2] = '&lcOrdHdr'
=GFCRTTMP(LCORDHDR,@LAFILESTRU,@LAINDEX)

SELECT(LCORDHDR)
INDEX ON 'cOrdType+ORDER' TAG (LCORDHDR)
SET ORDER TO TAG 'CUSTPO' IN (LCORDHDR)

*----------------
SELECT ORDLINE
=AFIELDS(LAFILESTRU)
LNFILESTRU = ALEN(LAFILESTRU,1)
DIMENSION LAFILESTRU[lnFileStru+2,18]
LAFILESTRU[lnFileStru+1,1] = 'nSteps'
LAFILESTRU[lnFileStru+1,2] = 'N'
LAFILESTRU[lnFileStru+1,3] = 2
LAFILESTRU[lnFileStru+1,4] = 0
LAFILESTRU[lnFileStru+2,1] = 'lContract'
LAFILESTRU[lnFileStru+2,2] = 'L'
LAFILESTRU[lnFileStru+2,3] = 1
LAFILESTRU[lnFileStru+2,4] = 0

FOR LNLOOP = 1 TO  2
  STORE ' ' TO  LAFILESTRU[lnFileStru +lnLoop,7],LAFILESTRU[lnFileStru +lnLoop,8],;
    LAFILESTRU[lnFileStru +lnLoop,9],LAFILESTRU[lnFileStru +lnLoop,10],;
    LAFILESTRU[lnFileStru +lnLoop,11],LAFILESTRU[lnFileStru +lnLoop,12],;
    LAFILESTRU[lnFileStru +lnLoop,13],LAFILESTRU[lnFileStru +lnLoop,14],;
    LAFILESTRU[lnFileStru +lnLoop,15],LAFILESTRU[lnFileStru +lnLoop,16]
  STORE 0 TO    LAFILESTRU[lnFileStru +lnLoop,17] ,LAFILESTRU[lnFileStru +lnLoop,18]
ENDFOR
=GFCRTTMP(LCORDLINE,@LAFILESTRU,'STYLE+STORE','ORDLINES',.F.)

SELECT(LCORDLINE)
USE IN (LCORDLINE)
=GFOPENTABLE(OARIAAPPLICATION.WORKDIR+LCORDLINE,'ORDLINES','EX')

SELECT(LCORDLINE)
INDEX ON CORDTYPE+ORDER+STR(LINENO,6) TAG ORDLINE
SET ORDER TO ORDLINES

SELECT BOMVAR
=AFIELDS(LAFILESTRU)
LNFILESTRU = ALEN(LAFILESTRU,1)
DIMENSION LAFILESTRU[lnFileStru+2,18]


LAFILESTRU[lnFileStru+1,1] = 'nRecno'
LAFILESTRU[lnFileStru+1,2] = 'N'
LAFILESTRU[lnFileStru+1,3] = 10
LAFILESTRU[lnFileStru+1,4] = 0
LAFILESTRU[lnFileStru+2,1] = 'cStatus'
LAFILESTRU[lnFileStru+2,2] = 'C'
LAFILESTRU[lnFileStru+2,3] = 1
LAFILESTRU[lnFileStru+2,4] = 0
FOR LNLOOP = 1 TO  2
  STORE ' ' TO  LAFILESTRU[lnFileStru+lnLoop,7],LAFILESTRU[lnFileStru+lnLoop,8],;
    LAFILESTRU[lnFileStru+lnLoop,9],LAFILESTRU[lnFileStru+lnLoop,10],;
    LAFILESTRU[lnFileStru+lnLoop,11],LAFILESTRU[lnFileStru+lnLoop,12],;
    LAFILESTRU[lnFileStru+lnLoop,13],LAFILESTRU[lnFileStru+lnLoop,14],;
    LAFILESTRU[lnFileStru+lnLoop,15],LAFILESTRU[lnFileStru+lnLoop,16]
  STORE 0 TO    LAFILESTRU[lnFileStru+lnLoop,17] ,LAFILESTRU[lnFileStru+lnLoop,18]
ENDFOR
IF USED(LCT_BOMVAR)
  ZAP IN (LCT_BOMVAR)
ELSE
  =GFCRTTMP(LCT_BOMVAR,@LAFILESTRU,[cIdType+cCost_Id+STR(LineNo,6)],LCT_BOMVAR)
ENDIF

* Create a temp table to append HDR lines into, It will be used also for the PRM promo ( with no problems )
LNFILESTRU = 52

DECLARE LAFILESTRU[lnFileStru,18]
LNFILESTRU = 1
LAFILESTRU[lnFileStru, 1] = 'BATCHNO'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 5
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 2
LAFILESTRU[lnFileStru, 1] = 'BATCHDT'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 8
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 3
LAFILESTRU[lnFileStru, 1] = 'VENDORID'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 7
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 4
LAFILESTRU[lnFileStru, 1] = 'RECORDTYPE'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 1
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 5
LAFILESTRU[lnFileStru, 1] = 'ORDERNO'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 9
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 6
LAFILESTRU[lnFileStru, 1] = 'ORDERSEQ'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 3
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 7
LAFILESTRU[lnFileStru, 1] = 'SOURCECODE'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 7
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 8
LAFILESTRU[lnFileStru, 1] = 'ORDERDATE'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 8
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 9
LAFILESTRU[lnFileStru, 1] = 'SHIPTONAME'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 35
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 10
LAFILESTRU[lnFileStru, 1] = 'SHIPTOADD1'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 30
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 11
LAFILESTRU[lnFileStru, 1] = 'SHIPTOADD2'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 30
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 12
LAFILESTRU[lnFileStru, 1] = 'SHIPTOCITY'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 26
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 13
LAFILESTRU[lnFileStru, 1] = 'SHIPTOSTAT'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 2
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 14
LAFILESTRU[lnFileStru, 1] = 'SHIPTOZIP'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 9
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 15
LAFILESTRU[lnFileStru, 1] = 'ITEMQTY'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 3
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 16
LAFILESTRU[lnFileStru, 1] = 'ITEMNO'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 5
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 17
LAFILESTRU[lnFileStru, 1] = 'ITEMSUB'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 3
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 18
LAFILESTRU[lnFileStru, 1] = 'VENDORITEM'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 15
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 19
LAFILESTRU[lnFileStru, 1] = 'DRSVNDITEM'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 40
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 20
LAFILESTRU[lnFileStru, 1] = 'ITEMSEQ'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 3
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 21
LAFILESTRU[lnFileStru, 1] = 'ITEMSIZE'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 3
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 22
LAFILESTRU[lnFileStru, 1] = 'BIRTHMONTH'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 2
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 23
LAFILESTRU[lnFileStru, 1] = 'PERSONAL'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 20
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 24
LAFILESTRU[lnFileStru, 1] = 'ITEMPRICE'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 7
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 25
LAFILESTRU[lnFileStru, 1] = 'DEPOSAMT'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 7
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 26
LAFILESTRU[lnFileStru, 1] = 'BALANCE'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 7
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 27
LAFILESTRU[lnFileStru, 1] = 'SHIPHANDL'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 7
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 28
LAFILESTRU[lnFileStru, 1] = 'TAXAMT'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 7
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 29
LAFILESTRU[lnFileStru, 1] = 'TOTALAMT'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 7
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 30
LAFILESTRU[lnFileStru, 1] = 'SYNDNBR'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 3
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 31
LAFILESTRU[lnFileStru, 1] = 'SYNDORDER'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 15
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 32
LAFILESTRU[lnFileStru, 1] = 'BILLTONAME'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 35
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 33
LAFILESTRU[lnFileStru, 1] = 'BILLTOADD1'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 30
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 34
LAFILESTRU[lnFileStru, 1] = 'BILLTOADD2'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 30
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 35
LAFILESTRU[lnFileStru, 1] = 'BILLTOCITY'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 26
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 36
LAFILESTRU[lnFileStru, 1] = 'BILLTOSTAT'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 2
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 37
LAFILESTRU[lnFileStru, 1] = 'BILLTOZIP'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 9
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 38
LAFILESTRU[lnFileStru, 1] = 'CUSTEMAIL'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 50
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 39
LAFILESTRU[lnFileStru, 1] = 'SHIPMTHD'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 1
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 40
LAFILESTRU[lnFileStru, 1] = 'REJ'
LAFILESTRU[lnFileStru, 2] = 'L'
LAFILESTRU[lnFileStru, 3] = 1
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 41
LAFILESTRU[lnFileStru, 1] = 'style'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 19
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 42
LAFILESTRU[lnFileStru, 1] = 'SIZE'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 10
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 43
LAFILESTRU[lnFileStru, 1] = 'PACK_ID'
LAFILESTRU[lnFileStru, 2] = 'C'
LAFILESTRU[lnFileStru, 3] = 16
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 44
LAFILESTRU[lnFileStru, 1] = 'QTY1'
LAFILESTRU[lnFileStru, 2] = 'N'
LAFILESTRU[lnFileStru, 3] = 6
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 45
LAFILESTRU[lnFileStru, 1] = 'QTY2'
LAFILESTRU[lnFileStru, 2] = 'N'
LAFILESTRU[lnFileStru, 3] = 6
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 46
LAFILESTRU[lnFileStru, 1] = 'QTY3'
LAFILESTRU[lnFileStru, 2] = 'N'
LAFILESTRU[lnFileStru, 3] = 6
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 47
LAFILESTRU[lnFileStru, 1] = 'QTY4'
LAFILESTRU[lnFileStru, 2] = 'N'
LAFILESTRU[lnFileStru, 3] = 6
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 48
LAFILESTRU[lnFileStru, 1] = 'QTY5'
LAFILESTRU[lnFileStru, 2] = 'N'
LAFILESTRU[lnFileStru, 3] = 6
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 49
LAFILESTRU[lnFileStru, 1] = 'QTY6'
LAFILESTRU[lnFileStru, 2] = 'N'
LAFILESTRU[lnFileStru, 3] = 6
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 50
LAFILESTRU[lnFileStru, 1] = 'QTY7'
LAFILESTRU[lnFileStru, 2] = 'N'
LAFILESTRU[lnFileStru, 3] = 6
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 51
LAFILESTRU[lnFileStru, 1] = 'QTY8'
LAFILESTRU[lnFileStru, 2] = 'N'
LAFILESTRU[lnFileStru, 3] = 6
LAFILESTRU[lnFileStru, 4] = 0

LNFILESTRU = 52
LAFILESTRU[lnFileStru, 1] = 'LastLine'
LAFILESTRU[lnFileStru, 2] = 'L'
LAFILESTRU[lnFileStru, 3] = 1
LAFILESTRU[lnFileStru, 4] = 0



FOR LNROWPOS =  1 TO LNFILESTRU
  FOR LNCOLPOS = 7 TO 16
    LAFILESTRU[lnRowPos, lnColPos] = ""
  ENDFOR
  FOR LNCOLPOS = 17 TO 18
    LAFILESTRU[lnRowPos, lnColPos] = 0
  ENDFOR
ENDFOR

*- Create a temporary file (tmpOrders) with the above structure and add an index tag on ORDER
*=gfCrtTmp( lcTmImprt ,@laFileStru,'ORDERSEQ','ORDERSEQ',.F.)
=GFCRTTMP( LCTMPIMPRT ,@LAFILESTRU,'ORDERSEQ','ORDERSEQ',.F.)
SELECT (LCTMPIMPRT)
SET ORDER TO

SELECT(LNALIAS)
*- End of FUNCTION lfCratTemp

*!**************************************************************************
*!* Name        : lfAddUsrDt
*!* Developer   : TMI - TAREK MOHAMMED IBRAHIM
*!* Date        : 04/11/2011
*!* Purpose     : Add user id ,date  and time to files08/01/2002
*!***************************************************************************
FUNCTION LFADDUSRDT
PARAMETERS LCALIAS
PRIVATE LCCURRALS
LCCURRALS = SELECT()
IF !EMPTY(LCALIAS)
  SELECT (LCALIAS)
ENDIF
REPLACE CADD_USER WITH OARIAAPPLICATION.USER_ID,;
  DADD_DATE WITH OARIAAPPLICATION.SYSTEMDATE,;
  CADD_TIME WITH TIME()

SELECT (LCCURRALS)
*- End of FUNCTION lfAddUsrDt

*!**************************************************************************
*!* Name        : lfSaveData
*!* Developer   : TMI - TAREK MOHAMMED IBRAHIM
*!* Date        : 04/11/2011
*!* Purpose     : Save temp file to Actual orders
*!***************************************************************************
FUNCTION LFSAVEDATA

PRIVATE LLORDSAVED
LNALIAS = SELECT()

STORE 'O'  TO LCORDTYPE
DECLARE LAWAREHOUSES[1,2]  , LASETUPS[6,2] , LAKEYFIELD[2,4] , LASEASONS[1,2] ,;
  LACODES[1,10]      , LASOURCE[1,2] , LAORDSTATUS [1]
STORE ''   TO LAWAREHOUSES , LCODEFWARE    , LCWAREHOUSE     , LCSCFIELDS ,;
  LASEASONS     , LACODES       , LCODEFSES       , LCSEASON   ,LAORDSTATUS
STORE .F.  TO LASETUPS     , LLFOUND1      , LLFOUND2        , LLBROWSE
STORE 1    TO LNWAREHOUSE  , LNORDSTATUS   , LNSEASON
STORE m.STNAME TO LCSHIPNAME
STORE m.CADDRESS1 TO LCSHIPADD1
STORE m.CADDRESS2 TO LCSHIPADD2
STORE PADR(m.CADDRESS3,15,' ')+','+m.CADDRESS4+','+m.CADDRESS5 TO LCSHIPADD3
STORE ' ' TO LCSHIPADD4
STORE ' ' TO LCSHIPADD5

STORE 0    TO LNBOOK    , LNOPEN
STORE 0.00 TO LNOPENAMT , LNBOOKAMT , LNTOTAMT

*-- variables of SOUPDATE.PRG [Start]
DECLARE LAVARIABLES[6] , LASCRMODE[4]
STORE .F. TO LLCONTINUE , LLBOMVARNT , LLCDPERL
STORE ''  TO LCFLTOUPD  , LCSESSION  , LCFILES   , LAVARIABLES , LCGLYEAR , LCGLPERIOD ,;
  LCEXRSIN   , LCUNTSIN   , LCODEFDIV , LCSCRMODE   , LCCURRORD
STORE {}  TO LDDEFORDDATE
LCFLTOUPD = GFTEMPNAME()
*-- variables of SOUPDATE.PRG [End]

LASETUPS[1,1]  = 'M_PACK'           && System has been steup to use packs
LASETUPS[2,1]  = 'M_STY_COM'        && Edit sales reps commissions at style level
LASETUPS[3,1]  = 'M_OR_NOTE'        && Edit order lines notepad
LASETUPS[4,1]  = 'M_LINK_GL'        && System has been linked to GL
LASETUPS[5,1]  = 'M_WareHouse'      && System has been steup to use multiple warehouses
LASETUPS[6,1]  = 'M_GenOrNum'       && Generate order number manually
=GFGETMEMVAR(@LASETUPS,GCACT_COMP)

*-- variables of SOUPDATE.PRG [Start]
LAVARIABLES[1] = 'ldDefOrdDate'
LAVARIABLES[2] = 'lcODefSes'
LAVARIABLES[3] = 'lcODefDiv'
LAVARIABLES[4] = 'lcODefWare'
LAVARIABLES[5] = 'lcScrMode'
LAVARIABLES[6] = 'lcCurrOrd'
*-- variables of SOUPDATE.PRG [End]

LCSCFIELDS = 'ORDER,ACCOUNT,STORE,CUSTPO,STATUS,MULTI,MULTIPO,ENTERED,START,'+;
  'COMPLETE,cTermCode,SHIPVIA,SPCINST,SEASON,cDivision,DISC,DEPT,'+;
  'NOTE1,NOTE2,BUYER,PHONE,CINSUR,BULK,CREORDER,PRIORITY,CFACCODE,'+;
  'REP1,COMM1,REP2,COMM2,CWARECODE,LINK_CODE,CCURRCODE,NEXRATE,BOOK,BOOKAMT,'+;
  'SHIP,SHIPAMT,CANCEL,CANCELAMT,OPEN,OPENAMT,CFROMORDER,'+;
  'CANCELLED,DECL_DATE,DECL_CODE,CCANCRESON,APPROVAL,APPRAMT,'+;
  'NCURRUNIT,Alt_ShpTo,CORDERCAT,GL_SALES,INT_VEND,EVENT_COD,'+;
  'BILLNO,MERC_TYPE,BLANK_ORD,DISTRB_NO,CCLASS,LFROMWEB'


SELECT WAREHOUS
SELECT CDESC,CWARECODE FROM WAREHOUS INTO ARRAY LAWAREHOUSES

LNWAREHOUSE = ASCAN(LAWAREHOUSES,LCODEFWARE)
LNWAREHOUSE = IIF(LNWAREHOUSE=0,1,ASUBSCRIPT(LAWAREHOUSES,LNWAREHOUSE,1))

STORE .F. TO LLMFDSPRC , LLPODSPRC
LLORDSAVED = .F.
*--Loop throght the temp file lcOrdHdr to save data
SELECT (LCORDHDR)
SET ORDER TO TAG &LCORDHDR

LLORDSAVED = .T.
SCATTER FIELDS &LCSCFIELDS MEMVAR

SELECT (LCORDLINE)
LOCATE
SELECT (LCORDHDR)
LLCONTINUE = .T.

LOFORMSET = CREATEOBJECT('Custom')
LOFORMSET.ADDPROPERTY('laEvntTrig[1]','SOIMPRO   ')
LOFORMSET.ADDPROPERTY('lcdeposittemp',GFTEMPNAME())
LOFORMSET.ADDPROPERTY('Activemode','A')
LOFORMSET.ADDPROPERTY('llUpdate',.F.)


DO LFSAVSCR IN (OARIAAPPLICATION.APPLICATIONHOME + 'SO\SOUPDATE.FXP') ;
  WITH .F., 'A', LCORDHDR,LCORDLINE,.F.,.F.,LCT_BOMVAR ,LOFORMSET

LNORDCOUNT = LNORDCOUNT + 1
DIMENSION LAORDERS[lnOrdCount]
LAORDERS[lnOrdCount] = ORDHDR.ORDER
REPLACE FLAG WITH ' ' IN ORDHDR
*B609822,1 MMT 02/06/2012 Custom Import SO program does not save SOs[T20120119.0025][Start]
*loFormSet.AddProperty('DataSessionId',oAriaApplication.laRemoteTable[1].lnDataSession)
LOFORMSET.ADDPROPERTY('DataSessionId',SET("Datasession"))
*B609822,1 MMT 02/06/2012 Custom Import SO program does not save SOs[T20120119.0025][END]
INSERT INTO ORDERCHG (CORDTYPE,ORDER       ,LINENO,CCHRGCODE,CCHRGTYPE,NCHRGAMNT,DADD_DATE,CADD_USER,CADD_TIME) VALUES ;
  ('O'     ,ORDHDR.ORDER,1,"FREIGH","C",LNSHIPHANDL,OARIAAPPLICATION.SYSTEMDATE,OARIAAPPLICATION.USER_ID,GFGETTIME())

INSERT INTO ORDERCHG (CORDTYPE,ORDER,LINENO,CCHRGCODE,CCHRGTYPE,NCHRGAMNT,DADD_DATE,CADD_USER,CADD_TIME) VALUES ;
  ('O',ORDHDR.ORDER,2,"TAXES","T",LNTAXAMT,OARIAAPPLICATION.SYSTEMDATE,OARIAAPPLICATION.USER_ID,GFGETTIME())

=LFSAVEFILES()

*--Delele Records of the immediately saved order
SELECT (LCORDLINE)
DELETE ALL
PACK

SELECT (LCORDHDR)
DELETE ALL
PACK

*- End of FUNCTION lfSaveData

*!**************************************************************************
*!* Name        : lfSavefiles
*!* Developer   : TMI - TAREK MOHAMMED IBRAHIM
*!* Date        : 04/11/2011
*!* Purpose     : commit updated data
*!***************************************************************************
FUNCTION LFSAVEFILES
LCTRANCODE = OARIAAPPLICATION.REMOTECOMPANYDATA.BEGINTRAN(OARIAAPPLICATION.ACTIVECOMPANYCONSTR,3,'')
IF TYPE('lcTranCode') = 'N'
  =OARIAAPPLICATION.REMOTECOMPANYDATA.CHECKRETRESULT("BeginTran",LCTRANCODE,.T.)
  RETURN .F.
ENDIF
LNUPDATED = 0
LNARYLEN = ALEN(OARIAAPPLICATION.LAREMOTETABLE)
FOR LNCOUNTER=1 TO LNARYLEN
  IF OARIAAPPLICATION.LAREMOTETABLE[lnCounter].LNDATASESSION == LOFORMSET.DATASESSIONID
    IF !OARIAAPPLICATION.LAREMOTETABLE[lnCounter].TABLEUPDATE(LCTRANCODE)
      LNUPDATED=LNCOUNTER
      EXIT
    ENDIF
  ENDIF
NEXT
IF LNUPDATED>0
  OARIAAPPLICATION.REMOTECOMPANYDATA.ROLLBACKTRAN(LCTRANCODE)
  MESSAGEBOX('Saving Process is Rolled Back')
  THISFORMSET.UNDO()
  RETURN
ELSE
  OARIAAPPLICATION.REMOTECOMPANYDATA.COMMITTRAN(LCTRANCODE)
ENDIF
*- End of FUNCTION lfSavefiles

*!**************************************************************************
*!* Name        : lfVerify
*!* Developer   : HIA - Hassan Ibrahim
*!* Date        : 17/04/2012
*!* Purpose     : Check that the imported flat file is ok
*!***************************************************************************
FUNCTION LFVERIFY
LOCAL LNSLCT
LNSLCT = SELECT(0)
SELECT (LCTMPIMPRT)
LOCATE
PRIVATE LCDIV
STORE ''  TO LCERRSKU, LCERRSKUQTY, LCERRDIV, LCERRSES, lcOrderErrors

LCDIV = '  '
LCERRDIV = LCCR
SCAN
  SELECT (LCTMPIMPRT)
  *! E303163,1 HIA Validate Custpo, address format [T20120228.0054][BEGIN]
  SELECT OrdHdr
  LOCATE 
  IF SEEK(LCRPACCT+&LCTMPIMPRT..ORDERNO,"ordhdr","ORDCUST") AND INLIST(ORDHDR.STATUS, "O", "H", "C")

    IF NOT (&LCTMPIMPRT..ORDERNO+"|" $ LCREJORDERS)
      lcOrderErrors =  lcOrderErrors + "Order No. : "+&LCTMPIMPRT..ORDERNO+" Exist Before, Please refer to Sales Order Screen. "+ LCCR    
      LCREJORDERS = LCREJORDERS +"|"+&LCTMPIMPRT..ORDERNO+"|"
    ENDIF
    LLFOUND = .F.
    LOOP
  ENDIF

  SELECT (LCTMPIMPRT)
  *! E303163,1 HIA Validate Custpo, address format [T20120228.0054][END]

  *SCATTER MEMVAR MEMO

  *!*	  SELECT (lcTmImprt)
  *!*	  APPEND BLANK
  *!*	  GATHER MEMVAR MEMO

  *- check for SKU's
  *MType = ""
  LLFOUND = .F.
  LCSKU = ALLTRIM(&LCTMPIMPRT..ITEMNO+&LCTMPIMPRT..ITEMSUB)
  MKEY  = PADR(ALLTRIM(LCRPACCT),5)+PADR(ALLTRIM(LCSKU),16)
  DO CASE
    && As per Alaa, this customer not used packs.
    *!*	  CASE   (SEEK('P' + Mkey , 'Spck_Lin') .OR. SEEK('P' + '*****' + PADR(ALLTRIM(MStyle + MSiz),16),'Spck_Lin'))
    *!*	    SELECT Spck_Lin
    *!*	    Mkey  = Spck_Lin.Account + Spck_Lin.Pack_ID
    *!*	    MType = 'P'
    *!*	    MPrePck = .F.
    *!*	    LLFOUND = .T.
    *!*	    *SUM REST TOTQTY TO MPckQty WHILE TYPE + Account + Pack_ID = 'P' + Mkey
    *!*	    *=SEEK('P'+Mkey)

    *!*	    MPckQty =0
    *!*	    SCAN REST WHILE TYPE + Account + Pack_ID = 'P' + Mkey
    *!*	      m.STYLE = Spck_Lin.STYLE
    *!*	      m.Pack_ID = Spck_Lin.Pack_ID

    *!*	      IF MPckQty <> 0
    *!*	        SELECT (lcTmImprt)
    *!*	        APPEND BLANK
    *!*	        GATHER MEMVAR MEMO
    *!*	      ENDIF

    *!*	      REPLACE &lcTmImprt..STYLE WITH Spck_Lin.STYLE
    *!*	      REPLACE &lcTmImprt..Pack_ID WITH Spck_Lin.Pack_ID
    *!*	      FOR i = 1 TO 8
    *!*	        lcStr = ALLTRIM(STR(i))
    *!*	        REPLACE &lcTmImprt..QTY&lcStr.  WITH Spck_Lin.QTY&lcStr.
    *!*	      ENDFOR
    *!*	      SELECT Spck_Lin
    *!*	      MPckQty =  MPckQty + TOTQTY

    *!*	    ENDSCAN


  CASE SEEK('S' + MKEY , 'Spck_Lin')
    *MType   = 'S'
    MPREPCK = (SPCK_LIN.TOTQTY = 0)
    TOTQTY  = 1
    MKEY    = SPCK_LIN.ACCOUNT +SPCK_LIN.STYLE
    LLFOUND = .T.
    REPLACE &LCTMPIMPRT..STYLE WITH SPCK_LIN.STYLE
    FOR I = 1 TO 8
      LCSTR = ALLTRIM(STR(I))
      IF SPCK_LIN.QTY&LCSTR. = 1
        REPLACE &LCTMPIMPRT..SIZE  WITH LCSTR
        EXIT
      ENDIF
    ENDFOR

  OTHERWISE
    LCERRSKU =  LCERRSKU + "Order No. : "+&LCTMPIMPRT..ORDERNO+" Item: "+ALLTRIM(&LCTMPIMPRT..ITEMNO+&LCTMPIMPRT..ITEMSUB)+ LCCR

    IF NOT (&LCTMPIMPRT..ORDERNO+"|" $ LCREJORDERS)
      LCREJORDERS = LCREJORDERS +"|"+&LCTMPIMPRT..ORDERNO+"|"
    ENDIF

    LLFOUND = .F.
  ENDCASE
  *- check upc qty
  =SEEK(SPCK_LIN.STYLE,'STYLE')
  *!*	  IF VAL(SUBSTR(MRLORDER,4))<=0
  *!*	    lcErrSKUQty = lcErrSKUQty + lcUPC + lcCr
  *!*	  ENDIF
  IF LLFOUND = .T.
    *- Check division
    IF EMPTY(LCDIV )
      LCDIV = STYLE.CDIVISION
    ELSE
      LCMRLORDER = &LCTMPIMPRT..ORDERNO
      IF LCDIV <> STYLE.CDIVISION AND !LCMRLORDER $ LCERRDIV
        LCERRDIV = LCERRDIV + 'Order # '+&LCTMPIMPRT..ORDERNO+', styles are related to more than one division'+LCCR
      ENDIF
    ENDIF

    *- Check empty seasons
    IF EMPTY(STYLE.SEASON) AND !(STYLE.STYLE $ LCERRSES)
    *  LCERRSES = LCERRSES + STYLE.STYLE + LCCR
      LCERRSES = LCERRSES + 'Order # '+&LCTMPIMPRT..ORDERNO+', Style '+STYLE.STYLE+', Has No Season. '+LCCR
    ENDIF

  ENDIF
ENDSCAN

*! E303163,1 HIA Validate Custpo, address format [T20120228.0054][Begin]
IF !EMPTY(lcOrderErrors)
  lfErr('The following Orders exist Before :')
  lfErr(lcOrderErrors)
ENDIF
*! E303163,1 HIA Validate Custpo, address format [T20120228.0054][End]

IF !EMPTY(LCERRSKU)
  LFERR('The following SKUs are not found in the system:')
  LFERR(LCERRSKU)
ENDIF

IF !EMPTY(LCERRSES)
  LFERR('The following Styles are not assigned to a season:')
  LFERR(LCERRSES)
ENDIF

IF !EMPTY(LCERRDIV)
  LFERR(LCERRDIV)
ENDIF


&& Determine the last line for each order, to insert the order hder
&& Reject all order lines, if there exist one line has incorrect SKU.
SELECT (LCTMPIMPRT)
LOCATE
REPLACE &LCTMPIMPRT..LASTLINE WITH .F. , &LCTMPIMPRT..REJ WITH .F. ALL

LOCATE
LCCUSTPO = &LCTMPIMPRT..ORDERNO
SCAN

  IF NOT( LCCUSTPO == &LCTMPIMPRT..ORDERNO)
    IF !BOF()
      SKIP -1
      REPLACE &LCTMPIMPRT..LASTLINE WITH .T.
      SKIP
    ENDIF
  ENDIF

  LCCUSTPO = &LCTMPIMPRT..ORDERNO
  IF LCCUSTPO+"|" $ LCREJORDERS
    REPLACE &LCTMPIMPRT..REJ WITH .T.
  ENDIF
  SELECT (LCTMPIMPRT)
  IF RECNO() = RECCOUNT()
    REPLACE &LCTMPIMPRT..LASTLINE WITH .T.
  ENDIF

ENDSCAN

*!*	IF !EMPT(lcErrStr)
*!*	  lcAdd2Msg = ''
*!*	  lcAdd2Msg = lcAdd2Msg + REPL('-',70)+ lcCr
*!*	  lcAdd2Msg = lcAdd2Msg + 'Data imported from file '+lcRpFlPath+' at Date : '+DTOC(DATE())+' Time : '+TIME() + lcCr
*!*	  lcAdd2Msg = lcAdd2Msg + REPL('-',70)+ lcCr
*!*	  lcAdd2Msg = lcAdd2Msg + 'Import Failed,..'

*!*	  lcErrStr = lcAdd2Msg + lcCr + lcErrStr
*!*	  LOCAL oObject,o
*!*	  o = CREATEOBJECT('TextBox')
*!*	  o.VALUE = lcErrStr
*!*	  o.READONLY = .T.

*!*	  oObject = CREATEOBJECT("AriaZoom",o)
*!*	  oObject.CAPTION = 'Importing Orders from Morely Error Log file'
*!*	  oObject.VISIBLE = .T.
*!*	  oObject.SHOW()
*!*	ENDIF
SELECT (LNSLCT)
RETURN EMPTY(LCERRSTR)
*- End of FUNCTION lfVerify

*!**************************************************************************
*!* Name        : lfErr
*!* Developer   : TMI - TAREK MOHAMMED IBRAHIM
*!* Date        : 04/11/2011
*!* Purpose     : Add a line to the Error string
*!***************************************************************************
FUNCTION LFERR
PARAMETERS LCERR
LCERRSTR = LCERRSTR + LCERR + CHR(13)
*- End of FUNCTION lfErr

*!**************************************************************************
*!* Name        : lfClear
*!* Developer   : TMI - TAREK MOHAMMED IBRAHIM
*!* Date        : 04/11/2011
*!* Purpose     : Erase temp files , close opened files
*!***************************************************************************
FUNCTION LFCLEAR
=LFERASE(LCTMPIMPRT )
=LFERASE(LCLOGFILE )
=LFERASE(LCORDHDR )
=LFERASE(LCORDLINE )
=LFERASE(LCT_BOMVAR )

*- Close files
=GFCLOSETABLE('BomVar')
=GFCLOSETABLE('STYLE')
=GFCLOSETABLE('SCALE')
=GFCLOSETABLE('WAREHOUS')
=GFCLOSETABLE('CUSTOMER')
=GFCLOSETABLE('SALESREP')
=GFCLOSETABLE('ORDHDR')
=GFCLOSETABLE('ORDLINE')
=GFCLOSETABLE('UnCmSess')
=GFCLOSETABLE('STYDYE')
=GFCLOSETABLE('NOTEPAD')
=GFCLOSETABLE('SycCurr')
=GFCLOSETABLE('SYCEXCH')
=GFCLOSETABLE('SYCINT')
=GFCLOSETABLE('STYLEUPC')
*- End of FUNCTION lfClear

*!**************************************************************************
*!* Name        : lfErase
*!* Developer   : TMI - TAREK MOHAMMED IBRAHIM
*!* Date        : 04/11/2011
*!* Purpose     : Erase temp files
*!***************************************************************************
FUNCTION LFERASE
LPARAMETERS LCALIAS
LOCAL LNSLCT,LCDBF
LNSLCT = SELECT(0)
IF USED(LCALIAS)
  LCDBF = STRTRAN( DBF(LCALIAS) , '.DBF' , '.*' )
  USE IN (LCALIAS)
  ERASE (LCDBF)
ENDIF
SELECT (LNSLCT)
*- End of FUNCTION lfErase

*!**************************************************************************
*!* Name        : lfGetDt
*!* Developer   : TMI - TAREK MOHAMMED IBRAHIM
*!* Date        : 04/11/2011
*!* Purpose     : convert string to date using DATE func.
*!***************************************************************************
FUNCTION LFGETDT
PARAMETERS LCFLD

RETURN DATE(VAL(SUBSTR(LCFLD,1,4)),VAL(SUBSTR(LCFLD,5,2)),VAL(SUBSTR(LCFLD,7,2)))
*- End of FUNCTION lfGetDt

*!**************************************************************************
*!* Name        : lfGetLvl
*!* Developer   : TMI - TAREK MOHAMMED IBRAHIM
*!* Date        : 04/11/2011
*!* Purpose     : Get customer's level
*!***************************************************************************
FUNCTION LFGETLVL

IF CUSTOMER.PRICELVL = 'Q'
  DO CASE
  CASE STYLE.NATQTYC > 0 AND &LCORDLINE..TOTQTY > STYLE.NATQTYC
    LCLEVEL = 'C'
  CASE STYLE.NATQTYB > 0 AND &LCORDLINE..TOTQTY > STYLE.NATQTYB
    LCLEVEL = 'B'
  OTHERWISE
    LCLEVEL = 'A'
  ENDCASE
ELSE
  LCLEVEL=IIF(INLIST(CUSTOMER.PRICELVL,'A','B','C'),CUSTOMER.PRICELVL,'A')
ENDIF
RETURN LCLEVEL
*- End of FUNCTION lfGetLvl

*!**************************************************************************
*!* Name        : lfCustMsg
*!* Developer   : TMI - TAREK MOHAMMED IBRAHIM
*!* Date        : 04/11/2011
*!* Purpose     : just added to prevent a message to emits up from the SOUPDATE
*                 that a sales orders # is saved
*!***************************************************************************
FUNCTION LFCUSTMSG
**
*- End of FUNCTION lfCustMsg


************************************************

FUNCTION LFVOUTPTH

LOCAL LCEXT, LCDIRECT
LCEXT    = UPPER(ALLTRIM(SUBSTR(LCRPOUTNAM,AT('.',LCRPOUTNAM))))
LCDIRECT = SUBSTR(LCRPOUTNAM,1,AT('\',LCRPOUTNAM,OCCURS('\',LCRPOUTNAM)))

IF '?' $ LCRPOUTNAM OR (!EMPTY(LCRPOUTNAM) AND ((LCEXT <> '.CSV') .OR. !DIRECTORY(LCDIRECT)))
  LCRPOUTNAM = GETFILE('CSV', '','Save')
ENDIF

IF !EMPTY(LCRPOUTNAM)
  LCRPOUTNAM=  FORCEEXT(LCRPOUTNAM,'CSV')
ENDIF

IF !EMPTY(LCRPOUTNAM)
  SAVE TO OARIAAPPLICATION.DATADIR+'SOROFS'+'.MEM'  ALL LIKE LCRPOUTNAM
ENDIF

ENDFUNC


*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/11/2011
*! Purpose   : When Function of the Option grid
*!*************************************************************
FUNCTION LFWREPWHEN
LOOGSCROLL.PARENT.OGTOOLBAR.CNTPRINT.CMDPRINT.ENABLED =.F.
LOOGSCROLL.PARENT.OGTOOLBAR.CNTPRINT.CHKPRINTCRITERIA.ENABLED = .F.
LOOGSCROLL.PARENT.OGTOOLBAR.CNTEXTERNAL.CMDEMAIL.ENABLED =.F.
LOOGSCROLL.PARENT.OGTOOLBAR.CNTPRINT.CMDEXPORT.ENABLED =.F.
LCREPFORM = ""
IF EMPTY(LCRPOUTNAM)
  IF FILE(OARIAAPPLICATION.DATADIR+'SOROFS'+'.MEM')
    RESTORE FROM OARIAAPPLICATION.DATADIR+'SOROFS'+'.MEM' ADDITIVE
  ENDIF
ENDIF

*!*************************************************************


FUNCTION LFORDEXP
IF EMPTY(LCRPOUTNAM) OR !FILE(LCRPOUTNAM)
  =GFMODALGEN('TRM00000B00000','ALERT','','','Please Enter The Outgoing Shipment File Name.')
  RETURN .F.
ENDIF
IF EMPTY(LCRPACCT)
  =GFMODALGEN('TRM00000B00000','ALERT','','','Please Enter The Account Name.')
  RETURN .F.
ENDIF
*! C201516,1 MMT 09/10/2012 Add location option to custom receiving program from SETA[Start]
IF EMPTY(lcRpOrdWrh)
  =GFMODALGEN('TRM00000B00000','ALERT','','','Please Enter The Default Location')
  RETURN .F.
ENDIF
*! C201516,1 MMT 09/10/2012 Add location option to custom receiving program from SETA[End]
ENDFUNC


*!*************************************************************
*! Name      : lfvAccount
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : Validate Account
*!*************************************************************
FUNCTION LFVACCOUNT

PRIVATE LCOBJVAL
PRIVATE LNALSNO,LCCUSTORD,LCOBJNAME
LNALSNO = SELECT(0)
IF !USED('CUSTOMER')
  =GFOPENTABLE('Customer','Customer')
ENDIF
SELECT CUSTOMER
LCCUSTORD = ORDER()
GFSETORDER('CUSTOMER')
*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ LCRPACCT .OR. (!EMPTY(LCRPACCT) .AND. !GFSEEK('M' +LCRPACCT, 'CUSTOMER'))
  LLOBJRET = CUSBROWM(@LCRPACCT, '' , 'M')
  LCRPACCT= IIF(LLOBJRET , LCRPACCT, '')
ENDIF

SELECT CUSTOMER
GFSETORDER(LCCUSTORD)
SELECT(LNALSNO)
ENDFUNC
*!**************************************************************************
*!* Name        : lfDelOrder
*!* Developer   : HIA
*!* Date        : 04/09/2012
*!* Purpose     : Delete order
*!***************************************************************************
FUNCTION LFDELORDER

SELECT ORDHDR

LOFORMSET = CREATEOBJECT('Custom')
LOFORMSET.ADDPROPERTY('laEvntTrig[1]','SOROFS')
LOFORMSET.ADDPROPERTY('lcdeposittemp',GFTEMPNAME())
LOFORMSET.ADDPROPERTY('Activemode','A')
LOFORMSET.ADDPROPERTY('llUpdate',.F.)

LCSTATUS = ORDHDR.STATUS
***
LCOLDSETPROCEDURE =  SET("Procedure")
LCOLDSETPROCEDURE = OARIAAPPLICATION.CLIENTREPORTHOME + "SO\SOROFM.FXP" +IIF(EMPTY(LCOLDSETPROCEDURE),'',',')+LCOLDSETPROCEDURE
SET PROCEDURE TO &LCOLDSETPROCEDURE.
LNCHOICE  = 1
***
DO LFDELSCR IN (OARIAAPPLICATION.APPLICATIONHOME + 'SO\SOUPDATE.FXP') WITH .F.,LOFORMSET
***
IF 'SOROFM.FXP' $ SET("Procedure")
  RELEASE PROCEDURE (OARIAAPPLICATION.CLIENTREPORTHOME + 'SO\SOROFM.FXP')
ENDIF
****
IF LCSTATUS <> ORDHDR.STATUS
  *MMT
  *!*	  lnOrdCount = lnOrdCount + 1
  *!*	  DIMENSION laOrders[lnOrdCount]
  *!*	  laOrders[lnOrdCount] = ORDHDR.ORDER
  *MMT
  *REPLACE FLAG WITH ' ' IN ORDHDR
  LOFORMSET.ADDPROPERTY('DataSessionId',SET("Datasession"))
  =LFSAVEFILES()

  RETURN .T.
ENDIF
RETURN .F.


*- End of FUNCTION lfDelOrder

*!**************************************************************************
FUNCTION LFDISPLAYERRORS
LOCAL LNSLCT
LNSLCT = SELECT(0)

IF !EMPT(LCERRSTR)
  LCADD2MSG = ''
  LCADD2MSG = LCADD2MSG + REPL('-',70)+ LCCR
  LCADD2MSG = LCADD2MSG + 'Data imported from file '+LCRPFLPATH+' at Date : '+DTOC(DATE())+' Time : '+TIME() + LCCR
  LCADD2MSG = LCADD2MSG + REPL('-',70)+ LCCR
  LCADD2MSG = LCADD2MSG + 'Import Failed,..'

  LCERRSTR = LCADD2MSG + LCCR + LCERRSTR
  LOCAL OOBJECT,O
  O = CREATEOBJECT('TextBox')
  O.VALUE = LCERRSTR
  O.READONLY = .T.

  OOBJECT = CREATEOBJECT("AriaZoom",O)
  OOBJECT.CAPTION = 'Importing Orders from Morely Error Log file'
  OOBJECT.VISIBLE = .T.
  OOBJECT.SHOW()
ENDIF
SELECT (LNSLCT)
ENDFUNC
*! C201516,1 MMT 09/10/2012 Add location option to custom receiving program from SETA[Start]
*!*************************************************************
*! Name      : lfvOrdWare
*: Developer : Mariam Mazhar(MMT)
*: Date      : 09/10/2012
*! Purpose   : Valid function of the Order Warehouse
*!*************************************************************
*! Called from : Option grid [Order Warehouse Get field]
*!*************************************************************
*! Calls       : gfBrowWare()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvOrdWare
lcControl  = loOGScroll.FocusControl
lcCurVar   = loOGScroll.&lcControl.
lcPkWOldVl = lcCurVar.oldValue

lcObjName = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field
IF !USED('WAREHOUS')
  =GFOPENTABLE(OARIAAPPLICATION.DATADIR+'WAREHOUS','WAREHOUS','SH')
ENDIF
*-- IF The user want to Browse or if the Warehouse he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !gfSEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , lcPkWOldVl , lcObjVal)
  lcPkWOldVl = lcObjVal
  lcRpOrdWrh  = lcObjVal
ENDIF    && End of IF
*! C201516,1 MMT 09/10/2012 Add location option to custom receiving program from SETA[ENd]