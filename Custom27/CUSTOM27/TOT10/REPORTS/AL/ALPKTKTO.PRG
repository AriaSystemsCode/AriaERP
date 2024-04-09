************************************************************************
*: Program file  : ALPKTKTO.PRG
*: Program desc. : Customized Sales Order Allocation Picking Ticket Form [ For Tot10 ]
*: System        : Aria Advantage Series.
*: Module        : Sales Order (AL)
*: Developer     : Abdou Elgendy [ABD]
*: Date          : 05/18/2003
*:***************************************************************************
*: Calls : 
*:    Procedures : Lpfilempln , Lpsidlabls , Lphrdlabls , Lpoldfoter
*:               : Lpnewfoter , Lpprtsku   , Lfgetloc   , Lfcontuloc
*:               : Lfchkrow   , Lfprnstrn
*:               : ---------------
*:    Functions  : ....
*:***************************************************************************
*: Example : DO Sordjac
*:***************************************************************************
*: This Program is due to C200535 ...
*:***************************************************************************
*: Modifications
*:***************************************************************************
*:
LLNOREC  = .F.
XPIKNAME = LLPRNTCOMP
PRTPRICE = LLRPSTYPRC
PRTNPAD  = LLRPORDNOT
MSGL1    = LCRPMSG1
MSGL2    = LCRPMSG2
MSGL3    = LCRPMSG3
LCPRNSTLOC = LLRPSTYLOC
LCPRNSKPCK = LLRPSKUBCK
LCPRNSKSIZ = LLRPSKUSIZ
LNLENGTH   = 7

STORE SPACE(1) TO LCSTRTOPRN, LCSKUSIZE, LCSCLSTRN, LCSTR, HLINE1
LNLEN  = 0
HLINE2 = TRIM(LACOMPADD(1))
HLINE3 = TRIM(LACOMPADD(2))
HLINE4 = TRIM(LACOMPADD(3))

DIMENSION LADIVLNAME[ 1, 2]
LADIVLNAME[ 1, 1] = 'DIVLNAME'
LADIVLNAME[ 1, 2] = 'HLINE1'
LCCUSTPOVL = ''
IF LEN(TRIM(HLINE3))=0
   HLINE3 = HLINE4
   HLINE4 = SPACE(1)
ENDIF
ROW = 0
LLSCALE = .T.
LLENDPT = .F.
NEWDOC = .T.
LNNOTLINE = 1
LLRET = .F.

CLEAR TYPEAHEAD

SET DEVICE TO PRINTER

IF EOF(LCTMPORDL)
   LLNOREC = .T.
   = GFMODALGEN('TRM00052B00000','DIALOG')
   SET DEVICE TO SCREEN
   RETURN
ENDIF
SELECT SPCK_LIN
SET ORDER TO SPCKLINS
SELECT WHSLOC
SET ORDER TO WhsLocSt
SELECT (LCTMPORDL)
DO WHILE  .NOT. EOF(LCTMPORDL)
   SELECT (LCTMPORDL)
   LNCURREC = RECNO()
   IF NEWDOC
      SELECT (LCTMPORDL)
      XPIKTKT = PIKTKT
      XORDER = ORDER
      XDATE = PIKTKT.DATE
      XSTORE = PIKTKT.STORE
      SELECT ORDHDR
      = SEEK('O'+XORDER,'ORDHDR')
      XMULTI = MULTI
      XACCOUNT = ACCOUNT
      STORE 0.00 TO XORDTOT, XVALUE, SVALUE
      lcCustPoVl = IIF(ORDHDR.MultiPO,&lcTmpOrdL..CustPO,ORDHDR.CustPO)
      SELECT CUSTOMER
      XBTNAME = CUSTOMER.BTNAME
      XBTADDR1 = GFGETADR('CUSTOMER','','','',1,'2')
      XBTADDR2 = GFGETADR('CUSTOMER','','','',2,'2')
      XBTADDR3 = GFGETADR('CUSTOMER','','','',3,'2')
      IF LEN(TRIM(XBTADDR2))=0
         XBTADDR2 = XBTADDR3
         XBTADDR3 = ''
      ENDIF
      XTERM_DATA = GFCODDES(ORDHDR.CTERMCODE,'CTERMCODE')
      XSHIP_DATA = GFCODDES(IIF(ALLTRIM(ORDHDR.SHIPVIA)='*',CUSTOMER.SHIPVIA,ORDHDR.SHIPVIA),'SHIPVIA')
      XSPCI_DATA = GFCODDES(ORDHDR.SPCINST,'SPCINST')
      XSEAS_DATA = GFCODDES(ORDHDR.SEASON,'SEASON')
      XDIVI_DATA = GFCODDES(ORDHDR.CDIVISION,'CDIVISION')
      = GFRLTFLD(XDIVI_DATA,@LADIVLNAME,'CDIVISION')
      HLINE1 = IIF(EMPTY(HLINE1),LCCOMPNAME,HLINE1)
      STORE '' TO XSTNAME, XSTADDR1, XSTADDR2, XSTADDR3, XSTADDR4
      XPIECES = 0
   ENDIF
   SELECT ORDHDR
   SEEK CORDTYPE+XORDER
   IF ALT_SHPTO
      LCSHPTNAME = ORDHDR.STNAME
      LASHIPTO[ 1] = ORDHDR.CADDRESS1
      LASHIPTO[ 2] = ORDHDR.CADDRESS2
      LASHIPTO[ 3] = ORDHDR.CADDRESS3
      LASHIPTO[ 4] = ORDHDR.CADDRESS4
      LASHIPTO[ 5] = ORDHDR.CADDRESS5
      XSTNAME = LCSHPTNAME
      XSTADDR1 = LASHIPTO(1)
      XSTADDR2 = LASHIPTO(2)
      XSTADDR3 = TRIM(LASHIPTO(3))+' '+TRIM(LASHIPTO(4))+' '+LASHIPTO(5)
      IF LEN(TRIM(XSTADDR2))=0
         XSTADDR2 = XSTADDR3
         XSTADDR3 = ''
      ENDIF
   ELSE
      SELECT CUSTOMER
      IF  .NOT. EMPTY(CUSTOMER.DIST_CTR)
         = SEEK('S'+CUSTOMER.ACCOUNT+CUSTOMER.DIST_CTR)
      ENDIF
      XSTNAME = IIF(EMPTY(DBA),STNAME,DBA)
      XSTADDR1 = GFGETADR('CUSTOMER','','','',1)
      XSTADDR2 = GFGETADR('CUSTOMER','','','',2)
      XSTADDR3 = GFGETADR('CUSTOMER','','','',3)
      IF LEN(TRIM(XSTADDR2))=0
         XSTADDR2 = XSTADDR3
         XSTADDR3 = ''
      ENDIF
   ENDIF
   SELECT ORDHDR
   DO LPHRDLABLS
   IF LLRET
      LOOP
   ENDIF
   SELECT (LCTMPORDL)
   NEWDOC = .T.
   XTOTQTY = 0
   SCAN WHILE PIKTKT=XPIKTKT
      DO CASE
         CASE EOF()
            NEWDOC = .T.
            EXIT
         CASE TOTPIK<=0
            LOOP
         CASE ROW>=55
            NEWDOC = .F.
            EXIT
      ENDCASE
      SELECT STYLE
      IF SEEK(&lcTmpOrdL..Style) 
         XSTYDESC = DESC
         LCSTYLOCAT = LOCATION
         LCSCALE = SCALE
      ELSE
         XSTYDESC = ''
         LCSTYLOCAT = ''
         LCSCALE = ''
      ENDIF
      SELECT (LCTMPORDL)
      @ ROW, 1 SAY SUBSTR(STYLE,1,12)
      @ ROW, 13 SAY SUBSTR(STYLE,14,19)
      @ ROW, 19 SAY LEFT(LCSCALE,2)
      @ ROW, 21 SAY PIK1 PICTURE '@Z 9999'
      @ ROW, 26 SAY PIK2 PICTURE '@Z 9999'
      @ ROW, 30 SAY PIK3 PICTURE '@Z 9999'
      @ ROW, 35 SAY PIK4 PICTURE '@Z 9999'
      @ ROW, 39 SAY PIK5 PICTURE '@Z 9999'
      @ ROW, 44 SAY PIK6 PICTURE '@Z 9999'
      @ ROW, 48 SAY PIK7 PICTURE '@Z 9999'
      @ ROW, 53 SAY PIK8 PICTURE '@Z 9999'
      @ ROW, 57 SAY TOTPIK PICTURE '99999'
      IF PRTPRICE=.T.
         @ ROW, 66 SAY PRICE PICTURE '9999.99'
      ENDIF
      @ ROW, 74 SAY TOTPIK PICTURE '99999'
      DO LPSIDLABLS
      ROW = ROW+1
      = LFCHKROW('B')
      IF LCPRNSKPCK=.F.
         @ ROW, 1 SAY XSTYDESC
         IF LCPRNSTLOC=.T.
            = LFGETLOC()
            IF  .NOT. EMPTY(LCSTR)
               @ ROW, 60 SAY 'BINS    :'+SUBSTR(LCSTR,1,10)
               DO LPSIDLABLS
               ROW = ROW+1
               = LFCHKROW('B')
               IF LEN(LCSTR)>18
                  = LFCONTULOC()
               ENDIF
            ELSE
               DO LPSIDLABLS
               ROW = ROW+1
               = LFCHKROW('B')
            ENDIF
         ENDIF
      ELSE
         IF SEEK('P'+ORDLINE.ACCOUNT+ORDLINE.STYLE,'SPCK_LIN')
            @ ROW, 1 SAY 'PACK ID#:'+SPCK_LIN.PACK_ID
            @ ROW, 25 SAY XSTYDESC
            DO LPSIDLABLS
            ROW = ROW+1
            = LFCHKROW('B')
         ELSE
            DO LPPRTSKU
            IF LCPRNSKSIZ=.F.
               @ ROW, 0 SAY SUBSTR(LCSTRTOPRN,1,IIF(LEN(LCSTRTOPRN)>=40,40,LEN(LCSTRTOPRN)))
               @ ROW, IIF(LEN(LCSTRTOPRN)>=40,41,LEN(LCSTRTOPRN)+2) SAY XSTYDESC
               DO LPSIDLABLS
               ROW = ROW+1
               = LFCHKROW('B')
               IF LCPRNSTLOC=.T.
                  = LFGETLOC()
                  IF  .NOT. EMPTY(LCSTR)
                     @ ROW, 0 SAY 'BINS    :'+SUBSTR(LCSTR,1,85)
                     DO LPSIDLABLS
                     ROW = ROW+1
                     = LFCHKROW('B')
                     IF LEN(LCSTR)>85
                        = LFCONTULOC()
                     ENDIF
                  ELSE
                     DO LPSIDLABLS
                     ROW = ROW+1
                     = LFCHKROW('B')
                  ENDIF
               ENDIF
               DO LPSIDLABLS
               ROW = ROW+1
               = LFCHKROW('B')
            ELSE
               @ ROW, 0 SAY XSTYDESC
               IF LCPRNSTLOC=.T.
                  = LFGETLOC()
                  IF  .NOT. EMPTY(LCSTR)
                     @ ROW, 60 SAY 'BINS    :'+SUBSTR(LCSTR,1,10)
                     DO LPSIDLABLS
                     ROW = ROW+1
                     = LFCHKROW('B')
                     IF LEN(LCSTR)>18
                        = LFCONTULOC()
                     ENDIF
                  ELSE
                     DO LPSIDLABLS
                     ROW = ROW+1
                     = LFCHKROW('B')
                  ENDIF
               ELSE
                  DO LPSIDLABLS
                  ROW = ROW+1
                  = LFCHKROW('B')
               ENDIF
               = LFPRNSTRN()
               LCSKUSIZE = LCSTRTOPRN+' '+LCSKUSIZE
               @ ROW, 0 SAY SUBSTR(LCSKUSIZE,1,86)
               IF  .NOT. EMPTY(LCSKUSIZE)
                  DO LPSIDLABLS
                  ROW = ROW+1
                  = LFCHKROW('B')
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      DO LPSIDLABLS
      ROW = ROW+1
      = LFCHKROW('B')
      IF LCPRNSKSIZ=.T.
      ENDIF
      SELECT (LCTMPORDL)
      XTOTQTY = XTOTQTY+TOTQTY
      XVALUE = XVALUE+TOTPIK*PRICE
      XPIECES = XPIECES+TOTPIK
      SELECT (LCTMPORDL)
   ENDSCAN
   IF NEWDOC
      SELECT (LCTMPORDL)
      IF PRTNPAD=.T.
         SELECT NOTEPAD
         IF SEEK('B'+XORDER)
            LNOLDMEMW = SET('MEMOWIDTH')
            SET MEMOWIDTH TO 75
            LNMEMLINS = MEMLINES(NOTEPAD.MNOTES)
            @ ROW, 0 SAY '* -- N O T E S -- *'
            DO LPSIDLABLS
            ROW = ROW+1
            = LFCHKROW('B')
            DO WHILE LNNOTLINE<=LNMEMLINS
               IF ROW>=55
                  DO LPOLDFOTER
                  DO LPHRDLABLS
               ENDIF
               @ ROW, 0 SAY MLINE(MNOTES,LNNOTLINE)
               DO LPSIDLABLS
               ROW = ROW+1
               LNNOTLINE = LNNOTLINE+1
            ENDDO
            SET MEMOWIDTH TO LNOLDMEMW
         ENDIF
      ENDIF
      LNNOTLINE = 1
      LLENDPT = .T.
      DO LPFILEMPLN
      DO LPNEWFOTER
   ELSE
      LLENDPT = .F.
      DO LPFILEMPLN
      DO LPOLDFOTER
      LOOP
   ENDIF
   SELECT PIKTKT
   SEEK &lcTmpOrdL..PIKTKT
   LLSCALE = .T.
   SELECT (LCTMPORDL)
ENDDO
SET DEVICE TO SCREEN

*-- End OF Code.
*:*************************************************************
*: Name      : Lpfilempln
*: Developer : Abdou Elgendy [ABD]
*: Date      : 05/18/2003
*: Purpose   : 
*:*************************************************************
*: Called from : ...
*:*************************************************************
*: Calls       : ...
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : Do Lpfilempln
*:*************************************************************
*:
PROCEDURE Lpfilempln
DO WHILE ROW<=53
   DO LPSIDLABLS
   IF ROW<53
      ROW = ROW+1
   ELSE
      EXIT
   ENDIF
ENDDO
*-- End OF Lpfilempln
*:*************************************************************
*: Name      : Lpsidlabls
*: Developer : Abdou Elgendy [ABD]
*: Date      : 05/18/2003
*: Purpose   : 
*:*************************************************************
*: Called from : ...
*:*************************************************************
*: Calls       : ...
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : Do Lpsidlabls
*:*************************************************************
*:
PROCEDURE Lpsidlabls
DO CASE
   CASE ROW=34 .OR. ROW=50
      @ ROW, 93 SAY IIF(XPIKNAME=.T.,HLINE1,'')
   CASE ROW=35 .OR. ROW=51
      @ ROW, 93 SAY IIF(XPIKNAME=.T.,HLINE2,'')
   CASE ROW=36 .OR. ROW=52
      @ ROW, 93 SAY IIF(XPIKNAME=.T.,HLINE3,'')
   CASE ROW=37 .OR. ROW=53
      IF LLENDPT .AND. ROW=53 .AND. NEWDOC
         @ 53, 0 SAY MSGL1
      ENDIF
      @ ROW, 93 SAY IIF(XPIKNAME=.T.,HLINE4,'')
   CASE ROW=23 .OR. ROW=40
      @ ROW, 93 SAY XSTNAME
   CASE ROW=24 .OR. ROW=41
      @ ROW, 93 SAY XSTADDR1
   CASE ROW=25 .OR. ROW=42
      @ ROW, 93 SAY XSTADDR2
   CASE ROW=26 .OR. ROW=43
      @ ROW, 93 SAY XSTADDR3
   CASE ROW=30 .OR. ROW=46
      @ ROW, 86 SAY LEFT(LCCUSTPOVL,13)
      @ ROW, 100 SAY XSTORE
      @ ROW, 111 SAY ORDHDR.DEPT
      @ ROW, 120 SAY XORDER
   CASE ROW=31 .OR. ROW=48
      @ ROW, 101 SAY XPIKTKT
      @ ROW, 108 SAY 'SHIP VIA: '+SUBSTR(XSHIP_DATA,1,10)
ENDCASE

*-- End OF Lpsidlabls
*:*************************************************************
*: Name      : Lphrdlabls
*: Developer : Abdou Elgendy [ABD]
*: Date      : 05/18/2003
*: Purpose   : 
*:*************************************************************
*: Called from : ...
*:*************************************************************
*: Calls       : ...
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : Do Lphrdlabls
*:*************************************************************
*:
PROCEDURE Lphrdlabls

@ 2, 1 SAY IIF(XPIKNAME=.T.,HLINE1,'')
@ 2, 95 SAY IIF(XPIKNAME=.T.,HLINE1,'')
@ 3, 1 SAY IIF(XPIKNAME=.T.,HLINE2,'')
@ 3, 95 SAY IIF(XPIKNAME=.T.,HLINE2,'')
@ 4, 1 SAY IIF(XPIKNAME=.T.,HLINE3,'')
@ 4, 95 SAY IIF(XPIKNAME=.T.,HLINE3,'')
@ 5, 1 SAY IIF(XPIKNAME=.T.,HLINE4,'')
@ 5, 61 SAY LEFT(XDIVI_DATA,7)
@ 5, 74 SAY XPIKTKT
@ 5, 95 SAY IIF(XPIKNAME=.T.,HLINE4,'')
@ 7, 95 SAY XSTNAME
@ 8, 61 SAY ORDHDR.APPROVAL
@ 8, 74 SAY XDATE
@ 8, 95 SAY XSTADDR1
@ 9, 95 SAY XSTADDR2
@ 10, 5 SAY XBTNAME
@ 10, 48 SAY XSTNAME
@ 10, 95 SAY XSTADDR3
@ 11, 5 SAY XBTADDR1
@ 11, 48 SAY XSTADDR1
@ 12, 5 SAY XBTADDR2
@ 12, 48 SAY XSTADDR2
@ 13, 5 SAY XBTADDR3
@ 13, 48 SAY XSTADDR3
@ 13, 86 SAY LEFT(LCCUSTPOVL,13)
@ 13, 100 SAY XSTORE
@ 13, 111 SAY ORDHDR.DEPT
@ 13, 120 SAY XORDER
@ 15, 101 SAY XPIKTKT
@ 15, 108 SAY 'SHIP VIA: '+SUBSTR(XSHIP_DATA,1,10)
@ 17, 93 SAY IIF(XPIKNAME=.T.,HLINE1,'')
@ 18, 1 SAY XACCOUNT
@ 18, 7 SAY XORDER
@ 18, 13 SAY ORDHDR.REP1
@ 18, 17 SAY ORDHDR.REP2
@ 18, 21 SAY ORDHDR.ENTERED
@ 18, 30 SAY ORDHDR.START
@ 18, 38 SAY ORDHDR.COMPLETE
@ 18, 46 SAY SUBSTR(XTERM_DATA,1,14)
@ 18, 61 SAY XSTORE
@ 18, 69 SAY LCCUSTPOVL PICTURE '##########'
@ 18, 80 SAY ORDHDR.DEPT
@ 18, 93 SAY IIF(XPIKNAME=.T.,HLINE2,'')
@ 19, 93 SAY IIF(XPIKNAME=.T.,HLINE3,'')
ROW = 20
IF LLSCALE
   DIMENSION LASCALESZ[ 5]
   STORE SPACE(0) TO LASCALESZ
   LNCOUNT = 1
   LCSCALCNT = 0
   LCSELFILE = SELECT(0)
   SELECT ORDLINE
   SEEK ORDHDR.CORDTYPE+XORDER
   LCKEY = CORDTYPE+ORDER+STR(LINENO,6)
   SCAN WHILE XORDER=ORDER .AND. LCSCALCNT<=5
      IF ASCAN(LASCALESZ,SUBSTR(ORDLINE.SCALE,1,2))=0 .AND. SEEK('S'+ORDLINE.SCALE,'SCALE')
         LCSCALCNT = LCSCALCNT+1
         DIMENSION LASCALESZ[ LCSCALCNT]
         LCSCALSIZ = SUBSTR(ORDLINE.SCALE,1,2)+PADL(ALLTRIM(SCALE.SZ1),4,' ')+' '+PADL(ALLTRIM(SCALE.SZ2),4,' ')+PADL(ALLTRIM(SCALE.SZ3),4,' ')+' '+PADL(ALLTRIM(SCALE.SZ4),4,' ')+PADL(ALLTRIM(SCALE.SZ5),4,' ')+' '+PADL(ALLTRIM(SCALE.SZ6),4,' ')+PADL(ALLTRIM(SCALE.SZ7),4,' ')+' '+PADL(ALLTRIM(SCALE.SZ8),4,' ')
         LASCALESZ[ LCSCALCNT] = LCSCALSIZ
      ENDIF
   ENDSCAN
   SEEK (LCKEY)
   FOR LNCOUNT = 1 TO ALEN(LASCALESZ,1)
      @ ROW, 19 SAY LASCALESZ(LNCOUNT)
      IF LNCOUNT=1
         @ ROW, 93 SAY IIF(XPIKNAME=.T.,HLINE4,'')
      ENDIF
      DO LPSIDLABLS
      ROW = ROW+1
   ENDFOR
   LLSCALE = .F.
   SELECT (LCSELFILE)
ELSE
   @ ROW, 93 SAY IIF(XPIKNAME=.T.,HLINE4,'')
ENDIF
ROW = IIF(ROW<23,23,ROW)

*-- End OF Lphrdlabls
*:*************************************************************
*: Name      : Lpoldfoter
*: Developer : Abdou Elgendy [ABD]
*: Date      : 05/18/2003
*: Purpose   : 
*:*************************************************************
*: Called from : ...
*:*************************************************************
*: Calls       : ...
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : Do Lpoldfoter
*:*************************************************************
*:
PROCEDURE Lpoldfoter
@ 55, 93 SAY XSTNAME
@ 56, 93 SAY XSTADDR1
@ 57, 12 SAY '*** CONTINUED NEXT PAGE ***'
@ 57, 93 SAY XSTADDR2
@ 58, 93 SAY XSTADDR3
@ 63, 86 SAY LEFT(LCCUSTPOVL,13)
@ 63, 100 SAY XSTORE
@ 63, 111 SAY ORDHDR.DEPT
@ 63, 120 SAY XORDER
@ 64, 101 SAY XPIKTKT
@ 64, 108 SAY 'SHIP VIA: '+SUBSTR(XSHIP_DATA,1,10)
ROW = ROW+1

*-- End Of Lpoldfoter.
*:*************************************************************
*: Name      : Lpnewfoter
*: Developer : Abdou Elgendy [ABD]
*: Date      : 05/18/2003
*: Purpose   : 
*:*************************************************************
*: Called from : ...
*:*************************************************************
*: Calls       : ...
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : Do Lpnewfoter
*:*************************************************************
*:
PROCEDURE Lpnewfoter
= LFCHKROW('A')
@ 54, 0 SAY MSGL2
@ 55, 0 SAY MSGL3
@ 56, 93 SAY XSTNAME
@ 57, 93 SAY XSTADDR1
= SEEK('O'+XORDER,'ORDHDR')
LNCOL = 11
IF SUBSTR(ORDHDR.NOTE1,1,1)<>'*'
   @ 58, LNCOL SAY ORDHDR.NOTE1
   LNCOL = 42
ENDIF
@ 58, LNCOL SAY IIF(SUBSTR(ORDHDR.NOTE2,1,1)<>'*',SUBSTR(ORDHDR.NOTE2,1,25),'')
@ 58, 76 SAY XPIECES PICTURE '999999'
@ 58, 93 SAY XSTADDR2
@ 59, 93 SAY XSTADDR3
@ 62, 60 SAY SUBSTR(XSHIP_DATA,1,11)
@ 62, 76 SAY IIF(PRTPRICE=.T.,XVALUE,'') PICTURE '999999.99'
@ 63, 86 SAY LEFT(LCCUSTPOVL,13)
@ 63, 100 SAY XSTORE
@ 63, 111 SAY ORDHDR.DEPT
@ 63, 120 SAY XORDER
@ 64, 101 SAY XPIKTKT
@ 64, 108 SAY 'SHIP VIA: '+SUBSTR(XSHIP_DATA,1,10)
XVALUE = 0.00
ROW = ROW+1

*-- End OF Lpnewfoter.
*:*************************************************************
*: Name      : Lpprtsku
*: Developer : Abdou Elgendy [ABD]
*: Date      : 05/18/2003
*: Purpose   : 
*:*************************************************************
*: Called from : ...
*:*************************************************************
*: Calls       : ...
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : Do Lpprtsku
*:*************************************************************
*:
PROCEDURE Lpprtsku
IF  .NOT. SEEK('S'+ORDLINE.ACCOUNT+ORDLINE.STYLE,'Spck_Lin')
   LCSTRTOPRN = ' '
   RETURN
ENDIF
SELECT SPCK_LIN
IF EMPTY(SKU)
   LNI = 1
   = SEEK('S'+STYLE.SCALE,'Scale')
   = SEEK('M'+ORDLINE.ACCOUNT,'Customer')
   LCSKUTMPL = IIF( .NOT. EMPTY(CUSTOMER.SKUTMPL),CUSTOMER.SKUTMPL,'DEF')
   IF SEEK('S'+LCSKUTMPL,'SkuTmpl')
      LNDIME1 = SKUTMPL.LEN1+SKUTMPL.LEN2+SKUTMPL.LEN3
      LNDIME2 = SKUTMPL.LEN4
   ELSE
      LNDIME1 = 8
      LNDIME2 = 8
   ENDIF
   = SEEK('S'+ORDLINE.ACCOUNT+ORDLINE.STYLE,'Spck_Lin')
   LCSTRTOPRN = 'SKU#: '+SUBSTR(PACK_ID,1,LNDIME1)
   LCSTRTOPRN = IIF(LEN(LCSTRTOPRN)>65,SUBSTR(LCSTRTOPRN,1,65),ALLTRIM(LCSTRTOPRN))
   LNLENGTH = LEN(LCSTRTOPRN)+2
   LNLOCCOL = LEN(LCSTRTOPRN)+5
ELSE
   @ ROW, 1 SAY SUBSTR(SKU,1,8)
   LNLOCCOL = 35
   @ ROW, 17 SAY 'CUSTOMER SKU #'
ENDIF
SET DEVICE TO SCREEN
RETURN

*-- End OF Lpprtsku
*:*************************************************************
*: Name      : Lfgetloc
*: Developer : Abdou Elgendy [ABD]
*: Date      : 05/18/2003
*: Purpose   : 
*:*************************************************************
*: Called from : ...
*:*************************************************************
*: Calls       : ...
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : Do Lfgetloc
*:*************************************************************
*:
PROCEDURE Lfgetloc
LCALIAS = ALIAS()
LCSTR = ' '
SELECT WHSLOC
IF SEEK(&lcTmpOrdL..Style)
   SCAN REST WHILE Style = &lcTmpOrdL..Style
      LCSTR = LCSTR+' '+CLOCATION
   ENDSCAN
   LCSTR = ALLTRIM(LCSTR)
   LNLEN = LEN(LCSTR)
ENDIF
SELECT (LCALIAS)

*-- End OF Lfgetloc.
*:*************************************************************
*: Name      : Lfcontuloc
*: Developer : Abdou Elgendy [ABD]
*: Date      : 05/18/2003
*: Purpose   : 
*:*************************************************************
*: Called from : ...
*:*************************************************************
*: Calls       : ...
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : Do Lfcontuloc
*:*************************************************************
*:
PROCEDURE Lfcontuloc
FOR I = 12 TO LNLEN
   @ ROW, 0 SAY SUBSTR(LCSTR,I,86)
   I = I+86
   DO LPSIDLABLS
   ROW = ROW+1
   = LFCHKROW('B')
ENDFOR

*-- End OF Lfcontuloc.
*:*************************************************************
*: Name      : Lfchkrow
*: Developer : Abdou Elgendy [ABD]
*: Date      : 05/18/2003
*: Purpose   : 
*:*************************************************************
*: Called from : ...
*:*************************************************************
*: Calls       : ...
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : Do Lfcontuloc
*:*************************************************************
*:
PROCEDURE Lfchkrow
PARAMETER LCTYPE
IF ROW>=IIF(LCTYPE='A',54,55)
   DO LPOLDFOTER
   DO LPHRDLABLS
ENDIF

*-- End OF Lfchkrow.
*:*************************************************************
*: Name      : Lfprnstrn
*: Developer : Abdou Elgendy [ABD]
*: Date      : 05/18/2003
*: Purpose   : 
*:*************************************************************
*: Called from : ...
*:*************************************************************
*: Calls       : ...
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : Do Lfprnstrn
*:*************************************************************
*:
PROCEDURE Lfprnstrn
PRIVATE LNALIAS
LNALIAS = SELECT(0)
LCSKUSIZE = ''
IF SEEK('S'+ORDLINE.ACCOUNT+ORDLINE.STYLE,'Spck_Lin')
   LCKEY = 'S'+ORDLINE.ACCOUNT+ORDLINE.STYLE
   = SEEK('S'+STYLE.SCALE,'Scale')
   DIMENSION LASKUS[ SCALE.CNT, 2]
   LASKUS = ''
   LNCUSTREC = RECNO('Customer')
   = SEEK('M'+ORDLINE.ACCOUNT,'Customer')
   LCSKUTMPL = IIF( .NOT. EMPTY(CUSTOMER.SKUTMPL),CUSTOMER.SKUTMPL,'DEF')
   IF BETWEEN(LNCUSTREC,1,RECCOUNT('Customer'))
      GOTO IN CUSTOMER LNCUSTREC
   ENDIF
   IF SEEK('S'+LCSKUTMPL,'SkuTmpl')
      LNDIME1 = SKUTMPL.LEN1+SKUTMPL.LEN2+SKUTMPL.LEN3
      LNDIME2 = SKUTMPL.LEN4
   ELSE
      LNDIME1 = 8
      LNDIME2 = 8
   ENDIF
   SELECT SPCK_LIN
   SCAN REST WHILE 'S'+ACCOUNT+STYLE=LCKEY
      FOR LNCOUNTER = 1 TO SCALE.CNT
         LCCOUNTER = STR(LNCOUNTER,1)
         IF Qty&lcCounter > 0
            LASKUS[ LNCOUNTER, 1] = STR(LNCOUNTER,1)
            LASKUS[ LNCOUNTER, 2] = SUBSTR(PACK_ID,LNDIME1+1,5)+' '
            EXIT
         ENDIF
      ENDFOR
   ENDSCAN
   = ASORT(LASKUS,1)
   FOR LNARRAYCNT = 1 TO ALEN(LASKUS,1)
      LCARRAYCNT = ALLTRIM(STR(LNARRAYCNT))
      IF &lcTmpOrdL..Qty&lcArrayCnt > 0
         LCSKUSIZE = LCSKUSIZE+'S'+LCARRAYCNT+':'+ALLTRIM(LASKUS(LNARRAYCNT,2))+' '
      ENDIF
   ENDFOR
   LCSKUSIZE = ALLTRIM(LCSKUSIZE)
ENDIF
SELECT (LNALIAS)
RETURN
*-- End OF Lfprnstrn.
*:***************************************************************************
