*:********************************************************************
*: Program file  : RMJEN100  (REFERENCE, C102063)             
*: Program desc. : DETAILED LISTING OF ALL THE RETURNS (JEN).
*: For screen    : None.
*:         System: ARIA APPAREL SERIES
*:         Module: REPORT
*:      Developer: ABD ELRAIM KOURA . KAM
*!********************************************************************
*!********************************************************************
*: Calls         : PROCEDURE  RA911
*:                 FUNCTION   =lfvFabric
*:                            =lfEvalSegs 
*:                            =lfMajPic      
*:                            =lfSetSty
*:                            =lfvloc               
*:********************************************************************
*: Passed Parameters  :NOME
*:********************************************************************
*: Example            :DO RMJEN100 
*:********************************************************************


*-- Option Grid Layout :
*-- Range of dates returned    : from .. to ..
*-- Range of styles            : subdivided according to style segments
*-- Only this fabric           : ..
*-- Range of accounts          : range of accounts
*-- Print size breakdown       : <Y>es <N>o       default Y
*-- Print the amount           : <Y>es <N>o       default Y
*-- Report                     : <O>pen R/A <R>eceived returns or <B>oth  default R
*-- Only this warehouse        : range of warehouse
*-- Note that the range of styles option is subdivided according to the style code
*-- structure (e.g. if we have style code structure consists of two segments
*-- free and color then the range of styles option in the option grid will appear
*-- as two options one representing the free segment and the other for the color)
*-- llMultWare  : company use multiwarehouse or not
*-- lcRpTitle   : get style header title
*-- XTOT        : array to calculate qtys. , totalqty. , qtys.*price , dmgqtys.*price
*-- lcRpSort    : sort by
*-- lcRpPSize   : print size breakdown
*-- lcRpAmt     : print the amount
*-- lcRpRpt     : either Open R/A , Received returns or Both
*-- NULDATE     : check entered null date.
 
*-- set relations


SELE RALINE
SET RELATION TO RANO INTO RETAUTH, STYLE INTO STYLE  ADDITIVE
SELE RETLINE
SET RELATION TO CRMEMO INTO RETHDR, STYLE INTO STYLE ADDITIVE

*-- lcRpExp2 will be applied in case of xreport $ 'OB'
*-- lcRpExp  will be applied in case of xreport $ 'OR'
**** GENERAL VARIABLES INITIALIZATION ******
*// llMultiWH hold company status multiware or not 
*// CHOICE    hold sort field
*// SIZEB     hold print size or not [Y or N]
*// XAMOUNT   hold print amount or not [Y or N] 
*// XREPORT   hold received or open or both
*// lcWarec   hold cwarecode
*// lDate     hold low date
*// hDate     hold high date
*// lnFirstOcc hold position of "|' in date


lnFirstOcc    =AT("|",laOgFxflt[3,6])

IF lnFirstOcc>0
  lDate       =CTOD(substr(laOgFxflt[3,6],1,lnFirstOcc-1))
  hDate       =CTOD(substr(laOgFxflt[3,6],lnFirstOcc+1))
ELSE
  lDATE       = CTOD('  /  /  ')
  hDATE       = CTOD('  /  /  ')
ENDIF  
llMultiWH     = llMultWare
CHOICE        = lcRpSort
SIZEB         = lcRpPSize
XAMOUNT       = lcRpAmt
XREPORT       = lcRpRpt
lcWareC       = laOgFxflt[4,6]
NULDATE       = CTOD('  /  /  ')
XTITLE        = lcRpTitle
DIMENSION XTOT(2,12)
STORE 0 TO XTOT

*-- filter adjustment
*-- XREPORT  : 'O' means open R/A
*-- XREPORT  : 'R' means recieved returns
*-- XREPORT  : 'B' means both
*-- lcRpExp   : report expression in case of XREPORT = 'R' means received returns
*-- lcRpExp2  : report expression in case of XREPORT = 'O' means open R/A or both
*-- if nothing is seleted then lcRpExp will be .T.
lcRpExp2  = lcRpExp
*-- if selected open R/A or both
IF XREPORT $ 'OB'

  lcRpExp2 = STRTRAN(lcRpExp2,'RETHDR.CWARECODE','RETAUTH.CWARECODE')
  lcRpExp2 = STRTRAN(lcRpExp2,'RETHDR.CRDATE','RETAUTH.RaDate')  
  lcRpExp2 = STRTRAN(lcRpExp2,'RETLINE.ACCOUNT+RETLINE.INVOICE+RETLINE.STYLE+RETLINE.CRMEMO'     ,'RALINE.ACCOUNT') 

  lcRpExp2 = lcRpExp2 + " AND RETAUTH->STATUS = 'O'"
ELSE
  lcRpExp = lcRpExp+  " AND RETHDR->STATUS <> 'X' " 
ENDIF  && end if selected open R/A or both

*-- SELECT SORT SEQUENCE
SORTFIELD = ' '
BREAK     = ' '
XSORT     = CHOICE
XSW       = .T.        && Switch for print subtotal name by acct. or style
DO CASE
  CASE XREPORT = 'O'
    R_TITLE  = 'DETAILED LISTING OF OPEN R/A'
    XNO = 'RANO'
  CASE XREPORT = 'R'
    R_TITLE  = 'DETAILED LISTING OF RETURNS'
    XNO = 'CRMEMO'
  CASE XREPORT = 'B'
    R_TITLE  = 'DETAILED LISTING OF OPEN R/A AND RETURNS'
ENDCASE

DO CASE
  CASE CHOICE = 'C'
            
  CASE CHOICE = 'S'
    SORTFIELD = 'STYLE+ &XNO'
    BREAK     = 'STYLE'
    XSORT     = ' - SORT BY STYLE'
    XTOT_NAME = 'S'
  CASE CHOICE = 'A'
    SORTFIELD = 'ACCOUNT + STYLE + &XNO'
    BREAK     = 'ACCOUNT'
    XSORT     = ' - SORT BY ACCOUNT'
    XTOT_NAME = 'A'
  CASE CHOICE = 'R'
    SORTFIELD = 'REASON + STYLE +  &XNO'
    BREAK     = 'REASON'
    XSORT     = ' - SORT BY REASON'
    XTOT_NAME = 'R'
  CASE CHOICE = 'L'
    SORTFIELD = 'REP + ACCOUNT + &XNO'
    BREAK     = 'REP'
    XSORT     = ' - SORT BY SALES REP'
    XTOT_NAME = 'L'
ENDCASE
  
**********************************************************
****** SELECT REPORT FILE 
**********************************************************

XBOTH_FLG = .F.
XNAME     = ' '
XSUB_TOT  = 'SUB_TOTAL:'
DO CASE
  CASE XREPORT = 'O'
    SELECT RALINE
    LOCATE ALL FOR &lcRpExp2
    IF EOF()
      WAIT 'No records have been selected for the report...' WINDOW  NOWAIT
      RETURN
    ELSE
      XFILE  = 'RALINE'
      FILTER = lcRpExp2
      XNAME  = 'RALTEMP'
      DMGQTY = 0
      XRECNO = RECNO()
     ENDIF
  CASE XREPORT = 'R'
    SELE RETLINE
    LOCATE ALL FOR &lcRpExp
    IF EOF()
      WAIT'No records have been selected for the report...' WINDOW NOWAIT
      RETURN
    ELSE
      FILTER    = lcRpExp
      XFILE     = 'RETLINE'
      XNAME     = 'RETLTEMP'
      XRECNO    = RECNO()
    ENDIF
  CASE XREPORT = 'B'
    DIMENSION XTOT_B(2,12)
    STORE 0 TO XTOT_B
    SELECT RALINE
    LOCATE ALL FOR &lcRpExp2 
    IF FOUND()
      XNAME   = 'RALTEMP'
      XRAL    = RECNO()
    ENDIF
    SELE RETLINE
    LOCATE ALL FOR &lcRpExp
    IF FOUND()
      XNAME1 = 'RETLTEMP'
      XREL   = RECNO()
    ENDIF
    DO CASE
      CASE !FOUND('RALINE') .AND. !FOUND('RETLINE')
        WAIT'No records have been selected for the report...' WINDOW NOWAIT
        RETURN
      CASE FOUND('RALINE') .AND. FOUND('RETLINE')
        XBOTH_FLG = .T.
        XNO      = 'TYPE+RANO+CRMEMO'
    ENDCASE
ENDCASE

IF !XBOTH_FLG
  *// case xrepport='o' or 'r' or both but not found in one or two files
  IF XREPORT = 'B'
    XFILE = IIF(EMPTY(XNAME),'RETLINE','RALINE')
    FILTER= IIF(EMPTY(XNAME),lcRpExp,lcRpExp2)
    XNO   = IIF(EMPTY(XNAME),'CRMEMO','RANO')
    DMGQTY= IIF(EMPTY(XNAME),DMGQTY,0)
    XTYPE = IIF(EMPTY(XNAME),'RCV','R/A')
    XRECNO= IIF(EMPTY(XNAME),XREL,XRAL)
  ENDIF

  XNAME  = gfTempName()  &&global function gives temporarily name for the workfile.
  SELECT &XFILE
  GOTO XRECNO
  COPY TO (gcWorkDir+XNAME) FOR &FILTER    && appends all needed records.
  =gfOpenFile(gcWorkDir+XNAME,' ','EX') &&global function opens DBF (with-optionally- its index) share or exculsive.
  XFILE = IIF(XFILE='RALINE','RETAUTH','RETHDR')
  SET RELATION TO IIF(EMPTY(&XFILE->STORE),'M'+ACCOUNT,'S'+ACCOUNT+&XFILE->STORE) INTO CUSTOMER
ELSE  && !xbothflg
  *// case xreport='b' and found in credit memo file and returnauth file
  
  CREATE CURSOR (XNAME) ;
  (STYLE  C(19), ACCOUNT C(05), TYPE C(01), DYELOT C(10), RANO C(06), CRMEMO C(06) ,;
  REASON C(06), PRICE N(12,2), QTY1    N(05), QTY2 N(05), QTY3 N(05), QTY4   N(05), QTY5 N(05) ,;
  QTY6   N(05), QTY7  N(05) , QTY8    N(05), TOTQTY N(10), DMGQTY N(06), AMOUNT N(13,2), NEWFIELD C(10)) 
  XTEMP = gfTempName()    &&global function gives temporarily name for the workfile.
  SELECT RALINE
  GOTO XRAL
  COPY TO (gcWorkDir+XTEMP) FOR &lcRpExp2
  SELECT &XNAME
  APPEND FROM (gcWorkDir+XTEMP)
  SELECT RETLINE
  GOTO XREL
  COPY TO (gcWorkDir+XTEMP) FOR &lcRpExp
  SELECT &XNAME
  APPEND FROM (gcWorkDir+XTEMP)
  GO TOP
  REPLACE ALL TYPE WITH '1' FOR !EMPTY(CRMEMO)
  XFILE = IIF(EMPTY(TYPE),'RETAUTH','RETHDR')
  SET RELATION TO IIF(EMPTY(&XFILE->STORE),'M'+ACCOUNT,'S'+ACCOUNT+&XFILE->STORE) INTO CUSTOMER

ENDIF &&!bothflg

GO TOP
REPLACE ALL TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
****** Sort to Workfile Index ******



IF XTOT_NAME ='L'
  SELECT *,SPACE(3) AS Rep ;
  FROM &XNAME ;
  INTO TABLE (gcWorkDir+'TMP')
  SELECT &XNAME
  USE
  ERASE (gcWorkDir+XNAME+'.DBF')
  SELECT TMP
  USE 
  RENAME (gcWorkDir+'TMP') TO (gcWorkDir+XNAME)
  SELECT 0
  USE (gcWorkDir+XNAME)
  Lcfile =' '
  LLBoth =.F.
  SELECT (XNAME)  
  DO CASE
    
    CASE XREPORT ='O' 
      Lcfile = 'RETAUTH'
      SET RELATION TO Rano INTO RETAUTH
      SET RELATION TO IIF(EMPTY(RETAUTH.STORE),'M'+ACCOUNT,'S'+ACCOUNT+RETAUTH.STORE) INTO CUSTOMER ADDITIVE
    
    CASE XREPORT ='R'
      Lcfile ='RETHDR'
      SET RELATION TO Crmemo INTO RETHDR
      SET RELATION TO IIF(EMPTY(RETHDR.STORE),'M'+ACCOUNT,'S'+ACCOUNT+RETHDR.STORE) INTO CUSTOMER ADDITIVE
    
    CASE XREPORT ='B'  
    
        
      LLBoth=.T.
      SET RELATION TO IIF(EMPTY(RETAUTH.Rano), RETHDR.Crmemo  ,Rano) INTO IIF(EMPTY(RETAUTH.Rano),'RETHDR','RETAUTH') ADDITIVE
      SET RELATION TO IIF(EMPTY(RETAUTH.Rano),IIF(EMPTY(RETHDR.STORE),'M'+ACCOUNT,;
      'S'+ACCOUNT+RETHDR.STORE),;
      IIF(EMPTY(RETAUTH.STORE),'M'+ACCOUNT,'S'+ACCOUNT+RETAUTH.STORE)) INTO CUSTOMER ADDITIVE
  ENDCASE
  SELECT RETAUTH
  SET RELATION TO Invoice INTO INVHDR ADDITIVE
  SELECT RETHDR
  SET RELATION TO Invoice INTO INVHDR ADDITIVE
  SELECT &XNAME
  GO TOP
  SCAN WHILE !EOF()
    IF LLBoth
      Lcfile = IIF( EMPTY(RETAUTH.Rano),'RETHDR','RETAUTH')
    ENDIF
    IF EMPTY(&Lcfile..invoice)         
      REPLACE Rep WITH CUSTOMER.salesrep 
    ELSE
      REPLACE Rep WITH INVHDR.rep1
    ENDIF
  ENDSCAN
  LnRec=RECCOUNT()
  LnNo = 1
  DO WHILE LnNo <= LnRec
    GO LnNo 
    IF LLBoth
      Lcfile = IIF(EMPTY(RETAUTH.Rano),'RETHDR','RETAUTH')
    ENDIF 
    IF EMPTY(&Lcfile..invoice)         
      IF !EMPTY(CUSTOMER.rep2)  
        SCATTER MEMVAR
        APPEND BLANK
        GATHER MEMVAR
        GO RECNO()
        REPLACE Rep WITH CUSTOMER.rep2 
      ENDIF
    ELSE
      IF !EMPTY(INVHDR.rep2)  
        SCATTER MEMVAR
        APPEND BLANK
        GATHER MEMVAR
        GO RECNO()
        REPLACE Rep WITH INVHDR.rep2
      ENDIF
    ENDIF
    LnNo=LnNo+1
  ENDDO  
ENDIF

lcRecNo = STR(RECCOUNT(),7)
WAIT 'Sorting &lcRecNo records for report ' WINDOW NOWAIT
SELECT &XNAME
INDEX ON &SORTFIELD TAG &XNAME
SET ORDER TO TAG &XNAME
GO TOP

SELECT &XNAME
IF LEN(TRIM(BREAK))<>0
   HBREAK=&BREAK
ENDIF

IF EMPTY(XTITLE) AND !EMPTY(LDATE)
  
  XTITLE= 'FROM: '+DTOC(LDATE)+' THRU: '+DTOC(HDATE)
ENDIF

*//Xcustomer hold customer name 
XCUSTOMER = CUSTOMER->STNAME


R_TITLE  = ALLTRIM(R_TITLE)+XSORT+IIF(llMultiWH .AND. !EMPTY(lcWareC),' FOR WARHOUSE: '+lcWareC,'')
XSUB     = .T.
ROW      = 99
PAGENO   = 0

R_WIDTH='W'
R_LEN=132

X_STYLE  =SPACE(19)




*0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
*STYLE        COLOR  ACCT# TYP CR/RA  REASON         STK.QTY  STK.AMOUNT  DMG.QTY    DMG.AMT
*123456789012 123456 12345 XXX 123456 12 1234567890  1234567  1234567.99  1234567  123456.99



*0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*
*STYLE        COLOR  ACCT# TYP CR/RA  REASON         QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY   STK.AMT
*123456789012 123456 12345 R/A 123456 12 1234567890 XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX 123456 1234567.99
*                          R/A 123456 12 1234567890 XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX 123456 1234567.99
*                          R/A 123456 12 1234567890 XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX 123456 1234567.99 
*                          -------------------------------------------------------------------------------------------
*                          RCV 123456 12 1234567890 XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX 123456 1234567.99 
*                          RCV 123456 12 1234567890 XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX 123456 1234567.99 
*                          -------------------------------------------------------------------------------------------
*   SUBTOTALS              R/A                       99999 99999 99999 99999......
*                          RCV                       99999 99999 99999 99999.....
*   GRAND TOTAL            R/A                       99999 99999 99999 99999......
*                          RCV                       99999 99999 99999 99999......
*
****** Begin Printing ******


WAIT 'Report Printing Press <SPACE BAR > to ABORT ..' WINDOW NOWAIT
SET DEVICE TO PRINT
SELE &XNAME
GO TOP
*----------------------------
* BEGIN [MAIN REPORT] LOOP
*----------------------------
LLFlag = .T.
DO WHILE .T.

  
  IF ROW > 53
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH 'RA910',XTITLE,R_WIDTH
    DO CASE
      CASE XREPORT = 'R'
        IF SIZEB = 'Y'
          @ 05,00 SAY 'STYLE        COLOR  ACCOUNT#  CRMEMO REASON                      QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY    STK.AMT ' 
        ELSE
          @ 05,00 SAY 'STYLE        COLOR  ACCT# TYP CR/RA  REASON                   STK.QTY  STK.AMOUNT  '          
        ENDIF
      CASE XREPORT = 'O'
        IF SIZEB = 'Y'
          @ 05,00 SAY 'STYLE        COLOR  ACCOUNT#  RANO   REASON                      QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY    STK.AMT '          
        ELSE
          @ 05,00 SAY 'STYLE        COLOR  ACCOUNT#  RANO   REASON                   STK.QTY  STK.AMOUNT  '
        ENDIF
      CASE XREPORT = 'B'
        IF SIZEB = 'Y'
          @ 05,00 SAY 'STYLE        COLOR  ACCT# TYP CR/RA  REASON                      QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY    STK.AMT '
        ELSE
          @ 05,00 SAY 'STYLE        COLOR  ACCT# TYP CR/RA  REASON                   STK.QTY  STK.AMOUNT  '
        ENDIF
    ENDCASE
    ***
    @ 06,00 SAY REPLICATE('=',R_LEN)
    ROW = 7
  ENDIF
  IF XTOT_NAME = 'L' AND LLflag
    @ ROW,00 SAY '**SALESREP : ' + IIF(SEEK(HBREAK,"SALESREP"),SALESREP.name,' ')    
    ROW =ROW + 1
    LLflag = .F.
  ENDIF
  IF HBREAK <> &BREAK

    @ ROW,00 SAY REPLICATE('-',R_LEN)
    ROW = ROW + 1
    @ ROW,00 SAY XSUB_TOT
    DO CASE
      CASE XTOT_NAME = 'L'
        @ ROW,12 SAY IIF(SEEK(HBREAK,"SALESREP"),SUBSTR(SALESREP.name,1,18),' ')
      CASE XTOT_NAME = 'S'
        @ ROW,12 SAY SUBSTR(HBREAK,1,12)+'  '+SUBSTR(HBREAK,14,7)
      CASE XTOT_NAME = 'A'
        @ ROW,12 SAY SUBSTR(XCUSTOMER,1,18)
      CASE XTOT_NAME = 'R'
        @ ROW,12 SAY  SUBSTR(gfCodDes(XREASON,'REASON'),1,18)
    ENDCASE
    ***
    IF XBOTH_FLG
      @ ROW,37 SAY 'R/A :'
    ENDIF

    SELECT &XNAME
    IF SIZEB = 'Y'
      @ ROW,064 SAY XTOT(1,1) PICTURE '99999'
      @ ROW,070 SAY XTOT(1,2) PICTURE '99999'
      @ ROW,076 SAY XTOT(1,3) PICTURE '99999'
      @ ROW,082 SAY XTOT(1,4) PICTURE '99999'
      @ ROW,088 SAY XTOT(1,5) PICTURE '99999'
      @ ROW,094 SAY XTOT(1,6) PICTURE '99999'
      @ ROW,100 SAY XTOT(1,7) PICTURE '99999'
      @ ROW,106 SAY XTOT(1,8) PICTURE '99999'
      @ ROW,112 SAY XTOT(1,9) PICTURE '9999999'
      IF XAMOUNT = 'Y'
        @ ROW,120 SAY XTOT(1,10) PICTURE '9999999.99'
      ENDIF
      STORE 0 TO XTOT(1,1),XTOT(1,2),XTOT(1,3),XTOT(1,4)
      STORE 0 TO XTOT(1,5),XTOT(1,6),XTOT(1,7),XTOT(1,8)
      STORE 0 TO XTOT(1,9),XTOT(1,10),XTOT(1,11),XTOT(1,12)

      IF XBOTH_FLG
        ROW = ROW + 1
        @ ROW,37 SAY 'RCV :'
        @ ROW,064 SAY XTOT_B(1,1) PICTURE '99999'
        @ ROW,070 SAY XTOT_B(1,2) PICTURE '99999'
        @ ROW,076 SAY XTOT_B(1,3) PICTURE '99999'
        @ ROW,082 SAY XTOT_B(1,4) PICTURE '99999'
        @ ROW,088 SAY XTOT_B(1,5) PICTURE '99999'
        @ ROW,094 SAY XTOT_B(1,6) PICTURE '99999'
        @ ROW,100 SAY XTOT_B(1,7) PICTURE '99999'
        @ ROW,106 SAY XTOT_B(1,8) PICTURE '99999'
        @ ROW,112 SAY XTOT_B(1,9) PICTURE '9999999'
        IF XAMOUNT = 'Y'
          @ ROW,120 SAY XTOT_B(1,10) PICTURE '9999999.99'
        ENDIF
        STORE 0 TO XTOT_B(1,1),XTOT_B(1,2),XTOT_B(1,3),XTOT_B(1,4)
        STORE 0 TO XTOT_B(1,5),XTOT_B(1,6),XTOT_B(1,7),XTOT_B(1,8)
        STORE 0 TO XTOT_B(1,9),XTOT_B(1,10),XTOT_B(1,11),XTOT_B(1,12)
      ENDIF
    ELSE
      @ ROW,62 SAY XTOT(1,1) PICTURE '9999999'
      IF XAMOUNT= 'Y'
        @ ROW,69 SAY XTOT(1,2) PICTURE '9999999.99'
      ENDIF
      STORE 0 TO XTOT(1,1),XTOT(1,2),XTOT(1,3),XTOT(1,4)
      IF XBOTH_FLG
        ROW = ROW + 1
        @ ROW,37 SAY 'RCV :'
        @ ROW,62 SAY XTOT_B(1,1) PICTURE '9999999'
        IF XAMOUNT= 'Y'
          @ ROW,69 SAY XTOT_B(1,2) PICTURE '9999999.99'
        ENDIF
        STORE 0 TO XTOT_B(1,1),XTOT_B(1,2),XTOT_B(1,3),XTOT_B(1,4)
      ENDIF
    ENDIF

    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('-',R_LEN)
    ROW = ROW + 1
    SELECT &XNAME
    HBREAK=&BREAK
    
    IF XTOT_NAME ='L'AND !EOF()
      @ ROW,00 SAY '**SALESREP : ' + IIF(SEEK(HBREAK,"SALESREP"),SALESREP.name,' ')
      ROW = ROW + 1
    ENDIF 
    
    XCUSTOMER = CUSTOMER->STNAME

  ENDIF

  IF INKEY() = 32 .OR. EOF()
    EXIT
  ENDIF
  IF XBOTH_FLG
    IF EMPTY(TYPE)
      XTYPE = 'R/A'
      XSUB  = .T.
      XNO   = 'RANO'
      
    ELSE
      XTYPE = 'RCV'
      XNO   = 'CRMEMO'
      XSUB  = .F.
      
    ENDIF
  ENDIF
  
  IF X_STYLE <> STYLE
    @ ROW,00 SAY SUBSTR(STYLE,1,12) PICT '!!!!!!!!!!!!'
    @ ROW,13 SAY SUBSTR(style,14) PICT'!!!!!!'
    X_STYLE  = STYLE
  ENDIF
  @ ROW,20 SAY ACCOUNT
  IF XREPORT = 'B'
    @ ROW,26 SAY XTYPE
  ENDIF
  @ ROW,30 SAY &XNO
  @ ROW,37 SAY REASON
  XREASON = REASON
  XRETU_DATA  = gfCodDes(XREASON,'REASON')
  SELE &XNAME
  @ ROW,45 SAY SUBSTR(XRETU_DATA,1,17)

  IF SIZEB = 'Y'
    
    @ ROW,064 SAY QTY1 PICTURE '99999'
    @ ROW,070 SAY QTY2 PICTURE '99999'
    @ ROW,076 SAY QTY3 PICTURE '99999'
    @ ROW,082 SAY QTY4 PICTURE '99999'
    @ ROW,088 SAY QTY5 PICTURE '99999'
    @ ROW,094 SAY QTY6 PICTURE '99999'
    @ ROW,100 SAY QTY7 PICTURE '99999'
    @ ROW,106 SAY QTY8 PICTURE '99999'
    @ ROW,112 SAY TOTQTY PICTURE '9999999' 

    IF XAMOUNT = 'Y'
      @ ROW,120 SAY TOTQTY*PRICE PICTURE '9999999.99'
    ENDIF
    ***
    IF XSUB
      XTOT(1,1)  = XTOT(1,1)  + QTY1
      XTOT(1,2)  = XTOT(1,2)  + QTY2
      XTOT(1,3)  = XTOT(1,3)  + QTY3
      XTOT(1,4)  = XTOT(1,4)  + QTY4
      XTOT(1,5)  = XTOT(1,5)  + QTY5
      XTOT(1,6)  = XTOT(1,6)  + QTY6
      XTOT(1,7)  = XTOT(1,7)  + QTY7
      XTOT(1,8)  = XTOT(1,8)  + QTY8
      XTOT(1,9)  = XTOT(1,9)  + TOTQTY

      IF XAMOUNT = 'Y'
        XTOT(1,10) = XTOT(1,10) + TOTQTY * PRICE
      ENDIF

      XTOT(2,1)  = XTOT(2,1)  + QTY1
      XTOT(2,2)  = XTOT(2,2)  + QTY2
      XTOT(2,3)  = XTOT(2,3)  + QTY3
      XTOT(2,4)  = XTOT(2,4)  + QTY4
      XTOT(2,5)  = XTOT(2,5)  + QTY5
      XTOT(2,6)  = XTOT(2,6)  + QTY6
      XTOT(2,7)  = XTOT(2,7)  + QTY7
      XTOT(2,8)  = XTOT(2,8)  + QTY8
      XTOT(2,9)  = XTOT(2,9)  + TOTQTY

      IF XAMOUNT = 'Y'
        XTOT(2,10) = XTOT(2,10) + TOTQTY * PRICE
      ENDIF
    ENDIF

    IF XBOTH_FLG
      IF !EMPTY(TYPE)
        XTOT_B(1,1)  = XTOT_B(1,1)  + QTY1
        XTOT_B(1,2)  = XTOT_B(1,2)  + QTY2
        XTOT_B(1,3)  = XTOT_B(1,3)  + QTY3
        XTOT_B(1,4)  = XTOT_B(1,4)  + QTY4
        XTOT_B(1,5)  = XTOT_B(1,5)  + QTY5
        XTOT_B(1,6)  = XTOT_B(1,6)  + QTY6
        XTOT_B(1,7)  = XTOT_B(1,7)  + QTY7
        XTOT_B(1,8)  = XTOT_B(1,8)  + QTY8
        XTOT_B(1,9)  = XTOT_B(1,9)  + TOTQTY

        IF XAMOUNT = 'Y'
          XTOT_B(1,10) = XTOT_B(1,10) + TOTQTY * PRICE
        ENDIF
        XTOT_B(2,1)  = XTOT_B(2,1)  + QTY1
        XTOT_B(2,2)  = XTOT_B(2,2)  + QTY2
        XTOT_B(2,3)  = XTOT_B(2,3)  + QTY3
        XTOT_B(2,4)  = XTOT_B(2,4)  + QTY4
        XTOT_B(2,5)  = XTOT_B(2,5)  + QTY5
        XTOT_B(2,6)  = XTOT_B(2,6)  + QTY6
        XTOT_B(2,7)  = XTOT_B(2,7)  + QTY7
        XTOT_B(2,8)  = XTOT_B(2,8)  + QTY8
        XTOT_B(2,9)  = XTOT_B(2,9)  + TOTQTY

        IF XAMOUNT = 'Y'
          XTOT_B(2,10) = XTOT_B(2,10) + TOTQTY * PRICE
        ENDIF
      ENDIF
    ENDIF
  ELSE &&sizeb no
    @ ROW,52 SAY TOTQTY PICTURE '9999999'
    IF XAMOUNT = 'Y'
      @ ROW,60 SAY TOTQTY*PRICE PICTURE '9999999.99'
    ENDIF
    IF XSUB
      XTOT(1,1) = XTOT(1,1) + TOTQTY
      XTOT(2,1) = XTOT(2,1) + TOTQTY
      IF XAMOUNT = 'Y'
        XTOT(1,2) = XTOT(1,2) + TOTQTY * PRICE
        XTOT(2,2) = XTOT(2,2) + TOTQTY * PRICE
      ENDIF
    ENDIF
    IF XBOTH_FLG
      IF !EMPTY(TYPE)
        XTOT_B(1,1) = XTOT_B(1,1) + TOTQTY
        XTOT_B(2,1) = XTOT_B(2,1) + TOTQTY
        IF XAMOUNT = 'Y'
          XTOT_B(1,2) = XTOT_B(1,2) + TOTQTY * PRICE
          XTOT_B(2,2) = XTOT_B(2,2) + TOTQTY * PRICE
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  ROW = ROW + 1
  IF ROW >53 
    @ ROW,00 SAY REPLICATE('=',R_LEN) 
  ENDIF
  SELECT(XNAME)  
  SKIP
ENDDO
 IF XTOT_NAME != 'L'
   DO RA911
 ENDIF 
DO ENDREPORT
SELECT (XNAME)
USE
ERASE (gcWorkDir+XNAME+'.DBF')
ERASE (gcWorkDir+XNAME+'.CDX')
SET DEVICE TO SCREEN

****************************************************************************
* PROC:RA911                                 VERSION AAS 2.5 03/01/94
* DESC:PRINT THE GRAND TOTAL
* DATE:05/12/2000
* AUTH:ABD ELRAHIM KOURA -KAM
*  -------------------------------------------------------------------
*       03/01/94 MFM
****************************************************************************
PROCEDURE RA911

@ ROW,00 SAY REPLICATE('=',R_LEN)
ROW = ROW + 1
@ ROW,10 SAY 'GRAND TOTAL=>'
IF XBOTH_FLG
  @ ROW,37 SAY 'R/A :'
ENDIF
IF SIZEB = 'Y'
  @ ROW,064 SAY XTOT(2,1) PICTURE '99999'
  @ ROW,070 SAY XTOT(2,2) PICTURE '99999'
  @ ROW,076 SAY XTOT(2,3) PICTURE '99999'
  @ ROW,082 SAY XTOT(2,4) PICTURE '99999'
  @ ROW,088 SAY XTOT(2,5) PICTURE '99999'
  @ ROW,094 SAY XTOT(2,6) PICTURE '99999'
  @ ROW,100 SAY XTOT(2,7) PICTURE '99999'
  @ ROW,106 SAY XTOT(2,8) PICTURE '99999'
  @ ROW,112 SAY XTOT(2,9) PICTURE '9999999'
  IF XAMOUNT = 'Y'
    @ ROW,120 SAY XTOT(2,10) PICTURE '9999999.99'
  ENDIF
  IF XBOTH_FLG
    ROW = ROW +1
    @ ROW,37 SAY 'RCV :'
    @ ROW,064 SAY XTOT_B(2,1) PICTURE '99999'
    @ ROW,070 SAY XTOT_B(2,2) PICTURE '99999'
    @ ROW,076 SAY XTOT_B(2,3) PICTURE '99999'
    @ ROW,082 SAY XTOT_B(2,4) PICTURE '99999'
    @ ROW,088 SAY XTOT_B(2,5) PICTURE '99999'
    @ ROW,094 SAY XTOT_B(2,6) PICTURE '99999'
    @ ROW,100 SAY XTOT_B(2,7) PICTURE '99999'
    @ ROW,106 SAY XTOT_B(2,8) PICTURE '99999'
    @ ROW,112 SAY XTOT_B(2,9) PICTURE '9999999'
    IF XAMOUNT = 'Y'
      @ ROW,120 SAY XTOT_B(2,10) PICTURE '9999999.99'
    ENDIF
  ENDIF
ELSE && if sizeb='y'
  @ ROW,62 SAY XTOT(2,1) PICTURE '9999999'
  IF XAMOUNT = 'Y'
    @ ROW,69 SAY XTOT(2,2) PICTURE '9999999.99'
  ENDIF
  IF XBOTH_FLG
    ROW = ROW + 1
    @ ROW,37 SAY 'RCV :'
    @ ROW,62 SAY XTOT_B(2,1) PICTURE '9999999'
    IF XAMOUNT = 'Y'
      @ ROW,69 SAY XTOT_B(2,2) PICTURE '9999999.99'
    ENDIF
  ENDIF
ENDIF
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('=',R_LEN)
RETURN

*!*************************************************************
*! Name      : lfvFabric
*! Developer : ABD ELRAHIM KOURA (KAM) 
*! Date      : 05/12/2000
*! Purpose   : validate fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFabric()
*!*************************************************************
FUNCTION lfvFabric
lcFabObj = VARREAD()
lcFab    = &lcFabObj
*--logical variable if True we close all files open in this function
llUseByMe = .F.

IF !USED('FABRIC')
  llUseByMe = .T.
  USE (gcDataDir+'FABRIC') IN 0 SHARE
ENDIF
*--lcTag hold the order of fabric file
lcTag = ORDER('FABRIC')
SET ORDER TO FABRIC IN FABRIC
IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(lcFab,'FABRIC') 
    &lcFabObj = FABRIC.Fabric
  ELSE
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF
ELSE
 * &lcFabObj = ''
ENDIF
SET ORDER TO FABRIC IN FABRIC
IF llUseByMe
  USE IN FABRIC
ENDIF  
*:***************************************************************************
*: Name        : lfEvalSegs
*: Developer   : ABD ELRAHIM KOURA (KAM)
*: Date        : 05/12/2000
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

*-- end of lfEvalSegs.


*!*************************************************************
*! Name      : lfMajPic
*! Developer : ABD ELRAHIM KOURA (KAM)
*! Date      : 05/12/2000
*! Purpose   : get major segment picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************

FUNCTION lfMajPic
*--lcmajpic var hold the picture of major part of style
lcMajPic = "@! " + gfItemMask("PM")
RETURN lcMajPic

*!*************************************************************
*! Name      : lfSetSty
*! Developer : ABD ELRAHIM KOURA (KAM)
*! Date      : 05/12/2000
*! Purpose   : To set or reset the Style browse
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : OpGrdParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfSetSty ()
*!*************************************************************
FUNCTION lfSetSty
PARAMETERS OpGrdParm
*--depend on the value of parameter we select the order of index
DO CASE
   CASE OpGrdParm = 'S'
      SET ORDER TO TAG CSTYLE IN STYLE
      GOTO TOP IN Style
   CASE OpGrdParm = 'R'
      SET ORDER TO TAG STYLE IN STYLE
ENDCASE
*!*************************************************************
*! Name      : lfvloc
*! Developer : Abd elrahim koura-kam
*! Date      : 06/12/2000
*! Purpose   : Validation function for the Location field
*!*************************************************************
*! Called from : Location field [Option Grid]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvloc

PRIVATE lcObjName , lcObjVal , llObjRet
lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*-- IF The user want to Browse or if the Location he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*-- End of lfvloc.
***********************************************************************************
*!*************************************************************
*! Name      : lfSetAcc
*! Developer : ABD ELRAHIM KOURA (KAM)
*! Date      : 06/12/2000
*! Purpose   : To set or reset the account browse
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : OpGrdParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfSetAcc ()
*!*************************************************************
FUNCTION lfSetAcc
PARAMETERS OpGrdParm
PRIVATE lcOldSelect

*--depend on the value of parameter we select the order of index
lcOldSelect=SELECT()
DO CASE
   CASE OpGrdParm = 'S'
     GOTO TOP IN CUSTOMER
   CASE OpGrdParm = 'R'
      
ENDCASE
SELECT(lcOldSelect)
*************************************************************

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Abd Elrahim Koura-kam
*! Date      : 12/18/2000
*! Purpose   : Report When Function
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
PRIVATE lnTranDate

lnTranDate = lfItmPos('RETHDR.CRDATE')   && get Tran. Date Fixed filter Position

*-- If Trans. Date found in Fixed filter option
IF lnTranDate > 0
  *-- Replace TO DATE in Transaction date with Current Date (System Date)
  laOGFxFlt[lnTranDate,6] = DTOC(gdSysDate)+ '|' + DTOC(gdSysDate)
  
ENDIF
*-- Endif of Trans. Date found in Fixed filter option 

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Abd Elrahim Koura-kam
*! Date      : 12/18/1999
*! Purpose   : To get the position of the fixed filter in OG
*!**************************************************************************
*! Called from : OG When Function 
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*!*************************************************************
*! Name      : lfOldValue
*! Developer : ABD ELRAHIM KOURA-kam.
*! Date      : 04/04/98
*! Purpose   : Function to store old value of the current filed.
*!*************************************************************
FUNCTION lfOldValue
lcOldValue = EVALUATE(SYS(18))
RETURN
    







