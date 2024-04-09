*********************************************************
*  I M P O R T I N G 
*********************************************************

lcCurPath = FULLPATH('')
SET DEFA TO (ALLT(SETUP.cImpPath))

*TTTT
*lnTmpImpnum = ADIR(LaTMImpFL, '*.TXT')  && Create FILES array
lnTmpImpnum = ADIR(LaTMImpFL, 'UPLOAD*.*')  && Create FILES array
lnImpnumber = 0
lnNewLstSq  = 0

DIME LaImpFiles[1]
LaImpFiles = ""
lnY = 1

IF lnTmpImpnum <> 0
  FOR lnI=1 TO ALEN(LaTMImpFL,1)
    lcCurSeq = SUBSTR(LaTMImpFL(lnI,1),7,4)
    IF VAL(lcCurSeq) > VAL(SETUP.LastSeq)
       lnNewLstSq = MAX(lnNewLstSq,VAL(lcCurSeq))
       DIME LaImpFiles[lnY]
       LaImpFiles[lnY] = LaTMImpFL(lnI,1)
       lnY = lnY + 1
    ENDIF
  ENDFOR
  IF !EMPTY(LaImpFiles[1])
    lnImpnumber = ALEN(LaImpFiles,1)
  ENDIF
ENDIF

LaTMImpFL = ""

SET DEFA TO (lcCurPath)
IF USED('IMPMAP')
  USE IN IMPMAP
ENDIF

IF lnImpnumber = 0
  *--Nothing new to Import!
  RETURN
ENDIF  

*--------
IF !USED('IMPMAP')
  SELECT 0
  USE ('IMPMAP.DBF') SHARED
ENDIF
SELECT IMPMAP
SET ORDER TO TAG FLKEY

*--Open needed master files.
lcAriaDtPh = ALLT(SETUP.cAriaPath)+'DBFS\'+SUBSTR(SETUP.cComp,1,2)+'\'

IF !USED('ORDHDR')
  SELECT 0
  USE (lcAriaDtPh+'ORDHDR.DBF') SHARED
ENDIF
IF !USED('NOTEPAD')
  SELECT 0
  USE (lcAriaDtPh+'NOTEPAD.DBF') SHARED
ENDIF
IF !USED('ORDLINE')
  SELECT 0
  USE (lcAriaDtPh+'ORDLINE.DBF') SHARED
ENDIF
IF !USED('STYLE')
  SELECT 0
  USE (lcAriaDtPh+'STYLE.DBF') SHARED
  SET ORDER TO TAG Style
ENDIF
IF !USED('CUSTOMER')
  SELECT 0
  USE (lcAriaDtPh+'CUSTOMER.DBF') SHARED
  SET ORDER TO TAG CUSTOMER
ENDIF
IF !USED('BOMVAR')
  SELECT 0
  USE (lcAriaDtPh+'BOMVAR.DBF') SHARED
ENDIF
IF !USED('ICNAMDRP')
  SELECT 0
  USE (lcAriaDtPh+'ICNAMDRP.DBF') SHARED
  SET ORDER TO TAG Icnamdrpid 
ENDIF
IF !USED('ICDESIGN')
  SELECT 0
  USE (lcAriaDtPh+'ICDESIGN.DBF') SHARED
  SET ORDER TO TAG Icdesign
ENDIF

*--Close temp files if used.
IF USED('TMPORDHDR')
  USE IN TMPORDHDR
ENDIF
IF USED('TMPORDLIN')
  USE IN TMPORDLIN
ENDIF
IF USED('TMPBOMVAR')
  USE IN TMPBOMVAR
ENDIF
IF USED('TMPNAMDRP')
  USE IN TMPNAMDRP
ENDIF
IF USED('TMPNOTEPD')
  USE IN TMPNOTEPD
ENDIF

*--Create new temp files.
SELECT ORDHDR
COPY STRU TO TMPORDHDR
SELECT NOTEPAD
COPY STRU TO TMPNOTEPD
SELECT ORDLINE
COPY STRU TO TMPORDLIN
SELECT BOMVAR
COPY STRU TO TMPBOMVAR
SELECT ICNAMDRP  
COPY STRU TO TMPNAMDRP

*--Open new temp files.
SELECT 0
USE TMPORDHDR
SELECT 0
USE TMPNOTEPD
SELECT 0
USE TMPORDLIN
SELECT 0
USE TMPBOMVAR
SELECT 0
USE TMPNAMDRP
*-----------------


SET DEFA TO (ALLT(SETUP.cImpPath))

lcNewLstSq = STRTRAN(STR(lnNewLstSq,4),' ','0')

*--My Sequence
lnSqOrder = 800000


FOR LnCount = 1 TO lnImpnumber 
  lcIFile = LaImpFiles(LnCount)

  STORE 0 TO lnHdGroup,lnLnGroup
  STORE .F. TO llHdr,llLin
  lnLineNo = 0

  lnHndl=fopen(lcIFile)
  DO WHILE !FEOF(lnHndl)
    lcline = FGETS(lnHndl)
    IF 'ORDH450' $  lcline
      lnSqOrder = lnSqOrder + 1
      lnHdGroup = 0
      llHdr = .T.
      llLin = .F.
    ENDIF
    IF 'ORDD450' $  lcline
      lnLineNo = lnLineNo + 1
      lnLnGroup = 0
      llHdr = .F.
      llLin = .T.
    ENDIF
    IF llHdr
      lnHdGroup = lnHdGroup + 1
    ENDIF
    IF llLin
      lnLnGroup = lnLnGroup + 1
    ENDIF
      
    *--Start prepering....
    IF llHdr AND lnHdGroup <> 2
      =lfUpdOrdHd()
    ENDIF

    IF llLin
      =lfUpdOrdLn()
    ENDIF

  ENDDO
  =FCLOSE(lnHndl)

ENDFOR

SET DEFA TO (lcCurPath)


*--Run ORDERS split
=lfSplit()

*--Post to Master
*Simulate soupdate.prg
=lfUpdate()


*--Close files..
USE IN TMPORDHDR
USE IN TMPORDLIN
USE IN TMPBOMVAR
USE IN TMPNAMDRP
USE IN TMPNOTEPD

ERASE TMPORDHDR.DBF
ERASE TMPORDLIN.DBF
ERASE TMPBOMVAR.DBF
ERASE TMPNAMDRP.DBF
ERASE TMPNOTEPD.DBF

ERASE TMPORDHDR.CDX
ERASE TMPORDLIN.CDX
ERASE TMPBOMVAR.CDX
ERASE TMPNOTEPD.CDX

ERASE TMPBOMVAR.FPT
ERASE TMPNOTEPD.FPT
ERASE TMPORDLIN.FPT

IF USED('ORDHDR')
  USE IN ORDHDR
ENDIF
IF USED('NOTEPAD')
  USE IN NOTEPAD
ENDIF
IF USED('ORDLINE')
  USE IN ORDLINE
ENDIF
IF USED('STYLE')
  USE IN STYLE
ENDIF
IF USED('CUSTOMER')
  USE IN CUSTOMER
ENDIF
IF USED('BOMVAR')
  USE IN BOMVAR
ENDIF
IF USED('ICNAMDRP')
  USE IN ICNAMDRP
ENDIF
IF USED('ICDESIGN')
  USE IN ICDESIGN
ENDIF

SELECT SETUP
REPLACE LastSeq WITH lcNewLstSq
RETURN


***************************************************************************
***************************************************************************
FUNCTION lfUpdOrdHd

SELECT TMPORDHDR
IF lnHdGroup = 1
  APPEND BLANK
  REPLACE CORDTYPE   WITH 'O',;
          ORDER      WITH '',;
          cOwner     WITH STR(lnSqOrder,6),;
          STATUS     WITH 'O',;
          ACCOUNT    WITH IIF(SEEK('ORDHDR    '+'ACCOUNT   ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),'') ,;
          STORE      WITH IIF(SEEK('ORDHDR    '+'STORE     ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),'') ,;
          CUSTPO     WITH IIF(SEEK('ORDHDR    '+'CUSTPO    ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),'') ,;
          NOTE1      WITH IIF(SEEK('ORDHDR    '+'NOTE1     ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),'') ,;
          CORDERCAT  WITH IIF(SEEK('ORDHDR    '+'ORDERCAT  ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),'') ,;
          SEASON     WITH IIF(SEEK('ORDHDR    '+'SEASON    ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),'') ,;
          CDIVISION  WITH 'FPSW',;
          CTERMCODE  WITH IIF(SEEK('ORDHDR    '+'CTERMCODE ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),'') ,;
          SHIPVIA    WITH IIF(SEEK('ORDHDR    '+'SHIPVIA   ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),'') ,;
          APPROVAL   WITH IIF(SEEK('ORDHDR    '+'APPROVAL  ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),'') ,;
          REP1       WITH IIF(SEEK('ORDHDR    '+'REP1      ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),'') ,;
          SPCINST    WITH IIF(SEEK('ORDHDR    '+'SPCINST   ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),'')
*          CADDRESS1  WITH IIF(SEEK('ORDHDR    '+'NOTEPAD   ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,30),'') ,;
*          CADDRESS2  WITH IIF(SEEK('ORDHDR    '+'NOTEPAD   ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart+30,20),'')

  lcEntDat = IIF(SEEK('ORDHDR    '+'ENTERED   ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),'20000101')
  lcStrDat = IIF(SEEK('ORDHDR    '+'START     ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),'20000101')
  lcComDat = IIF(SEEK('ORDHDR    '+'COMPLETE  ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),'20000101')
  REPLACE ENTERED    WITH CTOD(SUBSTR(lcEntDat,5,2)+'/'+SUBSTR(lcEntDat,7,2)+'/'+SUBSTR(lcEntDat,1,4)),;
          START      WITH CTOD(SUBSTR(lcStrDat,5,2)+'/'+SUBSTR(lcStrDat,7,2)+'/'+SUBSTR(lcStrDat,1,4)),;
          COMPLETE   WITH CTOD(SUBSTR(lcComDat,5,2)+'/'+SUBSTR(lcComDat,7,2)+'/'+SUBSTR(lcComDat,1,4))

ELSE
  REPLACE NOTE2      WITH IIF(SEEK('ORDHDR    '+'NOTE2     ','IMPMAP'),IIF(!EMPTY(SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd)),"Fax#:","")+SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),'') ,;
          PHONE      WITH IIF(SEEK('ORDHDR    '+'PHONE     ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),'') ,;
          LALLSHPEXT WITH IIF(SEEK('ORDHDR    '+'ALLSHPEXT ','IMPMAP'),IIF(SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd)='1',.T.,.F.),''),;
          LALWBKORD  WITH IIF(SEEK('ORDHDR    '+'ALWBKORD  ','IMPMAP'),IIF(SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd)='1',.T.,.F.),''),;
          DEPT       WITH IIF(SEEK('ORDHDR    '+'DEPT      ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),'')
          
  REPLACE BULK       WITH 'N',;
          CREORDER   WITH 'N',;
          MULTI      WITH 'N',;
          MULTIPO    WITH .F.,;
          LINK_CODE  WITH 'DEFDEF',;
          GL_SALES   WITH 'DEF',;
          CWARECODE  WITH 'FP-BLD',;
          CCURRCODE  WITH 'USD',;
          NEXRATE    WITH 1,;
          NCURRUNIT  WITH 1,;
          lItemRatio WITH .T.,;
          CADD_USER  WITH 'SHOWCASE',;
          CADD_TIME  WITH TIME(),;
          DADD_DATE  WITH DATE()

   =SEEK('M'+TMPORDHDR.ACCOUNT,'CUSTOMER')
   REPLACE PRIORITY  WITH CUSTOMER.PRIORITY,;
           CINSUR    WITH CUSTOMER.CINSUR,;
           COMM1     WITH CUSTOMER.COMM,;
           REP2      WITH CUSTOMER.REP2,;
           COMM2     WITH CUSTOMER.COMM2,;
           nMinAloPer WITH CUSTOMER.nMinAloPer,;
           LHASNOTES WITH !EMPTY(ALLT(CADDRESS1+CADDRESS2))

  IF !EMPTY(TMPORDHDR.STORE)
    =SEEK('S'+TMPORDHDR.ACCOUNT+TMPORDHDR.STORE,'CUSTOMER')
    REPLACE COMM1    WITH CUSTOMER.COMM,;
            REP2     WITH CUSTOMER.REP2,;
            COMM2    WITH CUSTOMER.COMM2,;
            nMinAloPer WITH CUSTOMER.nMinAloPer
  ENDIF
   
  IF !EMPTY(ALLT(TMPORDHDR.CADDRESS1 + TMPORDHDR.CADDRESS2))
    SELECT TMPNOTEPD
    APPEND BLANK
    REPLACE TYPE   WITH 'B',;
            cOwner WITH STR(lnSqOrder,6),;
            KEY    WITH '',;
            CDESC  WITH 'Notes For Order Number :',;
            MNOTES WITH ALLT(TMPORDHDR.CADDRESS1 + TMPORDHDR.CADDRESS2)
  ENDIF
  
ENDIF
RETURN

***************************************************************************
***************************************************************************
FUNCTION lfUpdOrdLn

IF lnLnGroup = 1
  SELECT TMPORDLIN
  APPEND BLANK
  REPLACE CORDTYPE  WITH 'O',;
          ORDER     WITH '',;
          cOwner    WITH STR(lnSqOrder,6),;
          ACCOUNT   WITH TMPORDHDR.ACCOUNT,;
          CWARECODE WITH 'FP-BLD',;
          LINENO    WITH lnLineNo,;
          STORE     WITH TMPORDHDR.STORE,;
          CUSTPO    WITH IIF(SEEK('ORDLINE   '+'CUSTPO    ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),'') ,;
          STYLE     WITH IIF(SEEK('ORDLINE   '+'STYLE     ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),'') ,;
          SEASON    WITH IIF(SEEK('ORDLINE   '+'SEASON    ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),'') ,;
          PRICE     WITH IIF(SEEK('ORDLINE   '+'PRICE     ','IMPMAP'),(VAL(SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd))/100),0),;
          COMM1     WITH TMPORDHDR.COMM1,;
          COMM2     WITH TMPORDHDR.COMM2

  REPLACE QTY1 WITH IIF(SEEK('ORDLINE   '+'QTY1      ','IMPMAP'),VAL(SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd)),0) ,;
          QTY2 WITH IIF(SEEK('ORDLINE   '+'QTY2      ','IMPMAP'),VAL(SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd)),0) ,;
          QTY3 WITH IIF(SEEK('ORDLINE   '+'QTY3      ','IMPMAP'),VAL(SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd)),0) ,;
          QTY4 WITH IIF(SEEK('ORDLINE   '+'QTY4      ','IMPMAP'),VAL(SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd)),0) ,;
          QTY5 WITH IIF(SEEK('ORDLINE   '+'QTY5      ','IMPMAP'),VAL(SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd)),0) ,;
          QTY6 WITH IIF(SEEK('ORDLINE   '+'QTY6      ','IMPMAP'),VAL(SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd)),0) ,;
          QTY7 WITH IIF(SEEK('ORDLINE   '+'QTY7      ','IMPMAP'),VAL(SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd)),0) ,;
          QTY8 WITH IIF(SEEK('ORDLINE   '+'QTY8      ','IMPMAP'),VAL(SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd)),0)

   REPLACE STYLE      WITH PADR(Style,12)+'-'+IIF(SEEK('ORDLINE   '+'COLOR     ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),'') ,;
           BOOK1      WITH Qty1,;
           BOOK2      WITH Qty2,;
           BOOK3      WITH Qty3,;
           BOOK4      WITH Qty4,;
           BOOK5      WITH Qty5,;
           BOOK6      WITH Qty6,;
           BOOK7      WITH Qty7,;
           BOOK8      WITH Qty8,;
           NOTE_MEM   WITH IIF(SEEK('ORDLINE   '+'NOTE_MEM  ','IMPMAP'),'Showcase order detail Reference#:'+SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),'') ,;
           GROS_PRICE WITH Price,;
           GL_SALES   WITH 'DEFDEF',;
           CADD_USER  WITH 'SHOWCASE',;
           CADD_TIME  WITH TIME(),;
           DADD_DATE  WITH DATE()

  =SEEK(TMPORDLIN.Style,'STYLE')
  REPLACE DESC1       WITH STYLE.Desc1,;
          SCALE       WITH STYLE.Scale

ENDIF

IF lnLnGroup = 2
  SELECT TMPORDLIN
  lcStrDat = IIF(SEEK('ORDLINE   '+'START     ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),'20000101')
  lcComDat = IIF(SEEK('ORDLINE   '+'COMPLETE  ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),'20000101')
  REPLACE START      WITH CTOD(SUBSTR(lcStrDat,5,2)+'/'+SUBSTR(lcStrDat,7,2)+'/'+SUBSTR(lcStrDat,1,4)),;
          COMPLETE   WITH CTOD(SUBSTR(lcComDat,5,2)+'/'+SUBSTR(lcComDat,7,2)+'/'+SUBSTR(lcComDat,1,4)),;
          TOTQTY      WITH IIF(SEEK('ORDLINE   '+'TOTQTY    ','IMPMAP'),VAL(SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd)),0),;
          TOTBOOK     WITH TotQty

  llAdornmt = !EMPTY(IIF(SEEK('BOMVAR    '+'CSTYLEPOS ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),''))
  IF llAdornmt
    SELECT TMPBOMVAR
    APPEND BLANK
    REPLACE CIDTYPE    WITH 'SO',;
            CCOST_ID   WITH '',;
            cOwner     WITH STR(lnSqOrder,6),;
            LINENO     WITH lnLineNo,;
            CSTYLEPOS  WITH IIF(SEEK('BOMVAR    '+'CSTYLEPOS ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),''),;
            CNDRPID    WITH IIF(SEEK('BOMVAR    '+'CNDRIPID  ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),''),;
            CDSGNCODE  WITH IIF(SEEK('BOMVAR    '+'CDSGNCODE ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),''),;
            NPRICEADD  WITH IIF(SEEK('BOMVAR    '+'NPRICEADD ','IMPMAP'),VAL(SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd)),0)

    =SEEK(TMPBOMVAR.CDSGNCODE,'ICDESIGN')
    REPLACE UNTCOST     WITH ICDESIGN.NDSGNCST,;
            NBOMTOTQTY  WITH 1,;
            TOTCOST     WITH UNTCOST,;
            CADD_USER   WITH 'SHOWCASE',;
            CADD_TIME   WITH TIME(),;
            DADD_DATE   WITH DATE()
  
    SELECT TMPNAMDRP
    IF !EMPTY(TMPBOMVAR.CNDRPID) AND !SEEK(TMPBOMVAR.CNDRPID,'ICNAMDRP')
      LOCATE FOR CNDRPID = TMPBOMVAR.CNDRPID 
      IF !FOUND()
        SELECT TMPNAMDRP
        APPEND BLANK
        REPLACE CNDRPID     WITH TMPBOMVAR.CNDRPID,;     
                ACCOUNT     WITH TMPORDLIN.ACCOUNT,;
                STORE       WITH TMPORDLIN.STORE,;
                CNDRPTT     WITH IIF(SEEK('ICNAMDRP  '+'CNDRPTT   ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),''),;
                CNDRPTB     WITH IIF(SEEK('ICNAMDRP  '+'CNDRPTB   ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),''),;
                CNDRPCLR    WITH IIF(SEEK('ICNAMDRP  '+'CNDRPCLR  ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),''),;
                CNDRPFNT    WITH IIF(SEEK('ICNAMDRP  '+'CNDRPFNF  ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),''),;
                CDSNSTATUS  WITH 'A',;
                CADD_USER   WITH 'SHOWCASE',;
                CADD_TIME   WITH TIME(),;
                DADD_DATE   WITH DATE()
      ENDIF
    ENDIF  
  ENDIF
  
  SELECT TMPORDHDR
  REPLACE BOOK       WITH BOOK + TMPORDLIN.TOTQTY ,;
          BOOKAMT    WITH BOOKAMT + (TMPORDLIN.TOTQTY * TMPORDLIN.PRICE ),;
          OPEN       WITH BOOK,;
          OPENAMT    WITH BOOKAMT,;
          LASTLINE   WITH TMPORDLIN.LINENO

ENDIF

IF lnLnGroup = 3
  llAdornmt = !EMPTY(IIF(SEEK('BOMVAR    '+'CSTYLEPOS ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),''))
  IF llAdornmt
    IF !EMPTY(TMPBOMVAR.CNDRPID) AND !SEEK(TMPBOMVAR.CNDRPID,'ICNAMDRP')
      SELECT TMPNAMDRP
      LOCATE FOR CNDRPID = TMPBOMVAR.CNDRPID
      IF FOUND()
        REPLACE NNDRPPSIZE WITH IIF(SEEK('ICNAMDRP  '+'NNDRPPSIZE','IMPMAP'),VAL(SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd)),0),;
                NNDRPMAXLN WITH IIF(SEEK('ICNAMDRP  '+'NNDRPMAXLN','IMPMAP'),VAL(SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd)),0),;
                NNDRPPRIC  WITH IIF(SEEK('ICNAMDRP  '+'NNDRPPRIC ','IMPMAP'),VAL(SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd)),0),;
                CSPECINS   WITH IIF(SEEK('ICNAMDRP  '+'CSPECINS  ','IMPMAP'),SUBSTR(lcline,IMPMAP.nStart,IMPMAP.nEnd),'')
      ENDIF
    ENDIF
  ENDIF
ENDIF

RETURN



***************************************************************************
***************************************************************************
FUNCTION lfSplit


SELECT 0
USE (lcAriaDtPh+'Sequence.DBF') SHARED
SET ORDER TO TAG Cseq_type
LNORD = IIF(SEEK('ORDER     '),NSEQ_NO,900001)

SELECT Tmpordhdr
INDEX ON cowner tag temp

SELECT Tmpbomvar 
INDEX ON cowner+Str(LineNo,6)+cDsgnCode+cndrpid tag temp1
INDEX ON cowner+cDsgnCode+cndrpid+Str(LineNo,6) tag temp2

SELECT Tmpordlin
INDEX ON cowner+Str(LineNo,6) tag temp

*--UPDATE BLANK ORDERS SEQUENCE
SELECT Tmpbomvar
SET ORDER TO TAG temp1
lcOrdrNo = ""
SELECT Tmpordlin
SET FILTER TO !SEEK(cowner+Str(LineNo,6),'Tmpbomvar')
GO TOP
lcOrdrNo = cowner
SCAN
  IF cowner<>lcOrdrNo
    LNORD = LNORD + 1
    lcOrdrNo = cowner
  ENDIF
  REPLACE Tmpordlin.Order WITH STRTRAN(STR(LNORD,6),' ','0')

  *--UPDATE ORDER HEADER FOR BLANKS ORDERS
  IF SEEK(cowner,'Tmpordhdr') .AND. EMPTY(Tmpordhdr.Order)
    SELECT Tmpordhdr
    REPLACE Tmpordhdr.Order WITH Tmpordlin.Order
  ENDIF

  SELECT Tmpordlin
ENDSCAN
SET FILTER TO


*--UPDATE ADORNMENT ORDERS SEQUENCE
SELECT Tmpbomvar 
SET ORDER TO TAG temp2
GO TOP
IF !EOF()
  LNORD = LNORD + 1
ENDIF
lcOrdAdKey = cowner+cDsgnCode+cndrpid
SCAN
  IF cowner+cDsgnCode+cndrpid<>lcOrdAdKey
    LNORD = LNORD + 1
    lcOrdAdKey = cowner+cDsgnCode+cndrpid
  ENDIF
  REPLACE Tmpbomvar.cCost_Id WITH STRTRAN(STR(LNORD,6),' ','0')
ENDSCAN

*--UPDATE ORDER LINE AND ORDER HEADER FOR ADORNMENTS ORDERS
SELECT Tmpbomvar
SET ORDER TO TAG temp1
GO TOP
SCAN
  lcOrder = Tmpbomvar.cCost_Id

  *--UPDATE ORDLINE
  IF SEEK(cowner+Str(LineNo,6),'Tmpordlin')
    SELECT Tmpordlin
    SCATTER MEMVAR MEMO
    IF EMPTY(Tmpordlin.Order)
      REPLACE Tmpordlin.Order WITH lcOrder
    ELSE
      LOCATE REST WHILE cowner+Str(LineNo,6) = Tmpbomvar.cowner+Str(Tmpbomvar.LineNo,6) FOR Order = lcOrder
      IF !FOUND() 
        APPEND BLANK
        GATHER MEMVAR MEMO
        REPLACE Tmpordlin.Order WITH lcOrder 
      ENDIF 
    ENDIF
  ENDIF

  *--UPDATE ORDER HEADER
  SELECT Tmpbomvar
  IF SEEK(cowner,'Tmpordhdr')
    SELECT Tmpordhdr
    SCATTER MEMVAR MEMO
    IF EMPTY(Tmpordhdr.Order)
      REPLACE Tmpordhdr.Order WITH lcOrder
    ELSE
      LOCATE REST WHILE cowner = Tmpbomvar.cowner FOR Order = lcOrder
      IF !FOUND() 
        APPEND BLANK
        GATHER MEMVAR MEMO
        REPLACE Tmpordhdr.Order WITH lcOrder 

        SELECT TMPNOTEPD
        APPEND BLANK
        REPLACE TYPE   WITH 'B',;
                KEY    WITH lcOrder,;
                CDESC  WITH 'Notes For Order Number :'+lcOrder,;
                MNOTES WITH ALLT(m.CADDRESS1+m.CADDRESS2)
      ENDIF 
    ENDIF
  ENDIF

  SELECT Tmpbomvar
ENDSCAN
SELECT Tmpbomvar
SET ORDER TO


*--Update Order Header NotepaD
SELECT TMPNOTEPD
SCAN FOR EMPTY(TMPNOTEPD.KEY)
  IF SEEK(cOwner,'TMPORDHDR')
    SELECT TMPNOTEPD
    REPLACE cOwner WITH '',;
            KEY    WITH TMPORDHDR.Order,;
            CDESC  WITH 'Notes For Order Number :'+TMPORDHDR.Order
  ENDIF
ENDSCAN


SELECT Sequence
IF SEEK('ORDER     ')
  =RLOCK()
  REPLACE NSEQ_NO WITH LNORD+1
  UNLOCK
ENDIF
USE IN Sequence
RETURN


***************************************************************************
***************************************************************************
FUNCTION lfUpdate


SET ORDER TO TAG 'ORDHDR' IN 'ORDHDR'
SET ORDER TO TAG 'ORDLINE' IN 'ORDLINE'

SELECT Tmpordhdr
index on Order tag temp

*--Update BOMVAR
SELECT TMPBOMVAR
REPLACE ALL cOwner WITH ''
GO TOP
SELECT BOMVAR
APPEND FROM TMPBOMVAR

*--Update NAMEDROP
SELECT TMPNAMDRP
SCAN FOR !EMPTY(CNDRPID)
  IF !SEEK(CNDRPID,'ICNAMDRP')
    SCATTER MEMVAR MEMO
    SELECT ICNAMDRP
    APPEND BLANK
    GATHER MEMVAR MEMO 
  ENDIF
ENDSCAN


*--Update Notepad
SELECT TMPNOTEPD
GO TOP
IF !EOF()
  SELECT NOTEPAD
  APPEND FROM TMPNOTEPD
ENDIF


*--Update Style Position
IF !USED('ICSTYPOS')
  SELECT 0
  USE (lcAriaDtPh+'icstypos.DBF') SHARED
  SET ORDER TO TAG Icstypos 
ENDIF
SELECT TMPORDLIN
index on Order+Str(LineNo,6) tag temp

SELECT TMPBOMVAR
SCAN
  IF SEEK(TMPBOMVAR.CCOST_ID+STR(TMPBOMVAR.lineno,6),'TMPORDLIN') AND ;
    !SEEK(TMPORDLIN.Style+TMPBOMVAR.CSTYLEPOS,'ICSTYPOS')
      LCDSSIZ = IIF(SEEK(TMPBOMVAR.CDSGNCODE,'ICDESIGN'),ICDESIGN.cDSGNSIZE,'')
      IF EMPTY(TMPBOMVAR.CNDRPID)
        LNNNDRP = 0
      ELSE
        =SEEK(TMPBOMVAR.CNDRPID,'ICNAMDRP')
        IF !EMPTY(ICNAMDRP.CNDRPTT) AND !EMPTY(ICNAMDRP.CNDRPTB)
          LNNNDRP = 2
        ELSE
          LNNNDRP = 1
        ENDIF      
      ENDIF
      SELECT ICSTYPOS
      APPEND BLANK
      REPLACE STYLE     WITH TMPORDLIN.Style,;
              CSTYLEPOS WITH TMPBOMVAR.CSTYLEPOS,;
              CSTYMAJOR WITH SUBSTR(TMPORDLIN.Style,1,12),;
              M0ND_DSGN WITH IIF(LNNNDRP=0,LCDSSIZ,''),;
              M1ND_DSGN WITH IIF(LNNNDRP=1,LCDSSIZ,''),;
              M2ND_DSGN WITH IIF(LNNNDRP=2,LCDSSIZ,''),;
              CADD_USER WITH 'SHOWCASE',;
              CADD_TIME WITH TIME(),;
              DADD_DATE WITH DATE()
  ENDIF
ENDSCAN


*--Open needed files
IF !USED('icStyHst')
  SELECT 0
  USE (lcAriaDtPh+'icStyHst.DBF') SHARED
  SET ORDER TO TAG Styhst
ENDIF
IF !USED('arCusHst')
  SELECT 0
  USE (lcAriaDtPh+'arCusHst.DBF') SHARED
  SET ORDER TO TAG Acthst
ENDIF
IF !USED('STYDYE')
  SELECT 0
  USE (lcAriaDtPh+'STYDYE.DBF') SHARED
  SET ORDER TO TAG STYDYE
ENDIF

*--Clear temp ordhdr after split.
SELECT TMPORDHDR
REPLACE ALL cOwner  WITH "",;
            Book    WITH 0,;
            BookAmt WITH 0,;
            Open    WITH 0,;
            Openamt WITH 0,;
            CADDRESS1 WITH SPACE(1),;
            CADDRESS2 WITH SPACE(1)

SELECT TMPORDLIN
GO TOP
SCAN FOR TotQty > 0

  *--Update temp Order Header
  IF SEEK(TMPORDLIN.Order,'TMPORDHDR')
    SELECT TMPORDHDR 
    REPLACE BOOK       WITH BOOK + TMPORDLIN.TOTQTY ,;
            BOOKAMT    WITH BOOKAMT + (TMPORDLIN.TOTQTY * TMPORDLIN.PRICE ),;
            OPEN       WITH BOOK,;
            OPENAMT    WITH BOOKAMT
  ENDIF

  *--Update Order Line
  SELECT TMPORDLIN
  SCATTER MEMVAR MEMO
  INSERT INTO ORDLINE FROM MEMVAR

  *--Update style positions if needed.

  *--Update Style
  IF SEEK(TMPORDLIN.Style,'Style')
    SELECT Style 
    =RLOCK()
    REPLACE Ord1   WITH Ord1 + TMPORDLIN.Qty1,;
            Ord2   WITH Ord2 + TMPORDLIN.Qty2,;
            Ord3   WITH Ord3 + TMPORDLIN.Qty3,;
            Ord4   WITH Ord4 + TMPORDLIN.Qty4,;
            Ord5   WITH Ord5 + TMPORDLIN.Qty5,;
            Ord6   WITH Ord6 + TMPORDLIN.Qty6,;
            Ord7   WITH Ord7 + TMPORDLIN.Qty7,;
            Ord8   WITH Ord8 + TMPORDLIN.Qty8,;
            TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
    UNLOCK
  
    *--Update StyDye
    IF !SEEK(TMPORDLIN.Style+TMPORDLIN.cWareCode+SPACE(10),'StyDye')
      SELECT StyDye 
      APPEND BLANK  
    ENDIF
    SELECT StyDye 
    =RLOCK()
    REPLACE Ord1   WITH Ord1 + TMPORDLIN.Qty1,;
            Ord2   WITH Ord2 + TMPORDLIN.Qty2,;
            Ord3   WITH Ord3 + TMPORDLIN.Qty3,;
            Ord4   WITH Ord4 + TMPORDLIN.Qty4,;
            Ord5   WITH Ord5 + TMPORDLIN.Qty5,;
            Ord6   WITH Ord6 + TMPORDLIN.Qty6,;
            Ord7   WITH Ord7 + TMPORDLIN.Qty7,;
            Ord8   WITH Ord8 + TMPORDLIN.Qty8,;
           TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
    UNLOCK
  ENDIF  

  *--Update Style history
  lcyear = STR(YEAR(TMPORDHDR.ENTERED),4)
  lcPerd = STRTRAN(STR(month(TMPORDHDR.ENTERED),2),' ','0')
  SELECT icStyHst
  IF !SEEK(TMPORDLIN.Style+lcyear ,'icStyHst')
    APPEND BLANK  
    REPLACE STYLE WITH TMPORDLIN.Style,CFISFYEAR WITH lcyear
  ENDIF
  =RLOCK()
  REPLACE nOrdQty&lcPerd WITH nOrdQty&lcPerd+ TMPORDLIN.TotQty,;
          nOrdQty        WITH nOrdQty       + TMPORDLIN.TotQty,;
          nOrdAmt&lcPerd WITH nOrdAmt&lcPerd+ (TMPORDLIN.TOTQTY * TMPORDLIN.PRICE ),;
          nOrdAmt        WITH nOrdAmt       + (TMPORDLIN.TOTQTY * TMPORDLIN.PRICE )
  UNLOCK
  SELECT TMPORDLIN  
  
ENDSCAN

*--Update Order Header
SELECT ORDHDR
APPEND FROM TMPORDHDR

SELECT TMPORDHDR
SCAN
  lcyear = STR(YEAR(TMPORDHDR.ENTERED),4)
  lcPerd = STRTRAN(STR(month(TMPORDHDR.ENTERED),2),' ','0')

  SELECT arCusHst
  IF SEEK(TMPORDHDR.Account+lcyear,'arCusHst')
    =RLOCK()
    REPLACE nOrdQty&lcPerd WITH nOrdQty&lcPerd + TMPORDHDR.OPEN,;
            nOrdQty        WITH nOrdQty        + TMPORDHDR.OPEN ,;
            nOrdAmt&lcPerd WITH nOrdAmt&lcPerd + TMPORDHDR.OPENAMT ,;
            nOrdAmt        WITH nOrdAmt        + TMPORDHDR.OPENAMT
    UNLOCK
  ENDIF

ENDSCAN

*--Close opened files
USE IN ICstypos
USE IN icStyHst
USE IN arCusHst
USE IN STYDYE

RETURN
