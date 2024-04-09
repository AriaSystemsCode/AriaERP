***********************************************************************
*:  Program file : ARSNDML.PRG
*:  Program desc.: Custom output file for Factor (Milberg)
*:         System: Aria 4XP
*:      Developer: Mariam Mazhar[MMT]
*:           Date: 02/03/2014
*:      Reference: C201601.Exe[T20140106.0053]
**************************************************************************
* Modifications:
*:************************************************************************
lcExpr = gfOpGrid('ARSNDMLB', .T.)
*!*************************************************************
*! Name      : lfvPath
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/03/2014
*! Purpose   : Validate file
*!*************************************************************
FUNCTION lfvPath
IF (!EMPTY(lcRpFile) AND !DIRECTORY(JUSTPATH(lcRpFile))) OR '?' $ lcRpFile
  lcRpFile= GETFILE('txt','Output File','Save',1)
ENDIF
*!*************************************************************
*! Name      : lfExport
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/03/2014
*! Purpose   : Export function
*!*************************************************************
FUNCTION lfExport
IF (EMPTY(lcRpFile) OR !DIRECTORY(JUSTPATH(lcRpFile))) 
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Invalid Output file name')
  RETURN .F.
ENDIF
IF !EMPTY(lcRpFile)
  SAVE ALL LIKE lcRpFile* TO (ADDBS(ALLTRIM(oAriaApplication.ResourceHome))+ ADDBS(ALLTRIM(oAriaApplication.User_ID))+'\ICMLBRG.MEM')
ENDIF	

lfCreateTemp()

lfDataCollect()

SELECT(lcTempWork)
LOCATE 
IF EOF()
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No Records to export')
  RETURN .F.
ELSE
  llBytesWrote = 0
  lfCreateTXTFile()  
  IF llBytesWrote > 0 AND FILE(lcRpFile)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,"File "+ALLTRIM(lcRpFile)+" has been exported successfully")
  ELSE
    =gfModalGen('TRM00000B00000','ALERT','','','Could not create output file. Cannot proceed.')
    RETURN .F.
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfCreateTemp
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/03/2014
*! Purpose   : Create temp.
*!*************************************************************
FUNCTION lfCreateTemp

IF USED(lcTempWork)
  USE IN (lcTempWork)
ENDIF

DIMENSION laTempStru[17,4]
laTempStru[1,1] = 'CLNTNO'
laTempStru[1,2] = "C"
laTempStru[1,3] = 5
laTempStru[1,4] = ''
laTempStru[2,1] = 'ACCOUNT'
laTempStru[2,2] = "C"
laTempStru[2,3] = 15
laTempStru[2,4] = ''
laTempStru[3,1] = 'BTNAME'
laTempStru[3,2] = "C"
laTempStru[3,3] = 30
laTempStru[3,4] = ''
laTempStru[4,1] = 'BTADD1'
laTempStru[4,2] = "C"
laTempStru[4,3] = 30
laTempStru[4,4] = ''
laTempStru[5,1] = 'BTADD2'
laTempStru[5,2] = "C"
laTempStru[5,3] = 30
laTempStru[5,4] = ''
laTempStru[6,1] = 'BTCITY'
laTempStru[6,2] = "C"
laTempStru[6,3] = 17
laTempStru[6,4] = ''
laTempStru[7,1] = 'BTSTATE'
laTempStru[7,2] = "C"
laTempStru[7,3] = 2
laTempStru[7,4] = ''
laTempStru[8,1] = 'BTZIP'
laTempStru[8,2] = "C"
laTempStru[8,3] = 5
laTempStru[8,4] = ''
laTempStru[9,1] = 'BTSTORE'
laTempStru[9,2] = "C"
laTempStru[9,3] = 4
laTempStru[9,4] = ''
laTempStru[10,1] = 'INVOICE'
laTempStru[10,2] = "C"
laTempStru[10,3] = 12
laTempStru[10,4] = ''
laTempStru[11,1] = 'INVDATE'
laTempStru[11,2] = "D"
laTempStru[11,3] = 8
laTempStru[11,4] = ''
laTempStru[12,1] = 'INVAMT'
laTempStru[12,2] = "N"
laTempStru[12,3] = 14
laTempStru[12,4] = 2
laTempStru[13,1] = 'TERMCODE'
laTempStru[13,2] = "C"
laTempStru[13,3] = 3
laTempStru[13,4] = ''
laTempStru[14,1] = 'TERMDESC'
laTempStru[14,2] = "C"
laTempStru[14,3] = 30
laTempStru[14,4] = ''
laTempStru[15,1] = 'CUSTPO'
laTempStru[15,2] = "C"
laTempStru[15,3] = 22
laTempStru[15,4] = ''
laTempStru[16,1] = 'FREIGHT'
laTempStru[16,2] = "N"
laTempStru[16,3] = 13
laTempStru[16,4] = 2
laTempStru[17,1] = 'CCONT_CODE'
laTempStru[17,2] = "C"
laTempStru[17,3] = 2
laTempStru[17,4] = ''
=gfCrtTmp(lcTempWork,@laTempStru,"INVOICE",lcTempWork,.T.)
*!*************************************************************
*! Name      : lfDataCollect
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/03/2014
*! Purpose   : Collect Data
*!*************************************************************
FUNCTION lfDataCollect

IF !USED("INVHDR_D")
  = gfOpenTable("INVHDR","INVHDR",'SH',"INVHDR_D")
ENDIF

IF !USED("CUSTOMER_D")
  = gfOpenTable("CUSTOMER","CUSTOMER",'SH',"CUSTOMER_D")
ENDIF


lcCursorInvoice = ''
llSelectInv = .F. 
lnPosInv = ASCAN(laOgFXFLT,"INVHDR.INVOICE")
IF lnPosInv > 0 
  lnPosInv = ASUBSCRIPT(laOgFXFLT,lnPosInv,1)
  lcCursorInvoice = laOgFXFLT[lnPosInv,6]
  IF !EMPTY(lcCursorInvoice) AND USED(lcCursorInvoice)
    SELECT(lcCursorInvoice)
    LOCATE
    IF !EOF()
      llSelectInv = .T. 
    ENDIF  
  ENDIF  
ENDIF    

lcCursorAccount= ''
llSelectAcc = .F. 
lnPosAcc = ASCAN(laOgFXFLT,"INVHDR.ACCOUNT")
IF lnPosAcc > 0 
  lnPosAcc  = ASUBSCRIPT(laOgFXFLT,lnPosAcc,1)
  lcCursorAccount = laOgFXFLT[lnPosAcc,6]
  IF !EMPTY(lcCursorAccount) AND USED(lcCursorAccount)
    SELECT(lcCursorAccount)
    LOCATE
    IF !EOF()
      llSelectAcc = .T. 
    ENDIF  
  ENDIF  
ENDIF    

STORE {} to LINVDATE ,HINVDATE 
IF ASCAN(laOGFxFlt,'INVHDR.INVDATE')> 0
  lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVHDR.INVDATE'),1)
  LINVDATE = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,ATC('|',laOGFxFlt[lnDatePos,6])-1))
  HINVDATE = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],  ATC('|',laOGFxFlt[lnDatePos,6])+1))
ENDIF

STORE {} to LPOSTDATE ,HPOSTDATE 
IF ASCAN(laOGFxFlt,'INVHDR.DPOSTDATE')> 0
  lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVHDR.DPOSTDATE'),1)
  LPOSTDATE = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,ATC('|',laOGFxFlt[lnDatePos,6])-1))
  HPOSTDATE = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],  ATC('|',laOGFxFlt[lnDatePos,6])+1))
ENDIF


IF llSelectInv && if user selected invoice range
  SELECT INVHDR_D
  =gfSetOrder("INVHDR")
  SELECT (lcCursorInvoice)
  LOCATE
  SCAN
    IF gfSEEK(&lcCursorInvoice..invoice,'INVHDR_D')
      =gfSeek(IIF(!Empty(INVHDR_D.STORE),'S','M')+INVHDR_D.ACCOUNT+INVHDR_D.STORE,"CUSTOMER_D")
      IF IIF(llSelectAcc,SEEK(INVHDR_D.ACCOUNT,lcCursorAccount),.T.) AND;
         IIF(!EMPTY(LINVDATE) AND !EMPTY(HINVDATE),BETWEEN(INVHDR_D.INVDATE ,LINVDATE ,HINVDATE ),.T.)  AND ;
         IIF(!EMPTY(LPOSTDATE) AND !EMPTY(HPOSTDATE),BETWEEN(INVHDR_D.DPOSTDATE,LPOSTDATE,HPOSTDATE),.T.)  
        WAIT WINDOW 'Collecting Data' NOWAIT                              
        INSERT INTO (lcTempWork)(CLNTNO,ACCOUNT,BTNAME,BTADD1,BTADD2,BTCITY,BTSTATE,BTZIP,BTSTORE,INVOICE,INVDATE,INVAMT,TERMCODE,TERMDESC,CUSTPO,FREIGHT,CCONT_CODE);
        VALUES (lcClntNumber,RTRIM(INVHDR_D.ACCOUNT),RTRIM(CUSTOMER_D.Btname),RTRIM(CUSTOMER_D.Caddress12),RTRIM(CUSTOMER_D.Caddress22),;
        SUBSTR(CUSTOMER_D.Caddress32,1,17),SUBSTR(CUSTOMER_D.Caddress42,1,2),SUBSTR(CUSTOMER_D.Caddress52,1,5),PADL(ALLTRIM(SUBSTR(INVHDR_D.STORE,1,4)),4),;
        PADL(ALLTRIM(INVHDR_D.INVOICE),12),INVHDR_D.INVDATE,INVHDR_D.totalchg,SUBSTR(INVHDR_D.Ctermcode,1,3),gfCodDes(INVHDR_D.Ctermcode, 'CTERMCODE'),;
        INVHDR_D.Custpo,INVHDR_D.Freight,lcClntCountry)
      ENDIF  
    ENDIF
  ENDSCAN
ELSE
  IF llSelectAcc
    SELECT INVHDR_D
    =gfSetOrder("INVHDRA")
    SELECT (lcCursorAccount)
    LOCATE
    SCAN
      SELECT INVHDR_D
      = gfSEEK(&lcCursorAccount..ACCOUNT,'INVHDR_D')
      SCAN REST WHILE ACCOUNT+INVOICE = &lcCursorAccount..ACCOUNT FOR STATUS <> 'V' AND IIF(!EMPTY(LINVDATE) AND !EMPTY(HINVDATE),BETWEEN(INVHDR_D.INVDATE ,LINVDATE ,HINVDATE ),.T.)  AND ;
         IIF(!EMPTY(LPOSTDATE) AND !EMPTY(HPOSTDATE),BETWEEN(INVHDR_D.DPOSTDATE,LPOSTDATE,HPOSTDATE),.T.)  
        WAIT WINDOW 'Collecting Data' NOWAIT                     
        =gfSeek(IIF(!Empty(INVHDR_D.STORE),'S','M')+INVHDR_D.ACCOUNT+INVHDR_D.STORE,"CUSTOMER_D")
        INSERT INTO (lcTempWork)(CLNTNO,ACCOUNT,BTNAME,BTADD1,BTADD2,BTCITY,BTSTATE,BTZIP,BTSTORE,INVOICE,INVDATE,INVAMT,TERMCODE,TERMDESC,CUSTPO,FREIGHT,CCONT_CODE);
        VALUES (lcClntNumber,RTRIM(INVHDR_D.ACCOUNT),RTRIM(CUSTOMER_D.Btname),RTRIM(CUSTOMER_D.Caddress12),RTRIM(CUSTOMER_D.Caddress22),;
        SUBSTR(CUSTOMER_D.Caddress32,1,17),SUBSTR(CUSTOMER_D.Caddress42,1,2),SUBSTR(CUSTOMER_D.Caddress52,1,5),PADL(ALLTRIM(SUBSTR(INVHDR_D.STORE,1,4)),4),;
        PADL(ALLTRIM(INVHDR_D.INVOICE),12),INVHDR_D.INVDATE,INVHDR_D.totalchg,SUBSTR(INVHDR_D.Ctermcode,1,3),gfCodDes(INVHDR_D.Ctermcode, 'CTERMCODE'),;
        INVHDR_D.Custpo,INVHDR_D.Freight,lcClntCountry)
      ENDSCAN    
    ENDSCAN
  ELSE
    SELECT INVHDR_D
    =gfSetOrder("INVHDR")
    LOCATE 
    SCAN FOR STATUS <> 'V' AND IIF(!EMPTY(LINVDATE) AND !EMPTY(HINVDATE),BETWEEN(INVHDR_D.INVDATE ,LINVDATE ,HINVDATE ),.T.)  AND ;
         IIF(!EMPTY(LPOSTDATE) AND !EMPTY(HPOSTDATE),BETWEEN(INVHDR_D.DPOSTDATE,LPOSTDATE,HPOSTDATE),.T.)  
        WAIT WINDOW 'Collecting Data' NOWAIT                     
        =gfSeek(IIF(!Empty(INVHDR_D.STORE),'S','M')+INVHDR_D.ACCOUNT+INVHDR_D.STORE,"CUSTOMER_D")
        INSERT INTO (lcTempWork)(CLNTNO,ACCOUNT,BTNAME,BTADD1,BTADD2,BTCITY,BTSTATE,BTZIP,BTSTORE,INVOICE,INVDATE,INVAMT,TERMCODE,TERMDESC,CUSTPO,FREIGHT,CCONT_CODE);
        VALUES (lcClntNumber,RTRIM(INVHDR_D.ACCOUNT),RTRIM(CUSTOMER_D.Btname),RTRIM(CUSTOMER_D.Caddress12),RTRIM(CUSTOMER_D.Caddress22),;
        SUBSTR(CUSTOMER_D.Caddress32,1,17),SUBSTR(CUSTOMER_D.Caddress42,1,2),SUBSTR(CUSTOMER_D.Caddress52,1,5),PADL(ALLTRIM(SUBSTR(INVHDR_D.STORE,1,4)),4),;
        PADL(ALLTRIM(INVHDR_D.INVOICE),12),INVHDR_D.INVDATE,INVHDR_D.totalchg,SUBSTR(INVHDR_D.Ctermcode,1,3),gfCodDes(INVHDR_D.Ctermcode, 'CTERMCODE'),;
        INVHDR_D.Custpo,INVHDR_D.Freight,lcClntCountry)
    ENDSCAN    
  ENDIF
ENDIF 
*!*************************************************************
*! Name      : lfCreateTXTFile
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/03/2014
*! Purpose   : Create text file
*!*************************************************************
FUNCTION lfCreateTXTFile

lcOFile = oAriaApplication.workdir+gfTempName()+".txt"
SELECT (lcTempWork)
SCAN
  lcDateValue = PADL(ALLTRIM(sTR(MONTH(INVDATE))),2,'0')+PADL(ALLTRIM(sTR(day(INVDATE))),2,'0')+PADL(SUBSTR(ALLTRIM(sTR(year(INVDATE))),3,2),2,'0')
  lcDetLine = CLNTNO+ACCOUNT+BTNAME+BTADD1+BTADD2+BTCITY+BTSTATE+BTZIP+BTSTORE+INVOICE+lcDateValue+lcDateValue+PADL(ALLTRIM(STR(INVAMT*100)),10,'0')+SPACE(1)+;
              TERMCODE+TERMDESC+CUSTPO+PADL(ALLTRIM(STR(FREIGHT*100)),10,'0')+CCONT_CODE+CHR(13)+CHR(10)
  llBytesWrote = STRTOFILE(lcDetLine ,lcOFile,1)              
ENDSCAN 
IF FILE(lcOFile)
  COPY FILE (lcOFile) TO (lcRpFile) 
  ERASE (lcOFile)  
ENDIF
*!*************************************************************
*! Name      : lfWhenExp
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/03/2014
*! Purpose   : When function
*!*************************************************************
FUNCTION lfWhenExp
IF EMPTY(lcRpFile) AND FILE((ADDBS(ALLTRIM(oAriaApplication.ResourceHome))+ ADDBS(ALLTRIM(oAriaApplication.User_ID))+'\ICMLBRG.MEM'))
  RESTORE FROM (ADDBS(ALLTRIM(oAriaApplication.ResourceHome))+ ADDBS(ALLTRIM(oAriaApplication.User_ID))+'\ICMLBRG.MEM') ADDITIVE 
ENDIF
