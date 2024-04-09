*:************************************************************************
*: Program file  : MFCTREC
*: Program desc. : Receive Simtex C/T For C#102717
*: For screen    : MFCTREC.SCX
*:         System: Aria advantage series
*:         Module: MF
*:      Developer: AHMED MAHER (AMH)
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************

lcTxtFile  = SPACE(25)

IF !gfSetup()
  RETURN
ENDIF

*--Use extended size scale.  
llMScale = gfGetMemVar('M_USEEXSSC')

*--Get color segment information.
STORE 0 TO lnClrSrt,lnClrEnd
=lfGetColor()
lnMajorLen = LEN(gfItemMask('PM'))

DECLARE laDueDate[1,2]
laDueDate[1,1] = 'LEADTIME  '
laDueDate[1,2] = 'lnLeadTime'

ON KEY LABEL ESC DO gfEscap
DO (gcScrDir+gcWinAppl+"\MFCTREC.SPX")
ON KEY LABEL ESC

*!*************************************************************
*! Name      : lfGetFile
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/01/2002
*! Purpose   : Validation of Text file button
*!*************************************************************
*! Returns   : 
*!*************************************************************
*! Example   : lfGetFile()
*!*************************************************************
*!
FUNCTION lfGetFile

lcTxtFile =GetFile('TXT','Select the text file.')

=lfTxtFile()

SHOW GET lcTxtFile

*!*************************************************************
*! Name      : lfTxtFile
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/01/2002
*! Purpose   : Validation of Text file field
*!*************************************************************
*! Returns   : 
*!*************************************************************
*! Example   : lfTxtFile()
*!*************************************************************
*!
FUNCTION lfTxtFile

IF !EMPTY(lcTxtFile)
  IF ATC('.TXT',lcTxtFile) = 0 
    IF ATC('.',lcTxtFile) <> 0 
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'You have to select a text file.')
      lcTxtFile = ""
      RETURN
    ELSE
      lcTxtFile = ALLTRIM(lcTxtFile) + '.TXT'
    ENDIF
  ENDIF
  IF !FILE(lcTxtFile)
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'This file does not exist.') 
    lcTxtFile = ""
    RETURN
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvOK
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/01/2002
*! Purpose   : Validation of OK button
*!*************************************************************
*! Returns   : 
*!*************************************************************
*! Example   : lfvOK()
*!*************************************************************
*!
FUNCTION lfvOK

IF EMPTY(lcTxtFile)
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,;
              'You must select a text file.')  
  RETURN              
ENDIF

PRIVATE lcFileName,lnSepOccrs,lnSepPos
lnSepOccrs = OCCURS('\',lcTxtFile)
lnSepPos = AT('\',lcTxtFile,lnSepOccrs)
lcFileName = PADR(STRTRAN(UPPER(SUBSTR(lcTxtFile,lnSepPos+1)),'.TXT'),6)
IF SEEK(lcFileName,'CTKTRECV')
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'File name has beeen received before.')
  RETURN
ENDIF

PRIVATE lnFileHand

lnFileHand = FOPEN(lcTxtFile)
IF lnFileHand = -1
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Cannot open this file.')
  RETURN
ENDIF

lcTmpCrsr = gfTempName()
=lfCrtCrsr()

= FSEEK(lnFileHand,0)
llRejct = .F.
DO WHILE !FEOF(lnFileHand)
  lcTxtLine = FGETS(lnFileHand)
  IF EMPTY(lcTxtLine)
    LOOP
  ENDIF
  DECLARE laCols[1]
  laCols = ' '
  = gfSubStr(lcTxtLine,@laCols,'|')
  IF !SEEK(laCols[1],'CUTTKTH')
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'C/T # : '+laCols[1]+;
                ' does not exist. Cannot Receive this file.')
    llRejct = .T.
    EXIT
  ENDIF
  IF CUTTKTH.STATUS $ 'HCS'
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'The status of C/T # : '+laCols[1]+' is '+;
                         IIF(CUTTKTH.STATUS='H','Hold.',;
                         IIF(CUTTKTH.STATUS='C','Completed.','Closed.'))+;
                         ' Cannot Receive this file.')
    llRejct = .T.
    EXIT
  ENDIF
  IF SEEK(PADR(laCols[2],lnMajorLen),'STYLE')
    SELECT STYLE
    LOCATE REST WHILE STYLE = PADR(laCols[2],lnMajorLen);
                  FOR SUBSTR(STYLE,lnClrSrt,lnClrEnd) = laCols[3]
    IF FOUND()
      llFound = .F.
      lnCount = 0
      SCAN REST WHILE STYLE = PADR(laCols[2],lnMajorLen);
                  FOR SUBSTR(STYLE,lnClrSrt,lnClrEnd) = laCols[3]
        IF SEEK('S'+SCALE,'SCALE') .AND. IIF(EMPTY(laCols[5]),.T.,SCALE.CDIM1 = laCols[4])
          FOR lnCount = 1 TO SCALE.CNT
            lcCount = STR(lnCount,1)
            IF EVALUATE('SCALE.SZ'+lcCount) = IIF(EMPTY(laCols[5]),laCols[4],laCols[5])
              llFound = .T.
              EXIT
            ENDIF
          ENDFOR
          IF llFound
            EXIT
          ENDIF
        ENDIF
      ENDSCAN
      IF !llFound
        =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Style/Color/Size : '+laCols[2]+'/'+;
                    laCols[3]+'/'+laCols[4]+'-'+laCols[5]+' does not exist. Cannot Receive this file.')
        llRejct = .T.
        EXIT
      ENDIF
    ELSE
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Style/Color : '+laCols[2]+'/'+laCols[3]+;
                  ' does not exist. Cannot Receive this file.')
      llRejct = .T.
      EXIT
    ENDIF
  ELSE
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Style # : '+laCols[2]+;
                ' does not exist. Cannot Receive this file.')
    llRejct = .T.
    EXIT
  ENDIF
  IF !SEEK(CUTTKTH.CUTTKT+STYLE.STYLE,'CUTTKTL')
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Style/Color : '+laCols[2]+'/'+laCols[3]+;
                ' does not exist in C/T # '+laCols[1]+'. Cannot Receive this file.')
    llRejct = .T.
    EXIT
  ENDIF
  IF !SEEK('DMFGCODE   '+PADR(laCols[6],6),'CODES') .AND. !SEEK('NMFGCODE   '+PADR(laCols[6],6),'CODES')
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'MFG. Operation # : '+laCols[6]+;
                ' does not exist. Cannot Receive this file.')
    llRejct = .T.
    EXIT
  ENDIF
  IF !SEEK('DMFGCODE   '+PADR(laCols[8],6),'CODES') .AND. !SEEK('NMFGCODE   '+PADR(laCols[8],6),'CODES')
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'MFG. Operation # : '+laCols[8]+;
                ' does not exist. Cannot Receive this file.')
    llRejct = .T.
    EXIT
  ENDIF
  IF !SEEK('M'+laCols[1]+PADR(laCols[6],6),'MFGOPRHD')
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'MFG. Operation # : '+laCols[6]+;
                ' does not exist in C/T # '+laCols[1]+'. Cannot Receive this file.')
    llRejct = .T.
    EXIT
  ENDIF
  lcOperSeq1 = MFGOPRHD.COPERSEQ
  IF !SEEK('M'+laCols[1]+PADR(laCols[8],6),'MFGOPRHD')
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'MFG. Operation # : '+laCols[8]+;
                ' does not exist in C/T # '+laCols[1]+'. Cannot Receive this file.')
    llRejct = .T.
    EXIT
  ENDIF
  lcOperSeq2 = MFGOPRHD.COPERSEQ
  IF VAL(lcOperSeq1) > VAL(lcOperSeq2)
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'The sequance of MFG. Operation # : '+;
                laCols[6]+' is greater than the sequance of MFG. Operation # : '+laCols[8]+;
                '. Cannot Receive this file.')
    llRejct = .T.
    EXIT
  ENDIF
  SELECT (lcTmpCrsr)
  IF !SEEK(PADR(lcFileName,8)+laCols[1]+STYLE.STYLE+PADR(laCols[6],6)+PADR(laCols[8],6)+;
           PADR(laCols[9],2))
    APPEND BLANK
    REPLACE CFILE_NAM WITH lcFileName,;
            CUTTKT    WITH laCols[1],;
            STYLE     WITH STYLE.STYLE,;
            COLOR     WITH laCols[3],;
            SCALE     WITH STYLE.SCALE,;
            COPRCODE  WITH laCols[6],;
            CTRGOPR   WITH laCols[8],;
            CLOTNO    WITH laCols[9],;
            DAUTSTGEN WITH gdSysDate
  ENDIF
  REPLACE TOTQTY WITH TOTQTY + VAL(laCols[7]),;
          ('QTY'+lcCount) WITH VAL(laCols[7])
ENDDO

=FCLOSE(lnFileHand)
IF llRejct
  USE IN (lcTmpCrsr)
  RETURN
ENDIF
SELECT (lcTmpCrsr)
SCAN
  SCATTER MEMVAR
  IF SEEK('M'+CUTTKT+COPRCODE+CLOTNO+'1','MFGOPRDT')
    SELECT MFGOPRDT
    LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd =;
                      'M'+m.Cuttkt+m.cOprCode+m.cLotNo+'1' FOR ITEM = m.Style
    IF !FOUND()
      SELECT (lcTmpCrsr)
      SEEK m.cFile_Nam+m.Cuttkt+m.Style
      LOCATE REST WHILE cfile_nam+cuttkt+style+coprcode+ctrgopr+clotno =;
                        m.cFile_Nam+m.Cuttkt+m.Style FOR CTRGOPR+CLOTNO = m.cOprCode+m.cLotNo
      IF !FOUND()
        =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'MFG. Operation/Lot : '+;
                    m.cOprCode+'/'+m.cLotNo+' not issued. Cannot Receive this file.')
        llRejct = .T.
        EXIT
      ENDIF
      SEEK m.cFile_Nam+m.Cuttkt+m.Style+m.cOprCode+m.cTrgOpr+m.cLotNo
    ENDIF
  ELSE
    SELECT (lcTmpCrsr)
    SCATTER MEMVAR
    SEEK m.cFile_Nam+m.Cuttkt+m.Style
    LOCATE REST WHILE cfile_nam+cuttkt+style+coprcode+ctrgopr+clotno =;
                      m.cFile_Nam+m.Cuttkt+m.Style FOR CTRGOPR+CLOTNO = m.cOprCode+m.cLotNo
    IF !FOUND()
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'MFG. Operation/Lot : '+;
                  m.cOprCode+'/'+m.cLotNo+' not issued. Cannot Receive this file.')
      llRejct = .T.
      EXIT
    ENDIF
    SEEK m.cFile_Nam+m.Cuttkt+m.Style+m.cOprCode+m.cTrgOpr+m.cLotNo
  ENDIF
ENDSCAN
IF llRejct
  USE IN (lcTmpCrsr)
  RETURN
ENDIF
=lfTblUpdt()

*!*************************************************************
*! Name      : lfCrtCrsr
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2002
*! Purpose   : Create temp. cursor
*!*************************************************************
*! Returns   : 
*!*************************************************************
*! Example   : lfCrtCrsr()
*!*************************************************************
*!
FUNCTION lfCrtCrsr

SELECT CTKTRECV
=AFIELDS(laFields)
CREATE CURSOR (lcTmpCrsr) FROM ARRAY laFields
SELECT (lcTmpCrsr)
INDEX ON cfile_nam+cuttkt+style+coprcode+ctrgopr+clotno TAG (lcTmpCrsr) OF (lcTmpCrsr)

*!*************************************************************
*! Name      : lfGetColor
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2002
*! Purpose   : Get the color length and width.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetColor()
*!*************************************************************
FUNCTION lfGetColor

DIME laMajSeg[1,1]
=gfItemMask(@laMajSeg)
FOR lnCnt=1 TO ALEN(laMajSeg,1)
  *--Check for existance of color segment in style structure.
  IF laMajSeg[lnCnt,1]='C'
    *--Get the color length and width.
    lnClrSrt = laMajSeg[lnCnt,4]
    lnClrEnd = LEN(laMajSeg[lnCnt,3])
    EXIT
  ENDIF
ENDFOR
RETURN

*!*************************************************************
*! Name      : lfTblUpdt
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2002
*! Purpose   : Update master tables
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfTblUpdt()
*!*************************************************************
FUNCTION lfTblUpdt

SELECT (lcTmpCrsr)

SCAN
  SCATTER MEMVAR
  INSERT INTO CTKTRECV FROM MEMVAR
  SELECT MFGOPRDT
  llFoundOpr = .T.
  IF !SEEK('M'+m.Cuttkt+m.cOprCode+m.cLotNo+'2')
    llFoundOpr = .F.
  ELSE
    LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd='M'+m.Cuttkt+m.cOprCode+m.cLotNo+'2';
                  FOR ITEM = m.Style
    IF !FOUND()
      llFoundOpr = .F.
    ENDIF
  ENDIF
  IF !llFoundOpr
    IF SEEK('M'+m.Cuttkt+m.cOprCode+m.cLotNo+'1')
      LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd='M'+m.Cuttkt+m.cOprCode+m.cLotNo+'2';
                    FOR ITEM = m.Style
      IF !FOUND()
        IF SEEK('M'+m.Cuttkt+m.cOprCode,'MFGOPRHD')
          SCATTER FIELDS MFGOPRHD.LINHOUSE,MFGOPRHD.CCONTCODE,MFGOPRHD.CCONTNAME MEMVAR
        ENDIF
      ELSE
        SCATTER FIELDS lInHouse,cContCode,cContName MEMVAR
      ENDIF
    ELSE
      IF SEEK('M'+m.Cuttkt+m.cOprCode,'MFGOPRHD')
        SCATTER FIELDS MFGOPRHD.LINHOUSE,MFGOPRHD.CCONTCODE,MFGOPRHD.CCONTNAME MEMVAR
      ENDIF
    ENDIF
    APPEND BLANK
    REPLACE CIMTYP    WITH 'M',;
            CTKTNO    WITH m.Cuttkt,;
            COPRCODE  WITH m.cOprCode,;
            CLOTNO    WITH m.cLotNo,;
            ITEM      WITH m.Style,;
            LINHOUSE  WITH m.lInHouse,;
            CCONTCODE WITH m.cContCode,;
            CCONTNAME WITH m.cContName,;
            TRANCD    WITH '2',;
            DTRANDATE WITH gdSysDate,;
            CTRGOPR   WITH m.cTrgOpr,;
            CTRGLOT   WITH m.cLotNo
  ENDIF
  REPLACE NLOTQTY1   WITH NLOTQTY1 + m.Qty1,;
          NLOTQTY2   WITH NLOTQTY2 + m.Qty2,;
          NLOTQTY3   WITH NLOTQTY3 + m.Qty3,;
          NLOTQTY4   WITH NLOTQTY4 + m.Qty4,;
          NLOTQTY5   WITH NLOTQTY5 + m.Qty5,;
          NLOTQTY6   WITH NLOTQTY6 + m.Qty6,;
          NLOTQTY7   WITH NLOTQTY7 + m.Qty7,;
          NLOTQTY8   WITH NLOTQTY8 + m.Qty8,;
          NLOTTOTQTY WITH NLOTTOTQTY + m.TotQty
  =gfAdd_Info('MFGOPRDT')
  llFoundOpr = .T.
  IF !SEEK('M'+m.Cuttkt+m.cTrgOpr+m.cLotNo+'1')
    llFoundOpr = .F.
  ELSE
    LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd='M'+m.Cuttkt+m.cTrgOpr+m.cLotNo+'1';
                  FOR ITEM = m.Style
    IF !FOUND()
      llFoundOpr = .F.
    ENDIF
  ENDIF
  IF !llFoundOpr
    IF SEEK('M'+m.Cuttkt+m.cTrgOpr,'MFGOPRHD')
      SCATTER FIELDS MFGOPRHD.LINHOUSE,MFGOPRHD.CCONTCODE,MFGOPRHD.CCONTNAME MEMVAR
    ENDIF
    lnLeadTime = 0
    = gfRltFld(m.cTrgOpr , @laDueDate , 'MFGCODE   ')
    ldDueDate = gdSysDate + lnLeadTime
    APPEND BLANK
    REPLACE CIMTYP    WITH 'M',;
            CTKTNO    WITH m.Cuttkt,;
            COPRCODE  WITH m.cTrgOpr,;
            CLOTNO    WITH m.cLotNo,;
            ITEM      WITH m.Style,;
            LINHOUSE  WITH m.lInHouse,;
            CCONTCODE WITH m.cContCode,;
            CCONTNAME WITH m.cContName,;
            TRANCD    WITH '1',;
            DTRANDATE WITH gdSysDate,;
            DUEDATE   WITH ldDueDate
  ENDIF
  REPLACE NLOTQTY1   WITH NLOTQTY1 + m.Qty1,;
          NLOTQTY2   WITH NLOTQTY2 + m.Qty2,;
          NLOTQTY3   WITH NLOTQTY3 + m.Qty3,;
          NLOTQTY4   WITH NLOTQTY4 + m.Qty4,;
          NLOTQTY5   WITH NLOTQTY5 + m.Qty5,;
          NLOTQTY6   WITH NLOTQTY6 + m.Qty6,;
          NLOTQTY7   WITH NLOTQTY7 + m.Qty7,;
          NLOTQTY8   WITH NLOTQTY8 + m.Qty8,;
          NLOTTOTQTY WITH NLOTTOTQTY + m.TotQty
  =gfAdd_Info('MFGOPRDT')
ENDSCAN

=gfModalGen("INM00000B00000","DIALOG",.F.,.F.,'Receiveing file completed successfully.')