*:************************************************************************
*: Program file  : POSTYPAN  (Copied form STY681Z.PRG FOR Andrew Marc )
*: Program desc. : PRINT STYLE PURCHASE ORDER FOR ANDREW.
*:               : Converted from Aria26 to Aria27.
*:         System: ARIA APPAREL SERIES
*:      Developer: Ashraf Sherif Mohammad (ASH)
*:           Date: 01/26/99
*:      Refer To : (C101389)
*:************************************************************************
*:B803006,1 WAB	- 01/26/2000  Fix the bug of wrong optinal program name and
*:B803006,1 WAB	              do not calling the disp report function from 
*:B803006,1 WAB	              the optional program
*:B803077,1 SSH 29/02/00      Prevent line duplicate
*:************************************************************************

*-- Initializing the necessary variables.
DIMENSION laScales[4,9]
*B803006,1 WAB - store space to aray holding scales
laScales = ''
*B803006,1 WAB - END
lcName = ""   
lcPo   = '  '
STORE 0 TO lnCount,lnCounter

lnPoLns    = 1

*B803006,1 WAB - comment this line cause temp name must be intialise in SyRepUVr.dbf
*B803006,1 WAB - and also create temp table in function lfCreatTmp when we run the 
*B803006,1 WAB - optional prog first time 
*lcTmpLin   = gfTempName()
*lcTempFile = gfTempName()
*TmpOrd     = gfTempName()
*PoSHdTmp   = gfTempName()
*wab
*-- To hold all the Orders that are allocated to the Po(s)
*CREATE CURSOR (TmpOrd) (cTranCd C(1),cPoNo C(6),cOrderNo C(6),;
*                            cAccount C(5), dCompDate D)
*INDEX ON cTranCd+cPoNo+cOrderNo TAG (TmpOrd) OF (TmpOrd)
*-- Opening the necessary files.
*=gfOpenFile (gcDataDir+'CutPick',gcDataDir+'Cutpkord','SH')
*=gfOpenFile (gcDataDir+'OrdHdr',gcDataDir+'OrdHdr','SH')
*=gfOpenFile (gcSysHome+'SycCurr',gcSysHome+'cCurrCode ','SH')
*SELECT PosLn
*=AFIELDS(laPosLnFld)
*lnPosLnFld = ALEN(laPosLnFld,1)
*DIMENSION laPosLnFld[lnPosLnFld+1,4]
*laPosLnFld[lnPosLnFld+1,1]='cNotes'
*laPosLnFld[lnPosLnFld+1,2]='M'
*laPosLnFld[lnPosLnFld+1,3]=''
*laPosLnFld[lnPosLnFld+1,4]='0'
*CREATE TABLE (gcWorkDir+lcTmpLin) FROM ARRAY laPosLnFld
*INDEX ON &lcTmpLin->Po+&lcTmpLin->Style+STR(&lcTmpLin->LineNo,4)+&lcTmpLin->Trancd TAG &lcTmpLin of (lcTmpLin)
*=gfOpenFile (gcWorkDir+lcTmpLin,' ','EX')
*SELECT PoSHdr
*SET RELATION TO 
*SET SKIP TO
*COPY STRUCTURE TO (gcWorkDir+PoSHdTmp)
*=gfOpenFile (gcWorkDir+PoSHdTmp,' ','EX')

=lfCreatTmp()
*B803006,1 WAB - END

SELECT PoSHdr
SCAN FOR &lcRpExp 
  SCATTER MEMVAR
  SELECT (PoSHdTmp)
  APPEND BLANK
  GATHER MEMVAR
ENDSCAN
SELECT (PoSHdTmp)

SCAN
  *-- Selecting all the Po Lines for the selected POs into lcTmpLin
  SELECT (PoSHdTmp)
  lcPoNumber = Po
  SELECT PosLn
  =SEEK('P'+lcPoNumber,'PosLn')
  DO WHILE Po = lcPoNumber
    lcStyle = Style
    SCAN REST WHILE Po+Style+STR(LineNo,4)+Trancd+STR(RECNO(),7)=lcPoNumber+lcStyle;
               FOR TranCd = "1"      
      WAIT WINDOW "Selecting recods. Po/Style :" + lcPoNumber+"/"+ALLTRIM(Style) NOWAIT
      SCATTER MEMVAR MEMO
      INSERT INTO (lcTmpLin) FROM MEMVAR
    ENDSCAN
    IF llRpPrtSn .AND. SEEK("F"+lcStyle,'NotePad')
      SELECT (lcTmpLin)
      GOTO BOTTOM
      SCATTER MEMVAR MEMO
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE cNotes WITH NotePad.mNotes
    ENDIF
    SELECT PosLn
  ENDDO
  FOR lnCnt = 1 TO 2
    SELECT (TmpOrd)
    APPEND BLANK
    REPLACE cTranCd   WITH '2'       ,;
           cPoNo     WITH lcPoNumber
  ENDFOR
  IF SEEK('2'+lcPoNumber,'CutPick')
    SELECT CutPick    
    SCAN REST WHILE Trancd+cTktNo+Order+Style+cOrdLine='2'+lcPoNumber
      WAIT WINDOW "Select allocating order#: " + Order NOWAIT
      IF !SEEK('2'+lcPoNumber+Order,TmpOrd)
        =SEEK('O'+Order,'OrdHdr')
        SELECT (TmpOrd)
        APPEND BLANK
        REPLACE cTranCd   WITH '2'       ,;
                cPoNo     WITH lcPoNumber,;
                cOrderNo  WITH CutPick.Order,;
                cAccount  WITH OrdHdr.Account,;
                dCompDate WITH OrdHdr.Complete
      ENDIF
    ENDSCAN    
  ENDIF
  SELECT (PoSHdTmp)  
ENDSCAN
WAIT CLEAR


*B803006,1 WAB - Release relation already created in master prog
SELECT PoSHdr
SET RELATION TO 
SET SKIP TO
*B803006,1 WAB - END

*-- Setting the relation between the PosLn and the necessary files.
SELECT (lcTmpLin)
SET RELATION TO 'P'+Po         INTO PoSHdr ADDITIVE
SET RELATION TO "S"+SCALE      INTO SCALE  ADDITIVE
SET RELATION TO STYLE          INTO STYLE  ADDITIVE
SET RELATION TO IIF(lnCounter >= lnPoLns,'2'+Po,' ')  INTO (TmpOrd) ADDITIVE
SET SKIP TO (TmpOrd)

SELECT PoSHdr
SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE
SET RELATION TO "P"+Po      INTO NOTEPAD ADDITIVE
SET RELATION TO PosHdr.cPriceCur INTO SycCurr ADDITIVE

SELECT SYCCOMP
SEEK gcAct_Comp
lcCompFax   = cCom_Fax              && Variable to hold the Company Fax
lcPhonPict  = gfPhoneTem()          && Variable to hold the Company Phone Format


SELECT (lcTmpLin)
GOTO TOP
lcRpExp='.T.'
*B803006,1 WAB - calling gfdispRE only from mater prog not in optional prog
*DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
*SELECT (lcTmpLin)
*SET RELATION TO
*B803006,1 WAB - END
*!*************************************************************
*! Name      : lfContPoLn
*! Developer : Ashraf Sherif Mohammad
*! Date      : 01/26/1999
*! Purpose   : To count all the PO lines for a certain Po.
*!*************************************************************
*! Passed Parameters  :  lcPoNo : PosLn.Po
*!*************************************************************
*! Example            :  lfContPoLn('PO')
*!*************************************************************
FUNCTION lfContPoLn
*B803006,1 WAB - add parameter lcDummy so we can call this function using 
*B803006,1 WAB - lfUsrVldFn
*PARAMETERS lcPoNo
PARAMETERS lcdummy,lcPoNo
*B803006,1 WAB - END
PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT * FROM (lcTmpLin) where cStyType+Po= 'P'+lcPoNo;
INTO CURSOR (lcTempFile)
lnPoLns = _TALLY
USE
SELECT(lnAlias)
IF !EOF()
  GO RECNO()
ENDIF
RETURN 0

*!*************************************************************
*! Name      : lfScale
*! Developer : Ashraf Sherif Mohammad
*! Date      : 01/26/1999
*! Purpose   : To get the scale size titles.
*!*************************************************************
*! Passed Parameters  : Y1,Y2
*!*************************************************************
*! Example            :  lfScale(1,2)
*!*************************************************************

FUNCTION lfScale
*B803006,1 WAB - add parameter lcDummy so we can call this function using 
*B803006,1 WAB - lfUsrVldFn
*PARAMETERS Y1,Y2
PARAMETERS lcDummy,Y1,Y2
*B803006,1 WAB - END
PRIVATE lnAlias 

lnAlias = SELECT(0)
IF  lcPo  = PosHdr.Po
  llFirst = .F.
ENDIF
IF llFirst
  llFirst=.F.
  SELECT PoSLn
  =SEEK('P'+PoSHdr.Po)
  STORE " " to laScales
  lcPo  = Po
  K=1
  SCAN WHILE Po = lcPo
    =SEEK("S"+Scale,"Scale")
    laScales(k,1)=Scale
    IF K > 1
      lnFor=lnCount
      FOR lnFor = K-1 TO 1 STEP -1
        IF laScales(k,1) = laScales(LNFOR,1)
          laScales(k,1)=" "
          llExit =.T.
          EXIT
        ELSE
          llExit =.F.
        ENDIF
      ENDFOR
      IF llExit
        LOOP
      ENDIF
    ENDIF
    FOR I=2 TO Scale.Cnt+1
      J = ALLTRIM(STR(I-1))
      laScales(k,i)=Scale.Sz&J
    ENDFOR
    k=k+1
    IF k>4
      EXIT
    ENDIF
  ENDSCAN
  =SEEK(lcPo)  
ENDIF
SELECT(lnAlias)
IF EMPTY(laScales[Y1,Y2])
  *B803006,1 WAB - the return must be the lcDummy cause we calling this 
  *B803006,1 WAB - function using lfUsrVldFn
   *RETURN('')
  lcDummy = ' '
  *B803006,1 WAB - END

ELSE  
  *B803006,1 WAB - the return must be the lcDummy cause we calling this 
  *B803006,1 WAB - function using lfUsrVldFn
  *RETURN IIF(_DOS,SUBSTR(laScales[Y1,Y2],1,3),laScales[Y1,Y2])
  lcDummy = laScales[Y1,Y2]
  *B803006,1 WAB - END
ENDIF

*!*************************************************************
*! Name      : lfref
*! Developer : Ashraf Sherif Mohammad
*! Date      : 01/26/1999
*! Purpose   : To refresh the relation between the lcTmpLin file
*!             and the file that contains the allocated orders TmpOrd
*!             for the selected po.
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Example            :  =lfref()
*!*************************************************************

FUNCTION lfRef
*B803006,1 WAB - add parameter lcDummy so we can call this function using 
*B803006,1 WAB - lfUsrVldFn
PARAMETERS lcDummy
*B803006,1 WAB - END
PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpLin)
IF !EOF()
  GOTO RECNO()
ENDIF
SELECT(lnAlias)
RETURN ""  



*!**************************************************************************
*! Name      : lfCreatTmp
*! Developer : WAB - Walid A. Wahab
*! Date      : 01/26/2000
*! Purpose   : Create temp. files
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfCreatTmp()
*!**************************************************************************
*!*B803006,1
*!**************************************************************************
FuncTion lfCreatTmp
*--check if the files already created so we zaped only the files
IF !llCreatTmp
*-- To hold all the Orders that are allocated to the Po(s)
  CREATE CURSOR (TmpOrd) (cTranCd C(1),cPoNo C(6),cOrderNo C(6),;
                               cAccount C(5), dCompDate D)
  INDEX ON cTranCd+cPoNo+cOrderNo TAG (TmpOrd) OF (TmpOrd)
  *-- Opening the necessary files.
  =gfOpenFile (gcDataDir+'CutPick',gcDataDir+'Cutpkord','SH')
  =gfOpenFile (gcDataDir+'OrdHdr',gcDataDir+'OrdHdr','SH')
  =gfOpenFile (gcSysHome+'SycCurr',gcSysHome+'cCurrCode ','SH')
  SELECT PosLn
  =AFIELDS(laPosLnFld)
  lnPosLnFld = ALEN(laPosLnFld,1)
  DIMENSION laPosLnFld[lnPosLnFld+1,4]
  laPosLnFld[lnPosLnFld+1,1]='cNotes'
  laPosLnFld[lnPosLnFld+1,2]='M'
  laPosLnFld[lnPosLnFld+1,3]=''
  laPosLnFld[lnPosLnFld+1,4]='0'
  CREATE TABLE (gcWorkDir+lcTmpLin) FROM ARRAY laPosLnFld
  INDEX ON Po+Style+STR(LineNo,4)+Trancd TAG &lcTmpLin
  
  =gfOpenFile (gcWorkDir+lcTmpLin,' ','EX')

  SELECT PoSHdr
  SET SKIP TO
  COPY STRUCTURE TO (gcWorkDir+PoSHdTmp)
  =gfOpenFile (gcWorkDir+PoSHdTmp,' ','EX')
  llCreatTmp = .T.
ELSE
  *-- remove relation between posln and scale,style cause lctmplin also
  *-- have an relation with the same files and the pohdr have an relation 
  *-- with posln and lctmpln so it make error 'relation is already engajed'
  *-- when we select lctmplin 
  select POSLN
  SET RELATION OFF INTO SCALE
  SET RELATION OFF INTO STYLE
  *:B803077,1 SSH 29/02/00      Prevent line duplicate
  SELECT PoSHdr
  SET SKIP TO
  *:B803077,1 SSH (End)
  SELECT (lcTmpLin)
  SET RELATION TO 
  SET SKIP TO
  SELECT (PoSHdTmp)
  ZAP
  SELECT (TmpOrd)
  ZAP
  SELECT (lcTmpLin)
  ZAP
  IF USED(lcTempFile)
    SELECT (lcTempFile)
    ZAP
  ENDIF
ENDIF