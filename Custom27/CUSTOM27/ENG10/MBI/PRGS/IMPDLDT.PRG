*:************************************************************************
*: Program file  : IMPDLDT
*: Program desc. : Import Delivery Detail   For C#200299 & C#200300
*: For screen    :
*:         System: Aria advantage series
*:         Module: 
*:      Developer: Hend Ghanem (HBG)
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************

lcTxtFile  = SPACE(25)
lcVendor   = ""
lcVenName  = ""
lcWareCode = ""

llMulCurr  = gfGetMemVar('llMulCurr')
llDispPric = gfGetMemVar('M_PoDspPrc')
llStyMark  = gfGetMemVar('M_stymark')  ='T'
llWareLoc  = gfGetMemVar('M_WareLoc')  ='Y'

STORE gcBaseCurr TO lcPCurr,lcDCurr
STORE  1  TO lnUnit1,lnUnit2,lnPRate,lnDRate
STORE ' ' TO lcCont ,lcPhone

DIMENSION laTerm[1,2],laShip[1,2],laCodInfo[2,10]
STORE "" TO laTerm,laShip

STORE .F. TO llBrowse

DIMENSION laPercet[8]
STORE 100 TO laPercet
STORE '' TO lcIType1,lcIType2,lcIType3,lcIType4,lcIType5

lcIType1   = gfGetMemVar('M_cIType1')
lcIType2   = gfGetMemVar('M_cIType2')
lcIType3   = gfGetMemVar('M_cIType3')
lcIType4   = gfGetMemVar('M_cIType4')
lcIType5   = gfGetMemVar('M_cIType5')
llEditExRt = gfGetMemVar('LLEDITEXRA')

ON KEY LABEL ESC DO lfEscap
lcStySep = "-"
DO (gcScrDir + '\IMPDLDT.SPR')
ON KEY LABEL ESC

*!*************************************************************
*! Name      : lfGetFile
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/03/2002
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
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/03/2002
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
      SHOW GET lcTxtFile
      RETURN
    ELSE
      lcTxtFile = ALLTRIM(lcTxtFile) + '.TXT'
    ENDIF
  ENDIF
  IF !FILE(lcTxtFile)
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'This file does not exist.') 
    lcTxtFile = ""
  ENDIF
  SHOW GET lcTxtFile
ENDIF

*!*************************************************************
*! Name      : lfEscap
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/03/2002
*! Purpose   : Traping of Esacp
*!*************************************************************
*! Returns   : 
*!*************************************************************
*! Example   : lfEscap()
*!*************************************************************
*!
FUNCTION lfEscap

CLEAR READ

*!*************************************************************
*! Name      : lfvOK
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/03/2002
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

IF EMPTY(lcStySep)
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'You must enter a separator.')
  RETURN
ENDIF


=lfOpenFile()

lcTmPikHdr = gfTempName()
lcTmOrdLin = gfTempName()
lcPikOrder = gfTempName()
lcOrdLine  = gfTempName()
lcPOH      = gfTempName()
lcPOLine   = gfTempName()
lcCutPick  = gfTempName()
lcTmpLine  = gfTempName()
lcTemLoc   = gfTempName()
lcTempFile = gfTempName()
lcTmpCtPk  = gfTempName()

lnFileHNDL = ""

=lfCrtPikFl()
=lfCreatPik()   && Get the information of the Piktkt in temp files
=lfSavePik()    && Save The created PikTkt in master files

laCodInfo[1,01] = "CTERMCODE"    && Field Name
laCodInfo[1,02] = "laTerm"       && Array Name
laCodInfo[1,03] = "lnTerm"       && Popup Name
laCodInfo[1,04] = ""             && Popup Status  ("D"->Default,"A"->All)
laCodInfo[1,05] = .F.            && Include "N/A" (.T.->Yes,.F.,No)
laCodInfo[1,06] = .F.            && Include "ALL" (.T.->Yes,.F.,No)
laCodInfo[1,07] = ""             && Alternative File (For default val.)
laCodInfo[1,08] = ""             && Use this index for the Alternative file.
laCodInfo[1,09] = ""             && Seek this expretion.
laCodInfo[1,10] = "CTERMCODE"        && Alternative Field Name

laCodInfo[2,01] = "SHIPVIA   "
laCodInfo[2,02] = "laShip"
laCodInfo[2,03] = "lnShip"
laCodInfo[2,04] = ""
laCodInfo[2,05] = .F.
laCodInfo[2,06] = .F.
laCodInfo[2,07] = ""
laCodInfo[2,08] = ""
laCodInfo[2,09] = ""
laCodInfo[2,10] = "SHIPVIA"
lnShip    = 1
lnTerm    = 1
=gfwCodePop(@laCodInfo, "CTERMCODE", "L")
=gfwCodePop(@laCodInfo, "SHIPVIA", "L")

=lfCrtPoFl()
llGenPO = .F.
llCancelPO = .F.
*-- Call the screen of the vendor
DO (gcScrDir + '\IMPDLVEN.SPR')
lnGPoDim = 0
DIME laGenPO[1]
IF !llCancelPO 
  IF llGenPO AND lfUpdPO()
    =gfModalGen('INM38134B38018','DIALOG','P/O'+'|'+ALLTRIM(laGenPO[1])+"|"+ALLTRIM(laGenPO[ALEN(laGenPO,1)]))
  ELSE
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Could not Create any PO.') 
  ENDIF
ENDIF

=FFLUSH(lnFileHNDL)
*-- Close the text file
DO WHILE !FCLOSE(lnFileHNDL)
ENDDO
CREATE CURSOR TMPSTR (mStrRep M(10))
APPEND BLANK
APPEND MEMO mStrRep FROM (gcWorkDir+"ErrReprt.txt") OVERWRITE
lcWinTitl  = "Error Report"
IF !EMPTY(mStrRep)
  DO (gcScrDir + 'SM\SMSTRREP.SPR')
ENDIF  
USE IN TMPSTR


*!*************************************************************
*! Name      : lfCrtPikFl
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/03/2002
*! Purpose   : Creat temp files
*!*************************************************************
*! Returns   : 
*!*************************************************************
*! Example   : lfCrtPikFl()
*!*************************************************************
*!
FUNCTION lfCrtPikFl

SELECT PIKTKT
=AFIELDS(lafilestru)
CREATE DBF (gcWorkDir+lcTmPikHdr) FROM ARRAY lafilestru
INDEX ON PikTkt TAG (lcTmPikHdr)

SELECT ORDLINE
=AFIELDS(lafilestru)
lnI = ALEN(lafilestru,1)+1
DIMENSION lafilestru[lnI,4]
lafilestru[lnI,1] = 'Delete'
lafilestru[lnI,2] = 'L'
lafilestru[lnI,3] = 1
lafilestru[lnI,4] = 0
CREATE DBF (gcWorkDir+lcTmOrdLin) FROM ARRAY lafilestru
INDEX ON Order + Store + Style TAG (lcTmOrdLin)
INDEX ON Order + Piktkt TAG (lcPikOrder)

*---Error Report File
lnFileHNDL = FCREAT('&gcWorkDir.ErrReprt.txt')

*!*************************************************************
*! Name      : lfCreatPik
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/03/2002
*! Purpose   : Get Data
*!*************************************************************
*! Returns   : 
*!*************************************************************
*! Example   : lfCreatPik()
*!*************************************************************
*!
FUNCTION lfCreatPik

CREATE CURSOR lcMemFile (FMemo M(10))
APPEND BLANK
APPEND MEMO FMemo FROM &lcTxtFile
lcPiktkt   = ""
ldPikDate  = {}
ldComplete = {}

SET ORDER TO ORDLINES IN ORDLINE
SET ORDER TO (lcTmOrdLin) IN (lcTmOrdLin)
SET MEMOWIDTH TO 600
lcDateSet = SET('DATE')
SET DATE TO "BRITISH"

FOR lnI = 1 TO MEMLINES(lcMemFile.FMEMO)
  lcLine = MLINE(lcMemFile.FMEMO,lnI)
  IF PADL(lcLine,3) = '100'
    m.PikTkt = SUBSTR(lcLine,62,6)
    lcPiktkt = m.PikTkt
    WAIT WINDOW 'Creating piktkt # ' + m.PikTkt + ' Header' NOWAIT
    ldDate    = SUBSTR(lcLine,79,11)
    lcYear    = SUBSTR(ldDate,1,4)
    lcMonth   = SUBSTR(ldDate,5,2)    
    lcDay     = SUBSTR(ldDate,7,2)  
    lcDate    = lcMonth+'/'+lcDay+'/'+lcyear
    m.Date    = CTOD(lcDate)
    ldPikDate = m.Date
    m.Order   = SUBSTR(lcLine,242,6)

    =lfGetPkHdr()
  ELSE
    IF PADL(lcLine,3) = '200'
      *ash done as per Tony's email on 07/24
      *lcStyMaj  = SUBSTR(lcLine,31,12)
      lcStyMaj  = SUBSTR(lcLine,33,7)+lcStySep+SUBSTR(lcLine,40,4)
      lcStyClr  = SUBSTR(lcLine,72,3)
      lcNrfCode = SUBSTR(lcLine,65,3) 
      *ash done as per Tony's email on 09/16/2002
      *IF SEEK(lcStyMaj+'-'+lcStyClr,'STYLE')
        *SELECT STYLE 
      IF SEEK(lcStyMaj+'-'+lcStyClr,'ORDLINE')
        SELECT ORDLINE
        llExit = .F.
        llFound = .F.
        *ash done as per Tony's email on 09/16/2002
        *SCAN REST WHILE PADL(STYLE,16) = lcStyMaj+'-'+lcStyClr
         * IF SEEK('S'+STYLE.Scale,'SCALE')
        SCAN REST WHILE Style = lcStyMaj+'-'+lcStyClr
          IF SEEK('S'+OrdLine.Scale,'SCALE')
            FOR lnJ = 1 TO 8
              lcJ = STR(lnJ,1)
              IF ALLTRIM(SCALE.cNrfCode&lcJ) = ALLTRIM(lcNrfCode)
                *ash done as per Tony's email on 09/16/2002
                *m.Style = lcStyMaj+'-'+lcStyClr+Style.Scale
                m.Style = lcStyMaj+'-'+lcStyClr+OrdLine.Scale
                lcSize  = lcJ
                llExit  = .T.
                llFound = .T.
                EXIT
              ENDIF
            ENDFOR
            IF llExit
              WAIT WINDOW 'Creating piktkt # ' + m.PikTkt + ' Detail, Style : ' + m.Style  NOWAIT            
              llExit = .F.
              lnQty = EVAL(SUBSTR(lcLine,172,8))
              =lfGetPklin()
              EXIT
            ENDIF  
          ENDIF
        ENDSCAN   
        IF !llFound
          =FPUTS(lnFileHNDL,'Refrence Code : ' + lcNrfCode +;
                 ' Not found in scale file for style : '+ lcStyMaj )
        ENDIF
      ELSE  
        =FPUTS(lnFileHNDL,'Style : '+ lcStyMaj + ' Not found in Style file.')
      ENDIF 
    ENDIF
  ENDIF
ENDFOR
SET DATE TO &lcDateSet

*!*************************************************************
*! Name      : lfGetPkHdr
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/03/2002
*! Purpose   : Get information of Piktkt header
*!*************************************************************
*! Returns   : 
*!*************************************************************
*! Example   : lfGetPkHdr()
*!*************************************************************
*!
FUNCTION lfGetPkHdr

IF SEEK('O'+m.Order,'ORDHDR')
  m.Account   = ORDHDR.Account
  m.cWareCode = ORDHDR.cWareCode
  ldComplete  = ORDHDR.Complete
  IF ORDHDR.MULTI = 'N'
    m.Store = ORDHDR.STORE
  ENDIF  
  IF !ORDHDR.MULTIPO
    m.CustPo = ORDHDR.CustPo
  ENDIF   
  INSERT INTO (lcTmPikHdr) FROM MEMVAR
ELSE
  =FPUTS(lnFileHNDL,'Order # '+ m.Order + 'Not found in Order header file.')
ENDIF

*!*************************************************************
*! Name      : lfGetPkLin
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/03/2002
*! Purpose   : Get information of Piktkt Detail
*!*************************************************************
*! Returns   : 
*!*************************************************************
*! Example   : lfGetPkLin()
*!*************************************************************
*!
FUNCTION lfGetPkLin

IF SEEK(m.Style+DTOS(ldComplete)+'O'+m.Order,'ORDLINE')
  SELECT ORDLINE
  SCATTER MEMVAR MEMO

  m.PikTkt  = lcPiktkt
  m.PikDate = ldPikDate
  FOR lnK = 1 TO 8
    lcK = STR(lnK,1)
    m.Qty&lcK = 0
  ENDFOR
  m.TotQty = 0
  m.Qty&lcSize = lnQty
  
  IF SEEK(m.Piktkt,lcTmPikHdr)
    SELECT (lcTmPikHdr)
    REPLACE Store  WITH m.Store,;
            CustPo WITH m.CustPo    
  ENDIF            
        
  IF !SEEK(m.Order+m.Store+m.Style,lcTmOrdLin)
    m.TotQty = m.Qty&lcSize
    INSERT INTO (lcTmOrdLin) FROM MEMVAR    
  ELSE
    SELECT (lcTmOrdLin)
    REPLACE Qty&lcSize WITH Qty&lcSize + m.Qty&lcSize,;
            TotQty     WITH TotQty     + m.Qty&lcSize
  ENDIF 
  
ELSE 
  =FPUTS(lnFileHNDL,'Style : '+ m.Style + 'Not found in Order header Detail file for order # ' +;
                                           m.Order) 
ENDIF

*!*************************************************************
*! Name      : lfSavePik
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/03/2002
*! Purpose   : Save The created PikTkt in master files
*!*************************************************************
*! Returns   : 
*!*************************************************************
*! Example   : lfSavePik()
*!*************************************************************
*!
FUNCTION lfSavePik

SET ORDER TO Ordlinst IN ORDLINE
SELECT (lcTmOrdLin)
llLoop = .F.
llUpdPik = .F.
DIMENSION laPikOrd[1,2]
lnI = 0

SCAN
  IF SEEK(&lcTmOrdLin..PikTkt,'PIKTKT')
    lnFound = ASCAN(laPikOrd,&lcTmOrdLin..PikTkt)
    IF lnFound  > 0
      lnPos   = ASUBSCRIPT(laPikOrd,lnFound,1)
      IF laPikOrd[lnI,1] = &lcTmOrdLin..PikTkt AND laPikOrd[lnI,2] = PIKTKT.Order
        LOOP
      ENDIF
    ENDIF  
    lnI = lnI + 1
    DIMENSION laPikOrd[lnI,2]
    laPikOrd[lnI,1] = &lcTmOrdLin..PikTkt
    laPikOrd[lnI,2] = PIKTKT.Order
    
    =FPUTS(lnFileHNDL,'Piktkt # '+ &lcTmOrdLin..PikTkt + 'Created befor for order # '+;
                     PIKTKT.Order) 
    LOOP
  ENDIF
  IF SEEK('O'+Order+Store+Style,'ORDLINE') AND EMPTY(ORDLINE.PIKTKT)
    llUpdPik = .T.
    SELECT ORDLINE
    REPLACE PIKTKT  WITH &lcTmOrdLin..PikTkt ,;
            PikDate WITH &lcTmOrdLin..PikDate,;
            Pik1    WITH &lcTmOrdLin..Qty1,;
            Pik2    WITH &lcTmOrdLin..Qty2,;
            Pik3    WITH &lcTmOrdLin..Qty3,;
            Pik4    WITH &lcTmOrdLin..Qty4,;                                    
            Pik5    WITH &lcTmOrdLin..Qty5,;
            Pik6    WITH &lcTmOrdLin..Qty6,;
            Pik7    WITH &lcTmOrdLin..Qty7,;
            Pik8    WITH &lcTmOrdLin..Qty8,;                                    
            TotPik  WITH &lcTmOrdLin..TOTQTY,;
            Picked  WITH .T.
 
    SELECT STYLE
    IF SEEK(&lcTmOrdLin..Style)        
      REPLACE Alo1   WITH Alo1   + &lcTmOrdLin..Qty1,;
              Alo2   WITH Alo2   + &lcTmOrdLin..Qty2,;
              Alo3   WITH Alo3   + &lcTmOrdLin..Qty3,;
              Alo4   WITH Alo4   + &lcTmOrdLin..Qty4,;                                    
              Alo5   WITH Alo5   + &lcTmOrdLin..Qty5,;
              ALO6   WITH Alo6   + &lcTmOrdLin..Qty6,;
              ALo7   WITH Alo7   + &lcTmOrdLin..Qty7,;
              Alo8   WITH Alo8   + &lcTmOrdLin..Qty8,;                                    
              TotAlo WITH TotAlo + &lcTmOrdLin..TOTQTY
    ENDIF

    SELECT STYDYE
    IF SEEK(&lcTmOrdLin..Style+&lcTmOrdLin..cWareCode)        
      REPLACE Alo1   WITH Alo1   + &lcTmOrdLin..Qty1,;
              Alo2   WITH Alo2   + &lcTmOrdLin..Qty2,;
              Alo3   WITH Alo3   + &lcTmOrdLin..Qty3,;
              Alo4   WITH Alo4   + &lcTmOrdLin..Qty4,;                                    
              Alo5   WITH Alo5   + &lcTmOrdLin..Qty5,;
              ALO6   WITH Alo6   + &lcTmOrdLin..Qty6,;
              ALo7   WITH Alo7   + &lcTmOrdLin..Qty7,;
              Alo8   WITH Alo8   + &lcTmOrdLin..Qty8,;                                    
              TotAlo WITH TotAlo + &lcTmOrdLin..TOTQTY
    ENDIF
  ELSE
    IF SEEK('O'+Order+Store+Style,'ORDLINE') AND !EMPTY(ORDLINE.PIKTKT)
      SELECT (lcTmOrdLin)
      REPLACE &lcTmOrdLin..Delete WITH .T.
    ENDIF  
  ENDIF
ENDSCAN

SELECT (lcTmOrdLin)
DELETE FOR &lcTmOrdLin..Delete 
llCreate = .F.
SET ORDER TO (lcPikOrder) IN (lcTmOrdLin)
IF llUpdPik 
  SELECT (lcTmPikHdr)
  SCAN
    IF !SEEK(Order+Piktkt,lcTmOrdLin)
      =FPUTS(lnFileHNDL,'Order # '+ &lcTmPikHdr..Order + 'have no details. '+;
                       'Cannot create a piktkt.') 
      LOOP
    ENDIF
    IF !SEEK(&lcTmPikHdr..PikTkt,'PIKTKT')
      llCreate = .T.
      REPLACE Status WITH 'O'
      SCATTER MEMVAR MEMO
      INSERT INTO PIKTKT FROM MEMVAR
    ENDIF  
  ENDSCAN
ENDIF

IF llCreate
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Creating pick ticket Completed successfully.') 
ELSE
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Could not Create any Pick Ticket.')   
ENDIF

*!*************************************************************
*! Name      : lfCrtPoFl
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/03/2002
*! Purpose   : Creat temp files for PO
*!*************************************************************
*! Returns   : 
*!*************************************************************
*! Example   : lfCrtPoFl()
*!*************************************************************
*!
FUNCTION lfCrtPoFl

SELECT OrdLine
=AFIELDS(laFilField)
lnAlen = ALEN(laFilField,1)
DIMENSION laFilField[lnAlen+11,4]
laFilField[lnAlen+1,1] = 'cSelect'
laFilField[lnAlen+1,2] = 'C'
laFilField[lnAlen+1,3] = 1
laFilField[lnAlen+1,4] = 0
laFilField[lnAlen+2,1] = 'cSortExp'
laFilField[lnAlen+2,2] = 'C'
laFilField[lnAlen+2,3] = 30
laFilField[lnAlen+2,4] = 0
laFilField[lnAlen+3,1] = 'cDivision'
laFilField[lnAlen+3,2] = 'C'
laFilField[lnAlen+3,3] = 6
laFilField[lnAlen+3,4] = 0
laFilField[lnAlen+4,1] = 'cPurCode'
laFilField[lnAlen+4,2] = 'C'
laFilField[lnAlen+4,3] = 6
laFilField[lnAlen+4,4] = 0
laFilField[lnAlen+5,1] = 'cStyGrade'
laFilField[lnAlen+5,2] = 'C'
laFilField[lnAlen+5,3] = 1
laFilField[lnAlen+5,4] = 0
laFilField[lnAlen+6,1] = 'Fabric'
laFilField[lnAlen+6,2] = 'C'
laFilField[lnAlen+6,3] = 7
laFilField[lnAlen+6,4] = 0
laFilField[lnAlen+7,1] = 'cFabClr'
laFilField[lnAlen+7,2] = 'C'
laFilField[lnAlen+7,3] = 6
laFilField[lnAlen+7,4] = 0
laFilField[lnAlen+8,1] = 'cFabWare'
laFilField[lnAlen+8,2] = 'C'
laFilField[lnAlen+8,3] = 6
laFilField[lnAlen+8,4] = 0
laFilField[lnAlen+9,1] = 'nRequired'
laFilField[lnAlen+9,2] = 'N'
laFilField[lnAlen+9,3] = 12
laFilField[lnAlen+9,4] = 3
laFilField[lnAlen+10,1] = 'nWIPUsed'
laFilField[lnAlen+10,2] = 'N'
laFilField[lnAlen+10,3] = 12
laFilField[lnAlen+10,4] = 3
laFilField[lnAlen+11,1] = 'nYeild'
laFilField[lnAlen+11,2] = 'N'
laFilField[lnAlen+11,3] = 7
laFilField[lnAlen+11,4] = 3

DECLARE laIndex[3,2]
laIndex[1,1] = 'CSELECT+CDIVISION+CPURCODE+CSTYGRADE+STYLE+DYELOT'
laIndex[1,2] = 'TICKET'
laIndex[2,1] = 'cordType+ORDER+STR(LINENO,6)'
laIndex[2,2] = '(lcOrdLine)'
laIndex[3,1] = 'cSelect+Fabric+cFabClr+cSortExp+Order+Store+Group'
laIndex[3,2] = 'Groups'
=gfCrtTmp(lcOrdLine,@laFilField,@laIndex)
SET ORDER TO TAG (lcOrdLine) IN (lcOrdLine)

SELECT POSHDR
=AFIELDS(lafilfield)
lnAlen = ALEN(laFilField,1)
DIMENSION laFilField[lnAlen+3,4]
laFilField[lnAlen+1,1] = 'cStyGrade'
laFilField[lnAlen+1,2] = 'C'
laFilField[lnAlen+1,3] = 1
laFilField[lnAlen+1,4] = 0
lafilfield[lnAlen+2,1] = 'nSteps'
lafilfield[lnAlen+2,2] = 'N'
lafilfield[lnAlen+2,3] = 2
lafilfield[lnAlen+2,4] = 0
lafilfield[lnAlen+3,1] = 'cTmpPo'
lafilfield[lnAlen+3,2] = 'C'
lafilfield[lnAlen+3,3] = 6
lafilfield[lnAlen+3,4] = 0

=gfCrtTmp(lcPOH,@lafilfield,[PO+CDIVISION+CPURCODE+CSTYGRADE],[CVENDIV])

SELECT POSLN
=AFIELDS(lafilfield)
lnAlen = ALEN(lafilfield,1)
DIMENSION lafilfield[lnAlen+7,4]
lafilfield[lnAlen+1,1] = 'CDIVISION'
lafilfield[lnAlen+1,2] = 'C'
lafilfield[lnAlen+1,3] = 6
lafilfield[lnAlen+1,4] = 0
lafilfield[lnAlen+2,1] = 'cPurCode'
lafilfield[lnAlen+2,2] = 'C'
lafilfield[lnAlen+2,3] = 6
lafilfield[lnAlen+2,4] = 0
lafilfield[lnAlen+3,1] = 'nSteps'
lafilfield[lnAlen+3,2] = 'N'
lafilfield[lnAlen+3,3] = 2
lafilfield[lnAlen+3,4] = 0
laFilField[lnAlen+4,1] = 'Fabric'
laFilField[lnAlen+4,2] = 'C'
laFilField[lnAlen+4,3] = 7
laFilField[lnAlen+4,4] = 0
laFilField[lnAlen+5,1] = 'cFabClr'
laFilField[lnAlen+5,2] = 'C'
laFilField[lnAlen+5,3] = 6
laFilField[lnAlen+5,4] = 0
laFilField[lnAlen+6,1] = 'cFabWare'
laFilField[lnAlen+6,2] = 'C'
laFilField[lnAlen+6,3] = 6
laFilField[lnAlen+6,4] = 0
laFilField[lnAlen+7,1] = 'nYeild'
laFilField[lnAlen+7,2] = 'N'
laFilField[lnAlen+7,3] = 7
laFilField[lnAlen+7,4] = 3
=gfCrtTmp(lcPOLine,@lafilfield,[PO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+DYELOT],[CSTYCLR])

SELECT CUTPICK
=AFIELDS(lafilfield)
lnAlen = ALEN(lafilfield,1)
DIMENSION lafilfield[lnAlen+7,4]
lafilfield[lnAlen+1,1] = 'CDIVISION'
lafilfield[lnAlen+1,2] = 'C'
lafilfield[lnAlen+1,3] = 6
lafilfield[lnAlen+1,4] = 0
lafilfield[lnAlen+2,1] = 'SEASON'
lafilfield[lnAlen+2,2] = 'C'
lafilfield[lnAlen+2,3] = 6
lafilfield[lnAlen+2,4] = 0
lafilfield[lnAlen+3,1] = 'cPurCode'
lafilfield[lnAlen+3,2] = 'C'
lafilfield[lnAlen+3,3] = 6
lafilfield[lnAlen+3,4] = 0
laFilField[lnAlen+4,1] = 'cStyGrade'
laFilField[lnAlen+4,2] = 'C'
laFilField[lnAlen+4,3] = 1
laFilField[lnAlen+4,4] = 0
lafilfield[lnAlen+5,1] = 'CWARECODE'
lafilfield[lnAlen+5,2] = 'C'
lafilfield[lnAlen+5,3] = 6
lafilfield[lnAlen+5,4] = 0
laFilField[lnAlen+6,1] = 'Dyelot'
laFilField[lnAlen+6,2] = 'C'
laFilField[lnAlen+6,3] = 10
laFilField[lnAlen+6,4] = 0
lafilfield[lnAlen+7,1] = 'nSteps'
lafilfield[lnAlen+7,2] = 'N'
lafilfield[lnAlen+7,3] = 2
lafilfield[lnAlen+7,4] = 0
DECLARE laIndex[1,2]
laIndex[1,1] = 'CTKTNO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+CWARECODE+DYELOT'
laIndex[1,2] = 'CSTYCLR'
=gfCrtTmp(lcCutPick,@lafilfield,@laIndex)


SELECT POSLN
=AFIELDS(laFStru)
lnFStru = ALEN(laFStru,1)
DIMENSION laFStru[lnFStru+13,4]

laFStru[lnFStru+1,1] = 'TOTSTK'
laFStru[lnFStru+2,1] = 'TOTDAM'
laFStru[lnFStru+3,1] = 'TOTCAN'  
laFStru[lnFStru+4,1] = 'TOTBAL'
laFStru[lnFStru+5,1] = 'CCARTON'
laFStru[lnFStru+6,1] = 'NLINENO'
laFStru[lnFStru+7,1] = 'LNEWLN'
laFStru[lnFStru+8,1] = 'NSTEPS'      
laFStru[lnFStru+9,1] = 'LALOCHG'
laFStru[lnFStru+10,1]= 'CLASTOPR'
laFStru[lnFStru+11,1]= 'LNEWLUPD'
laFStru[lnFStru+12,1]= 'LCOSTMADE'
laFStru[lnFStru+13,1]= 'LAUTOMODE'

STORE 'N' TO laFStru[lnFStru+1,2],laFStru[lnFStru+2,2],;
             laFStru[lnFStru+3,2],laFStru[lnFStru+4,2],;
             laFStru[lnFStru+6,2],laFStru[lnFStru+8,2]
STORE 'C' TO laFStru[lnFStru+5,2],laFStru[lnFStru+10,2]

STORE 'L' TO laFStru[lnFStru+7,2],laFStru[lnFStru+9,2],;
             laFStru[lnFStru+11,2],laFStru[lnFStru+12,2],;
             laFStru[lnFStru+13,2]

STORE  6  TO laFStru[lnFStru+1,3],laFStru[lnFStru+2,3],;
             laFStru[lnFStru+3,3],laFStru[lnFStru+4,3],;
             laFStru[lnFStru+6,3],laFStru[lnFStru+10,3]
STORE  3  TO laFStru[lnFStru+5,3]  
STORE  2  TO laFStru[lnFStru+8,3]
STORE  1  TO laFStru[lnFStru+7,3],laFStru[lnFStru+9,3],;
             laFStru[lnFStru+11,3],laFStru[lnFStru+12,3],;
             laFStru[lnFStru+13,3]

STORE  0  TO laFStru[lnFStru+1,4],laFStru[lnFStru+2,4],;
             laFStru[lnFStru+3,4],laFStru[lnFStru+4,4],;
             laFStru[lnFStru+5,4],laFStru[lnFStru+6,4],;
             laFStru[lnFStru+7,4],laFStru[lnFStru+8,4],;
             laFStru[lnFStru+9,4],laFStru[lnFStru+10,4],;
             laFStru[lnFStru+11,4],laFStru[lnFStru+12,4],;
             laFStru[lnFStru+13,4]

DIMENSION laTags[5,2]
laTags[1,1]='TranCd+cCarton+Po+Style+Dyelot+cWareCode+STR(LineNo,6)'
laTags[2,1]='cCarton+PO+Style+Dyelot+cWareCode+STR(LineNo,6)+TranCd'
laTags[3,1]='PO+Style+Dyelot+cWareCode+STR(LineNo,6)+cCarton+TranCd'
laTags[4,1]='cstytype+po+style+STR(lineno,6)+TranCd'
laTags[5,1]='shipno+cstytype+po+style+STR(lineno,6)+trancd'
laTags[1,2]='TmpLine1'
laTags[2,2]='TmpLine2'
laTags[3,2]='TmpLine3'
laTags[4,2]='POSLN'
laTags[5,2]='POSLNSH'

=gfCrtTmp(lcTmpLine,@laFStru,@laTags)

SELECT POSLN
DIMENSION laFileStru[1,4]
=AFIELDS(laFileStru)
DIMENSION laFileStru[ALEN(laFileStru,1) , 4] 
CREATE TABLE &gcWorkDir.&lcTempFile FROM ARRAY laFileStru
INDEX ON cstytype+po+style+STR(lineno,6)+trancd TAG (lcTempFile)

*--Warehouse location tmp file.
IF llWareLoc 
  SELECT WhsLoc
  =AFIELDS(laFStru)
  =gfCrtTmp(lcTemLoc,@laFStru,'Style+cWareCode+cLocation',lcTemLoc)
ENDIF  

SELECT CUTPICK
=AFIELDS(laFStru)
lnFStru = ALEN(laFStru,1)
DIMENSION laFStru[lnFStru+9,4]
FOR I=1 TO 9
  laFStru[lnFStru+I,1] = 'nCurPck'+STR(I,1)
  laFStru[lnFStru+I,2] = 'N'
  laFStru[lnFStru+I,3] = 6
  laFStru[lnFStru+I,4] = 0
ENDFOR  
CREATE DBF (gcWorkDir+lcTmpCtPk) FROM ARRAY laFStru
=gfOpenFile(gcWorkDir+lcTmpCtPk,'','EX')
INDEX ON Trancd+cTktNo+Style+cOrdLine TAG lcTmpCtPk

*!*************************************************************
*! Name      : lfCreatPo
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/03/2002
*! Purpose   : Get Data of PO
*!*************************************************************
*! Returns   : 
*!*************************************************************
*! Example   : lfCreatPo()
*!*************************************************************
FUNCTION lfCreatPo

*-- if the vendor is left empty, give the user a message
*-- (vendor field cannot be left empty)
IF EMPTY(lcVendor)
  =gfModalGen('INM36002B36000','DIALOG','Vendor')
  _CUROBJ = OBJNUM(lcVendor)
  RETURN
ENDIF

IF EMPTY(lcWareCode)
  =gfModalGen('INM00000B00000','DIALOG',.F.,.F.,'You have to select a warehouse.')
  _CUROBJ = OBJNUM(lcWareCode)
  RETURN
ENDIF

=SEEK(lcVendor,'APVENDOR')
lcPhone = ApVendor.cphoneno
lcCont  = ApVendor.cVenCont
lnI = 0

SELECT (lcTmOrdLin)
lcStyle = ""
SCAN 
  IF lcStyle <> SUBSTR(&lcTmOrdLin..Style,1,12)
    lcStyle = SUBSTR(&lcTmOrdLin..Style,1,12)
    IF !SEEK(SUBSTR(&lcTmOrdLin..Style,1,12),'BOM')
      lnI = lnI + 1
      DIME laOrder[lnI,2]
      laOrder[lnI,1] = &lcTmOrdLin..Order
      laOrder[lnI,2] = SUBSTR(&lcTmOrdLin..Style,1,12)
    ENDIF
  ENDIF  
ENDSCAN

IF lnI > 0
  FOR lnI = 1 TO ALEN(laOrder,1)
    REPLACE Delete WITH .T. FOR ORDER = laOrder[lnI] 
    =FPUTS(lnFileHNDL,'Order #: '+ laOrder[lnI,1] +' have a Style : '+laOrder[lnI,2]+;
                      ' Which have no cost sheet.')
  ENDFOR
ENDIF

DELETE FOR &lcTmOrdLin..Delete

LOCATE
IF EOF()
  llGenPO = .F.
  RETURN
ENDIF

SET RELATION TO &lcTmOrdLin..cOrdType+ &lcTmOrdLin..Order INTO Ordhdr ADDITIVE
SET RELATION TO &lcTmOrdLin..style INTO STYLE ADDITIVE
SET RELATION TO 'N'+'COLOR     '+SUBSTR(&lcTmOrdLin..Style,14,3);
                INTO CODES ADDITIVE
  
*khalid
*SCAN FOR !STYLE.MAKE AND !(Ordhdr.Status $ 'XCB') AND TOTQTY-TOTCUT > 0
SCAN FOR !STYLE.MAKE AND !(Ordhdr.Status $ 'XCB')
*khalid
  SCATTER MEMVAR
  m.cDivision = ORDHDR.cDivision
  m.cStyGrade = STYLE.cStyGrade
  m.cPurCode  = STYLE.cPurCode
  IF !SEEK(&lcTmOrdLin..ORDER+STR(&lcTmOrdLin..LINENO,6),lcOrdLine)
    INSERT INTO (lcOrdLine) FROM MEMVAR
  ENDIF
ENDSCAN

=lfUpdPOPik()

SELECT (lcTmOrdLin)
SET RELATION TO
llGenPO = .T.

CLEAR READ

*!*************************************************************
*! Name      : lfUpdPOPik
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/03/2002
*! Purpose   : Generate P/O's
*!*************************************************************
*! Calls     : gfGetExSin()
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfUpdPOPik()
*!*************************************************************
FUNCTION lfUpdPOPik

PRIVATE lnAlias,lcStyOrd,lcPUntSin,lcDUntSin,lcPExSign,lcDExSign,lcDivision,;
  lcPurCode,lcStyGrade,lnTCost1,lnTCost2,lnTCost3,lnTCost4,lnTCost5,;
  lnOrder,lnAllocat,lnTFCost1,lnTFCost2,lnTFCost3,lnTFCost4,lnTFCost5

PRIVATE lnCutQty1,lnCutQty2,lnCutQty3,lnCutQty4,lnCutQty5,lnCutQty6,lnCutQty7,lnCutQty8,;
  lnOrdQty1,lnOrdQty2,lnOrdQty3,lnOrdQty4,lnOrdQty5,lnOrdQty6,lnOrdQty7,lnOrdQty8

WAIT 'Generating Purchase Orders. Please standby....' WINDOW NOWAIT
STORE '/' TO lcPUntSin,lcDUntSin
lcPExSign = gfGetExSin(@lcPUntSin,lcPCurr)
lcDExSign = gfGetExSin(@lcDUntSin,lcDCurr)
lnAlias = SELECT()
SELECT STYLE
lcStyOrd = TAG()
SET ORDER TO TAG STYLE

SELECT (lcOrdLine)
lcOrdTag = TAG()
SET ORDER TO TAG TICKET

SELECT (lcOrdLine)
SCAN
  lcDivision = cDivision
  lcPurCode  = cPurCode
  lcStyGrade = cStyGrade
  
  SELECT (lcPOH)
  IF !SEEK(SPACE(6)+lcDivision+lcPurCode+lcStyGrade)
    APPEND BLANK
    REPLACE cStyType   WITH 'P' ,;
            Vendor     WITH lcVendor ,;
            STATUS     WITH 'H',;
            CDIVISION  WITH lcDivision,;
            cPurCode   WITH lcPurCode ,;
            ENTERED    WITH gdSysDate ,;
            COMPLETE   WITH ENTERED+90,;
            SHIPVIA    WITH laShip[lnShip,2]   ,;
            CTERMCODE  WITH laTerm[lnTerm,2]   ,;
            cWareCode  WITH lcWareCode ,;
            cStyGrade  WITH lcStyGrade ,;
            cPriceCur  WITH lcPCurr ,;
            nPriceRat  WITH lnPRate ,;
            nCurrUnit  WITH lnUnit1 ,;
            cDutyCur   WITH lcDCurr ,;
            nDutyRat   WITH lnDRate ,;
            nDCurUnit  WITH lnUnit2 ,;
            lMultiWare WITH .F.
  ENDIF
  STORE 0 TO lnTCost1,lnTCost2,lnTCost3,lnTCost4,lnTCost5,lnOrder,lnAllocat,;
             lnTFCost1,lnTFCost2,lnTFCost3,lnTFCost4,lnTFCost5

  IF llDispPric
    STORE 0 TO lnSelPrice,lnAllQty,lnRotSub,lnGrosMrgn
  ENDIF

  SELECT (lcOrdLine)
  DO WHILE cDivision +cPurCode+cStyGrade+STYLE+Dyelot = ;
           lcDivision+lcPurCode+lcStyGrade
    lcStyle  = STYLE
    lcDyelot = Dyelot
    lcFabric = Fabric
    lcFabClr = cFabClr
    lcFabWare= cFabWare
    lnYeild  = nYeild
    STORE 0 TO lnCutQty1,lnCutQty2,lnCutQty3,lnCutQty4,lnCutQty5,lnCutQty6,lnCutQty7,lnCutQty8
    STORE 0 TO lnOrdQty1,lnOrdQty2,lnOrdQty3,lnOrdQty4,lnOrdQty5,lnOrdQty6,lnOrdQty7,lnOrdQty8


    SCAN REST WHILE cDivision +cPurCode+cStyGrade+STYLE+Dyelot = ;
                    lcDivision+lcPurCode+lcStyGrade+lcStyle+lcDyelot

      SELECT(lcCutPick)
      APPEND BLANK
      REPLACE TRANCD    WITH '2',;
              ORDER     WITH &lcOrdLine..ORDER,;
              CORDLINE  WITH STR(&lcOrdLine..LINENO,6),;
              STYLE     WITH lcSTYLE ,;
              Dyelot    WITH lcDyelot,;
              cWareCode WITH lcWareCode ,;
              CDIVISION WITH lcDivision ,;
              cPurCode  WITH lcPurCode ,;
              cStyGrade WITH lcStyGrade

      REPLACE QTY1      WITH &lcOrdLine..QTY1,;
              QTY2      WITH &lcOrdLine..QTY2,;
              QTY3      WITH &lcOrdLine..QTY3,;
              QTY4      WITH &lcOrdLine..QTY4,;
              QTY5      WITH &lcOrdLine..QTY5,;
              QTY6      WITH &lcOrdLine..QTY6,;
              QTY7      WITH &lcOrdLine..QTY7,;
              QTY8      WITH &lcOrdLine..QTY8,;
              TOTQTY    WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8

      lnCutQty1 = lnCutQty1 + &lcOrdLine..QTY1
      lnCutQty2 = lnCutQty2 + &lcOrdLine..QTY2
      lnCutQty3 = lnCutQty3 + &lcOrdLine..QTY3
      lnCutQty4 = lnCutQty4 + &lcOrdLine..QTY4
      lnCutQty5 = lnCutQty5 + &lcOrdLine..QTY5
      lnCutQty6 = lnCutQty6 + &lcOrdLine..QTY6
      lnCutQty7 = lnCutQty7 + &lcOrdLine..QTY7
      lnCutQty8 = lnCutQty8 + &lcOrdLine..QTY8

      lnOrdQty1 = lnOrdQty1 + &lcCutPick..Qty1
      lnOrdQty2 = lnOrdQty2 + &lcCutPick..Qty2
      lnOrdQty3 = lnOrdQty3 + &lcCutPick..Qty3
      lnOrdQty4 = lnOrdQty4 + &lcCutPick..Qty4
      lnOrdQty5 = lnOrdQty5 + &lcCutPick..Qty5
      lnOrdQty6 = lnOrdQty6 + &lcCutPick..Qty6
      lnOrdQty7 = lnOrdQty7 + &lcCutPick..Qty7
      lnOrdQty8 = lnOrdQty8 + &lcCutPick..Qty8


      IF llDispPric
        STORE 0 TO lnTotQty
        FOR lnI = 1 TO 8
          lcI = STR(lnI,1)
          lnTotQty = lnTotQty + &lcOrdLine..QTY&lcI
        ENDFOR
        lnAllQty = lnAllQty + lnTotQty
        lnselPrice = lnSelPrice + ( lnTotQty*&lcOrdLine..Price )
      ENDIF
    ENDSCAN

    IF llDispPric
      lnSelPrice = lnSelPrice / lnAllQty
    ENDIF
 
    IF lnCutQty1+lnCutQty2+lnCutQty3+lnCutQty4+lnCutQty5+lnCutQty6+lnCutQty7+lnCutQty8 > 0
      SELECT (lcPOLine)
      IF !SEEK(SPACE(6)+lcDivision+lcPurCode+lcStyGrade+lcStyle+lcDyelot)
        SELECT (lcPOH)
        REPLACE LASTLINE WITH LASTLINE + 1
        SELECT (lcPOLine)
        APPEND BLANK
        REPLACE STYLE     WITH lcStyle    ,;
                DYELOT    WITH lcDyelot   ,;
                TRANCD    WITH '1'        ,;
                CDIVISION WITH lcDivision ,;
                cWareCode WITH lcWareCode ,;
                LINENO    WITH &lcPOH..LastLine ,;
                Vendor    WITH lcVendor   ,;
                cPurCode  WITH lcPurCode  ,;
                cStyGrade WITH lcStyGrade ,;
                Fabric    WITH lcFabric   ,;
                cFabClr   WITH lcFabClr   ,;
                cFabWare  WITH lcFabWare  ,;
                nYeild    WITH lnYeild
      ENDIF
      REPLACE QTY1 WITH lnCutQty1 ,;
              QTY2 WITH lnCutQty2 ,;
              QTY3 WITH lnCutQty3 ,;
              QTY4 WITH lnCutQty4 ,;
              QTY5 WITH lnCutQty5 ,;
              QTY6 WITH lnCutQty6 ,;
              QTY7 WITH lnCutQty7 ,;
              QTY8 WITH lnCutQty8 ,;
              Ord1 WITH lnOrdQty1 ,;
              Ord2 WITH lnOrdQty2 ,;
              Ord3 WITH lnOrdQty3 ,;
              Ord4 WITH lnOrdQty4 ,;
              Ord5 WITH lnOrdQty5 ,;
              Ord6 WITH lnOrdQty6 ,;
              Ord7 WITH lnOrdQty7 ,;
              Ord8 WITH lnOrdQty8 ,;
              TOTQTY WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
              TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
      =SEEK(lcStyle,'Style')
      FOR lnCount = 1 TO 5
        lcCount = STR(lnCount,1)
        DO CASE
          CASE lcIType&lcCount = 'P'
            lnCost&lcCount = IIF(lcPCurr=Style.cPriceCur,TOTQTY*STYLE.nICost&lcCount,0)
            lnECost&lcCount= lnCost&lcCount &lcPExSign lnPRate &lcPUntSin lnUnit1
          CASE INLIST(lcIType&lcCount,'M','D')
            lnCost&lcCount = IIF(lcDCurr=Style.cDutyCur,TOTQTY*STYLE.nICost&lcCount,0)
            lnECost&lcCount= lnCost&lcCount &lcDExSign lnDRate &lcDUntSin lnUnit2
          OTHERWISE
            lnCost&lcCount = TOTQTY*STYLE.nICost&lcCount
            lnECost&lcCount= lnCost&lcCount
        ENDCASE
        lnTCost&lcCount  = lnTCost&lcCount  + lnECost&lcCount
        lnTFCost&lcCount = lnTFCost&lcCount + lnCost&lcCount
      ENDFOR
      REPLACE nCost1  WITH nCost1 + lnCost1  ,;
              nCost2  WITH nCost2 + lnCost2  ,;
              nCost3  WITH nCost3 + lnCost3  ,;
              nCost4  WITH nCost4 + lnCost4  ,;
              nCost5  WITH nCost5 + lnCost5  ,;
              nECost1 WITH nECost1+ lnECost1 ,;
              nECost2 WITH nECost2+ lnECost2 ,;
              nECost3 WITH nECost3+ lnECost3 ,;
              nECost4 WITH nECost4+ lnECost4 ,;
              nECost5 WITH nECost5+ lnECost5

      IF llDispPric
        lnTotCost   = (nEcost1+nEcost2+nEcost3+nEcost4+nEcost5)/TOTQTY
        lnRotSub    = IIF(llStyMark,lnTotCost,lnSelPrice)
        lnGrosMrgn  = IIF(lnRotSub=0,0,((lnSelPrice - lnTotCost)/lnRotSub)*100)
        REPLACE nSelPrice WITH lnSelPrice ,;
                nGrosMrgn WITH lnGrosMrgn
        STORE 0 TO lnSelPrice,lnAllQty
      ENDIF
      lnOrder   = lnOrder   + TOTORD
      lnAllocat = lnAllocat + TOTQTY
    ENDIF
    SELECT (lcOrdLine)
  ENDDO
  SELECT (lcPOH)
  REPLACE nStyOrder WITH lnAllocat ,;
          TotOrd    WITH lnOrder   ,;
          NFCOST1   WITH lnTFCost1 ,;
          NFCOST2   WITH lnTFCost2 ,;
          NFCOST3   WITH lnTFCost3 ,;
          NFCOST4   WITH lnTFCost4 ,;
          NFCOST5   WITH lnTFCost5 ,;
          NICOST1   WITH lnTCost1  ,;
          NICOST2   WITH lnTCost2  ,;
          NICOST3   WITH lnTCost3  ,;
          NICOST4   WITH lnTCost4  ,;
          NICOST5   WITH lnTCost5
  IF nStyOrder = 0
    DELETE
  ENDIF
  SELECT (lcOrdLine)
  IF !BOF()
    SKIP -1
  ENDIF  
ENDSCAN

SET ORDER TO TAG (lcStyOrd) IN STYLE
SET ORDER TO TAG (lcOrdTag) IN (lcOrdLine)
SELECT (lnAlias)
WAIT CLEAR

*!*************************************************************
*! Name      : lfUpdPO
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/03/2002
*! Purpose   : Update the main files in case of generating PO
*!*************************************************************
*! Calls     : gfModalGen(),gfSequence()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfUpdPO()
*!*************************************************************
FUNCTION lfUpdPO
PRIVATE lcDivision,lcPurCode,lcPO,lcGrade,lcPONumber , llUpdBom ,;
        llGnCTBom

llGnCTBom = (gfGetMemVar("M_CRTCSTSH")  = "T")

SET ORDER TO TAG POSLN IN POSLN
SET ORDER TO TAG ORDLINE IN ORDLINE

lcFirstCT  = SPACE(06)
SELECT (lcPOH)
*-- Scan the tmp.POHdr for qty > 0 to generate auto. PO numbers
llUpdPo = .F.
SCAN FOR nStyOrder > 0
  llUpdPo = .T.
  lcPO       = PO
  llUpdBom   = .T.
  lcDivision = CDIVISION
  lcGrade    = cStyGrade
  lcPurCode  = CPURCODE
  lcPONumber = IIF(EMPTY(PO),IIF(EMPTY(cTmpPo),gfSequence('PO','','',CDIVISION),cTmpPo),PO)
  =RLOCK()
  REPLACE cTmpPo WITH lcPONumber
  UNLOCK
  lcFirstCT = IIF(EMPTY(lcFirstCT),lcPONumber,lcFirstCT)
  lcLastCt  = lcPONumber
  SELECT (lcPOLine)
  =SEEK(lcPO+lcDivision+lcPurCode+lcGrade)
  SCAN REST WHILE PO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+DYELOT =;
      lcPO+lcDivision+lcPurCode+lcGrade FOR  TOTQTY > 0

    llUpdBom  =  IIF(llUpdBom,SEEK(SUBSTR(Style,1,12),'BOM'),llUpdBom)
    IF !SEEK('P'+lcPONumber+STYLE+STR(LINENO,6)+TRANCD,'POSLN')
      SCATTER MEMVAR
      m.PO = lcPONumber
      =SEEK(m.STYLE,'STYLE')
      m.Scale    = STYLE.SCALE
      m.nCost1   = m.nCost1 / m.TotQty
      m.nCost2   = m.nCost2 / m.TotQty
      m.nCost3   = m.nCost3 / m.TotQty
      m.nCost4   = m.nCost4 / m.TotQty
      m.nCost5   = m.nCost5 / m.TotQty
      m.nECost1  = m.nECost1 / m.TotQty
      m.nECost2  = m.nECost2 / m.TotQty
      m.nECost3  = m.nECost3 / m.TotQty
      m.nECost4  = m.nECost4 / m.TotQty
      m.nECost5  = m.nECost5 / m.TotQty
      m.cStyType = 'P'
      IF !SEEK(m.STYLE+m.CWARECODE+SPACE(10),'STYDYE')
        WAIT "Assigning Style"+ALLTRIM(m.STYLE)+" to warehouse "+ALLTRIM(m.cWareCode) WINDOW NOWAIT
        DO gpAdStyWar WITH m.STYLE,SPACE(10),m.cWareCode
        =SEEK(m.STYLE+m.CWARECODE+SPACE(10),'STYDYE')
        WAIT CLEAR
      ENDIF
      IF SEEK(m.STYLE+m.CWARECODE+SPACE(10),'STYDYE')
        IF &lcPOLine..nSteps < 1 AND STYLE.lInvSty
          SELECT STYDYE
          =RLOCK()
          REPLACE WIP1   WITH WIP1   + m.Qty1  ,;
                  WIP2   WITH WIP2   + m.Qty2  ,;
                  WIP3   WITH WIP3   + m.Qty3  ,;
                  WIP4   WITH WIP4   + m.Qty4  ,;
                  WIP5   WITH WIP5   + m.Qty5  ,;
                  WIP6   WITH WIP6   + m.Qty6  ,;
                  WIP7   WITH WIP7   + m.Qty7  ,;
                  WIP8   WITH WIP8   + m.Qty8  ,;
                  TotWIP WITH TotWIP + m.TotQty,;
                  nWO1   WITH nWO1   + m.Qty1  ,;
                  nWO2   WITH nWO2   + m.Qty2  ,;
                  nWO3   WITH nWO3   + m.Qty3  ,;
                  nWO4   WITH nWO4   + m.Qty4  ,;
                  nWO5   WITH nWO5   + m.Qty5  ,;
                  nWO6   WITH nWO6   + m.Qty6  ,;
                  nWO7   WITH nWO7   + m.Qty7  ,;
                  nWO8   WITH nWO8   + m.Qty8  ,;
                  nTotWo WITH nTotWo + m.TotQty
          UNLOCK
          =gfTraceKey('STYDYE',STYLE+CWARECODE+DYELOT,'M')
          SELECT (lcPOLine)
          =RLOCK()
          REPLACE nSteps WITH 1
          UNLOCK
        ENDIF
      ENDIF 
      IF SEEK(m.STYLE,'STYLE')
        IF &lcPOLine..nSteps < 2 AND STYLE.lInvSty
          SELECT STYLE
          =RLOCK()
          REPLACE WIP1   WITH WIP1   + m.Qty1  ,;
                  WIP2   WITH WIP2   + m.Qty2  ,;
	              WIP3   WITH WIP3   + m.Qty3  ,;
	              WIP4   WITH WIP4   + m.Qty4  ,;
	              WIP5   WITH WIP5   + m.Qty5  ,;
                  WIP6   WITH WIP6   + m.Qty6  ,;
	              WIP7   WITH WIP7   + m.Qty7  ,;
	              WIP8   WITH WIP8   + m.Qty8  ,;
	              TotWIP WITH TotWIP + m.TotQty,;
                  nWO1   WITH nWO1   + m.Qty1  ,;
	              nWO2   WITH nWO2   + m.Qty2  ,;
	              nWO3   WITH nWO3   + m.Qty3  ,;
	              nWO4   WITH nWO4   + m.Qty4  ,;
  	              nWO5   WITH nWO5   + m.Qty5  ,;
	              nWO6   WITH nWO6   + m.Qty6  ,;
	              nWO7   WITH nWO7   + m.Qty7  ,;
	              nWO8   WITH nWO8   + m.Qty8  ,;
	              nTotWo WITH nTotWo + m.TotQty
          UNLOCK
          =gfTraceKey('STYLE',STYLE,'M')
          SELECT (lcPOLine)
          =RLOCK()
          REPLACE nSteps WITH 2
          UNLOCK
        ENDIF
      ENDIF  
      m.cAdd_User = gcUser_id
      m.cAdd_Time = TIME()
      m.dAdd_Date = gdSysDate
      SELECT (lcPOLine)
      =RLOCK()
      REPLACE nSteps WITH 3
      UNLOCK
      INSERT INTO POSLN FROM MEMVAR
    ELSE
      IF &lcPOLine..nSteps < 1
        SELECT POSLN
        =RLOCK()
        REPLACE Ord1   WITH Ord1 + &lcPOLine..Ord1 ,;
	            Ord2   WITH Ord2 + &lcPOLine..Ord2 ,;
	            Ord3   WITH Ord3 + &lcPOLine..Ord3 ,;
	            Ord4   WITH Ord4 + &lcPOLine..Ord4 ,;
	            Ord5   WITH Ord5 + &lcPOLine..Ord5 ,;
	            Ord6   WITH Ord6 + &lcPOLine..Ord6 ,;
	            Ord7   WITH Ord7 + &lcPOLine..Ord7 ,;
	            Ord8   WITH Ord8 + &lcPOLine..Ord8 ,;
	            TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
        UNLOCK
        =gfTraceKey('POSLN','P'+lcPONumber+cRsession+Shipno+STYLE+STR(LINENO,6)+Trancd,'M')
        SELECT (lcPOLine)
        =RLOCK()
        REPLACE nSteps WITH 1
        UNLOCK
      ENDIF
    ENDIF
    
    SELECT (lcCutPick)
    IF SEEK(lcPO+lcDivision+lcPurCode+lcGrade+POSLn.Style+POSLn.cWareCode+POSLn.Dyelot)
      SCAN REST WHILE CTKTNO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+CWARECODE+DYELOT =;
          lcPO+lcDivision+lcPurCode+lcGrade+POSLn.Style+;
          POSLn.cWareCode+POSLn.Dyelot FOR TotQty > 0
        SCATTER MEMVAR
        SELECT ORDHDR
        =SEEK('O'+m.ORDER)
        IF &lcCutPick..nSteps < 1
          =RLOCK()
          REPLACE TotCut WITH TotCut + m.TotQty
          UNLOCK
          =gfTraceKey('ORDHDR','O'+ORDER,'M')
          SELECT (lcCutPick)
          =RLOCK()
          REPLACE nSteps WITH 1
          UNLOCK
        ENDIF
        SELECT ORDLINE
        =SEEK('O'+m.Order+m.cOrdLine)
        IF &lcCutPick..nSteps < 2
          =RLOCK()
          REPLACE Dyelot WITH m.Dyelot     ,;
	              Cut1   WITH Cut1 + m.Qty1,;
	              Cut2   WITH Cut2 + m.Qty2,;
	              Cut3   WITH Cut3 + m.Qty3,;
	              Cut4   WITH Cut4 + m.Qty4,;
                  Cut5   WITH Cut5 + m.Qty5,;
	              Cut6   WITH Cut6 + m.Qty6,;
	              Cut7   WITH Cut7 + m.Qty7,;
	              Cut8   WITH Cut8 + m.Qty8,;
                  TotCut WITH TotCut + m.TotQty
          UNLOCK
          =gfTraceKey('ORDLINE','O'+ORDER+STR(LINENO,6),'M')
          SELECT (lcCutPick)
          =RLOCK()
          REPLACE nSteps WITH 2
          UNLOCK
        ENDIF
        m.CTKTNO     = lcPONumber
        m.cTktLineNo = STR(POSLn.LineNo,6)
        IF &lcCutPick..nSteps < 3
          INSERT INTO CUTPICK FROM MEMVAR
          =gfTraceKey('CUTPICK',m.trancd+m.ctktno+m.order+m.style+m.cordline,'A')
          SELECT (lcCutPick)
          =RLOCK()
          REPLACE nSteps WITH 3
          UNLOCK
        ENDIF
      ENDSCAN
    ENDIF  
    REPLACE ALL CTKTNO WITH lcPONumber ;
            FOR CTKTNO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+CWARECODE+DYELOT =;
                lcPO+lcDivision+lcPurCode+lcGrade+POSLn.Style+;
                POSLn.cWareCode+POSLn.Dyelot AND TotQty > 0
  ENDSCAN

  IF !SEEK('P'+lcPONumber,'POSHDR')
    SELECT (lcPOH)
    SCATTER MEMVAR
    m.PO = lcPONumber
    m.cMultiLot= 'S'
    =SEEK(m.cWarecode,'WAREHOUS')
    m.cOutAddr1 = WareHous.cAddress1
    m.cOutAddr2 = WareHous.cAddress2
    m.cOutAddr3 = WareHous.cAddress3
    m.cOutAddr4 = WareHous.cAddress4
    m.cOutAddr5 = WareHous.cAddress5
    m.ShpName   = WareHous.cDesc
    m.Available = COMPLETE
    m.Open      = nStyOrder
    m.cStyType  = "P"
    m.Contact   = lcCont
    m.Phone     = lcPhone
    m.POTOTAL   = NICOST1+NICOST2+NICOST3+NICOST4+NICOST5
    m.cAdd_User = gcUser_id
    m.cAdd_Time = TIME()
    m.dAdd_Date = gdSysDate
    =RLOCK()
    REPLACE nSteps WITH 1
    UNLOCK
    INSERT INTO POSHDR FROM MEMVAR
    laData[1]  = POSHDR.PO
    =gfTraceKey('POSHDR','P'+lcPONumber,'A')
    =gfTraceKey('POSLN','P'+lcPONumber+cRsession+Shipno+STYLE+STR(LINENO,6)+Trancd,'A')
  ELSE
    IF &lcPOH..nSteps < 1
      SELECT POSHDR
      laData[1]  = POSHDR.PO
      =RLOCK()
      REPLACE FLAG   WITH '' ,;
        TotOrd WITH TotOrd + &lcPOH..TotOrd
      UNLOCK
      SELECT (lcPOH)
      =RLOCK()
      REPLACE nSteps WITH 1
      UNLOCK
      =gfTraceKey('POSHDR','P'+lcPONumber,'M')
    ENDIF
  ENDIF
  *--- Get the module seting for generating cost sheet
  *--- if this seting is alwayes ("A") we will call function to call
  *--- C/T,PO's Cost Sheet program to generate Cost Sheet.
  lnGPoDim = lnGPoDim + 1
  DIME laGenPO[lnGPoDim]
  laGenPO[lnGPoDim] = laData[1]  
  *--- Creat Po Cost Sheet
  glFirsTime = .T.
  lcCrtCSH   = 'T' 
  lcAType    = 'P'
  lcPrvBasWn = gcBaseWind 
  lcProg_Nam = 'AWRPOCSSH'
  gcBaseWind = ALLTRIM(UPPER(IIF(' ' $ lcProg_Nam,;
                    SUBSTR(lcProg_Nam,1,ATC(' ',lcProg_Nam)-1),lcProg_Nam)))
  gnProgCopy = 0
  FOR lnAscii = 65 TO 90
    gnProgCopy = gnProgCopy + 1
    gcBaseWind =STUFF(gcBaseWind,2,1,CHR(lnAscii))
    IF WVISIBLE(gcBaseWind)
      LOOP
    ELSE
      EXIT 
    ENDIF
  ENDFOR
  lcParameter = "'" + laData[1] + "'" + ",.T."
  =gfRunProg("AWRPOCSSH",lcParameter)
  gcBaseWind = lcPrvBasWn
  
  *-- Receive the PO
  =lfOpenFile()
  =SEEK('I'+laData[1],"CTktBom")  
  lcLinkCode = CTktBom.Link_Code
  llDyelot   = (gfGetMemVar('M_Dyelot') ='Y')
  llLinkToGl = (gfGetMemVar('M_Link_GL') = 'Y')
  lcDropLoc  = gfGetMemVar('M_Link_GL')
  llSOInstld = (OCCURS('SO',gcComp_mdl)<>0)
  llMFCall   = .F.
  llContinue = .F.
  lcPType    = 'I'
  lcUnCmPgId = 'RECIVING_'+lcPType
  lcSession  = gfsequence('cSession')
  lcAProgm   = 'IMPDLDT'
  lcOthrTrCd = '4'
  STORE gdSysDate TO ldRcvDate,ldTrDate
  STORE ' ' TO lcGlPeriod,lcGLFYear
  IF llLinkToGl
    lcGlDist = gfTempName()   
  ENDIF
  lcMasterLn = 'POSLN'
  STORE '' TO lcErrorM1,lcErrorM2,lcAddMes1,lcAddMes2
  IF CHECKPRD(ldTrDate,'lcGLFYear','lcGLPeriod','PO')

    *khalid 09/17/2002 (Start)
    DIME laECost[5]
    laECost = 0
    STORE "" TO lcCur1,lcCur2
    STORE 1   TO lnCurrUnt1,lnCurrUnt2,lnCrRt1,lnCrRt2    
    *khalid 09/17/2002 (End)

    =lfPreFile()
    =lfSaveRec()
  ELSE
    =FPUTS(lnFileHNDL,lcErrorM1+lcErrorM2+lcAddMes1+lcAddMes2)
  ENDIF  
ENDSCAN

SELECT (lcPOH)
REPLACE ALL PO WITH cTmpPo
GO TOP

RETURN llUpdPo

*!*************************************************************
*! Name      : lfvDefVen
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/03/2002
*! Purpose   : Validation of the vendor at the PO defaults screen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvDefVen()
*!*************************************************************
FUNCTION lfvDefVen

lnAlias = SELECT()

*-- If the enterd vendor does not ecxit in the apvendor file
IF llBrowse OR (!EMPTY(lcVendor) AND !SEEK(lcVendor,'APVENDOR')) .OR. ;
    !('S' $ APVENDOR.cVenSupTyp .OR. 'C' $ APVENDOR.cVenSupTyp)
  =gfApVnBrow(@lcVendor,.F.,'CS')
ENDIF
llBrowse = .F.

IF !EMPTY(lcVendor)
  lcVenName = ApVendor.cVenComp
  SHOW GET lcVenName
  IF SEEK('N'+'CTERMCODE '+APVENDOR.CTERMCODE,'CODES')
    = gfwCodePop(@laCodInfo, "CTERMCODE", "L")
    lnTerm = ASUBSCRIPT(laTerm,ASCAN(laTerm,APVENDOR.CTERMCODE),1)
    SHOW GET lnTerm
  ELSE
    = gfwCodePop(@laCodInfo, "CTERMCODE", "D")
  ENDIF
  *-- if the system id set to use multi currency
  IF llMulCurr
    lcPCurr = ApVendor.ccurrcode
    
    lcDcurr = gcBaseCurr
    
    lnPRate = gfchkrate('lnUnit1',lcPCurr ,gdSysDate,llEditExRt,gcAct_Comp,.F.)
    lnDRate = gfchkrate('lnUnit2',lcDcurr ,gdSysDate,llEditExRt,gcAct_Comp,.F.)
    *-- if there is no valid rate for the entered currency,give a message
    *-- (No valid rate .default to the base currency)
    IF lnPRate <= 0
      =gfModalGen('INM36004B36001','DIALOG','price') = 1
      lcPCurr = gcBaseCurr
      lnPRate = 1
    ENDIF
    IF lnDRate <= 0
      =gfModalGen('INM36004B36001','DIALOG','duty') = 1
      lcDCurr = gcBaseCurr
      lnDRate = 1
    ENDIF
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvWareHs
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/03/2002
*! Purpose   : This function is to valid Warehouse code in WareHous File.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvWareHs()
*!*************************************************************
FUNCTION lfvWareHs

IF llBrowse OR !SEEK(lcWareCode,'WareHous')
  llBrowse = .F.
  lcWareCode = gfBrowWare( .F. )
ENDIF  

*!*************************************************************
*! Name      : lfPreFile
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/03/2002
*! Purpose   : Prepare the needed files to receive PO
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfPreFile()
*!*************************************************************
FUNCTION lfPreFile


=SEEK('P'+laData[1],'POSHDR')

SELECT POSLN
=SEEK('P'+laData[1])
SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = 'P'+laData[1]
  SCATTER MEMVAR MEMO
  INSERT INTO (lcTempFile) FROM MEMVAR
  STORE 1 TO lnCrRt1,lnCrRt2

  IF llMulCurr
    lcCur1  = POSHDR.cPriceCur
    lnCrRt1 = IIF(lcCur1=gcBaseCurr,1,gfChkRate('lnCurrUnt1',lcCur1,ldRcvDate,llEditExRt,gcAct_comp,.F.)) 
    lcCur2  = POSHDR.cDutyCur
    lnCrRt2 = IIF(lcCur2=gcBaseCurr,1,gfChkRate('lnCurrUnt2',lcCur2,ldRcvDate,llEditExRt,gcAct_comp,.F.)) 
    *--Cannot accept zero or -ve rates.
    IF lnCrRt1=0 OR lnCrRt2=0
      IF !llEditExRt
       =FPUTS(lnFileHNDL,'Style : ' + m.Style + ' in PO : '+ laData[1] +;
                      ' has currency with zero rate, it will be ignored.')
        LOOP
      ELSE
        STORE 1 TO lnCrRt1,lnCrRt2
      ENDIF
    ENDIF
  ENDIF
    
  m.nLanPrRat = lnCrRt1
  m.nLanDuRat = lnCrRt2
  m.cLotNo    = lfSelLots(STR(POSLN.LineNo,6),POSHDR.cLastOpr)
  m.TotStk   = m.TotQty
  STORE 0 TO m.TotDam, m.TotCan, m.TotBal
  m.lautomode = .T.
  INSERT INTO (lcTmpline) FROM MEMVAR
  m.Trancd = '2'
  STORE .F. TO m.lnewln, m.lalochg, m.lnewlupd, m.lcostmade 
  STORE 0   TO m.nlineno, m.nsteps 
  INSERT INTO (lcTmpline) FROM MEMVAR
ENDSCAN


*!*************************************************************
*! Name     : lfOpenFile
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose  : Read line information.
*!*************************************************************
FUNCTION lfOpenFile

= gfOpenFile(gcDataDir+'APVENDOR',gcDataDir+'VENCODE','SH')
= gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
= gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
= gfOpenFile(gcDataDir+'PIKTKT',gcDataDir+'PIKTKT','SH')
= gfOpenFile(gcDataDir+'PIKLINE',gcDataDir+'PIKLINE','SH')
= gfOpenFile(gcDataDir+'ORDLINE',gcDataDir+'ORDLINES','SH')
= gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH')
= gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
= gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLN','SH')
= gfOpenFile(gcDataDir+'BOM',gcDataDir+'BOM','SH')
= gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTPICK','SH')
= gfOpenFile(gcDataDir+'CTKTBOM',gcDataDir+'CTKTBOM','SH')
= gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')
= gfOpenFile(gcDataDir+'CODES',gcDataDir+'CCODE_NO','SH')
= gfOpenFile(gcDataDir+'Bom',gcDataDir+'Bom','SH')
= gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
= gfOpenFile(gcDataDir+'WhsLoc',gcDataDir+'WhsLoc','SH')
= gfOpenFile(gcDataDir+'BOMLINE',gcDataDir+'BOMLINE','SH')
= gfOpenFile(gcDataDir+'UNCMSESS',gcDataDir+'TRANS','SH')
= gfOpenFile(gcDataDir+'FABDYE',gcDataDir+'FABDYE','SH')
= gfOpenFile(gcDataDir+'MFGOPRDT',gcDataDir+'MFGOPRDT','SH')

*:*************************************************************
*! Name     : lfSaveRec
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose  : Post receinving (Save and Update).
*:*************************************************************
FUNCTION lfSaveRec
PRIVATE lnCurAlias , lcPosOrd , lnPoshPOS , lnPoslPos , lnStyPos , lnTmpPos ;
                   , lcTmpOrd , lnApvenPos
R_Width = ''       && variabel defined in the report                   

SELECT (lcTmpLine)
lnCrRec = IIF(EOF(),0,RECNO())

WAIT WINDOW 'Posting all transactions ...' NOWAIT
STORE .T. TO llCSave,llCUpdate,llUpdated,llShow
= gfOpenFile(gcDataDir+"APVENDOR","Vencode","SH")

*--Open GLDIST file to call G/L Distribution Procedure.
IF llLinkToGl AND ! llContinue
  = gfOpenFile(gcDataDir+'GLDIST','GlDistAc','SH')
  SELECT GLDist
  =AFIELDS(laFStru)
  =gfCrtTmp(lcGlDist,@laFStru)
ENDIF

*--Session no. variable.
IF ! llContinue
  lcGlSession = gfsequence('GLSESSION')
ENDIF

*--Srart Update lines except Issue inter Location P/o.
DIME laOpnQty[8]
STORE 0   TO lnLstPoRAm,lnOpnPoAmt,laOpnQty
STORE ' ' TO lcLstTrn,lcPoVend,lcOrjWareH
STORE ' ' TO lcSvCode,lcSvStyle
lcSTKSgn= IIF( lcPType $ 'R' , '-' , '+' )
lcLotNo = SPACE(2)
lnNxtStp= 2

SELECT BOMLINE
PRIVATE laBomLineF, TmpBomLine
TmpBomLine = gfTempName()

DECLARE laBomLineF[1,4]
=AFIELDS(laBomLineF)
CREATE CURSOR (TmpBomLine) FROM ARRAY laBomLineF

*--Update lines for Cutting Ticket or Purchase Order.
SELECT (lcTmpLine)
SET ORDER TO TAG TmpLine3
GO TOP
SCAN FOR TranCd <> '1' AND TotQty <> 0
  WAIT WINDOW 'Posting all transactions ... Style :'+Style NOWAIT
  
  *--Stock line is receive to Main Stock or receive to Others Stock.
  llStkLine = ( TranCd='2' OR TranCd = lcOthrTrCd )

  *--Read Saved variables.
  lcSvCode  = Po
  lcHdFile  = 'POSHDR'
  lcSvTpe   = 'P'
  *hbg
  =SEEK(&lcTmpLine..Style,'STYLE') AND SEEK('P'+&lcTmpLine..po+&lcTmpLine..Style,'POSLN')
  lcDyelot = IIF(llDyelot AND STYLE.cDye_Flg = 'Y',POSLN.Dyelot,'')
  *hbg
  *--Compute previous open balance on style P/o or C/t.----------------
  lcMainKy = lcSvTpe+PO+Style+STR(LineNo,6)
  lcOrjWareH=lfGetOpen(lcMainKy)

  *--Update BomLine (Step 1)--------------------------------------------
  SELECT (lcTmpLine)

  IF llStkLine
    lcBomPTyp = IIF(lcPType='S','S','I')
    lcBomPKey = Po+STR(LineNo,6)

    *--Create new records in BOMLINE file with type '2' if it is not found.
    IF &lcTmpLine..nSteps < 1 
      DO gpCrtBom WITH lcBomPTyp,lcBomPKey,Style,IIF(lcPType='S',ShipNo,''),;
                       lcGlSession,&lcTmpLine..cStyGrade
      SELECT (lcTmpLine)
      =RLOCK()
      REPLACE nSteps WITH 1
      UNLOCK
    ENDIF 

    *--Calculate landed costs case of detail costing. 
    =lfGetLanded()

  ENDIF

  *--New Receiving Cost.
  SELECT (lcTmpLine)
  lnELanded = nELanCost1
  IF lnELanded = ROUND(Gros_Price*(1-Disc_Pcnt/100),3)
     lnELanded = Gros_Price*(1-Disc_Pcnt/100)
  ENDIF

  lnNewCost = lnELanded+nELanCost2+nELanCost3+nELanCost4+nELanCost5
  =SEEK(&lcTmpLine..Style,'STYLE')

  *--Read the G/L link code that will be used to create GL entres.
  IF llStkLine
    IF &lcTmpLine..Trancd = lcOthrTrCd
      =SEEK(&lcTmpLine..cRetSty,'STYLE')
    ENDIF  
    lclinkCode = IIF(!EMPTY(STYLE.link_code),STYLE.link_code,'DEFDEF')
    IF &lcTmpLine..Trancd = '2'
      =SEEK(&lcTmpLine..Style  +&lcTmpLine..cWareCode+SPACE(10),'STYDYE')
    ELSE        && Trancd = lcOthrTrCd
      =SEEK(&lcTmpLine..cRetSty+&lcTmpLine..cWareCode+SPACE(10),'STYDYE')
    ENDIF  
    lclinkCode=IIF(!EMPTY(STYDYE.Gl_link),STYDYE.Gl_link,lclinkcode)
  ENDIF
  lcGLPLkC=IIF(SEEK(lcSvTpe+lcSvCode,lcHdFile),&lcHdFile..Link_Code,'DEFDEF')

  lcCurSty = STYLE.STYLE
  = SEEK(&lcTmpLine..Style,'STYLE')

  *--Create StyDye Dyelot record in C/t or PO if new for receiving warehouse.------
  IF llStkLine AND llDyelot AND Style.cDye_Flg = 'Y'
    IF &lcTmpLine..Trancd = '2'
      llFound=SEEK(&lcTmpLine..Style  +&lcTmpLine..cWareCode+&lcTmpLine..Dyelot,'STYDYE')
    ELSE    && Trancd = lcOthrTrCd
      llFound=SEEK(&lcTmpLine..cRetSty+&lcTmpLine..cWareCode+&lcTmpLine..Dyelot,'STYDYE')
    ENDIF  

    IF ! llFound
      SELECT STYDYE
      APPEND BLANK
      REPLACE Style     WITH IIF(&lcTmpLine..Trancd='2',&lcTmpLine..Style,&lcTmpLine..cRetSty),;
              cWareCode WITH &lcTmpLine..cWareCode,;
              Dyelot    WITH &lcTmpLine..Dyelot
      =gfAdd_Info('STYDYE')
      *--Call TraceKey global function.
     =gfTraceKey('STYDYE',STYDYE.style+STYDYE.cwarecode+STYDYE.dyelot,'A')
    ENDIF
  ENDIF

  SELECT (lcTmpLine)

  *--Create FabDye Dyelot record in P/O if new for receiving warehouse.------
  IF !lCostMade AND llStkLine AND llDyelot AND Style.cDye_Flg = 'Y'
    lcUpDatFab = ''
    IF &lcTmpLine..Trancd = '2'
      lcFabColor = PADR(lfGetColor(&lcTmpLine..Style),6)    && Get Fabric color
      = lfUpDatFab(lcUpDatFab,lcFabColor,&lcTmpLine..Dyelot,&lcTmpLine..cWareCode)
    ELSE    && Trancd = lcOthrTrCd
      lcFabColor = PADR(lfGetColor(&lcTmpLine..cRetSty),6)   && Get Fabric color
      = lfUpDatFab(lcUpDatFab,lcFabColor,&lcTmpLine..Dyelot,&lcTmpLine..cWareCode)
    ENDIF  
  ENDIF
  = SEEK(lcCurSty,'STYLE')

  *--Update Style Inventory (Start step 2 & Next Step lnNxtStp)---------------
  lnNxtStp = 2

  *--Update Style (Step lnNxtStp)-----------------------------------------------
  =SEEK(&lcTmpLine..Style,'STYLE')
  IF &lcTmpLine..nSteps < lnNxtStp  AND ! &lcTmpLine..lNewLUpd
    lcWipSgn = IIF(lcPType $ 'R','+','-')
    SELECT STYLE
    =RLOCK()
    REPLACE Wip1   WITH Wip1 &lcWipSgn MIN(&lcTmpLine..Qty1, laOpnQty[1]) ,;
            Wip2   WITH Wip2 &lcWipSgn MIN(&lcTmpLine..Qty2, laOpnQty[2]) ,;
            Wip3   WITH Wip3 &lcWipSgn MIN(&lcTmpLine..Qty3, laOpnQty[3]) ,;
            Wip4   WITH Wip4 &lcWipSgn MIN(&lcTmpLine..Qty4, laOpnQty[4]) ,;
            Wip5   WITH Wip5 &lcWipSgn MIN(&lcTmpLine..Qty5, laOpnQty[5]) ,;
            Wip6   WITH Wip6 &lcWipSgn MIN(&lcTmpLine..Qty6, laOpnQty[6]) ,;
            Wip7   WITH Wip7 &lcWipSgn MIN(&lcTmpLine..Qty7, laOpnQty[7]) ,;
            Wip8   WITH Wip8 &lcWipSgn MIN(&lcTmpLine..Qty8, laOpnQty[8]) ,;
            TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
    UNLOCK 
    SELECT (lcTmpLine)
    =RLOCK()
    REPLACE nSteps WITH lnNxtStp
    UNLOCK
    *--Call TraceKey global function.
    =gfTraceKey('STYLE',STYLE.Style,'M')
  ENDIF 


  *--Update StyDye Warehouse record (Step lnNxtStp+1)-------------------------------
  SELECT STYDYE
  =SEEK(&lcTmpLine..Style+lcOrjWareH+SPACE(10))
  IF &lcTmpLine..nSteps < lnNxtStp+1  AND ! &lcTmpLine..lNewLUpd
    lcWipSgn = IIF(lcPType $ 'R','+','-')
    =RLOCK()
    REPLACE Wip1   WITH Wip1 &lcWipSgn MIN(&lcTmpLine..Qty1, laOpnQty[1]),;
            Wip2   WITH Wip2 &lcWipSgn MIN(&lcTmpLine..Qty2, laOpnQty[2]),;
            Wip3   WITH Wip3 &lcWipSgn MIN(&lcTmpLine..Qty3, laOpnQty[3]),;
            Wip4   WITH Wip4 &lcWipSgn MIN(&lcTmpLine..Qty4, laOpnQty[4]),;
            Wip5   WITH Wip5 &lcWipSgn MIN(&lcTmpLine..Qty5, laOpnQty[5]),;
            Wip6   WITH Wip6 &lcWipSgn MIN(&lcTmpLine..Qty6, laOpnQty[6]),;
            Wip7   WITH Wip7 &lcWipSgn MIN(&lcTmpLine..Qty7, laOpnQty[7]),;
            Wip8   WITH Wip8 &lcWipSgn MIN(&lcTmpLine..Qty8, laOpnQty[8]),;
            TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8
    UNLOCK 
    SELECT (lcTmpLine)
    =RLOCK()
    REPLACE nSteps WITH lnNxtStp+1
    UNLOCK
    *--Call TraceKey global function.
   =gfTraceKey('STYDYE',STYDYE.style+STYDYE.cwarecode+STYDYE.dyelot,'M')
  ENDIF

  *-Update APVendor (Step lnNxtStp+2)-------------------------------
  IF SEEK(&lcTmpLine..Vendor,'APVENDOR') 
    SELECT (lcTmpLine)
    IF lcLstTrn <> Po AND lcPoVend <> Vendor
      STORE 0 TO lnLstPoRAm,lnOpnPoAmt
      lcLstTrn  = Po
      lcPoVend = Vendor
    ENDIF
    lnLstPoRAm = lnLstPoRAm + (TotQty * nELanCost1)
    =SEEK('P'+Po+Style+STR(LineNo,6)+'1','POSLn')
    lnOpnPoAmt = ( MIN((TotQty * nELanCost1),(POSLn.TotQty*POSLn.nECost1)) )
 
    IF &lcTmpLine..nSteps < lnNxtStp+2
      SELECT APVENDOR
      =RLOCK()
      REPLACE dVenLPoRD WITH ldRcvDate  ,;
              nVenLPoRA with lnLstPoRAm ,;
              nVenOpnPo with nVenOpnPo - lnOpnPoAmt
      UNLOCK
      SELECT (lcTmpLine)
      =RLOCK()
      REPLACE nSteps WITH lnNxtStp+2
      UNLOCK    
      *--Call TraceKey global function.
      =gfTraceKey('APVENDOR',APVENDOR.cvendcode,'M')
    ENDIF
  ENDIF


  *--Update Header file CuttktH or POSHDR (Step lnNxtStp+3) --------------------------
  lcHdFile='POSHDR'
  SELECT (lcHdFile)
  =SEEK(lcSvTpe+lcSvCode)
  lcGLPLkC = Link_Code

  lnOpnSub=0
  FOR I=1 TO 8
    lcCnt=STR(I,1)
    lnOpnSub = lnOpnSub + MIN( laOpnQty[I] , &lcTmpLine..Qty&lcCnt )
  ENDFOR
  lcFrstSess = ''

  IF &lcTmpLine..nSteps < lnNxtStp+3
    =RLOCK()
    DO CASE
      CASE llStkLine
        IF &lcTmpLine..Trancd = '2'
           REPLACE Receive WITH Receive + &lcTmpLine..TotQty,;
                   Open    WITH MAX(Open - lnOpnSub,0)
          
        ELSE     &&Trancd = lcOthrTrCd
           REPLACE Damage  WITH Damage  + &lcTmpLine..TotQty,;
                   Open    WITH MAX(Open - lnOpnSub,0)

        ENDIF
        REPLACE nFlanCost2 WITH nFlanCost2 +(ROUND(&lcTmpLine..nlan_cst2 * &lcTmpLine..TotQty,3)),;
                nFlanCost3 WITH nFlanCost3 +(ROUND(&lcTmpLine..nlan_cst3 * &lcTmpLine..TotQty,3)),;
                nFlanCost4 WITH nFlanCost4 +(ROUND(&lcTmpLine..nlan_cst4 * &lcTmpLine..TotQty,3)),;
                nFlanCost5 WITH nFlanCost5 +(ROUND(&lcTmpLine..nlan_cst5 * &lcTmpLine..TotQty,3)),;
                nlan_cost2 WITH nlan_cost2 +(ROUND(&lcTmpLine..nElanCost2 * &lcTmpLine..TotQty,3)),;
                nlan_cost3 WITH nlan_cost3 +(ROUND(&lcTmpLine..nElanCost3 * &lcTmpLine..TotQty,3)),;
                nlan_cost4 WITH nlan_cost4 +(ROUND(&lcTmpLine..nElanCost4 * &lcTmpLine..TotQty,3)),;
                nlan_cost5 WITH nlan_cost5 +(ROUND(&lcTmpLine..nElanCost5 * &lcTmpLine..TotQty,3))
          IF &lcTmpLine..nElanCost1 = ROUND(&lcTmpLine..Gros_Price*(1-&lcTmpLine..Disc_Pcnt/100),3)
            REPLACE nFlanCost1 WITH nFlanCost1 +(ROUND(&lcTmpLine..nlan_cst1 * &lcTmpLine..TotQty,3)),;
                    nlan_cost1 WITH nlan_cost1 +(ROUND((&lcTmpLine..Gros_Price*(1-&lcTmpLine..Disc_Pcnt/100))* &lcTmpLine..TotQty,3))
          ELSE
            REPLACE nFlanCost1 WITH nFlanCost1 +(ROUND(&lcTmpLine..nlan_cst1 * &lcTmpLine..TotQty,3)),;
                    nlan_cost1 WITH nlan_cost1 +(ROUND(&lcTmpLine..nElanCost1 * &lcTmpLine..TotQty,3))
          ENDIF
          REPLACE nTot_Cost  WITH nlan_cost1+nlan_cost2+nlan_cost3+nlan_cost4+nlan_cost5


      CASE &lcTmpLine..Trancd = lcCanlTrCd
         REPLACE Cancel  WITH Cancel  + &lcTmpLine..TotQty,;
                 Open    WITH MAX((Open    - lnOpnSub),0)
    ENDCASE

    IF Open = 0
      REPLACE Status with 'C'
    ENDIF
    =gfAdd_Info(lcHdFile)
    UNLOCK
    SELECT (lcTmpLine)
    =RLOCK()
    REPLACE nSteps WITH lnNxtStp+3
    UNLOCK    
    *--Call TraceKey global function.
    =gfTraceKey('POSHDR',POSHDR.cStyType+POSHDR.Po,'M')
  ENDIF



  *--Update Line in CuttktL or PoSLn (lnNxtStp+4)---------------------------
  SELECT (lcTmpLine)
  IF &lcTmpLine..nSteps < lnNxtStp+4
    SCATTER MEMVAR
    SELECT (lcMasterLn)
    APPEND BLANK
    GATHER MEMVAR
    REPLACE Date      WITH ldRcvDate,;
            dPostDate WITH ldTrDate ,;
            cOwner    WITH ' '
    *--Changing receiving from customer to location.
    IF cWareCode <> lcDropLoc AND !EMPTY(Account)
       REPLACE Account WITH SPACE(5),;
               Store   WITH SPACE(8)
    ENDIF
    IF llStkLine
      REPLACE cRSession WITH lcGlSession
    ENDIF
    =gfAdd_Info(lcMasterLn)
    SELECT (lcTmpLine)
    =RLOCK()
    REPLACE nSteps WITH lnNxtStp+4
    UNLOCK     
    *--Call TraceKey global function.
    SELECT POSLN
    =gfTraceKey('POSLN',cStyType+Po+cRsession+Shipno+Style+STR(Lineno,6)+Trancd,'A')
  ENDIF


  *--Update BomLine (Step lnNxtStp+5)-------------------------------------------------
  SELECT (lcTmpLine)

  IF llStkLine
    lcBomLKey = 'I2'+IIF(lcPType='S',Shipno,'')+Po+STR(LineNo,6)    
    lcWhileCn = "cIMTyp+cType+IIF(lcPType='S',Shipno,'')+cTktNo+STR(LineNo,6)=lcBomLKey"

    SELECT BOMLINE
    SET ORDER TO TAG BomLine
    IF SEEK(lcBomLKey) AND &lcTmpLine..nSteps < lnNxtStp+5
      SCAN REST WHILE &lcWhileCn ;
                FOR Style = &lcTmpLine..Style AND EMPTY(cRSession) AND cStyGrade = &lcTmpLine..cStyGrade AND ;
                gfTraceKey('BOMLINE',cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode,'M')
        REPLACE cRSession WITH lcGlSession
        SCATTER MEMVAR
        STORE 0 TO m.StyQty,m.ItemQty,m.ItemAmt
        m.cRSession = ''
        SELECT (TmpBomLine)
        APPEND BLANK
        GATHER MEMVAR
      ENDSCAN
      
      SELECT (lcTmpLine)
      =RLOCK()      
      REPLACE nSteps WITH lnNxtStp+5
      UNLOCK    
    ENDIF
  ENDIF

  *--Update MfgOprDt file (Step lnNxtStp+6)--------------------------------------
  SELECT (lcTmpLine)
  lcLastOpr = clastopr
  IF !EMPTY(lcLastOpr) AND lcPType $ 'IMD' AND &lcTmpLine..nSteps < lnNxtStp+6
    SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laRecvQty
    *-- hend
    IF laRecvQty[9] > 0 
      =lfUpdLot(lcSvCode,lcLastOpr,&lcTmpLine..cLotNo,'laRecvQty',&lcTmpLine..Style+&lcTmpLine..Dyelot)
    ENDIF
    
    SELECT (lcTmpLine)
    =RLOCK()
    REPLACE nSteps WITH lnNxtStp+6
    UNLOCK    
  ENDIF
   

  *--Update Shipment In-Transit P/o line qty. ------------
  SELECT (lcTmpLine)
  
  
  *--Update Style Inventory (Start step 2 & Next Step lnNxtStp)---------------
  lnNxtStp = lnNxtStp + 10
  IF llStkLine
    *--G/L Array difinition and initialization.
    *-- Update general ledger entreis in gfStyCrl()
    IF llLinkToGl
      DECLARE laGLDistAr[2,13]
      laGLDistAr[1,1] = lcLinkCode
      laGLDistAr[2,1] = lcGLPLkC
      laGLDistAr[1,2] = '006'
      laGLDistAr[2,2] = '013'
      laGLDistAr[1,3] =  1
      laGLDistAr[2,3] = -1
      STORE IIF(llMFCall,'CT','PO') TO laGLDistAr[1,4],laGLDistAr[2,4]
      STORE lcSvCode   TO laGLDistAr[1,5],laGLDistAr[2,5]
      STORE ldTrDate   TO laGLDistAr[1,6],laGLDistAr[2,6]
      STORE lcGLFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
      STORE lcGlPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
      STORE lcGlDist   TO laGLDistAr[1,9],laGLDistAr[2,9]
    ELSE
      DIME laGLDistAr[1,1]
      laGLDistAr = ''
    ENDIF

    SELECT (lcTmpLine)
    lcJTType = IIF(llMFCall,'5','6') 
    lcJrlSty = IIF(TranCd = lcOthrTrCd,cRetSty,Style)
    lcJDyelt = IIF(llDyelot,Dyelot,SPACE(10))     
    
    lnJSgn   = IIF(lcPType $ 'R',-1,1)
    DECLARE laAdjust[9]
    FOR I = 1 TO 8
      Z=STR(I,1)
      laAdjust[I] = (lnJSgn*Qty&Z)
    ENDFOR
    laAdjust[9] = (lnJSgn*TotQty)

    *--Call the global function for update style inventory control.

    PRIVATE lcRefer
    lcRefer = "VEN. " + APVendor.cVendCode + APVendor.cVenComp
    lnNxtStp = gfStyCrl(lcJTType,lcJrlSty,cWareCode,lcJDyelt,ldRcvDate,;
                   lcSvCode,@laAdjust,lnNewCost,lcRefer,lcGlSession,'',;
                   lnNxtStp,lcTmpLine,'nSteps',@laGLDistAr,;
                   &lcTmpLine..LineNo,lcFrstSess)    
  ENDIF
ENDSCAN

*--Header steps will use the first line nsteps field.
SELECT (lcTmpLine)
GO TOP

*--Update GLDist (lnNxtStp + 10)-------------------------------------------------
IF llLinkToGl AND &lcTmpLine..nSteps < lnNxtStp + 10
  WAIT WINDOW 'Updating General Ledger Distribution File ' NOWAIT
  SELECT (lcGlDist)
  REPLACE ALL GlSession with lcGlSession
  USE
  SELECT GlDist
  APPEND FROM (gcWorkDir+lcGlDist)
  ERASE (gcWorkDir+lcGLDist+'.DBF')
  ERASE (gcWorkDir+lcGLDist+'.CDX')

  SELECT (lcTmpLine)
  =RLOCK()
  REPLACE nSteps WITH lnNxtStp + 10
  UNLOCK  
ENDIF
WAIT CLEAR

*--Update BomLine for empty session no (lnNxtStp + 11)-----------------------------
IF &lcTmpLine..nSteps < lnNxtStp+11
  =gfOpenFile(gcDataDir+'BOMLINE','BOMLINE','SH')
  SELECT BOMLINE
  SET ORDER TO TAG BomLine
  SELECT (lcTmpLine)
  SCAN
    lcLKey = IIF(llMFCall,'M2'+&lcTmpLine..Cuttkt,'I2'+&lcTmpLine..Po)
    IF SEEK(lcLKey,'BOMLINE')
      SELECT BomLine
      SCAN REST WHILE cIMTyp+cType+cTktNo=lcLKey ;
           FOR EMPTY(cRSession) AND gfTraceKey('BOMLINE',cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode,'D')
         SCATTER MEMVAR
         DELETE
         SELECT (TmpBomLine)
         APPEND BLANK
         GATHER MEMVAR
      ENDSCAN
    ENDIF
  ENDSCAN

  SELECT (lcTmpLine)
  GO TOP
  =RLOCK()
  REPLACE nSteps WITH lnNxtStp + 11
  UNLOCK  
ENDIF

IF llStkLine
  SELECT (TmpBomLine)
  SCAN
    SCATTER MEMVAR
    SELECT BOMLINE
    APPEND BLANK
    GATHER MEMVAR
  ENDSCAN
  USE IN (TmpBomLine)
ENDIF

*--Update allocation quantity if changed the link.------------------
IF llSOInstld AND !(lcPType$'R') AND USED(lcTmpCtPk)
  SELECT (lcTmpLine)
  SCAN FOR TranCd='1' AND lAloChg
    SCATTER FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd TO laNewOrd
    =SEEK(cStyType+Po+Style+STR(LineNo,6)+TranCd,'POSLN') 
    SELECT (lcMasterLn)
    GATHER FROM laNewOrd FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
    SELECT (lcTmpLine)
    REPLACE lAloChg WITH .F.
  ENDSCAN
  = gfOpenFile(gcDataDir+"OrdHdr","OrdHdr","SH")  
  = gfOpenFile(gcDataDir+"OrdLine","OrdLine","SH")  

  SELECT (lcTmpCtPk)
  SCAN
    lcCpkky=Trancd+cTktNo+Style
    SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laNewOrd
    SCATTER FIELDS nCurPck1,nCurPck2,nCurPck3,nCurPck4,nCurPck5,nCurPck6,nCurPck7,nCurPck8,nCurPck9 TO laOldOrd
    SELECT CUTPICK
    SEEK lcCpkky
    LOCATE REST WHILE Trancd+cTktNo+Style=lcCpkky FOR cOrdLine=&lcTmpCtPk..cOrdLine
    IF FOUND()
      GATHER FROM laNewOrd FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty
      =SEEK('O'+CUTPICK.Order,'ORDHDR')
      =SEEK('O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'ORDLINE')    
      FOR lnSizeNo = 1 TO 8
        lcSz = STR(lnSizeNo,1)
        REPLACE ORDHDR.TotCut    WITH ORDHDR.TotCut    - laOldOrd[lnSizeNo] + laNewOrd[lnSizeNo]
        REPLACE ORDLINE.TotCut   WITH ORDLINE.TotCut   - laOldOrd[lnSizeNo] + laNewOrd[lnSizeNo],;
                ORDLINE.Cut&lcSz WITH ORDLINE.Cut&lcSz - laOldOrd[lnSizeNo] + laNewOrd[lnSizeNo]
      ENDFOR
      *--Call TraceKey global function.
      =gfTraceKey('ORDHDR','O'+CUTPICK.Order,'M')
      =gfTraceKey('ORDLINE','O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'M')
      SELECT CUTPICK
      IF CUTPICK.TotQty = 0
        *--Call TraceKey global function DELETE.
        =lfTraceTag('CUTPICK','D')
        SELECT CUTPICK
        =RLOCK()
        DELETE
        UNLOCK 
      ELSE
        *--Call TraceKey global function MODIFY.
        =lfTraceTag('CUTPICK','M')
      ENDIF  
    ENDIF
  ENDSCAN
ENDIF


*--Update WhsLoc----------------------------------------------------
IF llWareLoc 
  SELECT WHSLOC
  SET ORDER TO TAG WhsLocSt
  SELECT (lcTemLoc)
  SCAN FOR !EMPTY(clocation)
    IF !SEEK(Style+SPACE(6)+cWareCode+cLocation,'WHSLOC')
      SCATTER MEMVAR
      INSERT INTO WhsLoc FROM MEMVAR
      REPLACE cOwner WITH ' '
      =gfAdd_Info('WhsLoc')
      =gfTraceKey('WHSLOC',WHSLOC.cwarecode+WHSLOC.clocation+WHSLOC.style,'A')
    ENDIF
  ENDSCAN
ENDIF


*--Complete unCmSess record ,Session completed.
IF 'MA' $ gcComp_mdl AND llDyelot
  = gfArDyRl('' , '' , lcTmDyeRel,.T.)
ENDIF
IF USED(lcTempFile)
  SELECT (lcTempFile)
  ZAP
  STORE ' ' TO lcTCodeOld
ENDIF  

SELECT (lcTmpLine)
SET ORDER TO TAG TmpLine1
DELETE ALL

RETURN


*:*************************************************************
*! Name    : lfGetOpen
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Get original open quantity on P/o and return
*!           the original warehouse.
*:*************************************************************
FUNCTION lfGetOpen
PARA lcParmKey
IF EMPTY(lcDyelot)
  lcParmKey = ALLTRIM(lcParmKey)
ENDIF

lnAlias = SELECT()
SELECT (lcMasterLn)
PRIVATE lnMsLnRNo 
lnMsLnRNo = RECNO()
SEEK lcParmKey

lcOrjWareH = cWareCode
laOpnQty   = 0
lcWhlCndn  = "cStyType+PO+Style+STR(LineNo,6)"+ "=lcParmKey"
lcBaseTrCd = '1'

SCAN REST WHILE &lcWhlCndn
  FOR I=1 TO 8
    lcCnt=STR(I,1)
    laOpnQty[I]= IIF(TranCd=lcBaseTrCd,laOpnQty[I]+Qty&lcCnt,;
                 IIF(lcPType $ 'IBSD' AND !EMPTY(DYELOT),laOpnQty[I]-Qty&lcCnt,;
                 IIF(lcPType ='O',laOpnQty[I]-Qty&lcCnt,MAX(laOpnQty[I]-Qty&lcCnt,0)))) 
  ENDFOR
ENDSCAN

SELECT (lcMasterLn)
IF BETWEEN(lnMsLnRNo,1,RECCOUNT(lcMasterLn))
 GO lnMsLnRNo 
ENDIF

SELECT(lnAlias)
RETURN (lcOrjWareH)


*:*************************************************************
*! Name     : lfGetLanded
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose  : Calculate landed costs case of detail costing. 
*:*************************************************************
FUNCTION lfGetLanded

SELECT BOMLINE
SET ORDER TO TAG BomLine

SELECT (lcTmpLine)
SCATTER FIELDS Qty1,qty2,qty3,qty4,qty5,qty6,qty7,qty8 TO laLnQty

FOR lnCnt=1 to 5
  lcCnt=STR(lnCnt,1)
  lcBomLKey = 'I2'+&lcTmpLine..Po+STR(&lcTmpLine..LineNo,6)+lcCnt+&lcTmpLine..Style
  lcWCondtn = "cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style = lcBomLKey"
  lcFCondtn = "(EMPTY(cRSession) OR cRSession=lcGlSession) AND cStyGrade = &lcTmpLine..cStyGrade"
  
  lnNLCs&lcCnt = 0
  lnCurSQt = 0
  SELECT BOMLINE
  IF SEEK(lcBomLKey)
    REPLACE REST StyQty  WITH lfBomSzQt(),;
                 ItemQty WITH (StyQty*UnitQty),;
                 ItemAmt WITH (ItemQty*UnitCost);
           WHILE &lcWCondtn FOR &lcFCondtn 
    =SEEK(lcBomLKey)
     SUM REST (UnitCost*UnitQty)*StyQty WHILE &lcWCondtn FOR &lcFCondtn TO lnNLCs&lcCnt
     lnNLCs&lcCnt = IIF(&lcTmpLine..TotQty<>0,(lnNLCs&lcCnt/&lcTmpLine..TotQty),0)
  ENDIF 
  
  SELECT (lcTmpLine)
  REPLACE nLan_Cst&lcCnt WITH lnNLCs&lcCnt
  IF ! llMFCall
    IF llMulCurr

      *khalid 09/17/2002 (Start)
      *=lfGetEqv(lcCnt,lnRate1,lnRate2,lnCurrUnt1,lnCurrUnt2,nLan_Cst1,nLan_Cst2,nLan_Cst3,nLan_Cst4,nLan_Cst5)
      =lfGetEqv(lcCnt,nLanPrRat,nLanDuRat,lnCurrUnt1,lnCurrUnt2,nLan_Cst1,nLan_Cst2,nLan_Cst3,nLan_Cst4,nLan_Cst5)
      *khalid 09/17/2002 (Start)
      
      REPLACE nELanCost&lcCnt WITH laECost[1]
    ELSE
      REPLACE nELanCost&lcCnt WITH lnNLCs&lcCnt
    ENDIF
  ENDIF
ENDFOR

SELECT (lcTmpLine)
RETURN

*:*************************************************************
*! Name    : lfBomSzQt
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : 
*:*************************************************************
FUNCTION lfBomSzQt
lnCurSQt = 0
FOR lnI=1 TO 8
  IF STR(lnI,1) $ BOMLINE.CSIZES
    lnCurSQt = lnCurSQt + laLnQty[lnI]
  ENDIF
ENDFOR
RETURN lnCurSQt

*!*************************************************************
*! Name      : lfTraceTag
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/31/1999
*! Purpose   : Trace cutpick file
*!*************************************************************
*! Calls       : gfOpenFile
*!*************************************************************
*! Passed Parameters : File Name, Communication status.
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfTraceTag()
*!*************************************************************
FUNCTION lfTraceTag
PARAMETERS lcFileNam,lcCommStat
PRIVATE lcPickVal,lnCurAlias
lnCurAlias = SELECT(0)

IF EMPTY(lcPickIndx)
  =gfOpenFile(gcSysHome+'SYDFILES','Cfile_nam','SH')
  IF SEEK(lcFileNam,'SYDFILES')
    lcPickIndx = SYDFILES.cFile_Tag
  ENDIF

  PRIVATE lnTagNo
  lnTagNo    = 1

  SELECT CUTPICK
  DO WHILE .T.
    IF ALLTRIM(TAG(lnTagNo)) = ALLTRIM(lcPickIndx)
      lcPickIndx = KEY(lnTagNo) 
      EXIT
    ENDIF
    lnTagNo = lnTagNo + 1
  ENDDO

ENDIF

lcPickVal  = EVALUATE(lcPickIndx)
=gfTraceKey(lcFileNam,lcPickVal,lcCommStat)
SELECT (lnCurAlias)
*-- end of lfTraceTag.

*!*************************************************************
*! Name      : lfGetEqv
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose   : Get eguevalent costs.
*!*************************************************************
FUNCTION lfGetEqv
PARA lcUpdCsts,lnPRate1,lnDRate2,lnCurUnt1,lnCurUnt2,;
     lnFCost1,lnFCost2,lnFCost3,lnFCost4,lnFCost5

DIME laECost[LEN(lcUpdCsts)]
lnPt = 1
IF '1' $ lcUpdCsts 
  laECost[lnPt] = lfvEquCost('1',lnFCost1,lnPRate1,lnCurUnt1)
  lnPt = lnPt + 1
ENDIF
IF '2' $ lcUpdCsts 
  laECost[lnPt] = lfvEquCost('2',lnFCost2,lnDRate2,lnCurUnt2)
  lnPt = lnPt + 1
ENDIF
IF '3' $ lcUpdCsts 
  laECost[lnPt] = lfvEquCost('3',lnFCost3,lnDRate2,lnCurUnt2)
  lnPt = lnPt + 1
ENDIF
IF '4' $ lcUpdCsts 
  laECost[lnPt] = lfvEquCost('4',lnFCost4,lnDRate2,lnCurUnt2)
  lnPt = lnPt + 1
ENDIF
IF '5' $ lcUpdCsts 
  laECost[lnPt] = lfvEquCost('5',lnFCost5,lnDRate2,lnCurUnt2)
  lnPt = lnPt + 1
ENDIF
RETURN

*!*************************************************************
*! Name      : lfUpDatFab
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/10/1998
*! Purpose   : Update FabDye File.
*!*************************************************************
*! Called from : lpSavScr
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : Color
*!*************************************************************
*! Example     : = lfUpDatFab()
*!*************************************************************
FUNCTION lfUpDatFab
PARAMETERS lcFabKey,lcClrKey,lcDyeKey,lcWareKey 
PRIVATE lcCurAlias,lcDyeOrder,lcFabOrder,lcMyFabKey
lcCurAlias = ALIAS()
lcDyeOrder = ORDER('FABDYE')
SET ORDER TO FABDYE IN FABDYE

lcFabOrder = ORDER('FABRIC')
lcMyFabKey = FABRIC.FABRIC + FABRIC.COLOR 
SET ORDER TO FABRIC IN FABRIC

SELECT FABDYE
*-- if you does not find location record in fabdye file, add it.
IF llWareHous AND !SEEK(lcFabKey+lcClrKey+lcWareKey)
  DO gpAdFabWar WITH lcFabKey,lcClrKey,SPACE(10),lcWareKey
  =gfTraceKey('FABDYE',lcFabKey+lcClrKey+SPACE(10)+lcWareKey,'A')
ENDIF  && end if you does not find location record in fabdye file, add it.

IF llDyelot AND llFabDye AND SEEK(lcFabKey + lcClrKey,'FABRIC') AND ;
   FABRIC.CDYE_FLG = 'Y' AND !SEEK(lcFabKey+lcClrKey+lcWareKey+lcDyeKey,'FABDYE')
  *-- add dyelot record and save tmpscope to arrange it later.
  DO gpAdFabWar WITH lcFabKey,lcClrKey,lcDyeKey,lcWareKey,lcTmDyeRel
  =gfTraceKey('FABDYE',lcFabKey+lcClrKey+lcDyeKey+lcWareKey,'A')
ENDIF              

SET ORDER TO &lcDyeOrder IN FABDYE

= SEEK(lcMyFabKey,'FABRIC')
SET ORDER TO &lcFabOrder IN FABRIC

SELECT (lcCurAlias)
RETURN
*-- end of lfUpDatFab.
*!*************************************************************
*! Name      : CheckPrd
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/10/1998
*! Purpose   : Function to validate transaction date
*!*************************************************************
*! Called from : lpSavScr
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : ldDate   : Transaction date to be check
*!                   : lcPeriod : Transaction Period
*!                   : lcFYear  : Transaction Fiscal Year 
*!                   : lcTranTyp: Type of transaction calls this function
*!*************************************************************
*! Return      : Color
*!*************************************************************
*! Example     : = CheckPrd()
*!*************************************************************
FUNCTION Checkprd
PARAMETERS ldDate,lcFYear,lcPeriod,lcTranTyp,llHideMsg

PRIVATE lcDType,lcAddMes1,lcAddMes2,lcSysDir,lcGlVers,lcGlComp, ;
        lcDate,llContinue,lcErrorM1,lcErrorM2, lnAlias
        
lnAlias = SELECT()
STORE '' TO M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO

=gfGetMemVar('M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO',gcAct_Comp)
lcSysDir   = ALLTRIM(M_SYS_DIR)
lcGlVers   = ALLTRIM(M_GL_VERS)
lcGlComp   = ALLTRIM(M_GL_CO)
STORE SPACE(1) TO lcDType,lcAddMes1,lcAddMes2

lcDate = DTOC(ldDate)      && Transaction date as a string used in messages
IF lcGlVers = 'S'            &&   <<<... SBT 2.5 ... >>>

  USE lcSysDir+'SYCCOMP' ORDER TAG 'COMPID' IN 0 AGAIN ALIAS 'SBTCOMP'
  =SEEK(lcGlComp,'SBTCOMP')
  =gfOpenFile(lcSysDir+'SYCHFIS',lcSysDir+'COMPID1','SH')
  =gfOpenFile(lcSysDir+'SYCDFIS',lcSysDir+'COMPID1','SH')

  llContinue = .T.
  IF SEEK(lcGlComp)
    LOCATE REST FOR BETWEEN(ldDate,Bdate,Edate) ;
                WHILE (ldDate >= Bdate) .AND. (CompId = lcGlComp)
  ENDIF
  IF !FOUND()                && No period match checked date
    llContinue = .F.
    lcErrorM1 = ' does not fall within any period. '
    lcErrorM2 = ''
  ELSE
    &lcFYear  = SUBSTR(Yearprd,1,4)      && Transaction date year
    &lcPeriod = SUBSTR(Yearprd,5,2)      && Transaction date period     
  ENDIF  
  IF llContinue .AND. Permlck         && Permanently locked period
    llContinue = .F.
    lcErrorM1 = ' falls in a permanently locked period.'
    lcErrorM2 = ''
  ENDIF  
  IF llContinue .AND. Plocked         && Locked period
    llContinue = .F.
    lcErrorM1 = ' falls in a locked period.'
    lcErrorM2 = ''
  ENDIF  
  IF llContinue              && So far so good
    IF Pclosed               && Closed period
      IF !(lcTranTyp $ 'VI2VR2')  && Transaction is neither 
                                  && 'Void invoice' nor 'void return'.
        llDummy =  FErrInfo(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
        lcErrorM1 = '&lcDType&lcDate belongs to prior period.'
        lcErrorM2 = ''
      ELSE  
        llContinue = .F.
      ENDIF
    ELSE    && Period not closed. Check if it is a future period
      IF Yearprd <>  SBTCOMP.CURYR+SBTCOMP.CURPRD .AND. !(lcTranTyp $ 'VI2VR2')
        llDummy   =  FErrInfo(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
        lcErrorM1 = '&lcDType&lcDate belongs to a future period.'
        lcErrorM2 = ''
      ENDIF
    ENDIF    
  ENDIF  
  USE IN SBTCOMP
ELSE
  =gfOpenFile(gcSysHome+'SYCCOMP',gcSysHome+'CCOMP_ID','SH')
  =SEEK(gcPrnt_Cmp,'SYCCOMP')
  IF 'GL' $ SYCCOMP.mModlset
    USE (gfGetDataDir(ALLTRIM(SYCCOMP.CCOM_DDIR))+'GLSETUP') SHARED AGAIN ALIAS TGLSETUP IN 0
    lDSETBBDAT=TGLSETUP.DSETBBDAT
    llAllPBB = TGLSETUP.LSETALBBE
    USE IN TGLSETUP 
  ELSE  
    lDSETBBDAT={}
    llAllPBB = .T.
  ENDIF  
  =gfOpenFile(gcDataDir+'FISHD',gcDataDir+'COMPFYEAR','SH')
  =gfOpenFile(gcDataDir+'FSPRD',gcDataDir+'COMFYRPRDI','SH')
  llContinue = .T.
  LOCATE
  IF FOUND()
    LOCATE REST FOR BETWEEN(ldDate,Dfsppbgdt,Dfsppendt) ;
                WHILE (ldDate >= Dfsppbgdt)                 
  ENDIF
  IF !FOUND()                  && No period match checked date
    llContinue = .F.
    lcErrorM1 = ' does not fall within any period. '
    lcErrorM2 = ''
  ELSE
    &lcFYear  = Cfisfyear      && Transaction date year
    &lcPeriod = Cfspprdid      && Transaction date period     
  ENDIF  
  IF llHideMsg
    SELECT (lnAlias)
    RETURN(llContinue)
  ENDIF
  *** Check if transaction date falls in a history period.
  IF llContinue .AND. Cfisfyear < STR(VAL(SYCCOMP.CCURR_YER)-1)
    llContinue = .F.
    lcErrorM1 = ' belongs to a history fiscal year.'
    lcErrorM2 = ''
  ENDIF 
  IF llContinue         
    *** Check if the transaction date before the begining balance
    *** date, and if the user is allowed to post before the begining
    *** balance date

    IF lcGlVers='A' AND !llAllPBB AND !EMPTY(lDSETBBDAT) .AND. ldDate < lDSETBBDAT
    *-- Check if the system is linked to GL And Allow posting before beginning Balance (End) 
      llContinue = .F.
      lcErrorM1 = ' falls before the begining balance date.'
      lcErrorM2 = ' No posting allowed before the begining balance date. '
    ENDIF  
  ENDIF  
  IF llContinue .AND. Lfsplocks         && Locked period
    llContinue = .F.
    lcErrorM1 = ' falls in a locked period.'
    lcErrorM2 = ''
  ENDIF  
  IF llContinue 
    IF Lfspclsds               && Closed period
      IF !(lcTranTyp $ 'VI2VR2')
        llDummy =  FErrInfo(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
        lcErrorM1 = '&lcDType&lcDate belongs to prior period.'
        lcErrorM2 = ''
      ELSE  
        IF lcTranTyp # 'VI2'
          llContinue = .F.
        ENDIF
      ENDIF
    ELSE      && Period not closed. Check if it is a future period.
      IF Cfisfyear+Cfspprdid <> SYCCOMP.CCURR_YER+SYCCOMP.CCURR_PRD .AND. !(lcTranTyp $ 'VI2VR2')
        llDummy =  FErrInfo(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
        lcErrorM1 = '&lcDType&lcDate belongs to a future period.'
        lcErrorM2 = ''
      ENDIF
    ENDIF    
  ENDIF  
ENDIF
IF !llContinue                  && There is an error.
  IF lcTranTyp $ 'VI2VR2'       && Transaction is either 'Void invoice'
                                && or 'Void return'
    lcErrorM1  = ' not in the current period. '
    lcErrorM2 = ''
  ENDIF
  llDummy =  FErrInfo(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
  lcErrorM1= lcDType + lcDate + lcErrorM1
  lcErrorM2 = ''
  SELECT (lnAlias)
  RETURN(.F.)
ENDIF
SELECT (lnAlias)
RETURN(.T.)

*!*************************************************************
*! Name      : lfUpdLot
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose   : Function to update reciving quantity.
*!*************************************************************
*! Parameters: lcCuTick   ---> P/O or C/T number.
*!             lcOpration ---> Operation code.
*!             lcRecvLot  ---> Selected lot no.
*!             laQuantity ---> Array holding Received quantity
*!             lcRcvItem  ---> Received Style.
*!*************************************************************
FUNCTION lfUpdLot
PARAMETERS lcCTCode, lcOpration, lcRecvLot, laQuantity, lcRcvItem
PRIVATE laAddQty, laOpenQty, lcSaveRecKey, lnCount, lcCount, lcLotNo

DECLARE laAddQty[9], laOpenQty[9]

llLotFound = .F.
SELECT MFGOPRDT
SET ORDER TO MFGOPRTR
=SEEK('I'+lcCTCode+lcOpration+lcRecvLot+'1')
LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd=;
                  'I'+lcCTCode+lcOpration+lcRecvLot+'1' ;
            FOR Item+cDyelot = lcRcvItem
IF !FOUND()
  IF  !((llMFCall .AND. &lcTmpLine..TranCd = '4' ) .OR. (!llMFCall .AND. &lcTmpLine..TranCd='5'))
    =SEEK('I'+lcCTCode+lcOpration)
    SCATTER MEMVAR MEMO
    SELECT CODES
    =SEEK('N'+lcOpration+'Y'+'MFGCODE   ','CODES')
    LOCATE REST WHILE cDefCode+ccode_no+crltfield+cfld_name = ;
                      'N'+lcOpration+'Y'+'MFGCODE   ' FOR CRLTD_NAM = 'CCONTCODE'
    SELECT MFGOPRDT
    m.cDyelot = &lcTmpLine..Dyelot
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE cIMtyp    WITH 'I',;
            cTktNo    WITH lcCTCode,;
            Item      WITH &lcTmpLine..Style,;
            cLotNo    WITH lcRecvLot ,;
            cOprCode  WITH lcOpration,;
            cContcode WITH IIF(EMPTY(cContcode),ALLT(CODES.cRltd_vlu),cContcode),;
            dTranDate WITH gdSysDate ,;
            DueDate   WITH gdSysDate ,;
            TranCd    WITH '1'       ,;
            cTrgOpr   WITH SPACE(2)  ,;
            cTrgLot   WITH SPACE(2)  ,;
            nLotQty1   WITH 0,;
            nLotQty2   WITH 0,;
            nLotQty3   WITH 0,;
            nLotQty4   WITH 0,;
            nLotQty5   WITH 0,;
            nLotQty6   WITH 0,;
            nLotQty7   WITH 0,;
            nLotQty8   WITH 0,;
            nLotTotQty WITH 0
  ENDIF
ENDIF

=SEEK('I'+lcCTCode+lcOpration)
DO WHILE cIMTyp+cTktNo+cOprCode = 'I'+lcCTCode+lcOpration AND !EOF()
  IF Item+cDyelot <> lcRcvItem
    SKIP
    LOOP
  ENDIF
  IF !EMPTY(lcRecvLot) AND cLotNo<>lcRecvLot
    SKIP
    LOOP
  ENDIF

  lcLotNo    = cLotNo
  llLotFound = .T.
  *--Check if there is any open quantity for this Opr/Lor/Clr
  STORE 0 TO laOpenQty
  SCAN REST WHILE cIMTyp+cTktNo+cOprCode+cLotNo='I'+lcCTCode+lcOpration+lcLotNo ;
              FOR Item+cDyelot=lcRcvItem
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      laOpenQty[lnCount] = MAX(laOpenQty[lnCount] + ;
                         IIF(TranCd='1',nLotQty&lcCount,-nLotQty&lcCount),0)
      laOpenQty[9] = laOpenQty[9] + laOpenQty[lnCount]
    ENDFOR
  ENDSCAN  
  *--Comupte quantity to be added for this Opr/Lot/Clr
  STORE 0 TO laAddQty  
  IF laOpenQty[9] > 0  
    FOR lnCount = 1 TO 8
      laAddQty[lnCount]    = MIN(&laQuantity[lnCount],laOpenQty[lnCount])
      &laQuantity[lnCount] = &laQuantity[lnCount] - laAddQty[lnCount]
      laAddQty[9]          = laAddQty[9]    + laAddQty[lnCount]
      &laQuantity[9]       = &laQuantity[9] - laAddQty[lnCount]
    ENDFOR
    IF laAddQty[9]>0
      lnSavRec=RECNO()
      =lfAppDetRec(lcLotNo, &lcTmpLine..TranCd,'laAddQty')
      
      IF BETWEEN(lnSavRec,1,RECCOUNT())
        GOTO lnSavRec
      ENDIF  

    ENDIF
  ENDIF  
ENDDO
*--If there is over received quantity, Update the last lot with the remaind quantity.
IF llLotFound AND &laQuantity[9] > 0 .AND. !((llMFCall .AND. &lcTmpLine..TranCd = '4' ) .OR. (!llMFCall .AND. &lcTmpLine..TranCd='5'))
  =lfAppDetRec(lcLotNo, &lcTmpLine..TranCd,'&laQuantity')
ENDIF
SET ORDER TO MFGOPRDT IN MFGOPRDT
RETURN

*!*************************************************************
*! Name      : lfAppDetRec
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose   : Function to append transaction record in the
*!             MFG operation detailed file.   
*!*************************************************************
*! Parameters: lcLotNum   ---> Lot number
*!             lcTranCode ---> Transaction code
*!             laNewQty   ---> Array holding Quantity to be added
*!*************************************************************
FUNCTION lfAppDetRec
PARAMETERS lcLotNum, lcTranCode, laNewQty


SELECT MFGOPRDT
=SEEK('I'+lcCTCode+lcLastOpr+lcLotNum+'1')
LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd=;
                  IIF(llMFCall,'M','I')+lcCTCode+lcLastOpr+lcLotNum+'1' ;
              FOR Item+cDyelot = lcRcvItem

SCATTER MEMVAR MEMO
APPEND BLANK
GATHER  MEMVAR MEMO
REPLACE nLotQty1   WITH &laNewQty[1] ,;
        nLotQty2   WITH &laNewQty[2] ,;
        nLotQty3   WITH &laNewQty[3] ,;
        nLotQty4   WITH &laNewQty[4] ,;
        nLotQty5   WITH &laNewQty[5] ,;
        nLotQty6   WITH &laNewQty[6] ,;
        nLotQty7   WITH &laNewQty[7] ,;
        nLotQty8   WITH &laNewQty[8] ,;
        nLotTotQty WITH &laNewQty[9] ,;
        dTranDate  WITH gdSysDate    ,;
        DueDate    WITH {}           ,;
        TranCd     WITH lcTranCode   ,;
        cTrgOpr    WITH SPACE(2)     ,;
        cTrgLot    WITH SPACE(2)     ,;
        cOwner     WITH ' '
=gfAdd_Info('MFGOPRDT')
*--Call TraceKey global function.
=gfTraceKey('MFGOPRDT',MFGOPRDT.cimtyp+MFGOPRDT.ctktno+MFGOPRDT.coprcode+MFGOPRDT.clotno+MFGOPRDT.trancd,'A')
RETURN(.T.)

*!*************************************************************
*! Name      : lfSelLots
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose   : Function to select lot number to receive to.
*!*************************************************************
*! Parameters: Style line no.
*!*************************************************************
*! Returns   : Selected lot number 
*!*************************************************************
FUNCTION lfSelLots
PARA lcTktLn,lcHdrLOpr


lnAlias = SELECT()
SELECT MfgOprDt
lcMFOldTg = ORDER('MFGOPRDT')
SET ORDER TO TAG TktOpTrn

SELECT BOMLINE
lcBmOldTg = ORDER('BOMLINE')
SET ORDER TO TAG Mfgopr


lcLastMfgOr = lcHdrLOpr

*--If not empty last operation , read receive lot.
lcRecvLot = '  ' 
IF !EMPTY(lcLastMfgOr)
  SELECT MFGOPRDT
  =SEEK(lcPtype+laData[1]+lclastMfgor+'1')
  lnNumLots = 0
  COUNT REST TO lnNumLots ;
          WHILE cIMTyp+cTktNo+cOprCode+TranCd = ;
                lcPType+laData[1]+lcLastMfgOr+'1' FOR Item = lcStyle .AND. nLotTotQty >0
  IF lnNumLots <> 0
    =SEEK(lcPType+laData[1]+lcLastMfgOr+'1')
    LOCATE REST WHILE cIMTyp+cTktNo+cOprCode+TranCd = ;
                      lcPType+laData[1]+lcLastMfgOr+'1' FOR Item = lcStyle .AND. cDyelot = lcDyelot
    DO CASE
      CASE lnNumLots = 1
        lcRecvLot = cLotNo
      CASE lnNumLots > 1
        *--Do you want to select a lot to receive to or distribute received quantity to all lots?,\<Select;\<Distribute
        IF gfModalGen('QRM42108B42011','DIALOG') = 1
          lcBrFields = [cLotNo    :H='Lot No.',]+;
                       [cOprCode  :H='Operation',]+;
                       [cContCode :H='Cont./Dept.',]+;
                       [cContName :H='Name',]+;             
                       [dTranDate :H='Trans.Date',]+;
                       [dueDate   :H='Due Date',]+;
                       [nLotTotQty :H='Total']
          DIMENSION laTemp[1]
          laTemp = ''
          lcTCode = laData[1]
          =ARIABROW([IIF(lcPType='D','I',lcPType)+lcTCode+lcLastMfgOr+'1' FOR Item=lcStyle;
                  .AND. cDyelot = lcDyelot],'Lots',gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','cLotNo','laTemp')
          lcRecvLot=laTemp[1]
          IF !EMPTY(lcRecvLot)
            llSpecLot = .T.
            laLotArry = 0
            SELECT MfgOprDt
            SEEK lcPType+laData[1]+lcLastMfgOr+'1'       
            SCAN WHILE cIMTyp+cTktNo+cOprCode = IIF(lcPType='D','I',lcPType)+laData[1]+lcLastMfgOr;
                  FOR Item = lcStyle AND cLotNo = lcRecvLot

              FOR I=1 TO 8
                Z=STR(I,1)
                laLotArry[I]=laLotArry[I]+(NLOTQTY&Z * IIF(TranCd='1',1,-1))
              ENDFOR
            ENDSCAN
          ENDIF 
        ENDIF                
    ENDCASE      
  ELSE
    lcRecvLot = '01'
  ENDIF
ENDIF

*--Set Order Tags Back.
SELECT MfgOprDt
SET ORDER TO TAG &lcMFOldTg
SELECT BOMLINE
SET ORDER TO TAG &lcBmOldTg
SELECT (lnAlias)

*--Read last operation for color.
lcClrLstOp = lcLastMfgOr
lcRecvLot  = IIF(EMPTY(lcRecvLot),SPACE(2),lcRecvLot)
RETURN(lcRecvLot)

*!*************************************************************
*! Name      : lfPrnt
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/03/2002
*! Purpose   : Print the rebalance log report
*!*************************************************************
*! Passed Parameters      : None.
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfPrnt()
*!*************************************************************
FUNCTION lfVPrnt

IF pSetup(.T.)
  gcOutFile = gcWorkDir+gfTempName()+'.TXT'
  COPY MEMO TMPSTR.mStrRep TO &gcOutFile
  gcDevice = 'PRINTER'
  DO ENDREPORT
  gcDevice = 'SCREEN'
ENDIF

*!*************************************************************
*! Name      : lfvCancel
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/03/2002
*! Purpose   : Valid function for cancel button in Vendor screen
*!*************************************************************
*! Passed Parameters      : None.
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfvCancel()
*!*************************************************************
FUNCTION lfvCancel

lnChoose = gfModalGen("TRM00000B00006","DIALOG",.F.,.F.,;
           'Are you sure you want to cancel the generation process.')

IF lnChoose = 1
  CLEAR READ
  llCancelPO = .T.
ENDIF             

*khalid 09/17/2002 (Start)
FUNCTION lfvEquCost
PARAMETERS lcCstNo,lnFrnCost,lnCurRate,lnCurUnt

lnCstType = lcIType&lcCstNo
IF lnCstType $ 'PMD'
  STORE '' TO lcPMethod,lcPUnMeth,lcDMethod,lcDUnMeth
  IF lnCstType='P'
    lcPMethod = gfGetExSin(@lcPUnMeth,lcCur1)
    lcPMethod = IIF(EMPTY(lcPMethod),'*',lcPMethod)
    lcPUnMeth = IIF(EMPTY(lcPUnMeth),'/',lcPUnMeth)
    lnEquCost = lnFrnCost &lcPMethod lnCurRate &lcPUnMeth lnCurUnt
  ELSE
    lcDMethod = gfGetExSin(@lcDUnMeth,lcCur2)
    lcDMethod = IIF(EMPTY(lcDMethod),'*',lcDMethod)
    lcDUnMeth = IIF(EMPTY(lcDUnMeth),'/',lcDUnMeth)
    lnEquCost = lnFrnCost &lcDMethod lnCurRate &lcDUnMeth lnCurUnt
  ENDIF
ELSE
  lnEquCost = lnFrnCost
ENDIF
lnEquCost = ROUND(lnEquCost,2)
RETURN (lnEquCost)
