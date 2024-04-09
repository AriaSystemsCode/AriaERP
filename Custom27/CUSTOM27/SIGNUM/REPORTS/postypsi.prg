*!********************************************************************
*: Program file  : POSTYPSI.PRG (Converted from 26 to 27) for SIGNUM.
*: Program desc. : PRINT STYLE PURCHASE ORDER for SIGNUM
*:         System: ARIA APPAREL SERIES
*:         Module: 
*:      Developer: Adel MOhammed El Gazzar (ADEL)
*:      Refer to : (C101527)
*:          Date : 09/14/1999
*!********************************************************************

*--llrpPrtSn   && Print style notepad?
*--llrpPrtPn   && Print transaction notepad?
*--Open currency file.
=gfOpenFile(gcSysHome+'syccurr',gcSysHome+'ccurrcode','SH')
*-- Get the copmany fax to print it in the FRX.
lcCompFax = SycComp.cCom_Fax
*-- Get the style major and color.
lcStyMajor = gfItemMask('HI')
*-Get the style major length
lnMajorLen = LEN(gfItemMask('PM'))
lnColorLen = 0
lcNonMajTl = ''
*--Get the No. of major segments.
lnMajSeg = gfItemMask('SM')
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
*-- Get the Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lnNonMajSt = laMajSegs[lnI,4]
    lnColorLen = LEN(IIF(lnColorLen = 0 .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lnColorLen + laMajSegs[lnI-1,6] + laMajSegs[lnI,3]))
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
                 
    EXIT
  ENDIF
ENDFOR 
*-- Initializing the necessary variables.
DIMENSION laScales[4,9]
STORE "" TO lcName,lcAddr1,lcAddr2,lcAddr3,lcAddr4
lcPo      = '  '
STORE 0 TO lnCount,lnCounter
*--The following array carrys the temp files names and directories to be erased in the master file.
*--This program contains 2 temp files and 2 cursors.
lnArElmts = ALEN(laOpTmpFls,1)
DIMENSION laOpTmpFls[lnArElmts+3,2]
*-Element 1 : Holds the temp name.
*-Element 2 : Holds the temp directory (cursors don't have).
lcTmpLin  = gfTempName()
lcTempCur = gfTempName()
TmpOrd    = gfTempName()
laOpTmpFls[1,1] = lcTmpLin
laOpTmpFls[1,2] = gcWorkDir
laOpTmpFls[2,1] = lcTempCur
laOpTmpFls[2,2] = ""
laOpTmpFls[3,1] = TmpOrd
laOpTmpFls[3,2] = ""
*-- To hold all the BMPs of the divisions for the selected POs.
CREATE CURSOR &lcTempCur (Division c(6),Gen G)
INDEX ON Division TAG (lcTempCur) OF (lcTempCur)
*-- To hold all the Orders that are allocated to the Po(s)
CREATE CURSOR (TmpOrd) (cTranCd C(1),cPoNo C(6),cOrderNo C(6),;
                            cAccount C(5), dCompDate D)
ZAP
INDEX ON cTranCd+cPoNo+cOrderNo TAG (TmpOrd) OF (gcWorkDir+TmpOrd+".CDX")

*-- Openinig the necessary files.
=gfOpenFile (gcDataDir+'CutPick',gcDataDir+'Cutpkord','SH')
=gfOpenFile (gcDataDir+'OrdHdr',gcDataDir+'OrdHdr','SH')
SELECT PosLn
=AFIELDS(laPosLnFld)
lnPosLnFld = ALEN(laPosLnFld,1)
DIMENSION laPosLnFld[lnPosLnFld+1,4]
laPosLnFld[lnPosLnFld+1,1]='cNotes'
laPosLnFld[lnPosLnFld+1,2]='M'
laPosLnFld[lnPosLnFld+1,3]=''
laPosLnFld[lnPosLnFld+1,4]='0'
CREATE TABLE (gcWorkDir+lcTmpLin) FROM ARRAY laPosLnFld
INDEX ON &lcTmpLin..Po+&lcTmpLin..Style+STR(&lcTmpLin->LineNo,4)+&lcTmpLin->Trancd TAG &lcTmpLin of (lcTmpLin)
=gfOpenFile (gcWorkDir+lcTmpLin,' ','EX')
*--Scale temp file.
lcScTemp = gfTempName()
*--Fill the array to erase temp files in the master prog.
laOpTmpFls[4,1] = lcScTemp 
laOpTmpFls[4,2] = gcWorkDir
CREATE TABLE (gcWorkDir+lcScTemp) (PO C(6),;
                                  Scale1 C(03),Size1 C(5),Size2 C(5),Size3 C(5),Size4 C(5),Size5 C(5),Size6 C(5),Size7 C(5),Size8 C(5),;
                                  Scale2 C(03),Size9 C(5),Size10 C(5),Size11 C(5),Size12 C(5),Size13 C(5),Size14 C(5),Size15 C(5),Size16 C(5),;
                                  Scale3 C(03),Size17 C(5),Size18 C(5),Size19 C(5),Size20 C(5),Size21 C(5),Size22 C(5),Size23 C(5),Size24 C(5),;
                                  Scale4 C(03),Size25 C(5),Size26 C(5),Size27 C(5),Size28 C(5),Size29 C(5),Size30 C(5),Size31 C(5),Size32 C(5),nCount N(2))
INDEX ON PO TAG (lcScTemp)

*--Collect data
SELECT POSHDR
=lfCollData()
*--End of program POSTYPSI


*!*************************************************************
*! Name      : lfCollData
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/14/1999
*! Purpose   : To collect needed data.
*!*************************************************************
FUNCTION lfCollData

SELECT POSHDR
SET SKIP TO
SCAN FOR &lcRpExp
  WAIT WINDOW "Selecting BMP for division : " + cDivision NOWAIT
  IF !SEEK(cDivision , lcTempCur)
    SELECT (lcTempCur)
    APPEND BLANK
    REPLACE DIVISION WITH PoSHdr.cDivision 
    lcBmpF = gcBMPHome + ALLTRIM(PoSHdr.cDivision)+'.BMP'
    IF FILE(lcBmpF)
      APPEND GENERAL GEN FROM &lcBmpF
    ENDIF
  ENDIF
  *-- Selecting all the Po Lines for the selected POs into lcTmpLin
  *-- Now re-initial the used variables for scales.
  STORE 0 TO lnCount,lnSclCnt
  lcScale  = ' '
  SELECT POSHDR
  lcPoNumber = Po
  INSERT INTO (lcScTemp) (PO) VALUES (lcPoNumber)
  SELECT PosLn
  DO WHILE Po = lcPoNumber  
    lcStyle = SUBSTR(Style,1,lnMajorLen)
    SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd='P'+lcPoNumber+lcStyle;
              FOR TranCd = "1"      
      WAIT WINDOW "Selecting recods. Po/"+lcStyMajor+" :" + lcPoNumber+"/"+ALLTRIM(Style) NOWAIT
      SCATTER MEMVAR MEMO
      INSERT INTO (lcTmpLin) FROM MEMVAR
      *--Update scale temp
      lnCount = lnCount +1
      IF Scale <> lcScale AND lnSclCnt <= 4
        *--Another scale
        lnSclCnt = lnSclCnt+IIF(lnSclCnt<4,1,0)
        lcSclCnt = STR(lnSclCnt,1)
        lcScale  = Scale
        SELECT (lcScTemp)
        =SEEK(lcPoNumber)
        REPLACE Scale&lcSclCnt WITH lcScale
        *--There are 32 size fields foe the 4 scales.
        lnExtra = IIF(lnSclCnt =1,0,IIF(lnSclCnt=2,8,IIF(lnSclCnt=3,16,24)))
        IF SEEK ('S'+Scale&lcSclCnt,'SCALE')
          FOR lnCnt = 1 TO 8
            SclFilS = STR(lnCnt,1)
            lcStr = ALLTRIM(STR(lnCnt+lnExtra,IIF(lnSclCnt = 1,1,2)))
            REPLACE Size&lcStr WITH SCALE.SZ&SclFilS
          ENDFOR  
        ENDIF
      ENDIF
    ENDSCAN
    IF llrpPrtSn .AND. SEEK("F"+lcStyle,'NotePad')
      lnCount = lnCount +1
      SELECT (lcTmpLin)
      GOTO BOTTOM
      SCATTER MEMVAR MEMO
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE cNotes WITH NotePad.mNotes
    ENDIF
    *--Update scale file with PO LINES.
    SELECT (lcScTemp)
    =SEEK(lcPoNumber)
    REPLACE ALL nCount WITH lnCount FOR PO = lcPoNumber
    SELECT PosLn    
  ENDDO  
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
  SELECT POSHDR
ENDSCAN 
WAIT CLEAR
SELECT PosHdr
SET RELATION TO 
*-- Setting the relation between the PosLn and the necessary files.
SELECT (lcTmpLin)
SET RELATION TO "P"+Po      INTO PoSHdr ADDITIVE
SET RELATION TO "S"+SCALE   INTO SCALE  ADDITIVE
SET RELATION TO STYLE       INTO STYLE  ADDITIVE
SET RELATION TO PO INTO (lcScTemp) ADDITIVE
*SET RELATION TO IIF(lnCounter >= EVAL(lcScTemp+'.nCount'),'2'+Po,' ')  INTO (TmpOrd) ADDITIVE
SET RELATION TO '2'+Po  INTO (TmpOrd) ADDITIVE
SET SKIP TO (TmpOrd)
SELECT PoSHdr
SET RELATION TO cDivision   INTO &lcTempCur ADDITIVE
SET RELATION TO "P"+Po      INTO NOTEPAD ADDITIVE
SET RELATION TO vendor INTO Apvendor ADDITIVE
*--We are going to print from the temp file not from  posln so fix the lcRpExp
lcRpExp = STRTRAN(lcRpExp,'POSLN','&lcTmpLin')
SELECT (lcTmpLin)
GOTO TOP
