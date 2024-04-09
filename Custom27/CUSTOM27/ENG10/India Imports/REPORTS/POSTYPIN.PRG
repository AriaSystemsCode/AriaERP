*:***************************************************************************
*: Program file  : POSTYPIN
*: Program desc. : Print PO for INDIA IMPORTS
*: For Report    : POSTYPIN.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : PO
*: Developer     : AHMED MAHER (AMH)
*: Reference     : C200248,1
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*! Modifications :
*! C200291,1 AMH 02/25/2002 Make some changes.
*:***************************************************************************

PRIVATE lcRltColor
lcRltColor = SPACE(0)
DECLARE laColor[1,2]
laColor[1,1] = 'CNRFCODE'
laColor[1,2] = 'lcRltColor'

PRIVATE lnI, lnColorStr, lnColorLen
STORE 0 TO lnColorStr, lnColorLen
SELECT PosHdr
SET SKIP TO
SELECT PosLn
SET SKIP TO
SET ORDER TO PosLn

*-- Open Files.
IF !USED('CUTPICK')
  =gfOpenFile(gcDataDir+'CUTPICK','CUTPICK','SH')
ENDIF
IF !USED('ORDHDR')
  =gfOpenFile(gcDataDir+'ORDHDR','ORDHDR','SH')
ENDIF
IF !USED('CUSTDEPT')
  =gfOpenFile(gcDataDir+'CUSTDEPT','CUSTDEPT','SH')
ENDIF

*C200291,1 AMH Open ctktbom file [Start]
llOpenBom = .F.
IF !USED('CTKTBOM')
  =gfOpenFile(gcDataDir+'CTKTBOM','CTKTBOM','SH')
  llOpenBom = .T.
ENDIF
*C200291,1 AMH [End]

*-- Creating the temporary file
lcTmpPolns = gfTempName()
SELECT PosLn
=AFIELDS(laFStru)

*C200291,1 AMH Rebuild the laFStru array to add new fields and remove not needed fields [Start]
*DIMENSION laFStru[ALEN(laFStru,1)+17,4]

*laFStru[ALEN(laFStru,1),1] = 'cNotePad'
*laFStru[ALEN(laFStru,1),2] = 'M'
*laFStru[ALEN(laFStru,1),3] = 0
*laFStru[ALEN(laFStru,1),4] = 0

*laFStru[ALEN(laFStru,1)-1,1] = 'nPoTotQty'
*laFStru[ALEN(laFStru,1)-1,2] = 'N'
*laFStru[ALEN(laFStru,1)-1,3] = 8
*laFStru[ALEN(laFStru,1)-1,4] = 0

*laFStru[ALEN(laFStru,1)-2,1] = 'cCustDept'
*laFStru[ALEN(laFStru,1)-2,2] = 'C'
*laFStru[ALEN(laFStru,1)-2,3] = 36
*laFStru[ALEN(laFStru,1)-2,4] = 0

*laFStru[ALEN(laFStru,1)-3,1] = 'cCompos'
*laFStru[ALEN(laFStru,1)-3,2] = 'C'
*laFStru[ALEN(laFStru,1)-3,3] = 80
*laFStru[ALEN(laFStru,1)-3,4] = 0

*laFStru[ALEN(laFStru,1)-4,1] = 'cFab1'
*laFStru[ALEN(laFStru,1)-4,2] = 'C'
*laFStru[ALEN(laFStru,1)-4,3] = 30
*laFStru[ALEN(laFStru,1)-4,4] = 0

*laFStru[ALEN(laFStru,1)-5,1] = 'cFabc1'
*laFStru[ALEN(laFStru,1)-5,2] = 'C'
*laFStru[ALEN(laFStru,1)-5,3] = 15
*laFStru[ALEN(laFStru,1)-5,4] = 0

*laFStru[ALEN(laFStru,1)-6,1] = 'cFabbw1'
*laFStru[ALEN(laFStru,1)-6,2] = 'C'
*laFStru[ALEN(laFStru,1)-6,3] = 30
*laFStru[ALEN(laFStru,1)-6,4] = 0

*laFStru[ALEN(laFStru,1)-7,1] = 'cFabcom1'
*laFStru[ALEN(laFStru,1)-7,2] = 'C'
*laFStru[ALEN(laFStru,1)-7,3] = 30
*laFStru[ALEN(laFStru,1)-7,4] = 0

*laFStru[ALEN(laFStru,1)-8,1] = 'cType'
*laFStru[ALEN(laFStru,1)-8,2] = 'C'
*laFStru[ALEN(laFStru,1)-8,3] = 1
*laFStru[ALEN(laFStru,1)-8,4] = 0

*laFStru[ALEN(laFStru,1)-9,1] = 'cFab2'
*laFStru[ALEN(laFStru,1)-9,2] = 'C'
*laFStru[ALEN(laFStru,1)-9,3] = 30
*laFStru[ALEN(laFStru,1)-9,4] = 0

*laFStru[ALEN(laFStru,1)-10,1] = 'cFabc2'
*laFStru[ALEN(laFStru,1)-10,2] = 'C'
*laFStru[ALEN(laFStru,1)-10,3] = 15
*laFStru[ALEN(laFStru,1)-10,4] = 0

*laFStru[ALEN(laFStru,1)-11,1] = 'cFabbw2'
*laFStru[ALEN(laFStru,1)-11,2] = 'C'
*laFStru[ALEN(laFStru,1)-11,3] = 30
*laFStru[ALEN(laFStru,1)-11,4] = 0

*laFStru[ALEN(laFStru,1)-12,1] = 'cFabcom2'
*laFStru[ALEN(laFStru,1)-12,2] = 'C'
*laFStru[ALEN(laFStru,1)-12,3] = 30
*laFStru[ALEN(laFStru,1)-12,4] = 0

*laFStru[ALEN(laFStru,1)-13,1] = 'cColor'
*laFStru[ALEN(laFStru,1)-13,2] = 'C'
*laFStru[ALEN(laFStru,1)-13,3] = 6
*laFStru[ALEN(laFStru,1)-13,4] = 0

*laFStru[ALEN(laFStru,1)-14,1] = 'cClrDesc'
*laFStru[ALEN(laFStru,1)-14,2] = 'C'
*laFStru[ALEN(laFStru,1)-14,3] = 30
*laFStru[ALEN(laFStru,1)-14,4] = 0

*laFStru[ALEN(laFStru,1)-15,1] = 'cNRFCode'
*laFStru[ALEN(laFStru,1)-15,2] = 'C'
*laFStru[ALEN(laFStru,1)-15,3] = 5
*laFStru[ALEN(laFStru,1)-15,4] = 0

*laFStru[ALEN(laFStru,1)-16,1] = 'cObject_id'
*laFStru[ALEN(laFStru,1)-16,2] = 'C'
*laFStru[ALEN(laFStru,1)-16,3] = 10
*laFStru[ALEN(laFStru,1)-16,4] = 0

DIMENSION laFStru[ALEN(laFStru,1)+35,4]

laFStru[ALEN(laFStru,1),1] = 'cNotePad'
laFStru[ALEN(laFStru,1),2] = 'M'
laFStru[ALEN(laFStru,1),3] = 0
laFStru[ALEN(laFStru,1),4] = 0

laFStru[ALEN(laFStru,1)-1,1] = 'cCustName'
laFStru[ALEN(laFStru,1)-1,2] = 'C'
laFStru[ALEN(laFStru,1)-1,3] = 30
laFStru[ALEN(laFStru,1)-1,4] = 0

laFStru[ALEN(laFStru,1)-2,1] = 'cCustDept'
laFStru[ALEN(laFStru,1)-2,2] = 'C'
laFStru[ALEN(laFStru,1)-2,3] = 36
laFStru[ALEN(laFStru,1)-2,4] = 0

laFStru[ALEN(laFStru,1)-3,1] = 'cType'
laFStru[ALEN(laFStru,1)-3,2] = 'C'
laFStru[ALEN(laFStru,1)-3,3] = 1
laFStru[ALEN(laFStru,1)-3,4] = 0

laFStru[ALEN(laFStru,1)-4,1] = 'cFab1'
laFStru[ALEN(laFStru,1)-4,2] = 'C'
laFStru[ALEN(laFStru,1)-4,3] = 20
laFStru[ALEN(laFStru,1)-4,4] = 0

laFStru[ALEN(laFStru,1)-5,1] = 'cFabc1'
laFStru[ALEN(laFStru,1)-5,2] = 'C'
laFStru[ALEN(laFStru,1)-5,3] = 15
laFStru[ALEN(laFStru,1)-5,4] = 0

laFStru[ALEN(laFStru,1)-6,1] = 'cFabbw1'
laFStru[ALEN(laFStru,1)-6,2] = 'C'
laFStru[ALEN(laFStru,1)-6,3] = 30
laFStru[ALEN(laFStru,1)-6,4] = 0

laFStru[ALEN(laFStru,1)-7,1] = 'cFabcom1'
laFStru[ALEN(laFStru,1)-7,2] = 'C'
laFStru[ALEN(laFStru,1)-7,3] = 40
laFStru[ALEN(laFStru,1)-7,4] = 0

laFStru[ALEN(laFStru,1)-8,1] = 'cFab2'
laFStru[ALEN(laFStru,1)-8,2] = 'C'
laFStru[ALEN(laFStru,1)-8,3] = 20
laFStru[ALEN(laFStru,1)-8,4] = 0

laFStru[ALEN(laFStru,1)-9,1] = 'cFabc2'
laFStru[ALEN(laFStru,1)-9,2] = 'C'
laFStru[ALEN(laFStru,1)-9,3] = 15
laFStru[ALEN(laFStru,1)-9,4] = 0

laFStru[ALEN(laFStru,1)-10,1] = 'cFabbw2'
laFStru[ALEN(laFStru,1)-10,2] = 'C'
laFStru[ALEN(laFStru,1)-10,3] = 30
laFStru[ALEN(laFStru,1)-10,4] = 0

laFStru[ALEN(laFStru,1)-11,1] = 'cFabcom2'
laFStru[ALEN(laFStru,1)-11,2] = 'C'
laFStru[ALEN(laFStru,1)-11,3] = 40
laFStru[ALEN(laFStru,1)-11,4] = 0

laFStru[ALEN(laFStru,1)-12,1] = 'cFab3'
laFStru[ALEN(laFStru,1)-12,2] = 'C'
laFStru[ALEN(laFStru,1)-12,3] = 20
laFStru[ALEN(laFStru,1)-12,4] = 0

laFStru[ALEN(laFStru,1)-13,1] = 'cFabc3'
laFStru[ALEN(laFStru,1)-13,2] = 'C'
laFStru[ALEN(laFStru,1)-13,3] = 15
laFStru[ALEN(laFStru,1)-13,4] = 0

laFStru[ALEN(laFStru,1)-14,1] = 'cFabbw3'
laFStru[ALEN(laFStru,1)-14,2] = 'C'
laFStru[ALEN(laFStru,1)-14,3] = 30
laFStru[ALEN(laFStru,1)-14,4] = 0

laFStru[ALEN(laFStru,1)-15,1] = 'cFabcom3'
laFStru[ALEN(laFStru,1)-15,2] = 'C'
laFStru[ALEN(laFStru,1)-15,3] = 40
laFStru[ALEN(laFStru,1)-15,4] = 0

laFStru[ALEN(laFStru,1)-16,1] = 'cFab4'
laFStru[ALEN(laFStru,1)-16,2] = 'C'
laFStru[ALEN(laFStru,1)-16,3] = 20
laFStru[ALEN(laFStru,1)-16,4] = 0

laFStru[ALEN(laFStru,1)-17,1] = 'cFabc4'
laFStru[ALEN(laFStru,1)-17,2] = 'C'
laFStru[ALEN(laFStru,1)-17,3] = 15
laFStru[ALEN(laFStru,1)-17,4] = 0

laFStru[ALEN(laFStru,1)-18,1] = 'cFabbw4'
laFStru[ALEN(laFStru,1)-18,2] = 'C'
laFStru[ALEN(laFStru,1)-18,3] = 30
laFStru[ALEN(laFStru,1)-18,4] = 0

laFStru[ALEN(laFStru,1)-19,1] = 'cFabcom4'
laFStru[ALEN(laFStru,1)-19,2] = 'C'
laFStru[ALEN(laFStru,1)-19,3] = 40
laFStru[ALEN(laFStru,1)-19,4] = 0

laFStru[ALEN(laFStru,1)-20,1] = 'cFab5'
laFStru[ALEN(laFStru,1)-20,2] = 'C'
laFStru[ALEN(laFStru,1)-20,3] = 20
laFStru[ALEN(laFStru,1)-20,4] = 0

laFStru[ALEN(laFStru,1)-21,1] = 'cFabc5'
laFStru[ALEN(laFStru,1)-21,2] = 'C'
laFStru[ALEN(laFStru,1)-21,3] = 15
laFStru[ALEN(laFStru,1)-21,4] = 0

laFStru[ALEN(laFStru,1)-22,1] = 'cFabbw5'
laFStru[ALEN(laFStru,1)-22,2] = 'C'
laFStru[ALEN(laFStru,1)-22,3] = 30
laFStru[ALEN(laFStru,1)-22,4] = 0

laFStru[ALEN(laFStru,1)-23,1] = 'cFabcom5'
laFStru[ALEN(laFStru,1)-23,2] = 'C'
laFStru[ALEN(laFStru,1)-23,3] = 40
laFStru[ALEN(laFStru,1)-23,4] = 0

laFStru[ALEN(laFStru,1)-24,1] = 'cFab6'
laFStru[ALEN(laFStru,1)-24,2] = 'C'
laFStru[ALEN(laFStru,1)-24,3] = 20
laFStru[ALEN(laFStru,1)-24,4] = 0

laFStru[ALEN(laFStru,1)-25,1] = 'cFabc6'
laFStru[ALEN(laFStru,1)-25,2] = 'C'
laFStru[ALEN(laFStru,1)-25,3] = 15
laFStru[ALEN(laFStru,1)-25,4] = 0

laFStru[ALEN(laFStru,1)-26,1] = 'cFabbw6'
laFStru[ALEN(laFStru,1)-26,2] = 'C'
laFStru[ALEN(laFStru,1)-26,3] = 30
laFStru[ALEN(laFStru,1)-26,4] = 0

laFStru[ALEN(laFStru,1)-27,1] = 'cFabcom6'
laFStru[ALEN(laFStru,1)-27,2] = 'C'
laFStru[ALEN(laFStru,1)-27,3] = 40
laFStru[ALEN(laFStru,1)-27,4] = 0

laFStru[ALEN(laFStru,1)-28,1] = 'cColor'
laFStru[ALEN(laFStru,1)-28,2] = 'C'
laFStru[ALEN(laFStru,1)-28,3] = 6
laFStru[ALEN(laFStru,1)-28,4] = 0

laFStru[ALEN(laFStru,1)-29,1] = 'cClrDesc'
laFStru[ALEN(laFStru,1)-29,2] = 'C'
laFStru[ALEN(laFStru,1)-29,3] = 30
laFStru[ALEN(laFStru,1)-29,4] = 0

laFStru[ALEN(laFStru,1)-30,1] = 'cNRFCode'
laFStru[ALEN(laFStru,1)-30,2] = 'C'
laFStru[ALEN(laFStru,1)-30,3] = 5
laFStru[ALEN(laFStru,1)-30,4] = 0

laFStru[ALEN(laFStru,1)-31,1] = 'cObject_id'
laFStru[ALEN(laFStru,1)-31,2] = 'C'
laFStru[ALEN(laFStru,1)-31,3] = 10
laFStru[ALEN(laFStru,1)-31,4] = 0

laFStru[ALEN(laFStru,1)-32,1] = 'cCusVend'
laFStru[ALEN(laFStru,1)-32,2] = 'C'
laFStru[ALEN(laFStru,1)-32,3] = 15
laFStru[ALEN(laFStru,1)-32,4] = 0

laFStru[ALEN(laFStru,1)-33,1] = 'dIssDate'
laFStru[ALEN(laFStru,1)-33,2] = 'D'
laFStru[ALEN(laFStru,1)-33,3] = 8
laFStru[ALEN(laFStru,1)-33,4] = 0

laFStru[ALEN(laFStru,1)-34,1] = 'mPack'
laFStru[ALEN(laFStru,1)-34,2] = 'M'
laFStru[ALEN(laFStru,1)-34,3] = 0
laFStru[ALEN(laFStru,1)-34,4] = 0
*C200291,1 AMH [End]

=gfCrtTmp(lcTmpPolns,@laFStru,'cstytype+po+cType+style+STR(lineno,6)+trancd',lcTmpPolns)

*-- Save current setting of memowidth.
lnMemoWdth = SET('MEMOWIDTH')
SET MEMOWIDTH TO 115

*-- Declare Array to hold the total Qty.
DECLARE laTotQty[8]

*-- Get Color Segment.
=lfGetColor()

*-- Collecting Data.
SELECT PosHdr
SCAN FOR &lcRpExp
  WAIT WINDOW "PO# : " + PosHdr.PO NOWAIT
  IF SEEK(cStyType+Po,'PosLn')
    *-- Get Header information.
    SELECT PosLn
    
    *C200291,1 AMH Don't need to get the total quantity of PO [Start]
    *STORE 0 TO lnPoTotQty,lnPosLnCnt
    STORE 0 TO lnPosLnCnt
    *SUM Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ;
        REST WHILE cstytype+po+style+STR(lineno,6)+trancd = PosHdr.cStyType+PosHdr.PO ;
        FOR TranCd = '1' ;
        TO lnPoTotQty
    IF SEEK('2'+POSHDR.PO,'CUTPICK') .AND. SEEK('O'+CUTPICK.ORDER,'ORDHDR') .AND.;
       SEEK(ORDHDR.ACCOUNT+ORDHDR.DEPT,'CUSTDEPT')
      lcCustDept = CUSTDEPT.DEPT + ' ' + CUSTDEPT.CDEPTDESC
    ELSE
      lcCustDept = ''
    ENDIF
    IF SEEK('M'+ORDHDR.ACCOUNT,'CUSTOMER')
      lcCustName = CUSTOMER.BTNAME
      lcCusVend  = CUSTOMER.CCUSVEND
    ELSE
      lcCustName = ''
      lcCusVend  = ''
    ENDIF
    *SEEK POSHDR.cStyType+POSHDR.Po
    IF SEEK('I'+POSHDR.PO,'CTKTBOM')
      ldIssDate = CTKTBOM.DATE
    ELSE
      ldIssDate = POSHDR.ENTERED
    ENDIF
    *C200291,1 AMH [End]
    
    COUNT REST WHILE cstytype+po+style+STR(lineno,6)+trancd = PosHdr.cStyType+PosHdr.PO ;
          FOR TranCd = '1' TO lnPosLnCnt
    SEEK POSHDR.cStyType+POSHDR.Po
    IF SEEK('F'+SUBSTR(style,1,lnMajSize),'NOTEPAD')
      lcCompos = MLINE(NOTEPAD.MNOTES,1)
    ELSE
      lcCompos = ''
    ENDIF
    *-- Add Footer # 1.
    SELECT POSLN
    SEEK POSHDR.cStyType+POSHDR.Po
    SCATTER MEMVAR MEMO
    INSERT INTO (lcTmpPolns) FROM MEMVAR
    SELECT (lcTmpPolns)
    REPLACE cType    WITH 'F'
    IF SEEK('2'+POSHDR.PO,'CUTPICK') .AND. SEEK('O'+CUTPICK.ORDER,'ORDHDR')
      
      *C200291,1 AMH Collecting data from the correct new fields [Start]
      *REPLACE cFab1    WITH ORDHDR.CFAB1;
              cFabc1   WITH ORDHDR.CFABC1;
              cFabbw1  WITH ORDHDR.CFABBW1;
              cFabcom1 WITH ORDHDR.CFABCOM1
      *REPLACE cFab2    WITH ORDHDR.CFAB2;
              cFabc2   WITH ORDHDR.CFABC2;
              cFabbw2  WITH ORDHDR.CFABBW2;
              cFabcom2 WITH ORDHDR.CFABCOM2
      REPLACE cFab1    WITH ORDHDR.CFAB1;
              cFabc1   WITH ORDHDR.CCOL1;
              cFabbw1  WITH ORDHDR.CBW1;
              cFabcom1 WITH ORDHDR.CCOMP1
      REPLACE cFab2    WITH ORDHDR.CFAB2;
              cFabc2   WITH ORDHDR.CCOL2;
              cFabbw2  WITH ORDHDR.CBW2;
              cFabcom2 WITH ORDHDR.CCOMP2
      REPLACE cFab3    WITH ORDHDR.CFAB3;
              cFabc3   WITH ORDHDR.CCOL3;
              cFabbw3  WITH ORDHDR.CBW3;
              cFabcom3 WITH ORDHDR.CCOMP3
      REPLACE cFab4    WITH ORDHDR.CFAB4;
              cFabc4   WITH ORDHDR.CCOL4;
              cFabbw4  WITH ORDHDR.CBW4;
              cFabcom4 WITH ORDHDR.CCOMP4
      REPLACE cFab5    WITH ORDHDR.CFAB5;
              cFabc5   WITH ORDHDR.CCOL5;
              cFabbw5  WITH ORDHDR.CBW5;
              cFabcom5 WITH ORDHDR.CCOMP5
      REPLACE cFab6    WITH ORDHDR.CFAB6;
              cFabc6   WITH ORDHDR.CCOL6;
              cFabbw6  WITH ORDHDR.CBW6;
              cFabcom6 WITH ORDHDR.CCOMP6
      *C200291,1 AMH [End]
      
    ENDIF
    
    *C200291,1 AMH Collecting corrected data for header [Start]
    *REPLACE nPoTotQty WITH lnPoTotQty;
            cCustDept WITH lcCustDept;
            cCompos   WITH lcCompos
    REPLACE cCustDept WITH lcCustDept;
            cCustName WITH lcCustName;
            cCusVend  WITH lcCusVend;
            DISSDATE  WITH ldIssDate
    *C200291,1 AMH [End]
    
    *-- Add Po Notes.
    IF llRpPrtPn .AND. SEEK('2'+POSHDR.PO,'CUTPICK') .AND. SEEK('B'+CUTPICK.ORDER,'NOTEPAD')
      APPEND BLANK
      REPLACE cStyType WITH PosHdr.cStyType;
              Po       WITH PosHdr.PO;
              cType    WITH 'N'
      lnI = 0
      
      *C200291,1 AMH Print two line notepad only [Start]
      *FOR lnI = 1 TO MIN(16-lnPosLnCnt,MEMLINES(NotePad.mNotes))
      FOR lnI = 1 TO 2
      *C200291,1 AMH [End]
      
        REPLACE cNotePad WITH cNotePad + IIF(lnI=1,'',CHR(13)) + MLINE(NotePad.mNotes,lnI)
      ENDFOR
      
      *C200291,1 AMH Collecting corrected data for header [Start]
      *REPLACE nPoTotQty WITH lnPoTotQty;
              cCustDept WITH lcCustDept;
              cCompos   WITH lcCompos
      REPLACE cCustDept WITH lcCustDept;
              cCustName WITH lcCustName;
              cCusVend  WITH lcCusVend;
              DISSDATE  WITH ldIssDate
      *C200291,1 AMH [End]
      
    ENDIF
    *-- Add Footer # 2.
    APPEND BLANK
    REPLACE cStyType WITH PosHdr.cStyType;
            Po       WITH PosHdr.PO;
            cType    WITH 'O'
    
    *C200291,1 AMH Collecting corrected data for header [Start]
    *REPLACE nPoTotQty WITH lnPoTotQty;
            cCustDept WITH lcCustDept;
            cCompos   WITH lcCompos
    REPLACE cCustDept WITH lcCustDept;
            cCustName WITH lcCustName;
            cCusVend  WITH lcCusVend;
            DISSDATE  WITH ldIssDate
    IF SEEK('T' + POSHDR.CPACK , 'NOTEPAD')
      STORE 0 TO lnCount,lnLineNo
      DO WHILE .T.
        lnLineNo = lnLineNo + 1
        IF lnLineNo > MEMLINES(NotePad.mNotes)
          EXIT
        ELSE
          IF !EMPTY(MLINE(NotePad.mNotes,lnLineNo))
            REPLACE mPack WITH mPack + IIF(lnLineNo=1,'',CHR(13)) + MLINE(NotePad.mNotes,lnLineNo)
            lnCount = lnCount + 1
          ENDIF
        ENDIF
        IF lnCount = 8
          EXIT
        ENDIF
      ENDDO
    ENDIF
    *C200291,1 AMH [End]
    
    *-- Add Objects.
    IF llRpPic .AND. SEEK('S'+SUBSTR(Posln.style,1,lnMajSize),'Objlink')
      SELECT OBJLINK
      SCAN REST WHILE cObjLnkTyp+cObjLink = 'S'+SUBSTR(Posln.style,1,lnMajSize)
        SELECT (lcTmpPolns)
        APPEND BLANK
        REPLACE cStyType WITH PosHdr.cStyType;
                Po       WITH PosHdr.PO;
                cType    WITH 'Z';
                cObject_id WITH OBJLINK.COBJECT_ID
      ENDSCAN
    ENDIF
    *-- Add Detail lines.
    SELECT PosLn
    STORE 0 TO laTotQty
    SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = ;
                    PosHdr.cStyType+PosHdr.PO FOR TranCd = '1' AND TotQty <> 0
      SCATTER MEMVAR MEMO
      INSERT INTO (lcTmpPolns) FROM MEMVAR
      lnI = 0
      FOR lnI = 1 TO 8
        laTotQty[lnI] = laTotQty[lnI] + EVALUATE('m.Qty'+STR(lnI,1))
      ENDFOR
      SELECT (lcTmpPolns)
      REPLACE cType  WITH 'D';
              cColor WITH SUBSTR(STYLE,lnColorStr,lnColorLen);
              cClrDesc WITH gfCodDes(cColor, 'COLOR')
      = gfRltFld(cColor, @laColor , 'COLOR     ')
      REPLACE cNRFCode WITH lcRltColor
      
      *C200291,1 AMH Collecting corrected data for header [Start]
      *REPLACE nPoTotQty WITH lnPoTotQty;
              cCustDept WITH lcCustDept;
              cCompos   WITH lcCompos
      REPLACE cCustDept WITH lcCustDept;
              cCustName WITH lcCustName;
              cCusVend  WITH lcCusVend;
              DISSDATE  WITH ldIssDate
      *C200291,1 AMH [End]
      
    ENDSCAN
    *-- Add Total line.
    SELECT (lcTmpPolns)
    REPLACE cStyType WITH PosHdr.cStyType;
            Po       WITH PosHdr.PO;
            cType    WITH 'E'
    
    *C200291,1 AMH Collecting corrected data for header [Start]
    *REPLACE nPoTotQty WITH lnPoTotQty;
            cCustDept WITH lcCustDept;
            cCompos   WITH lcCompos
    REPLACE cCustDept WITH lcCustDept;
            cCustName WITH lcCustName;
            cCusVend  WITH lcCusVend;
            DISSDATE  WITH ldIssDate
    *C200291,1 AMH [End]
    
  ENDIF
ENDSCAN

*-- Restore setting of memowidth.
SET MEMOWIDTH TO EVALUATE('lnMemoWdth')

IF USED('CUTPICK')
  USE IN CUTPICK
ENDIF

IF USED('ORDHDR')
  USE IN ORDHDR
ENDIF

IF USED('CUSTDEPT')
  USE IN CUSTDEPT
ENDIF

*C200291,1 AMH close ctktbom [Start]
IF llOpenBom
  USE IN CTKTBOM
ENDIF
*C200291,1 AMH [End]

SELECT PosLn
SET RELATION TO
SELECT PosHdr
SET RELATION TO

SELECT (lcTmpPolns)
LOCATE
SET RELATION TO cStyType+PO INTO PosHdr ADDITIVE
SET RELATION TO STYLE INTO STYLE ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SET RELATION TO cObject_id INTO Objects ADDITIVE

SELECT PosHdr
SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE

llOpenComp = .F.
IF !USED('SYCCOMP')
  =gfOpenFile(gcSysHome+'SYCCOMP','','SH')
  llOpenComp = .T.
ENDIF
SELECT SYCCOMP
SEEK gcAct_Comp

*C200291,1 AMH Print only the company name [Start]
*DECLARE laCompAdd[2,1]
*laCompAdd = ''                     && Array to hold the Company address
lcCompName = cCom_Name             && Variable to hold the Company Name
*lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
*lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
*lcCompFax = cCom_Fax               && Variable to hold the Company Fax
*laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
*laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
*lcCompFax = TRANSFORM(lcCompFax , lcPhonPict) && Fax No. Pic
*lcXphone = TRANSFORM(lcCompPhon , lcPhonPict) && variable hold the phone format to use it
                                              && in the forms befor shifting the array.
*C200291,1 AMH [End]

IF llOpenComp
  USE IN SYCCOMP
ENDIF

SELECT (lcTmpPolns)
lcRpExp = ".T."

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Ahmed Maher
*! Date      : 11/21/2001
*! Purpose   : Get the color information.
*!*************************************************************
*! Calls     : gfItemMask
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfGetColor()
*!*************************************************************
*
FUNCTION lfGetColor

*-- Array hold the segmants specifications of the style code structure.
DECLARE laStySeg[1,1]

*-- Count of the major part.
lnMjorCnt  = gfItemMask("SM")
  
*-- Fill an array with the segments strucure, & loop in it to 
*-- know if there a color segment in the style code strucure.
=gfItemMask(@laStySeg)
FOR lnCnt = lnMjorCnt + 1 TO ALEN(laStySeg,1)
  IF laStySeg[lnCnt , 1] = "C"
    *-- Flag to know if there is color in the style code strucure.
    llColorExt = .T.
    *-- Var. hold the start position of the color segment in the style code strucure.
    lnColorStr = laStySeg[lnCnt , 4]
    *-- Var. hold the color segment lenght in the style code strucure.
    lnColorLen = LEN(laStySeg[lnCnt , 3])
  ENDIF
ENDFOR

*C200291,1 AMH Create cursor to save bmps files [Start]
*!*************************************************************
*! Name      : lfCreatBmp
*! Developer : Ahmed Maher
*! Date      : 02/27/2002
*! Purpose   : Create cursor to save bmps files.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfCreatBmp()
*!*************************************************************
*
FUNCTION lfCreatBmp
PARAMETERS lcRetVal

lcRetVal = .T.
CREATE CURSOR (lcRpTmpBmp) (gBmp1 G,gBmp2 G,gBmp3 G,gBmp4 G,gBmp5 G)
SELECT (lcRpTmpBmp)
APPEND BLANK
APPEND GENERAL gBmp1 FROM (gcBmpHome + "02.BMP")
APPEND GENERAL gBmp2 FROM (gcBmpHome + "01.BMP")
APPEND GENERAL gBmp3 FROM (gcBmpHome + "03.BMP")
APPEND GENERAL gBmp4 FROM (gcBmpHome + "04.BMP")
APPEND GENERAL gBmp5 FROM (gcBmpHome + "05.BMP")
RETURN lcRetVal
*--end of lfCreatBmp.
*C200291,1 AMH [End]