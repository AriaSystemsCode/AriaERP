*!***************************************************************************
*! Program file  : ICSTKHND.PRG
*! Program desc. : STOCK ON HAND REPORT FOR NORTH BAY
*! Date          : 11/09/2004
*! System        : Aria Advantage Series.
*! Module        : INVENTORY CONTROL (IC)
*! Developer     : NADER NABIL (NNA)
*! Tracking Job Number: C124891
*!***************************************************************************
*! Calls :
*!    Procedures : ....
*!***************************************************************************
*! Passed Parameters  : None
*!***************************************************************************
*! Notes   : ....
*!***************************************************************************
*! Example : DO ICSTKHND
*!***************************************************************************
*! Modifications :
*! B126604,1 NNA 02/20/2005 fix bug that some values not shown under the Belong Columns
*!***************************************************************************
STORE ''  TO lcNonMjr,lcMjrTtl,lcNMjrTl,lcItemTl,lcMjrPct,lcNMjrPt,lcScale,lcTmpClr
STORE ''  TO lcTmpSize , laSizes , lcMajor , lcClrCode 
STORE 0   TO lnstylewid , lnClrPo , lnColorLen

DIME laSizes[1,1]
lcTmpClr  = gfTempName()
lcTmpSize = gfTempName()
lcDataTmp = gfTempName()
lcRprtTmp = gfTempName()

DIME laMajSeg[1,1]
=gfItemMask(@laMajSeg)
lnMajSeg  = gfItemMask('SM')
lnNonSeg  = gfItemMask('SN')  
lcItemTl  = gfItemMask('HI')
lcMjrTtl  = gfItemMask('HM')
lcNMjrTl  = gfItemMask('HN')
lcMjrPct  = gfItemMask('PM')
lcNMjrPt  = gfItemMask('PN')
lcIMjrPt  = gfItemMask('PI')
lnstylewid=LEN(lcMjrPct)

lcRpExp  = STRTRAN(lcRpExp ,'STYLE.STYLE','STYLE.CSTYMAJOR')

*--Create file that will store the default values for the edit screen if not found.
IF !FILE(gcDataDir+'EditVal.DBF')
  CREATE TABLE (gcDataDir+'EditVal') (STYLE C(lnstylewid),COLOR C(30),SIZ C(10),AVG_VAL N(10))
  INDEX ON (STYLE+COLOR+SIZ) TAG ('STYCLR')
ENDIF
*-- open the file that has the default values if found and not used.
IF FILE(gcDataDir+'EditVal.DBF') AND !USED('EditVal')
  =gfOpenFile(gcDataDir+'EditVal',gcDataDir+'STYCLR','SH')
ENDIF

*--If Edit Average sales/Week is required.
IF llRpEditAvg
  =lfEditAvg()
ENDIF

=lfCrTempF()
=lfColData()

SELECT (lcRprtTmp)
LOCATE
DO gfDispRe WITH gcRepHome +gcAct_Appl+ '\ICSTKHND.FRX'

USE IN STYLE_X

*!***************************************************************************
*! Name      : lfCrTempF
*! Developer : NADER NABIL (NNA)
*! Date      : 11/09/2004
*! Purpose   : Create the Temp files that will use to print the report
*!***************************************************************************
*! Passed Parameters  : ............
*!***************************************************************************
*! Calls              : from the Prg.
*!***************************************************************************
*! Example   : =lfCrTempF()
*!***************************************************************************
FUNCTION lfCrTempF

CREATE TABLE (gcWorkDir+lcDataTmp) (STYLE C(lnstylewid),COLOR C(30),SDESC C(10),;
              RpRecNo c(1),RpVal N(10,1))
INDEX ON (STYLE+COLOR) TAG 'STYCOLOR' OF (lcDataTmp)
INDEX ON (STYLE+COLOR+SDESC+RpRecNo) TAG (lcDataTmp) OF (lcDataTmp)

*--Create the report Temp File.
DIMENSION laFileStru[21,4]
STORE '' TO laFileStru
laFileStru[1 , 1] = 'STYLE'
laFileStru[1 , 2] = 'C'
laFileStru[1 , 3] = lnstylewid
laFileStru[1 , 4] = 0

laFileStru[2 , 1] = 'Color'
laFileStru[2 , 2] = 'C'
laFileStru[2 , 3] = 30
laFileStru[2 , 4] = 0

laFileStru[3 , 1] = 'RPRECNO'
laFileStru[3 , 2] = 'C'
laFileStru[3 , 3] = 1
laFileStru[3 , 4] = 0

laFileStru[4 , 1] = 'RPDESC'
laFileStru[4 , 2] = 'C'
laFileStru[4 , 3] = 30
laFileStru[4 , 4] = 0

FOR LNI = 5 TO 21
  laFileStru[LNI , 1] = 'SDesc' + ALLTRIM(STR(LNI-4))
  laFileStru[LNI , 2] = 'C'
  laFileStru[LNI , 3] = 10
  laFileStru[LNI , 4] = 0
ENDFOR

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru + 18,4]
FOR LNI = 1 TO 17
  laFileStru[lnFileStru + LNI , 1] = 'SIZ_VAL' + ALLTRIM(STR(LNI))
  laFileStru[lnFileStru + LNI , 2] = 'N'
  laFileStru[lnFileStru + LNI , 3] = 10
  laFileStru[lnFileStru + LNI , 4] = 1
ENDFOR

laFileStru[lnFileStru + LNI , 1] = 'CLR_TOTAL'
laFileStru[lnFileStru + LNI , 2] = 'N'
laFileStru[lnFileStru + LNI , 3] = 10
laFileStru[lnFileStru + LNI , 4] = 0

CREATE TABLE (gcWorkDir+lcRprtTmp) FROM ARRAY laFileStruc
INDEX ON (STYLE+COLOR+RPRECNO) TAG (lcRprtTmp) OF (lcRprtTmp)
             
*--End of function lfCrTempF.
*!***************************************************************************
*! Name      : lfColData
*! Developer : NADER NABIL (NNA)
*! Date      : 11/09/2004
*! Purpose   : Collecting data for the Report
*!***************************************************************************
*! Passed Parameters  : ............
*!***************************************************************************
*! Calls              : from the Prg.
*!***************************************************************************
*! Example   : =lfColData()
*!***************************************************************************
FUNCTION lfColData
PRIVATE lcScale , lcSize , lnUnAlo , lnWipUnAlo , lnAdjAvg , lnWOnHnd1 , lnWOnHnd2
PRIVATE lnSclCnt , lcClrDsc
STORE 0 TO lnUnAlo , lnWipUnAlo , lnAdjAvg , lnWOnHnd1 , lnWOnHnd2 , lnSclCnt
STORE '' TO lcScale , lcSize , lcClrDsc

SELECT STYLE 
SET ORDER TO TAG STYLE
SET RELATION TO 'S' + scale INTO Scale ADDITIVE

IF !USED('STYLE_X')
  USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG CStyle IN 0
ENDIF
SELECT STYLE_X
SET RELATION TO LEFT(Style_X.Style,lnStyleWid) INTO Style ADDITIVE
 = lfNonMaj()
*-- scan within style_x for style Major that selected in the Option Grid.
SCAN FOR &LCRPEXP

  SELECT STYLE
  *--Scan For style colors
  SCAN REST WHILE STYLE = LEFT(Style_X.cStyMajor,lnStyleWid)
    lCScale = STYLE.Scale
    SELECT SCALE
    =SEEK('S' + lcScale)
    lnSclCnt = SCALE.CNT
    *--Loop for Scale count to get its Sizes (Desc. from the Scale.Sz,UnAlo.&Wip from the Style file)
    *--Then get the Adj Avg Sales/Week from the Editval file if it has data for this style
    FOR lnSclSiz = 1 TO lnSclCnt
      LCI = STR(lnSclSiz,1)
      lcSize = ALLTRIM(Scale.cDim1) + 'x' + ALLTRIM(Scale.SZ&LCI)
      SELECT STYLE
      lnUnAlo    = STK&LCI - ALO&LCI
      lnWipUnAlo = lnUnAlo + WIP&LCI
      lcNonMjr   = SUBSTR(STYLE.Style,lnClrPo,lnColorLen)
      *-- Get the Color Description from Codes File.
      SELECT CODES
      IF SEEK('N'+PADR('COLOR',FSIZE('CFLD_NAME'),' ')+ALLTRIM(lcNonMjr))
        lcClrDsc = ALLTRIM(CDISCREP)
      ENDIF
      *--get the Adj. average Sales/Weeks from EditVal File if this Style Found in it.
      IF SEEK(LEFT(Style_X.Style,lnStyleWid)+PADR(lcClrDsc,30,' ')+lcSize,'EditVal')
        SELECT EditVal
        lnAdjAvg = EditVal.Avg_Val
      ELSE
        lnAdjAvg = 0
      ENDIF
      lnWOnHnd1  = IIF((lnAdjAvg) > 0 , ROUND((lnWipUnAlo / lnAdjAvg),1) , 0)
      lnWOnHnd2  = IIF((lnAdjAvg) > 0 , ROUND((lnUnAlo / lnAdjAvg),1) , 0)
      *-- Replace the collected data in the Temp file
      SELECT (lcDataTmp)
      *B126604,1 NNA 02/20/2005 (Begin) Insert into file only the positive values and sizes that
      *B126604,1 NNA             have value the Editval File
      *IF !SEEK(LEFT(Style_X.Style,lnStyleWid)+lcClrDsc+lcSize) AND ;
        (LNUNALO + lnWipUnAlo + LNADJAVG + lnWOnHnd1 + lnWOnHnd2)<>0
      IF !SEEK(LEFT(Style_X.Style,lnStyleWid)+lcClrDsc+lcSize) AND ;
        (LNUNALO + lnWipUnAlo + LNADJAVG + lnWOnHnd1 + lnWOnHnd2)>0 AND lnAdjAvg > 0
      *B126604,1 NNA (End)
        lnVal1 = ROUND(lnWipUnAlo,0)
        lnVal2 = ROUND(LNADJAVG,0)
        lnVal3 = lnWOnHnd1
        lnVal4 = ROUND(LNUNALO,0)
        lnVal5 = lnWOnHnd2                              
        *-Loop 5 times to input the 5th types :
        *--1 - 'ATSHIP + WIP'
        *--2 - 'ADJ AVG SLS/WK'
        *--3 - 'WEEKS ON HAND'
        *--4 - 'AVAIL TO SHIP'
        *--5 - 'WEEKS ON HAND'
        FOR I = 1 TO 5
          LCI = STR(I,1)
          APPEND BLANK
          REPLACE STYLE   WITH LEFT(Style_X.Style,lnStyleWid) ,;
                  COLOR   WITH lcClrDsc                       ,;
                  SDESC   WITH lcSize                         ,;
                  RpRecNo WITH LCI                            ,;
                  RPVAL   WITH LNVAL&LCI
        ENDFOR
      ENDIF
    ENDFOR  
  ENDSCAN
  STORE 0 TO lnUnAlo , lnWipUnAlo , lnAdjAvg , lnWOnHnd1 , lnWOnHnd2 , lnSclCnt
ENDSCAN
SELECT STYLE_X
SET RELATION TO 
=lfHorData()
RETURN
*--End of function lfColData.
*!***************************************************************************
*! Name      : lfHorData
*! Developer : NADER NABIL (NNA)
*! Date      : 11/09/2004
*! Purpose   : get data from lcDataTmp file horizontally into lcRprtTmp File 
*!***************************************************************************
*! Passed Parameters  : ............
*!***************************************************************************
*! Calls              : from the Prg.
*!***************************************************************************
*! Example   : =lfHorData()
*!***************************************************************************
FUNCTION lfHorData
PRIVATE lcRpRec1 ,lcRpRec2,lcRpRec3,lcRpRec4 , lcRpRec5
STORE '' TO lcRpRec1 ,lcRpRec2,lcRpRec3,lcRpRec4 , lcRpRec5
lcRpRec1 = 'ATSHIP + WIP'
lcRpRec2 = 'ADJ AVG SLS/WK'
lcRpRec3 = 'WEEKS ON HAND'
lcRpRec4 = 'AVAIL TO SHIP'
lcRpRec5 = 'WEEKS ON HAND'
SELECT (lcDataTmp)
SET ORDER TO STYCOLOR
lcColor = ''
lcStyle = ''
lcSDesc = ''
LNI     = 0
lnSzNo  = 0
SCAN
  IF !((lcStyle + ALLTRIM(lcColor) + lcSDesc)==(&lcDataTmp..STYLE +;
       ALLTRIM(&lcDataTmp..COLOR) + &lcDataTmp..SDESC))
    *--Empty lnSzNo (has the size No. inside the color) if Color Changed.
    IF !((lcStyle + ALLTRIM(lcColor))==(&lcDataTmp..STYLE + ALLTRIM(&lcDataTmp..COLOR)))
      lnSzNo  = 0
    ENDIF
    lcStyle = &lcDataTmp..STYLE 
    lcColor = &lcDataTmp..COLOR
    lcSDesc = &lcDataTmp..SDESC
    LNI     = 1
    lnSzNo  = lnSzNo + 1
    lcSzNo  = ALLTRIM(STR(lnSzNo))
  ELSE
    LNI     = LNI + 1
  ENDIF
  LCI = STR(LNI,1)    
  SELECT (lcRprtTmp)
  IF !SEEK(&lcDataTmp..STYLE + &lcDataTmp..COLOR + &lcDataTmp..RpRecNo)
    APPEND BLANK
    REPLACE &lcRprtTmp..STYLE   WITH &lcDataTmp..STYLE       ,;
            &lcRprtTmp..COLOR   WITH &lcDataTmp..COLOR       ,;
            &lcRprtTmp..RPRecNo WITH &lcDataTmp..RpRecNo     ,;
            &lcRprtTmp..RPDESC  WITH lcRpRec&LCI             ,;
            &lcRprtTmp..SDesc&lcSzNo WITH &lcDataTmp..SDESC  ,;
            &lcRprtTmp..SIZ_VAL&lcSzNo WITH &lcDataTmp..RpVal,;
            &lcRprtTmp..CLR_TOTAL WITH CLR_TOTAL + &lcDataTmp..RpVal                        
  ELSE
    *--If sizes are more than 17 sizes ,collect all the rest sizes under the last size (17)
    IF lnSzNo <= 17
      REPLACE &lcRprtTmp..SDesc&lcSzNo WITH &lcDataTmp..SDESC  ,;
              &lcRprtTmp..SIZ_VAL&lcSzNo WITH &lcDataTmp..RpVal,;   
              &lcRprtTmp..CLR_TOTAL WITH CLR_TOTAL + &lcDataTmp..RpVal            
    ELSE
      REPLACE &lcRprtTmp..SIZ_VAL17 WITH &lcRprtTmp..SIZ_VAL17 + &lcDataTmp..RpVal
    ENDIF
  ENDIF 
ENDSCAN

*--End of function lfHorData.
*!***************************************************************************
*! Name      : lfEditAvg
*! Developer : NADER NABIL (NNA)
*! Date      : 11/09/2004
*! Purpose   : Edit the Average sales per Week if user select 'Yes'.
*!***************************************************************************
*! Passed Parameters  : ............
*!***************************************************************************
*! Call               : ARIABROW() , lfvEdtAvg()
*!***************************************************************************
*! Example   : =lfEditAvg()
*!***************************************************************************
FUNCTION lfEditAvg
lcBrFields = [cStyMajor :H="Style" :25,Desc:15:H="Description",]+;
             [Season:15:H="Season",cdivision:15:H="Division",Pricea :h="Price" :15]

SELECT STYLE
SET ORDER TO TAG CSTYLE
GO TOP
= ARIABROW([FOR &LCRPEXP],'Edit Average Sales/Week',gnbrfsrow1,gnbrfscol1,gnbrfsrow2,gnbrfscol2,'lfvEdtAvg',;
             'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;\<Edit;\<Ok' )
SELECT STYLE
SET RELATION TO
RETURN
*--End of FUNCTION lfEditAvg.
*!***************************************************************************
*! Name      : lfvEdtAvg
*! Developer : NADER NABIL (NNA)
*! Date      : 11/09/2004
*! Purpose   : Modify Average Sales/Week, Valid of edit Avg. Sales.
*!***************************************************************************
*! Passed Parameters  : ............
*!***************************************************************************
*! Calls     : From the Edit screen
*!***************************************************************************
*! Returns            : ............
*!***************************************************************************
*! Example   : =lfvEdtAvg()
*!***************************************************************************
FUNCTION lfvEdtAvg
PRIVATE lnCurrRec, lcCurrSty
llOk = .F.
lcBrTtlZ  = "Color'S edit Average Sales/Week"
lcMajor = PADR(STYLE.CSTYMAJOR,lnstylewid)             
lcScale = SUBSTR(STYLE.SCALE,1,1)

PUSH KEY
ON KEY LABEL ALT+B SHOW WINDOW (lcBrTtlZ)
ON KEY LABEL ESC llDummy = .T.
DO (gcRepHome+"IC\ICEDIT.SPX")
POP KEY
IF llOk			&& if used pressed OK
  SELECT(lcTmpClr)
  LOCATE
  IF SEEK(LCMAJOR , 'EditVal')
    SCAN
      SCATTER MEMVAR MEMO
      SELECT EditVal
      *-- Take sizes from lcTmpClr horizontally and update it in EditVal vertically
      FOR I = 1 TO (FCOUNT(lcTmpClr)-2)/2
        LCI = ALLTRIM(STR(I))
        *-- Save only sizes that have Value greater than Zero
        IF m.SV&LCI > 0
          IF SEEK(LCMAJOR+&lcTmpClr..COLOR + &lcTmpClr..SDesc&LCI,'EditVal')
            REPLACE AVG_VAL WITH m.SV&LCI
          ELSE
            APPEND BLANK
            REPLACE EditVal.STYLE   WITH LCMAJOR              
            REPLACE EditVal.COLOR   WITH &lcTmpClr..COLOR     
            REPLACE EditVal.SIZ     WITH &lcTmpClr..SDesc&LCI
            REPLACE EditVal.AVG_VAL WITH m.SV&LCI
          ENDIF
        ELSE  && If user Change the size Qty. to be Zero.
          IF SEEK(LCMAJOR+&lcTmpClr..COLOR + &lcTmpClr..SDesc&LCI,'EditVal')
            REPLACE AVG_VAL WITH m.SV&LCI
            DELETE
          ENDIF        
        ENDIF
      ENDFOR           
    ENDSCAN
  ELSE		&& IF user select another style/colors against the default (the Previous style/color)
    SELECT(lcTmpClr)
    SCAN
      SCATTER MEMVAR MEMO
      SELECT EditVal
      *-- Take sizes from lcTmpClr horizontally and update it in EditVal vertically
      FOR I = 1 TO (FCOUNT(lcTmpClr)-2)/2
        LCI = ALLTRIM(STR(I))
        *-- Save only sizes that have Value greater than Zero
        IF m.SV&LCI > 0
          IF SEEK(LCMAJOR+&lcTmpClr..COLOR + &lcTmpClr..SDesc&LCI,'EditVal')
            REPLACE AVG_VAL WITH m.SV&LCI
          ELSE
            APPEND BLANK
            REPLACE STYLE WITH LCMAJOR  
            REPLACE COLOR WITH &lcTmpClr..COLOR
            REPLACE SIZ   WITH &lcTmpClr..SDesc&LCI
            REPLACE AVG_VAL WITH m.SV&LCI
          ENDIF
        ENDIF
      ENDFOR           
    ENDSCAN
  ENDIF
ENDIF
SELECT STYLE
SET ORDER TO TAG CSTYLE
RETURN
*--End of FUNCTION lfvEdtAvg.
*!***************************************************************************
*! Name      : lfDisBrow2
*! Developer : NADER NABIL (NNA)
*! Date      : 11/09/2004
*! Purpose   : To browse the allocated records.
*!***************************************************************************
*! Calls     : From the screen
*!***************************************************************************
*! Example   : = lfDisBrow2()
*!***************************************************************************
FUNCTION lfDisBrow2
PRIVATE lcClrCode
STORE '' TO lcClrCode

SELECT STYLE
SET ORDER TO TAG STYLE
*--Creat Color's Temp file.
= LFCRTTMPED()

*--get the Color Position and length.
SELECT STYLE
= lfNonMaj()

SCAN REST WHILE SUBSTR(STYLE.Style,1,lnStyleWid) = lcMajor
  lcNonMjr = SUBSTR(STYLE.Style,lnClrPo,lnColorLen)
  IF !(lcClrCode == lcNonMjr)
    lcClrCode = lcNonMjr
    SELECT CODES
    IF SEEK('N'+PADR('COLOR',FSIZE('CFLD_NAME'),' ')+ALLTRIM(lcNonMjr))
      lcClrDsc = ALLTRIM(CDISCREP)
      SELECT (lcTmpClr)
      IF !SEEK(lcClrDsc)
        APPEND BLANK
        REPLACE COLOR WITH lcClrDsc
        FOR I = 1 TO ALEN(LASIZES,1)
          LCI = ALLTRIM(STR(I))
          REPLACE &lcTmpClr..SDESC&LCI WITH LASIZES[1,I]
        ENDFOR
      ENDIF
    ENDIF
  ENDIF
ENDSCAN

SELECT(lcTmpClr)
SCAN
  IF SEEK(lcMajor + &lcTmpClr..COLOR , 'EditVal')
    SELECT EditVal
    SCAN REST WHILE STYLE+COLOR+SIZ = lcMajor + &lcTmpClr..COLOR
      SCATTER MEMVAR MEMO
      SELECT (lcTmpClr)
      FOR I = 1 TO (FCOUNT(lcTmpClr)-2)/2
        LCI = ALLTRIM(STR(I))
        IF &lcTmpClr..SDESC&LCI = EditVal.SIZ
          REPLACE &lcTmpClr..SV&LCI WITH EditVal.AVG_VAL
        ENDIF
      ENDFOR
    ENDSCAN
  ENDIF
ENDSCAN

SELECT(lcTmpClr)
LOCATE
*-- lnFldCount = (Fields count - fields for style and color ) / 2 to get count of fields that have
*--              the sizes values not description
lnFldCount = (FCOUNT() - 2)/2
lcFldName  = ''
lcFldName1 = ''
lcFldName2  = ''
lcFldName3  = ''

FOR I = 1 TO lnFldCount
  lcNo = ALLTRIM(STR(I))
  lcFldName = lcFldName + ',' + 'SV&lcNo' +':H=' + "'" + ALLTRIM(lASizes[1,I]) + "'"
ENDFOR
lcDistBrow = "COLOR:R:H='Color':30" + lcFldName 
BROWSE FIELDS &lcDistBrow ;
       WINDOW ICEDIT2 IN WINDOW ICEDIT;
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOMENU;
       NOWAIT;
       SAVE;
       WHEN lfwBrows();
       TITLE lcBrTtlZ

*!***************************************************************************
*! Name      : lfCrtTmpEd
*! Developer : NADER NABIL (NNA)
*! Date      : 11/09/2004
*! Purpose   : Create Temp file for edit colors Avg. Sales/Week.
*!***************************************************************************
*! Passed Parameters  : ............
*!***************************************************************************
*! Call      : none
*!***************************************************************************
*! Example   : =lfCrtTmpEd()
*!***************************************************************************
FUNCTION lfCrtTmpEd
PRIVATE lnOldAlias
STORE 0 TO lnOldAlias
lnOldAlias = SELECT(0)
CREATE TABLE (gcWorkDir+lcTmpSize) (CSIZE C(10))
INDEX ON CSIZE TAG (lcTmpSize) OF (lcTmpSize)
SELECT SCALE
IF SEEK('S' + SUBSTR(STYLE.SCALE,1,1))
  SCAN REST WHILE TYPE+SCALE = 'S' + SUBSTR(STYLE.SCALE,1,1)
    FOR lnSiz = 1 TO SCALE.CNT
      lcSiz = STR(lnSiz,1)
      lcSizDesc = SUBSTR(SCALE.SCALE,2,2) + 'x' + ALLTRIM(SZ&lcSiz)
      SELECT(LCTMPSIZE)
      IF !SEEK(lcSizDesc)
        APPEND BLANK
        REPLACE CSIZE WITH lcSizDesc
      ENDIF
      SELECT SCALE
    ENDFOR
  ENDSCAN
ENDIF

SELECT(LCTMPSIZE)
SELECT * FROM &LCTMPSIZE INTO ARRAY laSizes

LNI = 2 + RECCOUNT(LCTMPSIZE)
DIMENSION laFileStru[lnI,4]
STORE '' TO laFileStru
laFileStru[1 , 1] = 'STYLE'
laFileStru[1 , 2] = 'C'
laFileStru[1 , 3] = lnstylewid
laFileStru[1 , 4] = 0

laFileStru[2 , 1] = 'Color'
laFileStru[2 , 2] = 'C'
laFileStru[2 , 3] = 30
laFileStru[2 , 4] = 0

FOR LNZ = 3 TO LNI
  laFileStru[LNZ , 1] = 'SDesc' + ALLTRIM(STR(LNZ-2))
  laFileStru[LNZ , 2] = 'C'
  laFileStru[LNZ , 3] = 10
  laFileStru[LNZ , 4] = 0
ENDFOR

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru + RECCOUNT(LCTMPSIZE) ,4]
FOR LNZ = 1 TO RECCOUNT(LCTMPSIZE)
  laFileStru[lnFileStru + LNZ , 1] = 'SV' + ALLTRIM(STR(LNZ))
  laFileStru[lnFileStru + LNZ , 2] = 'N'
  laFileStru[lnFileStru + LNZ , 3] = 10
  laFileStru[lnFileStru + LNZ , 4] = 0
ENDFOR

CREATE TABLE (gcWorkDir+lcTmpClr) FROM ARRAY laFileStruc
INDEX ON COLOR TAG (lcTmpClr) OF (lcTmpClr)

SELECT(lnOldAlias)
*--End function of lfCrtTmpEd.
*!***************************************************************************
*! Name      : lfNonMaj
*! Developer : NADER NABIL (NNA)
*! Date      : 11/09/2004
*! Purpose   : To get the style nonmajor segement structure
*!***************************************************************************
*! Called from : Option Grid
*!***************************************************************************
*! Calls       : ....
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************
*! Example     : = lfNonMaj()
*!***************************************************************************

FUNCTION lfNonMaj

PRIVATE lnOldAlias
STORE 0 TO lnOldAlias
lnOldAlias = SELECT(0)

*-- Compute Free/Color Items in Style Structure. [Begin]
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)

  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')

    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT
    ELSE
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.
      
    ENDIF

  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
SELECT(lnOldAlias)
RETURN ''
*--End function of lfNonMaj.
*!***************************************************************************
*! Name      : lfwBrows
*! Developer : NADER NABIL (NNA)
*! Date      : 11/09/2004
*! Purpose   : When Browse Temp. sequence file fn.
*!***************************************************************************
*! Passed Parameters  : ....
*!***************************************************************************
*! Returns            : None
*!***************************************************************************
*! Example   : = lfwBrows()
*!***************************************************************************
FUNCTION lfwBrows

SELECT(lcTmpClr)
SHOW WINDOW (lcBrTtlZ) REFRESH

*!***************************************************************************
*! Name      : lfsrSty
*! Developer : NADER NABIL (NNA)
*! Date      : 11/09/2004
*! Purpose   : Set and Rest functions for style filter.
*!***************************************************************************
*! Passed Parameters  : None
*!***************************************************************************
*! Returns            : None
*!***************************************************************************
*! Example   : =lfsrSty()
*!***************************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!***************************************************************************
FUNCTION lfSRSty
PARAMETERS lcParm
IF lcParm = 'S'  && Set code
  *-- open this file in another alias to set order to Style Major 
  *-- unique index.
  USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
  SELECT STYLE
  SET ORDER TO TAG Cstyle
  SET RELATION TO STYLE.STYLE INTO STYLE_X
  GO TOP IN STYLE
ELSE  && Reset code
  USE IN STYLE_X
  SELECT STYLE
  SET ORDER TO TAG STYLE
ENDIF
*-- End of Function lfsrvSty.

*!***************************************************************************
*! Name      : lfStySum
*! Developer : NADER NABIL (NNA)
*! Date      : 11/09/2004
*! Purpose   : sum a specific field for the current style in style file
*!***************************************************************************
*! Passed Parameters  : None
*!***************************************************************************
*! Returns            : Calculated field value.
*!***************************************************************************
*! Example   : =lfStySum()
*!***************************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) == ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  
  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- End of Function lfStySum.
