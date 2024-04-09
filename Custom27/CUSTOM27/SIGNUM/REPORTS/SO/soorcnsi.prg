****************************************************************************
* PROG : SOORCNSI.PRG Converted from 26 to 27.
* DATE : 05/16/99 .
* DESC : Custom Order Confirmation FOR SIGNUM.
* AUTH : Adel Mohammed El Gazzar (ADEL)
* REFER TO : (C101525)
****************************************************************************

*-- Get the copmany fax to print it in the FRX.
lcCompFax = SycComp.cCom_Fax
*-- Get the major and nonmajor titles and lengths.
lcMajTitle = ALLTRIM(gfItemMask('HM'))
lnMajorLen = LEN(ALLTRIM(gfItemMask('PM')))
lcNonMajTl = ''
lcNonMajPi = ''
*-- No. of major segments.
lnMajSeg    = gfItemMask('SM')
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = laMajSegs[lnI,4]      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    EXIT
  ENDIF                     
ENDFOR
*-- Create a temp cursor to hold logo of all divisions choosed.
lcTempCur  = gfTempName()
CREATE CURSOR &lcTempCur (cDivision c(6),Gen G)
INDEX ON &lcTempCur..cDivision TAG &lcTempCur OF (gcWorkDir + lcTempCur)
*-- Get the scales.
lcScTemp = gfTempName()
CREATE TABLE (gcWorkDir+lcScTemp) (Order C(6),Store C(8),;
                                  Scale1 C(03),Size1 C(5),Size2 C(5),Size3 C(5),Size4 C(5),Size5 C(5),Size6 C(5),Size7 C(5),Size8 C(5),;
                                  Scale2 C(03),Size9 C(5),Size10 C(5),Size11 C(5),Size12 C(5),Size13 C(5),Size14 C(5),Size15 C(5),Size16 C(5),;
                                  Scale3 C(03),Size17 C(5),Size18 C(5),Size19 C(5),Size20 C(5),Size21 C(5),Size22 C(5),Size23 C(5),Size24 C(5),;
                                  Scale4 C(03),Size25 C(5),Size26 C(5),Size27 C(5),Size28 C(5),Size29 C(5),Size30 C(5),Size31 C(5),Size32 C(5),nCount N(2))
INDEX ON Order+Store TAG (lcScTemp)
SELECT OrdHdr
SCAN FOR &lcRpExp
  WAIT WINDOW 'Collecting data. Please wait......' NOWAIT
  SELECT (lcTempOrd)
  lcScale  = ' '
  lcOrder  = ORDER  
  *-- Now re-initial the used variables.  
  DO WHILE  CORDTYPE + ORDER + STORE = 'O' + lcOrder  
    STORE 0 TO lnCount,lnSclCnt
    lcStore  = Store
    SCAN REST WHILE CORDTYPE + ORDER + STORE = 'O' + lcOrder + lcStore
      IF !SEEK(lcOrder+lcStore,'&lcScTemp')
        INSERT INTO (lcScTemp) (Order,Store) VALUES (lcOrder,lcStore)  
      ENDIF
      lnCount = lnCount +1
      IF Scale <> lcScale AND lnSclCnt <= 4
        *--Another scale
        lnSclCnt = IIF(lnSclCnt <> 4 ,lnSclCnt+1,lnSclCnt)
        lcSclCnt = STR(lnSclCnt,1)
        lcScale  = Scale
        SELECT (lcScTemp)
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
    SELECT (lcScTemp)
    REPLACE ALL FOR Order+Store = lcOrder+lcStore nCount WITH lnCount
    SELECT (lcTempOrd)
  ENDDO
  *--Update the logo file
  SELECT OrdHdr
  IF !SEEK(cDivision , lcTempCur)
    SELECT (lcTempCur)
    APPEND BLANK
    REPLACE cDIVISION WITH ordhdr.cDivision
    lcBmpF = gcBmpHome + ALLTRIM(ordhdr.cDivision)+'.BMP'
    IF FILE(lcBmpF)
      APPEND GENERAL GEN FROM &lcBmpF
    ENDIF
  ENDIF
ENDSCAN 
*--Open SKUTMPL file to be used in the frx.
= gfOpenFile (gcDataDir+'SKUTMPL',gcDataDir+'SKUTMPL','SH')  
SELECT Customer
SET RELATION TO 'S'+IIF(!EMPTY(SkuTmpl),SkuTmpl,'DEF') INTO SKUTMPL ADDITIVE
*-- Select ordhdr file and set a relation to get the relation on division.
SELECT ordhdr
SET RELATION TO cDivision INTO (lcTempCur) ADDITIVE
SET RELATION TO "B"+ORDER INTO NOTEPAD ADDITIVE
*--Open the USE spck_lin file
IF !USED('spck_lin')
  =gfOpenFile(gcDataDir+'spck_lin',gcDataDir+'SPCKLINS','SH')
  SELECT (lcTempOrd)
  SET RELATION TO "S"+ACCOUNT+STYLE  INTO SPCK_LIN ADDI
ELSE
  SELECT (lcTempOrd)
  SET RELATION TO "S"+ACCOUNT+STYLE  INTO SPCK_LIN ADDI
ENDIF  

SELECT (lcTempOrd)
SET SKIP TO SPCK_LIN
SET RELATION TO ORDER+STORE INTO (lcScTemp) ADDITIVE
SELECT ORDHDR
lcSkipExpr = lcSkipExpr + ' , SPCK_LIN'
SET SKIP TO &lcSkipExpr
GO TOP
COUNT FOR &LCRPEXP TO lnTotRec
GO TOP
RETURN
