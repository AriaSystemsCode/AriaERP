****************************************************************************
* PROG : ARPINVSI.PRG    (Converted from 26 to 27)
* AUTH : Adel Mohammed El Gazzar (ADEL)
* DATE : 05/9/99
* DESC : PRINT INVOICES 
* Notes: This report is copied from the standard report format K 
*        customized for SIGNUM INTERNATIONAL.
* Ref  : (C101523)
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
CREATE TABLE (gcWorkDir+lcScTemp) (Invoice C(6),;
                                  Scale1 C(03),Size1 C(5),Size2 C(5),Size3 C(5),Size4 C(5),Size5 C(5),Size6 C(5),Size7 C(5),Size8 C(5),;
                                  Scale2 C(03),Size9 C(5),Size10 C(5),Size11 C(5),Size12 C(5),Size13 C(5),Size14 C(5),Size15 C(5),Size16 C(5),;
                                  Scale3 C(03),Size17 C(5),Size18 C(5),Size19 C(5),Size20 C(5),Size21 C(5),Size22 C(5),Size23 C(5),Size24 C(5),;
                                  Scale4 C(03),Size25 C(5),Size26 C(5),Size27 C(5),Size28 C(5),Size29 C(5),Size30 C(5),Size31 C(5),Size32 C(5),nCount N(2))
INDEX ON Invoice TAG (lcScTemp)
SELECT INVHDR
SET RELATION TO INVOICE INTO (lcScTemp) ADDITIVE
SCAN FOR &lcRpExp
  WAIT WINDOW 'Collecting data. Please wait......' NOWAIT
  *-- Skip the invoice having SKIP TO record.
  IF Invoice <> InvLine.Invoice
    LOOP
  ENDIF
  STORE 0 TO lnCount,lnSclCnt
  lcScale   = ' '
  SELECT INVLINE
  INSERT INTO (lcScTemp) (INVOICE) VALUES (INVLINE.INVOICE)
  SCAN WHILE INVOICE = INVHDR.INVOICE
    lnCount = lnCount +1
    IF Scale <> lcScale AND lnSclCnt <= 4
      *--Another scale
      lnSclCnt = lnSclCnt+IIF(lnSclCnt<4,1,0)
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
  REPLACE ALL FOR INVOICE = INVHDR.INVOICE nCount WITH lnCount
  *--Update the logo file
  SELECT INVHDR
  IF !SEEK(cDivision , lcTempCur)
    SELECT (lcTempCur)
    APPEND BLANK
    REPLACE cDIVISION WITH InvHdr.cDivision
    lcBmpF = gcBmpHome + ALLTRIM(InvHdr.cDivision)+'.BMP'
    IF FILE(lcBmpF)
      APPEND GENERAL GEN FROM &lcBmpF
    ENDIF
  ENDIF
ENDSCAN
*-- Select invhdr file
SELECT INVHDR
SET RELATION TO cDivision INTO (lcTempCur) ADDITIVE
RETURN
