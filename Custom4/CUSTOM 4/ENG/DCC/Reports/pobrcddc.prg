*:************************************************************************
*: Program file  : pobrcddc.PRG
*: Program desc. : Print Bar Codes for receiving P/Os. For DCC
*:         System: ARIA APPAREL SYSTEM 4xp
*:         Module: Purchase Orders (PO).
*:      Developer: Mariam Mazhar [MMT]
*:           Date: 10/09/2011 (C201404.EXE){T20110902.0001}
*:************************************************************************
*! Modifications:
*! C201404,1 10/27/2011 MMT Print each PO Barcodes in separate page[T20110902.0001]
*:************************************************************************
IF lcpotyp='M'
  RETURN
ENDIF
loOgScroll.cCRorientation = 'P'
PRIVATE lnclrlen, lnclrpos
STORE 0 TO lnclrlen, lnclrpos, lnstylngl, lnstyposgl, lnrec1, lnrec2
PRIVATE lcoldselec
lcoldselec = SELECT()
STORE SPACE(0) TO lcclrdesc, latempstru
= lfclrdesc()
lctmpbars = gftempname()
DIMENSION latempstru[1, 18]
lnfildlen = AFIELDS(latempstru)
IF EMPTY(FIELD('cClrDesc'))
  lntmpstru = ALEN(latempstru, 1)
  DIMENSION latempstru[lnfildlen+12, 18]
  latempstru[lnfildlen+1, 1] = 'cClrDesc'
  latempstru[lnfildlen+1, 2] = 'C'
  latempstru[lnfildlen+1, 3] = 30
  latempstru[lnfildlen+1, 4] = 0
  latempstru[lnfildlen+2, 1] = 'cSty'
  latempstru[lnfildlen+2, 2] = 'C'
  latempstru[lnfildlen+2, 3] = 35
  latempstru[lnfildlen+2, 4] = 0
  latempstru[lnfildlen+3, 1] = 'cStyDesc'
  latempstru[lnfildlen+3, 2] = 'C'
  latempstru[lnfildlen+3, 3] = 33
  latempstru[lnfildlen+3, 4] = 0
  latempstru[lnfildlen+4, 1] = 'cPo'
  latempstru[lnfildlen+4, 2] = 'C'
  latempstru[lnfildlen+4, 3] = 6
  latempstru[lnfildlen+4, 4] = 0
  latempstru[lnfildlen+5, 1] = 'cClrDesc1'
  latempstru[lnfildlen+5, 2] = 'C'
  latempstru[lnfildlen+5, 3] = 30
  latempstru[lnfildlen+5, 4] = 0
  latempstru[lnfildlen+6, 1] = 'cSty1'
  latempstru[lnfildlen+6, 2] = 'C'
  latempstru[lnfildlen+6, 3] = 35
  latempstru[lnfildlen+6, 4] = 0
  latempstru[lnfildlen+7, 1] = 'cStyDesc1'
  latempstru[lnfildlen+7, 2] = 'C'
  latempstru[lnfildlen+7, 3] = 33
  latempstru[lnfildlen+7, 4] = 0
  latempstru[lnfildlen+8, 1] = 'cPo1'
  latempstru[lnfildlen+8, 2] = 'C'
  latempstru[lnfildlen+8, 3] = 6
  latempstru[lnfildlen+8, 4] = 0
  latempstru[lnfildlen+9, 1] = 'cClrDesc2'
  latempstru[lnfildlen+9, 2] = 'C'
  latempstru[lnfildlen+9, 3] = 30
  latempstru[lnfildlen+9, 4] = 0
  latempstru[lnfildlen+10, 1] = 'cSty2'
  latempstru[lnfildlen+10, 2] = 'C'
  latempstru[lnfildlen+10, 3] = 35
  latempstru[lnfildlen+10, 4] = 0
  latempstru[lnfildlen+11, 1] = 'cStyDesc2'
  latempstru[lnfildlen+11, 2] = 'C'
  latempstru[lnfildlen+11, 3] = 33
  latempstru[lnfildlen+11, 4] = 0
  latempstru[lnfildlen+12, 1] = 'cPo2'
  latempstru[lnfildlen+12, 2] = 'C'
  latempstru[lnfildlen+12, 3] = 6
  latempstru[lnfildlen+12, 4] = 0
  FOR lnfildlen = 1 TO ALEN(latempstru, 1)-lntmpstru
    STORE .F. TO latempstru[lntmpstru+lnfildlen, 5], latempstru[lntmpstru+lnfildlen, 6]
    STORE '' TO latempstru[lntmpstru+lnfildlen, 7], latempstru[lntmpstru+lnfildlen, 8], latempstru[lntmpstru+lnfildlen, 9], latempstru[lntmpstru+lnfildlen, 10], latempstru[lntmpstru+lnfildlen, 11], latempstru[lntmpstru+lnfildlen, 12], latempstru[lntmpstru+lnfildlen, 13], latempstru[lntmpstru+lnfildlen, 14], latempstru[lntmpstru+lnfildlen, 15], latempstru[lntmpstru+lnfildlen, 16]
    STORE 0 TO latempstru[lntmpstru+lnfildlen, 17], latempstru[lntmpstru+lnfildlen, 18]
  ENDFOR
ENDIF
= gfcrttmp(lctmpbars, @latempstru)
SELECT (lcoldselec)
USE
llRdate = .F.
llTdate = .F.
Ldate = {}
Hdate = {}

IF lcRpBasdOn = 'R'
  lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,'POSLN.DATE'),1)
  IF lnPosition > 0 .AND. !EMPTY(LOOGSCROLL.laOGFxFlt[lnPosition,6])
    Ldate = CTOD(SUBSTR(laOGFxFlt[lnPosition,6],1,10))
    Hdate = CTOD(SUBSTR(laOGFxFlt[lnPosition,6],12,21))
    llRdate  = .T.
  ENDIF
ELSE
  lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,'POSHDR.ENTERED'),1)
  IF lnPosition > 0 .AND. !EMPTY(LOOGSCROLL.laOGFxFlt[lnPosition,6])
    Ldate = CTOD(SUBSTR(laOGFxFlt[lnPosition,6],1,10))
    Hdate = CTOD(SUBSTR(laOGFxFlt[lnPosition,6],12,21))
    llTdate = .T.
  ENDIF
ENDIF


llUsePo = .F.
lcPOFile  = ""
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,IIF(lcPoTyp = "M",'POSHDR.PO1',IIF(lcPoTyp = "P",'POSHDR.PO','POSHDR.PO2'))),1)
IF lnPosition > 0
  lcPOFile = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  llUsePo  = IIF(!EMPTY(lcPOFile) .AND. USED(lcPOFile) .AND. RECCOUNT(lcPOFile) > 0,.T.,.F.)
ENDIF
lnLbCount = 1
IF llUsePo
  SELECT (lcPOFile)
  SCAN
    IF loDBFPOSHDR.SEEK(IIF(lcPoTyp = "M",'PU',IIF(lcPoTyp = "P",'PP','NN'))+&lcPOFile..PO) AND loDBFPOSLN.SEEK(IIF(lcPoTyp = "M",'PU',IIF(lcPoTyp = "P",'PP','NN'))+&lcPOFile..PO)
      SELECT posln
      lnlbcount = 1
      SCAN REST WHILE cbusdocu+cstytype+po = IIF(lcPoTyp = "M",'PU',IIF(lcPoTyp = "P",'PP','NN'))+&lcPOFile..PO ;
        FOR IIF(lcRpBasdOn = 'R',TranCd = '2',TranCd = '1')  AND IIF(lcRpBasdOn = 'R' AND llRdate,BETWEEN(POSLN.DATE,Ldate,Hdate),IIF(lcRpBasdOn = 'B' AND llTdate,BETWEEN(POSHDR.ENTERED,Ldate,Hdate),.T.));
        AND Poshdr.STATUS <> 'X' AND IIF(lcrpbasdon = "R",POSHDR.Receive > 0,.T.)
        lcstyle = PADR(posln.STYLE, lnstylen)
        lcstycd = lfremsepr(lcstyle)
        = SEEK(lcstyle, 'STYLE')
        = SEEK('S'+STYLE.SCALE, 'SCALE')
        FOR i = 1 TO SCALE.CNT
          lcsz = STR(i, 1)
          IF posln.qty&lcsz > 0
            lczdesc = SCALE.sz&lcsz
            lcstysz = lcstyle+lcsz
            lnqty   = CEILING(posln.qty&lcsz * (lnrpperc/100))
            SELECT (lctmpbars)
            FOR lnqtycount = 1 TO lnqty
              IF lllibinst
                IF lnlbcount=1
                  APPEND BLANK
                ENDIF
                DO CASE
                  CASE lnlbcount=1
                    REPLACE ccode WITH lcstysz,;
                    cstyle WITH lcstycd+lcsz,;
                    cclrdesc WITH gfcoddes(SUBSTR(STYLE.STYLE, lnclrpos, lnclrlen), 'COLOR'),;
                    cstydesc WITH LEFT(STYLE.desc1, 33),;
                    csty WITH LEFT(ALLTRIM(STYLE.cstymajor), 12)+' '+IIF(EMPTY(SCALE.cdim1) .AND. EMPTY(SCALE.cdim2), '', LEFT(ALLTRIM(SCALE.cdim1), 4))+' '+lczdesc,;
                    cpo WITH posln.po
                  CASE lnlbcount=2
                    REPLACE ccode1 WITH lcstysz,;
                    cstyle1 WITH lcstycd+lcsz,;
                    cclrdesc1 WITH gfcoddes(SUBSTR(STYLE.STYLE, lnclrpos, lnclrlen), 'COLOR'),;
                    cstydesc1 WITH LEFT(STYLE.desc1, 33), ;
                    csty1 WITH LEFT(ALLTRIM(STYLE.cstymajor), 12)+' '+IIF(EMPTY(SCALE.cdim1) .AND. EMPTY(SCALE.cdim2), '', LEFT(ALLTRIM(SCALE.cdim1), 4))+' '+lczdesc,;
                    cpo1 WITH posln.po
                  CASE lnlbcount=3
                    REPLACE ccode2 WITH lcstysz,;
                    cstyle2 WITH lcstycd+lcsz,;
                    cclrdesc2 WITH gfcoddes(SUBSTR(STYLE.STYLE, lnclrpos, lnclrlen), 'COLOR'), cstydesc2 WITH LEFT(STYLE.desc1, 33),;
                    csty2 WITH LEFT(ALLTRIM(STYLE.cstymajor), 12)+' '+IIF(EMPTY(SCALE.cdim1) .AND. EMPTY(SCALE.cdim2), '', LEFT(ALLTRIM(SCALE.cdim1), 4))+' '+lczdesc,;
                    cpo2 WITH posln.po
                ENDCASE
                lnlbcount = lnlbcount+1
                IF lnlbcount=4
                  lnlbcount = 1
                ENDIF
              ELSE
                SELECT (lctmpbars)
                APPEND BLANK
                REPLACE ccode WITH lcstysz,;
                cstyle WITH lcstycd+lcsz,;
                cclrdesc WITH gfcoddes(SUBSTR(STYLE.STYLE, lnclrpos, lnclrlen), 'COLOR'),;
                cstydesc WITH LEFT(STYLE.desc1, 33),;
                csty WITH LEFT(ALLTRIM(STYLE.cstymajor), 12)+' '+IIF(EMPTY(SCALE.cdim1) .AND. EMPTY(SCALE.cdim2), '', LEFT(ALLTRIM(SCALE.cdim1), 4))+' '+lczdesc,;
                cpo WITH posln.po
              ENDIF
            ENDFOR
          ENDIF
        ENDFOR
      ENDSCAN

    ENDIF
  ENDSCAN
ELSE
  IF lcRpBasdOn = 'R' AND llRdate
    IF loDBFPOSLN.Sqlrun("Select POSLN.CSTYTYPE,POSLN.CBUSDOCU ,POSLN.style,POSLN.PO,"+;
      "posln.qty1,qty2,qty3,qty4,qty5,qty6,qty7,qty8,totqty,trancd,Posln.cinvtype,posln.[lineno]"+;
      " FROM POSLN(INDEX = POSLN) INNER JOIN POSHDR(INDEX = POSHDR) "+;
      " ON POSLN.CBUSDOCU = POSHDR.CBUSDOCU AND POSLN.CSTYTYPE =POSHDR.CSTYTYPE AND "+;
      " POSLN.PO = POSHDR.PO WHERE POSLN.CBUSDOCU = '"+IIF(lcPoTyp = "M",'P',IIF(lcPoTyp = "P",'P','N'))+;
      "' AND POSLN.CSTYTYPE = '"+IIF(lcPoTyp = "M",'U',IIF(lcPoTyp = "P",'P','N'))+"' AND Poshdr.Status <> 'X' AND "+;
      IIF(lcRpBasdOn = 'R'," TranCd = '2' AND POSHDR.Receive > 0 "," TranCd = '1'")+;
      IIF(llRdate," AND POSLN.DATE BETWEEN '"+DTOC(Ldate)+"' And '"+DTOC(Hdate)+"'",""))
      *! C201404,1 10/27/2011 MMT Print each PO Barcodes in separate page[Start]
      lcPONO = SPACE(6)
      *! C201404,1 10/27/2011 MMT Print each PO Barcodes in separate page[END]
      SELECT Posln
      lnlbcount = 1
      SCAN
        *! C201404,1 10/27/2011 MMT Print each PO Barcodes in separate page[Start]
        IF lcPONO <> posln.PO
          lnlbcount = 1
          lcPONO = posln.PO
        ENDiF
        *! C201404,1 10/27/2011 MMT Print each PO Barcodes in separate page[END]
        lcstyle = PADR(posln.STYLE, lnstylen)
        lcstycd = lfremsepr(lcstyle)
        = SEEK(lcstyle, 'STYLE')
        = SEEK('S'+STYLE.SCALE, 'SCALE')
        FOR i = 1 TO SCALE.CNT
          lcsz = STR(i, 1)
          IF posln.qty&lcsz > 0
            lczdesc = SCALE.sz&lcsz
            lcstysz = lcstyle+lcsz
            lnqty   = CEILING(posln.qty&lcsz * (lnrpperc/100))
            SELECT (lctmpbars)
            FOR lnqtycount = 1 TO lnqty
              IF lllibinst
                IF lnlbcount=1
                  APPEND BLANK
                ENDIF
                DO CASE
                  CASE lnlbcount=1
                    REPLACE ccode WITH lcstysz,;
                    cstyle WITH lcstycd+lcsz,;
                    cclrdesc WITH gfcoddes(SUBSTR(STYLE.STYLE, lnclrpos, lnclrlen), 'COLOR'),;
                    cstydesc WITH LEFT(STYLE.desc1, 33),;
                    csty WITH LEFT(ALLTRIM(STYLE.cstymajor), 12)+' '+IIF(EMPTY(SCALE.cdim1) .AND. EMPTY(SCALE.cdim2), '', LEFT(ALLTRIM(SCALE.cdim1), 4))+' '+lczdesc,;
                    cpo WITH posln.po
                  CASE lnlbcount=2
                    REPLACE ccode1 WITH lcstysz,;
                    cstyle1 WITH lcstycd+lcsz,;
                    cclrdesc1 WITH gfcoddes(SUBSTR(STYLE.STYLE, lnclrpos, lnclrlen), 'COLOR'),;
                    cstydesc1 WITH LEFT(STYLE.desc1, 33), ;
                    csty1 WITH LEFT(ALLTRIM(STYLE.cstymajor), 12)+' '+IIF(EMPTY(SCALE.cdim1) .AND. EMPTY(SCALE.cdim2), '', LEFT(ALLTRIM(SCALE.cdim1), 4))+' '+lczdesc,;
                    cpo1 WITH posln.po
                  CASE lnlbcount=3
                    REPLACE ccode2 WITH lcstysz,;
                    cstyle2 WITH lcstycd+lcsz,;
                    cclrdesc2 WITH gfcoddes(SUBSTR(STYLE.STYLE, lnclrpos, lnclrlen), 'COLOR'),;
                    cstydesc2 WITH LEFT(STYLE.desc1, 33),;
                    csty2 WITH LEFT(ALLTRIM(STYLE.cstymajor), 12)+' '+IIF(EMPTY(SCALE.cdim1) .AND. EMPTY(SCALE.cdim2), '', LEFT(ALLTRIM(SCALE.cdim1), 4))+' '+lczdesc,;
                    cpo2 WITH posln.po
                ENDCASE
                lnlbcount = lnlbcount+1
                IF lnlbcount=4
                  lnlbcount = 1
                ENDIF
              ELSE
                SELECT (lctmpbars)
                APPEND BLANK
                REPLACE ccode WITH lcstysz,;
                cstyle WITH lcstycd+lcsz,;
                cclrdesc WITH gfcoddes(SUBSTR(STYLE.STYLE, lnclrpos, lnclrlen), 'COLOR'),;
                cstydesc WITH LEFT(STYLE.desc1, 33),;
                csty WITH LEFT(ALLTRIM(STYLE.cstymajor), 12)+' '+IIF(EMPTY(SCALE.cdim1) .AND. EMPTY(SCALE.cdim2), '', LEFT(ALLTRIM(SCALE.cdim1), 4))+' '+lczdesc,;
                cpo WITH posln.po
              ENDIF
            ENDFOR
          ENDIF
        ENDFOR
      ENDSCAN
    ENDIF
  ELSE
    IF lcRpBasdOn = 'B' AND llTdate
      IF loDBFPOSLN.Sqlrun("Select POSLN.CSTYTYPE,POSLN.CBUSDOCU ,POSLN.style,POSLN.PO,"+;
        "posln.qty1,qty2,qty3,qty4,qty5,qty6,qty7,qty8,totqty,trancd,Posln.cinvtype,posln.[lineno]"+;
        " FROM POSLN(INDEX = POSLN) INNER JOIN POSHDR(INDEX = POSHDR) "+;
        " ON POSLN.CBUSDOCU = POSHDR.CBUSDOCU AND POSLN.CSTYTYPE =POSHDR.CSTYTYPE AND "+;
        " POSLN.PO = POSHDR.PO WHERE POSLN.CBUSDOCU = '"+IIF(lcPoTyp = "M",'P',IIF(lcPoTyp = "P",'P','N'))+;
        "' AND POSLN.CSTYTYPE = '"+IIF(lcPoTyp = "M",'U',IIF(lcPoTyp = "P",'P','N'))+"' AND Poshdr.Status <> 'X' AND "+;
        IIF(lcRpBasdOn = 'R'," TranCd = '2' AND POSHDR.Receive > 0 "," TranCd = '1'")+;
        IIF(llTdate," AND POSHDR.ENTERED BETWEEN '"+DTOC(Ldate)+"' AND '"+DTOC(Hdate)+"'",""))
        *! C201404,1 10/27/2011 MMT Print each PO Barcodes in separate page[Start]
        lcPONO = SPACE(6)
        *! C201404,1 10/27/2011 MMT Print each PO Barcodes in separate page[End]
        SELECT Posln
        lnlbcount = 1
        SCAN
          *! C201404,1 10/27/2011 MMT Print each PO Barcodes in separate page[Start]
          IF lcPONO <> posln.PO
            lnlbcount = 1
            lcPONO = posln.PO
          ENDiF
          *! C201404,1 10/27/2011 MMT Print each PO Barcodes in separate page[END]
          lcstyle = PADR(posln.STYLE, lnstylen)
          lcstycd = lfremsepr(lcstyle)
          = SEEK(lcstyle, 'STYLE')
          = SEEK('S'+STYLE.SCALE, 'SCALE')
          FOR i = 1 TO SCALE.CNT
            lcsz = STR(i, 1)
            IF posln.qty&lcsz > 0
              lczdesc = SCALE.sz&lcsz
              lcstysz = lcstyle+lcsz
              lnqty   = CEILING(posln.qty&lcsz * (lnrpperc/100))
              SELECT (lctmpbars)
              FOR lnqtycount = 1 TO lnqty
                IF lllibinst
                  IF lnlbcount=1
                    APPEND BLANK
                  ENDIF
                  DO CASE
                    CASE lnlbcount=1
                      REPLACE ccode WITH lcstysz,;
                      cstyle WITH lcstycd+lcsz,;
                      cclrdesc WITH gfcoddes(SUBSTR(STYLE.STYLE, lnclrpos, lnclrlen), 'COLOR'),;
                      cstydesc WITH LEFT(STYLE.desc1, 33),;
                      csty WITH LEFT(ALLTRIM(STYLE.cstymajor), 12)+' '+IIF(EMPTY(SCALE.cdim1) .AND. EMPTY(SCALE.cdim2), '', LEFT(ALLTRIM(SCALE.cdim1), 4))+' '+lczdesc,;
                      cpo WITH posln.po
                    CASE lnlbcount=2
                      REPLACE ccode1 WITH lcstysz,;
                      cstyle1 WITH lcstycd+lcsz,;
                      cclrdesc1 WITH gfcoddes(SUBSTR(STYLE.STYLE, lnclrpos, lnclrlen), 'COLOR'),;
                      cstydesc1 WITH LEFT(STYLE.desc1, 33),;
                      csty1 WITH LEFT(ALLTRIM(STYLE.cstymajor), 12)+' '+IIF(EMPTY(SCALE.cdim1) .AND. EMPTY(SCALE.cdim2), '', LEFT(ALLTRIM(SCALE.cdim1), 4))+' '+lczdesc,;
                      cpo1 WITH posln.po
                    CASE lnlbcount=3
                      REPLACE ccode2 WITH lcstysz,;
                      cstyle2 WITH lcstycd+lcsz,;
                      cclrdesc2 WITH gfcoddes(SUBSTR(STYLE.STYLE, lnclrpos, lnclrlen), 'COLOR'),;
                      cstydesc2 WITH LEFT(STYLE.desc1, 33),;
                      csty2 WITH LEFT(ALLTRIM(STYLE.cstymajor), 12)+' '+IIF(EMPTY(SCALE.cdim1) .AND. EMPTY(SCALE.cdim2), '', LEFT(ALLTRIM(SCALE.cdim1), 4))+' '+lczdesc,;
                      cpo2 WITH posln.po
                  ENDCASE
                  lnlbcount = lnlbcount+1
                  IF lnlbcount=4
                    lnlbcount = 1
                  ENDIF
                ELSE
                  SELECT (lctmpbars)
                  APPEND BLANK
                  REPLACE ccode WITH lcstysz,;
                  cstyle WITH lcstycd+lcsz,;
                  cclrdesc WITH gfcoddes(SUBSTR(STYLE.STYLE, lnclrpos, lnclrlen), 'COLOR'),;
                  cstydesc WITH LEFT(STYLE.desc1, 33),;
                  csty WITH LEFT(ALLTRIM(STYLE.cstymajor), 12)+' '+IIF(EMPTY(SCALE.cdim1) .AND. EMPTY(SCALE.cdim2), '', LEFT(ALLTRIM(SCALE.cdim1), 4))+' '+lczdesc,;
                  cpo WITH posln.po
                ENDIF
              ENDFOR
            ENDIF
          ENDFOR
        ENDSCAN
      ENDIF
    ELSE
      IF loDBFPOSLN.Sqlrun("Select POSLN.CSTYTYPE,POSLN.CBUSDOCU ,POSLN.style,POSLN.PO,"+;
        "posln.qty1,qty2,qty3,qty4,qty5,qty6,qty7,qty8,totqty,trancd,Posln.cinvtype,posln.[lineno]"+;
        "FROM POSLN(INDEX = POSLN) INNER JOIN POSHDR(INDEX = POSHDR) "+;
        " ON POSLN.CBUSDOCU = POSHDR.CBUSDOCU AND POSLN.CSTYTYPE =POSHDR.CSTYTYPE AND "+;
        " POSLN.PO = POSHDR.PO WHERE POSLN.CBUSDOCU = '"+IIF(lcPoTyp = "M",'P',IIF(lcPoTyp = "P",'P','N'))+;
        "' AND POSLN.CSTYTYPE = '"+IIF(lcPoTyp = "M",'U',IIF(lcPoTyp = "P",'P','N'))+"' AND Poshdr.Status <> 'X' AND "+;
        IIF(lcRpBasdOn = 'R'," TranCd = '2' AND POSHDR.Receive > 0"," TranCd = '1'"))
        *! C201404,1 10/27/2011 MMT Print each PO Barcodes in separate page[Start]
        lcPONO = SPACE(6)
        *! C201404,1 10/27/2011 MMT Print each PO Barcodes in separate page[END]
        SELECT Posln
        lnlbcount = 1
        SCAN
          *! C201404,1 10/27/2011 MMT Print each PO Barcodes in separate page[Start]
          IF lcPONO <> posln.PO
            lnlbcount = 1
            lcPONO = posln.PO
          ENDiF
          *! C201404,1 10/27/2011 MMT Print each PO Barcodes in separate page[END]
          lcstyle = PADR(posln.STYLE, lnstylen)
          lcstycd = lfremsepr(lcstyle)
          = SEEK(lcstyle, 'STYLE')
          = SEEK('S'+STYLE.SCALE, 'SCALE')
          FOR i = 1 TO SCALE.CNT
            lcsz = STR(i, 1)
            IF posln.qty&lcsz > 0
              lczdesc = SCALE.sz&lcsz
              lcstysz = lcstyle+lcsz
              lnqty   = CEILING(posln.qty&lcsz * (lnrpperc/100))
              SELECT (lctmpbars)
              FOR lnqtycount = 1 TO lnqty
                IF lllibinst
                  IF lnlbcount=1
                    APPEND BLANK
                  ENDIF
                  DO CASE
                    CASE lnlbcount=1
                      REPLACE ccode WITH lcstysz,;
                      cstyle WITH lcstycd+lcsz,;
                      cclrdesc WITH gfcoddes(SUBSTR(STYLE.STYLE, lnclrpos, lnclrlen), 'COLOR'),;
                      cstydesc WITH LEFT(STYLE.desc1, 33),;
                      csty WITH LEFT(ALLTRIM(STYLE.cstymajor), 12)+' '+IIF(EMPTY(SCALE.cdim1) .AND. EMPTY(SCALE.cdim2), '', LEFT(ALLTRIM(SCALE.cdim1), 4))+' '+lczdesc,;
                      cpo WITH posln.po
                    CASE lnlbcount=2
                      REPLACE ccode1 WITH lcstysz,;
                      cstyle1 WITH lcstycd+lcsz,;
                      cclrdesc1 WITH gfcoddes(SUBSTR(STYLE.STYLE, lnclrpos, lnclrlen), 'COLOR'),;
                      cstydesc1 WITH LEFT(STYLE.desc1, 33),;
                      csty1 WITH LEFT(ALLTRIM(STYLE.cstymajor), 12)+' '+IIF(EMPTY(SCALE.cdim1) .AND. EMPTY(SCALE.cdim2), '', LEFT(ALLTRIM(SCALE.cdim1), 4))+' '+lczdesc,;
                      cpo1 WITH posln.po
                    CASE lnlbcount=3
                      REPLACE ccode2 WITH lcstysz,;
                      cstyle2 WITH lcstycd+lcsz,;
                      cclrdesc2 WITH gfcoddes(SUBSTR(STYLE.STYLE, lnclrpos, lnclrlen), 'COLOR'),;
                      cstydesc2 WITH LEFT(STYLE.desc1, 33),;
                      csty2 WITH LEFT(ALLTRIM(STYLE.cstymajor), 12)+' '+IIF(EMPTY(SCALE.cdim1) .AND. EMPTY(SCALE.cdim2), '', LEFT(ALLTRIM(SCALE.cdim1), 4))+' '+lczdesc,;
                      cpo2 WITH posln.po
                  ENDCASE
                  lnlbcount = lnlbcount+1
                  IF lnlbcount=4
                    lnlbcount = 1
                  ENDIF
                ELSE
                  SELECT (lctmpbars)
                  APPEND BLANK
                  REPLACE ccode WITH lcstysz,;
                  cstyle WITH lcstycd+lcsz,;
                  cclrdesc WITH gfcoddes(SUBSTR(STYLE.STYLE, lnclrpos, lnclrlen), 'COLOR'),;
                  cstydesc WITH LEFT(STYLE.desc1, 33),;
                  csty WITH LEFT(ALLTRIM(STYLE.cstymajor), 12)+' '+IIF(EMPTY(SCALE.cdim1) .AND. EMPTY(SCALE.cdim2), '', LEFT(ALLTRIM(SCALE.cdim1), 4))+' '+lczdesc,;
                  cpo WITH posln.po
                ENDIF
              ENDFOR
            ENDIF
          ENDFOR
        ENDSCAN
      ENDIF
    ENDIF
  ENDIF
ENDIF
WAIT CLEAR
SELECT (lctmpbars)
GOTO TOP
*!*************************************************************
*! Name      : LFCLRDESC
*! Developer : Mariam Mazhar (MMT)
*! Date      : 10/09/2011
*! Purpose   : Get Color and Major positions in style structure
*!*************************************************************
FUNCTION LFCLRDESC
DIMENSION laitemseg[1]
= gfitemmask(@laitemseg)
FOR lncount = 1 TO ALEN(laitemseg, 1)
  IF laitemseg(lncount, 1)='F'
    lnstylngl = LEN(laitemseg(lncount, 3))
    lnstyposgl = laitemseg(lncount, 4)
    EXIT
  ENDIF
ENDFOR
DIMENSION laitemseg[1]
= gfitemmask(@laitemseg)
FOR lncount = 1 TO ALEN(laitemseg, 1)
  IF laitemseg(lncount, 1)='C'
    lnclrlen = LEN(laitemseg(lncount, 3))
    lnclrpos = laitemseg(lncount, 4)
    EXIT
  ENDIF
ENDFOR

