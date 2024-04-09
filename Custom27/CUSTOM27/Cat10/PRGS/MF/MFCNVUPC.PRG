*:***************************************************************************
*: Program file  : MFCNVUPC
*: Program desc. : Programe to convert styleupc file from 2.0 to 2.7
*:        System : Aria Advantage Series.
*:        Module : Manufactering Module (MF)
*:        Date   : 06/30/2002
*:     Developer : AHMED MAHER (AMH)
*:***************************************************************************
*: Calls : 
*:     Procedures : 
*:     Functions  : 
*:***************************************************************************
*: Example : DO MFCNVUPC
*:***************************************************************************
*: Modifications :
*:***************************************************************************
*
CLOSE DATA
lcSorDir = GETDIR('','Select the old data directory')
IF EMPTY(lcSorDir)
  RETURN
ELSE
  IF !(FILE(lcSorDir+'STYLEUPC.DBF') .AND. FILE(lcSorDir+'CODE.DBF'))
    WAIT WINDOW 'Incorrect Directory'
    RETURN
  ELSE
    lcTarDir = GETDIR('','Select the New data directory')
    IF EMPTY(lcTarDir)
      RETURN
    ELSE
      IF !(FILE(lcTarDir+'STYLEUPC.DBF') .AND. FILE(lcTarDir+'CODES.DBF');
           .AND. FILE(lcTarDir+'STYLE.DBF'))
        WAIT WINDOW 'Incorrect Directory'
        RETURN
      ELSE
        USE (lcTarDir+'STYLEUPC') IN 0 ORDER TAG STYLEUPC SHARE ALIAS TARSTYUPC
        USE (lcTarDir+'CODES') IN 0 ORDER TAG CCODE_NO SHARE ALIAS TARCODE
        USE (lcTarDir+'STYLE') IN 0 ORDER TAG STYLE SHARE ALIAS TARSTYLE
      ENDIF
    ENDIF
    USE (lcSorDir+'STYLEUPC') IN 0 ORDER TAG STYLEUPC SHARE ALIAS SORSTYUPC
    USE (lcSorDir+'CODE') IN 0 ORDER TAG CODE SHARE ALIAS SORCODE
    IF FILE(lcTarDir+'LOGUPC.DBF')
      USE (lcTarDir+'LOGUPC') IN 0 EXCLUSIVE ALIAS LOGUPC
      SELECT LOGUPC
      ZAP
    ELSE
      CREATE TABLE (lcTarDir+'LOGUPC') (SEASON C(3),STYLE C(7),COLOR C(3),SIZE C(2),CUPCNUM1 C(6),;
                                        CUPCNUM2 C(5),CUPCNUM3 C(1),CREASON C(40))
    ENDIF
  ENDIF
ENDIF

SELECT SORSTYUPC
SCAN
  WAIT WINDOW 'Converting styleupc file for '+CHR(13)+;
              'Season : ' + SEASON + CHR(13)+;
              'Style : ' + STYLE + CHR(13)+;
              'Color : ' + COLOR + CHR(13)+;
              'Size : ' + SIZE NOWAIT
  IF SEEK(SEASON+'-'+PADR(STYLE,8)+'-'+COLOR+'000','TARSTYLE')
    SELECT TARSTYUPC
    IF !SEEK(TARSTYLE.STYLE+SORSTYUPC.SIZE)
      APPEND BLANK
      REPLACE STYLE WITH TARSTYLE.STYLE,;
              SIZE  WITH SORSTYUPC.SIZE
    ENDIF
    REPLACE CUPCNUM1 WITH SORSTYUPC.CUPCNUM1,;
            CUPCNUM2 WITH SORSTYUPC.CUPCNUM2,;
            CUPCNUM3 WITH PADR(SORSTYUPC.CUPCNUM3,2)
  ELSE
    *khm
    *IF SEEK('NCOLOR     '+COLOR+'000','TARCODE')
     * SCATTER MEMVAR
     * IF SEEK(SEASON,'TARSTYLE')
     *   m.cReason = 'Style not found in the style file'
     * ELSE
     *   m.cReason = 'Season not found in the style file'
     * ENDIF
     * INSERT INTO LOGUPC FROM MEMVAR
    *ELSE
      IF SEEK('C'+COLOR,'SORCODE') .AND. SEEK('NCOLOR     ','TARCODE')
        =lfLocate()
      ELSE
        SCATTER MEMVAR
        m.cReason = 'Color not found in the old code file'
        INSERT INTO LOGUPC FROM MEMVAR
      ENDIF
    *khm
    *ENDIF
    *khm
  ENDIF
ENDSCAN
WAIT CLEAR
CLOSE ALL

*****************************
FUNCTION lfLocate
PARAMETERS llReCall

PRIVATE llRet
SELECT TARCODE
LOCATE REST WHILE cDefCode+cFld_Name+cCode_No+cDisCrep+cRltd_Nam = 'NCOLOR     ';
            FOR CDISCREP = SORCODE.CDATA
IF FOUND()
  IF SEEK(SORSTYUPC.SEASON+'-'+PADR(SORSTYUPC.STYLE,8)+'-'+CCODE_NO,'TARSTYLE')
    SELECT TARSTYUPC
    IF !SEEK(TARSTYLE.STYLE+SORSTYUPC.SIZE)
      APPEND BLANK
      REPLACE STYLE WITH TARSTYLE.STYLE,;
              SIZE  WITH SORSTYUPC.SIZE
    ENDIF
    REPLACE CUPCNUM1 WITH SORSTYUPC.CUPCNUM1,;
            CUPCNUM2 WITH SORSTYUPC.CUPCNUM2,;
            CUPCNUM3 WITH PADR(SORSTYUPC.CUPCNUM3,2)
    llRet = .T.
  ELSE

    SKIP

    IF !lfLocate(.T.)
      SELECT SORSTYUPC
      SCATTER MEMVAR
      IF SEEK(SEASON,'TARSTYLE')
        m.cReason = 'Style not found in the style file'
      ELSE
        m.cReason = 'Season not found in the style file'
      ENDIF
      INSERT INTO LOGUPC FROM MEMVAR
    ENDIF
    llRet = .T.
  ENDIF
ELSE
  IF !llReCall
    SELECT SORSTYUPC
    SCATTER MEMVAR
    m.cReason = 'Color not found in the codes file'
    INSERT INTO LOGUPC FROM MEMVAR
  ENDIF
  llRet = .F.
ENDIF

RETURN llRet