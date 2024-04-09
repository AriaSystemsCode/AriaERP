*!***********************************************************************************************
*! Program   : SOORCNJH (Option program for Josie under SOORCN parent order confirmation program)
*! Developer : Hesham Elmasry(HES)
*! Date      : 07/11/2012
*! Purpose   : Add special information needed by Josie Heart
*!***********************************************************************************************
SELECT (lcTempOrd)

&& Add the new memo field holds all existing order style profiles.
ALTER TABLE (lcTempOrd) ADD COLUMN PROFVAL M(10)
&& Function to add the related order style profiles.
=lfAddProfVal(lcTempOrd)
*-- End of program
SET ORDER TO (lcTempOrd) IN (lcTempOrd)
SELECT ORDHDR
SET RELATION TO cordtype+ order INTO (lcTempOrd) ADDITIVE
SELECT (lcTempOrd)
SET RELATION TO 'S'+SUBSTR(Style,1,lnMajorLen) INTO OBJLINK_A ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SET RELATION TO style INTO Style ADDITIVE
SELECT (lcTempOrd)
SET RELATION TO 'N' INTO (lcNoteLns) ADDITIVE
SELECT ORDHDR
SET SKIP TO &lcSkipExpr
SELECT ORDHDR
*!****************************************************************
*! Name      : lfAddProfVal
*! Developer : Hesham Elmasry(HES)
*! Date      : 07/11/2012
*! Purpose   : Add the related order style profiles for each line.
*!****************************************************************
*! Passed Parameters  : The temp file you need to add profile info
*!****************************************************************
FUNCTION lfAddProfVal
  PARAMETERS lctemp

  IF !USED('profvalu')
    gfOpenFile(oAriaApplication.DataDir+'profvalu',oAriaApplication.DataDir+'profile','SH')
  ENDIF

  IF !USED('proflist')
    gfOpenFile(oAriaApplication.DataDir+'proflist',oAriaApplication.DataDir+'profile','SH')
  ENDIF

  IF !USED('codes')
    gfOpenFile(oAriaApplication.DataDir+'codes',oAriaApplication.DataDir+'ccode_no','SH')
  ENDIF

  SELECT (lctemp)
  SCAN
    lcOrdTyp = cOrdType
    lcOrder = ORDER
    lcLinNo = STR(LINENO,6)
    lcCodDesc = ''
    lcValue = ''
    SELECT profvalu
    IF SEEK('SO'+lcOrdTyp+lcOrder+lcLinNo)
      SCAN REST WHILE cpro_type+ckey+cpro_code = 'SO'+lcOrdTyp+lcOrder+lcLinNo
        lcValue = cpro_value  && Order style profile value
        lcCode = cpro_code && The Order style profile value code to get its related description
        SELECT codes
        lcOrd = ORDER()
        SET ORDER TO cCode_No
        IF SEEK('N'+'CPRO_CODE '+lcCode)
          SCAN REST WHILE cdefcode+cfld_name+cCode_No = 'N'+'CPRO_CODE '+lcCode FOR crltfield = 'N'
            lcCodDesc = ALLTRIM(cDiscrep)
          ENDSCAN
        ELSE
          MESSAGEBOX('No code reference for '+lcCode+ 'Ccode, please refer to Aria',16+512,_SCREEN.CAPTION)
        ENDIF
        SET ORDER TO &lcOrd
        SELECT (lctemp)
        REPLACE PROFVAL WITH PROFVAL + ALLTRIM(lcCodDesc)+': '+ALLTRIM(lcValue)+ '          '
      ENDSCAN
    ENDIF
  ENDSCAN
  * End of lfAddProfVal
