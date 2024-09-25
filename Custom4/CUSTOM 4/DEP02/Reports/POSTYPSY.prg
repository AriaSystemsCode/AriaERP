SET STEP ON 
SELECT POSLN
LOCATE
SCAN
  lcColorCode =PADR(substr(Posln.style,lnNonMajSt,lnColorLen),6)
  lcCLRLNAME  =''
*!*	  DECLARE LADIVDLT[1,2]
*!*	  LADIVDLT[1,1] = 'CLRLNAME'
*!*	  LADIVDLT[1,2] = 'lcCLRLNAME'
*!*	  = gfCodDes(lcColorCode , 'COLOR     ')
*!*	  =GFRLTFLD(PADR(lcColorCode ,6),@LADIVDLT, 'COLOR     ')
  lcAlias = ALIAS()
  SELECT Codes
  SET ORDER TO TAG Codes
  IF SEEK('N'+lcColorCode +'Y'+'COLOR')
    SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = 'N'+lcColorCode +'Y'+'COLOR'
      IF crltd_nam = 'CLRLNAME'
        lcCLRLNAME= ALLTRIM(crltd_vlu)
        EXIT
      ENDIF
    ENDSCAN
  ENDIF
  SELECT (lcAlias)
  REPLACE CITEMFLD3 WITH lcCLRLNAME IN posln 
ENDSCAN
SELECT POSLN
LOCATE 


