LPARAMETERS lcA27Sys
USE ADDBS(lcA27Sys)+'SYCCOMP.DBF' SHAR ALIAS 'SYCCOMP_A' IN 0 
SELECT SYCCOMP_A
lcDataDir = ''
SCAN FOR lRunfroma4
  lcDataDir = ADDBS(ALLTRIM(syccomp_a.ccom_ddir))
  USE (lcDataDir +'CODES') share IN 0 
  USE (lcDataDir +'CODES') share IN 0 alia 'Codes2' AGAIN 
  SELECT Codes2
  SET ORDER TO CCODE_NO   && CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM
  *SHIPVIA   
  SELECT CODES
  SET ORDER TO CCODE_NO   && CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM
  =SEEK('N'+'SHIPVIA   ')
  SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+'SHIPVIA   ' FOR crltfield = 'N'
    lcCodeNum =CCODE_NO
    SELECT Codes2
    =SEEK('N'+'SHIPVIA   '+lcCodeNum)
    LOCATE REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM = 'N'+'SHIPVIA   '+lcCodeNum ;
    	   FOR ALLTRIM(crltd_nam)='CCARRIERID'
    IF !FOUND()
      SELECT Codes2
      =SEEK('N'+'SHIPVIA   '+lcCodeNum)
      lcCarr = 'OTHER'
      LOCATE REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM = 'N'+'SHIPVIA   '+lcCodeNum ;
    	     FOR ALLTRIM(crltd_nam)='CUPS'
      IF FOUND()
        IF SUBSTR(ALLTRIM(Codes2.crltd_vlu),1,5) ='USUPS'
          lcCarr = 'UPS'
        ELSE
         IF SUBSTR(ALLTRIM(Codes2.crltd_vlu),1,4) ='USPS'
           lcCarr = 'USPS'
         ENDIF
        ENDIF
      ENDIF	     
	  SELECT CODES2    
	  APPEND BLANK 
	  REPLACE CDEFCODE WITH 'N',;
		      CFLD_NAME WITH 'SHIPVIA   ',;
		      CCODE_NO WITH lcCodeNum ,;
		      crltfield WITH 'Y' ,;
		      crltd_nam WITH 'CCARRIERID',;
		      crltd_vlu WITH lcCarr ,;
		      crltd_typ WITH 'C',;
		      lrltfields WITH .F.
      SELECT Codes2
      =SEEK('N'+'SHIPVIA   '+lcCodeNum)
       LOCATE REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM = 'N'+'SHIPVIA   '+lcCodeNum ;
        	   FOR ALLTRIM(Codes2.crltd_nam)='LINCFRGHT'
       IF !FOUND()
         SELECT CODES2    
		 APPEND BLANK 
		 REPLACE CDEFCODE WITH 'N',;
			      CFLD_NAME WITH 'SHIPVIA   ',;
			      CCODE_NO WITH lcCodeNum ,;
			      crltfield WITH 'Y' ,;
			      crltd_nam WITH 'LINCFRGHT',;
			      crltd_vlu WITH IIF(lcCarr='UPS','T','F') ,;
			      crltd_typ WITH 'L',;
			      lrltfields WITH .F.
       ENDIF
    ENDIF
    
  ENDSCAN
  *syccomp.ccom_ddir
  USE IN Codes
  USE IN Codes2
ENDSCAN 