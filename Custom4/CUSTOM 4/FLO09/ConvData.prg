*This a conversion program to convert custom codes to profiles
*Mariam
lcDataPath = GETDIR("","Select Dbfs directory")
IF !DIRECTORY(lcDataPath)
  MESSAGEBOX('Selected Directory is Invalid')
  RETURN .F.
ENDIF 

USE  lcDataPath +'Codes.dbf' IN 0 SHARED 
USE  lcDataPath +'sequence.dbf' IN 0 SHARED 

lcSBNameCode = ''
lcSBColrCode = ''
lcSBTrmCode  = ''

SELECT Sequence
SET ORDER TO CSEQ_TYPE   && CSEQ_TYPE+CSEQ_GROUP

SELECT Codes
SET ORDER to CCODE_NO   && CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM

*Add 3 new profiles  (Color,Acc,Name)
IF !SEEK('N'+'CPRO_CODE')
  *Name 
  m.CDEFCODE = 'N'
  m.CFLD_NAME = 'CPRO_CODE'
  
  SELECT Sequence
  =SEEK('CCODE_NO')
  lnNextCode = sequence.nseq_no 
  REPLACE sequence.nseq_no  WITH sequence.nseq_no + 1 
  m.CCODE_NO = PADL(ALLTRIM(STR(lnNextCode)),6,'0')
  
  lcSBNameCode = m.CCODE_NO
  
  
  m.cdiscrep = 'Screen Code'
  m.lrltfields = .F.
  m.crltfield = 'N' 
  m.crltd_nam = ''
  m.crltd_typ = ''
  m.crltd_vlu = ''
  SELECT Codes
  INSERT INTO Codes FROM MEMVAR 
  
  m.cdiscrep = ''
  m.crltfield = 'Y' 
  m.crltd_nam = 'CPRO_TYPE'
  m.crltd_typ = 'C'
  m.crltd_vlu = 'ST'
  INSERT INTO Codes FROM MEMVAR 
  
  m.crltd_nam = 'CPRO_UPDMD'
  m.crltd_vlu = 'M'
  INSERT INTO Codes FROM MEMVAR   	
  
  m.crltd_nam = 'CPRO_SCR'
  m.crltd_vlu = 'SO'
  INSERT INTO Codes FROM MEMVAR   	
  
  
  *Acc
  m.CDEFCODE = 'N'
  m.CFLD_NAME = 'CPRO_CODE'
  
  SELECT Sequence
  =SEEK('CCODE_NO')
  lnNextCode = sequence.nseq_no 
  REPLACE sequence.nseq_no  WITH sequence.nseq_no + 1 
  
  
  m.CCODE_NO = PADL(ALLTRIM(STR(lnNextCode)),6,'0')
  lcSBTrmCode  = m.CCODE_NO 
  m.cdiscrep = 'Accessory Color'
  m.lrltfields = .F.
  m.crltfield = 'N' 
  m.crltd_nam = ''
  m.crltd_typ = ''
  m.crltd_vlu = ''
  SELECT Codes
  INSERT INTO Codes FROM MEMVAR 
  
  m.cdiscrep = ''
  m.crltfield = 'Y' 
  m.crltd_nam = 'CPRO_TYPE'
  m.crltd_typ = 'C'
  m.crltd_vlu = 'ST'
  INSERT INTO Codes FROM MEMVAR 
  
  m.crltd_nam = 'CPRO_UPDMD'
  m.crltd_vlu = 'M'
  INSERT INTO Codes FROM MEMVAR   	
  
  m.crltd_nam = 'CPRO_SCR'
  m.crltd_vlu = 'SO'
  INSERT INTO Codes FROM MEMVAR   	
  
  *Color
  m.CDEFCODE = 'N'
  m.CFLD_NAME = 'CPRO_CODE'
  
  SELECT Sequence
  =SEEK('CCODE_NO')
  lnNextCode = sequence.nseq_no 
  REPLACE sequence.nseq_no  WITH sequence.nseq_no + 1 

  
  m.CCODE_NO = PADL(ALLTRIM(STR(lnNextCode)),6,'0')
  m.cdiscrep = 'Screen Color'
  lcSBColrCode =  m.CCODE_NO 
  
  m.lrltfields = .F.
  m.crltfield = 'N' 
  m.crltd_nam = ''
  m.crltd_typ = ''
  m.crltd_vlu = ''
  SELECT Codes
  INSERT INTO Codes FROM MEMVAR 
  
  m.cdiscrep = ''
  m.crltfield = 'Y' 
  m.crltd_nam = 'CPRO_TYPE'
  m.crltd_typ = 'C'
  m.crltd_vlu = 'ST'
  INSERT INTO Codes FROM MEMVAR 
  
  m.crltd_nam = 'CPRO_UPDMD'
  m.crltd_vlu = 'M'
  INSERT INTO Codes FROM MEMVAR   	
  
  m.crltd_nam = 'CPRO_SCR'
  m.crltd_vlu = 'SO'
  INSERT INTO Codes FROM MEMVAR   	
ELSE
  LOCATE REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM = 'N'+'CPRO_CODE' FOR ;
  cdiscrep = 'Screen Code'
  
  IF !FOUND()
    *Name 
    m.CDEFCODE = 'N'
    m.CFLD_NAME = 'CPRO_CODE'
    
    SELECT Sequence
    =SEEK('CCODE_NO')
    lnNextCode = sequence.nseq_no 
    REPLACE sequence.nseq_no  WITH sequence.nseq_no + 1 
    m.CCODE_NO = PADL(ALLTRIM(STR(lnNextCode)),6,'0')
    m.cdiscrep = 'Screen Code'
    lcSBNameCode = m.CCODE_NO
    
    m.lrltfields = .F.
    m.crltfield = 'N' 
    m.crltd_nam = ''
    m.crltd_typ = ''
    m.crltd_vlu = ''
    SELECT Codes
    INSERT INTO Codes FROM MEMVAR 
    
    m.cdiscrep = ''
    m.crltfield = 'Y' 
    m.crltd_nam = 'CPRO_TYPE'
    m.crltd_typ = 'C'
    m.crltd_vlu = 'ST'
    INSERT INTO Codes FROM MEMVAR 
    
    m.crltd_nam = 'CPRO_UPDMD'
    m.crltd_vlu = 'M'
    INSERT INTO Codes FROM MEMVAR   	
    
    m.crltd_nam = 'CPRO_SCR'
    m.crltd_vlu = 'SO'
    INSERT INTO Codes FROM MEMVAR   	
  ENDIF 
  SELECT codes
  =SEEK('N'+'CPRO_CODE')
  LOCATE REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM = 'N'+'CPRO_CODE' FOR ;
  cdiscrep = 'Accessory Color'
  IF !FOUND()
    m.CDEFCODE = 'N'
    m.CFLD_NAME = 'CPRO_CODE'
    
    SELECT Sequence
    =SEEK('CCODE_NO')
    lnNextCode = sequence.nseq_no 
    REPLACE sequence.nseq_no  WITH sequence.nseq_no + 1 

    m.CCODE_NO = PADL(ALLTRIM(STR(lnNextCode)),6,'0')
    m.cdiscrep = 'Accessory Color'
    lcSBTrmCode  = m.CCODE_NO
    
    m.lrltfields = .F.
    m.crltfield = 'N' 
    m.crltd_nam = ''
    m.crltd_typ = ''
    m.crltd_vlu = ''
    SELECT Codes
    INSERT INTO Codes FROM MEMVAR 
    
    m.cdiscrep = ''
    m.crltfield = 'Y' 
    m.crltd_nam = 'CPRO_TYPE'
    m.crltd_typ = 'C'
    m.crltd_vlu = 'ST'
    INSERT INTO Codes FROM MEMVAR 
    
    m.crltd_nam = 'CPRO_UPDMD'
    m.crltd_vlu = 'M'
    INSERT INTO Codes FROM MEMVAR     
    
    m.crltd_nam = 'CPRO_SCR'
    m.crltd_vlu = 'SO'
    INSERT INTO Codes FROM MEMVAR     
  ENDIF 
  
  SELECT codes
  =SEEK('N'+'CPRO_CODE')
  LOCATE REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM = 'N'+'CPRO_CODE' FOR ;
  cdiscrep = 'Screen Color'
  IF !FOUND()
    m.CDEFCODE = 'N'
    m.CFLD_NAME = 'CPRO_CODE'
    
    SELECT Sequence
    =SEEK('CCODE_NO')
    lnNextCode = sequence.nseq_no 
    REPLACE sequence.nseq_no  WITH sequence.nseq_no + 1 

    m.CCODE_NO = PADL(ALLTRIM(STR(lnNextCode)),6,'0')
    lcSBColrCode = m.CCODE_NO
    
    m.cdiscrep = 'Screen Color'
    m.lrltfields = .F.
    m.crltfield = 'N' 
    m.crltd_nam = ''
    m.crltd_typ = ''
    m.crltd_vlu = ''
    SELECT Codes
    INSERT INTO Codes FROM MEMVAR 
    
    m.cdiscrep = ''
    m.crltfield = 'Y' 
    m.crltd_nam = 'CPRO_TYPE'
    m.crltd_typ = 'C'
    m.crltd_vlu = 'ST'
    INSERT INTO Codes FROM MEMVAR 
    
    m.crltd_nam = 'CPRO_UPDMD'
    m.crltd_vlu = 'M'
    INSERT INTO Codes FROM MEMVAR     
    
    m.crltd_nam = 'CPRO_SCR'
    m.crltd_vlu = 'SO'
    INSERT INTO Codes FROM MEMVAR     
  ENDIF 
ENDIF 

SELECT Codes 
CLOSE ALL 

USE  lcDataPath +'Codes.dbf' IN 0 SHARED 
USE  lcDataPath +'PROFLIST.dbf' IN 0 SHARED 

SELECT PROFLIST
SET ORDER TO PROFILE   && CPRO_CODE+CPRO_VALUE

SELECT Codes 

SET ORDER TO CCODE_NO   && CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM
IF SEEK('N'+'CSBLNAME')
  SELECT Codes 
  SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM ='N'+'CSBLNAME'
    m.cpro_code  = lcSBNameCode
	m.cpro_value = codes.cdiscrep 
    IF !SEEK(m.CPRO_CODE+m.CPRO_VALUE,'PROFLIST')
      INSERT INTO 'PROFLIST' FROM MEMVAR 
    ENDIF 
  ENDSCAN 
ENDIF 

IF SEEK('N'+'CSBLCOLOR')
  SELECT Codes 
  SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM ='N'+'CSBLCOLOR'
    m.cpro_code  = lcSBColrCode 
	m.cpro_value = codes.cdiscrep 
    IF !SEEK(m.CPRO_CODE+m.CPRO_VALUE,'PROFLIST')
      INSERT INTO 'PROFLIST' FROM MEMVAR 
    ENDIF 
  ENDSCAN 
ENDIF 


IF SEEK('N'+'CTRMCOLOR')
  SELECT Codes 
  SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM ='N'+'CTRMCOLOR'
    m.cpro_code  = lcSBTrmCode  
	m.cpro_value = codes.cdiscrep 
    IF !SEEK(m.CPRO_CODE+m.CPRO_VALUE,'PROFLIST')
      INSERT INTO 'PROFLIST' FROM MEMVAR 
    ENDIF 
  ENDSCAN 
ENDIF 

CLOSE ALL

USE  lcDataPath +'ARTWRKDS.dbf' IN 0 SHARED 
SELECT ARTWRKDS
LOCATE 
IF EOF()
  APPEND BLANK 
  REPLACE Type 		WITH  'D',;
		  cdesignid WITH  'DEFDESIGN',;
		  cprntdsgn WITH  'DEFDESIGN',;
		  cdsgnname WITH  'Default Design'   
ENDIF  
USE IN ARTWRKDS

USE  lcDataPath +'orddsgn.dbf' IN 0 SHARED 
USE  lcDataPath +'SOCODES.DBF' IN 0 SHARED 
USE  lcDataPath +'PROFVALU.DBF' IN 0 SHARED 
USE  lcDataPath +'Codes.DBF' IN 0 SHARED 
USE  lcDataPath +'ORDLINE.DBF' IN 0 SHARED 

SELECT ordline
SET ORDER TO ORDLINE   && CORDTYPE+ORDER+STR(LINENO,6)

SELECT codes 
SET ORDER TO CODES   && CDEFCODE+CCODE_NO+CRLTFIELD+CFLD_NAME

SELECT SOCODES 

SET ORDER TO SOCODES   && CORDTYPE+ORDER+STR(LINENO,6)
lcCurOrder = ''
lnLineNo = 0



SCAN FOR !EMPTY(Order)
  IF lcCurOrder  <> SOCODES.Order
    lnLineNo = 0
  ENDIF 
  m.Order = SOCODES.Order
  m.cordline  = STR(SOCODES.lineno,6)
  lnLineNo = lnLineNo + 1
  m.lineno  = lnLineNo 
  m.cdesignid = 'DEFDESIGN'
  INSERT INTO 'orddsgn' FROM MEMVAR 
  m.cpro_type  = 'SO'
  m.Ckey = SOCODES.cordtype + SOCODES.Order + m.cordline +STR(m.lineno,6)
  
  m.cpro_code = lcSBNameCode
  
  lcStyle = IIF(SEEK(SOCODES.cordtype +SOCODES.Order + m.cordline,'ORdLine'),ORdline.Style,'')
  
  
  IF SEEK('N'+SOCODES.CSBLNAME+'N'+'CSBLNAME','codes')
    m.cpro_value = codes.cdiscrep
  ENDIF 
  IF !SEEK(m.cpro_type+m.Ckey +m.cpro_code,'PROFVALU','PROFILE')
    INSERT INTO 'PROFVALU' FROM MEMVAR 
  ENDIF   
  IF !SEEK('ST'+PADR(lcStyle,130)+m.cpro_code,'PROFVALU','PROFILE')
    m.Ckey = lcStyle 
    m.cpro_type = 'ST'
    INSERT INTO 'PROFVALU' FROM MEMVAR 
  ENDIF 
   m.Ckey =  SOCODES.cordtype + SOCODES.Order + m.cordline +STR(m.lineno,6)
   m.cpro_type = 'SO'
  
  
  
  
  m.cpro_code = lcSBColrCode 
  IF SEEK('N'+SOCODES.CTRMCOLOR+'N'+'CTRMCOLOR','codes')
    m.cpro_value = codes.cdiscrep
  ENDIF 
  IF !SEEK(m.cpro_type+m.Ckey +m.cpro_code,'PROFVALU','PROFILE')
    INSERT INTO 'PROFVALU' FROM MEMVAR 
  ENDIF   
  
  IF !SEEK('ST'+PADR(lcStyle,130)+m.cpro_code,'PROFVALU','PROFILE')
    m.Ckey = lcStyle 
    m.cpro_type = 'ST'
    INSERT INTO 'PROFVALU' FROM MEMVAR 
  ENDIF 
  m.Ckey =  SOCODES.cordtype + SOCODES.Order + m.cordline +STR(m.lineno,6)
  m.cpro_type = 'SO'
  
  m.cpro_code = lcSBTrmCode  
  IF SEEK('N'+SOCODES.CSBLCOLOR+'N'+'CSBLCOLOR','codes')
    m.cpro_value = codes.cdiscrep
  ENDIF 
  IF !SEEK(m.cpro_type+m.Ckey +m.cpro_code,'PROFVALU','PROFILE')
    INSERT INTO 'PROFVALU' FROM MEMVAR 
  ENDIF 
  
  IF !SEEK('ST'+PADR(lcStyle,130)+m.cpro_code,'PROFVALU','PROFILE')
    m.Ckey = lcStyle 
    m.cpro_type = 'ST'
    INSERT INTO 'PROFVALU' FROM MEMVAR 
  ENDIF 

    
  lcCurOrder = m.Order 
  SELECT SOCODES 
ENDSCAN  
CLOSE ALL
