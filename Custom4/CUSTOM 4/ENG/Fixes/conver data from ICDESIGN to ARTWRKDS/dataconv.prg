*:*************************************************************************
*: Purpose   : Program to convert data from ICDESIGN table to ARTWRKS table
*: Date      : 06/03/2009
*: Developer : AHMED MOUSTAFA (AHS)
*:*************************************************************************
SET EXCLUSIVE OFF 
_screen.Caption = 'Data Conversion from ICDESIGN to ARTWRKDS'

   *-- Selecting the company DBFs folder
lcDataFolder = GETDIR('','Select Company folder')
IF EMPTY(lcDataFolder )
  MESSAGEBOX('Wrong path!')
  RETURN 
ENDIF 

   *-- To check that all files are found
IF !FILE(lcDataFolder+'ICDESIGN.dbf') OR !FILE(lcDataFolder+'ARTWRKDS.dbf') OR !FILE(lcDataFolder+'CODES.dbf')
  MESSAGEBOX('Table is missing!')
  RETURN 
ENDIF 

   *-- Opening used files
USE (lcDataFolder+'ARTWRKDS.dbf') IN 0 ORDER TYPE ALIAS ARTWRKDS
USE (lcDataFolder+'ICDESIGN.dbf') IN 0 ORDER ICDESIGN ALIAS ICDESIGN
USE (lcDataFolder+'CODES.dbf') IN 0 ORDER CCODE_NO ALIAS CODES

   *-- Getting the value of the fields from Codes file
=SEEK('D'+'CDSGNCTGRY','CODES')  
M.CDSGNCTGRY  =  CODES.CCODE_NO
=SEEK('D'+'CDSGNSIZE ','CODES')  
M.CDSGNSIZE   =  CODES.CCODE_NO
=SEEK('D'+'CDSGNTYPE ','CODES')  
M.CDSGNTYPE   =  CODES.CCODE_NO
=SEEK('D'+'CSTYLEPOS ','CODES')  
M.CSTYLEPOS   =  CODES.CCODE_NO
=SEEK('D'+'ROYALTY   ','CODES')  
lcDefROYALTY  =  CODES.CCODE_NO
=SEEK('D'+'CDESIGNER ','CODES')  
M.CDESIGNER   =  CODES.CCODE_NO
=SEEK('D'+'CDSGNSTAT ','CODES')  
M.CDSGNSTAT   =  CODES.CCODE_NO

   *-- Moving lines from ICDESIGN to ARTWRKDS
SELECT ICDESIGN
SCAN
     SCATTER MEMVAR MEMO
     IF EMPTY(M.ROYALTY)        && If ROYALTY field in ICDESIGN is empty, get it from CODES file
       M.ROYALTY = lcDefROYALTY
     ENDIF
     M.CDESIGNID = CDSGNCODE
     M.CVENCODE1 = CVENDCODE
     M.NPRICELVL1 = NDSGNPRC
     M.DDSGNDATE = DADD_DATE 
     SELECT ARTWRKDS
     M.TYPE ='D'
     IF !SEEK('D'+PADR(M.CDESIGNID,15),'ARTWRKDS')
       APPEND BLANK
       GATHER MEMVAR MEMO 
     ENDIF 
     WAIT WINDOW 'Converting lines...' NOWAIT 
ENDSCAN 
MESSAGEBOX('Conversion done!','Aria Systems')
