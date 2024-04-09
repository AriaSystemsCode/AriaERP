_Screen.Visible =.F.
SET STEP ON 
lcFileCSV = GETFILE('CSV','Select CSV File')
IF EMPTY(lcFileCSV) OR !FILE(lcFileCSV)
  MESSAGEBOX('Invalid input CSV file.')
  RETURN .F.
ENDIF
lcDataDir = GETDIR('','Select DBFs Directory.')
IF EMPTY(lcDataDir) OR !DIRECTORY(lcDataDir) OR !FILE(ADDBS(lcDataDir)+"STYLE.DBF")
  MESSAGEBOX('Invalid DBFs Folder.')
  RETURN .F.
ENDIF

CREATE CURSOR 'TmpData' (Style C(19), Weight N(7,2),Width N(7,2),Height N(7,2), Depth N(7,2))
SELECT  'TmpData'
APPEND FROM  (lcFileCSV) TYPE CSV
LOCATE 
IF EOF()
 MESSAGEBOX('The input CSV file is empty')
 RETURN .F.
ENDIF

USE (ADDBS(lcDataDir)+"STYLE.DBF") SHARED IN 0 ORDER STYLE
SELECT 'TmpData'
SCAN 
  IF SEEK(ALLTRIM(PADR(TmpData.STYLE,19)),"STYLE","STYLE")
    SELECT STYLE
    SCAN REST WHILE STYLE = ALLTRIM(PADR(TmpData.STYLE,19))
      REPLACE ninpackhgt WITH TmpData.Height ,;
			  ninpacklen WITH TmpData.Depth ,;
			  ninpackwgt WITH TmpData.Weight ,;
			  ninpackwdt WITH TmpData.Width ,;
			  CPackUOM WITH '2' in STYLE
    ENDSCAN 
  ENDIF
ENDSCAN
USE IN STYLE
USE IN TmpData
CLOSE ALL
MESSAGEBOX("Style table has been updated successfully")