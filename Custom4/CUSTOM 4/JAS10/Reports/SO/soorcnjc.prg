*!***********************************************************************************
*! Name         : SOORCNJC.PRG
*! Developer    : Mostafa Eid(MOS)
*! Date         : 05/13/2009
*! Module       : SO (Sales Order)
*! Purpose      : Custom Order Confirmation form for Direct Corporate Clothing (DIR03)
*! Reference    : C201149  TICKET# - T20090313.0006 
*!***********************************************************************************
*! Called from  : Option Grid
*!***********************************************************************************
*! Calls        : ......
*!***********************************************************************************
*! Passed Parameters : None
*!***********************************************************************************
*! Return       : None
*!***********************************************************************************
*! Modifications:
*!***********************************************************************************

*-- if Conslidate store is No RETURN TO The standard program 
IF !llRPConsStyle   
  lcFormName = "SOORCNA"
  lcOgTmpForm = IIF(EMPTY(lcOgTmpForm),loOgScroll.gfTempName(),lcOgTmpForm)
  =gfCrtFrm(lcFormName,'',llOGRefForm)  && Create Temp. file for new form.
 SELECT ORDHDR
 RETURN 
ENDIF 


lcTargtFl = loogScroll.gftempname()
lcOrdInf = loogScroll.gftempname()

IF !USED('STYLEUPC')
 =gfOpenTable('STYLEUPC','STYLEUPC','SH')
ENDIF

=lfCrtTmp()
=lfColData()

lcTempOrd = lcTargtFl

SELECT (lcTempOrd )
SET RELATION TO 'S'+SUBSTR(Style,1,lnMajorLen) INTO OBJLINK_A ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SET RELATION TO style INTO Style ADDITIVE

SELECT ORDHDR
SET RELATION TO order INTO (lcTempOrd) ADDITIVE

SELECT (lcTempOrd )
LOCATE

SELECT ORDHDR
SET SKIP TO &lcTempOrd
LOCATE


*!***********************************************************************************
*! Name         : lfCrtTmp
*! Developer    : Mostafa Eid
*! Date         : 05/13/2009
*! Purpose      : Create Temp file
*!***********************************************************************************
*! Called from  : Soorcndr.prg
*!***********************************************************************************
*! Passed Parameters : None
*!***********************************************************************************
*! Return       : None
*!***********************************************************************************
*! Example      : = lfCrtTmp()
*!***********************************************************************************
FUNCTION lfCrtTmp

IF USED(lcTargtFl) AND RECCOUNT(lcTargtFl) > 0
 USE IN (lcTargtFl)
ENDIF

*-- Create File

SELECT ORDLINE
=AFIELDS(laTmpStru)
lnTmpStru = ALEN(laTmpStru,1)

DIMENSION laTmpStru[ALEN(laTmpStru , 1) + 2 , 18]

lnI = 0
lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'upc_no'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 13
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'AMOUNT'
laTmpStru[lnTmpStru + lnI ,2] = 'N'
laTmpStru[lnTmpStru + lnI ,3] = 13
laTmpStru[lnTmpStru + lnI ,4] = 2

FOR lnI = 1 TO ALEN(laTmpStru,1)-lnTmpStru
  STORE .F. TO laTmpStru[lnTmpStru+lnI,5],laTmpStru[lnTmpStru+lnI,6]
  STORE ''  TO laTmpStru[lnTmpStru+lnI,7],laTmpStru[lnTmpStru+lnI,8],laTmpStru[lnTmpStru+lnI,9],laTmpStru[lnTmpStru+lnI,10],laTmpStru[lnTmpStru+lnI,11],;
               laTmpStru[lnTmpStru+lnI,12],laTmpStru[lnTmpStru+lnI,13],laTmpStru[lnTmpStru+lnI,14],laTmpStru[lnTmpStru+lnI,15],laTmpStru[lnTmpStru+lnI,16]
  STORE 0  TO laTmpStru[lnTmpStru+lnI,17],laTmpStru[lnTmpStru+lnI,18]
ENDFOR
=gfCrtTmp(lcTargtFl,@laTmpStru)

SELECT (lcTargtFl)
INDEX ON ORDER + STYLE  TAG (lcTargtFl)
SET ORDER TO TAG (lcTargtFl)


*!***********************************************************************************
*! Name        : lfColData
*! Developer   : Mostafa Eid (MOS)
*! Date        : 01/21/2009
*! Purpose     : Collecting Data
*!***********************************************************************************
*! Called from : Soorcndr.Prg
*!***********************************************************************************
*! Passed Parameters : None
*!***********************************************************************************
*! Return      : ....
*!***********************************************************************************

FUNCTION lfColData
SELECT ORDHDR
SET RELATION OFF INTO (lcTempOrd)
SELECT (lcTempOrd)
INDEX on ORDER+STYLE TAG lcStyle
SET ORDER TO lcStyle
SCAN 
  lcKey = ORDER+STYLE
  SELECT STYLEUPC
  gfSeek(&lcTempOrd..Style,'STYLEUPC','STYLEUPC')
  m.upc_no = cupcnum1+cupcnum2+cupcnum3 

  SELECT (lcTempOrd)
  lnTotqty = 0  
  lnAmount = 0  
  SCATTER MEMVAR MEMO
  SCAN REST WHILE  ORDER+STYLE = lcKey
    lnTotqty = lnTotqty + TOTQTY
    lnAmount = lnAmount + TOTQTY * PRICE     
  ENDSCAN
  
  SKIP-1
  m.TOTQTY = lnTotqty  
  m.AMOUNT = lnAmount
  INSERT INTO (lcTargtFl) FROM MEMVAR
ENDSCAN 

*--End of Function lfColData.
