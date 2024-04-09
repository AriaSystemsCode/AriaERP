*!*************************************************************
*! Name      : alpktklu.prg
*: Developer : Tarek Noaman (TNA)
*: Date      : 02/14/2006
*! Purpose   : Get image path
*!*************************************************************
lnAlias = SELECT()
lcDivImgPath  = ''
DECLARE laDivImg[1,2]
laDivImg[1,1] = 'CDIVIMGP'
laDivImg[1,2] = 'lcDivImgPath'
SELECT (lcTmpOrdL)
DELETE FOR Cgrupdetal='H'
SELECT DISTINCT CDivision,REPLICATE(' ',80) AS Cimgpath FROM (lcORDHDR) INTO CURSOR lcTmpDivImg READWRITE
SELECT lcTmpDivImg
INDEX ON CDivision TAG div_idx
SCAN
  =gfRltFld(lcTmpDivImg.CDivision , @laDivImg , 'CDIVISION')
  REPLACE Cimgpath WITH lcDivImgPath
ENDSCAN
SELECT (lcORDHDR)
SET RELATION TO CDivision INTO lcTmpDivImg ADDITIVE 
SELECT(lnAlias)
LOCATE