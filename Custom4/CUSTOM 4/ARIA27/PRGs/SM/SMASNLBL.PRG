*****************************************************************
* Program          : SMASNLBL.PRG
*----------------------------------------------------------------
* Purpose          : PROGRAM TO FIX all ASN Label from SycAsnLb to SycAsnHd  
*----------------------------------------------------------------
* Passed Parameter : System Files Path
*----------------------------------------------------------------
* Developer        : Waleed Hamed (WLD)
*----------------------------------------------------------------
* Date             : 10/11/2004
*----------------------------------------------------------------
PARAMETER lcDataDir

IF !USED('SYCASNLB')
  USE (lcDataDir+'SYCASNLB') IN 0 ORDER TAG ASNLBL SHARED
ENDIF
IF !USED('SYCASNHD')
  USE (lcDataDir+'SYCASNHD') IN 0 ORDER TAG VERPRT SHARED
ENDIF
SELECT SYCASNLB
GO TOP
DO WHILE !EOF()
  lclblVer = cVer
  IF !SEEK(lclblVer,'SYCASNHD')
    INSERT INTO SYCASNHD (cver,DESC1,ctype,ldetlabel) VALUES (lclblVer,lclblVer,'N',UPPER(LEFT(lclblVer,2))=='XX')
  ENDIF
  SCAN REST WHILE cver+ceditype+STR(nseq,4) = lclblVer
  ENDSCAN
ENDDO  
IF USED('SYCASNLB')
  USE IN SYCASNLB
ENDIF
IF USED('SYCASNHD')
  USE IN SYCASNHD
ENDIF
