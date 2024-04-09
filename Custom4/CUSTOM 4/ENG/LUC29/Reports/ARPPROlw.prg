*:***************************************************************************
*: Program file  : ARPPROLW.PRG
*: Program desc. : CUSTOMIZED ProForma FOR LUCY WERNICK INC.
*: Date          : 18/09/2007
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER (SO)
*: Developer     : Mariam Mazhar[MMT] 
*: Tracking Job Number: C200860
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO SOORCNLW
*:***************************************************************************
*: Modifications :
*****************************************************************************
PRIVATE lnMainDbf
lnMainDbf = SELECT(0)

*-- Get the note

IF !USED('CODES')
  = gfOpenTable(oAriaApplication.DataDir + 'CODES' ,'CODES','SH')
ENDIF 

SELECT CCODE_NO,CRLTD_VLU FROM CODES WHERE CFLD_NAME='CDIVISION' AND CRLTD_NAM='CDIVIMGP' INTO CURSOR CODIMG READWRITE 
SELECT CODIMG
INDEX ON CCODE_NO TAG CODS

SELECT(lcTmpOrdhdr)
SET RELATION TO CDIVISION INTO CODIMG ADDITIVE 

SELECT(lnMainDbf)
			************** End of Program **************
