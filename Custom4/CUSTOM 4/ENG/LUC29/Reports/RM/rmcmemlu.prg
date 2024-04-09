*:***************************************************************************
*: Program file  : RMCMEMLU.PRG
*: Program desc. : CUSTOMIZED CREDIT MEMO FORM FOR LUCY WERNICK INC.
*: Date          : 17/07/2007
*: System        : Aria Advantage Series.
*: Module        : CREDIT MEMO (RM)
*: Developer     : NADER NABIL (NNA)
*: Job Number    : C#200819
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO RMCMEMLU
*:***************************************************************************
*: Modifications :
*:C201680,1 SAR 24-05-2015 T20150422.0007 - Request builder items missing
*****************************************************************************

*! Cursors to get diviosn data and image build .... Begin
STORE '' TO LCIMG
*C201680,1 SAR 24-05-2015 T20150422.0007 - Request builder items missing[Start]
IF TYPE('lcXMLFileName') = 'C'
	=gfOpenFile('CODES','CODES')
ENDIF
*C201680,1 SAR 24-05-2015 T20150422.0007 - Request builder items missing [END] 
SELECT CCODE_NO,CDISCREP FROM CODES WHERE CFLD_NAME='CDIVISION' AND CRLTFIELD='N' INTO CURSOR CODTAB READWRITE 
SELECT CODTAB
INDEX ON CCODE_NO TAG CODS
SELECT CCODE_NO,CDISCREP,CRLTD_VLU FROM CODES WHERE CFLD_NAME='CDIVISION' AND CRLTD_NAM='CDIVIMGP' INTO CURSOR CODIMG READWRITE 
SELECT CODIMG
INDEX ON CCODE_NO TAG CODS
LCIMG = CODIMG.CRLTD_VLU
SELECT RetHdr
SET RELATION TO CDIVISION INTO CODTAB ADDITIVE 
SET RELATION TO CDIVISION INTO CODIMG ADDITIVE 
