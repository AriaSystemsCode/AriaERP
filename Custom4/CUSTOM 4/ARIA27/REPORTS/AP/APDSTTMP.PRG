*B601403,1 RENEE 11/12/96. Fix misspell of 'Distribution' in report title
*E300643,1  HS 04/21/97 Make some changes for we have change the file
*E300643,1              SYCCODES name to CODES and make it a data file
*E300643,1              [Change its dir. from SYSFILES to DBFS]
*E300789,4  AMM 03/07/99 Remove field cComp_id from files Accod, FisHD, FsPrd, FsHld, Codes
************************************************************************************

*E300643,1 Change this lines for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
*SET FILTER TO SYCCODES.crltfield='N'
SELECT CODES
SET FILTER TO CODES.cRltField='N'
*E300643,1 Change this lines for the changes we have made to SYCCODES [End]

SELECT APINVAHD
SET ORDER TO TAG HTYPCOD 
SET ORDER TO TAG DTYPCOD IN APINVADT
SET RELATION TO apinvahd.cautmtype+ apinvahd.cautmcode INTO APINVADT ADDITIVE
SET SKIP TO APINVADT

DO gfDispRe WITH EVAL('LCRPFORM'),'FOR '+lcRpExp
SET RELATION TO

*E300643,1 Change this lines for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this lines for the changes we have made to SYCCODES [End]

SET FILTER TO

*!**************************************************************************
*!
*!      Function: lfvTmpCode
*!
*!**************************************************************************

FUNCTION lfvTmpCode

SELECT APINVAHD
lcSavOrd=SET("ORDER")
SET ORDER TO TAG HTYPCOD 

lcFldNam=SYS(18)

IF SEEK("T"+EVAL(SYS(18)))
  &lcFldNam=APINVAHD.CAUTMCODE
ELSE
  IF !SEEK(EVAL(SYS(18))) .OR. ATC("?",EVAL(SYS(18))) > 0
    DIMENSION laTemp[2]   
    laTemp = ''
    lcSavBrFld=lcBrFields
    lcSavTitle=lcFile_Ttl
  
    lcBrFields="CAUTMTYPE :H= 'Template type',;
                CAUTMCODE :H= 'Template code'"
    lcFile_Ttl= "Templates"

    =gfBrows([FOR CAUTMTYPE="T"],'CAUTMTYPE,CAUTMCODE','laTemp')

    lcBrFields=lcSavBrFld
    lcFile_Ttl=lcSavTitle
    
    IF !EMPTY(laTemp[2])
      &lcFldNam=laTemp[2]
    ELSE
      &lcFldNam=SPACE(24)
    ENDIF
  ENDIF
ENDIF

IF !EMPTY(lcSavOrd)
  SET ORDER TO &lcSavOrd
ENDIF