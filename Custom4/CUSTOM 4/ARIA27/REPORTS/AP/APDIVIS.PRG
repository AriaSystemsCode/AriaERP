*E300643,1  HS 04/21/97 Make some changes for we have change the file
*E300643,1              SYCCODES name to CODES and make it a data file
*E300643,1              [Change its dir. from SYSFILES to DBFS]
*E300789,4  AMM 03/07/99 Remove field cComp_id from files Accod, FisHD, FsPrd, FsHld, Codes
************************************************************************************

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SET ORDER TO TAG CCODE_NO IN SYCCODES
SET ORDER TO TAG CCODE_NO IN CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

SELECT APDIV
SET ORDER TO TAG DIVISION  

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SET RELATION TO gcAct_Comp+"CDIVISION "+APDIV.CDIVISION INTO SYCCODES ADDITIVE
*E300789,4  AMM Adjust to fit the new index
*SET RELATION TO gcAct_Comp+"CDIVISION "+APDIV.CDIVISION INTO CODES ADDITIVE
SET RELATION TO 'N'+"CDIVISION "+APDIV.CDIVISION INTO CODES ADDITIVE
*E300789,4  AMM end
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

DO gfDispRe WITH EVAL('LCRPFORM'),'FOR '+lcRpExp

SET RELATION TO

*!**************************************************************************
*!
*!      Function: lfvChecks
*!
*!**************************************************************************
*
FUNCTION lfvChecks

SELECT APCHECKS
lcSavOrder = SET('ORDER')   && Save old order
SET ORDER TO TAG BANKCHECK
lcFldNam = SYS(18)
IF SEEK(EVAL(SYS(18)))
  &lcFldNam=APCHECKS.CCHKACCT
ELSE
  IF !SEEK(EVAL(SYS(18))) .OR. ATC("?",EVAL(SYS(18))) > 0
    DIMENSION laTemp[2]   && Define an array get Vendors codes and company.
    laTemp = ''
    lcSavBrFld=lcBrFields
    lcSavTitle=lcFile_Ttl
  
    lcBrFields="CBNKCODE :H= 'Bank Code',;
                CCHKACCT :H= 'Check Code'"
    lcFile_Ttl= "Checks"

    =gfBrows(.F.,'CBNKCODE,CCHKACCT','laTemp')

    lcBrFields=lcSavBrFld
    lcFile_Ttl=lcSavTitle
    
    IF !EMPTY(laTemp[2])
      &lcFldNam=laTemp[2]
    ELSE
      &lcFldNam=SPACE(24)
    ENDIF
  ENDIF
ENDIF
SET ORDER TO &lcSavOrder
SELECT APDIV


 

