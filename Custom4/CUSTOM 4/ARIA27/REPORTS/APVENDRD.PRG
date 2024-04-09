**************************************************************************
* Modifications :_
*
*B500790,1  MAN 06/21/95 Changing the numeric fields picture in the FRX
*B500790,1               to print the commas   
*
*E300266,6 Reham 08/08/95 In (APVENDRD.FRX) call func. gfGetAdr() to print
*E300266,6                the right format.
*B601013,1 RENEE 04/02/96. Correct the spelling of 'Curruncy' to 'Currency'
*B601013,1                 in APVENDRD.FRX
*B800739,1 M.H On 09/09/96 change the expression of the terms field 
*B800739,1 M.H             From :LOOKUP(SYCCODES.CDISCREP,(gcPrnt_Cmp)+apvendor.CTERMCODE,SYCCODES.CFLD_NAME,"CCODE_NO")
*B800739,1 M.H             To   :LOOKUP(SYCCODES.CDISCREP,(gcPrnt_Cmp)+apvendor.CTERMCODE,SYCCODES.CFLD_NAME,"CODES")
*E300643,1  HS 04/21/97 Make some changes for we have change the file
*E300643,1              SYCCODES name to CODES and make it a data file
*E300643,1              [Change its dir. from SYSFILES to DBFS]
*B602576,1 SAMEH 02/23/1999 commenting out the lines which contain
*B602576,1                  lcSavBrFld,lcBrFields,lcSavTitle,lcFile_Ttl 
*B602576,1					and use them as private variables
*E300789,4  AMM 03/07/99 Remove field cComp_id from files Accod, FisHD, FsPrd, FsHld, Codes
*B038541,1 MHM 09/15/2004 modify vendor code from in range to in list in SYREPUVR 
*B038541,1                and SYDREPRT
*B127457,1 MMR 06/27/2005 Fix bug of Selecting All Vendors.
**************************************************************************

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SET ORDER    TO TAG CCODE_NO  IN SYCCODES
SET ORDER    TO TAG CCODE_NO  IN CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

SET ORDER    TO TAG CFACCODE  IN SYCFACT
SET ORDER    TO TAG CCONTCODE IN SYCINT
SET ORDER    TO TAG VENCODE   IN APVENDOR
SELECT APVENDOR
SET RELATION TO apvendor.cfaccode   INTO SYCFACT ADDITIVE
SET RELATION TO apvendor.CCONT_CODE INTO SYCINT  ADDITIVE

DO gfDispRe WITH EVAL('LCRPFORM'),'FOR '+lcRpExp

SET RELATION TO

*!**************************************************************************
*!
*!      Function: lfAddress
*!
*!**************************************************************************
* 
FUNCTION lfAddress
*Removed By MAN
*RETURN IIF(ALLTRIM(STR(SYCINT.NPART3ORD))+ALLTRIM(STR(SYCINT.NPART4ORD))+ALLTRIM(STR(SYCINT.NPART5ORD))='123',;
ALLTRIM(APVENDOR.CADDRESS3)+" "+ALLTRIM(APVENDOR.CADDRESS4)+" "+ALLTRIM(APVENDOR.CADDRESS5),;
IIF(ALLTRIM(STR(SYCINT.NPART3ORD))+ALLTRIM(STR(SYCINT.NPART4ORD))+ALLTRIM(STR(SYCINT.NPART5ORD))='132',;
ALLTRIM(APVENDOR.CADDRESS3)+" "+ALLTRIM(APVENDOR.CADDRESS5)+" "+ALLTRIM(APVENDOR.CADDRESS4),;
IIF(ALLTRIM(STR(SYCINT.NPART3ORD))+ALLTRIM(STR(SYCINT.NPART4ORD))+ALLTRIM(STR(SYCINT.NPART5ORD))='213',;
ALLTRIM(APVENDOR.CADDRESS4)+" "+ALLTRIM(APVENDOR.CADDRESS3)+" "+ALLTRIM(APVENDOR.CADDRESS5),;
IIF(ALLTRIM(STR(SYCINT.NPART3ORD))+ALLTRIM(STR(SYCINT.NPART4ORD))+ALLTRIM(STR(SYCINT.NPART5ORD))='231',;
ALLTRIM(APVENDOR.CADDRESS4)+" "+ALLTRIM(APVENDOR.CADDRESS5)+" "+ALLTRIM(APVENDOR.CADDRESS3),;
IIF(ALLTRIM(STR(SYCINT.NPART3ORD))+ALLTRIM(STR(SYCINT.NPART4ORD))+ALLTRIM(STR(SYCINT.NPART5ORD))='312',;
ALLTRIM(APVENDOR.CADDRESS5)+" "+ALLTRIM(APVENDOR.CADDRESS3)+" "+ALLTRIM(APVENDOR.CADDRESS4),;
ALLTRIM(APVENDOR.CADDRESS5)+" "+ALLTRIM(APVENDOR.CADDRESS4)+" "+ALLTRIM(APVENDOR.CADDRESS3))))))

lnMin = MIN(MIN(SYCINT.NPART3ORD ,SYCINT.NPART4ORD),SYCINT.NPART5ORD)
lnMax = MAX(MAX(SYCINT.NPART3ORD ,SYCINT.NPART4ORD),SYCINT.NPART5ORD)
DO CASE
  CASE SYCINT.NPART3ORD = lnMin
    lcAddr = ALLTRIM(APVENDOR.CADDRESS3)
    IF SYCINT.NPART4ORD = lnMax
      lcAddr = lcAddr +;
               IIF(LEFT(APVENDOR.CADDRESS5,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS5)+;  
               IIF(LEFT(APVENDOR.CADDRESS4,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS4)
    ELSE
      lcAddr = lcAddr +;
               IIF(LEFT(APVENDOR.CADDRESS4,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS4)+;
               IIF(LEFT(APVENDOR.CADDRESS5,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS5)  
    ENDIF           
  CASE SYCINT.NPART4ORD = lnMin
    lcAddr = ALLTRIM(APVENDOR.CADDRESS4)
    IF SYCINT.NPART5ORD = lnMax
      lcAddr = lcAddr +;
               IIF(LEFT(APVENDOR.CADDRESS3,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS3)+;  
               IIF(LEFT(APVENDOR.CADDRESS5,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS5)
    ELSE
      lcAddr = lcAddr +;
               IIF(LEFT(APVENDOR.CADDRESS5,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS5)+;
               IIF(LEFT(APVENDOR.CADDRESS3,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS3)  
    ENDIF           
   CASE SYCINT.NPART5ORD = lnMin
    lcAddr = ALLTRIM(APVENDOR.CADDRESS5)
    IF SYCINT.NPART3ORD = lnMax
      lcAddr = lcAddr +;
               IIF(LEFT(APVENDOR.CADDRESS3,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS3)+;  
               IIF(LEFT(APVENDOR.CADDRESS4,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS4)
    ELSE
      lcAddr = lcAddr +;
               IIF(LEFT(APVENDOR.CADDRESS4,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS4)+;
               IIF(LEFT(APVENDOR.CADDRESS3,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS3)  
    ENDIF           
ENDCASE  
RETURN lcAddr

*!**************************************************************************
*!
*!      Function: lfPayMeth
*!
*!**************************************************************************
*
FUNCTION lfPayMeth

RETURN IIF(APVENDOR.CVENPMETH='M','Manual Checks',;
	    IIF(APVENDOR.CVENPMETH='P','Printed Checks',;
	    IIF(APVENDOR.CVENPMETH='H','Cash Payment','Non Check Payment')))

*!**************************************************************************
*!
*!      Function: lfGetComp
*!
*!**************************************************************************
*
FUNCTION lfGetComp
*B602576,1 Use private variables
PRIVATE lcBrFields, lcBrFields
*B602576,1 end

*** save the current alias. ***
lcSavAlias = ALIAS()
lcFldNam=SYS(18)
*** if not empty of vendor or the mouse clicked. ***
IF !EMPTY(EVAL(SYS(18))) 
  SELECT APVENDOR
  lcSavOrder = SET('ORDER')   && Save old order
  SET ORDER TO TAG VENCODE
  IF SEEK(EVAL(SYS(18)))
    &lcFldNam=CVENDCODE
  ELSE
    *** Define an array get Vendors codes and company.***
    IF !SEEK(EVAL(SYS(18))) .OR. ATC("?",EVAL(SYS(18))) > 0
      DIMENSION laTemp[2]
      laTemp = '' && fill the array.

      *** Save the old fields and title.***   
	  *B602576,1 Used as private variables, hence, commented out
      *lcSavBrFld=lcBrFields 
      *lcSavTitle=lcFile_Ttl 
      *B602576,1 end
    
      *** Get new fields name and title.***    
      lcBrFields="CVENDCODE :H= 'Vendor',;
                  CVENCOMP  :H= 'Company'"
      lcFile_Ttl="Vendors"
      *** Browse ***
      =gfBrows(.F.,'CVENDCODE,CVENCOMP','laTemp')

      *** Get the old fields name and title.***    
	  *B602576,1 not used anymore
      *lcSavBrFld=lcBrFields 
      *lcSavTitle=lcFile_Ttl 
      *B602576,1 end

      *** if not empty of browse. ***
      IF !EMPTY(laTemp[1])
        &lcFldNam  = laTemp[2]  && get the vendor comp.
      ELSE                      && if empty
        &lcFldNam  = " "           
      ENDIF
    ENDIF  
  ENDIF
ENDIF
SELECT(lcSavAlias) 
*:*************************************************************
*: Name      : lfSetVenRp
*: Developer : Mohamed Shokry . [MHM]
*: Date      : 09/14/2004
*: Purpose   : Go top in Vendor file.
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : Position.
*:*************************************************************
*: Example     : = lfSetVenRp()
*:*************************************************************
*:B038541
FUNCTION lfSetVenRp
PARAMETERS OpGrdParm
PRIVATE lnAlias

lnAlias = SELECT (0)

DO CASE
  CASE OpGrdParm = 'S'
   SELECT APVENDOR
   *B127457,1 MMR 06/27/2005 Fix bug of Selecting All Vendors[Start] 
   SET ORDER TO Vencode
   *B127457,1 MMR [End]
   LOCATE
ENDCASE

SELECT(lnAlias)
RETURN

*-- End of lfSetVenRp.
