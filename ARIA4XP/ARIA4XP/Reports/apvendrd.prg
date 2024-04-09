*:***************************************************************************
*: Program file  : APVNHSTS
*: Program desc. : Print Vendor history
*: For Report    : (N000574) T20070325.0004   
*: System        : Aria Advantage Series.4XP
*: Module        : MA 03/25/2007
*: Developer     : Hassan Ibrahim Ali [HIA]
*:***************************************************************************
*SET ORDER    TO TAG CCODE_NO  IN SYCCODES
SET ORDER    TO TAG CCODE_NO  IN CODES 

SET ORDER    TO TAG CFACCODE  IN SYCFACT
SET ORDER    TO TAG CCONTCODE IN SYCINT
SET ORDER    TO TAG VENCODE   IN APVENDOR
SELECT APVENDOR 
SET RELATION TO apvendor.cfaccode   INTO SYCFACT ADDITIVE
SET RELATION TO apvendor.CCONT_CODE INTO SYCINT  ADDITIVE
loogScroll.cCROrientation = 'L'

DO gfDispRe WITH EVAL('LCRPFORM'),'FOR '+IIF(!EMPTY(lcRpExp),lcRpExp,'.T. ')+ IIF(!EMPTY(lcRPAddDate)," AND APVENDOR.DADD_DATE = lcRPAddDate",'')

SET RELATION TO

*:*************************************************************
*: Name      : lfAddress
*: Developer : Hassan Ibrahim Ali [HIA]
*: Date      : 03/25/2007
*: Purpose   : Get Vendor address.
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : Position.
*:*************************************************************
*: Example     : = lfAddress()
*:************************************************************* 
FUNCTION lfAddress

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

*:*************************************************************
*: Name      : lfPayMeth
*: Developer : Hassan Ibrahim Ali [HIA]
*: Date      : 03/25/2007
*: Purpose   : Get Payment Method Descryption.
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : Position.
*:*************************************************************
*: Example     : = lfPayMeth()
*:************************************************************* 
FUNCTION lfPayMeth

RETURN IIF(APVENDOR.CVENPMETH='M','Manual Checks',;
	    IIF(APVENDOR.CVENPMETH='P','Printed Checks',;
	    IIF(APVENDOR.CVENPMETH='H','Cash Payment','Non Check Payment')))

*:*************************************************************
*: Name      : lfGetComp
*: Developer : Hassan Ibrahim Ali [HIA]
*: Date      : 03/25/2007
*: Purpose   : Get Vendor Codes And Company.
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : Position.
*:*************************************************************
*: Example     : = lfGetComp()
*:************************************************************* 

FUNCTION lfGetComp
PRIVATE lcBrFields, lcBrFields

*** save the current alias. ***
lcSavAlias = ALIAS()
lcFldNam=SYS18()
*** if not empty of vendor or the mouse clicked. ***
IF !EMPTY(EVAL(SYS18())) 
  SELECT APVENDOR
  lcSavOrder = SET('ORDER')   && Save old order
  SET ORDER TO TAG VENCODE
  IF SEEK(EVAL(SYS18()))
    &lcFldNam=CVENDCODE
  ELSE
    *** Define an array get Vendors codes and company.***
    IF !SEEK(EVAL(SYS18())) .OR. ATC("?",EVAL(SYS18())) > 0
      DIMENSION laTemp[2]
      laTemp = '' && fill the array.

      *** Save the old fields and title.***   
    
      *** Get new fields name and title.***    
      lcBrFields="CVENDCODE :H= 'Vendor',;
                  CVENCOMP  :H= 'Company'"
      lcFile_Ttl="Vendors"
      *** Browse ***
      =gfBrows(.F.,'CVENDCODE,CVENCOMP','laTemp')

      *** Get the old fields name and title.***    
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
*: Developer : Hassan Ibrahim Ali [HIA]
*: Date      : 03/25/2007
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
FUNCTION lfSetVenRp
PARAMETERS OpGrdParm
PRIVATE lnAlias

lnAlias = SELECT (0)

DO CASE
  CASE OpGrdParm = 'S'
   SELECT APVENDOR
   *Fix bug of Selecting All Vendors[Start] 
   SET ORDER TO Vencode
   *Fix bug of Selecting All Vendors[End]
   LOCATE
ENDCASE

SELECT(lnAlias)
RETURN
*-- End of lfSetVenRp.
*:*************************************************************