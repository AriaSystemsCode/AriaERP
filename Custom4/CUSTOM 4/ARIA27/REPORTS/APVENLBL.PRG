*B601404,1 HS 12/01/96 Fix the gfDispRe calling
*B800919,1 M.H Fix the bug of the Address parts order.
*B802268,1 BWA 06/10/99 Change the vendor code fixed filter to variable filter in 
*B802268,1 BWA 06/10/99 the SYDREPRT.DBF to get the vendor code IN LIST
*B802914,1 AKA 01/17/00 The ZIP code was printed before the STATE code which is not right. The right 
*B802914,1              formula should be City-State-Zip.
*B803318,1 AMH 06/14/2000 To Add 3*10 Vendor Mailing Label
*B038541,1 MHM 09/15/2004 modify vendor code from in range to in list in SYREPUVR 
************************************************************************

*B803318,1 AMH 06/14/2000 (Start)
* Beacuse the label will work in both sitaution (Text& Graphics) we will Stop
* the next command.
*lcOgPlatForm = 'DOS'
*B803318,1 AMH 06/14/2000 (End)

SELECT APVENDOR

*B803318,1 AMH 06/14/2000 (Start)
* Call the gfDispRe  with lcRpForm

*B601404,1 Change this line [Begin] 
*DO gfDispRe WITH 'APVENLBL','FOR '+lcRpExp,'L'
*DO gfDispRe WITH 'APVENLBL','FOR '+lcRpExp,.F.,'L'
*B601404,1 Change this line [End] 

DO gfDispRe WITH EVAL('lcRpForm') ,'FOR '+lcRpExp,.F.,'L'

*B803318,1 AMH 06/14/2000 (End)




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

*B802914,1 AKA 01/17/00 [Start]
*DO CASE
*  CASE SYCINT.NPART3ORD = lnMin
*    lcAddr = ALLTRIM(APVENDOR.CADDRESS3)
*    
*    *IF SYCINT.NPART4ORD = lnMax
*     IF SYCINT.NPART5ORD = lnMax    
*      *lcAddr = lcAddr +;
*               IIF(LEFT(APVENDOR.CADDRESS5,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS5)+;  
*               IIF(LEFT(APVENDOR.CADDRESS4,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS4)
*      lcAddr = lcAddr +;
*               IIF(LEFT(APVENDOR.CADDRESS4,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS4)+;
*               IIF(LEFT(APVENDOR.CADDRESS5,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS5)  
*    ELSE
*      *lcAddr = lcAddr +;
*               IIF(LEFT(APVENDOR.CADDRESS4,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS4)+;
*               IIF(LEFT(APVENDOR.CADDRESS5,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS5)  
*      lcAddr = lcAddr +;
*               IIF(LEFT(APVENDOR.CADDRESS5,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS5)+;  
*               IIF(LEFT(APVENDOR.CADDRESS4,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS4)
*    ENDIF           
* CASE SYCINT.NPART4ORD = lnMin
*    lcAddr = ALLTRIM(APVENDOR.CADDRESS4)
*    IF SYCINT.NPART5ORD = lnMax
*      lcAddr = lcAddr +;
*               IIF(LEFT(APVENDOR.CADDRESS3,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS3)+;  
*               IIF(LEFT(APVENDOR.CADDRESS5,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS5)
*    ELSE
*      lcAddr = lcAddr +;
*               IIF(LEFT(APVENDOR.CADDRESS5,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS5)+;
*               IIF(LEFT(APVENDOR.CADDRESS3,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS3)  
*    ENDIF           
*   CASE SYCINT.NPART5ORD = lnMin
*    lcAddr = ALLTRIM(APVENDOR.CADDRESS5)
*    IF SYCINT.NPART4ORD = lnMax
**B800919,1 M.H Begin.
**      lcAddr = lcAddr +;
*               IIF(LEFT(APVENDOR.CADDRESS3,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS3)+;  
*               IIF(LEFT(APVENDOR.CADDRESS4,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS4)
*      lcAddr = lcAddr +;
*               IIF(LEFT(APVENDOR.CADDRESS3,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS3)+;
*               IIF(LEFT(APVENDOR.CADDRESS4,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS4)  
*    ELSE
**      lcAddr = lcAddr +;
*               IIF(LEFT(APVENDOR.CADDRESS4,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS4)+;
*               IIF(LEFT(APVENDOR.CADDRESS3,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS3)  
*      lcAddr = lcAddr +;
*               IIF(LEFT(APVENDOR.CADDRESS4,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS4)+;  
*               IIF(LEFT(APVENDOR.CADDRESS3,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS3)
**B800919,1 M.H End.
*    ENDIF           
*ENDCASE  


DO CASE

  CASE SYCINT.NPART3ORD = lnMin
    lcAddr = ALLTRIM(APVENDOR.CADDRESS3)
    IF SYCINT.NPART5ORD = lnMax
      lcAddr = lcAddr +;
               IIF(LEFT(APVENDOR.CADDRESS4,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS4)+;  
               IIF(LEFT(APVENDOR.CADDRESS5,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS5)
    ELSE
      lcAddr = lcAddr +;
               IIF(LEFT(APVENDOR.CADDRESS5,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS5)+;
               IIF(LEFT(APVENDOR.CADDRESS4,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS4)  
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
    IF SYCINT.NPART4ORD = lnMax
      lcAddr = lcAddr +;
               IIF(LEFT(APVENDOR.CADDRESS3,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS3)+;
               IIF(LEFT(APVENDOR.CADDRESS4,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS4)  
    ELSE
      lcAddr = lcAddr +;
               IIF(LEFT(APVENDOR.CADDRESS4,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS4)+;  
               IIF(LEFT(APVENDOR.CADDRESS3,1)$', ','', ', ')+RTRIM(APVENDOR.CADDRESS3)
    ENDIF           
ENDCASE  
*B802914,1 AKA 01/17/00 [End]

RETURN lcAddr

*!*************************************************************
*! Name      : lfvPltForm
*! Developer : AMH (Ahmed Maher)
*! Date      : 06/14/2000
*! Purpose   : Function to Validate the format
*!*************************************************************
*B803318,1 AMH 06/14/2000 - Swap between the Text & Graphics modes.
FUNCTION lfvPltForm
= lfRepPltFr(lcRpForm)

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
   LOCATE
ENDCASE

SELECT(lnAlias)
RETURN

*-- End of lfSetVenRp.
