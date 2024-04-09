*B500790,1  MAN 06/21/95 Changing the numeric fields picture in the FRX
*B500790,1               to print the commas   
*B602576,1 SAMEH 02/23/1999 commenting out the lines which contain
*B602576,1                  lcSavBrFld,lcBrFields,lcSavTitle,lcFile_Ttl 
*B602576,1					and use them as private variables
*B038541,1 MHM 09/15/2004 modify vendor code from in range to in list in SYREPUVR 
*B038541,1                and SYDREPRT
*B127457,1 MMR 06/27/2005 Fix bug of Selecting All Vendors.
*******************************************************************************

SET ORDER    TO TAG VENCODE   IN APVENDOR
SELECT APVENDOR
DO gfDispRe WITH EVAL('LCRPFORM'),'FOR '+lcRpExp

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
*: Date      : 09/15/2004
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
*:B038541,1 
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
