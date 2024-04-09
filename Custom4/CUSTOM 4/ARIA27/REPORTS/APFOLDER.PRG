*B601404,1 HS 12/01/96 Fix the gfDispRe calling
*B802268,1 BWA 06/10/99 Change the vendor code fixed filter to variable filter in 
*B802268,1 BWA 06/10/99 the SYDREPRT.DBF to get the vendor code IN LIST
*B038541,1 MHM 09/15/2004 modify vendor code from in range to in list in SYREPUVR 
************************************************************
lcOgPlatForm = 'DOS'
SELECT APVENDOR
*B601404,1 Change this line [Begin] 
*DO gfDispRe WITH 'APFOLDER','FOR '+lcRpExp,'L'
DO gfDispRe WITH 'APFOLDER','FOR '+lcRpExp,.F.,'L'
*B601404,1 Change this line [End] 
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
