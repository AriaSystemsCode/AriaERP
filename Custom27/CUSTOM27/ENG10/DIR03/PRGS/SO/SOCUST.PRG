*!**************************************************************************
*! Name      : DAVMAIN.PRG
*! Developer : Tarek Mohammed Ibrahim (TMI)
*! Date      : 12/21/2004
*! Purpose   : Fix program for DIR03 ( DCC )
*              a  new user definable field is added to the Customer File called 
*              "Compulsory Customer PO ?      Yes/No". 
*              This field should default to "Yes".
*              this fix set this field for old added customers to 'Yes'
*!**************************************************************************
lcMsg = ;
 'This program will update the CUSTOMER file by setting the field "Compulsory Customer PO" to "Yes" for all added customers'+;
 ', Continue ?'

IF gfModalGen('INM00000B00006',.F.,.F.,.F.,lcMsg) <> 1
  RETURN
ENDIF

IF !USED('CUSTOMER')
  =gfOpenFile(gcDataDir+'CSTPRICH','CUSTOMER','SH')
ENDIF


SELECT CUSTOMER
SET ORDER TO CUSTOMER
GO TOP
=SEEK('M')
SCAN REST WHILE TYPE+ACCOUNT+STORE = 'M'
  REPLACE LCMPCSTPO WITH .T.
ENDSCAN

=gfModalGen('INM00000B00000',.F.,.F.,.F.,'Customer file updated.')