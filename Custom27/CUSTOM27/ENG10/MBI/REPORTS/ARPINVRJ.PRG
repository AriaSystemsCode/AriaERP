*!**************************************************************************
*! Name      : ARPINVRJ
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 10/17/02
*! Purpose   : Print Invoice Logo based on Division
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C200418
*--
lnAlias = SELECT()
llLogDiv = (gfGetMemVar('M_DIVLOG') = 'Y')
IF llLogDiv
  SELECT OBJLINK
  SET RELATION TO Objlink.cobject_id INTO Objects ADDITIVE
  SELECT INVHDR
  SET RELATION TO "*"+INVHDR.CDIVISION INTO OBJLINK ADDI
  llLogo = .T.
ELSE
  llLogo = IIF(SEEK('*' + 'LOGO' , 'OBJLINK') .AND. SEEK(OBJLINK.cObject_ID ,;
               'OBJECTS') , .T. , .F.)        && Flag to know if we are to print the Company Logo
ENDIF
SELECT (lnAlias)