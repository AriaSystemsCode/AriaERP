*:****************************************************************
*: Program file  : ICONPAT.PRG
*: Program desc. : Fix program to Convert pattern from 2.6 to 2.7.
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Inventory Control (IC)
*: Developer     : ABDOU ELGENDI -  (ABD)
*: Date          : 06/23/2003
*: Tracking Job Number : B#607236,1
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfGetStyle
*:               -----------------------------------------------
*:               : PROCEDURE  : None.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:B#607236,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- lcPathStyl : Path for the style file.
STORE '' TO lcPathStyl

llSPInstld = (OCCURS('SP',gcComp_Mdl)<>0)

*- Message Text   :- Style Data Manager  is not Installed for company ' +gcAct_Comp+'. Can not proceed.' 
*- Message No.    :- 000000.
*- Buttom Message :- Ok
*- Buttom Number  :- 00000.

lcMessage = 'Style Data Manager is not Installed for company ' +gcAct_Comp+'. Can not proceed.' 

IF !llSPInstld .AND. gfModalGen('INM00000B00000','F','ALERT', ' ',lcMessage) = 1
  RETURN
ENDIF

llOpnPatrn =  gfOpenFile(gcDataDir+'PDMPATRN','Cpattern','SH')


*- Message Text   :- 
*- Message No.    :- 000000.
*- Buttom Message :- Ok
*- Buttom Number  :- 00000.
IF gfModalGen('QRM00000B32000','F','ALERT',' ','Are you sure, you want to run Pattern Conversion to 2.7 program now ?') = 2
  RETURN
ENDIF

IF !lfGetStyle()
  RETURN
ENDIF

USE &lcPathStyl In 0 ORDER STYLEPAT
SELECT STYLE
LOCATE
SCAN
  WAIT WINDOW 'Convert pattern # :- ' + Pattern NOWAIT
  *-- Scan for pattern
  STORE '' TO lcPattern , lcRev_no
  *-- get the lenght of the field that because we should add 8 chracter from the
  *-- convert pattern to cpatt_no and the last 2 charcter from the pattern should
  *--  update the cRev_no
  
  lnFldLen  = LEN(ALLTRIM(PATTERN))
  IF lnFldLen>= 3
    lcPattern = PADR(LEFT(PATTERN,lnFldLen-2),8)
    lcRev_no  = RIGHT(ALLTRIM(PATTERN),2)
  ELSE
    lcPattern = PATTERN
  ENDIF
  *-- LEFT(cpatt_no,8)+crev_no
  IF !SEEK(lcPattern+lcRev_no,'PDMPATRN')
    *-- Add pattern to pattern file.
    SELECT PDMPATRN
    APPEND BLANK
    REPLACE cpatt_no WITH  lcPattern,;
            crev_no  WITH lcRev_no
  ENDIF

ENDSCAn


IF llOpnPatrn
  USE IN PDMPATRN
ENDIF

USE IN STYLE

*- Message Text   :- Process complete successfully.
*- Message No.    :- 000000.
*- Buttom Message :- Ok
*- Buttom Number  :- 00000.
= gfModalGen('INM00000B00000','F','ALERT',' ','Process complete successfully.')


RETURN

*-- End OF Code.
*:*************************************************************
*: Name      : lfGetStyle
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 06/23/2003
*: Purpose   : Funtion to get the Style File from 26
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfGetStyle()
*:*************************************************************
*:
FUNCTION lfGetStyle
PRIVATE lcoldPath , llSelStyle , llReturn
STORE .F. TO llSelStyle , llReturn

lcoldPath = FULLPATH('')

DO WHILE !llSelStyle
  lcPathStyl = GETFILE('DBF', 'Select style 2.6 :', 'Select',0)
  
  IF EMPTY(lcPathStyl)
    IF gfModalGen('QRM00000B32000','F','ALERT',' ','Are you sure you want to cancel the pattern conversion.') = 1
      EXIT
    ENDIF
  ELSE
    lcConStyle = UPPER(Right(lcPathStyl,9))

    IF lcConStyle = 'STYLE.DBF'
      STORE .T. TO llSelStyle , llReturn
    ELSE
      = gfModalGen('QRM00000B00000','F','ALERT',' ','Please make sure that you select the STYLE.DBF file from application 2.6 .')
    ENDIF
  ENDIF  
ENDDO

SET DEFA TO &lcoldPath
RETURN llReturn
*-- End OF lfGetStyle
*:*************************************************************