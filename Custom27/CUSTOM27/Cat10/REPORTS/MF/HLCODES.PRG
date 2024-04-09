*:****************************************************************
*: Program file  : HLCODES.PRG
*: Program desc. : Master Codes.
*: System        : Aria Apparel System (A27).
*: Module        : H.L.C. System. (HL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200246,1
*: Date          : 01/10/2002
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfwRepWhen , lfvCodes , lfCollect
*:               -----------------------------------------------
*:               : PROCEDURE  : 
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200246,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- Begin Declaration variables.
*-- lcTemp    :- Variable Hold the temp file that collect the data.
*-- laRpSource:- Source Array Hold the Codes.
*-- laRpTarget:- Target Array Hold the Codes.
*-- laRpcPYSrc:- Copy From the Source array.
*-- End Declaration variables.

*- Message Text   :- You must Select a code for the report to be displayed.
*- Message No.    :- 000000.
*- Buttom Message :- Ok
*- Buttom Number  :- 00000.
IF EMPTY(laRpTarget)
  = gfModalGen('INM00000B00000','F','ALERT',' ','You must Select a code for the report to be displayed.')
  RETURN
ENDIF

lcSavCent= SET('CENTURY')
SET CENTURY ON
lcDate = MDY(DATE())
SET CENTURY &lcSavCent

*-- if user change filter criteria then you must collect data again [Begin]
IF llOGFltCh
  *-- Create Temp File.
  CREATE TABLE (lcTemp) (cfld_head C(25) , cfld_name C(10), cdiscrep C(30))
  *-- Collecting Data In The First Time.
  = lfCollect ()
ENDIF

SELECT (lcTemp)
LOCATE
DO gfDispRe WITH EVAL('lcRpForm')

RETURN
*-- End of program code.
*:*************************************************************
*: Name      : lfwRepWhen
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : When function of OG
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfwRepWhen()
*:*************************************************************
*
FUNCTION lfwRepWhen

DIMENSION laRpSource[1] , laRpTarget[1] , laRpcPYSrc[1,2]
STORE '' TO laRpTarget , laRpSource , laRpcPYSrc

SELECT Sydfield.cfld_head;
  FROM &gcSyshome.sydField;
 WHERE sydField.lvldEntry= .T. .AND.  sydField.cupgrdlvl = 'U' ;
  INTO ARRAY laRpSource

SELECT Sydfield.cfld_name, Sydfield.cfld_head;
  FROM &gcSyshome.sydField;
 WHERE sydField.lvldEntry= .T. .AND.  sydField.cupgrdlvl = 'U' ;
  INTO ARRAY laRpcPYSrc

= ASORT(laRpSource)  


*-- End of lfwRepWhen.
*:*************************************************************
*: Name      : lfvCodes
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : to get all codes.
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfvCodes()
*:*************************************************************
*
FUNCTION lfvCodes

DIMENSION laOldCode [1]
laOldCode = ''
= ACOPY(laRpTarget,laOldCode)
=gfMover(@laRpSource,@laRpTarget,"Codes ",.T.,'')


IF ALEN (laOldCode) = ALEN(laRpTarget)
   =ASORT(laOldCode)
   =ASORT(laRpTarget)
  FOR X = 1 TO ALEN (laRpTarget)
    IF laOldCode[X] # laRpTarget[X]
      llOGFltCh = .T.
      EXIT
    ENDIF  
  ENDFOR
  
ELSE
  llOGFltCh = .T.
ENDIF

*-- End Of lfvCodes
*:*************************************************************
*: Name      : lfCollect
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : Collecting Data In The First Time.
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfCollect ()
*:*************************************************************
*
FUNCTION lfCollect

= ASORT(laRpTarget)  

FOR I = 1 TO ALEN(laRpTarget)
  WAIT WINDOW 'Selecting code ... ' + laRpTarget[I] NOWAIT
  lnCodePos = (ASCAN(laRpcPYSrc,laRpTarget[I]))/2
  lcFldName = laRpcPYSrc[lnCodePos,1]
  
  = SEEK(lcFldName,'Sydfield')
  SELECT CODES
  = SEEK('N'+lcFldName)
  SCAN REST WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = ;
                  'N'+lcFldName
    SELECT (lcTemp)
    APPEND BLANK
    REPLACE  cfld_head WITH Sydfield.cfld_head ,;
             cfld_name WITH Sydfield.cfld_name ,;
             cdiscrep  WITH Codes.cdiscrep
   ENDSCAN
   WAIT CLEAR
ENDFOR

*-- End OF lfCollect.
*:*************************************************************
