*!*************************************************************
*! Name      : gpErTrap
*! Developer : Yasser Saad Ibrahime
*! Date      : 1993-1995 
*! Purpose   : Error trap procedure
*!*************************************************************
*! Calls     : 
*!      Called by: ARIA3.PRG                
*!          Calls: ERRORMSG.SPR             
*!*************************************************************
*! Passed Parameters  : Error number
*!                      Error message
*!                      Source code line
*!                      Line number
*!                      Program name
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*:->>
PROCEDURE  gpErTrap
PARAMETERS lnErrNo,lcMessag,lcCode,lnLine,lcProgram

lcCurDbf = ALIAS()

IF !USED('SYCERROR')
  SELECT 0
  USE &gcSysHome.SYCERROR ORDER 1
ENDIF

IF !SEEK(lnErrNo,'SYCERROR')
  =SEEK(2000,'SYCERROR')
ENDIF

FOR lnPrg = 1 TO 20
  IF EMPTY(SYS(16,lnPrg))
    EXIT
  ENDIF    
ENDFOR

lcPrg1 = SYS(16,lnPrg-4)
lcPrg2 = SYS(16,lnPrg-5)

INSERT INTO &gcSysHome.SYUEROR;
     ( er_no, er_msg, kode, line_no, mmodul );
	VALUES (lnErrNo,lcMessag,lcCode,lnLine,lcPrg1)

DO ERRORMSG.SPR

IF RDLEVEL() > 1
  glQuitting = .t.
  CLEAR READ 
ELSE  
  IF !EMPTY(lcCurDbf)
    SELECT (lcCurDbf)
  ENDIF  
  RETURN TO MASTER
ENDIF  

IF !EMPTY(lcCurDbf)
  SELECT (lcCurDbf)
ENDIF  
