PARAMETER LCFORCEIMP
*--Start the application.
SET DELE ON
SET SAFETY OFF
SET TALK OFF

CLOSE DATA
USE Setup IN 0 SHARED

IF SETUP.CTYPE = 'FORCE' AND TYPE('LCFORCEIMP') = 'C' AND LCFORCEIMP = '.T.'
  =MESSAGEBOX("Importing process is currently running, unable to run the program now.",16,'Terminate')
  CLEAR EVENTS
  RETURN 

ELSE

  IF TYPE('LCFORCEIMP') = 'C' AND LCFORCEIMP = '.T.'
    LCFORCE = .T.
  ELSE
    LCFORCE = .F.
  ENDIF

  DO Form frmMain WITH LCFORCE
  read events
ENDIF
