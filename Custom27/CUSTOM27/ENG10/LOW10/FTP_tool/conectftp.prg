*E302377,1 HIA Upload all files in the upload folder [Begin]
*PARAMETERS tcBatch
PARAMETERS tcBatch, lctxtuserName,; 
                    lctxtpassword,; 
                    lctxtFTP,;
                    lctxtincoming,;
                    lctxthistory,; 
                    lctxtoutgoing,;                        
                    lctxtmailboxAdd,;                       
                    lctxtbatch 

*E302377,1 HIA Upload all files in the upload folder [End]
CLOSE ALL

_SCREEN.WIDTH       = 403 + 2
_SCREEN.HEIGHT      = 102 - 40
_SCREEN.AUTOCENTER  = .T.
_SCREEN.CONTROLBOX  =.F.
_SCREEN.MAXBUTTON     =.F.
_SCREEN.MINBUTTON     =.F.
_SCREEN.CAPTION='FTP Tool'

SET TALK OFF
SET SAFETY OFF
SET STATUS BAR OFF
SET SYSMENU OFF

IF TYPE('tcBatch') <> 'L'
  _SCREEN.WINDOWSTATE= 1
ENDIF
*E302377,1 HIA Upload all files in the upload folder [Begin]
*DO FORM connect_ftp WITH tcBatch
IF TYPE('tcBatch') = 'C'
*!*	  DO FORM connect_ftp WITH tcBatch, EVALUATE(lctxtuserName),; 
*!*	                      EVALUATE(lctxtpassword),; 
*!*	                      EVALUATE(lctxtFTP),;
*!*	                      EVALUATE(lctxtincoming),;
*!*	                      EVALUATE(lctxthistory),; 
*!*	                      EVALUATE(lctxtoutgoing),;                        
*!*	                      EVALUATE(lctxtmailboxAdd),;                       
*!*	                      EVALUATE(lctxtbatch )
  DO FORM connect_ftp WITH tcBatch, STRTRAN(lctxtuserName,"'",""),; 
                      STRTRAN(lctxtpassword,"'",""),; 
                      STRTRAN(lctxtFTP,"'",""),; 
                      STRTRAN(lctxtincoming,"'",""),;
                      STRTRAN(lctxthistory,"'",""),; 
                      STRTRAN(lctxtoutgoing,"'",""),;                        
                      STRTRAN(lctxtmailboxAdd,"'",""),;                       
                      STRTRAN(lctxtbatch,"'","") 
                                            
ELSE                      
  DO FORM connect_ftp WITH tcBatch
ENDIF                      
*E302377,1 HIA Upload all files in the upload folder [End]
RELEASE ALL
CLOSE ALL
QUIT
