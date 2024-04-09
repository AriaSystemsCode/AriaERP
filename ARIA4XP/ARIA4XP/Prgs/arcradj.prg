*! E037885,2 MAH 12/03/2004 Set this form refrerence in the host form [START]
IF TYPE('oAriaApplication.cHostFormName') = 'C' .AND. !EMPTY(oAriaApplication.cHostFormName)
  LOCAL lnIndex 
  FOR lnIndex = 1 TO _SCREEN.FormCount
    IF _SCREEN.Forms(lnIndex).Name == oAriaApplication.cHostFormName
      _SCREEN.Forms(lnIndex).Release()
      EXIT
    ENDIF
  ENDFOR
ENDIF
*! E037885,2 MAH 12/03/2004 [END]

*-- Call form to run debit & credit adjustment program
*B039985,1 MMT 11/28/2005 fix bug of not counting session no.[Start]
oAriaApplication.DoProgram('AWRARDCADJ',"'C',' '",.F.,"AR")
*DO FORM (oAriaApplication.ScreenHome+"\ARDCADJ.SCX") WITH 'C',' '
*B039985,1 MMT 11/28/2005 fix bug of not counting session no.[End]
