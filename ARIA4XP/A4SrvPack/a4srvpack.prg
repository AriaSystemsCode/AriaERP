PROCEDURE a4srvpack
*E303552,1 MMT 02/16/2015 Check for new builds for Non-SAAS clients[Start]
PARAMETERS lAutoBuild
*E303552,1 MMT 02/16/2015 Check for new builds for Non-SAAS clients[End]
  ON SHUTDOWN DO procEnd
*!*	  _SCREEN.CAPTION='Aria 4XP Service Pack'

*!*	  _SCREEN.MAXHEIGHT=390
*!*	  _SCREEN.HEIGHT=_SCREEN.MAXHEIGHT
*!*	  _SCREEN.MINHEIGHT =_SCREEN.MAXHEIGHT

*!*	  _SCREEN.MAXWIDTH=292
*!*	  _SCREEN.WIDTH =_SCREEN.MAXWIDTH
*!*	  _SCREEN.MINWIDTH =_SCREEN.MAXWIDTH

*!*	  _SCREEN.AUTOCENTER= .T.
*!*	  _SCREEN.MAXBUTTON= .F.

*!*	  _SCREEN.ICON='Aria.ico'

*!*	  SET SYSMENU OFF
 
  _Screen.Visible = .F.
  *E303552,1 MMT 02/16/2015 Check for new builds for Non-SAAS clients[Start]
  *DO FORM a4srvpack
  DO FORM a4srvpack WITH IIF(TYPE('lAutoBuild')='C',IIF(lAutoBuild ='.T',.T.,.F.),.F.)
  *E303552,1 MMT 02/16/2015 Check for new builds for Non-SAAS clients[End]
  READ EVENTS
  RETURN
