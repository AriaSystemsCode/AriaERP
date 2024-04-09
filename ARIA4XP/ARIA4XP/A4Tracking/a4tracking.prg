PROCEDURE a4tracking
PUBLIC lcSaveDirectory, lcTracking

*R:\a4tracking\fixes\"
lcSaveDirectory="T:\A4Tracking\fixes\"  
lcTracking="T:\A4Tracking\"


SET DEFAULT TO (lcTracking)

DO A4TRACKING.MPR
RETURN

*!*	PROCEDURE a4tracking
*!*	*!*	CLEAR ALL
*!*	*!*	CLEAR
*!*	*PUSH  MENU _MSYSMENU
*!*	SET DEFAULT TO "r:\a4tracking"
*!*	DO A4TRACKING.MPR
*!*	*!*	ON SHUTDOWN clear all
*!*	*!*	DO FORM frmtrack
*!*	RETURN
*!*	*SET SYSMENU TO DEFAULT
*!*	*CLEAR ALL
*!*	*CLEAR
