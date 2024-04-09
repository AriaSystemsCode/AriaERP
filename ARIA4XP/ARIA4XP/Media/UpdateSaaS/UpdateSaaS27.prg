****************************************************************************************************
* Program : updateSaaS 
* Purpose : copy all files in the SYSFILES or SQLDICTIONARY from the shared SaaS folder to the clients' folders
* Dev.    : MAB - Mohamad Badran
* App     : Aria SaaS A27,A4xp
****************************************************************************************************
*- Modification
****************************************************************************************************

 _SCREEN.visible = .F.
 LOCAL lcsetofdefault
 lcsetofdefault = ADDBS(JUSTPATH(SYS(16)))
 SET DEFAULT TO (lcsetofdefault)
 DO updatesaas WITH "A27"
 RETURN .T.
