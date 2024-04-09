PARAMETERS lcSiteID,lcDirection,llCanUpdate
*ON ERROR DO GFERROR WITH ERROR(),LINE(),SYS(16)
SET RESO OFF
SET CLASSLIB TO comm ADDI
lcSiteID = UPPER(lcSiteID)
PUBLIC AriaLogSite
_screen.caption = "AriaLog "+lcSiteID+' '+lcDirection
AriaLogSite = CREATEOBJECT('AriaLog',lcSiteID,lcDirection,llCanUpdate)
RELEASE AriaLogSite
*RELEASE CLASSLIB comm

FUNCTION  gfError
pARAMETERS lnError,lnLine,lcprog
=MESSAGEBOX('ERROR ='+STR(lnError)+' Line='+STR(lnLine)+' Prog='+lcProg)
QUIT