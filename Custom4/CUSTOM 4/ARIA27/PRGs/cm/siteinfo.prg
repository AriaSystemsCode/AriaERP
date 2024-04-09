Parameters lcSiteID,lcCommand
IF TYPE('lcCommand')='C'
  lcCommand = ALLT(lcCommand)
  ERASE (lcCommand)
  RETURN
ENDIF
SET CLASSLIB TO comm ADDI
lcSiteID = UPPER(lcSiteID)
PUBLIC AriaSiteInfo
_SCREEN.CAPTION = "SiteInfo "+lcSiteID
AriaSiteInfo = CREATEOBJECT('SiteInfo',lcSiteID)
RELEASE AriaSiteInfo
*RELEASE CLASSLIB comm

