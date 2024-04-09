*:***************************************************************************
*: Program file  : POSTYPBS
*: Program desc. : Print PO, Contract and Return PO
*: For Report    : POSTYPOF.FRX (C200923)
*: System        : Aria Advantage Series ARIA4XP  
*: Module        : Purchase Order (PO) 
*: Developer     : Mariam Mazhar (MMT)[T20071126.0001]
*:***************************************************************************
IF !USED('CUTPICK')
  gfOpenTable('CUTPICK','CUTPICK')
ENDIF 

IF !USED('NOTEPAD_A')
  gfOpenTable('NOTEPAD','NOTEPAD','Sh','NOTPAD_A')
ENDIF 


*Awd[Start]

IF !USED('ORDHDR')
   =gfOpenTable(oAriaApplication.DataDir+'ORDHDR','ORDHDR','SH')
ENDIF

IF !USED('ORDLINE')
  gfOpenTable('ORDLINE','ORDLINE')
ENDIF 





*:**************************************************************************
*:* Name        : GetCustPo
*:* Developer   : AWD - Ahmed Awaad
*:* Date        : 06/15/2008
*:* Purpose     : Get Customer Po from ordhdr table 
*:***************************************************************************

FUNCTION GetCustPo
PARAMETERS lcparam
LOCAL lcOldAlias

lcOldAlias=SELECT(0)
lcparam = ""
 
SELECT CUTPICK
IF GfSeek('2'+posln.po,'CUTPICK','CUTPICK')    
    IF GfSeek('O'+cutpick.order,'ordhdr','ordhdr')    
      lcparam=ORDHDR.CustPo
    ENDIF 
    SELECT(lcOldAlias)
    RETURN lcparam
  ENDIF
    SELECT(lcOldAlias)
RETURN lcparam







*:**************************************************************************
*:* Name        : GetClrDesc
*:* Developer   : AWD - Ahmed Awaad
*:* Date        : 06/15/2008
*:* Purpose     : Get Color description 
*:***************************************************************************

FUNCTION GetClrDesc
PARAMETERS lcparam
LOCAL lcOldAlias

lcOldAlias=SELECT(0)
lcparam = ""
*LNMAJR =LEN( gfItemMask("PM")) +2
lcparam="-"+gfCodDes(SUBSTR(posln.style, 14,lnColorLen),'COLOR')
SELECT(lcOldAlias)
RETURN lcparam

*Awd[End]