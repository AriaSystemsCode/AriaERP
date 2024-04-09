*!**************************************************************************
*! Name      : gfBrowPk
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/22/2000
*! Purpose   : Procedure to browse the packs file for Cathy Daniels
*!**************************************************************************
*! Example   : DO gfBrowPk
*!**************************************************************************
*! Parameters passed : lcYearPack (Which year to browse its packs number)
*! This Procedure returns Pack+Year in one string  
*! Ex. '200005' --> Year '2000' , Packs '05'
*!**************************************************************************
*! Return : Var. contains 6 characters (4 char for year & 2 char for pack) 
*!**************************************************************************
*
PARAMETERS lcYearPack 
PRIVATE lcBrWinName, lcBrTtl, lnCurAlias, lcPackOrd, lnWinWidth, lnWinHight,; 
        llEntered , llPackUsed , lcPackFlt

*-- Save the current alias.
lnCurAlias = SELECT()
llPackUsed = .F.              && assume the pack file is aleady used (Open).

IF USED('Packs')
  SELECT Packs
  lcPackOrd = ORDER()         && Save current tag index.
  lcPackFlt = FILTER()        && save current filter.
  SET FILTER TO
ELSE
  llPackUsed = .T.            && Packs file is used in this function only.
  SELECT 0
  USE Packs
ENDIF
SET ORDER TO TAG cYear        && use cYear index in this function

IF TYPE('lcYearPack') <> "C"
  lcYearPack = ""
ENDIF

=SEEK(lcYearPack)
IF EOF('Packs')
  *-- Message <No packs found>
  *-- Buttons <      OK      > 
  =gfModalGen("TRM000000B00000","DIALOG",'','',"No packs found.")
  IF llPackUsed
    USE IN Packs
  ELSE
    SELECT Packs
    SET FILTER TO &lcPackFlt
    SET ORDER TO &lcPackOrd
  ENDIF
  SELECT (lnCurAlias)       && Restore old file we use before calling this function  
  RETURN SPACE(6)
ENDIF

lcBrWinName = gfTempName()
lcBrTtl     = 'Packs'

IF _DOS OR _UNIX
  lnWinWidth = 41
  lnWinHight = 12
ELSE
  lnWinWidth = 44
  lnWinHight = 30
ENDIF  

DEFINE WINDOW (lcBrWinName);
       AT 0, 0 SIZE lnWinHight, lnWinWidth;
       FONT "MS Sans Serif", 9 ;
       FLOAT ;
       NOCLOSE ;
       SHADOW ;
       NOMINIMIZE ;
       SYSTEM ;
       COLOR SCHEME 10  	   
MOVE WINDOW (lcBrWinName) CENTER
llEntered = .F.
PUSH KEY
ON KEY
ON KEY LABEL ENTER DO lpSelOnEnt WITH lcBrTtl, llEntered

BROWSE FIELDS cYear      :H='Year',;
              cPack      :H='Pack',;
              dSale_Date :H='Sale',;
              dProd_Date :H='Production';
       FOR cYear+cPack = lcYearPack;
       WINDOW (lcBrWinName);
       LOCK 0;
       NOMENU;         
       NOAPPEND;
       NOEDIT;
       NODELETE;
       TITLE lcBrTtl
RELEASE WINDOW (lcBrWinName)
lcYearPack = IIF(llEntered, cYear+cPack, SPACE(6))

IF llPackUsed
  USE IN Packs
ELSE
  SELECT Packs
  SET FILTER TO &lcPackFlt
  SET ORDER TO &lcPackOrd
ENDIF

SELECT (lnCurAlias)       && Restore old file we use before calling this function
POP KEY
RETURN (lcYearPack)
*-- End of gfBrowPack.
