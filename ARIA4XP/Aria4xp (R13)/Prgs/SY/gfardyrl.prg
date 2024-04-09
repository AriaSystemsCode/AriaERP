*:************************************************************************
*: Program file  : GFARDYRL.PRG
*: Program desc. : Dyelot Arrange program.
*: For screen    : SYARDYRL.SPR
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: SY
*:      Developer: MAB - Mohamed Atia Badran
*:           Date: 08/12/1998
*:************************************************************************
*: Calls            lfFillClAr,lfFillDyAr
*:
*:************************************************************************
*: Passed Parameters
*:                  lcRelFab    => Specific Fabric , all if empty.
*:                  lcRelClr    => Specific Color  , all if empty.
*:                  lcTmpPar    => Temporary scope , all if empty.
*:                  llDyeEMode  => Flag control arrange screen mode.
*:                  llDonotRem  => Flag control Clear Temp. Name from File.
*:************************************************************************
*: Example          = GFARDYRL('SILK   ','BLUE  ','')
*:************************************************************************
*: Notes            You can pass the following shapes
*:                  1- Fabric only (For specific fabric) --
*:                     = gfArDyRl(lcFabric)
*:                  2- Fabric and Color (For specific fabric color)
*:                     = gfArDyRl(lcFabric,lcColor)
*:                  3- Scope only (for your screen range).
*:                     = gfArDyRl(,,lcScope)
*:                     gpAdFabWar marks DYE_REL file with a temp name.
*:                     This temp name is used as a scope so that
*:                     only fabric/colors whose dyelots were entered
*:                     in a session are presendted in the screen
*:                     for arrangement.
*:                  4- Nothing (for All Fabrics, colors , dyelots).
*:                     = gfArDyRl()
*:                  5- You can also add scope to all.
*:************************************************************************
*FUNCTION gfArDyRl
PARAMETERS lcRelFab , lcRelClr , lcTmpPar , llDyeEMode , llDonotRem
llCanDoIt  = .F.
llHaveDyes = .T.

lnCurAlias = SELECT(0)
lcOldDye   = ''

llOpDyeRel = gfOpenFile(oAriaApplication.DataDir+"Dye_Rel","","SH")

llFabOpen = .F.
IF USED('FABRIC')
  SELECT FABRIC
  lcFabOrder = ORDER()
  lcFabKey   = FABRIC + COLOR
  SET ORDER TO FABRIC
ELSE
  llFabOpen  = gfOpenFile(oAriaApplication.DataDir+"FABRIC","FABRIC","SH")
ENDIF

*-- if programer does not Pass fabric or it is empty.
IF (TYPE('lcRelFab') $ 'UL') OR ((TYPE('lcRelFab') = 'C') AND EMPTY(lcRelFab))
   lcRelFab = ''
   lcRelClr  = lcRelFab

ELSE  && programer Pass specific fabric.
  lcRelFab = PADR(lcRelFab,7)
ENDIF  && end if programer does not Pass fabric or it is empty.

*-- if programer does not Pass color or it is empty , or pass color only.
IF (TYPE('lcRelClr') $ 'UL') OR ((TYPE('lcRelClr') = 'C') AND EMPTY(lcRelClr)) OR ;
  EMPTY(lcRelFab)
   lcRelClr = ''

ELSE   && programer Pass specific color and specific fabric.
  lcRelClr = PADR(lcRelClr,6)
ENDIF  && end if programer does not Pass color or it is empty , or pass color only.

*-- if programer does not Pass specif tmpscope or it is empty.
IF (TYPE('lcTmpPar') $ 'UL') OR ((TYPE('lcTmpPar') = 'C') AND EMPTY(lcTmpPar))
  lcTmpPar = ''
ENDIF  && end if programer does not Pass specif tmpscope or it is empty.

SELECT DYE_REL
*-- if no scope passed to the function.
IF EMPTY(lcTmpPar)
  llCanDoIt  = .T.
ELSE  && else If a scope has been passed, check for new entries in DYE_REL marked by it.
  SET ORDER TO SCOPE
  *-- if you found dyelots added at this session
  IF SEEK(lcTmpPar)
    *-- the following message is
    *-- Do you want to arrange dyelots.
    *--        < Yes >  < No >
    *-- if the user press < Yes > he/she continue arrange dyelots, and vic versa.
    llCanDoIt = gfModalGen('QRM36116B00006','Dialog', '') = 1
  ELSE
    llHaveDyes = .F.
  ENDIF
ENDIF

IF llCanDoIt
  SET ORDER TO
  = lfArDyeRel(lcRelFab , lcRelClr , lcTmpPar , llDyeEMode)
ENDIF

* if programmer choice is to clear temp. name (Default)
IF !llDonotRem AND !EMPTY(lcTmpPar)
  SELECT DYE_REL
  REPLACE cTmpScope WITH SPACE(10) FOR cTmpScope = lcTmpPar
ENDIF  && end if programmer choice is to clear temp. name (Default) .

IF llOpDyeRel
  USE IN Dye_Rel
ENDIF

IF llFabOpen
  USE IN FABRIC
ELSE
  SELECT FABRIC
  = SEEK(lcFabKey)
  SET ORDER TO &lcFabOrder
ENDIF
SELECT (lnCurAlias)
RETURN llHaveDyes
*-- END OF program code.


*!*************************************************************
*! Name      : lfArDyeRel
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/11/1998
*! Purpose   : Fill Color array for specific fabric and only
*!           : for specified scope
*!*************************************************************
*! Called from : GFARDYRL.PRG CODE
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : Same parameters of main program
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfArDyeRel()
*!*************************************************************
FUNCTION lfArDyeRel
PARAMETERS lcRlFabric , lcRlColor , lcTmpScope , llArrMode

*-- Fill Fabric array (Fabric Popup is from this array )
SELECT DISTINCT FABRIC FROM DYE_REL ORDER BY FABRIC ;
          WHERE FABRIC + COLOR + DYELOT = lcRlFabric + lcRlColor AND cTmpScope = lcTmpScope ;
     INTO ARRAY laFabrics

IF _TALLY # 0
  DIMENSION laColors[1]
  = lfFillClAr(1)
ENDIF
IF _TALLY # 0
  *-- Fill Dyelot array.
  SELECT DYELOT FROM DYE_REL ORDER BY cDye_Seq ;
    WHERE FABRIC + COLOR + DYELOT = ;
          laFabrics[1] + laColors[1] ;
    INTO ARRAY laDyeRel
ENDIF

*-- If no scope has been passed to the function, and no dyelots found check fabric/colors
IF _TALLY = 0
  *-- If no fabric has been passed to the function, (i.e. all fabric/colors)
  IF EMPTY(lcRlFabric)
    *-- the following message is
    *-- No dyelots found for ð.
    *--                < Ok >
    = gfModalGen('TRM00319B00000','Dialog', 'any fabric/color')
  ELSE
    *-- If a fabric has been entered, check for colors\dyelots
    IF EMPTY(lcRlColor)
      *-- the following message is
      *-- No dyelots found for ð.
      *--             < Ok >
      = gfModalGen('TRM00319B00000','Dialog','fabric ' + ALLTRIM(lcRlFabric))
    ELSE
      *-- the following message is
      *-- No dyelots found for ð.
      *--             < Ok >
      = gfModalGen('TRM00319B00000','Dialog','fabric/color ' + ALLTRIM(lcRlFabric) + '/' +ALLTRIM(lcRlColor))
    ENDIF
  ENDIF
  llHaveDyes = .F.
  RETURN
ENDIF


= lfFillDyAr(1,1,.T.)

STORE 1 TO lnFabPop,lnClrPop
=SEEK(PADR(laFabrics[lnFabPop],7)+PADR(laColors[lnClrPop],6),'FABRIC')
lcDescrip = FABRIC.Desc

DO FORM (oAriaApplication.ScreenHome+"\SYArDyRl.scx") WITH llArrMode

*-- end of GFARDYRL.PRG code.

*!*************************************************************
*! Name      : lfFillClAr
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/11/1998
*! Purpose   : Fill Color array for specific fabric and only
*!           : for specified scope
*!*************************************************************
*! Called from : GFARDYRL.PRG CODE,
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : Fabric item position in fabric array.
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfFillClAr(3)
*!*************************************************************
FUNCTION lfFillClAr
PARAMETERS lnFabItem

SELECT DISTINCT COLOR FROM DYE_REL ORDER BY COLOR ;
  WHERE FABRIC + COLOR + DYELOT = laFabrics[lnFabItem] + lcRlColor AND cTmpScope = lcTmpScope ;
   INTO ARRAY laColors
*-- end of lfFillClAr.

*!*************************************************************
*! Name      : lfFillDyAr
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/11/1998
*! Purpose   : Fill Dyelots array and evaluate list items for specific
*!           : fabric + color.
*!*************************************************************
*! Called from : GFARDYRL.PRG CODE,,lfvClrPop
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : Fabric item position in fabric array.
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfFillDyAr(3,5)
*!*************************************************************

FUNCTION lfFillDyAr
PARAMETERS lnFabItem,lnClrItem,llBars

IF !llBars
  *-- Fill Dyelot array.
  SELECT DYELOT FROM DYE_REL ORDER BY cDye_Seq WHERE FABRIC + COLOR + DYELOT = ;
          laFabrics[lnFabItem] + laColors[lnClrItem] INTO ARRAY laDyeRel
ENDIF
*-- end of lfFillDyAr.


