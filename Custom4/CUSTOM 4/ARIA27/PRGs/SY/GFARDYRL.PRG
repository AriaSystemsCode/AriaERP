*:************************************************************************
*: Program file  : GFARDYRL.PRG
*: Program desc. : Dyelot Arrange program.
*: For screen    : SYARDYRL.SPR
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: SY
*:      Developer: MAB - Mohamed Atia Badran
*:           Date: 08/12/1998               
*:************************************************************************
*: Calls            lfFillClAr,lfFillDyAr,lfvFabPop,lfvClrPop,lfRelBars
*:                  lfvDyeRel,lfDyeArngd... 
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
*:DUE TO E300955,1 MAB 08/12/1998. 
*
PARAMETERS lcRelFab , lcRelClr , lcTmpPar , llDyeEMode , llDonotRem
llCanDoIt  = .F.
llHaveDyes = .T.

lnCurAlias = SELECT(0)
lcOldDye   = ''

llOpDyeRel = gfOpenFile(gcDataDir+"Dye_Rel","","SH")

llFabOpen = .F.
IF USED('FABRIC')
  SELECT FABRIC
  lcFabOrder = ORDER()
  lcFabKey   = FABRIC + COLOR
  SET ORDER TO FABRIC
ELSE  
  llFabOpen  = gfOpenFile(gcDataDir+"FABRIC","FABRIC","SH")
ENDIF

*-- if programer does not Pass fabric or it is empty.
IF (TYPE('lcRelFab') $ 'UL') OR;
  ((TYPE('lcRelFab') = 'C') AND EMPTY(lcRelFab))
   lcRelFab = ''
   lcRelClr  = lcRelFab

ELSE  && programer Pass specific fabric.
  lcRelFab = PADR(lcRelFab,7)
ENDIF  && end if programer does not Pass fabric or it is empty.

*-- if programer does not Pass color or it is empty , or pass color only.
IF (TYPE('lcRelClr') $ 'UL') OR;
  ((TYPE('lcRelClr') = 'C') AND EMPTY(lcRelClr)) OR ;
  EMPTY(lcRelFab)
   lcRelClr = ''

ELSE   && programer Pass specific color and specific fabric.
  lcRelClr = PADR(lcRelClr,6)
ENDIF  && end if programer does not Pass color or it is empty , or pass color only.

*-- if programer does not Pass specif tmpscope or it is empty.
IF (TYPE('lcTmpPar') $ 'UL') OR;
  ((TYPE('lcTmpPar') = 'C') AND EMPTY(lcTmpPar))
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

* MAB 8/29/1998
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
*E300955,1
FUNCTION lfArDyeRel
PARAMETERS lcRlFabric , lcRlColor , lcTmpScope , llArrMode

*-- Fill Fabric array (Fabric Popup is from this array )
SELECT DISTINCT FABRIC FROM DYE_REL ORDER BY FABRIC ;
  WHERE FABRIC + COLOR + DYELOT = lcRlFabric + lcRlColor AND ;
        cTmpScope = lcTmpScope ;
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
      = gfModalGen('TRM00319B00000','Dialog',;
      			 'fabric ' + ALLTRIM(lcRlFabric))  
    ELSE
      *-- the following message is 
      *-- No dyelots found for ð.
      *--             < Ok >
      = gfModalGen('TRM00319B00000','Dialog',;
      			 'fabric/color ' + ALLTRIM(lcRlFabric) + '/' + ;
      			 ALLTRIM(lcRlColor))  
    ENDIF
  ENDIF
  llHaveDyes = .F.
  RETURN
ENDIF

IF llArrMode
  DEFINE POPUP puSort MARGIN MOVER RELATIVE SCROLL MARK CHR(16)
ELSE
  DEFINE POPUP puSort MARGIN RELATIVE SCROLL MARK CHR(16)
ENDIF  

= lfFillDyAr(1,1,.T.)

STORE 1 TO lnFabPop,lnClrPop
=SEEK(PADR(laFabrics[lnFabPop],7)+PADR(laColors[lnClrPop],6),'FABRIC')
lcDescrip = FABRIC.DESC
DO SYArDyRl.SPR
RELEASE POPUPS puSort
*-- end of GFARDYRL.PRG code.

*!*************************************************************
*! Name      : lfFillClAr
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/11/1998
*! Purpose   : Fill Color array for specific fabric and only
*!           : for specified scope
*!*************************************************************
*! Called from : GFARDYRL.PRG CODE,lfvFabPop
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : Fabric item position in fabric array.
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfFillClAr(3)
*!*************************************************************
*E300955,1
FUNCTION lfFillClAr
PARAMETERS lnFabItem

SELECT DISTINCT COLOR FROM DYE_REL ORDER BY COLOR ;
  WHERE FABRIC + COLOR + DYELOT = laFabrics[lnFabItem] + lcRlColor AND ;
        cTmpScope = lcTmpScope ;
  INTO ARRAY laColors

*-- end of lfFillClAr.

*!*************************************************************
*! Name      : lfFillDyAr
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/11/1998
*! Purpose   : Fill Dyelots array and evaluate list items for specific
*!           : fabric + color.
*!*************************************************************
*! Called from : GFARDYRL.PRG CODE,lfvFabPop,lfvClrPop
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : Fabric item position in fabric array.
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfFillDyAr(3,5)
*!*************************************************************
*E300955,1
FUNCTION lfFillDyAr
PARAMETERS lnFabItem,lnClrItem,llBars

IF !llBars
  *-- Fill Dyelot array.
  SELECT DYELOT FROM DYE_REL ORDER BY cDye_Seq ;
    WHERE FABRIC + COLOR + DYELOT = ;
          laFabrics[lnFabItem] + laColors[lnClrItem] ;
    INTO ARRAY laDyeRel
ENDIF

IF _TALLY # 0
  *-- Define list items.
  FOR lnI = 1 TO ALEN(laDyeRel,1)
    DEFINE BAR lnI OF puSort PROMPT PADR(laDyeRel[lnI,1],10)
  ENDFOR
  lsSort = PRMBAR('puSort',1)    && Adjust List pointer to be in first item.
ENDIF  
*-- end of lfFillDyAr.

*!*************************************************************
*! Name      : lfvFabPop
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/11/1998
*! Purpose   : Fabric popup validation, if popup pointer is changed 
*!           : it check for old list and if changed save to dye_rel file,
*!           : also refill color popup and default it to first item 
*!           : and also refill list popup.
*!*************************************************************
*! Called from : GFARDYRL Screen [Fabric popup]
*!*************************************************************
*! Calls       : lfvDyeRel,lfFillClAr,lfRelBars,lfFillDyAr,lfRefresh
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfvFabPop()
*!*************************************************************
*E300955,1
FUNCTION lfvFabPop

IF lnFabPop # lcOldDye

  = lfvDyeRel(lcOldDye,lnClrPop)    && Save current fabric + color + dyelot seq.
  lnClrPop = 1
  = lfFillClAr(lnFabPop)              && Fill color popup with its items.
  = lfRelBars()                       && Release Bars.
  = lfFillDyAr(lnFabPop,lnClrPop)     && Fill List with new bars. 
  SHOW GET lnClrPop 
  SHOW GET lsSort

  =SEEK(PADR(laFabrics[lnFabPop],7)+PADR(laColors[lnClrPop],6),'FABRIC')
  lcDescrip = FABRIC.DESC
  = lfRefresh('SYDYEREL')
ENDIF  
*-- end of lfvFabPop.

*!*************************************************************
*! Name      : lfvClrPop
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/11/1998
*! Purpose   : Color popup validation, if popup pointer is changed 
*!           : it check for old list and if changed save to dye_rel file,
*!           : refill List popup.
*!*************************************************************
*! Called from : GFARDYRL Screen [Color popup]
*!*************************************************************
*! Calls       : lfvDyeRel,lfRelBars,lfFillDyAr,lfRefresh
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfvClrPop()
*!*************************************************************
*E300955,1
FUNCTION lfvClrPop

IF lnClrPop # lcOldDye
  = lfvDyeRel(lnFabPop,lcOldDye)    && Save current fabric + color + dyelot seq.
  = lfRelBars()                       && Release Bars.
  = lfFillDyAr(lnFabPop,lnClrPop)     && Fill List with new bars
  SHOW GET lsSort                     

  = SEEK(PADR(laFabrics[lnFabPop],7)+PADR(laColors[lnClrPop],6),'FABRIC')
  lcDescrip = FABRIC.DESC
  = lfRefresh('SYDYEREL')
ENDIF  
*-- end of lfvClrPop.

*!*************************************************************
*! Name      : lfRelBars
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/11/1998
*! Purpose   : Clear previous list items 
*!*************************************************************
*! Called from : lfvFabPop,lfvClrPop
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfRelBars()
*!*************************************************************
*E300955,1
FUNCTION lfRelBars
FOR lnCount = CNTBAR('puSort') TO 1 STEP -1
  RELEASE BAR lnCount OF puSort
ENDFOR  
*-- end of lfRelBars.

*!*************************************************************
*! Name      : lfvDyeRel
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/11/1998
*! Purpose   : Save new sequence if user was changed it.
*!*************************************************************
*! Called from : GFARDYRL Screen [Close button],lfvFabPop,lfvClrPop
*!*************************************************************
*! Calls       : lfDyeArngd
*!*************************************************************
*! Passed Parameters : Position of fabric and color arrays. 
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfvDyeRel(1,2)
*!*************************************************************
*E300955,1
FUNCTION lfvDyeRel
PARAMETERS lnLocFab , lnLocClr
*-- if user change dyelot items position, save new position.
IF llArrMode AND lfDyeArngd()
  
  PRIVATE lcDyelot
  SELECT DYE_REL
  SET ORDER TO DYE_REL
  *-- Loop around all items to save new sequence.
  FOR lnCount = 1 TO CNTBAR('puSort')
    lnBar    = GETBAR('puSort',lnCount)
    lcDyelot = PADR(ALLTRIM(PRMBAR('puSort',lnBar)),10)
  
    *= SEEK(PADR(laFabrics[lnLocFab],7) + PADR(laColors[lnLocClr],6) + lcDyelot)
    *REPLACE cDye_Seq WITH PADL(ALLTRIM(STR(lnCount)),4,'0')
    IF SEEK(PADR(laFabrics[lnLocFab],7) + PADR(laColors[lnLocClr],6) + lcDyelot)
      REPLACE cDye_Seq WITH PADL(ALLTRIM(STR(lnCount)),4,'0')
    ENDIF
  ENDFOR   && end Loop around all items to save new sequence. 
  SET ORDER TO
ENDIF    && end if user change dyelot items position, save new position.
RETURN
*-- end of lfvDyeRel.

*!*************************************************************
*! Name      : lfDyeArngd
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/11/1998
*! Purpose   : Detect if user change position of any list item(s).
*!*************************************************************
*! Called from : lfvDyeRel
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : Logical (.T. if user change items and else vic versa)
*!*************************************************************
*! Example     : = lfDyeArngd()
*!*************************************************************
*E300955,1
FUNCTION lfDyeArngd
PRIVATE llSaveDye
llSaveDye = .F.
FOR lnCount = 1 TO CNTBAR('puSort')
  IF lnCount # (GETBAR('puSort',lnCount))
    llSaveDye = .T.
    EXIT
  ENDIF
ENDFOR  
RETURN llSaveDye
*-- end of lfDyeArngd.

*-- END OF E300955,1 CODE By MAB.

*!*************************************************************
*! Name      : lfwOldDye
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/11/1998
*! Purpose   : Save old value of current variable
*!*************************************************************
*! Called from : Screen When Functions
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfwOldDye()
*!*************************************************************
*E300955,1
FUNCTION lfwOldDye
lcOldDye = EVALUATE(SYS(18))
*-- end of OldDyelot.
