*:************************************************************************
*: Program file  : ICSCLFR.PRG
*: Program desc. : Program To Change The Scale pf the style
*: System        : ARIA BUSINESS SYSTEM
*: Module        : Inventory control Module
*: Developer     : Hend Ghanem (HBG)
*: Date          : 11/05/2000
*: Refer to      : C101979
*:************************************************************************
*:

*-- lcMajor       && Variabel to hold the Style majore
*-- lcMjrTtl      && Variabel to hold the Style majore Titel 
*-- lcMjrPct      && Variabel to hold the Style majore picture
*-- lcOldScale    && Variabel to hold the the scale of the Style
*-- lcScale       && Variabel to hold the the new scale length
*-- lcScalePic    && Variabel to hold the Scale picture
*-- lnScaleLen    && Variabel to hold the Scale length

STORE "" TO lcMajor,lcMjrTtl,lcMjrPct,lcOldScale,lcScale,lcScalePic
STORE 0 TO lnstylewid,lnScaleLen

llBrowse = .T.

*--Declare needed Variabels

lcMjrTtl   = gfItemMask('HM')
lcMjrPct   = gfItemMask('PM')
lnstylewid = LEN(lcMjrPct)

lnScaleLen= 1
lcScalePic='X'


*--Open the needed files
=lfOpenFile()

DO (gcScrDir+gcWinAppl+"\ICSCLFR.SPX")


*!*************************************************************
*! Name      : lfOpenFile
*! Developer : Hend Ghanem (HBG)
*! Date      : 11/05/2000
*! Purpose   : Open files
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfOpenFile()
*!*************************************************************
*
FUNCTION lfOpenFile


=gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
=gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')
=gfOpenFile(gcDataDir+'TMPINVT','Tmpinvts','SH')
=gfOpenFile(gcDataDir+'POSLN','Poslns','SH')
=gfOpenFile(gcDataDir+'ORDLINE','Ordlines','SH')
=gfOpenFile(gcDataDir+'INVLINE','Invlines','SH')
=gfOpenFile(gcDataDir+'CONSINVL','','SH')
=gfOpenFile(gcDataDir+'MDINVNTL','Mdinvntls','SH')
=gfOpenFile(gcDataDir+'PIKLINE','','SH')
=gfOpenFile(gcDataDir+'POSTRAN','','SH')



*-- End Of lfOpenFile

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Hend Ghanem (HBG)
*! Date      : 11/05/2000
*! Purpose   : Valid Function for style field
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvStyle()
*!*************************************************************
*
FUNCTION lfvStyle

IF MDOWN()
  RETURN
ENDIF

IF LASTKEY()=13 AND !EMPTY(lcMajor) AND SEEK(lcMajor,'STYLE') OR LASTKEY()=9
  IF SEEK(lcMajor,'STYLE')
    lcOldScale = IIF(!EMPTY(lcMajor),STYLE.SCALE,'')
    SHOW GETS
  ENDIF  
  RETURN
ENDIF

IF llbrowse OR !EMPTY(lcMajor) .OR. !SEEK(lcMajor,'STYLE') .OR. ATC('?',lcMajor) <> 0
  llbrowse = .F.
  lcMajor = gfStyBrw('M',lcMajor,"",.F.)
ENDIF

*-- If user select a style , get its scale
lcOldScale = IIF(!EMPTY(lcMajor),STYLE.SCALE,'')  
SHOW GET lcOldScale 
SHOW GETS

RETURN

*-- End Of lfvStyle


*!*************************************************************
*! Name      : lfvScale
*! Developer : Hend Ghanem (HBG)
*! Date      : 11/05/2000
*! Purpose   : Valid Function for Scale field
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvScale()
*!*************************************************************
*
FUNCTION lfvScale

IF MDOWN()
  RETURN
ENDIF

IF LASTKEY()=13 AND !EMPTY(lcScale) AND SEEK('S'+lcScale,'SCALE') OR LASTKEY()=9
  RETURN
ENDIF

IF !SEEK('S'+lcScale,'SCALE') OR llBrowse OR ATC('?',lcMajor) <> 0
  =gfScalBrow(@lcScale)
ENDIF

RETURN

SHOW GETS

*-- End Of lfvScale

*!*************************************************************
*! Name      : lfvProceed
*! Developer : Hend Ghanem (HBG)
*! Date      : 11/05/2000
*! Purpose   : Valiv Function for Proceed "Check if we can change 
*!           : the scale or not , and update files"
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvProceed()
*!*************************************************************
*
FUNCTION lfvProceed

*-- lnOldCnt        && Variabel hold sizes number for the old Scale
*-- lnNewCnt        && Variabel hold sizes number for the New Scale
*-- llNotAllow      && Flag to check if we can chang the scale or not

PRIVATE lnOldCnt , lnNewCnt , llNotAllow 


*--IF user didn't select a style
IF EMPTY(lcMajor)
  *--'You have to select a style!'
  *--<OK>
  =gfModalGen('INM00000B42001','DIALOG',.F.,.F.,'You have to select a style!')
  _CUROBJ = OBJNUM(lcMajor)
  RETURN
ENDIF

*--IF user didn't select a new scale
IF EMPTY(lcScale)
  *--'You have to select a new scale!'
  *--<OK>
  =gfModalGen('INM00000B42001','DIALOG',.F.,.F.,'You have to select a new scale!')
  _CUROBJ = OBJNUM(lcScale)
  RETURN
ENDIF

*--IF Old Scale is the same with selected Scale 
IF lcOldScale=lcScale
  *--'Scale codes cannot be the same!'
  *--<OK>
  =gfModalGen('INM00000B42001','DIALOG',.F.,.F.,'Scale codes cannot be the same!')
  STORE "" TO lcScale
  SHOW GETS
  _CUROBJ = OBJNUM(lcScale)
  RETURN
ENDIF

*-- Get the sizes number for the old Scale
=SEEK('S'+lcOldScale,'SCALE')
lnOldCnt = SCALE.Cnt

*-- Get the sizes number for the New Scale
=SEEK('S'+lcScale,'SCALE')
lnNewCnt = SCALE.Cnt

*-- IF Sizes number for old Scale more than Sizes number for new Scale
*-- check if the Qty for the exceeded sizes empty or not , if not empty we can't 
*-- change the scale
IF lnOldCnt > lnNewCnt 
   SELECT STYLE
   llNotAllow = .F.
   =SEEK(lcMajor)

   FOR lnI=lnNewCnt + 1 TO lnOldCnt 
     lcI=STR(lnI,1)
     SCAN WHILE Style = lcMajor
       IF (ORD&lcI+WIP&lcI+ABS(STK&lcI)+ALO&lcI+SHP&lcI+RET&lcI) <> 0
         llNotAllow = .T.
         EXIT
       ENDIF
     ENDSCAN
     IF llNotAllow
       EXIT
     ENDIF  
   ENDFOR

   IF llNotAllow
     *--'You have quantity in some sizes that you want to remove. Cannot proceed!'
     *--<OK>
     =gfModalGen('INM00000B42001','DIALOG',.F.,.F.,'You have quantity in some sizes that you want to remove. Cannot proceed!')
     _CUROBJ = OBJNUM(lcScale)
     RETURN
   ENDIF

ENDIF

*--Updating files

SELECT STYLE
WAIT WINDOW 'Updating style file...' NOWAIT
REPLACE ALL Scale WITH lcScale FOR PADR(style,lnstylewid ) = lcMajor AND Scale=lcOldScale 


SELECT TMPINVT
WAIT WINDOW 'Updating temp. inventory file...' NOWAIT
REPLACE ALL Scale WITH lcScale FOR PADR(style,lnstylewid )+color+cwarecode+DTOS(dpostdate) = lcMajor AND Scale=lcOldScale


SELECT POSLN
WAIT WINDOW 'Updating purchase orders file...' NOWAIT
REPLACE ALL Scale WITH lcScale FOR PADR(style,lnstylewid )+cstytype+po+STR(lineno,6)+trancd = lcMajor AND Scale=lcOldScale

SELECT ORDLINE
WAIT WINDOW 'Updating orders file...' NOWAIT
REPLACE ALL Scale WITH lcScale FOR PADR(style,lnstylewid )+DTOS(complete)+cordtype+order+store+STR(lineno,6) = lcMajor AND Scale=lcOldScale

SELECT INVLINE
WAIT WINDOW 'Updating invoice file...' NOWAIT
REPLACE ALL Scale WITH lcScale FOR PADR(style,lnstylewid )+invoice+STR(lineno,6) = lcMajor AND Scale=lcOldScale

SELECT CONSINVL
WAIT WINDOW 'Updating consol invoice file...' NOWAIT
REPLACE ALL Scale WITH lcScale FOR PADR(style,lnstylewid ) = lcMajor AND Scale=lcOldScale 

SELECT MDINVNTL
WAIT WINDOW 'Updating consol invoice file...' NOWAIT
REPLACE ALL Scale WITH lcScale FOR PADR(style,lnstylewid )+color+dyelot+clocation+cbattype+clkbatch = lcMajor AND STYLE = lcMajor

SELECT PIKLINE
WAIT WINDOW 'Updating consol invoice file...' NOWAIT
REPLACE ALL Scale WITH lcScale FOR PADR(style,lnstylewid ) = lcMajor AND Scale=lcOldScale  

SELECT POSTRAN
WAIT WINDOW 'Updating consol invoice file...' NOWAIT
REPLACE ALL Scale WITH lcScale FOR PADR(style,lnstylewid ) = lcMajor AND Scale=lcOldScale 


*--'All files are updated successfully'
*--<OK>
=gfModalGen('INM00000B42001','DIALOG',.F.,.F.,'All files have been successfully updated')
_CUROBJ = OBJNUM(lcMajor)

*-- Refresh the screen
STORE "" TO lcMajor,lcOldScale,lcScale
SHOW GETS

*-- End Of lfvProceed