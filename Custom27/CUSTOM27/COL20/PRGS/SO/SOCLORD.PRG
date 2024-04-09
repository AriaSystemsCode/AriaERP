*:**************************************************************************
*: Program file  : ICCLGPSH.PRG
*: Program desc. : Custom "Sales Order Data" Program FOR COL20
*: Date          : 03/09/2004
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER MODULE (SO2)
*: Developer     : TMI - TAREK MOHAMED IBRAHIM
*: Purpose       : To pull the data from Aria27 master files and prepare a DBF file for our customer. 
*:               : Our customer will import the data into Excel via ODBC and prepare a report that 
*:               : will be used as a customer order form.
*: Entry #       : C # *C037420,1
*:**************************************************************************

Private lnClrPos,lnClrLen 
STORE 0 TO lnClrPos,lnClrLen
lcMjrPct   = gfItemMask('PM')
lnstylewid = LEN(lcMjrPct)

=lfGtClr()

*--Calling the option grid

lcExpr = gfOpGrid('SOCLORD' , .T.)

IF EMPTY(lcExpr)
  RETURN
ENDIF
*-Get the output target file
lcOutputFl = ''
lcTmpFl = gfTempName()
llRetry = .T.
*C037420,3  TMI [Start] Ask for Overwrite/Append/cancel
lnResp = 0
*C037420,3  TMI [End  ] 
DO WHILE llRetry
  lcOutputFl = GETFILE('DBF')
  IF EMPTY(lcOutputFl)
    RETURN
  ENDIF
  llRetry = .F.
  IF FILE(lcOutputFl)
    *C037420,3  TMI [Start] Ask to append or overwrite target file
    *IF gfModalGen('INM00000B00006',.F.,.F.,.F.,'Overwrite existing file:'+lcOutputFl) = 1
    lnResp = gfModalGen('INM00000B00010',.F.,.F.,.F.,'The target file:'+lcOutputFl+' already exists.')
    IF INLIST(lnResp,1,2)
       *-\<Overwrite;\!\<Append;\?\<Cancel
      *C037420,3  TMI [End  ] 
      
      *C037420,3  TMI [Start] Try to open the file exclusivly
      *lnHndl = FOPEN(lcOutputFl)
      *IF lnHndl>0
      *  =FCLOSE(lnHndl)
      *ELSE
        *IF gfModalGen('INM00000B00006',.F.,.F.,.F.,'File:'+lcOutputFl+' is being opened by another user. Retry?') = 1
      llErr = .F.
      lcSvErr = ON('ERROR')
      ON ERROR llErr = .T.
      SELECT 0
      USE (lcOutputFl) EXCLUSIVE
      USE
      ON ERROR &lcSvErr
      IF llErr
        IF gfModalGen('INM00000B00006',.F.,.F.,.F.,'File:'+lcOutputFl+' is being opened by another user or it is not a dbf file. Retry?') = 1
          *C037420,3  TMI [End  ] 
          llRetry = .T.
          *C037420,3  TMI [Start] if selects not to retry then exit
        ELSE
          lnResp = 3
          *C037420,3  TMI [End  ] 
        ENDIF
      ENDIF
    ELSE
      llRetry = .T.
    ENDIF    
  ENDIF
ENDDO

*C037420,3  TMI [Start] If cancel is pressed then exit
IF lnResp = 3
  RETURN
ENDIF
*C037420,3  TMI [End  ] 

=lfCrtOutpt()

*-Open needed files
=lfOpenFls()

*-Get selected styles
=lfGtStyles()

*:**************************************************************************
*:* Name        : lfCrtOutpt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/09/2004
*:* Purpose     : Create the output target file
*:***************************************************************************
FUNCTION lfCrtOutpt
PRIVATE lnI,laFileStru
lnI = 0 

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'STYLE'      
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 12
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'COLOR'      
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'DESCRIPTN'  
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 30
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PRDLINE'    
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 25
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'GRPCODE'    
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 25
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PRICE'      
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 2

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'MSRP'       
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 2

CREATE TABLE (gcWorkDir+lcTmpFl) FROM ARRAY laFileStru
INDEX ON PRDLINE+GRPCODE+STYLE+COLOR TAG &lcTmpFl

*-- end of lfCrtOutpt.

*:**************************************************************************
*:* Name        : OpenFls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/09/2004
*:* Purpose     : Open needed files
*:***************************************************************************
FUNCTION lfOpenFls
=gfOpenFile(gcDataDir+'STYLE','STYLE','SH')
=gfOpenFile(gcDataDir+'CODES','CODES','SH')

*-- end of lfOpenFls.

*:**************************************************************************
*:* Name        : lfGtStyles
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/09/2004
*:* Purpose     : Collect styles according to selected options
*:***************************************************************************
FUNCTION lfGtStyles

*- I will colltect styles in a temp file indexed with the desired order
*  then copying to the needed output file with the records already sorted.

SELECT STYLE
SCAN FOR &lcExpr
  SELECT (lcTmpFl)
  APPEND BLANK
  REPLACE STYLE      WITH SUBSTR(STYLE.STYLE,1,lnstylewid) ;
          COLOR      WITH SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen) ;
          DESCRIPTN  WITH STYLE.DESC1 ;
          PRDLINE    WITH gfCodDes(STYLE.CPURCODE,'CPURCODE') ;
          GRPCODE    WITH gfCodDes(STYLE.CSTYGROUP,'CSTYGROUP') ;
          PRICE      WITH STYLE.PRICEA ;
          MSRP       WITH STYLE.NSUGRETPRI

ENDSCAN

*- Copy to the output file
SELECT &lcTmpFl
LOCATE
IF !EOF()
  lcError = ON('ERROR')
  llError = .F.
  ON ERROR llError = .T.
  *C037420,3  TMI [Start] Overwrite or create new file
  IF INLIST(lnResp , 0 , 1)
    *C037420,3  TMI [End  ] 
    COPY TO (lcOutputFl)
    lcAction = 'created'
    *C037420,3  TMI [Start] && append case
  ELSE 
    SELECT 0
    USE (lcOutputFl)
    APPEND FROM (DBF(lcTmpFl))
    USE 
    lcAction = 'appended'
  ENDIF
  *C037420,3  TMI [End  ] 
  ON ERROR &lcError

  IF !llError
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'The file:'+lcOutputFl+' was &lcAction. successfully.')
  ELSE
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'The file:'+lcOutputFl+' can not be created.')
  ENDIF  
ELSE
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No records match the selected criteria.')
ENDIF  

* Erase the temp created file
USE IN &lcTmpFl
ERASE (gcWorkDir+lcTmpFl+'.DBF')
ERASE (gcWorkDir+lcTmpFl+'.CDX')

*-- end of lfGtStyles.

*:**************************************************************************
*:* Name        : lfGtClr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/09/2004
*:* Purpose     : Get color data
*:***************************************************************************
FUNCTION lfGtClr
PRIVATE laItemSeg
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
*-- End of lfGtClr.


*:**************************************************************************
*:* Name        : lfvStyle
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/09/2004
*:* Purpose     : Valid fn. for style
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfvStyle

lcStyle = VARREAD()
IF EMPTY(&lcStyle.)
  RETURN
ENDIF
  
lcTag = ORDER('STYLE')

SET ORDER TO cStyle IN STYLE

IF INLIST(LASTKEY(),9,13) AND !MDOWN()
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
ELSE
  &lcStyle = ''
ENDIF

SET ORDER TO lcTag IN STYLE
