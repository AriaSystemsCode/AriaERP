*:****************************************************************************
*: Program file        : SOFALXLS.PRG
*: Program description : EXCEL Orders Upload sessions reports
*: Aria customer       : Falcon Bay Sportswear  ( FAL03 )
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : System Manager - (SO2)
*: Developer Name      : TMI TAREK MOHAMED IBRAHIM
*: Tracking Job Number : *C102818,1
*: Date developed      : 08/14/2003
*:****************************************************************************

*- Collecting data
=llOgFltCh .AND. lfCollect()

*- Show report
SELECT (lcWorkFile)
GO TOP
lcTime = TIME()
DO gfDispRe WITH EVAL('lcRpForm')

*- Close opened files
*=lfClosFils()

*:**************************************************************************
*:* Name        : lfwRpWhen
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/14/2003
*:* Purpose     : When fn. for the report sofalxls
*:***************************************************************************
FUNCTION lfwRpWhen
PRIVATE lnI,laStru

IF lnOgSeting = 1
  *- Remove "All" with "Both" in the status popup 
  FOR lnI = 1 TO ALEN(laOgPopAry,1)
    IF laOgPopAry[lnI,1] = 'All'
      laOgPopAry[lnI,1] = 'Both'
      EXIT
    ENDIF
  ENDFOR

  *- Fill the file lcXlsFils with all imported excel files
  SELECT XLIMPSES
  DIMENSION laStru[2,4]
  
  laStru[1,1] = 'CXLSFILE'
  laStru[1,2] = 'C'
  laStru[1,3] = 8
  laStru[1,4] = 0
  
  laStru[2,1] = 'CSTATUS'
  laStru[2,2] = 'C'
  laStru[2,3] = 1
  laStru[2,4] = 0
  
  CREATE DBF (gcWorkDir+lcXlsFils) FROM ARRAY laStru
  INDEX ON CXLSFILE TAG &lcXlsFils
  
  SELECT XLIMPSES
  SCAN
    IF !SEEK(XLIMPSES.CXLSFILE,lcXlsFils)
      m.CXLSFILE = XLIMPSES.CXLSFILE
      m.CSTATUS  = XLIMPSES.CSTATUS
      INSERT INTO &lcXlsFils FROM MEMVAR
    ENDIF
  ENDSCAN

ENDIF
*-- end of lfwRpWhen.

*:**************************************************************************
*:* Name        : lfCollect
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/14/2003
*:* Purpose     : Collect data from xlimpses file 
*:***************************************************************************
FUNCTION lfCollect
*- Selected csv file position in the laOgFxflt array
lnXlsPos = ASUBSCRIPT( laOgFxFlt , ASCAN( laOgFxFlt , 'XLIMPSES.CXLSFILE' ) , 1 )

SELECT XLIMPSES
IF !USED(lcWorkFile)
  COPY STRUCTURE TO (gcWorkDir+lcWorkFile) 
  USE (gcWorkDir+lcWorkFile) EXCL IN 0
ENDIF
SELECT &lcWorkFile  
ZAP
*C102818,4  TMI [Start] Index work file on Session Id + status
INDEX ON CIMPSESSID+CSTATUS+ORDER+CXLSFILE TAG &lcWorkFile
*C102818,4  TMI [End  ] 

SELECT XLIMPSES
GO TOP
=!EMPTY(laOgFxFlt[lnXlsPos,6]) .AND. SEEK(laOgFxFlt[lnXlsPos,6],'XLIMPSES')
SCAN FOR &lcRpExp
  SCATTER MEMVAR MEMO
  INSERT INTO &lcWorkFile FROM MEMVAR  
ENDSCAN
*-- end of lfCollect.

*:**************************************************************************
*:* Name        : lfvExlFil
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/14/2003
*:* Purpose     : Valid funtion for selecting the Excel file
*:***************************************************************************
FUNCTION lfvExlFil
PRIVATE lnSlct,laData,lcTitle
lnSlct = SELECT()

*- Selected csv file position in the laOgFxflt array
lnXlsPos = ASUBSCRIPT( laOgFxFlt , ASCAN( laOgFxFlt , 'XLIMPSES.CXLSFILE' ) , 1 )

DIMENSION laData[1]
laData[1] = ''
lcTitle = 'Select an Imported Excel file'

lcBrFields = [CXLSFILE  :H = 'Imported Excel File' :20 ,]  + ;
             [CSTATUS   :H = 'Status']

IF !EMPTY(laOgFxFlt[lnXlsPos,6]) .AND. !SEEK(laOgFxFlt[lnXlsPos,6],lcXlsFils)
  SELECT &lcXlsFils
  llWasSel  =ARIABROW('',lcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','',"CXLSFILE","laData")
  laOgFxFlt[lnXlsPos,6] =IIF(llWasSel,laData[1],SPACE(8))
ENDIF

SELECT (lnSlct)
*-- end of lfvExlFil.



*:**************************************************************************
*:* Name        : lfClosFils
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/14/2003
*:* Purpose     : Close open temp files
*:***************************************************************************
FUNCTION lfClosFils
IF USED(lcWorkFile)
  USE IN (lcWorkFile)
ENDIF
ERASE (gcWorkDir+lcWorkFile+'.DBF')
ERASE (gcWorkDir+lcWorkFile+'.CDX')
ERASE (gcWorkDir+lcWorkFile+'.FPT')

IF USED(lcXlsFils)
  USE IN (lcXlsFils)
ENDIF  
ERASE (gcWorkDir+lcXlsFils+'.DBF')
ERASE (gcWorkDir+lcXlsFils+'.CDX')
ERASE (gcWorkDir+lcXlsFils+'.FPT')

*-- end of lfClosFils.


