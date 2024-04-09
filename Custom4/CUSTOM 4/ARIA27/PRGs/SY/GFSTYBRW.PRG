*:*************************************************************************
*: Program file  : GFSTYBRW.PRG
*: Program desc. :
*: For Screen    :  
*:        System : SY
*:        Module : SY
*:     Developer : Hesham El-Sheltawi 
*:*************************************************************************
*: Passed parameters  : llcanadd
*:*************************************************************************
*E300703,1 Hesham Style Global Browse
*: Modifications
*: E301036,1 HDM 10/07/98 Change style browse to be like regular browse
*: B602277,1 HDM 11/29/98 Make Style Info. Variables Private
*: E301157,1 Hesham 03/03/99
*: E301157,1 Make the fuction perform a softseek and return the style
*: E301157,1 if there is only one non major record
*: E300789,1 Hesham 03/04/99
*: E300789,1 remove field ccomp_id from files ACCOD,FISHD,FSPRD,FSHLD,CODES
*: E300789,1 Change Any seeking in these files
*: B602656,1 HDM 03/08/1999 Don't return just make the program to return itself
*: B802065,1 MAN 03/22/1999 The Reported problem is that shipping Log report 
*: B802065,1 MAN            Display part of the style major as Non Major ... 
*: B802065,1 MAN            The reason is that 2 variables are not private in the
*: B802065,1 MAN            StyBrowse therfore it afects the reprots variables 
*: B602691,1 HDM 03/25/1999 Force empty lcMajor case no records matches the user input
*: B602703,1 HDM 03/25/1999 We Have To Check llBrowse Value we may force browsing
*: B602700,1 HDM 03/31/1999 Enable soft seek while browsing Major
*: B602928,1 Hesham 05/24/1999
*: B602928,1 Clear the keyboard buffer to permit the key traping from
*: B602928,1 firing before the window display
*! B603012,1 ADEL 11/25/01 Check if the privilages of the users enabled them to Add or not.
*! E301853,1 WAB 04/01/2002 - add the color description to the browse fields
*:*************************************************************************
PARAMETERS lcMajorOrNon,lcMajor,lcNonMajor,llCanAdd


*B802065,1 MAN 03/22/1999 Added lnNonMajStart,lnNonMajEnd to the private vars
PRIVATE laOpenFiles,laSeg,lafoldwinds,laSegSz,laSegVal,laAcDes,;
        lcfolder,lcReturn,laSeg,laAcDes,lcMask,lcSegVal,lcHeader,lcBrTitl1,;
        lcBrTitl2,lcWith,lcEmptyErr,lnAlias,lnStyRec,lnSegRec,lcBtMpCan,lcBtMpSel,;
        IBFOLDER,lnActFolder,lcPrnWin,lcWinChP,lcWinCh1,;
        lcWinCh2,lcWinCh3,lcWinCh4,lcfoldprnt,lcwfoldchng,lnFolderCEnd,;
        lnfolderrend,lcMajorOrNon,lcMajor,lcNonMajor,llCanAdd,lnNoSeg,;
        lnPosition,lnMajorStart,lnMajorEnd,lcBarDesc,lcBarCode,;
        lcBarType,lcUsedType,llStructUse,llStyleUse,lcStyleTag,llStyleData,;
        llCanCont,lnCount,lnStartSeg,lnEndSeg,lnActSeg,lcExpr,lcAlias,lnLastFold,;
        lnStyRec,lnNonMajStart,lnNonMajEnd

*E301853,1 WAB (Start)
PRIVATE   llUseColor,lnClrStrt,lnClrWidth 
llUseColor = .F.
lcNMjrTl  = gfItemMask('HN')
*E301853,1 WAB (END)

PRIVATE ALL LIKE l?FOLD*
PRIVATE ALL LIKE IBFOLD*

*llCanAdd = .t.
*lcMajorOrNon='M'
*lcNonMajor = SPACE(11)
*lcMajor = 'TAK2 -A '
*lcNonMajor = gfItemMask('PN')

DECLARE laOpenFiles[1,1],laSeg[9,9],lafoldwinds[2,2],laSegSz[9,1],laSegVal[9],;
        laAcDes[9],laSegInfo[1,9]
STORE '' TO lcfolder,lcReturn,laSeg,laAcDes,lcMask,lcSegVal,lcHeader,lcBrTitl1,;
            lcBrTitl2,lcWith,lcEmptyErr
STORE 0 TO laSegVal,lnAlias,lnStyRec,lnSegRec

*-- B602223,1 HDM [Start] Store '' to lcStyleTag case the style file not opened
STORE '' TO lcStyleTag
*-- B602223,1 HDM [End]

lnAlias = SELECT()
lcBtMpCan = gcBMPHome + "CAN.BMP"
lcBtMpSel = gcBMPHome + "SEL.BMP"

lcPrnWin = gfTempName()
lcWinChP = gfTempName()
lcWinCh1 = gfTempName()
lcWinCh2 = gfTempName()
lcWinCh3 = gfTempName()
lcWinCh4 = gfTempName()

lafoldwinds[1,1] = 'General'
lafoldwinds[1,2] = lcWinCh1
lafoldwinds[2,1] = 'By Segment'
lafoldwinds[2,2] = lcWinCh2
lcBrTitl1 = 'Style'
lcWith = lcPrnWin+','+lcBrTitl1+',Browse,Scale,cDivision,Domestic,Quality,Other,Free,Group'


lnActFolder = 1                 && active folder

lcfolder    = gfTempName()      && Folder Window Name
lcfoldprnt  = lcPrnWin          && window parent name for the folder
lcwfoldchng = '=lfActFolder()'  && function to control shows after change the folder

lnFolderCEnd= 93.00            && Folder window end column
lnfolderrend=   2.00            && Folder window end row

lcDash = '-'

FOR lnCount = 1 TO 9
  laSeg[lnCount,1] = 0
ENDFOR
llStructUse = .F.
IF !USED('ICISTRU')
  USE (gcDataDir+'ICISTRU') IN 0
  llStructUse = .T.
ENDIF
SELECT ICISTRU
SET ORDER TO TAG SEGNO
*GO TOP
=SEEK('U1')
lnNoSeg = 0
lnPosition = 1
lnMajorStart = 1
lnMajorEnd = 0
lcBarDesc = 'Browse  DivisionSeason  Scale   DomesticQuality Group   Other   Free    '
lcBarCode = 'CDZSTQGOF'
lcBarType = 'CCCSVVCOF'
lcUsedType =''
SCAN WHILE citemrecty+cisegno = 'U'
  lnNoSeg = lnNoSeg + 1
  laSeg[lnNoSeg,1]   = nisegsize
  laSeg[lnNoSeg,2]   = ALLT(cisegtype)
  laSeg[lnNoSeg,3]   = lnPosition
  *E301853,1 WAB (Start) - check if the style code structre contains color
  IF laSeg[lnNoSeg,2] = 'C'
    llUseColor = .T.
    lnClrStrt  = laSeg[lnNoSeg,3]
    lnClrWidth = laSeg[lnNoSeg,1] 
  ENDIF
  *E301853,1 WAB (End)
  
  IF laSeg[lnNoSeg,2] $ 'CDGZ'
    laSeg[lnNoSeg,4]   = 'CODES'
*    laSeg[lnNoSeg,5]   = ["]+gcAct_comp+IIF(laSeg[lnNoSeg,2]='C','COLOR',;
                             IIF(laSeg[lnNoSeg,2]='G','CSTYGROUP',;
                             IIF(laSeg[lnNoSeg,2]='Z','SEASON','CDIVISION')));
                             +["]+[ AND CRLTFIELD='N']

    *E300789,1 Hesham (Start)
    *laSeg[lnNoSeg,5]   = ["]+gcAct_comp+PADR(IIF(laSeg[lnNoSeg,2]='C','COLOR',;
                             IIF(laSeg[lnNoSeg,2]='G','CSTYGROUP',;
                             IIF(laSeg[lnNoSeg,2]='Z','SEASON','CDIVISION'))),10)+["]

    laSeg[lnNoSeg,5]   = ["]+'N'+PADR(IIF(laSeg[lnNoSeg,2]='C','COLOR',;
                             IIF(laSeg[lnNoSeg,2]='G','CSTYGROUP',;
                             IIF(laSeg[lnNoSeg,2]='Z','SEASON','CDIVISION'))),10)+["]

    *E300789,1 Hesham (End)
    laSeg[lnNoSeg,6]   = 'CCODE_NO'
    laSeg[lnNoSeg,8] =  [CCODE_NO:H='Code',CDISCREP:H='Description']
    laSeg[lnNoSeg,9] =   [CODES.CCODE_NO]
  ELSE
    laSeg[lnNoSeg,4]   = IIF(laSeg[lnNoSeg,2]='S','Scale','ICSEGVAL')
    laSeg[lnNoSeg,5]   = [']+IIF(laSeg[lnNoSeg,2]='S','S',PADR(lnNoSeg,1))+[']
    laSeg[lnNoSeg,6]   = IIF(laSeg[lnNoSeg,2]='S','SCALE','SEGVAL')
    laSeg[lnNoSeg,8] =  IIF(laSeg[lnNoSeg,2]='S',[Scale,Cscl_desc:H='Description'],;
                        [cisegval:H='Value',cisgvalSd:H='Short Description',cisgvalld:H='Long Description'])
    laSeg[lnNoSeg,9] =   IIF(laSeg[lnNoSeg,2]='S',[SCALE],[CISEGVAL])
  ENDIF
  lcUsedType = lcUsedType +IIF(laSeg[lnNoSeg,4] $ lcUsedType,'',IIF(EMPTY(lcUsedType),'',',')+PADR(laSeg[lnNoSeg,4],10))
  laSeg[lnNoSeg,7] =  SUBSTR(lcBarDesc,(ATC(laSeg[lnNoSeg,2],lcBarCode)-1)*8+1,8)
  laAcDes[lnNoSeg] = SUBSTR(cisegsdes,1,laSeg[lnNoSeg,1])
  lcMask = lcMask+REPL('X',laSeg[lnNoSeg,1])+ALLT(CISEGSEPR)
  lcSegVal = lcSegVal +REPL(' ',laSeg[lnNoSeg,1])+ALLT(CISEGSEPR)
  lnPosition         = lnPosition + laSeg[lnNoSeg,1] + LEN(ALLT(CISEGSEPR))
  lcHeader = IIF(lnNoSeg=1,cIsegHead,lcHeader)
  lnMajorEnd = IIF(lSegEndMaj,lnNoSeg,lnMajorEnd)
ENDSCAN


IF llStructUse AND USED('ICISTRU')
  USE IN ICISTRU
ENDIF

llStyleUse = .F.


lnStyRec = 0
IF !USED('STYLE')

  USE (gcDataDir+'STYLE') IN 0
  llStyleUse = .T.

ELSE

  *--HDM B602223 Pik The Style file Tag First[start]
  lcStyleTag = ORDER('STYLE')
  *--HDM B602223 Pik The Style file Tag First[end]

ENDIF
SELECT STYLE
lnStyRec = RECNO()
SET ORDER TO TAG STYLE
LOCATE
llStyleData = FOUND()
llCanCont = llCanAdd OR FOUND()
lnNonMajStart = lnMajorEnd+1
lnNonMajEnd = lnNoSeg
llValidate = .F.
IF llCanCont
  DIMENSION laUseFile[1,1]
  =gfSubStr(lcUsedType,@laUseFile)
  DIMENSION laOpenFiles[ALEN(laUseFile,1),4]
  STORE '' TO laOpenFiles
  FOR lnCount = 1 TO ALEN(laUseFile,1)
    lcFileName = ALLT(laUseFile[lnCount,1])
    laOpenFiles[lnCount,1] = lcFileName
    laOpenFiles[lnCount,2] = !USED(lcFileName)
    IF laOpenFiles[lnCount,2]
      USE (gcDataDir+laOpenFiles[lnCount,1]) IN 0
    ELSE
      laOpenFiles[lnCount,3] = FILTER(laOpenFiles[lnCount,1])
      laOpenFiles[lnCount,4] = ORDER(laOpenFiles[lnCount,1])
    ENDIF
    SELECT (laOpenFiles[lnCount,1])
    lnFilePos = ASCAN(laSeg,lcFileName)
    lnFilePos = ASUBSC(laSeg,lnFilePos,1)
    SET ORDER TO TAG (laSeg[lnFilePos,6])
    SET FILTER TO
  ENDFOR
**** Hesh    
  SELECT STYLE

  DO CASE
    CASE lcMajorOrNon='M' AND !lfEmpty(lcMajor,'M')  
      DO CASE
        CASE llCanAdd AND !SEEK(lcMajor,'STYLE')
          lnOption = gfModalGen('QRM00001B42003','Dialog',ALLT(gfItemMask('HM'))+": "+ALLTRIM(lcMajor))  
          DO CASE
            CASE  lnOption = 3
              llCanCont = .F.           
              lcReturn = ''
            CASE lnOption = 1
              llCanCont = !(lcMajor = lfvSeg(lcMajor,'M'))
              IF !llCanCont
                lcReturn = lcMajor
              ELSE
                lcSegVal = STUFF(lcSegVal,1,laSeg[lnMajorEnd,3]+laSeg[lnMajorEnd,1]-1,lfvSeg(lcMajor,'M'))
                llStyleData = .F.
              ENDIF
              *B603012,1 (Add) Check if the privilages of the users enabled them to Add or not.
              IF !llAddRec
                =gfModalGen("TRM00088B00000","ALERT",'Adding') 
                llCanCont = .F.           
                lcReturn = ''
              ENDIF  
              *B603012,1 (End)
            OTHERWISE 
              SELECT STYLE
              *E301157,1 Hesham (Start)
              *=SEEK(ALLT(lcMajor)
              lnRecNO = IIF(SEEK(ALLT(lcMajor),'STYLE'),RECNO(),RECNO(0))
              *IF  RECNO(0)>0 .AND. RECNO(0) <= RECCOUNT()
              IF  lnRecNO > 0 
                *GO RECNO(0)
                GO lnRecNO &&RECNO(0)
              *E301157,1 Hesham (End)                
              ELSE
                GO TOP
              ENDIF
              *-- HDM B602700,1 03/31/1999[Start] Enable soft seek while browsing Major
              *lcMajor = ''
              IF lcMajorOrNon $'NI'
                lcMajor = ''
              ENDIF
              *-- HDM B602700,1 03/31/1999[End]
              lcSegVal = STUFF(lcSegVal,1,laSeg[lnMajorEnd,3]+laSeg[lnMajorEnd,1]-1,lcMajor)
          ENDCASE
        CASE !llCanAdd 
            lcMajor = lfvSeg(lcMajor,'M')
            lcSegVal = STUFF(lcSegVal,1,laSeg[lnMajorEnd,3]+laSeg[lnMajorEnd,1]-1,lcMajor)
            llStyleData = .F.
      ENDCASE

    CASE lcMajorOrNon='N' AND lfEmpty(lcMajor,'M')
      lcMajorOrNon = 'I'
    CASE lcMajorOrNon='N' AND lfEmpty(lcNonMajor,'N')
      lcReturn = STRTRAN(gfItemMask('PN'),'X','*')
      llCanCont = .F.
    CASE lcMajorOrNon='N'
      lcSegVal = STUFF(lcSegVal,1,laSeg[lnMajorEnd,3]+laSeg[lnMajorEnd,1]-1,lcMajor)
      lcMask = STUFF(lcMask,1,laSeg[lnMajorEnd,3]+laSeg[lnMajorEnd,1]-1,lcMajor)
      llStyleData = .F.
      llValidate = .T.
    CASE lcMajorOrNon='M'
       lnNoSeg = lnMajorEnd
       lcMask  = SUBSTR(lcMask,1,laSeg[lnMajorEnd,3]+laSeg[lnMajorEnd,1]-1)
       lcHeader = SUBSTR(lcHeader ,1,laSeg[lnMajorEnd,3]+laSeg[lnMajorEnd,1]-1)
    *E301157,1 Hesham (Start)
    *E301157,1 if we are browsing for a complete style and there is only one record
    *E301157,1 then return the style
    CASE lcMajorOrNon = 'I' AND !EMPTY(lcMajor)
         SELECT STYLE
         *--HDM B602703,1 03/25/1999 [Start] Also Check llBrowse Value we may forcing browse
         llMajorFound = SEEK(lcMajor)
         IF TYPE('llbrowse')='L' AND llbrowse
           llCanCont = .T.
         ELSE
         *--HDM B602703,1 03/25/1999 [End]

           IF llMajorFound
             lcReturn = STYLE.STYLE
             llCanCont = .F.
           ELSE
             lcMajor = PADR(lcMajor,LEN(gfItemMask('PM')))
             lnRecNO = IIF(SEEK(lcMajor,'STYLE'),RECNO(),RECNO(0))
             llCanCont = .T.
           ENDIF  
           IF llCanCont 
             IF  lnRecNO>0 
               GO lnRecNO 
               IF PADR(STYLE.STYLE,LEN(ALLT(lcMajor)))=ALLT(lcMajor)
                 SKIP
                 IF PADR(STYLE.STYLE,LEN(ALLT(lcMajor)))<>ALLT(lcMajor)
                   SKIP -1
                   lcReturn = STYLE.STYLE
                   llCanCont = .F.
                 ELSE  
                  SKIP -1
                 ENDIF
               *--HDM B602691,1 03/25/1999[Start] Added <else> to Force empty 
               *--              lcMajor case no records matches the user input
               ELSE
                 lcMajor = ''  
               *--HDM B602691,1 03/25/1999[End]
               ENDIF
             ELSE
               GO TOP
               lcMajor = ''  
             ENDIF
          ENDIF
         ENDIF  
        *E301157,1 Hesham (End)
  ENDCASE
  lcMask = '@! '+lcMask
  lnStartSeg = IIF(lcMajorOrNon = 'N',lnNonMajStart,1)
  lnEndSeg   = IIF(lcMajorOrNon = 'M',lnMajorEnd,lnNonMajEnd)
  lnActSeg    = lnStartSeg 
  IF lcMajorOrNon = 'M'
    lcMajor = IIF(lfEmpty(lcMajor,'M'),'',lcMajor)
    SELECT STYLE
    SET ORDER TO TAG CSTYLE
  ENDIF
  *--HDM E301037,1[START]

  lnSegWidth = lfStyInfo('M')
  FOR lnLoop = 1 TO ALEN(laSegInfo,1)
    IF laSegInfo[lnLoop,3] $'OTQZCDGS' .AND. laSegInfo[lnLoop,1] = 'M'
      llSegBrow = .T.
      EXIT
    ELSE
      llSegBrow = .F.
    ENDIF
  ENDFOR
  *--HDM E301037,1[END]
  IF llCanCont
*    FOR lnCount = 1 TO lnNoSeg
*    FOR lnCount = lnStartSeg TO lnEndSeg
*      lcAlias = laSeg[lnCount,4]  
*      SELECT (lcAlias)
*      lcExpr = SYS(14,VAL(SYS(21)))
*      SET FILTER TO &lcExpr = &laSeg[lnCount,5]
*      LOCATE
*      llCanCont = FOUND() &&OR laSeg[lnCount,2] = 'F'
*      IF !llCanCont
*        lcEmptyErr = laSeg[lnCount,7]
*        lcEmptyErr = IIF(UPPER(lcEmptyErr)='BROWSE','Colors',lcEmptyErr)
*        EXIT
*      ENDIF
*    ENDFOR 
    IF llCanCont
      IF llStyleData
        lcSegVal = STYLE.STYLE
      ENDIF  

      PUSH MENU _MSYSMENU
      PUSH KEY CLEAR
      *B602928,1 Hesham (Start)
      *B602928,1 Clear the keyboard buffer to permit the key traping from
      *B602928,1 firing before the window display
      CLEAR TYPEAHEAD
      *B602928,1 Hesham (End)
      ON KEY
      ON KEY LABEL CTRL+W ll = .T.
      ON KEY LABEL CTRL+Q ll = .T.    
      ON KEY LABEL ESC DO lfCancel 
      ON KEY LABEL ENTER DO lfSelect
      ON KEY LABEL CTRL+END DO lfGoRec WITH "E"
      ON KEY LABEL CTRL+Home DO lfGoRec WITH "H"      
      ON KEY LABEL TAB DO lfNextObj WITH 'N'
      ON KEY LABEL BACKTAB DO lfNextObj WITH 'P'    
**     DO (gcScrDir+"SY\STYLEBR.SPX")
      KEYBOARD "{ALT+B}"
*-- E301036,1 HDM 10/07/98 [start] This should be replaced with 'DO STYBROW' WHICH IS LOCATED IN ARIA3.PRG

IF llCanCont
DO CASE
  CASE llSegBrow = .F.
    lcClr=IIF(lcMajorOrNon = 'N' , '' , '*')
    llMajor = IIF(ALLTRIM(lcMajorOrNon) = 'M', .T. , .F.)
    *--Call STYBROW proceedure instead of STYLEBR.SPX
    lnSegWidth = lfStyInfo('M')
      DO STYBROW WITH lcMajor,lcNonMajor , lcClr,.T.
      *--HDM B602656,1 Don't return just make the program to return itself[start]
      *ON KEY
      *POP KEY &&ALL
      *POP MENU _MSYSMENU
      *--HDM B602656,1 Don't return just make the program to return itself[End]
 
      IF lcMajorOrNon='M'
        lcMajor = SUBSTR(lcMajor,1,lnSegWidth)
      ELSE
        IF lcMajorOrNon='N'
          lcMajor = SUBSTR(lcMajor,lnSegWidth + 2,LEN(lcMajor))
        ENDIF
      ENDIF
    IF !EMPTY(lcStyleTag)
      SELECT STYLE
      SET ORDER TO TAG (lcStyleTag)
    ELSE
      SELECT STYLE
      SET ORDER TO
    ENDIF
    SELECT (lnAlias)
    *--HDM B602656,1 Don't return just make the program to return itself[start]
    *RETURN lcMajor
    lcReturn = lcMajor
    *--HDM B602656,1 Don't return just make the program to return itself[End]
    
  CASE llSegBrow = .T.
    DO STYLEBR.SPX
ENDCASE
ENDIF
*-- E301036,1 HDM 10/07/98 [end]
      ON KEY
      POP KEY &&ALL
      POP MENU _MSYSMENU
    ELSE  
      =gfModalGen('QRM00276B00000','DIALOG',lcEmptyErr)    
    ENDIF  
    FOR lnCount = 1 TO ALEN(laOpenFiles,1)
      IF laOpenFiles[lnCount,2] AND USED(laOpenFiles[lnCount,1])
        USE IN (laOpenFiles[lnCount,1])
      ELSE
        SELECT (laOpenFiles[lnCount,1])
        SET FILTER TO &laOpenFiles[lnCount,3]
        IF !EMPTY(laOpenFiles[lnCount,4])
          SET ORDER TO TAG (laOpenFiles[lnCount,4])
        ELSE
          SET ORDER TO
        ENDIF
      ENDIF
    ENDFOR
  ENDIF  
ELSE
   =gfModalGen('QRM00275B00000','DIALOG')    
ENDIF  
IF llStyleUse AND USED('STYLE')
  USE IN STYLE
ELSE
  SELECT STYLE
  IF !EMPTY(lcStyleTag)
    SET ORDER TO TAG (lcStyleTag)
  ELSE
    SET ORDER TO
  ENDIF
ENDIF
SELECT (lnAlias)
RETURN lcReturn


*!*************************************************************************
*! Name      : LFACTFOLDER
*! Developer : Hesham El-Sheltawi 
*! Date      : 07/21/97    
*! Purpose   :
*!*************************************************************************
*: Calls       :
*:            FUNCTION : lfBrowCod
*:            FUNCTION : lfIncSeg
*:            FUNCTION : lfBrowSty
*!*************************************************************************      
*! Returns   :  
*:*************************************************************************     
FUNCTION lfActFolder
IF lnActFolder = 2
  SHOW GET lcSegVal DISABLE
  FOR lnCount  = 1 TO lnNoSeg
    IF BETWEEN(lnCount,lnStartSeg,lnEndSeg)
      SHOW GET laSegVal[lnCount] ENABLE
    ELSE
      SHOW GET laSegVal[lnCount] DISABLE    
    ENDIF  
  ENDFOR
  =lfBrowCod()
  DEFINE BAR 101 OF P01PU01 PROMPT "Next Segment" KEY RIGHTARROW
  DEFINE BAR 102 OF P01PU01 PROMPT "Previous Segment" KEY LEFTARROW
  ON SELECTION BAR 101 OF P01PU01 DO lfIncSeg WITH 1
  ON SELECTION BAR 102 OF P01PU01 DO lfIncSeg WITH -1
ELSE
  IF !EMPTY(lcMajor) AND lcMajorOrNon='N'
    SHOW GET lcSegVal DISABLE  
  ELSE
    SHOW GET lcSegVal ENABLE
  ENDIF  
  RELEASE BAR 101 OF P01PU01
  RELEASE BAR 102 OF P01PU01
  FOR lnCount  = 1 TO lnNoSeg
    SHOW GET laSegVal[lnCount] DISABLE
  ENDFOR
  =lfBrowSty()
ENDIF
=lfRefresh()



*!*************************************************************************
*! Name      : LFVNEWVAL
*! Developer : Hesham El-Sheltawi 
*! Date      : 07/21/97    
*! Purpose   :
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfvNewVal


*!*************************************************************************
*! Name      : LFMARKSEG
*! Developer : Hesham El-Sheltawi 
*! Date      : 07/21/97    
*! Purpose   :
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfMarkSeg
lcMark = ''
lnMark = 2.00
IF lnActSeg>1
*  FOR lnCount = 1 TO lnActSeg
*    lcMark = lcMark+IIF(lnCount>1,PADR(' ',laSeg[lnCount-1,1]),'')+IIF(lnCount>1,' ','')+IIF(lnCount=lnActSeg,' ','')
*    lnMark = (LEN(lcMark)*1.700)
*  ENDFOR
    lcMark = SPACE(laSeg[lnActSeg,3])
*    lnMark = (LEN(lcMark)*1.700)  
    lnMark = LEN(lcMark)+1
ENDIF
RETURN lnMark


*!*************************************************************************
*! Name      : LFWSEGCHNG
*! Developer : Hesham El-Sheltawi 
*! Date      : 07/21/97    
*! Purpose   :
*!*************************************************************************
*: Calls       :
*:            FUNCTION : lfBrowCod
*!*************************************************************************      
*! Returns   :  
*:*************************************************************************     
FUNCTION lfwSegChng
IF MDOWN()
  lnActSeg = VAL(SUBSTR(VARREAD(),ATC('(',VARREAD())+1,1))
  =lfBrowCod()
  =lfRefresh()
  _CUROBJ = OBJNUM(ibfolder(2))  
   KEYBOARD '{ALT+B}'
*  ACTIVATE WINDOW (lcBrTitl2)
ENDIF
RETURN .F.


*!*************************************************************************
*! Name      : LFBROWSTY
*! Developer : Hesham El-Sheltawi 
*! Date      : 07/21/97    
*! Purpose   :
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfBrowSty
PRIVATE lcBrFields,lcMajDesc
IF lcMajorOrNon = 'M'
  lcMajDesc = gfItemMask('HM')
  lcBrFields = [cPoint=IIF(lnStyRec=RECNO(),'>',''):2:H='',cstymajor:H=']+;
               lcMajDesc+[':22,DESC:13:H="Description",]+;
               [Season:7:H="Se",cdivision:7:H="Division",pricea]+;
               [:6:h="Price",totWip:7:h="WIP",totstk:7:h="Stock",]+;
               [totord:7:h="Orders",OTS=(TOTWIP+TOTSTK-TOTORD):7:H="O.T.S.",]+;
               [Fabric:9:h="Fabric"]
ELSE
  lcMajDesc = gfItemMask('HI')
  lcBrFields = [cPoint=IIF(lnStyRec=RECNO(),'>',''):2:H='',Style:H=']+;
               lcMajDesc+[':22,DESC:13:H="Description",]+;
               [Season:7:H="Se",cdivision:7:H="Division",pricea]+;
               [:6:h="Price",totWip:7:h="WIP",totstk:7:h="Stock",]+;
               [totord:7:h="Orders",OTS=(TOTWIP+TOTSTK-TOTORD):7:H="O.T.S.",]+;
               [Fabric:9:h="Fabric"]
  *E301853,1 WAB (Start)- if the style code structure contain color add the color description
  IF llUseColor 
    lcBrFields = [cPoint=IIF(lnStyRec=RECNO(),'>',''):2:H='',Style:H=']+;
                 lcMajDesc+[':22,DESC:13:H="Description",]+;
                 [lcColorDesc = gfCodDes(SUBSTR(STYLE.STYLE,lnClrStrt,lnClrWidth),"COLOR") :H=lcNMjrTl+'Description',]+;                              
                 [Season:7:H="Se",cdivision:7:H="Division",pricea]+;
                 [:6:h="Price",totWip:7:h="WIP",totstk:7:h="Stock",]+;
                 [totord:7:h="Orders",OTS=(TOTWIP+TOTSTK-TOTORD):7:H="O.T.S.",]+;
                 [Fabric:9:h="Fabric"]
  ENDIF
  *E301853,1 WAB (END)
               

ENDIF             
SELECT STYLE
lcKey = IIF(lfEMPTY(lcMajor,'M'),'','KEY (lcMajor)')
IF !WEXIST(lcBrTitl1)
  BROWSE &lcKey FIELDS &lcBrFields;
         WINDOW (lcWinCh3) IN WINDOW (lcWinCh1) TITLE lcBrTitl1 ;
         NOMENU NOAPP NODEL NOMODIFY NOWAIT NOCLEAR;
         WHEN lfChngVal()
  MOVE WINDOW (lcBrTitl1) TO 0,0
ENDIF  
SHOW WINDOW (lcBrTitl1) REFRESH

*!*************************************************************************
*! Name      : LFBROWCOD
*! Developer : Hesham El-Sheltawi 
*! Date      : 07/21/97    
*! Purpose   :
*!*************************************************************************
*: Calls       :
*:            FUNCTION : lfChngSeg
*!*************************************************************************      
*! Returns   :  
*:*************************************************************************     
FUNCTION lfBrowCod
PRIVATE lcBrFields
lcAlias = laSeg[lnActSeg,4]
lcBrFields = [cPoint=IIF(lnSegRec=RECNO(),'>',''):2:H='',]+laSeg[lnActSeg,8]
lcBrTitl2 = ALLT(laSeg[lnActSeg,7])
*lcBrTitl2 = lcBrTitl2
SELECT (lcAlias)
lcExpr = SYS(14,VAL(SYS(21)))
lcStyFlt = ''
IF !llCanAdd
  IF lnActSeg>1
    lcStyFlt = ' AND SEEK(SUBSTR(lcSegVal,1,'+ALLT(STR(laSeg[lnActSeg,3]-1))+')+PADR('+laSeg[lnActSeg,9]+','+ALLT(STR(laSeg[lnActSeg,1]))+[),'STYLE')]
  ELSE
    lcStyFlt = ' AND SEEK(PADR('+laSeg[lnActSeg,9]+','+ALLT(STR(laSeg[lnActSeg,1]))+[),'STYLE')]
  ENDIF
ENDIF
lcStyFlt = lcStyFlt+IIF(UPPER(lcAlias)='CODES',[ AND CRLTFIELD='N'],'')
SET FILTER TO &lcExpr = &laSeg[lnActSeg,5] &lcStyFlt
IF !WEXIST(lcBrTitl2)
  BROWSE FIELDS &lcBrFields;
         WINDOW (lcWinCh3) IN WINDOW (lcWinCh2) TITLE lcBrTitl2+ IIF(lcBrTitl2 = "Browse"," Color","") ;
         NOMENU NOAPP NODEL NOMODIFY NOWAIT NOCLEAR;
         WHEN lfChngSeg()
  MOVE WINDOW (lcBrTitl2) TO 0,0
ENDIF
SHOW WINDOW (lcBrTitl2) REFRESH
=lfChngSeg()

*!*************************************************************************
*! Name      : LFCHNGVAL
*! Developer : Hesham El-Sheltawi 
*! Date      : 07/21/97    
*! Purpose   :
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfChngVal
lcSegVal = STYLE
lnStyRec = RECNO()
SHOW WINDOW (WONTOP()) REFRESH SAME
SHOW GET lcSegVal


*!*************************************************************************
*! Name      : LFCHNGSEG
*! Developer : Hesham El-Sheltawi 
*! Date      : 07/21/97    
*! Purpose   :
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfChngSeg
lcSegVal = STUFF(lcSegVal,laSeg[lnActSeg,3],laSeg[lnActSeg,1],PADR(EVAL(laSeg[lnActSeg,9]),laSeg[lnActSeg,1]))
IF !llCanAdd AND !SEEK(lcSegVal,'STYLE')
    lnSeekLen = IIF(lnActSeg<lnNoSeg,laSeg[lnActSeg+1,3]-1,laSeg[lnActSeg,3]+laSeg[lnActSeg,1]-1)
    =SEEK(SUBSTR(lcSegVal,1,lnSeekLen),'STYLE')
    lcSegVal = STYLE.STYLE
ENDIF
lnSegRec = RECNO()
SHOW WINDOW (WONTOP()) REFRESH SAME
SHOW GET lcSegVal

*!*************************************************************************
*! Name      : LFINCSEG
*! Developer : Hesham El-Sheltawi 
*! Date      : 07/21/97    
*! Purpose   :
*!*************************************************************************
*: Calls       :
*:            FUNCTION : lfBrowCod
*!*************************************************************************      
*: Passed parameters  : lnincseg
*:*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfIncSeg
PARAMETERS lnIncSeg
lnActSeg = lnActSeg + lnIncSeg
*lnActSeg = IIF(lnActSeg>lnNoSeg,1,IIF(lnActSeg<1,lnNoSeg,lnActSeg))
lnActSeg = IIF(lnActSeg>lnEndSeg,lnStartSeg,IIF(lnActSeg<lnStartSeg,lnEndSeg,lnActSeg))
=lfBrowCod()
=lfRefresh()




*!*************************************************************************
*! Name      : LFVSELECT
*! Developer : Hesham El-Sheltawi 
*! Date      : 07/21/97    
*! Purpose   :
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION LFVSELECT
lcWinToRele = lcBrTitl1+',Color,Scale,cDivision,Domestic,Quality,Other,Free,Group'
RELEASE WINDOWS &lcWinToRele 
lcReturn = SUBSTR(lcSegVal,laSeg[lnStartSeg,3],laSeg[lnEndSeg,3]+laSeg[lnEndSeg,1]-1)


*!*************************************************************************
*! Name      : LFVCANCEL
*! Developer : Hesham El-Sheltawi 
*! Date      : 07/21/97    
*! Purpose   :
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfvCancel
SELECT STYLE
IF BETWEEN(lnStyRec,1,RECCOUNT())
  GO lnStyRec
ENDIF
lcWinToRele = lcBrTitl1+',Color,Scale,cDivision,Domestic,Quality,Other,Free,Group'
RELEASE WINDOWS &lcWinToRele 
lcReturn = ''


FUNCTION lfReadWhen
DEFINE BAR 100 OF P01PU01 PROMPT " " KEY ALT+B
ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (IIF(lnActFolder=1,'STYLE',lcBrTitl2))


FUNCTION lfvStyle
IF !SEEK(lcSegVal,'STYLE')
    lnSeekSeg = lnNoSeg
    lnSeekLen = IIF(lnSeekSeg<lnNoSeg,laSeg[lnSeekSeg+1,3]-1,laSeg[lnSeekSeg,3]+laSeg[lnSeekSeg,1]-1)
    DO WHILE !SEEK(SUBSTR(lcSegVal,1,lnSeekLen),'STYLE') AND lnSeekSeg>=1
      lnSeekSeg = lnSeekSeg -1
      lnSeekLen = IIF(lnSeekSeg<lnNoSeg,laSeg[lnSeekSeg+1,3]-1,laSeg[lnSeekSeg,3]+laSeg[lnSeekSeg,1]-1)
    ENDDO
  lcSegVal = STYLE.STYLE
ENDIF
SHOW GET lcSegVal
KEYBOARD '{ALT+B}'

FUNCTION lfCancel
ACTIVATE WINDOW (lcWinCh4) TOP
_CUROBJ = OBJNUM(pbcancel)
KEYBOARD "{SPACEBAR}"

FUNCTION lfGoRec
Parameters lcDirection
IF !(WONTOP()=lcWinChP OR WONTOP()=lcWinCh4)
  IF lcDirection = 'E'
    GO BOTT
  ELSE
    GO TOP
  ENDIF
ENDIF

FUNCTION lfSelect
IF WONTOP()=lcWinChP OR WONTOP()=lcWinCh4
  KEYBOARD "{ENTER}" PLAIN
ELSE
  ACTIVATE WINDOW (lcWinCh4) TOP
  _CUROBJ = OBJNUM(pbSelect)
  KEYBOARD "{SPACEBAR}"
ENDIF


FUNCTION lfNextObj
PARAMETERS lcDirection
lcWontop = IIF(ATC(' ',WONTOP())=0,WONTOP(),SUBSTR(WONTOP(),1,ATC(' ',WONTOP())-1))
IF lcWontop $ lcWith
  IF lcDirection = 'N'
    _CUROBJ = OBJNUM(pbselect)
     ACTIVATE WINDOW (lcWinCh4) TOP    
  ELSE
    IF lnActFolder = 1
      ACTIVATE WINDOW (lcWinChP) TOP
      _CUROBJ = OBJNUM(lcSegVal)
    ELSE
      ACTIVATE WINDOW (lcWinCh4) TOP
     _CUROBJ = OBJNUM(ibfolder[2])
    ENDIF
  ENDIF  
ELSE
 IF lcDirection = 'N'
   IF (lnActFolder = 2 AND 'IBFOLDER' $ VARREAD()) OR  (lnActFolder = 1 AND 'LCSEGVAL' $ VARREAD())
     KEYBOARD "{ALT+B}"
   ELSE
    _CUROBJ = _CUROBJ +1     
   ENDIF
 ELSE
   IF VARREAD() = 'PBSELECT'
     KEYBOARD "{ALT+B}"     
   ELSE
     IF VARREAD() = 'LCSEGVAL'
       _CUROBJ = OBJNUM(IBFOLDER[lnActFolder])
     ELSE
       IF 'IBFOLDER' $ VARREAD()  &&AND lnActFolder = 2
         _CUROBJ = OBJNUM(pbCancel)
       ELSE
         _CUROBJ = _CUROBJ -1
       ENDIF  
     ENDIF  
   ENDIF  
 ENDIF  
ENDIF

FUNCTION LFEMPTY
PARAMETERS lcValue,lcMajORNon
PRIVATE lnStartSeg,lnEndSeg,lnPosition,lcValue,lcMajORNon,llEmpty
lnStartSeg = IIF(lcMajORNon= 'N',lnNonMajStart,1)
lnEndSeg   = IIF(lcMajORNon= 'M',lnMajorEnd,lnNonMajEnd)
lnPosition = laSeg[lnStartSeg,3] - 1
llEmpty = .T.
IF !EMPTY(lcValue)
  FOR lnCount = lnStartSeg TO lnEndSeg
    IF !EMPTY(SUBSTR(lcValue,laSeg[lnCount,3]-lnPosition,laSeg[lnCount,1]))
      llEmpty = .F.
      EXIT
    ENDIF  
  ENDFOR
ENDIF  
RETURN llEmpty



FUNCTION lfvSeg
PARAMETERS lcValue,lcMajORNon
PRIVATE lnStartSeg,lnEndSeg,lnPosition,lcValue,lcMajORNon,llEmpty
lnStartSeg = IIF(lcMajORNon= 'N',lnNonMajStart,1)
lnEndSeg   = IIF(lcMajORNon= 'M',lnMajorEnd,lnNonMajEnd)
lnPosition = laSeg[lnStartSeg,3] - 1
IF !EMPTY(lcValue)
  FOR lnCount = lnStartSeg TO lnEndSeg
    lcAlias = laSeg[lnCount,4]
    SELECT (lcAlias)
    lcExpr = SYS(14,VAL(SYS(21))) 
    IF (laSeg[lnCount,2] # 'F' OR EMPTY(SUBSTR(lcValue,laSeg[lnCount,3]-lnPosition,laSeg[lnCount,1])))AND !SEEK(EVAL(laSeg[lnCount,5])+SUBSTR(lcValue,laSeg[lnCount,3]-lnPosition,laSeg[lnCount,1]))
      GO TOP
      lcValue = STUFF(lcValue,laSeg[lnCount,3]-lnPosition,laSeg[lnCount,1],;
      PADR(EVAL(laSeg[lnCount,9]),laSeg[lnCount,1]))
    ENDIF  
  ENDFOR
ENDIF  
RETURN lcValue
*!*************************************************************
*! Name      : lfStyInfo
*! Developer : Hossam eletreby
*! Date      : 06/02/97
*! Purpose   : Biuld array for segment information.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : array elements as :
*!-1 MAJOR,NONMAJOR
*!-2 SEGMENT NUMBER
*!-3 SEGMENT TYPE 'FOTQZCDGS'
*!   F-> Free , O-> Other , T->Make , Q->Quality ,Z->Season
*!   C-> Color , D->Division ,G->Group ,S->Size
*!-4 SEGMENT START
*!-5 SEGMENT SIZE
*!-6 SEGMENT FILE
*!-7 SEGMENT FILE TAG
*!-8 SEGMENT FILE KEY
*!-9 SEGMENT FILE FIELD
*!*************************************************************
*! Example   : =lfStyInfo('M')
*!*************************************************************

FUNCTION lfStyInfo
PARAMETER lcSegment
*B602277,1 HDM 11/29/98 Make Them Private[START]
PRIVATE lnAlias    , lnMajSeg   , lnNonSeg , lcItemTl   , lcMjrTtl   , lcNMjrTl , lcMjrPct,;
                     lcNMjrPt   , lcIMjrPt , lnstylewid , lncolorwid , lcSepart
*B602277,1 HDM 11/29/98 Make Them Private[END]

lnAlias=SELECT()
llStruOp=gfOpenFile(gcDataDir+'ICISTRU','Segno','SH')
IF !SEEK('U','ICISTRU')
  IF USED('ICISTRU') AND llStruOp
    USE IN ICISTRU
  ENDIF
  RETURN .F.
ENDIF
IF USED('ICISTRU') AND llStruOp
  USE IN ICISTRU
ENDIF


DIME laMajSeg[1,1]
=gfItemMask(@laMajSeg)
lnMajSeg  = gfItemMask('SM')
lnNonSeg  = gfItemMask('SN')  
lcItemTl  = gfItemMask('HI')
lcMjrTtl  = gfItemMask('HM')
lcNMjrTl  = gfItemMask('HN')
lcMjrPct  = gfItemMask('PM')
lcNMjrPt  = gfItemMask('PN')
lcIMjrPt  = gfItemMask('PI')
lnstylewid=LEN(lcMjrPct)
lncolorwid=LEN(lcNMjrPt)
lcSepart  =SUBSTR(lcIMjrPt,lnstylewid+1,1)

FOR lnCnt=1 TO ALEN(laMajSeg,1)
  DIME laSegInfo[lnCnt,9]
  IF lnCnt<=lnMajSeg
     laSegInfo[lnCnt,1] = 'M' 
  ELSE
    IF lnCnt<=lnNonSeg+lnMajSeg
      laSegInfo[lnCnt,1] = 'N' 
    ELSE
      laSegInfo[lnCnt,1] = ' ' 
    ENDIF
  ENDIF
  laSegInfo[lnCnt,2]=STR(lnCnt,1)
  laSegInfo[lnCnt,3]=laMajSeg[lnCnt,1]
  laSegInfo[lnCnt,4]=laMajSeg[lnCnt,4]
  laSegInfo[lnCnt,5]=LEN(laMajSeg[lnCnt,3])
  laSegInfo[lnCnt,6]=IIF(laMajSeg[lnCnt,1]$'FOQT','ICSEGVAL',;
                     IIF(laMajSeg[lnCnt,1]='S','SCALE','CODES'))
  laSegInfo[lnCnt,7]=IIF(laMajSeg[lnCnt,1]$'FOQT','SEGVAL',;
                     IIF(laMajSeg[lnCnt,1]='S','SCALE','Idrltfname'))
  *E300789,1 Hesham (Start)                   
  *laSegInfo[lnCnt,8]=IIF(laMajSeg[lnCnt,1]$'FOQT',STR(lnCnt,1),;
                     IIF(laMajSeg[lnCnt,1]='S','S',gcAct_Comp+'N'))

  laSegInfo[lnCnt,8]=IIF(laMajSeg[lnCnt,1]$'FOQT',STR(lnCnt,1),;
                     IIF(laMajSeg[lnCnt,1]='S','S','NN'))

  *E300789,1 Hesham (End)
  IF laMajSeg[lnCnt,1]$'CZDGF'
    DO CASE
      CASE laSegInfo[lnCnt,3]='C'
        lcCdTyp = 'COLOR     '
      CASE laSegInfo[lnCnt,3]='Z'
        lcCdTyp = 'SEASON    '
      CASE laSegInfo[lnCnt,3]='D'
        lcCdTyp = 'CDIVISION '
      CASE laSegInfo[lnCnt,3]='G'
        lcCdTyp = 'CSTYGROUP '
      CASE laSegInfo[lnCnt,3]='F'
        lcCdTyp  =''
        lcFreeDc = laMajSeg[lnCnt,2]
    ENDCASE    
    laSegInfo[lnCnt,8] = laSegInfo[lnCnt,8]+lcCdTyp
  ENDIF
  laSegInfo[lnCnt,9] = IIF(laMajSeg[lnCnt,1]$'FOQT',"ciSegVal",;
                       IIF(laMajSeg[lnCnt,1]='S',"Scale","cCode_no"))
ENDFOR
SELECT(lnalias)
RETURN IIF(lcSegment='M',lnstylewid , lncolorwid)