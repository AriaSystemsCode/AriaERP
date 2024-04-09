*:************************************************************************
*: Program file  : GFCHREPF.PRG
*: Program desc. : 
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: 
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
PARAMETERS lcFormArr,lcFormCnt
PRIVATE llTerminate,laObjScrP
PRIVATE ALL LIKE LN*
llTermStat = .F.

lcOGCol1   = SCHEME(5,2)
lcOGCol2   = SCHEME(5,3)
lcOGCol3   = SCHEME(5,2)
lnMaxPosV  = 0
lnMaxPosH  = 0
lnWVPos    = 0
lnWHPos    = 0
lnWVMax    = 18
lnWHMax    = 78
lnWVEnd    = 18
lnWHEnd    = 78
lnScrolCol = IIF(_DOS,2,3.300)
lnScrolRow = IIF(_DOS,1,3.300)
*******
lcListColor=SCHEME(6,3)
lcListColor=STRTRAN(SUBSTR(lcListColor,1,ATC('/',lcListColor)-1),'+','')+;
            SUBSTR(lcListColor,ATC('/',lcListColor))
lcXorColor=SCHEME(6,3)
lcXorColor=STRTRAN(SUBSTR(lcXorColor,ATC('/',lcXorColor)+1),'*','')+'/'+;
           STRTRAN(SUBSTR(lcXorColor,1,ATC('/',lcXorColor)-1),'+','')
******
store 0 to inv
lnObjVDiv =  IIF(lcOGPlatForm='WINDOWS',104.167*8,1) 
lnObjhDiv = IIF(lcOGPlatForm='WINDOWS',104.167*8,1) 
lnCharV=1 &&IIF(_WINDOWS,FONTMETRIC(3),1)
lnCharH=1 &&IIF(_WINDOWS,FONTMETRIC(3),1)
=lfGetMaxPos()


*lnwHeight = IIF(_DOS,lnMaxPosV,(lnMaxPosV/(FONTMETRIC(20,lcRepFont,lnFontSize)+FONTMETRIC(20,lcRepFont,lnFontSize)/FONTMETRIC(20,'MS SANS SERIF',8))) /FONTMETRIC(1,'MS SANS SERIF',8)) 
*lnwWidth = IIF(_DOS,lnMaxPosH,(lnMaxPosH/(FONTMETRIC(19,lcRepFont,lnFontSize)+FONTMETRIC(19,lcRepFont,lnFontSize)/FONTMETRIC(19,'MS SANS SERIF',8))) /FONTMETRIC(6,'MS SANS SERIF',8))  

lnwHeight = IIF(lcOGPlatForm='DOS',lnMaxPosV,(lnMaxPosV/(FONTMETRIC(20,lcRepFont,lnFontSize)+FONTMETRIC(20,lcRepFont,lnFontSize)/FONTMETRIC(20,'MS SANS SERIF',8))) /FONTMETRIC(1,'MS SANS SERIF',8)) 
lnwWidth = IIF(lcOGPlatForm='DOS',lnMaxPosH,(lnMaxPosH/(FONTMETRIC(19,lcRepFont,lnFontSize)+FONTMETRIC(19,lcRepFont,lnFontSize)/FONTMETRIC(19,'MS SANS SERIF',8))) /FONTMETRIC(6,'MS SANS SERIF',8))  

lcLastWin=''
llTerminate=.F.

PUSH KEY CLEAR
ON KEY LABEL DNARROW DO lfkScrollDN
ON KEY LABEL UPARROW DO lfkScrollUP
ON KEY LABEL LEFTARROW DO lfkScrollL
ON KEY LABEL RIGHTARROW DO lfkScrollR
*IF _WINDOWS OR _MAC
 IF lcOGPlatForm='WINDOWS' OR lcOGPlatForm='MAC'
  DO WHILE !llTerminate
    REPDISPOBJ=SYS(2015)
    DO MODIREP.SPR
  ENDDO  
ELSE
    REPDISPOBJ=SYS(2015)
    DO MODIREP.SPR  
ENDIF  
POP KEY


RETURN 

FUNCTION lfGetRepFld
PARAMETERS llDefInv
DIMENSION LACOLOR[3]
LACOLOR[1]='RGB(255,0,0,0,255,0)'
LACOLOR[2]='RGB(0,255,0,0,0,255)'
LACOLOR[3]='RGB(0,0,255,255,0,0)'
STORE ''to lcColor,lcColor1,lcFont
FOR lnCount = 1 TO ALEN(&lcFormArr,1)
  IF &lcFormCnt[lnCount]
      lcValid="lfGetstyle("+ALLTRIM(STR(lnCount))+")"
      lcWhen="lfModiPos("+ALLTRIM(STR(lnCount))+")"      
      lcColor=''
*    IF _WINDOWS OR _MAC    
     IF lcOGPlatForm='WINDOWS' OR lcOGPlatForm='MAC'
      lcFont=IIF(EMPTY(&lcFormArr[lnCount,7]),'','FONT "'+&lcFormArr[lnCount,7]+'",'+ALLTRIM(STR(&lcFormArr[lnCount,9])))
      lcFont= lcFont+IIF(EMPTY(&lcFormArr[lnCount,8]),'',' STYLE "'+GETSTYLE(&lcFormArr[lnCount,8])+'Q"')    
      lnColSize=IIF(&lcFormArr[lnCount,10]=8,&lcFormArr[lnCount,5]/FONTMETRIC(19,&lcFormArr[lnCount,7],&lcFormArr[lnCount,9])/FONTMETRIC(6),;
                   (&lcFormArr[lnCount,5]/(FONTMETRIC(19,&lcFormArr[lnCount,7],&lcFormArr[lnCount,9])+FONTMETRIC(19,&lcFormArr[lnCount,7],&lcFormArr[lnCount,9])/FONTMETRIC(19)))/FONTMETRIC(6,&lcFormArr[lnCount,7],&lcFormArr[lnCount,9])*;
                   fontmetric(6,&lcFormArr[lnCount,7],&lcFormArr[lnCount,9])/fontmetric(6))

      lnColSize=&lcFormArr[lnCount,5]/FONTMETRIC(19,lcRepFont,lnFontSize)/FONTMETRIC(6)
      lcColor='COLOR '+laColor[IIF(MOD(lnCount,3)>0,MOD(lnCount,3),3)]
      @ &lcFormArr[lnCount,2]/FONTMETRIC(20,lcRepFont,lnFontSize)/FONTMETRIC(1),;
      &lcFormArr[lnCount,3]/FONTMETRIC(19,lcRepFont,lnFontSize)/FONTMETRIC(6) SAY ;
      IIF(TYPE(&lcFormArr[lnCount,1])<>'U',EVAL(&lcFormArr[lnCount,1]),ALLTRIM(&lcFormArr[lnCount,1]));
      SIZE &lcFormArr[lnCount,4]/FONTMETRIC(20,&lcFormArr[lnCount,7],&lcFormArr[lnCount,9])/fontmetric(1),;
      lnColSize;
       &lcFont;
       &lcColor;



                 
      *lnColSize=IIF(&lcFormArr[lnCount,10]=8,(&lcFormArr[lnCount,5]/lnObjVDiv)*fontmetric(6)/fontmetric(6,&lcFormArr[lnCount,7],&lcFormArr[lnCount,9]),;
                      (&lcFormArr[lnCount,5]/(FONTMETRIC(19,&lcFormArr[lnCount,7],&lcFormArr[lnCount,9])+FONTMETRIC(19,&lcFormArr[lnCount,7],&lcFormArr[lnCount,9])/FONTMETRIC(19)))/FONTMETRIC(6,&lcFormArr[lnCount,7],&lcFormArr[lnCount,9])*;
                     fontmetric(6,&lcFormArr[lnCount,7],&lcFormArr[lnCount,9])/fontmetric(6))

      *@ (&lcFormArr[lnCount,2]/(FONTMETRIC(20,lcRepFont,lnFontSize)+FONTMETRIC(20,lcRepFont,lnFontSize)/FONTMETRIC(20)))/FONTMETRIC(1,lcRepFont,lnFontSize),;
      (&lcFormArr[lnCount,3]/(FONTMETRIC(19,lcRepFont,lnFontSize)+FONTMETRIC(19,lcRepFont,lnFontSize)/FONTMETRIC(19)))/FONTMETRIC(6,lcRepFont,lnFontSize) SAY ;
      IIF(TYPE(&lcFormArr[lnCount,1])<>'U',EVAL(&lcFormArr[lnCount,1]),ALLTRIM(&lcFormArr[lnCount,1]));
      SIZE (&lcFormArr[lnCount,4]/(FONTMETRIC(20,&lcFormArr[lnCount,7],&lcFormArr[lnCount,9])+FONTMETRIC(20,&lcFormArr[lnCount,7],&lcFormArr[lnCount,9])/FONTMETRIC(20)))/FONTMETRIC(1,&lcFormArr[lnCount,7],&lcFormArr[lnCount,9])*;
      fontmetric(1)/fontmetric(1,&lcFormArr[lnCount,7],&lcFormArr[lnCount,9]),;
      lnColSize;
      &lcFont;
      &lcColor
  
      lnColSize=MIN(lnwWidth-((&lcFormArr[lnCount,3]/(FONTMETRIC(19,lcRepFont,lnFontSize)+FONTMETRIC(19,lcRepFont,lnFontSize)/;
                         FONTMETRIC(19)))/FONTMETRIC(6,lcRepFont,lnFontSize)+lnColSize),lnColSize)

      lnRowSize=(&lcFormArr[lnCount,4]/(FONTMETRIC(20,&lcFormArr[lnCount,7],&lcFormArr[lnCount,9])+FONTMETRIC(20,&lcFormArr[lnCount,7],&lcFormArr[lnCount,9])/FONTMETRIC(20)))/FONTMETRIC(1,&lcFormArr[lnCount,7],&lcFormArr[lnCount,9])*;
        fontmetric(1,&lcFormArr[lnCount,7],&lcFormArr[lnCount,9])/fontmetric(1)
   
     lnRowSize=MIN(ABS(lnWHeight-((&lcFormArr[lnCount,3]/(FONTMETRIC(19,lcRepFont,lnFontSize)+FONTMETRIC(19,lcRepFont,lnFontSize)/;
       FONTMETRIC(19)))/FONTMETRIC(6,lcRepFont,lnFontSize)+lnRowSize)),lnRowSize)  
   ELSE
     IF &lcFormArr[lnCount,7]=7
       lcLineType=''
       DO CASE
          CASE &lcFormArr[lnCount,8]=5
             lcLineType='DOUBLE'          
          CASE &lcFormArr[lnCount,8]=6
             lcLineType='PANEL'          
          CASE &lcFormArr[lnCount,8]=7
        ENDCASE    
        @ &lcFormArr[lnCount,2]/lnObjVDiv,&lcFormArr[lnCount,3]/lnObjVDiv TO ;
          &lcFormArr[lnCount,2]/lnObjVDiv+&lcFormArr[lnCount,4]/lnObjVDiv-1,;
          &lcFormArr[lnCount,3]/lnObjVDiv+&lcFormArr[lnCount,5]/lnObjVDiv;
          &lcLineType
      ELSE
       @ &lcFormArr[lnCount,2],&lcFormArr[lnCount,3] SAY ;
         IIF(TYPE(&lcFormArr[lnCount,1])<>'U',EVAL(&lcFormArr[lnCount,1]),ALLTRIM(&lcFormArr[lnCount,1]));
         SIZE &lcFormArr[lnCount,4],&lcFormArr[lnCount,5]
         *;
        &lcColor1
  
        @ &lcFormArr[lnCount,2],&lcFormArr[lnCount,3] GET INV;
          SIZE &lcFormArr[lnCount,4],&lcFormArr[lnCount,5];
          PICTURE "@*IVN ";
          VALID &lcValid ;
          WHEN  &lcWhen 
      ENDIF  
    ENDIF   
  ENDIF
ENDFOR
IF !EMPTY(lcLastWin)
  RELEASE WINDOW (lcLastWin)
ENDIF
  
FUNCTION GETSTYLE
PARAMETERS lnFntStyle
DO CASE
  CASE TYPE('lnFntSTYLE')='N'
    DO CASE
     CASE lnFntStyle=0
         RETURN 'N'
      CASE lnFntStyle=1
         RETURN 'B'
      CASE lnFntStyle=2
         RETURN 'I'  
      CASE lnFntStyle=3
         RETURN 'BI'  
    ENDCASE
  CASE TYPE('lnFntSTYLE')='C'
    DO CASE
      CASE lnFntStyle='N'
         RETURN 0
      CASE lnFntStyle='BI'
         RETURN 3
      CASE lnFntStyle='B'
         RETURN 1
      CASE lnFntStyle='I'
         RETURN 2           
    ENDCASE
  OTHERWISE
     RETURN 'N'  
ENDCASE    


FUNCTION lfGetstyle
PARAMETERS lnArrElm
*IF _DOS OR _UNIX
IF lcOGPlatForm='DOS' OR lcOGPlatForm='UNIX'
  lbold    = 'B' $ &lcFormArr[lnArrElm,6]
  lItalic  = 'I' $ &lcFormArr[lnArrElm,6]
  lUndLine = 'U' $ &lcFormArr[lnArrElm,6]
  lSupScr  = 'R' $ &lcFormArr[lnArrElm,6] 
  lSubScr  = 'L' $ &lcFormArr[lnArrElm,6] 
  RBALIGN=IIF('C' $ &lcFormArr[lnArrElm,6],2,IIF('J' $ &lcFormArr[lnArrElm,6],3,1))
  llStyle=.F.
  DO STYLE.SPR
  IF llStyle
    &lcFormArr[lnArrElm,6]= SUBSTR(' CJ',rbalign,1)+;
                          IIF(lBold,'B','')+;
                          IIF(lItalic,'I','') +;
                          IIF(lUndLine,'U','')   +;
                          IIF(lSupScr,'R','')+;
                          IIF(lSubScr,'L','')
   ENDIF                          
ELSE
  lcTmpWin=SYS(2015)
  lcFontStyle=GETSTYLE(&lcFormArr[lnArrElm,8])
   DEFINE WINDOW (lcTmpWin) from 1,1 to 2,2 FONT &lcFormArr[lnArrElm,7],;
      &lcFormArr[lnArrElm,9] STYLE lcFontStyle
   ACTIVATE WINDOW (lcTmpWin) NOSHOW
   lcWinFont=GETFONT()
   IF !EMPTY(lcWinFont)
     lcWinFont='"'+STRTRAN(STRTRAN(lcWinFont,',',' STYLE "',OCCURS(',',lcWinFont),;
                   OCCURS(',',lcWinFont)),',','",',1,1)+'"'
     DEFINE WINDOW (lcTmpWin) from 1,1 to 2,2 FONT &lcWinFont
     &lcFormArr[lnArrElm,7]=WFONT(1,lcTmpWin)
     &lcFormArr[lnArrElm,8]=GETSTYLE(WFONT(3,lcTmpWin))   
     &lcFormArr[lnArrElm,9]=WFONT(2,lcTmpWin)   
     RELEASE WINDOW (lcTmpWin)
     CLEAR READ
     lcLastWin=REPDISPOBJ
   ENDIF  
ENDIF


FUNCTION lfModiPos
PARAMETERS lnArrElm
lnWStartCol=WLCOL('REPMODIFY')-WLCOL(REPDISPOBJ)
lnWStartRow=WLROW('REPMODIFY')-WLROW(REPDISPOBJ)
lnWEndCol=lnWStartCol+WCOL('REPMODIFY')
lnWEndRow=lnWStartRow+WROW('REPMODIFY')
*IF _WINDOWS OR _MAC
IF lcOGPlatForm='WINDOWS' OR lcOGPlatForm='MAC'
lnObjStartR=(&lcFormArr[_curobj,2]/(FONTMETRIC(20,lcRepFont,lnFontSize)+FONTMETRIC(20,lcRepFont,lnFontSize)/FONTMETRIC(20,'MS SANS SERIF',8)))/FONTMETRIC(1,lcRepFont,lnFontSize)
*  lnObjStartR=(&lcFormArr[lnArrElm,2]/(FONTMETRIC(20,lcRepFont,lnFontSize)+FONTMETRIC(20,lcRepFont,lnFontSize)/FONTMETRIC(20,'MS SANS SERIF',8)))/FONTMETRIC(1)
  lnObjStartC=(&lcFormArr[lnArrElm,3]/(FONTMETRIC(19,lcRepFont,lnFontSize)+FONTMETRIC(19,lcRepFont,lnFontSize)/FONTMETRIC(19)))/FONTMETRIC(6) 
  lnObjEndC=IIF(&lcFormArr[lnArrElm,10]=8,(&lcFormArr[lnArrElm,5]/lnObjVDiv)*fontmetric(6)/fontmetric(6,&lcFormArr[lnArrElm,7],&lcFormArr[lnArrElm,9]),;
                  (&lcFormArr[lnArrElm,5]/(FONTMETRIC(19,&lcFormArr[lnArrElm,7],&lcFormArr[lnArrElm,9])+FONTMETRIC(19,&lcFormArr[lnArrElm,7],&lcFormArr[lnArrElm,9])/FONTMETRIC(19)))/FONTMETRIC(6,&lcFormArr[lnArrElm,7],&lcFormArr[lnArrElm,9])*;
                 fontmetric(6,&lcFormArr[lnArrElm,7],&lcFormArr[lnArrElm,9])/fontmetric(6))
  
  lnObjEndC=lnObjStartC+MIN(lnwWidth-lnObjStartC+lnObjEndC,lnObjEndC)

  lnObjEndR=(&lcFormArr[lnArrElm,4]/(FONTMETRIC(20,&lcFormArr[lnArrElm,7],&lcFormArr[lnArrElm,9])+FONTMETRIC(20,&lcFormArr[lnArrElm,7],&lcFormArr[lnArrElm,9])/FONTMETRIC(20)))/FONTMETRIC(1,&lcFormArr[lnArrElm,7],&lcFormArr[lnArrElm,9])*;
    fontmetric(1,&lcFormArr[lnArrElm,7],&lcFormArr[lnArrElm,9])/fontmetric(1)
  
  lnObjEndR=lnObjStartR+MIN(ABS(lnWHeight-lnObjStartR+lnObjEndR),lnObjEndR)  
  
WAIT STR(lnObjStartR)+' '+  STR(lnObjStartc)+' '+ STR(lnObjEndR)+' '+STR(lnObjEndC)   window nowait
 llObjDisp= (lnObjStartR>=lnWStartRow+1 AND lnObjStartC>=lnWStartCol+1) OR ;
                 (lnObjEndR<=lnWEndRow-1 AND lnObjStartC>=lnWStartCol+1) OR;
                 (lnObjStartR>=lnWStartRow+1 AND lnObjEndC<=lnWEndCol-1) OR;
                 (lnObjEndR<=lnWEndRow-1 AND lnObjEndC<=lnWEndCol-1)
                
ELSE
  llObjDisp=((&lcFormArr[lnArrElm,2]/lnObjVDiv)+(&lcFormArr[lnArrElm,4]/lnObjVDiv)>=lnWStartRow+1 AND;
       (&lcFormArr[lnArrElm,2]/lnObjVDiv)<=lnWEndRow-1) AND;
       ((&lcFormArr[lnArrElm,3]/lnObjVDiv)+(&lcFormArr[lnArrElm,5]/lnObjVDiv)>=lnWStartCol+1 AND;
       (&lcFormArr[lnArrElm,3]/lnObjVDiv)<=lnWEndCol-1)   
ENDIF  
RETURN MDOWN() OR llObjDisp





*!********************************************************************
*!
*!              Function: lfScrollup
*!
*!********************************************************************
*
FUNCTION lfScrollUP
lnObjNo1=lfObjPos('IBSCRUP')
IF !MDOWN() OR !(BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
   AND BETWEEN(MCOL(),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4]))
  RETURN .F.
ENDIF
DO WHILE MDOWN() AND lnWvPos>0 AND BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
   AND BETWEEN(MCOL(),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4])
  lnWvPos = lnWvPos-1
  lnWvEnd=lnWvPos/lnObjVDiv+lnWvMax
   MOVE WINDOW (REPDISPOBJ) TO IIF(lnWVPos>0,-lnWvPos,0),IIF(lnWHPos>0,-lnWHPos,0)  
   =lfModiHScrol()   
ENDDO





*!********************************************************************
*!
*!              Function: lfScrollDn
*!
*!********************************************************************
*
FUNCTION lfScrollDn
lnObjNo1=lfObjPos('IBSCRDN')
IF !MDOWN() OR !(BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
   AND BETWEEN(MCOL(),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4]))
  RETURN .F.
ENDIF
DO WHILE MDOWN() AND lnWvEnd<lnwHeight AND BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
   AND BETWEEN(MCOL(),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4])
  lnWvPos = lnWvPos +1
  lnWvEnd=lnWvPos/lnObjVDiv+lnWvMax
   MOVE WINDOW (REPDISPOBJ) TO IIF(lnWVPos>0,-lnWvPos,0),IIF(lnWHPos>0,-lnWHPos,0)  
   =lfModiHScrol()
ENDDO



*!********************************************************************
*!
*!              Function: lfScrollLf
*!
*!********************************************************************
*
FUNCTION lfScrollLf
lnObjNo1=lfObjPos('IBSCRL')
IF !MDOWN() OR !(BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
   AND BETWEEN(MCOL(),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4]))
  RETURN .F.
ENDIF
DO WHILE MDOWN() AND lnWHPos>0 AND BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
   AND BETWEEN(MCOL(),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4])
  lnWHPos = lnWHPos-1
  lnWHEnd=lnWHPos/lnObjHDiv+lnWHMax
   MOVE WINDOW (REPDISPOBJ) TO IIF(lnWVPos>0,-lnWvPos,0),IIF(lnWHPos>0,-lnWHPos,0)  
   =lfModiWScrol()
ENDDO





*!********************************************************************
*!
*!              Function: lfScrollDn
*!
*!********************************************************************
*
FUNCTION lfScrollRg
lnObjNo1=lfObjPos('IBSCRRG')
IF !MDOWN() OR !(BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
   AND BETWEEN(MCOL(),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4]))
  RETURN .F.
ENDIF
DO WHILE MDOWN() AND lnwHpos+lnWHMax<lnwWidth AND BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
   AND BETWEEN(MCOL(),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4])
  lnWHPos = lnWHPos +IIF(lnwWidth-lnWHpos<1,lnwWidth-lnWHpos,1)
  lnWHEnd=lnWHPos/lnObjHDiv+lnWHMax
   MOVE WINDOW (REPDISPOBJ) TO IIF(lnWVPos>0,-lnWvPos,0),IIF(lnWHPos>0,-lnWHPos,0)  
     =lfModiWScrol()
ENDDO



*!********************************************************************
*!
*!              Function: lfScrollHor
*!
*!********************************************************************
*
FUNCTION lfScrollHor
lnObjNo1=lfObjPos('IBSCRWBAR')
IF !MDOWN() !(BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
   AND BETWEEN(MCOL("gwdscrollw"),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4]))
  RETURN .F.
ENDIF
lnCurCol=MCOL("gwdscrollw")
lnWCurPos=CEILING((MCOL("gwdscrollw")-laObjScrP[lnObjNo1,2])/((laObjScrP[lnObjNo1,4]-laObjScrP[lnObjNo1,2])/ ;
                 ((lnWWidth-lnWHMax))))
DO CASE 
  CASE IIF(_DOS,lnCurCol<>lnScrolCol,lnWCurPos<>lnWHPos)
    lnSign=IIF(lnWCurPos>lnWHPos,1,-1)                 
    DO WHILE MDOWN() 
      IF BETWEEN(MCOL("gwdscrollw"),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4]) AND;
         BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3])
        lnWCurPos=CEILING((MCOL("gwdscrollw")-laObjScrP[lnObjNo1,2])/((laObjScrP[lnObjNo1,4]-laObjScrP[lnObjNo1,2])/ ;
                  ((lnWWidth-lnWHMax))))             
        lnSign2=IIF(lnWCurPos>lnWHPos,1,-1)                             
       IF lnSign=lnSign2
         lnWHPos=lnWHpos+(lnSign*IIF(ABS(lnWCurPos-lnWHPos)>3,3,ABS(lnWCurPos-lnWHPos)))
         MOVE WINDOW (REPDISPOBJ) TO IIF(lnWVPos>0,-lnWvPos,0),IIF(lnWHPos>0,-lnWHPos,0)  
         =lfModiWScrol()        
        ENDIF  
     ENDIF  
       =INKEY(.01,'HM')
   ENDDO
 OTHERWISE
    lnTmpCol=lnScrolCol
    lnWCurPos=CEILING((MCOL("gwdscrollw")-laObjScrP[lnObjNo1,2])/((laObjScrP[lnObjNo1,4]-laObjScrP[lnObjNo1,2])/ ;
                  ((lnWWidth-lnWHMax))))             
    =lfDrawCurs()    
    DO WHILE MDOWN() 
      IF (BETWEEN(MCOL("gwdscrollw"),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4]))
        lnWCurPos=CEILING((MCOL("gwdscrollw")-laObjScrP[lnObjNo1,2])/((laObjScrP[lnObjNo1,4]-laObjScrP[lnObjNo1,2])/ ;
                  ((lnWWidth-lnWHMax))))             
         =lfDrawCurs()                                
         =INKEY(.01,'HM')         
     ENDIF  
       =INKEY(.01,'HM')     
   ENDDO    
     IF lnWHPos<>lnWcurPos
       lnWHPos=lnWCurPos
       lnScrolCol=lnTmpCol
       MOVE WINDOW (REPDISPOBJ) TO IIF(lnWVPos>0,-lnWvPos,0),IIF(lnWHPos>0,-lnWHPos,0)  
       =lfModiWScrol()    
     ENDIF      
ENDCASE   
RETURN .F.



*!********************************************************************
*!
*!              Function: lfScrollVer
*!
*!********************************************************************
*
FUNCTION lfScrollVer
lnObjNo1=lfObjPos('IBSCRHBAR')
IF !MDOWN() !(BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
   AND BETWEEN(MCOL("gwdFormscl"),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4]))
  RETURN .F.
ENDIF
lnCurCol=MROW("gwdFormscl")
lnWCurPos=CEILING((MROW("gwdFormscl")-laObjScrP[lnObjNo1,1])/((laObjScrP[lnObjNo1,3]-laObjScrP[lnObjNo1,1])/ ;
                 ((lnWHEIGHT-lnWVMax))))
DO CASE 
  CASE IIF(_DOS,lnCurCol<>lnScrolRow,lnWCurPos<>lnWVPos)
    lnSign=IIF(lnWCurPos>lnWVPos,1,-1)                 
    DO WHILE MDOWN() 
      IF BETWEEN(MCOL("gwdFormscl"),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4]) AND;
         BETWEEN(MROW("gwdFormscl"),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3])
        lnWCurPos=CEILING((MROW("gwdFormscl")-laObjScrP[lnObjNo1,1])/((laObjScrP[lnObjNo1,3]-laObjScrP[lnObjNo1,1])/ ;
                  ((lnWHEIGHT-lnWVMax))))             
        lnSign2=IIF(lnWCurPos>lnWVPos,1,-1)                             
       IF lnSign=lnSign2
         lnWVPos=lnWVpos+(lnSign*IIF(ABS(lnWCurPos-lnWVPos)>1,1,ABS(lnWCurPos-lnWVPos)))
         MOVE WINDOW (REPDISPOBJ) TO IIF(lnWVPos>0,-lnWvPos,0),IIF(lnWHPos>0,-lnWHPos,0)  
         =lfModiHScrol()        
        ENDIF  
     ENDIF  
       =INKEY(.01,'HM')
   ENDDO
 OTHERWISE
    lnTmpCol=lnScrolROW
    lnWCurPos=CEILING((MROW("gwdFormscl")-laObjScrP[lnObjNo1,1])/((laObjScrP[lnObjNo1,3]-laObjScrP[lnObjNo1,1])/ ;
                  ((lnWHEIGHT-lnWVMax))))             
    =lfVerCurs()                                
    DO WHILE MDOWN() 
      IF (BETWEEN(MROW("gwdFormscl"),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]))
        lnWCurPos=CEILING((MROW("gwdFormscl")-laObjScrP[lnObjNo1,1])/((laObjScrP[lnObjNo1,3]-laObjScrP[lnObjNo1,1])/ ;
                  ((lnWHEIGHT-lnWVMax))))             
         =lfVerCurs()                                
         =INKEY(.01,'HM')         
     ENDIF  
       =INKEY(.01,'HM')     
   ENDDO    
     IF lnWvPos<>lnWcurPos
       lnWvPos=lnWCurPos
       lnScrolRow=lnTmpCol
       MOVE WINDOW (REPDISPOBJ) TO IIF(lnWVPos>0,-lnWvPos,0),IIF(lnWHPos>0,-lnWHPos,0)  
       =lfModiHScrol()    
     ENDIF      
ENDCASE   
RETURN .F.



*!********************************************************************
*!
*!              Function: lfkScrollup
*!
*!********************************************************************
*
FUNCTION lfKScrollUP
PUSH KEY CLEAR
IF lnWvPos>0
  lnWvPos = lnWvPos-1
  lnWvEnd=lnWvPos/lnObjVDiv+lnWvMax
   MOVE WINDOW (REPDISPOBJ) TO IIF(lnWVPos>0,-lnWvPos,0),IIF(lnWHPos>0,-lnWHPos,0)  
   =lfModiHScrol()
ENDIF
POP KEY





*!********************************************************************
*!
*!              Function: lfkScrollDn
*!
*!********************************************************************
*
FUNCTION lfKScrollDn
PUSH KEY CLEAR
IF lnWvEnd<lnwHeight
  lnWvPos = lnWvPos +1
  lnWvEnd=lnWvPos/lnObjVDiv+lnWvMax
   MOVE WINDOW (REPDISPOBJ) TO IIF(lnWVPos>0,-lnWvPos,0),IIF(lnWHPos>0,-lnWHPos,0)  
   =lfModiHScrol()
ENDIF
POP KEY



*!********************************************************************
*!
*!              Function: lfkScrollL
*!
*!********************************************************************
*
FUNCTION lfKScrollL
PUSH KEY CLEAR
IF lnWHPos>0
  lnWHPos = lnWHPos-1
  lnWHEnd=lnWHPos/lnObjHDiv+lnWHMax
   MOVE WINDOW (REPDISPOBJ) TO IIF(lnWVPos>0,-lnWvPos,0),IIF(lnWHPos>0,-lnWHPos,0)  
   =lfModiWScrol()
ENDIF
POP KEY




*!********************************************************************
*!
*!              Function: lfkScrollR
*!
*!********************************************************************
*
FUNCTION lfKScrollR
PUSH KEY CLEAR
IF lnwHpos+lnWHMax<lnwWidth
  lnWHPos = lnWHPos +1
  lnWHEnd=lnWHPos/lnObjhDiv+lnWHMax
*    lnWHEnd=lnWHPos+lnWHMax
   MOVE WINDOW (REPDISPOBJ) TO IIF(lnWVPos>0,-lnWvPos,0),IIF(lnWHPos>0,-lnWHPos,0)  
     =lfModiWScrol()
ENDIF
POP KEY


*********************************************************************

*!********************************************************************
*!
*!              Function: lfModiWScrol
*!
*!********************************************************************
*
FUNCTION lfModiWScrol
  lnObjPos=lfObjPos('IBSCRWBAR')
  DO CASE
    CASE _DOS   
      lnPos=lnScrolCol
      lnScrolCol=MIN(laObjScrP[lnObjPos,4],MAX(laObjScrP[lnObjPos,2],FLOOR((laObjScrP[lnObjPos,4]-laObjScrP[lnObjPos,2]-1)/ ;
                 ((lnWWidth-lnWHMax)-1)*(lnWHPos-1)+laObjScrP[lnObjPos,2])))
       STORE WOUTPUT() TO currwind
*       IF SYS(2016) = "gwdscrollw" OR SYS(2016) = "*"
         ACTIVATE WINDOW gwdscrollw SAME
         @ laObjScrP[lnObjPos,1],lnPos SAY 'Û' COLOR &lcListColor
         @ laObjScrP[lnObjPos,1],lnScrolCol SAY '' COLOR &lcXorColor
*       ENDIF
       IF NOT EMPTY(currwind)
         ACTIVATE WINDOW (currwind) SAME 
       ENDIF
    CASE _WINDOWS
      lnPos=lnScrolCol
      lnScrolCol=MIN(laObjScrP[lnObjPos,4]-2.600,MAX(laObjScrP[lnObjPos,2]+0.1,FLOOR((laObjScrP[lnObjPos,4]-laObjScrP[lnObjPos,2]-1)/ ;
                 ((lnWWidth-lnWHMax)-1)*(lnWHPos-1)+laObjScrP[lnObjPos,2])))
     IF lnPos<>lnScrolCol
       STORE WOUTPUT() TO currwind
        ACTIVATE WINDOW gwdscrollw SAME
         @laObjScrP[lnObjPos,1]+0.1,lnPos CLEAR TO laObjScrP[lnObjPos,3]-0.144,lnPos+2.600         
         @ laObjScrP[lnObjPos,3]-0.300,lnScrolCol TO laObjScrP[lnObjPos,3]-0.300,lnScrolCol+2.250;
         PEN 2, 8 ;
         STYLE "1";
         COLOR RGB(128,128,128,128,128,128)

         @ laObjScrP[lnObjPos,1]+0.1,lnScrolCol TO laObjScrP[lnObjPos,1]+0.1,lnScrolCol+2.250;                  
         PEN 2, 8 ;
         STYLE "1";
         COLOR RGB(255,255,255,255,255,255)		           	         
         
        @  laObjScrP[lnObjPos,1]+0.1,lnScrolCol+2.250 TO laObjScrP[lnObjPos,3]-0.200,lnScrolCol+2.250;       
         PEN 2, 8 ;
         STYLE "1";
         COLOR RGB(128,128,128,128,128,128)

        @  laObjScrP[lnObjPos,1]+0.1,lnScrolCol TO laObjScrP[lnObjPos,3]-0.300,lnScrolCol;       
         PEN 2, 8 ;
         STYLE "1";
         COLOR RGB(255,255,255,255,255,255)		           	         
       IF NOT EMPTY(currwind)
         ACTIVATE WINDOW (currwind) SAME 
       ENDIF
    ENDIF 
  ENDCASE        


*!********************************************************************
*!
*!              Function: lfModiHScrol
*!
*!********************************************************************
*
FUNCTION lfModiHScrol
  lnObjPos=lfObjPos('IBSCRHBAR')
  DO CASE
    CASE _DOS   
      lnPos=lnScrolRow
      lnScrolRow=MAX(laObjScrP[lnObjPos,1],FLOOR((laObjScrP[lnObjPos,3]-laObjScrP[lnObjPos,1]-1)/ ;
                 ((lnWHeight-lnWVMax))*(lnWVPos)+laObjScrP[lnObjPos,1]))
     IF lnPos<>lnScrolRow
       STORE WOUTPUT() TO currwind
*       IF SYS(2016) = "gwdFormscl" OR SYS(2016) = "*"
         ACTIVATE WINDOW gwdFormscl SAME
         @ lnPos,laObjScrP[lnObjPos,2] SAY 'Û' COLOR &lcListColor
         @ lnScrolRow,laObjScrP[lnObjPos,2] SAY '' COLOR &lcXorColor
*       ENDIF
       IF NOT EMPTY(currwind)
         ACTIVATE WINDOW (currwind) SAME 
       ENDIF
    ENDIF 
     =INKEY(.01,'HM')        
    CASE _WINDOWS
     lnPos=lnScrolRow
      lnScrolRow=MAX(laObjScrP[lnObjPos,1],MIN(laObjScrP[lnObjPos,3]-1.00,;
                 FLOOR((laObjScrP[lnObjPos,3]-laObjScrP[lnObjPos,1]-1)/ ;
                 ((lnWHeight-lnWVMax)-1)*(lnWVPos-1)+laObjScrP[lnObjPos,1])))
                 
*      MIN(laObjScrP[lnObjPos,1],MAX(laObjScrP[lnObjPos,3]-1.00,;
                 FLOOR((laObjScrP[lnObjPos,3]-laObjScrP[lnObjPos,1]-1)/ ;
                 ((lnWHeight-lnWVMax)-1)*(lnWVPos-1)+laObjScrP[lnObjPos,1])))
     IF lnPos<>lnScrolRow
       STORE WOUTPUT() TO currwind
*       IF SYS(2016) = "gwdFormscl" OR SYS(2016) = "*"
         ACTIVATE WINDOW gwdFormscl SAME
         @ lnPos,laObjScrP[lnObjPos,2] CLEAR TO lnPos+1.415,laObjScrP[lnObjPos,4]-0.088         
         @ lnScrolRow,laObjScrP[lnObjPos,2] TO lnScrolRow+1.00,laObjScrP[lnObjPos,2];
         PEN 2, 8 ;
         STYLE "1";
         COLOR RGB(255,255,255,255,255,255)		           	
                  
         @ lnScrolRow,laObjScrP[lnObjPos,2] TO lnScrolRow,laObjScrP[lnObjPos,4]-0.500;
         PEN 2, 8 ;
         STYLE "1";
         COLOR RGB(255,255,255,255,255,255)		           	         
         
         @ lnScrolRow+1.00,laObjScrP[lnObjPos,2] TO lnScrolRow+1.00,laObjScrP[lnObjPos,4]-0.500;
         PEN 2, 8 ;
         STYLE "1";
         COLOR RGB(128,128,128,128,128,128)
         
         @ lnScrolRow,laObjScrP[lnObjPos,4]-0.500 TO lnScrolRow+1.00,laObjScrP[lnObjPos,4]-0.500;
         PEN 2, 8 ;
         STYLE "1";
         COLOR RGB(128,128,128,128,128,128)		  
		  
 *      ENDIF
       IF NOT EMPTY(currwind)
         ACTIVATE WINDOW (currwind) SAME 
       ENDIF
    ENDIF 
  ENDCASE        


*!*************************************************************************
*!
*!              Function: lfObjPos
*!
*!*************************************************************************
*  This function to return an object position in the array that defined in
* the screen layout 'setup' to hold the position of the screen objects and
* the names to and this function is to return the row # that have the data
* of this object
FUNCTION lfObjPos
PARAMETERS lcOGObjNam
PRIVATE lnCount
llOGFound=.f.
FOR lnCount=1 TO ALEN(laObjScrP,1)
  IF UPPER(laObjScrP[lnCount,5])=UPPER(lcOGObjNam)
    llOGFound=.t.
    EXIT  
  ENDIF
ENDFOR
RETURN IIF(llOGFound,lnCount,0)





FUNCTION lfDrawCurs
  lnObjPos=lfObjPos('IBSCRWBAR')
  DO CASE
    CASE _DOS   
      lnPos=lnScrolCol
      lnScrolCol=MIN(laObjScrP[lnObjPos,4],MAX(laObjScrP[lnObjPos,2],FLOOR((laObjScrP[lnObjPos,4]-laObjScrP[lnObjPos,2]-1)/ ;
                 ((lnWWidth-lnWHMax)-1)*(lnWCurPos-1)+laObjScrP[lnObjPos,2])))
     IF lnPos<>lnScrolCol
       STORE WOUTPUT() TO currwind
         ACTIVATE WINDOW gwdscrollw SAME
         @ laObjScrP[lnObjPos,1],lnPos SAY 'Û' COLOR &lcListColor                 
         @ laObjScrP[lnObjPos,1],lnScrolCol SAY '' COLOR &lcXorColor
       IF NOT EMPTY(currwind)
         ACTIVATE WINDOW (currwind) SAME 
       ENDIF
    ENDIF 
    CASE _WINDOWS
      lnPos=lnScrolCol
       lnScrolCol=MIN(laObjScrP[lnObjPos,4]-2.600,MAX(laObjScrP[lnObjPos,2]+0.1,FLOOR((laObjScrP[lnObjPos,4]-laObjScrP[lnObjPos,2]-1)/ ;
                 ((lnWWidth-lnWHMax)-1)*(lnWCurPos-1)+laObjScrP[lnObjPos,2])))
     IF lnPos<>lnScrolCol                 
       STORE WOUTPUT() TO currwind
        ACTIVATE WINDOW gwdscrollw SAME
         @laObjScrP[lnObjPos,1]+0.1,lnPos TO laObjScrP[lnObjPos,3]-0.300,lnPos+2.250 ;
         	PEN 1, 1 ;
         	STYLE "T";
         	COLOR RGB(192,192,192,,,,)
          @laObjScrP[lnObjPos,1]+0.1,lnScrolCol TO laObjScrP[lnObjPos,3]-0.300,lnScrolCol+2.250 ;
         	PEN 1, 1 ;
         	STYLE "T";
         	COLOR RGB(255,255,255,,,,)         	        
       IF NOT EMPTY(currwind)
         ACTIVATE WINDOW (currwind) SAME 
       ENDIF
     ENDIF  
  ENDCASE        



                                
         
         
         
*!********************************************************************
*!
*!              Function: lfVerCurs
*!
*!********************************************************************
*
FUNCTION lfVerCurs
  lnObjPos=lfObjPos('IBSCRHBAR')
  DO CASE
    CASE _DOS   
      lnPos=lnScrolRow
      lnScrolRow=MAX(laObjScrP[lnObjPos,1],FLOOR((laObjScrP[lnObjPos,3]-laObjScrP[lnObjPos,1]-1)/ ;
                 ((lnWHeight-lnWVMax))*(lnWCurPos)+laObjScrP[lnObjPos,1]))
     IF lnPos<>lnScrolRow
       STORE WOUTPUT() TO currwind
*       IF SYS(2016) = "gwdFormscl" OR SYS(2016) = "*"
         ACTIVATE WINDOW gwdFormscl SAME
         @ lnPos,laObjScrP[lnObjPos,2] SAY 'Û' COLOR &lcListColor
         @ lnScrolRow,laObjScrP[lnObjPos,2] SAY '' COLOR &lcXorColor
*       ENDIF
       IF NOT EMPTY(currwind)
         ACTIVATE WINDOW (currwind) SAME 
       ENDIF
    ENDIF 
     =INKEY(.01,'HM')        
    CASE _WINDOWS
     lnPos=lnScrolRow
      lnScrolRow=MAX(laObjScrP[lnObjPos,1],MIN(laObjScrP[lnObjPos,3]-1.00,;
                 FLOOR((laObjScrP[lnObjPos,3]-laObjScrP[lnObjPos,1]-1)/ ;
                 ((lnWHeight-lnWVMax)-1)*(lnWCurPos-1)+laObjScrP[lnObjPos,1])))
                 
     IF lnPos<>lnScrolRow
       STORE WOUTPUT() TO currwind
         ACTIVATE WINDOW gwdFormscl SAME

         @ lnPos,laObjScrP[lnObjPos,2] TO lnPos+1.00,laObjScrP[lnObjPos,4]-0.500;
         	PEN 1, 1 ;
         	STYLE "T";
         	COLOR RGB(192,192,192,,,,)
         	
         @ lnScrolRow,laObjScrP[lnObjPos,2] TO lnScrolRow+1.00,laObjScrP[lnObjPos,4]-0.500;
         	PEN 1, 1 ;
         	STYLE "T";
         	COLOR RGB(255,255,255,,,,)         	    	
         	                  
       IF NOT EMPTY(currwind)
         ACTIVATE WINDOW (currwind) SAME 
       ENDIF
    ENDIF 
  ENDCASE        
         


FUNCTION lfGetMaxPos
FOR lnCount= 1 TO ALEN(&lcFormArr,1)
  IF &lcFormCnt[lnCount]
    lnMaxPosV=MAX(&lcFormArr[lnCount,2]+&lcFormArr[lnCount,4],lnMaxPosV)
    lnMaxPosH=MAX(&lcFormArr[lnCount,3]+&lcFormArr[lnCount,5],lnMaxPosH)
  ENDIF
ENDFOR
RETURN
