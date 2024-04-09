*:***************************************************************************
*: Program file  : SOSTYCUS.PRG      (SOSTYCUS.FRX)
  *: Program desc. : CUSTOMIZED SALES ORDER WITH CUSTOMER FOR ERIC.
*: Date          : 08/12/2007
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : Mariam Mazhar[MMT]
*: Tracking Job Number: (2.7 --> C126843) (Aria4 --> C200826)
*:
*:***************************************************************************
*: Calls : 
*:    Procedures :
*:    Functions  :
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   :
*:***************************************************************************
*: Example : DO SOSTYCUS
*:***************************************************************************
*: Modification:
*****************************************************************************
STORE SPACE(0) TO lcFilter , lcRpColor , lcRpStatus

loogscroll.cCROrientation = 'P'

IF loogscroll.llOgFltCh
  =lfCreatTmp()
  =lfvCrATVar()
  =lfColctDat()
ENDIF

SELECT (lclines)
SET FILTER TO EVAL(lclines+'.TOTOPEN') # 0
LOCATE

DO gfDispRe WITH "SOSTYCUS" , 'FOR ' + "&lclines..TotOpen <> 0 "
SELECT (lclines)
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : Mariam Mazhar[MMT]
*: Date          : 08/12/2007
*! Purpose   : Function to create the temp. file hold the data.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCreatTmp

DIMENSION laFileStruct[6,4]

laFileStruct[1,1]= "Account"
laFileStruct[1,2]= "C"
laFileStruct[1,3]= 5
laFileStruct[1,4]= 0


laFileStruct[2,1]= "TotOpen"
laFileStruct[2,2]= "N"
laFileStruct[2,3]= 7
laFileStruct[2,4]= 0

laFileStruct[3,1]= "StyDesc"
laFileStruct[3,2]= "C"
laFileStruct[3,3]= 60
laFileStruct[3,4]= 0


laFileStruct[4,1]= "llPrnTot"
laFileStruct[4,2]= "L"
laFileStruct[4,3]= 1
laFileStruct[4,4]= 0

laFileStruct[5,1]= "Color"
laFileStruct[5,2]= "C"
laFileStruct[5,3]= lnClrLnGl
laFileStruct[5,4]= 0

laFileStruct[6,1]= "Style"
laFileStruct[6,2]= "C"
laFileStruct[6,3]= lnStyLnGl
laFileStruct[6,4]= 0

gfCrtTmp(lclines ,@laFileStruct,"Style + Color + Account",lclines)
*-- End of lfCreatTmp.
*!*************************************************************
*!*************************************************************
*! Name      : lfvCrATVar
*: Developer     : Mariam Mazhar[MMT]
*: Date          : 08/12/2007
*! Purpose   : Function to prepare the variables holds the collection
*!           : Criteria.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvCrATVar

lcFilter = IIF(!EMPTY(LCRPEXP),LCRPEXP,'.T.')

*--The Colors.
FOR lnClr = 1 TO ALEN(laRpTColor)
  lcRpColor = lcRpColor + PADR(laRpTColor[lnClr],lnClrLnGl) + ' | '
ENDFOR
lcRpColor = IIF(ALLTRIM(lcRpColor) = '|','',lcRpColor)

*--The Status.
FOR lnI = 1 TO ALEN(laRpTStats,1)
  DO CASE
    CASE laRpTStats[lnI] == "Open"
      laRpTStats[lnI] = "O"
    CASE laRpTStats[lnI] == "Hold"
      laRpTStats[lnI] = "H"
    CASE laRpTStats[lnI] == "Canceled"
      laRpTStats[lnI] = "X"
    CASE laRpTStats[lnI] == "Completed"
      laRpTStats[lnI] = "C"
  ENDCASE
ENDFOR

FOR lnStats = 1 TO ALEN(laRpTStats,1)
  lcRpStatus = lcRpStatus + laRpTStats[lnStats] + ','
ENDFOR
lcRpStatus = IIF(ALLTRIM(lcRpStatus) = "," , '' , lcRpStatus)

*--Return the original values.
FOR lnI = 1 TO ALEN(laRpTStats,1)
  DO CASE
    CASE laRpTStats[lnI] == "O"
      laRpTStats[lnI] = "Open"
    CASE laRpTStats[lnI] == "H"
      laRpTStats[lnI] = "Hold"
    CASE laRpTStats[lnI] == "X"
      laRpTStats[lnI] = "Canceled"
    CASE laRpTStats[lnI] == "C"
      laRpTStats[lnI] = "Completed"
  ENDCASE
ENDFOR

IF !EMPTY(lcRpStatus)
  lcFilter = lcFilter + ' AND ORDHDR.STATUS $ lcRpStatus'
ENDIF


*!*************************************************************
*! Name      : lfcolctDat
*: Developer     : Mariam Mazhar[MMT]
*: Date          : 08/12/2007
*! Purpose   : Function to Collect the date from the ordline file.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfcolctDat
PRIVATE lcAlias , lcAccount , lcTagStyl
lcAlias = ALIAS()



*Style
llStyle = .F.
lcStyleFile = ''
lnClrSgPo = 0
IF ASCAN(loogscroll.laOGFXFlt,'STYLE.CSTYMAJOR') > 0
  lnClrSgPo = ASUBSCRIPT(loogscroll.laOGFXFlt,;
    ASCAN(loogscroll.laOGFXFlt,'STYLE.CSTYMAJOR'),1)
  
  lcStyleFile = IIF(!EMPTY(loogscroll.laOGFXFlt[lnClrSgPo,6]),loogscroll.laOGFXFlt[lnClrSgPo,6],'')  
  IF !EMPTY(lcStyleFile) AND USED(lcStyleFile)
    SELECT(lcStyleFile)
    LOCATE 
    IF !EOF()
      llStyle = .T.
    ENDIF 
  ENDIF 
ENDIF 

*Account
llAcc = .F.
lcAccFile = ''
lnClrSgPo = 0
IF ASCAN(loogscroll.laOGFXFlt,'ORDHDR.ACCOUNT') > 0
  lnClrSgPo = ASUBSCRIPT(loogscroll.laOGFXFlt,;
    ASCAN(loogscroll.laOGFXFlt,'ORDHDR.ACCOUNT'),1)
  lcAccFile = IIF(!EMPTY(loogscroll.laOGFXFlt[lnClrSgPo,6]),loogscroll.laOGFXFlt[lnClrSgPo,6],'')  
  IF !EMPTY(lcAccFile ) AND USED(lcAccFile)
    SELECT(lcAccFile)
    LOCATE 
    IF !EOF()
      llAcc= .T.
    ENDIF 
  ENDIF 
ENDIF 


*Color
llUseClr1  = .F.
lnClr1Pos = 0
lnClr1Pos = ASCAN(loOgScroll.laOgFXFlt,"STYLE.COLOR")
IF lnClr1Pos > 0 
  lnClr1Pos  = ASUBSCRIPT(loOgScroll.laOgFXFlt,lnClr1Pos,1)
  lcClr1Sel =IIF(!EMPTY(loOgScroll.laOgFXFlt[lnClr1Pos ,6]),loOgScroll.laOgFXFlt[lnClr1Pos,6],'')
  IF !EMPTY(lcClr1Sel ) 
    lcClr1File = loOGScroll.gfTempName()
    llUseClr1= IIF(LEN(lcClr1Sel)>0,.T.,.F.) AND lfConvertToCursor(lcClr1Sel,'CSTYCLR',lcClr1File)
  ENDIF   
ENDIF   

*Start date
ldStart = DTOC({})
ldEnd = DTOC({})
lnPosDate = ASCAN(loOGScroll.laogFXflt,'ORDHDR.START')
IF lnPosDate  <> 0 
  lnPosDate = ASUBSCRIPT(loOGScroll.laogFXflt,lnPosDate ,1)
  ldStart = IIF(EMPTY(SUBSTR(loOGScroll.laogFXflt[lnPosDate ,6],1,10)),DTOC({}),SUBSTR(loOGScroll.laogFXflt[lnPosDate ,6],1,10))
  ldEnd   = IIF(EMPTY(SUBSTR(loOGScroll.laogFXflt[lnPosDate ,6],12,21)),DTOC({}),SUBSTR(loOGScroll.laogFXflt[lnPosDate ,6],12,21))
ENDIF  


IF llStyle 
  SELECT Style 
  =gfSetOrder('Style')
  SELECT(lcStyleFile)
  SCAN 
    IF gfSeek(ALLTRIM(&lcStyleFile..cstymajor),'Style','Style')  
      SELECT Style 
*      WAIT WINDOW 'Selecting Records For The Report ...' + ALLTRIM(STYLE.CSTYMAJOR) NOWAIT
      SCAN REST WHILE STYLE = ALLTRIM(&lcStyleFile..cstymajor) FOR IIF(llUseClr1,SEEK(SUBSTR(STYLE.STYLE,lnClrPosGl,lnClrLnGl),lcClr1File),.T.)
        SELECT Ordline
        IF gfseek(Style.Style)
          SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = Style.Style ;
               FOR gfSeek('O'+ordline.order,'ordhdr') AND IIF(!EMPTY(lcRpStatus),ordhdr.status $ lcRpStatus,.T.) AND IIF(llAcc ,SEEK(ordhdr.account,lcAccFile),.T.) AND ;
	               IIF(!EMPTY(CTOD(ldStart)) AND !EMPTY(CTOD(ldEnd)),BETWEEN(Ordhdr.Start,CTOD(ldStart),CTOD(ldEnd)),.T.) 

           WAIT WINDOW 'Selecting Records For The Report ...' + ALLTRIM(SUBSTR(ORDLINE.STYLE,lnStyPosGl,lnStyLnGl)) NOWAIT                 
            lcValuSeek = SUBSTR(ORDLINE.STYLE,lnStyPosGl,lnStyLnGl) + SUBSTR(ORDLINE.STYLE,lnClrPosGl,lnClrLnGl) + ORDLINE.ACCOUNT
            =gfseek(ordline.style,'Style','Style')
            IF ORDHDR.STATUS ='C'
        		  IF SEEK(lcValuSeek,lclines)
	              REPLACE &lclines..TotOpen WITH &lclines..TotOpen  + ORDLINE.TotBook
	            ELSE
    	          SELECT (lclines)
        	      APPEND BLANK
              	REPLACE &lclines..Account WITH ORDLINE.ACCOUNT                            ,;
	                      &lclines..TotOpen WITH ORDLINE.TotBook                            ,;
        		            &lclines..StyDesc WITH STYLE.DESC1                                ,;
		                    &lclines..Color   WITH SUBSTR(ORDLINE.STYLE,lnClrPosGl,lnClrLnGl) ,;
                        &lclines..Style   WITH SUBSTR(ORDLINE.STYLE,lnStyPosGl,lnStyLnGl)
              ENDIF
            ELSE
              IF SEEK(lcValuSeek,lclines)
                REPLACE &lclines..TotOpen WITH &lclines..TotOpen  + ORDLINE.TOTQTY
              ELSE
                SELECT (lclines)
                APPEND BLANK
                REPLACE &lclines..Account WITH ORDLINE.ACCOUNT                            ,;
	                      &lclines..TotOpen WITH ORDLINE.TOTQTY                             ,;
		                    &lclines..StyDesc WITH STYLE.DESC1                                ,;
        		            &lclines..Color   WITH SUBSTR(ORDLINE.STYLE,lnClrPosGl,lnClrLnGl) ,;
                        &lclines..Style   WITH SUBSTR(ORDLINE.STYLE,lnStyPosGl,lnStyLnGl)
              ENDIF
            ENDIF  
          ENDSCAN
        ENDIF 
      ENDSCAN
    ENDIF
  ENDSCAN 
ELSE
  IF llAcc 
    SELECT(lcAccFile)    
    SCAN 
      SELECT ordline
      gfSetOrder('ORDLACC')
      IF gfseek(&lcAccFile..Account,'ordline')
        SELECT Ordline
        SCAN REST WHILE ACCOUNT+STORE+ORDER+STR(LINENO,6) =  &lcAccFile..Account;
             FOR IIF(llUseClr1,SEEK(SUBSTR(ordline.STYLE,lnClrPosGl,lnClrLnGl),lcClr1File),.T.) AND;
                 gfSeek('O'+ordline.order,'ordhdr') AND IIF(!EMPTY(lcRpStatus),ordhdr.status $ lcRpStatus,.T.) ;
                 AND IIF(!EMPTY(CTOD(ldStart)) AND !EMPTY(CTOD(ldEnd)),BETWEEN(Ordhdr.Start,CTOD(ldStart),CTOD(ldEnd)),.T.) 
                 
           WAIT WINDOW 'Selecting Records For The Report ...' + ALLTRIM(SUBSTR(ORDLINE.STYLE,lnStyPosGl,lnStyLnGl)) NOWAIT                                  
           lcValuSeek = SUBSTR(ORDLINE.STYLE,lnStyPosGl,lnStyLnGl) + SUBSTR(ORDLINE.STYLE,lnClrPosGl,lnClrLnGl) + ORDLINE.ACCOUNT
           =gfseek(ordline.style,'Style','Style')
            IF ORDHDR.STATUS ='C'
              IF SEEK(lcValuSeek,lclines)
                REPLACE &lclines..TotOpen WITH &lclines..TotOpen  + ORDLINE.TotBook
              ELSE
                SELECT (lclines)
                APPEND BLANK
                REPLACE &lclines..Account WITH ORDLINE.ACCOUNT                            ,;
                        &lclines..TotOpen WITH ORDLINE.TotBook                            ,;
                        &lclines..StyDesc WITH STYLE.DESC1                                ,;
                        &lclines..Color   WITH SUBSTR(ORDLINE.STYLE,lnClrPosGl,lnClrLnGl) ,;
                        &lclines..Style   WITH SUBSTR(ORDLINE.STYLE,lnStyPosGl,lnStyLnGl)
              ENDIF
            ELSE
              IF SEEK(lcValuSeek,lclines)
                REPLACE &lclines..TotOpen WITH &lclines..TotOpen  + ORDLINE.TOTQTY
              ELSE
                SELECT (lclines)
                APPEND BLANK
                REPLACE &lclines..Account WITH ORDLINE.ACCOUNT                            ,;
                        &lclines..TotOpen WITH ORDLINE.TOTQTY                             ,;
                        &lclines..StyDesc WITH STYLE.DESC1                                ,;
                        &lclines..Color   WITH SUBSTR(ORDLINE.STYLE,lnClrPosGl,lnClrLnGl) ,;
                        &lclines..Style   WITH SUBSTR(ORDLINE.STYLE,lnStyPosGl,lnStyLnGl)
              ENDIF
            ENDIF  
        ENDSCAN         
      ENDIF 
    ENDSCAN
  ELSE
    SELECT ordline
    gfSetOrder('ORDLINE')
    =gfSeek('O')  
    SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O' FOR IIF(llUseClr1,SEEK(SUBSTR(ordline.STYLE,lnClrPosGl,lnClrLnGl),lcClr1File),.T.) AND;
                 gfSeek('O'+ordline.order,'ordhdr') AND IIF(!EMPTY(lcRpStatus),ordhdr.status $ lcRpStatus,.T.) ;
                 AND IIF(!EMPTY(CTOD(ldStart)) AND !EMPTY(CTOD(ldEnd)),BETWEEN(Ordhdr.Start,CTOD(ldStart),CTOD(ldEnd)),.T.) 
            
            WAIT WINDOW 'Selecting Records For The Report ...' + ALLTRIM(SUBSTR(ORDLINE.STYLE,lnStyPosGl,lnStyLnGl)) NOWAIT                 
            lcValuSeek = SUBSTR(ORDLINE.STYLE,lnStyPosGl,lnStyLnGl) + SUBSTR(ORDLINE.STYLE,lnClrPosGl,lnClrLnGl) + ORDLINE.ACCOUNT
            =gfseek(ordline.style,'Style','Style')
            IF ORDHDR.STATUS ='C'
              IF SEEK(lcValuSeek,lclines)
                REPLACE &lclines..TotOpen WITH &lclines..TotOpen  + ORDLINE.TotBook
              ELSE
                SELECT (lclines)
                APPEND BLANK
                REPLACE &lclines..Account WITH ORDLINE.ACCOUNT                            ,;
                        &lclines..TotOpen WITH ORDLINE.TotBook                            ,;
                        &lclines..StyDesc WITH STYLE.DESC1                                ,;
                        &lclines..Color   WITH SUBSTR(ORDLINE.STYLE,lnClrPosGl,lnClrLnGl) ,;
                        &lclines..Style   WITH SUBSTR(ORDLINE.STYLE,lnStyPosGl,lnStyLnGl)
              ENDIF
            ELSE
              IF SEEK(lcValuSeek,lclines)
                REPLACE &lclines..TotOpen WITH &lclines..TotOpen  + ORDLINE.TOTQTY
              ELSE
                SELECT (lclines)
                APPEND BLANK
                REPLACE &lclines..Account WITH ORDLINE.ACCOUNT                            ,;
                        &lclines..TotOpen WITH ORDLINE.TOTQTY                             ,;
                        &lclines..StyDesc WITH STYLE.DESC1                                ,;
                        &lclines..Color   WITH SUBSTR(ORDLINE.STYLE,lnClrPosGl,lnClrLnGl) ,;
                        &lclines..Style   WITH SUBSTR(ORDLINE.STYLE,lnStyPosGl,lnStyLnGl)
              ENDIF
            ENDIF  
    ENDSCAN
  ENDIF 
ENDIF
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfwRepWhen
*: Developer     : Mariam Mazhar[MMT]
*! Date      : 26/04/2005
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

IF EMPTY(laRpSStats)
  DECLARE laRpSStats[4],laRpTStats[1,1] , laRpSColor[1,1],laRpTColor[1,1]

  STORE '' TO laRpTStats , laRpTColor
  STORE 'Open'      TO laRpSStats[1]
  STORE 'Hold'      TO laRpSStats[2]
  STORE 'Canceled'  TO laRpSStats[3]
  STORE 'Completed' TO laRpSStats[4]
ENDIF

=gfOpenTable(oAriaApplication.DataDir+'ORDLINE',oAriaApplication.DataDir+'ORDLINE','SH')
=gfOpenTable(oAriaApplication.DataDir+'ORDHDR',oAriaApplication.DataDir+'ORDHDR','SH')
=gfOpenTable(oAriaApplication.DataDir+'Style',oAriaApplication.DataDir+'Style','SH')


*!*************************************************************
*! Name      : lfChkStrct
*: Developer     : Mariam Mazhar[MMT]
*: Date          : 08/12/2007
*! Purpose   : Get the Style and Color Length.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfChkStrct()
*!*************************************************************
FUNCTION lfChkStrct

*--THE COLOR LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnGl  = LEN(laItemSeg[lnCount,3])
    lnClrPosGL = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lnStyLnGl  = LEN(laItemSeg[lnCount,3])
    lnStyPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE SCALE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='S'
    lnScaLnGl  = LEN(laItemSeg[lnCount,3])
    lnScaPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
*!*************************************************************
*! Name      : lfsrAcc
*: Developer     : Mariam Mazhar[MMT]
*: Date          : 08/12/2007
*! Purpose   : Rise change account flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm

IF lcParm = 'S'
  GO TOP IN CUSTOMER
ENDIF

*!**************************************************************************
*! Name      : lfSetSTY
*: Developer     : Mariam Mazhar[MMT]
*: Date          : 08/12/2007
*! Purpose   : Go top in the style IN RANGE
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSetSty()
*!**************************************************************************
FUNCTION lfSetSty
PARAMETERS OpGrdParm

IF OpGrdParm = 'S'
  SET ORDER TO TAG CSTYLE IN STYLE
  GO TOP IN STYLE
ENDIF
*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Mariam Mazhar[MMT]
*: Date          : 08/12/2007
*! Purpose   : Show the mover with all the status to be select from it.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************
FUNCTION lfvOStatus

=lfOGMover(@laRpSStats,@laRpTStats,'Select Order Status',.T.,'')  && call mover function.

*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer     : Mariam Mazhar[MMT]
*: Date          : 08/12/2007
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName 

DO CASE 
CASE  ALLTRIM(lcFieldName) = 'CSTYCLR'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0
ENDCASE 
 = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1 
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"") 
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO 
  IF lnEnd = 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH lcValuesToConvert 
  ENDIF 
ENDIF 
RETURN .T.

*!*************************************************************
*! Name      : RefreshStatus
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 12/04/2003
*! Purpose   : Return the selected status in the ReadBox
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!***************************************************************************
*! Modification:
*!***************************************************************************
FUNCTION RefreshStatus
  LOCAL lcStatusStr, lnTarget
  lcStatusStr = ""
  IF !EMPTY(laRpTStats)
    FOR lnTarget = 1 TO ALEN(laRpTStats,1)
      lcStatusStr = lcStatusStr + ", " + laRpTStats[lnTarget]
    ENDFOR 
    lcStatusStr = SUBSTR(lcStatusStr,3)
  ENDIF   
  RETURN lcStatusStr
ENDFUNC 
