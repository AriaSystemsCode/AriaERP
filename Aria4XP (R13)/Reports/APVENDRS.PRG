*:***************************************************************************
*: Program file  : APVENDRS
*: Program desc. : Print Vendor Information
*: For Report    : (N000572)
*: System        : Aria Advantage Series.4XP
*: Module        : MA
*: Developer     : Mariam Mazhar[MMT]
*:***************************************************************************
*E302975,1 AP Conv.Proj. Attaching all files the phase to TMI 10/23/2011 
*:************************************************************************
IF looGscroll.llOGFltCh
  lfCrtTemp()
  lfCollectData()
ENDIF   

SELECT(lcReportFile)&&the file will print from it

IF RECCOUNT()=0   &&if the file is empty
*--no records to display
  =gfModalGen('TRM00052B34000','ALERT')
  RETURN .F.
ENDIF  &&endif the file is not empty
loogScroll.cCROrientation = 'L'
=gfDispRe ()
*
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/03/2007
*! Purpose   : Report When Function
*!*************************************************************
FUNCTION lfwRepWhen
= gfOpenTable(oAriaApplication.DataDir + 'APVENDOR' ,'Vencode','SH')

*!*************************************************************
*! Name      : lfCrtTemp
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/03/2007
*! Purpose   : Create temp. file
*!*************************************************************
FUNCTION lfCrtTemp
IF USED(lcReportFile)
  SELECT(lcReportFile)
  ZAP 
ENDIF 

SELECT APVENDOR
DIMENSION laFileStruct[1,18]
AFIELDS(laFileStruc)
=gfCrtTmp(lcReportFile,@laFileStruc,"CVENDCODE",lcReportFile,.T.)

*!*************************************************************
*! Name      : lfCollectData
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/03/2007
*! Purpose   : Collecting data
*!*************************************************************
FUNCTION lfCollectData

lnPos = ASCAN(loOGScroll.laogVRflt,'APVENDOR.CVENDCODE')
llSeleVend = .F.
lcSelected = ''
IF lnPos <> 0 
  lnPos = ASUBSCRIPT(loOGScroll.laogVRflt,lnPos,1)
  lcSelected = IIF(EMPTY(loOGScroll.laogVRflt[lnPos,6]),'',loOGScroll.laogVRflt[lnPos,6])
  IF !EMPTY(lcSelected) AND USED(lcSelected)
    SELECT (lcSelected)
    LOCATE 
    IF !EOF()
      llSeleVend = .T.
    ENDIF
  ENDIF 
ENDIF   

lnPosOurVend = ASCAN(loOGScroll.laogVRflt,'APVENDOR.CVENOURAC')
lcOurVend = ''
IF lnPosOurVend <> 0 
  lnPosOurVend = ASUBSCRIPT(loOGScroll.laogVRflt,lnPosOurVend,1)
  lcOurVend  =  loOGScroll.laogVRflt[lnPosOurVend,6]
ENDIF 

lnPosTAxVend = ASCAN(loOGScroll.laogVRflt,'APVENDOR.CVENTAXID')
lcTaxVend = ''
IF lnPosTAxVend <> 0 
  lnPosTAxVend = ASUBSCRIPT(loOGScroll.laogVRflt,lnPosTAxVend,1)
  lcTaxVend    =  loOGScroll.laogVRflt[lnPosTAxVend,6]
ENDIF 

*E302975,1 Vend. Summary/History/Detail TMI 10/20/2011 [Start] 
lcRpExp = STRTRAN(lcRpExp,"  "," ")
=lfPolishExp(@lcRpExp,"(APVENDOR.CVENDCODE")
=lfReplExp(@lcRpExp,"AND APVENDOR.CVENOURAC","")
=lfReplExp(@lcRpExp,"AND APVENDOR.CVENTAXID","")
=lfReplExp(@lcRpExp,"APVENDOR.CVENDCODE = apvendor.cvendcode",".T.")
lcRpExp = IIF(!EMPTY(lcRpExp),' AND '+lcRpExp,'')
*E302975,1 Vend. Summary/History/Detail TMI 10/20/2011 [End  ] 

IF llSeleVend 
  SELECT (lcSelected)
  SCAN
    IF gfSeek(CVENDCODE,'APVENDOR') 
      SELECT APVENDOR
      *E302975,1 Vend. Summary/History/Detail TMI 10/20/2011 [Start] 
      *SCAN REST WHILE CVENDCODE =&lcSelected..CVENDCODE FOR IIF(!EMPTY(lcTaxVend),APVENDOR.CVENTAXID=lcTaxVend,.T.);
        AND IIF(!EMPTY(lcOurVend),APVENDOR.CVENOURAC = lcOurVend ,.T.)
      SCAN REST WHILE CVENDCODE =&lcSelected..CVENDCODE FOR IIF(!EMPTY(lcTaxVend),APVENDOR.CVENTAXID=lcTaxVend,.T.);
        AND IIF(!EMPTY(lcOurVend),APVENDOR.CVENOURAC = lcOurVend ,.T.) ;
        &lcRpExp
        *E302975,1 Vend. Summary/History/Detail TMI 10/20/2011 [End  ] 
        SCATTER MEMO MEMVAR 
        INSERT INTO (lcReportFile) FROM MEMVAR 
      ENDSCAN   
    ENDIF
  ENDSCAN 
ELSE
  SELECT APVENDOR
  gfSeek('')  
  *E302975,1 Vend. Summary/History/Detail TMI 10/20/2011 [Start] 
  *SCAN FOR IIF(!EMPTY(lcTaxVend),APVENDOR.CVENTAXID=lcTaxVend,.T.);
    AND IIF(!EMPTY(lcOurVend),APVENDOR.CVENOURAC = lcOurVend ,.T.)
  SCAN FOR IIF(!EMPTY(lcTaxVend),APVENDOR.CVENTAXID=lcTaxVend,.T.);
    AND IIF(!EMPTY(lcOurVend),APVENDOR.CVENOURAC = lcOurVend ,.T.) ;
    &lcRpExp
    *E302975,1 Vend. Summary/History/Detail TMI 10/20/2011 [End  ] 
    SCATTER MEMO MEMVAR 
    INSERT INTO (lcReportFile) FROM MEMVAR 
  ENDSCAN   
ENDIF

*!*************************************************************
*! Name      : lfReplExp
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 10/17/2011
*! Purpose   : I created this function as I faced the following problem
* The lcRpExp is created with expression contains 
*                      APINVHDR.CBNKCODE = 'CHASE'
* which it should be 
*                      APINVHDR.CBNKCODE = 'CHASE   '
* But this would cause problems especially in case if there are more than one bank code with same initials
*!*************************************************************
FUNCTION lfReplExp
LPARAMETERS lcExp,lcFind,lcRepl
LOCAL lnPos,lnAndPos
lnPos = AT(lcFind,lcExp)
IF lnPos>0
  lnAndPos = AT(' AND ',SUBSTR(lcExp,lnPos))
  lnAndPos = IIF(lnAndPos<>0,lnAndPos-1,LEN(SUBSTR(lcExp,lnPos)))
  lcExp = STUFF(lcExp,lnPos,lnAndPos,lcRepl)
ENDIF
*- end of lfReplExp

************************************************************************************************
* Name        : lfPolishExp
* Developer   : Tarek Mohammed Ibrahim - TMI
* Date        : 10/03/2011
* Purpose     : to remove a part of the filter from the lcRpExp
************************************************************************************************
FUNCTION lfPolishExp
PARAMETERS lcExp,lcRmv
LOCAL lnPos,lcRight
lcRight = ")"
lnPos = AT(lcRmv,lcExp)
DO WHILE lnPos>0
  lnAndPos = RAT(' AND ',SUBSTR(lcExp,1,lnPos))
  lcLeftStr = LEFT(lcExp,lnAndPos-1)
  lnPranth = AT(lcRight,SUBSTR(lcExp,lnAndPos))
  lcRightStr = SUBSTR(lcExp,lnAndPos+lnPranth+LEN(lcRight)-1)
  lcExp = lcLeftStr+lcRightStr
  lnPos = AT(lcRmv,lcExp)
ENDDO
