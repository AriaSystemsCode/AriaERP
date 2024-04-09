*:*************************************************************************
*: Program file  : iccodes.prg
*: Program desc. :
*: For Screen    : ICICODES
*:        System : Aria 27
*:        Module : IC
*:     Developer : Hesham El-Sheltawi 
*:*************************************************************************
*: Calls       :
*:            FUNCTION : lfGetClen
*:*************************************************************************
*E300703,1 New Program to define the Style Code Structure
*B602125,1 Hesham 11/12/98
*B602125,1 Check before removing any array element from the segments array 
*B602125,1 if it is not a user defined element to prevend subscript out of
*B602125,1 bounds if the user change the use extended size scale setup
*B602125,1 variable
*B604052,1 WAB 11/29/2000 cannot create a style code structure with code ( season) 
*B604052,1                and decrease the size of season less than 6 digit.
*B606624,1 TMI 01/21/2003 Correct the array name "laDomistic" to "laDomestic" to meet the spelling used in 
*B606624,1                the screen ICITEMVE.SCX
*:*************************************************************************

EXTERNAL ARRAY laData,laDefProc

DECLARE laObjStat[9,5]  && Array to hold the display status of each seg.
DECLARE laObjdisp[9]    && Array to control the display of seg. Des. in Child.
DECLARE laAcDes[9,3]    && Array to hold the description of each seg.
DECLARE laSeg[9,4]      && Array to hold the Seg. info.
DECLARE laObjPos[1,1]   && Array to hold the SCREEN OBJECTS POS.
DECLARE laBarCode[7,4]  && Array to hold valid seg. types
*B606624 TMI [Start] Change "laDomistic" to "laDomestic"
*DECLARE laDomistic[1,3] && Array to hold the valid entries of the MAKE FIELD
DECLARE laDomestic[1,3] && Array to hold the valid entries of the MAKE FIELD
*B606624 TMI [End  ]
DECLARE laQuality[1,3]  && Array to hold the valid entries of the CSTYGRADE field
DECLARE laItemTemp[1,3]  && Array of Predefined Item Templates
laAcDes    = ' '        && Default blank to seg description array
STORE 0 TO lnMajCode,lnItemDef
llUseExSSc = gfGetMemVar('M_USEEXSSC')

laObjStat   = "DISABLE"  && Default status of all seg. is DISABLE
lcMajorStat = "DISABLE"
laObjdisp  = .T.        && Default Display all lines in child screen
llCanEdit  = .T.

lnOldTotal = 0          && Old total size
lcOldMask = ''          && Old code Mask
lcMask    = ''          && Current code Mask
lnOldNoSeg= 0           && Old No of segments
lnTotal   = 0           && Current total size
lnNoSeg   = 0           && Current No of segments
lnOldVal  = 0           && Old value for length objects
lcOldVal  = ''          && Old value for seperator objects
lcItemHead = ''          && value for Item Code Header
lcItemStat = 'DISABLE'
IF !gfSetup()
  RETURN
ENDIF
laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)
laDefProc[10] = .F.              && Disable the control panel CLOSE proc.  (lpCLSScr)
lcBarDesc = 'Color   DivisionSeason  Scale   DomesticQuality Group   Other   Free    Remove  '
lcStyleFile = 'STYLE.DBF'
lcBarCode = 'CDZSTQGOF '
lcBarType = 'CCCSVVCOF '
DIMEN laBarCode[LEN(lcBarCode),4]
FOR lnCount = 1 TO LEN(lcBarCode)
  laBarCode[lnCount,1] = SUBSTR(lcBarCode,lnCount,1)
  
  *B604052,1 - WAB (START) Add the type of season (Z) to The Condition
  *IF laBarCode[lnCount,1] $ 'CDG'
  IF laBarCode[lnCount,1] $ 'CDGZ'
  *B604052,1 - WAB (END)
  
    laBarCode[lnCount,2] = lfGetClen(IIF(laBarCode[lnCount,1]='C','COLOR',;
                             IIF(laBarCode[lnCount,1]='G','CSTYGROUP',;
                             IIF(laBarCode[lnCount,1]='Z','SEASON','CDIVISION'))))
  ELSE
    laBarCode[lnCount,2] = IIF(laBarCode[lnCount,1]=' ',0,IIF(laBarCode[lnCount,1]='S',3,6))
  ENDIF
  laBarCode[lnCount,3] = (SUBSTR(lcBarType,lnCount,1) $ 'VOF')
  laBarCode[lnCount,4] = SUBSTR(lcBarDesc,(lnCount-1)*8+1,8)
ENDFOR

IF !WEXIST(gcBaseWind)
  STORE .F. TO laScrMode
  laScrMode[IIF(lfInitSeg(),2,4)] = .T.
  llNoShow = .F.
ELSE
  puMajor = lnMajCode
  puitemdef = lnItemDef
ENDIF
DO (gcScrDir+gcWinAppl+"\ICICODES.SPX")
RELEASE POPUP PUITEMTYPE

*!*************************************************************************
*! Name      : LPSHOW
*! Developer : Hesham El-Sheltawi 
*! Date      : 08/06/97    
*! Purpose   :
*!*************************************************************************
*: Calls       :
*:            FUNCTION : lfInitSeg
*:            FUNCTION : lfShowSegBt
*!*************************************************************************      
*! Returns   :  
*:*************************************************************************     
FUNCTION lpShow
SHOW GET pbDlt DISABLE
laCtrStat[8] = 'DISABLE'

SHOW GET pbSlct DISABLE
laCtrStat[9] = 'DISABLE'

IF !llCanEdit
*  SHOW GET pbEdt DISABLE
*  laCtrStat[7] = 'DISABLE'
  SHOW GET puitemdef DISABLE
ELSE  
  SHOW GET puitemdef ENABLE
ENDIF
DO CASE
  CASE laScrMode[1]
*     =lfInitSeg()
     lascrMode = .F.
     laScrMode[IIF(lfInitSeg(),2,4)] = .T.
     =lpClsScr()
*     SHOW GETS
*     SHOW GETS ONLY
  CASE laScrMode[2]
    =lfInitSeg()
    FOR lnActSeg = 1 TO 9
      lcpbName = 'pbseg'+PADR(lnActSeg,1)
      SHOW GET &lcpbName,1 PROMPT laObjStat[lnActSeg,3]
    ENDFOR
    SHOW GETS WINDOW (gcBaseWind) DISABLE ONLY
  CASE (laScrMode[3] OR laScrMode[4]) AND llCanEdit
    IF !EMPTY(laSeg[1,1])
      SHOW GET pbseg1 ENABLE
      laObjStat[1,1] = 'ENABLE'
      SHOW GET laObjStat[1,4] ENABLE
       IF laBarCode[ATC(laSeg[1,2],lcBarCode),3]
         SHOW GET laSeg[1,1] ENABLE
         laObjStat[1,2] = 'ENABLE'
       ELSE
         SHOW GET laSeg[1,1] DISABLE
         laObjStat[1,2] = 'DISABLE'
       ENDIF
    ENDIF
    FOR lnActSeg = 2 TO 9
      lcpbName = 'pbseg'+PADR(lnActSeg,1)
      IF !EMPTY(laSeg[lnActSeg-1,1])
          SHOW GET &lcpbName,1 PROMPT laObjStat[lnActSeg,3] ENABLE
          laObjStat[lnActSeg,1] = 'ENABLE'
      ELSE
        SHOW GET &lcpbName,1 PROMPT laObjStat[lnActSeg,3] DISABLE
        laObjStat[lnActSeg,1] = 'DISABLE'
      ENDIF
     IF !EMPTY(laSeg[lnActSeg,1]) AND laBarCode[ATC(laSeg[lnActSeg,2],lcBarCode),3]
       SHOW GET laSeg[lnActSeg,1] ENABLE
       laObjStat[lnActSeg,2] = 'ENABLE'
     ELSE
       SHOW GET laSeg[lnActSeg,1] DISABLE
       laObjStat[lnActSeg,2] = 'DISABLE'
     ENDIF
    ENDFOR
    IF lnTotal >=19 AND lnNoSeg < 9
*    IF lnTotal >=18 AND lnNoSeg < 9
       SHOW GET laSeg[lnNoSeg+1,1] DISABLE
       laObjStat[lnNoSeg+1,2] = 'DISABLE'
        lcpbName = 'pbseg'+PADR(lnNoSeg+1,1)
       laObjStat[lnNoSeg+1,1] = 'DISABLE'
       SHOW GET &lcpbName DISABLE
    ENDIF
  CASE (laScrMode[3] OR laScrMode[4]) AND !llCanEdit
    =lfInitSeg()
    SHOW GETS WINDOW (gcBaseWind) DISABLE ONLY
ENDCASE
= lfShowSegBt()


*!*************************************************************************
*! Name      : LFTOTALSIZ
*! Developer : Hesham El-Sheltawi 
*! Date      : 08/06/97    
*! Purpose   :
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfTotalSiz
lnTotal = 0
lnNoSeg = 0
lcMask = ''
FOR lnCount = 1 TO ALEN(laSeg,1)
  lnNoSeg = lnNoSeg+IIF(!EMPTY(laSeg[lnCount,1]),1,0)
  lnTotal = lnTotal + laSeg[lnCount,1]
*  lcMask = lcMask+IIF(lnCount>1 AND !EMPTY(laSeg[lnCount,4]),laSeg[lnCount,4],'')+REPL('#',laSeg[lnCount,1])
  lcMask = lcMask+IIF(!EMPTY(laSeg[lnCount,1]),REPL('#',laSeg[lnCount,1])+ALLT(laSeg[lnCount,4]),'')
  laObjdisp[lnCount]   = !EMPTY(laSeg[lnCount,1])
  lnTotal = lnTotal+IIF(!EMPTY(laSeg[lnCount,1]) AND !EMPTY(laSeg[lnCount,4]),1,0)
ENDFOR
lnTotal = lnTotal-IIF(lnNoSeg>0 AND !EMPTY(laSeg[lnNoSeg,1]) AND !EMPTY(laSeg[lnNoSeg,4]),1,0)
*lnTotal = lnTotal+MAX(lnNoSeg-1,0)
=lfRefresh()
RETURN lnTotal<=19


*!*************************************************************************
*! Name      : LFVNEWVAL
*! Developer : Hesham El-Sheltawi 
*! Date      : 08/06/97    
*! Purpose   :
*!*************************************************************************
*: Calls       :
*:            FUNCTION : lfShowSegBt
*!*************************************************************************      
*! Returns   :  
*:*************************************************************************     
FUNCTION lfvNewVal
IF EVAL(VARREAD()) >0 AND lfTotalSiz()
  lnActSeg = VAL(SUBSTR(VARREAD(),ATC('(',VARREAD())+1,1))
  laAcDes[lnActSeg,1] = SUBSTR(laAcDes[lnActSeg,1],1,laSeg[lnActSeg,1])
  DO CASE
    CASE laSeg[lnActSeg,2] = 'T'
      *B606624 TMI [Start] Change "laDomistic" to "laDomestic"
      *FOR lnCount = 1 TO ALEN(laDomistic,1)
      *  laDomistic[lnCount,3] = PADR(laDomistic[lnCount,3],laSeg[lnActSeg,1])
      FOR lnCount = 1 TO ALEN(laDomestic,1)
        laDomestic[lnCount,3] = PADR(laDomestic[lnCount,3],laSeg[lnActSeg,1])      
        *B606624 TMI [End  ] 
      ENDFOR
    CASE laSeg[lnActSeg,2] = 'Q'
      FOR lnCount = 1 TO ALEN(laQuality,1)
        laQuality[lnCount,3] = PADR(laQuality[lnCount,3],laSeg[lnActSeg,1])
      ENDFOR
  ENDCASE
  lnPos = 0
  FOR lnCount = 1 TO lnActSeg-1
    lnPos = lnPos + laSeg[lnCount,1] +LEN(ALLT(laSeg[lnCount,4]))
  ENDFOR
  lnPos = lnPos + 1
  lcDesc = PADR(SUBSTR(lcItemhead,lnPos),EVAL(VARREAD()))
  lcItemHead = STUFF(lcItemhead,lnPos,lnOldVal,lcDesc)
ELSE
  lcVarName = VARREAD()
  &lcVarname = lnOldVal
ENDIF
=lfShowSegBt()


*!*************************************************************************
*! Name      : LFWNEWVAL
*! Developer : Hesham El-Sheltawi 
*! Date      : 08/06/97    
*! Purpose   :
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfwNewVal
lnOldVal = EVAL(VARREAD())

*!*************************************************************************
*! Name      : LFWSEPRAT
*! Developer : Hesham El-Sheltawi 
*! Date      : 08/06/97    
*! Purpose   :
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfwSeprat
lcOldVal = EVAL(VARREAD())


*!*************************************************************************
*! Name      : LFVSEPRAT
*! Developer : Hesham El-Sheltawi 
*! Date      : 08/06/97    
*! Purpose   :
*!*************************************************************************
*: Calls       :
*:            FUNCTION : lfShowSegBt
*!*************************************************************************      
*! Returns   :  
*:*************************************************************************     
FUNCTION lfvSeprat
IF !EMPTY(EVAL(VARREAD())) AND !lfTotalSiz()
  lcVarName = VARREAD()
  &lcVarname = lcOldVal
ENDIF
IF lcOldval<>EVAL(VARREAD())
  lnActSeg = VAL(SUBSTR(VARREAD(),ATC('(',VARREAD())+1,1))
  lnPos = 0
  FOR lnCount = 1 TO lnActSeg
    lnPos = lnPos + laSeg[lnCount,1]
    IF lnCount < lnActSeg
      lnPos = lnPos +IIF(!EMPTY(laSeg[lnCount,1]) AND !EMPTY(laSeg[lnCount,4]),1,0)
    ENDIF
  ENDFOR
  IF lnPos >0
    lcItemHead = STUFF(lcItemHead,lnPos+1,IIF(EMPTY(lcOldVal),0,1),ALLT(laSeg[lnActSeg,4]))
  ENDIF
ENDIF

=lfShowSegBt()

*!*************************************************************************
*! Name      : LFVSELTYP
*! Developer : Hesham El-Sheltawi 
*! Date      : 08/06/97    
*! Purpose   :
*!*************************************************************************
*: Calls       :
*:            FUNCTION : lfShowSegBt
*!*************************************************************************      
*: Passed parameters  : lnbarno
*:*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfvSelTyp
PARAMETERS lnBarNo
IF lnBarNo>0
*  laSeg[lnActSeg,1] = MIN(laBarCode[lnBarNo,2],(19-(lnTotal-laSeg[lnActSeg,1]))-IIF(laSeg[lnActSeg,1]=0,1,0))
  laSeg[lnActSeg,1] = MIN(laBarCode[lnBarNo,2],(19-(lnTotal-laSeg[lnActSeg,1])))
  IF laSeg[lnActSeg,1]=0 AND laSeg[lnActSeg,3]
    laSeg[lnActSeg,3] = .F.
    laSeg[1,3] = .T.
    STORE 1 TO puMajor,lnMajCode
  ENDIF
  IF laSeg[lnActSeg,1]=0
    laSeg[lnActSeg,4] = ' '
    IF lnActSeg>1
      laSeg[lnActSeg-1,4] = ' '
    ENDIF
  ENDIF
  laSeg[lnActSeg,2] = laBarCode[lnBarNo,1]
  laAcDes[lnActSeg,1] = SUBSTR(laAcDes[lnActSeg,1],1,laSeg[lnActSeg,1])
  DO CASE
    CASE laSeg[lnActSeg,2] = 'T'
      *B606624 TMI [Start] Change "laDomistic" to "laDomestic"
      *FOR lnCount = 1 TO ALEN(laDomistic,1)
      *  laDomistic[lnCount,3] = PADR(laDomistic[lnCount,3],laSeg[lnActSeg,1])
      FOR lnCount = 1 TO ALEN(laDomestic,1)
        laDomestic[lnCount,3] = PADR(laDomestic[lnCount,3],laSeg[lnActSeg,1])
      *B606624 TMI [End  ] 
      ENDFOR
    CASE laSeg[lnActSeg,2] = 'Q'
      FOR lnCount = 1 TO ALEN(laQuality,1)
        laQuality[lnCount,3] = PADR(laQuality[lnCount,3],laSeg[lnActSeg,1])
      ENDFOR
  ENDCASE

  laObjStat[lnActSeg,3] = IIF(laSeg[lnActSeg,1]= 0,'Seg '+PADR(lnActSeg,1),PRMBAR('PUITEMTYPE', lnBarNo))
  lcpbName = 'pbseg'+PADR(lnActSeg,1)
  SHOW GET &lcpbName,1 PROMPT IIF(laSeg[lnActSeg,1]= 0,'Seg '+PADR(lnActSeg,1),PRMBAR('PUITEMTYPE', lnBarNo))
  IF laBarCode[lnBarNo,3]
    SHOW GET laSeg[lnActSeg,1] ENABLE
    laObjStat[lnActSeg,2] = 'ENABLE'
  ELSE
    SHOW GET laSeg[lnActSeg,1] DISABLE
    laObjStat[lnActSeg,2] = 'DISABLE'
  ENDIF
  =lfShowSegBt()
ENDIF
DEACTIVATE POPUP PUITEMTYPE


*!*************************************************************************
*! Name      : LFVSEGTYPE
*! Developer : Hesham El-Sheltawi 
*! Date      : 08/06/97    
*! Purpose   :
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfvSegType
lnActSeg = VAL(RIGHT(VARREAD(),1))
lnBarpos = ASCAN(laObjPos,VARREAD())
IF lnBarpos > 0
  lnBarPos = ASUBSCRIPT(laObjPos,lnBarPos,1)
  ACTIVATE POPUP PUITEMTYPE AT laObjPos[lnBarPos,3],laObjPos[lnBarPos,2]
ENDIF



FUNCTION lfvPreDefItem
IF puitemdef>0
  lnTotal   = 0           && Current total size
  lnNoSeg   = 0           && Current No of segments
  FOR lnCount = 1 TO 9
    laSeg[lnCount,1] = 0
    laSeg[lnCount,3] = .F.
    laSeg[lnCount,4] = ''
    laObjStat[lnCount,3] = 'Seg '+PADR(lnCount,1)
    laObjStat[lnCount,4] = 0
  ENDFOR
   lnItemDef = puitemdef
   lnNoSeg = 0
   SELECT ICISTRU
   =SEEK(laItemTemp[puitemdef,2])
   SCAN WHILE citemrecty+cisegno = laItemTemp[puitemdef,2]
     lnNoSeg = lnNoSeg + 1
     laSeg[lnNoSeg,1]   = nisegsize
     laSeg[lnNoSeg,2]   = ALLT(cisegtype)
     laSeg[lnNoSeg,3] = LSEGENDMAJ
     laSeg[lnNoSeg,4] = CISEGSEPR
     laAcDes[lnNoSeg,1] = cisegsdes
     lcItemHead = IIF(lnNoSeg=1,CISEGHEAD,lcItemHead)
     puMajor = IIF(LSEGENDMAJ AND puMajor=0,lnNoSeg,puMajor)
     laObjStat[lnNoSeg,3] = laBarCode[ATC(laSeg[lnNoSeg,2],lcBarCode),4]
     laObjdisp[lnNoSeg]   = .T.
     lnTotal = lnTotal + laSeg[lnNoSeg,1]
     lnTotal = lnTotal+IIF(!EMPTY(laSeg[lnNoSeg,4]),1,0)
     lcMask = lcMask+REPL('#',laSeg[lnNoSeg,1])+ALLT(laSeg[lnNoSeg,4])
   ENDSCAN
   lnTotal = lnTotal-IIF(!EMPTY(laSeg[lnNoSeg,1]) AND !EMPTY(laSeg[lnNoSeg,4]),1,0)
   lnOldTotal = lnTotal         && Old total size
   lcOldMask = lcMask          && Old code Mask
   lnOldNoSeg = lnNoSeg
   SHOW GETS
ENDIF


*!*************************************************************************
*! Name      : LFREADWHEN
*! Developer : Hesham El-Sheltawi 
*! Date      : 08/06/97    
*! Purpose   :
*!*************************************************************************
*: Calls       :
*:            FUNCTION : lfvSelTyp
*!*************************************************************************      
*! Returns   :  
*:*************************************************************************     
FUNCTION lfReadWhen
DEFINE POPUP PUITEMTYPE IN WINDOW (gcBaseWind) SCROLL
FOR lnCount = 1 TO ALEN(laBarCode,1)
  lcskip = ''
  IF !EMPTY(laBarCode[lnCount,1])
    IF !(laBarCode[lnCount,1] $ 'OF')
      lcskip = 'SKIP FOR ASCAN(laSeg,laBarCode['+PADR(lnCount,1)+',1])>0'
      IF laBarCode[lnCount,1] = 'S' AND !llUseExSSc
      lcskip = 'SKIP FOR .T.'      
      ENDIF
    ENDIF
    IF !laBarCode[lnCount,3]
      lcSkip = lcSkip+IIF(EMPTY(lcSkip),'SKIP FOR',' OR ')+;
      'lntotal-laSeg[lnActSeg,1]+laBarCode['+PADR(lnCount,1)+',2]>19'
    ENDIF
  ELSE
    lcskip = 'SKIP FOR lnActSeg<>lnNoSeg'
  ENDIF
  DEFINE BAR lnCount OF PUITEMTYPE PROMPT laBarCode[lnCount,4] &lcskip
ENDFOR
ON SELECTION POPUP PUITEMTYPE DO lfvSelTyp WITH BAR()



*!*************************************************************************
*! Name      : LFSHOWSEGBT
*! Developer : Hesham El-Sheltawi 
*! Date      : 08/06/97    
*! Purpose   : function to control the objects show status
*!*************************************************************************
*: Calls       :
*:            FUNCTION : lfTotalSiz
*!*************************************************************************      
*! Returns   :  
*:*************************************************************************     
FUNCTION lfShowSegBt
=lfTotalSiz()
IF lnNoSeg>0
  SHOW GET pbDiscrip ENABLE
ELSE
  SHOW GET pbDiscrip DISABLE
ENDIF
IF lnNoSeg>0 AND (laScrMode[3] OR laScrMode[4]) AND llCanEdit
  lcMajorStat = 'ENABLE'
  SHOW GET puMajor ENABLE
ELSE
  lcMajorStat = 'DISABLE'
  SHOW GET puMajor DISABLE
ENDIF
IF ASCAN(laSeg,'Q') >0 OR ASCAN(laSeg,'T') >0
  SHOW GET pbVldEnt ENABLE
ELSE
  SHOW GET pbVldEnt DISABLE
ENDIF
FOR lnActSeg = 2 TO 9
  lcpbName = 'pbseg'+PADR(lnActSeg,1)
  IF !EMPTY(laSeg[lnActSeg-1,1]) AND (laScrMode[3] OR laScrMode[4])
    IF lnTotal<19
*    IF lnTotal<18
      SHOW GET &lcpbName ENABLE
      laObjStat[lnActSeg,1] = 'ENABLE'
    ENDIF
  ELSE
    SHOW GET &lcpbName DISABLE
    laObjStat[lnActSeg,1] = 'DISABLE'
  ENDIF
ENDFOR

FOR lnActSeg = 1 TO 9
  IF lnActSeg <= lnNoSeg-1
    IF (laScrMode[3] OR laScrMode[4]) AND llCanEdit
      SHOW GET laSeg[lnActSeg,4] ENABLE
      laObjStat[lnActSeg,4] = 'ENABLE'
    ELSE
      SHOW GET laSeg[lnActSeg,4] DISABLE
      laObjStat[lnActSeg,4] = 'DISABLE'
    ENDIF
  ELSE
    SHOW GET laSeg[lnActSeg,4] DISABLE
    laObjStat[lnActSeg,4] = 'DISABLE'
  ENDIF
ENDFOR
IF lnTotal >=19 AND lnNoSeg < 9
*IF lnTotal >=18 AND lnNoSeg < 9
  SHOW GET laSeg[lnNoSeg+1,1] DISABLE
  laObjStat[lnNoSeg+1,2] = 'DISABLE'
  lcpbName = 'pbseg'+PADR(lnNoSeg+1,1)
  laObjStat[lnNoSeg+1,1] = 'DISABLE'
  SHOW GET &lcpbName DISABLE
  SHOW GET laObjStat[lnNoSeg+1,4] DISABLE
ENDIF




*!*************************************************************************
*! Name      : LFVDISCRIP
*! Developer : Hesham El-Sheltawi 
*! Date      : 08/06/97    
*! Purpose   : Branch to segments descriptions screen
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfvDiscrip
lnBoxh = (lnNoSeg * 2) + 5 +lnNoSeg * .25 &&6.25
lcBtMpCan  = gcBMPHome + "CAN.BMP"
lcBtMpOK   = gcBMPHome + "OK.BMP"
IF laScrMode[3]
  PRIVATE laObjStTmp
  =ACOPY(laObjStat,laObjStTmp)
  FOR lnCount = 1 TO lnNoSeg
    laObjStat[lnCount,1] = 'ENABLE'
  ENDFOR
ENDIF
lcItemStat = IIF(laScrMode[3] OR laScrMode[4],'ENABLE','DISABLE')
DO (gcScrDir +gcWinAppl+'\ICITEMDS.SPR')
IF laScrMode[3]
  =ACOPY(laObjStTmp,laObjStat)
ENDIF
=lfRefresh()




*!*************************************************************************
*! Name      : LFVVLDENT
*! Developer : Hesham El-Sheltawi 
*! Date      : 08/06/97    
*! Purpose   : Branch to segments Valid entries screen
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfvVldEnt
lcBtMpOK   = gcBMPHome + "OK.BMP"
DO (gcScrDir +gcWinAppl+'\ICITEMVE.SPR')


*!*************************************************************************
*! Name      : LFSEGDES
*! Developer : Hesham El-Sheltawi 
*! Date      : 08/06/97    
*! Purpose   : Function used in the picture of all short descriptions to 
*!             define how manay Characters to be accepted in each field
*!*************************************************************************
*: Passed parameters  : lnwidth
*:*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfSegDes
PARAMETERS lnWidth

RETURN REPLICATE("N",lnWidth)



*!*************************************************************************
*! Name      : LPSAVSCR
*! Developer : Hesham El-Sheltawi 
*! Date      : 08/06/97    
*! Purpose   : Save/Update Style Code Structure
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
PROCEDURE lpSavScr
IF llCupdate AND lnNoSeg=0
  =gfModalGen('QRM42070B00000','DIALOG')
  llCSave = .F.
  RETURN
ENDIF
FOR lnCount = 1 TO lnNoSeg
  IF EMPTY(laAcDes[lnCount,1])
    =gfModalGen('QRM42071B00000','DIALOG')
    llCSave = .F.
    _CUROBJ = OBJNUM(pbDiscrip)
    KEYBOARD "{SPACEBAR}"
    RETURN
  ENDIF
ENDFOR
lnCount = ASUBSCRIPT(laSeg,ASCAN(laSeg,.T.),1)
IF lnCount < lnNoSeg AND EMPTY(laSeg[lnCount,4])
  =gfModalGen('QRM42073B00000','DIALOG')
  llCSave = .F.
  RETURN
ENDIF
IF llUseExSSc AND (ASCAN(laSeg,'S')=0 OR (laSeg[lnNoSeg,2]<>'S' OR lnNoSeg=lnMajCode))
  =gfModalGen('QRM42075B00000','DIALOG')
  llCSave = .F.
  RETURN
ENDIF
lnPos = 1
llEmpty = .F.
FOR lnCount = 1 TO lnNoSeg
   IF IIF(lnCount>1 ,!EMPTY(laSeg[lnCount-1,4]),.T.) AND EMPTY(SUBSTR(lcItemHead,lnPos,laSeg[lnCount,1]))
     llEmpty = .T.
     EXIT
   ENDIF
   lnPos = lnPos + laSeg[lnCount,1]+LEN(ALLTRIM(laSeg[lnCount,4]))
ENDFOR
IF llEmpty
    =gfModalGen('QRM42074B00000','DIALOG')
    llCSave = .F.
    _CUROBJ = OBJNUM(pbDiscrip)
    KEYBOARD "{SPACEBAR}"
    RETURN
ENDIF

IF ASCAN(laSeg,'Q') >0
  FOR lnCount = 1 TO ALEN(laQuality,1)
    IF EMPTY(laQuality[lnCount,3])
      llCSave = .F.
      =gfModalGen('QRM42072B00000','DIALOG','Quality')
      _CUROBJ = OBJNUM(pbvldEnt)
      KEYBOARD "{SPACEBAR}"
      RETURN
    ENDIF
  ENDFOR
ENDIF


IF ASCAN(laSeg,'T') >0
  *B606624 TMI [Start] Change "laDomistic" to "laDomestic"
  *FOR lnCount = 1 TO ALEN(laDomistic,1)
  *  IF EMPTY(laDomistic[lnCount,3])
  FOR lnCount = 1 TO ALEN(laDomestic,1)
    IF EMPTY(laDomestic[lnCount,3])
      *B606624 TMI [End  ] 
      llCSave = .F.
      =gfModalGen('QRM42072B00000','DIALOG','Domestic')
      _CUROBJ = OBJNUM(pbvldEnt)
      KEYBOARD "{SPACEBAR}"
      RETURN
    ENDIF
  ENDFOR
ENDIF


SELECT ICISTRU
llCSave = .T.

FOR lnCount = 1 TO ALEN(laSeg,1)
  llWasAdd = .F.
  IF !SEEK('U'+STR(lnCount,1)) AND lnCount <= lnNoSeg
    llWasAdd = .T.
    APPEND BLANK
  ELSE
    IF FOUND() AND lnCount > lnNoSeg
      =gfTraceKey('ICISTRU',citemrecty+cisegno,'D')
      DELETE
    ENDIF
  ENDIF
  IF lnCount <= lnNoSeg
     REPLACE  cisegno    WITH STR(lnCount,1) ,;
              nisegsize  WITH laSeg[lnCount,1],;
              lsegendmaj WITH laSeg[lnCount,3],;
              cisegldes  WITH laAcDes[lnCount,2],;
              cisegsdes  WITH laAcDes[lnCount,1],;
              cisegtype  WITH laSeg[lnCount,2],;
              ciSegSepr  WITH  laSeg[lnCount,4],;
              cisegHead  WITH IIF(lnCount=1,lcItemHead,' '),;
              citemrecty WITH 'U'
    =gfTraceKey('ICISTRU',citemrecty+cisegno,IIF(llWasAdd,'A','M'))              
    =gfAdd_Info()                     && Add the add user,time and date
  ENDIF
ENDFOR
IF llCanEdit
  SELECT icsegval
  GO TOP
  DELETE ALL FOR gfTraceKey('ICSEGVAL',cisegno+cisegval,'D')
  IF ASCAN(laSeg,'Q') >0
    lnVldPos = ASCAN(laSeg,'Q')
    lnVldPos = ASUBSC(laSeg,lnVldPos,1)
    FOR lnCount = 1 TO ALEN(laQuality,1)
      APPEND BLANK
      REPLACE cisegno   WITH STR(lnVldPos,1) ,;
              cisgvalsd WITH laQuality[lnCount,1],;
              cisegval  WITH PADR(laQuality[lnCount,3],laSeg[lnVldPos,1]),;
              cvldentry WITH laQuality[lnCount,2]
      =gfTraceKey('ICSEGVAL',cisegno+cisegval,'A')        
      =gfAdd_Info()                     && Add the add user,time and date
    ENDFOR
  ENDIF
  IF ASCAN(laSeg,'T') >0
    lnVldPos = ASCAN(laSeg,'T')
    lnVldPos = ASUBSC(laSeg,lnVldPos,1)
    *B606624 TMI [Start] Change "laDomistic" to "laDomestic"
    *FOR lnCount = 1 TO ALEN(laDomistic,1)
    FOR lnCount = 1 TO ALEN(laDomestic,1)
      *B606624 TMI [End  ] 
      APPEND BLANK
      *B606624 TMI [Start] Change "laDomistic" to "laDomestic"
      *REPLACE cisegno   WITH STR(lnVldPos,1) ,;
              cisgvalsd WITH laDomistic[lnCount,1],;
              cisegval  WITH PADR(laDomistic[lnCount,3],laSeg[lnVldPos,1]),;
              cvldentry WITH laDomistic[lnCount,2]
      REPLACE cisegno   WITH STR(lnVldPos,1) ,;
              cisgvalsd WITH laDomestic[lnCount,1],;
              cisegval  WITH PADR(laDomestic[lnCount,3],laSeg[lnVldPos,1]),;
              cvldentry WITH laDomestic[lnCount,2]
      *B606624 TMI [End  ] 
      =gfTraceKey('ICSEGVAL',cisegno+cisegval,'A')
      =gfAdd_Info()                     && Add the add user,time and date
    ENDFOR
  ENDIF
ENDIF
STORE .F. TO laScrMode
laScrMode[2] = .T.




*!*************************************************************************
*! Name      : LPCLSSCR
*! Developer : Hesham El-Sheltawi 
*! Date      : 08/06/97    
*! Purpose   : Push button close valid function
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lpClsScr
IF laScrMode[4]
    glQuitting = .T.
    CLEAR READ
ENDIF



*!*************************************************************************
*! Name      : LFINITSEG
*! Developer : Hesham El-Sheltawi 
*! Date      : 08/06/97    
*! Purpose   : function to initialize the item code structure segments
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfInitSeg
  *CSTYGRADE 'Quality'
  *MAKE      'Domestic'
  SELECT SYDFIELD
  =SEEK('CSTYGRADE')
  DECLARE laVldEnt[1,1]
  =gfSubStr(ALLT(MVENTRIES),@laVldEnt,'|~')
  DECLARE laQuality[ALEN(laVldEnt,1),3]
  STORE 0 TO puMajor,puitemdef,lnItemDef
  FOR lnCount = 1 TO ALEN(laVldEnt,1)
    laQuality[lnCount,1] = laVldEnt[lnCount,1]
    laQuality[lnCount,2] = laVldEnt[lnCount,2]
    laQuality[lnCount,3] = ''
  ENDFOR
  =SEEK('MAKE     ')
  DECLARE laVldEnt[1,1]
  =gfSubStr(ALLT(MVENTRIES),@laVldEnt,'|~')
  *B606624 TMI [Start] Change "laDomistic" to "laDomestic"
  *DECLARE laDomistic[ALEN(laVldEnt,1),3]
  DECLARE laDomestic[ALEN(laVldEnt,1),3]
  *B606624 TMI [End  ] 
  FOR lnCount = 1 TO ALEN(laVldEnt,1)
    *B606624 TMI [Start] Change "laDomistic" to "laDomestic"
    *laDomistic[lnCount,1] = laVldEnt[lnCount,1]
    *laDomistic[lnCount,2] = laVldEnt[lnCount,2]
    *laDomistic[lnCount,3] = ''
    laDomestic[lnCount,1] = laVldEnt[lnCount,1]
    laDomestic[lnCount,2] = laVldEnt[lnCount,2]
    laDomestic[lnCount,3] = ''
    *B606624 TMI [End  ]    
  ENDFOR
  
*  SELECT ciseghead,citemrecty;
  FROM (gcDataDir+'ICISTRU');
  WHERE citemrecty+cisegno <> 'U';
     AND cisegno = '1';
  ORDER BY citemrecty;   
  INTO ARRAY laItemTemp
 
 
  SELECT ciseghead,citemrecty;
  FROM (gcDataDir+'ICISTRU');
  WHERE cisegno = '1';
  ORDER BY citemrecty;   
  INTO ARRAY laItemTemp

  SELECT ICISTRU
  SET ORDER TO TAG SEGNO DESC
  lnCount = 1
*  FOR  lnCount = 1 TO ALEN(laItemTemp,1)
   DO WHILE lnCount <= ALEN(laItemTemp,1)
    =SEEK(laItemTemp[lnCount,2])
    IF llUseExSSc
*      laItemTemp[lnCount,1] = IIF( !FOUND() OR CISEGTYPE <> 'S' ,'\','')+laItemTemp[lnCount,1]
       *B602125,1 Hesham (Start)
       *B602125,1 Check before removing the array element if it is not a user
       *B602125,1 defined element
*       IF !FOUND() OR CISEGTYPE <> 'S'
       IF (!FOUND() OR CISEGTYPE <> 'S') AND CITEMRECTY <> 'U'
       *B602125,1 Hesham (End)       
         =ADEL(laItemTemp,lnCount)
         DIMEN laItemTemp[ALEN(laItemTemp,1)-1,2]
       ELSE
         lnCount =   lnCount + 1
       ENDIF 
    ELSE
*      laItemTemp[lnCount,1] = IIF( !FOUND() OR CISEGTYPE = 'S' ,'\','')+laItemTemp[lnCount,1]
       *B602125,1 Hesham (Start)
       *B602125,1 Check before removing the array element if it is not a user
       *B602125,1 defined element
*      IF !FOUND() OR CISEGTYPE = 'S'
      IF (!FOUND() OR CISEGTYPE = 'S') AND CITEMRECTY <> 'U'
       *B602125,1 Hesham (End)             
        =ADEL(laItemTemp,lnCount)
        DIMEN laItemTemp[ALEN(laItemTemp,1)-1,2]
      ELSE
         lnCount =   lnCount + 1      
      ENDIF  
    ENDIF  
   ENDDO 
*  ENDFOR
  SET ORDER TO TAG SEGNO ASCE
  laObjStat    = 'DISABLE'
  lnOldTotal = 0          && Old total size
  lcOldMask = ''          && Old code Mask
  lcMask    = ''          && Current code Mask
  lnOldNoSeg= 0           && Old No of segments
  lnTotal   = 0           && Current total size
  lnNoSeg   = 0           && Current No of segments
  FOR lnCount = 1 TO 9
    laSeg[lnCount,1] = 0
    laSeg[lnCount,3] = .F.
    laSeg[lnCount,4] = ''
    laObjStat[lnCount,3] = 'Seg '+PADR(lnCount,1)
    laObjStat[lnCount,4] = 0
  ENDFOR
  SELECT ICISTRU
*  LOCATE
  llReturn = SEEK('U1') &&found()
  IF !llReturn &&!FOUND()
    STORE 1 TO puitemdef
    =lfvPreDefItem()
    laObjdisp    = .F.
    laObjStat[1,1] = 'ENABLE'
    laObjdisp[1]   = .T.
    llCanEdit = .T.
  ELSE
    lnNoSeg = 0
    SCAN WHILE citemrecty+cisegno = 'U'
      lnNoSeg = lnNoSeg + 1
      laSeg[lnNoSeg,1]   = nisegsize
      laSeg[lnNoSeg,2]   = ALLT(cisegtype)
      laSeg[lnNoSeg,3] = LSEGENDMAJ
      laSeg[lnNoSeg,4] = CISEGSEPR
      laAcDes[lnNoSeg,1] = cisegsdes
      lcItemHead = IIF(lnNoSeg=1,CISEGHEAD,lcItemHead)
      puMajor = IIF(LSEGENDMAJ AND puMajor=0,lnNoSeg,puMajor)
      laObjStat[lnNoSeg,3] = laBarCode[ATC(laSeg[lnNoSeg,2],lcBarCode),4]
      laObjdisp[lnNoSeg]   = .T.
      lnTotal = lnTotal + laSeg[lnNoSeg,1]
      lnTotal = lnTotal+IIF(!EMPTY(laSeg[lnNoSeg,4]),1,0)
      lcMask = lcMask+REPL('#',laSeg[lnNoSeg,1])+ALLT(laSeg[lnNoSeg,4])
    ENDSCAN
    lnTotal = lnTotal-IIF(!EMPTY(laSeg[lnNoSeg,1]) AND !EMPTY(laSeg[lnNoSeg,4]),1,0)
    lnOldTotal = lnTotal         && Old total size
    lcOldMask = lcMask          && Old code Mask
    lnOldNoSeg = lnNoSeg
    puitemdef = ASUBS(laItemTemp,ASCAN(laItemTemp,'U'),1)
    SELECT (lcStyleFile)
    LOCATE
*    llCanEdit = .T.
    llCanEdit = !FOUND()
    IF llCanEdit
      SELECT icsegval
      FOR lnCount = 1 TO lnNoSeg
        IF SUBSTR(lcBarType,ATC(laSeg[lnCount,2],lcBarCode),1) $ 'FO'
          llCanEdit = !SEEK(STR(lnCount,1))
          IF !llCanEdit
            EXIT
          ENDIF
        ENDIF
      ENDFOR
    ENDIF
    SELECT icsegval
    IF ASCAN(laSeg,'Q') >0
      lnVldPos = ASCAN(laSeg,'Q')
      lnVldPos = ASUBSC(laSeg,lnVldPos,1)
      IF SEEK(STR(lnVldPos,1))
        DECLARE laQuality[1,3]
        STORE '' TO laQuality
        SELECT cisgvalsd,cvldentry,;
               PADR(cisegval,laSeg[lnVldPos,1]);
             FROM (gcDataDir+'icsegval')  ;
             WHERE cisegno = STR(lnVldPos,1);
             INTO ARRAY laQuality
      ENDIF
    ENDIF
    IF ASCAN(laSeg,'T') >0
      lnVldPos = ASCAN(laSeg,'T')
      lnVldPos = ASUBSC(laSeg,lnVldPos,1)
      IF SEEK(STR(lnVldPos,1))
        *B606624 TMI [Start] Change "laDomistic" to "laDomestic"
        *DECLARE laDomistic[1,3]
        *STORE '' TO laDomistic
        *SELECT cisgvalsd,cvldentry,;
               PADR(cisegval,laSeg[lnVldPos,1]);
             FROM (gcDataDir+'icsegval')  ;
             WHERE cisegno = STR(lnVldPos,1);
             INTO ARRAY laDomistic
        DECLARE laDomestic[1,3]
        STORE '' TO laDomestic
        SELECT cisgvalsd,cvldentry,;
               PADR(cisegval,laSeg[lnVldPos,1]);
             FROM (gcDataDir+'icsegval')  ;
             WHERE cisegno = STR(lnVldPos,1);
             INTO ARRAY laDomestic
        *B606624 TMI [End  ] 
      ENDIF
    ENDIF

  ENDIF
  IF puMajor=0
    puMajor = 1
    laSeg[1,3] = .T.
  ENDIF
  lnMajCode = puMajor
*  llCanEdit = .T.
RETURN llReturn



*!*************************************************************************
*! Name      : LFGETCLEN
*! Developer : Hesham El-Sheltawi 
*! Date      : 08/06/97    
*! Purpose   : function to get the code fields length
*!*************************************************************************
*: Passed parameters  : lcfieldname
*:*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfGetClen
PARAMETERS lcFieldName
IF SEEK(PADR(UPPER(lcFieldName),10),'CODESET')
  RETURN CODESET.NFLD_WDTH
ELSE
  RETURN 6
ENDIF


*!*************************************************************************
*! Name      : LFVCODEMAJ
*! Developer : Hesham El-Sheltawi 
*! Date      : 08/06/97    
*! Purpose   : valid function for the major account
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfvCodeMaj
FOR lnCount = 1 TO 9
  laSeg[lnCount,3] = .F.
ENDFOR
laSeg[puMajor,3] = .T.
lnMajCode = puMajor