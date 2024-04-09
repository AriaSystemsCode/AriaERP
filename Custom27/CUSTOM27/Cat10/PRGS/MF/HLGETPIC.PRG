*:****************************************************************
*: Program file  : HLGetPic.PRG
*: Program desc. : Add,Change and Inquire Styles Picture.
*: System        : Aria Apparel System (A27).
*: Module        : H.L.C. System. (HL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200246,1
*: Date          : 01/01/2002
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfShow    , lfvAAS  , lfvDisk , 
*:               :            : lfZomInPic.
*:               -----------------------------------------------
*:               : PROCEDURE  : lpClose
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200246,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
EXTERNAL ARRAY laData,laScrMode

lgPic     = "gPic_Sty"
lcCurAlias= SELECT(0)

IF !USED(lcTempCur)
  Create TABLE (gcWorkDir+lcTempCur) (gPic_Sty G )
  SELECT (lcTempCur)
  APPEND BLANK
ENDIF

IF laScrMode[2] .OR. laScrMode[3]                      && Veiw & Edit Mode
  IF !EMPTY(HLCSTYLE.gPic_Sty)
    lgPic = 'HLCSTYLE.gPic_Sty'
  ENDIF  
ENDIF

SELECT (lcTempCur)

PUSH KEY
ON KEY LABEL ESC 
ON KEY LABEL ESC DO lpClose
DO (gcScrDir+gcAct_Appl+"\hlGetPic.SPX")
POP KEY

SELECT(lcCurAlias)

RETURN
*-- End of program code.
*:*************************************************************
*: Name      : lfShow
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/31/2001
*: Purpose   : Zoom In the Picture.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  = lfShow ()
*:*************************************************************
*
FUNCTION lfShow

IF laScrMode[2]
  SHOW GET pbRefAAS DISABLE
  SHOW GET pbRefDsk DISABLE
ELSE
  SHOW GET pbRefAAS ENABLE
  SHOW GET pbRefDsk ENABLE
  SHOW GET lgPic
ENDIF

*-- End Of lfShow.
*:*************************************************************
*: Name      : lfvAAS
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/31/2001
*: Purpose   : Zoom In the Picture.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  = lfvAAS ()
*:*************************************************************
*
FUNCTION lfvAAS

*-- Seek with the type and Style in the Object Link file , if the 
*-- User enter style Picture in the AAS I will get it
*-- Also I will get the First Picture 

IF !USED('OBJECTS')        && Open the OBJECTS File.
  IF FILE('&lcAASDir.OBJECTS.DBF')
    llOBJECTID = gfOpenFile(lcAASDir+'OBJECTS',lcAASDir+'OBJECTID','SH')  
  ELSE
    *- Message Text   :- AAS OBJECTS file not found. Cannot import any Data.
    *- Message Text   :- Please check your AAS Directory Path
    *- Message No.    :- 38216.
    *- Buttom Message :- Ok
    *- Buttom Number  :- 00000.
    = gfModalGen('INM38231B00000','ALERT',"OBJECTS")
    RETURN .F.
  ENDIF  
ENDIF

IF !USED('OBJLINK')        && Open the OBJLINK File.
  IF FILE('&lcAASDir.STYLE.DBF')
    llOBJLINK = gfOpenFile(lcAASDir+'OBJLINK',lcAASDir+'Objlnkty','SH')  
  ELSE
    *- Message Text   :- AAS OBJLINK file not found. Cannot import any Data.
    *- Message Text   :- Please check your AAS Directory Path
    *- Message No.    :- 38216.
    *- Buttom Message :- Ok
    *- Buttom Number  :- 00000.
    = gfModalGen('INM38231B00000','ALERT',"OBJLINK")
    RETURN .F.
  ENDIF  
ENDIF

SELECT OBJLINK
IF SEEK ('S'+laData[24]+'-'+laData[1])
  SELECT (lcTempCur)
  ZAP
  lcPicName = lcImgDir+ALLTRIM(laData[24])+;
              '\'+ALLTRIM(laData[24])+'.BMP'

   lcObjectid = OBJLINK.cObject_id
    SELECT (lcTempCur)

    SELECT gobject AS gpic_sty           ; 
    FROM   &lcAASDir.OBJECTS             ;
    WHERE OBJECTS.cobject_id = lcObjectid;
    INTO TABLE (gcWorkDir+lcTempCur)
    
    lgPic = 'gPic_Sty'
    SHOW GET lgPic
    lcNewPic  = lcPicName
    llNewPic  = .T.
ELSE
  *- Message Text   :- There is no image for style number XXX
  *- Message Text   :- in AAS Directory,Cannot Import any picture.
  *- Message No.    :- 000000.
  *- Buttom Message :- Ok
  *- Buttom Number  :- 00000.
  = gfModalGen('INM00000B00000','F','ALERT',' ',;
  'There is no image for style number '+ladata[1]+' in AAS Directory,Cannot Import any picture.')
  SELECT (lcTempCur)
ENDIF
SHOW GETS

=lfRefresh('awdHlPic')

@ 1.846,16.800 SAY &lgPic ;
		SIZE 22.846,57.800 ;
		CENTER ;
		STRETCH ;
		 STYLE ""

*-- End Of lfvAAS.
*:*************************************************************
*: Name      : lfvDisk
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/31/2001
*: Purpose   : Get picture from any directory.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  = lfvDisk ()
*:*************************************************************
*
FUNCTION lfvDisk

lcName    = lcPicName
lcPicName = GETFILE('Bmp','STYLE IMAGES')

IF EMPTY(lcPicName)
  lcPicName = lcName
  SELECT (lcTempCur)
ELSE
  SELECT (lcTempCur)
  APPEND GENERAL gPic_Sty from &lcPicName
  lgPic = 'gPic_Sty'
  SHOW GET lgPic
  lcNewPic = lcPicName
  llNewPic  = .T.
ENDIF

SHOW GETS

=lfRefresh('awdHlPic')

@ 1.846,16.800 SAY &lgPic ;
		SIZE 22.846,57.800 ;
		CENTER ;
		STRETCH ;
		 STYLE ""

*-- End Of lfvDisk.
*:*************************************************************
*: Name      : lpClose
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/31/2001
*: Purpose   : Trap Esc for lines entry.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  DO lpClose
*:*************************************************************
*
PROCEDURE lpClose

_CUROBJ = OBJNUM(pbOk)
KEYBOARD '{ENTER}'
RETURN

*-- End Of lpClose
*:*************************************************************
*: Name      : lfZomInPic
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/31/2001
*: Purpose   : Zoom In the Picture.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  = lfZomInPic ()
*:*************************************************************
*
FUNCTION lfZomInPic

PUSH KEY
DO (gcScrDir+gcAct_Appl+"\hlZomPic.SPX")
POP KEY

*-- End Of lfZomInPic
*:*************************************************************