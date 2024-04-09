*:***********************************************************************************************************
*: Program file  : GMAEDICR
*: Program desc. : FIx program to Update EDICRTSQ File For GMA  (B037982)
*:        System : Aria Advantage Series.
*:        Module : (EDI).
*:     Developer : Hend Ghanem (HBG)
*:          Date : 04/14/2004
*:***********************************************************************************************************

CLOSE DATA

=gfModalGen("INM00000B42001","Dialog",.F.,.F.,"Please select the Data Directory.")
lcDataDir = GETDIR()
IF !FILE(lcDataDir+'PACK_HDR.DBF')
  =gfModalGen("INM00000B42001","Dialog",.F.,.F.,"This is not the Data Directory. Cannot proceed.")
  RETURN  
ENDIF

USE lcDataDir+"ASN_SHIP" SHARE 
USE lcDataDir+"PACK_HDR" SHARE IN 0
USE lcDataDir+"EDICRTSQ" SHARE IN 0
USE lcDataDir+"EDIACPRT" SHARE IN 0
SELECT EDIACPRT
SET ORDER TO ACCfACT
SELECT EDICRTSQ
SET ORDER TO PCKCRTSQ
SELECT PACK_HDR
SET ORDER TO PACK_HDR
SELECT ASN_SHIP
SET ORDER TO ASN_SHIP
SET RELATION TO pack_no INTO Pack_HDR ADDITIVE
lcTemp    = SYS(2015)
lcTempQry = SYS(2015)

SELECT Asn_ship.Cart_NO , Asn_ship.Pack_no , Asn_ship.ucc9 , PAck_hdr.account ,PAck_hdr.ship_date;
 FROM Asn_ship, Pack_hdr;
 WHERE Pack_hdr.pack_no = Asn_ship.pack_no;
   INTO CURSOR (lcTemp)
   
SELECT (lcTemp)
SCAN
  SCATTER MEMVAR MEMO
  IF !SEEK(m.Pack_No+STR(m.Cart_No),'EDICRTSQ')
    WAIT WINDOW 'Update Cartons information for Packing slip # ' + m.Pack_No NOWAIT
    INSERT INTO EDICRTSQ FROM MEMVAR
  ENDIF
ENDSCAN

SELECT DISTINCT Edicrtsq.account, MAX(Edicrtsq.ucc9);
 FROM Edicrtsq;
 WHERE Edicrtsq.ucc9 = "";
 GROUP BY Edicrtsq.account INTO CURSOR (lcTempQry)

SELECT (lcTempQry)
SCAN
  IF SEEK('A'+Account,'EDIACPRT')
    REPLACE EDIACPRT.Ucc9 WITH &lcTempQry..MAX_UCC9
  ENDIF
ENDSCAN

CLOSE DATA
*!*************************************************************
*! Name      : gfModalGen
*! Developer : Yasser Saad Ibrahime
*! Date      : 1993-1995 
*! Purpose   : To display any data driven dialog
*!*************************************************************
*! Passed Parameters  : 
*!                1-lcDlgID   (Dialog ID)
*!                          1st 2 characters are TR for Terminat icon
*!                                               QR for Quiry    icon
*!                                               IN for Inform   icon
*!                          2nd 4 characters are the messag ID 
*!                          3rd 4 characters are the button ID
*!                2-lcDlgTyp  (Dialog type)
*!                          'D' --> Dialog colors
*!                          'A' --> Alert  colors
*!                3-lcVarsStr  (variable(s) to be replased in the messag 
*!                4-lcDlgValid (Validation function name to be used in 
*!                              the valid of the dialog buttons)
*!                5-lcDlgMessg if you want to display a specific message
*!                            send the message string to this parameter         
*!
*!*************************************************************
FUNCTION gfModalGen
PARAMETER lcDlgID,lcDlgTyp,lcVarsStr,lcDlgValid,lcDlgMessg

PRIVATE lcDlgID,lcDlgTyp,lcVarsStr,lcDlgValid,lcCurrDbf,lnButtons,lcDlgMessg,;
lcDevice

DECLARE laButtNo [1]         && Array to hold the buttons separetly
DECLARE laVarsStr[1]         && Array to hold variabels to be replaced
                             && in the messag
lcDevice = SET('DEVICE')
SET DEVICE TO SCREEN
PUSH KEY
ON KEY
lcCurrDbf   = ALIAS()
lcDlgID     = IIF(TYPE('lcDlgID'   ) $ "UL",'',lcDlgID   )
lcDlgTyp    = IIF(TYPE('lcDlgTyp'  ) $ "UL",'D',lcDlgTyp )
lcVarsStr   = IIF(TYPE('lcVarsStr' ) $ "UL",'',lcVarsStr )
lcDlgValid  = IIF(TYPE('lcDlgValid') $ "UL",'',lcDlgValid)
lcDlgMessg  = IIF(TYPE('lcDlgMessg') $ "UL",'',lcDlgMessg)
lcMsgCatgry = SUBSTR(lcDlgID,1,2)

lnColrSchm  = VAL(SUBSTR('57',AT(LEFT(lcDlgTyp,1),'DA'),1))  
lnDlgOpTion = 1              && Variable to get the result in
lnButtSize  = 1
lnOldRec    = 0
lnTextStrt  = IIF(!EMPTY(lcMsgCatgry),7,2)
lnRigtShft  = IIF(!EMPTY(lcMsgCatgry),10,5)

lcMessag   = ALLTRIM (lcDlgMessg)
lcButton   = "\!\?\<Ok"  
lnButtNo   = 1
lnButtSize = 4


*** Buttons width should not be less than 10 after adding 4 (< >) 
lnButtSize = MAX(10,lnButtSize+ 4 )


*** Calculat window width accordin to total buttons size
lnWinWidth = MAX(50,(lnButtSize * lnButtNo)+6)

*** Calculat space between buttons 
lnBetwButt = INT((lnWinWidth -(lnButtSize * lnButtNo))/lnButtNo)

*** Calculat X position of the button to be centerd
lnButXPos  = (lnWinWidth/2)-((lnButtSize * lnButtNo)+;
             (lnBetwButt*(lnButtNo-1)))/2

*** Calculat window higth according to the length of the messag
lnWinHight = INT(LEN(ALLTRIM(lcMessag))/lnWinWidth) + 6 
gcSysName  = "Information"
*** Define dialog window with right width, hight and color
DEFINE WINDOW gwdDialog ;
       AT  0.000, 0.000  ;
       SIZE lnWinHight,lnWinWidth;
	   FONT "System", 10 ;
	   STYLE "B" ;
	   FLOAT ;
          SYSTEM ;
          TITLE (gcSysName);
	   NOMINIMIZE ;
	   COLOR RGB(,,,192,192,192)
			  
*** Activate dialog window no show so the window will pop with messag 
*** when activated
ACTIVATE WINDOW gwdDialog NOSHOW

*** Move the dialog window to center of the screen
MOVE WINDOW gwdDialog CENTER

@ 0.000,0.000 TO 0.000,lnWinWidth ;
PEN 1, 8 ;
STYLE "1" ;
COLOR RGB(255,255,255,255,255,255)
@ 0.000,0.000 TO lnWinHight,0.000 ;
PEN 1, 8 ;
COLOR RGB(255,255,255,255,255,255)
@ 0.188,0.429 TO 0.188,lnWinWidth-.6 ;
PEN 1, 8 ;
STYLE "1" ;
COLOR RGB(128,128,128,128,128,128)
@ 0.188,0.429 TO lnWinHight-.3,0.429 ;
PEN 1, 8 ;
COLOR RGB(128,128,128,128,128,128)
@ 0.000,lnWinWidth TO lnWinHight,lnWinWidth ;
PEN 1, 8 ;
COLOR RGB(128,128,128,128,128,128)
@ 0.188,lnWinWidth-.6 TO lnWinHight-.3,lnWinWidth-.6 ;
PEN 1, 8 ;
COLOR RGB(255,255,255,255,255,255)
@ lnWinHight,0.143 TO lnWinHight,lnWinWidth ;
PEN 1, 8 ;
STYLE "1" ;
COLOR RGB(128,128,128,128,128,128)
@ lnWinHight-.3,0.4 TO lnWinHight-.3,lnWinWidth-.4 ;
PEN 1, 8 ;
STYLE "1" ;
COLOR RGB(255,255,255,255,255,255)
@ 1.5,lnTextStrt SAY lcMessag SIZE lnWinHight-3,lnWinWidth-lnRigtShft

  
IF EMPTY(lcDlgValid )
  @ lnWinHight -2 ,lnButXPos ;
    GET lnDlgOpTion ;
    PICTURE "@*HT "+lcButton;
    SIZE 1.5,lnButtSize,lnBetwButt;
    MESSAGE ""
ELSE
  @ lnWinHight -2 ,lnButXPos ;
    GET lnDlgOpTion ;
    PICTURE "@*H "+lcButton;
    SIZE 1.5,lnButtSize,lnBetwButt;
    VALID &lcDlgValid;
    MESSAGE ""
ENDIF    

  
*** When the read is activated fox will activate the window 
ACTIVATE WINDOW gwdDialog

IF !EMPTY(lcDlgMessg)
  SET BELL TO 2000,2
  ?? CHR(7)
  SET BELL TO 
ENDIF  

READ CYCLE MODAL

IF lnOldRec > 0  .AND. lnOldRec <= RECCOUNT('SydDlObj')
  GO lnOldRec IN ALIAS('SydDlObj')
ENDIF

IF !EMPTY(lcCurrDbf)
  SELECT (lcCurrDbf)
ENDIF

RELEASE WINDOW gwdDialog
SET DEVICE TO &lcDevice
POP KEY
RETURN lnDlgOpTion
