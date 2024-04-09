*:***************************************************************************
*: Program file  : SOORCNFL.PRG   
*: Program desc. : Order Confirmation Form For Flowers By Zoe [FLO09]
*: For Report    : SOORCNFL.FRX
*: System        : Aria Advantage Series 4XP.
*: Module        : Sales Order (SO)
*: Developer     : Walid Hamed (WLD)
*: Date          : 07/09/2007
*: Reference     : C200810
*!****************************************************************************
*! Calls :
*!         Procedures  : ....
*!         Functions   : 
*!****************************************************************************
*! Called From         : System Menu (SO --> Output --> Order Confirmation)
*!****************************************************************************
*! Passed Parameters   : None
*!****************************************************************************
*! Example             : DO SOORCNFL
*!****************************************************************************
*! Modification        :
*Adding 5th selling Price 07/09/2007 
*B608285,1 MMT 09/25/2007 fix bug of not printing more than 5 scale sizes.	  [T20070730.0004]
*C200968,1 MMT 03/23/2008 Convert Sablon Information to be Profiles in Aria4xp[T20080214.0006]
*C200968,2 MMT 08/07/2008 Fix bug of wrong sablon and acc. colors [T20080214.0006]
*!****************************************************************************

*-- Define Arrays to Hold the codes data for SABLON Code ,SABLON Color , Trim Color

STORE '' TO lcSblName , lcSblColor , lcTrmColor
STORE 0 TO lnOldAls 
STORE .F. TO llMScale
DIMENSION laFacCode[6]
lnOldAls = SELECT(0)
lnMjrLen   = LEN(gfItemMask("PM"))             && Style major length.
llMScale   = gfGetMemVar('M_USEEXSSC')
lnSepMins  = lfvGtSclSp()                      && the length of Scale with Separator.
lnColorLen = LEN(gfItemMask("PN"))-lnSepMins   && Style Color length.
= lfCrtTemp()
= lfGetData()
 
=gfOpentable(oAriaApplication.SysPath+'SYCFACT',oAriaApplication.SysPath+'CFACCODE','SH')

SELECT (lcTempOrd)
USE
IF !USED(lcTmpCode)
  =gfOpentable(oAriaApplication.WorkDir+lcTmpQty,oAriaApplication.WorkDir+lcTmpQty,'SH', @lcTempOrd, .T.)
ELSE
  SELECT (lcTmpCode)
  USE
  =gfOpentable(oAriaApplication.WorkDir+lcTmpCode,oAriaApplication.WorkDir+lcTmpCode,'SH', @lcTempOrd, .T.)
ENDIF
SELECT ORDHDR
SET RELATION TO cordtype+ order INTO (lcTempOrd) ADDITIVE

SELECT (lcTempOrd)
SET RELATION TO 'S'+SUBSTR(Style,1,lnMajorLen) INTO OBJLINK_A ADDITIVE
SET RELATION TO SUBSTR(Style,1,lnMajorLen) INTO Style ADDITIVE
SET RELATION TO 'N' INTO (lcNoteLns) ADDITIVE
SELECT (lnOldAls)
SET SKIP TO &lcSkipExpr

loOGScroll.cCROrientation='L'
loOGScroll.cCRPapersize = 'Legal'

*-- END OF REPORT PROG.
*!*************************************************************
*! Name      : lfCrtTemp
*! Developer : Walid Hamed (WLD)
*! Date      : 07/09/2007
*! Purpose   : Create Temp File to collect the SoCodes Data
*!*************************************************************
*! Called from : SOORCNFL.PRG
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCrtTemp()
*!*************************************************************
FUNCTION lfCrtTemp
*Adding 5th selling Price 07/09/2007 [Begin]
*DIMENSION laFileStru[80,4]
DIMENSION laFileStru[81,4]
*Adding 5th selling Price 07/09/2007 [End]

laFileStru[ 1 , 1 ] = 'CORDTYPE' 
laFileStru[ 1 , 2 ] = 'C' 
laFileStru[ 1 , 3 ] = 1          
laFileStru[ 1 , 4 ] = 0

laFileStru[ 2 , 1 ] = 'ORDER'
laFileStru[ 2 , 2 ] = 'C'
laFileStru[ 2 , 3 ] = 6
laFileStru[ 2 , 4 ] = 0

laFileStru[ 3 , 1 ] = 'Account'
laFileStru[ 3 , 2 ] = 'C'
laFileStru[ 3 , 3 ] = 5
laFileStru[ 3 , 4 ] = 0

laFileStru[ 4 , 1 ] = 'Store'
laFileStru[ 4 , 2 ] = 'C'
laFileStru[ 4 , 3 ] = 8
laFileStru[ 4 , 4 ] = 0

laFileStru[ 5 , 1 ] = 'Note_Mem'
laFileStru[ 5 , 2 ] = 'M'
laFileStru[ 5 , 3 ] = 10
laFileStru[ 5 , 4 ] = 0

laFileStru[ 6 , 1 ] = 'LINENO'
laFileStru[ 6 , 2 ] = 'N'
laFileStru[ 6 , 3 ] = 6
laFileStru[ 6 , 4 ] = 0

laFileStru[ 7 , 1 ] = 'STYLE'
laFileStru[ 7 , 2 ] = 'C'
laFileStru[ 7 , 3 ] = lnMjrLen
laFileStru[ 7 , 4 ] = 0

laFileStru[ 8 , 1 ] = 'COLOR'
laFileStru[ 8 , 2 ] = 'C'
laFileStru[ 8 , 3 ] = lnColorLen
laFileStru[ 8 , 4 ] = 0

laFileStru[ 9 , 1 ] = 'CSBLNAME'
laFileStru[ 9 , 2 ] = 'C'
laFileStru[ 9 , 3 ] = 30
laFileStru[ 9 , 4 ] = 0

laFileStru[ 10, 1 ] = 'CSBLCOLOR'
laFileStru[ 10, 2 ] = 'C'
laFileStru[ 10, 3 ] = 30
laFileStru[ 10, 4 ] = 0

laFileStru[ 11, 1 ] = 'CTRMCOLOR'
laFileStru[ 11, 2 ] = 'C'
laFileStru[ 11, 3 ] = 30
laFileStru[ 11, 4 ] = 0

FOR I=1 TO 20
  laFileStru[ I+11 , 1 ] = 'SIZ' + STR(I,IIF(I<10,1,2))
  laFileStru[ I+11 , 2 ] = 'C'
  laFileStru[ I+11 , 3 ] = 5
  laFileStru[ I+11 , 4 ] = 0
ENDFOR
FOR I=1 TO 20
  laFileStru[ I+31 , 1 ] = 'QTY' + STR(I,IIF(I<10,1,2))
  laFileStru[ I+31 , 2 ] = 'N'
  laFileStru[ I+31 , 3 ] = 6
  laFileStru[ I+31 , 4 ] = 0
ENDFOR

laFileStru[ 52 , 1 ] = 'TOTQTY'
laFileStru[ 52 , 2 ] = 'N'
laFileStru[ 52 , 3 ] = 8
laFileStru[ 52 , 4 ] = 0

FOR I=1 TO 20
  laFileStru[ I+52 , 1 ] = 'BOOK' + STR(I,IIF(I<10,1,2))
  laFileStru[ I+52 , 2 ] = 'N'
  laFileStru[ I+52 , 3 ] = 6
  laFileStru[ I+52 , 4 ] = 0
ENDFOR

laFileStru[ 73 , 1 ] = 'TOTBOOK'
laFileStru[ 73 , 2 ] = 'N'
laFileStru[ 73 , 3 ] = 8
laFileStru[ 73 , 4 ] = 0

*Adding 5th selling Price 07/09/2007 [Begin]
*!*	FOR I=1 TO 4
*!*	  laFileStru[ I+73 , 1 ] = 'PRICE' + STR(I,1)
*!*	  laFileStru[ I+73 , 2 ] = 'N'
*!*	  laFileStru[ I+73 , 3 ] = 7
*!*	  laFileStru[ I+73 , 4 ] = 2
*!*	ENDFOR

*!*	laFileStru[ 78 , 1 ] = 'TOTQAMNT'
*!*	laFileStru[ 78 , 2 ] = 'N'
*!*	laFileStru[ 78 , 3 ] = 10
*!*	laFileStru[ 78 , 4 ] = 2

*!*	laFileStru[ 79 , 1 ] = 'TOTBAMNT'
*!*	laFileStru[ 79 , 2 ] = 'N'
*!*	laFileStru[ 79 , 3 ] = 10
*!*	laFileStru[ 79 , 4 ] = 2

*!*	laFileStru[ 80 , 1 ] = 'CGroup'
*!*	laFileStru[ 80 , 2 ] = 'C'
*!*	laFileStru[ 80 , 3 ] = 3
*!*	laFileStru[ 80 , 4 ] = 0
FOR I=1 TO 5
  laFileStru[ I+73 , 1 ] = 'PRICE' + STR(I,1)
  laFileStru[ I+73 , 2 ] = 'N'
  laFileStru[ I+73 , 3 ] = 7
  laFileStru[ I+73 , 4 ] = 2
ENDFOR

laFileStru[ 79 , 1 ] = 'TOTQAMNT'
laFileStru[ 79 , 2 ] = 'N'
laFileStru[ 79 , 3 ] = 10
laFileStru[ 79 , 4 ] = 2

laFileStru[ 80 , 1 ] = 'TOTBAMNT'
laFileStru[ 80 , 2 ] = 'N'
laFileStru[ 80 , 3 ] = 10
laFileStru[ 80 , 4 ] = 2

laFileStru[ 81 , 1 ] = 'CGroup'
laFileStru[ 81 , 2 ] = 'C'
laFileStru[ 81 , 3 ] = 3
laFileStru[ 81 , 4 ] = 0
*Adding 5th selling Price 07/09/2007 [End]

CREATE TABLE (gcWorkDir+lcTmpCode) FROM ARRAY laFileStru
INDEX ON cordtype+order+Store+Style+Color+STR(LineNo,6)+ALLTRIM(CSBLNAME) + ;
         ALLTRIM(CSBLCOLOR) + ALLTRIM(CTRMCOLOR) TAG (lcTmpCode)

*-- END OF FUNCTION lfCrtTemp.

*!*************************************************************
*! Name      : lfGetData
*! Developer : Walid Hamed (WLD)
*! Date      : 07/09/2007
*! Purpose   : Function To Collect data from the Socodes File
*!             and save it in lcTmpCode with codes's Description
*!*************************************************************
*! Called from : SOORCNFL.PRG
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetData()
*!*************************************************************
FUNCTION lfGetData

PRIVATE lnCurTBook , lnCurTQty , lnLineNo , lcPrice , lcGrup , lcScale , lcOrder , llFillGrup , lcStore
STORE 0 TO lnLineNo , lnCurTBook , lnCurTQty
STORE '' TO lcPrice , lcGrup , lcScale , lcOrder , lcStore

STORE .F. TO llFillGrup
*-- [lcGrup] I store 'A' as for every new Order that to not the sizes group after I Already 
  *- printed the title Header , then I go down with next Chrs. if the scale changed inside 
  *- the same Order that to Reprint Sizes Codes only Every Time Scale changing
*-- [lcPrice] I'm using this variable to get No. of the price that by getting the scale Sequence
  *- No. from the laSize array (row 2)
*-- [llFillGrup] I used this variable only to hold .T. if the codition of the new order achieved
  *- because i get a new lcGrup that will be 'A' in this case but if the codition of a new scale
  *- achieved too the the new order i'll go to get a new lcGrup that will be 'B' and this in wrong


*C200968,1 MMT 03/23/2008 Convert Sablon Information to be Profiles in Aria4xp[Start]
gfOpenTable('PROFVALU','PROFILE','SH')
*C200968,1 MMT 03/23/2008 Convert Sablon Information to be Profiles in Aria4xp[End]

=gfOpenTable('SOCODES','SOCODES','SH','SOCODES')

SELECT (lcTempOrd)
LOCATE

SCAN
  IF !(lcOrder == &lcTempOrd..Order) .OR. !(lcStore == &lcTempOrd..Store)
    lcOrder = &lcTempOrd..Order
    lcStore = &lcTempOrd..Store
    IF !(EMPTY(lcGrup) .AND. llFillGrup)
      lcGrup     = ''
      lcGrup     = CHR(65)
      llFillGrup = .T.
    ENDIF
  ENDIF
  IF lcScale <> SUBSTR(SCALE,1,1)
    IF !EMPTY(lcScale)
      lnLineNo = lnLineNo + 1
    ENDIF
    lcScale = SUBSTR(SCALE,1,1)
    IF !llFillGrup
      lcGrup  = IIF(EMPTY(lcGrup) , CHR(65) , CHR(65 +(ASC(lcGrup)-65) + 1))
    ENDIF
    =lfGetSizes()
  ENDIF
  llFillGrup = .F.
  SCATTER MEMVAR MEMO
  =lfGetSblCO()

  IF gfSEEK(cOrdType + Order + STORE + SUBSTR(Style,1,lnMjrLen) + ;
         SUBSTR(Style,lnMjrLen+2,lnColorLen)+ STR(lnLineNo,6) + ;
         lcSblName + lcSblColor + lcTrmColor,lcTmpCode) 
    lnCurTBook = m.BOOK1+m.BOOK2+m.BOOK3+m.BOOK4+m.BOOK5 
    lnCurTQty  = m.QTY1+m.QTY2+m.QTY3+m.QTY4+m.QTY5
    SELECT (lcTmpCode)
    
    *B608285,1 MMT 09/25/2007 fix bug of not printing more than 5 scale sizes[Start]
    *FOR N = 1 TO IIF(SCALE.CNT > 5 , 5 , SCALE.CNT)
    FOR N = 1 TO SCALE.CNT
    *B608285,1 MMT 09/25/2007 fix bug of not printing more than 5 scale sizes[End]
    
      lcI      = STR(N,1)
      lnSizeNo = ASCAN(laSize , SCALE.SZ&lcI,1)
      lcI2     = STR(lnSizeNo , IIF(lnSizeNo<10,1,2))
      *Adding 5th selling Price 07/09/2007 [Begin]
      *lcPrice  = IIF(laSize[2,lnSizeNo]>4,'',STR(laSize[2,lnSizeNo],1))
       lcPrice  = IIF(laSize[2,lnSizeNo]>5,'',STR(laSize[2,lnSizeNo],1))
      *Adding 5th selling Price 07/09/2007 [End]
      REPLACE QTY&lcI2         WITH QTY&lcI2  + m.QTY&lCI   ,;
              BOOK&lcI2        WITH BOOK&lcI2 + m.BOOK&lCI  ,;
              TotQty           WITH TotQty  + m.QTY&lCI     ,;
              TotBook          WITH TotBook + m.Book&lCI
      IF !EMPTY(lcPrice)
        REPLACE PRICE&lcPrice  WITH m.PRICE
      ENDIF
    ENDFOR           
      REPLACE TOTQAMNT         WITH (TOTQAMNT+(PRICE&lcPrice * lnCurTQty)) ,;
              TOTBAMNT         WITH (TOTBAMNT+(PRICE&lcPrice * lnCurTBook))      
    lnCurTBook = 0
    lnCurTQty  = 0
  ELSE
    SELECT (lcTmpCode)
    lnLineNo = lnLineNo + 1
    APPEND BLANK
    REPLACE cOrdType           WITH m.cOrdType                              ,;
            Order              WITH m.Order                                 ,;
            Account            WITH m.Account                               ,;
            Store              WITH m.Store                                 ,;
            cGroup             WITH lcGrup                                  ,;
            Note_Mem           WITH m.Note_Mem                              ,;
            LineNO             WITH lnLineNo                                ,;
            Style              WITH SUBSTR(m.Style,1,lnMjrLen)              ,;
            Color              WITH SUBSTR(m.Style,lnMjrLen+2,lnColorLen)   ,;
            CSblName           WITH lcSblName                               ,;
            CSblColor          WITH lcSblColor                              ,;
            CTrmColor          WITH lcTrmColor                              ,;
            TOTQTY             WITH m.QTY1+m.QTY2+m.QTY3+m.QTY4+m.QTY5      ,;
            TOTBOOK            WITH m.BOOK1+m.BOOK2+m.BOOK3+m.BOOK4+m.BOOK5 
            
    *B608285,1 MMT 09/25/2007 fix bug of not printing more than 5 scale sizes[Start]
    *FOR N = 1 TO IIF(SCALE.CNT > 5 , 5 , SCALE.CNT)
    FOR N = 1 TO SCALE.CNT
    *B608285,1 MMT 09/25/2007 fix bug of not printing more than 5 scale sizes[End]
    
      lcI      = STR(N,1)
      lnSizeNo = ASCAN(laSize , SCALE.SZ&lcI,1)
      lcI2     = STR(lnSizeNo , IIF(lnSizeNo<10,1,2))
      *Adding 5th selling Price 07/09/2007 [Begin]
      *lcPrice  = IIF(laSize[2,lnSizeNo]>4,'',STR(laSize[2,lnSizeNo],1))
       lcPrice  = IIF(laSize[2,lnSizeNo]>5,'',STR(laSize[2,lnSizeNo],1))
      *Adding 5th selling Price 07/09/2007 [End]
      REPLACE QTY&lcI2  WITH m.QTY&lCI
      REPLACE BOOK&lcI2 WITH m.BOOK&lCI
      IF !EMPTY(lcPrice)
        REPLACE PRICE&lcPrice  WITH m.PRICE                           
      ENDIF
    ENDFOR           
      REPLACE   TOTBAMNT       WITH TOTBOOK*PRICE&lcPrice              ,;
                TOTQAMNT       WITH TOTQTY*PRICE&lcPrice                 
    FOR N = 1 TO 20
      lcI = STR(N,IIF(N<10,1,2))
      REPLACE SIZ&lcI WITH laSize[1,N]
    ENDFOR           
  ENDIF
ENDSCAN

*-- END OF FUNCTION lfGetData.

*!*************************************************************
*! Name      : lfGetSblCO
*! Developer : Walid Hamed (WLD)
*! Date      : 07/09/2007
*! Purpose   : Function To Collect data from the Socodes File
*!             and save it in lcTmpCode with codes's Description
*!*************************************************************
*! Called from : SOORCNFL.PRG
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetSblCO()
*!*************************************************************
FUNCTION lfGetSblCO

*-- Get Sablon Codes in the temp file
*C200968,1 MMT 03/23/2008 Convert Sablon Information to be Profiles in Aria4xp[Start]
*!*	IF gfSEEK(m.cOrdType + m.Order + STR(m.LineNo,6),'SOCODES')
*!*	  lcSblName  = ALLTRIM(gfCodDes(SOCODES.CSblName  , 'CSBLNAME' ))
*!*	  lcSblColor = ALLTRIM(gfCodDes(SOCODES.CSblColor , 'CSBLCOLOR'))  
*!*	  lcTrmColor = ALLTRIM(gfCodDes(SOCODES.CTrmColor , 'CTRMCOLOR'))
*!*	ELSE
*!*	  STORE '' TO lcSblName,lcSblColor,lcTrmColor
*!*	ENDIF
  lcAlias  = SELECT()
  IF gfSeek('SO'+ m.cOrdType + m.Order + STR(m.LineNo,6),'PROFVALU')
    SELECT PROFVALU
    SCAN REST WHILE CPRO_TYPE+CKEY+CPRO_CODE = 'SO'+ m.cOrdType + m.Order + STR(m.LineNo,6)
      IF UPPER(ALLTRIM(gfCodDes(PROFVALU.CPRO_CODE  , 'CPRO_CODE' ))) = 'SCREEN CODE'
 		lcSblName = ALLTRIM(profvalu.cpro_value)       
 		LOOP 
      ENDIF 
      
      IF UPPER(ALLTRIM(gfCodDes(PROFVALU.CPRO_CODE  , 'CPRO_CODE' ))) = UPPER('Accessory Color')
      
        *C200968,2 MMT 08/07/2008 Fix bug of wrong sablon and acc. colors [Start]
        *lcSblColor = ALLTRIM(profvalu.cpro_value)       
        lcTrmColor= ALLTRIM(profvalu.cpro_value)       
 		*C200968,2 MMT 08/07/2008 Fix bug of wrong sablon and acc. colors [End]
 		
 		LOOP 
      ENDIF 

      IF UPPER(ALLTRIM(gfCodDes(PROFVALU.CPRO_CODE  , 'CPRO_CODE' ))) = UPPER('Screen Color')
        
        *C200968,2 MMT 08/07/2008 Fix bug of wrong sablon and acc. colors [Start]
        *lcTrmColor= ALLTRIM(profvalu.cpro_value)       
 		lcSblColor = ALLTRIM(profvalu.cpro_value)      
 		*C200968,2 MMT 08/07/2008 Fix bug of wrong sablon and acc. colors [End]
 		
 		LOOP 
      ENDIF 
    ENDSCAN 
  ELSE
    STORE '' TO lcSblName,lcSblColor,lcTrmColor
  ENDIF 
  SELECT(lcAlias)
*C200968,1 MMT 03/23/2008 Convert Sablon Information to be Profiles in Aria4xp[End]

*-- END OF FUNCTION lfGetSblCO.
*!*************************************************************
*! Name      : lfvGtSclSp
*! Developer : Walid Hamed (WLD)
*! Date      : 07/09/2007
*! Purpose   : Function returns the length of Scale with Separator.
*!*************************************************************
*! Returns   : len of Scale 3 or 4 if separator exist.
*!*************************************************************
*! Example   : = lfvGtSclSp()
*!*************************************************************
FUNCTION lfvGtSclSp
PRIVATE lnAlias
lnAlias = SELECT(0)

IF llMScale
   *llStruOp
   =gfOpenTable('ICISTRU','Segno','SH','ICISTRU')
  SELECT ICISTRU
  SCAN 
    IF cItemRecty='U' AND cISegType='S'
      SKIP -1
      lnSepMins = IIF(!lSegEndMaj AND !EMPTY(cISegSepr),4,3)
      EXIT 
    else
      lnSepMins = 3
    ENDIF
  ENDSCAN
  *IF USED('ICISTRU') &&AND llStruOp
  *  USE IN ICISTRU
  *ENDIF
ELSE
  lnSepMins = 0
ENDIF
SELECT(lnAlias)
RETURN (lnSepMins)

*--End of lfvGtSclSp.
*!*************************************************************
*! Name      : lfFacCode
*! Developer : Walid Hamed (WLD)
*! Date      : 07/09/2007
*! Purpose   : Get the Factor Addresses If there is a factor for 
*!           : the order in the Ordhdr file
*!*************************************************************
*! Called from : SOORCNFL.Frx
*!*************************************************************
*! Passed Para.: None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFacCode()
*!*************************************************************
FUNCTION lfFacCode
PARAMETER lcReturn
PRIVATE lnAlias
lnAlias = SELECT(0)

DIMENSION laFacCode[6]

SELECT SYCFACT
IF !EMPTY(ORDHDR.CFACCODE) .AND. gfSEEK(ORDHDR.CFACCODE,'SYCFACT')
  laFacCode[1] = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
  laFacCode[2] = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
  laFacCode[3] = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
  laFacCode[4] = gfGetAdr('SYCFACT' , '' , '' , '' , 4)
  laFacCode[5] = gfGetAdr('SYCFACT' , '' , '' , '' , 5)
  *--search for the Phone No. in the SycFact File and show it with the Phone Format
  laFacCode[6] = 'Tel: '+ TRANSFORM(LOOKUP(sycFact.CPhoneNo,OrdHdr.CfacCode,;
                                           SycFact.cFacCode,'cFacCode'), lcPhonPict)
  = lfAdrShift('laFacCode')    && Shift Company address if there is empty line.
ENDIF
SELECT(lnAlias)
RETURN
*--End of lfFacCode.

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Walid Hamed (WLD)
*! Date      : 07/09/2007
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : SOORDCNFL.PRG
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdrShift()
*!*************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 6
  
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 6
  
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- end of lfAdrShift.
*!***************************************************************************
*! Name      : lfGetSizes
*! Developer : Walid Hamed (WLD)
*! Date      : 07/09/2007
*! Purpose   : to fill the lasize array with sizes of the scale file
*!***************************************************************************
*! Called from : Report
*!***************************************************************************
*! Example     : = lfGetSizes()
*!***************************************************************************
FUNCTION lfGetSizes

PRIVATE lnOldAls , lnRecNo , lnSizNo , lnScaleNo
DECLARE laSize[2,20]
STORE '' TO laSize
STORE 0 TO lnOldAls , lnRecNo , lnSizNo , lnScaleNo
lnOldAls = SELECT(0)
lnRecNo  = RECNO('SCALE')
SELECT SCALE
SCAN FOR SUBSTR(SCALE,1) = lcScale
  lnScaleNo = lnScaleNo + 1
  
  *B608285,1 MMT 09/25/2007 fix bug of not printing more than 5 scale sizes[Start]
  *FOR I=1 TO IIF(SCALE.CNT > 5 , 5 , SCALE.CNT)
  FOR I=1 TO SCALE.CNT 
  *B608285,1 MMT 09/25/2007 fix bug of not printing more than 5 scale sizes[End]
  
    lcI = STR(I,1)
    lnSizNo = lnSizNo + 1 
    laSize[1,lnSizNo] = SZ&lcI
    laSize[2,lnSizNo] = lnScaleNo
    IF EOF() .OR. lnSizNo = 20
      EXIT
    ENDIF
  ENDFOR
ENDSCAN
IF BETWEEN (lnRecNo,1,RECCOUNT())
   GOTO lnRecNo 
ENDIF
SELECT(lnOldAls)

*-- End of lfGetSizes.

*!***************************************************************************
*! Name      : lfTotPages
*! Developer : Walid Hamed (WLD)
*! Date      : 07/09/2007
*! Purpose   : to get the total number of pages
*:             Notes : if a change is done in Frx Remember to 
*:                     1) Print the Report and count the number of lines per page 
*:                     2) Adjust the number of lines (Hardcoded) in lfTotPages()
*!***************************************************************************
*! Called from : Report
*!***************************************************************************
*! Example     : = lfTotPages()
*!***************************************************************************
FUNCTION lfTotPages
PARAMETER lcReturn
PRIVATE lnRecNo,lnRecCount , lnRowNo , lcGroup , lcStore , lnOldAls
STORE '' TO lcGroup , lcStore
STORE 0  TO lnRecNo,lnRecCount , lnRowNo , lnOldAls
lnOldAls = SELECT(0)

SELECT (lcTempOrd)
lnRecNo = RECNO()
lnRowNo    = 22
LOCATE FOR &lcTempOrd..ORDER = ORDHDR.ORDER
lcGroup = cGroup
lcStore = Store

SCAN FOR &lcTempOrd..ORDER = ORDHDR.ORDER
  lnRecCount = lnRecCount + 1
  IF !(lcGroup==cGroup)
    lcGroup=cGroup
    lnRecCount = lnRecCount + 1
  ENDIF

  IF !(lcStore == Store)
    lcStore = Store
    lnRecCount = lnRecCount + lnRowNo
  ENDIF
ENDSCAN

lnPages = CEILING(lnRecCount/lnRowNo)

IF BETWEEN (lnRecNo,1,RECCOUNT())
   GOTO lnRecNo 
ENDIF

SELECT(lnOldAls)
RETURN ''
*-- End of lfTotPages.
