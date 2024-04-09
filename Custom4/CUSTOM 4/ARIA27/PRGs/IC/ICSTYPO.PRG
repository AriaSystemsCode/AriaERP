*!***********************************************************************************************
*:
*: Procedure file: icstypo.PRG 
*:                 (Style Position)
*:
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: Inventory Contol
*:         Author: WAB - Walid A. Wahab
*:		   Date  : 06/01/1999
*:      Copyright (c) 
*!***********************************************************************************************
*: Last modified : 
*!***********************************************************************************************
*:           Functions  : lpShow,KeyValidation,lfvData_1,lfwstypos
*:                      : 
*:                      : 
*!***********************************************************************************************
*: Passed Parameters  : None
*!***********************************************************************************************
*: Modifications      :
*:B802594,1 WAB 09/07/1999 The button called No Name Drop is displaying the deign where it should 
*:B802594,1                display the Desgin Sizes.Also, have the ability to assign design codes to a style major, 
*:B802594,1                all non majors, without having to selet all non majors.
*:B603200,1 WAB 10/10/1999 Fix the bug 'after the user selects style and all colors they immeidately get a 
*:B603200,1 WAB            Style+Color pop-up browse window.
*:B603211,2 WAB 10/17/1999 when new colors were added. In Edit mode the program should 
*:B603211,2 WAB            update all the style colors with the same positions.
*!***************************************************************************
EXTERNAL ARRAY laData,laKeyField,laDefProc

*--Define key FIELD array, ,Array For List Element
*DECLARE laKeyField[2,4],lanonmdrp[1],la1nmdrp[1],la2nmdrp[1]
DECLARE laKeyField[1,4],lanonmdrp[1],la1nmdrp[1],la2nmdrp[1]
lcfldseek = ''

*--Define Code information Aray for global Function gfwCodePop,
*--WAB -Add onther codes (Design codes  & array for Holding  codes)
*DECLARE laCodeInfo[1,10],lastypos[1,10]
DECLARE laCodeInfo[2,10],lastypos[1,10],laDesign[1]
*--Declare Arrays For mover Window
DECLARE laDsgnCode[1]


*--For Position Popup
laCodeInfo[1,1]  = 'CSTYLEPOS'
laCodeInfo[1,2]  = 'lastypos'
laCodeInfo[1,3]  = 'lnstypos'
laCodeInfo[1,4]  = ''
laCodeInfo[1,5]  = .F.
laCodeInfo[1,6]  = .F.
laCodeInfo[1,7]  = ''
laCodeInfo[1,8]  = ''
laCodeInfo[1,9]  = ''
laCodeInfo[1,10] = ''

*--WAB -  For Design Size
laCodeInfo[2,1]  = 'cDsgnSize'
laCodeInfo[2,2]  = 'laDesign'
laCodeInfo[2,3]  = 'puDesign'
laCodeInfo[2,4]  = ''
laCodeInfo[2,5]  = .F.
laCodeInfo[2,6]  = .F.
laCodeInfo[2,7]  = ''
laCodeInfo[2,8]  = ''
laCodeInfo[2,9]  = ''
laCodeInfo[2,10] = ''


*--The Tag Expression of the default Selected Database File
laKeyField[1,1] = 'lcfldseek'
laKeyField[1,2] = .T.
laKeyField[1,3] = 'icstypos'
laKeyField[1,4] = 1
*laKeyField[2,1] = 'laData[2]'
*laKeyField[2,2] = .T.
*laKeyField[2,3] = 'icstypos'
*laKeyField[2,4] = 2

*--Initialize screen and program variables .
llChkDsgn    = .F.					&& check if there are design code in icdesgn table
lcScFields   = ''	
llBrowse     = .F.					&&  Browse Window Is Not Active
store space(20) to lcStylDesc	    &&  style description
lanonmdrp = ' '                  && No Name Drop 
la1nmdrp  = ' '   				&& 1 Name Drop 
la2nmdrp  = ' '					&& 1 Name Drop 
lsNonmdrp    = 0					&& variable for (No Name Drop ) list 
ls1nmdrp    = 0					&& variable for (1 Name Drop ) list 
ls2nmdrp    = 0					&& variable for (1 Name Drop ) list 
lnstypos = 0

*B802594,1 WAB - No Need For Get item Mastk cause we will get the major mastk and nonmajor
*B802594,1 WAB - And Intialise Variables And Declare Arrays
*B802594,1 WAB - START
*lcStyPic   = gfItemMask("PI")       && get style picture
*lcStyTtl   = gfItemMask("HI")       && get style Title
*lnStyleWid = LEN(lcStyPic)          && style width

*------ lcMjrTtl    ---> hold Major title
*------ lcMajor     ---> hold Major Field
*------ lcMjrPct    ---> hold MAjor picture
*------ lcNMjrTl    ---> hold Non Major Title
*------ lcNMjrPt    ---> hold Non MAjor picture
*------ lcNmjrDisc  ---> hold Non majr decription
*------ lcSepart    ---> hold speratore between major nad non major
*------ lcMjrDesc   ---> hold major description
*------ laSegInfo   ---> hold segment information
*------ laMajSeg    ---> hold major segment
*------ llDspClrDs  ---> hold Check For Color Segment Exsisting
*------ lnColorWid  ---> hold the length of nonmajor picture
*------ lnstylewid  ---> hold the length of major picture
*------ lnGetXPos   ---> hold Position of color Fields 
*------ lnXPos      ---> hold Position fo style Fields 
*------ llMScale    ---> hold check for use extended size scale
*------ lnClrPos    ---> hold color position
*------ lnStrtPos   ---> hold strat color position at style field
*------ lnFldLngth  ---> hold color length
*------ llallColr   ---> hold check for all colors
*------ llLastRec   ---> hold Check For LastRecord
DECLARE laSegInfo[1,9],laMajSeg[1,1],laNMjr[1,2]
STORE  '' TO lcMjrTtl,lcMajor,lcNonMjr, lcMjrPct,lcNMjrTl,lcNMjrPt,lcNmjrDisc,lcSepart,lcMjrDesc
STORE  '' TO laSegInfo,laMajSeg
STORE .F. TO llDspClrDs,llMScale,llallColr,llLastRec
STORE  0  TO lnColorWid,lnstylewid,lnGetXPos,lnClrPos,lnStrtPos,lnFldLngth
STORE 25 TO lnXPos
STORE 25 TO lnGetXPos

*--Use extended size scale.  
llMScale   = gfGetMemVar('M_USEEXSSC')

laNMjr[1,1] = 'CLRLNAME'
laNMjr[1,2] = 'lcNmjrRlt'

laDefProc[1]  = .F.              && Disable the control panel prev proc.(lpTopScr)
laDefProc[2]  = .F.              && Disable the control panel prev proc.(lpBtmScr)
laDefProc[3]  = .F.              && Disable the control panel prev proc.(lpNxtScr)
laDefProc[4]  = .F.              && Disable the control panel prev proc.(lpPrvScr)
laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)
laDefProc[7]  = .F.              && Disable the control panel global delete proc. and (do lpDelScr)

STORE .F. TO llEof,llBof		&& for record postion

*B802594,1 WAB - END


*WAB - declare variables for popup codes 
puDesign = 0
cDsgnSize = space(6)

*-- open Tables, define variables like:
*-- lcWindTitle : window title
*-- lcBaseFile  : base file of the screen
*-- lcFileTtl   : File title of the base file	&&used by the global browse
*-- lcBrFields  : browse fields string of the base file &&used by the global browse
*-- lcScFields  : ''
IF !gfSetup()    
  RETURN
ENDIF  

*B802594,1 WAB - call Function to get segment information.
*B802594,1 WAB - START

IF !lfSegInfo() 
  *--Item structure not found, Cannot Proceed.
  =gfModalGen('QRM42080B42001','DIALOG','Item structure not found')
  glQuitting = .T.
  RETURN
ENDIF
*--Check For Color Segment Exsisting
*--If !exists Don't display The Color Discription SayField
FOR I=1 TO ALEN(laSegInfo,1)
  IF laSegInfo[I,3] ='C'
    llDspClrDs = .T.
  ENDIF
ENDFOR

*B802594,1 WAB - END

*-- WAB - call the gf. win code poppup for fill the ladesgn array 
=gfwCodePop(@laCodeInfo,'cDsgnSize','D')
=gfwCodePop(@laCodeInfo,'cDsgnSize','L')

*-- fields list to be scattered to Ladata array
lcScFields = 'Style,cStylePos,m0nd_dsgn,m1nd_dsgn,m2nd_dsgn,cStyMajor'

*-- Global Function For Reintialised User Define Fields
*=gfReInUsrF(lcScFields,@laUsrFields)

*-- Open Table Vendor 

*-- Open Table STYLE
=gfOpenFile(gcDataDir+'STYLE','STYLE','SH')

SELECT (lcBaseFile)
*-- if ~st time to activate the window scatter an empty record
IF !WEXIST(gcBaseWind)
 SCATTER FIELDS &lcScFields MEMO TO laData BLANK
ENDIF  


*-- call the gf. win code poppup 
=gfwCodePop(@laCodeInfo,'CSTYLEPOS','N')
=gfwCodePop(@laCodeInfo,'CSTYLEPOS','L')

*B802594,1 Add a selection bar to style position popup
=lfAddElmnt()
*B802594,1 end

*-if there are no style position codes. 
IF lastypos[2,1] = 'N/A'
  =gfModalGen('TRM42189B00000','DIALOG')
  RETURN
ENDIF


*-- call the screen file ( ICSTYPO.SPX  )
DO (gcScrDir + gcWinAppl + '\ICSTYPO.SPX')

*B802594,1 WAB - close files
IF USED("ICSEGVAL")
  USE IN ICSEGVAL
ENDIF
IF USED("SYCCOMP")
  USE IN SYCCOMP
ENDIF
*B802594,1 WAB - END
IF glQuitting
  *-- Erase temp files
  *-- Reset relations
ENDIF  

*-- Functions and Procedures :
*!***************************************************************************
*! Name      : lpShow
*! Developer : WAB - Walid A. Wahab
*! Date      : 06/01/1999
*! Purpose   : Show objects function.
*!***************************************************************************
*! Calls     : lfRefresh()   
*!***************************************************************************
*! Parameters: None
*!***************************************************************************
*! Returns   :  None.
*!***************************************************************************
*! Example   :  =lpShow()
*!***************************************************************************
FUNCTION lpShow
DO CASE
  ***--- S E L E C T   M O D E ---***
  CASE laScrMode[1]
    *-- Reset No Name Drop, 1 Name Drop, 1 Name Drop, lcStylDesc 
    lnstypos   = 1 
    STORE '' TO lanonmdrp, la1nmdrp, la2nmdrp, lcStylDesc
    SHOW GET lnstypos   ENABLE

    *B802594,1 WAB - get the browse buttons and the key fileds enabled in select mode
    *B802594,1 WAB - START
    llallColr = .F.

    *-- Reset lcMajor, lcNonMjr, lcNMjrDisc 
    STORE '' TO lcMajor, lcNonMjr, lcNMjrDisc
    SET ORDER TO TAG icStyPos
    *B802594,1 WAB - END

  ***--- V I E W   M O D E ---***
  CASE laScrMode[2]
    
    lnstypos = ASCAN(lastypos,laData[2]) / 2
    lnstypos = Round(lnstypos,0)

    *B802594,1 WAB - case all colors seek with lcmajor else seek with style(ladata[1])
    *B802594,1 WAB - START
    *lcStylDesc = LOOKUP(STYLE.DESC, laData[1], STYLE.STYLE, 'STYLE')     

    IF !llallColr
      lcStylDesc = LOOKUP(STYLE.DESC, laData[1], STYLE.STYLE, 'STYLE')     
    ELSE
      lcStylDesc = LOOKUP(STYLE.DESC, lcMajor, STYLE.STYLE, 'STYLE')     
    ENDIF

    *B802594,1 WAB - START
    
    =lfgetlist()
    SHOW GET lnstypos  DISABLE

    *B802594,1 WAB - get the lcmajor and the lcnonmajor from ladata[1]
    *B802594,1 WAB - and get the browse buttons and the key fileds DISABLED in view mode
    *B802594,1 WAB - START
    lcMajor = SUBSTR(laData[1],1,lnStrtPos-2)
    IF !llallColr
      lcNonMjr= SUBSTR(laData[1],lnStrtPos,lnFldLngth)
    ENDIF
    llallcolors = (!EMPTY(lcMajor) AND (EMPTY(lcNonMjr) OR SUBSTR(lcNonMjr,1,1)='*'))
    lcCodGet    = IIF(llMScale AND !llallcolors,SUBSTR(lcNonMjr,1,LEN(lcNonMjr)-3),lcNonMjr)
    lcNMjrDisc  = gfCodDes(lcCodGet,'COLOR')
    
    IF llallColr
      SET ORDER TO TAG StyMajor
    ELSE
      SET ORDER TO TAG icStyPos
    ENDIF
    lcDispTyp = IIF(llLastRec,"DISABLE","ENABLE")
    SHOW GET pbNxt  &lcDispTyp    
    SHOW GET pbBtm  &lcDispTyp    
    =lfShowNavg()
    *B802594,1 WAB - END

    
  ***--- E D I T   M O D E ---***
  CASE laScrMode[3]

    *B802594,1 WAB - get the browse buttons and the key fileds DISABLED in EDIT mode
    *B802594,1 WAB - START

    *SHOW GET ibStyle   DISABLE
    *SHOW GET ibColor   DISABLE
    *SHOW GET lcMajor   DISABLE
    *SHOW GET lcNonMjr  DISABLE
    *SHOW GET lcNMjrDisc DISABLE    
    *SHOW GET lcStylDesc DISABLE    

    *B802594,1 WAB - END


  ***--- A D D   M O D E ---***
  CASE laScrMode[4]

    *B802594,1 WAB - get the browse buttons and the key fileds DISABLED in view mode
    *B802594,1 WAB - START

    *SHOW GET ibStyle   DISABLE
    *SHOW GET ibColor   DISABLE
    *SHOW GET lcMajor   DISABLE
    *SHOW GET lcNonMjr  DISABLE
    *SHOW GET lcNMjrDisc DISABLE    
    *SHOW GET lcStylDesc DISABLE    

    *B802594,1 WAB - END

    
    
ENDCASE
SELECT (lcBaseFile)

*!***************************************************************************
*! Name      : lfvData_1
*! Developer : WAB - Walid A. Wahab
*! Date      : 06-01-99
*! Purpose   : valid and browse Styles 
*!***************************************************************************
*! Calls     : gfstybrw()
*!***************************************************************************
*! Parameters: None
*!***************************************************************************
*! Returns   :  None.
*!***************************************************************************
*! Example   :  =lfvData_1()
*!***************************************************************************
FUNCTION lfvData_1

  PRIVATE lcOldArea
  lnOldArea = SELECT(0)
  SELECT STYLE

  *B802594,1 WAB - commit this line and call style browse function with parameter "M" ( major )
  *B802594,1 WAB - So We Can Browse major only
  *B802594,1 WAB - START

  *IF llBrowse .OR. (!EMPTY(laData[1]) .AND. LASTKEY() = 13) 
  *  IF !SEEK(laData[1],"Style") .AND. !llbrowse
  *    llbrowse = .T.
  *  ENDIF
  *  IF llBrowse
  *     laData[1] = gfStyBrw('I',"","",.F.)  && call global func. for browse the style
  *  ENDIF
  *  lcStylDesc = DESC 
  *ENDIF

  IF !llBrowse 
    IF !EMPTY(lcMajor) AND LASTKEY()=13 
      llBrowse = !SEEK(lcMajor,'STYLE')    
      lnNearRec = RECNO(0)
      IF BETWEEN(lnNearRec, 1, RECCOUNT())
      ENDIF  
    ENDIF
  ENDIF
  IF llBrowse
    lcMajor = gfStyBrw('M',lcMajor,"",.F.)
    IF EMPTY(lcMajor)
      lcStylDesc =  ' '
      laScrMode    = .F.
      laScrMode[1] = .T.
      SHOW GETS
    ELSE
      lcStylDesc = DESC   
    ENDIF
  ENDIF
  *B802594,1 WAB - END
  =lfRefresh()   
  SELECT (lnOldArea)
  llBrowse = .F.

*!***************************************************************************
*! Name      : lfvData_2
*! Developer : WAB - Walid A. Wahab
*! Date      : 06/01/99
*! Purpose   : valid & Seek For style +Postion 
*!***************************************************************************
*! Calls     : None.
*!***************************************************************************
*! Parameters: None
*!***************************************************************************
*! Returns   :  None.
*!***************************************************************************
*! Example   :  =lfvData_2()
*!***************************************************************************
FUNCTION lfvData_2
ladata[2] = lastypos[lnstypos,2]

  *B802594,1 WAB - commet this line cause validation know after select color
  *B802594,1 WAB - START  
  *lcfldseek = ladata[1]+ladata[2]
  *IF !EMPTY(laData[1]) .AND. !EMPTY(laData[2]) .AND. (lastkey() = 13 .OR. lastkey() = 9)
  *   =gfseekrec()
  *   IF EMPTY(laData[1])
  *    laScrMode    = .F.
  *    laScrMode[1] = .T.
  *    SHOW GETS
  *  ENDIF
  *ENDIF

llFound = .T.
SET ORDER TO TAG StyMajor
IF !SEEK(PADR(lcMajor,19)+laData[2])
  llFound = .F.
  *Ren 
*  lcNonMjr = SPACE(6)
  *Ren END
ENDIF

SET ORDER TO TAG icStyPos
IF (!EMPTY(lcMajor) .AND. !EMPTY(lcNonMjr) .AND. !EMPTY(laData[2])) .OR. !llFound
  =lfvNonMjr()
ENDIF
*B802594,1 WAB - END
 

*B802594,1 WAB - START
*!**************************************************************************
*! Func. Name: lfvNonMjr
*! Developer : WAB - Walid A. Wahab
*! Date      : 09/09/99
*! Purpose   : Valid function to validate non major part.
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Parameters: NONE
*!**************************************************************************
*! Returns   : None.
*!**************************************************************************
*! Example   : =lfvNonMjr()
*!**************************************************************************
FUNCTION lfvNonMjr
PRIVATE lcOldArea
lnOldArea = SELECT(0)
llbrowse = llbrowse OR '?' $ lcNonMjr
lcNonMjr = IIF(llbrowse,'?',lcNonMjr)
IF EMPTY(lcMajor) AND EMPTY(lcNonMjr) 
  RETURN
ENDIF

*B603200,1 WAB - validation fired only for  specific color
*B603200,1 WAB - START
IF SUBSTR(lcNonMjr,1,1)<>'*'			
*B603200,1 WAB - END

  SELECT STYLE
  IF llBrowse OR (LASTKEY()=13 AND !MDOWN()) 
    llCanBrow = .T.
    lcNonToSek = lcNonMjr
    IF LEN(RTRIM(lcNonMjr)) < lnColorWid
      lcNonToSek = ''
      lcNxtSep = ''
      FOR I=1 TO ALEN(laSegInfo,1)
        IF laSegInfo[I,1]='N'
          lcSeg = SUBSTR(lcNonMjr,laSegInfo[I,4]-lnStyleWid-1,laSegInfo[I,5])
          IF EMPTY(lcSeg)
            EXIT
          ENDIF
          IF I < ALEN(laSegInfo,1)
            lcNxtSep = SUBSTR(lcNMjrPt,laSegInfo[I,4]+laSegInfo[I,5]-lnStyleWid-1,1)
            lcNxtSep = IIF(lcNxtSep='X','',lcNxtSep) 
          ENDIF 
          lcNonToSek = lcNonToSek+lcSeg+lcNxtSep
        ENDIF
      ENDFOR
    ENDIF
    lcNonToSek = IIF(EMPTY(lcNonToSek),PADR(lcNonMjr,lnColorWid),lcNonToSek)
    IF SEEK(PADR(lcMajor,lnStyleWid)+lcSepart+lcNonToSek,'STYLE')
      llCanBrow = .F.
    ELSE  
      IF !SEEK(PADR(lcMajor,lnStyleWid),'STYLE')  
        lcMajor = ' '
        GO TOP
      ENDIF  
    ENDIF
    llbrowse = .F.
    IF llCanBrow
      IF !EMPTY(lcMajor)
        lcNonMjr = gfStyBrw('N',lcMajor,lcNonMjr,.F.)
        IF !EMPTY(lcNonMjr) AND SUBSTR(lcNonMjr,1,1) = '*'
          =SEEK(lcMajor,'STYLE')
        ENDIF
      ELSE
        lcItemCd = gfStyBrw('I',lcMajor+lcSepart+lcNonMjr,'',.F.)
        lcMajor  = SUBSTR(lcItemCd ,1,lnstylewid)
        lcNonMjr = SUBSTR(lcItemCd ,lnstylewid+2,lnColorwid)
      ENDIF
    ENDIF
    IF EMPTY(lcNonMjr)
      _CUROBJ=OBJNUM(lcNonMjr)
      SELECT (lnOldArea)
      RETURN
    ENDIF
    IF LEN(RTRIM(lcNonMjr))<lnColorWid
      FOR I=1 TO ALEN(laSegInfo,1)
        IF laSegInfo[I,1]='N'
          lcSeg = SUBSTR(lcNonMjr,laSegInfo[I,4]-lnStyleWid-1,laSegInfo[I,5])
          IF EMPTY(lcSeg)
            lcSeg=STRTRAN(lcSeg,' ','*') 
          ENDIF
          lcNonMjr = STUFF(lcNonMjr,laSegInfo[I,4]-lnStyleWid-1,laSegInfo[I,5],lcSeg)
        ENDIF
      ENDFOR
    ENDIF

    SELECT STYLE
    IF !EMPTY(lcMajor) AND !EMPTY(lcNonMjr) AND SUBSTR(lcNonMjr,1,1)<>'*')
      lcStylDesc = DESC1
    ENDIF
    llallcolors = (!EMPTY(lcMajor) AND (EMPTY(lcNonMjr) OR SUBSTR(lcNonMjr,1,1)='*'))
    lcCodGet  = IIF(llMScale AND !llallcolors,SUBSTR(lcNonMjr,1,LEN(lcNonMjr)-3),lcNonMjr)
    lcNMjrDisc = gfCodDes(lcCodGet,'COLOR')
    =gfRltFld(lcCodGet , @laNMjr , 'COLOR')
    laData[1]   = lcMajor + lcSepart + lcNonMjr
  ENDIF
  SHOW GET lcMajor    
  SHOW GET lcNonMjr   
  =lfRefresh()   
*B603200,1 WAB - Close IF Statement 
*B603200,1 WAB - START
ENDIF
*B603200,1 WAB - END

  SELECT (lnOldArea)
  IF !EMPTY(laData[1]).AND. !EMPTY(laData[2]) 

    *--- if all colors browse frst record if found
    *--- id specific call use global function gfseek()

    IF SUBSTR(lcNonMjr,1,1)='*'			
      *wab
      =seek(PADR(lcMajor,19))
      SET ORDER TO TAG StyMajor
      llallColr = .T.       && all colors 
      lnGetAnsr = 0			&& for get user choice at message 
      IF !seek(PADR(lcMajor,19)+laData[2])
         lnGetAnsr=gfModalGen('QRM00001B00001','DIALOG',lcMajor + ' ' + laData[2]+' ')
        DO CASE
          CASE lnGetAnsr = 1			&& choice ( browse )
            =gfBrows()
          CASE lnGetAnsr = 2			&& choice ( ADD    )
            laScrMode    = .F.
            laScrMode[4] = .T.
            SHOW GETS
          CASE lnGetAnsr = 3			&& choice ( REENTER )
            llallColr = .F.
            STORE SPACE(1) TO lcMAjor,lcNonMjr,laData[1],laData[2]
            laScrMode    = .F.
            laScrMode[1] = .T.
            SHOW GETS
        ENDCASE
      ELSE				&& record found
        laData[3]    = m0nd_Dsgn
        laData[4]    = m1nd_Dsgn
        laData[5]    = m2nd_Dsgn
        laData[6]    = cStyMajor
        laScrMode    = .F.
        laScrMode[2] = .T.
        SHOW GETS
      ENDIF
      SET ORDER TO TAG icStyPos
      RETURN
    ELSE    
      llallColr = .F.  		&& allcolrs
      lcfldseek = ladata[1]+ladata[2]
      =gfseekrec()
      IF EMPTY(laData[1])
        STORE SPACE(1) TO lcMAjor,lcNonMjr
        laScrMode    = .F.
        laScrMode[1] = .T.
        SHOW GETS
      ENDIF
    ENDIF
  ENDIF
RETURN
*B802594,1 WAB - END


*!**************************************************************************
*! Func. Name: lfvDsgnSz()
*! Developer : WAB - Walid A. Wahab
*! Date      : 06/02/1999
*! Purpose   : show Mover Window
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None.
*!**************************************************************************
*! Example   : =lfvDsgnSz()
*!**************************************************************************
FUNCTION lfvDsgnSz
PARAMETER laNameDrp, lcMoverTxt, lnDataObj, lcLstObj
PRIVATE laSource
*-- If we do not have designs size codes
IF EMPTY(laDesign[1,2])
  =gfModalGen('TRM42188B00000','DIALOG','Design Size Codes')
ELSE
  DIMENSION laSource[ALEN(laDesign, 1), 1]
  FOR lnCount = 1 TO ALEN(laDesign, 1)
    laSource[lnCount] = laDesign[lnCount, 1] 
  ENDFOR
  IF laScrMode[2]
    =gfMover(@laSource, @laNameDrp,lcMoverTxt + "design sizes")
  ELSE
    =gfMover(@laSource, @laNameDrp,"Select " + LOWER(lcMoverTxt) + "design sizes",.T.)
    laData[lnDataObj] = lfFilToMmo("laNameDrp")
  ENDIF
  SHOW GET (lcLstObj)
ENDIF
    

*!**************************************************************************
*! Func. Name: lfgetlist()
*! Developer : WAB - Walid A. Wahab
*! Date      : 06/03/1999
*! Purpose   : get list Element from Memo Fields
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None.
*!**************************************************************************
*! Example   : =lfgetlist()
*!**************************************************************************
FUNCTION lfgetlist
PRIVATE lnArrPos
=lfFilFrMmo(ladata[3],"lanonmdrp")
=lfFilFrMmo(ladata[4],"la1nmdrp")
=lfFilFrMmo(ladata[5],"la2nmdrp")
FOR lnCount = 1 TO ALEN(lanonmdrp,1)
  *B802594,1 WAB - CHANGE ARRAY NAME ( array hold design sise code)
  *B802594,1 WAB - START
  *lnArrPos = ROUND((ASCAN(laDsgnCode,lanonmdrp[i]))/2,0)
  *IF lnArrPos > 0 THEN
  *   lanonmdrp[i] = laDsgnCode[lnArrPos,1]+'--'+laDsgnCode[lnArrPos,2]
  *ENDIF
  lnArrPos = ROUND((ASCAN(ladesign,lanonmdrp[lnCount]))/2,0)
  IF lnArrPos > 0 
    lanonmdrp[lnCount] = ladesign[lnArrPos,1]
  ENDIF
  *B802594,1 WAB - END
ENDFOR
FOR lnCount = 1 TO ALEN(la1nmdrp,1)   
  lnArrPos = ROUND((ASCAN(ladesign,la1nmdrp[lnCount]))/2,0)
  IF lnArrPos > 0 
    la1nmdrp[lnCount] = ladesign[lnArrPos,1]
  ENDIF
ENDFOR
FOR lnCount = 1 TO ALEN(la2nmdrp,1)
  lnArrPos = ROUND((ASCAN(ladesign,la2nmdrp[lnCount]))/2,0)
  IF lnArrPos > 0 THEN
    la2nmdrp[lnCount] = ladesign[lnArrPos,1]
  ENDIF
ENDFOR
SHOW GET lsNoNmDrp &lcButStat
SHOW GET ls1NmDrp  &lcButStat
SHOW GET ls2NmDrp  &lcButStat
RETURN

*!**************************************************************************
*! Func. Name: lfFilFrMmo()
*! Developer : WAB - Walid A. Wahab
*! Date      : 06/03/1999
*! Purpose   : FILL an ARRAY from Memo Filed
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Parameters: lcMemoTxt && FIELD
*!**************************************************************************
*! Returns   : None.
*!**************************************************************************
*! Example   : =lfFilFrMmo()
*!**************************************************************************
FUNCTION  lfFilFrMmo
PARAMETER lcMemoTxt,lcArryName
PRIVATE lnlstElmnt
  IF EMPTY(lcMemoTxt) 
   DECLARE &lcArryName[1]
   &lcArryName = ' '
   RETURN
  ENDIF 
  lnlstElmnt = occurs(',',lcMemoTxt) + 1
  IF lnlstElmnt = 0 THEN
     lnlstElmnt = 1
  ENDIF 
  DECLARE &lcArryName[lnlstElmnt]
  &lcArryName = ''
  FOR I = 1 TO (lnlstElmnt - 1)
    FOR C= 1 to LEN(lcMemoTxt)  
      IF SUBSTR(lcMemoTxt,C,C) = ','
         &lcArryName[I] = substr(lcMemoTxt,1,C-1)
         lcMemoTxt      = substr(lcMemoTxt,C+1,len(lcMemoTxt))
         EXIT
      ENDIF
    NEXT C
  NEXT I
  &lcArryName[lnlstElmnt] = lcMemoTxt
RETURN

*!**************************************************************************
*! Func. Name: lfFilToMmo()
*! Developer : WAB - Walid A. Wahab
*! Date      : 06/06/1999
*! Purpose   : FILL an ARRAY from Memo Filed
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Parameters: lcMemoTxt && FIELD
*!**************************************************************************
*! Returns   : None.
*!**************************************************************************
*! Example   : laData[x] =lfFilTOMmo()
*!**************************************************************************
FUNCTION  lfFilToMmo
PARAMETER lcArryName
PRIVATE lnlstElmnt,lnstypos

lcMemoTxt = ''
lnlstElmnt = alen(&lcArryName)
FOR i = 1 TO lnlstElmnt
  lnStyPos = ASCAN(ladesign,&lcArryName[I]) /2
      
  *B802594,1 WAB - the postion of array depend on lnstyPos
  *B802594,1 WAB - START
  *lcMemoTxt = ALLTRIM(lcMemoTxt)+ALLTRIM(ladesign[I,2])+','

  lnstypos = ROUND(lnstypos,0)
  IF lnStyPos <> 0
    lcMemoTxt = ALLTRIM(lcMemoTxt)+ALLTRIM(ladesign[lnStyPos,2])+','
  ENDIF

  *B802594,1 WAB - END

ENDFOR
lcMemoTxt = substr(lcMemoTxt,1,len(lcMemoTxt)-1)
RETURN(lcMEmoTxt)

*B802594,1 WAB - START
*!**************************************************************************
*! Func. Name: lfoldvalue()
*! Developer : WAB - Walid A. Wahab
*! Date      : 09/08/99
*! Purpose   : Function to store old value of the current filed.
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Parameters: NONE
*!**************************************************************************
*! Returns   : None.
*!**************************************************************************
*! Example   : =lfoldvalue()
*!**************************************************************************
FUNCTION lfoldvalue
lcOldValue = EVALUATE(SYS(18))
RETURN
*B802594,1 WAB - END

*B802594,1 WAB - START
*!*************************************************************
*! Name      : lfSegInfo
*! Developer : WAB  WALID A WAHAB
*! Date      : 09/09/99
*! Purpose   : Bild array for segment information.
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
*! Example   : =lfSegInfo()
*!*************************************************************
FUNCTION lfSegInfo
lnAlias=SELECT()
llStruOp=gfOpenFile(gcDataDir+'ICISTRU','Segno','SH')
IF !SEEK('U1','ICISTRU')
  IF USED('ICISTRU') AND llStruOp
    USE IN ICISTRU
  ENDIF
  RETURN .F.
ENDIF
DECLARE laMajSeg[1,1]
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

lnClrPos = int(ASCAN(laMajSeg,'C')/7+0.9)
lnStrtPos = laMajSeg[lnClrPos,4] 
lnFldLngth= LEN(laMajSeg[lnClrPos,3])

*--RE-Position style & color Fields 
lnXPos    = 5.13 + lnstylewid + 1.65
lnGetXPos = 4.13 + lnXPos  + 0.15

FOR lnCnt=1 TO ALEN(laMajSeg,1)
  DECLARE laSegInfo[lnCnt,9]
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

  laSegInfo[lnCnt,8]=IIF(laMajSeg[lnCnt,1]$'FOQT',STR(lnCnt,1),;
                     IIF(laMajSeg[lnCnt,1]='S','S','NN'))
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
RETURN
*B802594,1 WAB - END


*B802594,1 WAB - START
*!***********************************************************************************************
*! Func. Name: lfAddElmnt
*! Developer : WAB - Walid A. Wahab
*! Date      : 09/12/99
*! Purpose   : function to add elelemnt 'Select Position' to style position popup
*!***********************************************************************************************
*! Calls     : None
*!***********************************************************************************************
*! Parameters: NONE
*!***********************************************************************************************
*! Returns   : None.
*!***********************************************************************************************
*! Example   : =lfAddElmnt()
*!***********************************************************************************************
FUNCTION lfAddElmnt
DECLARE lastypos[ALEN(lastypos,1) + 1, 2]     && add on row to the array 
=AINS(lastypos, 1)

*-- First. elemnt 'Select position'
lastypos[1,1]= 'Select a Position'
lastypos[1,2]= ' '
REtURN
*B802594,1 WAB - END
 

*B802594,1 WAB - START
*!***********************************************************************************************
*! PROC. Name: lpSavScr
*! Developer : WAB - Walid A. Wahab
*! Date      : 09/13/99
*! Purpose   : Procedure to save/update to style position table
*!***********************************************************************************************
*! Calls     : None
*!***********************************************************************************************
*! Parameters: NONE
*!***********************************************************************************************
*! Returns   : None.
*!***********************************************************************************************
*! Example   : do lpSavScr
*!***********************************************************************************************
PROCEDURE lpSavScr
  PRIVATE lnAlias
  lnAlias=SELECT()
  SELECT icStyPos
  IF laScrMode[3]  AND SEEK(IIF(llallColr,lcMajor,laData[1]))	&& edit mode

    IF llallColr     		&& all colors
      *B603211,2 WAB - Commet this line and let the program add new record if 
      *B603211,2 WAB - there are new color added to the style
      *B603211,2 WAB - START
      *SCAN REST FOR cStyMajor = lcMajor AND cStylePos = laData[2]
      *  REPLACE m0nd_Dsgn WITH  laData[3] ,;
      *         m1nd_Dsgn WITH  laData[4] ,;
      *         m2nd_Dsgn WITH  laData[5]  
      *ENDSCAN
      *=SEEK(lcMajor)
      SELECT Style			
      SEEK lcMajor
      SCAN REST FOR cStyMajor = lcMajor
        laData[1] = Style
        laData[6] = lcMajor
        IF SEEK(ladata[1]+ladata[2],'icStyPos') 	&&exist record
           REPLACE icStyPos.m0nd_Dsgn WITH  laData[3] ,;
                   icStyPos.m1nd_Dsgn WITH  laData[4] ,;
                   icStyPos.m2nd_Dsgn WITH  laData[5]  
        ELSE	  && new color was added to the style after addinf the style pos.
          INSERT INTO icStyPos ;
                 (Style,cStylePos,m0nd_dsgn,m1nd_dsgn,m2nd_dsgn,cStyMajor) ;
                 VALUES(laData[1], laData[2], laData[3], laData[4], laData[5], laData[6])
        ENDIF    
      ENDSCAN
      SELECT icStyPos
      *B603211,2 WAB -END
    ELSE				&& specific calor
      REPLACE m0nd_Dsgn WITH  laData[3] ,;
              m1nd_Dsgn WITH  laData[4] ,;
              m2nd_Dsgn WITH  laData[5]  
    ENDIF
  ENDIF
  IF laScrMode[4] 				&& add mode
    IF llAllColr				&& all colors
      *-- case all colors ,select style table and insert into table ictypos records dependse 
      *-- on NO. colors of that style
      SELECT Style			
      SEEK lcMajor
      SCAN REST FOR cStyMajor = lcMajor
        laData[1] = Style
        laData[6] = lcMajor
        INSERT INTO icStyPos ;
               (Style,cStylePos,m0nd_dsgn,m1nd_dsgn,m2nd_dsgn,cStyMajor) ;
               VALUES(laData[1], laData[2], laData[3], laData[4], laData[5], laData[6])
      ENDSCAN
    ELSE						&& specified color	
        laData[6] = lcMajor
        INSERT INTO icStyPos ;
              (Style,cStylePos,m0nd_dsgn,m1nd_dsgn,m2nd_dsgn,cStyMajor) ;
              VALUES(laData[1], laData[2], laData[3], laData[4], laData[5], laData[6])
    ENDIF  
  ENDIF
  SELECT (lnAlias)
RETURN

*B802594,1 WAB - END
 

*B802594,1 WAB - START
*!***********************************************************************************************
*! PROC. Name: lpDelScr
*! Developer : WAB - Walid A. Wahab
*! Date      : 09/13/99
*! Purpose   : Procedure to DELETE from style position table
*!***********************************************************************************************
*! Calls     : None
*!***********************************************************************************************
*! Parameters: NONE
*!***********************************************************************************************
*! Returns   : None.
*!***********************************************************************************************
*! Example   : do lpDelScr
*!***********************************************************************************************
PROCEDURE lpDelScr
PRIVATE lnAlias
lnAlias=SELECT()
SELECT icStyPos
IF laScrMode[2]  AND SEEK(IIF(llallColr,lcMajor,laData[1]))
  IF llallColr					&& all colors 
    DELETE REST FOR cStyMajor = lcMajor AND cStylePos = laData[2]
  ELSE							&& specified color
    DELETE
  ENDIF
ENDIF
laScrMode    = .F.
laScrMode[1] = .T.
SHOW GETS
SELECT (lnAlias)
RETURN
*B802594,1 WAB - END
 

*B802594,1 WAB - START
*!***********************************************************************************************
*! PROC. Name: lpTopScr
*! Developer : WAB - Walid A. Wahab
*! Date      : 09/13/99
*! Purpose   : Top record in Navigation pb.
*!***********************************************************************************************
*! Calls     : None
*!***********************************************************************************************
*! Parameters: NONE
*!***********************************************************************************************
*! Returns   : None.
*!***********************************************************************************************
*! Example   : do lpTopScr
*!***********************************************************************************************
PROCEDURE lpTopScr
  llLastRec = .F.
  SELECT ICSTYPOS
  GO TOP
  lcMajor = cStyMajor
  IF llAllColr					&& all colors
    lcNonMjr = STRTRAN(lcNMjrPt,'X','*')
  ELSE							
    lcNonMjr = SUBSTR(Style,lnStyleWid+2,lnColorWid)
  ENDIF
  lcCodGet  = IIF(llMScale AND !llallcolors,SUBSTR(lcNonMjr,1,LEN(lcNonMjr)-3),lcNonMjr)
  lcNMjrDisc = gfCodDes(lcCodGet,'COLOR')		
  =lfGetData() 				&& get data from table to array ladata
RETURN

*B802594,1 WAB - END
 

*B802594,1 WAB - START
*!***********************************************************************************************
*! PROC. Name: lpBtmScr
*! Developer : WAB - Walid A. Wahab
*! Date      : 09/13/99
*! Purpose   : Bottom record in Navigation pb.
*!***********************************************************************************************
*! Calls     : None
*!***********************************************************************************************
*! Parameters: NONE
*!***********************************************************************************************
*! Returns   : None.
*!***********************************************************************************************
*! Example   : do lpBtmScr
*!***********************************************************************************************
PROCEDURE lpBtmScr
  llLastRec = .T. 
  SELECT ICSTYPOS
  IF llAllColr			&& all color
    SET ORDER TO StyMajor  
    =seek(PADR(laData[6],19)+laData[2])
  ENDIF
  GO BOTTOM
  lcMajor = cStyMajor
  IF llAllColr
    lcNonMjr = STRTRAN(lcNMjrPt,'X','*')
  ELSE
    lcNonMjr = SUBSTR(Style,lnStyleWid+2,lnColorWid)
  ENDIF  
  IF llAllColr			&& all color
    SET ORDER TO icStyPos  
  ENDIF  
  lcCodGet  = IIF(llMScale AND !llallcolors,SUBSTR(lcNonMjr,1,LEN(lcNonMjr)-3),lcNonMjr)
  lcNMjrDisc = gfCodDes(lcCodGet,'COLOR')
  =lfGetData() 
RETURN

*B802594,1 WAB - END
 

*B802594,1 WAB - START
*!***********************************************************************************************
*! PROC. Name: lpNxtScr
*! Developer : WAB - Walid A. Wahab
*! Date      : 09/13/99
*! Purpose   : Next record in Navigation pb.
*!***********************************************************************************************
*! Calls     : None
*!***********************************************************************************************
*! Parameters: NONE
*!***********************************************************************************************
*! Returns   : None.
*!***********************************************************************************************
*! Example   : do lpNxtScr
*!***********************************************************************************************
PROCEDURE lpNxtScr
  SELECT ICSTYPOS
  llLastRec = .F.
  IF llAllColr			&& all color
    SET ORDER TO StyMajor
    =seek(PADR(laData[6],19)+laData[2])
    SKIP 1
    IF EOF()
      SKIP -1
    ENDIF
    =lfNavgChk()
    SET ORDER TO icStyPos
    lcMajor = SUBSTR(Style,1,lnStyleWid)
    lcNonMjr = STRTRAN(lcNMjrPt,'X','*')
  ELSE				&& specified color
    SKIP 1
    IF EOF()
      SKIP -1
    ENDIF
    =lfNavgChk()
    lcMajor = SUBSTR(Style,1,lnStyleWid)
    lcNonMjr = SUBSTR(Style,lnStyleWid+2,lnColorWid)
  ENDIF  
  lcCodGet  = IIF(llMScale AND !llallcolors,SUBSTR(lcNonMjr,1,LEN(lcNonMjr)-3),lcNonMjr)
  lcNMjrDisc = gfCodDes(lcCodGet,'COLOR')
  =lfGetData() 
RETURN

*B802594,1 WAB - END
 

*B802594,1 WAB - START
*!***********************************************************************************************
*! PROC. Name: lpPrvScr
*! Developer : WAB - Walid A. Wahab
*! Date      : 09/13/99
*! Purpose   : Previus record in Navigation pb.
*!***********************************************************************************************
*! Calls     : None
*!***********************************************************************************************
*! Parameters: NONE
*!***********************************************************************************************
*! Returns   : None.
*!***********************************************************************************************
*! Example   : do lpPrvScr
*!***********************************************************************************************
PROCEDURE lpPrvScr
  SELECT ICSTYPOS
  llLastRec = .F.
  IF llAllColr					&& all colors
    SET ORDER TO StyMajor
    =seek(PADR(laData[6],19)+laData[2])
    SKIP -1 
    IF BOF()
      GO BOTTOM
    ENDIF
    lcMajor = SUBSTR(Style,1,lnStyleWid)
    lcNonMjr = STRTRAN(lcNMjrPt,'X','*')
    SET ORDER TO icStyPos
  ELSE
    IF BOF()
      GO BOTTOM
    ENDIF
    lcMajor = SUBSTR(Style,1,lnStyleWid)
    lcNonMjr = SUBSTR(Style,lnStyleWid+2,lnColorWid)
  ENDIF  
  lcCodGet  = IIF(llMScale AND !llallcolors,SUBSTR(lcNonMjr,1,LEN(lcNonMjr)-3),lcNonMjr)
  lcNMjrDisc = gfCodDes(lcCodGet,'COLOR')
  =lfGetData() 
RETURN

*B802594,1 WAB - END
 

*B802594,1 WAB - START
*!***********************************************************************************************
*! FUNC. Name: lfGetData
*! Developer : WAB - Walid A. Wahab
*! Date      : 09/13/99
*! Purpose   : get data from table to ladata array
*!***********************************************************************************************
*! Calls     : None
*!***********************************************************************************************
*! Parameters: NONE
*!***********************************************************************************************
*! Returns   : None.
*!***********************************************************************************************
*! Example   : =lfGetData()
*!***********************************************************************************************
FUNCTION lfGetData
  ladata[1]    = STYLE
  ladata[2]    = cStylePos
  laData[3]    = m0nd_Dsgn
  laData[4]    = m1nd_Dsgn
  laData[5]    = m2nd_Dsgn
  laData[6]    = cStyMajor
  SHOW GETS
RETURN
*B802594,1 WAB - END       
    

*B802594,1 WAB - START
*!***********************************************************************************************
*! FUNC. Name: lfNavgChk
*! Developer : WAB - Walid A. Wahab
*! Date      : 09/19/99
*! Purpose   : check the position of the current record
*!***********************************************************************************************
*! Calls     : None
*!***********************************************************************************************
*! Parameters: NONE
*!***********************************************************************************************
*! Returns   : None.
*!***********************************************************************************************
*! Example   : =lfNavgChk()
*!***********************************************************************************************
FUNCTION lfNavgChk
lnOldRec = RECNO()
SKIP 1
llEof = EOF()
SKIP -2
llBof = BOF()
IF lnOldRec <=RECCOUNT()
  GO lnOldRec
ENDIF
*B802594,1 WAB - END       
    

*B802594,1 WAB - START
*!***********************************************************************************************
*! FUNC. Name: lfShowNavg
*! Developer : WAB - Walid A. Wahab
*! Date      : 09/19/99
*! Purpose   : Set the objects enable/disable status for Navigation buttons
*!***********************************************************************************************
*! Calls     : None
*!***********************************************************************************************
*! Parameters: NONE
*!***********************************************************************************************
*! Returns   : None.
*!***********************************************************************************************
*! Example   : =lfNavgChk()
*!***********************************************************************************************
FUNCTION lfShowNavg
DO CASE
  *** Record is the only record in the file
  CASE llBof .AND. llEof  
    SHOW GET pbTop   DISABLE
    SHOW GET pbBtm   DISABLE
    SHOW GET pbNxt   DISABLE
    SHOW GET pbPrvs  DISABLE
  *** Top record   
  CASE llBof
    SHOW GET pbTop   DISABLE
    SHOW GET pbBtm   ENABLE
    SHOW GET pbNxt   ENABLE
    SHOW GET pbPrvs  DISABLE
  *** Last record  
  CASE llEof
    SHOW GET pbTop   ENABLE
    SHOW GET pbBtm   DISABLE
    SHOW GET pbNxt   DISABLE
    SHOW GET pbPrvs  ENABLE
  *** Any other record  
  OTHERWISE
    SHOW GET pbTop   ENABLE
    SHOW GET pbBtm   ENABLE
    SHOW GET pbNxt   ENABLE
    SHOW GET pbPrvs  ENABLE
 ENDCASE   
*B802594,1 WAB - END       
