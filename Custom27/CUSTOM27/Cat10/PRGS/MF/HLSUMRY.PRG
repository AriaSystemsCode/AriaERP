*:****************************************************************
*: Program file  : HLSUMRY.PRG
*: Program desc. : Summary T.S.U.S.A. Guide.
*: System        : Aria Apparel System (A27).
*: Module        : H.L.C. System. (HL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200246,1
*: Date          : 12/10/2001
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfvChfwt , lpShow , lfvConst
*:               :            : lfvGrmnt , gfFieldPic.
*:               -----------------------------------------------
*:               : PROCEDURE  : None.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200246,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- Begin Declaration variables.
*-- laChfWtCod :- Chief Weight Code.
*-- laConstCod :- Construction Code.
*-- laGrmntCod :- Garment Description Code.
*-- End Declaration variables.


EXTERNAL ARRAY laData,laKeyField,laScrMode
DECLARE laKeyField [3,4],laData[1]

*-- Array to hold the description & codes for.
DECLARE laCodes[3,10] , laChfWtCod[1,2] , laConstCod[1,2] ,;
                        laGrmntCod[1,2]

STORE '' TO laCodes , laChfWtCod , laConstCod , laGrmntCod ,;
            lcScFields , lcModal
STORE 1  TO lnChfWtCod , lnConstCod , lnGrmntCod

IF !gfSetup()
  RETURN
ENDIF  

laKeyField[1,1] = 'lnChfWtCod'
laKeyField[1,2] =.F.
laKeyField[1,3] = 'CHNCONGAR'
laKeyField[1,4] = 1
laKeyField[2,1] = 'lnConstCod'
laKeyField[2,2] =.F.
laKeyField[2,3] = 'CHNCONGAR'
laKeyField[2,4] = 2
laKeyField[3,1] = 'lnGrmntCod'
laKeyField[3,2] =.T.
laKeyField[3,3] = 'CHNCONGAR'
laKeyField[3,4] = 3


IF !WEXIST(gcBaseWind)
  *-- Codes For Chief Weight Code , Construction Code & Garment Description Code.
  laCodes[1,1]  = 'CTSUCHWCD'
  laCodes[1,2]  = 'laChfWtCod'
  laCodes[1,3]  = 'lnChfWtCod'
  laCodes[1,4]  = ''
  laCodes[1,5]  = .F.
  laCodes[1,6]  = .F.
  laCodes[1,10] = 'CTSUCHWCD'
 
  laCodes[2,1]  = 'CTSUCONCD'
  laCodes[2,2]  = 'laConstCod'
  laCodes[2,3]  = 'lnConstCod'
  laCodes[2,4]  = ''
  laCodes[2,5]  = .F.
  laCodes[2,6]  = .F.
  laCodes[2,10] = 'CTSUCONCD'

  laCodes[3,1]  = 'CTSUGARCD'
  laCodes[3,2]  = 'laGrmntCod'
  laCodes[3,3]  = 'lnGrmntCod'
  laCodes[3,4]  = ''
  laCodes[3,5]  = .F.
  laCodes[3,6]  = .F.
  laCodes[3,10] = 'CTSUGARCD'


  lcScFields = 'CTSUCHWCD  , CTSUCONCD   , CTSUGARCD  , NTSUDTRAT ,'+;
               'CTSUQTCAT  , CTSUSUFIX   , NTSUFRTNM  ,NTSUFRTNL '  
  SCATTER FIELDS &lcScFields TO laData BLANK
ENDIF

PUSH KEY
DO (gcScrDir+gcAct_Appl+"\HlSumry.SPX")
POP KEY

RETURN
*-- End of program code.
*:*************************************************************
*: Name      : lpShow
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/10/2001
*: Purpose   : Show function
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : lcVarNam = ladata [13]
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : Do lpShow
*:*************************************************************
*:
PROCEDURE lpShow

EXTERNAL ARRAY laScrMode,laGrmntDis

DO CASE
  ***--- S E L E C T   M O D E ---***
  CASE laScrMode[1]
    STORE 1  TO lnChfWtCod , lnConstCod , lnGrmntCod

    lnChfwtCod = ASCAN(laChfwtCod,laData[1])/2
    lnConstCod = ASCAN(laConstCod,laData[2])/2
    lnGrmntCod = ASCAN(laGrmntCod,laData[3])/2
    
    SHOW GET lnChfWtCod ENABLE
    SHOW GET lnConstCod ENABLE
    SHOW GET lnGrmntCod ENABLE
    _CUROBJ = OBJNUM(pbCls)
    
  ***--- V I E W   M O D E ---***
  CASE laScrMode[2]
 
    *-- Chief Weight Code.
    IF !EMPTY(laData[1])
      =gfwCodePop(@laCodes,'CTSUCHWCD','L')
    ENDIF  
  
    *-- Construction Code.
    IF !EMPTY(laData[2])  
      =gfwCodePop(@laCodes,'CTSUCONCD','L')
    ENDIF
    
    *-- Garment Description Code.
    IF !EMPTY(laData[3])
      =gfwCodePop(@laCodes,'CTSUGARCD','L')
    ENDIF

    lnChfwtCod = ASCAN(laChfwtCod,laData[1])/2
    lnConstCod = ASCAN(laConstCod,laData[2])/2
    lnGrmntCod = ASCAN(laGrmntCod,laData[3])/2

    SHOW GET lnChfWtCod DISABLE
    SHOW GET lnConstCod DISABLE
    SHOW GET lnGrmntCod DISABLE


  ***--- E D I T   M O D E ---***
  ***--- A D D   M O D E ---***
  CASE laScrMode[4] .OR. laScrMode[3] && ADD OR EDIT Mode

    SHOW GET lnChfWtCod DISABLE
    SHOW GET lnConstCod DISABLE
    SHOW GET lnGrmntCod DISABLE
    SHOW GET laData[4] ENABLE
    SHOW GET laData[5] ENABLE
    SHOW GET laData[6] ENABLE
    SHOW GET laData[7] ENABLE
    SHOW GET laData[8] ENABLE

ENDCASE    

*-- End Of lpShow.
*:*************************************************************
*: Name      : lfvChfwt
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/10/2001
*: Purpose   : Valid the Chief Weight Code.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvChfwt()
*:*************************************************************
*:

FUNCTION lfvChfwt

IF lnChfWtCod  <> 0 .AND.!( PADR(ALLTRIM(laData[1]),6) == PADR(ALLTRIM(laChfWtCod[lnChfWtCod,2]),6))
  laData[1] = PADR(ALLTRIM(laChfWtCod[lnChfWtCod,2]),6)
ENDIF

*-- End Of lfvChfwt.
*:*************************************************************
*: Name      : lfvConst
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/10/2001
*: Purpose   : Valid the Construction Code.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvConst()
*:*************************************************************
*:
FUNCTION lfvConst

IF lnConstCod <> 0 .AND. !( PADR(ALLTRIM(laData[2]),6) == PADR(ALLTRIM(laConstCod[lnConstCod,2]),6))
  laData[2] = PADR(ALLTRIM(laConstCod[lnConstCod,2]),6)
ENDIF

*-- End Of lfvConst.
*:*************************************************************
*: Name      : lfvGrmnt
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/10/2001
*: Purpose   : Valid the Garment Description Code.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvGrmnt()
*:*************************************************************
*:
FUNCTION lfvGrmnt
PRIVATE lcMessage

lcMessage = ''

IF lnGrmntCod = 0
  RETURN
ENDIF
IF !( PADR(ALLTRIM(laData[3]),6) == PADR(ALLTRIM(laGrmntCod[lnGrmntCod,2]),6))
  laData[3] = PADR(ALLTRIM(laGrmntCod[lnGrmntCod,2]),6)
ENDIF

IF !EMPTY(laData[1]) .AND. !EMPTY(laData[2]) .AND. !EMPTY(laData[3])
  SELECT HLCTSUSA
  = SEEK (laData[1]+laData[2]+laData[3])

  IF FOUND()
    SCATTER FIELDS &lcScFields MEMO TO laData
    laScrMode=.F.
    laScrMode[2]=.T.
    SHOW GETS 
  ELSE
    *- Text Message   :- XXXXXXX is not found in the data file.
    *- Text Number    :- 00001
    *- button message :- \<Add;\<Browse;\?\<Reenter
    *- button Number  :- 42003.

    *-- Codes not found in the Code File .
    lcMessage = ALLTRIM(laChfwtCod((ASCAN(laChfwtCod,laData[1])/2),1)) + ' ' +;
                ALLTRIM(laConstCod((ASCAN(laConstCod,laData[2])/2),1)) + ' ' +;
                ALLTRIM(laGrmntCod((ASCAN(laGrmntCod,laData[3])/2),1))
    
    lnOption = gfModalGen('INM00001B42003','DIALOG',lcMessage )
    DO CASE

      CASE lnOption = 1
        laScrMode = .F.
        laScrMode[4] = .T.
        SHOW GETS
      CASE lnOption = 2
        = gfBrows()
           
      CASE lnOption = 3
        STORE SPACE(6) TO laData[1] , laData[2] , laData[3]
        laScrMode = .F.
        laScrMode[1] = .T.
        SHOW GETS

    ENDCASE  
  ENDIF  
ENDIF

*-- End Of lfvGrmnt.
*:*************************************************************
*: Name      : gfFieldPic
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/10/2001
*: Purpose   : Picture Field.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = gfFieldPic()
*:*************************************************************
*:
FUNCTION gfFieldPic

PARAMETERS lcFld_Name

PRIVATE lcFld_Name

RETURN REPLICATE("X",FSIZE(lcFld_Name))

*-- End Of gfFieldPic.
*:*************************************************************