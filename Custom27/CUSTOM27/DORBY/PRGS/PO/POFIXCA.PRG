*:--------------------------------------------------------------------------
*: Program file        : POfixca.PRG
*: Program description : Fix Categoty type.
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : (PO)
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : B#603636
*:--------------------------------------------------------------------------
*: Calls               : None.
*:--------------------------------------------------------------------------
*: Called From         : (PO)
*:--------------------------------------------------------------------------
*: Example             : DO POFIXCA.PRG
*:--------------------------------------------------------------------------
*: Modifications       :
*:--------------------------------------------------------------------------

WAIT WINDOW "Start Fixing Cost type categories " NOWAIT
IF !lfvProcced()
  RETURN
ENDIF
=lfvCancel()
WAIT WINDOW "Fixing Cost type categories completed successfully. Press any key to continue."
WAIT CLEAR
*:----------------------------------------------------------------
*: Name       : lfvProcced
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 18/04/2000
*: Purpose    : Function to start fix FIles.
*:----------------------------------------------------------------
*: Parameters : None
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvProcced()
*:----------------------------------------------------------------
*:
FUNCTION lfvProcced

DECLARE laSetupsI[5,2]
DECLARE laSetupsM[5,2]
DECLARE laTypeI[5]
DECLARE laTypeM[5]
laSetupsM[1,1]  = 'M_C'+'MTYPE1'
laSetupsM[2,1]  = 'M_C'+'MTYPE2'
laSetupsM[3,1]  = 'M_C'+'MTYPE3'
laSetupsM[4,1]  = 'M_C'+'MTYPE4'
laSetupsM[5,1]  = 'M_C'+'MTYPE5'

laSetupsI[1,1]  = 'M_C'+'ITYPE1'
laSetupsI[2,1]  = 'M_C'+'ITYPE2'
laSetupsI[3,1]  = 'M_C'+'ITYPE3'
laSetupsI[4,1]  = 'M_C'+'ITYPE4'
laSetupsI[5,1]  = 'M_C'+'ITYPE5'

=gfGetMemVar(@laSetupsM,gcAct_Comp)
=gfGetMemVar(@laSetupsI,gcAct_Comp)
= gfOpenFile(gcDataDir+"Style","Style",'SH')
=lfPrePare()
WAIT WINDOW "Start Fixing BomLine " NOWAIT
= gfOpenFile(gcDataDir+"BOMLINE","",'SH')
=lfFxBomLin()

*--- Check BOM File
WAIT WINDOW "Start Fixing Bom " NOWAIT
= gfOpenFile(gcDataDir+"BOM","",'SH')
=lfFxBom()

*--- Check Existing of Link File
WAIT WINDOW "Start Fixing BomCost " NOWAIT
= gfOpenFile(gcDataDir+"BOMCOST","",'SH')
=lfFxBomCst()

*--- Check CTKTBOM File
WAIT WINDOW "Start Fixing cTktBom " NOWAIT
= gfOpenFile(gcDataDir+"CTKTBOM","",'SH')
=lfFxCTkt()
*--- Check APINVDT File
WAIT WINDOW "Start Fixing APVINVDT " NOWAIT
= gfOpenFile(gcDataDir+"APVINVDT","",'SH')
=lfFxApInv()


*:----------------------------------------------------------------
*: Name       : lfvCancel
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 18/04/2000
*: Purpose    : Valid function for Cancel button
*:----------------------------------------------------------------
*: Parameters : None
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvCancel()
*:----------------------------------------------------------------
*:
FUNCTION lfvCancel

USE IN BOMLINE
USE IN BOM
USE IN CTKTBOM
USE IN APVINVDT
USE IN BOMCOST

*:----------------------------------------------------------------
*: Name       : lfPrePare
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 18/04/2000
*: Purpose    : Prepare category array
*:----------------------------------------------------------------
*: Parameters : None
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfPrePare()
*:----------------------------------------------------------------
*:
FUNCTION lfPrePare

FOR lnInd=1 TO 5
  laTypeM[lnInd] = ALLTRIM(laSetupsM[lnInd,2])+STR(lnInd,1)
  laTypeI[lnInd] = ALLTRIM(laSetupsI[lnInd,2])+STR(lnInd,1)
ENDFOR


*:----------------------------------------------------------------
*: Name       : lfFxBomLin
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 18/04/2000
*: Purpose    : Fix BomLine
*:----------------------------------------------------------------
*: Parameters : None
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfFxBomLin()
*:----------------------------------------------------------------
*:
FUNCTION lfFxBomLin

SELECT BOMLINE
GOTO TOP
SCAN
  IF cImTyp='I'  &&--- Imported
    IF !EMPTY(cCatgTyp)
     WAIT WINDOW "Fix  Category types for PO# "+cTktNo NOWAIT
      DO CASE
        CASE  laTypeI[1] = cCatgTyp
          REPLACE cBomTyp WITH '1'
        CASE  laTypeI[2] = cCatgTyp
          REPLACE cBomTyp WITH '2'
        CASE  laTypeI[3] = cCatgTyp
          REPLACE cBomTyp WITH '3'
        CASE  laTypeI[4] = cCatgTyp
          REPLACE cBomTyp WITH '4'
      CASE  laTypeI[5] = cCatgTyp
        REPLACE cBomTyp WITH '5'
      ENDCASE
    ENDIF
  ELSE
    IF !EMPTY(cCatgTyp)
      WAIT WINDOW "Fix  Category types for C/T# "+cTktNo NOWAIT
      DO CASE
        CASE  laTypeM[1] = cCatgTyp
          REPLACE cBomTyp WITH '1'
        CASE  laTypeM[2] = cCatgTyp
          REPLACE cBomTyp WITH '2'
        CASE  laTypeM[3] = cCatgTyp
          REPLACE cBomTyp WITH '3'
        CASE  laTypeM[4] = cCatgTyp
          REPLACE cBomTyp WITH '4'
        CASE  laTypeM[5] = cCatgTyp
          REPLACE cBomTyp WITH '5'
        ENDCASE
    ENDIF
  ENDIF
ENDSCAN

*:----------------------------------------------------------------
*: Name       : lfFxBom
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 18/04/2000
*: Purpose    : Fix Bom
*:----------------------------------------------------------------
*: Parameters : None
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfFxBom()
*:----------------------------------------------------------------
*:
FUNCTION lfFxBom
SELECT BOM
GOTO TOP
SCAN
  IF SEEK(ALLTRIM(cItmMajor),'Style')
    WAIT WINDOW "Fix  Category types Style "+cItmMajor NOWAIT
    IF !Style.Make   &&--- Imported
      IF !EMPTY(cCatgTyp)
        =lfImpCas()
      ENDIF
    ELSE
      IF !EMPTY(cCatgTyp)
        =lfManCas()
      ENDIF
    ENDIF
  ENDIF
ENDSCAN

*:----------------------------------------------------------------
*: Name       : lfFxBomCst
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 18/04/2000
*: Purpose    : Fix BomCost
*:----------------------------------------------------------------
*: Parameters : None
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfFxBomCst()
*:----------------------------------------------------------------
*:
FUNCTION lfFxBomCst

SELECT BOMCOST
SCAN
  IF cImTyp='I'  &&--- Imported
    WAIT WINDOW "Fix  Category types PO# "+cTktNo NOWAIT
    DO CASE
      CASE  laTypeI[1] = cCostType
        REPLACE cBomType WITH '1'
      CASE  laTypeI[2] = cCostType
        REPLACE cBomType WITH '2'
      CASE  laTypeI[3] = cCostType
        REPLACE cBomType WITH '3'
      CASE  laTypeI[4] = cCostType
        REPLACE cBomType WITH '4'
      CASE  laTypeI[5] = cCostType
        REPLACE cBomType WITH '5'
    ENDCASE
  ELSE
    WAIT WINDOW "Fix  Category types C/T# "+cTktNo NOWAIT
    DO CASE
      CASE  laTypeM[1] = cCostType
        REPLACE cBomType WITH '1'
      CASE  laTypeM[2] = cCostType
        REPLACE cBomType WITH '2'
      CASE  laTypeM[3] = cCostType
        REPLACE cBomType WITH '3'
      CASE  laTypeM[4] = cCostType
        REPLACE cBomType WITH '4'
      CASE  laTypeM[5] = cCostType
        REPLACE cBomType WITH '5'
    ENDCASE
  ENDIF
ENDSCAN


*:----------------------------------------------------------------
*: Name       : lfImpCas
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 18/04/2000
*: Purpose    : Function Imported case.
*:----------------------------------------------------------------
*: Parameters : None
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfImpCas()
*:----------------------------------------------------------------
*:
FUNCTION lfImpCas

DO CASE
  CASE  laTypeI[1] = cCatgTyp
    REPLACE Typ WITH '1'
  CASE  laTypeI[2] = cCatgTyp
    REPLACE Typ WITH '2'
  CASE  laTypeI[3] = cCatgTyp
    REPLACE Typ WITH '3'
  CASE  laTypeI[4] = cCatgTyp
    REPLACE Typ WITH '4'
  CASE  laTypeI[5] = cCatgTyp
    REPLACE Typ WITH '5'
ENDCASE

*:----------------------------------------------------------------
*: Name       : lfManCas
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 18/04/2000
*: Purpose    : Function Manuf. case.
*:----------------------------------------------------------------
*: Parameters : None
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfManCas()
*:----------------------------------------------------------------
*:
FUNCTION lfManCas

DO CASE
  CASE  laTypeM[1] = cCatgTyp
    REPLACE Typ WITH '1'
  CASE  laTypeM[2] = cCatgTyp
    REPLACE Typ WITH '2'
  CASE  laTypeM[3] = cCatgTyp
    REPLACE Typ WITH '3'
  CASE  laTypeM[4] = cCatgTyp
    REPLACE Typ WITH '4'
  CASE  laTypeM[5] = cCatgTyp
    REPLACE Typ WITH '5'
ENDCASE


*:----------------------------------------------------------------
*: Name       : lfFxBomLin
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 18/04/2000
*: Purpose    : Fix BomLine
*:----------------------------------------------------------------
*: Parameters : None
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfFxBomLin()
*:----------------------------------------------------------------
*:
FUNCTION lfFxCTkt

SELECT cTktBom
GOTO TOP
SCAN
  IF cImTyp='I'  &&--- Imported
    IF !EMPTY(cCatgTyp)
      WAIT WINDOW "Fix  Category types For PO# "+CutTkt NOWAIT
      DO CASE
        CASE  laTypeI[1] = cCatgTyp
          REPLACE Typ WITH '1'
        CASE  laTypeI[2] = cCatgTyp
          REPLACE Typ WITH '2'
        CASE  laTypeI[3] = cCatgTyp
          REPLACE Typ WITH '3'
        CASE  laTypeI[4] = cCatgTyp
          REPLACE Typ WITH '4'
      CASE  laTypeI[5] = cCatgTyp
        REPLACE Typ WITH '5'
      ENDCASE
    ENDIF
  ELSE
    IF !EMPTY(cCatgTyp)
      WAIT WINDOW "Fix  Category types For C/T# "+CutTkt NOWAIT
      DO CASE
        CASE  laTypeM[1] = cCatgTyp
          REPLACE Typ WITH '1'
        CASE  laTypeM[2] = cCatgTyp
          REPLACE Typ WITH '2'
        CASE  laTypeM[3] = cCatgTyp
          REPLACE Typ WITH '3'
        CASE  laTypeM[4] = cCatgTyp
          REPLACE Typ WITH '4'
        CASE  laTypeM[5] = cCatgTyp
          REPLACE Typ WITH '5'
        ENDCASE
    ENDIF
  ENDIF
ENDSCAN


*:----------------------------------------------------------------
*: Name       : lfFxApInv
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 18/04/2000
*: Purpose    : Fix ApInv FIle.
*:----------------------------------------------------------------
*: Parameters : None
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfFxApInv()
*:----------------------------------------------------------------
*:
FUNCTION lfFxApInv

SELECT APVINVDT
GOTO TOP
SCAN
  IF SEEK(ALLTRIM(Item),'Style')
    IF !Style.Make   &&--- Imported
      IF !EMPTY(cCostType)
        DO CASE
          CASE  laTypeI[1] = cCostType
            REPLACE cBomType WITH '1'
          CASE  laTypeI[2] = cCostType
            REPLACE cBomType WITH '2'
          CASE  laTypeI[3] = cCostType
            REPLACE cBomType WITH '3'
          CASE  laTypeI[4] = cCostType
            REPLACE cBomType WITH '4'
          CASE  laTypeI[5] = cCostType
            REPLACE cBomType WITH '5'
        ENDCASE
      ENDIF
    ELSE
      IF !EMPTY(cCostType)
        DO CASE
          CASE  laTypeM[1] = cCostType
            REPLACE cBomType WITH '1'
          CASE  laTypeM[2] = cCostType
            REPLACE cBomType WITH '2'
          CASE  laTypeM[3] = cCostType
            REPLACE cBomType WITH '3'
          CASE  laTypeM[4] = cCostType
            REPLACE cBomType WITH '4'
          CASE  laTypeM[5] = cCostType
            REPLACE cBomType WITH '5'
        ENDCASE
      ENDIF
    ENDIF
  ENDIF
ENDSCAN