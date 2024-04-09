*:************************************************************************
*:
*: Procedure file: GLACADD.PRG
*:
*:         System: ARIA Ver. 4.0
*:         Module: General Ledger
*:         Author: Reham Aly Alallamy
*:         Date  : 12/05/2002
*:************************************************************************
PRIVATE lncounter , lcCounter

*-- Put all the account segments in an array to handle each segment seperately ..
DECLARE laAccSeg [1]
=gfSubStr(ALLTRIM(lcAccCode),@laAccSeg,"-")

STORE ""  TO lcFilTyp  , lcTmpTyp
llReturn = .F.
lnCount  = 1
lcCount  = STR(lnCount,1)

*-- If not branching from the segment browse
IF llAddSeg
  *-- Validate the first segment
  IF !USED("GLSEGVAL")
    =gfOpenFile(oAriaApplication.DataDir+'GLSEGVAL',oAriaApplication.DataDir+'Acssegval','SH') 
  ELSE
    SELECT GLSEGVAL
  ENDIF
  IF !SEEK(STR(lnCount,1)+laAccSeg[lnCount],"GLSEGVAL")
    *-- If it is not exist in the segment value Check in the types & ranges if this segment is between any ranges ...
    SELECT GLTYPES
    GO TOP
    LOCATE FOR BETWEEN(VAL(laAccSeg[1]),VAL(cTyplacno),VAL(cTypuacno))
    IF !FOUND()
      *** This account major does not belong to any ranges ***
      =gfModalGen("INM02024B00000","DIALOG",laAccSeg[1])
      llCallRet  = .F.
    ELSE
      IF lfChckTyp(GLTYPES.CSTANDARD)
        *** This account major is not exist in the segment file ***
        *** <Define segment> - <Cancel> ***
        IF  gfModalGen("QRM02028B02006","DIALOG",laAccSeg[1]) = 1
          *-- Check if the segment value screen is running
          llSegVlRun = .F.
          lnCount    = 2
          DO WHILE TYPE("_Screen.Forms(lnCount)") = "O" AND TYPE("_Screen.Forms(lnCount).Parent") = "O"
            IF UPPER(_Screen.Forms(lnCount).Parent.Name) = "AAWRGLSGVLU"
              llSegVlRun = .T.
              EXIT
            ENDIF
            lnCount = lnCount + 1
            LOOP
          ENDDO
          
          *-- If the segment value screen is running, inform the user to close it first
          IF llSegVlRun
            =gfModalgen('INM00009B00000','A')
            llCallRet  = .F.
          ELSE
            *-- Call the segment values form
            oAriaApplication.DoProgram("AWRGLSGVLU","'"+&lcCount+"','"+&laAccSeg[lnCount]+"'",.F.,"GL")
          ENDIF
        ELSE
          llCallRet  = .F.
        ENDIF
      ELSE
        llCallRet  = .F.
      ENDIF
    ENDIF
  ELSE
    llCallRet = lfChckTyp(GLSEGVAL.CSTANDARD)
  ENDIF

  IF !llCallRet
    RETURN 
  ENDIF
  FOR lnCounter = 2 TO ALEN(laAccSeg,1)
    IF !SEEK(STR(lnCounter,1)+laAccSeg[lnCounter],"GLSEGVAL")
      *** This account major is not exist in the segment file ***
      *** <Define segment> - <Cancel> ***
      IF gfModalGen("QRM02029B02006","DIALOG",ALLTRIM(STR(lnCounter))+"|"+laAccSeg[lnCounter]) = 1
        *-- Check if the segment value screen is running
        llSegVlRun = .F.
        lnCount    = 2
        DO WHILE TYPE("_Screen.Forms(lnCount)") = "O" AND TYPE("_Screen.Forms(lnCount).Parent") = "O"
          IF UPPER(_Screen.Forms(lnCount).Parent.Name) = "AAWRGLSGVLU"
            llSegVlRun = .T.
            EXIT
          ENDIF
          lnCount = lnCount + 1
          LOOP
        ENDDO
        
        *-- If the segment value screen is running, inform the user to close it first
        IF llSegVlRun
          =gfModalgen('INM00009B00000','A')         && Close window first
          llCallRet  = .F.
        ELSE
          *-- Call the segment values form
          oAriaApplication.DoProgram("AWRGLSGVLU","'"+&lcCount+"','"+&laAccSeg[lnCount]+"'",.F.,"GL")
        ENDIF  
      ELSE
        llCallRet  = .F.
        EXIT
      ENDIF
    ENDIF
  ENDFOR
  IF !llCallRet 
    RETURN
  ENDIF
ENDIF

*-- Add the combination of the whole segments in the chart of 
*-- account file by branching to the single account screen.
IF !llSngAcn
  *-- Check if the single account screen is running
  llSngAcRun = .F.
  lnCount    = 2
  DO WHILE TYPE("_Screen.Forms(lnCount)") = "O" AND TYPE("_Screen.Forms(lnCount).Parent") = "O"
    IF UPPER(_Screen.Forms(lnCount).Parent.Name) = "AAWRGLSNGAC"
      llSngAcRun = .T.
      EXIT
    ENDIF
    lnCount = lnCount + 1
    LOOP
  ENDDO
  
  *-- If the single account screen is running, inform the user to close it first
  IF llSngAcRun
    =gfModalgen('INM00009B00000','A')         && Close window first
    llCallRet  = .F.
  ELSE
    *-- Call the single account form
    oAriaApplication.DoProgram("AWRGLSNGAC","'"+lcAccCode+"'",.F.,"GL")
  ENDIF
ENDIF
IF llCallRet
  lcAccDes1 = ""
  FOR lnCount = 1 TO ALEN(laAccSeg,1)
    IF SEEK(STR(lnCount,1)+laAccSeg[lnCount],"GLSEGVAL")
      lcAccDes1 = lcAccDes1 + ;
                  IIF(EMPTY(lcAccDes1) .OR. RIGHT(lcAccDes1,1)='-','','-');
                  + ALLTRIM(GLSEGVAL.cSegshdes)
      IF lnCount = 1
        lcTypCode1 = GLSEGVAL.ctypecode
        lcTypDesc1 = LOOKUP(GLTYPES.cTypedesc,lcTypCode1,GLTYPES.cTypecode,'Typecode')
      ENDIF
    ENDIF
  ENDFOR
ENDIF
RETURN

*!**************************************************************************
*!
*!      Function: lfChckTyp
*!
*!**************************************************************************
*
FUNCTION lfChckTyp
PARAMETERS lcTyp2Chck

*-- If this account major between any ranges Check if the account type code 
*-- for this account major is allowed in the calling program or not ...

IF lcAccType1 <> "A"
  lcTmpTyp = IIF(lcAccType1 = 'T','Y','N')
  *-- If the type for this account major is not allewed to add in the calling program ..
  IF lcTyp2Chck <> lcTmpTyp
    *-- This account major has statistical(standard).. type Only standard(statistical) types is allowed ...
    IF lcTmpTyp = 'Y'
      =gfModalGen("INM02031B00000","DIALOG",laAccSeg [1])
    ELSE
      =gfModalGen("INM02030B00000","DIALOG",laAccSeg [1])
    ENDIF
    RETURN .F.
  ENDIF
ENDIF

