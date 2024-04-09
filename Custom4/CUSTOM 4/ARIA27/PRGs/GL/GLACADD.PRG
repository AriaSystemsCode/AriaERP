*:************************************************************************
*:
*: Procedure file: GLACADD.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Reham Aly Alallamy
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: lfChckTyp
*:               
*:      Documented  /  /   at   :  
*:************************************************************************
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*E301077,72 Open and close files.
*B602221,1 AAN 14/09/2000 Fix bug that not enter the desc. of the third segment

*** Put all the account segments in an array to ***
*** handle each segment seperately ..
DECLARE laAccSeg [1]

**B602221,1 AAN Definbe a varibale [Start]
PRIVATE lncounter , lcCounter  
**B602221,1 AAN Definbe a varibale [End]

=gfSubStr(ALLTRIM(lcAccCode),@laAccSeg,"-")

STORE ""  TO lcFilTyp  , lcTmpTyp

llReturn = .F.

lnCount = 1
lcCount = STR(lnCount,1)

IF llAddSeg          && If not branching from the segment browse

  *** Validate the first segment ***

  *** If it is exist in the segment value file ***
  
  *E301077,72 Open segment value file with proper index [Begin
  =gfOpenFile(gcDataDir+'GLSEGVAL',gcDataDir+'Acssegval','SH') 
  *E301077,72 Open segment value file with proper index [End

  IF !SEEK(STR(lnCount,1)+laAccSeg[lnCount],"GLSEGVAL")

    *** If it is not exist in the segment value     ***
    *** Check in the types & ranges if this segment ***
    *** is between any ranges ...
    SELECT GLTYPES
    GO TOP
    LOCATE  FOR BETWEEN(VAL(laAccSeg[1]),VAL(cTyplacno),VAL(cTypuacno))

    IF !FOUND()

      *** If this account major does not exist between any ranges ***
      *** This account major does not belong to any ranges ***
      =gfModalGen("INM02024B00000","DIALOG",laAccSeg[1])
      llCallRet  = .F.
    ELSE
      IF lfChckTyp(GLTYPES.CSTANDARD)

        *** This account major is not exist in the segment file ***
        *** <Define segment> - <Cancel> ***
        IF  gfModalGen("QRM02028B02006","DIALOG",laAccSeg[1]) = 1
          IF WEXIST('AWRGLSGVLU')
            =gfModalgen('INM00009B00000','A')         && Close window first
            llCallRet  = .F.
          ELSE

            *E301077,72 Call gpDoProg to run the specified screen[Begin]
            *E301077,72 like run it from menu, to take system
            *E301077,72 handle files open and close .
            lcCurrWind = gcBaseWind
            =gfStatic()
            lcWindow   = UPPER(lcBaseWind)
            llSave     = .F.
            glFirsTime = .T.

            gcBaseWind ='AWRGLSGVLU'

            lcAppName  = gcAppHome+'GL.APP'
            *E300683,5 Change call to pass parameter
            *DO (lcAppName) WITH 'Glsgvlu WITH "&lcCount","&laAccSeg[lnCount]"'    
			
			DO (lcAppName) WITH 'Glsgvlu','"&lcCount","&laAccSeg[lnCount]"'    
			*E300683,5 end
            lcSavExact = SET('EXACT')
            SET EXACT ON

            *E301077,72 IF SEEK ('WIN'+lcWindow+UPPER(gcUser_Id)+gcStation,'syustatc')   
            *** Restore data for this window
            *E301077,72 RESTORE FROM MEMO syustatc.mObj_Data  ADDITIVE
            *E301077,72 ENDIF  
            
            *E301077,72 SET EXACT &lcSavExact
            *E301077,72 glQuitting = .F.
            *E301077,72 llCallRet  = llSave
            
            gcBaseWind = lcCurrWind
            =gfSetup()
            
            *lcParameter = "'"+lcCount+"','"+laAccSeg[lnCount]+"'"
            *DO gpDoProg WITH 'AWRGLSGVLU',.F.,'GL',lcParameter
            *E301077,72 Call gpDoProg to run the specified screen[End  ]
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

  *** Check if the rest segments exist in the segments file or not ***
  *** to add the segments that not exist in the file ...
  *B602221,1 AAN Fix bug that not enter the desc. of the third segment [BEGIN]
  *FOR lnCount = 2 TO ALEN(laAccSeg,1)
  *IF !SEEK(STR(lnCount,1)+laAccSeg[lnCount],"GLSEGVAL")
  FOR lnCounter = 2 TO ALEN(laAccSeg,1)
    IF !SEEK(STR(lnCounter,1)+laAccSeg[lnCounter],"GLSEGVAL")
  *B602221,1 AAN Fix bug that not enter the desc. of the third segment [End]

      *** This account major is not exist in the segment file ***
      *** <Define segment> - <Cancel> ***
      
      *B602221,1 AAN Fix bug that not enter the desc. of the third segment [BEGIN]
      *IF gfModalGen("QRM02029B02006","DIALOG",ALLTRIM(STR(lnCount))+"|"+laAccSeg[lnCount]) = 1
      IF gfModalGen("QRM02029B02006","DIALOG",ALLTRIM(STR(lnCounter))+"|"+laAccSeg[lnCounter]) = 1
      *B602221,1 AAN Fix bug that not enter the desc. of the third segment [End]
      
        IF WEXIST('AWRGLSGVLU')
          =gfModalgen('INM00009B00000','A')         && Close window first
          llCallRet  = .F.
        ELSE

          *E301077,72 Call gpDoProg to run the specified screen[Begin]
          *E301077,72 like run it from menu, to take system
          *E301077,72 handle files open and close .
          *E RELEASE llSave
           lcCurrWind = gcBaseWind
           =gfStatic()
           lcWindow   = UPPER(lcBaseWind)
           llSave     = .F.
           glFirsTime = .T.

           gcBaseWind = 'AWRGLSGVLU'
           *B602221,1 AAN Fix bug that not enter the desc. of the third segment [BEGIN]           
           *lcCount    = STR(lnCount,1)
           lcCounter    = STR(lnCounter,1)
           *B602221,1 AAN Fix bug that not enter the desc. of the third segment [End]

           lcAppName  = gcAppHome+'GL.APP'
          *E300683,5 Change call to pass parameter
          *DO (lcAppName) WITH 'Glsgvlu WITH "&lcCount","&laAccSeg[lnCount]"'
          *B602221,1 AAN Fix bug that not enter the desc. of the third segment [BEGIN]
          *DO (lcAppName) WITH 'Glsgvlu','"&lcCount","&laAccSeg[lnCount]"'
		   DO (lcAppName) WITH 'Glsgvlu','"&lcCounter","&laAccSeg[lnCounter]"'
		  *B602221,1 AAN Fix bug that not enter the desc. of the third segment [End]
		  *E300683,5 end
          *E lcSavExact = SET('EXACT')
          *E SET EXACT ON

          *E IF SEEK ('WIN'+lcWindow+UPPER(gcUser_Id)+gcStation,'syustatc')   
            *** Restore data for this window
            *E RESTORE FROM MEMO syustatc.mObj_Data  ADDITIVE
          *E ENDIF
          *E SET EXACT &lcSavExact

          *E glQuitting = .F.        
           gcBaseWind = lcCurrWind
           =gfSetup()
          
          *E IF !llSave
          *E   llCallRet  = .F.
          *E   EXIT
          *E ENDIF  

          *lcParameter = "'"+lcCount+"','"+laAccSeg[lnCount]+"'"
          *DO gpDoProg WITH 'AWRGLSGVLU',.F.,'GL',lcParameter
             
          *E301077,72 Call gpDoProg to run the specified screen[End  ]

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

*** Add the combination of the whole segments in the ***
*** chart of account file by branching to the single ***
*** account screen ...
IF !llSngAcn
  IF WEXIST('AWRGLSNGAC')
    =gfModalgen('INM00009B00000','A')         && Close window first
    llCallRet  = .F.
  ELSE

    *E301077,72 Call gpDoProg to run the specified screen[Begin]
    *E301077,72 like run it from menu, to take system
    *E301077,72 handle files open and close .
     lcCurrWind = gcBaseWind
     =gfStatic()
     lcWindow   = UPPER(lcBaseWind)
     llSavSngAc = .F.
     glFirsTime = .T.

     gcBaseWind ='AWRGLSNGAC'
    
     lcAppName  = gcAppHome+'GL.APP'
    *E300683,5 Change call to pass parameter
    *DO (lcAppName) WITH 'GLSNGAC WITH "&lcAccCode"'
     DO (lcAppName) WITH 'GLSNGAC', '"&lcAccCode"'
    *E300683,5 end
    
    *E lcSavExact = SET('EXACT')
    *E SET EXACT ON

    *E IF SEEK ('WIN'+lcWindow+UPPER(gcUser_Id)+gcStation,'syustatc')
      *** Restore data for this window
    *E   RESTORE FROM MEMO syustatc.mObj_Data  ADDITIVE
    *E ENDIF
    *E SET EXACT &lcSavExact
    *E glQuitting = .F.    
    *E llCallRet  = llSavSngAc
     gcBaseWind = lcCurrWind    
     =gfSetup()
    
    *lcParameter = "'"+lcAccCode+"'"
    *gcBaseWind ='AWRGLSNGAC'
    *DO gpDoProg WITH gcBaseWind,.F.,'GL',lcParameter
             
    *E301077,72 Call gpDoProg to run the specified screen[End  ]
    
  ENDIF
ENDIF
IF llCallRet
  IF FSIZE(lcObj_Nam) > 0
    REPLACE &lcObj_Nam WITH lcAccCode  
  ELSE
    &lcObj_Nam = lcAccCode
  ENDIF
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
  lcAccDes1 = ALLTRIM(lcAccDes1) + SPACE(65 - LEN(lcAccDes1))
  _CUROBJ = OBJNUM(lcObj_Nam)
  SHOW GET &lcObj_Nam ENABLE
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

*** If this account major between any ranges ***
*** Check if the account type code for this  ***
*** account major is allowed in the calling  ***
*** program or not ...

IF lcAccType1 <> "A"
  lcTmpTyp = IIF(lcAccType1 = 'T','Y','N')

  IF lcTyp2Chck <> lcTmpTyp
  
    *** If the type for this account major is not allewed ***
    *** to add in the calling program ..
 
    *** This account major has statistical(standard).. type ***
    *** Only standard(statistical) types is allowed ...

    IF lcTmpTyp = 'Y'
      =gfModalGen("INM02031B00000","DIALOG",laAccSeg [1])
    ELSE
      =gfModalGen("INM02030B00000","DIALOG",laAccSeg [1])
    ENDIF
    RETURN .F.
  ENDIF
ENDIF

