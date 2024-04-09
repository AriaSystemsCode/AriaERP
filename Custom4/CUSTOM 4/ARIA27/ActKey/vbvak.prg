*:****************************************************************
*: Program file  : VBVAK.PRG
*: Program desc. : Valid Visual Basic Activation Key
*: System        : Aria Apparel System (A27).
*: Module        : System Manager (SM)
*: Developer     : ABDOU ELGENDI - (ABD) Due to E301874,1
*: Date          : 07/24/2002
*:****************************************************************
*: Calls
*:               : FUNCTIONS  : lfOpnGetVr.
*:               -----------------------------------------------
*:               : PROCEDURE  : None.
*:****************************************************************
*: Passed Parameters  : lcParam1 ==> Hold the run type, this param will
*:                    : hold 'S' in case run this case from the setup ,
*:                    : else will hold 'V' in case we run this exe
*:                    : from the Visual Basic.
*:                    : ------------------------------------------
*:                    : lcParam2 ==> Will Hold the path for the customer
*:                    : exe (Aria.EXE) in case run this exe from the setup
*:                    : or will hold the system directry in case run fom
*:                    : the visual basic.
*:****************************************************************
*:E301874,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*-- lccust_id   ==> Variable Hold the Customer Id.
*-- lccust_nam  ==> Variable Hold the Customer Name
*-- lcActKey    ==> Variable Hold the Activation Key.
*-- ldexpr_date ==> Variable Hold the Expiration Date.
*-- lcactplat   ==> Variable Hold the platform.
*-- Define needed Variables

PARAMETERS lcparam1, lcparam2

*-- This procedure to get out from the program in case any error.
ON ERROR DO lpErrHand
PRIVATE llvalid ,lccust_id , lccust_nam , lcactkey , lcactplat , lcsysactky ,;
  lcvalidchr , lcomparsys ,lcoldsydat

STORE '' TO lccust_id , lccust_nam , lcactkey , lcactplat , lcsysactky,;
  lcvalidchr , lcomparsys ,lcoldsydat
STORE {} TO ldexpr_date
STORE .F. TO llvalid

lnFnd2Prt = ATC(";",lcParam1)
IF TYPE("lcParam1") = "C" AND !EMPTY(lcparam1) .AND. lnFnd2Prt # 0
  IF lnFnd2Prt # 0
    lcAllParams = lcParam1
    lcParam1    = Left(lcAllParams,lnFnd2Prt-1)
    lcParam2    = SUBSTR(lcAllParams,lnFnd2Prt+1,Len(lcAllParams))
    ENDIF
ENDIF

llvalid = TYPE("lcParam1") = "C" AND;
  !EMPTY(lcparam1)       AND;
  TYPE("lcParam2") = "C" AND;
  !EMPTY(lcparam2)


IF llvalid
  lcparam1 = ALLTRIM(lcparam1)
  lcparam2 = ALLTRIM(lcparam2)
ENDIF

*-- The First Param should be S or D
llvalid = IIF(llvalid,(lcparam1 $ 'SV'),llvalid)

*-- In case the parameter valid or not.
IF llvalid
  *-- Case Run from the Setup screen.
  IF ALLTRIM(lcparam1) = 'S'
    IF FILE(lcparam2)
      = lfopngetvr (lcparam2)
      IF !(EMPTY(lcsysactky) .OR. EMPTY(lcomparsys)).AND. lcsysactky = lcomparsys
        llvalid = .T.
      ELSE
        llvalid = .F.
      ENDIF
    ELSE
      llvalid = .F.
    ENDIF
    lnfhand = FCREATE("A.N")
    = FPUTS(lnfhand, IIF(llvalid, "V", "N"))
    = FCLOSE(lnfhand)

  ELSE
    lccurrpath = lcparam2 + IIF(RIGHT(lcparam2,1)='\','','\')
    lcfilepath = lccurrpath + 'Act_Key.BIN'

    IF FILE(lcfilepath)
      *-- Function to Open the EXE file and Get the needed variables
      = lfopngetvr (lcfilepath)
      IF  lcsysactky = lcomparsys
        lcchrcase = 'V'
      ELSE
        lcchrcase = 'N'
      ENDIF





      DO CASE
          *-- Check on the Demo Version.
        CASE lcchrcase = 'V' .AND. 'D' $ ALLTRIM(lcactplat)
          IF FILE(lccurrpath+"I900INV.FXP")
            RESTORE FROM (lccurrpath+"I900INV.FXP") ADDI
            PRIVATE lcoldsydat
            lcoldsydat = SET ('DATE')
            SET DATE TO american
            IF DATE() > ldexpdate+30
              lcchrcase = lcchrcase +','+ 'N'
            ELSE
              lcchrcase = lcchrcase +','+ 'V'
            ENDIF
            SET DATE TO &lcoldsydat
          ELSE
            lcchrcase = lcchrcase +','+ 'N'
          ENDIF

          *-- Check on the expiration Version.
        CASE lcchrcase = 'V' .AND. 'E' $ ALLTRIM(lcactplat)
          PRIVATE lcoldsydat
          lcoldsydat = SET ('DATE')
          SET DATE TO american
          IF DATE() > CTOD(ldexpr_date)
            lcchrcase = lcchrcase +','+ 'N'
          ELSE
            lcchrcase = lcchrcase +','+ 'V'
          ENDIF
          SET DATE TO &lcoldsydat
        OTHERWISE
          lcchrcase = lcchrcase +','+ 'V'
      ENDCASE
    ELSE
      lcchrcase = 'N'
    ENDIF

    lnfhand = FCREATE("A.N")
    = FPUTS(lnfhand,lcchrcase)
    = FCLOSE(lnfhand)
  ENDIF
ELSE
  *-- In case not valid return 'N'
  lnfhand = FCREATE("A.N")
  = FPUTS(lnfhand,"N")
  = FCLOSE(lnfhand)
ENDIF

*-- End Of Code.
*:****************************************************************
*: Name      : lfOpnGetVr
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 07/25/2002
*: Purpose   : Function to Open File And get the data.
*:****************************************************************
*: Calls     :
*:             Procedures : ....
*:             Functions  : ....
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Passed Parameters  : ...
*:****************************************************************
*: Returns            : None.
*:****************************************************************
*: Example   : = lfOpnGetVr()
*:****************************************************************
*
FUNCTION lfopngetvr
PARAMETERS lcfilename

STORE FOPEN(lcfilename) TO file_handle		    && Open the file
IF file_handle > 0
  STORE FSEEK(file_handle,12876) TO ifp_size	&& Move pointer to Spec. Position
  STORE FREAD(file_handle, 76)   TO lcstring  && Store to memory
  IF !EMPTY(lcstring)
    *-- Get the Customer Id.
    lccust_id  = SUBSTR(lcstring,03,05)

    *-- Get the Customer Name.
    lccust_nam = SUBSTR(lcstring,08,030)

    *-- Get the Customer activation Key.
    lcactkey   = SUBSTR(lcstring,38,20)

    *-- Get the Customer expiration Date.
    ldexpr_date = IIF(EMPTY(SUBSTR(lcstring,58,10)),{},SUBSTR(lcstring,58,10))

    *-- Get the Customer Version Type.
    lcactplat  = SUBSTR(lcstring,68,04)

    *-- Get the Customer sys(2007) for the current record.
    lcsysactky = SUBSTR(lcstring,72,05)

    lcomparsys = SYS(2007,'NADABDXMLX'+lccust_nam+lcactkey+ldexpr_date+lcactplat)

    =FCLOSE(file_handle)
  ENDIF
ENDIF

*-- End Of lfOpnGetVr
*:****************************************************************
*: Name      : lpErrHand
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 07/25/2002
*: Purpose   : Procedure to get the invalid file.
*:****************************************************************
*: Calls     :
*:             Procedures : ....
*:             Functions  : ....
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Passed Parameters  : ...
*:****************************************************************
*: Returns            : None.
*:****************************************************************
*: Example   : Do lpErrHand
*:****************************************************************
*
PROCEDURE lpErrHand
WAIT Window  'PROCEDURE lpErrHand'
lnfhand = FCREATE("A.N")
= FPUTS(lnfhand, "N")
= FCLOSE(lnfhand)
QUIT

*-- End OF lpErrHand.
*:****************************************************************
