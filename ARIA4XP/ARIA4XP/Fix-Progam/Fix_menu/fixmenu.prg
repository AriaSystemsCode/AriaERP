*T20120207.0013
CLOSE ALL
SET DELETED ON

lcSysFiles = ""
lcSysFiles = GETDIR("","Get Aria27 Sysfiles")
lcFile     = UPPER(ADDBS(lcSysFiles)+"SYCMENU.DBF")
IF !EMPTY(lcSysFiles) and DIRECTORY(lcSysFiles) and FILE(lcFile)
  USE (ADDBS(lcSysFiles)+"SYCMENU.DBF") IN 0 SHARED
  SELECT SYCMENU
  SET ORDER TO APPPOPBAR   && CAPP_ID+CPAD_POS+CPOP_POS+CPOP_LEVL+CBAR_POS
  GO TOP
  =SEEK("PO")
  YPos = ""
  SCAN REST WHILE CAPP_ID+CPAD_POS+CPOP_POS+CPOP_LEVL+CBAR_POS  = "PO"
    IF ALLTRIM(UPPER(SYCMENU.CSUB_PRPT)) == '\<RECEIVINGS'
      YPos = ALLTRIM(UPPER(SYCMENU.CPROSS_ID))
      EXIT
    ENDIF
  ENDSCAN

  IF !EMPTY(YPos)
    GO TOP
    =SEEK("PO")

    SCAN REST WHILE CAPP_ID+CPAD_POS+CPOP_POS+CPOP_LEVL+CBAR_POS  = "PO"
      IF INLIST(ALLTRIM(UPPER(SYCMENU.CPROSS_ID)),'MFRCVTP','POACFRV','POISSBT','PORCVIP','PORCVRT','PORECPO','POSTREC')
        REPLACE cmstr_nam WITH YPos
      ENDIF
    ENDSCAN
    =MESSAGEBOX("Update RECEIVINGS, Sub menu items is Done.")
  ELSE
    =MESSAGEBOX("RECEIVINGS, item menu not found")

  ENDIF

  USE IN sycmenu
ELSE
  =MESSAGEBOX("Sysfiles path not found or incorrect")
ENDIF