****** MAB 09/22/2000 Mohamed Atia Badran
*B603908,1 MAB
*****************************************
PARAMETERS  lcDataDir
PRIVATE lcSysDir , lnI , llUsed , lcOrder , lcDataDir
lcSysDir = ""
lcSysDir = GETDIR('','Select System files directory')
IF !EMPTY(lcSysDir)
  IF FILE(lcSysDir+"sydfield.DBF")
    DECLARE laCorrupt[7]
    laCorrupt[1] = "CBARMODULE"
    laCorrupt[2] = "CERRSEQNCE"
    laCorrupt[3] = "CMODPARENT"
    laCorrupt[4] = "CRETNOTE4"
    laCorrupt[5] = "CTRANMTHD"
    laCorrupt[6] = "LUPCPACK"
    laCorrupt[7] = "LPACKUPC"

    llUsed = USED("sydfield")
    IF llUsed
      SELECT sydfield
      lcOrder = ORDER()
      SET ORDER TO Cfld_name
    ELSE
      USE (lcSysDir+"sydfield") ORDER Cfld_name IN 0 SHARED
    ENDIF  
    SELECT sydfield
    lnI = 0
    FOR lnI = 1 TO ALEN(laCorrupt)
      IF SEEK(laCorrupt[lnI])
        BLANK REST WHILE cfld_name = laCorrupt[lnI] FOR cupgrdlvl = "U"
      ENDIF
    ENDFOR
    LOCATE
    DELETE FOR EMPTY(cfld_name)

    IF llUsed
      SELECT sydfield
      SET ORDER TO &lcOrder
    ELSE
      USE IN sydfield
    ENDIF  
    
  ELSE
    WAIT WINDOW "Invalid System files directory" TIMEOUT 3
  ENDIF
ENDIF
