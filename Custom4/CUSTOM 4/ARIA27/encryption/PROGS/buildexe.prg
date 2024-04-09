*** 
*** ReFox XI  #ES110215  Edelmiro González Martínez  Asesoria Informatica Gallega S.L [VFP90]
***
LCPATH = GETDIR('', '', 'Select Sysfiles Folder Please')
IF  .NOT. EMPTY(LCPATH)
LCSYSTEMPATH = LCPATH
LCMAPPATH = LCPATH
CD (LCSYSTEMPATH)
SET TALK OFF
SET TEXTMERGE ON
STORE FCREATE('Mappings.prg') TO _TEXT
TEXT NOSHOW

  PROCEDURE OpenFile
  LPARAMETERS lcFile, lcTag
  LOCAL oFile
  IF !USED(lcFile)
    USE (lcFile) IN 0 ORDER TAG (lcTag) SHARED
  ELSE
    SELECT (lcFile)
    SET ORDER TO TAG (lcTag) IN (lcFile)
  ENDIF
  
ENDTEXT
= FCLOSE(_TEXT)
STORE FCREATE('Temp.prg') TO _TEXT
TEXT NOSHOW
  
  PROCEDURE Temp
    _screen.WindowState= 1
    _screen.Visible = .F.
    SET TALK OFF
    SET SAFETY OFF
    
        
    DECLARE laMapFiles[19]
    laMapFiles[1] = 'SycEdiFs'
    laMapFiles[2] = 'SycEdiSf'
    laMapFiles[3] = 'SycEdiSg'
    laMapFiles[4] = 'SycEdiSh'
    laMapFiles[5] = 'SYCEDITX'
    laMapFiles[6] = 'SYCASNLB'
    laMapFiles[7] = 'SYCEDISC'
    laMapFiles[8] = 'SYCEDIPR'
    laMapFiles[9] = 'SYCEDIRS'
    laMapFiles[10]= 'SYCEDISM'
    laMapFiles[11]= 'SYCEDIPO'
    laMapFiles[12]= 'SYCEDIPA'
    laMapFiles[13]= 'SYCEDIPC'
    laMapFiles[14]= 'SYCEDICD'
    laMapFiles[15]= 'SYCEDIRF'
    laMapFiles[16]= 'SYCUPCLB'
    laMapFiles[17]= 'SYCEDIOW'
    laMapFiles[18]= 'SYCASNHD'
    laMapFiles[19]= 'SYCASNDT'
    
    CREATE PROJECT ('mappings') NOSHOW SAVE nowait
    FOR lnCount = 1 TO ALEN(laMapFiles)
      IF FILE(laMapFiles[lnCount]+'.DBF')
        APPLICATION.PROJECTS(1).Files.Add(laMapFiles[lnCount]+'.DBF')
        APPLICATION.PROJECTS(1).Files(laMapFiles[lnCount]+'.DBF').Exclude = .F.
      ENDIF
      IF FILE('BackUp\'+laMapFiles[lnCount]+'.DBF')
        APPLICATION.PROJECTS(1).Files.Add('BackUp\'+laMapFiles[lnCount]+'.DBF')
        APPLICATION.PROJECTS(1).Files(laMapFiles[lnCount]+'.DBF').Exclude = .F.
      ENDIF
    ENDFOR
    
    APPLICATION.PROJECTS(1).Files.Add('mappings.prg')
    APPLICATION.PROJECTS(1).Files('mappings.prg').Exclude=.F.
    APPLICATION.PROJECTS(1).setmain('mappings.prg')
    
    APPLICATION.PROJECTS(1).BUILD('mappings',3)
    APPLICATION.PROJECTS(1).close
    
    IF !DIRECTORY('BackUp')
      MD ('BackUp')
    ENDIF
    FOR lnCount = 1 TO ALEN(laMapFiles)
      IF FILE(laMapFiles[lnCount]+'.DBF')
        COPY FILE (laMapFiles[lnCount]+'.*') TO ('BackUp\'+laMapFiles[lnCount]+'.*')
        ERASE (laMapFiles[lnCount]+'.*')
      ENDIF
    ENDFOR
    ERASE ('Mappings.prg')
    ERASE ('Mappings.pjx')
    ERASE ('Mappings.pjt')
    KEYBOARD '{ALT+F4}'
ENDTEXT
= FCLOSE(_TEXT)
RUN /n "..\VFP9.exe" "temp.prg"
ENDIF
ENDPROC
