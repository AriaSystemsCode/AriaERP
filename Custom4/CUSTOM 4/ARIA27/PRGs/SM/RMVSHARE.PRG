PARAMETER lcSysPath
PRIVATE lcAriaPath,lcCommand
IF TYPE('lcSysPath') <> 'C'
  lcSysPath = GETDIR('','Select System Files Directory')
ENDIF
IF EMPTY(lcSysPath)
  WAIT WINDOW 'No Sysfiles directory found...!'
  RETURN
ENDIF
lcAriaPath = SUBSTR(lcSysPath,1,LEN(lcSysPath) - 9)
lcCommand = "!ATTRIB " + lcAriaPath + "REPORTS\AR\ARPINV*.* -R"
&lcCommand
ERASE (lcAriaPath + 'REPORTS\AR\ARPINV.FXP')
ERASE (lcAriaPath + 'REPORTS\AR\ARPINVA.FRX')
ERASE (lcAriaPath + 'REPORTS\AR\ARPINVA.FRT')
ERASE (lcAriaPath + 'REPORTS\AR\ARPINVB.FXP')
ERASE (lcAriaPath + 'REPORTS\AR\ARPINVC.FXP')
ERASE (lcAriaPath + 'REPORTS\AR\ARPINVD.FXP')
ERASE (lcAriaPath + 'REPORTS\AR\ARPINVE.FXP')
ERASE (lcAriaPath + 'REPORTS\AR\ARPINVF.FXP')
ERASE (lcAriaPath + 'REPORTS\AR\ARPINVG.FXP')
ERASE (lcAriaPath + 'REPORTS\AR\ARPINVH.FXP')
ERASE (lcAriaPath + 'REPORTS\AR\ARPINVI.FXP')
ERASE (lcAriaPath + 'REPORTS\AR\ARPINVJ.FXP')
ERASE (lcAriaPath + 'REPORTS\AR\ARPINVK.FXP')

lcCommand = "!ATTRIB " + lcAriaPath + "REPORTS\PO\POSTYP*.* -R"
&lcCommand
ERASE (lcAriaPath + 'REPORTS\PO\POSTYP.FXP')
ERASE (lcAriaPath + 'REPORTS\PO\POSTYPA.FRX')
ERASE (lcAriaPath + 'REPORTS\PO\POSTYPA.FRT')

lcCommand = "!ATTRIB " + lcAriaPath + "REPORTS\PO\POSTYDRE.FXP -R"
&lcCommand
ERASE (lcAriaPath + 'REPORTS\PO\POSTYDRE.FXP')

lcCommand = "!ATTRIB " + lcAriaPath + "REPORTS\RM\RMCMEM*.* -R"
&lcCommand
ERASE (lcAriaPath + 'REPORTS\RM\RMCMEM.FXP')
ERASE (lcAriaPath + 'REPORTS\RM\RMCMEMA.FRX')
ERASE (lcAriaPath + 'REPORTS\RM\RMCMEMA.FRT')

lcCommand = "!ATTRIB " + lcAriaPath + "REPORTS\IC\ICSTYREP.FXP -R"
&lcCommand
lcCommand = "!ATTRIB " + lcAriaPath + "REPORTS\IC\ICSTYBYW.* -R"
&lcCommand
lcCommand = "!ATTRIB " + lcAriaPath + "REPORTS\IC\ICSTYBYO.* -R"
&lcCommand
ERASE (lcAriaPath + 'REPORTS\IC\ICSTYREP.FXP')
ERASE (lcAriaPath + 'REPORTS\IC\ICSTYBYW.FRX')
ERASE (lcAriaPath + 'REPORTS\IC\ICSTYBYW.FRT')
ERASE (lcAriaPath + 'REPORTS\IC\ICSTYBYO.FRX')
ERASE (lcAriaPath + 'REPORTS\IC\ICSTYBYO.FRT')


