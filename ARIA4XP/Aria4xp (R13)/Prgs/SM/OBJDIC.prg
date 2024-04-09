*B610632,1 SAB 12/23/2013 Fix error when open object dictionary screen [T20131115.0005]

WAIT WINDOW "Please wait, object dictionary is loading" NOWAIT

o = CREATEOBJECT("Aria.ObjectDictionary.UI.AriaObjectDictionaryUI")
*B610632,1 SAB 12/23/2013 Fix error when open object dictionary screen [T20131115.0005][Start]
o.ServerName = oAriaApplication.RemoteServer
o.ServerPort = oAriaApplication.RemotePort
*B610632,1 SAB 12/23/2013 Fix error when open object dictionary screen [T20131115.0005][End]
o.showObjectDictionaryUI()

WAIT CLEAR
