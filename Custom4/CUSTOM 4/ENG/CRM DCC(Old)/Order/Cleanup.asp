<HTML>
<HEAD>
<TITLE>Session Cleanup</Title>
</HEAD>
<BODY Onload="CallClose();">
<%
'This page is used to clean up any Crystal Reports RDC Runtime objects remaining in a
'a user's session.  This page is called by a new browser window that is launched when
'a user closes a browser window containing a Crystal Reports viewer.
'
'You want to make sure that you clean up any instances of the oApp, oRpt and oPageEngine
'objects for 2 reasons:
'1) Licensing
'2) Resources
'When you create an instance of the Application and Report objects you are using
'an RDC Runtime license.  Once you hit your maximum concurrent users (all licenses in use)
'you will no longer be able to create an instance of the report object (session("oRpt")
'Thus, when a user closes his/her browser you want to make sure that user's session is
'terminated as quickly as possible so that their license can be reclaimed.
'By destroying the session objects you are also reclaiming memory.


Function DestroyObjects(ByRef ObjectToDestroy)
  If isobject(ObjectToDestroy) then
    set ObjectToDestroy = nothing
    DestroyObjects = true
  Else
    DestroyObjects = false
  End if
End Function

'This function takes an object (by reference rather than by value) and destroys it.
'If the object was destroyed we return true otherwise we return false.

If DestroyObjects(session("oPageEngine")) then
  Response.Write("Page Engine Object Destroyed" & "<BR>")
Else
  Response.Write("Page Engine Object not in Scope" & "<BR>")
End If

If DestroyObjects(session("oRpt")) then
  Response.Write("Report Object Destroyed" & "<BR>")
Else
  Response.Write("Report Object not in Scope" & "<BR>")
End If

If DestroyObjects(session("oApp")) then
  Response.Write("Application Object Destroyed" & "<BR>")  
Else
  Response.Write("Application Object not in Scope" & "<BR>")
End If

'This series of If...Then...Else loops calls the DestroyObjects function
'for each of the three main objects used in Crystal Reports ASP pages.
%>
<SCRIPT LANGUAGE="Javascript">

function CallClose()
{
	window.close();
}
</SCRIPT>
</BODY>
</HTML>
<%
'The javascript code is used to close the newly opened browser window once the page has run
Session.Abandon
Response.End
'These last two lines terminate that user's active session and flushes html to the browser.
%>
