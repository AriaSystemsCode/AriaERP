<%@ Language=VBScript %>
<html>

<head>
<title>New Page 1</title>
<meta name="GENERATOR" content="Microsoft FrontPage 4.0">
<meta name="ProgId" content="FrontPage.Editor.Document">
</head>
<%if Request.QueryString("fr") = "done" then%>

<%else%>
<%end if%>
<frameset rows="64,25%,*">
  <frame name="top" scrolling="no" noresize target="_parent" src="../login.asp">
  <frame name="middle" target="newwin" src="../login.asp">
  <frame name="bottom" src="../login.asp">
  <noframes>
  <body>

  <p>This page uses frames, but your browser doesn't support them.</p>

  </body>
  </noframes>
</frameset>

</html>
