<%@ Page Language="C#" MasterPageFile="~/WebPages/MasterPage.master" AutoEventWireup="true"
    CodeFile="Order.aspx.cs" Inherits="Order" Title="Untitled Page" EnableEventValidation = "false" AsyncTimeout = "3600" %>

<%@ Register Src="~/CustomControls/OrderDetails/OrderDetailsContainerControl.ascx"
    TagName="OrderDetailsContainerControl" TagPrefix="uc2" %>
<%@ Register Src="~/CustomControls/OrderDetails/TemplateControl.ascx" TagName="TemplateControl"
    TagPrefix="uc1" %>
<%@ Register Src="~/CustomControls/OrderSummary/OrderSummaryControl.ascx" TagName="OrderSummaryControl"
    TagPrefix="uc11" %>
<%@ Register Src="~/CustomControls/OrderDetails/ButtonsControl.ascx" TagName="ButtonsControl"
    TagPrefix="uc10" %>
<%@ Register Src="~/CustomControls/OrderDetails/ProfilesControl.ascx" TagName="ProfilesControl"
    TagPrefix="uc9" %>
<%@ Register Src="~/CustomControls/OrderDetails/DescriptionControl.ascx" TagName="DescriptionControl"
    TagPrefix="uc8" %>
<%@ Register Src="~/CustomControls/OrderDetails/StylesControl.ascx" TagName="StylesControl"
    TagPrefix="uc7" %>
<%@ Register Src="~/CustomControls/OrderHeader/OrderInfoControl.ascx" TagName="OrderInfoControl"
    TagPrefix="uc6" %>
<%@ Register Src="~/CustomControls/OrderHeader/GeneralControl.ascx" TagName="GeneralControl"
    TagPrefix="uc3" %>
<%@ Register Src="~/CustomControls/OrderHeader/SalesRepsInformationControl.ascx"
    TagName="SalesRepsInformationControl" TagPrefix="uc4" %>
<%@ Register Src="~/CustomControls/OrderHeader/NoteControl.ascx" TagName="NoteControl"
    TagPrefix="uc5" %>
<%@ Register Src="~/CustomControls/OrderHeader/ContactInfromationControl.ascx" TagName="ContactInfromationControl"
    TagPrefix="uc2" %>
<%@ Register Src="~/CustomControls/OrderHeader/AddressOrderControl.ascx" TagName="AddressOrderControl"
    TagPrefix="uc1" %>
<%@ Register Assembly="AjaxControlToolkit" Namespace="AjaxControlToolkit" TagPrefix="cc1" %>


<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" runat="Server">
    <script language = "jscript" type = "text/jscript">
    var lastValue = "";

    function SaveLastValue(value)
    {
      lastValue = value;
      
    }

    function IsValidDate(fld)
    {
      var RegExPattern = /^(?=\d)(?:(?:(?:(?:(?:0?[13578]|1[02])(\/|-|\.)31)\1|(?:(?:0?[1,3-9]|1[0-2])(\/|-|\.)(?:29|30)\2))(?:(?:1[6-9]|[2-9]\d)?\d{2})|(?:0?2(\/|-|\.)29\3(?:(?:(?:1[6-9]|[2-9]\d)?(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00))))|(?:(?:0?[1-9])|(?:1[0-2]))(\/|-|\.)(?:0?[1-9]|1\d|2[0-8])\4(?:(?:1[6-9]|[2-9]\d)?\d{2}))($|\ (?=\d)))?(((0?[1-9]|1[012])(:[0-5]\d){0,2}(\ [AP]M))|([01]\d|2[0-3])(:[0-5]\d){1,2})?$/;

      if ((fld.value.match(RegExPattern)) && (fld.value!='')) 
      {
      } 
      else 
      {
        alert("Invalid date fromat!");
        fld.value = lastValue;
        fld.focus();
        fld.select();
        LoadingComplete();
      }
    } 

    function IsValidDateOrEmpty(fld)
    {
      var RegExPattern = /^(?=\d)(?:(?:(?:(?:(?:0?[13578]|1[02])(\/|-|\.)31)\1|(?:(?:0?[1,3-9]|1[0-2])(\/|-|\.)(?:29|30)\2))(?:(?:1[6-9]|[2-9]\d)?\d{2})|(?:0?2(\/|-|\.)29\3(?:(?:(?:1[6-9]|[2-9]\d)?(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00))))|(?:(?:0?[1-9])|(?:1[0-2]))(\/|-|\.)(?:0?[1-9]|1\d|2[0-8])\4(?:(?:1[6-9]|[2-9]\d)?\d{2}))($|\ (?=\d)))?(((0?[1-9]|1[012])(:[0-5]\d){0,2}(\ [AP]M))|([01]\d|2[0-3])(:[0-5]\d){1,2})?$/;

      if ((fld.value.match(RegExPattern)) || (fld.value == '')) 
      {
      } 
      else 
      {
        alert("Invalid date fromat!");
        fld.value = lastValue;
        fld.focus();
        fld.select();
        LoadingComplete();
      }
    } 

    function IsNumber(fld, length)
    {
      var RegExPattern = /^[0-9]+$/;

      if ((fld.value.match(RegExPattern) && fld.value.length <= length) || fld.value == '')
      {
      }
      else
      {
        alert("Invalid number!");
        fld.value = lastValue;
        fld.focus();
        fld.select();
        LoadingComplete();
      }
    }

    function IsDouble(fld)
    {
      var RegExPattern = /^\d{1,4}(\.\d{1,3}){0,1}$/;

      if (fld.value.match(RegExPattern) || fld.value == '')
      {
      }
      else
      {
        alert("Invalid number!");
        fld.value = lastValue;
        fld.focus();
        fld.select();
        LoadingComplete();
      }
    }
    
    function IsPercent(fld)
    {
      var RegExPattern = /^\d{1,2}(\.\d{1,2}){0,1}$/;

      if (fld.value.match(RegExPattern) || fld.value == '')
      {
      }
      else
      {
        alert("Invalid number!");
        fld.value = lastValue;
        fld.focus();
        fld.select();
        LoadingComplete();
      }
    }

    function IsCurrency(fld)
    {
      var RegExPattern = /^\d{1,9}(\.\d{1,2}){0,1}$/;

      if (fld.value.match(RegExPattern) || fld.value == '')
      {
      }
      else
      {
        alert("Invalid number!");
        fld.value = lastValue;
        fld.focus();
        fld.select();
        LoadingComplete();
      }
    }

    function Loading(loading)
    {
      try
      {
        window.document.getElementById("LoadingControl").style.top = 27;
        window.document.getElementById("LoadingControl").value = loading;
      }
      catch(e)
      {
      alert(e.message);
      }
    }

    function LoadingComplete()
    {
      try
      {
      window.document.getElementById("LoadingControl").style.top = -100;
      }
      catch(e)
      {
      alert(e.message);
      }
    }


    function LoadConfig()
    {
      var args = window; 
      var modal = window.showModalDialog("configure.aspx", args, "dialogWidth=820px;dialogHeight=650px;scrollbars=yes;resizable=no;status=no;help=no"); 
    }


    </script>

    <cc1:ToolkitScriptManager ID="ToolkitScriptManager1" runat="server" AsyncPostBackTimeout = "3600"  >
        <Services>
            <asp:ServiceReference Path="~/WebServices/AutoComplete/OrderDetails/StyleCodeAutoComplete.asmx" />
            <asp:ServiceReference Path="~/WebServices/AutoComplete/OrderDetails/StyleDescriptionAutoComplete.asmx" />
            <asp:ServiceReference Path="~/WebServices/AutoComplete/OrderHeader/SalesRepAutoComplete.asmx" />
        </Services>
    </cc1:ToolkitScriptManager>
              <input type = "text" ID="LoadingControl"
                Style="left: 708px; color: white; position: absolute; top: 27px;
                background-color: HotPink; z-index: 1; width: 94px; border-top-style: none; border-right-style: none; border-left-style: none; height: 19px; border-bottom-style: none;" Visible="False" value="    Loading..." />

                        <asp:UpdatePanel ID="OrderAllPages" runat="server">
                            <contenttemplate>

    <cc1:Accordion ID="Accordion1" runat="server" HeaderSelectedCssClass="AccordionExpandedHeader"
        HeaderCssClass="AccordionCollapsedHeader" ContentCssClass="AccordionContent"
        TransitionDuration="80"  SuppressHeaderPostbacks ="false ">
        <Panes>
            <cc1:AccordionPane ID="AccordionPane1" runat="server" AccessKey = "h" >
                <Header>
                    <table style="position: relative; top: 3px; left: 3px">
                        <tr>
                            <td>
                                <asp:Label ID="Label1" runat="server" Font-Bold="true" ForeColor="white" Font-Names="tahoma"
                                    Font-Size="Small" Text="Account#"></asp:Label>
                            </td>
                            <td>
                                <asp:Label ID="Label2" runat="server" Font-Bold="true" ForeColor="white" Font-Names="tahoma"
                                    Font-Size="Small" Text=""></asp:Label></td>
                            <td>
                                <asp:Label ID="Label3" runat="server" Font-Bold="true" ForeColor="white" Font-Names="tahoma"
                                    Font-Size="Small" Text=" >> Order Header"></asp:Label></td>
                            <td style="width: 85px">
                            </td>
                            <td>
                                <asp:CheckBox ID="CheckBox1" runat="server" Font-Bold="true" ForeColor="white" Font-Names="tahoma"
                                    Font-Size="Small" Text="Multi Ship To" Enabled="false" /></td>
                            <td>
                                <asp:CheckBox ID="CheckBox2" runat="server" Font-Bold="true" ForeColor="white" Font-Names="tahoma"
                                    Font-Size="Small" Text="Multi Customer PO" Enabled="false" OnCheckedChanged="CheckBox2_CheckedChanged" AutoPostBack =true/></td>
                            </td>
                            <td>
                                <asp:Button ID="BtnLogin" runat="server" Text="Login" Visible = false />
                            </td>
                        </tr>
                    </table>
                        <div>        
        <asp:Panel ID="Panel1" runat="server" CssClass="modalPopup" Style="display: none">
            <table width="100%">
                <tr>
                    <td style="height: 40px">
                        <asp:Panel ID="Panel3" runat="server" Height="30px" Style="cursor: move; background-color: #DDDDDD;
                            border: solid 1px Gray; color: Black" Width="213px">
                            <div style="text-align: center; position: relative; top: 7px">
                                Login</div>
                        </asp:Panel>
                    </td>
                </tr>
                <tr>
                    <td>
                        <table>
                            <tr>
                                <td align="center">
                                    <asp:TextBox ID="txtUserName" runat="server" Width="200px"></asp:TextBox>
                                </td>
                            </tr>
                            <tr>
                                <td align="center">
                                    <asp:TextBox ID="txtPassword" runat="server" Width="200px"></asp:TextBox>
                                </td>
                            </tr>
                        </table>
                    </td>
                </tr>
                <tr>
                    <td align="center">
                        <div align="center">
                            <asp:Button ID="OkButton" runat="server" Text="OK" OnClick="OkButton_Click" Width="76px" />
                            <asp:Button ID="CancelButton" runat="server" Text="Cancel" Width="76px" />
                        </div>
                    </td>
                </tr>
            </table>
        </asp:Panel>
        <cc1:ModalPopupExtender ID="ModalPopupExtender1" runat="server" TargetControlID="BtnLogin"
            PopupControlID="Panel1" BackgroundCssClass="modalBackground" DropShadow="true"
            OnOkScript="" CancelControlID="CancelButton">
        </cc1:ModalPopupExtender>
                        
    </div>
                </Header>
                <Content>
                    <div>
                        <asp:UpdatePanel ID="OrderHeaderUpdatePanel" runat="server">
                            <contenttemplate>
                    <table style="width: 789px;" id="TABLE1">
                        <tr>
                            <td style="width: 460px">
                                <uc1:AddressOrderControl ID="AddressOrderControl1" runat="server" />
                            </td>
                            <td>
                                <uc6:OrderInfoControl ID="OrderInfoControl1" runat="server" />
                            </td>
                        </tr>
                        <tr>
                            <td colspan="2" style="top: 5px">
                                <uc2:ContactInfromationControl ID="ContactInfromationControl1" runat="server" />
                            </td>
                        </tr>
                        <tr>
                            <td colspan="2" style="height: 100px; top: 15px; z-index: 5">
                                <uc3:GeneralControl ID="GeneralControl1" runat="server" />
                            </td>
                        </tr>
                        <tr>
                            <td colspan="2" style="height: 33px; position: relative; top: 5px; z-index: 4">
                                <uc4:SalesRepsInformationControl ID="SalesRepsInformationControl1" runat="server" />
                            </td>
                        </tr>
                        <tr>
                            <td colspan="2" style="height: 65px; position: relative; top: 10px; z-index: 2">
                                <uc5:NoteControl ID="NoteControl1" runat="server" />
                            </td>
                        </tr>
                        <tr>
                            <td>
                                <br />
                            </td>
                        </tr>
                    </table>
                    </div>
                    </contenttemplate> </asp:UpdatePanel>
                </Content>
            </cc1:AccordionPane>
            <cc1:AccordionPane ID="AccordionPane2" runat="server" AccessKey = "d">
                <Header>
                    <table style="position: relative; top: 3px; left: 3px">
                        <tr>
                            <td>
                                <asp:Label ID="Label4" runat="server" Font-Bold="true" ForeColor="white" Font-Names="tahoma"
                                    Font-Size="Small" Text="Account#"></asp:Label>
                            </td>
                            <td>
                                <asp:Label ID="Label5" runat="server" Font-Bold="true" ForeColor="white" Font-Names="tahoma"
                                    Font-Size="Small" Text=""></asp:Label></td>
                            <td>
                                <asp:Label ID="Label6" runat="server" Font-Bold="true" ForeColor="white" Font-Names="tahoma"
                                    Font-Size="Small" Text=" >> Order Details"></asp:Label></td>
                            <td style="width: 30px">
                            </td>
                        </tr>
                    </table>
                </Header>
                <Content>
                    <asp:UpdatePanel ID="OrderDetailsUpdatePanel" runat="server">
                        <contenttemplate>
                            <uc2:OrderDetailsContainerControl ID="OrderDetailsContainerControl1" runat="server" />
                            <uc1:TemplateControl Visible="false" ID="TemplateControl1" runat="server" />
                        </contenttemplate>
                    </asp:UpdatePanel>
                </Content>
            </cc1:AccordionPane>
            <cc1:AccordionPane ID="AccordionPane3" runat="server" AccessKey = "n">
                <Header>
                    <table style="position: relative; top: 3px; left: 3px">
                        <tr>
                            <td>
                                <asp:Label ID="Label7" runat="server" Font-Bold="true" ForeColor="white" Font-Names="tahoma"
                                    Font-Size="Small" Text="Account#"></asp:Label>
                            </td>
                            <td>
                                <asp:Label ID="Label8" runat="server" Font-Bold="true" ForeColor="white" Font-Names="tahoma"
                                    Font-Size="Small" Text=""></asp:Label></td>
                            <td>
                                <asp:Label ID="Label9" runat="server" Font-Bold="true" ForeColor="white" Font-Names="tahoma"
                                    Font-Size="Small" Text=" >> Order Summary"></asp:Label></td>
                            <td style="width: 30px">
                            </td>
                        </tr>
                        
                    </table>
                </Header>
                <Content>
                    <asp:UpdatePanel ID="OrderSummaryUpdatePanel" runat="server">
                        <contenttemplate>
                    <table style="width: 789px">
                        <tr>
                            <td>
                                <uc11:OrderSummaryControl ID="OrderSummary1" runat="server" />
                            </td>
                        </tr>
                    </table>
                    </contenttemplate>
                    </asp:UpdatePanel>
                </Content>
            </cc1:AccordionPane>
        </Panes>
    </cc1:Accordion>
</contenttemplate>
</asp:UpdatePanel>
</asp:Content>
