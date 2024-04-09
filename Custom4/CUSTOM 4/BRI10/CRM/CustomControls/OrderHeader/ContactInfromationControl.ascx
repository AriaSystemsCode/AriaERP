<%@ Control Language="C#" AutoEventWireup="true" CodeFile="ContactInfromationControl.ascx.cs"
    Inherits="Contact_Infromation" %>
<%@ Register Assembly="AjaxControlToolkit" Namespace="AjaxControlToolkit" TagPrefix="cc1" %>
<asp:Panel ID="Panel2" runat="server" Height="36px" Width="780px" Style="position: relative;
    left: 0px;">
    <table style=" position:relative;top:10px">
        <tr>
            <td style="width: 10px; height: 26px;" >
            </td>
            <td style="height: 26px">
                <asp:LinkButton ID="Label1" runat="server" Text="Name:" OnClick="Label1_Click"></asp:LinkButton>
            </td>
            <td style="height: 26px">
              &nbsp;</td>
            <td style="width: 11px; height: 26px;">
              <asp:TextBox ID="ContactName" runat="server" MaxLength="30" OnTextChanged="ContactName_TextChanged" AutoPostBack="True"></asp:TextBox></td>
            <td style="height: 26px">
                <asp:Label ID="Label3" runat="server" Text="Title:"></asp:Label>
            </td>
            <td style="height: 26px">
                <asp:TextBox ID="ContactTitle" runat="server" Width="198px" MaxLength="30" Enabled="False"></asp:TextBox>&nbsp;
            </td>
            <td style="width: 10px; height: 26px;">
            </td>
            <td style="height: 26px">
                <asp:Label ID="Label4" runat="server" Text="Phone#:"></asp:Label>
            </td>
            <td style="height: 26px">
                <asp:TextBox ID="Phone" runat="server" Width="105px" MaxLength="16"></asp:TextBox>
            </td>
        </tr>
    </table>
</asp:Panel>
<cc1:RoundedCornersExtender ID="RoundedCornersExtender1" runat="server" BorderColor="LightGray"
    TargetControlID="Panel2">
</cc1:RoundedCornersExtender>
<asp:Panel ID="Panel1" runat="server" Height="1px" Style="left: 12px; position: relative;
    top: -59px; background-color: white" Width="125px">
    <asp:Label ID="Label2" runat="server" Text="Contact Information" Width="100%" BackColor="White"></asp:Label>
    </asp:Panel>
<cc1:AutoCompleteExtender ID="AutoCompleteExtenderContact" runat="server"
  EnableCaching="false" MinimumPrefixLength="1"
  ServiceMethod="GetCompletionList" ServicePath="~/WebServices/AutoComplete/OrderHeader/ContactAutoComplete.asmx"
  TargetControlID="ContactName" UseContextKey="True" CompletionListCssClass ="Contact">
</cc1:AutoCompleteExtender>
