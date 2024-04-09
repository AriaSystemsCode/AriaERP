<%@ Page Language="C#" AutoEventWireup="true" CodeFile="CustomerSelection.aspx.cs"
  Inherits="CustomerSelection" Title="Untitled Page" EnableEventValidation = "false"  %>

<%@ Register Assembly="AjaxControlToolkit" Namespace="AjaxControlToolkit" TagPrefix="cc1" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">

<html xmlns="http://www.w3.org/1999/xhtml">
<head id="Head" runat="server">
  <title></title>
  <link href="../StyleSheet.css" rel="stylesheet" type="text/css" />
<script language ="jscript" type ="text/jscript">
function Loading(loading)
{
  window.document.getElementById("LoadingControl").style.top = 3;
  window.document.getElementById("LoadingControl").value = loading;
}

// -->
</script>

</head>

<body>
  <form id="Form" runat="server">
    <cc1:ToolkitScriptManager ID="ToolkitScriptManager1" runat="server">
    </cc1:ToolkitScriptManager>
    <asp:UpdatePanel ID="CustomerSelectionPanel" runat="server">
        <contenttemplate>
    <table>
      <tbody>
        <tr>
          <td style="width: 757px; height: 184px;">
            <asp:Panel Style="left: 0px; width: 789px; position: relative; top: 0px; background-color: #527ad5"
              ID="Panel1" runat="server" Width="125px" Height="39px">
              &nbsp;<asp:Label ID="Label1" runat="server" Font-Bold="True" Font-Names="Tahoma" Font-Size="Large"
                Height="18px" Style="left: 15px; position: relative; top: 12px" Text="Select Customer"
                Width="182px"></asp:Label>
              &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
              &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
              &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
              &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
              &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
              &nbsp; &nbsp; &nbsp;&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
              &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
              &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
              &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
              &nbsp; &nbsp; &nbsp; &nbsp;
              <input type = "text" ID="LoadingControl"
                Style="left: 690px; color: white; position: absolute; top: -100px;
                background-color: HotPink; z-index: 1; width: 94px; border-top-style: none; border-right-style: none; border-left-style: none; height: 19px; border-bottom-style: none;" Visible="False" value="    Loading..." />
            </asp:Panel>
            <asp:Panel Style="border-top: #aacbee 2px solid; left: 0px; width: 789px; position: relative;
              top: 0px; background-color: #527ad5" ID="Panel2" runat="server" Width="425px" Height="15px">
            </asp:Panel>
            <table style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
              width: 789px; border-bottom: #cadefa thin solid; position: relative; top: 0px;
              background-color: #eff3ff; left: 0px;">
              <tbody>
                <tr>
                  <td style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
                    width: 27px; border-bottom: #cadefa thin solid; height: 6px">
                    <asp:LinkButton ID="AcctLinkBtn" runat="server" Font-Size="Medium" Font-Names="Tahoma" OnClick="AcctLinkBtn_Click">Acct#    :</asp:LinkButton></td>
                  <td style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
                    width: 16px; border-bottom: #cadefa thin solid; height: 6px">
                            <asp:TextBox ID="AccountNoTextBox" runat="server" Width="190px" OnTextChanged="AccountNoTextBox_TextChanged"
                              AutoPostBack="True"></asp:TextBox>
                    </td>
                  <td style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
                    width: 25px; border-bottom: #cadefa thin solid; height: 6px">
                    <asp:LinkButton ID="NameLinkBtn" runat="server" Font-Size="Medium" Font-Names="Tahoma" OnClick="NameLinkBtn_Click">Name    :</asp:LinkButton>
                  </td>
                  <td style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
                    width: 55px; border-bottom: #cadefa thin solid; height: 6px">
                    <asp:TextBox Style="left: 15px" ID="NameTextBox" runat="server" Width="190px" OnTextChanged="NameTextBox_TextChanged"
                      AutoPostBack="True"></asp:TextBox>
                  </td>
                </tr>
                <tr>
                  <td style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
                    width: 27px; border-bottom: #cadefa thin solid; height: 20px">
                    <asp:LinkButton ID="CityLinkBtn" runat="server" Font-Size="Medium" Font-Names="Tahoma" OnClick="CityLinkBtn_Click">City    :</asp:LinkButton>
                  </td>
                  <td style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
                    width: 16px; border-bottom: #cadefa thin solid; height: 20px">
                    <asp:TextBox Style="left: 14px; top: 0px" ID="CityTextBox" runat="server" Width="190px"
                      OnTextChanged="CityTextBox_TextChanged" AutoPostBack="True"></asp:TextBox>
                  </td>
                  <td style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
                    width: 25px; border-bottom: #cadefa thin solid; height: 20px">
                    <asp:LinkButton ID="ZipCodeLinkBtn" runat="server" Font-Size="Medium" Font-Names="Tahoma" OnClick="ZipCodeLinkBtn_Click">Zipcode :</asp:LinkButton>
                  </td>
                  <td style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
                    width: 55px; border-bottom: #cadefa thin solid; height: 20px">
                    <asp:TextBox ID="ZipCodeTextBox" runat="server" Width="189px" OnTextChanged="ZipCodeTextBox_TextChanged"
                      AutoPostBack="True"></asp:TextBox>
                  </td>
                </tr>
                <tr>
                  <td style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
                    width: 27px; border-bottom: #cadefa thin solid; height: 24px">
                    <asp:LinkButton ID="Phone" runat="server" Font-Names="Tahoma" Font-Size="Medium" OnClick="Phone_Click">Phone :</asp:LinkButton></td>
                  <td style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
                    width: 16px; border-bottom: #cadefa thin solid; height: 24px">
                    <asp:TextBox ID="txtPhone" runat="server" AutoPostBack="True" Style="left: 14px; top: 0px" Width="190px" OnTextChanged="txtPhone_TextChanged"></asp:TextBox></td>
                  <td style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
                    width: 25px; border-bottom: #cadefa thin solid; height: 24px">
                    <asp:LinkButton ID="StoreLinkBtn" runat="server" Font-Names="Tahoma" Font-Size="Medium"
                      OnClick="StoreLinkBtn_Click">Store :</asp:LinkButton></td>
                  <td style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
                    width: 200px; border-bottom: #cadefa thin solid; height: 24px">
                    <asp:DropDownList ID="cboStoreType" runat="server" AutoPostBack="True" OnSelectedIndexChanged="cboStoreType_SelectedIndexChanged" Width="67px">
                      <asp:ListItem Selected="True">Main</asp:ListItem>
                      <asp:ListItem Enabled="False">Multi</asp:ListItem>
                      <asp:ListItem Enabled="False">Single</asp:ListItem>
                    </asp:DropDownList>&nbsp;&nbsp; &nbsp;
                    <asp:TextBox ID="StoreTextBox" runat="server" AutoPostBack="True" OnTextChanged="StoreTextBox_TextChanged"
                      Style="left: 14px; top: 0px" Width="101px" Enabled="False"></asp:TextBox></td>
                </tr>
              </tbody>
            </table>
          </td>
        </tr>
        <tr>
          <td style="height: 65px; text-align: right; width: 757px;" dir="ltr">
            &nbsp;<asp:Button Style="left: 309px; top: -9px" ID="SelectBtn" OnClick="SelectBtn_Click"
              runat="server" Width="83px" Text="Select"></asp:Button>
            <asp:Button ID="btnReset" runat="server" Style="left: 309px;
              top: -9px" Text="Reset" Width="79px" OnClick="btnReset_Click" />
          </td>
        </tr>
      </tbody>
    </table>

    <cc1:AutoCompleteExtender ID="AutoCompleteExtenderAccount" runat="server" EnableCaching="false"
      CompletionListCssClass="AutoCompleteExtenderAccount" MinimumPrefixLength="1" UseContextKey="True"
      TargetControlID="AccountNoTextBox" ServiceMethod="GetCompletionList" ServicePath="~/WebServices/AutoComplete/CustomerSelection/CustomerAccAutoComplete.asmx"
      BehaviorID="AutoCompleteEx1">
    </cc1:AutoCompleteExtender>
    <cc1:AutoCompleteExtender ID="AutoCompleteExtenderCity" runat="server" EnableCaching="false"
      CompletionListCssClass="AutoCompleteExtenderAccount" MinimumPrefixLength="1" UseContextKey="True"
      TargetControlID="CityTextBox" ServiceMethod="GetCompletionList" ServicePath="~/WebServices/AutoComplete/CustomerSelection/CustomerCityAutoComplete.asmx"
      BehaviorID="AutoCompleteEx2">
    </cc1:AutoCompleteExtender>
    <cc1:AutoCompleteExtender ID="AutoCompleteExtenderName" runat="server" EnableCaching="false" 
      CompletionListCssClass="AutoCompleteExtenderAccountRight" MinimumPrefixLength="1" UseContextKey="True"
      TargetControlID="NameTextBox" ServiceMethod="GetCompletionList" ServicePath="~/WebServices/AutoComplete/CustomerSelection/CustomerNameAutoComplete.asmx"
      BehaviorID="AutoCompleteEx3">
    </cc1:AutoCompleteExtender>
    <cc1:AutoCompleteExtender ID="AutoCompleteExtenderZipCode" runat="server" EnableCaching="false"
      CompletionListCssClass="AutoCompleteExtenderAccountRight" MinimumPrefixLength="1" UseContextKey="True"
      TargetControlID="ZipCodeTextBox" ServiceMethod="GetCompletionList" ServicePath="~/WebServices/AutoComplete/CustomerSelection/CustomerZipCodeAutoComplete.asmx"
      BehaviorID="AutoCompleteEx4">
    </cc1:AutoCompleteExtender><cc1:AutoCompleteExtender ID="AutoCompleteExtenderPhone" runat="server" EnableCaching="false"
      CompletionListCssClass="AutoCompleteExtenderAccount" MinimumPrefixLength="1" UseContextKey="True"
      TargetControlID="txtPhone" ServiceMethod="GetCompletionList" ServicePath="~/WebServices/AutoComplete/CustomerSelection/CustomerPhoneAutoComplete.asmx"
      BehaviorID="AutoCompleteEx5" >
    </cc1:AutoCompleteExtender><cc1:AutoCompleteExtender ID="AutoCompleteExtenderStore" runat="server" EnableCaching="false"
      CompletionListCssClass="AutoCompleteExtenderAccountRight" MinimumPrefixLength="1" UseContextKey="True"
      TargetControlID="StoreTextBox" ServiceMethod="GetCompletionList" ServicePath="~/WebServices/AutoComplete/CustomerSelection/CustomerStoreAutoComplete.asmx"
      BehaviorID="AutoCompleteEx6">
    </cc1:AutoCompleteExtender>
    </contenttemplate>
    </asp:UpdatePanel>
    &nbsp;&nbsp;
  </form>
</body>
</html>
