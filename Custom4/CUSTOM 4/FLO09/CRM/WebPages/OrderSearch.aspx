<%@ Page Language="C#" AutoEventWireup="true" CodeFile="OrderSearch.aspx.cs"
  Inherits="OrderSearch" Title="Untitled Page" EnableEventValidation = "false"  %>

<%@ Register Assembly="AjaxControlToolkit" Namespace="AjaxControlToolkit" TagPrefix="cc1" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head id="Head" runat="server">
  <title></title>
  <link href="../StyleSheet.css" rel="stylesheet" type="text/css" />
  <script language ="jscript" type ="text/jscript">
  function Loading(loading)
  {
    window.document.getElementById("LoadingControl").style.top = 20;
    window.document.getElementById("LoadingControl").value = loading;
  }
  </script>
</head>
<body>
  <form id="Form" runat="server">
    <cc1:ToolkitScriptManager ID="ToolkitScriptManager1" runat="server">
    </cc1:ToolkitScriptManager>
    
    <asp:UpdatePanel ID="UpdatePanel1" runat="server">
    <contenttemplate>
    <table>
      <tbody>
        <tr>
          <td style="width: 787px">
            <asp:Panel Style="left: 0px; width: 789px; position: relative; top: 0px; background-color: #527ad5"
              ID="Panel1" runat="server" Width="125px" Height="39px">
              <asp:Label ID="Label1" runat="server" Font-Bold="True" Font-Names="Tahoma" Font-Size="Large"
                Height="18px" Style="left: 7px; position: relative; top: 8px" Text="Modify Order"
                Width="182px"></asp:Label>
            </asp:Panel>
            <asp:Panel Style="border-top: #aacbee 2px solid; left: 0px; width: 789px; position: relative;
              top: 0px; background-color: #527ad5" ID="Panel2" runat="server" Width="425px" Height="15px">
            </asp:Panel>
            <table style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
              width: 789px; border-bottom: #cadefa thin solid; position: relative; top: 0px;
              background-color: #eff3ff">
              <tbody>
                <tr>
                  <td style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
                    width: 53px; border-bottom: #cadefa thin solid; height: 7px">
                    <asp:LinkButton ID="AcctLinkBtn" runat="server" Font-Size="Medium" Font-Names="Tahoma" OnClick="AcctLinkBtn_Click">Acct#    :</asp:LinkButton></td>
                  <td style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
                    width: 86px; border-bottom: #cadefa thin solid; height: 7px">
                            <asp:TextBox ID="AccountNoTextBox" runat="server" Width="191px" OnTextChanged="AccountNoTextBox_TextChanged"
                              AutoPostBack="True" Height="16px"></asp:TextBox></td>
                  <td style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
                    width: 52px; border-bottom: #cadefa thin solid; height: 7px">
                    <asp:LinkButton ID="NameLinkBtn" runat="server" Font-Size="Medium" Font-Names="Tahoma" OnClick="NameLinkBtn_Click">Name    :</asp:LinkButton>
                  </td>
                  <td style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
                    width: 63px; border-bottom: #cadefa thin solid; height: 7px">
                    <asp:TextBox Style="left: 15px" ID="NameTextBox" runat="server" Width="190px" OnTextChanged="NameTextBox_TextChanged"
                      AutoPostBack="True"></asp:TextBox></td>
                </tr>
                <tr>
                  <td style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
                    width: 53px; border-bottom: #cadefa thin solid; height: 32px">
                    <asp:LinkButton ID="CityLinkBtn" runat="server" Font-Size="Medium" Font-Names="Tahoma" OnClick="CityLinkBtn_Click">City    :</asp:LinkButton>
                  </td>
                  <td style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
                    width: 86px; border-bottom: #cadefa thin solid; height: 32px">
                    <asp:TextBox Style="left: 14px; top: 0px" ID="CityTextBox" runat="server" Width="190px"
                      OnTextChanged="CityTextBox_TextChanged" AutoPostBack="True"></asp:TextBox>
                  </td>
                  <td style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
                    width: 52px; border-bottom: #cadefa thin solid; height: 32px">
                    <asp:LinkButton ID="ZipCodeLinkBtn" runat="server" Font-Size="Medium" Font-Names="Tahoma" OnClick="ZipCodeLinkBtn_Click">Zipcode :</asp:LinkButton>
                  </td>
                  <td style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
                    width: 63px; border-bottom: #cadefa thin solid; height: 32px">
                    <asp:TextBox ID="ZipCodeTextBox" runat="server" Width="190px" OnTextChanged="ZipCodeTextBox_TextChanged"
                      AutoPostBack="True"></asp:TextBox>
                  </td>
                </tr>
                <tr>
                  <td style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
                    width: 53px; border-bottom: #cadefa thin solid; height: 32px">
                    <asp:LinkButton ID="Phone" runat="server" Font-Names="Tahoma" Font-Size="Medium"
                      OnClick="Phone_Click">Phone :</asp:LinkButton></td>
                  <td style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
                    width: 86px; border-bottom: #cadefa thin solid; height: 32px">
                    <asp:TextBox ID="txtPhone" runat="server" AutoPostBack="True" OnTextChanged="txtPhone_TextChanged"
                      Style="left: 14px; top: 0px" Width="190px"></asp:TextBox></td>
                  <td style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
                    width: 52px; border-bottom: #cadefa thin solid; height: 32px">
                    <asp:LinkButton ID="StoreLinkBtn" runat="server" Font-Names="Tahoma" Font-Size="Medium"
                      OnClick="StoreLinkBtn_Click">Store :</asp:LinkButton></td>
                  <td style="border-right: #cadefa thin solid; border-top: #cadefa thin solid; border-left: #cadefa thin solid;
                    width: 200px; border-bottom: #cadefa thin solid; height: 32px" dir="ltr">
                    <asp:DropDownList ID="cboStoreType" runat="server" AutoPostBack="True" OnSelectedIndexChanged="cboStoreType_SelectedIndexChanged"
                      Width="67px">
                      <asp:ListItem Selected="True">Main</asp:ListItem>
                      <asp:ListItem Enabled="False">Multi</asp:ListItem>
                      <asp:ListItem Enabled="False">Single</asp:ListItem>
                    </asp:DropDownList>
                    &nbsp; &nbsp;&nbsp;
                    <asp:TextBox ID="StoreTextBox" runat="server" AutoPostBack="True" Enabled="False"
                      OnTextChanged="StoreTextBox_TextChanged" Style="left: 14px; top: 0px" Width="101px"></asp:TextBox></td>
                </tr>
              </tbody>
            </table>
          </td>
        </tr>
      <tr>
        <td style="font-weight: bold; color: white; background-color: #527ad5 ; width: 787px; font-size: 0.8em;" dir="ltr">Enter your searching criteria to select the order(s) you want to modify or just click search button for all:
        </td>
      </tr>
      <tr>
        <td style="height: 141px; width: 787px;">
          <table  style ="background-color: #eff3ff">
          <tbody>
            <tr>
            <td style="width: 147px">Order#
            </td>
            <td style="width: 189px">
              &nbsp;<asp:TextBox ID="txtOrderFrom" runat="server" MaxLength="6"></asp:TextBox></td>
            <td> To
            </td>
            <td style="width: 452px">
              <asp:TextBox ID="txtOrderTo" runat="server" MaxLength="6"></asp:TextBox>
            </td>
            </tr>
            <tr>
            <td style="width: 147px; height: 26px">
              Enter Date</td>
            <td style="width: 189px; height: 26px;">
              &nbsp;<asp:TextBox ID="txtEnterDateFrom" runat="server" OnTextChanged="txtEnterDateFrom_TextChanged" AutoPostBack="True"></asp:TextBox>
              <asp:ImageButton ID="imgEnterDateFrom" runat="server" ImageUrl="~/resources/images/Calendar_scheduleHS.png" /></td>
            <td style="height: 26px"> To
            </td>
            <td style="width: 452px; height: 26px;">
              <asp:TextBox ID="txtEnterDateTo" runat="server" AutoPostBack="True" OnTextChanged="txtEnterDateTo_TextChanged"></asp:TextBox>
              <asp:ImageButton ID="imgEnterDateTo" runat="server" ImageUrl="~/resources/images/Calendar_scheduleHS.png" /></td>
            </tr>
            <tr>
            <td style="width: 147px">
              Start Shipping Date</td>
            <td style="width: 189px">
              &nbsp;<asp:TextBox ID="txtStartShipDateFrom" runat="server" AutoPostBack="True" OnTextChanged="txtStartShipDateFrom_TextChanged"></asp:TextBox>
              <asp:ImageButton ID="imgStartShipDateFrom" runat="server" ImageUrl="~/resources/images/Calendar_scheduleHS.png" /></td>
            <td> To
            </td>
            <td style="width: 452px">
              <asp:TextBox ID="txtStartShipDateTo" runat="server" AutoPostBack="True" OnTextChanged="txtStartShipDateTo_TextChanged"></asp:TextBox>
              <asp:ImageButton ID="imgStartShipDateTo" runat="server" ImageUrl="~/resources/images/Calendar_scheduleHS.png" /></td>
            </tr>
            <tr>
            <td style="width: 147px">
              Expected Ship Date</td>
            <td style="width: 189px">
              &nbsp;<asp:TextBox ID="txtExpectedShipDateFrom" runat="server" AutoPostBack="True" OnTextChanged="txtExpectedShipDateFrom_TextChanged"></asp:TextBox>
              <asp:ImageButton ID="imgExpectedShipDateFrom" runat="server" ImageUrl="~/resources/images/Calendar_scheduleHS.png" /></td>
            <td> To
            </td>
            <td style="width: 452px">
              <asp:TextBox ID="txtExpectedShipDateTo" runat="server" AutoPostBack="True" OnTextChanged="txtExpectedShipDateTo_TextChanged"></asp:TextBox>
              <asp:ImageButton ID="imgExpectedShipDateTo" runat="server" ImageUrl="~/resources/images/Calendar_scheduleHS.png" /></td>
            </tr>
            <tr>
            <td style="width: 147px">
              Season</td>
            <td style="width: 189px">
              &nbsp;<asp:DropDownList ID="cboSeason" runat="server" Width="154px">
              </asp:DropDownList></td>
            <td> &nbsp;</td>
            <td style="width: 452px">
              &nbsp;</td>
            </tr>
            </tbody>
          </table>
          <input type = "text" ID="LoadingControl"
            Style="left: 703px; color: white; position: absolute; top: -100px;
            background-color: HotPink; z-index: 1; width: 94px; border-top-style: none; border-right-style: none; border-left-style: none; height: 19px; border-bottom-style: none;" Visible="False" value="    Loading..." />
          &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
          &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
          &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
          &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
          &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
          &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
          &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp;
          <asp:Button ID="btnSearch" runat="server" Text="Search" OnClick="btnSearch_Click" />
          <asp:Button ID="btnReset" runat="server" Text="Reset" OnClick="btnReset_Click" /></td>
      </tr>
      </tbody>
    </table>

    <cc1:AutoCompleteExtender ID="AutoCompleteExtenderAccount" runat="server" EnableCaching="false"
      CompletionListCssClass="AutoCompleteExtenderAccount" MinimumPrefixLength="1" UseContextKey="True"
      TargetControlID="AccountNoTextBox" ServiceMethod="GetCompletionList" ServicePath="~/WebServices/AutoComplete/CustomerSelection/CustomerAccAutoComplete.asmx"
      BehaviorID="AutoCompleteEx1" DelimiterCharacters ="|">
    </cc1:AutoCompleteExtender>
    <cc1:AutoCompleteExtender ID="AutoCompleteExtenderCity" runat="server" EnableCaching="false"
      CompletionListCssClass="AutoCompleteExtenderAccount" MinimumPrefixLength="1" UseContextKey="True"
      TargetControlID="CityTextBox" ServiceMethod="GetCompletionList" ServicePath="~/WebServices/AutoComplete/CustomerSelection/CustomerCityAutoComplete.asmx"
      BehaviorID="AutoCompleteEx2">
    </cc1:AutoCompleteExtender>
    <cc1:AutoCompleteExtender ID="AutoCompleteExtenderName" runat="server" EnableCaching="false"
      CompletionListCssClass="AutoCompleteExtenderAccount" MinimumPrefixLength="1" UseContextKey="True"
      TargetControlID="NameTextBox" ServiceMethod="GetCompletionList" ServicePath="~/WebServices/AutoComplete/CustomerSelection/CustomerNameAutoComplete.asmx"
      BehaviorID="AutoCompleteEx3">
    </cc1:AutoCompleteExtender>
    <cc1:AutoCompleteExtender ID="AutoCompleteExtenderZipCode" runat="server" EnableCaching="false"
      CompletionListCssClass="AutoCompleteExtenderAccount" MinimumPrefixLength="1" UseContextKey="True"
      TargetControlID="ZipCodeTextBox" ServiceMethod="GetCompletionList" ServicePath="~/WebServices/AutoComplete/CustomerSelection/CustomerZipCodeAutoComplete.asmx"
      BehaviorID="AutoCompleteEx4">
    </cc1:AutoCompleteExtender>
      <cc1:AutoCompleteExtender ID="AutoCompleteExtenderPhone" runat="server" BehaviorID="AutoCompleteEx5"
        CompletionListCssClass="AutoCompleteExtenderAccount" EnableCaching="false" MinimumPrefixLength="1"
        ServiceMethod="GetCompletionList" ServicePath="~/WebServices/AutoComplete/CustomerSelection/CustomerPhoneAutoComplete.asmx"
        TargetControlID="txtPhone" UseContextKey="True">
      </cc1:AutoCompleteExtender>
      <cc1:AutoCompleteExtender ID="AutoCompleteExtenderStore" runat="server" BehaviorID="AutoCompleteEx6"
        CompletionListCssClass="AutoCompleteExtenderAccount" EnableCaching="false" MinimumPrefixLength="1"
        ServiceMethod="GetCompletionList" ServicePath="~/WebServices/AutoComplete/CustomerSelection/CustomerStoreAutoComplete.asmx"
        TargetControlID="StoreTextBox" UseContextKey="True">
      </cc1:AutoCompleteExtender>
    <br />
    <cc1:CalendarExtender ID="CalendarExtenderEnterDateFrom" runat="server" Format="M/d/yyyy" PopupButtonID="imgEnterDateFrom"
      TargetControlID="txtEnterDateFrom">
    </cc1:CalendarExtender>
    <cc1:CalendarExtender ID="CalendarExtenderEnterDateTo" runat="server" Format="M/d/yyyy" PopupButtonID="imgEnterDateTo"
      TargetControlID="txtEnterDateTo">
    </cc1:CalendarExtender>
    <br />
    <cc1:CalendarExtender ID="CalendarExtenderStartShipDateFrom" runat="server" Format="M/d/yyyy" PopupButtonID="imgStartShipDateFrom"
      TargetControlID="txtStartShipDateFrom">
    </cc1:CalendarExtender>
    <cc1:CalendarExtender ID="CalendarExtenderStartShipDateTo" runat="server" Format="M/d/yyyy" PopupButtonID="imgStartShipDateTo"
      TargetControlID="txtStartShipDateTo">
    </cc1:CalendarExtender>
    &nbsp;&nbsp;<br />
    
    <cc1:CalendarExtender ID="CalendarExtenderExpectedShipDateFrom" runat="server" Format="M/d/yyyy" PopupButtonID="imgExpectedShipDateFrom"
      TargetControlID="txtExpectedShipDateFrom">
    </cc1:CalendarExtender>
    <cc1:CalendarExtender ID="CalendarExtenderExpectedShipDateTo" runat="server" Format="M/d/yyyy" PopupButtonID="imgExpectedShipDateTo"
      TargetControlID="txtExpectedShipDateTo">
    </cc1:CalendarExtender>
    </contenttemplate>
    </asp:UpdatePanel>
  </form>
</body>
</html>
