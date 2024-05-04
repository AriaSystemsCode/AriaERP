<%@ Page Language="C#" AutoEventWireup="true" CodeFile="Distribution.aspx.cs" Inherits="Distribution" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
    <title></title>
    <style type="text/css">
        .style1
        {
            width: 100%;
        }
    </style>
</head>
<body>
    <form id="form1" runat="server">
    <div>
        <table class="style1">
            <tr>
                <td>
                    &nbsp;
                </td>
                <td>
                                        <asp:Label ID="ErrorLabel" runat="server"></asp:Label>
                </td>
                <td>
                    &nbsp;
                </td>
            </tr>
            <tr>
                <td>
                    &nbsp;
                </td>
                <td>
                    <asp:MultiView ID="DisMultiView" runat="server" ActiveViewIndex="0">
                        <asp:View ID="FolderAndDBView" runat="server">
                            <table class="style1">
                                <tr>
                                    <td>
                                        <small><font face="Verdana">Customer code :</font></small>
                                    </td>
                                    <td>
                                        <asp:TextBox ID="CustCodeTextBox" runat="server"></asp:TextBox>
                                        <asp:RequiredFieldValidator ID="CustomerCodeRequiredFieldValidator" runat="server"
                                            ErrorMessage="*" ControlToValidate="CustCodeTextBox" Display="Dynamic"></asp:RequiredFieldValidator>
                                    </td>
                                </tr>
                                <tr>
                                    <td>
                                        <small><font face="Verdana">Customer Name :</font></small>
                                    </td>
                                    <td>
                                        <asp:TextBox ID="CustNameTextBox" runat="server"></asp:TextBox>
                                        <asp:RequiredFieldValidator ID="CustomerNameRequiredFieldValidator" runat="server"
                                            ErrorMessage="*" ControlToValidate="CustNameTextBox" Display="Dynamic"></asp:RequiredFieldValidator>
                                    </td>
                                </tr>
                                <tr>
                                    <td>
                                        <small><font face="Verdana">Number of companies :</font></small>
                                    </td>
                                    <td>
                                        <asp:TextBox ID="NumOfCompTextBox" runat="server"></asp:TextBox>
                                        <asp:RequiredFieldValidator ID="NumOfCompRequiredFieldValidator" runat="server" ErrorMessage="*"
                                            ControlToValidate="NumOfCompTextBox" Display="Dynamic"></asp:RequiredFieldValidator>
                                        <asp:RangeValidator ID="NumOfCompRangeValidator" runat="server" ControlToValidate="NumOfCompTextBox"
                                            Display="Dynamic" ErrorMessage="Please Must Enter Number only" Type="Integer"
                                            MaximumValue="1000" MinimumValue="0"></asp:RangeValidator>
                                    </td>
                                </tr>
                                <tr>
                                    <td>
                                        <small><font face="Verdana">Companies Code :</font></small>
                                    </td>
                                    <td>
                                        <asp:TextBox ID="CompsNameTextBox" runat="server"></asp:TextBox>
                                        <asp:RequiredFieldValidator ID="CompNameRequiredFieldValidator" runat="server" ErrorMessage="*"
                                            ControlToValidate="CompsNameTextBox" Display="Dynamic"></asp:RequiredFieldValidator>
                                        <asp:Label ID="Label1" runat="server" Text="Note: Please not forget Separate between companies by &quot;|&quot;"></asp:Label>
                                    </td>
                                </tr>
                                <tr>
                                    <td>
                                        <small><font face="Verdana">Companies Name :</font></small>
                                    </td>
                                    <td>
                                        <asp:TextBox ID="TextBox1" runat="server" Width="430px"></asp:TextBox>
                                        <asp:RequiredFieldValidator ID="RequiredFieldValidator1" runat="server" ErrorMessage="*"
                                            ControlToValidate="TextBox1" Display="Dynamic"></asp:RequiredFieldValidator>
                                        <asp:Label ID="Label2" runat="server" Text="Note: Please not forget Separate between companies by &quot;|&quot;"></asp:Label>
                                    </td>
                                </tr>
                                <tr>
                                    <td>
                                        <small><font face="Verdana">Customer phone number:</font></small>
                                    </td>
                                    <td>
                                        <asp:TextBox ID="CustPhoneTextBox" runat="server"></asp:TextBox>
                                        <asp:RequiredFieldValidator ID="CustmPhoneRequiredFieldValidator" runat="server"
                                            ErrorMessage="*" ControlToValidate="CustPhoneTextBox" Display="Dynamic"></asp:RequiredFieldValidator>
                                    </td>
                                </tr>
                                <tr>
                                    <td>
                                        <small><font face="Verdana">Number of Domain users:</font></small></td>
                                    <td>
                                        <asp:TextBox ID="NumDomainUsersTextBox" runat="server"></asp:TextBox>
                                        <asp:RequiredFieldValidator ID="NumDomainUserRequiredFieldValidator" 
                                            runat="server" ControlToValidate="NumDomainUsersTextBox" Display="Dynamic" 
                                            ErrorMessage="*"></asp:RequiredFieldValidator>
                                    </td>
                                </tr>
                                <tr>
                                    <td>
                                        Client Database Setup</td>
                                    <td>
                                        <table class="style1">
                                            <tr>
                                                <td>
                                                    Server name:</td>
                                                <td>
                                                    <asp:TextBox ID="ServerTextBox" runat="server"></asp:TextBox>
                                                    &nbsp;Note: if empty that server name will be the default.</td>
                                            </tr>
                                            <tr>
                                                <td>
                                                    Database Name:</td>
                                                <td>
                                                    <asp:TextBox ID="DBNameTextBox" runat="server"></asp:TextBox>
                                                    &nbsp;Note: if empty that database created call &lt;ClientID&gt;.Master</td>
                                            </tr>
                                            <tr>
                                                <td>
                                                    Username:</td>
                                                <td style="direction: ltr">
                                                    <asp:TextBox ID="UserNameTextBox" runat="server"></asp:TextBox>
                                                    &nbsp;Note: if empty that username created as user for company database.</td>
                                            </tr>
                                            <tr>
                                                <td>
                                                    Password:</td>
                                                <td style="direction: ltr">
                                                    <asp:TextBox ID="PassTextBox" runat="server" Height="22px" TextMode="Password" 
                                                        Width="128px"></asp:TextBox>
                                                    &nbsp;Note: if empty that password created as password for user company database.</td>
                                            </tr>
                                        </table>
                                    </td>
                                </tr>
                                <tr>
                                    <td>
                                    </td>
                                    <td>
                                        &nbsp;</td>
                                </tr>
                                <tr>
                                    <td>
                                        &nbsp; </td>
                                    <td>
                                        <asp:Button ID="ExecuteButton" runat="server" OnClick="ExecuteButton_Click" 
                                            Text="Execute" />
                                    </td>
                                </tr>
                                <tr>
                                    <td colspan="2">
                                        &nbsp;</td>
                                </tr>
                            </table>
                        </asp:View>
                        <asp:View ID="SydapplView" runat="server">
                            <table class="style1">
                                <tr>
                                    <td>
                                        &nbsp;</td>
                                    <td>
                                        &nbsp;</td>
                                </tr>
                                <tr>
                                    <td>
                                        <small><font face="Verdana">Upload Activation Key:</font></small></td>
                                    <td>
                                        <asp:TextBox ID="ActKeyTextBox" 
                                            runat="server" Width="221px"></asp:TextBox>
                                    </td>
                                </tr>
                                <tr>
                                    <td>
                                        <small><font face="Verdana">Modules:</font></small></td>
                                    <td>
                                        <asp:CheckBoxList ID="SydApplCheckBoxList" runat="server">
                                        </asp:CheckBoxList>
                                    </td>
                                </tr>
                                <tr>
                                    <td>
                                        &nbsp;</td>
                                    <td>
                                        <asp:Button ID="SydappButton" runat="server" onclick="SydappButton_Click" 
                                            Text="Execute" />
                                    </td>
                                </tr>
                            </table>
                        </asp:View>
                        <asp:View ID="AriaOnlineView" runat="server">
                            <table class="style1">
                                <tr>
                                    <td>
                                        &nbsp;</td>
                                    <td>
                                        &nbsp;</td>
                                </tr>
                                <tr>
                                    <td>
                                        <small><font face="Verdana">Applications:</font></small></td>
                                    <td>
                                        <asp:CheckBoxList ID="AppCheckBoxList" runat="server">
                                        </asp:CheckBoxList>
                                    </td>
                                </tr>
                                <tr>
                                    <td>
                                        &nbsp;</td>
                                    <td>
                                        <asp:Button ID="ApplButton" runat="server" onclick="ApplButton_Click" 
                                            Text="Execute" />
                                    </td>
                                </tr>
                                <tr>
                                    <td>
                                        &nbsp;</td>
                                    <td>
                                        &nbsp;</td>
                                </tr>
                            </table>
                        </asp:View>
                        <asp:View ID="FinishView" runat="server">
                            Finish
                    </asp:View>
                    </asp:MultiView>
                </td>
                <td>
                    &nbsp;
                </td>
            </tr>
            <tr>
                <td>
                    &nbsp;
                </td>
                <td>
                    &nbsp;
                    
                </td>
                <td>
                    &nbsp;
                </td>
            </tr>
        </table>
    </div>
    </form>
</body>
</html>
