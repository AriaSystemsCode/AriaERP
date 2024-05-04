<%@ Control Language="C#" AutoEventWireup="true" CodeBehind="Configration.ascx.cs" Inherits="Aria5SystemAdmin.Web.UserControls.ClientManager.Configration" %>
<style type="text/css">
    .SqlTitle
    {
        font-size: larger;
    }

    .mainContent input
    {
        width: 300px;
    }

    .AlignButtonCenter
    {
        margin-left: auto;
        margin-right: auto;
    }
</style>

<div class="mainContent">
    <dx:ASPxPageControl ID="MainContentASPxPageControl" runat="server" ClientInstanceName="Tabs" ClientVisible="true" EnableClientSideAPI="true" Width="100%" ActiveTabIndex="0">
        <TabPages>
            <dx:TabPage Text="Paths">
                <ContentCollection>
                    <dx:ContentControl>
                        <table id="PathsInfo" cellspacing="15">
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="Aria Source Path" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox ID="txtAriaSourcePath" runat="server" />
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="Aria Master Source Path" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox ID="txtAriaMasterPath" runat="server"></dx:ASPxTextBox>
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel ID="ASPxLabel1" runat="server" Text="Aria Shared Server Name" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox ID="txtAriaSharedServerName" runat="server"></dx:ASPxTextBox>
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="Aria Shared Directory Path" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox ID="txtAriaSharedPath" runat="server"></dx:ASPxTextBox>
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="Aria Shared Directory Local Path" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox ID="txtAriaSharedLocalPath" runat="server"></dx:ASPxTextBox>
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="Aria SQL DBs Directory Path" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox ID="txtAriaSQLDBsPath" runat="server"></dx:ASPxTextBox>
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="Aria4XP Mapped Path" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox ID="txtAriaMappedPath" runat="server"></dx:ASPxTextBox>
                                </td>
                            </tr>
                        </table>
                    </dx:ContentControl>
                </ContentCollection>
            </dx:TabPage>
            <dx:TabPage Text="SQL Info">
                <ContentCollection>
                    <dx:ContentControl>
                        <table id="SQLInfo" cellspacing="15">
                            <tr>
                                <td colspan="2">
                                    <dx:ASPxLabel runat="server" Text="Aria4 SQL" CssClass="SqlTitle" />
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="Server Name" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox runat="server" ClientInstanceName="txtServerNameAria4" ID="txtServerNameAria4" />
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="User Name" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox runat="server" ClientInstanceName="txtUserNameAria4" ID="txtUserNameAria4" />
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="Password" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox runat="server" ClientInstanceName="txtPasswordAria4" ID="txtPasswordAria4" />
                                </td>
                            </tr>
                            <tr>
                                <td colspan="2"></td>
                            </tr>
                            <tr>
                                <td></td>
                            </tr>
                            <tr>
                                <td colspan="2">
                                    <dx:ASPxLabel runat="server" Text="System Master SQL" CssClass="SqlTitle" />
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="Server Name" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox runat="server" ClientInstanceName="txtServerNameSystemMaster" ID="txtServerNameSystemMaster" />
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="DataBase Name" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox runat="server" ClientInstanceName="txtDataBaseNameSystemMaster" ID="txtDataBaseNameSystemMaster" />
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="User Name" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox runat="server" ClientInstanceName="txtUserNameSystemMaster" ID="txtUserNameSystemMaster" />
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="Password" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox runat="server" ClientInstanceName="txtPasswordSystemMaster" ID="txtPasswordSystemMaster" />
                                </td>
                            </tr>
                            <tr>
                                <td></td>
                            </tr>
                            <tr>
                                <td colspan="2">
                                    <dx:ASPxLabel runat="server" Text="Client Master SQL" CssClass="SqlTitle" />
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="Server Name" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox runat="server" ClientInstanceName="txtServerNameClientMaster" ID="txtServerNameClientMaster" />
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="User Name" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox runat="server" ClientInstanceName="txtUserNameClientMaster" ID="txtUserNameClientMaster" />
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="Password" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox runat="server" ClientInstanceName="txtPasswordClientMaster" ID="txtPasswordClientMaster" />
                                </td>
                            </tr>
                        </table>
                    </dx:ContentControl>
                </ContentCollection>
            </dx:TabPage>
            <dx:TabPage Text="Domain Info">
                <ContentCollection>
                    <dx:ContentControl>
                        <table id="DomainInfo" cellspacing="15">
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="Domain Name" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox runat="server" ClientInstanceName="txtDomainName" ID="txtDomainName" />
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="Admin User Name" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox runat="server" ClientInstanceName="txtDomainAdminUserName" ID="txtDomainAdminUserName" />
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="Admin Password" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox runat="server" ClientInstanceName="txtDomainAdminUserPassword" ID="txtDomainAdminUserPassword" />
                                </td>
                            </tr>
                        </table>
                    </dx:ContentControl>
                </ContentCollection>
            </dx:TabPage>
        </TabPages>
    </dx:ASPxPageControl>
    <br />
    <dx:ASPxButton HorizontalAlign="Center" CssClass="AlignButtonCenter" runat="server" Text="Save" ID="btnSave" Width="80px" OnClick="btnSave_Click" UseSubmitBehavior="false"/>
</div>
<br />
<dx:ASPxLabel ID="lblStatus" Style="padding-left: 50px;" Font-Bold="true" Font-Size="12px" runat="server" Width="100%" />
