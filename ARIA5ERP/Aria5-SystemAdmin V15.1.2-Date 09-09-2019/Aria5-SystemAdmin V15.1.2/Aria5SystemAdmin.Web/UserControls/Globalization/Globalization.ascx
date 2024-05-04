<%@ Control Language="C#" AutoEventWireup="true" CodeBehind="Globalization.ascx.cs"
    Inherits="AriaDevExpress.Web.UserControls.Globalization.Globalization" %>
<table>
    <tr>
        <td valign="top">
            Target Language :
        </td>
        <td>
            <dx:ASPxComboBox ID="ddlLanguages" runat="server" EnableClientSideAPI="True" OnInit="ddlLanguages_Init"
                DataSourceID="LanguagesXpoDataSource" AutoPostBack="True" TextField="cShort_Des"
                ValueField="cLang_ID" OnSelectedIndexChanged="ddlLanguages_SelectedIndexChanged">
                <Items>
                    <dx:ListEditItem Text="(Select Language)" Value="0" />
                </Items>
            </dx:ASPxComboBox>
            <dx:XpoDataSource ID="LanguagesXpoDataSource" runat="server" EnableViewState="true"
                Criteria="cLang_ID != 'EN'" TypeName="AriaDevExpress.Module.BusinessObjects.Globalization.SycLang">
            </dx:XpoDataSource>
        </td>
    </tr>
</table>
<br />
<dx:ASPxPageControl ID="MainContentASPxPageControl" runat="server" ActiveTabIndex="0">
    <TabPages>
        <dx:TabPage Text="Object List">
            <ContentCollection>
                <dx:ContentControl ID="ContentControl1" runat="server">
                    <table width="100%">
                        <tr>
                            <td width="100%">
                                <dx:ASPxLabel ID="lblFirstTabStatus" runat="server">
                                </dx:ASPxLabel>
                            </td>
                        </tr>
                    </table>
                    <dx:ASPxGridView ID="ObjectsListASPxGridView" runat="server" EnableCallBacks="false"
                        DataSourceID="ObjectGridXpoDataSource" AutoGenerateColumns="False" KeyFieldName="Obj_Key"
                        OnCustomButtonCallback="ObjectsListASPxGridView_CustomButtonCallback">
                        <SettingsPager PageSize="30">
                        </SettingsPager>
                        <SettingsBehavior AllowSelectByRowClick="true" AllowSelectSingleRowOnly="true" AllowSort="true" />
                        <Settings ShowFilterBar="Auto" ShowFilterRow="true" ShowFilterRowMenu="true" />
                        <Columns>
                            <dx:GridViewDataTextColumn FieldName="cObject" Caption="Object Name" ShowInCustomizationForm="True"
                                VisibleIndex="2">
                            </dx:GridViewDataTextColumn>
                            <dx:GridViewDataTextColumn FieldName="ModifyUser" ShowInCustomizationForm="True"
                                VisibleIndex="5">
                            </dx:GridViewDataTextColumn>
                            <dx:GridViewDataDateColumn FieldName="ModifyDateTime" ShowInCustomizationForm="True"
                                VisibleIndex="6">
                            </dx:GridViewDataDateColumn>
                            <dx:GridViewCommandColumn Caption="Edit" Name="EditButton" ShowInCustomizationForm="True"
                                VisibleIndex="8">
                                <CustomButtons>
                                    <dx:GridViewCommandColumnCustomButton ID="EditButton" Text="Edit" Visibility="AllDataRows">
                                    </dx:GridViewCommandColumnCustomButton>
                                </CustomButtons>
                                <CellStyle HorizontalAlign="Left">
                                </CellStyle>
                            </dx:GridViewCommandColumn>
                        </Columns>
                    </dx:ASPxGridView>
                    <dx:XpoDataSource ID="ObjectGridXpoDataSource" runat="server" TypeName="AriaDevExpress.Module.BusinessObjects.Globalization.SydLangObject">
                    </dx:XpoDataSource>
                </dx:ContentControl>
            </ContentCollection>
        </dx:TabPage>
        <dx:TabPage ToolTip="Please Select Object First" Name="TabObjectTranslation">
            <ContentCollection>
                <dx:ContentControl ID="ContentControl2" runat="server">
                    <div style="float: right; width: 100%">
                        <table width="100%" style="height: 30px;">
                            <tr width="100%">
                                <td align="left">
                                    <dx:ASPxLabel ID="lblStatus" Font-Bold="true" runat="server" />
                                    <dxrp:ASPxPanel ID="pnlTooLong" Font-Bold="true" runat="server" ForeColor="Red" Visible="false">
                                        <PanelCollection>
                                            <dxrp:PanelContent>
                                                <table>
                                                    <tr>
                                                        <td>
                                                            One or more translated texts are too long. Do you want to save anyway ?
                                                        </td>
                                                        <td>
                                                            <dx:ASPxButton runat="server" ID="btn_SaveAnyWay" OnClick="btn_SaveAnyWay_OnClick"
                                                                Text="Ok" />
                                                        </td>
                                                    </tr>
                                                </table>
                                            </dxrp:PanelContent>
                                        </PanelCollection>
                                    </dxrp:ASPxPanel>
                                </td>
                                <td align="right" width="158px">
                                    <dx:ASPxButton ID="SuggestEmptyASPxButton" runat="server" OnClick="SuggestEmptyASPxButton_Click"
                                        Text="Suggest For Empty Only">
                                    </dx:ASPxButton>
                                </td>
                                <td align="right" width="110px">
                                    <dx:ASPxButton ID="SuggestAllASPxButton" runat="server" OnClick="SuggestAllASPxButton_Click"
                                        Text="Suggest For All">
                                    </dx:ASPxButton>
                                </td>
                                <td align="right" width="100px">
                                    <dx:ASPxButton ID="CheckLengthASPxButton" runat="server" OnClick="CheckLengthASPxButton_Click"
                                        Text="Check Length">
                                    </dx:ASPxButton>
                                </td>
                                <td align="right" width="50px">
                                    <dx:ASPxButton ID="SaveASPxButton" runat="server" Text="Save" OnClick="SaveASPxButton_Click">
                                    </dx:ASPxButton>
                                </td>
                                <td align="right" width="50px">
                                    <dx:ASPxButton ID="CancelASPxButton" runat="server" Text="Cancel" OnClick="CancelASPxButton_Click">
                                    </dx:ASPxButton>
                                </td>
                            </tr>
                        </table>
                    </div>
                    <br />
                    <br />
                    <dx:ASPxGridView ID="TranslateASPxGridView" Paddings-PaddingTop="0px" runat="server"
                        KeyFieldName="EnStr_Key" ClientInstanceName="TranslateASPxGridView" AutoGenerateColumns="False"
                        DataSourceID="TranslationsGridSqlDatasource">
                        <SettingsPager PageSize="30">
                        </SettingsPager>
                        <Columns>
                            <dx:GridViewDataTextColumn Caption="Object Name" FieldName="cObject" ShowInCustomizationForm="True"
                                VisibleIndex="1" Visible="False">
                            </dx:GridViewDataTextColumn>
                            <%--                            <dx:GridViewDataTextColumn Caption="Control Name" FieldName="OriginalControlName"
                                ShowInCustomizationForm="True" VisibleIndex="2">
                            </dx:GridViewDataTextColumn>
                            <dx:GridViewDataTextColumn Caption="Property Name" FieldName="cProperty" ShowInCustomizationForm="True"
                                VisibleIndex="3">
                            </dx:GridViewDataTextColumn>--%>
                            <dx:GridViewDataTextColumn Caption="Original Text" FieldName="OriginalText" ShowInCustomizationForm="True"
                                VisibleIndex="4">
                            </dx:GridViewDataTextColumn>
                            <dx:GridViewDataTextColumn Caption="Full Text" FieldName="FullText" ShowInCustomizationForm="True"
                                VisibleIndex="5">
                            </dx:GridViewDataTextColumn>
                            <dx:GridViewDataTextColumn Name="LangText" Caption="Target Language Text" FieldName="LangText"
                                ShowInCustomizationForm="True" VisibleIndex="6">
                                <DataItemTemplate>
                                    <dx:ASPxTextBox ID="TranslationTxtBox" RightToLeft='<%# GetRightToLeftStatus()%>'
                                        Width="100%" runat="server" BackColor='<%# GetLangTextBoxBackColor()%>' Value='<%# GetLangText()%>'>
                                    </dx:ASPxTextBox>
                                </DataItemTemplate>
                            </dx:GridViewDataTextColumn>
                            <dx:GridViewDataColumn FieldName="DontBing" VisibleIndex="0" Visible="false" Name="DontBing">
                                <EditFormSettings Visible="False" />
                            </dx:GridViewDataColumn>
                        </Columns>
                        <Settings ShowGroupPanel="True" />
                        <Paddings PaddingTop="0px"></Paddings>
                    </dx:ASPxGridView>
                    <asp:SqlDataSource ID="TranslationsGridSqlDatasource" runat="server" ProviderName="System.Data.SqlClient"
                        SelectCommand="SELECT SydLangObject.cObject, SydEnString.FullText,SydEnString.OriginalText, SydEnString.OriginalControlName, SydEnString.cProperty,SydEnString.EnStr_Key,SydEnString.DontBing,
                          (SELECT     LangText
                             FROM         SydString
                             WHERE     (cLang_ID = @LanguageID) AND (SydString.EnStr_Key = SydEnString.EnStr_Key)) AS LangText
FROM         SydLangObject INNER JOIN
                      SydEnString ON SydLangObject.Obj_Key = SydEnString.Obj_Key
WHERE     (SydLangObject.Obj_Key = @ObjectID)" InsertCommand="INSERT INTO SydString(EnStr_Key, cLang_ID, LangText) VALUES (@EnStr_Key, @LanguageID, @LangText)">
                        <InsertParameters>
                            <asp:Parameter Name="EnStr_Key" />
                            <asp:ControlParameter ControlID="ddlLanguages" Name="LanguageID" PropertyName="Value" />
                            <asp:Parameter Name="LangText" />
                        </InsertParameters>
                        <SelectParameters>
                            <asp:ControlParameter ControlID="ddlLanguages" Name="LanguageID" PropertyName="Value" />
                            <asp:SessionParameter Name="ObjectID" SessionField="ObjectID" />
                        </SelectParameters>
                    </asp:SqlDataSource>
                    <asp:SqlDataSource ID="SydStringSqlDatasource" runat="server" ProviderName="System.Data.SqlClient"
                        SelectCommand="SELECT * FROM SydString where EnStr_Key = @EnStr_Key and  cLang_ID = @cLang_ID"
                        InsertCommand="INSERT INTO SydString(EnStr_Key, cLang_ID, LangText) VALUES (@EnStr_Key, @cLang_ID, @LangText)"
                        UpdateCommand="UPDATE SydString set LangText =@LangText  where EnStr_Key = @EnStr_Key and  cLang_ID = @cLang_ID">
                        <SelectParameters>
                            <asp:Parameter Name="EnStr_Key" />
                            <asp:ControlParameter ControlID="ddlLanguages" Name="cLang_ID" PropertyName="Value" />
                        </SelectParameters>
                        <InsertParameters>
                            <asp:Parameter Name="EnStr_Key" />
                            <asp:ControlParameter ControlID="ddlLanguages" Name="cLang_ID" PropertyName="Value" />
                            <asp:Parameter Name="LangText" ConvertEmptyStringToNull="false" />
                        </InsertParameters>
                        <UpdateParameters>
                            <asp:Parameter Name="EnStr_Key" />
                            <asp:ControlParameter ControlID="ddlLanguages" Name="cLang_ID" PropertyName="Value" />
                            <asp:Parameter Name="LangText" ConvertEmptyStringToNull="false" />
                        </UpdateParameters>
                    </asp:SqlDataSource>
                </dx:ContentControl>
            </ContentCollection>
        </dx:TabPage>
    </TabPages>
</dx:ASPxPageControl>
