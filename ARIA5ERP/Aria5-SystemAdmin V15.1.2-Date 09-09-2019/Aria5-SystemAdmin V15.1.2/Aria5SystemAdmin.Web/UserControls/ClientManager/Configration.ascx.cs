using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;

namespace Aria5SystemAdmin.Web.UserControls.ClientManager
{
    public partial class Configration : System.Web.UI.UserControl
    {
        protected void Page_Load(object sender, EventArgs e)
        {
            
                string et = Request.Form["__EVENTTARGET"];
                lblStatus.Text = "";
                if (string.IsNullOrWhiteSpace(et)) //(!IsPostBack)
                    LoadSettings();
            
            
        }

        protected void LoadSettings()
        {

            try
            {
                Settings s = Settings.Load();
                txtAriaMappedPath.Text = s.AriaMappedPath;
                txtAriaMasterPath.Text = s.AriaMasterPath;
                txtAriaSharedPath.Text = s.AriaSharedPath;
                txtAriaSharedServerName.Text = s.AriaSharedServerName;
                txtAriaSharedLocalPath.Text = s.AriaSharedLocalPath;
                txtAriaSourcePath.Text = s.AriaSourcePath;
                txtAriaSQLDBsPath.Text = s.AriaSQLDBPath;
                txtDomainName.Text = s.DomainName;
                txtDomainAdminUserName.Text = s.DomainAdminUserName;
                txtDomainAdminUserPassword.Text = s.DomainAdminUserPassword;

                if (s.Aria4 != null)
                {
                    txtServerNameAria4.Text = s.Aria4.ServerName;
                    if (s.Aria4.Admin != null)
                    {
                        txtUserNameAria4.Text = s.Aria4.Admin.UserName;
                        txtPasswordAria4.Text = s.Aria4.Admin.Password;
                    }
                }

                if (s.ClientMaster != null)
                {
                    txtServerNameClientMaster.Text = s.ClientMaster.ServerName;
                    if (s.ClientMaster.Admin != null)
                    {
                        txtUserNameClientMaster.Text = s.ClientMaster.Admin.UserName;
                        txtPasswordClientMaster.Text = s.ClientMaster.Admin.Password;
                    }
                }

                if (s.SystemMaster != null)
                {
                    txtServerNameSystemMaster.Text = s.SystemMaster.ServerName;
                    txtDataBaseNameSystemMaster.Text = s.SystemMaster.DataBaseName;
                    if (s.SystemMaster.Admin != null)
                    {
                        txtUserNameSystemMaster.Text = s.SystemMaster.Admin.UserName;
                        txtPasswordSystemMaster.Text = s.SystemMaster.Admin.Password;
                    }
                }
            }
            catch (Exception ex)
            {
                lblStatus.Text = ex.Message;
            }
        }

        protected void btnSave_Click(object sender, EventArgs e)
        {
            Settings s = new Settings();
            s.AriaMappedPath = txtAriaMappedPath.Text;
            s.AriaMasterPath = txtAriaMasterPath.Text;
            s.AriaSharedPath = txtAriaSharedPath.Text;
            s.AriaSharedServerName = txtAriaSharedServerName.Text;
            s.AriaSharedLocalPath = txtAriaSharedLocalPath.Text;
            s.AriaSourcePath = txtAriaSourcePath.Text;
            s.AriaSQLDBPath = txtAriaSQLDBsPath.Text;

            s.DomainName = txtDomainName.Text;
            s.DomainAdminUserName = txtDomainAdminUserName.Text;
            s.DomainAdminUserPassword = txtDomainAdminUserPassword.Text;

            s.Aria4 = new Core.Utilites.SQLInfo();
            s.Aria4.ServerName = txtServerNameAria4.Text;
            s.Aria4.Admin = new Core.Utilites.Credential(txtUserNameAria4.Text, txtPasswordAria4.Text);

            s.ClientMaster = new Core.Utilites.SQLInfo();
            s.ClientMaster.ServerName = txtServerNameClientMaster.Text;
            s.ClientMaster.Admin = new Core.Utilites.Credential(txtUserNameClientMaster.Text, txtPasswordClientMaster.Text);

            s.SystemMaster = new Core.Utilites.SQLInfo();
            s.SystemMaster.ServerName = txtServerNameSystemMaster.Text;
            s.SystemMaster.DataBaseName = txtDataBaseNameSystemMaster.Text;
            s.SystemMaster.Admin = new Core.Utilites.Credential(txtUserNameSystemMaster.Text, txtPasswordSystemMaster.Text);
            s.Save();
            lblStatus.Text = "Saved Successfully";
        }
    }
}