using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Configuration;
using System.Net.Configuration;
using System.Web.Configuration;

namespace AriaDevExpress.Web.UserControls.OneTouchAway
{
    public partial class EmailTemplate : System.Web.UI.UserControl
    {
        protected void Page_Load(object sender, EventArgs e)
        { 
            Configuration config = WebConfigurationManager.OpenWebConfiguration(HttpContext.Current.Request.ApplicationPath);
            MailSettingsSectionGroup settings = (MailSettingsSectionGroup)config.GetSectionGroup("system.net/mailSettings");
            txtFrom.Text = settings.Smtp.From;
            txtFromName.Text = settings.Smtp.Network.TargetName;
            txtHost.Text = settings.Smtp.Network.Host;
            txtPass.Text = settings.Smtp.Network.Password;
            txtPort.Text = settings.Smtp.Network.Port.ToString();
            txtUser.Text = settings.Smtp.Network.UserName;
            chkSSL.Checked = settings.Smtp.Network.EnableSsl;
        }

        protected void btnSave_Click(object sender, EventArgs e)
        {
            int port = 25;
            int.TryParse(txtPort.Text, out port);

            Configuration webConfig = WebConfigurationManager.OpenWebConfiguration("~");
            MailSettingsSectionGroup settings = (MailSettingsSectionGroup)webConfig.GetSectionGroup("system.net/mailSettings");
            SmtpSection smtp = settings.Smtp;
            SmtpNetworkElement net = smtp.Network;

            smtp.From = txtFrom.Text;
            net.Host = txtHost.Text;
            net.UserName = txtUser.Text;
            net.EnableSsl = chkSSL.Checked;
            net.Password = txtPass.Text;
            net.TargetName = txtFromName.Text;
            net.Port = port;
            webConfig.Save();
        }
    }
}