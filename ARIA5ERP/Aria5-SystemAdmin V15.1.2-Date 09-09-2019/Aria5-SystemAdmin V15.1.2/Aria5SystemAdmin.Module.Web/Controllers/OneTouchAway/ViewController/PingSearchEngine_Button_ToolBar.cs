using System;
using System.ComponentModel;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Actions;
using DevExpress.Persistent.Base;
using System.Data;
using System.Data.SqlClient;
using AriaDevExpress.Module.BusinessObjects.OneTouchAway;
using AriaDevExpress.Module.Web.Classes.OneTouchAway.SiteMap;

namespace AriaDevExpress.Module.Web.Controllers.OneTouchAway
{
    public partial class PingSearchEngine_Button_ToolBar : ViewController
    {
        public PingSearchEngine_Button_ToolBar()
        {
            InitializeComponent();
            RegisterActions(components);
        }

      

        void Ok_Click(object sender, ShowViewParameters e)
        {
            //new GenericMessageBox.GenericMessageBox(e, Application, "Ping Search Engine");
        }

        private void PingSearchEngine_Button_ToolBar_Activated(object sender, EventArgs e)
        {
          
                OTAWAYPingSerchEngine2.Active.SetItemValue(
                 "ObjectType", View.ObjectTypeInfo.Type == typeof(Sitemap));
            
        }

        private void PingSerchEngine2_Execute(object sender, SimpleActionExecuteEventArgs e)
        {

            new GenericMessageBox.GenericMessageBox(e.ShowViewParameters, Application, Sitemap.Ping("http://ariaftp.blob.core.windows.net/ariany/AriaNY.xml" + "AriaNY.xml") + "\n Site Map Url: " + "http://ariaftp.blob.core.windows.net/ariany/AriaNY.xml", Ok_Click);     
        }


        public static string myurl;

        
    }
}
