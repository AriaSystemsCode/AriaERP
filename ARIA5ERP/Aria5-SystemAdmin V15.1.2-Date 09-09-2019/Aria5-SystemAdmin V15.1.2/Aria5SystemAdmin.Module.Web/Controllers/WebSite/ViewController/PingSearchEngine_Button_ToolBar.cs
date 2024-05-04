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
using AriaDevExpress.Module.BusinessObjects.WebSite;
using AriaDevExpress.Module.Web.Classes.WebSite.SiteMap;

namespace AriaDevExpress.Module.Web.Controllers.WebSite
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
          
                PingSerchEngine2.Active.SetItemValue(
                 "ObjectType", View.ObjectTypeInfo.Type == typeof(SiteMap));
            
        }

        private void PingSerchEngine2_Execute(object sender, SimpleActionExecuteEventArgs e)
        {

            new GenericMessageBox.GenericMessageBox(e.ShowViewParameters, Application, Sitemap.Ping("http://ariaftp.blob.core.windows.net/ariany/AriaNY.xml" + "AriaNY.xml") + "\n Site Map Url: " + "http://ariaftp.blob.core.windows.net/ariany/AriaNY.xml", Ok_Click);     
        }


        public static string myurl;

        
    }
}
