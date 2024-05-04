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
    public partial class RefreshSiteMap_Button_ToolBar : ViewController
    {
        DataTable table = new DataTable();
        private string connectionString = AriaDevExpress.Module.DataContext.ConnectionString.Getvalue();
  
        public RefreshSiteMap_Button_ToolBar()
        {
            InitializeComponent();
            RegisterActions(components);
        }

        private void RefreshSiteMap_Button_ToolBar_Activated(object sender, EventArgs e)
        {
           
                OTAWAYRefreshSiteMap.Active.SetItemValue(
              "ObjectType", View.ObjectTypeInfo.Type == typeof(Sitemap));
            
        }

        private void RefreshSiteMap_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            SqlConnection conn = new SqlConnection(connectionString);
            conn.Open();
            SqlCommand comm = new SqlCommand();
            comm.CommandText = "SELECT [URL] as loc FROM [recurrsiveSiteMap]";
            comm.CommandType = CommandType.Text;
            comm.Connection = conn;

            SqlDataAdapter dataAdapter = new SqlDataAdapter(comm);
            dataAdapter.Fill(table);

            conn.Close();

            Sitemap sitemap0 = new Sitemap("http://ariaftp.blob.core.windows.net/ariany/AriaNY.xml");

            foreach (DataRow row in table.Rows)
            {
                Url url1 = new Url();
                url1.Loc = row["loc"].ToString();
                url1.Priority = "1";
                url1.LastModifiedDateTime = DateTime.Now;
                url1.ChangeFreq = "always";

                sitemap0.Add(url1);
            }
            sitemap0.Write();
            new GenericMessageBox.GenericMessageBox2(e.ShowViewParameters, Application, "SiteMap Generated Successfully URL \n Check URL : http://ariaftp.blob.core.windows.net/ariany/AriaNY.xml", Ok_Click);

        }
        void Ok_Click(object sender, ShowViewParameters e)
        {
            //new GenericMessageBox.GenericMessageBox(e, Application, "Ping Search Engine");
        }
    }
}
