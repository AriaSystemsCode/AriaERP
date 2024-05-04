using System;
using System.Data;
using System.IO;
using AriaDevExpress.Module.BusinessObjects.SysFiles;
using DevExpress.Xpo;
using Ionic.Zip;



namespace AriaDevExpress.Web.UserControls.SysFiles
{
    public partial class ExportUserControl : System.Web.UI.UserControl
    {
        protected override void OnInit(EventArgs e)
        {
            base.OnInit(e);

            foreach (string item in Enum.GetNames(typeof(Products)))
                applicationDropDownlist.Items.Add(item);

            foreach (string item in Enum.GetNames(typeof(DataBaseTypes)))
                DataBaseDropDownlist.Items.Add(item);

            DevExpress.Xpo.Session session = new DevExpress.Xpo.Session();
            session.ConnectionString = AriaDevExpress.Module.DataContext.ConnectionString.Getvalue();
            XPQuery<Sys_Client> clients = new XPQuery<Sys_Client>(session);
            foreach (Sys_Client client in clients)
            {
                ClientDropDownlist.Items.Add(client.ClientName, client.ClientID);
            }
            TablesListBox.SelectAll();
        }

        protected void btnExport_Click(object sender, EventArgs e)
        {
            if (TablesListBox.SelectedIndex >= 0)
            {
                System.Data.SqlClient.SqlCommand cmd = new System.Data.SqlClient.SqlConnection(AriaDevExpress.Module.DataContext.ConnectionString.Getvalue()).CreateCommand();

                string sqlSelect = "Select * from {0} {1}";

                string where = "";
                if (ClientDropDownlist.SelectedIndex >= 0 || applicationDropDownlist.SelectedIndex >= 0 || DataBaseDropDownlist.SelectedIndex >= 0)
                {
                    where = " where ";
                    if (ClientDropDownlist.SelectedIndex >= 0)
                        where += string.Format("[Client]='{0}'", ClientDropDownlist.SelectedItem.Value.ToString()) + " And ";
                    if (applicationDropDownlist.SelectedIndex >= 0)
                        where += string.Format("[Product]='{0}'", applicationDropDownlist.SelectedItem.Value.ToString()) + " And ";
                    if (DataBaseDropDownlist.SelectedIndex >= 0)
                        where += string.Format("[Database]='{0}'", DataBaseDropDownlist.SelectedItem.Value.ToString());
                    if (where.EndsWith(" And "))
                        where = where.Remove(where.Length - 4);
                }
                DataSet data = new DataSet();
                data.DataSetName = "SysFilesXML";
                foreach (DevExpress.Web.ListEditItem item in TablesListBox.SelectedItems)
                {
                    cmd.CommandText = string.Format(sqlSelect, item.Value.ToString(), where);
                    System.Data.SqlClient.SqlDataAdapter da = new System.Data.SqlClient.SqlDataAdapter(cmd);
                    da.Fill(data);
                    data.Tables[data.Tables.Count - 1].TableName = item.Value.ToString();
                }
                using (var zip = new ZipFile())
                {
                    foreach (DataTable table in data.Tables)
                    {
                        MemoryStream memStream = new MemoryStream();
                        table.WriteXml(memStream);
                        memStream.Position = 0;
                        zip.AddEntry(table.TableName + ".xml", memStream);
                    }
                    zip.Save(Response.OutputStream);

                  //  Response.ContentType = "text/xml";
                    Response.ContentType = "application/zip";
                    Response.AppendHeader("Content-Disposition", "attachment; filename=Export " +
                        DateTime.Now.ToShortDateString() + " " +
                        DateTime.Now.ToShortTimeString() + ".Zip");
                    Response.End();
                }
                //data.WriteXml(Response.OutputStream);
            }
        }
    }
}