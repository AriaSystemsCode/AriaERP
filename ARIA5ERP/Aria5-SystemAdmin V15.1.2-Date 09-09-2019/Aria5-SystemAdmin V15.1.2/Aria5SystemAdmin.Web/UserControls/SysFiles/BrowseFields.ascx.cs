using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.Web;
using AriaDevExpress.Module.BusinessObjects.SysFiles;

namespace AriaDevExpress.Web.UserControls.SysFiles
{
    public partial class BrowseFields : System.Web.UI.UserControl
    {
        protected override void OnInit(System.EventArgs e)
        {
            lbAvailable.DataSource = ParentFile.Fields.ToList();
            lbAvailable.DataBind();
            base.OnInit(e);
        }
        protected void Page_Load(object sender, EventArgs e)
        {
            if (IsPostBack && Request["__CALLBACKPARAM"].Contains("DialogOK"))
                Save();
        }

        public File ParentFile { get; set; }

       protected void lbAvailable_DataBound(object sender, EventArgs e)
        {
            if (ParentFile != null && !string.IsNullOrWhiteSpace(ParentFile.UserBrowseFileds))
            {
                string script = "";
                string scriptTemplate = lbAvailable.ClientInstanceName + ".SelectValues(['{0}']);AddSelectedItems();";
                string[] browseFields = ParentFile.UserBrowseFileds.Split(new string[] { "|" }, StringSplitOptions.RemoveEmptyEntries);
                foreach (string field in browseFields)
                {
                    if (lbAvailable.Items.FindByValue(field) != null)
                    {
                        lbAvailable.Items.FindByValue(field).Selected = true;
                        script += string.Format(scriptTemplate, field);
                    }
                }
                if (!string.IsNullOrWhiteSpace(script))
                {
                    Page.ClientScript.RegisterStartupScript(typeof(Page), "MoveItems", script, true);
                }
            }
        }


        protected void Save()
        {
            var items = lbChoosen.Items;
            string browseFieldsString = "";
            foreach (ListEditItem item in items)
            {
                browseFieldsString += item.Value + "|";
            }
            browseFieldsString = browseFieldsString.EndsWith("|") ? browseFieldsString.Remove(browseFieldsString.Length - 1) : browseFieldsString;
            ParentFile.UserBrowseFileds = browseFieldsString;
            //DBDataContext db = new DBDataContext();
            //SYDFILE File = db.SYDFILEs.Where(file => file.cfile_nam == Convert.ToString(AllFieldsComboBox.SelectedItem.Value)).First();
            //if (File != null)
            //{
            //    File.musr_brow = browseFieldsString;
            //    db.SubmitChanges();
            //}
            //lblStatus.Text = "Saved Successfully.";
            // Response.Redirect(Request.RawUrl, true);

        }
    }
}