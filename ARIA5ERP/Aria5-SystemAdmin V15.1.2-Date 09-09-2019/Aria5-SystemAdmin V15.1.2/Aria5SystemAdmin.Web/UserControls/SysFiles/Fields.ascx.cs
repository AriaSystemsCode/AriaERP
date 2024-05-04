using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using AriaDevExpress.Module.BusinessObjects.SysFiles;
using DevExpress.ExpressApp.Editors;
using DevExpress.ExpressApp;
using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.Data.Filtering;
namespace Aria5SystemAdmin.Web.UserControls.SysFiles
{
    public partial class Fields : System.Web.UI.UserControl, IComplexControl
    {

        private IObjectSpace currentObj;
        public IList<AriaDevExpress.Module.BusinessObjects.SysFiles.Fields> list
        {
            get
            {
                if (HttpContext.Current.Session["Fieldslist"] == null)
                {
                    HttpContext.Current.Session["Fieldslist"] = new List<AriaDevExpress.Module.BusinessObjects.SysFiles.Fields>();
                }
                return HttpContext.Current.Session["Fieldslist"] as List<AriaDevExpress.Module.BusinessObjects.SysFiles.Fields>;
            }
            set
            {
                HttpContext.Current.Session["Fieldslist"] = value;
            }
        }

        protected override void OnInit(EventArgs e)
        {
            base.OnInit(e);
        }
        protected void Page_Load(object sender, EventArgs e)
        {
            if (!IsPostBack)
            {
                //  list.Clear();
                //  this.LB_Filters.Items.Clear();               
            }


            //ViewState.Add("types",TypeNames);
            //ViewState.Add("Datatypes", datatypeNames);





        }

        protected void AddToList_Click(object sender, EventArgs e)
        {
            AriaDevExpress.Module.BusinessObjects.SysFiles.Fields F1 = new AriaDevExpress.Module.BusinessObjects.SysFiles.Fields();
            F1.DisplayName = this.TXT_Alias.Text;
            F1.Expression = this.TXT_Expression.Text;
            list.Add(F1);


            this.LB_Fields.DataSource = list;
            LB_Fields.DataTextField = "Expression";
            LB_Fields.DataBind();
            clearcontrols();
            //   }

        }

        public void clearcontrols()
        {
            this.TXT_Alias.Text = "";
            this.combo_Fields.SelectedIndex = -1;
            this.TXT_Expression.Text = "";
        }
        public void fillcontrols(AriaDevExpress.Module.BusinessObjects.SysFiles.Fields Field)
        {
            this.TXT_Alias.Text = Field.DisplayName;
            this.TXT_Expression.Text = Field.Expression;
        }
        public void Refresh()
        {

        }

        public void Setup(DevExpress.ExpressApp.IObjectSpace objectSpace, DevExpress.ExpressApp.XafApplication application)
        {
            if (list.Count > 0)
            {
                string text = "";
                FieldsConverter c = new FieldsConverter();
                text = (string)c.ConvertToStorageType(list);
                IList<AriaDevExpress.Module.BusinessObjects.SysFiles.Fields> group = (IList<AriaDevExpress.Module.BusinessObjects.SysFiles.Fields>)c.ConvertFromStorageType(text);
                ((Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveSetting)application.MainWindow.View.CurrentObject).Value = text;

            }
            else
            {
                string value = ((Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveSetting)application.MainWindow.View.CurrentObject).Value;
                FieldsConverter c = new FieldsConverter();
                list = (IList<AriaDevExpress.Module.BusinessObjects.SysFiles.Fields>)c.ConvertFromStorageType(value);
                this.LB_Fields.DataSource = list;
                LB_Fields.DataTextField = "Expression";
                LB_Fields.DataBind();
                // text = (string)c.ConvertToStorageType(list);
            }
            if (currentObj == null)
            {
                currentObj = objectSpace;
            }
           // IList<AriaDevExpress.Module.BusinessObjects.SysFiles.Files> lustoffiles = new List<AriaDevExpress.Module.BusinessObjects.SysFiles.Files>();
            //AriaDevExpress.Module.BusinessObjects.SysFiles.Files files = new AriaDevExpress.Module.BusinessObjects.SysFiles.Files();
            //files.Name = "ahmed";
            //files.Alias = "tawfik";
            //files.Index = "scdfrtet";
            //lustoffiles.Add(files);
          //  FilesConverter x = new FilesConverter();
            //  string ahmed = (string)x.ConvertToStorageType(lustoffiles);
            //lustoffiles = (List<AriaDevExpress.Module.BusinessObjects.SysFiles.Files>)x.ConvertFromStorageType("4D0F4C4153454C46494C450000410000000000000000000000000003000000000000010007004C4153454C46494C45000043000000000600000000000000000000000000000061686D6564004C4153454C46494C45000043000000000700000000000000000000000000000074617766696B004C4153454C46494C4500004300000000090000000000000000000000000000007363646672746574004C4153454C46494C4500004C0000000001000000000000000000000000000000004C4153454C46494C4500004C0000000001000000000000000000000000000000004C4153454C46494C4500004C0000000001000000000000000000000000000000004C4153454C46494C4500004C0000000001000000000000000000000000000000001A");

        }

        protected void RemoveFromList_Click(object sender, EventArgs e)
        {
            if (this.LB_Fields.SelectedItem != null)
            {
                AriaDevExpress.Module.BusinessObjects.SysFiles.Fields exist = list.Where(x => x.Expression == this.LB_Fields.SelectedItem.Text.ToString()).FirstOrDefault();
                if (exist != null)
                {
                    list.Remove(exist);
                    clearcontrols();
                    this.LB_Fields.DataSource = list;
                    LB_Fields.DataTextField = "Name";
                    LB_Fields.DataBind();
                }
            }
        }

        protected void LB_Fields_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (this.LB_Fields.SelectedItem != null)
            {
                AriaDevExpress.Module.BusinessObjects.SysFiles.Fields exist = list.Where(x => x.Expression == this.LB_Fields.SelectedItem.Text.ToString()).FirstOrDefault();
                if (exist != null)
                {
                    fillcontrols(exist);
                    this.AddToList.Enabled = false;
                }
            }
        }

        protected void Clear_Click(object sender, EventArgs e)
        {
            clearcontrols();
            this.LB_Fields.SelectedIndex = -1;
            if (!this.AddToList.Enabled)
                this.AddToList.Enabled = true;

        }

        protected void LB_Fields_Init(object sender, EventArgs e)
        {
            this.LB_Fields.DataSource = list;
            LB_Fields.DataTextField = "Name";
            LB_Fields.DataBind();
        }

        protected void combo_Fields_Init(object sender, EventArgs e)
        {
            IList<AriaObject> listofariaobjects = currentObj.GetObjects<AriaObject>(CriteriaOperator.Parse("ObjectType.Name ='Aria Field'"));
            combo_Fields.DataSource = listofariaobjects;
            combo_Fields.ValueField = "ObjectName";
            combo_Fields.TextField = "ObjectName";
            combo_Fields.DataBind();
        }
    }
}