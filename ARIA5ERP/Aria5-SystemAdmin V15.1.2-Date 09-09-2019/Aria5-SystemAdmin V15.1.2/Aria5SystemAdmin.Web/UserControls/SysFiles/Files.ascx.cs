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
    public partial class Files : System.Web.UI.UserControl, IComplexControl
    {
        private IObjectSpace CurrentObj;
        private XafApplication Application;
        public IList<AriaDevExpress.Module.BusinessObjects.SysFiles.Files> list
        {
            get
            {
                if (HttpContext.Current.Session["Fielslist"] == null)
                {
                    HttpContext.Current.Session["Fielslist"] = new List<AriaDevExpress.Module.BusinessObjects.SysFiles.Files>();
                }
                return HttpContext.Current.Session["Fielslist"] as List<AriaDevExpress.Module.BusinessObjects.SysFiles.Files>;
            }
            set
            {
                HttpContext.Current.Session["Fielslist"] = value;
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
                list.Clear();
              //  this.LB_Filters.Items.Clear();               
            }
           

                //ViewState.Add("types",TypeNames);
                //ViewState.Add("Datatypes", datatypeNames);
               
              

               
           
        }

        protected void AddToList_Click(object sender, EventArgs e)
        {
            AriaDevExpress.Module.BusinessObjects.SysFiles.Files F1 = new AriaDevExpress.Module.BusinessObjects.SysFiles.Files();
            F1.Alias = this.TXT_Name.Text;
            if (this.combo_Files.SelectedItem != null)
                F1.Name = combo_Files.SelectedItem.Value.ToString();
            if (Combo_Indeces.SelectedItem != null)
                F1.Index = Combo_Indeces.SelectedItem.Value.ToString();
            list.Add(F1);


            this.LB_Files.DataSource = list;
            LB_Files.DataTextField = "Alias";
            LB_Files.DataBind();
            clearcontrols();
         //   }
          
        }

        public void clearcontrols()
        {
            this.TXT_Name.Text = "";
            this.combo_Files.SelectedIndex = -1;
            this.Combo_Indeces.SelectedIndex = -1;
        }
        public void fillcontrols(AriaDevExpress.Module.BusinessObjects.SysFiles.Files File)
        {
            this.TXT_Name.Text = File.Alias;
            if (File.Name != null)
                this.combo_Files.SelectedItem = combo_Files.Items.FindByText(File.Name.ToString());
            if (File.Index != null)
                this.Combo_Indeces.SelectedItem = Combo_Indeces.Items.FindByText(File.Index.ToString()); 
        }
        public void Refresh()
        {

        }

        public void Setup(DevExpress.ExpressApp.IObjectSpace objectSpace, DevExpress.ExpressApp.XafApplication application)
        {
            if (list.Count > 0)
            {
                string text = "";
                FilesConverter c = new FilesConverter();
                text = (string)c.ConvertToStorageType(list);
                IList<AriaDevExpress.Module.BusinessObjects.SysFiles.Files> group = (IList<AriaDevExpress.Module.BusinessObjects.SysFiles.Files>)c.ConvertFromStorageType(text);
                ((Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveSetting)application.MainWindow.View.CurrentObject).Value = text;

            }
            else
            {
               
                // text = (string)c.ConvertToStorageType(list);
            }
            if (CurrentObj == null)
            {
                CurrentObj = objectSpace;
                Application = application;
            }
           // IList<AriaDevExpress.Module.BusinessObjects.SysFiles.Files> lustoffiles = new List<AriaDevExpress.Module.BusinessObjects.SysFiles.Files>();
            //AriaDevExpress.Module.BusinessObjects.SysFiles.Files files = new AriaDevExpress.Module.BusinessObjects.SysFiles.Files();
            //files.Name = "ahmed";
            //files.Alias = "tawfik";
            //files.Index = "scdfrtet";
            //lustoffiles.Add(files);
           // FilesConverter x = new FilesConverter();
          //  string ahmed = (string)x.ConvertToStorageType(lustoffiles);
           // lustoffiles = (List<AriaDevExpress.Module.BusinessObjects.SysFiles.Files>)x.ConvertFromStorageType("4D0F4C4153454C46494C450000410000000000000000000000000003000000000000010007004C4153454C46494C45000043000000000600000000000000000000000000000061686D6564004C4153454C46494C45000043000000000700000000000000000000000000000074617766696B004C4153454C46494C4500004300000000090000000000000000000000000000007363646672746574004C4153454C46494C4500004C0000000001000000000000000000000000000000004C4153454C46494C4500004C0000000001000000000000000000000000000000004C4153454C46494C4500004C0000000001000000000000000000000000000000004C4153454C46494C4500004C0000000001000000000000000000000000000000001A");
            
        }

        protected void RemoveFromList_Click(object sender, EventArgs e)
        {
            if (this.LB_Files.SelectedItem != null)
            {
                AriaDevExpress.Module.BusinessObjects.SysFiles.Files exist = list.Where(x => x.Name == this.LB_Files.SelectedItem.Text.ToString()).FirstOrDefault();
                if (exist != null)
                {
                    list.Remove(exist);
                    clearcontrols();
                    this.LB_Files.DataSource = list;
                    LB_Files.DataTextField = "Alias";
                    LB_Files.DataBind();
                }
            }
        }

        protected void LB_Filters_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (this.LB_Files.SelectedItem != null)
            {
                AriaDevExpress.Module.BusinessObjects.SysFiles.Files exist = list.Where(x => x.Name == this.LB_Files.SelectedItem.Text.ToString()).FirstOrDefault();
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
            this.LB_Files.SelectedIndex = -1;
            if (!this.AddToList.Enabled)
            this.AddToList.Enabled = true;
            
        }

        protected void LB_Files_Init(object sender, EventArgs e)
        {
            string value = ((Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveSetting)Application.MainWindow.View.CurrentObject).Value;
            FilesConverter c = new FilesConverter();
            list = (IList<AriaDevExpress.Module.BusinessObjects.SysFiles.Files>)c.ConvertFromStorageType(value);
            this.LB_Files.DataSource = list;
            LB_Files.DataTextField = "Alias";
            LB_Files.DataBind();
        }

        protected void combo_Files_Init(object sender, EventArgs e)
        {
            IList<AriaObject> listofariaobjects = CurrentObj.GetObjects<AriaObject>(CriteriaOperator.Parse("ObjectType.Name ='Data'"));
            combo_Files.DataSource = listofariaobjects;
            combo_Files.ValueField = "ObjectName";
            combo_Files.TextField = "ObjectName";
            combo_Files.DataBind();
        }

        protected void Combo_Indeces_Init(object sender, EventArgs e)
        {
            IList<AriaObject> listofariaobjects = CurrentObj.GetObjects<AriaObject>(CriteriaOperator.Parse("ObjectType.Name ='Aria Index'"));
            combo_Files.DataSource = listofariaobjects;
            combo_Files.ValueField = "ObjectName";
            combo_Files.TextField = "ObjectName";
            combo_Files.DataBind();
        }
    }
}