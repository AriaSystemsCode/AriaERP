using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using AriaDevExpress.Module.BusinessObjects.SysFiles;
using DevExpress.ExpressApp.Editors;
namespace Aria5SystemAdmin.Web.UserControls.ClientManager
{
    public partial class Filter : System.Web.UI.UserControl, IComplexControl
    {

        public IList<AriaDevExpress.Module.BusinessObjects.SysFiles.Filter> list
        {
            get
            {
                if (HttpContext.Current.Session["list"] == null)
                {
                    HttpContext.Current.Session["list"] = new List<AriaDevExpress.Module.BusinessObjects.SysFiles.Filter>();
                }
                return HttpContext.Current.Session["list"] as List<AriaDevExpress.Module.BusinessObjects.SysFiles.Filter>;
            }
            set
            {
                HttpContext.Current.Session["list"] = value;
            }
        }

        public IList<string> Types
        {
            get
            {

                if (HttpContext.Current.Session["Types"] == null)
                {
                    HttpContext.Current.Session["Types"] = new List<string>();
                }
                return HttpContext.Current.Session["Types"] as List<string>;
            }
            set
            {
                HttpContext.Current.Session["Types"] = value;
            }
        }
        public IList<string> DataTypes
        {
            get
            {

                if (HttpContext.Current.Session["DataTypes"] == null)
                {
                    HttpContext.Current.Session["DataTypes"] = new List<string>();
                }
                return HttpContext.Current.Session["DataTypes"] as List<string>;
            }
            set
            {
                HttpContext.Current.Session["DataTypes"] = value;
            }
        }
        public IList<string> SysOperator
        {
            get
            {

                if (HttpContext.Current.Session["SysOperator"] == null)
                {
                    HttpContext.Current.Session["SysOperator"] = new List<string>();
                }
                return HttpContext.Current.Session["SysOperator"] as List<string>;
            }
            set
            {
                HttpContext.Current.Session["SysOperator"] = value;
            }
        }
        public IList<string> variabletype
        {
            get
            {

                if (HttpContext.Current.Session["variabletype"] == null)
                {
                    HttpContext.Current.Session["variabletype"] = new List<string>();
                }
                return HttpContext.Current.Session["variabletype"] as List<string>;
            }
            set
            {
                HttpContext.Current.Session["variabletype"] = value;
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
            AriaDevExpress.Module.BusinessObjects.SysFiles.Filter F1 = new AriaDevExpress.Module.BusinessObjects.SysFiles.Filter();
            F1.Name = this.TXT_Name.Text.ToString();
            F1.Value = this.TXT_Value.Text.ToString();
            if (combo_type.SelectedItem != null)
            F1.Type = (FilterVariableType)Enum.Parse(typeof(FilterVariableType), combo_type.SelectedItem.Value.ToString());
            if (Combo_datatype.SelectedItem != null)
            F1.DataType = (SystemDataType)Enum.Parse(typeof(SystemDataType), Combo_datatype.SelectedItem.Value.ToString());
            if (combo_sysoperator.SelectedItem != null)
            F1.Operator = (FilterOperator)Enum.Parse(typeof(FilterOperator), combo_sysoperator.SelectedItem.Value.ToString());
            if (Combo_valueType.SelectedItem != null)
            F1.ValueType = (FilterValueType)Enum.Parse(typeof(FilterValueType), Combo_valueType.SelectedItem.Value.ToString());
            F1.Not = CB_Not.Checked;
            list.Add(F1);


            this.LB_Filters.DataSource = list;
            LB_Filters.DataTextField = "Name";
            LB_Filters.DataBind();
            clearcontrols();
         //   }
          
        }

        public void clearcontrols()
        {
            this.TXT_Name.Text = "";
            this.TXT_Value.Text = "";
            this.CB_Not.Checked = false;
            this.Combo_datatype.SelectedIndex = -1;
            this.combo_sysoperator.SelectedIndex = -1;
            this.combo_type.SelectedIndex = -1;
            this.Combo_valueType.SelectedIndex = -1;
        }
        public void fillcontrols(AriaDevExpress.Module.BusinessObjects.SysFiles.Filter Filter)
        {
            this.TXT_Name.Text = Filter.Name;
            this.TXT_Value.Text =Filter.Value;
            this.CB_Not.Checked = Filter.Not;
            if (Filter.DataType != null)
            this.Combo_datatype.SelectedItem = Combo_datatype.Items.FindByText(Filter.DataType.ToString());
            if (Filter.Operator != null)
            this.combo_sysoperator.SelectedItem = combo_sysoperator.Items.FindByText(Filter.Operator.ToString()); 
            if (Filter.Type != null)
            this.combo_type.SelectedItem = combo_type.Items.FindByText(Filter.Type.ToString());
            if (Filter.ValueType != null)
            this.Combo_valueType.SelectedItem = Combo_valueType.Items.FindByText(Filter.ValueType.ToString());
        }
        protected void Combo_datatype_Init(object sender, EventArgs e)
        {
            Array datatypeNames = System.Enum.GetNames(typeof(SystemDataType));
            Array datatypevalues = System.Enum.GetValues(typeof(SystemDataType));
            DataTypes.Clear();
            for (int i = 0; i <= datatypeNames.Length - 1; i++)
            {
                //datatypes[i, 0] = datatypeNames.GetValue(i).ToString();
                //datatypes[i, 1] = datatypevalues.GetValue(i).ToString();
                // ListItem item = new ListItem(datatypeNames.GetValue(i).ToString(), datatypevalues.GetValue(i).ToString());
                // this.DDL_DataType.Items.Add(item);
                // dropdownlist.Items.Add(item);
                DataTypes.Add(datatypeNames.GetValue(i).ToString());
            }
            this.Combo_datatype.DataSource = DataTypes;
            this.Combo_datatype.DataBind();
            
        }

        protected void combo_type_Init(object sender, EventArgs e)
        {
            Array Typevalues = System.Enum.GetValues(typeof(FilterVariableType));

            Array TypeNames = System.Enum.GetNames(typeof(FilterVariableType));
            Types.Clear();
            for (int i = 0; i <= TypeNames.Length - 1; i++)
            {

                //  types[i, 0] = TypeNames.GetValue(i).ToString();
                //  types[i, 1] = Typevalues.GetValue(i).ToString();
                //ListItem item = new ListItem(TypeNames.GetValue(i).ToString(), Typevalues.GetValue(i).ToString());
                // this.DDL_Type.Items.Add(item);
                // dropdownlist.Items.Add(item);
                Types.Add(TypeNames.GetValue(i).ToString());
            }
            this.combo_type.DataSource = Types;
            this.combo_type.DataBind();
        }

        protected void combo_sysoperator_Init(object sender, EventArgs e)
        {
            Array Operatorvalues = System.Enum.GetValues(typeof(FilterOperator));
            Array Operatornames = System.Enum.GetNames(typeof(FilterOperator));
            SysOperator.Clear();
            for (int i = 0; i <= Operatornames.Length - 1; i++)
            {
                //ListItem item = new ListItem(Operatornames.GetValue(i).ToString(), Operatorvalues.GetValue(i).ToString());
                //  this.DDL_Operator.Items.Add(item);
                // dropdownlist.Items.Add(item);
                SysOperator.Add(Operatornames.GetValue(i).ToString());
            }
            this.combo_sysoperator.DataSource = SysOperator;
            this.combo_sysoperator.DataBind();
        }

        protected void Combo_valueType_Init(object sender, EventArgs e)
        {
            Array ValueTypeValues = System.Enum.GetNames(typeof(FilterValueType));
            Array ValueTypenames = System.Enum.GetNames(typeof(FilterValueType));

            variabletype.Clear();
            for (int i = 0; i <= ValueTypenames.Length - 1; i++)
            {
                // ListItem item = new ListItem(ValueTypenames.GetValue(i).ToString(), ValueTypeValues.GetValue(i).ToString());
                // this.DDL_ValueType.Items.Add(item);
                // dropdownlist.Items.Add(item);
                variabletype.Add(ValueTypenames.GetValue(i).ToString());
            }
            this.Combo_valueType.DataSource = variabletype;
            this.Combo_valueType.DataBind();
        }

        public void Refresh()
        {

        }

        public void Setup(DevExpress.ExpressApp.IObjectSpace objectSpace, DevExpress.ExpressApp.XafApplication application)
        {
            if (list.Count > 0)
            {
                string text = "";
                FilterConverter c = new FilterConverter();
                text = (string)c.ConvertToStorageType(list);
                IList<AriaDevExpress.Module.BusinessObjects.SysFiles.Filter> group = (IList<AriaDevExpress.Module.BusinessObjects.SysFiles.Filter>)c.ConvertFromStorageType(text);
                ((Aria5SystemAdmin.Module.BusinessObjects.AriaObjectSetting)application.MainWindow.View.CurrentObject).Value = text;

            }
            else
            {
                string value = ((Aria5SystemAdmin.Module.BusinessObjects.AriaObjectSetting)application.MainWindow.View.CurrentObject).Value;
                FilterConverter c = new FilterConverter();
                list = (IList<AriaDevExpress.Module.BusinessObjects.SysFiles.Filter>)c.ConvertFromStorageType(value);
                this.LB_Filters.DataSource = list;
                LB_Filters.DataTextField = "Name";
                LB_Filters.DataBind();
                // text = (string)c.ConvertToStorageType(list);
            }
           // IList<AriaDevExpress.Module.BusinessObjects.SysFiles.Files> lustoffiles = new List<AriaDevExpress.Module.BusinessObjects.SysFiles.Files>();
           // AriaDevExpress.Module.BusinessObjects.SysFiles.Files files = new AriaDevExpress.Module.BusinessObjects.SysFiles.Files();
           // files.Name = "ahmed";
           // files.Alias = "tawfik";
           // files.Index = "scdfrtet";
           // lustoffiles.Add(files);
           // FilesConverter x = new FilesConverter();
           //string ahmed = (string)x.ConvertToStorageType(lustoffiles);
           // lustoffiles = (List<AriaDevExpress.Module.BusinessObjects.SysFiles.Files>)x.ConvertFromStorageType("4D0F4C4153454C46494C450000410000000000000000000000000003000000000000010007004C4153454C46494C45000043000000000600000000000000000000000000000061686D6564004C4153454C46494C45000043000000000700000000000000000000000000000074617766696B004C4153454C46494C4500004300000000090000000000000000000000000000007363646672746574004C4153454C46494C4500004C0000000001000000000000000000000000000000004C4153454C46494C4500004C0000000001000000000000000000000000000000004C4153454C46494C4500004C0000000001000000000000000000000000000000004C4153454C46494C4500004C0000000001000000000000000000000000000000001A");
            
        }

        protected void RemoveFromList_Click(object sender, EventArgs e)
        {
            if (this.LB_Filters.SelectedItem != null)
            {
                AriaDevExpress.Module.BusinessObjects.SysFiles.Filter exist = list.Where(x => x.Name == this.LB_Filters.SelectedItem.Text.ToString()).FirstOrDefault();
                if (exist != null)
                {
                    list.Remove(exist);
                    clearcontrols();
                    this.LB_Filters.DataSource = list;
                    LB_Filters.DataTextField = "Name";
                    LB_Filters.DataBind();
                }
            }
        }

        protected void LB_Filters_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (this.LB_Filters.SelectedItem != null)
            {
                AriaDevExpress.Module.BusinessObjects.SysFiles.Filter exist = list.Where(x => x.Name == this.LB_Filters.SelectedItem.Text.ToString()).FirstOrDefault();
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
            this.LB_Filters.SelectedIndex = -1;
            if (!this.AddToList.Enabled)
            this.AddToList.Enabled = true;
            
        }

        protected void LB_Filters_Init(object sender, EventArgs e)
        {
            this.LB_Filters.DataSource = list;
            LB_Filters.DataTextField = "Name";
            LB_Filters.DataBind();
        }
    }
}