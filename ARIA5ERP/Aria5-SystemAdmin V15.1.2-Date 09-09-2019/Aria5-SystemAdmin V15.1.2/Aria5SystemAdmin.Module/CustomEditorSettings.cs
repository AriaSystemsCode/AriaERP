using System;
using System.Globalization;
using System.Web.UI.WebControls;
using DevExpress.Web;
using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Web;
using DevExpress.ExpressApp.Editors;
using DevExpress.ExpressApp.Web.Editors.ASPx;
using DevExpress.ExpressApp.Model;
using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.Utils;
using AriaDevExpress.Module;
using AriaDevExpress.Module.BusinessObjects.HR;
//... 
namespace Aria5SystemAdmin.Module
{
    [PropertyEditor(typeof(String), false)]
    public class CustomEditorSettings : ASPxPropertyEditor, IComplexViewItem
        
    {
        ASPxComboBox ASPxComboBoxControl = null;
        ASPxDateEdit ASPxDateEditControl = null;
        ASPxSpinEdit ASPxSpinEditControl = null;
        ASPxCheckBox ASPxCheckBoxControl = null;
        ASPxTextBox ASPxTextEditControl = null;
        ASPxImagePropertyEditor ASPxImagePropertyEditorControl = null;
        ASPxListEdit ASPxListEditControl = null;
        ASPxColorPropertyEditor ASPxColorPropertyEditorControl = null;
        ASPxDecimalPropertyEditor ASPxDecimalPropertyEditorControl = null;

        ASPxButton btn = null;
        XafApplication currentapp = null;
        IObjectSpace currentobject = null;
        string Columnname = "";
        public CustomEditorSettings(Type objectType, IModelMemberViewItem info)
            : base(objectType, info) { }

        protected override WebControl CreateEditModeControlCore()
        {

            string ColumnType = "";
            int VisibleNumber = 0;
            string PropertyValueOver = "";
            int width = 0;
            int decimalPlaces = 0;
            String [] ValidValues = null;
            String Format = "";
            
            try
            {
                if (this.View == null)
                {
                    ASPxGridView GridView1 = ((ASPxGridView)((DevExpress.ExpressApp.Web.TableEx)this.Control).NamingContainer.Parent.NamingContainer);
                    VisibleNumber = GridView1.EditingRowVisibleIndex;
                    ColumnType = GridView1.GetRowValues(VisibleNumber, "DataType").ToString().ToUpper().TrimEnd()  ;
                    PropertyValueOver = GridView1.GetRowValues(VisibleNumber, "Value").ToString();

                    width = int.Parse(GridView1.GetRowValues(VisibleNumber, "Width").ToString() );
                    decimalPlaces = int.Parse(GridView1.GetRowValues(VisibleNumber, "DecimalPlaces").ToString());

                    if (GridView1.GetRowValues(VisibleNumber, "Validvalues") != null)
                    { ValidValues = GridView1.GetRowValues(VisibleNumber, "Validvalues").ToString().Split(','); };

                    if (GridView1.GetRowValues(VisibleNumber, "Format") != null)
                    { Format = GridView1.GetRowValues(VisibleNumber, "Format").ToString(); };

                    
                }
                else
                {
                    if (this.CurrentObject.GetType() == typeof(AriaObjectSetting))
                    {
                        ColumnType = ((AriaObjectSetting)this.CurrentObject).DataType.ToString().ToUpper().TrimEnd();
                        PropertyValueOver = this.PropertyValue.ToString();
                        
                        //BElal
                        Columnname = ((AriaObjectSetting)this.CurrentObject).ActualColumnName.ToString().ToUpper().TrimEnd();
                        //BElal

                        width = int.Parse(((AriaObjectSetting)this.CurrentObject).Width.ToString());
                        decimalPlaces = int.Parse(((AriaObjectSetting)this.CurrentObject).DecimalPlaces.ToString());

                        if (((AriaObjectSetting)this.CurrentObject).Validvalues != null)
                        { ValidValues = ((AriaObjectSetting)this.CurrentObject).Validvalues.ToString().Split(','); };

                        if (((AriaObjectSetting)this.CurrentObject).Format != null)
                        { Format = ((AriaObjectSetting)this.CurrentObject).Format.ToString().TrimEnd(); };
                    }
                    else if (this.CurrentObject.GetType() == typeof(AriaObjectShelveSetting))
                    {
                        ColumnType = ((AriaObjectShelveSetting)this.CurrentObject).DataType.ToString().ToUpper().TrimEnd();
                       // PropertyValueOver = this.PropertyValue.ToString();
                        Columnname = ((AriaObjectShelveSetting)this.CurrentObject).ActualColumnName.ToString().ToUpper().TrimEnd();
                        
                      //  width = int.Parse(((AriaObjectShelveSetting)this.CurrentObject).Width.ToString());
                       // decimalPlaces = int.Parse(((AriaObjectShelveSetting)this.CurrentObject).DecimalPlaces.ToString());

                       // if (((AriaObjectShelveSetting)this.CurrentObject).Validvalues != null)
                        //{ ValidValues = ((AriaObjectShelveSetting)this.CurrentObject).Validvalues.ToString().Split(','); };

//                        if (((AriaObjectShelveSetting)this.CurrentObject).Format != null)
  //                      { Format = ((AriaObjectShelveSetting)this.CurrentObject).Format.ToString().TrimEnd(); };
                    }
                    
                }


            }
            catch (Exception ex)
            { };

            switch (ColumnType)
            {
                #region VALIDENTRIES
                case "VALIDENTRIES":

                    ASPxComboBoxControl = RenderHelper.CreateASPxComboBox();
                    ASPxComboBoxControl.ValueChanged += new EventHandler(ExtendedEditValueChangedHandler);
                    if (ValidValues != null)
                    {
                        for (int intValid = 0; intValid < ValidValues.Length; intValid++)
                        {
                            ((ASPxComboBox)ASPxComboBoxControl).Items.Add(ValidValues[intValid].ToString().TrimEnd());

                        };
                    };


                    return ASPxComboBoxControl;
                    break;
                #endregion

                #region DATE
                case "DATE" :

                    ASPxDateEditControl = RenderHelper.CreateASPxDateEdit();
                    ASPxDateEditControl.ValueChanged += new EventHandler(ExtendedEditValueChangedHandler);
                    ASPxDateEditControl.MaxDate = DateTime.Parse("01/01/2100");
                    //ASPxDateEdit1.DisplayFormatString = "yyyy/MM/dd";
                    //ASPxDateEdit1.Value = this.PropertyValue.ToString();
                    if (string.IsNullOrEmpty(PropertyValueOver) == false)
                    {
                        ASPxDateEditControl.Date = DateTime.Parse(PropertyValueOver);
                    }
                    if (string.IsNullOrEmpty(Format) == false)
                    {
                        ASPxDateEditControl.DisplayFormatString = Format;
                        
                    };


                    return ASPxDateEditControl;
                    break;
                #endregion

                #region DATETIME
                case "DATETIME":

                    ASPxDateEditControl = RenderHelper.CreateASPxDateEdit();
                    ASPxDateEditControl.ValueChanged += new EventHandler(ExtendedEditValueChangedHandler);
                    ASPxDateEditControl.MaxDate = DateTime.Parse("01/01/2100");
                    //ASPxDateEdit1.DisplayFormatString = "yyyy/MM/dd";
                    //ASPxDateEdit1.Value = this.PropertyValue.ToString();
                    ASPxDateEditControl.Date = DateTime.Parse(PropertyValueOver);
                    if (string.IsNullOrEmpty(Format) == false)
                    {
                        ASPxDateEditControl.DisplayFormatString = Format;

                    };


                    return ASPxDateEditControl;
                    break;
                #endregion

                #region NUMBER
                case "NUMBER":
                    ASPxSpinEditControl = RenderHelper.CreateASPxSpinEdit();
                    ASPxSpinEditControl.ValueChanged += new EventHandler(ExtendedEditValueChangedHandler);
                    ASPxSpinEditControl.MaxValue = 100000000;
                    ASPxSpinEditControl.MinValue = 0;
                    ASPxSpinEditControl.MaxLength = width;
                    ASPxSpinEditControl.DecimalPlaces = decimalPlaces;
                    if (string.IsNullOrEmpty(Format) == false)
                    {
                        ASPxSpinEditControl.DisplayFormatString = Format;
                        
                    };
                    return ASPxSpinEditControl;
                    break;

                #endregion

                #region NUMBER
                case "INT":
                    ASPxSpinEditControl = RenderHelper.CreateASPxSpinEdit();
                    ASPxSpinEditControl.ValueChanged += new EventHandler(ExtendedEditValueChangedHandler);
                    ASPxSpinEditControl.MaxValue = 100000000;
                    ASPxSpinEditControl.MinValue = 0;
                    ASPxSpinEditControl.MaxLength = width;
                    ASPxSpinEditControl.DecimalPlaces = decimalPlaces;
                    if (string.IsNullOrEmpty(Format) == false)
                    {
                        ASPxSpinEditControl.DisplayFormatString = Format;

                    };
                    return ASPxSpinEditControl;
                    break;

                #endregion
               
                #region BOOLEAN
                case "BOOLEAN":
                    ASPxCheckBoxControl = RenderHelper.CreateASPxCheckBox();
                    ASPxCheckBoxControl.ValueChanged += new EventHandler(ExtendedEditValueChangedHandler);
                    ASPxCheckBoxControl.Text = "";
                    ASPxCheckBoxControl.Checked = (PropertyValueOver.ToUpper().TrimEnd() == "TRUE") ? true : false;
                    ASPxCheckBoxControl.ValueChecked = "True";
                    ASPxCheckBoxControl.ValueUnchecked = "False";
                    ASPxCheckBoxControl.ValueType = typeof(String);
                    return ASPxCheckBoxControl;

                    break;

                #endregion
                case "MEMO" :
                    //MMT
                    if (Columnname.ToUpper().Contains("FILS") || Columnname.ToUpper().Contains("FLT") || Columnname.ToUpper().Contains("FIELD") || Columnname.ToUpper().Contains("FLD"))
                        {
                        //MMT
                        btn = RenderHelper.CreateASPxButton();
                        btn.Click += btn_Click;
                        btn.Text = "Update The Memo Field Value";
                        return btn;
                        //MMT
                    }
                    else
                    {
                        ASPxTextEditControl = RenderHelper.CreateASPxTextBox();
                        ASPxTextEditControl.ValueChanged += new EventHandler(ExtendedEditValueChangedHandler);
                        ASPxTextEditControl.MaxLength = width;
                        if (string.IsNullOrEmpty(Format) == false)
                        {
                            ASPxTextEditControl.DisplayFormatString = Format;
                            ASPxTextEditControl.MaskSettings.Mask = Format;
                        };

                        return ASPxTextEditControl;
                        
                    }
                    //MMT
                    break;
                #region Text
                default:

                    ASPxTextEditControl = RenderHelper.CreateASPxTextBox();
                    ASPxTextEditControl.ValueChanged += new EventHandler(ExtendedEditValueChangedHandler);
                    ASPxTextEditControl.MaxLength = width;
                    if (string.IsNullOrEmpty(Format) == false)
                    {
                        ASPxTextEditControl.DisplayFormatString = Format;
                        ASPxTextEditControl.MaskSettings.Mask = Format;
                    };

                    return ASPxTextEditControl;
                #endregion
            }
        }

        void btn_Click(object sender, EventArgs e)
        {
            //MessageOptions ms = new MessageOptions();
            //ms.Message = "Hi i am a new dev message ";
            //ms.Type = InformationType.Info;
            if (Columnname.ToUpper().Contains("FILS"))
            {
                currentobject = this.currentapp.CreateObjectSpace(typeof(AriaDevExpress.Module.BusinessObjects.SysFiles.Files));
                DetailView view = this.currentapp.CreateDetailView(currentobject, new AriaDevExpress.Module.BusinessObjects.SysFiles.Files());
                this.currentapp.ShowViewStrategy.ShowViewInPopupWindow(view);
            }
            else if (Columnname.ToUpper().Contains("FLT"))
            {
                currentobject = this.currentapp.CreateObjectSpace(typeof(AriaDevExpress.Module.BusinessObjects.SysFiles.Filter));
                DetailView view = this.currentapp.CreateDetailView(currentobject, new AriaDevExpress.Module.BusinessObjects.SysFiles.Filter());
                this.currentapp.ShowViewStrategy.ShowViewInPopupWindow(view);
            }
            else if (Columnname.ToUpper().Contains("FIELD") || Columnname.ToUpper().Contains("FLD"))
            {
                currentobject = this.currentapp.CreateObjectSpace(typeof(AriaDevExpress.Module.BusinessObjects.SysFiles.Fields));
                DetailView view = this.currentapp.CreateDetailView(currentobject, new AriaDevExpress.Module.BusinessObjects.SysFiles.Fields());
                this.currentapp.ShowViewStrategy.ShowViewInPopupWindow(view);
            }
        }


        public override void BreakLinksToControl(bool unwireEventsOnly)
        {
            if (ASPxComboBoxControl != null)
            {
                ASPxComboBoxControl.ValueChanged -= new EventHandler(ExtendedEditValueChangedHandler);
            }
            if (ASPxDateEditControl != null)
            {
                ASPxDateEditControl.ValueChanged -= new EventHandler(ExtendedEditValueChangedHandler);
            }

            if (ASPxSpinEditControl != null)
            {
                ASPxSpinEditControl.ValueChanged -= new EventHandler(ExtendedEditValueChangedHandler);
            }

            if (ASPxCheckBoxControl != null)
            {
                ASPxCheckBoxControl.ValueChanged -= new EventHandler(ExtendedEditValueChangedHandler);
            }

            if (ASPxTextEditControl != null)
            {
                ASPxTextEditControl.ValueChanged -= new EventHandler(ExtendedEditValueChangedHandler);
            }

            if (ASPxImagePropertyEditorControl != null)
            {
                ASPxImagePropertyEditorControl.ControlValueChanged -= new EventHandler(ExtendedEditValueChangedHandler);
            }

            if (ASPxListEditControl != null)
            {
                ASPxListEditControl.ValueChanged -= new EventHandler(ExtendedEditValueChangedHandler);
            }

            if (ASPxColorPropertyEditorControl != null)
            {
                ASPxColorPropertyEditorControl.ControlValueChanged -= new EventHandler(ExtendedEditValueChangedHandler);
            }

            if (ASPxDecimalPropertyEditorControl != null)
            {
                ASPxDecimalPropertyEditorControl.ControlValueChanged -= new EventHandler(ExtendedEditValueChangedHandler);
            }

            base.BreakLinksToControl(unwireEventsOnly);

        }

        protected override object GetControlValueCore()
        {
            try
            {
                if (this.View == null)
                {
                    return base.GetControlValueCore().ToString();
                }
                else
                { return base.GetControlValueCore(); }
            }
            catch (Exception ex)
            { return base.GetControlValueCore(); };

        }

        public void ExtendedEditValueChangedHandler(object source, EventArgs e)
        {
            if (this.View != null)
            {
                if (ASPxComboBoxControl != null)
                {
                    base.ExtendedEditValueChangedHandler(source, e);
                    return;
                }
                if (ASPxDateEditControl != null)
                {
                    this.PropertyValue = ((ASPxDateEdit)source).Date.ToString();
                    return;
                }

                if (ASPxSpinEditControl != null)
                {
                    this.PropertyValue = ((ASPxSpinEdit)source).Value.ToString();
                    return;
                }

                if (ASPxCheckBoxControl != null)
                {
                    this.PropertyValue = ((ASPxCheckBox)source).Value.ToString();
                    return;
                }

                if (ASPxTextEditControl != null)
                {
                    base.ExtendedEditValueChangedHandler(source, e);
                    return;
                }

                if (ASPxImagePropertyEditorControl != null)
                {
                    this.PropertyValue = ASPxImagePropertyEditorControl.ControlValue.ToString();
                    //base.ExtendedEditValueChangedHandler(source, e);
                    return;
                }

                if (ASPxListEditControl != null)
                {
                    this.PropertyValue = ASPxListEditControl.Value.ToString();
                    //base.ExtendedEditValueChangedHandler(source, e);
                    return;
                }

                if (ASPxColorPropertyEditorControl != null)
                {
                    this.PropertyValue = ASPxColorPropertyEditorControl.ControlValue.ToString();
                    //base.ExtendedEditValueChangedHandler(source, e);
                    return;
                }

                if (ASPxDecimalPropertyEditorControl != null)
                {
                    this.PropertyValue = ASPxDecimalPropertyEditorControl.ControlValue.ToString();
                    //base.ExtendedEditValueChangedHandler(source, e);
                    return;
                }

            }

            base.ExtendedEditValueChangedHandler(source, e);


        }

        //ATa implement complex view item 
        public void Setup(IObjectSpace objectSpace, XafApplication application)
        {
            currentapp = application;
            currentobject = objectSpace;
        }
    }
}