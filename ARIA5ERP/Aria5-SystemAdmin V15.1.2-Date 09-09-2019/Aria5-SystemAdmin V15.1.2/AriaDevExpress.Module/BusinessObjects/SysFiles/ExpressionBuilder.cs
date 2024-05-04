using System;
using System.ComponentModel;

using DevExpress.Xpo;
using DevExpress.Data.Filtering;

using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;
using DevExpress.ExpressApp.ConditionalAppearance;
using DevExpress.ExpressApp.Editors;

namespace AriaDevExpress.Module.BusinessObjects.SysFiles
{
    [Appearance("Hide File", TargetItems = "File", Visibility = ViewItemVisibility.Hide)]
    [NonPersistent()]
    [DefaultProperty("Expression")]
    public class ExpressionBuilder : BaseObject
    {
        public ExpressionBuilder(Session session)
            : base(session)
        {
            // This constructor is used when an object is loaded from a persistent storage.
            // Do not place any code here or place it only when the IsLoading property is false:
            // if (!IsLoading){
            //    It is now OK to place your initialization code here.
            // }
            // or as an alternative, move your initialization code into the AfterConstruction method.
        }

        [ImmediatePostData(true)]
        public FoxMathFunctions Math
        {
            get
            {
                return GetPropertyValue<FoxMathFunctions>("Math");
            }
            set
            {
                Expression += BO.GetEnumDisplayName(value);
                SetPropertyValue<FoxMathFunctions>("Math", value);
            }
        }

        [ImmediatePostData(true)]
        public FoxStringFunctions String
        {
            get
            {
                return GetPropertyValue<FoxStringFunctions>("String");
            }
            set
            {
                Expression += BO.GetEnumDisplayName(value);
                SetPropertyValue<FoxStringFunctions>("String", value);
            }
        }

        [DataSourceProperty("File.Fields")]
        [ImmediatePostData(true)]
        [Size(SizeAttribute.Unlimited)]
        public FileField FieldNames
        {
            get
            {
                return GetPropertyValue<FileField>("FieldNames");
            }
            set
            {
                if (value != null)
                    Expression += value.Field.Name;
                SetPropertyValue<FileField>("FieldNames", value);
            }
        }

        [ImmediatePostData(true)]
        public FoxLogicalFunctions Logical
        {
            get
            {
                return GetPropertyValue<FoxLogicalFunctions>("Logical");
            }
            set
            {
                Expression += BO.GetEnumDisplayName(value);
                SetPropertyValue<FoxLogicalFunctions>("Logical", value);
            }
        }

        [ImmediatePostData(true)]
        public FoxDateFunctions Date
        {
            get
            {
                return GetPropertyValue<FoxDateFunctions>("Date");
            }
            set
            {
                Expression += BO.GetEnumDisplayName(value);
                SetPropertyValue<FoxDateFunctions>("Date", value);
            }
        }



        public File File
        {
            get
            {
                return GetPropertyValue<File>("File");
            }
            set
            {
                SetPropertyValue<File>("File", value);
            }
        }

        [Size(SizeAttribute.Unlimited)]
        public string Expression
        {
            get
            {
                return GetPropertyValue<string>("Expression");
            }
            set
            {
                SetPropertyValue("Expression", value);
            }
        }

        public override void AfterConstruction()
        {
            base.AfterConstruction();
            // Place here your initialization code.
        }
    }

}