using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using DevExpress.Xpo;
using DevExpress.Data.Filtering;

using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;
using DevExpress.ExpressApp.ConditionalAppearance;

namespace AriaDevExpress.Module.BusinessObjects.SysFiles
{
    [NonPersistent()]
    public class ReportVariableBrowseFieldSelector : BaseObject
    {
        public ReportVariableBrowseFieldSelector(Session session)
            : base(session)
        {
            // This constructor is used when an object is loaded from a persistent storage.
            // Do not place any code here or place it only when the IsLoading property is false:
            // if (!IsLoading){
            //    It is now OK to place your initialization code here.
            // }
            // or as an alternative, move your initialization code into the AfterConstruction method.
        }

        [Size(SizeAttribute.Unlimited)]
        public ReportVariable MainVariable { get; private set; }

        public void SetVariable(ReportVariable repVar)
        {
            MainVariable = repVar;
        }

        File _MainFile;
        [Size(SizeAttribute.Unlimited)]
        public File MainFile
        {
            get
            {
                if (_MainFile == null && MainVariable.Table != null)
                    _MainFile = MainVariable.Table;
                return _MainFile;
            }
            set
            {
                _MainFile = value;
            }
        }

        FileField _BrowseField;
        [ImmediatePostData(true)]
        [DataSourceProperty("MainFile.Fields")]
        [DisplayName("Select Browse Fields")]
        [Size(SizeAttribute.Unlimited)]
        public FileField BrowseField
        {
            get
            {
                return _BrowseField;
            }
            set
            {
                if (value != null)
                {
                    if (!string.IsNullOrWhiteSpace(BrowseFields))
                        BrowseFields += ",";
                    BrowseFields += value.Field.Name;
                }
                _BrowseField = value;
            }
        }


        [Size(SizeAttribute.Unlimited)]
        public string BrowseFields
        {
            get
            {
                return MainVariable.BrowseFields;
            }
            set
            {
                MainVariable.BrowseFields = value;
            }
        }


        FileField _ReturnField;
        [ImmediatePostData(true)]
        [DataSourceProperty("MainFile.Fields")]
        [DisplayName("Select Return Field")]
        [Size(SizeAttribute.Unlimited)]
        public FileField ReturnFieldSelector
        {
            get
            {
                return _ReturnField;
            }
            set
            {
                if (value != null)
                {
                    ReturnField = value.Field.Name;
                }
                _ReturnField = value;
            }
        }

        [Size(SizeAttribute.Unlimited)]
        public string ReturnField
        {
            get
            {
                return MainVariable.ReturnedFieldName;
            }
            set
            {
                MainVariable.ReturnedFieldName = value;
            }
        }

    }
}