using System;
using System.ComponentModel;

using DevExpress.Xpo;
using DevExpress.Data.Filtering;

using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;
using System.Collections.Generic;

namespace AriaDevExpress.Module.BusinessObjects.SysFiles
{

    public class RelatedField : XPLiteObject
    {
        public RelatedField(Session session)
            : base(session)
        {
            // This constructor is used when an object is loaded from a persistent storage.
            // Do not place any code here or place it only when the IsLoading property is false:
            // if (!IsLoading){
            //    It is now OK to place your initialization code here.
            // }
            // or as an alternative, move your initialization code into the AfterConstruction method.
        }


        [Key(AutoGenerate = true)]
        [Persistent()]
        private int ID
        {
            get { return GetPropertyValue<int>("ID"); }
            set { SetPropertyValue("ID", value); }
        }


        [Association("Field-RelatedField")]
        public Field ParentField
        {
            get { return GetPropertyValue<Field>("ParentField"); }
            set { SetPropertyValue("ParentField", value); }
        }

        [NonPersistent()]
        private List<Field> FieldsMatchCurrentProduct
        {
            get
            {
                List<Field> _products = new List<Field>();
                var list = Session.GetObjects(Session.GetClassInfo<Field>(), CriteriaOperator.Parse("ProductString = '" + ParentField.Product.ToString() + "'"), null, 0, 0, true, true);
                foreach (var x in list)
                {
                    bool exclude = false;
                    foreach (RelatedField related in ParentField.RelatedFieldsCollection)
                    {
                        if (related.Field == x || ParentField == x)
                        {
                            exclude = true;
                            break;
                        }
                    }
                    if (!exclude)
                        _products.Add((Field)x);
                }
                return _products;
            }
        }

        [DataSourceProperty("FieldsMatchCurrentProduct")]
        // [DataSourceCriteria("Product = '@This.ParentField.Product'")]
        public Field Field
        {
            get
            {
                return GetPropertyValue<Field>("Field");
            }
            set
            {
                SetPropertyValue("Field", value);
            }
        }

        public bool IsMandatory
        {
            get { return GetPropertyValue<bool>("IsMandatory"); }
            set { SetPropertyValue("IsMandatory", value); }
        }

        protected override void OnSaved()
        {
            base.OnSaved();
            if (!Field.IsRelated)
                Field.IsRelated = true;
            if (ParentField.RelatedFields != ParentField.ReCalculateRelatedFieldsString())
                ParentField.RelatedFields = ParentField.ReCalculateRelatedFieldsString();

            if (BO.ValidateWithoutTicket(ParentField, ContextIdentifier.Save))
                Session.CommitTransaction();
        }

        protected override void OnDeleting()
        {
            //Session sess = new Session();
            //XPCollection<SystemFields> systemFields = new XPCollection<SystemFields>(sess);
            //systemFields.CriteriaString = "FieldName != '" + ParentField.FieldName + "'";

            //bool stillRelated = false;
            //foreach (var sysfield in systemFields)
            //{
            //    foreach (var relatedfield in sysfield.RelatedFieldsCollection)
            //        if (relatedfield.Field == Field)
            //        {
            //            stillRelated = true;
            //            break;
            //        }
            //}
            //if (!stillRelated)
            //    Field.IsRelatedField = false;
            //base.OnDeleting();
        }

        public override void AfterConstruction()
        {
            base.AfterConstruction();
            // Place here your initialization code.
        }
    }

}
