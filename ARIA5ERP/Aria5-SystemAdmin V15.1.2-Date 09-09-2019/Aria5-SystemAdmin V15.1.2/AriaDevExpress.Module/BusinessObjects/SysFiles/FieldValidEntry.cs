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
    [RuleCriteria("WidthCheck", DefaultContexts.Save, "Len(Code) <= Field.Width", "Code length cann't exceed field width!")]
    [DevExpress.Xpo.Custom("Caption", "Field Valid Entry")]
    [Persistent("SYDFIELD_ValidEntry")]
    public class FieldValidEntry : XPLiteObject, IAuditable
    {
        public FieldValidEntry(Session session)
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
        [Browsable(false)]
        public int ID
        {
            get { return GetPropertyValue<int>("ID"); }
            set { SetPropertyValue("ID", value); }
        }

        [Association("Field-ValidEntry")]
        public Field Field
        {
            get { return GetPropertyValue<Field>("Field"); }
            set { SetPropertyValue("Field", value); }
        }

        public string Name
        {
            get { return GetPropertyValue<string>("Name"); }
            set { SetPropertyValue("Name", value); }
        }

        [Size(100)]
        public string Code
        {
            get { return GetPropertyValue<string>("Code"); }
            set { SetPropertyValue("Code", value); }
        }

        protected override void OnSaved()
        {
            base.OnSaved();
            Field.ValidEntries = Field.ReCalculateValidEntries();
            //bool valid = true;
            //foreach (RuleBase rule in Validator.RuleSet)
            //{
            //    if (!rule.UsedProperties.Contains("Ticket"))
            //        valid = valid && rule.Validate(Field).State == ValidationState.Valid;
            //}
            //if (BO.ValidateWithoutTicket(Field, ContextIdentifier.Save))
            //    Field.Session.CommitTransaction();
        }

        public override void AfterConstruction()
        {
            base.AfterConstruction();
            // Place here your initialization code.
        }


        #region Audit Trail

        [NonPersistent()]
        [RuleRequiredField(CustomMessageTemplate = AriaAudit.TicketRequiredMessage), Browsable(false)]
        public string Ticket
        {
            get { return Field.Ticket; }
            set { Field.Ticket = value; }
        }


        [NonPersistent()]
        private string TrackingEntry
        {
            get { return Field.TrackingEntry; }
            set { Field.TrackingEntry = value; }
        }

        [NonPersistent()]
        private string Comments
        {
            get { return Field.Comments; }
            set { Field.Comments = value; }
        }

        #endregion
    }

}