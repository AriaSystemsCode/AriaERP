using Aria5SystemAdmin.Module.Controllers;
using DevExpress.ExpressApp;
using DevExpress.ExpressApp.ConditionalAppearance;
using DevExpress.Persistent.Validation;
using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;


namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class AriaObjectShelveProperty
    {
        public enum ModificationTypes
        {
            Add,
            Modify,
            Delete
        };

      

        private bool _changed = false;

        public override void AfterConstruction()
        {
            base.AfterConstruction();
            //MMT
            // IsNew = Session.IsNewObject(this);//MMT
            a = this;
            
        }

        protected override void OnLoaded()
        {
            base.OnLoaded();
            //mmt
           // IsNew = Session.IsNewObject(this);//mmt
        }

        protected override void OnChanged(string propertyName, object oldValue, object newValue)
        {
            base.OnChanged(propertyName, oldValue, newValue);

            if (!IsLoading && propertyName == "PropertyType" && oldValue != newValue)
            {
                _changed = true;
            }
        }

        

        public bool CannotModify()
        {
            if (AriaObjectShelveViewController.CurrentAriaObjectShelve != Guid.Empty)
            {
                AriaObjectShelve shelve = Session.GetObjectByKey<AriaObjectShelve>(AriaObjectShelveViewController.CurrentAriaObjectShelve);
                if (shelve.TrackingEntry != null)
                {
                    return shelve.TrackingEntry.Status == TrackingEntry.TrackingStatus.Complete;
                }
            }

            return false;
        }


        public static AriaObjectShelveProperty a;

        protected override void OnDeleting()
        {
            base.OnDeleting();
            if (AriaObjectShelveViewController.CurrentAriaObjectShelve != Guid.Empty)
            {
                AriaObjectShelve shelve = Session.GetObjectByKey<AriaObjectShelve>(AriaObjectShelveViewController.CurrentAriaObjectShelve);
                if (shelve.TrackingEntry != null && shelve.TrackingEntry.Status == TrackingEntry.TrackingStatus.Complete)
                {
                    throw new Exception("Can't Delete, TrackingEntry is marked as complete");
                }
            }
        }

        protected override void OnSaving()
        {
            //MMT
            bool isNew = Session.IsNewObject(this);
            this.IsNew = this.IsNew || isNew;
            //mmt
            base.OnSaving();

            if (AriaObjectShelveViewController.CurrentAriaObjectShelve != Guid.Empty)
            {
                AriaObjectShelve shelve = Session.GetObjectByKey<AriaObjectShelve>(AriaObjectShelveViewController.CurrentAriaObjectShelve);
                if (shelve.TrackingEntry != null && shelve.TrackingEntry.Status == TrackingEntry.TrackingStatus.Complete)
                {
                    throw new Exception("Can't Save, TrackingEntry is marked as complete");
                }

                else
                {
                    if (_changed == true)
                    {
                        int length = AriaObjectShelvePropertySettings.Count;
                        for (int i = 0; i < length; i++)
                        {
                            AriaObjectShelvePropertySettings[0].Delete();
                        }

                        if (PropertyType != null)
                        {
                            foreach (var settingType in PropertyType.SettingTypes)
                            {
                                AriaObjectShelvePropertySetting setting = new AriaObjectShelvePropertySetting(Session);

                                setting.SettingType = settingType;
                                setting.DataType = settingType.DataType.ToString();
                                setting.DecimalPlaces = settingType.Decimal;
                                setting.Width = settingType.Width;

                                if (AriaObjectShelve.AriaObject != null)
                                {
                                    var props = AriaObjectShelve.AriaObject.AriaObjectProperties.OrderByDescending(r => r.ObjectRevision);

                                    if (props.Count() > 0)
                                    {
                                        if (string.IsNullOrWhiteSpace(props.Last().ObjectRevision))
                                        {
                                            var propSettings = props.Last().AriaObjectPropertiesSettings.Where(r => r.SettingType.Oid == settingType.Oid);

                                            if (propSettings.Count() > 0)
                                            {
                                                setting.Value = propSettings.First().Value;
                                            }
                                        }
                                        else
                                        {
                                            var propSettings = props.First().AriaObjectPropertiesSettings.Where(r => r.SettingType.Oid == settingType.Oid);

                                            if (propSettings.Count() > 0)
                                            {
                                                setting.Value = propSettings.First().Value;
                                            }
                                        }
                                    }
                                }
                                AriaObjectShelvePropertySettings.Add(setting);
                            }
                        }
                    }
                }
            }
            
        }

        [CodeRule]
        public class AriaObjectShelvePropertyCodeRule : RuleBase<AriaObjectShelveProperty>
        {
            protected override bool IsValidInternal(AriaObjectShelveProperty target, out string errorMessageTemplate)
            {
               

                        AriaObjectShelveViewController e = new AriaObjectShelveViewController();

                        if (e.TargetViewType == ViewType.DetailView && target.IsNew)
                        {
                            var AriaObjectShelve = target.AriaObjectShelve;

                            foreach (AriaObjectShelveProperty p in AriaObjectShelve.AriaObjectShelveProperties)
                            {
                                if (p.Oid != Guid.Empty && p.PropertyName.ToUpper().Trim() == target.PropertyName.ToUpper().Trim())
                                {
                                    errorMessageTemplate = "Can't save, property is already exists";
                                    return false;
                                }
                            }

                            if (target.AriaObjectShelve.AriaObject != null)
                            {
                                var AriaObject = target.AriaObjectShelve.AriaObject;

                                foreach (AriaObjectProperty p in AriaObject.AriaObjectProperties)
                                {
                                    if (p.Oid != Guid.Empty && p.PropertyName.ToUpper().Trim() == target.PropertyName.ToUpper().Trim())
                                    {
                                        errorMessageTemplate = "Can't save, property is already exists";
                                        return false;
                                    }
                                }
                            }

                            errorMessageTemplate = "";
                            return true;
                        }

                        errorMessageTemplate = "";
                        return true;
                    
            }

            public AriaObjectShelvePropertyCodeRule() : base("", DefaultContexts.Save) { }
            public AriaObjectShelvePropertyCodeRule(IRuleBaseProperties properties) : base(properties) { }
        }

    }
}
