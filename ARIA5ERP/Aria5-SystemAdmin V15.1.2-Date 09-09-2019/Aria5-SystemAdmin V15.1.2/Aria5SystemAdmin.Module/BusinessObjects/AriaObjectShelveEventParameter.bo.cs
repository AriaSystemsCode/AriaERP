using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;
using DevExpress.Persistent.Validation;
using Aria5SystemAdmin.Module.Controllers;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class AriaObjectShelveEventParameter
    {

        public enum ModificationTypes
        {
            Add,
            Modify,
            Delete
        }



        private bool _changed = false;

        public override void AfterConstruction()
        {
            base.AfterConstruction();

            IsNew = Session.IsNewObject(this);
        }

        protected override void OnLoaded()
        {
            base.OnLoaded();

            IsNew = Session.IsNewObject(this);
        }

        protected override void OnChanged(string propertyName, object oldValue, object newValue)
        {
            base.OnChanged(propertyName, oldValue, newValue);

            if (!IsLoading && propertyName == "ParameterType" && oldValue != newValue)
            {
                _changed = true;
            }
        }

        protected override void OnSaving()
        {
            base.OnSaving();

            if (_changed == true)
            {
                int length = AriaObjectShelveEventParameterSettings.Count;
                for (int i = 0; i < length; i++)
                {
                    AriaObjectShelveEventParameterSettings[0].Delete();
                }

                if (ParameterType != null)
                {
                    foreach (var settingType in ParameterType.SettingTypes)
                    {
                        AriaObjectShelveEventParameterSetting setting = new AriaObjectShelveEventParameterSetting(Session);

                        setting.SettingType = settingType;
                        setting.DataType = settingType.DataType.ToString();
                        setting.DecimalPlaces = settingType.Decimal;
                        setting.Width = settingType.Width;

                        if (AriaObjectShelveEvent.AriaObjectShelve.AriaObject != null)
                        {
                            var eventPar = AriaObjectShelveEvent.AriaObjectShelve.AriaObject.AriaObjectEvents.Where(r => r.EventName == AriaObjectShelveEvent.EventName).Last().AriaObjectEventParameters; //Select(r => r.AriaObjectMethodParameterSettingses).Last().Where(r => r.SettingType.Oid == settingType.Oid);
                            if (eventPar.Count() > 0)
                            {
                                var paraSettings = eventPar.Last().AriaObjectEventParameterSettings.Where(r => r.SettingType.Oid == settingType.Oid);
                                if (paraSettings.Count() > 0)
                                {
                                    setting.Value = paraSettings.First().Value;
                                }

                            }
                        }

                        AriaObjectShelveEventParameterSettings.Add(setting);
                    }
                }
            }
        }




        [CodeRule]
        public class AriaObjectShelveEventParameterCodeRule : RuleBase<AriaObjectShelveEventParameter>
        {
            protected override bool IsValidInternal(AriaObjectShelveEventParameter target, out string errorMessageTemplate)
            {
                //AriaObjectShelveEventViewController e = new AriaObjectShelveEventViewController();

                if (target.IsNew)
                {
                    var AriaObjectShelveEvent = target.AriaObjectShelveEvent;

                    foreach (AriaObjectShelveEventParameter p in AriaObjectShelveEvent.AriaObjectShelveEventParameters)
                    {
                        if (p.Oid != Guid.Empty && p.Oid != target.Oid && p.ParameterName.ToUpper().Trim() == target.ParameterName.ToUpper().Trim())
                        {
                            errorMessageTemplate = "Can't save, Event Parameter is already exists";
                            return false;
                        }
                    }

                    if (target.AriaObjectShelveEvent.AriaObjectShelve.AriaObject != null)
                    {
                        var AriaObject = target.AriaObjectShelveEvent.AriaObjectShelve.AriaObject;

                        foreach (AriaObjectEventParameter p in AriaObject.AriaObjectEvents.Where(r => r.EventName == target.AriaObjectShelveEvent.EventName).FirstOrDefault().AriaObjectEventParameters)
                        {
                            if (p.Oid != Guid.Empty && p.ParameterName.ToUpper().Trim() == target.ParameterName.ToUpper().Trim())
                            {
                                errorMessageTemplate = "Can't save, Event Parameter is already exists";
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

            public AriaObjectShelveEventParameterCodeRule() : base("", DefaultContexts.Save) { }
            public AriaObjectShelveEventParameterCodeRule(IRuleBaseProperties properties) : base(properties) { }
        }

    }
}
