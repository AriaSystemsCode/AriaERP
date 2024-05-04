using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;
using DevExpress.Persistent.Validation;
using Aria5SystemAdmin.Module.Controllers;
using DevExpress.ExpressApp;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class AriaObjectShelveMethodParameter
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
                        int length = AriaObjectShelveMethodParameterSettings.Count;
                        for (int i = 0; i < length; i++)
                        {
                            AriaObjectShelveMethodParameterSettings[0].Delete();
                        }

                        if (ParameterType != null)
                        {
                            foreach (var settingType in ParameterType.SettingTypes)
                            {
                                AriaObjectShelveMethodParameterSetting setting = new AriaObjectShelveMethodParameterSetting(Session);

                                setting.SettingType = settingType;
                                setting.DataType = settingType.DataType.ToString();
                                setting.DecimalPlaces = settingType.Decimal;
                                setting.Width = settingType.Width;
                                if (AriaObjectShelveMethod.AriaObjectShelve.AriaObject != null)
                                {
                                    var methodPar = AriaObjectShelveMethod.AriaObjectShelve.AriaObject.AriaObjectMethods.Where(r => r.MethodName == AriaObjectShelveMethod.MethodName).Last().AriaObjectMethodParameters; //Select(r => r.AriaObjectMethodParameterSettingses).Last().Where(r => r.SettingType.Oid == settingType.Oid);
                                    if (methodPar.Count() > 0)
                                    {
                                        var paraSettings = methodPar.Last().AriaObjectMethodParameterSettings.Where(r => r.SettingType.Oid == settingType.Oid);
                                        if (paraSettings.Count() > 0)
                                        {
                                            setting.Value = paraSettings.First().Value;
                                        }

                                    }
                                }
                                AriaObjectShelveMethodParameterSettings.Add(setting);
                            }
                        }
                    }
                }
            }
        }




        [CodeRule]
        public class AriaObjectShelveMethodParameterCodeRule : RuleBase<AriaObjectShelveMethodParameter>
        {
            protected override bool IsValidInternal(AriaObjectShelveMethodParameter target, out string errorMessageTemplate)
            {
                //AriaObjectShelveMethodViewController e = new AriaObjectShelveMethodViewController();

                if ( target.IsNew)
                {
                    var AriaObjectShelveMethod = target.AriaObjectShelveMethod;

                    foreach (AriaObjectShelveMethodParameter p in AriaObjectShelveMethod.AriaObjectShelveMethodParameters)
                    {
                        if (p.Oid != Guid.Empty && p.Oid != target.Oid && p.ParameterName.ToUpper().Trim() == target.ParameterName.ToUpper().Trim())
                        {
                            errorMessageTemplate = "Can't save, Method Parameter is already exists";
                            return false;
                        }
                    }

                    if (target.AriaObjectShelveMethod.AriaObjectShelve.AriaObject != null)
                    {
                        Guid AriaObject = target.AriaObjectShelveMethod.AriaObjectShelve.AriaObject.Oid;
                        AriaObject ariaObject = new BusinessObjects.AriaObject(target.Session);
                        ariaObject = target.Session.GetObjectByKey<AriaObject>(AriaObject);
                        foreach (AriaObjectMethodParameter p in ariaObject.AriaObjectMethods.Where(r=>r.MethodName == target.AriaObjectShelveMethod.MethodName).FirstOrDefault().AriaObjectMethodParameters)
                        {
                            if (p.Oid != Guid.Empty && p.ParameterName.ToUpper().Trim() == target.ParameterName.ToUpper().Trim())
                            {
                                errorMessageTemplate = "Can't save, Method Parameter is already exists";
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

            public AriaObjectShelveMethodParameterCodeRule() : base("", DefaultContexts.Save) { }
            public AriaObjectShelveMethodParameterCodeRule(IRuleBaseProperties properties) : base(properties) { }
        }
    }
}
