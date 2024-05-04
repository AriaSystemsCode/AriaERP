using Aria5SystemAdmin.Module.Controllers;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class AriaObjectShelve
    {
        public enum ModificationTypes
        {
            Add,
            Modify,
            Delete
        };


     

        protected override void OnSaving()
        {
            bool isNew = Session.IsNewObject(this);

            base.OnSaved();


            if (Session.IsNewObject(this) && ObjectType != null)
            {
                //MMT
                int plength = AriaObjectShelveProperties.Count;
                for (int p = 0; p < plength; p++)
                {
                    AriaObjectShelveProperties[p].Delete();
                }
                foreach (var property in AriaObject.AriaObjectProperties)
                {

                    AriaObjectShelveProperty prop = new AriaObjectShelveProperty(Session);
                    prop.PropertyType = property.PropertyType;
                    if (ModificationType != ModificationTypes.Add && AriaObject != null )
                    {
                      //  var objectRevs = AriaObject.AriaObjectRevisions.OrderByDescending(r => r.ObjectRevision);

                      //  if (objectRevs.Count() > 0)
                        {
                         //   var currentRev = objectRevs.First();
                         //   var objProps = currentRev.AriaObjectProperties.ToList().Where(r => r.PropertyType.Oid == property.PropertyType.Oid);
                          //  if (objProps == null)
                           //     objProps = currentRev.AriaObjectProperties.ToList().Where(r => r.PropertyName != null && r.PropertyName.ToString().ToUpper().Trim() == property.PropertyName.ToString().ToUpper().Trim());


                           // if (objProps.Count() > 0)
                            {

                                //prop.PropertyName = objProps.First().PropertyName;
                                //prop.IsNew = Session.IsNewObject(objProps.First());
                                prop.PropertyName = property.PropertyName;
                                prop.RelatedAriaObject = property.RelatedAriaObject;
                                if (!string.IsNullOrWhiteSpace(prop.PropertyName))
                                prop.IsNew = true;

                            }
                        }
                    }
                    AriaObjectShelveProperties.Add(prop);
                }
                //MMT


                int length = AriaObjectShelveSettings.Count;
                for (int i = 0; i < length; i++)
                {
                    AriaObjectShelveSettings[0].Delete();
                }

                foreach (var settingType in ObjectType.SettingTypes)
                {
                    AriaObjectShelveSetting setting = new AriaObjectShelveSetting(Session);

                    setting.SettingType = settingType;
                    setting.DataType = settingType.DataType.ToString();
                    setting.DecimalPlaces = settingType.Decimal;
                    setting.Width = settingType.Width;
                    //ATA add column name to new objects 8/12/2017   start
                    setting.ActualColumnName = settingType.ActualColumnName;
                    //ATA add column name to new objects 8/12/2017      End
                    try
                    {
                        if (ModificationType != ModificationTypes.Add && AriaObject != null)
                        {
                            var objectRevs = AriaObject.AriaObjectRevisions.OrderByDescending(r => r.ObjectRevision);

                            if (objectRevs.Count() > 0)
                            {
                                var currentRev = objectRevs.First();
                                var objSettings = currentRev.AriaObjectSettings.ToList().Where(r => r.SettingType.Oid == settingType.Oid);
                                if (objSettings.Count() == 0)// (objSettings==null)
                                    objSettings = currentRev.AriaObjectSettings.ToList().Where(r => r.SettingType.ActualColumnName != null && r.SettingType.ActualColumnName.ToString().ToUpper().Trim() == settingType.ActualColumnName.ToString().ToUpper().Trim());


                                if (objSettings.Count() > 0)
                                {
                                    setting.Value = objSettings.First().Value;
                                }
                            }
                        }
                    }
                    catch (Exception e)
                    { }
                    AriaObjectShelveSettings.Add(setting);
                }
            }

        }
    }
}
