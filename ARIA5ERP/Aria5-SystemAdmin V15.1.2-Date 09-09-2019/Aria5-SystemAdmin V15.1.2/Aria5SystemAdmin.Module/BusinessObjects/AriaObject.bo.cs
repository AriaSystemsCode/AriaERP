using DevExpress.Persistent.Base.ReportsV2;
using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;
using System.Xml.Serialization;
using System.Xml;
using System.Runtime.Serialization;
using System.IO;
using DevExpress.ExpressApp.Web.SystemModule;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class AriaObject
    {
        public string AssignNewRevision()
        {
            if (AriaObjectRevisions.Where(r => String.IsNullOrWhiteSpace(r.ObjectRevision)).Count() > 0)
            {
                // Create new revsion
                var newEmptyRev = new AriaObjectRevision(Session);

                // Get the new revsion number
                var revs = AriaObjectRevisions.Where(r => !String.IsNullOrWhiteSpace(r.ObjectRevision)).OrderByDescending(r => r.ObjectRevision);

                int newRev = 0;

                if (revs.Count() > 0)
                {
                    newRev = int.Parse(revs.First().ObjectRevision);
                }

                string newRevision = (newRev + 1).ToString().PadLeft(6, '0');

                // Assign the new revsion number to the empty revision
                var rev = AriaObjectRevisions.Where(r => String.IsNullOrWhiteSpace(r.ObjectRevision)).First();

                rev.ObjectRevision = newRevision;

                // settings
                if (ObjectType != null && ObjectType.SettingTypes != null)
                {
                    foreach (var settingType in ObjectType.SettingTypes)
                    {
                        AriaObjectSetting setting = new AriaObjectSetting(Session);

                        setting.SettingType = settingType;
                        setting.DataType = settingType.DataType.ToString();
                        setting.DecimalPlaces = settingType.Decimal;
                        //ADD this new field at object type 
                        setting.ActualColumnName = settingType.ActualColumnName;
                        setting.Width = settingType.Width;

                        var objSettings = rev.AriaObjectSettings.Where(r => r.SettingType.Oid == settingType.Oid);

                        if (objSettings.Count() > 0)
                        {
                            setting.Value = objSettings.First().Value;
                        }

                        newEmptyRev.AriaObjectSettings.Add(setting);
                    }
                }

                // Properties
                foreach (var prop in AriaObjectProperties)
                {
                    if (String.IsNullOrWhiteSpace(prop.ObjectRevision))
                    {
                        prop.ObjectRevision = newRevision;
                        prop.Save();
                    }
                }

                //Methods
                foreach (var method in AriaObjectMethods)
                {
                    if (String.IsNullOrWhiteSpace(method.ObjectRevision))
                    {
                        method.ObjectRevision = newRevision;
                        method.Save();
                    }
                }

                //Events
                foreach (var ev in AriaObjectEvents)
                {
                    if (String.IsNullOrWhiteSpace(ev.ObjectRevision))
                    {
                        ev.ObjectRevision = newRevision;
                        ev.Save();
                    }
                }

                AriaObjectRevisions.Add(newEmptyRev);

                ActiveRevision = newRevision;

                Save();

                Session.CommitTransaction();

                return newRevision;
            }

            return "";
        }



        public string AssignNewRevision(TrackingEntry tracking)
        {
            if (AriaObjectRevisions.Where(r => String.IsNullOrWhiteSpace(r.ObjectRevision)).Count() > 0)
            {
                var newEmptyRev = new AriaObjectRevision(Session);

                var rev = AriaObjectRevisions.Where(r => String.IsNullOrWhiteSpace(r.ObjectRevision)).First();

                var revs = AriaObjectRevisions.Where(r => !String.IsNullOrWhiteSpace(r.ObjectRevision)).OrderByDescending(r => r.ObjectRevision);

                int newRev = 0;

                if (revs.Count() > 0)
                {
                    newRev = int.Parse(revs.First().ObjectRevision);
                }

                string newRevision = (newRev + 1).ToString().PadLeft(6, '0');

                rev.ObjectRevision = newRevision;

                // settings
                if (ObjectType != null && ObjectType.SettingTypes != null)
                {
                    foreach (var settingType in ObjectType.SettingTypes)
                    {
                        AriaObjectSetting setting = new AriaObjectSetting(Session);

                        setting.SettingType = settingType;
                        setting.DataType = settingType.DataType.ToString();
                        setting.DecimalPlaces = settingType.Decimal;
                        setting.Width = settingType.Width;

                        var objSettings = rev.AriaObjectSettings.Where(r => r.SettingType.Oid == settingType.Oid);

                        if (objSettings.Count() > 0)
                        {
                            setting.Value = objSettings.First().Value;
                        }

                        newEmptyRev.AriaObjectSettings.Add(setting);
                    }
                }

                // Properties
                foreach (var prop in AriaObjectProperties)
                {
                    if (String.IsNullOrWhiteSpace(prop.ObjectRevision))
                    {
                        prop.ObjectRevision = newRevision;
                        prop.Save();
                    }
                }

                //Methods
                foreach (var method in AriaObjectMethods)
                {
                    if (String.IsNullOrWhiteSpace(method.ObjectRevision))
                    {
                        method.ObjectRevision = newRevision;
                        method.Save();
                    }
                }

                //Events
                foreach (var ev in AriaObjectEvents)
                {
                    if (String.IsNullOrWhiteSpace(ev.ObjectRevision))
                    {
                        ev.ObjectRevision = newRevision;
                        ev.Save();
                    }
                }

                AriaObjectRevisions.Add(newEmptyRev);

                ActiveRevision = newRevision;

                rev.TrackingNo = Session.GetObjectByKey<TrackingEntry>(tracking.Oid);

                rev.Save();

                Save();
                //MMT
                //Session.CommitTransaction();
                //MMT
                return newRevision;
            }

            return "";
        }

        [DevExpress.Persistent.Base.Action]
        public void CreateMetaData()
        {
            int max = 0;

            foreach (AriaObjectRevision rev in AriaObjectRevisions.Where(r => !string.IsNullOrWhiteSpace(r.ObjectRevision)))
            {
                if (max < int.Parse(rev.ObjectRevision))
                {
                    max = int.Parse(rev.ObjectRevision);
                }
            }
            //AriaObjectRevision revision = new AriaObjectRevision();
            //if (max > 1)
            //{
            //    AriaObjectRevision revision = AriaObjectRevisions.Where(r => !string.IsNullOrWhiteSpace(r.ObjectRevision)).Where(r => int.Parse(r.ObjectRevision) == max - 1).ToList()[0];
            //    SpliteMetadata(this, revision);
            //}
            //else
            //{
            AriaObjectRevision revision;

            if (AriaObjectRevisions.Where(r => !string.IsNullOrWhiteSpace(r.ObjectRevision)).Where(r => int.Parse(r.ObjectRevision) == max).Count() == 0)
            {
                revision = AriaObjectRevisions[0];
            }
            else
            {
                revision = AriaObjectRevisions.Where(r => !string.IsNullOrWhiteSpace(r.ObjectRevision)).Where(r => int.Parse(r.ObjectRevision) == max).ToList()[0];
            }


            try
            {
                //System.IO.File.WriteAllText(@"D:\XML.XML", SpliteMetadata(Oid, revision.Oid));
                string metadata = SpliteMetadata(Oid, revision.Oid);
                List<byte> bytes = new List<byte>();
                foreach (var ch in metadata.ToCharArray())
                {
                    bytes.Add(Convert.ToByte(ch));
                }

                MemoryStream stream = new MemoryStream(bytes.ToArray());
              
                ResponseWriter.WriteFileToResponse(stream, "Metadata.XML");

            }
            catch (Exception e)
            { }

            
            //}
        }


        public List<AriaObjectProperty> GetRevisionProperties(Guid ariaObjectOid, Guid revisionOid)
        {
            AriaObject ariaObject = Session.GetObjectByKey<AriaObject>(ariaObjectOid);
            AriaObjectRevision revision = Session.GetObjectByKey<AriaObjectRevision>(revisionOid);

            int max;
            int min;
            if (string.IsNullOrWhiteSpace(revision.ObjectRevision))
            {
                max = 0;
                min = 0;
            }
            else
            {
                max = int.Parse(revision.ObjectRevision);
                min = int.Parse(revision.ObjectRevision);

                // Get minimum revision
                foreach (AriaObjectRevision rev in ariaObject.AriaObjectRevisions.Where(r => !string.IsNullOrWhiteSpace(r.ObjectRevision)))
                {
                    if (min > int.Parse(rev.ObjectRevision))
                    {
                        min = int.Parse(rev.ObjectRevision);
                    }
                }
            }

            List<AriaObjectProperty> AriaObjectProperties = new List<AriaObjectProperty>();

            //loop from minimum to maximum revision
            for (int i = max; i <= max; i++)
            {
                AriaObjectRevision objectRevision;
                if (ariaObject.AriaObjectRevisions.Where(r => !string.IsNullOrWhiteSpace(r.ObjectRevision)).Count() == 0)
                {
                    objectRevision = ariaObject.AriaObjectRevisions[0];
                }
                else
                {
                    objectRevision = ariaObject.AriaObjectRevisions.Where(r => !string.IsNullOrWhiteSpace(r.ObjectRevision)).Where(r => int.Parse(r.ObjectRevision) == i).ToList()[0];
                }

                //Loop on the properties in one revision
                foreach (AriaObjectProperty property in objectRevision.AriaObjectProperties)
                {
                    property.AriaObjectPropertiesSettingsArray = property.AriaObjectPropertiesSettings.ToArray();

                    //Property added
                    if (property.ModificationType == AriaObjectProperty.modificationType.Add)
                    {
                        AriaObjectProperties.Add(property);
                    }
                    //property deleted
                    else if (property.ModificationType == AriaObjectProperty.modificationType.Delete)
                    {
                        AriaObjectProperties.Remove(property);
                    }
                    //Property modified
                    else if (property.ModificationType == AriaObjectProperty.modificationType.Modify)
                    {
                        AriaObjectProperty prop = AriaObjectProperties.Where(r => r.PropertyName == property.PropertyName).ToList()[0];
                        AriaObjectProperties.Remove(prop);
                        AriaObjectProperties.Add(property);
                    }
                }

            }
            return AriaObjectProperties;
        }

        public List<AriaObjectMethod> GetRevisionMethods(Guid ariaObjectOid, Guid revisionOid)
        {
            AriaObject ariaObject = Session.GetObjectByKey<AriaObject>(ariaObjectOid);
            AriaObjectRevision revision = Session.GetObjectByKey<AriaObjectRevision>(revisionOid);

            int max;
            int min;
            if (string.IsNullOrWhiteSpace(revision.ObjectRevision))
            {
                max = 0;
                min = 0;
            }
            else
            {
                max = int.Parse(revision.ObjectRevision);
                min = int.Parse(revision.ObjectRevision);

                // Get minimum revision
                foreach (AriaObjectRevision rev in ariaObject.AriaObjectRevisions.Where(r => !string.IsNullOrWhiteSpace(r.ObjectRevision)))
                {
                    if (min > int.Parse(rev.ObjectRevision))
                    {
                        min = int.Parse(rev.ObjectRevision);
                    }
                }
            }

            List<AriaObjectMethod> AriaObjectMethods = new List<AriaObjectMethod>();
            List<AriaObjectMethodParameter> AriaObjectMethodParameters = new List<AriaObjectMethodParameter>();
            //          List<AriaObjectMethodParameterSetting> AriaObjectMethodParameterSettings = new List<AriaObjectMethodParameterSetting>();



            //loop from minimum to maximum revision
            for (int i = max; i <= max; i++)
            {
                AriaObjectRevision objectRevision;
                if (ariaObject.AriaObjectRevisions.Where(r => !string.IsNullOrWhiteSpace(r.ObjectRevision)).Count() == 0)
                {
                    objectRevision = ariaObject.AriaObjectRevisions[0];
                }
                else
                {
                    objectRevision = ariaObject.AriaObjectRevisions.Where(r => !string.IsNullOrWhiteSpace(r.ObjectRevision)).Where(r => int.Parse(r.ObjectRevision) == i).ToList()[0];
                }

                //Loop on the methods in one revision
                foreach (AriaObjectMethod method in objectRevision.AriaObjectMethods)
                {

                    switch (method.ModificationType)
                    {
                        //method added
                        case AriaObjectMethod.modificationType.Add:
                            foreach (AriaObjectMethodParameter methodParam in method.AriaObjectMethodParameters)
                            {
                                methodParam.AriaObjectMethodParameterSettingsArray = methodParam.AriaObjectMethodParameterSettings.ToArray();
                                AriaObjectMethodParameters.Add(methodParam);

                            }
                            method.AriaObjectMethodParametersArray = AriaObjectMethodParameters.ToArray();
                            AriaObjectMethods.Add(method);
                            break;

                        //method deleted
                        case AriaObjectMethod.modificationType.Delete:
                            AriaObjectMethods.Remove(method);
                            break;

                        //method modified
                        case AriaObjectMethod.modificationType.Modify:
                            foreach (AriaObjectMethodParameter methodParam in method.AriaObjectMethodParameters)
                            {
                                methodParam.AriaObjectMethodParameterSettingsArray = methodParam.AriaObjectMethodParameterSettings.ToArray();
                                switch (methodParam.ModificationType)
                                {
                                    case AriaObjectMethodParameter.modificationType.Add:
                                        AriaObjectMethodParameters.Add(methodParam);
                                        break;
                                    case AriaObjectMethodParameter.modificationType.Delete:
                                        AriaObjectMethodParameters.Remove(methodParam);
                                        break;
                                    case AriaObjectMethodParameter.modificationType.Modify:
                                        AriaObjectMethodParameter methParam = AriaObjectMethodParameters.Where(r => r.ParameterName == methodParam.ParameterName).ToList()[0];
                                        AriaObjectMethodParameters.Remove(methParam);
                                        AriaObjectMethodParameters.Add(methodParam);
                                        break;

                                }


                            }
                            method.AriaObjectMethodParametersArray = AriaObjectMethodParameters.ToArray();
                            //AriaObjectMethods.Add(method);
                            AriaObjectMethod meth = AriaObjectMethods.Where(r => r.MethodName == method.MethodName).ToList()[0];
                            AriaObjectMethods.Remove(meth);
                            AriaObjectMethods.Add(method);
                            break;
                    }


                }

            }
            return AriaObjectMethods;
        }


        public List<AriaObjectEvent> GetRevisionEvents(Guid ariaObjectOid, Guid revisionOid)
        {
            AriaObject ariaObject = Session.GetObjectByKey<AriaObject>(ariaObjectOid);
            AriaObjectRevision revision = Session.GetObjectByKey<AriaObjectRevision>(revisionOid);

            int max;
            int min;
            if (string.IsNullOrWhiteSpace(revision.ObjectRevision))
            {
                max = 0;
                min = 0;
            }
            else
            {
                max = int.Parse(revision.ObjectRevision);
                min = int.Parse(revision.ObjectRevision);

                // Get minimum revision
                foreach (AriaObjectRevision rev in ariaObject.AriaObjectRevisions.Where(r => !string.IsNullOrWhiteSpace(r.ObjectRevision)))
                {
                    if (min > int.Parse(rev.ObjectRevision))
                    {
                        min = int.Parse(rev.ObjectRevision);
                    }
                }
            }

            List<AriaObjectEvent> AriaObjectEvents = new List<AriaObjectEvent>();
            List<AriaObjectEventParameter> AriaObjectEventParameters = new List<AriaObjectEventParameter>();


            //loop from minimum to maximum revision
            for (int i = max; i <= max; i++)
            {
                AriaObjectRevision objectRevision;
                if (ariaObject.AriaObjectRevisions.Where(r => !string.IsNullOrWhiteSpace(r.ObjectRevision)).Count() == 0)
                {
                    objectRevision = ariaObject.AriaObjectRevisions[0];
                }
                else
                {
                    objectRevision = ariaObject.AriaObjectRevisions.Where(r => !string.IsNullOrWhiteSpace(r.ObjectRevision)).Where(r => int.Parse(r.ObjectRevision) == i).ToList()[0];
                }

                //Loop on the events in one revision
                foreach (AriaObjectEvent ev in objectRevision.AriaObjectEvents)
                {

                    switch (ev.ModificationType)
                    {
                        //method added
                        case AriaObjectEvent.modificationType.Add:
                            foreach (AriaObjectEventParameter evParam in ev.AriaObjectEventParameters)
                            {
                                evParam.AriaObjectEventParameterSettingsArray = evParam.AriaObjectEventParameterSettings.ToArray();
                                AriaObjectEventParameters.Add(evParam);

                            }
                            ev.AriaObjectEventParametersArray = AriaObjectEventParameters.ToArray();
                            AriaObjectEvents.Add(ev);
                            break;

                        //method deleted
                        case AriaObjectEvent.modificationType.Delete:
                            AriaObjectEvents.Remove(ev);
                            break;

                        //method modified
                        case AriaObjectEvent.modificationType.Modify:
                            foreach (AriaObjectEventParameter evParam in ev.AriaObjectEventParameters)
                            {
                                evParam.AriaObjectEventParameterSettingsArray = evParam.AriaObjectEventParameterSettings.ToArray();
                                switch (evParam.ModificationType)
                                {
                                    case AriaObjectEventParameter.modificationType.Add:
                                        AriaObjectEventParameters.Add(evParam);
                                        break;
                                    case AriaObjectEventParameter.modificationType.Delete:
                                        AriaObjectEventParameters.Remove(evParam);
                                        break;
                                    case AriaObjectEventParameter.modificationType.Modify:
                                        AriaObjectEventParameter eParam = AriaObjectEventParameters.Where(r => r.ParameterName == evParam.ParameterName).ToList()[0];
                                        AriaObjectEventParameters.Remove(eParam);
                                        AriaObjectEventParameters.Add(evParam);
                                        break;

                                }


                            }
                            ev.AriaObjectEventParametersArray = AriaObjectEventParameters.ToArray();
                            //AriaObjectMethods.Add(method);
                            AriaObjectEvent e = AriaObjectEvents.Where(r => r.EventName == ev.EventName).ToList()[0];
                            AriaObjectEvents.Remove(e);
                            AriaObjectEvents.Add(ev);
                            break;
                    }


                }

            }
            return AriaObjectEvents;
        }


        public string SpliteMetadata(Guid ariaObjectOid, Guid revisionOid)
        {
            AriaObject ariaObject = Session.GetObjectByKey<AriaObject>(ariaObjectOid);
            AriaObjectRevision revision = Session.GetObjectByKey<AriaObjectRevision>(revisionOid);

            List<AriaObjectProperty> properties = GetRevisionProperties(ariaObjectOid, revisionOid);
            List<AriaObjectMethod> methods = GetRevisionMethods(ariaObjectOid, revisionOid);
            List<AriaObjectEvent> events = GetRevisionEvents(ariaObjectOid, revisionOid);
            List<AriaObjectSetting> settings = revision.AriaObjectSettings.ToList();
            List<AriaObject> children = ariaObject.GetChildren.ToList();
            //List<AriaObjectRevision> childrenRevisions = new List<AriaObjectRevision>();



            foreach (AriaObject obj in children)
            {
                //ObjectChidren child = new ObjectChidren();
                //child.ObjectChidrenOid = obj.Oid;
                foreach (AriaObjectRevision rev in obj.AriaObjectRevisions.Where(r => !string.IsNullOrWhiteSpace(r.ObjectRevision)).OrderByDescending(r => r.ObjectRevision))
                {
                    //if (rev.TrackingNo == revision.TrackingNo)
                    {
                        obj.AriaObjectPropertysArray = GetRevisionProperties(obj.Oid, rev.Oid).ToArray(); ;
                        obj.AriaObjectMethodsArray = GetRevisionMethods(obj.Oid, rev.Oid).ToArray();
                        obj.AriaObjectEventsArray = GetRevisionEvents(obj.Oid, rev.Oid).ToArray();
                        obj.AriaObjectSettingsArray = rev.AriaObjectSettings.ToList().ToArray();

                    }
                    break;
                }
                //objectChildren.Add(child);
            }
            ariaObject.AriaObjectEventsArray = events.ToArray();
            ariaObject.AriaObjectMethodsArray = methods.ToArray();
            ariaObject.AriaObjectPropertysArray = properties.ToArray();
            ariaObject.AriaObjectSettingsArray = settings.ToArray();
            ariaObject.GetChildrenArray = children.ToArray();

            XmlSerializer xmlSerializer = new XmlSerializer(typeof(AriaObject));

            //Created the StringWriterUtf8 class to just overwrite the encoding to return UTF8 instead of the UTF16 default encoding
            StringWriterUtf8 text = new StringWriterUtf8();

            //This to omit the xmlns attribute as it makes problems while deserializing.
            XmlSerializerNamespaces xmlnsEmpty = new XmlSerializerNamespaces(new[]
            {
                new XmlQualifiedName(string.Empty, string.Empty),
            });

            xmlSerializer.Serialize(text, ariaObject, xmlnsEmpty);

            return text.ToString();

        }

        private class StringWriterUtf8 : System.IO.StringWriter
        {
            public override Encoding Encoding
            {
                get
                {
                    return Encoding.UTF8;
                }
            }
        }


    }
}
