using System;
using System.Collections.Generic;
using System.Text;
using DevExpress.Xpo.Metadata;
using System.Data;
using DevExpress.Xpo;
using System.IO;
using DevExpress.Data.Mask;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp.Security;
using DevExpress.ExpressApp;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class Entity
    {
        protected override void OnSaving()
        {
            base.OnSaving();

            DataTable table = new DataTable("ExtraData");
            table.Columns.Add("Name");
            table.Columns.Add("Value");

            foreach (object item in ClassInfo.Members)
            {
                if (item is XPMemberInfo && ((XPMemberInfo)item).FindAttributeInfo(typeof(NonPersistentAttribute)) != null)
                {
                    if ( ((XPMemberInfo)item).Name.ToString().ToUpper()  != "IMAGE"){

                        string memberType = "";
                        try
                        {

                            memberType = (((XPMemberInfo)item)).ReferenceType.FullName ;
                        }
                        catch (Exception ex)
                        {}
                        if (memberType.ToUpper().Contains("ARIA5SYSTEM") == true)
                        {
                            try
                            {
                                Entity memberValue = new Entity(Session);

                                memberValue = ((Entity)GetMemberValue(((XPMemberInfo)item).Name));
                                table.Rows.Add(((XPMemberInfo)item).Name, memberValue.Oid.ToString());
                            }
                            catch { table.Rows.Add(((XPMemberInfo)item).Name, GetMemberValue(((XPMemberInfo)item).Name)); }
                        }
                        else
                        {
                            table.Rows.Add(((XPMemberInfo)item).Name, GetMemberValue(((XPMemberInfo)item).Name));
                        };
                        
                        
                        
                        };
                }
            }

            StringWriter sw = new StringWriter();
            table.WriteXml(sw);
            ExtraData = sw.ToString();

            //Mina.B 2015-09-30 [Begin]
              string userName = string.Empty;
            Attribute divisionAttribute = ClassInfo.FindAttributeInfo("DivisionAttribute");

           // Sara.N Trace webservice 3-11-2015[Start]
           
            if ( ((AuthenticationStandardLogonParameters)SecuritySystem.LogonParameters)!=null)
            {
                userName = ((AuthenticationStandardLogonParameters)SecuritySystem.LogonParameters).UserName;
                CriteriaOperator criteriaUser = CriteriaOperator.Parse("UserName = '" + userName + "'");
                AriaSecuritySystemUser user = Session.FindObject<AriaSecuritySystemUser>(criteriaUser);
              
                 if (divisionAttribute != null && ((DivisionAttribute)divisionAttribute).ContainDivision == true && user.Division != null)
                 {
                     Division = user.Division.Oid;
                 }
            }
            //Sara.N Trace webservice 3-11-2015[End]
           //Mina.B 2015-09-30 [End]
            }
        protected override void OnLoaded()
        {
            base.OnLoaded();
            if (ExtraData.Length >25)
            {
                StringReader sw = new StringReader(ExtraData);

                DataTable table = new DataTable("ExtraData");
                table.Columns.Add("Name");
                table.Columns.Add("Value");

                table.ReadXml(sw);

                foreach (object item in ClassInfo.Members)
                {
                    if (item is XPMemberInfo && ((XPMemberInfo)item).FindAttributeInfo(typeof(NonPersistentAttribute)) != null)
                    {
                        string propertyName = ((XPMemberInfo)item).Name;

                        if (table.Select("Name = '" + propertyName + "'").Length > 0)
                        {
                            string value = table.Select("Name = '" + propertyName + "'")[0]["Value"].ToString();

                            if (((XPMemberInfo)item).Converter != null && ((XPMemberInfo)item).Converter.GetType().Name == typeof(DevExpress.Xpo.Metadata.Helpers.EnumsConverter).Name)
                            {
                                SetMemberValue(propertyName, Enum.Parse(((XPMemberInfo)item).MemberType, value));
                            }
                            else
                            {
                                try
                                {
                                    SetMemberValue(propertyName, value);
                                }
                                catch
                                {
                                }

                                try
                                {
                                    SetMemberValue(propertyName, Convert.ToByte(value));
                                }
                                catch
                                {

                                }

                                try
                                {
                                    Int16 valueint = Convert.ToInt16(value);
                                    SetMemberValue(propertyName, valueint);
                                }
                                catch
                                {

                                }

                                try
                                {
                                    int valueint = Convert.ToInt32(value);
                                    SetMemberValue(propertyName, valueint);
                                }
                                catch
                                {

                                }

                                try
                                {
                                    Int64 valueint = Convert.ToInt64(value);
                                    SetMemberValue(propertyName, valueint);
                                }
                                catch
                                {

                                }

                                try
                                {
                                    SetMemberValue(propertyName, Convert.ToDecimal(value));
                                }
                                catch
                                {
                                }

                                try
                                {
                                    SetMemberValue(propertyName, Convert.ToDouble(value));
                                }
                                catch
                                {
                                }

                                try
                                {
                                    SetMemberValue(propertyName, Convert.ToDateTime(value));
                                }
                                catch
                                {
                                }

                                try
                                {
                                    SetMemberValue(propertyName, Convert.ToBoolean(value));
                                }
                                catch
                                {
                                }
                                try
                                {
                                    //Entity membarvalue = new Entity(Session);

                                    Entity membarvalue = Session.FindObject<Entity>(CriteriaOperator.Parse("[Oid] = '" + value + "'"));
                                    SetMemberValue(propertyName, membarvalue);


                                }
                                catch
                                {
                                }
                            }
                        }
                    }
                }

                // Wael
                // 1) Search the Attachment table for the Entity.DefaultAttachment
                // 2) Compare the attachment file with the name as Attachment.Name located in the Attachment.Location with 
                // the attachmanet file located in the folder Attachmanets and has a name same as the field Attachment.ID for the current record
                // If they are different, copy the attachment file Attachment.Location\Attachment.Name to the Attacments folder with a name same as Attachment.ID
                // 3) Use the method Attachment.LoadFromStream() to load the attachment in the Attachments folder

                // load the attachment
                try
                {
                    bool imageFound = false;
                    if (DefaultAttachment != null)
                    {
                        if (DefaultAttachment.AttachmentFile != null)
                        {
                            IMAGE = DefaultAttachment.AttachmentFile;
                            imageFound = true;
                        };
                    };
                    if (EntityAttachments.Count > 0)
                    {
                        for (int i = 0; i < EntityAttachments.Count; i++)
                        {
                            if (EntityAttachments[i].IsDefault == true)
                            {

                                IMAGE = EntityAttachments[i].Attachment.AttachmentFile;
                                imageFound = true;
                            }

                        }
                    }

                    if (EntityAttachments[0] != null && imageFound == false)
                    {
                        if (EntityAttachments[0].Attachment != null)
                        {
                            IMAGE = EntityAttachments[0].Attachment.AttachmentFile;
                        };
                    };

                }
                catch (Exception ex)
                { };



            

        }
            }
    }
}
