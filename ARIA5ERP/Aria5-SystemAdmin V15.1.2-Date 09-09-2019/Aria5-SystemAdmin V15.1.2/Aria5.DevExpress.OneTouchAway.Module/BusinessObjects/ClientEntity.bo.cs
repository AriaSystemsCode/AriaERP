using Aria5SystemAdmin.Module;
using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Xpo;
using DevExpress.Xpo.Metadata;
using DevExpress.Xpo.Metadata.Helpers;
using Microsoft.Azure.NotificationHubs;
using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Text;

namespace Aria5.DevExpress.OneTouchAway.Module.BusinessObjects
{

    public partial class ClientEntity
    {
        protected override void OnSaving()
        {
            base.OnSaving();
            

            //Sara.n,1 Call Notification service from backend[Start]
            try
            {
                AriaSecuritySystemUser x = this.Session.FindObject<AriaSecuritySystemUser>(CriteriaOperator.Parse("Oid='" + SecuritySystem.CurrentUserId + "'"));
                if (x != null && x.Account!=null)
                {
                    NotifyFromWeb("SendNotification", x.Account.Oid);
                }
            }
            catch (Exception)
            {
                
               
            }
           
         
            //Sara.n,1 Call Notification service from backend[End]

            if (CallFromSync)
            {
                return;
            }

            DataTable table = new DataTable("ExtraData");
            table.Columns.Add("Name");
            table.Columns.Add("Value");

            foreach (object item in this.ClassInfo.Members)
            {
                if (item is XPMemberInfo && ((XPMemberInfo)item).FindAttributeInfo(typeof(NonPersistentAttribute)) != null)
                {
                    if (((XPMemberInfo)item).Name.ToString().ToUpper() != "IMAGE")
                    {

                        string memberType = "";
                        try
                        {

                            memberType = (((XPMemberInfo)item)).ReferenceType.FullName;
                        }
                        catch (Exception ex)
                        { }
                        if (memberType.ToUpper().Contains("ARIA5SYSTEM") == true)
                        {
                            try
                            {
                                ClientEntity memberValue = new ClientEntity(Session);

                                memberValue = ((ClientEntity)this.GetMemberValue(((XPMemberInfo)item).Name));
                                table.Rows.Add(((XPMemberInfo)item).Name, memberValue.Oid.ToString());
                            }
                            catch { table.Rows.Add(((XPMemberInfo)item).Name, this.GetMemberValue(((XPMemberInfo)item).Name)); }
                        }
                        else
                        {
                            table.Rows.Add(((XPMemberInfo)item).Name, this.GetMemberValue(((XPMemberInfo)item).Name));
                        };



                    };
                }
            }

            StringWriter sw = new StringWriter();
            table.WriteXml(sw);
            this.ExtraData = sw.ToString();
        }

        //Sara.n,1 Call Notification service from backend[Start]
        public void NotifyFromWeb(String txt, Guid account)
        {
            string AzureMobileConstr = @"Endpoint=sb://mobileserviceresto-ns.servicebus.windows.net/;SharedAccessKeyName=DefaultFullSharedAccessSignature;SharedAccessKey=x3TIEN74DpHlKwvS+oZS0a1r52kqZ8u2JqQibzAhvQg=";
            NotificationHubClient hub = NotificationHubClient.CreateClientFromConnectionString(AzureMobileConstr, "mobileserviceresto");
            Notification notification = new WindowsNotification("New Data");
            notification.Headers.Add("X-WNS-Cache-Policy", "cache");
            notification.Headers.Add("X-WNS-Type", "wns/raw");
            notification.ContentType = "application/octet-stream";
            hub.SendNotificationAsync(notification, account.ToString());
           
        }
        //Sara.n,1 Call Notification service from backend[End]

        protected override void OnLoaded()
        {
            base.OnLoaded();
            if (!CallFromSync)
            {
                return;
            }
            if (!string.IsNullOrEmpty(this.ExtraData))
            {


                StringReader sw = new StringReader(this.ExtraData);

                DataTable table = new DataTable("ExtraData");
                table.Columns.Add("Name");
                table.Columns.Add("Value");

                table.ReadXml(sw);

                foreach (object item in this.ClassInfo.Members)
                {
                    if (item is XPMemberInfo && ((XPMemberInfo)item).FindAttributeInfo(typeof(NonPersistentAttribute)) != null)
                    {
                        string propertyName = ((XPMemberInfo)item).Name;

                        if (table.Select("Name = '" + propertyName + "'").Length > 0)
                        {
                            string value = table.Select("Name = '" + propertyName + "'")[0]["Value"].ToString();

                            if (((XPMemberInfo)item).Converter != null && ((XPMemberInfo)item).Converter.GetType().Name == typeof(EnumsConverter).Name)
                            {
                                this.SetMemberValue(propertyName, Enum.Parse(((XPMemberInfo)item).MemberType, value));
                            }
                            else
                            {
                                try
                                {
                                    this.SetMemberValue(propertyName, value);
                                }
                                catch
                                {
                                }

                                try
                                {
                                    this.SetMemberValue(propertyName, Convert.ToByte(value));
                                }
                                catch
                                {

                                }

                                try
                                {
                                    Int16 valueint = Convert.ToInt16(value);
                                    this.SetMemberValue(propertyName, valueint);
                                }
                                catch
                                {

                                }

                                try
                                {
                                    int valueint = Convert.ToInt32(value);
                                    this.SetMemberValue(propertyName, valueint);
                                }
                                catch
                                {

                                }

                                try
                                {
                                    Int64 valueint = Convert.ToInt64(value);
                                    this.SetMemberValue(propertyName, valueint);
                                }
                                catch
                                {

                                }

                                try
                                {
                                    this.SetMemberValue(propertyName, Convert.ToDecimal(value));
                                }
                                catch
                                {
                                }

                                try
                                {
                                    this.SetMemberValue(propertyName, Convert.ToDouble(value));
                                }
                                catch
                                {
                                }

                                try
                                {
                                    this.SetMemberValue(propertyName, Convert.ToDateTime(value));
                                }
                                catch
                                {
                                }

                                try
                                {
                                    this.SetMemberValue(propertyName, Convert.ToBoolean(value));
                                }
                                catch
                                {
                                }
                                try
                                {
                                    //Entity membarvalue = new Entity(Session);

                                    ClientEntity membarvalue = Session.FindObject<ClientEntity>(CriteriaOperator.Parse("[Oid] = '" + value + "'"));
                                    this.SetMemberValue(propertyName, membarvalue);


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



            }
        }
    }
}
