// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.ShipService
// Assembly: UPS, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null
// MVID: FE2C527D-E7B1-4D14-9A59-DD5F5EB261A9
// Assembly location: D:\Aria4xp\DLLS\UPS.dll

using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Net;
using System.Text;
using System.Threading;
using System.Web.Services;
using System.Web.Services.Description;
using System.Web.Services.Protocols;
using System.Xml.Serialization;
using UPS.Properties;


namespace UPS.UPSShip
{
    [WebServiceBinding(Name = "ShipBinding", Namespace = "http://www.ups.com/WSDL/XOLTWS/Ship/v1.0")]
    [DesignerCategory("code")]
    [XmlInclude(typeof (CompanyInfoType))]
    [DebuggerStepThrough]
    [XmlInclude(typeof (ShipmentServiceOptionsType))]
    [XmlInclude(typeof (CompanyInfoType1))]
    [GeneratedCode("System.Web.Services", "4.6.1087.0")]
    public class ShipService : SoapHttpClientProtocol
    {
      private UPSSecurity uPSSecurityValueField;
      private SendOrPostCallback ProcessShipmentOperationCompleted;
      private SendOrPostCallback ProcessShipConfirmOperationCompleted;
      private SendOrPostCallback ProcessShipAcceptOperationCompleted;
      private bool useDefaultCredentialsSetExplicitly;
      public string ClientId { get; set; }
        public string ClientSecret { get; set; }
        public ShipService()
      {

        this.Url = Settings.Default.UPS_UPSShip_ShipService;
        if (this.IsLocalFileSystemWebService(this.Url))
        {
          this.UseDefaultCredentials = true;
          this.useDefaultCredentialsSetExplicitly = false;
        }
        else
          this.useDefaultCredentialsSetExplicitly = true;
      }

      public UPSSecurity UPSSecurityValue
      {
        get => this.uPSSecurityValueField;
        set => this.uPSSecurityValueField = value;
      }

      public new string Url
      {
        get => base.Url;
        set
        {
          if (this.IsLocalFileSystemWebService(base.Url) && !this.useDefaultCredentialsSetExplicitly && !this.IsLocalFileSystemWebService(value))
            base.UseDefaultCredentials = false;
          base.Url = value;
        }
      }

      public new bool UseDefaultCredentials
      {
        get => base.UseDefaultCredentials;
        set
        {
          base.UseDefaultCredentials = value;
          this.useDefaultCredentialsSetExplicitly = true;
        }
      }

      public event ProcessShipmentCompletedEventHandler ProcessShipmentCompleted;

      public event ProcessShipConfirmCompletedEventHandler ProcessShipConfirmCompleted;

      public event ProcessShipAcceptCompletedEventHandler ProcessShipAcceptCompleted;

      [SoapHeader("AuthToken")]
      [SoapDocumentMethod("http://onlinetools.ups.com/webservices/ShipBinding/v1.0", Use = SoapBindingUse.Literal, ParameterStyle = SoapParameterStyle.Bare)]
      [return: XmlElement("ShipmentResponse", Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0")]
      public ShipmentResponse ProcessShipment([XmlElement(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0")] ShipmentRequest ShipmentRequest)
      {
        return (ShipmentResponse) this.Invoke(nameof (ProcessShipment), new object[1]
        {
          (object) ShipmentRequest
        })[0];
      }
        //MMT
        public AuthTokenHeader AuthToken { get; set; }

        protected override WebRequest GetWebRequest(Uri uri)
        {
            //mariam
            HttpWebRequest request = (HttpWebRequest)base.GetWebRequest(uri);

            string accessToken = AuthToken.Token; //"YOUR_ACCESS_TOKEN";

            // IMPORTANT: Add Bearer token
            request.Headers.Add(
                "Authorization",
                "Bearer " + accessToken
            );

            // UPS required headers (recommended)
            request.Headers.Add("transId", Guid.NewGuid().ToString());
            request.Headers.Add("transactionSrc", "MyApp");
            request.ContentType = "application/json";
            request.Method = "POST";

            return request;
            //mariam

            /*
            // Get the standard WebRequest
            HttpWebRequest request = (HttpWebRequest)base.GetWebRequest(uri);

            //request.Method = "POST";
            //request.ContentType = "application/json";

            // Send UPS token
            
            // Add the Authorization Header if the token exists
            if (!string.IsNullOrEmpty(AuthToken.Token))
            {
                request.Headers.Add("Authorization", "Bearer " + AuthToken.Token.TrimEnd());
                //request.Headers["Authorization"] =
                  //  "Bearer "+ AuthToken.Token.TrimEnd();
            }
            //MMM
            //string credentials = Convert.ToBase64String(
            //Encoding.UTF8.GetBytes(this.ClientId + ":" + this.ClientSecret)
        //);

            // Add Authorization header
          //  request.Headers["Authorization"] = "Basic " + credentials;

            // Optional UPS headers
            request.Headers["transId"] = Guid.NewGuid().ToString();
            request.Headers["transactionSrc"] = "TestApp";
            //MMM*/
            //return request;
        }
        //MMt
        public void ProcessShipmentAsync(ShipmentRequest ShipmentRequest)
      {
        this.ProcessShipmentAsync(ShipmentRequest, (object) null);
      }

      public void ProcessShipmentAsync(ShipmentRequest ShipmentRequest, object userState)
      {
        if (this.ProcessShipmentOperationCompleted == null)
          this.ProcessShipmentOperationCompleted = new SendOrPostCallback(this.OnProcessShipmentOperationCompleted);
        this.InvokeAsync("ProcessShipment", new object[1]
        {
          (object) ShipmentRequest
        }, this.ProcessShipmentOperationCompleted, userState);
      }

      private void OnProcessShipmentOperationCompleted(object arg)
      {
        if (this.ProcessShipmentCompleted == null)
          return;
        InvokeCompletedEventArgs completedEventArgs = (InvokeCompletedEventArgs) arg;
        this.ProcessShipmentCompleted((object) this, new ProcessShipmentCompletedEventArgs(completedEventArgs.Results, completedEventArgs.Error, completedEventArgs.Cancelled, completedEventArgs.UserState));
      }

      [SoapDocumentMethod("http://onlinetools.ups.com/webservices/ShipBinding/v1.0", Use = SoapBindingUse.Literal, ParameterStyle = SoapParameterStyle.Bare)]
      [SoapHeader("UPSSecurityValue")]
      [return: XmlElement("ShipConfirmResponse", Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0")]
      public ShipConfirmResponse ProcessShipConfirm([XmlElement(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0")] ShipConfirmRequest ShipConfirmRequest)
      {
        return (ShipConfirmResponse) this.Invoke(nameof (ProcessShipConfirm), new object[1]
        {
          (object) ShipConfirmRequest
        })[0];
      }

      public void ProcessShipConfirmAsync(ShipConfirmRequest ShipConfirmRequest)
      {
        this.ProcessShipConfirmAsync(ShipConfirmRequest, (object) null);
      }

      public void ProcessShipConfirmAsync(ShipConfirmRequest ShipConfirmRequest, object userState)
      {
        if (this.ProcessShipConfirmOperationCompleted == null)
          this.ProcessShipConfirmOperationCompleted = new SendOrPostCallback(this.OnProcessShipConfirmOperationCompleted);
        this.InvokeAsync("ProcessShipConfirm", new object[1]
        {
          (object) ShipConfirmRequest
        }, this.ProcessShipConfirmOperationCompleted, userState);
      }

      private void OnProcessShipConfirmOperationCompleted(object arg)
      {
        if (this.ProcessShipConfirmCompleted == null)
          return;
        InvokeCompletedEventArgs completedEventArgs = (InvokeCompletedEventArgs) arg;
        this.ProcessShipConfirmCompleted((object) this, new ProcessShipConfirmCompletedEventArgs(completedEventArgs.Results, completedEventArgs.Error, completedEventArgs.Cancelled, completedEventArgs.UserState));
      }

      [SoapDocumentMethod("http://onlinetools.ups.com/webservices/ShipBinding/v1.0", Use = SoapBindingUse.Literal, ParameterStyle = SoapParameterStyle.Bare)]
      [SoapHeader("UPSSecurityValue")]
      [return: XmlElement("ShipAcceptResponse", Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0")]
      public ShipAcceptResponse ProcessShipAccept([XmlElement(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0")] ShipAcceptRequest ShipAcceptRequest)
      {
        return (ShipAcceptResponse) this.Invoke(nameof (ProcessShipAccept), new object[1]
        {
          (object) ShipAcceptRequest
        })[0];
      }

      public void ProcessShipAcceptAsync(ShipAcceptRequest ShipAcceptRequest)
      {
        this.ProcessShipAcceptAsync(ShipAcceptRequest, (object) null);
      }

      public void ProcessShipAcceptAsync(ShipAcceptRequest ShipAcceptRequest, object userState)
      {
        if (this.ProcessShipAcceptOperationCompleted == null)
          this.ProcessShipAcceptOperationCompleted = new SendOrPostCallback(this.OnProcessShipAcceptOperationCompleted);
        this.InvokeAsync("ProcessShipAccept", new object[1]
        {
          (object) ShipAcceptRequest
        }, this.ProcessShipAcceptOperationCompleted, userState);
      }

      private void OnProcessShipAcceptOperationCompleted(object arg)
      {
        if (this.ProcessShipAcceptCompleted == null)
          return;
        InvokeCompletedEventArgs completedEventArgs = (InvokeCompletedEventArgs) arg;
        this.ProcessShipAcceptCompleted((object) this, new ProcessShipAcceptCompletedEventArgs(completedEventArgs.Results, completedEventArgs.Error, completedEventArgs.Cancelled, completedEventArgs.UserState));
      }

      public new void CancelAsync(object userState) => base.CancelAsync(userState);

      private bool IsLocalFileSystemWebService(string url)
      {
        if (url == null || url == string.Empty)
          return false;
        Uri uri = new Uri(url);
        return uri.Port >= 1024 /*0x0400*/ && string.Compare(uri.Host, "localHost", StringComparison.OrdinalIgnoreCase) == 0;
      }
    }
}
