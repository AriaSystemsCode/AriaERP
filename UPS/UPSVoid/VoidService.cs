// Decompiled with JetBrains decompiler
// Type: UPS.UPSVoid.VoidService
// Assembly: UPS, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null
// MVID: FE2C527D-E7B1-4D14-9A59-DD5F5EB261A9
// Assembly location: D:\Aria4xp\DLLS\UPS.dll

using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Net;
using System.Threading;
using System.Web.Services;
using System.Web.Services.Description;
using System.Web.Services.Protocols;
using System.Xml.Serialization;
using UPS.Properties;


namespace UPS.UPSVoid
{
    [DesignerCategory("code")]
    [GeneratedCode("System.Web.Services", "4.6.1087.0")]
    [DebuggerStepThrough]
    [WebServiceBinding(Name = "VoidBinding", Namespace = "http://www.ups.com/WSDL/XOLTWS/Void/v1.1")]
    public class VoidService : SoapHttpClientProtocol
    {
      private UPSSecurity uPSSecurityValueField;
      private SendOrPostCallback ProcessVoidOperationCompleted;
      private bool useDefaultCredentialsSetExplicitly;

      public VoidService()
      {
        this.Url = Settings.Default.UPS_UPSVoid_VoidService;
        if (this.IsLocalFileSystemWebService(this.Url))
        {
          this.UseDefaultCredentials = true;
          this.useDefaultCredentialsSetExplicitly = false;
        }
        else
          this.useDefaultCredentialsSetExplicitly = true;
      }
        //MMT
        public AuthTokenHeader AuthToken { get; set; }

        protected override WebRequest GetWebRequest(Uri uri)
        {
            // Get the standard WebRequest
            HttpWebRequest request = (HttpWebRequest)base.GetWebRequest(uri);

            // Add the Authorization Header if the token exists
            if (!string.IsNullOrEmpty(AuthToken.Token))
            {
                request.Headers.Add("Authorization", "Bearer " + AuthToken.Token);
            }

            return request;
        }
        //MMt
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

      public event ProcessVoidCompletedEventHandler ProcessVoidCompleted;

      [SoapDocumentMethod("http://onlinetools.ups.com/webservices/VoidBinding/v1.1", Use = SoapBindingUse.Literal, ParameterStyle = SoapParameterStyle.Bare)]
      [SoapHeader("UPSSecurityValue")]
      [return: XmlElement("VoidShipmentResponse", Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Void/v1.1")]
      public VoidShipmentResponse ProcessVoid([XmlElement(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Void/v1.1")] VoidShipmentRequest VoidShipmentRequest)
      {
        return (VoidShipmentResponse) this.Invoke(nameof (ProcessVoid), new object[1]
        {
          (object) VoidShipmentRequest
        })[0];
      }

      public void ProcessVoidAsync(VoidShipmentRequest VoidShipmentRequest)
      {
        this.ProcessVoidAsync(VoidShipmentRequest, (object) null);
      }

      public void ProcessVoidAsync(VoidShipmentRequest VoidShipmentRequest, object userState)
      {
        if (this.ProcessVoidOperationCompleted == null)
          this.ProcessVoidOperationCompleted = new SendOrPostCallback(this.OnProcessVoidOperationCompleted);
        this.InvokeAsync("ProcessVoid", new object[1]
        {
          (object) VoidShipmentRequest
        }, this.ProcessVoidOperationCompleted, userState);
      }

      private void OnProcessVoidOperationCompleted(object arg)
      {
        if (this.ProcessVoidCompleted == null)
          return;
        InvokeCompletedEventArgs completedEventArgs = (InvokeCompletedEventArgs) arg;
        this.ProcessVoidCompleted((object) this, new ProcessVoidCompletedEventArgs(completedEventArgs.Results, completedEventArgs.Error, completedEventArgs.Cancelled, completedEventArgs.UserState));
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
