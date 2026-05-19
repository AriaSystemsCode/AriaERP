// Decompiled with JetBrains decompiler
// Type: UPS.UPSTrack.TrackService
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


namespace UPS.UPSTrack
{
    //[WebServiceBinding(Name = "TrackBinding", Namespace = "http://www.ups.com/WSDL/XOLTWS/Track/v1.1")]
    [WebServiceBinding(Name = "TrackBinding", Namespace = "http://www.ups.com/WSDL/XOLTWS/Track/v2.0")]
    [GeneratedCode("System.Web.Services", "4.6.1087.0")]
    [DebuggerStepThrough]
    [DesignerCategory("code")]
    public class TrackService : SoapHttpClientProtocol
    {
      private UPSSecurity uPSSecurityValueField;
      private SendOrPostCallback ProcessTrackOperationCompleted;
      private bool useDefaultCredentialsSetExplicitly;

      public TrackService()
      {
        this.Url = Settings.Default.UPS_UPSTrack_TrackService;
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

      public event ProcessTrackCompletedEventHandler ProcessTrackCompleted;

      [SoapHeader("UPSSecurityValue")]
      [SoapDocumentMethod("http://onlinetools.ups.com/webservices/TrackBinding/v1.1", Use = SoapBindingUse.Literal, ParameterStyle = SoapParameterStyle.Bare)]
      [return: XmlElement("TrackResponse", Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Track/v1.1")]
      public TrackResponse ProcessTrack([XmlElement(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Track/v1.1")] TrackRequest TrackRequest)
      {
        return (TrackResponse) this.Invoke(nameof (ProcessTrack), new object[1]
        {
          (object) TrackRequest
        })[0];
      }

      public void ProcessTrackAsync(TrackRequest TrackRequest)
      {
        this.ProcessTrackAsync(TrackRequest, (object) null);
      }

      public void ProcessTrackAsync(TrackRequest TrackRequest, object userState)
      {
        if (this.ProcessTrackOperationCompleted == null)
          this.ProcessTrackOperationCompleted = new SendOrPostCallback(this.OnProcessTrackOperationCompleted);
        this.InvokeAsync("ProcessTrack", new object[1]
        {
          (object) TrackRequest
        }, this.ProcessTrackOperationCompleted, userState);
      }

      private void OnProcessTrackOperationCompleted(object arg)
      {
        if (this.ProcessTrackCompleted == null)
          return;
        InvokeCompletedEventArgs completedEventArgs = (InvokeCompletedEventArgs) arg;
        this.ProcessTrackCompleted((object) this, new ProcessTrackCompletedEventArgs(completedEventArgs.Results, completedEventArgs.Error, completedEventArgs.Cancelled, completedEventArgs.UserState));
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
