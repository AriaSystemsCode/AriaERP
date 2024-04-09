using Fedex.Properties;
using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Threading;
using System.Web.Services;
using System.Web.Services.Description;
using System.Web.Services.Protocols;
using System.Xml.Serialization;

namespace Fedex.FedexTrack
{
	[DebuggerStepThrough]
	[DesignerCategory("code")]
	[GeneratedCode("System.Web.Services", "4.0.30319.1")]
	[WebServiceBinding(Name="TrackServiceSoapBinding", Namespace="http://fedex.com/ws/track/v4")]
	public class TrackService : SoapHttpClientProtocol
	{
		private SendOrPostCallback trackOperationCompleted;

		private SendOrPostCallback getTrackNotificationOperationCompleted;

		private SendOrPostCallback sendSignatureProofOfDeliveryFaxOperationCompleted;

		private SendOrPostCallback retrieveSignatureProofOfDeliveryLetterOperationCompleted;

		private bool useDefaultCredentialsSetExplicitly;

		public new string Url
		{
			get
			{
				return base.Url;
			}
			set
			{
				if ((!this.IsLocalFileSystemWebService(base.Url) || this.useDefaultCredentialsSetExplicitly ? false : !this.IsLocalFileSystemWebService(value)))
				{
					base.UseDefaultCredentials = false;
				}
				base.Url = value;
			}
		}

		public new bool UseDefaultCredentials
		{
			get
			{
				return base.UseDefaultCredentials;
			}
			set
			{
				base.UseDefaultCredentials = value;
				this.useDefaultCredentialsSetExplicitly = true;
			}
		}

		public TrackService()
		{
			this.Url = Settings.Default.Fedex_FedexTrack_TrackService;
			if (!this.IsLocalFileSystemWebService(this.Url))
			{
				this.useDefaultCredentialsSetExplicitly = true;
			}
			else
			{
				this.UseDefaultCredentials = true;
				this.useDefaultCredentialsSetExplicitly = false;
			}
		}

		public new void CancelAsync(object userState)
		{
			base.CancelAsync(userState);
		}

		[SoapDocumentMethod("getTrackNotification", Use=SoapBindingUse.Literal, ParameterStyle=SoapParameterStyle.Bare)]
		[return: XmlElement("TrackNotificationReply", Namespace="http://fedex.com/ws/track/v4")]
		public TrackNotificationReply getTrackNotification([XmlElement(Namespace="http://fedex.com/ws/track/v4")]  TrackNotificationRequest TrackNotificationRequest)
		{
			object[] trackNotificationRequest = new object[] { TrackNotificationRequest };
			return (TrackNotificationReply)base.Invoke("getTrackNotification", trackNotificationRequest)[0];
		}

		public void getTrackNotificationAsync( TrackNotificationRequest TrackNotificationRequest)
		{
			this.getTrackNotificationAsync(TrackNotificationRequest, null);
		}

		public void getTrackNotificationAsync( TrackNotificationRequest TrackNotificationRequest, object userState)
		{
			if (this.getTrackNotificationOperationCompleted == null)
			{
				this.getTrackNotificationOperationCompleted = new SendOrPostCallback(this.OngetTrackNotificationOperationCompleted);
			}
			object[] trackNotificationRequest = new object[] { TrackNotificationRequest };
			base.InvokeAsync("getTrackNotification", trackNotificationRequest, this.getTrackNotificationOperationCompleted, userState);
		}

		private bool IsLocalFileSystemWebService(string url)
		{
			bool flag;
			if ((url == null ? false : !(url == string.Empty)))
			{
				System.Uri uri = new System.Uri(url);
				flag = ((uri.Port < 1024 ? true : string.Compare(uri.Host, "localHost", StringComparison.OrdinalIgnoreCase) != 0) ? false : true);
			}
			else
			{
				flag = false;
			}
			return flag;
		}

		private void OngetTrackNotificationOperationCompleted(object arg)
		{
			if (this.getTrackNotificationCompleted != null)
			{
				InvokeCompletedEventArgs invokeCompletedEventArg = (InvokeCompletedEventArgs)arg;
				this.getTrackNotificationCompleted(this, new getTrackNotificationCompletedEventArgs(invokeCompletedEventArg.Results, invokeCompletedEventArg.Error, invokeCompletedEventArg.Cancelled, invokeCompletedEventArg.UserState));
			}
		}

		private void OnretrieveSignatureProofOfDeliveryLetterOperationCompleted(object arg)
		{
			if (this.retrieveSignatureProofOfDeliveryLetterCompleted != null)
			{
				InvokeCompletedEventArgs invokeCompletedEventArg = (InvokeCompletedEventArgs)arg;
				this.retrieveSignatureProofOfDeliveryLetterCompleted(this, new retrieveSignatureProofOfDeliveryLetterCompletedEventArgs(invokeCompletedEventArg.Results, invokeCompletedEventArg.Error, invokeCompletedEventArg.Cancelled, invokeCompletedEventArg.UserState));
			}
		}

		private void OnsendSignatureProofOfDeliveryFaxOperationCompleted(object arg)
		{
			if (this.sendSignatureProofOfDeliveryFaxCompleted != null)
			{
				InvokeCompletedEventArgs invokeCompletedEventArg = (InvokeCompletedEventArgs)arg;
				this.sendSignatureProofOfDeliveryFaxCompleted(this, new sendSignatureProofOfDeliveryFaxCompletedEventArgs(invokeCompletedEventArg.Results, invokeCompletedEventArg.Error, invokeCompletedEventArg.Cancelled, invokeCompletedEventArg.UserState));
			}
		}

		private void OntrackOperationCompleted(object arg)
		{
			if (this.trackCompleted != null)
			{
				InvokeCompletedEventArgs invokeCompletedEventArg = (InvokeCompletedEventArgs)arg;
				this.trackCompleted(this, new trackCompletedEventArgs(invokeCompletedEventArg.Results, invokeCompletedEventArg.Error, invokeCompletedEventArg.Cancelled, invokeCompletedEventArg.UserState));
			}
		}

		[SoapDocumentMethod("retrieveSignatureProofOfDeliveryLetter", Use=SoapBindingUse.Literal, ParameterStyle=SoapParameterStyle.Bare)]
		[return: XmlElement("SignatureProofOfDeliveryLetterReply", Namespace="http://fedex.com/ws/track/v4")]
		public SignatureProofOfDeliveryLetterReply retrieveSignatureProofOfDeliveryLetter([XmlElement(Namespace="http://fedex.com/ws/track/v4")]  SignatureProofOfDeliveryLetterRequest SignatureProofOfDeliveryLetterRequest)
		{
			object[] signatureProofOfDeliveryLetterRequest = new object[] { SignatureProofOfDeliveryLetterRequest };
			return (SignatureProofOfDeliveryLetterReply)base.Invoke("retrieveSignatureProofOfDeliveryLetter", signatureProofOfDeliveryLetterRequest)[0];
		}

		public void retrieveSignatureProofOfDeliveryLetterAsync( SignatureProofOfDeliveryLetterRequest SignatureProofOfDeliveryLetterRequest)
		{
			this.retrieveSignatureProofOfDeliveryLetterAsync(SignatureProofOfDeliveryLetterRequest, null);
		}

		public void retrieveSignatureProofOfDeliveryLetterAsync( SignatureProofOfDeliveryLetterRequest SignatureProofOfDeliveryLetterRequest, object userState)
		{
			if (this.retrieveSignatureProofOfDeliveryLetterOperationCompleted == null)
			{
				this.retrieveSignatureProofOfDeliveryLetterOperationCompleted = new SendOrPostCallback(this.OnretrieveSignatureProofOfDeliveryLetterOperationCompleted);
			}
			object[] signatureProofOfDeliveryLetterRequest = new object[] { SignatureProofOfDeliveryLetterRequest };
			base.InvokeAsync("retrieveSignatureProofOfDeliveryLetter", signatureProofOfDeliveryLetterRequest, this.retrieveSignatureProofOfDeliveryLetterOperationCompleted, userState);
		}

		[SoapDocumentMethod("sendSignatureProofOfDeliveryFax", Use=SoapBindingUse.Literal, ParameterStyle=SoapParameterStyle.Bare)]
		[return: XmlElement("SignatureProofOfDeliveryFaxReply", Namespace="http://fedex.com/ws/track/v4")]
		public SignatureProofOfDeliveryFaxReply sendSignatureProofOfDeliveryFax([XmlElement(Namespace="http://fedex.com/ws/track/v4")]  SignatureProofOfDeliveryFaxRequest SignatureProofOfDeliveryFaxRequest)
		{
			object[] signatureProofOfDeliveryFaxRequest = new object[] { SignatureProofOfDeliveryFaxRequest };
			return (SignatureProofOfDeliveryFaxReply)base.Invoke("sendSignatureProofOfDeliveryFax", signatureProofOfDeliveryFaxRequest)[0];
		}

		public void sendSignatureProofOfDeliveryFaxAsync( SignatureProofOfDeliveryFaxRequest SignatureProofOfDeliveryFaxRequest)
		{
			this.sendSignatureProofOfDeliveryFaxAsync(SignatureProofOfDeliveryFaxRequest, null);
		}

		public void sendSignatureProofOfDeliveryFaxAsync( SignatureProofOfDeliveryFaxRequest SignatureProofOfDeliveryFaxRequest, object userState)
		{
			if (this.sendSignatureProofOfDeliveryFaxOperationCompleted == null)
			{
				this.sendSignatureProofOfDeliveryFaxOperationCompleted = new SendOrPostCallback(this.OnsendSignatureProofOfDeliveryFaxOperationCompleted);
			}
			object[] signatureProofOfDeliveryFaxRequest = new object[] { SignatureProofOfDeliveryFaxRequest };
			base.InvokeAsync("sendSignatureProofOfDeliveryFax", signatureProofOfDeliveryFaxRequest, this.sendSignatureProofOfDeliveryFaxOperationCompleted, userState);
		}

		[SoapDocumentMethod("track", Use=SoapBindingUse.Literal, ParameterStyle=SoapParameterStyle.Bare)]
		[return: XmlElement("TrackReply", Namespace="http://fedex.com/ws/track/v4")]
		public TrackReply track([XmlElement(Namespace="http://fedex.com/ws/track/v4")]  TrackRequest TrackRequest)
		{
			object[] trackRequest = new object[] { TrackRequest };
			return (TrackReply)base.Invoke("track", trackRequest)[0];
		}

		public void trackAsync( TrackRequest TrackRequest)
		{
			this.trackAsync(TrackRequest, null);
		}

		public void trackAsync( TrackRequest TrackRequest, object userState)
		{
			if (this.trackOperationCompleted == null)
			{
				this.trackOperationCompleted = new SendOrPostCallback(this.OntrackOperationCompleted);
			}
			object[] trackRequest = new object[] { TrackRequest };
			base.InvokeAsync("track", trackRequest, this.trackOperationCompleted, userState);
		}

		public event getTrackNotificationCompletedEventHandler getTrackNotificationCompleted;

		public event retrieveSignatureProofOfDeliveryLetterCompletedEventHandler retrieveSignatureProofOfDeliveryLetterCompleted;

		public event sendSignatureProofOfDeliveryFaxCompletedEventHandler sendSignatureProofOfDeliveryFaxCompleted;

		public event trackCompletedEventHandler trackCompleted;
	}
}