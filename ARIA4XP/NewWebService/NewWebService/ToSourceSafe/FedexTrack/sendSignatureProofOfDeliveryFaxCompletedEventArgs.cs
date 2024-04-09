using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;

namespace Fedex.FedexTrack
{
	[DebuggerStepThrough]
	[DesignerCategory("code")]
	[GeneratedCode("System.Web.Services", "4.0.30319.1")]
	public class sendSignatureProofOfDeliveryFaxCompletedEventArgs : AsyncCompletedEventArgs
	{
		private object[] results;

		public SignatureProofOfDeliveryFaxReply Result
		{
			get
			{
				base.RaiseExceptionIfNecessary();
				return (SignatureProofOfDeliveryFaxReply)this.results[0];
			}
		}

		internal sendSignatureProofOfDeliveryFaxCompletedEventArgs(object[] results, Exception exception, bool cancelled, object userState) : base(exception, cancelled, userState)
		{
			this.results = results;
		}
	}
}