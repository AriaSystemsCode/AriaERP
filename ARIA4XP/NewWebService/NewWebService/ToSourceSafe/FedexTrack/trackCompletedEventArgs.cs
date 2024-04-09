using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;

namespace Fedex.FedexTrack
{
	[DebuggerStepThrough]
	[DesignerCategory("code")]
	[GeneratedCode("System.Web.Services", "4.0.30319.1")]
	public class trackCompletedEventArgs : AsyncCompletedEventArgs
	{
		private object[] results;

		public TrackReply Result
		{
			get
			{
				base.RaiseExceptionIfNecessary();
				return (TrackReply)this.results[0];
			}
		}

		internal trackCompletedEventArgs(object[] results, Exception exception, bool cancelled, object userState) : base(exception, cancelled, userState)
		{
			this.results = results;
		}
	}
}