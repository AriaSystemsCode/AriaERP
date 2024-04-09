using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;

namespace Fedex.FedexTrack
{
	[DebuggerStepThrough]
	[DesignerCategory("code")]
	[GeneratedCode("System.Web.Services", "4.0.30319.1")]
	public class getTrackNotificationCompletedEventArgs : AsyncCompletedEventArgs
	{
		private object[] results;

		public TrackNotificationReply Result
		{
			get
			{
				base.RaiseExceptionIfNecessary();
				return (TrackNotificationReply)this.results[0];
			}
		}

		internal getTrackNotificationCompletedEventArgs(object[] results, Exception exception, bool cancelled, object userState) : base(exception, cancelled, userState)
		{
			this.results = results;
		}
	}
}