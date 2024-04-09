using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;

namespace Endicia.WS
{
	[DebuggerStepThrough]
	[DesignerCategory("code")]
	[GeneratedCode("System.Web.Services", "4.0.30319.1")]
	public class BuyPostageCompletedEventArgs : AsyncCompletedEventArgs
	{
		private object[] results;

		public RecreditRequestResponse Result
		{
			get
			{
				base.RaiseExceptionIfNecessary();
				return (RecreditRequestResponse)this.results[0];
			}
		}

		internal BuyPostageCompletedEventArgs(object[] results, Exception exception, bool cancelled, object userState) : base(exception, cancelled, userState)
		{
			this.results = results;
		}
	}
}