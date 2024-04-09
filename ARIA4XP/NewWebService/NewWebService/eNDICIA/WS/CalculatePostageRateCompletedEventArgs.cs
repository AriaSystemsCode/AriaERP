using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;

namespace Endicia.WS
{
	[DebuggerStepThrough]
	[DesignerCategory("code")]
	[GeneratedCode("System.Web.Services", "4.0.30319.1")]
	public class CalculatePostageRateCompletedEventArgs : AsyncCompletedEventArgs
	{
		private object[] results;

		public PostageRateResponse Result
		{
			get
			{
				base.RaiseExceptionIfNecessary();
				return (PostageRateResponse)this.results[0];
			}
		}

		internal CalculatePostageRateCompletedEventArgs(object[] results, Exception exception, bool cancelled, object userState) : base(exception, cancelled, userState)
		{
			this.results = results;
		}
	}
}