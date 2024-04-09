using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Xml.Serialization;

namespace Fedex.FedexTrack
{
	[DebuggerStepThrough]
	[DesignerCategory("code")]
	[GeneratedCode("System.Xml", "4.0.30319.1")]
	[Serializable]
	[XmlType(Namespace="http://fedex.com/ws/track/v4")]
	public class Localization
	{
		private string languageCodeField;

		private string localeCodeField;

		public string LanguageCode
		{
			get
			{
				return this.languageCodeField;
			}
			set
			{
				this.languageCodeField = value;
			}
		}

		public string LocaleCode
		{
			get
			{
				return this.localeCodeField;
			}
			set
			{
				this.localeCodeField = value;
			}
		}

		public Localization()
		{
		}
	}
}