using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Xml.Serialization;

namespace Endicia.WS
{
	[DebuggerStepThrough]
	[DesignerCategory("code")]
	[GeneratedCode("System.Xml", "4.0.30319.1")]
	[Serializable]
	[XmlInclude(typeof(AccountStatusRequest))]
	[XmlInclude(typeof(ChangePassPhraseRequest))]
	[XmlInclude(typeof(LabelRequest))]
	[XmlInclude(typeof(PostageRateRequest))]
	[XmlInclude(typeof(PostageRatesRequest))]
	[XmlInclude(typeof(RecreditRequest))]
	[XmlType(Namespace="www.envmgr.com/LabelService")]
	public class DataValidator
	{
		public DataValidator()
		{
		}
	}
}