using System;
using System.CodeDom.Compiler;
using System.Xml.Serialization;

namespace Fedex.FedexTrack
{
	[GeneratedCode("System.Xml", "4.0.30319.1")]
	[Serializable]
	[XmlType(Namespace="http://fedex.com/ws/track/v4")]
	public enum CarrierCodeType
	{
		FDXC,
		FDXE,
		FDXG,
		FXCC,
		FXFR,
		FXSP
	}
}