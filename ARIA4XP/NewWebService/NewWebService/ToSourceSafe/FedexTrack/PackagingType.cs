using System;
using System.CodeDom.Compiler;
using System.Xml.Serialization;

namespace Fedex.FedexTrack
{
	[GeneratedCode("System.Xml", "4.0.30319.1")]
	[Serializable]
	[XmlType(Namespace="http://fedex.com/ws/track/v4")]
	public enum PackagingType
	{
		FEDEX_10KG_BOX,
		FEDEX_25KG_BOX,
		FEDEX_BOX,
		FEDEX_ENVELOPE,
		FEDEX_PAK,
		FEDEX_TUBE,
		YOUR_PACKAGING
	}
}