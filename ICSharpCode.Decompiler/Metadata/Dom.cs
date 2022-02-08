#nullable enable

using System;
using System.Collections.Immutable;
using System.IO;


using ICSharpCode.Decompiler.TypeSystem;
using ICSharpCode.Decompiler.Util;

namespace ICSharpCode.Decompiler.Metadata
{
	public enum TargetRuntime
	{
		Unknown,
		Net_1_0,
		Net_1_1,
		Net_2_0,
		Net_4_0
	}
}
