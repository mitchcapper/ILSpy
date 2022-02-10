using dnlib.DotNet;

namespace ICSharpCode.Decompiler.Documentation
{
	public class DummyDocumentationProvider : IDocumentationProvider
	{
		public bool HasDocumentation(MethodDef method)
		{
			return false;
		}
	}
}
