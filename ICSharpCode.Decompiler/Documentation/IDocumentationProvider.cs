using dnlib.DotNet;

namespace ICSharpCode.Decompiler.Documentation
{
	public interface IDocumentationProvider
	{
		bool HasDocumentation(MethodDef method);
	}
}
