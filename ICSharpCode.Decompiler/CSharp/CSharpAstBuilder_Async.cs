using System.Text;

using dnlib.DotNet;

using dnSpy.Contracts.Decompiler;

using ICSharpCode.Decompiler.CSharp.Syntax;

namespace ICSharpCode.Decompiler.CSharp
{
	public partial class CSharpAstBuilder
	{
		private struct AsyncMethodBodyResult
		{
			public AsyncMethodBodyResult(EntityDeclaration methodNode, MethodDef method, BlockStatement body, MethodDebugInfoBuilder builder, bool currentMethodIsAsync, bool currentMethodIsYieldReturn)
			{
				this.MethodNode = methodNode;
				this.Method = method;
				this.Body = body;
				this.Builder = builder;
				this.CurrentMethodIsAsync = currentMethodIsAsync;
				this.CurrentMethodIsYieldReturn = currentMethodIsYieldReturn;
			}

			public readonly EntityDeclaration MethodNode;

			public readonly MethodDef Method;

			public readonly BlockStatement Body;

			public readonly MethodDebugInfoBuilder Builder;

			public readonly bool CurrentMethodIsAsync;

			public readonly bool CurrentMethodIsYieldReturn;
		}

		private sealed class AsyncMethodBodyDecompilationState
		{
			public readonly StringBuilder StringBuilder = new StringBuilder();
		}

		AsyncMethodBodyDecompilationState GetAsyncMethodBodyDecompilationState() {
			lock (asyncMethodBodyDecompilationStates) {
				if (asyncMethodBodyDecompilationStates.Count > 0) {
					var state = asyncMethodBodyDecompilationStates[asyncMethodBodyDecompilationStates.Count - 1];
					asyncMethodBodyDecompilationStates.RemoveAt(asyncMethodBodyDecompilationStates.Count - 1);
					return state;
				}
			}
			return new AsyncMethodBodyDecompilationState();
		}

		void Return(AsyncMethodBodyDecompilationState state) {
			lock (asyncMethodBodyDecompilationStates)
				asyncMethodBodyDecompilationStates.Add(state);
		}

		void WaitForBodies() {
			if (methodBodyTasks.Count == 0)
				return;
			try {
				for (int i = 0; i < methodBodyTasks.Count; i++) {
					var result = methodBodyTasks[i].GetAwaiter().GetResult();
					context.CancellationToken.ThrowIfCancellationRequested();
					if (result.CurrentMethodIsAsync)
						result.MethodNode.Modifiers |= Modifiers.Async;
					result.MethodNode.AddChild(result.Body, Roles.Body);
					result.MethodNode.AddAnnotation(result.Builder);
					//ConvertAttributes(result.MethodNode, result.Method, result.CurrentMethodIsAsync, result.CurrentMethodIsYieldReturn);

					comments.Clear();
					comments.AddRange(result.MethodNode.GetChildrenByRole(Roles.Comment));
					for (int j = comments.Count - 1; j >= 0; j--) {
						var c = comments[j];
						c.Remove();
						result.MethodNode.InsertChildAfter(null, c, Roles.Comment);
					}
				}
			}
			finally {
				methodBodyTasks.Clear();
			}
		}

		void ClearCurrentMethodState() {
			context.CurrentMethodIsAsync = false;
			context.CurrentMethodIsYieldReturn = false;
		}
	}
}
