#nullable enable
// Copyright (c) 2014 Daniel Grunwald
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this
// software and associated documentation files (the "Software"), to deal in the Software
// without restriction, including without limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons
// to whom the Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or
// substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
// INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
// PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
// FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

using dnSpy.Contracts.Decompiler;
using dnSpy.Contracts.Text;

using ICSharpCode.Decompiler.Disassembler;
using ICSharpCode.Decompiler.TypeSystem;
using ICSharpCode.Decompiler.Util;

namespace ICSharpCode.Decompiler.IL
{
	public static partial class InstructionOutputExtensions
	{
		public static void Write(this IDecompilerOutput output, OpCode opCode)
		{
			output.Write(originalOpCodeNames[(int)opCode], BoxedTextColor.OpCode);
		}

		public static void Write(this IDecompilerOutput output, StackType stackType)
		{
			output.Write(stackType.ToString().ToLowerInvariant(), BoxedTextColor.Text);
		}

		public static void Write(this IDecompilerOutput output, PrimitiveType primitiveType)
		{
			output.Write(primitiveType.ToString().ToLowerInvariant(), BoxedTextColor.Text);
		}

		public static void WriteTo(this IType type, IDecompilerOutput output, ILNameSyntax nameSyntax = ILNameSyntax.ShortTypeName)
		{
			output.Write(type.ReflectionName, type.GetDefinition()?.MetadataToken, DecompilerReferenceFlags.None, BoxedTextColor.Text);
		}

		public static void WriteTo(this ISymbol symbol, IDecompilerOutput output)
		{
			if (symbol is IMethod method && method.IsConstructor)
				output.Write(method.DeclaringType?.Name + "." + method.Name, symbol.Name, DecompilerReferenceFlags.None, BoxedTextColor.Text);
			else
				output.Write(symbol.Name, symbol.Name, DecompilerReferenceFlags.None, BoxedTextColor.Text);
		}

		public static void WriteTo(this Interval interval, IDecompilerOutput output, ILAstWritingOptions options)
		{
			if (!options.ShowILRanges)
				return;
			if (interval.IsEmpty)
				output.Write("[empty] ", BoxedTextColor.Text);
			else
				output.Write($"[{interval.Start:x4}..{interval.InclusiveEnd:x4}] ", BoxedTextColor.Text);
		}
	}
}
