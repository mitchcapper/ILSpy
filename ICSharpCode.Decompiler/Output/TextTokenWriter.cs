// Copyright (c) 2011 AlphaSierraPapa for the SharpDevelop Team
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

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using dnSpy.Contracts.Decompiler;
using dnSpy.Contracts.Text;
using ICSharpCode.Decompiler.CSharp;
using ICSharpCode.Decompiler.CSharp.OutputVisitor;
using ICSharpCode.Decompiler.CSharp.Syntax;
using ICSharpCode.Decompiler.IL;
using ICSharpCode.Decompiler.Semantics;
using ICSharpCode.Decompiler.TypeSystem;
using ICSharpCode.Decompiler.TypeSystem.Implementation;
using PrimitiveType = ICSharpCode.Decompiler.CSharp.Syntax.PrimitiveType;

namespace ICSharpCode.Decompiler
{
	public class TextTokenWriter : TokenWriter
	{
		private readonly MetadataTextColorProvider colorProvider;
		readonly IDecompilerOutput output;
		readonly Stack<AstNode> nodeStack = new Stack<AstNode>();
		int braceLevelWithinType = -1;

		public TextTokenWriter(IDecompilerOutput output, MetadataTextColorProvider colorProvider)
		{
			if (output == null)
				throw new ArgumentNullException(nameof(output));
			this.output = output;
			this.colorProvider = colorProvider;
		}

		public override void WriteIdentifier(Identifier identifier, object data)
		{
			if (BoxedTextColor.Text.Equals(data)) {
				data = colorProvider.GetColor(identifier.AnnotationVT<TextColor>() ?? identifier.Annotation<object>());
			}

			var escapedName = IdentifierEscaper.Escape(identifier.Name);
			if (!BoxedTextColor.Keyword.Equals(data) && (identifier.IsVerbatim || CSharpOutputVisitor.IsKeyword(identifier.Name, identifier))) {
				escapedName = "@" + escapedName;
			}

			var definition = GetCurrentDefinition();
			if (definition != null) {
				dnlib.DotNet.IMDTokenProvider cecil = SymbolToCecil(definition);
				if (cecil != null) {
					output.Write(escapedName, cecil, DecompilerReferenceFlags.Definition, data);
					return;
				}
			}

			var member = GetCurrentMemberReference();
			if (member != null) {
				dnlib.DotNet.IMDTokenProvider cecil = SymbolToCecil(member);
				if (cecil != null) {
					output.Write(escapedName, cecil, DecompilerReferenceFlags.None, data);
					return;
				}
			}

			object memberRef = identifier.Annotation<NamespaceReference>();
			if (memberRef != null) {
				output.Write(escapedName, memberRef, DecompilerReferenceFlags.None, data);
				return;
			}

			var localDefinition = GetCurrentLocalDefinition(identifier);
			if (localDefinition != null) {
				output.Write(escapedName, localDefinition, DecompilerReferenceFlags.Local | DecompilerReferenceFlags.Definition, data);
				return;
			}

			var localRef = GetCurrentLocalReference();
			if (localRef != null) {
				output.Write(escapedName, localRef, DecompilerReferenceFlags.Local, data);
				return;
			}

			if (identifier.Annotation<IdentifierFormatted>() != null)
				escapedName = identifier.Name;
			output.Write(escapedName, data);
		}

		dnlib.DotNet.IMDTokenProvider SymbolToCecil(ISymbol symbol)
		{
			if (symbol is IType type) {
				return type.GetDefinition().MetadataToken;
			}
			if (symbol is IMember member) {
				return member.MetadataToken;
			}
			return null;
		}

		ISymbol GetCurrentMemberReference()
		{
			AstNode node = nodeStack.Peek();
			var symbol = node.GetSymbol();
			if (symbol == null && node.Role == Roles.TargetExpression && node.Parent is InvocationExpression) {
				symbol = node.Parent.GetSymbol();
			}
			if (symbol != null && node.Role == Roles.Type && node.Parent is ObjectCreateExpression) {
				symbol = node.Parent.GetSymbol();
			}

			if (node is IdentifierExpression && node.Role == Roles.TargetExpression && node.Parent is InvocationExpression && symbol is IMember member) {
				var declaringType = member.DeclaringType;
				if (declaringType != null && declaringType.Kind == TypeKind.Delegate)
					return null;
			}
			return FilterMember(symbol);
		}

		ISymbol FilterMember(ISymbol symbol)
		{
			if (symbol == null)
				return null;

			if (symbol is LocalFunctionMethod)
				return null;

			return symbol;
		}

		object GetCurrentLocalReference()
		{
			AstNode node = nodeStack.Peek();
			ILVariable variable = node.Annotation<ILVariableResolveResult>()?.Variable;
			if (variable != null)
				return variable.Name;

			var letClauseVariable = node.Annotation<CSharp.Transforms.LetIdentifierAnnotation>();
			if (letClauseVariable != null)
				return letClauseVariable;

			if (node is GotoStatement gotoStatement) {
				var method = nodeStack.Select(nd => nd.GetSymbol() as IMethod).FirstOrDefault(mr => mr != null);
				if (method != null)
					return method + gotoStatement.Label;
			}

			if (node.Role == Roles.TargetExpression && node.Parent is InvocationExpression) {
				var symbol = node.Parent.GetSymbol();
				if (symbol is LocalFunctionMethod s)
					return s.FullName;
			}

			return null;
		}

		object GetCurrentLocalDefinition(Identifier id)
		{
			AstNode node = nodeStack.Peek();
			if (node is Identifier && node.Parent != null)
				node = node.Parent;

			if (node is ParameterDeclaration || node is VariableInitializer || node is CatchClause || node is VariableDesignation) {
				var variable = node.Annotation<ILVariableResolveResult>()?.Variable;
				if (variable != null)
					return variable.Name;
			}

			if (id.Role == QueryJoinClause.IntoIdentifierRole || id.Role == QueryJoinClause.JoinIdentifierRole)
			{
				var variable = id.Annotation<ILVariableResolveResult>()?.Variable;
				if (variable != null)
					return variable.Name;
			}

			if (node is QueryLetClause)
			{
				var variable = node.Annotation<CSharp.Transforms.LetIdentifierAnnotation>();
				if (variable != null)
					return variable;
			}

			if (node is LabelStatement label) {
				var method = nodeStack.Select(nd => nd.GetSymbol() as IMethod).FirstOrDefault(mr => mr != null);
				if (method != null)
					return method + label.Label;
			}

			if (node is MethodDeclaration && node.Parent is LocalFunctionDeclarationStatement) {
				var localFunction = node.Parent.GetResolveResult() as MemberResolveResult;
				if (localFunction != null)
					return localFunction.Member.FullName;
			}

			return null;
		}

		ISymbol GetCurrentDefinition()
		{
			if (nodeStack == null || nodeStack.Count == 0)
				return null;

			var node = nodeStack.Peek();
			if (node is Identifier)
				node = node.Parent;
			if (IsDefinition(ref node))
				return node.GetSymbol();

			return null;
		}

		public override void WriteKeyword(Role role, string keyword)
		{
			WriteKeyword(keyword);
		}

		void WriteKeyword(string keyword)
		{
			var node = nodeStack.Peek();

			var memberRef = GetCurrentMemberReference();
			if (node is IndexerDeclaration)
				memberRef = node.GetSymbol();
			if (keyword != "async" && memberRef != null && memberRef is IMember member && member.MetadataToken != null && (node is PrimitiveType || node is ConstructorInitializer ||
				node is BaseReferenceExpression || node is ThisReferenceExpression ||
				node is ObjectCreateExpression ||
				node is AnonymousMethodExpression)) {
				output.Write(keyword, member.MetadataToken, keyword == "new" ? DecompilerReferenceFlags.Hidden : DecompilerReferenceFlags.None, BoxedTextColor.Keyword);
			}
			else if (memberRef != null && memberRef is IMember member2 && member2.MetadataToken != null && node is IndexerDeclaration && keyword == "this") {
				output.Write(keyword, member2.MetadataToken, DecompilerReferenceFlags.Definition, BoxedTextColor.Keyword);
			}
			else {
				output.Write(keyword, BoxedTextColor.Keyword);
			}
		}

		public override void WriteToken(Role role, string token, object data)
		{
			var memberRef = GetCurrentMemberReference();
			var node = nodeStack.Peek();

			bool addRef = memberRef != null &&
						  (node is BinaryOperatorExpression ||
						   node is UnaryOperatorExpression ||
						   node is AssignmentExpression ||
						   node is IndexerExpression);

			// Add a ref to the method if it's a delegate call
			if (!addRef && node is InvocationExpression && memberRef is IMember member && member.MetadataToken is dnlib.DotNet.IMethod) {
				var md = (member.MetadataToken as dnlib.DotNet.IMethod).Resolve();
				if (md != null && md.DeclaringType != null && md.DeclaringType.IsDelegate)
					addRef = true;
			}

			if (addRef && memberRef is IMember member2 && member2.MetadataToken != null)
				output.Write(token, member2.MetadataToken, DecompilerReferenceFlags.None, data);
			else
				output.Write(token, data);
		}

		public override void Space()
		{
			output.Write(" ", BoxedTextColor.Text);
		}

		public void OpenBrace(BraceStyle style, out int? start, out int? end)
		{
			if (braceLevelWithinType >= 0 || nodeStack.Peek() is TypeDeclaration)
				braceLevelWithinType++;
			output.WriteLine();
			start = output.NextPosition;
			output.Write("{", BoxedTextColor.Punctuation);
			end = output.NextPosition;
			output.WriteLine();
			output.IncreaseIndent();
		}

		public void CloseBrace(BraceStyle style, out int? start, out int? end)
		{
			output.DecreaseIndent();
			start = output.NextPosition;
			output.Write("}", BoxedTextColor.Punctuation);
			end = output.NextPosition;
			if (braceLevelWithinType >= 0)
				braceLevelWithinType--;
		}

		public override void Indent()
		{
			output.IncreaseIndent();
		}

		public override void Unindent()
		{
			output.DecreaseIndent();
		}

		public override void NewLine()
		{
			output.WriteLine();
		}

		public override void WriteComment(CommentType commentType, string content, CommentReference[] refs)
		{
			switch (commentType) {
				case CommentType.SingleLine:
					output.Write("//", BoxedTextColor.Comment);
					Write(content, refs);
					output.WriteLine();
					break;
				case CommentType.MultiLine:
					output.Write("/*", BoxedTextColor.Comment);
					Write(content, refs);
					output.Write("*/", BoxedTextColor.Comment);
					break;
				case CommentType.Documentation:
					output.Write("///", BoxedTextColor.XmlDocCommentDelimiter);
					Debug.Assert(refs == null);
					output.WriteXmlDoc(content);
					output.WriteLine();
					break;
				default:
					Write(content, refs);
					break;
			}
		}

		void Write(string content, CommentReference[] refs)
		{
			if (refs == null) {
				output.Write(content, BoxedTextColor.Comment);
				return;
			}

			int offs = 0;
			for (int i = 0; i < refs.Length; i++) {
				var @ref = refs[i];
				var s = content.Substring(offs, @ref.Length);
				offs += @ref.Length;
				if (@ref.Reference == null)
					output.Write(s, BoxedTextColor.Comment);
				else
					output.Write(s, @ref.Reference, @ref.IsLocal ? DecompilerReferenceFlags.Local : DecompilerReferenceFlags.None, BoxedTextColor.Comment);
			}
			Debug.Assert(offs == content.Length);
		}

		public override void WritePreProcessorDirective(PreProcessorDirectiveType type, string argument)
		{
			// pre-processor directive must start on its own line
			output.Write("#", BoxedTextColor.Text);
			output.Write(type.ToString().ToLowerInvariant(), BoxedTextColor.Text);
			if (!string.IsNullOrEmpty(argument)) {
				output.Write(" ", BoxedTextColor.Text);
				output.Write(argument, BoxedTextColor.Text);
			}
			output.WriteLine();
		}

		public override void WritePrimitiveValue(object value, object data = null, LiteralFormat format = LiteralFormat.None)
		{
			int column = 0;
			//TODO: parameters
			var numberFormatter = NumberFormatter.GetCSharpInstance(false, upper: true);
			TextWriterTokenWriter.WritePrimitiveValue(value, data, format, 1000, ref column, numberFormatter, WritePrimitiveValueCore, WriteToken);
		}

		void WritePrimitiveValueCore(string text, object reference, object color)
		{
			if (color == BoxedTextColor.String || color == BoxedTextColor.Char) {
				int start = output.NextPosition;
				output.Write(text, color);
				int end = output.NextPosition;
				output.AddBracePair(new TextSpan(start, 1), new TextSpan(end - 1, 1), CodeBracesRangeFlags.SingleQuotes);
			}
			else if (reference != null)
				output.Write(text, reference, DecompilerReferenceFlags.Local | DecompilerReferenceFlags.Hidden | DecompilerReferenceFlags.NoFollow, color);
			else
				output.Write(text, color);
		}

		public override void WriteInterpolatedText(string text, object data = null)
		{
			output.Write(TextWriterTokenWriter.ConvertString(text), data);
		}

		public override void WritePrimitiveType(string type)
		{
			WriteKeyword(type);
			if (type == "new") {
				int startPos1 = output.NextPosition;
				output.Write("(", BoxedTextColor.Punctuation);
				int startPos2 = output.NextPosition;
				output.Write(")", BoxedTextColor.Punctuation);
				output.AddBracePair(new TextSpan(startPos1, 1), new TextSpan(startPos2, 1), CodeBracesRangeFlags.Parentheses);
			}
		}

		MethodDebugInfoBuilder currentMethodDebugInfoBuilder;
		Stack<MethodDebugInfoBuilder> parentMethodDebugInfoBuilder = new Stack<MethodDebugInfoBuilder>();
		List<Tuple<MethodDebugInfoBuilder, List<ILSpan>>> multiMappings;

		public override void StartNode(AstNode node)
		{
			nodeStack.Push(node);

			MethodDebugInfoBuilder mapping = node.Annotation<MethodDebugInfoBuilder>();
			if (mapping != null) {
				parentMethodDebugInfoBuilder.Push(currentMethodDebugInfoBuilder);
				currentMethodDebugInfoBuilder = mapping;
			}
			// For ctor/cctor field initializers
			var mms = node.Annotation<List<Tuple<MethodDebugInfoBuilder, List<ILSpan>>>>();
			if (mms != null) {
				Debug.Assert(multiMappings == null);
				multiMappings = mms;
			}
		}

		public override void EndNode(AstNode node)
		{
			if (nodeStack.Pop() != node)
				throw new InvalidOperationException();

			if (node.Annotation<MethodDebugInfoBuilder>() != null) {
				// if (context.CalculateILSpans) {
				// 	foreach (var ns in context.UsingNamespaces)
				// 		currentMethodDebugInfoBuilder.Scope.Imports.Add(ImportInfo.CreateNamespace(ns));
				// }
				if (parentMethodDebugInfoBuilder.Peek() != currentMethodDebugInfoBuilder)
					output.AddDebugInfo(currentMethodDebugInfoBuilder.Create());
				currentMethodDebugInfoBuilder = parentMethodDebugInfoBuilder.Pop();
			}
			var mms = node.Annotation<List<Tuple<MethodDebugInfoBuilder, List<ILSpan>>>>();
			if (mms != null) {
				Debug.Assert(mms == multiMappings);
				if (mms == multiMappings) {
					foreach (var mm in mms)
						output.AddDebugInfo(mm.Item1.Create());
					multiMappings = null;
				}
			}
		}

		public static bool IsDefinition(ref AstNode node)
		{
			if (node is EntityDeclaration && !(node.Parent is LocalFunctionDeclarationStatement))
				return true;
			if (node is VariableInitializer && node.Parent is FieldDeclaration) {
				node = node.Parent;
				return true;
			}
			if (node is FixedVariableInitializer && node.Parent is FixedFieldDeclaration) {
				node = node.Parent;
				return true;
			}
			return false;
		}

		class DebugState
		{
			public List<AstNode> Nodes = new List<AstNode>();
			public int StartSpan;
		}
		readonly Stack<DebugState> debugStack = new Stack<DebugState>();
		public override void DebugStart(AstNode node, int? start)
		{
			debugStack.Push(new DebugState { StartSpan = start ?? output.NextPosition });
		}

		public override void DebugHidden(AstNode hiddenNode)
		{
			if (hiddenNode == null || hiddenNode.IsNull)
				return;
			if (debugStack.Count > 0)
				debugStack.Peek().Nodes.AddRange(hiddenNode.DescendantsAndSelf);
		}

		public override void DebugExpression(AstNode node)
		{
			if (debugStack.Count > 0)
				debugStack.Peek().Nodes.Add(node);
		}

		public override void DebugEnd(AstNode node, int? end)
		{
			var state = debugStack.Pop();
		}

		public override int? GetLocation()
		{
			return output.NextPosition;
		}

		public override void AddHighlightedKeywordReference(object reference, int start, int end) {
			Debug.Assert(reference != null);
			if (reference != null)
				output.AddSpanReference(reference, start, end, PredefinedSpanReferenceIds.HighlightRelatedKeywords);
		}

		public override void AddBracePair(int leftStart, int leftEnd, int rightStart, int rightEnd, CodeBracesRangeFlags flags) =>
			output.AddBracePair(TextSpan.FromBounds(leftStart, leftEnd), TextSpan.FromBounds(rightStart, rightEnd), flags);

		public override void AddLineSeparator(int position) => output.AddLineSeparator(position);
	}
}
