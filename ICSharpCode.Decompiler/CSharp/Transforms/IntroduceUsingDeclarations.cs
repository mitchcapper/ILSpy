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
using System.Text;
using dnlib.DotNet;
using dnSpy.Contracts.Decompiler;
using dnSpy.Contracts.Text;

using ICSharpCode.Decompiler.CSharp.Resolver;
using ICSharpCode.Decompiler.CSharp.Syntax;
using ICSharpCode.Decompiler.CSharp.TypeSystem;
using ICSharpCode.Decompiler.Semantics;
using ICSharpCode.Decompiler.TypeSystem;
using ICSharpCode.Decompiler.TypeSystem.Implementation;
using IMethod = ICSharpCode.Decompiler.TypeSystem.IMethod;

namespace ICSharpCode.Decompiler.CSharp.Transforms
{
	/// <summary>
	/// Introduces using declarations.
	/// </summary>
	public class IntroduceUsingDeclarations : IAstTransform
	{
		static readonly char[] namespaceSep = new char[] { '.' };

		public void Run(AstNode rootNode, TransformContext context)
		{
			// First determine all the namespaces that need to be imported:
			var requiredImports = new FindRequiredImports(context);
			rootNode.AcceptVisitor(requiredImports);

			var usingScope = new UsingScope();
			rootNode.AddAnnotation(usingScope);

			if (context.Settings.UsingDeclarations)
			{
				var stringBuilder = context.TypeSystem.SharedStringBuilder;

				var insertionPoint = rootNode.Children.LastOrDefault(n => n is PreProcessorDirective p && p.Type == PreProcessorDirectiveType.Define);

				// Now add using declarations for those namespaces:
				foreach (NamespaceRef ns in requiredImports.ImportedNamespaces.OrderByDescending(n => n.Namespace))
				{
					Debug.Assert(context.RequiredNamespacesSuperset.Contains(ns.Namespace), $"Should not insert using declaration for namespace that is missing from the superset: {ns}");
					// we go backwards (OrderByDescending) through the list of namespaces because we insert them backwards
					// (always inserting at the start of the list)

					string[] parts = ns.Namespace.Split(namespaceSep);
					stringBuilder.Clear();
					stringBuilder.Append(parts[0]);
					var nsAsm = ns.Assembly;
					SimpleType simpleType;
					AstType nsType = simpleType = new SimpleType(parts[0]).WithAnnotation(BoxedTextColor.Namespace);
					simpleType.IdentifierToken.WithAnnotation(BoxedTextColor.Namespace).WithAnnotation(new NamespaceReference(nsAsm, parts[0]));
					for (int i = 1; i < parts.Length; i++)
					{
						stringBuilder.Append('.');
						stringBuilder.Append(parts[i]);
						var nsPart = stringBuilder.ToString();
						nsType = new MemberType {
							Target = nsType,
							MemberNameToken = Identifier.Create(parts[i]).WithAnnotation(BoxedTextColor.Namespace)
														.WithAnnotation(new NamespaceReference(nsAsm, nsPart))
						}.WithAnnotation(BoxedTextColor.Namespace);
					}
					if (nsType.ToTypeReference(NameLookupMode.TypeInUsingDeclaration) is TypeOrNamespaceReference reference)
						usingScope.Usings.Add(reference);
					rootNode.InsertChildAfter(insertionPoint, new UsingDeclaration { Import = nsType }, SyntaxTree.MemberRole);
				}
			}

			// verify that the SimpleTypes refer to the correct type (no ambiguities)
			rootNode.AcceptVisitor(new FullyQualifyAmbiguousTypeNamesVisitor(context, usingScope));
		}

		sealed class FindRequiredImports : DepthFirstAstVisitor
		{
			string currentNamespace;
			private StringBuilder stringBuilder;

			public readonly HashSet<string> DeclaredNamespaces = new HashSet<string>() { string.Empty };
			public readonly HashSet<NamespaceRef> ImportedNamespaces = new HashSet<NamespaceRef>();

			public FindRequiredImports(TransformContext context)
			{
				this.stringBuilder = context.TypeSystem.SharedStringBuilder;
				this.currentNamespace = context.CurrentTypeDefinition?.Namespace ?? string.Empty;
			}

			bool IsParentOfCurrentNamespace(StringBuilder sb)
			{
				if (sb.Length == 0)
					return true;
				if (currentNamespace.StartsWith(sb))
				{
					if (currentNamespace.Length == sb.Length)
						return true;
					if (currentNamespace[sb.Length] == '.')
						return true;
				}
				return false;
			}

			public override void VisitSimpleType(SimpleType simpleType)
			{
				ITypeDefOrRef tr = simpleType.Annotation<ITypeDefOrRef>();
				if (tr != null)
				{
					var sb = GetNamespace(tr);
					if (!IsParentOfCurrentNamespace(sb))
					{
						string ns = sb.ToString();
						ImportedNamespaces.Add(new NamespaceRef(tr.DefinitionAssembly, ns));
					}
				}
				base.VisitSimpleType(simpleType); // also visit type arguments
			}

			StringBuilder GetNamespace(dnlib.DotNet.IType type)
			{
				this.stringBuilder.Clear();
				if (type == null)
					return this.stringBuilder;
				return FullNameFactory.NamespaceSB(type, false, this.stringBuilder);
			}

			public override void VisitNamespaceDeclaration(NamespaceDeclaration namespaceDeclaration)
			{
				string oldNamespace = currentNamespace;
				foreach (string ident in namespaceDeclaration.Identifiers)
				{
					currentNamespace = NamespaceDeclaration.BuildQualifiedName(currentNamespace, ident);
					DeclaredNamespaces.Add(currentNamespace);
				}
				base.VisitNamespaceDeclaration(namespaceDeclaration);
				currentNamespace = oldNamespace;
			}
		}

		sealed class FullyQualifyAmbiguousTypeNamesVisitor : DepthFirstAstVisitor
		{
			readonly Stack<CSharpTypeResolveContext> context;
			readonly bool ignoreUsingScope;
			readonly DecompilerSettings settings;

			TypeSystemAstBuilder astBuilder;

			public FullyQualifyAmbiguousTypeNamesVisitor(TransformContext context, UsingScope usingScope)
			{
				this.ignoreUsingScope = !context.Settings.UsingDeclarations;
				this.settings = context.Settings;

				CSharpTypeResolveContext currentContext;
				if (ignoreUsingScope)
				{
					currentContext = new CSharpTypeResolveContext(context.TypeSystem.MainModule);
				}
				else
				{
					this.context = new Stack<CSharpTypeResolveContext>();
					if (!string.IsNullOrEmpty(context.CurrentTypeDefinition?.Namespace))
					{
						foreach (string ns in context.CurrentTypeDefinition.Namespace.Split('.'))
						{
							usingScope = new UsingScope(usingScope, ns);
						}
					}
					currentContext = new CSharpTypeResolveContext(context.TypeSystem.MainModule, usingScope.Resolve(context.TypeSystem), context.CurrentTypeDefinition);
					this.context.Push(currentContext);
				}
				this.astBuilder = CreateAstBuilder(currentContext);
			}

			TypeSystemAstBuilder CreateAstBuilder(CSharpTypeResolveContext context, IL.ILFunction function = null)
			{
				CSharpResolver resolver = new CSharpResolver(context);
				if (function != null)
				{
					foreach (var v in function.Variables)
					{
						if (v.Kind != IL.VariableKind.Parameter && v.Name != null)
							resolver = resolver.AddVariable(new DefaultVariable(v.Type, v.Name));
					}
				}

				return new TypeSystemAstBuilder(resolver) {
					UseNullableSpecifierForValueTypes = settings.LiftNullables,
					AddResolveResultAnnotations = true,
					UseAliases = true
				};
			}

			public override void VisitNamespaceDeclaration(NamespaceDeclaration namespaceDeclaration)
			{
				if (ignoreUsingScope)
				{
					base.VisitNamespaceDeclaration(namespaceDeclaration);
					return;
				}
				var previousContext = context.Peek();
				var usingScope = previousContext.CurrentUsingScope.UnresolvedUsingScope;
				foreach (string ident in namespaceDeclaration.Identifiers)
				{
					usingScope = new UsingScope(usingScope, ident);
				}
				var currentContext = new CSharpTypeResolveContext(previousContext.CurrentModule, usingScope.Resolve(previousContext.Compilation));
				context.Push(currentContext);
				try
				{
					astBuilder = CreateAstBuilder(currentContext);
					base.VisitNamespaceDeclaration(namespaceDeclaration);
				}
				finally
				{
					astBuilder = CreateAstBuilder(previousContext);
					context.Pop();
				}
			}

			public override void VisitTypeDeclaration(TypeDeclaration typeDeclaration)
			{
				if (ignoreUsingScope)
				{
					base.VisitTypeDeclaration(typeDeclaration);
					return;
				}
				var previousContext = context.Peek();
				var currentContext = previousContext.WithCurrentTypeDefinition(typeDeclaration.GetSymbol() as ITypeDefinition);
				context.Push(currentContext);
				try
				{
					astBuilder = CreateAstBuilder(currentContext);
					base.VisitTypeDeclaration(typeDeclaration);
				}
				finally
				{
					astBuilder = CreateAstBuilder(previousContext);
					context.Pop();
				}
			}

			public override void VisitMethodDeclaration(MethodDeclaration methodDeclaration)
			{
				Visit(methodDeclaration, base.VisitMethodDeclaration);
			}

			public override void VisitAccessor(Accessor accessor)
			{
				Visit(accessor, base.VisitAccessor);
			}

			public override void VisitConstructorDeclaration(ConstructorDeclaration constructorDeclaration)
			{
				Visit(constructorDeclaration, base.VisitConstructorDeclaration);
			}

			public override void VisitDestructorDeclaration(DestructorDeclaration destructorDeclaration)
			{
				Visit(destructorDeclaration, base.VisitDestructorDeclaration);
			}

			public override void VisitOperatorDeclaration(OperatorDeclaration operatorDeclaration)
			{
				Visit(operatorDeclaration, base.VisitOperatorDeclaration);
			}

			void Visit<T>(T entityDeclaration, Action<T> baseCall) where T : EntityDeclaration
			{
				if (ignoreUsingScope)
				{
					baseCall(entityDeclaration);
					return;
				}
				if (entityDeclaration.GetSymbol() is IMethod method)
				{
					var previousContext = context.Peek();
					CSharpTypeResolveContext currentContext;
					if (CSharpDecompiler.IsWindowsFormsInitializeComponentMethod(method))
					{
						currentContext = new CSharpTypeResolveContext(previousContext.CurrentModule);
					}
					else
					{
						currentContext = previousContext.WithCurrentMember(method);
					}
					context.Push(currentContext);
					try
					{
						var function = entityDeclaration.Annotation<IL.ILFunction>();
						astBuilder = CreateAstBuilder(currentContext, function);
						baseCall(entityDeclaration);
					}
					finally
					{
						astBuilder = CreateAstBuilder(previousContext);
						context.Pop();
					}
				}
				else
				{
					baseCall(entityDeclaration);
				}
			}

			public override void VisitSimpleType(SimpleType simpleType)
			{
				TypeResolveResult rr;
				if ((rr = simpleType.Annotation<TypeResolveResult>()) == null)
				{
					base.VisitSimpleType(simpleType);
					return;
				}
				astBuilder.NameLookupMode = simpleType.GetNameLookupMode();
				if (astBuilder.NameLookupMode == NameLookupMode.Type)
				{
					AstType outermostType = simpleType;
					while (outermostType.Parent is AstType)
						outermostType = (AstType)outermostType.Parent;
					if (outermostType.Parent is TypeReferenceExpression)
					{
						// ILSpy uses TypeReferenceExpression in expression context even when the C# parser
						// wouldn't know that it's a type reference.
						// Fall back to expression-mode lookup in these cases:
						astBuilder.NameLookupMode = NameLookupMode.Expression;
					}
				}
				if (simpleType.Parent is Syntax.Attribute)
				{
					simpleType.ReplaceWith(astBuilder.ConvertAttributeType(rr.Type));
				}
				else
				{
					simpleType.ReplaceWith(astBuilder.ConvertType(rr.Type));
				}
			}
		}

		struct NamespaceRef : IEquatable<NamespaceRef> {
			public IAssembly Assembly { get; }
			public string Namespace { get; }
			public NamespaceRef(IAssembly asm, string ns) {
				Assembly = asm;
				Namespace = ns;
			}
			public bool Equals(NamespaceRef other) => StringComparer.Ordinal.Equals(other.Namespace, Namespace);
			public override bool Equals(object obj) => obj is NamespaceRef && Equals((NamespaceRef)obj);
			public override int GetHashCode() => StringComparer.Ordinal.GetHashCode(Namespace);
		}
	}
}
