// Copyright (c) 2010-2013 AlphaSierraPapa for the SharpDevelop Team
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
using ICSharpCode.Decompiler.IL;
using ICSharpCode.Decompiler.TypeSystem.Implementation;

namespace ICSharpCode.Decompiler.TypeSystem
{
	/// <summary>
	/// Static helper methods for reflection names.
	/// </summary>
	public static class ReflectionHelper
	{
		/// <summary>
		/// A reflection class used to represent <c>null</c>.
		/// </summary>
		public sealed class Null {}

		/// <summary>
		/// A reflection class used to represent <c>dynamic</c>.
		/// </summary>
		public sealed class Dynamic {}

		/// <summary>
		/// A reflection class used to represent <c>nint</c>.
		/// </summary>
		public sealed class NInt { }

		/// <summary>
		/// A reflection class used to represent <c>nuint</c>.
		/// </summary>
		public sealed class NUInt { }

		/// <summary>
		/// A reflection class used to represent an unbound type argument.
		/// </summary>
		public sealed class UnboundTypeArgument {}

		#region ICompilation.FindType
		/// <summary>
		/// Retrieves the specified type in this compilation.
		/// Returns <see cref="SpecialType.UnknownType"/> if the type cannot be found in this compilation.
		/// </summary>
		/// <remarks>
		/// This method cannot be used with open types; all type parameters will be substituted
		/// with <see cref="SpecialType.UnknownType"/>.
		/// </remarks>
		public static IType FindType(this ICompilation compilation, Type type)
		{
			return type.ToTypeReference().Resolve(new SimpleTypeResolveContext(compilation));
		}

		public static IType FindType(this ICompilation compilation, StackType stackType, Sign sign = Sign.None)
		{
			switch (stackType)
			{
				case StackType.Unknown:
					return SpecialType.UnknownType;
				case StackType.Ref:
					return new ByReferenceType(SpecialType.UnknownType);
				default:
					return compilation.FindType(stackType.ToKnownTypeCode(sign));
			}
		}
		#endregion

		#region Type.ToTypeReference()
		/// <summary>
		/// Creates a reference to the specified type.
		/// </summary>
		/// <param name="type">The type to be converted.</param>
		/// <returns>Returns the type reference.</returns>
		/// <remarks>
		/// If the type is open (contains type parameters '`0' or '``0'),
		/// an <see cref="ITypeResolveContext"/> with the appropriate CurrentTypeDefinition/CurrentMember is required
		/// to resolve the type reference.
		/// For closed types, the root type resolve context for the compilation is sufficient.
		/// </remarks>
		public static ITypeReference ToTypeReference(this Type type)
		{
			if (type == null)
				return SpecialType.UnknownType;
			if (type.IsGenericType && !type.IsGenericTypeDefinition) {
				ITypeReference def = ToTypeReference(type.GetGenericTypeDefinition());
				Type[] arguments = type.GetGenericArguments();
				ITypeReference[] args = new ITypeReference[arguments.Length];
				bool allUnbound = true;
				for (int i = 0; i < arguments.Length; i++) {
					args[i] = ToTypeReference(arguments[i]);
					allUnbound &= args[i].Equals(SpecialType.UnboundTypeArgument);
				}
				if (allUnbound)
					return def;
				else
					return new ParameterizedTypeReference(def, args);
			} else if (type.IsArray) {
				return new ArrayTypeReference(ToTypeReference(type.GetElementType()), type.GetArrayRank());
			} else if (type.IsPointer) {
				return new PointerTypeReference(ToTypeReference(type.GetElementType()));
			} else if (type.IsByRef) {
				return new ByReferenceTypeReference(ToTypeReference(type.GetElementType()));
			} else if (type.IsGenericParameter) {
				if (type.DeclaringMethod != null) {
					return TypeParameterReference.Create(SymbolKind.Method, type.GenericParameterPosition);
				} else {
					return TypeParameterReference.Create(SymbolKind.TypeDefinition, type.GenericParameterPosition);
				}
			} else if (type.DeclaringType != null) {
				if (type == typeof(Dynamic))
					return SpecialType.Dynamic;
				else if (type == typeof(NInt))
					return SpecialType.NInt;
				else if (type == typeof(NUInt))
					return SpecialType.NUInt;
				else if (type == typeof(Null))
					return SpecialType.NullType;
				else if (type == typeof(UnboundTypeArgument))
					return SpecialType.UnboundTypeArgument;
				ITypeReference baseTypeRef = ToTypeReference(type.DeclaringType);
				int typeParameterCount;
				string name = SplitTypeParameterCountFromReflectionName(type.Name, out typeParameterCount);
				return new NestedTypeReference(baseTypeRef, name, typeParameterCount);
			} else {
				IModuleReference assemblyReference = new DefaultAssemblyReference(type.Assembly.FullName);
				int typeParameterCount;
				string name = SplitTypeParameterCountFromReflectionName(type.Name, out typeParameterCount);
				return new GetClassTypeReference(assemblyReference, type.Namespace, name, typeParameterCount);
			}
		}
		#endregion

		#region SplitTypeParameterCountFromReflectionName
		/// <summary>
		/// Removes the ` with type parameter count from the reflection name.
		/// </summary>
		/// <remarks>Do not use this method with the full name of inner classes.</remarks>
		public static string SplitTypeParameterCountFromReflectionName(string reflectionName)
		{
			int pos = reflectionName.LastIndexOf('`');
			if (pos < 0) {
				return reflectionName;
			} else {
				return reflectionName.Substring(0, pos);
			}
		}

		/// <summary>
		/// Removes the ` with type parameter count from the reflection name.
		/// </summary>
		/// <remarks>Do not use this method with the full name of inner classes.</remarks>
		public static string SplitTypeParameterCountFromReflectionName(string reflectionName, out int typeParameterCount)
		{
			int pos = reflectionName.LastIndexOf('`');
			if (pos < 0) {
				typeParameterCount = 0;
				return reflectionName;
			} else {
				string typeCount = reflectionName.Substring(pos + 1);
				if (int.TryParse(typeCount, out typeParameterCount))
					return reflectionName.Substring(0, pos);
				else
					return reflectionName;
			}
		}
		#endregion

		#region TypeCode support
		/// <summary>
		/// Retrieves a built-in type using the specified type code.
		/// </summary>
		public static IType FindType(this ICompilation compilation, TypeCode typeCode)
		{
			return compilation.FindType((KnownTypeCode)typeCode);
		}

		/// <summary>
		/// Creates a reference to the specified type.
		/// </summary>
		/// <param name="typeCode">The type to be converted.</param>
		/// <returns>Returns the type reference.</returns>
		public static ITypeReference ToTypeReference(this TypeCode typeCode)
		{
			return KnownTypeReference.Get((KnownTypeCode)typeCode);
		}

		/// <summary>
		/// Gets the type code for the specified type, or TypeCode.Empty if none of the other type codes match.
		/// </summary>
		public static TypeCode GetTypeCode(this IType type)
		{
			ITypeDefinition def = type as ITypeDefinition;
			if (def != null) {
				KnownTypeCode typeCode = def.KnownTypeCode;
				if (typeCode <= KnownTypeCode.String && typeCode != KnownTypeCode.Void)
					return (TypeCode)typeCode;
				else
					return TypeCode.Empty;
			}
			return TypeCode.Empty;
		}
		#endregion
	}
}
