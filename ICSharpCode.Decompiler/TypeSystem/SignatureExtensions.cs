using System;
using System.Linq;
using dnlib.DotNet;
using ICSharpCode.Decompiler.TypeSystem.Implementation;
using ICSharpCode.Decompiler.Util;

namespace ICSharpCode.Decompiler.TypeSystem
{
	internal static class SignatureExtensions
	{
		internal static IType DecodeSignature(this TypeSig sig, MetadataModule module, GenericContext context)
		{
			if (sig is null)
				return null;

			switch (sig) {
				case CorLibTypeSig corLibTypeSig:
					return corLibTypeSig.TypeDefOrRef.DecodeSignature(module, context, IsValueType(corLibTypeSig));
				case GenericMVar mVar:
					// TODO: store OriginalMember
					return context.GetMethodTypeParameter((int)mVar.Number);
				case GenericVar tVar:
					// TODO: store OriginalMember
					return context.GetClassTypeParameter((int)tVar.Number);
				case FnPtrSig fnPtr: {
					// pointers to member functions are not supported even in C# 9
					if (!fnPtr.Signature.HasThis && fnPtr.Signature is MethodBaseSig mSig) {
						var retType = mSig.RetType.DecodeSignature(module, context);
						var paramTypes = mSig.Params.Select(t => t.DecodeSignature(module, context)).ToList();
						var tsFnPtr = FunctionPointerType.FromSignature(retType, paramTypes, mSig.CallingConvention, module);
						tsFnPtr.MetadataToken = tsFnPtr.OriginalMember = fnPtr;
						return tsFnPtr;
					}

					return module.Compilation.FindType(KnownTypeCode.IntPtr);
				}
				case GenericInstSig instSig:
					return new ParameterizedType(instSig.GenericType.DecodeSignature(module, context),
							instSig.GenericArguments.Select(x => x.DecodeSignature(module, context)))
						{ OriginalMember = instSig, MetadataToken = instSig };
				case ByRefSig byRefSig:
					return new ByReferenceType(byRefSig.Next.DecodeSignature(module, context))
						{ OriginalMember = byRefSig, MetadataToken = byRefSig };
				case PinnedSig pinnedSig:
					return new PinnedType(pinnedSig.Next.DecodeSignature(module, context))
						{ OriginalMember = pinnedSig, MetadataToken = pinnedSig };
				case CModOptSig cModOptSig:
					return new ModifiedType(cModOptSig.Modifier.DecodeSignature(module, context),
							cModOptSig.Next.DecodeSignature(module, context), false)
						{ OriginalMember = cModOptSig, MetadataToken = cModOptSig };
				case CModReqdSig cModReqdSig:
					return new ModifiedType(cModReqdSig.Modifier.DecodeSignature(module, context),
							cModReqdSig.Next.DecodeSignature(module, context), true)
						{ OriginalMember = cModReqdSig, MetadataToken = cModReqdSig };
				case PtrSig ptrSig:
					return new PointerType(ptrSig.Next.DecodeSignature(module, context))
						{ OriginalMember = ptrSig, MetadataToken = ptrSig };
				case ArraySigBase arraySigBase:
					return new ArrayType(module.Compilation, arraySigBase.Next.DecodeSignature(module, context),
						(int)arraySigBase.Rank) { OriginalMember = arraySigBase, MetadataToken = arraySigBase };
				case ClassOrValueTypeSig classOrValueTypeSig:
					ThreeState isVT = ThreeState.Unknown;
					if (classOrValueTypeSig is ClassSig)
						isVT = ThreeState.False;
					else if (classOrValueTypeSig is ValueTypeSig)
						isVT = ThreeState.True;

					return classOrValueTypeSig.TypeDefOrRef.DecodeSignature(module, context, isVT);
				default:
					throw new ArgumentOutOfRangeException();
			}
		}

		internal static IType DecodeSignature(this ITypeDefOrRef typeDefOrRef, MetadataModule module, GenericContext context, ThreeState isVT = ThreeState.Unknown)
		{
			if (typeDefOrRef is null)
				return null;

			if (typeDefOrRef is TypeSpec spec) {
				return spec.TypeSig.DecodeSignature(module, context);
			}

			if (typeDefOrRef is TypeDef def) {
				return module.GetDefinition(def);
			}
			if (typeDefOrRef is TypeRef typeRef) {
				lock (module.typeRefDict) {
					if (module.typeRefDict.TryGetValue(typeRef, out var tsType))
						return tsType;

					var resolved = typeRef.Resolve();
					if (resolved != null && module.Compilation.GetOrAddModule(resolved.Module) is MetadataModule mod)
						tsType = mod.GetDefinitionInternal(resolved).WithOriginalMember(typeRef);
					else
					{
						Console.WriteLine("Failed to resolve TypeRef: {0}", typeRef);
						bool? isReferenceType;
						if (isVT != ThreeState.Unknown)
							isReferenceType = isVT == ThreeState.False;
						else
							isReferenceType = null;
						tsType = new UnknownType(typeRef.GetFullTypeName(), isReferenceType) { OriginalMember = typeRef };
					}

					return module.typeRefDict[typeRef] = tsType;
				}
			}

			throw new InvalidOperationException();
		}

		private static ThreeState IsValueType(CorLibTypeSig corlib) {
			switch (corlib.ElementType) {
				case ElementType.Void:
				case ElementType.Boolean:
				case ElementType.Char:
				case ElementType.I1:
				case ElementType.U1:
				case ElementType.I2:
				case ElementType.U2:
				case ElementType.I4:
				case ElementType.U4:
				case ElementType.I8:
				case ElementType.U8:
				case ElementType.R4:
				case ElementType.R8:
				case ElementType.TypedByRef:
				case ElementType.I:
				case ElementType.U:
				case ElementType.R:
					return ThreeState.True;
				case ElementType.String:
				case ElementType.Object:
					return ThreeState.False;
				default:
					return ThreeState.Unknown;
			}
		}
	}
}
