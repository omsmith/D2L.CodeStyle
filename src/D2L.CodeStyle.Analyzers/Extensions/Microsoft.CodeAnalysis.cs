using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;

namespace D2L.CodeStyle.Analyzers.Extensions {
	internal static partial class RoslynExtensions {
		/// <summary>
		/// A list of marked immutable types owned externally.
		/// </summary>
		private static readonly ImmutableHashSet<string> MarkedImmutableTypes = ImmutableHashSet.Create(
			"System.StringComparer",
			"System.Text.ASCIIEncoding",
			"System.Text.Encoding",
			"System.Text.UTF8Encoding"
		);

		public static bool IsTypeMarkedImmutable( this ITypeSymbol symbol ) {
			if( symbol.IsExternallyOwnedMarkedImmutableType() ) {
				return true;
			}
			if( symbol.IsMarkedImmutableGeneric() ) {
				return true;
			}
			if( Attributes.Objects.Immutable.IsDefined( symbol ) ) {
				return true;
			}
			if( symbol.Interfaces.Any( IsTypeMarkedImmutable ) ) {
				return true;
			}
			if( symbol.BaseType != null && IsTypeMarkedImmutable( symbol.BaseType ) ) {
				return true;
			}
			return false;
		}

		public static bool IsExternallyOwnedMarkedImmutableType( this ITypeSymbol symbol ) {
			return MarkedImmutableTypes.Contains( symbol.GetFullTypeName() );
		}

		private static bool IsMarkedImmutableGeneric( this ITypeSymbol symbol ) {
			var type = symbol as INamedTypeSymbol;
			if( type == null ) {
				return false;
			}

			if( !type.IsGenericType ) {
				return false;
			}

			/*  We can have an annotation in:
			 *  (1) symbol's assembly,
			 *  (2) any of symbol's type arguments's assemblies
			 */
			if( type.ContainingAssembly.HasImmutableGenericAnnotation( type ) ) {
				return true;
			}
			foreach( var typeArgument in type.TypeArguments ) {
				if( typeArgument.ContainingAssembly.HasImmutableGenericAnnotation( type ) ) {
					return true;
				}
			}

			return false;
		}

		private static bool HasImmutableGenericAnnotation( this IAssemblySymbol assembly, INamedTypeSymbol type) {
			var attributes = Attributes.Objects.ImmutableGeneric.GetAll( assembly );
			foreach( var attr in attributes ) {

				if( attr.ConstructorArguments.Length != 1 ) {
					continue;
				}

				var arg = attr.ConstructorArguments[0];
				if( arg.Value.Equals( type ) ) {
					return true;
				}
			}

			return false;
		}

		public static bool IsTypeMarkedSingleton( this ITypeSymbol symbol ) {
			if( Attributes.Singleton.IsDefined( symbol ) ) {
				return true;
			}
			if( symbol.Interfaces.Any( IsTypeMarkedSingleton ) ) {
				return true;
			}
			if( symbol.BaseType != null && IsTypeMarkedSingleton( symbol.BaseType ) ) {
				return true;
			}
			return false;
		}

		private static readonly SymbolDisplayFormat FullTypeDisplayFormat = new SymbolDisplayFormat(
			typeQualificationStyle: SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces,
			miscellaneousOptions: SymbolDisplayMiscellaneousOptions.ExpandNullable
		);

		private static readonly SymbolDisplayFormat FullTypeWithGenericsDisplayFormat = new SymbolDisplayFormat(
			typeQualificationStyle: SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces,
			genericsOptions: SymbolDisplayGenericsOptions.IncludeTypeParameters,
			miscellaneousOptions: SymbolDisplayMiscellaneousOptions.ExpandNullable
		);

		public static string GetFullTypeName( this ITypeSymbol symbol ) {
			var fullyQualifiedName = symbol.ToDisplayString( FullTypeDisplayFormat );
			return fullyQualifiedName;
		}


		public static string GetFullTypeNameWithGenericArguments( this ITypeSymbol symbol ) {
			var fullyQualifiedName = symbol.ToDisplayString( FullTypeWithGenericsDisplayFormat );
			return fullyQualifiedName;
		}

		public static IEnumerable<ISymbol> GetExplicitNonStaticMembers( this ITypeSymbol type ) {
			return type.GetMembers()
				.Where( t => !t.IsStatic && !t.IsImplicitlyDeclared );
		}

		public static bool IsNullOrErrorType( this ITypeSymbol symbol ) {
			if( symbol == null ) {
				return true;
			}
			if( symbol.Kind == SymbolKind.ErrorType ) {
				return true;
			}
			if( symbol.TypeKind == TypeKind.Error ) {
				return true;
			}

			return false;
		}

		public static bool IsNullOrErrorType( this ISymbol symbol ) {
			if( symbol == null ) {
				return true;
			}
			if( symbol.Kind == SymbolKind.ErrorType ) {
				return true;
			}

			return false;
		}
	}
}
