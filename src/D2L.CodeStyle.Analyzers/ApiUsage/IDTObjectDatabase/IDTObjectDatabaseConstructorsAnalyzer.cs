using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using D2L.CodeStyle.Analyzers.Extensions;

namespace D2L.CodeStyle.Analyzers.ApiUsage.IDTObjectDatabase {
	[DiagnosticAnalyzer( LanguageNames.CSharp )]
	internal sealed class IDTObjectDatabaseConstructorsAnalyzer : DiagnosticAnalyzer {
		internal const string PROP_DB_TYPE = "DbType";

		public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
			=> ImmutableArray.Create( Diagnostics.ConstructorShouldTakeIDbT );

		public override void Initialize( AnalysisContext context ) {
			context.EnableConcurrentExecution();
			context.RegisterCompilationStartAction( RegisterAnalyzer );
		}

		public static void RegisterAnalyzer( CompilationStartAnalysisContext context ) {
			// Cache some important type lookups
			var dtoDbType = context.Compilation.GetTypeByMetadataName( "D2L.LP.LayeredArch.Data.IDTObjectDatabase" );
			var dbTType = context.Compilation.GetTypeByMetadataName( "D2L.LP.LayeredArch.Data.IDb`1" );
			var factoryType = context.Compilation.GetTypeByMetadataName( "D2L.LP.Extensibility.Activation.Domain.IFactory`1" );

			if( dtoDbType == null || dtoDbType.Kind == SymbolKind.ErrorType ) {
				return;
			}

			if( dbTType == null || dbTType.Kind == SymbolKind.ErrorType ) {
				return;
			}

			if( factoryType == null || factoryType.Kind == SymbolKind.ErrorType ) {
				return;
			}

			context.RegisterSyntaxNodeAction(
				ctx => AnalyzeConstruction(
					ctx,
					dtoDbType: dtoDbType,
					dbTType: dbTType,
					factoryType: factoryType
				),
				SyntaxKind.ObjectCreationExpression
			);
		}

		private static void AnalyzeConstruction(
			SyntaxNodeAnalysisContext context,
			INamedTypeSymbol dtoDbType,
			INamedTypeSymbol dbTType,
			INamedTypeSymbol factoryType
		) {
			var invocation = context.Node as ObjectCreationExpressionSyntax;

			if( invocation == null ) {
				return;
			}

			if( !IsInFactoryCreate( context.SemanticModel, factoryType, invocation ) ) {
				return;
			}

			ImmutableArray<( ArgumentSyntax arg, ITypeSymbol dbType )> dbTConvertedArgs =
				GetDbTConvertedArgs( context.SemanticModel, dtoDbType, dbTType, invocation ).ToImmutableArray();
			if( dbTConvertedArgs.Length == 0 ) {
				return;
			}

			foreach( var argDetails in dbTConvertedArgs ) {
				IParameterSymbol parameterSymbol = argDetails.arg.DetermineParameter( context.SemanticModel );
				if( parameterSymbol == null ) {
					continue;
				}

				ParameterSyntax parameterSyntax = parameterSymbol.GetDeclarationSyntax<ParameterSyntax>();
				Location typeLocation = parameterSyntax.Type.GetLocation();
				string dbTypeString = argDetails.dbType.ToDisplayString();

				context.ReportDiagnostic(
					Diagnostic.Create(
						Diagnostics.ConstructorShouldTakeIDbT,
						typeLocation,
						properties: ImmutableDictionary.Create<string, string>()
							.Add( PROP_DB_TYPE, dbTypeString )
					)
				);
			}
		}

		private static bool IsInFactoryCreate(
			SemanticModel model,
			INamedTypeSymbol factoryType,
			ObjectCreationExpressionSyntax invocation
		) {
			var containingMethod = invocation
				.FirstAncestorOrSelf<MethodDeclarationSyntax>();

			if( containingMethod == null ) {
				return false;
			}

			IMethodSymbol containingMethodSymbol = model.GetDeclaredSymbol( containingMethod );

			if( containingMethodSymbol.ExplicitInterfaceImplementations.Length == 0 ) {
				return false;
			}

			if( containingMethodSymbol.ExplicitInterfaceImplementations[0].OriginalDefinition.ContainingType != factoryType ) {
				return false;
			}

			return true;
		}

		private static IEnumerable<(ArgumentSyntax arg, ITypeSymbol dbType)> GetDbTConvertedArgs(
			SemanticModel model,
			INamedTypeSymbol dtoDbType,
			INamedTypeSymbol dbTType,
			ObjectCreationExpressionSyntax invocation
		) {
			ImmutableArray<IdentifierNameSyntax> variableArgs = invocation.ArgumentList.Arguments.Select( x => x.Expression ).OfType<IdentifierNameSyntax>().ToImmutableArray();

			foreach( IdentifierNameSyntax variableArg in variableArgs ) {
				TypeInfo varType = model.GetTypeInfo( variableArg );

				if( varType.Type?.OriginalDefinition != dbTType ) {
					continue;
				}

				if( varType.ConvertedType?.OriginalDefinition != dtoDbType ) {
					continue;
				}

				yield return ( variableArg.Parent as ArgumentSyntax, varType.Type );
			}
		}
	}
}