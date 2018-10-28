using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace D2L.CodeStyle.Analyzers.ApiUsage {
	[DiagnosticAnalyzer( LanguageNames.CSharp )]
	internal sealed class UselessDbAssignmentAnalyzer : DiagnosticAnalyzer {
		public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
			=> ImmutableArray.Create( Diagnostics.UselessDbAssignment );

		public override void Initialize( AnalysisContext context ) {
			// context.EnableConcurrentExecution();
			context.RegisterCompilationStartAction( RegisterUselessDbAssignmentAnalyzer );
		}

		public static void RegisterUselessDbAssignmentAnalyzer( CompilationStartAnalysisContext context ) {
			// Cache some important type lookups
			var dtoDbType = context.Compilation.GetTypeByMetadataName( "D2L.LP.LayeredArch.Data.IDTObjectDatabase" );
			var dbTType = context.Compilation.GetTypeByMetadataName( "D2L.LP.LayeredArch.Data.IDb`1" );

			if( dtoDbType == null || dtoDbType.Kind == SymbolKind.ErrorType ) {
				return;
			}

			if( dbTType == null || dbTType.Kind == SymbolKind.ErrorType ) {
				return;
			}

			context.RegisterSyntaxNodeAction(
				ctx => AnalyzeDeclaration(
					ctx,
					dtoDbType: dtoDbType,
					dbTType: dbTType
				),
				SyntaxKind.LocalDeclarationStatement
			);
		}

		private static void AnalyzeDeclaration(
			SyntaxNodeAnalysisContext context,
			INamedTypeSymbol dtoDbType,
			INamedTypeSymbol dbTType
		) {
			var localDeclaration = context.Node as LocalDeclarationStatementSyntax;
			var declaration = localDeclaration.Declaration;

			if( declaration == null ) {
				return;
			}

			if( !CheckDeclarationIsDb( context.SemanticModel, declaration, dtoDbType, dbTType ) ) {
				return;
			}

			context.ReportDiagnostic(
				Diagnostic.Create( Diagnostics.UselessDbAssignment, localDeclaration.GetLocation() )
			);
		}

		private static bool CheckDeclarationIsDb(
			SemanticModel model,
			VariableDeclarationSyntax declaration,
			INamedTypeSymbol dtoDbType,
			INamedTypeSymbol dbTType
		) {
			ITypeSymbol variableType = model.GetTypeInfo( declaration.Type ).Type?.OriginalDefinition;

			if( variableType != dtoDbType ) {
				return false;
			}

			EqualsValueClauseSyntax initializer = declaration.Variables.SingleOrDefault()?.Initializer;
			IdentifierNameSyntax initializingVariable = initializer.ChildNodes().OfType<IdentifierNameSyntax>().SingleOrDefault();

			if( initializingVariable == null ) {
				return false;
			}

			ITypeSymbol initializingVariableType = model.GetTypeInfo( initializingVariable ).Type?.OriginalDefinition;

			if( initializingVariableType != dbTType ) {
				return false;
			}

			return true;
		}
	}
}