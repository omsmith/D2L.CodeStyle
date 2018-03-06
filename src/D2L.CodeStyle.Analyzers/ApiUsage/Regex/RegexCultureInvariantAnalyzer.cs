using System.Collections.Immutable;
using D2L.CodeStyle.Analyzers.Extensions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace D2L.CodeStyle.Analyzers.ApiUsage.Regex {

	[DiagnosticAnalyzer( LanguageNames.CSharp )]
	public class RegexCultureInvariantAnalyzer : DiagnosticAnalyzer {

		public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
			=> ImmutableArray.Create( Diagnostics.RegexCultureInvariant );

		public override void Initialize( AnalysisContext context ) {
			context.EnableConcurrentExecution();
			context.RegisterCompilationStartAction( RegisterAnalysis );
		}

		private void RegisterAnalysis( CompilationStartAnalysisContext context ) {

			INamedTypeSymbol regexType = context.Compilation.GetTypeByMetadataName( "System.Text.RegularExpressions.Regex" );
			if( regexType.IsNullOrErrorType() ) {
				return;
			}

			context.RegisterSyntaxNodeAction(
				ctx => AnalyzeCreation( ctx, regexType ),
				SyntaxKind.ObjectCreationExpression
			);

			context.RegisterSyntaxNodeAction(
				ctx => AnalyzeInvocation( ctx, regexType ),
				SyntaxKind.InvocationExpression
			);
		}

		private void AnalyzeCreation( SyntaxNodeAnalysisContext context, INamedTypeSymbol regexType ) {
			ObjectCreationExpressionSyntax node = context.Node as ObjectCreationExpressionSyntax;
			if( node == null ) {
				return;
			}

			if( node.Type.ToString() != "Regex" ) {
				return;
			}

			INamedTypeSymbol actualType = context.SemanticModel.GetTypeInfo( node ).Type as INamedTypeSymbol;
			if( actualType.IsNullOrErrorType() ) {
				return;
			}

			if( actualType != regexType ) {
				return;
			}

			ArgumentListSyntax args = node.ArgumentList;
			if( args.ToString().Contains( "RegexOptions.CultureInvariant" ) ) {
				return;
			}

			Diagnostic diagnostic = Diagnostic.Create( Diagnostics.RegexCultureInvariant, node.GetLocation() );
			context.ReportDiagnostic( diagnostic );
		}

		private void AnalyzeInvocation( SyntaxNodeAnalysisContext context, INamedTypeSymbol regexType ) {
			InvocationExpressionSyntax node = context.Node as InvocationExpressionSyntax;
			if( node == null ) {
				return;
			}

			MemberAccessExpressionSyntax memberNode = node.Expression as MemberAccessExpressionSyntax;
			if( memberNode == null ) {
				return;
			}

			string memberName = memberNode.Name.ToString();
			if( memberName != "Match"
				&& memberName != "Matches"
				&& memberName != "IsMatch"
			) {
				return;
			}

			IMethodSymbol memberSymbol = context.SemanticModel.GetSymbolInfo( memberNode ).Symbol as IMethodSymbol;
			if( memberSymbol.IsNullOrErrorType() ) {
				return;
			}

			if( !memberSymbol.IsStatic ) {
				return;
			}

			ExpressionSyntax maybeRegexNode = memberNode.Expression;
			if( maybeRegexNode == null ) {
				return;
			}

			INamedTypeSymbol actualType = context.SemanticModel.GetTypeInfo( maybeRegexNode ).Type as INamedTypeSymbol;
			if( actualType.IsNullOrErrorType() ) {
				return;
			}

			if( actualType != regexType ) {
				return;
			}

			ArgumentListSyntax args = node.ArgumentList;
			if( args.ToString().Contains( "RegexOptions.CultureInvariant" ) ) {
				return;
			}

			Diagnostic diagnostic = Diagnostic.Create( Diagnostics.RegexCultureInvariant, node.GetLocation() );
			context.ReportDiagnostic( diagnostic );
		}

	}

}
