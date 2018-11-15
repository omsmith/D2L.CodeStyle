using System;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;

namespace D2L.CodeStyle.Analyzers.ApiUsage.IDTObjectDatabase {

	[ExportCodeFixProvider(
		LanguageNames.CSharp,
		Name = nameof( UselessDbAssignmentCodefix )
	)]
	public sealed class UselessDbAssignmentCodefix : CodeFixProvider {
		public override ImmutableArray<string> FixableDiagnosticIds
			=> ImmutableArray.Create(
				Diagnostics.UselessDbAssignment.Id
			);

		public override FixAllProvider GetFixAllProvider() {
			return WellKnownFixAllProviders.BatchFixer;
		}

		public override async Task RegisterCodeFixesAsync(
			CodeFixContext context
		) {
			var root = await context.Document
				.GetSyntaxRootAsync( context.CancellationToken )
				.ConfigureAwait( false ) as CompilationUnitSyntax;

			foreach( var diagnostic in context.Diagnostics ) {
				var declarationSpan = diagnostic.Location.SourceSpan;

				var localDeclaration = root.FindNode( declarationSpan ) as LocalDeclarationStatementSyntax;

				context.RegisterCodeFix(
					CodeAction.Create(
						title: "Remove useless variable",
						createChangedDocument: ct => Fix(
							context.Document,
							root,
							localDeclaration,
							ct
						)
					),
					diagnostic
				);
			}
		}

		private static async Task<Document> Fix(
			Document orig,
			CompilationUnitSyntax root,
			LocalDeclarationStatementSyntax localDeclaration,
			CancellationToken ct
		) {
			VariableDeclarationSyntax decl = localDeclaration.Declaration;

			SemanticModel model = await orig.GetSemanticModelAsync( ct ).ConfigureAwait( false );

			ISymbol declSymbol = model.GetDeclaredSymbol( decl.Variables.Single() );

			IdentifierNameSyntax dbTIdentifier = decl.Variables.Single().Initializer.ChildNodes().OfType<IdentifierNameSyntax>().Single();

			ImmutableArray<IdentifierNameSyntax> variableReferences = root
				.DescendantNodes()
				.OfType<IdentifierNameSyntax>()
				.Select( x => ( syntax: x, symbol: model.GetSymbolInfo( x ).Symbol ) )
				.Where( x => x.symbol?.OriginalDefinition == declSymbol.OriginalDefinition )
				.Select( x => x.syntax )
				.ToImmutableArray();

			if( variableReferences.Length == 0 ) {
				throw new Exception( "Found no references!" );
			}

			DocumentEditor editor = await DocumentEditor.CreateAsync( orig, ct ).ConfigureAwait( false );

			foreach( IdentifierNameSyntax variableReference in variableReferences ) {
				editor.ReplaceNode( variableReference, dbTIdentifier.WithTriviaFrom( variableReference ) );
			}

			editor.RemoveNode( localDeclaration, SyntaxRemoveOptions.KeepNoTrivia );

			return editor.GetChangedDocument();
		}

	}
}
