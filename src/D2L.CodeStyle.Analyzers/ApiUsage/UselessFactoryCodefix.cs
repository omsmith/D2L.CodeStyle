using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.FindSymbols;
using Microsoft.CodeAnalysis.Text;

namespace D2L.CodeStyle.Analyzers.ApiUsage {

	[ExportCodeFixProvider(
		LanguageNames.CSharp,
		Name = nameof( UselessFactoryCodefix )
	)]
	public sealed class UselessFactoryCodefix : CodeFixProvider {

		public override ImmutableArray<string> FixableDiagnosticIds
			=> ImmutableArray.Create(
				Diagnostics.UselessFactory.Id
			);

		public override FixAllProvider GetFixAllProvider() {
			return UselessFactoryFixAllProvider.Instance;
		}

		public override async Task RegisterCodeFixesAsync(
			CodeFixContext context
		) {
			var root = await context.Document
				.GetSyntaxRootAsync( context.CancellationToken )
				.ConfigureAwait( false ) as CompilationUnitSyntax;

			foreach( var diagnostic in context.Diagnostics ) {
				var identifierSpan = diagnostic.Location.SourceSpan;

				var decl = root.FindNode( identifierSpan ) as TypeDeclarationSyntax;

				if( diagnostic.Properties.TryGetValue( UselessFactoryAnalyzer.CONCRETE_TYPE_NAME_PROP, out string concreteTypeName ) ) {
					context.RegisterCodeFix(
						CodeAction.Create(
							title: $"Remove useless factory",
							createChangedSolution: ct => Fix(
								context.Document.Project.Solution,
								context.Document,
								root,
								decl,
								concreteTypeName,
								ct
							)
						),
						diagnostic
					);
				}
			}
		}

		private static async Task<Solution> Fix(
			Solution origSln,
			Document factoryDocument,
			CompilationUnitSyntax root,
			TypeDeclarationSyntax decl,
			string concreteTypeName,
			CancellationToken ct
		) {
			Solution sln = origSln;

			SemanticModel model = await factoryDocument
				.GetSemanticModelAsync( ct )
				.ConfigureAwait( false );

			INamedTypeSymbol factorySymbol = model.GetDeclaredSymbol( decl, ct );

			IEnumerable<ReferencedSymbol> factoryReferences = await SymbolFinder
				.FindReferencesAsync( factorySymbol, sln, ct )
				.ConfigureAwait( false );

			sln = sln.RemoveDocument( factoryDocument.Id );

			foreach( ReferencedSymbol reference in factoryReferences ) {
				foreach( ReferenceLocation refLocation in reference.Locations ) {
					if( refLocation.IsImplicit ) {
						return origSln;
					}

					Document refDocument = refLocation.Document;
					CompilationUnitSyntax refRoot = await refDocument
						.GetSyntaxRootAsync( ct )
						.ConfigureAwait( false ) as CompilationUnitSyntax;

					IdentifierNameSyntax referenceNode = refRoot.FindNode( refLocation.Location.SourceSpan ) as IdentifierNameSyntax;

					if( referenceNode == null ) {
						return origSln;
					}

					Document newRefDocument = await FixLoader( refDocument, refRoot, referenceNode, factorySymbol.ContainingNamespace, concreteTypeName, ct ).ConfigureAwait( false );

					if( newRefDocument == refDocument ) {
						return origSln;
					}

					sln = sln.WithDocumentSyntaxRoot(
						refDocument.Id,
						await newRefDocument.GetSyntaxRootAsync( ct ).ConfigureAwait( false )
					);
				}
			}

			return sln;
		}

		private static async Task<Document> FixLoader(
			Document orig,
			CompilationUnitSyntax root,
			IdentifierNameSyntax factoryReference,
			INamespaceSymbol factoryNamespace,
			string concreteTypeName,
			CancellationToken ct
		) {
			GenericNameSyntax registerFactoryCall = factoryReference.FirstAncestorOrSelf<GenericNameSyntax>();

			if( registerFactoryCall == null ) {
				return orig;
			}

			if( registerFactoryCall.Identifier.ToString() != "RegisterFactory" ) {
				return orig;
			}

			GenericNameSyntax registerCall = SyntaxFactory
				.GenericName( "Register" )
				.WithTypeArgumentList( SyntaxFactory.TypeArgumentList( SyntaxFactory.SeparatedList<TypeSyntax>( new[] {
					registerFactoryCall.TypeArgumentList.Arguments.First() as TypeSyntax,
					SyntaxFactory.IdentifierName( concreteTypeName ).WithTriviaFrom( registerFactoryCall.TypeArgumentList.Arguments.Last() )
				} ) ) )
				.WithTriviaFrom( registerFactoryCall );

			if( factoryNamespace != null ) {
				UsingDirectiveSyntax usingForFactoryNamespace = root.Usings.Where( x => x.Name.ToString() == factoryNamespace.ToDisplayString() ).FirstOrDefault();
				if( usingForFactoryNamespace != null ) {
					ImmutableArray<INamespaceSymbol> namespacesInUse = await GetNamespacesInUseAsync( orig, root, factoryReference, ct ).ConfigureAwait( false );

					if( !namespacesInUse.Contains( factoryNamespace ) ) {
						root = root.WithUsings( root.Usings.Remove( usingForFactoryNamespace ) );
					}
				}
			}

			root = root.ReplaceNode( registerFactoryCall, registerCall );

			return orig.WithSyntaxRoot( root );
		}

		private static async Task<ImmutableArray<INamespaceSymbol>> GetNamespacesInUseAsync(
			Document document,
			CompilationUnitSyntax root,
			IdentifierNameSyntax factoryReference,
			CancellationToken ct
		) {
			SemanticModel model = await document.GetSemanticModelAsync( ct ).ConfigureAwait( false );

			return root
				.DescendantNodes()
				.OfType<IdentifierNameSyntax>()
				.Where( x => x != factoryReference )
				.Select( x => model.GetSymbolInfo( x, ct ).Symbol?.ContainingNamespace )
				.Where( x => x != null )
				.Distinct()
				.ToImmutableArray();
		}
	}
}