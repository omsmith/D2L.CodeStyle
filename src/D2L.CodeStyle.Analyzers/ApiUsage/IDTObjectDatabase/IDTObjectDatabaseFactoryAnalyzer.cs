using System;
using System.Collections.Immutable;
using System.Linq;
using D2L.CodeStyle.Analyzers.Extensions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace D2L.CodeStyle.Analyzers.ApiUsage.IDTObjectDatabase {
	[DiagnosticAnalyzer( LanguageNames.CSharp )]
	public sealed class IDTObjectDatabaseFactoryAnalyzer : DiagnosticAnalyzer {
		public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
			=> ImmutableArray.Create( Diagnostics.UseAnIDbTInstead );

		public override void Initialize( AnalysisContext context ) {
			context.EnableConcurrentExecution();
			context.RegisterCompilationStartAction( RegisterSingletonLocatorAnalyzer );
		}

		public void RegisterSingletonLocatorAnalyzer( CompilationStartAnalysisContext context ) {
			// Cache some important type lookups
			var factoryType = context.Compilation.GetTypeByMetadataName( "D2L.LP.LayeredArch.Data.IDTObjectDatabaseFactory" );
			var factoryExtensionsType = context.Compilation.GetTypeByMetadataName( "D2L.LP.LayeredArch.Data.IDTObjectDatabaseFactoryExtensions" );
			var factoryConcreteType = context.Compilation.GetTypeByMetadataName( "D2L.LP.LayeredArch.Data.DatabaseFactory" );

			// If this type lookup failed then SingletonLocator cannot resolve
			// and we don't need to register our analyzer.
			if( factoryType.IsNullOrErrorType() && factoryExtensionsType.IsNullOrErrorType() && factoryConcreteType.IsNullOrErrorType() ) {
				return;
			}

			context.RegisterSyntaxNodeAction(
				ctx => EnforceSingletonsOnly(
					ctx,
					IsDbFactory
				),
				SyntaxKind.SimpleMemberAccessExpression,
				SyntaxKind.InvocationExpression
			);

			bool IsDbFactory( INamedTypeSymbol other ) {
				if( !factoryType.IsNullOrErrorType() && other == factoryType ) {
					return true;
				}

				if( !factoryExtensionsType.IsNullOrErrorType() && other == factoryExtensionsType ) {
					return true;
				}

				if( !factoryConcreteType.IsNullOrErrorType() && other == factoryConcreteType ) {
					return true;
				}

				return false;
			}
		}

		// Enforce that SingletonLocator can only load actual [Singleton]s
		private static void EnforceSingletonsOnly(
			SyntaxNodeAnalysisContext context,
			Func<INamedTypeSymbol, bool> isDbFactory
		) {
			var root = GetRootNode( context );
			if( root == null ) {
				return;
			}
			var symbolinfo = context.SemanticModel.GetSymbolInfo( root );

			var method = symbolinfo.Symbol as IMethodSymbol;

			if( method == null ) {
				if( symbolinfo.CandidateSymbols == null ) {
					return;
				}

				if( symbolinfo.CandidateSymbols.Length != 1 ) {
					return;
				}

				//This happens on method groups, such as
				//  Func<IFoo> fooFunc = OldAndBrokenServiceLocator.Get<IFoo>;
				method = symbolinfo.CandidateSymbols.First() as IMethodSymbol;

				if( method == null ) {
					return;
				}
			}

			// At this point method is a non-null IMethodSymbol
			if( !isDbFactory( method.ContainingType ) ) {
				return;

			}

			if( !IsDbCreate( method ) ) {
				return;
			}

			if( !TryGetDbType( context.SemanticModel, root, out GenericNameSyntax dbTypeReference ) ) {
				return;
			}

			context.ReportDiagnostic(
				Diagnostic.Create( Diagnostics.UseAnIDbTInstead, context.Node.GetLocation() )
			);
		}

		private static InvocationExpressionSyntax GetRootNode( SyntaxNodeAnalysisContext context ) {
			InvocationExpressionSyntax root = context.Node as InvocationExpressionSyntax;
			if( root != null ) {
				return root;
			}

			return null;
		}

		private static bool IsDbCreate( IMethodSymbol method ) {
			return ( "Create".Equals( method.Name ) || "CreateDTObjectDatabase".Equals( method.Name ) )
				&& !method.IsGenericMethod
				&& method.Parameters.Length == 1;
		}

		private static bool TryGetSplitName( SemanticModel model, ExpressionSyntax syntaxNode, out string splitName ) {
			if( !( syntaxNode is InvocationExpressionSyntax invocationSyntaxNode ) ) {
				splitName = null;
				return false;
			}

			SeparatedSyntaxList<ArgumentSyntax> arguments = invocationSyntaxNode.ArgumentList.Arguments;
			if( arguments.Count != 1 ) {
				splitName = null;
				return false;
			}

			ExpressionSyntax splitNameArgument = arguments[ 0 ].Expression;

			Optional<object> maybeSplitName = model.GetConstantValue( splitNameArgument );
			if( !maybeSplitName.HasValue ) {
				splitName = null;
				return false;
			}

			splitName = ( string )maybeSplitName.Value;
			return true;
		}

		internal static bool TryGetDbType( SemanticModel model, ExpressionSyntax syntaxNode, out GenericNameSyntax syntax ) {
			if( !TryGetSplitName( model, syntaxNode, out string splitName ) ) {
				syntax = null;
				return false;
			}

			if( splitName.Equals( "Main", StringComparison.OrdinalIgnoreCase ) ) {
				syntax = ( GenericNameSyntax )SyntaxFactory.ParseExpression( "IDb<Split.Main>" );
				return true;
			}

			if( splitName.Equals( "Analytics", StringComparison.OrdinalIgnoreCase ) ) {
				syntax = ( GenericNameSyntax )SyntaxFactory.ParseExpression( "IDb<Split.Analytics>" );
				return true;
			}

			if( splitName.Equals( "Logging", StringComparison.OrdinalIgnoreCase ) ) {
				syntax = ( GenericNameSyntax )SyntaxFactory.ParseExpression( "IDb<Split.Logging>" );
				return true;
			}

			if( splitName.Equals( "Lor", StringComparison.OrdinalIgnoreCase ) ) {
				syntax = ( GenericNameSyntax )SyntaxFactory.ParseExpression( "IDb<Split.Lor>" );
				return true;
			}

			if( splitName.Equals( "HoldingTank", StringComparison.OrdinalIgnoreCase ) ) {
				syntax = ( GenericNameSyntax )SyntaxFactory.ParseExpression( "IDb<Split.HoldingTank>" );
				return true;
			}

			if( splitName.Equals( "Reporting", StringComparison.OrdinalIgnoreCase ) ) {
				syntax = ( GenericNameSyntax )SyntaxFactory.ParseExpression( "IDb<Split.Reporting>" );
				return true;
			}

			if( splitName.Equals( "Warehouse", StringComparison.OrdinalIgnoreCase ) ) {
				syntax = ( GenericNameSyntax )SyntaxFactory.ParseExpression( "IDb<Split.Warehouse>" );
				return true;
			}

			syntax = null;
			return false;
		}

		internal static bool TryGetDbFactoryInvocation( SemanticModel model, ExpressionSyntax syntaxNode, out ExpressionSyntax syntax ) {
			if( !TryGetSplitName( model, syntaxNode, out string splitName ) ) {
				syntax = null;
				return false;
			}

			if( splitName.Equals( "Main", StringComparison.OrdinalIgnoreCase ) ) {
				syntax = SyntaxFactory.ParseExpression( "DbFactory.Create<Split.Main>()" );
				return true;
			}

			if( splitName.Equals( "Analytics", StringComparison.OrdinalIgnoreCase ) ) {
				syntax = SyntaxFactory.ParseExpression( "DbFactory.Create<Split.Analytics>()" );
				return true;
			}

			if( splitName.Equals( "Logging", StringComparison.OrdinalIgnoreCase ) ) {
				syntax = SyntaxFactory.ParseExpression( "DbFactory.Create<Split.Logging>()" );
				return true;
			}

			if( splitName.Equals( "Lor", StringComparison.OrdinalIgnoreCase ) ) {
				syntax = SyntaxFactory.ParseExpression( "DbFactory.Create<Split.Lor>()" );
				return true;
			}

			if( splitName.Equals( "HoldingTank", StringComparison.OrdinalIgnoreCase ) ) {
				syntax = SyntaxFactory.ParseExpression( "DbFactory.Create<Split.HoldingTank>()" );
				return true;
			}

			if( splitName.Equals( "Reporting", StringComparison.OrdinalIgnoreCase ) ) {
				syntax = SyntaxFactory.ParseExpression( "DbFactory.Create<Split.Reporting>()" );
				return true;
			}

			if( splitName.Equals( "Warehouse", StringComparison.OrdinalIgnoreCase ) ) {
				syntax = SyntaxFactory.ParseExpression( "DbFactory.Create<Split.Warehouse>()" );
				return true;
			}

			syntax = null;
			return false;
		}

	}
}