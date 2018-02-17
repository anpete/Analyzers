// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace EFCore.Analyzers
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class SqlInjectionDiagnosticAnalyzer : DiagnosticAnalyzer
    {
        public const string Id = "EF1000";

        private static readonly DiagnosticDescriptor _diagnosticDescriptor
            = new DiagnosticDescriptor(
                Id,
                "Possible SQL injection vulnerability.",
                "The SQL expression passed to '{0}' embeds data that will not be parameterized. Review for SQL injection vulnerability.",
                "Security",
                DiagnosticSeverity.Warning,
                isEnabledByDefault: true);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
            => ImmutableArray.Create(_diagnosticDescriptor);

        public override void Initialize(AnalysisContext analysisContext)
        {
            analysisContext.RegisterSyntaxNodeAction(
                AnalyzeSimpleMemberAccessExpressionSyntaxNode, SyntaxKind.SimpleMemberAccessExpression);
        }

        private static void AnalyzeSimpleMemberAccessExpressionSyntaxNode(SyntaxNodeAnalysisContext syntaxNodeAnalysisContext)
        {
            if (!(syntaxNodeAnalysisContext.Node is MemberAccessExpressionSyntax memberAccessExpressionSyntax))
            {
                return;
            }

            if (!(memberAccessExpressionSyntax.Parent is InvocationExpressionSyntax invocationExpressionSyntax))
            {
                return;
            }

            var identifierValueText = memberAccessExpressionSyntax.Name.Identifier.ValueText;

            if (!(identifierValueText == "FromSql"
                  || identifierValueText == "ExecuteSqlCommand"
                  || identifierValueText == "ExecuteSqlCommandAsync"))
            {
                return;
            }

            var symbolInfo
                = syntaxNodeAnalysisContext.SemanticModel
                    .GetSymbolInfo(memberAccessExpressionSyntax, syntaxNodeAnalysisContext.CancellationToken);

            if (symbolInfo.Symbol is IMethodSymbol methodSymbol
                && (methodSymbol.ContainingType.Name == "RelationalQueryableExtensions"
                    || methodSymbol.ContainingType.Name == "RelationalDatabaseFacadeExtensions"))
            {
                var sqlArgumentExpressionSyntax = invocationExpressionSyntax.ArgumentList.Arguments[0].Expression;

                switch (sqlArgumentExpressionSyntax)
                {
                    case LiteralExpressionSyntax _:
                        return;
                    case InterpolatedStringExpressionSyntax _:
                        return;
                }

                if (sqlArgumentExpressionSyntax.DescendantNodes()
                    .Any(sn => sn is InterpolatedStringExpressionSyntax interpolatedStringExpressionSyntax
                        && interpolatedStringExpressionSyntax.DescendantNodes().Any(n => n is InterpolationSyntax)))
                {
                    var diagnostic
                        = Diagnostic.Create(
                            _diagnosticDescriptor,
                            memberAccessExpressionSyntax.GetLocation(),
                            identifierValueText);

                    syntaxNodeAnalysisContext.ReportDiagnostic(diagnostic);
                }
            }
        }
    }
}
