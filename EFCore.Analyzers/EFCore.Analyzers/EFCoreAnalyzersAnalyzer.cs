// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Collections.Immutable;
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
                "SQL expression contains interpolated strings that will not be parameterized. Review for SQL injection vulnerability.",
                "SQL expression contains interpolated strings that will not be parameterized. Review for SQL injection vulnerability.",
                "Security",
                DiagnosticSeverity.Warning,
                isEnabledByDefault: true);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics 
            => ImmutableArray.Create(_diagnosticDescriptor);

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction(AnalyzeSyntax, SyntaxKind.SimpleMemberAccessExpression);
        }

        private static void AnalyzeSyntax(SyntaxNodeAnalysisContext context)
        {
            var memberAccessExpressionSyntax = (MemberAccessExpressionSyntax)context.Node;
            var identifierValueText = memberAccessExpressionSyntax.Name.Identifier.ValueText;

            if (identifierValueText == "FromSql" 
                || identifierValueText == "ExecuteSqlCommand"
                || identifierValueText == "ExecuteSqlCommandAsync")
            {
                
                
                //var diagnostic = Diagnostic.Create(Rule, memberAccessExpressionSyntax.Locations[0], methodSymbol.Name);

                Console.WriteLine();

                //context.ReportDiagnostic(diagnostic);
            }
        }
    }
}
