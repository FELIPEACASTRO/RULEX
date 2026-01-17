/*
 * AUTO-GENERATED FILE.
 * Source: docs/DIAGRAMS_RULEX_INVENTORY.json
 * Do not edit manually. Run: pnpm diagrams:sync-inventory
 */
export const RULEX_INVENTORY = {
  "generatedAt": "2026-01-16T21:57:10.121Z",
  "repoRoot": "RULEX",
  "backend": {
    "controllers": [
      {
        "className": "RulesV31Controller",
        "basePath": "/rules",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/v31/rules/RulesV31Controller.java",
          "line": 19
        },
        "endpoints": [
          {
            "httpMethod": "POST",
            "path": "/rules/validate",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/v31/rules/RulesV31Controller.java",
              "line": 33,
              "snippet": "@PostMapping(\"/validate\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/rules/lint",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/v31/rules/RulesV31Controller.java",
              "line": 39,
              "snippet": "@PostMapping(\"/lint\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/rules/simulate",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/v31/rules/RulesV31Controller.java",
              "line": 50,
              "snippet": "@PostMapping(\"/simulate\")"
            }
          }
        ]
      },
      {
        "className": "FieldDictionaryController",
        "basePath": "/field-dictionary",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/v31/field/FieldDictionaryController.java",
          "line": 14
        },
        "endpoints": [
          {
            "httpMethod": "GET",
            "path": "/field-dictionary",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/v31/field/FieldDictionaryController.java",
              "line": 26,
              "snippet": "@GetMapping"
            }
          }
        ]
      },
      {
        "className": "AuditController",
        "basePath": "/audit",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/controller/AuditController.java",
          "line": 30
        },
        "endpoints": [
          {
            "httpMethod": "GET",
            "path": "/audit",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/AuditController.java",
              "line": 44,
              "snippet": "@GetMapping"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/audit/export",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/AuditController.java",
              "line": 81,
              "snippet": "@GetMapping(value = \"/export\", produces = MediaType.APPLICATION_JSON_VALUE)"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/audit/export/csv",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/AuditController.java",
              "line": 113,
              "snippet": "@GetMapping(value = \"/export/csv\", produces = \"text/csv\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/audit/transaction/{transactionId}",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/AuditController.java",
              "line": 162,
              "snippet": "@GetMapping(\"/transaction/{transactionId}\")"
            }
          }
        ]
      },
      {
        "className": "ComplexRuleCrudController",
        "basePath": "/complex-rules",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/controller/ComplexRuleCrudController.java",
          "line": 21
        },
        "endpoints": [
          {
            "httpMethod": "GET",
            "path": "/complex-rules",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/ComplexRuleCrudController.java",
              "line": 33,
              "snippet": "@GetMapping"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/complex-rules/{id}",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/ComplexRuleCrudController.java",
              "line": 46,
              "snippet": "@GetMapping(\"/{id}\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/complex-rules/key/{key}",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/ComplexRuleCrudController.java",
              "line": 60,
              "snippet": "@GetMapping(\"/key/{key}\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/complex-rules",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/ComplexRuleCrudController.java",
              "line": 74,
              "snippet": "@PostMapping"
            }
          },
          {
            "httpMethod": "PUT",
            "path": "/complex-rules/{id}",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/ComplexRuleCrudController.java",
              "line": 84,
              "snippet": "@PutMapping(\"/{id}\")"
            }
          },
          {
            "httpMethod": "DELETE",
            "path": "/complex-rules/{id}",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/ComplexRuleCrudController.java",
              "line": 99,
              "snippet": "@DeleteMapping(\"/{id}\")"
            }
          },
          {
            "httpMethod": "PATCH",
            "path": "/complex-rules/{id}/toggle",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/ComplexRuleCrudController.java",
              "line": 116,
              "snippet": "@PatchMapping(\"/{id}/toggle\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/complex-rules/{id}/duplicate",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/ComplexRuleCrudController.java",
              "line": 133,
              "snippet": "@PostMapping(\"/{id}/duplicate\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/complex-rules/validate",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/ComplexRuleCrudController.java",
              "line": 147,
              "snippet": "@PostMapping(\"/validate\")"
            }
          }
        ]
      },
      {
        "className": "EvaluateController",
        "basePath": "/evaluate",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/controller/EvaluateController.java",
          "line": 26
        },
        "endpoints": [
          {
            "httpMethod": "POST",
            "path": "/evaluate",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/EvaluateController.java",
              "line": 55,
              "snippet": "@PostMapping"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/evaluate/raw",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/EvaluateController.java",
              "line": 96,
              "snippet": "@PostMapping(\"/raw\")"
            }
          }
        ]
      },
      {
        "className": "MetricsController",
        "basePath": "/metrics",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/controller/MetricsController.java",
          "line": 14
        },
        "endpoints": [
          {
            "httpMethod": "GET",
            "path": "/metrics",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/MetricsController.java",
              "line": 26,
              "snippet": "@GetMapping"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/metrics/mcc",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/MetricsController.java",
              "line": 40,
              "snippet": "@GetMapping(\"/mcc\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/metrics/merchant",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/MetricsController.java",
              "line": 54,
              "snippet": "@GetMapping(\"/merchant\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/metrics/timeline",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/MetricsController.java",
              "line": 66,
              "snippet": "@GetMapping(\"/timeline\")"
            }
          }
        ]
      },
      {
        "className": "RuleApprovalController",
        "basePath": "/rules/approvals",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/controller/RuleApprovalController.java",
          "line": 26
        },
        "endpoints": [
          {
            "httpMethod": "POST",
            "path": "/rules/approvals/create",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleApprovalController.java",
              "line": 38,
              "snippet": "@PostMapping(\"/create\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/rules/approvals/update/{ruleId}",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleApprovalController.java",
              "line": 51,
              "snippet": "@PostMapping(\"/update/{ruleId}\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/rules/approvals/delete/{ruleId}",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleApprovalController.java",
              "line": 65,
              "snippet": "@PostMapping(\"/delete/{ruleId}\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/rules/approvals/{id}/approve",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleApprovalController.java",
              "line": 76,
              "snippet": "@PostMapping(\"/{id}/approve\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/rules/approvals/{id}/reject",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleApprovalController.java",
              "line": 89,
              "snippet": "@PostMapping(\"/{id}/reject\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/rules/approvals/{id}/cancel",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleApprovalController.java",
              "line": 101,
              "snippet": "@PostMapping(\"/{id}/cancel\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/rules/approvals/pending",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleApprovalController.java",
              "line": 112,
              "snippet": "@GetMapping(\"/pending\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/rules/approvals/pending/page",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleApprovalController.java",
              "line": 121,
              "snippet": "@GetMapping(\"/pending/page\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/rules/approvals/pending/count",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleApprovalController.java",
              "line": 133,
              "snippet": "@GetMapping(\"/pending/count\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/rules/approvals/{id}",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleApprovalController.java",
              "line": 142,
              "snippet": "@GetMapping(\"/{id}\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/rules/approvals/history/{ruleId}",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleApprovalController.java",
              "line": 154,
              "snippet": "@GetMapping(\"/history/{ruleId}\")"
            }
          }
        ]
      },
      {
        "className": "RuleController",
        "basePath": "/rules",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/controller/RuleController.java",
          "line": 22
        },
        "endpoints": [
          {
            "httpMethod": "GET",
            "path": "/rules",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleController.java",
              "line": 34,
              "snippet": "@GetMapping"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/rules/{id}",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleController.java",
              "line": 51,
              "snippet": "@GetMapping(\"/{id}\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/rules",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleController.java",
              "line": 66,
              "snippet": "@PostMapping"
            }
          },
          {
            "httpMethod": "PUT",
            "path": "/rules/{id}",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleController.java",
              "line": 82,
              "snippet": "@PutMapping(\"/{id}\")"
            }
          },
          {
            "httpMethod": "DELETE",
            "path": "/rules/{id}",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleController.java",
              "line": 99,
              "snippet": "@DeleteMapping(\"/{id}\")"
            }
          },
          {
            "httpMethod": "PATCH",
            "path": "/rules/{id}/toggle",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleController.java",
              "line": 111,
              "snippet": "@PatchMapping(\"/{id}/toggle\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/rules/enabled/{enabled}",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleController.java",
              "line": 133,
              "snippet": "@GetMapping(\"/enabled/{enabled}\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/rules/{id}/history",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleController.java",
              "line": 148,
              "snippet": "@GetMapping(\"/{id}/history\")"
            }
          }
        ]
      },
      {
        "className": "RuleExportImportController",
        "basePath": "/v1/rules/export-import",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/controller/RuleExportImportController.java",
          "line": 19
        },
        "endpoints": [
          {
            "httpMethod": "GET",
            "path": "/v1/rules/export-import/export",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleExportImportController.java",
              "line": 30,
              "snippet": "@GetMapping(\"/export\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/v1/rules/export-import/export/selective",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleExportImportController.java",
              "line": 76,
              "snippet": "@PostMapping(\"/export/selective\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/v1/rules/export-import/export/complex",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleExportImportController.java",
              "line": 117,
              "snippet": "@GetMapping(\"/export/complex\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/v1/rules/export-import/import",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleExportImportController.java",
              "line": 159,
              "snippet": "@PostMapping(\"/import\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/v1/rules/export-import/import/file",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleExportImportController.java",
              "line": 215,
              "snippet": "@PostMapping(value = \"/import/file\", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/v1/rules/export-import/validate",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleExportImportController.java",
              "line": 272,
              "snippet": "@PostMapping(\"/validate\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/v1/rules/export-import/template/simple",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleExportImportController.java",
              "line": 309,
              "snippet": "@GetMapping(\"/template/simple\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/v1/rules/export-import/template/complex",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleExportImportController.java",
              "line": 349,
              "snippet": "@GetMapping(\"/template/complex\")"
            }
          }
        ]
      },
      {
        "className": "RuleMetricsController",
        "basePath": "/rules/metrics",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/controller/RuleMetricsController.java",
          "line": 19
        },
        "endpoints": [
          {
            "httpMethod": "GET",
            "path": "/rules/metrics/dashboard",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleMetricsController.java",
              "line": 31,
              "snippet": "@GetMapping(\"/dashboard\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/rules/metrics/{ruleId}",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleMetricsController.java",
              "line": 55,
              "snippet": "@GetMapping(\"/{ruleId}\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/rules/metrics/all",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleMetricsController.java",
              "line": 78,
              "snippet": "@GetMapping(\"/all\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/rules/metrics/{ruleId}/false-positive",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleMetricsController.java",
              "line": 102,
              "snippet": "@PostMapping(\"/{ruleId}/false-positive\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/rules/metrics/{ruleId}/true-positive",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleMetricsController.java",
              "line": 115,
              "snippet": "@PostMapping(\"/{ruleId}/true-positive\")"
            }
          }
        ]
      },
      {
        "className": "RuleSimulationController",
        "basePath": "/rules/simulation",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/controller/RuleSimulationController.java",
          "line": 24
        },
        "endpoints": [
          {
            "httpMethod": "POST",
            "path": "/rules/simulation/test",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleSimulationController.java",
              "line": 36,
              "snippet": "@PostMapping(\"/test\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/rules/simulation/backtest/{ruleId}",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleSimulationController.java",
              "line": 51,
              "snippet": "@PostMapping(\"/backtest/{ruleId}\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/rules/simulation/compare",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleSimulationController.java",
              "line": 67,
              "snippet": "@PostMapping(\"/compare\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/rules/simulation/batch",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/RuleSimulationController.java",
              "line": 86,
              "snippet": "@PostMapping(\"/batch\")"
            }
          }
        ]
      },
      {
        "className": "TransactionController",
        "basePath": "/transactions",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/controller/TransactionController.java",
          "line": 40
        },
        "endpoints": [
          {
            "httpMethod": "POST",
            "path": "/transactions/analyze",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/TransactionController.java",
              "line": 60,
              "snippet": "@PostMapping(\"/analyze\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/transactions",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/TransactionController.java",
              "line": 79,
              "snippet": "@GetMapping"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/transactions/export",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/TransactionController.java",
              "line": 145,
              "snippet": "@GetMapping("
            }
          },
          {
            "httpMethod": "GET",
            "path": "/transactions/export/csv",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/TransactionController.java",
              "line": 191,
              "snippet": "@GetMapping(value = \"/export/csv\", produces = \"text/csv\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/transactions/{id}",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/TransactionController.java",
              "line": 244,
              "snippet": "@GetMapping(\"/{id}\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/transactions/external/{externalId}",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/TransactionController.java",
              "line": 258,
              "snippet": "@GetMapping(\"/external/{externalId}\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/transactions/analyze-advanced",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/TransactionController.java",
              "line": 275,
              "snippet": "@PostMapping(\"/analyze-advanced\")"
            }
          }
        ]
      },
      {
        "className": "HomologRuleController",
        "basePath": "/homolog/rules",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/controller/homolog/HomologRuleController.java",
          "line": 12
        },
        "endpoints": [
          {
            "httpMethod": "POST",
            "path": "/homolog/rules",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/homolog/HomologRuleController.java",
              "line": 22,
              "snippet": "@PostMapping"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/homolog/rules/{ruleId}/latest",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/homolog/HomologRuleController.java",
              "line": 29,
              "snippet": "@GetMapping(\"/{ruleId}/latest\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/homolog/rules/versions/{ruleVersionId}/publish",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/homolog/HomologRuleController.java",
              "line": 38,
              "snippet": "@PostMapping(\"/versions/{ruleVersionId}/publish\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/homolog/rules/{ruleId}/rollback/{version}",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/homolog/HomologRuleController.java",
              "line": 45,
              "snippet": "@PostMapping(\"/{ruleId}/rollback/{version}\")"
            }
          }
        ]
      },
      {
        "className": "HomologRuleSetController",
        "basePath": "/homolog/rulesets",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/controller/homolog/HomologRuleSetController.java",
          "line": 9
        },
        "endpoints": [
          {
            "httpMethod": "POST",
            "path": "/homolog/rulesets",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/homolog/HomologRuleSetController.java",
              "line": 19,
              "snippet": "@PostMapping"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/homolog/rulesets/versions/{ruleSetVersionId}/publish",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/homolog/HomologRuleSetController.java",
              "line": 26,
              "snippet": "@PostMapping(\"/versions/{ruleSetVersionId}/publish\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/homolog/rulesets/activate",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/homolog/HomologRuleSetController.java",
              "line": 33,
              "snippet": "@PostMapping(\"/activate\")"
            }
          }
        ]
      },
      {
        "className": "HomologSimulationController",
        "basePath": "/homolog/simulations",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/controller/homolog/HomologSimulationController.java",
          "line": 10
        },
        "endpoints": [
          {
            "httpMethod": "POST",
            "path": "/homolog/simulations/run",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/homolog/HomologSimulationController.java",
              "line": 20,
              "snippet": "@PostMapping(\"/run\")"
            }
          }
        ]
      },
      {
        "className": "ComplexRuleController",
        "basePath": "/v1/complex-rules",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/controller/complex/ComplexRuleController.java",
          "line": 20
        },
        "endpoints": [
          {
            "httpMethod": "POST",
            "path": "/v1/complex-rules/{ruleVersionId}/conditions",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/complex/ComplexRuleController.java",
              "line": 32,
              "snippet": "@PostMapping(\"/{ruleVersionId}/conditions\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/v1/complex-rules/{ruleVersionId}/conditions",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/complex/ComplexRuleController.java",
              "line": 41,
              "snippet": "@GetMapping(\"/{ruleVersionId}/conditions\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/v1/complex-rules/{ruleVersionId}/expressions",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/complex/ComplexRuleController.java",
              "line": 53,
              "snippet": "@PostMapping(\"/{ruleVersionId}/expressions\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/v1/complex-rules/{ruleVersionId}/expressions",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/complex/ComplexRuleController.java",
              "line": 62,
              "snippet": "@GetMapping(\"/{ruleVersionId}/expressions\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/v1/complex-rules/{ruleVersionId}/variables",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/complex/ComplexRuleController.java",
              "line": 70,
              "snippet": "@PostMapping(\"/{ruleVersionId}/variables\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/v1/complex-rules/{ruleVersionId}/variables",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/complex/ComplexRuleController.java",
              "line": 80,
              "snippet": "@GetMapping(\"/{ruleVersionId}/variables\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/v1/complex-rules/{ruleVersionId}/actions",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/complex/ComplexRuleController.java",
              "line": 89,
              "snippet": "@PostMapping(\"/{ruleVersionId}/actions\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/v1/complex-rules/{ruleVersionId}/actions",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/complex/ComplexRuleController.java",
              "line": 98,
              "snippet": "@GetMapping(\"/{ruleVersionId}/actions\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/v1/complex-rules/{ruleVersionId}/fields",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/complex/ComplexRuleController.java",
              "line": 106,
              "snippet": "@GetMapping(\"/{ruleVersionId}/fields\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/v1/complex-rules/{ruleVersionId}/validate-depth",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/complex/ComplexRuleController.java",
              "line": 114,
              "snippet": "@GetMapping(\"/{ruleVersionId}/validate-depth\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/v1/complex-rules/templates",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/complex/ComplexRuleController.java",
              "line": 127,
              "snippet": "@GetMapping(\"/templates\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/v1/complex-rules/templates/system",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/complex/ComplexRuleController.java",
              "line": 133,
              "snippet": "@GetMapping(\"/templates/system\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/v1/complex-rules/templates/category/{category}",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/complex/ComplexRuleController.java",
              "line": 139,
              "snippet": "@GetMapping(\"/templates/category/{category}\")"
            }
          },
          {
            "httpMethod": "GET",
            "path": "/v1/complex-rules/templates/{name}",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/complex/ComplexRuleController.java",
              "line": 145,
              "snippet": "@GetMapping(\"/templates/{name}\")"
            }
          },
          {
            "httpMethod": "POST",
            "path": "/v1/complex-rules/{ruleVersionId}/evaluate",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/complex/ComplexRuleController.java",
              "line": 157,
              "snippet": "@PostMapping(\"/{ruleVersionId}/evaluate\")"
            }
          },
          {
            "httpMethod": "DELETE",
            "path": "/v1/complex-rules/{ruleVersionId}",
            "evidence": {
              "file": "backend/src/main/java/com/rulex/controller/complex/ComplexRuleController.java",
              "line": 190,
              "snippet": "@DeleteMapping(\"/{ruleVersionId}\")"
            }
          }
        ]
      }
    ],
    "services": [
      {
        "className": "FieldDictionaryService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/v31/field/FieldDictionaryService.java",
          "line": 6,
          "snippet": "@Service"
        }
      },
      {
        "className": "RuleExecutionLogService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/v31/execlog/RuleExecutionLogService.java",
          "line": 17,
          "snippet": "@Service"
        }
      },
      {
        "className": "ABTestingService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/ABTestingService.java",
          "line": 37,
          "snippet": "@Service"
        }
      },
      {
        "className": "AccessLogService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/AccessLogService.java",
          "line": 26,
          "snippet": "@Service"
        }
      },
      {
        "className": "AdvancedRuleEngineService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/AdvancedRuleEngineService.java",
          "line": 23,
          "snippet": "@Service"
        }
      },
      {
        "className": "AuditQueryService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/AuditQueryService.java",
          "line": 22,
          "snippet": "@Service"
        }
      },
      {
        "className": "AuditService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/AuditService.java",
          "line": 20,
          "snippet": "@Service"
        }
      },
      {
        "className": "BloomFilterService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/BloomFilterService.java",
          "line": 43,
          "snippet": "@Service"
        }
      },
      {
        "className": "DatabaseRuleExecutorService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/DatabaseRuleExecutorService.java",
          "line": 33,
          "snippet": "@Service"
        }
      },
      {
        "className": "DeviceFingerprintService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/DeviceFingerprintService.java",
          "line": 45,
          "snippet": "@Service"
        }
      },
      {
        "className": "EnrichmentService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/EnrichmentService.java",
          "line": 25,
          "snippet": "@Service"
        }
      },
      {
        "className": "FuzzyLogicService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/FuzzyLogicService.java",
          "line": 12,
          "snippet": "@Service"
        }
      },
      {
        "className": "GeoService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/GeoService.java",
          "line": 30,
          "snippet": "@Service"
        }
      },
      {
        "className": "ImpossibleTravelService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/ImpossibleTravelService.java",
          "line": 47,
          "snippet": "@Service"
        }
      },
      {
        "className": "MetricsService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/MetricsService.java",
          "line": 19,
          "snippet": "@Service"
        }
      },
      {
        "className": "Neo4jGraphService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/Neo4jGraphService.java",
          "line": 20,
          "snippet": "@Service"
        }
      },
      {
        "className": "OperatorDataService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/OperatorDataService.java",
          "line": 17,
          "snippet": "@Service"
        }
      },
      {
        "className": "ParallelRuleExecutionService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/ParallelRuleExecutionService.java",
          "line": 42,
          "snippet": "@Service"
        }
      },
      {
        "className": "RedisVelocityCacheService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/RedisVelocityCacheService.java",
          "line": 35,
          "snippet": "@Service"
        }
      },
      {
        "className": "RedisVelocityService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/RedisVelocityService.java",
          "line": 50,
          "snippet": "@Service"
        }
      },
      {
        "className": "RuleApprovalService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/RuleApprovalService.java",
          "line": 25,
          "snippet": "@Service"
        }
      },
      {
        "className": "RuleConfigurationService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/RuleConfigurationService.java",
          "line": 26,
          "snippet": "@Service"
        }
      },
      {
        "className": "RuleEngineService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/RuleEngineService.java",
          "line": 44,
          "snippet": "@Service"
        }
      },
      {
        "className": "RuleExportImportService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/RuleExportImportService.java",
          "line": 25,
          "snippet": "@Service"
        }
      },
      {
        "className": "RuleMetricsService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/RuleMetricsService.java",
          "line": 18,
          "snippet": "@Service"
        }
      },
      {
        "className": "RuleOrderingService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/RuleOrderingService.java",
          "line": 36,
          "snippet": "@Service"
        }
      },
      {
        "className": "RuleSimulationService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/RuleSimulationService.java",
          "line": 27,
          "snippet": "@Service"
        }
      },
      {
        "className": "RulexMetricsService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/RulexMetricsService.java",
          "line": 21,
          "snippet": "@Service"
        }
      },
      {
        "className": "SecurityContextService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/SecurityContextService.java",
          "line": 15,
          "snippet": "@Service"
        }
      },
      {
        "className": "ShadowModeService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/ShadowModeService.java",
          "line": 50,
          "snippet": "@Service"
        }
      },
      {
        "className": "StatisticalAnalysisService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/StatisticalAnalysisService.java",
          "line": 18,
          "snippet": "@Service"
        }
      },
      {
        "className": "StringSimilarityService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/StringSimilarityService.java",
          "line": 15,
          "snippet": "@Service"
        }
      },
      {
        "className": "TransactionQueryService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/TransactionQueryService.java",
          "line": 32,
          "snippet": "@Service"
        }
      },
      {
        "className": "TransactionRawStoreService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/TransactionRawStoreService.java",
          "line": 14,
          "snippet": "@Service"
        }
      },
      {
        "className": "VelocityService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/VelocityService.java",
          "line": 31,
          "snippet": "@Service"
        }
      },
      {
        "className": "VelocityServiceFacade",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/VelocityServiceFacade.java",
          "line": 26,
          "snippet": "@Service"
        }
      },
      {
        "className": "TransactionEnrichmentFacade",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/enrichment/TransactionEnrichmentFacade.java",
          "line": 38,
          "snippet": "@Service"
        }
      },
      {
        "className": "ComplexRuleCrudService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/complex/ComplexRuleCrudService.java",
          "line": 22,
          "snippet": "@Service"
        }
      },
      {
        "className": "ComplexRuleExecutionService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/complex/ComplexRuleExecutionService.java",
          "line": 19,
          "snippet": "@Service"
        }
      },
      {
        "className": "ComplexRuleService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/complex/ComplexRuleService.java",
          "line": 16,
          "snippet": "@Service"
        }
      },
      {
        "className": "RuleValidationService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/service/complex/RuleValidationService.java",
          "line": 17,
          "snippet": "@Service"
        }
      },
      {
        "className": "HomologRuleApplicationService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/homolog/application/HomologRuleApplicationService.java",
          "line": 11,
          "snippet": "@Service"
        }
      },
      {
        "className": "HomologRuleSetApplicationService",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/homolog/application/HomologRuleSetApplicationService.java",
          "line": 12,
          "snippet": "@Service"
        }
      }
    ],
    "entities": [
      {
        "className": "FieldDictionaryEntity",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/v31/field/FieldDictionaryEntity.java",
          "line": 14,
          "snippet": "@Entity"
        }
      },
      {
        "className": "RuleExecutionLogEntity",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/v31/execlog/RuleExecutionLogEntity.java",
          "line": 21,
          "snippet": "@Entity"
        }
      },
      {
        "className": "AccessLog",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/AccessLog.java",
          "line": 14,
          "snippet": "@Entity"
        }
      },
      {
        "className": "AuditLog",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/AuditLog.java",
          "line": 14,
          "snippet": "@Entity"
        }
      },
      {
        "className": "AuthenticationFailure",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/AuthenticationFailure.java",
          "line": 12,
          "snippet": "@Entity"
        }
      },
      {
        "className": "BinLookup",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/BinLookup.java",
          "line": 16,
          "snippet": "@Entity"
        }
      },
      {
        "className": "BloomFilterMetadata",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/BloomFilterMetadata.java",
          "line": 16,
          "snippet": "@Entity"
        }
      },
      {
        "className": "CustomerAccountInfo",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/CustomerAccountInfo.java",
          "line": 14,
          "snippet": "@Entity"
        }
      },
      {
        "className": "CustomerBeneficiaryHistory",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/CustomerBeneficiaryHistory.java",
          "line": 14,
          "snippet": "@Entity"
        }
      },
      {
        "className": "CustomerChargebackHistory",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/CustomerChargebackHistory.java",
          "line": 13,
          "snippet": "@Entity"
        }
      },
      {
        "className": "CustomerIncomingTransfer",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/CustomerIncomingTransfer.java",
          "line": 13,
          "snippet": "@Entity"
        }
      },
      {
        "className": "CustomerLastTransaction",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/CustomerLastTransaction.java",
          "line": 13,
          "snippet": "@Entity"
        }
      },
      {
        "className": "DeviceFingerprint",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/DeviceFingerprint.java",
          "line": 16,
          "snippet": "@Entity"
        }
      },
      {
        "className": "DevicePanAssociation",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/DevicePanAssociation.java",
          "line": 14,
          "snippet": "@Entity"
        }
      },
      {
        "className": "GeoPolygon",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/GeoPolygon.java",
          "line": 14,
          "snippet": "@Entity"
        }
      },
      {
        "className": "GeoReference",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/GeoReference.java",
          "line": 14,
          "snippet": "@Entity"
        }
      },
      {
        "className": "Holiday",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/Holiday.java",
          "line": 13,
          "snippet": "@Entity"
        }
      },
      {
        "className": "MccCategory",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/MccCategory.java",
          "line": 16,
          "snippet": "@Entity"
        }
      },
      {
        "className": "MerchantChargeback",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/MerchantChargeback.java",
          "line": 14,
          "snippet": "@Entity"
        }
      },
      {
        "className": "PanLocationHistory",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/PanLocationHistory.java",
          "line": 14,
          "snippet": "@Entity"
        }
      },
      {
        "className": "RefdataVersion",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/RefdataVersion.java",
          "line": 17,
          "snippet": "@Entity"
        }
      },
      {
        "className": "RuleAbTest",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/RuleAbTest.java",
          "line": 17,
          "snippet": "@Entity"
        }
      },
      {
        "className": "RuleAbTestAssignment",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/RuleAbTestAssignment.java",
          "line": 15,
          "snippet": "@Entity"
        }
      },
      {
        "className": "RuleApproval",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/RuleApproval.java",
          "line": 14,
          "snippet": "@Entity"
        }
      },
      {
        "className": "RuleConfiguration",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/RuleConfiguration.java",
          "line": 16,
          "snippet": "@Entity"
        }
      },
      {
        "className": "RuleConfigurationHistory",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/RuleConfigurationHistory.java",
          "line": 11,
          "snippet": "@Entity"
        }
      },
      {
        "className": "RuleList",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/RuleList.java",
          "line": 16,
          "snippet": "@Entity"
        }
      },
      {
        "className": "RuleListEntry",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/RuleListEntry.java",
          "line": 11,
          "snippet": "@Entity"
        }
      },
      {
        "className": "RuleMetrics",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/RuleMetrics.java",
          "line": 16,
          "snippet": "@Entity"
        }
      },
      {
        "className": "ShadowEvaluationLog",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/ShadowEvaluationLog.java",
          "line": 17,
          "snippet": "@Entity"
        }
      },
      {
        "className": "Transaction",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/Transaction.java",
          "line": 15,
          "snippet": "@Entity"
        }
      },
      {
        "className": "TransactionDecision",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/TransactionDecision.java",
          "line": 14,
          "snippet": "@Entity"
        }
      },
      {
        "className": "TransactionRawStore",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/TransactionRawStore.java",
          "line": 14,
          "snippet": "@Entity"
        }
      },
      {
        "className": "VelocityCounter",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/VelocityCounter.java",
          "line": 14,
          "snippet": "@Entity"
        }
      },
      {
        "className": "VelocityMetrics",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/VelocityMetrics.java",
          "line": 14,
          "snippet": "@Entity"
        }
      },
      {
        "className": "VelocityTransactionLog",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/VelocityTransactionLog.java",
          "line": 12,
          "snippet": "@Entity"
        }
      },
      {
        "className": "VoipPhoneRange",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/VoipPhoneRange.java",
          "line": 13,
          "snippet": "@Entity"
        }
      },
      {
        "className": "ActiveRuleSetEntity",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/homolog/ActiveRuleSetEntity.java",
          "line": 8,
          "snippet": "@Entity"
        }
      },
      {
        "className": "AuditEntryEntity",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/homolog/AuditEntryEntity.java",
          "line": 11,
          "snippet": "@Entity"
        }
      },
      {
        "className": "DecisionLogEntity",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/homolog/DecisionLogEntity.java",
          "line": 12,
          "snippet": "@Entity"
        }
      },
      {
        "className": "RoleEntity",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/homolog/RoleEntity.java",
          "line": 9,
          "snippet": "@Entity"
        }
      },
      {
        "className": "RuleEntity",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/homolog/RuleEntity.java",
          "line": 9,
          "snippet": "@Entity"
        }
      },
      {
        "className": "RuleSetEntity",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/homolog/RuleSetEntity.java",
          "line": 9,
          "snippet": "@Entity"
        }
      },
      {
        "className": "RuleSetVersionEntity",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/homolog/RuleSetVersionEntity.java",
          "line": 11,
          "snippet": "@Entity"
        }
      },
      {
        "className": "RuleSetVersionItemEntity",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/homolog/RuleSetVersionItemEntity.java",
          "line": 8,
          "snippet": "@Entity"
        }
      },
      {
        "className": "RuleVersionEntity",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/homolog/RuleVersionEntity.java",
          "line": 11,
          "snippet": "@Entity"
        }
      },
      {
        "className": "SimulationRunEntity",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/homolog/SimulationRunEntity.java",
          "line": 11,
          "snippet": "@Entity"
        }
      },
      {
        "className": "UserEntity",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/homolog/UserEntity.java",
          "line": 9,
          "snippet": "@Entity"
        }
      },
      {
        "className": "UserRoleEntity",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/homolog/UserRoleEntity.java",
          "line": 8,
          "snippet": "@Entity"
        }
      },
      {
        "className": "ComplexRule",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/complex/ComplexRule.java",
          "line": 15,
          "snippet": "@Entity"
        }
      },
      {
        "className": "RuleAction",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/complex/RuleAction.java",
          "line": 16,
          "snippet": "@Entity"
        }
      },
      {
        "className": "RuleCondition",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/complex/RuleCondition.java",
          "line": 16,
          "snippet": "@Entity"
        }
      },
      {
        "className": "RuleConditionGroup",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/complex/RuleConditionGroup.java",
          "line": 21,
          "snippet": "@Entity"
        }
      },
      {
        "className": "RuleContextVariable",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/complex/RuleContextVariable.java",
          "line": 16,
          "snippet": "@Entity"
        }
      },
      {
        "className": "RuleExecutionDetail",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/complex/RuleExecutionDetail.java",
          "line": 13,
          "snippet": "@Entity"
        }
      },
      {
        "className": "RuleExpression",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/complex/RuleExpression.java",
          "line": 16,
          "snippet": "@Entity"
        }
      },
      {
        "className": "RuleTemplate",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/entity/complex/RuleTemplate.java",
          "line": 17,
          "snippet": "@Entity"
        }
      }
    ],
    "repositories": [
      {
        "name": "FieldDictionaryRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/v31/field/FieldDictionaryRepository.java",
          "line": 9,
          "snippet": "public interface FieldDictionaryRepository extends JpaRepository<FieldDictionaryEntity, UUID> {"
        }
      },
      {
        "name": "RuleExecutionLogRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/v31/execlog/RuleExecutionLogRepository.java",
          "line": 7,
          "snippet": "public interface RuleExecutionLogRepository extends JpaRepository<RuleExecutionLogEntity, UUID> {"
        }
      },
      {
        "name": "AccessLogRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/AccessLogRepository.java",
          "line": 16,
          "snippet": "public interface AccessLogRepository extends JpaRepository<AccessLog, Long> {"
        }
      },
      {
        "name": "AuditLogRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/AuditLogRepository.java",
          "line": 15,
          "snippet": "public interface AuditLogRepository extends JpaRepository<AuditLog, Long> {"
        }
      },
      {
        "name": "Repository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/AuthenticationFailureRepository.java",
          "line": 13,
          "snippet": "extends JpaRepository<AuthenticationFailure, UUID> {"
        }
      },
      {
        "name": "BinLookupRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/BinLookupRepository.java",
          "line": 14,
          "snippet": "public interface BinLookupRepository extends JpaRepository<BinLookup, Long> {"
        }
      },
      {
        "name": "BloomFilterMetadataRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/BloomFilterMetadataRepository.java",
          "line": 18,
          "snippet": "public interface BloomFilterMetadataRepository extends JpaRepository<BloomFilterMetadata, Long> {"
        }
      },
      {
        "name": "CustomerAccountInfoRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/CustomerAccountInfoRepository.java",
          "line": 13,
          "snippet": "public interface CustomerAccountInfoRepository extends JpaRepository<CustomerAccountInfo, UUID> {"
        }
      },
      {
        "name": "Repository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/CustomerBeneficiaryHistoryRepository.java",
          "line": 14,
          "snippet": "extends JpaRepository<CustomerBeneficiaryHistory, UUID> {"
        }
      },
      {
        "name": "Repository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/CustomerChargebackHistoryRepository.java",
          "line": 13,
          "snippet": "extends JpaRepository<CustomerChargebackHistory, UUID> {"
        }
      },
      {
        "name": "Repository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/CustomerIncomingTransferRepository.java",
          "line": 14,
          "snippet": "extends JpaRepository<CustomerIncomingTransfer, UUID> {"
        }
      },
      {
        "name": "Repository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/CustomerLastTransactionRepository.java",
          "line": 14,
          "snippet": "extends JpaRepository<CustomerLastTransaction, UUID> {"
        }
      },
      {
        "name": "DeviceFingerprintRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/DeviceFingerprintRepository.java",
          "line": 14,
          "snippet": "public interface DeviceFingerprintRepository extends JpaRepository<DeviceFingerprint, Long> {"
        }
      },
      {
        "name": "DevicePanAssociationRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/DevicePanAssociationRepository.java",
          "line": 14,
          "snippet": "public interface DevicePanAssociationRepository extends JpaRepository<DevicePanAssociation, Long> {"
        }
      },
      {
        "name": "GeoPolygonRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/GeoPolygonRepository.java",
          "line": 13,
          "snippet": "public interface GeoPolygonRepository extends JpaRepository<GeoPolygon, Long> {"
        }
      },
      {
        "name": "GeoReferenceRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/GeoReferenceRepository.java",
          "line": 14,
          "snippet": "public interface GeoReferenceRepository extends JpaRepository<GeoReference, Long> {"
        }
      },
      {
        "name": "HolidayRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/HolidayRepository.java",
          "line": 13,
          "snippet": "public interface HolidayRepository extends JpaRepository<Holiday, UUID> {"
        }
      },
      {
        "name": "MccCategoryRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/MccCategoryRepository.java",
          "line": 14,
          "snippet": "public interface MccCategoryRepository extends JpaRepository<MccCategory, Long> {"
        }
      },
      {
        "name": "MerchantChargebackRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/MerchantChargebackRepository.java",
          "line": 13,
          "snippet": "public interface MerchantChargebackRepository extends JpaRepository<MerchantChargeback, UUID> {"
        }
      },
      {
        "name": "PanLocationHistoryRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/PanLocationHistoryRepository.java",
          "line": 17,
          "snippet": "public interface PanLocationHistoryRepository extends JpaRepository<PanLocationHistory, Long> {"
        }
      },
      {
        "name": "RefdataVersionRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/RefdataVersionRepository.java",
          "line": 16,
          "snippet": "public interface RefdataVersionRepository extends JpaRepository<RefdataVersion, UUID> {"
        }
      },
      {
        "name": "RuleAbTestAssignmentRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/RuleAbTestAssignmentRepository.java",
          "line": 18,
          "snippet": "public interface RuleAbTestAssignmentRepository extends JpaRepository<RuleAbTestAssignment, Long> {"
        }
      },
      {
        "name": "RuleAbTestRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/RuleAbTestRepository.java",
          "line": 15,
          "snippet": "public interface RuleAbTestRepository extends JpaRepository<RuleAbTest, Long> {"
        }
      },
      {
        "name": "RuleApprovalRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/RuleApprovalRepository.java",
          "line": 16,
          "snippet": "public interface RuleApprovalRepository extends JpaRepository<RuleApproval, Long> {"
        }
      },
      {
        "name": "Repository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/RuleConfigurationHistoryRepository.java",
          "line": 10,
          "snippet": "extends JpaRepository<RuleConfigurationHistory, Long> {"
        }
      },
      {
        "name": "RuleConfigurationRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/RuleConfigurationRepository.java",
          "line": 12,
          "snippet": "public interface RuleConfigurationRepository extends JpaRepository<RuleConfiguration, Long> {"
        }
      },
      {
        "name": "RuleListEntryRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/RuleListEntryRepository.java",
          "line": 17,
          "snippet": "public interface RuleListEntryRepository extends JpaRepository<RuleListEntry, Long> {"
        }
      },
      {
        "name": "RuleListRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/RuleListRepository.java",
          "line": 15,
          "snippet": "public interface RuleListRepository extends JpaRepository<RuleList, Long> {"
        }
      },
      {
        "name": "RuleMetricsRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/RuleMetricsRepository.java",
          "line": 14,
          "snippet": "public interface RuleMetricsRepository extends JpaRepository<RuleMetrics, Long> {"
        }
      },
      {
        "name": "ShadowEvaluationLogRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/ShadowEvaluationLogRepository.java",
          "line": 17,
          "snippet": "public interface ShadowEvaluationLogRepository extends JpaRepository<ShadowEvaluationLog, Long> {"
        }
      },
      {
        "name": "TransactionDecisionRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/TransactionDecisionRepository.java",
          "line": 17,
          "snippet": "public interface TransactionDecisionRepository extends JpaRepository<TransactionDecision, Long> {"
        }
      },
      {
        "name": "TransactionRawStoreRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/TransactionRawStoreRepository.java",
          "line": 8,
          "snippet": "public interface TransactionRawStoreRepository extends JpaRepository<TransactionRawStore, String> {}"
        }
      },
      {
        "name": "TransactionRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/TransactionRepository.java",
          "line": 17,
          "snippet": "public interface TransactionRepository extends JpaRepository<Transaction, Long> {"
        }
      },
      {
        "name": "VelocityCounterRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/VelocityCounterRepository.java",
          "line": 14,
          "snippet": "public interface VelocityCounterRepository extends JpaRepository<VelocityCounter, Long> {"
        }
      },
      {
        "name": "VelocityMetricsRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/VelocityMetricsRepository.java",
          "line": 14,
          "snippet": "public interface VelocityMetricsRepository extends JpaRepository<VelocityMetrics, Long> {"
        }
      },
      {
        "name": "Repository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/VelocityTransactionLogRepository.java",
          "line": 17,
          "snippet": "extends JpaRepository<VelocityTransactionLog, Long> {"
        }
      },
      {
        "name": "VoipPhoneRangeRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/VoipPhoneRangeRepository.java",
          "line": 12,
          "snippet": "public interface VoipPhoneRangeRepository extends JpaRepository<VoipPhoneRange, UUID> {"
        }
      },
      {
        "name": "ActiveRuleSetRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/homolog/ActiveRuleSetRepository.java",
          "line": 6,
          "snippet": "public interface ActiveRuleSetRepository extends JpaRepository<ActiveRuleSetEntity, Short> {}"
        }
      },
      {
        "name": "AuditEntryRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/homolog/AuditEntryRepository.java",
          "line": 7,
          "snippet": "public interface AuditEntryRepository extends JpaRepository<AuditEntryEntity, UUID> {}"
        }
      },
      {
        "name": "DecisionLogRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/homolog/DecisionLogRepository.java",
          "line": 7,
          "snippet": "public interface DecisionLogRepository extends JpaRepository<DecisionLogEntity, UUID> {}"
        }
      },
      {
        "name": "RoleRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/homolog/RoleRepository.java",
          "line": 8,
          "snippet": "public interface RoleRepository extends JpaRepository<RoleEntity, UUID> {"
        }
      },
      {
        "name": "RuleRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/homolog/RuleRepository.java",
          "line": 8,
          "snippet": "public interface RuleRepository extends JpaRepository<RuleEntity, UUID> {"
        }
      },
      {
        "name": "RuleSetRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/homolog/RuleSetRepository.java",
          "line": 8,
          "snippet": "public interface RuleSetRepository extends JpaRepository<RuleSetEntity, UUID> {"
        }
      },
      {
        "name": "Repository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/homolog/RuleSetVersionItemRepository.java",
          "line": 9,
          "snippet": "extends JpaRepository<RuleSetVersionItemEntity, RuleSetVersionItemEntity.Pk> {"
        }
      },
      {
        "name": "RuleSetVersionRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/homolog/RuleSetVersionRepository.java",
          "line": 11,
          "snippet": "public interface RuleSetVersionRepository extends JpaRepository<RuleSetVersionEntity, UUID> {"
        }
      },
      {
        "name": "RuleVersionRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/homolog/RuleVersionRepository.java",
          "line": 11,
          "snippet": "public interface RuleVersionRepository extends JpaRepository<RuleVersionEntity, UUID> {"
        }
      },
      {
        "name": "SimulationRunRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/homolog/SimulationRunRepository.java",
          "line": 7,
          "snippet": "public interface SimulationRunRepository extends JpaRepository<SimulationRunEntity, UUID> {}"
        }
      },
      {
        "name": "UserRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/homolog/UserRepository.java",
          "line": 8,
          "snippet": "public interface UserRepository extends JpaRepository<UserEntity, UUID> {"
        }
      },
      {
        "name": "UserRoleRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/homolog/UserRoleRepository.java",
          "line": 6,
          "snippet": "public interface UserRoleRepository extends JpaRepository<UserRoleEntity, UserRoleEntity.Pk> {}"
        }
      },
      {
        "name": "ComplexRuleRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/complex/ComplexRuleRepository.java",
          "line": 13,
          "snippet": "public interface ComplexRuleRepository extends JpaRepository<ComplexRule, UUID> {"
        }
      },
      {
        "name": "RuleActionRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/complex/RuleActionRepository.java",
          "line": 11,
          "snippet": "public interface RuleActionRepository extends JpaRepository<RuleAction, UUID> {"
        }
      },
      {
        "name": "RuleConditionGroupRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/complex/RuleConditionGroupRepository.java",
          "line": 14,
          "snippet": "public interface RuleConditionGroupRepository extends JpaRepository<RuleConditionGroup, UUID> {"
        }
      },
      {
        "name": "RuleConditionRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/complex/RuleConditionRepository.java",
          "line": 13,
          "snippet": "public interface RuleConditionRepository extends JpaRepository<RuleCondition, UUID> {"
        }
      },
      {
        "name": "RuleContextVariableRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/complex/RuleContextVariableRepository.java",
          "line": 12,
          "snippet": "public interface RuleContextVariableRepository extends JpaRepository<RuleContextVariable, UUID> {"
        }
      },
      {
        "name": "RuleExecutionDetailRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/complex/RuleExecutionDetailRepository.java",
          "line": 16,
          "snippet": "public interface RuleExecutionDetailRepository extends JpaRepository<RuleExecutionDetail, UUID> {"
        }
      },
      {
        "name": "RuleExpressionRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/complex/RuleExpressionRepository.java",
          "line": 12,
          "snippet": "public interface RuleExpressionRepository extends JpaRepository<RuleExpression, UUID> {"
        }
      },
      {
        "name": "RuleTemplateRepository",
        "evidence": {
          "file": "backend/src/main/java/com/rulex/repository/complex/RuleTemplateRepository.java",
          "line": 13,
          "snippet": "public interface RuleTemplateRepository extends JpaRepository<RuleTemplate, UUID> {"
        }
      }
    ],
    "flywayMigrations": [
      {
        "version": 1,
        "name": "init",
        "file": "backend/src/main/resources/db/migration/V1__init.sql"
      },
      {
        "version": 10,
        "name": "derived_context_improvements",
        "file": "backend/src/main/resources/db/migration/V10__derived_context_improvements.sql"
      },
      {
        "version": 11,
        "name": "bin_lookup_table",
        "file": "backend/src/main/resources/db/migration/V11__bin_lookup_table.sql"
      },
      {
        "version": 12,
        "name": "complex_rules_crud",
        "file": "backend/src/main/resources/db/migration/V12__complex_rules_crud.sql"
      },
      {
        "version": 13,
        "name": "geo_reference_table",
        "file": "backend/src/main/resources/db/migration/V13__geo_reference_table.sql"
      },
      {
        "version": 14,
        "name": "velocity_counters",
        "file": "backend/src/main/resources/db/migration/V14__velocity_counters.sql"
      },
      {
        "version": 15,
        "name": "add_velocity_operators",
        "file": "backend/src/main/resources/db/migration/V15__add_velocity_operators.sql"
      },
      {
        "version": 16,
        "name": "fix_geo_polygon_id_type",
        "file": "backend/src/main/resources/db/migration/V16__fix_geo_polygon_id_type.sql"
      },
      {
        "version": 17,
        "name": "fix_geo_reference_id_type",
        "file": "backend/src/main/resources/db/migration/V17__fix_geo_reference_id_type.sql"
      },
      {
        "version": 18,
        "name": "enable_condition_groups_constraint",
        "file": "backend/src/main/resources/db/migration/V18__enable_condition_groups_constraint.sql"
      },
      {
        "version": 19,
        "name": "access_log_table",
        "file": "backend/src/main/resources/db/migration/V19__access_log_table.sql"
      },
      {
        "version": 2,
        "name": "core_schema",
        "file": "backend/src/main/resources/db/migration/V2__core_schema.sql"
      },
      {
        "version": 20,
        "name": "shadow_mode_and_device_fingerprinting",
        "file": "backend/src/main/resources/db/migration/V20__shadow_mode_and_device_fingerprinting.sql"
      },
      {
        "version": 21,
        "name": "rule_configurations_shadow_mode",
        "file": "backend/src/main/resources/db/migration/V21__rule_configurations_shadow_mode.sql"
      },
      {
        "version": 22,
        "name": "fraud_detection_rules_seed",
        "file": "backend/src/main/resources/db/migration/V22__fraud_detection_rules_seed.sql"
      },
      {
        "version": 23,
        "name": "web_research_fraud_rules",
        "file": "backend/src/main/resources/db/migration/V23__web_research_fraud_rules.sql"
      },
      {
        "version": 24,
        "name": "regras_fraude_portugues_completo",
        "file": "backend/src/main/resources/db/migration/V24__regras_fraude_portugues_completo.sql"
      },
      {
        "version": 25,
        "name": "additional_fraud_rules_200plus",
        "file": "backend/src/main/resources/db/migration/V25__additional_fraud_rules_200plus.sql"
      },
      {
        "version": 26,
        "name": "fix_complex_rules_conditions",
        "file": "backend/src/main/resources/db/migration/V26__fix_complex_rules_conditions.sql"
      },
      {
        "version": 27,
        "name": "migrate_hardcoded_advanced_rules",
        "file": "backend/src/main/resources/db/migration/V27__migrate_hardcoded_advanced_rules.sql"
      },
      {
        "version": 28,
        "name": "add_missing_condition_operators",
        "file": "backend/src/main/resources/db/migration/V28__add_missing_condition_operators.sql"
      },
      {
        "version": 29,
        "name": "insert_advanced_fraud_rules_catalog",
        "file": "backend/src/main/resources/db/migration/V29__insert_advanced_fraud_rules_catalog.sql"
      },
      {
        "version": 3,
        "name": "extend_workflow_length",
        "file": "backend/src/main/resources/db/migration/V3__extend_workflow_length.sql"
      },
      {
        "version": 30,
        "name": "insert_aml_ato_advanced_rules",
        "file": "backend/src/main/resources/db/migration/V30__insert_aml_ato_advanced_rules.sql"
      },
      {
        "version": 31,
        "name": "add_velocity_extended_fields",
        "file": "backend/src/main/resources/db/migration/V31__add_velocity_extended_fields.sql"
      },
      {
        "version": 32,
        "name": "add_missing_tables_for_operators",
        "file": "backend/src/main/resources/db/migration/V32__add_missing_tables_for_operators.sql"
      },
      {
        "version": 33,
        "name": "fix_pos_entry_mode_length",
        "file": "backend/src/main/resources/db/migration/V33__fix_pos_entry_mode_length.sql"
      },
      {
        "version": 34,
        "name": "add_v31_plus_operators",
        "file": "backend/src/main/resources/db/migration/V34__add_v31_plus_operators.sql"
      },
      {
        "version": 35,
        "name": "add_velocity_temporal_indexes",
        "file": "backend/src/main/resources/db/migration/V35__add_velocity_temporal_indexes.sql"
      },
      {
        "version": 4,
        "name": "raw_hash_idempotency",
        "file": "backend/src/main/resources/db/migration/V4__raw_hash_idempotency.sql"
      },
      {
        "version": 5,
        "name": "raw_as_received",
        "file": "backend/src/main/resources/db/migration/V5__raw_as_received.sql"
      },
      {
        "version": 6,
        "name": "v31_exec_log_field_dictionary",
        "file": "backend/src/main/resources/db/migration/V6__v31_exec_log_field_dictionary.sql"
      },
      {
        "version": 7,
        "name": "v31_exec_log_dedup",
        "file": "backend/src/main/resources/db/migration/V7__v31_exec_log_dedup.sql"
      },
      {
        "version": 8,
        "name": "complex_rules_support",
        "file": "backend/src/main/resources/db/migration/V8__complex_rules_support.sql"
      },
      {
        "version": 9,
        "name": "audit_compliance_enhancements",
        "file": "backend/src/main/resources/db/migration/V9__audit_compliance_enhancements.sql"
      }
    ],
    "configFiles": [
      "backend/src/main/resources/application-dev.yml",
      "backend/src/main/resources/application-prod.yml",
      "backend/src/main/resources/application.yml",
      "backend/src/main/resources/prometheus-alerts.yml"
    ],
    "openapi": {
      "springdocDependency": {
        "file": "backend/pom.xml",
        "line": 118
      }
    }
  },
  "frontend": {
    "pages": [
      {
        "file": "client/src/pages/Audit.test.tsx"
      },
      {
        "file": "client/src/pages/Audit.tsx"
      },
      {
        "file": "client/src/pages/ComplexRules.tsx"
      },
      {
        "file": "client/src/pages/ComponentShowcase.tsx"
      },
      {
        "file": "client/src/pages/Dashboard.test.tsx"
      },
      {
        "file": "client/src/pages/Dashboard.tsx"
      },
      {
        "file": "client/src/pages/DashboardProfessional.tsx"
      },
      {
        "file": "client/src/pages/Diagrams.tsx"
      },
      {
        "file": "client/src/pages/DiagramsHub.tsx"
      },
      {
        "file": "client/src/pages/Home.test.tsx"
      },
      {
        "file": "client/src/pages/Home.tsx"
      },
      {
        "file": "client/src/pages/Login.test.tsx"
      },
      {
        "file": "client/src/pages/Login.tsx"
      },
      {
        "file": "client/src/pages/Manual.test.tsx"
      },
      {
        "file": "client/src/pages/Manual.tsx"
      },
      {
        "file": "client/src/pages/Monitoring.tsx"
      },
      {
        "file": "client/src/pages/NotFound.test.tsx"
      },
      {
        "file": "client/src/pages/NotFound.tsx"
      },
      {
        "file": "client/src/pages/Rules.test.tsx"
      },
      {
        "file": "client/src/pages/Rules.tsx"
      },
      {
        "file": "client/src/pages/RulesAdvanced.tsx"
      },
      {
        "file": "client/src/pages/RulesDidactic.tsx"
      },
      {
        "file": "client/src/pages/Settings.tsx"
      },
      {
        "file": "client/src/pages/Transactions.test.tsx"
      },
      {
        "file": "client/src/pages/Transactions.tsx"
      },
      {
        "file": "client/src/pages/TransactionSimulator.tsx"
      },
      {
        "file": "client/src/pages/TransactionsProfessional.tsx"
      }
    ]
  },
  "infra": {
    "dockerCompose": {
      "file": "docker-compose.yml",
      "services": [
        {
          "name": "postgres",
          "evidence": {
            "file": "docker-compose.yml",
            "line": 2,
            "snippet": "postgres:"
          }
        },
        {
          "name": "redis",
          "evidence": {
            "file": "docker-compose.yml",
            "line": 18,
            "snippet": "redis:"
          }
        },
        {
          "name": "neo4j",
          "evidence": {
            "file": "docker-compose.yml",
            "line": 30,
            "snippet": "neo4j:"
          }
        },
        {
          "name": "backend",
          "evidence": {
            "file": "docker-compose.yml",
            "line": 50,
            "snippet": "backend:"
          }
        },
        {
          "name": "web",
          "evidence": {
            "file": "docker-compose.yml",
            "line": 85,
            "snippet": "web:"
          }
        }
      ]
    },
    "dockerfiles": [
      {
        "file": "Dockerfile.web"
      },
      {
        "file": "backend/Dockerfile"
      }
    ]
  },
  "openapi": {
    "specs": [
      {
        "file": "openapi/rulex.yaml"
      }
    ]
  }
} as const;
export default RULEX_INVENTORY;
