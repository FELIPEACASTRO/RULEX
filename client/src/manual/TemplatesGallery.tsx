/**
 * TemplatesGallery.tsx - Galeria de templates de regras pré-definidas
 *
 * Features:
 * - Cards clicáveis com preview
 * - Explicação didática de cada template
 * - Estrutura de condições visualizada
 */
import { useState } from "react";

import { Badge } from "@/components/ui/badge";
import { Button } from "@/components/ui/button";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
  DialogTrigger,
} from "@/components/ui/dialog";

import { MANUAL_TEMPLATES, type ManualTemplate } from "./manualData";

// Cores por categoria de template
const CATEGORY_COLORS: Record<string, string> = {
  Valor: "bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-200",
  Geolocalização: "bg-blue-100 text-blue-800 dark:bg-blue-900 dark:text-blue-200",
  Horário: "bg-purple-100 text-purple-800 dark:bg-purple-900 dark:text-purple-200",
  Categoria: "bg-orange-100 text-orange-800 dark:bg-orange-900 dark:text-orange-200",
  Autenticação: "bg-red-100 text-red-800 dark:bg-red-900 dark:text-red-200",
  Combinada: "bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-200",
  Avançada: "bg-gray-100 text-gray-800 dark:bg-gray-700 dark:text-gray-200",
};

function TemplateCard({
  template,
  highlightTemplateId,
}: {
  template: ManualTemplate;
  highlightTemplateId?: string;
}) {
  const [isOpen, setIsOpen] = useState(false);
  const isHighlighted = highlightTemplateId === template.id;

  return (
    <Dialog open={isOpen} onOpenChange={setIsOpen}>
      <DialogTrigger asChild>
        <Card
          id={`manual-template-${template.id}`}
          className={
            [
              "cursor-pointer hover:shadow-lg transition-shadow hover:border-primary",
              isHighlighted ? "ring-2 ring-primary ring-inset bg-primary/10" : "",
            ].join(" ")
          }
        >
          <CardHeader className="pb-3">
            <div className="flex items-center justify-between">
              <span className="text-3xl">{template.icon}</span>
              <Badge className={CATEGORY_COLORS[template.category] || "bg-gray-100"}>
                {template.category}
              </Badge>
            </div>
            <CardTitle className="text-lg">{template.name}</CardTitle>
            <CardDescription>{template.description}</CardDescription>
          </CardHeader>
          <CardContent>
            <div className="space-y-2">
              <p className="text-xs text-muted-foreground uppercase font-medium">
                Condições:
              </p>
              <div className="space-y-1">
                {template.conditions.map((condition, idx) => (
                  <code
                    key={idx}
                    className="block text-xs bg-muted px-2 py-1 rounded font-mono"
                  >
                    {condition}
                  </code>
                ))}
              </div>
            </div>
          </CardContent>
        </Card>
      </DialogTrigger>

      <DialogContent className="max-w-2xl">
        <DialogHeader>
          <DialogTitle className="flex items-center gap-3">
            <span className="text-3xl">{template.icon}</span>
            {template.name}
            <Badge className={CATEGORY_COLORS[template.category] || "bg-gray-100"}>
              {template.category}
            </Badge>
          </DialogTitle>
          <DialogDescription>{template.description}</DialogDescription>
        </DialogHeader>

        <div className="space-y-6 mt-4">
          {/* Condições */}
          <div className="space-y-2">
            <h4 className="font-semibold text-sm">Condições da Regra</h4>
            <div className="bg-muted rounded-lg p-4 space-y-2">
              {template.conditions.map((condition, idx) => (
                <code
                  key={idx}
                  className="block text-sm font-mono"
                >
                  {condition}
                </code>
              ))}
            </div>
          </div>

          {/* Explicação didática */}
          <div className="space-y-4">
            <h4 className="font-semibold text-sm flex items-center gap-2">
              💡 Entenda esta regra
            </h4>

            <div className="grid gap-4 md:grid-cols-2">
              <Card className="bg-blue-50 dark:bg-blue-950">
                <CardHeader className="pb-2">
                  <CardTitle className="text-sm">O que faz?</CardTitle>
                </CardHeader>
                <CardContent>
                  <p className="text-sm">{template.explanation.oQueFaz}</p>
                </CardContent>
              </Card>

              <Card className="bg-green-50 dark:bg-green-950">
                <CardHeader className="pb-2">
                  <CardTitle className="text-sm">Por que é importante?</CardTitle>
                </CardHeader>
                <CardContent>
                  <p className="text-sm">{template.explanation.porQueImportante}</p>
                </CardContent>
              </Card>

              <Card className="bg-yellow-50 dark:bg-yellow-950">
                <CardHeader className="pb-2">
                  <CardTitle className="text-sm">Exemplo Real</CardTitle>
                </CardHeader>
                <CardContent>
                  <p className="text-sm">{template.explanation.exemploReal}</p>
                </CardContent>
              </Card>

              <Card className="bg-purple-50 dark:bg-purple-950">
                <CardHeader className="pb-2">
                  <CardTitle className="text-sm">💭 Analogia</CardTitle>
                </CardHeader>
                <CardContent>
                  <p className="text-sm italic">{template.explanation.analogia}</p>
                </CardContent>
              </Card>
            </div>
          </div>

          {/* Ações */}
          <div className="flex justify-end gap-2">
            <Button variant="outline" onClick={() => setIsOpen(false)}>
              Fechar
            </Button>
          </div>
        </div>
      </DialogContent>
    </Dialog>
  );
}

export interface TemplatesGalleryProps {
  highlightTemplateId?: string;
}

export function TemplatesGallery({ highlightTemplateId }: TemplatesGalleryProps) {
  // Agrupar templates por categoria
  const categories = Array.from(new Set(MANUAL_TEMPLATES.map((t) => t.category)));

  return (
    <div className="space-y-6">
      {/* Header */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            🎯 Templates de Regras
            <Badge variant="secondary" className="text-lg">
              {MANUAL_TEMPLATES.length}
            </Badge>
          </CardTitle>
          <CardDescription>
            Modelos prontos para criar regras de fraude. Clique em um template para ver detalhes.
          </CardDescription>
        </CardHeader>
        <CardContent>
          <div className="flex flex-wrap gap-2">
            {categories.map((cat) => (
              <Badge
                key={cat}
                className={CATEGORY_COLORS[cat] || "bg-gray-100"}
              >
                {cat} ({MANUAL_TEMPLATES.filter((t) => t.category === cat).length})
              </Badge>
            ))}
          </div>
        </CardContent>
      </Card>

      {/* Grid de templates */}
      <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-3">
        {MANUAL_TEMPLATES.map((template) => (
          <TemplateCard
            key={template.id}
            template={template}
            highlightTemplateId={highlightTemplateId}
          />
        ))}
      </div>

      {/* Dica */}
      <Card className="bg-muted/50">
        <CardContent className="pt-4">
          <div className="flex items-start gap-3">
            <span className="text-2xl">💡</span>
            <div>
              <p className="font-medium">Dica: Use templates como ponto de partida</p>
              <p className="text-sm text-muted-foreground">
                Os templates são exemplos de regras comuns. Você pode usá-los como base e
                customizar as condições para atender às necessidades específicas do seu negócio.
                Na tela de Regras de Fraude, clique em "Nova Regra" e depois no ícone de template
                para aplicar um desses modelos.
              </p>
            </div>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}

export default TemplatesGallery;
