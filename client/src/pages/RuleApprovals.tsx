import { useMemo, useState } from "react";
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";
import { Textarea } from "@/components/ui/textarea";
import {
  AlertDialog,
  AlertDialogAction,
  AlertDialogCancel,
  AlertDialogContent,
  AlertDialogDescription,
  AlertDialogFooter,
  AlertDialogHeader,
  AlertDialogTitle,
} from "@/components/ui/alert-dialog";
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog";
import { Loader2, CheckCircle2, XCircle, Ban, ClipboardList } from "lucide-react";
import { toast } from "sonner";
import {
  approveRuleApproval,
  cancelRuleApproval,
  listPendingRuleApprovals,
  rejectRuleApproval,
  RuleApproval,
} from "@/lib/javaApi";

const statusBadgeMap: Record<RuleApproval["status"], string> = {
  PENDING: "bg-amber-100 text-amber-800",
  APPROVED: "bg-green-100 text-green-800",
  REJECTED: "bg-red-100 text-red-800",
  CANCELLED: "bg-slate-200 text-slate-700",
};

const actionLabelMap: Record<RuleApproval["actionType"], string> = {
  CREATE: "Criar",
  UPDATE: "Atualizar",
  DELETE: "Excluir",
  TOGGLE: "Ativar/Desativar",
};

export default function RuleApprovals() {
  const queryClient = useQueryClient();
  const [selectedApproval, setSelectedApproval] = useState<RuleApproval | null>(null);
  const [comments, setComments] = useState("");
  const [rejectReason, setRejectReason] = useState("");
  const [rejectingApproval, setRejectingApproval] = useState<RuleApproval | null>(null);
  const [approvingApproval, setApprovingApproval] = useState<RuleApproval | null>(null);
  const [cancellingApproval, setCancellingApproval] = useState<RuleApproval | null>(null);

  const pendingQuery = useQuery({
    queryKey: ["rule-approvals", "pending"],
    queryFn: listPendingRuleApprovals,
    retry: 1,
  });

  const approvals = useMemo(() => pendingQuery.data ?? [], [pendingQuery.data]);

  const approveMutation = useMutation({
    mutationFn: ({ id, comments }: { id: number; comments?: string }) =>
      approveRuleApproval(id, comments),
    onSuccess: () => {
      toast.success("Solicitação aprovada");
      setApprovingApproval(null);
      setComments("");
      queryClient.invalidateQueries({ queryKey: ["rule-approvals", "pending"] });
    },
    onError: (error: Error) => toast.error(`Erro ao aprovar: ${error.message}`),
  });

  const rejectMutation = useMutation({
    mutationFn: ({ id, reason }: { id: number; reason: string }) =>
      rejectRuleApproval(id, reason),
    onSuccess: () => {
      toast.success("Solicitação rejeitada");
      setRejectingApproval(null);
      setRejectReason("");
      queryClient.invalidateQueries({ queryKey: ["rule-approvals", "pending"] });
    },
    onError: (error: Error) => toast.error(`Erro ao rejeitar: ${error.message}`),
  });

  const cancelMutation = useMutation({
    mutationFn: (id: number) => cancelRuleApproval(id),
    onSuccess: () => {
      toast.success("Solicitação cancelada");
      setCancellingApproval(null);
      queryClient.invalidateQueries({ queryKey: ["rule-approvals", "pending"] });
    },
    onError: (error: Error) => toast.error(`Erro ao cancelar: ${error.message}`),
  });

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-3xl font-bold text-foreground">Aprovações de Regras</h1>
          <p className="text-muted-foreground mt-1">
            Workflow de publicação com segregação de funções (4 olhos)
          </p>
        </div>
        <Badge variant="outline" className="bg-amber-50 text-amber-700 border-amber-200">
          {approvals.length} pendente(s)
        </Badge>
      </div>

      <Card>
        <CardHeader>
          <CardTitle>Solicitações pendentes</CardTitle>
          <CardDescription>Revisar, aprovar ou rejeitar alterações em regras.</CardDescription>
        </CardHeader>
        <CardContent>
          {pendingQuery.isLoading ? (
            <div className="flex items-center justify-center h-48">
              <Loader2 className="h-6 w-6 animate-spin" />
            </div>
          ) : approvals.length === 0 ? (
            <div className="flex flex-col items-center justify-center h-48 text-muted-foreground gap-2">
              <ClipboardList className="h-6 w-6" />
              Nenhuma solicitação pendente
            </div>
          ) : (
            <div className="overflow-x-auto">
              <table className="w-full">
                <thead className="border-b border-border">
                  <tr>
                    <th className="text-left py-3 px-4 text-sm font-semibold text-foreground">Regra</th>
                    <th className="text-left py-3 px-4 text-sm font-semibold text-foreground">Ação</th>
                    <th className="text-left py-3 px-4 text-sm font-semibold text-foreground">Solicitante</th>
                    <th className="text-left py-3 px-4 text-sm font-semibold text-foreground">Data</th>
                    <th className="text-left py-3 px-4 text-sm font-semibold text-foreground">Status</th>
                    <th className="text-center py-3 px-4 text-sm font-semibold text-foreground">Ações</th>
                  </tr>
                </thead>
                <tbody>
                  {approvals.map((approval) => (
                    <tr key={approval.id} className="border-b border-border">
                      <td className="py-3 px-4">
                        <div className="flex flex-col">
                          <span className="text-sm font-medium text-foreground">{approval.ruleName}</span>
                          <span className="text-xs text-muted-foreground">ID: {approval.ruleId}</span>
                        </div>
                      </td>
                      <td className="py-3 px-4 text-sm">
                        {actionLabelMap[approval.actionType]}
                      </td>
                      <td className="py-3 px-4 text-sm text-muted-foreground">
                        {approval.requestedBy}
                      </td>
                      <td className="py-3 px-4 text-sm text-muted-foreground">
                        {new Date(approval.requestedAt).toLocaleString()}
                      </td>
                      <td className="py-3 px-4 text-sm">
                        <Badge className={statusBadgeMap[approval.status]}>
                          {approval.status}
                        </Badge>
                      </td>
                      <td className="py-3 px-4 text-center space-x-2">
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => setSelectedApproval(approval)}
                          title="Ver detalhes"
                          aria-label="Ver detalhes"
                        >
                          Detalhes
                        </Button>
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => setApprovingApproval(approval)}
                          title="Aprovar"
                          aria-label="Aprovar"
                        >
                          <CheckCircle2 className="h-4 w-4" />
                        </Button>
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => setRejectingApproval(approval)}
                          title="Rejeitar"
                          aria-label="Rejeitar"
                        >
                          <XCircle className="h-4 w-4" />
                        </Button>
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => setCancellingApproval(approval)}
                          title="Cancelar"
                          aria-label="Cancelar"
                        >
                          <Ban className="h-4 w-4" />
                        </Button>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}
        </CardContent>
      </Card>

      <Dialog open={!!selectedApproval} onOpenChange={(open) => !open && setSelectedApproval(null)}>
        <DialogContent className="sm:max-w-2xl">
          <DialogHeader>
            <DialogTitle>Detalhes da solicitação</DialogTitle>
            <DialogDescription>Revisar payload da regra antes de aprovar.</DialogDescription>
          </DialogHeader>
          {selectedApproval ? (
            <div className="space-y-4">
              <div className="text-sm text-muted-foreground">
                Regra: <span className="text-foreground font-medium">{selectedApproval.ruleName}</span>
              </div>
              <Textarea
                readOnly
                rows={12}
                className="font-mono text-xs"
                value={selectedApproval.payloadJson || "(sem payload)"}
              />
            </div>
          ) : null}
        </DialogContent>
      </Dialog>

      <AlertDialog open={!!approvingApproval} onOpenChange={(open) => !open && setApprovingApproval(null)}>
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle>Aprovar solicitação</AlertDialogTitle>
            <AlertDialogDescription>
              Informe comentários opcionais antes de aprovar.
            </AlertDialogDescription>
          </AlertDialogHeader>
          <Textarea
            value={comments}
            onChange={(e) => setComments(e.target.value)}
            placeholder="Comentários (opcional)"
            rows={3}
          />
          <AlertDialogFooter>
            <AlertDialogCancel onClick={() => setApprovingApproval(null)}>Cancelar</AlertDialogCancel>
            <AlertDialogAction
              onClick={() => approvingApproval && approveMutation.mutate({ id: approvingApproval.id, comments })}
            >
              Aprovar
            </AlertDialogAction>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>

      <AlertDialog open={!!rejectingApproval} onOpenChange={(open) => !open && setRejectingApproval(null)}>
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle>Rejeitar solicitação</AlertDialogTitle>
            <AlertDialogDescription>
              Informe o motivo da rejeição.
            </AlertDialogDescription>
          </AlertDialogHeader>
          <Textarea
            value={rejectReason}
            onChange={(e) => setRejectReason(e.target.value)}
            placeholder="Motivo da rejeição"
            rows={3}
          />
          <AlertDialogFooter>
            <AlertDialogCancel onClick={() => setRejectingApproval(null)}>Cancelar</AlertDialogCancel>
            <AlertDialogAction
              onClick={() => rejectingApproval && rejectMutation.mutate({ id: rejectingApproval.id, reason: rejectReason })}
              className="bg-red-600 hover:bg-red-700"
            >
              Rejeitar
            </AlertDialogAction>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>

      <AlertDialog open={!!cancellingApproval} onOpenChange={(open) => !open && setCancellingApproval(null)}>
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle>Cancelar solicitação</AlertDialogTitle>
            <AlertDialogDescription>
              Confirme o cancelamento da solicitação pendente.
            </AlertDialogDescription>
          </AlertDialogHeader>
          <AlertDialogFooter>
            <AlertDialogCancel onClick={() => setCancellingApproval(null)}>Voltar</AlertDialogCancel>
            <AlertDialogAction
              onClick={() => cancellingApproval && cancelMutation.mutate(cancellingApproval.id)}
              className="bg-amber-600 hover:bg-amber-700"
            >
              Cancelar solicitação
            </AlertDialogAction>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>
    </div>
  );
}