import React, { useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { rulesApi, Rule } from '@/lib/api';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Input } from '@/components/ui/input';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { 
  Shield, Search, AlertTriangle, CheckCircle, XCircle, Info, 
  HelpCircle, Lightbulb, CreditCard, Globe, Clock, DollarSign,
  Lock, Smartphone, ShoppingCart, AlertOctagon, Eye, BookOpen,
  Edit, Save, X, Plus, Trash2
} from 'lucide-react';
import { Dialog, DialogContent, DialogHeader, DialogTitle, DialogFooter, DialogDescription } from '@/components/ui/dialog';
import { Label } from '@/components/ui/label';
import { Textarea } from '@/components/ui/textarea';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Switch } from '@/components/ui/switch';


// ==================== EXPLICAÇÕES DIDÁTICAS PARA LEIGOS ====================
const EXPLICACOES_REGRAS: Record<string, {
  oQueFaz: string;
  porQueImportante: string;
  exemploReal: string;
  analogia: string;
  icone: string;
  categoria: string;
}> = {
  // === REGRAS DE VALOR ===
  'MICRO_TRANSACTION': {
    oQueFaz: 'Detecta compras com valores muito pequenos, menores que R$ 1,00',
    porQueImportante: 'Criminosos testam se o cartão roubado funciona fazendo compras de centavos antes de fazer compras grandes',
    exemploReal: 'Alguém faz uma compra de R$ 0,50 em um site desconhecido às 3h da manhã. Isso pode ser um teste para ver se o cartão está ativo.',
    analogia: 'É como um ladrão que tenta abrir uma porta devagar para ver se está trancada antes de entrar',
    icone: '💰',
    categoria: 'Valor da Compra'
  },
  'HIGH_AMOUNT_THRESHOLD': {
    oQueFaz: 'Alerta quando o valor da compra passa de R$ 5.000,00',
    porQueImportante: 'Compras de alto valor merecem atenção extra porque o prejuízo é maior se for fraude',
    exemploReal: 'Uma compra de R$ 6.000 em eletrônicos. Pode ser legítima, mas vale verificar se o dono do cartão realmente fez.',
    analogia: 'É como o banco ligar para confirmar quando você faz um saque grande no caixa eletrônico',
    icone: '💵',
    categoria: 'Valor da Compra'
  },
  'VERY_HIGH_AMOUNT': {
    oQueFaz: 'Bloqueia compras acima de R$ 10.000,00 automaticamente',
    porQueImportante: 'Valores muito altos têm grande chance de serem fraude ou erro',
    exemploReal: 'Tentativa de compra de R$ 15.000 em joias. Se o cartão foi roubado, o criminoso quer gastar o máximo possível rapidamente.',
    analogia: 'É como um alarme que dispara quando alguém tenta carregar muita coisa de uma vez',
    icone: '🚨',
    categoria: 'Valor da Compra'
  },
  'ROUND_AMOUNT_SUSPICIOUS': {
    oQueFaz: 'Suspeita de valores redondos acima de R$ 500 (ex: R$ 1.000,00 exatos)',
    porQueImportante: 'Compras reais raramente têm valores redondos perfeitos. Fraudadores costumam usar valores redondos.',
    exemploReal: 'Compra de exatamente R$ 2.000,00 em uma loja online. Compras reais geralmente têm centavos (R$ 1.999,90).',
    analogia: 'É como desconfiar quando alguém paga uma conta de restaurante com valor exato, sem centavos',
    icone: '🔢',
    categoria: 'Valor da Compra'
  },

  // === REGRAS DE HORÁRIO ===
  'LATE_NIGHT_TRANSACTION': {
    oQueFaz: 'Alerta para compras feitas entre meia-noite e 5h da manhã',
    porQueImportante: 'A maioria das pessoas não faz compras de madrugada. Criminosos preferem esse horário porque há menos vigilância.',
    exemploReal: 'Uma compra às 3h30 da manhã em um site de eletrônicos. Se você estava dormindo, provavelmente não foi você.',
    analogia: 'É como desconfiar de alguém entrando em uma loja às 3h da manhã - pode ser legítimo, mas merece atenção',
    icone: '🌙',
    categoria: 'Horário'
  },
  'WEEKEND_HIGH_VALUE': {
    oQueFaz: 'Alerta para compras de alto valor em fins de semana',
    porQueImportante: 'Fins de semana têm menos funcionários de segurança monitorando, e os bancos estão fechados para confirmar',
    exemploReal: 'Compra de R$ 4.000 em um sábado à noite. Criminosos sabem que é mais difícil bloquear o cartão no fim de semana.',
    analogia: 'É como um ladrão que prefere agir quando o dono da casa está viajando',
    icone: '📅',
    categoria: 'Horário'
  },

  // === REGRAS GEOGRÁFICAS ===
  'HIGH_RISK_COUNTRY': {
    oQueFaz: 'Bloqueia compras em países conhecidos por fraudes (Nigéria, Rússia, Coreia do Norte, Irã)',
    porQueImportante: 'Alguns países têm taxas muito altas de fraude com cartão de crédito',
    exemploReal: 'Seu cartão é usado para uma compra na Nigéria, mas você nunca saiu do Brasil. Isso é quase certamente fraude.',
    analogia: 'É como não aceitar cheques de certos lugares conhecidos por calotes',
    icone: '🌍',
    categoria: 'Localização'
  },
  'CROSS_BORDER_ECOMMERCE': {
    oQueFaz: 'Alerta quando você compra em sites de outros países sem estar presente',
    porQueImportante: 'Compras internacionais online são mais arriscadas porque é difícil verificar a identidade',
    exemploReal: 'Compra em um site chinês de eletrônicos. Pode ser legítima, mas merece verificação extra.',
    analogia: 'É como comprar algo de um vendedor que você nunca viu pessoalmente, em outro país',
    icone: '✈️',
    categoria: 'Localização'
  },
  'ACQUIRER_COUNTRY_MISMATCH': {
    oQueFaz: 'Detecta quando a máquina de cartão está em um país diferente da loja',
    porQueImportante: 'Isso pode indicar que a transação está sendo processada por uma empresa fraudulenta',
    exemploReal: 'Você compra em uma loja "brasileira", mas a cobrança vem de outro país. A loja pode ser falsa.',
    analogia: 'É como receber uma ligação de um número brasileiro, mas a pessoa está falando de outro país',
    icone: '🏦',
    categoria: 'Localização'
  },

  // === REGRAS DE TIPO DE LOJA (MCC) ===
  'HIGH_RISK_MCC_GAMBLING': {
    oQueFaz: 'Alerta para compras em sites de jogos de azar e apostas',
    porQueImportante: 'Sites de apostas são muito usados para lavar dinheiro de cartões roubados',
    exemploReal: 'Compra de R$ 500 em um site de apostas online. Criminosos usam esses sites para converter crédito roubado em dinheiro.',
    analogia: 'É como ficar de olho em quem troca muito dinheiro em fichas de cassino',
    icone: '🎰',
    categoria: 'Tipo de Loja'
  },
  'HIGH_RISK_MCC_CRYPTO': {
    oQueFaz: 'Alerta para compras de criptomoedas (Bitcoin, etc.)',
    porQueImportante: 'Criptomoedas são difíceis de rastrear, então criminosos as usam para "sumir" com o dinheiro roubado',
    exemploReal: 'Compra de R$ 3.000 em Bitcoin. Uma vez convertido em cripto, o dinheiro é quase impossível de recuperar.',
    analogia: 'É como trocar dinheiro por ouro e esconder - muito difícil de rastrear depois',
    icone: '₿',
    categoria: 'Tipo de Loja'
  },
  'HIGH_RISK_MCC_MONEY_TRANSFER': {
    oQueFaz: 'Alerta para transferências de dinheiro e serviços de remessa',
    porQueImportante: 'Serviços de transferência são usados para enviar dinheiro roubado para outros países rapidamente',
    exemploReal: 'Transferência de R$ 2.000 via Western Union para o exterior. O dinheiro some em minutos.',
    analogia: 'É como mandar uma carta com dinheiro para um endereço desconhecido - uma vez enviado, não volta',
    icone: '💸',
    categoria: 'Tipo de Loja'
  },

  // === REGRAS DE AUTENTICAÇÃO ===
  'LOW_AUTHENTICATION_SCORE': {
    oQueFaz: 'Bloqueia quando o sistema não consegue confirmar que é realmente você',
    porQueImportante: 'Um score baixo significa que há muitas dúvidas sobre quem está fazendo a compra',
    exemploReal: 'Alguém tenta comprar usando seu cartão, mas o comportamento é muito diferente do seu padrão normal.',
    analogia: 'É como quando o segurança do banco não reconhece sua assinatura e pede mais documentos',
    icone: '🔐',
    categoria: 'Verificação de Identidade'
  },
  'MEDIUM_LOW_AUTH_SCORE': {
    oQueFaz: 'Alerta quando a verificação de identidade tem algumas dúvidas',
    porQueImportante: 'Não é certeza de fraude, mas há sinais de que algo pode estar errado',
    exemploReal: 'Você faz uma compra de um celular novo, em um horário diferente do habitual. O sistema fica em dúvida.',
    analogia: 'É como quando o caixa olha duas vezes para sua foto no documento - não está certo, mas também não está errado',
    icone: '🤔',
    categoria: 'Verificação de Identidade'
  },
  'LOW_EXTERNAL_SCORE': {
    oQueFaz: 'Bloqueia quando empresas de segurança externas classificam a compra como arriscada',
    porQueImportante: 'Várias empresas especializadas analisam milhões de transações e identificam padrões de fraude',
    exemploReal: 'Uma empresa de segurança detectou que o mesmo padrão de compra foi usado em outras fraudes recentemente.',
    analogia: 'É como quando vários vizinhos avisam que viram alguém suspeito rondando sua casa',
    icone: '🛡️',
    categoria: 'Verificação de Identidade'
  },
  'CAVV_FAILED': {
    oQueFaz: 'Bloqueia quando a verificação de segurança 3D Secure falhou',
    porQueImportante: 'O 3D Secure é aquela tela que pede senha ou código SMS. Se falhou, alguém pode estar tentando burlar.',
    exemploReal: 'Alguém tentou fazer uma compra mas não conseguiu passar pela verificação do banco (código SMS ou senha).',
    analogia: 'É como alguém que não sabe a senha do seu celular tentando desbloqueá-lo',
    icone: '🚫',
    categoria: 'Verificação de Identidade'
  },
  'ECI_NO_AUTH': {
    oQueFaz: 'Alerta quando uma compra online não passou por nenhuma verificação de segurança',
    porQueImportante: 'Compras sem verificação são mais arriscadas porque qualquer pessoa com os dados do cartão pode fazer',
    exemploReal: 'Compra em um site que não pediu código SMS nem senha - apenas os números do cartão.',
    analogia: 'É como entrar em um prédio sem passar pela portaria - qualquer um pode entrar',
    icone: '⚠️',
    categoria: 'Verificação de Identidade'
  },
  'CRYPTOGRAM_INVALID': {
    oQueFaz: 'Bloqueia quando o código de segurança do chip do cartão é inválido',
    porQueImportante: 'Um criptograma inválido pode indicar que o cartão foi clonado',
    exemploReal: 'Alguém fez uma cópia do seu cartão, mas não conseguiu copiar o chip corretamente.',
    analogia: 'É como uma chave falsa que parece igual mas não abre a fechadura',
    icone: '🔓',
    categoria: 'Verificação de Identidade'
  },

  // === REGRAS DE CVV/PIN ===
  'CVV_MISMATCH': {
    oQueFaz: 'Bloqueia quando o código de segurança de 3 dígitos (atrás do cartão) está errado',
    porQueImportante: 'Se alguém tem o número do cartão mas não o CVV, provavelmente roubou os dados de algum lugar',
    exemploReal: 'Tentativa de compra com CVV errado. A pessoa tem o número do cartão mas não tem o cartão físico.',
    analogia: 'É como saber o endereço de uma casa mas não ter a chave da porta',
    icone: '❌',
    categoria: 'Código de Segurança'
  },
  'CVV_NOT_PROCESSED': {
    oQueFaz: 'Alerta quando o código de segurança não foi verificado pelo sistema',
    porQueImportante: 'Algumas lojas não verificam o CVV, o que facilita fraudes',
    exemploReal: 'Compra em um site que não pediu o código de segurança do cartão.',
    analogia: 'É como uma loja que não pede documento na hora de pagar com cheque',
    icone: '❓',
    categoria: 'Código de Segurança'
  },
  'CVV_ENTRY_LIMIT_EXCEEDED': {
    oQueFaz: 'Bloqueia quando alguém errou o código de segurança muitas vezes seguidas',
    porQueImportante: 'Isso indica que alguém está tentando adivinhar o código - típico de criminosos',
    exemploReal: 'Alguém tentou 5 códigos diferentes em sequência. Está claramente tentando descobrir o CVV correto.',
    analogia: 'É como alguém tentando várias senhas diferentes para entrar na sua conta',
    icone: '🔄',
    categoria: 'Código de Segurança'
  },
  'PIN_ENTRY_LIMIT_EXCEEDED': {
    oQueFaz: 'Bloqueia quando a senha do cartão foi digitada errada muitas vezes',
    porQueImportante: 'Se alguém está tentando adivinhar sua senha, provavelmente roubou seu cartão',
    exemploReal: 'Alguém tentou sacar dinheiro no caixa eletrônico mas errou a senha 3 vezes.',
    analogia: 'É como alguém tentando abrir seu cadeado testando várias combinações',
    icone: '🔢',
    categoria: 'Código de Segurança'
  },

  // === REGRAS DE TERMINAL/MÁQUINA ===
  'POS_SECURITY_LOW': {
    oQueFaz: 'Alerta quando a máquina de cartão tem segurança baixa',
    porQueImportante: 'Máquinas antigas ou mal configuradas são mais fáceis de hackear',
    exemploReal: 'Compra em uma maquininha muito antiga que não tem as proteções de segurança modernas.',
    analogia: 'É como usar um cadeado velho e enferrujado que qualquer um consegue abrir',
    icone: '📟',
    categoria: 'Máquina de Cartão'
  },
  'POS_OFF_PREMISES': {
    oQueFaz: 'Alerta quando a máquina de cartão está fora do local da loja',
    porQueImportante: 'Máquinas móveis podem ser usadas por golpistas que se passam por entregadores',
    exemploReal: 'Cobrança de uma "loja" mas a máquina está em outro endereço. Pode ser um golpe.',
    analogia: 'É como um vendedor que diz ser de uma loja famosa mas está vendendo na rua',
    icone: '📍',
    categoria: 'Máquina de Cartão'
  },
  'MANUAL_ENTRY_HIGH_VALUE': {
    oQueFaz: 'Alerta quando o número do cartão foi digitado manualmente em compra de alto valor',
    porQueImportante: 'Digitar o número manualmente (sem passar o cartão) é mais arriscado porque qualquer um com os números pode fazer',
    exemploReal: 'Compra de R$ 2.000 onde o vendedor digitou o número do cartão em vez de passar na máquina.',
    analogia: 'É como fazer um cheque sem mostrar documento - mais fácil de falsificar',
    icone: '⌨️',
    categoria: 'Máquina de Cartão'
  },
  'CARD_CAPTURED': {
    oQueFaz: 'Alerta quando o cartão foi "engolido" pela máquina',
    porQueImportante: 'Isso pode indicar que o cartão é roubado ou que há problema com a máquina',
    exemploReal: 'O caixa eletrônico reteve o cartão. Pode ser proteção contra fraude ou problema técnico.',
    analogia: 'É como quando a catraca do metrô trava seu bilhete - algo está errado',
    icone: '🎰',
    categoria: 'Máquina de Cartão'
  },
  'FALLBACK_TRANSACTION': {
    oQueFaz: 'Alerta quando o chip do cartão não funcionou e usaram a tarja magnética',
    porQueImportante: 'A tarja magnética é muito mais fácil de clonar que o chip',
    exemploReal: 'O chip do cartão "não leu" e o vendedor passou na tarja. Golpistas fazem isso de propósito.',
    analogia: 'É como usar uma fechadura antiga porque a nova "não está funcionando" - pode ser golpe',
    icone: '🔄',
    categoria: 'Máquina de Cartão'
  },

  // === REGRAS EMV (CHIP) ===
  'EMV_AIP_MISMATCH': {
    oQueFaz: 'Detecta quando os dados do chip do cartão não batem entre si',
    porQueImportante: 'Isso pode indicar que alguém tentou clonar o chip do cartão',
    exemploReal: 'O cartão tem informações conflitantes no chip - sinal de que foi adulterado.',
    analogia: 'É como um documento com foto de uma pessoa e nome de outra',
    icone: '🔧',
    categoria: 'Chip do Cartão'
  },
  'TVR_FAILED': {
    oQueFaz: 'Detecta quando a máquina encontrou problemas ao verificar o cartão',
    porQueImportante: 'A máquina faz várias verificações de segurança. Se alguma falhou, há risco.',
    exemploReal: 'A maquininha detectou algo estranho no cartão durante a verificação.',
    analogia: 'É como quando o detector de metais apita no aeroporto - precisa verificar melhor',
    icone: '🔍',
    categoria: 'Chip do Cartão'
  },

  // === REGRAS DE CARTÃO ===
  'EXPIRED_CARD': {
    oQueFaz: 'Bloqueia tentativas de uso de cartão vencido',
    porQueImportante: 'Cartões vencidos não deveriam funcionar. Se alguém tenta usar, pode ser fraude.',
    exemploReal: 'Tentativa de compra com cartão que venceu há 2 meses.',
    analogia: 'É como tentar usar um cupom de desconto que já expirou',
    icone: '📅',
    categoria: 'Validade do Cartão'
  },
  'CARD_EXPIRING_SOON': {
    oQueFaz: 'Alerta para compras com cartão que vai vencer em breve',
    porQueImportante: 'Criminosos às vezes usam cartões perto de vencer porque sabem que serão substituídos em breve',
    exemploReal: 'Compra com cartão que vence no próximo mês. Pode ser legítima, mas merece atenção.',
    analogia: 'É como usar um documento que está quase vencendo - ainda vale, mas fica de olho',
    icone: '⏰',
    categoria: 'Validade do Cartão'
  },

  // === REGRAS DE CONTEXTO ===
  'CNP_HIGH_VALUE': {
    oQueFaz: 'Alerta para compras de alto valor pela internet (sem cartão físico presente)',
    porQueImportante: 'Compras online de alto valor são mais arriscadas porque não dá para verificar se a pessoa tem o cartão',
    exemploReal: 'Compra de R$ 3.000 em um site de eletrônicos. A pessoa só digitou os números, não mostrou o cartão.',
    analogia: 'É como aceitar um cheque alto de alguém que você nunca viu pessoalmente',
    icone: '🛒',
    categoria: 'Tipo de Compra'
  },
  'RECURRING_FIRST_HIGH_VALUE': {
    oQueFaz: 'Alerta quando a primeira cobrança de uma assinatura é muito alta',
    porQueImportante: 'Golpistas criam "assinaturas" falsas para cobrar valores altos automaticamente',
    exemploReal: 'Primeira cobrança de uma "assinatura" de R$ 1.500. Você autorizou isso?',
    analogia: 'É como assinar um contrato de academia e a primeira mensalidade vir 10x mais cara',
    icone: '🔁',
    categoria: 'Tipo de Compra'
  },
  'ECOMMERCE_NO_3DS': {
    oQueFaz: 'Alerta para compras online que não pediram verificação de segurança',
    porQueImportante: 'Sites sérios pedem confirmação por SMS ou senha. Sites que não pedem são mais arriscados.',
    exemploReal: 'Compra em um site que não pediu código SMS nem senha do banco - só os números do cartão.',
    analogia: 'É como uma loja que não pede documento na hora de pagar - qualquer um pode se passar por você',
    icone: '🌐',
    categoria: 'Tipo de Compra'
  },

  // === REGRAS COMBINADAS (PADRÕES DE FRAUDE) ===
  'CARD_TESTING_PATTERN': {
    oQueFaz: 'Detecta o padrão clássico de "teste de cartão" usado por criminosos',
    porQueImportante: 'Criminosos testam cartões roubados com compras pequenas antes de fazer compras grandes',
    exemploReal: 'Várias compras pequenas (R$ 1, R$ 2, R$ 5) em sites diferentes em poucos minutos. Alguém está testando se o cartão funciona.',
    analogia: 'É como um ladrão que testa se a chave funciona antes de entrar na casa',
    icone: '🧪',
    categoria: 'Padrão de Fraude'
  },
  'ATO_PATTERN': {
    oQueFaz: 'Detecta sinais de que alguém invadiu sua conta (Account Takeover)',
    porQueImportante: 'Criminosos invadem contas e mudam senhas para fazer compras em seu nome',
    exemploReal: 'Várias tentativas de senha errada seguidas de uma compra grande. Alguém descobriu sua senha e está usando.',
    analogia: 'É como alguém que conseguiu a chave da sua casa e está levando suas coisas',
    icone: '👤',
    categoria: 'Padrão de Fraude'
  },
  'HIGH_RISK_COMBO': {
    oQueFaz: 'Detecta combinação de vários fatores de risco juntos',
    porQueImportante: 'Quando vários sinais de alerta aparecem juntos, a chance de fraude é muito maior',
    exemploReal: 'Compra de madrugada + país de risco + valor alto + sem verificação. Muitos sinais ruins juntos.',
    analogia: 'É como ver fumaça, sentir cheiro de queimado e ouvir o alarme - certamente há fogo',
    icone: '🎯',
    categoria: 'Padrão de Fraude'
  },
  'BRAZIL_PIX_PATTERN': {
    oQueFaz: 'Detecta padrão de golpe do Pix: transferência de madrugada para conta desconhecida',
    porQueImportante: 'Golpistas brasileiros usam esse padrão para roubar dinheiro via Pix',
    exemploReal: 'Transferência de R$ 5.000 às 2h da manhã para uma conta que você nunca usou antes.',
    analogia: 'É como acordar de madrugada e ver alguém transferindo dinheiro da sua conta',
    icone: '🇧🇷',
    categoria: 'Padrão de Fraude'
  },
  'BRAZIL_BOLETO_FRAUD': {
    oQueFaz: 'Detecta padrão de golpe do boleto falso',
    porQueImportante: 'Golpistas criam boletos falsos que parecem legítimos mas vão para contas de criminosos',
    exemploReal: 'Pagamento de "boleto" em horário suspeito para uma empresa de serviços financeiros desconhecida.',
    analogia: 'É como pagar uma conta de luz que parece real mas o dinheiro vai para outra pessoa',
    icone: '📄',
    categoria: 'Padrão de Fraude'
  },
};

// Tipo para explicação didática
interface ExplicacaoRegra {
  oQueFaz: string;
  porQueImportante: string;
  exemploReal: string;
  analogia: string;
  icone: string;
  categoria: string;
}

// Função para obter explicação de uma regra
const getExplicacao = (ruleCode: string): ExplicacaoRegra => {
  return EXPLICACOES_REGRAS[ruleCode] || {
    oQueFaz: 'Esta regra analisa um padrão específico de transação',
    porQueImportante: 'Ajuda a identificar possíveis fraudes',
    exemploReal: 'Transações que não seguem o padrão esperado',
    analogia: 'É como um detector de anomalias',
    icone: '🔍',
    categoria: 'Geral'
  };
};

// Função para traduzir operadores
const traduzirOperador = (op: string): string => {
  const traducoes: Record<string, string> = {
    '>': 'maior que',
    '<': 'menor que',
    '>=': 'maior ou igual a',
    '<=': 'menor ou igual a',
    '==': 'igual a',
    '!=': 'diferente de',
    'IN': 'está na lista',
    'NOT_IN': 'não está na lista',
    'CONTAINS': 'contém',
    'NOT_CONTAINS': 'não contém',
  };
  return traducoes[op] || op;
};

// Função para traduzir nome do campo
const traduzirCampo = (campo: string): string => {
  const traducoes: Record<string, string> = {
    'transactionAmount': 'valor da compra',
    'consumerAuthenticationScore': 'pontuação de verificação de identidade',
    'externalScore3': 'pontuação de risco externo',
    'merchantCountryCode': 'país da loja',
    'mcc': 'tipo de loja',
    'cvv2Response': 'código de segurança (CVV)',
    'cryptogramValid': 'chip do cartão válido',
    'customerPresent': 'cliente presente',
    'posEntryMode': 'como o cartão foi usado',
    'transactionTime': 'horário da compra',
    'transactionDate': 'data da compra',
    'cardExpirationDate': 'validade do cartão',
    'eciIndicator': 'tipo de verificação online',
    'cavvResult': 'resultado da verificação 3D Secure',
    'cvv2EntryLimitExceeded': 'tentativas de CVV excedidas',
    'pinEntryLimitExceeded': 'tentativas de senha excedidas',
    'terminalVerificationResults': 'verificação da máquina',
    'recurringTransaction': 'compra recorrente',
    'acquirerCountryCode': 'país do processador',
  };
  return traducoes[campo] || campo;
};

// Função para formatar valor
const formatarValor = (campo: string, valor: string): string => {
  if (campo === 'transactionAmount') {
    const centavos = parseInt(valor);
    if (!isNaN(centavos)) {
      return `R$ ${(centavos / 100).toLocaleString('pt-BR', { minimumFractionDigits: 2 })}`;
    }
  }
  if (campo === 'transactionTime') {
    if (valor.length === 6) {
      return `${valor.slice(0, 2)}:${valor.slice(2, 4)}`;
    }
  }
  if (valor === 'Y') return 'Sim';
  if (valor === 'N') return 'Não';
  if (valor === 'true') return 'Sim';
  if (valor === 'false') return 'Não';
  return valor;
};

// Campos válidos do payload de entrada
const CAMPOS_PAYLOAD = [
  { value: 'transactionAmount', label: 'Valor da Transação', tipo: 'number' },
  { value: 'transactionTime', label: 'Horário da Transação', tipo: 'string' },
  { value: 'transactionDate', label: 'Data da Transação', tipo: 'string' },
  { value: 'mcc', label: 'Código MCC do Comerciante', tipo: 'string' },
  { value: 'merchantCountryCode', label: 'País do Comerciante', tipo: 'string' },
  { value: 'merchantId', label: 'ID do Comerciante', tipo: 'string' },
  { value: 'merchantName', label: 'Nome do Comerciante', tipo: 'string' },
  { value: 'customerPresent', label: 'Cliente Presente (Y/N)', tipo: 'string' },
  { value: 'consumerAuthenticationScore', label: 'Score de Autenticação', tipo: 'number' },
  { value: 'externalScore3', label: 'Score Externo', tipo: 'number' },
  { value: 'cvv2Response', label: 'Resposta CVV2', tipo: 'string' },
  { value: 'cvv2EntryLimitExceeded', label: 'Limite CVV Excedido', tipo: 'boolean' },
  { value: 'pinEntryLimitExceeded', label: 'Limite PIN Excedido', tipo: 'boolean' },
  { value: 'cryptogramValid', label: 'Criptograma Válido', tipo: 'boolean' },
  { value: 'cavvResult', label: 'Resultado CAVV (3D Secure)', tipo: 'string' },
  { value: 'eciIndicator', label: 'Indicador ECI', tipo: 'number' },
  { value: 'posSecurity', label: 'Segurança do Terminal', tipo: 'number' },
  { value: 'posOffPremises', label: 'Terminal Fora da Loja', tipo: 'number' },
  { value: 'posEntryMode', label: 'Modo de Entrada', tipo: 'string' },
  { value: 'cardAipStatic', label: 'AIP Estático do Cartão', tipo: 'number' },
  { value: 'cardAipDynamic', label: 'AIP Dinâmico do Cartão', tipo: 'number' },
  { value: 'terminalVerificationResults', label: 'Verificação do Terminal', tipo: 'string' },
  { value: 'cardExpireDate', label: 'Data de Expiração', tipo: 'string' },
  { value: 'cardCaptured', label: 'Cartão Capturado', tipo: 'boolean' },
  { value: 'recurringTransaction', label: 'Transação Recorrente', tipo: 'boolean' },
  { value: 'acquirerCountryCode', label: 'País do Adquirente', tipo: 'string' },
];

const OPERADORES = [
  { value: '==', label: 'Igual a' },
  { value: '!=', label: 'Diferente de' },
  { value: '>', label: 'Maior que' },
  { value: '<', label: 'Menor que' },
  { value: '>=', label: 'Maior ou igual a' },
  { value: '<=', label: 'Menor ou igual a' },
  { value: 'IN', label: 'Está na lista' },
  { value: 'NOT_IN', label: 'Não está na lista' },
];

const CATEGORIAS = [
  'VALUE', 'TEMPORAL', 'GEOGRAPHIC', 'MCC', 'AUTHENTICATION',
  'CVV_PIN', 'TERMINAL', 'EMV', 'CARD', 'CONTEXT', 'COMBINED', 'BRAZIL_SPECIFIC'
];

const CLASSIFICACOES = ['APPROVED', 'SUSPICIOUS', 'FRAUD'];

interface Condicao {
  field: string;
  operator: string;
  value: string | number | boolean | string[];
}

interface RegraEditando {
  id?: number;
  name: string;
  description: string;
  category: string;
  classification: string;
  weight: number;
  conditions: Condicao[];
  logicOperator: string;
  isActive: boolean;
  source: string;
}

export default function RulesDidactic() {
  const [searchTerm, setSearchTerm] = useState('');
  const [filtroClassificacao, setFiltroClassificacao] = useState<string>('all');
  const [filtroCategoria, setFiltroCategoria] = useState<string>('all');
  const [regraExpandida, setRegraExpandida] = useState<number | null>(null);
  
  // Estados do modal de edição
  const [modalAberto, setModalAberto] = useState(false);
  const [regraEditando, setRegraEditando] = useState<RegraEditando | null>(null);
  const [salvando, setSalvando] = useState(false);

  const queryClient = useQueryClient();
  const { data: rules, isLoading, refetch } = useQuery({
    queryKey: ['rules'],
    queryFn: rulesApi.list,
  });
  const updateMutation = useMutation({
    mutationFn: (data: { id: number; data: any }) => rulesApi.update({ id: data.id, ...data.data }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['rules'] });
      setModalAberto(false);
      setRegraEditando(null);
    },
  });
  const createMutation = useMutation({
    mutationFn: (data: any) => rulesApi.create(data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['rules'] });
      setModalAberto(false);
      setRegraEditando(null);
    },
  });

  // Função para abrir modal de edição
  const abrirEdicao = (rule: typeof rules extends (infer T)[] | undefined ? T : never) => {
    const conditions = rule.conditions as unknown as Condicao[];
    setRegraEditando({
      id: rule.id,
      name: rule.name,
      description: rule.description || '',
      category: rule.category,
      classification: rule.classification,
      weight: rule.weight,
      conditions: conditions || [],
      logicOperator: rule.logicOperator ?? 'AND',
      isActive: rule.isActive ?? true,
      source: rule.source || '',
    });
    setModalAberto(true);
  };

  // Função para criar nova regra
  const criarNovaRegra = () => {
    setRegraEditando({
      name: '',
      description: '',
      category: 'VALUE',
      classification: 'SUSPICIOUS',
      weight: 50,
      conditions: [{ field: 'transactionAmount', operator: '>', value: '' }],
      logicOperator: 'AND',
      isActive: true,
      source: '',
    });
    setModalAberto(true);
  };

  // Função para salvar regra
  const salvarRegra = async () => {
    if (!regraEditando) return;
    setSalvando(true);
    try {
      if (regraEditando.id) {
        await updateMutation.mutateAsync({
          id: regraEditando.id,
          data: {
            name: regraEditando.name,
            description: regraEditando.description,
            category: regraEditando.category as any,
            classification: regraEditando.classification as any,
            weight: regraEditando.weight,
            conditions: regraEditando.conditions as any,
            logicOperator: regraEditando.logicOperator as any,
            isActive: regraEditando.isActive,
            source: regraEditando.source,
          },
        });
      } else {
        await createMutation.mutateAsync({
          name: regraEditando.name,
          description: regraEditando.description,
          category: regraEditando.category as any,
          classification: regraEditando.classification as any,
          weight: regraEditando.weight,
          conditions: regraEditando.conditions as any,
          logicOperator: regraEditando.logicOperator as any,
          isActive: regraEditando.isActive,
          source: regraEditando.source,
        });
      }
    } catch (error) {
      console.error('Erro ao salvar regra:', error);
    } finally {
      setSalvando(false);
    }
  };

  // Função para adicionar condição
  const adicionarCondicao = () => {
    if (!regraEditando) return;
    setRegraEditando({
      ...regraEditando,
      conditions: [...regraEditando.conditions, { field: 'transactionAmount', operator: '>', value: '' }],
    });
  };

  // Função para remover condição
  const removerCondicao = (index: number) => {
    if (!regraEditando) return;
    setRegraEditando({
      ...regraEditando,
      conditions: regraEditando.conditions.filter((_, i) => i !== index),
    });
  };

  // Função para atualizar condição
  const atualizarCondicao = (index: number, campo: keyof Condicao, valor: string | number | boolean | string[]) => {
    if (!regraEditando) return;
    const novasCondicoes = [...regraEditando.conditions];
    novasCondicoes[index] = { ...novasCondicoes[index], [campo]: valor };
    setRegraEditando({ ...regraEditando, conditions: novasCondicoes });
  };

  // Filtrar regras
  const regrasFiltradas = rules?.filter(rule => {
    const explicacao = getExplicacao(rule.name.toUpperCase().replace(/\s+/g, '_'));
    const matchSearch = searchTerm === '' || 
      rule.name.toLowerCase().includes(searchTerm.toLowerCase()) ||
      (rule.description || '').toLowerCase().includes(searchTerm.toLowerCase()) ||
      explicacao.oQueFaz.toLowerCase().includes(searchTerm.toLowerCase());
    
    const matchClassificacao = filtroClassificacao === 'all' || rule.classification === filtroClassificacao || (filtroClassificacao === 'SUSPECT' && rule.classification === 'SUSPICIOUS');
    const matchCategoria = filtroCategoria === 'all' || explicacao.categoria === filtroCategoria;
    
    return matchSearch && matchClassificacao && matchCategoria;
  }) || [];

  // Obter categorias únicas
  const categorias = Array.from(new Set(Object.values(EXPLICACOES_REGRAS).map(e => e.categoria)));

  // Renderizar badge de classificação
  const renderClassificacao = (classification: string) => {
    switch (classification) {
      case 'APPROVED':
        return (
          <Badge className="bg-green-100 text-green-800 border-green-300 text-sm px-3 py-1">
            <CheckCircle className="w-4 h-4 mr-1" />
            ✅ Aprovada
          </Badge>
        );
      case 'SUSPECT':
        return (
          <Badge className="bg-yellow-100 text-yellow-800 border-yellow-300 text-sm px-3 py-1">
            <AlertTriangle className="w-4 h-4 mr-1" />
            ⚠️ Suspeita
          </Badge>
        );
      case 'FRAUD':
        return (
          <Badge className="bg-red-100 text-red-800 border-red-300 text-sm px-3 py-1">
            <XCircle className="w-4 h-4 mr-1" />
            🚫 Fraude
          </Badge>
        );
      default:
        return <Badge>{classification}</Badge>;
    }
  };

  // Renderizar condições de forma didática
  const renderCondicoes = (conditions: unknown): React.ReactNode => {
    if (!conditions || typeof conditions !== 'object') return null;
    const condObj = conditions as { conditions?: Array<{field: string; operator: string; value: string}>; operator?: string };
    if (!condObj.conditions) return null;
    
    const conds = condObj.conditions;
    const operadorLogico = condObj.operator === 'AND' ? 'E' : 'OU';
    
    return (
      <div className="space-y-2">
        {conds.map((cond, index) => (
          <div key={index} className="flex items-start gap-2">
            {index > 0 && (
              <Badge variant="outline" className="bg-blue-50 text-blue-700 text-xs">
                {operadorLogico}
              </Badge>
            )}
            <div className="flex-1 bg-gray-50 rounded-lg p-3 border border-gray-200">
              <div className="flex items-center gap-2 flex-wrap">
                <span className="font-medium text-gray-700">
                  {traduzirCampo(cond.field)}
                </span>
                <Badge variant="secondary" className="text-xs">
                  {traduzirOperador(cond.operator)}
                </Badge>
                <span className="font-bold text-blue-600">
                  {formatarValor(cond.field, cond.value)}
                </span>
              </div>
            </div>
          </div>
        ))}
      </div>
    );
  };

  if (isLoading) {
    return (
      <div className="flex items-center justify-center min-h-screen">
        <div className="text-center">
          <Shield className="w-16 h-16 text-blue-500 animate-pulse mx-auto mb-4" />
          <p className="text-lg text-gray-600">Carregando regras de segurança...</p>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gradient-to-br from-slate-50 to-blue-50 p-6">
      {/* Header */}
      <div className="max-w-7xl mx-auto">
        <div className="flex items-center gap-4 mb-8">
          <div className="p-3 bg-blue-600 rounded-xl shadow-lg">
            <Shield className="w-8 h-8 text-white" />
          </div>
          <div>
            <h1 className="text-3xl font-bold text-gray-900">
              Regras de Proteção
            </h1>
            <p className="text-gray-600 mt-1">
              Entenda como protegemos suas transações contra fraudes
            </p>
          </div>
        </div>

        {/* Introdução Didática */}
        <Card className="mb-8 bg-gradient-to-r from-blue-50 to-indigo-50 border-blue-200">
          <CardContent className="p-6">
            <div className="flex items-start gap-4">
              <div className="p-3 bg-blue-100 rounded-full">
                <Lightbulb className="w-6 h-6 text-blue-600" />
              </div>
              <div>
                <h2 className="text-xl font-semibold text-gray-900 mb-2">
                  Como funciona a proteção?
                </h2>
                <p className="text-gray-700 leading-relaxed">
                  Cada vez que você faz uma compra com cartão, nosso sistema analisa <strong>dezenas de informações</strong> em 
                  milissegundos para verificar se a transação parece legítima. Usamos <strong>{rules?.length || 0} regras de segurança</strong> que 
                  identificam padrões suspeitos - como compras de madrugada, valores muito altos, ou tentativas de adivinhar 
                  o código de segurança do cartão.
                </p>
                <div className="mt-4 flex flex-wrap gap-4">
                  <div className="flex items-center gap-2 bg-green-100 px-3 py-2 rounded-lg">
                    <CheckCircle className="w-5 h-5 text-green-600" />
                    <span className="text-green-800 font-medium">Aprovada</span>
                    <span className="text-green-600 text-sm">= Tudo certo, pode prosseguir</span>
                  </div>
                  <div className="flex items-center gap-2 bg-yellow-100 px-3 py-2 rounded-lg">
                    <AlertTriangle className="w-5 h-5 text-yellow-600" />
                    <span className="text-yellow-800 font-medium">Suspeita</span>
                    <span className="text-yellow-600 text-sm">= Precisa de verificação extra</span>
                  </div>
                  <div className="flex items-center gap-2 bg-red-100 px-3 py-2 rounded-lg">
                    <XCircle className="w-5 h-5 text-red-600" />
                    <span className="text-red-800 font-medium">Fraude</span>
                    <span className="text-red-600 text-sm">= Bloqueada para sua proteção</span>
                  </div>
                </div>
              </div>
            </div>
          </CardContent>
        </Card>

        {/* Filtros */}
        <Card className="mb-6">
          <CardContent className="p-4">
            <div className="flex flex-col md:flex-row gap-4">
              <div className="flex-1 relative">
                <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-gray-400 w-5 h-5" />
                <Input
                  placeholder="Buscar regras... (ex: CVV, valor alto, madrugada)"
                  value={searchTerm}
                  onChange={(e) => setSearchTerm(e.target.value)}
                  className="pl-10"
                />
              </div>
              <select
                value={filtroClassificacao}
                onChange={(e) => setFiltroClassificacao(e.target.value)}
                className="px-4 py-2 border rounded-lg bg-white"
              >
                <option value="all">Todas as classificações</option>
                <option value="APPROVED">✅ Aprovadas</option>
                <option value="SUSPECT">⚠️ Suspeitas</option>
                <option value="FRAUD">🚫 Fraudes</option>
              </select>
              <select
                value={filtroCategoria}
                onChange={(e) => setFiltroCategoria(e.target.value)}
                className="px-4 py-2 border rounded-lg bg-white"
              >
                <option value="all">Todas as categorias</option>
                {categorias.map(cat => (
                  <option key={cat} value={cat}>{cat}</option>
                ))}
              </select>
            </div>
          </CardContent>
        </Card>

        {/* Estatísticas */}
        <div className="grid grid-cols-1 md:grid-cols-4 gap-4 mb-6">
          <Card className="bg-white">
            <CardContent className="p-4 text-center">
              <div className="text-3xl font-bold text-blue-600">{rules?.length || 0}</div>
              <div className="text-gray-600 text-sm">Regras Ativas</div>
            </CardContent>
          </Card>
          <Card className="bg-green-50 border-green-200">
            <CardContent className="p-4 text-center">
              <div className="text-3xl font-bold text-green-600">
                {rules?.filter(r => r.classification === 'APPROVED').length || 0}
              </div>
              <div className="text-green-700 text-sm">Regras de Aprovação</div>
            </CardContent>
          </Card>
          <Card className="bg-yellow-50 border-yellow-200">
            <CardContent className="p-4 text-center">
              <div className="text-3xl font-bold text-yellow-600">
                {rules?.filter(r => r.classification === 'SUSPICIOUS').length ?? 0}            </div>
              <div className="text-yellow-700 text-sm">Regras de Suspeita</div>
            </CardContent>
          </Card>
          <Card className="bg-red-50 border-red-200">
            <CardContent className="p-4 text-center">
              <div className="text-3xl font-bold text-red-600">
                {rules?.filter(r => r.classification === 'FRAUD').length || 0}
              </div>
              <div className="text-red-700 text-sm">Regras de Fraude</div>
            </CardContent>
          </Card>
        </div>

        {/* Lista de Regras */}
        <div className="space-y-4">
          {regrasFiltradas.map((rule) => {
            const explicacao = getExplicacao(rule.name.toUpperCase().replace(/\s+/g, '_'));
            const isExpanded = regraExpandida === rule.id;
            
            return (
              <Card 
                key={rule.id} 
                className={`transition-all duration-300 hover:shadow-lg cursor-pointer ${
                  isExpanded ? 'ring-2 ring-blue-400' : ''
                } ${
                  rule.classification === 'FRAUD' ? 'border-l-4 border-l-red-500' :
                  rule.classification === 'SUSPICIOUS' ? 'border-l-4 border-l-yellow-500' :
                  'border-l-4 border-l-green-500'
                }`}
                onClick={() => setRegraExpandida(isExpanded ? null : rule.id)}
              >
                <CardContent className="p-6">
                  {/* Cabeçalho da Regra */}
                  <div className="flex items-start justify-between mb-4">
                    <div className="flex items-start gap-4">
                      <div className="text-4xl">{explicacao.icone}</div>
                      <div>
                        <div className="flex items-center gap-3 mb-1">
                          <h3 className="text-xl font-bold text-gray-900">{rule.name}</h3>
                          {rule.isActive && (
                            <Badge className="bg-green-100 text-green-700 text-xs">✅ Ativa</Badge>
                          )}
                        </div>
                        <p className="text-gray-600">{rule.description}</p>
                        <div className="flex items-center gap-2 mt-2">
                          <Badge variant="outline" className="text-xs">{explicacao.categoria}</Badge>
                          {renderClassificacao(rule.classification)}
                        </div>
                      </div>
                    </div>
                    <div className="flex items-center gap-2">
                      <Button 
                        variant="outline" 
                        size="sm"
                        onClick={(e) => {
                          e.stopPropagation();
                          abrirEdicao(rule);
                        }}
                        className="text-blue-600 hover:text-blue-800 hover:bg-blue-50"
                      >
                        <Edit className="w-4 h-4 mr-1" />
                        Editar
                      </Button>
                      <Button variant="ghost" size="sm">
                        {isExpanded ? '▲ Menos detalhes' : '▼ Mais detalhes'}
                      </Button>
                    </div>
                  </div>

                  {/* Resumo sempre visível */}
                  <div className="bg-blue-50 rounded-lg p-4 mb-4">
                    <div className="flex items-start gap-2">
                      <Info className="w-5 h-5 text-blue-600 mt-0.5 flex-shrink-0" />
                      <div>
                        <span className="font-semibold text-blue-900">O que esta regra faz: </span>
                        <span className="text-blue-800">{String(explicacao.oQueFaz)}</span>
                      </div>
                    </div>
                  </div>

                  {/* Detalhes expandidos */}
                  {isExpanded ? (
                    <div className="space-y-4 mt-4 pt-4 border-t border-gray-200">
                      {/* Por que é importante */}
                      <div className="bg-amber-50 rounded-lg p-4">
                        <div className="flex items-start gap-2">
                          <AlertTriangle className="w-5 h-5 text-amber-600 mt-0.5 flex-shrink-0" />
                          <div>
                            <span className="font-semibold text-amber-900">Por que isso é importante? </span>
                            <span className="text-amber-800">{String(explicacao.porQueImportante)}</span>
                          </div>
                        </div>
                      </div>

                      {/* Exemplo Real */}
                      <div className="bg-green-50 rounded-lg p-4">
                        <div className="flex items-start gap-2">
                          <Lightbulb className="w-5 h-5 text-green-600 mt-0.5 flex-shrink-0" />
                          <div>
                            <span className="font-semibold text-green-900">Exemplo prático: </span>
                            <span className="text-green-800">{String(explicacao.exemploReal)}</span>
                          </div>
                        </div>
                      </div>

                      {/* Analogia */}
                      <div className="bg-purple-50 rounded-lg p-4">
                        <div className="flex items-start gap-2">
                          <BookOpen className="w-5 h-5 text-purple-600 mt-0.5 flex-shrink-0" />
                          <div>
                            <span className="font-semibold text-purple-900">Para entender melhor: </span>
                            <span className="text-purple-800">{String(explicacao.analogia)}</span>
                          </div>
                        </div>
                      </div>

                      {/* Condições Técnicas */}
                      {rule.conditions ? (
                        <div className="bg-gray-50 rounded-lg p-4">
                          <div className="flex items-center gap-2 mb-3">
                            <Eye className="w-5 h-5 text-gray-600" />
                            <span className="font-semibold text-gray-900">Detalhes técnicos (como o sistema verifica):</span>
                          </div>
                          {renderCondicoes(rule.conditions)}
                        </div>
                      ) : null}

                      {/* Fonte */}
                      {rule.source && (
                        <div className="text-sm text-gray-500 flex items-center gap-2">
                          <BookOpen className="w-4 h-4" />
                          <span>Fonte: {rule.source}</span>
                        </div>
                      )}
                    </div>
                  ) : null}
                </CardContent>
              </Card>
            );
          })}
        </div>

        {/* Mensagem se não houver resultados */}
        {regrasFiltradas.length === 0 && (
          <Card className="mt-8">
            <CardContent className="p-12 text-center">
              <Search className="w-16 h-16 text-gray-300 mx-auto mb-4" />
              <h3 className="text-xl font-semibold text-gray-700 mb-2">
                Nenhuma regra encontrada
              </h3>
              <p className="text-gray-500">
                Tente buscar por outros termos ou remova os filtros
              </p>
            </CardContent>
          </Card>
        )}

        {/* Rodapé Informativo */}
        <Card className="mt-8 bg-gradient-to-r from-gray-50 to-slate-50">
          <CardContent className="p-6">
            <div className="flex items-start gap-4">
              <HelpCircle className="w-8 h-8 text-gray-400 flex-shrink-0" />
              <div>
                <h3 className="font-semibold text-gray-900 mb-2">Dúvidas frequentes</h3>
                <div className="space-y-3 text-gray-600">
                  <p>
                    <strong>Por que minha compra foi bloqueada?</strong> Se sua compra foi classificada como "Suspeita" ou "Fraude", 
                    pode ser que algum padrão incomum foi detectado. Isso não significa que você fez algo errado - é uma proteção 
                    para evitar que criminosos usem seu cartão.
                  </p>
                  <p>
                    <strong>Como liberar uma compra bloqueada?</strong> Entre em contato com seu banco ou operadora do cartão. 
                    Eles podem verificar sua identidade e liberar a transação se confirmarem que é você.
                  </p>
                  <p>
                    <strong>Essas regras podem errar?</strong> Sim, às vezes uma compra legítima pode ser bloqueada (chamamos de 
                    "falso positivo"). Por isso temos a categoria "Suspeita" que pede verificação extra em vez de bloquear direto.
                  </p>
                </div>
              </div>
            </div>
          </CardContent>
        </Card>

        {/* Botão para criar nova regra */}
        <div className="mt-8 flex justify-center">
          <Button 
            onClick={criarNovaRegra}
            className="bg-blue-600 hover:bg-blue-700 text-white"
          >
            <Plus className="w-5 h-5 mr-2" />
            Criar Nova Regra
          </Button>
        </div>
      </div>

      {/* Modal de Edição de Regra */}
      <Dialog open={modalAberto} onOpenChange={setModalAberto}>
        <DialogContent className="max-w-4xl max-h-[90vh] overflow-y-auto">
          <DialogHeader>
            <DialogTitle className="text-xl font-bold">
              {regraEditando?.id ? '✏️ Editar Regra' : '➕ Criar Nova Regra'}
            </DialogTitle>
            <DialogDescription>
              {regraEditando?.id 
                ? 'Modifique os campos abaixo para atualizar a regra. Apenas campos do payload são permitidos nas condições.'
                : 'Preencha os campos abaixo para criar uma nova regra de detecção de fraude.'}
            </DialogDescription>
          </DialogHeader>

          {regraEditando && (
            <div className="space-y-6 py-4">
              {/* Nome e Descrição */}
              <div className="grid grid-cols-1 gap-4">
                <div>
                  <Label htmlFor="name" className="text-sm font-medium">Nome da Regra *</Label>
                  <Input
                    id="name"
                    value={regraEditando.name}
                    onChange={(e) => setRegraEditando({ ...regraEditando, name: e.target.value })}
                    placeholder="Ex: HIGH_VALUE_TRANSACTION"
                    className="mt-1"
                  />
                </div>
                <div>
                  <Label htmlFor="description" className="text-sm font-medium">Descrição</Label>
                  <Textarea
                    id="description"
                    value={regraEditando.description}
                    onChange={(e) => setRegraEditando({ ...regraEditando, description: e.target.value })}
                    placeholder="Descreva o que esta regra faz..."
                    className="mt-1"
                    rows={2}
                  />
                </div>
              </div>

              {/* Categoria, Classificação e Peso */}
              <div className="grid grid-cols-3 gap-4">
                <div>
                  <Label className="text-sm font-medium">Categoria *</Label>
                  <Select
                    value={regraEditando.category}
                    onValueChange={(value) => setRegraEditando({ ...regraEditando, category: value })}
                  >
                    <SelectTrigger className="mt-1">
                      <SelectValue />
                    </SelectTrigger>
                    <SelectContent>
                      {CATEGORIAS.map((cat) => (
                        <SelectItem key={cat} value={cat}>{cat}</SelectItem>
                      ))}
                    </SelectContent>
                  </Select>
                </div>
                <div>
                  <Label className="text-sm font-medium">Classificação *</Label>
                  <Select
                    value={regraEditando.classification}
                    onValueChange={(value) => setRegraEditando({ ...regraEditando, classification: value })}
                  >
                    <SelectTrigger className="mt-1">
                      <SelectValue />
                    </SelectTrigger>
                    <SelectContent>
                      <SelectItem value="APPROVED">🟢 Aprovada</SelectItem>
                      <SelectItem value="SUSPICIOUS">🟡 Suspeita</SelectItem>
                      <SelectItem value="FRAUD">🔴 Fraude</SelectItem>
                    </SelectContent>
                  </Select>
                </div>
                <div>
                  <Label className="text-sm font-medium">Peso (0-100)</Label>
                  <Input
                    type="number"
                    min={0}
                    max={100}
                    value={regraEditando.weight}
                    onChange={(e) => setRegraEditando({ ...regraEditando, weight: parseInt(e.target.value) || 0 })}
                    className="mt-1"
                  />
                </div>
              </div>

              {/* Condições */}
              <div>
                <div className="flex items-center justify-between mb-3">
                  <Label className="text-sm font-medium">Condições da Regra *</Label>
                  <div className="flex items-center gap-2">
                    <span className="text-sm text-gray-500">Operador Lógico:</span>
                    <Select
                      value={regraEditando.logicOperator}
                      onValueChange={(value) => setRegraEditando({ ...regraEditando, logicOperator: value })}
                    >
                      <SelectTrigger className="w-24">
                        <SelectValue />
                      </SelectTrigger>
                      <SelectContent>
                        <SelectItem value="AND">E (AND)</SelectItem>
                        <SelectItem value="OR">OU (OR)</SelectItem>
                      </SelectContent>
                    </Select>
                  </div>
                </div>

                <div className="space-y-3 bg-gray-50 p-4 rounded-lg">
                  {regraEditando.conditions.map((cond, index) => (
                    <div key={index} className="flex items-center gap-2 bg-white p-3 rounded border">
                      <span className="text-sm text-gray-500 w-6">{index + 1}.</span>
                      
                      {/* Campo */}
                      <Select
                        value={cond.field}
                        onValueChange={(value) => atualizarCondicao(index, 'field', value)}
                      >
                        <SelectTrigger className="w-48">
                          <SelectValue placeholder="Campo" />
                        </SelectTrigger>
                        <SelectContent>
                          {CAMPOS_PAYLOAD.map((campo) => (
                            <SelectItem key={campo.value} value={campo.value}>
                              {campo.label}
                            </SelectItem>
                          ))}
                        </SelectContent>
                      </Select>

                      {/* Operador */}
                      <Select
                        value={cond.operator}
                        onValueChange={(value) => atualizarCondicao(index, 'operator', value)}
                      >
                        <SelectTrigger className="w-40">
                          <SelectValue placeholder="Operador" />
                        </SelectTrigger>
                        <SelectContent>
                          {OPERADORES.map((op) => (
                            <SelectItem key={op.value} value={op.value}>
                              {op.label}
                            </SelectItem>
                          ))}
                        </SelectContent>
                      </Select>

                      {/* Valor */}
                      <Input
                        value={String(cond.value)}
                        onChange={(e) => atualizarCondicao(index, 'value', e.target.value)}
                        placeholder="Valor"
                        className="flex-1"
                      />

                      {/* Botão remover */}
                      {regraEditando.conditions.length > 1 && (
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => removerCondicao(index)}
                          className="text-red-500 hover:text-red-700 hover:bg-red-50"
                        >
                          <Trash2 className="w-4 h-4" />
                        </Button>
                      )}
                    </div>
                  ))}

                  <Button
                    variant="outline"
                    size="sm"
                    onClick={adicionarCondicao}
                    className="w-full mt-2"
                  >
                    <Plus className="w-4 h-4 mr-2" />
                    Adicionar Condição
                  </Button>
                </div>
              </div>

              {/* Fonte e Ativa */}
              <div className="grid grid-cols-2 gap-4">
                <div>
                  <Label htmlFor="source" className="text-sm font-medium">Fonte/Referência</Label>
                  <Input
                    id="source"
                    value={regraEditando.source}
                    onChange={(e) => setRegraEditando({ ...regraEditando, source: e.target.value })}
                    placeholder="Ex: FEBRABAN, Mastercard..."
                    className="mt-1"
                  />
                </div>
                <div className="flex items-center gap-3 pt-6">
                  <Switch
                    checked={regraEditando.isActive}
                    onCheckedChange={(checked) => setRegraEditando({ ...regraEditando, isActive: checked })}
                  />
                  <Label className="text-sm">Regra Ativa</Label>
                </div>
              </div>

              {/* Aviso sobre campos do payload */}
              <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
                <div className="flex items-start gap-2">
                  <Info className="w-5 h-5 text-blue-600 mt-0.5" />
                  <div className="text-sm text-blue-800">
                    <strong>Campos válidos do Payload:</strong> Apenas os campos listados no seletor podem ser usados nas condições. 
                    Todos os 26 campos disponíveis foram validados e correspondem ao payload de entrada do sistema.
                  </div>
                </div>
              </div>
            </div>
          )}

          <DialogFooter className="gap-2">
            <Button
              variant="outline"
              onClick={() => {
                setModalAberto(false);
                setRegraEditando(null);
              }}
            >
              <X className="w-4 h-4 mr-2" />
              Cancelar
            </Button>
            <Button
              onClick={salvarRegra}
              disabled={salvando || !regraEditando?.name || regraEditando?.conditions.length === 0}
              className="bg-green-600 hover:bg-green-700"
            >
              <Save className="w-4 h-4 mr-2" />
              {salvando ? 'Salvando...' : 'Salvar Regra'}
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>
    </div>
  );
}
