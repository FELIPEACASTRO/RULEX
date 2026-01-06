/**
 * Mapeamento completo de todos os 102 campos do TransactionRequest
 * para labels em português legíveis por humanos.
 *
 * Fonte: TransactionRequest.java
 * @version 1.0.0
 */

export const FIELD_LABELS: Record<string, string> = {
  // Identificadores da Transação
  externalTransactionId: 'ID Externo da Transação',
  transactionDate: 'Data da Transação',
  transactionTime: 'Hora da Transação',
  transactionType: 'Tipo de Transação',
  transactionCategory: 'Categoria da Transação',
  transactionAmount: 'Valor da Transação',
  transactionCurrencyCode: 'Código da Moeda',
  transactionCurrencyConversionRate: 'Taxa de Conversão',
  
  // Cliente
  customerIdFromHeader: 'ID do Cliente (Header)',
  clientIdFromHeader: 'ID do Cliente (Header Alt)',
  customerAcctNumber: 'Número da Conta do Cliente',
  customerPresent: 'Cliente Presente',
  
  // Cartão
  pan: 'Número do Cartão (PAN)',
  cardExpireDate: 'Data de Validade do Cartão',
  cardSeqNum: 'Número de Sequência do Cartão',
  cardMediaType: 'Tipo de Mídia do Cartão',
  expandedBIN: 'BIN Expandido',
  cardCashBalance: 'Saldo em Dinheiro do Cartão',
  cardDelinquentAmount: 'Valor Inadimplente do Cartão',
  
  // Token
  tokenId: 'ID do Token',
  tokenRequestorId: 'ID do Solicitante do Token',
  tokenAssuranceLevel: 'Nível de Garantia do Token',
  tokenizationIndicator: 'Indicador de Tokenização',
  paymentInstrumentId: 'ID do Instrumento de Pagamento',
  
  // Merchant
  merchantId: 'ID do Merchant',
  merchantName: 'Nome do Merchant',
  merchantCity: 'Cidade do Merchant',
  merchantState: 'Estado do Merchant',
  merchantCountryCode: 'País do Merchant',
  merchantPostalCode: 'CEP do Merchant',
  mcc: 'Categoria do Merchant (MCC)',
  onUsMerchantId: 'ID Merchant On-Us',
  
  // Terminal/POS
  terminalId: 'ID do Terminal',
  terminalType: 'Tipo de Terminal',
  terminalEntryCapability: 'Capacidade de Entrada do Terminal',
  terminalVerificationResults: 'Resultados de Verificação do Terminal',
  posEntryMode: 'Modo de Entrada POS',
  posConditionCode: 'Código de Condição POS',
  posCardCapture: 'Captura de Cartão POS',
  posOffPremises: 'POS Fora das Instalações',
  posSecurity: 'Segurança POS',
  
  // Autenticação e Verificação
  authId: 'ID de Autorização',
  authIndicator: 'Indicador de Autorização',
  authResponseCode: 'Código de Resposta da Autorização',
  authDecisionCode: 'Código de Decisão da Autorização',
  authPostFlag: 'Flag de Pós-Autorização',
  processorAuthReasonCode: 'Código de Razão do Processador',
  
  // CVV/CVV2
  cvv2Present: 'CVV2 Presente',
  cvv2Response: 'Resposta CVV2',
  cvvVerifyCode: 'Código de Verificação CVV',
  cvvPinTryLimitExceeded: 'Limite de Tentativas PIN Excedido',
  
  // AVS (Address Verification System)
  avsRequest: 'Requisição AVS',
  
  // PIN
  pinVerifyCode: 'Código de Verificação PIN',
  cvrofflinePinVerificationPerformed: 'Verificação PIN Offline Realizada',
  cvrofflinePinVerificationFailed: 'Verificação PIN Offline Falhou',
  
  // Criptografia e Segurança
  cryptogramValid: 'Criptograma Válido',
  cavvResult: 'Resultado CAVV',
  cavvKeyIndicator: 'Indicador de Chave CAVV',
  cardVerificationResults: 'Resultados de Verificação do Cartão',
  
  // AIP (Application Interchange Profile)
  cardAipCombined: 'AIP Combinado',
  cardAipDynamic: 'AIP Dinâmico',
  cardAipStatic: 'AIP Estático',
  cardAipVerify: 'AIP Verificação',
  cardAipRisk: 'AIP Risco',
  cardAipIssuerAuthentication: 'AIP Autenticação do Emissor',
  
  // ATC (Application Transaction Counter)
  atcCard: 'ATC do Cartão',
  atcHost: 'ATC do Host',
  
  // ECI (Electronic Commerce Indicator)
  eciIndicator: 'Indicador ECI',
  
  // Acquirer
  acquirerId: 'ID do Adquirente',
  acquirerBin: 'BIN do Adquirente',
  acquirerCountry: 'País do Adquirente',
  
  // Network
  networkId: 'ID da Rede',
  
  // Scores e Autenticação
  externalScore3: 'Score Externo 3',
  consumerAuthenticationScore: 'Score de Autenticação do Consumidor',
  secondFactorAuthCode: 'Código de Autenticação de Segundo Fator',
  
  // Workflow e Portfolio
  workflow: 'Workflow',
  portfolio: 'Portfólio',
  
  // Crédito
  availableCredit: 'Crédito Disponível',
  
  // Outros
  tranCode: 'Código de Transação',
  recordType: 'Tipo de Registro',
  recordCreationDate: 'Data de Criação do Registro',
  recordCreationTime: 'Hora de Criação do Registro',
  recordCreationMilliseconds: 'Milissegundos de Criação do Registro',
  dataSpecificationVersion: 'Versão da Especificação de Dados',
  gmtOffset: 'Offset GMT',
  idMethod: 'Método de Identificação',
  checkNumber: 'Número do Cheque',
  standinAdvice: 'Aviso Stand-In',
  atmOwner: 'Proprietário do ATM',
  
  // Campos de Usuário (User Data)
  userData01: 'Dados do Usuário 01',
  userData02: 'Dados do Usuário 02',
  userData03: 'Dados do Usuário 03',
  userData04: 'Dados do Usuário 04',
  userData05: 'Dados do Usuário 05',
  userData06: 'Dados do Usuário 06',
  userData06_2: 'Dados do Usuário 06.2',
  userData09: 'Dados do Usuário 09',
  
  // Indicadores de Usuário
  userIndicator01: 'Indicador do Usuário 01',
  userIndicator03: 'Indicador do Usuário 03',
  userIndicator04: 'Indicador do Usuário 04',
  userIndicator05: 'Indicador do Usuário 05',
  userIndicator08: 'Indicador do Usuário 08',
};

/**
 * Obtém o label em português para um campo, ou retorna o nome do campo
 * se não houver tradução disponível.
 */
export function getFieldLabel(fieldName: string): string {
  return FIELD_LABELS[fieldName] || fieldName;
}

/**
 * Obtém todos os nomes de campos disponíveis.
 */
export function getAllFieldNames(): string[] {
  return Object.keys(FIELD_LABELS);
}

/**
 * Busca campos por texto (nome ou label).
 */
export function searchFields(query: string): string[] {
  const lowerQuery = query.toLowerCase();
  return Object.entries(FIELD_LABELS)
    .filter(([fieldName, label]) => 
      fieldName.toLowerCase().includes(lowerQuery) || 
      label.toLowerCase().includes(lowerQuery)
    )
    .map(([fieldName]) => fieldName);
}
