// Script para validar se todos os campos das regras estão no payload

// Campos válidos do TransactionRequest (payload de entrada)
const CAMPOS_VALIDOS_PAYLOAD = [
  'externalTransactionId',
  'transactionAmount',
  'transactionDate',
  'transactionTime',
  'mcc',
  'merchantCountryCode',
  'merchantId',
  'merchantName',
  'merchantPostalCode',
  'customerIdFromHeader',
  'customerPresent',
  'consumerAuthenticationScore',
  'externalScore3',
  'cvv2Response',
  'cvv2EntryLimitExceeded',
  'pinEntryLimitExceeded',
  'cryptogramValid',
  'cavvResult',
  'eciIndicator',
  'posSecurity',
  'posOffPremises',
  'posEntryMode',
  'cardAipStatic',
  'cardAipDynamic',
  'terminalVerificationResults',
  'cardExpireDate',
  'cardCaptured',
  'recurringTransaction',
  'pan',
  'panSequenceNumber',
  'acquirerId',
  'acquirerCountryCode',
  'gmtOffset',
  'transactionCurrencyCode',
  'billingCurrencyCode',
  'conversionRate',
  'authorizationIdResponse',
  'responseCode',
  'additionalResponseData',
  // Campos customizados
  'custom1', 'custom2', 'custom3', 'custom4', 'custom5',
  'custom6', 'custom7', 'custom8', 'custom9', 'custom10',
  'custom11', 'custom12', 'custom13', 'custom14', 'custom15',
  'custom16', 'custom17', 'custom18', 'custom19', 'custom20',
];

// Campos usados nas regras (extraídos do banco)
const CAMPOS_USADOS_REGRAS = [
  'acquirerCountryCode',
  'cardAipDynamic',
  'cardAipStatic',
  'cardCaptured',
  'cardExpireDate',
  'cavvResult',
  'consumerAuthenticationScore',
  'cryptogramValid',
  'customerPresent',
  'cvv2EntryLimitExceeded',
  'cvv2Response',
  'eciIndicator',
  'externalScore3',
  'mcc',
  'merchantCountryCode',
  'pinEntryLimitExceeded',
  'posEntryMode',
  'posOffPremises',
  'posSecurity',
  'recurringTransaction',
  'terminalVerificationResults',
  'transactionAmount',
  'transactionTime',
];

console.log('='.repeat(60));
console.log('VALIDAÇÃO DE CAMPOS DAS REGRAS vs PAYLOAD DE ENTRADA');
console.log('='.repeat(60));
console.log('');

console.log('📋 CAMPOS VÁLIDOS NO PAYLOAD DE ENTRADA:');
console.log(`   Total: ${CAMPOS_VALIDOS_PAYLOAD.length} campos`);
console.log('');

console.log('🔍 CAMPOS USADOS NAS REGRAS:');
console.log(`   Total: ${CAMPOS_USADOS_REGRAS.length} campos`);
console.log('');

// Verificar campos inválidos
const camposInvalidos = CAMPOS_USADOS_REGRAS.filter(
  campo => !CAMPOS_VALIDOS_PAYLOAD.includes(campo)
);

if (camposInvalidos.length === 0) {
  console.log('✅ RESULTADO: TODAS AS REGRAS USAM CAMPOS VÁLIDOS DO PAYLOAD!');
  console.log('');
  console.log('Todos os 23 campos usados nas regras estão presentes no payload de entrada.');
} else {
  console.log('❌ RESULTADO: CAMPOS INVÁLIDOS ENCONTRADOS!');
  console.log('');
  console.log('Os seguintes campos NÃO existem no payload de entrada:');
  camposInvalidos.forEach(campo => console.log(`   - ${campo}`));
}

console.log('');
console.log('='.repeat(60));
console.log('LISTA DE CAMPOS USADOS NAS REGRAS (com descrição):');
console.log('='.repeat(60));

const descricoesCampos = {
  'transactionAmount': 'Valor da transação em centavos',
  'transactionTime': 'Horário da transação (HHMMSS)',
  'transactionDate': 'Data da transação (YYYYMMDD)',
  'mcc': 'Código de Categoria do Comerciante',
  'merchantCountryCode': 'Código do país do comerciante (ISO 3166)',
  'customerPresent': 'Cliente presente (Y/N)',
  'consumerAuthenticationScore': 'Score de autenticação do consumidor (0-100)',
  'externalScore3': 'Score externo de terceiros',
  'cvv2Response': 'Resposta da validação do CVV2 (M=Match, N=No Match)',
  'cvv2EntryLimitExceeded': 'Limite de tentativas de CVV excedido',
  'pinEntryLimitExceeded': 'Limite de tentativas de PIN excedido',
  'cryptogramValid': 'Criptograma EMV válido',
  'cavvResult': 'Resultado da verificação 3D Secure',
  'eciIndicator': 'Indicador de E-commerce (ECI)',
  'posSecurity': 'Nível de segurança do terminal POS',
  'posOffPremises': 'Terminal fora das instalações',
  'posEntryMode': 'Modo de entrada do cartão',
  'cardAipStatic': 'Indicador de autenticação estática do cartão',
  'cardAipDynamic': 'Indicador de autenticação dinâmica do cartão',
  'terminalVerificationResults': 'Resultados da verificação do terminal',
  'cardExpireDate': 'Data de expiração do cartão',
  'cardCaptured': 'Cartão capturado/retido',
  'recurringTransaction': 'Transação recorrente',
  'acquirerCountryCode': 'Código do país do adquirente',
};

CAMPOS_USADOS_REGRAS.forEach(campo => {
  const descricao = descricoesCampos[campo] || 'Sem descrição';
  console.log(`• ${campo}`);
  console.log(`  └─ ${descricao}`);
});

console.log('');
console.log('='.repeat(60));
