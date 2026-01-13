import http from 'k6/http';
import { check } from 'k6';

const BASE_URL = 'http://localhost:8080/api';
const AUTH_HEADER = 'Basic YWRtaW46cnVsZXg=';

export default function () {
  const payload = {
    externalTransactionId: `TX-${Date.now()}-${__VU}-${__ITER}`,
    customerIdFromHeader: "CUST-12345",
    customerAcctNumber: 4111111111111111,
    pan: "411111******1111",
    transactionAmount: 250.00,
    transactionDate: 20260105,
    transactionTime: 143000,
    transactionCurrencyCode: 986,
    mcc: 5411,
    consumerAuthenticationScore: 85,
    externalScore3: 70,
    cavvResult: 2,
    eciIndicator: 5,
    atcCard: 100,
    atcHost: 100,
    tokenAssuranceLevel: 3,
    availableCredit: 5000.00,
    cardCashBalance: 0.00,
    cardDelinquentAmount: 0.00
  };

  const response = http.post(
    `${BASE_URL}/transactions/analyze`,
    JSON.stringify(payload),
    {
      headers: {
        'Content-Type': 'application/json',
        'Authorization': AUTH_HEADER,
      },
    }
  );

  console.log(`Status: ${response.status}, Body: ${response.body.substring(0, 200)}`);
  
  check(response, {
    'status is 200': (r) => r.status === 200,
  });
}
