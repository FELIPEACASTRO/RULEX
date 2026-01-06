package com.rulex.domain.service;

import com.rulex.domain.exception.TamperDetectedException;

/**
 * Domain Service para detecção de adulteração de payload.
 *
 * <p>Lógica pura de domínio sem dependências de framework. Compara hashes de payload para detectar
 * tentativas de reuso de externalTransactionId com payload diferente.
 */
public class TamperDetector {

  /**
   * Verifica se houve adulteração de payload.
   *
   * @param externalTransactionId ID da transação
   * @param originalHash hash do payload original armazenado
   * @param receivedHash hash do payload recebido
   * @throws TamperDetectedException se hashes não conferem
   */
  public void checkTamper(String externalTransactionId, String originalHash, String receivedHash) {
    if (originalHash == null || receivedHash == null) {
      return; // Sem hash para comparar
    }

    if (!originalHash.equals(receivedHash)) {
      throw new TamperDetectedException(externalTransactionId, originalHash, receivedHash);
    }
  }

  /**
   * Verifica se houve adulteração e retorna resultado sem lançar exceção.
   *
   * @param originalHash hash do payload original
   * @param receivedHash hash do payload recebido
   * @return true se foi detectada adulteração
   */
  public boolean isTampered(String originalHash, String receivedHash) {
    if (originalHash == null || receivedHash == null) {
      return false;
    }
    return !originalHash.equals(receivedHash);
  }
}
