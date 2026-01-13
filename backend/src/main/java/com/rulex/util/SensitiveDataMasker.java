package com.rulex.util;

import java.util.regex.Pattern;

/**
 * Utility para mascarar dados sensiveis em logs e outputs.
 *
 * <p>Evita exposicao de PAN, CPF, tokens e outros dados sensiveis em conformidade com PCI-DSS e
 * LGPD.
 */
public final class SensitiveDataMasker {

  private SensitiveDataMasker() {
    // Utility class
  }

  /** Padrao para PAN (13-19 digitos) */
  private static final Pattern PAN_PATTERN = Pattern.compile("\\b(\\d{4})\\d{5,11}(\\d{4})\\b");

  /** Padrao para CPF (11 digitos, com ou sem formatacao) */
  private static final Pattern CPF_PATTERN =
      Pattern.compile("\\b(\\d{3})\\.?(\\d{3})\\.?(\\d{3})\\-?(\\d{2})\\b");

  /** Padrao para CNPJ (14 digitos, com ou sem formatacao) */
  private static final Pattern CNPJ_PATTERN =
      Pattern.compile("\\b(\\d{2})\\.?(\\d{3})\\.?(\\d{3})/?(\\d{4})\\-?(\\d{2})\\b");

  /** Padrao para email */
  private static final Pattern EMAIL_PATTERN =
      Pattern.compile(
          "\\b([a-zA-Z0-9._%+-]{1,3})[a-zA-Z0-9._%+-]*@([a-zA-Z0-9.-]+\\.[a-zA-Z]{2,})\\b");

  /** Padrao para telefone brasileiro */
  private static final Pattern PHONE_PATTERN =
      Pattern.compile("\\b\\(?\\d{2}\\)?\\s*\\d{4,5}[\\-\\s]?(\\d{4})\\b");

  /** Caractere de mascara */
  private static final String MASK_CHAR = "*";

  /**
   * Mascara um PAN (numero de cartao) mostrando apenas primeiros 4 e ultimos 4 digitos.
   *
   * @param pan numero do cartao
   * @return PAN mascarado (ex: 1234****5678)
   */
  public static String maskPan(String pan) {
    if (pan == null || pan.length() < 8) {
      return "****";
    }
    String digitsOnly = pan.replaceAll("\\D", "");
    if (digitsOnly.length() < 8) {
      return "****";
    }
    int maskLength = digitsOnly.length() - 8;
    return digitsOnly.substring(0, 4)
        + MASK_CHAR.repeat(maskLength)
        + digitsOnly.substring(digitsOnly.length() - 4);
  }

  /**
   * Mascara um CPF mostrando apenas ultimos 2 digitos.
   *
   * @param cpf CPF com ou sem formatacao
   * @return CPF mascarado (ex: ***.***.**-78)
   */
  public static String maskCpf(String cpf) {
    if (cpf == null || cpf.length() < 2) {
      return "***.***.***-**";
    }
    String digitsOnly = cpf.replaceAll("\\D", "");
    if (digitsOnly.length() < 11) {
      return "***.***.***-**";
    }
    return "***.***.***-" + digitsOnly.substring(9, 11);
  }

  /**
   * Mascara um CNPJ mostrando apenas ultimos 2 digitos.
   *
   * @param cnpj CNPJ com ou sem formatacao
   * @return CNPJ mascarado (ex: xx.xxx.xxx/xxxx-78)
   */
  public static String maskCnpj(String cnpj) {
    if (cnpj == null || cnpj.length() < 2) {
      return "**.***.****/****-**";
    }
    String digitsOnly = cnpj.replaceAll("\\D", "");
    if (digitsOnly.length() < 14) {
      return "**.***.****/****-**";
    }
    return "**.***.****/****-" + digitsOnly.substring(12, 14);
  }

  /**
   * Mascara um email mostrando apenas primeiros 3 caracteres do usuario.
   *
   * @param email endereco de email
   * @return email mascarado (ex: joh***@domain.com)
   */
  public static String maskEmail(String email) {
    if (email == null || !email.contains("@")) {
      return "***@***.***";
    }
    String[] parts = email.split("@");
    if (parts.length != 2) {
      return "***@***.***";
    }
    String user = parts[0];
    String domain = parts[1];
    String maskedUser;
    if (user.length() > 3) {
      maskedUser = user.substring(0, 3) + "***";
    } else {
      maskedUser = user.substring(0, 1) + "***";
    }
    return maskedUser + "@" + domain;
  }

  /**
   * Mascara um telefone mostrando apenas ultimos 4 digitos.
   *
   * @param phone numero de telefone
   * @return telefone mascarado (ex: (**) *****-1234)
   */
  public static String maskPhone(String phone) {
    if (phone == null || phone.length() < 4) {
      return "(**) *****-****";
    }
    String digitsOnly = phone.replaceAll("\\D", "");
    if (digitsOnly.length() < 4) {
      return "(**) *****-****";
    }
    return "(**) *****-" + digitsOnly.substring(digitsOnly.length() - 4);
  }

  /**
   * Mascara todos os dados sensiveis em uma string.
   *
   * <p>Detecta e mascara automaticamente: PAN, CPF, CNPJ, email, telefone.
   *
   * @param text texto contendo dados sensiveis
   * @return texto com dados mascarados
   */
  public static String maskAll(String text) {
    if (text == null || text.isEmpty()) {
      return text;
    }

    String masked = text;

    // Mascarar PANs
    masked = PAN_PATTERN.matcher(masked).replaceAll("$1****$2");

    // Mascarar CPFs
    masked = CPF_PATTERN.matcher(masked).replaceAll("***.***.$3-$4");

    // Mascarar CNPJs
    masked = CNPJ_PATTERN.matcher(masked).replaceAll("**.$2.***/$4-$5");

    // Mascarar emails
    masked = EMAIL_PATTERN.matcher(masked).replaceAll("$1***@$2");

    // Mascarar telefones
    masked = PHONE_PATTERN.matcher(masked).replaceAll("(**) *****-$1");

    return masked;
  }

  /**
   * Cria uma versao segura de um ID de transacao para logs.
   *
   * @param transactionId ID completo
   * @return ID parcialmente mascarado
   */
  public static String maskTransactionId(String transactionId) {
    if (transactionId == null || transactionId.length() < 8) {
      return "***";
    }
    return transactionId.substring(0, 4)
        + "..."
        + transactionId.substring(transactionId.length() - 4);
  }

  /**
   * Retorna asteriscos para valor completamente mascarado.
   *
   * @param length quantidade de asteriscos
   * @return string de asteriscos
   */
  public static String fullMask(int length) {
    return MASK_CHAR.repeat(Math.max(4, length));
  }

  /**
   * Verifica se uma string parece conter um PAN.
   *
   * @param text texto a verificar
   * @return true se contem padrao de PAN
   */
  public static boolean containsPan(String text) {
    if (text == null) {
      return false;
    }
    return PAN_PATTERN.matcher(text).find();
  }

  /**
   * Verifica se uma string parece conter um CPF.
   *
   * @param text texto a verificar
   * @return true se contem padrao de CPF
   */
  public static boolean containsCpf(String text) {
    if (text == null) {
      return false;
    }
    return CPF_PATTERN.matcher(text).find();
  }
}
