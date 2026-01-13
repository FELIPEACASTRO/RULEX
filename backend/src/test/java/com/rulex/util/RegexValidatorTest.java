package com.rulex.util;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.regex.Pattern;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

/** Testes para o RegexValidator com proteção contra ReDoS. */
class RegexValidatorTest {

  @Nested
  @DisplayName("Validação de Patterns")
  class ValidationTests {

    @Test
    @DisplayName("Deve aceitar pattern válido simples")
    void shouldAcceptSimpleValidPattern() {
      var result = RegexValidator.validate("^[a-z]+$");
      assertThat(result.valid()).isTrue();
      assertThat(result.errorMessage()).isNull();
    }

    @Test
    @DisplayName("Deve aceitar pattern de email")
    void shouldAcceptEmailPattern() {
      var result = RegexValidator.validate("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$");
      assertThat(result.valid()).isTrue();
    }

    @Test
    @DisplayName("Deve aceitar pattern de telefone")
    void shouldAcceptPhonePattern() {
      var result = RegexValidator.validate("^\\+?[0-9]{10,14}$");
      assertThat(result.valid()).isTrue();
    }

    @Test
    @DisplayName("Deve rejeitar pattern nulo")
    void shouldRejectNullPattern() {
      var result = RegexValidator.validate(null);
      assertThat(result.valid()).isFalse();
      assertThat(result.errorMessage()).contains("vazio");
    }

    @Test
    @DisplayName("Deve rejeitar pattern vazio")
    void shouldRejectEmptyPattern() {
      var result = RegexValidator.validate("");
      assertThat(result.valid()).isFalse();
      assertThat(result.errorMessage()).contains("vazio");
    }

    @Test
    @DisplayName("Deve rejeitar pattern muito longo")
    void shouldRejectTooLongPattern() {
      String longPattern = "a".repeat(RegexValidator.MAX_PATTERN_LENGTH + 1);
      var result = RegexValidator.validate(longPattern);
      assertThat(result.valid()).isFalse();
      assertThat(result.errorMessage()).contains("muito longo");
    }

    @Test
    @DisplayName("Deve rejeitar pattern com sintaxe inválida")
    void shouldRejectInvalidSyntax() {
      var result = RegexValidator.validate("[a-z");
      assertThat(result.valid()).isFalse();
      assertThat(result.errorMessage()).contains("Sintaxe");
    }
  }

  @Nested
  @DisplayName("Proteção contra ReDoS")
  class ReDoSProtectionTests {

    @Test
    @DisplayName("Deve rejeitar (a+)+ - catastrophic backtracking")
    void shouldRejectNestedQuantifier1() {
      var result = RegexValidator.validate("(a+)+");
      assertThat(result.valid()).isFalse();
      assertThat(result.errorMessage()).contains("perigosa");
    }

    @Test
    @DisplayName("Deve rejeitar (a*)*")
    void shouldRejectNestedQuantifier2() {
      var result = RegexValidator.validate("(a*)*");
      assertThat(result.valid()).isFalse();
    }

    @Test
    @DisplayName("Deve rejeitar (.*)+")
    void shouldRejectDotStarPlus() {
      var result = RegexValidator.validate("(.*)+");
      assertThat(result.valid()).isFalse();
    }

    @Test
    @DisplayName("Deve rejeitar (.+)+")
    void shouldRejectDotPlusPlus() {
      var result = RegexValidator.validate("(.+)+");
      assertThat(result.valid()).isFalse();
    }

    @Test
    @DisplayName("Deve rejeitar ([a-zA-Z]+)*")
    void shouldRejectCharClassQuantifier() {
      var result = RegexValidator.validate("([a-zA-Z]+)*");
      assertThat(result.valid()).isFalse();
    }

    @Test
    @DisplayName("Deve rejeitar (a+)+$")
    void shouldRejectEvilRegex() {
      var result = RegexValidator.validate("(a+)+$");
      assertThat(result.valid()).isFalse();
    }

    @Test
    @DisplayName("Deve rejeitar pattern muito complexo")
    void shouldRejectTooComplexPattern() {
      // Pattern com muitos grupos e quantificadores
      String complex = "((a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)+)+";
      var result = RegexValidator.validate(complex);
      assertThat(result.valid()).isFalse();
    }
  }

  @Nested
  @DisplayName("Execução com Timeout")
  class TimeoutTests {

    @Test
    @DisplayName("Deve executar match simples com sucesso")
    void shouldMatchSimplePattern() throws RegexValidator.TimeoutException {
      Pattern pattern = Pattern.compile("^[a-z]+$");
      boolean result = RegexValidator.matchWithTimeout(pattern, "hello");
      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false para não-match")
    void shouldReturnFalseForNoMatch() throws RegexValidator.TimeoutException {
      Pattern pattern = Pattern.compile("^[a-z]+$");
      boolean result = RegexValidator.matchWithTimeout(pattern, "HELLO");
      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("Deve retornar false para input nulo")
    void shouldReturnFalseForNullInput() throws RegexValidator.TimeoutException {
      Pattern pattern = Pattern.compile("^[a-z]+$");
      boolean result = RegexValidator.matchWithTimeout(pattern, null);
      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("Deve truncar input muito longo")
    void shouldTruncateLongInput() throws RegexValidator.TimeoutException {
      Pattern pattern = Pattern.compile("^a+$");
      String longInput = "a".repeat(RegexValidator.MAX_INPUT_LENGTH + 1000);
      // Não deve lançar exceção, deve truncar
      boolean result = RegexValidator.matchWithTimeout(pattern, longInput);
      assertThat(result).isTrue(); // Truncado ainda é só 'a's
    }
  }

  @Nested
  @DisplayName("Safe Compile")
  class SafeCompileTests {

    @Test
    @DisplayName("Deve compilar pattern válido")
    void shouldCompileValidPattern() {
      Pattern pattern = RegexValidator.safeCompile("^[0-9]+$");
      assertThat(pattern).isNotNull();
      assertThat(pattern.matcher("12345").matches()).isTrue();
    }

    @Test
    @DisplayName("Deve retornar null para pattern perigoso")
    void shouldReturnNullForDangerousPattern() {
      Pattern pattern = RegexValidator.safeCompile("(a+)+");
      assertThat(pattern).isNull();
    }

    @Test
    @DisplayName("Deve retornar null para pattern inválido")
    void shouldReturnNullForInvalidPattern() {
      Pattern pattern = RegexValidator.safeCompile("[invalid");
      assertThat(pattern).isNull();
    }
  }

  @Nested
  @DisplayName("Casos de Uso Reais")
  class RealWorldTests {

    @Test
    @DisplayName("Deve validar pattern de CPF")
    void shouldValidateCpfPattern() {
      var result = RegexValidator.validate("^\\d{3}\\.\\d{3}\\.\\d{3}-\\d{2}$");
      assertThat(result.valid()).isTrue();
    }

    @Test
    @DisplayName("Deve validar pattern de CNPJ")
    void shouldValidateCnpjPattern() {
      var result = RegexValidator.validate("^\\d{2}\\.\\d{3}\\.\\d{3}/\\d{4}-\\d{2}$");
      assertThat(result.valid()).isTrue();
    }

    @Test
    @DisplayName("Deve validar pattern de CEP")
    void shouldValidateCepPattern() {
      var result = RegexValidator.validate("^\\d{5}-?\\d{3}$");
      assertThat(result.valid()).isTrue();
    }

    @Test
    @DisplayName("Deve validar pattern de cartão de crédito")
    void shouldValidateCreditCardPattern() {
      var result = RegexValidator.validate("^\\d{4}[- ]?\\d{4}[- ]?\\d{4}[- ]?\\d{4}$");
      assertThat(result.valid()).isTrue();
    }

    @Test
    @DisplayName("Deve validar pattern de UUID simplificado")
    void shouldValidateUuidPattern() {
      // Pattern simplificado para UUID (menos grupos)
      var result = RegexValidator.validate("^[0-9a-f-]{36}$");
      assertThat(result.valid()).isTrue();
    }

    @Test
    @DisplayName("Deve validar pattern de IP simplificado")
    void shouldValidateIpPattern() {
      // Pattern simplificado para IP (menos alternações)
      var result = RegexValidator.validate("^\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}$");
      assertThat(result.valid()).isTrue();
    }
  }
}
