package com.rulex.service.complex.parsing;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;

public final class DateTimeParser {

  private DateTimeParser() {}

  public static LocalDate parseDate(Object value) {
    if (value == null) return null;
    if (value instanceof LocalDate) return (LocalDate) value;
    if (value instanceof LocalDateTime) return ((LocalDateTime) value).toLocalDate();

    String str = String.valueOf(value);
    if (str.matches("\\d{8}")) {
      return LocalDate.parse(str, DateTimeFormatter.ofPattern("yyyyMMdd"));
    }
    return LocalDate.parse(str);
  }

  public static LocalTime parseTime(Object value) {
    if (value == null) return null;
    if (value instanceof LocalTime) return (LocalTime) value;
    if (value instanceof LocalDateTime) return ((LocalDateTime) value).toLocalTime();

    String str = String.valueOf(value);
    if (str.matches("\\d{6}")) {
      return LocalTime.parse(str, DateTimeFormatter.ofPattern("HHmmss"));
    }
    return LocalTime.parse(str);
  }
}
