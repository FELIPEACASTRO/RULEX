package com.rulex.util;

public final class PanMaskingUtil {

    private PanMaskingUtil() {}

    public static String mask(String pan) {
        if (pan == null || pan.isBlank()) {
            return "";
        }
        String trimmed = pan.trim();
        if (trimmed.length() <= 10) {
            return "******";
        }
        String first6 = trimmed.substring(0, 6);
        String last4 = trimmed.substring(trimmed.length() - 4);
        return first6 + "******" + last4;
    }
}
