package com.rulex.service.complex.parsing;

import com.rulex.service.VelocityService;

public final class TimeWindowParser {

  private TimeWindowParser() {}

  public static VelocityService.TimeWindow fromDays(int days) {
    if (days <= 1) return VelocityService.TimeWindow.HOUR_24;
    if (days <= 7) return VelocityService.TimeWindow.DAY_7;
    return VelocityService.TimeWindow.DAY_30;
  }

  public static VelocityService.TimeWindow fromHours(int hours) {
    if (hours <= 1) return VelocityService.TimeWindow.HOUR_1;
    if (hours <= 6) return VelocityService.TimeWindow.HOUR_6;
    if (hours <= 12) return VelocityService.TimeWindow.HOUR_12;
    if (hours <= 24) return VelocityService.TimeWindow.HOUR_24;
    if (hours <= 168) return VelocityService.TimeWindow.DAY_7;
    return VelocityService.TimeWindow.DAY_30;
  }
}
