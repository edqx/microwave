const std = @import("std");

pub const DateTime = union(enum) {
    pub const Year = std.math.IntFittingRange(0, 10000);
    pub const Month = std.math.IntFittingRange(1, 12);
    pub const Day = std.math.IntFittingRange(1, 31);

    pub const Hour = std.math.IntFittingRange(0, 23);
    pub const OffsetHour = std.math.IntFittingRange(0, 24);
    pub const Minute = std.math.IntFittingRange(0, 59);
    pub const Second = std.math.IntFittingRange(0, 59);
    pub const Millisecond = u64;

    pub const Date = struct {
        year: Year,
        month: Month,
        day: Day,
    };

    pub const Offset = struct {
        negative: bool,
        hour: OffsetHour,
        minute: Minute,

        pub fn isUtc(self: Offset) bool {
            return self.hour == 0 and self.minute == 0;
        }
    };

    pub const Time = struct {
        hour: Hour,
        minute: Minute,
        second: Second,
        millisecond: ?u64,
    };

    just_date: Date,
    just_time: Time,
    local_date_time: struct {
        date: Date,
        time: Time,
    },
    offset_date_time: struct {
        date: Date,
        time: Time,
        offset: Offset,
    },
};

pub fn parseOffsetDateTimeValue(token_contents: []const u8) !DateTime {
    const offset_index = if (std.mem.lastIndexOfAny(u8, token_contents, "Zz") == token_contents.len - 1)
        token_contents.len - 1
    else blk: {
        const offset_separator = std.mem.lastIndexOfAny(u8, token_contents, "+-") orelse unreachable;
        break :blk offset_separator;
    };

    const datetime_contents = token_contents[0..offset_index];
    const offset_contents = token_contents[offset_index..];

    const local_date_time = try parseLocalDateTimeValue(datetime_contents);
    const offset: DateTime.Offset = if (offset_contents.len == 1 and (offset_contents[0] == 'Z' or offset_contents[0] == 'z')) .{
        .negative = false,
        .hour = 0,
        .minute = 0,
    } else parse_offset: {
        const sign = offset_contents[0];
        std.debug.assert(sign == '+' or sign == '-');

        var time_parts = std.mem.tokenizeAny(u8, offset_contents[1..], ":");

        const hour_part = time_parts.next().?;
        if (hour_part.len < 2) return error.AstError;
        const minute_part = time_parts.next().?;
        if (minute_part.len < 2) return error.AstError;

        var offset: DateTime.Offset = .{
            .negative = false,
            .hour = 0,
            .minute = 0,
        };
        offset.negative = sign == '-';
        offset.hour = std.fmt.parseInt(DateTime.OffsetHour, hour_part, 10) catch return error.AstError;
        if (offset.hour <= -24 or offset.hour >= 24) return error.AstError;
        offset.minute = std.fmt.parseInt(DateTime.Minute, minute_part, 10) catch return error.AstError;
        if (offset.minute >= 60) return error.AstError;

        break :parse_offset offset;
    };

    return .{
        .offset_date_time = .{
            .date = local_date_time.local_date_time.date,
            .time = local_date_time.local_date_time.time,
            .offset = offset,
        },
    };
}

pub fn parseLocalDateTimeValue(token_contents: []const u8) !DateTime {
    const time_index = (std.mem.indexOfAny(u8, token_contents, "Tt ") orelse unreachable) + 1;
    const date_contents = token_contents[0 .. time_index - 1];
    const time_contents = token_contents[time_index..];

    const local_date_value = try parseLocalDateValue(date_contents);
    const local_time_value = try parseLocalTimeValue(time_contents);

    return .{
        .local_date_time = .{
            .date = local_date_value.just_date,
            .time = local_time_value.just_time,
        },
    };
}

pub fn parseLocalDateValue(token_contents: []const u8) !DateTime {
    var time_parts = std.mem.tokenizeAny(u8, token_contents, "-");

    const year_part = time_parts.next().?;
    const month_part = time_parts.next().?;
    if (month_part.len < 2) return error.AstError;

    const day_part = time_parts.next().?;
    if (day_part.len < 2) return error.AstError;

    var date: DateTime.Date = .{
        .year = 0,
        .month = 0,
        .day = 0,
    };
    date.year = std.fmt.parseInt(DateTime.Year, year_part, 10) catch return error.AstError;
    if (date.year >= 10_000) return error.AstError;
    date.month = std.fmt.parseInt(DateTime.Month, month_part, 10) catch return error.AstError;
    if (date.month < 1 or date.month > 12) return error.AstError;
    date.day = std.fmt.parseInt(DateTime.Day, day_part, 10) catch return error.AstError;
    if (date.day < 1 or date.day > 31) return error.AstError;

    const is_leap_year = (date.year % 4 == 0) and (date.year % 100 != 0 or date.year % 400 == 0);
    const month_dates: []const usize = &.{ 31, if (is_leap_year) 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
    if (date.day > month_dates[date.month - 1]) return error.AstError;

    return .{
        .just_date = date,
    };
}

pub fn parseLocalTimeValue(token_contents: []const u8) !DateTime {
    var millisecond_parts = std.mem.tokenizeAny(u8, token_contents, ".");

    const non_millisecond_part = millisecond_parts.next().?;
    var time_parts = std.mem.tokenizeAny(u8, non_millisecond_part, ":");

    const hour_part = time_parts.next().?;
    if (hour_part.len < 2) return error.AstError;
    const minute_part = time_parts.next().?;
    if (minute_part.len < 2) return error.AstError;

    const second_part = time_parts.next().?;
    if (second_part.len < 2) return error.AstError;

    const millisecond_part = millisecond_parts.next();

    var time: DateTime.Time = .{
        .hour = 0,
        .minute = 0,
        .second = 0,
        .millisecond = null,
    };
    time.hour = std.fmt.parseInt(DateTime.Hour, hour_part, 10) catch return error.AstError;
    if (time.hour >= 24) return error.AstError;
    time.minute = std.fmt.parseInt(DateTime.Minute, minute_part, 10) catch return error.AstError;
    if (time.minute >= 60) return error.AstError;
    time.second = std.fmt.parseInt(DateTime.Second, second_part, 10) catch return error.AstError;
    if (time.second >= 60) return error.AstError;
    if (millisecond_part) |millisecond_str| {
        time.millisecond = std.fmt.parseInt(DateTime.Millisecond, millisecond_str, 10) catch return error.AstError;
    }

    return .{
        .just_time = time,
    };
}
