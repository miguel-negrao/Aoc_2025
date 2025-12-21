#!/usr/bin/env python3
import sys
from datetime import datetime


def parse_times(path):
    with open(path, "r", encoding="utf-8") as f:
        rows = [line.strip() for line in f.readlines()]
    entries = [row for row in rows if row]
    if len(entries) % 2 != 0:
        raise SystemExit("Expected an even number of timestamps (start/end pairs).")
    try:
        return [datetime.strptime(entry, "%Y-%m-%d %H:%M") for entry in entries]
    except ValueError as exc:
        raise SystemExit(f"Failed to parse timestamp: {exc}") from exc


def format_minutes(total_minutes):
    hours, minutes = divmod(int(total_minutes), 60)
    return f"{hours}h{minutes:02d}m"


def main():
    path = "dates.txt"
    times = parse_times(path)

    total_minutes = 0
    for idx in range(0, len(times), 2):
        start, end = times[idx], times[idx + 1]
        delta = end - start
        if delta.total_seconds() < 0:
            raise SystemExit(f"Pair {idx // 2 + 1}: end precedes start ({start} -> {end}).")
        minutes = delta.total_seconds() / 60
        total_minutes += minutes
        print(f"Pair {idx // 2 + 1}: {start} -> {end} = {format_minutes(minutes)}")

    print(f"Total: {format_minutes(total_minutes)}")


if __name__ == "__main__":
    main()
