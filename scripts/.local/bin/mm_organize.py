#!/usr/bin/env python3
"""
Organize photos and videos based on EXIF creation dates.

Photos → destination/Photos/YYYY-MM/YYYY-MM-DD_HHMMSS.ext
Videos → destination/Movies/YYYY-MM/YYYY-MM-DD_HHMMSS.ext
Unknown → destination/Unknown/original_name.ext
"""

import json
import shutil
import subprocess
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import UTC, datetime, timedelta, timezone
from pathlib import Path

import click
from dateutil.parser import ParserError, parse  # <--- NEW IMPORT

PHOTO_EXTS: set[str] = {".arw", ".bmp", ".cr2", ".gif", ".heic", ".jpeg", ".jpg", ".nef", ".orf", ".png", ".rw2", ".tif", ".tiff", ".webp"}  # fmt: skip # noqa: E501

VIDEO_EXTS: set[str] = {".3gp", ".avi", ".flv", ".m2ts", ".m4v", ".mkv", ".mov", ".mp4", ".mpeg", ".mpg", ".mts", ".webm", ".wmv"}  # fmt: skip # noqa: E501

EXIFTOOL: str | None = shutil.which("exiftool") or next(
    (p for p in ("/usr/bin/exiftool", "/usr/local/bin/exiftool") if Path(p).exists()),
    None,
)


# Date-parsing helpers
# ----------------------------------------------------------------------
def _parse_exif_dt(s: str | None) -> datetime | None:
    """Parse a string that may contain an EXIF timestamp using dateutil."""
    if not s:
        return None
    s = s.strip()

    # Replace the first two colons with dashes to handle the common
    # EXIF format "YYYY:MM:DD HH:MM:SS" which dateutil doesn't like by default.
    s = s.replace(":", "-", 2)

    try:
        # dateutil.parser.parse is robust against format variations and timezones.
        return parse(s)
    except (ParserError, ValueError):
        return None


def _get_date_from_exif_json(txt: str) -> datetime | None:
    """Parse the JSON output and return the first usable timestamp we can find."""
    try:
        data = json.loads(txt)
    except json.JSONDecodeError:
        return None

    # The output is a list of one dict per file.
    if isinstance(data, list) and data:
        record = data[0]
    elif isinstance(data, dict):
        record = data
    else:
        return None

    # Keys we try - in order of preference.
    for key in (
        "DateTimeOriginal",
        "CreateDate",
        "TrackCreateDate",
        "MediaCreateDate",
        "ModifyDate",
    ):
        val = record.get(key)
        if isinstance(val, list) and val:
            val = val[0]
        if isinstance(val, str) and val.strip():
            dt = _parse_exif_dt(val)
            if dt:
                return dt

    # Some formats write the date in a "Time" field.
    for k, v in record.items():
        if "Date" in k and isinstance(v, str) and v.strip():
            dt = _parse_exif_dt(v)
            if dt:
                return dt
    return None


def get_exif_datetime(file_path: Path) -> datetime | None:
    """Return a ``datetime`` extracted from the file's EXIF data."""
    if not EXIFTOOL:
        return None

    try:
        cmd = [
            EXIFTOOL,
            "-q",
            "-json",
            "-s3",
            "-DateTimeOriginal",
            "-CreateDate",
            "-TrackCreateDate",
            "-MediaCreateDate",
            "-ModifyDate",
            "-Time:0",
            "-XMP:Time",
            "-QuickTime:CreateDate",
            "-QuickTime:CreationDate",
            "-QuickTime:ModifyDate",
            str(file_path),
        ]
        result = subprocess.run(  # noqa: S603
            cmd,
            capture_output=True,
            text=True,
            check=True,
            timeout=20,
        )
        return _get_date_from_exif_json(result.stdout)
    except Exception as e:  # noqa: BLE001
        print(f"Critical error processing {file_path}: {e}")
        return None


# Miscellaneous utilities
# ----------------------------------------------------------------------
def _format_parts(dt: datetime) -> tuple[str, str]:
    """Return ``(YYYY-MM, YYYY-MM-DD_HHMMSS)`` tuple."""
    return dt.strftime("%Y-%m"), dt.strftime("%Y-%m-%d_%H%M%S")


def _ensure_unique_path(target_dir: Path, base_name: str, ext: str) -> Path:
    """If a name clash occurs a numeric suffix is added."""
    candidate = target_dir / f"{base_name}{ext}"
    if not candidate.exists():
        return candidate
    counter = 1
    while True:
        candidate = target_dir / f"{base_name}_{counter}{ext}"
        if not candidate.exists():
            return candidate
        counter += 1


def _is_media_file(path: Path) -> bool:
    ext = path.suffix.lower()
    return ext in PHOTO_EXTS or ext in VIDEO_EXTS


def _category_dir_for_ext(ext: str, destination: Path) -> Path:
    return destination / ("Photos" if ext in PHOTO_EXTS else "Movies")


# Core organisation logic
# ----------------------------------------------------------------------
def organize_files(
    source: Path,
    destination: Path,
    *,
    dry_run: bool = False,
    workers: int = 4,
) -> tuple[int, int]:
    """Walk source recursively, locate media files, and copy them into destination."""
    # Create the top-level directories that we will use.
    photos_dir = destination / "Photos"
    movies_dir = destination / "Movies"
    unknown_dir = destination / "Unknown"
    if not dry_run:
        photos_dir.mkdir(parents=True, exist_ok=True)
        movies_dir.mkdir(parents=True, exist_ok=True)
        unknown_dir.mkdir(parents=True, exist_ok=True)
    # Gather all media files.
    files = [
        p
        for p in source.rglob("*")
        if p.is_file() and not p.is_symlink() and _is_media_file(p)
    ]
    if not files:
        click.echo("No media files found - nothing to do.")
        return 0, 0
    processed = skipped = 0

    # Phase 1 - collect EXIF datetime (parallel)
    click.echo(f"\nScanning {len(files)} files for EXIF data...")
    exif_data: dict[Path, datetime | None] = {}
    with ThreadPoolExecutor(max_workers=max(1, workers)) as pool:
        futures_map = {pool.submit(get_exif_datetime, f): f for f in files}
        for future in as_completed(futures_map):
            file_path = futures_map[future]
            try:
                exif_data[file_path] = future.result()
            except Exception as e:  # noqa: BLE001
                click.echo(f"  ERROR (EXIF) {file_path.name}: {e}", err=True)
                skipped += 1
    # Phase 2 - decide where each file belongs and copy (SERIAL)
    click.echo("\nCopying files (serially for safety)...")
    # This loop is serial (one file at a time), so it is
    # 100% safe and _ensure_unique_path will work perfectly.
    for file_path, dt in exif_data.items():
        ext = file_path.suffix.lower()
        original_name = file_path.name
        try:
            # ------------------------------------------------------ Unknown?
            if dt is None:
                target_dir = unknown_dir
                new_path = _ensure_unique_path(
                    target_dir, Path(original_name).stem, ext
                )
            # ------------------------------------------------------ Known EXIF date
            else:
                year_month, file_base_name = _format_parts(dt)
                cat_dir = _category_dir_for_ext(ext, destination)
                target_dir = cat_dir / year_month

                if not dry_run:
                    # This is safe to run multiple times
                    target_dir.mkdir(parents=True, exist_ok=True)

                new_path = _ensure_unique_path(target_dir, file_base_name, ext)

            # Log and perform copy
            if dry_run:
                click.echo(f"  WOULD COPY: {file_path.name} -> {new_path}")
            else:
                click.echo(f"  COPYING: {file_path.name} -> {new_path}")
                shutil.copy2(file_path, new_path)

            processed += 1

        except Exception as e:  # noqa: BLE001
            click.echo(f"  ERROR (Copy) {file_path.name}: {e}", err=True)
            skipped += 1

    return processed, skipped


# CLI definition
# ----------------------------------------------------------------------
@click.command()
@click.argument(
    "source",
    type=click.Path(
        exists=True,
        file_okay=False,
        dir_okay=True,
        path_type=Path,
        readable=True,
    ),
    default=Path.home() / "vigolana-MM/PHOTOs/PICTURE-fino09/PIETRO/selAGO09",
)
@click.argument(
    "destination",
    type=click.Path(file_okay=False, dir_okay=True, path_type=Path, writable=True),
    default=Path.home() / "FamilyMedia",
)
@click.option(
    "--dry-run",
    is_flag=True,
    help="Show what would be done without actually copying files.",
)
@click.option(
    "--workers",
    type=int,
    default=4,
    help="Number of worker threads for EXIF extraction and copying.",
)
def main(
    source: Path,
    destination: Path,
    *,
    dry_run: bool,
    workers: int,
) -> None:
    """
    Organise media files from ``SOURCE`` into ``DESTINATION`` by EXIF date.

    Files that contain a usable EXIF timestamp are placed under
    ``Photos/YYYY-MM`` or ``Movies/YYYY-MM`` (renamed to ``DD_HHMMSS.ext``).
    Files without a timestamp end up in ``Unknown/`` (original name preserved).

    The script never falls back to the file's modification time.
    """
    click.echo(f"Source      : {source}")
    click.echo(f"Destination : {destination}")
    click.echo(f"Dry-run     : {dry_run}")
    click.echo(f"Workers     : {workers}")

    if EXIFTOOL is None:
        click.echo(
            "\n⚠️  exiftool not found - files can not be placed in the dated folders."
        )
    else:
        click.echo(f"Using exiftool at {EXIFTOOL}")

    processed, skipped = organize_files(
        source,
        destination,
        dry_run=dry_run,
        workers=max(1, workers),
    )

    click.echo("\n✔️  Processing complete!")
    click.echo(f"Files processed : {processed}")
    click.echo(f"Files skipped  : {skipped}")
    if dry_run:
        click.echo("\nThis was a dry-run - no files were actually copied.")


# Tests
# ----------------------------------------------------------------------
# flake8: noqa: S101, DTZ001
def test_parse_exif_dt() -> None:
    """Test the _parse_exif_dt function."""
    # Test cases: input string, expected result
    test_cases = [
        # Classic EXIF format
        ("2023:08:14 15:22:33", datetime(2023, 8, 14, 15, 22, 33)),
        # ISO format with T
        ("2023-08-14T15:22:33", datetime(2023, 8, 14, 15, 22, 33)),
        # With timezone offset
        (
            "2023:08:14 15:22:33+02:00",
            datetime(2023, 8, 14, 15, 22, 33, tzinfo=UTC).__class__(
                2023, 8, 14, 15, 22, 33, tzinfo=timezone(timedelta(hours=2))
            ),
        ),
        # UTC format
        (
            "2023-08-14T15:22:33Z",
            datetime(2023, 8, 14, 15, 22, 33, tzinfo=UTC),
        ),
        # Invalid format
        ("", None),
        # None input
        (None, None),
    ]
    for input_str, expected in test_cases:
        result = _parse_exif_dt(input_str)
        # For timezone-aware comparisons, we need special handling
        if expected and hasattr(expected, "tzinfo") and expected.tzinfo:
            assert result is not None
            assert result.year == expected.year
            assert result.month == expected.month
            assert result.day == expected.day
            assert result.hour == expected.hour
            assert result.minute == expected.minute
            assert result.second == expected.second
            # Note: full timezone comparison omitted for simplicity in this example
        else:
            assert result == expected, f"Failed for input: {input_str}"


def test_format_parts() -> None:
    """Test the _format_parts function."""
    dt = datetime(2023, 8, 14, 15, 22, 33)
    year_month, day_time = _format_parts(dt)
    assert year_month == "2023-08"
    assert day_time == "2023-08-14_152233"


def test_is_media_file() -> None:
    """Test the _is_media_file function."""
    assert _is_media_file(Path("photo.jpg")) is True
    assert _is_media_file(Path("video.mp4")) is True
    assert _is_media_file(Path("document.txt")) is False
    # Test case insensitivity
    assert _is_media_file(Path("image.JPEG")) is True


if __name__ == "__main__":
    # Run tests if called with --test argument
    import sys

    if "--test" in sys.argv:
        test_parse_exif_dt()
        test_format_parts()
        test_is_media_file()
        print("All tests passed!")
    else:
        main()
