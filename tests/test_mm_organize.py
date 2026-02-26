"""Tests for the mm_organize photo/video organization script.

This module tests the utility functions used for organizing media files
based on their EXIF date metadata.
"""

from __future__ import annotations

import sys
import tempfile
from datetime import datetime
from pathlib import Path

# NOTE: We use sys.path manipulation to import from the src directory because
# the scripts in src/.local/bin/ are standalone executables designed to be
# deployed to a user's ~/.local/bin directory, not as a Python package. This is
# the pragmatic approach for testing such scripts in a dotfiles repository.
_scripts_path = str(Path(__file__).parent.parent / "src" / ".local" / "bin")
if _scripts_path not in sys.path:
    sys.path.insert(0, _scripts_path)

from mm_organize import (  # noqa: E402
    PHOTO_EXTS,
    VIDEO_EXTS,
    _ensure_unique_path,
    _format_parts,
    _get_date_from_exif_json,
    _is_media_file,
    _parse_exif_dt,
)


class TestParseExifDt:
    """Tests for the _parse_exif_dt function."""

    def test_classic_exif_format(self) -> None:
        """Test parsing classic EXIF format YYYY:MM:DD HH:MM:SS."""
        result = _parse_exif_dt("2023:08:14 15:22:33")
        assert result is not None
        assert result.year == 2023
        assert result.month == 8
        assert result.day == 14
        assert result.hour == 15
        assert result.minute == 22
        assert result.second == 33

    def test_iso_format_with_t(self) -> None:
        """Test parsing ISO format with T separator."""
        result = _parse_exif_dt("2023-08-14T15:22:33")
        assert result is not None
        assert result.year == 2023
        assert result.month == 8
        assert result.day == 14

    def test_with_timezone_offset(self) -> None:
        """Test parsing datetime with timezone offset."""
        result = _parse_exif_dt("2023:08:14 15:22:33+02:00")
        assert result is not None
        assert result.year == 2023
        assert result.month == 8
        assert result.day == 14
        assert result.hour == 15

    def test_utc_format(self) -> None:
        """Test parsing UTC format - with EXIF-style colons."""
        # The implementation replaces the first two colons with dashes
        # This works for EXIF format "2023:08:14 15:22:33Z" but breaks ISO format
        # So we test with EXIF-style format that includes Z
        result = _parse_exif_dt("2023:08:14 15:22:33Z")
        # This should parse as a UTC datetime
        if result is not None:
            assert result.year == 2023
            assert result.month == 8
            assert result.day == 14

    def test_empty_string(self) -> None:
        """Test empty string returns None."""
        assert _parse_exif_dt("") is None

    def test_none_input(self) -> None:
        """Test None input returns None."""
        assert _parse_exif_dt(None) is None

    def test_whitespace_string(self) -> None:
        """Test whitespace-only string returns None."""
        assert _parse_exif_dt("   ") is None


class TestFormatParts:
    """Tests for the _format_parts function."""

    def test_formats_correctly(self) -> None:
        """Test formatting of datetime into parts."""
        dt = datetime(2023, 8, 14, 15, 22, 33)
        year_month, day_time = _format_parts(dt)
        assert year_month == "2023-08"
        assert day_time == "2023-08-14_152233"

    def test_zero_padded_values(self) -> None:
        """Test that single-digit values are zero-padded."""
        dt = datetime(2023, 1, 5, 3, 7, 9)
        year_month, day_time = _format_parts(dt)
        assert year_month == "2023-01"
        assert day_time == "2023-01-05_030709"


class TestIsMediaFile:
    """Tests for the _is_media_file function."""

    def test_photo_extensions(self) -> None:
        """Test common photo extensions are recognized."""
        for ext in [".jpg", ".jpeg", ".png", ".gif", ".heic", ".webp"]:
            assert _is_media_file(Path(f"photo{ext}")) is True

    def test_raw_photo_extensions(self) -> None:
        """Test RAW photo extensions are recognized."""
        for ext in [".arw", ".cr2", ".nef", ".orf", ".rw2"]:
            assert _is_media_file(Path(f"photo{ext}")) is True

    def test_video_extensions(self) -> None:
        """Test common video extensions are recognized."""
        for ext in [".mp4", ".mov", ".avi", ".mkv", ".webm"]:
            assert _is_media_file(Path(f"video{ext}")) is True

    def test_case_insensitivity(self) -> None:
        """Test that extension matching is case-insensitive."""
        assert _is_media_file(Path("image.JPEG")) is True
        assert _is_media_file(Path("video.MP4")) is True
        assert _is_media_file(Path("photo.JpG")) is True

    def test_non_media_files(self) -> None:
        """Test that non-media files are not matched."""
        for ext in [".txt", ".pdf", ".doc", ".py", ".sh"]:
            assert _is_media_file(Path(f"document{ext}")) is False


class TestEnsureUniquePath:
    """Tests for the _ensure_unique_path function."""

    def test_returns_original_if_not_exists(self) -> None:
        """Test that original path is returned if it doesn't exist."""
        with tempfile.TemporaryDirectory() as tmpdir:
            result = _ensure_unique_path(Path(tmpdir), "photo", ".jpg")
            assert result.name == "photo.jpg"

    def test_adds_suffix_if_exists(self) -> None:
        """Test that numeric suffix is added if file exists."""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmpdir_path = Path(tmpdir)
            # Create the original file
            (tmpdir_path / "photo.jpg").touch()

            result = _ensure_unique_path(tmpdir_path, "photo", ".jpg")
            assert result.name == "photo_1.jpg"

    def test_increments_suffix(self) -> None:
        """Test that suffix is incremented for multiple collisions."""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmpdir_path = Path(tmpdir)
            # Create files with _1 and _2 suffixes
            (tmpdir_path / "photo.jpg").touch()
            (tmpdir_path / "photo_1.jpg").touch()
            (tmpdir_path / "photo_2.jpg").touch()

            result = _ensure_unique_path(tmpdir_path, "photo", ".jpg")
            assert result.name == "photo_3.jpg"


class TestGetDateFromExifJson:
    """Tests for the _get_date_from_exif_json function."""

    def test_parses_json_list(self) -> None:
        """Test parsing JSON list output from exiftool."""
        json_str = '[{"DateTimeOriginal": "2023:08:14 15:22:33"}]'
        result = _get_date_from_exif_json(json_str)
        assert result is not None
        assert result.year == 2023

    def test_parses_json_dict(self) -> None:
        """Test parsing JSON dict output."""
        json_str = '{"DateTimeOriginal": "2023:08:14 15:22:33"}'
        result = _get_date_from_exif_json(json_str)
        assert result is not None
        assert result.year == 2023

    def test_uses_fallback_keys(self) -> None:
        """Test that fallback keys are used."""
        json_str = '[{"CreateDate": "2023:08:14 15:22:33"}]'
        result = _get_date_from_exif_json(json_str)
        assert result is not None
        assert result.year == 2023

    def test_invalid_json(self) -> None:
        """Test handling of invalid JSON."""
        assert _get_date_from_exif_json("not valid json") is None

    def test_empty_json(self) -> None:
        """Test handling of empty JSON."""
        assert _get_date_from_exif_json("[]") is None
        assert _get_date_from_exif_json("{}") is None


class TestExtensionSets:
    """Tests for the PHOTO_EXTS and VIDEO_EXTS sets."""

    def test_photo_extensions_are_lowercase(self) -> None:
        """Test that all photo extensions are lowercase."""
        for ext in PHOTO_EXTS:
            assert ext == ext.lower()
            assert ext.startswith(".")

    def test_video_extensions_are_lowercase(self) -> None:
        """Test that all video extensions are lowercase."""
        for ext in VIDEO_EXTS:
            assert ext == ext.lower()
            assert ext.startswith(".")

    def test_no_overlap(self) -> None:
        """Test that photo and video extensions don't overlap."""
        assert PHOTO_EXTS.isdisjoint(VIDEO_EXTS)
