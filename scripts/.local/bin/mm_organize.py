#!/usr/bin/env python3
"""
Media file organization script.

Organizes photos and videos from a source directory into a destination directory
by date, using EXIF metadata when available.
"""

import shutil
import subprocess
from datetime import UTC, datetime
from pathlib import Path

import click


def get_exif_date(file_path: Path) -> tuple[str | None, str | None]:
    """Extract date from EXIF data using exiftool."""
    # Use full path to exiftool for security
    exiftool_path = shutil.which("exiftool")
    if not exiftool_path:
        return None, None

    try:
        # Safe subprocess call with trusted input
        result = subprocess.run(  # noqa: S603
            [exiftool_path, "-DateTimeOriginal", "-d", "%Y-%m", str(file_path)],
            capture_output=True,
            text=True,
            check=True,
            timeout=30,
        )

        if result.stdout:
            for line in result.stdout.split("\n"):
                if "Date/Time Original" in line:
                    year_month = line.split(":", 1)[1].strip()

                    # Get day and time for filename
                    day_time_result = subprocess.run(  # noqa: S603
                        [
                            exiftool_path,
                            "-DateTimeOriginal",
                            "-d",
                            "%d_%H%M%S",
                            str(file_path),
                        ],
                        capture_output=True,
                        text=True,
                        check=True,
                        timeout=30,
                    )

                    if day_time_result.stdout:
                        for dt_line in day_time_result.stdout.split("\n"):
                            if "Date/Time Original" in dt_line:
                                day_time = dt_line.split(":", 1)[1].strip()
                                return year_month, day_time
    except (
        subprocess.CalledProcessError,
        FileNotFoundError,
        subprocess.TimeoutExpired,
    ):
        pass

    # Fallback to file modification time
    try:
        mtime = file_path.stat().st_mtime
    except OSError:
        return None, None
    else:
        dt = datetime.fromtimestamp(mtime, tz=UTC)
        year_month = dt.strftime("%Y-%m")
        day_time = dt.strftime("%d_%H%M%S")
        return year_month, day_time


def organize_files(
    source_path: Path, destination_path: Path, *, dry_run: bool = False
) -> tuple[int, int]:
    """Organize media files from the source directory to the destination.

    Parameters
    ----------
    source_path : Path
        Source directory containing media files.
    destination_path : Path
        Destination directory for organized files.
    dry_run : bool, optional
        If True, only show what would be done without moving files.

    Returns
    -------
    tuple[int, int]
        A tuple ``(processed_count, skipped_count)`` indicating how many
        files were organized and how many were skipped.
    """
    # Create destination directories
    photos_dir = destination_path / "Photos"
    movies_dir = destination_path / "Movies"

    if not dry_run:
        photos_dir.mkdir(parents=True, exist_ok=True)
        movies_dir.mkdir(parents=True, exist_ok=True)

    # File type mappings
    photo_extensions = {
        ".jpg",
        ".jpeg",
        ".png",
        ".gif",
        ".bmp",
        ".tiff",
        ".heic",
        ".webp",
    }
    video_extensions = {
        ".mp4",
        ".mov",
        ".avi",
        ".mkv",
        ".m4v",
        ".3gp",
        ".wmv",
        ".flv",
        ".webm",
    }

    processed_count = 0
    skipped_count = 0

    for file_path in source_path.rglob("*"):
        if not file_path.is_file():
            continue

        click.echo(f"Processing: {file_path}")

        # Get file extension (lowercase)
        extension = file_path.suffix.lower()

        # Skip unsupported file types
        if extension not in photo_extensions and extension not in video_extensions:
            click.echo(f"  WARNING: Unsupported file type {extension}, skipping")
            skipped_count += 1
            continue

        # Get date information
        year_month, day_time = get_exif_date(file_path)
        if not year_month or not day_time:
            click.echo(f"  WARNING: Could not get date for {file_path}, skipping")
            skipped_count += 1
            continue

        # Determine target directory
        if extension in photo_extensions:
            target_dir = photos_dir / year_month
        else:
            target_dir = movies_dir / year_month

        if not dry_run:
            target_dir.mkdir(exist_ok=True)

        # Create new filename
        new_name = f"{day_time}{extension}"
        target_path = target_dir / new_name

        # Ensure unique filename
        counter = 1
        base_name = day_time
        while target_path.exists():
            new_name = f"{base_name}_{counter}{extension}"
            target_path = target_dir / new_name
            counter += 1

        if dry_run:
            click.echo(f"  WOULD COPY to: {target_path}")
        else:
            click.echo(f"  Copying to: {target_path}")
            shutil.copy2(file_path, target_path)

        processed_count += 1

    return processed_count, skipped_count


@click.command()
@click.argument(
    "source",
    type=click.Path(
        exists=True, file_okay=False, dir_okay=True, path_type=Path, readable=True
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
def main(
    source: Path,
    destination: Path,
    *,
    dry_run: bool,  # Changed from positional to keyword-only
) -> None:
    """
    Organize media files from SOURCE directory into DESTINATION by date.

    SOURCE: Directory containing your source media files.
            [default: ~/vigolana-MM/PHOTOs/PICTURE-fino09/PIETRO/selAGO09]

    DESTINATION: Target directory where organized files will be copied.
                 [default: ~/FamilyMedia]
    """
    click.echo(f"Source: {source}")
    click.echo(f"Destination: {destination}")
    click.echo(f"Dry run: {dry_run}")

    if not source.exists():
        error_msg = f"Source directory does not exist: {source}"
        raise click.ClickException(error_msg)

    processed_count, skipped_count = organize_files(
        source, destination, dry_run=dry_run
    )

    click.echo("\nProcessing complete!")
    click.echo(f"Files processed: {processed_count}")
    click.echo(f"Files skipped: {skipped_count}")
    if dry_run:
        click.echo("This was a dry run - no files were actually copied.")


if __name__ == "__main__":
    main()
