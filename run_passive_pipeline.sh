#!/usr/bin/env bash

set -u
set -o pipefail

usage() {
  cat <<'EOF'
Usage:
  run_passive_pipeline.sh <SonoChiro|Tadarida> <ouvert|ferme> [indicator options]

Run this command from the directory containing the *_clean.csv files.
Remaining options are passed to 04_indicateurs_passifs_lot.R.

Examples:
  /path/to/bat_activity/run_passive_pipeline.sh SonoChiro ouvert
  /path/to/bat_activity/run_passive_pipeline.sh Tadarida ferme --ville Bordeaux --ephemerides 30 30
EOF
}

if [[ $# -lt 2 ]]; then
  usage
  exit 2
fi

software=$1
environment=$2
shift 2
indicator_args=("$@")

case "$software" in
  SonoChiro|Tadarida) ;;
  *)
    usage
    exit 2
    ;;
esac

case "$environment" in
  ouvert|ferme) ;;
  *)
    usage
    exit 2
    ;;
esac

data_dir=$PWD
script_dir=$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)

if [[ -n "${RSCRIPT:-}" ]]; then
  rscript_bin=$RSCRIPT
else
  rscript_bin=$(command -v Rscript || true)
fi

if [[ -z "$rscript_bin" ]]; then
  printf '%s\n' "ERROR: Rscript was not found. Install R or add Rscript to PATH." >&2
  exit 1
fi

if [[ ! -f "$script_dir/batactivity/R/standardize.R" ]]; then
  printf '%s\n' "ERROR: The shell script must remain at the repository root." >&2
  exit 1
fi

printf 'Data directory: %s\n' "$data_dir"
printf 'Rscript: %s\n\n' "$rscript_bin"

cd "$script_dir" || exit 1

printf '[1/3] Standardizing %s CSV files...\n' "$software"
if ! "$rscript_bin" scripts/03_standardiser_lot.R "$software" --dossier "$data_dir"; then
  printf '%s\n' "ERROR: The pipeline stopped during standardization." >&2
  exit 1
fi

printf '\n[2/3] Calculating passive indicators...\n'
if ! "$rscript_bin" scripts/04_indicateurs_passifs_lot.R "${indicator_args[@]}" --dossier "$data_dir"; then
  printf '%s\n' "ERROR: The pipeline stopped while calculating passive indicators." >&2
  exit 1
fi

printf '\n[3/3] Applying detectability correction for a %s environment...\n' "$environment"
if ! "$rscript_bin" scripts/05_correction_detectabilite.R --milieu "$environment" --dossier "$data_dir"; then
  printf '%s\n' "ERROR: The pipeline stopped while applying detectability correction." >&2
  exit 1
fi

printf '\n%s\n' "Pipeline completed successfully."
