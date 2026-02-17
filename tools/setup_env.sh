#!/bin/sh
set -e

echo "[1/5] Creating Python virtual environment (.venv)..."
python3 -m venv .venv

echo "[2/5] Activating virtual environment..."
# shellcheck disable=SC1091
. .venv/bin/activate

echo "[3/5] Upgrading pip..."
python -m pip install --upgrade pip

echo "[4/5] Installing dependencies from requirements.txt..."
pip install -r requirements.txt

echo "[5/5] Environment setup complete."
