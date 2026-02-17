$ErrorActionPreference = "Stop"

Write-Host "[1/5] Creating Python virtual environment (.venv)..."
python -m venv .venv

Write-Host "[2/5] Activating virtual environment..."
. .\.venv\Scripts\Activate.ps1

Write-Host "[3/5] Upgrading pip..."
python -m pip install --upgrade pip

Write-Host "[4/5] Installing dependencies from requirements.txt..."
pip install -r requirements.txt

Write-Host "[5/5] Environment setup complete."
