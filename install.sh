#!/bin/sh

# Install uv for venv management
curl -LsSf https://astral.sh/uv/install.sh | sh

# Set path to pick up uv install
export PATH=${PATH}:${HOME}/.local/bin

# Install tools for use with python-mode
uv tool install python-lsp-server
uv tool install ruff
uv tool install python-lsp-server[all]
