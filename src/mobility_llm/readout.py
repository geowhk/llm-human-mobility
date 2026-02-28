from __future__ import annotations

from typing import Any

import torch
from torch import nn


class LinearReadout(nn.Module):
    """A simple linear readout head."""

    def __init__(self, in_dim: int):
        super().__init__()
        self.linear = nn.Linear(in_dim, 1)

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        return self.linear(x).squeeze(-1)


def train_readout(
    X_train,
    y_train,
    X_val,
    y_val,
    epochs: int = 50,
    lr: float = 1e-3,
    weight_decay: float = 0.0,
    device: str = "cpu",
) -> LinearReadout:
    """Train readout head on log1p(flow)."""
    X_train_t = torch.as_tensor(X_train, dtype=torch.float32, device=device)
    y_train_t = torch.as_tensor(y_train, dtype=torch.float32, device=device)
    X_val_t = torch.as_tensor(X_val, dtype=torch.float32, device=device)
    y_val_t = torch.as_tensor(y_val, dtype=torch.float32, device=device)

    y_train_log = torch.log1p(torch.clamp(y_train_t, min=0.0))
    y_val_log = torch.log1p(torch.clamp(y_val_t, min=0.0))

    head = LinearReadout(X_train_t.shape[1]).to(device)
    optimizer = torch.optim.AdamW(head.parameters(), lr=lr, weight_decay=weight_decay)
    loss_fn = nn.MSELoss()

    final_train_loss = None
    final_val_loss = None
    for _ in range(int(epochs)):
        head.train()
        optimizer.zero_grad()
        pred_log = head(X_train_t)
        loss = loss_fn(pred_log, y_train_log)
        loss.backward()
        optimizer.step()
        final_train_loss = float(loss.detach().cpu())

        head.eval()
        with torch.no_grad():
            val_pred_log = head(X_val_t)
            val_loss = loss_fn(val_pred_log, y_val_log)
            final_val_loss = float(val_loss.detach().cpu())

    head.train_history = {
        "final_train_loss": final_train_loss,
        "final_val_loss": final_val_loss,
    }
    return head


def predict_readout(head: LinearReadout, X, device: str = "cpu"):
    """Predict on raw flow scale from log1p-trained head."""
    X_t = torch.as_tensor(X, dtype=torch.float32, device=device)
    head.eval()
    with torch.no_grad():
        pred_log = head(X_t)
        pred = torch.expm1(pred_log)
        pred = torch.clamp(pred, min=0.0)
    return pred.detach().cpu().numpy()
