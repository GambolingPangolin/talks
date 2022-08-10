% Drivechain
% Chicago Bitdevs
% August 10, 2022

# The problem

##

Transfer bitcoin into some cool application or from the application back to the
main chain

## But ...

::: incremental

- non-users should not need to validate the application logic
- this includes miners (as much as possible)

:::

##

The core technical problem is what to do with coins when they are "in the other
application"

# BIP 300

## Main ideas

::: incremental

- Miners declare drivechains
- Each active drivechain always has a special UTXO with all of its funds
- Anybody can add funds to a drivechain at any time
- Long proposal & ack process for withdrawing funds from a drivechain

:::

## Create a chain

::: incremental

- Propose a drivechain using a special `OP_RETURN` message in the coinbase
- Subsequent blocks can optionally ack a drivechain proposal
- Must get acks in 90% of the 2016 blocks following the proposal
- Like BIP 9

:::

## Special UTXOS and TXs

::: incremental

- Creation marks special (anyone can spend) UTXO as the drivechain funds
- TXs spending the current special UTXO must create the next in position 0

:::

## Add funds

::: incremental

- Anyone, at any time
- Spend the current special UTXO, leave more bitcoin in the (properly structure)
  output 0

:::

## Withdraw funds

::: incremental

- Intent to withdraw via an `OP_RETURN` message with a commitment to the spend
- Each block can ack, nack, or abstain
- Spends that decrease funds only valid if their proposal gets 13150 acks (less
  nacks) in the 26299 blocks after proposal

:::

##

Withdraw proposals are public and must get at least ~ 90 days worth of blocks of
acks

# BIP 301

##

Any application that uses bitcoin will need to regularly commit to its state

. . .

Drivechain applications commit via paying miners to include a commitment in the
coinbase tx

##

Miners do not need to validate the state, they just pocket the money and adjust
the coinbase tx

##

Many sidechain miners compete to have their choice of block included

. . .

Need a way for only one of these txs to be valid at a time

##

::: incremental

- A coinbase tx can include at most one `OP_RETURN` message committing to a
  drivechain block per drivechain
- New tx version with fields for the drivechain index and commitment and the
  prev main block hash

:::

##

::: incremental

- New rule: if `BLOCK` has `TX` with commitment `H` for drivechain `DC` then there has
  to be an `OP_RETURN` message for drivechain `DC` with commitment `H`
- New rule: if `TX` includes the prev block hash field, then the prev block must
  actually have that hash

:::

# Alternatives

##

::: incremental

- Liquid & minimint: Coins in use in the application are held in a multisig wallet
- ZK rollups: Use a proof system to control spends of sidechain funds

:::
