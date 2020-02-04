% Taproot
% Ian Shipman, Bitnomial
% February 5, 2020

# Slides and code

**https://github.com/GambolingPangolin/talks/taproot**

# Software upgrades

We often install software and software updates without thinking about it.

. . .

::: incremental
- Via an app store (Mobile, MacOS, Windows, etc.)
- Using a standard command like `sudo apt-get upgrade`
:::

. . .

When was the last time you thought carefully about whether or not to update your software?

# Assessing an upgrade

There are many dimensions to consider if you want to be more systematic.

. . .

::: incremental
- Feature set: Will I get new features?  Will I get to keep those I use today?
- Performance: Will the software run faster, use less bandwidth, etc.?
- Usability: Will the sofware be easier to use?
- Software quality: Will the software be less buggy?
- Security: Will the software be more secure?
:::

# Consensus upgrades

Bitcoin users have to agree on some shared global state: the blockchain.  This leads to an additional upgrade assessment dimension.

. . .

::: incremental
- soft fork: non-upgraded users will accept a blockchain accepted by upgraded users
- hard fork: non-upgraded users might not accept a blockchain upgraded users consider valid
:::

. . .

Only has to do with the subset of the functionality that validates the shared state.

# Hard fork example

One noteworthy hard fork of Bitcoin was Bitcoin Cash, launched in August 2017

. . .

::: incremental
- Bitcoin Cash users have to run a modified version of the Bitcoin node software in order to synchronize the Bitcoin Cash UTXO set.
- Upgraded users accept blocks with much more content (in bytes) than the pre-fork limit
- Unpatched Bitcoin users reject many Bitcoin Cash blocks due to their size, among other reasons
:::

# Soft fork example

Pay to Script Hash was introduced by a soft fork in April 2012

. . .

::: incremental
- non upgraded nodes perform the validation
  * compute the hash of the top stack item
  * compare it to the hash defined in the output being spent
- upgraded nodes perform the additional validation steps
  * deserialize the top stack item as a script
  * execute against the remaining stack items
:::

# BIP 2: The BIP Process

Every BIP has a _champion_ who takes it through several stages.

. . .

This can be anyone; could be you!

. . .

::: incremental
1. Float the idea wherever people are talking about Bitcoin.  See if the idea is original and if not, learn the history.
2. If the idea merits further discussion, post a draft BIP to the bitcoin-dev mailing list (following the BIP2 style guide)
3. At some point during the discussion, decide that the BIP is stable enough to submit to the BIPs repository, where Luke Dashjr will assign it a number
4.  Advocate for the idea until it makes it into Bitcoin or gets unambiguously rejected by the community.
:::

# Rolling out an upgrade

Matt Corallo recently codified the spirit of the bitcoin upgrade process into some principles

. . .

::: incremental
1. Avoid activating in the face of significant, reasonable, and directed objection.
2. Avoid activating within a timeframe which does not make high economic node-level-adoption likely.
3. Don't (needlessly) lose hashpower to un-upgraded miners.
4. Use hashpower enforcement to de-risk the upgrade process, wherever possible.
5. Follow the will of the community, irrespective of individuals or unreasoned objection, but without ever overruling any reasonable objection.
:::

# BIP 9 - Version bits with timeout and delay

- Activates based on miner signaling (95% during a retargeting period)
- Activation can fail
- Provides a start time and end time for signaling

# BIP 8 - Version bits with lock-in by height

- Activates early based on miner signaling
- Cannot fail after starting
- Locks in after a "timeout" height

# The new BIPs

Three of the exciting new BIPs under discussion these days are

. . .

::: incremental
- BIP 340: Schnorr signatures - a new digital signature algorithm, signature, and key encodings
- BIP 341: Taproot - a new kind of output
- BIP 342: Tapscript - a new script version, redefining `OP_CHECKSIG` to verify Schnorr sigs among other things
:::

# BIP 340: Schnorr signatures 1

- digital signatures are the basis of coin ownership
- Signature = `(R,s)` where `s * G = H(R|P|m)P + R`
- useful standard potentially beyond bitcoin
- ECDSA is the current signing algorithm
- advantages of Schnorr over ECDSA
  * simpler security model (DLP)
  * batch verification
  * signature aggregation
- applications: MuSig, adaptor signatures

# BIP 340: Schnorr signatures 2

Security model

- Schnorr security provably reduces to the security of the discrete log problem
- Easier to analyze protocols built using Schnorr
- ECDSA has empirical security

# BIP 340: Schnorr signatures 3

Schnorr signatures can be batch verified

```
Keys = P_1, ..., P_n
Messages = m_1, ..., m_n
Signatures = (R_1, s_1), ..., (R_n, s_n)
Randomly sample x_1, ..., x_n
Test: (x_1 * s_1 + ... + x_n * s_n) * G
   == x_1 * [ H(R_1|P_1|m_1) * P_1 + R_1] +
      ... + x_n * [ H(R_n|P_n|m_n) * P_n + R_n]
```

# BIP 340: Schnorr signatures 4

Applications

- `MuSig` - where N parties combine pubkeys to produce a group key and interactively build signatures for it
- Adaptor signatures - where publishing a signature reveals a secret
  * scriptless atomic swaps
  * certain upgrades to lightning payment routing


# BIP 341: Taproot 1

Structure of a taproot output

. . .

::: incremental
- A taproot output has an associated "output key" `Q`
- There is also a commitment `m` to a set of output scripts
- The taproot "internal key" `P` satisifies `H(P|m)*G + P = Q`
:::

. . .

Taproot provides two spending pathways

. . .

::: incremental
- Spend with a `Q` signature
- Spend by proving a script `s` is in the script set and passing evaluation
:::

# BIP 341: Taproot

- two spending pathways: pubkey & script
- scripts are stored in a merkle tree
- fungibility
- script privacy

# BIP 342: Tapscript

- redefine `OP_CHECKSIG` and cousins to use Schnorr
- `OP_SUCCESS` upgrade mechanism

# Upgrade assessment

::: incremental
- Feature set: get new output type and a more powerful signing algorithm, lose no existing features
- Performance: big win with batch verification
- Usability: you decide ...
- Software quality: significant complexity increase with the corresponding potential for bugs
- Security: Schnorr signatures have clearer security and compose better than ECDSA
:::

# ... for users

- added output type makes wallet software more complex
- script branch hiding makes complex spend logic more efficient in general

# ... for miners

# ... for developers

- schnorr makes new protocols possible
- complexity, departure from some established techniques and conventions
