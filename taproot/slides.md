% Taproot
% Ian Shipman, Bitnomial
% February 5, 2020

# Slides and code

**https://github.com/GambolingPangolin/talks/taproot**

# Upgrading bitcoin

# Soft and hard forks

# Activation pathways

# BIP 340: Schnorr signatures

- digital signatures allow coin ownership
- useful standard potentially beyond bitcoin
- ECDSA is the current signing algorithm
- advantages of Schnorr over ECDSA
  * simpler security model (DLP)
  * batch verification
- applications: MuSig, adaptor signatures

# BIP 341: Taproot

- two spending pathways: pubkey & script
- scripts are stored in a merkle tree
- fungibility
- script privacy

# BIP 342: Tapscript

- redefine `OP_CHECKSIG` and cousins to use Schnorr
- `OP_SUCCESS` upgrade mechanism

# Costs and benefits

# ... for users
# ... for miners
# ... for developers
