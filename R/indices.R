.a       <- 1
.b       <- 2
.c       <- 3
.d       <- 4
.e       <- 5
.Ai      <- 6
.Aj      <- 7
.aii     <- 8   # portée Matérn variable i
.ajj     <- 9   # portée Matérn variable j
.nuii    <- 10  # lissage Matérn variable i
.nujj    <- 11  # lissage Matérn variable j
.rho1ij  <- 12  # correlation temporelle pure 
.r2ii    <- 13  # décroissance exp. spatiotemporelle variable i
.r2jj    <- 14  # décroissance exp. spatiotemporelle variable j
.r1ii    <- 15  # décroissance exp. temporelle variable i
.r1jj    <- 16

# ============================================================
# Bornes sup et inf
# ============================================================
.lower <- c(
  1e-6,    # a > 0
  1e-6,    # 0 < b <= 1
  0,       # 0 <= c <= 1
  1e-6,    # d > 0
  1e-6,    # 0 < e <= 1
  0,       # 0 <= Ai < 1
  0,       # 0 <= Aj < 1
  1e-6,    # aii > 0 (min = 1/taille domaine) A changer ensuite pour paufiner
  1e-6,    # ajj > 0
  0.25,    # nuii >= 0.25 (Matérn valide)
  0.25,    # nujj >= 0.25
  -1,      # rho1ij >= -1 (cross-pair) — remplacé par 0 pour self-pair A rechecker pour les cas
  1e-6,    # r2ii > 0
  1e-6,    # r2jj > 0
  1e-6,    # r1ii > 0
  1e-6     # r1jj > 0
)

.upper <- c(
  Inf,        # a
  1,          # b <= 1
  1,          # c <= 1
  Inf,        # d
  1,          # e <= 1
  1 - 1e-6,  # Ai < 1
  1 - 1e-6,  # Aj < 1
  Inf,        # aii (max = 1/taille pixel) A rechecker
  Inf,        # ajj
  3,          # nuii <= 3 (suffisant physiquement)
  3,          # nujj <= 3
  1 - 1e-6,  # rho1ij <= 1 (self et cross)
  Inf,        # r2ii
  Inf,        # r2jj
  Inf,        # r1ii
  Inf         # r1jj
)

# ============================================================
# Valeurs initiales
# ============================================================
.init <- c(
  0.5,     # a = 1 
  0.5,   # b = 0.5 (milieu de (0,1])
  0.5,   # c = 0.5 (milieu de [0,1])
  0.5,     # d = 1 
  0.5,   # e = 0.5 (milieu de (0,1])
  0.1,   # Ai
  0.1,   # Aj
  1,     # aii (depuis par_s)
  1,     # ajj (depuis par_s)
  1.1,   # nuii (depuis par_s)
  1.1,   # nujj (depuis par_s)
  0.9999, # rho1ij self-pair
  1,     # r2ii
  1,     # r2jj
  1,     # r1ii
  1      # r1jj
)