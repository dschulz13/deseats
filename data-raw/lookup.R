### lookup tables for smoots
p3_lookup <- list(function(u) {3 / 8 * (3 - 5 * u^2)},
                  function(u) {15 / 32 * (3 - 10 * u^2 + 7 * u^4)},
                  function(u) {105 / 64 * (1 - 5 * u^2 + 7 * u^4 - 3 * u^6)},
                  function(u) {315 / 512 * (3 - 20 * u^2 + 42 * u^4 - 36 *
                               u^6 + 11 * u^8)}
               )

lookup_3kerns <- matrix(list(function(u) {-3 / 2 * u},
                             function(u) {15 / 4 * (u - u ** 3)},
                             function(u) {105 / 16 * (-u + 2 * u ** 3 - u ** 5)},
                             function(u) {315 / 32 * (-u + 3 * u ** 3 - 3 *
                                          u ** 5 + u ** 7)},
                             function(u) {15 / 4 * (1 - 3 * u ** 2)},
                             function(u) {105 / 16 * (-1 + 6 * u ** 2 - 5 *
                                          u ** 4)},
                             function(u) {315 / 32 * (-1 + 9 * u ** 2 - 15 *
                                          u ** 4 + 7 * u ** 6)},
                             function(u) {3465 / 256 * (-1 + 12 * u ** 2 - 30 *
                                          u ** 4 + 28 * u ** 6 - 9 * u ** 8)}),
                        ncol = 2)

InfR_lookup <- matrix(c(function(w) {w^(5 / 7)}, function(w) {w^(9 / 11)},
                        function(w) {w^(5 / 9)}, function(w) {w^(9 / 13)},
                        function(w) {w^(1 / 2)}, function(w) {w^(1 / 2)}),
                        ncol = 3, dimnames = list(c("1", "3"), c("Opt",
                        "Nai", "Var")))

InfR2_lookup <- matrix(c(function(w) {w^(7 / 9)}, function(w) {w^(9 / 11)},
                         function(w) {w^(7 / 11)}, function(w) {w^(9 / 13)},
                         function(w) {w^(1 / 2)}, function(w) {w^(1 / 2)}),
                       ncol = 3, dimnames = list(NULL, c("Opt", "Nai", "Var")))

bvc_lookup <- matrix(c(function(w) {1.3195 * w}, function(w) {1.2599 * w},
                       function(w) {1.4310 * w}, function(w) {1.2913 * w},
                       function(w) {1.4541 * w}, function(w) {1.3006 * w},
                       function(w) {1.4640 * w}, function(w) {1.3052 * w},
                       function(w) {1.4888 * w},
                       NA), nrow = 2, dimnames = list(c("1", "3")))

expon1 <- c("1" = 1 / 5, "3" = 1 / 9)

expon2 <- c("1" = -5 / 7, "3" = -9 / 11)

exponDer1 <- c(1 / 7, 1 / 9)

exponDer2 <- c(-7 / 9, -9 / 11)

lookup_alg <- matrix(c("NP", "Opt", "Y", "NP", "Nai", "Y", "NP", "Nai", "N", "AR", "Nai",
                       "N", "NP", "Opt", "N", "AR", "Opt", "N", "MA", "Opt", "N", "MA", "Nai",
                       "N", "ARMA", "Opt", "N", "ARMA", "Nai", "N"), ncol = 10,
                     dimnames = list(c("Mcf", "InfR", "bvc"), c("A", "B", "N", "NA",
                                                                "O", "OA", "OM", "NM", "OAM", "NAM")))


lookup <- list(lookup_alg = lookup_alg, InfR_lookup = InfR_lookup,
               bvc_lookup = bvc_lookup, p3_lookup = p3_lookup,
               lookup_3kerns = lookup_3kerns, InfR2_lookup = InfR2_lookup,
               expon1 = expon1, expon2 = expon2, exponDer1 = exponDer1,
               exponDer2 = exponDer2)
