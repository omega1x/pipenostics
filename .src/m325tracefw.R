# Start with the test bench:
nx <- pipenostics::m325testbench

nx$sender <- paste0("N", nx$sender)
nx$acceptor <- paste0("N", nx$acceptor)

# Alter units:
nx$d <- 1e3*nx$d  # convert [m] to [mm]


# Perform backward tracing:
bw_report <- do.call("m325tracebw", c(as.list(nx), verbose = FALSE))

# Embed the traced values to the test bench for forward tracing:
starting_node <- "N12"
regime_param  <- c("temperature", "pressure", "consumption")
nx[12, regime_param] <-
  subset(bw_report, node == starting_node & aggregation == "median", regime_param)
rm(regime_param, starting_node)

with(nx, {
# Perform forward tracing ----

# Compute discharges ----
discharge <- setNames(1 - d^2/tapply(d^2, sender, sum)[sender], acceptor)

# List search paths ----
paths <- flowls(sender, acceptor)

# Trace searched paths ----
for (i in seq_along(paths)) {
print(i)
foo <-
  m325traceline(
    temperature[paths[[i]]][[1]],
    pressure[paths[[i]]][[1]],
    consumption[paths[[i]]][[1]],
    discharge[paths[[i]]][-1],
    d[paths[[i]]][-1],
    len[paths[[i]]][-1],
    year[paths[[i]]][-1],
    insulation[paths[[i]]][-1],
    laying[paths[[i]]][-1],
    beta[paths[[i]]][-1],
    exp5k[paths[[i]]][-1],
    roughness[paths[[i]]][-1],
    inlet[paths[[i]]][-1],
    outlet[paths[[i]]][-1],
    elev_tol = .5,
    absg = FALSE
  )
}

})