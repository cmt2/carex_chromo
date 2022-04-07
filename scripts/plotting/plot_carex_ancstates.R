library(RevGadgets)
res <- processAncStates(path = "~/Desktop/anc_states/anc_states_withoutSid.tre",
                      labels_as_numbers = T )

states <- levels(attributes(res)$state_labels)
colors <- colorRampPalette(colFun(2))(length(states))
names(colors) <- states
plot <- plotAncStatesMAP(t = res, node_color_as = "state", node_size_as = "state_posterior",
                         node_color = colors, tip_labels_offset = 0.5,
                         node_labels_as = "state", node_labels_offset = 0,
                         tip_labels_size = 1, node_labels_size = 1,
                         node_labels_centered = TRUE,tip_states_size = 4,
                         node_size = c(0.5, 10),state_transparency = 1,
                         tip_labels_states_size = 1, cladogenetic = T,
                         tip_labels_states = TRUE, tree_layout = "circular",
                         tip_labels_states_offset = -0.1, tip_states = TRUE)

pdf("~/Desktop/anc_states2.pdf", height = 30, width = 30)

plot

dev.off()
