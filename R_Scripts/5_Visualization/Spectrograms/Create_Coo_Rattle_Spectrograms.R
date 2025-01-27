##### Create Spectrograms ######

# Create spectrograms for use in methods chapter
library(seewave)
library(tuneR)
library(viridis)


bbcu <- readMP3("./Python_Scripts/BBCU_YBCU_Call_Training/Training_Packet/Cuckoo_Call_Ex/BBCU_Call_Ex/XC241134-BBCURattleCadCo.mp3")

# visualize the coo 
spectro(bbcu,
        ovlp = 90,
        flim = c(0.6,4),
        tlim = c(0,30),
        collevels = seq(-25, 0, 0.5),
        scale = FALSE,
        grid = TRUE,
        #osc = TRUE,
        colbg = "black",
        colgrid = "gray40",
        colaxis = "white",
        collab = "white",
        cexlab = 1.25,
        cexaxis = 1.25,
        #colwave = "white",
        palette=magma) 

# visualize the rattle 
rattle_spectro <- spectro(bbcu,
        ovlp = 90,
        flim = c(0.6,4),
        tlim = c(2.8,3.8),
        collevels = seq(-25, 0, 0.5),
        scale = FALSE,
        grid = TRUE,
        #osc = TRUE,
        colbg = "black",
        colgrid = "gray40",
        colaxis = "white",
        collab = "white",
        cexlab = 1.25,
        cexaxis = 1.25,
        #colwave = "white",
        palette=magma)

### Export for use in manuscript ####
# Single panel figure - Coo
jpeg("./Deliverables/Spectrograms/BBCU_Coo_XC241134.jpg", width=600, height=400)
spectro(bbcu,
        ovlp = 90,
        flim = c(0.6,4),
        tlim = c(5.5,7),
        collevels = seq(-25, 0, 0.5),
        scale = FALSE,
        grid = TRUE,
        #osc = TRUE,
        colbg = "black",
        colgrid = "gray40",
        colaxis = "white",
        collab = "white",
        cexlab = 1.25,
        cexaxis = 1.25,
        palette=magma) 
dev.off()

# Single panel figure - Rattle
jpeg("./Deliverables/Spectrograms/BBCU_Rattle_XC241134.jpg", width=400, height=400)
spectro(bbcu,
        ovlp = 90,
        flim = c(0.6,4),
        tlim = c(2.8,3.8),
        collevels = seq(-25, 0, 0.5),
        scale = FALSE,
        grid = TRUE,
        #osc = TRUE,
        colbg = "black",
        colgrid = "gray40",
        colaxis = "white",
        collab = "white",
        cexlab = 1.25,
        cexaxis = 1.25,
        palette=magma)
dev.off()

# Multipanel figure - black background
jpeg("./Deliverables/Spectrograms/MultiFig_RattleCoo_XC241134.jpg", width=700, height=400)
# Make a multipanel figure
par(mfrow = c(2,1))
par(mar = c(2, 4.1, 3, 1))
spectro(bbcu,
        ovlp = 90,
        flim = c(0.6,4),
        tlim = c(26.5,28),
        collevels = seq(-25, 0, 0.5),
        scale = FALSE,
        grid = TRUE,
        #osc = TRUE,
        colbg = "black",
        colgrid = "gray40",
        colaxis = "white",
        collab = "white",
        tlab = "",
        cexlab = 1.25,
        cexaxis = 1,
        palette=magma) 
# Add "A" label in the top right of the first plot
text(x = par("usr")[2] - 0.1, y = par("usr")[4] - 0.4, "A", col = "white", cex = 1.5)
# set margins
par(mar = c(4, 4.1, 1, 1))
spectro(bbcu,
        ovlp = 90,
        flim = c(0.6,4),
        tlim = c(2.5,4), # old is 2.5 to 4
        collevels = seq(-25, 0, 0.5),
        scale = FALSE,
        grid = TRUE,
        #osc = TRUE,
        colbg = "black",
        colgrid = "gray40",
        colaxis = "white",
        collab = "white",
        cexlab = 1.25,
        cexaxis = 1,
        palette=magma)
# Add "B" label in the top right of the second plot
text(x = par("usr")[2] - 0.1, y = par("usr")[4] - 0.4, "B", col = "white", cex = 1.5)
dev.off()

# Multipanel figure - white background for talk
jpeg("./Deliverables/Spectrograms/TalkMultiFig_RattleCoo_XC241134.jpg", width=700, height=400)
# Make a multipanel figure
par(mfrow = c(2,1))
par(mar = c(2, 4.1, 3, 1))
spectro(bbcu,
        ovlp = 90,
        flim = c(0.6,4),
        tlim = c(26.5,28),
        collevels = seq(-25, 0, 0.5),
        scale = FALSE,
        grid = TRUE,
        colbg = "white",
        colgrid = "gray40",
        colaxis = "black",
        collab = "black",
        tlab = "",
        cexlab = 1.25,
        cexaxis = 1,
        palette=reverse.gray.colors.1) 
# set margins
par(mar = c(4, 4.1, 1, 1))
spectro(bbcu,
        ovlp = 90,
        flim = c(0.6,4),
        tlim = c(2.5,4), # old is 2.5 to 4
        collevels = seq(-25, 0, 0.5),
        scale = FALSE,
        grid = TRUE,
        colbg = "white",
        colgrid = "gray40",
        colaxis = "black",
        collab = "black",
        cexlab = 1.25,
        cexaxis = 1,
        palette=reverse.gray.colors.1)
dev.off()