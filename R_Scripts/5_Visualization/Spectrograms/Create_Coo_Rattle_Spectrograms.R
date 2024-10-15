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