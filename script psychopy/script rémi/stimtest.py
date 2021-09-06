

from psychopy import visual, monitors,core
#from psychopy.hardware import keyboard
#kb = keyboard.Keyboard()
import keyboard


mon1 = monitors.Monitor('testMonitor')
#calibName = mon1.setCurrent(-1)
mon1.setDistance(70)
mon1.setWidth(53)
mon1.setSizePix([1920, 1080])
mon1.saveMon()

'''
def ask(text='', color='white'):
    """
    Display instruction.
    key = space
    """
    while True:
        theseKeys = kb.getKeys()
        if 'escape' in theseKeys:
            core.quit()
        if len(theseKeys):
            break

        instr = visual.TextStim(win,
                                ori=0,
                                height=0.6,
                                wrapWidth=35,
                                )
        instr.color = color
        instr.text = text
        instr.draw()
        win.flip()
'''
win = visual.Window(size=[1920, 1080], monitor=mon1, allowGUI=False, fullscr=False, units='deg', color=[128, 128, 128],
                    colorSpace='rgb255')


gabor1 = visual.GratingStim(win, tex="sin",
                           mask="gauss", texRes=256, pos=[0, 5],
                           size=2, sf=[2, 0], ori=90, name='gabor')

gabor1.contrast = 0.1

circle1 = visual.NoiseStim(win=win, units="deg", noiseType = "Uniform", size=(1.8,1.8), mask="circle",
                            contrast = 0.25,noiseElementSize=0.01, pos=[0, 5],opacity=0.3)
                            
gabor2 = visual.GratingStim(win, tex="sin",
                           mask="gauss", texRes=256, pos=[0, -5],
                           size=2, sf=[2, 0], ori=0, name='gabor')

gabor2.contrast = 0.1

circle2 = visual.NoiseStim(win=win, units="deg", noiseType = "Uniform", size=(1.8,1.8), mask="circle",
                            contrast = 0.25,noiseElementSize=0.01, pos=[0, -5],opacity=0.3)
                            

#gamma = mon1.getGamma()
#print(gamma)
 # 2 frames with 60HZ = 33.32 msec
instr_conf2 = "Vous utiliserez pour cela les deux pieds et les deux mains.\n\n\
Les chiffres vont être présenté aléatoirement aux quatres coins,\n\n\n\
Chaque coin correspond à un de vos membres (main gauche, main droite, pied gauche, pied droit)\n\n\
Comme ceci : "

#ask(text=instr_conf2)
gabor1.draw()
circle1.draw()
gabor2.draw()
circle2.draw()
win.flip()
#win.getMovieFrame(buffer='front')
#win.saveMovieFrames('CircleConf.png')
keyboard.wait('esc')
core.quit()
win.close()
"""

instr_1 = "Bienvenue ! \n\n\
Placez votre anulaire, votre  majeur et votre index \n\n\
de la main gauche sur les touches \n\n\
portant une gommette de couleur  \n\n\n\
Vous utiliserez votre main droite pour \n\n\
les touches avec des gommettes blanches"

instr = visual.TextStim(win,
                        ori=0,
                        height=0.6,
                        font='Consolas',
                        wrapWidth=35,
                        color='white',
                        text=instr_1
                        )

instr.draw()
win.flip()
keyboard.wait('esc')
win.close()


import numpy as np
ordre = 1
color_array = np.array([1, 2])  # color: 1=rouge, 2=bleu
ori_array = np.array([1, 2])  # orientation: 1=horizontal, 2=vertical, 0=none
diff_array = np.array([1, 2])
possible_stim = [[color, ori, diff] for color in color_array for ori in ori_array for diff in diff_array]
n_block_key = 5  ## number of blocks in main condition
n_block_feet = 5  ## number of blocks in feet condition
n_trial = 64  ## number of trials/block

primeratio = 0.375  # proportion of gabor trials
total_trials = int((n_block_key + n_block_feet) * n_trial)  # total number of trials
gabor_trials = int(primeratio * total_trials)

no_gab_array = [(1, 0, 0), (2, 0, 0)]
no_gab_array = np.tile(no_gab_array, 1).reshape(-1, 3)  # possible stims for no_gabor

##  sequence
seq = np.empty((0, 2))
for i in range(10):  # 2 blocks with 24 gabor trials and 40 no_gabor trials randomized (20*64=1280)
    block = np.tile(possible_stim, int(3)).reshape(-1, 3)  # 16 gabor trials added to sequence
    no_gab_block = np.tile(no_gab_array, int(20)).reshape(-1, 3)  # 32 no_gabor trials created
    block = np.append(no_gab_block, block).reshape(-1, 3)  # add no_gabor trials to sequence
    np.random.shuffle(block)  # randomize
    seq = np.append(block, seq).reshape(-1, 3)  # whole trial sequence


if ordre == 1:
    hand_cond = range(1, n_block_key * n_trial + 1)
    feet_cond = range(n_block_key * n_trial + 1, (n_block_key + n_block_feet) * n_trial + 1)
else:
    feet_cond = range(1, n_block_feet * n_trial + 1)
    hand_cond = range(n_block_feet * n_trial + 1, (n_block_key + n_block_feet) * n_trial + 1)
# hand_cond: trials with hand answer
# feet_cond: trials with feet answer

# gabor 2 difficulties
diff_cond = np.array([1.15, 1.30])
diff_seq = np.tile(diff_cond, int(gabor_trials / len(diff_cond))).reshape(-1, 1)
np.random.shuffle(diff_seq)
"""