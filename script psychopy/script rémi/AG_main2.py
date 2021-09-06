#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on 02.03.2020

@author: remi sanchez

for triggers, run InstallDriver from the inpout32 folder and then copy inpoutx64.dll
in the same folder as the python scripts and in windows/system32

PILOT EXPERIMENT : MAIN
"""

## * libraries
import os  # handy system and path functions
from psychopy import core, visual, gui, data, monitors, parallel, logging
from psychopy.hardware import keyboard
import numpy as np
import settings  # script file with experiment parameters 
from datetime import datetime
import time
import pandas as pd 
# import pandas as pd

## * Functions
def ask(text='', color='white'):
    """
    Display instruction.
    key = space
    """
    kb.clearEvents()
    while True:
        theseKeys = kb.getKeys()
        if 'escape' in theseKeys:
            win.close()
            core.quit()
        if len(theseKeys):
            break

        instr = visual.TextStim(win,
                                ori=0,
                                height=size_instruc,
                                font=police,
                                wrapWidth=35,
                                )
        instr.color = color
        instr.text = text
        instr.draw()
        win.flip()
    core.wait(instr_time)


def current_milli_time():
    return round(time.time() * 1000)

## * informations & files

## participant and expe info infos
expInfo = {'participant': '', 'response condition': ['1', '2', '3', '4', '5', '6','7','8'], 'effector': ['hand1','hand2','feet1','feet2'] }
expName = 'Motor'
expInfo['date'] = data.getDateStr()
expInfo['expName'] = expName
dlg = gui.DlgFromDict(dictionary=expInfo, title=expName)
if dlg.OK == False:
    core.quit()  # user pressed cancel

    # left/right response balance (90° = horizontal; 0° = vertical)
if expInfo['response condition'] == '1':  # black left and 90° left and blue left
    conditionCUE_feet = "A"
    conditionCUE_hand = "A"
    conditionGAB = "A"
    print("black left and 90° left and blue left")
    
elif expInfo['response condition'] == '2':  # white left and 0° left and blue left
    conditionCUE_feet = "A"
    conditionCUE_hand = "B"
    conditionGAB = "B"
    print("white left and 0° left and blue left")
    
elif expInfo['response condition'] == '3':  # black left and 0° left and blue left
    conditionCUE_feet = "A"
    conditionCUE_hand = "A"
    conditionGAB = "B"
    print("black left and 0° left and blue left")
    
elif expInfo['response condition'] == '4':  # white left and 90° left and blue left
    conditionCUE_feet = "A"
    conditionCUE_hand = "B"
    conditionGAB = "A"
    print("white left and 90° left and blue left")
    
elif expInfo['response condition'] == '5':  # black left and 90° left and yellow left
    conditionCUE_feet = "B"
    conditionCUE_hand = "A"
    conditionGAB = "A"
    print("black left and 90° left and yellow left")
    
elif expInfo['response condition'] == '6':  # white left and 0° left and yellow left
    conditionCUE_feet = "B"
    conditionCUE_hand = "B"
    conditionGAB = "B"
    print("white left and 0° left and yellow left")
    
elif expInfo['response condition'] == '7':  # black left and 0° left and yellow left
    conditionCUE_feet = "B"
    conditionCUE_hand = "A"
    conditionGAB = "B"
    print("black left and 0° left and yellow left")
    
elif expInfo['response condition'] == '8':  # white left and 90° left and yellow left
    conditionCUE_feet = "B"
    conditionCUE_hand = "B"
    conditionGAB = "A"
    print("white left and 90° left and yellow left")
    
if expInfo['effector'] == 'hand1':
    effector_order = 'hand1'
    effector = 'hand'
    b_run = 1 # first run of the script
elif expInfo['effector'] == 'hand2':
    effector_order = 'hand2'
    effector = 'hand'
    b_run = 2  # second run of the script
elif expInfo['effector'] == 'feet1':
    effector_order = 'feet1'
    effector = 'feet'
    b_run = 1  # first run of the script
elif expInfo['effector'] == 'feet2':
    effector_order = 'feet2'
    effector = 'feet'
    b_run = 2  # second run of the script
    
_thisDir = os.path.abspath(os.getcwd())
data_dir = _thisDir + os.sep + 'pilotdata' + os.sep + expInfo['participant']
if not os.path.exists(data_dir):
    os.makedirs(data_dir)

subject_id = expInfo['participant']

## create files 
filename = data_dir + os.sep + '%s_%s' % (expInfo['participant'], expInfo['date']) + '_' + str(b_run) + '_' + effector_order
filename_short = data_dir + os.sep + expInfo['participant'] + '_' + effector_order
filename_final = data_dir + os.sep + expInfo['participant'] + '_' + 'final'
output = "subject_id,trial,effector_order,block,condition,stim1,gabor.contrast,pressed_gabor,expected_gabor,pressed_cue,expected_cue,rt_gabor,rt_cue,accuracy_gabor,accuracy_cue,congruency,SDT,pressed_conf,conf,rt_conf,trial_start_time_rel2bloc,bloc_start_time_rel2exp,exp_start_date,effector\n"
running_filename = filename_short + '_TEMP.txt'
f_handle = open(running_filename, 'a')
f_handle.write(output)
f_handle.close()


logFile = logging.LogFile(filename+'.log', level=logging.DEBUG, filemode = 'a')

# experiment settings
## monitor parameters
monDistance = settings.monDistance
monWidth = settings.monWidth
monres1 = settings.monres1
monres2 = settings.monres2
win_color = settings.win_color

## display
fixation_size = settings.fixation_size
stim2_size = settings.stim2_size
size_instruc = settings.size_instruc
instr_time = settings.instr_time
police = settings.police
t_fb = settings.t_fb

# gabor
gabor_sf = settings.gabor_sf
noise_contrast = settings.noise_contrast
noise_opacity = settings.noise_opacity
noise_point = settings.noise_point
noise_size = settings.noise_size


## experimental parameters
deadline = settings.deadline
ITIarray = settings.ITIarray
SOA = settings.SOA
cue_time = settings.cue_time
black_cross_time = settings.black_cross_time
stim2_duration = settings.stim2_duration
neutral_color = settings.neutral_color
gab_cue_radius = settings.gab_cue_radius
gabor_cue_duration = settings.gabor_cue_duration
gabor_SOA = settings.gabor_SOA

# keys
LH = settings.LH
RH = settings.RH
LF = settings.LF
RF = settings.RF

# confidence display
conf_circle_color = settings.conf_circle_color
conf_lineColor= settings.conf_lineColor
conf_circle_radius = settings.conf_circle_radius
conf_text_height = settings.conf_text_height

# position
pos1 = settings.pos1
pos2 = settings.pos2
pos3 = settings.pos3
pos4 = settings.pos4

# experimental trials handling
n_trial_break = settings.n_trial_break
seq = settings.seq
init_seq = seq
conf_seq = settings.conf_seq
n_trial = settings.n_trial
n_block = settings.n_block

## * Hardware
# Monitor definition
mon1 = monitors.Monitor('testMonitor')
mon1.setDistance(monDistance)  # cm
mon1.setWidth(monWidth)  # cm
mon1.setSizePix([monres1,monres2])
mon1.saveMon()

## keyboard
kb = keyboard.Keyboard()  # initialize keyboards

## * Stimuli & texts
# create a window to draw in
win = visual.Window(size=[monres1, monres2], monitor=mon1, allowGUI=False, units='deg', fullscr=False, color=win_color,
                    colorSpace='rgb255')
win.setMouseVisible(False)  # hide the mouse cursor

## stimuli
fixation = visual.GratingStim(win=win,
                              mask='cross', size=fixation_size,
                              pos=[0, 0], sf=0)

blackCross = visual.GratingStim(win=win,
                                mask='cross', size=fixation_size,
                                pos=[0,0], sf=0)
blackCross.color = neutral_color

gabor = visual.GratingStim(win, tex="sin",
                           mask="gauss", texRes=256, pos=[0, 0],
                           size=stim2_size, sf=gabor_sf, ori=0, name='gabor')

# indicate response to cue as a gabor flash
no_gabor = visual.GratingStim(win, tex="sin",
                              mask="gauss", texRes=256, pos=[0, 0],
                              size=stim2_size, sf=[0, 0], ori=0, name='gabor')

# noise circle for gabor
noise_circle = visual.NoiseStim(win=win, units="deg", noiseType = "Uniform", size=noise_size, mask="circle",
                            contrast = noise_contrast, noiseElementSize = noise_point, pos=[0, 0], opacity = noise_opacity)


## Instructions


instr_1 = "Bienvenue ! \n\n\
Placez l'index de vos deux mains sur les touches de gauche et de droite \n\n\
Et vos deux pieds sur les deux pédales \n\n\n"

instr_2 = "A chaque essai vous verrez un premier stimulus : une croix de couleur \n\n\
suivie d'un deuxième stimulus : un cercle blanc OU un cercle bruité suivi d'un stimulus composé de lignes parallèles"

instr_3 = "Après la présentation du 2ème stimulus, vous devrez indiquer:\n\n\
\t - soit la couleur de la croix si vous avez vu un flash blanc \n\n\
\t - soit l'orientation du stimulus qui arrive si vous avez vu un cercle bruité"

instr_3bis = "Autrement dit, si le 2ème stimulus est un flash blanc: \n\
\t - indiquez la couleur de la croix que vous venez de voir\n\n\n\
SINON: \n\
\t - indiquez l'orientation du stimulus qui arrive (vertical ou horizontal)"

instr_4CUE_HAND_A = "Pour répondre à la couleur de la croix avec les deux mains, appuyez sur la touche: \n\n\
\t - de gauche si la croix est bleue\n\n\
\t - de droite si la croix est jaune\n\n"

instr_4CUE_HAND_B = "Pour répondre à la couleur de la croix avec les deux mains, appuyez sur la touche: \n\n\
\t - de gauche si la croix est jaune\n\n\
\t - de droite si la croix est bleue\n\n"

instr_4CUE_FEET_A = "Pour répondre à la couleur de la croix avec les deux pieds, appuyez sur la touche: \n\n\
\t - de gauche si la croix est noire\n\n\
\t - de droite si la croix est blanche\n\n"

instr_4CUE_FEET_B = "Pour répondre à la couleur de la croix avec les deux pieds, appuyez sur la pédale: \n\n\
\t - de gauche si la croix est blanche\n\n\
\t - de droite si la croix est noire\n\n"

instr_4GAB_A = "Pour répondre à la couleur de la croix, appuyez sur la touche: \n\n\
\t - de gauche si le stimulus est orienté horizontalement — \n\n\
\t - de droite si le stimulus est orienté verticalement | \n\n"

instr_4GAB_B = "Pour répondre à la couleur de la croix, appuyez sur la touche: \n\n\
\t - de gauche si le stimulus est orienté verticalement | \n\n\
\t - de droite si le stimulus est orienté horizontalement — \n\n"

instr_5 = "Attention! \n\n\n\
ATTENDEZ et FAITES BIEN ATTENTION AU DEUXIEME STIMULUS pour savoir ce qu'il faut répondre !"

instr_5bis = "Essayez de répondre le plus rapidement possible après avoir observé le deuxième stimulus" 

instr_6 = "Petit rappel, à chaque essai, indiquez uniquement : \n\n\n\
\t - l'orientation du deuxième stimulus si celui-ci est composé de lignes\n\n\
\t - la couleur de la croix si le deuxième stimulus est un flash blanc."

instr_conf1 = "Si lors d'un essai vous avez indiqué l'orientation des lignes, \n\n\
vous devrez ensuite indiquer votre degré de confiance dans cette dernière réponse \n\n\
(et uniquement celle-ci)\n\n\
en utilisant une échelle de 1 à 4:\n\n\
1 = il est très peu probable que votre réponse était correcte\n\n\
à  \n\n\
4 = il est très probable que votre réponse était correcte"

instr_conf2 = "Vous utiliserez pour cela les deux pieds et les deux mains.\n\n\
Les chiffres vont être présenté ALEATOIREMENT à quatres coins autour du centre,\n\n\n\
Chaque coin correspond spatialement à un de vos membres:"

instr_conf2bis = "main gauche = en haut à gauche, \n\n\
main droite = en haut à droite,\n\n\
pied gauche = en bas à gauche,\n\n\
pied droit = en bas à droite, \n\n\n\
comme ceci : "

instr_conf3 = "Faites attention à bien selectionner le degré de confiance voulu, les chiffres changent de place à chaque essai.\n\n\n\
Essayez de bien évaluer votre réponse, et de bien utiliser les 4 degrés de confiance tout au long de l'expérience,\n\n\
Attention! La confiance ne porte QUE sur votre réponse à l'orientation du deuxième stimulus..."

instr_repos = "Vous pouvez prendre un petit temps de pause, reposez-vous les yeux...\n\n\
puis appuyez sur n'importe quelle touche pour continuer."

instr_fin = "Bravo, et merci! \n\n\n\
Vous avez fini cette partie de l'expérience. \n\n\n\
Merci de vous adresser à l'expérimentateur"

too_late = visual.TextStim(win, ori=0,
                           height=size_instruc,
                           font=police,
                           color='black',
                           text="Trop lent!")


## load threshold from staircase
print(f"participant {expInfo['participant']}")
thresh_array = np.loadtxt(data_dir + os.sep + expInfo['participant']+'_thresh' + '.csv')
delta = thresh_array.flat[0] 
print(f"threshold loaded. delta = {delta}")
    

## STARTING EXPERIMENT
core.wait(.5)  # for some reason, the first ask isn't displayed unless waiting a bit...

# get time framework
now_date = datetime.now().time()
exp_start_date = now_date.strftime("%H:%M:%S.%f")
exp_start_ms_abs = current_milli_time()

# Instructions
ask(text=instr_1)
ask(text=instr_2)
ask(text=instr_3)
ask(text=instr_3bis)
if effector == "hand":
    if conditionCUE_hand == "A":
        ask(text=instr_4CUE_HAND_A)
    else:
        ask(text=instr_4CUE_HAND_B)
if effector == "feet":
    if conditionCUE_feet == "A":
        ask(text=instr_4CUE_FEET_A)
    else:
        ask(text=instr_4CUE_FEET_B)

if conditionGAB == "A":
    ask(text = instr_4GAB_A)
else:
    ask(text=instr_4GAB_B)
ask(text=instr_5)
ask(text=instr_5bis)
ask(text=instr_6)
ask(text=instr_conf1)
ask(text=instr_conf2)
ask(instr_conf2bis)
confidence_image = visual.ImageStim(win,image=_thisDir + os.sep + 'confidence.png', size=[48,24])
confidence_image.draw()
win.flip()
kb.clearEvents()
while True:
    theseKeys = kb.getKeys()
    if 'escape' in theseKeys:
        core.quit()
    if len(theseKeys):
        break
confidence2_image = visual.ImageStim(win,image=_thisDir + os.sep + 'confidence2.png', size=[48,24])
confidence2_image.draw()
win.flip()
kb.clearEvents()
while True:
    theseKeys = kb.getKeys()
    if 'escape' in theseKeys:
        core.quit()
    if len(theseKeys):
        break  
ask(text=instr_conf3)


## ** Main loop

# get bloc start time
now = current_milli_time()
bloc_start_ms_abs = now
bloc_start_time_rel2exp = now - exp_start_ms_abs 

## Initialisation
gabor.contrast = 0
g_trial = 0  # current gabor trial
currentblock_trial = 0  # current within block trial
i = 0
trial = 0
toolate = 0
lst_toolate = np.zeros((11,1))
block = 1
data_container = []

while i < len(seq):  # main loop through all trials #
    ## initialize
    pressed_gabor = -1
    expected_gabor = -1
    pressed_cue = -1
    expected_cue = -1
    rt_gabor = -1
    rt_cue = -1
    accuracy_gabor = -1
    accuracy_cue = -1
    Not_answered = False
    conf = -1
    rt_conf = -1
    pressed_conf = -1
    gabor.contrast = -1

    # get trial timestamp
    now = current_milli_time()
    trial_start_time_rel2bloc = now - bloc_start_ms_abs
    
    ## clear kb  buffer   
    kb.clearEvents()
        
    ## update trial and block number
    trial += 1
    c_i = seq[i][0]
    o_i = seq[i][1]
    
    if o_i == 0:  # we are in a cue trial
        condition = "cue"
    else:
        condition = "gabor"

    ## define color and orientation

    if effector == 'hand':
        if c_i == 1:
            color_i = 'white'
        elif c_i == 2:
            color_i = 'black'
            
    elif effector == 'feet':
        if c_i == 1:
            color_i = 'blue'
        elif c_i == 2:
            color_i = 'yellow'
        
    if o_i == 1:
        ori_i = 0
    elif o_i == 2:
        ori_i = 90
    elif o_i == 0:  # no_gabor trial
        ori_i = -1
        
    fixation.color = color_i
    gabor.ori = ori_i

    if condition == "gabor":
        g_trial += 1 # new gabor trial 
        gabor.contrast = delta

    ## expected responses
    # cue
    if effector == 'hand':
        if conditionCUE_hand == "A":  # key order
            if color_i == 'black':
                expected_cue = LH
            elif color_i == 'white':
                expected_cue = RH
    
        elif conditionCUE_hand == "B":  # key order
            if color_i == 'black':
                expected_cue = RH
            elif color_i == 'white':
                expected_cue = LH

    elif effector == 'feet':  
        if conditionCUE_feet == "A": # key order
            if color_i == 'blue':
                expected_cue = LF
            elif color_i == 'yellow':
                expected_cue = RF

        elif conditionCUE_feet == "B":  # key order
            if color_i == 'blue':
                expected_cue = RF
            elif color_i == 'yellow':
                expected_cue = LF

    # gabor
    if conditionGAB == "A":  # key order
        if ori_i == 90:
            expected_gabor = LH
        elif ori_i == 0:
            expected_gabor = RH
    elif conditionGAB == "B":  # key order
        if ori_i == 90:
            expected_gabor = RH
        elif ori_i == 0:
            expected_gabor = LH


    # draw black fixation
    blackCross.draw()
    win.flip()
    core.wait(black_cross_time)

    # draw cue
    fixation.draw()
    win.flip()
    # sendTrigger(trigstim1)a
    core.wait(cue_time)
    win.flip()
    core.wait(SOA)

    # draw flash or gabor
    if condition == "gabor":  # gabor trial
        noise_circle.draw() # draw gabor cue
        win.flip()
        core.wait(gabor_cue_duration)
        win.flip()
        core.wait(gabor_SOA)
        
        for k in range(0, stim2_duration):  # 3 frames with 60HZ = 50 msec
            gabor.draw()
            noise_circle.draw()
            win.flip()
            # sendTrigger(trigstim2)
        win.flip()

    elif condition == "cue":    # draw flash
        for k in range(0, stim2_duration):  # 3 frames with 60HZ = 50 msec
            no_gabor.draw()
            win.flip()
            # sendTrigger(trigstim2)
        win.flip()
        
    
    # Response I
    
    kb.clearEvents()
    kb.clock.reset()
    
    if condition == "cue":  # cue response
    
        if effector == 'hand': 
            
            timer = core.CountdownTimer(deadline)           
            while timer.getTime() > 0:
                theseKeys = kb.getKeys(keyList=[LH, RH, 'escape'], waitRelease = False, clear = False)                
                if theseKeys != []:    
                
                    if 'escape' in theseKeys:
                        core.quit()
                                            
                    theseKeys_cue = theseKeys[0]
                    pressed_cue = theseKeys_cue.name
                    rt_cue = theseKeys_cue.rt
                    
                    if expected_cue == pressed_cue:
                        accuracy_cue = 1
                        accdisp = 1
                    else:
                        accuracy_cue = 0
                        accdisp = 0
                    break

            if theseKeys == []:     # too late
                        too_late.draw()
                        print("too late !")
                        win.flip()
                        core.wait(t_fb)
                        # update sequence
                        retry = seq[i]  # get current conditions for retry
                        seq = np.delete(seq, i,0)  # remove current row from seq
                        seq = np.insert(seq,((block*n_trial)-1), retry, axis=0)  # add it to the end of the current block
                        trial -= 1  # restart trial
                        toolate += 1
                        continue # restart trial   
                        

        elif effector == 'feet':
            
            timer = core.CountdownTimer(deadline)           
            while timer.getTime() > 0:
                theseKeys = kb.getKeys(keyList=[LF, RF, 'escape'], waitRelease = False, clear = False)
                if theseKeys != []: 
                    
                    if 'escape' in theseKeys:
                        core.quit()
                        
                    theseKeys_cue = theseKeys[0]
                    pressed_cue = theseKeys_cue.name
                    rt_cue = theseKeys_cue.rt
                    
                    if expected_cue == pressed_cue:
                        accuracy_cue = 1
                        accdisp = 1
                    else:
                        accuracy_cue = 0
                        accdisp = 0
                    break

            if theseKeys == []:  # too late
                too_late.draw()
                print("too late !")
                win.flip()
                core.wait(t_fb)
                # update sequence
                retry_index = i - (n_trial*(block-1))
                retry = seq[i]  # gqet retry conditions
                seq = np.delete(seq, i,0)  # remove current row from seq
                seq = np.insert(seq,((block*n_trial)-1), retry, axis=0)  # add it to the end of the block
                trial -= 1  # restart trial
                toolate += 1
                continue # restart trial  

    elif condition == "gabor":  # gabor response
        timer = core.CountdownTimer(deadline)           
        while timer.getTime() > 0:
            theseKeys = kb.getKeys(keyList=[LH, RH, 'escape'], waitRelease = False, clear = False)
            if theseKeys != []:
                
                if 'escape' in theseKeys:
                    core.quit()
                    
                theseKeys_gabor = theseKeys[0]
                pressed_gabor = theseKeys_gabor.name
                rt_gabor = theseKeys_gabor.rt
                
                if expected_gabor == pressed_gabor:
                    accuracy_gabor = 1
                    accdisp=1
                else:
                    accuracy_gabor = 0
                    accdisp = 0
                break

        if theseKeys == []:  # too late
            too_late.draw()
            print("too late !")
            win.flip()
            core.wait(t_fb)
            # update sequence
            retry_index = i - (n_trial * (block - 1))
            retry = seq[i]  # get retry conditions
            seq = seq.tolist()
            seq = np.delete(seq, i,0)  # remove current row from seq
            seq = np.insert(seq,((block*n_trial)-1), retry, axis=0)  # add it to the end of the block
            trial -= 1  # restart trial
            g_trial -= 1
            toolate += 1
            continue # restart trial 
            
            
    # Condition specification for the dataframe   
    #congruency
    if condition == "gabor":
        if effector == 'hand':
            if expected_cue == pressed_gabor:
                congruency = "congruent"
            elif expected_cue != pressed_gabor:
                congruency = "incongruent"
                
        elif effector == 'feet':
            if expected_cue == LF and pressed_gabor == LH:
                congruency = "congruent"
            elif expected_cue == RF and pressed_gabor == RH:
                congruency = "congruent"            
            elif expected_cue == LF and pressed_gabor == RH:
                congruency = "incongruent"    
            elif expected_cue == RF and pressed_gabor == LH:
                congruency = "incongruent"                    
    else:
        congruency = "none"
        
    # first-order stim   
    if condition == "gabor":
        stim1 = ori_i
    elif condition == "cue":
        stim1 = color_i
        
    # SDT 
    if condition == "gabor":
        if ori_i == 0:
            if accuracy_gabor == 1:
                SDT = "hit"
            elif accuracy_gabor == 0:
                SDT = "miss"
        if ori_i == 90:
            if accuracy_gabor == 1:
                SDT = "CR"
            elif accuracy_gabor == 0:
                SDT = "FA"
    else:
        SDT = "none"
    
    
    ## Confidence for gabor trials
    
    if condition == "gabor":
        ## draw conf scale
        s = np.random.randint(1, 5)
    
        conf1 = int(conf_seq[g_trial][0])
        conf2 = int(conf_seq[g_trial][1])
        conf3 = int(conf_seq[g_trial][2])
        conf4 = int(conf_seq[g_trial][3])
    
        circle1 = visual.Circle(win=win, units="deg", radius=conf_circle_radius, fillColor=conf_circle_color,
                                   lineColor=conf_lineColor, pos=pos1)
        circle2 = visual.Circle(win=win, units="deg", radius=conf_circle_radius, fillColor=conf_circle_color,
                                lineColor=conf_lineColor, pos=pos2)
        circle3 = visual.Circle(win=win, units="deg", radius=conf_circle_radius, fillColor=conf_circle_color,
                                lineColor=conf_lineColor, pos=pos3)
        circle4 = visual.Circle(win=win, units="deg", radius=conf_circle_radius, fillColor=conf_circle_color,
                                lineColor=[-1, -1, -1], pos=pos4)
        
        conf_fixation = visual.GratingStim(win=win,
                                      mask='cross', size=fixation_size,
                                      pos=[0, 0], sf=0, color='black')
        
        text_conf1 = visual.TextStim(win, text=str(conf1), units="deg", color='black',
                                     pos=pos1, bold=True, height=conf_text_height,
                                       opacity=1.0)
        text_conf2 = visual.TextStim(win, text=str(conf2), units="deg", color='black',
                                     pos=pos2, bold=True, height=conf_text_height,
                                       opacity=1.0)
        text_conf3 = visual.TextStim(win, text=str(conf3), units="deg", color='black',
                                     pos=pos3, bold=True, height=conf_text_height,
                                       opacity=1.0)
        text_conf4 = visual.TextStim(win, text=str(conf4), units="deg", color='black',
                                     pos=pos4, bold=True, height=conf_text_height,
                                       opacity=1.0)
    
        circle1.draw()
        circle2.draw()
        circle3.draw()
        circle4.draw()
        text_conf1.draw()
        text_conf2.draw()
        text_conf3.draw()
        text_conf4.draw()
        conf_fixation.draw()
        win.flip()
        
    
        # Response II
        
        kb.clearEvents()
        kb.clock.reset()
        while True:
            theseKeys = kb.getKeys(keyList=[LH, RH, LF, RF, 'escape'], waitRelease = False, clear = False)
            if 'escape' in theseKeys:
                win.close()
                core.quit()
            if len(theseKeys):
                theseKeys_conf = theseKeys[0]
                pressed_conf = theseKeys_conf.name
                rt_conf = theseKeys_conf.rt
                if pressed_conf == LH:  # upper left
                    conf = conf2
                    circle_choice = circle2
                    conf_choice = text_conf2
                elif pressed_conf == RH:  # upper right
                    conf = conf3
                    circle_choice = circle3
                    conf_choice = text_conf3
                elif pressed_conf == LF:  # down left
                    conf = conf1
                    circle_choice = circle1
                    conf_choice = text_conf1
                elif pressed_conf == RF:  # down right
                    conf = conf4
                    circle_choice = circle4
                    conf_choice = text_conf4
                win.flip()
                break

    print(
        (f"{block} trial {trial}, conditions:{condition}, accuracy={accdisp} {congruency}({pressed_gabor}), stim: {stim1}") + ' contrast= %0.3f' %(gabor.contrast) )

    i = i + 1

    ## write
    data_array = [subject_id,
                  trial,
                  effector_order,
                  block,
                  condition,
                  stim1,
                  gabor.contrast,
                  pressed_gabor,
                  expected_gabor,
                  pressed_cue,
                  expected_cue,
                  rt_gabor,
                  rt_cue,
                  accuracy_gabor,
                  accuracy_cue,
                  congruency,
                  SDT,
                  pressed_conf,
                  conf,
                  rt_conf,
                  trial_start_time_rel2bloc,
                  bloc_start_time_rel2exp,
                  exp_start_date,
                  effector
                  ]

    data_container.append(data_array)
    output = str(data_array[0]) + ',' + str(data_array[1]) + ',' + str(data_array[2]) + ',' + str(
        data_array[3]) + ',' + str(data_array[4]) + ',' + str(data_array[5]) + ',' + str(data_array[6]) + ',' + str(
        data_array[7]) + ',' + str(data_array[8]) + ',' + str(data_array[9]) + ',' + str(data_array[10]) + ',' + str(
        data_array[11]) + ',' + str(data_array[12]) + ',' + str(data_array[13]) + ',' + str(data_array[14]) + ',' + str(
        data_array[15]) + ',' + str(data_array[16]) + ',' + str(data_array[17]) + ',' + str(data_array[18]) + ',' + str(
        data_array[19]) + ',' + str(data_array[20]) + ',' + str(data_array[21]) + ',' + str(data_array[22]) + ',' + str(data_array[23]) + '\n'

    f_handle = open(running_filename, 'a')
    f_handle.write(output)
    f_handle.close()
        
    logging.flush()
    kb.clearEvents()

    if trial % n_trial == 0:
        lst_toolate[block-1] = toolate
        block += 1
        print(f"End of block {block-1}. Number of toolate trials: {toolate}")
        toolate = 0
        core.wait(t_fb)
        ask(f"end of bloc {block-1} / {n_block}")
        ask(instr_repos)
        # get current time
        now = current_milli_time()
        bloc_start_time = now - exp_start_ms_abs 

        if block == (n_block + 1):
            ask("End of this part of the experiment")

		
    win.flip()  # clean screen
    ITI = np.random.choice(ITIarray)
    core.wait(ITI)

## ** Fin des essais
core.wait(1.0)
ask(instr_fin)
win.flip()
#sendTrigger(trigend)

## save & cleanup
data_container = np.array(data_container, dtype="object")
np.save(filename, data_container)
np.savetxt(filename + '.csv', data_container, delimiter=',', fmt='%s')


import glob 

if b_run == 2:
    
    list_df = []
    
    exp = data_dir + os.sep + '%s_*' % (expInfo['participant']) + '_*.csv'
    filenames = glob.glob(exp)
        
    for cur_filename in filenames:
        df = pd.read_csv(cur_filename, header = None)
        list_df.append(df)
        
    df_final = pd.concat(list_df)
    df_final.to_csv(filename_final + '.csv', header = False, index = False)
    
df_toolate = pd.DataFrame(lst_toolate) # too late info table
df_toolate.index = range(1,(n_block +1))
df_init_seq = pd.DataFrame(init_seq) # initial sequence info table

df_toolate.to_csv(data_dir + os.sep + effector + '_toolate_info_S' + expInfo['participant'] + '.csv', header = False, index = True)
df_init_seq.to_csv(data_dir + os.sep + effector + '_initial_seq_info_S' + expInfo['participant'] + '.csv', header = False, index = False)
    


win.close()
core.quit()

