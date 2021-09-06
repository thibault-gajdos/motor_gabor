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
from psychopy import core, visual, gui, data, monitors, logging
from psychopy.hardware import keyboard
from psychopy.visual import ShapeStim
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
expInfo = {'participant': '', 'response condition': ['1', '2'], 'effector': ['same1','same2','other1','other2'] }
expName = 'Motor'
expInfo['date'] = data.getDateStr()
expInfo['expName'] = expName
dlg = gui.DlgFromDict(dictionary=expInfo, title=expName)
if dlg.OK == False:
    core.quit()  # user pressed cancel

    # left/right response balance (90° = horizontal; 0° = vertical)
if expInfo['response condition'] == '1':  # 90° left 
    conditionGAB = "A"
    print("90° left")
    
elif expInfo['response condition'] == '2':  #0° left
    conditionGAB = "B"
    print("0° left")
    

    
if expInfo['effector'] == 'same1':
    effector_order = 'same1'
    effector = 'same'
    b_run = 1 # first run of the script
elif expInfo['effector'] == 'same2':
    effector_order = 'same2'
    effector = 'same'
    b_run = 2  # second run of the script
elif expInfo['effector'] == 'other1':
    effector_order = 'other1'
    effector = 'other'
    b_run = 1  # first run of the script
elif expInfo['effector'] == 'other':
    effector_order = 'other2'
    effector = 'other'
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
output = "subject_id,trial,effector_order,block,condition,stim1,gabor.contrast,pressed_gabor,expected_gabor,pressed_cue,expected_cue,rt_gabor,rt_cue,accuracy_gabor,accuracy_cue,congruency,SDT,trial_start_time_rel2bloc,bloc_start_time_rel2exp,exp_start_date,effector\n"
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
cue_deadline = settings.cue_deadline
gab_deadline = settings.gab_deadline
ITIarray = settings.ITIarray
SOA = settings.SOA
cue_time = settings.cue_time
white_cross_time = settings.white_cross_time
stim2_duration = settings.stim2_duration
neutral_color = settings.neutral_color
gab_cue_radius = settings.gab_cue_radius
arrow_size = settings.arrow_size
#gabor_SOA = settings.gabor_SOA

# keys
LH = settings.LH
RH = settings.RH
LF = settings.LF
RF = settings.RF

# experimental trials handling
n_trial_break = settings.n_trial_break
seq = settings.seq
init_seq = seq
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
# arrows coordinates
arrowright = [(-1,0.5),(-1,-0.5),(0,-0.5),(0,-1),(1,0),(0,1),(0,0.5)] 
arrowleft = [(-1,0),(0,1),(0,0.5),(1,0.5),(1,-0.5),(0,-0.5),(0,-1)]

arrow = ShapeStim(win, vertices=arrowleft, fillColor='white', size=arrow_size, lineColor='white')

whiteCross = visual.GratingStim(win=win,
                                mask='cross', size=fixation_size,
                                pos=[0,0], sf=0)
whiteCross.color = neutral_color

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


instr_1 = "Bienvenue dans cette expérience !"

instr_2 = "A chaque essai vous verrez un premier stimulus : une flèche blanche orientée à gauche ou à droite, \n\n\
suivie d'un deuxième stimulus : un cercle blanc OU un stimulus composé de lignes parallèles"

instr_3 = "Après la présentation du 2ème stimulus, vous devrez indiquer:\n\n\
\t - soit l'orientation de la flèche si vous avez vu un flash blanc \n\n\
\t - soit l'orientation du stimulus qui arrive si vous avez vu un cercle bruité"

instr_3bis = "Autrement dit, si le 2ème stimulus est un flash blanc: \n\
\t - indiquez l'orientation de la flèche que vous venez de voir\n\n\n\
SINON: \n\
\t - indiquez l'orientation du deuxième stimulus (vertical ou horizontal)"

instr_4CUE_SAME = "Pour répondre à l'orientation de la flèche avec les index des deux mains, appuyez sur la touche: \n\n\
\t - de gauche (x) avec votre index \n\n\
\t - de droite (y) avec votre index \n\n"

instr_4CUE_OTHER = "Pour répondre à l'orientation de la flèche avec les majeurs des deux mains, appuyez sur la touche: \n\n\
\t - de gauche (z) avec votre majeur\n\n\
\t - de droite (i) avec votre majeur\n\n"

instr_4GAB_A = "Pour répondre à la couleur de la croix, appuyez sur la touche: \n\n\
\t - de gauche (x) si le stimulus est orienté horizontalement — \n\n\
\t - de droite (n) si le stimulus est orienté verticalement | \n\n"

instr_4GAB_B = "Pour répondre à la couleur de la croix, appuyez sur la touche: \n\n\
\t - de gauche (x) si le stimulus est orienté verticalement | \n\n\
\t - de droite (n) si le stimulus est orienté horizontalement — \n\n"

instr_5 = "Attention! \n\n\n\
ATTENDEZ et FAITES BIEN ATTENTION AU DEUXIEME STIMULUS pour savoir ce qu'il faut répondre !"

instr_5bis = "Essayez de répondre le plus rapidement possible après avoir observé le deuxième stimulus, et il faut toujours répondre.\n\n\n\
Si vous ne savez pas quoi répondre, répondez au hasard." 

instr_6 = "Petit rappel, à chaque essai, indiquez uniquement : \n\n\n\
\t - l'orientation du deuxième stimulus si celui-ci est composé de lignes dans du bruit\n\n\
\t - l'orientation de la flèche que vous venez de voir si le deuxième stimulus est un flash blanc."

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
if effector == "same":
    ask(text=instr_4CUE_SAME)
if effector == "other":
    ask(text=instr_4CUE_OTHER)

if conditionGAB == "A":
    ask(text = instr_4GAB_A)
else:
    ask(text=instr_4GAB_B)
ask(text=instr_5)
ask(text=instr_5bis)
ask(text=instr_6)


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
lst_toolate = np.zeros((n_block,1))
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


    if c_i == 1:
        side_i = arrowleft
    elif c_i == 2:
        side_i = arrowright
                    
    if o_i == 1:
        ori_i = 0
    elif o_i == 2:
        ori_i = 90
    elif o_i == 0:  # no_gabor trial
        ori_i = -1
        
    arrow.vertices = side_i
    gabor.ori = ori_i

    if condition == "gabor":
        g_trial += 1 # new gabor trial 
        gabor.contrast = delta

    ## expected responses
    # cue
    if effector == 'same':
        if side_i == arrowleft:
            expected_cue = LH
        elif side_i == arrowright:
            expected_cue = RH
    

    elif effector == 'other':  
        if side_i == arrowleft:
            expected_cue = LF
        elif side_i == arrowright:
            expected_cue = RF

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


    # draw white fixation
    whiteCross.draw()
    win.flip()
    core.wait(white_cross_time)

    # draw cue
    arrow.draw()
    win.flip()
    # sendTrigger(trigstim1)a
    core.wait(cue_time)
    win.flip()
    core.wait(SOA)

    # draw flash or gabor
    if condition == "gabor":  # draw gabor      
        for k in range(0, stim2_duration):  # 3 frames with 60HZ = 50 msec
            gabor.draw()
            noise_circle.draw()
            win.flip()
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
        if effector == 'same':             
            timer = core.CountdownTimer(cue_deadline)           
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
                        

        elif effector == 'other':
            timer = core.CountdownTimer(cue_deadline)           
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
        timer = core.CountdownTimer(gab_deadline)           
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
        if effector == 'same':
            if expected_cue == pressed_gabor:
                congruency = "congruent"
            elif expected_cue != pressed_gabor:
                congruency = "incongruent"
                
        elif effector == 'other':
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
        if side_i == arrowright:
            stim1 = 'right'
        elif side_i == arrowleft:
            stim1 = 'left'
        
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
        data_array[19]) + ',' + str(data_array[20]) + ',' + '\n'

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

