#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on 02.03.2020

@author: thibault gajdos
"""
## * libraries

import os #handy system and path functions
from psychopy import core, visual, event, gui, data, monitors, microphone
from psychopy.hardware import keyboard
import numpy as np
import pandas as pd
import speech_recognition as sr
from psychopy import prefs
prefs.hardware['audioLib'] = ['pyo', 'PTB', 'pygame','sounddevice']

## * Functions
def ask(text='', color = 'white'):
    """
    Display instruction.
    key = space
    """
    while True:
        theseKeys = kb.getKeys()
        if len(theseKeys):
            break
        if kb.getKeys(keyList=["escape"], waitRelease=False):
            core.quit()
        instr = visual.TextStim(win,
                                  ori=0,
                                  height = size_instruc,
                                  font = police,
                                wrapWidth = 35)
        instr.color = color
        instr.text = text
        instr.draw()
        win.flip()
    core.wait(instr_time)

 

    
## * informations & files

## participant and expe info infos

expInfo = {'participant':'','ordre':['1','2']}
expName = 'Motor' 
expInfo['date'] = data.getDateStr() 
expInfo['expName'] = expName
dlg = gui.DlgFromDict(dictionary=expInfo, title=expName)
if dlg.OK == False: core.quit()  #user pressed cancel

_thisDir =  os.path.abspath(os.getcwd())
data_dir = _thisDir + os.sep + 'data' + os.sep + expInfo['participant']  
if not os.path.exists(data_dir):
    os.makedirs(data_dir)

sound_dir = data_dir + os.sep + 'sound'
if not os.path.exists(sound_dir):
    os.makedirs(sound_dir)

subject_id = expInfo['participant']
ordre = int(expInfo['ordre'])

## create files 
filename = data_dir + os.sep + '%s_%s' %(expInfo['participant'], expInfo['date'])
filename_short =  data_dir + os.sep + expInfo['participant']
output = "subject_id,trial,trial_type,block,gabor.contrast,pressed_gabor,expected_gabor,pressed_cue,expected_cue,rt_gabor, rt_cue,accuracy_gabor,accuracy_cue,conf,rt_conf\n"
running_filename = filename_short + '_TEMP.txt'
f_handle = open(running_filename, 'a') 
f_handle.write(output)
f_handle.close() 


## * Hardware
#Monitor definition
mon1 = monitors.Monitor('testMonitor')
mon1.setDistance(50) #cm
mon1.setWidth(30) #cm
mon1.setSizePix([800, 600])
mon1.saveMon()

## keyboard
kb = keyboard.Keyboard() # initialize keyboards

## micro
r = sr.Recognizer()

## * Stimuli & texts
#create a window to draw in
win = visual.Window(size=[800, 600], monitor=mon1, allowGUI=False, units='deg', fullscr=False, color=[128,128,128], colorSpace='rgb255')
win.setMouseVisible(False) #hide the mouse cursor

## stimuli
fixation = visual.GratingStim(win=win, 
                              mask='cross', size=0.8, 
                              pos=[0,0], sf=0)

gabor = visual.GratingStim(win,tex="sin",
                           mask="gauss",texRes=256,  pos=[0,0],
                           size=6, sf=[2,0], ori = 0, name='gabor')

## Instructions
police = 'Consolas' #monospaced font
size_instruc = 1
instr_time = 0.1

instr_1 = "Bienvenue ! \n\n\
Placez votre anulaire, votre  majeur et votre index \n\n\
de la main gauche sur les touches \n\n\
portant une gommette de couleur  \n\n\n\
Vous utiliserez votre main droite pour \n\n\
les touches avec des gommettes blanches"

instr_2 = "A chaque essai vous verrez une croix de couleur \n\n\
suivie d'un stimulus orienté verticalement ou horizontalement"

instr_3 = "Après la présentation du stimulus, vous devrez successivement\n\n\
répondre en fonction de la couleur de la croix \n\n\
et de l'orientation du stimulus"

instr_31 = "Après la présentation du stimulus, appuyez sur la touche: \n\n\
\t - de gauche si la croix est rouge\n\n\
\t - du milieu si la croix est verte\n\n\
\t - de droite si la croix est bleue"

instr_32 = "Après la présentation du stimulus, dites:\n\n\
\t - \"gauche\" si la croix est rouge\n\n\
\t - \"milieu\" si la croix est verte\n\n\
\t - \"droite\" si la croix est bleue"

instr_4 = "Puis appuyez sur la touche: \n\n\
\t - de gauche si le stimulus est horizontal\n\n\
\t - de droite si le stimulus est vertical \t"

instr_5 = "Attention! \n\n\n\
Il ne faut pas répondre avant l'apparition du stimulus!"

instr_6 = "Une fois que vous aurez indiqué l'orientation\n\n\
du stimulus, vous devrez indiquer votre degré de confiance\n\n\
dans cette dernière réponse (et uniquement celle-ci)\n\n\
en utilisant une échelle de\n\n\
1 (indiquant que vous avez répondu au hasard)\n\n\
à  \n\n\
4 (indiquant que vous êtes sûr de votre réponse)"

instr_7 ="Vous utiliserez pour cela les touches\n\n\
avec des gommettes blanches marquées 1 à 4.\n\n\n\
Si vous êtes sûr que vous-vous êtes trompés\n\n\
indiquez le en utilisant la touche jaune \n\n\n\
Attention! La confiance ne porte que sur l'orientation du stimulus..."

instr_repos = "Prenez un petit temps de pause, reposez-vous les yeux...\n\n\
puis appuyez sur n'importe quelle touche pour continuer."

instr_fin = "Bravo, et merci! \n\n\n\
Vous avez fini l'expérience. \n\n\n\
Merci d'attendre les instructions de l'expérimentateur"

too_late = visual.TextStim(win, ori=0,
                           height = size_instruc,
                           font = police,
                           color = 'red',
                           text = "Trop lent!")



## * trials handler
## possible_stim
color_array = np.array([1, 2, 2,  3]) #color: 1=rouge, 2=vert, 3=bleu
ori_array =  np.array([1, 2]) #orientation: 1=horizontal, 2=vertical
possible_stim = [[color,ori] for color in color_array for ori in ori_array]
n_block_key = 2 ## number of blocks in main condition
n_block_voice = 1 ## number of blocks in voice condition
n_trial = 16 ## number of trials/block 

##  sequence
seq = np.empty((0,2))
for i in range (n_block_key + n_block_voice):#2 blocks
   block = np.tile(possible_stim, int(n_trial/8)).reshape(-1,2)
   np.random.shuffle (block)
   seq = np.append(block, seq).reshape(-1,2)

if ordre == 1:
    key_cond = range(1,n_block_key*n_trial+1)
    voice_cond = range(n_block_key*n_trial+1,(n_block_key+n_block_voice)*n_trial+1)
else:
    voice_cond = range(1,n_block_voice*n_trial+1)
    key_cond = range(n_block_voice*n_trial+1,(n_block_key+n_block_voice)*n_trial+1)
## key_cond: trials with keyboard  answer
## voice_cond: trials with voice answer
    


## *  Trials
## parameters
deadline = 10
ITI = 1
SOA = .2
cue_time = .3
## load threshold from staircase
#thresh_array = np.loadtxt(data_dir + os.sep +  expInfo['participant']+'_thresh' + '.csv')
#delta = thresh_array.flat[0]
delta = .05


## ** Instructions
instr_time = .1
ask(text = "") # for some reason, the first ask isn't displayed... 
ask(text = instr_1)
ask(text = instr_2)
ask(text = instr_3)
if ordre == 1:
    ask(text = instr_31)
else:
    ask(text = instr_7)
ask(text = instr_4)
ask(text = instr_5)
ask(text = instr_6)
ask(text = instr_7)

## ** Main loop

## Initialisation
gabor.contrast = delta

trial = 0
block = 0
data_container = []

for i in range(len(seq)):
    ## initialize
    pressed_gabor = -1
    expected_gabor = -1
    pressed_cue = -1
    expected_cue = -1
    rt_gabor = -1
    rt_cue = -1
    accuracy_gabor = -1
    accuracy_cue = -1
    trial_type = None
    conf = -1
    rt_conf = -1
    pressed_conf = None
    
    ## clear kb  buffer
    kb.clearEvents(eventType='keyboard')

    ## update trial and block number
    trial += 1
    if (trial -1) % n_trial == 0:
        block += 1
    ## update trial type
    if trial in key_cond:
        trial_type = 'key'
    else:
        trial_type = 'voice'
    ## define color and orientation
    c_i = seq[i][0]
    o_i = seq[i][1]
    if c_i == 1:
        color_i = 'red'
    elif c_i == 2:
        color_i = 'green'
    elif c_i == 3:
        color_i = 'blue'
    if o_i == 1:
        ori_i = 0
    elif o_i == 2:
        ori_i = 90
    fixation.color = color_i
    gabor.ori = ori_i
    ## draw cue
    fixation.color = color_i
    fixation.draw()
    win.flip()
    core.wait(cue_time)
    win.flip()
    core.wait(SOA)

    ## draw gabor
    win.callOnFlip(kb.clock.reset)
    for i in range(0,2): #2 frames with 60HZ = 33.32 msec
            gabor.draw()
            win.flip()
    win.flip()

    ## expected responses
    if color_i == 'blue':
        expected_cue = 'q'
    elif color_i == 'green':
        expected_cue = 's'
    elif color_i == 'red':
        expected_cue = 'd'
    if ori_i == 90:
        expected_gabor = 'q'
    elif ori_i == 0:
         expected_gabor = 'd'
        
    ## cue response
    if trial_type == 'key':
        while True:
            t = kb.clock.getTime()
            if t > deadline:
                too_late.draw()
                win.flip()
                core.wait(1)
                break
            theseKeys = kb.getKeys(keyList=['q', 'd', 's'], waitRelease=False)
            if kb.getKeys(keyList=["escape"], waitRelease=False):
                core.quit()
            if len(theseKeys):
                theseKeys_cue = theseKeys[0] 
                pressed_cue = theseKeys_cue.name
                rt_cue = theseKeys_cue.rt
                if expected_cue == pressed_cue:
                    accuracy_cue = 1
                    break
                else:
                    accuracy_cue = 0
                    break

    if trial_type == 'voice':
        with sr.Microphone() as source:
            audio = r.listen(source)
            sound_file = sound_dir + os.sep + str(trial) + '.wav'
            with open(sound_file, "wb") as f:
                f.write(audio.get_wav_data())
                
    ## Gabor response
    while True:
        t = kb.clock.getTime()
        if t > 2*deadline:
            too_late.draw()
            win.flip()
            core.wait(1)
            break
        theseKeys = kb.getKeys(keyList=['q', 'd'], waitRelease=False)
        if kb.getKeys(keyList=["escape"], waitRelease=False):
                core.quit()
        if len(theseKeys):
            theseKeys_gabor = theseKeys[0] 
            pressed_gabor = theseKeys_gabor.name
            rt_gabor = theseKeys_gabor.rt
            if expected_gabor == pressed_gabor:
                accuracy_gabor = 1
                break
            else:
                accuracy_gabor = 0
                break
            break

    ## Confidence

    ## draw conf scale
    s = np.random.randint(1,5)
    conf_scale = visual.RatingScale(win, low=1, high=4, scale=None, labels = ('hasard','certain'), singleClick=True, marker = 'triangle', noMouse=True, markerStart=s, stretch = 2, showAccept = False)
    conf_scale.draw()
    win.flip()
     
    while True:  
        theseKeys = kb.getKeys(keyList=['j', 'k', 'l', 'm', 'u'], waitRelease=False)
        if kb.getKeys(keyList=["escape"], waitRelease=False):
            core.quit()
        if len(theseKeys):
            theseKeys_conf = theseKeys[0] 
            pressed_conf = theseKeys_conf.name
            rt_conf = theseKeys_conf.rt
            if pressed_conf == 'j':
                conf = 1
            elif pressed_conf == 'k':
                conf = 2
            elif pressed_conf == 'l':
                conf = 3
            elif pressed_conf == 'm':
                conf = 4
            elif pressed_conf == 'u':
                conf = 0
            win.flip()
            break
                    
    if conf > 0 :
        #conf_scale.markerStart = conf
        conf_scale = visual.RatingScale(win, low=1, high=4, scale=None, labels = ('hasard','certain'), singleClick=True, marker = 'triangle', noMouse=True, markerStart=conf, stretch = 2, showAccept = False)
        conf_scale.draw()
        win.flip()
        core.wait(.5)
        win.flip()
        
    ## write
    data_array =   [subject_id,
                    trial, 
                    trial_type,
                    block,
                    gabor.contrast,
                    pressed_gabor,
                    expected_gabor,
                    pressed_cue,
                    expected_cue,
                    rt_gabor,
                    rt_cue,
                    accuracy_gabor,
                    accuracy_cue,
                    conf,
                    rt_conf
                    ]    


    data_container.append(data_array)
    output = str(data_array[0]) +',' +  str(data_array[1]) + ',' + str(data_array[2]) + ',' + str(data_array[3]) + ',' + str(data_array[4]) + ',' + str(data_array[5]) + ',' + str(data_array[6]) + ',' + str(data_array[7]) + ',' + str(data_array[8]) + ',' + str(data_array[9])+',' + str(data_array[10])+',' + str(data_array[11])+ str(data_array[12])+ str(data_array[13])+ str(data_array[14]) + '\n'
   
    f_handle = open(running_filename, 'a') 
    f_handle.write(output)
    f_handle.close()

    if trial %  n_trial == 0:
        ask(instr_repos)                  

    win.flip() # clean screen
    core.wait(ITI)

## ** Fin des essais
core.wait(2.0)
ask(instr_fin)
win.flip()

## ** compute thresh, save & cleanup
data_container = np.array(data_container)
np.save(filename, data_container)
np.savetxt(filename+'.csv', data_container, delimiter=',')

win.close()
core.quit()


 

