

from psychopy import visual, monitors
from psychopy.visual import ShapeStim
import keyboard


mon1 = monitors.Monitor('testMonitor')
#calibName = mon1.setCurrent(-1)
mon1.setDistance(70)
mon1.setWidth(53)
mon1.setSizePix([1920, 1080])
mon1.saveMon()

win = visual.Window(size=[1920, 1080], monitor=mon1, allowGUI=False, fullscr=True, units='deg', color=[128, 128, 128],
                    colorSpace='rgb255')

# 1: coin haut gauche rect; 2: coin bas gauche; 3: coin bas droit; 4: pointe bas flèche  5 : pointe flèche;
arrowright = [(-1,0.5),(-1,-0.5),(0,-0.5),(0,-1),(1,0),(0,1),(0,0.5)] # 1: coin haut gauche rect; 2: coin bas gauche; 3: coin haut droit; 4: coin bas droit  5 : pointe flèche;
arrowleft = [(-1,0),(0,1),(0,0.5),(1,0.5),(1,-0.5),(0,-0.5),(0,-1)]


arrow = ShapeStim(win, vertices=arrowleft, fillColor='white', size=0.65, lineColor='white')


#arrow.vertices = arrowright

arrow.draw()

win.flip()
keyboard.wait('esc')
win.close()
