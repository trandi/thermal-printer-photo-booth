#!/usr/bin/python

import io
import picamera
import pygame
from pygame.locals import *
from PIL import Image
import time
import subprocess


SCREEN_RES = (480, 320)
PIC_RES = (360, 240) #tuple(map(lambda x: x/2, SCREEN_RES))
PIC_COORDS = tuple(map(lambda (x, y): (x-y)/2, zip(SCREEN_RES, PIC_RES)))
FILE_NAME = 'original.png'

pygame.init()
screen = pygame.display.set_mode(SCREEN_RES, FULLSCREEN | DOUBLEBUF)


def pilToPygameImg(pilImg):
    return pygame.image.frombuffer(pilImg.tobytes(), pilImg.size, pilImg.mode)


def printPicture(pilImg, pixelise):
    # display on the screen what we are doing
    screen.fill(Color('black'))
    # convert to RGB in case it was pixelated and pygame doesn't know how to blit that format
    screen.blit(pilToPygameImg(pilImg.convert('1').convert('RGB') if pixelise else pilImg), PIC_COORDS)
    pygame.display.update()

    # now prepare the image for printing
    updatedImg = (pilImg
            # dots on the printer are taller than they are wide, account of this here given that we use 'no-scaling'
            .resize((int(PIC_RES[0] * 0.7), PIC_RES[1]))
            # need to rotate to account for the screen vs camera orientation
            .rotate(270, resample=Image.BICUBIC, expand=True)
            )
    convertedImg = updatedImg.convert('1') if pixelise else updatedImg
    # save the printable image
    convertedImg.save(FILE_NAME, 'PNG')

    # call the Haskell code to do its transformation and drive the thermal printer
    subprocess.call(['/home/pi/thermal-printer-photo-booth/.stack-work/dist/arm-linux/Cabal-1.24.2.0/build/thermal-printer-photo-booth-exe/thermal-printer-photo-booth-exe', FILE_NAME, 'no-scaling'])


stream = io.BytesIO()   #create a new in memory stream for the camera
with picamera.PiCamera() as camera:
    camera.resolution = PIC_RES
    for unused in camera.capture_continuous(stream, "bmp", use_video_port=True):
        pilImg = Image.open(stream).transpose(Image.FLIP_LEFT_RIGHT).convert()
        pygameImg = pilToPygameImg(pilImg) 
        blitedRect = screen.blit(pygameImg, PIC_COORDS)
        pygame.display.update(blitedRect)
        stream.seek(0)

        for event in pygame.event.get():
            if event.type == pygame.MOUSEBUTTONDOWN:
                clickVertPos = event.dict['pos'][0]
                clickHorizPos = event.dict['pos'][1]
                if ((clickVertPos > (SCREEN_RES[0] - 50)) and (clickHorizPos > (SCREEN_RES[1] - 50))):
                    # bottom left corner, STOP the app
                    pygame.quit()
                elif (clickVertPos < (SCREEN_RES[0] / 2)):
                    # top half, use pixelated image
                    printPicture(pilImg, True)
                else:
                    # bottom half, use normal image (the Haskell code will convert it)
                    printPicture(pilImg, False)

