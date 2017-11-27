#!/usr/bin/python

import io
import picamera
import pygame
from pygame.locals import *
from PIL import Image
import time


SCREEN_RES = (480, 320)

pygame.init()
screen = pygame.display.set_mode(SCREEN_RES, FULLSCREEN | DOUBLEBUF)


def pilToPygameImg(pilImg):
    return pygame.image.frombuffer(pilImg.tobytes(), pilImg.size, pilImg.mode)


def printPicture(pilImg):
    resizedPilImg = pilImg.resize((360, 240)).convert('1').convert('RGB')
    screen.fill(Color('black'))
    screen.blit(pilToPygameImg(resizedPilImg), (60, 40))
    pygame.display.update()


stream = io.BytesIO()   #create a new in memory stream for the camera
with picamera.PiCamera() as camera:
    camera.resolution = SCREEN_RES
    for unused in camera.capture_continuous(stream, "bmp", use_video_port=True):
        pilImg = Image.open(stream).transpose(Image.FLIP_LEFT_RIGHT).convert()
        pygameImg = pilToPygameImg(pilImg) 
        blitedRect = screen.blit(pygameImg, (0, 0))
        pygame.display.update(blitedRect)
        stream.seek(0)

        for event in pygame.event.get():
            if event.type == pygame.MOUSEBUTTONDOWN:
                clickVertPos = event.dict['pos'][0]
                if clickVertPos > SCREEN_RES[0] / 2:
                    pygame.quit()
                else:
                    printPicture(pilImg)
                    time.sleep(5)
