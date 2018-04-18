from scene import *
from tracer import Tracer
from PIL import Image

import time

def create_def():
    return Scene(
        [Plan(np.array([0, 1, 0]), 0, CheckSurface()),
         Sphere( MirrorSurface(),np.array([0, 1, -0.25]), 1),
         Sphere( RedSurface(),np.array( [-1, 0.5, 1.5]), 0.5)],
        [Light(np.array([-2, 2.5, 0]), np.array([0.49, 0.07, 0.07])),
         Light(np.array([1.5, 2.5, 1.5]), np.array([0.07, 0.07, 0.49])),
         Light(np.array([1.5, 2.5, -1.5]), np.array([0.07, 0.49, 0.071])),
         Light(np.array([0, 3.5, 0]), np.array([0.21, 0.21, 0.35]))
         ],
        Camera(np.array([3, 2, 4]), np.array([-1, 0.5, 0]))
    )

def create_bonus():
    return Scene(
        [Plan(np.array([0, 1, 0]), 0, CheckSurface()),
         Sphere( MirrorSurface(),np.array([0, 1, -0.25]), 1),
         Sphere( RedSurface(),np.array( [-1, 0.5, 1.5]), 0.5)],
        [Light(np.array([-2, 2.5, 0]), np.array([0.8, 0.8, 0.8])),
         Light(np.array([1.5, 2.5, 1.5]), np.array([0.8, 0.8, 0.8]))

         ],
        Camera(np.array([3, 2, 4]), np.array([-1, 0.5, 0]))
    )


width = 250
height= 250

img = Image.new('RGB', (width, height), "black")
pixels = img.load()

start = time.time()
tracer = Tracer(5)
tracer.render(create_def(), pixels, width, height)

end = time.time()

print("Time spent : {}".format(end-start))
img.show()

img.save("result.bmp")
