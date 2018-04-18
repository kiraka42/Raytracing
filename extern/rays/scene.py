import numpy as np
import math

WHITE = np.array([1, 1, 1])
BLACK = np.array([0, 0, 0])
GREY = np.array([0.5,0.5,0.5])

def norme(v1):
    mag = np.linalg.norm(v1)
    if mag == 0:
        return v1
    else:
        return v1/mag


class Camera:
    def __init__(self, pos, dir):
        down = np.array([0, -1, 0])
        self.position = pos
        self.x_axis = norme(dir-pos)
        self.y_axis = 1.5* norme(np.cross(self.x_axis, down))
        self.z_axis = 1.5*norme(np.cross(self.x_axis, self.y_axis))


class Rayon:
    def __init__(self, pos, dir):
        self.pos = pos
        self.dir = dir


class Intersection:
    def __init__(self, object, rayon, dist):
        self.object = object
        self.rayon = rayon
        self.dist = dist


class Surface:
    def __init__(self, rough):
        self.rough = rough

    def diffuse(self, vect):
        raise NotImplementedError("Not implemented yet")

    def specular(self, vect):
        raise NotImplementedError("Not implemented yet")

    def reflection(self, vect):
        raise NotImplementedError("Not implemented yet")


class Object:
    def __init__(self, surface):
        self.surface = surface

    def intersect(self, rayon):
        raise NotImplementedError("Not implemented yet")

    def normal(self, vect):
        raise NotImplementedError("Not implemented yet")


class Light:
    def __init__(self, pos, color):
        self.pos = pos
        self.color=color


class Scene:
    def __init__(self, objects, lights, camera):
        self.objects = objects
        self.lights =lights
        self.camera = camera
        self.background = BLACK


class Sphere(Object):
    def __init__(self, surface, center, radius):
        super(Sphere, self).__init__(surface)
        self.radius = radius
        self.center = center

    def normal(self, vect):
        return norme(vect - self.center)

    def intersect(self, rayon):
        eo = self.center - rayon.pos
        res = np.dot(eo,rayon.dir)
        dist = 0

        if(res >= 0):
            disc = self.radius**2 - (np.dot(eo,eo) - res*res)
            if disc >= 0 :
                dist = res - math.sqrt(disc)
        #no intersection
        if dist == 0:
            return None
        else:
            return Intersection(self, rayon, dist)


class Plan(Object):
    def __init__(self, norme, offset, surface):
        super(Plan, self).__init__(surface)
        self.norme = norme
        self.offset = offset

    def normal(self, vect):
        return self.norme

    def intersect(self, rayon):
        denom = np.dot(self.norme, rayon.dir)
        if denom>0:
            return None
        else:
            dist = (np.dot(self.norme, rayon.pos)+self.offset)/(-denom)
            return Intersection(self, rayon, dist)


class MirrorSurface(Surface):

    def __init__(self):
        self.rough = 250

    def diffuse(self, vect):
        return WHITE

    def specular(self, vect):
        return GREY

    def reflection(self, vect):
        return 0.7

class RedSurface(Surface):

    def __init__(self):
        self.rough = 250

    def diffuse(self, vect):
        return np.array([0.8, 0.2, 0.2])

    def specular(self, vect):
        return np.array([1, 0.2, 0.2])

    def reflection(self, vect):
        return 0.4

class CheckSurface(Surface):

    def __init__(self):
        self.rough = 150

    def diffuse(self, vect):
        if (math.floor(vect[0])+math.floor(vect[2])) % 2 !=0:
            return WHITE
        else:
            return BLACK

    def specular(self, vect):
        return WHITE

    def reflection(self, vect):
        if (math.floor(vect[0])+math.floor(vect[2])) % 2 !=0:
            return 0.1
        else:
            return 0.7

