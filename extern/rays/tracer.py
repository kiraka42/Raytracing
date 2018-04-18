from scene import *
import numpy as np
import threading


class Tracer:

    def __init__(self, depth=5):
        self.depth = depth


    def intersections(self, rayon, scene):
        closest = math.inf
        closest_inter = None

        for obj in scene.objects:
            inter = obj.intersect(rayon)
            if inter is not None and inter.dist < closest:
                closest_inter = inter
                closest = inter.dist

        return closest_inter

    def test_ray(self, rayon, scene):
        inter = self.intersections(rayon, scene)
        return inter.dist if inter is not None else None

    def trace_ray(self, rayon, scene, depth):
        inter = self.intersections(rayon, scene)
        return scene.background if inter is None else self.shade(inter, scene, depth)

    def shade(self, inter, scene, depth):
        d= inter.rayon.dir
        pos = inter.dist * d + inter.rayon.pos
        normale = inter.object.normal(pos)
        reflection = d - 2*(np.dot(normale, d) * normale)
        couleur_nat = scene.background + self.couleur_nat(inter.object, pos, normale, reflection, scene)
        couleur_ref = GREY if depth>=self.depth else self.ref_color(inter.object, pos, normale, reflection, scene, depth)

        return couleur_nat+couleur_ref

    def ref_color(self, object, pos, normal, rd, scene, depth):
        return object.surface.reflection(pos) * self.trace_ray(Rayon(pos, rd), scene, depth+1)


    def couleur_nat(self, object, pos, norm, rd, scene):
        couleur = BLACK
        for light in scene.lights:
            ldis = light.pos - pos
            livec = norme(ldis)
            neatIsect = self.test_ray(Rayon(pos, livec), scene)
            inShadow = False if neatIsect is None else neatIsect<=np.linalg.norm(ldis)
            if inShadow:
                pass
            else:
                illum = np.dot(livec, norm)
                lcolor = illum * light.color if (illum>0) else BLACK
                spec = np.dot(livec, norme(rd))
                scolor = spec**object.surface.rough * light.color

                res = BLACK + object.surface.diffuse(pos)*lcolor + object.surface.specular(pos)*scolor
                couleur = couleur+res
        return couleur



    def render(self, scene, pixels, width, height):
        for y in range(height):
            for x in range(width):
                x_pos = (x - (width/2.0))/2.0/width
                y_pos = -(y - (height/2.0))/2.0/height
                direct = norme(scene.camera.x_axis + x_pos*scene.camera.y_axis + y_pos*scene.camera.z_axis)
                color = self.trace_ray(Rayon(scene.camera.position, direct), scene, 0)
                pixels[x, y] = (math.floor(color[0]*255), math.floor(color[1]*255), math.floor(color[2]*255))

    def render_thread(self, scene, pixels, width, height):
        threads = []
        for y in range(height):
            for x in range(width):
                x_pos = (x - (width/2.0))/2.0/width
                y_pos = -(y - (height/2.0))/2.0/height
                direct = norme(scene.camera.x_axis + x_pos*scene.camera.y_axis + y_pos*scene.camera.z_axis)
                rayon = Rayon(scene.camera.position, direct)
                t = ColorThread(rayon, scene, x, y, pixels, self)
                t.start()
                threads.append(t)

        for t in threads:
            t.join()

class ColorThread(threading.Thread):
    def __init__(self, rayon, scene, x, y, pixels, tracer):
        threading.Thread.__init__(self)
        self.rayon = rayon
        self.scene = scene
        self.x = x
        self.y = y
        self.pixels = pixels
        self.tracer = tracer

    def run(self):
        color = self.tracer.trace_ray(self.rayon, self.scene, 0)
        self.pixels[self.x, self.y] = (math.floor(color[0]*255), math.floor(color[1]*255), math.floor(color[2]*255))
