from PIL import Image 

CROP = 1 # Fit smallest side to screen (cuts off parts of image)
FIT = 2 # Fit largest side (leaves black spaces)
FILL = 3 # Adjust scale to fill (may distort)


class ImageScale(object):
    def __init__(self, path):
        self.path = path

    def adjust_size(self, dst_x, dst_y, mode=FIT):
        """ 
        given a x and y of dest, determine the ration and return
        an (x,y,w,h) for a output image.
        """
        # get image size
        image = Image.open(self.path)
        width, height = image.size
        if mode == FIT:
            return adjust_crop(dst_x, dst_y, width, height)
        
def adjust_crop(dst_w, dst_h, img_w, img_h):
    """
    given a x and y of dest, determine the ratio and return
    an (x,y,w,h) for a cropped image (note x or y could be neg).
    >>> adjust_crop(4,3,5,5)
    (0, -0.5, 4.0, 4.0)
    >>> adjust_crop(8,6,5,5)
    (0, -1.0, 8.0, 8.0)
    >>> adjust_crop(4,3,5,2)
    (-1.75, 0, 7.5, 3.0)
    >>> adjust_crop(8,6,5,2)
    (-3.5, 0, 15.0, 6.0)
    """
    dst_w = float(dst_w)
    dst_h = float(dst_h)
    img_w = float(img_w)
    img_h = float(img_h)
    
    dst_ratio = float(dst_w)/dst_h
    
    img_ratio = float(img_w)/img_h
    
    if dst_ratio > img_ratio:
        scale = dst_w/img_w
        x = 0
        w = dst_w
        h = img_h * scale
        y = dst_h/2 - h/2
            
    elif dst_ratio <= img_ratio:
        scale = dst_h/img_h
        y = 0
        w = img_w * scale
        h = img_h * scale
        x = dst_w/2 - w/2

    return x,y,w,h

def adjust_fit(dst_w, dst_h, img_w, img_h):
    """
    given a x and y of dest, determine the ratio and return
    an (x,y,w,h) for a fitted image (note x or y could be neg).
    >>> adjust_fit(4,3,5,5)
    (0.5, 0, 3.0, 3.0)
    >>> adjust_fit(8,6,5,5)
    (1.0, 0, 6.0, 6.0)
    >>> adjust_fit(4,3,5,2)
    (0, 0.69999999999999996, 4.0, 1.6000000000000001)
    >>> adjust_fit(8,6,5,2)
    (0, 1.3999999999999999, 8.0, 3.2000000000000002)
    """
    dst_w = float(dst_w)
    dst_h = float(dst_h)
    img_w = float(img_w)
    img_h = float(img_h)
    

    dst_ratio = float(dst_w)/dst_h
    
    img_ratio = float(img_w)/img_h
    
    if dst_ratio > img_ratio:
        # image is narrower, use height
        y = 0
        h = dst_h
        w = h * img_ratio
        x = dst_w/2 - w/2
    else:
        scale = dst_h/img_h
        x = 0
        w = dst_w
        h = w/img_ratio
        y = dst_h/2 - h/2
    return x,y,w,h

def adjust_fill(dst_w, dst_h, img_w, img_h):
    return 0,0,dst_w,dst_h

if __name__ == "__main__":
    import doctest
    doctest.testmod()
