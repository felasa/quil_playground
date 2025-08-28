(ns util.masking
  (:import [processing.core PImage])
  (:require [quil.core :as q]))

"""
from https://discourse.processing.org/t/how-to-create-a-clipping-mask-that-preserves-transparency/16093/2
void alphaSubtract(PGraphics img, PGraphics cm){
  img.loadPixels();
  cm.loadPixels();
  if(img.pixels.length != cm.pixels.length){
    return;
  }
  for(int j = 0; j<img.height; j++){
    for(int i = 0; i<img.width; i++){
      // get argb values
      color argb = img.pixels[(j*img.width) + i];
      int a = argb >> 24 & 0xFF;
      int r = argb >> 16 & 0xFF;
      int g = argb >> 8 & 0xFF;
      int b = argb & 0xFF;
      
      color maskPixel = cm.pixels[(j*img.width) + i];
      int alphaShift = 0xFF - (maskPixel & 0xFF);  //grab blue value from mask pixel
      
      // subtract alphaShift from pixel's alpha value;
      img.pixels[(j*img.width) + i] = color(r,g,b,a-alphaShift);
    }
  }
}
"""

""" Approach2
int[] minAlphas(PImage img, PImage img2) {
  img.loadPixels();
  img2.loadPixels();
  int[] a = new int[img.pixels.length];
  for (int i =0; i<img.pixels.length; i++) {
    a[i] = min(img.pixels[i] >> 24 & 0xFF, img2.pixels[i] >> 24 & 0xFF);
  }
  return a;
}
"""
(defn
  mask-w-alpha
  "Masks part of an image from displaying by loading another image and
  using it as an alpha channel and preserving transparency"
  ([^PImage mask] (mask-w-alpha (q/current-graphics) mask))
  ([^PImage img ^PImage mask]
   (let [pxls-mask (q/pixels mask)
         pxls-img  (q/pixels img)
         ^ints alphas 
         (amap ^ints pxls-mask idx ret 
               (min (int (bit-and (bit-shift-right (aget ^ints pxls-mask idx) 24) 0xff))
                    (int (bit-and (bit-shift-right (aget ^ints pxls-img idx)  24)  0xff))))]
     (.mask img alphas))))

