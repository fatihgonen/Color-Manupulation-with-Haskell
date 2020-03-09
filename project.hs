
module Project where
import Prelude
import Data.Fixed

rgb2hsv :: (Float, Float, Float) -> (Float, Float, Float)
rgb2hsv (r, g, b) = (h, s, v)  
    where
        r' = r/255
        g' = g/255
        b' = b/255

        cmax = maximum[r', g', b']
        cmin = minimum[r', g', b']
        delta = cmax - cmin

        v = cmax

        s = calculate_s s
            where
                calculate_s :: Float -> Float
                calculate_s s = 
                    if cmax == 0 then 0
                    else delta/cmax

        h = calculate_h h
            where 
                calculate_h :: Float -> Float
                calculate_h h =
                    if delta == 0 then 0
                    else if cmax == r' then   60 * ( mod' ( (g'-b')/delta ) 6 ) 
                    else if cmax == g' then 60 * ( ((b'-r')/delta) +2 )
                    else 60 * ( ((r'-g')/delta) +4 )

-- question 2

hsv2rgb :: (Float, Float, Float) -> (Float, Float, Float)
hsv2rgb (h,s,v) = (r, g, b)  
  where
    c = v * s
    x = c * (1 - abs( mod' (h/60) 2 -1 ) )
    m = v - c

    (r',g',b') = calculate_rgb c x
      where   
        calculate_rgb :: Float -> Float -> (Float, Float, Float)
        calculate_rgb c x
          | h >= 0   &&   h < 60    = (c, x, 0)
          | h >= 60  &&   h < 120   = (x, c, 0)
          | h >= 120 &&   h < 180    = (0, c, x)
          | h >= 180 &&   h < 240   = (0, x, c)
          | h >= 240 &&   h < 300   = (x, 0, c)
          | h >= 300 &&   h < 360   = (c, 0, x)
        
    r = fromIntegral(round ( ( r' + m ) * 255 ))
    g = fromIntegral(round ( ( g' + m ) * 255 ))
    b = fromIntegral(round ( ( b' + m ) * 255 ))

-- question 3

name2rgb :: [Char] -> (Float, Float, Float)
name2rgb color_name

  | color_name == "aqua" = (0, 255, 255)
  | color_name == "blue" = (0, 0, 255)
  | color_name == "brown" = (165,42,42)
  | color_name == "black" = (0,0,0) 
  | color_name == "darkred" = (139, 0, 0)
  | color_name == "green"= (0,128,0)
  | color_name == "gray" || color_name== "grey" = (128,128,128)
  | color_name == "gold" = (255, 215, 0)
  | color_name == "ivory" = (255,255,240)
  | color_name == "khaki" = (240,230,140)
  | color_name == "magenta" = (255,0,255)
  | color_name == "navy" = (0,0,128)
  | color_name == "red" = (255,0,0)
  | color_name == "purple" = (128,0,128)
  | color_name == "pink" = (255,192,203)
  | color_name == "orange" = (255,165,0)
  | color_name == "turquoise" = (64, 224, 208)
  | color_name == "lime" = (0,255,0)
  | color_name == "yellow" = (255,255,0)
  | color_name == "violet" = (238,130,238)
  | color_name == "white" = (255,255,255)

-- question 4

hsvGradient :: (Float, Float, Float) -> (Float, Float, Float) -> Float -> [(Float, Float, Float)]
hsvGradient (h, s, v) (h2, s2, v2) step = z
  where
    st  = (h2 - h) / step
    st2 = (s2 - s) / step
    st3 = (v2 - v) / step  
    hs = gradIter h 0 step st [ ]
    ss = gradIter s 0 step st2 [ ]
    vs = gradIter v 0 step st3 [ ]
    z = zip3 hs ss vs

gradIter :: Float -> Float -> Float -> Float -> [Float] -> [Float]
gradIter a step step_n dist xs
  | step < (step_n+1)  = gradIter (a+dist) (step+1) step_n dist (xs++[a])
  | step == (step_n+1) = xs

-- question 5

hsv2desc :: (Float, Float, Float) -> [Char]
hsv2desc (h,s,v) = desc
  where
    s' = s*100
    v' = v*100

    hue_desc = hueName h
      where 
        hueName :: Float -> [Char]
        hueName h
          | h > 344 = "red"
          | h > 327 = "rose"
          | h > 291 = "magenta"
          | h > 270 = "purple"
          | h > 260 = "violet"
          | h > 240 = "indigo"
          | h > 193 = "blue"
          | h > 163 = "cyan"
          | h > 79 = "green"
          | h > 70 = "lime"
          | h > 45 = "yellow"
          | h > 15 = "orange"
          | h == 15 = "reddish"
          | h < 15 = "red"

    sat_desc = satName s'
      where 
        satName :: Float -> [Char]
        satName s
          | s > 90 = "very saturated"
          | s > 80 = "very saturated"
          | s > 60 = "rather saturated"
          | s > 46 = "saturated"
          | s > 30 = "unsaturated"
          | s > 10 = "very unsaturated"
          | s > 3 = "almost grey"
          | s < 4 = "grey"

    light_desc = lightName v'
      where 
        lightName :: Float -> [Char]
        lightName v
          | v > 94 = "almost white"
          | v > 80 = "very light"
          | v > 60 = "light"
          | v > 30 = "normal"
          | v > 22 = "dark"
          | v > 9 = "very dark"
          | v < 10 = "almost black"
    desc = "This color is " ++ hue_desc ++ ", " ++ sat_desc ++ " and " ++ light_desc 

--question 6

name2gradient :: [Char] -> [Char] -> Float -> [(Float, Float, Float)]
name2gradient color1 color2 step = gradient
  where
    c1_rgb = name2rgb color1
    c2_rgb = name2rgb color2 
    c1_hsv = rgb2hsv c1_rgb
    c2_hsv = rgb2hsv c2_rgb
    gradient = hsvGradient c1_hsv c2_hsv step



--question 7

rgb2name :: (Float, Float, Float) -> [Char]
rgb2name (r,g,b) = c_name
  where
    colors = ["aqua","blue","brown","black","darkred","green","gray","gold","ivory","khaki","magenta","navy","red","purple","pink","orange","turquoise","lime","yellow","violet","white"]
    to_cmp = replicate 21 (r,g,b)
    results = my_map color_distance colors to_cmp
    c_name = fst (minimum' results)

color_distance :: [Char] -> (Float, Float, Float) -> ([Char],Float)
color_distance name (r2,g2,b2) = (c_name,distance)
  where
    (r,g,b) = name2rgb name
    r' = r-r2
    g' = g-g2
    b' = b-b2
    distance = sqrt(r'*r' + g'*g' + b'*b')
    c_name = name

my_map :: (a -> b -> c) -> [a] -> [b] -> [c]
my_map f xs ys = map (\(x, y) -> f x y) (zip xs ys)

minimum' :: Ord a => [(t, a)] -> (t, a)
minimum' (x:xs) = minTail x xs
  where minTail currentMin [] = currentMin
        minTail (m, n) (p:ps)
          | n > (snd p) = minTail p ps
          | otherwise   = minTail (m, n) ps
