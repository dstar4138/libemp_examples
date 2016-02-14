# motion

This LibEMP App uses a library called `erlcam` [1] to capture images from your 
local webcam and slowly compare them to one another. If it recognizes motion,
it will send an event to the logger along with the path to the image it saw 
the motion in. This app demonstrates the ability to call into C programs when 
necessary.

[1] - https://github.com/dstar4138/erlcam/