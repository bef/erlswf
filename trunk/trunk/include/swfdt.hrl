-record(rect, {xmin, xmax, ymin, ymax}).

-record(rgb, {red, green, blue}).
-record(rgba, {red, green, blue, alpha}).
-record(argb, {alpha, red, green, blue}).

-record(cxform, {redmult, greenmult, bluemult, redadd, greenadd, blueadd}).
-record(cxformwa, {redmult, greenmult, bluemult, alphamult, redadd, greenadd, blueadd, alphaadd}). %% with alpha
