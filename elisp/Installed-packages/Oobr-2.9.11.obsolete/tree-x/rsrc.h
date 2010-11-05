#define TrNbackgroundColor "backgroundColor"
#define TrNtreeColor       "treeColor"
#define TrNcontourColor    "contourColor"
#define TrNsplitColor      "splitColor"
#define TrNhighlightColor  "highlightColor"
#define TrNactionColor     "actionColor"   
#define TrNcontourWidth    "contourWidth"

#define TrCBackgroundColor "BackgroundColor"
#define TrCTreeColor       "TreeColor"
#define TrCContourColor    "ContourColor"
#define TrCSplitColor      "SplitColor"
#define TrCHighlightColor  "HighlightColor"
#define TrCActionColor     "ActionColor"    
#define TrCContourWidth    "ContourWidth"

#define TrDefaultBackgroundColor "white"
#define TrDefaultTreeColor       "black"
#define TrDefaultContourColor    "MediumSlateBlue"
#define TrDefaultSplitColor      "goldenrod"
#define TrDefaultHighlightColor  "OrangeRed"
#define TrDefaultActionColor     "OrangeRed"
#define TrDefaultContourWidth    2

typedef struct {
   Pixel background_color;
   Pixel tree_color;
   Pixel contour_color;
   Pixel split_color;
   Pixel highlight_color;
   Pixel action_color;
   int   contour_width;
} ApplRsrcData, *ApplRsrcDataPtr;

