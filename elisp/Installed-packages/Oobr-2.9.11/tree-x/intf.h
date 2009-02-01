/* ----------------------------------------------------------------------------
 * File    : intf.h
 * Purpose : include file for intf.c and draw.c 
 * ----------------------------------------------------------------------------
 */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#define DEFAULT_FONT	"-adobe-helvetica-bold-r-normal--12-*"
#define BIG_FONT	"-adobe-helvetica-bold-r-normal--12-*"

#define X11_APPLICATION_CLASS    "Tree"
#define X11_DEFAULT_FONT         "fixed"
#define X11_FONT_RESOURCE        "tree.font"
#define X11_FONT_CLASS_RESOURCE  "Tree.Font"

#define HELP_FILE                 "tree.help"

#define LABEL_MAT_WIDTH           3
#define LABEL_MAT_HEIGHT          3
#define BORDER_SIZE               4      /* beginning border size     */
#define MAX_BORDER_SIZE           25
#define PARENT_DISTANCE           30     /* beginning parent distance */
#define MAX_PARENT_DISTANCE       50
#define DENSITY_FACTOR            1.50
#define MAT_SIZE                  BORDER_SIZE * 2
#define ELISION_WIDTH             5
#define ANIMATION_STEP            3
#define ANIMATION_STEP_STEP       4

#define BACKGROUND_COLOR          0
#define TREE_COLOR                1
#define CONTOUR_COLOR             2
#define HIGHLIGHT_COLOR           3
#define SPLIT_COLOR               4
#define ACTION_COLOR              5  
#define NUM_COLORS                6

#define TREE_MENU_NEW             0
#define TREE_MENU_LOAD            1
#define TREE_MENU_SAVE            2
#define TREE_MENU_SEP1            3
#define TREE_MENU_QUIT            4
#define TREE_MENU_STATS           6
#define TREE_MENU_ITEMS           5

#define TREE_MENU_SEP2            5

#define LAYOUT_MENU_FIXED         0 /* not used at this time */
#define LAYOUT_MENU_VARIABLE      1
#define LAYOUT_MENU_SEP1          2

#define LAYOUT_MENU_SPACING       3
#define LAYOUT_MENU_SEP2          4
#define LAYOUT_MENU_ALIGN_NODES   5
#define LAYOUT_MENU_ITEMS         5

#define NODE_MENU_LABEL           0
#define NODE_MENU_SEP1            1
#define NODE_MENU_ADD_CHILD       2
#define NODE_MENU_ADD_BEFORE      3
#define NODE_MENU_ADD_AFTER       4
#define NODE_MENU_ELISION         5
#define NODE_MENU_DELETE          6
#define NODE_MENU_ITEMS           7

#define TREE_MENU                 0
#define LAYOUT_MENU               1
#define NODE_MENU                 2
#define NUM_MENUS                 3

#define STR_SHOW_STATS            0
#define STR_HIDE_STATS            1
#define STR_NODE_COLLAPSE         2
#define STR_NODE_EXPAND           3
#define STR_SHOW_CONTOUR          4
#define STR_HIDE_CONTOUR          5
#define STR_LOAD_FILE             6
#define STR_SAVE_FILE             7
#define NUM_MENU_STRS             8 

#define DLG_NEW                   0
#define DLG_NODE_NAME             1
#define DLG_FILE                  2
#define DLG_INFO                  3
#define DLG_ERROR                 4
#define DLG_SPACING               5
#define NUM_DLG                   6

/* in 1/10ths of a second */
#define ANIMATION_SPEED_FAST       1

typedef enum {
   NoContours,
   OutsideContour,
   AllContours,
   SelectedContours
} ContourOption;

typedef enum {
   Child,
   Before,
   After
} NodePosition;

typedef enum {
   Fixed,
   Variable
} DensityOption;

extern  Widget        TreeTopLevel;
extern  Widget        TreeDrawingArea;
extern  Display      *TreeDisplay;
extern  int           TreeScreen;
extern  int           TreeContourWidth;
extern  int           TreeBorderSize;
extern  int           TreeParentDistance;
extern  XFontStruct  *TreeLabelFont;
extern  DoubleBuffer *TreeDrawingAreaDB;
extern  char          TreeShowSteps;
extern  ContourOption TreeShowContourOption;
extern  DensityOption TreeLayoutDensity;
extern  char          TreeAlignNodes;   
extern  char          PauseAfterStep;
