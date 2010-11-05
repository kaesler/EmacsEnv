/* ----------------------------------------------------------------------------
 * File    : intf.c
 * Purpose : X11/Athena interface code
 * ----------------------------------------------------------------------------
 */

#define GLOBALS 1

#include <stdio.h>
#include <string.h>
#include <fcntl.h>

#include "X11/IntrinsicP.h"
#include <X11/ShellP.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Simple.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/AsciiSrc.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Panner.h>
#include <X11/Xaw/Reports.h>

#include "defs.h"
#include "tree.h"
#include "dbl.h"
#include "intf.h"
#include "rsrc.h"
#include "input.h"
#include "help.h"

#include <X11/cursorfont.h>


/* ------------------------------------------------------------------------- */
/*				Global Variables                             */
/* ------------------------------------------------------------------------- */

extern char  *EnvNm;    /* Stores name of current Envir file */

Atom	      wm_delete_window;
Widget        TreeTopLevel;
Widget        TreeDrawingArea;
Display      *TreeDisplay;
XtAppContext  app;
int           TreeScreen;
int           TreeContourWidth;
int           TreeBorderSize = BORDER_SIZE;
int           TreeParentDistance = PARENT_DISTANCE;
XFontStruct  *TreeLabelFont;
DoubleBuffer *TreeDrawingAreaDB;
ContourOption TreeShowContourOption = NoContours;
DensityOption TreeLayoutDensity = Fixed;
char          TreeShowSteps =  FALSE;
char          TreeAlignNodes = FALSE;
char          PauseAfterStep    = FALSE;

/* ------------------------------------------------------------------------- */
/*				Local Variables                              */
/* ------------------------------------------------------------------------- */

static Tree *PopupNode;		/* node selected for popup */
static NodePosition NodePos;

static Widget mainWindow;
static Widget topForm;

static Widget statusLabel;
static Widget statusText;

static Widget scrolledWindow;

static Widget helpShell;
static Widget helpForm;
static Widget helpScrollWindow;
static Widget helpText;
static Widget helpDoneButton;
static Widget helpTitle;
static Widget helpFrame;

static Widget treeMenubar;
static Widget treeMenus[NUM_MENUS];
static Widget treeMenuBtns[NUM_MENUS - 1];   /* don't need button for */
					     /* node popup menu       */

static Widget treeMenuItems[TREE_MENU_ITEMS];
static Widget layoutMenuItems[LAYOUT_MENU_ITEMS];
static Widget nodeMenuItems[NODE_MENU_ITEMS];
static Widget helpBtn;

static String labelStr[NUM_MENU_STRS];
static String errStr[NUM_ERRS];

static Widget dialog[NUM_DLG];

/* globals used for state values */

static Widget dlgLevelScale, dlgLevelValuator, 
              dlgSiblingValuator, dlgSiblingScale;
static int dlgLevelValue;
static int dlgSiblingValue;
static int loading_file = 0;

static Pixmap check_pixmap, slider_pixmap;

static Boolean nodeFound;

static int    PauseTime         = ANIMATION_SPEED_FAST;
static Cursor PauseCursor;

static XtResource applRsrcsDesc[] = {
   {
      TrNbackgroundColor,       TrCBackgroundColor,
      XtRPixel,                 sizeof(Pixel),
      XtOffset(ApplRsrcDataPtr, background_color),
      XtRString,                TrDefaultBackgroundColor
   },
   {
      TrNtreeColor,             TrCTreeColor,
      XtRPixel,                 sizeof(Pixel),
      XtOffset(ApplRsrcDataPtr, tree_color),
      XtRString,                TrDefaultTreeColor
   },
   {
      TrNcontourColor,          TrCContourColor,
      XtRPixel,                 sizeof(Pixel),
      XtOffset(ApplRsrcDataPtr, contour_color),
      XtRString,                TrDefaultContourColor
   },
   {
      TrNsplitColor,            TrCSplitColor,
      XtRPixel,                 sizeof(Pixel),
      XtOffset(ApplRsrcDataPtr, split_color),
      XtRString,                TrDefaultSplitColor
   },
   {
      TrNhighlightColor,        TrCHighlightColor,
      XtRPixel,                 sizeof(Pixel),
      XtOffset(ApplRsrcDataPtr, highlight_color),
      XtRString,                TrDefaultHighlightColor
   },
   {
      TrNactionColor,           TrCActionColor,
      XtRPixel,                 sizeof(Pixel),
      XtOffset(ApplRsrcDataPtr, action_color),
      XtRString,                TrDefaultActionColor
   },
   {
      TrNcontourWidth,          TrCContourWidth,
      XtRInt,                   sizeof(int),
      XtOffset(ApplRsrcDataPtr, contour_width),
      XtRImmediate,             (XtPointer) TrDefaultContourWidth
   },
};

static void popdown_action();
static void activate_action();
static void quit_action();
static void Action();

static void select_action();
static void button_action();
static void menu_popup_action();

static void set_levelscale_CB();
static void levelscale_CB();
static void set_siblingscale_CB();
static void siblingscale_CB();
static void set_levelvalue_CB();
static void set_siblingvalue_CB();
static void levelvalue_CB();
static void siblingvalue_CB();

static void center_widget();

static XtActionsRec local_actions[] = {
  {"wmpopdown", popdown_action},
  {"dlg_activate", activate_action},
  {"node_select", select_action},
  {"node_menu_prepare", button_action},
  {"maybe_menu_popup", menu_popup_action},
  {"quit", quit_action},
};

String fallback_resources[]  = {
"*statusText.width:                      310",
"*treeDrawingArea.width:                 310",
"*treeDrawingArea.height:                400",
"*Text.Font:	      -*-lucidatypewriter-medium-r-*-*-12-*-*-*-*-*-iso8859-1",
"*Label.Font:	      -*-helvetica-medium-r-*-*-12-*-*-*-*-*-iso8859-1",
"*Command.Font:	      -*-helvetica-bold-r-*-*-12-*-*-*-*-*-iso8859-1",
"*SimpleMenu.Font:    -*-helvetica-bold-r-*-*-12-*-*-*-*-*-iso8859-1",
"*SmeBSB.Font:	      -*-helvetica-bold-r-*-*-12-*-*-*-*-*-iso8859-1",
"*MenuButton.Font:    -*-helvetica-bold-r-*-*-12-*-*-*-*-*-iso8859-1",
"*List.Font:	      -*-helvetica-bold-r-*-*-12-*-*-*-*-*-iso8859-1",
"*treeMenuBtn.label:			Tree",
"*layoutMenuBtn.label:			Layout",
"*helpBtn.label:			Help...",
"*treeMenu*treeMenuNew.label:		New Tree",
"*treeMenu*treeMenuLoad.label:		Load Tree",
"*treeMenu*treeMenuSave.label:		Save Tree",
"*treeMenu*treeMenuQuit.label:		Quit",
"*layoutMenu*layoutMenuFixed.label:	Fixed",
"*layoutMenu*layoutMenuVariable.label:	Variable",
"*layoutMenu*layoutMenuSpacing.label:	Spacing...",
"*layoutMenu*layoutMenuAlignNodes.label: Align Nodes on Level",
"*dlgNewTree.title:			New Tree",
"*dlgNodeName.title:			Node Name",
"*dlgInfo.title:			Information",
"*dlgError.title:			Error!",
"*dlgSpacing.title:			Tree Node Spacing",
"*helpShell.title:			Help",
"*dlgNewTreeForm.label:			Enter name of root node:",
"*dlgNodeNameForm.label:		Enter name of new node:",
"*dlgFileForm.label:			Enter name of file:",
"*dlgInfoForm.label:			Information:",
"*dlgErrorForm.label:			Error:",
"*dlgSpacing.label:			Spacing Dialog",
"*dlgLevelLabel.label:			Level spacing:",
"*dlgSiblingLabel.label:		Sibling spacing:",
"*dlgLevelValuator.width:		40",
"*dlgLevelValuator.width:		40",
"*dlgNewTreeOk.label:			Ok",
"*dlgNewTreeCancel.label:		Cancel",
"*dlgFileOk.label:			Ok",
"*dlgFileCancel.label:			Cancel",
"*dlgInfoButton.label:			Acknowledged",
"*dlgErrorButton.label:			Acknowledged",
"*dlgOkButton.label:			Ok",
"*dlgCancelButton.label:		Cancel",
"*dlgNodeNameOk.label:			Ok",
"*dlgNodeNameCancel.label:		Cancel",
"*helpDoneButton.label:			Dismiss",
"*helpTitle.label:			Help for Drawing Dynamic Trees",
"*treeDrawingArea.Translations: #override \\n\
                                <Btn1Down>:	node_select(EDIT)\\n\
                                <Btn2Down>:	node_select(VIEW)\\n\
                                <Btn3Down>:	XawPositionSimpleMenu(nodeMenu)node_menu_prepare()XtMenuPopup(nodeMenu)\\n",
NULL,
};

static ApplRsrcData    applRsrcs;
static ApplRsrcDataPtr applRsrcsPtr;

/* included bitmaps */

#define gray_width 8
#define gray_height 8
static unsigned char gray_bits[] = {
  0x55, 0xaa, 0x55, 0xaa, 0x55, 0xaa, 0x55, 0xaa
};

/* This is a check mark stolen from the xmh application. :) */
#define check_width 9
#define check_height 8
static unsigned char check_bits[] = {
  0x00, 0x01, 0x80, 0x01, 0xc0, 0x00, 0x60, 0x00,
  0x31, 0x00, 0x1b, 0x00, 0x0e, 0x00, 0x04, 0x00
};

#define slider_pixmap_width 4
#define slider_pixmap_height 4
static char slider_pixmap_bits[] = {
  0x05, 0x0a, 0x05, 0x0a
};

/* ------------------------------------------------------------------------- */
/*			 Forward Function Declarations                       */
/* ------------------------------------------------------------------------- */

static void popup_dialog();

static void Quit_CB();

static void Contours_CB();
static void ShowContours_CB();
static void Pause_CB();
static void Speed_CB();

static void Help_CB();
static void HelpDone_CB();
static void Scale_CB();

static void TreeMenu_CB();
static void LayoutMenu_CB();
static void NodeMenu_CB();

static void NodeLabel_CB();
static void NewTree_CB();
static void File_CB();

static void Popdown_CB();
static void ButtonHandler();

extern void ExposeHandler();

/* ------------------------------------------------------------------------- */
/*				   Functions                                 */
/* ------------------------------------------------------------------------- */

/* ----------------------------------------------------------------------------
 * 
 *   Status() displays the specified text in the status area.
 *   'urgent' overrides the value of TreeShowSteps.
 * 
 * ----------------------------------------------------------------------------
 */

void
StatusMsg(msg, urgent)
     char *msg;
     int  urgent;
{
  if (TreeShowSteps || urgent) 
    XtVaSetValues(statusText, XtNlabel, msg, NULL);
}


/* ----------------------------------------------------------------------------
 * 
 *  Pause is a trivial function, for the benefit of interface-dependent code
 *  inside tree.c. This avoids having to include X11 stuff inside that file.
 *  PauseTime is expected to contain an integer indicating 1/10ths of a sec.
 * 
 * ----------------------------------------------------------------------------
 */

void
Pause()
{
  XEvent event;
  
  if (PauseAfterStep) {
    XGrabKeyboard(TreeDisplay, XtWindow(TreeTopLevel), False,
		  GrabModeAsync, GrabModeAsync, CurrentTime);
    XGrabPointer(TreeDisplay, XtWindow(TreeTopLevel), False,
		 ButtonPressMask | ButtonReleaseMask,
		 GrabModeAsync, GrabModeAsync, None,
		 PauseCursor, CurrentTime);
    XMaskEvent(TreeDisplay, ButtonPressMask | KeyPress, &event);
    XUngrabPointer(TreeDisplay, CurrentTime);
    XUngrabKeyboard(TreeDisplay, CurrentTime);
  }
  else if (TreeShowSteps) {
    XSync(TreeDisplay, 0);
    usleep(PauseTime * 100000);
  }
}

/* ----------------------------------------------------------------------------
 * 
 *   BuildPopupMenus() builds the popup menus for use in the application
 *   window.
 * 
 * ----------------------------------------------------------------------------
 */

void
BuildPopupMenus(parent)
     Widget parent;
{
  Arg      args[2];
  String   xstr;
  
  labelStr[STR_NODE_COLLAPSE] = "Collapse Node";
  labelStr[STR_NODE_EXPAND]   = "Expand Node";
  
  treeMenus[NODE_MENU] = XtVaCreatePopupShell("nodeMenu",
					      simpleMenuWidgetClass,
					      parent, 
					      XtNlabel, " ",
					      NULL);
  
  (void)  XtVaCreateManagedWidget("nodeMenuAddChild", smeLineObjectClass,
				  treeMenus[NODE_MENU],
				  NULL);
  
  nodeMenuItems[NODE_MENU_ADD_CHILD] = 
    XtVaCreateManagedWidget("nodeMenuAddChild", smeBSBObjectClass,
			    treeMenus[NODE_MENU],
			    XtNlabel, "Add Child",
			    NULL);
  XtAddCallback(nodeMenuItems[NODE_MENU_ADD_CHILD],
		XtNcallback, NodeMenu_CB, (XtPointer) NODE_MENU_ADD_CHILD);
  
  nodeMenuItems[NODE_MENU_ADD_BEFORE] = 
    XtVaCreateManagedWidget("nodeMenuAddSiblingBefore", smeBSBObjectClass,
			    treeMenus[NODE_MENU],
			    XtNlabel, "Add Sibling Before",
			    NULL);
  XtAddCallback(nodeMenuItems[NODE_MENU_ADD_BEFORE],
		 XtNcallback, NodeMenu_CB, (XtPointer) NODE_MENU_ADD_BEFORE);
		 
   nodeMenuItems[NODE_MENU_ADD_AFTER] = 
       XtVaCreateManagedWidget("nodeMenuAddSiblingAfter", smeBSBObjectClass,
			       treeMenus[NODE_MENU],
			       XtNlabel, "Add Sibling After",
			       NULL);
   XtAddCallback(nodeMenuItems[NODE_MENU_ADD_AFTER],
		 XtNcallback, NodeMenu_CB, (XtPointer) NODE_MENU_ADD_AFTER);

   nodeMenuItems[NODE_MENU_ELISION] =
       XtVaCreateManagedWidget("nodeMenuElision", smeBSBObjectClass,
			       treeMenus[NODE_MENU],
			       XtNlabel, labelStr[STR_NODE_COLLAPSE],
			       NULL);
   XtAddCallback(nodeMenuItems[NODE_MENU_ELISION],
		 XtNcallback, NodeMenu_CB, (XtPointer) NODE_MENU_ELISION);

   
   nodeMenuItems[NODE_MENU_DELETE] =
       XtVaCreateManagedWidget("nodeMenuDeleteNode", smeBSBObjectClass,
			       treeMenus[NODE_MENU],
			       XtNlabel, "Delete Node",
			       NULL);
   XtAddCallback(nodeMenuItems[NODE_MENU_DELETE],
		 XtNcallback, NodeMenu_CB, (XtPointer) NODE_MENU_DELETE);

		 
}

/* ----------------------------------------------------------------------------
 * 
 *   BuildMenubar() builds the menubar that appears in the main window.
 * 
 * ----------------------------------------------------------------------------
 */


void
BuildMenubar(parent)
   Widget parent;
{
   Arg args[4];
   int i;
   Widget box;

   box = XtVaCreateManagedWidget("menuBox",
				 boxWidgetClass,
				 parent,
				 XtNshowGrip, FALSE,
				 XtNborderWidth, 0,
				 NULL);
				 

   /* menu button widgets */
   treeMenuBtns[TREE_MENU] = 
       XtVaCreateManagedWidget("treeMenuBtn",
			       menuButtonWidgetClass,
			       box,
			       XtNmenuName, "treeMenu",
			       NULL);
   treeMenuBtns[LAYOUT_MENU] =
       XtVaCreateManagedWidget("layoutMenuBtn",
			       menuButtonWidgetClass,
			       box,
			       XtNmenuName, "layoutMenu",
			       NULL);

   helpBtn = XtVaCreateManagedWidget("helpBtn",
				     commandWidgetClass,
				     box,
				     NULL);

   XtAddCallback(helpBtn, XtNcallback, Help_CB, NULL);

   /* create pulldwon menus */

   treeMenus[TREE_MENU] = XtVaCreatePopupShell("treeMenu",
					       simpleMenuWidgetClass,
					       treeMenuBtns[TREE_MENU],
					       NULL, 0);
   treeMenus[LAYOUT_MENU] = XtVaCreatePopupShell("layoutMenu",
						 simpleMenuWidgetClass,
						 treeMenuBtns[LAYOUT_MENU],
						 NULL, 0);


   /* adding menu entries */
   
   /* Tree menu */

   treeMenuItems[TREE_MENU_NEW] = 
       XtVaCreateManagedWidget("treeMenuNew", smeBSBObjectClass,
			       treeMenus[TREE_MENU],
			       NULL);
   XtAddCallback(treeMenuItems[TREE_MENU_NEW],
		 XtNcallback, TreeMenu_CB, TREE_MENU_NEW);
   
   treeMenuItems[TREE_MENU_LOAD] = 
       XtVaCreateManagedWidget("treeMenuLoad", smeBSBObjectClass,
			       treeMenus[TREE_MENU],
			       NULL);
   XtAddCallback(treeMenuItems[TREE_MENU_LOAD],
		 XtNcallback, TreeMenu_CB, (XtPointer) TREE_MENU_LOAD);
		 
   treeMenuItems[TREE_MENU_SAVE] = 
       XtVaCreateManagedWidget("treeMenuSave", smeBSBObjectClass,
			       treeMenus[TREE_MENU],
			       NULL);
   XtAddCallback(treeMenuItems[TREE_MENU_SAVE],
		 XtNcallback, TreeMenu_CB, (XtPointer) TREE_MENU_SAVE);
		 
   treeMenuItems[TREE_MENU_SEP1] = 
       XtVaCreateManagedWidget("treeMenuSep1", smeLineObjectClass,
			       treeMenus[TREE_MENU],
			       NULL);

#ifdef COMMENT
   treeMenuItems[TREE_MENU_STATS] = 
       XtVaCreateManagedWidget("treeMenuStats", smeBSBObjectClass,
			       treeMenus[TREE_MENU],
			       XtNlabel, labelStr[STR_SHOW_STATS],
			       NULL);

   XtAddCallback(treeMenuItems[TREE_MENU_STATS],
		 XtNcallback, TreeMenu_CB, (XtPointer) TREE_MENU_STATS);

   XtSetSensitive(treeMenuItems[TREE_MENU_STATS], FALSE);

   treeMenuItems[TREE_MENU_SEP2] = 
       XtVaCreateManagedWidget("treeMenuSep2", smeLineObjectClass,
			       treeMenus[TREE_MENU],
			       NULL);
#endif /* COMMENT */
   
   treeMenuItems[TREE_MENU_QUIT] = 
       XtVaCreateManagedWidget("treeMenuQuit", smeBSBObjectClass,
			       treeMenus[TREE_MENU],
			       NULL);
   XtAddCallback(treeMenuItems[TREE_MENU_QUIT],
		 XtNcallback, TreeMenu_CB, (XtPointer) TREE_MENU_QUIT);
		 


   /* Layout menu */

#ifdef COMMENT
   treeMenuItems[LAYOUT_MENU_FIXED] = 
       XtVaCreateManagedWidget("layoutMenuFixed", smeBSBObjectClass,
			       treeMenus[LAYOUT_MENU],
			       XtNleftMargin, 18,
			       NULL);

   XtAddCallback(treeMenuItems[LAYOUT_MENU_FIXED],
		 XtNcallback, LayoutMenu_CB, (XtPointer) LAYOUT_MENU_FIXED);

   treeMenuItems[LAYOUT_MENU_VARIABLE] = 
       XtVaCreateManagedWidget("layoutMenuVariable", smeBSBObjectClass,
			       treeMenus[LAYOUT_MENU],
			       XtNleftMargin, 18,
			       NULL);

   XtAddCallback(treeMenuItems[LAYOUT_MENU_VARIABLE],
		 XtNcallback, LayoutMenu_CB, (XtPointer) LAYOUT_MENU_VARIABLE);


   treeMenuItems[LAYOUT_MENU_SEP1] = 
       XtVaCreateManagedWidget("layoutSep1", smeLineObjectClass,
			       treeMenus[LAYOUT_MENU],
			       NULL);
#endif /* COMMENT */

   treeMenuItems[LAYOUT_MENU_SPACING] = 
       XtVaCreateManagedWidget("layoutMenuSpacing", smeBSBObjectClass,
			       treeMenus[LAYOUT_MENU],
			       XtNleftMargin, 18,
			       NULL);
   XtAddCallback(treeMenuItems[LAYOUT_MENU_SPACING],
		 XtNcallback, LayoutMenu_CB, (XtPointer) LAYOUT_MENU_SPACING);

   treeMenuItems[LAYOUT_MENU_SEP2] = 
       XtVaCreateManagedWidget("layoutMenuSep2", smeLineObjectClass,
			       treeMenus[LAYOUT_MENU],
			       NULL);

   /* the following is a toggle button - we hack it using a bitmap */
   treeMenuItems[LAYOUT_MENU_ALIGN_NODES] = 
       XtVaCreateManagedWidget("layoutMenuAlignNodes", smeBSBObjectClass,
			       treeMenus[LAYOUT_MENU],
			       XtNleftMargin, 18,
			       NULL);
   XtAddCallback(treeMenuItems[LAYOUT_MENU_ALIGN_NODES],
		 XtNcallback, LayoutMenu_CB, 
		 (XtPointer) LAYOUT_MENU_ALIGN_NODES);

}

/* ----------------------------------------------------------------------------
 * 
 *   BuildDialogs() builds all the dialogs used in this application. 
 * 
 * ----------------------------------------------------------------------------
 */


BuildDialogs(parent)
     Widget parent;
{
  int	i;
  Widget 
    tmpwidget, dlgForm, dlgValue, 
    dlgLevelLabel, dlgSiblingLabel, 
    dlgOkButton, dlgCancelButton;
  
  labelStr[STR_LOAD_FILE] = "Name of file to load:";
  labelStr[STR_SAVE_FILE] = "Name of file to save:";
  
  errStr[ERR_OPENFAIL]  = "Failed to open file.";
  errStr[ERR_EMPTYFILE] = "Input file is empty.";
  errStr[ERR_MEMALLOC]  = "Memory allocation failed.";
  errStr[ERR_NOBEGIN] = "End of list does not have a matching beginning.";
  errStr[ERR_NOEND] = "Beginning of list does not have a matching end.";
  errStr[ERR_NOROOT] = "Input file does not have a root label.";
  errStr[ERR_MANYROOT] = "Input file has more than one root label.";
  
  
  dialog[DLG_NEW] = XtVaCreatePopupShell("dlgNewTree",
					 transientShellWidgetClass,
					 parent,
					 XtNresizable, TRUE,
					 NULL);
  
  tmpwidget = XtVaCreateManagedWidget("dlgNewTreeForm",
				      dialogWidgetClass,
				      dialog[DLG_NEW], 
				      XtNresizable, TRUE,
				      XtNvalue, "",
				      NULL);
  
  XawDialogAddButton(tmpwidget, "dlgNewTreeOk", NewTree_CB, 
		     (XtPointer) TRUE);
  XawDialogAddButton(tmpwidget, "dlgNewTreeCancel", NewTree_CB, 
		     (XtPointer) FALSE);
  
  dlgValue = XtNameToWidget(tmpwidget, "value");
  
  XtOverrideTranslations
    (dlgValue, XtParseTranslationTable
     ("<Key>Return:dlg_activate(dlgNewTreeOk)\n"));
  
  
  dialog[DLG_NODE_NAME] = XtVaCreatePopupShell("dlgNodeName",
					       transientShellWidgetClass,
					       TreeTopLevel,
					       XtNresizable, TRUE,
					       NULL);
				    
  
  tmpwidget= XtVaCreateManagedWidget("dlgNodeNameForm",
				     dialogWidgetClass,
				     dialog[DLG_NODE_NAME], 
				     XtNresizable, TRUE,
				     XtNvalue, "",
				     NULL);
  
  XawDialogAddButton(tmpwidget, "dlgNodeNameOk", NodeLabel_CB, 
		     (XtPointer) TRUE);
  XawDialogAddButton(tmpwidget, "dlgNodeNameCancel", NodeLabel_CB, 
		     (XtPointer) FALSE);
  
  dlgValue = XtNameToWidget(tmpwidget, "value");
  
  XtOverrideTranslations
    (dlgValue, XtParseTranslationTable
     ("<Key>Return: dlg_activate(dlgNodeNameOk)\n"));
  

  dialog[DLG_FILE] = XtVaCreatePopupShell("dlgFile",
					  transientShellWidgetClass,
					  TreeTopLevel, 
					  XtNresizable, TRUE,
					  NULL);

  tmpwidget = XtVaCreateManagedWidget("dlgFileForm",
				      dialogWidgetClass,
				      dialog[DLG_FILE], 
				      XtNresizable, TRUE,
				      XtNvalue, "",
				      NULL);
   
  XawDialogAddButton(tmpwidget, "dlgFileOk", File_CB, (XtPointer) TRUE);
  XawDialogAddButton(tmpwidget, "dlgFileCancel", File_CB, (XtPointer) FALSE);

  dlgValue = XtNameToWidget(tmpwidget, "value");
  
  XtOverrideTranslations
    (dlgValue, XtParseTranslationTable
     ("<Key>Return:dlg_activate(dlgFileOk)\n"));
  

  dialog[DLG_INFO] = XtVaCreatePopupShell("dlgInfo",
					  transientShellWidgetClass,
					  TreeTopLevel, 
					  XtNresizable, TRUE,
					  NULL);
  
  tmpwidget = XtVaCreatePopupShell("dlgInfoForm",
				   dialogWidgetClass,
				   dialog[DLG_INFO], 
				   XtNresizable, TRUE,
				   XtNvalue, "",
				   NULL);
  
  XawDialogAddButton(tmpwidget, "dlgInfoButton", Popdown_CB, 
		     (XtPointer) dialog[DLG_INFO]);
  
  
  dialog[DLG_ERROR] = XtVaCreatePopupShell("dlgError",
					   transientShellWidgetClass,
					   TreeTopLevel, 
					   XtNresizable, TRUE,
					   NULL);
   
  tmpwidget = XtVaCreateManagedWidget("dlgErrorForm",
				      dialogWidgetClass,
				      dialog[DLG_ERROR], 
				      XtNresizable, TRUE,
				      XtNvalue, "",
				      NULL);
   
  XawDialogAddButton(tmpwidget, "dlgErrorButton", Popdown_CB, 
		     (XtPointer) dialog[DLG_ERROR]);
   

  dialog[DLG_SPACING] = XtVaCreatePopupShell("dlgSpacing",
					     transientShellWidgetClass,
					     TreeTopLevel, 
					     XtNresizable, FALSE,
					     NULL);
  dlgForm = XtVaCreateManagedWidget("dlgSpacingForm",
				    formWidgetClass,
				    dialog[DLG_SPACING],
				    XtNresizable, FALSE,
				    NULL);
   
  dlgLevelLabel = XtVaCreateManagedWidget("dlgLevelLabel",
					  labelWidgetClass,
					  dlgForm,
					  XtNfromVert, NULL,
					  XtNfromHoriz, NULL,
					  XtNtop, XawChainTop,
					  XtNbottom, XawChainTop,
					  XtNleft, XawChainLeft,
					  XtNright, XawChainLeft,
					  XtNborderWidth, 0,
					  NULL);

  dlgLevelValuator = XtVaCreateManagedWidget("dlgLevelValuator",
					     labelWidgetClass,
					     dlgForm,
					     XtNfromVert, NULL,
					     XtNfromHoriz, dlgLevelLabel,
					     XtNtop, XawChainTop,
					     XtNbottom, XawChainTop,
					     XtNleft, XawChainLeft,
					     XtNright, XawChainLeft,
					     XtNlabel, "   ",
					     NULL);

  
  dlgLevelScale = XtVaCreateManagedWidget("dlgLevelScale",
					  pannerWidgetClass,
					  dlgForm,
					  XtNfromVert, dlgLevelLabel,
					  XtNfromHoriz, NULL,
					  XtNtop, XawChainTop,
					  XtNbottom, XawChainTop,
					  XtNleft, XawChainLeft,
					  XtNright, XawChainLeft,
					  XtNcanvasWidth, 110,
					  XtNdefaultScale, 1,
					  XtNinternalSpace, 1,
					  XtNrubberBand, FALSE,
					  XtNshadowThickness, 0,
					  XtNallowOff, FALSE,
					  XtNwidth, 200,
					  XtNsliderWidth, 10,
					  XtNheight, 15,
					  NULL);
  
  XtOverrideTranslations(dlgLevelScale, XtParseTranslationTable("\
<Btn2Down>:		start()\n\
<Btn2Motion>:		move()\n\
<Btn2Up>:		notify()stop()\n\
<Btn3Down>:		abort()\n"));

  XtAddCallback(dlgLevelScale, XtNreportCallback, levelscale_CB,
		(XtPointer) dlgLevelValuator);
  XtAddCallback(dlgLevelScale, XtNreportCallback, levelvalue_CB,
		(XtPointer) &dlgLevelValue);

  dlgSiblingLabel = XtVaCreateManagedWidget("dlgSiblingLabel",
					    labelWidgetClass,
					    dlgForm,
					    XtNfromVert, dlgLevelScale,
					    XtNfromHoriz, NULL,
					    XtNtop, XawChainTop,
					    XtNbottom, XawChainTop,
					    XtNleft, XawChainLeft,
					    XtNright, XawChainLeft,
					    XtNborderWidth, 0,
					    NULL);

  dlgSiblingValuator = XtVaCreateManagedWidget("dlgLevelValuator",
					       labelWidgetClass,
					       dlgForm,
					       XtNfromVert, dlgLevelScale,
					       XtNfromHoriz, dlgSiblingLabel,
					       XtNtop, XawChainTop,
					       XtNbottom, XawChainTop,
					       XtNleft, XawChainLeft,
					       XtNright, XawChainLeft,
					       XtNlabel, "   ",
					       NULL);
  dlgSiblingScale = XtVaCreateManagedWidget("dlgSiblingScale",
					    pannerWidgetClass,
					    dlgForm,
					    XtNfromVert, dlgSiblingLabel,
					    XtNfromHoriz, NULL,
					    XtNtop, XawChainTop,
					    XtNbottom, XawChainTop,
					    XtNleft, XawChainLeft,
					    XtNright, XawChainLeft,
					    XtNcanvasWidth, 110,
					    XtNdefaultScale, 1,
					    XtNinternalSpace, 1,
					    XtNrubberBand, FALSE,
					    XtNshadowThickness, 0,
					    XtNallowOff, FALSE,
					    XtNwidth, 200,
					    XtNsliderWidth, 10,
					    XtNheight, 15,
					    NULL);
  
  XtOverrideTranslations(dlgSiblingScale, XtParseTranslationTable("\
<Btn2Down>:		start()\n\
<Btn2Motion>:		move()\n\
<Btn2Up>:		notify()stop()\n\
<Btn3Down>:		abort()\n"));

  XtAddCallback(dlgSiblingScale, XtNreportCallback, siblingscale_CB,
		(XtPointer) dlgSiblingValuator);
  XtAddCallback(dlgSiblingScale, XtNreportCallback, siblingvalue_CB,
		(XtPointer) &dlgSiblingValue);
  
  dlgOkButton = XtVaCreateManagedWidget("dlgOkButton",
					commandWidgetClass,
					dlgForm,
					XtNfromVert, dlgSiblingScale,
					XtNfromHoriz, NULL,
					XtNtop, XawChainTop,
					XtNbottom, XawChainTop,
					XtNleft, XawChainLeft,
					XtNright, XawChainLeft,
					NULL);
  
  dlgCancelButton = XtVaCreateManagedWidget("dlgCancelButton",
					    commandWidgetClass,
					    dlgForm,
					    XtNfromVert, dlgSiblingScale,
					    XtNfromHoriz, dlgOkButton,
					    XtNtop, XawChainTop,
					    XtNbottom, XawChainTop,
					    XtNleft, XawChainLeft,
					    XtNright, XawChainLeft,
					    NULL);
  
  XtAddCallback(dlgOkButton, XtNcallback, Scale_CB, (XtPointer) TRUE);
  XtAddCallback(dlgCancelButton, XtNcallback, Scale_CB, (XtPointer) FALSE);
}

/* ----------------------------------------------------------------------------
 * 
 *   BuildHelpWindow() builds the help window that is displayed when the
 *   Help button on the application main window is pressed.
 * 
 * ----------------------------------------------------------------------------
 */


static void
BuildHelpWindow(parent)
     Widget parent;
{
  int	i;

  helpShell = XtVaCreatePopupShell("helpShell", transientShellWidgetClass,
				   parent,
				   NULL);
  
  helpForm = XtVaCreateManagedWidget("helpForm",
				     formWidgetClass,
				     helpShell,
				     NULL);

  helpDoneButton = XtVaCreateManagedWidget("helpDoneButton",
					   commandWidgetClass,
					   helpForm,
					   XtNfromVert, NULL,
					   XtNfromHoriz, NULL,
					   XtNtop, XawChainTop,
					   XtNbottom, XawChainTop,
					   XtNleft, XawChainLeft,
					   XtNright, XawChainLeft,
					   NULL);
  
  XtAddCallback(helpDoneButton, XtNcallback, Popdown_CB, 
		(XtPointer) helpShell);

  helpTitle = XtVaCreateManagedWidget("helpTitle",
				      labelWidgetClass,
				      helpForm,
				      XtNfromVert, NULL,
				      XtNfromHoriz, helpDoneButton,
				      XtNtop, XawChainTop,
				      XtNbottom, XawChainTop,
				      XtNleft, XawChainLeft,
				      XtNright, XawChainLeft,
				      XtNborderWidth, 0,
				      XtNlabel, "",
				      NULL);

  helpText = XtVaCreateManagedWidget("helpText",
				     asciiTextWidgetClass,
				     helpForm,
				     XtNfromVert, helpDoneButton,
				     XtNfromHoriz, NULL,
				     XtNtop, XawChainTop,
				     XtNbottom, XawChainBottom,
				     XtNleft, XawChainLeft,
				     XtNright, XawChainRight,
				     XtNeditType, XawtextRead,
				     XtNdisplayCaret, FALSE,
				     XtNscrollVertical, XawtextScrollAlways,
				     XtNscrollHorizontal, 
				     XawtextScrollWhenNeeded,
				     XtNuseStringInPlace, TRUE,
				     XtNstring, help_text,
				     XtNwidth, 530,
				     XtNheight, 650,
				     NULL);
}

/* ----------------------------------------------------------------------------
 * 
 *   BuildApplicationWindow() assumes that TreeTopLevel has been initialized 
 *   by XtInitialize(). TreeDrawingArea is set to the drawing area in this
 *   application window.
 * 
 * ----------------------------------------------------------------------------
 */


static void
BuildApplicationWindow(parent)
     Widget parent;
{
  Widget	topPane, box;

  topPane = XtVaCreateManagedWidget("topPane",
				    panedWidgetClass,
				    parent,
				    XtNorientation, XtorientVertical,
				    NULL);

  BuildMenubar(topPane);

  statusText = XtVaCreateManagedWidget("statusText",
				       labelWidgetClass,
				       topPane,
				       XtNborderWidth, 0,
				       NULL);

  scrolledWindow = XtVaCreateManagedWidget("scrolledWindow",
					   viewportWidgetClass,
					   topPane, 
					   XtNshowGrip, FALSE,
					   XtNallowHoriz, TRUE,
					   XtNallowVert, TRUE,
					   XtNuseBottom, TRUE,
					   XtNuseRight, TRUE,
					   NULL);

  TreeDrawingArea = XtVaCreateManagedWidget("treeDrawingArea",
					    simpleWidgetClass,
					    scrolledWindow,
					    NULL);
}

/* ----------------------------------------------------------------------------
 * 
 *   HandleError() is used when ReadTreeFromFile() or SaveTreeToFile() returns
 *   an error. 
 * 
 * ----------------------------------------------------------------------------
 */

void
HandleError(error, tree)
     ErrCode error;
     Tree *tree;
{
  Widget	dlg, text;
  
  if (tree) {
    dlg = XtNameToWidget(dialog[DLG_INFO], "dlgInfoForm");
    if (dlg) {
      XtVaSetValues(dlg, 
		    XtNvalue, errStr[error],
		    XtNwidth, 200,
		    NULL);
      text = XtNameToWidget(dlg, "value");
      if (text) {
	XtVaSetValues(text, XtNdisplayCaret, FALSE, NULL);
	XtVaSetValues(XawTextGetSource(text), XtNeditType, XawtextRead, NULL);
      }
    }
    popup_dialog(dialog[DLG_INFO], XtGrabExclusive);
  } else {
    dlg = XtNameToWidget(dialog[DLG_ERROR], "dlgErrorForm");
    if (dlg) {
      XtVaSetValues(dlg, 
		    XtNvalue, errStr[error], 
		    XtNwidth, 200,
		    NULL);
      text = XtNameToWidget(dlg, "value");
      if (text) {
	XtVaSetValues(text, XtNdisplayCaret, FALSE, NULL);
	XtVaSetValues(XawTextGetSource(text), XtNeditType, XawtextRead, NULL);
      }
    }
    popup_dialog(dialog[DLG_ERROR], XtGrabExclusive);
  }
}


/* ----------------------------------------------------------------------------
 * 
 *   InitializeInterface() initializes the environment for displaying
 *   the tree and creates the necessary supporting structures for interaction
 *   and display. This function should be called only once.
 * 
 * ----------------------------------------------------------------------------
 */

void
InitializeInterface(argc, argv)
     int *argc;
     char *argv[];
{
  XrmValue       rmvalue;
  char          *rmtype;
  Window         win;
  Pixel          background;
  XColor         colors[NUM_COLORS];
  Colormap       cmap;
  Pixmap         gray_pixmap;
  Atom		 wm_delete_window;

  /* XtSetLanguageProc is broken on some systems.
     The symptom is an error of the form:
        Warning: X locale modifiers not supported, using default
     so don't use this call:

        XtSetLanguageProc(NULL, NULL, NULL);
  */
  
  TreeTopLevel = XtVaAppInitialize(&app, "Xoobr", 
				   NULL, 0,
				   argc, argv, 
				   fallback_resources, 
				   NULL);
  
  ASSERT(TreeTopLevel, "failed to open connection to X server");
  
  TreeDisplay = XtDisplay(TreeTopLevel);
  TreeScreen  = DefaultScreen(TreeDisplay);

  /* set up color resources */
  applRsrcsPtr = &applRsrcs;
  XtGetApplicationResources(TreeTopLevel, applRsrcsPtr,
			    applRsrcsDesc, XtNumber(applRsrcsDesc),
			    NULL, 0);

  /* add actions */
  XtAppAddActions(app, local_actions, 6);
  XawSimpleMenuAddGlobalActions(app);
  
  TreeContourWidth = applRsrcsPtr->contour_width;
  
  /* set up label font info */
  if (XrmGetResource(XtDatabase(TreeDisplay),
		     X11_FONT_RESOURCE, X11_FONT_CLASS_RESOURCE,
		     &rmtype, &rmvalue)) {
    if (! (TreeLabelFont = XLoadQueryFont(TreeDisplay, rmvalue.addr)) ) {
      WARN_4("cannot load user-specified font:", rmvalue.addr,
	     "using font:", X11_DEFAULT_FONT);
      TreeLabelFont = XLoadQueryFont(TreeDisplay, X11_DEFAULT_FONT);
    }
  }
  else
    TreeLabelFont = XLoadQueryFont(TreeDisplay, X11_DEFAULT_FONT);
  TreeLabelFont = XLoadQueryFont(TreeDisplay, "fixed");
  
  check_pixmap =
    XCreateBitmapFromData(TreeDisplay, RootWindow(TreeDisplay, TreeScreen),
			  check_bits, check_width, check_height);
  
  slider_pixmap =
    XCreateBitmapFromData(TreeDisplay, RootWindow(TreeDisplay, TreeScreen),
			  slider_pixmap_bits, slider_pixmap_width, 
			  slider_pixmap_height);
  
  BuildApplicationWindow(TreeTopLevel);
  BuildHelpWindow(TreeTopLevel);
  BuildPopupMenus(TreeTopLevel);
  BuildDialogs(TreeTopLevel);
  
  
  XtAddEventHandler(TreeDrawingArea, ExposureMask, FALSE,
		    ExposeHandler, (XtPointer) NULL);
  
  XtVaSetValues(TreeDrawingArea, 
		XtNbackground, applRsrcsPtr->background_color, 
		NULL);
  
  XtRealizeWidget(TreeTopLevel);
  
  XtOverrideTranslations
    (TreeTopLevel,
     XtParseTranslationTable("<Message>WM_PROTOCOLS: quit()"));

  wm_delete_window = XInternAtom(TreeDisplay, "WM_DELETE_WINDOW", FALSE);
  (void) XSetWMProtocols(TreeDisplay, XtWindow(TreeTopLevel),
			 &wm_delete_window, 1);


  win = XtWindow(TreeDrawingArea);
  cmap = DefaultColormap(TreeDisplay, TreeScreen);
  
  /* set up double buffer */
  colors[BACKGROUND_COLOR].pixel = applRsrcsPtr->background_color;
  XQueryColor(TreeDisplay, cmap, &(colors[BACKGROUND_COLOR]));
  colors[TREE_COLOR].pixel = applRsrcsPtr->tree_color;
  XQueryColor(TreeDisplay, cmap, &(colors[TREE_COLOR]));
  colors[CONTOUR_COLOR].pixel = applRsrcsPtr->contour_color;
  XQueryColor(TreeDisplay, cmap, &(colors[CONTOUR_COLOR]));
  colors[HIGHLIGHT_COLOR].pixel = applRsrcsPtr->highlight_color;
  XQueryColor(TreeDisplay, cmap, &(colors[HIGHLIGHT_COLOR]));
  colors[SPLIT_COLOR].pixel = applRsrcsPtr->split_color;
  XQueryColor(TreeDisplay, cmap, &(colors[SPLIT_COLOR]));
  colors[ACTION_COLOR].pixel = applRsrcsPtr->action_color;
  XQueryColor(TreeDisplay, cmap, &(colors[ACTION_COLOR]));
  
  TreeDrawingAreaDB = 
    DBLcreate_double_buffer(TreeDisplay, 
			    XtWindow(TreeDrawingArea), FALSE,
			    colors, NUM_COLORS);

  ASSERT(TreeDrawingAreaDB, "could not create double buffer");
  
  background = DBLinq_background(TreeDrawingAreaDB);
  
  
  XtVaSetValues(TreeDrawingArea, XtNbackground, background, NULL);
  
  /* also set the scrolledWindow background to same color */
  XtVaSetValues(XtParent(TreeDrawingArea), XtNbackground, background, NULL);
  
  XSetFont(TreeDrawingAreaDB->display, TreeDrawingAreaDB->gc,
	   TreeLabelFont->fid); 
  
  gray_pixmap =
    XCreatePixmapFromBitmapData(TreeDisplay, XtWindow(TreeDrawingArea),
				gray_bits, gray_width, gray_height,
				TreeDrawingAreaDB->colors[TREE_COLOR],
				background,
				DefaultDepth(TreeDisplay, TreeScreen));
  
#ifdef COMMENT
  XtVaSetValues(treeMenuItems[LAYOUT_MENU_VARIABLE],
		XtNleftBitmap, check_pixmap, 
		NULL);
#endif

  XSetTile(TreeDrawingAreaDB->display, TreeDrawingAreaDB->gc, gray_pixmap);
  SetDrawColor(TREE_COLOR);
  
  InitializeDissolveEffect(TreeDisplay, XtWindow(TreeDrawingArea),
			   TreeDrawingAreaDB->colors[TREE_COLOR], background);
  
  PauseCursor = XCreateFontCursor(TreeDisplay, XC_watch);
  
  StatusMsg("Ready", TRUE);
}


/* ----------------------------------------------------------------------------
 * 
 *   UserEventLoop() is called after InitializeInterface() to handle
 *   processing of events.
 * 
 * ----------------------------------------------------------------------------
 */

void
UserEventLoop()
{
  XtAppMainLoop(app);
}


/* ------------------------------------------------------------------------- */
/*			  Callback Routines                                  */
/* ------------------------------------------------------------------------- */

static void
TreeMenu_CB(w, client_data, call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  long		menuItem = (long) client_data;
  Widget	dlgFileForm;
  
  switch (menuItem) {
  case TREE_MENU_NEW:
    popup_dialog(dialog[DLG_NEW], XtGrabExclusive);
    break;
  case TREE_MENU_LOAD:
    loading_file = 1;
    XtVaSetValues(dialog[DLG_FILE],  XtNtitle, "Load File", NULL);
    dlgFileForm = XtNameToWidget(dialog[DLG_FILE], "dlgFileForm");
    XtVaSetValues(dlgFileForm, XtNvalue, "", NULL);
    popup_dialog(dialog[DLG_FILE], XtGrabExclusive);
    break;
  case TREE_MENU_SAVE:
    loading_file = 0;
    XtVaSetValues(dialog[DLG_FILE],  XtNtitle, "Save File", NULL);
    dlgFileForm = XtNameToWidget(dialog[DLG_FILE], "dlgFileForm");
    XtVaSetValues(dlgFileForm, XtNvalue, "", NULL);
    popup_dialog(dialog[DLG_FILE], XtGrabExclusive);
    break;
  case TREE_MENU_QUIT:
    exit(0);
    break;
    
  default:
    WARN("unknown menu item in TreeMenu_CB");
  }
}


/* ------------------------------------------------------------------------- */

static void
LayoutMenu_CB(w, client_data, call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  long	menuItem = (long) client_data;
  char	tmpstr[10];
  
  switch (menuItem) {
#ifdef COMMENT
  case LAYOUT_MENU_FIXED:
    XtVaSetValues(layoutMenuItems[LAYOUT_MENU_VARIABLE],
                  XtNleftBitmap, None, NULL);
    XtVaSetValues(layoutMenuItems[LAYOUT_MENU_FIXED], 
                  XtNleftBitmap, check_pixmap, NULL);

    if (TreeLayoutDensity != Fixed) {
      TreeLayoutDensity = Fixed;
      DeleteTree(TheTree, TRUE);
      SetupTree(TheTree);
    }
    break;    
  case LAYOUT_MENU_VARIABLE:
    XtVaSetValues(layoutMenuItems[LAYOUT_MENU_VARIABLE],
                  XtNleftBitmap, check_pixmap, NULL);
    XtVaSetValues(layoutMenuItems[LAYOUT_MENU_FIXED], 
                  XtNleftBitmap, None, NULL);
    
    if (TreeLayoutDensity != Variable) {
      TreeLayoutDensity = Variable;
      DeleteTree(TheTree, TRUE);
      SetupTree(TheTree);
    }
    break;
#endif COMMENT
  case LAYOUT_MENU_SPACING:
    dlgLevelValue = TreeParentDistance;
    dlgSiblingValue = TreeBorderSize;
    sprintf(tmpstr, "%d  ", dlgLevelValue);
    XtVaSetValues(dlgLevelValuator, XtNlabel, tmpstr, NULL);

    /* These setvalues need some adjustments... */
    XtVaSetValues(dlgLevelScale, 
		  XtNsliderX, (dlgLevelValue * 2),
		  NULL);
    sprintf(tmpstr, "%d  ", dlgSiblingValue);
    XtVaSetValues(dlgSiblingValuator, XtNlabel, tmpstr, NULL);
    XtVaSetValues(dlgSiblingScale, 
		  XtNsliderX, (dlgSiblingValue * 4),
		  NULL);
    popup_dialog(dialog[DLG_SPACING], XtGrabNone);
    break;
  case LAYOUT_MENU_ALIGN_NODES:
    TreeAlignNodes = !TreeAlignNodes;
    XtVaSetValues(treeMenuItems[LAYOUT_MENU_ALIGN_NODES],
		  XtNleftBitmap, TreeAlignNodes ? check_pixmap : None, 
		  NULL);
    DeleteTree(TheTree, TRUE);
    ResetLabels(TheTree);
    SetupTree(TheTree);
    break;
  default:
    WARN("unknown menu item in LayoutMenu_CB");
  }
}


/* ------------------------------------------------------------------------- */

static void
NodeMenu_CB(w, client_data, call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  long	menuItem = (long) client_data;
   
  switch (menuItem) {
  case NODE_MENU_ADD_CHILD:
    NodePos = Child;
    popup_dialog(dialog[DLG_NODE_NAME], XtGrabExclusive);
    break;
  case NODE_MENU_ADD_BEFORE:
    NodePos = Before;
    popup_dialog(dialog[DLG_NODE_NAME], XtGrabExclusive);
    break;
  case NODE_MENU_ADD_AFTER:
    NodePos = After;
    popup_dialog(dialog[DLG_NODE_NAME], XtGrabExclusive);
    break;
  case NODE_MENU_ELISION:
    ExpandCollapseNode(PopupNode);
    break;
  case NODE_MENU_DELETE:
    DeleteNode(PopupNode);
    if (TheTree == NULL)
      XtSetSensitive(treeMenuItems[TREE_MENU_SAVE], FALSE);
    break;
  default:
    WARN("unknown menu item in NodeMenu_CB");
  }
}


/* ------------------------------------------------------------------------- */

static void
Help_CB(w, client_data, call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  popup_dialog(helpShell, XtGrabNone);
}


/* ------------------------------------------------------------------------- */

static void
HelpDone_CB(w, client_data, call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  XtPopdown(helpShell);
}


/* ------------------------------------------------------------------------- */

static void
NodeLabel_CB(w, client_data, call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  char	*node_label;

  XtPopdown(dialog[DLG_NODE_NAME]);
  if (client_data == (XtPointer) TRUE) {
    node_label = XtMalloc(strlen(XawDialogGetValueString(XtParent(w)))
			  * sizeof(char));
    node_label = strcpy(node_label, XawDialogGetValueString(XtParent(w)));
    if (*node_label) 
      InsertNode(PopupNode, NodePos, node_label);
  }
}
      

/* ------------------------------------------------------------------------- */

static void
NewTree_CB(w, client_data, call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  char	*node_label;
  Tree	*tree;
  
  XtPopdown(dialog[DLG_NEW]);
  if (client_data == (XtPointer) TRUE) {
    node_label = XtMalloc((strlen(XawDialogGetValueString(XtParent(w))) + 1)
			  * sizeof(char));
    node_label = strcpy(node_label, XawDialogGetValueString(XtParent(w)));
    if (*node_label) {
      if (TheTree) {
	Delete(TheTree);
	XtSetSensitive(treeMenuItems[TREE_MENU_SAVE], FALSE);
      }
      tree = MakeNode();
      SetNodeLabel(tree, node_label);
      SetupTree(tree);
      XtSetSensitive(treeMenuItems[TREE_MENU_SAVE], TRUE);
    }
  }
  XSync(TreeDisplay, FALSE);
}


/* ------------------------------------------------------------------------- */

static void
File_CB(w, client_data, call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  char *fname;
  Tree *tree;
  ErrCode error;
  Arg args[1];
  int menuItem;
  
  if (client_data == (XtPointer) TRUE) {
    fname = XawDialogGetValueString(XtParent(w));
    if (*fname) { 
      
      if (loading_file == 1) {
	if (TheTree) {
	  Delete(TheTree);
	  XtSetSensitive(treeMenuItems[TREE_MENU_SAVE], FALSE);
	}
	tree = ReadTreeFromFile(fname, &error);
	XtPopdown(dialog[DLG_FILE]);
	if (error != ERR_NONE)
	  HandleError(error, tree);
	XSync(TreeDisplay, 0);
	if (tree) {
	  SetupTree(tree);
	  XtSetSensitive(treeMenuItems[TREE_MENU_SAVE], TRUE);
	} else {
	  SetDrawingTree(NULL);
	  BeginFrame();
	  EndFrame();
	}
      } else if (menuItem == TREE_MENU_SAVE) {
	if (!SaveTreeToFile(TheTree, fname)) {
	  XtPopdown(dialog[DLG_FILE]);
	  HandleError(ERR_OPENFAIL, NULL);
	} else
	  XtPopdown(dialog[DLG_FILE]);
      }
    } else
      XtPopdown(dialog[DLG_FILE]);
  } else 
    XtPopdown(dialog[DLG_FILE]); 
}


/* ------------------------------------------------------------------------- */

static void
Scale_CB(w, client_data, call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  int	level_value, sibling_value;
  float	tmp;
    
  XtPopdown(dialog[DLG_SPACING]);

  /* check if OK button was selected */
  if (client_data) {
    level_value = dlgLevelValue;
    
    sibling_value = dlgSiblingValue;
    
    if (level_value != TreeParentDistance ||
	sibling_value != TreeBorderSize) {
      
      TreeParentDistance = level_value;
      TreeBorderSize = sibling_value;
      XSync(TreeDisplay, 0);
      if (TheTree) {
	DeleteTree(TheTree, TRUE);
	if (TreeAlignNodes)
	  ResetLabels(TheTree);
	SetupTree(TheTree);
      }
    }
  }
}


/* ------------------------------------------------------------------------- */
/*									     */
/* Auxilary actions and callback functions                                   */
/*									     */
/* ------------------------------------------------------------------------- */

static void
Popdown_CB(w, client_data, call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  Widget	widget = (Widget) client_data;
    
  XtPopdown(widget);
}

static void
activate_action(w, event, params, num_params)
     Widget w;
     XButtonEvent *event;
     String *params;
     Cardinal *num_params;
{
  Widget	button;
  
  if (*num_params == 1) {
    button = XtNameToWidget(XtParent(w), params[0]);
    
    if (button)
      XtCallCallbacks(button, XtNcallback, (XtPointer) TRUE);
  }
}

static void
quit_action(w, event, params, num_params)
     Widget w;
     XButtonEvent *event;
     String *params;
     Cardinal *num_params;
{
  exit(0);
}



/* ------------------------------------------------------------------------- */

static void 
select_action(w, event, params, num_params)
     Widget w;
     XButtonEvent *event;
     String *params;
     Cardinal *num_params;
{
  Tree		*node;
  int		height;
  Boolean	edit = FALSE;

  if ((*num_params > 0) && 
      (!strcmp(params[0], "EDIT") ||
       !strcmp(params[0], "edit")))
    edit = TRUE;

  if (SearchTree(TheTree, event->x, event->y, &node)) {
    static Tree *PrevNode = NULL;
    Tree *ParNode = NULL;
    
    PopupNode = node;
    if (PrevNode != NULL)
      DrawNode(PrevNode, New);
    PrevNode = PopupNode;
    
    printf("%s^^%s^^%s\n", 
	   EnvNm,
	   edit ? "br-edit" : "br-view",
	   (PopupNode->value) ? PopupNode->value : PopupNode->label.text);

    StatusMsg(PopupNode->label.text, 1);
    
    ParNode = PopupNode->parent;
    PopupNode->parent = NULL;
    HiliteNode(PopupNode, New);
    PopupNode->parent = ParNode;
  }
}

static void 
menu_popup_action(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
  Widget menu;
  Boolean popup;
  ShellWidget shell_widget;

  char	    *strargs[] = {"nodeMenu", NULL};
  
  menu = XtNameToWidget(TreeTopLevel, params[0]);
  shell_widget = (ShellWidget) menu;
  
  if (nodeFound) {

    /* This is giving me a headache. 
     * Why does popup menus come up unselectable? 
     */

    XtCallActionProc(TreeDrawingArea,
		     "XtMenuPopup",
		     event,
		     strargs,
		     1);
		 
/*
    XtSetSensitive(menu, TRUE);
    XtPopupSpringLoaded(menu);
 */

/*    if (!XtIsShell(menu)) {
      printf("error: not shell widget!\n");
    } else {
      if (! shell_widget->shell.popped_up) {
	
	XtGrabKind call_data = XtGrabExclusive;
	
	XtCallCallbacks(menu, XtNpopupCallback, (XtPointer) &call_data);
	
	shell_widget->shell.popped_up = TRUE;
	shell_widget->shell.grab_kind = XtGrabExclusive;
	shell_widget->shell.spring_loaded = TRUE;
	
	if (shell_widget->shell.create_popup_child_proc != NULL) {
	  (*(shell_widget->shell.create_popup_child_proc))(menu);
	}
	
	XtAddGrab(menu, TRUE, TRUE);
	
	XtRealizeWidget(menu);
	
	XMapRaised(XtDisplay(menu), XtWindow(menu));
	
      } else
	XRaiseWindow(XtDisplay(menu), XtWindow(menu));   
    }
    */
  }
}


static void 
button_action(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
  Tree     *node;
  
  if (SearchTree(TheTree, event->xbutton.x, event->xbutton.y, &node)) {
    
    nodeFound = TRUE;
    
    XtSetSensitive(nodeMenuItems[NODE_MENU_DELETE], TRUE);

    XtVaSetValues(treeMenus[NODE_MENU], 
		  XtNlabel, node->label.text, 
		  NULL);
    PopupNode = node;
    if (IS_LEAF(node))  {
      
      XtSetSensitive(nodeMenuItems[NODE_MENU_ELISION], FALSE);
      
      XtVaSetValues(nodeMenuItems[NODE_MENU_ELISION], 
		    XtNlabel, labelStr[STR_NODE_COLLAPSE], 
		    NULL);
      
    } else {
      
      XtSetSensitive(nodeMenuItems[NODE_MENU_ELISION], TRUE);
      if (node->elision) {
	XtVaSetValues(nodeMenuItems[NODE_MENU_ELISION],
		      XtNlabel, labelStr[STR_NODE_EXPAND],
		      NULL);
      } else {
	XtVaSetValues(nodeMenuItems[NODE_MENU_ELISION],
		      XtNlabel, labelStr[STR_NODE_COLLAPSE],
		      NULL);
      }
    }
    
    if (node->parent) {
      XtSetSensitive(nodeMenuItems[NODE_MENU_ADD_BEFORE], TRUE);
      XtSetSensitive(nodeMenuItems[NODE_MENU_ADD_AFTER], TRUE);
    } else {
      XtSetSensitive(nodeMenuItems[NODE_MENU_ADD_BEFORE], FALSE);
      XtSetSensitive(nodeMenuItems[NODE_MENU_ADD_AFTER], FALSE);
    }
    
    if (node->elision) {
      XtSetSensitive(nodeMenuItems[NODE_MENU_ADD_CHILD], FALSE);
    } else {
      XtSetSensitive(nodeMenuItems[NODE_MENU_ADD_CHILD], TRUE);
    }
    
  } else {
    nodeFound = FALSE;

    XtVaSetValues(treeMenus[NODE_MENU], 
		  XtNlabel, " ", 
		  NULL);

    XtSetSensitive(nodeMenuItems[NODE_MENU_ADD_BEFORE], FALSE);
    XtSetSensitive(nodeMenuItems[NODE_MENU_ADD_AFTER], FALSE);
    XtSetSensitive(nodeMenuItems[NODE_MENU_ADD_CHILD], FALSE);
    XtSetSensitive(nodeMenuItems[NODE_MENU_ELISION], FALSE);
    XtSetSensitive(nodeMenuItems[NODE_MENU_DELETE], FALSE);
  }
}


/* ------------------------------------------------------------------------- */
/*									     */
/* General purpose popdown widget callback (for dialog popdown buttons)      */
/*									     */
/* ------------------------------------------------------------------------- */

static void center_widget(w, x, y)
Widget w;
int x, y;
{
  Dimension	width, height, border;
  Position	max_x, max_y;
  
  XtVaGetValues(w,
		XtNwidth, &width,
		XtNheight, &height,
		XtNborderWidth, &border,
		NULL);
  
  width += 2 * border;
  height += 2 * border;
  
  x -= ((Position) width / 2);
  y -= ((Position) height / 2);
  
  if (x < 0) x = 0;
  if (y < 0) y = 0;
  
  if (x > (max_x = (Position) (XtScreen(w)->width - width)) )
    x = max_x;
  
  if (y > (max_y = (Position) (XtScreen(w)->height - height)) )
    y = max_y;
  
  XtVaSetValues(w,
		XtNx, x,
		XtNy, y,
		NULL);
}

#define shell_translations "<Message>WM_PROTOCOLS: wmpopdown()\n"
		     
static void 
popup_dialog(shell, grab_kind)
     Widget shell;
     XtGrabKind grab_kind;
{
  int		idummy, x, y;
  Window	wdummy;

  XtOverrideTranslations(shell, XtParseTranslationTable(shell_translations));
  XtRealizeWidget(shell);
  wm_delete_window = XInternAtom(XtDisplay(shell), "WM_DELETE_WINDOW", 
				 FALSE);
  (void) XSetWMProtocols(XtDisplay(shell), XtWindow(shell),
			 &wm_delete_window, 1);
  
  XQueryPointer(TreeDisplay, DefaultRootWindow(TreeDisplay), 
		&wdummy, &wdummy,
		&x, &y, &idummy, &idummy, &idummy);
  
  center_widget(shell, x, y);
  
  XtPopup(shell, grab_kind);
}

static void 
popdown_action(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
  XtPopdown(widget);
}

static void 
set_siblingscale_CB(widget, w, percent_ptr)
     Widget widget;
     XtPointer w;
     XtPointer percent_ptr;
{
  char tmpstr[10];
  
  sprintf(tmpstr, "%d  ", (int) (*(float*) percent_ptr*MAX_BORDER_SIZE));
  
  XtVaSetValues((Widget) w, XtNlabel, tmpstr, NULL);
}

static void 
siblingscale_CB(widget, w, report)
     Widget widget;
     XtPointer w;
     XawPannerReport *report;
{
  char tmpstr[10];
  
  sprintf(tmpstr, "%d ", (int) 
	  ((float) report->slider_x / report->slider_width 
	   * MAX_BORDER_SIZE / 10));
  
  XtVaSetValues((Widget) w, XtNlabel, tmpstr, NULL);
}

static void
set_levelscale_CB(widget, w, percent_ptr)
     Widget widget;
     XtPointer w;
     XtPointer percent_ptr;
{
  char	tmpstr[10];

  sprintf(tmpstr, "%d  ", (int) (*(float*) percent_ptr*MAX_PARENT_DISTANCE));

  XtVaSetValues((Widget) w, XtNlabel, tmpstr, NULL);
}

static void
levelscale_CB(widget, w, report)
     Widget widget;
     XtPointer w;
     XawPannerReport *report;
{
  char	tmpstr[10];
  
  sprintf(tmpstr, "%d ", (int) 
	  ((float) report->slider_x / report->slider_width 
	   * MAX_PARENT_DISTANCE / 10));
  
  XtVaSetValues((Widget) w, XtNlabel, tmpstr, NULL);
}

static void 
set_siblingvalue_CB(widget, var_ptr, percent_ptr)
     Widget widget;
     XtPointer var_ptr;
     XtPointer percent_ptr;
{
  * (int *)var_ptr = (int) (*(float*) percent_ptr*MAX_BORDER_SIZE);
}

static void 
set_levelvalue_CB(widget, var_ptr, percent_ptr)
     Widget widget;
     XtPointer var_ptr;
     XtPointer percent_ptr;
{
  * (int *)var_ptr = (int) (*(float*) percent_ptr*MAX_PARENT_DISTANCE);
}


static void 
siblingvalue_CB(widget, var_ptr, report)
     Widget widget;
     XtPointer var_ptr;
     XawPannerReport *report;
{
  * (int *) var_ptr = (int) 
    ((float) report->slider_x / report->slider_width 
     * MAX_BORDER_SIZE / 10);
}

static void 
levelvalue_CB(widget, var_ptr, report)
     Widget widget;
     XtPointer var_ptr;
     XawPannerReport *report;
{
  * (int *) var_ptr = (int) 
    ((float) report->slider_x / report->slider_width 
     * MAX_PARENT_DISTANCE / 10);
}


/* ----------------------------------------------------------------------------
 * 
 *   Main routine
 * 
 * ----------------------------------------------------------------------------
 */

main(argc, argv)
     int argc;
     char *argv[];
{
  Tree *tree = NULL;
  ErrCode error = ERR_NONE;
  
  ProgramName = strdup(argv[0]);
  if (ProgramName == NULL) {
    fprintf(stderr, "%s: insufficient memory available\n", argv[0]);
    exit(0);
  }
  
  InitializeInterface(&argc, argv);
  XSync(TreeDisplay, 0);

  if (argc >= 2) {
    tree = ReadTreeFromFile(argv[1], &error);
    if (tree) StatusMsg(argv[1], TRUE);
    if (argc > 2)
      WARN("extraneous arguments ignored");
  } 
     
  if (tree) {
    SetupTree(tree);
    XtSetSensitive(treeMenuItems[TREE_MENU_SAVE], TRUE);
  } else {
    SetDrawingTree(NULL);
    XtSetSensitive(treeMenuItems[TREE_MENU_SAVE], FALSE);
  }

  if (error != ERR_NONE)
    HandleError(error, tree);
  
  StatusMsg("Left = Edit Node; Mid = View Node; Right = Node Menu", 1);
  UserEventLoop();
}

/* ------------------------------------------------------------------------- */
/*				  End of File                                */
/* ------------------------------------------------------------------------- */






