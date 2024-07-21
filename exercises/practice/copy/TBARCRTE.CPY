       String-From-tempBuffer SECTION.
           MOVE LENGTH of tempBuffer TO i
           INVOKE CharacterArray "fromBuffer" USING i
                                                    tempBuffer
                                          RETURNING aString
           .

       Create-Menu-And-Tool-Bar SECTION.

      * Initialize the data structures
           PERFORM Init-Data-Structure

      *   Add a menu bar to the window instance created from
      *   the handle of the Dialog System window.
           PERFORM Create-Menu-Bar

      * Add a tool bar, associating buttons with menu items.
           PERFORM Create-Tool-Bar

      * Redraw the menu bar.
           INVOKE aWindow "getMenubar" RETURNING aMenubar
           INVOKE aMenubar "reDraw"

      * Hide the toolbar for now. It should be explicitly shown
      * using dialog in the screenset once its parent window
      * has been created. Please see tbards.gs for an example of
      * how to show a toolbar using the dialog INVOKE function.
           INVOKE aToolbar "hide"
           .

       Init-Data-Structure SECTION.
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > maxMenuItems
               SET mReference(i) TO NULL
           END-PERFORM
           MOVE defaultMenuItems   TO numberOfMenuItems
           MOVE defaultButtons     TO numberOfButtons
           .

       Create-Menu-Bar SECTION.
           MOVE 0 TO wsIndex
           SET lsParent TO aWindow

      * Add the menu structure defined in mData (in tbardefn.cpy)
      * to the window aWindow.
           PERFORM UNTIL wsIndex >= defaultMenuItems
               ADD 1 TO wsIndex
CCCPY1*        CALL processMenu using lsParent
CCC004*    END-PERFORM
CCC004*    .

       Process-Menu-Entry SECTION.
       ENTRY processMenu USING lnkParent.
      * Recursively build the menu structure as a set of top level
      * menu items and/or sub menus.
           SET lsParent TO lnkParent
           MOVE wsIndex TO lsIndex

           PERFORM Add-A-Menu-Item
           ADD 1 TO lsIndex

           PERFORM UNTIL lsIndex > defaultMenuItems
               IF hasChildren(lsIndex)
      *Build the sub menu and all its siblings (including any sub menus)
                   MOVE lsIndex TO wsIndex
CCCPY1*            CALL processMenu USING lsParent

CCC004*            IF lastSibling(lsIndex)
CCC004*                EXIT PROGRAM
CCC004*            END-IF
CCC004*            MOVE wsIndex TO lsIndex

CCC004*        ELSE
      * Add a menu item or separator.
CCC004*            PERFORM Add-A-Menu-Item
CCC004*            IF lastSibling(lsIndex)
CCC004*                EXIT PERFORM
CCC004*            END-IF

CCC004*        END-IF

CCC004*        ADD 1 TO lsIndex
CCC004*    END-PERFORM

CCC004*    MOVE lsIndex TO wsIndex
CCC004*    EXIT PROGRAM
CCC004*    .

       Create-Tool-Bar SECTION.

           PERFORM Add-Tool-Bar
           PERFORM Process-Bitmaps
           PERFORM Process-Buttons
           .

       Add-Tool-Bar SECTION.
      * Add the tool bar to the window
           INVOKE toolbar "new" USING aWindow
                            RETURNING aToolbar

      *    set the flat  style.
           INVOKE aToolbar "flat"
      * Set tool bar width and height
           MOVE 16 TO height
           INVOKE aToolbar "setHeight" USING height
           INVOKE aWindow "getWidth" RETURNING width
           INVOKE aToolbar "setWidth" USING width

      * Make the tool bar resizable
           INVOKE aToolbar "resizable"

      * Set the width and height of the buttons
           MOVE 16 TO width
           MOVE 16 TO height
           INVOKE aToolbar "setBitmapWidthHeight" USING width height
           .

       Process-Bitmaps SECTION.

      * Load bitmap resources from resource file
           INVOKE module "newZ" USING resourceDllName
                            RETURNING aResource

      * Add button bitmaps
           PERFORM Add-Button-Bitmaps
           .

       Add-Button-Bitmaps SECTION.
           MOVE 16 TO width
           MOVE 16 TO height
           INVOKE maskedImageList "new" USING width
                                              height
                                    RETURNING buttonImageList

           MOVE 1 TO i
           PERFORM Add-Button-Bitmap until i > numberOfButtons
           INVOKE aToolBar "setImageList" USING buttonImageList
           .

       Add-Button-Bitmap SECTION.
           IF bMenuIndex(i) NOT = 0
              and bBitmapID(i) NOT = 0
               MOVE bBitmapID(i) TO resourceID
               INVOKE bitmapData "fromResource" USING aResource
                                                      resourceID
                                            RETURNING aBitmap
               INVOKE buttonImageList "addDefaultMaskedBitmap"
                                                       USING aBitmap
                                                       RETURNING wsIndex
               INVOKE aBitmap "finalize" RETURNING aBitmap
           END-IF
           add 1 TO i
           .

       Process-Buttons SECTION.
      * Associate toolbar buttons with menu items and add them
      * to the toolbar
           MOVE 1 TO buttonIndex
           PERFORM varying wsIndex from 1 by 1
                           until wsIndex > defaultButtons

               PERFORM Check-Prev-Menu-Indexes

      * Only add this button if it's a separator or if its related
      * menu index has not been used before for this toolbar
               IF i = 0
                   PERFORM Add-Indexed-Button
               ELSE
                   MOVE bMenuIndex(wsIndex) TO displayNumber
                   MOVE wsIndex             TO displayNumber2
                   MOVE SPACES TO tempBuffer
                   STRING "Menu index "        DELIMITED BY SIZE
                          displayNumber        DELIMITED BY SIZE
                     " already has an associated toolbar button,"&x"0A"
                                               DELIMITED BY SIZE
                          "therefore button index "
                                               DELIMITED BY SIZE
                          displayNumber2       DELIMITED BY SIZE
                          " was not added."    DELIMITED BY SIZE

                       INTO tempBuffer
                   END-STRING
                   PERFORM Display-Warning-Message

               END-IF

           END-PERFORM
           .

       Check-Prev-Menu-Indexes SECTION.
      * Loop back through button table to see if the current menu index
      * already has an associated toolbar button.
           IF bMenuIndex(wsIndex) NOT = 0 *> Only check non-separators
               MOVE wsIndex TO j
               SUBTRACT 1 FROM j
               PERFORM VARYING i FROM j BY -1 UNTIL i < 1
                   IF bMenuIndex(wsIndex) = bMenuIndex(i)
                       *> Menu index bMenuIndex(wsIndex) already has
                       *> an associated toolbar button.
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           ELSE
               MOVE 0 TO i *> Make separator buttons get added
           END-IF
           .

       Add-Indexed-Button SECTION.

      * Get associated menu item reference
           MOVE bMenuIndex(wsIndex) TO i

           IF i = 0
      * Add separator
               INVOKE aToolBar "addSeparator"
                                     RETURNING bReference(wsIndex)

           ELSE
      * Add button
               INVOKE aToolbar "addTool" USING buttonIndex
                                               mReference(i)
                                     RETURNING bReference(wsIndex)
               add 1 TO buttonIndex

      * Set tool tip (if specified)
               IF bString(wsIndex) NOT = SPACES
                   INVOKE bReference(wsIndex) "setTipZ"
                                             USING bString(wsIndex)
               END-IF

           END-IF
           .

       Add-A-Menu-Item SECTION.
      * Get the menu item text
           INVOKE CharacterArray "withValue" USING mString(lsIndex)
                                         RETURNING aString
           IF aString NOT = NULL
               IF hasChildren(lsIndex)
                   INVOKE lsParent "addSubMenu"
                                       USING aString
                                   RETURNING mReference(lsIndex)
                   SET lsParent TO mReference(lsIndex)

               ELSE
                   IF isSeparator(lsIndex)
                       INVOKE lsParent "addSeparator"
                                       RETURNING mReference(lsIndex)
                   ELSE
                       INVOKE lsParent "addItem"
                                           USING aString
                                       RETURNING mReference(lsIndex)
                   END-IF

               END-IF

      * Find out if any menu item hint text was specified
               INVOKE aString "size" RETURNING i
               INVOKE aString "finalize" RETURNING aString
               add 2 TO i
               IF mString(lsIndex)(i:1) NOT = x"00"
      * Menu item hint text was specified, so set it up
                   INVOKE CharacterArray "withValue"
                                   USING mString(lsIndex)(i:1)
                               RETURNING aString
                   IF aString NOT = NULL
                       INVOKE mReference(lsIndex) "setStatusText"
                                                        USING aString
                       INVOKE aString "finalize" RETURNING aString
                   END-IF
               END-IF

      * Setup accelerator key (if specified)
               IF K-keyAsNum of mAccelKey(lsIndex) NOT = 0
                   INVOKE mReference(lsIndex) "setAccelerator"
                                                USING mAccelKey(lsIndex)
               END-IF

           END-IF
           .

       Display-Warning-Message SECTION.
           INVOKE MessageBox "new" USING aWindow
                               RETURNING aMessageBox

           INVOKE aMessageBox "setTitleZ"
                       USING z"Dialog System Toolbar Template"

           PERFORM String-From-tempBuffer
           INVOKE aMessageBox "setMessage" USING aString
           INVOKE aString "finalize" RETURNING aString

           INVOKE aMessageBox "setTypeWarning"

           INVOKE aMessageBox "show"

           INVOKE aMessageBox "finalize" RETURNING aMessageBox
           .

       GetEventHwnd SECTION.
      * Get the handle of the window the event occured on
           INVOKE anEvent "getObject" RETURNING menuItem
           INVOKE menuItem "rootObject" RETURNING aWindow
           INVOKE aWindow "getID" RETURNING aHwnd
           .

       Register-Callbacks SECTION.
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > defaultMenuItems
               PERFORM Register-Callback
           END-PERFORM

           PERFORM saveDataBlockPointer
           .

       Register-Callback SECTION.
           IF mCallback(i) NOT = SPACES
               INVOKE EntryCallback "new" USING mCallback(i)
                                      RETURNING aCallback
               MOVE p2ce-menuSelect TO j
               INVOKE mReference(i) "setEvent" USING j aCallback
               INVOKE aCallback "finalize" RETURNING aCallback
           END-IF
           .

