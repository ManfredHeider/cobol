      * Include this copyfile in your toolbar template program
      * to provide access to menu and toolbar maintenance functions.

       Update-Menu-Text SECTION.
      * Update the menu text of the menu item index numeric-value
      * using the text in menu-text.
           IF mReference(Numeric-Value) NOT = NULL
               MOVE Menu-Text TO tempBuffer
               PERFORM String-From-tempBuffer
               INVOKE mReference(Numeric-Value) "setLabel"
                                                   USING aString

      * Update the menu text stored in the menu item table.
               INVOKE aString "size" RETURNING i
               INVOKE aString "finalize" RETURNING aString
               MOVE Menu-Text TO mString(Numeric-Value)
               MOVE x"0000"   TO mString(Numeric-Value)(i + 1:2)
           END-IF
           .

       Toggle-CheckMark SECTION..
           IF mReference(Numeric-Value) NOT = NULL
               INVOKE mReference(Numeric-Value) "toggle"
           END-IF
           .

       Enable-Object SECTION.
           IF mReference(Numeric-Value) NOT = NULL
               INVOKE mReference(Numeric-Value) "enable"
           END-IF
           .

       Disable-Object SECTION.
           IF mReference(Numeric-Value) NOT = NULL
               INVOKE mReference(Numeric-Value) "disable"
           END-IF
           .

       Delete-Menu-Item SECTION.
           IF mReference(Numeric-Value) NOT = NULL
              AND Numeric-Value > 0
              AND Numeric-Value <= numberOfMenuItems

      * Get hold of the menu bar reference
               SET aMenuBar TO mReference(Numeric-Value)
               MOVE 0 TO wsBool
               PERFORM UNTIL isTrue
                   INVOKE aMenuBar "getAncestor" RETURNING aMenuBar
                   INVOKE aMenuBar "isMenuBar" RETURNING wsBool
               END-PERFORM

      * Delete any associated tool bar button.
      * Note: This code assumes each menu item has no more than
      *       one associated button.
               PERFORM VARYING i FROM 1 BY 1 UNTIL i > numberOfButtons
                   IF bMenuIndex(i) = Numeric-Value
                       IF bReference(i) NOT = NULL
                           INVOKE bReference(i) "finalize"
                                               RETURNING bReference(i)
                           SET bReference(i) TO NULL
                           EXIT PERFORM
                       END-IF
                   END-IF
               END-PERFORM

      * Delete the menu item
               INVOKE mReference(Numeric-Value) "destroy"

      * Re-draw the menu bar just in case the item deleted
      * was visible
               INVOKE aMenuBar "reDraw"

               SET mReference(Numeric-Value) TO NULL
           END-IF
           .

       Add-Menu-Item SECTION.
           IF numberOfMenuItems < maxMenuItems

      * If an invalid menu index was specified, don't add the menu item.
           IF Numeric-Value < 1
              OR Numeric-Value > numberOfMenuItems
               MOVE 1                  TO displayNumber
               MOVE numberOfMenuItems  TO displayNumber2
               MOVE SPACES TO tempBuffer
               STRING "Please specify a menu index in the range "
                                               DELIMITED BY SIZE
                      displayNumber            DELIMITED BY SIZE
                      " to "                   DELIMITED BY SIZE
                      displayNumber2           DELIMITED BY SIZE
                      "."                      DELIMITED BY SIZE
                   INTO tempBuffer
               END-STRING
               PERFORM Display-Warning-Message

               EXIT SECTION
           END-IF

      * If an invalid menu item was specified, don't add the menu item.
           IF mReference(Numeric-Value) = NULL
               MOVE Numeric-Value TO displayNumber
               MOVE SPACES TO tempBuffer
               STRING "The object reference stored for menu index "
                                               DELIMITED BY SIZE
                      displayNumber            DELIMITED BY SIZE
                      " was null,"&x"0A"
                      &"therefore a menu item cannot be added."
                                               DELIMITED BY SIZE
                   INTO tempBuffer
               END-STRING
               PERFORM Display-Warning-Message

               EXIT SECTION
           END-IF

           MOVE Menu-Text TO tempBuffer
           PERFORM String-From-tempBuffer
           ADD 1 TO numberOfMenuItems

      * Flag indicating type of menu item to add
           MOVE Numeric-Value2 TO newMenuType

      * Add the menu item

           IF addBefore
      * Numeric-Value is the menu index to insert before
               INVOKE mReference(Numeric-Value) "getAncestor"
                                       RETURNING subMenuReference
               IF addSubMenu
                   INVOKE subMenuReference "addSubMenuBefore"
                                    USING aString
                                          mReference(Numeric-Value)
                                RETURNING mReference(numberOfMenuItems)

               ELSE
                   IF addSeparator
                       INVOKE subMenuReference "addSeparatorBefore"
                                    USING mReference(Numeric-Value)
                                RETURNING mReference(numberOfMenuItems)
                   ELSE
                       INVOKE subMenuReference "addItemBefore"
                                    USING aString
                                          mReference(Numeric-Value)
                                RETURNING mReference(numberOfMenuItems)
                   END-IF

               END-IF


           ELSE
      * Numeric-Value is the menu index of the sub menu or a menu item
      * on the sub menu to add to the end of.
               INVOKE mReference(Numeric-Value) "isSubMenu"
                                           RETURNING wsBool
               IF isFalse
                   INVOKE mReference(Numeric-Value) "getAncestor"
                                           RETURNING subMenuReference
               ELSE
                   SET subMenuReference
                               TO mReference(Numeric-Value)
               END-IF


               IF addSubMenu
                   INVOKE subMenuReference "addSubMenu"
                                     USING aString
                                 RETURNING mReference(numberOfMenuItems)

               ELSE
                   IF addSeparator
                       INVOKE subMenuReference "addSeparator"
                                 RETURNING mReference(numberOfMenuItems)
                   ELSE
                       INVOKE subMenuReference "addItem"
                                     USING aString
                                 RETURNING mReference(numberOfMenuItems)
                   END-IF

               END-IF

           END-IF

           *> Store the menu text in the table
           INVOKE aString "getValue"
                               RETURNING mString(numberOfMenuItems)

           *> NULL terminate the stored menu text
           INVOKE aString "size" RETURNING i
           MOVE x"0000" TO mString(numberOfMenuItems)(i + 1:2)

           INVOKE aString "finalize" RETURNING aString

      * Return index of new menu item. This will be displayed
      * in the "Menu index" entry field once the new menu item
      * has been added.
           MOVE numberOfMenuItems TO Numeric-Value

      * Add hint text (if specified)
           IF Menu-Hint-Text NOT = spaces
               MOVE Menu-Hint-Text TO tempBuffer
               PERFORM String-From-tempBuffer
               INVOKE mReference(numberOfMenuItems) "setStatusText"
                                               USING aString
               INVOKE aString "finalize" RETURNING aString
           END-IF

      * Setup callback (if specified)
           MOVE Callback-Entry-Name TO mCallback(numberOfMenuItems)
           MOVE numberOfMenuItems TO i
           PERFORM Register-Callback

      * Setup accelerator key (if specified)
           IF Accel-Key NOT = 0
               MOVE Accel-Flags
                       TO K-keyFlags of mAccelKey(numberOfMenuItems)
               MOVE Accel-Key
                       TO K-keyAsNum of mAccelKey(numberOfMenuItems)
               INVOKE mReference(numberOfMenuItems) "setAccelerator"
                                      USING mAccelKey(numberOfMenuItems)
           END-IF

           ELSE
               MOVE maxMenuItems TO displayNumber
               MOVE SPACES TO tempBuffer
               STRING "Cannot add more than "        DELIMITED BY SIZE
                      displayNumber                  DELIMITED BY SIZE
                      " menu items."                 DELIMITED BY SIZE
                       INTO tempBuffer
                   END-STRING
               PERFORM Display-Warning-Message

           END-IF
           .

       Add-Toolbar-Button SECTION.

           IF numberOfButtons < maxButtons

      * Check that a valid menu reference has been supplied, if not
      * then don't add the toolbar button.
           IF Numeric-Value NOT = 0 *> Not adding a separator
               IF Numeric-Value <= numberOfMenuItems
                   IF mReference(Numeric-Value) = NULL
                    MOVE Numeric-Value TO displayNumber
                    MOVE SPACES TO tempBuffer
                    STRING "The object reference stored for menu index "
                                                DELIMITED BY SIZE
                       displayNumber            DELIMITED BY SIZE
                       " was null."&x"0A"
                       &"A button must be associated with a menu item,"
                       &x"0A"&"therefore a button cannot be added."
                                               DELIMITED BY SIZE
                       INTO tempBuffer
                    END-STRING
                    PERFORM Display-Warning-Message

                    EXIT SECTION
                   END-IF
               ELSE
                   MOVE 1                  TO displayNumber
                   MOVE numberOfMenuItems  TO displayNumber2
                   MOVE SPACES TO tempBuffer
                   STRING "Please specify a menu index in the range "
                                                   DELIMITED BY SIZE
                          displayNumber            DELIMITED BY SIZE
                          " to "                   DELIMITED BY SIZE
                          displayNumber2           DELIMITED BY SIZE
                          "."                      DELIMITED BY SIZE
                       INTO tempBuffer
                   END-STRING
                   PERFORM Display-Warning-Message

                   EXIT SECTION
               END-IF

      * Only allow one button per menu item on this toolbar
      * (this is a restriction imposed by the Win32 API)
               PERFORM VARYING i FROM 1 BY 1 UNTIL i > numberOfButtons
                   IF bMenuIndex(i) = Numeric-Value
                      AND bReference(i) NOT = NULL
                       MOVE Numeric-Value TO displayNumber
                       MOVE SPACES TO tempBuffer
                       STRING "Menu index "        DELIMITED BY SIZE
                              displayNumber        DELIMITED BY SIZE
                            " already has an associated toolbar button,"
                            &x"0A"&"therefore a button cannot be added."
                                                   DELIMITED BY SIZE
                           INTO tempBuffer
                       END-STRING
                       PERFORM Display-Warning-Message

                       EXIT SECTION
                   END-IF
               END-PERFORM

           END-IF

           ADD  1 TO numberOfButtons

      * Setup menu item index associated with this tool bar button
           MOVE Numeric-Value TO bMenuIndex(numberOfButtons)

           MOVE Resource-File TO tempBuffer
           PERFORM String-From-tempBuffer

      * Load bitmap resource file
           INVOKE module "new" USING aString
                           RETURNING aResource

      * Add button bitmap
           MOVE 1 TO numberOfImages

           MOVE Resource-ID TO bBitmapID(numberOfButtons)
           MOVE numberOfButtons TO i
           PERFORM Add-Button-Bitmap
           INVOKE aToolBar "setImageList" USING buttonImageList

      * Add tool bar button and set tool tip (if specified)
           MOVE Tool-Tip-Text TO bString(numberOfButtons)
           MOVE numberOfButtons TO wsIndex
           PERFORM Add-Indexed-Button

      * If Numeric-Value is non-zero then insert button before
      * button index Numeric-Value2, otherwise add button to end
      * of tool bar.
           IF Numeric-Value2 > 0
              AND Numeric-Value2 < numberOfButtons
               INVOKE bReference(numberOfButtons) "insertAt"
                                            USING Numeric-Value2
           END-IF

      * Create and show the new button
           INVOKE bReference(numberOfButtons) "create"
           INVOKE bReference(numberOfButtons) "show"

           ELSE
               MOVE maxButtons TO displayNumber
               MOVE SPACES TO tempBuffer
               STRING "Cannot add more than "        DELIMITED BY SIZE
                      displayNumber                  DELIMITED BY SIZE
                      " buttons."                    DELIMITED BY SIZE
                   INTO tempBuffer
               END-STRING
               PERFORM Display-Warning-Message

           END-IF
           .
