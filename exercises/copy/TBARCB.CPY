      *>----------------------------------------------------------------
      *> Callback entry points for menu items defined in tbardefn.cpy
      *>----------------------------------------------------------------

      *
      * This copyfile contains sections of code that can be executed
      * when a menu item is selected or a toolbar button is clicked.
      *
      * When a menu item is selected, a named entry point can be
      * called to process the menu event. The entry point name must
      * be defined in the appropriate menu item record within the
      * mData table (defined in tbardefn.cpy in this example).
      *
      * It is recommended that you prefix each entry point name with
      * the program ID of your program (in this case TBAR) to help
      * ensure that your entry point names are unique. Examples of this
      * shown in the code below.
      *
      * If required, a Panels2 user event may be posted to the window
      * owning the menu bar to perform some processing using dialog
      * script on the window. In the screenset, the user event
      * code can be used to determine what action to perform.
      *
      * Use the code for the entry points below as an exmaple to add
      * your own code to process menu selections.
      *
       On-Item-Selected SECTION.
       Entry ProgramID & "OnItemSelected" USING anEvent.
           INVOKE aToolbar "setInputFocus"
           PERFORM GetEventHwnd
           PERFORM GetDataBlockPointer

           *> Build text string containing name of menu item selected
           MOVE SPACES TO Msg-Box-Text
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > numberOfMenuItems
               IF mReference(i) = menuItem
                   STRING "Menu item """       DELIMITED BY SIZE
                          mString(i)           DELIMITED BY x"00"
                          """ was selected"    DELIMITED BY SIZE
                          INTO Msg-Box-Text
                   END-STRING
                   EXIT PERFORM
               END-IF
           END-PERFORM

           *> Post a Panels2 User Event to the Dialog System window
           *> to display a message box containing the text of the
           *> menu item that was selected
           IF Msg-Box-Text NOT = SPACES

               *> Remove any '&' or '~' characters from the string
               MOVE 1 TO j
               PERFORM VARYING i FROM 1 BY 1 UNTIL i > 255
                   IF Msg-Box-Text(i:1) NOT = '&' AND NOT = '~'
                       MOVE Msg-Box-Text(i:1) TO Msg-Box-Text(j:1)
                       ADD 1 TO j
                   END-IF
               END-PERFORM

               MOVE 34600  TO Event-Type
               PERFORM Post-User-Event-Routine

           END-IF

           EXIT PROGRAM.

       On-Exit SECTION.
       ENTRY ProgramID & "OnExit" USING anEvent.
           INVOKE aToolbar "setInputFocus"
           PERFORM GetEventHwnd
           MOVE 34601  TO Event-Type
           PERFORM Post-User-Event-Routine

           EXIT PROGRAM.

       On-Toggle SECTION.
       ENTRY ProgramID & "OnToggle" USING anEvent.
           INVOKE aToolbar "setInputFocus"
           INVOKE anEvent "getObject" RETURNING menuItem
           INVOKE menuItem "toggle"

           EXIT PROGRAM.

       On-Help SECTION.
       ENTRY ProgramID & "OnHelp" USING anEvent.
           INVOKE aToolbar "setInputFocus"
           PERFORM GetEventHwnd
           MOVE 34602  TO Event-Type
           PERFORM Post-User-Event-Routine

           EXIT PROGRAM.
