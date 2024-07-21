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
       On-Exit SECTION.
       Entry ProgramID & "OnExit" USING anEvent.
           PERFORM GetEventHwnd
           MOVE 34650  TO Event-Type
           PERFORM Post-User-Event-Routine
           EXIT PROGRAM.

       On-Arrange-Icons SECTION.
       Entry ProgramID & "OnArrangeIcons" USING anEvent.
           PERFORM GetEventHwnd
           MOVE 34651  TO Event-Type
           PERFORM Post-User-Event-Routine
           EXIT PROGRAM.

       On-Cascade SECTION.
       Entry ProgramID & "OnCascade" USING anEvent.
           PERFORM GetEventHwnd
           MOVE 34652  TO Event-Type
           PERFORM Post-User-Event-Routine
           EXIT PROGRAM.

       On-Tile-Horiz SECTION.
       Entry ProgramID & "OnTileHoriz" USING anEvent.
           PERFORM GetEventHwnd
           MOVE 34653  TO Event-Type
           PERFORM Post-User-Event-Routine
           EXIT PROGRAM.

       On-Tile-Vert SECTION.
       Entry ProgramID & "OnTileVert" USING anEvent.
           PERFORM GetEventHwnd
           MOVE 34654  TO Event-Type
           PERFORM Post-User-Event-Routine
           EXIT PROGRAM.

       On-Set-Win-Focus SECTION.
       ENTRY ProgramID & "OnSetWinFocus" USING anEvent.
           PERFORM GetEventHwnd
      * Get the menu text of the selected item
           INVOKE menuItem "getLabel" RETURNING aString
           MOVE SPACES TO tempBuffer
           INVOKE aString "getValue" RETURNING tempBuffer
           INVOKE aString "finalize" RETURNING aString


      * Return the index of the window to set focus on in Numeric-Value
           PERFORM GetDataBlockPointer
           MOVE windowNumber TO Numeric-Value

           MOVE 34655  TO Event-Type
           PERFORM Post-User-Event-Routine

           EXIT PROGRAM.

