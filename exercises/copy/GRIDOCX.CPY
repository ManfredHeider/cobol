
       SetEvent-Input SECTION.

      *   Registering a callback causes a block of your COBOL code
      *   to be executed

      *   Here, when a known OCX event occurs, call the entry point
      *   detailed in CodeName variable. The entry point code (and the
      *   callback input parameter definitions) exist in this  program


      *   Setup the parameters for the OCX event callback registration
      *   We're interested in 4 events - register a callback for each
      *
      *   Note the change from NetExpress V2.0, we register an eventID
      *   rather than an event name. This is quicker and may be done
      *   before control creation, enabling receipt of events fired
      *   immediately the control is created.
      *------------------------------------------------------------
      *
      *    an ID for the Entry point name
           move ID-Change to eventIndex
           move programID & z"OnChange" to entryName

      *    Invoke the control instance object, (setEventToEntry method)
      *    to register the event ID on which a callback should occur
           invoke anOLEControlSite "setEventToEntry"
                                              using eventIndex
                                                    entryName

      *------------------------------------------------------------

           move ID-RowSel to eventIndex
           move programID & z"OnRowSel" to entryName
           invoke anOLEControlSite "setEventToEntry"
                                              using eventIndex
                                                    entryName

      *------------------------------------------------------------

           move ID-RowUnSel to eventIndex
           move programID & z"OnRowUnSel" to entryName
           invoke anOLEControlSite "setEventToEntry"
                                              using eventIndex
                                                    entryName


      *------------------------------------------------------------

      *    Always register a callback to delete the dictionary item
      *    when the containing window is deleted.
           MOVE ProgramID & z"OnClosedWindow" TO MessageName
           INVOKE EntryCallBack "new" USING MessageName
                                  RETURNING aCallback
           MOVE P2Ce-Destroyed TO i
           INVOKE anOLEControlSite "SetEvent" USING i aCallback
           INVOKE aCallback "finalize" RETURNING aCallback
           .

      *---------------------------------------------------------------
      * Callback codeblocks follow:

       Data-has-Changed section.
       ENTRY ProgramID & "OnChange" using anEvent.

      *    The Cell contents update has finished
           PERFORM Get-Object-And-Event-Data

           SET Object-Reference TO anOLEControlSite

           MOVE "DATA-FROM-ROW" TO Call-Function
           PERFORM DO-ActiveX-Function

           EXIT PROGRAM
           .


       A-Row-Has-Been-Deleted section.
       ENTRY ProgramID & "OnDeletedRow" using anEvent.

      *    We need to adjust the datablock group so it matches the grid.

           PERFORM Get-Object-And-Event-Data

           MOVE "GROUP-ELEMENT-DELETE" TO Call-Function
           PERFORM DO-ActiveX-Function

           EXIT PROGRAM
           .


       A-Row-Has-Been-Selected section.
       ENTRY ProgramID & "OnRowSel" using anEvent.

      *    We have set the control to not detect the Delete key.

      *    All we want to do here is post an event back into the
      *    screenset, to enable the Delete Row pushbutton

           PERFORM Get-Object-And-Event-Data

           MOVE 40007 TO Event-Type
           PERFORM Post-User-Event-Routine

           EXIT PROGRAM
           .


       A-Row-Has-Been-UnSelected section.
       ENTRY ProgramID & "OnRowUnSel" using anEvent.

      *    We have set the control to not detect the Delete key.

      *    All we want to do here is post an event back into the
      *    screenset, to disable the Delete Row pushbutton

           PERFORM Get-Object-And-Event-Data

           MOVE 40008 TO Event-Type
           PERFORM Post-User-Event-Routine

           EXIT PROGRAM
           .

       The-Object-is-Destroyed section.
       Entry ProgramID & "OnClosedWindow" USING anEvent.

          INVOKE anEvent "GetObject" RETURNING anOLEControlSite
          INVOKE anOLEControlSite "getAncestor" RETURNING aWindow

          SET DataBlock-Pointer TO NULL
          INVOKE aDictionary "AtOrNil" USING aWindow
                                RETURNING DataBlock-Pointer
          IF DataBlock-Pointer NOT = NULL
             INVOKE aDictionary "removeKey" USING aWindow
          END-IF
          SET aWindow TO NULL
          EXIT PROGRAM
          .


      *---------------------------------------------------------------

       Get-Object-And-Event-Data section.

      *    Get an Object reference for the control the event occurred on
           INVOKE anEvent "GetObject" RETURNING anOLEControlSite

      *    Identify the window instance for this control
      *    INVOKE anOLEControlSite "getAncestor" RETURNING aWindow
           INVOKE anOLEControlSite "getOLEDocument"
                                                 RETURNING anOLEDocument

           INVOKE anOLEDocument "GetActiveView" RETURNING aWindow

           PERFORM GetDataBlockPointer

      *    Cell edit events return a Row and a Column index of the item
      *    changed in the grid, returned as long values x(4) comp-5.

      *    These values are placed in the datablock and passed to the
      *    gridinvk program to query/set values and synchronise the
      *    datablock fields with the grid

           MOVE 1 TO k
           INVOKE anEvent "GetParameterAsInteger" USING k
                                       RETURNING Numeric-Value
           ADD 1 TO k
           INVOKE anEvent "GetParameterAsInteger" USING k
                                       RETURNING Numeric-Value2

           .
