
       Create-ActiveX-Instance section.

      * Do the create having been passed the Panels2 Session ID, the
      * parent window handle, the co-ordinates for the creation of the
      * control, the stored property defaults data (or programmatic ID)
      * and the datablock master field associated with this control.

          MOVE EntryPanels-Session-ID TO Queue-ID
          MOVE EntryWindow-Data       TO aHwnd
          PERFORM GetWindow-Instance-Routine

      *   Initialize for OLE support
          INVOKE EventManager "getCurrentManager"
                                                RETURNING wsEventManager
          INVOKE wsEventManager "initializeOLE"

      *   Controls on MDI children need different treatment so determine
      *   if this is an instance of the mdiChild class
          MOVE 0 TO Frame-State
          INVOKE aWindow "ClassName" RETURNING anObject
          MOVE 8 TO i
          INVOKE anObject "getValueWithSize" USING i
                                         RETURNING Term-Field
          IF Term-Field(1:8) NOT = "mdichild"
             INVOKE aWindow "IsContainer" RETURNING ContainerState

      *      If not done already...
             IF IsNotContainer

      *         Register the window instance for use as an OLE container
                INVOKE aWindow "makeOLEContainer"

      *         Remove the clipchildren flag
                INVOKE aWindow "removeBasicApiFlags" USING
                                            BY VALUE WS-CLIPCHILDREN
             END-IF
          ELSE
             SET MDI-Frame TO TRUE
             SET mdiChild TO aWindow

             INVOKE mdiChild  "getAncestor" RETURNING mdiClient
             INVOKE mdiClient "getAncestor" RETURNING aWindow

             *> For MDI we make the Frame the container
             INVOKE aWindow "IsContainer" RETURNING ContainerState
             IF IsNotContainer
                INVOKE aWindow "makeOLEContainer"
             END-IF

             *> Place a Client pane on the MDI child,
             INVOKE mdiChild "defineClientPane" USING mdichild
                                            RETURNING mdiChildView
             INVOKE mdiChildView "noBorder"

      *      Convert the native ActiveX co-ordinates to P2 generic in
      *      order to size the client pane
             MOVE EntryPosition-x TO i
             MOVE EntryPosition-y TO j
             INVOKE mdiChildView "scaleXYUp" USING i j

             MOVE EntrySize-width TO k
             MOVE EntrySize-height TO l
             INVOKE mdiChildView "scaleXYUp" using k l

             INVOKE mdiChildView "setRectangle" using i j k l

             *> make the Client pane and OLEView taking a null document
             SET anOLEDocument TO NULL
             INVOKE mdiChildview "makeOLEView"  using aWindow
                                                      anOLEDocument
             SET aWindow TO mdiChildView
          END-IF

      *   Set the style so that borders arent emphasised
          INVOKE aWindow "ShowObjects" USING ObjEmphasis


      *   If properties were saved for this Control at definition time,
      *   then use them to instanciate the control, otherwise just
      *   use the programmatic ID.

          IF EntryOCXProperty-Size > 0
      *      Setup an OLEStream object containing the property data
      *      stored in the screenset
             INVOKE OLEStream "fromMemory" USING EntryOCXProperty-Size
                                       BY VALUE EntryOCXProperty-Pointer
                                      RETURNING anOLEStream

             INVOKE OLEControlSite "fromStream" USING aWindow
                                                      anOLEStream
                                            RETURNING anOLEControlSite

      *      Screensets saved with DS from NetExpress V3.0 now include
      *      additional license key data (passed as an object reference
      *      by DSGRUN,) which must be supplied at run time if licensing
      *      is implemented by the activex you use
             IF EntryOCXLicense-Key NOT = NULL
                INVOKE anOLEControlSite "SetLicenseKey" USING
                                                     EntryOCXLicense-Key
             END-IF
          ELSE

      *      Create a chararray instance for the ProgID
             MOVE EntryText-Buffer                TO CodeName
             INVOKE CharacterArray "withValue" USING CodeName
                                           RETURNING aPROGID

      *      Create a new site on the Window for the control
             INVOKE OLEControlSite "new" USING aWindow
                                               aProgID
                                     RETURNING anOLEControlSite

             INVOKE aProgID "finalize" RETURNING aProgID
          END-IF

      *   Callback registration can be done directly after create,
      *   as DSGRUN passes a pointer to the datablock (see
      *   SaveDataBlockPointer section.)
          PERFORM Register-Callbacks

      *   Show and size it!
          INVOKE anOLEControlSite "show" RETURNING ReturnValue

          MOVE EntryPosition-x TO i
          MOVE EntryPosition-y TO j
          IF MDI-Frame
             MOVE 0 TO i j
          END-IF
          INVOKE anOLEControlSite "setRectangle" USING i j
                                                EntrySize-Width
                                                EntrySize-Height

          INVOKE anOLEControlSite "getControlWindow" RETURNING anObject
          INVOKE anObject "placeonBottom"

      *   Store the ActiveX object reference in the datablock
          SET EntryObject-Reference TO anOLEControlSite

          .
