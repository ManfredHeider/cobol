      *-- This copyfile contains code Specific to the COBOL CLOCK SetAlarm 
      *-- method.  This method will force the generation of an OCX event, 
      *-- for which the "Alarm" Entry point defined below will be called.

      *-- You will need to adapt this code to meet the requirements of 
      *-- the OCX and methods you are using.

      *      
      * Call the CLOCInvk program to do the OCX method invoke for us.
      * This program is needed because programs which invoke OLE routines must 
      * pass parameter type information alongside the parameter itself.
 
      * To implement this a program must use the OOCTRL(+P) directive. Use of 
      * this directive in OCXCTRL will cause problems for other called routines


      * Additionally the parameter types are specific to the OCX being invoked, 
      * and therefore need to be of the correct type. In this program they are 
      * stored only as PIC x(4) comp-5 items for both Numerics and Object 
      * references for character array instances. The CLOCINVK program will
      * derive the data items to the correct type for each method.

CCCPY1*   CALL "CLOCINVK" USING Object-Reference Parm-Name
CCC004*                                          P1
CCC004*                                          P2
CCC004*                                          P3
CCC004*                                          P4
CCC004*                                          P5
CCC004*                                          P6
CCC004*                                          P7
CCC004*   .

       SetEvent-Input SECTION.

      *    Setup the parameters for the OCX event callback registration
      *    The OCX Event name 

           MOVE "SoundAlarm" TO Term-Field
           PERFORM Null-Terminate-Routine
           MOVE Term-Field TO MessageName


      *    The Entry point name 

           MOVE "Alarm " TO Term-Field
           PERFORM Null-Terminate-Routine
           MOVE Term-Field TO CodeName.


       The-Alarm-Has-Gone-Off section.
       Entry "Alarm" USING anEvent.

      *     The set alarm time has been reached

      *     Get an Object reference for the control the event occurred on
            INVOKE anEvent "GetObject" RETURNING anOleControl

      *     Identify the window instance for this control
            INVOKE anOleControl "getAncestor" RETURNING aWindow
            INVOKE aWindow "GetID" RETURNING aHwnd

            PERFORM GetDataBlockPointer

      *     Determine number of parameters associated with this event

            INVOKE anEvent "GetParameterCount"
                                          RETURNING Numeric-Value

            MOVE 1 TO Function-Length
            MOVE SPACES TO IO-Text-Buffer
            MOVE SPACES TO IO-Text-Buffer2

            MOVE "Alarm Time:" TO Term-Field
            PERFORM Null-Terminate-Routine


      *     String manipulation example using CharacterArrays

            INVOKE CharacterArray "withValue" USING Term-Field
                                     RETURNING aCharArray

            MOVE 1 TO k

      *     Return here is a characterarray instance,
      *     GetParameterasInteger is also available if required

            INVOKE anEvent "GetParameterAsString" USING k
                                                 RETURNING anObject
            INVOKE aCharArray "concat" USING anObject
                                   RETURNING aCharArray

            INVOKE anObject "setValue" USING Z": "

            INVOKE aCharArray "concat" USING anObject
                                   RETURNING aCharArray

      *     Loop around getting each parameter as a string, and updating
      *     the character array instance with the results we want
            PERFORM VARYING k FROM 2 BY 1 UNTIL k > Numeric-Value

               INVOKE anEvent "GetParameterAsString" USING k
                                                 RETURNING anObject

               INVOKE aCharArray "concat" USING anObject
                                      RETURNING aCharArray

               IF k NOT = Numeric-Value
                  INVOKE anObject "setValue" USING Z", "
                  INVOKE aCharArray "concat" USING anObject
                                         RETURNING aCharArray
               END-IF
            END-PERFORM

            MOVE 256 TO i
            INVOKE aCharArray "getValueWithSize" USING i
                                            RETURNING IO-Text-Buffer2

            INVOKE anObject "finalize"   RETURNING anObject
            INVOKE aCharArray "finalize" RETURNING aCharArray

      *-----34570 is a user defined event number that the DS screenset
      *     responds to - see DS/PanelsV2 documentation for further
      *     details

            MOVE 34570  TO Event-Type
            PERFORM Post-User-Event-Routine

            INVOKE anEvent "finalize" RETURNING anEvent

            EXIT PROGRAM.

