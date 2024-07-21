      ****************************************************************
      * Copyright (C) 1993-2002 Micro Focus.                         *
      * All Rights Reserved                                          *
      * This demonstration program is provided for use by users of   *
      * Micro Focus products and may be used, modified and           *
      * distributed as part of your application provided that you    *
      * properly acknowledge the copyright of Micro Focus in this    *
      * material.                                                    *
      ****************************************************************


       working-storage section.

       01 aWindow              usage object reference value null.
       01 aStorageClass        usage object reference value null.
       01 anAssocTemplate      usage object reference.
       01 aDictionary          usage object reference.
       01 anObject             usage object reference.
       01 aCharArray           usage object reference.
       01 aCallback            usage object reference.
       01 aForeColor           usage object reference.
       01 aBackColor           usage object reference.
       01 aCursor              usage object reference.
       01 aFont                usage object reference.
       01 aMessageBox          usage object reference.
       01 wsEventManager       usage object reference.

       01 DataBlock-Pointer    usage pointer.
       01 Term-Field           Pic X(256).
       01 MessageName          Pic X(255).
       01 Function-Length      Pic X(4) comp-5.
       01 Invalid-Function     Pic X(60)
          value "Invalid function Passed to routine".
       01 ContainerState       PIC X comp-5 value 0.
          88 IsContainer       VALUE 1.
          88 IsNotContainer    VALUE 0.
       01 Frame-State              pic x comp-5 value 0.
         88 MDI-Frame              value 1.
       01 mdiChild                 usage object reference.
       01 mdiChildView             usage object reference.
       01 mdiClient                usage object reference.

       01 i                    Pic X(4) comp-5.
       01 j                    Pic X(4) comp-5.
       01 k                    Pic X(4) comp-5.
       01 l                    Pic X(4) comp-5.
       01 wsSize               Pic X(4) comp-5.
       01 Numeric-8Bytes       Pic 9(8).
       01                      Pic X comp-5 VALUE 1.
         88 Not1stTimeThru     VALUE 0.
         88 1stTimeThru        VALUE 1.

       01 aHwnd                 Pic 9(9) comp-5.
       01 Event-Type            Pic 9(4) comp-5.
       01 Queue-ID              Pic 9(9) comp-5.

      *   End of CopyFile

