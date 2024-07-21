      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0801.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0801.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:14:22   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:18   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-HISTORYTAB-GESAMT.
         10 EA-NI-HISTORYTAB-TAB.
           20 EA-NI-HISTORYTAB-ELE PIC X
                  OCCURS 14 TIMES.
         10 EA-NI-HISTORYTAB
                 REDEFINES EA-NI-HISTORYTAB-TAB.
           20 EA-NI-HID                      PIC X.
           20 EA-NI-HISTUSER                 PIC X.
           20 EA-NI-FKREFERENZ               PIC X.
           20 EA-NI-ATTRIBUTENAME            PIC X.
           20 EA-NI-KEY1                     PIC X.
           20 EA-NI-KEY2                     PIC X.
           20 EA-NI-KEY3                     PIC X.
           20 EA-NI-KEY4                     PIC X.
           20 EA-NI-KEY5                     PIC X.
           20 EA-NI-KEY6                     PIC X.
           20 EA-NI-CLASSNAME                PIC X.
           20 EA-NI-PRESENTATIONSTRING       PIC X.
           20 EA-NI-DELETEFLAG               PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-HISTORYTAB.
           20 EA-HID                         PIC X(13).
           20 EA-HISTUSER                    PIC X(10).
           20 EA-FKREFERENZ                  PIC X(13).
           20 EA-ATTRIBUTENAME.
              49 EA-ATTRIBUTENAME-LEN        PIC -----9    .
              49 EA-ATTRIBUTENAME-TEXT       PIC X(128).
           20 EA-KEY1                        PIC X(13).
           20 EA-KEY2                        PIC X(13).
           20 EA-KEY3                        PIC X(13).
           20 EA-KEY4                        PIC X(13).
           20 EA-KEY5                        PIC X(13).
           20 EA-KEY6                        PIC X(13).
           20 EA-CLASSNAME.
              49 EA-CLASSNAME-LEN            PIC -----9    .
              49 EA-CLASSNAME-TEXT           PIC X(128).
           20 EA-PRESENTATIONSTRING.
              49 EA-PRESENTATIONSTRING-LEN   PIC -----9    .
              49 EA-PRESENTATIONSTRING-TEXT  PIC X(200).
           20 EA-DELETEFLAG                  PIC X(01).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
