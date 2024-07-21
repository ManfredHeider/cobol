      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0612.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0612.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:30:34   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:04   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-FENSTER-VORGABEN-GESAMT.
         10 EA-NI-FENSTER-VORGABEN-TAB.
           20 EA-NI-FENSTER-VORGABEN-ELE PIC X
                  OCCURS 11 TIMES.
         10 EA-NI-FENSTER-VORGABEN
                 REDEFINES EA-NI-FENSTER-VORGABEN-TAB.
           20 EA-NI-FEVBENUTZERID            PIC X.
           20 EA-NI-FEVBENUTZERROLLE         PIC X.
           20 EA-NI-FEVHORIZONTALGR          PIC X.
           20 EA-NI-FEVVERTIKALGR            PIC X.
           20 EA-NI-FEVXPOSLINKSOBEN         PIC X.
           20 EA-NI-FEVYPOSLINKSOBEN         PIC X.
           20 EA-NI-FEVHINTERGRUNDFAR        PIC X.
           20 EA-NI-FEVSTATUSZEILE           PIC X.
           20 EA-NI-FEVFENSTERNAME           PIC X.
           20 EA-NI-FEVFELDMITFOCUS          PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-FENSTER-VORGABEN.
           20 EA-FEVBENUTZERID               PIC X(13).
           20 EA-FEVBENUTZERROLLE            PIC X(01).
           20 EA-FEVHORIZONTALGR             PIC --------9 .
           20 EA-FEVVERTIKALGR               PIC --------9 .
           20 EA-FEVXPOSLINKSOBEN            PIC --------9 .
           20 EA-FEVYPOSLINKSOBEN            PIC --------9 .
           20 EA-FEVHINTERGRUNDFAR           PIC --------9 .
           20 EA-FEVSTATUSZEILE              PIC X(01).
           20 EA-FEVFENSTERNAME.
              49 EA-FEVFENSTERNAME-LEN       PIC -----9    .
              49 EA-FEVFENSTERNAME-TEXT      PIC X(200).
           20 EA-FEVFELDMITFOCUS.
              49 EA-FEVFELDMITFOCUS-LEN      PIC -----9    .
              49 EA-FEVFELDMITFOCUS-TEXT     PIC X(200).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
