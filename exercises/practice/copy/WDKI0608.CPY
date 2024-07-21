      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0608.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0608.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:13:40   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:04   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-GRUPPEBENUTZERBEZ-GESAMT.
         10 EA-NI-GRUPPEBENUTZERBEZ-TAB.
           20 EA-NI-GRUPPEBENUTZERBEZ-ELE PIC X
                  OCCURS 4 TIMES.
         10 EA-NI-GRUPPEBENUTZERBEZ
                 REDEFINES EA-NI-GRUPPEBENUTZERBEZ-TAB.
           20 EA-NI-GBBMITAROLLENID          PIC X.
           20 EA-NI-GBBMITAROLLENKZ          PIC X.
           20 EA-NI-GBBGRUPPENNAME           PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-GRUPPEBENUTZERBEZ.
           20 EA-GBBMITAROLLENID             PIC X(13).
           20 EA-GBBMITAROLLENKZ             PIC X(01).
           20 EA-GBBGRUPPENNAME              PIC X(13).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
