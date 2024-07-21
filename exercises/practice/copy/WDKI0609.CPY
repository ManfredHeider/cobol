      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0609.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/WDKI0609.CPv  $
      *
      *   Rev 2.2   22 Oct 2001 14:48:56   t13ex27
      *AKI00240 - DB-Änderung Fixpack 6 
      *
      *   Rev 2.1   Apr 16 1999 10:13:42   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:04   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-BENUTZER-GESAMT.
         10 EA-NI-BENUTZER-TAB.
170901*    10 NI-BENUTZER-ELE PIC S9(04) COMP OCCURS 6 TIMES.
170901     20 EA-NI-BENUTZER-ELE PIC X
                  OCCURS 7 TIMES.
         10 EA-NI-BENUTZER
                 REDEFINES EA-NI-BENUTZER-TAB.
           20 EA-NI-BENMITAROLLENID          PIC X.
           20 EA-NI-BENMITAROLLENKZ          PIC X.
           20 EA-NI-RACFID                   PIC X.
           20 EA-NI-BENKENNWORT              PIC X.
           20 EA-NI-BENART                   PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
170901     20 EA-NI-BENVWKENNWORT            PIC X.
         10 EA-DCL-BENUTZER.
           20 EA-BENMITAROLLENID             PIC X(13).
           20 EA-BENMITAROLLENKZ             PIC X(01).
           20 EA-RACFID                      PIC X(8).
           20 EA-BENKENNWORT                 PIC X(15).
           20 EA-BENART                      PIC X(01).
           20 EA-LETZTEAENDERUNG             PIC X(26).
170901     20 EA-BENVWKENNWORT               PIC X(15).
      ******************************************************************
