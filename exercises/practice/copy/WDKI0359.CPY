      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0359.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/WDKI0359.CPv  $
      *
      *   Rev 2.2   Oct 15 2002 08:15:54   t13ex27
      *AKI00308 DB-Änderung Fixpack 26
      *
      *   Rev 2.1   Apr 16 1999 10:11:02   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:46   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-VERTRAGROLLEBEZ-GESAMT.
         10 EA-NI-VERTRAGROLLEBEZ-TAB.
           20 EA-NI-VERTRAGROLLEBEZ-ELE PIC X
300902*           OCCURS 9 TIMES.
300902            OCCURS 10 TIMES.
         10 EA-NI-VERTRAGROLLEBEZ
                 REDEFINES EA-NI-VERTRAGROLLEBEZ-TAB.
           20 EA-NI-VRBVERTRAGSID            PIC X.
           20 EA-NI-VRBPERSONENID            PIC X.
           20 EA-NI-VRBPERSONENROLLEKZ       PIC X.
           20 EA-NI-VRBPABPERSONENID         PIC X.
           20 EA-NI-VRBPABADRESSEID          PIC X.
           20 EA-NI-VRBABLAUFDATUM           PIC X.
           20 EA-NI-VRBGUELTIGKEITSDAT       PIC X.
           20 EA-NI-VRBAVKENNZEICHEN         PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
300902     20 EA-NI-VRBKOMMBETRKZ            PIC X.
         10 EA-DCL-VERTRAGROLLEBEZ.
           20 EA-VRBVERTRAGSID               PIC X(13).
           20 EA-VRBPERSONENID               PIC X(13).
           20 EA-VRBPERSONENROLLEKZ          PIC X(01).
           20 EA-VRBPABPERSONENID            PIC X(13).
           20 EA-VRBPABADRESSEID             PIC X(13).
           20 EA-VRBABLAUFDATUM              PIC X(10).
           20 EA-VRBGUELTIGKEITSDAT          PIC X(10).
           20 EA-VRBAVKENNZEICHEN            PIC X(01).
           20 EA-LETZTEAENDERUNG             PIC X(26).
300902     20 EA-VRBKOMMBETRKZ               PIC X(1).
      ******************************************************************
