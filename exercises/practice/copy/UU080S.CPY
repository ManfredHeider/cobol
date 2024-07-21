      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU080S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU080S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:53:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   1    20.07.93 10:07:09
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER BESTANDSAETZE   INVEST                     *
      *    ---------------------------------------                     *
      *                                                                *
      *    LAENGE =  VB  176         ERSTELLT AM 19.11.90.             *
      *    MIN    =  176 BYTE                    KUHN                  *
      *    MAX    =  176 BYTE                                          *
      *                                                                *
      *    GEÄNDERT 25.05.93 / BORK                                    *
      *    ÄNDERUNG WEGEN DER UMSTELLUNG AUF DIE NEUE, 5-STELLIGE PLZ. *
      *    DIE PLZ KOMMT IN DER ALTEN VERSION IN DEN ERSTEN 4 BZW. 8   *
      *    BYTE DES ORTES, AB DER NEUEN IMMER IN DEN ERSTEN 8 BYTE.    *
      *    AB DER NEUEN VERSION, EXTRA PLZ-FELD.                       *
      *----------------------------------------------------------------*
CCC009*    SKIP1
       01  UU080S.
         05  INV-SATZ.
           10  INV-SA                  PIC X.
         05  INV-CSATZ.
           10  INV-SANR                PIC X.
           10  INV-ORGA.
               15  INV-BFGNL           PIC 9(4)       COMP-3.
               15  INV-BFGKTO          PIC 9(10)      COMP-3.
               15  INV-BFGGES          PIC 9.
            12 INV-ORGA-I.
               15  INV-GES             PIC 99.
               15  INV-ORGGS           PIC 9(3)       COMP-3.
               15  INV-VERM            PIC 9(10)      COMP-3.
               15  INV-PIF             PIC X(20).
               15  INV-ANREDE          PIC XX.
               15  INV-NAME            PIC X(30).
               15  INV-STRASSE         PIC X(30).
250593         15  INV-PLZORT.
250593             20  INV-PLZ         PIC X(08).
250593             20  INV-ORT         PIC X(22).
               15  INV-GEB-DAT         PIC 9(12)      COMP-3.
               15  INV-BERUF           PIC XX.
               15  INV-LAND            PIC X(3).
               15  FILLER-C1           PIC X(30).

            12 INV-PRODUKT REDEFINES INV-ORGA-I.
               15  INV-FONDS           PIC 9(3)       COMP-3.
               15  INV-PRODSCHL        PIC 99.
               15  INV-V-BEG           PIC 9(8)       COMP-3.
               15  INV-V-ENDE          PIC 9(8)       COMP-3.
               15  INV-KUSCHU-ABL      PIC 9(8)       COMP-3.
               15  INV-ISTSUMME        PIC S9(9)V99   COMP-3.
               15  INV-URSPRUNGSSUM    PIC 9(9)V9(3)  COMP-3.
               15  INV-RATE            PIC 9(9)V99    COMP-3.
               15  INV-MODUS           PIC X.
               15  INV-HAUSTARIF       PIC X.
               15  INV-PROVKZ          PIC X.
               15  INV-GES-2           PIC 99.
               15  INV-ORGGS-2         PIC 9(3)       COMP-3.
               15  INV-VERM-2          PIC 9(10)      COMP-3.
               15  INV-PIF-2           PIC X(20).
               15  FILLER-C2           PIC X(93).

         05  INV-ASATZ REDEFINES INV-CSATZ.
           10  INV-ABSENDER            PIC X(27).
           10  INV-ERST-DAT            PIC 9(6).
           10  INV-DATEISTAND          PIC 9(6).
           10  INV-EMPFAENGER          PIC X(27).
           10  INV-INHALT              PIC X(20).
           10  FILLER-A1               PIC X(89).

         05  INV-ESATZ REDEFINES INV-CSATZ.
           10  INV-SATZANZAHL          PIC 9(7)       COMP-3.
           10  FILLER-E1               PIC X(171).
