      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0865.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRKI0865.CPv  $
      *
      *   Rev 2.2   Apr 06 2000 11:18:54   T13EX39
      *AKI00099 LOE-KZ
      *
      *   Rev 2.1   Aug 06 1999 11:56:10   T13EX47
      *Produktionsübergabe AKI00023-6
      *
      *   Rev 2.0   Apr 29 1999 13:57:02   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *****  VERSION   1    20.07.93 10:07:09
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER BESTANDSAETZE   INVEST                     *
      *    ---------------------------------------                     *
      *                                                                *
      *    LAENGE =   176        ERSTELLT AM 31.05.99.                 *
      *                                                                *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *20.03.00! INV-LOE-KZ aufgenommen                !WHO      ! 01  *
      *AKI00099!                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
       01  DRKI0865.
         05  INV-SATZ.
           10  INV-SA                  PIC X.
         05  INV-CSATZ.
           10  INV-SANR                PIC X.
           10  INV-ORGA.
               15  INV-BFGNL           PIC 9(4)        comp-3.
               15  INV-BFGKTO          PIC 9(10)       comp-3.
               15  INV-BFGGES          PIC 9.
            12 INV-ORGA-I.
               15  INV-GES             PIC 99.
               15  INV-ORGGS           PIC 9(3)        comp-3.
               15  INV-VERM            PIC 9(10)       comp-3.
               15  INV-PIF             PIC X(20).
               15  INV-ANREDE          PIC XX.
               15  INV-NAME            PIC X(30).
               15  INV-STRASSE         PIC X(30).
               15  INV-PLZORT.
                   20  INV-PLZ         PIC X(08).
                   20  INV-ORT         PIC X(22).
               15  INV-GEB-DAT         PIC 9(12)       comp-3.
               15  INV-BERUF           PIC XX.
               15  INV-LAND            PIC X(3).
200300         15  INV-LOE-KZ          PIC X(03).
               15  FILLER-C1           PIC X(27).

            12 INV-PRODUKT REDEFINES INV-ORGA-I.
               15  INV-FONDS           PIC 9(3)        comp-3.
               15  INV-PRODSCHL        PIC 99.
               15  INV-V-BEG           PIC 9(8)        comp-3.
               15  INV-V-ENDE          PIC 9(8)        comp-3.
               15  INV-KUSCHU-ABL      PIC 9(8)        comp-3.
               15  INV-WAEHRUNG        PIC X(3).
               15  INV-ISTSUMME        PIC S9(9)V99    comp-3.
               15  INV-URSPRUNGSSUM    PIC 9(9)V9(3)   comp-3.
               15  INV-RATE            PIC 9(9)V99     comp-3.
               15  INV-MODUS           PIC X.
               15  INV-HAUSTARIF       PIC X.
               15  INV-PROVKZ          PIC X.
               15  INV-GES-2           PIC 99.
               15  INV-ORGGS-2         PIC 9(3)        comp-3.
               15  INV-VERM-2          PIC 9(10)       comp-3.
               15  INV-PIF-2           PIC X(20).
               15  FILLER-C2           PIC X(90).

         05  INV-ASATZ REDEFINES INV-CSATZ.
           10  INV-ABSENDER            PIC X(27).
           10  INV-ERST-DAT            PIC 9(8).
           10  INV-DATEISTAND          PIC 9(8).
           10  INV-EMPFAENGER          PIC X(27).
           10  INV-INHALT              PIC X(20).
           10  INV-WAEHRUNG            PIC X(03).
           10  FILLER-A1               PIC X(82).

         05  INV-ESATZ REDEFINES INV-CSATZ.
           10  INV-SATZANZAHL          PIC 9(7)        comp-3.
           10  FILLER-E1               PIC X(171).

