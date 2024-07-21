      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1136.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1136.CPv  $
      *
      *   Rev 2.3   Nov 11 2011 14:37:22   t13ex14
      *AKI01079: Anpassung Aufbereitung u. Aktualisierung
      *
      *   Rev 2.2   16 Oct 2001 14:14:32   t13ex49
      *AKI00242 - DTA-Satzstruktur Prämie
      *
      *   Rev 2.1   02 Nov 2000 14:57:42   T13EX49
      *AKI00154 - erstmalige Prod-Übergabe
      *
      *   Rev 2.0   07 Jun 2000 17:08:16   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    Komplettbestand DMB als KSDS-Datei                         *
      *                                                               *
      *                                                               *
      *    COPY-MEMBER ZUM ERMITTLUNGSPROGRAMMEN                      *
      *                                                               *
      *                                                               *
      *    SATZLÄNGE 453                                              *
      *                                                               *
      *---------------------------------------------------------------*
      *    LOGBUCH                                                    *
      *                                                               *
      *AUFTRAG !Datum   !Text                              !Name      *
      *--------!-------------------------------------------!----------*
      *AKI00154!24.10.00! Neuerstellung                    !P. Pohl   *
280901*AKI00242!28.09.01! Prämie geändert                  !Peter Pohl*
      * ---------------------------------------------------------------*
181011*AKI01079!18.10.11! KB-VART Format erweitert         !W. Misgeld *
      *        !        ! für Migration                    !          *
      *        !        !                                  !          *
      *--------!-------------------------------------------!----------*
       01  DRKI1136.

           05 KB-VERTRAG-KEY.
               10 KB-VNR               PIC 9(015).

      *-------------------------------------------------
           05  KB-ORGANR2              PIC 9(8).
           05  KB-BAKKZ                PIC X.
           05  KB-VBEG1.
               10 KB-VBEG-JJ           PIC 99.
               10 KB-VBEG              PIC 9(6).
           05  KB-VABL1.
               10 KB-VABL-JJ           PIC 99.
               10 KB-VABL              PIC 9(6).
           05  KB-AEND1.
               10 KB-AEND-JJ           PIC XX.
               10 KB-AEND              PIC X(6).
           05  KB-ZAHLW                PIC 9.
           05  KB-FAEL                 PIC 99.
           05  KB-LAST                 PIC X.
           05  KB-VSU.
               10 KB-VSU-T             PIC 9(6).
               10 KB-VSU-H             PIC 9(3).
           05  KB-ANREDE               PIC X.
           05  KB-TITEL                PIC X.
           05  KB-NAME                 PIC X(30).
           05  KB-AUSL                 PIC X(3).
           05  KB-PLZ.
               10 KB-PLZ1              PIC X.
               10 KB-PLZ2              PIC X(4).
           05  KB-ORT                  PIC X(25).
           05  KB-STR                  PIC X(30).
           05  KB-GEBD1.
               10 KB-GEBD-JJ           PIC XX.
               10 KB-GEBD              PIC X(6).
280901*    05  KB-PRAEMIE              PIC 9(8)V9.
280901*    05  FILLER                  PIC X(26).
280901     05  KB-PRAEMIE              PIC 9(8)V99.
280901     05  FILLER                  PIC X(25).
           05  KB-RISZAEHL             PIC 9(3).
           05  KB-VARIABEL OCCURS 05.
               10 KB-WAG               PIC X(4).
               10 KB-VTEXT             PIC X(25).
               10 KB-VART              PIC 9(5).
181011         10 KB-VART-RED REDEFINES KB-VART.
181011            15 KB-VART-ST1       PIC X.
181011            15 KB-VART-ST2-5     PIC 9(04).
               10 FILLER               PIC X(16).
           05 KB-STATUS                PIC X(1).
      *       D --> Gelöscht - sonst SPACE

