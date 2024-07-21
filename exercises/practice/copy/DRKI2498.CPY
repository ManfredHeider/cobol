      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.06.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2498.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2498.CPv  $
      *
      *   Rev 2.1   Jul 11 2011 16:17:06   t13ex18
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertrag
      *
      *   Rev 2.0   Jun 09 2011 14:52:50   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  08.06.2011
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      ******************************************************************
      *                                                                *
      *    Veränderungsbestand DWS-Zulagen                             *
      *                                                                *
      *    SATZLÄNGE   100                                             *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *Auftrag ! Datum  ! Text                             ! Name      *
      *--------!-------------------------------------------!-----------*
      *AKI01040!09.06.11! Neuerstellung                    ! MDI       *
      *----------------------------------------------------------------*
      * ---------------------------------------------------------------*
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      *
       01  DRKI2498.
               05  BNK-SA                  PIC  X(002).
      *            SATZART
               05  BNK-GES                 PIC  9(002).
      *            GESELLSCHAFT
               05  BNK-VNR.
                   10 FILIALNUMMER         PIC  9(003).
                   10 KUNDENNUMMER         PIC  9(007).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
               05  BNK-VNR-ORDNUNGSNR      PIC  9(002).
      *            Ordnungskennzeichen für interne DVAG-Verarbeitung:
      *            laufende Nummer beginnend mit 00.

               05  BNK-VERARB-KZ           PIC  X(001).
      *            VERARBEITUNGSKENNZEICHEN
      *                I = INSERT
      *                U = UPDATE
      *                D = DELETE

               05  BNK-PORTFOLIONR         PIC  X(010).
      *            PORTFOLIO-NUMMER
               05  BNK-BEITRJAHR           PIC  9(004).
      *                BEITRAGSJAHR (JHJJ)
               05  BNK-ZULAGEN-BEITRJAHR   PIC  9(011)V9(002).
      *                Zulagen für Beitragsjahr
               05  BNK-AEND-ZUL-BEITRJAHR-KZ   PIC  X(001).
      *                J = ÄNDERUNG ZULAGEN FÜR BEITRAGSJAHR

               05  BNK-ZUL-BEITRJAHR-ALT   PIC  9(011)V9(002).
      *                VORHERIGE ZULAGEN FÜR BEITRAGSJAHR

               05  BNK-DEL-KZ              PIC  X(001).

               05  BNK-RESERVE             PIC  X(041).
