      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.07.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2510.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2510.CPv  $
      *
      *   Rev 2.1   Jul 11 2011 16:17:52   t13ex18
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertrag
      *
      *   Rev 2.0   Jul 07 2011 09:19:36   t13ex18
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    DWS-Zulagen - zu löschende Verträge (Historisierung)        *
      *                                                                *
      *    SATZLÄNGE   100                                             *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *Auftrag ! Datum  ! Text                             ! Name      *
      *--------!-------------------------------------------!-----------*
      *AKI01040!07.07.11! Neuerstellung                    ! MDI       *
      *----------------------------------------------------------------*
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
       01  DRKI2510.
           05 DRKI2510-VERTRAGSID       PIC X(013).

           05 DRKI2510-KEY.
              10 DRKI2510-VNR.
                 15 DRKI2510-FILIALNR   PIC 9(003).
                 15 DRKI2510-KUNDENNR   PIC 9(007).
                 15 DRKI2510-ORDNUNGSNR PIC 9(002).
                 15 DRKI2510-VNR-REST   PIC X(003).

              10 DRKI2510-PORTFOLIONR   PIC X(010).
              10 DRKI2510-BEITRJAHR     PIC 9(004).

           05 DRKI2510-FILLER           PIC X(001).

           05 DRKI2510-GES              PIC S9(004) BINARY.

           05 DRKI2510-RESERVE          PIC X(055).
