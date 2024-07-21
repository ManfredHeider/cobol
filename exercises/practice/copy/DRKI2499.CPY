      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.06.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2499.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2499.CPv  $
      *
      *   Rev 2.4   Jan 24 2012 13:13:16   t13ex18
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertrag
      *
      *   Rev 2.3   Jan 24 2012 12:25:46   t13ex18
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertrag
      *
      *   Rev 2.2   Jan 24 2012 10:44:06   t13ex18
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertrag
      *
      *   Rev 2.1   Jul 11 2011 16:17:10   t13ex18
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertrag
      *
      *   Rev 2.0   Jun 09 2011 16:41:10   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    KSDS DWS-Zulagen                                            *
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
       01  DRKI2499.
               05  KSDS-SA                 PIC  X(002).
      *            SATZART
               05  KSDS-GES                PIC  9(002).
      *            GESELLSCHAFT
               05  KSDS-KEY.
                   10  KSDS-VNR.
                       15 FILIALNUMMER     PIC  9(003).
                       15 KUNDENNUMMER     PIC  9(007).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
                   10  KSDS-VNR-ORDNUNGSNR PIC  9(002).
      *            Ordnungskennzeichen für interne DVAG-Verarbeitung:
      *            laufende Nummer beginnend mit 00
                   10  KSDS-PORTFOLIONR    PIC  X(010).
      *            PORTFOLIO-NUMMER
                   10  KSDS-BEITRJAHR      PIC  9(004).
      *            BEITRAGSJAHR (JHJJ)
               05  KSDS-ZULAGEN-BEITRJAHR  PIC  9(011)V9(002).
      *            Zulagen für Beitragsjahr

               05  KSDS-RESERVE            PIC  X(057).
