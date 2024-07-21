      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0608.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0608.CPv  $
      *
      *   Rev 2.2   Jun 15 2016 14:27:52   t13ae35
      *AVB01400: Projekt Lucy2 - Konstanten-Copy DRAT0044
      *
      *   Rev 2.1   May 04 2016 17:22:54   t13ae35
      *AVB01400: Projekt Lucy2 - Konstanten-Copy DRAT0044
      *
      *   Rev 2.0   May 04 2016 17:13:20   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRAT0608  KONSTANTEN FÜR KTY 401             *
      *                           Verabeitungs-KZ S-TAB              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: J.Hering                                        *
      *    DATUM:    04.05.2016                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01400!04.05.16!2.1 ! NEU                       ! J.Hering *
150616* AVB01400!15.06.16!2.2 ! 5 neue KNR                ! J.Hering *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
      *----aktive ursprüngliche Buchung in STAB         ------*
           05 K-KNR-STAB-VERARB-AKT-UB    PIC S9(004) BINARY VALUE  1.
      *----aktive abgeleitete Buchung in STAB  = Korrektur ---*
           05 K-KNR-STAB-VERARB-AKT-AB    PIC S9(004) BINARY VALUE  2.
      *----inaktive Buchung in STAB Vorlage für Storno  ------*
           05 K-KNR-STAB-VERARB-IAKT-VS   PIC S9(004) BINARY VALUE  3.
      *----inaktive Buchung in STAB Nullsetzungsbuchung = Storno ---*
           05 K-KNR-STAB-VERARB-IAKT-NS   PIC S9(004) BINARY VALUE  4.
150616*----analog KNR=1 für Zeitraum zw. FiBu-SST und Akt. AbrechnNr -*
           05 K-KNR-STAB-VERARB-AKT-UB11  PIC S9(004) BINARY VALUE 11.
150616*----analog KNR=2 für Zeitraum zw. FiBu-SST und Akt. AbrechnNr -*
           05 K-KNR-STAB-VERARB-AKT-AB12  PIC S9(004) BINARY VALUE 12.
150616*----analog KNR=2 für Zeitraum zw. FiBu-SST und Akt. AbrechnNr -*
           05 K-KNR-STAB-VERARB-IAKT-N14  PIC S9(004) BINARY VALUE 14.
150616*----aktive ursprüngliche Buchung in STAB als zu korrigieren --*
      *----identifiziert weil VB seinen Status gewechselt hat      --*
           05 K-KNR-STAB-VERARB-AKT-UB91  PIC S9(004) BINARY VALUE 91.
150616*----aktive abgeleitete Buchung in STAB  = Korrektur, als zu --*
      *----korrig. identifiziert weil VB seinen Status gewechselt hat*
           05 K-KNR-STAB-VERARB-AKT-AB    PIC S9(004) BINARY VALUE 92.
      *
 
 

