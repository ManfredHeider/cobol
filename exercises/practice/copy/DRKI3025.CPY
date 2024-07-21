      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3025.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3025.CPv  $
      *
      *   Rev 2.1   Jul 09 2015 09:41:52   t13ex48
      *AKI01372: AML: Einführung Teil- und Zeitrenten, Kapitalentna
      *hmen: Ermittlung, Aufbereitung
      *
      *   Rev 2.0   Jun 23 2015 19:34:16   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 3 0 2 5    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUSGABE AUFBEREITUNG AML KAPITALENTNAHME     *
      *                                                                *
      *    SATZLÄNGE   80                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      ******************************************************************
      * AENDERUNGEN:                                                   *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME       *
      *---------!--------!----!---------------------------!------------*
      *AKI01372 !01.07.15!    !NEUERSTELLUNG              ! HSE        *
      *         !        !    !                           !            *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
       01  AML-KAPENTNAHME-AUFB.
           05  AMLKE-A-KEY.
               10  AMLKE-A-GES                     PIC 9(02).
               10  AMLKE-A-VNR                     PIC 9(15).
               10  AMLKE-A-KAPENTNNR               PIC 9(04).
      *
           05  AMLKE-A-DATEN.
               10  AMLKE-A-KAPENTNBETRAG           PIC 9(11)V99.
               10  AMLKE-A-KAPENTNTERMIN           PIC X(10).

           05  AMLKE-A-AKTUAL-KZ                   PIC X(01).
           05  AMLKE-A-RESERVE                     PIC X(35).



