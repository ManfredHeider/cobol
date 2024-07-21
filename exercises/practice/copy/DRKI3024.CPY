      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3024.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3024.CPv  $
      *
      *   Rev 2.1   Jul 09 2015 09:41:42   t13ex48
      *AKI01372: AML: Einführung Teil- und Zeitrenten, Kapitalentna
      *hmen: Ermittlung, Aufbereitung
      *
      *   Rev 2.0   Jun 23 2015 19:34:10   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 3 0 2 4    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUSGABE ERMITTLUNG AML KAPITALENTNAHME       *
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
      *
       01  AML-KAPENTNAHME-ERM.
           05  AMLKE-E-KEY.
               10  AMLKE-E-GES                     PIC 9(02).
               10  AMLKE-E-VNR                     PIC 9(15).
               10  AMLKE-E-KAPENTNNR               PIC 9(04).
      *
           05  AMLKE-DATEN.
               10  AMLKE-E-KAPENTNBETRAG           PIC S9(11)V99.
               10  AMLKE-E-KAPENTNTERMIN           PIC X(10).
      *
           05  AMLKE-E-AKTUAL-KZ                   PIC X(01).
           05  AMLKE-E-RESERVE                     PIC X(35).

