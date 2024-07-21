      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.07.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3028.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3028.CPv  $
      *
      *   Rev 2.1   Jul 20 2015 11:32:34   t13ex48
      *AKI01372: AML: Einführung Teil- und Zeitrenten, Kapitalentna
      *hmen: Ermittlung, Aufbereitung
      *
      *   Rev 2.0   Jul 13 2015 17:52:22   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *****************************************************************
      *    COPY-MEMBER: DRKI3028                                      *
      *    Satzbeschreibung KSDS-Datei AML Kapitalentnahmen (LKE)     *
      *    Satzlaenge: 80 Bytes                                       *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    LOGBUCH                                                    *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME     *
      * --------!--------!----!----------------------------!----------*
      * AKI01372!01.07.15! 01 ! Neuerstellung              ! HSE      *
      * --------!--------!----!----------------------------!----------*
      *         !        !    !                            !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************

      *
       01  LKE-KOMPLETT-SATZ.
           05  LKE-KOMPL-SA                      PIC X(01).
      * SATZART        = 'C'
           05  LKE-KOMPL-KEY.
             10 LKE-KOMPL-VNR                  PIC 9(10).
      * VERTRAGSNUMMER
             10 LKE-KOMPL-KAPENTNNR            PIC 9(04).
      * Laufende Nummer der Kapitalentnahme
           05  LKE-KOMPL-KAPENTNBETRAG         PIC 9(11)V99.
      * BRUTTOENTNAHMEBETRAG
           05  LKE-KOMPL-KAPENTNTERMIN         PIC X(10).
      * TERMIN DER KAPITALENTNAHME, NICHT DER AUSZAHLUNG
           05  FILLER                          PIC X(42).
      *--------------- ENDE COPYBOOK DRKI3028 -------------------------

