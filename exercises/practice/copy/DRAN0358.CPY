      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  08.10.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0358.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0358.CPv  $
      *
      *   Rev 2.1   Jun 22 2020 10:21:24   u130377
      *AAN01436: BKK-Linde: Anbindung E-Antragsverfahren - Verarbei
      *tung Vorschlagsdaten zu eAnträgen auf dem Host
      *
      *   Rev 2.0   Oct 08 2019 08:51:54   u130377
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0358                                      *
      *    --------------------------------------------------------   *
      *    ELEKTRONISCHE ANTRÄGE FÜR PRODUKTGEBER BKK-LINDE (51)      *
      *                                                               *
      *                                                               *
      *    AUFBAU DER BLACK-BOX-DATEN IN ELEKTR_ANTRAEGE (TCOK0016)   *
      *    --------------------------------------------------------   *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AAN01436!08.10.19!2.1 ! NEU                       ! Töpfer    *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *
       01  EA-DATEN-BKK-LINDE.

           03  BKK-LINDE-DATEN.
               05  BKK-LINDE-VERSION              PIC X(04).
               05  BKK-LINDE-VORSCHLAG            PIC X(09).
               05  BKK-LINDE-VM                   PIC 9(07).
               05  BKK-LINDE-KUNDE-NAME           PIC X(26).
               05  BKK-LINDE-KUNDE-VNAME          PIC X(26).
               05  BKK-LINDE-PLZ                  PIC X(05).
               05  BKK-LINDE-ORT                  PIC X(21).
               05  BKK-LINDE-TARIF                PIC X(01).
               05  BKK-LINDE-ANTRAG-DATUM         PIC X(10).
               05  FILLER                         PIC X(41).
      ****
      **** ENDE COPY DRAN0358
      *****************************************************************





