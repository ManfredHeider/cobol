      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0301.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRAN0301.CPv  $
      *
      *   Rev 2.5   Jun 27 2017 13:30:22   t13ae23
      *AAN01291: Elektron. Antrag DVAG Wien Verarbeitung d. Header-
      *Datensätze
      *
      *   Rev 2.4   Jun 27 2017 13:01:40   t13ae23
      *AAN01291: Elektron. Antrag DVAG Wien Verarbeitung d. Header-
      *Datensätze
      *
      *   Rev 2.3   Jun 27 2017 12:52:24   t13ae23
      *AAN01291: Elektron. Antrag DVAG Wien Verarbeitung d. Header-
      *Datensätze
      *
      *   Rev 2.2   Jun 27 2017 10:54:22   t13ae23
      *AAN01291: Elektron. Antrag DVAG Wien Verarbeitung d. Header-
      *Datensätze
      *
      *   Rev 2.1   Jun 20 2017 12:24:30   t13ae23
      *AAN01291: Elektron. Antrag DVAG Wien Verarbeitung d. Header-
      *Datensätze
      *
      *   Rev 2.0   Jun 25 2015 08:35:08   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0301                                      *
      *    --------------------------------------------------------   *
      *    SST-BESCHREIBUNG NACHERFASSUNG E-SIGN.                     *
      *    ANTRÄGE GENERALI, GESELLSCHAFT 26                          *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AAN01291!20.06.17!2.1 ! E-SIGN ANTRAG ÖSTERREICH  ! RAHNER    *
      * AAN01291!27.06.17!2.2 ! TARIF SELBST ERMITTELN    ! RAHNER    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************
      *
       01  EA-DATEN-NACHERF-GENL.

           03  GENL-NACHERF-DATEN.

             05  GENL-NACHERF-HVANTNUM                   PIC X(09).
             05  GENL-NACHERF-PB-GES                     PIC 9(02).
             05  GENL-NACHERF-VB-GES                     PIC 9(02).
             05  GENL-NACHERF-PIN                        PIC 9(11).
             05  GENL-NACHERF-AVERM                      PIC 9(07).
             05  GENL-NACHERF-AVERM-PZ                   PIC X(01).
             05  GENL-NACHERF-VMNR                       PIC 9(07).
             05  GENL-NACHERF-VMNR-PZ                    PIC X(01).
             05  GENL-NACHERF-VM-ZUNA                    PIC X(26).
             05  GENL-NACHERF-VM-VORNA                   PIC X(26).
             05  GENL-NACHERF-VB-GW                      PIC 9(07).
             05  GENL-NACHERF-VB-GW-PZ                   PIC X(01).
             05  GENL-NACHERF-DIR                        PIC 9(03).
             05  GENL-NACHERF-VN-ZUNA                    PIC X(26).
             05  GENL-NACHERF-VN-VORNA                   PIC X(26).
             05  GENL-NACHERF-VN-AK-TITEL                PIC X(26).
             05  GENL-NACHERF-VN-SVNR                    PIC 9(10).
             05  GENL-NACHERF-VN-PLZ                     PIC X(05).
             05  GENL-NACHERF-VN-ORT                     PIC X(26).
             05  GENL-NACHERF-VN-STRASSE                 PIC X(26).
             05  GENL-NACHERF-AK                         PIC X(03).
             05  GENL-NACHERF-KD-BEST-ID                 PIC 9(18).
             05  GENL-NACHERF-PROD                       PIC 9(04).
             05  GENL-NACHERF-VN-GEBDAT                  PIC X(10).
             05  GENL-NACHERF-SUMME-RENTE                PIC 9(09).
             05  GENL-NACHERF-BEGINN                     PIC X(10).
             05  GENL-NACHERF-PZ-DAUER                   PIC 9(02).
             05  GENL-NACHERF-VD-KZ-ZUW                  PIC X(01).
             05  GENL-NACHERF-VD-BUZB                    PIC X(01).
             05  GENL-NACHERF-VD-BUZR                    PIC X(01).
             05  GENL-NACHERF-VD-PO                      PIC X(01).
             05  GENL-NACHERF-VD-KZ-UTZ                  PIC X(01).
             05  GENL-NACHERF-BEITRAG                    PIC X(12).
             05  GENL-NACHERF-ZAHLWEISE                  PIC 9(01).
             05  GENL-NACHERF-WAEHRUNG                   PIC X(03).
             05  GENL-NACHERF-JAHRE-NTP                  PIC X(12).
             05  GENL-NACHERF-KONTONR                    PIC 9(15).
             05  GENL-NACHERF-BANKLEITZAHL               PIC 9(15).
             05  GENL-NACHERF-PRAEMIENSUMME              PIC X(12).
             05  GENL-NACHERF-EINH                       PIC X(09).
             05  GENL-NACHERF-AN-ART                     PIC X(01).
             05  GENL-NACHERF-AN-DAT                     PIC X(10).
             05  GENL-NACHERF-VERSION                    PIC 9(04).
             05  GENL-NACHERF-VN-STAATSANG               PIC X(03).
             05  GENL-NACHERF-VN-BRANCHE                 PIC X(40).
             05  GENL-NACHERF-VN-BERUFSGRUPPE            PIC X(40).
             05  GENL-NACHERF-VN-PEP-STATUS              PIC X(01).
             05  GENL-NACHERF-GEN-ANT-NR                 PIC 9(15).
             05  GENL-NACHERF-GEN-B-ANT-NR               PIC 9(15).
             05  GENL-NACHERF-VN-IBAN                    PIC X(34).
             05  GENL-NACHERF-FILLER                     PIC X(149).
      ****
      **** ENDE COPY DRAN0301 *****************************************
      ****






