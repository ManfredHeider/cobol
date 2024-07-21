      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0299.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0299.CPv  $
      *
      *   Rev 2.1   Jun 20 2017 12:24:18   t13ae23
      *AAN01291: Elektron. Antrag DVAG Wien Verarbeitung d. Header-
      *Datensätze
      *
      *   Rev 2.0   Jun 25 2015 08:34:50   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *****************************************************************
      *    COPY-MEMBER: DRAN0299                                      *
      *    --------------------------------------------------------   *
      *    SST-BESCHREIBUNG FÜR MASCH. NACHERFASSEN E-SIGN.           *
      *    ANTRÄGE GENERALI, GESELLSCHAFT 16                          *
      *    --------------------------------------------------------   *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AAN01291!20.06.17!2.1 ! NEU E-SIGN ANTR.ÖSTERREICH! RAHNER    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************
      *
       01  EA-DATEN-NACHERF-GENS.

           03  GENS-NACHERF-DATEN.

             05  GENS-NACHERF-HVANTNUM                   PIC X(09).
             05  GENS-NACHERF-PB-GES                     PIC 9(02).
             05  GENS-NACHERF-VB-GES                     PIC 9(02).
             05  GENS-NACHERF-PIN                        PIC 9(11).
             05  GENS-NACHERF-AVERM                      PIC 9(07).
             05  GENS-NACHERF-AVERM-PZ                   PIC X(01).
             05  GENS-NACHERF-VMNR                       PIC 9(07).
             05  GENS-NACHERF-VMNR-PZ                    PIC X(01).
             05  GENS-NACHERF-VM-ZUNA                    PIC X(26).
             05  GENS-NACHERF-VM-VORNA                   PIC X(26).
             05  GENS-NACHERF-VB-GW                      PIC 9(07).
             05  GENS-NACHERF-VB-GW-PZ                   PIC X(01).
             05  GENS-NACHERF-DIR                        PIC 9(03).
             05  GENS-NACHERF-VN-ZUNA                    PIC X(26).
             05  GENS-NACHERF-VN-VORNA                   PIC X(26).
             05  GENS-NACHERF-VN-AK-TITEL                PIC X(26).
             05  GENS-NACHERF-VN-SVNR                    PIC X(10).
             05  GENS-NACHERF-VN-PLZ                     PIC X(05).
             05  GENS-NACHERF-VN-ORT                     PIC X(26).
             05  GENS-NACHERF-VN-STRASSE                 PIC X(26).
             05  GENS-NACHERF-VN-AK                      PIC X(03).
             05  GENS-NACHERF-KD-BEST-ID                 PIC 9(18).
             05  GENS-NACHERF-VN-PRODKEY-10              PIC 9(04).
             05  GENS-NACHERF-VN-BTP-P10                 PIC X(09).
             05  GENS-NACHERF-VN-JNPT-P10                PIC X(09).
             05  GENS-NACHERF-VN-EH-P10                  PIC X(09).
             05  GENS-NACHERF-VN-PRODKEY-20              PIC 9(04).
             05  GENS-NACHERF-VN-BTP-P20                 PIC X(09).
             05  GENS-NACHERF-VN-JNPT-P20                PIC X(09).
             05  GENS-NACHERF-VN-EH-P20                  PIC X(09).
             05  GENS-NACHERF-VN-PRODKEY-30              PIC 9(04).
             05  GENS-NACHERF-VN-BTP-P30                 PIC X(09).
             05  GENS-NACHERF-VN-JNPT-P30                PIC X(09).
             05  GENS-NACHERF-VN-EH-P30                  PIC X(09).
             05  GENS-NACHERF-VN-PRODKEY-40              PIC 9(04).
             05  GENS-NACHERF-VN-BTP-P40                 PIC X(09).
             05  GENS-NACHERF-VN-JNPT-P40                PIC X(09).
             05  GENS-NACHERF-VN-EH-P40                  PIC X(09).
             05  GENS-NACHERF-VN-PRODKEY-50              PIC 9(04).
             05  GENS-NACHERF-VN-BTP-P50                 PIC X(09).
             05  GENS-NACHERF-VN-JNPT-P50                PIC X(09).
             05  GENS-NACHERF-VN-EH-P50                  PIC X(09).
             05  GENS-NACHERF-VN-PRODKEY-60              PIC 9(04).
             05  GENS-NACHERF-VN-BTP-P60                 PIC X(09).
             05  GENS-NACHERF-VN-JNPT-P60                PIC X(09).
             05  GENS-NACHERF-VN-EH-P60                  PIC X(09).
             05  GENS-NACHERF-VN-PRODKEY-70              PIC 9(04).
             05  GENS-NACHERF-VN-BTP-P70                 PIC X(09).
             05  GENS-NACHERF-VN-JNPT-P70                PIC X(09).
             05  GENS-NACHERF-VN-EH-P70                  PIC X(09).
             05  GENS-NACHERF-VN-PRODKEY-80              PIC 9(04).
             05  GENS-NACHERF-VN-BTP-P80                 PIC X(09).
             05  GENS-NACHERF-VN-JNPT-P80                PIC X(09).
             05  GENS-NACHERF-VN-EH-P80                  PIC X(09).
             05  GENS-NACHERF-BEGINN                     PIC X(10).
             05  GENS-NACHERF-ENDE                       PIC X(10).
             05  GENS-NACHERF-ZAHLWEISE                  PIC 9(01).
             05  GENS-NACHERF-WAEHRUNG                   PIC X(03).
             05  GENS-NACHERF-VN-KTONR                   PIC 9(15).
             05  GENS-NACHERF-VN-BLZ                     PIC 9(15).
             05  GENS-NACHERF-VN-KFZ-KZ                  PIC X(10).
             05  GENS-NACHERF-VN-KFZ-FGST                PIC X(26).
             05  GENS-NACHERF-VN-KFZ-B-NR                PIC X(20).
             05  GENS-NACHERF-AN-ART                     PIC X(01).
             05  GENS-NACHERF-AN-DAT                     PIC X(10).
             05  GENS-NACHERF-VERSION                    PIC 9(04).
             05  GENS-NACHERF-VN-STAATSANG               PIC X(03).
             05  GENS-NACHERF-VN-GEBDAT                  PIC X(10).
             05  GENS-NACHERF-GEN-ANT-NR                 PIC 9(15).
             05  GENS-NACHERF-GEN-B-ANT-NR               PIC 9(15).
             05  GENS-NACHERF-VN-IBAN                    PIC X(34).
             05  GENS-NACHERF-FILLER                     PIC X(101).
      ****
      **** ENDE COPY DRAN0299 *****************************************
      ****
      *---------------------------------------------------------------*


