      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.08.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKW0018.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKW0018.CPv  $
      *
      *   Rev 2.3   Oct 11 2019 10:15:14   t13ex27
      *AKI01683: Capital Bank - Neuer WAG-Bogen (WpHG) nicht immer 
      *erforderlich, sofern bereits ein aktueller vorliegt - Erweit
      *ands-DTA Depot
      *
      *   Rev 2.2   May 06 2019 13:52:18   t13ex27
      *AKI01683: Capital Bank - Neuer WAG-Bogen (WpHG) nicht immer
      *erforderlich, sofern bereits ein aktueller vorliegt - Erweit
      *ands-DTA Depot
      *
      *   Rev 2.1   Sep 28 2010 13:34:28   t13ex15
      *AKI00966: DVAG Österreich: Kreditsparkonto Aktualisierung
      *
      *   Rev 2.0   Aug 18 2010 13:02:08   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K W 0 0 1 8    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMME DER DVAG Wien,        *
      *    AUFBEREITUNG GUTHABENPRODUKT                                *
      *        - AUSGABE: ALLGEMEIN GUTHABENPRODUKT FUER DVAG Wien     *
      *                                                                *
      *    SATZLÄNGE   150                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *31.08.10!NEUERSTELLUNG                          !SWZ      ! 01  *
      *AKI00966!                                       !         !     *
      *        !                                       !         !     *
      * --------+--------+----+---------------------------+------------*
      * AKI01683!24.04.19!2.2 ! Neue Attribute            ! IFR        *
      *         !        !    ! Risikoklasse und Wieder-  !            *
      *         !        !    ! anlagedatum               !            *
      *         !        !    !                           !            *
      ******************************************************************
       01  DRKW0018.

           05  GHP-GES                     PIC  9(02).
           05  GHP-VNR                     PIC  9(15).
           05  GHP-LFDNR                   PIC  9(04).
      *        NUR Unterkontonummer

           05  GHP-PRODUKT-ID              PIC  S9(04).

           05  GHP-DATUM-EROEFFNUNG        PIC  X(10).
           05  GHP-AEND-DATUM-E-KZ         PIC  X(01).
      *        J WENN ERÖFFNUNGSDATUM GEAENDERT

           05  GHP-DATUM-SCHLIESSUNG       PIC  X(10).
           05  GHP-AEND-DATUM-S-KZ         PIC  X(01).
      *        J WENN SCHLIESSUNGSDATUM GEAENDERT

           05  GHP-ZINSSATZ                PIC  9(003)V99.
           05  GHP-AEND-ZINSSATZ-KZ        PIC  X(001).

           05  GHP-PRODDETAILNAME          PIC  X(050).
           05  GHP-AEND-PRODDETAILNAME-KZ  PIC  X(001).

           05  GHP-RISIKOKLASSE            PIC  9(001).
           05  GHP-AEND-RISIKOKLASSE-KZ    PIC  X(001).

           05  GHP-WPHGDAT                 PIC  X(010).
           05  GHP-AEND-WPHGDAT-KZ         PIC  X(001).

           05  GHP-AKTUAL-KZ               PIC  X(01).

      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
      *
           05  FILLER                      PIC  X(32).
      *        Reserve
      ******************************************************************


