      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.06.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2325.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2325.CPv  $
      *
      *   Rev 2.2   Aug 30 2011 12:51:00   t13ex15
      *AKI01072: Techn. Umbenennung SEB-Bank in Santander
      *
      *   Rev 2.1   Jun 18 2009 10:38:28   t13ex51
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.0   Jun 15 2009 16:27:12   t13ex51
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 3 2 5    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMME DER SEB-BANK,         *
      *    AUFBEREITUNG GUTHABENPRODUKT                                *
      *        - AUSGABE: ALLGEMEIN GUTHABENPRODUKT FUER SEB-BANK      *
      *                                                                *
      *    SATZLÄNGE   100                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *15.06.09!NEUERSTELLUNG                          !HRO      ! 01  *
      *KI-NEU  !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
       01  DRKI2325.

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

           05  GHP-VL-KZ                   PIC  X(01).
      *        KZ, OB PRODUKT VERMOEGENSWIRKSAMELEISTUNG

           05  GHP-WKZ                     PIC  X(01).
           05  GHP-AEND-WKZ-KZ             PIC  X(01).
      *        J WENN WÄHRUNGSKENNZEICHEN GEAENDERT

           05  GHP-RISIKOKLASSE            PIC  X(01).
      *        Risikoklasse 0 - 9
           05  GHP-AEND-RISIKO-KZ          PIC  X(01).
      *        J WENN Risikoklasse GEAENDERT

           05  GHP-AEND-PRODUKT-ID-KZ      PIC  X(01).
      *        J wenn Produkt-ID GEAENDERT

           05  GHP-ANLAGE-ZIEL             PIC  X(01).
      *        ANLAGEZIEL
           05  GHP-AEND-ANLAGE-ZIEL-KZ     PIC  X(01).
      *        J Wenn geändert

           05  GHP-VERFUEG-KZ              PIC  X(001).
           05  GHP-AEND-VERFUEG-KZ-KZ      PIC  X(001).
           05  GHP-TELREVERS               PIC  X(001).
           05  GHP-AEND-TELREVERS-KZ       PIC  X(001).
           05  GHP-ONLREVERS               PIC  X(001).
           05  GHP-AEND-ONLREVERS-KZ       PIC  X(001).

           05  GHP-EIGEN-KZ-FIX            PIC  X(001).

           05  GHP-AKTUAL-KZ               PIC  X(01).

      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
      *
           05  GHP-DEPOT-ART               PIC  X(01).
           05  GHP-AEND-DEPOT-ART-KZ       PIC  X(01).
           05  FILLER                      PIC  X(35).
      *        Reserve
      ******************************************************************

