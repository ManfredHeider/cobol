      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.02.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2983.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2983.CPv  $
      *
      *   Rev 2.2   Jan 25 2018 07:04:16   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs  KARTEN
      *
      *   Rev 2.1   Feb 18 2015 16:23:54   t13ex29
      *AKI01348: Auslagern EC-Karten in eig. Tabelle-Bearbeitung
      *
      *   Rev 2.0   Feb 11 2015 15:00:42   t13ex29
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 9 8 3    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMME DER BANKEN,           *
      *    AUFBEREITUNG GUTHABENPRODUKT                                *
      *        - AUSGABE: ALLGEMEIN DBPRODUKT FUER DEUTSCHE BANK       *
      *                                                                *
      *    SATZLÄNGE   443                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *12.02.15!NEUERSTELLUNG                          !THIES    ! 01  *
      *KI-NEU  !                                       !         !     *
      *--------------------------------------------------------------*
101217* AKI01556!10.12.17! PIN verarbeiten                ! WHO  !   *
      *         !        !                                !      !   *
      *         !        !                                !      !   *
      *         !        !                                !      !   *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
       01  DRKI2983.
 
           05  DB-GES                             PIC  9(02).
           05  DB-VNR                             PIC  9(15).
           05  DB-GHP-LFDNR                       PIC  9(02).
      *        NUR Unterkontonummer
           05  DB-GHP-PRODUKT-ID                  PIC  S9(04).
      *        Produkt-ID
           05  DB-DBPROD-LFDNR                    PIC  S9(04).
      *        lfd. Nr. von DBPRODUKT
 
           05  DB-KARTEN-ART                      PIC  X(03).
      *        Kartenart
           05  DB-AEND-KARTEN-ART-KZ              PIC  X(01).
      *        J WENN WKZ GEAENDERT
 
           05  DB-ANZAHL-KARTEN-AKTIV             PIC S9(04).
      *        Anzahl der aktiven Karten je Kartenart
           05  DB-AEND-ANZAHL-KARTEN-AKTIV-KZ     PIC  X(01).
      *        J WENN WKZ GEAENDERT
 
           05  DB-HAUPT-KARTEN-NR                 PIC  X(16).
      *        Hauptkartennummer
           05  DB-AEND-HAUPT-KARTEN-NR-KZ         PIC  X(01).
      *        J WENN WKZ GEAENDERT
 
           05  DB-AKTUAL-KZ                       PIC  X(01).
 
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
 
101217     05  DB-PIN                             PIC 9(09).

101217*    05  FILLER                             PIC X(046).
101217     05  FILLER                             PIC X(037).
      *        Filler um auf xxx-Stellen zu kommen


