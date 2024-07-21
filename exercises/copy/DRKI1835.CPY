      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.11.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1835.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1835.CPv  $
      *
      *   Rev 2.2   Jan 16 2006 18:03:30   t13ex39
      *AKI00494: Änderung der Lieferung DeuBa-Sparpläne
      *
      *   Rev 2.0   Nov 16 2005 07:54:42   t13ex39
      *Archiverstellung
      *
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 1 8 3 5    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMME DER BANKEN,           *
      *    AUFBEREITUNG GUTHABENPRODUKT BEI GEÄNDERTER UNTERNUMMER     *
      *        - AUSGABE: ALLGEMEIN GUTHABENPRODUKT FUER DEUTSCHE BANK *
      *                                                                *
      *    SATZLÄNGE   100                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *--------!---------------------------------------!---------!-----*
      *11.11.05!FOZ-LFDNR-ALT für Änderung Primary Key !HORNUNG  ! 03  *
      *AKI00494!                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
      *---------------------------------------------------------------*
       01  DRKI1835.

           05  FOZ-GES                     PIC  9(02).
           05  FOZ-VNR                     PIC  9(15).
           05  FOZ-LFDNR                   PIC  9(02).
      *        NUR Unterkontonummer
           05  FOZ-PRODUKT-ID              PIC  S9(04).
           05  FOZ-ISIN                    PIC   X(12).

           05  FOZ-LFDNR-ALT               PIC  9(02).
      *        NUR Unterkontonummer ALT

           05  FOZ-PLAN-NR                 PIC  9(02).
      *        Sparplannummer
           05  FOZ-VL-KZ                   PIC  X(01).

           05  FOZ-AKTUAL-KZ               PIC  X(01).

      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
      *
           05  FILLER                      PIC  X(19).
      *        Reserve
      ******************************************************************
