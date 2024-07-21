      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2351.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2351.CPv  $
      *
      *   Rev 2.3   Oct 30 2009 10:52:32   t13ex50
      *AKI00844: Copy Ausg. Aufber. Depotstamm (GHP)
      *
      *   Rev 2.2   Oct 27 2009 12:38:50   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.1   Aug 19 2009 09:22:32   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.0   Aug 11 2009 11:39:08   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR AUSGABEDATEI DEPOTBSTAMM DWS                *
      *    ------------------------------------------------------      *
      *                                                                *
      *                                                                *
      *    SATZLÄNGE   106                                             *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00844!01.08.09! 1.0! NEU                       !WHO       *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  DRKI2351.

           05  GHP-GES                     PIC  9(02).
           05  GHP-VNR                     PIC  9(10).
           05  GHP-LFDNR                   PIC  9(02).
      *        NUR Unterkontonummer
           05  GHP-PRODUKT-ID              PIC  S9(04).

           05  GHP-DATUM-EROEFFNUNG        PIC  X(10).
           05  GHP-AEND-DATUM-E-KZ         PIC  X(01).
      *        J WENN ERÖFFNUNGSDATUM GEAENDERT

           05  GHP-DATUM-SCHLIESSUNG       PIC  X(10).
           05  GHP-AEND-DATUM-S-KZ         PIC  X(01).
      *        J WENN SCHLIESSUNGSDATUM GEAENDERT

           05  GHP-PORTFOLIO-NR            PIC  X(10).
           05  GHP-AEND-PORTFOLIO-KZ       PIC  X(01).
      *        KZ, OB PORTFOLIONR GEÄNDERT

           05  GHP-AKTUAL-KZ               PIC  X(01).

      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
      *
           05  FILLER                      PIC  X(54).
      *        Reserve
      ******************************************************************


