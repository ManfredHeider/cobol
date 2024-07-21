      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1440.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI1440.CPv  $
      *
      *   Rev 2.4   Mar 01 2016 14:32:16   t13ex73
      *AKI01419: Copystrecken Erweiterung
      *
      *   Rev 2.3   Feb 16 2016 14:09:42   t13ex73
      *AKI01419: Copystrecken Erweiterung
      *
      *   Rev 2.2   Oct 13 2004 14:11:52   t13ex39
      *AKI00249: DEPOTSTAMM -BESTAND
      *
      *   Rev 2.1   Oct 24 2002 18:19:16   T13EX39
      *AKI00301 DEUTSCHE BANK DEPOTTRANSAKTIONEN
      *
      *   Rev 2.0   Dec 14 2001 09:21:16   p13sp20
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR DEPOTTRANSAKTION DEUTSCHE BANK              *
      *    ----------------------------------------------              *
      *                                                                *
      *    DATEITYP  = QSAM                                            *
      *    Länge = 201 bytes                                           *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00301!01.09.02!01  ! NEU                       !W.Hornung *
      * -------------------------------------------------------------*
      * AKI00249!15.09.04!02  ! BNK-BUCH-ANTEILE          !W.Hornung *
      *         !        !    ! jetzt mit 4 Nachkommastel.!          *
      * -------------------------------------------------------------*
      * AKI01419!12.02.16!03  ! BUCH-ZINS-AB erweitern    !DDY       *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1440.
           05  BNK-GES                        PIC  9(02).
           05  BNK-VNR                        PIC  9(15).
           05  BNK-ANTEIL-LFDNR               PIC  9(02).
      *        LFDNR VON ANTEIL = VERTRAGSUNTERNUMMER
 
           05  BNK-WP-KENN-NR                 PIC  X(06).
           05  BNK-WP-KENN-NR-ISIN            PIC  X(12).
 
           05  BNK-BUCH-GUTHABEN              PIC  9(11)V99.
           05  BNK-BUCH-BETRAG                PIC  9(07)V99.
           05  BNK-BUCH-DATUM                 PIC  X(10).
           05  BNK-BUCH-SCHLUESSEL            PIC S9(03).
           05  BNK-BUCH-STEUERBETRAG          PIC  9(07)V99.
      *    05  BNK-BUCH-ANTEILE               PIC  9(06)V999.
           05  BNK-BUCH-ANTEILE               PIC  9(06)V9999.
 
      *    05  BNK-BUCH-ZINS-AB-ST            PIC  9(06)V99.
           05  BNK-BUCH-ZINS-AB-ST            PIC  9(09)V99.
      *        ZINSABSCHLAGSTEUER
           05  BNK-VL-KZ                      PIC  X(01).
      *        KZ, OB PRODUKT VERMOEGENSWIRKSAMELEISTUNG (J oder leer)
 
           05  BNK-WKZ                        PIC  X(01).
 
      *    05  FILLER                        PIC  X(100).
      *    05  FILLER                        PIC  X(099).
           05  FILLER                        PIC  X(096).
 
           05  BNK-AKTUAL-KZ                  PIC  X(01).
 
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
 
 

