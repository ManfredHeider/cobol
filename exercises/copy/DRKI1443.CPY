      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1443.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI1443.CPv  $
      *
      *   Rev 2.4   Mar 01 2016 14:32:22   t13ex73
      *AKI01419: Copystrecken Erweiterung
      *
      *   Rev 2.3   Feb 16 2016 14:09:46   t13ex73
      *AKI01419: Copystrecken Erweiterung
      *
      *   Rev 2.2   Oct 13 2004 14:12:00   t13ex39
      *AKI00249: DEPOTSTAMM -BESTAND
      *
      *   Rev 2.1   Oct 24 2002 18:47:24   T13EX39
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
      *    Länge = 214 bytes                                           *
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
      * AKI00249!15.09.04!02  ! ST-BUCH-ANTEILE           !W.Hornung *
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
       01  DRKI1443.
           05  DT-VERT-ID                        PIC  X(13).
           05  DT-GESAMT.
               15  DT-GES                        PIC  9(02).
               15  DT-VNR                        PIC  9(15).
               15  DT-ANTEIL-LFDNR               PIC  9(02).
      *        LFDNR VON ANTEIL = VERTRAGSUNTERNUMMER
 
               15  DT-WP-KENN-NR                 PIC  X(06).
               15  DT-WP-KENN-NR-ISIN            PIC  X(12).
 
               15  DT-BUCH-GUTHABEN              PIC  9(11)V99.
               15  DT-BUCH-BETRAG                PIC  9(07)V99.
               15  DT-BUCH-DATUM                 PIC  X(10).
               15  DT-BUCH-SCHLUESSEL            PIC S9(03).
               15  DT-BUCH-STEUERBETRAG          PIC  9(07)V99.
      *        15  DT-BUCH-ANTEILE               PIC  9(06)V999.
               15  DT-BUCH-ANTEILE               PIC  9(06)V9999.
 
      *        15  DT-BUCH-ZINS-AB-ST            PIC  9(06)V99.
               15  DT-BUCH-ZINS-AB-ST            PIC  9(09)V99.
      *        ZINSABSCHLAGSTEUER
               15  DT-VL-KZ                      PIC  X(01).
      *        KZ, OB PRODUKT VERMOEGENSWIRKSAMELEISTUNG (J oder leer)
 
               15  DT-WKZ                        PIC  X(01).
 
      *        15  FILLER                        PIC  X(099).
               15  FILLER                        PIC  X(096).
 
               15  DT-AKTUAL-KZ                  PIC  X(01).
 
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
 
 
 

