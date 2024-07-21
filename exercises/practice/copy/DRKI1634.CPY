      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1634.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1634.CPv  $
      *
      *   Rev 2.1   Feb 18 2004 16:08:56   t13ex39
      *AKI00391 Fonds aktualisieren
      *
      *   Rev 2.0   Dec 14 2001 09:21:16   p13sp20
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR FONDS                                       *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    DATEITYP  = QSAM                                            *
      *    Länge = 150 bytes                                           *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00391!09.02.04!01  ! NEU                       !W.Hornung *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1634.
           05  FOND-GES                        PIC  9(03).

           05  FOND-WP-KENN-NR                 PIC  X(06).
           05  FOND-WP-KENN-NR-ISIN            PIC  X(12).

           05  FOND-DVAG-PROD-SCHL             PIC  9(02).
           05  FOND-VL-KZ                      PIC  X(01).
           05  FOND-BEZ                        PIC  X(40).
           05  FOND-TYP                        PIC  X(20).

           05  FOND-AKTUAL-KZ                  PIC  X(01).

           05  FILLER                          PIC  X(20).
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
