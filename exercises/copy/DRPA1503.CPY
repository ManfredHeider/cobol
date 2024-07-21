      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1503.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1503.CPv  $
      *
      *   Rev 2.1   20 Aug 2001 09:30:14   t13ex21
      *APA00133 - Neuerstellung
      *
      *   Rev 2.0   04 Jul 2001 11:10:28   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1503  PEG-DATENSATZ VON DMB (82)          *
      *    ---------------------------------------------------------  *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    08.08.01                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00133!08.08.01!2.1 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    DMB-DTA'S.                                                *
      ****************************************************************
      *
             30 DMB-DATENSATZ.
                35 PGEB-GES                    PIC X(02).
                35 FILLER                      PIC X(03).
                35 AVERM                       PIC 9(07).
                35 FILLER                      PIC X(01).
                35 VNR                         PIC 9(14).
                35 PIN                         PIC 9(07).
                35 FILLER                      PIC X(08).
                35 VORNAME                     PIC X(10).
                35 NAME                        PIC X(20).
                35 STRASSE                     PIC X(20).
                35 ORT                         PIC X(30).
                35 PRODUKT                     PIC 9(02).
                35 VERTRAGSBEGINN              PIC X(08).
                35 VERTRAGSABLAUF              PIC X(08).
                35 PGV.
                   40 BG                       PIC 9(02).
                   40 WERTST                   PIC 9(02).
                35 FAELLIG-VON                 PIC 9(06).
                35 ZAHLWEISE                   PIC 9(01).
                35 PPFL-BEITRAG                PIC 9(7)V99.
                35 PROV-SATZ                   PIC 9(02)V9.
                35 PROV-BETRAG                 PIC 9(07)V99.
                35 PG-BUCHDAT                  PIC 9(06).
                35 PG-ABRECHDAT                PIC 9(06).
      *
      ******** ENDE COPY-MEMBER **************************************
