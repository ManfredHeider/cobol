      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.11.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0255.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0255.CPv  $
      *
      *   Rev 2.1   Dec 10 2009 15:59:26   t13ae34
      *AAN00896: OptimA Stufe 1 - SST OKI -> Antragsbestand
      *
      *   Rev 2.0   Nov 30 2009 16:31:08   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    STATISTIKDATEN HANDSCHRIFTLICHE ÄNDERUNG ELEKTR. ANTRÄGE
      ****************************************************************
      *    COPY-MEMBER: DRAN0255                                     *
      *    SATZBESCHREIBUNG                                          *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00896!01.12.02!2.1 !NEU                        !HEIDER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  STH-SATZ.
           05 STH-KEY.
      *          DATUM DES ERZEUGTEN ANTRAGS BEI DVAG FORMAT JHJJMMTT
      *          MONATSWERTE WERDEN MIT TT=00 GESPEICHERT
              10 STH-DATUM.
                 15 STH-DATUM-JH     PIC 9(02).
                 15 STH-DATUM-JJ     PIC 9(02).
                 15 STH-DATUM-MM     PIC 9(02).
                 15 STH-DATUM-TT     PIC 9(02).
      *          SPARTE BEI AMV: PK ODER KFZ
              10 STH-AMV-SPARTE      PIC X(01).
                 88 STH-AMV-SPARTE-PK   VALUE 'P'.
                 88 STH-AMV-SPARTE-KFZ  VALUE 'K'.
      *          USER
              10 STH-ERFASSER        PIC X(07).
      *          PRODUKTGEBER-GESELLSCHAFT
              10 STH-PGEB            PIC 9(02).
           05 STH-DATEN.
      *       ANZAHL MANUELLE ANTRÄGE (AN01)
              10 STH-ANZAHL-MANU     PIC 9(07).
      *       ANZAHL MASCHINELL NACHERFASSTER ANTRÄGE PRODUKTGEBER
              10 STH-ANZAHL-AUTO-P   PIC 9(07).
      *       ANZAHL AUTOMATISCHE ANTRÄGE (AN03)
              10 STH-ANZAHL-AUTO     PIC 9(07).
      *       DAVON MIT HANDSCHRIFTLICHER ÄNDERUNG
              10 STH-ANZAHL-HAND-J   PIC 9(07).
      *       DAVON OHNE HANDSCHRIFTLICHE ÄNDERUNG
              10 STH-ANZAHL-HAND-N   PIC 9(07).
      *       GESAMT
              10 STH-ANZAHL-GESAMT   PIC 9(07).
      *       ANZAHL SCHNELLANTRÄGE
              10 STH-ANZAHL-SCHNELL  PIC 9(07).
      *
