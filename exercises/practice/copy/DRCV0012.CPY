      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRCV0012.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRCV0012.CPv  $
      *
      *   Rev 2.1   Sep 15 2008 12:31:08   t13ae35
      *ACV00106: CTV Sonderaktion
      *
      *   Rev 2.0   Sep 11 2008 11:40:22   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00049!11.09.08!2.1 ! Neuerstellung             ! J.Hering *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                               *
      *            COPY-MEMBER für CTC-Spendenbescheid                *
      *            DATEINUMMER: CV004300 QSAM                         *
      *            COPYMEMBER:  DRCV0012                              *
      *            SATZLAENGE:  79 BYTES                              *
      *                                                               *
      *---------------------------------------------------------------*
       01  DRCV0012-SATZ.
           02 DRCV0012-VB-NR                    PIC 9(007).
           02 DRCV0012-DATUM                    PIC X(010).
           02 DRCV0012-BETRAG-IN-WORTEN         PIC X(050).
           02 DRCV0012-BETRAG                   PIC ZZZZZZZ9,99.
           02 DRCV0012-ZUSTIFT-KZ               PIC X(001).

