      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.03.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRSR0003.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRSR0003.CPv  $
      *
      *   Rev 2.2   Mar 22 2011 12:52:24   t13ae14
      *ASR00023: EMail-Archivierung in OnDemand Routine zur Indexer
      *zeugung
      *
      *   Rev 2.1   Mar 22 2011 11:47:58   t13ae14
      *ASR00023: EMail-Archivierung in OnDemand Routine zur Indexer
      *zeugung
      *
      *   Rev 2.0   Mar 22 2011 11:34:08   t13ae14
      *Archiverstellung
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRSR0003
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * ASR00023!22.03.11! 2.0! NEU                       !K. OBER    *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************

       01  EDATEI-REC.
           05 REC-OHNE-BELEGART.
              10  VB-NR-6                                PIC  9(006).
              10  FILLER                                 PIC  X(001).
              10  EXTENSION                              PIC  X(003).
              10  FILLER                                 PIC  X(070).
           05 REC-MIT-BELEGART  REDEFINES REC-OHNE-BELEGART.
              10  VB-NR-6-BA                             PIC  9(006).
              10  FILLER                                 PIC  X(001).
              10  BELEGART-BA.
                  15 GV-1                                PIC  X(001).
                  15 GV-2                                PIC  X(001).
                  15 GV-3                                PIC  X(001).
                  15 GV-4                                PIC  X(001).
              10  FILLER                                 PIC  X(001).
              10  NUMMER-BA.
                  15 NR-1                                PIC  X(001).
                  15 NR-2                                PIC  X(001).
                  15 NR-3                                PIC  X(001).
                  15 NR-4                                PIC  X(001).
              10  FILLER                                 PIC  X(001).
              10  EXTENSION-BA                           PIC  X(003).
              10  FILLER                                 PIC  X(056).
      * ----------------------------------------------------------------
