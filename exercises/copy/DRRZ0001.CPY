      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.09.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRRZ0001.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRRZ0001.CPv  $
      *
      *   Rev 2.2   Oct 13 2005 12:30:08   t13ex22
      *ASR00001.UE036 VK ergänzt
      *
      *   Rev 2.1   Dec 23 2004 10:33:56   t13ex22
      *ASR00001: Erstellen Index-Datei aus Verzeichnisinhalt für Mi
      *gration
      *
      *   Rev 2.0   Sep 10 2004 12:54:38   t13ex22
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRRZ0001
      *    --------------------------------------------------------   *
      *    STANDARD-ERROR-BEREICH (INCL. TRACE)                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    10.09.2004                                       *
      *****************************************************************
      *    VERSION   1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ARZ00108!10.09.04! 01 ! NEU                       ! T.MUND   *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
       01  EDATEI-REC.
           05 REC-OHNE-BELEGART.
              10  VB-NR-6                                PIC  9(006).
              10  FILLER                                 PIC  X(001).
              10  EXTENSION                              PIC  X(003).
              10  FILLER                                 PIC  X(070).
           05 REC-MIT-BELEGART  REDEFINES REC-OHNE-BELEGART.
              10  VB-NR-6-BA                             PIC  9(006).
              10  FILLER                                 PIC  X(001).
              10  BELEGART.
                  15 GV-1                                PIC  X(001).
                  15 GV-2                                PIC  X(001).
                  15 GV-3                                PIC  X(001).
                  15 GV-4                                PIC  X(001).
              10  FILLER                                 PIC  X(001).
              10  EXTENSION-BA                           PIC  X(003).
              10  FILLER                                 PIC  X(061).
      * ----------------------------------------------------------------
