      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.03.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2235.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2235.CPv  $
      *
      *   Rev 2.2   Apr 11 2008 10:08:36   t13ex14
      *AKI00703: Auswertung für Kündigerbefragung
      *
      *   Rev 2.1   Apr 04 2008 13:25:20   t13ex14
      *AKI00703: Auswertung für Kündigerbefragung
      *
      *   Rev 2.0   Mar 19 2008 14:21:36   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * AKI00703!19.03.08! 1.0! NEU                       ! WMI      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  DRKI2235.

           05  KUENDIGER-AUSGABE.
      *
               10  KD-VBNUMMER                   PIC  9(007).
               10  FILLER                        PIC  X(001) VALUE ','.
               10  KD-VBVORNAME                  PIC  X(030).
               10  FILLER                        PIC  X(001) VALUE ','.
               10  KD-VBNAME                     PIC  X(030).
               10  FILLER                        PIC  X(001) VALUE ','.
               10  KD-VBSTUFE                    PIC  X(004).
               10  FILLER                        PIC  X(001) VALUE ','.
               10  KD-GESELLSCHAFT               PIC  9(002).
               10  FILLER                        PIC  X(001) VALUE ','.
               10  KD-VERTRAGSNUMMER             PIC  X(015).
               10  FILLER                        PIC  X(001) VALUE ','.
               10  KD-VTRABLAUFDAT               PIC  X(010).
               10  FILLER                        PIC  X(001) VALUE ','.
               10  KD-KUNDENNUMMER               PIC  9(009).
               10  FILLER                        PIC  X(001) VALUE ','.
               10  KD-KUNDENVORNAME              PIC  X(030).
               10  FILLER                        PIC  X(001) VALUE ','.
               10  KD-KUNDENNAME                 PIC  X(030).
               10  FILLER                        PIC  X(001) VALUE ','.
               10  KD-TELEFONNR                  PIC  X(015).
               10  FILLER                        PIC  X(001) VALUE ','.
               10  KD-LOESCHKZ                   PIC  X(001).
               10  FILLER                        PIC  X(001) VALUE ','.
