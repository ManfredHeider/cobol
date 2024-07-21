      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.11.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRSL0012.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRSL0012.CPv  $
      *
      *   Rev 2.2   Oct 13 2005 09:03:30   t13ex43
      *ACV00072: PGM - Manageraufbauplan - BB als CTV-Brief (2. Sei
      *te)
      *
      *   Rev 2.1   Nov 18 2004 14:49:50   t13ex43
      *ASL00183: PGM - CTV-Anschreiben für D-Leiter (1)
      *
      *   Rev 2.0   Nov 10 2004 12:26:28   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME     *
      * --------------------------------------------------------------*
      * ASL00183!10.11.04!2.1 ! NEU                        ! KLEEMEIER*
310805* ACV00072!31.08.05!2.2 ! ÜBERSICHT NEUE PART. HINZU ! KLEEMEIER*
      *         !        !    !                            !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *            INHALT    :  CTV-DATEI MAP-ANSCHREIBEN D-LEITER (1)*
      *            DATEI     :  CV001700                              *
      *            MEMBER    :  DRSL0012                              *
      *            ORGANISAT.:  VSAM                                  *
      *            SATZLÄNGE :  3023 BYTE                             *
      *            KEY       :  VBNR (1-7)                            *
      *                                                               *
      *---------------------------------------------------------------*
       01  DRSL0012-SATZ.
           05 DRSL0012-VBNR                     PIC  9(007).
           05 DRSL0012-STUFE                    PIC  X(004).
           05 DRSL0012-EH-BEGINN                PIC  X(007).
310805     05 DRSL0012-ZEITRAUM                 PIC  X(023).
310805     05 DRSL0012-DATUM                    PIC  X(010).
310805     05 DRSL0012-GESELL                   PIC  X(009).
310805     05 DRSL0012-DRUCK-TAB-ANZ            PIC  9(003).
310805     05 DRSL0012-DRUCK-TAB.
310805        10 FILLER OCCURS 22.
310805           15 DRSL0012-DRUCK              PIC  X(132).
310805     05 DRSL0012-SUM-ZUG-VB               PIC S9(009)V9  COMP-3.
310805     05 DRSL0012-SUM-ABG-VB               PIC S9(009)V9  COMP-3.
310805     05 DRSL0012-SUM-GES-VB               PIC S9(009)V9  COMP-3.
310805     05 DRSL0012-GES-ZUG-VB               PIC S9(009)V9  COMP-3.
310805     05 DRSL0012-GES-ABG-VB               PIC S9(009)V9  COMP-3.
310805     05 DRSL0012-GES-GES-VB               PIC S9(009)V9  COMP-3.
310805     05 DRSL0012-EQF                      PIC S9(003)V99 COMP-3.
310805     05 DRSL0012-EINH-KORR                PIC S9(009)V9  COMP-3.
310805     05 DRSL0012-IST-EINH                 PIC S9(009)V9  COMP-3.
310805     05 DRSL0012-IST-PART                 PIC  9(003).
310805     05 DRSL0012-ERFUELLT                 PIC  X(001).
310805     05 DRSL0012-STERN                    PIC  X(001).
      *****
      *****   ENDE DRSL0012
      *****
