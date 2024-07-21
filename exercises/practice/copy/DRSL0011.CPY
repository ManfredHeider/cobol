      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRSL0011.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRSL0011.CPv  $
      *
      *   Rev 2.1   Oct 08 2004 13:28:14   t13ex43
      *ASL00184: PGM - CTV-Parameter für Bestandsleistungs-Zuschuss
      *
      *   Rev 2.0   Sep 27 2004 10:49:16   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME     *
      * --------------------------------------------------------------*
      * ASL00184!27.09.04!2.1 ! NEU                        ! KLEEMEIER*
      *         !        !    !                            !          *
      *         !        !    !                            !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *            INHALT    :  LZB-BERECHNUNGSBOGEN-CTV-DATEI        *
      *            DATEI     :  SL010400 UND SL010500                 *
      *            MEMBER    :  DRSL0011                              *
      *            ORGANISAT.:  VSAM                                  *
      *            SATZLÄNGE :  126 BYTE                              *
      *            KEY       :  VBNR (1-7)                            *
      *                                                               *
      *---------------------------------------------------------------*
       01  DRSL0011-SATZ.
           05 DRSL0011-VBNR                     PIC 9(007).
           05 DRSL0011-VBNR-DLEITER             PIC 9(007).
           05 DRSL0011-DIR-NR                   PIC 9(003).
           05 DRSL0011-QUARTAL                  PIC 9(001).
           05 DRSL0011-JAHR                     PIC 9(004).
           05 DRSL0011-LAUFDATUM                PIC X(010).
           05 DRSL0011-STUFE-BEZ                PIC X(004).
           05 DRSL0011-STUFE-PRAXIS             PIC X(002).
           05 DRSL0011-P-KENNZ                  PIC X(001).
           05 DRSL0011-P-12MON-ZEIT             PIC X(013).
           05 DRSL0011-P-FAKTOR                 PIC 9(003)V99 COMP-3.
           05 DRSL0011-P-12MON-EH               PIC 9(010)V9  COMP-3.
           05 DRSL0011-P-03MON-ZEIT             PIC X(013).
           05 DRSL0011-P-03MON-EH               PIC 9(010)V9  COMP-3.
           05 DRSL0011-P-01MON-EH               PIC 9(010)V9  COMP-3.
           05 DRSL0011-P-BETRAG                 PIC 9(009)V99 COMP-3.
           05 DRSL0011-B-KENNZ                  PIC X(001).
           05 DRSL0011-B-FAKTOR                 PIC 9(003)V99 COMP-3.
           05 DRSL0011-B-12MON-EH               PIC 9(010)V9  COMP-3.
           05 DRSL0011-B-03MON-EH               PIC 9(010)V9  COMP-3.
           05 DRSL0011-B-01MON-EH               PIC 9(010)V9  COMP-3.
           05 DRSL0011-B-BETRAG                 PIC 9(009)V99 COMP-3.
           05 DRSL0011-GESAMT                   PIC 9(009)V99 COMP-3.
      *****
      *****   ENDE DRSL0011
      *****
