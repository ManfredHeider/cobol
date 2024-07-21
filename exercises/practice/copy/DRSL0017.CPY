      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.01.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRSL0017.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRSL0017.CPv  $
      *
      *   Rev 2.1   18 Jan 2008 10:17:52   t13ex43
      *ASL00285: PGM2 - Leistungszuschuß Allfinanz
      *
      *   Rev 2.0   14 Jan 2008 17:01:04   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME     *
      * --------------------------------------------------------------*
      * ASL00285!14.01.08!2.1 ! NEU                        ! KLEEMEIER*
      *         !        !    !                            !          *
      *         !        !    !                            !          *
      *                                                               *
      *****************************************************************
      *                                                               *
      *            INHALT    :  LEISTUNGSZUSCHUSS ALLFINANZ           *
      *            DATEI     :  CV003400                              *
      *            MEMBER    :  DRSL0017                              *
      *            ORGANISAT.:  VSAM                                  *
      *            SATZLÄNGE :  41 BYTE                               *
      *            KEY       :  VBNR (1-7)                            *
      *                                                               *
      *---------------------------------------------------------------*
       01  DRSL0017-SATZ.
           05 DRSL0017-VBNR                     PIC 9(007).
           05 DRSL0017-VBNR-DLEITER             PIC 9(007).
           05 DRSL0017-DIR-NR                   PIC 9(003).
           05 DRSL0017-MONAT                    PIC X(009).
           05 DRSL0017-JAHR                     PIC 9(004).
           05 DRSL0017-LZ-BETRAG                PIC 9(005)V99 COMP-3.
           05 DRSL0017-EG-FG-EINH               PIC 9(006)V9  COMP-3.
           05 DRSL0017-FAKTOR                   PIC 9(003)V99 COMP-3.
      *****
      *****   ENDE DRSL0017
      *****
