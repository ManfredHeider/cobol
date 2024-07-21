      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.07.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRSL0015.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRSL0015.CPv  $
      *
      *   Rev 2.2   Jan 20 2011 13:31:36   t13ex43
      *ASL00364: PGM - CTV-Beschaffung KLZ DVAG
      *
      *   Rev 2.1   Jul 08 2005 10:16:58   t13ex43
      *ASL00204: CTV - K-Leistungszuschuss
      *
      *   Rev 2.0   Jul 04 2005 16:23:40   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME     *
      * --------------------------------------------------------------*
      * ASL00204!04.07.05!2.1 ! NEU                        ! KLEEMEIER*
190111* ASL00364!19.01.11!2.2 ! NEUE BETRÄGE HINZU         ! KLEEMEIER*
      *         !        !    !                            !          *
      *                                                               *
      *****************************************************************
      *                                                               *
      *            INHALT    :  KUNDEN-LEISTUNGSZUSCHUSS              *
      *            DATEI     :  SL012000                              *
      *            MEMBER    :  DRSL0015                              *
      *            ORGANISAT.:  VSAM                                  *
      *            SATZLÄNGE :  51 BYTE                               *
      *            KEY       :  VBNR (1-7)                            *
      *                                                               *
      *---------------------------------------------------------------*
       01  DRSL0015-SATZ.
           05 DRSL0015-VBNR                     PIC 9(007).
           05 DRSL0015-VBNR-DLEITER             PIC 9(007).
           05 DRSL0015-DIR-NR                   PIC 9(003).
           05 DRSL0015-MONAT                    PIC X(009).
           05 DRSL0015-JAHR                     PIC 9(004).
           05 DRSL0015-KLZ-BETRAG               PIC 9(005)    COMP-3.
190111**** 05 DRSL0015-KDAS-VOLUMEN             PIC 9(001)V99 COMP-3.
190111     05 DRSL0015-NET-VOLUMEN              PIC 9(009)V99 COMP-3.
190111     05 DRSL0015-INVEST                   PIC 9(009)V99 COMP-3.
190111     05 DRSL0015-GES-VOLUMEN              PIC 9(009)V99 COMP-3.
      *****
      *****   ENDE DRSL0015
      *****
