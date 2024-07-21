      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.10.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRCV0011.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRCV0011.CPv  $
      *
      *   Rev 2.1   07 Dec 2007 15:38:48   t13ae35
      *ARW00041: SWNP - Rechnungen per CTV
      *
      *   Rev 2.0   17 Oct 2007 13:59:30   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ARW00041!17.10.07!2.1 ! Neu                       ! Hering   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *            Inhalt    :  Parameter für CTV-Anschreiben SWNP   *
      *            Datei     :  CV003200                             *
      *            Member    :  DRCV0011                             *
      *            Organisat.:  VSAM                                 *
      *            Satzl„nge :  38 Byte                              *
      *            Key       :  VBNR,Jahr,Monat (13 Byte)            *
      *                                                              *
      *--------------------------------------------------------------*
       01  DRCV0011-SATZ.
           05 DRCV0011-KEY.
              10 DRCV0011-VBNR                     PIC 9(007).
              10 DRCV0011-JAHR                     PIC 9(004).
              10 DRCV0011-MONAT                    PIC 9(002).
           05 DRCV0011-SWNP-BETRAG                 PIC 9(013)V99.
           05 DRCV0011-RECH-NR                     PIC 9(010).

